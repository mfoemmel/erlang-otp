%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(mod_security).
-behaviour(gen_server).

%% Security Audit Functionality

%% User API exports
-export([list_blocked_users/1, list_blocked_users/2, 
	 block_user/4, unblock_user/2, unblock_user/3,
	 list_auth_users/1, list_auth_users/2]).

%% module API exports
-export([do/1, load/2, store/2, remove/1]).

%% gen_server exports
-export([stop/1, init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2]).
-include("httpd.hrl").


%% do/1
do(Info) ->
    %% Check and see if any user has been authorized.
    case httpd_util:key1search(Info#mod.data, remote_user, not_defined_user) of
	not_defined_user ->
	    %% No user has been authorized.
	    case httpd_util:key1search(Info#mod.data, status) of
		%% A status code has been generated!
		{401, PhraseArgs, Reason} ->
		    case httpd_util:key1search(Info#mod.parsed_header,
					       "Authorization") of
			undefined ->
			    %% Not an authorization attempt (server just replied to
			    %% challenge for authentication)
			    {proceed, Info#mod.data};
			[$B,$a,$s,$i,$c,$ |EncodedString] ->
			    %% Someone tried to authenticate, and obviously failed!
			    report_failed(Info, EncodedString),
			    take_failed_action(Info, EncodedString),
			    {proceed, Info#mod.data}
		    end;
		_ ->
		    {proceed, Info#mod.data}
	    end;
	User ->
	    %% A user has been authenticated, now is he blocked ?
	    Path=mod_alias:path(Info#mod.data,Info#mod.config_db,
				Info#mod.request_uri),
	    {Dir, SDirData} = secretp(Path, Info#mod.config_db),
	    Port = httpd_util:lookup(Info#mod.config_db, port),
	    DF = httpd_util:key1search(SDirData, data_file),
	    case check_blocked_user(Info, User, SDirData, Port) of
		true ->
		    {proceed, [{status, {403, Info#mod.request_uri, ""}}|Info#mod.data]};
		false ->
		    store_successful_auth(Port, User, SDirData),
		    {proceed, Info#mod.data}
	    end
    end.


report_failed(Info, EncodedString) ->
    Request = Info#mod.request_line,
    Decoded = httpd_util:decode_base64(EncodedString),
    {PortNumber,RemoteHost}=(Info#mod.init_data)#init_data.peername,
    String = RemoteHost++" : Failed authentication: "++Request++" : "++Decoded,
    mod_log:security_log(Info, String).

take_failed_action(Info, EncodedString) ->
    Path = mod_alias:path(Info#mod.data,Info#mod.config_db, Info#mod.request_uri),
    {Dir, SDirData} = secretp(Path, Info#mod.config_db),
    Port = httpd_util:lookup(Info#mod.config_db, port),
    DecodedString = httpd_util:decode_base64(EncodedString),
    store_failed_auth(Info, Port, DecodedString, SDirData).

secretp(Path, ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory,'$1','_'}),
    case secret_path(Path, Directories) of
	{yes, Directory} ->
	    SDirs0 = httpd_util:multi_lookup(ConfigDB, security_directory),
	    SDir = lists:filter(fun(X) ->
					lists:member({path, Directory}, X)
				end, SDirs0),
	    {Directory, lists:flatten(SDir)};
	no ->
	    error_logger:error_report({internal_error_secretp, ?MODULE}),
	    {[], []}
    end.

secret_path(Path,Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)), to_be_found).

secret_path(Path, [], to_be_found) ->
    no;
secret_path(Path, [], Directory) ->
    {yes, Directory};
secret_path(Path, [[NewDirectory]|Rest], Directory) ->
    case regexp:match(Path, NewDirectory) of
	{match, _, _} when Directory == to_be_found ->
	    secret_path(Path, Rest, NewDirectory);
	{match, _, Length} when Length > length(Directory)->
	    secret_path(Path, Rest, NewDirectory);
	{match, _, Length} ->
	    secret_path(Path, Rest, Directory);
	nomatch ->
	    secret_path(Path, Rest, Directory)
    end.


load([$<,$D,$i,$r,$e,$c,$t,$o,$r,$y,$ |Directory],[]) ->
    Dir = httpd_conf:custom_clean(Directory,"",">"),
    {ok, [{security_directory, Dir, [{path, Dir}]}]};
load(eof,[{security_directory,Directory, DirData}|_]) ->
    {error, ?NICE("Premature end-of-file in "++Directory)};
load([$S,$e,$c,$u,$r,$i,$t,$y,$D,$a,$t,$a,$F,$i,$l,$e,$ |FileName],
     [{security_directory, Dir, DirData}]) ->
    File = httpd_conf:clean(FileName),
    {ok, [{security_directory, Dir, [{data_file, File}|DirData]}]};
load([$S,$e,$c,$u,$r,$i,$t,$y,$C,$a,$l,$l,$b,$a,$c,$k,$M,$o,$d,$u,$l,$e,$ |ModuleName],
     [{security_directory, Dir, DirData}]) ->
    Mod = list_to_atom(httpd_conf:clean(ModuleName)),
    {ok, [{security_directory, Dir, [{callback_module, Mod}|DirData]}]};
load([$S,$e,$c,$u,$r,$i,$t,$y,$M,$a,$x,$R,$e,$t,$r,$i,$e,$s,$ |Retries],
     [{security_directory, Dir, DirData}]) ->
    MaxRetries = httpd_conf:clean(Retries),
    load_return_int_tag("SecurityMaxRetries", max_retries, 
			httpd_conf:clean(Retries), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$B,$l,$o,$c,$k,$T,$i,$m,$e,$ |Time],
     [{security_directory, Dir, DirData}]) ->
    load_return_int_tag("SecurityBlockTime", block_time,
			httpd_conf:clean(Time), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$F,$a,$i,$l,$E,$x,$p,$i,$r,$e,$T,$i,$m,$e,$ |Time],
     [{security_directory, Dir, DirData}]) ->
    load_return_int_tag("SecurityFailExpireTime", fail_expire_time,
			httpd_conf:clean(Time), Dir, DirData);
load([$S,$e,$c,$u,$r,$i,$t,$y,$A,$u,$t,$h,$T,$i,$m,$e,$o,$u,$t,$ |Time0],
     [{security_directory, Dir, DirData}]) ->
    Time = httpd_conf:clean(Time0),
    load_return_int_tag("SecurityAuthTimeout", auth_timeout,
			httpd_conf:clean(Time), Dir, DirData);
load([$A,$u,$t,$h,$N,$a,$m,$e,$ |Name0],
     [{security_directory, Dir, DirData}]) ->
    Name = httpd_conf:clean(Name0),
    {ok, [{security_directory, Dir, [{auth_name, Name}|DirData]}]};
load("</Directory>",[{security_directory,Directory, DirData}]) ->
    {ok, [], {security_directory, Directory, DirData}}.

load_return_int_tag(Name, Atom, Time, Dir, DirData) ->
    case Time of
	"infinity" ->
	    {ok, [{security_directory, Dir, [{Atom, 99999999999999999999999999999}|DirData]}]};
	Int ->
	    case catch list_to_integer(Time) of
		{'EXIT', _} ->
		    {error, Time++" is an invalid "++Name};
		Val ->
		    {ok, [{security_directory, Dir, [{Atom, Val}|DirData]}]}
	    end
    end.

store({security_directory, Dir0, DirData}, ConfigList) ->
    Port = httpd_util:key1search(ConfigList, port),
    start(Port),
    SR = httpd_util:key1search(ConfigList, server_root),
    Dir = 
	case filename:pathtype(Dir0) of
	    relative ->
		filename:join(SR, Dir0);
	    _ ->
		Dir0
	end,
    case httpd_util:key1search(DirData, data_file, no_data_file) of
	no_data_file ->
	    {error, no_security_data_file};
	DataFile0 ->
	    DataFile = 
		case filename:pathtype(DataFile0) of
		    relative ->
			filename:join(SR, DataFile0);
		    _ ->
			DataFile0
		end,
	    case new_table(Port, DataFile) of
		{ok, TwoTables} ->
		    NewDirData0 = lists:keyreplace(data_file, 1, DirData, 
						   {data_file, TwoTables}),
		    {ok, {security_directory, [{port, Port}|NewDirData0]}};
		{error, Err} ->
		    {error, {{open_data_file, DataFile}, Err}}
	    end
    end.


remove(ConfigDB) ->
    [{port, Port}] = ets:lookup(ConfigDB, port),
    delete_tables(Port),
    stop(Port).
    


%%
%% The gen_server code.
%%
%% A gen_server is needed in this module to take care of shared access to the data file
%% used to store failed and successful authentications aswell as user blocks.
%%
%% The storage model is a write-through model with both an ets and a dets table.
%% Writes are done to both the ets and then the dets table, but reads are only
%% done from the ets table.
%%
%% This approach also enables parallelism when using dets by returning the same dets
%% table identifier when opening several files with the same physical location.
%%
%% NOTE: This could be implemented using a single dets table, as it is possible to
%%       open a dets file with the ram_file flag, but this would require periodical
%%       sync's to disk, and it would be hard to decide when such an operation should
%%       occur.
%%

%%
%% gen_server internal API functions
%%
start(Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    case whereis(SName) of
	undefined ->
	    gen_server:start({local, SName}, ?MODULE, [], [{timeout, infinity}]);
	_ ->
	    ok
    end.

block_user(User, Port, Dir, Time) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {block_user, User, Port, Dir, Time}).

list_blocked_users(Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {list_blocked_users, Port, '_'}).

list_blocked_users(Port, Dir) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {list_blocked_users, Port, Dir}).

unblock_user(User, Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {unblock_user, User, Port, '_'}).

unblock_user(User, Port, Dir) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {unblock_user, User, Port, Dir}).

list_auth_users(Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {list_auth_users, Port}).

list_auth_users(Port, Dir) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {list_auth_users, Port, Dir}).
    
    
stop(Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    case whereis(SName) of
	undefined ->
	    ok;
	Pid ->
	    gen_server:call(SName, stop)
    end.

delete_tables(Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    case whereis(SName) of
	undefined ->
	    ok;
	_ ->
	    gen_server:call(SName, delete_tables)
    end.

new_table(Port, Name) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {new_table, Name}, infinity).

store_failed_auth(Info, Port, DecodedString, SDirData) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:cast(SName, {store_failed_auth, [Info, DecodedString, SDirData]}).

check_blocked_user(Info, User, SDirData, Port) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:call(SName, {check_blocked_user, [Info, User, SDirData]}).

store_successful_auth(Port, User, SDirData) ->
    SName = list_to_atom("mod_security_gen_server_"++integer_to_list(Port)),
    gen_server:cast(SName, {store_successful_auth, [User, Port, SDirData]}).
    
%%
%% gen_server callback functions.
%%
init(Args) ->
    process_flag(trap_exit, true),
    {ok, []}.

%% handle_info

handle_info(Info, State) ->
    {noreply, State}.

%% handle_call

handle_call(stop, _From, Tables) ->
    {stop, normal, ok, []};

handle_call({block_user, User, Port, Dir, Time}, _From, Tables) ->
    Ret = block_user_int({User, Port, Dir, Time}),
    {reply, Ret, Tables};

handle_call({list_blocked_users, Port, Dir}, _From, Tables) ->
    Blocked = list_blocked(Tables, Port, Dir, []),
    {reply, Blocked, Tables};

handle_call({unblock_user, User, Port, Dir}, _From, Tables) ->
    Ret = unblock_user_int({User, Port, Dir}),
    {reply, Ret, Tables};

handle_call({list_auth_users, Port}, _From, Tables) ->
    Auth = list_auth(Tables, Port, '_', []),
    {reply, Auth, Tables};

handle_call({list_auth_users, Port, Dir}, _From, Tables) ->
    Auth = list_auth(Tables, Port, Dir, []),
    {reply, Auth, Tables};

handle_call({new_table, Name}, _From, Tables) ->
    case lists:keysearch(Name, 1, Tables) of
	{value, {Name, {Ets, Dets}}} ->
	    {reply, {ok, {Ets, Dets}}, Tables};
	false ->
	    TName = list_to_atom("mod_security_data_"++integer_to_list(length(Tables))),
	    case dets:open_file(TName, [{type, bag}, {file, Name}, {repair, true}, {access, read_write}]) of
		{ok, DFile} ->
		    ETS = ets:new(TName, [bag, private]),
		    sync_dets_to_ets(DFile, ETS),
		    NewTables = [{Name, {ETS, DFile}}|Tables],
		    {reply, {ok, {ETS, DFile}}, NewTables};
		{error, Err} ->
		    {reply, {error, {create_dets, Err}}, Tables}
	    end
    end;

handle_call(delete_tables, _From, Tables) ->
    lists:foreach(fun({Name, {ETS, DETS}}) ->
			  dets:close(DETS),
			  ets:delete(ETS)
		  end, Tables),
    {reply, ok, []};

handle_call({check_blocked_user, [Info, User, SDirData]}, _From, Tables) ->
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir = httpd_util:key1search(SDirData, path),
    Port = httpd_util:key1search(SDirData, port),
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    Ret = check_blocked_user(Info, User, Dir, Port, ETS, DETS, CBModule),
    {reply, Ret, Tables}.
  
handle_cast({store_failed_auth, [Info, DecodedString, SDirData]}, Tables) ->
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir = httpd_util:key1search(SDirData, path),
    Port = httpd_util:key1search(SDirData, port),
    {ok, [User,Password]} = httpd_util:split(DecodedString,":",2),
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Key = {User, Dir, Port},

    %% Event
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    (catch CBModule:event(auth_fail, Port, Dir, [{user, User}, {password, Password}])),
    
    %% Find out if any of this user's other failed logins are too old to keep..
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	[] ->
	    no;
	List when list(List) ->
	    ExpireTime = httpd_util:key1search(SDirData, fail_expire_time, 30)*60,
	    lists:map(fun({failed, {TheKey, LS, Gen}}) ->
			      Diff = Seconds-LS,
			      if
				  Diff > ExpireTime ->
				      ets:match_delete(ETS, {failed, {TheKey, LS, Gen}}),
				      dets:match_delete(DETS, {failed, {TheKey, LS, Gen}});
				  true ->
				      ok
			      end
		      end,
		      List);
	_ ->
	    no
    end,

    %% Insert the new failure..
    Generation = length(ets:match_object(ETS, {failed, {Key, '_', '_'}})),
    ets:insert(ETS, {failed, {Key, Seconds, Generation}}),
    dets:insert(DETS, {failed, {Key, Seconds, Generation}}),
    
    %% See if we should block this user..
    MaxRetries = httpd_util:key1search(SDirData, max_retries, 3),
    BlockTime = httpd_util:key1search(SDirData, block_time, 60),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	List1 ->
	    if 
		length(List1) >= MaxRetries ->
		    %% Block this user until Future
		    Future = Seconds+BlockTime*60,
		    Reason = io_lib:format("Blocking user ~s from dir ~s for ~p minutes", 
					   [User, Dir, BlockTime]),
		    mod_log:security_log(Info, lists:flatten(Reason)),
		    
		    %% Event
		    (catch CBModule:event(user_block, Port, Dir, [{user, User}])),
		    
		    ets:match_delete(ETS, {blocked_user, {User, Port, Dir, '$1'}}),
		    dets:match_delete(DETS, {blocked_user, {User, Port, Dir, '$1'}}),
		    BlockRecord = {blocked_user, {User, Port, Dir, Future}},
		    ets:insert(ETS, BlockRecord),
		    dets:insert(DETS, BlockRecord),
		    %% Remove previous failed requests.
		    ets:match_delete(ETS, {failed, {Key, '_', '_'}}),
		    dets:match_delete(DETS, {failed, {Key, '_', '_'}});
		true ->
		    no
	    end;
	Other ->
	    no
    end,
    {noreply, Tables};

handle_cast({store_successful_auth, [User, Port, SDirData]}, Tables) ->
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    AuthTimeOut = httpd_util:key1search(SDirData, auth_timeout, 30),
    Dir = httpd_util:key1search(SDirData, path),
    Key = {User, Dir, Port},
    %% Remove failed entries for this Key
    dets:match_delete(DETS, {failed, {Key, '_', '_'}}),
    ets:match_delete(ETS, {failed, {Key, '_', '_'}}), 
    %% Keep track of when the last successful login took place.
    Seconds = calendar:datetime_to_gregorian_seconds(calendar:universal_time())+AuthTimeOut,
    ets:match_delete(ETS, {success, {Key, '_'}}),
    dets:match_delete(DETS, {success, {Key, '_'}}),
    ets:insert(ETS, {success, {Key, Seconds}}),
    dets:insert(DETS, {success, {Key, Seconds}}),
    {noreply, Tables};
	    
handle_cast(Req, Tables) ->
    Str=lists:flatten(io_lib:format("mod_security gen_server got unknown cast: ~p",[Req])),
    error_logger:error_report(Str),
    {noreply, Tables}.

terminate(_Reason, _Tables) ->
    ok.


%% block_user_int/2
block_user_int({User, Port, Dir, Time}) ->
    Dirs = httpd_listener:config_match(Port, {security_directory, '_'}),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    Time1 = 
		case Time of
		    infinity ->
			99999999999999999999999999999;
		    _ ->
			Time
		end,
	    Future = calendar:datetime_to_gregorian_seconds(
		       calendar:universal_time())+Time1,
	    ets:match_delete(ETS, {blocked_user, {User, Port, Dir, '_'}}),
	    dets:match_delete(DETS, {blocked_user, {User, Port, Dir, '_'}}),
	    ets:insert(ETS, {blocked_user, {User, Port, Dir, Future}}),
	    dets:insert(DETS, {blocked_user, {User, Port, Dir, Future}}),
	    CBModule = httpd_util:key1search(DirData, callback_module, 
					     no_module_at_all),
	    (catch CBModule:event(user_block, Port, Dir, [{user, User}])),
	    true;
	_ ->
	    {error, no_such_directory}
    end.
    

find_dirdata([], _Dir) ->
    false;
find_dirdata([{security_directory, DirData}|SDirs], Dir) ->
    case lists:keysearch(path, 1, DirData) of
	{value, {path, Dir}} ->
	    {value, {data_file, {ETS, DETS}}} =
		lists:keysearch(data_file, 1, DirData),
	    {ok, DirData, {ETS, DETS}};
	_ ->
	    find_dirdata(SDirs, Dir)
    end.

%% unblock_user_int/2

unblock_user_int({User, Port, Dir}) ->
    Dirs = httpd_listener:config_match(Port, {security_directory, '_'}),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    case ets:match_object(ETS, {blocked_user, {User, Port, Dir, '_'}}) of
		[] ->
		    {error, not_blocked};
		Objects ->
		    ets:match_delete(ETS, {blocked_user,
					   {User, Port, Dir, '_'}}),
		    dets:match_delete(DETS, {blocked_user,
					     {User, Port, Dir, '_'}}),
	       	    CBModule = httpd_util:key1search(DirData, callback_module, 
						     no_module_at_all),
		    (catch CBModule:event(user_unblock, Port, Dir,
					  [{user, User}])),
		    true
	    end;
	_ ->
	    {error, no_such_directory}
    end.



%% list_auth/2

list_auth([], _Port, Dir, Acc) ->
    Acc;
list_auth([{Name, {ETS, DETS}}|Tables], Port, Dir, Acc) ->
    case ets:match_object(ETS, {success, {{'_', Dir, Port}, '_'}}) of
	[] ->
	    list_auth(Tables, Port, Dir, Acc);
	List when list(List) ->
	    TN = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	    NewAcc = lists:foldr(fun({success, {{U, P, D}, T}}, A) -> 
					 if
					     T-TN > 0 ->
						 [U|A];
					     true ->
						 ets:match_delete(ETS,
								  {success, {{U,P,D}, T}}),
						 dets:match_delete(DETS,
								   {success, {{U,P,D}, T}}),
						 A
					 end
				 end,
				 Acc, List),
	    list_auth(Tables, Port, Dir, NewAcc);
	_ ->
	    list_auth(Tables, Port, Dir, Acc)
    end.


%% list_blocked/2

list_blocked([], Port, Dir, Acc) ->
    TN = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    lists:foldl(fun({U,P,D,T}, A) ->
			if
			    T-TN > 0 ->
				[{U,P,D,calendar:universal_time_to_local_time(
					  calendar:gregorian_seconds_to_datetime(T))}|A];
			    true ->
				A
			end
		end, [], Acc);
list_blocked([{Name, {ETS, DETS}}|Tables], Port, Dir, Acc) ->
    NewBlocked = 
	case ets:match_object(ETS, {blocked_user, {'_', Port, Dir, '_'}}) of
	    List when list(List) ->
		lists:foldl(fun({blocked_user, X}, A) -> [X|A] end, Acc, List);
	    _ ->
		Acc
	end,
    list_blocked(Tables, Port, Dir, NewBlocked).
    

%%
%% sync_dets_to_ets/2
%%
%% Reads dets-table DETS and syncronizes it with the ets-table ETS.
%%
sync_dets_to_ets(DETS, ETS) ->
    dets:traverse(DETS, fun(X) ->
				ets:insert(ETS, X),
				continue
			end).

%%
%% check_blocked_user/7 -> true | false
%%
%% Check if a specific user is blocked from access.
%%
%% The sideeffect of this routine is that it unblocks also other users
%% whos blocking time has expired. This to keep the tables as small
%% as possible.
%%
check_blocked_user(Info, User, Dir, Port, ETS, DETS, CBModule) ->
    TN = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    case ets:match_object(ETS, {blocked_user, {User, '_', '_', '_'}}) of
	List when list(List) ->
	    Blocked = lists:foldl(fun({blocked_user, X}, A) ->
					  [X|A] end, [], List),
	    check_blocked_user(Info, User, Dir, Port, ETS, DETS, TN, Blocked, CBModule);
	_ ->
	    false
    end.
check_blocked_user(Info, User, Dir, Port, ETS, DETS, TN, [], CBModule) ->
    false;
check_blocked_user(Info, User, Dir, Port, ETS, DETS, TN, [{User,Port,Dir,T}|Ls], CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove and grant access.
	    unblock_user(Info, User, Dir, Port, ETS, DETS, CBModule),
	    false;
	true ->
	    true
    end;
check_blocked_user(Info, User, Dir, Port, ETS, DETS, TN, [{OUser,ODir,OPort,T}|Ls], 
		   CBModule) ->
    TD = T-TN,
    if
	TD =< 0 ->
	    %% Blocking has expired, remove.
	    unblock_user(Info, OUser, ODir, OPort, ETS, DETS, CBModule);
	true ->
	    true
    end,
    check_blocked_user(Info, User, Dir, Port, ETS, DETS, TN, Ls, CBModule).

unblock_user(Info, User, Dir, Port, ETS, DETS, CBModule) ->
    Reason = io_lib:format("User ~s was removed from the block list for dir ~s",
			   [User, Dir]),
    mod_log:security_log(Info, lists:flatten(Reason)),
    (catch CBModule:event(user_unblock, Port, Dir, [{user, User}])),
    dets:match_delete(DETS, {blocked_user, {User, Port, Dir, '_'}}),
    ets:match_delete(ETS, {blocked_user, {User, Port, Dir, '_'}}).
  
