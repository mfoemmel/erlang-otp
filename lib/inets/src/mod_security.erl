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
-export([list_blocked_users/1, list_blocked_users/2, list_blocked_users/3, 
	 block_user/4, block_user/5, 
	 unblock_user/2, unblock_user/3, unblock_user/4,
	 list_auth_users/1, list_auth_users/2, list_auth_users/3]).

%% module API exports
-export([do/1, load/2, store/2, remove/1]).

%% gen_server exports
-export([stop/1, stop/2, init/1, 
	 handle_info/2, handle_call/3, handle_cast/2, 
	 terminate/2,
	 code_change/3]).

-export([verbosity/3]).

-include("httpd.hrl").

-define(VMODULE,"SEC").
-include("httpd_verbosity.hrl").


%% do/1
do(Info) ->
    ?DEBUG("do -> entry with ~n   Info: ~p",[Info]),
    ?vdebug("~n   do with ~n   Info: ~p",[Info]),
    %% Check and see if any user has been authorized.
    case httpd_util:key1search(Info#mod.data,remote_user,not_defined_user) of
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
			    ?LOG("do -> authentication failed: ~s",
				 [EncodedString]),
			    ?vlog("~n   Authentication failed: ~s",
				  [EncodedString]),
			    report_failed(Info, EncodedString),
			    take_failed_action(Info, EncodedString),
			    {proceed, Info#mod.data}
		    end;
		_ ->
		    {proceed, Info#mod.data}
	    end;
	User ->
	    %% A user has been authenticated, now is he blocked ?
	    ?DEBUG("do -> User authenticated: ~p",[User]),
	    ?vtrace("User '~p' authentication",[User]),
	    Path = mod_alias:path(Info#mod.data,Info#mod.config_db,
				  Info#mod.request_uri),
	    {Dir, SDirData} = secretp(Path, Info#mod.config_db),
	    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
	    Port = httpd_util:lookup(Info#mod.config_db, port),
	    DF = httpd_util:key1search(SDirData, data_file),
	    case check_blocked_user(Info, User, SDirData, Addr, Port) of
		true ->
		    ?DEBUG("do -> user blocked",[]),
		    ?vtrace("User blocked",[]),
		    {proceed, [{status, {403, Info#mod.request_uri, ""}}|Info#mod.data]};
		false ->
		    ?DEBUG("do -> user not blocked",[]),
		    ?vtrace("User not blocked",[]),
		    store_successful_auth(Addr, Port, User, SDirData),
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
    Addr = httpd_util:lookup(Info#mod.config_db, bind_address),
    Port = httpd_util:lookup(Info#mod.config_db, port),
    DecodedString = httpd_util:decode_base64(EncodedString),
    store_failed_auth(Info, Addr, Port, DecodedString, SDirData).

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
    ?CDEBUG("store(security_directory) -> ~n"
	    "      Dir0:       ~p~n"
	    "      DirData:    ~p",
	    [Dir0, DirData]),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Port = httpd_util:key1search(ConfigList, port),
    start(Addr,Port),
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
	    case new_table(Addr,Port,DataFile) of
		{ok, TwoTables} ->
		    NewDirData0 = lists:keyreplace(data_file, 1, DirData, 
						   {data_file, TwoTables}),
		    NewDirData1 = case Addr of
				      undefined ->
					  [{port,Port}|NewDirData0];
				      _ ->
					  [{port,Port},{bind_address,Addr}|
					   NewDirData0]
				  end,
		    {ok, {security_directory,NewDirData1}};
		{error, Err} ->
		    {error, {{open_data_file, DataFile}, Err}}
	    end
    end.


remove(ConfigDB) ->
    Addr = case ets:lookup(ConfigDB, bind_address) of
	       [] -> 
		   undefined;
	       [{bind_address, Address}] ->
		   Address
	   end,
    [{port, Port}] = ets:lookup(ConfigDB, port),
    delete_tables(Addr,Port),
    stop(Addr,Port).
    


%%
%% The gen_server code.
%%
%% A gen_server is needed in this module to take care of shared access to the
%% data file used to store failed and successful authentications aswell as 
%% user blocks.
%%
%% The storage model is a write-through model with both an ets and a dets 
%% table. Writes are done to both the ets and then the dets table, but reads 
%% are only done from the ets table.
%%
%% This approach also enables parallelism when using dets by returning the 
%% same dets table identifier when opening several files with the same 
%% physical location.
%%
%% NOTE: This could be implemented using a single dets table, as it is 
%%       possible to open a dets file with the ram_file flag, but this 
%%       would require periodical sync's to disk, and it would be hard 
%%       to decide when such an operation should occur.
%%

%%
%% gen_server internal API functions
%%
start(Addr,Port) ->
    SName = make_name(Addr,Port),
    Verbosity = get(security_verbosity),
    case whereis(SName) of
	undefined ->
	    ?LOG("start -> ~n"
		 "      Addr: ~p~n"
		 "      Port: ~p",
		 [Addr,Port]),
	    gen_server:start({local,SName},?MODULE,[Verbosity],
			     [{timeout,infinity}]);
	_ ->
	    ok
    end.

verbosity(Addr,Port,Verbosity) ->
    SName = make_name(Addr,Port),
    case (catch gen_server:call(SName, {verbosity,Verbosity})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	OldVerbosity ->
	    OldVerbosity
    end.
    
stop(Port) ->
    stop(undefined,Port).
stop(Addr,Port) ->
    ?LOG("stop -> ~n"
	 "     Addr: ~p~n"
	 "     Port: ~p",[Addr,Port]),
    SName = make_name(Addr,Port),
    case whereis(SName) of
	undefined ->
	    ok;
	Pid ->
	    gen_server:call(SName, stop)
    end.

block_user(User, Port, Dir, Time) ->
    block_user(User, undefined, Port, Dir, Time).
block_user(User, Addr, Port, Dir, Time) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {block_user, User, Addr, Port, Dir, Time}).

unblock_user(User, Port) ->
    unblock_user(User, undefined, Port).
unblock_user(User, Port, Dir) when integer(Port) ->
    unblock_user(User, undefined, Port, Dir);
unblock_user(User, Addr, Port) when integer(Port) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {unblock_user, User, Addr, Port, '_'}).
unblock_user(User, Addr, Port, Dir) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {unblock_user, User, Addr, Port, Dir}).

list_blocked_users(Port) ->
    list_blocked_users(undefined,Port).
list_blocked_users(Port,Dir) when integer(Port) ->
    list_blocked_users(undefined,Port,Dir);
list_blocked_users(Addr,Port) when integer(Port) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {list_blocked_users, Addr, Port, '_'}).
list_blocked_users(Addr, Port, Dir) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {list_blocked_users, Addr, Port, Dir}).

list_auth_users(Port) ->
    list_auth_users(undefined,Port).
list_auth_users(Port,Dir) when integer(Port) ->
    list_auth_users(undefined,Port,Dir);
list_auth_users(Addr,Port) when integer(Port) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {list_auth_users, Addr, Port}).
list_auth_users(Addr, Port, Dir) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {list_auth_users, Addr, Port, Dir}).
    
    
delete_tables(Addr,Port) ->
    SName = make_name(Addr,Port),
    case whereis(SName) of
	undefined ->
	    ok;
	_ ->
	    gen_server:call(SName, delete_tables)
    end.

new_table(Addr, Port, Name) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {new_table, Addr, Port, Name}, infinity).

store_failed_auth(Info, Addr, Port, DecodedString, SDirData) ->
    SName = make_name(Addr,Port),
    gen_server:cast(SName,{store_failed_auth,[Info,DecodedString,SDirData]}).

check_blocked_user(Info, User, SDirData, Addr, Port) ->
    SName = make_name(Addr,Port),
    gen_server:call(SName, {check_blocked_user, [Info, User, SDirData]}).

store_successful_auth(Addr, Port, User, SDirData) ->
    SName = make_name(Addr,Port),
    gen_server:cast(SName, {store_successful_auth, [User,Addr,Port,SDirData]}).
    
%%
%% gen_server callback functions.
%%
init([undefined]) ->
    init([?default_verbosity]);
init([Verbosity]) ->
    ?DEBUG("init -> entry with Verbosity: ~p",[Verbosity]),
    process_flag(trap_exit, true),
    put(sname,sec),
    put(verbosity,Verbosity),
    ?vlog("starting",[]),
    {ok, []}.

%% handle_info

handle_info(Info, State) ->
    ?LOG("handle_info -> ~n"
	 "       Info:  ~p~n"
	 "       State: ~p",
	 [Info,State]),
    ?vinfo("~n   unknown info '~p'",[Info]),
    {noreply, State}.

%% handle_call

handle_call(stop, _From, Tables) ->
    ?LOG("handle_call(stop) -> Tables: ~p",[Tables]),
    ?vlog("stop",[]),
    {stop, normal, ok, []};

handle_call({verbosity,Verbosity}, _From, Tables) ->
    ?vlog("set verbosity to ~p",[Verbosity]),
    OldVerbosity = get(verbosity),
    put(verbosity,Verbosity),
    ?vdebug("old verbosity: ~p",[OldVerbosity]),
    {reply,OldVerbosity,Tables};

handle_call({block_user, User, Addr, Port, Dir, Time}, _From, Tables) ->
    ?LOG("handle_call(block_user) -> ~n"
	 "       User: ~p~n"
	 "       Addr: ~p~n"
	 "       Port: ~p~n"
	 "       Dir:  ~p~n"
	 "       Time: ~p",
	 [User,Addr,Port,Dir,Time]),
    ?vlog("block user '~p' for ~p",[User,Dir]),
    Ret = block_user_int({User, Addr, Port, Dir, Time}),
    ?DEBUG("handle_call(block_user) -> Ret: ~p",[Ret]),
    ?vdebug("block user result: ~p",[Ret]),
    {reply, Ret, Tables};

handle_call({list_blocked_users, Addr, Port, Dir}, _From, Tables) ->
    ?LOG("handle_call(list_blocked_users) -> ~n"
	 "       Addr: ~p~n"
	 "       Port: ~p~n"
	 "       Dir:  ~p",[Addr,Port,Dir]),
    ?vlog("list blocked users for ~p",[Dir]),
    Blocked = list_blocked(Tables, Addr, Port, Dir, []),
    ?DEBUG("handle_call(list_blocked_users) -> Blocked: ~p",[Blocked]),
    ?vdebug("list blocked users: ~p",[Blocked]),
    {reply, Blocked, Tables};

handle_call({unblock_user, User, Addr, Port, Dir}, _From, Tables) ->
    ?LOG("handle_call(unblock_user) -> ~n"
	 "       User: ~p~n"
	 "       Addr: ~p~n"
	 "       Port: ~p~n"
	 "       Dir:  ~p",
	 [User,Addr,Port,Dir]),
    ?vlog("unblock user '~p' for ~p",[User,Dir]),
    Ret = unblock_user_int({User, Addr, Port, Dir}),
    ?DEBUG("handle_call(unblock_user) -> Ret: ~p",[Ret]),
    ?vdebug("unblock user result: ~p",[Ret]),
    {reply, Ret, Tables};

handle_call({list_auth_users, Addr, Port}, _From, Tables) ->
    ?LOG("handle_call(list_auth_users) -> ~n"
	 "       Addr: ~p~n"
	 "       Port: ~p",
	 [Addr,Port]),
    ?vlog("list auth users",[]),
    Auth = list_auth(Tables, Addr, Port, '_', []),
    ?DEBUG("handle_call(list_auth_users) -> Auth: ~p",[Auth]),
    ?vdebug("list auth users result: ~p",[Auth]),
    {reply, Auth, Tables};

handle_call({list_auth_users, Addr, Port, Dir}, _From, Tables) ->
    ?LOG("handle_call(list_auth_users) -> ~n"
	 "       Addr: ~p~n"
	 "       Port: ~p~n"
	 "       Dir:  ~p",
	 [Addr,Port,Dir]),
    ?vlog("list auth users for ~p",[Dir]),
    Auth = list_auth(Tables, Addr, Port, Dir, []),
    ?DEBUG("handle_call(list_auth_users) -> Auth: ~p",[Auth]),
    ?vdebug("list auth users result: ~p",[Auth]),
    {reply, Auth, Tables};

handle_call({new_table, Addr, Port, Name}, _From, Tables) ->
    case lists:keysearch(Name, 1, Tables) of
	{value, {Name, {Ets, Dets}}} ->
	    ?DEBUG("handle_call(new_table) -> we already have this table: ~p",
		   [Name]),
	    ?vdebug("new table; we already have this one: ~p",[Name]),
	    {reply, {ok, {Ets, Dets}}, Tables};
	false ->
	    ?LOG("handle_call(new_table) -> new_table: Name = ~p",[Name]),
	    ?vlog("new table: ~p",[Name]),
	    TName = make_name(Addr,Port,length(Tables)),
	    ?DEBUG("handle_call(new_table) -> TName: ~p",[TName]),
	    ?vdebug("new table: ~p",[TName]),
	    case dets:open_file(TName, [{type, bag}, {file, Name}, 
					{repair, true}, 
					{access, read_write}]) of
		{ok, DFile} ->
		    ETS = ets:new(TName, [bag, private]),
		    sync_dets_to_ets(DFile, ETS),
		    NewTables = [{Name, {ETS, DFile}}|Tables],
		    ?DEBUG("handle_call(new_table) -> ~n"
			   "       NewTables: ~p",[NewTables]),
		    ?vtrace("new tables: ~p",[NewTables]),
		    {reply, {ok, {ETS, DFile}}, NewTables};
		{error, Err} ->
		    ?LOG("handle_call -> Err: ~p",[Err]),
		    ?vinfo("failed open dets file: ~p",[Err]),
		    {reply, {error, {create_dets, Err}}, Tables}
	    end
    end;

handle_call(delete_tables, _From, Tables) ->
    ?LOG("handle_call(delete_table) -> entry",[]),
    ?vlog("delete tables",[]),
    lists:foreach(fun({Name, {ETS, DETS}}) ->
			  dets:close(DETS),
			  ets:delete(ETS)
		  end, Tables),
    {reply, ok, []};

handle_call({check_blocked_user, [Info, User, SDirData]}, _From, Tables) ->
    ?LOG("handle_call(check_blocked_user) -> ~n"
	 "       User:     ~p~n"
	 "       SDirData: ~p",
	 [User,SDirData]),
    ?vlog("check blocked user '~p'",[User]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir = httpd_util:key1search(SDirData, path),
    Addr = httpd_util:key1search(SDirData, bind_address),
    Port = httpd_util:key1search(SDirData, port),
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    ?DEBUG("handle_call(check_blocked_user) -> CBModule: ~p",[CBModule]),
    ?vdebug("call back module: ~p",[CBModule]),
    Ret = check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule),
    ?DEBUG("handle_call(check_blocked_user) -> Ret: ~p",[Ret]),
    ?vdebug("check result: ~p",[Ret]),
    {reply, Ret, Tables};
handle_call(Request,From,Tables) ->
    ?vinfo("~n   unknown call '~p' from ~p",[Request,From]),
    {reply,ok,Tables}.
  
handle_cast({store_failed_auth, [Info, DecodedString, SDirData]}, Tables) ->
    ?LOG("handle_cast(store_failed_auth) -> ~n"
	 "       DecodedString: ~p~n"
	 "       SDirData:      ~p",
	 [DecodedString, SDirData]),
    ?vlog("store failed auth",[]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    Dir  = httpd_util:key1search(SDirData, path),
    Addr = httpd_util:key1search(SDirData, bind_address),
    Port = httpd_util:key1search(SDirData, port),
    {ok, [User,Password]} = httpd_util:split(DecodedString,":",2),
    ?DEBUG("handle_cast(store_failed_auth) -> ~n"
	   "       User:     ~p~n"
	   "       Password: ~p",[User,Password]),
    ?vdebug("user '~p' and password '~p'",[User,Password]),
    Seconds = universal_time(),
    Key = {User, Dir, Addr, Port},

    %% Event
    CBModule = httpd_util:key1search(SDirData, callback_module, no_module_at_all),
    ?vtrace("call back module: ~p",[CBModule]),
    auth_fail_event(CBModule,Addr,Port,Dir,User,Password),
    
    %% Find out if any of this user's other failed logins are too old to keep..
    ?DEBUG("handle_cast(store_failed_auth) -> All(ets) before delete: ~p",
	   [ets:match_object(ETS, '_')]),
    ?DEBUG("handle_cast(store_failed_auth) -> Remove old login failures",[]),
    ?vtrace("remove old login failures",[]),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	[] ->
	    ?DEBUG("handle_cast(store_failed_auth) -> []",[]),
	    ?vtrace("no old login failures",[]),
	    no;
	List when list(List) ->
	    ?DEBUG("handle_cast(store_failed_auth) -> ~n"
		   "       List: ~p",[List]),
	    ?vtrace("~p old login failures",[length(List)]),
	    ExpireTime = httpd_util:key1search(SDirData, fail_expire_time, 30)*60,
	    ?DEBUG("handle_cast(store_failed_auth) -> ExpireTime: ~p",[ExpireTime]),
	    ?vtrace("expire time ~p",[ExpireTime]),
	    lists:map(fun({failed, {TheKey, LS, Gen}}) ->
			      Diff = Seconds-LS,
			      ?DEBUG("handle_cast(store_failed_auth) -> Diff: ~p",
				     [Diff]),
			      if
				  Diff > ExpireTime ->
				      ?DEBUG("handle_cast(store_failed_auth) -> "
					     "to old to keep: ~p",[Gen]),
				      ?vtrace("~n   '~p' is to old to keep: ~p",
					      [TheKey,Gen]),
				      ets:match_delete(ETS, {failed, {TheKey, LS, Gen}}),
				      dets:match_delete(DETS, {failed, {TheKey, LS, Gen}});
				  true ->
				      ?DEBUG("handle_cast(store_failed_auth) -> "
					     "not old enough, keep",[]),
				      ?vtrace("~n   '~p' is not old enough: ~p",
					      [TheKey,Gen]),
				      ok
			      end
		      end,
		      List);
	O ->
	    ?DEBUG("handle_cast(store_failed_auth) -> ~n"
		   "       O: ~p",[O]),
	    ?vlog("~n   unknown login failure search resuylt: ~p",[O]),
	    no
    end,

    ?DEBUG("handle_cast(store_failed_auth) -> All(ets) after delete: ~p",
	   [ets:match_object(ETS, '_')]),

    %% Insert the new failure..
    Generation = length(ets:match_object(ETS, {failed, {Key, '_', '_'}})),
    ?DEBUG("handle_cast(store_failed_auth) -> Insert new login failure: ~p",
	   [Generation]),
    ?vtrace("insert ('~p') new login failure: ~p",[Key,Generation]),
    ets:insert(ETS, {failed, {Key, Seconds, Generation}}),
    dets:insert(DETS, {failed, {Key, Seconds, Generation}}),
    
    ?DEBUG("handle_cast(store_failed_auth) -> All(ets) after insert: ~p",
	   [ets:match_object(ETS, '_')]),

    %% See if we should block this user..
    MaxRetries = httpd_util:key1search(SDirData, max_retries, 3),
    ?DEBUG("handle_cast(store_failed_auth) -> MaxRetries: ~p",[MaxRetries]),
    BlockTime = httpd_util:key1search(SDirData, block_time, 60),
    ?DEBUG("handle_cast(store_failed_auth) -> BlockTime: ~p",[BlockTime]),
    ?vtrace("~n   Max retries ~p, block time ~p",[MaxRetries,BlockTime]),
    case ets:match_object(ETS, {failed, {Key, '_', '_'}}) of
	List1 ->
	    ?DEBUG("handle_cast(store_failed_auth) -> tries so far: ~p",
		   [length(List1)]),
	    ?vtrace("~n   ~p tries so far",[length(List1)]),
	    if 
		length(List1) >= MaxRetries ->
		    %% Block this user until Future
		    ?DEBUG("handle_cast(store_failed_auth) -> block this user "
			   "(~s)",[User]),
		    ?vtrace("block user '~p'",[User]),
		    Future = Seconds+BlockTime*60,
		    ?DEBUG("handle_cast(store_failed_auth) -> Future: ~p",
			   [Future]),
		    ?vtrace("future: ~p",[Future]),
		    Reason = io_lib:format("Blocking user ~s from dir ~s "
					   "for ~p minutes", 
					   [User, Dir, BlockTime]),
		    mod_log:security_log(Info, lists:flatten(Reason)),
		    
		    %% Event
		    user_block_event(CBModule,Addr,Port,Dir,User),
		    
		    ets:match_delete(ETS,{blocked_user,
					  {User, Addr, Port, Dir, '$1'}}), 
		    dets:match_delete(DETS, {blocked_user,
					     {User, Addr, Port, Dir, '$1'}}),
		    BlockRecord = {blocked_user, 
				   {User, Addr, Port, Dir, Future}},
		    ets:insert(ETS, BlockRecord),
		    dets:insert(DETS, BlockRecord),
		    %% Remove previous failed requests.
		    ets:match_delete(ETS, {failed, {Key, '_', '_'}}),
		    dets:match_delete(DETS, {failed, {Key, '_', '_'}});
		true ->
		    ?DEBUG("handle_cast(store_failed_auth) -> "
			   "still some tries to go",[]),
		    ?vtrace("still some tries to go",[]),
		    no
	    end;
	Other ->
	    no
    end,
    {noreply, Tables};

handle_cast({store_successful_auth, [User, Addr, Port, SDirData]}, Tables) ->
    ?LOG("handle_cast(store_successful_auth) -> ~n"
	 "       User:     ~p~n"
	 "       Addr:     ~p~n"
	 "       Port:     ~p~n"
	 "       SDirData: ~p",
	 [User,Addr,Port,SDirData]),
    ?vlog("store successfull auth",[]),
    {ETS, DETS} = httpd_util:key1search(SDirData, data_file),
    AuthTimeOut = httpd_util:key1search(SDirData, auth_timeout, 30),
    Dir = httpd_util:key1search(SDirData, path),
    Key = {User, Dir, Addr, Port},
    %% Remove failed entries for this Key
    dets:match_delete(DETS, {failed, {Key, '_', '_'}}),
    ets:match_delete(ETS, {failed, {Key, '_', '_'}}), 
    %% Keep track of when the last successful login took place.
    Seconds = universal_time()+AuthTimeOut,
    ets:match_delete(ETS, {success, {Key, '_'}}),
    dets:match_delete(DETS, {success, {Key, '_'}}),
    ets:insert(ETS, {success, {Key, Seconds}}),
    dets:insert(DETS, {success, {Key, Seconds}}),
    {noreply, Tables};
	    
handle_cast(Req, Tables) ->
    ?LOG("handle_cast -> ~n"
	 "       Req: ~p",[Req]),
    ?vinfo("~n   unknown cast '~p'",[Req]),
    Str=lists:flatten(io_lib:format("mod_security server got unknown cast: ~p",[Req])),
    error_logger:error_report(Str),
    {noreply, Tables}.

terminate(Reason, _Tables) ->
    ?LOG("terminate -> entry with Reason ~p",[Reason]),
    ?vlog("~n   Terminating for reason: ~p",[Reason]),
    ok.


%% code_change({down,ToVsn}, State, Extra)
%%
%% NOTE 1:
%% Actually upgrade from 2.5.1 to 2.5.3 and downgrade from 
%% 2.5.3 to 2.5.1 is done with an application restart, so 
%% these function is actually never used. The reason for keeping
%% this stuff is only for future use.
%% 
%% NOTE 2:
%% The upgrade/downgrade has never been tested!!
%% 
code_change({down,169822921757293987708137157261718970447},State,Extra) ->
    ?vlog("~n   Downgrade to 2.5.1"
	  "~n   when state '~p'",[State]),
    downgrade_to_2_5_1(State),
    {ok,State};


%% code_change(FromVsn, State, Extra)
%%
code_change(169822921757293987708137157261718970447,State,Extra) ->
    ?vlog("~n   Upgrade from 2.5.1"
	  "~n   when state '~p'",[State]),
    upgrade_from_2_5_1(State),
    {ok,State}.


upgrade_from_2_5_1([]) ->
    ok;
upgrade_from_2_5_1([Table|Tables]) ->
    upgrade_from_2_5_1(Table),
    upgrade_from_2_5_1(Tables);
upgrade_from_2_5_1({ETS,DETS}) ->
    upgrade_from_2_5_1(failed,ETS,DETS),
    upgrade_from_2_5_1(blocked,ETS,DETS),
    upgrade_from_2_5_1(success,ETS,DETS),
    ok.

upgrade_from_2_5_1(Key,ETS,DETS) ->
    L = ets:match_object(ETS,{Key,'_'}),
    ets:match_delete(ETS,{Key,'_'}),
    dets:match_delete(DETS,{Key,'_'}),
    upgrade1_from_2_5_1(L,ETS,DETS).


upgrade1_from_2_5_1([],_ETS,_DETS) ->
    ok;
upgrade1_from_2_5_1([Rec|Recs],ETS,DETS) ->
    upgrade2_from_2_5_1(Rec,ETS,DETS),
    upgrade1_from_2_5_1(Recs,ETS,DETS);
upgrade1_from_2_5_1(_WhateverThisIs,_ETS,_DETS) ->
    ok.

upgrade2_from_2_5_1(Rec,ETS,DETS) ->
    UpdRec = upgrade3_from_2_5_1(Rec),
    ets:insert(ETS,UpdRec),
    dets:insert(DETS,UpdRec).

upgrade3_from_2_5_1({failed,{{User,Dir,Port},Sec,Gen}}) -> 
    {failed,{{User,Dir,undefined,Port},Sec,Gen}};
upgrade3_from_2_5_1({blocked,{User,Port,Dir,Future}}) -> 
    {blocked,{User,undefined,Port,Dir,Future}};
upgrade3_from_2_5_1({success,{User,Dir,Port}}) -> 
    {success,{User,Dir,undefined,Port}}.


downgrade_to_2_5_1([]) ->
    ok;
downgrade_to_2_5_1([Table|Tables]) ->
    downgrade_to_2_5_1(Table),
    downgrade_to_2_5_1(Tables);
downgrade_to_2_5_1({ETS,DETS}) ->
    downgrade_to_2_5_1(failed,ETS,DETS),
    downgrade_to_2_5_1(blocked,ETS,DETS),
    downgrade_to_2_5_1(success,ETS,DETS),
    ok.

downgrade_to_2_5_1(Key,ETS,DETS) ->
    L = ets:match_object(ETS,{Key,'_'}),
    ets:match_delete(ETS,{Key,'_'}),
    dets:match_delete(DETS,{Key,'_'}),
    downgrade1_to_2_5_1(L,ETS,DETS).


downgrade1_to_2_5_1([],_ETS,_DETS) ->
    ok;
downgrade1_to_2_5_1([Rec|Recs],ETS,DETS) ->
    downgrade2_to_2_5_1(Rec,ETS,DETS),
    downgrade1_to_2_5_1(Recs,ETS,DETS);
downgrade1_to_2_5_1(_WhateverThisIs,_ETS,_DETS) ->
    ok.

downgrade2_to_2_5_1(Rec,ETS,DETS) ->
    UpdRec = downgrade3_to_2_5_1(Rec),
    ets:insert(ETS,UpdRec),
    dets:insert(DETS,UpdRec).

downgrade3_to_2_5_1({failed,{{User,Dir,_Addr,Port},Sec,Gen}}) -> 
    {failed,{{User,Dir,Port},Sec,Gen}};
downgrade3_to_2_5_1({blocked,{User,_Addr,Port,Dir,Future}}) -> 
    {blocked,{User,Port,Dir,Future}};
downgrade3_to_2_5_1({success,{User,Dir,_Addr,Port}}) -> 
    {success,{User,Dir,Port}}.



%% block_user_int/2
block_user_int({User, Addr, Port, Dir, Time}) ->
    Dirs = httpd_manager:config_match(Addr, Port, {security_directory, '_'}),
    ?DEBUG("block_user_int -> Dirs: ~p",[Dirs]),
    ?vtrace("block '~p' for ~p during ~p",[User,Dir,Time]),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    Time1 = 
		case Time of
		    infinity ->
			99999999999999999999999999999;
		    _ ->
			Time
		end,
	    Future = universal_time()+Time1,
	    ets:match_delete(ETS, {blocked_user, {User,Addr,Port,Dir,'_'}}),
	    dets:match_delete(DETS, {blocked_user, {User,Addr,Port,Dir,'_'}}),
	    ets:insert(ETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
	    dets:insert(DETS, {blocked_user, {User,Addr,Port,Dir,Future}}),
	    CBModule = httpd_util:key1search(DirData, callback_module, 
					     no_module_at_all),
	    ?DEBUG("block_user_int -> CBModule: ~p",[CBModule]),
	    ?vtrace("call back module ~p",[CBModule]),
	    user_block_event(CBModule,Addr,Port,Dir,User),
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

unblock_user_int({User, Addr, Port, Dir}) ->
    ?DEBUG("unblock_user_int -> ~n"
	   "        User: ~p~n"
	   "        Addr: ~p~n"
	   "        Port: ~p~n"
	   "        Dir:  ~p",
	   [User,Addr,Port,Dir]),
    ?vtrace("unblock user '~p' for ~p",[User,Dir]),
    Dirs = httpd_manager:config_match(Addr, Port, {security_directory, '_'}),
    ?DEBUG("unblock_user_int -> ~n"
	   "        User: ~p~n"
	   "        Port: ~p~n"
	   "        Dir:  ~p",
	   [User,Port,Dir]),
    ?DEBUG("unblock_user_int -> Dirs: ~p",[Dirs]),
    ?vtrace("~n   dirs: ~p",[Dirs]),
    case find_dirdata(Dirs, Dir) of
	{ok, DirData, {ETS, DETS}} ->
	    case ets:match_object(ETS,{blocked_user,{User,Addr,Port,Dir,'_'}}) of
		[] ->
		    ?DEBUG("unblock_user_int -> not blocked",[]),
		    ?vtrace("not blocked",[]),
		    {error, not_blocked};
		Objects ->
		    ?DEBUG("unblock_user_int -> ~n"
			   "        Objects: ~p",[Objects]),
		    ets:match_delete(ETS, {blocked_user,
					   {User, Addr, Port, Dir, '_'}}),
		    dets:match_delete(DETS, {blocked_user,
					     {User, Addr, Port, Dir, '_'}}),
	       	    CBModule = httpd_util:key1search(DirData, callback_module, 
						     no_module_at_all),
		    ?DEBUG("unblock_user_int -> CBModule: ~p",[CBModule]),
		    user_unblock_event(CBModule,Addr,Port,Dir,User),
		    true
	    end;
	_ ->
	    ?DEBUG("unblock_user_int -> no such directory",[]),
	    ?vlog("~n   cannot unblock: no such directory '~p'",[Dir]),
	    {error, no_such_directory}
    end.



%% list_auth/2

list_auth([], _Addr, _Port, Dir, Acc) ->
    ?DEBUG("list_auth -> done",[]),
    Acc;
list_auth([{Name, {ETS, DETS}}|Tables], Addr, Port, Dir, Acc) ->
    ?DEBUG("list_auth -> ~n"
	   "     Addr: ~p~n"
	   "     Port: ~p~n"
	   "     Dir:  ~p~n"
	   "     Acc:  ~p",
	   [Addr,Port,Dir,Acc]),
    case ets:match_object(ETS, {success, {{'_', Dir, Addr, Port}, '_'}}) of
	[] ->
	    list_auth(Tables, Addr, Port, Dir, Acc);
	List when list(List) ->
	    ?DEBUG("list_auth -> List: ~p",[List]),
	    TN = universal_time(),
	    NewAcc = lists:foldr(fun({success,{{U,Ad,P,D},T}},Ac) -> 
					 if
					     T-TN > 0 ->
						 [U|Ac];
					     true ->
						 Rec = {success,{{U,Ad,P,D},T}},
						 ets:match_delete(ETS,Rec),
						 dets:match_delete(DETS,Rec),
						 Ac
					 end
				 end,
				 Acc, List),
	    list_auth(Tables, Addr, Port, Dir, NewAcc);
	_ ->
	    list_auth(Tables, Addr, Port, Dir, Acc)
    end.


%% list_blocked/2

list_blocked([], Addr, Port, Dir, Acc) ->
    ?DEBUG("list_blocked -> ~n"
	   "     Port: ~p~n"
	   "     Dir:  ~p~n"
	   "     Acc:  ~p",
	   [Port,Dir,Acc]),
    TN = universal_time(),
    ?DEBUG("list_blocked -> TN: ~p",[TN]),
    lists:foldl(fun({U,Ad,P,D,T}, Ac) ->
			?DEBUG("list_blocked -> T: ~p",[T]),
			if
			    T-TN > 0 ->
				[{U,Ad,P,D,local_time(T)}|Ac];
			    true ->
				Ac
			end
		end, 
		[], Acc);
list_blocked([{Name, {ETS, DETS}}|Tables], Addr, Port, Dir, Acc) ->
    ?DEBUG("list_blocked(~p) -> ~n"
	   "     Addr: ~p~n"
	   "     Port: ~p~n"
	   "     Dir:  ~p~n"
	   "     Acc:  ~p",
	   [Name,Addr,Port,Dir,Acc]),
    NewBlocked = 
	case ets:match_object(ETS, {blocked_user, {'_',Addr,Port,Dir,'_'}}) of
	    List when list(List) ->
		lists:foldl(fun({blocked_user, X}, A) -> [X|A] end, Acc, List);
	    _ ->
		Acc
	end,
    list_blocked(Tables, Addr, Port, Dir, NewBlocked).
    

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
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule) ->
    ?DEBUG("check_blocked_user -> ~n"
	   "     User:     ~p~n"
	   "     Addr:     ~p~n"
	   "     Port:     ~p~n"
	   "     Dir:      ~p~n"
	   "     CBModule: ~p",
	   [User,Addr,Port,Dir,CBModule]),
    TN = universal_time(),
    case ets:match_object(ETS, {blocked_user, {User, '_', '_', '_', '_'}}) of
	List when list(List) ->
	    ?DEBUG("check_blocked_user -> ~n"
		   "     List: ~p",
		   [List]),
	    Blocked = lists:foldl(fun({blocked_user, X}, A) ->
					  [X|A] end, [], List),
	    ?DEBUG("check_blocked_user -> ~n"
		   "     Blocked: ~p",
		   [Blocked]),
	    check_blocked_user(Info,User,Dir,Addr,Port,ETS,DETS,TN,Blocked,CBModule);
	_ ->
	    false
    end.
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, [], CBModule) ->
    ?DEBUG("check_blocked_user -> no blocked users",[]),
    false;
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, 
		   [{User,Addr,Port,Dir,T}|Ls], CBModule) ->
    ?DEBUG("check_blocked_user -> ~n"
	   "     TN: ~p~n"
	   "     Ls: ~p",
	   [TN,Ls]),
    TD = T-TN,
    ?DEBUG("check_blocked_user -> TD: ~p",[TD]),
    if
	TD =< 0 ->
	    %% Blocking has expired, remove and grant access.
	    ?DEBUG("check_blocked_user -> block has expired",[]),
	    unblock_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule),
	    false;
	true ->
	    ?DEBUG("check_blocked_user -> block has not expired",[]),
	    true
    end;
check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, 
		   [{OUser,ODir,OAddr,OPort,T}|Ls], CBModule) ->
    ?DEBUG("check_blocked_user -> ~n"
	   "     TN:    ~p~n"
	   "     OUser: ~p~n"
	   "     ODir:  ~p~n"
	   "     OAddr: ~p~n"
	   "     OPort: ~p~n"
	   "     T:     ~p~n"
	   "     Ls:    ~p",
	   [TN,OUser,ODir,OAddr,OPort,T,Ls]),
    TD = T-TN,
    ?DEBUG("check_blocked_user -> TD: ~p",[TD]),
    if
	TD =< 0 ->
	    %% Blocking has expired, remove.
	    ?DEBUG("check_blocked_user -> block has expired",[]),
	    unblock_user(Info, OUser, ODir, OAddr, OPort, ETS, DETS, CBModule);
	true ->
	    ?DEBUG("check_blocked_user -> block has not expired",[]),
	    true
    end,
    check_blocked_user(Info, User, Dir, Addr, Port, ETS, DETS, TN, Ls, CBModule).

unblock_user(Info, User, Dir, Addr, Port, ETS, DETS, CBModule) ->
    Reason=io_lib:format("User ~s was removed from the block list for dir ~s",
			 [User, Dir]),
    ?DEBUG("unblock_user -> ~n"
	   "        ~s",[Reason]),
    mod_log:security_log(Info, lists:flatten(Reason)),
    user_unblock_event(CBModule,Addr,Port,Dir,User),
    dets:match_delete(DETS, {blocked_user, {User, Addr, Port, Dir, '_'}}),
    ets:match_delete(ETS, {blocked_user, {User, Addr, Port, Dir, '_'}}).
  

make_name(Addr,Port) ->
    httpd_util:make_name("mod_security_server",Addr,Port).

make_name(Addr,Port,Num) ->
    httpd_util:make_name("mod_security_server",Addr,Port,
			 "__" ++ integer_to_list(Num)).


auth_fail_event(Mod,Addr,Port,Dir,User,Passwd) ->
    event(auth_fail,Mod,Addr,Port,Dir,[{user,User},{password,Passwd}]).

user_block_event(Mod,Addr,Port,Dir,User) ->
    event(user_block,Mod,Addr,Port,Dir,[{user,User}]).

user_unblock_event(Mod,Addr,Port,Dir,User) ->
    event(user_unblock,Mod,Addr,Port,Dir,[{user,User}]).

event(Event,Mod,undefined,Port,Dir,Info) ->
    (catch Mod:event(Event,Port,Dir,Info));
event(Event,Mod,Addr,Port,Dir,Info) ->
    (catch Mod:event(Event,Addr,Port,Dir,Info)).

universal_time() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

local_time(T) ->
    calendar:universal_time_to_local_time(
      calendar:gregorian_seconds_to_datetime(T)).
