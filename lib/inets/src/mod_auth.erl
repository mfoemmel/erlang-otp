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
-module(mod_auth).
-behaviour(gen_server).
-record(mod_state,{tab,crypto}).

%%The functions that the webbserver call on startup stop
%%and when the server traverse the modules.
-export([do/1,load/2,store/2,remove/1]).

%%The callback functions to the mod_auth_api part
-export([handle_call/3,init/1,terminate/2,handle_cast/2,handle_info/2,code_change/3]).


%%User entries to the genserver.
-export([add_user/2,add_user/5, add_user/6, 
	 add_group_member/3,add_group_member/4, add_group_member/5, 
	 list_users/1,list_users/2, list_users/3, 
	 delete_user/2,delete_user/3, delete_user/4,
	 delete_group_member/3,delete_group_member/4, delete_group_member/5, 
	 list_groups/1,list_groups/2, list_groups/3, 
	 delete_group/2,delete_group/3, delete_group/4,
	 get_user/2,get_user/3, get_user/4, 
	 list_group_members/2,list_group_members/3, list_group_members/4,
	 update_password/6,update_password/5,verbosity/3]).

-include("httpd.hrl").
-include("mod_auth.hrl").
-include("httpd_verbosity.hrl").

-define(NOPASSWORD,"NoPassword").

%% do
do(Info) ->
    case httpd_util:key1search(Info#mod.data,status) of
	%% A status code has been generated!
	{StatusCode,PhraseArgs,Reason} ->
	    {proceed, Info#mod.data};
	%% No status code has been generated!
	undefined ->
	    case httpd_util:key1search(Info#mod.data,response) of
		%% No response has been generated!
		undefined ->
		    Path=mod_alias:path(Info#mod.data,Info#mod.config_db,
					Info#mod.request_uri),
		    %% Is it a secret area?
		    case secretp(Path,Info#mod.config_db) of
			{yes, Directory, DirectoryData} ->
			    ?DEBUG("do -> secret area",[]),
			    %% Authenticate (allow)
			    case allow((Info#mod.init_data)#init_data.peername,
				       Info#mod.socket_type,Info#mod.socket,
				       DirectoryData) of
				allowed ->
				    ?DEBUG("do -> allowed",[]),
				    case deny((Info#mod.init_data)#init_data.peername,
					      Info#mod.socket_type, Info#mod.socket,
					      DirectoryData) of
					not_denied ->
					    case httpd_util:key1search(DirectoryData,
								       auth_type) of
						undefined ->
						    {proceed, Info#mod.data};
						none ->
						    {proceed, Info#mod.data};
						AuthType ->
						    do_auth(Info, Directory, DirectoryData,
							    AuthType)
					    end;
					{denied, Reason} ->
					    {proceed,
					     [{status,{403,Info#mod.request_uri,Reason}}|
					      Info#mod.data]}
				    end;
				{not_allowed, Reason} ->
				    ?DEBUG("do -> not allowed",[]),
				    {proceed,[{status,{403,Info#mod.request_uri,Reason}}|
					      Info#mod.data]}
			    end;
			no ->
			    ?DEBUG("do -> not a secret area",[]),
			    {proceed, Info#mod.data}
		    end;
		%% A response has been generated or sent!
		Response ->
		    {proceed, Info#mod.data}
	    end
    end.


do_auth(Info, Directory, DirectoryData, AuthType) ->
    ?DEBUG("do_auth -> entry",[]),
    %% Authenticate (require)
    case require(Info, Directory, DirectoryData) of
	authorized ->
	    {proceed,Info#mod.data};
	{authorized, User} ->
	    {proceed, [{remote_user,User}|Info#mod.data]};
	{authorization_failed, Reason} ->
	    ?DEBUG("do_auth -> authorization_failed: ~p",[Reason]),
	    {proceed, [{status,{401,none,Reason}}|Info#mod.data]};
	{authorization_required, Realm} ->
	    ?DEBUG("do_auth -> authorization_required: ~p",[Realm]),
	    ReasonPhrase = httpd_util:reason_phrase(401),
	    Message = httpd_util:message(401,none,Info#mod.config_db),
	    {proceed,
	     [{response,
	       {401,
		["WWW-Authenticate: Basic realm=\"",Realm,
		 "\"\r\n\r\n","<HTML>\n<HEAD>\n<TITLE>",
		 ReasonPhrase,"</TITLE>\n",
		 "</HEAD>\n<BODY>\n<H1>",ReasonPhrase,
		 "</H1>\n",Message,"\n</BODY>\n</HTML>\n"]}}|
	      Info#mod.data]};
	{status, {StatusCode,PhraseArgs,Reason}} ->
	    ?DEBUG("do_auth -> ~n"
		   "   StatusCode: ~p~n"
		   "   PhraseArgs: ~p~n"
		   "   Reason:     ~p",
		   [StatusCode,PhraseArgs,Reason]),
	    {proceed, [{status,{StatusCode,PhraseArgs,Reason}}|
		       Info#mod.data]}
    end.


%% require

require(Info, Directory, DirectoryData) ->
    ParsedHeader = Info#mod.parsed_header,
    ValidUsers = httpd_util:key1search(DirectoryData, require_user),
    ValidGroups = httpd_util:key1search(DirectoryData, require_group),

    ?DEBUG("require -> ~n"
	   "        ParsedHeader: ~p~n"
	   "        ValidUsers:   ~p~n"
	   "        ValidGroups:  ~p",
	   [ParsedHeader,ValidUsers,ValidGroups]),

    %% Any user or group restrictions?
    case ValidGroups of
	undefined when ValidUsers == undefined ->
	    authorized;
	_ ->
	    case httpd_util:key1search(ParsedHeader, "Authorization") of
		%% Authorization required!
		undefined ->
		    case httpd_util:key1search(DirectoryData, auth_name) of
			undefined ->
			    {status,{500,none,?NICE("AuthName directive not specified")}};
			Realm ->
			    {authorization_required, Realm}
		    end;
		%% Check credentials!
		[$B,$a,$s,$i,$c,$ |EncodedString] ->
		    DecodedString=httpd_util:decode_base64(EncodedString),
		    ?DEBUG("require -> ~n"
			   "        DecodedString: ~p",[DecodedString]),
		    case a_valid_user(Info, DecodedString, ValidUsers, ValidGroups, 
				      Directory, DirectoryData) of
			{yes, User} ->
			    {authorized, User};
			{no, Reason} ->
			    {authorization_failed, Reason};
			{status, {StatusCode,PhraseArgs,Reason}} ->
			    {status,{StatusCode,PhraseArgs,Reason}}
		    end;
		%% Bad credentials!
		BadCredentials ->
		    {status,{401,none,?NICE("Bad credentials "++BadCredentials)}}
	    end
    end.

a_valid_user(Info,DecodedString,ValidUsers,ValidGroups,Dir,DirData) ->
    case httpd_util:split(DecodedString,":",2) of
	{ok,[SupposedUser, Password]} ->
	    case user_accepted(SupposedUser, ValidUsers) of
		true ->
		    check_password(SupposedUser, Password, Dir, DirData);
		false ->
		    case group_accepted(Info,SupposedUser,ValidGroups,Dir,DirData) of
			true ->
			    check_password(SupposedUser,Password,Dir,DirData);
			false ->
			    {no,?NICE("No such user exists")}
		    end
	    end;
	{ok,BadCredentials} ->
	    {status,{401,none,?NICE("Bad credentials "++BadCredentials)}}
    end.

user_accepted(SupposedUser, undefined) ->
    ?DEBUG("user_accepted -> false",[]),
    false;
user_accepted(SupposedUser, ValidUsers) ->
    Res = lists:member(SupposedUser, ValidUsers),
    ?DEBUG("user_accepted -> ~p",[Res]),
    Res.


group_accepted(Info, User, undefined, Dir, DirData) ->
    ?DEBUG("group_accepted -> false (undefined)",[]),
    false;
group_accepted(Info, User, [], Dir, DirData) ->
    ?DEBUG("group_accepted -> false ([])",[]),
    false;
group_accepted(Info, User, [Group|Rest], Dir, DirData) ->
    Ret = int_list_group_members(Group, Dir, DirData),
    case Ret of
	{ok, UserList} ->
	    case lists:member(User, UserList) of
		true ->
		    ?DEBUG("group_accepted -> true",[]),
		    true;
		false ->
		    group_accepted(Info, User, Rest, Dir, DirData)
	    end;
	Other ->
	    false
    end.

check_password(User, Password, Dir, DirData) ->
    ?DEBUG("check_password -> ~n"
	   "      User:     ~p~n"
	   "      Password: ~p",[User,Password]),
    case int_get_user(DirData, User) of
	{ok, UStruct} ->
	    case UStruct#httpd_user.password of
		Password ->
		    %% FIXME
		    ?DEBUG("check_password -> yes",[]),
		    {yes, UStruct#httpd_user.username};
		Other ->
		    ?DEBUG("check_password -> no",[]),
		    {no, "No such user"}   % Don't say 'Bad Password' !!!
	    end;
	_ ->
	    ?DEBUG("check_password -> no",[]),
	    {no, "No such user"}
    end.


%% Middle API. Theese functions call the appropriate authentication module.
int_get_user(DirData, User) ->    
    AuthMod = auth_mod_name(DirData), 
    ?DEBUG("int_get_user(~p) -> AuthMod: ~p",[User,AuthMod]),
    apply(AuthMod, get_user, [DirData, User]).

int_list_group_members(Group, Dir, DirData) ->
    AuthMod = auth_mod_name(DirData),
    ?DEBUG("int_list_group_members(~p) -> AuthMod: ~p",[Group,AuthMod]),
    apply(AuthMod, list_group_members, [DirData, Group]).

auth_mod_name(DirData) ->
    case httpd_util:key1search(DirData, auth_type, plain) of
	plain ->    mod_auth_plain;
	mnesia ->   mod_auth_mnesia;
	dets ->	    mod_auth_dets
    end.

    
%%
%% Is it a secret area?
%%

%% secretp

secretp(Path,ConfigDB) ->
    Directories = ets:match(ConfigDB,{directory,'$1','_'}),
    case secret_path(Path, Directories) of
	{yes,Directory} ->
	    {yes,Directory,
	     lists:flatten(ets:match(ConfigDB,{directory,Directory,'$1'}))};
	no ->
	    no
    end.

secret_path(Path,Directories) ->
    secret_path(Path, httpd_util:uniq(lists:sort(Directories)),to_be_found).

secret_path(Path,[],to_be_found) ->
    no;
secret_path(Path,[],Directory) ->
    {yes,Directory};
secret_path(Path,[[NewDirectory]|Rest],Directory) ->
    case regexp:match(Path,NewDirectory) of
	{match,_,_} when Directory == to_be_found ->
	    secret_path(Path,Rest,NewDirectory);
	{match,_,Length} when Length > length(Directory)->
	    secret_path(Path,Rest,NewDirectory);
	{match,_,Length} ->
	    secret_path(Path,Rest,Directory);
	nomatch ->
	    secret_path(Path,Rest,Directory)
    end.

%%
%% Authenticate
%%

%% allow

allow({_,RemoteAddr},SocketType,Socket,DirectoryData) ->
    ?DEBUG("allow -> RemoteAddr ~p", [RemoteAddr]),
    Hosts = httpd_util:key1search(DirectoryData, allow_from, all),
    case validate_addr(RemoteAddr,Hosts) of
	true ->
	    allowed;
	false ->
	    {not_allowed, ?NICE("Connection from your host is not allowed")}
    end.

validate_addr(RemoteAddr,all) ->            % When called from 'allow'
    true;
validate_addr(RemoteAddr,none) ->           % When called from 'deny'
    false;
validate_addr(RemoteAddr,[]) ->
    false;
validate_addr(RemoteAddr,[HostRegExp|Rest]) ->
    ?DEBUG("validate_addr -> RemoteAddr: ~p HostRegExp: ~p",
	   [RemoteAddr, HostRegExp]),
    case regexp:match(RemoteAddr, HostRegExp) of
	{match,_,_} ->
	    true;
	nomatch ->
	    validate_addr(RemoteAddr,Rest)
    end.

%% deny

deny({_,RemoteAddr},SocketType,Socket,DirectoryData) ->
    ?DEBUG("deny -> RemoteAddr: ~p",[RemoteAddr]),
    Hosts = httpd_util:key1search(DirectoryData, deny_from, none),
    ?DEBUG("deny -> Hosts: ~p",[Hosts]),
    case validate_addr(RemoteAddr,Hosts) of
	true ->
	    {denied, ?NICE("Connection from your host is not allowed")};
	false ->
	    not_denied
    end.    

%%
%% Configuration
%%

%% load/2
%%

%% mod_auth recognizes the following Configuration Directives:
%% <Directory /path/to/directory>
%%  AuthDBType
%%  AuthName
%%  AuthUserFile
%%  AuthGroupFile
%%  AuthAccessPassword
%%  require
%%  allow
%% </Directory>

%% When a <Directory> directive is found, a new context is set to
%% [{directory, Directory, DirData}|OtherContext]
%% DirData in this case is a key-value list of data belonging to the
%% directory in question.
%%
%% When the </Directory> statement is found, the Context created earlier
%% will be returned as a ConfigList and the context will return to the
%% state it was previously.

load([$<,$D,$i,$r,$e,$c,$t,$o,$r,$y,$ |Directory],[]) ->
    Dir = httpd_conf:custom_clean(Directory,"",">"),
    {ok,[{directory, Dir, [{path, Dir}]}]};
load(eof,[{directory,Directory, DirData}|_]) ->
    {error, ?NICE("Premature end-of-file in "++Directory)};

load([$A,$u,$t,$h,$N,$a,$m,$e,$ |AuthName], [{directory,Directory, DirData}|Rest]) ->
    {ok, [{directory,Directory,
	   [ {auth_name, httpd_conf:clean(AuthName)}|DirData]} | Rest ]};

load([$A,$u,$t,$h,$U,$s,$e,$r,$F,$i,$l,$e,$ |AuthUserFile0],
     [{directory, Directory, DirData}|Rest]) ->
    AuthUserFile = httpd_conf:clean(AuthUserFile0),
    {ok,[{directory,Directory,
	  [ {auth_user_file, AuthUserFile}|DirData]} | Rest ]};

load([$A,$u,$t,$h,$G,$r,$o,$u,$p,$F,$i,$l,$e,$ |AuthGroupFile0],
	 [{directory,Directory, DirData}|Rest]) ->
    AuthGroupFile = httpd_conf:clean(AuthGroupFile0),
    {ok,[{directory,Directory,
	  [ {auth_group_file, AuthGroupFile}|DirData]} | Rest]};

%AuthAccessPassword
load([$A,$u,$t,$h,$A,$c,$c,$e,$s,$s,$P,$a,$s,$s,$w,$o,$r,$d,$ |AuthAccessPassword0],
	 [{directory,Directory, DirData}|Rest]) ->
    AuthAccessPassword = httpd_conf:clean(AuthAccessPassword0),
    {ok,[{directory,Directory,
	  [{auth_access_password, AuthAccessPassword}|DirData]} | Rest]};




load([$A,$u,$t,$h,$D,$B,$T,$y,$p,$e,$ |Type],
	 [{directory, Dir, DirData}|Rest]) ->
    case httpd_conf:clean(Type) of
	"plain" ->
	    {ok, [{directory, Dir, [{auth_type, plain}|DirData]} | Rest ]};
	"mnesia" ->
	    {ok, [{directory, Dir, [{auth_type, mnesia}|DirData]} | Rest ]};
	"dets" ->
	    {ok, [{directory, Dir, [{auth_type, dets}|DirData]} | Rest ]};
	_ ->
	    {error, ?NICE(httpd_conf:clean(Type)++" is an invalid AuthDBType")}
    end;

load([$r,$e,$q,$u,$i,$r,$e,$ |Require],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Require," ") of
	{ok,["user"|Users]} ->
	    {ok,[{directory,Directory,
		  [{require_user,Users}|DirData]} | Rest]};
	{ok,["group"|Groups]} ->
	    {ok,[{directory,Directory,
		  [{require_group,Groups}|DirData]} | Rest]};
	{ok,_} ->
	    {error,?NICE(httpd_conf:clean(Require)++" is an invalid require")}
    end;

load([$a,$l,$l,$o,$w,$ |Allow],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Allow," ") of
	{ok,["from","all"]} ->
	    {ok,[{directory,Directory,
		  [{allow_from,all}|DirData]} | Rest]};
	{ok,["from"|Hosts]} ->
	    {ok,[{directory,Directory,
		  [{allow_from,Hosts}|DirData]} | Rest]};
	{ok,_} ->
	    {error,?NICE(httpd_conf:clean(Allow)++" is an invalid allow")}
    end;

load([$d,$e,$n,$y,$ |Deny],[{directory,Directory, DirData}|Rest]) ->
    case regexp:split(Deny," ") of
	{ok, ["from", "all"]} ->
	    {ok,[{directory, Directory,
		  [{deny_from, all}|DirData]} | Rest]};
	{ok, ["from"|Hosts]} ->
	    {ok,[{directory, Directory,
		  [{deny_from, Hosts}|DirData]} | Rest]};
	{ok, _} ->
	    {error,?NICE(httpd_conf:clean(Deny)++" is an invalid deny")}
    end;

load("</Directory>",[{directory,Directory, DirData}|Rest]) -> 
    {ok, Rest, {directory, Directory, DirData}};

load([$A,$u,$t,$h,$M,$n,$e,$s,$i,$a,$D,$B,$ |AuthMnesiaDB],
      [{directory, Dir, DirData}|Rest]) ->
    case httpd_conf:clean(AuthMnesiaDB) of
	"On" ->
	    {ok,[{directory,Dir,[{auth_type,mnesia}|DirData]}|Rest]};
	"Off" ->
	    {ok,[{directory,Dir,[{auth_type,plain}|DirData]}|Rest]};
	_ ->
	    {error, ?NICE(httpd_conf:clean(AuthMnesiaDB)++" is an invalid AuthMnesiaDB")}
    end.

%% store

store({directory,Directory0, DirData0}, ConfigList) ->
    ?CDEBUG("store(directory) -> ~n"
	    "     Directory0: ~p~n"
	    "     DirData0:   ~p~n"
	    "     ConfigList: ~p",
	    [Directory0, DirData0, ConfigList]),
    Port = httpd_util:key1search(ConfigList, port),
    DirData= case httpd_util:key1search(ConfigList, bind_address) of
		  undefined ->
		      [{port, Port}|DirData0];
		  Addr ->
		      [{port, Port},{bind_address,Addr}|DirData0]
	      end,
    ?CDEBUG("store(directory) -> DirData: ~p",[DirData]),
    Directory = 
	case filename:pathtype(Directory0) of
	    relative ->
		SR = httpd_util:key1search(ConfigList, server_root),
		filename:join(SR, Directory0);
	    _ ->
		Directory0
	end,
    ?CDEBUG("store(directory) -> Directory: ~p",[Directory]),
    AuthMod =
	case httpd_util:key1search(DirData0, auth_type) of
	    mnesia -> mod_auth_mnesia;
	    dets ->   mod_auth_dets;
	    plain ->  mod_auth_plain;
	    _ ->      no_module_at_all
	end,
    ?CDEBUG("store(directory) -> AuthMod: ~p",[AuthMod]),
    case AuthMod of
	no_module_at_all ->
	    {ok, {directory, Directory, DirData}};
	_ ->
	    %%Control that there are a password or add a standard password: "NoPassword"
	    %%In this way a user must select to use a noPassword
	    Pwd=case httpd_util:key1search(DirData,auth_access_password)of
		 undefined->
			 ?NOPASSWORD;
		 PassW->
			 PassW
		end,
	    DirDataLast=lists:keydelete(auth_access_password,1,DirData), 
	    case catch AuthMod:store_directory_data(Directory, DirDataLast) of
		ok ->
		    add_auth_password(Directory,Pwd,ConfigList),
		    {ok, {directory, Directory, DirDataLast}};
		{ok, NewDirData} ->
		    ?CDEBUG("store(directory) -> NewDirData: ~p",
			    [NewDirData]),
		    add_auth_password(Directory,Pwd,ConfigList),
		    {ok, {directory, Directory, NewDirData}};
		{error, Reason} ->
		    {error, Reason};
		Other ->
		    ?ERROR("unexpected result: ~p",[Other]),
		    {error, Other}
	    end
    end.


add_auth_password(Dir,Pwd0,ConfigList)->    
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Port = httpd_util:key1search(ConfigList, port),
    start(Addr,Port),
    add_password(Addr,Port,Dir,Pwd0).
    
%% remove


remove(ConfigDB) ->
    lists:foreach(fun({directory, Dir, DirData}) -> 
			  AuthMod = auth_mod_name(DirData),
			  (catch apply(AuthMod, remove, [DirData]))
		  end,
		  ets:match_object(ConfigDB,{directory,'_','_'})),
    Addr = case ets:lookup(ConfigDB, bind_address) of
	       [] -> 
		   undefined;
	       [{bind_address, Address}] ->
		   Address
	   end,
    [{port, Port}] = ets:lookup(ConfigDB, port),
    stop(Addr,Port),
    ok.



%%
%% External API
%
%
add_password(Addr,Port,Dir,Pwd0)->
    Name=httpd_util:make_name("auth",Addr,Port),
    gen_server:call(whereis(Name),{add_auth_pass,Dir,Pwd0}).


start(Addr,Port)->
    SName = httpd_util:make_name("auth",Addr,Port),
    Verbosity = get(auth_verbosity),
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
    SName = httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(SName), {verbosity,Verbosity})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	OldVerbosity ->
	    OldVerbosity
    end.

stop(Addr,Port)->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),stop)) of
	{'EXIT',Reason} ->
	    {error,Reason};
	StopVal ->
	    StopVal
    end.


update_password(Port,Dir,Old,New,New)->
    update_password(undefined,Port,Dir,Old,New,New).

update_password(Addr,Port,Dir,Old,New,New)when list(New)->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{update_auth_pass,Dir,Old,New})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	PwdRet ->
	    PwdRet
    end;
	    

update_password(Addr,Port,Dir,Old,New,New)->
    {error,badtype};
update_password(Addr,Port,Dir,Old,New,New1)->
    {error,notqeual}.

%% add_user
add_user(UserName,Opt)->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    case getOptions(Opt,userData) of
		{error,Reason}->
		    {error,Reason};
		{UserData,Password}->
		    Name=httpd_util:make_name("auth",Addr,Port),
		    User=[#httpd_user{username=UserName, 
				      password=Password,
				      user_data=UserData}],
		    case (catch gen_server:call(whereis(Name),{add_user,Addr,Port,Dir,User,AuthPwd})) of
			{'EXIT',Reason} ->
			    {error,Reason};
			AddUserResponse ->
			    AddUserResponse 
		    end;
		{error,Reason}->
		    {error,Reason}
	    end
    end.


add_user(UserName, Password, UserData, Port, Dir) ->
    add_user(UserName, Password, UserData, undefined, Port, Dir).
add_user(UserName, Password, UserData, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    User=[#httpd_user{username=UserName, 
			  password=Password,
			  user_data=UserData}],
    case (catch gen_server:call(whereis(Name),{add_user,Addr,Port,Dir,User,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	AddUserResponse ->
	    AddUserResponse 
    end.

%% get_user
get_user(UserName,Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{get_user,Addr,Port,Dir,UserName,AuthPwd})) of
		{'EXIT',Reason} ->
		    {error,Reason};
		GetUserResp->
		    GetUserResp 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

get_user(UserName, Port, Dir) ->
    get_user(UserName, undefined, Port, Dir).
get_user(UserName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{get_user,Addr,Port,Dir,UserName,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
        GetUserResp->
	    GetUserResp
    end.


%% add_group_member
add_group_member(GroupName,UserName,Opt)->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{add_group_member,Addr,Port,Dir,GroupName,UserName,AuthPwd}))of
		{'EXIT',Reason} ->
		    {error,Reason};
		AddResp ->
		    AddResp 
	    end;	
	{error,Reason}->
	    {error,Reason}
    end.

add_group_member(GroupName, UserName, Port, Dir) ->
    add_group_member(GroupName, UserName, undefined, Port, Dir).

add_group_member(GroupName, UserName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{add_group_member,Addr,Port,Dir,GroupName,UserName,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	AddResp ->
	    AddResp 
    end.



%% delete_group_member
delete_group_member(GroupName,UserName,Opt)->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{delete_group_member,Addr,Port,Dir,GroupName,UserName,AuthPwd})) of
		{'EXIT',Reason} ->
		    {error,Reason};
		DelResp ->
		    DelResp 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

delete_group_member(GroupName, UserName, Port, Dir) ->
    delete_group_member(GroupName, UserName, undefined, Port, Dir).
delete_group_member(GroupName, UserName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{delete_group_member,Addr,Port,Dir,GroupName,UserName,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	DelResp->
	    DelResp 
    end.
    
%% list_users
list_users(Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{list_users,Addr,Port,Dir,AuthPwd})) of
		{'EXIT',Reason} ->
		    {error,Reason};
		Response ->
		    Response 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

list_users(Port, Dir) ->
    list_users(undefined, Port, Dir).
list_users(Addr, Port, Dir) ->
    ?DEBUG("list_users -> ~n"
	   "     Addr: ~p~n"
	   "     Port: ~p~n"
	   "     Dir:  ~p",
	   [Addr,Port,Dir]),
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{list_users,Addr,Port,Dir,?NOPASSWORD}))of
	{'EXIT',Reason} ->
	    {error,Reason};
	Response ->
	    Response 
    end.
    
%% delete_user
delete_user(UserName,Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{delete_user,Addr,Port,Dir,UserName,AuthPwd}))of
		{'EXIT',Reason} ->
		    {error,Reason};
		Response ->
		    Response 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

delete_user(UserName, Port, Dir) ->
    delete_user(UserName, undefined, Port, Dir).
delete_user(UserName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{delete_user,Addr,Port,Dir,UserName,?NOPASSWORD}))of
	  {'EXIT',Reason} ->
		 {error,Reason};
	  Response ->
		 Response 
    end.
	  
	  %% delete_group
delete_group(GroupName,Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{delete_group,Addr,Port,Dir,GroupName,AuthPwd}))of
		{'EXIT',Reason} ->
		    {error,Reason};
		Response ->
		    Response 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

delete_group(GroupName, Port, Dir) ->
    delete_group(GroupName, undefined, Port, Dir).
delete_group(GroupName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{delete_group,Addr,Port,Dir,GroupName,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	Response ->
	    Response 
    end.

%% list_groups
list_groups(Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case catch (gen_server:call(whereis(Name),{list_groups,Addr,Port,Dir,AuthPwd})) of
		{'EXIT',Reason} ->
		    {error,Reason};
		Response ->
		    Response 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

list_groups(Port, Dir) ->
    list_groups(undefined, Port, Dir).
list_groups(Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{list_groups,Addr,Port,Dir,?NOPASSWORD}) ) of
	{'EXIT',Reason} ->
	    {error,Reason};
	Response ->
	    Response 
    end.

%% list_group_members
list_group_members(GroupName,Opt) ->
    case getOptions(Opt,mandatory) of
	{Addr,Port,Dir,AuthPwd}->
	    Name=httpd_util:make_name("auth",Addr,Port),
	    case (catch gen_server:call(whereis(Name),{list_group_members,Addr,Port,Dir,GroupName,AuthPwd})) of
		{'EXIT',Reason} ->
		    {error,Reason};
		Response ->
		    Response 
	    end;
	{error,Reason}->
	    {error,Reason}
    end.

list_group_members(GroupName, Port, Dir) ->
    list_group_members(GroupName, undefined, Port, Dir).
list_group_members(GroupName, Addr, Port, Dir) ->
    Name=httpd_util:make_name("auth",Addr,Port),
    case (catch gen_server:call(whereis(Name),{list_group_members,Addr,Port,Dir,GroupName,?NOPASSWORD})) of
	{'EXIT',Reason} ->
	    {error,Reason};
	Response ->
	    Response 
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%The genserver callback functions                                  %%
%%                                                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init([undefined]) ->
    init([?default_verbosity]);

init([Verbosity]) ->
    ?DEBUG("init -> entry with Verbosity: ~p",[Verbosity]),
    put(sname,auth),
    put(verbosity,Verbosity),
    ?vlog("starting",[]),
    
    %If crypto already was started we do not want to stop it
    %When gen_server stop.
    StopCrypto=crypto:start(),
    {ok,#mod_state{tab=ets:new(auth_pwd,[set,protected]),crypto=StopCrypto}}.

terminate(Reason,State)->
    ets:delete(State#mod_state.tab),
    %if the start value was ok then we started it.
    case State#mod_state.crypto of
	ok->
	    crypto:stop(),
	    ok;
	_ ->
	   ok
    end.



%Add a user
handle_call({add_user,Addr,Port,Dir,User,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, add_user,User,AuthPwd,State),State};

%%Get data about a user
handle_call({get_user,Addr,Port,Dir,UserName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, get_user, [UserName],AuthPwd,State),State};

%%Add a group member
handle_call({add_group_member,Addr,Port,Dir,GroupName,UserName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, add_group_member, [GroupName, UserName],AuthPwd,State),State};

%%delete a group
handle_call({delete_group_member,Addr,Port,Dir,GroupName,UserName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, delete_group_member, [GroupName, UserName],AuthPwd,State),State};

%%List all users thats standalone users
handle_call({list_users,Addr,Port,Dir,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, list_users, [],AuthPwd,State),State};

%%Delete a user
handle_call({delete_user,Addr,Port,Dir,UserName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, delete_user, [UserName],AuthPwd,State),State};

%%Delete a group
handle_call({delete_group,Addr,Port,Dir,GroupName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, delete_group, [GroupName],AuthPwd,State),State};

%%List the current groups
handle_call({list_groups,Addr,Port,Dir,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, list_groups, [],AuthPwd,State),State};

%%List the members of the given group
handle_call({list_group_members,Addr,Port,Dir,GroupName,AuthPwd},_From,State)->
    {reply,api_call(Addr, Port, Dir, list_group_members, [GroupName],AuthPwd,State),State};


handle_call({add_auth_pass,Dir,Pwd0},_From,State)->
    {reply,add_auth_pass(Dir,Pwd0,State),State};

%%Update the password for a directory
  
handle_call({update_auth_pass,Dir,Old,New},_From,State)->
    case getPassword(State,Dir) of
	OldPwd when binary(OldPwd)->
	    case crypto:md5(Old) of
		OldPwd ->
		    %%The old password is right update the authPwd to the new
		    update_password(Dir,New,State),
		    {reply,ok,State};
		_->
		    {reply,{error,error_new},State}
	    end;
	_->
	    {reply,{error,error_old},State}
    end;

handle_call(stop,_From,State)->
    {stop,normal,State};

handle_call({verbosity,Verbosity},_From,State)->
    ?vlog("set verbosity to ~p",[Verbosity]),
    OldVerbosity = get(verbosity),
    put(verbosity,Verbosity),
    ?vdebug("old verbosity: ~p",[OldVerbosity]),
    {reply,OldVerbosity,State}.

handle_info(Info,State)->
    {noreply,State}.

handle_cast(Request,State)->
    {noreply,State}.
    
%%Since this is the first version no changes to do!    
code_change(_,State,_)->
    {ok,State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                  %%
%%The functions that really changes the data in the database        %%
%%of users to different directories                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% API gateway

api_call(Addr, Port, Dir, Func, Args,Password,State) ->
    case controlPassword(Password,State,Dir) of
	ok->
	    ConfigName = httpd_util:make_name("httpd_conf",Addr,Port),
	    case ets:match_object(ConfigName, {directory, Dir, '$1'}) of
		[{directory, Dir, DirData}] ->
		    AuthMod = auth_mod_name(DirData),
		    ?DEBUG("api_call -> call ~p:~p",[AuthMod,Func]),
		    Ret = (catch apply(AuthMod, Func, [DirData|Args])),
		    ?DEBUG("api_call -> Ret: ~p",[ret]),
		    Ret;
		O ->
		    ?DEBUG("api_call -> O: ~p",[O]),
		    {error, no_such_directory}
	    end;
	bad_password ->
	    {error,bad_password}
    end.

controlPassword(Password,State,Dir)when Password=:="DummyPassword"->
    bad_password;

controlPassword(Password,State,Dir)->
    case getPassword(State,Dir) of
	Pwd when binary(Pwd)->
	    case crypto:md5(Password) of
		Pwd ->
		    ok;
		_->
		    bad_password
	    end;
	_ ->
	    bad_password
    end.

    
getPassword(State,Dir)->
    case ets:lookup(State#mod_state.tab,Dir) of
	[{_,Pwd}]->
	    Pwd;
	_ ->
	    {error,bad_password}
    end.

update_password(Dir,New,State)->
    ets:insert(State#mod_state.tab,{Dir,crypto:md5(New)}).

add_auth_pass(Dir,Pwd0,State)->
    case getPassword(State,Dir) of
	PwdExists when binary(PwdExists) ->
	    {error,dir_protected};
	{error,_} ->
	    update_password(Dir,Pwd0,State)
    end.
	    

%Opt=[{port,Port},{addr,Addr},{dir,Dir},{authPassword,AuthPassword},|FunctionSpecificData}
getOptions(Opt,mandatory)->    
    case httpd_util:key1search(Opt,port,undef) of
	Port when integer(Port)->
	    case httpd_util:key1search(Opt,dir,undef) of
		Dir when list(Dir)->
		    Addr=httpd_util:key1search(Opt,addr,undefined),
		    AuthPwd=httpd_util:key1search(Opt,authPassword,"NoPassword"),
		    {Addr,Port,Dir,AuthPwd};
		_->
		    {error,bad_dir}
	    end;
	_ ->
	    {error,bad_dir}
    end;
%%FunctionSpecificData={userData,UserData}|{password,Password}
getOptions(Opt,userData)->
    case httpd_util:key1search(Opt,userData,undefined) of
	undefined->
	    {error,no_userdata};
	UserData ->
	    case httpd_util:key1search(Opt,password,undefined)of
		undefined->
		    {error,no_password};
		Pwd->
		    {UserData,Pwd}
	    end
    end.






