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
-module(mod_auth_plain).

-include("httpd.hrl").
-include("mod_auth.hrl").


%% Internal API
-export([store_directory_data/2]).


-export([get_user/2, 
	 list_group_members/2, 
	 add_user/2, 
	 add_group_member/3, 
	 list_users/1, 
	 delete_user/2, 
	 list_groups/1, 
	 delete_group_member/3, 
	 delete_group/2, 
	 remove/1]).

%%
%% API
%%

%%
%% Storage format of users in the ets table:
%% {UserName, Password, UserData}
%%

add_user(DirData, UStruct) ->
    PWDB = httpd_util:key1search(DirData, auth_user_file),
    UserName = UStruct#httpd_user.username,
    Record = {UserName,
	      UStruct#httpd_user.password, 
	      UStruct#httpd_user.user_data}, 
    case ets:lookup(PWDB, UserName) of
	[{UserName, SomePassword, SomeData}] ->
	    {error, user_already_in_db};
	_ ->
	    ets:insert(PWDB, Record),
	    true
    end.

get_user(DirData, User) ->
    ?DEBUG("get_user -> User: ~p",[User]),
    PwDB = httpd_util:key1search(DirData, auth_user_file),
    case ets:lookup(PwDB, User) of
	[{User, PassWd, Data}] ->
	    ?DEBUG("get_user -> ~n"
		   "    PassWd: ~p~n"
		   "    Data:   ~p",[PassWd,Data]),
	    {ok, #httpd_user{username=User, password=PassWd, user_data=Data}};
	_ ->
	    {error, no_such_user}
    end.

list_users(DirData) ->
    ?DEBUG("list_users -> ~n"
	   "     DirData: ~p", [DirData]),
    PWDB = httpd_util:key1search(DirData, auth_user_file),
    case ets:match(PWDB, '$1') of
	Records when list(Records) ->
	    ?DEBUG("list_users -> ~n"
		   "     Records: ~p", [Records]),
	    {ok, lists:foldr(fun({User,PassWd,Data}, A) -> [User|A] end, 
			     [], lists:flatten(Records))};
	O ->
	    ?DEBUG("list_users -> ~n"
		   "     O: ~p", [O]),
	    {ok, []}
    end.

delete_user(DirData, UserName) ->
    ?DEBUG("delete_user -> UserName: ~p",[UserName]),
    PWDB = httpd_util:key1search(DirData, auth_user_file),
    case ets:lookup(PWDB, UserName) of
	[{UserName, SomePassword, SomeData}] ->
	    ?DEBUG("delete_user -> ~n"
		   "    SomePassword: ~p~n"
		   "    SomeData:     ~p",[SomePassword,SomeData]),
	    ets:delete(PWDB, UserName),
	    case list_groups(DirData) of
		{ok,Groups}->
		    lists:foreach(fun(Group) -> 
					  delete_group_member(DirData, Group, UserName) 
				  end,Groups),
		    true;
		_->
		    true
	    end;
	_ ->
	    {error, no_such_user}
    end.

%%
%% Storage of groups in the ets table:
%% {Group, UserList} where UserList is a list of strings.
%%
  
add_group_member(DirData, Group, UserName) ->
    ?DEBUG("add_group_members -> ~n"
	   "    Group:    ~p~n"
	   "    UserName: ~p",[Group,UserName]),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] ->
	    case lists:member(UserName, Users) of
		true ->
		    ?DEBUG("add_group_members -> already member in group",[]),
		    true;
		false ->
		    ?DEBUG("add_group_members -> add",[]),
		    ets:insert(GDB, {Group, [UserName|Users]}),
		    true
	    end;
	[] ->
	    ?DEBUG("add_group_members -> create grouo",[]),
	    ets:insert(GDB, {Group, [UserName]}),
	    true;
	Other ->
	    ?ERROR("add_group_members -> Other: ~p",[Other]),
	    {error, Other}
    end.

list_group_members(DirData, Group) ->
    ?DEBUG("list_group_members -> Group: ~p",[Group]),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] ->
	    ?DEBUG("list_group_members -> Users: ~p",[Users]),
	    {ok, Users};
	_ ->
	    {error, no_such_group}
    end.

list_groups(DirData) ->
    ?DEBUG("list_groups -> entry",[]),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    case ets:match(GDB, '$1') of
	[] ->
	    ?DEBUG("list_groups -> []",[]),
	    {ok, []};
	Groups0 when list(Groups0) ->
	    ?DEBUG("list_groups -> Groups0: ~p",[Groups0]),
	    {ok, httpd_util:uniq(lists:foldr(fun({G, U}, A) -> [G|A] end,
					     [], lists:flatten(Groups0)))};
	_ ->
	    {ok, []}
    end.

delete_group_member(DirData, Group, User) ->
    ?DEBUG("list_group_members -> ~n"
	   "     Group: ~p~n"
	   "     User:  ~p",[Group,User]),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    UDB = httpd_util:key1search(DirData, auth_user_file),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] when list(Users) ->
	    case lists:member(User, Users) of
		true ->
		    ?DEBUG("list_group_members -> deleted from group",[]),
		    ets:delete(GDB, Group),
		    ets:insert(GDB, {Group, lists:delete(User, Users)}),
		    true;
		false ->
		    ?DEBUG("list_group_members -> not member",[]),
		    {error, no_such_group_member}
	    end;
	_ ->
	    ?ERROR("list_group_members -> no such group",[]),
	    {error, no_such_group}
    end.

delete_group(DirData, Group) ->
    ?DEBUG("list_group_members -> Group: ~p",[Group]),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    case ets:lookup(GDB, Group) of
	[{Group, Users}] ->
	    ?DEBUG("list_group_members -> delete",[]),
	    ets:delete(GDB, Group),
	    true;
	_ ->
	    ?ERROR("delete_group -> no such group",[]),
	    {error, no_such_group}
    end.


store_directory_data(Directory, DirData) ->
    ?CDEBUG("store_directory_data -> ~n"
	    "      Directory: ~p~n"
	    "      DirData:   ~p",
	    [Directory, DirData]),
    PWFile = httpd_util:key1search(DirData, auth_user_file),
    ?CDEBUG("store_directory_data -> ~n"
	    "      PWFile: ~p",[PWFile]),
    GroupFile = httpd_util:key1search(DirData, auth_group_file),
    ?CDEBUG("store_directory_data -> ~n"
	    "      GroupFile: ~p",[GroupFile]),
    case load_passwd(PWFile) of
	{ok, PWDB} ->
	    case load_group(GroupFile) of
		{ok, GRDB} ->
		    %% Address and port is included in the file names...
		    Addr = httpd_util:key1search(DirData, bind_address),
		    Port = httpd_util:key1search(DirData, port),
		    {ok, PasswdDB} = store_passwd(Addr,Port,PWDB),
		    ?CDEBUG("store_directory_data -> ~n"
			    "      PasswdDB: ~p",[PasswdDB]),
		    {ok, GroupDB} = store_group(Addr,Port,GRDB),
		    ?CDEBUG("store_directory_data -> ~n"
			    "      GroupDB: ~p",[GroupDB]),
		    NDD1 = lists:keyreplace(auth_user_file, 1, DirData, 
					    {auth_user_file, PasswdDB}),
		    ?CDEBUG("store_directory_data -> ~n"
			    "      NDD1: ~p",[NDD1]),
		    NDD2 = lists:keyreplace(auth_group_file, 1, NDD1, 
					    {auth_group_file, GroupDB}),
		    ?CDEBUG("store_directory_data -> ~n"
			    "      NDD2: ~p",[NDD2]),
		    {ok, NDD2};
		Err ->
		    ?ERROR("failed storing directory data: "
			   "load group error: ~p",[Err]),
		    {error, Err}
	    end;
	Err2 ->
	    ?ERROR("failed storing directory data: "
		   "load passwd error: ~p",[Err2]),
	    {error, Err2}
    end.



%% load_passwd

load_passwd(AuthUserFile) ->
    case file:open(AuthUserFile, read) of
	{ok,Stream} ->
	    parse_passwd(Stream, []);
	{error, _} ->
	    {error, ?NICE("Can't open "++AuthUserFile)}
    end.

parse_passwd(Stream,PasswdList) ->
    Line=
	case io:get_line(Stream, '') of
	    eof ->
		eof;
	    String ->
		httpd_conf:clean(String)
	end,
    parse_passwd(Stream, PasswdList, Line).

parse_passwd(Stream, PasswdList, eof) ->
    file:close(Stream),
    {ok, PasswdList};
parse_passwd(Stream, PasswdList, "") ->
    parse_passwd(Stream, PasswdList);
parse_passwd(Stream, PasswdList, [$#|_]) ->
    parse_passwd(Stream, PasswdList);
parse_passwd(Stream, PasswdList, Line) ->      
    case regexp:split(Line,":") of
	{ok, [User,Password]} ->
	    parse_passwd(Stream, [{User,Password, []}|PasswdList]);
	{ok,_} ->
	    {error, ?NICE(Line)}
    end.

%% load_group

load_group(AuthGroupFile) ->
    case file:open(AuthGroupFile, read) of
	{ok, Stream} ->
	    parse_group(Stream,[]);
	{error, _} ->
	    {error, ?NICE("Can't open "++AuthGroupFile)}
    end.

parse_group(Stream, GroupList) ->
    Line=
	case io:get_line(Stream,'') of
	    eof ->
		eof;
	    String ->
		httpd_conf:clean(String)
	end,
    parse_group(Stream, GroupList, Line).

parse_group(Stream, GroupList, eof) ->
    file:close(Stream),
    {ok, GroupList};
parse_group(Stream, GroupList, "") ->
    parse_group(Stream, GroupList);
parse_group(Stream, GroupList, [$#|_]) ->
    parse_group(Stream, GroupList);
parse_group(Stream, GroupList, Line) ->      
    case regexp:split(Line, ":") of
	{ok, [Group,Users]} ->
	    {ok, UserList} = regexp:split(Users," "),
	    parse_group(Stream, [{Group,UserList}|GroupList]);
	{ok, _} ->
	    {error, ?NICE(Line)}
    end.


%% store_passwd

store_passwd(Addr,Port,PasswdList) ->
    Name = httpd_util:make_name("httpd_passwd",Addr,Port),
    PasswdDB = ets:new(Name, [set, public]),
    store_passwd(PasswdDB, PasswdList).

store_passwd(PasswdDB, []) ->
    {ok, PasswdDB};
store_passwd(PasswdDB, [User|Rest]) ->
    ets:insert(PasswdDB, User),
    store_passwd(PasswdDB, Rest).

%% store_group

store_group(Addr,Port,GroupList) ->
    Name = httpd_util:make_name("httpd_group",Addr,Port),
    GroupDB = ets:new(Name, [set, public]),
    store_group(GroupDB, GroupList).


store_group(GroupDB,[]) ->
    {ok, GroupDB};
store_group(GroupDB,[User|Rest]) ->
    ets:insert(GroupDB, User),
    store_group(GroupDB, Rest).


%% remove/1
%%
%% Deletes ets tables used by this auth mod.
%%
remove(DirData) ->
    PWDB = httpd_util:key1search(DirData, auth_user_file),
    GDB = httpd_util:key1search(DirData, auth_group_file),
    ets:delete(PWDB),
    ets:delete(GDB).






