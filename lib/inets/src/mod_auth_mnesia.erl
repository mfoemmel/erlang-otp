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
-module(mod_auth_mnesia).
-export([get_user/2,
	 list_group_members/2,
	 add_user/2,
	 add_group_member/3,
	 list_users/1,
	 delete_user/2,
	 list_groups/1,
	 delete_group_member/3,
	 delete_group/2]).

-export([store_user/5, store_group_member/5, list_group_members/3, list_groups/2,
	 list_users/2, remove_user/4, remove_group_member/5, remove_group/4]).

-export([store_directory_data/2]).

-include("httpd.hrl").
-include("mod_auth.hrl").



store_directory_data(Directory, DirData) ->
    %% We don't need to do anything here, we could ofcourse check that the appropriate
    %% mnesia tables has been created prior to starting the http server.
    ok.


%%
%% API
%%

%% Compability API


store_user(UserName, Password, Port, Dir, AccessPassword) ->
   %% AccessPassword is ignored - was not used in previous version
   DirData = [{path,Dir},{port,Port}],
   UStruct = #httpd_user{username = UserName,
			 password = Password},
   add_user(DirData, UStruct).

store_group_member(GroupName, UserName, Port, Dir, AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   add_group_member(DirData, GroupName, UserName).

list_group_members(GroupName, Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_group_members(DirData, GroupName).

list_groups(Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_groups(DirData).

list_users(Port, Dir) ->
   DirData = [{path,Dir},{port,Port}],
   list_users(DirData).
    
remove_user(UserName, Port, Dir, _AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_user(DirData, UserName).

remove_group_member(GroupName,UserName,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_group_member(DirData, GroupName, UserName).

remove_group(GroupName,Port,Dir,_AccessPassword) ->
   DirData = [{path,Dir},{port,Port}],
   delete_group(DirData, GroupName).

%%
%% Storage format of users in the mnesia table:
%% httpd_user records
%%

add_user(DirData, UStruct) ->
    {Port, Dir} = lookup_common(DirData),
    UserName = UStruct#httpd_user.username,
    Password = UStruct#httpd_user.password,
    Data     = UStruct#httpd_user.user_data,
    User=#httpd_user{username={UserName,Port,Dir},
		     password=Password,
		     user_data=Data},
    case mnesia:transaction(fun() -> mnesia:write(User) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    ok
    end.

get_user(DirData, UserName) ->
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:read({httpd_user, {UserName,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error, Reason};
	{atomic,[]} ->
	    {error, no_such_user};
	{atomic, [Record]} when record(Record, httpd_user) ->
	    {ok, Record#httpd_user{username=UserName}};
	Other ->
	    {error, no_such_user}
    end.

list_users(DirData) ->
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:match_object({httpd_user,{'_',Port,Dir},'_','_'})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	{atomic,Users} ->
	    {ok, 
	     lists:foldr(fun({httpd_user, {UserName, AnyPort, AnyDir}, Password, Data}, Acc) ->
				 [UserName|Acc]
			 end,
			 [], Users)}
    end.

delete_user(DirData, UserName) ->
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:delete({httpd_user,{UserName,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%%
%% Storage of groups in the mnesia table:
%% Multiple instances of {#httpd_group, User}
%%

add_group_member(DirData, GroupName, User) ->
    {Port, Dir} = lookup_common(DirData),
    Group=#httpd_group{name={GroupName, Port, Dir}, userlist=User},
    case mnesia:transaction(fun() -> mnesia:write(Group) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

list_group_members(DirData, GroupName) ->
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:read({httpd_group,{GroupName,Port,Dir}})
			    end) of
	{aborted, Reason} ->
	    {error,Reason};
	{atomic, Members} ->
	    {ok,[UserName || {httpd_group,{AnyGroupName,AnyPort,AnyDir},UserName} <- Members,
			     AnyGroupName == GroupName,AnyPort == Port,
			     AnyDir == Dir]}
  end.

list_groups(DirData) -> 
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:match_object({httpd_group,{'_',Port,Dir},'_'}) 
			    end) of
	{aborted, Reason} ->
	    {error, Reason};
	{atomic, Groups} ->
	    GroupNames=
		[GroupName || {httpd_group,{GroupName,AnyPort,AnyDir}, UserName} <- Groups,
			      AnyPort == AnyPort, AnyDir == Dir],
	    {ok, httpd_util:uniq(lists:sort(GroupNames))}
    end.

delete_group_member(DirData, GroupName, UserName) ->
    {Port, Dir} = lookup_common(DirData),
    Group = #httpd_group{name={GroupName, Port, Dir}, userlist=UserName},
    case mnesia:transaction(fun() -> mnesia:delete_object(Group) end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%% THIS IS WRONG (?) !
%% Should first match out all httpd_group records for this group and then
%% do mnesia:delete on those. Or ?

delete_group(DirData, GroupName) ->
    {Port, Dir} = lookup_common(DirData),
    case mnesia:transaction(fun() ->
				    mnesia:delete({httpd_group, {GroupName,Port,Dir}})
			    end) of
	{aborted,Reason} ->
	    {error,Reason};
	_ ->
	    true
    end.

%% Utility functions.

lookup_common(DirData) ->
    Dir = httpd_util:key1search(DirData, path),
    Port = httpd_util:key1search(DirData, port),
    {Port, Dir}.







