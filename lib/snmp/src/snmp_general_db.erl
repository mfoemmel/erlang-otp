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
-module(snmp_general_db).


%%%-----------------------------------------------------------------
%%% This module implements a very simple "generic" MIB database 
%%% interface. It contains the most common functions performed.
%%% It is generic in the sense that it implements both an interface
%%% to mnesia and ets.
%%%
%%% Note that this module has nothing to do with the snmp_generic
%%% and snmp_generic_mnesia modules.
%%%-----------------------------------------------------------------

-export([open/5,close/1,read/2,write/2,delete/2]).
-export([tab2list/1,match_object/2,match_delete/2]).


-define(VMODULE,"GDB").
-include("snmp_verbosity.hrl").


%% ---------------------------------------------------------------
%% open(Info,Name,Type) -> term()
%% Info   -> ets | {mnesia,Nodes} | {mnesia,Nodes,Action}
%% Name   -> atom()
%% Type   -> set | bag
%% Nodes  -> [node()]
%% Action -> keep | clear
%% 
%% Open (actually create) a database table. In the mnesia case, 
%% where the table is stored on disc, it could be of interest
%% to clear the table. This is controlled by the Action parameter.
%% An empty node list ([]), is traslated into a list containing
%% only the own node.
%% ---------------------------------------------------------------
open({mnesia,Nodes,clear},Name,RecName,Attr,Type) when list(Nodes) ->
    ?vtrace("open ~p database ~p for ~p on ~p; clear",
	    [Type,Name,RecName,Nodes]),
    mnesia_open(mnesia_table_check(Name),Nodes,RecName,Attr,Type,clear);
open({mnesia,Nodes,_},Name,RecName,Attr,Type) ->
    ?vtrace("open ~p database ~p for ~p on ~p; keep",
	    [Type,Name,RecName,Nodes]),
    open({mnesia,Nodes},Name,RecName,Attr,Type);
open({mnesia,Nodes},Name,RecName,Attr,Type) when list(Nodes) ->
    ?vtrace("open ~p database ~p for ~p on ~p; ",[Type,Name,RecName,Nodes]),
    mnesia_open(mnesia_table_check(Name),Nodes,RecName,Attr,Type,keep);

open({dets,Dir,clear},Name,_RecName,_Attr,Type) ->
    dets_open(dets_table_check(Name),Dir,Type,clear);
open({dets,Dir,_},Name,_RecName,_Attr,Type) ->
    dets_open(dets_table_check(Name),Dir,Type,keep);
open({dets,Dir},Name,_RecName,_Attr,Type) ->
    dets_open(dets_table_check(Name),Dir,Type,keep);

%% This function creates the ets table 
open(ets,Name,_RecName,_Attr,Type) ->
    Ets = ets:new(Name, [Type, protected, {keypos, 2}]),
    {ets,Ets}.

mnesia_open({table_exist,Name},_Nodes,_RecName,_Attr,_Type,clear) ->
    ?vtrace("database ~p already exists; clear content",[Name]),
    F = fun() -> mnesia:delete(Name,'_',write) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	{atomic,_} ->
	    {mnesia,Name}
    end;
mnesia_open({table_exist,Name},_Nodes,_RecName,_Attr,_Type,keep) ->
    ?vtrace("database ~p already exists; keep content",[Name]),
    {mnesia,Name};
mnesia_open({no_table,Name},[],Type,RecName,Attr,Action) ->
    mnesia_open({no_table,Name},[node()],Type,RecName,Attr,Action);
mnesia_open({no_table,Name},Nodes,RecName,Attr,Type,_) ->
    ?vtrace("no database ~p: create for ~p of type ~p",[Name,RecName,Type]),
    %% Ok, we assume that this means that the table does not exist
    Args = [{record_name,RecName},{attributes,Attr},
	    {type,Type},{disc_copies,Nodes}],
    case mnesia:create_table(Name,Args) of
	{atomic,ok} ->
	    {mnesia,Name};
	{aborted,Reason} ->
	    exit({failed_create_mnesia_table,Reason})
    end.


mnesia_table_check(Name) ->
    ?vtrace("check existens of database ~p",[Name]),
    case (catch mnesia:table_info(Name,type)) of
	{'EXIT',Reason} ->
	    {no_table,Name};
	_ ->
	    {table_exist,Name}
    end.
    

dets_open({table_exist,Name},_Dir,_Type,keep) ->
    ?vtrace("database ~p already exists; keep content",[Name]),
    {dets,Name};
dets_open({table_exist,Name},_Dir,_Type,clear) ->
    ?vtrace("database ~p already exists; clear content",[Name]),
    dets:delete(Name,'_'),
    {dets,Name};
dets_open({no_table,Name},Dir,Type,_Action) ->
    ?vtrace("no database ~p: create of type ~p",[Name,Type]),
    Dir1 = dets_open1(Dir),
    Dir2 = string:strip(Dir1,right,$/),
    File = io_lib:format("~s/~p.dat",[Dir2,Name]),
    {ok,N} = dets:open_file(Name,[{file,File},{type,Type},{keypos,2}]),
    {dets,N}.


dets_open1([])  -> ".";
dets_open1(Dir) -> Dir.


dets_table_check(Name) ->
    ?vtrace("check existens of database ~p",[Name]),
    case (catch dets:info(Name,size)) of
	{'EXIT',Reason} ->
	    {no_table,Name};
	_ ->
	    {table_exist,Name}
    end.
    

%% ---------------------------------------------------------------
%% close(DbRef) -> 
%% DbRef -> term()
%% 
%% Close the database. This does nothing in the mnesia case, but
%% deletes the table in the ets case.
%% ---------------------------------------------------------------
close({mnesia,_}) ->
    ?vtrace("close mnesia database: NO ACTION",[]),
    ok;
close({dets,Name}) ->
    ?vtrace("close dets database ~p",[Name]),
    dets:close(Name);
close({ets,Name}) ->
    ?vtrace("close (delete) ets table ~p",[Name]),
    ets:delete(Name).


%% ---------------------------------------------------------------
%% read(DbRef,Key) -> false | {value,Rec}
%% DbRef -> term()
%% Rec   -> tuple()
%% 
%% Retrieve a record from the database.
%% ---------------------------------------------------------------
read({mnesia,Name},Key) ->
    ?vtrace("read (dirty) from mnesia database ~p: ~p",[Name,Key]),
    case (catch mnesia:dirty_read(Name,Key)) of
	[Rec|_] -> {value,Rec};
	_ -> false
    end;
read({dets,Name},Key) ->
    ?vtrace("read from dets table ~p: ~p",[Name,Key]),
    case dets:lookup(Name, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end;
read({ets,Name},Key) ->
    ?vtrace("read from ets table ~p: ~p",[Name,Key]),
    case ets:lookup(Name, Key) of
	[Rec|_] -> {value, Rec};
	_ -> false
    end.
    

%% ---------------------------------------------------------------
%% write(DbRef,Rec) -> ok
%% DbRef -> term()
%% Rec   -> tuple()
%% 
%% Write a record to the database.
%% ---------------------------------------------------------------
write({mnesia,Name},Rec) -> 
    ?vtrace("write to mnesia database ~p: ~p",[Name,Rec]),
    F = fun() -> mnesia:write(Name,Rec,write) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	{atomic,_} ->
	    ok
    end;
write({dets,Name},Rec) ->
    ?vtrace("write to dets table ~p: ~p",[Name,Rec]),
    dets:insert(Name,Rec);
write({ets,Name},Rec) ->
    ?vtrace("write to ets table ~p: ~p",[Name,Rec]),
    ets:insert(Name,Rec).


%% ---------------------------------------------------------------
%% delete(DbRef,Rec) -> ok
%% DbRef -> term()
%% Rec   -> tuple()
%% 
%% Delete a record from the database.
%% ---------------------------------------------------------------
delete({mnesia,Name},Rec) -> 
    ?vtrace("delete from mnesia database ~p: ~p",[Name,Rec]),
    F = fun() -> mnesia:delete(Name,Rec,write) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	{atomic,_} ->
	    ok
    end;
delete({dets,Name},Rec) ->
    ?vtrace("delete from dets table ~p: ~p",[Name,Rec]),
    dets:delete(Name,Rec);
delete({ets,Name},Rec) ->
    ?vtrace("delete from ets table ~p: ~p",[Name,Rec]),
    ets:delete(Name,Rec).


%% ---------------------------------------------------------------
%% match_object(DbRef,Pattern) -> [tuple()]
%% DbRef -> term()
%% Pattern -> tuple()
%% 
%% Search the database for records witch matches the pattern.
%% ---------------------------------------------------------------
match_object({mnesia,Name},Pattern) ->
    ?vtrace("match_object(mnesia) in ~p of ~p",[Name,Pattern]),
    F = fun() -> mnesia:match_object(Name,Pattern,read) end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	 {atomic,Recs} ->
	    Recs
    end;
match_object({dets,Name},Pattern) ->
    ?vtrace("match_object(dets) in ~p of ~p",[Name,Pattern]),
    dets:match_object(Name,Pattern);
match_object({ets,Name},Pattern) ->
    ?vtrace("match_object(ets) in ~p of ~p",[Name,Pattern]),
    ets:match_object(Name,Pattern).
    

%% ---------------------------------------------------------------
%% tab2list(DbRef) -> [tuple()]
%% DbRef -> term()
%% 
%% Return all records in the table in the form of a list.
%% ---------------------------------------------------------------
tab2list({mnesia,Name}) ->
    ?vtrace("mnesia tab -> list of ~p",[Name]),
    match_object({mnesia,Name},mnesia:table_info(Name,wild_pattern));
tab2list({dets,Name}) ->
    ?vtrace("dets tab -> list of ~p",[Name]),
    dets_tab2list(Name);
tab2list({ets,Name}) ->
    ?vtrace("ets tab -> list of ~p",[Name]),
    ets:tab2list(Name).


dets_tab2list(Name) ->
    dets_tab2list(dets:first(Name),Name,[]).

dets_tab2list('$end_of_table',_Name,L) ->
    lists:flatten(L);
dets_tab2list(Key,Name,L) ->
    case dets:lookup(Name,Key) of
	{error,_Reason} ->
	    dets_tab2list(dets:next(Name,Key),Name,L);
	L1 when list(L1) ->
	    dets_tab2list(dets:next(Name,Key),Name,[L1|L])
    end.


%% ---------------------------------------------------------------
%% match_delete(DbRef,Pattern) -> 
%% DbRef -> term()
%% Pattern -> tuple()
%% 
%% Search the database for records witch matches the pattern and 
%% deletes them from the database.
%% ---------------------------------------------------------------
match_delete({mnesia,Name},Pattern) -> 
    ?vtrace("match_delete(mnesia) in ~p of ~p",[Name,Pattern]),
    F = fun() -> 
		Recs = mnesia:match_object(Name,Pattern,read),
		lists:foreach(fun(Rec) -> 
				      mnesia:delete_object(Name,Rec,write)
			      end, Recs)
	end,
    case mnesia:transaction(F) of
	{aborted,Reason} ->
	    exit({aborted,Reason});
	 {atomic,_} ->
	    ok
    end;
match_delete({dets,Name},Pattern) -> 
    ?vtrace("match_delete(dets) in ~p of ~p",[Name,Pattern]),
    ets:match_delete(Name, Pattern);
match_delete({ets,Name},Pattern) -> 
    ?vtrace("match_delete(ets) in ~p of ~p",[Name,Pattern]),
    ets:match_delete(Name, Pattern).


