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
%% Purpose : The database handler for the Erlang interpreter.

-module(dbg_idb).
-export([new/0,destroy/0,insert/2,lookup/1,lookup/2,lookup/3,
	 cr_new_module/1,delete/1,delete_all_breaks/0,delete_all_breaks/1,
	 get_all_breaks/0,get_all_breaks/1,rm_usage/1,
	 del_mod/1,insert/3,mod_db/2,which_db/2,
	 match_object/2]).

%% ====================================================
%% User interface

%%% -------------------------------------------------------
%%% Create the database server, 'int_db'.
%%% -------------------------------------------------------

new() ->
    Pid = spawn(dbg_idb_server,start,[]),
    register(int_db,Pid),
    Pid.

destroy() ->
    int_db ! {self(),stop},
    ok.

%%% -------------------------------------------------------
%%% Insert Value for Key in the 'global' database.
%%% -------------------------------------------------------

insert(Key,Value) ->
    int_db ! {self(),insert,Key,Value},
    ok.

%%% -------------------------------------------------------
%%% Insert a value with Key in the table Db.
%%% -------------------------------------------------------

insert(Db,Key,Value) ->
   dbg_ets:insert(Db,{Key,Value}).

%%% -------------------------------------------------------
%%% Delete a Key from the 'global' database.
%%% Only break points can be deleted, Key == {Mod,Line}.
%%% -------------------------------------------------------

delete(Key) ->
    int_db ! {self(),delete,Key},
    ok.

%%% -------------------------------------------------------
%%% Lookup the value for Key from the "global" database.
%%% -------------------------------------------------------

lookup(Key) ->
    int_db ! {self(),lookup,Key},
    Db = whereis(int_db),
    receive
	{Db,look_resp,Res} ->
	    Res
    end.

%%% -------------------------------------------------------
%%% Lookup the value for Key in table Db.
%%% If a module (atom) is given as table, ask the 'int_db'
%%% server to lookup the value in the latest database
%%% table for Module.
%%% -------------------------------------------------------

lookup(Module,Key) when atom(Module) ->
    int_db ! {self(),lookup,Module,Key},
    Db = whereis(int_db),
    receive
	{Db,look_resp,Res} ->
	    Res
    end;
lookup(Db,Key) ->
    case get_db(Db,Key) of
	undefined ->
	    not_found;
	Value ->
	    {ok,Value}
    end.

%%% -------------------------------------------------------
%%% Lookup a local or external function.
%%% -------------------------------------------------------

lookup(Mod, Name, Arity) ->
    int_db ! {self(),lookup,Mod,Name,Arity},
    Db = whereis(int_db),
    receive
	{Db,look_resp,Res} ->
	    Res
    end.

%%% -------------------------------------------------------
%%% Like ets:match_object/2.
%%% -------------------------------------------------------
match_object(Db, Key) ->
    dbg_ets:match_object(Db, Key).

%%% -------------------------------------------------------
%%% Fetch the reference to the database table that Pid shall
%%% use to access Module. This usage will be noted in the db.
%%% -------------------------------------------------------

mod_db(Module,Pid) ->
    int_db ! {self(),get_mod_db,Module,Pid},
    Db = whereis(int_db),
    receive
	{Db,get_mod_db_resp,Resp} ->
	    Resp
    end.

%%% -------------------------------------------------------
%%% Remove all usage references of Pid in the database.
%%% -------------------------------------------------------

rm_usage(Pid) ->
    int_db ! {self(),clear,Pid},
    ok.

%%% -------------------------------------------------------
%%% Create a new database table for Module.
%%% -------------------------------------------------------

cr_new_module(Module) ->
    int_db ! {self(),cr_new_module,Module},
    Db = whereis(int_db),
    receive
	{Db,created,Module,ModDbRef} ->
	    ModDbRef
    end.

%%% -------------------------------------------------------
%%% Delete all references and tables for Module.
%%% -------------------------------------------------------

del_mod(Module) ->
    int_db ! {self(),del_mod,Module},
    ok.

%%% -------------------------------------------------------
%%% Delete all break points.
%%% -------------------------------------------------------

delete_all_breaks() ->
    case whereis(int_db) of
	undefined ->
	    ok;
	Db ->
	    Db ! {self(),delete_all_breaks},
	    ok
    end.

%%% -------------------------------------------------------
%%% Delete all break points in module 'Mod'
%%% -------------------------------------------------------

delete_all_breaks(Mod) ->
    case whereis(int_db) of
	undefined ->
	    ok;
	Db ->
	    Db ! {self(),delete_all_breaks,Mod},
	    ok
    end.

%%% -------------------------------------------------------
%%% Get all current break points.
%%% Returns: [{Break,Options},...] or []
%%% -------------------------------------------------------

get_all_breaks() ->
    case whereis(int_db) of
	undefined ->
	    [];
	Db ->
	    Db ! {self(),get_all_breaks},
	    receive
		{Db,all_breaks,Breaks} ->
		    Breaks
	    end
    end.

%%% -------------------------------------------------------
%%% Get all current break points associated with Module.
%%% Returns: [{Break,Options},...] or []
%%% -------------------------------------------------------

get_all_breaks(Module) ->
    case whereis(int_db) of
	undefined ->
	    [];
	Db ->
	    Db ! {self(),get_all_breaks,Module},
	    receive
		{Db,all_breaks,Module,Breaks} ->
		    Breaks
	    end
    end.

%%% -------------------------------------------------------
%%% Fetch the database reference to the table used by Pid
%%% for Module.
%%% If Pid is located at another erlang node, fetch the
%%% reference from the database located at that node.
%%% -------------------------------------------------------

which_db(Pid,Module) -> which_mod_db(Pid,Module,is_alive()).


%%% which_mod_db (undefined, Module, _)
%%% 
%%% Pid can be undefined when none distributed, ok. But which_db/2 
%%% also gets Pid as undefined, from dbg_ui_aux:load_file/5, 
%%% when the node is distributed.

which_mod_db (undefined, Module, _) ->
    DbPid = whereis(int_db),
    which_mod_db1(undefined, Module, DbPid, false);
which_mod_db(Pid,Module,false) ->
    DbPid = whereis(int_db),
    which_mod_db1(Pid,Module,DbPid,false);
which_mod_db(Pid,Module,_) when node() == node(Pid) ->
    DbPid = whereis(int_db),
    which_mod_db1(Pid,Module,DbPid,false);
which_mod_db(Pid,Module,_) ->
    Node = node(Pid),
    case catch rpc:call(Node,erlang,whereis,[int_db]) of
	DbPid when pid(DbPid) ->
	    which_mod_db1(Pid,Module,DbPid,Node);
	_ ->
	    not_found
    end.

which_mod_db1(Pid,Module,DbPid,false) ->
    DbPid ! {self(),which_db,Module,Pid},
    receive
	{DbPid,which_db_resp,Resp} ->
	    Resp
    end;
which_mod_db1(Pid,Module,DbPid,Node) ->
    erlang:monitor_node(Node,true),
    DbPid ! {self(),which_db,Module,Pid},
    receive
	{DbPid,which_db_resp,Resp} ->
	    erlang:monitor_node(Node,false),
	    Resp;
	{nodedown,Node} ->
	    not_found
    end.

%% End of user interface
%% ====================================================
	



%%% -------------------------------------------------------
%%% Get the associated value with Key from the database
%%% table Db.
%%% -------------------------------------------------------

get_db(Db,Key) ->
    case dbg_ets:lookup(Db,Key) of
	[{Key,Value}] ->
	    Value;
	_ ->
	    undefined
    end.
