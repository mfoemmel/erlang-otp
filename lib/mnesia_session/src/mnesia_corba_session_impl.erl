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
-module(mnesia_corba_session_impl).

%%% Purpose : Callback module which handles all session functions

-export([init/1, terminate/2]).

%% Dirty
-export([dirty_read/4,
	 dirty_index_read/5,
	 dirty_delete/4,
	 dirty_delete_object/4,
	 dirty_update_counter/5,
	 dirty_write/4,
	 dirty_all_keys/3,
	 dirty_slot/4,
	 dirty_first/3,
	 dirty_next/4,
	 dirty_match_all/3
	]).

%% Admin Database management
-export([activate_checkpoint/3,
	 add_table_copy/5,
	 add_table_index/4,
	 backup1/3,
	 backup2/4,
	 backup_checkpoint1/4,
	 backup_checkpoint2/5,
	 change_table_access_mode/4,
	 change_table_copy_type/5,
	 change_table_load_order/4,
	 create_table/4,
	 deactivate_checkpoint/3,
	 del_table_copy/4,
	 del_table_index/4,
	 delete_table/3,
	 dump_log/2,
	 dump_tables/3,
	 dump_to_textfile/3,
	 force_load_table/3,
	 install_fallback1/3,
	 install_fallback2/4,
	 load_textfile/3,
	 move_table_copy/5,
	 set_master_nodes1/3,
	 set_master_nodes2/4,
	 uninstall_fallback/2,
	 wait_for_tables/4,
	 table_info/3,
	 system_info/2
	]).

init(ConnectionType) ->
    mnesia_session_impl:init(ConnectionType).

terminate(Reason, State) ->
    mnesia_session_impl:terminate(Reason, State).

%% Dirty functions

dirty_write(_This, State, Tab, Obj) -> 
    mnesia_session_impl:dirty_write(State, Tab, Obj).

dirty_read(_This, State, Tab, KeyObj) ->
    mnesia_session_impl:dirty_read(State, Tab, KeyObj).

dirty_update_counter(_This, State, Tab, KeyObj, Val) ->
    mnesia_session_impl:dirty_update_counter(State, Tab, KeyObj, Val).

dirty_delete(_This, State, Tab, KeyObj) ->
    mnesia_session_impl:dirty_delete(State, Tab, KeyObj).

dirty_delete_object(_This, State, Tab, RecordObj) ->
    mnesia_session_impl:dirty_delete_object(State, Tab, RecordObj).

dirty_slot(_This, State, Tab, SlotNr) ->
    mnesia_session_impl:dirty_slot(State, Tab, SlotNr).

dirty_first(_This, State, TabName) ->
    mnesia_session_impl:dirty_first(State, TabName).

dirty_next(_This, State, TabName, KeyObj) ->
    mnesia_session_impl:dirty_next(State, TabName, KeyObj).

dirty_all_keys(_This, State, TabName) ->
    mnesia_session_impl:dirty_all_keys(State, TabName).

dirty_match_all(_This, State, TabName) ->
    mnesia_session_impl:dirty_match_all(State, TabName).

dirty_index_read(_This, State, Tab, KeyObj, Pos) ->
    mnesia_session_impl:dirty_index_read(State, Tab, KeyObj, Pos).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Database management

backup1(_This, State, Filename) ->
    mnesia_session_impl:backup1(State, Filename).

backup2(_This, State, OpaqueData, Module) ->
    mnesia_session_impl:backup2(State, OpaqueData, Module).

install_fallback1(_This, State, Filename) ->
    mnesia_session_impl:install_fallback1(State, Filename).

install_fallback2(_This, State, OpaqueData, Module) ->
    mnesia_session_impl:install_fallback2(State, OpaqueData, Module).

uninstall_fallback(_This, State) ->
    mnesia_session_impl:uninstall_fallback(State).

create_table(_This, State, TabName, Tabdef) ->
    mnesia_session_impl:create_table(State, TabName, Tabdef).

delete_table(_This, State, TabName) ->
    mnesia_session_impl:delete_table(State, TabName).

add_table_copy(_This, State, TabName, ToNode, ReplicaStorage) ->
    mnesia_session_impl:add_table_copy(State, TabName, ToNode, ReplicaStorage).

del_table_copy(_This, State, TabName, FromNode) ->
    mnesia_session_impl:del_table_copy(State, TabName, FromNode).

move_table_copy(_This, State, TabName, FromNode, ToNode) ->
    mnesia_session_impl:move_table_copy(State, TabName, FromNode, ToNode).

add_table_index(_This, State, TabName, ColumnNr) ->
    mnesia_session_impl:add_table_index(State, TabName, ColumnNr).

del_table_index(_This, State, TabName, ColumnNr) ->
    mnesia_session_impl:del_table_index(State, TabName, ColumnNr).

change_table_copy_type(_This, State, TabName, NodeName, SType) ->
    mnesia_session_impl:change_table_copy_type(State, TabName, NodeName, SType).
 
change_table_access_mode(_This, State, TabName, Mode) ->
    mnesia_session_impl:change_table_access_mode(State, TabName, Mode).

wait_for_tables(_This, State, TabNames, Timeout) ->
    mnesia_session_impl:wait_for_tables(State, TabNames, Timeout).

force_load_table(_This, State, TabName) ->
    mnesia_session_impl:force_load_table(State, TabName).

change_table_load_order(_This, State, TabName, LoadOrder) ->
    mnesia_session_impl:change_table_load_order(State, TabName, LoadOrder).

set_master_nodes1(_This, State, NodeNames) ->
    mnesia_session_impl:set_master_nodes1(State, NodeNames).

set_master_nodes2(_This, State, TabName, NodeNames) ->
    mnesia_session_impl:set_master_nodes2(State, TabName, NodeNames).

dump_log(_This, State) ->
    mnesia_session_impl:dump_log(State).

dump_tables(_This, State, TableList) ->
    mnesia_session_impl:dump_tables(State, TableList).

activate_checkpoint(_This, State, CpDef) ->
    mnesia_session_impl:activate_checkpoint(State, CpDef).

deactivate_checkpoint(_This, State, CpName) ->
    mnesia_session_impl:deactivate_checkpoint(State, CpName).

backup_checkpoint1(_This, State, CpName, Filename) ->
    mnesia_session_impl:backup_checkpoint1(State, CpName, Filename).

backup_checkpoint2(_This, State, CpName, OpaqueData, ModName) ->
    mnesia_session_impl:backup_checkpoint2(State, CpName, OpaqueData, ModName).

load_textfile(_This, State, Filename) ->
    mnesia_session_impl:load_textfile(State, Filename).

dump_to_textfile(_This, State, Filename) ->
    mnesia_session_impl:dump_to_textfile(State, Filename).

table_info(_This, State, TabName) -> 
    mnesia_session_impl:table_info(State, TabName) .

system_info(_This, State) ->
    mnesia_session_impl:system_info(State).

