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
-module(mnesia_session_impl).

%%% Purpose : Callback module which handles all session functions

-export([init/1, terminate/2]).

%% Dirty
-export([dirty_read/3, dirty_index_read/4,
	 dirty_delete/3, dirty_delete_object/3,
	 dirty_update_counter/4, dirty_write/3,
	 dirty_all_keys/2, dirty_slot/3,
	 dirty_first/2,	 dirty_next/3,
	 dirty_match_all/2, dirty_match_object/3, dirty_index_match_object/4
	]).

%% Admin Database management
-export([activate_checkpoint/2,
	 add_table_copy/4,
	 add_table_index/3,
	 backup1/2,
	 backup2/3,
	 backup_checkpoint1/3,
	 backup_checkpoint2/4,
	 change_table_access_mode/3,
	 change_table_copy_type/4,
	 change_table_load_order/3,
	 create_table/3,
	 deactivate_checkpoint/2,
	 del_table_copy/3,
	 del_table_index/3,
	 delete_table/2,
	 dump_log/1,
	 dump_tables/2,
	 dump_to_textfile/2,
	 force_load_table/2,
	 install_fallback1/2,
	 install_fallback2/3,
	 load_textfile/2,
	 move_table_copy/4,
	 set_master_nodes1/2,
	 set_master_nodes2/3,
	 uninstall_fallback/1,
	 wait_for_tables/3,
	 table_info/2,
	 system_info/1,
	 create_schema/2,
	 delete_schema/2,
	 start_mnesia/1,
	 stop_mnesia/1
	]).

-record(state, {client_type}).
-include_lib("mnesia_session/include/mnesia.hrl").
-include_lib("orber/include/corba.hrl").

init(ConnectionType) -> {ok, #state{client_type = ConnectionType}}.

terminate(Reason, State) ->  ok.

%% Dirty functions

dirty_write(State, Tab, Obj) -> 
    Object = type_transform(Obj, State#state.client_type),
    Res = (catch mnesia:dirty_write(list_to_atom(Tab), Object)),
    handle_result(State, Res, noreturn).

dirty_read(State, Tab, KeyObj) ->
    Key = type_transform(KeyObj, State#state.client_type),
    Res = (catch mnesia:dirty_read(list_to_atom(Tab), Key)),
    handle_result(State, Res, reclist).

dirty_update_counter(State, Tab, KeyObj, Val) ->
    Key = type_transform(KeyObj, State#state.client_type),
    Res = (catch mnesia:dirty_update_counter(list_to_atom(Tab), Key, Val)),
    handle_result(State, Res, {val, integer}).

dirty_delete(State, Tab, KeyObj) ->
    Key = type_transform(KeyObj, State#state.client_type),
    Res = (catch mnesia:dirty_delete(list_to_atom(Tab), Key)),
    handle_result(State, Res, noreturn).

dirty_delete_object(State, Tab, RecordObj) ->
    Object = type_transform(RecordObj, State#state.client_type),
    Res = (catch mnesia:dirty_delete_object(list_to_atom(Tab), Object)),
    handle_result(State, Res, noreturn).

dirty_slot(State, Tab, SlotNr) ->
    Res = (catch mnesia:dirty_slot(list_to_atom(Tab), SlotNr)),    
    case Res of 
	'$end_of_table' -> 
	    handle_result(State, end_of_table, reclist); 
	Else ->
	    handle_result(State, Else, reclist)
    end.

dirty_first(State, TabName) ->
    Tab = list_to_atom(TabName),
    Res = (catch mnesia:dirty_first(Tab)),
    case Res of 
	'$end_of_table' ->  
	    handle_result(State, end_of_table, any_null);
	Else ->
	    case State#state.client_type of
		corba_session ->
		    RecName = mnesia:table_info(Tab, record_name),
		    TabType = RecName:tc(),
		    KeyType = get_keytype(TabType),
		    Obj = #any{typecode = KeyType, value = Else},
		    handle_result(State, Obj, any_null);
		session ->
		    handle_result(State, Else, any_null)
	    end
    end.

dirty_next(State, TabName, KeyObj) ->
    Key = type_transform(KeyObj, State#state.client_type),
    Res = (catch mnesia:dirty_next(list_to_atom(TabName), Key)),
    case Res of
	'$end_of_table' ->
	    handle_result(State, end_of_table, any_null);
	Else ->
	    case State#state.client_type of
		corba_session ->
		    KeyType = KeyObj#any.typecode,
		    Obj = #any{typecode = KeyType, value = Else},
		    handle_result(State, Obj, any_null);
		session ->
		    handle_result(State, Else, any_null)
	    end
    end.

dirty_all_keys(State, TabName) ->
    Tab = list_to_atom(TabName),
    Res = (catch mnesia:dirty_all_keys(Tab)),
    case State#state.client_type of
	corba_session ->
	    RecName = mnesia:table_info(Tab, record_name),
	    TabType = RecName:tc(),
	    KeyType = get_keytype(TabType),
	    handle_result(State, Res, {keylist, KeyType});
	session ->
	    handle_result(State, Res, {keylist, undefined})
    end.

% Not supported in corba
dirty_match_object(State, Tab, Pattern)
  when State#state.client_type /= corba_session ->   
    Res = (catch mnesia:dirty_match_object(list_to_atom(Tab), Pattern)),
    handle_result(State, Res, reclist).

dirty_match_all(State, TabName) ->
    Res = (catch mnesia:dirty_match_object(list_to_atom(TabName), 
					   mnesia:table_info(list_to_atom(TabName),
							     wild_pattern))),
    handle_result(State, Res, reclist).

dirty_index_read(State, Tab, KeyObj, Pos) ->
    Key = type_transform(KeyObj, State#state.client_type),
    Res = (catch mnesia:dirty_index_read(list_to_atom(Tab), Key, Pos)),
    handle_result(State, Res, reclist).

%% Not supported in corba
dirty_index_match_object(State, Tab, Pattern, Pos) 
  when State#state.client_type /= corba_session ->
    Res = (catch mnesia:dirty_index_match_object(list_to_atom(Tab), 
						 Pattern, Pos)),
    handle_result(State, Res, reclist).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Database management

backup1(State, Filename) ->
    Res = (catch mnesia:backup(Filename)),
    handle_result(State, Res, noreturn).
backup2(State, OpaqueData, Module) ->
    Opaque = type_transform(OpaqueData, State#state.client_type),
    Res = (catch mnesia:backup(Opaque, list_to_atom(Module))),
    handle_result(State, Res, noreturn).

install_fallback1(State, Filename) ->
    Res = (catch mnesia:install_fallback(Filename)),
    handle_result(State, Res, noreturn).
install_fallback2(State, OpaqueData, Module) ->
    Opaque = type_transform(OpaqueData, State#state.client_type),
    Res = (catch mnesia:install_fallback(Opaque, list_to_atom(Module))),
    handle_result(State, Res, noreturn).

uninstall_fallback(State) ->
    Res = (catch mnesia:uninstall_fallback()),
    handle_result(State, Res, noreturn).

create_table(State, TabName, Tabdef) ->
    Tab = list_to_atom(TabName),
    SetOrbag = {type, Tabdef#mnesia_TableDef.type},
    Mode = {access_mode, Tabdef#mnesia_TableDef.mode},
    Ram  = {ram_copies, strings_to_atomlist(Tabdef#mnesia_TableDef.ram_copies)},
    Disc = {disc_copies,strings_to_atomlist(Tabdef#mnesia_TableDef.disc_copies)},
    DiscOnly = {disc_only_copies, 
		strings_to_atomlist(Tabdef#mnesia_TableDef.disc_only_copies)},
    Index = {index, Tabdef#mnesia_TableDef.index_list},
    Attrs = {attributes, strings_to_atomlist(Tabdef#mnesia_TableDef.attributes)},
    RecName = {record_name, list_to_atom(Tabdef#mnesia_TableDef.record_name)},
    Def = [SetOrbag, Mode, Ram, Disc, DiscOnly, Index, Attrs, RecName],
    Res = (catch mnesia:create_table(Tab, Def)),
    handle_result(State, Res, noreturn).

delete_table(State, TabName) ->
    Res = (catch mnesia:delete_table(list_to_atom(TabName))),
    handle_result(State, Res, noreturn).

add_table_copy(State, TabName, ToNode, ReplicaStorage) ->
    Res = (catch mnesia:add_table_copy(list_to_atom(TabName),
				       list_to_atom(ToNode),
				       ReplicaStorage)),
    handle_result(State, Res, noreturn).

del_table_copy(State, TabName, FromNode) ->
    Res = (catch mnesia:del_table_copy(list_to_atom(TabName),
				       list_to_atom(FromNode))),
    handle_result(State, Res, noreturn).

move_table_copy(State, TabName, FromNode, ToNode) ->
    Res = (catch mnesia:move_table_copy(list_to_atom(TabName),
					list_to_atom(FromNode),
					list_to_atom(ToNode))),
    handle_result(State, Res, noreturn).

add_table_index(State, TabName, ColumnNr) ->
    Res = (catch mnesia:add_table_index(list_to_atom(TabName), ColumnNr)),
    handle_result(State, Res, noreturn).

del_table_index(State, TabName, ColumnNr) ->
    Res = (catch mnesia:del_table_index(list_to_atom(TabName), ColumnNr)),
    handle_result(State, Res, noreturn).

change_table_copy_type(State, TabName, NodeName, SType) ->
    Res = (catch mnesia:change_table_copy_type(list_to_atom(TabName),
					       list_to_atom(NodeName),
					       SType)),
    handle_result(State, Res, noreturn).
 
change_table_access_mode(State, TabName, Mode) ->
    Res = (catch mnesia:change_table_access_mode(list_to_atom(TabName), Mode)),
    handle_result(State, Res, noreturn).

wait_for_tables(State, TabNames, Timeout) ->
    Res = (catch mnesia:wait_for_tables(strings_to_atomlist(TabNames), Timeout)),
    handle_result(State, Res, tablist).

force_load_table(State, TabName) ->
    Res = case (catch mnesia:force_load_table(list_to_atom(TabName))) of
	      yes -> ok;
	      Else -> Else
	  end,
    handle_result(State, Res, noreturn).

change_table_load_order(State, TabName, LoadOrder) ->
    Res = (catch mnesia:change_table_load_order(list_to_atom(TabName), LoadOrder)),
    handle_result(State, Res, noreturn).

set_master_nodes1(State, NodeNames) ->
    Nodes = strings_to_atomlist(NodeNames),
    Res = (catch mnesia:set_master_nodes(Nodes)),
    handle_result(State, Res, noreturn).

set_master_nodes2(State, TabName, NodeNames) ->
    Nodes = strings_to_atomlist(NodeNames),
    Res = (catch mnesia:set_master_nodes(list_to_atom(TabName), Nodes)),
    handle_result(State, Res, noreturn).

dump_log(State) ->
    case mnesia:dump_log() of 
	dumped -> 
	    handle_result(State, ok, noreturn);
	Else ->
	    handle_result(State, Else, noreturn)
    end.

dump_tables(State, TableList) ->
    Res = (catch mnesia:dump_tables(strings_to_atomlist(TableList))),
    handle_result(State, Res, noreturn).

activate_checkpoint(State, CpDef) ->
    Name = {name, list_to_atom(CpDef#mnesia_CheckpointDef.cpName)},
    Max = {max, strings_to_atomlist(CpDef#mnesia_CheckpointDef.max)},
    Min = {min, strings_to_atomlist(CpDef#mnesia_CheckpointDef.min)},
    AR =  {allow_remote, CpDef#mnesia_CheckpointDef.allow_remote},
    ROD = {ram_overrides_dump, CpDef#mnesia_CheckpointDef.ram_overrides_dump},
    Def = [Name, Max, Min, AR, ROD],
    case (catch mnesia:activate_checkpoint(Def)) of
	{ok, _, _} ->
	    handle_result(State, ok, noreturn);
	Else ->
	    handle_result(State, Else, noreturn)
    end.

deactivate_checkpoint(State, CpName) ->
    Res = (catch mnesia:deactivate_checkpoint(list_to_atom(CpName))),
    handle_result(State, Res, noreturn).

backup_checkpoint1(State, CpName, Filename) ->
    Res = (catch mnesia:backup_checkpoint(list_to_atom(CpName), 
					  list_to_atom(Filename))),
    handle_result(State, Res, noreturn).

backup_checkpoint2(State, CpName, OpaqueData, ModName) ->
    Res = (catch mnesia:backup_checkpoint(list_to_atom(CpName), 
					  type_transform(OpaqueData, State#state.client_type),
					  list_to_atom(ModName))),
    handle_result(State, Res, noreturn).

load_textfile(State, Filename) ->
    Res = (catch mnesia:load_textfile(list_to_atom(Filename))),
    handle_result(State, Res, noreturn).

dump_to_textfile(State, Filename) ->
    Res = (catch mnesia:dump_to_textfile(list_to_atom(Filename))),
    handle_result(State, Res, noreturn).

table_info(State, TabName) -> 
    case catch mnesia:table_info(list_to_atom(TabName), all) of 
	Res when list(Res) ->	    
	    Info = remap_tableinfo(Res, #mnesia_TableInfo{}),
	    handle_result(State, Info, {val, table_info});
	Else ->
	    handle_result(State, Else, {val, table_info})
    end.

system_info(State) ->
    case catch mnesia:system_info(all) of 
	Res when list(Res) ->	    
	    Info = remap_systeminfo(Res, empty_systeminfo()),
	    handle_result(State, Info, {val, system_info});
	Else ->
	    handle_result(State, Else, {val, system_info})
    end.

%% Functions NOT supported in corba

create_schema(State, Nodes) when State#state.client_type /= corba_session ->
    Res = (catch mnesia:create_schema(strings_to_atomlist(Nodes))),
    handle_result(State, Res, noreturn).

delete_schema(State, Nodes) when State#state.client_type /= corba_session ->
    Res = (catch mnesia:delete_schema(strings_to_atomlist(Nodes))),
    handle_result(State, Res, noreturn).
    
start_mnesia(State) when State#state.client_type /= corba_session ->
    Res = (catch mnesia:start()),
    handle_result(State, Res, noreturn).

stop_mnesia(State) when State#state.client_type /= corba_session ->
    case (catch mnesia:stop()) of
	stopped -> 
	    handle_result(State, ok, noreturn);
	Else ->
	    handle_result(State, Else, noreturn)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internals

%% Error cases
handle_result(State, {aborted, Reason}, Type) ->
    handle_error(State,  Reason, Type);
handle_result(State, {'EXIT', Reason}, Type) ->
    handle_error(State,  Reason, Type);
handle_result(State, {error, Reason}, Type) -> 
    handle_error(State,  Reason, Type);

handle_result(State, ok, noreturn) -> 
    {reply, {ok, ""}, State};
handle_result(State, ok, tablist) ->                 %% Used by wait_for_tables             
    {reply, {ok, [], ""}, State};        
handle_result(State, {timeout, TableList}, tablist) -> %%Used by wait_for_tables
    {reply, {timeout, atomlist_to_stringlist(TableList), ""}, State}; 
handle_result(State, {atomic, ok}, noreturn) -> 
    {reply, {ok, ""}, State};
handle_result(State, end_of_table, reclist) ->
    {reply, {end_of_table, [], ""}, State};
handle_result(State, end_of_table, any_null) 
  when State#state.client_type == corba_session ->    
    {reply, {end_of_table, #any{typecode = tk_null, value = null}, ""}, State};
handle_result(State, end_of_table, any_null) 
  when State#state.client_type == session ->    
    {reply, {end_of_table, null, ""}, State};
handle_result(State, ResList, reclist) 
  when list(ResList), State#state.client_type == corba_session ->
    case catch remap_anytype(ResList) of
	{'EXIT', Reason} -> 
	    handle_result(State, {'EXIT', Reason}, reclist);
	Result ->
	    {reply, {ok, Result, ""}, State}
    end;
handle_result(State, ResList, reclist) 
  when list(ResList), State#state.client_type == session ->
    {reply, {ok, ResList, ""}, State};

handle_result(State, ResList, {keylist, _}) 
  when list(ResList), State#state.client_type == session ->
    {reply, {ok, ResList, ""}, State};
handle_result(State, ResList, {keylist, TC}) 
  when list(ResList), State#state.client_type == corba_session ->
    Res = [#any{typecode = TC, value = Val} || Val <- ResList],
    {reply, {ok, Res, ""}, State};

handle_result(State, Val, {val, _}) ->
    {reply, {ok, Val, ""}, State};
handle_result(State, Val, any_null) ->
    {reply, {ok, Val, ""}, State}.

handle_error(State, Reason, Type) ->
    ErrorString = flatten_error(Reason),
    case Type of
	noreturn ->
	    {reply, {error, ErrorString}, State};
	reclist -> 
	    {reply, {error, [], ErrorString}, State};
	tablist -> 
	    {reply, {error, [], ErrorString}, State};
	{keylist, _TC} -> 
	    {reply, {error, [], ErrorString}, State};
	{val, integer} ->
	    {reply, {error, 0, ErrorString}, State};
	{val, table_info} ->
	    {reply, {error, empty_tableinfo(), ErrorString}, State};
	{val, system_info} ->
	    {reply, {error, empty_systeminfo(), ErrorString}, State};
%%	{cyclic, _, _, _, _, _} ->  
%%	    {reply, {deadlock, "Lock not granted"}, State};
	any_null ->
	    case State#state.client_type of
		corba_session ->
		    {reply, {error, #any{typecode = tk_null, value = null}, 
			     ErrorString}, State};
		session ->
		    {reply, {error, null, ErrorString}, State}
	    end
    end.


empty_tableinfo() ->
    #mnesia_TableInfo{
		 mode = read_only, attributes = [],
		 arity = 0, checkpoints = [],
		 ram_copies = [], disc_copies = [], disc_only_copies = [],
		 indexlist = [], load_order = 0, local_content = true,
		 master_nodes = [],  memory = 0, size = 0,
		 %%snmp = true, 
		 storage_type = ram_copies,
		 type = set, where_to_read = "", where_to_write = "",
		 record_name = ""
		}.

empty_systeminfo() ->
    #mnesia_SystemInfo{
		  auto_repair = true,
		  backup_module = "",
		  checkpoints = [],
		  event_module = "",
		  db_nodes = [],
		  debug = "",
		  directory = "",
		  dump_log_load_regulation = true,
		  dump_log_time_threshold = 0,
		  dump_log_update_in_place = true,
		  dump_log_write_threshold = 0,
		  extra_db_nodes = [],
		  fallback_activated = true,
		  %% held_locks = ,
		  is_running = true,
		  local_tables = [],
		  %%lock_queue = ,
		  %%log_version = ,
		  master_node_tables = [],
		  %% protocol_version = ,
		  %% communication protocol = ,
		  running_db_nodes = [],
		  schema_location = "",
		  %%subscribers = ,
		  tables = [],
		  %%transactions = ,
		  transaction_failures = 0,
		  transaction_commits = 0,
		  transaction_restarts = 0,
		  transaction_log_writes = 0,
		  use_dir = true
		  %% version = ,
		 }.

remap_tableinfo([], S) ->
    S;
remap_tableinfo([{access_mode, Mode}|R], S) ->
    S1 = S#mnesia_TableInfo{mode = Mode},
    remap_tableinfo(R, S1);
remap_tableinfo([{attributes, Attr}|R], S) ->
    S1 = S#mnesia_TableInfo{attributes = atomlist_to_stringlist(Attr)},
    remap_tableinfo(R, S1);
remap_tableinfo([{arity, Arity}|R], S) ->
    S1 = S#mnesia_TableInfo{arity = Arity},
    remap_tableinfo(R, S1);
remap_tableinfo([{checkpoints, Cps}|R], S) ->
    S1 = S#mnesia_TableInfo{checkpoints = atomlist_to_stringlist(Cps)},
    remap_tableinfo(R, S1);
remap_tableinfo([{ram_copies, RNs}|R], S) ->
    S1 = S#mnesia_TableInfo{ram_copies = atomlist_to_stringlist(RNs)},
    remap_tableinfo(R, S1);
remap_tableinfo([{disc_copies, DCs}|R], S) ->
    S1 = S#mnesia_TableInfo{disc_copies = atomlist_to_stringlist(DCs)},
    remap_tableinfo(R, S1);
remap_tableinfo([{disc_only_copies, DOCs}|R], S) ->
    S1 = S#mnesia_TableInfo{disc_only_copies = atomlist_to_stringlist(DOCs)},
    remap_tableinfo(R, S1);
remap_tableinfo([{index, Idxs}|R], S) ->
    S1 = S#mnesia_TableInfo{indexlist = Idxs},
    remap_tableinfo(R, S1);
remap_tableinfo([{load_order, LO}|R], S) ->
    S1 = S#mnesia_TableInfo{load_order = LO},
    remap_tableinfo(R, S1);
remap_tableinfo([{local_content, Bool}|R], S) ->
    S1 = S#mnesia_TableInfo{local_content = Bool},
    remap_tableinfo(R, S1);
remap_tableinfo([{master_nodes, MNs}|R], S) ->
    S1 = S#mnesia_TableInfo{master_nodes = atomlist_to_stringlist(MNs)},
    remap_tableinfo(R, S1);
remap_tableinfo([{memory, Mem}|R], S) ->
    S1 = S#mnesia_TableInfo{memory = Mem},
    remap_tableinfo(R, S1);
remap_tableinfo([{size, Sz}|R], S) ->
    S1 = S#mnesia_TableInfo{size = Sz},
    remap_tableinfo(R, S1);
%remap_tableinfo([{snmp, Bool}|R], S) ->
%    S1 = S#mnesia_TableInfo{snmp = Bool},
%    remap_tableinfo(R, S1);
remap_tableinfo([{storage_type, ST}|R], S) ->
    S1 = S#mnesia_TableInfo{storage_type = ST},
    remap_tableinfo(R, S1);
remap_tableinfo([{setorbag, SoB}|R], S) ->
    S1 = S#mnesia_TableInfo{type = SoB},
    remap_tableinfo(R, S1);
remap_tableinfo([{where_to_read, Node}|R], S) ->
    S1 = S#mnesia_TableInfo{where_to_read = atom_to_list(Node)},
    remap_tableinfo(R, S1);
remap_tableinfo([{where_to_write, Node}|R], S) ->
    S1 = S#mnesia_TableInfo{where_to_write = atomlist_to_stringlist(Node)},
    remap_tableinfo(R, S1);
remap_tableinfo([{record_name, Name}|R], S) ->
    S1 = S#mnesia_TableInfo{record_name = atom_to_list(Name)},
    remap_tableinfo(R, S1);
remap_tableinfo([_|R], S) ->
    remap_tableinfo(R, S).

remap_systeminfo([], S) -> S;
remap_systeminfo([{auto_repair, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{auto_repair= Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{backup_module, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{backup_module = atom_to_list(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{checkpoints, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{checkpoints = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{event_module, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{event_module = atom_to_list(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{db_nodes, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{db_nodes = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{debug, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{debug = atom_to_list(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{directory, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{directory = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{dump_log_load_regulation, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{dump_log_load_regulation = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{dump_log_time_threshold, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{dump_log_time_threshold = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{dump_log_update_in_place, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{dump_log_update_in_place = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{dump_log_write_threshold, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{dump_log_write_threshold = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{extra_db_nodes, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{extra_db_nodes = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
remap_systeminfo([{fallback_activated, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{fallback_activated = Val},
    remap_systeminfo(R, S1);
	     %% held_locks
remap_systeminfo([{is_running, yes} | R], S) ->
    S1 = S#mnesia_SystemInfo{is_running = true},
    remap_systeminfo(R, S1);
remap_systeminfo([{is_running, _} | R], S) ->
    S1 = S#mnesia_SystemInfo{is_running = false},
    remap_systeminfo(R, S1);
remap_systeminfo([{local_tables, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{local_tables = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
	     %%lock_queue
	     %%log_version
remap_systeminfo([{master_node_tables, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{master_node_tables = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
	     %% protocol_version
	     %% communication protocol. 
remap_systeminfo([{running_db_nodes, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{running_db_nodes = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
	     %% subscirbers
remap_systeminfo([{tables, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{tables = atomlist_to_stringlist(Val)},
    remap_systeminfo(R, S1);
	     %% transactions
remap_systeminfo([{transaction_failures, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{transaction_failures = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{transaction_commits, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{transaction_commits = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{transaction_restarts, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{transaction_restarts = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{transaction_log_writes, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{transaction_log_writes = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([{use_dir, Val} | R], S) ->
    S1 = S#mnesia_SystemInfo{use_dir = Val},
    remap_systeminfo(R, S1);
remap_systeminfo([_ | R], S) ->
    remap_systeminfo(R, S).

type_transform(Any, corba_session) when record(Any, any) ->
    Any#any.value;
type_transform(Type, session) ->
    Type.

atomlist_to_stringlist([]) -> [];
atomlist_to_stringlist([H|R]) -> 
    [atom_to_list(H) | atomlist_to_stringlist(R)].

strings_to_atomlist([]) ->  [];
strings_to_atomlist([H|R]) ->
    [list_to_atom(H) | strings_to_atomlist(R)].

%% Corba records remapping!
remap_anytype([]) -> [];
remap_anytype([L |Lrest]) ->
    RecName = element(1, L),
    Type = RecName:tc(),
    [#any{typecode = Type, value = Obj} || Obj <- [L |Lrest]].

get_keytype(Type) ->
    {tk_struct, _Ref, _Tab, Elements} = Type,
    element(2, hd(Elements)).

%% BUG BUG
flatten_error(Error) ->
    lists:flatten(io_lib:format("~p", [Error])).
