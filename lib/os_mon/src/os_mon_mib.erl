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
-module(os_mon_mib).

-export([init/1, stop/1]).
-export([load_table/1, load_table/3, disk_table/1, disk_table/3,
	 update_load_table/0, get_load/1,
	 update_disk_table/0, get_disks/1,
	 mem_sys_mark/1, mem_proc_mark/1, disk_threshold/1]).

%%%-----------------------------------------------------------------
%%% This module implements the OS-MON-MIB.
%%% The tables are implemented as shadow tables with the module
%%% snmp_shadow_table.  Here the update functions are implemented.
%%%-----------------------------------------------------------------
-record(loadTable,
	{erlNodeId, loadSystemTotalMemory, loadSystemUsedMemory,
	 loadLargestErlProcess, loadLargestErlProcessUsedMemory,
	 loadCpuLoad}).

-define(loadShadowArgs, 
	{loadTable, integer, record_info(fields, loadTable), 5000,
	 {os_mon_mib, update_load_table}}). 

-record(diskTable,
	{key, diskDescr, diskKBytes, diskCapacity}).
	
-define(diskShadowArgs, 
	{diskTable, {integer, integer}, record_info(fields, diskTable), 5000,
	 {os_mon_mib, update_disk_table}}). 

-record(diskAlloc, {diskDescr, diskId}).

-define(verify(Expr, Error), verify(catch Expr, Error, ?FILE, ?LINE)).

verify(Res, Error, File, Line) ->
    case Res of
	{atomic, _} ->
	    Res;
	ok ->
	    Res;
	_ ->
	    error_logger:format("~s(~w): crashed ~p -> ~p ~p~n",
				[File, Line, Error, Res, process_info(self())]),
	    Res
    end.

init(Agent) ->
    MibDir = code:priv_dir(os_mon) ++ "/mibs",
    snmp:load_mibs(Agent, [MibDir ++ "/OTP-OS-MON-MIB"]).

stop(Agent) ->
    snmp:unload_mibs(Agent, ["OTP-OS-MON-MIB"]).
    

load_table(Op) ->
    snmp_shadow_table:table_func(Op, ?loadShadowArgs).
load_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?loadShadowArgs).

disk_table(new) ->
    Tab = diskAlloc,
    Storage = ram_copies, 
    case lists:member(Tab, mnesia:system_info(tables)) of
	true ->
	    case mnesia:table_info(Tab, storage_type) of
		unknown ->
		    ?verify(mnesia:add_table_copy(Tab, node(), Storage),
			    [add_table_copy, Tab, node(), Storage]);
		Storage ->
		    catch delete_all(Tab)
	    end;
	false ->
	    Nodes = [node()],
	    Props = [{type, set},
		     {attributes, record_info(fields, diskAlloc)},
		     {local_content, true},
		     {Storage, Nodes}],
	    ?verify(mnesia:create_table(Tab, Props),
		    [create_table, Tab, Props])
    end,
    Rec = #diskAlloc{diskDescr = next_index, diskId = 1},
    ok = mnesia:dirty_write(Rec),
    snmp_shadow_table:table_func(new, ?diskShadowArgs);
disk_table(delete) ->
    Tab = diskAlloc,
    case ?verify(mnesia:delete_table_copy(Tab, node()),
		 [delete_table_copy, Tab, node()]) of
	{atomic, ok} ->
	    snmp_shadow_table:table_func(delete, ?diskShadowArgs);
	{aborted, Reason} ->
	    case lists:member(Tab, mnesia:system_info(tables)) of
		true ->
		    catch delete_all(Tab),
		    snmp_shadow_table:table_func(delete, ?diskShadowArgs);
		false ->
		    snmp_shadow_table:table_func(delete, ?diskShadowArgs)
	    end
    end.

disk_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?diskShadowArgs).

update_load_table() ->
    delete_all(loadTable),
    lists:foreach(
      fun(Node) ->
	      case otp_mib:get_node_id(Node) of
		  {ok, Idx} ->
		      Load = rpc:call(Node, os_mon_mib, get_load, [Idx]),
		      ok = mnesia:dirty_write(Load);
		  undefined -> ok
	      end
      end, [node() | nodes()]).

delete_all(Name) -> delete_all(mnesia:dirty_first(Name), Name).
delete_all('$end_of_table', _Name) -> done;
delete_all(Key, Name) ->
    Next = mnesia:dirty_next(Name, Key),
    ok = mnesia:dirty_delete({Name, Key}),
    delete_all(Next, Name).

get_load(Id) ->
    {Total, Allocated, {Pid, PidAllocated}} = memsup:get_memory_data(),
    #loadTable{erlNodeId = Id,
	       loadSystemTotalMemory = Total,
	       loadSystemUsedMemory = Allocated,
	       loadLargestErlProcess = pid_to_str(Pid),
	       loadLargestErlProcessUsedMemory = PidAllocated,
	       loadCpuLoad = get_cpu_load()}.

get_cpu_load() ->
    Avg1 = cpu_sup:avg1(),
    D = 50,
    100 - (D * 100) div (D + Avg1).

pid_to_str(Pid) -> lists:flatten(io_lib:format("~w", [Pid])).
    
update_disk_table() ->
    delete_all(diskTable),
    lists:foreach(
      fun(Node) ->
	      case otp_mib:get_node_id(Node) of
		  {ok, Idx} ->
		      Disks = rpc:call(Node, os_mon_mib, get_disks, [Idx]),
		      lists:foreach(fun(Disk) ->
					    mnesia:dirty_write(Disk)
				    end, Disks);
		  _ -> ok
	      end
      end, [node() | nodes()]).

get_disks(NodeId) ->
    element(1,
      lists:mapfoldl(
	fun({Descr, KByte, Capacity}, DiskId) ->
		{#diskTable{key = {NodeId, DiskId}, diskDescr = Descr,
			    diskKBytes = KByte, diskCapacity = Capacity},
		 DiskId + 1}
	end, 1, disksup:get_disk_data())).

mem_sys_mark(get) ->
    {value, memsup:get_sysmem_high_watermark()};
mem_sys_mark(_) ->
    ok.

mem_proc_mark(get) ->
    {value, memsup:get_procmem_high_watermark()};
mem_proc_mark(_) ->
    ok.

disk_threshold(get) ->
    {value, disksup:get_almost_full_threshold()};
disk_threshold(_) ->
    ok.
