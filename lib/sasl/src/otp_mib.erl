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
-module(otp_mib).

-export([init/1, stop/1]).
-export([erl_node_table/1, erl_node_table/3, appl_table/1, appl_table/3,
	 update_erl_node_table/0, get_erl_node/1,
	 update_appl_table/0, get_appls/2, get_node_id/1]).

%%%-----------------------------------------------------------------
%%% This module implements the OTP-MIB.
%%% The tables are implemented as shadow tables with the module
%%% snmp_shadow_table.  Here, the update functions are implemented.
%%%-----------------------------------------------------------------
-record(erlNodeTable,
	{erlNodeId, erlNodeName, erlNodeMachine, erlNodeVersion, 
	 erlNodeRunQueue,
         erlNodeRunTime, erlNodeWallClock, erlNodeReductions,
	 erlNodeProcesses, erlNodeInBytes, erlNodeOutBytes}).

-define(erlNodeShadowArgs, 
	{erlNodeTable, integer, record_info(fields, erlNodeTable), 5000,
	 {otp_mib, update_erl_node_table}}). 

-record(applTable, {key = '_', applName = '_', applDescr = '_',
		    applVsn = '_', valid = '_'}).

-define(applShadowArgs,
	{applTable, {integer, integer}, record_info(fields, applTable), 5000,
	 {otp_mib, update_appl_table}}).

-record(erlNodeAlloc, {nodeName, nodeId}).

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
    MibDir = code:priv_dir(sasl) ++ "/mibs",
    snmp:load_mibs(Agent, [MibDir ++ "/OTP-MIB"]).

stop(Agent) ->
    snmp:unload_mibs(Agent, ["OTP-MIB"]).
    

erl_node_table(new) ->
    Tab = erlNodeAlloc,
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
		     {attributes, record_info(fields, erlNodeAlloc)},
		     {local_content, true},
		     {Storage, Nodes}],
	    ?verify(mnesia:create_table(Tab, Props),
		    [create_table, Tab, Props])
    end,
    ok = mnesia:dirty_write({erlNodeAlloc, next_index, 1}),
    update_node_alloc(),
    snmp_shadow_table:table_func(new, ?erlNodeShadowArgs);
erl_node_table(delete) ->
    Tab = erlNodeAlloc,
    case ?verify(mnesia:del_table_copy(Tab, node()),
		 [del_table_copy, Tab, node()]) of
	{atomic, ok} ->
	    snmp_shadow_table:table_func(delete, ?erlNodeShadowArgs);
	{aborted, Reason} ->
	    case lists:member(Tab, mnesia:system_info(tables)) of
		true ->
		    catch delete_all(Tab),
		    snmp_shadow_table:table_func(delete, ?erlNodeShadowArgs);
		false ->
		    snmp_shadow_table:table_func(delete, ?erlNodeShadowArgs)
	    end
    end.
		    	    
erl_node_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?erlNodeShadowArgs).

appl_table(Op) ->
    snmp_shadow_table:table_func(Op, ?applShadowArgs).
appl_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?applShadowArgs).


update_erl_node_table() ->
    delete_all(erlNodeTable),
    update_node_alloc(),
    lists:foreach(
      fun(Node) ->
	      case get_node_id(Node) of
		  {ok, Idx} ->
		      ErlNode = rpc:call(Node, otp_mib, get_erl_node, [Idx]),
		      ok = mnesia:dirty_write(ErlNode);
		  _ -> ok
	      end
      end, [node() | nodes()]).

update_node_alloc() ->
    una([node() | nodes()]).

una([Node | T]) ->
    case mnesia:dirty_read({erlNodeAlloc, Node}) of
	[] ->
	    [{_, _, Idx}] = mnesia:dirty_read({erlNodeAlloc, next_index}),
	    ok = mnesia:dirty_write(#erlNodeAlloc{nodeName = Node, nodeId = Idx}),
	    ok = mnesia:dirty_write({erlNodeAlloc, next_index, Idx + 1});
	_ ->
	    ok
    end,
    una(T);
una([]) -> ok.

%% I don't believe this!
%% delete_all(Name) -> mnesia:delete_all(Name).
delete_all(Name) -> delete_all(mnesia:dirty_first(Name), Name).
delete_all('$end_of_table', _Name) -> done;
delete_all(Key, Name) ->
    Next = mnesia:dirty_next(Name, Key),
    ok = mnesia:dirty_delete({Name, Key}),
    delete_all(Next, Name).

get_erl_node(Id) ->
    IO = erlang:statistics(io),
    #erlNodeTable{erlNodeId = Id,
		  erlNodeName = atom_to_list(node()),
		  erlNodeVersion = erlang:info(version),
		  erlNodeMachine = erlang:info(machine),
		  erlNodeRunQueue = erlang:statistics(run_queue),
		  erlNodeRunTime = element(1, erlang:statistics(runtime)),
		  erlNodeWallClock = element(1, erlang:statistics(wall_clock)),
		  erlNodeReductions = element(1, erlang:statistics(reductions)),
		  erlNodeProcesses = length(processes()),
		  erlNodeInBytes = element(2, element(1, IO)),
		  erlNodeOutBytes = element(2, element(2, IO))}.
    
%%-----------------------------------------------------------------
%% The Valid field is toggled at each update, and all records with
%% the old value is deleted.  In this way we don't have to delete
%% the entire table before we fill it with fresh data.  I don't know
%% which strategy is the best one, so I use both :-)
%%-----------------------------------------------------------------
update_appl_table() ->
    InValid =
	case mnesia:dirty_first(applTable) of
	    X when tuple(X) -> 
		[Y] = mnesia:dirty_read({applTable, X}),
		element(size(Y), Y);
	    _ -> false
	end,
    Valid = toggle(InValid),
    lists:foreach(
      fun(Node) ->
	      case get_node_id(Node) of
		  {ok, Idx} ->
		      Appls = rpc:call(Node, otp_mib, get_appls, [Idx, Valid]),
		      lists:foreach(fun(Appl) ->
					    ok = mnesia:dirty_write(Appl)
				    end, Appls);
		  _ -> ok
	      end
      end, [node() | nodes()]),
    lists:foreach(fun(#applTable{key = Key}) ->
			  ok = mnesia:dirty_delete({applTable, Key})
		  end,
		  mnesia:dirty_match_object(#applTable{valid = InValid})).

get_appls(NodeId, Valid) ->
    element(1,
      lists:mapfoldl(
	fun({ApplName, ApplDescr, ApplVsn}, ApplId) ->
		{#applTable{key = {NodeId, ApplId},
			    applName = atom_to_list(ApplName),
			    applDescr = ApplDescr,
			    applVsn = ApplVsn,
			    valid = Valid},
		 ApplId + 1}
	end, 1, application:which_applications())).

get_node_id(Node) ->
    case mnesia:dirty_read({erlNodeAlloc, Node}) of
	[{_,_,Idx}] -> {ok, Idx};
	_ -> undefined
    end.


toggle(true) -> false;
toggle(false) -> true.
