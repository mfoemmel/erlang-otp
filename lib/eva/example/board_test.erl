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
-module(board_test).
-compile(export_all).

-record(boardTable, {index, name, type}).

-include_lib("eva/include/eva.hrl").

%%%-----------------------------------------------------------------
%%% Test code
%%%-----------------------------------------------------------------
%%% This example makes use of the BOARD-MIB in eva/examples

%% Creates the directory './ex_log' if it doesn't exist.
init_test() ->
    file:make_dir("ex_log"),
    init_mnesia().

init_mnesia() ->
    mnesia:create_table([{name, boardTable},
			 {snmp, [{key, integer}]},
			 {attributes, record_info(fields, boardTable)}]),
    mnesia:dirty_write(#boardTable{index = 1, name = "board1", type = "t1"}),
    mnesia:dirty_write(#boardTable{index = 2, name = "board2", type = "t2"}),
    mnesia:dirty_write(#boardTable{index = 4, name = "board4", type = "t4"}).
    
slog() ->
    disk_log:open([{name, "board"}, {file, "ex_log/board.LOG"},
		   {type, wrap}, {size, {10000, 4}}]),
    eva_log:open("board", {eva_log_snmp, log_filter, [[4]]}, 3600).

init() ->
    reg(),
    slog().

init_snmp() ->
    init(),
    mgm_init().

%S1
%%%-----------------------------------------------------------------
%%% Resource code 
%%%-----------------------------------------------------------------
reg() ->
    eva:register_event(boardRemoved, true),
    eva:register_event(boardInserted, false),
    eva:register_alarm(boardFailure, true, equipment, minor).

remove_board(No) ->
    eva:send_event(boardRemoved, {board, No}, []).

insert_board(No, BoardName, BoardType) ->
    eva:send_event(boardInserted, {board, No}, {BoardName, BoardType}).

board_on_fire(No) ->
    FaultId = eva:get_fault_id(),
    %% Cause = fire, ExtraParams = []
    eva:send_alarm(boardFailure, FaultId, {board, No}, fire, []),
    FaultId.
%S1

%S2
%%%-----------------------------------------------------------------
%%% SNMP adaptation code
%%%-----------------------------------------------------------------
mgm_init() ->
    snmp:load_mibs(snmp_master_agent, ["BOARD-MIB"]),
    Events = [{boardRemoved, boardRemoved, snmpTrap, "standard trap",
	       {?MODULE, boardRemoved}},
	      {boardInserted, boardInserted, snmpTrap, "standard trap",
	       {?MODULE, boardInserted}}],
    Alarms = [{boardFailure, boardFailure, snmpTrap, "standard trap",
	       {?MODULE, boardFailure}}],
    eva_snmp_adaptation:register_events(Events),
    eva_snmp_adaptation:register_alarms(Alarms).


%%-----------------------------------------------------------------
%% instrumentation functions
%%-----------------------------------------------------------------

% Using default instrumentation

%%-----------------------------------------------------------------
%% "backwards" instrumentation functions  event -> trap
%%-----------------------------------------------------------------
boardRemoved(#event{sender = {board, Idx}}) ->
    [#boardTable{name = Name}] = mnesia:dirty_read({boardTable, Idx}),
    {ok, [{boardName, [Idx], Name}]}.

boardInserted(#event{sender = {board, Idx}, extra = {Name, Type}}) ->
    {ok, [{boardName, [Idx], Name},
	  {boardType, [Idx], Type}]}.

boardFailure(#alarm{sender = {board, Idx}, cause = Cause}) ->
    [#boardTable{name = Name}] = mnesia:dirty_read({boardTable, Idx}),
    {value, Oid} = snmp:name_to_oid(boardName),
    {value, COid} = snmp_cause(Cause),
    {ok, {Oid ++ [Idx], COid, [{boardName, [Idx], Name}]}}.

snmp_cause(fire) -> snmp:name_to_oid(fire);
snmp_cause(_) -> [0,0].
%S2
