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
-module(eva_snmp_adaptation).

-behaviour(gen_event).

-include("OTP-EVA-MIB.hrl").
-include("eva.hrl").

-record(state, {event_struct, next_event_index = 1, alarm_struct,
		last_time_sent = snmp:date_and_time(), mibs}).
-record(eva_snmp_map, {key, val}).

%% External exports
-export([create_tables/1, start_link/0, start/0]).
-export([register_events/1, register_alarms/1, name2index/1]).
-export([table_func/2, table_func/4, numberOfCurAlarms/1,
	 curAlarmLastTimeChanged/1, curAlarmTable/1, curAlarmTable/3,
	 clear_alarm/1, eventTable/3, alarmTable/3]).

%% Internal exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).
-export([init/2, system_continue/3, system_terminate/4, system_code_change/4]).

%%%-----------------------------------------------------------------
%%% General
%%% -------
%%% Adaptation of event and alarm handling (EVA) for SNMP.  This
%%% module implements the OTP-EVA-MIB, which is the SNMP interface to
%%% EVA.  Also, it implements the general mapping from EVA events and
%%% alarms to SNMP traps.
%%%
%%% Restart behaviour
%%% -----------------
%%% This handler may very well be locally or globally (failover)
%%% restarted without affecting any other part of the EVA
%%% functionality.  When it initialises itself, it rebuilds its
%%% internal state from the Mnesia tables.
%%%
%%% When performing a takeover, no special state transfer is needed.
%%% The handler rebuilds its state from the Mnesia tables, not from
%%% data within the old handler.
%%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
create_tables(Nodes) ->
    {atomic, ok} =
	mnesia:create_table([{name, eva_snmp_map},
			     {attributes, record_info(fields, eva_snmp_map)},
			     {ram_copies, Nodes}]),
    ok.
    
%% TMP DEBUG
start() ->
    gen_event:add_handler(alarm_handler, ?MODULE, []),
    %% Register the EVA defined events
    register_events([{clear_alarm, alarmCleared, snmpTrap, "standard trap",
		      {?MODULE, clear_alarm}}]).

%%-----------------------------------------------------------------
%% Func: register_events(Events) -> ok
%% Types: Events = [{Name, Trap, Treatment, Community, Func}]
%%        Name = Trap = atom()
%%        Func = fun(#event) -> {ok, SnmpVarbinds}
%%        Treatment = none | snmpTrap
%%        Community = string()
%% Purpose:
%%-----------------------------------------------------------------
register_events(Events) ->
    gen_event:call(alarm_handler, ?MODULE, {reg_events, Events}).

%%-----------------------------------------------------------------
%% Func: register_alarms(Alarms) -> ok
%% Types: Alarms = [{Name, Trap, Treatment, Community, Func}]
%%        Name = Trap = atom()
%%        Func = fun(#alarm) -> {ok, ObjOID, CauseOID, SnmpVarbinds}
%%        Treatment = none | snmpTrap
%%        Community = string()
%% Purpose: 
%%-----------------------------------------------------------------
register_alarms(Alarms) ->
    gen_event:call(alarm_handler, ?MODULE, {reg_alarms, Alarms}).


%%-----------------------------------------------------------------
%% Returns the eventIndex (as defined in the MIB) for the event
%% with name == Name.
%%-----------------------------------------------------------------
name2index(Name) ->
    case ets:lookup(eva_snmp_map, {event, Name}) of
	[{_, _, Data}] -> {ok, element(2, Data)};
	[] -> undefined
    end.

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_event
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% The eva_snmp_map is replicated as eventTable and alarmTable,
%% which means that if these tables exist and contains data, so does
%% eva_snmp_map.
%% eva_snmp_map:
%%  key = {event, name}     => val = {Func,Index,Trap,Treatment,Community,Owner}
%%  key = {traps, name}     => val = SentTraps
%%  key = {alarm, fault_id} => val = {ObjOid,CauseOid,Info,Ex1Oid,Ex2Oid}
%% The state of this handler is:
%%   - event_struct. An snmp_index that maps eventIndex to Name
%%   - alarm_struct. An snmp index that maps eventIndex to Name.
%%-----------------------------------------------------------------
init(_) ->
    case mnesia:force_load_table(eva_snmp_map) of
	yes ->
	    {EStruct, AStruct, NextEIndex} = init_snmp_structs(),
	    Priv = code:priv_dir(eva),
	    Mibs = [Priv ++ "/mibs/OTP-EVA-MIB"],
	    ok = snmp:load_mibs(snmp_master_agent, Mibs),
	    {ok, #state{event_struct = EStruct, alarm_struct = AStruct,
			mibs = Mibs, next_event_index = NextEIndex}};
	{error, R} ->
	    {stop, {mnesia_tables_not_available, [eva_snmp_map], R}}
    end.

init_snmp_structs() ->
    EStruct = snmp_index:new(integer),
    AStruct = snmp_index:new(integer),
    init_snmp_structs(mnesia:dirty_first(eva_snmp_map), EStruct, AStruct, 0).

init_snmp_structs({event, EName}, EStruct, AStruct, NextEIndex) ->
    [{_,_,Data}] = mnesia:dirty_read({eva_snmp_map, {event, EName}}),
    Index = element(2, Data),
    NAStruct = 
	case mnesia:dirty_read({alarmTable, EName}) of
	    [_Found] ->
		snmp_index:insert(AStruct, Index, EName);
	    _ -> AStruct
	end,
    NEStruct = snmp_index:insert(EStruct, Index, EName),
    init_snmp_structs(mnesia:dirty_next(eva_snmp_map, {event, EName}),
		      NEStruct, NAStruct, max(Index, NextEIndex));
init_snmp_structs('$end_of_table', EStruct, AStruct, NextEIndex) ->
    {EStruct, AStruct, NextEIndex + 1};
init_snmp_structs(Key, EStruct, AStruct, NextEIndex) ->
    init_snmp_structs(mnesia:dirty_next(eva_snmp_map, Key),
		      EStruct, AStruct, NextEIndex).


max(X,Y) when X > Y -> X;
max(X,Y) -> Y.

handle_event({send_event, Event}, S) ->
    case mnesia:dirty_read({eva_snmp_map, {event, Event#event.name}}) of
	[#eva_snmp_map{val = Data}] ->
	    send_event(Data, Event);
	_ ->
	    ok
    end,
    {ok, S};
handle_event({send_alarm, Alarm}, S) ->
    case mnesia:dirty_read({eva_snmp_map, {event, Alarm#alarm.name}}) of
	[#eva_snmp_map{val = Data}] ->
	    Time = date_and_time(Alarm#alarm.time),
	    send_alarm(Data, Alarm, Time),
	    {ok, S#state{last_time_sent = Time}};
	_ ->
	    {ok, S}
    end;
handle_event({unregister_event, Name}, S) ->
    #state{event_struct = EStruct} = S,
    case name2index(Name) of
	{ok, Index} ->
	    NEStruct = snmp_index:delete(EStruct, Index),
	    mnesia:dirty_delete({eva_snmp_map, {event, Name}}),
	    mnesia:dirty_delete({eva_snmp_map, {traps, Name}}),
	    {ok, S#state{event_struct = NEStruct}};
	_ ->
	    {ok, S}
    end;
handle_event({unregister_alarm, Name}, S) ->
    #state{event_struct = EStruct, alarm_struct = AStruct} = S,
    case name2index(Name) of
	{ok, Index} ->
	    NEStruct = snmp_index:delete(EStruct, Index),
	    NAStruct = snmp_index:delete(AStruct, Index),
	    mnesia:dirty_delete({eva_snmp_map, {event, Name}}),
	    mnesia:dirty_delete({eva_snmp_map, {traps, Name}}),
	    {ok, S#state{event_struct = NEStruct, alarm_struct = NAStruct}};
	_ ->
	    {ok, S}
    end;
handle_event(_, S) ->
    {ok, S}.

handle_call({get, eventTable, RowIndex, Cols}, S) ->
    #state{event_struct = EStruct} = S,
    {ok, get_event_cols(EStruct, RowIndex, Cols), S};

handle_call({get, alarmTable, RowIndex, Cols}, S) ->
    #state{alarm_struct = AStruct} = S,
    {ok, get_alarm_cols(AStruct, RowIndex, Cols), S};

handle_call({get_next, eventTable, RowIndex, Cols}, S) ->
    #state{event_struct = EStruct} = S,
    {ok, get_next_event_cols(EStruct, RowIndex, Cols), S};

handle_call({get_next, alarmTable, RowIndex, Cols}, S) ->
    #state{alarm_struct = AStruct} = S,
    {ok, get_next_alarm_cols(AStruct, RowIndex, Cols), S};

handle_call(get_last_time_sent, S) ->
    {ok, S#state.last_time_sent, S};

handle_call({set_last_time_sent, Time}, S) ->
    {ok, ok, S#state{last_time_sent = Time}};

handle_call({is_set_ok, eventTable, RowIndex, Cols}, S) ->
    #state{event_struct = EStruct}  = S,
    Reply = case snmp_index:get(EStruct, RowIndex) of
		{ok, _Found} ->
		    {noError, 0};
		_ ->
		    [{Col,_}|_] = Cols,
		    {inconsistentName, Col}
	    end,
    {ok, Reply, S};

handle_call({is_set_ok, alarmTable, RowIndex, [{?alarmSeverity, Sev}]}, S) ->
    #state{event_struct = EStruct} = S,
    Reply = case snmp_index:get(EStruct, RowIndex) of
		{ok, _Found} ->
		    case Sev of
			?'AlarmSeverity_clear' ->
			    {inconsistentValue, ?alarmSeverity};
			_ ->
			    {noError, 0}
		    end;
		_ ->
		    {inconsistentName, ?alarmSeverity}
	    end,
    {ok, Reply, S};

handle_call({set, eventTable, RowIndex, Cols}, S) ->
    #state{event_struct = EStruct} = S,
    {ok, {_, Name}} = snmp_index:get(EStruct, RowIndex),
    set_event(Name, Cols),
    {ok, {noError, 0}, S};


handle_call({set, alarmTable, RowIndex, Cols}, S) ->
    #state{alarm_struct = AStruct} = S,
    {ok, {_, Name}} = snmp_index:get(AStruct, RowIndex),
    set_alarm(Name, Cols),
    {ok, {noError, 0}, S};

handle_call({reg_events, Events}, S) ->
    #state{event_struct = EStruct, next_event_index = Index} = S,
    {NEStruct, NIndex} = reg_events(Events, EStruct, Index),
    {ok, ok, S#state{event_struct = NEStruct, next_event_index = NIndex}};

handle_call({reg_alarms, Alarms}, S) ->
    #state{event_struct = EStruct, next_event_index = Index,
	   alarm_struct = AStruct} = S,
    {NEStruct, NAStruct, NIndex} =
	reg_alarms(Alarms, EStruct, AStruct, Index), 
    {ok, ok, S#state{event_struct = NEStruct, next_event_index = NIndex,
		     alarm_struct = NAStruct}}.

handle_info(_, S) ->
    {ok, S}.

terminate(R, S) ->
    snmp:unload_mibs(snmp_master_agent, S#state.mibs).

%%%-----------------------------------------------------------------
%%% Instrumentation functions for OTP-EVA-MIB
%%%-----------------------------------------------------------------
eventTable(Op, RowIndex, Cols) ->
    table_func(Op, RowIndex, Cols, eventTable).

alarmTable(Op, RowIndex, Cols) ->
    table_func(Op, RowIndex, Cols, alarmTable).

table_func(_Op, _Table) ->
    ok.
table_func(Op, RowIndex, Cols, Table) ->
    gen_event:call(alarm_handler, ?MODULE, {Op, Table, RowIndex, Cols}).

numberOfCurAlarms(get) ->
    {ok, No} = eva:get_no_alarms(),
    {value, No};
numberOfCurAlarms(_Op) ->
    ok.

curAlarmLastTimeChanged(get) ->
    {value, gen_event:call(alarm_handler, ?MODULE, get_last_time_sent)};
curAlarmLastTimeChanged(_Op) ->
    ok.

%%-----------------------------------------------------------------
%% The curAlarmTable is ordered.
%% get and get-next executes within an transaction, because if an
%% alarm is cleared at the same time as it is read here, the
%% transaction ensures that the alarm exists during the entire read
%% operation.
%%-----------------------------------------------------------------
curAlarmTable(_Op) ->
    ok.
curAlarmTable(get, [FaultId], Cols) ->
    {atomic, Res} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read({alarm, FaultId}) of
		      [Alarm] ->
			  select_cur_alarm_cols(Cols, Alarm);
		      [] ->
			  {noValue, inconsistentName}
		  end
	  end),
    Res;
%% We should use mnesia:next, if the ordering was propely implemented in
%% Mnesia; for now, we use eva:alarm_first and eva:alarm_next.
curAlarmTable(get_next, RowIndex, Cols) ->
    {atomic, Res} = 
	mnesia:transaction(
	  fun() ->
		  case eva:alarm_next(idx(RowIndex)) of
		      {ok, NextIndex} ->
			  [Alarm] = mnesia:read({alarm, NextIndex}),
			  nselect_cur_alarm_cols(Cols, Alarm, [NextIndex]);
		      _ ->
			  case eva:alarm_first() of
			      {ok, FirstIndex} ->
				  [Alarm] = mnesia:read({alarm, FirstIndex}),
				  NewCols = add_one_to_cols(Cols),
				  nselect_cur_alarm_cols(NewCols, Alarm,
							 [FirstIndex]);
			      _ ->
				  end_of_table(Cols)
			  end
		  end
	  end),
    Res;

curAlarmTable(is_set_ok, [CurAlarmFaultId],
	      [{?currentAlarmSeverity, ?'AlarmSeverity_clear'}]) ->
    case mnesia:dirty_read({alarm, CurAlarmFaultId}) of
	[_Alarm] ->
	    {noError, 0};
	[] ->
	    {noCreation, ?currentAlarmSeverity}
    end;
curAlarmTable(is_set_ok, [CurAlarmFaultId],
	      [{?currentAlarmSeverity, Severity}]) ->
    {wrongValue, ?currentAlarmSeverity};


curAlarmTable(set, [CurAlarmFaultId],
	      [{?currentAlarmSeverity, ?'AlarmSeverity_clear'}]) ->
    case mnesia:dirty_read({alarm, CurAlarmFaultId}) of
	[#alarm{fault_id = FaultId}] ->
	    eva:clear_alarm(FaultId);
	[] ->
	    ok
    end,
    {noError, 0}.

idx([]) -> 0;
idx([H|_]) -> H.

%%-----------------------------------------------------------------
%% "backwards" instrumentation functions  event -> trap
%%-----------------------------------------------------------------
clear_alarm(#event{sender = CurAlarmFaultId, time = Time}) ->
    [#alarm{name = Name}] = mnesia:dirty_read({alarm, CurAlarmFaultId}),
    {ok, EventIndex} = name2index(Name),
    DateAndTime = date_and_time(Time),
    %% Update the last time sent variable.  We're executing in the
    %% alarm_handler, so we can't just call it, but have to spawn
    %% a new process.  rpc:cast does just that.
    rpc:cast(node(), gen_event, call,
	     [alarm_handler, ?MODULE, {set_last_time_sent, DateAndTime}]),
    {ok, [{currentAlarmEventIndex, [CurAlarmFaultId], EventIndex},
	  {eventTime, DateAndTime}]}.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
reg_events([{Name, Trap, Treatment, Community, Func} | T], EStruct, Index) ->
    case mnesia:dirty_read({eva_snmp_map, {event, Name}}) of
	[] ->
	    Val = {Func, Index, Trap, Treatment, Community, ""},
	    mnesia:dirty_write(#eva_snmp_map{key = {event, Name},
					     val = Val}),
	    mnesia:dirty_write(#eva_snmp_map{key = {traps, Name}, val = 0}),
	    NEStruct = snmp_index:insert(EStruct, Index, Name),
	    reg_events(T, NEStruct, Index + 1);
	_X -> % ignore if the event is alredy registered
	    reg_events(T, EStruct, Index)
    end;
reg_events([], EStruct, Index) ->
    {EStruct, Index}.

reg_alarms([{Name, Trap, Treatment, Community, Func} | T],
	   EStruct, AStruct, Index) ->
    case mnesia:dirty_read({eva_snmp_map, {event, Name}}) of
	[] ->
	    Val = {Func, Index, Trap, Treatment, Community, ""},
	    mnesia:dirty_write(#eva_snmp_map{key = {event, Name},
					     val = Val}),
	    mnesia:dirty_write(#eva_snmp_map{key = {traps, Name}, val = 0}),
	    NEStruct = snmp_index:insert(EStruct, Index, Name),
	    NAStruct = snmp_index:insert(AStruct, Index, Name),
	    reg_alarms(T, NEStruct, NAStruct, Index + 1);
	_ -> % ignore if the alarm is alredy registered
	    reg_alarms(T, EStruct, AStruct, Index)
    end;
reg_alarms([], EStruct, AStruct, Index) ->
    {EStruct, AStruct, Index}.

get_event_cols(EStruct, RowIndex, Cols) ->
    case snmp_index:get(EStruct, RowIndex) of
	{ok, {_, Name}}->
	    [{_, _, Row}] = mnesia:dirty_read({eva_snmp_map, {event, Name}}),
	    select_event_cols(Cols, Row, Name);
	_ ->
	    {noValue, inconsistentName}
    end.

get_next_event_cols(EStruct, RowIndex, Cols) ->
    case snmp_index:get_next(EStruct, RowIndex) of
	{ok, {NextOid, Name}} ->
	    [{_, _, Row}] = mnesia:dirty_read({eva_snmp_map, {event, Name}}),
	    nselect_event_cols(Cols, Row, NextOid, Name);
	_ ->
	    case snmp_index:get_next(EStruct, []) of
		{ok, {FirstOid, Name}} ->
		    [{_, _, Row}] =
			mnesia:dirty_read({eva_snmp_map, {event, Name}}),
		    NewCols = add_one_to_cols(Cols),
		    nselect_event_cols(NewCols, Row, FirstOid, Name);
		_ ->
		    end_of_table(Cols)
	    end
    end.

get_alarm_cols(AStruct, RowIndex, Cols) ->
    case snmp_index:get(AStruct, RowIndex) of
	{ok, {_, Name}} ->
	    [Alarm] = mnesia:dirty_read({alarmTable, Name}),
	    select_alarm_cols(Cols, Alarm);
	_ ->
	    {noValue, inconsistentName}
    end.

get_next_alarm_cols(AStruct, RowIndex, Cols) ->
    case snmp_index:get_next(AStruct, RowIndex) of
	{ok, {NextOid, Name}} ->
	    [Alarm] = mnesia:dirty_read({alarmTable, Name}),
	    nselect_alarm_cols(Cols, Alarm, NextOid);
	_ ->
	    case snmp_index:get_next(AStruct, []) of
		{ok, {FirstOid, Name}} ->
		    NewCols = add_one_to_cols(Cols),
		    [Alarm] = mnesia:dirty_read({alarmTable, Name}),
		    nselect_alarm_cols(NewCols, Alarm, FirstOid);
		_ ->
		    end_of_table(Cols)
	    end
    end.

%%----------------------------------------------------------------
%% Make a list of endOfTable with as many elems as Cols list.
%%----------------------------------------------------------------
end_of_table([Col | Cols]) ->
    [endOfTable | end_of_table(Cols)];
end_of_table([]) ->
    [].

add_one_to_cols([Col | Cols]) ->
    [Col + 1 | add_one_to_cols(Cols)];
add_one_to_cols([]) ->
    [].

%% Row = {Func,Index,Trap,Treatment,Community,Owner}
select_event_cols([?eventTrapName | Cols], Row, Name) ->
    Trap = atom_to_list(element(3, Row)),
    [{value, Trap} | select_event_cols(Cols, Row, Name)];
select_event_cols([?eventTreatment | Cols], Row, Name) ->
    T = get_treatment(element(4, Row), Name),
    [{value, T} | select_event_cols(Cols, Row, Name)];
select_event_cols([?eventCommunity | Cols], Row, Name) ->
    [{value, element(5, Row)} | select_event_cols(Cols, Row, Name)];
select_event_cols([?eventSentTraps | Cols], Row, Name) ->
    [{_, _, SentTraps}] = mnesia:dirty_read({eva_snmp_map, {traps, Name}}),
    [{value, SentTraps rem 4294967296} | select_event_cols(Cols, Row, Name)];
select_event_cols([?eventOwner | Cols], Row, Name) ->
    [{value, element(6, Row)} | select_event_cols(Cols, Row, Name)];
select_event_cols([], _Row, _Name) ->
    [].

%% Row = {Func,Index,Trap,Treatment,Community,Owner}
nselect_event_cols([N | Cols], Row, Oid, Name) when N =< ?eventTrapName ->
    Trap = atom_to_list(element(3, Row)),
    [{[?eventTrapName | Oid], Trap} | nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([?eventTreatment | Cols], Row, Oid, Name) ->
    T = get_treatment(element(4, Row), Name),
    [{[?eventTreatment | Oid], T} | nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([?eventCommunity | Cols], Row, Oid, Name) ->
    [{[?eventCommunity | Oid], element(5, Row)} |
     nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([?eventSentTraps | Cols], Row, Oid, Name) ->
    [{_, _, SentTraps}] = mnesia:dirty_read({eva_snmp_map, {traps, Name}}),
    [{[?eventSentTraps | Oid], SentTraps rem 4294967296} |
     nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([?eventOwner | Cols], Row, Oid, Name) ->
    [{[?eventOwner | Oid], element(6, Row)} |
     nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([N | Cols], Row, Oid, Name) ->
    [endOfTable | nselect_event_cols(Cols, Row, Oid, Name)];
nselect_event_cols([], _Row, _Oid, _Name) ->
    [].

%% Alarm = #alarmTable
select_alarm_cols([?alarmClass | Cols], Alarm) ->
    [{value, Alarm#alarmTable.class} | select_alarm_cols(Cols, Alarm)];
select_alarm_cols([?alarmSeverity | Cols], Alarm) ->
    [{value, Alarm#alarmTable.severity} | select_alarm_cols(Cols, Alarm)];
select_alarm_cols([], _Alarm) ->
    [].

%% Alarm = #alarmTable
nselect_alarm_cols([N | Cols], Alarm, Oid) when N =< ?alarmClass ->
    [{[?alarmClass | Oid], Alarm#alarmTable.class} |
     nselect_alarm_cols(Cols, Alarm, Oid)];
nselect_alarm_cols([?alarmSeverity | Cols], Alarm, Oid) ->
    [{[?alarmSeverity | Oid], Alarm#alarmTable.severity} |
     nselect_alarm_cols(Cols, Alarm, Oid)];
nselect_alarm_cols([N | Cols], Alarm, Oid) ->
    [endOfTable | nselect_alarm_cols(Cols, Alarm, Oid)];
nselect_alarm_cols([], _Alarm, _Oid) ->
    [].

%% Alarm = #alarm, Data = {ObjOid,CauseOid,Info,Ex1Oid,Ex2Oid}
select_cur_alarm_cols([?currentAlarmEventIndex | Cols], Alarm) ->
    {ok, Index} = name2index(Alarm#alarm.name),
    [{value, Index} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmObject | Cols], Alarm) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{value, element(1,Data)} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmCause | Cols], Alarm) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{value, element(2,Data)} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmSeverity | Cols], Alarm) ->
    [{value, Alarm#alarm.severity} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmTime | Cols], Alarm) ->
    Time = date_and_time(Alarm#alarm.time),
    [{value, Time} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmInformation | Cols], Alarm) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{value, element(3,Data)} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmExtra1 | Cols], Alarm) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{value, element(4,Data)} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([?currentAlarmExtra2 | Cols], Alarm) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{value, element(5,Data)} | select_cur_alarm_cols(Cols, Alarm)];
select_cur_alarm_cols([], _Alarm) ->
    [].

%% Alarm = #alarm, Data = {ObjOid,CauseOid,Info,Ex1Oid,Ex2Oid}
nselect_cur_alarm_cols([N | Cols], Alarm, Oid)
  when N =< ?currentAlarmEventIndex ->
    {ok, Index} = name2index(Alarm#alarm.name),
    [{[?currentAlarmEventIndex | Oid], Index} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmObject | Cols], Alarm, Oid) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{[?currentAlarmObject | Oid], element(1, Data)} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmCause | Cols], Alarm, Oid) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{[?currentAlarmCause | Oid], element(2, Data)} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmSeverity | Cols], Alarm, Oid) ->
    [{[?currentAlarmSeverity | Oid], Alarm#alarm.severity} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmTime | Cols], Alarm, Oid) ->
    Time = date_and_time(Alarm#alarm.time),
    [{[?currentAlarmTime | Oid], Time} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmInformation | Cols], Alarm, Oid) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{[?currentAlarmInformation | Oid], element(3, Data)} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmExtra1 | Cols], Alarm, Oid) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{[?currentAlarmExtra1 | Oid], element(4, Data)} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([?currentAlarmExtra2 | Cols], Alarm, Oid) ->
    Data = ets:lookup_element(eva_snmp_map, {alarm, Alarm#alarm.index}, 3),
    [{[?currentAlarmExtra2 | Oid], element(5, Data)} |
     nselect_cur_alarm_cols(Cols, Alarm, Oid)];
nselect_cur_alarm_cols([N | Cols], Row, Oid) ->
    [endOfTable | nselect_cur_alarm_cols(Cols, Row, Oid)];
nselect_cur_alarm_cols([], _Alarm, _Oid) ->
    [].

set_event(Name, Cols) ->
    [#eva_snmp_map{val = Row}] = mnesia:dirty_read({eva_snmp_map,{event,Name}}),
    NRow = make_event(Cols, Row, Name),
    mnesia:dirty_write(#eva_snmp_map{key = {event, Name}, val = NRow}).

set_alarm(Name, [{?alarmSeverity, Severity}]) ->
    NSev = make_severity(Severity),
    {atomic, _} =
	mnesia:transaction(fun() ->
				   [Alarm] = mnesia:read({alarmTable, Name}),
				   NAlarm = Alarm#alarmTable{severity = NSev},
				   mnesia:write(NAlarm)
			   end).

%% Row = {Func,Index,Trap,Treatment,Community,Owner}
make_event([{?eventTreatment, Treatment} | Cols], Row, Name) ->
    set_treatment(Name, Treatment),
    make_event(Cols, setelement(4, Row, make_treatment(Treatment)), Name);
make_event([{?eventCommunity, Community} | Cols], Row, Name) ->
    make_event(Cols, setelement(5, Row, Community), Name);
make_event([{?eventOwner, Owner} | Cols], Row, Name) ->
    make_event(Cols, setelement(6, Row, Owner), Name);
make_event([], Row, _Name) -> Row.

make_treatment(?eventTreatment_snmpTrap) -> snmpTrap;
make_treatment(?eventTreatment_logAndTrap) -> snmpTrap;
make_treatment(_) -> none.

make_severity(?'AlarmSeverity_indeterminate') -> indeterminate;
make_severity(?'AlarmSeverity_critical') -> critical;
make_severity(?'AlarmSeverity_major') -> major;
make_severity(?'AlarmSeverity_minor') -> minor;
make_severity(?'AlarmSeverity_warning') -> warning.

date_and_time(undefined) ->
    snmp:universal_time_to_date_and_time({{1970,1,1},{12,0,0}});
date_and_time(UTC) ->
    snmp:universal_time_to_date_and_time(UTC).

send_event({Func, _Index, Trap, snmpTrap, Community, _Owner}, Event) ->
    case catch Func(Event) of
	{ok, Varbinds} ->
	    Name = Event#event.name,
	    mnesia:dirty_update_counter({eva_snmp_map, {traps, Name}}, 1),
	    snmp:send_trap(snmp_master_agent, Trap, Community, Varbinds);
	Error ->
	    error_logger:format("~w: ~w returned ~p~n", [?MODULE, Func, Error])
    end;
send_event(_Data, _Event) ->
    ok.

send_alarm({Func, _Index, Trap, snmpTrap, Community, _Owner}, Alarm, Time) ->
    case catch Func(Alarm) of
	{ok, {ObjectOid, CauseOid, Varbinds}} ->
	    atrap(Alarm, Trap, Community, ObjectOid, CauseOid, "",
		  [0,0], [0,0], Varbinds, Time);
	{ok, {ObjectOid, CauseOid, Info, Extra1Oid, Extra2Oid, Varbinds}} ->
	    atrap(Alarm, Trap, Community, ObjectOid, CauseOid, Info,
		  Extra1Oid, Extra2Oid, Varbinds, Time);
	Error ->
	    error_logger:format("~w: ~w returned ~p. "
				"Trying to send empty trap~n",
				[?MODULE, Func, Error]),
	    atrap(Alarm, Trap, Community, [0,0], [0,0], "",[0,0],[0,0],[],Time)
    end;
send_alarm(_Data, _Alarm, Time) ->
    ok.
	    
atrap(Alarm,Trap,Community,ObjOid,CauseOid,Info,Ex1Oid,Ex2Oid,Varbinds,Time) ->
    % Add the mandatory columns
    RowIndex = [Alarm#alarm.index],
    NVarbinds = [{currentAlarmObject, RowIndex, ObjOid},
		 {currentAlarmSeverity, RowIndex, Alarm#alarm.severity},
		 {currentAlarmTime, RowIndex, Time} |
		 Varbinds],
    mnesia:dirty_update_counter({eva_snmp_map, {traps, Alarm#alarm.name}}, 1),
    Val = {ObjOid, CauseOid, Info, Ex1Oid, Ex2Oid},
    mnesia:dirty_write(#eva_snmp_map{key = {alarm, Alarm#alarm.index},
				     val = Val}),
    snmp:send_trap(snmp_master_agent, Trap, Community, NVarbinds).

get_treatment(SnmpTreatment, Name) ->
    {atomic, Res} =
	mnesia:transaction(fun() ->
				   [#eventTable{log = Log}] =
				       mnesia:read({eventTable, Name}),
				   gt(SnmpTreatment, Log)
			   end),
    Res.

gt(snmpTrap, true) -> ?eventTreatment_logAndTrap;
gt(snmpTrap, _)    -> ?eventTreatment_snmpTrap;
gt(_, true)        -> ?eventTreatment_log;
gt(_, _)           -> ?eventTreatment_none.

gt(?eventTreatment_logAndTrap) -> true;
gt(?eventTreatment_snmpTrap)   -> false;
gt(?eventTreatment_log)        -> true;
gt(_)                          -> false.
    
set_treatment(Name, Treatment) ->
    Log = gt(Treatment),
    mnesia:transaction(fun() ->
			       [E] = mnesia:read({eventTable, Name}),
			       mnesia:write(E#eventTable{log = Log})
		       end).

%%%-----------------------------------------------------------------
%%% Handler supervisor
%%%-----------------------------------------------------------------
start_link() ->
    proc_lib:start_link(?MODULE, init, [self(), self()]).

init(Caller, Parent) ->
    process_flag(trap_exit, true),
    install(),
    proc_lib:init_ack(Caller, {ok, self()}),
    loop(Parent).

loop(Parent) ->
    receive
	{'EXIT', Parent, Reason} ->
	    %% Parent orders shutdown
	    uninstall(),
	    exit(Reason);
	{gen_event_EXIT, eva_log, Reason} ->
	    exit(Reason);
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, [], []);
	_ ->
	    loop(Parent)
    end.

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, _, _) ->
    loop(Parent).
system_terminate(Reason, _Parent, _, _) ->
    uninstall(),
    exit(Reason).
system_code_change(State, _Module, OldVsn, Extra) ->
    {ok, State}.

install() ->
    ok = gen_event:add_sup_handler(alarm_handler, ?MODULE, []),
    %% Register the EVA defined events
    register_events([{clear_alarm, alarmCleared, snmpTrap, "standard trap",
		      {?MODULE, clear_alarm}}]).


uninstall() ->
    gen_event:delete_handler(alarm_handler, ?MODULE, []).
