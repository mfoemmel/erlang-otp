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
-module(eva_server).

-include("eva.hrl").

%% External exports
-export([create_tables/1, start/0, start_link/0, get_alarm_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {no_alarms = 0, next_alarm_id = 1}).

%%%----------------------------------------------------------------------
%%% General
%%% -------
%%% This module implements the server part of EVA. All events and
%%% alarms in the system are sent to this global server, and it
%%% maintains the eventTable, alarmTable and alarm (active alarm
%%% list), and forwards all events/alarms to the local alarm_handler,
%%% where protocol adaptations and subscribers must be installed.
%%% 
%%% Restart behaviour
%%% -----------------
%%% This server may very well be locally or globally (failover)
%%% restarted without affecting any other part of the EVA When it
%%% initialises itself, it rebuilds its internal state from the Mnesia
%%% tables. The active alarm list is not persistent, which means that
%%% if the table exists, this server has been alive previously,
%%% (either locally or at another node), and it may rebuild its state
%%% from the tables.
%%%
%%% When performing a takeover, no special state transfer is needed.
%%% The server rebuilds its state from the Mnesia tables, not from data
%%% within the old server.  However, if the old server crashes and
%%% restarts when the new server just registered itself, it must not
%%% perform a re_register!  Note: This is _not_ taken care of in this
%%% version; we need uwiger's new sysApp for this!  It's quite
%%% unlikely though...
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
create_tables(Nodes) ->
    %% All tables must be accessed within transactions, as both a manager
    %% and the application may read and modify the tables.
    %% One exception: if the validity of the data in a row is
    %% unimportant (but not that the row exists!), the eventTable and
    %% alarmTable may be accessed dirty.
    {atomic, ok} =
	mnesia:create_table([{name, eventTable},
			     {attributes, record_info(fields, eventTable)},
			     {disc_copies, Nodes}]),
    {atomic, ok} =
	mnesia:create_table([{name, alarmTable},
			     {attributes, record_info(fields, alarmTable)},
			     {disc_copies, Nodes}]),
    %% Current Alarm List
    %% This table should be ordered on the key.  The only way to do this
    %% today is to use the snmp flag to mnesia.
    {atomic, ok} =
	mnesia:create_table([{name, alarm},
			     {snmp, [{key, integer}]},  % (sort of) hack...
			     {attributes, record_info(fields, alarm)},
			     {index, [#alarm.fault_id]},
			     {ram_copies, Nodes}]),
    ok.

% tmp
start() ->
    gen_server:start({local, eva_server}, eva_server, [], []).

start_link() ->
    gen_server:start_link({local, eva_server}, eva_server, [], []).

get_alarm_status() ->
    {atomic, Res} = mnesia:transaction(fun() -> gas() end),
    Res.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init(_) ->
    case mnesia:force_load_table(alarm) of
	yes ->
	    %% We must check for takeover here; we can't just call re_register,
	    %% because we might have crashed and restarted during takeover to
	    %% other node.
	    global:re_register_name(eva_server, self()),

	    %% Register clear_alarm event
	    reg_event(clear_alarm, true),
	    {NoAlarms, NextId} = check_alarm_list(),
	    {ok, #state{no_alarms = NoAlarms, next_alarm_id = NextId}};
	{error, R} ->
	    {stop, {mnesia_tables_not_available, [alarm], R}}
    end.

handle_call({send_alarm, Data}, _From, S) ->
    {Reply, NewS} = do_send_alarm(Data, S),
    {reply, Reply, NewS};

handle_call({send_event, Data}, _From, S) ->
    Reply = do_send_event(Data),
    {reply, Reply, S};

handle_call({clear_alarm, Data}, _From, S) ->
    {Reply, NewS} = do_clear_alarm(Data, S),
    {reply, Reply, NewS};

handle_call(get_no_alarms, _From, S) ->
    {reply, {ok, S#state.no_alarms}, S};

handle_call({reg_event, Name, Log}, _From, S) ->
    {reply, reg_event(Name, Log), S};

handle_call({reg_alarm, Name, Log, Class, Severity}, _From, S) ->
    EventDef = #eventTable{name = Name, log = Log, generated = 0},
    AlarmDef = #alarmTable{name = Name, class = Class, severity = Severity},
    {atomic, Res} =
	mnesia:transaction(
	  fun() ->
		  case mnesia:read({eventTable, Name}) of
		      [] ->
			  mnesia:write(EventDef),
			  mnesia:write(AlarmDef),
			  true;
		      _ ->
			  false
		  end
	  end),
    gen_event:notify(alarm_handler, {register_alarm, Name}),
    {reply, Res, S};

handle_call({unreg_event, Name}, _From, S) ->
    mnesia:transaction(
      fun() ->
	      mnesia:delete({eventTable, Name})
      end),
    gen_event:notify(alarm_handler, {unregister_event, Name}),
    {reply, ok, S};

handle_call({unreg_alarm, Name}, _From, S) ->
    mnesia:transaction(
      fun() ->
	      mnesia:delete({eventTable, Name}),
	      mnesia:delete({alarmTable, Name})
      end),
    gen_event:notify(alarm_handler, {unregister_alarm, Name}),
    {reply, ok, S}.


handle_cast({send_alarm, Data}, S) ->
    {_Reply, NewS} = do_send_alarm(Data, S),
    {noreply, NewS};

handle_cast({send_event, Data}, S) ->
    do_send_event(Data),
    {noreply, S};

handle_cast({clear_alarm, Data}, S) ->
    {_Reply, NewS} = do_clear_alarm(Data, S),
    {noreply, NewS}.


handle_info(Info, S) ->
    {noreply, S}.


terminate(Reason, S) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
check_alarm_list() ->
    check_alarm_list(eva:alarm_first(), 0, 0).

check_alarm_list({ok, Index}, Cnt, _) ->
    check_alarm_list(eva:alarm_next(Index), Cnt + 1, Index);
check_alarm_list(_, Cnt, Index) ->
    {Cnt, Index + 1}.

reg_event(Name, Log) ->
    EventDef = #eventTable{name = Name, log = Log, generated = 0},
    {atomic, Res} = 
	mnesia:transaction(
	  fun() ->
		  case mnesia:read({eventTable, Name}) of
		      [] ->
			  mnesia:write(EventDef),
			  true;
		      _ ->
			  false
		  end
	  end),
    gen_event:notify(alarm_handler, {register_event, Name}),
    Res.

%%-----------------------------------------------------------------
%% Send Alarm:
%% 1) Update eventTable.generated counter
%% 2) Check if there is an alarm with the same fault_id, if not,
%%    it's the first time this fault gives rise to an alarm -
%%    allocate index to the fault_id, and increment next index counter.
%% 3) Construct an #alarm
%% 4) Insert #alarm into curAlarmTable
%% 5) Send #alarm to handler
%%-----------------------------------------------------------------
do_send_alarm({Name, FaultId, Sender, Cause, Extra}, S) ->
    Index = S#state.next_alarm_id,
    NoAlarms = S#state.no_alarms,
    case mnesia:transaction(
	   fun() ->
		   AIndex = fault2index(FaultId, Index),
		   case mnesia:read({alarmTable, Name}) of
		       [#alarmTable{severity = Severity}] ->
			   Time = calendar:universal_time(),
			   Alarm = #alarm{index = AIndex, severity = Severity,
					  time = Time, fault_id = FaultId,
					  name = Name, cause = Cause,
					  sender = Sender, extra = Extra},
			   [Event] = mnesia:read({eventTable, Name}),
			   Gen = Event#eventTable.generated,
			   NEvent = Event#eventTable{generated = Gen + 1},
			   mnesia:write(Alarm),
			   mnesia:write(NEvent),			   
			   {ok, Alarm};
		       _ ->
			   {no_such_alarm, Name}
		   end
	   end) of
	{atomic, {ok, Alarm}} ->
	    gen_event:notify(alarm_handler, {send_alarm, Alarm}),
	    if
		Alarm#alarm.index == Index ->
		    {ok, S#state{next_alarm_id = Index + 1,
				 no_alarms = NoAlarms + 1}};
		true ->
		    {ok, S}
	    end;
	{atomic, Error} ->
	    {{error, Error}, S};
	{aborted, Reason} ->
	    {{error, {aborted, Name, Reason}}, S}
    end.

do_clear_alarm(FaultId, S) ->
    NoAlarms = S#state.no_alarms,
    % We must do this dirty, as we want to send the event before
    % the alarm is deleted.  But that's ok, as the indexes are not
    % reused.  We don't want to delete the alarm until all adaptations
    % have taken care of it, so we use gen_event:sync_notify.
    case mnesia:dirty_index_read(alarm, FaultId, #alarm.fault_id) of
	[#alarm{index = Index}] ->
	    case make_event(clear_alarm, Index, []) of
		{ok, Event} ->
		    gen_event:sync_notify(alarm_handler, {send_event, Event}),
		    mnesia:transaction(fun() ->
					       mnesia:delete({alarm, Index})
				       end);
		_ ->
		    ok
	    end,
	    {ok, S#state{no_alarms = NoAlarms - 1}};
	[] ->
	    {{error, {no_such_active_alarm, FaultId}}, S}
    end.

%%-----------------------------------------------------------------
%% Send Event:
%% 1) Update eventTable.generated counter
%% 2) Construct an #event
%% 3) Send #event to handler
%%-----------------------------------------------------------------
do_send_event({Name, Sender, Extra}) ->
    case make_event(Name, Sender, Extra) of
	{ok, Event} ->
	    gen_event:notify(alarm_handler, {send_event, Event}),
	    ok;
	Else ->
	    Else
    end.

make_event(Name, Sender, Extra) ->
    case mnesia:transaction(
	   fun() ->
		   case mnesia:read({eventTable, Name}) of
		       [Event] ->
			   Time = calendar:universal_time(),
			   Gen = Event#eventTable.generated,
			   NEvent = Event#eventTable{generated = Gen + 1},
			   mnesia:write(NEvent),
			   #event{name = Name, sender = Sender,
				  time = Time, extra=Extra};
		       _ ->
			   no_such_event
		   end
	   end) of
	{atomic, no_such_event} ->
	    {error, {no_such_event, Name}};
	{atomic, Event} ->
	    {ok, Event};
	{aborted, Reason} ->
	    {error, {aborted, Name, Reason}}
    end.

fault2index(FaultId, NextIndex) ->
    case mnesia:dirty_index_read(alarm, FaultId, #alarm.fault_id) of
	[Alarm] -> Alarm#alarm.index;
	_ -> NextIndex
    end.
	     
%% Get alarm status (executes within a transaction)
gas() ->
    A = ?wild_alarm,
    C = length(mnesia:match_object(A#alarm{severity = critical})),
    Ma = length(mnesia:match_object(A#alarm{severity = major})),
    Mi = length(mnesia:match_object(A#alarm{severity = minor})),
    W = length(mnesia:match_object(A#alarm{severity = warning})),
    I = length(mnesia:match_object(A#alarm{severity = indeterminate})),
    {C, Ma, Mi, W, I}.
