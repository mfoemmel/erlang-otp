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
-module(eva).

-include("eva.hrl").
%%-include_lib("mnemosyne/include/mnemosyne.hrl").

-export([register_event/2, register_alarm/4,
	 unregister_event/1, unregister_alarm/1,
	 asend_event/3, send_event/3, send_event/4,
	 get_fault_id/0, asend_alarm/5, send_alarm/5, send_alarm/6,
	 aclear_alarm/1, clear_alarm/1, clear_alarm/2,
	 get_no_alarms/0, get_alarms/1, get_alarm_status/0]).
-export([alarm_first/0, alarm_next/1]).

%% Timeout for calls to the globally registered eva_server.
-define(TIME, 10000).

register_event(Name, Log) ->
    gen_server:call({global, eva_server}, {reg_event, Name, Log}, ?TIME).

register_alarm(Name, Log, Class, Severity) ->
    gen_server:call({global, eva_server},
		    {reg_alarm, Name, Log, Class, Severity}, ?TIME).

unregister_event(Name) ->
    gen_server:call({global, eva_server}, {unreg_event, Name}, ?TIME).

unregister_alarm(Name) ->
    gen_server:call({global, eva_server}, {unreg_alarm, Name}, ?TIME).


asend_event(Name, Sender, Extra) ->
    gen_server:cast({global, eva_server}, {send_event, {Name, Sender, Extra}}).
send_event(Name, Sender, Extra) ->
    gen_server:call({global, eva_server},
		    {send_event, {Name, Sender, Extra}}, ?TIME).
send_event(Name, Sender, Extra, Time) ->
    gen_server:call({global, eva_server},
		    {send_event, {Name, Sender, Extra}}, Time).

get_fault_id() -> {node(), now()}.

asend_alarm(Name, AlarmFaultId, Sender, Cause, Extra) ->
    Msg = {send_alarm, {Name, AlarmFaultId, Sender, Cause, Extra}},
    gen_server:cast({global, eva_server}, Msg).
send_alarm(Name, AlarmFaultId, Sender, Cause, Extra) ->
    Msg = {send_alarm, {Name, AlarmFaultId, Sender, Cause, Extra}},
    gen_server:call({global, eva_server}, Msg, ?TIME).
send_alarm(Name, AlarmFaultId, Sender, Cause, Extra, Time) ->
    Msg = {send_alarm, {Name, AlarmFaultId, Sender, Cause, Extra}},
    gen_server:call({global, eva_server}, Msg, Time).

aclear_alarm(AlarmFaultId) ->
    gen_server:cast({global, eva_server}, {clear_alarm, AlarmFaultId}).
clear_alarm(AlarmFaultId) ->
    gen_server:call({global, eva_server}, {clear_alarm, AlarmFaultId}, ?TIME).
clear_alarm(AlarmFaultId, Time) ->
    gen_server:call({global, eva_server}, {clear_alarm, AlarmFaultId}, Time).

get_no_alarms() ->
    gen_server:call({global, eva_server}, get_no_alarms).

get_alarms({name, Name}) ->
    mnesia:dirty_match_object(?wild_alarm#alarm{name = Name});
%% This is how I would write if it wasn't for bug in mnemosyne
%    Handle = query [Alarm || Alarm <- table(alarm),
%			     Alarm.name = Name] end,
%    mnesia:ets(fun() -> mnemosyne:eval(Handle) end);
		       
get_alarms({sender, Sender}) ->
    mnesia:dirty_match_object(?wild_alarm#alarm{sender = Sender}).
%% This is how I would write if it wasn't for bug in mnemosyne
%    Handle = query [Alarm || Alarm <- table(alarm),
%			     Alarm.sender = Sender] end,
%    mnesia:ets(fun() -> mnemosyne:eval(Handle) end).

get_alarm_status() ->
    {Critical, Major, Minor, Warning, Indeterminate} =
	eva_server:get_alarm_status(),
    [{critical, to_boolean(Critical)},
     {major, to_boolean(Major)},
     {minor, to_boolean(Minor)},
     {warning, to_boolean(Warning)},
     {indeterminate, to_boolean(Indeterminate)}].

to_boolean(0) -> false;
to_boolean(Cnt) -> true.

%% These should be replaced by calls to appropriate Mnesia functions,
%% when the order is correctly implemented in Mnesia.
alarm_first() ->
    case mnesia:snmp_get_next_index(alarm, []) of
	{ok, [FirstIndex]} -> {ok, FirstIndex};
	_ -> '$end_of_table'
    end.

alarm_next(Index) ->
    case mnesia:snmp_get_next_index(alarm, [Index]) of
	{ok, [NextIndex]} -> {ok, NextIndex};
	_ -> '$end_of_table'
    end.
	    
