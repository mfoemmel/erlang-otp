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
%%-----------------------------------------------------------------
%% Record: alarm
%% Types: index = integer()     : an index that makes the table ordered
%%        fault_id = fault_id() : from eva:get_fault_id()
%%        name = atom()         : the unique name of the alarm
%%        sender = term()       : the object that generated the alarm
%%        cause = term()        : the cause of the alarm
%%        severity = indeterminate | critical | major | minor |
%%                    warning   : the perceived severity of the alarm
%%        time = {{Y,Mo,D},{H,Mi,S}} : local time
%%        extra = term()        : any extra information about the alarm
%% Purpose: This record is sent to all adaptations and subscribers
%%          when an alarm is sent.  It is also use in the Mnesia alarm
%%          table, that implements the current alarm list.
%%-----------------------------------------------------------------
-record(alarm, {index, fault_id, name, sender, cause, severity, time, extra}).
-define(wild_alarm, {alarm, '_', '_', '_', '_', '_', '_', '_', '_'}).

%%-----------------------------------------------------------------
%% Record: event
%% Types: name = atom()         : the unique name of the event
%%        sender = term()       : the object that generated the event
%%        time = {{Y,Mo,D},{H,Mi,S}} : local time
%%        extra = term()        : any extra information about the event
%% Purpose: This record is sent to all adaptations and subscribers
%%          when an event is sent.
%%-----------------------------------------------------------------
-record(event, {name, sender, time, extra}).

%%-----------------------------------------------------------------
%% Record: eventTable
%% Types: name = atom()         : the unique name of the event
%%        log = boolean()       : should the event be logged, when
%%                                it's generated
%%        generated = integer() : counter
%% Purpose: Used as definition for the Mnesia table eventTable.
%%          All events in the system are defined in this table.
%%-----------------------------------------------------------------
-record(eventTable, {name, log, generated}).

%%-----------------------------------------------------------------
%% Record: alarmTable
%% Types: name = atom()         : the unique name of the alarm
%%        class = unknown | communications | qos | processing |
%%                equipment | environmental : the type of the alarm
%%        severity = indeterminate | critical | major | minor |
%%                    warning   : the perceived severity of the alarm
%% Purpose: Used as definition for the Mnesia table alarmTable.
%%          All alarms in the system are defined in this table,
%%          and in eventTable.
%%-----------------------------------------------------------------
-record(alarmTable, {name, class, severity}).
