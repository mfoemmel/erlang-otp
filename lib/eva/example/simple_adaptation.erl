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
%S1
-module(simple_adaptation).

-behaviour(gen_event).

-include_lib("eva/include/eva.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").

%%%-----------------------------------------------------------------
%%% Simple EVA adaptation that formats events and alarms to standard
%%% out.
%%%-----------------------------------------------------------------

%% External exports
-export([start/0, print_alarms/0]).

%% Internal exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
start() ->
    gen_event:add_handler(alarm_handler, ?MODULE, []).

print_alarms() ->
    gen_event:call(alarm_handler, ?MODULE, print_alarms).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_event
%%%-----------------------------------------------------------------
init(_) ->
    io:format("Initializing simple EVA adaptation...~n"),
    {ok, []}.

handle_event({send_event, #event{name = Name}}, S) ->
    X = Name, % due to bug in mnemosyne...    
    Handle = query [E.generated || E <- table(eventTable),
				   E.name = Name] end,
    {atomic, [Generated]} =
       mnesia:transaction(fun() -> mnemosyne:eval(Handle) end),

    io:format("** Event: ~w, ~w generated~n", [Name, Generated]),
    {ok, S};

handle_event({send_alarm, #alarm{name = Name, severity = Severity}}, S) ->
    X = Name, % due to bug in mnemosyne...    
    Handle = query [E.generated || E <- table(eventTable),
				   E.name = Name] end,
    {atomic, [Generated]} = 
       mnesia:transaction(fun() -> mnemosyne:eval(Handle) end),

    Handle2 = query [A.class || A <- table(alarmTable),
				A.name = Name] end,
    {atomic, [Class]} = 
       mnesia:transaction(fun() -> mnemosyne:eval(Handle2) end),

    io:format("** ~w alarm ~w of class ~w, ~w generated~n",
	      [Severity, Name, Class, Generated]),
    {ok, S};

handle_event(_, S) ->
    {ok, S}.

handle_call(print_alarms, S) ->
    Handle = query [{A.name, A.sender, A.cause, A.severity, AlarmDef.class} ||
			A <- table(alarm),
			AlarmDef <- table(alarmTable),
			A.name = AlarmDef.name] end,
    {atomic, Alarms} = mnesia:transaction(fun() -> mnemosyne:eval(Handle) end),
    io:format("** Active alarm list~n"),		    
    lists:foreach(
      fun({Name, Sender, Cause, Severity, Class}) ->
	      io:format("~14w ~10w alarm ~p from ~p, probable cause: ~p~n",
			[Severity, Class, Name, Sender, Cause])
      end, Alarms),
    {ok, ok, S}.

handle_info(_, S) ->
    {ok, S}.

terminate(R, S) ->
    io:format("Terminating simple EVA adaptation...~n"),
    ok.
%S1
