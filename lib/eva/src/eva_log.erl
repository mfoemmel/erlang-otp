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
-module(eva_log).

-behaviour(gen_event).

-include("eva.hrl").
-include("eva_log.hrl").

-record(state, {default_log, logs = []}).

%% External exports
-export([start_link/0, start_link/1]).
-export([open/3, set_filter/2, close/1, get_logs/0, eva_log_search/4]).

%% Internal exports
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2]).
-export([init/3, system_continue/3, system_terminate/4, system_code_change/4]).

%%%-----------------------------------------------------------------
%%% General
%%% -------
%%% This module implements log functionality for events defined in
%%% eva.
%%%
%%% Restart behaviour
%%% -----------------
%%% This handler has no persistent state, i.e.  the internal state is
%%% lost during failover in a distributed system, or when the
%%% process/subsystem restarts.  The state is built from calls to
%%% open/2, set_*/2 and close/1, and it is up to the caller of these
%%% functions (i.e. the owner of the logs) to call them again after
%%% failover, takeover or restart.  The reason for this is that
%%% failover behavoiur must be configurable.  Some logs may be
%%% replicated on several nodes, so log info is not lost during
%%% failover.  Other logs are not replicated, and log items are copied
%%% on takeover, and for other logs, the log items are deleted on
%%% takeover.  This is entirely up to the owner of the log.  This
%%% means that log-owners may want to monitor the behaviour of this
%%% handler, either directly by linking to its handler-supervisor
%%% process, or indirectly by making sure that the supervisor restarts
%%% the owner if this process terminates.
%%%
%%% When performing a takeover, no special state transfer is needed,
%%% as the handler has no 'real' state.
%%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
%% DefaultLog == {Name, WrapT} | false

start_link() ->
    start_link(false).
start_link(false) ->
    proc_lib:start_link(?MODULE, init, [self(), self(), false]);
start_link({Name, WrapT}) when list(Name), integer(WrapT) ->
    DefaultLog = {Name, WrapT},
    proc_lib:start_link(?MODULE, init, [self(), self(), DefaultLog]);
start_link(Name) when list(Name) ->
    %% For backwards compatibility only
    start_link({Name, 24*3600}).

%% Name = string()

open(Name, {M,F,A}, WrapTime) when list(Name) ->
    case gen_event:call(alarm_handler, ?MODULE, {open, Name, {M,F,A}}) of
	ok ->
	    log:open(Name, eva_log, WrapTime);
	Error ->
	    Error
    end.

set_filter(Name, {M,F,A}) ->
    gen_event:call(alarm_handler, ?MODULE, {set_filter, Name, {M,F,A}}).
    
close(Name) ->
    gen_event:call(alarm_handler, ?MODULE, {close, Name}),
    log:close(Name).

get_logs() ->
    gen_event:call(alarm_handler, ?MODULE, get_logs).    

%%-----------------------------------------------------------------
%% Search function that can be called from log:transfer.
%%-----------------------------------------------------------------
eva_log_search(Cont, Name, Start, Stop) ->
    case disk_log:chunk(Name, Cont) of
	eof ->
	    eof;
	{error, R} ->
	    {error, R};
	{NCont, ListOfTerms} ->
	    List = lists:map(fun(Term) ->
				     case is_date_in_range(Term, Start, Stop) of
					 true ->
					     io_lib:format("~999999p~n",[Term]);
					 false ->
					     []
				     end
			     end, ListOfTerms),
	    Bin = list_to_binary(List),
	    {NCont, Bin}
    end.

is_date_in_range(#event{time = Time}, Start, Stop) 
  when Start =< Time, Time =< Stop -> true;
is_date_in_range(#alarm{time = Time}, Start, Stop)
  when Start =< Time, Time =< Stop -> true;
is_date_in_range(_Term, _Start, _Stop) ->
    false.


%%%-----------------------------------------------------------------
%%% Call-back functions from gen_event
%%%-----------------------------------------------------------------
init(DefaultLog) ->
    case DefaultLog of
	{Name, DefLogWrapT} when list(Name) ->
	    case log:open(Name, eva_log, DefLogWrapT) of
		ok ->
		    DL = #eva_log{name = Name},
		    {ok, #state{default_log = DL}};
		{error, Reason} ->
		    {badarg, {log, open, Reason}}
	    end;
	_ ->
	    {ok, #state{}}
    end.

handle_event({send_event, Event}, S) ->
    log_event(Event#event.name, Event, S),
    {ok, S};
handle_event({send_alarm, Alarm}, S) ->
    log_event(Alarm#alarm.name, Alarm, S),
    {ok, S};
handle_event(_, S) ->
    {ok, S}.

handle_call(get_logs, S) ->
    case S#state.default_log of
	undefined ->
	    {ok, S#state.logs, S};
	Def ->
	    {ok, [Def | S#state.logs], S}
    end;

handle_call({open, Name, Filter}, S) ->
    case disk_log_open(Name) of
	{ok, Name} ->
	    Log = #eva_log{name = Name, filter = Filter},
	    {ok, ok, S#state{logs = [Log | S#state.logs]}};
	Error ->
	    {ok, Error, S}
    end;

handle_call({set_filter, Name, Filter}, S) ->
    case lists:keysearch(Name, #eva_log.name, S#state.logs) of
	{value, Log} ->
	    {ok, ok, update(S, Log#eva_log{filter = Filter})};
	_ ->
	    {ok, {error, {no_such_log, Name}}, S}
    end;

handle_call({close, Name}, S) ->
    disk_log:close(Name),
    {ok, ok, S#state{logs = lists:keydelete(Name, #eva_log.name,S#state.logs)}}.

handle_info(_, S) ->
    {ok, S}.

terminate(R, S) ->
    Logs = case S#state.default_log of
	       undefined -> S#state.logs;
	       Def -> [Def | S#state.logs]
	   end,
    lists:foreach(fun(Log) -> disk_log:close(Log) end, Logs),
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
log_event(Name, Event, S) ->
    #state{logs = Logs, default_log = Def} = S,
    DoLog = case mnesia:dirty_read({eventTable, Name}) of
		[#eventTable{log = true}] -> true;
		_ -> false
	    end,
    case DoLog of
	true ->
	    Res = lists:foldl(
		    fun(Log, IsLogged) ->
			    #eva_log{name = LName, filter = {M,F,A}} = Log,
			    case catch apply(M, F, [Event | A]) of
				true ->
				    disk_log:log(LName, Event);
				_ ->
				    IsLogged
			    end
		    end, false, Logs),
	    case Res of
		ok -> 
		    ok;
		_ ->
		    case Def of
			#eva_log{name = LName} ->
			    disk_log:log(LName, Event);
			_ ->
			    ok
		    end
	    end;
	false ->
	    ok
    end.

update(S, Log) ->
    Logs = lists:keyreplace(Log#eva_log.name, #eva_log.name, S#state.logs, Log),
    S#state{logs = Logs}.

disk_log_open(Name) ->
    case disk_log:info(Name) of
	L when list(L) ->
	    {value, {_, File}} = lists:keysearch(file, 1, L),
	    disk_log:open([{name, Name}, {notify, true}, {file, File}]);
	Error ->
	    Error
    end.

%%%-----------------------------------------------------------------
%%% Handler supervisor
%%%-----------------------------------------------------------------
init(Caller, Parent, DefaultLog) ->
    process_flag(trap_exit, true),
    install(DefaultLog),
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

install(DefaultLog) ->
    ok = gen_event:add_sup_handler(alarm_handler, ?MODULE, DefaultLog).

uninstall() ->
    gen_event:delete_handler(alarm_handler, ?MODULE, []).
