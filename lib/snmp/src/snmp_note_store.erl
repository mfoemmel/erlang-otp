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
-module(snmp_note_store).

-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").

%% External exports
-export([start_link/1, start_link/2, get_note/1, set_note/3, verbosity/1]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([timer/2]).

-define(timeout, 30000).  % Perform gc twice in a minute.

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-record(state, {notes, timer = start_timer(), timeout = false}).

%%%-----------------------------------------------------------------
%%% Implements a database for notes with a lifetime. Once in a
%%% while, the database will be gc:ed, to get rid of old notes.
%%% This database will not contain much data.
%%% Options is a list of Option, where Option is
%%%   {verbosity, silence|log|debug|trace} % undocumented feature
%%%-----------------------------------------------------------------
start_link(Prio) ->
    start_link(Prio,[]).

start_link(Prio,Options) ->
    gen_server:start_link({local, snmp_note_store}, snmp_note_store, [Prio,Options],[]).

%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------
get_note(Key) ->
    gen_server:call(snmp_note_store, {get_note, Key}, infinity).
%% Lifetime is in 1/10 sec.
set_note(Lifetime, Key, Value) ->
    gen_server:call(snmp_note_store, {set_note, Lifetime, Key, Value},
		    infinity).

verbosity(Verbosity) -> 
    gen_server:cast(snmp_note_store,{verbosity,Verbosity}).

init([Prio,Options]) ->
    process_flag(trap_exit, true),
    process_flag(priority, Prio),
    put(sname,ns),
    put(verbosity,get_verbosity(Options)),        
    ?vlog("starting",[]),
    Notes = ets:new(snmp_note_store, [set, protected]), % *never* use private!
    ?vdebug("started",[]),
    {ok, #state{notes = Notes}}.

%%-----------------------------------------------------------------
%% A note is any internal information that has to be
%% stored for some time (the Lifetime).
%% A note is stored in ets as {Key, {BestBefore, Value}},
%% where BestBefore is currentTime + Lifetime. 
%% A GC-op can destroy any notes with CurTime > BestBore.
%% Lifetime is in centiseconds or infinity, in which case
%% the note is eternal.
%%-----------------------------------------------------------------
handle_call({set_note, Lifetime, Key, Value}, _From, State) 
  when integer(Lifetime) ->
    ?vlog("set note ~p with life time ~p",[{Key,Value},Lifetime]),
    RealUpTime = snmp_misc:now(cs) - snmp:system_start_time(),
    BestBefore = RealUpTime + Lifetime,
    Val = ets:insert(State#state.notes, {Key, {BestBefore, Value}}),
    NState = activate_timer(State),
    {reply, Val, NState};

handle_call({set_note, infinity, Key, Value}, _From, State) ->
    ?vlog("set note ~p",[{Key,Value}]),
    Val = ets:insert(State#state.notes, {Key, {infinity, Value}}),
    ?vdebug("set note; old value: ~p",[Val]),
    {reply, Val, State};

handle_call({get_note, Key}, _From, State) ->
    ?vlog("get note ~p",[Key]),
    Val = handle_get_note(Key, State#state.notes),
    ?vdebug("get note: ~p",[Val]),
    {reply, Val, State};

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, ok, State};

handle_call(Req, From, State) ->
    ?vlog("~n    RECEIVED UNEXPECTED REQUEST: ~p from ~p",[Req,From]),
    {reply, {error,{unknown_request,Req}}, State}.

handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, State) ->
    ?vlog("~n    RECEIVED UNEXPECTED MESSAGE: ~p",[Msg]),
    {noreply, State}.
    
%%-----------------------------------------------------------------
%% If there are no possible garbage left, we don't
%% have to wait for timeout, and perform another
%% gc, because we won't do anything. So
%% we switch the timeout off in that case.
%% It will be switched on as soon as we get some
%% other message.
%%-----------------------------------------------------------------
handle_info(timeout, State) ->
    ?vdebug("timeout",[]),
    case gc(State#state.notes) of
	nothing_left ->
	    NState = deactivate_timer(State),
	    {noreply, NState};
	work_to_do ->
	    NState = activate_timer(State),
	    {noreply, NState}
    end;

handle_info({'EXIT',Pid,Reason}, State) ->
    ?vinfo("~n    Received exit message from ~p for reason ~p",[Pid,Reason]),
    case State#state.timer of
	Pid ->
	    set_state(State#state{timer = start_timer()});
	_ ->
	    {noreply, State}
    end;

handle_info(Event, State) ->
    ?vlog("~n    RECEIVED UNEXPECTED EVENT: ~p",[Event]),
    {noreply, State}.


set_state(S) ->
    case gc(S#state.notes) of
	nothing_left ->
	    NState = deactivate_timer(S),
	    {noreply, NState};
	work_to_do ->
	    NState = activate_timer(S),
	    {noreply, NState}
    end.

terminate(Reason, _State) ->
    ?vdebug("terminate: ~p",[Reason]),
    ok.

%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

% downgrade
code_change({down, _Vsn}, State, _Extra) ->
%     ?debug("code_change(down) -> entry with~n"
% 	   "  Vsn:   ~p~n"
% 	   "  State: ~p~n"
% 	   "  Extra: ~p",
% 	   [Vsn,State,Extra]),
    NState = activate_timer(deactivate_timer(State)),
%     ?debug("code_change(down) -> ~n"
% 	   "  NState: ~p",[NState]),
    {ok, NState};

% upgrade
code_change(_Vsn, State, _Extra) ->
    process_flag(trap_exit, true),
%     ?debug("code_change(up) -> entry with~n"
% 	   "  Vsn:   ~p~n"
% 	   "  State: ~p~n"
% 	   "  Extra: ~p",
% 	   [Vsn,State,Extra]),
    NState = restart_timer(State),
%     ?debug("code_change(up) -> ~n"
% 	   "  NState: ~p",[NState]),
    {ok, NState}.


%%----------------------------------------------------------
%% Timer
%%----------------------------------------------------------
activate_timer(State) ->
    case State#state.timeout of
	false -> 
	    State#state.timer ! activate,
	    receive
		activated -> ok
	    end;
	true ->
	    ok
    end,
    State#state{timeout = true}.

deactivate_timer(State) ->
    case State#state.timeout of
	true ->
	    State#state.timer ! deactivate,
	    receive
		deactivated -> ok
	    end;
	false ->
	    ok
    end,
    State#state{timeout = false}.

start_timer() ->
    spawn_link(?MODULE, timer, [self(), deactive]).

%% Kill, restart and activate timer.
restart_timer(State) ->
    TPid = State#state.timer,
    ?debug("restart_timer -> kill current timer process ~p",[TPid]),
    exit(TPid, kill),
    ?debug("restart_timer -> await acknowledgement",[]),
    receive
	{'EXIT', TPid, _Reason} ->
	    ok
    end,
    ?debug("restart_timer -> start a new timer process",[]),
    activate_timer(State#state{timer = start_timer()}).

timer(Pid, deactive) ->
    receive
	activate ->
	    ?debug("timer(deactive) -> activate request, send ack",[]),
	    Pid ! activated,
	    ?debug("timer(deactive) -> activate",[]),
	    ?MODULE:timer(Pid, active)		% code replacement
    after
	?timeout ->
	    ?debug("timer(deactive) -> timeout",[]),
	    ?MODULE:timer(Pid, deactive)
    end;
timer(Pid, active) ->
    receive
	deactivate ->
	    ?debug("timer(active) -> deactivate request, send ack",[]),
	    Pid ! deactivated,
	    ?debug("timer(active) -> deactivate",[]),
	    ?MODULE:timer(Pid, deactive)
    after
	?timeout ->
	    ?debug("timer(active) -> timeout",[]),
	    Pid ! timeout,
	    ?MODULE:timer(Pid, active)
    end.
    

handle_get_note(Key, Notes) ->
    case ets:lookup(Notes, Key) of
	[{Key, {infinity, Val}}] ->
	    Val;
	[{Key, {BestBefore, Val}}] ->
	    StartTime = snmp:system_start_time(),
	    case (snmp_misc:now(cs) - StartTime) of
		Now when BestBefore >= Now ->
		    Val;
		_ ->
		    ets:delete(Notes, Key),
		    undefined
	    end;
	[] -> undefined
    end.

%%-----------------------------------------------------------------
%% Clean up all old notes in the database.
%%-----------------------------------------------------------------
gc(Tab) ->
    RealUpTime = snmp_misc:now(cs) - snmp:system_start_time(),
    gc(nothing_left, ets:tab2list(Tab), Tab, RealUpTime).

gc(Flag, [{_Key, {infinity, _}} | T], Tab, Now) -> gc(Flag, T, Tab, Now);
gc(Flag, [{Key, {BestBefore, _}} | T], Tab, Now) 
  when integer(BestBefore), BestBefore < Now ->
    ets:delete(Tab, Key),
    gc(Flag, T, Tab, Now);
gc(_Flag, [_ | T], Tab, Now) -> gc(work_to_do, T, Tab, Now);
gc(Flag, [], _Tab, _Now) -> Flag.
    
    
get_verbosity(L) ->
    snmp_misc:get_option(verbosity,L,?default_verbosity).

