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
-module(timer).

-export([apply_after/4,
	 send_after/3, send_after/2,
	 exit_after/3, exit_after/2, kill_after/2, kill_after/1,
	 apply_interval/4, send_interval/3, send_interval/2,
	 cancel/1, sleep/1, tc/3,
	 seconds/1, minutes/1, hours/1, hms/3]).

-export([start_link/0, start/0, 
	 handle_call/3,  handle_info/2,  
	 init/1, start_link/0,
	 code_change/3, handle_cast/2, terminate/2]).

%% Max
-define(MAX_TIMEOUT, 16#0800000).

%%
%% Interface functions
%%
%% Time is in milliseconds.
%%
apply_after(Time, M, F, A) ->
    req(apply_after, {Time, {M, F, A}}).

send_after(Time, Pid, Message) ->
    req(apply_after, {Time, {?MODULE, send, [Pid, Message]}}).

send_after(Time, Message) ->
    send_after(Time, self(), Message).

exit_after(Time, Pid, Reason) ->
    req(apply_after, {Time, {erlang, exit, [Pid, Reason]}}).

exit_after(Time, Reason) ->
    exit_after(Time, self(), Reason).

kill_after(Time, Pid) ->
    exit_after(Time, Pid, kill).

kill_after(Time) ->
    exit_after(Time, self(), kill).

apply_interval(Time, M, F, A) ->
    req(apply_interval, {Time, self(), {M, F, A}}).

send_interval(Time, Pid, Message) ->
    req(apply_interval, {Time, Pid, {?MODULE, send, [Pid, Message]}}).

send_interval(Time, Message) ->
    send_interval(Time, self(), Message).

cancel(BRef) ->
    req(cancel, BRef).

sleep(T) ->
    receive
    after T -> ok
    end.

%%
%% Measure the execution time (in microseconds) for an MFA.
%%
tc(M, F, A) ->
    Before = erlang:now(),
    Val = (catch apply(M, F, A)),
    After = erlang:now(),
    Elapsed = (element(1,After)*1000000000000 +
	       element(2,After)*1000000 + 
	       element(3,After)) -
	(element(1,Before)*1000000000000 +
	 element(2,Before)*1000000 + element(3,Before)),
    {Elapsed, Val}.


%%
%% Convert seconds, minutes etc. to milliseconds.    
%%
seconds(Seconds) ->
    1000*Seconds.
minutes(Minutes) ->
    1000*60*Minutes.
hours(Hours) ->
    1000*60*60*Hours.
hms(H, M, S) ->
    hours(H) + minutes(M) + seconds(S).

%%   
%%   Start/init functions
%%

%%   Start is only included because of backward compatibility!
start() ->
    ensure_started().

start_link() ->
    gen_server:start_link({local, timer_server}, ?MODULE, [], []).    

init([]) ->
    process_flag(trap_exit, true),
    {ok, [], infinity}.

ensure_started() ->
    case whereis(timer_server) of
	undefined -> 
	    C = {timer_server, {?MODULE, start_link, []}, permanent, 1000, 
		 worker, [?MODULE]},
	    supervisor:start_child(kernel_safe_sup, C),  % kernel_safe_sup
	    ok;
	_ -> ok
    end.

%% server calls

req(Req, Arg) ->
    SysTime = system_time(),
    ensure_started(),
    gen_server:call(timer_server, {Req, Arg, SysTime}, infinity).

%%
%% handle_call(Request, From, Timers) -> 
%%  {reply, Response, Timers, Timeout}
%%
%% Time and Timeout is in milliseconds. Started is in microseconds.
%%
handle_call({apply_after, {Time, Op}, Started}, _From, Ts) 
  when integer(Time), Time >= 0 ->
    SysTime = system_time(),
    BRef = {SysTime, make_ref()},
    Timer = {Started + 1000*Time, BRef, timeout, Op}, 
    {Timeout, Ts0} = timer_timeout(insert_sort(Timer, Ts), SysTime),
    {reply, {ok, BRef}, Ts0, Timeout};

handle_call({apply_interval, {Time, To, MFA}, Started}, _From, Ts) 
  when integer(Time), Time >= 0 ->
    %% To must be a pid or a registered name
    case get_pid(To) of
	Pid when pid(Pid) ->
	    catch link(Pid),
	    SysTime = system_time(),
	    BRef = {SysTime, make_ref()},
	    Interval = Time*1000,
	    Timer = {Started + Interval, BRef, {repeat, Interval, Pid}, MFA},
	    {Timeout, Ts0} = timer_timeout(insert_sort(Timer, Ts), SysTime),
	    {reply, {ok, BRef}, Ts0, Timeout};
	_ ->
	    {reply, {error, badarg}, Ts, next_timeout(Ts)}
    end;

handle_call({cancel, {SysTime, Ref}, _}, _From, Ts) when reference(Ref) ->
    Ts0 = delete_ref({SysTime, Ref}, Ts),
    {reply, {ok, cancel}, Ts0, next_timeout(Ts0)};
handle_call({cancel, _BRef, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout(Ts)};
handle_call({apply_after, _, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout(Ts)};
handle_call({apply_interval, _, _}, _From, Ts) ->
    {reply, {error, badarg}, Ts, next_timeout(Ts)};
handle_call(Else, _From, Ts) ->			% Catch anything else
    {noreply, Ts, next_timeout(Ts)}.

handle_info(timeout, Ts) ->                     % Handle timeouts 
    {Timeout, Ts0} = timer_timeout(Ts, system_time()),
    {noreply, Ts0, Timeout};
handle_info({'EXIT',  Pid, Reason}, Ts) ->      % Oops someone died
    Ts0 = pid_delete(Pid, Ts),
    {noreply, Ts0, next_timeout(Ts0)};
handle_info(Msg, Ts) ->                         % Other Msg's
    {noreply, Ts, next_timeout(Ts)}.

handle_cast(Req, Ts) ->                         % Not predicted but handled
    {noreply, Ts, next_timeout(Ts)}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    %% According to the man for gen server no timer can be set here.
    {ok, State}.				

%% 
%% timer_timeout(Timers, SysTime)
%%
%% Apply and remove already timed-out timers. A timer is a tuple
%% {Time, BRef, Op, MFA}, where Time is in microseconds.
%% Returns {Timeout, Timers}, where Timeout is in milliseconds.
%%
timer_timeout([], _) ->
    {infinity, []};

timer_timeout([{Time, BRef, Op, MFA} | Ts], SysTime) when Time > SysTime ->
    Timeout = positive(Time - SysTime) div 1000,
    %% Returned timeout must fit in a small int
    {min(Timeout, ?MAX_TIMEOUT), [{Time, BRef, Op, MFA} | Ts]};

timer_timeout([{Time, BRef, timeout, MFA} | Ts], SysTime) ->
    do_apply(MFA),
    timer_timeout(Ts, SysTime);

timer_timeout([{Time, BRef, {repeat, Interv, To}, MFA} | Ts], SysTime) ->
    do_apply(MFA),
    Ts0 = insert_sort({Time + Interv, BRef, {repeat, Interv, To}, MFA}, Ts),
    timer_timeout(Ts0, SysTime).

%%
%% Insert timer in a sorted timer list. 
%%
insert_sort(Timer, []) ->
    [Timer];
insert_sort({Time0, BRef0, Op0, MFA0}, 
	    [{Time1, BRef1, Op1, MFA1} | Rest]) when Time0 < Time1 ->
    [{Time0, BRef0, Op0, MFA0}, {Time1, BRef1, Op1, MFA1} | Rest];
insert_sort({Time0, BRef0, Op0, MFA0}, 
	    [{Time1, BRef1, Op1, MFA1} | Rest]) ->
    [{Time1, BRef1, Op1, MFA1} | insert_sort({Time0, BRef0, Op0, MFA0}, Rest)].
%%
%% delete_ref 
%%
delete_ref(BRef, [{_,BRef,_,_} | Rest]) ->
    Rest;
delete_ref(BRef, [H|R]) ->
    [H|delete_ref(BRef, R)];
delete_ref(BRef, []) ->
    [].

%%
%% pid_delete
%%
pid_delete(Pid, [{Time, BRef, {repeat, Int, Pid}, Item}|R]) ->
    pid_delete(Pid, R);
pid_delete(Pid, [H|T]) ->
    [H|pid_delete(Pid, T)];
pid_delete(_,[]) ->
    [].

%% Calculate time to the next timeout.Returned timeout must fit in a 
%% small int.
next_timeout([{Time, _, _, _} | R]) ->
    min(positive((Time - system_time()) div 1000), ?MAX_TIMEOUT);
next_timeout([]) ->
    infinity.

%% Help functions
do_apply({M,F,A}) ->
    case {M, F, A} of
	{?MODULE, send, A} -> 
	    %% If send op. send directly, (faster than spawn)
	    catch send(A);
	{erlang, exit, [Name, Reason]} ->
	    catch exit(get_pid(Name), Reason);
	_ -> 
	    %% else spawn process with the operation
	    catch spawn(M,F,A)      
    end.

max(X, Y) when X > Y ->
    X;
max(X, Y) ->
    Y.
min(X, Y) when X < Y ->
    X;
min(X, Y) ->
    Y.
positive(X) ->
    max(X, 0).


%%
%%  system_time() -> time in microseconds
%%
system_time() ->    
    {M,S,U} = erlang:now(),
    1000000*(M*1000000 + S) + U.


send([Pid, Msg]) ->
    Pid ! Msg.

get_pid(Name) when pid(Name) ->
    Name;
get_pid(undefined) ->
    undefined;
get_pid(Name) when atom(Name) ->
    get_pid(whereis(Name));
get_pid(_) ->
    undefined.
