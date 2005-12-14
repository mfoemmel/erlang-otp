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
-module(memsup).
-behaviour(gen_server).

%% NOTE: Contains code for live upgrade from 1.7.8 to 1.8 (and live
%% downgrade from 1.8 to 1.7.8) -- this code can be removed when
%% the module is updated next time.
%% Look for "% vsn 1.7.8"

%% API
-export([start_link/0]).
-export([get_check_interval/0,
	 get_memory_data/0, get_system_memory_data/0,
	 get_procmem_high_watermark/0, get_sysmem_high_watermark/0,
	 get_helper_timeout/0, set_helper_timeout/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Other exports
-export([format_status/2]).

%%%-----------------------------------------------------------------
%%% This is a rewrite of memsup from BS.3 by Peter Högfeldt.
%%%
%%% This module implements a server process that checks the memory
%%% usage.
%%%-----------------------------------------------------------------

-include("memsup.hrl").

-record(state,
	{timeout,             % int()   memory_check_interval, ms

	 mem_usage,           % undefined | {Alloc, Total}
	 worst_mem_user,      % undefined | {Pid, Alloc}

	 sys_mem_watermark,   % float() system_memory_high_watermark, %
	 proc_mem_watermark,  % float() process_memory_high_watermark, %
                              % | undefined
	 collect_procmem,     % bool()  memsup_system_only

	 wd_timer,            % undefined | TimerRef
	 pending = [],        % [From]

	 ext_wd_timer,        % undefined | TimerRef
	 ext_pending = [],    % [From]

	 helper_timeout       % int()   memsup_helper_timeout, ms
	}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% get_check_interval() -> Time (in ms)
%% Returns the time interval which defines how often memory is checked.
%% The value is actually initiated by the configuration parameter
%% 'memory_check_interval', but although this parameter has a value in
%% minutes, for some unknown reason, the value returned here is in ms.
get_check_interval() ->
    call(get_check_interval).

%% get_memory_data() -> MemData
%%   MemData = {TotMemSize, AllBytes, {Pid, PidAllBytes}}
%% Returns the latest result of the data collection.
get_memory_data() ->
    call(get_memory_data).

%% get_system_memory_data() -> [MemData]
%%   MemData = {Tag, Val}
%% Initiates a data collection and returns the result.
get_system_memory_data() ->
    call(get_system_memory_data).

%% get_procmem_high_watermark() -> Threshold (percent)
%% Returns the process memory threshold in percent.
%% The value is actually initiated by the configuration parameter
%% 'process_memory_high_watermark', but although this parameter has
%% float value, for some unknown reason, the value returned here is in
%% percent.
get_procmem_high_watermark() ->
    call(get_procmem_high_watermark).

%% get_sysmem_high_watermark() -> Threshold (percent)
%% Returns the system memory threshold in percent.
%% The value is actually initiated by the configuration parameter
%% 'system_memory_high_watermark', but although this parameter has
%% float value, for some unknown reason, the value returned here is in
%% percent.
get_sysmem_high_watermark() ->
    call(get_sysmem_high_watermark).

%% get_helper_timeout() -> Seconds
%% The timeout value initiated by the configuration paramter
%% 'memsup_helper_timeout'.
get_helper_timeout() ->
    call(get_helper_timeout).

%% set_helper_timeout(Seconds) -> ok
%% The timeout value initiated by the configuration paramter
%% 'memsup_helper_timeout'.
set_helper_timeout(Seconds) when is_integer(Seconds), Seconds>0 ->
    call({set_helper_timeout, Seconds});
set_helper_timeout(Other) ->
    erlang:error(badarg, [Other]).

call(Req) ->
    gen_server:call(memsup, Req, infinity).

%%====================================================================
%% process initiation
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),

    %% Read the values of some configuration parameters
    CollectProcmem =
	case application:get_env(os_mon, memsup_system_only) of
	    {ok, true} ->
		false;
	    _ ->
		true
	end,
    Timeout = get_timeout(),
    HelperTimeout = get_memsup_helper_timeout(),
    SysMem = get_system_memory_high_watermark(),
    ProcMem = case CollectProcmem of
		  false ->
		      undefined;
		  true ->
		      get_process_memory_high_watermark()
	      end,

    %% Initiate first data collection
    self() ! time_to_collect,

    {ok, #state{timeout=Timeout,
		sys_mem_watermark=SysMem,
		proc_mem_watermark=ProcMem,
		collect_procmem=CollectProcmem,
		helper_timeout=HelperTimeout}}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};

handle_call(get_memory_data, From, State) ->
    MemUsage = State#state.mem_usage,
    Worst = State#state.worst_mem_user,

    %% Special case: get_memory_data called before first data collection
    %% is ready, wait with the reply
    if
	Worst==undefined, State#state.collect_procmem==true ->
	    {noreply, State#state{pending=[From|State#state.pending]}};
	MemUsage==undefined ->
	    {noreply, State#state{pending=[From|State#state.pending]}};
	true ->
	    {Alloc, Total} = MemUsage,
	    {reply, {Total, Alloc, Worst}, State}
    end;

handle_call(get_system_memory_data, From, State) ->
    case State#state.ext_wd_timer of
	undefined ->
	    WDTimer = erlang:send_after(State#state.helper_timeout,
					self(),
					ext_collection_timeout),
	    catch memsup_helper ! {self(), collect_ext_sys},
	    Pending = [From | State#state.ext_pending],
	    {noreply, State#state{ext_wd_timer=WDTimer,
				  ext_pending=Pending}};
	_TimerRef -> % we've already ordered ext data collection
	    Pending = [From | State#state.ext_pending],
	    {noreply, State#state{ext_pending=Pending}}
    end;

handle_call(get_procmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.proc_mem_watermark), State};

handle_call(get_sysmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.sys_mem_watermark), State};

handle_call(get_helper_timeout, _From, State) ->
    {reply, ms_to_sec(State#state.helper_timeout), State};

handle_call({set_helper_timeout, Secs}, _From, State) ->
    {reply, ok, State#state{helper_timeout=sec_to_ms(Secs)}};

%% The following are only for test purposes (whitebox testing).
handle_call({set_sys_hw, HW}, _From, State) ->
    {reply, ok, State#state{sys_mem_watermark=HW}};
handle_call({set_pid_hw, HW}, _From, State) ->
    {reply, ok, State#state{proc_mem_watermark=HW}};
handle_call({set_check_interval, Timeout}, _From, State) ->
    {reply, ok, State#state{timeout=Timeout}};
handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(nothing_should_match, State) ->
    {noreply, State}.

%% It's time to check memory
%% Ask memsup_handler for system data
handle_info(time_to_collect, State) ->
    WDTimer = erlang:send_after(State#state.helper_timeout,
				self(),
				reg_collection_timeout),
    catch memsup_helper ! {self(), collect_sys},
    {noreply, State#state{wd_timer=WDTimer}};

%% System memory collected, if necessary set the system alarm
%% Note that the collected data is {0,0} collection was temporarily
%% impossible to make.
handle_info({collected_sys, {Alloc,Total}=Sys}, State) ->
    if
	Alloc > State#state.sys_mem_watermark*Total ->
	    set_alarm(system_memory_high_watermark, []);
	Alloc==0 -> % no correct data collected
	    ignore;
	true ->
	    reset_alarm(system_memory_high_watermark)
    end,
    State2 = State#state{mem_usage=Sys},
    case State2#state.collect_procmem of
	true ->
	    %% Ask memsup_handler for process data as well
	    catch memsup_helper ! {self(), collect_proc},
	    {noreply, State2};
	false ->
	    %% No process data should be collected, proceed immediately
	    %% to next function clause
	    handle_info({collected_proc, undefined}, State2)
    end;

%% Process memory collected, if necessary set the process alarm
handle_info({collected_proc, Worst}, State) ->
    TimeSpent = case erlang:cancel_timer(State#state.wd_timer) of
		    false ->
			State#state.helper_timeout;
		   TimeLeft ->
			State#state.helper_timeout-TimeLeft
	       end,

    {SysAlloc, Total} = State#state.mem_usage,
    %% Special case: if this is the first data collection, Pending may
    %% be a non-empty list of pending clients. (Later it will always be
    %% the empty list).
    reply(State#state.pending, {Total, SysAlloc, Worst}),
    Threshold = State#state.proc_mem_watermark*Total,
    case Worst of
	{Pid, PidAlloc} when Threshold/=0, PidAlloc>Threshold ->
	    set_alarm(process_memory_high_watermark, Pid);
	{_Pid, PidAlloc} when Threshold/=0, PidAlloc=<Threshold ->
	    reset_alarm(process_memory_high_watermark);
	_ ->
	    %% Either
	    %%   Threshold==0, ie no system data could be collected, or
	    %%   Worst==undefined, ie no process data was collected
	    %% In either case, don't touch the process alarm
	    ignore
    end,

    %% Set the new timeout (interval-time spent collecting),
    Time = case State#state.timeout - TimeSpent of
		  MS when MS<0 ->
		      0;
		  MS ->
		      MS
	   end,
    erlang:send_after(Time, self(), time_to_collect),

    {noreply, State#state{worst_mem_user=Worst,
			  wd_timer=undefined, pending=[]}};

%% Timeout during regular data collection
handle_info(reg_collection_timeout, State) ->
    Str = "OS_MON (memsup) timeout, no data collected~n",
    error_logger:warning_msg(Str),

    %% Special case: First data collection has failed, use dummy
    %% values and send reply to pending clients
    {Alloc, Total} = case State#state.mem_usage of
			 undefined ->
			     {0, 0};
			 MemUsage ->
			     MemUsage
		     end,
    Worst = case State#state.worst_mem_user of
		undefined when State#state.collect_procmem==true ->
		    {self(), 0};
		Else ->
		    Else
	    end,
    reply(State#state.pending, {Total, Alloc, Worst}),

    %% Set the new timeout (interval-time spent collecting)
    Time = case State#state.timeout - State#state.helper_timeout of
	       MS when MS<0 ->
		   0;
	       MS ->
		   MS
	   end,
    erlang:send_after(Time, self(), time_to_collect),

    {noreply, State#state{mem_usage={Alloc,Total}, worst_mem_user=Worst,
			  wd_timer=undefined, pending=[]}};

%% Extensive data collected
handle_info({collected_ext_sys, ExtSys}, State) ->
    erlang:cancel_timer(State#state.ext_wd_timer),
    reply(State#state.ext_pending, ExtSys),
    {noreply, State#state{ext_wd_timer=undefined, ext_pending=[]}};

%% Timeout during extensive data collection
handle_info(ext_collection_timeout, State) ->
    Str = "OS_MON (memsup) timeout, no data collected~n",
    error_logger:warning_msg(Str),
    reply(State#state.ext_pending, []),
    {noreply, State#state{ext_wd_timer=undefined, ext_pending=[]}};

%% vsn 1.7.8
%% This clause is only needed for backwards compatibility with 1.7.8
%% to take care of a timer set before the release upgrade was made
%% which expired before it was cancelled (in code_change/3)
handle_info(collection_timeout, State) -> % vsn 1.7.8
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% vsn 1.7.8
%% For live upgrade/downgrade to/from 1.8
code_change(Vsn, PrevState, _Extra) ->
    case Vsn of

	%% Downgrade from this version
	{down, _Vsn} ->

	    %% Cancel timers unknown to 1.7.8 version and send dummy
	    %% replies to any pending clients
	    case PrevState#state.wd_timer of
		undefined ->
		    ignore;
		TimerRef1 ->
		    erlang:cancel_timer(TimerRef1),
		    Reply = case PrevState#state.collect_procmem of
				true ->
				    {0, 0, {self(), 0}};
				false ->
				    {0, 0, undefined}
			    end,
		    reply(PrevState#state.pending, Reply)
	    end,
	    case PrevState#state.ext_wd_timer of
		undefined ->
		    ignore;
		TimerRef2 ->
		    erlang:cancel_timer(TimerRef2),
		    reply(PrevState#state.ext_pending, [])
	    end,

	    %% Downgrade to old state record
	    State = {state,
		     PrevState#state.timeout,
		     PrevState#state.mem_usage,
		     PrevState#state.worst_mem_user,
		     PrevState#state.sys_mem_watermark,
		     PrevState#state.proc_mem_watermark,
		     PrevState#state.collect_procmem,
		     0, % last_timeout,
		     undefined, % wd_timer
		     undefined, % ext_wd_timer
		     [],        % ext_pending
		     PrevState#state.helper_timeout},
	    {ok, State};

	%% Upgrade to this version
	_Vsn ->

	    %% Old state record
	    {state,
	     Timeout,
	     MemUsage, WorstMemUser,
	     SysMemWatermark, ProcMemWatermark,
	     CollectProcMem,
	     _LastTimeout,
	     WdTimer,
	     ExtWdTimer,
	     ExtPending,
	     HelperTimeout} = PrevState,

	    %% Cancel timers and send dummy replies to any pending
	    %% clients
	    if
		WdTimer/=undefined ->
		    timer:cancel(WdTimer);
		true ->
		    ignore
	    end,
	    if
		ExtWdTimer/=undefined ->
		    timer:cancel(ExtWdTimer),
		    reply(ExtPending, []);
		true ->
		    ignore
	    end,

	    %% Upgrade to this state record
	    State = #state{timeout=Timeout,
			   mem_usage=MemUsage,
			   worst_mem_user=WorstMemUser,
			   sys_mem_watermark=SysMemWatermark,
			   proc_mem_watermark=ProcMemWatermark,
			   collect_procmem=CollectProcMem,
			   wd_timer=undefined,
			   pending=[],
			   ext_wd_timer=undefined,
			   ext_pending=[],
			   helper_timeout=HelperTimeout},
	    {ok, State}
    end.

%%====================================================================
%% Other exports
%%====================================================================

format_status(_Opt, [_PDict, #state{timeout=Timeout, mem_usage=MemUsage,
				    worst_mem_user=WorstMemUser}]) ->
    {Allocated, Total} = MemUsage,
    WorstMemFormat = case WorstMemUser of
			 {Pid, Mem} ->
			     [{"Pid", Pid}, {"Memory", Mem}];
			 undefined ->
			     undefined
		     end,
    [{data, [{"Timeout", Timeout}]},
     {items, {"Memory Usage", [{"Allocated", Allocated},
			       {"Total", Total}]}},
     {items, {"Worst Memory User", WorstMemFormat}}].


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%--Get application configuration parameter values-----------------

%% memory_check_interval, minutes
get_timeout() ->
    case application:get_env(os_mon, memory_check_interval) of
	{ok, Value} when is_number(Value), Value>=1 -> % FIXME int()
	    minutes_to_ms(Value);
	{ok, BadValue} ->
	    exit({bad_config_parameter,
		  {memory_check_interval, BadValue}});
	undefined ->
	    minutes_to_ms(1) % default value = 1 minute
    end.

%% memsup_helper_timeout, seconds
get_memsup_helper_timeout() ->
    case application:get_env(os_mon, memsup_helper_timeout) of
	{ok, Value} when is_integer(Value), Value>=0 ->
	    sec_to_ms(Value);
	{ok, BadValue} ->
	    exit({bad_config_parameter,
		  {memsup_helper_timeout, BadValue}});
	undefined ->
	    30000 % default value = 30 seconds
    end.

%% system_memory_high_watermark, percent (given as a float)
get_system_memory_high_watermark() ->
    case application:get_env(os_mon, system_memory_high_watermark) of
	{ok, Value} when is_number(Value) -> % FIXME range 0<x<1
	    Value;
	{ok, BadValue} ->
	    exit({bad_config_parameter,
		  {system_memory_high_watermark, BadValue}});
	undefined ->
	    0.80 % default value = 80%
    end.

%% process_memory_high_watermark, percent (given as a float)
get_process_memory_high_watermark() ->
    case application:get_env(os_mon, process_memory_high_watermark) of
	{ok, Value} when is_number(Value) -> % FIXME range 0<x<1
	    Value;
	{ok, BadValue} ->
	    exit({bad_config_parameter,
		  {process_memory_high_watermark, BadValue}});
	undefined ->
	    0.05 % default value = 5%
    end.

%%--Alarm handling-------------------------------------------------

set_alarm(AlarmCode, AddInfo) ->
    case get({alarm_status, AlarmCode}) of
	set ->
	    ok;
	_ ->
	    alarm_handler:set_alarm({AlarmCode, AddInfo}),
	    put({alarm_status, AlarmCode}, set)
    end,
    ok.

reset_alarm(AlarmCode) ->
    case get({alarm_status, AlarmCode}) of
	set ->
	    alarm_handler:clear_alarm(AlarmCode),
	    erase({alarm_status, AlarmCode});
	_ ->
	    ok
    end,
    ok.

%%--Auxiliary------------------------------------------------------

%% Type conversions
minutes_to_ms(Minutes) -> trunc(60000*Minutes).
sec_to_ms(Sec) -> trunc(1000*Sec).
ms_to_sec(MS) -> MS div 1000.

reply([], _Reply) ->
    ignore;
reply(Pending, Reply) ->
    lists:foreach(fun(From) -> gen_server:reply(From, Reply) end,
		  Pending).
