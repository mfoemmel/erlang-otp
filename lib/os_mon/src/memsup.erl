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
-export([start_link/0, get_memory_data/0, get_check_interval/0,
	 get_sysmem_high_watermark/0, get_procmem_high_watermark/0,
	 get_system_memory_data/0,
	 get_helper_timeout/0, set_helper_timeout/1]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, format_status/2,
	 code_change/3]).

%%%-----------------------------------------------------------------
%%% This is a rewrite of memsup from BS.3 by Peter Högfeldt.
%%%
%%%  This module implements a server process that checks the memory
%%%  usage.
%%%-----------------------------------------------------------------

-define(DEFAULT_HELPER_TIMEOUT, 30000). % In milli seconds

%%%-define(MEM_SHOW, 1).
-include("memsup.hrl").
-record(state, {timeout, 
		mem_usage, 
		worst_mem_user,
		sys_mem_watermark, 
		proc_mem_watermark,
		collect_procmem = true,
		last_timeout = 0,
		wd_timer = undefined,
		ext_wd_timer = undefined,
		ext_pending = [],
		helper_timeout = ?DEFAULT_HELPER_TIMEOUT}).

start_link() -> gen_server:start_link({local, memsup}, memsup, [], []).

%%-----------------------------------------------------------------
%% Returns: {TotalMemorySize, AllocatedBytes},
%%           {LargestPid, AllocatedBytes}
%%-----------------------------------------------------------------
get_memory_data() -> call(get_memory_data).

get_system_memory_data() -> call(get_system_memory_data).

get_check_interval() -> call(get_check_interval).

get_sysmem_high_watermark() ->
    call(get_sysmem_high_watermark).

get_procmem_high_watermark() ->
    call(get_procmem_high_watermark).

get_helper_timeout() ->
    call(get_helper_timeout).

set_helper_timeout(I) when integer(I), I > 0 ->
    call({set_helper_timeout, I});
set_helper_timeout(I) ->
    erlang:fault(badarg, [I]).

call(Req) ->
    gen_server:call(memsup, Req, 30000).

init([]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),
    CollectProcmem = 
	case application:get_env(os_mon,memsup_system_only) of
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
    timer:send_after(Timeout, time_to_collect),
    {ok,#state{timeout = Timeout,
	       helper_timeout = HelperTimeout,
	       sys_mem_watermark = SysMem,
	       proc_mem_watermark = ProcMem,
	       collect_procmem = CollectProcmem}}.

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
handle_call(get_memory_data, From, #state{mem_usage=undefined,
					  helper_timeout=Timeout}=St) ->
    MemUsage = get_synchronous_data(Timeout, collect_sys, collected_sys),
    handle_call(get_memory_data, From, St#state{mem_usage=MemUsage});
handle_call(get_memory_data, From, #state{collect_procmem=true,
					  worst_mem_user=undefined,
					  helper_timeout=Timeout}=St) ->
    Worst = get_synchronous_data(Timeout, collect_proc, collected_proc),
    handle_call(get_memory_data, From, St#state{worst_mem_user=Worst});
handle_call(get_memory_data, _From, State) ->
    {Alloc, Total} = State#state.mem_usage,
    {reply, {Total, Alloc, State#state.worst_mem_user}, State};
handle_call(get_system_memory_data, From, State) ->
    {ok, WDTimer} = timer:send_after(State#state.helper_timeout,
				     collection_timeout),
    memsup_helper ! {self(), collect_ext_sys},
    NPending = [ From |State#state.ext_pending],
    {noreply, State#state{ext_wd_timer = WDTimer,
			  ext_pending = NPending}};
handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};
handle_call(get_sysmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.sys_mem_watermark), State};
handle_call(get_procmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.proc_mem_watermark), State};
handle_call(get_helper_timeout, _From, State) ->
    {reply, ms_to_sec(State#state.helper_timeout), State};
handle_call({set_helper_timeout, Secs}, _From, State) ->
    {reply, ok, State#state{helper_timeout = sec_to_ms(Secs)}};

%% The following are only for test purposes (whitebox testing).
handle_call({set_sys_hw,HW}, _From, State) ->
    {reply, ok, State#state{sys_mem_watermark=HW}};
handle_call({set_pid_hw,HW}, _From, State) ->
    {reply, ok, State#state{proc_mem_watermark=HW}};
handle_call({set_check_interval,Timeout}, _From, State) ->
    {reply, ok, State#state{timeout=Timeout}}.

handle_info(time_to_collect, State) ->
    LastTimeout = get_timestamp(),
    {ok, WDTimer} = timer:send_after(State#state.helper_timeout,
				     collection_timeout),
    case State#state.collect_procmem of
	true ->
	    memsup_helper ! {self(), collect_proc};
	false ->
	    memsup_helper ! {self(), collect_sys}
    end,
    {noreply, 
     State#state{wd_timer = WDTimer, last_timeout = LastTimeout}};

handle_info(collection_timeout, State) ->
    {stop, {memsup_collection_error, helper_timeout}, State};

handle_info({collected_proc,{Pid,PidAllocated}=Worst},
	    #state{mem_usage=Usage,proc_mem_watermark=MemWatermark}=State) ->
    memsup_helper ! {self(), collect_sys},
    case Usage of
	{_,Total} when PidAllocated > MemWatermark*Total ->
	    set_alarm(process_memory_high_watermark, Pid);
	_ ->
	    reset_alarm(process_memory_high_watermark)
    end,
    {noreply,State#state{worst_mem_user=Worst}};

handle_info({collected_sys,{Allocated,Total}=Sys}, State) ->
    timer:cancel(State#state.wd_timer),
    Diff = get_timestamp() - State#state.last_timeout,
    Timeout = case State#state.timeout - Diff of
		  N when N < 0 ->
		      0;
		  M ->
		      M
	      end,
    timer:send_after(Timeout, time_to_collect),
    if
	Allocated > State#state.sys_mem_watermark*Total ->
	    set_alarm(system_memory_high_watermark, []);
	true ->
	    reset_alarm(system_memory_high_watermark)
    end,
    {noreply, State#state{last_timeout = 0, wd_timer = undefined, 
			  mem_usage = Sys}};

handle_info({collected_ext_sys, ExtSys}, State) ->
    timer:cancel(State#state.ext_wd_timer),
    lists:foreach(fun(To) -> gen_server:reply(To, ExtSys) end, 
		  State#state.ext_pending),
    {noreply, State#state{ext_wd_timer = undefined, 
			  ext_pending = []}};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
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

minutes_to_ms(Minutes) -> trunc(60000*Minutes).
sec_to_ms(Sec) -> trunc(1000*Sec).
ms_to_sec(MS) -> MS div 1000.

get_timeout() ->
    case application:get_env(os_mon, memory_check_interval) of
	{ok, Value} when number(Value), Value >= 1 -> minutes_to_ms(Value);
	{ok, BadValue} -> exit({bad_config_parameter,
				{memory_check_interval, BadValue}});
	_ -> minutes_to_ms(1)
    end.

get_memsup_helper_timeout() ->
    case application:get_env(os_mon, memsup_helper_timeout) of
	{ok, Value} when integer(Value), Value >= 1 -> sec_to_ms(Value);
	{ok, BadValue} -> exit({bad_config_parameter,
				{memsup_helper_timeout, BadValue}});
	_ -> ?DEFAULT_HELPER_TIMEOUT
    end.

get_timestamp() ->
    {A,B,C} = erlang:now(), 
    (C div 1000) + B * 1000 + A * 1000000.

get_system_memory_high_watermark() ->
    case application:get_env(os_mon, system_memory_high_watermark) of
	{ok, Value} when number(Value) -> Value;
	{ok, BadValue} -> exit({bad_config_parameter,
				{system_memory_high_watermark, BadValue}});
	_ -> 0.80
    end.

get_process_memory_high_watermark() ->
    case application:get_env(os_mon, process_memory_high_watermark) of
	{ok, Value} when number(Value) -> Value;
	{ok, BadValue} -> exit({bad_config_parameter,
				{process_memory_high_watermark, BadValue}});
	_ -> 0.05
    end.


get_synchronous_data(Timeout, Tag, ResTag) ->
    get_synchronous_data_1(50, Timeout, Tag, ResTag).

get_synchronous_data_1(0, _, _, _) ->
    exit({timeout,memsup_helper_not_alive});
get_synchronous_data_1(N, Timeout, Tag, ResTag) ->
    MonitorRef = erlang:monitor(process, memsup_helper),
    catch memsup_helper ! {self(),Tag},
    receive
	{ResTag,Result} ->
	    erlang:demonitor(MonitorRef),
	    receive
		{'DOWN',MonitorRef,process,_,_} -> ok
	    after 0 -> ok
	    end,
	    Result;
	{'DOWN',MonitorRef,process,_,_} ->
	    receive 
		after 500 ->
			ok
		end,
	    get_synchronous_data_1(N-1, Timeout, Tag, ResTag)
    after Timeout ->
	    exit({memsup_collection_error,helper_timeout})
    end.

format_status(_Opt, [_PDict, #state{timeout = Timeout, mem_usage = MemUsage,
				  worst_mem_user = WorstMemUser}]) ->
    {Allocated, Total} = MemUsage,
    WorstMemFormat = case WorstMemUser of
			 {Pid, Mem} ->
			     [{"Pid", Pid}, {"Memory", Mem}];
			 _ ->
			     undefined
		     end,
    [{data, [{"Timeout", Timeout}]},
     {items, {"Memory Usage", [{"Allocated", Allocated}, {"Total", Total}]}},
     {items, {"Worst Memory User", WorstMemFormat}}].

