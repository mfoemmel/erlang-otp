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
	 get_system_memory_data/0]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, format_status/2]).

%%%-----------------------------------------------------------------
%%% This is a rewrite of memsup from BS.3 by Peter Högfeldt.
%%%
%%%  This module implements a server process that checks the memory
%%%  usage.
%%%-----------------------------------------------------------------

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
		ext_pending = []}).

start_link() -> gen_server:start_link({local, memsup}, memsup, [], []).

%%-----------------------------------------------------------------
%% Returns: {TotalMemorySize, AllocatedBytes},
%%           {LargestPid, AllocatedBytes}
%%-----------------------------------------------------------------
get_memory_data() -> gen_server:call(memsup, get_memory_data).

get_system_memory_data() -> gen_server:call(memsup, get_system_memory_data).

get_check_interval() -> gen_server:call(memsup, get_check_interval).

get_sysmem_high_watermark() ->
    gen_server:call(memsup, get_sysmem_high_watermark).

get_procmem_high_watermark() ->
    gen_server:call(memsup, get_procmem_high_watermark).


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
    SysMem = get_system_memory_high_watermark(),
    ProcMem = case CollectProcmem of
		  false ->
		      undefined;
		  true ->
		      get_process_memory_high_watermark()
	      end,
    %% Syncronous collection of memory data the first time
    case catch 
	begin
	    case whereis(memsup_helper) of
		Pid when pid(Pid) ->
		    Ref = erlang:monitor(process,Pid),
		    Worst = case ProcMem of
				undefined ->
				    undefined;
				_ ->
				    get_worst_sync()
			    end,
		    MemUsage = get_sys_sync(),
		    timer:send_after(Timeout, time_to_collect),
		    erlang:demonitor(Ref), % Don't care any more
		    {ok, 
		     #state{timeout = Timeout,
			    mem_usage = MemUsage, 
			    worst_mem_user = Worst,
			    sys_mem_watermark = SysMem,
			    proc_mem_watermark = ProcMem,
			    collect_procmem = CollectProcmem}};
		_ ->
		    exit(helper_not_present)
	    end
	end of
	{'EXIT', Reason} ->
	    {stop, Reason, []};
	Else ->
	    Else
    end.

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
handle_call(get_memory_data, _From, State) ->
    {Alloc, Total} = State#state.mem_usage,
    {reply, {Total, Alloc, State#state.worst_mem_user}, State};
handle_call(get_system_memory_data, From, State) ->
    {ok, WDTimer} = timer:send_after(30000, collection_timeout),
    memsup_helper ! {self(), collect_ext_sys},
    NPending = [ From |State#state.ext_pending],
    {noreply, State#state{ext_wd_timer = WDTimer,
			  ext_pending = NPending}};
handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};
handle_call(get_sysmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.sys_mem_watermark), State};
handle_call(get_procmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.proc_mem_watermark), State}.

handle_info(time_to_collect, State) ->
    LastTimeout = get_timestamp(),
    {ok, WDTimer} = timer:send_after(30000, collection_timeout),
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

handle_info({collected_proc,{Pid, PidAllocated} = Worst}, State) ->
    memsup_helper ! {self(), collect_sys},
    {_,Total} = State#state.mem_usage, % OK; its, the old total, but
				       % system available memory does not 
				       % change on a regular basis...
    if
	PidAllocated > State#state.proc_mem_watermark*Total ->
	    set_alarm(process_memory_high_watermark, Pid);
	true ->
	    reset_alarm(process_memory_high_watermark)
    end,
    {noreply, State#state{worst_mem_user = Worst}};

handle_info({collected_sys,{Allocated, Total} = Sys}, State) ->
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

get_timeout() ->
    case application:get_env(os_mon, memory_check_interval) of
	{ok, Value} -> minutes_to_ms(Value);
	_ -> minutes_to_ms(1)
    end.

get_timestamp() ->
    {A,B,C} = erlang:now(), 
    (C div 1000) + B * 1000 + A * 1000000.

get_system_memory_high_watermark() ->
    case application:get_env(os_mon, system_memory_high_watermark) of
	{ok, Value} -> Value;
	_ -> 0.80
    end.

get_process_memory_high_watermark() ->
    case application:get_env(os_mon, process_memory_high_watermark) of
	{ok, Value} -> Value;
	_ -> 0.05
    end.

get_worst_sync() ->
    get_worst_sync(10).
get_worst_sync(0) ->
    exit({timeout, memsup_helper_not_alive});
get_worst_sync(N) ->
    (catch memsup_helper ! {self(), collect_proc}),
    receive
	{'DOWN', _, _, Info} -> % gotta be the helper here
	    receive 
	    after 1000 ->
		    ok
	    end,
	    get_worst_sync(N - 1);
	{collected_proc, Worst} ->
	    Worst;
	Else ->
	    exit({unexpected_message,Else})
    end.
	    
get_sys_sync() ->
    get_sys_sync(10).
get_sys_sync(0) ->
    exit({timeout, memsup_helper_not_alive});
get_sys_sync(N) ->
    (catch memsup_helper ! {self(), collect_sys}),
    receive
	{'DOWN', _, _, _} -> % gotta be the helper here
	    receive 
	    after 1000 ->
		    ok
	    end,
	    get_worst_sync(N - 1);
	{collected_sys, Sys} ->
	    Sys;
	Else ->
	    exit({unexpected_message,Else})
    end.
	    

format_status(Opt, [PDict, #state{timeout = Timeout, mem_usage = MemUsage,
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
