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
-record(state, {port, timeout, mem_usage, worst_mem_user,
		sys_mem_watermark, proc_mem_watermark}).

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
    OsType = os:type(),
    Timeout = get_timeout(),
    SysMem = get_system_memory_high_watermark(),
    ProcMem = get_process_memory_high_watermark(),
    process_flag(trap_exit, true),
    process_flag(priority, low),
    PortResult = case OsType of
		     {win32,_} ->{ok,not_used};
		     _ -> start_portprogram()
		 end,
    case PortResult of
	{ok, Port} ->
	    Reply = case OsType of
			{win32,_} ->
			    get_memory_usage_win32(Port);
			_ ->
			    get_memory_usage(Port)
		    end,
	    case Reply of
		{ok, MemUsage} ->
		    Worst = get_worst_memory_user(),
		    timer:send_after(Timeout, timeout),
		    {ok, #state{port = Port, timeout = Timeout,
				mem_usage = MemUsage, worst_mem_user = Worst,
				sys_mem_watermark = SysMem,
				proc_mem_watermark = ProcMem}};
		{error, Reason} ->
		    {stop, {bad_portprogram, Reason}}
	    end;
	{error, Reason} ->
	    {stop, {bad_portprogram, Reason}}
    end.

%%-----------------------------------------------------------------
%% Callback functions from gen_server
%%-----------------------------------------------------------------
handle_call(get_memory_data, _From, State) ->
    {Alloc, Total} = State#state.mem_usage,
    {WPid, WAlloc} = State#state.worst_mem_user,
    {reply, {Total, Alloc, {WPid, WAlloc}}, State};
handle_call(get_system_memory_data, _From, State) ->
    %% This call is syncronous, we do NOT want this to happen at every poll 
    Res = get_system_memory_usage(State#state.port),
    {reply, Res, State};
handle_call(get_check_interval, _From, State) ->
    {reply, State#state.timeout, State};
handle_call(get_sysmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.sys_mem_watermark), State};
handle_call(get_procmem_high_watermark, _From, State) ->
    {reply, trunc(100 * State#state.proc_mem_watermark), State}.

handle_info(timeout, State) ->
    MemUsage = case os:type() of
		   {win32,_} ->
		       get_memory_usage_win32(State#state.port);
		   _ ->
		       get_memory_usage(State#state.port)
	       end,
    case MemUsage of
	{ok, {Allocated, Total}} ->
	    if
		Allocated > State#state.sys_mem_watermark*Total ->
		    set_alarm(system_memory_high_watermark, []);
		true ->
		    reset_alarm(system_memory_high_watermark)
	    end,
	    {Pid, PidAllocated} = get_worst_memory_user(),
	    if
		PidAllocated > State#state.proc_mem_watermark*Total ->
		    set_alarm(process_memory_high_watermark, Pid);
		true ->
		    reset_alarm(process_memory_high_watermark)
	    end,
	    timer:send_after(State#state.timeout, timeout),
	    {noreply, State#state{mem_usage = {Allocated, Total},
				  worst_mem_user = {Pid, PidAllocated}}};  
	{error, Reason} ->
	    {stop, {bad_portprogram, Reason}}
    end;
handle_info({'EXIT', Port, Reason}, State) when State#state.port == Port ->
    {stop, {port_terminated, Reason}, State};
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

start_portprogram() ->
    Pgm = code:priv_dir("os_mon") ++ "/bin/memsup",
    Opts = [{packet, 1}],
    case catch open_port({spawn, Pgm}, Opts) of
	Port when port(Port) ->
	    {ok, Port};
	{'EXIT', Reason} ->
	    {error, Reason}
    end.

%%-----------------------------------------------------------------
%% Func: get_memory_usage_win32(_Port)
%% Purpose: Find out how much memory is in use by asking the os_mon_sysinfo
%% process.
%% Returns {ok,Allocated,Total}} | {error, Reason}
%%-----------------------------------------------------------------
get_memory_usage_win32(_Port) ->
    [Result|_] = os_mon_sysinfo:get_mem_info(),
    case io_lib:fread("~d~d~d~d~d~d~d", Result) of
	{ok, [MemLoad,TotPhys,AvailPhys,TotPage,AvailPage,TotV,AvailV], RestStr} ->
	    {ok,{TotPhys-AvailPhys,TotPhys}};
	Reason ->
	    {error,Reason}
    end.

%%-----------------------------------------------------------------
%% Func: get_memory_usage(Port)
%% Purpose: Function which finds out how much memory is in use by
%%          using the external portprogram.
%% Returns: {ok, {Allocated, Total}} | {error, Reason}
%%-----------------------------------------------------------------
get_memory_usage(Port) ->
    Port ! {self(), {command, [?MEM_SHOW]}},
    receive
	{Port, {data, Alloc}} ->
	    get_total(Port, 256 * list_to_integer(Alloc));
	{'EXIT', Port, _} ->
	    {error, port_terminated}
    end.

get_total(Port, Alloc) ->
    receive
	{Port, {data, Total}} ->
	    {ok, {Alloc, 256 * list_to_integer(Total)}};
	{'EXIT', Port, _} ->
	    {error, port_terminated}
    end.

%%-----------------------------------------------------------------
%% Returns: {Pid, MemoryBytes}
%%-----------------------------------------------------------------

%% All except zombies.
alive_processes() ->
    lists:filter({erlang, is_process_alive}, processes()).

get_worst_memory_user()  ->
    get_worst_memory_user(alive_processes(), {self(), 0}).

get_worst_memory_user([Pid|Rest], {MaxPid, MaxMemBytes}) ->
    case process_memory(Pid) of
	undefined ->
	    get_worst_memory_user(Rest, {MaxPid, MaxMemBytes});
	MemoryBytes ->
	    if 
		MemoryBytes > MaxMemBytes ->
		    get_worst_memory_user(Rest, {Pid, MemoryBytes});
		true ->
		    get_worst_memory_user(Rest, {MaxPid, MaxMemBytes})
	    end
    end;
get_worst_memory_user([], Return) ->
    Return.


%%-----------------------------------------------------------------
%% Return the extended memory data as a tagged list
%% The (current) possible tag values are:
%% total_memory: The total memory available to erlang.
%% free_memory: The amount of unused memory in the erlang "pool".
%% system_total_memory: The amount of memory available to the whole
%%                system (often equal to total_memory).
%% largest_free: The size of the largest contigous free block.
%% number_of_free: The number of blocks in the free list.
%%-----------------------------------------------------------------
get_system_memory_usage(Port) ->
    case os:type() of
	{win32,_} ->
	    case get_memory_usage_win32(Port) of
		{ok, {Alloced,Tot}} ->
		    [{total_memory, Tot},
		     {free_memory, Tot - Alloced},
		     {system_total_memory, Tot}];
		{error, Reason} ->
		    {error, Reason};
		_ ->
		    {error, port_terminated}
	    end;
	_ -> 
	    Port ! {self(), {command, [?SYSTEM_MEM_SHOW]}},
	    collect_sysmem(Port)
    end.

collect_sysmem(Port) ->
    collect_sysmem(Port,[]).

collect_sysmem(Port,Accum) ->
    Tab = [{?SYSTEM_TOTAL_MEMORY, system_total_memory},
	   {?TOTAL_MEMORY, total_memory},
	   {?FREE_MEMORY, free_memory},
	   {?LARGEST_FREE, largest_free},
	   {?NUMBER_OF_FREE, number_of_free}],
    receive
	{Port, {data, [?SYSTEM_MEM_SHOW_END]}} ->
	    Accum;
	{Port, {data, [Tag]}} ->
	    case lists:keysearch(Tag, 1, Tab) of
		{value, {Tag, ATag}} ->
		    collect_sysmem(ATag, Port,Accum);
		_ ->
		    {error, port_protocol_error}
	    end;
	_ ->
	    {error, port_protocol_error}
    end.

collect_sysmem(ATag, Port, Accum) ->
    receive
	{Port, {data, Value}} ->
	    collect_sysmem(Port, [{ATag, list_to_integer(Value)} | Accum]);
	_ ->
	    {error, port_protocol_error}
    end.

process_memory(Pid) ->
    case process_info(Pid, stack_size) of
	{stack_size, StackWords} ->
	    case process_info(Pid, heap_size) of
		{heap_size, HeapWords} ->
		    words_to_bytes(StackWords + HeapWords);
		_ ->
		    undefined
	    end;
	_ ->
	    undefined
    end.

words_to_bytes(X) -> 4*X.

minutes_to_ms(Minutes) -> trunc(60000*Minutes).

get_timeout() ->
    case application:get_env(os_mon, memory_check_interval) of
	{ok, Value} -> minutes_to_ms(Value);
	_ -> minutes_to_ms(1)
    end.

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

format_status(Opt, [PDict, #state{timeout = Timeout, mem_usage = MemUsage,
				  worst_mem_user = WorstMemUser}]) ->
    {Allocated, Total} = MemUsage,
    {Pid, Mem} = WorstMemUser,
    [{data, [{"Timeout", Timeout}]},
     {items, {"Memory Usage", [{"Allocated", Allocated}, {"Total", Total}]}},
     {items, {"Worst Memory User", [{"Pid", Pid}, {"Memory", Mem}]}}].
