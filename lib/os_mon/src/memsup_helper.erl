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

-module(memsup_helper).

-behaviour(supervisor_bridge).

%%%----------------------------------------------------------------------
%% Supervisor API
-export([start_link/0, init/1, terminate/2]).
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% Export for internal use
%%%----------------------------------------------------------------------
-export([server_init/1]).
%%%----------------------------------------------------------------------
%%% Sys callbacks
%%%----------------------------------------------------------------------
-export([system_code_change/4, system_continue/3, system_terminate/4]).

%%%----------------------------------------------------------------------
%%% The single API and internal main function
%%%----------------------------------------------------------------------

-include("memsup.hrl").


-define(CALL_TIMEOUT, 10000). % Timeout for startup of server
-define(PROCNAME,memsup_helper). % The real process registered name

%%%------------------------------------------------------------------------
%%% Here is the complete supervisor bridge, init, terminate and start_link.
%%%------------------------------------------------------------------------
init([]) -> % Called by supervisor_bridge:start_link
    Pid = spawn_link(?MODULE,server_init,[self()]),
    receive
	{Pid, false} ->
	    ignore;
	{Pid, _} ->
	    {ok, Pid, Pid}
    after ?CALL_TIMEOUT ->
	    {error, {timeout, ?PROCNAME}}
    end.

terminate(_Reason,Pid) ->
    (catch exit(Pid,kill)),
    ok.

start_link() ->
    supervisor_bridge:start_link(?MODULE, []).

%%%-------------------------------------------------------------------------
%%% ...and here's the server
%%%-------------------------------------------------------------------------

server_init(Starter) ->
    case (catch register(?PROCNAME, self())) of
	{'EXIT', {badarg, _}} -> %% Parallel starting, we lost.
	    Starter ! {self(), false};
	true ->
	    Starter ! {self(), true},
	    process_flag(trap_exit,true),
	    Port = case os:type() of
		       {win32,_} -> not_used;
		       {unix,linux} -> not_used;
		       {unix,freebsd} -> not_used;
		       {unix, darwin} -> not_used;
		       _ -> start_portprogram()
		   end,
	    process_flag(priority, low),
	    loop(Starter, Port);
	_Other ->
	    exit({badarg, {?MODULE, init, [Starter]}})
    end.
    
loop(Parent,Port) ->
    receive
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, [], Port);
	{Proc, collect_proc} ->
	    Proc ! {collected_proc, get_worst_memory_user()},
	    loop(Parent, Port);
	{Proc, collect_sys} ->
	    MemUsage = case os:type() of
			   {win32,_} ->
			       get_memory_usage_win32(Port);
			   {unix,linux} ->
			       get_memory_usage_linux(Port);
			   {unix,freebsd} ->
			       get_memory_usage_freebsd(Port);
			   {unix,darwin} ->
			       get_memory_usage_darwin(Port);
			   _ ->
			       get_memory_usage(Port)
		       end,
	    Proc ! {collected_sys, MemUsage},
	    loop(Parent, Port);
	{Proc, collect_ext_sys} ->
	    Proc ! {collected_ext_sys, get_system_memory_usage(Port)},
	    loop(Parent,Port);
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.
%%------------------------------------------------------------------
%% System messages callbacks
%%------------------------------------------------------------------

system_continue(Parent, _, Port) ->
    loop(Parent,Port).

system_terminate(Reason, _Parent, _, _Port) ->
    exit(Reason).

system_code_change(Port, _Module, _OldVsn, _Extra) ->
    {ok, Port}. %% Nothing to do in this version.

%%%------------------------------------------------------------------
%%% And the actual logic...
%%%------------------------------------------------------------------
%%-----------------------------------------------------------------
%% Returns: {Pid, MemoryBytes}
%%-----------------------------------------------------------------

get_worst_memory_user()  ->
    get_worst_memory_user(processes(), self(), 0).

get_worst_memory_user([Pid|Rest], MaxPid, MaxMemBytes) ->
    case process_memory(Pid) of
	undefined ->
	    get_worst_memory_user(Rest, MaxPid, MaxMemBytes);
	MemoryBytes when MemoryBytes > MaxMemBytes ->
	    get_worst_memory_user(Rest, Pid, MemoryBytes);
	_ ->
	    get_worst_memory_user(Rest, MaxPid, MaxMemBytes)
    end;
get_worst_memory_user([], MaxPid, MaxMemBytes) ->
    {MaxPid, MaxMemBytes}.

process_memory(Pid) ->
    case process_info(Pid, memory) of
	{memory, Bytes} ->
	    Bytes;
	_ ->
	    undefined
    end.

%%%----------------------------------------------------------------------------
%%% Port program
%%%----------------------------------------------------------------------------

start_portprogram() ->
    Pgm = code:priv_dir("os_mon") ++ "/bin/memsup",
    Opts = [{packet, 1}],
    open_port({spawn, Pgm}, Opts).

%%-----------------------------------------------------------------
%% Func: get_memory_usage(Port)
%% Purpose: Function which finds out how much memory is in use by
%%          using the external portprogram.
%% Returns: {Allocated, Total}
%%-----------------------------------------------------------------
get_memory_usage(Port) ->
    Port ! {self(), {command, [?MEM_SHOW]}},
    receive
	{Port, {data, Alloc}} ->
	    get_total(Port, 256 * list_to_integer(Alloc));
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.

get_total(Port, Alloc) ->
    receive
	{Port, {data, Total}} ->
	    {Alloc, 256 * list_to_integer(Total)};
	{'EXIT', Port, Reason} ->
	    exit({port_terminated, Reason})
    end.
%%-----------------------------------------------------------------
%% Func: get_memory_usage_win32(_Port)
%% Purpose: Find out how much memory is in use by asking the os_mon_sysinfo
%% process.
%% Returns {Allocated,Total}
%%-----------------------------------------------------------------
get_memory_usage_win32(_Port) ->
    [Result|_] = os_mon_sysinfo:get_mem_info(),
    case io_lib:fread("~d~d~d~d~d~d~d", Result) of
	{ok, [_MemLoad,TotPhys,AvailPhys,
	      _TotPage,_AvailPage,_TotV,_AvailV], _RestStr} ->
	    {TotPhys-AvailPhys,TotPhys};
	Reason ->
	    exit({win32_sysinfo_failed, Reason})
    end.

%%-----------------------------------------------------------------
%% get_memory_usage_linux(_Port)
%% Returns {Allocated,Total}
%%-----------------------------------------------------------------
get_memory_usage_linux(_) ->
    {ok,F} = file:open("/proc/meminfo", [read,read_ahead]),
    try
	case get_memory_usage_linux_1(F, undefined, undefined) of
	    {MemTotal,MemFree} when is_integer(MemTotal), is_integer(MemFree) ->
		{MemTotal-MemFree,MemTotal}
	end
	after
	    file:close(F)
	end.

get_memory_usage_linux_1(_, Tot, Free) when is_integer(Tot), is_integer(Free) ->
    {Tot,Free};
get_memory_usage_linux_1(F, Tot0, Free0) ->
    case io:get_line(F, '') of
	"MemTotal:"++T ->
	    Tot = get_kbytes(T),
	    get_memory_usage_linux_1(F, Tot, Free0);
	"MemFree:"++T ->
	    Free = get_kbytes(T),
	    get_memory_usage_linux_1(F, Tot0, Free);
	Line when is_list(Line) ->
	    get_memory_usage_linux_1(F, Tot0, Free0)
    end.

get_kbytes(Line) ->
    {ok,[N],_}  = io_lib:fread("~d", Line),
    N*1024.

%%-----------------------------------------------------------------
%% get_memory_usage_freebsd(_Port)
%%
%% Look in /usr/include/sys/vmmeter.h for the format of struct vmmeter
%% Returns {Allocated,Total}
%%-----------------------------------------------------------------
get_memory_usage_freebsd(_) ->
    PageSize  = freebsd_sysctl("vm.stats.vm.v_page_size"),
    PageCount = freebsd_sysctl("vm.stats.vm.v_page_count"),
    FreeCount = freebsd_sysctl("vm.stats.vm.v_free_count"),
    NMemUsed  = (PageCount - FreeCount) * PageSize,
    NMemTotal = PageCount * PageSize,
    {NMemUsed,NMemTotal}.

freebsd_sysctl(Def) ->
    list_to_integer(os:cmd("/sbin/sysctl -n " ++ Def) -- "\n").

%%-----------------------------------------------------------------
%% get_memory_usage_darwin(_Port)
%%
%% Uses vm_stat command. This appears to lie about the page size in
%% Mac OS X 10.2.2 - the pages given are based on 4000 bytes, but
%% the vm_stat command tells us that it is 4096...
%% Returns {Allocated,Total}
%%-----------------------------------------------------------------
get_memory_usage_darwin(_) ->
    Str = os:cmd("/usr/bin/vm_stat"),
    {ok, [Free],Str2} = io_lib:fread("Pages free:~d.", skip_to_eol(Str)),
    {ok, [Active],Str3} = io_lib:fread("Pages active:~d.", skip_to_eol(Str2)),
    {ok, [Inactive],Str4} = io_lib:fread("Pages inactive:~d.", skip_to_eol(Str3)),
    {ok, [Wired],_} = io_lib:fread("Pages wired down:~d.", skip_to_eol(Str4)),
    NMemUsed  = (Wired + Active + Inactive) * 4000,
    NMemTotal = NMemUsed + Free * 4000,
    {NMemUsed,NMemTotal}.

skip_to_eol([]) ->
    [];
skip_to_eol([$\n | T]) ->
    T;
skip_to_eol([_ | T]) ->
    skip_to_eol(T).

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
	    {Alloced,Tot} = get_memory_usage_win32(Port),
	    [{total_memory, Tot},
	     {free_memory, Tot - Alloced},
	     {system_total_memory, Tot}];
	{unix,linux} ->
	    {Alloced,Tot} = get_memory_usage_linux([]),
	    [{total_memory,Tot},{free_memory, Tot-Alloced},
	     {system_total_memory,Tot}]; % correct unless setrlimit() set
	{unix,freebsd} ->
	    {Alloced,Tot} = get_memory_usage_freebsd([]),
	    [{total_memory,Tot},{free_memory, Tot-Alloced},
	     {system_total_memory,Tot}];
	{unix,darwin} ->
	    {Alloced,Tot} = get_memory_usage_darwin([]),
	    [{total_memory,Tot},{free_memory, Tot-Alloced},
	     {system_total_memory,Tot}];
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
		    exit({port_protocol_error,{Port, {data, [Tag]}}})
	    end;
	{Port, Else} ->
	    exit({port_protocol_error,{Port, Else}})
    end.

collect_sysmem(ATag, Port, Accum) ->
    receive
	{Port, {data, Value}} ->
	    collect_sysmem(Port, [{ATag, list_to_integer(Value)} | Accum]);
	{Port, Else} ->
	    exit({port_protocol_error,{Port, Else}})
    end.

