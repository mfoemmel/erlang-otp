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

%% API
-export([start_link/0]).

%% sys callbacks
-export([system_continue/3, system_terminate/4, system_code_change/4]).

-export([init/1]).

-include("memsup.hrl").

%%====================================================================
%% This process fetches memory data, either via a port or by reading
%% some file (OS dependent). It makes lots of assumptions about
%% the fetched data and normally just crashes with whatever error
%% reason if something goes wrong.
%%
%% If the process uses a port, and the port terminates unexpectedly,
%% the process exits with reason {memsup_port_died, Reason}.
%% If the port sends unexptected data, the process exits with reason
%% {memsup_port_error, Data}.
%%
%% In the Linux case, where the file used may temporarily be
%% unavailable or at least unreadable, the process tries and retries to
%% read it a number of times (with a small delay in between), and
%% returns the dummy value {0,0} if the read doesn't succeed eventually.
%%====================================================================

%%====================================================================
%% API
%%====================================================================

start_link() ->
    proc_lib:start_link(?MODULE, init, [self()]).

%%====================================================================
%% Process loop
%%====================================================================

init(Parent) ->
    register(?MODULE, self()),
    process_flag(trap_exit,true),
    process_flag(priority, low),

    %% Initiate debug, note however that currently no tracing is
    %% implemented - meaning sys:trace(mem_sup_helper, true) etc. does
    %% not have any effect.
    Deb = sys:debug_options([]),

    Port = case os:type() of
	       {win32, _OSname} -> not_used;
	       {unix, linux} -> not_used;
	       {unix, freebsd} -> not_used;
	       {unix, darwin} -> not_used;
	       _ -> % {OSfamily, OSname} | OSfamily
		   start_portprogram()
	   end,
    
    proc_lib:init_ack(Parent, {ok, self()}),

    loop(Parent, Deb, Port).

loop(Parent, Deb, Port) ->
    receive

	%% System messages
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Deb,Port);

	%% Shutdown order from parent
	{'EXIT', Parent, Reason} ->
	    system_terminate(Reason, Parent, Deb, Port);

	%% Messages from memsup
	{Memsup, collect_proc} ->
	    MemUsage = get_worst_memory_user(),
	    Memsup ! {collected_proc, MemUsage},
	    loop(Parent, Deb, Port);
	{Memsup, collect_sys} ->
	    MemUsage = case os:type() of
			   {win32, _OSname} ->
			       get_memory_usage_win32();
			   {unix, linux} ->
			       get_memory_usage_linux();
			   {unix, freebsd} ->
			       get_memory_usage_freebsd();
			   {unix, darwin} ->
			       get_memory_usage_darwin();
			   _ -> % {OSfamily, OSname} | OSfamily
			       get_memory_usage(Port)
		       end,
	    Memsup ! {collected_sys, MemUsage},
	    loop(Parent, Deb, Port);
	{Memsup, collect_ext_sys} ->
	    MemUsage = case os:type() of
			   {win32, _OSname} ->
			       {Alloc,Tot} = get_memory_usage_win32(),
			       [{total_memory, Tot},
				{free_memory, Tot-Alloc},
				{system_total_memory, Tot}];
			   {unix, linux} ->
			       case get_memory_usage_linux() of
				   {0, 0} ->
				       [];
				   {Alloc,Tot} ->
				       [{total_memory, Tot},
					{free_memory, Tot-Alloc},
					%% corr. unless setrlimit() set
					{system_total_memory, Tot}]
			       end;
			   {unix, freebsd} ->
			       {Alloc,Tot} = get_memory_usage_freebsd(),
			       [{total_memory, Tot},
				{free_memory, Tot-Alloc},
				{system_total_memory, Tot}];
			   {unix, darwin} ->
			       {Alloc,Tot} = get_memory_usage_darwin(),
			       [{total_memory, Tot},
				{free_memory, Tot-Alloc},
				{system_total_memory, Tot}];
			   _ -> % {OSfamily, OSname} | OSfamily
			       get_system_memory_usage(Port)
		       end,
	    Memsup ! {collected_ext_sys, MemUsage},
	    loop(Parent, Deb, Port);

	%% Port messages
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.

%%====================================================================
%% sys callbacks
%%====================================================================

system_continue(Parent, Deb, Port) ->
    loop(Parent, Deb, Port).

system_terminate(Reason, _Parent, _Debug, Port) ->
    case Port of
	not_used ->
	    ignore;
	_ ->
	    port_close(Port)
    end,
    exit(Reason).

system_code_change(Port, _Module, _OldVsn, _Extra) ->
    {ok, Port}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%--Process memory----------------------------------------------------

%% get_worst_memory_user() -> {Pid, Bytes}
get_worst_memory_user()  ->
    get_worst_memory_user(processes(), self(), 0).

get_worst_memory_user([Pid|Pids], MaxPid, MaxMemBytes) ->
    case process_memory(Pid) of
	undefined ->
	    get_worst_memory_user(Pids, MaxPid, MaxMemBytes);
	MemoryBytes when MemoryBytes>MaxMemBytes ->
	    get_worst_memory_user(Pids, Pid, MemoryBytes);
	_MemoryBytes ->
	    get_worst_memory_user(Pids, MaxPid, MaxMemBytes)
    end;
get_worst_memory_user([], MaxPid, MaxMemBytes) ->
    {MaxPid, MaxMemBytes}.

process_memory(Pid) ->
    case process_info(Pid, memory) of
	{memory, Bytes} ->
	    Bytes;
	undefined -> % Pid must have died
	    undefined
    end.

%%--System memory (no port)-------------------------------------------

%% get_memory_usage_<OS>() -> {Alloc, Total}

%% Win32: Find out how much memory is in use by asking
%% the os_mon_sysinfo process.
get_memory_usage_win32() ->
    [Result|_] = os_mon_sysinfo:get_mem_info(),
    {ok, [_MemLoad, TotPhys, AvailPhys,
	  _TotPage, _AvailPage, _TotV, _AvailV], _RestStr} =
	io_lib:fread("~d~d~d~d~d~d~d", Result),
    {TotPhys-AvailPhys, TotPhys}.

%% Linux: Read pseudo file /proc/meminfo
get_memory_usage_linux() ->
    get_memory_usage_linux(50).

get_memory_usage_linux(N) ->
    Res = os:cmd("cat /proc/meminfo"),
    case get_memory_usage_linux(Res, undef, undef) of
	{MemTotal, MemFree} ->
	    {MemTotal-MemFree, MemTotal};
	error when N==1 ->
	    Str = "OS_MON (memsup) timeout, /proc/meminfo unavailable",
	    error_logger:warning_msg(Str),
	    {0, 0};
	error ->
	    timer:sleep(100),
	    get_memory_usage_linux(N-1)
    end.

get_memory_usage_linux(_Fd, Tot, Free) when is_integer(Tot),
					    is_integer(Free) ->
    {Tot, Free};
get_memory_usage_linux("MemTotal:"++T, _Tot0, Free0) ->
    {ok, [N], Rest} = io_lib:fread("~d", T),
    Tot = N*1024,
    get_memory_usage_linux(skip_to_eol(Rest), Tot, Free0);
get_memory_usage_linux("MemFree:"++T, Tot0, _Free0) ->
    {ok, [N], Rest} = io_lib:fread("~d", T),
    Free = N*1024,
    get_memory_usage_linux(skip_to_eol(Rest), Tot0, Free);
get_memory_usage_linux("", _Tot0, _Free0) ->
    error;
get_memory_usage_linux(Str, Tot0, Free0) ->
    get_memory_usage_linux(skip_to_eol(Str), Tot0, Free0).

%% FreeBSD: Look in /usr/include/sys/vmmeter.h for the format of struct
%% vmmeter
get_memory_usage_freebsd() ->
    PageSize  = freebsd_sysctl("vm.stats.vm.v_page_size"),
    PageCount = freebsd_sysctl("vm.stats.vm.v_page_count"),
    FreeCount = freebsd_sysctl("vm.stats.vm.v_free_count"),
    NMemUsed  = (PageCount - FreeCount) * PageSize,
    NMemTotal = PageCount * PageSize,
    {NMemUsed, NMemTotal}.

freebsd_sysctl(Def) ->
    list_to_integer(os:cmd("/sbin/sysctl -n " ++ Def) -- "\n").

%% Darwin:
%% Uses vm_stat command. This appears to lie about the page size in
%% Mac OS X 10.2.2 - the pages given are based on 4000 bytes, but
%% the vm_stat command tells us that it is 4096...
get_memory_usage_darwin() ->
    Str = os:cmd("/usr/bin/vm_stat"),
    {ok, [Free],Str2} = io_lib:fread("Pages free:~d.",skip_to_eol(Str)),
    {ok, [Active],Str3} =
	io_lib:fread("Pages active:~d.", skip_to_eol(Str2)),
    {ok, [Inactive],Str4} =
	io_lib:fread("Pages inactive:~d.", skip_to_eol(Str3)),
    {ok, [Wired],_} =
	io_lib:fread("Pages wired down:~d.", skip_to_eol(Str4)),
    NMemUsed  = (Wired + Active + Inactive) * 4000,
    NMemTotal = NMemUsed + Free * 4000,
    {NMemUsed,NMemTotal}.

skip_to_eol([]) ->
    [];
skip_to_eol([$\n | T]) ->
    T;
skip_to_eol([_ | T]) ->
    skip_to_eol(T).

%%--System memory (using port)----------------------------------------

start_portprogram() ->
    Pgm = code:priv_dir("os_mon") ++ "/bin/memsup",
    Opts = [{packet, 1}],
    open_port({spawn, Pgm}, Opts).


%% get_memory_usage(Port) -> {Allocated, Total}
get_memory_usage(Port) ->
    Port ! {self(), {command, [?MEM_SHOW]}},
    receive
	{Port, {data, Alloc}} ->
	    get_total(Port, 256 * list_to_integer(Alloc));
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.

get_total(Port, Alloc) ->
    receive
	{Port, {data, Total}} ->
	    {Alloc, 256 * list_to_integer(Total)};
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.

%% get_system_memory_usage(Port) -> [{Tag, Bytes}]
%% The (current) possible Tag values are:
%% total_memory: The total memory available to Erlang.
%% free_memory: The amount of unused memory in the Erlang "pool".
%% system_total_memory: The amount of memory available to the whole
%%                system (often equal to total_memory).
%% largest_free: The size of the largest contigous free block.
%% number_of_free: The number of blocks in the free list.
%%-----------------------------------------------------------------
get_system_memory_usage(Port) ->
    Port ! {self(), {command, [?SYSTEM_MEM_SHOW]}},
    collect_sysmem(Port, []).

collect_sysmem(Port, Accum) ->
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
		    collect_sysmem(ATag, Port, Accum);
		_ ->
		    exit({memsup_port_error, {Port,{data,[Tag]}}})
	    end;
	{Port, Else} ->
	    exit({memsup_port_error, {Port,Else}});
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.

collect_sysmem(ATag, Port, Accum) ->
    receive
	{Port, {data, Value}} ->
	    collect_sysmem(Port, [{ATag,list_to_integer(Value)}|Accum]);
	{'EXIT', Port, Reason} ->
	    exit({memsup_port_died, Reason})
    end.
