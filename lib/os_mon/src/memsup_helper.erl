%%%----------------------------------------------------------------------
%%% File    : memsup_helper.erl
%%% Author  : Patrik Nyblom <support@erlang.ericsson.se>
%%% Purpose : Scans processes for the worst memory user
%%% Created : 23 May 2001 by Patrik Nyblom <support@erlang.ericsson.se>
%%%----------------------------------------------------------------------

-module(memsup_helper).

%%-compile(export_all).
%%-export([Function/Arity, ...]).

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

terminate(Reason,Pid) ->
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
		       _ -> start_portprogram()
		   end,
	    process_flag(priority, low),
	    loop(Starter, Port);
	Other ->
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

system_terminate(Reason, Parent, _, _Port) ->
    exit(Reason).

system_code_change(Port, _Module, _OldVsn, _Extra) ->
    {ok, Port}. %% Nothing to do in this version.

%%%------------------------------------------------------------------
%%% And the actual logic...
%%%------------------------------------------------------------------
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


process_memory(Pid) ->
    case process_info(Pid,memory) of
	{memory, Words} ->
	    words_to_bytes(Words);
	_ ->
	    undefined
    end.

words_to_bytes(X) -> 4*X.

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
%% Returns: {ok, {Allocated, Total}} | {error, Reason}
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
%% Returns {ok,Allocated,Total}} | {error, Reason}
%%-----------------------------------------------------------------
get_memory_usage_win32(_Port) ->
    [Result|_] = os_mon_sysinfo:get_mem_info(),
    case io_lib:fread("~d~d~d~d~d~d~d", Result) of
	{ok, [MemLoad,TotPhys,AvailPhys,
	      TotPage,AvailPage,TotV,AvailV], RestStr} ->
	    {TotPhys-AvailPhys,TotPhys};
	Reason ->
	    exit({win32_sysinfo_failed, Reason})
    end.

%%-----------------------------------------------------------------
%% get_memory_usage_linux(_Port)
%%-----------------------------------------------------------------
get_memory_usage_linux(_) ->
    {ok,F} = file:open("/proc/meminfo",[read,raw]),
    {ok,D} = file:read(F,1024),
    {ok,[_Headers,MemInfo | _]} = regexp:split(D,"\n"),
    {ok,[_,NMemTotal,NMemUsed],_} = io_lib:fread("~s ~d ~d",MemInfo),
    {NMemUsed,NMemTotal}.

%%-----------------------------------------------------------------
%% get_memory_usage_freebsd(_Port)
%%
%% Look in /usr/include/sys/vmmeter.h for the format of struct vmmeter
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
	{unix,linux} ->
	    {ok,{Alloced,Tot}} = get_memory_usage_linux([]),
	    [{total_memory,Tot},{free_memory, Tot-Alloced},
	     {system_total_memory,Tot}]; % correct unless setrlimit() set
	{unix,freebsd} ->
	    {ok,{Alloced,Tot}} = get_memory_usage_freebsd([]),
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

