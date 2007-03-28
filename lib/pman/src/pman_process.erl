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
%%%----------------------------------------------------------------------
%%% Purpose : A front-end to the erlang:process_info() functions, that
%%%           can handle processes on different nodes in a transparent way. 
%%%	      Also some convenience functions for process info, as well
%%%	      as some application specific functions for process 
%%%           classification.
%%%----------------------------------------------------------------------

-module(pman_process).

%%-compile(export_all).
-export([pinfo/1,
	 pinfo/2,
	 pinfo_notag/2,
	 r_processes/1,
	 function_info/1,
	 current_module/1,
	 psize/1,
	 is_running/1,
	 is_pid_or_shell/1,
	 get_pid/1,
	 is_system_process/1,
	 is_hidden_by_module/2,
	 is_started_by/2,
	 is_registered_as/2,
	 initial_call/1,
	 msg/1,
	 get_name/1,
	 mk_procname_pairs/1
	]).


%% List of module:function/arity calls that will make the caller a 
%% "SYSTEM"-process.
%% 

-define(SYSTEM_INIT_CALLS,
	[{init, boot, 1},
	 {rpc,init,1},
	 {file,init,1},
	 {code,init,1},
	 {global,init,1},
	 {kernel_config,init,1},

	 %% GS processes
	 {gs_frontend,init,1},
	 {gstk,init,1},
	 {gstk,worker_init,1},
	 {gstk_port_handler,init,1},
	 {gstk_port_handler,init_nt,1},

	 %% PMAN processes
	 {pman_main,init,2},
	 {pman_shell,init,1},
	 {pman_shell, safe_init,2},
	 {pman_shell,internal,3},
	 {pman_buf_converter,init,2},
	 {pman_buf_buffer,init,1},
	 {pman_buf_printer,init,2},

	 {shell,server,0},
	 {shell,server,1},
	 {shell,evaluator,3},
	 {supervisor, kernel,1},
	 {erl_prim_loader, start_it,4},

	 %% Stuf started for distributed nodes
	 {net_kernel, ticker,2},
	 {inet_tcp_dist,accept_loop,2},
	 

	 %% Misc

	 {io, wait_io_reply,2},
	 {group, server,2},
	 {user_drv,server,2},
	 {application_master, start_it,4}

	]).

%% List of module:function/arity calls that will make the executing
%% process a "SYSTEM"-process.

-define(SYSTEM_RUNNING_CALLS,
	[
	 {application_master,main_loop,2},
	 {inet_tcp, sync_cmd, 3},
	 {inet_tcp,listen_loop,4},
	 {inet_tcp,socket_loop,3}
	 ]).





%% List of registered name that will make a prodcess a "SYSTEM"-process 
-define(SYSTEM_REGISTERED_NAMES,
	[
	 alarm_handler,
	 application_controller,
	 appmon_info,
	 auth,
	 coast_server,
	 code_server,
	 cover_server,
	 cover_server_001,
	 dbg,
	 dets,
	 disk_log_server,
	 disk_log_sup,
	 erl_epmd,
	 erl_prim_loader,
	 error_logger,
	 eva_log_sup,
	 eva_server,
	 eva_sup,
 	 file_server,
	 global_group,
	 global_group_check,
	 global_name_server,
	 gs_frontend,
	 heart,
	 help_main,
	 inet_db,
	 init,
	 int_db,
	 interpret,
	 jive_server,
	 kernel_safe_sup,
	 kernel_sup,
	 log_server,
	 mandel_server,
	 mesh_sup,
	 mesh_server,
	 mnesia_checkpoint_sup,
	 mnesia_dumper,
	 mnesia_event,
	 mnesia_fallback,
	 mnesia_init,
	 mnesia_kernel_sup,
	 mnesia_late_loader,
	 mnesia_locker,
	 mnesia_monitor,
	 mnesia_recover,
	 mnesia_snmp_sup,
	 mnesia_subscr,
	 mnesia_sup,
	 mnesia_tm,
	 net_kernel,
	 net_sup,
	 overload,
	 perfmon_sampler,
	 pxw_server,
	 release_handler,
	 rex,
	 rsh_starter,
	 sasl_safe_sup,
	 sasl_sup,
	 snmp_agent_sup,
	 snmp_local_db,
	 snmp_master_agent,
	 snmp_misc_sup,
	 snmp_note_store,
	 snmp_supervisor,
	 snmp_symbolic_store,
	 socket,
	 sounder,
	 ssl_socket,
	 take_over_monitor,
	 timer_server,
	 toolbar,
	 tk,
	 udp_server,
	 user,
	 winshell_controller,
	 xerl_copy,
	 xerl_monitor
	]).
	 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pinfo/1 - A version of process_info/1 that handles processes 
%%   on different nodes.
%%
%% Arguments:
%%  Pid		Process identifier, either as Pid, or {_, Pid}
%% 
%% Returns: 
%%   Whatever process_info/1 would return. I.e. ususally a tuple
%%   {Key, Value}
%%
%% Non local exits:
%% Exits with  the following reasons:
%%   dead	When the process is not running
%%   badrpc	When a process_info performed with RPC to
%%              another node fails. 
%%

%%Handle internal process format
pinfo({_,Pid}) ->
    pinfo(Pid);

pinfo(P) when node(P) == node() ->		%On same node
     case process_info(P) of
	 undefined ->
	     exit(dead);
	 Info -> Info
     end;

pinfo(P) ->					%On different node
    case rpc:call(node(P), erlang, process_info, [P]) of
	{badrpc, _} ->
	    exit(badrpc);
	undefined ->				%(???) Does this ever happen
	    exit(dead);
	Info -> Info
    end.

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pinfo/2 - A version of process_info/2 that handles processes
%% on different	nodes.
%%
%% Arguments:
%%   Process	Process identifier, either as Pid or {_, Pid}
%%   Item	One of the valid keywords for erlang:process_info
%% 
%% Returns:
%%   Whatever process_info/2 would return
%%
%% Non local exits:
%% Exits with the following the following reasons:
%%   dead	When the process is not running
%%   badrpc	When a process_info performed with RPC to
%%              another node fails. 
%%   

%Handle internal process format
pinfo({_,Pid},Item) ->				
    pinfo(Pid,Item);

pinfo(P, Item) when node(P) == node() ->	%On same node
    case erlang:process_info(P, Item) of
	undefined ->
	    exit(dead);
	Info -> Info
    end;

pinfo(P, Item) ->				%On different node
    case rpc:call(node(P), erlang, process_info, [P, Item]) of
	{badrpc, _} ->
	    exit(badrpc);
	undefined ->				%(???) Does this ever happen
	    exit(dead);				
	Info -> Info
    end.

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pinfo_notag/2 - Returns the actual value of the specified item
%%   by extracting it from the process_info return tuple
%%
%% Arguments
%%  Pid		Process identifier, either as Pid or {_, Pid}
%%  Item	One of the valid keywords for erlang:process_info
%%
%% Non local exits:
%% Exits with the following the following reasons:
%%   dead	When the process is not running
%%   badrpc	When a process_info performed with RPC to
%%              another node fails. 
%%   


pinfo_notag(Pid, Item) ->
    {_Tag, Data} = pinfo(Pid, Item),
    Data.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function_info/1 - Returns the initial function for the specified
%%   process. 
%%
%% Arguments:
%%  Pid		The Pid of the process
%%
%% Returns:
%%  A tuple on the form {M, F, A} that indicates (as good as possible)
%%  the function that started the specified process.
%%
%% Non local exits:
%% Exits with the following the following reasons:
%%   dead	When the process is not running
%%   badrpc	When a process_info performed with RPC to
%%              another node fails. 
%%   
%%  

function_info(Pid) ->
    case pinfo_notag(Pid, current_function) of
	%% Crap information, show intitial call instead
	0 ->
	    pinfo(Pid, initial_call);
	%% Crap information, show initial call instead.
	current_function ->
	    pinfo(Pid, initial_call);
	
	{Module,Function,Arity} ->
	    {Module, Function, Arity}
    end.


current_module(Pid) ->
    element(1,pinfo_notag(Pid, current_function)).

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% psize/1 - Returns the total process size.
%%
%% Arguments:
%%  P		Process specifier
%%
%% Returns:
%%  Total process size (stack + heap)
%%

psize(P) ->
    Stack = pinfo(P, stack_size),
    Heap = pinfo(P, heap_size),
    case {Heap,Stack} of
	{undefined, undefined} -> 0;
	{undefined, {_,Sz}} -> Sz;
	{{_,Sz}, undefined} -> Sz;
	{{_,Sz0}, {_,Sz1}}  -> Sz0 + Sz1
    end.



%% ---------------------------------------------------------------
%% list of all active  processes on the current node
%%
%% If there is a problem with getting information from a remote 
%% node, an empty list is returned. 
%% 
%% Return: A list of all processes
%% ---------------------------------------------------------------

r_processes(Node) ->
    ordsets:from_list(r_processes1(Node)).


r_processes1(N) ->
    if
	N == node() ->
	    processes();
	true ->
	    case rpc:block_call(N, erlang, processes, []) of
		{badrpc, _} ->
		    [];
		undefined ->			%(???) Does this ever happen
		    [];				
		Info -> Info
	    end
    end.


%%
%% Check if the process is still running
%%

is_running({shell,Pid}) -> 
    case is_running(Pid) of
	{true,Pid} ->
	    {true,{shell,Pid}};
	false ->
	    false
    end;

is_running({link,Pid,_}) -> 
    is_running(Pid);

is_running(Pid) ->
    case is_pid_or_shell(Pid) of
	true ->
	    case (catch pinfo(Pid)) of
		{'EXIT', badrpc} ->
		    false;
		{'EXIT', dead} ->
		    false;
		_Other ->
		    {true,Pid}
	    end;
	false ->
	    false
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_pid_or_shell/1 - Checks if the argument is an (internal) process
%%   identifier, i.e. a Pid, or a tuple {shell, Pid}
%%
%% Arguments:
%%   Specifier		The specifier to check
%%
%% Returns:
%%  true	if it is a process specifier.
%%  false	if it is not a process specifier.
%%

is_pid_or_shell({shell,_Spec}) ->			%(???) Check the Pid ?
    true;

is_pid_or_shell(P) when is_pid(P) ->
    true;

is_pid_or_shell(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_pid/1 - returns the Pid of the object provided that 
%%   it is a proper process specifier.
%%
%% Arguments:
%%   Object	A process specifier
%%
%% Returns:
%%   The Pid.

get_pid({shell,Pid}) ->
    Pid;
get_pid(Pid) when is_pid(Pid) ->
    Pid.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_system_process/1 - Checks if a PID is a system process. (???)
%%   This is a prototype version, use file configuration later.
%%
%%	
%% Arguments:
%%  Pid		Process to check
%%
%% Returns
%%   true	if the process is a system process
%%   false	if the process in not a system process
%%
is_system_process(Pid) when is_pid(Pid)->

    
    %% Test if the start specification is a "system start function"
    SystemStartPred =
	fun(StartSpec) ->
		is_started_by(Pid, StartSpec)
	end,

    IsSystemStartSpec = lists:any(SystemStartPred, ?SYSTEM_INIT_CALLS),

    
    %% Test if the registered name is a system registered name.
    SystemNamePred =
	fun(RegName) ->
		is_registered_as(Pid, RegName)
	end,

    IsSystemRegName = lists:any(SystemNamePred, ?SYSTEM_REGISTERED_NAMES),

    %% Test if the running specification is a "system running function"
    SystemRunPred =
	fun(RunSpec) ->
		is_running_in(Pid, RunSpec)
	end,

    IsSystemRunSpec = lists:any(SystemRunPred, ?SYSTEM_RUNNING_CALLS),

    %% If the process is in any of the above categories, it is
    %% a system process.
    IsSystemStartSpec or IsSystemRegName or IsSystemRunSpec.



%% We have to do some assumptions about the initial call.
%% If the initial call is proc_lib:init_p/5 we can find more information
%% calling the function proc_lib:initial_call/1.

%% (???) This appears to rely a bit too much on internal details of 
%%       proc_lib/gen_server/etc. 

initial_call(Pid)  ->
    case pinfo_notag(Pid,initial_call) of
	{proc_lib, init_p, 5} ->
	    proc_lib:translate_initial_call(Pid);
	ICall ->
	    ICall
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_started_by/2

is_started_by(Pid, Funcspec) when is_pid(Pid)->
    case (catch initial_call(Pid)) of
	{'EXIT', _Reason} ->
	    false;
	InitialCall ->
	    InitialCall == Funcspec
    end;
	

is_started_by(_Pid, _Funcspec) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_registered_as/2

is_registered_as(Pid, Funcspec) when is_pid(Pid) ->
    case (catch pman_process:pinfo(Pid, registered_name)) of
	{registered_name, Name} ->
	    Name==Funcspec;
	{'EXIT', dead} ->
	    false;
	_Catchall ->
	    false
    end;

is_registered_as(_Pid, _Funcspec) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_running_in/2

is_running_in(Pid, Funcspec) ->
    case (catch pman_process:pinfo(Pid, current_function )) of
	{current_function, Name} ->
	    Name==Funcspec;
	{'EXIT', dead} ->
	    false;
	_Catchall ->
	    false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% is_hidden_by_module/2 - Checks if a PID to be hidden because
%%    it executes code from a specified module.
%%
%% Arguments:
%%   Pid		Process to check
%%   OSModuleExcluded	Ordset of modules to hide.
%%
%% Returns
%%   true	if the process is not to be hidden by module
%%   false	if the process in to be hidden by module
%%
%% Note: Catches dead-'EXIT' to return only a useful boolean value.
%%

is_hidden_by_module(Pid, OSModuleExcluded) ->
    case (catch pinfo_notag(Pid, current_function)) of
	{'EXIT', dead} ->
	    true;
	{Module, _Function, _Arity} ->
	    ordsets:is_element(Module, OSModuleExcluded);
	_Otherwise ->
	    true
    end.



%% ---------------------------------------------------------------
%% Help functions for the grid function info
%% ---------------------------------------------------------------
msg(P) when is_pid(P) -> 
    case (catch pman_process:pinfo(P, messages)) of
	{'EXIT', undefined} ->
	    0;
	{messages, L} ->
	    length(L)
    end;
msg(_) -> 0.



%% get_name/1 - Returns the registered name of a process
%% 
%% Pid - The Pid to get the registered name for
%%
%%
%% Returns:
%%  The registered name for the Pid, if it is registered. Otherwise it will
%%  return a string with a space.
%%
%% (???) This may be a clever(?) way to save
%%  some error handling in the callers. But it also makes the function 
%%  a bit difficult to reuse by others that need a different error handling
%%  mechanism. 

get_name(P) when is_pid(P) ->
    case (catch pman_process:pinfo(P, registered_name)) of
	{registered_name, Name} ->
	    Name;
	{'EXIT', dead}  ->
	    " ";
	_Catchall ->
	    " "
    end;

get_name(_) -> " ".


%%
%% mk_procname_pairs/1 - returns a list with Pid:s and their "description"
%% 
%% List - a list of Pid:s to create "descriptions" for.
%%
%% Returns:
%% A list of tuples
%%

mk_procname_pairs(List) ->
    lists:reverse(mk_procname_pairs(List, [])).


%%
%% mk_procname_pairs/2 - worker function for mk_procname_pairs/1
%% 
%% List - a list of Pid:s to create "descriptions" for
%% Pairs - the list of pairs sofar
%%
%% Returns:
%% A list of tuples

mk_procname_pairs([], Pairs) ->
    Pairs;
mk_procname_pairs([Pid|PidRest], Pairs) ->
    case (catch mk_proc_selection_entry(Pid)) of
	
	%% Process is dead or process info otherwise unavailable
	{'EXIT', _Reason} ->
	    mk_procname_pairs(PidRest, Pairs);

	{Pid, Value} ->
	    mk_procname_pairs(PidRest, [{Pid, Value}| Pairs])
    end.



%% mk_proc_selection_entry/1 - Returns a tuple with the Pid and it's "descr."
%%
%% Pid - A Pid to return a usefule description for
%%
%% Returns:
%% {Pid, StringDescr} - A Pid, Description tuple where StringDescr is a
%%                      descriptive string.
%%
%% Non local exits:
%% Exits with the following the following reasons:
%%   dead	When the process is not running
%%   badrpc	When a process_info performed with RPC to
%%              another node fails. 
%%   

mk_proc_selection_entry(Pid)->
    {Module, Function, Arity} = function_info(Pid),

    RegName =
	case get_name(Pid) of
	    " " ->
		" ";
	    Name ->
		lists:flatten(io_lib:format(" [~w]", [Name]))
	end,
    



    StringDescr =
	lists:flatten(
	  io_lib:format("~w~s ~w:~w/~w",
			[Pid, RegName, Module, Function, Arity])),
    {Pid, StringDescr}.
    
    
    
