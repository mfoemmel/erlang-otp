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
%%
%%           New initial version of init.
%% Booting from a script. The script is fetched either from
%% a local file or distributed from another erlang node.
%% 
%% Flags:
%%        -id Identity   : identity of the system.
%%        -boot File     : Absolute file name of the boot script.
%%        -boot_var Var Value
%%                       : $Var in the boot script is expanded to
%%                         Value.
%%        -loader LoaderMethod
%%                       : efile, inet
%%                         (Optional - default efile)
%%        -hosts [Node]  : List of hosts from which we can boot.
%%                         (Mandatory if -loader inet)
%%        -mode embedded : Load all modules at startup, no automatic
%%                         loading
%%        -mode interactive : Auto. load modules (default system behaviour).
%%        -path          : Override path in bootfile.
%%        -pa Path+      : Add my own paths first.
%%        -pz Path+      : Add my own paths last.
%%        -s             : Start own processes.
%% 
%%
%% 

-module(init).
-export([restart/0,reboot/0,stop/0,get_args/0,
	 get_status/0,boot/1,get_arguments/0,
	 get_argument/1,script_id/0]).

% internal exports
-export([do_boot/3,fetch_loaded/0,ensure_loaded/1,timer/1,
	 make_permanent/2]).

% old interface functions kept for backward compability.
-export([get_flag/1,get_flags/0]).

-record(state, {flags = [],
		args = [],
		start = [],
		kernel = [],
		bootpid,
		status = {starting, starting},
		script_id = [],
		loaded = []}).

get_arguments() ->
    request(get_arguments).

get_argument(Arg) ->
    request({get_argument,Arg}).

script_id() ->
    request(script_id).

get_args() ->
    request(get_args).

get_flag(F) ->  % Old interface
    request({get_flag,F}).

get_flags() ->  % Old interface
    request(get_flags).

get_status() ->
    request(get_status).

fetch_loaded() ->
    request(fetch_loaded).

%% Handle dynamic code loading until the
%% real code_server has been started.
ensure_loaded(Module) ->
    request({ensure_loaded, Module}).

make_permanent(Boot,Config) ->
    request({make_permanent,Boot,Config}).

request(Req) ->
    init ! {self(),Req},
    receive 
	{init,Rep} -> 
	    Rep
    end.

restart() -> init ! {stop,restart},ok.
reboot()  -> init ! {stop,reboot},ok.
stop()    -> init ! {stop,stop},ok.

boot(BootArgs) ->
    register(init, self()),
    process_flag(trap_exit, true),
    {Start,Flags,Args} = parse_boot_args(BootArgs),
    boot(Start,Flags,Args).

boot(Start,Flags,Args) ->
    BootPid = do_boot(Flags,Start),
    State = #state{flags = Flags,
		   args = Args,
		   start = Start,
		   bootpid = BootPid},
    boot_loop(BootPid,State).

%%% Convert a term to a printable string, if possible
to_string(X) when list(X) ->
    X;						% assume it's a string..
to_string(X) when atom(X) ->
    atom_to_list(X);
to_string(X) when pid(X) ->
    pid_to_list(X);
to_string(X) when float(X) ->
    float_to_list(X);
to_string(X) when integer(X) ->
    integer_to_list(X);
to_string(X) ->
    "".						% can't do anything with it

things_to_string([X|Rest]) ->
    " (" ++ to_string(X) ++ ")" ++ things_to_string(Rest);
things_to_string([]) ->
    "".

halt_string(String, List) when list(List) ->
    String ++ things_to_string(List);
%% Just in case someone forgot to listify the argument:
halt_string(String, List) ->
    String ++ things_to_string([List]).

crash(String, List) ->
    halt(halt_string(String, List)).

%% Status is {InternalStatus,ProvidedStatus}
boot_loop(BootPid, State) ->
    receive
	{BootPid,loaded,ModLoaded} ->
	    Loaded = State#state.loaded,
	    boot_loop(BootPid,State#state{loaded = [ModLoaded|Loaded]});
	{BootPid,started,KernelPid} ->
	    boot_loop(BootPid, new_kernelpid(KernelPid, BootPid, State));
	{BootPid,progress,NewStatus} ->
            {InS,_} = State#state.status,
	    boot_loop(BootPid,State#state{status = {InS,NewStatus}});
	{BootPid,{script_id,Id}} ->
	    boot_loop(BootPid,State#state{script_id = Id});
	{'EXIT',BootPid,normal} ->
            {_,PS} = State#state.status,
	    loop(State#state{status = {started,PS}});
	{'EXIT',BootPid,Reason} ->
	    erlang:display({'init terminating in do_boot',Reason}),
	    crash("init terminating in do_boot", [Reason]);
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt() !
	    boot_loop(BootPid,State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->   %% Fetch and reset initially loaded modules.
	    From ! {init,State#state.loaded},
	    garb_boot_loop(BootPid,State#state{loaded = []});
	{From, {ensure_loaded, Module}} ->
	    {Res, Loaded} = ensure_loaded(Module, State#state.loaded),
	    From ! {init, Res},
	    boot_loop(BootPid,State#state{loaded = Loaded});
	Msg ->
	    boot_loop(BootPid,handle_msg(Msg,State))
    end.

ensure_loaded(Module, Loaded) ->
    File = concat([Module,extension()]),
    case catch load_mod(Module,File) of
	{ok, FullName} ->
	    {{module, Module}, [{Module, FullName}|Loaded]};
	Res ->
	    {Res, Loaded}
    end.

%% Garbage collect all info about initially loaded modules.
%% This information is temporary stored until the code_server
%% is started.
%% We force the garbage collection as the init process holds
%% this information during the initialisation of the system and
%% it will be automatically garbed much later (perhaps not at all
%% if it is not accessed much).

garb_boot_loop(BootPid,State) ->
    garbage_collect(),
    boot_loop(BootPid,State).

new_kernelpid({Name,{ok,Pid}},BootPid,State) when pid(Pid) ->
    link(Pid),
    BootPid ! {self(),ok,Pid},
    Kernel = State#state.kernel,
    State#state{kernel = [{Name,Pid}|Kernel]};
new_kernelpid({Name,ignore},BootPid,State) ->
    BootPid ! {self(),ignore},
    State;
new_kernelpid({Name,What},BootPid,State) ->
    erlang:display({'could not start kernel pid',Name,What}),
    clear_system(BootPid,State),
    crash("could not start kernel pid", [Name, What]).

%% Here is the main loop after the system has booted.

loop(State) ->
    receive
	{'EXIT',Pid,Reason} ->
	    Kernel = State#state.kernel,
	    terminate(Pid,Kernel,Reason), %% If Pid is a Kernel pid, halt() !
	    loop(State);
	{stop,Reason} ->
	    stop(Reason,State);
	{From,fetch_loaded} ->           %% The Loaded info is cleared in
	    Loaded = State#state.loaded, %% boot_loop but is handled here 
	    From ! {init,Loaded},        %% anyway.
	    loop(State);
	{From, {ensure_loaded, _}} ->
	    From ! {init, not_allowed},
	    loop(State);
	Msg ->
	    loop(handle_msg(Msg,State))
    end.

handle_msg(Msg,State0) ->
    case catch do_handle_msg(Msg,State0) of
	{new_state,State} -> State;
	_                 -> State0
    end.

do_handle_msg(Msg,State) ->
    #state{flags = Flags,
	   status = Status,
	   script_id = Sid,
	   args = Args} = State,
    case Msg of
	{From,get_arguments} ->
	    From ! {init,get_arguments(Flags)};
	{From,{get_argument,Arg}} ->
	    From ! {init,get_argument(Arg,Flags)};
	{From,get_status} -> 
	    From ! {init,Status};
	{From,script_id} -> 
	    From ! {init,Sid};
	{From,get_args} ->
	    From ! {init,Args};
	{From,{make_permanent,Boot,Config}} ->
	    {Res,State1} = make_permanent(Boot,Config,Flags,State),
	    From ! {init,Res},
	    {new_state,State1};
	{From,{get_flag,F}} -> % Old interface
	    case search(F,Flags) of
		{value,{F,V}} ->
		    From ! {init,list_to_tuple([F|V])};
		{value,{F}} ->
		    From ! {init,{F}};
		_ -> 
		    From ! {init,notfound}
	    end;
	{From,get_flags} -> % Old interface 
	    From ! {init,Flags};
	X ->
	    case whereis(user) of
		undefined ->
		    catch error_logger ! {info, self(), {self(), X, []}};
		User ->
		    User ! X,
		    ok
	    end
    end.		  

%%% -------------------------------------------------
%%% A new release has been installed and made
%%% permanent.
%%% Both restart/0 and reboot/0 shall startup using
%%% the new release. reboot/0 uses new boot script
%%% and configuration file pointed out externally.
%%% In the restart case we have to set new -boot and
%%% -config arguments.
%%% -------------------------------------------------

make_permanent(Boot,Config,Flags0,State) ->
    case set_flag('-boot',Boot,Flags0) of
	{ok,Flags1} ->
	    case set_flag('-config',Config,Flags1) of
		{ok,Flags} ->
		    {ok,State#state{flags = Flags}};
		Error ->
		    {Error,State}
	    end;
	Error ->
	    {Error,State}
    end.

set_flag(Flag,false,Flags) ->
    {ok,Flags};
set_flag(Flag,Value,Flags) when list(Value) ->
    case catch list_to_atom(Value) of
	{'EXIT',_} ->
	    {error,badarg};
	AValue ->
	    {ok,set_argument(Flags,Flag,AValue)}
    end;
set_flag(_,_,_) ->
    {error,badarg}.

%%% -------------------------------------------------
%%% Stop the system. 
%%% Reason is: restart | reboot | stop
%%% According to reason terminate emulator or restart
%%% system using the same init process again.
%%% -------------------------------------------------

stop(Reason,State) ->
    BootPid = State#state.bootpid,
    {_,Progress} = State#state.status,
    State1 = State#state{status = {stopping, Progress}},
    clear_system(BootPid,State1),
    do_stop(Reason,State1).

do_stop(restart,#state{start = Start, flags = Flags, args = Args}) ->
    boot(Start,Flags,Args);
do_stop(reboot,_) ->
    halt();
do_stop(stop,State) ->
    stop_heart(State),
    halt().

clear_system(BootPid,State) ->
    Heart = get_heart(State#state.kernel),
    shutdown_pids(Heart,BootPid,State),
    unload(Heart).

stop_heart(State) ->
    case get_heart(State#state.kernel) of
	false ->
	    ok;
	Pid ->
	    %% As heart survives a restart the Parent of heart is init.
	    BootPid = self(),
	    %% ignore timeout
	    shutdown_kernel_pid(Pid,BootPid,shutdown,self(),State) 
    end.

shutdown_pids(Heart,BootPid,State) ->
    Timer = shutdown_timer(State#state.flags),
    catch shutdown(State#state.kernel,BootPid,Timer,State),
    kill_all_pids(Heart), % Even the shutdown timer.
    kill_all_ports(Heart),
    flush_timout(Timer).

get_heart([{heart,Pid}|Kernel]) -> Pid;
get_heart([_|Kernel])           -> get_heart(Kernel);
get_heart(_)                    -> false.


shutdown([{heart,Pid}|Kernel],BootPid,Timer,State) ->
    shutdown(Kernel,BootPid,Timer,State);
shutdown([{Name,Pid}|Kernel],BootPid,Timer,State) ->
    shutdown_kernel_pid(Pid,BootPid,shutdown,Timer,State),
    shutdown(Kernel,BootPid,Timer,State);
shutdown(_,_,_,_) ->
    true.


%%
%% A kernel pid must handle the special case message
%% {'EXIT',Parent,Reason} and terminate upon it !!!
shutdown_kernel_pid(Pid,BootPid,kill,_,_) ->
    exit(Pid,kill);
shutdown_kernel_pid(Pid,BootPid,Reason,Timer,State) ->
    Pid ! {'EXIT',BootPid,Reason},
    shutdown_loop(Pid,Timer,State,[]).

%%
%% We have to handle init requests here in case a process
%% performs such a request and cant shutdown (deadlock).
%% Keep all other exit messages in case it was another
%% kernel process. Resend this messages and handle later.
%%
shutdown_loop(Pid,Timer,State,Exits) ->
    receive
	{'EXIT',Pid,_} ->
	    resend(reverse(Exits)),
	    ok;
	{Timer,timeout} ->
	    erlang:display({init,shutdown_timeout}),
	    throw(timeout);
	{stop,_} ->
	    shutdown_loop(Pid,Timer,State,Exits);
	{From,fetch_loaded} ->
	    From ! {init,State#state.loaded},
	    shutdown_loop(Pid,Timer,State,Exits);
	{'EXIT',OtherP,Reason} ->
	    shutdown_loop(Pid,Timer,State,
			  [{'EXIT',OtherP,Reason}|Exits]);
	Msg ->
	    State1 = handle_msg(Msg,State),
	    shutdown_loop(Pid,Timer,State1,Exits)
    end.

resend([ExitMsg|Exits]) ->
    self() ! ExitMsg,
    resend(Exits);
resend(_) ->
    ok.

%%
%% Kill all existing pids in the system (except init and heart)
kill_all_pids(Heart) ->
    case get_pids(Heart) of
	[] ->
	    ok;
	Pids ->
	    kill_em(Pids),
	    kill_all_pids(Heart)  % Continue until all are really killed.
    end.
    
%% All except zombies.
alive_processes() ->
    lists:filter({erlang, is_process_alive}, processes()).

get_pids(Heart) ->
    Pids = alive_processes(),
    delete(Heart,self(),Pids).

delete(Heart,Init,[Heart|Pids]) -> delete(Heart,Init,Pids);
delete(Heart,Init,[Init|Pids])  -> delete(Heart,Init,Pids);
delete(Heart,Init,[Pid|Pids])   -> [Pid|delete(Heart,Init,Pids)];
delete(_,_,[])                  -> [].
    
kill_em([Pid|Pids]) ->
    exit(Pid,kill),
    kill_em(Pids);
kill_em([]) ->
    ok.

%%
%% Kill all existing ports in the system (except the heart port),
%% i.e. ports still existing after all processes have been killed.
kill_all_ports(Heart) ->
    kill_all_ports(Heart,erlang:ports()).

kill_all_ports(Heart,[P|Ps]) ->
    case erlang:port_info(P,connected) of
	{connected,Heart} ->
	    kill_all_ports(Heart,Ps);
	_ ->
	    exit(P,kill),
	    kill_all_ports(Heart,Ps)
    end;
kill_all_ports(_,_) ->
    ok.

unload(false) ->
    do_unload(sub(erlang:pre_loaded(),erlang:loaded()));
unload(_) ->
    do_unload(sub([heart|erlang:pre_loaded()],erlang:loaded())).

do_unload([M|Mods]) ->
    catch erlang:purge_module(M),
    catch erlang:delete_module(M),
    catch erlang:purge_module(M),
    do_unload(Mods);
do_unload([]) ->
    ok.

sub([H|T],L) -> sub(T,del(H,L));
sub([],L)    -> L.
    
del(Item, [Item|T]) -> T;
del(Item, [H|T])    -> [H|del(Item, T)];
del(Item, [])       -> [].

%%% -------------------------------------------------
%%% If the terminated Pid is one of the processes
%%% added to the Kernel, take down the system brutal.
%%% We are not sure that ANYTHING can work anymore,
%%% i.e. halt the system.
%%% Sleep awhile, it is thus possible for the
%%% error_logger (if it is still alive) to write errors
%%% using the simplest method.
%%% -------------------------------------------------

terminate(Pid,Kernel,Reason) ->
    case kernel_pid(Pid,Kernel) of
	{ok,Name} ->
	    sleep(500), %% Flush error printouts !
	    erlang:display({'Kernel pid terminated',Name,Reason}),
	    crash("Kernel pid terminated", [Name, Reason]);
	_ ->
	    false
    end.

kernel_pid(Pid,[{Name,Pid}|_]) ->
    {ok,Name};
kernel_pid(Pid,[_|T]) ->
    kernel_pid(Pid,T);
kernel_pid(_,_) ->
    false.

sleep(T) -> receive after T -> ok end.

%%% -------------------------------------------------
%%% Start the loader. 
%%% The loader shall run for ever !!
%%% -------------------------------------------------

start_prim_loader(Init,Id,Pgm,Nodes,Root,Path,{Pa,Pz}) ->
    case erl_prim_loader:start(Id,Pgm,Nodes) of
	{ok,Pid} when Path == false ->
	    InitPath = append(Pa,["."|Pz]),
	    erl_prim_loader:set_path(InitPath),
	    add_to_kernel(Init,Pid),
	    Pid;
	{ok,Pid} ->
	    erl_prim_loader:set_path(Path),
	    add_to_kernel(Init,Pid),
	    Pid;
	{error,Reason} ->
	    erlang:display({'can not start loader',Reason}),
	    exit(Reason)
    end.

add_to_kernel(Init,Pid) ->
    Init ! {self(),started,{erl_prim_loader,{ok,Pid}}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),
	    ok
    end.

prim_load_flags(Flags) ->
    PortPgm = get_flag('-loader',Flags,efile),
    Hosts = get_flag_list('-hosts', Flags, []),
    Id = get_flag('-id',Flags,none),
    Path = get_flag_list('-path',Flags,false),
    {PortPgm, Hosts, Id, Path}.

%%% -------------------------------------------------
%%% The boot process fetches a boot script and loads
%%% all modules specified and starts spec. processes.
%%% Processes specified with -s are finally started.
%%% -------------------------------------------------

do_boot(Flags,Start) ->
    spawn_link(init,do_boot,[self(),Flags,Start]).

do_boot(Init,Flags,Start) ->
    process_flag(trap_exit,true),
    {Pgm,Nodes,Id,Path} = prim_load_flags(Flags),
    Root = get_flag('-root',Flags),
    PathFls = path_flags(Flags),
    LoadPid = start_prim_loader(Init,Id,Pgm,Nodes,Root,Path,PathFls),
    BootFile = bootfile(Flags,Root),
    BootList = get_boot(BootFile,Root),
    Embedded = get_flag('-mode',Flags,false),
    Deb = get_flag('-init_debug',Flags,false),
    BootVars = get_flag_args('-boot_var',Flags),
    eval_script(BootList,Init,PathFls,{Root,BootVars},Path,
		{true,Embedded},Deb),

    %% To help identifying Purify windows that pop up,
    %% print the node name into the Purify log.
    (catch erlang:info({purify, "Node: " ++ atom_to_list(node())})),

    start_em(Start).

bootfile(Flags,Root) ->
    get_flag('-boot',Flags,concat([Root,"/bin/start"])).

path_flags(Flags) ->
    Pa = append(reverse(get_flag_args('-pa',Flags))),
    Pz = append(get_flag_args('-pz',Flags)),
    {Pa,Pz}.

get_boot(BootFile0,Root) ->
    BootFile = concat([BootFile0,".boot"]),
    case get_boot(BootFile) of
	{ok, CmdList} ->
	    CmdList;
	not_found -> %% Check for default
	    BootF = concat([Root,"/bin/",BootFile]),
	    case get_boot(BootF)  of
		{ok, CmdList} ->
		    CmdList;
		not_found ->
		    exit({'can not get bootfile',list_to_atom(BootFile)});
		_ ->
		    exit({'bootfile format error',list_to_atom(BootF)})
	    end;
	_ ->
	    exit({'bootfile format error',list_to_atom(BootFile)})
    end.
    
get_boot(BootFile) ->
    case erl_prim_loader:get_file(BootFile) of
	{ok,Bin,_} ->
	    case binary_to_term(Bin) of
		{script,Id,CmdList} when list(CmdList) ->
		    init ! {self(),{script_id,Id}}, % ;-)
		    {ok, CmdList};
		_ ->
		    error
	    end;
	_ ->
	    not_found
    end.

%%
%% Eval a boot script.
%% Load modules and start processes.
%% If a start command does not spawn a new process the
%% boot process hangs (we want to ensure syncronicity).
%%

eval_script([{progress,Info}|CfgL],Init,PathFs,Vars,P,Ph,Deb) ->
    debug(Deb,{progress,Info}),
    init ! {self(),progress,Info},
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb);
eval_script([{preLoaded,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb) ->
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb);
eval_script([{path,Path}|CfgL],Init,{Pa,Pz},Vars,false,Ph,Deb) ->
    RealPath = append([Pa,append([fix_path(Path,Vars),Pz])]),
    erl_prim_loader:set_path(RealPath),
    eval_script(CfgL,Init,{Pa,Pz},Vars,false,Ph,Deb);
eval_script([{path,_}|CfgL],Init,PathFs,Vars,P,Ph,Deb) ->
    %% Ignore, use the command line -path flag.
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,embedded},Deb) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{true,embedded},Deb);
eval_script([{kernel_load_completed}|CfgL],Init,PathFs,Vars,P,{_,E},Deb) ->
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E},Deb);
eval_script([{primLoad,Mods}|CfgL],Init,PathFs,Vars,P,{true,E},Deb)
  when list(Mods) ->
    load_modules(Mods),
    eval_script(CfgL,Init,PathFs,Vars,P,{true,E},Deb);
eval_script([{primLoad,Mods}|CfgL],Init,PathFs,Vars,P,{false,E},Deb) ->
    %% Do not load know, code_server does that dynamically !!
    eval_script(CfgL,Init,PathFs,Vars,P,{false,E},Deb);
eval_script([{kernelProcess,Server,{Mod,Fun,Args}}|CfgL],Init,
	    PathFs,Vars,P,Ph,Deb) ->
    debug(Deb,{start,Server}),
    start_in_kernel(Server,Mod,Fun,Args,Init),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb);
eval_script([{apply,{Mod,Fun,Args}}|CfgL],Init,PathFs,Vars,P,Ph,Deb) ->
    debug(Deb,{apply,{Mod,Fun,Args}}),
    apply(Mod,Fun,Args),
    eval_script(CfgL,Init,PathFs,Vars,P,Ph,Deb);
eval_script([],_,_,_,_,_,_) ->
    ok;
eval_script(What,_,_,_,_,_,_) ->
    exit({'unexpected command in bootfile',What}).

debug(false, _) -> ok;
debug(_, T)     -> erlang:display(T).

load_modules([Mod|Mods]) ->
    File = concat([Mod,extension()]),
    {ok,Full} = load_mod(Mod,File),
    init ! {self(),loaded,{Mod,Full}},  %% Tell init about loaded module
    load_modules(Mods);
load_modules([]) ->
    ok.
    
%% For all Paths starting with $ROOT add rootdir and for those
%% starting with $xxx/ expand $xxx to the value supplied with -boot_var !
%% If $xxx can not be expanded this process terminates.

fix_path([Path|Ps], Vars) when atom(Path) ->
    [add_var(atom_to_list(Path), Vars)|fix_path(Ps, Vars)];
fix_path([Path|Ps], Vars) ->
    [add_var(Path, Vars)|fix_path(Ps, Vars)];
fix_path(_, _) ->
    [].

add_var("$ROOT/" ++ Path, {Root,_}) ->
    concat([Root, "/", Path]);
add_var([$$|Path0], {_,VarList}) ->
    {Var,Path} = extract_var(Path0,[]),
    Value = get_var_value(list_to_atom(Var),VarList),
    concat([Value, "/", Path]);
add_var(Path, _)   ->
    Path.

extract_var([$/|Path],Var) -> {reverse(Var),Path};
extract_var([H|T],Var)     -> extract_var(T,[H|Var]);
extract_var([],Var)        -> {reverse(Var),[]}.

%% get_var_value(Var, [Vars]) where Vars == [atom()]
get_var_value(Var,[Vars|VarList]) ->
    case get_var_val(Var,Vars) of
	{ok, Value} ->
	    Value;
	_ ->
	    get_var_value(Var,VarList)
    end;
get_var_value(Var,[]) ->
    exit(list_to_atom(concat(["can not expand \$", Var, " in bootfile"]))).

get_var_val(Var,[Var,Value|_]) -> {ok, Value};
get_var_val(Var,[_,_|Vars])    -> get_var_val(Var,Vars);
get_var_val(_,_)               -> false.

%% Servers that are located in the init kernel is linked
%% and supervised by init.

start_in_kernel(Server,Mod,Fun,Args,Init) ->
    Res = apply(Mod,Fun,Args),
    Init ! {self(),started,{Server,Res}},
    receive
	{Init,ok,Pid} ->
	    unlink(Pid),  %% Just for sure ...
	    ok;
	{Init,ignore} ->
	    ignore
    end.

%% Do start all processes specified at command line using -s !!!
%% Use apply here instead of spawn to ensure syncronicity for
%% those servers that wish to have it so.
%% Disadvantage: any thing started with -s that does not
%% eventually spawn will hang the startup routine.

start_em([S|Tail]) ->
    case whereis(user) of
	undefined -> 
	    ok;
	P when pid(P) ->			%Let's set the group_leader()
	    erlang:group_leader(P, self())
    end,
    start_it(S),
    start_em(Tail);
start_em([]) -> ok.

start_it([]) -> 
    ok;
start_it([M]) ->
    case catch apply(M,start,[]) of
	{'EXIT',Reason} ->
	    exit(Reason);
	Else -> 
	    Else
    end;

start_it([M,F]) ->
    case catch apply(M,F,[]) of
	{'EXIT',Reason} ->
	    exit(Reason);
	Else -> 
	    Else
    end;

start_it([M,F|Args]) ->
    case catch apply(M,F,[Args]) of	%One variable len arg only
	{'EXIT',Reason} ->
	    exit(Reason);
	Else -> 
	    Else
    end.

%%
%% Fetch a module and load it into the system.
%%
load_mod(Mod, File) ->
    case erl_prim_loader:get_file(File) of
	{ok,BinCode,FullName} ->
	    case erlang:module_loaded(Mod) of
		false ->
		    case erlang:load_module(Mod, BinCode) of
			{module,Mod} -> {ok,FullName};
			Other ->
			    exit({'can not load',Mod,Other})
		    end;
		_ ->       % Already loaded, i.e. heart during restart !
		    {ok,FullName}
	    end;
	_ ->
	    exit({'can not load',Mod,get_file})
    end.

%% --------------------------------------------------------
%% If -shutdown_time is specified at the command line
%% this timer will inform the init process that it has to
%% force processes to terminate. It can not be handled
%% sotfly any longer.
%% --------------------------------------------------------

shutdown_timer(Flags) ->
    case get_flag('-shutdown_time',Flags,infinity) of
	infinity ->
	    self();
	Time ->
	    case catch list_to_integer(atom_to_list(Time)) of
		T when integer(T) ->
		    Pid = spawn(init,timer,[T]),
		    receive
			{Pid, started} ->
			    Pid
		    end;
		_ ->
		    self()
	    end
    end.
    
flush_timout(Pid) ->
    receive
	{Pid, timeout} -> true
    after 0            -> true
    end.

timer(T) ->
    init ! {self(), started},
    receive
    after T ->
	init ! {self(), timeout}
    end.
    
%% --------------------------------------------------------
%% Parse the command line arguments and extract things to start, flags
%% and other arguments. We keep the relative of the groups.
%% --------------------------------------------------------

parse_boot_args(Args) ->
    parse_boot_args(Args, [], [], []).

parse_boot_args([B|Bs], Ss, Fs, As) ->
    case check(B) of
	start_arg ->
	    {S,Rest} = get_args(Bs, []),
	    parse_boot_args(Rest, [S|Ss], Fs, As);
	flag ->
	    {F,Rest} = get_args(Bs, []),
	    Fl = case F of
		     []   -> [B];
		     FF   -> [B,FF]
		 end,
	    parse_boot_args(Rest, Ss,  
			    [list_to_tuple(Fl)|Fs], As);
	arg ->
	    parse_boot_args(Bs, Ss, Fs, [B|As]);
	end_args ->
	    parse_boot_args(Bs, Ss, Fs, As)
    end;
parse_boot_args([], Start, Flags, Args) ->
    {reverse(Start),reverse(Flags),reverse(Args)}.

check('-s') -> start_arg;
check('--') -> end_args;
check(X) when atom(X) ->
    case atom_to_list(X) of
	[$-|Rest] -> flag;
	Chars     -> arg			%Even empty atoms
    end;
check(X) -> arg.				%This should never occur

get_args([B|Bs], As) ->
    case check(B) of
	start_arg -> {reverse(As), [B|Bs]};
	end_args -> {reverse(As), Bs};
	flag -> {reverse(As), [B|Bs]};
	arg ->
	    get_args(Bs, [B|As])
    end;
get_args([], As) -> {reverse(As),[]}.

%%
%% Internal get_flag function, with default value.
%% Return: true if flag given without args
%%         atom() if a single arg was given.
%%         list(atom()) if several args was given.
%%
get_flag(F,Flags,Default) ->
    case catch get_flag(F,Flags) of
	{'EXIT',_} ->
	    Default;
	Value ->
	    Value
    end.

get_flag(F,Flags) ->
    case search(F,Flags) of
	{value,{F,[V]}} ->
	    V;
	{value,{F,V}} ->
	    V;
	{value,{F}} -> % Flag given !
	    true;
	_ ->
	    exit(list_to_atom(concat(["no ",F," flag"])))
    end.

%%
%% Internal get_flag function, with default value.
%% Return: list(atom()) 
%%
get_flag_list(F,Flags,Default) ->
    case catch get_flag_list(F,Flags) of
	{'EXIT',_} ->
	    Default;
	Value ->
	    Value
    end.

get_flag_list(F,Flags) ->
    case search(F,Flags) of
	{value,{F,V}} ->
	    V;
	_ ->
	    exit(list_to_atom(concat(["no ",F," flag"])))
    end.

%%
%% Internal get_flag function.
%% Fetch all occurances of flag.
%% Return: [Args,Args,...] where Args ::= list(atom())
%%
get_flag_args(F,Flags) -> get_flag_args(F,Flags,[]).

get_flag_args(F,[{F,V}|Flags],Acc) when list(V) ->
    get_flag_args(F,Flags,[V|Acc]);
get_flag_args(F,[{F,V}|Flags],Acc) ->
    get_flag_args(F,Flags,[[V]|Acc]);
get_flag_args(F,[_|Flags],Acc) ->
    get_flag_args(F,Flags,Acc);
get_flag_args(_,[],Acc) ->
    reverse(Acc).

get_arguments([{F,V}|Flags]) ->
    [$-|Fl] = atom_to_list(F),
    [{list_to_atom(Fl),to_strings(V)}|get_arguments(Flags)];
get_arguments([{F}|Flags]) ->
    [$-|Fl] = atom_to_list(F),
    [{list_to_atom(Fl),[]}|get_arguments(Flags)];
get_arguments([]) ->
    [].

to_strings([H|T]) -> [atom_to_list(H)|to_strings(T)];
to_strings([])    -> [].

get_argument(Arg,Flags) ->
    Args = get_arguments(Flags),
    case get_argument1(Arg,Args) of
	[] ->
	    error;
	Value ->
	    {ok,Value}
    end.

get_argument1(Arg,[{Arg,V}|Args]) ->
    [V|get_argument1(Arg,Args)];
get_argument1(Arg,[_|Args]) ->
    get_argument1(Arg,Args);
get_argument1(_,[]) ->
    [].

set_argument([{Flag,_}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([{Flag}|Flags],Flag,Value) ->
    [{Flag,[Value]}|Flags];
set_argument([Item|Flags],Flag,Value) ->
    [Item|set_argument(Flags,Flag,Value)];
set_argument([],Flag,Value) ->
    [{Flag,[Value]}].

concat([A|T]) when atom(A) ->			%Atom
    append(atom_to_list(A), concat(T));
concat([C|T]) when C >= 0, C =< 255 ->
    [C|concat(T)];
concat([S|T]) ->				%String
    append(S, concat(T));
concat([]) ->
    [].

append(L, Z) -> erlang:append(L, Z).

append([E]) -> E;
append([H|T]) ->
    H ++ append(T);
append([]) -> [].

reverse(X) ->
    reverse(X, []).

reverse([H|T], Y) ->
    reverse(T, [H|Y]);
reverse([], X) -> X.
			
search(Key, [H|T]) when tuple(H), element(1, H) == Key ->
    {value, H};
search(Key, [_|T]) -> search(Key, T);
search(Key, []) -> false.

extension() -> 
    case erlang:info(machine) of
        "JAM" -> ".jam";
        "VEE" -> ".vee";
        "BEAM" -> ".beam"
    end.
