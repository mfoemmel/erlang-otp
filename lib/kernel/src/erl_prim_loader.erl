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
%%     $Id $
%%
%%


%% A primary filer, provides two different methods to fetch a file:
%% efile and inet. The efile method is simple communication with a
%% port program.
%%
%% The distribution loading was removed and replaced with
%% inet loading
%%
%% The start_it/4 function initializes a record with callback 
%% functions used to handle the interface functions.
%%


-module(erl_prim_loader).

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)


-include("inet_boot.hrl").

-export([start/3, set_path/1, get_path/0, get_file/1, get_files/2]).
-export([list_dir/1, read_file_info/1, get_cwd/0, get_cwd/1]).

-record(state, 
	{
	 loader,		  % efile | inet
	 hosts = [],              % hosts list (to boot from)
	 id,                      % not used any more?
	 data,                    % data port etc
	 timeout,                 % idle timeout
	 multi_get = false       % true | false
	}).

%% Defines for inet as prim_loader
-define(IDLE_TIMEOUT, 60000).  %% tear connection after 1 minutes
-define(INET_FAMILY, inet).
-define(INET_ADDRESS, {0,0,0,0}).

-ifdef(DEBUG).
-define(dbg(Tag, Data), erlang:display({Tag,Data})).
-else.
-define(dbg(Tag, Data), true).
-endif.

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

-spec(start/3 :: (_, atom() | string(), atom() | [atom()]) -> {'ok',pid()} | {'error',_}).

start(Id, Pgm, Hosts) when is_atom(Hosts) ->
    start(Id, Pgm, [Hosts]);
start(Id, Pgm0, Hosts) ->
    Pgm = if
	      is_atom(Pgm0) ->
		  atom_to_list(Pgm0);
	      true ->
		  Pgm0
	  end,
    Self = self(),
    Pid = spawn_link(fun() -> start_it(Pgm, Id, Self, Hosts) end),
    register(erl_prim_loader, Pid),
    receive
	{Pid,ok} ->
	    {ok,Pid};
	{'EXIT',Pid,Reason} ->
	    {error,Reason}
    end.

start_it("ose_inet", Id, Pid, Hosts) ->
    %% Setup reserved port for ose_inet driver (only OSE)
    case catch erlang:open_port({spawn,ose_inet},[binary]) of
	{'EXIT',Why} ->
	    ?dbg(ose_inet_port_open_fail, Why),
	    Why;
	OseInetPort ->
	    ?dbg(ose_inet_port, OseInetPort),
	    OseInetPort
    end,
    start_it("inet", Id, Pid, Hosts);

%% Hosts must be a list on form ['1.2.3.4' ...]
start_it("inet", Id, Pid, Hosts) ->
    process_flag(trap_exit, true),
    ?dbg(inet, {Id,Pid,Hosts}),
    AL = ipv4_list(Hosts),
    ?dbg(addresses, AL),
    {ok,Tcp} = find_master(AL),
    init_ack(Pid),
    State = #state {
                    loader = inet,
		    hosts = AL,
		    id = Id,
		    data = Tcp,
		    timeout = ?IDLE_TIMEOUT
		   },
    loop(State, Pid, []);

start_it("efile", Id, Pid, _Hosts) ->
    process_flag(trap_exit, true),
    {ok, Port} = prim_file:open([binary]),
    init_ack(Pid),
    MultiGet = case erlang:system_info(thread_pool_size) of
		   0 -> false;
		   _ -> true
	       end,
    State = #state {
                    loader = efile,
		    id = Id,
		    data = Port,
                    timeout = infinity,
                    multi_get = MultiGet
		    },
    loop(State, Pid, []).

init_ack(Pid) ->
    Pid ! {self(),ok}.

%% -> ok
-spec(set_path/1 :: ([string()]) -> 'ok').

set_path(Paths) when is_list(Paths) ->
    request({set_path,Paths}).

%% -> {ok,Paths}
-spec(get_path/0 :: () -> {'ok',[string()]}).

get_path() ->
    request({get_path,[]}).

%% -> {ok,BinFile,File} | error
-spec(get_file/1 :: (atom() | string()) -> {'ok',binary(),string()} | 'error').

get_file(File) when is_atom(File) ->
    get_file(atom_to_list(File));
get_file(File) ->
    check_file_result(get_file, File, request({get_file,File})).

%% -> ok | {error,Module}
get_files(ModFiles, Fun) ->
    case request({get_files,{ModFiles,Fun}}) of
	E = {error,_M} ->
	    E;
        {error,Reason,M} ->
	    check_file_result(get_files, M, {error,Reason}),
	    {error,M};
	ok ->
	    ok
    end.

%% -> {ok,List} | error
-spec(list_dir/1 :: (string()) -> {'ok',[string()]} | 'error').

list_dir(Dir) ->
    check_file_result(list_dir, Dir, request({list_dir,Dir})).

%% -> {ok,Info} | error
-spec(read_file_info/1 :: (string()) -> {'ok',tuple()} | 'error').

read_file_info(Elem) ->
    check_file_result(read_file_info, Elem, request({read_file_info,Elem})).

%% -> {ok,Cwd} | error
-spec(get_cwd/0 :: () -> {'ok',string()} | 'error').

get_cwd() ->
    check_file_result(get_cwd, [], request({get_cwd,[]})).

%% -> {ok,Cwd} | error
-spec(get_cwd/1 :: (string()) -> {'ok',string()} | 'error').

get_cwd(Drive) ->
    check_file_result(get_cwd, Drive, request({get_cwd,[Drive]})).

request(Req) ->
    Loader = whereis(erl_prim_loader),
    Loader ! {self(),Req},
    receive
	{Loader,Res} ->
	    Res;
	{'EXIT',Loader,_What} ->
	    error
    end.

check_file_result(_, _, {error,enoent}) ->
    error;
check_file_result(_, _, {error,enotdir}) ->
    error;
check_file_result(Func, Target, {error,Reason}) ->   
    case (catch atom_to_list(Reason)) of
	{'EXIT',_} ->				% exit trapped
	    error;
	Errno ->				% errno
	    Process = case process_info(self(), registered_name) of
			  {registered_name,R} -> 
			      "Process: " ++ atom_to_list(R) ++ ".";
			  _ -> 
			      ""
		      end,
	    TargetStr =
		if is_atom(Target) -> atom_to_list(Target);
		   is_list(Target) -> Target;
		   true -> []
		end,
	    Report = 
		case TargetStr of
		    [] ->
			"File operation error: " ++ Errno ++ ". " ++
			"Function: " ++ atom_to_list(Func) ++ ". " ++ Process;
		    _ ->
			"File operation error: " ++ Errno ++ ". " ++
			"Target: " ++ TargetStr ++ ". " ++
			"Function: " ++ atom_to_list(Func) ++ ". " ++ Process
		end,
	    %% this is equal to calling error_logger:error_report/1 which
	    %% we don't want to do from code_server during system boot
	    error_logger ! {notify,{error_report,group_leader(),
				    {self(),std_error,Report}}},
	    error
    end;
check_file_result(_, _, Other) ->
    Other.

%%% --------------------------------------------------------
%%% The main loop.
%%% --------------------------------------------------------

loop(State, Parent, Paths) ->
    receive
	{Pid,Req} when is_pid(Pid) ->
	    {Resp,State2,Paths2} =
		case Req of
		    {set_path,NewPaths} ->
			{ok,State,to_strs(NewPaths)};
		    {get_path,_} ->
			{{ok,Paths},State,Paths};
		    {get_file,File} ->
			{Res,State1} = get_file(State, Paths, File),
			{Res,State1,Paths};
		    {get_files,{ModFiles,Fun}} ->
			{Res,State1} = get_files(State, ModFiles, Paths, Fun),
			{Res,State1,Paths};
		    {list_dir,Dir} ->
			{Res,State1} = loader_list_dir(State, Dir),
			{Res,State1,Paths};
		    {read_file_info,Elem} ->
			{Res,State1} = loader_read_file_info(State, Elem),
			{Res,State1,Paths};
		    {get_cwd,[]} ->
			{Res,State1} = loader_get_cwd(State, []),
			{Res,State1,Paths};
		    {get_cwd,[_]=Args} ->
			{Res,State1} = loader_get_cwd(State, Args),
			{Res,State1,Paths};
		    _Other ->
			{ignore,State,Paths}
		end,
	    if Resp =:= ignore -> ok;
	       true -> Pid ! {self(),Resp}
	    end,
	    loop(State2, Parent, Paths2);
	{'EXIT',Parent,W} ->
	    stop(State),
	    exit(W);
	{'EXIT',P,W} ->
	    State1 = exit(State, P, W),
	    loop(State1, Parent, Paths);
	_Message ->
	    loop(State, Parent, Paths)
    after State#state.timeout ->
	    State1 = handle_timeout(State, Parent),
	    loop(State1, Parent, Paths)
    end.

get_files(State = #state{multi_get = true}, ModFiles, Paths, Fun) ->
    case catch multi_get_from_port_efile(State, ModFiles, Paths, Fun) of
	{'EXIT',Reason} -> {{error,Reason},State};
	Res -> Res
    end;
get_files(State, _ModFiles, _Paths, _Fun) ->	% no multi get
    {{error,no_multi_get},State}.
    
get_file(State = #state{loader = efile}, Paths, File) ->
    case catch get_from_port_efile(State, File, Paths) of
	{'EXIT',Reason} -> {{error,Reason},State};
	Res -> Res
    end;
get_file(State = #state{loader = inet}, Paths, File) ->
    case catch get_from_port_inet(State, File, Paths) of
	{'EXIT',Reason} -> {{error,Reason},State};
	Res -> Res
    end.

loader_list_dir(State = #state{loader = efile}, Dir) ->
    case catch efile_list_dir(State, Dir) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end;
loader_list_dir(State = #state{loader = inet}, Dir) ->
    case catch inet_list_dir(State, Dir) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end.

loader_read_file_info(State = #state{loader = efile}, Elem) ->
    case catch efile_read_file_info(State, Elem) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end;
loader_read_file_info(State = #state{loader = inet}, Elem) ->
    case catch inet_read_file_info(State, Elem) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end.

loader_get_cwd(State = #state{loader = efile}, Args) ->
    case catch efile_get_cwd(State, Args) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end;
loader_get_cwd(State = #state{loader = inet}, Args) ->
    case catch inet_get_cwd(State, Args) of
	{'EXIT',Reason} -> {error,Reason};
	Res -> Res
    end.
    
stop(State = #state{loader = efile}) ->
    efile_stop_port(State);
stop(State = #state{loader = inet}) ->
    inet_stop_port(State).

exit(State = #state{loader = efile}, Who, Reason) ->
    efile_exit_port(State, Who, Reason);
exit(State = #state{loader = inet}, Who, Reason) ->
    inet_exit_port(State, Who, Reason).

handle_timeout(State = #state{loader = efile}, Pid) ->
    efile_timeout_handler(State, Pid);
handle_timeout(State = #state{loader = inet}, Pid) ->
    inet_timeout_handler(State, Pid).

%%% --------------------------------------------------------
%%% Functions which handles efile as prim_loader (default).
%%% --------------------------------------------------------

%%% Reading many files in parallel is an optimization. 
%%% See also comment in init.erl.

%% -> {ok,State} | {{error,Module},State} | {{error,Reason,Module},State}
multi_get_from_port_efile(State, ModFiles, Paths, Fun) ->
    Ref = make_ref(),
    %% More than 200 processes is no gain.
    Max = min(200, erlang:system_info(thread_pool_size)),
    get_files(ModFiles, 0, Max, State, Paths, Fun, Ref, ok).

get_files([MF | MFs], Out, Max, State, Paths, Fun, Ref, Ret) when Out < Max ->
    Self = self(),
    _Pid = spawn(fun() -> par_get_file(Ref, State, MF, Paths, Self, Fun) end),
    get_files(MFs, Out+1, Max, State, Paths, Fun, Ref, Ret);
get_files(MFs, Out, Max, _State, Paths, Fun, Ref, Ret) when Out > 0 ->
    receive 
	{Ref, ok, State1} ->
	    get_files(MFs, Out-1, Max, State1, Paths, Fun, Ref, Ret);
	{Ref, {error,_Mod} = Error, State1} ->
	    get_files(MFs, Out-1, Max, State1, Paths, Fun, Ref, Error);
	{Ref, MF, {error,emfile,State1}} ->
            %% Max can take negative values. Out cannot.
	    get_files([MF | MFs], Out-1, Max-1, State1, Paths, Fun, Ref, Ret);
	{Ref, {M,_F}, {error,Error,State1}} -> 
	    get_files(MFs, Out-1, 0, State1, Paths, Fun, Ref, {error,Error,M})
    end;
get_files(_MFs, 0, _Max, State, _Paths, _Fun, _Ref, Ret) ->
    {Ret,State}.

par_get_file(Ref, State, {Mod,File} = MF, Paths, Pid, Fun) ->
    %% One port for each file read in "parallel":
    case prim_file:open([binary]) of
	{ok, Port} ->
	    Port0 = State#state.data,
	    State1 = State#state{data = Port},
	    R = case get_from_port_efile1(State1, File, Paths) of
		    {error,_,_} = Error -> 
			{Ref,MF,Error};
		    {{ok,BinFile,Full},State2} -> 
			%% Fun(...) -> ok | {error,Mod}
			{Ref,Fun(Mod, BinFile, Full),State2#state{data=Port0}}
		end,
            prim_file:close(Port),
            Pid ! R;
        {error, Error} ->
            Pid ! {Ref,MF,{error,Error,State}}
    end.

%% -> {{ok,BinFile,File},State} | {{error,Reason},State}
get_from_port_efile(State, File, Paths) ->
    case get_from_port_efile1(State, File, Paths) of
	{error,Reason,State1} ->
	    {{error,Reason},State1};
	Res ->
	    Res
    end.

get_from_port_efile1(State, File, Paths) ->
    case absolute_filename(File) of
	true ->					% get absolute file name.
	    get_from_port_efile(File, State);
	false when Paths =:= [] ->		% get plain file name.
	    get_from_port_efile(File, State);
	false ->				% use paths.
	    get_from_port_efile2(File, Paths, State)
    end.

get_from_port_efile2(File, [P | Paths], State) ->
    case get_from_port_efile(concat([P,"/",File]), State) of
	{error,Reason,State1} when Reason =/= emfile ->
	    case Paths of
		[] ->				% return last error
		    {error,Reason,State1};
		_ ->				% try more paths
		    get_from_port_efile2(File, Paths, State1)
	    end;
	Result -> Result
    end;
get_from_port_efile2(_File, [], State) ->
    {error,enoent,State}.

get_from_port_efile(File, #state{data = Port} = State) ->
    case prim_file:read_file(Port, File) of
	{error,port_died} ->
	    exit('prim_load port died');
	{error,Error} ->
	    {error,Error,State};
	{ok,BinFile} ->
	    {{ok,BinFile,File},State}
    end.

%% -> {{ok,List},State} | {{error,Reason},State}
efile_list_dir(State, Dir) ->
    {prim_file:list_dir(Dir),State}.

%% -> {{ok,Info},State} | {{error,Reason},State}
efile_read_file_info(State, Elem) ->
    {prim_file:read_file_info(Elem),State}.

%% -> {{ok,Cwd},State} | {{error,Reason},State}
efile_get_cwd(State, []) ->
    {prim_file:get_cwd(),State};
efile_get_cwd(State, [Drive]) ->
    {prim_file:get_cwd(Drive),State}.

efile_stop_port(#state{data=Port}=State) ->
    prim_file:close(Port),
    State#state{data=noport}.

efile_exit_port(State, Port, Reason) when State#state.data =:= Port ->
    exit({port_died,Reason});
efile_exit_port(State, _Port, _Reason) ->
    State.

efile_timeout_handler(State, _Pid) ->  State.

%%% --------------------------------------------------------
%%% Functions which handles inet prim_loader
%%% --------------------------------------------------------

%%
%% Connect to a boot master
%% return {ok, Socket}  TCP
%% AL is a list of boot servers (including broadcast addresses)
%%
find_master(AL) ->
    find_master(AL, ?EBOOT_RETRY, ?EBOOT_REQUEST_DELAY, ?EBOOT_SHORT_RETRY_SLEEP, 
	       ?EBOOT_UNSUCCESSFUL_TRIES, ?EBOOT_LONG_RETRY_SLEEP).

find_master(AL, Retry, ReqDelay, SReSleep, Tries, LReSleep) ->
    {ok,U} = ll_udp_open(0),
    find_master(U, Retry, AL, ReqDelay, SReSleep, [], Tries, LReSleep).

%%
%% Master connect loop
%%
find_master(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, Tries, LReSleep) ->
    case find_loop(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, 
		   Tries, LReSleep) of
	[] ->	
	    find_master(U, Retry, AddrL, ReqDelay, SReSleep, Ignore, 
			Tries, LReSleep);
	Servers ->
	    ?dbg(servers, Servers),
	    case connect_master(Servers) of
		{ok, Socket} -> 
		    ll_close(U),
		    {ok, Socket};
		_Error ->
		    find_master(U, Retry, AddrL, ReqDelay, SReSleep, 
				Servers ++ Ignore, Tries, LReSleep)
	    end
    end.

connect_master([{_Prio,IP,Port} | Servers]) ->
    case ll_tcp_connect(0, IP, Port) of
	{ok, S} -> {ok, S};
	_Error -> connect_master(Servers)
    end;
connect_master([]) ->
    {error, ebusy}.

%%
%% Always return a list of boot servers or hang.
%%
find_loop(U, Retry, AL, ReqDelay, SReSleep, Ignore, Tries, LReSleep) ->
    case find_loop(U, Retry, AL, ReqDelay, []) of
	[] ->					% no response from any server
	    erlang:display({erl_prim_loader,'no server found'}), % lifesign
	    Tries1 = if Tries > 0 ->
			     sleep(SReSleep),
			     Tries - 1;
			true ->
			     sleep(LReSleep),
			     0
		     end,
	    find_loop(U, Retry, AL, ReqDelay, SReSleep, Ignore, Tries1, LReSleep);
	Servers ->
	    keysort(1, Servers -- Ignore)
    end.

%% broadcast or send
find_loop(_U, 0, _AL, _Delay, Acc) ->
    Acc;
find_loop(U, Retry, AL, Delay, Acc) ->
    send_all(U, AL, [?EBOOT_REQUEST, erlang:system_info(version)]),
    find_collect(U, Retry-1, AL, Delay, Acc).

find_collect(U,Retry,AL,Delay,Acc) ->
    receive
	{udp, U, IP, _Port, [$E,$B,$O,$O,$T,$R,Priority,T1,T0 | _Version]} ->
	    Elem = {Priority,IP,T1*256+T0},
	    ?dbg(got, Elem),
	    case member(Elem, Acc) of
		false  -> find_collect(U, Retry, AL, Delay, [Elem | Acc]);
		true -> find_collect(U, Retry, AL, Delay, Acc)
	    end;
	_Garbage ->
	    ?dbg(collect_garbage, _Garbage),
	    find_collect(U, Retry, AL, Delay, Acc)
	    
    after Delay ->
	    ?dbg(collected, Acc),
	    case keymember(0, 1, Acc) of  %% got high priority server?
		true -> Acc;
		false -> find_loop(U, Retry, AL, Delay, Acc)
	    end
    end.

    
sleep(Time) ->
    receive after Time -> ok end.

inet_exit_port(State, Port, _Reason) when State#state.data =:= Port ->
    State#state { data = noport, timeout = infinity };
inet_exit_port(State, _, _) ->
    State.


inet_timeout_handler(State, _Pid) ->
    Tcp = State#state.data,
    if is_port(Tcp) -> ll_close(Tcp);
       true -> ok
    end,
    State#state { timeout = infinity, data = noport }.

%% -> {{ok,BinFile,Tag},State} | {{error,Reason},State}
get_from_port_inet(State, File, Paths) ->
    case absolute_filename(File) of
	true ->					% get absolute file name.
	    inet_send_and_rcv({get,File}, File, State);
	false when Paths =:= [] ->		% get plain file name.
	    inet_send_and_rcv({get,File}, File, State);
	false ->				% use paths.
	    get_from_port_inet1(File, Paths, State)
    end.

get_from_port_inet1(File, [P | Paths], State) ->
    File1 = concat([P,"/",File]),
    case inet_send_and_rcv({get,File1}, File1, State) of
	{{error,Reason},State1} ->
	    case Paths of
		[] ->				% return last error
		    {{error,Reason},State1};
		_ ->				% try more paths	    
		    get_from_port_inet1(File, Paths, State1)
	    end;
	Result -> Result
    end;
get_from_port_inet1(_File, [], State) ->
    {{error,file_not_found},State}.

inet_send_and_rcv(Msg, Tag, State) when State#state.data =:= noport ->
    {ok,Tcp} = find_master(State#state.hosts),     %% reconnect
    inet_send_and_rcv(Msg, Tag, State#state { data = Tcp,
					      timeout = ?IDLE_TIMEOUT });
inet_send_and_rcv(Msg, Tag, #state{data=Tcp,timeout=Timeout}=State) ->
    prim_inet:send(Tcp, term_to_binary(Msg)),
    receive
	{tcp,Tcp,BinMsg} ->
	    case catch binary_to_term(BinMsg) of
		{get,{ok,BinFile}} ->
		    {{ok,BinFile,Tag},State};
		{_Cmd,Res={ok,_}} ->
		    {Res,State};
		{_Cmd,{error,Error}} ->
		    {{error,Error},State};
		{error,Error} ->
		    {{error,Error},State};
		{'EXIT',Error} ->
		    {{error,Error},State}
	    end;
	{tcp_closed,Tcp} ->
	    %% Ok we must reconnect
	    inet_send_and_rcv(Msg, Tag, State#state { data = noport });
	{tcp_error,Tcp,_Reason} ->
	    %% Ok we must reconnect
	    inet_send_and_rcv(Msg, Tag, inet_stop_port(State));
	{'EXIT', Tcp, _} -> 
	    %% Ok we must reconnect
	    inet_send_and_rcv(Msg, Tag, State#state { data = noport })
    after Timeout ->
	    %% Ok we must reconnect
	    inet_send_and_rcv(Msg, Tag, inet_stop_port(State))
    end.

%% -> {{ok,List},State} | {{error,Reason},State}
inet_list_dir(State, Dir) ->
    inet_send_and_rcv({list_dir,Dir}, list_dir, State).

%% -> {{ok,Info},State} | {{error,Reason},State}
inet_read_file_info(State, Elem) ->
    inet_send_and_rcv({read_file_info,Elem}, read_file_info, State).

%% -> {{ok,Cwd},State} | {{error,Reason},State}
inet_get_cwd(State, []) ->
    inet_send_and_rcv(get_cwd, get_cwd, State);
inet_get_cwd(State, [Drive]) ->
    inet_send_and_rcv({get_cwd,Drive}, get_cwd, State).

inet_stop_port(#state{data=Tcp}=State) ->
    prim_inet:close(Tcp),
    State#state{data=noport}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Direct inet_drv access
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcp_options() ->
    [{mode,binary}, {packet,4}, {active, true}, {deliver,term}].

tcp_timeout() -> 
    15000.

%% options for udp  [list, {broadcast, true}, {active,true}]
udp_options() ->
    [{mode,list}, {active, true}, {deliver,term}, {broadcast,true}].
%%
%% INET version IPv4 addresses
%%
ll_tcp_connect(LocalPort, IP, RemotePort) ->
    case ll_open_set_bind(tcp, ?INET_FAMILY, tcp_options(),
			  ?INET_ADDRESS, LocalPort) of
	{ok,S} ->
	    case prim_inet:connect(S, IP, RemotePort, tcp_timeout()) of
		ok -> {ok, S};
		Error -> port_error(S, Error)
	    end;
	Error -> Error
    end.

%%
%% Open and initialize an udp port for broadcast
%%
ll_udp_open(P) ->
    ll_open_set_bind(udp, ?INET_FAMILY, udp_options(), ?INET_ADDRESS, P).


ll_open_set_bind(Protocol, Family, SOpts, IP, Port) ->
    case prim_inet:open(Protocol, Family) of
	{ok, S} ->
	    case prim_inet:setopts(S, SOpts) of
		ok ->
		    case prim_inet:bind(S, IP, Port) of
			{ok,_} ->
			    {ok, S};
			Error -> port_error(S, Error)
		    end;
		Error -> port_error(S, Error)
	    end;
	Error -> Error
    end.
		    

ll_close(S) ->
    unlink(S),
    exit(S, kill).

port_error(S, Error) ->
    unlink(S),
    prim_inet:close(S),
    Error.
    
%%% --------------------------------------------------------
%%% Misc. functions.
%%% --------------------------------------------------------

%%% Slightly improperly named, since we look for directory
%%% separators anywhere in the string, not just at the front.
absolute_filename(File) ->
    case member($/,File) of
	true -> true;
	false ->
	    case erlang:system_info(os_type) of
		{win32, _} ->
		    case File of
			[_,$:|_] -> true;
			_ -> member($\\, File)
		    end;
		_ -> false
	    end
    end.

send_all(U, [IP | AL], Cmd) ->
    ?dbg(sendto, {U, IP, ?EBOOT_PORT, Cmd}),
    prim_inet:sendto(U, IP, ?EBOOT_PORT, Cmd),
    send_all(U, AL, Cmd);
send_all(_U, [], _) -> ok.

concat([A|T]) when is_atom(A) ->			%Atom
    atom_to_list(A) ++ concat(T);
concat([C|T]) when C >= 0, C =< 255 ->
    [C|concat(T)];
concat([S|T]) ->				%String
    S ++ concat(T);
concat([]) ->
    [].

member(X, [X|_]) -> true;
member(X, [_|Y]) -> member(X, Y);
member(_X, [])    -> false.

keymember(X, I, [Y | _]) when element(I,Y) =:= X -> true;
keymember(X, I, [_ | T]) -> keymember(X, I, T);
keymember(_X, _I, []) -> false.

keysort(I, L) -> keysort(I, L, []).

keysort(I, [X | L], Ls) ->
    keysort(I, L, keyins(X, I, Ls));
keysort(_I, [], Ls) -> Ls.

keyins(X, I, [Y | T]) when X < element(I,Y) -> [X,Y|T];
keyins(X, I, [Y | T]) -> [Y | keyins(X, I, T)];
keyins(X, _I, []) -> [X].

min(X, Y) when X < Y -> X;
min(_X, Y) -> Y.

to_strs([P|Paths]) when is_atom(P) ->
    [atom_to_list(P)|to_strs(Paths)];
to_strs([P|Paths]) when is_list(P) ->
    [P|to_strs(Paths)];
to_strs([_|Paths]) ->
    to_strs(Paths);
to_strs([]) ->
    [].

%% Parse list of ipv4 addresses 
ipv4_list([H | T]) ->
    IPV = if is_atom(H) -> ipv4_address(atom_to_list(H));
	     is_list(H) -> ipv4_address(H);
	     true -> {error,einal}
	  end,
    case IPV of
	{ok,IP} -> [IP | ipv4_list(T)];
	_ -> ipv4_list(T)
    end;
ipv4_list([]) -> [].
    
%%
%% Parse Ipv4 address: d1.d2.d3.d4 (from inet_parse)
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4_address(Cs) ->
    case catch ipv4_addr(Cs, []) of
	{'EXIT',_} -> {error,einval};
	Addr -> {ok,Addr}
    end.

ipv4_addr([C | Cs], IP) when C >= $0, C =< $9 -> ipv4_addr(Cs, C-$0, IP).

ipv4_addr([$.|Cs], N, IP) when N < 256 -> ipv4_addr(Cs, [N|IP]);
ipv4_addr([C|Cs], N, IP) when C >= $0, C =< $9 ->
    ipv4_addr(Cs, N*10 + (C-$0), IP);
ipv4_addr([], D, [C,B,A]) when D < 256 -> {A,B,C,D}.
