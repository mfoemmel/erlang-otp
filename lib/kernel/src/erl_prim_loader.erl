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


%% A primary filer (Ahh, wrong name prim_loader:-).
%% Provides three different methods to fetch a file,
%%  i.e. binary_filer, efile and inet.
%% The binary_filer and efile methods are simple communication
%% with a port program.
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

-record(state, 
	{
	 hosts = [],              % hosts list (to boot from)
	 id,                      % not used any more?
	 get,                     % load fun
	 multi_get,               % load fun (many files)
	 stop,                    % stop fun
	 exit,                    % cleanup fun
	 data,                    % data port etc
	 timeout,                 % idle timeout
	 timeout_handler
	}).

%% Defines for the binary_filer (local) prim_loader.
-define(get_file, $f).
-define(FILE_OK, $y).

%% Defines for inet as prim_loader
-define(inet_get_file, $F).
-define(FILE_INET_OK, $f).
-define(FILE_INET_ERROR, $e).
-define(IDLE_TIMEOUT, 60000).  %% tear connection after 1 minutes
-define(INET_PROTO, inet).

-ifdef(DEBUG).
-define(dbg(Tag, Data), erlang:display({Tag,Data})).
-else.
-define(dbg(Tag, Data), true).
-endif.

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

start(Id, Pgm, Hosts) when atom(Hosts) ->
    start(Id, Pgm, [Hosts]);
start(Id, Pgm0, Hosts) ->
    Pgm = if
	      atom(Pgm0) ->
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

%% Hosts must be a list on form ['1.2.3.4' ...]
start_it("inet", Id, Pid, Hosts) ->
    process_flag(trap_exit, true),
    %% Setup reserved port for ose_inet driver (only OSE, fails on others)
    case catch erlang:open_port({spawn,ose_inet},[binary]) of
	{'EXIT',_Why} ->
	    ?dbg(ose_inet_port_open_fail, Why);
	_OseInetPort ->
	    ?dbg(ose_inet_port, OseInetPort)
    end,
    ?dbg(inet, {Id,Pid,Hosts}),
    AL = ipv4_list(Hosts),
    ?dbg(addresses, AL),
    {ok,Tcp} = find_master(AL),
    init_ack(Pid),
    State = #state {
		    hosts = AL,
		    id = Id,
		    get = fun get_from_port_inet/3,
                    multi_get = undefined,
		    stop = fun stop_port/1,
		    exit = fun inet_exit_port/3,
		    data = Tcp,
		    timeout = ?IDLE_TIMEOUT,
                    timeout_handler = fun inet_timeout_handler/2
		   },
    loop(State, Pid, []);
start_it("efile", Id, Pid, _Hosts) ->
    process_flag(trap_exit, true),
    {ok, Port} = prim_file:open([binary]),
    init_ack(Pid),
    MultiGetFun = case erlang:system_info(thread_pool_size) of
		      0 -> undefined;
		      _ -> fun multi_get_from_port_efile/4
		  end,
    State = #state {
		    id = Id,
		    get = fun get_from_port_efile/3,
                    multi_get = MultiGetFun,
		    stop = fun efile_stop_port/1,
		    exit = fun exit_port/3,
		    data = Port,
                    timeout = infinity,
                    timeout_handler = fun timeout_handler/2
		    },
    loop(State, Pid, []);
start_it(Pgm, Id, Pid, _Hosts) ->
    process_flag(trap_exit, true),
    Port = erlang:open_port({spawn,Pgm}, [binary]),
    init_ack(Pid),
    State = #state {
		    id = Id,
		    get = fun get_from_port/3,
                    multi_get = undefined,
		    stop = fun stop_port/1,
		    exit = fun exit_port/3,
		    data = Port,
                    timeout = infinity,
                    timeout_handler = fun timeout_handler/2
		   },
    loop(State, Pid, []).

init_ack(Pid) ->
    Pid ! {self(),ok}.

set_path(Paths) when list(Paths) ->
    request({set_path,Paths}).

get_path() ->
    request({get_path}).

get_file(File) when atom(File) ->
    get_file(atom_to_list(File));
get_file(File) ->
    request({get_file,File}).

get_files(ModFiles, Fun) ->
    request({get_files,ModFiles,Fun}).

request(Req) ->
    Loader = whereis(erl_prim_loader),
    Loader ! {self(),Req},
    receive
	{Loader,Res} ->
	    Res;
	{'EXIT',Loader,_What} ->
	    error
    end.

%%% --------------------------------------------------------
%%% The main loop.
%%% --------------------------------------------------------

loop(State, Parent, Paths) ->
    receive
	{Pid,{set_path,NewPaths}} ->
	    Pid ! {self(),ok},
	    loop(State, Parent, to_strs(NewPaths));
	{Pid,{get_path}} ->
	    Pid ! {self(),{ok,Paths}},
	    loop(State, Parent, Paths);
	{Pid,{get_file,File}} ->
	    {Res,State1} = get_file(State, Paths, File),
	    Pid ! {self(),Res},
	    loop(State1, Parent, Paths);
	{Pid,{get_files,ModFiles,Fun}} ->
	    {Res,State1} = get_files(State, ModFiles, Paths, Fun),
	    Pid ! {self(),Res},
	    loop(State1, Parent, Paths);
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

get_files(State, ModFiles, Paths, Fun) ->
    case catch (State#state.multi_get)(State, ModFiles, Paths, Fun) of
	{'EXIT',_} -> {error,State};
	Res -> Res
    end.
    
get_file(State, Paths, File) ->
    case catch (State#state.get)(State, File, Paths) of
	{'EXIT',_} -> {error,State};
	Res -> Res
    end.

stop(State) ->
    (State#state.stop)(State).

exit(State, Who, Reason) ->
    (State#state.exit)(State, Who, Reason).

handle_timeout(State, Pid) ->
    (State#state.timeout_handler)(State, Pid).

%%% --------------------------------------------------------
%%% Functions which handles the binary_filer prim_loader.
%%% --------------------------------------------------------

get_from_port(State, File, Paths) ->
    case absolute_filename(File) of
	true ->               %% Get absolute file name.
	    get_from_port(File, State);
	false when Paths == [] -> %% Get plain file name.
	    get_from_port(File, State);
	false ->                  %% Use Paths.
	    get_from_port1(File, Paths, State)
    end.

get_from_port1(File, [P | Paths], State) ->
    case get_from_port(concat([P,"/",File]), State) of
	{error,State1} ->
	    get_from_port1(File, Paths, State1);
	Result -> Result
    end;
get_from_port1(_File, [], State) ->
    {error,State}.

get_from_port(File, State) ->
    Port = State#state.data,
    Port ! {self(),{command,[?get_file|File]}},
    receive
	{Port,{data,Bin}} ->
	    case binary_to_list(Bin, 1, 1) of
		[?FILE_OK] ->
		    {_,BinFile} = split_binary(Bin, 1),
		    {{ok,BinFile,File},State};
		_Other ->
		    {error,State}
	    end;
	{'EXIT',Port,_} ->
	    exit('prim_load port died')
    end.

stop_port(State) ->
    Port = State#state.data,
    unlink(Port),
    exit(Port, die),
    ok.

exit_port(State, Port, Reason) when State#state.data == Port ->
    exit({port_died,Reason});
exit_port(State, _Port, _Reason) ->
    State.

timeout_handler(State, _Pid) ->  State.

%%% --------------------------------------------------------
%%% Functions which handles efile as prim_loader (default).
%%% --------------------------------------------------------

%%% Reading many files in parallel is an optimization. 
%%% See also comment in init.erl.

%% -> ok | {error,module()}
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
	{Ref, {M,_F}, {error,_Error,State1}} -> 
	    get_files(MFs, Out-1, 0, State1, Paths, Fun, Ref, {error,M})
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

get_from_port_efile(State, File, Paths) ->
    case get_from_port_efile1(State, File, Paths) of
	{error,_Reason,State1} ->
	    {error,State1};
	Res ->
	    Res
    end.

get_from_port_efile1(State, File, Paths) ->
    case absolute_filename(File) of
	true ->               %% Get absolute file name.
	    get_from_port_efile(File, State);
	false when Paths == [] -> %% Get plain file name.
	    get_from_port_efile(File, State);
	false ->                  %% Use Paths.
	    get_from_port_efile2(File, Paths, State)
    end.

get_from_port_efile2(File, [P | Paths], State) ->
    case get_from_port_efile(concat([P,"/",File]), State) of
	{error,Reason,State1} when Reason =/= emfile ->
	    get_from_port_efile2(File, Paths, State1);
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

efile_stop_port(#state{data = Port}) ->
    prim_file:close(Port),
    ok.

%%% --------------------------------------------------------
%%% Functions which handles inet prim_loader
%%% --------------------------------------------------------

%%
%% Connect to a boot master
%% return {ok, Socket}  TCP
%% AL is a list of boot servers (including broadcast addresses)
%%
find_master(AL) ->
    find_master(AL, ?EBOOT_RETRY, ?EBOOT_REQUEST_DELAY, ?EBOOT_RETRY_SLEEP).

find_master(AL, Retry, RequestDelay, RetrySleep) ->
    {ok,U} = ll_udp_open(0),
    find_master(U, Retry, AL, RequestDelay, RetrySleep, []).

%%
%% Master connect loop
%%
find_master(U, Retry, AddressL, RequestDelay, RetrySleep, Ignore) ->
    case find_loop(U, Retry, AddressL, RequestDelay, RetrySleep, Ignore) of
	[] -> 
	    find_master(U, Retry, AddressL, RequestDelay, RetrySleep, Ignore);
	Servers ->
	    ?dbg(servers, Servers),
	    case connect_master(Servers) of
		{ok, Socket} -> 
		    ll_close(U),
		    {ok, Socket};
		_Error ->
		    find_master(U, Retry, AddressL, RequestDelay, RetrySleep, 
				Servers ++ Ignore)
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
find_loop(U, Retry, AL, RequestDelay, RetrySleep, Ignore) ->
    case find_loop(U, Retry, AL, RequestDelay, []) of
	[] ->
	    sleep(RetrySleep),
	    find_loop(U, Retry, AL, RequestDelay, RetrySleep, Ignore);
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

inet_exit_port(State, Port, _Reason) when State#state.data == Port ->
    State#state { data = noport, timeout = infinity };
inet_exit_port(State, _, _) ->
    State.


inet_timeout_handler(State, _Pid) ->
    Tcp = State#state.data,
    if port(Tcp) -> ll_close(Tcp);
       true -> ok
    end,
    State#state { timeout = infinity, data = noport }.


get_from_port_inet(State, File, Paths) ->
    case absolute_filename(File) of
	true ->               %% Get absolute file name.
	    get_from_port_inet(File, State);
	false when Paths == [] -> %% Get plain file name.
	    get_from_port_inet(File, State);
	false ->                  %% Use Paths.
	    get_from_port_inet1(File, Paths, State)
    end.

get_from_port_inet1(File, [P | Paths], State) ->
    case get_from_port_inet(concat([P,"/",File]), State) of
	{error,State1} ->
	    get_from_port_inet1(File, Paths, State1);
	Result -> Result
    end;
get_from_port_inet1(_File, [], State) ->
    {error,State}.

get_from_port_inet(File, State) when State#state.data == noport ->
    {ok,Tcp} = find_master(State#state.hosts),     %% reconnect
    get_from_port_inet(File, State#state { data = Tcp,
					   timeout = ?IDLE_TIMEOUT });
get_from_port_inet(File, State) ->
    Tcp = State#state.data,
    prim_inet:send(Tcp, [?inet_get_file | File]),
    receive
	{tcp, Tcp, [?FILE_INET_OK | BinFile]} ->
	    {{ok, BinFile, File},State};
	{tcp, Tcp, [?FILE_INET_ERROR | _Err]} ->
	    {error,State};
	{'EXIT', Tcp, _} -> 
	    %% Ok we must reconnect
	    get_from_port_inet(File,State#state { data = noport })
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Direct inet_drv access
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcp_options() ->
    [{mode,binary}, {header,1}, {packet,4}, {active, true}, {deliver,term}].

tcp_timeout() -> 
    15000.

%% options for udp  [list, {broadcast, true}, {active,true}]
udp_options() ->
    [{mode,list}, {active, true}, {deliver,term}, {broadcast,true}].
%%
%% INET version IPv4 addresses
%%
ll_tcp_connect(LocalPort, IP, RemotePort) ->
    case ll_open_set_bind(stream, ?INET_PROTO, tcp_options(),
			  {0,0,0,0}, LocalPort) of
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
    ll_open_set_bind(dgram, ?INET_PROTO, udp_options(), {0,0,0,0}, P).


ll_open_set_bind(Type, Proto, SOpts, IP, Port) ->
    case prim_inet:open(Type, Proto) of
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
	true ->
	    true;
	false ->
	    case erlang:system_info(os_type) of
		{win32, _} ->
		    member($\\, File);
		_ ->
		    false
	    end
    end.

send_all(U, [IP | AL], Cmd) ->
    ?dbg(sendto, {U, IP, ?EBOOT_PORT, Cmd}),
    prim_inet:sendto(U, IP, ?EBOOT_PORT, Cmd),
    send_all(U, AL, Cmd);
send_all(_U, [], _) -> ok.

concat([A|T]) when atom(A) ->			%Atom
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

keymember(X, I, [Y | _]) when element(I,Y) == X -> true;
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

to_strs([P|Paths]) when atom(P) ->
    [atom_to_list(P)|to_strs(Paths)];
to_strs([P|Paths]) when list(P) ->
    [P|to_strs(Paths)];
to_strs([_|Paths]) ->
    to_strs(Paths);
to_strs([]) ->
    [].

%% Parse list of ipv4 addresses 
ipv4_list([H | T]) ->
    IPV = if atom(H) -> ipv4_address(atom_to_list(H));
	     list(H) -> ipv4_address(H);
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
