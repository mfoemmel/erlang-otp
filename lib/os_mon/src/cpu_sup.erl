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
-module(cpu_sup).

%%% Purpose : Obtain cpu statistics

-export([nprocs/0,avg1/0,avg5/0,avg15/0,ping/0,util/0,util/1]).

%% External exports
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(APPLICATION, "os_mon").
-ifndef(PORT_PROG_BIN_DIR).
-define(PORT_PROG_BIN_DIR, "bin"). %% This is relative priv_dir
-endif.
-define(PORT_PROG, "cpu_sup").
-define(NAME,cpu_sup).

%% Internal protocol with the port program
-define(nprocs,"n").
-define(avg1,"1").
-define(avg5,"5").
-define(avg15,"f").
-define(quit,"q").
-define(ping,"p").
-define(util,"u").

-define(INT32(D3,D2,D1,D0),
	(((D3) bsl 24) bor ((D2) bsl 16) bor ((D1) bsl 8) bor (D0))).

-define(MAX_INT32, ((1 bsl 32) - 1)).

-record(cpu_util, {cpu, busy = [], non_busy = []}).

-record(state, {port = not_used, util = [], os_type}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start()  -> gen_server:start({local, cpu_sup}, cpu_sup, [], []).
start_link() -> gen_server:start_link({local, cpu_sup}, cpu_sup, [], []).
stop()   -> gen_server:call(?NAME,?quit).

nprocs() -> gen_server:call(?NAME,?nprocs).
avg1()   -> gen_server:call(?NAME,?avg1).
avg5()   -> gen_server:call(?NAME,?avg5).
avg15()  -> gen_server:call(?NAME,?avg15).
ping()   -> gen_server:call(?NAME,?ping).
util(ArgList) when list(ArgList) ->
    case lists:foldl(fun (detailed, {_, PC}) -> {true, PC};
			 (per_cpu,  {D, _}) ->  {D,    true};
			 (_, _) ->              badarg
		     end,
		     {false, false},
		     ArgList) of
	badarg ->
	    erlang:fault(badarg, ArgList);
	{Detailed, PerCpu} ->
	    gen_server:call(?NAME, {?util, Detailed, PerCpu})
    end;
util(Arg) -> erlang:fault(badarg, Arg).
util() ->
    case util([]) of
	{all, Busy, _, _} ->
	    Busy;
	Error ->
	    Error
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    case os:type() of
	{unix, sunos} = OsType ->
	    Prog = filename:join([code:priv_dir(?APPLICATION),
				  ?PORT_PROG_BIN_DIR, ?PORT_PROG]),
	    Port = open_port({spawn, Prog}, [stream,exit_status]),
	    Port ! {self(), {command, ?ping}},
	    case receive_int(Port) of
		4711 ->
		    {ok, #state{port=Port, os_type = OsType}};
		_ ->
		    {stop, {port_program_not_available, Prog}}
	    end;
	{unix, linux} = OsType ->
	    case file:read_file_info("/proc/loadavg") of
		{ok, _} ->
		    {ok,#state{os_type = OsType}};
		_ ->
		    {stop, proc_file_system_not_accessible}
	    end;
	{unix, Flavor} = OsType when Flavor == freebsd;
				     Flavor == openbsd ->
	    {ok,#state{os_type = OsType}};
	OsType ->
	    {stop, {os_type_not_supported,OsType}}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(?quit, _From, State) ->
    case State#state.port of
	not_used ->
	    ok;
	Port when port(Port) ->
	    Port ! {self(), {command, ?quit}},
	    Port ! {self(), close}
    end,
    {stop, shutdown, ok, State};
handle_call({?util, Detailed, PerCpu}, {Client, _Tag},
	    #state{port = Port, os_type = {unix, sunos}} = State) ->
    Port ! {self(), {command, ?util}},
    get_util_measurement_reply(Client,
			       Detailed,
			       PerCpu,
			       fun () ->
				       %% Port program sends cpu
				       %% information sorted on
				       %% cpu index in ascending
				       %% order.
				       sunos_receive_cpu_util(Port)
			       end,
			       State);
handle_call({?util, Detailed, PerCpu}, {Client, _Tag},
	    #state{os_type = {unix, linux}} = State) ->
    get_util_measurement_reply(Client,
			       Detailed,
			       PerCpu,
			       fun () -> linux_read_cpu_util() end,
			       State);
handle_call({?util, _Detailed, _PerCpu}, _From,
	    #state{os_type = OsType} = State) ->
    {reply, {error, {os_type_not_supported, OsType}}, State};
handle_call(Request, _From, State) ->
    case get_int_measurement(Request, State) of
	{error, Error} ->
	    {stop, Error, Error, State};
	Result ->
	    {reply, Result, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    {stop, {port_program_exited, Status}, State#state{port=closed}};
handle_info({Port,closed}, #state{port = Port} = State) ->
    {stop, port_closed, State#state{port=closed}};
handle_info({'DOWN',Monitor,process,_,_}, #state{util = Utils} = State) ->
    {noreply, State#state{util = lists:keydelete(Monitor, 2, Utils)}};
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

get_util_measurement_reply(Client, Detailed, PerCpu, NewCpuUtilFun,
			   #state{util = Utils} = State) ->
    {Monitor, OldCpuUtil, Utils2} = case keysearchdelete(Client, 1, Utils) of
					{{value, {Client, Mon, U}}, Us} ->
					    {Mon, U, Us};
					{false, Us} ->
					    {erlang:monitor(process, Client),
					     [],
					     Us}
				    end,
    case NewCpuUtilFun() of
	{error, Error} ->
	    {stop, Error, Error, State};
	NewCpuUtil ->
	    {reply,
	     cpu_util_rel(NewCpuUtil, OldCpuUtil, Detailed, PerCpu),
	     State#state{util = [{Client,Monitor,NewCpuUtil}|Utils2]}}
    end.

get_int_measurement(Request, #state{port = Port, os_type = {unix, sunos}}) ->
    Port ! {self(), {command, Request}},
    receive_int(Port);
get_int_measurement(Request, #state{os_type = {unix, linux}}) ->
    {ok,F} = file:open("/proc/loadavg",[read,raw]),
    {ok,D} = file:read(F,24),
    ok = file:close(F),
    {ok,[Load1,Load5,Load15,PRun,PTotal],_} = io_lib:fread("~f ~f ~f ~d/~d", D),
    case Request of
	?avg1  -> sunify(Load1);
	?avg5  -> sunify(Load5);
	?avg15 -> sunify(Load15);
	?ping -> 4711;
	?nprocs -> PTotal
    end;
get_int_measurement(Request, #state{os_type = {unix, freebsd}}) ->
    D = os:cmd("/sbin/sysctl -n vm.loadavg") -- "\n",
    {ok,[Load1,Load5,Load15],_} = io_lib:fread("{ ~f ~f ~f }", D),
    %% We could count the lines from the ps command as well
    case Request of
	?avg1  -> sunify(Load1);
	?avg5  -> sunify(Load5);
	?avg15 -> sunify(Load15);
	?ping -> 4711;
	?nprocs ->
	    {ok, DirList} = file:list_dir("/proc"),
	    length(DirList)
    end;
get_int_measurement(Request, #state{os_type = {unix, openbsd}}) ->
    D = os:cmd("/sbin/sysctl -n vm.loadavg") -- "\n",
    {ok, [L1, L5, L15], _} = io_lib:fread("~f ~f ~f", D),
    case Request of
	?avg1  -> sunify(L1);
	?avg5  -> sunify(L5);
	?avg15 -> sunify(L15);
	?ping -> 4711;
	?nprocs ->
	    Ps = os:cmd("/bin/ps -ax | /usr/bin/wc -l"),
	    {ok, [N], _} = io_lib:fread("~d", Ps),
	    N-1
    end;
get_int_measurement(Request, #state{os_type = OsType}) ->
    {error, {os_type_not_supported, OsType}}.

sunify(Val)  ->
    round(Val*256). % Note that Solaris and Linux load averages are
		    % measured quite differently anyway


receive_int(Port) ->
    receive_int(Port, []).

receive_int(Port, [D3,D2,D1,D0]) ->
    ?INT32(D3,D2,D1,D0);
receive_int(_, [_,_,_,_|Garbage]) ->
    {error, {garbage_from_port_program, Garbage}};
receive_int(Port, Data) ->
    receive
	{Port, {data, NxtData}} ->
	    receive_int(Port, Data ++ NxtData);
	{Port, {exit_status, Status}} ->
	    {error, {port_program_exited, Status}}
    end.

keysearchdelete(_, _, []) ->
    {false, []};
keysearchdelete(K, N, [T|Ts]) when element(N, T) == K ->
    {{value, T}, Ts};
keysearchdelete(K, N, [T|Ts]) ->
    {X, NTs} = keysearchdelete(K, N, Ts),
    {X, [T|NTs]}.

%%% Internal cpu utilization functions 

%% cpu_util_diff(New, Old) takes a list of new cpu_util records as first
%% argument and a list of old cpu_util records as second argument. The
%% two lists have to be sorted on cpu index in ascending order.
%%
%% The returned value is a difference list in descending order.
cpu_util_diff(New, Old) ->
    cpu_util_diff(New, Old, []).

cpu_util_diff([], [], Acc) ->
    Acc;
cpu_util_diff([#cpu_util{cpu      = Cpu,
			 busy     = NewBusy,
			 non_busy = NewNonBusy} | NewCpuUtils],
	      [#cpu_util{cpu      = Cpu,
			 busy     = OldBusy,
			 non_busy = OldNonBusy} | OldCpuUtils],
	      Acc) ->
    {PreBusy, GotBusy} = state_list_diff(NewBusy, OldBusy),
    {NonBusy, GotNonBusy} = state_list_diff(NewNonBusy, OldNonBusy),
    Busy = case GotBusy orelse GotNonBusy of
	       true ->
		   PreBusy;
	       false ->
		   %% This can happen if cpu_sup:util/[0,1] is called
		   %% again immediately after the previous call has
		   %% returned. Because the user obviously is doing
		   %% something we charge "user".
		   lists:map(fun ({user, 0}) -> {user, 1};
				 ({_, 0} = StateTup) -> StateTup
			     end,
			     PreBusy)
	   end,
    cpu_util_diff(NewCpuUtils, OldCpuUtils, [#cpu_util{cpu      = Cpu,
						       busy     = Busy,
						       non_busy = NonBusy}
					     | Acc]);

%% A new cpu appeared
cpu_util_diff([#cpu_util{cpu = NC}|_] = New,
	      [#cpu_util{cpu = OC}|_] = Old,
	      Acc) when NC < OC ->
    cpu_util_diff(New, [#cpu_util{cpu = NC}|Old], Acc);
cpu_util_diff([#cpu_util{cpu = NC}|_] = New, [], Acc) ->
    cpu_util_diff(New, [#cpu_util{cpu = NC}], Acc);

%% An old cpu disappeared
cpu_util_diff([#cpu_util{cpu = NC}|Ns],
	      [#cpu_util{cpu = OC}|_] = Old,
	      Acc) when NC > OC ->
    cpu_util_diff(Ns, Old, Acc);
cpu_util_diff([], [#cpu_util{cpu = OC}|_] = Old, Acc) ->
    cpu_util_diff([], Old, Acc).


cpu_util_rel(NewCpuUtils, OldCpuUtils, Detailed, PerCpu) ->
    cpu_util_rel(cpu_util_diff(NewCpuUtils, OldCpuUtils), Detailed, PerCpu).

%% 
%% cpu_util_rel/3 takes a difference list of cpu_util records as first
%% argument, a boolean determining if the result should be detailed as
%% second argument, and a boolean determining if the result should be
%% per cpu as third argument. The first argument (the difference list)
%% has to be sorted on cpu index in descending order.
%% 
cpu_util_rel(CUDiff, false, false) ->
    {B, T} = lists:foldl(fun (#cpu_util{busy     = BusyList,
					non_busy = NonBusyList},
			      {BusyAcc, TotAcc}) ->
				 Busy  = state_list_sum(BusyList),
				 NonBusy = state_list_sum(NonBusyList),
				 {BusyAcc+Busy, TotAcc+Busy+NonBusy}
			 end,
			 {0, 0},
			 CUDiff),
    BRel = B/T*100,
    {all, BRel, 100-BRel, []};
cpu_util_rel(CUDiff, true, false) ->
    cpu_util_rel_det(CUDiff, #cpu_util{cpu = [], busy = [], non_busy = []}); 
cpu_util_rel(CUDiff, false, true) ->
    cpu_util_rel_pcpu(CUDiff, []);
cpu_util_rel(CUDiff, true, true) ->
    cpu_util_rel_det_pcpu(CUDiff, []).

cpu_util_rel_pcpu([], Acc) ->
    Acc;
cpu_util_rel_pcpu([#cpu_util{cpu      = C,
			     busy     = BusyList,
			     non_busy = NonBusyList} | Rest], Acc) ->
    Busy  = state_list_sum(BusyList),
    NonBusy = state_list_sum(NonBusyList),
    Tot = Busy + NonBusy,
    cpu_util_rel_pcpu(Rest, [{C, Busy/Tot*100, NonBusy/Tot*100, []}|Acc]).

cpu_util_rel_det([], #cpu_util{cpu      = CpuAcc,
			       busy     = BusyAcc,
			       non_busy = NonBusyAcc}) ->
    Total = state_list_sum(BusyAcc) + state_list_sum(NonBusyAcc),
    {CpuAcc, mk_rel_states(BusyAcc,Total), mk_rel_states(NonBusyAcc,Total), []};
cpu_util_rel_det([#cpu_util{cpu      = Cpu,
			    busy     = Busy,
			    non_busy = NonBusy} | Rest],
		 #cpu_util{cpu      = CpuAcc,
			   busy     = BusyAcc,
			   non_busy = NonBusyAcc} = Acc) ->
    cpu_util_rel_det(Rest, #cpu_util{cpu      = [Cpu|CpuAcc],
				     busy     = state_list_add(Busy,
							       BusyAcc),
				     non_busy = state_list_add(NonBusy,
							       NonBusyAcc)}).

cpu_util_rel_det_pcpu([], Acc) ->
    Acc;
cpu_util_rel_det_pcpu([#cpu_util{cpu      = Cpu,
				 busy     = Busy,
				 non_busy = NonBusy}| Rest], Acc) ->
    Total = state_list_sum(Busy) + state_list_sum(NonBusy),
    cpu_util_rel_det_pcpu(Rest,
			  [{Cpu,
			    mk_rel_states(Busy, Total),
			    mk_rel_states(NonBusy, Total),
			    []} | Acc]).


mk_rel_states(States, Total) ->
    lists:map(fun ({State, Value}) -> {State, 100*Value/Total} end, States).

state_list_sum(StateList) ->
    lists:foldl(fun ({_, X}, Acc) -> Acc+X end, 0, StateList).


state_list_diff([],[]) ->
    {[], false};
state_list_diff([{State,ValueNew}|RestNew], []) ->
    state_list_diff([{State, ValueNew} | RestNew], [{State, 0}]);
state_list_diff([{State,ValueNew}|RestNew], [{State,ValueOld}|RestOld]) ->
    ValDiff = case val_diff(ValueNew, ValueOld) of
		  Int when Int >= 0 -> Int;
		  _ -> throw({error, negative_utilizaton})
	      end,
    {RestStateDiff, FoundDiff} = state_list_diff(RestNew, RestOld),
    {[{State, ValDiff} | RestStateDiff], FoundDiff orelse ValDiff /= 0}.

state_list_add([],[]) ->
    [];
state_list_add([{State, ValueA}|RestA], []) ->
    [{State, ValueA} | state_list_add(RestA, [])];
state_list_add([{State, ValueA} | RestA], [{State, ValueB} | RestB]) ->
    [{State, ValueA + ValueB} | state_list_add(RestA, RestB)].

val_diff(New, Old) when New < Old ->
    %% Value wrapped around between old and new (32 bit integer)
    ?MAX_INT32 + New - Old;
val_diff(New, Old) ->
    New - Old.

%%%
%%% Sunos specific functions...
%%%

sunos_receive_cpu_util(Port) ->
    receive
	{Port, {data, [N3,N2,N1,N0|CpuUtilData]}} ->
	    sunos_receive_cpu_util(Port, ?INT32(N3,N2,N1,N0), CpuUtilData, []);
	{Port, {exit_status, Status}} ->
	    {error, {port_program_exited, Status}}
    end.

sunos_receive_cpu_util(_, 0, [], Acc) ->
    lists:reverse(Acc); % Now sorted in ascending order on cpu index.
sunos_receive_cpu_util(_, 0, Garbage, _) ->
    {error, {garbage_from_port_program, Garbage}};
sunos_receive_cpu_util(Port, N, [C3,C2,C1,C0,
				 U3,U2,U1,U0,
				 K3,K2,K1,K0,
				 W3,W2,W1,W0,
				 I3,I2,I1,I0 | CpuSD], Acc) ->
    sunos_receive_cpu_util(Port,
			   N-1,
			   CpuSD,
			   [#cpu_util{cpu      = ?INT32(C3,C2,C1,C0),
				      busy     = [{user, ?INT32(U3,U2,U1,U0)},
						  {kernel,?INT32(K3,K2,K1,K0)}],
				      non_busy = [{wait, ?INT32(W3,W2,W1,W0)},
						  {idle, ?INT32(I3,I2,I1,I0)}]}
			    | Acc]);
sunos_receive_cpu_util(Port, N, CpuSD, Acc) ->
    receive
	{Port, {data, NxtCpuSD}} ->
	    sunos_receive_cpu_util(Port, N, CpuSD ++ NxtCpuSD, Acc);
	{Port, {exit_status, Status}} ->
	    {error, {port_program_exited, Status}}
    end.

%%%
%%% Linux specific functions...
%%%

linux_get_cpu(all, Data, MoreDataFun) ->
    linux_get_cpu([], Data, MoreDataFun, "cpu ~d ~d ~d ~d");
linux_get_cpu(no, Data, MoreDataFun) ->
    linux_get_cpu([], Data, MoreDataFun, "cpu~d ~d ~d ~d ~d").

linux_get_cpu(Cont, Data, MoreDataFun, Format) ->
    case io_lib:fread(Cont, Data, Format) of
	{more, Cont2} ->
	    Data2 = case MoreDataFun() of
			eod -> throw({error, unexpected_end_of_data});
			D -> D
		    end,
	    linux_get_cpu(Cont2, Data2, MoreDataFun, Format);
	{done, {ok, CpuUtilList}, NextData} ->
	    {linux_cpu_util_list_to_cpu_util(CpuUtilList), NextData};
	_ ->
	    throw({error, unexpected_data_format})
    end.

linux_cpu_util_list_to_cpu_util([_User, _NiceUser, _Kernel, _Idle] = Data) ->
    linux_cpu_util_list_to_cpu_util([-1|Data]);
linux_cpu_util_list_to_cpu_util([Cpu, User, NiceUser, Kernel, Idle]) ->
    #cpu_util{cpu      = Cpu,
	      busy     = [{user,User}, {nice_user,NiceUser}, {kernel,Kernel}],
	      non_busy = [{idle,Idle}]}.

linux_read_cpus(MoreDataFun) ->
    catch linux_read_cpus([], MoreDataFun, []).

linux_read_cpus(Data, MoreDataFun, CpuUtils) ->
    case linux_read_until_cpu(Data, MoreDataFun) of
	eod ->
	    case CpuUtils of
		[#cpu_util{cpu = -1} = CpuUtil] ->
		    [CpuUtil#cpu_util{cpu = 0}];
		_ ->
		    case lists:keysort(#cpu_util.cpu, CpuUtils) of
			[#cpu_util{cpu = -1} | NewCpuUtils] ->
			    NewCpuUtils;
			NewCpuUtils ->
			    NewCpuUtils
		    end
	    end;
	{Type, Data2} ->
	    {CpuUtil, Data3} = linux_get_cpu(Type, Data2, MoreDataFun),
	    linux_read_cpus(Data3, MoreDataFun, [CpuUtil|CpuUtils])
    end.


linux_read_until_cpu([$c,$p,$u,X|_] = Data, _) ->
    {case X of $0 -> no; $1 -> no; $2 -> no; $3 -> no; $4 -> no; $5 -> no;
	 $6 -> no; $7 -> no; $8 -> no; $9 -> no; _ -> all end, Data};
linux_read_until_cpu([_,_,_,_|_] = Data, MoreDataFun) ->
    [_|Rest] = Data,
    linux_read_until_cpu(Rest, MoreDataFun);
linux_read_until_cpu(Data, MoreDataFun) ->
    case MoreDataFun() of
	eod  -> eod;
	NewData -> linux_read_until_cpu(Data++NewData, MoreDataFun)
    end.    

linux_read_cpu_util() ->
    case catch file:open("/proc/stat",[read,raw]) of
	{ok, IODev} ->
	    linux_read_cpus(fun () ->
				    case file:read(IODev, 100) of
					{ok, Data} -> Data;
					_ -> file:close(IODev), eod
				    end
			    end);
	_ ->
	    {error, {file_open_failed, "/proc/stat"}}
    end.

%%%----------------------------------------------------------------------

