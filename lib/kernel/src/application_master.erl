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
-module(application_master).

%% External exports
-export([start_link/2, start_type/0, stop/1]).
-export([here_i_am/2, spawn_request/1, get_child/1]).

%% Internal exports
-export([init/4, start_it/4]).

-include("application_master.hrl").

-record(state, {child, appl_data, children = [], procs = 0, gleader}).

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: ApplData = record(appl_data)
%% Purpose: Starts an application master for the application.
%%          Called from application_controller.  (The application is
%%          also started).
%% Returns: {ok, Pid} | {error, Reason} (Pid is unregistered)
%%-----------------------------------------------------------------
start_link(ApplData, Type) ->
    Parent = whereis(application_controller),
    proc_lib:start_link(application_master, init,
			[Parent, self(), ApplData, Type]).

start_type() ->
    group_leader() ! {start_type, self()},
    receive
	{start_type, Type} ->
	    Type
		after 5000 ->
			{error, timeout}
		end.

%%-----------------------------------------------------------------
%% Func: stop/1
%% Purpose: Stops the application.  This function makes sure
%%          that all processes belonging to the applicication is
%%          stopped (shutdown or killed).  The application master
%%          is also stopped.
%% Returns: ok
%%-----------------------------------------------------------------
stop(Application) -> call(Application, stop).

%%-----------------------------------------------------------------
%% Purpose: Check if this application can start another process.
%% Returns: true | false
%%-----------------------------------------------------------------
spawn_request(Application) -> call(Application, spawn_request).

%%-----------------------------------------------------------------
%% Purpose: Tell an application master that a new process exists.
%%-----------------------------------------------------------------
here_i_am(Application, Pid) -> Application ! {here_i_am, Pid}.

get_child(Application) -> call(Application, get_child).
    
call(Application, Req) ->
    Tag = make_ref(),
    Ref = erlang:monitor(process, Application),
    Application ! {Req, Tag, self()},
    receive 
	{'DOWN', Ref, process, _, _Info} ->
	    ok;
	{Tag, Res} ->
	    erlang:demonitor(Ref),
	    receive 
		{'DOWN', Ref, process, _, _Info} -> 
		    Res
	    after 0 ->
		    Res
	    end
    end.

%%%-----------------------------------------------------------------
%%% The logical and physical process structrure is as follows:
%%%
%%%         logical                physical
%%%
%%%         --------               --------
%%%         |AM(GL)|               |AM(GL)|               
%%%         --------               -------- 
%%%            |                       |
%%%         --------               --------
%%%         |Appl P|               |   X  |
%%%         --------               --------
%%%                                    |
%%%                                --------
%%%                                |Appl P|
%%%                                --------
%%%
%%% Where AM(GL) == Application Master (Group Leader)
%%%       Appl P == The application specific 'root' process (child to AM)
%%%       X      == A special 'invisible' process
%%% The reason for not using the logical structrure is that the
%%% application start function is synchronous, and 
%%% that the AM is GL.  This means that if AM executed the start
%%% function, and this function uses spawn_request/1
%%% or io, deadlock would occur.  Therefore, this function is
%%% executed by the process X.  Also, AM needs three loops;
%%% init_loop (waiting for the start function to return)
%%% main_loop
%%% terminate_loop (waiting for the process to die)
%%% In each of these loops, io and other requests are handled.
%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
init(Parent, Starter, ApplData, Type) ->
    link(Parent),
    process_flag(trap_exit, true),
    OldGleader = group_leader(),
    group_leader(self(), self()),
    % Insert ourselves as master for the process.  This ensures that
    % the processes in the application can use get_env/1 at startup.
    Name = ApplData#appl_data.name,
    ets:insert(ac_tab, {{application_master, Name}, self()}),
    State = #state{appl_data = ApplData, gleader = OldGleader},
    case start_it(State, Type) of
	{ok, Pid} ->          % apply(M,F,A) returned ok
	    set_timer(ApplData#appl_data.maxT),
	    unlink(Starter),
	    proc_lib:init_ack(Starter, {ok,self()}),
	    main_loop(Parent, State#state{child = Pid});
	{error, Reason} ->    % apply(M,F,A) returned error
	    exit(Reason);
	Else ->               % apply(M,F,A) returned erroneous
	    exit(Else)
    end.

%%-----------------------------------------------------------------
%% We want to start the new application synchronously, but we still
%% want to handle io requests.  So we spawn off a new process that
%% performs the apply, and we wait for a start ack.
%%-----------------------------------------------------------------
start_it(State, Type) ->
    Tag = make_ref(),
    Pid = spawn_link(application_master, start_it, [Tag, State, self(), Type]),
    init_loop(Pid, Tag, State, Type).


%%-----------------------------------------------------------------
%% These are the three different loops executed by the application_
%% master
%%-----------------------------------------------------------------
init_loop(Pid, Tag, State, Type) ->
    receive
 	IoReq when element(1, IoReq) == io_request ->
	    State#state.gleader ! IoReq,
	    init_loop(Pid, Tag, State, Type);
	{Tag, Res} ->
	    Res;
	{'EXIT', Pid, Reason} ->
	    {error, Reason};
	{start_type, From} ->
	    From ! {start_type, Type},
	    init_loop(Pid, Tag, State, Type);
	Other ->
	    NewState = handle_msg(Other, State),
	    init_loop(Pid, Tag, NewState, Type)
    end.

main_loop(Parent, State) ->
    receive
	IoReq when element(1, IoReq) == io_request ->
	    State#state.gleader ! IoReq,
	    main_loop(Parent, State);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, State);
	{'EXIT', Child, Reason} when State#state.child == Child ->
	    terminate(Reason, State#state{child = undefined});
	{'EXIT', _, timeout} ->
	    terminate(normal, State);
	{'EXIT', Pid, Reason} ->
	    Children = lists:delete(Pid, State#state.children),
	    Procs = State#state.procs - 1,
	    main_loop(Parent, State#state{children = Children, procs=Procs});
	{start_type, From} ->
	    From ! {start_type, local},
	    main_loop(Parent, State);
	Other ->
	    NewState = handle_msg(Other, State),
	    main_loop(Parent, NewState)
    end.

terminate_loop(Child, State) ->
    receive
 	IoReq when element(1, IoReq) == io_request ->
	    State#state.gleader ! IoReq,
	    terminate_loop(Child, State);
	{'EXIT', Child, _} ->
	    ok;
	Other ->
	    NewState = handle_msg(Other, State),
	    terminate_loop(Child, State)
    end.


%%-----------------------------------------------------------------
%% The Application Master is linked to *all* processes in the group
%% (application).  
%%-----------------------------------------------------------------
handle_msg({spawn_request, Tag, From}, State) ->
    ApplData = State#state.appl_data,
    Procs = State#state.procs,
    MaxP = ApplData#appl_data.maxP,
    Reply = 
	if 
	    MaxP == infinity -> true;
	    MaxP > Procs -> true;
	    true -> false
	end,
    From ! {Tag, Reply},
    State;
handle_msg({here_i_am, Pid}, State) ->
    link(Pid),
    Children = State#state.children,
    Procs = State#state.procs + 1,
    State#state{children = [Pid | Children], procs = Procs};
handle_msg({get_child, Tag, From}, State) ->
    From ! {Tag, get_child_i(State#state.child)},
    State;
handle_msg({stop, Tag, From}, State) ->
    catch terminate(normal, State),
    From ! {Tag, ok},
    exit(normal);
handle_msg(_, State) ->
    State.


terminate(Reason, State) ->
    terminate_child(State#state.child, State),
    kill_children(State#state.children),
    exit(Reason).




%%%=============================================================================
%%%=============================================================================
%%%=============================================================================
%% This is the process X above...
%%%=============================================================================
%%%=============================================================================
%%%=============================================================================

%%%=============================================================================
%%% Start an application.
%%% If the start_phases is defined in the .app file the application is to be
%%% started in one or several start phases.
%%% If the Module in the mod-key is set to application_starter then the
%%% geniric help module application_starter is used to control the start.
%%%=============================================================================

start_it(Tag, State, From, Type) ->
    process_flag(trap_exit, true),
    ApplData = State#state.appl_data,
    case {ApplData#appl_data.phases, ApplData#appl_data.mod} of
	{undefined, _} ->
	    start_it_old(Tag, From, Type, ApplData);
	{Phases, {application_starter, [M, A]}} ->
	    start_it_new(Tag, From, Type, M, A, Phases, 
			 [ApplData#appl_data.name]);
	{Phases, {M, A}} ->
	    start_it_new(Tag, From, Type, M, A, Phases, 
			 [ApplData#appl_data.name]);
	{OtherP, OtherM} ->
	    From ! {Tag, {error, {bad_keys, {{mod, OtherM}, 
					     {start_phases, OtherP}}}}}
    end.


%%%-----------------------------------------------------
%%% No start phases are defined
%%%-----------------------------------------------------
start_it_old(Tag, From, Type, ApplData) ->
    {M,A} = ApplData#appl_data.mod,
    case catch M:start(Type, A) of
	{ok, Pid} ->
	    link(Pid),
	    {ok, self()},
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, []);
	{ok, Pid, AppState} ->
	    link(Pid),
	    {ok, self()},
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, AppState);
	{'EXIT', normal} ->
	    From ! {Tag, {error, "application exited with reason: normal"}};
	{error, Reason} ->
	    From ! {Tag, {error, {Reason, {M,start,[Type,A]}}}};
	Other ->
	    Call = io_lib:format("~p:start(~p,~p) -> ~p",[M, Type, A, Other]), 
	    From ! {Tag, {error, "invalid return value from " ++ 
			  lists:flatten(Call)}}
    end.


%%%-----------------------------------------------------
%%% Start phases are defined
%%%-----------------------------------------------------
start_it_new(Tag, From, Type, M, A, Phases, Apps) ->
    case catch start_the_app(Type, M, A, Phases, Apps) of
	{ok, Pid, AppState} ->
	    From ! {Tag, {ok, self()}},
	    loop_it(From, Pid, M, AppState);    
	Error ->
	    From ! {Tag, Error}
    end.



%%%=====================================================
%%% Start the application in the defined phases, 
%%% but first the supervisors are starter.
%%%=====================================================
start_the_app(Type, M, A, Phases, Apps) ->
    case start_supervisor(Type, M, A) of
 	{ok, Pid, AppState} ->
	    link(Pid),
	    case application_starter:start(Phases, Type, Apps) of
		ok ->
		    {ok, Pid, AppState};
		Error2 ->
		    unlink(Pid),
		    Error2
	    end;
	Error ->
	    Error
    end.

%%%-------------------------------------------------------------
%%% Start the supervisors
%%%-------------------------------------------------------------
start_supervisor(Type, M, A) ->
    case catch M:start(Type, A) of
	{ok, Pid} ->
	    {ok, Pid, []};
	{ok, Pid, AppState} ->
	    {ok, Pid, AppState};
	{error, Reason} ->
	    {error, {Reason, {M, start, [Type, A]}}};
	{'EXIT', normal} ->
	    {error, "application exited with reason: normal"};
	Other ->
	    {error, {bad_return, {{M, start, [Type, A]}, Other}}}
    end.




%%%=============================================================================
%%%
%%%=============================================================================

loop_it(Parent, Child, Mod, AppState) ->
    receive
	{Parent, get_child} ->
	    Parent ! {self(), Child, Mod},
	    loop_it(Parent, Child, Mod, AppState);
	{Parent, terminate} ->
	    NewAppState = prep_stop(Mod, AppState),
	    exit(Child, shutdown),
	    receive
		{'EXIT', Child, _} -> ok
	    end,
	    catch Mod:stop(NewAppState),
	    exit(normal);
	{'EXIT', Parent, Reason} ->
	    NewAppState = prep_stop(Mod, AppState),
	    exit(Child, Reason),
	    receive
		{'EXIT', Child, Reason2} ->
		    exit(Reason2)
	    end,
	    catch Mod:stop(NewAppState);
	{'EXIT', Child, Reason} -> % forward *all* exit reasons (inc. normal)
	    NewAppState = prep_stop(Mod, AppState),
	    catch Mod:stop(NewAppState),
	    exit(Reason);
	_ ->
	    loop_it(Parent, Child, Mod, AppState)
    end.

prep_stop(Mod, AppState) ->
    case catch Mod:prep_stop(AppState) of
	{'EXIT', {undef, _}} ->
	    AppState;
	{'EXIT', Reason} ->
	    error_logger:error_report([{?MODULE, shutdown_error},
				       {Mod, {prep_stop, [AppState]}},
				       {error_info, Reason}]),
	    AppState;
	NewAppState ->
	    NewAppState
    end.

get_child_i(Child) ->
    Child ! {self(), get_child},
    receive
	{Child, GrandChild, Mod} -> {GrandChild, Mod}
    end.

terminate_child_i(Child, State) ->
    Child ! {self(), terminate},
    terminate_loop(Child, State).

%% Try to shutdown the child gently
terminate_child(undefined, _) -> ok;
terminate_child(Child, State) ->
    terminate_child_i(Child, State).

kill_children(Children) ->
    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Children),
    kill_all_procs().

%% All except zombies.
alive_processes() ->
    lists:filter({erlang, is_process_alive}, processes()).

kill_all_procs() ->
    Self = self(),
    case lists:filter(fun(Pid) when Pid /= Self ->
			      case erlang:process_info(Pid, group_leader) of
				  {group_leader, Self} -> true;
				  _ -> false
			      end;
			 (_) -> false
		      end, alive_processes()) of
	[] -> ok;
	Children ->
	    lists:foreach(fun(Pid) -> exit(Pid, kill) end, Children),
	    kill_all_procs()
    end.

set_timer(infinity) -> ok;
set_timer(Time) -> timer:exit_after(Time, timeout).
