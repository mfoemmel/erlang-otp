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
-module(dbg_ui_mon).

%% -- Exported user functions.
-export([
	 start_timeout/1,
	 q/3,
	 quick/3
	]).


%% -- Exported internal functions.
-export([
	 init/1,
	 attach_menus/1
	]).


-define(FRAMEDEFS,{open,open,open,close}).
-define(BACKTRACE,100).
-define(ATTACH_FUNCTION, {dbg_ui_trace, a_start}).

-record(gs_mon,{dir,    % Dir in file dialog
		win,    % The window ID
		grid,   % The grid Id
		size,   % The number of visible processes
		pids,   % The attached processes
		brpid,  % For future use. Will contain other win pids
		modpid, % For future use. Will contain other win pids
		mods,   % Trace compiled Monules
		breaks, % All the breaks
		trace,  % Trace Frame in attach window status
		bind,   % Bind Frame in attach window status
		eval,   % Eval Frame in attach window status
		button, % Button Frame in attach window status
		attach, % Auto Attach Status
		load,   % Auto load Status
		stack,  % Stack Status
		coords, % Mouse pointer position
		nodes,  % Nodes being monitored
		btrace, % Back Trace default
		focus,  % Process in focus
		table
	       }). 



start_timeout(Timeout) ->
    PidInit = spawn (?MODULE, init, [self ()]),

    %% Wait for a initialization completion message from
    %% the spawned process before returning its Pid.

    receive
        {initialization_complete, PidInit} ->
            PidInit;

        {already_exists, PidInit} ->
            exit (PidInit, kill),
            exit (already_exists)

    %% (Conditional) Failure to start within the time limit will
    %% result in termination (Timeout may be infinite).

    after
	Timeout ->
            exit (PidInit, kill),
            exit ({startup_timeout, ?MODULE})
    end.



%%% quick  /3
%%% 
%%% quick opens an attach window with the given module
%%%

quick (Module, Fun, Args) ->
    int:i (Module),
    int:auto_attach (init),
    apply (Module, Fun, Args).


q (Module, Fun, Args) ->
    quick (Module, Fun, Args).



%%% init  /1
%%% Initializes the necessary environment needed to follow attached
%%% processes. Creates the monitor window and initializes it, as well
%%% as updating menus
%%%
%%% Before entering the loop it sends a message to the calling function
%%% that the initialization was complete.
%%%

init (CallingPid) ->
    Text = dbg_ui_mon_win:mon_title (),
    case dbg_ui_winman:win_started (self (), Text) of 
	true ->
	    CallingPid ! {already_exists, self ()};

	_false ->
	    ok
    end,
    
    process_flag(trap_exit,true),
    GS = gs:start([{kernel,true}]),
    int:start(),
    int:refresh(),
    {Processes,Nodes} =initial_procs([],[]),
    ForProcesses = format_procs(Processes),

    {Win, Grid} = dbg_ui_mon_win:mon_window(ForProcesses),
    dbg_ui_winman:insert_win (monitor, Win, self ()),    % windows manager
    dbg_ui_mon_win:update_backtrace_menu(?BACKTRACE),
    Size = length(ForProcesses),
    Mods = lists:sort(code:interpreted()),
    Breaks = int:all_breaks(),
    dbg_ui_aux:insert_breaks(Breaks,0),
    {Button,Eval,Bind,Trace} = get_frame(),
    set_frame_buttons({Button,Eval,Bind,Trace}),
    dbg_idb:insert(frame_defs,{Button,Eval,Bind,Trace}),
    dbg_idb:insert(stack_defs,all),
    dbg_idb:insert(backtrace_def,?BACKTRACE),
    {ok,Dir} = file:get_cwd(),
    TabId = dbg_ets:new(interpreter_includedirs_macros, 
		    [set, public, {keypos,1}]),
    Gs_mon = #gs_mon{win    = Win,
		     grid   = Grid,
		     size   = Size,
		     pids   = ForProcesses,
		     brpid  = null,
		     modpid = null,
		     mods   = [],
		     breaks = Breaks,
		     eval   = Eval,
		     bind   = Bind, 
		     button = Button,
		     trace  = Trace,
		     attach = false, 
		     load   = false,
		     coords = {0,0},
		     dir    = Dir,
		     focus  = 1,
		     nodes  = Nodes,
		     btrace = ?BACKTRACE,
		     stack  = all,
		     table  = TabId},

    check_focus_buttons(Gs_mon),

    CallingPid ! {initialization_complete, self ()},

    loop(new_mods(Gs_mon,Mods)).

    
%%% initial_procs(Pids) -> [PidTuple]
%%% Reads all the messages regarding already attached pids

initial_procs(Pids,Nodes) ->
    receive
	{IntPid,new_proc,PidTuple} ->
	    NewNodes = monitor_nodes(PidTuple,Nodes),
	    initial_procs([PidTuple|Pids],NewNodes)
    after 1 ->  % suspend !!
	    {lists:reverse(Pids),Nodes}
    end.
	

%% Functions for monitoring of remote Erlang nodes.
%% Interpreted processes at a disconnected remote node
%% are not accessible !!!


monitor_nodes(PidTuple,Nodes) ->
    Pid = element(1,PidTuple),
    case is_alive() of
	true when node() =/= node(Pid) ->
	    Node = node(Pid),
	    case lists:member(Node,Nodes) of
		false ->
		    erlang:monitor_node(Node,true),
		    [Node|Nodes];
		_ ->
		    Nodes
	    end;
	_ ->
	    Nodes
    end.


nodedown(Node,Gs_mon) ->
    NewNodes = lists:delete(Node,Gs_mon#gs_mon.nodes),
    nodedown(Node,Gs_mon#gs_mon.pids,Gs_mon#gs_mon{nodes=NewNodes}).

nodedown(Node,[PidS|T],Gs_mon) when node(element(2,PidS)) == Node ->
    case PidS of
	%% Row, Pid, Meta, Function, Status, Information, ?, Name 
	{R,P,M,F,exit,I,E,N} ->
	    nodedown(Node,T,Gs_mon);
	{R,P,M,F,_,_,_,_} ->
	    NGs_mon = new_status({P,M,F,no_conn,'NODE DOWN',{}},Gs_mon),
	    nodedown(Node,T,NGs_mon)
    end;
nodedown(Node,[_|T],Gs_mon) -> nodedown(Node,T,Gs_mon);
nodedown(_,   [],   Gs_mon) -> Gs_mon.

get_frame() ->
    case dbg_idb:lookup(frame_defs) of
	not_found  -> ?FRAMEDEFS;
	{ok,Frames}-> Frames
    end.


%%% loop(#gs_mon).
%%% The user loop where events from the attached processes are monitored.
%%% User events in the window, with the exception of motion, are 
%%% handled in the gs_cmd set of functions.

loop(Gs_mon) ->
    receive
	{ping, Pid} ->
	    Pid ! {debugger, alive},
 	    loop (Gs_mon);

	{stop, Pid} ->
	    Pid ! {debugger, stopped},
            exit_debugger (Gs_mon, stop);
	
        {new_dir,Dir}         ->
	    loop(Gs_mon#gs_mon{dir=Dir});

	{P,auto_attach,Opt }  ->
	    loop(auto_attach(Opt,Gs_mon));

	{P,trace,Bool}        ->
	    loop(trace_flag(Bool,Gs_mon));

	{P,stack_trace,Opt}   ->
	    loop(stack_trace(Opt,Gs_mon));

	{P,new_status,Status} ->
	    loop(new_status(Status,Gs_mon));

	{P,new_proc,Status}   ->
	    loop(new_proc(Status,Gs_mon));
	
	{P,clear}             ->
	    loop(clear_grid(Gs_mon));

	{P,interpret,Mod}     ->
	    loop(new_mod(Gs_mon,Mod));

	{P,no_interpret,Mod}  ->
	    loop(no_interpret(Gs_mon,Mod));

	{P,no_break,Br}        ->
	    loop(break(Gs_mon,{no_break,Br}));

	{P,delete_break,Br}    ->
	    loop(break(Gs_mon,{delete_break,Br}));

	{P,new_break_options,I}->
	    loop(break(Gs_mon,{new_break_options,I}));

	{P,new_break,I}        ->
	    loop(break(Gs_mon,{new_break,I}));

	{nodedown,Node}       ->
	    loop(nodedown(Node, Gs_mon));

	{new_backtrace,Size}  ->
	    loop(backtrace(Gs_mon,Size));

	{update_windows, Data} ->
	    dbg_ui_winman:update_windows_menu (Data),
	    loop (Gs_mon);

	{gs,_,motion,_,[X,Y]} ->
	    loop(flush_motion(X,Y,Gs_mon));

	T when element(1,T) == gs ->
	    loop(gs_cmd(T,Gs_mon));

	What -> 
	    loop(Gs_mon)
    end.


%%% stack_trace(Opt,Gs_mon) -> #gs_mon
%%% Called when the stack options have been changed. The menus are
%%% updated, as is GS_mon
 
stack_trace(Opt,Gs_mon) ->
    On = case Opt of
	     all     -> 'Stack Tail';
	     true    -> 'Stack Tail';
	     no_tail -> 'Stack';
	     _       -> 'Stack Off'
	 end,
    dbg_ui_aux:select_menus([On],true),
    dbg_idb:insert(stack_defs,Opt),
    Gs_mon#gs_mon{stack=Opt}.

%%% auto_attached(Opt,Gs_mon) -> #gs_mon
%%% Called when the auto attach options have been changed. The menus are
%%% updated, as is GS_mon

auto_attach(Opt,Gs_mon) ->
    case Opt of
	false ->
	    Menus = ['First Call','On Break','On Exit'],
	    dbg_ui_aux:select_menus(Menus,false);
	{Opts,_} ->
	    lists:map({?MODULE,attach_menus},[],Opts)
    end,
    Gs_mon#gs_mon{attach=Opt}.

attach_menus(init) ->
    dbg_ui_aux:select_menus(['First Call'],true);
attach_menus(exit) ->
    dbg_ui_aux:select_menus(['On Exit'],true);
attach_menus(break) ->
    dbg_ui_aux:select_menus(['On Break'],true).

%%% trace_flag(Opt,Gs_mon) -> #gs_mon
%%% Called when the trace flags options have been changed.
%%% This is ignored, seen that trace flags should be controled
%%% by the individual processes.

trace_flag(Flag,Gs_mon) ->
    %%Ignore global trace flags
    Gs_mon.


get_flags(#gs_mon{trace=Trace,bind=Bind,
		 button=Button,eval=Eval})->	   
    {Button,Eval,Bind,Trace}.

%%% clear(Gs_mon) -> #gs_mon
%%% Removes all the dead processes from the debugger and monitor 
%%% window.

clear(Gs_mon) ->
    int:clear(),
    Gs_mon.

clear_grid(Gs_mon) ->
    Grid = Gs_mon#gs_mon.grid,
    Focus = Gs_mon#gs_mon.focus,
    FocusData = get_focus_data(Gs_mon),

    %% Get a list of Pids that should remain after the 'Clear'.
    NewPids = clear_pids(Gs_mon#gs_mon.pids),
    Size = length(NewPids),


    %% Update the information in the grid.
    dbg_ui_mon_win:update_pids(Grid,NewPids,Size,Focus),

    NewFocus = set_focus(FocusData,filter_info(NewPids),Focus),

    NewGs_mon = Gs_mon#gs_mon{pids=NewPids,size=Size,focus=NewFocus},
    check_focus_buttons(NewGs_mon),

    %% Delete unused gridlines in the grid.
    dbg_ui_mon_win:delete_items(NewGs_mon#gs_mon.grid,Size+1),

    dbg_ui_mon_win:change_colour(Grid, Focus, NewFocus),
    NewGs_mon.



clear_pids(Pids) -> 
    clear_pids(Pids,1).


clear_pids([{_,P,_,_,exit,_,_,_}|Pids],Count) ->
    dbg_ui_winman:delete_att_win (P),
    clear_pids(Pids,Count);


clear_pids([{_,P,_,_,no_conn,_,_,_}|Pids],Count) ->
    dbg_ui_winman:delete_att_win (P),
    clear_pids(Pids,Count);


clear_pids([{_R,P,M,C,S,I,E,N}|Pids],Count) ->
    [{Count,P,M,C,S,I,E,N}|clear_pids(Pids,Count+1)];


clear_pids([],_) ->
    [].



%%% new_status(StatTup,Gs_mon) -> #gs_mon
%%% StatTup = {Pid,Meta,Func,Status,Where,Exit}
%%% New Status is called whenever a running process changes
%%% status. The monitor window is updated, as is the information 
%%% in the GS_mon record.

new_status(StatTup,Gs_mon) ->
    Pids = Gs_mon#gs_mon.pids,
    {Pid,Meta,Func,Status,Where,Exit} = flush_new_status(StatTup),
    {value,{Row,_,_,_,_,_,_,Name}} = lists:keysearch(Pid,2,Pids),
    Grid = Gs_mon#gs_mon.grid,
    Data = {pidfunc,Pid,Status},
    dbg_ui_mon_win:update_field(Gs_mon#gs_mon.grid,{4,Row},Status,Data),
    case Where of
	{} ->
	    dbg_ui_mon_win:update_field(Gs_mon#gs_mon.grid,{5,Row},"",Data);
	_ ->
	    dbg_ui_mon_win:update_field(Gs_mon#gs_mon.grid,{5,Row},Where,Data)
    end,
    P = lists:keyreplace(Pid,2,Pids,{Row,Pid,Meta,Func,Status,Where,Exit,Name}),
    NewGs_mon = Gs_mon#gs_mon{pids=P},
    check_focus_buttons(NewGs_mon),
    NewGs_mon.

flush_new_status(StatTuple) ->
    Pid = element(1,StatTuple),
    receive
	{IntPid,new_status,NewTuple} when element(1,NewTuple) == Pid ->
	    flush_new_status(NewTuple)
    after 0 ->
	    StatTuple
    end.

%%% new_proc(StatTup,Gs_mon) -> #gs_mon
%%% StatTup = a tuple describing the newly attached process
%%% Usen whenever a new process is to be displaied in the monitor window

new_proc(StatTup,Gs_mon) ->
    Grid = Gs_mon#gs_mon.grid,
    Size = Gs_mon#gs_mon.size+1,
    Focus = Gs_mon#gs_mon.focus,
    NewNodes = monitor_nodes(StatTup,Gs_mon#gs_mon.nodes),
    Proc = format_proc(length(Gs_mon#gs_mon.pids),StatTup),
    dbg_ui_mon_win:update_pids(Grid,[Proc],Size,Focus),
    Gs_mon#gs_mon{pids= Gs_mon#gs_mon.pids ++ [Proc],
		  size= Size,
		  nodes = NewNodes}.

%%% new_mods(Gs_mon,[Mod|Mods]) -> #gs_mon
%%% Mods = list of interpreted modules
%%% called whenever a new set of modules have been trace compiled
%%% Menues are added, and possible turned on depending on the state

new_mods(Gs_mon,[]) -> Gs_mon;
new_mods(Gs_mon,[Mod|Mods]) ->
    new_mods(new_mod(Gs_mon,Mod),Mods).

new_mod(Gs_mon, Mod) ->
    case lists:member(Mod,Gs_mon#gs_mon.mods) of
	true  -> Gs_mon;
	false ->
	    dbg_ui_aux:add_menu('Delete',Mod),
		 dbg_ui_aux:add_menu('View',Mod),
		 case length(Gs_mon#gs_mon.mods) of
		     0 -> dbg_ui_aux:enable_menus(['DeleteMenu','ViewMenu',
						    'Delete All Modules']);
		     _ -> ok
		 end,
		 Gs_mon#gs_mon{mods = [Mod|Gs_mon#gs_mon.mods]}
    end.

%%% backtrace(Gs_mon,Size) -> #gs_mon
%%% Size: New default size of the back trace
backtrace(Gs_mon,Size) ->
    dbg_ui_mon_win:update_backtrace_menu(Size),
    dbg_idb:insert(backtrace_def,Size),
    Gs_mon#gs_mon{btrace = Size}.

%%% no_interpret(Gs_mon, Mod) -> #gs_mon
%%% A module which was trace compile has been deleted from the
%%% database. 

no_interpret(Gs_mon, Mod) ->
    Del  = list_to_atom(lists:flatten(io_lib:format("~p~p",['Delete',Mod]))),
    View = list_to_atom(lists:flatten(io_lib:format("~p~p",['View',Mod]))),
    dbg_ui_aux:remove_menu(Del),
    dbg_ui_aux:remove_menu(View),
    case length(Gs_mon#gs_mon.mods) of
	1 -> dbg_ui_aux:disable_menus(['DeleteMenu','ViewMenu',
					'Delete All Modules']);
	_ -> ok
    end,
    Gs_mon#gs_mon{mods = lists:delete(Mod,Gs_mon#gs_mon.mods)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The following calls are executed whenever there has been a change in
%%% the state of all breaks. Menus are either updated, turned on, 
%%% or added to the monitor window. The state is determined by the 
%%% tuple passed to the function call. The return value is always 
%%% the new #gs_mon record, updated with the new break information.

break(Gs_mon,{new_break,{{Mod,Line},Status}}) ->
    Breaks = Gs_mon#gs_mon.breaks,
    case lists:keysearch({Mod,Line},1,Breaks) of
	false ->
	    dbg_ui_aux:add_break(Mod,Line,Status,length(Breaks));
	_ ->
	    dbg_ui_aux:new_break_options(Mod,Line,Status)
    end,
    Gs_mon#gs_mon{breaks = Breaks ++ [{{Mod,Line},Status}]};

break(Gs_mon,{delete_break,{Mod,Line}})->
    Breaks = Gs_mon#gs_mon.breaks,
    dbg_ui_aux:delete_break(Mod,Line,length(Breaks)-1),
    Gs_mon#gs_mon{breaks = lists:keydelete({Mod,Line},
					   1,Breaks)};

break(Gs_mon,{new_break_options,{{Mod,Line},Status}})->
    dbg_ui_aux:new_break_options(Mod,Line,Status),
    Gs_mon;

break(Gs_mon,{no_break,{}})->
    dbg_ui_aux:delete_breaks(Gs_mon#gs_mon.breaks,0),
    Gs_mon#gs_mon{breaks = []};
break(Gs_mon,{no_break,{Mod}})->
    {New,Old} = filter_mod_breaks(Mod,Gs_mon#gs_mon.breaks),
    dbg_ui_aux:delete_breaks(Old,length(New)),
    Gs_mon#gs_mon{breaks = New};

break(Gs_mon,Break)->
    Gs_mon.

filter_mod_breaks(Mod,Breaks) ->
    filter_mod_breaks(Mod,Breaks,{[],[]}).

filter_mod_breaks(_Mod,[],{New,Old}) -> {New,Old};
filter_mod_breaks(Mod,[{{Mod,Line},Opts}|Breaks],{New,Old}) ->
    filter_mod_breaks(Mod,Breaks,{New,[{{Mod,Line},Opts}|Old]});
filter_mod_breaks(Mod,[{{O_Mod,Line},Opts}|Breaks],{New,Old}) ->
    filter_mod_breaks(Mod,Breaks,{[{{O_Mod,Line},Opts}|New],Old}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  gs_cmd(Cmd,Gs_mon) -> #gs_mon
%%%  Cmd: a gs event received by the window, which was produced
%%%       by the user. 
%%%   this function redirects the flow of command to the functions
%%%   handling these events. 

gs_cmd(Cmd,Gs_mon) ->
    Win   = Gs_mon#gs_mon.win,
    case Cmd of
       
	%%Window manager commands
        {gs,Win,configure ,_,[X,Y|_]} ->
	    execute_cmd({configure,X,Y},Gs_mon);

	{gs,_W,destroy,_,_} ->
	    exit_debugger (Gs_mon, destroy);

	%%Dynamic commands
	{gs,Menu_Id, click,{break,{Module,Line,Action}} , _A} ->
	    execute_cmd({break, {Module,Line,Action,Menu_Id}},Gs_mon);

	{gs,_W,click,T, _Args} when tuple(T), size(T) == 2 ->
	    execute_cmd(T,Gs_mon);

	{gs,GridLine,click,{pidfunc,_Pid,_Comm},[C,R|_]} when integer(R) ->
	    execute_cmd({focus,R},Gs_mon);
	
	{gs,Gridline,doubleclick,{pidfunc,_P,_C},[C,R| _T]} when integer(R) ->
	    execute_cmd('Trace',Gs_mon);
      

	%%Menu Commands

        %% manages the Windows menu - puts the choosen window on top
        {gs, _, click, [win_menu, Win_Pid], _}     ->
            dbg_ui_winman:on_top(Win_Pid),
            Gs_mon;

        {gs,Command, click, _D, _A}     ->
	    execute_cmd(Command,Gs_mon);
      

        %% Keyboard accelerator commands
	{gs,_W,keypress,[],['Up',_,0,0]}   ->
	    execute_cmd({focus,Gs_mon#gs_mon.focus-1},Gs_mon);

	{gs,_W,keypress,[],['Down',_,0,0]} ->
	    execute_cmd({focus,Gs_mon#gs_mon.focus+1},Gs_mon);

	{gs,_W,keypress,[],[p,_,0,1]} ->
	    execute_cmd({focus,Gs_mon#gs_mon.focus-1},Gs_mon);

	{gs,_W,keypress,[],[n,_,0,1]} ->
	    execute_cmd({focus,Gs_mon#gs_mon.focus+1},Gs_mon);

	{gs,_W,keypress,[],['Return',_,0,0]} ->
	    execute_cmd('Trace',Gs_mon);

        {gs,_W,keypress,D,[Key,_,_,1]} ->
	    execute_cmd(key(Key),Gs_mon);

	Other ->
	    Gs_mon
    end.


% Start of additions made by Fredrik Gustafson

%%% Help Menu

execute_cmd('Help',Gs_mon) -> 
    HelpFile = filename:join(code:priv_dir(debugger), "../doc/index.html"),
    tool_utils:open_help(gs:start([{kernel, true}]), HelpFile),
    Gs_mon;

% End of additions made by Fredrik Gustafson



%%% File Menu

execute_cmd('Save',Gs_mon) -> 
    save_setting(Gs_mon),
    Gs_mon;
execute_cmd('Load',Gs_mon) -> 
    load_setting(Gs_mon);
execute_cmd('Reset Options',Gs_mon) -> 
    reset_options (Gs_mon);
execute_cmd('Exit',Gs_mon) -> 
    confirm_exit_debugger (Gs_mon, exit);      
						
%%% Edit Menu

execute_cmd('Clear',Gs_mon) -> 
    clear(Gs_mon);
execute_cmd('Kill All',Gs_mon) -> 
    kill_all(tl(Gs_mon#gs_mon.pids)),Gs_mon;

%%% Modules Menu

execute_cmd('Interpret',Gs_mon) ->
    Pos = get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    dbg_ui_interpret:start({int,ni,[]},
			  Pos,
			  "Interpret Dialog",
			  Gs_mon#gs_mon.dir, 
			  Gs_mon#gs_mon.table),
    Gs_mon;
execute_cmd('Delete All Modules',Gs_mon) ->
    delete_all(Gs_mon#gs_mon.mods),
    Gs_mon;
execute_cmd({'Delete',Mod},Gs_mon)  ->
    int:nn(Mod),
    Gs_mon;
execute_cmd({'View',Mod},Gs_mon)  ->
    dbg_ui_view:start(Mod),
    Gs_mon;


%%% Process Menu

execute_cmd({focus,To},Gs_mon) ->
    Real_to = check_focus_choice(To,Gs_mon#gs_mon.size),
    dbg_ui_mon_win:change_colour(Gs_mon#gs_mon.grid, Gs_mon#gs_mon.focus,
			      Real_to),
    NGs_mon = Gs_mon#gs_mon{focus = Real_to},

    check_focus_buttons(NGs_mon),
    NGs_mon;

execute_cmd('Trace',Gs_mon) ->
     case get_focus_data(Gs_mon) of
	false ->
	    Gs_mon;
	{true,{pidfunc,Pid,_}} ->
	     case lists:keysearch(Pid,2,Gs_mon#gs_mon.pids) of
		 {value,{_,Pid,Meta,Func,exit,Where,Exit,_}} -> 
		     WinFlags = get_flags(Gs_mon),
		     dbg_ui_trace:start({Pid,Where,Exit},WinFlags);
		 {value,{_,_,_,_,no_conn,_,_,_}} -> ok;
		 {value,{_,Pid,Meta,_,_,_,_,_}} ->
		     WinFlags = get_flags(Gs_mon),
		     dbg_ui_trace:start({Pid,Meta},WinFlags);
		 
		 %% FIXME: 
		 %% This case is added to remedy an intermittent problem
		 %% with the lists:keysearch() call not returning
		 %% expected values. (OTP-2246)
		 %% 
		 Catchall ->
		     ok				%Do nothing
	     end,
	     Gs_mon
     end;
execute_cmd('Attach',Gs_mon) ->
    int:auto_attach([init,break,exit],?ATTACH_FUNCTION),
    Gs_mon;
execute_cmd('Step',Gs_mon) ->
    case get_focus_meta(Gs_mon) of
	{ok,Meta} ->
	    dbg_icmd:step(Meta);
	false ->
	    ok
    end,
    Gs_mon;
execute_cmd('Next',Gs_mon) ->
    case get_focus_meta(Gs_mon) of
	{ok,Meta} ->
	    dbg_icmd:next(Meta);
	false ->
	    ok
    end,
    Gs_mon;
execute_cmd('Continue',Gs_mon) ->
    case get_focus_meta(Gs_mon) of
	{ok,Meta} ->
	    dbg_icmd:continue(Meta);
	false ->
	    ok
    end,
    Gs_mon;
execute_cmd('Finish',Gs_mon) ->
    case get_focus_meta(Gs_mon) of
	{ok,Meta} ->
	    dbg_icmd:finish(Meta);
	false ->
	    ok
    end,
    Gs_mon;
execute_cmd('Kill',Gs_mon) ->
    case get_focus_data(Gs_mon) of
	false ->
	    Gs_mon;
	{true,{pidfunc,Pid,_}} ->
	    exit(Pid,kill),
	    Gs_mon
    end;

%%% Breaks Menu
execute_cmd({break,{Module,Line,Action,MenuId}},Gs_mon) ->
    break_points(Gs_mon,Module,Line,Action,MenuId);
execute_cmd('Normal Break',Gs_mon) ->
    Pos = get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    dbg_ui_break:start(normal,{"",""},Pos), Gs_mon;
execute_cmd('Functional Break',Gs_mon) ->
    Pos = get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    dbg_ui_break:start(functional,"",Pos),Gs_mon;
execute_cmd('Conditional Break',Gs_mon) ->
    Pos = get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    dbg_ui_break:start(conditional,{"",""},Pos),Gs_mon;
execute_cmd('Delete All Breaks',Gs_mon) -> 
    int:no_break(),Gs_mon;

%%% Options Commands

execute_cmd('Never Attach',Gs_mon) ->
    int:auto_attach(false,{dbg_ui_trace,a_start}), Gs_mon;
execute_cmd('On Exit',Gs_mon) ->
    attach_cmd(exit,Gs_mon);
execute_cmd('On Break',Gs_mon) ->
    attach_cmd(break,Gs_mon);
execute_cmd('First Call',Gs_mon) ->
    attach_cmd(init,Gs_mon);

execute_cmd('Stack',Gs_mon) ->
    dbg_idb:insert(stack_defs,no_tail),
    %%dbg_icmd:stack_trace(no_tail),
    Gs_mon#gs_mon{stack=no_tail};
execute_cmd('Stack Tail',Gs_mon) ->
    dbg_idb:insert(stack_defs,true),    %dbg_icmd:stack_trace(true),
    Gs_mon#gs_mon{stack=true};
execute_cmd('Stack Off',Gs_mon) ->
    dbg_idb:insert(stack_defs,false),    %%dbg_icmd:stack_trace(false),
    Gs_mon#gs_mon{stack=false};

execute_cmd('Back Trace',Gs_mon)  ->
    Pos =  get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    dbg_ui_aux:back_trace_window(Pos),
    Gs_mon;

execute_cmd('Bindings Frame',Gs_mon) ->
    Flag = switch_state(gs:read('Bindings Frame',select)),
    NGs_mon = Gs_mon#gs_mon{bind = Flag},
    NFlags = get_flags(NGs_mon),
    dbg_idb:insert(frame_defs,NFlags),
    NGs_mon;

execute_cmd('Evaluator Frame',Gs_mon) ->
    Flag = switch_state(gs:read('Evaluator Frame',select)),
    NGs_mon = Gs_mon#gs_mon{eval = Flag},
    NFlags = get_flags(NGs_mon),
    dbg_idb:insert(frame_defs,NFlags),
    NGs_mon;

execute_cmd('Button Frame',Gs_mon) ->
    Flag = switch_state(gs:read('Button Frame',select)),
    NGs_mon = Gs_mon#gs_mon{button = Flag},
    NFlags = get_flags(NGs_mon),
    dbg_idb:insert(frame_defs,NFlags),
    NGs_mon;

execute_cmd('Trace Flag',Gs_mon) ->
    Flag = switch_state(gs:read('Trace Flag',select)),
    NGs_mon = Gs_mon#gs_mon{trace = Flag},
    NFlags = get_flags(NGs_mon),
    dbg_idb:insert(frame_defs,NFlags),
    NGs_mon;



%%% The window has been reconfigured.

execute_cmd({configure,X,Y},Gs_mon) ->
    dbg_ui_mon_win:config_win(Gs_mon#gs_mon.win,Gs_mon#gs_mon.grid,{X,Y}),
    dbg_ui_mon_win:config_grid(Gs_mon#gs_mon.grid, {X, Y}),
    Gs_mon;

%%% Unknown command
execute_cmd(Other,Gs_mon) ->
    Gs_mon.

%%% An new auto attach option has been chosen.

attach_cmd(Which,Gs_mon) ->
    case Gs_mon#gs_mon.attach of
	false -> int:auto_attach(Which);
    {On,_}  ->
	    case lists:member(Which,On) of
		true ->
		    int:auto_attach(lists:delete(Which,On),
				    {dbg_ui_trace,a_start});
		false ->
		    int:auto_attach([Which|On],
				    {dbg_ui_trace,a_start})
	    end
    end,
    Gs_mon.

%%% Kill all attached processes

kill_all([]) -> ok;
kill_all([Pid|Pids]) ->
    exit(element(2,Pid),kill),
    kill_all(Pids).

%%% Delete all trace compiled modules.

delete_all([]) ->     ok;
delete_all([Mod|Mods]) ->
    int:nn(Mod),
    delete_all(Mods).

%%% Load settings from the current working directory

load_setting(Gs_mon) ->
    make_dir (),
    Pos =  get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),

    case file_to_load (Pos) of
	{error, Error} ->
	    Text = io_lib:format("Error, could not load file~n~s", [Error]),
	    dbg_ui_aux:message_window (Text, self (), Pos),
	    Gs_mon;

	{ok, List} ->
	    Busy_win_pid =
		dbg_ui_aux:start_busy_window (Gs_mon#gs_mon.win,
					      "Loading settings..."),
            dbg_ui_aux:mark_busy (Gs_mon#gs_mon.win),
            NGs_mon = initialize_setting(Gs_mon,List),
            dbg_ui_aux:stop_busy_window (Busy_win_pid),
            dbg_ui_aux:mark_nonbusy (Gs_mon#gs_mon.win),
            NGs_mon;

	no_file ->
	    Gs_mon
    end.



%%% file_to_load  /2
%%%
%%% file_to_load returns a file or no_file
%%%
%%% Pre:
%%%    Pos   ==  tuple
%%%              {X, Y}
%%%
%%% Def:
%%%    file_to_load  ==  a file  ||  no_file
%%%

file_to_load (Pos) ->
    case dbg_ui_get_file:get_file ('Load', Pos) of
	no_file ->
	    no_file;
	
	File ->
	    load_file (list_to_atom (File))
    end.



load_file (File) ->
    case file:read_file (File) of
        {ok, Bin} -> 
	    {ok, binary_to_term(Bin)};

	{error, Reason} -> 
	    {error, Reason}
    end.



%%% Initialize all settings in accordance to the elements in List

initialize_setting(Gs_mon,List) ->
    [{load, Load},
     {mods, Mods},
     {breaks, Breaks},
     {attach, Attach},
     {trace, Trace},
     {eval, Eval},
     {bind, Bind},
     {button, Button},
     {stack, Stack},
     {btrace, Btrace},
     {dir, Dir},
     {opts, Opts}] = List,

    load_opts (Gs_mon#gs_mon.table, Opts),
    Opts2 = dbg_ui_compilerdefs:get_opts (Gs_mon#gs_mon.table, 
					  Gs_mon#gs_mon.dir),
    file_interpret(Mods, Opts2),
    file_breaks(Breaks),
    file_attach(Attach),
    dbg_ui_mon_win:update_backtrace_menu(Btrace),
    dbg_idb:insert(backtrace_def,Btrace),
    NGs_mon = stack_trace(Stack,Gs_mon),
    set_frame_buttons({Button,Eval,Bind,Trace}),
    NGs_mon#gs_mon{load=Load,eval=Eval,bind=Bind,trace=Trace,
		  button = Button,btrace=Btrace,dir=Dir}.

%%% Handle special cases needed when loading settings.

switch_bool(open)  -> true;
switch_bool(close) -> false.

set_frame_buttons({Button,Eval,Bind,Trace}) ->
    gs:config('Bindings Frame',{select,switch_bool(Bind)}),
    gs:config('Evaluator Frame',{select,switch_bool(Eval)}),
    gs:config('Button Frame',{select,switch_bool(Button)}),
    gs:config('Trace Flag',{select,switch_bool(Trace)}),
    dbg_idb:insert(frame_defs,{Button,Eval,Bind,Trace}).
    
file_attach(false) -> int:auto_attach(false);
file_attach({When,{Mod,Func,Node}}) ->
    int:auto_attach(When,{Mod,Func}).

file_interpret([Mod|Mods], Opts) ->
    int:ni(Mod, Opts),
    file_interpret(Mods ,Opts);
file_interpret([], _) -> ok.

file_breaks([{{Mod,Line},[Status,Action,_,Cond]}|Breaks]) ->
    int:break(Mod,Line),
    break_status(Mod,Line,Status),
    break_action(Mod,Line,Action),
    break_cond(Mod,Line,Cond),
    file_breaks(Breaks);
file_breaks([]) -> ok.

break_status(Mod,Line,active) ->
    int:enable_break(Mod,Line);
break_status(Mod,Line,inactive) ->
    int:disable_break(Mod,Line).
break_action(Mod,Line,Action) ->
    int:action_at_break(Mod,Line,Action).
break_cond(Mod,Line,{TestMod,TestFunc}) ->
    int:test_at_break(Mod,Line,{TestMod,TestFunc});
break_cond(_,_,_) -> ok.

%%% Save the settings.

save_setting(Gs_mon) ->
    make_dir (),
    Pos =  get_cursor_pos(Gs_mon#gs_mon.win,Gs_mon#gs_mon.coords),
    Mods = get_abs_modules (),
    Opts = dbg_ui_compilerdefs:get_complete_opts (Gs_mon#gs_mon.table,
						  Gs_mon#gs_mon.dir),
    
    Data = term_to_binary([{load, Gs_mon#gs_mon.load},
			   {mods, Mods},
			   {breaks, int:all_breaks()},
			   {attach, Gs_mon#gs_mon.attach},
			   {trace, Gs_mon#gs_mon.trace},
			   {eval, Gs_mon#gs_mon.eval},
			   {bind, Gs_mon#gs_mon.bind},
			   {button, Gs_mon#gs_mon.button},
			   {stack, Gs_mon#gs_mon.stack},
			   {btrace, Gs_mon#gs_mon.btrace},
			   {dir, Gs_mon#gs_mon.dir},
			   {opts, Opts}]),

    case file_to_save (Pos, Data) of
	ok ->
	    ok;

	no_file ->
	    no_file;

	{error,Error} ->
	    Text = io_lib:format("Error, could not save file~n~s", [Error]),
	    dbg_ui_aux:message_window(Text,self(),Pos),
	    error
    end.
    


%%% file_to_save  /2
%%%
%%% file_to_save returns a file or no_file
%%%
%%% Pre:
%%%    Pos   ==  tuple
%%%              {X, Y}
%%%    Data  ==  binary
%%%              data to store in file
%%%
%%% Def:
%%%    file_to_save  ==  a file  ||  no_file
%%%

file_to_save (Pos, Data) ->
    case dbg_ui_get_file:get_file ('Save', Pos) of
	no_file ->
	    no_file;
	
	File ->
	    save_file (list_to_atom (File), Data)
    end.



save_file (File, Data) ->
    case file:write_file(File, Data) of
        ok -> 
	    ok;
	
	{error, Reason} -> 
	    {error, Reason}
    end.



%%% make_dir  /0
%%%
%%% make_dir returns the default directory for the setting files
%%%

make_dir () ->
    Dir = tool_utils:appstate_filename (debugger, "."),
    tool_utils:mkdir_for_file (Dir).



%%% get_abs_modules  /0
%%%
%%% get_abs_modules returns the interpreted modules with their
%%% absolute path.
%%%

get_abs_modules () ->
    get_abs_modules (int:interpreted (), []).



get_abs_modules ([], L) ->
    lists:reverse (L);


get_abs_modules ([H | T], L) ->
    get_abs_modules (T, [int:file (H) | L]).



%%% reset_options /1
%%%
%%% reset_options sets the options to the default start up settings
%%%

reset_options (Gs_mon) ->
    Gs_mon1 = execute_cmd ('Never Attach', Gs_mon),
    Gs_mon2 = execute_cmd ('Stack Tail', Gs_mon1),
    
    gs:config ('Stack Tail', {select, true}),
    gs:config ('Button Frame', {select, true}),
    gs:config ('Evaluator Frame', {select, true}),
    gs:config ('Bindings Frame', {select, true}),
    gs:config ('Trace Flag', {select, false}),

    dbg_idb:insert (frame_defs,
		   {open, open, open, close}), % {button, eval, bind, trace}

    Gs_mon2#gs_mon{button = open, 
		   eval = open, 
		   bind = open, 
		   trace = close}.
    
    
    
%%% exit_debugger  /2
%%%

exit_debugger (Gs_mon, Reason) ->
    [H1 | Metas] = get_pids (3, Gs_mon#gs_mon.pids, []),
    [H2 | Pids] = get_pids (2, Gs_mon#gs_mon.pids, []),

    %% Remove all metas and pids to avoid exit-attach windows 
    %% after closing the debugger.

    lists:foldl (fun(Pid, Sum) -> exit(Pid, kill), Sum + 1 end, 0, Metas),
    lists:foldl (fun(Pid, Sum) -> exit(Pid, kill), Sum + 1 end, 0, Pids),

    delete_all(Gs_mon#gs_mon.mods),  
    reset_options (Gs_mon),
    clear (Gs_mon),
    dbg_ui_winman:delete_win (Gs_mon#gs_mon.win),
    exit (Reason).



get_pids (_N, [], L) ->
    lists:reverse (L);

get_pids (N, [H | T], L) when element (5, H) == exit ->
    get_pids (N, T, L);

get_pids (N, [H | T], L) ->
    get_pids (N, T, [element (N, H) | L]).



%%% confirm_exit_debugger  /2
%%%

confirm_exit_debugger (Gs_mon, Reason) ->
    case confirm_exit () of
	yes ->
	    case save_setting (Gs_mon) of
		ok ->
		    exit_debugger (Gs_mon, Reason);
		
		no_file ->
		    confirm_exit_debugger (Gs_mon, Reason);

		_error ->
		    Gs_mon
	    end;
	
	no ->
	    exit_debugger (Gs_mon, Reason);

	_cancel ->
	    Gs_mon
	
    end.



%%% confirm_exit  /0
%%%

confirm_exit () ->
    tool_utils:confirm_exit (gs:start (), 
			     ["Would you like to save your settings",
			      "before you exit?"]).



%%% Handle all change of states in breaks requested by the user.

break_points(GS_mon,Module,Line,delete,ID) ->
    int:delete_break(Module,Line),GS_mon;
break_points(GS_mon,Module,Line,inactive,ID) ->
    int:disable_break(Module,Line),GS_mon;
break_points(GS_mon,Module,Line,active,ID) ->
    int:enable_break(Module,Line),GS_mon;
break_points(GS_mon,Module,Line,trigger_disable,ID) ->
    int:action_at_break(Module,Line,disable),GS_mon;
break_points(GS_mon,Module,Line,trigger_enable,ID) ->
    int:action_at_break(Module,Line,enable),GS_mon;
break_points(GS_mon,Module,Line,trigger_delete,ID) ->
    int:action_at_break(Module,Line,delete),GS_mon;
break_points(GS_mon,Module,Line,Action,ID) ->
    GS_mon.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Help Functions

%%% Empty the message queue from all motion eventsin the 
%%% window, saving the last x,y coordinates of the mouse cursor.
flush_motion(X,Y,Gs_mon) ->
    receive
	{gs,_,motion,_,[NX,NY]} -> flush_motion(NX,NY,Gs_mon)
    after 0 ->
	    Gs_mon#gs_mon{coords = {X,Y}}
    end.

%%% Get the last known cursor position in the monitor window
get_cursor_pos(Win,{X,Y}) ->
    WinX = gs:read(Win,x),
    WinY = gs:read(Win,y),
    {WinX+X-5,WinY+Y-5}.

switch_state(false) -> close;
switch_state(true)  -> open.

%%% Get the meta process for an attached process.

get_focus_meta(Gs_mon) ->
     case get_focus_data(Gs_mon) of
	 {true,{pidfunc,Pid,_}} ->
	     case lists:keysearch(Pid,2,Gs_mon#gs_mon.pids) of
		 {value,{_,_,Meta,_,_,_,_,_}} -> {ok,Meta};
		 _                            -> false
	     end;
	 _  -> false
     end.

%% -----------------------------------------------------------------
%%% Format the information on the processes prior to their being
%%% printed out in the monitor grid.

format_procs(Procs) ->
    format_procs(Procs,[{1,'Pid',{},'Initial Call','Status','Information'
			 ,{},'Name'}],1).
format_procs([], Info, _) ->
    lists:reverse(Info);
format_procs([Head|Tail], Buffer,Pos) ->
    Form = format_proc(Pos,Head),
    format_procs(Tail,[Form|Buffer],Pos+1).
format_proc(Pos,{Pid,Meta,Func,Status,Where,Exit}) ->
    Name = registered_name(node(Pid),node(),Pid),
    {Pos+1,Pid,Meta,Func,Status,Where,Exit,Name}.

%%% Find the registered name of a process.

registered_name(Node,Node,Pid) ->  %handles case when local/not distributed
    case erlang:process_info(Pid,registered_name) of
	{registered_name,Reg} -> Reg;
	_                     -> ""
    end;
registered_name(Node,_LocalNode,Pid) ->
    case rpc:call(Node,erlang,process_info,[Pid,registered_name]) of
	{registered_name,Reg} -> Reg;
	_                     -> ""
    end.

%% ---------------------------------------------------------------
%% Focus is the highlited process in the grid. This set of 
%% functions keep track of it and handle changes to it, cheking
%% if the change is legitimate. It also retrives the data stored
%% for each process which is currently on focus.
%% ---------------------------------------------------------------

check_focus_choice(To,      0) ->  1;
check_focus_choice(0,    Size) ->  Size; 
check_focus_choice(To,   Size) ->  
    case Size+1 of
	To ->
	    1;
	_  ->
	    To
    end.

get_pos(List,Element) ->
    get_pos(List,Element,1).
get_pos([],_,_) ->
    false;
get_pos([Element|Rest],Element,Count) ->
    Count;
get_pos([_|Rest],Element,Count) ->
    get_pos(Rest,Element,Count+1).

filter_info([_Static|Rest]) ->
    filter(Rest,[]).
filter([],Buff) -> lists:reverse(Buff);
filter([{_,Pid,_,_,_,_,_,_}|Rest],Buff) ->
    filter(Rest,[Pid|Buff]).

get_focus_data(Gs_mon) ->
    case Gs_mon#gs_mon.focus of
	1   ->
	    false;
	Row ->
	    GridLine = gs:read(Gs_mon#gs_mon.grid, 
			       {obj_at_row, Row}),

	    D = gs:read(GridLine,data),

	    {true, D}
    end.

set_focus(false,_Pids,Current) ->
    1;
set_focus({true,{pidfunc,Pid,_}},Pids,Current) ->
    case get_pos(Pids,Pid) of
	false ->
	    focus1(length(Pids),Current);
	Pos   ->
	    Pos+1
    end.

focus1(Size,Current)  when Size >= Current ->
    Current+1;
focus1(Size,_Current) ->
    Size+1.

%%% Enable/disable buttons depending on the state of the
%%% Process currently in Focus.
check_focus_buttons(Gs_mon) ->
    case Gs_mon#gs_mon.focus of
	1 ->
	    dbg_ui_aux:disable_menus(['Trace','Step','Next','Continue',
				      'Finish','Kill']);
	Other ->
	    case get_focus_data(Gs_mon) of
		{true,{pidfunc,_,exit}} -> 
		    dbg_ui_aux:disable_menus(['Kill','Step','Next',
					      'Continue','Finish']),
		    dbg_ui_aux:enable_menus(['Trace']);
		{true,{pidfunc,_,_break}} -> 
		    dbg_ui_aux:enable_menus(['Kill','Continue',
					     'Finish','Trace',
					     'Step','Next']);
		{true,{pidfunc,_,_State}} -> %%running, waiting, more?
		    dbg_ui_aux:enable_menus(['Kill','Trace']),
		    dbg_ui_aux:disable_menus(['Continue','Finish',
					      'Step','Next'])
	    end
    end.



%%%Key accelerators
key(K) ->
    Command =  key_acc(K),
    case catch gs:read(Command,enable) of
	true -> Command;
	_    -> false
    end.

key_acc(e) -> 'Exit';
key_acc(l) -> 'Load';
key_acc(s) -> 'Save';
key_acc(f) -> 'Default';

key_acc(c) -> 'Clear';

key_acc(i) -> 'Interpret';
key_acc(a) -> 'Delete All Modules';

key_acc(t) -> 'Trace';
key_acc(z) -> 'Step';
key_acc(x) -> 'Next';
key_acc(o) -> 'Continue';
key_acc(h) -> 'Finish';
key_acc(k) -> 'Kill';

key_acc(b) -> 'Normal Break';
key_acc(r) -> 'Conditional Break';
key_acc(d) -> 'Delete All Breaks';

key_acc(_) -> false.



load_opts (Table, [{Key, L} | T]) ->
    dbg_ets:insert(Table, {Key, L}),
    load_opts (Table, T);
load_opts (_, []) ->
    ok.
