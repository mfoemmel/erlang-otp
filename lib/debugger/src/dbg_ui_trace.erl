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
%%% Purpose : dbg_ui_trace is the user interface to the trace window in the
%%%           new debugger. It starts the trace window and then stays in
%%%           a receive loop.
%%% History : The int_show_new module has been divided into two seperate 
%%%           files for the view and trace (attach) windows in the new 
%%%           debugger.
%%%----------------------------------------------------------------------

-module (dbg_ui_trace).

-include ("dbg_ui_data_struct.hrl").
-export ([start/2, a_start/3, a_start/4, init/2]).

-define(FRAMEDEFS,{open,open,open,close}).
-define(STACKDEF,all).
-define(BACKTRACE,100).
-define(ATTACH_FUNCTION,{?MODULE, a_start}).


-define(STEP_ACTIONS,['Next','NextMenu',
		      'Step','StepMenu',
		      'Finish','FinishMenu',
		      'Continue','ContinueMenu',
		      'Skip', 'SkipMenu',
		      'Break']).

-define(STACK_ACTIONS, ['Up','UpMenu',
			'Down','DownMenu']).

-define(MESSAGE_ACTIONS, ['Messages','MessagesMenu']).

-define(TIMEOUT_ACTIONS, ['Time Out','Time OutMenu']).

-define(WHERE_ACTIONS,['Where','WhereMenu']).

-define(STOP_ACTIONS, [ 'Stop','StopMenu']).
%%-----------------------------------------------------------------
%% start(Data,Mode,Flag) -> Pid
%% Data: {Pid,Meta}|{PidReason,Exit}|Module
%% Mode: view|trace
%% Flags: FrameOptions
%% The start function called from debugger.erl

start(Data, Flag) ->
    spawn_link(?MODULE, init, [Data, Flag]).



%% a_start(What,Pid,Meta) -> Pid
%% What: false|Node
%% Pid: The pid we are tracing
%% Meta: The process tracing Pid
%% Process is still alive. 
%% The start functions for processes which are auto attached.

a_start(false,Pid,Meta) ->
    start({Pid, Meta}, get_frame());

a_start(Node, Pid, Meta) ->
      spawn_link(Node, ?MODULE, init, [{Pid, Meta}, get_frame()]).



%% a_start(What,Pid,Reason,Exit) -> Pid
%% What: false|Node
%% Pid: The pid which died
%% Reason: Reason why the Pid died
%% Exit: Exit Data
%% The start functions for processes which are auto attached on Exit

a_start(false, Pid, Reason, Exit) ->
    start({Pid, Reason, Exit}, get_frame());

a_start(Node, Pid, Reason, Exit) ->
    spawn_link(Node, ?MODULE, init, [{Pid, Reason, Exit}, get_frame()]).



%%% Create the window

init (Data, Flags) ->
    Title = init_format(Data),

    case dbg_ui_winman:win_started (self (), Title) of
	true ->
	    exit (self (), kill),
            exit (already_exists);

	_false ->
	    ok
    end,
    
    dbg_ui_cache:start (self ()),  % initiate this window at dbg_ui_cache
    Win = dbg_ui_trace_win:create_attach_window(Title, trace, Flags),
    dbg_ui_winman:insert_win (trace, Win, self ()),
    setup_loop(Data, Win, Flags).



%%% init_format(Data,Mode) -> {WindowTitle,File|no_file}
%%% Data: ModuleName|{Pid,Meta}|{Pid,'Exit',Reason}
%%% Mode: trace|view

init_format (Data) ->
    case Data of
	{Pid,_} -> 
	    io_lib:format ("Attach Process ~p", [Pid]);
	{Pid,_,_} -> 
	    io_lib:format ("Exited Process ~p", [Pid])
    end.



trace_flag(Meta,open) ->
    dbg_icmd:trace_pid(Meta,on);

trace_flag(_Meta,close) ->
    ok. %dbg_icmd:trace_pid(Trace#trace.meta,off)

    

    
%% setup_loop({Pid,Meta},Win,Flags).
%% Pid: The pid we are tracing
%% Meta: The process tracing Pid
%% Flags: FrameOptions
%% Window: WindowId
%% Start the trace loop for processes which are still alive,
%% setting up the environment needed.

setup_loop({Pid,Meta},Win,Flags) ->
    process_flag(trap_exit,true),
    BackTrace = get_backtrace(),
    dbg_ui_trace_win:update_backtrace_menu(BackTrace),
    {Button,Eval,Bind,TraceFrame} = Flags,
    trace_flag(Meta,TraceFrame),
    X = gs:read(Win,width),
    Y = gs:read(Win,height),
    int:attach(Pid,Meta),
    link(Meta),
    Mods = lists:sort(int:interpreted()),
    {ok,Dir} = file:get_cwd(),
    Trace = #trace{state={running,dummy,dummy},
		   meta=Meta,
		   pid=Pid,
		   cm="",
		   trace=TraceFrame, 
		   button=Button,
		   eval=Eval,
		   bind=Bind, 
		   mods = [],
		   breaks =[],
		   stack = empty_stack(),
		   size={X,Y},
		   win=Win,
		   coords = {0,0}, 
		   btrace = BackTrace, 
		   dir = Dir,
		   line = 0},
    NTrace = stack_flag(Trace),

    loop(dbg_ui_aux:new_mods(NTrace,Mods));


%% setup_loop({Pid,Meta},Flags).
%% Pid: The pid which has died
%% Reason: Reason why the Pid died
%% Exit: Exit Data
%% Flags: FrameOptions
%% Start the trace loop for processes which have died,
%% Setting up the right environment.

setup_loop({Pid,Reason,Exit},Win,Flags) ->
    process_flag(trap_exit,true),
    BackTrace = get_backtrace(),
    dbg_ui_trace_win:update_backtrace_menu(BackTrace),
    {Button,Eval,Bind,TraceFrame} = Flags,
    Meta = int:attach(Pid,Reason,Exit),
    link(Meta),
    trace_flag(Meta,TraceFrame),
    X = gs:read(Win,width),
    Y=gs:read(Win,height),
    Mods = lists:sort(int:interpreted()),
    {ok,Dir} = file:get_cwd(),
    Trace = #trace{state={{exit,Reason} ,unknown,unknown},
		   meta=Meta, 
		   pid=Pid, 
		   cm="", 
		   dir = Dir,
		   trace=TraceFrame, 
		   button=Button,
		   eval=Eval,
		   bind=Bind,
		   mods=[],
		   breaks =[], 
		   stack = empty_stack(),
		   coords = {0,0},
		   size={X,Y},
		   win=Win,
		   btrace=BackTrace,
		   line = 0},
    NTrace = stack_flag(Trace),
    loop(dbg_ui_aux:new_mods(NTrace,Mods)).

%% loop(Trace).
%% The main loop of the application. All events from the
%% attach process are handled here. All gs (User) events, 
%% with the exception of the motion event, are handled
%% in other subroutines. FIXME: Split up, user events high
%% priority.

loop(Trace) ->
    Meta = Trace#trace.meta,
    Pid = Trace#trace.pid,

    receive
	%% Windows menu
	{update_windows, Data} ->
	    dbg_ui_aux:update_windows (Data),
	    loop(Trace);


	%% Execution States
	{P, trace_flag, TraceF} ->
	    loop(trace_frame_flag(Trace, TraceF));

	{P, attached, Mod, Line, TraceF} ->
	    loop(attached(Trace, Mod, Line, TraceF));

	{P, wait_at, Mod, Line, Sp} ->
	    loop(wait_at(Trace, Mod, Line, Sp));

	{P, wait_after_at, Mod, Line, Sp} ->
	    loop(wait_after_at(Trace, Mod, Line, Sp));

	{P, break_at, Mod, Line, Sp} ->
	    loop(break_at(Trace, Mod, Line, Sp, Meta));

	{P,interpret,Mod}     ->
	    if Mod == Trace#trace.cm ->
		    NTrace = Trace#trace{cm_obsolete=true},
		    loop(dbg_ui_aux:new_mod(NTrace,Mod));
	       true ->
		    loop(dbg_ui_aux:new_mod(Trace,Mod))
	    end;

	{P,no_interpret,Mod}  ->
	    loop(dbg_ui_aux:no_interpret(Trace, Mod));

	{P, running} ->
	    loop(running(Trace));

	{P, idle} ->
	    loop(idle(Trace));

	{P, exit_at, Mod, Line, Reason, Sp} ->
	    loop(exit_at(Trace, Mod, Line, Reason, Sp));

	{P, func_at, Mod, Line, Sp} ->
	    loop(func_at(Trace, Mod, Line, Sp));


	%% Search calls

	{editor_search, CallingPid, String, Pos, CaseS} ->
	    Editor = dbg_ui_cache:current_editor (self()),
	    dbg_ui_search:editor_search (CallingPid, Editor,
					 String, Pos, CaseS),
	    loop (Trace);

	{unmark_string, Editor, OldString} ->
	    dbg_ui_search:unmark_string (Editor, OldString),
	    loop (Trace);

	{gotoline, Editor, Line, OldLine} ->
	    dbg_ui_trace_win:select_line (Editor, undefined_mode, 
					  Line, OldLine),
	    loop (Trace);

	{clear_oldline, Editor, OldLine} ->
	    dbg_ui_trace_win:select_line (Editor, clear, 0, OldLine),
	    loop (Trace);


	%%Internal states

        {new_dir, Dir} ->
	    loop(Trace#trace{dir=Dir});

	{P, command_resp, NCmd, Reply} ->
	    loop(print_reply(Reply, Trace)); 

        {bs,  Bs} ->
	    dbg_ui_trace_win:update_bindings(Bs),
	    loop(Trace);  

	{back_trace, Txt} ->
	    loop(pp_backtrace(Trace, Txt));


	{Meta, re_entry, M, F} when pid(Meta) ->
	    if Trace#trace.cm_obsolete == true ->
		    dbg_ui_aux:load_file(M,M,Trace#trace.breaks, Pid, Trace#trace.win),
		    NTrace = Trace#trace{cm_obsolete=false},
		    loop(NTrace);
	       true ->
		    loop(Trace)
	    end;

	{IntServer, no_interpret, Module} ->
	    loop(Trace);


	{P, new_break, {{Mod, Line}, Status}} ->
	    loop(dbg_ui_aux:break(Trace, {new_break, {Mod, Line, Status}}));

	{P, no_break, Module} ->
	    loop(dbg_ui_aux:break(Trace, {no_break, Module}));

	{P, delete_break, Br} ->
	    loop(dbg_ui_aux:break(Trace, {delete_break, Br}));

	{P, new_break_options, I}->
	    loop(dbg_ui_aux:break(Trace, {new_break_options, I}));

       	{P,  which_func} ->
	    P ! {self(), this_func, ?ATTACH_FUNCTION},
            loop(Trace);  

	{P, trace, Str} ->
	    loop(trace(Trace, Str));

	{P, stack_trace_flag, [Flag|_]} ->
            loop(stack_trace(Trace, Flag));  		

	{P, stack_trace_flag, Flag} ->
            loop(stack_trace(Trace, Flag));

	{new_backtrace, Size} ->
	    loop(back_trace(Trace, Size));


	%%User events

	{gs, _, motion, _, [X, Y]} ->
	    loop(dbg_ui_aux:flush_motion(X, Y, Trace));

	{gs, _, click, {print, Data}, _} ->
	    loop(show_binding(Data, Trace));

	{gs, _, doubleclick, {print, Data}, _} ->
	    NewTrace = record_editor (Trace, Data),
	    loop(NewTrace);

	%% term edit
	%% receives the edited term from the record editor 

	{term_edit, Cmd} ->
	    dbg_ui_aux:term_edit (Cmd, Trace),
	    loop (Trace);

	{gs, _W, destroy, _, _} -> 
	    dbg_ui_winman:delete_win (Trace#trace.win), 
	    exit(destroy);

	Gs_Cmd when tuple(Gs_Cmd), element(1, Gs_Cmd) == gs ->
	    loop(dbg_ui_aux:gs_cmd(Gs_Cmd, Trace));


	%%Exit Events

	{'EXIT', Meta, {Meta, Reason, Where, Bs, NewStack, _}} ->
	    New_meta = int:attach(Pid, Reason, {Where, Bs, NewStack}),
	    loop(Trace#trace{meta = New_meta, stack=NewStack});

	{'EXIT', Meta, {Meta, Reason, _}} ->
            New_meta = int:attach(Pid, Reason, {}),
	    loop(Trace#trace{meta = New_meta});

	{'EXIT', Meta, Reason} ->
	    New_meta = int:attach(Pid, Reason, {}),
	    loop(Trace#trace{meta = New_meta}); 

	{'EXIT', LinkedPid, Reason} when LinkedPid == Trace#trace.rec_edit ->
	    case Reason of
		normal ->
		    ignore;
		_ ->
		    Text = ["Error in Record Editor",
			    lists:flatten(io_lib:format("~p", [Reason]))],
		    tool_utils:notify(gs:start(), Text)
	    end,
	    loop (Trace#trace{rec_edit = undefined});

	{'EXIT', LinkedPid, Reason} ->
	    io:format("**** Unexpected(?) {'EXIT',~p,~p}~n", [LinkedPid, Reason]),
	    loop(Trace);

        Unexpected  ->
	    %%	    io:format("**** Unhandled message: ~p~n",[Unexpected]),
	    loop(Trace)		
    end.



%% Retuurn the flags regarding the Frame options

get_frame() ->
    case dbg_idb:lookup(frame_defs) of
	not_found  -> ?FRAMEDEFS;
	{ok,Frames}-> Frames
    end.

get_backtrace() ->
    case dbg_idb:lookup(backtrace_def) of
	not_found  -> ?BACKTRACE;
	{ok,Size}-> Size
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Execute user commands not handled directly in execute_cmd/3

%%% back_trace(Trace,Size) -> #trace
%%% Called when back trace option is changed

back_trace(Trace,Size) ->
    dbg_ui_trace_win:update_backtrace_menu(Size),
    Trace#trace{btrace = Size}.

%%% stack_trace(Opt,Gs_mon) -> #gs_mon
%%% Called when the stack options have been changed. The menus are
%%% updated, as is GS_mon

stack_flag(Trace) ->
    Opt = case dbg_idb:lookup(stack_defs) of
	      {ok,Flag} -> Flag;
	      _undefined -> ?STACKDEF
	  end,
    stack_trace(Trace,Opt).

stack_trace(Trace,Opt) ->
    {_,Sp} = Trace#trace.stack,
    dbg_ui_aux:stack_buttons(Opt,Sp,Sp,bottom),
    On = case Opt of
	     all     -> 'Stack Tail';
	     true    -> 'Stack Tail';
	     no_tail -> 'Stack';
	     _       -> 'Stack Off'
	 end,
    dbg_ui_aux:select_menus([On],true),
    Trace#trace{stack_flag=Opt}.




%%% Stack Functions: FIXME: Rewrite, and insert in Record.

%% Create an empty stack

empty_stack() ->
%%    put(stack,{1,1}),  % Use dict. temporary solution ... NOT
    {1,1}.

%% Create a new stack

new_stack(SP) ->
    {SP,SP}.



%%% Put text in the evaluator editor
print_reply(Reply,Trace) ->
    ReplyStr = dbg_pretty:term(Reply),
    dbg_ui_trace_win:print_shell([$<,ReplyStr,10],normal),
    Trace.

pp_backtrace(Trace,Txt) ->
    dbg_ui_trace_win:put_trace([10],'FunctionEditor'),
    pp_backtrace(Txt),
    Trace.
pp_backtrace([{Lev,Mod,{Fun,Arity},Line}|Tl]) ->
    ReplyStr = io_lib:format("~p > ~p:~p/~p",[Lev,Mod,Fun,Arity]),
    dbg_ui_trace_win:put_trace([ReplyStr,10],'FunctionEditor'),
    pp_backtrace(Tl);
pp_backtrace([_|Tl]) ->
    pp_backtrace(Tl);
pp_backtrace([]) -> ok.

%%% Put text in the Trace frame editor
trace(Trace,String) ->
    dbg_ui_trace_win:put_trace(String,'FunctionEditor'),
    Trace.

%%% bindings(Trace,Bs) -> #trace.
%%% Update the binding values in the bindings frame
%%% Bs: A list of {Name,Value} (bindings), where the name is an Atom.
%%% Return the same trace-record as

%bindings(Trace,Bs) when record(Trace,trace), list(Bs) ->
%    dbg_ui_trace_win:update_bindings(Bs),
%    Trace.

%%% show_binding({Name,Val},Trace) -> #trace
%%% Show a binding of a specific variable in the Evaluator editor

show_binding({Name,Val},Trace) ->
    BindStr = io_lib:format("< ~s = ~p~n",[atom_to_list(Name),Val]), 
    dbg_ui_trace_win:print_shell(BindStr,bold),
    Trace.


%%% Check Trace status with Window Status

trace_frame_flag(Trace,Flag) ->
    NFlag = flush_frame(Flag),
    case {Trace#trace.trace,NFlag} of
	{close,false} -> Trace;
	{open,true}   -> Trace;
	_  ->
	    Flags = dbg_ui_aux:get_flags(Trace),
	    NTrace= Trace#trace{trace=dbg_ui_aux:switch(Trace#trace.trace)},
	    NFlags = dbg_ui_aux:get_flags(NTrace),
	    dbg_ui_aux:back_trace_button(NTrace),
	    NewSize = dbg_ui_trace_win:configure(Trace#trace.win,function,
					      dbg_ui_aux:switch(Trace#trace.trace),
					      Flags,NFlags),
	    gs:config('Trace Flag',{select,NFlag}),
	    NTrace#trace{size=NewSize}
    end.

flush_frame(Flag) ->
    receive
	{P,stack_trace_flag,NFlag} ->
	    flush_frame(NFlag)
    after 1 ->
	    Flag
    end.

%%% Attach a new Process , Used whenever the stop button is used.

attached(Trace,Mod,Line,TraceF) ->
    Cm = Trace#trace.cm,
    NewTrace = trace_frame_flag(Trace,TraceF),
    {_Mod,Breaks} = dbg_ui_aux:is_loaded(Mod, Cm, Trace#trace.breaks, Line,
			      Trace#trace.pid, Trace#trace.win),
    NewTrace#trace{cm = Mod, breaks=Breaks, line=Line}.


%%% break_at(Trace,Mod,Line,Sp,Meta) -> #trace
%%% Mod: Module where the execution is at. (Possibly Cm)
%%% Line: New line we are at.
%%% Sp: Stack POinter
%%% Meta: Attached process Pid.
%%% We have changed exec state. We are at a break at line


break_at(Trace,Mod,Line,Sp,Meta) ->
    dbg_ui_aux:request_bindings(Trace#trace.meta, false),
    dbg_ui_aux:enable_menus(?STEP_ACTIONS ++
				 ?MESSAGE_ACTIONS ++
				 ?WHERE_ACTIONS ++
				 [ 'Kill',
				  'Break']),
    dbg_ui_aux:disable_menus(?TIMEOUT_ACTIONS ++
				  ?STOP_ACTIONS),
    dbg_ui_aux:back_trace_button(Trace),
    dbg_ui_aux:stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),

    {_Mod,Breaks} = dbg_ui_aux:is_loaded(Mod, Trace#trace.cm, 
					 Trace#trace.breaks, Line,
					 Trace#trace.pid, Trace#trace.win),

    dbg_ui_aux:show_line(Mod,Line,Trace#trace.line,break_at,Trace#trace.pid),
    dbg_ui_trace_win:update_label(break,Line,Mod),
    Trace#trace{state =  {break_at,Mod,Line}, cm=Mod, breaks=Breaks,
	       stack =  new_stack(Sp), line = Line}.

%%% wait_at(Trace,Mod,Line,Sp) -> #trace
%%% Mod: Module where the execution is at. (Possibly Cm)
%%% Line: New line we are at.
%%% Sp: Stack POinter
%%% Meta: Attached process Pid.
%%% We have changed exec state. Handle all menus and update lines in
%%% The code editor, and bindings. A process has been newly attached,
%%% and is at a receive line. wait_at is thus called.

wait_after_at(Trace,Mod,Line,Sp) ->
    dbg_ui_aux:enable_menus(?TIMEOUT_ACTIONS),
    wait_at(Trace,Mod,Line,Sp).

wait_at(Trace,Mod,Line,Sp) ->
    {_Mod,Breaks} = dbg_ui_aux:is_loaded(Mod, Trace#trace.cm, Trace#trace.breaks, Line,
			      Trace#trace.pid, Trace#trace.win),
    dbg_ui_aux:show_line(Mod,Line,Trace#trace.line,wait_at,Trace#trace.pid),
    dbg_ui_trace_win:update_label(wait_at,Line,Mod),
    dbg_ui_aux:disable_menus(?STEP_ACTIONS ++
				  ?MESSAGE_ACTIONS ),
    dbg_ui_aux:enable_menus(?WHERE_ACTIONS ++
				 ?STOP_ACTIONS ++
				 ['Kill']),
    dbg_ui_aux:back_trace_button(Trace),
    dbg_ui_aux:stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),
    dbg_ui_aux:request_bindings(Trace#trace.meta, false),
    Trace#trace{state = {wait_at,Mod,Line}, cm = Mod, breaks=Breaks, 
		stack= new_stack(Sp),line=Line}.

%%% func_at(Trace,Mod,Line,Sp) -> #trace
%%% Mod: Module where the execution is at. (Possibly Cm)
%%% Line: New line we are at.
%%% Sp: Stack POinter
%%% We have changed exec state. Handle all menus and update lines in
%%% The code editor, and bindings. A process has been newly attached,
%%% and is executing a func. call in another module.

func_at(Trace,Mod,Line,Sp) ->
    {_Mod,Breaks} = dbg_ui_aux:is_loaded(Mod, Trace#trace.cm, Trace#trace.breaks, 
			      Line,Trace#trace.pid, Trace#trace.win),
    dbg_ui_aux:show_line(Mod,Line,Line,running,Trace#trace.pid),
    dbg_ui_aux:stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),
    Trace#trace{breaks=Breaks,cm=Mod,stack= new_stack(Sp),line=Line}.

%%% running(Trace) -> #trace.
%%% We are either in a receive state, executing code in another module,
%%% or have anattached the process we where tracing.

running(Trace) ->
    dbg_ui_aux:disable_menus(?STEP_ACTIONS ++
				  ?STACK_ACTIONS ++
				  ?MESSAGE_ACTIONS ++
				  ?TIMEOUT_ACTIONS ++
				  ?WHERE_ACTIONS ++
				  ['BackTraceMenu']),
    dbg_ui_aux:enable_menus(?STOP_ACTIONS ++
				 ['Kill']),
    dbg_ui_trace_win:update_label(running,dummy,Trace#trace.cm),

    Editor = dbg_ui_cache:current_editor (self()),
    dbg_ui_trace_win:select_line(Editor, clear,0,Trace#trace.line),

    Trace#trace{state =  {running,dummy,dummy}, stack=empty_stack(),
		line = 0}.

idle(Trace) ->
    dbg_ui_aux:disable_menus(?STEP_ACTIONS ++
				  ?STACK_ACTIONS ++
				  ?MESSAGE_ACTIONS ++
				  ?TIMEOUT_ACTIONS ++
				  ?WHERE_ACTIONS ++
				  ?STOP_ACTIONS ++ 
				  ['BackTraceMenu']),
    dbg_ui_aux:enable_menus(['Kill']),
    dbg_ui_trace_win:update_label(idle,dummy,Trace#trace.cm),
    Trace#trace{state =  {running,dummy,dummy}, stack=empty_stack()}.

%%% exit_at(Trace,Mod,Line,Reason,Sp) -> #trace.
%%% Called whenever an attached process exits.

%% Stack Existed.


%% No module, no line. Code is no longer interpreted.
exit_at(Trace, no,no, Reason, Sp) ->
    dbg_ui_aux:disable_menus(?STEP_ACTIONS ++
				  ?MESSAGE_ACTIONS ++
				  ?TIMEOUT_ACTIONS ++
				  ?STOP_ACTIONS ++
				  ?WHERE_ACTIONS ++
				  ['Kill','Break' ]),
    dbg_ui_aux:stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),
    dbg_ui_aux:back_trace_button(Trace),
    exit_file(Trace#trace{stack=new_stack(Sp),state={{exit,Reason},no,no}},
	      no,no,Reason);



%% Module and line. Show where exit happened.
exit_at(Trace,Mod,Line,Reason,Sp) ->
    dbg_ui_aux:disable_menus(?STEP_ACTIONS ++
				  ?MESSAGE_ACTIONS ++
				  ?TIMEOUT_ACTIONS ++
				  ?STOP_ACTIONS ++
				  ['Kill' ]),
    dbg_ui_aux:enable_menus(?WHERE_ACTIONS),
    dbg_ui_aux:stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),
    dbg_ui_aux:back_trace_button(Trace),
    exit_file(Trace#trace{stack=new_stack(Sp),state={{exit,Reason},Mod,Line}},
	      Mod,Line,Reason).

   
%% No Stack, and unknown non interpreted module

exit_file(Trace,no,no,Reason) ->  %%Exited in unknown module
    dbg_ui_trace_win:update_label({no,Reason},dummy,dummy),
    Str = io_lib:format("<**exited:~s~n",[dbg_pretty:term(Reason)]),
    dbg_ui_trace_win:print_shell(Str,normal),
    Trace;

%%No Stack, but exited in an interpreted module.

exit_file(Trace,Mod,Line,Reason) ->
    {_Mod,Breaks} = dbg_ui_aux:is_loaded(Mod, Trace#trace.cm, Trace#trace.breaks, 
			      Line, Trace#trace.pid, Trace#trace.win),
    dbg_ui_trace_win:update_label({exit,Reason},Line,Mod),
    dbg_ui_aux:show_line(Mod, Line, Trace#trace.line, exit, Trace#trace.pid),
    dbg_ui_aux:request_bindings(Trace#trace.meta, false),
    Str = io_lib:format("<**exited:~s~n",[dbg_pretty:term(Reason)]),
    dbg_ui_trace_win:print_shell(Str,normal),
    Trace#trace{cm=Mod, breaks=Breaks,line=Line}.


%%% record_editor  /2
%%%

record_editor (Trace, Data) ->
    NewPid = 
	case Trace#trace.rec_edit of
	    undefined ->
		dbg_ui_recedit:start ([Data], Trace#trace.cm);

	    RecEditPid ->
		dbg_ui_recedit:update (RecEditPid, [Data]),
		RecEditPid
	end,
    
    Trace#trace{rec_edit = NewPid}.
