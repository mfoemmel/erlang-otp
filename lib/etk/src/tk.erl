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
%% Tk interface

-module(tk).

-include("tk.hrl").

-include_lib("kernel/include/file.hrl").

% -define(TRACE, true).
-export([enc_argv/2, dec_argv/1, dec_value/1]).
-export([mkname/2, mkname/3]).

%%
%% Tk interface
%%
-export([start/0, start/1, stop/0]).

%%
%% WIDGETS
-export([button/2, canvas/2, checkbutton/2, entry/2, frame/2,
	 label/2, listbox/2, menu/2, menubutton/2,
	 message/2, radiobutton/2, scale/2, scrollbar/2, text/2,
	 toplevel/2]).

%% COMMANDS
-export([bell/0, bell/1,
	 bindtags/1, bindtags/2,
	 destroy/1, focus/1, 
	 lower/1, lower/2,
	 raise/1, raise/2,
	 bind/1, bind/2, bind/4, unbind/2,
	 bind_tag/2, bind_tag/3, bind_tag/5, unbind_tag/3,
	 bind_ctag/2, bind_ctag/3, bind_ctag/5, unbind_ctag/3,
	 pack/1, pack/2,
	 place/1, place/2,
	 grid/1, grid/2, 
	 tk/1, tk/2,
	 wm/1, wm/2,
	 update/0, update/1,
	 selection/1, rselection/1,
	 clipboard/1, 
	 grab/1,
	 option/1, 
	 image/1, 
	 winfo/1,
	 cmd/2,
	 rcmd/2,
	 setvar/2,
	 getvar/1]).

%% UTILS
-export([register_function/2, unregister_function/1,
	 childrenof/1, parentof/1, toplevelof/1, classof/1,
	 tkfun/1,
	 cget/2, rcget/2, 
	 configure/2,
	 prog/1,
	 tkRepeat/2, tkRepeat/3, tkCancelRepeat/0,
	 tkStrictMotif/1, tkStrictMotif/0,
	 tkrgb/3]).

%% WIDGET LINKING
-export([wlink/1, wlink/2,
	 wunlink/1, wunlink/2,
	 wunlink_pid/1,
	 join/3, leave/1
	]).

%% WIDGET NAME
-export([mkpath/2]).

%% GENERIC EXPORTS
-export([init/2]).

%% INTERNAL EXPORTS
-export([call/1, tk_event_init/2, tk_invoke/2, 
	 tk_destroy_list/2, tk_detach_slaves/2, tk_detach_master/2]).

%% SYSTEM
-export([system_continue/3, system_terminate/4]).

%% IMPORT
-import(lists, [reverse/1, map/2, foreach/2]).

-ifdef(TRACE).
-define(trace(Fmt, As), io:format(Fmt, As)).
-else.
-define(trace(Fmt, As), true).
-endif.

-define(is_digit(X), (X)>=$0, (X)=<$9 ).
-define(is_lower(X), (X)>=$a, (X)=<$z ).
-define(is_upper(X), (X)>=$A, (X)=<$Z ).

-define(uint16(X), ((X) bsr 8) band 16#ff, (X) band 16#ff).

-define(uint24(X), ((X) bsr 16) band 16#ff, 
		    ((X) bsr 8) band 16#ff, (X) band 16#ff).

-define(uint32(X), 
	((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	((X) bsr 8) band 16#ff, (X) band 16#ff).


-define(arg_data(),    ?TK_OP_OFFSET,0,0,0).

-define(arg_string(X), ?TK_OP_STRING,?uint24(X)).

-define(arg_option(X), ?TK_OP_OPTION,?uint24(X)).

-record(state, 
	{
	  port,            %% I/O to tk
	  synclist = [],   %% processes executing start in parallell
	  started = false, %% flags set to true when init
	  debug = []       %% debug options
	}).


%%
%% Start tk
%%
start() ->
    start([]).

start(Opts) ->
    %% Make sure that the application is loaded (need environment)
    application:load(etk), 
    Tag = make_ref(),
    Pid = spawn(?MODULE, init, [[self()|Tag], Opts]),
    receive 
	{Tag, Rep} -> Rep
    end.

stop() ->
    call(stop).

%%
%% Init function for the tk generic server
%%

init(From, Options) ->
    case whereis(tk) of
	undefined ->
	    case catch register(tk, self()) of
		true -> init1(From,Options);
		{'EXIT', _} -> tk ! {sync_start, From}
	    end;
	Pid when pid(Pid) ->
	    tk ! {sync_start, From}
    end.


init1(From,Options) ->
    TkPriv = ets:new(tkPriv, [set,public,named_table]),
    TkFun = ets:new(tkFun, [set,public,named_table]),
    TkLink = ets:new(tkLink, [bag,named_table]),
    CmdOpts = etk_opts(Options),
    case etk_cmd(From,Options) of
	{ok,{port,Cmd}} ->
	    SpawnCmd = Cmd ++ CmdOpts,
	    ?trace("spawn: ~p\n", [SpawnCmd]),
	    TkPort = open_port({spawn, SpawnCmd},[use_stdio,{packet,4}]),
	    TkEvent = spawn_link(tk, tk_event_init, [TkPort,From]),
	    process_flag(trap_exit, true),
	    St = #state { port = TkPort },
	    tk_main(St);
	{ok,{sock,Cmd}} ->
	    %% start a listner
	    {ok,L} = inet_tcp:listen(0, [{packet,4},{nodelay,true}]),
	    {ok,P} = inet:port(L),
	    OsCmd = filename:nativename(Cmd) ++ CmdOpts ++ 
		" -port " ++ integer_to_list(P),
	    ?trace("os: ~p\n", [OsCmd]),
	    os:cmd(OsCmd),
	    {ok,TkSock} = inet_tcp:accept(L),
	    inet_tcp:close(L),
	    TkEvent = spawn_link(tk, tk_event_init, [TkSock,From]),
	    process_flag(trap_exit, true),
	    St = #state { port = TkSock },
	    tk_main(St);
	{error,Reason} ->
	    io:format("TK ERROR: ~p\n", [erl_posix_msg:message(Reason)]),
	    reply(From, {error, Reason}),
	    exit(load_error)	    
    end.

%%
%% Generate the port command
%%
etk_cmd(From,Options) ->
    Dir = filename:join([code:priv_dir(etk), "bin"]),
    find_exec(Dir, Options).


%% setup etk program options  flat list of characters
etk_opts(Options) ->
    case lists:keysearch(withdrawn, 1, Options) of
	{value, {_, true}} -> " -withdrawn";
	_ -> ""
    end 
	++ 
    case lists:keysearch(name, 1, Options) of
	{value, {_, Name}} when list(Name) -> " -name " ++ Name;
	_ -> " -name ErlTk"
    end 
	++ 
    case lists:keysearch(display, 1, Options) of
	{value, {_, Display}} -> " -display " ++ Display;
	_ -> ""
    end.

%%
%% Select executable:
%% 0. if 'safe' option is given then always use a port program!
%%    either a pipe/socket variant. on win32 socket variant must be
%%    used.
%% 1. if 'driver' option is given try use it otherwise etk
%% 2. if command line option -tk_driver [drivername ] try it
%% 3. if etk does not exist but etk[x]_drv does the use etk[x]_drv
%% 4. if etk exists use it
%% 5. error
%%

find_exec(Dir, Options) ->
    case lists:keysearch(safe, 1, Options) of
	{value, {_, true}} ->
	    try_exec(Dir);
	_ ->
	    case lists:keysearch(driver, 1, Options) of
		{value, {_, Drv}} -> try_drv(Dir, [Drv]);
		_ ->
		    case init:get_argument(tk_driver) of
			error ->
			    case try_exec(Dir) of
				{ok,Cmd} -> {ok,Cmd};
				_ -> try_drv(Dir, ["etk_drv", "etkgl_drv"])
			    end;
			{ok, DrvList} ->
			    case lists:append(DrvList) of
				[] -> try_drv(Dir, ["etk_drv"]);
				Drvs -> try_drv(Dir, Drvs)
			    end
		    end
	    end
    end.


try_exec(Dir) ->
    Cmd = filename:join([Dir, "etk"]) ++ exe_extension(),
    case file_exists(Cmd, 8#111) of
	true ->
	    case os:type() of
		{win32,_} -> 
		    {ok,{sock,Cmd}};
		_ -> {ok, {port,Cmd}}
	    end;
	false -> {error, enoent}
    end.
	
		    
try_drv(Dir, [Drv|Ds]) ->
    Cmd = filename:join([Dir, Drv]) ++ dll_extension(),
    case file_exists(Cmd, 8#444) of
	true -> 
	    case erl_ddll:load_driver(Dir, Drv) of
		ok -> {ok,{port,Drv}};
		_  -> try_drv(Dir, Ds)
	    end;
	false -> 
	    try_drv(Dir, Ds)
    end;
try_drv(Dir, []) ->
    {error, elibacc}.

%% check if file exists and some mode bits in Mode are set
file_exists(File, Mode) ->
    case file:read_file_info(File) of
	{ok,Info} when Info#file_info.type == regular,
		       Info#file_info.mode band Mode =/= 0 ->
	    true;
	_ -> 
	    false
    end.

exe_extension() ->
    case os:type() of
	{win32, _} -> ".exe";
	{unix, _} ->  "";
	vxworks ->  ""
    end.    

dll_extension() ->
    case os:type() of
	{win32, _} -> ".dll";
	{unix, _} -> ".so";
	vxworks -> ""
    end.        


tk_terminate(Reason, St) ->
    catch erlang:port_close(St#state.port),
    foreach(
      fun([Pid,Seq]) ->
	      Pid ! {Seq, {error, Reason}}
      end, ets:match(tkPriv, {[wait|'$2'],'$1'})),
    ets:delete(tkPriv),
    ets:delete(tkFun),
    ets:delete(tkLink),
    exit(Reason).

%%
%% Init all globals and bindinsg
%%
tk_init(Port) ->
    %% erlang:display(tk_init),
    ?tkset(afterId, make_ref()), %% init to dummy id !!!
    ?tkset(seq, 0),              %% internal sequence number
    ?tkset(func, 0),             %% internal function id counter
    ?tkset(port, Port),          %% internal tk port
    ?tkset(buttons, 0),
    ?tkset(buttonWindow, ""),
    ?tkset(dragging, ""),
    ?tkset(focus, ""),
    ?tkset(grab, ""),
    ?tkset(initPos, ""),
    ?tkset(inMenubutton, ""),
    ?tkset(listboxSelection, ""),
    ?tkset(cursor, ""),
    ?tkset(relief, ""),
    ?tkset(listboxPrev, ""),
    ?tkset(mouseMoved, 0),
    ?tkset(prevPos, ""),
    ?tkset(char, 0),
    ?tkset(oldGrab, ""),
    ?tkset(grabStatus, ""),
    ?tkset(popup, ""),
    ?tkset(postedMb, ""),
    ?tkset(pressX, 0),
    ?tkset(pressY, 0),
    ?tkset(screen, ""),
    ?tkset(selectMode, char),
    ?tkset(window, ""),
    ?tkset(activeBg, ""),
    ?tkset(initValue,""),
    ?tkset(deltaX, 0),
    ?tkset(deltaY, 0),

    %% initialize all counters
    ?tkset([counter|t], 0),    %% toplevel
    ?tkset([counter|b], 0),    %% button
    ?tkset([counter|c], 0),    %% canvas
    ?tkset([counter|cb], 0),   %% check button
    ?tkset([counter|e], 0),    %% entry
    ?tkset([counter|f], 0),    %% frame
    ?tkset([counter|l], 0),    %% label
    ?tkset([counter|li], 0),   %% listbox
    ?tkset([counter|m], 0),    %% menu
    ?tkset([counter|mb], 0),   %% menu button
    ?tkset([counter|me], 0),   %% message
    ?tkset([counter|r], 0),    %% radio button
    ?tkset([counter|s], 0),    %% scale
    ?tkset([counter|sb], 0),   %% scroll bar
    ?tkset([counter|tx], 0),   %% text

    %% Load string/option table
    %% inserted into tkPriv as { [str|Foo], Offset} or
    %%                         { [opt|Bar], Offset }
    foreach(fun({Op,Name,Offset}) ->
		    ets:insert(tkPriv, {[Op|Name], Offset});
	       (eof) ->
		    true
	    end, tkstr:strings()),
    %% set non-strict motif
    tkStrictMotif(false),
    %% erlang:display(tk_init2),

    %% unix
    case tk:getvar("tcl_platform(platform)") of
	"unix" ->
	    event([add, "<<Cut>>", "<Control-Key-w>", "<Key-F20>"]),
	    event([add, "<<Copy>>", "<Meta-Key-w>", "<Key-F16>"]),
	    event([add, "<<Paste>>", "<Control-Key-y>", "<Key-F18>"]);
	"windows" ->
	    event([add, "<<Cut>>","<Control-Key-x>","<Shift-Key-Delete>"]),
	    event([add, "<<Copy>>","<Control-Key-c>","<Control-Key-Insert>"]),
	    event([add, "<<Paste>>","<Control-Key-v>","Shift-Key-Insert>"]);
	"macintosh" ->
	    event([add, "<<Cut>>", "<Control-Key-x>", "<Key-F2>"]),
	    event([add, "<<Copy>>", "<Meta-Key-c>", "<Key-F3>"]),
	    event([add, "<<Paste>>", "<Control-Key-v>", "<Key-F4>"]),
	    event([add, "<<Clear>>", "<Clear>"]);
	Other ->
	    io:format("tk: WARNING unknown platform ~p\n", [Other])
    end,
    %% erlang:display(tk_init3),

    bind("all", "<Tab>", ['%W'],
	 fun(W) -> focus([tkfocus:tk_focusNext(W)]) end),
    bind("all", "<Shift-Tab>", ['%W'],
	 fun(W) -> focus([tkfocus:tk_focusPrev(W)]) end),
    bind("all", "<Destroy>", ['%W'],
	 fun(W) -> tk ! {destroy, W} end),
    %% erlang:display(tk_init4),
    tkbutton:init(),
    tkentry:init(),
    tklistbox:init(),
    tkmenu:init(),
    tkscale:init(),
    tkscrlbar:init(),
    tktext:init(),
    %% erlang:display(tk_init5),
    %% Initialize screen
    ?tkset(screen, winfo(["screen", "."])).

    
%%
%% Event executer (must be serialized)
%%
%%  1) init global
%%  2) init all bindings (MAY NOT GENERATE EVENTS!!!)
%%  3) sync start user
%%  3) start main event loop
%%
tk_event_init(Port, From) ->
    register(tk_event, self()),
    tk_init(Port),
    tk ! sync_done,             %% sync tk 
    reply(From, {ok, started}), %% sync first user
    tk_event_loop().

tk_event_loop() ->
    receive
	{event,As} ->
	    tk_event_eval(As),
	    tk_event_loop();

	{timeout,_,{timer_event,Fun,As}} ->
	    case catch apply(Fun, As) of
		{'EXIT', Reason} ->
		    io:format("tktimer error: ~w : ~w\n", 
			      [Fun, Reason]),
		    tk_event_loop();
		_ ->
		    tk_event_loop()
	    end;
	Other ->
	    io:format("tk_event_loop: got ~w\n", [Other]),
	    tk_event_loop()
    end.
%%
%% tk_event_eval:
%%
%% Evaluate each event function.
%% the arguments and next event is found by scan_event
%% 
%%
tk_event_eval([Id | Args]) ->
    {As, Next} = scan_event(Args),
    case tkfun(Id) of
	undefined -> 
	    ?trace("EVENT: ~p undefined\n", [Id]),
	    tk_event_eval(Next);
	Fun ->
	    ?trace("EVENT : ~w, args = ~p\n", [Id, As]),
	    case catch apply(Fun, As) of
		{'EXIT', Reason} ->
		    io:format("tkfun error: ~p ~p: ~p\n", [Id, As, Reason]),
		    tk_event_eval(Next);
		break ->
		    true;
		continue ->
		    tk_event_eval(Next);
		_ ->
		    tk_event_eval(Next)
	    end
    end;
tk_event_eval([]) ->
    true.

%%
%%
%%
mk_rgb([R0,G0,B0]) ->
    #tkRgb{red=hex(R0),green=hex(G0),blue=hex(B0)};
mk_rgb([R1,R0,G1,G0,B1,B0]) ->
    #tkRgb{red=hex(R1,R0),green=hex(G1,G0),blue=hex(B1,B0)};
mk_rgb([R2,R1,R0,G2,G1,G0,B2,B1,B0]) ->
    #tkRgb{red=hex(R2,R1,R0),green=hex(G2,G1,G0),blue=hex(B2,B1,B0)};
mk_rgb([R3,R2,R1,R0,G3,G2,G1,G0,B3,B2,B1,B0]) ->
    #tkRgb{red=hex(R3,R2,R1,R0),green=hex(G3,G2,G1,G0),blue=hex(B3,B2,B1,B0)}.

tkrgb(R, G, B) ->
    #tkRgb{red = R, green=G, blue=B}.

hex(X) when X >= $0, X =< $9 -> (X - $0);
hex(X) when X >= $a, X =< $f -> ((X - $a) + 10);
hex(X) when X >= $A, X =< $F -> ((X - $A) + 10).

hex(X1,X0) ->
    hex(X1)*16 + hex(X0).

hex(X2,X1,X0) ->
    hex(X2,X1)*16 + hex(X0).

hex(X3,X2,X1,X0) ->
    hex(X3,X2)*256 + hex(X1,X0).

format_rgb(RGB) -> 
    format_rgb(RGB#tkRgb.red, RGB#tkRgb.green, RGB#tkRgb.blue).
    
format_rgb(R, G, B) when R < 16, G < 16, B < 16 ->
    "#" ++ format_hex(R,1) ++ format_hex(G,1) ++ format_hex(B,1);
format_rgb(R, G, B) when R < 256, G < 256, B < 256 ->
    "#" ++ format_hex(R,2) ++ format_hex(G,2) ++ format_hex(B,2);
format_rgb(R, G, B) when R < 4096, G < 4096, B < 4096 ->
    "#" ++ format_hex(R,3) ++ format_hex(G,3) ++ format_hex(B,3);
format_rgb(R, G, B) ->
    "#" ++ format_hex(R,4) ++ format_hex(G,4) ++ format_hex(B,4).

format_hex(X, N) ->
    format_hex(X, N, []).

format_hex(X, 0, Hex) -> 
    Hex;
format_hex(X, I, Hex) ->
    N = X band 16#f,
    if
	N < 10 -> format_hex(X bsr 4, I-1, [N+$0 | Hex]);
	true -> format_hex(X bsr 4, I-1, [(N-10)+$a | Hex])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Short cuts
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parentof(W) -> winfo([parent, W]).

childrenof(W) -> winfo([children, W]).

toplevelof(W) -> winfo([toplevel, W]).

classof(W) -> winfo([class, W]).

cget(W, Opt) when atom(Opt) -> cmd(W, [cget, {Opt}]).

rcget(W, Opt) when atom(Opt) -> rcmd(W, [cget, {Opt}]).

configure(W, Opts) -> cmd(W, [configure | Opts]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Widget linking
%%
%% When a widget is linked to a process or vice versa the
%% Process will receive an {'EXIT', {tkwidget,W}}
%% when the widget is destroyed.
%%
%% If a process linked to some widgets dies the all widgets
%% are destroyed.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Link to widget
wlink(W) ->
    wlink(self(), W).

%% Unlink from widget
wunlink(W) ->
    wunlink(self(), W).

%% Link a process to a widget
wlink(Pid, W) ->
    Request = {link, Pid, W},
    case get(tk_cmds) of 
	undefined ->
	    case tk:winfo([exists, W]) of
		0 -> exit({tkerror, "widget not found"});
		1 -> call(Request)
	    end;
	L ->
	    Id = register_function(operation, fun() -> Request end),
	    put(tk_cmds,[{?ERL_TK_WLINK, enc_argv("", [wlink, W, Id])}|L]),
	    true
    end.

%% Unlink a process from a widget
wunlink(Pid, W) ->
    Request = {unlink, Pid, W},
    case get(tk_cmds) of
	undefined -> call(Request);
	L  ->
	    Id = register_function(operation, fun() -> Request end),
	    put(tk_cmds,[{?ERL_TK_OPERATION, enc_argv("", [operation,Id])}|L]),
	    true
    end.
			
%% Unlink all widget from a process
wunlink_pid(Pid) ->
    Request = {unlink_pid, Pid},
    case get(tk_cmds) of
	undefined -> call(Request);
	L  -> 
	    Id = register_function(operation, fun() -> Request end),
	    put(tk_cmds,[{?ERL_TK_OPERATION, enc_argv("", [operation,Id])}|L]),
	    true
    end.

%% join a widget group (works only instance created in the same way)
join(W, Node, MasterW) when atom(Node) ->
    call({set_master, W, {MasterW,Node}}),
    rpc:call(Node, tk, call, [{add_slave, MasterW, {W,node()}}]).

%% leave widget group
leave(W) ->
    case ets:lookup(tkLink, [master|W]) of
	[] -> true;
	[{_,{MasterW,Node}}] ->
	    call({unset_master, W}),
	    rpc:call(Node, tk, call, [{del_slave, MasterW, {W,node()}}])
    end.

%% Generate a widget name
mkpath(Parent, Key) when atom(Key) ->
    Id = ets:update_counter(tkPriv, [counter|Key], 1),
    if
	Parent == "." ->
	    ".#" ++ atom_to_list(Key) ++ integer_to_list(Id);
	true ->
	    Parent ++ ".#" ++ atom_to_list(Key) ++ integer_to_list(Id)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Exectute a bunch of commands
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prog(Fun) ->
    case get(tk_cmds) of
	undefined ->
	    put(tk_cmds, []),
	    case catch Fun() of
		{'EXIT',Reason} ->
		    erase(tk_cmds),
		    exit(Reason);
		Value ->
		    L = get(tk_cmds),
		    erase(tk_cmds),
		    dn_request(L),
		    Value
	    end;
	_ ->
	    Fun()
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  TK COMMANDS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Return a list of bound Events
bind(W) ->
    d_request(?ERL_TK_BIND, enc_argv(W, [bind, W])).

%% Return a function or nil
bind(W, Event) ->
    d_request(?ERL_TK_BIND, enc_argv(W, [bind, W, Event])).

%% set a function to evaluate on event

bind(W, Event, Template, Fun) when function(Fun) ->
    unbind(W, Event),
    d_request(?ERL_TK_BIND, enc_argv(W, [bind, W, Event,
				   tkeventfun(W,Event,Template,Fun)])).

unbind(W, Event) ->
    case bind(W, Event) of
	[] -> [];
	["event", _, Id | As] ->
	    d_request(?ERL_TK_BIND, enc_argv(W, [bind, W, Event, ""])),
	    unregister_function(Id)
    end.

%%
%% Text widget tags
%%
bind_tag(W, Tag, Event, Template, Fun) when function(Fun) ->
    d_request(?ERL_TK_CMD,
	    enc_argv(W, [W, tag, bind, Tag, Event, 
		     tkeventfun(W,Tag,Event,Template, Fun)])).

bind_tag(W, Tag, Event) ->
    d_request(?ERL_TK_CMD, enc_argv(W, [W,tag,bind,Tag,Event])).

bind_tag(W, Tag) ->
    d_request(?ERL_TK_CMD, enc_argv(W, [W,tag,bind,Tag])).


unbind_tag(W, Tag, Event) ->
    case bind_tag(W, Tag, Event) of
	[] -> [];
	["event", _, Id | As] ->
	    d_request(?ERL_TK_CMD,
		    enc_argv(W, [W, tag, bind, Tag, Event, ""])),
	    unregister_function(Id)
    end.

%%
%% Canvas widget tags
%%    

bind_ctag(W, Tag, Event, Template, Fun) when function(Fun) ->
    d_request(?ERL_TK_CMD,
	    enc_argv(W, [W, bind, Tag, Event, 
			 tkeventfun(W,Tag,Event,Template,Fun)])).

bind_ctag(W, Tag, Event) ->
    d_request(?ERL_TK_CMD, enc_argv(W, [W, bind, Tag, Event])).

bind_ctag(W, Tag) ->
    d_request(?ERL_TK_CMD, enc_argv(W, [W, bind, Tag])).

unbind_ctag(W, Tag, Event) ->
    case bind_ctag(W, Tag, Event) of
	[] -> [];
	["event", _, Id | As] ->
	    d_request(?ERL_TK_CMD,
		    enc_argv(W, [W, bind, Tag, Event, ""])),
	    unregister_function(Id)
    end.

%%
%% WIDGETS
%%
button(W, Opts) ->
    d_request(?ERL_TK_BUTTON, enc_argv(W, [button, W | Opts])).

checkbutton(W, Opts) ->
    d_request(?ERL_TK_CHECKBUTTON, enc_argv(W, [checkbutton, W | Opts])).

radiobutton(W, Opts) ->
    d_request(?ERL_TK_RADIOBUTTON, enc_argv(W, [radiobutton, W | Opts])).

label(W, Opts) ->
    d_request(?ERL_TK_LABEL, enc_argv(W, [label, W | Opts])).

listbox(W, Opts) ->
    d_request(?ERL_TK_LISTBOX, enc_argv(W, [listbox, W | Opts])).

menu(W, Opts) ->
    d_request(?ERL_TK_MENU, enc_argv(W, [menu, W | Opts])).

menubutton(W, Opts) ->
    d_request(?ERL_TK_MENUBUTTON, enc_argv(W, [menubutton, W | Opts])).

message(W, Opts) ->
    d_request(?ERL_TK_MESSAGE, enc_argv(W, [message, W | Opts])).

scale(W, Opts) ->
    d_request(?ERL_TK_SCALE, enc_argv(W, [scale, W | Opts])).

scrollbar(W, Opts) ->
    d_request(?ERL_TK_SCROLLBAR, enc_argv(W, [scrollbar, W | Opts])).

canvas(W, Opts) ->
    d_request(?ERL_TK_CANVAS, enc_argv(W, [canvas, W | Opts])).

entry(W, Opts) ->
    d_request(?ERL_TK_ENTRY, enc_argv(W, [entry, W | Opts])).

frame(W, Opts) ->
    d_request(?ERL_TK_FRAME, enc_argv(W, [frame, W | Opts])).

toplevel(W, Opts) ->
    d_request(?ERL_TK_TOPLEVEL, enc_argv(W, [toplevel, W | Opts])).

text(W, Opts) ->
    d_request(?ERL_TK_TEXT, enc_argv(W, [text, W | Opts])).

%%
%% COMMAND
%%
destroy([W | Ws]) when list(W) ->
    foreach(fun(WW) -> wunlink(WW) end, [W | Ws]),
    d_request(?ERL_TK_DESTROY, enc_argv("", [destroy, W | Ws]));
destroy(W) ->
    wunlink(W),
    d_request(?ERL_TK_DESTROY, enc_argv("", [destroy, W])).

lower(Win) ->
    d_request(?ERL_TK_LOWER, enc_argv("", [lower, Win])).

lower(Win, Other) ->
    d_request(?ERL_TK_LOWER, enc_argv("", [lower, Win, Other])).

raise(Win) ->
    d_request(?ERL_TK_RAISE, enc_argv("", [raise, Win])).

raise(Win, Other) ->
    d_request(?ERL_TK_RAISE, enc_argv("", [raise, Win, Other])).

bell() -> 
    d_request(?ERL_TK_BELL, enc_argv("", [bell])).

bell(Win) ->
    d_request(?ERL_TK_BELL, enc_argv("", [bell, {displayof,Win}])).

wm(Opts) ->
    d_request(?ERL_TK_WM, enc_argv("", [wm | Opts])).

wm(W, Opts) -> 
    d_request(?ERL_TK_WM, enc_argv(W, [wm, W | Opts])).

focus(Opts) ->
    d_request(?ERL_TK_FOCUS, enc_argv("", [focus | Opts])).

grab(Opts) ->
    d_request(?ERL_TK_GRAB, enc_argv("", [grab | Opts])).

image(Opts) ->
    d_request(?ERL_TK_IMAGE, enc_argv("", [image | Opts])).

option(Opts) ->
    d_request(?ERL_TK_OPTION, enc_argv("", [option | Opts])).

clipboard(Opts) ->
    d_request(?ERL_TK_CLIPBOARD, enc_argv("", [clipboard | Opts])).

selection(Opts) ->
    d_request(?ERL_TK_SELECTION, enc_argv("", [selection | Opts])).

rselection(Opts) ->
    d_raw_request(?ERL_TK_SELECTION, enc_argv("", [selection | Opts])).

grid(Opts) ->
    d_request(?ERL_TK_GRID, enc_argv("", [grid | Opts])).
    
grid(W, Opts) -> 
    grid([W | Opts]).

pack(Ws) ->
    d_request(?ERL_TK_PACK, enc_argv("", [pack | Ws])).

pack(W, Opts) ->
    pack([W | Opts]).

place(Ws) ->
    d_request(?ERL_TK_PLACE, enc_argv("", [place | Ws])).

place(W, Opts) ->
    place([W | Opts]).

%%
%% Events
%%
update() ->
    d_request(?ERL_TK_UPDATE, enc_argv("", [update])).

update(Flag) ->
    d_request(?ERL_TK_UPDATE, enc_argv("", [update, Flag])).

bindtags(W) ->
    d_request(?ERL_TK_BINDTAGS, enc_argv("", [bindtags, W])).

bindtags(W, Tags) ->
    d_request(?ERL_TK_BINDTAGS, enc_argv("", [bindtags, W | Tags])).
    
winfo(Opts) ->
    d_request(?ERL_TK_WINFO, enc_argv("", [winfo | Opts])).

event(Opts) ->
    d_request(?ERL_TK_EVENT, enc_argv("", [event | Opts])).

%%
%% Other commands:
%% appname (set application name)
%%
tk(Opt) -> 
    d_request(?ERL_TK_TK, enc_argv("", [tk, Opt])).

tk(Opt, Arg) ->
    d_request(?ERL_TK_TK, enc_argv("", [tk, Opt, Arg])).

%%
%% The widget command
%%
cmd(W, Opts) ->
    slave_cmd(W, cmd, Opts), %% check for slave updates
    d_request(?ERL_TK_CMD, enc_argv(W, [W | Opts])).

%%
%% The raw widget command 
%% The return value is not interpreted, just text
%%
rcmd(W, Opts) ->
    slave_cmd(W, rcmd, Opts), %% check for slave updates
    d_raw_request(?ERL_TK_CMD, enc_argv(W, [W | Opts])).

%%
%% Variables
%%
setvar(Var, Value) ->
    d_request(?ERL_TK_SETVAR, enc_argv("", [setvar, Var, Value])).

getvar(Var) ->
    d_request(?ERL_TK_GETVAR, enc_argv("", [getvar, Var])).

%%
%% Check if widget W has slaves and propagate
%%
slave_cmd(W, Command, Opts) ->
    case ets:lookup(tkLink, [slave|W]) of
	[] -> false;
	Slaves ->
	    lists:foreach(
	      fun ({_,{SW,Node}}) ->
		      rpc:call(Node,tk,Command,[SW,Opts])
	      end, Slaves)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  GENERAL TK REQUEST
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% direct request, avoid sending list data to tk server and
%% possibly avoid one context switch.
%% by-passing tk for the call:
%% 1. create a uniq sequence number
%% 2. register pid with the sequence number
%% 3. do port command
%% 4. wait for reply containing seqence number
%% 5. remove pid registration
%% 
d_request(Cmd, CmdData) ->
    case get(tk_cmds) of
	undefined -> 
	    d1_request(Cmd, CmdData);
	L -> 
	    put(tk_cmds, [{Cmd,CmdData}|L]),
	    ok
    end.

dn_request(CmdList) ->
    Seq = ets:update_counter(tkPriv, seq, 1) band 16#ffff,
    CmdBytes = dl_make_cmd(CmdList, Seq,[]),
    ?trace("dn(~w): - ~w\n", [Seq,CmdBytes]),
    do_request(CmdBytes, Seq).

d1_request(Cmd, CmdData) ->
    Seq = ets:update_counter(tkPriv, seq, 1) band 16#ffff,
    ?trace("d1(~w): ~w - ~w\n", [Seq,Cmd,CmdData]),
    do_request([?uint16(Seq),?uint16(Cmd),CmdData], Seq).

%% sequence mark (and reverse) all commands
dl_make_cmd([{Cmd,CmdData}|L], Seq, Buf) ->
    dl_make_cmd(L, Seq, [?uint16(Seq),?uint16(Cmd), CmdData | Buf]);
dl_make_cmd([], _, Buf) -> Buf.


do_request(CmdBytes, Seq) ->
    ets:insert(tkPriv, {[wait|Seq], self()}),
    case d_send(?tkget(port), CmdBytes) of
	true ->
	    receive
		{Seq,{ok,Bytes}} -> 
		    case dec_value(Bytes) of
			{ok, Value} -> Value;
			{error,Cause} -> exit({tkerror, Cause})
		    end;
		{Seq,{error, Cause}} ->
		    exit({tkerror, Cause})
	    end;
	false ->
	    %% remove entry only if we crash!!
	    ets:delete(tkPriv, [wait|Seq]),
	    exit({tkerror, "bad argument"})
    end.

%% direct request
%% by passing tk for the call
d_raw_request(Cmd, Argv) ->
    Seq = ets:update_counter(tkPriv, seq, 1) band 16#ffff,
    ets:insert(tkPriv, {[wait|Seq], self()}),
    case d_send(?tkget(port), [?uint16(Seq),?uint16(Cmd),Argv]) of
	true ->
	    receive
		{Seq,{ok,Value}} -> Value;
		{Seq,{error, Cause}} -> exit({tkerror, Cause})
	    end;
	false ->
	    %% remove entry only if we crash!!
	    ets:delete(tkPriv, [wait|Seq]),
	    exit({tkerror, "bad argument"})
    end.

%% output to port or socket!
d_send(Port, Bytes) when port(Port) ->
    case catch erlang:port_command(Port, Bytes) of
	true -> true;
	{'EXIT',_} -> false
    end;
d_send(Sock, Bytes) ->
    case inet_tcp:send(Sock, Bytes) of
	ok -> true;
	_ -> false
    end.


call(Call) ->
    Tag = make_ref(),
    tk ! {call, Call, [self()|Tag]},
    receive
	{Tag, Reply} -> Reply
    end.

reply([Pid|Tag], Reply) ->
    Pid ! {Tag, Reply};
reply(none, Reply) ->  %% no reply from internal operation
    Reply.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  GENERIC TK HANDLE
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_main(St) ->
    receive
	{Port, {data, [X1,X0,C1,C0 | Bytes]}} when Port == St#state.port ->
	    handle_input((C1 bsl 8) + C0, (X1 bsl 8) + X0, Bytes, St);

	{tcp,Sock,[X1,X0,C1,C0|Bytes]} when Sock == St#state.port ->
	    handle_input((C1 bsl 8) + C0, (X1 bsl 8) + X0, Bytes, St);

	{call, Request, From} ->
	    handle_call(Request, From, St);

	{destroy, W} ->
	    tk_widget_died(W),
	    tk_main(St);

	{'EXIT', Port, Reason} when Port == St#state.port ->
	    tk_terminate(port_dead, St);
	
	{tcp_closed, Sock} when Sock == St#state.port ->
	    tk_terminate(port_dead, St);

	{'EXIT', Pid, Reason} ->
	    ?trace("Process ~w died\n", [Pid]),
	    ets:match_delete(tkPriv, {[wait|'_'],Pid}),
	    tk_process_died(Pid),
	    tk_main(St);

	{sync_start, From} -> %% send from clients executing tk:start
	    if St#state.started == false ->
		    tk_main(St#state { synclist = [From|St#state.synclist]});
	       true ->
		    reply(From, {error, already_started}),
		    tk_main(St)
	    end;

	sync_done -> %% sent from tk_event_init!!
	    foreach(
	      fun(From) ->
		      reply(From, {error, already_started})
	      end, St#state.synclist),
	    tk_main(St#state { started = true, synclist = []});

	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, self(), ?MODULE,
				  St#state.debug, [St]);

	Garbage ->
	    io:format("tk: got garbage ~p\n", [Garbage]),
	    tk_main(St)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle System Eventy
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Continue and set debug flags
system_continue(Owner, NDebug, [St]) ->
    tk_main(St#state { debug = NDebug }).

%% Terminate
system_terminate(Reason, Owner, Debug, [St]) ->
    tk_terminate(Reason, St).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Calls
%%   handle_call(Request, From, State, Waiting)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({link,Pid,W}, From, St) ->
    V = tk_link(Pid, W),
    reply(From, V),
    tk_main(St);    
handle_call({unlink,Pid,W}, From, St) ->
    V = tk_unlink(Pid, W),
    reply(From, V),
    tk_main(St);    
handle_call({unlink_pid, Pid}, From, St) ->
    V = tk_unlink(Pid),
    reply(From, V),
    tk_main(St);
handle_call({add_slave, W, Slave}, From, St) ->
    V = tk_add_slave(W, Slave),
    reply(From, V),
    tk_main(St);
handle_call({del_slave, W, Slave}, From, St) ->
    V = tk_del_slave(W, Slave),
    reply(From, V),
    tk_main(St);
handle_call({set_master, W, Master},From,St) ->
    V = tk_set_master(W, Master),
    reply(From, V),
    tk_main(St);
handle_call({unset_master, W},From,St) ->
    V = tk_unset_master(W),
    reply(From, V),
    tk_main(St);    
handle_call(stop, From, St) ->
    reply(From, ok),
    tk_terminate(stopped, St);
handle_call(Req, From, St) ->
    io:format("tk: got unknown call ~p from ~p~n", [Req, From]),
    tk_main(St).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Input
%%   handle_input(Code, Seq, Data, State, Waiting)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_input(?TK_ERL_OK, Seq, Data, St) ->
    ?trace("Reply(~w) ~w: ~p\n", [Seq, ok, Data]),
    handle_reply(Seq, {ok, Data}, St);
handle_input(?TK_ERL_ERROR, Seq, Data, St) ->
    ?trace("Reply(~w) ~w: ~p\n", [Seq, error, Data]),
    handle_reply(Seq, {error, Data}, St);
handle_input(?TK_ERL_EVENT, Seq, Data, St) ->
    case dec_argv(Data) of
	{ok, [W|As]} ->
	    case ets:lookup(tkLink, [master|W]) of
		[] ->
		    tk_event ! {event, As};  %% local
		[{_,{MW,Node}}] ->
		    As1 = subst(MW, W, As),
		    {tk_event,Node} ! {event,As1};
		Other ->
		    io:format("bad master in tk_event_loop ~p\n", [Other])
	    end;
	_ -> false
    end,
    tk_main(St);
handle_input(?TK_ERL_INVOKE, Seq, Data, St) ->
    case dec_argv(Data) of
	{ok, [Id | As]} when integer(Id) ->
	    spawn(?MODULE,tk_invoke,[Id, As]);
	{ok, Other} ->
	    io:format("tk: Invoke Error: ~p~n", [Other]);
	_ -> false
    end,
    tk_main(St);
handle_input(?TK_ERL_OPERATION, Seq, Data, St) ->
    case dec_argv(Data) of
	{ok, [Id | As]} when integer(Id) ->
	    case tkfun(Id) of
		undefined ->
		    tk_main(St);
		Fun ->
		    unregister_function(Id),
		    case catch apply(Fun, As) of
			{'EXIT', Reason} -> tk_main(St);
			Request -> handle_call(Request, none, St)
		    end
	    end;
	{ok, Other} ->
	    io:format("tk: Operation Error: ~p~n", [Other]),
	    tk_main(St);
	_ ->
	    tk_main(St)
    end;
handle_input(?TK_ERL_TKERROR, Seq, Data, St) ->
    case dec_argv(Data) of
	{ok, As} ->
	    io:format("tk: bgerror: ~s~n", [As]);
	_ -> false
    end,
    tk_main(St);
handle_input(?TK_ERL_TKSCREEN, Seq, Data, St) ->
    case dec_argv(Data) of
	{ok, [Screen | As]} -> ?tkset(screen, Screen);
	_ -> false
    end,
    tk_main(St);    
handle_input(Code, Seq, Data, St) ->
    io:format("tk: got unknown port code ~w : ~p~n", [Code, Data]),
    tk_main(St).

%%
%% Find caller send reply and goto main
%%
handle_reply(Seq, Reply, St) ->
    case ets:lookup(tkPriv, [wait|Seq]) of
	[] -> 
	    ?trace(" to NOT FOUND (~w)\n", [Seq]),
	    tk_main(St);
	[{_,Pid}] ->
	    ets:delete(tkPriv, [wait|Seq]),
	    Pid ! {Seq, Reply},
	    tk_main(St)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Handle Commands
%%   tk_invoke is run in a separate process as a result of
%%   the invoke command
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tk_invoke(Id, As) ->
    case tkfun(Id) of
	undefined ->
	    false;
	Fun ->
	    ?trace("INVOKE: ~w, args = ~p\n", [Id, As]),
	    case catch apply(Fun, As) of
		{'EXIT', Reason} ->
		    io:format("tkfun error: ~w : ~p\n", [Id, Reason]),
		    false;
		_ ->
		    true
	    end
    end.


%%
%% Link table is a bag of:
%% Key is   [link|Pid]      -> Widget    -- multiple
%%     and  [link|Widget]   -> Pid       -- multiple
%%          [slave|Widget]  -> {W,Node}} -- multiple
%%          [master|Widget] -> {W,Node}  -- one
%%          
%%
%%
tk_link(Pid, W) ->
    ets:insert(tkLink, {[link|Pid],W}),
    ets:insert(tkLink, {[link|W],Pid}),
    link(Pid).

tk_unlink(Pid, W) ->
    ets:match_delete(tkLink, {[link|Pid],W}),
    ets:match_delete(tkLink, {[link|W],Pid}),
    case ets:lookup(tkLink, [link|Pid]) of
	[] -> unlink(Pid);
	_ -> true
    end.

tk_unlink(Pid) ->
    ets:delete(tkLink, Pid),
    ets:match_delete(tkLink, {[link|'_'],Pid}),
    unlink(Pid).

tk_add_slave(W, Slave) ->
    ets:insert(tkLink, {[slave|W], Slave}).

tk_del_slave(W, Slave) ->
    ets:match_delete(tkLink, {[slave|W],Slave}).

%% attach master widget
tk_set_master(W, Master) ->
    case ets:lookup(tkLink, [master|W]) of
	[] ->
	    ets:insert(tkLink, {[master|W],Master});
	_ ->
	    {error, "master already set"}
    end.

%% detach master widget
tk_unset_master(W) ->
    ets:delete(tkLink, [master|W]).

%%
%% Utility to iterate and destroy widgets (spawned by tk_process_died)
%%
%% NOTE: Depending on which order widgets are destroyed the
%%       tk_widget_died DESTROY command may fail.
%%       When deleting top levels first, then children are
%%       destroyed by Tk.
%%
%%       My guess is that it's better to destroy the toplevel
%%       stuff first then the children, otherwise complex 
%%       gui's will look funny when they are destroyed.
%%
tk_destroy_list(Pid, [W|Ws]) ->
    Res = (catch d1_request(?ERL_TK_DESTROY, enc_argv("", [destroy, W]))),
    case Res of 
	{'EXIT', Reason} ->
	    io:format("Process ~w killed widget ~p : got ~p\n", 
		      [Pid, W, Reason]);
	_ ->
	    io:format("Process ~w killed widget ~p\n", [Pid, W])
    end,
    tk_destroy_list(Pid, Ws);
tk_destroy_list(Pid, []) ->
    true.


tk_process_died(Pid) ->
    case ets:lookup(tkLink, [link|Pid]) of
	[] -> true;
	PWs ->
	    tk_unlink(Pid),
	    %% Extract widgets and sort them
	    Ws = lists:sort(map(fun({_,W}) -> W end, PWs)),
	    %% run recursivly in a new process 
	    spawn(?MODULE, tk_destroy_list, [Pid, Ws]),
	    true
    end.

%%
%% Util to detach master from all slaves
%%
tk_detach_slaves(W, Slaves) ->
    foreach(
      fun({_,{SW,Node}}) ->
	      rpc:call(Node,tk,call,[{unset_master, SW}])
      end, Slaves).

tk_detach_master(W, {Master,Node}) ->
    rpc:call(Node, ?MODULE, call, [{del_slave, Master, [W,node()]}]).

%%
%% When a widget dies we have to:
%% 1. remove all function bindings
%% 2. if widget is slave then remove slave from master
%% 3. if widget is master then detach all slaves from master
%% 4. propagate exits to a attached processes
%%
tk_widget_died(W) ->
    %% remove functions
    ets:match_delete(tkFun, {'_',W,'_'}),
    %% detach from master 
    case ets:lookup(tkLink, [master|W]) of
	[] -> true;  %% no master
	[{_,Master}] ->
	    spawn(?MODULE, tk_detach_master, [W,Master]),
	    ets:delete(tkLink, [master|W])
    end,
    %% detach slaves
    case ets:lookup(tkLink, [slave|W]) of
	[] -> true;
	Slaves ->
	    spawn(?MODULE, tk_detach_slaves, [W,Slaves]),
	    %% remove slave(s)
	    ets:delete(tkLink, [slave|W])
    end,
    %% propagte links
    case ets:lookup(tkLink, [link|W]) of
	[] -> true;
	Links ->
	    foreach(
	      fun({_,Pid}) ->
		      exit(Pid, {tkwidget,W})
	      end, Links),
	    %% remove links
	    ets:delete(tkLink, [link|W])
    end,
    %% removed widgets linked to W
    ets:match_delete(tkLink, {[link|'_'], W}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  ARGV ENCODER
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Encode an argument list
%%
%% {opt,value}  => -opt value
%% {opt}        => -opt
%% atom x       => x
%% string x     => x
%% integer x    => integer_to_list(x)
%% float x      => float_to_list(x)
%%
%% generated structure:
%% argc     4
%% arg1     4
%% arg2     4
%% arg3     4
%% 
%% argn     4
%% NULL     4
%% Length   4     total length of string data (including string lengths & pad)
%% Len1 (4) data(Len1)
%% Len2 (4) data(Len2)
%% ...
%% Lenk (4) data(Len3)
%% Pad to multiple of 4!
%%
%% where argi is either:
%%     [TK_OP_OFFSET,0,0,0]      for data in data area
%%     [TK_OP_OPTION,I2,I1,I0]   for option
%%     [TK_OP_STRING,I2,I1,I0]   for string
%%

enc_argv(W, Opts) ->
    enc_argv(W, Opts, 0, 0, [], []).

enc_argv(W, [{Opt,Value} | Opts], ALen, Argc, Argv, Args) when atom(Opt) ->
    case ets:lookup(tkPriv, [opt|Opt]) of
	[] ->
	    OptStr = [$- | atom_to_list(Opt)],
	    OptLen = length(OptStr)+1,
	    case enc_arg(W, Value) of
		{string,Str} ->
		    Len = length(Str) + 1,
		    enc_argv(W, Opts, ALen+OptLen+Len+8, Argc + 2,
			     [ Argv, ?arg_data(), ?arg_data()],
			     [ Args, 
			       ?uint32(OptLen), OptStr, 0,
			       ?uint32(Len), Str, 0] );
		{strix,Ix} ->
		    enc_argv(W, Opts, ALen+OptLen+4, Argc + 2,
			     [ Argv, ?arg_data(), ?arg_string(Ix)],
			     [ Args, ?uint32(OptLen), OptStr, 0])
	    end;
	[{_, OptIx}] ->
	    case enc_arg(W, Value) of
		{string,Str} ->
		    Len = length(Str) + 1,
		    enc_argv(W, Opts, ALen + Len + 4, Argc + 2,
			     [ Argv, ?arg_option(OptIx), ?arg_data()],
			     [ Args, ?uint32(Len), Str, 0]);
		{strix,Ix} ->
		    enc_argv(W, Opts, ALen, Argc+2,
			     [ Argv, ?arg_option(OptIx), ?arg_string(Ix)],
			     Args)
	    end
    end;
enc_argv(W, [{Opt} | Opts], ALen, Argc, Argv, Args) when atom(Opt) ->
    case ets:lookup(tkPriv, [opt|Opt]) of
	[] ->
	    OptStr = [$- | atom_to_list(Opt)],
	    OptLen = length(OptStr)+1,
	    enc_argv(W, Opts, ALen + OptLen + 4, Argc+1,
		     [ Argv, ?arg_data()],
		     [ Args, ?uint32(OptLen), OptStr, 0]);
	[{_, OptIx}] ->
	    enc_argv(W, Opts, ALen, Argc+1,
		     [Argv, ?arg_option(OptIx)], Args)
    end;
enc_argv(W, [Val | Opts], ALen, Argc, Argv, Args) ->
    case enc_arg(W, Val) of
	{string, Str} ->
	    Len = length(Str) + 1,
	    enc_argv(W, Opts, ALen + Len + 4, Argc+1, 
		     [Argv, ?arg_data()],
		     [Args, ?uint32(Len), Str, 0]);
	{strix,Ix} ->
	    enc_argv(W, Opts, ALen, Argc+1, 
		     [Argv, ?arg_string(Ix)],
		     Args)
    end;
enc_argv(W, [], ALen, Argc, Argv, Args) ->
    case ALen band 3 of
	0 -> [?uint32(Argc), Argv, 0,0,0,0, ?uint32(ALen), Args];
	1 ->
	    RLen = ALen + 3,
	    [?uint32(Argc), Argv, 0,0,0,0, ?uint32(RLen), Args,0,0,0];
	2 ->
	    RLen = ALen + 2,
	    [?uint32(Argc), Argv, 0,0,0,0, ?uint32(RLen), Args,0,0];
	3 ->
	    RLen = ALen + 1,
	    [?uint32(Argc), Argv, 0,0,0,0, ?uint32(RLen), Args,0]
    end.

%%
%% enc_arg
%% returns:
%%   {string, Str} or {strix, Ix}
%%
enc_arg(W, Arg) when atom(Arg) ->
    case ets:lookup(tkPriv, [str|Arg]) of
	[{_,Ix}] -> {strix, Ix};
	_ -> {string, atom_to_list(Arg)}
    end;
enc_arg(W, Arg) when list(Arg) -> {string, Arg};
enc_arg(W, Arg) when integer(Arg) -> {string,integer_to_list(Arg)};
enc_arg(W, Arg) when float(Arg) -> {string,float_to_list(Arg)};
enc_arg(W, Arg) when function(Arg) -> {string, tkfunction(W, Arg)};
enc_arg(W, Arg) when record(Arg, tkRgb) -> {string,format_rgb(Arg)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  ARGV DECODER
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Small string
dec_argv(Cs) ->
    case catch dec_v(Cs) of
	{'EXIT', Reason} ->
	    io:format("Failure: dec_argv : ~p\n", [Reason]),
	    {error, Reason};
	Other -> 
	    {ok, Other}
    end.

dec_v([L3,L2,L1,L0|Cs0]) ->
    N = (L3 bsl 24) bor (L2 bsl 16) bor (L1 bsl 8) bor L0,
    dec_a(Cs0, N);
dec_v([]) -> 
    [].

dec_a(Cs, 0) -> ["" | dec_v(Cs)];
dec_a([C|Cs], N) when ?is_lower(C) -> %% normal case?
    scan_chars(Cs, N-1, [C]);
dec_a([C|Cs], N) when ?is_digit(C) ->
    scan_number(Cs, N-1, 1, (C-$0), [C]);
dec_a([$-,C|Cs], N) when ?is_digit(C), N > 1 ->
    scan_number(Cs, N-2, -1,(C-$0), [C,$-]);
dec_a([$-,C|Cs], N) when ?is_lower(C), N > 1 ->
    scan_option(Cs, N-2, [C,$-]);
dec_a([C|Cs], N) ->
    scan_chars(Cs, N-1, [C]).

%% scan characters
scan_chars(Cs, 0, Acc) ->
    [reverse(Acc) | dec_v(Cs)];
scan_chars([C|Cs], N, Acc) ->
    scan_chars(Cs, N-1, [C|Acc]).

%% scan option
scan_option(Cs, 0, Acc) ->
    [_|Opt] = reverse(Acc),
    [list_to_atom(Opt) | dec_v(Cs)];
scan_option([C|Cs], N, Acc) ->
    scan_option(Cs, N-1, [C|Acc]).

%% scan numbers
scan_number(Cs, 0, Sign, Val, Acc) ->
    [Sign*Val | dec_v(Cs)];
scan_number([C|Cs], N, Sign, Val, Acc) when C >= $0, C =< $9 ->
    scan_number(Cs, N-1, Sign, Val*10+(C-$0), [C|Acc]);
scan_number([$.|Cs], N, Sign, Val, Acc) ->
    scan_frac(Cs, N-1, Sign, Val, 0.0, 0.1, Acc);
scan_number(Cs, N, Sign, Val, Acc) -> %% revert to chars
    if Sign < 0 ->
	    scan_option(Cs, N, Acc);
       true ->
	    scan_chars(Cs, N, Acc)
    end.

%% scan fraction part
scan_frac(Cs, 0, Sign, Mantissa, Frac, Exp, Acc) ->
    [Sign*(Mantissa+Frac) | dec_v(Cs)];
scan_frac([C|Cs], N, Sign, Mantissa, Frac, Exp, Acc) when ?is_digit(C) ->
    scan_frac(Cs, N-1, Sign, Mantissa,Frac+Exp*(C-$0),Exp*0.1, [C|Acc]);
scan_frac([$e,$-,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>2,?is_digit(C) ->
    scan_exp(Cs,N-3,Sign*(Mantissa+Frac),-1,C-$0,[C,$-,$e|Acc]);
scan_frac([$E,$-,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>2,?is_digit(C) ->
    scan_exp(Cs,N-3,Sign*(Mantissa+Frac),-1,C-$0,[C,$-,$E|Acc]);
scan_frac([$e,$+,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>2,?is_digit(C) ->
    scan_exp(Cs,N-3,Sign*(Mantissa+Frac),1,C-$0,[C,$+,$e|Acc]);
scan_frac([$E,$+,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>2,?is_digit(C) ->
    scan_exp(Cs,N-3,Sign*(Mantissa+Frac),1,C-$0,[C,$+,$E|Acc]);

scan_frac([$e,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>1,?is_digit(C) ->
    scan_exp(Cs,N-2,Sign*(Mantissa+Frac),1,C-$0,[C,$e|Acc]);
scan_frac([$E,C|Cs],N,Sign,Mantissa,Frac,Exp,Acc) when N>2,?is_digit(C) ->
    scan_exp(Cs,N-2,Sign*(Mantissa+Frac),1,C-$0,[C,$E|Acc]);
scan_frac(Cs, N, Sign, Mantissa, Frac, Exp, Acc) ->
    if Sign < 0 ->
	    scan_option(Cs, N, Acc);
       true ->
	    scan_chars(Cs, N, Acc)
    end.    

%% scan exponent part
scan_exp(Cs, 0, Number, Sign, Exp, Acc) ->
    [ Number* math:pow(10, Sign*Exp) | dec_v(Cs)];
scan_exp([C|Cs], N, Number, Sign, Exp, Acc) when ?is_digit(C) ->
    scan_exp(Cs, N-1, Number, Sign, Exp*10 +(C-$0), [C|Acc]);
scan_exp(Cs, N, Number, Sign, Exp, Acc) ->
    if Number < 0 -> 
	    scan_option(Cs, N, Acc);
       true ->
	    scan_chars(Cs, N, Acc)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  EVENT ARGUMENT DECODER
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% return {As, NextEvent}
scan_event(Args) ->
    scan_event(Args, []).

scan_event(["event", _ | Rest], As) ->
    { reverse(As), Rest };
scan_event([ A | Rest], As) ->
    scan_event(Rest, [A|As]);
scan_event([], As) ->
    { reverse(As), [] }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  VALUE DECODER
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_value(Bytes) ->
    case catch dec(Bytes, []) of
	[Value] -> {ok,Value};
	Value -> {ok, reverse(Value)};
	{'EXIT', Reason} -> 
	    io:format("error: when scanning ~p\n", [Bytes]),
	    {error, Reason};
	{error, Reason} ->
	    io:format("error: when scanning ~p\n", [Bytes]),
	    {error, Reason};
	Other -> 
	    io:format("error: when scanning ~p\n", [Bytes]),
	    {error,"unexpected value"}
    end.

dec([C|Cs],S) when ?is_digit(C) -> dec_number(Cs,+1,C-$0,S);
dec([], S) -> S;
dec([$   |Cs],S)  -> dec(Cs,S);
dec([$\t |Cs], S) -> dec(Cs, S);
dec([$\n |Cs], S) -> dec(Cs, S);
dec([$"  |Cs], S) -> dec_string(Cs,[],S);
dec([$-,C|Cs], S) ->
    if
	?is_digit(C) -> dec_number(Cs,-1,C-$0,S);
	?is_lower(C) -> dec_option(Cs, [C,$-], S);
	?is_upper(C) -> dec_name(Cs, [C,$-], S);
	true -> dec_name([C|Cs], [$-], S)
    end;
dec([$+,C|Cs], S) ->
    if
	?is_digit(C) -> dec_number(Cs,+1,C-$0,S);
	true -> dec_name([C|Cs], [$+], S)
    end;
dec([$# | Cs], S) -> dec_rgb(Cs, [], S);
dec([${ | Cs], S) -> dec(Cs, [{}|S]);
dec([$} | Cs], S) -> dec_list(Cs, [], S);
dec([C|Cs],S) -> dec_name(Cs, [C], S);
dec([], _) -> throw({error, "unterminated list"}).

%%
%% decode list by build a list of all items until {}
%%
dec_list(Cs, Acc, [{}|S]) -> dec(Cs, [Acc|S]);
dec_list(Cs, Acc, [E|S]) ->  dec_list(Cs, [E|Acc], S);
dec_list(Cs, Acc, []) ->
    throw({error,"mismatched list termination"}).

%%
%% Scan a name terminated with a blank or a }
%%
dec_name([C|Cs], Acc, S) ->
    case C of
	$   -> dec(Cs, [reverse(Acc)|S]);
	$\t -> dec(Cs, [reverse(Acc)|S]);
	$}  -> dec([$} | Cs], [reverse(Acc)|S]);
	_   -> dec_name(Cs, [C|Acc], S)
    end;
dec_name(Cs, Acc, S) -> dec(Cs, [reverse(Acc)|S]).

%%
%% Scan an option  -Opt thing
%%
dec_option([C|Cs],Acc,S)  ->
    if ?is_lower(C) -> dec_option(Cs,[C|Acc],S);
       ?is_upper(C) -> dec_option(Cs,[C|Acc],S);
       ?is_digit(C) -> dec_option(Cs,[C|Acc],S);
       C == $_ ->  dec_option(Cs,[C|Acc],S);
       C == $- ->  dec_name(Cs,[C|Acc],S);
       true ->
	    [$-|Opt] = reverse(Acc),
	    dec([C|Cs], [list_to_atom(Opt)|S])
    end;
dec_option(Cs, Acc, S) ->
    [$-|Opt] = reverse(Acc),
    dec(Cs, [list_to_atom(Opt)|S]).

%%
%% scan numbers on form [+-][0-9]+[[.][0-9]+[E|e[+-][0-9]+]]
%%
dec_number([C|Cs], Sign, Val, S) when C >= $0, C =< $9 ->
    dec_number(Cs, Sign, Val*10+(C-$0), S);
dec_number([$.|Cs], Sign, Val, S) ->
    dec_frac(Cs, Sign, Val, 0.0, 0.1, S);
dec_number(Cs, Sign, Val, S) ->
    dec(Cs, [Val*Sign|S]).

%% scan fraction part
dec_frac([C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_frac(Cs, Sign, Mantissa,Frac+Exp*(C-$0),Exp*0.1, S);
dec_frac([$e,$-,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),-1,C-$0,S);
dec_frac([$E,$-,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),-1,C-$0,S);
dec_frac([$e,$+,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),1,C-$0,S);
dec_frac([$E,$+,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),1,C-$0,S);
dec_frac([$e,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),1,C-$0,S);
dec_frac([$E,C|Cs],Sign,Mantissa,Frac,Exp,S) when ?is_digit(C) ->
    dec_exp(Cs,Sign*(Mantissa+Frac),1,C-$0,S);
dec_frac(Cs, Sign, Mantissa, Frac, Exp, S) ->
    Val = Sign*(Mantissa+Frac),
    case trunc(Val) of
	Int when Int == Val ->
	    dec(Cs, [Int|S]);
	_ ->
	    dec(Cs, [Val|S])
    end.

%% scan exponent part
dec_exp([C|Cs], Number, Sign, Exp, S) when ?is_digit(C) ->
    dec_exp(Cs, Number, Sign, Exp*10 +(C-$0), S);
dec_exp(Cs, Number, Sign, Exp, S) ->
    Val = Number* math:pow(10, Sign*Exp),
    case trunc(Val) of
	Int when Int == Val ->
	    dec(Cs, [Int|S]);
	_ ->
	    dec(Cs, [Val|S])
    end.

dec_rgb([C|Cs],Acc,S) ->
    if
	?is_digit(C) -> dec_rgb(Cs, [C|Acc], S);
	C >= $a, C =< $f -> dec_rgb(Cs, [C|Acc], S);
	C >= $A, C =< $F -> dec_rgb(Cs, [C|Acc], S);
	true ->
	    dec([C|Cs], [mk_rgb(reverse(Acc))|S])
    end;
dec_rgb(Cs, Acc, S) ->
    dec(Cs, [mk_rgb(reverse(Acc))|S]).

%% XXX handle escape chars!
dec_string([$"|Cs],Acc,S) -> dec(Cs, [reverse(Acc)|S]);
dec_string([C|Cs],Acc,S) ->  dec_string(Cs,[C|Acc],S);
dec_string([],Acc,S) -> throw({error,"unterminated string."}).

%% Value substitute (in flat list only)
%% subst(This, ForThat, InList)
%%
subst(A, B, [B|Bs]) -> [A|subst(A,B,Bs)];
subst(A, B, [X|Bs]) when list(X) ->  %% should be string
    case is_prefix(B, X) of
	{true,Xs} -> [A++Xs|subst(A,B,Bs)];
	false -> [X|subst(A,B,Bs)]
    end;
subst(A, B, [X|Bs]) -> [X|subst(A,B,Bs)];
subst(A, B, []) -> [].

%% check if the first list is a prefix of the other
%% then return the tail after the common prefix
%% false otherwise
%%
is_prefix([H|T1], [H|T2]) -> is_prefix(T1,T2);
is_prefix([], T2) -> {true, T2};
is_prefix(_, _) -> false.

%%
%% Create event names
%%
-define(CONTROL_MASK, 16#00001).  %% c
-define(SHIFT_MASK,   16#00002).  %% s
-define(LOCK_MASK,    16#00004).  %% l
-define(META_MASK,    16#00008).  %% m
-define(ALT_MASK,     16#00010).  %% a
-define(BUTTON1_MASK, 16#00020).  %% 1
-define(BUTTON2_MASK, 16#00040).  %% 2
-define(BUTTON3_MASK, 16#00080).  %% 3
-define(BUTTON4_MASK, 16#00100).  %% 4
-define(BUTTON5_MASK, 16#00200).  %% 5
-define(MOD1_MASK,    16#00400).  %% 6
-define(MOD2_MASK,    16#00800).  %% 7
-define(MOD3_MASK,    16#01000).  %% 8
-define(MOD4_MASK,    16#02000).  %% 9
-define(MOD5_MASK,    16#04000).  %% 0
-define(DOUBLE_MASK,  16#08000).  %% d
-define(TRIPLE_MASK,  16#10000).  %% t
                                  %% n if no mod present

-define(KEYPRESS,         "k").
-define(KEYRELEASE,       "r").
-define(BUTTONPRESS,      "bp").
-define(BUTTONRELEASE,    "br").
-define(MOTIONNOTIFY,     "m").
-define(ENTERNOTIFY,      "e").
-define(LEAVENOTIFY,      "l").
-define(FOCUSIN,          "fi").
-define(FOCUSOUT,         "fo").
-define(EXPOSE,           "x").
-define(VISIBILITYNOTIFY, "v").
-define(DESTROYNOTIFY,    "d").
-define(UNMAPNOTIFY,      "un").
-define(MAPNOTIFY,        "mn").
-define(REPARENTNOTIFY,   "re").
-define(CONFIGURENOTIFY,  "co").
-define(GRAVITYNOTIFY,    "gn").
-define(CIRCULATENOTIFY,  "cn").
-define(PROPERTYNOTIFY,   "pn").
-define(COLORMAPNOTIFY,   "cm").
-define(ACTIVATENOTIFY,   "at").
-define(DEACTIVATENOTIFY, "dt").

%% modifiers (bitmasked)
mod("Control") ->       ?CONTROL_MASK;
mod("Shift")   ->       ?SHIFT_MASK;
mod("Lock") ->	        ?LOCK_MASK;
mod("Meta") ->		?META_MASK;
mod("M") ->		?META_MASK;
mod("Alt") ->		?ALT_MASK;
mod("B1") ->		?BUTTON1_MASK;
mod("Button1") ->	?BUTTON1_MASK;
mod("B2") ->		?BUTTON2_MASK;
mod("Button2") ->	?BUTTON2_MASK;
mod("B3") ->		?BUTTON3_MASK;
mod("Button3") ->	?BUTTON3_MASK;
mod("B4") ->		?BUTTON4_MASK;
mod("Button4") ->	?BUTTON4_MASK;
mod("B5") ->		?BUTTON5_MASK;
mod("Button5") ->	?BUTTON5_MASK;
mod("Mod1") ->		?MOD1_MASK;
mod("M1") ->		?MOD1_MASK;
mod("Command") ->	?MOD1_MASK;
mod("Mod2") ->		?MOD2_MASK;
mod("M2") ->		?MOD2_MASK;
mod("Option") ->	?MOD2_MASK;
mod("Mod3") ->		?MOD3_MASK;
mod("M3") ->		?MOD3_MASK;
mod("Mod4") ->		?MOD4_MASK;
mod("M4") ->		?MOD4_MASK;
mod("Mod5") ->		?MOD5_MASK;
mod("M5") ->		?MOD5_MASK;
mod("Double") ->	?DOUBLE_MASK;
mod("Triple") ->	?TRIPLE_MASK;
mod("Any") ->		0;
mod(_) -> -1.

%% events (named)
evt("Key")           -> ?KEYPRESS;
evt("KeyPress")      -> ?KEYPRESS;
evt("KeyRelease")    -> ?KEYRELEASE;
evt("Button")        -> ?BUTTONPRESS;
evt("ButtonPress")   -> ?BUTTONPRESS;
evt("ButtonRelease") -> ?BUTTONRELEASE;
evt("Motion")        ->	?MOTIONNOTIFY;
evt("Enter")         -> ?ENTERNOTIFY;
evt("Leave")         -> ?LEAVENOTIFY;
evt("FocusIn")       -> ?FOCUSIN;
evt("FocusOut")      -> ?FOCUSOUT;
evt("Expose")        -> ?EXPOSE;
evt("Visibility")    -> ?VISIBILITYNOTIFY;
evt("Destroy")       -> ?DESTROYNOTIFY;
evt("Unmap")         -> ?UNMAPNOTIFY;
evt("Map")           -> ?MAPNOTIFY;
evt("Reparent")      -> ?REPARENTNOTIFY;
evt("Configure")     -> ?CONFIGURENOTIFY;
evt("Gravity")       -> ?GRAVITYNOTIFY;
evt("Circulate")     -> ?CIRCULATENOTIFY;
evt("Property")      -> ?PROPERTYNOTIFY;
evt("Colormap")      -> ?COLORMAPNOTIFY;
evt("Activate")      -> ?ACTIVATENOTIFY;
evt("Deactivate")    -> ?DEACTIVATENOTIFY;
evt(_) -> [].

cl(W) when hd(W) == $. -> W; %% instance
cl("all") -> "a";
cl("Button") -> "b";
cl("Canvas") -> "c";
cl("Checkbutton") -> "cb";
cl("Entry") -> "e";
cl("Frame") -> "f";
cl("Label") -> "l";
cl("Listbox") -> "li";
cl("Menu") -> "m";
cl("Menubutton") -> "mb";
cl("Message") -> "me";
cl("Radiobutton") -> "r";
cl("Scale") -> "s";
cl("Scrollbar") -> "sb";
cl("Text") -> "tx";
cl("Toplevel") -> "t";
cl(W) -> W.
    
split_event([$<,$<|Cs]) ->
    {Field,_} = get_field(Cs),
    [Field];
split_event([$<|Cs]) ->
    split_event(Cs, []).

split_event([], Acc) -> 
    reverse(Acc);
split_event(Cs, Acc) ->
    case skip(Cs) of
	[] -> reverse(Acc);
	Cs1 ->
	    {Field, Cs2} = get_field(Cs1),
	    split_event(Cs2, [Field|Acc])
    end.


get_field(Cs) ->
    get_field(Cs, []).

get_field([$ | Cs],Acc) -> {reverse(Acc),  Cs};
get_field([$- | Cs],Acc) -> {reverse(Acc), Cs};
get_field([$> | Cs], Acc) -> {reverse(Acc), []};
get_field([C | Cs], Acc) -> get_field(Cs, [C|Acc]);
get_field([], Acc) -> {reverse(Acc), []}.

skip([$- | Cs]) -> skip(Cs);
skip([$  | Cs]) -> skip(Cs);
skip(Cs) -> Cs.

get_mod(Fs) ->
    get_mod(Fs, 0).

get_mod([F | Fs], Mod) ->
    case mod(F) of
	-1 -> {Mod, [F|Fs]};
	M  -> get_mod(Fs, M bor Mod)
    end.

enc_mod(0) -> "n";
enc_mod(Mod) -> enc_mod(Mod, "cslma1234567890dt").

enc_mod(Mod, [C|Cs]) ->
    if Mod band 1 == 1 -> [C|enc_mod(Mod bsr 1, Cs)];
       true -> enc_mod(Mod bsr 1, Cs)
    end;
enc_mod(Mod, []) -> [].
    
mangle(Name) ->
    L0 = split_event(Name),
    case get_mod(L0) of
	{0,[Evt|T]} -> enc_evt(T,Evt,evt(Evt));
	{Mod,[Evt|T]} ->
	    enc_mod(Mod) ++ ":" ++ enc_evt(T,Evt,evt(Evt))
    end.

enc_evt(_, [C], []) when C >= $1,C =< $5 -> ?BUTTONPRESS ++ [$:,C];
enc_evt([], Key, []) -> ?KEYPRESS ++ ":" ++ Key;
enc_evt([], _, Event) -> Event;
enc_evt([Key], _, Event) -> Event ++ ":" ++ Key.


mkname(Widget, Event) -> 
    mkname(Widget, [], Event).

mkname(Widget, Tag, Event) ->
    cl(Widget) ++ ":" ++
	if
	    Tag == [] -> "";
	    list(Tag) -> Tag ++ ":";
	    atom(Tag) -> atom_to_list(Tag) ++ ":";
	    integer(Tag) -> integer_to_list(Tag) ++ ":"
	end ++
	mangle(Event).

%%
%% strictMotif must set an internal variable as well !!!
%%
tkStrictMotif(true) ->
    tk:setvar(tk_strictMotif, 1),
    ?tkset(tk_strictMotif, true);
tkStrictMotif(false) ->
    tk:setvar(tk_strictMotif, 0),
    ?tkset(tk_strictMotif, false).

%%
%% Read tcl value of strictMotif
%%
tkStrictMotif() ->
    case tk:getvar(tk_strictMotif) of
	1 -> true;
	0 -> false
    end.

%%
%% INTERNAL! May only be used in event bindings !!
%%
tkCancelRepeat() ->
    Ref = ?tkget(afterId),
    case erlang:cancel_timer(Ref) of
	false ->  %% no timer set or timer already trigged
	    receive
		{timeout,Ref,_} -> %% remove timer event
		    true
	    after 0 ->
		    true
	    end;
	TimeLeft ->
	    true
    end.

%%
%% INTERNAL! May only be used in event bindings !!
%%
tkRepeat(Ms, Fun) ->
    tkRepeat(Ms, Fun, []).

tkRepeat(Ms, Fun, As) ->
    Ref = erlang:start_timer(Ms,self(),{timer_event,Fun,As}),
    ?tkset(afterId, Ref),
    true.

register_function(W, Fun) when function(Fun) ->
    Id = ets:update_counter(tkPriv, func, 1),
    ets:insert(tkFun, {Id, W, Fun}),
    Id.

insert_function(Id, W, Fun) ->
    ets:insert(tkFun, {Id, W, Fun}).

unregister_function(Id) ->
    ets:delete(tkFun, Id).

tkfun(Id) ->
    case ets:lookup(tkFun, Id) of
	[{_,_,Fun}] -> Fun;
	_ ->
	    io:format("Error: function ~p not found\n", [Id]),
	    undefined
    end.

tkfunction(W, Fun) ->
    Id = register_function(W, Fun),
    "invoke " ++ integer_to_list(Id).

%%
%% Register an event function:
%% on form widget[:tag][:mod]:event[:key]
%%

tkeventfun(W,Event,Template,Fun) ->
    case catch mkname(W, Event) of
	{'EXIT',Reason} ->
	    io:format("failed to mkname : ~p ~p : ~p\n",[W,Event,Reason]),
	    Id = register_function(W, Fun),
	    "event %W " ++ integer_to_list(Id) ++ tkeventstring(Template);
	Id ->
	    insert_function(Id, W, Fun),
	    "event %W " ++ Id ++ tkeventstring(Template)
    end.

tkeventfun(W,Tag,Event,Template,Fun) ->
    case catch mkname(W,Tag,Event) of
	{'EXIT',Reason} ->
	    io:format("failed to mkname : ~p ~p : ~p\n",[W,Event,Reason]),
	    Id = register_function(W, Fun),
	    "event %W " ++ integer_to_list(Id) ++ tkeventstring(Template);
	Id ->
	    insert_function(Id, W, Fun),
	    "event %W " ++ Id ++ tkeventstring(Template)
    end.

%%
%% Generate event string
%% '%x' -> %x  
%% 'abc cdef' -> abc cdef
%% 123        -> 123
%% "abc"      -> abc
%% ...
%%

tkeventstring([H|T]) ->
    if
	atom(H) ->
	    [$  | atom_to_list(H) ++ tkeventstring(T)];
	integer(H) ->
	    [$  | integer_to_list(H) ++ tkeventstring(T)];
	list(H) ->
	    [$  | H ++ tkeventstring(T)];
	float(H) ->
	    [$  | float_to_list(H) ++ tkeventstring(T)]
    end;
tkeventstring([]) -> [].
