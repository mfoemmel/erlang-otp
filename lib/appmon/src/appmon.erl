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
-module(appmon).

%%----------------------------------------------------------------------
%%
%%
%% Manages top window
%%
%%
%%----------------------------------------------------------------------


-export([start/0, stop/0]).


%% gen_server stuff
-export([init/1, handle_cast/2, handle_info/2, terminate/2]).

-behaviour(gen_server).



-define(MON, {global, appmon}).			% The name of this server

-define(QUITTXT, "Quit").
-define(HELPTXT, "Help").
-define(LOAD1TXT, "Load: time").
-define(LOAD2TXT, "Load: queue").
-define(LOAD3TXT, "Load: progressive").
-define(LOAD4TXT, "Load: linear").

-define(MAXW, 500).
-define(MINW, 470).

-define( lightsteelblue, {176, 196, 222}).
-define( black, black).
-define( antiquewhite, {250, 235, 215}).

%% led colours
-define( highloadfg, {255,  99,  71}).
-define( midloadfg, yellow).
-define( lowloadfg,  green).

-define( highloadbg, {140, 157, 178}).
-define( midloadbg, ?highloadbg).
-define( lowloadbg,  ?highloadbg).


%%------------------------------------------------------------
%%------------------------------------------------------------


%%------------------------------------------------------------
%% Record representing an application in a monitor frame.
%%
%% An application is represented (currently) as a button with 
%% coords x,y,w,h below. Then a connection line is drawn between 
%% the application and the application master.
%%
%% 
%% name		- The application name (or node name for controller butt)
%% type		- master|app
%% butt		- butt id
%% connection	- line id of connection between app and master
%% aux		- used for master menu
%% 		- 
%% 
%%
-record(gui_app, {name, 
		  pid, 
		  type, 
		  butt, 
		  connection, 
		  aux}).


%%------------------------------------------------------------
%% Record representing a node in a monitor frame.
%%
%% An application is represented (currently) as a button with 
%% coords x,y,w,h below. Then a connection line is drawn between 
%% the application and the application master.
%%
%% 
%% win		- is monitor frame
%% name		- is the name of the appl.
%% me		- is my button (or whatever: handle to gs)
%% txt		- is the text in the application button
%% x,y,w,h	- is coordinates for application button/label
%% high		- is coordinates for upper connection point for application
%% low		- is coordinates for lower connection point for applications
%% 
%%
-record(gui_node, {node = node, 
		   name = name, 
		   pid = pid, 
		   master = master, 
		   cross,
		   aux = aux,
		   end_line = end_line,
		   meter_leds = nil,
		   apps = []
		  }).


%%------------------------------------------------------------
%% Record keeping track of the window stuff
%%
%% An application is represented (currently) as a button with 
%% coords x,y,w,h below. Then a connection line is drawn between 
%% the application and the application master.
%%
%% 
%% root		- the window system itself
%% win		- is root (top level) window
%% frame	- the monitor frame
%% canvas	- the canvas on which lines and rectangles are drawn
%% 
-record(win, {root,
	      win}).


%%------------------------------------------------------------
%% Record keeping track of state, which is done by keeping track 
%% of all nodes
%%
%% CURRENTLY A RAW LIST OF NODES
%%
%% nodes	- a list of nodes
%% 
-record(state, {win, 
		client_opts = [],
		node_count = 0,
		nodes = [],
		lb_pid}).          %% the pid of the listbox window





%%------------------------------------------------------------
%%------------------------------------------------------------

start() ->
     gen_server:start(?MON, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MON, stop).


%%------------------------------------------------------------
%% Public Interface

%%------------------------------------------------------------
%% Administration

init(X) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    LbPid = appmon_lb:start(self ()),
    setup_base_win(),
    State = start_clients([node()|nodes()], #state{lb_pid = LbPid}),
    {ok, State}.

terminate(A1, State) ->
    kill_all_nodes(State#state.nodes),
    appmon_lb:stop(State#state.lb_pid),
    ok.

%%------------------------------------------------------------
%% handle cast

handle_cast(stop, State) ->
    {stop,  shutdown, State}.
    

%%------------------------------------------------------------
%% handle info


handle_info({delivery, Serv, load, Node, {Old, New}}, State) ->
    load_report(State, Node, {Old, New}),
    {noreply, State};
handle_info({delivery, Serv, app_ctrl, NodeName, Apps}, State) ->
    Node = find_node(State#state.nodes, NodeName),
    A2 = del_apps(Node#gui_node.apps, Apps),
    NewNode = add_apps(Apps, Node#gui_node{apps=A2}),
    NewNodes = replace_node(State#state.nodes, NewNode),
    appmon_lb:add_apps (State#state.lb_pid, Apps, NodeName),
    resize(NewNodes),
    {noreply, State#state{nodes=NewNodes}};
handle_info({gs, ButtId, click, Data, ButtTxt}, State) ->
    butt_pressed(ButtId, ButtTxt, Data, State);

handle_info({nodeup, NodeName}, State) ->
    NewState = start_clients([NodeName], State),
    appmon_lb:update_status(NewState#state.lb_pid, NodeName, alive),
    {noreply, NewState};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({kill}, State) ->
    {stop, normal, State};
handle_info({gs, _, destroy, _, _}, State) ->	% OTP-1179
    {stop, normal, State};
handle_info({state}, State) ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
    case winroot() of
	{_, Pid} ->
	    %% GS exited, kill appmon
	    {stop, normal, State};
	_ ->
	    NewState = del_node(State, Pid),
	    {noreply, NewState}
    end;
handle_info(Other, State) ->
    {noreply, State}.
	
%%------------------------------------------------------------
%% Real functionality

butt_pressed(_, [?QUITTXT|_], Data, State) ->
    {stop, normal, State};
butt_pressed(_, Txt, {bcast, Msg}, State) ->
    bcast(State#state.nodes, Msg),
    {noreply, State};
butt_pressed(_, Txt, {load_opt, Opts}, State) ->
    lists:foreach(fun(N) -> 
			 appmon_info:load(N#gui_node.pid, N#gui_node.name, 
					  true, Opts) end,
		  State#state.nodes),
    {noreply, State};
butt_pressed(_, ["Debug On"], Data, State) ->
    {noreply, State};
butt_pressed(_, ["Debug Off"], Data, State) ->
    {noreply, State};

butt_pressed(_, ["List Box" | _], _Data, State) ->
    Nodes_apps = parse_nodes ([], State#state.nodes),
    appmon_lb:open_win (State#state.lb_pid, Nodes_apps),
    {noreply, State};

butt_pressed(_, [?HELPTXT|_], Data, State) ->
    catch 
	appmon_txt:fprint([code:priv_dir(appmon) ++ 
			   "/appmon_help.txt"]),
    {noreply, State};
butt_pressed(ButtId, ButtTxt, {M, F, A}, State) ->
    catch apply(M, F, A),
    {noreply, State};
butt_pressed(ButtId, ButtTxt, {N, {M, F, A}}, State) ->
    catch rpc:cast(N, M, F, A),
    {noreply, State}.


find_butt(ButtId, ButtTxt, []) -> {"button not found"};
find_butt(ButtId, ButtTxt, [N|Ns]) ->
    Master = N#gui_node.master,
    case lists:keysearch(ButtId, 1, Master#gui_app.aux) of
	{value, {ButtId, proc_view}} ->
	    {node, N, nil, ButtTxt};
	{value, {ButtId, Command}} ->
	    {cmd, Command, N, nil, ButtTxt};
	Other ->
	    case lists:keysearch(ButtId, 4, N#gui_node.apps) of
		{value, App} ->
		    {app, N, App, ButtTxt};
		Other ->
		    find_butt(ButtId, ButtTxt, Ns)
	    end
    end.


%%--------------------
%% New node comming up

%% Returns list of {ok, Pid, Node}
start_clients([C|Cs], State) ->
    case catch start_client(C, State) of
	{Pid, NewState} -> start_clients(Cs, NewState);
	_ ->
	    io:format("Could not start ~p~n", [C]),
	    start_clients(Cs, State)
    end;
start_clients([], State) -> State.
    
start_client(Node, State) ->
    {ok, Pid} = appmon_info:start_link(Node, self(), State#state.client_opts),
    NewNodes = add_node2(State, Pid, Node, State#state.nodes),
    NewState = State#state{nodes=NewNodes, 
			   node_count=length(NewNodes)},
    update_win_h(NewState),
    appmon_info:load(Pid, Node, true, [{timeout, 1000}]),
    appmon_info:app_ctrl(Pid, Node, true, []),
    {Pid, NewState}.

add_node2(State, From, Node, []) ->
    {BaseY, Line, Leds} = new_node_area(State),
    M = mk_appl_master(BaseY, Node),
    N = #gui_node{node=Node, name=Node, pid=From, 
		  master=M,
		  end_line=Line,
		  meter_leds=Leds,
		  apps=[]},
    appmon_lb:add_node (State#state.lb_pid, N#gui_node.name),
    [N];
add_node2(State, From, Node, [N|Nodes]) when N#gui_node.name==Node ->
    lists:foreach(fun(X) -> destroy(X) end, N#gui_node.cross),
    lists:foreach(fun(X) -> remove_app(X) end, N#gui_node.apps),
    [N#gui_node{apps=[], pid=From}|Nodes];
add_node2(State, From, Node, [N|Nodes]) ->
    [N|add_node2(State, From, Node, Nodes)].
    



%%--------------------
%% Node gone down

%% Called from exit signal received
del_node(State, Pid) ->
    NewNodes = dn(Pid, State#state.nodes, State#state.lb_pid),
    %%compact_n(NewNodes),
    NewState = State#state{nodes=NewNodes, 
			   node_count=length(NewNodes)},
    %%update_win_h(NewState),
    NewState.

dn(Pid, [], _LbPid) -> [];
dn(Pid, [N|Ns], LbPid) when N#gui_node.pid == Pid ->
    %%remove_node(N),
    appmon_lb:remove_node (LbPid, N#gui_node.node),
    NewNode = show_dead_node(N),
    [NewNode|Ns];
dn(Pid, [N|Ns], LbPid) -> 
    [N| dn(Pid,Ns, LbPid)].


show_dead_node(N) ->
    %% put a cros over the node
    BaseY = get_base_y(N),
    RX = 0,
    LX = node_area_w()-50,
    UY = BaseY,
    LY = BaseY+node_area_h()-20,
    L1 = gs:create(line, canvas(), [{width, 6}, {fg, red}, raise,
				    {coords, [{LX, UY}, {RX, LY}]}]),

    L2 = gs:create(line, canvas(), [{width, 6}, {fg, red}, raise,
				    {coords, [{LX, LY}, {RX, UY}]}]),
    leds_down(N#gui_node.meter_leds, led_count(), 0),
    N#gui_node{cross=[L1, L2]}.



remove_app(A) ->
    destroy(A#gui_app.butt),
    destroy(A#gui_app.connection).

compact_n([]) -> 0;
compact_n([N|Ns]) ->
    LastY = compact_n(Ns),
    Y = get_base_y(N),
    %%?D("Node: ~p, Y: ~p, LastY: ~p~n", 
    %%	[N#gui_node.node, Y, LastY]),
    if 
	Y > LastY ->
	    move_node(N, LastY-Y);
	true ->
	    true
    end,
    LastY+node_area_h().


move_node(N,DeltaY) ->
    %%?D("Moving Node: ~p, ~p~n", [N#gui_node.node,DeltaY]),
    move_y(N#gui_node.master, DeltaY),
    move_y(lists:nth(led_count()+1,N#gui_node.meter_leds), DeltaY),
    move_line_y(N#gui_node.end_line, DeltaY),
    move_apps(N#gui_node.apps, DeltaY).

move_apps([], DeltaY) -> [];
move_apps([A|As], DeltaY) ->
    move_y(A, DeltaY),
    move_line_y(A#gui_app.connection, DeltaY),
    move_apps(As, DeltaY).



%%--------------------
%% New application comming up
    

%% Add those applications occuring in NewApps but not in OldApps. Note
%% that OldApps is a lists of gui_apps while NewApps is a list of
%% application tuples {Pid, Name, WhateverWhichApplReturned}. Returns
%% updated node. Note also the non-intuitive order
add_apps([A | As], Node) ->
    Pid = element(1, A),
    Name = element(2, A),
    case lists:keysearch(Pid, #gui_app.pid, Node#gui_node.apps) of
	{value, _} -> 
	    add_apps(As, Node);

	_ ->
	    N2 = add_apps(As, Node),
	    NewApps = [mk_appl(N2, Pid, Name) | N2#gui_node.apps],
	    N2#gui_node{apps = NewApps}
    end;
add_apps([], Node) -> Node.
	    
    

%%--------------------
%% Application disappeared

%% Delete those applications occuring in OldApps but not in
%% NewApps. Note that OldApps is a list of gui_apps while NewApps is a
%% list of application names. Returns updated list of applications.
del_apps(OldApps, NewApps) ->
    Apps = del_apps2(OldApps, NewApps),
    compact_a(Apps),
    Apps.

del_apps2([App | Apps], NewApps) ->
    case lists:keysearch(App#gui_app.name, 1, NewApps) of
	{value, _} -> 
	    [App | del_apps2(Apps, NewApps)];

	_ -> 
	    remove_app(App),
	    del_apps2(Apps, NewApps)
    end;
del_apps2([], NewApps) -> [].


compact_a([]) -> 10;
compact_a([A|As]) ->
    LastX = compact_a(As),
    X = get_x(A),
    %%?D("Compacting ~p, x=~p, lastx=~p~n", [A#gui_app.name, X, LastX]),
    if 
	X > LastX ->
	    move_x(A, LastX-X);
	true ->
	    true
    end,
    LastX+get_w(A)+app_space().


%%--------------------
%% Load report from Node
load_report(State, Node, {Old, New}) ->
    Leds = find_leds(Node, State#state.nodes),
    display_load(Leds, Old, New),
    ok.

find_leds(Node, [N|Ns]) when N#gui_node.node == Node ->
    N#gui_node.meter_leds;
find_leds(Node, [N|Ns]) ->
    find_leds(Node, Ns).
    

%%------------------------------------------------------------
%% Utilities

kill_all_nodes(Nodes) ->
    bcast(Nodes, {kill}).
bcast([], Msg) -> done;
bcast([N|Nodes], Msg) ->
    send(N, Msg),
    %%N#gui_node.pid ! Msg,
    bcast(Nodes, Msg).


%% Node utilities
find_node([N | Ns], Name) when N#gui_node.name == Name -> N;
find_node([N | Ns], Name) -> find_node(Ns, Name).

replace_node([N1 | Ns], N2) when N1#gui_node.name == N2#gui_node.name -> 
    [N2 | Ns];
replace_node([N1 | Ns], N2) ->
    [N1 | replace_node(Ns, N2)].



send(X, Msg) when record(X, gui_node) ->
    X#gui_node.pid ! Msg;
send(X, Msg) ->
    X ! Msg.


find_app(ButtId, []) -> ok;
find_app(ButtId, [N|Nodes]) -> 
    case lists:keysearch(ButtId, 3, N#gui_node.apps) of
	{value, App} ->
	    {N, App};
	Other ->
	    find_app(ButtId, Nodes)
    end.
    



%%------------------------------------------------------------
%% Bridges between graphical and program
%%------------------------------------------------------------




%%------------------------------------------------------------
%% Shall create the application master, both on screen and as 
%% an application
%% 
%% Shall return a new application
%%
mk_appl_master(BaseY, Node) ->
    Txt = atom_to_list(Node),

    %%B = mk_butt(frame(), Txt, 10, BaseY, app_butt_h()),
    
    MB = gs:create(menubutton, frame(), [{x,10}, {y,BaseY},
					 %%{width, length(Txt)}, 
					 %%{height, 1}, 
					 {fg, master_fg()},
					 {bg, master_bg()},
					 {label, {text,Txt}}]),

    
    M = gs:create(menu,MB,[]),
    %%M5 = gs:create(menuitem,M,[{label,{text,"Process view"}}]),
    M1 = gs:create(menuitem,M,[{label,{text,"Reboot"}},
			       {data, {Node, {init, reboot, []}}}]),
    M2 = gs:create(menuitem,M,[{label,{text,"Restart"}},
			       {data, {Node, {init, restart, []}}}]),
    M3 = gs:create(menuitem,M,[{label,{text,"Stop"}},
			       {data, {Node, {init, stop, []}}}]),
    M4 = gs:create(menuitem,M,[{label,{text,"Ping"}},
			       {data, {net, ping, [Node]}}]),

    #gui_app{name=Node, type=master, butt=MB, 
	     aux=[{M1, reboot}, {M2, restart}, {M3, stop}, {M4, ping}]}.%%, 


%%------------------------------------------------------------
%% Shall find a place for an application button and set up 
%% all coordinates needed. Then the application will be displayed
%%
%% Assumption: the first application in State is the rightmost one.
%%
%% Shall return a partly filled gui_app record
%%
mk_appl(Node, Pid, Name) ->
    Master = Node#gui_node.master,
    BaseY = get_base_y(Node),
    Apps = Node#gui_node.apps,
    
    %% First find a place for the new app
    X = find_app_x(Apps),
    Y = BaseY+master_butt_h()+app_space_h(),

    %%Y = BaseY+node_area_h()-40,
    Txt = Name,

    B = mk_butt(frame(), Txt, X, Y, app_butt_h(), 
		{appmon_a, start, [Node#gui_node.node, Name, Pid]}),

    A1 = #gui_app{name=Name, pid=Pid, type=app, butt=B},

    Line = connect(canvas(), Master, A1),

    A1#gui_app{connection=Line}.


find_app_x([]) -> 10;
find_app_x([A|_]) when record(A, gui_app) ->
    {X,Y,W,H} = get_xywh(A#gui_app.butt),
    X+W+app_space().


%%------------------------------------------------------------
%% Shall connect two applications with a line
%% Will make nice "broken" line
%%
%% Assumption: the From application is above the To application
%% 
%% Shall return a handle to the line
%%
connect(Canvas, From, To) ->
    {X1,Y1} = get_low(From),
    {X2,Y2} = get_high(To),
    Coords = calc_coords(X1, Y1, X2, Y2),
    draw_line(Canvas, Coords).








%%------------------------------------------------------------
%% Graphical stuff
%%------------------------------------------------------------

setup_base_win() ->
    %% Setup basic window
    set_winroot(gs:start([{kernel,true}])),

    %%W = 500, H = 0,
    W = ?MINW+meter_area_w(), H = 0,
    Win = gs:create(window, winroot(), [{title,"appmon"}, 
					{x, 100}, {y, 200},
					{width, W},
					{height, H+std_butt_area_h()},
					{bg, bg()}]),
    set_win(Win),
    
    %% standard buttons
    mk_std_butts(Win, W),

    {F, L, C} = mk_area(Win, meter_area_w(), std_butt_area_h(), 
			W-meter_area_w(), H),
    
    %% Area for load meters
    M = gs:create(frame, win(), [{x,0}, {y,std_butt_area_h()},
				 {width, meter_area_w()}, 
				 {height, H}, {bg, bg()}]),
    
    set_frame(F), set_canvas(C), set_meter_frame(M), set_line(L),

    gs:config(Win, {map, true}),		%Make win visible
    ok.

mk_area(Win, X, Y, W, H) ->
    F = gs:create(frame, Win,[{x,X}, {y,Y}, {bg, bg()},
			      {width,W}, {height,H}]),
    C = gs:create(canvas,F,[{x,0}, {y,0}, {width, W}, {height, H}, 
			    {bg, bg()}]),
    
    L = gs:create(line,C,[{coords,[{0,H-10}, {W,H-10}]}]),
    {F, L, C}.

mk_std_butts(Win, W) ->
    {F, L, C} = mk_area(Win,0,0,W, std_butt_area_h()),    
    set_bline(L), set_bcanvas(C), set_bframe(F),

    MB = gs:create(menubar, Win, []),

    FMB = gs:create(menubutton, MB, [{label, {text, "File"}}]),
    FM = gs:create(menu, FMB, []),
    gs:create(menuitem, FM, [{label, {text, ?QUITTXT}}]),
    
    OMB = gs:create(menubutton, MB, [{label, {text, "Options"}}]),
    OM = gs:create(menu, OMB, []),
    G = now(),

    gs:create(menuitem, OM, 
	      [{label, {text, "List Box"}}, {data, {list_box}}]),
    
    gs:create(menuitem, OM, 
	      [{itemtype, separator}]),

    gs:create(menuitem, OM, 
	      [{label, {text, ?LOAD1TXT}}, {itemtype, radio},
	       {group, G}, {select, true},
	       {data, {load_opt, [{load_method, time}]}}]),
    gs:create(menuitem, OM, 
	      [{label, {text, ?LOAD2TXT}}, {itemtype, radio},
	       {group, G}, 
	       {data, {load_opt, [{load_method, queue}]}}]),
    
    G2 = now(),

    gs:create(menuitem, OM, 
	      [{label, {text, ?LOAD3TXT}}, {itemtype, radio}, {group, G2},
	       {select, true}, {data, {load_opt, [{load_scale, prog}]}}]),
    gs:create(menuitem, OM, 
	      [{label, {text, ?LOAD4TXT}}, {itemtype, radio}, {group, G2}, 
	       {data, {load_opt, [{load_scale, linear}]}}]),
    
    HMB = gs:create(menubutton, MB, [{label, {text, "Help"}}, {side, right}]),
    HM = gs:create(menu, HMB, []),
    gs:create(menuitem, HM, [{label, {text, ?HELPTXT}}]),
  
%%    mk_butt_row(Win, 10, ["Quit"]). %%, "Debug On", "Debug Off"]).
    true.


%%
%% Must return new Base Y, where node can begin to put appls
%% etc, and the line that marks the end of the node area
%%
new_node_area(State) ->
    FrameH = node_area_h()*State#state.node_count+node_area_h(),
    W = node_area_w(),
    H = node_area_h(),
    X = 10,
    Y = FrameH-node_area_h(),
    %%?D("FrameH: ~p Y: ~p~n", [FrameH, Y]),

    %% Now make windows bigger
    %%update_win_h(State),
    set_h(frame(), FrameH),
    set_h(canvas(), FrameH),
    set_h(meter_frame(), FrameH),

    %% Put a nice line at the end of the node area
    L = gs:create(line,canvas(),[{coords,[{0,FrameH-10}, {W-20,FrameH-10}]}]),
    Leds = new_meter(H, Y),

    {Y, L, Leds}.


new_meter(H, Y) ->
    UnitH = trunc((H-20)/(led_count())),
    Canvas = gs:create(canvas, meter_frame(), [{x,5}, {y,Y}, {bg, bg()},
					       {width, meter_area_w()}, 
					       {height,H}]),
    ListOfLeds = mk_leds(led_count(), Canvas, UnitH,  [Canvas]),
    leds_down(ListOfLeds, led_count(), 0),
    gs:create(text, Canvas, [{coords, [{0, UnitH*led_count()+5}]},
			     {anchor, nw}, 
			     {font, {screen, 8}}, 
			     {text, "Load"}]),
    gs:create(text, Canvas, [{coords, [{22, 0}]},
			     {anchor, nw}, 
			     {font, {screen, 8}}, 
			     {text, "Hi"}]),
    gs:create(text, Canvas, [{coords, [{22, UnitH*led_count()}]},
			     {anchor, w}, 
			     {font, {screen, 8}}, 
			     {text, "Lo"}]),
    ListOfLeds.

mk_leds(0, Canvas, UnitH, Leds) -> Leds;
mk_leds(N, Canvas, UnitH, Leds) ->
    L = mk_led(Canvas, (led_count()-N)*UnitH, UnitH),
    mk_leds(N-1, Canvas, UnitH, [L | Leds]).

mk_led(Canvas, Y,H) ->
    %%?D("Making led: ~p to ~p~n", [{0, Y}, {meter_area_w()-10, Y+H-2}]),
    gs:create(rectangle, Canvas, [{coords,[{0, Y},
					   {led_w(), Y+H}]}]).

display_load(Leds, Old, New) ->
    if
	Old == New ->
	    true;
	Old > New ->
	    leds_down(Leds, Old, New);
	true ->
	    leds_up(Leds, Old, New)
    end.

leds_down(Leds, Old, New) when Old == New -> 
    done;
leds_down(Leds, Old, New) when Old > New -> 
    reset_led(Leds, Old),
    leds_down(Leds, Old-1, New).
leds_up(Leds, Old, New) when Old == New -> 
    done;
leds_up(Leds, Old, New) when Old < New -> 
    set_led(Leds, Old),
    leds_up(Leds, Old+1, New).

%%col(6) -> yellow;
led_on_col(N) when N > 13 -> ?highloadfg;
led_on_col(N) when N > 9 -> ?midloadfg;
led_on_col(N) -> ?lowloadfg.

led_off_col(N) when N > 13 -> ?highloadbg;
led_off_col(N) when N > 9 -> ?midloadbg;
led_off_col(N) -> ?lowloadbg.

reset_led(Leds, 0) -> ok;
reset_led(Leds, N) ->
    gs:config(lists:nth(N, Leds), [{fill, led_off_col(N)}]).

set_led(Leds, 0) -> ok;
set_led(Leds, N) ->
    gs:config(lists:nth(N, Leds), [{fill, led_on_col(N)}]).
    


%%------------------------------------------------------------
%% Graphical utilities

resize(Nodes) ->
    Max		= get_max_w(Nodes),
    W		= Max+20,
    User	= get(user_resize),
    CurrW	= get_w(frame()),
    if  W <  ?MINW, CurrW /= ?MINW -> real_resize(Nodes, ?MINW);
	W <  ?MINW -> ok;
	W /= CurrW -> real_resize(Nodes, W);
	true       -> ok
    end.


%% Resize canvas and frame to width W and also resize the surrounding
%% window.
real_resize(Nodes, W) ->
    WW = W+meter_area_w(),
    set_w(win(), WW), set_w(bframe(), WW), set_w(bcanvas(), WW),
    resize_line(bline(), WW, 0),
    real_resize_area(Nodes, W).

%% Resizes canvas and fram, makes all lines longer
real_resize_area(Nodes, W) ->
    set_w(frame(), W),
    set_w(canvas(), W),
    resize_lines(Nodes, W, -20).

resize_lines([N | Ns], W, Offset) ->
    resize_line(N#gui_node.end_line, W, Offset),
    resize_lines(Ns, W, Offset);
resize_lines([], W, Offset) -> ok.

resize_line(L, W, Offset) ->
    [{X1, Y}, {X2, Y}] = gs:read(L, coords),
    set_coords(L, [{0, Y}, {W+Offset, Y}]).



%% find the maximum width (look for the app button farthest to the
%% right.
get_max_w([]) -> 0;
get_max_w([N | Ns]) when record(N, gui_node) ->
    M = find_app_x(N#gui_node.apps),
    get_max_w(Ns, M).

get_max_w([N | Ns], SoFar) when record(N, gui_node) ->
    M = find_app_x(N#gui_node.apps),
    if  M > SoFar -> get_max_w(Ns, M);
	true      -> get_max_w(Ns, SoFar)
    end;
get_max_w([], SoFar) -> SoFar.

set_coords(Win, Coords) ->
    gs:config(Win, [{coords,Coords}]).

%%
%% WARNING:
%% This thing assumes thatW and H is useless
%% due to a bug in gs when using menubuttons.
%% MUST BE CHANGED WHENEVER GS IS CHANGED
get_low(App) ->
    {X,Y,W,H} = get_xywh(App#gui_app.butt),
    %%{X+trunc(W/2), Y+H}. Use this when gs works
    {X+35, Y+master_butt_h()}.

get_high(App) ->
    {X,Y,W,H} = get_xywh(App#gui_app.butt),
    {X+trunc(W/2), Y}.

get_xywh(Win) ->
    X = gs:read(Win,x),
    Y = gs:read(Win,y),
    W = gs:read(Win,width),
    H = gs:read(Win,height),
    {X,Y,W,H}.

get_base_y(Node) -> 
    M = Node#gui_node.master,
    get_y(M#gui_app.butt).

set_h(Win,H) -> gs:config(Win,[{height,H}]).
%%get_h(Win) ->gs:read(Win,height).

set_w(Win,W) -> gs:config(Win,[{width,W}]).
get_w(A) when record(A,gui_app) -> get_w(A#gui_app.butt);
get_w(Win) ->gs:read(Win,width).

get_x(A) when record(A,gui_app) -> get_x(A#gui_app.butt);
get_x(Win) ->gs:read(Win,x).
set_x(Win,X) -> gs:config(Win,[{x,X}]).

get_y(A) when record(A,gui_app) -> get_y(A#gui_app.butt);
get_y(Win) ->gs:read(Win,y).
set_y(Win,Y) -> gs:config(Win,[{y,Y}]).


calc_coords(X1, Y1, X2, Y2 ) ->
    Y3 = trunc((Y1+Y2)/2),
    [{X1,Y1}, {X1,Y3}, {X2,Y3}, {X2,Y2}].
    
draw_line(Canvas,Coords) ->
    gs:create(line, Canvas, [{coords,Coords}]).


%%------------------------------
%% Button stuff

mk_butt(Parent,Txt, X, Y, H, Data) ->
    W = butt_width(Txt),
    gs:create(button, Parent,[{align,c},
			      {label,{text,Txt}},
			      {x,X}, 
			      {y,Y}, 
			      {width, W}, 
			      {height, H},
			      {data, Data},
			      {bg, bg()}]).

butt_width(Txt) when atom(Txt) ->
    butt_width( atom_to_list( Txt ));
butt_width(Txt) when list(Txt) ->
    case 8*length(Txt)+10 of
	X when X < 70 ->
	    70;
	X ->
	    X
    end.


destroy(undefined) -> ok;
destroy(Win) -> gs:destroy(Win).

move_y(App, DeltaY) when record(App,gui_app) ->
    move_y(App#gui_app.butt, DeltaY);
move_y(Win, DeltaY) ->
    set_y(Win, get_y(Win)+DeltaY).
move_x(App, DeltaX) when record(App,gui_app) ->
    move_x(App#gui_app.butt, DeltaX),
    
    %% SORT THE LIST OF COORDS
    [{X2, Y2},_,_,{X1,Y1}] = gs:read(App#gui_app.connection, coords),
    NewCoords = calc_coords(X1, Y1, X2+DeltaX, Y2),
    set_coords(App#gui_app.connection, NewCoords);
move_x(Win, DeltaX) ->
    set_x(Win, get_x(Win)+DeltaX).
move_line_y(Win, DeltaY) ->
    gs:config(Win, [{move,{0,DeltaY}}]).


update_win_h(State) ->
    H = node_area_h()*State#state.node_count+std_butt_area_h(),
    set_h(win(), H).


%%------------------------------------------------------------
%% Global Window info

winroot() ->		get(winroot).
win() ->		get(win).
frame() ->		get(frame).
canvas() ->		get(canvas).
set_meter_frame(X) ->	put(meter_frame, X).
set_winroot(X) ->	put(winroot, X).
set_win(X) ->		put(win, X).
set_frame(X) ->		put(frame, X).
set_canvas(X) ->	put(canvas, X).
set_line(X) ->		put(line, X).
meter_frame() ->	get(meter_frame).
set_bframe(X) ->	put(bframe, X).
set_bcanvas(X) ->	put(bcanvas, X).
set_bline(X) ->		put(bline, X).
bframe() ->		get(bframe).
bcanvas() ->		get(bcanvas).
bline() ->		get(bline).


master_butt_h() -> 30.

app_butt_h() -> 20.
app_space() -> 10.
app_space_h() -> 40.

node_area_h() -> 20+master_butt_h()+app_butt_h()+app_space_h(). 
node_area_w() -> get_w(frame()).

meter_area_w() -> 50.				% OTP-1358 (was 40 before)
led_count() -> 16.
led_w() -> 20.

std_butt_h() -> 30.
std_butt_area_h() -> std_butt_h()+30.

bg() -> ?lightsteelblue.
master_bg() -> ?black.
master_fg() -> ?antiquewhite.



%%% parse_nodes  /2
%%%
%%% parse_nodes returns a list with nodes to send to the listbox.
%%%

parse_nodes (Nodes, []) ->
    Nodes;

parse_nodes (Nodes, [#gui_node{node = Node, 
			       apps = Apps, 
			       cross = undefined} | T]) ->
    Apps_1 = parse_apps ([], Apps), 
    parse_nodes ([{Node, alive, Apps_1} | Nodes], T);

parse_nodes (Nodes, [#gui_node{node = Node, 
			       apps = Apps} | T]) ->
    Apps_1 = parse_apps ([], Apps), 
    parse_nodes ([{Node, dead, Apps_1} | Nodes], T).



%%% parse_apps  /2
%%%
%%% parse_apps returns a list with apllications to send to the listbox.
%%%

parse_apps (Apps, []) ->
    Apps;

parse_apps (Apps, [#gui_app{name = Name, pid = Pid} | T]) ->
    parse_apps ([{Pid, Name} | Apps], T).
