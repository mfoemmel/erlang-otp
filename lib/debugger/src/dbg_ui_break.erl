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
%%% Purpose: Handle graphical entry of breaks for the debugger

-module(dbg_ui_break).

-export([start/3,init/5]).

%%% start(Type,Data,Coord) -> Pid
%%% Type: normal|conditional|functional
%%% Data: {ModuleStr,LineStr} Possible values to fill in the entry boxes
%%% Coord: {X,Y} Positon where to place the window

start(Type,Data,Coord) ->
    spawn_link(?MODULE,init,[Type,Data,Coord,self(),
				 gs:start([{kernel,true}])]).

%%% Create the normal break Window

init(normal,{Module,Line},{X,Y},Parent,Gs) ->
    Win=gs:window(win,Gs,[{title,"Line Break Dialog"},{width,250},{height,180},
			  {configure,true},{x,X},{y,Y},{destroy,true}]),
    field(10,f1,"Module:",Module,true,Win),
    field(40,f2,"Line:",Line,false,Win),
    events(Win,80),
    buttons(Win,80),
    gs:config(Win,{map,true}),
    loop(normal,{250,130},Parent,enable);

%%% create the conditional break Window

init(conditional,{Mod,Line},{X,Y},Parent,Gs) ->
    Title = "Conditional Break Dialog",
    Win=gs:window(win,Gs,[{title,Title},{width,250},{height,240},
			  {configure,true},{x,X},{y,Y},{destroy,true}]),
    field(10,f1,"Module:",Mod,true,Win),
    field(40,f2,"Line:",Line,false,Win),
    field(70,f3,"C-Module:","",false,Win),
    field(100,f4,"C-function:","",false,Win),
    buttons(Win,140),
    events(Win,140),
    gs:config(Win,{map,true}),
    loop(conditional,{250,240},Parent,enable);

%%% Create the functional break Window

init(functional,Mod,{X,Y},Parent,Gs) ->
    Win=gs:window(win,Gs,[{title,"Function Break Dialog"},{width,250},{height,210},
			  {configure,true},{x,X},{y,Y},{destroy,true}]),
    field(10,f1,"Module:",Mod,true,Win),
    field(40,f2,"Function:","",false,Win),
    field(70,f3,"Arity:","",false,Win),
    buttons(Win,110),
    gs:config(Win,{map,true}),
    loop(functional,{250,210},Parent,enable).

%%% Create an entry box.

field(Y,Name,Label,Init,Focus,Win) ->
    gs:create(label,Win,[{x,83},{y,Y},{width,90},{label,{text,Label}},
			 {anchor,ne},{align,e}]),
    gs:create(entry,Name,Win,[{x,83},{y,Y},{width,150},{keypress,true},
			      {text,Init},{setfocus,Focus},{anchor,nw}]).

%%% Handle all events

events(Win,Y) ->
    F = gs:frame(Win,[{bg,black},{x,3},{y,Y-4},{width,103},{height,89},
		 {anchor,nw}]),
    Id = element(1,Win),  %%Tuples not allowed as group ids
    gs:label(F,[{label,{text,"Trigger Action"}},{x,1},{y,1},{height,20},
	       {width,101}]),
    gs:radiobutton(enable,F,[{label,{text,enable}},{y,23},{x,1},
			       {select,true},{data,{cond,enable}},{height,22},
			       {group,Id},{anchor,nw},{align,w},{width,101}]),
    gs:radiobutton(disable,F,[{label,{text,disable}},{y,45},
				{x,1},{data,{cond,disable}},{height,22},
				{group,Id},{anchor,nw},{align,w},{width,101}]),
    gs:radiobutton(delete,F,[{label,{text,delete}},{y,66},
			       {x,1},{data,{cond,delete}},{height,22},
			       {group,Id},{anchor,nw},{align,w},{width,101}]).

%%% Create the buttons.

buttons(Win,Y) ->
    gs:button(ok,Win,[{width,80},{y,Y},{x,213},{label,{text,"Ok"}},
		      {anchor,ne}]),
    gs:button(cancel,Win,[{width,80},{y,Y+55},{x,213},{label,{text,"Cancel"}},
			  {anchor,ne}]).

loop(Type,Size,Parent,Action) ->
    receive
	{gs,f1,keypress,_,['Return'|_]} ->
	    gs:config(f2,{setfocus,true}),
	    loop(Type,Size,Parent,Action);
	{gs,f2,keypress,_,['Return'|_]} ->
	    case Type of
		normal ->
		    execute_input(Type,Size,Parent,Action);
		_ ->
		    gs:config(f3,{setfocus,true}),
		    loop(Type,Size,Parent,Action)
	    end;
	{gs,f3,keypress,_,['Return'|_]} ->
	    case Type of
		functional ->
		    execute_input(Type,Size,Parent,Action);
		_ ->
		    gs:config(f4,{setfocus,true}),
		    loop(Type,Size,Parent,Action)
	    end;
	{gs,f4,keypress,_,['Return'|_]} ->
	    execute_input(Type,Size,Parent,Action);
	{gs,ok,click,_,_} ->
	  execute_input(Type,Size,Parent,Action);   %%exit or loop/3
	{gs,cancel,click,_,_} ->
	    exit(normal);
	{gs,win,configure,_,[Dx,Dy|_]} ->
	    resize({Dx,Dy},Size,Type),
	    loop(Type,{Dx,Dy},Parent,Action);
	{gs,_,click,{cond,What},_} ->
	    loop(Type,Size,Parent,What);
	{gs,_,keypress,[],[p,112,0,1]} ->
	    loop(Type,Size,Parent,previous(Action));
	{gs,_,keypress,[],[n,110,0,1]} ->
	    loop(Type,Size,Parent,next(Action));
	{gs,_,keypress,[],[c,99,0,1]} ->
	    ok;
	{gs,_,destroy,_,_} -> exit(normal);
	Other ->
	    loop(Type,Size,Parent,Action)
    end.

next(enable) -> gs:config(disable,{select,true}),
		disable;
next(disable) -> gs:config(delete,{select,true}),
		 delete;
next(delete) -> gs:config(enable,{select,true}),
		enable.
    
previous(enable) -> gs:config(delete,{select,true}),
		    delete;
previous(disable) -> gs:config(enable,{select,true}),
		     enable;
previous(delete) -> gs:config(disable,{select,true}),
		    disable.


execute_input(Type,Size,Parent,Action) ->
    case catch execute(Type,Action) of
	ok-> Parent!new_break,exit(normal);
	Error ->
	    X = gs:read(win,x),
	    Y = gs:read(win,y),
	    Str = "Error:Bad Input.",
	    dbg_ui_aux:message_window(Str,Parent,{X,Y}),
	    loop(Type,Size,Parent,Action)
    end.

execute(Type,Action) ->
    Mod = gs:read(f1,text),
    case Type of
	normal ->
	    Line =  gs:read(f2,text),
	    [N_m,N_l] = parse([{atom,Mod},{int,Line}]),
	    int:break(N_m,N_l),
	    int:action_at_break(N_m,N_l,Action);
	conditional ->
	    Line =  gs:read(f2,text),
	    C_mod = gs:read(f3,text),
	    C_fun = gs:read(f4,text),
	    [N_m,N_l,C_m,C_f] = parse([{atom,Mod},{int,Line},{atom,C_mod},
				       {atom,C_fun}]),
	    int:break(N_m,N_l),
	    int:action_at_break(N_m,N_l,Action),
	    int:test_at_break(N_m,N_l,{C_m,C_f});
	functional ->
	    Fun =  gs:read(f2,text),
	    Arity =  gs:read(f3,text),
	    [N_m,N_f,N_a] = parse([{atom,Mod},{atom,Fun},{int,Arity}]),
	    int:break_in(N_m,N_f,N_a)
    end. 

resize(Size,Size,_) -> ok;
resize({Dx,Dy},_,Type) ->
    gs:config(f1,{width,Dx-100}),
    gs:config(f2,{width,Dx-100}),
    case Type of
	normal      -> ok;
	functional  -> gs:config(f3,{width,Dx-100});
	conditional -> gs:config(f3,{width,Dx-100}),
		       gs:config(f4,{width,Dx-100})
    end.
			  

parse({_,       []})      -> throw({error,empty});
parse({int, String})      -> list_to_integer(String);
parse({atom,String})      -> list_to_atom(String);
parse([])                 -> [];
parse([Element|Elements]) -> [parse(Element)|parse(Elements)].



