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
%% ------------------------------------------------------------
%%  Focus Demo
%% ------------------------------------------------------------

-module(focus_demo).

-export([start/0,init/0]).


%% ----- File Selection ----
start() ->
    spawn(focus_demo,init,[]).


init() ->
    S=gs:start(),
    Win=gs:window(S,[{title,"Focus Demo"},{width,200},{height,150}]),
    gs:create(entry,e1,Win,[{focus,true}]),
    gs:create(entry,e2,Win,[{y,30},{focus,true}]),
    gs:create(entry,e3,Win,[{y,60},{focus,true}]),
    gs:create(entry,e4,Win,[{y,90},{focus,true}]),
    gs:create(button,b1,Win,[{x,100},{width,40},{label,{text,"e1"}}]),
    gs:create(button,b2,Win,[{y,30},{x,100},{width,40},{label,{text,"e2"}}]),
    gs:create(button,b3,Win,[{y,60},{x,100},{width,40},{label,{text,"e3"}}]),
    gs:create(button,b4,Win,[{y,90},{x,100},{width,40},{label,{text,"e4"}}]),
    gs:create(button,clear,Win,[{y,120},{x,100},{width,40},{label,{text,"Clear"}}]),
    gs:create(button,ask,Win,[{y,120},{x,140},{width,30},{label,{text,"?"}}]),
    gs:create(button,quit,Win,[{y,120},{x,170},{width,30},{bg,yellow},
			       {label,{image,"die_icon"}}]),
    gs:config(Win,{map,true}),
    loop().

loop() ->
    receive
	{gs,quit,_,_,_} -> exit(normal);
	{gs,ask,_,_,_} -> 
	    R1=gs:read(e1,setfocus),R2=gs:read(e2,setfocus),
	    R3=gs:read(e3,setfocus),R4=gs:read(e4,setfocus),
	    R= if R1==true -> e1; 
		   R2==true -> e2;
		   R3==true -> e3;
		   R4==true -> e4;
		   true -> nobody
	       end,
	    io:format("Focus status: ~w has focus.~n",[R]);
	{gs,clear,_,_,_} -> 
	    gs:config(e1,{setfocus,false}), 
	    gs:config(e2,{setfocus,false}), 
	    gs:config(e3,{setfocus,false}), 
	    gs:config(e4,{setfocus,false}),
	    io:format("Focus is cleared.~n",[]);
	{gs,b1,_,_,_} -> gs:config(e1,{setfocus,true});
	{gs,b2,_,_,_} -> gs:config(e2,{setfocus,true});
	{gs,b3,_,_,_} -> gs:config(e3,{setfocus,true});
	{gs,b4,_,_,_} -> gs:config(e4,{setfocus,true});
	{gs,Id,focus,_,[0|_]} -> 
	    io:format("~w lost focus.~n",[Id]);
	{gs,Id,focus,_,[1|_]} -> 
	    io:format("~w gained focus.~n",[Id]);
	{gs,_,destroy,_,_} -> 
	    exit(normal);
	X ->
	    io:format("Got X=~w~n",[X])
    end,
    loop().

%% ----------------------------------------
%% done
