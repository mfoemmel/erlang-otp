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
%% A simple demo for choosing
%% colors in a window.
%% ------------------------------------------------------------

-module(color_demo).

-export([start/0,init/0]).

start() ->
    spawn(color_demo,init,[]).

init() ->
    I= gs:start(),
    W= gs:window(I,[{title,"Color Demo"},{width,300},{height,195}]), 
    B=gs:button(W,[{bitmap,"die_icon"},{x,271},{y,166},{width,30}]),
    gs:config(B,[{bg,yellow},{fg,hotpink1},{data,quit}]),
    gs:scale(W,[{text,"Red"},{y,0},{range,{0,255}},{orient,horizontal},
		{height,65},{data,red},{pos,42}]),
    gs:scale(W,[{text,"Blue"},{y,65},{range,{0,255}},{orient,horizontal},
		{height,65},{data,blue},{pos,42}]),
    gs:scale(W,[{text,"Green"},{y,130},{range,{0,255}},{orient,horizontal},
		{height,65},{data,green},{pos,42}]),
    gs:config(W,{map,true}),
    loop(W,0,0,0).

loop(W,R,G,B) ->
    gs:config(W,{bg,{R,G,B}}),
    receive
	{gs,_,click,red,[New_R|_]} ->
	    loop(W,New_R,G,B);
	{gs,_,click,green,[New_G|_]} ->
	    loop(W,R,New_G,B);
	{gs,_,click,blue,[New_B|_]} ->
	    loop(W,R,G,New_B);
	{gs,_,click,quit,_} ->
	    true;
	{gs,W,destroy,_,_} ->
	    true
    end.

%% ------------------------------------------------------------
