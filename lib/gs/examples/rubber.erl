%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%
%% ------------------------------------------------------------
%% A rubberbanding example in Erlang
%% ------------------------------------------------------------

-module(rubber).

-export([start/0, init/0]).

start() ->
    spawn(rubber,init,[]).

init() ->
    I=gs:start(),
    Win = gs:window(I,[{title,"Rubberbanding in Erlang"},
		      {width,300},{height,200}]),
    C= gs:canvas(Win,[{width,300},{height,200},{bg,green}]),
    gs:radiobutton(Win,[{width,100},{label,{text,"Oval"}},{align,w},
			{data,oval},{y,0},{select,true}]),
    gs:radiobutton(Win,[{width,100},{label,{text,"Rectangle"}},{align,w},
			{data,rectangle},{y,30}]),
    gs:radiobutton(Win,[{width,100},{label,{text,"Line"}},{align,w},
			{data,line},{y,60}]),
    gs:button(Win,[{label,{text,"Quit"}},{data,quit},{y,90}]),
    gs:config(Win,[{motion,true},{buttonpress,true},{buttonrelease,true}]),
    gs:config(Win,{map,true}),
    loop(C,nil,oval,0,0).


loop(Win,Obj,Objtype,X1,Y1) ->
    receive
	{gs,_,motion,_,[X2,Y2]} -> 
	    flush(Win,Obj,Objtype,X1,Y1,X2,Y2);
	{gs,_,buttonpress,_,[1,X2,Y2]} -> 
	    down(Win,Obj,Objtype,X2,Y2);
	{gs,_,buttonrelease,_,[1,X2,Y2]} -> 
	    up(Win,Obj,Objtype,X1,Y1,X2,Y2);
	{gs,_,click,quit,_} -> 
	    exit(normal);
	{gs,_,click,Newtype,_} -> 
	    loop(Win,Obj,Newtype,X1,Y1);
	{gs,_,destroy,_,_} -> 
	    exit(normal);
	Other -> 
	    io:format("Other:~w~n",[Other]),
	    loop(Win,Obj,Objtype,X1,Y1)
    end.


down(Win,nil,oval,X1,Y1) ->
    Obj = gs:create(oval,Win,[{coords,[{X1,Y1},{X1+1,Y1+1}]},{fill,red}]),
    loop(Win,Obj,oval,X1,Y1);
down(Win,nil,line,X1,Y1) ->
    Obj = gs:create(line,Win,[{coords,[{X1,Y1},{X1+1,Y1+1}]},{width,3},{fg,black}]),
    loop(Win,Obj,line,X1,Y1);
down(Win,nil,Objtype,X1,Y1) ->
    Obj = gs:create(Objtype,Win,[{coords,[{X1,Y1},{X1+1,Y1+1}]},{bw,2},{fg,blue}]),
    loop(Win,Obj,Objtype,X1,Y1);
down(Win,Obj,Objtype,X1,Y1) ->
    gs:destroy(Obj),
    down(Win,nil,Objtype,X1,Y1).

up(Win,nil,Objtype,_X1,_Y1,X2,Y2) ->
    loop(Win,nil,Objtype,X2,Y2);
up(Win,Obj,Objtype,X1,Y1,X2,Y2) ->
    gs:config(Obj,{coords,[{X1,Y1},{X2,Y2}]}),
    loop(Win,nil,Objtype,X2,Y2).


move(Win,nil,Objtype,X1,Y1,_X2,_Y2) ->
    loop(Win,nil,Objtype,X1,Y1);
move(Win,Obj,Objtype,X1,Y1,X2,Y2) ->
    gs:config(Obj,{coords,[{X1,Y1},{X2,Y2}]}),
    loop(Win,Obj,Objtype,X1,Y1).


flush(Win,Obj,Objtype,X1,Y1,X2,Y2) ->
    receive
        {gs,_,motion,_,[XX2,YY2]} -> 
            flush(Win,Obj,Objtype,X1,Y1,XX2,YY2)
    after
        0 -> move(Win,Obj,Objtype,X1,Y1,X2,Y2)
    end.

%% ------------------------------------------------------------
