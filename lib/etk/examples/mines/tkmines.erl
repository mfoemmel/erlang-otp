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
-module(tkmines).

-import(lists, [flatten/1,map/2, member/2,foldl/3]).

-define(WSZ, 300).  %% win size
-define(BSZ, 9).    %% board size

-export([start/0,init/0]).

%% One of these for each square on the board
-record(sq, {
	  cc,           %% coords
	  mine = false, %% true | false
	  text = nil,   %% possible text
	  blank = nil,   %% possible blank
	  ball = nil,   %% possible ball
	  cross = nil}).%% possible cross

-record(board, {
	  canvas,
	  count = 0,  %% number of filled squares
	  arr = make_board()}). %% the array

start() -> spawn(tkmines, init, []).


menus() ->
    {menubar,
     [{menu, "Game", "left", 
       [but(start, "Start", -1, ""),
	but({control, easy, 1}, "Easy", -1, ""),
	but({control, medium, 2}, "Medium", -1, ""),
	but({control, hard, 3}, "Hard", -1, ""),
	but(explode, "Explode", -1, ""),
	but(exit, "Exit", -1, "")]}]}.


but(Tag, Text, U, A) ->
    Self = self(),
    F = fun() ->Self ! {menu, Tag} end,
    {button, [{label, Text},{underline, U},
              {func, F}, {accelerator,A}]}.


init() ->
    etk:start(),
    put(toplevel, W = etk:toplevel([])),
    tk:wm([geometry, W, "300x300+100+200"]),
    tk:wm([title, W, "TkMines"]),
    MenuBar = etk_menu:create_menubar(W, menus()),
    tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    F1 = etk:frame(W, [ {borderwidth, 1}, {relief, ridge}]),
    tk:pack(F1, [{anchor, "nw"}, {fill, "both"}, {expand, yes}]),
    C = etk:canvas(F1, []),
    tk:pack(C, [{anchor, "nw"}, {fill, "both"}, {expand, yes}]),

    Self = self(),
    lists:foreach(fun({BP, I}) ->
		    tk:bind(C, BP, [ '%x', '%y'], 
			    fun(X, Y) ->
				    Self ! {button_press, I, X, Y}
			    end)
	    end,
	    [{"<ButtonPress-1>", 1},	     
	     {"<ButtonPress-2>", 2},	
	     {"<ButtonPress-3>", 3}]),

    draw_lines(C, 0),
    put(difficulty, 2),
    process_flag(trap_exit, true),
    loop(#board{canvas = C}, idle).

uu() ->
    receive x ->
	     ok
    end.


make_board() ->
    {X,Y,Z} = now(),
    random:seed(X,Y,Z),
    make_board(?BSZ, []).
make_board(I, Lines) when I < 0 ->
    list_to_tuple(Lines);
make_board(Lno, Lines) ->
    make_board(Lno-1, [make_line(Lno, ?BSZ, []) | Lines]).

make_line(_, I, Ack) when I < 0 ->
    list_to_tuple(Ack);
make_line(Lno, I, Ack) ->
    R = random:uniform(10),
    Sq = #sq{cc = {Lno, I},  
	     mine = (R =< get(difficulty))},
    make_line(Lno, I-1, [Sq|Ack]).


get_sq({X, Y}, Board) ->
    Line = element(X+1, Board#board.arr),
    element(Y+1, Line).

set_sq(Sq, Board) ->
    {X,Y} = Sq#sq.cc,
    Arr = Board#board.arr,
    Line = element(X+1, Arr),
    Line2 = setelement(Y+1, Line, Sq),
    A2 = setelement(X+1, Arr, Line2),
    Board#board{arr = A2}.

draw_lines(Can, X) when X > ?WSZ ->
    tk:cmd(Can, [create, line, 0, 0 , ?WSZ, 0]);
draw_lines(Can, I) ->
    tk:cmd(Can, [create, line, I, 0, I, ?WSZ]),
    tk:cmd(Can, [create, line, 0, I, ?WSZ, I]),
    draw_lines(Can, I+30).

incr(Board) ->
    Board#board{count = 1 + Board#board.count}.

decr(Board) ->
    Board#board{count = Board#board.count - 1}.

%% Set a red ball an return new board
draw_ball(Board, {X, Y}, Colour) ->
    Sq = get_sq({X, Y}, Board),
    Red = tk:cmd(Board#board.canvas, [create, oval, (X * 30) + 5,
			       (Y * 30) + 5,
			       (X * 30) + 25,
			       (Y * 30) + 25,  {fill, Colour}]),
    incr(set_sq(Sq#sq{ball = Red}, Board)).
    

unzip([{X,Y}|T]) ->
    [X, Y | unzip(T)];
unzip([]) ->
    [].
	   
%% Draw a cross and return a new board   
draw_cross(Board, Coord) ->
    {X0, Y0} = Coord,
    Coords = unzip(map(fun({X, Y}) -> {(30*X0) + X, 30*Y0 + Y} end,
		       [{12,2}, {18,2},
			{18, 12}, {28,12},
			{28,18}, {18,18},
			{18, 28}, {12, 28},
			{12, 18}, {2, 18},
			{2,12}, {12, 12}])),
    P = tk:cmd(Board#board.canvas, [create, polygon] ++ Coords ++ [{fill, black}]),
    Sq = get_sq(Coord, Board),
    Sq2 = Sq#sq{cross = P},
    incr(set_sq(Sq2, Board)).

draw_blank(Coord, Board) ->
    {X, Y} = Coord,
    Coords = [{X*30, Y*30},
	      {(X+1) * 30, Y*30},
	      {(X+1) * 30, (Y+1) * 30},
	      {X*30, (Y+1) * 30}],
    P = tk:cmd(Board#board.canvas, 
	       [create, rectangle, X*30, Y*30, (X+1) * 30, (Y+1) * 30,
			     {fill, green}]),
    
    Sq = get_sq(Coord, Board),
    Sq2 = Sq#sq{blank = P},
    incr(set_sq(Sq2, Board)).


%% handle right button and return a new board
cross(Board, Coord) ->
    Sq =  get_sq(Coord, Board),
    case has_graphic_object(Sq) of
	cross ->
	    decr(destroy(Sq, Board));
	false ->
	    draw_cross(Board, Coord);
	_ ->
	    Board
    end.


%% destro graphics obj in Sq and return new Sq
destroy(Sq, B) ->
    Sq2 = 
	if
	    Sq#sq.text /= nil ->
		tk:cmd(B#board.canvas, [delete, Sq#sq.text]),
		Sq#sq{text = nil};
	    Sq#sq.ball /= nil ->
		tk:cmd(B#board.canvas, [delete, Sq#sq.ball]),
		Sq#sq{ball = nil};
	    Sq#sq.cross /= nil ->
		tk:cmd(B#board.canvas, [delete, Sq#sq.cross]),
		Sq#sq{cross = nil};
	    Sq#sq.blank /= nil ->
		tk:cmd(B#board.canvas, [delete, Sq#sq.blank]),
		Sq#sq{blank = nil};
	    true ->
		Sq
	end,
    set_sq(Sq2, B).
    

has_graphic_object(Sq) ->
    if
	Sq#sq.text /= nil -> text;
	Sq#sq.ball /= nil -> ball;
	Sq#sq.cross /= nil -> cross;
	Sq#sq.blank /= nil -> blank;
	true -> false
    end.

%% handle button 1 and return a new board
down(Board, Coord) ->
    Sq = get_sq(Coord, Board),
    if
	Sq#sq.mine == true ->
	    explode(Board, red);
	true ->
	    case has_graphic_object(Sq) of
		false ->
		    draw_number(Coord, Board);
		_ ->
		    Board
	    end
    end.

draw_number(Coord, Board) ->
    Ns = neighbours(Coord),
    Num = count_mines(Board, Ns),
    print_number(get_sq(Coord, Board), Board, Num).

%% special case, fill greens
print_number(Sq, Board, 0) ->
    {Blanks, Borders} = get_blanks(Sq, Board),
    B2 = foldl(fun(N, Ack) -> draw_number(N, Ack) end, 
	       Board, Borders),
    foldl(fun(N, Ack) -> draw_blank(N, Ack) end,
	  B2, Blanks);
    
print_number(Sq, Board, Num) ->
    Str = flatten(io_lib:format("~p", [Num])),
    {X, Y} = Sq#sq.cc,
    T = tk:cmd(Board#board.canvas, 
	       [create, text, (X*30) + 10, (Y*30) + 8, {text, Str}]),
    Sq2 = Sq#sq{text = T},
    incr(set_sq(Sq2, Board)).
	

to_pix({X, Y}) ->
    {(X*30) + 10, (Y*30) + 8}.


%% Return all {Blanks, Borders} originating at Sq
get_blanks(Sq, Board) ->    
    get_blanks(Board, neighbours(Sq#sq.cc), [Sq#sq.cc], []).

get_blanks(Board, [], Blanks, Borders) ->
    {Blanks, Borders};
get_blanks(Board, [H|T], Blanks, Borders) ->
    case {member(H, Blanks), member(H, Borders)} of
	{false, false} ->
	    case {has_graphic_object(get_sq(H, Board)),
		  count_mines(Board, neighbours(H))} of
		{false, 0} ->
		    get_blanks(Board, join(T, neighbours(H)),
			       [H|Blanks], Borders);
		{false, I} ->
		    get_blanks(Board, T, Blanks, [H|Borders]);
		{_, _} ->
		    get_blanks(Board, T, Blanks, Borders)
	    end;
	_ ->
	    get_blanks(Board, T, Blanks, Borders)
    end.

join(L1, L2) ->
    (L1 -- L2)  ++ L2.


neighbours({X, Y}) ->
    [{X1, Y1} || X1 <- [X-1, X, X+1] -- [-1,10], 
		     Y1 <- [Y-1, Y, Y+1] -- [-1,10]].


count_mines(Board, Neighbours) ->
    foldl(fun(N, Ack) ->
		 Sq =  get_sq(N, Board),
		  if
		      Sq#sq.mine == true -> 
			  1 + Ack;
		      Sq#sq.mine == false ->
			  Ack
		  end
	  end, 0, Neighbours).

loop(Board, S) when S == run, Board#board.count == 100 ->
    explode(Board, blue);

loop(Board, S) ->
    receive
	{button_press, 1, X0, Y0} when S == run ->
	    loop(down(Board, {X0 div 30, Y0 div 30}), S);
	{button_press, _, X0, Y0} when S == run ->
	    loop(cross(Board, {X0 div 30, Y0 div 30}), S);
	{menu, {control, C, Diff}} ->
	    put(difficulty, Diff), 
	    loop(Board, S);
	{menu, start} ->
	    clear(Board),
	    loop(#board{canvas = Board#board.canvas}, run);
	{menu, explode} ->
	    explode(Board, red);
	{menu, exit} ->
	    tk:destroy(get(toplevel)),
	    exit(normal);
	X ->
	    io:format("Got ~p~n", [X]),
	    loop(Board, S)
    end.

explode(Board, Colour) ->
    B2 = traverse_board(Board, 
			fun(B,Sq) when Sq#sq.mine == true ->
				B2 = destroy(Sq, B),
				draw_ball(B2, Sq#sq.cc, Colour);
			   (B, Sq) ->
				B
			end),
    loop(B2, idle).

clear(Board) ->
     traverse_board(Board, 
		    fun(B, Sq) -> destroy(Sq, B) end).

%% Traverse the board, apply Fun and return
%% a new board
traverse_board(Board, Fun) ->
    traverse_board(Board, ?BSZ, ?BSZ, Fun).

traverse_board(Board, -1, _, _) ->
    Board;
traverse_board(Board, I, -1, Fun) ->
    traverse_board(Board, I-1, ?BSZ, Fun);
traverse_board(Board, I, J, Fun) ->
    B2 = Fun(Board, get_sq({I, J}, Board)),
    traverse_board(B2, I, J-1, Fun).


