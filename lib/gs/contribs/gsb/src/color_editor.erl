%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	color_editor.erl
%%  Module   :	color_editor
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-06 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(color_editor).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.9 $').
-export([]).
-record(loop, {rgb, atomcolor, state, gspid}).

-export([edit_color/2, edit_color/3]).

edit_color({to_list, Value}, List) when atom(Value) ->
  case lists:member(Value, List) of
    true ->
      atom_to_list(Value);
    false ->
      undefined
  end;
edit_color({to_list, Value}, List) when tuple(Value) ->
  lists:flatten(io_lib:format("~w", [Value]));  
edit_color({to_list, Value}, List) ->
  undefined;
edit_color({to_erlang, List}, ListOfColors) ->
  Value = parse(List),
  case is_atom(Value) of
    true ->
      case lists:member(Value, ListOfColors) of
	true -> Value;
	_else -> undefined
      end;
    _else ->
      case is_tuple(Value) of
	true ->
	  case check_for_integer(Value, 3) of
	    true ->Value;
	    _else -> undefined
	  end;
	_else ->
	  undefined
      end
  end.

check_for_integer(Term, 0) -> true;
check_for_integer(Term, N) ->
  case is_integer(element(N, Term)) of
    true ->
      check_for_integer(Term, N -1);
    false ->
      false
  end.


is_integer(A) when integer(A) ->
  true;
is_integer(_) ->
  false.

is_atom(V) when atom(V) -> true;
is_atom(_) -> false.

is_tuple(V) when tuple(V) -> true;
is_tuple(_) -> false.

parse(String) ->
  case erl_scan:string(lists:append([String, ". "])) of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
	{ok, Term} ->
	  Term;
	{error, _} -> undefined
      end;
    {error, _} -> undefined
  end.

edit_color({change, {R,G,B}}, [FirstC|Rest], {GS, X, Y}) ->
  I= gsb_run:start(GS,color_editor),
  gs:config(window, [{x, X}, {y, Y}]),
  gs:config(lb, [{items, ["{R,G,B}",FirstC|Rest]}, {selection, 0}]),
  gs:scale(red, window,[{text,"Red"},{y,140}, {x, 10},{range,{0,255}},
			{orient,horizontal}, {click, true}, 
			{fg, blue}, {height,60}, {width, 250},
			{data,red},{pos,R}]),
  gs:scale(green, window,[{text,"Green"},{y,200},{x, 10}, {range,{0,255}},
			  {orient,horizontal}, {click, true},
			  {fg, blue}, {height,60}, {width, 250},
			  {data,green},{pos,G}]),
  gs:scale(blue, window,[{text,"Blue"},{y,260}, {x, 10}, {range,{0,255}},
			 {orient,horizontal}, {click, true},
			 {fg, blue}, {height,60}, {width, 250},
			 {data,blue},{pos,B}]),
  gs:config(window,{map,true}),
  loop(#loop{rgb = {R,G,B}, atomcolor = FirstC, state = tuple, gspid = GS});

edit_color({change, Value}, List, {GS, X, Y}) ->
  I= gsb_run:start(GS,color_editor),
  N = search(Value, List, 0),
  gs:config(window, [{x, X}, {y, Y}]),
  gs:config(lb, [{items, ["{R,G,B}"|List]}, {selection, N + 1}]),
  gs:scale(red, window,[{text,"Red"},{y,140}, {x, 10},{range,{0,255}},
			{orient,horizontal}, {click, true}, 
			{fg, blue}, {height,60}, {width, 250},
			{data,red},{pos,0}, {enable, false}]),
  gs:scale(green, window,[{text,"Green"},{y,200},{x, 10}, {range,{0,255}},
			  {orient,horizontal}, {click, true},
			  {fg, blue}, {height,60}, {width, 250},
			  {data,green},{pos,0}, {enable, false}]),
  gs:scale(blue, window,[{text,"Blue"},{y,260}, {x, 10}, {range,{0,255}},
			 {orient,horizontal}, {click, true},
			 {fg, blue}, {height,60}, {width, 250},
			 {data,blue},{pos,0}, {enable, false}]),
  gs:config(window,{map,true}),
  loop(#loop{rgb = {0,0,0}, atomcolor = Value, state = atom, gspid = GS}).

loop(Record) ->
  case Record#loop.state of
    atom ->
      gs:config(test,{bg,Record#loop.atomcolor}),
      receive 
	{gs, lb, click, _, [I, "{R,G,B}", _]} ->
	  gs:config(red, {enable, true}),
	  gs:config(green, {enable, true}),
	  gs:config(blue, {enable, true}),
	  loop(Record#loop{state = tuple});
	{gs, lb, click, _, [I, Color, _]} ->
	  loop(Record#loop{atomcolor = list_to_atom(Color), state = atom});
	{gs,_,click,red,[New_R|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {New_R,G,B}});
	{gs,_,click,green,[New_G|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {R,New_G,B}});
	{gs,_,click,blue,[New_B|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {R,G,New_B}});
	{gs, ok, click, _, _} ->
	  stop(Record#loop.gspid),
	  {Record#loop.atomcolor, atom_to_list(Record#loop.atomcolor)};
	Other ->
	  case rec_common(Other, Record) of
	    undefined -> undefined;
	    NewRecord -> loop(NewRecord)
	  end
      end;
    tuple ->
      gs:config(test, {bg, Record#loop.rgb}),
      receive
	{gs,_,click,red,[New_R|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {New_R,G,B}});
	{gs,_,click,green,[New_G|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {R,New_G,B}});
	{gs,_,click,blue,[New_B|_]} ->
	  {R,G,B} = Record#loop.rgb,
	  loop(Record#loop{rgb = {R,G,New_B}});
	{gs, lb, click, _, [I, "{R,G,B}", _]} ->
	  loop(Record);
	{gs, lb, click, _, [I, Color, _]} ->
  	  gs:config(red, {enable, false}),
	  gs:config(green, {enable, false}),
	  gs:config(blue, {enable, false}),
	  loop(Record#loop{atomcolor = list_to_atom(Color), state = atom});
	{gs, ok, click, _, _} ->
	  stop(Record#loop.gspid),
	  Tuple = Record#loop.rgb,
	  {Record#loop.rgb, io_lib:format("~w", [Tuple])};
	Other ->
	  case rec_common(Other, Record) of
	    undefined -> undefined;
	    NewRecord -> loop(NewRecord)
	  end
      end
  end.

rec_common(Other, Record) ->
  case Other of
    {gs,_,click,quit,_} ->
      stop(Record#loop.gspid),
      undefined;
    {gs,_,destroy,_,_} ->
      stop(Record#loop.gspid),
      undefined;
    {gs, cancel, click, _, _} ->
      stop(Record#loop.gspid),
      undefined;
    {gs, _, click, _, _} ->
      Record;
    Unknown ->
      io:format("color_editor: ~p~n", [Other]),
      Record
  end.

stop(Gspid) ->
  gs:destroy(window).

  
search(_Value, [], N) -> N;
search(Value, [Value|_Rest], N) -> N;
search(Value, [_Other|Rest], N) ->
  search(Value, Rest, N + 1).
