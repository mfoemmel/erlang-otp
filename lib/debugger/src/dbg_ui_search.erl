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
%%% Purpose : To search for text strings in an editor.
%%% Comments: The owner of a gs object is the only process that is allowed
%%%           to manipulate the object. 
%%%           That's the reason of the process calls back and forth.
%%%----------------------------------------------------------------------


-module(dbg_ui_search).


-export ([
	  start/3,            
	  unmark_string/2,
	  editor_search/5
	 ]).

-export ([init/3]).   % Internal export


-define (BUTT_W, 60).     % Button with
-define (WIN_W, 300).     % Windows width
-define (WIN_H, 150).     % Windows height



%%% start  /3
%%%
%%% start spawns the init function with the given editor to search in.
%%%
%%% Pre:
%%%    ParentPid  ==  pid ()
%%%    Editor     ==  gs object
%%%    ParentWin  ==  gs object
%%%

start (ParentPid, Editor, ParentWin) ->
    spawn_link (?MODULE, init, [ParentPid, Editor, ParentWin]).



%%% init  /3
%%%
%%% init calls the win_init function to create the search window
%%% and then enters the receive loop.
%%%
%%% Pre:
%%%    ParentPid  ==  pid ()
%%%    Editor     ==  gs object
%%%    ParentWin  ==  gs object
%%%

init (ParentPid, Editor, ParentWin) ->
    win_init (ParentWin),
    loop (ParentPid, Editor, {1, 0}, false).



%%% loop  /4
%%%
%%% Pos is the position of the begining of the found string.
%%% E.g. the position of 'f' in "file" is equal to Pos.
%%%
%%% Pre:
%%%    Parent   ==  pid ()
%%%    Editor   ==  gs object
%%%    Pos      ==  {integer (), integer ()}
%%%                 {Row, Col}
%%%    CaseSen  ==  bool ()
%%%                 case sensitive
%%%

loop (Parent, Editor, Pos, CaseSen) ->

    %% Pos1 below is the position to begin 
    %% the next search from.
    %%

    receive
	{gs, _button, click, _, ["Search"]} ->
	    gs:config (search_win, [{cursor, busy}]),
	    String = gs:read (search_entry, text),
	    {Row, Col} = Pos,
	    Pos1 = {Row, Col + string:len (String)},
	    NewPos = search_action (String, CaseSen, Parent, Pos1),
	    gs:config (search_win, [{cursor, arrow}]),
	    loop (Parent, Editor, NewPos, CaseSen);


	{gs, search_entry, keypress, _, ['Return' | _]} ->
	    gs:config (search_win, [{cursor, busy}]),
	    gs:config (search_button, flash),
	    String = gs:read (search_entry, text),
	    {Row, Col} = Pos,
	    Pos1 = {Row, Col + string:len (String)},
	    NewPos = search_action (String, CaseSen, Parent, Pos1),
	    gs:config (search_win, [{cursor, arrow}]),
	    loop (Parent, Editor, NewPos,  CaseSen);


	{gs, search_entry, keypress, _, Key} ->
	    gs:config (search_win, [{cursor, busy}]),
	    String = gs:read (search_entry, text),
	    {Row, Col} = Pos,
	    Pos1 = {Row, Col - string:len (String)},
	    NewPos = search_action (String, CaseSen, Parent, Pos1),
	    gs:config (search_win, [{cursor, arrow}]),
	    loop (Parent, Editor, NewPos, CaseSen);


	{gs, _button, click, _, ["Clear"]} ->
	    Parent ! {unmark_string, Editor, element (1, Pos)},
	    gs:config (search_entry, {delete, {0, last}}),
	    write_info (""),
	    loop (Parent, Editor, {1, 0}, CaseSen);


	{gs, _button, click, _, ["Close"]} ->
	    Parent ! {unmark_string, Editor, element (1, Pos)},
	    exit (normal);


	%% cs_cb == Case Sensitive Check Button
	%%

	{gs, cs_cb, click, _, [_, _, Mode]} ->
	    loop (Parent, Editor, Pos, Mode);


	{gs, search_win, configure, _, [Width, Height | _]} ->
	    win_configure (Width, Height),
	    loop (Parent, Editor, Pos, CaseSen);


	{gs, search_win, destroy, _, _} ->
	    Parent ! {unmark_string, Editor, element (1, Pos)},
	    exit (normal);


	Other ->
	    loop (Parent, Editor, Pos, CaseSen)
    end.



%%% win_init  /1
%%%
%%% win_init creates the search window.
%%%
%%% Pre:
%%%    ParentWin  ==  gs object
%%%                   view or attach window
%%%

win_init (ParentWin) ->
    GS = gs:start (),

    gs:window (search_win, GS, [{x, gs:read (ParentWin, x) + 50}, 
				{y, gs:read (ParentWin, y) + 50},
				{title, "Search"},
				{configure, true}]),

    gs:entry (search_entry, search_win, [{keypress, true}]),

    gs:checkbutton (cs_cb, search_win, [{label, {text, "Case Sensitive"}},
					{select, false}]),

    gs:label (info_label, search_win, [{label, {text, ""}}]),

    gs:button (search_button, search_win, [{label, {text, "Search"}}]),

    gs:button (clear_button, search_win, [{label, {text, "Clear"}}]),

    gs:button (close_button, search_win, [{label, {text, "Close"}}]),

    win_configure (?WIN_W, ?WIN_H),
    gs:config (search_win, [{map, true}, raise]).



%%% win_configure  /2
%%%
%%% Minimum height is ?WIN_H
%%%
%%% Pre:
%%%    W, H  ==  integer ()
%%%              Width, Height
%%%

win_configure (W, H) when H > ?WIN_H ->
    gs:config (search_entry, {width, W -20}),

    gs:config (cs_cb, {x, W / 2 - 75}),

    gs:config (info_label, {width, W - 20}),

    gs:config (search_button, [{x, W / 4 - ?BUTT_W}, {y, H - 40}]),

    gs:config (clear_button, [{x, W / 2 - ?BUTT_W / 2}, {y, H - 40}]),

    gs:config (close_button, [{x, W * 3 / 4}, {y, H - 40}]);


win_configure (W, H) -> 
    gs:config (search_win, [{width, W}, {height, ?WIN_H}]),

    gs:config (search_entry, [{x, 10}, {y, 10}, 
			      {width, W - 20}]),

    gs:config (cs_cb, [{x, W / 2 - 75}, {y, 40},
		       {width, 150}]),

    gs:config (info_label, [{x, 10}, {y, 70},
			    {width, W - 20}]),

    gs:config (search_button, [{x, W / 4 - ?BUTT_W}, 
			       {y, ?WIN_H - 40}, 
			       {width ,?BUTT_W}]),

    gs:config (clear_button, [{x, W / 2 - ?BUTT_W / 2}, 
			      {y, ?WIN_H - 40}, 
			      {width ,?BUTT_W}]),

    gs:config (close_button, [{x, W * 3 / 4}, 
			      {y, ?WIN_H - 40}, 
			      {width ,?BUTT_W}]).


%%% write_info  /1
%%%
%%% write_info puts the given Text at 
%%% the gs label info_label.
%%%

write_info (Text) ->
    gs:config (info_label, {label, {text, Text}}).



%%% search_action  /4
%%%
%%% search_action returns the position of the given string.
%%%
%%% Pre:
%%%    String    ==  string ()
%%%    CaseSen   ==  bool ()
%%%                  case sensitive
%%%    Parent    ==  pid ()
%%%                  owner of the edior
%%%    Pos       ==  {Row, Col}
%%%    Row, Col  ==  integer ()
%%%                  position to search from
%%%

%%% The search entry was empty.
%%%

search_action ([], _CaseSen, _Parent, Pos) ->
    write_info (""),
    Pos;

search_action (String, CaseSen, Parent, Pos) ->
    write_info (""),
    String_2 = 
	case CaseSen of
	    false ->
		to_lower (String);

	    true ->
		String
	end,

    Parent ! {editor_search, self (), String_2, Pos, CaseSen},

    receive
	{new_pos, Parent, NPos} ->
	    NPos;

	{not_found, Parent, NPos} ->
	    write_info (io_lib:format ("~nCould not find ~p~n", [String])),
	    NPos
    after 
	20000 ->
	    {1, 0}
    end.



%%% editor_search  /5
%%%
%%% editor_search is called by the owner of the editor.
%%% It returns the position of the found string or the top
%%% position of the editor, if not found.
%%%
%%% Pre:
%%%    SearchPid  ==  pid ()
%%%                   the process of the search window
%%%    Editor     ==  gs editor
%%%    String     ==  string ()
%%%    Pos        ==  {Row, Col}
%%%    CaseSen    ==  bool ()
%%%

editor_search (SearchPid, Editor, String, Pos, CaseSen) ->
    unmark_string (Editor, element (1, Pos)),       % unmark the old string
    Answ = search (Editor, String, Pos, CaseSen),

    case Answ of
	{found, NewPos} ->
	    mark_string (Editor, {String, element (1, NewPos), 
				  element (2, NewPos)}),
	    SearchPid ! {new_pos, self (), NewPos};

	not_found ->
	    SearchPid ! {not_found, self (), {1, 0}}
    end.    



%%% search  /4, 6
%%%
%%% search returns the position of the given string or else false.
%%%
%%% Pre: 
%%%    search/4
%%%    Editor   ==  gs editor
%%%    String   ==  string ()
%%%    var. 3   ==  {Row, Col}
%%%                 start position
%%%    CaseSen  ==  bool ()
%%%    
%%%    search/6
%%%    Col, Row  ==  integer ()
%%%    Size      ==  integer ()
%%%                  #rows in the editor
%%%

%%% Search from the beginning of the editor.
%%%

search (Editor, String, {1, 0}, CaseSen) ->
    Row = 1,
    Size = gs:read (Editor, size) + 1,
    Col = get_col (Editor, String, {Row, 8}, CaseSen, 0),
    search (Editor, String, Col, Row + 1, Size, CaseSen);


%%% Search for the next string.
%%% At {Row, Col} the previous string was found, 
%%% begin the search after that one.
%%%

search (Editor, String, {Row, Col}, CaseSen) ->
    Size = gs:read (Editor, size) + 1,
    StrLen = string:len (String),
    NewCol = get_col (Editor, String, {Row, Col + StrLen}, 
		      CaseSen, Col + StrLen - 8),
    search (Editor, String, NewCol, Row + 1, Size, CaseSen).


%%% The bottom of the editor has been reached.
%%%

search (_Editor, _String, _Col, Size, Size, _CaseSen) ->
    not_found;


%%% The string was not found (Col == 0), search again.
%%%

search (Editor, String, 0, Row, Size, false) ->
    RowString = gs:read (Editor, {get, {{Row, 8}, {Row, lineend}}}),  
    RowStringLowC = to_lower (RowString),
    Col = string:str (RowStringLowC, String), 
    search (Editor, String, Col, Row + 1, Size, false);


search (Editor, String, 0, Row, Size, true) ->
    RowString = gs:read (Editor, {get, {{Row, 8}, {Row, lineend}}}),  
    Col = string:str (RowString, String),
    search (Editor, String, Col, Row + 1, Size, true);


search (_Editor, _String, Col, Row, _Size, _CaseSen) ->
    {found, {Row - 1, Col + 7}}.  % Col 0 - 7 are for the row numbers



%%% get_col  /5
%%%
%%% get_col returns the column of the row where the 
%%% wanted string was found.
%%%
%%% Pre:
%%%    Editor    ==  gs editor
%%%    String    ==  string ()
%%%    Row, Col  ==  integer ()
%%%    var. 4    ==  bool ()
%%%                  case sensitive
%%%    AddOn     ==  integer ()
%%%                  compensates if the search was started somewhere
%%%                  in the row, the row had more than one string
%%%

get_col (Editor, String, {Row, Col}, false, AddOn) ->
    RowString = gs:read (Editor, {get, {{Row, Col}, {Row, lineend}}}),  
    RowStringLowC = to_lower (RowString),
    case string:str (RowStringLowC, String) of
	0 ->
	    0;

	NCol ->
	    NCol + AddOn
    end;

get_col (Editor, String, {Row, Col}, true, AddOn) ->
    RowString = gs:read (Editor, {get, {{Row, Col}, {Row, lineend}}}),  
    case string:str (RowString, String) of
	0 ->
	    0;

	NCol ->
	    NCol + AddOn
    end.



%%% mark_string  /2
%%%
%%% mark_string marks the found string in the given editor
%%%
%%% Pre:
%%%    Editor    ==  gs editor
%%%    String    ==  string ()
%%%    Row, Col  ==  integer ()
%%%                  the position of the string
%%%

mark_string (Editor, {String, Row, Col}) ->
    Length = string:len (String),
    {Font, _, Size} = gs:read (Editor, {font_style, {Row, 0}}),
    gs:config (Editor, {vscrollpos, Row - 5}),
    gs:config (Editor, [{font_style, {{{Row, Col}, 
				       {Row, Col + Length}}, 
				      {Font, [bold], Size}}}]),

    case gs:read (Editor, {fg, {Row, 0}}) of

	%% If it's a red breakrow
	%%

	red ->
	    gs:config (Editor, [{fg, {{{Row, Col}, 
				       {Row, Col + Length}}, 
				      black}}]);
	    
	Else ->
	    gs:config (Editor, [{fg, {{{Row, Col}, 
				       {Row, Col + Length}}, 
				      red}}])
    end.


%%% unmark_string  /2
%%%
%%% unmark_string restores the given row with the marked string.
%%%
%%% Pre:
%%%    Editor  ==  gs editor
%%%    Row     ==  integer ()
%%%

unmark_string (Editor, Row) ->
    {Font, _, Size} = gs:read (Editor, {font_style, {Row, 0}}),
    gs:config (Editor, [{font_style, {{{Row, 0}, {Row, lineend}}, 
				      {Font, [], Size}}},
			{fg, {{{Row, 0}, {Row, lineend}}, 
			      gs:read (Editor, {fg, {Row, 0}})}}]).
	    


%%%
%%% Make a string all lower case
%%%

to_lower( String ) ->
    [ lower_case(C) || C <- String ].



%%%
%%% Turn an (ascii, english) letter into its lower case equivalent
%%%

lower_case( C ) ->
    case (( C >= $A ) and ( C =< $Z )) of
	true -> 
	    C + 32;

	_ -> 
	    C
    end.    


