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
-module(wtour).

%% -export([start/0, start1/0, eval_str/1]).
-compile(export_all).

start() -> spawn(wtour, start1, []).

start1() ->
    etk:start(),
    {Top, Text} = mk_window(),
    StartTag = label1,
    Pid = load(Text, StartTag),
    server(Top, StartTag, Text, Pid, order()).

mk_window() ->
    Top = etk:toplevel([]),
    tk:wm([title,Top,"Widget tour"]),
    Self = self(),
    Text = Top ++ ".txt",
    ScrollY = Top ++ ".sy",
    tk:text(Text,
            [{yscrollcommand,
              fun(From,To) -> tk:cmd(ScrollY, ["set",From,To]) end},
             {wrap, "word"}]),
    tk:scrollbar(ScrollY,
                 [{relief, "flat"}, {width, 10},
                  {command, fun(Args) ->
				    tk:cmd(Text, ["yview" | Args])
			    end}]),
    MenuBar = etk_menu:create_menubar(Top, menu()),
    tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    tk:pack(ScrollY,[{side, "right"}, {fill, "y"}]),
    tk:pack(Text, [{expand, "yes"}, {fill, "both"}, {'after', ScrollY}]),
    Quit = etk:button(Top,[{text,"quit"},{anchor,w},
			   {command, fun() -> exit(Self, die) end}]),
    Run  = etk:button(Top,[{text,"run"},
			   {command, fun() -> Self ! run end}]),
    Next = etk:button(Top,[{text,"next"},
			   {command, fun() -> Self ! next end}]),
    Prev = etk:button(Top,[{text,"previous"},
			   {command, fun() -> Self ! previous end}]),
    Save = etk:button(Top,[{text,"save"},
			   {command, fun() -> Self ! save end}]),
    tk:pack([Quit,Run,Next,Prev,Save,{side,left}]),
    {Top, Text}.

load(Text, Tag) ->
    tk:cmd(Text,[delete,"1.0", 'end']),
    Str = example(Text, Tag),
    New = spawn(wtour, eval_str, [Str]).

server(Top, Current, Text, Old, Order) ->
    receive
	run ->
	    Where = tk:winfo([geometry, Top]),
	    io:format("Where:~p\n", [Where]),
	    killit(Old),
	    Str = tk:rcmd(Text, [get,"1.0",'end']),
	    New = spawn(wtour, eval_str, [Str]),
	    server(Top, Current, Text, New, Order);
	{menu, What} ->
	    killit(Old),
	    New = load(Text, What),
	    server(Top, What, Text, New, Order);
	next ->
	    Next = next(Current, Order),
	    killit(Old),
	    New = load(Text, Next),
	    server(Top, Next, Text, New, Order);
	previous ->
	    Prev = prev(Current, Order),
	    killit(Old),
	    New = load(Text, Prev),
	    server(Top, Prev, Text, New, Order);
	save ->
	    Str = tk:rcmd(Text, [get,"1.0",'end']),
	    file:write_file("saved", Str),
	    server(Top, Current, Text, Old, Order);
	Any -> 
	    io:format("Hi from server:~w\n", [Any]),
	    server(Top, Current, Text, Old, Order)
    end.

next(X, [X,Y|_]) -> Y;
next(X, [_|T])   -> next(X, T);
next(X, [])      -> X.

prev(X, [Y,X|_]) -> Y;
prev(X, [_|T])   -> prev(X, T);
prev(X, [])      -> X.


killit(P) when pid(P) ->  P ! die;
killit(_)             ->  true.

eval_str(Str) ->
    case erl_scan:tokens([],Str,1) of
	{done, {ok, Toks, Line}, _} ->
	    case handle_toks(Toks, Line) of
		ok ->
		    loop();
		error -> error
	    end;
	{error, {Line,Mod,Args}, Next} ->
	    io:format("*** ~w ~p~n", [Line,Mod:format_error(Args)]),
	    error;
	Other ->
	    io:format("Do:~p\n", [Other])
    end.

handle_toks(Toks, Line) ->
    %% io:format("Toks:~p\n", [Toks]),
    case catch erl_parse:parse_exprs(Toks) of
	{ok, Exprs} ->
	    %% io:format("Got:~p\n", [Exprs]),
	    Bind0 = erl_eval:new_bindings(),
	    case catch erl_eval:exprs(Exprs, Bind0) of
		{'EXIT', Reason} ->
		    io:format("** oops **~p\n", [Exprs]),
		    error;
		{value, _, Bind1} ->
		    ok
	    end;
	{error, {LineNo, Mod, What}} ->
	    Str = apply(Mod, format_error, [What]),
	    io:format("*** Line:~w  ***~s\n", [LineNo, Str]),
	    error;
	Parse_error ->
	    io:format("Parse Error:~p\n",[Parse_error]),
	    error
    end.

loop() ->
    receive
	die ->
	    exit(die);
	Any ->
	    io:format("Client loop:~p\n", [Any]),
	    loop()
    end.

order() ->
    {menubar,M} = menu(fun(Id,_) -> {id, Id} end),
    [Cmd || {menu,_,_,C} <- M, {id, Cmd} <- C].

menu() -> menu(fun but/2).

but(Id, Text) ->
    Pid = self(),
    {button, [{label,Text}, {func, fun() -> Pid ! {menu, Id} end}]}.

menu(F) ->
    {menubar,
     [{menu,  "File", "left", 
       [F(open, "Open ..."),
	F(save, "Save ..."),
	separator,
	F(exit, "Exit")]},
      {menu, "Widgets", "left",
       [F(label1, "Labels"),
	F(label2, "... options "),
	F(label3, "... with bitmaps"),
	F(message1, "Messages"),
	separator,
	F(button1, "Basic buttons"),
	F(button2, "... options"),
	separator,
	F(button3, "Checkbuttons"),
	separator,
	F(button4, "Radiobuttons"),
	separator,
	F(entry1, "Basic Entries"),
	F(entry2, "... options"),
	separator,
	F(scale1, "Basic scales"),
	F(scale2, "... options"),
	separator,
	F(listbox1, "Basic listboxes"),
	F(listbox2, "... with scrollbars"),
	separator,
	F(menu1, "Basic menu"),
	F(menu2, "... options")]},
      
      {menu, "Geometry", "left",
       [F(pack1, "Basic packer"),
	F(pack2, "... options"),
	F(pack3, "... more options"),
	separator,
	F(frame1, "Basic frames"),
	F(frame2, "... options"),
	F(frame3, "... more options"),
	separator,
	F(group1, "Grouping Widgets")]},

      {menu, "Events", "left",
       [F(bind1, "Binding events"),
	F(bind2, "Binding events #2"),
	F(bind3, "Binding events #3")]},
	
      {menu, "Canvas", "left",
       [F(canvas1, "Basic Canvas"),
	F(canvas2, "... item types"),
	F(canvas3, "... events"),
	F(canvas4, "... item stacking"),
	F(canvas5, "... tags"),
	F(canvas6, "... with scrollbars"),
	F(canvas7, "... with widgets"),
	separator,
	F(canvas8, "Drawing"),
	F(canvas9, "Funky drawing"),
	F(canvas10, "Rubber banding"),
	F(canvas11, "Animation"),
	F(canvas12, "Drag and Drop")]},

      {menu, "Text", "left", 
       [F(text1, "Basic Text"),
	F(text2, "... with scrollbars"),
	F(text3, "... wrap modes"),
	F(text4, "... basic tags"),
	F(text5, "... tags with bindings")]},
	
      {menu, "Misc", "left", 
       [F(misc1,"New toplevel windows"),
	F(misc2, "Dialog boxes"),
	F(misc3, "Widget options"),
	F(misc4, "X selection"),
	F(misc5, "Tk wait"),
	F(misc6, "Linked buttons"),
	F(misc7, "Drawing into image")
       ]},
      {menu, "Advanced", "left",
       [F(advanced1, "Netscape")]},
      {menu, "Help", "right", []}]}.

			    
example(W, Tag) ->
    BaseName = atom_to_list(Tag),
    File =  BaseName ++ ".ex",
    case file:open(File, [read]) of
	{ok, Fd} ->
	    {ok,Size} = file:position(Fd, {eof,0}),
	    file:position(Fd, {bof,0}),
	    case file:read(Fd, Size) of
		{ok, Cs} -> 
		    Cs1 = strip_cr(Cs),
		    tk:cmd(W,[insert,current,Cs1]),
		    Cs1;
		Error ->
		    tk:cmd(W,[insert,current,
			      "Read error (" ++ BaseName ++ ")"]),
		    []
	    end;
	Error ->
	    tk:cmd(W,[insert,current,
		      "No such example (" ++ BaseName ++ ")"]),
	    []
    end.

strip_cr([$\r,$\n | Cs]) -> [$\n | strip_cr(Cs)];
strip_cr([C | Cs]) -> [C | strip_cr(Cs)];
strip_cr([]) -> [].



	    
	    
	    



