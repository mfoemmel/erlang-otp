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
-module(wtour_netscape).

%% this is a much more complex example

-export([start/0]).

-import(lists, [foreach/2]).

%%
%% Create the browser window
%%

start() ->
    etk:start(),
    W = etk:toplevel([]),
    tk:wm([maxsize, W, 10000, 10000]),
    tk:wm([geometry, W, "550x400+100+200"]),
    tk:wm([title, W, "Escape"]),

    Text = W ++ ".text",
    ScrollY = W ++ ".sy",

    MenuBar = etk_menu:create_menubar(W, menus()),
    Tools = create_tools(W),
    Location = create_location(W),
    Dir = create_dir_buttons(W),

    tk:scrollbar(ScrollY,
		 [{relief, "flat"}, {width, 10},
		  {command, fun(Args) ->
				   tk:cmd(Text, ["yview" | Args])
			   end}]),
    create_text(W),
    Hr = W ++ ".hr",
    tk:frame(Hr, [{bd,2},{relief,"sunken"},{height,5}]),

    tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    tk:pack(Tools, [{side, "top"}, {anchor, "w"}, {padx, 8}]),
    tk:pack(Location, [{side, "top"}, {anchor, "w"}, {pady, 4},
		       {padx, 8}, {fill,"x"}]),
    tk:pack(Dir, [{side, "top"}, {anchor, "w"}, {padx, 8}]),
    tk:pack(Hr, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    tk:pack(ScrollY,[{side, "right"}, {fill, "y"}]),
    tk:pack(Text, [{expand, "yes"}, {fill, "both"}]),
    W.

%%
%% Directory buttons
%%

create_dir_buttons(W) ->
    Frame = W ++ ".dir",
    tk:frame(Frame, []),
    lists:foreach(
      fun({Name,Text}) ->
	      tk:button(Frame ++ "." ++ Name,
			[{text, Text},
			 {font, button_font()},
			 {padx,1}, {pady,2}]),
	      tk:pack(Frame ++ "." ++ Name, [{side, "left"}])
      end, dirs()),
    Frame.

%%
%% Location
%%

create_location(W) ->
    Frame = W ++ ".loc",
    Label = Frame ++ ".label",
    Entry = Frame ++ ".entry",
    tk:frame(Frame, []),
    tk:label(Label, [{text, "Location : "}, 
		     {font, label_font()}]),
    tk:entry(Entry, [{font, entry_font()}, 
		     {bg, "white"}]),
    tk:pack(Label, [{side, "left"}]),
    tk:pack(Entry, [{side, "left"},{expand, 1}, {fill, "x"}]),
    tk:bind(Entry, "<Return>", [],
	    fun() -> display({location,get_loc(W)}) end),
    Frame.



create_text(W) ->
    Text = W ++ ".text",
    ScrollY = W ++ ".sy",
    
    tk:text(Text,
	    [{yscrollcommand,
	      fun(From,To) -> tk:cmd(ScrollY, ["set",From,To]) end},
	     {wrap, "word"}]),

    tk:bind(Text, "<B1-Motion>", [], fun() -> false end),

    add_font(Text, "TN3", "-adobe-times-medium-r-*-*-14-*-*-*-*-*-*-*"),
    add_font(Text, "TI3", "-*-times-medium-i-*-*-14-*-*-*-*-*-*-*"),
    add_font(Text, "TB3", "-adobe-times-bold-r-*-*-14-*-*-*-*-*-*-*"),
    add_font(Text, "TB4", "-*-times-bold-r-*-*-14-*-*-*-*-*-*-*"),
    add_font(Text, "TB5", "-Adobe-Helvetica-Bold-R-Normal-*-*-240-*"),
    add_font(Text, "CN3", "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*"),
    add_font(Text, "code", "-adobe-helvetica-bold-r-normal-*-*-160-*-*-*-*-*-*"),
    tk:cmd(Text, ["tag", "configure", "red", {foreground, "red"}]),
    tk:cmd(Text, ["tag", "configure", "justify", {justify,"left"}]),
    Bold = fun() -> tk:cmd(Text, ["tag", "configure", "d1",
				  {background, "#43ce80"},
				  {relief,"raised"},
				  {borderwidth, 1}]) end,
    Normal = fun() -> tk:cmd(Text, ["tag", "configure", "d1",
				    {background, ""},
				    {relief,"flat"}]) end,
    tk:bind_tag(Text, "d1", "<Any-Enter>", [], Bold),
    tk:bind_tag(Text, "d1", "<Any-Leave>", [], Normal),

    tk:cmd(Text, ["tag", "configure", "l1",
		  {lmargin1, "1c"}, {lmargin2, "1c"}]),
    tk:cmd(Text, ["tag", "configure", "l2",
		  {lmargin1, "2c"}, {lmargin2, "2c"}]),
    tk:cmd(Text, ["tag", "configure", "l3",
		  {lmargin1, "3c"}, {lmargin2, "3c"}]),
    tk:cmd(Text, ["tag", "configure", "l4",
		  {lmargin1, "4c"}, {lmargin2, "4c"}]),

    tk:cmd(Text, ["tag", "configure", "BU",  
		  {font, "-adobe-times-bold-r-*-*-14-*-*-*-*-*-*-*"},
		  {foreground, "red"}]),
    Text.

init_images() ->
    foreach(
      fun ({_,_}) -> false;
          (Tool) ->
              tk:image(["create", "photo", "im" ++ Tool,
                        {format, "gif"},
                        {file, "ns_" ++ Tool ++ ".gif"}])
      end, tools()).

%%
%% Create the tool bar
%%

create_tools(W) ->
    init_images(),
    Frame = W ++ ".tools",
    tk:frame(Frame, []),
    foreach(
      fun ({F,Width}) ->
	      tk:frame(Frame ++ "." ++ F, [{width, Width}]),
	      tk:pack(Frame ++ "." ++ F, [{side, "left"}]);
	  (Tool) ->
	      tk:button(Frame ++ "." ++ Tool,
			[{image, "im" ++ Tool},
			 {command,
			  fun() -> display({tool,Tool}) end}]),
	      tk:pack(Frame ++ "." ++ Tool,
		      [{side, "left"}])
      end, tools()),
    Frame.

button_font() ->
    "-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*".

label_font() ->
    "-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*".

entry_font() ->
    "-*-courier-medium-r-*-*-12-*-*-*-*-*-*-*".



add_font(W, Name, F) ->
    (catch tk:cmd(W, ["tag", "configure", Name, {font, F}])).

dirs() ->
    [{"new", "What's New"},
     {"cool", "What's Cool"},
     {"handbook", "Handbook"},
     {"search", "Net Search"},
     {"dir", "Net Directory"},
     {"soft", "Software"}].


tools() ->
    [ "back", "forw", "home", {"g1",10}, "reload", 
     "image", "open", "print", "find", {"g2",10}, "stop"].

display(X) ->
    io:format("Netscape:~p\n", [X]).

get_loc(W) ->
    Entry = W ++ ".loc.entry",
    tk:cmd(Entry, ["get"]).

menus() ->
    {menubar,

     [{menu, "File", "left", 
       [but(newb, "New Web Browser", 8, "Alt+N"),
	but(newm, "New Mail Message", 4, "Alt+M"),
	but(maild, "Mail Document", 5, ""),
	separator,
	but(openl, "Open Location...", 5, "Alt+L"),
	but(opendf, "Open File...", 0, "Alt+O"),
	but(saveas, "Save As...", 5, "Alt+S"),
	but(upload, "Upload File", 0, ""),
	separator,
	but(print, "Print...", 0, "Alt+P"),
	separator,
	but(clone, "Clone", 0, "Alt+W"),
	but(exit, "Exit", 0, "Alt+Q")]},

      {menu, "Edit", "left",
       [but(undo, "Undo", 0, ""),
	separator,
	but(cut, "Cut", 2, ""),
	but(copy, "Copy", 0, ""),
	but(paste, "Paste", 0, ""),
	separator,
	but(find, "Find...", 0, "Alt+F"),
	but(finda, "Find Again", 6, "Alt+G")]},

      {menu, "Go", "left",
       [but(backw, "Back", 0, "Alt+Left"),
	but(forw, "Forward", 0, "Alt+Right"),
	but(home, "Home", 0, ""),
	but(stopl, "Stop loading", 0, "ESC"),
	separator,
	checkbut(err, "Error Message", {0,1}, 0, "Alt+1")]},

      {menu, "Bookmarks", "left",
       [but(addb, "Add Bookmark", 0, "Alt+A"),
	separator]},
	
      {menu, "Options", "left",
       [but(genp, "General Preferences...", -1, ""),
	but(mnp, "Mail and News Preferences...", -1, ""),
	but(netp, "Network Preferences...", -1, ""),
	but(secp, "Security Preferences...", -1, ""),
	separator,
	checkbut(showt, "Show Toolbar", {0,1}, 5, ""),
	checkbut(showl, "Show Location", {0,1}, 5, ""),
	checkbut(showd, "Show Directory Buttons", {0,1}, 5, ""),
	checkbut(showj, "Show Java Console", {0,1}, -1, ""),
	separator,
	checkbut(autoi, "Auto Load Images", {0,1}, 0, ""),
	separator,
	{submenu, "Document Encoding",
	 [radiobut(enc1, "Western (Latin-1)", 1, -1, ""),
	  separator,
	  radiobut(enc2, "Central European (Latin-2)", 1, -1, ""),
	  separator,
	  radiobut(enc3, "Japanese (Auto-Detect)", 1, -1, ""),
	  radiobut(enc4, "Japanese (Shift-JIS)", 1, -1, ""),
	  radiobut(enc5, "Japanese (EUC-JP)", 1, -1, "")]},
	separator,
	but(saveo, "Save Options", 0, "")]},

      {menu,  "Directory", "left", 
       [but(dir1, "Escape's Home", 9, ""),
	but(dir2, "What's New", 7, ""),
	but(dir3, "What's Cool", 7, ""),
	separator,
	but(dir4, "Netscape Galleria", 9, ""),
	but(dir5, "Internet Directory", 9, ""),
	but(dir5, "Internet Search", 9, ""),
	but(dir5, "Internet White Pages", 9, ""),
	but(dir5, "About the Internet", -1, "")]},
	
      {menu, "Window", "left", []},
      {menu, "Help", "right", []}]}.

but(Tag, Text, U, A) ->
    {button, [{label, Text},{underline, U},{accelerator,A}]}.

checkbut(Tag, Text, Val, U, A) ->
    {checkbutton, Val, [{label, Text},{underline, U},{accelerator,A}]}.
    
radiobut(Tag, Text, V1, V2, _) ->
    {radiobutton, V1, [{label, Text}]}.
