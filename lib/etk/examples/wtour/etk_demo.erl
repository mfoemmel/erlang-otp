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
-module(etk_demo).

-export([mk_window/2, text/5, nls/2, 
	 load_image/3, mk_frame/3]).

-export([run/1, start/0]).

-import(lists, [flatmap/2, reverse/1, foreach/2]).

start() ->
    Title = "Widget Demo",
    Self = self(),
    Quit = (fun() -> 
		exit(Self, die) 
	   end),
    Menu = {menubar,
	    [{menu,"File",
	      "left",
	      [{button,[{label,"Quit"},{func, Quit}]}]}]},
    Text = mk_window(Title, Menu),
    Font = {font, times,14,medium,r},
    Font1 = {font, times,24,medium,i},
    HeaderFont = {font,times,24,bold,r},
    text(Text, false, HeaderFont, [], "Etk Widget Demonstrations"),
    nls(Text, 2),

    Bigtext = "This application provides a front end for several short programswhich demonstrate what you can do with Erltk programs.  Each of the numbered lines below describes a demonstration;  you can click on it to invoke the demonstration.  Once the demonstration window appears, you can click the \"See Code\" button to see the ErlTk code which created the demonstration.",
    text(Text, true, Font, [], Bigtext),
    nls(Text,2),
    show(examples(), 1, Font, Text).

show([space|T], N, Font, Text) ->
    nls(Text,1),
    show(T, N, Font, Text);
show([{title, Str}|T], N, Font, Text) ->
    text(Text, false, Font, [], Str),
    nls(Text, 2),
    show(T, N, Font, Text);
show([{item, Tag, Str}|T], N, Font, Text) ->
    String = "      " ++ integer_to_list(N) ++ ". " ++ Str,
    make_active(Text, Font, String, Tag, 
		fun() -> 
			 spawn(?MODULE, run, [Tag])
			 end),
    nls(Text, 1),
    show(T, N + 1, Font, Text);
show([], _, _, _) ->
    void.

run(Tag) ->
    io:format("Do ~w~n", [Tag]),
    catch (apply(Tag, start, [])),
    receive
	Any ->
	    halt
    end.

make_active(Text, Font, Str, Tag, Fun) ->
    io:format("here:~p ~p~n", [Str, Tag]),
    tk:cmd(Text, [tag,bind,Tag,"<Enter>", 
		  fun() -> 
			 tk:cmd(Text, [tag,configure,Tag,{background,"SeaGreen3"}])
		 end]),
    tk:cmd(Text, [tag,bind,Tag,"<Leave>", 
		  fun() -> 
			 tk:cmd(Text, [tag,configure,Tag,{background,grey}])
		 end]),
    tk:cmd(Text, [tag,bind,Tag,"<Button-1>", Fun]),
    text(Text, false, Font, [Tag], Str).


mk_window(Title, Menu) ->
    etk:start(),
    Top = etk:toplevel([]),
    tk:wm([title,Top,Title]),
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
    MenuBar = etk_menu:create_menubar(Top, Menu),
    tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    tk:pack(ScrollY,[{side, "right"}, {fill, "y"}]),
    tk:pack(Text, [{expand, "yes"}, {fill, "both"}, {'after', ScrollY}]),
    Text.

nls(Text, N) ->
    Str = lists:duplicate(N, $\n),
    io:format("Nls:~w~", [N]),
    etk:cmd(Text,[insert, 'end', Str]).

text(Text, Fill, Font, Tags, Str) ->
    Name  = set_font(Text, Font),
    Str1  = fill_text(Fill, Str),
    Tags1 = format_tag_string(Tags),
    All = Name ++ Tags1,
    %% io:format("insert:|~s| |~s|~n",[Str1, All]),
    etk:cmd(Text,[insert, 'end', Str1, All]).

format_tag_string(Tags) ->
    flatmap(fun(Name) when atom(Name) -> 
		    [$ |atom_to_list(Name)];
	       (Str) -> [$ |Str]
	    end, Tags).

set_font(Text, Font) ->
    case get({font,Font}) of
	undefined ->
	    Tag = load_font(Text, Font),
	    put({font,Font}, Tag),
	    Tag;
	Tag ->
	    Tag
    end.

load_font(Text, {font, Family, Size, Weight, Style}) -> 
    Tag = atom_to_list(Family) ++ [$:|integer_to_list(Size)] ++ 
	[$:|atom_to_list(Weight)] ++ [$:|atom_to_list(Style)],
    Adjust_size = 0,
    Size1 = Size  + Adjust_size,
    config_font(Text, Tag, Family, Size1, Weight, Style).

config_font(Text, Tag, Family, Size, Weight, Style) ->
    F = "-*-" ++ atom_to_list(Family) ++ [$-|atom_to_list(Weight)] ++ 
	[$-|atom_to_list(Style)] ++ "-normal-*-*-" ++
	integer_to_list(Size) ++ "0-*-*-*-*-*-*", 
    %% io:format("load font ~p -> ~p~n", [Tag,F]),
    case catch tk:cmd(Text, [tag, configure, Tag, {font, F}]) of
	{'EXIT', Why} ->
	    io:format("oops");
	_ ->
	    true
    end,
    Tag.

fill_text(false, Str) -> Str;
fill_text(true, Str)  -> remove_nls(Str).

%% remove_nls(Str) -> Str'
%%  change \n's to blanks

remove_nls(T) -> remove_nls(T, []).

remove_nls([$\n|T], L) -> remove_nls(T, [$ |L]);
remove_nls([H|T], L)   -> remove_nls(T, [H|L]);
remove_nls([], L)      -> reverse(L).

load_image(Text, Url, RelFile) ->
    %% is this the easiest way to do this ??
    %% dunno - it works!
    ImageUrl = html:fix_url(Url, RelFile),
    case html:url_type(ImageUrl) of
	{file, ImageFile} ->
	    Label = etk:label(Text, []),
	    Image = Label ++ ".i",
	    tk:cmd(Text,[window,create,insert,{window,Label}]),
	    catch tk:image([create,photo,Image,{file,ImageFile}]),
	    tk:cmd(Label, [config,{image,Image}]);
	{http, _} ->
	    io:format("Cannot fetch remote images yet!~n");
	error ->
	    error
    end,
    Text.

mk_frame(Text, X, Y) ->
    io:format("Making a frame:~p ~p ~n", [X,Y]),
    W = integer_to_list(X) ++ "m",
    H = integer_to_list(Y) ++ "m",
    Frame = etk:frame(Text, [{width,W},{height,H}]),
    tk:cmd(Text,[window,create,insert,{window,Frame},
		 {pady,0},{stretch,false}]),
    Frame.

font() ->
    "-*-Helvetica-Medium-R-Normal--*-140-*-*-*-*-*-*".

examples() ->
    [{title, "Labels, buttons, checkbuttons and radiobuttons"},
     {item, etk_demo_label, "Labels (Text and bitmaps)"},
     {item, etk_demo_button, "Labels (Text and bitmaps)"}].



 
