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
-module(gdemo).

-export([mk_window/2, hr/1, text/5, nls/2, mk_hyper_tag/2, list_blop/1,
	 load_image/3, mk_frame/3]).

-export([run/1, test/0]).

-import(lists, [flatmap/2, reverse/1, foreach/2]).

test() ->
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
    Demos = [{"A simple buttone", hiho},
	     {"a dinger doo", dobo}],
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
    catch (apply(new, example, [Tag])),
    receive
	Any ->
	    halt
    end.

make_active(Text, Font, Str, Tag, Fun) ->
    io:format("here:~p ~p~n", [Str, Tag]),
    tk:cmd(Text, [tag,bind,Tag,"<Enter>", 
		  fun() -> 
			 tk:cmd(Text, [tag,configure,Tag,{foreground,red}])
		 end]),
    tk:cmd(Text, [tag,bind,Tag,"<Leave>", 
		  fun() -> 
			 tk:cmd(Text, [tag,configure,Tag,{foreground,black}])
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
    configure_standard_tags(Text),
    Text.

%% $win tag configure thin -font [HMx_font times 2 medium r]
%% $win tag configure hr -relief sunken -borderwidth 2 -wrap none \
%%		-tabs [winfo width $win]

configure_standard_tags(Text) ->
    tk:cmd(Text, [tag, configure, t_blue, {borderwidth, 2},{foreground,blue}]),
    %% <hr>
    config_font(Text, thin, times, 2, medium, r),
    Width = tk:winfo([width,Text]),
    tk:cmd(Text,[tag, configure, hr, {relief, sunken}, {borderwidth, 2},
		 {wrap, none},{tabs, Width}]),
    tk:bind(Text, "<Configure>", ['%w'],
	    fun(W) ->
		   tk:cmd(Text, [tag, configure, hr, {tabs, W}])
	    end),
    %% </hr>
    tk:cmd(Text, [tag, configure, t_underline, {underline,1}]),
    tk:cmd(Text, [tag, configure, t_center, {justify, center}]),
    tk:cmd(Text, [tag, configure, t_nowrap, {wrap, none}]),
    %%	$win tag configure rindent -rmargin $var(S_tab)c
    tk:cmd(Text, [tag, configure, t_strike, {overstrike, 1}]),
    tk:cmd(Text, [tag, configure, mark, {foreground, red}]), %% list markers
    tk:cmd(Text, [tag, configure, list, {spacing1, "3p"},
		 {spacing3, "3p"}]), %% regular lists
    tk:cmd(Text, [tag, configure, compact, {spacing1, "0p"}]), %% compact lists
    set_indents(Text),
    true.

set_indents(Text) ->
    foreach(fun(I) ->
		    Str = integer_to_list(I),
		    Name = "indent" ++ Str,
		    Cm = Str ++ "c",
		    tk:cmd(Text, [tag,configure,Name,
				  {lmargin1, Cm},{lmargin2, Cm}])
	    end,[1,2,3,4,5,6,7,8,9]).

nls(Text, N) ->
    Str = lists:duplicate(N, $\n),
    io:format("Nls:~w~", [N]),
    etk:cmd(Text,[insert, 'end', Str]).

hr(Text) ->
    etk:cmd(Text,[insert, 'end', "\n", space, "\n", thin,
		  "\t", "thin hr", "\n", thin]).

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

%% mk_hyper(Text, Font, Fun, Str) ->
%%    ButtonTag = tk:mkpath(Text, b),
%%    tk:cmd(Text, [tag,bind,ButtonTag,"<Button-1>", Fun]),
%%    text(Text, Font, [t_link, t_underline, ButtonTag], Str).

mk_hyper_tag(Text, Fun) ->
    Tag = tk:mkpath(Text, h),
    tk:cmd(Text, [tag,bind,Tag,"<Button-1>", Fun]),
    Tag.

list_blop(Text) ->
    etk:cmd(Text,[insert, 'end', "*"]).

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

examples() ->
    [{item, label1, "Labels"},
     {item, label2, "... options "},
     {item, label3, "... with bitmaps"},
     {item, message1, "Messages"},
     space,
     {item, button1, "Basic buttons"},
     {item, button2, "... options"},
     space,
     {item, button3, "Checkbuttons"},
     {item, button4, "Radiobuttons"},
     space,
     {item, item1, "Basic Entries"},
     {item, item2, "... options"},
     space,
     {item, scale1, "Basic scales"},
     {item, scale2, "... options"},
     space,
     {item, listbox1, "Basic listboxes"},
     {item, listbox2, "... with scrollbars"},
     space,
     {item, menu1, "Basic menu"},
     {item, menu2, "... options"},
     {title, "Geometry"},
     {item, pack1, "Basic packer"},
     {item, pack2, "... options"},
     {item, pack3, "... more options"},
     space,
     {item, frame1, "Basic frames"},
     {item, frame2, "... options"},
     {item, frame3, "... more options"},
     space,
     {item, group1, "Grouping Widgets"},
     {title, "Events"},
     {item, bind1, "Binding events"},
     {item, bind2, "Binding events #2"},
     {item, bind3, "Binding events #3"},
     {title, "Canvas"},
     {item, canvas1, "Basic Canvas"},
     {item, canvas2, "... item types"},
     {item, canvas3, "... events"},
     {item, canvas4, "... item stacking"},
     {item, canvas5, "... tags"},
     {item, canvas6, "... with scrollbars"},
     {item, canvas7, "... with widgets"},
     space,
     {item, canvas8, "Drawing"},
     {item, canvas9, "Funky drawing"},
     {item, canvas10, "Rubber banding"},
     {item, canvas11, "Animation"},
     {item, canvas12, "Drag and Drop"},
     {title, "Text"},
     {item, text1, "Basic Text"},
     {item, text2, "... with scrollbars"},
     {item, text3, "... wrap modes"},
     {item, text4, "... basic tags"},
     {item, text5, "... tags with bindings"},
     {title, "Misc"},
     {item, misc1,"New toplevel windows"},
     {item, misc2, "Dialog boxes"},
     {item, misc3, "Widget options"},
     {item, misc4, "X selection"},
     {item, misc5, "Tk wait"},
     {title, "Advanced"},
     {item, advanced1, "Netscape"}].

