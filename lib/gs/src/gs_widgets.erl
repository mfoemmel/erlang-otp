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
%% Widget specific data
%% ------------------------------------------------------------
%%

-module(gs_widgets).


%% ----- Exports -----
-compile(export_all).


options(ObjType, Options) ->
    options_impl(default_options(ObjType), Options).

options_impl([], Options) -> Options;
options_impl([{DefaultOption, Value} | T], Options) ->
    case lists:keymember(DefaultOption, 1, Options) of
	true -> options_impl(T, Options);
	false ->
	    [{DefaultOption,Value}|options_impl(T,Options)]
    end;
options_impl([DefaultOption | T], Options) ->
    case lists:member(DefaultOption, Options) of
	true -> options_impl(T, Options);
	false ->
	    [DefaultOption|options_impl(T,Options)]
    end.

%% ------------------------------------------------------------
%% default_options for widgets
%% Keep the options in the list sorted!
%% ------------------------------------------------------------

default_options(arc)         -> [{coords, [{0,0}, {0,0}]}];
default_options(button)      -> [{click,true}, {height,30}, {width,100}, {x,0},
				{y,0}];
default_options(canvas)      -> [{height,200}, {scrollregion,{0,0,300,200}},
				 {width,300}, {x,0}, {y,0}];
default_options(checkbutton) -> [{click,true}, {height,30}, {width,100}, {x,0},
				 {y,0}];
default_options(editor)      -> [{height,200}, {width,300}, {x,0}, {y,0}];
default_options(entry)       -> [{height,30}, {width,100}, {x,0}, {y,0}];
default_options(frame)       -> [{height,100}, {width,150}, {x,0}, {y,0}];
default_options(grid)        -> [{bg,grey}, {cellheight,20},
				 {columnwidths, [80,80,80,80]},
				 {fg,black}, {font,{screen, 12}},
				 {height,100},
				 {hscroll,bottom},
				 {rows,{1,10}},
				 {vscroll,right},
				 {width,300},
		 		 {x,0}, {y,0}];
						           % Keep the options in the list sorted!
default_options(gridline)    -> [{click,true}, {doubleclick,false}, {row,undefined}];
default_options(gs)          -> [{kernel,false},
				 {{default,all,font}, {screen,12}}];
default_options(image)       -> [{anchor,nw}, {coords,[{0,0}]}];
default_options(label)       -> [{height,30}, {width,100}, {x,0}, {y,0}];
default_options(line)        -> [{coords, [{-1,-1},{-1,-1}]}];
default_options(listbox)     -> [{height,130}, {hscroll,true},
				 {selectmode,single}, {vscroll,true},
				 {width,125}, {x,0}, {y,0}];
default_options(menu)        -> [];
						           % Keep the options in the list sorted!
default_options(menubar)     -> [{bw,2}, {height,25}, {highlightbw,0},
				 {relief,raised}];
default_options(menubutton)  -> [{anchor,nw}, {side,left}];
default_options(menuitem)    -> [{click,true}, {index,last}, {itemtype,normal}];
default_options(message)     -> [{height,75}, {width,100}];
default_options(oval)        -> [{coords, [{0,0},{0,0}]}];
default_options(polygon)     -> [{coords, [{0,0},{0,0}]}, {fg,black}, {fill,none}];
default_options(prompter)    -> [{height,200}, {prompt,[]}, {width,300}];
default_options(radiobutton) -> [{click,true}, {height,30}, {width,100},
				 {x,0}, {y,0}];
default_options(rectangle)   -> [{coords, [{0,0},{0,0}]}];
default_options(scale)       -> [{click,true}, {height,50}, {width,100},
				 {x,0}, {y,0}];
						           % Keep the options in the list sorted!
default_options(scrollbar)   -> [];
default_options(text)        -> [{anchor,nw}, {coords,[{0,0}]}, {justify,left}];
default_options(window)      -> [{configure,false}, {cursor,arrow}, {destroy,true},
				 {height,200}, {map,false}, {width,300}];
default_options(_)           -> [].

%%
%% Map primitive types to true and soft types to {soft, module} or false
%%

primitive(arc)         -> true;
primitive(button)      -> true;
primitive(canvas)      -> true;
primitive(checkbutton) -> true;
primitive(editor)      -> true;
primitive(entry)       -> true;
primitive(frame)       -> true;
primitive(grid)        -> true;
primitive(gridline)    -> true;
primitive(image)       -> true;
primitive(label)       -> true;
primitive(line)        -> true;
primitive(listbox)     -> true;
primitive(menu)        -> true;
primitive(menubar)     -> true;
primitive(menubutton)  -> true;
primitive(menuitem)    -> true;
primitive(message)     -> true;
primitive(oval)        -> true;
primitive(polygon)     -> true;
primitive(prompter)    -> true;
primitive(radiobutton) -> true;
primitive(rectangle)   -> true;
primitive(scale)       -> true;
primitive(scrollbar)   -> true;
primitive(text)        -> true;
primitive(window)      -> true;
primitive(_)           -> false.

container(canvas)      -> true;
container(frame)       -> true;
container(grid)        -> true;
container(menu)        -> true;
container(menubar)     -> true;
container(menubutton)  -> true;
container(menuitem)    -> true;
container(window)      -> true;
container(_)           -> false.
