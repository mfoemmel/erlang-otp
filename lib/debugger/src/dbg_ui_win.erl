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
-module(dbg_ui_win).

%% External exports
-export([init/0,
	 font/1,
	 create_menus/2, select/2, selected/1,
	 add_break/2, update_break/2, delete_break/1,
	 motion/2
	 
	]).

-record(break, {mb, smi, emi, dimi, demi}).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% init() -> GS
%%   GS = term()
%%--------------------------------------------------------------------
init() ->
    gs:start([{kernel, true}]).

%%--------------------------------------------------------------------
%% font(Style) -> Font
%%   Style = normal | bold
%% Select a suitable font. Defaults to {screen,12} and, if it does not
%% exist, {courier,12}.
%%--------------------------------------------------------------------
font(Style) ->
    GS = init(),
    Style2 = if
		 Style==normal -> [];
		 true -> [Style]
	     end,
    case gs:read(GS, {choose_font, {screen,Style2,12}}) of
	Font when element(1, Font)==screen ->
	    Font;
	_ ->
	    gs:read(GS, {choose_font, {courier,Style2,12}})
    end.

%%--------------------------------------------------------------------
%% create_menus(MenuBar, [Menu])
%%   MenuBar = gsobj()
%%   Menu = {Name, [Item]}
%%     Name = atom()
%%     Item = {Name, N} | {Name, N, Type} | {Name, N, cascade, [Item]}
%%          | separator
%%       N = no | integer()
%%       Type = check | radio
%% Create the specified menus and menuitems.
%%
%% Normal menuitems are specified as {Name, N}. Generates the event:
%%   {gs, _Id, click, {menuitem, Name}, _Arg}
%%
%% Check and radio menuitems are specified as {Name, N, check|radio}.
%% They are assumed to be children to a cascade menuitem! (And all children
%% to one cascade menuitem are assumed to be either check OR radio
%% menuitems)!
%% Selecting a check/radio menuitem generates the event:
%%   {gs, _Id, click, {menu, Menu}, _Arg}
%% where Menu is the name of the parent, the cascade menuitem.
%% Use selected(Menu) to retrieve which check/radio menuitems are
%% selected.
%%--------------------------------------------------------------------
create_menus(MenuBar, [{Title, Items}|Menus]) ->
    Title2 = " "++(atom_to_list(Title))++" ",
    MenuBtn = gs:menubutton(MenuBar, [{label, {text,Title2}}]),
    case Title of
	'Help' -> gs:config(MenuBtn, {side, right});
	_ -> ignore
    end,
    Menu = gs:menu(Title, MenuBtn, []),
    create_items(Menu, Items, Title),
    create_menus(MenuBar, Menus);
create_menus(_MenuBar, []) ->
    done.

create_items(Menu, [Item|Items], Group) ->
    create_item(Menu, Item, Group),
    create_items(Menu, Items, Group);
create_items(_Menu, [], _Group) ->
    done.

create_item(Menu, {Name, _N, cascade, Items}, _Group) ->
    MenuBtn = gs:menuitem(Menu, [{label, {text,Name}},
				 {itemtype, cascade}]),
    SubMenu = gs:menu(Name, MenuBtn, []),
    create_items(SubMenu, Items, Name);
create_item(Menu, separator, _Group) ->
    gs:menuitem(Menu, [{itemtype, separator}]);
create_item(Menu, MenuItem, Group) ->
    Options = case MenuItem of
		  {Name, N} ->
		      [{data, {menuitem,Name}}];
		  {Name, N, check} ->
		      [{itemtype, check}, {data, {menu, Group}}];
		  {Name, N, radio} ->
		      [{itemtype, radio}, {data, {menu, Group}},
		       {group, group(Group)}]
	      end,
    gs:menuitem(Name, Menu, [{label, {text,Name}} | Options]),
    if
	integer(N) -> gs:config(Name, {underline, N});
	true -> ignore
    end.

%% When grouping radio buttons, the group id must be an atom unique for
%% each window.
group(Group) ->
    list_to_atom(atom_to_list(Group)++pid_to_list(self())).

%%--------------------------------------------------------------------
%% select(MenuItem, Bool)
%%   MenuItem = atom()
%%   Bool = boolean()
%%--------------------------------------------------------------------
select(MenuItem, Bool) ->
    gs:config(MenuItem, {select, Bool}).

%%--------------------------------------------------------------------
%% selected(Menu) -> [Name]
%%   Menu = Name = atom()
%%--------------------------------------------------------------------
selected(Menu) ->
    Children = gs:read(Menu, children),
    Selected = lists:filter(fun(Child) -> gs:read(Child, select) end,
			    Children),
    lists:map(fun(Child) ->
		      {text, Name} = gs:read(Child, label),
		      list_to_atom(Name)
	      end,
	      Selected).

%%--------------------------------------------------------------------
%% add_break(Name, Point) -> #break{}
%%   Name = atom()
%%   Point = {Mod, Line}
%% The break will generate the following events:
%%   {gs, _Id, click, {break, Point, Event}, _Arg}
%%     Event = delete | {trigger, Action} | {status, Status}
%%       Action = enable | disable | delete
%%       Status = active | inactive
%%--------------------------------------------------------------------
add_break(Menu, Point) ->

    %% Create a name for the breakpoint
    {Mod, Line} = Point,
    Label = io_lib:format("~w ~5w", [Mod, Line]),

    %% Create a menu for the breakpoint
    MenuBtn = gs:menuitem(Menu, [{label, {text,Label}},
				 {itemtype, cascade}]),
    SubMenu = gs:menu(MenuBtn, []),
    SMI = gs:menuitem(SubMenu, [{data, {break,Point,null}}]),
    gs:menuitem(SubMenu, [{label, {text,"Delete"}},
			  {data, {break,Point,delete}}]),
    TriggerMenuBtn = gs:menuitem(SubMenu, [{label,{text,"Trigger Action"}},
					   {itemtype, cascade}]),
    TriggerMenu = gs:menu(TriggerMenuBtn, []),
    Group = element(3, erlang:now()),
    EMI = gs:menuitem(TriggerMenu, [{label, {text,"Enable"}},
				    {itemtype, radio}, {group, Group},
				    {data,
				     {break,Point,{trigger,enable}}}]),
    DiMI = gs:menuitem(TriggerMenu, [{label, {text,"Disable"}},
				     {itemtype, radio}, {group, Group},
				     {data,
				      {break,Point,{trigger,disable}}}]),
    DeMI = gs:menuitem(TriggerMenu, [{label, {text,"Delete"}},
				     {itemtype, radio}, {group, Group},
				     {data,
				      {break,Point,{trigger,delete}}}]),

    #break{mb=MenuBtn, smi=SMI, emi=EMI, dimi=DiMI, demi=DeMI}.

%%--------------------------------------------------------------------
%% update_break(Break, Options)
%%   Break = #break{}
%%   Options = [Status, Action, Mods, Cond]
%%     Status = active | inactive
%%     Action = enable | disable | delete
%%     Mods = null (not used)
%%     Cond = null | {Mod, Func}
%%--------------------------------------------------------------------
update_break(Break, Options) ->
    [Status, Trigger|_] = Options,
    {break, Point, _Status} = gs:read(Break#break.smi, data),
    
    {Label, Data} = case Status of
			active ->
			    {"Disable", {break,Point,{status,inactive}}};
			inactive ->
			    {"Enable", {break,Point,{status,active}}}
		    end,
    gs:config(Break#break.smi, [{label, {text,Label}}, {data, Data}]),

    TriggerMI = case Trigger of
		    enable -> Break#break.emi;
		    disable -> Break#break.dimi;
		    delete -> Break#break.demi
		end,
    gs:config(TriggerMI, {select, true}).

%%--------------------------------------------------------------------
%% delete_break(Break)
%%   Break = #break{}
%%--------------------------------------------------------------------
delete_break(Break) ->
    gs:destroy(Break#break.mb).

%%--------------------------------------------------------------------
%% motion(X, Y) -> {X, Y}
%%   X = Y = integer()
%%--------------------------------------------------------------------
motion(X, Y) ->
    receive
	{gs, _Id, motion, _Data, [NX,NY]} ->
	    motion(NX, NY)
    after 0 ->
	    {X, Y}
    end.
