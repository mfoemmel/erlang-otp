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
-module(dbg_ui_break_win).

%% External exports
-export([create_win/5,
	 update_functions/2,
	 handle_event/2]).

-record(winInfo, {type,            % line | conditional | function
		  packer,          % gsobj() | undefined
		  entries,         % [{atom|integer, GSobj()}]
		  trigger,         % enable | disable | delete
		  ok,              % gsobj()
		  cancel,          % gsobj()
		  listbox,         % gsobj()
		  funcs=[]         % [[Name, Arity]]
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Pos, Type, Mod, Line) -> #winInfo{}
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Type =  line | conditional | function
%%   Mod = atom() | ""
%%   Line = integer() | ""
%%--------------------------------------------------------------------
create_win(GS, {X, Y}, function, Mod, _Line) ->
    Pad = 10,
    Wbtn = 70, Hbtn = 30,

    Title = "Function Break",
		
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y},
			 {destroy, true}, {configure, true}]),
    Frm = gs:frame(Win, [{x, 0}, {y, 0}, {width, 230}, {height, 190},
			 {packer_x, [{fixed, 70}, {stretch, 1, 150},
				     {fixed, 10}]},
			 {packer_y, [{fixed, 10}, {fixed, 30},
				     {stretch, 1, 100}, {fixed, 50}]}]),

    %% Create module input field
    gs:label(Frm, [{label, {text,"Module"}}, {align, e},
		   {pack_x, 1}, {pack_y, 2}]),
    Ent = gs:entry(Frm, [{text, Mod},
			 {pack_x, 2}, {pack_y, 2},
			 {keypress, true}, {setfocus, true}]),
    Entries = [{Ent, atom}],

    %% Create a listbox containing the functions of the module
    gs:label(Frm, [{label, {text,"Function"}}, {align, e},
		   {pack_x, 1}, {pack_y, 3}]),
    Lb = gs:listbox(Frm, [{bw, 2}, {relief, ridge}, {vscroll, right},
			  {pack_x, 2}, {pack_y, 3}]),

    %% Add Ok and Cancel buttons
    Bot = gs:frame(Frm, [{pack_x, {1, 3}}, {pack_y, 4}]),
    Ok = gs:button(Bot, [{label, {text,"Ok"}},
			  {x, Pad}, {y, Pad},
			  {width, Wbtn}, {height, Hbtn}]),
    Cancel = gs:button(Bot, [{label, {text,"Cancel"}},
			     {x, 230-Pad-Wbtn}, {y, Pad},
			     {width, Wbtn}, {height, Hbtn}]),

    Wfrm = gs:read(Frm, width), Hfrm = gs:read(Frm, height),
    gs:config(Win, [{width, Wfrm}, {height, Hfrm}, {map, true}]),
    #winInfo{type=function,
	     packer=Frm, entries=Entries, trigger=enable,
	     ok=Ok, cancel=Cancel, listbox=Lb, funcs=[]};
create_win(GS, {X, Y}, Type, Mod, Line) ->
    Pad = 10,
    Wlbl = 80,
    Went = 150,
    Wbtn = 70, Hbtn = 30,

    Title = case Type of
		line -> "Line Break";
		conditional -> "Conditional Break"
	    end,
		
    Wwin = Pad+Wlbl+Went+Pad,
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y}, {width, Wwin},
			 {destroy, true}]),

    %% Create input fields (label+entry)
    Labels = case Type of
		 line ->
		     [{atom, "Module:", Mod}, {integer, "Line:", Line}];
		 conditional ->
		     [{atom, "Module:", Mod}, {integer, "Line:", Line},
		      {atom, "C-Module:", ""}, {atom, "C-Function:", ""}]
	     end,
    Fun = fun({DataType, Label, Default}, Yin) ->
		  Ent = gs:create(entry, Win, [{x, Pad+Wlbl}, {y, Yin},
					       {width, Went},
					       {text, Default},
					       {keypress, true}]),
		  Hent = gs:read(Ent, height),
		  gs:create(label, Win, [{label, {text,Label}}, {align, e},
					 {x, Pad}, {y, Yin},
					 {width, Wlbl}, {height, Hent}]),
		  {{Ent, DataType}, Yin+Hent}
	  end,
    {Entries, Yacc} = lists:mapfoldl(Fun, Pad, Labels),
    {First, _DataType} = hd(Entries),
    gs:config(First, {setfocus, true}),

    %% Add 'trigger action' buttons
    Wfrm = 100,
    Hfrm = 90,
    Frm = gs:frame(Win, [{bw, 2},
			 {x, Pad-2}, {y, Yacc+Pad-2},
			 {width, Wfrm+4}, {height, Hfrm+5}]),
    gs:label(Frm, [{label, {text,"Trigger Action"}},
		   {x, 0}, {y, 0}, {width, Wfrm}, {height, Hfrm/4}]),
    gs:radiobutton(Frm, [{label, {text, "Enable"}},
			 {x, 5}, {y, Hfrm/4}, {align, w},
			 {width, Wfrm-5}, {height, Hfrm/4},
			 {data, enable}, {group, g1}, {select, true}]),
    gs:radiobutton(Frm, [{label, {text, "Disable"}},
			 {x, 5}, {y, 2*Hfrm/4}, {align, w},
			 {width, Wfrm-5}, {height, Hfrm/4},
			 {data, disable}, {group, g1}]),
    gs:radiobutton(Frm, [{label, {text, "Delete"}},
			 {x, 5}, {y, 3*Hfrm/4}, {align, w},
			 {width, Wfrm-5}, {height, Hfrm/4},
			 {data, delete}, {group, g1}]),

    %% Add Ok and Cancel buttons
    Xbtn = Wwin-Pad-Wbtn, Ybtn = Yacc+Pad+Hfrm-Hbtn,
    Btn = gs:button(Win, [{label, {text,"Ok"}},
			  {x, Xbtn}, {y, Ybtn},
			  {width, Wbtn}, {height, Hbtn}]),
    gs:button(Win, [{label, {text,"Cancel"}},
		    {x, Xbtn}, {y, Ybtn-Pad-Hbtn},
		    {width, Wbtn}, {height, Hbtn}]),

    Hwin = Ybtn+Hbtn+Pad,
    gs:config(Win, [{height, Hwin}, {map, true}]),
    #winInfo{type=Type, entries=Entries, trigger=enable, ok=Btn}.

%%--------------------------------------------------------------------
%% update_functions(WinInfo, Funcs) -> WinInfo
%%   WinInfo = #winInfo{}
%%   Funcs = [{Name, Arity}]
%%     Name = atom()
%%     Arity = integer()
%%--------------------------------------------------------------------
update_functions(WinInfo, Funcs) ->
    Items = lists:map(fun([N, A]) -> io_lib:format("~p/~p", [N, A]) end,
		      Funcs),
    gs:config(WinInfo#winInfo.listbox, {items, Items}),
    WinInfo#winInfo{funcs=Funcs}.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%% GSEvent = {gs, Id, Event, Data, Arg}
%% WinInfo = #winInfo{}
%% Command = ignore
%%         | stopped
%%         | {win, WinInfo}
%%         | {module, Mod}
%%         | {break, [Mod, Line], Action}
%%         | {break, [Mod, Line, CMod, CFunc], Action}
%%         | {break, [Mod, Func, Arity], Action}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, configure, _Data, [W, H|_]}, WinInfo) ->
    gs:config(WinInfo#winInfo.packer, [{width, W-10}, {height, H-10}]),
    gs:config(WinInfo#winInfo.cancel, [{x, W-80}]),
    ignore;
handle_event({gs, Ent, keypress, Data, [Key|_]}, WinInfo) ->
    case WinInfo#winInfo.type of
	function when Key/='Tab', Key/='Return' ->
	    case gs:read(WinInfo#winInfo.listbox, items) of
		[] -> ignore;
		_Items ->
		    gs:config(WinInfo#winInfo.listbox, clear),
		    {win, WinInfo#winInfo{funcs=[]}}
	    end;
	function -> % 'Return' | 'Tab'
	    handle_event({gs, Ent, click, Data, ["Ok"]}, WinInfo);
	_Type when Key=='Tab'; Key=='Return' ->
	    case next_entry(Ent, WinInfo#winInfo.entries) of
		last ->
		    gs:config(WinInfo#winInfo.ok, flash),
		    handle_event({gs, Ent, click, Data, ["Ok"]}, WinInfo);
		Next ->
		    gs:config(Next, {setfocus, true}),
		    ignore
	    end;
	_Type -> ignore
    end;
handle_event({gs, _Id, click, _Data, ["Ok"|_]}, WinInfo) ->
    case check_input(WinInfo#winInfo.entries) of
	error -> ignore;
	Data when WinInfo#winInfo.type/=function ->
	    {break, Data, WinInfo#winInfo.trigger};
	[Mod] ->
	    case gs:read(WinInfo#winInfo.listbox, selection) of
		[Index] ->
		    Func = lists:nth(Index+1, WinInfo#winInfo.funcs),
		    {break, [Mod | Func], enable};
		[] ->
		    [{Ent, _}] = WinInfo#winInfo.entries,
		    gs:config(Ent, {setfocus, false}),
		    {module, Mod}
	    end
    end;
handle_event({gs, _Id, click, _Data, ["Cancel"|_]}, _WinInfo) ->
    stopped;
handle_event({gs, _Id, click, Trigger, _Arg}, WinInfo) ->
    {win, WinInfo#winInfo{trigger=Trigger}};
handle_event(_GSEvent, _WinInfo) ->
    ignore.

check_input(Entries) ->
    check_input(Entries, []).
check_input([{Entry, Type} | Entries], Data) ->
    Str = gs:read(Entry, text),
    case erl_scan:string(Str) of
	{ok, [{Type, _Line, Val}], _EndLine} ->
	    check_input(Entries, [Val|Data]);
	_Error -> error
    end;
check_input([], Data) -> lists:reverse(Data).

next_entry(Entry, [{Entry, _Type}]) ->
    last;
next_entry(Entry, [{Entry, _Type1}, {Next, _Type2}|_]) ->
    Next;
next_entry(Entry, [_|Entries]) ->
    next_entry(Entry, Entries).
