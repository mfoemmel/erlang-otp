%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	option.erl
%%  Module   :	option
%%  Purpose  :  
%%  Notes    : 
%%  History  :* 1996-07-18 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%
%% USES
%% gsb:config_toolbox
%% gsb_db:lookup
%% gsb_db:config

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(option).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.34 $').
%% exported functions
-export([stop/1,start/3, create_window/1, select_widgets/2,
	 lookup/2, read/2, list/1, select_toolbox/2,
	 unselect/1, show/1, config_widgets/4]).

%% editors
-export([edit_integer/2, edit_integer/3, edit_atom/2, edit_atom/3,
	 edit_string/2, edit_string/3, edit_tuple/2, edit_tuple/3,
	 edit_list/2, edit_list/3, edit_term/2, edit_term/3,
	 no_edit/2,no_edit/3]).


%% DB exports
-export([get_data/2, set_data/3,read/4]).

%% Gen_server exports
-export([handle_call/3, handle_info/2, terminate/2,init/1]).

%% apply exports
-export([check_atom_string/1, check_for_label/1,  sort_view_data/3,
	 always_true/1]).

-include("gsb.hrl").

-record(option, {db, gsb, grid, grids, focus_row, focus_objects, state, x, y,
		 gs_parent, gsb_db, mode}).
-define(ROWS, 30).
-define(COLWIDTH, [125, 175]).
-define(BUTTHEIGHT, 30).
-define(BUTTWIDTH, 150).
-define(ENTRYHEIGHT, 30).
-define(OPTHEIGHT, 425).
-define(OPTWIDTH, 300).
-define(UNFOCUSBG, {217,217,217}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Start/stop functions                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start(Gsb, GsParent, GsbDb) ->
  case gen_server:start_link(
	 option, {Gsb, GsParent, GsbDb}, []) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, option)
  end.

init({Gsb, GsParent, GsbDb}) ->
  DB = gen_db:start(option, self(), option),
  {Props, Events} = create_window(GsParent),
  {ok, #option{db = DB, gsb = Gsb, grid = Props,
	       focus_row = 1, focus_objects = [],
	       state = props, grids = {Props, Events},
	       gs_parent = GsParent, gsb_db = GsbDb, mode = single}}.

stop(Pid) ->
  gen_server:call(Pid, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

read(Pid, FileName) -> 
  gen_server:call(Pid, {read, FileName}, infinity).
lookup(Pid, Key) ->
  gen_server:call(Pid, {lookup, Key}, infinity).
select_widgets(Pid, Keys) ->
  gen_server:call(Pid, {select_widgets, Keys}, infinity).
select_toolbox(Pid, Data) ->
  gen_server:call(Pid, {select_toolbox, Data}, infinity).
unselect(Pid) ->
  gen_server:call(Pid, unselect, infinity).
show(Pid) ->
  gen_server:call(Pid, show, infinity).
config_widgets(Pid, Keys, Type, Config) ->
  gen_server:call(Pid, {config_widgets, Keys, Type, Config}, infinity).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% loop                                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% ____________________________________________________________________
%%
%%  loop(DB, Grid, FocusRow, Object, Status, Grids, {X,Y}))    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

handle_info({gs, Win, configure, Data, [X,Y,Width, Height|_]}, Record) ->
  %% Some smarter configuration, pleeeease
  gs:config(Win, [{height, ?OPTHEIGHT}, {width, ?OPTWIDTH}]),
  {noreply, Record};
handle_info({gs,props, click, {NewFrame, NewGrid}, _Other}, Record) ->
  gs:config(NewFrame, raise),
  NewFocusRow = set_entry_value(NewGrid, Record#option.focus_row),
  unset_focus_row(Record#option.grid, Record#option.focus_row,
		  Record#option.mode),
  set_focus_row(NewGrid, NewFocusRow, Record#option.mode),
  NewRecord = case Record#option.state of
		exports ->
		  Record#option{grid = NewGrid,
			  focus_row = NewFocusRow,
			  state = exports};
		Else ->
		  Record#option{grid = NewGrid,
			  focus_row = NewFocusRow,
			  state = props}
	      end,
  {noreply, NewRecord};
handle_info({gs,events, click, {NewFrame, NewGrid}, _Other}, Record) ->
  gs:config(NewFrame, raise),
  NewFocusRow = set_entry_value(NewGrid, Record#option.focus_row),
  unset_focus_row(Record#option.grid, Record#option.focus_row,
		  Record#option.mode),
  set_focus_row(NewGrid, NewFocusRow, Record#option.mode),
  NewRecord = case Record#option.state of
		exports ->
		  Record#option{grid = NewGrid,
			  focus_row = NewFocusRow,
			  state = exports};
		Else ->
		  Record#option{grid = NewGrid,
			  focus_row = NewFocusRow,
			  state = events}
	      end,
  {noreply, NewRecord};
handle_info({gs, entry_field, enter, Data, Other}, Record) ->
  gs:config(entry_field, {setfocus, true}),
  {noreply, Record};
handle_info({gs, entry_field, leave, Data, Other}, Record) ->
  gs:config(entry_field, {setfocus, false}),
  {noreply, Record};
handle_info({gs,entry_field,keypress,{ViewType,Keys,Option,V,{M,F,A}},['Return'|_]},Record)->
  DataString = gs:read(entry_field, text),
  case apply({M,F},[{to_erlang, DataString}, A]) of
    undefined ->
      L = length(DataString),
      gs:config(entry_field, {select, {0, L}}),
      {noreply, Record};
    NewValue ->
      Item = gs:read(Record#option.grid, {obj_at_row, Record#option.focus_row}),
      gs:config(Item, [{data, {ViewType, Keys, Option, NewValue, {M, F, A}}},
			{text, {2,DataString}}]),
      case Record#option.mode of
	toolbox ->
	  config_toolbox(Record#option.state,
			 Record#option.gsb,
			 Keys,
			 Option, NewValue);
	Else ->
	  config_them(Record#option.state,
		      Record#option.gsb,
		      Keys,
		      Record#option.focus_objects,
		      Option, NewValue)
      end,
      {noreply, Record}
  end;

handle_info({gs, entry_field, keypress, _Data, _Other}, Record) ->
    {noreply, Record};
handle_info({gs, option_window, buttonpress, Data, [_,X,Y|_]}, Record) ->
    {noreply, Record#option{x = X, y = Y}};
handle_info({gs,GridItem,click,{ViewType,Keys,Option,Value,{M,F,A}},[_,R|_]}, Record) ->
  case Record#option.focus_row of
    R ->
      gs:config(entry_field, {enter, false}),
      case apply({M, F}, [{change, Value}, A,
			  {Record#option.gs_parent,
			   Record#option.x, Record#option.y}]) of
	undefined ->
	  gs:config(entry_field, [{setfocus, true}, {enter, true}]),
	  {noreply, Record};
	{NewValue, NewString} ->
	  gs:config(GridItem, [{data,{ViewType,Keys,Option,NewValue,{M,F,A}}},
			       {text, {2,NewString}}]),
	  gs:config(entry_field, [{data,{ViewType,Keys,Option,NewValue,{M,F,A}}},
				  {text,NewString},
				  {setfocus, true}, {enter, true}]),
	  case Record#option.mode of
	    toolbox ->
	      config_toolbox(Record#option.state,
			     Record#option.gsb,
			     Record#option.focus_objects,
			     Option, NewValue);
	    Else ->
	      config_them(Record#option.state,
			  Record#option.gsb,
			  Keys,
			  Record#option.focus_objects,
			  Option, NewValue)
	  end,
	  {noreply, Record}
      end;
    _Else ->
      unset_focus_row(Record#option.grid, Record#option.focus_row,
		     Record#option.mode),
      NewFocusRow = set_entry_value(Record#option.grid, R),
      set_focus_row(Record#option.grid, NewFocusRow, Record#option.mode),
      {noreply, Record#option{focus_row = NewFocusRow}}
  end;

handle_info({'EXIT', From, Reason}, Record) ->
  messages:debug("EXIT from ~p reason ~p", [From, Reason],
		 option, handle_info),
  {stop, error}.

handle_call({read, FileName}, From, Record) ->
  gen_db:read(Record#option.db, undefined, FileName),
  {reply, true,  Record};
handle_call({lookup, Key}, From, Record) ->
  R = gen_db:lookup(Record#option.db, Key, editor),
  {reply, R, Record};
handle_call({select_toolbox,{Key,Module,Exp,Props,Events}},From,Record) ->
  State = 
    case Module of
      none ->
	create_items(element(1, Record#option.grids),
		     Record#option.db,
		     lists:keysort(1,Props),
		     [Key],normal),
	create_items(element(2, Record#option.grids),
		     Record#option.db,
		     lists:keysort(1, Events),[Key],normal),
	Record#option.state;
      Else ->
	create_toolbox_soft_items(element(1, Record#option.grids),
				  Record#option.db,
				  Exp,
				  [Key],normal),
	clear_items(element(2, Record#option.grids), 1),
	exports
    end,
  set_title(Key, toolbox),
  NewFocusRow = set_entry_value(Record#option.grid,
				Record#option.focus_row),
  set_focus_row(Record#option.grid, NewFocusRow,
		single), 
  {reply, true, Record#option{focus_objects = [Key],
			      focus_row = NewFocusRow,
			      state = State,
			      mode = toolbox}};
handle_call({select_widgets, [Key]}, From, Record) ->
  Data = gsb_db:lookup(Record#option.gsb_db, Key),
  State =
    case Data#gsb_db.type of
      hard ->
	create_items(
	  element(1, Record#option.grids),
	  Record#option.db,
	  lists:append(
	    [lists:keysort(1,lists:append([[{name, Data#gsb_db.name}],
					   Data#gsb_db.props])),
	     Data#gsb_db.exports]),
	  [Key],normal),
	create_items(element(2, Record#option.grids),
		     Record#option.db,
		     lists:keysort(1, Data#gsb_db.events),[Key],normal),
	Record#option.state;
      soft ->
	SoftRunTimeData =
	  replace_names(Data#gsb_db.exports,
			[Key], Record#option.gsb_db, []),
	create_soft_items(element(1, Record#option.grids),
			  Record#option.db,
			  Data#gsb_db.exports,
			  SoftRunTimeData, normal),
	clear_items(element(2, Record#option.grids), 1),
	exports
    end,
  set_title(Data#gsb_db.gs_type, Data#gsb_db.name),
  NewFocusRow = set_entry_value(Record#option.grid,
				Record#option.focus_row),
  set_focus_row(Record#option.grid, NewFocusRow,
		single), 
  {reply, true, Record#option{focus_objects = [Key],
			      focus_row = NewFocusRow,
			      mode = single,
			      state = State}};
handle_call({select_widgets, Keys}, From, Record) ->
  AllExports = lookup_all(Record#option.gsb_db, Keys, exports, []),
  State =
    case all_is_soft_widgets(Keys, Record#option.gsb_db) of
      false ->
	case someone_is_a_soft_widget(Keys, Record#option.gsb_db) of
	  true ->
	    clear_items(element(1, Record#option.grids), 1),
	    clear_items(element(2, Record#option.grids), 1),
	    exports;
	  false ->
	    %% Not a soft widget
	    AllProps = lookup_all(Record#option.gsb_db, Keys, props, []),
	    AllEvents = lookup_all(Record#option.gsb_db, Keys, events, []),
	    PropData = sort_view_data(AllProps, AllProps,[]),
	    EventData = sort_view_data(AllEvents, AllEvents, []), 
	    ExportData = sort_view_data(AllExports, AllExports, []),
	    create_items(
	      element(1, Record#option.grids),
	      Record#option.db,
	      lists:keysort(2, PropData),Keys,special),
	    create_items(element(2, Record#option.grids),
			 Record#option.db,
			 lists:keysort(2, EventData),Keys, special),
	    Record#option.state
	end;
      _true ->
	RuntimeData = sort_view_data(AllExports, AllExports, []),
	SoftRunTimeData = replace_names(RuntimeData,
					Keys,
					Record#option.gsb_db, []),
	create_soft_items(element(1, Record#option.grids),
			  Record#option.db,
			  RuntimeData,
			  SoftRunTimeData, special),
	clear_items(element(2, Record#option.grids),1),
	exports
    end,
  NewFocusRow = set_entry_value(Record#option.grid,
				Record#option.focus_row),
  set_focus_row(Record#option.grid, NewFocusRow,
		multiple), 
  {reply, true, Record#option{focus_objects = Keys, focus_row = NewFocusRow,
			      mode = multiple, state=State}};
handle_call({config_widgets, Keys, Type, Config}, From, Record) ->
  CompareWith = Record#option.focus_objects,
  case Keys of
    CompareWith ->
      case Type of 
	name ->
	  lookup_and_change(element(1, Record#option.grids),Config);
	props ->
	  lookup_and_change(element(1, Record#option.grids),Config);
	events ->
	  lookup_and_change(element(2, Record#option.grids),Config)
      end;
    _ -> true
  end,
  {reply, true, Record};
handle_call(show, From, Record) ->
  gs:config(option_window, {iconify, false}),
  {reply, true, Record};
handle_call(unselect, From, Record) ->
  clear_items(element(1, Record#option.grids), 1),
  clear_items(element(2, Record#option.grids), 1),
  gs:config(entry_field, [{text, ""}, {data, undefined}]),
  {reply, true, Record#option{focus_row = 0,
			      focus_objects = undefined}};
handle_call(stop, From, Record) ->
  {stop, normal, true, Record}.

terminate(Reason, Record) ->
  gen_db:stop(Record#option.db),
  gs:destroy(option_window),
  ok.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Behaviour functions                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

read([], _Gsb, _U, _State) -> true;
read([{Name, Editor}|Rest], Gsb, U, State) ->
  gen_db:handle_call({insert,
		      #option_db{name = Name,
				 editor = Editor}},
		    U,
		    State),
  read(Rest, Gsb, U, State).

list(Type) ->
  case Type of
    all ->
      '$0'
  end.

set_data(_all, CurrData, []) ->
  CurrData;
set_data(name, CurrData, Data) ->
  CurrData#option_db{name = Data};
set_data(editor, CurrData, Data) ->
  CurrData#option_db{editor = Data}.

get_data(record, Data) ->
  Data;
get_data(name, Data) ->
  Data#option_db.name;
get_data(editor, Data) ->
  Data#option_db.editor.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

create_window(GS) ->
  GridOptions = [{x, 0}, {y, 0},
		 {columnwidths, ?COLWIDTH},
		 {width, ?OPTWIDTH},
		 {height, ?OPTHEIGHT - ?BUTTHEIGHT - ?ENTRYHEIGHT},
		 {rows, {1, ?ROWS}},
		 {bg, ?UNFOCUSBG},
		 {hscroll, false},
		 {fg,black}],
  gs:create(window,option_window,GS,[{title, "option"},
				     {width, ?OPTWIDTH},
				     {height, ?OPTHEIGHT},
				     {map, false},
				     {buttonpress, true}]),
  gs:create(entry, entry_field, option_window,
	    [{x, 0}, {y, ?BUTTHEIGHT},
	     {width, 300}, {height, ?ENTRYHEIGHT},
	     {keypress, true}, {setfocus, true},
	     {leave, true}, {enter, true}]),
  gs:create(frame,  events_frame, option_window,
			  [{x, 0}, {y, ?BUTTHEIGHT+?ENTRYHEIGHT},
			   {width, ?OPTWIDTH},
			   {height, ?OPTHEIGHT - ?BUTTHEIGHT - ?ENTRYHEIGHT}]),
  Events = gs:create(grid, events_frame, GridOptions),
  gs:create(frame, props_frame, option_window,
			  [{x, 0}, {y, ?BUTTHEIGHT+?ENTRYHEIGHT},
			   {width, ?OPTWIDTH},
			   {height, ?OPTHEIGHT - ?BUTTHEIGHT - ?ENTRYHEIGHT}]),
  Props = gs:create(grid, props_frame, GridOptions),
  gs:create(radiobutton, props, option_window,
	    [{width, ?BUTTWIDTH}, {height, ?BUTTHEIGHT},
	     {relief, raised}, {select, true},
	     {data, {props_frame, Props}}, {group, radio1},
	     {label, {text, "Properties"}}]),
  gs:create(radiobutton, events, option_window,
	    [{width, ?BUTTWIDTH}, {height, ?BUTTHEIGHT},
	     {relief, raised},
	     {x, ?BUTTWIDTH}, {y,0}, {align, center},{group, radio1},
	     {data, {events_frame, Events}}, {label, {text, "Events"}}]),
  %% This is not good, but gs is slow on grids, so we have to delay
  %% the {map, true} before everything is created
  gs:create(menu, popup, option_window, []),
  %%  gs:read(Events, {row, ?ROWS}),
  gs:config(option_window, [{map, true}, {configure, true}]),
  {Props, Events}.

%% ____________________________________________________________________
%%
%%  set_title(Atom)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________


set_title(Key, Name) ->
  gs:config(option_window,
	    {title, lists:append(["option - ",
				  atom_to_list(Name),
				  ":",
				  atom_to_list(Key)])}).

%% ____________________________________________________________________
%%
%%  create_items(G, L)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

create_soft_items(Grid, DB, List, KeyList, Mode) ->
  Row = create_soft_items(Grid, DB, List, KeyList, 1, Mode),
  clear_items(Grid, Row).

create_soft_items(Grid, DB, [Tuple|TRest], [InKeys|IRest], Row, Mode) ->
  {Type,ViewType,Keys,Option,Value}=format_tuple(Mode,Tuple,InKeys),
  {M,F,A} =
    case Type of
      gs_type ->
	Data = gen_db:lookup(DB, element(2,Option)),
	Data#option_db.editor;
      user_type ->
	{option,edit_term,[]}
    end,
  ValueList = apply(M, F, [{to_list, Value}, A]),
  TextList =
    case Type of
      gs_type ->
	lists:append(["{",
		     atom_to_list(element(1,Option)),
		     ",",
		     atom_to_list(element(2,Option)),
		     "}"]);
      user_type -> atom_to_list(Option)
    end,
  DataField = {data,{ViewType, Keys,Option ,Value,{M,F,A}}},
  case gs:read(Grid, {obj_at_row, Row}) of
    undefined ->
      Item=gs:create(gridline,Grid, [{click,true},DataField,{font,{screen,12}},
				     {row, Row}, {text,{1,TextList}},
				     {text,{2,ValueList}}]),
      case ViewType of
	different ->
	  gs:config(Item, {font, {screen, [bold, italic],12}});
	_ -> true
      end;
    GridItem ->
      gs:config(GridItem, [{click, true},DataField,{font, {screen,12}},
			   {text, {1,TextList}}, {text, {2,ValueList}}]),
      case ViewType of
	different ->
	  gs:config(GridItem, {font, {screen, [bold, italic],12}});
	_ -> true
      end
  end,
  create_soft_items(Grid, DB, TRest, IRest, Row + 1, Mode);
create_soft_items(Grid, DB, [], [], Row, Mode) -> Row.

create_items(Grid, DB, List, Keys, Mode) ->
  Row = create_items(Grid, DB, List, Keys, 1, Mode),
  clear_items(Grid, Row).

create_items(Grid, DB, [Tuple|Rest], Keys, Row, Mode) ->
  {ViewType,Option,Value} = format_tuple(Mode, Tuple),
  Data = gen_db:lookup(DB, Option),
  {M, F, A} = Data#option_db.editor,
  ValueList = apply(M, F, [{to_list, Value}, A]),
  KeyList = atom_to_list(Option),
  DataField = {data, {ViewType, Keys, Option, Value, {M, F, A}}},
  GI = case gs:read(Grid, {obj_at_row, Row}) of
	 undefined ->
	   GridItem=gs:create(gridline,Grid, [{row,Row},{text,{1,KeyList}},
					      {text, {2, ValueList}},{click,true},
					      DataField]),
	   case ViewType of
	     same -> 
	       gs:config(GridItem, {font, {screen, 12}});
	     different ->
	       gs:config(GridItem, {font, {screen, 12}})
	   end,
	   GridItem;
	 GridItem ->
	   gs:config(GridItem, [DataField,{text,{1,KeyList}},
				{text, {2,ValueList}},{click, true},
				{font,{screen, 12}}]),
	   case ViewType of
	     different ->
	       gs:config(GridItem, {font, {screen, [bold, italic],12}});
	     _ -> true
	   end,
	   GridItem
       end,
  create_items(Grid, DB, Rest, Keys, Row + 1, Mode);
create_items(Grid, DB, [], Keys, Row, Mode) -> Row.

create_toolbox_soft_items(Grid, DB, List, Keys, Mode) ->
  Row = create_toolbox_soft_items(Grid, DB, List, Keys, 1, Mode),
  clear_items(Grid, Row).

create_toolbox_soft_items(Grid, DB, [Tuple|Rest], Keys, Row, Mode) ->
  {ViewType,Type,Option,Value}=format_soft_toolbox_tuple(Mode, Tuple),
  {DataField,{M,F,A}} =
    case Type of
      gs_type ->
	Data = gen_db:lookup(DB, element(2,Option)),
	{{data, {ViewType, Keys, Option, Value, Data#option_db.editor}},
	 Data#option_db.editor};
      user_type ->
	{{data, {ViewType, Keys, Option, Value, {option,edit_term,[]}}},
	 {option, edit_term, []}}
    end,
  ValueList = apply(M, F, [{to_list, Value}, A]),
  TextList =
    case Type of
      gs_type ->
	lists:append(["{",
		     atom_to_list(element(1,Option)),
		     ",",
		     atom_to_list(element(2,Option)),
		     "}"]);
      user_type -> atom_to_list(Option)
    end,
  GI = case gs:read(Grid, {obj_at_row, Row}) of
	 undefined ->
	   GridItem=gs:create(gridline,Grid, [{click,true},DataField,
					      {font, {screen, 12}},
					      {row,Row},{text,{1,TextList}},
					      {text, {2, ValueList}}]),
	   case ViewType of
	     different ->
	       gs:config(GridItem, {font, {screen, 12}});
	     _ -> true
	   end,
	   GridItem;
	 GridItem ->
	   gs:config(GridItem, [DataField,{text,{1,TextList}},
				{text, {2,ValueList}},{click, true},
				{font,{screen, 12}}]),
	   case ViewType of
	     different ->
	       gs:config(GridItem, {font, {screen, [bold, italic],12}});
	     _ -> true
	   end,
	   GridItem
       end,
  create_toolbox_soft_items(Grid, DB, Rest, Keys, Row + 1, Mode);
create_toolbox_soft_items(Grid, DB, [], Keys, Row, Mode) -> Row.

clear_items(G, ?ROWS) -> true;
clear_items(G, Row) ->
  case gs:read(G, {obj_at_row, Row}) of
    undefined ->
      true;
    Item ->
      gs:config(Item,[{bg,?UNFOCUSBG},{click,false},{click,false},
		      {text, ""},{data, undefined}]),
      clear_items(G, Row + 1)
  end.

%% ____________________________________________________________________
%%
%%  format_tuple(Mode, Tuple).    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

format_tuple(special, Tuple, InKeys) ->
  case Tuple of
    {AView, {AName, AOption}, AValue} ->
      {gs_type,AView, InKeys, {AName,AOption}, AValue};
    {AView, AOption, AValue} ->
      {user_type,AView, InKeys, AOption, AValue}
  end;
format_tuple(normal, Tuple, InKeys) ->
  case Tuple of
    {{AName,AOption}, AValue} ->
      {gs_type,same, InKeys, {AName,AOption}, AValue};
    {AOption, AValue} ->
      {user_type,same, InKeys, AOption, AValue}
  end.

format_tuple(normal, {AOption, AValue}) ->
  {same, AOption, AValue};
format_tuple(special, Tuple) ->
  Tuple.

format_soft_toolbox_tuple(normal, {{AName, AOption},AValue}) ->
  {same, gs_type, {AName,AOption}, AValue};
format_soft_toolbox_tuple(normal, {AOption, AValue}) ->
  {same, user_type, AOption, AValue};
format_soft_toolbox_tuple(special, {AView, {AName, AOption},AValue}) ->
  {AView, gs_type, {AName,AOption}, AValue};
format_soft_toolbox_tuple(special, {AView, {AOption, AValue}}) ->
  {AView, user_type, AOption, AValue}.


%% ____________________________________________________________________
%%
%%  set_focus_row    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

set_focus_row(Grid, Row, Mode) ->
  case gs:read(Grid, {obj_at_row, Row}) of
    undefined ->
      true;
    Item ->
      gs:config(Item, {bg, white})
  end.

%% ____________________________________________________________________
%%
%%  unset_focus_row    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

unset_focus_row(Grid, Row, Mode) ->
  case gs:read(Grid, {obj_at_row, Row}) of
    undefined ->
      true;
    Item ->
      gs:config(Item, {bg, ?UNFOCUSBG})
  end.

%% ____________________________________________________________________
%%
%%  set_entry_value    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

set_entry_value(Grid, 0) ->
  %% undefined setting of FocusRow, try to set row 1
  set_entry_value(Grid, 1);
set_entry_value(Grid, 1) ->
  case gs:read(Grid, {obj_at_row,1}) of
    undefined ->
      %% There is no item at position 1, empty entry field
      gs:config(entry_field, [{text, ""}, {data, undefined}]),
      0;
    GridItem ->
      case gs:read(GridItem, data) of
	undefined ->
	  gs:config(entry_field, [{text, ""},
				  {data, undefined}]),
	  1;
	Data ->
	  ValueList = gs:read(GridItem, {text,2}),
	  gs:config(entry_field, [{text, ValueList},
				  {data, Data}]),
	  1
      end
  end;
set_entry_value(Grid, Row) ->
  case gs:read(Grid, {obj_at_row, Row}) of
    undefined ->
      set_entry_value(Grid, Row - 1);
    GridItem ->
      case gs:read(GridItem, data) of
	undefined ->
	  gs:config(entry_field, [{text, ""},
				  {data, undefined}]),
	  set_entry_value(Grid, Row - 1);
	Data ->
	  ValueList = gs:read(GridItem, {text,2}),
	  gs:config(entry_field, [{text, ValueList},
				  {data, Data}]),
	  Row
      end
  end.

%% ____________________________________________________________________
%%
%%  lookup_and_change(Grid, List)    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

lookup_and_change(Grid, [First|Rest]) ->
  locate_row(Grid, First, 1),
  lookup_and_change(Grid, Rest);
lookup_and_change(Grid, {Option, Value}) ->
  locate_row(Grid, {Option, Value}, 1);
lookup_and_change(Grid, []) -> true.

locate_row(Grid, {Option, Value}, Row) ->
  GridLine = gs:read(Grid, {obj_at_row, Row}),
  case gs:read(GridLine, data) of
    {VT, Keys, Option,OldValue,{M,F,A}} ->
      ValueList = apply({M,F},[{to_list, Value}, A]),
      gs:config(GridLine, [{VT, Keys, Option, Value,{M,F,A}}]),
      gs:config(GridLine, {text, {2, ValueList}});
    _else ->
      locate_row(Grid, {Option, Value}, Row + 1)
  end.

replace_names([{Type, {Name, Option}, Value}|Rest], Keys, GSB_DB, Acc) ->
  RealKeys = gsb_db:lookup_keys_from_name(GSB_DB, Keys, Name),
  replace_names(Rest, Keys, GSB_DB, [RealKeys|Acc]);

replace_names([{{Name, Option}, Value}|Rest], Keys, GSB_DB, Acc) ->
  RealKeys = gsb_db:lookup_keys_from_name(GSB_DB, Keys, Name),
  replace_names(Rest, Keys, GSB_DB, [RealKeys|Acc]);

replace_names([Other|Rest], Keys, GSB_DB, Acc)->
  replace_names(Rest, Keys, GSB_DB, [Keys|Acc]);

replace_names([], Keys, GSB_DB, Acc) -> lists:reverse(Acc).


all_is_soft_widgets([Key|Rest], DB) ->
  case gsb_db:lookup(DB, Key, type) of
    soft -> all_is_soft_widgets(Rest, DB);
    hard -> false    
  end;
all_is_soft_widgets([], DB) -> true.

someone_is_a_soft_widget([Key|Rest], DB) ->
  case gsb_db:lookup(DB, Key, type) of
    soft -> true;
    hard -> someone_is_a_soft_widget(Rest, DB)
  end;
someone_is_a_soft_widget([], DB) -> false.

lookup_all(DB, [], Type, Acc) -> Acc;
lookup_all(DB, [Key|Rest], Type, Acc) ->
  Data = gsb_db:lookup(DB, Key, Type),
  lookup_all(DB, Rest, Type, [Data|Acc]).

sort_view_data([First|Rest], Ref, Acc) ->
  %% Sufficient to only check first list of data!
  in_all(First, Ref, []);
sort_view_data([], Ref, Acc) -> Acc.

in_all([{{Name, Option}, Value}|Rest], Ref, Acc) ->
  case check_all_refs_and_value({{Name, Option}, Value}, Ref) of
    true ->
      in_all(Rest, Ref, [{same, {Name, Option}, Value}|Acc]);
    false ->
      case check_all_refs({{Name, Option}, Value}, Ref) of
	true ->
	  in_all(Rest, Ref, [{different, {Name, Option}, Value}|Acc]);
	false ->
	  in_all(Rest, Ref, Acc)
      end
  end;
in_all([{Key, Value}|Rest], Ref, Acc) ->
  case check_all_refs_and_value({Key, Value}, Ref) of
    true ->
      in_all(Rest, Ref, [{same, Key, Value}|Acc]);
    false ->
      case check_all_refs({Key, Value}, Ref) of
	true ->
	  in_all(Rest, Ref, [{different, Key, Value}|Acc]);
	false ->
	  in_all(Rest, Ref, Acc)
      end
  end;
in_all([], Ref, Acc) -> Acc.

check_all_refs_and_value(Tuple, [First|Rest]) ->
  case lists:member(Tuple, First) of
    true -> check_all_refs_and_value(Tuple, Rest);
    false -> false
  end;
check_all_refs_and_value(Tuple, []) -> true.

check_all_refs(Tuple, [First|Rest]) ->
  case my_keymember(Tuple, First) of
    true -> check_all_refs(Tuple, Rest);
    false -> false
  end;
check_all_refs(Tuple, []) -> true.

my_keymember(_, []) -> false;
my_keymember({Key, _}, [{Key,_}|Rest]) -> true;
my_keymember(Tuple, [First|Rest]) ->
  my_keymember(Tuple, Rest).

config_toolbox(props, Gsb, Key, Type, NewValue) ->
  case Type of
    name ->
      gsb:config_toolbox(Gsb, Key, name, NewValue);
    Else ->
      gsb:config_toolbox(Gsb, Key, props, {Type, NewValue})
  end;
config_toolbox(exports, Gsb, Key, Type, NewValue) ->
  case Type of
    module ->
      gsb:config_toolbox(Gsb, Key, module, NewValue);
    Else ->
      gsb:config_toolbox(Gsb, Key, exports, {Type, NewValue})
  end;
config_toolbox(State, Gsb, Key, Type, NewValue) ->
  gsb:config_toolbox(Gsb, Key, State, {Type, NewValue}).

config_them(exports, Gsb, [Key], FocusObjects, Type, NewValue) ->
  %% First configure the objects
  case Type of
    {Name, OType} ->
      gsb:config_widgets(Gsb, [Key], props,{OType, NewValue});
    _ ->
      %% No gs option, do not configure props
      true
  end,
  %% Then configure export field
  gsb:config_widgets(Gsb,FocusObjects, exports,{Type, NewValue});
config_them(State, Gsb, Keys, FocusObjects, Type, NewValue) ->
  gsb:config_widgets(Gsb,Keys,State,{Type, NewValue}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% no_edit_atom                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

no_edit({to_list, _},_) ->
  undefined;
no_edit({to_erlang, _}, _) ->
  undefined.

no_edit({change, _}, Type, {GS, X, Y}) ->
  WarningText = lists:append([atom_to_list(Type), " not editable!"]), 
  messages:warning(GS, WarningText),
  undefined.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% list_editor                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

edit_list({to_list, Value}, List) ->
  case lists:member(Value, List) of
    true ->
      atom_to_list(Value);
    _else ->
      undefined
  end;
edit_list({to_erlang, Value}, List) ->
  Atom = list_to_atom(Value),
  case lists:member(Atom, List) of
    true ->
      Atom;
    _else ->
      undefined
  end.
edit_list({change, Value}, List, {GS, X, Y}) ->
  P = create_popup_items(List, []),
  gs:config(popup, {post_at, {X, Y}}),
  case list_rec(P) of
    undefined ->
      kill_popup_items(P),
      undefined;
    Result ->
      kill_popup_items(P),
      {Result, atom_to_list(Result)}
  end.

create_popup_items([], Acc) -> Acc;
create_popup_items([First|Rest], Acc) ->
  FirstList = atom_to_list(First),
  I = gs:create(menuitem, popup, [{label, {text, FirstList}},
				  {data, First}]),
  create_popup_items(Rest, [I|Acc]).

kill_popup_items([]) -> true;
kill_popup_items([First|Rest]) ->
  gs:destroy(First),
  kill_popup_items(Rest).

list_rec(P) ->
  receive
    {gs, I, click, Data, Other} ->
      case lists:member(I, P) of
	true ->
	  Data;
	_else ->
	  gs:config(entry_field, {setfocus, true}),
	  self()!{gs, I, click, Data, Other},
	  undefined
      end;
    Other ->
      self()!Other,
      undefined
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% integer_editor                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

edit_integer({to_list, Value}, _) ->
  integer_to_list(Value);
edit_integer({to_erlang, List}, _) ->
  convert_to_integer(List, 0).
edit_integer({change, Value}, List, _G) ->
  {Value, integer_to_list(Value)}.

convert_to_integer([], Acc) -> Acc;
convert_to_integer([First|Rest], Acc) ->
  Int = First - 48,
  if
    Int < 0 ->
      undefined;
    Int > 9 ->
      undefined;
    true ->
      convert_to_integer(Rest, 10 * Acc + Int)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% atom_editor                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

edit_atom({to_list, Value}, List) ->
  atom_to_list(Value);
edit_atom({to_erlang, Value}, List) ->
  list_to_atom(Value).
edit_atom({change, Value}, List, _G) ->
  {Value, atom_to_list(Value)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% tuple_editor                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% integer tuples
edit_tuple({to_list, Value}, {integer, _}) ->
  lists:flatten(io_lib:format("~w", [Value]));  
edit_tuple({to_erlang, String}, {integer, N}) ->
  parse_tuple(String, {check_for_integer, [N]});
%% label tuple
edit_tuple({to_list, {Type, []}}, label) ->
  lists:flatten(io_lib:format("{~w,[]}", [Type]));
edit_tuple({to_list, {Type, String}}, label) ->
  lists:flatten(io_lib:format("{~w,~s}", [Type, String]));
%% Strip "{text,
edit_tuple({to_erlang, [123, 116, 101, 120, 116, 44|R]}, label) ->
  S = strip_krull(R, []),
  {text, S};
%% Strip "{image,		      
edit_tuple({to_erlang, [123,105,109,97,103,101,44|R]}, label) ->
  S = strip_krull(R, []),
  {image, S};
edit_tuple({to_erlang, _else}, label) -> undefined.

edit_tuple({change, Value}, {integer, N}, _G) ->
  {Value, lists:flatten(io_lib:format("~w", [Value]))};
edit_tuple({change, {Type, []}}, label, _G) ->
  {{Type, []},
   lists:flatten(io_lib:format("{~w,[]}", [Type]))};
edit_tuple({change, {Type, String}}, label, _G) ->
  {{Type, String},
   lists:flatten(io_lib:format("{~w,~s}", [Type, String]))}.

strip_krull([125], Acc) -> lists:reverse(Acc);
strip_krull([F|R], Acc) -> strip_krull(R, [F|Acc]).

parse_tuple(String, {F, A}) ->
  case erl_scan:string(lists:append([String, ". "])) of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
	{ok, Term} ->
	  case apply({option, F}, [Term|A]) of
	    true -> Term;
	    false -> undefined
	  end;
	{error, _} -> undefined
      end;
    {error, _} -> undefined
  end.

check_atom_string({Atom, List}) when atom(Atom), list(List) -> true;
check_atom_string(_else) -> false.

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

check_for_label({label, {text, List}}) when list(List) -> true;
check_for_label({label, {image, List}}) when list(List) -> true;
check_for_label(_Other) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% list_editor                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

edit_string({to_list, []}, _) ->
  "[]";
edit_string({to_list, Value},_) ->
  Value;
edit_string({to_erlang, String},_) ->
  String.
edit_string({change, Value}, _Empty, _G) ->
  {Value, Value}.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% term_editor                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

edit_term({to_list, Value}, _) ->
  lists:flatten(io_lib:format("~w", [Value]));  
edit_term({to_erlang, String}, _) ->
  parse_tuple(String, {always_true, []}).
edit_term({change, Value}, _, _G) ->
  {Value, lists:flatten(io_lib:format("~w", [Value]))}.

always_true(_Term) -> true.

