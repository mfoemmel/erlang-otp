%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gsb_db.erl
%%  Module   :	gsb_db
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-08-27 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%
%% start(Gsb) -> Pid, start the gsb_db process and return pid.
%% stop(Pid) -> true, stop the gsb_db process
%%
%% create(Pid,Parent, Record) -> Record. create according to Record with Parent.
%% lookup(Pid, Key) -> Record, lookup whole record for Key.
%% lookup(Pid, Key, Type) -> Data, lookup up part of Record according to Type.
%% delete(Pid, Key) -> true, delete Key and all children.
%% list(Pid, Type) -> true, list parts of db according to Type
%% copy(Pid, Key, NewParent) -> Record, copy Key to NewParent, return a creation
%% record. 
%% write(Pid, TopKey, FileName) -> true, write tree of TopKey to FileName.
%% read(Pid, FileName) -> true, read FileName into database.

%% NEW
%% create_top(Pid,ToolboxRecord,Props) ->{File,CreateRecord}
%% config(Pid,Key,Type, {Option, Value})
%% config_widgets(Pid, Keys, Type, NewValue)
%% create(Pid,Parent,Toolbox,Record,Props) -> CreateRecord
%% write_as_template(Pid, FileName, ListOfKeys) -> true
%% USES
%% gsb:command(Gsb, {toolbox, get_new_key}, [GsType]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gsb_db).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.33 $').
-include("gsb.hrl").
-export([start/2, init/1, create/4, create_top/3, lookup/2, lookup/3,
	 config/4,delete/2, list/2, copy/4, write/3, read/2,
	 write_as_template/6,lookup_keys_from_name/3, config_widgets/4,
	 list/1, read/4, stop/1, make_filename/1,
	  handle_call/3, terminate/2]).
-export([lookup_write_data/3, set_data/3, get_data/2]).
%-import([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start(Gsb, Tool) ->
  case gen_server:start_link(
	 gsb_db, {Gsb, Tool}, []) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, gsb_db)
  end.

init({Gsb, Tool}) ->
  DB = gen_db:start(gsb_db, self(), gsb_db),
  {ok, {DB, Gsb, Tool}}.


stop(Pid) ->
  gen_server:call(Pid, stop).
create(Pid, Parent, ToolboxRecord, Props) ->
  gen_server:call(Pid, {create, Parent, ToolboxRecord, Props}).
create_top(Pid, ToolboxRecord, Props) ->
  gen_server:call(Pid, {create_top, ToolboxRecord, Props}).
copy(Pid, Key, NewParent, Props) ->
  gen_server:call(Pid, {copy, Key, NewParent, Props}).
lookup(Pid, Key) ->
  gen_server:call(Pid, {lookup, Key}).
lookup(Pid, Key, Type) ->
  gen_server:call(Pid, {lookup, Key, Type}).
config(Pid, Key, Type, Data) ->
  gen_server:call(Pid, {config, Key, Type, Data}).
config_widgets(Pid, Keys, Type, Data) ->
  gen_server:call(Pid, {config_widgets, Keys, Type, Data}).
delete(Pid, Key) ->
  gen_server:call(Pid, {delete, Key}).
list(Pid, TypeOfListing) ->
  gen_server:call(Pid, {list, TypeOfListing}).
write(Pid, Keys, FileName) ->
  gen_server:call(Pid, {write, Keys, FileName}).
read(Pid, FileName) -> 
  gen_server:call(Pid, {read, FileName}).
write_as_template(Pid, FileName, Key, Module, Name, Exports) ->
  gen_server:call(Pid, {write_as_template, FileName, Key,
			Module, Name, Exports}).
lookup_keys_from_name(Pid, Keys, Name) ->
  gen_server:call(Pid, {lookup_k_f_n, Keys, Name}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% loop                                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

handle_call({create, Parent, ToolboxRecord, Props}, From, {DB, Gsb, Tool}) ->
  Result = create_i(Parent, ToolboxRecord, Props, DB),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({create_top, ToolboxRecord, Props}, From, {DB, Gsb, Tool}) ->
  Result = create_top_i(ToolboxRecord, Props, DB),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({copy, Key, NewParent, Props}, From, {DB, Gsb, Tool}) ->
  Result = copy_i(Key, NewParent, Props, Tool, DB),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({lookup, Key}, From, {DB, Gsb, Tool}) ->
  Result = gen_db:lookup(DB, Key),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({lookup, Key, Type}, From, {DB, Gsb, Tool}) ->
  Result = gen_db:lookup(DB, Key, Type),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({config, Key, Type, Data}, From, {DB, Gsb, Tool}) ->
  Result = gen_db:config(DB, Key, Type, Data),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({config_widgets, Keys, Type, Data}, From, {DB, Gsb, Tool}) ->
  Result = config_all_widgets(DB, Keys, Type, Data),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({delete, Key}, From, {DB, Gsb, Tool}) ->
  Result = delete_i(DB, Key),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({list, TypeOfListing}, From, {DB, Gsb, Tool})->
  Result = gen_db:list(DB, TypeOfListing),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({write, Keys, FileName}, From, {DB, Gsb, Tool}) ->
  Result = gen_db:write(DB, Keys, FileName),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({write_as_template, FileName, Key, Module, Name, Exports},
	    From, {DB, Gsb, Tool}) ->
  List = create_toolbox_list(Key, DB, Module, Name, Exports, FileName),
  Result = gen_db:write_list(DB, List, FileName),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({read, FileName}, From, {DB, Gsb, Tool}) ->
  Result = gen_db:read(DB, Tool, FileName),
  {reply, Result, {DB, Gsb, Tool}};
handle_call({lookup_k_f_n, Keys, Name}, From, {DB, Gsb, Tool}) ->
  Result = lookup_k_f_n(DB, Keys, Name, []),
  {reply, Result, {DB, Gsb, Tool}};
handle_call(stop, From, {DB, Gsb, Tool}) ->
  {stop, normal, true, {DB, Gsb, Tool}}.

terminate(Reason, {DB, Gsb, Tool}) ->
  gen_db:stop(DB),
  ok.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Behaviour functions                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

read(List, Tool, U, State) ->
  {KeyList, [CreateRecord]} = read(List, Tool, U, State, 0, [],[], []),
  CreateRecord.

read([{Name,Class,GsType,none,[],Props,Events,Children}|Rest],
     Tool,U,State,Level, Parent,KAcc, CRAcc) ->
%% A hard widget
  Key = toolbox:get_new_key(Tool, GsType),
  {NKAcc,NCAcc} = read(Children, Tool, U, State, Level + 1, Key,[], []),
  Hierarchy = #hierarchy{visible = true,
			 children_visible = true,
			 level = Level,
			 text = add_space(Level, Name)},
  {reply, Reply, NewState} =
    gen_db:handle_call({insert,
			#gsb_db{key = Key,
				name = Name,
				class = Class,
				gs_type = GsType,
				type = hard,
				hierarchy = Hierarchy,
				module = none,
				exports = [],
				props = Props,
				events = Events,
				children = NKAcc,
				parent = Parent}}, U, State),
  CR = #create_widget{key = Key, name = Name, class = Class,
		      gs_type = GsType, type = hard,
		      props = Props, children = NCAcc},
  read(Rest, Tool, U, NewState, Level, Parent, [Key|KAcc],[CR|CRAcc]);
read([{N,Cl,Gs,Module,Exports,P,E,C}|Rest],Tool,U,State,Level,Par,KAcc,CRAcc)->
  %% A soft widget
  {_export, NKAcc, NCRAcc} =
    read_soft([{N,Cl,Gs,Module,Exports,P,E,C}|Rest],
	      [], Tool,U,State,Level,Par,KAcc,CRAcc), 
  NewAccKeys = lists:flatten([NKAcc|KAcc]),
  read(Rest, Tool, U, State, Level,Par, NewAccKeys, NCRAcc);
read([], _Gsb, _U, _State, _Level, _P,KAcc, CRAcc) ->
  {lists:reverse(KAcc), lists:reverse(CRAcc)}.

read_soft([{N,Cl,Gs,none,[],P,E,C}|Rest],
	  Export,Tool,U,State,Level,Parent,KAcc,CRAcc) ->
  %% A standard widget inside a softwidget
  Key = toolbox:get_new_key(Tool, Gs),
  {RestExported, NewProps} = toolbox:set_props(Export, N, P),
  {NewExport, NKAcc, NCRAcc} =
    read_soft(C, RestExported, Tool,U,State,Level+1,Key,[],[]),
  {reply, Reply, NewState} =
    gen_db:handle_call({insert,
			#gsb_db{key = Key,
				name = N,
				class = Cl,
				gs_type = Gs,
				type = soft,
				hierarchy = 
				#hierarchy{visible=false,
					   children_visible=false,
					   level=1,text="- "},
				module = none,
				exports = [],
				props = NewProps,
				events = E,
				children = NKAcc,
				parent = Parent}}, U, State),
  CR = #create_widget{key = Key, name = N, class = Cl,
		      gs_type = Gs, type = soft,
		      props = NewProps,  children = NCRAcc},
  read_soft(Rest,NewExport,Tool,U,State,Level,Parent,[Key|KAcc],[CR|CRAcc]);
read_soft([{N,Cl,Gs,M,Ex,P,E,C}|Rest],Export,Tool,U,State,Level,Par,KAcc,CRAcc) ->
  [{N1,Class,Gs1,M1,Ex1,P1,E1,C1}] = gen_db:read_list(make_filename(M)),
  Key = toolbox:get_new_key(Tool, Gs1),
  {RestExported, NewProps} =
    toolbox:set_props(lists:append([Export,Ex]), N, P1),
  {NewExport,NKAcc, NCRAcc} =
    read_soft(C1, RestExported, Tool, U, State, Level+1,Key,[],[]),
  Hierarchy = #hierarchy{visible = true,
			 children_visible = true,
			 level = Level,
			 text = add_space(Level, N)},
  {reply, Reply, NewState} =
    gen_db:handle_call({insert,
			#gsb_db{key = Key,
				name = N,
				class = Class,
				gs_type = Gs1,
				type = soft,
				hierarchy = Hierarchy,
				module = M,
				exports = Ex,
				props = NewProps,
				events = E1,
				children = NKAcc,
				parent = Par}}, U, State),
  CR = #create_widget{key = Key, name = N, class = Class,
		      gs_type = Gs1, type = soft,
		      props = NewProps, children = NCRAcc},
  read_soft(Rest,NewExport,Tool,U,NewState,Level,Par,[Key|KAcc],[CR|CRAcc]);

read_soft([], RE, T, _U, _NS, _L, _Par,KAcc, CRAcc) ->
  {RE, lists:reverse(KAcc), lists:reverse(CRAcc)}.

lookup_write_data(Keys, U, State) when list(Keys) ->
  lookup_write_data(Keys, U, State, []);
lookup_write_data(Key, U, State) ->
  lookup_write_data([Key], U, State, []).

lookup_write_data([Key|Rest], U, State, Acc) ->
  {reply, Data, NewState} = gen_db:handle_call({lookup, Key}, U, State),
  Children = lookup_write_data(Data#gsb_db.children, U, State, []),
  case Data#gsb_db.type of
    hard ->
      lookup_write_data(Rest, U, NewState,
			[{Data#gsb_db.name,
			  Data#gsb_db.class,
			  Data#gsb_db.gs_type,
			  Data#gsb_db.module,
			  Data#gsb_db.exports,
			  lists:keysort(1,Data#gsb_db.props),
			  lists:keysort(1,Data#gsb_db.events),
			  Children}|Acc]);
    soft ->
      lookup_write_data(Rest, U, NewState,
			[{Data#gsb_db.name,
			  Data#gsb_db.class,
			  Data#gsb_db.gs_type,
			  Data#gsb_db.module,
			  Data#gsb_db.exports,
			  [],
			  [],
			  []}|Acc])
  end;
lookup_write_data([], _U, _State, Acc) ->
  lists:reverse(Acc).


list(Type) ->
  case Type of
    all ->
      '$0'
  end.

set_data(_all, CurrData, []) ->
  CurrData;
set_data(_all, [],_) ->
  [];
set_data(name, CurrData, Data) ->
  CurrData#gsb_db{name = Data};
set_data(gs_type, CurrData, Data) ->
  CurrData#gsb_db{gs_type = Data};
set_data(class, CurrData, Data) ->
  CurrData#gsb_db{class = Data};
set_data(type, CurrData, Data) ->
  CurrData#gsb_db{type = Data};
set_data(hierarchy, CurrData, Data) ->
  CurrData#gsb_db{hierarchy = Data};
set_data(module, CurrData, Data) ->
  CurrData#gsb_db{module = Data};
set_data(exports, CurrData, {Type, Value}) ->
  CurrData#gsb_db{exports = replace(CurrData#gsb_db.exports,
				    {Type, Value})};
set_data(exports, CurrData, [First|Rest]) ->
  NewCurrData = set_data(exports, CurrData, First),
  set_data(exports, CurrData, Rest);
set_data(props, CurrData, {EntryType, Value}) ->
  CurrData#gsb_db{props = replace(CurrData#gsb_db.props,
				  {EntryType, Value})};
set_data(props, CurrData, [First|Rest]) ->
  NewCurrData = set_data(props, CurrData, First),
  set_data(props, NewCurrData, Rest);
set_data(events, CurrData, {EntryType, Value}) ->
  CurrData#gsb_db{events = replace(CurrData#gsb_db.events,
				   {EntryType, Value})};
set_data(events, CurrData, [First|Rest]) ->
  NewCurrData = set_data(events, CurrData, First),
  set_data(events, NewCurrData, Rest);
set_data(children, CurrData, Data) when list(Data) ->
  CurrData#gsb_db{children = lists:append([CurrData#gsb_db.children,Data])};
set_data(children, CurrData, Data) ->
  CurrData#gsb_db{children = lists:append([CurrData#gsb_db.children,[Data]])};
set_data(parent, CurrData, Data) ->
  CurrData#gsb_db{parent = Data}.

get_data(_, []) ->
  [];
get_data(record, Data) ->
  Data;
get_data(name, Data) ->
  Data#gsb_db.name;
get_data(gs_type, Data) ->
  Data#gsb_db.gs_type;
get_data(class, Data) ->
  Data#gsb_db.class;
get_data(type, Data) ->
  Data#gsb_db.type;
get_data(hierarchy, Data) ->
  Data#gsb_db.hierarchy;
get_data(module, Data) ->
  Data#gsb_db.module;
get_data(exports, Data) ->
  Data#gsb_db.exports;
get_data(props, Data) ->
  Data#gsb_db.props;
get_data(events, Data) ->
  Data#gsb_db.events;
get_data(children, Data) ->
  Data#gsb_db.children;
get_data(parent, Data) ->
  Data#gsb_db.parent.

%% ____________________________________________________________________
%%
%%  make_filename(Module)    
%%  Args    :
%%  Returns : A string with correct filename for Module
%%  Comment : Module -> "Module.swr"
%% ____________________________________________________________________

make_filename(Module) ->
  MList = atom_to_list(Module),
  lists:append([MList, ".swr"]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
create_top_i(TB, Props, DB) ->
  [CR] = create_from_record([],[TB],Props,0,DB,[]),
  {ok, Dir} = file:get_cwd(),
  FN = lists:append([Dir, "/", atom_to_list(CR#create_widget.name)]),
  {FN, CR}.

create_i(Parent, TB, Props, DB) ->
  %% Get Parent Data
  ParentData = gen_db:lookup(DB, Parent),
  %% Get Hierarchy Data
  ParentHierarchy = ParentData#gsb_db.hierarchy,
  gen_db:config(DB, Parent, children, [TB#create_tb.key]),
  [Re] =
    create_from_record(Parent,[TB],Props,
		       ParentHierarchy#hierarchy.level+1,DB,[]),
  Re.

create_from_record(Parent, [TB|Rest], Props, Level, DB, Acc) ->
  %% Construct Children first
  ChildrenList =
    case TB#create_tb.children of
      [] ->
	[];
      Children ->
	create_from_record(TB#create_tb.key,Children, [], Level + 1, DB, [])
    end,
  ChildrenKeys = extract_children_keys(ChildrenList, []),
  %% Configure props from toolbox according to Props
  %% Construct new record
  Data = #gsb_db{key = TB#create_tb.key,
		 name = TB#create_tb.name,
		 class = TB#create_tb.class,
		 gs_type = TB#create_tb.gs_type,
		 type = TB#create_tb.type,
		 hierarchy =
		 #hierarchy{visible = true,
			    children_visible = true,
			    level = Level, 
			    text = atom_to_list(TB#create_tb.name)},
		 module = TB#create_tb.module,
		 exports = TB#create_tb.exports,
  		 props = TB#create_tb.props, 
		 events = TB#create_tb.events,
		 children = ChildrenKeys,
		 parent = Parent},
  PropsData = set_all_props(Props, Data),
  gen_db:insert(DB, PropsData),
  %% Return create_widget record
  Re = #create_widget{key = Data#gsb_db.key,
		      name = Data#gsb_db.name,
		      class = Data#gsb_db.class,
		      gs_type = Data#gsb_db.gs_type,
		      type = Data#gsb_db.type, 
		      props = PropsData#gsb_db.props,
		      children = ChildrenList},
  create_from_record(Parent,Rest, Props, Level, DB, [Re|Acc]);
create_from_record(_Parent,[], Props, Level, DB, Acc) -> Acc.

copy_i(Key, NewParent, Props, Tool, DB) ->
  %% create all widgets
  [CR] = copy_ii(NewParent,[Key], Tool, DB, []),
  %% Set Props in new entry
  Data = gen_db:lookup(DB, CR#create_widget.key),
  PropsData = set_all_props(Props, Data),
  gen_db:insert(DB, PropsData),
  %% add pointer in Parent
  gen_db:config(DB, NewParent, children, CR#create_widget.key),
  TestData = gen_db:lookup(DB, NewParent),
  CR#create_widget{props = PropsData#gsb_db.props}.
  
copy_ii(Parent,[Key|Rest], Tool, DB, Acc) ->
  %%Lookup Key
  Data = gen_db:lookup(DB, Key),
  ParentData = gen_db:lookup(DB, Data#gsb_db.parent, children),
  %% Create new key
  NewKey = toolbox:get_new_key(Tool, Data#gsb_db.gs_type),
  %% Create children copies
  ChildrenList = copy_ii(NewKey,Data#gsb_db.children, Tool, DB, []),  
  %% get children keys
  ChildrenKeys = extract_children_keys(ChildrenList, []),
  %% construct and insert db record
  gen_db:insert(DB, Data#gsb_db{key = NewKey,
				name = NewKey,
				children = ChildrenKeys,
				parent = Parent}),
  %% construct create_widget record
  CR = #create_widget{key = NewKey,
		      name = NewKey,
		      class = Data#gsb_db.class,
		      gs_type = Data#gsb_db.gs_type,
		      type = Data#gsb_db.type,
		      props = Data#gsb_db.props,
		      children = ChildrenList},
		copy_ii(Parent, Rest, Tool, DB, [CR|Acc]);
copy_ii(Parent,[], _Tool, _DB, Acc) -> Acc.

create_toolbox_list(Key, DB, Module, Name, Exports, FileName) ->
  Data = gen_db:lookup(DB, Key),
  {RestExported,NewProps}=toolbox:set_props(Exports,Name,Data#gsb_db.props),
  CL = create_temporary_entries(Data#gsb_db.children, DB, RestExported,[]),
  [{Name,
   Data#gsb_db.class,
   Data#gsb_db.gs_type,
   Module,
   Exports,
   NewProps,
   Data#gsb_db.events,
   CL}].

create_temporary_entries([First|Rest], DB, Exports,Acc) ->
  Data = gen_db:lookup(DB, First),
  {RestExported,NewProps}=toolbox:set_props(Exports,
					    Data#gsb_db.name,
					    Data#gsb_db.props),
  CL = create_temporary_entries(Data#gsb_db.children, DB, RestExported,[]),
  Entry = {Data#gsb_db.name,
	   Data#gsb_db.class,
	   Data#gsb_db.gs_type,
	   Data#gsb_db.module,
	   Data#gsb_db.exports,
	   NewProps,
	   Data#gsb_db.events,
	   CL},
  create_temporary_entries(Rest, DB, RestExported,[Entry|Acc]);
create_temporary_entries([], DB, RestExported,Acc) -> Acc.

delete_i(DB, Key) ->
  Data = gen_db:lookup(DB, Key),
  case Data#gsb_db.parent of
    [] -> true;
    PKey ->
      ParentData = gen_db:lookup(DB, PKey),
      NewChildren = lists:delete(Key, ParentData#gsb_db.children),
      gen_db:insert(DB, ParentData#gsb_db{children = NewChildren})
  end,
  gen_db:delete(DB,Key).
      
extract_children_keys([], Acc) -> Acc;
extract_children_keys([First|Rest], Acc) ->
  extract_children_keys(Rest, [First#create_widget.key|Acc]).

set_all_props([], Record) -> Record;
set_all_props([Data|Rest], Record) ->
  set_all_props(Rest, set_data(props, Record, Data)).

add_space(Level, Name) ->
  S = atom_to_list(Name),
  add_space2(Level, S).

add_space2(0, String) -> String;
add_space2(N, String) ->
  add_space2(N-1, [32|String]).

config_all_widgets(DB, [First|Rest], Type, Value) ->
  gen_db:config(DB, First, Type, Value),
  config_all_widgets(DB, Rest, Type, Value);
config_all_widgets(DB, [], Type, Value) -> true.

%% ____________________________________________________________________
%%
%%  replace(List, Item)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________

replace(List, {EntryType, Value}) ->
  replace_type(List, [], {EntryType, Value}).

%% ____________________________________________________________________
%%
%%  replace_type(List, [], Type)    
%%  Returns :
%%  Args    :
%% ____________________________________________________________________


replace_type([{EntryType, _Value}| Rest], Saved, {EntryType, Value}) ->
  lists:append([{EntryType, Value}|Saved], Rest);

%% windows dont have x and y and will hit the buttom in a replace.
replace_type([],Saved,_ReplaceTuple)->
  Saved;

replace_type([{O, V}|Rest],Saved, {EntryType, Value}) ->
  replace_type(Rest, [{O,V}|Saved], {EntryType, Value}).



lookup_k_f_n(DB, [First|Rest], Name, Acc) ->
  Data = gen_db:lookup(DB, First, name),
  case gen_db:lookup(DB, First, name) of
    Name ->
      %% We have found the key. 
      lookup_k_f_n(DB, Rest, Name, [First|Acc]);
    Other ->
      %% Look in children
      ChildrenData = gen_db:lookup(DB, First),
      Children = gen_db:lookup(DB, First, children),
      %% The key is not always in the children
      case lookup_k_f_n(DB, Children, Name, []) of
	[] ->
	  lookup_k_f_n(DB, Rest, Name, Acc);
	[Key] ->
	  lookup_k_f_n(DB, Rest, Name, [Key|Acc])
      end
  end;
lookup_k_f_n(DB, [], Name, Acc) -> Acc.
