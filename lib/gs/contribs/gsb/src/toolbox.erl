%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	toolbox.erl
%%  Module   :	toolbox
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-08-27 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%
%% start(Gsb, GsParent) -> Pid, start the hierarchy process and return pid.
%% init() -> true, initialize the hierarchy process
%% stop() -> true, stop the hierarchy process
%%
%% unfocus(Pid) -> true, unfocus selection on listbox
%% create(Pid, Type) -> Record, extract information of Type for creation of new
%%                 object. returns a record specified in gsb.hrl
%% view_file(FileName) -> list all widgets
%% save(Pid, FileName) -> true, save current database to defined Filename
%% open(Pid, FileName) -> true, open a new database.
%% get_key(Pid, Type) -> true, lookup type in database and create new gsb key.
%% save_as(FileName) -> true, change name of current database and save
%% open(FileName) -> true, open a new database.
%% get_new_key(Pid, Type) -> Key, lookup type in database and create new gsb key.
%% add(RecordList).
%%
%% gsb:select_toolbox
%% gsb:set_action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(toolbox).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.22 $').
%% External exports
-export([start/3, stop/1, get_new_key/2, read/2, add/3,
	 create/2, unselect/1, lookup/2, write/2,config_toolbox/4,
	 set_props/3]).
%% gen_server exports
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
%% gen_db exports
-export([read/4, lookup_write_data/3, set_data/3, get_data/2, list/1]).
%% debug exports
-export([list_type/2]).
-include("gsb.hrl").

-record(tool_loop, {db, gsb, gs_parent}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start(Gsb, GsParent, MenuHeight) ->
  case gen_server:start_link(
	 toolbox, {Gsb, GsParent, MenuHeight}, []) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, toolbox)
  end.

init({Gsb, GsParent, MenuHeight}) ->
  DB = gen_db:start(toolbox, self(), toolbox),
  gs:create(listbox,toolbox,GsParent,
	    [{width,150},{height,265},{activebg,black}, {y, 55},
	     {selectmode,simple},{click,true},{doubleclick,true},
	     {vscroll,right}]),
  gs:config(toolbox, {focus, false}),
  {ok, #tool_loop{db = DB, gsb = Gsb, gs_parent = GsParent}}.

add(Pid, FileName, WidgetList) ->
  gen_server:call(Pid, {add, FileName, WidgetList}, infinity).
stop(Pid) ->
  gen_server:call(Pid, stop, infinity).
read(Pid, FileName) ->
  gen_server:call(Pid, {read, FileName}, infinity).
write(Pid, FileName) ->
  gen_server:call(Pid, {write, FileName}, infinity).
unselect(Pid) ->
  gen_server:call(Pid, unselect, infinity).
create(Pid, Type) -> 
  gen_server:call(Pid, {create, Type}, infinity).
get_new_key(Pid, Type) ->
  gen_server:call(Pid, {get_new_key, Type}, infinity).
lookup(Pid, Type) ->
  gen_server:call(Pid, {lookup, Type}, infinity).
config_toolbox(Pid, [Key], Type, Data) ->
  gen_server:call(Pid, {config, Key, Type, Data}, infinity).
list_type(Pid, Type) ->
  gen_server:call(Pid, {list, Type}, infinity).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% handle                                                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

handle_info({gs,toolbox,click,_,[Index,Text|_]}, State)->
  Key = list_to_atom(Text), 
  Data = gen_db:lookup(State#tool_loop.db, Key, class),
  gsb:set_action(State#tool_loop.gsb, {Key, Data}),
  {noreply, State};
handle_info({gs,toolbox,doubleclick,_,[Index,Text|_]}, State) ->
  Data = gen_db:lookup(State#tool_loop.db, list_to_atom(Text)),
  gsb:select_toolbox(State#tool_loop.gsb,
		     {Data#tb_db.key,
		      Data#tb_db.module,
		      Data#tb_db.exports,
		      Data#tb_db.props,
		      Data#tb_db.events}),
  {noreply, State}.

handle_call({add, FileName, WidgetList}, From, State) ->
  RecordList = read_subset(FileName, WidgetList),
  insert_records(State#tool_loop.db, RecordList),
  add_to_toolbox(WidgetList),
  {reply, true, State};
handle_call({config, Key, Type, Data}, From, State) ->
  gen_db:config(State#tool_loop.db, Key, Type, Data),
  {reply, true, State};
handle_call({read, FileName}, From, State) ->
  List = gen_db:read(State#tool_loop.db, State#tool_loop.gsb, FileName),
  gs:config(toolbox, [{items, List}]),
  {reply, true, State};
handle_call({write, FileName}, From, State) ->
  {reply, Keys, NState} = handle_call({list, all_keys}, From, State),
  gen_db:write(State#tool_loop.db, lists:flatten(Keys), FileName),
  {reply, true, State};
handle_call(unselect, From, State) ->
  gs:config(toolbox, {selection, clear}),
  {reply, true, State};
handle_call({get_new_key, Type}, From, State) ->
  C = gen_db:lookup(State#tool_loop.db, Type, counter),
  gen_db:config(State#tool_loop.db, Type, counter, C + 1),
  R = make_key(Type, C + 1),
  {reply, R, State};
handle_call({lookup, Key}, From, State) ->
  R = gen_db:lookup(State#tool_loop.db, Key),
  {reply, R, State};
handle_call({create, Type}, From, State) ->
  Result = create_top(Type, State),
  {reply, Result, State};
handle_call({list, Type}, From, State) ->
  Result = gen_db:list(State#tool_loop.db, Type),
  {reply, Result, State};
handle_call(stop, From, State) ->
  {stop, normal, true, State}.

terminate(Reason, State) ->
  gen_db:stop(State#tool_loop.db),
  gs:destroy(toolbox),
  ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Behaviour functions                                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

read(List, Gsb, U, State) ->
  read(List, Gsb, U, State, []).

read([], _Gsb, _U, _State, Acc) -> Acc;
read([{K, Cl, Gs, M, Exp, P, E, C}|Rest], Gsb, U, State, Acc) ->
  T = case M of
	none -> hard;
	Else -> soft
      end,
  gen_db:handle_call({insert,
		      #tb_db{key = K,
			     class = Cl,
			     gs_type = Gs,
			     type = T,
			     module = M,
			     exports = Exp,
			     props = P,
			     events = E,
			     children = C,
			     counter = 0}}, U, State),
  read(Rest, Gsb, U, State, [K|Acc]).

lookup_write_data(Keys, U, State) ->
  lookup_write_data(Keys, U, State, []).

lookup_write_data([], _U, _State, Acc) -> Acc;
lookup_write_data([Key|Rest], U, State, Acc) ->
  {reply, Data, Nstate} = gen_db:handle_call({lookup, Key}, U, State),
  lookup_write_data(Rest, U, State,
		    [{Data#tb_db.key,
		      Data#tb_db.class,
		      Data#tb_db.gs_type,
		      Data#tb_db.module,
		      Data#tb_db.exports,
		      Data#tb_db.props,
		      Data#tb_db.events,
		      Data#tb_db.children}|Acc]).
  
list(Type) ->
  case Type of
    all_keys ->
      {tb_db,'$1', '_','_','_','_','_','_','_','_','_','_'};
    Other -> '$0'
  end.

set_data(_all, CurrData, []) ->
  CurrData;
set_data(class, CurrData, Data) ->
  CurrData#tb_db{class = Data};
set_data(gs_type, CurrData, Data) ->
  CurrData#tb_db{gs_type = Data};
set_data(type, CurrData, Data) ->
  CurrData#tb_db{type = Data};
set_data(module, CurrData, Data) ->
  CurrData#tb_db{module = Data};
set_data(exports, CurrData, Data) ->
  CurrData#tb_db{exports = replace(CurrData#tb_db.exports, Data)};
set_data(props, CurrData, {Option, Value}) ->
  CurrData#tb_db{props = replace(CurrData#tb_db.props, {Option, Value})};
set_data(events, CurrData, Data) ->
  CurrData#tb_db{events = replace(CurrData#tb_db.events, Data)};
set_data(counter, CurrData, Data) ->
  CurrData#tb_db{counter = Data};
set_data(children, CurrData, [Data]) ->
  CurrData#tb_db{children = lists:append([[Data], CurrData#tb_db.children])};
set_data(children, CurrData, Data) ->
  CurrData#tb_db{children = [Data|CurrData#tb_db.children]}.


get_data(record, Data) ->
  Data;
get_data(class, Data) ->
  Data#tb_db.class;
get_data(gs_type, Data) ->
  Data#tb_db.gs_type;
get_data(type, Data) ->
  Data#tb_db.type;
get_data(module, Data) ->
  Data#tb_db.module;
get_data(exports, Data) ->
  Data#tb_db.exports;
get_data(props, Data) ->
  Data#tb_db.props;
get_data(events, Data) ->
  Data#tb_db.events;
get_data(counter, Data) ->
  Data#tb_db.counter;
get_data(children, Data) ->
  Data#tb_db.children.

replace(List, {EntryType, Value}) ->
  replace_type(List, [], {EntryType, Value});
replace(List, [First|Rest]) ->
  NewList = replace_type(List, [], First),	       
  replace(NewList, Rest);
replace(List, []) -> List.

replace_type([{EntryType, _Value}| Rest], Saved, {EntryType, Value}) ->
  lists:append([{EntryType, Value}|Saved], Rest);
replace_type([{O, V}|Rest],Saved, {EntryType, Value}) ->
  replace_type(Rest, [{O,V}|Saved], {EntryType, Value}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal functions                                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_key(Name, Number) ->
  L_Name = atom_to_list(Name),
  L_Number = integer_to_list(Number),
  L = lists:append([L_Name, L_Number]),
  list_to_atom(L).

create_top(Type, State) ->
  Data = gen_db:lookup(State#tool_loop.db, Type),
  gen_db:config(State#tool_loop.db, Type, counter, Data#tb_db.counter + 1),
  Key = make_key(Type, Data#tb_db.counter + 1), 
  case Data#tb_db.module of
    none ->
      %% Hard widgets do not have children
      #create_tb{key = Key,
		 name = Key,
		 class = Data#tb_db.class,
		 gs_type = Data#tb_db.gs_type,
		 type = Data#tb_db.type,
		 module = Data#tb_db.module,
		 exports = Data#tb_db.exports,
		 props = Data#tb_db.props,
		 events = Data#tb_db.events,
		 children = []};
    Module ->
      FileName = make_filename(Module),
      %% Possibility of error, maybe check if file exists!
      [{N, Cl, G, Mod, Exp, P, E, C}] = gen_db:read_list(FileName),
      {RestExported, Props} =
	set_props(Data#tb_db.exports, N, P),
      ChildrenList = create_children(C, RestExported, State, []),
      #create_tb{key = Key,
		 name = N,
		 class = Cl,
		 gs_type = G,
		 type = soft,
		 module = Mod,
		 exports = Data#tb_db.exports,
		 props = Props,
		 events = E,
		 children = ChildrenList}
  end.

create_children([{N, Cl, G, none, [], P, E, C}|Rest], Exports, State, Acc) ->
  Data = gen_db:lookup(State#tool_loop.db, G),
  gen_db:config(State#tool_loop.db, G, counter, Data#tb_db.counter + 1),
  Key = make_key(G, Data#tb_db.counter + 1),
  {RestExported, Props} = set_props(Exports, N, P),
  ChildrenList = create_children(C,  RestExported, State, []),
  Entry = #create_tb{key = Key,
		     name = N,
		     class = Cl,
		     gs_type = G,
		     type = soft,
		     module = none,
		     exports = [],
		     props = Props,
		     events = E,
		     children = ChildrenList},
  create_children(Rest, RestExported, State, [Entry|Acc]);
create_children([{N, Cl, G, Module, Exp,P,E,C}|Rest], Exports, State, Acc) ->
  Data = gen_db:lookup(State#tool_loop.db, G),
  gen_db:config(State#tool_loop.db, G, counter, Data#tb_db.counter + 1),
  Key = make_key(G, Data#tb_db.counter + 1),
  FileName = make_filename(Module),
  [{SN, SCl, SG, SM, SE, SP, SE, SC}] = gen_db:read_list(FileName),
  {RestExported, Props} = set_props(lists:append([Exp,Exports]), SN, SP), 
  ChildrenList = create_children(SC, RestExported, State, []),
  Entry = #create_tb{key = Key,
		     name = SN,
		     class = SCl,
		     gs_type = SG,
		     type = soft,
		     module = Module,
		     exports = Exp,
		     props = Props,
		     events = SE,
		     children = ChildrenList},
  create_children(Rest, RestExported,State, [Entry|Acc]);
create_children([], _RestExported,_State, Acc) -> Acc.

set_props(Exports, Name, Props) ->
  {SetExports, Rest} = get_relevant(Exports, Name, [], []),
  NewProps = replace(Props, SetExports), 
  {Rest, NewProps}.

get_relevant([{{Name, Type}, Value}|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, [{Type, Value}|SetAcc], RestAcc);
get_relevant([{{Other, Type}, Value}|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, SetAcc, [{{Other, Type}, Value}|RestAcc]);
get_relevant([], _Name, SetAcc, RestAcc) -> {SetAcc, RestAcc};
get_relevant([Other|Rest], Name, SetAcc, RestAcc) ->
  get_relevant(Rest, Name, SetAcc, RestAcc).

read_subset(FileName, WidgetList) ->
  TermList = gen_db:read_list(FileName),
  get_widgets(TermList, WidgetList, []).

get_widgets([{K, Cl, Gs, none, [], P, E, C}|Rest], WidgetList, Acc) ->
  case lists:member(K, WidgetList) of
    true ->
      get_widgets(Rest, WidgetList,
		  [#tb_db{key = K,
			  class = Cl,
			  gs_type = Gs,
			  module = none,
			  exports = [],
			  props = P,
			  events = E,
			  children = C,
			  counter = 0}|Acc]);
    false ->
      get_widgets(Rest, WidgetList, Acc)
  end;
get_widgets([{K, Cl, Gs, Module, Exp, P, E, C}|Rest], WidgetList, Acc) ->
  case lists:member(K, WidgetList) of
    true ->
      get_widgets(Rest, WidgetList,
		  [#tb_db{key = K,
			  class = Cl,
			  gs_type = Gs,
			  type = soft,
			  module = Module,
			  exports = Exp,
			  props = [],
			  events = [],
			  children = [],
			  counter = 0}|Acc]);
    false ->
      get_widgets(Rest, WidgetList, Acc)
  end;
get_widgets([], _WidgetList, Acc)-> Acc.

insert_records(DB, [First|Rest]) ->
  case gen_db:lookup(DB, First#tb_db.key) of
    [] ->
      gen_db:insert(DB, First);
    Data ->
      gen_db:insert(DB, First#tb_db{counter = Data#tb_db.counter})
  end;
insert_records(DB, []) -> true.

add_to_toolbox([First|Rest]) ->
  FirstList = atom_to_list(First),
  All = gs:read(toolbox, items),
  case lists:member(FirstList, All) of
    false ->
      gs:config(toolbox, {add, FirstList});
    true ->
      true
  end,
  add_to_toolbox(Rest);
add_to_toolbox([]) -> true.

%remove_from_toolbox(Name) ->
%  AllItems = gs:read(toolbox, items),
%  N = locate(atom_to_list(Name), AllItems, 0),
%  gs:config(toolbox, {del, N}).

%locate(NameString, [NameString|Rest], N) -> N;
%locate(NameString, [Other|Rest], N) ->
%  locate(NameString, Rest, N+1);
%locate(NameString, [], N) -> N.
  
make_filename(Module) ->
  lists:append([atom_to_list(Module), ".swr"]).
