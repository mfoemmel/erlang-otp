%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	hierarchy.erl
%%  Module   :	hierarchy
%%  Purpose  :  Hierarchy viewer in GSB main window.
%%  Notes    : 
%%  History  :	* 1996-08-27 Peter Molin EX (peterm@csd.uu.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%
%% start(Gsb, GsParent) -> Pid, start the hierarchy process and return pid.
%% init({Gsb, GsParent}) -> true, initialize the hierarchy process
%% stop(Pid) -> true, stop the hierarchy process
%%
%% select(Pid, Keys) ->
%%        true, add Keys to selected buffer 
%% add(Pid, Parent, Key, Name) -> true, add {Key, Name} to listbox list
%% delete(Pid) ->  true, delete selected Keys from Listbox.
%% config(Pid, [{Key, {Option, Value}}|Rest]) ->  true, change name.
%%
%% USES
%% gsb:select_widgets
%% gsb_db:lookup
%% gsb_db:config
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hierarchy).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.18 $').
-export([start/4, init/1, stop/1, select_widgets/2, add/4, add_top/3,
	 delete/2, config/3, handle_call/3, handle_info/2, insert/3,
	terminate/2]).
%-import([]).
-include("gsb.hrl").
-record(h_loop, {gsb, db, selected, members, state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Exported functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

start(Gsb, GsParent, DB, MenuHeight) ->
  case gen_server:start_link(
	 hierarchy, {Gsb, GsParent, DB, MenuHeight}, []) of
    {ok, Pid} -> Pid;
    {error, Reason} -> messages:error("~p", [Reason], start, hierarchy)
  end.

init({Gsb, GsParent, DB, MenuHeight}) ->
  gs:create(frame,frame,GsParent, [{x, 150}, {y, 55},
				    {width,150},{height,265},
				    {keypress, true}, {keyrelease, true}]),
  gs:create(listbox,hierarchy,frame,
	    [{x,0},{y,0},{width,150},{height,265},{activebg,black},
	     {selectmode,single},{click,true},{doubleclick,true},
	     {vscroll,right}, {enter, true}, {leave, true}]),
  {ok, #h_loop{gsb = Gsb, db = DB, selected = [], members = [],
	       state = {up, single}}}.

stop(Pid) ->
  gen_server:call(Pid, stop).
select_widgets(Pid, Keys) ->
  gen_server:call(Pid, {select_widgets, Keys}).
add(Pid, Parent, Key, Name) ->
  gen_server:call(Pid, {add, Parent, Key, Name}).
add_top(Pid, Key, Name) ->
  gen_server:call(Pid, {add_top, Key, Name}).
delete(Pid, Key) ->
  gen_server:call(Pid, {delete, Key}).
config(Pid, Key, NewName) ->
  gen_server:call(Pid, {config, Key, NewName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% handle_call                                                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% ---- GS events -----

handle_info({gs, hierarchy, enter, _,_}, State) ->
  %% gsb:set_label("Doubleclick to compress/expand, Click to focus"),
  gs:config(frame, {setfocus, true}),
  {noreply, State};
handle_info({gs, hierarchy, leave, _,_}, State) ->
  gs:config(frame, {setfocus, false}),
  {noreply, State};
handle_info({gs, frame, keypress, _, ['Shift_L'|_]}, State) ->
  gs:config(hierarchy, {selectmode, multiple}),
  {noreply, State#h_loop{state = {down, multiple}}};
handle_info({gs, frame, keypress, _, ['Shift_R'|_]}, State) ->
  gs:config(hierarchy, {selectmode, multiple}),
  {noreply, State#h_loop{state = {down, multiple}}};
handle_info({gs, frame, keyrelease, _, ['Shift_L'|_]}, State) ->
  gs:config(hierarchy, {selectmode, single}),
  {noreply, State#h_loop{state = {up, single}}};
handle_info({gs, frame, keyrelease, _, ['Shift_R'|_]}, State) ->
  gs:config(hierarchy, {selectmode, single}),
  {noreply, State#h_loop{state = {up, single}}};
handle_info({gs, frame, keypress, _, ['Delete'|_]}, State) ->
  %% gsb:broadcast(delete_selected),
  {noreply, State};
handle_info({gs, From, keypress, Data, Other}, State) ->
  {noreply, State};
handle_info({gs, From, keyrelease, Data, Other}, State) ->
  {noreply, State};
handle_info({gs,hierarchy,doubleclick,_,[Index,Text|_]}, State)->
  case locate_index(Index, State#h_loop.members) of
    undef ->
      {noreply, State};
    Key ->
      Data = gsb_db:lookup(State#h_loop.db, Key),
      HData = Data#gsb_db.hierarchy,
      {ResState, ResHier} =
	case Data#gsb_db.children of
	  [] ->
	    NewHier=HData#hierarchy{text=to_minus(HData#hierarchy.text)},
	    {State, NewHier};
	  Children ->
	    case HData#hierarchy.children_visible of
	    true ->
		NewState=compress(Children, State),
		NewHier =
		  HData#hierarchy{text=insert_mark(HData#hierarchy.text),
				  children_visible = false},
		{NewState, NewHier}; 
	    false ->
		{NewAcc, NewState} =
		  expand(lists:reverse(Children), Index + 1, State, 0),
		NewHier=
		  HData#hierarchy{text=delete_mark(HData#hierarchy.text),
				  children_visible = true},
		{NewState, NewHier}
	    end
	end,
      gs:config(hierarchy, {change, {Index, ResHier#hierarchy.text}}),
      gsb_db:config(State#h_loop.db, Key, hierarchy, ResHier),
      {noreply, ResState}
  end;
handle_info({gs,hierarchy,click,_,[Index,Text|_]}, State)->
  case locate_index(Index, State#h_loop.members) of
    undef ->
      {noreply, State};
    Key ->
      Data = gsb_db:lookup(State#h_loop.db, Key),
      case State#h_loop.state of
	{down, multiple} ->
	  gs:config(hierarchy, {selection, Index}),
	  gsb:select_widgets(State#h_loop.gsb,[Key|State#h_loop.selected]),
	  {noreply, State#h_loop{selected=[Key|State#h_loop.selected]}};
	{up,multiple} ->
	  gs:config(hierarchy, {selection, clear}),
	  gs:config(hierarchy, {selection, Index}),
	  gs:config(hierarchy, {selectmode, single}),
	  gsb:select_widgets(State#h_loop.gsb, [Key]),
	  {noreply, State#h_loop{selected = [Key],
				 state = {up, single}}};
	{_,single} -> 
	  gsb:select_widgets(State#h_loop.gsb, [Key]),
	  {noreply, State#h_loop{selected = [Key]}}
      end
  end.


%% ---- Exported events ----- 
handle_call({select_widgets, [Key]}, From, State) ->
  gs:config(hierarchy, [{selectmode, single}, {selection, clear}]),
  select_visible([Key], State#h_loop.db, State#h_loop.members),
  {reply, true, State#h_loop{selected = [Key], state = {up, single}}};
handle_call({select_widgets, Keys}, From, State) ->
  gs:config(hierarchy, [{selectmode, multiple},{selection, clear}]),
  select_visible(Keys, State#h_loop.db,
		 State#h_loop.members),
  {reply, true, State#h_loop{selected = Keys, state = {up, multiple}}};
handle_call({add_top, Key, Name}, From, State) ->
  DataC = gsb_db:lookup(State#h_loop.db, Key, hierarchy),
  NewText = create_space(DataC#hierarchy.level,
			 atom_to_list(Name), []),
  gsb_db:config(State#h_loop.db,
		Key, hierarchy, DataC#hierarchy{text = NewText}),
  Index = length(State#h_loop.members),
  gs:config(hierarchy,{add,{Index + 1, NewText}}), 
  NewMembers = insert(Key, Index + 1, State#h_loop.members),
  {reply, true, State#h_loop{members = NewMembers}};
handle_call({add, Parent, Key, Name}, From, State) ->
  ParentData=gsb_db:lookup(State#h_loop.db, Parent), 
  DataP=ParentData#gsb_db.hierarchy,
  Delta = get_length(State#h_loop.db,ParentData#gsb_db.children,0),
  DataC=gsb_db:lookup(State#h_loop.db,Key,hierarchy),
  NewH = DataP#hierarchy.level +1,
  NewText = create_space(NewH, atom_to_list(Name), []),
  gsb_db:config(State#h_loop.db,
	       Key, hierarchy, DataC#hierarchy{level = NewH,
					       text = NewText}),
  case DataP#hierarchy.children_visible of
    true ->
      Index = locate_key(Parent, State#h_loop.members),
      gs:config(hierarchy,{add,{Index + Delta, NewText}}), 
      NewMembers = insert(Key, Index + Delta, State#h_loop.members),
      {reply, true, State#h_loop{members = NewMembers}};
    false ->
      {reply, true, State}
  end;
handle_call({delete, Key}, From, State) ->
  NewMembers =
    case lists:member(Key, State#h_loop.members) of
      true ->
	Index = locate_key(Key, State#h_loop.members),
	gs:config(hierarchy, {del, {Index, Index}}),
	delete_key(Key, State#h_loop.members);
    _else -> State#h_loop.members
  end,
  {reply, true, State#h_loop{members = NewMembers}};
handle_call(unfocus, From, State) ->
  gs:config(hierarchy, {selection, clear}),
  gs:config(hierarchy, {selectmode, single}),
  {reply, true, State#h_loop{state = {up, single}}}; 
handle_call({config, Key, NewName}, From, State) ->
  Index = locate_key(Key, State#h_loop.members),
  HData = gsb_db:lookup(State#h_loop.db, Key, hierarchy),
  NewText = create_space(HData#hierarchy.level,atom_to_list(NewName),[]),
  gs:config(hierarchy, {change, {Index, NewText}}),
  gs:config(hierarchy, {selection, Index}),
  gsb_db:config(State#h_loop.db, Key, hierarchy, 
		HData#hierarchy{text = NewText}),
  {reply, true, State};
handle_call({gs,hierarchy,click,_,[Index,Text|_]}, From, State) ->
  {reply, true, State};
handle_call(stop, From, State) ->
  {stop, normal, true, State}.
    
terminate(Reason, State) ->
  gs:destroy(hierarchy),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% Internal functions                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

get_length(DB, [First|Rest], Acc) ->
  Children = gsb_db:lookup(DB, First, children),
  Delta = get_length(DB, Children, 0),
  get_length(DB, Rest, Acc + Delta + 1);
get_length(DB, [], Acc) -> Acc.

select_visible([], DB, List) -> true;
select_visible([First|Rest], DB, List) ->
  Data = gsb_db:lookup(DB,First, hierarchy),
  case Data#hierarchy.visible of
    true ->
      Index = locate_key(First, List),
      gs:config(hierarchy, {selection, Index});
    false ->
      true
  end,
  select_visible(Rest, DB, List).

to_minus([45|Text]) -> [45|Text];
to_minus([43|Text]) -> [45|Text].

insert_mark([45|Text]) ->
  [43|Text].
delete_mark([43|Text]) ->
  [45|Text].

%% create_space(0, [32|Text], Acc) -> lists:append([" ", Acc, Text]);
create_space(0, Text, Acc) -> lists:append(["- ", Acc, Text]);
create_space(N, [32,32|Text], Acc) ->
  create_space(N - 1, Text, [32,32|Acc]);
create_space(N, Text, Acc) ->
  create_space(N - 1, Text, [32,32|Acc]).

%% ____________________________________________________________________
%%
%%  compress    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

compress([], State) -> State;
compress([First|Rest], State) ->
  Data = gsb_db:lookup(State#h_loop.db, First),
  HData = Data#gsb_db.hierarchy,
  Index = locate_key(First, State#h_loop.members),
  gs:config(hierarchy, {del, Index}),
  InputState = State#h_loop{members =
			    delete_key(First, State#h_loop.members)},
  gsb_db:config(State#h_loop.db, First, hierarchy,
		HData#hierarchy{visible = false}),
  case HData#hierarchy.children_visible of
    true ->
      NewState = compress(Data#gsb_db.children, InputState),
      compress(Rest, NewState);
    false ->
      compress(Rest, InputState)
  end.

%% ____________________________________________________________________
%%
%%  expand(    
%%  Args    :
%%  Returns :
%%  Comment :
%% ____________________________________________________________________

expand([], _N, State, Acc) -> {Acc, State};
expand([First|Rest], Index, State, Acc) ->
  Data = gsb_db:lookup(State#h_loop.db, First),
  HData = Data#gsb_db.hierarchy,
  gsb_db:config(State#h_loop.db, First, hierarchy,
		HData#hierarchy{visible = true}),
  gs:config(hierarchy, {add, {Index, HData#hierarchy.text}}),
  InputState =
    State#h_loop{members = insert(First, Index, State#h_loop.members)},
  case HData#hierarchy.children_visible of
    true ->
      {NewAcc, NewState} =
	expand(lists:reverse(Data#gsb_db.children),
	       Index + 1, InputState, 0),
      expand(Rest, Index + 1 + NewAcc, NewState, Acc + 1);
    false ->
      expand(Rest, Index + 1, InputState, Acc + 1)
  end.    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% list processing                                                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

insert(E, N, List) ->
  insert(E, N, List, []).

insert(E, _N, [], Acc) -> lists:append([lists:reverse(Acc), [E]]);
insert(E, 0, List, Acc) -> lists:append([lists:reverse(Acc), [E], List]);
insert(E, N, [First|Rest], Acc) ->
  insert(E, N - 1, Rest, [First|Acc]).

%% ____________________________________________________________________
%%
%%  locate_key(Key, List)    
%%  Args    :
%%  Returns :
%%  Comment : Locate index of Key in List.
%% ____________________________________________________________________

locate_key(Key, List) ->
  locate_key(Key, List, 0).

locate_key(_Key, [], N) -> N;
locate_key(Key, [Key|_Rest], N) -> N;
locate_key(Key, [_Other|Rest], N) ->
  locate_key(Key, Rest, N + 1).

locate_index(_, []) -> undef;
locate_index(Index, List) ->
  lists:nth(Index + 1, List).
 
delete_key(Key, List) ->
  lists:delete(Key, List).
