%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	browse_dialog.erl
%%  Module   :	browse_dialog
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-17 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(browse_dialog).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.6 $').

-export([start/1,fs_init/2]).

-record(browse, {dir, owner, state, selected}).
%% ----- File Selection ----
start(Dir) ->
  Dir0 = case lists:last(Dir) of
	   $/ -> Dir;
	   _  -> lists:append(Dir,"/")
	 end,
  Pid=spawn(browse_dialog,fs_init,[Dir0, self()]),	
  receive
    {browse_dialog,Result} -> Result
  end.


%% ------------------------------------------------------------
fs_init(Dir, Owner) ->
  gsb_run:start(gs:start(),browse_dialog),
  Items=refresh(Dir),
  %% --- select File if it's given ---
  gs:config(win,{map,true}),
  fs_loop(#browse{dir = Dir, owner = Owner, state = {up, single},
		  selected = []}).

fs_loop(State) ->
  receive
    {gs, win, keypress, _, ['Shift_L'|_]} ->
      gs:config(lb2, {selectmode, multiple}),
      fs_loop(State#browse{state = {down, multi}});
    {gs, win, keypress, _, ['Shift_R'|_]} ->
      gs:config(lb2, {selectmode, multiple}),
      fs_loop(State#browse{state = {down, multi}});
    {gs, win, keyrelease, _, ['Shift_L'|_]} ->
      gs:config(lb2, {selectmode, single}),
      fs_loop(State#browse{state ={up, single}});
    {gs, win, keyrelease, _, ['Shift_R'|_]} ->
      gs:config(lb2, {selectmode, single}),
      fs_loop(State#browse{state ={up, single}});
    {gs,ok,click,_,_} ->
      File = gs:read(entry, text),
      State#browse.owner!
	{browse_dialog, {State#browse.dir, File, State#browse.selected}};
    {gs,cancel,click,_,_} ->
      State#browse.owner ! {browse_dialog,cancel};
    {gs,entry,keypress,_,['Return'|_]} ->
      entered_name(State);
    {gs,entry,keypress,_,[Keysym|_]} ->
      fs_loop(State);
    {gs,lb,click,Data,[Index, Text|_]} ->
      case lists:last(Text) of
	$/ -> %it's a dir
	  true;
	_ -> % it's a file
	  gs:config(entry,{text,Text}),
	  List = read_keys(lists:append([State#browse.dir, Text])),
	  gs:config(lb2, {items, List})
      end,
      fs_loop(State);
    {gs,lb,doubleclick,Data,[Index, Text|_]} ->
      case Index of
	0 -> % up one dir
	  NewDir=up_one_dir(State#browse.dir),
	  refresh(NewDir),
	  fs_loop(State#browse{dir = NewDir});
	Idx ->
	  case lists:last(Text) of
	    $/ -> % down a dir
	      NewDir=lists:append(State#browse.dir, Text),
	      refresh(NewDir),
	      fs_loop(State#browse{dir = NewDir});
	    _ ->
	      fs_loop(State)
	  end
      end;
    {gs, lb2, click, Data, [Index, Text|_]} ->
      case State#browse.state of
	{up, single} ->
	  fs_loop(State#browse{selected = [list_to_atom(Text)]});
	{down, multi} ->
	  gs:config(lb2, {selection, Index}),
	  fs_loop(State#browse{selected =
			       insert(State#browse.selected,list_to_atom(Text))})
      end;
    {gs, lb2, doubleclick, Data, [Index, Text|_]} ->
      File = gs:read(entry, text),
      State#browse.owner!
	{browse_dialog, {State#browse.dir, File, State#browse.selected}};
    stop ->
      exit(normal);
    {gs,_,destroy,_,_} -> 
      State#browse.owner ! {browse_dialog,cancel};
    X ->
      io:format("browse_dialog: got other: ~w.~n",[X]),
      fs_loop(State)
  end.

read_keys(FileName) ->
  case open_file(FileName) of
    true ->
      case file:consult(FileName) of
	{ok, TermList} -> get_second_element(TermList, []);
	{error, Reason} -> []
      end;
    false -> []
  end.
  
get_second_element([F|R], Acc) ->
  get_second_element(R, [element(1, F)|Acc]);
get_second_element([], Acc) -> Acc.

open_file(FileName) ->
  case file:open(FileName, read) of
    {ok, Fd} ->
      file:close(Fd),
      true;
    {error, Reason} ->
      false
  end.


refresh(Dir) ->
  gs:config(lb,clear),
  gs:config(label,{label, {text,Dir}}),
  gs:config(entry,{text,""}),
  Items=["../"|get_files(Dir)],
  gs:config(lb,[{items,Items}]),
  Items.

entered_name(State) ->
  File=gs:read(entry,text),
  case check_file(State#browse.dir,File) of
    {file,Dir2,File2} ->
      fs_loop(State);
    {dir,Dir2} ->
      refresh(Dir2),
      fs_loop(State#browse{dir = Dir2});
    _ ->
      fs_loop(State)
  end.

cut_suffix(N,L) when N=<0 -> 
  [];
cut_suffix(N,[H|T]) ->
  [H|cut_suffix(N-1,T)].

%% checks if a file exists
%% returns {file,Dir,File}
%%         {dir,Dir}
%%    or   {error,What}
check_file(Dir,File) ->
  case catch lists:last(File) of
    $/ -> % File is a Dir
      NewDir = case File of
		 [$/|_] -> %absolute path
		   File;
		 _ -> %relative path
		   lists:append(Dir,File)
	       end,
      case file:list_dir(NewDir) of
	{ok,_} -> {dir,NewDir};
	_      -> {error,bad_dir}
      end;
    {'EXIT',Why} -> {error,no_file};
    _ ->
      Words=string:tokens(File,[$/,$\\]),
      NewFile=lists:last(Words),
      NewDir = case File of
		 [$/|_] -> %absolute path
		   up_one_dir(File);
		 _ -> %relative path
		   case up_one_dir(File) of
		     [$/]        -> Dir;
		     [$/|SubDir] -> lists:flatten([Dir,SubDir,$/])
		   end
	       end,
      case file:file_info(lists:append(NewDir,NewFile)) of
	{ok,_} -> 
	  {file,NewDir,NewFile};
	_ -> 
	  {error,bad_file}
      end
  end.


get_files(Dir) -> 
  {ok,Files} = file:list_dir(Dir),
  T = add_slash(Dir,lists:sort(Files)),
  filter(T, []).

add_slash(_,[]) -> [];
add_slash(Dir,[H|T]) ->
  case file:file_info(lists:append(Dir,[$/|H])) of
    {ok,{_,directory,_,_,_,_,_}} ->
      [lists:append(H,"/")|add_slash(Dir,T)];
    _ ->
      [H|add_slash(Dir,T)]
  end.


filter([H|T], Acc) ->
  case lists:last(H) of
    $/ -> filter(T, [H|Acc]);
    _ ->
      Len =length(H),
      if Len>3 ->
	  case lists:nthtail(Len-4,H) of
	    ".swr" -> 
	      filter(T, [H|Acc]);
	    _ ->
	      case H of
		".toolbox" ->
		  filter(T, [H|Acc]);
		_ ->
		  filter(T, Acc)
	      end
	  end;
	true -> filter(T, Acc)
      end
  end;
filter([], Acc) -> Acc.
    
up_one_dir(Dir) ->
    L =string:tokens(Dir,[$/,$\\]),
    lists:flatten(rem_last(L)).

rem_last([Last]) ->
    [$/];
rem_last([Head|Tail]) ->
    [$/,Head|rem_last(Tail)];
rem_last([]) ->
    [$/].

insert(List, Member) ->
  case lists:member(Member, List) of
    true -> List;
    false -> [Member|List]
  end.


%% ----------------------------------------
%% done
