%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	gen_file_dialog.erl
%%  Module   :	gen_file_dialog
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-10-28 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(gen_file_dialog).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/3 $').
-revision('$Revision: 1.4 $').

-export([start/2, init/2, request/1, stop/1]).

start(Gs, Opts) ->
  spawn(gen_file_dialog, init, [Gs, Opts]).
  

init(Gs,Opts) ->
  gsb_run:start_sw(Gs, gen_file_dialog, Opts),
  {ok, Dir} = file:get_cwd(),
  State = gsb_run:get_value(state, Opts),
  File = gsb_run:get_value(file, Opts),
  refresh(Dir),
  gs:config(entry, {text, File}),
  gs:config(gen_file_dialog, {map, false}),
  loop(Dir, self(),Gs, State).

request(Pid) ->
  Pid!{gen_file_dialog, request,self()},
  receive
    {gen_file_dialog, Result} ->
      Result
  end.

stop(Pid) ->
  Pid!{gen_file_dialog, stop}.

loop(Dir, Owner, Gs, State) ->
  receive
    {gs,ok,click,_,_} ->
      entered_name(Dir,Owner,Gs, State);
    {gs,cancel,click,_,_} ->
      Owner ! {gen_file_dialog,cancel},
      Dir;
    {gs,entry,keypress,_,['Return'|_]} ->
      entered_name(Dir,Owner, Gs,State);
    {gs,entry,keypress,_,[Keysym|_]} ->
      loop(Dir,Owner, Gs,State);
    {gs,lb,click,_,_} ->
      clicked(Dir,Owner, Gs,State);
    {gs,lb,doubleclick,_,_} ->
      double_clicked(Dir,Owner,Gs, State);
    {gs,win,configure,_,_} ->
      gs:config(win,[{width, 282}, {height, 300}]),
      loop(Dir,Owner, Gs,State);
    stop ->
      exit(normal);
    {gs,_,destroy,_,_} -> 
      Owner ! {gen_file_dialog,cancel},
      Dir;
    {gen_file_dialog, request, From} ->
      gs:config(gen_file_dialog, {map, true}),
      loop(Dir,From,Gs,State);    
    {gen_file_dialog, stop} ->
      true;
    X ->
      io:format("gen_file_dialog: got other: ~w.~n",[X]),
      loop(Dir,Owner, Gs, State)
  end.

refresh(Dir) ->
  gs:config(lb,clear),
  gs:config(label,{label, {text,Dir}}),
  gs:config(entry,{text,""}),
  Items=["../"|get_files(Dir)],
  gs:config(lb,[{items,Items}]),
  Items.

entered_name(Dir,Owner, Gs, State) ->
  File=gs:read(entry,text),
  case check_file(Dir,File) of
    {file,Dir2,File2} ->
      case State of
	load ->
	  Owner ! {gen_file_dialog,{ok,Dir2,File2}},
	  gs:config(gen_file_dialog, {map, false});
	save ->
	  WarningText = lists:append(["Overwrite ", File2, "?"]),
	  ObjId = gs:read(Gs, id),
	  case messages:warning(ObjId, WarningText) of
	    ok ->
	      Owner ! {gen_file_dialog,{ok,Dir2,File2}},
	      gs:config(gen_file_dialog, {map, false});
	    cancel ->
	      loop(Dir, Owner, Gs, State)
	  end
      end;
    {dir,Dir2} ->
      refresh(Dir2),
      loop(Dir2,Owner, Gs, State);
    {error,no_file} ->
      case State of
	load ->
	  double_clicked(Dir,Owner, Gs, State);
	save ->
	  Owner ! {gen_file_dialog,{ok,Dir,File}},
	  gs:config(gen_file_dialog, {map, false})
      end;
    {error,bad_file} ->
      case State of
	load ->
	  double_clicked(Dir,Owner, Gs, State);
	save ->
	  Owner ! {gen_file_dialog,{ok,Dir,File}},
	  gs:config(gen_file_dialog, {map, false})
      end;
    _ ->
      loop(Dir,Owner, Gs, State)
  end.


clicked(Dir,Owner, Gs, State) ->
  [Idx|_]=gs:read(lb,selection),
  File=gs:read(lb,{get,Idx}),
  case lists:last(File) of
    $/ -> %it's a dir
      true;
    _ -> % it's a file
      gs:config(entry,{text,File})
  end,
  loop(Dir,Owner, Gs, State).


double_clicked(Dir,Owner, Gs,  State) ->
  case gs:read(lb,selection) of
    [0] -> % up one dir
      NewDir=up_one_dir(Dir),
      refresh(NewDir),
      loop(NewDir,Owner, Gs, State);
    [] ->
      loop(Dir,Owner, Gs, State);
    [Idx] ->
      File=gs:read(lb,{get,Idx}),
      case lists:last(File) of
	$/ -> % down a dir
	  NewDir=lists:append(Dir,File),
	  refresh(NewDir),
	  loop(NewDir,Owner, Gs, State);
	_ -> % done
	  case State of
	    load ->
	      Owner!{gen_file_dialog,{ok,Dir,File}},
	      gs:config(gen_file_dialog, {map, false});
	    save ->
	      WarningText = lists:append(["Overwrite ", File, "?"]),
	      case messages:warning(Gs, WarningText) of
		ok ->
		  Owner ! {gen_file_dialog,{ok,Dir,File}},
		  gs:config(gen_file_dialog, {map, false});
		cancel ->
		  loop(Dir, Owner, Gs, State)
	      end
	  end
      end
  end.




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
	  case lists:nthtail(Len-3,H) of
	    ".rc" -> filter(T, [H|Acc]);
	    _ -> filter(T, Acc)
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
		  
%% ----------------------------------------
%% done


