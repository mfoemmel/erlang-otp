%% ------------------------------------------------------------
%% File Selection Dialog
%% Written by Ola Samuelsson in July 1995
%% © Ericsson Infocom Consultants AB / Erlang Systems Division
%% ------------------------------------------------------------

-module(file_dialog).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.9 $').
-author('ola@erlang.ericsson.se').
-modified('$Date: 1996/11/01 10:30:55 $').
-modified_by('$Author: fredriks $').

-export([start/1,start/2,start/3,fs_init/4, check_file/2]).


%% ----- File Selection ----
start(State) ->
  {ok,Dir}=file:get_cwd(),
  start(Dir,[], State).

start(Dir, State) ->
  start(Dir,[], State).

start(Dir,File, State) ->
  Dir0 = case lists:last(Dir) of
	   $/ -> Dir;
	   _  -> lists:append(Dir,"/")
	 end,
  Pid=spawn(file_dialog,fs_init,[Dir0,File,self(), State]),	
  receive
    {file_dialog,Pid,Result} -> Result
  end.


%% ------------------------------------------------------------
fs_init(Dir,File,Owner, State) ->
  Gs = gs:start(),
  gsb_run:start(Gs,file_dialog),
  Items=refresh(Dir),
  %% --- select File if it's given ---
  gs:config(entry, {text, File}),
  case index_member(File,Items) of
    {ok,Index} ->
      gs:config(lb,{selection,clear}),
      gs:config(lb,{selection,Index});
    _ -> true
  end,
  gs:config(win,{map,true}),
  fs_loop(Dir,Owner, Gs, State).

fs_loop(Dir,Owner, Gs, State) ->
  receive
    {gs,ok,click,_,_} ->
      entered_name(Dir,Owner,Gs, State);
    {gs,cancel,click,_,_} ->
      Owner ! {file_dialog,self(),cancel};
    {gs,entry,keypress,_,['Return'|_]} ->
      entered_name(Dir,Owner, Gs,State);
    {gs,entry,keypress,_,[Keysym|_]} ->
      fs_loop(Dir,Owner, Gs,State);
    {gs,lb,click,_,_} ->
      clicked(Dir,Owner, Gs,State);
    {gs,lb,doubleclick,_,_} ->
      double_clicked(Dir,Owner,Gs, State);
    {gs,win,configure,_,[250,265|_]} -> % already got that size
      fs_loop(Dir,Owner, Gs,State);
    {gs,win,configure,_,_} ->
      gs:config(win,[{width, 282}, {height, 300}]),
      fs_loop(Dir,Owner, Gs,State);
    stop ->
      exit(normal);
    {gs,_,destroy,_,_} -> 
      Owner ! {file_dialog,self(),cancel};
    X ->
      io:format("file_dialog: got other: ~w.~n",[X]),
      fs_loop(Dir,Owner, Gs, State)
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
	  Owner ! {file_dialog,self(),{ok,Dir2,File2}};
	save ->
	  WarningText = lists:append(["Overwrite ", File2, "?"]),
	  ObjId = gs:read(Gs, id),
	  case messages:warning(ObjId, WarningText) of
	    ok ->
	      Owner ! {file_dialog,self(),{ok,Dir2,File2}};
	    cancel ->
	      fs_loop(Dir, Owner, Gs, State)
	  end
      end;
    {dir,Dir2} ->
      refresh(Dir2),
      fs_loop(Dir2,Owner, Gs, State);
    {error,no_file} ->
      case State of
	load ->
	  double_clicked(Dir,Owner, Gs, State);
	save ->
	  Owner ! {file_dialog,self(),{ok,Dir,File}}
      end;
    {error,bad_file} ->
      case State of
	load ->
	  double_clicked(Dir,Owner, Gs, State);
	save ->
	  Owner ! {file_dialog,self(),{ok,Dir,File}}
      end;
    _ ->
      fs_loop(Dir,Owner, Gs, State)
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
  fs_loop(Dir,Owner, Gs, State).


double_clicked(Dir,Owner, Gs,  State) ->
  case gs:read(lb,selection) of
    [0] -> % up one dir
      NewDir=up_one_dir(Dir),
      refresh(NewDir),
      fs_loop(NewDir,Owner, Gs, State);
    [] ->
      fs_loop(Dir,Owner, Gs, State);
    [Idx] ->
      File=gs:read(lb,{get,Idx}),
      case lists:last(File) of
	$/ -> % down a dir
	  NewDir=lists:append(Dir,File),
	  refresh(NewDir),
	  fs_loop(NewDir,Owner, Gs, State);
	_ -> % done
	  case State of
	    load ->
	      Owner!{file_dialog,self(),{ok,Dir,File}};
	    save ->
	      WarningText = lists:append(["Overwrite ", File, "?"]),
	      case messages:warning(Gs, WarningText) of
		ok ->
		  Owner ! {file_dialog,self(),{ok,Dir,File}};
		cancel ->
		  fs_loop(Dir, Owner, Gs, State)
	      end
	  end
      end
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
	  case lists:nthtail(Len-3,H) of
	    ".rc" -> filter(T, [H|Acc]);
	    _ -> filter(T, Acc)
	  end;
	true -> filter(T, Acc)
      end
  end;
filter([], Acc) -> Acc.
    
%% like member but also returns index
index_member(Item,List) ->
    i_m(0,Item,List).

i_m(N,Item,[Item|List]) ->
    {ok,N};
i_m(N,Item,[_|List]) ->
    i_m(N+1,Item,List);
i_m(N,Item,[]) ->
    false.

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
