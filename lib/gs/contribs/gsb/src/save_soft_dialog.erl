%% Copyright (c) 1996 by Fredrik Ström o Peter Molin.  All Rights Reserved 
%% Time-stamp: <>
%% ====================================================================
%%  Filename : 	save_soft_dialog.erl
%%  Module   :	save_soft_dialog
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1996-09-17 Peter Molin EX (peterm@erlang.ericsson.se): Created.
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(save_soft_dialog).
-copyright('Copyright (c) 1991-97 Ericsson Telecom AB').
-vsn('$Revision: /main/release/2 $').
-revision('$Revision: 1.7 $').
-export([start/1,fs_init/2]).

-record(save, {dir, module, owner, gs, file}).
%% ----- File Selection ----
start(Gs) ->
  ObjId = gs:read(Gs,id),
  Pid=spawn(save_soft_dialog,fs_init,[ObjId,self()]),	
  receive
    {save_soft_dialog,Result} -> Result
  end.


%% ------------------------------------------------------------
fs_init(Gs,Owner) ->
  gsb_run:start(Gs,save_soft_dialog),
  %% --- select File if it's given ---
  gs:config(win,{map,true}),
  {ok,Dir} = file:get_cwd(),
  fs_loop(#save{dir = Dir, owner = Owner, gs = Gs}).


fs_loop(State) ->
  receive
    {gs,ok,click,_,_} ->
      are_we_done(State);
    {gs,cancel,click,_,_} ->
      State#save.owner ! {save_soft_dialog,cancel};
    {gs, entry_module, keypress,_,['Return'|_]} ->
      are_we_done(State);
    {gs, entry_exports, keypress,_,['Return'|_]} ->
      are_we_done(State);
    {gs,_,keypress,_,[Keysym|_]} ->
      fs_loop(State);
    stop ->
      exit(normal);
    {gs,_,destroy,_,_} -> 
      State#save.owner ! {save_soft_dialog,cancel};
    X ->
      messages:debug("save_soft_dialog: got other: ~w.~n",[X],
		    save_soft_dialog, fs_loop),
      fs_loop(State)
  end.

get_second_element([F|R], Acc) ->
  get_second_element(R, [element(1, F)|Acc]);
get_second_element([], Acc) -> Acc.

are_we_done(State) ->
  Module = gs:read(entry_module,text),
  File=lists:append([Module, ".swr"]),
  case Module of 
    [] ->
      fs_loop(State);
    _ ->
      case file:file_info(lists:append([State#save.dir,"/",File])) of
	{ok,_} ->
	  WarningText = lists:append(["Overwrite ", File, "?"]),
	  ObjId = gs:read(State#save.gs, id),
	  case messages:warning(ObjId, WarningText) of
	    ok ->
	      check_exports(State#save{module = list_to_atom(Module),
				   file = File});
	    cancel ->
	      fs_loop(State)
	  end;
	{error, _} ->
	  check_exports(State#save{module= list_to_atom(Module),
				   file = File})
      end
  end.

check_exports(State) ->
  case parse(gs:read(entry_exports, text)) of
    undefined ->
      L = length(gs:read(entry_exports, text)),
      gs:config(entry_exports, {select, {0,L}}),
      fs_loop(State);
    Exports ->
      State#save.owner!{save_soft_dialog,
			{ok, State#save.dir,
			 State#save.file,
			 State#save.module,
			 Exports}}
  end.

parse(String) ->
  case erl_scan:string(lists:append([String, ". "])) of
    {ok, Tokens, _} ->
      case erl_parse:parse_term(Tokens) of
	{ok, Term} ->
	  check(Term);
	{error, _} -> undefined
      end;
    {error, _} -> undefined
  end.

check(Term) when list(Term) -> check_for_tuples(Term, []);
check(_) -> undefined.

check_for_tuples([First|Rest], Acc) when tuple(First) ->
  check_for_tuples(Rest, [First|Acc]);
check_for_tuples([First|Rest], Acc) when atom(First) ->
  check_for_tuples(Rest, [First|Acc]);
check_for_tuples([], Acc) -> Acc;
check_for_tuples(_else, _Acc) -> undefined.


