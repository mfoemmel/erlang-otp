%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_icode_exceptions.erl
%%  Module   :	hipe_icode_exceptions
%%  Purpose  :  Rewrite calls in intermediate code to use cont
%%              and fail-to labels.
%%
%%              The translation from BEAM to Icode handles catches
%%		as follows:
%%                - A push_catch(HandlerLabel) starts a catch-block
%%                  which stretches until a remove_catch instr.
%%                - The handler begins with a restore_catch instr.
%%              This pass removes all special catch instructions and
%%              rewrites calls within a catch to use a fail-to label.
%%  Notes    :  As of November 2003, primops that do not fail in the 
%%              normal sense are allowed to have a fail-label even
%%              before this pass. (Used for the mbox-empty + get_msg
%%              primitive in receives.)
%%
%%              Native floating point operations cannot fail in the
%%              normal sense. Instead they throw a hardware exception
%%              which will be caught by a special fp check error
%%              instruction. These primops do not need a fail label
%%              even in a catch, this pass checks for this with
%%              hipe_icode:call_fails, if a call cannot fail, no fail
%%              label is added.
%%
%%              Explicit fails (exit, error and throw) inside
%%              a catch have to be handled. They have to build their
%%              exit value and jump directly to the catch handler. An
%%              alternative solution would be to have a new type of
%%              fail instruction that takes a fail-to label...
%%
%%              When the new `try' construct is introduced, this whole
%%              approach has to be re-examined.
%%
%%  History  :	* 2000-11-15  Erik Johansson (happi@csd.uu.se): 
%%               New scheme for handling of catches.
%%              * 2003-11-07  Erik Stenman:
%%               Allow non-guards to have fail-to labels.
%%  CVS:
%%    $Id$
%%
%% ====================================================================
%%
%%  TODO     : Handle more instructions that cannot fail in the same
%%             way as fp-ops. 
%%             Move the call_fails to hipe_bif or some other nice place
%%             that knows about HiPE primops.
%%
%% ====================================================================
%%
%%
%% Code like:
%%
%%   L1: push_catch -> L3, L2
%%   L2:
%%       ...
%%       fail(R,exit) 
%% 
%%   L3: V1 = restore_catch L3
%%
%% Will be rewritten to:
%%   L1: 
%%       ...
%%       V2 = {exit,R}
%% 
%%   L3: V1 = V2
%%
%%
%% 
%% Code like:
%%
%%   L1: push_catch -> L3, L4
%%   L4:
%%       ...
%%       V2 = call(foo:bar/0,[])
%%   L2: remove_catch L3
%%       ...
%%       goto L4
%% 
%%   L3: V1 = restore_catch L3
%%
%% Will be rewritten to:
%%   L1: 
%%       ...
%%       V2 = call(foo:bar/0,[]) Fail to L3
%%   L2: 
%%       ...
%%       goto L4
%% 
%%   L3: 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_exceptions).
-export([fix_catches/1]).

%-ifndef(DEBUG).
%-define(DEBUG,1).  
%-endif.
-include("../main/hipe.hrl").

%%=====================================================================

%% @spec fix_catches(icode_cfg()) -> icode_cfg()
%%
%% @doc
%%    Rewrites the Icode (in its CFG form) so that it uses the new
%%    scheme for handling catches.
%% @end

fix_catches(IcodeCFG) ->
  ?IF_DEBUG(hipe_icode_cfg:pp(IcodeCFG),ok),
  ?opt_start_timer("Init"),
  Icode = hipe_icode_cfg:cfg_to_linear(IcodeCFG),
  State0 = empty_state(IcodeCFG),
  ?opt_stop_timer("Init"),  

  ?opt_start_timer("Find catches"),
  State1 = fixpoint(State0), 
  ?opt_stop_timer("Find catches"),
  
  ?opt_start_timer("Linear"),
  Icode2 = hipe_icode_cfg:cfg_to_linear(cfg(State1)),
  ?opt_stop_timer("Linear"),
  Code2 = hipe_icode:icode_code(Icode2),
  
  MFA = hipe_icode:icode_fun(Icode2),
  ?opt_start_timer("Update"),
  NewCode = rewrite(Code2, reset_visited(State1), MFA),
  ?opt_stop_timer("Update"),
  
  ICode2 = hipe_icode:icode_code_update(Icode, NewCode),
  
  CFG3 = hipe_icode_cfg:linear_to_cfg(ICode2),
  hipe_icode_cfg:remove_unreachable_code(CFG3).


fixpoint(State) ->
  State1 = find_catches(start(State), State),
  case is_changed(State1) of 
    true ->
      ?debug_msg("\n\nNew fixpoint iteration:\n\n",[]),
      fixpoint(reset_visited(unchange(State1)));
    false ->
      State1
  end.

%%---------------------------------------------------------------------

find_catches(AllLs=[L|Labels], State) ->
  case visited(L,State) of
    true ->
      find_catches(Labels, State);
    false ->
      BB = hipe_icode_cfg:bb(cfg(State), L),
      Code = hipe_bb:code(BB),
      case catches_in(L,State) of
	{ConflictingPred, CatchesIn, _CI2} ->
	  handle_bb_merge(AllLs,State,Code,ConflictingPred,CatchesIn);
	CatchesIn ->
	  handle_bb(AllLs,State,Code,CatchesIn)
      end
  end;
find_catches([], State) ->
  State.

handle_bb([L|Labels],State,Code,CatchesIn) ->	
  {CatchesOut, State0} = in_catch(Code, CatchesIn, State),
  %% Mark BB as visited.
  State1 = visit(L,State0),
  OldCOut = get_catches_out(L,State),
  State2 = catches_out(L,CatchesOut,State1),
  %% Has the state changed?
  State3 = 
    if OldCOut == CatchesOut -> State2;
       true -> change(State2)
    end,
  find_catches(succs(State3, L) ++ Labels, State3).

handle_bb_merge([L|Labels],State,Code,ConflictingPred,CatchesIn) ->
  %% This block is used by several catches
  %% TODO: A new block is needed for one of the catches.
  L2i = hipe_icode:mk_new_label(),
  L2 = hipe_icode:label_name(L2i), 
  CFG0 = hipe_icode_cfg:bb_add(cfg(State), L2, hipe_bb:mk_bb(Code)),
  PredBB = hipe_icode_cfg:bb(cfg(State), ConflictingPred),
  Jmp = hipe_bb:last(PredBB),
  PredCode = hipe_bb:butlast(PredBB) ++ [hipe_icode:redirect_jmp(Jmp, L, L2)],
  CFG1 = hipe_icode_cfg:bb_add(CFG0, ConflictingPred, hipe_bb:mk_bb(PredCode)),

  State0 = set_cfg(State, CFG1),

  check_multicatch(Code, CatchesIn, State),
  State1 = visit(L,State0),
  State2 = catches_out(L,CatchesIn,State1),
  find_catches([L2|succs(State2, L)] ++ Labels, State2).
	
%%---------------------------------------------------------------------
%% This is just an assert.
check_multicatch([], Cs, State) -> {Cs, State};
check_multicatch([I|Is], Cs, State) ->
  case hipe_icode:type(I) of
    remove_catch -> exit(nooo);
    restore_catch -> exit(nooo);
    pushcatch -> exit(nooo);
    _ ->
      check_multicatch(Is, Cs, State)
  end.

%%---------------------------------------------------------------------
in_catch([], Cs, State) ->
  {Cs, State};
in_catch([I|Is], Cs, State) ->
  case hipe_icode:type(I) of
    remove_catch ->
      Id = hipe_icode:remove_catch_label(I),
      case Cs of
	[Id|Rest] ->
	  in_catch(Is, Rest, State);
	_Other ->
	  in_catch(Is, Cs, State)
      end;
    restore_catch ->
      Id = hipe_icode:restore_catch_label(I),
      Var = hipe_icode:restore_catch_reason_dst(I),
      Var1 = hipe_icode:restore_catch_type_dst(I),
      Lbl = hipe_icode:mk_new_label(),
      %% msg("\n ~w restore to ~w\n",[Id,Var]),
      State2 = set_dest(State, Id, {Var,Var1}),
      State3 = set_shortcut(State2, Id, Lbl),
      case Cs of
	[Id|Rest] ->
	  in_catch(Is, Rest, State3);
	_Other ->
	  in_catch(Is, Cs, State3)
      end;
    pushcatch ->
      Id = hipe_icode:pushcatch_label(I),
      %% msg("In catch: ~w\n",[Id]),
      case Cs of
	unknown -> 
	  in_catch(Is, [Id], State);
	_ ->
	  in_catch(Is, [Id| Cs], State)
      end;
    _ ->
      in_catch(Is, Cs, State)
  end.

%%---------------------------------------------------------------------
%%---------------------------------------------------------------------

rewrite(Code, State, MFA) ->
  rewrite(Code, [], State, [], MFA).

%% TODO: Break this up into functions...
rewrite([], _Catches, _State, Acc, _) ->
  lists:reverse(Acc);
rewrite(AllIs=[I|Is], Catches, State, Acc, MFA) ->
  ?debug_msg("To ~w\n",[case Acc of
			  [PI,PII|_] -> [PII,PI];
			  [PI] -> PI;
			  _ -> none
			end]),
  ?debug_msg("~w ~w\n",[I, Catches]),
  case hipe_icode:type(I) of
    label -> %% A new BB, get catches to this BB
      L = hipe_icode:label_name(I),
      CatchesIn = catches_in(L,State),
      rewrite(Is, CatchesIn, State, [I|Acc],MFA);
    remove_catch ->
      Id = hipe_icode:remove_catch_label(I),
      case Catches of
	[Id|Rest]  ->
	  rewrite(Is, Rest, State, Acc, MFA);
	Other ->
	  %% In the failpart...
	  rewrite(Is, Other, State, Acc, MFA)
      end;
    restore_catch ->
      Id = hipe_icode:restore_catch_label(I),
      L = get_shortcut(Id,State),
      %% Create the code in reverse order...
      Code = [L,hipe_icode:mk_goto(hipe_icode:label_name(L)),I|Acc],
      rewrite(Is, Catches, State, Code, MFA);
    pushcatch ->
      Id = hipe_icode:pushcatch_label(I),
      rewrite(Is, [Id |Catches], State, Acc, MFA);
    call -> %% A call has to be updated with the rigth fail-to
      rewrite_call(AllIs, Catches, State, Acc, MFA);
    enter ->
      case Catches of
	[_|_] -> %% This could be turned into an Assert...
	  %% Doing a tailcall in a catch is not good!
	  ?EXIT({tailcallincatch,I});
 	[] ->
	  rewrite(Is, Catches, State, [I|Acc], MFA)
      end;
    fail ->
      case Catches of
	[_|_] -> %% Fail in a catch, has to create value
	         %% and jump to handler. 
	  rewrite_fail(AllIs, Catches, State, Acc, MFA);
	_ -> 
	  rewrite(Is, Catches, State, [I|Acc], MFA)
      end;
    _ ->
      rewrite(Is, Catches, State, [I|Acc], MFA)
  end.


rewrite_call(AllIs, Catches, State, Acc, MFA)->
  InGuard = hipe_icode:call_in_guard(hd(AllIs)),
  case Catches of
    [_|_] -> %% A call in a catch
      rewrite_call_in_catch(InGuard, AllIs, Catches, State, Acc, MFA);
    _ ->     %% A call outside a catch
      rewrite_call_no_catch(InGuard, AllIs, Catches, State, Acc, MFA)
  end.


rewrite_call_in_catch(true, [I|Is] , Catches, State, Acc, MFA) ->
  %% If the catch contains guard-like instructions,
  %%  then we already have a fail label.
  rewrite(Is, Catches, State, [I|Acc], MFA);
rewrite_call_in_catch(false, [I|Is] , Catches, State, Acc, MFA) ->
  case hipe_icode:call_fails(I) of
    true ->
      NewCall = hipe_icode:call_set_fail_label(I,hd(Catches)),
      case  hipe_icode:call_continuation(I) of 
	[] -> %% This was a fallthrough before.
	  NewLab = hipe_icode:mk_new_label(),
	  LabName = hipe_icode:label_name(NewLab),
	  NewCall2 = hipe_icode:call_set_continuation(NewCall,LabName),
	  rewrite(Is, Catches, State, [NewLab,NewCall2|Acc], MFA);
	_ -> %% Has a continuation
	  case hipe_icode:call_fail_label(I) of 
	    [] -> %% We have no fail label... so we add one
	      rewrite(Is, Catches, State, [NewCall|Acc], MFA);
	    _ ->  %% Don't touch a call that already has a fail-to
	      rewrite(Is, Catches, State, [I|Acc], MFA)
	  end
      end;
    false -> %% This call doesn't need a fail label
      rewrite(Is, Catches, State, [I|Acc], MFA)
  end.


rewrite_call_no_catch(true, [I|Is], Catches, State, Acc, MFA) ->
  case  hipe_icode:call_continuation(I) of 
    [] -> %% This was a fallthrough before.
      NewLab = hipe_icode:mk_new_label(),
      LabName = hipe_icode:label_name(NewLab),
      NewCall = hipe_icode:call_set_continuation(I,LabName),
      rewrite(Is, Catches, State, [NewLab,NewCall|Acc], MFA);
    _ ->
      rewrite(Is, Catches, State, [I|Acc], MFA)
  end;
rewrite_call_no_catch(false, [I|Is], Catches, State, Acc, MFA) ->
  case hipe_icode:call_continuation(I) of 
    [] -> rewrite(Is, Catches, State, [I|Acc], MFA);
    Cont -> %% We have a cont label
      %% Can we remove it 
      case hipe_icode:call_fail_label(I) of 
	[] -> %% We have no fail label...
	  %% safe to get rid of cont from call...
	  NewI = hipe_icode:call_set_continuation(hipe_icode:call_set_fail_label(I,[]),[]),
	  Goto = hipe_icode:mk_goto(Cont),
	  rewrite(Is, Catches, State, [Goto,NewI|Acc], MFA);
	_ ->
	  rewrite(Is, Catches, State, [I|Acc], MFA)
      end
  end.


rewrite_fail([I|Is], Catches, State, Acc, MFA) ->
  C = hd(Catches),
  {Dst1,_Dest2} = get_dest(C,State),
  Shortcut = hipe_icode:label_name(get_shortcut(C, State)),
  ExitVar = hipe_icode:mk_new_var(),
  MkExitAtom = hipe_icode:mk_move(ExitVar, hipe_icode:mk_const('EXIT')),
  TmpVar = hipe_icode:mk_new_var(),
  Src = hd(hipe_icode:fail_reason(I)),
%% NEW_EXCEPTIONS
%%   Reason = hipe_icode:fail_reason(I),
%%   Src = hd(Reason),
%%   Code = 
%%     case {hipe_icode:fail_type(I), length(Reason)} of
%%       {exit, 1} -> 
  Code = 
    case hipe_icode:fail_type(I) of
      exit ->
	%% Build an exit term and put it in Dst.
	%% Dst = {'EXIT', Src}
	T = hipe_icode:mk_primop([Dst1], mktuple, [ExitVar,Src], Shortcut, []),
	%% The code will be reversed, and is created reversed now.
	[T, MkExitAtom | Acc];
%% NEW_EXCEPTIONS
%%       {error, 1} -> 
      fault -> 
	%% Build an error value {'EXIT', {Src, Stacktrace}}
	%% For now the StackTrace will be [MFA]
	StackTraceVar = hipe_icode:mk_new_var(),
	MkStackTraceInstr = 
	  hipe_icode:mk_move(StackTraceVar, hipe_icode:mk_const([MFA])),
	MkTuple1Instr = 
	  hipe_icode:mk_primop([TmpVar], mktuple, [Src,StackTraceVar]),
	MkTuple2Instr =
	  hipe_icode:mk_primop([Dst1], mktuple, [ExitVar, TmpVar], 
			       Shortcut, []),
	%% The code will be reversed, and is created reversed now.
	[MkTuple2Instr, MkExitAtom, MkTuple1Instr, MkStackTraceInstr |
	 Acc];
%% NEW_EXCEPTIONS
%%       {{error, Atom}, 1} -> 
%% 	%% Build an error value {'EXIT', {{Atom, Src}, Stacktrace}}
%% 	%% For now the StackTrace will be [MFA]
%% 	StackTraceVar = hipe_icode:mk_new_var(),
%% 	MkStackTraceInstr = 
%% 	  hipe_icode:mk_move(StackTraceVar, hipe_icode:mk_const([MFA])),
%% 	MkTuple0Instr = 
%% 	  hipe_icode:mk_primop([TmpVar], mktuple,
%% 			       [hipe_icode:mk_const(Atom), Src]),
%% 	MkTuple1Instr = 
%% 	  hipe_icode:mk_primop([TmpVar], mktuple, [TmpVar,StackTraceVar]),
%% 	MkTuple2Instr =
%% 	  hipe_icode:mk_primop([Dst1], mktuple, [ExitVar, TmpVar], 
%% 			       Shortcut, []),
%% 	%% The code will be reversed, and is created reversed now.
%% 	[MkTuple2Instr, MkExitAtom, MkTuple1Instr, MkTuple0Instr,
%% 	 MkStackTraceInstr |
%% 	 Acc];
%%       {error, 2} -> 
      fault2 -> 
	%% Build an error value {'EXIT', {Src, Stacktrace}}
	%% For now the stacktrace will be [MFA]
	[_,StackTraceVar] = hipe_icode:fail_reason(I),
	MkTuple1Instr = 
	  hipe_icode:mk_primop([TmpVar], mktuple, [Src,StackTraceVar]),
	MkTuple2Instr =
	  hipe_icode:mk_primop([Dst1], mktuple, [ExitVar, TmpVar],
			       Shortcut, []),
	%% The code will be reversed, and is created reversed now.
	[MkTuple2Instr, MkExitAtom, MkTuple1Instr | Acc];
%% NEW_EXCEPTIONS
%%       {throw, 1} ->
      throw ->
	%% Dst = Src
	%% The code will be reversed, and is created reversed now. 
	[hipe_icode:mk_goto(Shortcut), hipe_icode:mk_move(Dst1, Src)|Acc]
    end,
  rewrite(Is, Catches, State, Code, MFA).

%%---------------------------------------------------------------------
%%---------------------------------------------------------------------

-record(state,{
	  visited, 
	  cfg, 
	  pred_map,
	  succ_map,
	  catches_out, 
	  dest,
	  start,
	  entry,
	  shortcuts,
	  changed
	 }).

empty_state(CFG) ->
  #state{visited = hipe_icode_cfg:none_visited(),
	 cfg = CFG,
	 pred_map = hipe_icode_cfg:pred_map(CFG),
	 succ_map = hipe_icode_cfg:succ_map(CFG),
	 start = [hipe_icode_cfg:start_label(CFG)],
	 entry = hipe_icode_cfg:start_label(CFG),
	 catches_out = empty_map(),
	 dest = empty_map(),
	 shortcuts = empty_map(),
	 changed = false
	}.

set_cfg(State, CFG) ->
  State#state{cfg = CFG,
	      pred_map = hipe_icode_cfg:pred_map(CFG),
	      succ_map = hipe_icode_cfg:succ_map(CFG),
	      changed = true
	     }.

vis(#state{visited=Visited}) -> Visited.
set_vis(State,Vis) -> State#state{visited=Vis}.
cfg(#state{cfg=CFG}) -> CFG.
pred_map(#state{pred_map=PredMap}) -> PredMap.
succ_map(#state{succ_map=SuccMap}) -> SuccMap.
catches_out(#state{catches_out=CatchMap}) -> CatchMap.
set_catches_out(State, Map) -> State#state{catches_out=Map}.
dest(#state{dest=Dst}) -> Dst.
start(#state{start=Start}) -> Start.
entry(#state{entry=Entry}) -> Entry.
set_dest(State, C, V) -> 
  Map = dest(State),
  NewMap = gb_trees:enter(C, V, Map),
  State#state{dest=NewMap}.
shortcuts(#state{shortcuts=ShortCuts}) -> ShortCuts.
set_shortcut(State, C, L) -> 
  Map = shortcuts(State),
  NewMap = gb_trees:enter(C, L, Map),
  State#state{shortcuts=NewMap}.
change(State) ->
  State#state{changed=true}.
unchange(State) ->
  State#state{changed=false}.
is_changed(#state{changed=Changed}) -> Changed.

preds(State, L) ->
   hipe_icode_cfg:pred(pred_map(State), L).
succs(State, L) ->
   hipe_icode_cfg:succ(succ_map(State), L).
  
visited(L, State) ->
  hipe_icode_cfg:visited(L, vis(State)).

visit(L,State) ->
  set_vis(State, hipe_icode_cfg:visit(L, vis(State))).

reset_visited(State) ->
  %% CFG = cfg(State),
  set_vis(State, hipe_icode_cfg:none_visited()).

empty_map() -> gb_trees:empty().

catches_in(L,State) ->
  Preds = preds(State, L),
  get_all_catches_in(Preds, catches_out(State),
		     case L == entry(State) of
		       true ->
			 [];
		       _ ->
			 unknown
		     end).

get_all_catches_in([P|Ps], Map, unknown) ->
  case gb_trees:lookup(P,Map) of
    {value, V} ->
      get_all_catches_in(Ps, Map, V);
    none ->
      get_all_catches_in(Ps, Map, unknown)
  end;
get_all_catches_in([P|Ps], Map, Cs) ->
  case gb_trees:lookup(P,Map) of
    {value, Cs} ->
      get_all_catches_in(Ps, Map, Cs);
    {value, unknown} ->
      get_all_catches_in(Ps, Map, Cs);
    {value, Other} ->
      get_all_catches_in(Ps, Map, {P,Cs, Other});
    none ->
      get_all_catches_in(Ps, Map, Cs)
  end;
get_all_catches_in([],_,Cs) -> Cs.

catches_out(L,CatchesOut,State) ->
  Map = catches_out(State),
  set_catches_out(State, gb_trees:enter(L, CatchesOut, Map)).

get_catches_out(L, State) ->
  case gb_trees:lookup(L,catches_out(State)) of
    {value, V} -> V;
    none -> unknown
  end.

get_dest(C, State) ->  
  case gb_trees:lookup(C, dest(State)) of
    {value, V} ->
      V;
    none ->
     hipe_icode:mk_new_var()
  end.

get_shortcut(C, State) ->  
  case gb_trees:lookup(C, shortcuts(State)) of
    {value, L} ->
      L;
    none ->
     hipe_icode:mk_new_label()
  end.

%% has_catches(CFG) ->
%%   Icode = hipe_icode_cfg:cfg_to_linear(CFG),
%%   Code = hipe_icode:icode_code(Icode),
%%   code_has_catches(Code).
%% 
%% code_has_catches([I|Is]) ->
%%   case hipe_icode:type(I) of
%%     pushcatch -> true;
%%     _ -> code_has_catches(Is)
%%   end;
%% code_has_catches([]) ->
%%   false.
