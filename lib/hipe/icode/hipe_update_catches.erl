%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Intermediate Code
%% ====================================================================
%%  Filename : 	hipe_update_catches.erl
%%  Module   :	hipe_update_catches
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-11-15  Erik Johansson (happi@csd.uu.se): 
%%               New scheme for handling of catches.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2002/05/13 16:51:07 $
%%              $Revision: 1.9 $
%% ====================================================================
%%  TODO     :  
%%
%%  Exports  : update_catches(CFG) - Rewrites the icode in CFG to
%%                                   te new catch scheme.
%%
%% ====================================================================
%%
%%
%% Code like:
%%
%%   L1: push_catch L3
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
%%   L1: push_catch L3
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

-module(hipe_update_catches).
-export([update_catches/2, has_catches/1]).

%-ifndef(DEBUG).
%-define(DEBUG,1).
%-endif.
-include("../main/hipe.hrl").

update_catches(CFG, Options) ->
  ?IF_DEBUG(hipe_icode_cfg:pp(CFG),ok),
  ?opt_start_timer("Init"),
  ?option_time(Icode = hipe_icode_cfg:linearize(CFG),Options,"Linear"),
  
  hipe_gensym:init(icode),
  {_IVMin, IVMax} = hipe_icode:icode_var_range(Icode),
  {_ILMin, ILMax} = hipe_icode:icode_label_range(Icode),
  ?ASSERT(begin
	    Code = hipe_icode:icode_code(Icode),
	    ActualVmax = hipe_icode:highest_var(Code),
	    ActualLmax = hipe_icode:highest_label(Code),
	    ((ActualVmax =< IVMax) 
	     and
	     (ActualLmax =< ILMax))
	  end),
  %% io:format("IVMax ~w ActualVmax ~w\n",[IVMax,ActualVmax]),
  hipe_gensym:set_var(icode,IVMax+1),
  hipe_gensym:set_label(icode,ILMax+1),
  
  %% ActualVmax = hipe_icode:highest_var(Code),
  %% ActualLmax = hipe_icode:highest_label(Code),
  %% 
  %% hipe_gensym:set_label(icode,ActualLmax+1),
  %% hipe_gensym:set_var(icode,ActualVmax+1),
  
  State0 = empty_state(CFG),
  
  ?opt_stop_timer("Init"),  

  ?opt_start_timer("Find catches"),
  State1 = fixpoint(State0), 
  ?opt_stop_timer("Find catches"),
  
  ?opt_start_timer("Linear"),
  Icode2 = hipe_icode_cfg:linearize(cfg(State1)),
  ?opt_stop_timer("Linear"),
  Code2 = hipe_icode:icode_code(Icode2),
  
  MFA = hipe_icode:icode_fun(Icode2),
  ?opt_start_timer("Update"),
  NewCode = rewrite(Code2, reset_visited(State1), MFA),
  ?opt_stop_timer("Update"),
  
  ICode2 = hipe_icode:icode_code_update(Icode,NewCode),
  
  CFG3 = hipe_icode_cfg:init(ICode2),
  %% hipe_icode_cfg:pp(CFG3),
  CFG3.

fixpoint(State) ->
  State1 = find_catches(start(State), State),
  case is_changed(State1) of 
    true ->
      ?debug_msg("\n\nNew fixpoint iteration:\n\n",[]),
      fixpoint(reset_visited(unchange(State1)));
    false ->
      State1
  end.

%% ____________________________________________________________________

find_catches([L|Labels], State) ->
  case visited(L,State) of
    true ->
      find_catches(Labels, State);
    false ->
      ?debug_msg("L~w:\n",[L]),
      BB = hipe_icode_cfg:bb(cfg(State), L),
      Code = hipe_bb:code(BB),

      case catches_in(L,State) of
	{ConflictingPred, CatchesIn, _CI2} ->
	  %% This block is used by several catches
	  %% TODO: A new block is needed for one of the catches.
	  L2i = hipe_icode:mk_new_label(),
	  L2 = hipe_icode:label_name(L2i), 
	  CFG0 = hipe_icode_cfg:bb_update(cfg(State), 
					    L2, 
					    hipe_bb:mk_bb(Code)),
	  PredBB = hipe_icode_cfg:bb(cfg(State), ConflictingPred),
	  Jmp = hipe_bb:last(PredBB),
	  PredCode = hipe_bb:butlast(PredBB) ++ 
	    [hipe_icode:redirect_jmp(Jmp, L, L2)],
	  CFG1 = hipe_icode_cfg:bb_update(CFG0, ConflictingPred,
					  hipe_bb:mk_bb(PredCode)),
	  
	  State0 = set_cfg(State, CFG1),

	  check_multicatch(Code, CatchesIn, State),
	  State1 = visit(L,State0),
	  State2 = catches_out(L,CatchesIn,State1),
	  %% hipe_icode_cfg:pp(cfg(State2)),
	  find_catches([L2|succs(State2, L)]
		       ++ Labels,
		       State2);
	
	CatchesIn ->
	  ?debug_msg("In: ~w\n",[CatchesIn]),
	  {CatchesOut, State0} = in_catch(Code, CatchesIn, State),
	  ?debug_msg("Out: ~w\n",[CatchesOut]),
	  State1 = visit(L,State0),
	  OldCOut = get_catches_out(L,State),
	  State2 = catches_out(L,CatchesOut,State1),
	  State3 = 
	    case OldCOut == CatchesOut of
	      true -> State2;
	      false -> change(State2)
	    end,
	  find_catches(succs(State3, L)
		       ++ Labels,
		       State3)
      end
  end;
find_catches([], State) ->
  State.
%% ____________________________________________________________________
check_multicatch([], Cs, State) -> {Cs, State};
check_multicatch([I|Is], Cs, State) ->
  case hipe_icode:type(I) of
    remove_catch -> exit(nooo);
    restore_catch -> exit(nooo);
    pushcatch -> exit(nooo);
    _ ->
      check_multicatch(Is, Cs, State)
  end.

%% ____________________________________________________________________
in_catch([], Cs, State) -> {Cs, State};
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
      Var = hipe_icode:restore_catch_dst(I),
      Lbl = hipe_icode:mk_new_label(),
      %% msg("\n ~w restore to ~w\n",[Id,Var]),
      State2 = set_dest(State, Id, Var),
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


%% ____________________________________________________________________
%% ____________________________________________________________________
rewrite(Code, State, MFA) ->
  rewrite(Code, [], State, [], MFA).

rewrite([], _Catches, _State, Acc, _) ->
  lists:reverse(Acc);
rewrite([I|Is], Catches, State, Acc, MFA) ->
  ?debug_msg("~w ~w\n",[I, Catches]),
  case hipe_icode:type(I) of
    label ->
      L = hipe_icode:label_name(I),
      CatchesIn = catches_in(L,State),
      rewrite(Is, CatchesIn, State, 
	      [hipe_icode:info_update(I,
				      [X ||
					X<-hipe_icode:info(I), X =/= entry]	   )|Acc], MFA);
    remove_catch ->
      Id = hipe_icode:remove_catch_label(I),
      % msg("Leaving catch: ~w\n",[Id]),
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
      % msg("In catch: ~w\n",[Id]),
     rewrite(Is, [Id |Catches], State, Acc, MFA);
    call ->
      InGuard = hipe_icode:call_in_guard(I),
      case Catches of
	[C|Rest] ->
	  if InGuard == true ->
	      %% If the catch contains guardlike instructions
	      %%  then we already have a fail label.
	      rewrite(Is, [C|Rest], State, [I|Acc], MFA);

	     true -> %% Not in a guard.
	     
	      NewCall = hipe_icode:call_set_fail(I, C),
	      case  hipe_icode:call_continuation(I) of 
		[] -> %% This was a fallthrough before.
		  NewLab = hipe_icode:mk_new_label(),
		  LabName = hipe_icode:label_name(NewLab),
		  NewCall2 = hipe_icode:call_set_continuation(NewCall,LabName),
		  rewrite(Is, [C|Rest], State, [NewLab,NewCall2|Acc], MFA);
		_ -> %% Has a continuation
		  rewrite(Is, [C|Rest], State, [NewCall|Acc], MFA)
	      end
	  end;
	[] ->
	  if InGuard == true ->
	      case  hipe_icode:call_continuation(I) of 
		[] -> %% This was a fallthrough before.
		  NewLab = hipe_icode:mk_new_label(),
		  LabName = hipe_icode:label_name(NewLab),
		  NewCall = hipe_icode:call_set_continuation(I,LabName),
		  rewrite(Is, [], State, [NewLab,NewCall|Acc], MFA);
		_ ->
		  rewrite(Is, [], State, [I|Acc], MFA)
	      end;
	     true ->
	      case hipe_icode:call_continuation(I) of 
		[] -> rewrite(Is, [], State, [I|Acc], MFA);
		Cont ->
		  NewI = hipe_icode:call_set_continuation(hipe_icode:call_set_fail(I,[]),[]),
		  Goto=hipe_icode:mk_goto(Cont),
		  rewrite(Is, [], State, [Goto,NewI|Acc], MFA)
	      end
	  end;
 	unknown ->
	  case hipe_icode:call_continuation(I) of 
	    [] -> rewrite(Is, Catches, State, [I|Acc], MFA);
	    Cont ->
	      NewI = hipe_icode:call_set_continuation(hipe_icode:call_set_fail(I,[]),[]),
	      Goto=hipe_icode:mk_goto(Cont),
	      rewrite(Is, Catches, State, [Goto,NewI|Acc], MFA)
	  end
      end;

    enter ->
      case Catches of
	[_|_] -> %% This could be turned into an Assert...
	  %% Doing a tailcall in a catch is not good!
	  ?EXIT({tailcallincatch,I});
 	_ ->
	  rewrite(Is, Catches, State,[I|Acc], MFA)
      end;
 
    fail ->
      case Catches of
	[C|Rest] ->
	  Dst = get_dest(C,State),
	  Shortcut = hipe_icode:label_name(get_shortcut(C, State)),
	  Code = 
	    case hipe_icode:fail_type(I) of
	      exit -> 
		%% Build an exit term and put it in Dst.
		%% Dst = {'EXIT', Src}
		Cont = hipe_icode:mk_new_label(),
		ExitV = hipe_icode:mk_new_var(),
		MkAtom = hipe_icode:mk_mov(ExitV, hipe_icode:mk_const('EXIT')),
		[Src] = hipe_icode:fail_reason(I),
		T = hipe_icode:mk_primop([Dst], mktuple, [ExitV, Src],
					 hipe_icode:label_name(Cont), []),
		%% The code will be reversed, and is created reversed
		%% now.
		[hipe_icode:mk_goto(Shortcut),
		 Cont,
		 T,
		 MkAtom |
		 Acc];
	      fault -> 
		%% Build a faultvalue {'EXIT', {Src, Stacktrace}}
		%% For now the StackTrace will be [MFA]
		Cont1Lab = hipe_icode:mk_new_label(),
		Cont2Lab = hipe_icode:mk_new_label(),
		StackTraceVar = hipe_icode:mk_new_var(),
		TmpVar = hipe_icode:mk_new_var(),
		ExitVar = hipe_icode:mk_new_var(),

		[Src] = hipe_icode:fail_reason(I),
		MkStackTraceInstr = 
		  hipe_icode:mk_mov(StackTraceVar, hipe_icode:mk_const([MFA])),
		MkTuple1Instr = 
		  hipe_icode:mk_primop([TmpVar], mktuple, [Src,StackTraceVar],
				       hipe_icode:label_name(Cont1Lab), []),
		MkAtomInstr = hipe_icode:mk_mov(ExitVar, 
						hipe_icode:mk_const('EXIT')),
		MkTuple2Instr =
		  hipe_icode:mk_primop([Dst], mktuple, [ExitVar, TmpVar],
				       hipe_icode:label_name(Cont2Lab), []),
		%% The code will be reversed, and is created reversed
		%% now.
		[hipe_icode:mk_goto(Shortcut),
		 Cont2Lab,
		 MkTuple2Instr,
		 MkAtomInstr,
		 Cont1Lab,
		 MkTuple1Instr,
		 MkStackTraceInstr |
		 Acc];
	      fault2 -> 
		%% Build a faultvalue {'EXIT', {Src, Stacktrace}}
		%% For now the stacktrace will be [MFA]
		Cont1Lab = hipe_icode:mk_new_label(),
		Cont2Lab = hipe_icode:mk_new_label(),
		TmpVar = hipe_icode:mk_new_var(),
		ExitVar = hipe_icode:mk_new_var(),
		[Src,StackTraceVar] = hipe_icode:fail_reason(I),
		MkTuple1Instr = 
		  hipe_icode:mk_primop([TmpVar], mktuple, [Src,StackTraceVar],
				       hipe_icode:label_name(Cont1Lab), []),
		MkAtomInstr = hipe_icode:mk_mov(ExitVar, 
						hipe_icode:mk_const('EXIT')),
		MkTuple2Instr =
		  hipe_icode:mk_primop([Dst], mktuple, [ExitVar, TmpVar],
				       hipe_icode:label_name(Cont2Lab), []),
		%% The code will be reversed, and is created reversed
		%% now.
		[hipe_icode:mk_goto(Shortcut),
		 Cont2Lab,
		 MkTuple2Instr,
		 MkAtomInstr,
		 Cont1Lab,
		 MkTuple1Instr |
		 Acc];
		
	      throw ->
		[Src] = hipe_icode:fail_reason(I),
		%% Dst = Src
		%% The code will be reversed, and is created reversed
		%% now. 
		[hipe_icode:mk_goto(Shortcut),
		 hipe_icode:mk_mov(Dst, Src)|Acc]
	    end,

	  rewrite(Is, [C|Rest], State, Code, MFA);
	_ ->
	  rewrite(Is, Catches, State, [I|Acc], MFA)
      end;

    _ ->
      rewrite(Is, Catches, State, [I|Acc], MFA)
  end.


%% ____________________________________________________________________
%% ____________________________________________________________________
%% 
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
  #state{visited=hipe_icode_cfg:none_visited(CFG),
	 cfg=CFG,
	 pred_map=hipe_icode_cfg:pred_map(CFG),
	 succ_map=hipe_icode_cfg:succ_map(CFG),
	 start=[hipe_icode_cfg:start(CFG) | 
		hipe_icode_cfg:fail_entrypoints(CFG)],
	 entry=hipe_icode_cfg:start(CFG),
	 catches_out = empty_map(),
	 dest = empty_map(),
	 shortcuts = empty_map(),
	 changed = false}.


set_cfg(State, CFG) ->
  State#state{
    cfg=CFG,
    pred_map=hipe_icode_cfg:pred_map(CFG),
    succ_map=hipe_icode_cfg:succ_map(CFG),
    changed = true
   }.

vis(State) ->  State#state.visited.
set_vis(State,Vis) ->  State#state{visited=Vis}.
cfg(State) -> State#state.cfg.
pred_map(State) -> State#state.pred_map.
succ_map(State) -> State#state.succ_map.
catches_out(State) -> State#state.catches_out.
set_catches_out(State, Map) -> State#state{catches_out=Map}.
dest(State) -> State#state.dest.
start(State) -> State#state.start.
entry(State) -> State#state.entry.
set_dest(State, C, V) -> 
  Map = dest(State),
  NewMap = gb_trees:enter(C, V, Map),
  State#state{dest=NewMap}.
shortcuts(State) -> State#state.shortcuts.
set_shortcut(State, C, L) -> 
  Map = shortcuts(State),
  NewMap = gb_trees:enter(C, L, Map),
  State#state{shortcuts=NewMap}.
change(State) ->
  State#state{changed=true}.
unchange(State) ->
  State#state{changed=false}.
is_changed(State) ->
  State#state.changed.

preds(State,L) ->
   hipe_icode_cfg:pred(pred_map(State),L).
succs(State,L) ->
   hipe_icode_cfg:succ(succ_map(State),L).
  
visited(L,State) ->
  hipe_icode_cfg:visited(L,
		    vis(State)).
visit(L,State) ->
  set_vis(State,
	     hipe_icode_cfg:visit(L,
			     vis(State))).
reset_visited(State) ->
  set_vis(State,hipe_icode_cfg:none_visited(cfg(State))).

empty_map() -> gb_trees:empty().

catches_in(L,State) ->
  Preds = preds(State,L),
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
%%    {value, []} ->
%%      get_all_catches_in(Ps, Map, Cs);
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


has_catches(CFG) ->
  Icode = hipe_icode_cfg:linearize(CFG),
  Code = hipe_icode:icode_code(Icode),
  code_has_catches(Code).

code_has_catches([I|Is]) ->
  case hipe_icode:type(I) of
    pushcatch -> true;
    _ -> code_has_catches(Is)
  end;
code_has_catches([]) -> false.
