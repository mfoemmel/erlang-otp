-module(hipe_sparc_cfg).

-export([bb/2, bb_add/3,
	 function/1,
	 init/1,
         labels/1, start_label/1,
	 linearize/1,
	 is_leaf/1,
         data/1,
         succ/2, succ_map/1,
         pred/2, pred_map/1,
	 predictionorder/1,
         remove_trivial_bbs/1
	]).
-export([postorder/1, reverse_postorder/1]).
%%-export([depth_first_ordering/1]).	% needed for hipe_finalize

-define(SPARC_CFG,true).		% needed for cfg.inc

-include("../main/hipe.hrl"). 
-include("../flow/cfg.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CFG interface to sparc
%%

init(Sparc) ->
   Code = hipe_sparc:sparc_code(Sparc),
   StartLabel = hipe_sparc:label_name(hd(Code)),

   CFG = mk_empty_cfg(hipe_sparc:sparc_fun(Sparc),
		      StartLabel,
		      hipe_sparc:sparc_data(Sparc),
		      hipe_sparc:sparc_is_closure(Sparc),
		      hipe_sparc:sparc_is_leaf(Sparc),
		      [], %% XXX: Params - do we need em?
		      hipe_sparc:sparc_arity(Sparc)),
   take_bbs(Code, CFG).

%% True if instr has no effect.
is_comment(Instr) ->
  hipe_sparc:is_comment(Instr).

%% True if instr is just a jump (no sideeffects).
is_goto(Instr) ->
   hipe_sparc:is_goto(Instr).

is_label(Instr) ->
   hipe_sparc:is_label(Instr).

label_name(Instr) ->
   hipe_sparc:label_name(Instr).

mk_label(Name) ->
   hipe_sparc:label_create(Name).

mk_goto(Name) ->
    hipe_sparc:goto_create(Name).

branch_successors(Instr) ->
  case hipe_sparc:type(Instr) of
    b -> [hipe_sparc:b_false_label(Instr),hipe_sparc:b_true_label(Instr)];
%%     br -> [hipe_sparc:br_false_label(Instr),hipe_sparc:br_true_label(Instr)];
    call_link -> 
      case hipe_sparc:call_link_fail(Instr) of
	[] -> [hipe_sparc:call_link_continuation(Instr)];
	F -> [hipe_sparc:call_link_continuation(Instr),F]
      end;
    jmp -> hipe_sparc:jmp_destinations(Instr);
    goto -> [hipe_sparc:goto_label(Instr)];
    _ -> []
  end.

fails_to(Instr) ->
  case hipe_sparc:type(Instr) of
    call_link -> [hipe_sparc:call_link_fail(Instr)];
    _ -> []
  end.

is_branch(Instr) ->
  case hipe_sparc:type(Instr) of
    b -> true;
    br -> true;
    goto -> true;
    jmp -> true;
    jmp_link -> true;
    pseudo_return -> true;
    pseudo_enter -> true;
    call_link -> 
      case hipe_sparc:call_link_fail(Instr) of
	[] -> 
	  case hipe_sparc:call_link_continuation(Instr) of
	    [] ->
	      false;
	    _ ->
	      true
	  end;
	_ -> true
      end;
    _ -> false
  end.


redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_sparc:redirect_jmp(Jmp, ToOld, ToNew).

redirect_ops([Label|Labels], CFG, Map) ->
  BB = bb(CFG, Label),
  Code = hipe_bb:code(BB),
  NewCode = [rewrite(I,Map) || I <- Code],
  NewCFG = bb_add(CFG, Label,hipe_bb:code_update(BB, NewCode)),
  redirect_ops(Labels, NewCFG, Map);
redirect_ops([],CFG,_) -> CFG.

rewrite(I,Map) ->
  case hipe_sparc:type(I) of
    load_address ->
	case hipe_sparc:load_address_type(I) of
	  constant -> I;
	  _ -> 
	    NewL =
	      find_new_label(hipe_sparc:load_address_address(I),Map),
	    hipe_sparc:load_address_address_update(I,NewL)
	end;
    _ -> I
  end.

%% pp(CFG) ->
%%    hipe_sparc_pp:pp(linearize(CFG)).

linearize(CFG) ->
  Code = linearize_cfg(CFG),
  hipe_sparc:mk_sparc(function(CFG),
		      extra(CFG),
		      is_closure(CFG),
		      is_leaf(CFG),
		      Code,
		      data(CFG),
		      [], %var_range(CFG),
		      []).%label_range(CFG)).


predictionorder(CFG) ->
  Start = start_label(CFG),
  Succ = succ_map(CFG),
  {_Vis, PO1} = prediction_list([Start], none_visited(), Succ, [], CFG),
  lists:reverse(PO1).


prediction_list([X|Xs], Vis, Succ, PO, CFG) ->
  case visited(X,Vis) of 
    true ->
      prediction_list(Xs, Vis, Succ, PO, CFG);
    false ->
      Succs = succ(Succ, X),
      NewVis = visit(X,Vis),
      NewPO = [X|PO],
      case Succs of
	[] -> 
	  prediction_list(Xs, NewVis, Succ, NewPO, CFG);
	[S] ->
	  prediction_list([S|Xs], NewVis, Succ, NewPO, CFG);
	_Ss ->
	  BB = bb(CFG, X),
	  Jmp = hipe_bb:last(BB),
	  case is_cond(Jmp) of
	    true ->
	      %% Switch the jump so the common case is not taken
	      Pred = cond_pred(Jmp),	
	      NewToDo =
	      if Pred >= 0.5 ->
		  [cond_true_label(Jmp),
		   cond_false_label(Jmp)|Xs];
		 true ->
		  [cond_false_label(Jmp),
		   cond_true_label(Jmp)|Xs]
	      end,
	      prediction_list(NewToDo,
			      NewVis, 
			      Succ, 
			      NewPO, 
			      CFG);
	    _ -> %% Not a cond. 
	      case hipe_sparc:is_call_link(Jmp) of
		true ->
		  prediction_list(
		   [hipe_sparc:call_link_continuation(Jmp),
		    hipe_sparc:call_link_fail(Jmp)|Xs],
		    NewVis, 
		    Succ, 
		    NewPO, 
		    CFG);
		false ->
		  prediction_list(
		    branch_successors(Jmp) ++ Xs,
		    NewVis, 
		    Succ, 
		    NewPO, 
		    CFG)

	      end
	  end
      end
  end;
prediction_list([], Vis, _, PO, _) ->
  {Vis, PO}.
	    
is_cond(I) ->
   case hipe_sparc:type(I) of
      br -> true;
      b -> true;
      _ -> false
   end.


cond_pred(I) ->
   case hipe_sparc:type(I) of
%%      br -> hipe_sparc:br_pred(I);
      b -> hipe_sparc:b_pred(I)
   end.
   

cond_true_label(B) ->
   case hipe_sparc:type(B) of
%%      br -> hipe_sparc:br_true_label(B);
      b -> hipe_sparc:b_true_label(B)
   end.


cond_false_label(B) ->
   case hipe_sparc:type(B) of
%%       br -> hipe_sparc:br_false_label(B);
      b -> hipe_sparc:b_false_label(B)
   end.

%% init_gensym(CFG) ->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(sparc, HighestVar),
%%   hipe_gensym:set_label(sparc, HighestLabel).
%% 
%% highest_var(Code) ->
%%   hipe_sparc:highest_reg(Code).


-ifdef(CFG_CAN_HAVE_PHI_NODES).
%% put appropriate code for these functions here...
%% is_phi(_I) ->
%%   ...
%%
%% phi_remove_pred(I, _Pred) ->
%%   ...
-endif.
