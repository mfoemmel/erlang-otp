-module(hipe_sparc_cfg).

-export([function/1,
	 linearize/1,
	 init/2,
	 predictionorder/1]).

%% Get rid of stupid compiler warning.
-export([extra_update/2]).
-include("../flow/cfg.inc").
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CFG interface to sparc
%

init(Sparc) ->
   Code = hipe_sparc:sparc_code(Sparc),
   StartLabel = hipe_sparc:label_name(hd(Code)),
   Extra = hipe_sparc:sparc_fun(Sparc),
   CFG = mk_empty_cfg(StartLabel,
		      hipe_sparc:sparc_var_range(Sparc),
		      hipe_sparc:sparc_label_range(Sparc),
		      hipe_sparc:sparc_data(Sparc),
		      Extra),
   take_bbs(Code, CFG).

init(Sparc,Entries) ->
   Code = hipe_sparc:sparc_code(Sparc),
   StartLabel = hipe_sparc:label_name(hd(Code)),
   Extra = hipe_sparc:sparc_fun(Sparc),
   CFG = mk_empty_cfg(StartLabel,
		      hipe_sparc:sparc_var_range(Sparc),
		      hipe_sparc:sparc_label_range(Sparc),
		      hipe_sparc:sparc_data(Sparc),
		      Extra),
  CFG1 = lists:foldl(fun (Ep,CFGAcc) -> 
			 hipe_sparc_cfg:add_fail_entrypoint(CFGAcc,Ep)
		     end,
		     CFG,
		     Entries),
   take_bbs(Code, CFG1).


is_fail_entrypoint(Label) ->
   lists:member(entry, hipe_sparc:info(Label)).


is_label(Instr) ->
   hipe_sparc:is_label(Instr).


label_name(Instr) ->
   hipe_sparc:label_name(Instr).


label_annot(Lbl) ->
   hipe_sparc:info(Lbl).


mk_label(Name, Annot) ->
   hipe_sparc:label_create(Name, Annot).

mk_goto(Name) ->
    hipe_sparc:goto_create(Name, []).

branch_successors(Instr) ->
  case hipe_sparc:type(Instr) of
    b -> [hipe_sparc:b_true_label(Instr), hipe_sparc:b_false_label(Instr)];
    br -> [hipe_sparc:br_true_label(Instr), hipe_sparc:br_false_label(Instr)];
    call_link -> 
      case hipe_sparc:call_link_fail(Instr) of
	[] -> [hipe_sparc:call_link_continuation(Instr)];
	F -> [F,hipe_sparc:call_link_continuation(Instr)]
      end;
    jmp -> hipe_sparc:jmp_destinations(Instr);
    goto -> [hipe_sparc:goto_label(Instr)];
    _ -> []
  end.


is_branch(Instr) ->
   hipe_sparc:is_any_branch(Instr).


redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_sparc:redirect_jmp(Jmp, ToOld, ToNew).


pp(CFG) ->
   hipe_sparc:pp(linearize(CFG)).


linearize(CFG) ->
   Code = linearize_cfg(CFG),
   hipe_sparc:mk_sparc(function(CFG),
		  Code,
		  data(CFG),     
		  var_range(CFG), 
		  label_range(CFG)).


function(CFG) ->
   extra(CFG).


predictionorder(CFG) ->
  Start = start(CFG),
  Succ = succ_map(CFG),
  {Vis, PO1} = prediction_list([Start], 
			      none_visited(), 
			      Succ, 
			      [],
			      CFG),
  {_, PO} = prediction_list(
	      fail_entrypoints(CFG) ++ other_entrypoints(CFG), Vis, 
	      Succ, PO1, CFG), 
  lists:reverse(PO).

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
	Ss ->
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
      br -> hipe_sparc:br_pred(I);
      b -> hipe_sparc:b_pred(I)
   end.
   

cond_true_label(B) ->
   case hipe_sparc:type(B) of
      br -> hipe_sparc:br_true_label(B);
      b -> hipe_sparc:b_true_label(B)
   end.


cond_false_label(B) ->
   case hipe_sparc:type(B) of
      br -> hipe_sparc:br_false_label(B);
      b -> hipe_sparc:b_false_label(B)
   end.

