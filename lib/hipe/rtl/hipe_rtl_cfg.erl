-module(hipe_rtl_cfg).

-export([function/1,
	 params/1, 
	 arity/1,
	 linearize/1,
	 pp/2]).

-include("../main/hipe.hrl").
-include("../flow/cfg.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CFG interface to rtl
%%

init(Rtl) ->
  %% hipe_rtl:pp(Rtl),
  Code = hipe_rtl:rtl_code(Rtl),
  StartLabel = hipe_rtl:label_name(hd(Code)),
  Extra = [],
  CFG0 = mk_empty_cfg(hipe_rtl:rtl_fun(Rtl), 
		      StartLabel, 
		      hipe_rtl:rtl_var_range(Rtl),
		      hipe_rtl:rtl_label_range(Rtl),
		      hipe_rtl:rtl_data(Rtl),
		      hipe_rtl:rtl_is_closure(Rtl),
		      hipe_rtl:rtl_is_leaf(Rtl),
		      hipe_rtl:rtl_params(Rtl),
		      Extra),
  CFG = hipe_rtl_cfg:info_update(CFG0, hipe_rtl:rtl_info(Rtl)),
  take_bbs(Code, CFG).


is_fail_entrypoint(Label) ->
  lists:member(entry, hipe_rtl:info(Label)).

%% True if instr has no effect.
is_comment(Instr) ->
  hipe_rtl:is_comment(Instr).

%% True if instr is just a jump (no sideeffects).
is_goto(Instr) ->
  hipe_rtl:is_goto(Instr).


is_label(Instr) ->
  hipe_rtl:is_label(Instr).

label_name(Instr) ->
  hipe_rtl:label_name(Instr).

label_annot(Lbl) ->
  hipe_rtl:info(Lbl).

mk_label(Name, Annot) ->
  hipe_rtl:info_update(hipe_rtl:mk_label(Name), Annot).

mk_goto(Name) ->
  hipe_rtl:mk_goto(Name).

branch_successors(Instr) ->
  case hipe_rtl:type(Instr) of
    branch -> [hipe_rtl:branch_true_label(Instr), 
	       hipe_rtl:branch_false_label(Instr)];
    alub -> [hipe_rtl:alub_true_label(Instr), 
	     hipe_rtl:alub_false_label(Instr)];
    switch -> hipe_rtl:switch_labels(Instr);
    call -> 
      case hipe_rtl:call_fail(Instr) of
	[] ->  [hipe_rtl:call_continuation(Instr)];
	Fail -> [hipe_rtl:call_continuation(Instr),Fail]
      end;
    goto -> [hipe_rtl:goto_label(Instr)];
    goto_index -> hipe_rtl:goto_index_labels(Instr);
    fail_to -> [hipe_rtl:fail_to_label(Instr)];
    _ -> []
  end.

is_branch(Instr) ->
   case hipe_rtl:type(Instr) of
      branch -> true;
      alub -> true;
      switch -> true;
      goto -> true;
      goto_index -> true;
      fail_to -> 
       case  hipe_rtl:fail_to_label(Instr) of
	 [] -> false;
	 _ -> true
       end;
      enter -> true;
      return -> true;
      call -> 
       case hipe_rtl:call_fail(Instr) of
	[] ->  
	   case hipe_rtl:call_continuation(Instr) of
	     [] -> false;
	     _ -> true
	   end;
	 _ -> true
       end;
      _ -> false
   end.


redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_rtl:redirect_jmp(Jmp, ToOld, ToNew).

redirect_ops([Label|Labels], CFG, Map) ->
  BB = bb(CFG, Label),
  Code = hipe_bb:code(BB),
  NewCode = [rewrite(I,Map) || I <- Code],
  NewCFG = bb_update(CFG, Label,hipe_bb:code_update(BB, NewCode)),
  redirect_ops(Labels, NewCFG, Map);
redirect_ops([],CFG,_) -> CFG.

rewrite(I,Map) ->
  case hipe_rtl:type(I) of
    load_address ->
	case hipe_rtl:load_address_type(I) of
	  constant -> I;
	  _ -> 
	    NewL =
	      find_new_label(hipe_rtl:load_address_address(I),Map),
	    hipe_rtl:load_address_address_update(I,NewL)
	end;
    _ -> I
  end.


pp(CFG) ->
  hipe_rtl:pp(linearize(CFG)).

pp(Dev, CFG) ->
  hipe_rtl:pp(Dev, linearize(CFG)).

linearize(CFG) ->
  Code = linearize_cfg(CFG),
  Rtl = hipe_rtl:mk_rtl(
	  function(CFG),
	  params(CFG),
	  is_closure(CFG),
	  is_leaf(CFG),
	  Code, 
	  data(CFG),	 
	  var_range(CFG), 
	  label_range(CFG)),
  hipe_rtl:rtl_info_update(Rtl, info(CFG)).


%% XXX: Warning this arity might not be the true arity.
%%      The true arity of a closure usually differ.
arity(CFG) ->
   {_M,_F,A} = function(CFG),
   A.
