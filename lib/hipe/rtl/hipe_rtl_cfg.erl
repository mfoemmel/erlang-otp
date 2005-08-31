-module(hipe_rtl_cfg).

-export([init/1,
         labels/1,
	 params/1, params_update/2,
         start_label/1,
         succ/2, succ_map/1,
         pred/2, pred_map/1,
         bb/2,
         bb_add/3,
	 redirect/4,
         remove_trivial_bbs/1,
         remove_unreachable_code/1,
	 linearize/1,
	 pp/1, pp/2]).
-export([postorder/1, reverse_postorder/1]).

-define(RTL_CFG,true).	% needed for cfg.inc below

-export([preorder/1]). %% Added Daz, 2005/08/09, to get the preorder too

-include("../main/hipe.hrl").
-include("../flow/cfg.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CFG interface to RTL.
%%

init(Rtl) ->
  %% hipe_rtl:pp(Rtl),
  Code = hipe_rtl:rtl_code(Rtl),
  StartLabel = hipe_rtl:label_name(hd(Code)),
  Extra = [],
  CFG0 = mk_empty_cfg(hipe_rtl:rtl_fun(Rtl), 
		      StartLabel, 
		      hipe_rtl:rtl_data(Rtl),
		      hipe_rtl:rtl_is_closure(Rtl),
		      hipe_rtl:rtl_is_leaf(Rtl),
		      hipe_rtl:rtl_params(Rtl),
		      Extra),
  CFG = hipe_rtl_cfg:info_update(CFG0, hipe_rtl:rtl_info(Rtl)),
  take_bbs(Code, CFG).

%% @spec is_comment(hipe_rtl:rtl_instruction()) -> bool()
%% @doc  Succeeds if Instr has no effect.
is_comment(Instr) ->
  hipe_rtl:is_comment(Instr).

%% @spec is_goto(hipe_rtl:rtl_instruction()) -> bool()
%% @doc  Succeeds if Instr is just a jump (no side-effects).
is_goto(Instr) ->
  hipe_rtl:is_goto(Instr).

is_label(Instr) ->
  hipe_rtl:is_label(Instr).

label_name(Instr) ->
  hipe_rtl:label_name(Instr).

mk_label(Name) ->
  hipe_rtl:mk_label(Name).

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
	[] -> [hipe_rtl:call_continuation(Instr)];
	Fail -> [hipe_rtl:call_continuation(Instr),Fail]
      end;
    goto -> [hipe_rtl:goto_label(Instr)];
    goto_index -> hipe_rtl:goto_index_labels(Instr);
    _ -> []
  end.

fails_to(Instr) ->
  case hipe_rtl:type(Instr) of
    call -> [hipe_rtl:call_fail(Instr)];
    _ -> []
  end.

is_branch(Instr) ->
   case hipe_rtl:type(Instr) of
      branch -> true;
      alub -> true;
      switch -> true;
      goto -> true;
      goto_index -> true;
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
  NewCFG = bb_add(CFG, Label,hipe_bb:code_update(BB, NewCode)),
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
  Rtl = hipe_rtl:mk_rtl(function(CFG),
			params(CFG),
			is_closure(CFG),
			is_leaf(CFG),
			Code, 
			data(CFG),	 
			hipe_gensym:var_range(rtl), 
			hipe_gensym:label_range(rtl)),
  hipe_rtl:rtl_info_update(Rtl, info(CFG)).

%% %% Warning: this arity might not be the true arity.
%% %% The true arity of a closure usually differs.
%% arity(CFG) ->
%%    {_M,_F,A} = function(CFG),
%%    A.

%% init_gensym(CFG)->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(rtl, HighestVar),
%%   hipe_gensym:set_label(rtl, HighestLabel).
%% 
%% highest_var(Code)->
%%   hipe_rtl:highest_var(Code).

is_phi(I) ->
  hipe_rtl:is_phi(I).

phi_remove_pred(I, _Pred) ->
  I. %%hipe_rtl:phi_remove_pred(I, Pred).NYI
