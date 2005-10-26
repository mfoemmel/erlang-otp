%% -*- erlang-indent-level: 2 -*-
%%======================================================================

-module(hipe_icode_cfg).

-export([bb/2, bb_add/3,
	 %% bb_insert_between/5,
	 cfg_to_linear/1,
	 is_closure/1,
	 closure_arity/1,
	 closure_arity_update/2,
         info/1, linear_to_cfg/1,
         labels/1, start_label/1,
	 params/1, params_update/2,
	 pp/1, pp/2,	 
         pred/2, pred_map/1,
         redirect/4,
	 remove_trivial_bbs/1, remove_unreachable_code/1,
         succ/2, succ_map/1,
	 visit/2, visited/2, none_visited/0
	]).
-export([postorder/1, reverse_postorder/1]).

-define(ICODE_CFG,true).	% needed by cfg.inc below

%%-define(DO_ASSERT, true).
-include("../main/hipe.hrl").
-include("../flow/cfg.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to Icode
%%

linear_to_cfg(LinearIcode) ->
  %% hipe_icode_pp:pp(Icode),
  Code = hipe_icode:icode_code(LinearIcode),
  StartLabel = hipe_icode:label_name(hd(Code)),
  CFG0 = mk_empty_cfg(hipe_icode:icode_fun(LinearIcode),
		      StartLabel,
		      hipe_icode:icode_data(LinearIcode),
		      hipe_icode:icode_is_closure(LinearIcode),
		      hipe_icode:icode_is_leaf(LinearIcode),
		      hipe_icode:icode_params(LinearIcode),
		      []),
  CFG1 = hipe_icode_cfg:info_update(CFG0, hipe_icode:icode_info(LinearIcode)),
  CFG2 = hipe_icode_cfg:closure_arity_update(CFG1, hipe_icode:icode_closure_arity(LinearIcode)),
  ?opt_start_timer("Get BBs icode"),
  FullCFG = take_bbs(Code, CFG2),
  ?opt_stop_timer("Get BBs icode"),
  FullCFG.
  

%% remove_blocks(CFG, []) ->
%%   CFG;
%% remove_blocks(CFG, [Lbl|Lbls]) ->
%%   remove_blocks(bb_remove(CFG, Lbl), Lbls).


is_label(Instr) ->
  hipe_icode:is_label(Instr).

label_name(Instr) ->
  hipe_icode:label_name(Instr).

mk_label(Name) ->
  hipe_icode:mk_label(Name).

mk_goto(Name) ->
  hipe_icode:mk_goto(Name).

branch_successors(Instr) ->
  hipe_icode:successors(Instr).

fails_to(Instr) ->
  hipe_icode:fails_to(Instr).

%% True if instr has no effect.
is_comment(Instr) ->
  hipe_icode:is_comment(Instr).

%% True if instr is just a jump (no sideeffects).
is_goto(Instr) ->
  hipe_icode:is_goto(Instr).

is_branch(Instr) ->
  hipe_icode:is_branch(Instr).

is_pure_branch(Branch) ->
  case hipe_icode:type(Branch) of
    'if' -> true;
    goto -> true;
    switch_val -> true;
    switch_tuple_arity -> true;
    type -> true;
    _ -> false
  end.

is_phi(I)->
  hipe_icode:is_phi(I).

phi_remove_pred(I, Pred)->
  hipe_icode:phi_remove_pred(I, Pred).

%% phi_redirect_pred(I, OldPred, NewPred)->
%%   hipe_icode:phi_redirect_pred(I, OldPred, NewPred).

redirect_jmp(Jmp, ToOld, ToNew) ->
  hipe_icode:redirect_jmp(Jmp, ToOld, ToNew).

redirect_ops(_,CFG,_) -> %% We do not refer to labels in Icode ops.
  CFG.

pp(CFG) ->
  hipe_icode_pp:pp(cfg_to_linear(CFG)).

pp(Dev, CFG) ->
  hipe_icode_pp:pp(Dev, cfg_to_linear(CFG)).

cfg_to_linear(CFG) ->
  Code = linearize_cfg(CFG),
  Icode = hipe_icode:mk_icode(function(CFG), 
			      params(CFG),
			      is_closure(CFG),
			      is_leaf(CFG),
			      Code,
			      data(CFG),
			      hipe_gensym:var_range(icode), 
			      hipe_gensym:label_range(icode)),
  Icode1 = hipe_icode:icode_info_update(Icode, info(CFG)),
  hipe_icode:icode_closure_arity_update(Icode1, closure_arity(CFG)).

%% init_gensym(CFG) ->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(icode, HighestVar),
%%   hipe_gensym:set_label(icode, HighestLabel).
%% 
%% highest_var(Code) ->
%%   hipe_icode:highest_var(Code).
