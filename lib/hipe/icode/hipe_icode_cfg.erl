%% -*- erlang-indent-level: 2 -*-


-module(hipe_icode_cfg).

-export([function/1,
	 params/1,
	 params_update/2,
	 pp/2,
	 linearize/1]).

%% To avoid warnings...
-export([find_new_label/2,remove_blocks/2]).

%%-define(DO_ASSERT, true).
-include("../main/hipe.hrl").
-include("../flow/cfg.inc").
-include("../rtl/hipe_literals.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to icode
%

init(Icode) ->
  %% hipe_icode_pp:pp(Icode),
  Code = hipe_icode:icode_code(Icode),
  StartLabel = hipe_icode:label_name(hd(Code)),
  {MinV,MaxV} = hipe_icode:icode_var_range(Icode),
  %% ?ASSERT(hipe_icode:highest_var(hipe_icode:icode_code(Icode1)) =< MaxV),
  ?opt_start_timer("highest var"),
  ?ASSERT(MaxV >= hipe_icode:highest_var(Code)),
  ?opt_stop_timer("highest var"),

  CFG0 = 
    mk_empty_cfg(
      hipe_icode:icode_fun(Icode),
      StartLabel,
      {MinV,MaxV},
      hipe_icode:icode_label_range(Icode),
      hipe_icode:icode_data(Icode),
      hipe_icode:icode_is_closure(Icode),
      hipe_icode:icode_is_leaf(Icode),
      hipe_icode:icode_params(Icode),
      []),

  CFG = hipe_icode_cfg:info_update(CFG0, hipe_icode:icode_info(Icode)),
  ?opt_start_timer("Get BBs icode"),
  FullCFG = take_bbs(Code, CFG),
  ?opt_stop_timer("Get BBs icode"),
  FullCFG.
  



remove_blocks(CFG, []) ->
   CFG;
remove_blocks(CFG, [Lbl|Lbls]) ->
   remove_blocks(bb_remove(CFG, Lbl), Lbls).


is_label(Instr) ->
   hipe_icode:is_label(Instr).


label_name(Instr) ->
   hipe_icode:label_name(Instr).


label_annot(Lbl) ->
   hipe_icode:info(Lbl).


mk_label(Name, Annot) ->
   hipe_icode:info_update(hipe_icode:mk_label(Name), Annot).

mk_goto(Name) ->
   hipe_icode:mk_goto(Name).

branch_successors(Instr) ->
   hipe_icode:successors(Instr).

%% True if instr has no effect.
is_comment(Instr) ->
  hipe_icode:is_comment(Instr).

%% True if instr is just a jump (no sideeffects).
is_goto(Instr) ->
   hipe_icode:is_goto(Instr).

is_branch(Instr) ->
   hipe_icode:is_branch(Instr).


redirect_jmp(Jmp, ToOld, ToNew) ->
  I =  hipe_icode:redirect_jmp(Jmp, ToOld, ToNew),
  I.

redirect_ops(_,CFG,_) -> %% We do not refer to labels in Icode ops.
  CFG.

pp(CFG) ->
   hipe_icode_pp:pp(linearize(CFG)).

pp(Dev, CFG) ->
   hipe_icode_pp:pp(Dev, linearize(CFG)).

linearize(CFG) ->
  Code = linearize_cfg(CFG),
  Icode = 
    hipe_icode:mk_icode(
      function(CFG), 
      params(CFG),
      is_closure(CFG),
      is_leaf(CFG),
      Code,
      data(CFG),     
      var_range(CFG), 
      label_range(CFG)),
  hipe_icode:icode_info_update(Icode, info(CFG)).


