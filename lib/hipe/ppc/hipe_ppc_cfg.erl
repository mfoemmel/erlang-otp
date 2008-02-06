%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_cfg).

-export([init/1,
         labels/1, start_label/1,
         succ/2, succ_map/1,
         bb/2, bb_add/3]).
-export([postorder/1]).
-export([linearise/1, params/1, reverse_postorder/1]).
-export([arity/1]).
%%%-export([redirect_jmp/3, arity/1]).

%%% these tell cfg.inc what to define (ugly as hell)
-define(BREADTH_ORDER,true).
-define(PARAMS_NEEDED,true).
-define(START_LABEL_UPDATE_NEEDED,true).

-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").
-include("hipe_ppc.hrl").

init(Defun) ->
  Code = hipe_ppc:defun_code(Defun),
  StartLab = hipe_ppc:label_label(hd(Code)),
  Data = hipe_ppc:defun_data(Defun),
  IsClosure = hipe_ppc:defun_is_closure(Defun),
  Name = hipe_ppc:defun_mfa(Defun),
  IsLeaf = hipe_ppc:defun_is_leaf(Defun),
  Formals = hipe_ppc:defun_formals(Defun),
  Extra = [],
  CFG0 = mk_empty_cfg(Name, StartLab, Data,
		      IsClosure, IsLeaf, Formals, Extra),
  take_bbs(Code, CFG0).

is_branch(I) ->
  case I of
    #b_fun{} -> true;
    #b_label{} -> true;
    %% not bc
    #bctr{} -> true;
    %% not bctrl
    %% not bl
    #blr{} -> true;
    #pseudo_bc{} -> true;
    #pseudo_call{} -> true;
    #pseudo_tailcall{} -> true;
    _ -> false
  end.

branch_successors(Branch) ->
  case Branch of
    #b_fun{} -> [];
    #b_label{label=Label} -> [Label];
    #bctr{labels=Labels} -> Labels;
    #blr{} -> [];
    #pseudo_bc{true_label=TrueLab,false_label=FalseLab} -> [FalseLab,TrueLab];
    #pseudo_call{contlab=ContLab, sdesc=#ppc_sdesc{exnlab=ExnLab}} ->
      case ExnLab of
	[] -> [ContLab];
	_ -> [ContLab,ExnLab]
      end;
    #pseudo_tailcall{} -> []
  end.

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).
fails_to(_Instr) -> [].
-endif.

-ifdef(notdef).
redirect_jmp(I, Old, New) ->
  case I of
    #b_label{label=Label} ->
      if Old =:= Label -> I#b_label{label=New};
	 true -> I
      end;
    #pseudo_bc{true_label=TrueLab, false_label=FalseLab} ->
      I1 = if Old =:= TrueLab -> I#pseudo_bc{true_label=New};
	      true -> I
	   end,
      if Old =:= FalseLab -> I1#pseudo_bc{false_label=New};
	 true -> I1
      end;
    %% handle pseudo_call too?
    _ -> I
  end.
-endif.

mk_goto(Label) ->
  hipe_ppc:mk_b_label(Label).

is_label(I) ->
  hipe_ppc:is_label(I).

label_name(Label) ->
  hipe_ppc:label_label(Label).

mk_label(Name) ->
  hipe_ppc:mk_label(Name).

linearise(CFG) ->	% -> defun, not insn list
  Fun = function(CFG),
  Formals = params(CFG),
  Code = linearize_cfg(CFG),
  Data = data(CFG),
  VarRange = hipe_gensym:var_range(ppc),
  LabelRange = hipe_gensym:label_range(ppc),
  IsClosure = is_closure(CFG),
  IsLeaf = is_leaf(CFG),
  hipe_ppc:mk_defun(Fun, Formals, IsClosure, IsLeaf,
		    Code, Data, VarRange, LabelRange).

arity(CFG) ->
  #ppc_mfa{a=Arity} = function(CFG),
  Arity.
