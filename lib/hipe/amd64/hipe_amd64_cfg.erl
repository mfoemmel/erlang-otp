%%% $Id$
%%% amd64 control-flow graph

-module(hipe_amd64_cfg).

-export([init/1,
         labels/1, start_label/1,
         succ/2, succ_map/1,
         bb/2, bb_add/3]).
-export([postorder/1, reverse_postorder/1]).
-export([linearise/1, params/1, arity/1, redirect_jmp/3]).

-define(AMD64_CFG,true).	% needed for cfg.inc below

-include("hipe_amd64.hrl").
-include("../main/hipe.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
    %% XXX: this assumes that the code starts with a label insn.
    %% Is that guaranteed?
    Code = hipe_amd64:defun_code(Defun),
    StartLab = hipe_amd64:label_label(hd(Code)),
    Data = hipe_amd64:defun_data(Defun),
    IsClosure = hipe_amd64:defun_is_closure(Defun),
    Name = hipe_amd64:defun_mfa(Defun),
    IsLeaf = hipe_amd64:defun_is_leaf(Defun),
    Formals = hipe_amd64:defun_formals(Defun),
    Extra = [],
    CFG0 = mk_empty_cfg(Name, StartLab, Data,
			IsClosure, IsLeaf, Formals, Extra),
    take_bbs(Code, CFG0).

is_branch(I) ->
    case I of
	#jmp_fun{} -> true;
	#jmp_label{} -> true;
	#jmp_switch{} -> true;
	#pseudo_call{} -> true;
	#pseudo_jcc{} -> true;
	#pseudo_tailcall{} -> true;
	#ret{} -> true;
	_ -> false
    end.

branch_successors(Branch) ->
    case Branch of
	#jmp_fun{} -> [];
	#jmp_label{label=Label} -> [Label];
	#jmp_switch{labels=Labels} -> Labels;
	#pseudo_call{contlab=ContLab, sdesc=#amd64_sdesc{exnlab=ExnLab}} ->
	    case ExnLab of
		[] -> [ContLab];
		_ -> [ContLab,ExnLab]
	    end;
	#pseudo_jcc{true_label=TrueLab,false_label=FalseLab} -> [FalseLab,TrueLab];
	#pseudo_tailcall{} -> [];
	#ret{} -> []
    end.

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).
fails_to(_Instr) -> [].
-endif.

redirect_jmp(I, Old, New) ->
    case I of
	#jmp_label{label=Label} ->
	    if Old =:= Label -> I#jmp_label{label=New};
	       true -> I
	    end;
	#pseudo_jcc{true_label=TrueLab, false_label=FalseLab} ->
	    J0 = if Old =:= TrueLab -> I#pseudo_jcc{true_label=New};
		    true -> I
		 end,
	    if Old =:= FalseLab -> J0#pseudo_jcc{false_label=New};
	       true -> J0
	    end;
	%% handle pseudo_call too?
	_ -> I
    end.

%%% XXX: fix if labels can occur in operands
%% redirect_ops(_Labels, CFG, _Map) -> 
%%   CFG.

mk_goto(Label) ->
  hipe_amd64:mk_jmp_label(Label).

is_label(I) ->
  hipe_amd64:is_label(I).

label_name(Label) ->
  hipe_amd64:label_label(Label).

mk_label(Name) ->
  hipe_amd64:mk_label(Name).

%% is_comment(I) ->
%%   hipe_amd64:is_comment(I).
%% 
%% is_goto(I) ->
%%   hipe_amd64:is_jmp_label(I).

%% pp(CFG) ->
%%   hipe_amd64_pp:pp(linearise(CFG)).

linearise(CFG) ->	% -> defun, not insn list
  Fun = function(CFG),
  Formals = params(CFG),
  Code = linearize_cfg(CFG),
  Data = data(CFG),
  VarRange = hipe_gensym:var_range(amd64),
  LabelRange = hipe_gensym:label_range(amd64),
  IsClosure = is_closure(CFG),
  IsLeaf = is_leaf(CFG),
  hipe_amd64:mk_defun(Fun, Formals, IsClosure, IsLeaf,
		    Code, Data, VarRange, LabelRange).

arity(CFG) ->
  #amd64_mfa{a=Arity} = function(CFG),
  Arity.

%% init_gensym(CFG)->
%%   HighestVar = find_highest_var(CFG),
%%   HighestLabel = find_highest_label(CFG),
%%   hipe_gensym:init(),
%%   hipe_gensym:set_var(amd64, HighestVar),
%%   hipe_gensym:set_label(amd64, HighestLabel).
%% 
%% highest_var(Code)->
%%   hipe_amd64:highest_temp(Code).

is_phi(_I)->
  false. %% We have no phi-nodes on this level.

phi_remove_pred(I, _Pred)->
  I. %% We have no phi-nodes on this level.
