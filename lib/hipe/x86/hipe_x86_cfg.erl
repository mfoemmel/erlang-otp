%%% $Id$
%%% x86 control-flow graph

-module(hipe_x86_cfg).

-export([linearise/1, is_branch/1, params/1, arity/1, redirect_jmp/3]).
-export([add_fail_entrypoint/2]).

%% To avoid warnings...
-export([find_new_label/2]).

-include("hipe_x86.hrl").
-include("../main/hipe.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
    %% XXX: this assumes that the code starts with a label insn.
    %% Is that guaranteed?
    Code = hipe_x86:defun_code(Defun),
    StartLab = hipe_x86:label_label(hd(Code)),
    VarRange = hipe_x86:defun_var_range(Defun),
    LabRange = hipe_x86:defun_label_range(Defun),
    Data = hipe_x86:defun_data(Defun),
    IsClosure = hipe_x86:defun_is_closure(Defun),
    Name = hipe_x86:defun_mfa(Defun),
    IsLeaf = hipe_x86:defun_is_leaf(Defun),
    Formals = hipe_x86:defun_formals(Defun),
    Extra = [],
    CFG0 = mk_empty_cfg(Name, StartLab, VarRange, LabRange, Data,
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
	#pseudo_call{contlab=ContLab, exnlab=ExnLab} ->
	    case ExnLab of
		[] -> [ContLab];
		_ -> [ContLab,ExnLab]
	    end;
	#pseudo_jcc{true_label=TrueLab,false_label=FalseLab} -> [FalseLab,TrueLab];
	#pseudo_tailcall{} -> [];
	#ret{} -> []
    end.

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
redirect_ops(_Labels, CFG, _Map) -> 
  CFG.

mk_goto(Label) ->
    hipe_x86:mk_jmp_label(Label).

is_label(I) ->
    hipe_x86:is_label(I).

label_name(Label) ->
    hipe_x86:label_label(Label).

label_annot(Label) ->
    hipe_x86:label_isfail(Label).

mk_label(Name, IsFail) ->	% IsFail came from label_annot/1
    hipe_x86:mk_label(Name, IsFail).

is_comment(I) ->
    hipe_x86:is_comment(I).

is_goto(I) ->
    hipe_x86:is_jmp_label(I).

pp(CFG) ->
    hipe_x86_pp:pp(linearise(CFG)).

linearise(CFG) ->	% -> defun, not insn list
    Fun = function(CFG),
    Formals = params(CFG),
    Code = linearize_cfg(CFG),
    Data = data(CFG),
    VarRange = var_range(CFG),
    LabelRange = label_range(CFG),
    IsClosure = is_closure(CFG),
    IsLeaf = is_leaf(CFG),
    hipe_x86:mk_defun(Fun, Formals, IsClosure, IsLeaf,
		      Code, Data, VarRange, LabelRange).

arity(CFG) ->
    #x86_mfa{a=Arity} = function(CFG),
    Arity.

add_fail_entrypoint(CFG, EP) ->
   Info = CFG#cfg.info,
   OEP = Info#cfg_info.fail_entrypoints,
   CFG#cfg{info=Info#cfg_info{fail_entrypoints=[EP|OEP]}}.
