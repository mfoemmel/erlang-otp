%%% $Id$
%%% x86 control-flow graph

-module(hipe_x86_cfg).

-export([linearise/1, is_branch/1, cfg_formals/1]).

-include("hipe_x86.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
    %% XXX: this assumes that the code starts with a label insn.
    %% Is that guaranteed?
    Code = hipe_x86:defun_code(Defun),
    StartLab = hipe_x86:label_label(hd(Code)),
    VarRange = hipe_x86:defun_var_range(Defun),
    LabRange = hipe_x86:defun_label_range(Defun),
    Data = hipe_x86:defun_data(Defun),
    Extra = {hipe_x86:defun_mfa(Defun), hipe_x86:defun_formals(Defun)},
    CFG0 = mk_empty_cfg(StartLab, VarRange, LabRange, Data, Extra),
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

%%% Note: When there are two successors [L0, L1], flow/cfg.inc will
%%% consider the first (L0) to be the "conditional" or "unlikely"
%%% successor, and the second (L1) to be the "fallthrough" successor.
%%% See cond/2, fallthrough/2, and lin_succ/3 in flow/cfg.inc.

branch_successors(Branch) ->
    case Branch of
	#jmp_fun{} -> [];
	#jmp_label{label=Label} -> [Label];
	#jmp_switch{labels=Labels} -> Labels;
	#pseudo_call{contlab=ContLab, exnlab=ExnLab} ->
	    case ExnLab of
		[] -> [ContLab];
		_ -> [ExnLab, ContLab]	% order matters here :-(
	    end;
	#pseudo_jcc{true_label=TrueLab, false_label=FalseLab} -> [TrueLab, FalseLab];
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

mk_goto(Label) ->
    hipe_x86:mk_jmp_label(Label).

is_label(I) ->
    hipe_x86:is_label(I).

label_name(Label) ->
    hipe_x86:label_label(Label).

is_fail_entrypoint(Label) ->
    hipe_x86:label_isfail(Label).

label_annot(Label) ->
    hipe_x86:label_isfail(Label).

mk_label(Name, IsFail) ->	% IsFail came from label_annot/1
    hipe_x86:mk_label(Name, IsFail).

pp(CFG) ->
    hipe_x86_pp:pp(linearise(CFG)).

linearise(CFG) ->	% -> defun, not insn list
    Fun = cfg_fun(CFG),
    Formals = cfg_formals(CFG),
    Code = linearize_cfg(CFG),
    Data = data(CFG),
    VarRange = var_range(CFG),
    LabelRange = label_range(CFG),
    hipe_x86:mk_defun(Fun, Formals, Code, Data, VarRange, LabelRange).

cfg_fun(CFG) ->
    {MFA, _} = extra(CFG),	% see init/1
    MFA.

cfg_formals(CFG) ->
    {_, Formals} = extra(CFG),	% see init/1
    Formals.
