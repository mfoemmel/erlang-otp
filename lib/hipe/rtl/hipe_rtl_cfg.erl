-module(hipe_rtl_cfg).

-export([function/1,
	 params/1, 
	 arity/1,
	 linearize/1,
	 pp/2]).

% Shut up silly compiler warning
-export([extra_update/2]).
-include("../flow/cfg.inc").
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CFG interface to rtl
%

init(Rtl) ->
  NewRtl = hipe_rtl:remove_empty_bbs(Rtl),
  %% hipe_rtl:pp(NewRtl),
  Code = hipe_rtl:rtl_code(NewRtl),
  StartLabel = hipe_rtl:label_name(hd(Code)),
  Extra = {hipe_rtl:rtl_fun(NewRtl), hipe_rtl:rtl_params(NewRtl)},
  CFG0 = mk_empty_cfg(StartLabel, 
		      hipe_rtl:rtl_var_range(NewRtl),
		      hipe_rtl:rtl_label_range(NewRtl),
		      hipe_rtl:rtl_data(NewRtl),
		      Extra),
   CFG = hipe_rtl_cfg:info_update(CFG0, hipe_rtl:rtl_info(NewRtl)),
   take_bbs(Code, CFG).

%init(Rtl,Entries) ->
%   Code = hipe_rtl:rtl_code(hipe_rtl:remove_empty_bbs(Rtl)),
%   StartLabel = hipe_rtl:label_name(hd(Code)),
%   Extra = {hipe_rtl:rtl_fun(Rtl), hipe_rtl:rtl_params(Rtl)},
%   CFG0 = mk_empty_cfg(StartLabel, 
%		       hipe_rtl:rtl_var_range(Rtl),
%		       hipe_rtl:rtl_label_range(Rtl),
%		       Extra),
%  CFG1 = lists:foldl(fun (Ep,CFGAcc) -> 
%			 hipe_rtl_cfg:add_fail_entrypoint(CFGAcc,Ep)
%		     end,
%		     CFG0,
%		     Entries),
%   CFG = hipe_rtl_cfg:info_update(CFG1, hipe_rtl:rtl_info(Rtl)),
%   take_bbs(Code, CFG).


is_fail_entrypoint(Label) ->
   lists:member(entry, hipe_rtl:info(Label)).


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
    jsr -> 
      case hipe_rtl:jsr_fail(Instr) of
	[] ->  [hipe_rtl:jsr_continuation(Instr)];
	Fail -> [hipe_rtl:jsr_continuation(Instr),Fail]
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
      fail_to -> true;
      jsr -> true;
      esr -> true;
      enter -> true;
      return -> true;
      jmp -> true;
      jmp_link -> true;
      call -> true;
      _ -> false
   end.


redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_rtl:redirect_jmp(Jmp, ToOld, ToNew).


pp(CFG) ->
   hipe_rtl:pp(linearize(CFG)).

pp(Dev, CFG) ->
   hipe_rtl:pp(Dev, linearize(CFG)).


linearize(CFG) ->
   Code = linearize_cfg(CFG),
   Rtl = hipe_rtl:mk_rtl(function(CFG),
		    params(CFG),
		    Code, 
		    data(CFG),	 
		    var_range(CFG), 
		    label_range(CFG)),
   hipe_rtl:rtl_info_update(Rtl, info(CFG)).


function(CFG) ->
   {Fun, _} = extra(CFG),
   Fun.


params(CFG) ->
   {_, Params} = extra(CFG),
   Params.


arity(CFG) ->
   {M,F,A} = function(CFG),
   A.
