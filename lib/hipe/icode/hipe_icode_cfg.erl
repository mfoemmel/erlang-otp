-module(hipe_icode_cfg).

-export([function/1,
	 params/1,
	 params_update/2,
	 pp/2,
	 linearize/1]).

-include("../flow/cfg.inc").

-define(ASSERT(X),
	   case X of true ->
	       io:format("Assertion ok ~w ~w\n",[?MODULE,?LINE]),
	       true;
	     _ -> 
	       io:format("Assertion failed ~w ~w\n",[?MODULE,?LINE]),
	       exit({assert,?MODULE,?LINE, failed})
	   end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to icode
%

init(Icode) ->
  Icode0 = hipe_icode_cleanup:code(Icode),
  %% hipe_icode:pp(Icode0),
  {StartLabel, Icode1} = hipe_icode:preprocess_code(Icode0),
  %% hipe_icode:pp(Icode1),
  Extra = {hipe_icode:icode_fun(Icode1), hipe_icode:icode_params(Icode1)},
  {MinV,MaxV} = hipe_icode:icode_var_range(Icode1),
  %% ?ASSERT(hipe_icode:highest_var(hipe_icode:icode_code(Icode1)) =< MaxV),
  NewMax = hipe_icode:highest_var(hipe_icode:icode_code(Icode1)),
  
  CFG0 = mk_empty_cfg(StartLabel,
		      {MinV,NewMax},
		      hipe_icode:icode_label_range(Icode1),
		      hipe_icode:icode_data(Icode1),
		      Extra),
  CFG = hipe_icode_cfg:info_update(CFG0, hipe_icode:icode_info(Icode1)),
  FullCFG = take_bbs(hipe_icode:icode_code(Icode1), CFG),
  Reachable = postorder(FullCFG),
  All = labels(FullCFG),
  remove_blocks(FullCFG, All -- Reachable).



remove_blocks(CFG, []) ->
   CFG;
remove_blocks(CFG, [Lbl|Lbls]) ->
   remove_blocks(bb_remove(CFG, Lbl), Lbls).


is_fail_entrypoint(Label) ->
   lists:member(entry, hipe_icode:info(Label)).


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


is_branch(Instr) ->
   hipe_icode:is_branch(Instr).


redirect_jmp(Jmp, ToOld, ToNew) ->
   hipe_icode:redirect_jmp(Jmp, ToOld, ToNew).


pp(CFG) ->
   hipe_icode:pp(linearize(CFG)).

pp(Dev, CFG) ->
   hipe_icode:pp(Dev, linearize(CFG)).

linearize(CFG) ->
   Code = linearize_cfg(CFG),
   Icode = hipe_icode:mk_icode(function(CFG), 
			  params(CFG), 
			  Code,
			  data(CFG),     
			  var_range(CFG), 
			  label_range(CFG)),
   hipe_icode:icode_info_update(Icode, info(CFG)).


function(CFG) ->
   {Fun, Params} = extra(CFG),
   Fun.

params(CFG) ->
   {Fun, Params} = extra(CFG),
   Params.

params_update(CFG, NewParams) ->
   {Fun, Params} = extra(CFG),
   extra_update(CFG, {Fun, NewParams}).
