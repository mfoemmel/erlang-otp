%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_arm_defuse).
-export([insn_def_all/1, insn_use_all/1]).
-export([insn_def_gpr/1, insn_use_gpr/1]).
-include("hipe_arm.hrl").

%%%
%%% Defs and uses for both general-purpose and floating-point registers.
%%% This is needed for the frame module, alas.
%%%
insn_def_all(I) ->
  insn_def_gpr(I).

insn_use_all(I) ->
  insn_use_gpr(I).

%%%
%%% Defs and uses for general-purpose (integer) registers only.
%%%
insn_def_gpr(I) ->
  case I of
    #alu{dst=Dst} -> [Dst];
    #load{dst=Dst} -> [Dst];
    #ldrsb{dst=Dst} -> [Dst];
    #move{dst=Dst} -> [Dst];
    #pseudo_call{} -> call_clobbered_gpr();
    #pseudo_li{dst=Dst} -> [Dst];
    #pseudo_move{dst=Dst} -> [Dst];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered_gpr();
    _ -> []
  end.

call_clobbered_gpr() ->
  [hipe_arm:mk_temp(R, T)
   || {R,T} <- hipe_arm_registers:call_clobbered() ++ all_fp_pseudos()].

all_fp_pseudos() -> [].	% XXX: for now

tailcall_clobbered_gpr() ->
  [hipe_arm:mk_temp(R, T)
   || {R,T} <- hipe_arm_registers:tailcall_clobbered() ++ all_fp_pseudos()].

insn_use_gpr(I) ->
  case I of
    #alu{src=Src,am1=Am1} -> am1_use(Am1, [Src]);
    #blx{src=Src} -> [Src];
    #cmp{src=Src,am1=Am1} -> am1_use(Am1, [Src]);
    #load{am2=Am2} -> am2_use(Am2, []);
    #ldrsb{am3=Am3} -> am3_use(Am3, []);
    #move{am1=Am1} -> am1_use(Am1, []);
    #pseudo_blr{} ->
      LR = hipe_arm:mk_temp(hipe_arm_registers:lr(), 'untagged'),
      RV = hipe_arm:mk_temp(hipe_arm_registers:return_value(), 'tagged'),
      [RV, LR];
    #pseudo_bx{src=Src} ->
      io:format("~w: whoa there! insn_use of ~w occurred\n", [?MODULE,I]),
      [Src];
    #pseudo_call{funv=FunV,sdesc=#arm_sdesc{arity=Arity}} ->
      funv_use(FunV, arity_use_gpr(Arity));
    #pseudo_move{src=Src} -> [Src];
    #pseudo_switch{jtab=JTabR,index=IndexR} -> addtemp(JTabR, [IndexR]);
    #pseudo_tailcall{funv=FunV,arity=Arity,stkargs=StkArgs} ->
      addargs(StkArgs, addtemps(tailcall_clobbered_gpr(), funv_use(FunV, arity_use_gpr(Arity))));
    #store{src=Src,am2=Am2} -> am2_use(Am2, [Src]);
    _ -> []
  end.

addargs([Arg|Args], Set) ->
  addargs(Args, addarg(Arg, Set));
addargs([], Set) ->
  Set.

addarg(Arg, Set) ->
  case Arg of
    #arm_temp{} -> addtemp(Arg, Set);
    _ -> Set
  end.

arity_use_gpr(Arity) ->
  [hipe_arm:mk_temp(R, 'tagged')
   || R <- hipe_arm_registers:args(Arity)].

funv_use(FunV, Set) ->
  case FunV of
    #arm_temp{} -> addtemp(FunV, Set);
    _ -> Set
  end.

am1_use(Am1, Set) ->
  case Am1 of
    #arm_temp{} -> addtemp(Am1, Set);
    {Src,rrx} -> addtemp(Src, Set);
    {Src,_,ShiftArg} ->
      Set1 = addtemp(Src, Set),
      case ShiftArg of
	#arm_temp{} -> addtemp(ShiftArg, Set1);
	_ -> Set1
      end;
    _ -> Set
  end.

am2_use(#am2{src=Src,offset=Am2Offset}, Set) ->
  Set1 = addtemp(Src, Set),
  case Am2Offset of
    #arm_temp{} -> addtemp(Am2Offset, Set1);
    {Src2,_} -> addtemp(Src2, Set1);
    {Src2,_,_} -> addtemp(Src2, Set1);
    _ -> Set1
  end.

am3_use(#am3{src=Src,offset=Am3Offset}, Set) ->
  Set1 = addtemp(Src, Set),
  case Am3Offset of
    #arm_temp{} -> addtemp(Am3Offset, Set1);
    _ -> Set1
  end.

%%%
%%% Auxiliary operations on sets of temps
%%% These sets are small. No point using gb_trees, right?
%%%

addtemps([Arg|Args], Set) ->
  addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
  Set.

addtemp(Temp, Set) ->
  case lists:member(Temp, Set) of
    false -> [Temp|Set];
    _ -> Set
  end.
