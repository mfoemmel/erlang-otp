%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra_naive).
-export([ra/3]).

-include("hipe_ppc.hrl").

ra(Defun, _Coloring_fp=[], _Options) ->	% -> {Defun, Coloring}
  #defun{code=Code0} = Defun,
  Code1 = do_insns(Code0),
  TempMap = [],
  VarRange = {0, hipe_gensym:get_var(ppc)},
  {Defun#defun{code=Code1,var_range=VarRange},
   TempMap}.

do_insns([I|Insns]) ->
  do_insn(I) ++ do_insns(Insns);
do_insns([]) ->
  [].

do_insn(I) ->	% Insn -> Insn list
  case I of
    #alu{} -> do_alu(I);
    #cmp{} -> do_cmp(I);
    #load{} -> do_load(I);
    #loadx{} -> do_loadx(I);
    #mfspr{} -> do_mfspr(I);
    #mtspr{} -> do_mtspr(I);
    #pseudo_li{} -> do_pseudo_li(I);
    #pseudo_move{} -> do_pseudo_move(I);
    #store{} -> do_store(I);
    #storex{} -> do_storex(I);
    #unary{} -> do_unary(I);
    _ -> [I]
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  {FixSrc1,NewSrc1} = fix_src1(Src1),
  {FixSrc2,NewSrc2} = fix_src2_or_imm(Src2),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  FixSrc1 ++ FixSrc2 ++ [NewI | FixDst].

do_cmp(I=#cmp{src1=Src1,src2=Src2}) ->
  {FixSrc1,NewSrc1} = fix_src1(Src1),
  {FixSrc2,NewSrc2} = fix_src2_or_imm(Src2),
  NewI = I#cmp{src1=NewSrc1,src2=NewSrc2},
  FixSrc1 ++ FixSrc2 ++ [NewI].

do_load(I=#load{dst=Dst,base=Base}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  {FixBase,NewBase} = fix_src1(Base),
  NewI = I#load{dst=NewDst,base=NewBase},
  FixBase ++ [NewI | FixDst].

do_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  {FixBase1,NewBase1} = fix_src1(Base1),
  {FixBase2,NewBase2} = fix_src2(Base2),
  NewI = I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2},
  FixBase1 ++ FixBase2 ++ [NewI | FixDst].

do_mfspr(I=#mfspr{dst=Dst}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  NewI = I#mfspr{dst=NewDst},
  [NewI | FixDst].

do_mtspr(I=#mtspr{src=Src}) ->
  {FixSrc,NewSrc} = fix_src1(Src),
  NewI = I#mtspr{src=NewSrc},
  FixSrc ++ [NewI].

do_pseudo_li(I=#pseudo_li{dst=Dst}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  NewI = I#pseudo_li{dst=NewDst},
  [NewI | FixDst].

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_tailcall are special cases: in
  %% all other instructions, all temps must be non-pseudos
  %% after register allocation.
  case temp_is_pseudo(Dst) of
    true -> % Src must not be a pseudo
      {FixSrc,NewSrc} = fix_src1(Src),
      NewI = I#pseudo_move{src=NewSrc},
      FixSrc ++ [NewI];
    _ ->
      [I]
  end.

do_store(I=#store{src=Src,base=Base}) ->
  {FixSrc,NewSrc} = fix_src1(Src),
  {FixBase,NewBase} = fix_src2(Base),
  NewI = I#store{src=NewSrc,base=NewBase},
  FixSrc ++ FixBase ++ [NewI].

do_storex(I=#storex{src=Src,base1=Base1,base2=Base2}) ->
  {FixSrc,NewSrc} = fix_src1(Src),
  {FixBase1,NewBase1} = fix_src2(Base1),
  {FixBase2,NewBase2} = fix_src3(Base2),
  NewI = I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2},
  FixSrc ++ FixBase1 ++ FixBase2 ++ [NewI].

do_unary(I=#unary{dst=Dst,src=Src}) ->
  {FixDst,NewDst} = fix_dst(Dst),
  {FixSrc,NewSrc} = fix_src1(Src),
  NewI = I#unary{dst=NewDst,src=NewSrc},
  FixSrc ++ [NewI | FixDst].

%%% Fix Dst and Src operands.

fix_src2_or_imm(Src2) ->
  case Src2 of
    #ppc_temp{} -> fix_src2(Src2);
    _ -> {[], Src2}
  end.

fix_src1(Src) -> fix_src(Src, hipe_ppc_registers:temp1()).
fix_src2(Src) -> fix_src(Src, hipe_ppc_registers:temp2()).
fix_src3(Src) -> fix_src(Src, hipe_ppc_registers:temp3()).

fix_src(Src, ScratchReg) ->	% -> {Moves, NewTemp}
  case temp_is_pseudo(Src) of
    true ->
      NewSrc = clone(Src, ScratchReg),
      {[hipe_ppc:mk_pseudo_move(NewSrc, Src)], NewSrc};
    _ ->
      {[], Src}
  end.

fix_dst(Dst) ->
  case temp_is_pseudo(Dst) of
    true ->
      NewDst = clone(Dst, hipe_ppc_registers:temp1()),
      {[hipe_ppc:mk_pseudo_move(Dst, NewDst)], NewDst};
    _ ->
      {[], Dst}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_pseudo(Temp) ->
  not(hipe_ppc:temp_is_precoloured(Temp)).

%%% Create a new temp with the same type as an old one.

clone(Temp, Reg) ->
  Type = hipe_ppc:temp_type(Temp),
  hipe_ppc:mk_temp(Reg, Type).
