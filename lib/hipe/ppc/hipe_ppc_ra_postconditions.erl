%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra_postconditions).
-export([check_and_rewrite/4]).
-include("hipe_ppc.hrl").

check_and_rewrite(Defun, Coloring, DontSpill, _Options) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_ppc_specific),
  #defun{code=Code0} = Defun,
  {Code1,NewDontSpill} = do_insns(Code0, TempMap, [], DontSpill),
  VarRange = {0, hipe_gensym:get_var(ppc)},
  {Defun#defun{code=Code1, var_range=VarRange},
   Coloring, % XXX: why return Coloring?
   NewDontSpill}.

do_insns([I|Insns], TempMap, Accum, DontSpill) ->
  {NewIs, NewDontSpill} = do_insn(I, TempMap, DontSpill),
  do_insns(Insns, TempMap, lists:reverse(NewIs, Accum), NewDontSpill);
do_insns([], _TempMap, Accum, DontSpill) ->
  {lists:reverse(Accum), DontSpill}.

do_insn(I, TempMap, DontSpill) ->
  case I of
    #alu{} -> do_alu(I, TempMap, DontSpill);
    #cmp{} -> do_cmp(I, TempMap, DontSpill);
    #load{} -> do_load(I, TempMap, DontSpill);
    #loadx{} -> do_loadx(I, TempMap, DontSpill);
    #mfspr{} -> do_mfspr(I, TempMap, DontSpill);
    #mtspr{} -> do_mtspr(I, TempMap, DontSpill);
    #pseudo_li{} -> do_pseudo_li(I, TempMap, DontSpill);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, DontSpill);
    #store{} -> do_store(I, TempMap, DontSpill);
    #storex{} -> do_storex(I, TempMap, DontSpill);
    #unary{} -> do_unary(I, TempMap, DontSpill);
    _ -> {[I], DontSpill}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixSrc1,NewSrc1,DontSpill2} = fix_src(Src1, TempMap, DontSpill1),
  {FixSrc2,NewSrc2,DontSpill3} = fix_src_or_imm(Src2, TempMap, DontSpill2),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DontSpill3}.

do_cmp(I=#cmp{src1=Src1,src2=Src2}, TempMap, DontSpill0) ->
  {FixSrc1,NewSrc1,DontSpill1} = fix_src(Src1, TempMap, DontSpill0),
  {FixSrc2,NewSrc2,DontSpill2} = fix_src_or_imm(Src2, TempMap, DontSpill1),
  NewI = I#cmp{src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI], DontSpill2}.

do_load(I=#load{dst=Dst,base=Base}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixBase,NewBase,DontSpill2} = fix_src(Base, TempMap, DontSpill1),
  NewI = I#load{dst=NewDst,base=NewBase},
  {FixBase ++ [NewI | FixDst], DontSpill2}.

do_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixBase1,NewBase1,DontSpill2} = fix_src(Base1, TempMap, DontSpill1),
  {FixBase2,NewBase2,DontSpill3} = fix_src(Base2, TempMap, DontSpill2),
  NewI = I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI | FixDst], DontSpill3}.

do_mfspr(I=#mfspr{dst=Dst}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  NewI = I#mfspr{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_mtspr(I=#mtspr{src=Src}, TempMap, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src(Src, TempMap, DontSpill0),
  NewI = I#mtspr{src=NewSrc},
  {FixSrc ++ [NewI], DontSpill1}.

do_pseudo_li(I=#pseudo_li{dst=Dst}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  NewI = I#pseudo_li{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, DontSpill0) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_tailcall are special cases: in
  %% all other instructions, all temps must be non-pseudos
  %% after register allocation.
  case temp_is_spilled(Dst, TempMap) of
    true -> % Src must not be a pseudo
      {FixSrc,NewSrc,DontSpill1} = fix_src(Src, TempMap, DontSpill0),
      NewI = I#pseudo_move{src=NewSrc},
      {FixSrc ++ [NewI], DontSpill1};
    _ ->
      {[I], DontSpill0}
  end.

do_store(I=#store{src=Src,base=Base}, TempMap, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src(Src, TempMap, DontSpill0),
  {FixBase,NewBase,DontSpill2} = fix_src(Base, TempMap, DontSpill1),
  NewI = I#store{src=NewSrc,base=NewBase},
  {FixSrc ++ FixBase ++ [NewI], DontSpill2}.

do_storex(I=#storex{src=Src,base1=Base1,base2=Base2}, TempMap, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src(Src, TempMap, DontSpill0),
  {FixBase1,NewBase1,DontSpill2} = fix_src(Base1, TempMap, DontSpill1),
  {FixBase2,NewBase2,DontSpill3} = fix_src(Base2, TempMap, DontSpill2),
  NewI = I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2},
  {FixSrc ++ FixBase1 ++ FixBase2 ++ [NewI], DontSpill3}.

do_unary(I=#unary{dst=Dst,src=Src}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixSrc,NewSrc,DontSpill2} = fix_src(Src, TempMap, DontSpill1),
  NewI = I#unary{dst=NewDst,src=NewSrc},
  {FixSrc ++ [NewI | FixDst], DontSpill2}.

%%% Fix Dst and Src operands.

fix_src_or_imm(Src2, TempMap, DontSpill) ->
  case Src2 of
    #ppc_temp{} -> fix_src(Src2, TempMap, DontSpill);
    _ -> {[], Src2, DontSpill}
  end.

fix_src(Src, TempMap, DontSpill) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src),
      {[hipe_ppc:mk_pseudo_move(NewSrc, Src)],
       NewSrc,
       [NewSrc|DontSpill]};
    _ ->
      {[], Src, DontSpill}
  end.

fix_dst(Dst, TempMap, DontSpill) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst),
      {[hipe_ppc:mk_pseudo_move(Dst, NewDst)],
       NewDst,
       [NewDst|DontSpill]};
    _ ->
      {[], Dst, DontSpill}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, TempMap) ->
  case hipe_ppc:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_ppc:temp_reg(Temp),
      case size(TempMap) > Reg of
	true -> hipe_temp_map:is_spilled(Reg, TempMap);
	false -> false
      end;
    false ->true
  end.

%%% Create a new temp with the same type as an old one.

clone(Temp) ->
  Type = hipe_ppc:temp_type(Temp),
  hipe_ppc:mk_new_temp(Type).
