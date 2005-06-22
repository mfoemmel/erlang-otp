%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra_postconditions).
-export([check_and_rewrite/5]).
-include("hipe_ppc.hrl").

check_and_rewrite(Defun, Coloring, Strategy, DontSpill, _Options) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_ppc_specific),
  #defun{code=Code0} = Defun,
  {Code1,NewDontSpill} = do_insns(Code0, TempMap, Strategy, [], DontSpill),
  VarRange = {0, hipe_gensym:get_var(ppc)},
  {Defun#defun{code=Code1, var_range=VarRange},
   Coloring, % XXX: why return Coloring?
   NewDontSpill}.

do_insns([I|Insns], TempMap, Strategy, Accum, DontSpill) ->
  {NewIs, NewDontSpill} = do_insn(I, TempMap, Strategy, DontSpill),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), NewDontSpill);
do_insns([], _TempMap, _Strategy, Accum, DontSpill) ->
  {lists:reverse(Accum), DontSpill}.

do_insn(I, TempMap, Strategy, DontSpill) ->
  case I of
    #alu{} -> do_alu(I, TempMap, Strategy, DontSpill);
    #cmp{} -> do_cmp(I, TempMap, Strategy, DontSpill);
    #load{} -> do_load(I, TempMap, Strategy, DontSpill);
    #loadx{} -> do_loadx(I, TempMap, Strategy, DontSpill);
    #mfspr{} -> do_mfspr(I, TempMap, Strategy, DontSpill);
    #mtspr{} -> do_mtspr(I, TempMap, Strategy, DontSpill);
    #pseudo_li{} -> do_pseudo_li(I, TempMap, Strategy, DontSpill);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy, DontSpill);
    #store{} -> do_store(I, TempMap, Strategy, DontSpill);
    #storex{} -> do_storex(I, TempMap, Strategy, DontSpill);
    #unary{} -> do_unary(I, TempMap, Strategy, DontSpill);
    #lfd{} -> do_lfd(I, TempMap, Strategy, DontSpill);
    #lfdx{} -> do_lfdx(I, TempMap, Strategy, DontSpill);
    #stfd{} -> do_stfd(I, TempMap, Strategy, DontSpill);
    #stfdx{} -> do_stfdx(I, TempMap, Strategy, DontSpill);
    _ -> {[I], DontSpill}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  {FixSrc1,NewSrc1,DontSpill2} = fix_src1(Src1, TempMap, Strategy, DontSpill1),
  {FixSrc2,NewSrc2,DontSpill3} = fix_src2_or_imm(Src2, TempMap, Strategy, DontSpill2),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DontSpill3}.

do_cmp(I=#cmp{src1=Src1,src2=Src2}, TempMap, Strategy, DontSpill0) ->
  {FixSrc1,NewSrc1,DontSpill1} = fix_src1(Src1, TempMap, Strategy, DontSpill0),
  {FixSrc2,NewSrc2,DontSpill2} = fix_src2_or_imm(Src2, TempMap, Strategy, DontSpill1),
  NewI = I#cmp{src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI], DontSpill2}.

do_load(I=#load{dst=Dst,base=Base}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  {FixBase,NewBase,DontSpill2} = fix_src1(Base, TempMap, Strategy, DontSpill1),
  NewI = I#load{dst=NewDst,base=NewBase},
  {FixBase ++ [NewI | FixDst], DontSpill2}.

do_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  {FixBase1,NewBase1,DontSpill2} = fix_src1(Base1, TempMap, Strategy, DontSpill1),
  {FixBase2,NewBase2,DontSpill3} = fix_src2(Base2, TempMap, Strategy, DontSpill2),
  NewI = I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI | FixDst], DontSpill3}.

do_mfspr(I=#mfspr{dst=Dst}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  NewI = I#mfspr{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_mtspr(I=#mtspr{src=Src}, TempMap, Strategy, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src1(Src, TempMap, Strategy, DontSpill0),
  NewI = I#mtspr{src=NewSrc},
  {FixSrc ++ [NewI], DontSpill1}.

do_pseudo_li(I=#pseudo_li{dst=Dst}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  NewI = I#pseudo_li{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, Strategy, DontSpill0) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_tailcall are special cases: in
  %% all other instructions, all temps must be non-pseudos
  %% after register allocation.
  case temp_is_spilled(Dst, TempMap) of
    true -> % Src must not be a pseudo
      {FixSrc,NewSrc,DontSpill1} = fix_src1(Src, TempMap, Strategy, DontSpill0),
      NewI = I#pseudo_move{src=NewSrc},
      {FixSrc ++ [NewI], DontSpill1};
    _ ->
      {[I], DontSpill0}
  end.

do_store(I=#store{src=Src,base=Base}, TempMap, Strategy, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src1(Src, TempMap, Strategy, DontSpill0),
  {FixBase,NewBase,DontSpill2} = fix_src2(Base, TempMap, Strategy, DontSpill1),
  NewI = I#store{src=NewSrc,base=NewBase},
  {FixSrc ++ FixBase ++ [NewI], DontSpill2}.

do_storex(I=#storex{src=Src,base1=Base1,base2=Base2}, TempMap, Strategy, DontSpill0) ->
  {FixSrc,NewSrc,DontSpill1} = fix_src1(Src, TempMap, Strategy, DontSpill0),
  {FixBase1,NewBase1,DontSpill2} = fix_src2(Base1, TempMap, Strategy, DontSpill1),
  {FixBase2,NewBase2,DontSpill3} = fix_src3(Base2, TempMap, Strategy, DontSpill2),
  NewI = I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2},
  {FixSrc ++ FixBase1 ++ FixBase2 ++ [NewI], DontSpill3}.

do_unary(I=#unary{dst=Dst,src=Src}, TempMap, Strategy, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, Strategy, DontSpill0),
  {FixSrc,NewSrc,DontSpill2} = fix_src1(Src, TempMap, Strategy, DontSpill1),
  NewI = I#unary{dst=NewDst,src=NewSrc},
  {FixSrc ++ [NewI | FixDst], DontSpill2}.

do_lfd(I=#lfd{base=Base}, TempMap, Strategy, DontSpill0) ->
  {FixBase,NewBase,DontSpill1} = fix_src1(Base, TempMap, Strategy, DontSpill0),
  NewI = I#lfd{base=NewBase},
  {FixBase ++ [NewI], DontSpill1}.

do_lfdx(I=#lfdx{base1=Base1,base2=Base2}, TempMap, Strategy, DontSpill0) ->
  {FixBase1,NewBase1,DontSpill1} = fix_src1(Base1, TempMap, Strategy, DontSpill0),
  {FixBase2,NewBase2,DontSpill2} = fix_src2(Base2, TempMap, Strategy, DontSpill1),
  NewI = I#lfdx{base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI], DontSpill2}.

do_stfd(I=#stfd{base=Base}, TempMap, Strategy, DontSpill0) ->
  {FixBase,NewBase,DontSpill1} = fix_src1(Base, TempMap, Strategy, DontSpill0),
  NewI = I#stfd{base=NewBase},
  {FixBase ++ [NewI], DontSpill1}.

do_stfdx(I=#stfdx{base1=Base1,base2=Base2}, TempMap, Strategy, DontSpill0) ->
  {FixBase1,NewBase1,DontSpill1} = fix_src1(Base1, TempMap, Strategy, DontSpill0),
  {FixBase2,NewBase2,DontSpill2} = fix_src2(Base2, TempMap, Strategy, DontSpill1),
  NewI = I#stfdx{base1=NewBase1,base2=NewBase2},
  {FixBase1 ++ FixBase2 ++ [NewI], DontSpill2}.

%%% Fix Dst and Src operands.

fix_src2_or_imm(Src2, TempMap, Strategy, DontSpill) ->
  case Src2 of
    #ppc_temp{} -> fix_src2(Src2, TempMap, Strategy, DontSpill);
    _ -> {[], Src2, DontSpill}
  end.

fix_src1(Src, TempMap, Strategy, DontSpill) ->
  fix_src(Src, TempMap, DontSpill, temp1(Strategy)).

temp1('normal') -> [];
temp1('linearscan') -> hipe_ppc_registers:temp1().

fix_src2(Src, TempMap, Strategy, DontSpill) ->
  fix_src(Src, TempMap, DontSpill, temp2(Strategy)).

temp2('normal') -> [];
temp2('linearscan') -> hipe_ppc_registers:temp2().

fix_src3(Src, TempMap, Strategy, DontSpill) -> % storex :-(
  fix_src(Src, TempMap, DontSpill, temp3(Strategy)).

temp3('normal') -> [];
temp3('linearscan') -> hipe_ppc_registers:temp3().

fix_src(Src, TempMap, DontSpill, RegOpt) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src, RegOpt),
      {[hipe_ppc:mk_pseudo_move(NewSrc, Src)],
       NewSrc,
       [NewSrc|DontSpill]};
    _ ->
      {[], Src, DontSpill}
  end.

fix_dst(Dst, TempMap, Strategy, DontSpill) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, temp3(Strategy)),
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
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_ppc:temp_type(Temp),
  case RegOpt of
    [] -> hipe_ppc:mk_new_temp(Type);
    Reg -> hipe_ppc:mk_temp(Reg, Type)
  end.
