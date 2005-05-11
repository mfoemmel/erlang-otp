%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_ppc_ra_postconditions_fp.erl,v 1.1 2004/12/06 03:10:13 mikpe Exp $

-module(hipe_ppc_ra_postconditions_fp).
-export([check_and_rewrite/4]).
-include("hipe_ppc.hrl").

check_and_rewrite(Defun, Coloring, DontSpill, _Options) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_ppc_specific_fp),
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
    #lfd{} -> do_lfd(I, TempMap, DontSpill);
    #lfdx{} -> do_lfdx(I, TempMap, DontSpill);
    #stfd{} -> do_stfd(I, TempMap, DontSpill);
    #stfdx{} -> do_stfdx(I, TempMap, DontSpill);
    #fp_binary{} -> do_fp_binary(I, TempMap, DontSpill);
    #fp_unary{} -> do_fp_unary(I, TempMap, DontSpill);
    #pseudo_fmove{} -> do_pseudo_fmove(I, TempMap, DontSpill);
    _ -> {[I], DontSpill}
  end.

%%% Fix relevant instruction types.

do_lfd(I=#lfd{dst=Dst}, TempMap, DontSpill0) ->
  {FixDst, NewDst, DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  NewI = I#lfd{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_lfdx(I=#lfdx{dst=Dst}, TempMap, DontSpill0) ->
  {FixDst, NewDst, DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  NewI = I#lfdx{dst=NewDst},
  {[NewI | FixDst], DontSpill1}.

do_stfd(I=#stfd{src=Src}, TempMap, DontSpill0) ->
  {FixSrc, NewSrc, DontSpill1} = fix_src(Src, TempMap, DontSpill0),
  NewI = I#stfd{src=NewSrc},
  {FixSrc ++ [NewI], DontSpill1}.

do_stfdx(I=#stfdx{src=Src}, TempMap, DontSpill0) ->
  {FixSrc, NewSrc, DontSpill1} = fix_src(Src, TempMap, DontSpill0),
  NewI = I#stfdx{src=NewSrc},
  {FixSrc ++ [NewI], DontSpill1}.

do_fp_binary(I=#fp_binary{dst=Dst,src1=Src1,src2=Src2}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixSrc1,NewSrc1,DontSpill2} = fix_src(Src1, TempMap, DontSpill1),
  {FixSrc2,NewSrc2,DontSpill3} = fix_src(Src2, TempMap, DontSpill2),
  NewI = I#fp_binary{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DontSpill3}.

do_fp_unary(I=#fp_unary{dst=Dst,src=Src}, TempMap, DontSpill0) ->
  {FixDst,NewDst,DontSpill1} = fix_dst(Dst, TempMap, DontSpill0),
  {FixSrc,NewSrc,DontSpill2} = fix_src(Src, TempMap, DontSpill1),
  NewI = I#fp_unary{dst=NewDst,src=NewSrc},
  {FixSrc ++ [NewI | FixDst], DontSpill2}.

do_pseudo_fmove(I=#pseudo_fmove{dst=Dst,src=Src}, TempMap, DontSpill0) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      {FixSrc,NewSrc,DontSpill1} = fix_src(Src, TempMap, DontSpill0),
      NewI = I#pseudo_fmove{src=NewSrc},
      {FixSrc ++ [NewI], DontSpill1};
    _ ->
      {[I], DontSpill0}
  end.

%%% Fix Dst and Src operands.

fix_src(Src, TempMap, DontSpill) ->
  case temp_is_spilled(Src, TempMap) of
    true ->
      NewSrc = clone(Src),
      {[hipe_ppc:mk_pseudo_fmove(NewSrc, Src)],
       NewSrc,
       [NewSrc|DontSpill]};
    _ ->
      {[], Src, DontSpill}
  end.

fix_dst(Dst, TempMap, DontSpill) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst),
      {[hipe_ppc:mk_pseudo_fmove(Dst, NewDst)],
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

%%% Create a new temp with the same type as an old one.

clone(Temp) ->
  Type = hipe_ppc:temp_type(Temp),	% XXX: always double?
  hipe_ppc:mk_new_temp(Type).
