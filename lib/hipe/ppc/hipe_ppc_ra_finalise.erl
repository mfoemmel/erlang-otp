%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra_finalise).
-export([finalise/3]).
-include("hipe_ppc.hrl").

finalise(Defun, TempMap, _FpMap0=[]) ->
  Code = hipe_ppc:defun_code(Defun),
  {_, SpillLimit} = hipe_ppc:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  NewCode = ra_code(Code, Map, []),
  Defun#defun{code=NewCode}.

ra_code([I|Insns], Map, Accum) ->
  ra_code(Insns, Map, [ra_insn(I, Map) | Accum]);
ra_code([], _Map, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map) ->
  case I of
    #alu{} -> ra_alu(I, Map);
    #cmp{} -> ra_cmp(I, Map);
    #load{} -> ra_load(I, Map);
    #loadx{} -> ra_loadx(I, Map);
    #mfspr{} -> ra_mfspr(I, Map);
    #mtspr{} -> ra_mtspr(I, Map);
    #pseudo_li{} -> ra_pseudo_li(I, Map);
    #pseudo_move{} -> ra_pseudo_move(I, Map);
    #pseudo_tailcall{} -> ra_pseudo_tailcall(I, Map);
    #store{} -> ra_store(I, Map);
    #storex{} -> ra_storex(I, Map);
    #unary{} -> ra_unary(I, Map);
    _ -> I
  end.

ra_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_temp_or_imm(Src2, Map),
  I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2}.

ra_cmp(I=#cmp{src1=Src1,src2=Src2}, Map) ->
  NewSrc1 = ra_temp(Src1, Map),
  NewSrc2 = ra_temp_or_imm(Src2, Map),
  I#cmp{src1=NewSrc1,src2=NewSrc2}.

ra_load(I=#load{dst=Dst,base=Base}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewBase = ra_temp(Base, Map),
  I#load{dst=NewDst,base=NewBase}.

ra_loadx(I=#loadx{dst=Dst,base1=Base1,base2=Base2}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#loadx{dst=NewDst,base1=NewBase1,base2=NewBase2}.

ra_mfspr(I=#mfspr{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#mfspr{dst=NewDst}.

ra_mtspr(I=#mtspr{src=Src}, Map) ->
  NewSrc = ra_temp(Src, Map),
  I#mtspr{src=NewSrc}.

ra_pseudo_li(I=#pseudo_li{dst=Dst}, Map) ->
  NewDst = ra_temp(Dst, Map),
  I#pseudo_li{dst=NewDst}.

ra_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  I#pseudo_move{dst=NewDst,src=NewSrc}.

ra_pseudo_tailcall(I=#pseudo_tailcall{stkargs=StkArgs}, Map) ->
  NewStkArgs = ra_args(StkArgs, Map),
  I#pseudo_tailcall{stkargs=NewStkArgs}.

ra_store(I=#store{src=Src,base=Base}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewBase = ra_temp(Base, Map),
  I#store{src=NewSrc,base=NewBase}.

ra_storex(I=#storex{src=Src,base1=Base1,base2=Base2}, Map) ->
  NewSrc = ra_temp(Src, Map),
  NewBase1 = ra_temp(Base1, Map),
  NewBase2 = ra_temp(Base2, Map),
  I#storex{src=NewSrc,base1=NewBase1,base2=NewBase2}.

ra_unary(I=#unary{dst=Dst,src=Src}, Map) ->
  NewDst = ra_temp(Dst, Map),
  NewSrc = ra_temp(Src, Map),
  I#unary{dst=NewDst,src=NewSrc}.

ra_args([Arg|Args], Map) ->
  [ra_temp_or_imm(Arg, Map) | ra_args(Args, Map)];
ra_args([], _) ->
  [].

ra_temp_or_imm(Arg, Map) ->
  case hipe_ppc:is_temp(Arg) of
    true ->
      ra_temp(Arg, Map);
    false ->
      Arg
  end.

ra_temp(Temp, Map) ->
  Reg = hipe_ppc:temp_reg(Temp),
  case hipe_ppc:temp_type(Temp) of
    double ->
      exit({?MODULE,temp});
    _->
      case hipe_ppc_registers:is_precoloured(Reg) of
	true -> Temp;
	_ ->
	  case gb_trees:lookup(Reg, Map) of
	    {value,NewReg} -> Temp#ppc_temp{reg=NewReg};
	    _ -> Temp
	  end
      end
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_ppc_registers:is_precoloured/1.)
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_ppc_registers:is_precoloured(From) of
	false -> [];
	_ ->
	  case To of
	    {reg, From} -> [];
	    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
	  end
      end;
     true -> exit({?MODULE,conv_ra_maplet,MapLet})
  end,
  %% end of From check
  case To of
    {reg, NewReg} ->
      %% NewReg should be a hard reg, or a pseudo mapped
      %% to itself (formals are handled this way).
      if is_integer(NewReg) ->
	  case hipe_ppc_registers:is_precoloured(NewReg) of
	    true -> [];
	    _ -> if From =:= NewReg -> [];
		    true ->
		     exit({?MODULE,conv_ra_maplet,MapLet})
		 end
	  end;
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of NewReg check
      {From, NewReg};
    {spill, SpillIndex} ->
      %% SpillIndex should be >= 0.
      if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      %% end of SpillIndex check
      ToTempNum = SpillLimit+SpillIndex+1,
      MaxTempNum = hipe_gensym:get_var(ppc),
      if MaxTempNum >= ToTempNum -> [];
	 true -> hipe_gensym:set_var(ppc, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

-ifdef(notdef).
mk_ra_map_fp(FpMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_fp),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FpMap).
-endif.
