%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%
%%% - apply temp -> reg/spill map from RA

-module(hipe_amd64_ra_finalise).
-export([finalise/4]).
-include("../x86/hipe_x86.hrl").

finalise(Defun, TempMap, FpMap, Options) ->
  Defun1 = finalise_ra(Defun, TempMap, FpMap, Options),
  case proplists:get_bool(x87, Options) of
    true ->
      hipe_amd64_x87:map(Defun1);
    _ ->
      Defun1
  end.

%%%
%%% Finalise the temp->reg/spill mapping.
%%% (XXX: maybe this should be merged with the main pass,
%%% but I just want this to work now)
%%%

finalise_ra(Defun, [], [], _Options) ->
  Defun;
finalise_ra(Defun, TempMap, FpMap, Options) ->
  Code = hipe_x86:defun_code(Defun),
  {_, SpillLimit} = hipe_x86:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  FpMap0 = case proplists:get_bool(x87, Options) of
	     true  -> mk_ra_map_x87(FpMap, SpillLimit);
	     false -> mk_ra_map_sse2(FpMap, SpillLimit)
	   end,
  NewCode = ra_code(Code, Map, FpMap0),
  Defun#defun{code=NewCode}.

ra_code(Code, Map, FpMap) ->
  [ra_insn(I, Map, FpMap) || I <- Code].

ra_insn(I, Map, FpMap) ->
  case I of
    #alu{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#alu{src=Src,dst=Dst};
    #call{} ->
      I;
    #cmovcc{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#cmovcc{src=Src,dst=Dst};
    #cmp{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#cmp{src=Src,dst=Dst};
    #comment{} ->
      I;
    #dec{dst=Dst0} ->
      Dst = ra_opnd(Dst0, Map),
      I#dec{dst=Dst};
    #fmove{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map, FpMap),
      Dst = ra_opnd(Dst0, Map, FpMap),
      I#fmove{src=Src,dst=Dst};
    #fp_unop{arg=Arg0} ->
      Arg = ra_opnd(Arg0, Map, FpMap),
      I#fp_unop{arg=Arg};
    #fp_binop{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map, FpMap),
      Dst = ra_opnd(Dst0, Map, FpMap),
      I#fp_binop{src=Src,dst=Dst};
    #inc{dst=Dst0} ->
      Dst = ra_opnd(Dst0, Map),
      I#inc{dst=Dst};
    #jcc{} ->
      I;
    #jmp_fun{'fun'=Fun0} ->
      Fun = ra_opnd(Fun0, Map),
      I#jmp_fun{'fun'=Fun};
    #jmp_label{} ->
      I;
    #jmp_switch{temp=Temp0, jtab=JTab0} ->
      Temp = ra_opnd(Temp0, Map),
      JTab = ra_opnd(JTab0, Map),      
      I#jmp_switch{temp=Temp, jtab=JTab};
    #label{} ->
      I;
    #lea{mem=Mem0,temp=Temp0} ->
      Mem = ra_mem(Mem0, Map),
      Temp = ra_temp(Temp0, Map),
      I#lea{mem=Mem,temp=Temp};
    #move{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#move{src=Src,dst=Dst};
    #move64{dst=Dst0} ->
      Dst = ra_opnd(Dst0, Map),
      I#move64{dst=Dst};
    #movsx{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#movsx{src=Src,dst=Dst};
    #movzx{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#movzx{src=Src,dst=Dst};
    #nop{} ->
      I;
    #pseudo_call{'fun'=Fun0} ->
      Fun = ra_opnd(Fun0, Map),
      I#pseudo_call{'fun'=Fun};
    #pseudo_jcc{} ->
      I;
    #pseudo_tailcall{'fun'=Fun0,stkargs=StkArgs0} ->
      Fun = ra_opnd(Fun0, Map),
      StkArgs = ra_args(StkArgs0, Map),
      I#pseudo_tailcall{'fun'=Fun,stkargs=StkArgs};
    #pseudo_tailcall_prepare{} ->
      I;
    #push{src=Src0} ->
      Src = ra_opnd(Src0, Map),
      I#push{src=Src};
    #ret{} ->
      I;
    #shift{src=Src0,dst=Dst0} ->
      Src = ra_opnd(Src0, Map),
      Dst = ra_opnd(Dst0, Map),
      I#shift{src=Src,dst=Dst};
    _ ->
      exit({?MODULE,ra_insn,I})
  end.

ra_args(Args, Map) ->
  [ra_opnd(Opnd, Map) || Opnd <- Args].

ra_opnd(Opnd, Map) ->
  ra_opnd(Opnd, Map, gb_trees:empty()).
ra_opnd(Opnd, Map, FpMap) ->
  case Opnd of
    #x86_temp{} -> ra_temp(Opnd, Map, FpMap);
    #x86_mem{} -> ra_mem(Opnd, Map);
    _ -> Opnd
  end.

ra_mem(Mem, Map) ->
  #x86_mem{base=Base0,off=Off0} = Mem,
  Base = ra_opnd(Base0, Map),
  Off = ra_opnd(Off0, Map),
  Mem#x86_mem{base=Base,off=Off}.

ra_temp(Temp, Map) ->
  ra_temp(Temp, Map, gb_trees:empty()).

ra_temp(Temp, Map, FpMap) ->
  Reg = hipe_x86:temp_reg(Temp),
  case hipe_x86:temp_type(Temp) of
    double ->
      case hipe_amd64_registers:is_precoloured_sse2(Reg) of
	true ->
	  Temp;
	_ ->
	  case gb_trees:lookup(Reg, FpMap) of
	    {value,NewReg} -> Temp#x86_temp{reg=NewReg};
	    _ -> Temp
	  end
      end;
    _->
      case hipe_amd64_registers:is_precoloured(Reg) of
	true ->
	  Temp;
	_ ->
	  case gb_trees:lookup(Reg, Map) of
	    {value,NewReg} -> Temp#x86_temp{reg=NewReg};
	    _ -> Temp
	  end
      end
  end.

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% (I'd prefer to use negative indices, but that breaks
  %% hipe_amd64_registers:is_precoloured/1.)
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit, IsPrecoloured) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_amd64_registers:IsPrecoloured(From) of
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
	  case hipe_amd64_registers:IsPrecoloured(NewReg) of
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
      MaxTempNum = hipe_gensym:get_var(x86),
      if MaxTempNum >= ToTempNum -> [];
	 true -> hipe_gensym:set_var(x86, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.

mk_ra_map_sse2(FpMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_sse2),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FpMap).

mk_ra_map_x87(FpMap, SpillLimit) ->
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit,
					     is_precoloured_x87),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      FpMap).
