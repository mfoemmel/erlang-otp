%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% simple local amd64 regalloc

-module(hipe_amd64_ra_dummy).
-export([ra/3]).
-include("hipe_amd64.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). % enable instrumentation
-include("../main/hipe.hrl").

ra(Amd64Defun, Coloring_fp, Options) ->
  #defun{code=Code0} = Amd64Defun,
  Code1 = do_insns(Code0),
  NofSpilledFloats = count_non_float_spills(Coloring_fp),
  NofFloats = length(Coloring_fp),
  ?add_spills(Options, hipe_gensym:get_var(amd64) -
	      hipe_amd64_registers:first_virtual()-
	      NofSpilledFloats -
	      NofFloats),
  TempMap = [],
  {Amd64Defun#defun{code=Code1,
		  var_range={0, hipe_gensym:get_var(amd64)}},
   TempMap}.

count_non_float_spills(Coloring_fp)->
  count_non_float_spills(Coloring_fp,0).
count_non_float_spills([{_,To}|Tail], Num)->
  case hipe_amd64_specific_sse2:is_precoloured(To) of
    true ->
      count_non_float_spills(Tail, Num);
    _ ->
      count_non_float_spills(Tail, Num+1)
  end;
count_non_float_spills([],Num) ->
  Num.

do_insns([I|Insns]) ->
  do_insn(I) ++ do_insns(Insns);
do_insns([]) ->
  [].

do_insn(I) ->	% Insn -> Insn list
  case I of
    #alu{} ->
      do_alu(I);
    #cmp{} ->
      do_cmp(I);
    #jmp_switch{} ->
      do_jmp_switch(I);
    #lea{} ->
      do_lea(I);
    #move{} ->
      do_move(I);
    #move64{} ->
      do_move64(I);
    #movzx{} ->
      do_movx(I);
    #movsx{} ->
      do_movx(I);
    #fmove{} ->
      do_fmove(I);
    #fp_unop{} ->
      do_fp_unop(I);
    #fp_binop{} ->
      do_fp_binop(I);
    #shift{} ->
      do_shift(I);
    #label{} ->
      [I];
    #pseudo_jcc{} ->
      [I]; 
    #pseudo_call{} ->
      [I];
    #ret{} ->
      [I];
    #pseudo_tailcall_prepare{} ->
      [I];
    #pseudo_tailcall{} ->      
      [I];
    #push{} ->     
      [I];
    #jmp_label{} ->     
      [I];
    #comment{} ->     
      [I];
 _ ->
      %% comment, jmp*, label, pseudo_call, pseudo_jcc, pseudo_tailcall,
      %% pseudo_tailcall_prepare, push, ret
      io:format("Instruction = ~w\n", [I]),
      exit({not_implemented_yet, ra_dummy , I})
  end.

%%% Fix an alu op.
do_alu(I) ->
  #alu{src=Src0,dst=Dst0} = I,
  {FixSrc,Src,FixDst,Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#alu{src=Src,dst=Dst}].

%%% Fix a cmp op.

do_cmp(I) ->
  #cmp{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#cmp{src=Src,dst=Dst}].

%%% Fix a jmp_switch op.

do_jmp_switch(I) ->
  #jmp_switch{temp=Temp, jtab=Tab} = I,
  case temp_is_pseudo(Temp) of
    false ->
      case temp_is_pseudo(Tab) of
        false ->
          [I];
        true ->
          Reg = hipe_amd64:mk_temp(hipe_amd64_registers:temp0(), 'untagged'),
          [hipe_amd64:mk_move(Temp, Reg), I#jmp_switch{jtab=Reg}]
      end;
    true ->
      Reg = hipe_amd64:mk_temp(hipe_amd64_registers:temp1(), 'untagged'),
      case temp_is_pseudo(Tab) of
        false ->
          [hipe_amd64:mk_move(Temp, Reg), I#jmp_switch{temp=Reg}];
        true ->
          Reg2 = hipe_amd64:mk_temp(hipe_amd64_registers:temp0(), 'untagged'),
          [hipe_amd64:mk_move(Temp, Reg),
           hipe_amd64:mk_move(Tab, Reg2),
           I#jmp_switch{temp=Reg, jtab=Reg2}]
      end
  end.

%%% Fix a lea op.

do_lea(I) ->
  #lea{temp=Temp} = I,
  case temp_is_pseudo(Temp) of
    false ->
      [I];
    true ->
      Reg = hipe_amd64:mk_temp(hipe_amd64_registers:temp0(), 'untagged'),
      [I#lea{temp=Reg}, hipe_amd64:mk_move(Reg, Temp)]
  end.

%%% Fix a move op.
do_move(I) ->
  #move{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}].

do_move64(I) ->
  #move64{dst=Dst} = I,
  case is_mem_opnd(Dst) of
    false ->
      [I];
    true ->     
      Reg = hipe_amd64_registers:temp1(),
      NewDst = clone(Dst, Reg),
      [I#move64{dst=NewDst}, hipe_amd64:mk_move(NewDst, Dst)]
  end.

do_movx(I) ->
  {FixSrc, Src} =
    case I of
      #movsx{src=Src0,dst=Dst0} ->
        fix_src_operand(Src0);
      #movzx{src=Src0,dst=Dst0} ->
        fix_src_operand(Src0)
    end,
  {FixDst, Dst} = fix_dst_operand(Dst0),
  Reg = hipe_amd64_registers:temp0(),
  Dst2 = clone(Dst, Reg),
  I2 =
    case is_mem_opnd(Dst) of
      true ->
        Reg = hipe_amd64_registers:temp0(),
        Dst2 = clone(Dst, Reg),
        case I of
          #movsx{} ->
            [hipe_amd64:mk_movsx(Src, Dst2), hipe_amd64:mk_move(Dst2, Dst)];
          #movzx{} ->
            [hipe_amd64:mk_movzx(Src, Dst2), hipe_amd64:mk_move(Dst2, Dst)]
        end;
      false ->
        case I of
          #movsx{} ->
            [hipe_amd64:mk_movsx(Src, Dst)];
          #movzx{} ->
            [hipe_amd64:mk_movzx(Src, Dst)]
        end
    end,
  FixSrc ++ FixDst ++ I2.

%%% Fix a fmove op.
%% conv_to_float
do_fmove(I=#fmove{src=#amd64_temp{type=untagged},
		  dst=#amd64_temp{type=double}}) ->
  #fmove{src=Src0,dst=Dst0} = I,
  Src = clone(Src0, hipe_amd64_registers:temp0()),
  Dst = clone(Dst0, hipe_amd64_registers:temp1()),
  [hipe_amd64:mk_move(Src0, Src),
   I#fmove{src=Src, dst=Dst},
   hipe_amd64:mk_fmove(Dst, Dst0)];
%% fmove
do_fmove(I) ->
  #fmove{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
  FixSrc ++ FixDst ++ [I#fmove{src=Src,dst=Dst}].

do_fp_unop(I) ->
  #fp_unop{arg=Arg} = I,
  case is_mem_opnd(Arg) of
    false ->
      [I];
    true ->
      Reg = hipe_amd64_registers:temp1(),
      NewArg = clone(Arg, Reg),
      [hipe_amd64:mk_fmove(Arg, NewArg),
       I#fp_unop{arg=NewArg},
       hipe_amd64:mk_fmove(NewArg, Arg)]
  end.

do_fp_binop(I) ->
  #fp_binop{src=Src0, dst=Dst0} = I,
  {FixSrc, Src} = fix_src_operand(Src0),
  {FixDst, Dst} = fix_dst_operand(Dst0),
  Reg = hipe_amd64_registers:temp1(),
  Dst2 = clone(Dst, Reg),
  FixSrc ++ FixDst ++ [hipe_amd64:mk_fmove(Dst, Dst2),
		       I#fp_binop{src=Src, dst=Dst2},
		       hipe_amd64:mk_fmove(Dst2, Dst)].  
  
do_shift(I) ->
  #shift{src=Src0,dst=Dst0} = I,
  {FixDst, Dst} = fix_dst_operand(Dst0),
  Reg = hipe_amd64_registers:rcx(),
  case Src0 of
    #amd64_imm{} ->
      FixDst ++ [I#shift{dst=Dst}];
    #amd64_temp{reg=Reg}  ->
      FixDst ++ [I#shift{dst=Dst}]
  end.

%%% Fix the operands of a binary op.
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if both operands are (implicit or explicit) memory operands,
%%%    move src to a reg and use reg as src in the original insn

do_binary(Src0, Dst0) ->
  {FixSrc, Src} = fix_src_operand(Src0),
  {FixDst, Dst} = fix_dst_operand(Dst0),
  {FixSrc3, Src3} =
    case is_mem_opnd(Src) of
      false ->
	{FixSrc, Src};
      true ->
	case is_mem_opnd(Dst) of
	  false ->
	    {FixSrc, Src};
	  true ->
	    Reg = hipe_amd64_registers:temp0(),
	    Src2 = clone(Src, Reg),
	    FixSrc2 = FixSrc ++ [mk_move(Src, Src2)],
	    {FixSrc2, Src2}
	end
    end,
  {FixSrc3, Src3, FixDst, Dst}.

%%% Fix any amd64_mem operand to not refer to any pseudos.
%%% The fixup may use additional instructions and registers.
%%% 'src' operands may clobber '%temp0'.
%%% 'dst' operands may clobber '%temp1'.

fix_src_operand(Opnd) ->
  fix_mem_operand(Opnd, hipe_amd64_registers:temp0()).

fix_dst_operand(Opnd) ->
  fix_mem_operand(Opnd, hipe_amd64_registers:temp1()).

fix_mem_operand(Opnd, Reg) ->    % -> {[fixupcode], newop}
  case Opnd of
    #amd64_mem{base=Base,off=Off} ->
      case is_mem_opnd(Base) of
	false ->
	  case src_is_pseudo(Off) of
	    false ->
	      {[], Opnd};
	    true ->		% pseudo(reg)
	      Temp = clone(Off, Reg),
	      {[hipe_amd64:mk_move(Off, Temp)],
	       Opnd#amd64_mem{off=Temp}}
	  end;
	true ->
	  Temp = clone(Base, Reg),
	  case src_is_pseudo(Off) of
	    false ->	% imm/reg(pseudo)
	      {[hipe_amd64:mk_move(Base, Temp)],
	       Opnd#amd64_mem{base=Temp}};
	    true ->		% pseudo1(pseudo0)
	      {[hipe_amd64:mk_move(Base, Temp),
		hipe_amd64:mk_alu('add', Off, Temp)],
	       Opnd#amd64_mem{base=Temp, off=hipe_amd64:mk_imm(0)}}
	  end
      end;
    _ ->
      {[], Opnd}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).
is_mem_opnd(Opnd) ->
  case Opnd of
    #amd64_mem{}  -> true;
    #amd64_temp{} -> temp_is_pseudo(Opnd);
    _ -> false
  end.

%%% Check if an operand is a pseudo-Temp.
src_is_pseudo(Src) ->
  case hipe_amd64:is_temp(Src) of
    true -> temp_is_pseudo(Src);
    false -> false
  end.

temp_is_pseudo(Temp) ->
  not(hipe_amd64_registers:is_precoloured(hipe_amd64:temp_reg(Temp))).

%%% Make Reg a clone of Dst (attach Dst's type to Reg).
clone(Dst, Reg) ->
  Type =
    case Dst of
      #amd64_mem{} -> hipe_amd64:mem_type(Dst);
      #amd64_temp{} -> hipe_amd64:temp_type(Dst)
    end,
  hipe_amd64:mk_temp(Reg, Type).

mk_move(Src, Dst=#amd64_temp{type=double}) ->
  hipe_amd64:mk_fmove(Src, Dst);
mk_move(Src, Dst) ->
  hipe_amd64:mk_move(Src, Dst).  
