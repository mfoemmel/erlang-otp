%%% -*- erlang-indent-level: 4 -*-
%%% $Id$
%%% simple local x86 regalloc

-module(hipe_x86_ra_dummy).
-export([ra/3]).
-include("hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). % enable instrumentation
-include("../main/hipe.hrl").

ra(X86Defun, Coloring_fp, Options) ->
    #defun{code=Code0} = X86Defun,
    Code1 = do_insns(Code0),
    NofSpilledFloats = count_non_float_spills(Coloring_fp),
    NofFloats = length(Coloring_fp),
    ?add_spills(Options, hipe_gensym:get_var(x86) -
		hipe_x86_registers:first_virtual()-
		NofSpilledFloats -
		NofFloats),
    TempMap = [],
    {X86Defun#defun{code=Code1,
		    var_range={0, hipe_gensym:get_var(x86)}},
     TempMap}.

count_non_float_spills(Coloring_fp)->
    count_non_float_spills(Coloring_fp,0).
count_non_float_spills([{_,To}|Tail], Num)->
    case hipe_x86_specific_fp:is_precoloured(To) of
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
 	#movzx{} ->
	    do_movx(I);
	#movsx{} ->
	    do_movx(I);
	#fmove{} ->
 	    do_fmove(I);
% 	#fp_unop{} ->
% 	    do_fp_unop(I);
% 	#fp_binop{} ->
% 	    do_fp_binop(I);
	#shift{} ->
	    do_shift(I);
	_ ->
	    %% comment, jmp*, label, pseudo_call, pseudo_jcc, pseudo_tailcall,
	    %% pseudo_tailcall_prepare, push, ret
	    [I]
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
    #jmp_switch{temp=Temp} = I,
    case temp_is_pseudo(Temp) of
	false ->
	    [I];
	true ->
	    Reg = hipe_x86:mk_temp(hipe_x86_registers:temp0(), 'untagged'),
	    [hipe_x86:mk_move(Temp, Reg), I#jmp_switch{temp=Reg}]
    end.

%%% Fix a lea op.

do_lea(I) ->
    #lea{temp=Temp} = I,
    case temp_is_pseudo(Temp) of
	false ->
	    [I];
	true ->
	    Reg = hipe_x86:mk_temp(hipe_x86_registers:temp0(), 'untagged'),
	    [I#lea{temp=Reg}, hipe_x86:mk_move(Reg, Temp)]
    end.

%%% Fix a move op.

do_move(I) ->
    #move{src=Src0,dst=Dst0} = I,
    {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
    FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}].

do_movx(I) ->
    {FixSrc, Src} =
	case I of
	    #movsx{src=Src0,dst=Dst0} ->
		fix_src_operand(Src0);
	    #movzx{src=Src0,dst=Dst0} ->
		fix_src_operand(Src0)
	end,
    {FixDst, Dst} = fix_dst_operand(Dst0),
    Reg = hipe_x86_registers:temp0(),
    Dst2 = clone(Dst, Reg),
    I2 =
	case is_mem_opnd(Dst) of
	    true ->
		Reg = hipe_x86_registers:temp0(),
		Dst2 = clone(Dst, Reg),
		case I of
		    #movsx{} ->
			[hipe_x86:mk_movsx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)];
		    #movzx{} ->
			[hipe_x86:mk_movzx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)]
		end;
	    false ->
		case I of
		    #movsx{} ->
			[hipe_x86:mk_movsx(Src, Dst)];
		    #movzx{} ->
			[hipe_x86:mk_movzx(Src, Dst)]
		end
	end,

    FixSrc ++ FixDst ++ I2.



%%% Fix a fmove op.

do_fmove(I) ->
    #fmove{src=Src0,dst=Dst0} = I,
    {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
    FixSrc ++ FixDst ++ [I#fmove{src=Src,dst=Dst}].

do_shift(I) ->
    #shift{src=Src0,dst=Dst0} = I,
    {FixDst, Dst} = fix_dst_operand(Dst0),
    Reg = hipe_x86_registers:ecx(),
    case Src0 of
	#x86_imm{} ->
	    FixDst ++ [I#shift{dst=Dst}];
	#x86_temp{reg=Reg}  ->
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
			Reg = hipe_x86_registers:temp0(),
			Src2 = clone(Src, Reg),
			FixSrc2 = FixSrc ++ [hipe_x86:mk_move(Src, Src2)],
			{FixSrc2, Src2}
		end
	end,
    {FixSrc3, Src3, FixDst, Dst}.

%%% Fix any x86_mem operand to not refer to any pseudos.
%%% The fixup may use additional instructions and registers.
%%% 'src' operands may clobber '%temp0'.
%%% 'dst' operands may clobber '%temp1'.
%%% XXX Is the following still valid now that fail_to has been dropped?
%%% (This is not arbitrary. The translation of begin_handler does a
%%% "move %eax,<dst>", and fail_to does a "move <src>,%eax".)

fix_src_operand(Opnd) ->
    fix_mem_operand(Opnd, hipe_x86_registers:temp0()).

fix_dst_operand(Opnd) ->
    fix_mem_operand(Opnd, hipe_x86_registers:temp1()).

fix_mem_operand(Opnd, Reg) ->	% -> {[fixupcode], newop}
    case Opnd of
	#x86_mem{base=Base,off=Off} ->
	    case is_mem_opnd(Base) of
		false ->
		    case src_is_pseudo(Off) of
			false ->
			    {[], Opnd};
			true ->		% pseudo(reg)
			    Temp = clone(Off, Reg),
			    {[hipe_x86:mk_move(Off, Temp)],
			     Opnd#x86_mem{off=Temp}}
		    end;
		true ->
		    Temp = clone(Base, Reg),
		    case src_is_pseudo(Off) of
			false ->	% imm/reg(pseudo)
			    {[hipe_x86:mk_move(Base, Temp)],
			     Opnd#x86_mem{base=Temp}};
			true ->		% pseudo1(pseudo0)
			    {[hipe_x86:mk_move(Base, Temp),
			      hipe_x86:mk_alu('add', Off, Temp)],
			     Opnd#x86_mem{base=Temp, off=hipe_x86:mk_imm(0)}}
		    end
	    end;
	_ ->
	    {[], Opnd}
    end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd) ->
    case Opnd of
	#x86_mem{} -> true;
	#x86_temp{} -> temp_is_pseudo(Opnd);
	_ -> false
    end.

%%% Check if an operand is a pseudo-Temp.

src_is_pseudo(Src) ->
    case hipe_x86:is_temp(Src) of
	true -> temp_is_pseudo(Src);
	false -> false
    end.

temp_is_pseudo(Temp = #x86_temp{type=Type}) ->
    if Type == 'double'-> false;
       true ->
	    not(hipe_x86_registers:is_precoloured(hipe_x86:temp_reg(Temp)))
    end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst, Reg) ->
    Type =
	case Dst of
	    #x86_mem{} -> hipe_x86:mem_type(Dst);
	    #x86_temp{} -> hipe_x86:temp_type(Dst)
	end,
    hipe_x86:mk_temp(Reg, Type).
