%%% -*- erlang-indent-level: 4 -*-
%%% $Id$
%%% simple local x86 regalloc

-module(hipe_x86_ra_dummy).
-export([ra/2]).
-include("hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). % enable instrumentation
-include("../main/hipe.hrl").

ra(X86Defun, Options) ->
    #defun{code=Code0} = X86Defun,
    Code1 = do_insns(Code0),
    %% Record all pseudos as spilled.
    ?add_spills(Options, hipe_gensym:get_var() -
		hipe_x86_registers:first_virtual()),
    TempMap = [],
    {X86Defun#defun{code=Code1}, TempMap}.

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
	_ ->
	    %% comment, jmp*, label, pseudo_jcc, pseudo_call, pseudo_tailcall,
	    %% push, ret
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
	    EAX = hipe_x86:mk_temp(hipe_x86_registers:eax(), 'untagged'),
	    [hipe_x86:mk_move(Temp, EAX), I#jmp_switch{temp=EAX}]
    end.

%%% Fix a lea op.

do_lea(I) ->
    #lea{temp=Temp} = I,
    case temp_is_pseudo(Temp) of
	false ->
	    [I];
	true ->
	    EAX = hipe_x86:mk_temp(hipe_x86_registers:eax(), 'untagged'),
	    [I#lea{temp=EAX}, hipe_x86:mk_move(EAX, Temp)]
    end.

%%% Fix a move op.

do_move(I) ->
    #move{src=Src0,dst=Dst0} = I,
    {FixSrc, Src, FixDst, Dst} = do_binary(Src0, Dst0),
    FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}].

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
			EAX = hipe_x86_registers:eax(),
			Src2 = clone(Src, EAX),
			FixSrc2 = FixSrc ++ [hipe_x86:mk_move(Src, Src2)],
			{FixSrc2, Src2}
		end
	end,
    {FixSrc3, Src3, FixDst, Dst}.

%%% Fix any x86_mem operand to not refer to any pseudos.
%%% The fixup may use additional instructions and registers.
%%% 'src' operands may clobber '%eax'.
%%% 'dst' operands may clobber '%edx'.
%%% (This is not arbitrary. The translation of restore_catch does a
%%% "move %eax,<dst>", and fail_to does a "move <src>,%eax".)

fix_src_operand(Opnd) ->
    fix_mem_operand(Opnd, hipe_x86_registers:eax()).

fix_dst_operand(Opnd) ->
    fix_mem_operand(Opnd, hipe_x86_registers:edx()).

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

temp_is_pseudo(Temp) ->
    not(hipe_x86_registers:is_precoloured(hipe_x86:temp_reg(Temp))).

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst, Reg) ->
    Type =
	case Dst of
	    #x86_mem{} -> hipe_x86:mem_type(Dst);
	    #x86_temp{} -> hipe_x86:temp_type(Dst)
	end,
    hipe_x86:mk_temp(Reg, Type).
