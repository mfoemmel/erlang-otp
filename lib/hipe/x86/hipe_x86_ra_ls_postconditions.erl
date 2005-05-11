%% -*- erlang-indent-level: 2 -*-
%% $Id: hipe_x86_ra_ls_postconditions.erl,v 1.15 2005/03/31 21:01:11 mikpe Exp $

-ifndef(HIPE_X86_RA_LS_POSTCONDITIONS).
-define(HIPE_X86_RA_LS_POSTCONDITIONS,	hipe_x86_ra_ls_postconditions).
-define(HIPE_X86_REGISTERS,		hipe_x86_registers).
-define(HIPE_X86_SPECIFIC,		hipe_x86_specific).
-define(ECX,				ecx).
-endif.

-module(?HIPE_X86_RA_LS_POSTCONDITIONS).
-export([check_and_rewrite/4]).

-include("../x86/hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true).
-include("../main/hipe.hrl").

-define(count_temp(T), ?cons_counter(counter_mfa_mem_temps, T)).

%%---------------------------------------------------------------------

check_and_rewrite(Defun, Coloring, DontSpill, _Options) ->  
  %% io:format("Converting\n"),
  TempMap = hipe_temp_map:cols2tuple(Coloring, ?HIPE_X86_SPECIFIC),
  %% io:format("Rewriting\n"),
  #defun{code=Code0} = Defun,
  {Code1, NewDontSpill} = do_insns(Code0, TempMap, [], DontSpill),
  {Defun#defun{code=Code1,var_range={0,hipe_gensym:get_var(x86)}}, 
   Coloring, NewDontSpill}.

do_insns([I|Insns], TempMap, Is, DontSpill) ->
  {NewIs, NewDontSpill} = do_insns(Insns, TempMap, Is, DontSpill),
  {NewI, FinalDontSpill} = do_insn(I, TempMap, NewDontSpill),
  {NewI ++ NewIs, FinalDontSpill};
do_insns([],_, Is, DontSpill) ->
  {Is, DontSpill}.

do_insn(I, TempMap, DontSpill) ->	% Insn -> Insn list
  case I of
    #alu{} ->
      do_alu(I, TempMap, DontSpill);
    #cmp{} ->
      do_cmp(I, TempMap, DontSpill);
    #jmp_switch{} ->
      do_jmp_switch(I, TempMap, DontSpill);
    #lea{} ->
      do_lea(I, TempMap, DontSpill);
    #move{} ->
      do_move(I, TempMap, DontSpill);
    #move64{} ->
      do_move64(I, TempMap, DontSpill);
    #movsx{} ->
      do_movx(I, TempMap, DontSpill);
    #movzx{} ->
      do_movx(I, TempMap, DontSpill);
    #fmove{} ->
      do_fmove(I, TempMap, DontSpill);
    #shift{} ->
      do_shift(I, TempMap, DontSpill);
    _ ->
      %% comment, jmp*, label, pseudo_call, pseudo_jcc, pseudo_tailcall,
      %% pseudo_tailcall_prepare, push, ret
      {[I], DontSpill}
  end.

%%% Fix an alu op.

do_alu(I, TempMap, DontSpill) ->
  #alu{src=Src0,dst=Dst0} = I,
  {FixSrc,Src,FixDst,Dst, NewDontSpill} = 
    do_binary(Src0, Dst0, TempMap, DontSpill),
  {FixSrc ++ FixDst ++ [I#alu{src=Src,dst=Dst}], NewDontSpill}.


%%% Fix a cmp op.

do_cmp(I, TempMap, DontSpill) ->
  #cmp{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst, NewDontSpill} = 
    do_binary(Src0, Dst0, TempMap, DontSpill),
  {FixSrc ++ FixDst ++ [I#cmp{src=Src,dst=Dst}], NewDontSpill}.

%%% Fix a jmp_switch op.

-ifdef(HIPE_AMD64).
do_jmp_switch(I, TempMap, DontSpill) ->
  #jmp_switch{temp=Temp, jtab=Tab} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      case is_spilled(Tab, TempMap) of
        false ->
          {[I], DontSpill};
        true ->
          NewTab = spill_temp('untagged'),
          {[hipe_x86:mk_move(Tab, NewTab), I#jmp_switch{jtab=Tab}],
          [NewTab|DontSpill]}
      end;
    true ->      
      case is_spilled(Tab, TempMap) of
        false ->
          NewTmp = spill_temp('untagged'),
          {[hipe_x86:mk_move(Temp, NewTmp), I#jmp_switch{temp=NewTmp}],
           [NewTmp|DontSpill]};
        true ->
          NewTmp = spill_temp('untagged'),
          NewTab = spill_temp('untagged'),
          {[hipe_x86:mk_move(Temp, NewTmp),
            hipe_x86:mk_move(Tab, NewTab), 
            I#jmp_switch{temp=NewTmp, jtab=NewTab}],
           [NewTmp,NewTab|DontSpill]}
      end
  end.
-else.	% not AMD64
do_jmp_switch(I, TempMap, DontSpill) ->
  #jmp_switch{temp=Temp} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      {[I], DontSpill};
    true ->
      NewTmp = spill_temp('untagged'),
      {[hipe_x86:mk_move(Temp, NewTmp), I#jmp_switch{temp=NewTmp}],
       [NewTmp|DontSpill]}
  end.
-endif.	% not AMD64

%%% Fix a lea op.

do_lea(I, TempMap, DontSpill) ->
  #lea{temp=Temp} = I,
  case is_spilled(Temp, TempMap) of
    false ->
      {[I], DontSpill};
    true ->
      NewTmp = spill_temp('untagged'),
      {[I#lea{temp=NewTmp}, hipe_x86:mk_move(NewTmp, Temp)],
       [NewTmp| DontSpill]}
  end.

%%% Fix a move op.

do_move(I, TempMap, DontSpill) ->
  #move{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, FixDst, Dst, NewDontSpill} = 
    do_check_byte_move(Src0, Dst0, TempMap, DontSpill),
  {FixSrc ++ FixDst ++ [I#move{src=Src,dst=Dst}],
   NewDontSpill}.

-ifdef(HIPE_AMD64).

%%% AMD64 has no issues with byte moves.
do_check_byte_move(Src0, Dst0, TempMap, DontSpill) ->
  do_binary(Src0, Dst0, TempMap, DontSpill).

-else.	% not AMD64

%%% x86 can only do byte moves to a subset of the integer registers.
do_check_byte_move(Src0, Dst0, TempMap, DontSpill) ->
  case Dst0 of
    #x86_mem{type=byte} ->
      do_byte_move(Src0, Dst0, TempMap, DontSpill);
    _ ->
      do_binary(Src0, Dst0, TempMap, DontSpill)
  end.

do_byte_move(Src0, Dst0,TempMap, DontSpill) ->
  {FixSrc, Src, DontSpill1} = fix_src_operand(Src0, TempMap),
  {FixDst, Dst, DontSpill2} = fix_dst_operand(Dst0, TempMap),
  Reg =  hipe_x86_registers:eax(),
  {FixSrc3, Src3, DontSpill3} =
    case Src of
      #x86_imm{} ->
	{FixSrc, Src, []};
      #x86_temp{reg=Reg} ->   %%Small moves must start from reg 1->4
	{FixSrc, Src, []}    %%Therefore variable sources are always put in eax 
    end,
  {FixSrc3, Src3, FixDst, Dst, 
   DontSpill3 ++ DontSpill2 ++
   DontSpill1 ++ DontSpill}.

-endif.	% not AMD64

%%% Fix a move64 op

do_move64(I, TempMap, DontSpill) ->
  #move64{dst=Dst} = I,
  case is_spilled(Dst, TempMap) of
    false ->
      {[I],DontSpill};
    true ->
      Reg = clone(Dst),
      {[I#move64{dst=Reg}, hipe_x86:mk_move(Reg, Dst)], [Reg| DontSpill]}
  end.

do_movx(I, TempMap, DontSpill) ->
  {FixSrc, Src, DontSpill1} =
    case I of
      #movsx{src=Src0,dst=Dst0} ->
	fix_src_operand(Src0, TempMap);
      #movzx{src=Src0,dst=Dst0} ->
	fix_src_operand(Src0, TempMap)
    end,
  {FixDst, Dst, DontSpill2} = fix_dst_operand(Dst0, TempMap),
  {I3, DontSpill3} =
    case is_spilled(Dst, TempMap) of
      false ->
	I2 = case I of
	       #movsx{} ->
		 [hipe_x86:mk_movsx(Src, Dst)];
	       #movzx{} ->
		 [hipe_x86:mk_movzx(Src, Dst)]
	     end,
	{I2, []};
      true ->
	Dst2 = clone(Dst),
	I2 = 
	  case I of
	    #movsx{} ->
	      [hipe_x86:mk_movsx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)];
	    #movzx{} ->
	      [hipe_x86:mk_movzx(Src, Dst2), hipe_x86:mk_move(Dst2, Dst)]
	  end,
	{I2, [Dst2]}
    end,
  {FixSrc++FixDst++I3, 
   DontSpill3 ++ DontSpill2 ++
   DontSpill1 ++ DontSpill}.

%%% Fix a fmove op.
do_fmove(I, TempMap, DontSpill) ->
  #fmove{src=Src0,dst=Dst0} = I,
  {FixSrc, Src, DontSpill1} = fix_src_operand(Src0, TempMap),
  {FixDst, Dst, DontSpill2} = fix_dst_operand(Dst0, TempMap),
  %% fmoves from memory position to memory position is handled
  %% by the f.p. register allocator.
  {FixSrc ++ FixDst ++ [I#fmove{src=Src,dst=Dst}], 
   DontSpill1 ++ DontSpill2 ++ DontSpill}.


%%% Fix a shift operation
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if the source is a register or memory position
%%% make sure to move it to %ecx

do_shift(I, TempMap, DontSpill) ->
  #shift{src=Src0,dst=Dst0} = I,
  {FixDst, Dst, DontSpill2} = fix_dst_operand(Dst0, TempMap),
  DontSpill3 = DontSpill ++ DontSpill2,
  Reg =  ?HIPE_X86_REGISTERS:?ECX(),
  case Src0 of 
    #x86_imm{} ->
      {FixDst ++ [I#shift{dst=Dst}], DontSpill3};
    #x86_temp{reg=Reg}  ->
      {FixDst ++ [I#shift{dst=Dst}], DontSpill3}
  end.

%%% Fix the operands of a binary op.
%%% 1. remove pseudos from any explicit memory operands
%%% 2. if both operands are (implicit or explicit) memory operands,
%%%    move src to a reg and use reg as src in the original insn

do_binary(Src0, Dst0, TempMap, DontSpill) ->
  {FixSrc, Src, DontSpill1} = fix_src_operand(Src0, TempMap),
  {FixDst, Dst, DontSpill2} = fix_dst_operand(Dst0, TempMap),
  {FixSrc3, Src3, DontSpill3} =
    case is_mem_opnd(Src, TempMap) of
      false ->
	{FixSrc, Src, []};
      true ->
	case is_mem_opnd(Dst, TempMap) of
	  false ->
	    {FixSrc, Src, []};
	  true ->
	    Src2 = clone(Src),
	    FixSrc2 = FixSrc ++ [hipe_x86:mk_move(Src, Src2)],
	    {FixSrc2, Src2, [Src2]}
	end
    end,
  {FixSrc3, Src3, FixDst, Dst, 
   DontSpill3 ++ DontSpill2 ++
   DontSpill1 ++ DontSpill}.

%%% Fix any x86_mem operand to not refer to any spilled temps.

fix_src_operand(Opnd,TmpMap) ->
  fix_mem_operand(Opnd, TmpMap, ?HIPE_X86_REGISTERS:temp1()).

fix_dst_operand(Opnd, TempMap) ->
  fix_mem_operand(Opnd,TempMap, ?HIPE_X86_REGISTERS:temp0()).

fix_mem_operand(Opnd, TempMap, Reg) ->	% -> {[fixupcode], newop, DontSpill}
  case Opnd of
    #x86_mem{base=Base,off=Off} ->
      case is_mem_opnd(Base, TempMap) of
	false ->
	  %% XXX: (Mikael) this test looks wrong to me, since it will
	  %% falsely trigger for temps that are actual registers.
	  %% The naive register allocator uses src_is_pseudo() here.
          %%
	  %% The assembler can't handle reg offsets, so at the moment
          %% it is handled here. (Happi)
	  case hipe_x86:is_imm(Off) of
	    true ->
	      {[], Opnd, []};
	    false ->		% pseudo(pseudo)
	      Temp = clone(Off, Reg),
	      {[hipe_x86:mk_move(Base, Temp),
		hipe_x86:mk_alu('add', Off, Temp)],
	       Opnd#x86_mem{base=Temp, off=hipe_x86:mk_imm(0)},
	       [Temp]}
	  end;
	true ->
	  Temp = clone(Base, Reg),
	  case hipe_x86:is_imm(Off) of
	    true ->		% imm/reg(pseudo)
	      {[hipe_x86:mk_move(Base, Temp)],
	       Opnd#x86_mem{base=Temp},
	       [Temp]};
	    false ->		% pseudo(pseudo)
	      {[hipe_x86:mk_move(Base, Temp),
		hipe_x86:mk_alu('add', Off, Temp)],
	       Opnd#x86_mem{base=Temp, off=hipe_x86:mk_imm(0)},
	       [Temp]}
	  end
      end;
    _ ->
      {[], Opnd, []}
  end.

%%% Check if an operand denotes a memory cell (mem or pseudo).

is_mem_opnd(Opnd, TempMap) ->
  R =
    case Opnd of
      #x86_mem{} -> true;
      #x86_temp{} -> 
	Reg = hipe_x86:temp_reg(Opnd),
	case hipe_x86:temp_is_allocatable(Opnd) of
	  true -> 
	    case size(TempMap) > Reg of 
	      true ->
		case 
		  hipe_temp_map:is_spilled(Reg,
					   TempMap) of
		  true ->
		    ?count_temp(Reg),
		    true;
		  false -> false
		end;
	      _ -> true
	    end;
	  false -> true
	end;
      _ -> false
    end,
  %% io:format("Op ~w mem: ~w\n",[Opnd,R]),
  R.

%%%%% Check if an operand is a spilled Temp.
%%
%%src_is_spilled(Src, TempMap) ->
%%  case hipe_x86:is_temp(Src) of
%%    true ->
%%      Reg = hipe_x86:temp_reg(Src),
%%      case hipe_x86:temp_is_allocatable(Src) of
%%	true -> 
%%	  case size(TempMap) > Reg of 
%%	    true ->
%%	      case hipe_temp_map:is_spilled(Reg, TempMap) of
%%		true ->
%%		  ?count_temp(Reg),
%%		  true;
%%		false ->
%%		  false
%%	      end;
%%	    false ->
%%	      false
%%	  end;
%%	false -> true
%%      end;
%%    false -> false
%%  end.

is_spilled(Temp, TempMap) ->
  case hipe_x86:temp_is_allocatable(Temp) of
    true -> 
      Reg = hipe_x86:temp_reg(Temp),
      case size(TempMap) > Reg of 
	true ->
	  case hipe_temp_map:is_spilled(Reg, TempMap) of
	    true ->
	      ?count_temp(Reg),
	      true;
	    false ->
	      false
	  end;
	false ->
	  false
      end;
    false -> true
  end.

%%% Make Reg a clone of Dst (attach Dst's type to Reg).

clone(Dst) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  spill_temp(Type).

spill_temp(Type) ->
  hipe_x86:mk_temp(?HIPE_X86_REGISTERS:temp1(),Type).

clone(Dst, Reg) ->
  Type =
    case Dst of
      #x86_mem{} -> hipe_x86:mem_type(Dst);
      #x86_temp{} -> hipe_x86:temp_type(Dst)
    end,
  hipe_x86:mk_temp(Reg,Type).
