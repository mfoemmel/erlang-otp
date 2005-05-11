%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_amd64_assemble.erl,v 1.38 2005/01/26 14:14:27 mikpe Exp $

-define(HIPE_X86_ASSEMBLE,  hipe_amd64_assemble).
-define(HIPE_X86_ENCODE,    hipe_amd64_encode).
-define(HIPE_X86_REGISTERS, hipe_amd64_registers).
-define(HIPE_X86_PP,        hipe_amd64_pp).
-ifdef(AMD64_SIMULATE_NSP).
-define(X86_SIMULATE_NSP, ?AMD64_SIMULATE_NSP).
-endif.
-define(EAX, rax).
-define(REGArch, reg64).
-define(RMArch, rm64).
-define(EA_DISP32_ABSOLUTE, ea_disp32_sindex).
-define(IMM_MOVE_ARGS, if is_number(Imm), Imm >= 0 ->
			   {temp_to_reg32(Dst),{imm32,Imm}};      
			  true -> {temp_to_rm64(Dst),{imm32,Imm}}
		       end).
-define(REG64, {reg64, _Reg64} -> Arg).

-define(MOVE64, #move64{} ->
	   Arg = resolve_move64_args(hipe_x86:move64_src(I),
				     hipe_x86:move64_dst(I),
				     {MFA,ConstMap}),
	   [{mov, Arg, I}]).

-define(RESOLVE_MOVE64_ARGS, %% mov reg,imm64
	resolve_move64_args(Src=#x86_imm{}, Dst=#x86_temp{}, Context) ->
	   {_,Imm} = resolve_arg(Src, Context),
	   {temp_to_reg64(Dst),{imm64,Imm}}).

-define(TEMP_TO_REG64, temp_to_reg64(#x86_temp{reg=Reg}) ->
	   {reg64, Reg}).

-define(TEMP_TO_RM64, temp_to_rm64(#x86_temp{reg=Reg}) ->
	   {rm64, hipe_amd64_encode:rm_reg(Reg)}).

-define(RESOLVE_JMP_SWITCH_ARG,
	resolve_jmp_switch_arg(I, _) ->
	   Base = hipe_x86:temp_reg(hipe_x86:jmp_switch_jtab(I)),
	   %% FIXME: We are only saved by a fluke
	   case Base of
	     13 -> 
	       exit({?MODULE, internal_compiler_error, r13});
	     5 -> 
	       exit({?MODULE, internal_compiler_error, r5});
	     _ ->
	       ok  
	   end,
	   SINDEX = hipe_amd64_encode:sindex(
		      3, hipe_x86:temp_reg(hipe_x86:jmp_switch_temp(I))),
	   SIB = hipe_amd64_encode:sib(Base, SINDEX),
	   EA  = hipe_amd64_encode:ea_sib(SIB),
	   {rm64,hipe_amd64_encode:rm_mem(EA)}).

-ifdef(AMD64_SIMULATE_NSP).
-define(TRANSLATE_CALL, %% Only used for simulate_nsp
%% FIXME: merge this better
translate_call(I) ->
  WordSize = hipe_amd64_registers:wordsize(),
  RegSP = 2#100, % esp/rsp
  TempSP = hipe_x86:mk_temp(RegSP, untagged),
  FunOrig = hipe_x86:call_fun(I),
  Fun =
    case FunOrig of
      #x86_mem{base=#x86_temp{reg=4}, off=#x86_imm{value=Off}} ->
	FunOrig#x86_mem{off=#x86_imm{value=Off+WordSize}};
      _ -> FunOrig
    end,
  RegRA =
    begin
      RegTemp0 = hipe_amd64_registers:temp0(),
      RegTemp1 = hipe_amd64_registers:temp1(),
      case Fun of
	#x86_temp{reg=RegTemp0} -> RegTemp1;
	#x86_mem{base=#x86_temp{reg=RegTemp0}} -> RegTemp1;
	_ -> RegTemp0
      end
    end,
  TempRA = hipe_x86:mk_temp(RegRA, untagged),
  PatchTypeExt =
    case hipe_x86:call_linkage(I) of
      remote -> ?PATCH_TYPE2EXT(call_remote);
      not_remote -> ?PATCH_TYPE2EXT(call_local)
    end,
  JmpArg = translate_fun(Fun, PatchTypeExt),
  I4 = {'.sdesc', hipe_x86:call_sdesc(I), #comment{term=sdesc}},
  I3 = {jmp, {JmpArg}, #comment{term=call}},
  Size3 = hipe_amd64_encode:insn_sizeof(jmp, {JmpArg}),
  MovArgs = {mem_to_rmArch(hipe_x86:mk_mem(TempSP,
					     hipe_x86:mk_imm(0),
					     untagged)),
	     temp_to_regArch(TempRA)},
  I2 = {mov, MovArgs, #comment{term=call}},
  Size2 = hipe_amd64_encode:insn_sizeof(mov, MovArgs),
  I1 = {lea, {temp_to_regArch(TempRA),
	      {ea, hipe_amd64_encode:ea_disp32_rip(Size2+Size3)}},
	#comment{term=call}},
  I0 = {sub, {temp_to_rmArch(TempSP), {imm8,WordSize}}, I},
  [I0,I1,I2,I3,I4]).
-endif.

-include("../x86/hipe_x86_assemble.erl").
