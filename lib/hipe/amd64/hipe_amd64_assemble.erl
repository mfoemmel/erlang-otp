%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% HiPE/amd64 assembler
%%%
%%% TODO:
%%% - Repair AMD64_SIMULATE_NSP.
%%% - Migrate old resolve_arg users to translate_src/dst.
%%% - Simplify combine_label_maps and mk_data_relocs.
%%% - Move find_const to hipe_pack_constants?

-module(hipe_amd64_assemble).
-export([assemble/4]).

-define(DEBUG,true).

-include("../main/hipe.hrl").
-include("hipe_amd64.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../misc/hipe_sdi.hrl").

assemble(CompiledCode, Closures, Exports, Flags) ->
  put(rex, 0),
  ?when_option(time, Flags, ?start_timer("amd64 assembler")),
  put(hipe_amd64_flags,Flags),
  print("****************** Assembling *******************\n"),
  %%
  Code = [{MFA,
	   hipe_amd64:defun_code(Defun),
	   hipe_amd64:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code, hipe_amd64_registers:alignment()),
  %%
  {CodeSize,AccCode,AccRefs,LabelMap,ExportMap} =
    encode(translate(Code, ConstMap)),
  CodeBinary = mk_code_binary(AccCode),
  print("Total num bytes=~w\n",[CodeSize]),
  put(code_size, CodeSize),
  put(const_size, ConstSize),
  put(rex, get(rex) div 2),  
  ?when_option(verbose, Flags,
	       ?debug_msg("Constants are ~w bytes\n",[ConstSize])),
  %%
  SC = hipe_pack_constants:slim_constmap(ConstMap),
  DataRelocs = mk_data_relocs(RefsFromConsts, LabelMap),
  SSE = slim_sorted_exportmap(ExportMap,Closures,Exports),
  SlimRefs = hipe_pack_constants:slim_refs(AccRefs),
  Bin = term_to_binary([{?VERSION(),?HIPE_SYSTEM_CRC},
			ConstAlign, ConstSize,
			SC,
			DataRelocs, % nee LM, LabelMap
			SSE,
			CodeSize,CodeBinary,SlimRefs,
			0,[] % ColdCodeSize, SlimColdRefs
		       ]),
  %%
  ?when_option(time, Flags, ?stop_timer("amd64 assembler")),
  Bin.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_code_binary(AccCode) ->
  Size = hipe_bifs:array_length(AccCode),
  list_to_binary(array_to_bytes(AccCode, Size, [])).

array_to_bytes(Array, I1, Bytes) ->
  I2 = I1 - 1,
  if I2 < 0 ->
      Bytes;
     true ->
      %% 'band 255' to fix up negative bytes (from disp8 operands?)
      Byte = hipe_bifs:array_sub(Array, I2) band 255,
      array_to_bytes(Array, I2, [Byte|Bytes])
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Assembly Pass 1.
%%% Process initial {MFA,Code,Data} list.
%%% Translate each MFA's body, choosing operand & instruction kinds.
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%% Build LabelMap for each MFA.
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap) ->
  translate_mfas(Code, ConstMap, []).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode) ->
  {NewInsns,CodeSize,LabelMap} =
    translate_insns(Insns, MFA, ConstMap, hipe_sdi:pass1_init(), 0, []),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode]);
translate_mfas([], _ConstMap, NewCode) ->
  lists:reverse(NewCode).

translate_insns([I|Insns], MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewIs = translate_insn(I, MFA, ConstMap),
  add_insns(NewIs, Insns, MFA, ConstMap, SdiPass1, Address, NewInsns);
translate_insns([], _MFA, _ConstMap, SdiPass1, Address, NewInsns) ->
  {LabelMap,CodeSizeIncr} = hipe_sdi:pass2(SdiPass1),
  {lists:reverse(NewInsns), Address+CodeSizeIncr, LabelMap}.

add_insns([I|Is], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewSdiPass1 =
    case I of
      {'.label',L,_} ->
	hipe_sdi:pass1_add_label(SdiPass1, Address, L);
      {jcc_sdi,{_,{label,L}},_} ->
	SdiInfo = #sdi_info{incr=(6-2),lb=(-128)+2,ub=127+2},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      {jmp_sdi,{{label,L}},_} ->
	SdiInfo = #sdi_info{incr=(5-2),lb=(-128)+2,ub=127+2},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      _ ->
	SdiPass1
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewSdiPass1, Address1, [I|NewInsns]);
add_insns([], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  translate_insns(Insns, MFA, ConstMap, SdiPass1, Address, NewInsns).

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.sdesc',_,_} -> 0;
    {jcc_sdi,_,_} -> 2;
    {jmp_sdi,_,_} -> 2;
    {Op,Arg,_Orig} -> hipe_amd64_encode:insn_sizeof(Op, Arg)
  end.

translate_insn(I, MFA, ConstMap) ->
  case I of
    #alu{} ->
      Arg = resolve_alu_args(hipe_amd64:alu_src(I), hipe_amd64:alu_dst(I)),
      [{hipe_amd64:alu_op(I), Arg, I}];
    #call{} ->
      translate_call(I);
    #cmovcc{} ->
      {Dst,Src} = resolve_move_args(
                    hipe_amd64:cmovcc_src(I), hipe_amd64:cmovcc_dst(I),
                    {MFA,ConstMap}),
      CC = {cc,hipe_amd64_encode:cc(hipe_amd64:cmovcc_cc(I))},
      Arg = {CC,Dst,Src},
      [{cmovcc, Arg, I}];
    #cmp{} ->
      Arg = resolve_alu_args(hipe_amd64:cmp_src(I), hipe_amd64:cmp_dst(I)),
      [{cmp, Arg, I}];
    #comment{} ->
      [];
    #dec{} ->
      Arg = translate_dst(hipe_amd64:dec_dst(I)),
      [{dec, {Arg}, I}];
    #fmove{} ->
      Arg = resolve_sse2_binop_args(hipe_amd64:fmove_src(I),
				    hipe_amd64:fmove_dst(I)),
      [{movsd, Arg, I}];
    #fp_binop{} ->
      case proplists:get_bool(x87, get(hipe_amd64_flags)) of
	true -> %% x87	
	  Arg = resolve_x87_binop_args(hipe_amd64:fp_binop_src(I),
				      hipe_amd64:fp_binop_dst(I)),
	  [{hipe_amd64:fp_binop_op(I), Arg, I}];
	false -> %% sse2
	  Arg = resolve_sse2_binop_args(hipe_amd64:fp_binop_src(I),
					hipe_amd64:fp_binop_dst(I)),
	  [{resolve_sse2_op(hipe_amd64:fp_binop_op(I)), Arg, I}]
      end;
    #fp_unop{} ->
      case proplists:get_bool(x87, get(hipe_amd64_flags)) of
	true -> %% x87	
	  Arg = resolve_x87_unop_arg(hipe_amd64:fp_unop_arg(I)),
	  [{hipe_amd64:fp_unop_op(I), Arg, I}];
	false -> %% sse2
	  case hipe_amd64:fp_unop_op(I) of
	    'fchs' ->
	      Arg = resolve_sse2_binop_args(
		      sse2_fnegate_mask,
		      hipe_amd64:fp_unop_arg(I)),
	      [{'xorpd', Arg, I}];
	    'fwait' -> % no op on amd64, magic on x87
	      []
	  end
      end;

    #inc{} ->
      Arg = translate_dst(hipe_amd64:inc_dst(I)),
      [{inc, {Arg}, I}];
    #jcc{} ->
      Cc = {cc,hipe_amd64_encode:cc(hipe_amd64:jcc_cc(I))},
      Label = translate_label(hipe_amd64:jcc_label(I)),
      [{jcc_sdi, {Cc,Label}, I}];
    #jmp_fun{} ->
      %% call and jmp are patched the same, so no need to distinguish
      %% call from tailcall
      PatchTypeExt =
	case hipe_amd64:jmp_fun_linkage(I) of
	  remote -> ?PATCH_TYPE2EXT(call_remote);
	  not_remote -> ?PATCH_TYPE2EXT(call_local)
	end,
      Arg = translate_fun(hipe_amd64:jmp_fun_fun(I), PatchTypeExt),
      [{jmp, {Arg}, I}];
    #jmp_label{} ->
      Arg = translate_label(hipe_amd64:jmp_label_label(I)),
      [{jmp_sdi, {Arg}, I}];
    #jmp_switch{} ->
      RM32 = resolve_jmp_switch_arg(I),
      [{jmp, {RM32}, I}];
    #label{} ->
      [{'.label', hipe_amd64:label_label(I), I}];
    #lea{} ->
      Arg = resolve_lea_args(hipe_amd64:lea_mem(I), hipe_amd64:lea_temp(I)),
      [{lea, Arg, I}];
    #move{} ->
      Arg = resolve_move_args(hipe_amd64:move_src(I), hipe_amd64:move_dst(I),
			      {MFA,ConstMap}),
      [{mov, Arg, I}];
    #move64{} ->
      Arg = resolve_move64_args(hipe_amd64:move64_src(I), hipe_amd64:move64_dst(I),
                                {MFA,ConstMap}),
      [{mov, Arg, I}];
    #movsx{} ->
      Arg = resolve_movx_args(hipe_amd64:movsx_src(I),
                              hipe_amd64:movsx_dst(I)),
      [{movsx, Arg, I}];
    #movzx{} ->
      Arg = resolve_movx_args(hipe_amd64:movzx_src(I),
                              hipe_amd64:movzx_dst(I)),
      [{movzx, Arg, I}];
    %% nop: we shouldn't have any as input
    #prefix_fs{} ->
      [{prefix_fs, {}, I}];
    %% pseudo_call: eliminated before assembly
    %% pseudo_jcc: eliminated before assembly
    %% pseudo_tailcall: eliminated before assembly
    %% pseudo_tailcall_prepare: eliminated before assembly
    #pop{} ->
      Arg = translate_dst(hipe_amd64:pop_dst(I)),
      [{pop, {Arg}, I}];
    #push{} ->
      Arg = translate_src(hipe_amd64:push_src(I), MFA, ConstMap),
      [{push, {Arg}, I}];
    #ret{} ->
      translate_ret(I);
    #shift{} ->
      Arg = resolve_shift_args(hipe_amd64:shift_src(I), hipe_amd64:shift_dst(I)),
      [{hipe_amd64:shift_op(I), Arg, I}];
    #test{} ->
      Arg = resolve_test_args(hipe_amd64:test_src(I), hipe_amd64:test_dst(I)),
      [{test, Arg, I}]
  end.

translate_call(I) ->
  %% call and jmp are patched the same, so no need to distinguish
  %% call from tailcall
  PatchTypeExt =
    case hipe_amd64:call_linkage(I) of
      remote -> ?PATCH_TYPE2EXT(call_remote);
      not_remote -> ?PATCH_TYPE2EXT(call_local)
    end,
  Arg = translate_fun(hipe_amd64:call_fun(I), PatchTypeExt),
  SDesc = hipe_amd64:call_sdesc(I),
  [{call, {Arg}, I}, {'.sdesc', SDesc, #comment{term=sdesc}}].

translate_ret(I) ->
  Arg =
    case hipe_amd64:ret_npop(I) of
      0 -> {};
      N -> {{imm16,N}}
    end,
  [{ret, Arg, I}].

translate_label(Label) when integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

translate_fun(Arg, PatchTypeExt) ->
  case Arg of
    #amd64_temp{} ->
      temp_to_rmArch(Arg);
    #amd64_mem{} ->
      mem_to_rmArch(Arg);
    #amd64_mfa{m=M,f=F,a=A} ->
      {rel32,{PatchTypeExt,{M,F,A}}};
    #amd64_prim{prim=Prim} ->
      {rel32,{PatchTypeExt,Prim}}
  end.

translate_src(Src, MFA, ConstMap) ->
  case Src of
    #amd64_imm{value=Imm} ->
      if is_atom(Imm) ->
	  {imm32,{?PATCH_TYPE2EXT(load_atom),Imm}};
	 is_integer(Imm) ->
	  case (Imm =< 127) and (Imm >= -128) of
	    true ->
	      {imm8,Imm};
	    false ->
	      {imm32,Imm}
	  end;
	 true ->
	  Val =
	    case Imm of
	      {Label, constant} ->
		ConstNo = find_const({MFA,Label}, ConstMap),
		{constant,ConstNo};
	      {Label, closure} ->
		{closure,Label};
	      {Label, c_const} ->
		{c_const,Label}
	    end,
	  {imm32,{?PATCH_TYPE2EXT(load_address),Val}}
      end;
    _ ->
      translate_dst(Src)
  end.

translate_dst(Dst) ->
  case Dst of
    #amd64_temp{} ->
      temp_to_regArch(Dst);
    #amd64_mem{type='double'} ->
      mem_to_rm64fp(Dst);
    #amd64_mem{} ->
      mem_to_rmArch(Dst);
    #amd64_fpreg{} ->
      fpreg_to_stack(Dst)
  end.

%%%
%%% Assembly Pass 3.
%%% Process final {MFA,Code,CodeSize,LabelMap} list from pass 2.
%%% Translate to a single binary code segment.
%%% Collect relocation patches.
%%% Build ExportMap (MFA-to-address mapping).
%%% Combine LabelMaps to a single one (for mk_data_relocs/2 compatibility).
%%% Return {CombinedCodeSize,BinaryCode,Relocs,CombinedLabelMap,ExportMap}.
%%%

-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

encode(Code) ->
  CodeSize = compute_code_size(Code, 0),
  ExportMap = build_export_map(Code, 0, []),
  CodeArray = hipe_bifs:array(CodeSize, 0), % XXX: intarray, should have bytearray support!
  Relocs = encode_mfas(Code, 0, CodeArray, []), % updates CodeArray via side-effects
  CombinedLabelMap = combine_label_maps(Code, 0, gb_trees:empty()),
  {CodeSize,CodeArray,Relocs,CombinedLabelMap,ExportMap}.

nr_pad_bytes(Address) -> (4 - (Address rem 4)) rem 4. % XXX: 16 or 32 instead?

align_entry(Address) -> Address + nr_pad_bytes(Address).

compute_code_size([{_MFA,_Insns,CodeSize,_LabelMap}|Code], Size) ->
  compute_code_size(Code, align_entry(Size+CodeSize));
compute_code_size([], Size) -> Size.

build_export_map([{{M,F,A},_Insns,CodeSize,_LabelMap}|Code], Address, ExportMap) ->
  build_export_map(Code, align_entry(Address+CodeSize), [{Address,M,F,A}|ExportMap]);
build_export_map([], _Address, ExportMap) -> ExportMap.

combine_label_maps([{MFA,_Insns,CodeSize,LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, align_entry(Address+CodeSize), NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

encode_mfas([{MFA,Insns,CodeSize,LabelMap}|Code], Address, CodeArray, Relocs) ->
  print("Generating code for:~w\n", [MFA]),
  print("Offset   | Opcode (hex)             | Instruction\n"),
  {Address1,Relocs1} = encode_insns(Insns, Address, Address, LabelMap, Relocs, CodeArray),
  ExpectedAddress = align_entry(Address + CodeSize),
  ?ASSERT(Address1 =:= ExpectedAddress),
  print("Finished.\n\n"),
  encode_mfas(Code, Address1, CodeArray, Relocs1);
encode_mfas([], _Address, _CodeArray, Relocs) -> Relocs.

encode_insns([I|Insns], Address, FunAddress, LabelMap, Relocs, CodeArray) ->
  case I of
    {'.label',L,_} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ?ASSERT(Address =:= LabelAddress),	% sanity check
      print_insn(Address, [], I),
      encode_insns(Insns, Address, FunAddress, LabelMap, Relocs, CodeArray);
    {'.sdesc',SDesc,_} ->
      #amd64_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      Reloc = {?PATCH_TYPE2EXT(sdesc),Address,
	       ?STACK_DESC(ExnRA, FSize, Arity, Live)},
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], CodeArray);
    _ ->
      {Op,Arg,_} = fix_jumps(I, Address, FunAddress, LabelMap),
      {Bytes, NewRelocs} = hipe_amd64_encode:insn_encode(Op, Arg, Address),
      Size = length(Bytes),		    
      print_insn(Address, Bytes, I),
      list_to_array(Bytes, CodeArray, Address),
      encode_insns(Insns, Address+Size, FunAddress, LabelMap, NewRelocs++Relocs, CodeArray)
  end;
encode_insns([], Address, FunAddress, LabelMap, Relocs, CodeArray) ->
  case nr_pad_bytes(Address) of
    0 ->
      {Address,Relocs};
    NrPadBytes ->	% triggers at most once per function body
      Padding = lists:duplicate(NrPadBytes, {nop,{},#comment{term=padding}}),
      encode_insns(Padding, Address, FunAddress, LabelMap, Relocs, CodeArray)
  end.

fix_jumps(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {jcc_sdi,{CC,{label,L}},OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ShortOffset = LabelAddress - (InsnAddress + 2),
      if ShortOffset >= -128, ShortOffset =< 127 ->
	  {jcc,{CC,{rel8,ShortOffset}},OrigI};
	 true ->
	  LongOffset = LabelAddress - (InsnAddress + 6),
	  {jcc,{CC,{rel32,LongOffset}},OrigI}
      end;
    {jmp_sdi,{{label,L}},OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ShortOffset = LabelAddress - (InsnAddress + 2),
      if ShortOffset >= -128, ShortOffset =< 127 ->
	  {jmp,{{rel8,ShortOffset}},OrigI};
	 true ->
	  LongOffset = LabelAddress - (InsnAddress + 5),
	  {jmp,{{rel32,LongOffset}},OrigI}
      end;
    _ -> I
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(AMD64_SIMULATE_NSP).

call_encode(I, MFA, Addr, Map, ConstMap) ->
  RegSP = hipe_amd64_encode:esp(),
  TempSP = hipe_amd64:mk_temp(RegSP, untagged),
  I0 = {sub, {temp_to_rmArch(TempSP), {imm8,4}}},
  Size0 = hipe_amd64_encode:insn_sizeof(I0),
  I1 = {mov, {mem_to_rmArch(hipe_amd64:mk_mem(TempSP,
					  hipe_amd64:mk_imm(0),
					  untagged)),
	      {imm32,0}}},
  Size1 = hipe_amd64_encode:insn_sizeof(I1),
  FunOrig = hipe_amd64:call_fun(I),
  Fun =
    case FunOrig of
      #amd64_mem{base=#amd64_temp{reg=4}, off=#amd64_imm{value=Off}} ->
	FunOrig#amd64_mem{off=#amd64_imm{value=Off+4}};
      _ -> FunOrig
    end,
  {Arg,Refs2} = resolve_jmp_arg(Fun, MFA, Addr+Size0+Size1, Map, 0, undefined),
  I2 = {jmp, {Arg}},
  Size2 = hipe_amd64_encode:insn_sizeof(I2),
  Ref1 = {?PATCH_TYPE2EXT(amd64_abs_pcrel), Addr+Size0+Size1-4, 4+Size2},
  {[I0,I1,I2],[Ref1|Refs2]}.

ret_encode(I) ->
  NPOP = 4 + hipe_amd64:ret_npop(I),
  RegSP = hipe_amd64_encode:esp(),
  TempSP = hipe_amd64:mk_temp(RegSP, untagged),
  RegRA = hipe_amd64_encode:ebx(),
  TempRA = hipe_amd64:mk_temp(RegRA, untagged),
  I1 = {mov, {temp_to_regArch(TempRA),
	      mem_to_rmArch(hipe_amd64:mk_mem(TempSP,
                                              hipe_amd64:mk_imm(0),
                                              untagged))}},
  I2 = {add, {temp_to_rmArch(TempSP),
	      case NPOP < 128 of
		true -> {imm8,NPOP};
		false -> {imm32,NPOP}
	      end}},
  I3 = {jmp, {temp_to_rmArch(TempRA)}},
  [I1,I2,I3].

-else.	% AMD64_SIMULATE_NSP

-endif.	% AMD64_SIMULATE_NSP

list_to_array(List, Array, Addr) ->
  lists:foldl(fun(X,I) -> hipe_bifs:array_update(Array,I,X), I+1 end, Addr, List).

fpreg_to_stack(#amd64_fpreg{reg=Reg}) ->
  {fpst, Reg}.

temp_to_regArch(#amd64_temp{reg=Reg}) ->
  %% Macro here:
  {reg64, Reg}.
temp_to_reg64(#amd64_temp{reg=Reg}) ->
  {reg64, Reg}.
temp_to_reg32(#amd64_temp{reg=Reg}) ->
  {reg32, Reg}.
temp_to_reg16(#amd64_temp{reg=Reg}) ->
  {reg16, Reg}.
temp_to_reg8(#amd64_temp{reg=Reg}) ->
  {reg8, Reg}.

temp_to_xmm(#amd64_temp{reg=Reg}) ->
  {xmm, Reg}. 

%%% temp_to_rm32(#amd64_temp{reg=Reg}) ->
%%%   {rm32, hipe_amd64_encode:rm_reg(Reg)}.
temp_to_rm64(#amd64_temp{reg=Reg}) ->
  {rm64, hipe_amd64_encode:rm_reg(Reg)}.
temp_to_rmArch(#amd64_temp{reg=Reg}) ->
  %% Macro here:
  {rm64, hipe_amd64_encode:rm_reg(Reg)}.
temp_to_rm64fp(#amd64_temp{reg=Reg}) ->
  {rm64fp, hipe_amd64_encode:rm_reg(Reg)}.

mem_to_ea(Mem) ->
  EA = mem_to_ea_common(Mem),
  {ea, EA}.

mem_to_rm32(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm32, hipe_amd64_encode:rm_mem(EA)}.

mem_to_rmArch(Mem) ->
  %% Macro here:
  EA = mem_to_ea_common(Mem),
  {rm64, hipe_amd64_encode:rm_mem(EA)}.

mem_to_rm64fp(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm64fp, hipe_amd64_encode:rm_mem(EA)}.

%%%%%%%%%%%%%%%%%
mem_to_rm8(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm8, hipe_amd64_encode:rm_mem(EA)}.

mem_to_rm16(Mem) ->
  EA = mem_to_ea_common(Mem),
  {rm16, hipe_amd64_encode:rm_mem(EA)}.
%%%%%%%%%%%%%%%%%

mem_to_ea_common(#amd64_mem{base=[], off=#amd64_imm{value=Off}}) ->
  hipe_amd64_encode:ea_disp32(Off);
mem_to_ea_common(#amd64_mem{base=#amd64_temp{reg=Base}, off=#amd64_imm{value=Off}}) ->
  if
    Off =:= 0 ->
      case Base of
	4 -> %esp, use SIB w/o disp8
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_sib(SIB);
	5 -> %ebp, use disp8 w/o SIB
	  hipe_amd64_encode:ea_disp8_base(Off, Base);
        12 -> %r12, use SIB w/o disp8
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_sib(SIB);
        13 -> %r13, use disp8 w/o SIB
 	  hipe_amd64_encode:ea_disp8_base(Off, Base);         
	_ -> %neither SIB nor disp8 needed
	  hipe_amd64_encode:ea_base(Base)
      end;
    Off >= -128, Off =< 127 ->
      case Base of
	4 -> %esp, must use SIB
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_disp8_sib(Off, SIB);
        12 -> %r12, must use SIB
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_disp8_sib(Off, SIB);
	_ -> %use disp8 w/o SIB
	  hipe_amd64_encode:ea_disp8_base(Off, Base)
      end;
    true ->
      case Base of
	4 -> %esp, must use SIB
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_disp32_sib(Off, SIB);
	12 -> %r12, must use SIB
	  SIB = hipe_amd64_encode:sib(Base),
	  hipe_amd64_encode:ea_disp32_sib(Off, SIB);
	_ ->
	  hipe_amd64_encode:ea_disp32_base(Off, Base)
      end
  end.

%%% jmp_switch
resolve_jmp_switch_arg(I) ->
  Base = hipe_amd64:temp_reg(hipe_amd64:jmp_switch_jtab(I)),
  %% Replace this with something else when register allocation works
  case Base of
    13 -> 
      exit({amd64_assemble, resolve_jmp_switch_arg,
            not_possible_to_have_r13_as_base, fix_register_allocator});
    5 -> 
      exit({amd64_assemble, resolve_jmp_switch_arg,
            not_possible_to_have_rbp_as_base, fix_register_allocator});
    _ ->
      ok  
  end,
  SINDEX = hipe_amd64_encode:sindex(
             3, hipe_amd64:temp_reg(hipe_amd64:jmp_switch_temp(I))),
  SIB = hipe_amd64_encode:sib(Base, SINDEX),
  EA  = hipe_amd64_encode:ea_sib(SIB),
  {rm64,hipe_amd64_encode:rm_mem(EA)}.

%% lea reg, mem
resolve_lea_args(Src=#amd64_mem{}, Dst=#amd64_temp{}) ->
  {temp_to_regArch(Dst),mem_to_ea(Src)}.

resolve_sse2_op(Op) ->
  case Op of
    fadd -> addsd;
    fdiv -> divsd;
    fmul -> mulsd;
    fsub -> subsd;
    _ -> exit({?MODULE, unknown_sse2_operator, Op})
  end.

%% OP xmm, mem
resolve_sse2_binop_args(Src=#amd64_mem{type=double},
			Dst=#amd64_temp{type=double}) ->
  {temp_to_xmm(Dst),mem_to_rm64fp(Src)};

%% movsd mem, xmm
resolve_sse2_binop_args(Src=#amd64_temp{type=double},
			Dst=#amd64_mem{type=double}) ->
  {mem_to_rm64fp(Dst),temp_to_xmm(Src)};

%% OP xmm, xmm
resolve_sse2_binop_args(Src=#amd64_temp{type=double},
			Dst=#amd64_temp{type=double}) ->
  {temp_to_xmm(Dst),temp_to_rm64fp(Src)};

%% cvtsi2sd xmm, reg
resolve_sse2_binop_args(Src=#amd64_temp{type=untagged},
			Dst=#amd64_temp{type=double}) ->
  {temp_to_xmm(Dst),temp_to_rm64(Src)};

%% xorpd xmm, mem
resolve_sse2_binop_args(sse2_fnegate_mask,
			Dst=#amd64_temp{type=double}) ->
  {temp_to_xmm(Dst),
   {rm64fp, {rm_mem, hipe_amd64_encode:ea_disp32_sindex(
		       {?PATCH_TYPE2EXT(load_address),
			{c_const, sse2_fnegate_mask}})}}}.

%% mov mem, imm
resolve_move_args(#amd64_imm{value=ImmSrc}, Dst=#amd64_mem{type=Type}, Context) ->
  case Type of   % to support byte, int16 and int32 stores
    byte ->
      ByteImm = ImmSrc band 255, %to ensure that it is a bytesized imm
      {mem_to_rm8(Dst),{imm8,ByteImm}};
    %% FIXME: small function for all of these
    int16 ->
      RMArch = mem_to_rmArch(Dst),
      {_,Imm} = resolve_arg(#amd64_imm{value=ImmSrc}, Context),
      {RMArch,{imm32,Imm}};
    int32 ->
      RMArch = mem_to_rmArch(Dst),
      {_,Imm} = resolve_arg(#amd64_imm{value=ImmSrc}, Context),
      {RMArch,{imm32,Imm}};
    tagged ->
      RMArch = mem_to_rmArch(Dst),
      {_,Imm} = resolve_arg(#amd64_imm{value=ImmSrc}, Context),
      {RMArch,{imm32,Imm}};
    untagged ->
      RMArch = mem_to_rmArch(Dst),
      {_,Imm} = resolve_arg(#amd64_imm{value=ImmSrc}, Context),
      {RMArch,{imm32,Imm}}
  end;

%% mov reg,mem
resolve_move_args(Src=#amd64_mem{}, Dst=#amd64_temp{}, _Context) ->
  {temp_to_regArch(Dst),mem_to_rmArch(Src)};

%% mov mem,reg
resolve_move_args(Src=#amd64_temp{}, Dst=#amd64_mem{type=Type}, _Context) ->
  case Type of   % to support byte, int16 and int32 stores
    byte ->
      {mem_to_rm8(Dst),temp_to_reg8(Src)};
    int16 ->
      {mem_to_rm16(Dst),temp_to_reg16(Src)};
    int32 ->
      {mem_to_rm32(Dst),temp_to_reg32(Src)};
    tagged -> % tagged, untagged
      {mem_to_rmArch(Dst),temp_to_regArch(Src)};
    untagged -> % tagged, untagged
      {mem_to_rmArch(Dst),temp_to_regArch(Src)}
  end;

%% mov reg,reg
resolve_move_args(Src=#amd64_temp{}, Dst=#amd64_temp{}, _Context) ->
  {temp_to_regArch(Dst),temp_to_rmArch(Src)};

%% mov reg,imm
resolve_move_args(Src=#amd64_imm{value=_ImmSrc}, Dst=#amd64_temp{}, Context) ->
  {_,Imm} = resolve_arg(Src, Context),
  %% temp_to_reg32 on x86; rm64 gives sign extension on amd64
  if is_number(Imm), Imm >= 0 -> {temp_to_reg32(Dst),{imm32,Imm}};      
     true -> {temp_to_rm64(Dst),{imm32,Imm}}
  end.

%% mov reg,imm64
resolve_move64_args(Src=#amd64_imm{}, Dst=#amd64_temp{}, Context) ->
  {_,Imm} = resolve_arg(Src, Context),
  {temp_to_reg64(Dst),{imm64,Imm}}.

%%% mov{s,z}x
resolve_movx_args(Src=#amd64_mem{type=Type}, Dst=#amd64_temp{}) ->
  {temp_to_regArch(Dst),
   case Type of
     byte ->
       mem_to_rm8(Src);
     int16 ->
       mem_to_rm16(Src);
     int32 ->
       mem_to_rm32(Src)
   end}.

%%% alu/cmp (_not_ test)
resolve_alu_args(Src, Dst) ->
  case {Src,Dst} of
    {#amd64_imm{}, #amd64_mem{}} ->
      {mem_to_rmArch(Dst), resolve_arg(Src, [])};
    {#amd64_mem{}, #amd64_temp{}} ->
      {temp_to_regArch(Dst), mem_to_rmArch(Src)};
    {#amd64_temp{}, #amd64_mem{}} ->
      {mem_to_rmArch(Dst), temp_to_regArch(Src)};
    {#amd64_temp{}, #amd64_temp{}} ->
      {temp_to_regArch(Dst), temp_to_rmArch(Src)};
    {#amd64_imm{}, #amd64_temp{reg=0}} -> % eax,imm
      NewSrc = resolve_arg(Src, []),
      NewDst =
	case NewSrc of
	  {imm8,_} -> temp_to_rmArch(Dst);
	  {imm32,_} -> rax %% XXX: fixme for x86
	end,
      {NewDst, NewSrc};
    {#amd64_imm{}, #amd64_temp{}} ->
      {temp_to_rmArch(Dst), resolve_arg(Src, [])}
  end.

%%% test
resolve_test_args(Src, Dst) ->
  case Src of
    #amd64_imm{} -> % imm8 not allowed
      {_ImmSize,ImmValue} = resolve_arg(Src, []),
      NewDst =
	case Dst of
	  #amd64_temp{reg=0} -> rax;
	  #amd64_temp{} -> temp_to_rmArch(Dst);
	  #amd64_mem{} -> mem_to_rmArch(Dst)
	end,
      {NewDst, {imm32,ImmValue}};
    #amd64_temp{} ->
      NewDst =
	case Dst of
	  #amd64_temp{} -> temp_to_rmArch(Dst);
	  #amd64_mem{} -> mem_to_rmArch(Dst)
	end,
      {NewDst, temp_to_regArch(Src)}
  end.

%%% shifts
resolve_shift_args(Src, Dst) ->
  RM32 =
    case Dst of
      #amd64_temp{} -> temp_to_rmArch(Dst);
      #amd64_mem{} -> mem_to_rmArch(Dst)
    end,
  Count =
    case Src of
      #amd64_imm{value=1} -> 1;
      #amd64_imm{} -> resolve_arg(Src, []); % must be imm8
      #amd64_temp{reg=1} -> cl	% temp must be ecx
    end,
  {RM32, Count}.

%% fp_binop mem
resolve_x87_unop_arg(Arg=#amd64_mem{type=Type})->
  case Type of
    'double' -> {mem_to_rm64fp(Arg)};
    'untagged' -> {mem_to_rmArch(Arg)};
    _ -> ?EXIT({fmovArgNotSupported,{Arg}})
  end;
resolve_x87_unop_arg(Arg=#amd64_fpreg{}) ->
  {fpreg_to_stack(Arg)};
resolve_x87_unop_arg([]) ->
  [].

%% fp_binop mem, st(i)
resolve_x87_binop_args(Src=#amd64_fpreg{}, Dst=#amd64_mem{})->
  {mem_to_rm64fp(Dst),fpreg_to_stack(Src)};
%% fp_binop st(0), st(i)
resolve_x87_binop_args(Src=#amd64_fpreg{}, Dst=#amd64_fpreg{})->
  {fpreg_to_stack(Dst),fpreg_to_stack(Src)}.

%% return arg for encoding
%% Context=[] when no relocs are expected, {MFA,ConstMap} otherwise
resolve_arg(Arg, Context) ->
  case Arg of
    {reg32,_Reg32} ->
      Arg;
    {reg64,_Reg64} ->
      Arg;
    #amd64_imm{value=Imm} ->
      if is_atom(Imm) ->
	  %%print("Atom:~w added to patchlist at addr:~w - ",[Imm,Addr+BytesToImm32]),
	  {imm32,{?PATCH_TYPE2EXT(load_atom),Imm}};
	 is_integer(Imm) ->
	  case (Imm =< 127) and (Imm >= -128) of
	    true ->
	      {imm8,Imm};
	    false ->
	      {imm32,Imm}
	  end;
	 true ->
	  %% Ok, this is the case when {label,type} (type=label,catch,constant)
	  case Context of
	    [] ->
	      {imm32,0};
	    {MFA,ConstMap} ->
	      Val =
		case Imm of
		  {ConstLab, constant} ->
		    ConstNo = find_const({MFA, ConstLab}, ConstMap),
		    {constant,ConstNo};
                  {ClosureLab, closure} ->
		    {closure, ClosureLab};
		  {C_constLab, c_const} ->
		    {c_const, C_constLab};
		  Other ->
		    ?EXIT({unsupportedimm32value,Other})
		end,
	      {imm32,{?PATCH_TYPE2EXT(load_address),Val}}
	  end
      end;
    #amd64_temp{} ->
      temp_to_regArch(Arg);
    %% Push uses this, and goes via ESP so the SIB byte stays...
    #amd64_mem{type=Type} ->
      case Type of
	'double'-> mem_to_rm64fp(Arg);
	_ -> mem_to_rmArch(Arg)
      end;
    #amd64_fpreg{} ->
      fpreg_to_stack(Arg)
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_data_relocs(RefsFromConsts, LabelMap) ->
  lists:flatten(mk_data_relocs(RefsFromConsts, LabelMap, [])).

mk_data_relocs([{MFA,Labels} | Rest], LabelMap, Acc) ->
  Map = [case Label of
	   {L,Pos} ->
	     Offset = find({MFA,L}, LabelMap),
	     {Pos,Offset};
	   {sorted,Base,OrderedLabels} ->
	     {sorted, Base, [begin
			       Offset = find({MFA,L}, LabelMap),
			       {Order, Offset}
			     end
			     || {L,Order} <- OrderedLabels]}
	 end
	 || Label <- Labels],
  %% msg("Map: ~w Map\n",[Map]),
  mk_data_relocs(Rest, LabelMap, [Map,Acc]);
mk_data_relocs([],_,Acc) -> Acc.

find({MFA,L},LabelMap) ->
  gb_trees:get({MFA,L}, LabelMap).

slim_sorted_exportmap([{Addr,M,F,A}|Rest], Closures, Exports) ->
  IsClosure = lists:member({M,F,A}, Closures),
  IsExported = is_exported(F, A, Exports),
  [Addr,M,F,A,IsClosure,IsExported | slim_sorted_exportmap(Rest, Closures, Exports)];
slim_sorted_exportmap([],_,_) -> [].

is_exported(_F, _A, []) -> true; % XXX: kill this clause when Core is fixed
is_exported(F, A, Exports) -> lists:member({F,A}, Exports).

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String) ->
  Flags = get(hipe_amd64_flags),
  ?when_option(pp_asm, Flags,io:format(String,[])).

print(String, Arglist) ->
  Flags = get(hipe_amd64_flags),
  ?when_option(pp_asm, Flags,io:format(String,Arglist)).

print_insn(Address, Bytes, I) ->
  Flags = get(hipe_amd64_flags),
  ?when_option(pp_asm, Flags, print_insn_2(Address, Bytes, I)),
  ?when_option(pp_cxmon, Flags, print_code_list_2(Bytes)).

print_code_list_2([H | Tail]) ->
  print_byte(H),
  io:format(","),
  print_code_list_2(Tail);
print_code_list_2([]) ->
  io:format("").

print_insn_2(Address, Bytes, {_,_,OrigI}) ->
  print("~8.16b | ",[Address]),
  print_code_list(Bytes, 0),
  hipe_amd64_pp:pp_insn(OrigI).

print_code_list([Byte|Rest], Len) ->
  print_byte(Byte),
  print_code_list(Rest, Len+1);
print_code_list([], Len) ->
  fill_spaces(24-(Len*2)),
  io:format(" | ").

print_byte(Byte) ->
  io:format("~2.16.0b", [Byte band 16#FF]).

fill_spaces(N) when N > 0 ->
  io:format(" "),
  fill_spaces(N-1);
fill_spaces(_) ->
  [].

%%%
%%% Lookup a constant in a ConstMap.
%%%

find_const({MFA,Label},[{pcm_entry,MFA,Label,ConstNo,_,_,_}|_]) ->
  ConstNo;
find_const(N,[_|R]) ->
  find_const(N,R);
find_const(C,[]) ->
  ?EXIT({constant_not_found,C}).
