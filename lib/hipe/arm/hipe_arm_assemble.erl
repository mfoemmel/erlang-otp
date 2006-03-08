%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%

-module(hipe_arm_assemble).
-export([assemble/4]).

-include("../main/hipe.hrl").	% for VERSION_STRING, when_option
-include("hipe_arm.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

assemble(CompiledCode, Closures, Exports, Options) ->
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_arm:defun_code(Defun),
	   hipe_arm:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code, 4),
  %%
  {CodeSize,CodeBinary,AccRefs,LabelMap,ExportMap} =
    encode(translate(Code, ConstMap), Options),
  print("Total num bytes=~w\n", [CodeSize], Options),
  %%
  SC = hipe_pack_constants:slim_constmap(ConstMap),
  DataRelocs = mk_data_relocs(RefsFromConsts, LabelMap),
  SSE = slim_sorted_exportmap(ExportMap,Closures,Exports),
  SlimRefs = hipe_pack_constants:slim_refs(AccRefs),
  Bin = term_to_binary([{?VERSION_STRING(),?HIPE_SYSTEM_CRC},
			ConstAlign, ConstSize,
			SC,
			DataRelocs, % nee LM, LabelMap
			SSE,
			CodeSize,CodeBinary,SlimRefs,
			0,[] % ColdCodeSize, SlimColdRefs
		       ]),
  %%
  Bin.

%%%
%%% Assembly Pass 1.
%%% Process initial {MFA,Code,Data} list.
%%% Translate each MFA's body, choosing operand & instruction kinds.
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%% (Trivial on ARM.)
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap) ->
  translate_mfas(Code, ConstMap, []).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode) ->
  {NewInsns,CodeSize,LabelMap} =
    translate_insns(Insns, MFA, ConstMap, gb_trees:empty(), 0, []),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode]);
translate_mfas([], _ConstMap, NewCode) ->
  lists:reverse(NewCode).

translate_insns([I|Insns], MFA, ConstMap, LabelMap, Address, NewInsns) ->
  NewIs = translate_insn(I, MFA, ConstMap),
  add_insns(NewIs, Insns, MFA, ConstMap, LabelMap, Address, NewInsns);
translate_insns([], _MFA, _ConstMap, LabelMap, Address, NewInsns) ->
  {lists:reverse(NewInsns), Address, LabelMap}.

add_insns([I|Is], Insns, MFA, ConstMap, LabelMap, Address, NewInsns) ->
  NewLabelMap =
    case I of
      {'.label',L,_} ->
	gb_trees:insert(L, Address, LabelMap);
      _ ->
	LabelMap
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewLabelMap, Address1, [I|NewInsns]);
add_insns([], Insns, MFA, ConstMap, LabelMap, Address, NewInsns) ->
  translate_insns(Insns, MFA, ConstMap, LabelMap, Address, NewInsns).

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.reloc',_,_} -> 0;
    _ -> 4
  end.

translate_insn(I, MFA, ConstMap) ->	% -> [{Op,Opnd,OrigI}]
  case I of
    #alu{} -> do_alu(I);
    #b_fun{} -> do_b_fun(I);
    #b_label{} -> do_b_label(I);
    #bl{} -> do_bl(I);
    #blx{} -> do_blx(I);
    #cmp{} -> do_cmp(I);
    #comment{} -> [];
    #label{} -> do_label(I);
    #load{} -> do_load(I);
    #ldrsb{} -> do_ldrsb(I);
    #move{} -> do_move(I);
    %% pseudo_b: eliminated by finalise
    %% pseudo_blr: eliminated by finalise
    %% pseudo_call: eliminated by finalise
    %% pseudo_call_prepare: eliminated by frame
    #pseudo_li{} -> do_pseudo_li(I, MFA, ConstMap);
    %% pseudo_move: eliminated by frame
    %% pseudo_switch: eliminated by finalise
    %% pseudo_tailcall: eliminated by frame
    %% pseudo_tailcall_prepare: eliminated by finalise
    #store{} -> do_store(I);
    _ -> exit({?MODULE,translate_insn,I})
  end.

do_alu(I) ->
  #alu{aluop=AluOp,s=S,dst=Dst,src=Src,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewS = do_s(S),
  NewDst = do_reg(Dst),
  NewSrc = do_reg(Src),
  NewAm1 = do_am1(Am1),
  {NewI,NewOpnds} = {AluOp, {NewCond,NewS,NewDst,NewSrc,NewAm1}},
  [{NewI, NewOpnds, I}].

do_b_fun(I) ->
  #b_fun{'fun'=Fun,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {b, {do_cond('al'),{imm24,0}}, I}].

do_b_label(I) ->
  #b_label{'cond'=Cond,label=Label} = I,
  [{b, {do_cond(Cond),do_label_ref(Label)}, I}].

do_bl(I) ->
  #bl{'fun'=Fun,sdesc=SDesc,linkage=Linkage} = I,
  [{'.reloc', {b_fun,Fun,Linkage}, #comment{term='fun'}},
   {bl, {do_cond('al'),{imm24,0}}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_blx(I) ->
  #blx{src=Src,sdesc=SDesc} = I,
  [{blx, {do_cond('al'),do_reg(Src)}, I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_cmp(I) ->
  #cmp{cmpop=CmpOp,src=Src,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewSrc = do_reg(Src),
  NewAm1 = do_am1(Am1),
  [{CmpOp, {NewCond,NewSrc,NewAm1}, I}].

do_label(I) ->
  #label{label=Label} = I,
  [{'.label', Label, I}].

do_load(I) ->
  #load{ldop=LdOp,dst=Dst,am2=Am2} = I,
  NewCond = do_cond('al'),
  NewDst = do_reg(Dst),
  NewAm2 = do_am2(Am2),
  [{LdOp, {NewCond,NewDst,NewAm2}, I}].

do_ldrsb(I) ->
  #ldrsb{dst=Dst,am3=Am3} = I,
  NewCond = do_cond('al'),
  NewDst = do_reg(Dst),
  NewAm3 = do_am3(Am3),
  [{'ldrsb', {NewCond,NewDst,NewAm3}, I}].

do_move(I) ->
  #move{movop=MovOp,s=S,dst=Dst,am1=Am1} = I,
  NewCond = do_cond('al'),
  NewS = do_s(S),
  NewDst = do_reg(Dst),
  NewAm1 = do_am1(Am1),
  [{MovOp, {NewCond,NewS,NewDst,NewAm1}, I}].

do_pseudo_li(I, MFA, ConstMap) ->
  #pseudo_li{dst=Dst,imm=Imm} = I,
  {RelocInsn,LongVal} =
    if is_integer(Imm) ->
	%% This is for immediates that require too much work
	%% to reconstruct using only arithmetic instructions.
	{[], Imm};
       true ->
	RelocData =
	  case Imm of
	    Atom when atom(Atom) ->
	      {load_atom, Atom};
	    {Label,constant} ->
	      ConstNo = find_const({MFA,Label}, ConstMap),
	      {load_address, {constant,ConstNo}};
	    {Label,closure} ->
	      {load_address, {closure,Label}};
	    {Label,c_const} ->
	      {load_address, {c_const,Label}}
	  end,
	{[{'.reloc', RelocData, #comment{term=reloc}}], 0}
    end,
  NewDst = do_reg(Dst),
  %% b 1f; .long <data>; 1: ldr dst, [pc,#-12]
  [{b, {do_cond('al'),{imm24,0}}, #comment{term='skip'}} |
   RelocInsn ++
   [{'.long', LongVal, I},
    {ldr, {do_cond('al'),NewDst,{immediate_offset,{r,15},'-',{imm12,12}}}, I}]].

do_store(I) ->
  #store{stop=StOp,src=Src,am2=Am2} = I,
  NewCond = do_cond('al'),
  NewSrc = do_reg(Src),
  NewAm2 = do_am2(Am2),
  [{StOp, {NewCond,NewSrc,NewAm2}, I}].

do_reg(#arm_temp{reg=Reg,type=Type}) when Reg >= 0, Reg < 16, Type /= 'double' ->
  {r,Reg}.
  
do_cond(Cond) -> {'cond',Cond}.

do_s(S) -> {'s', case S of false -> 0; true -> 1 end}.

do_label_ref(Label) when integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

do_am1(Am1) ->
  case Am1 of
    #arm_temp{} -> do_reg(Am1);
    {Src1,'rrx'} -> {do_reg(Src1),'rrx'};
    {Src1,ShiftOp,Src2=#arm_temp{}} -> {do_reg(Src1),{ShiftOp,do_reg(Src2)}};
    {Src1,ShiftOp,Imm5} -> {do_reg(Src1),{ShiftOp,{imm5,Imm5}}};
    {Imm8,Imm4} -> {{imm8,Imm8},{imm4,Imm4}}
  end.

do_am2(#am2{src=Src,sign=Sign,offset=Offset}) ->
  NewSrc = do_reg(Src),
  case Offset of
    #arm_temp{} -> {'register_offset',NewSrc,Sign,do_reg(Offset)};
    {Src3,'rrx'} -> {'scaled_register_offset',NewSrc,Sign,do_reg(Src3),'rrx'};
    {Src3,ShiftOp,Imm5} -> {'scaled_register_offset',NewSrc,Sign,do_reg(Src3),{ShiftOp,{imm5,Imm5}}};
    Imm12 -> {'immediate_offset',NewSrc,Sign,{imm12,Imm12}}
  end.

do_am3(#am3{src=Src,sign=Sign,offset=Offset}) ->
  NewSrc = do_reg(Src),
  case Offset of
    #arm_temp{} -> {'register_offset',NewSrc,Sign,do_reg(Offset)};
    _ -> {'immediate_offset',NewSrc,Sign,{'imm8',Offset}}
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

encode(Code, Options) ->
  CodeSize = compute_code_size(Code, 0),
  ExportMap = build_export_map(Code, 0, []),
  {AccCode,Relocs} = encode_mfas(Code, 0, [], [], Options),
  CodeBinary = list_to_binary(lists:reverse(AccCode)),
  ?ASSERT(CodeSize =:= size(CodeBinary)),
  CombinedLabelMap = combine_label_maps(Code, 0, gb_trees:empty()),
  {CodeSize,CodeBinary,Relocs,CombinedLabelMap,ExportMap}.

compute_code_size([{_MFA,_Insns,CodeSize,_LabelMap}|Code], Size) ->
  compute_code_size(Code, Size+CodeSize);
compute_code_size([], Size) -> Size.

build_export_map([{{M,F,A},_Insns,CodeSize,_LabelMap}|Code], Address, ExportMap) ->
  build_export_map(Code, Address+CodeSize, [{Address,M,F,A}|ExportMap]);
build_export_map([], _Address, ExportMap) -> ExportMap.

combine_label_maps([{MFA,_Insns,CodeSize,LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, Address+CodeSize, NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

encode_mfas([{MFA,Insns,CodeSize,LabelMap}|Code], Address, AccCode, Relocs, Options) ->
  print("Generating code for: ~w\n", [MFA], Options),
  print("Offset   | Opcode   | Instruction\n", [], Options),
  {Address1,Relocs1,AccCode1} =
    encode_insns(Insns, Address, Address, LabelMap, Relocs, AccCode, Options),
  ExpectedAddress = Address + CodeSize,
  ?ASSERT(Address1 =:= ExpectedAddress),
  print("Finished.\n", [], Options),
  encode_mfas(Code, Address1, AccCode1, Relocs1, Options);
encode_mfas([], _Address, AccCode, Relocs, _Options) ->
  {AccCode,Relocs}.

encode_insns([I|Insns], Address, FunAddress, LabelMap, Relocs, AccCode, Options) ->
  case I of
    {'.label',L,_} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      ?ASSERT(Address =:= LabelAddress),	% sanity check
      print_insn(Address, [], I, Options),
      encode_insns(Insns, Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    {'.reloc',Data,_} ->
      Reloc = encode_reloc(Data, Address, FunAddress, LabelMap),
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], AccCode, Options);
    {'.long',Value,_} ->
      Segment = <<Value:32/integer-big>>,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+4, FunAddress, LabelMap, Relocs, NewAccCode, Options);
    _ ->
      {Op,Arg,_} = fix_jumps(I, Address, FunAddress, LabelMap),
      Word = hipe_arm_encode:insn_encode(Op, Arg),
      print_insn(Address, Word, I, Options),
      Segment = <<Word:32/integer-big>>,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+4, FunAddress, LabelMap, Relocs, NewAccCode, Options)
  end;
encode_insns([], Address, _FunAddress, _LabelMap, Relocs, AccCode, _Options) ->
  {Address,Relocs,AccCode}.

encode_reloc(Data, Address, FunAddress, LabelMap) ->
  case Data of
    {b_fun,MFAorPrim,Linkage} ->
      %% b and bl are patched the same, so no need to distinguish
      %% call from tailcall
      PatchTypeExt =
	case Linkage of
	  remote -> ?PATCH_TYPE2EXT(call_remote);
	  not_remote -> ?PATCH_TYPE2EXT(call_local)
	end,
      {PatchTypeExt, Address, untag_mfa_or_prim(MFAorPrim)};
    {load_atom,Atom} ->
      {?PATCH_TYPE2EXT(load_atom), Address, Atom};
    {load_address,X} ->
      {?PATCH_TYPE2EXT(load_address), Address, X};
    {sdesc,SDesc} ->
      #arm_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      {?PATCH_TYPE2EXT(sdesc), Address,
       ?STACK_DESC(ExnRA, FSize, Arity, Live)}
  end.

untag_mfa_or_prim(#arm_mfa{m=M,f=F,a=A}) -> {M,F,A};
untag_mfa_or_prim(#arm_prim{prim=Prim}) -> Prim.

fix_jumps(I, InsnAddress, FunAddress, LabelMap) ->
  case I of
    {b, {Cond,{label,L}}, OrigI} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      Imm24 = (LabelAddress - (InsnAddress+8)) div 4,
      %% ensure Imm24 fits in a 24 bit sign-extended field
      ?ASSERT(Imm24 =<   16#7FFFFF),
      ?ASSERT(Imm24 >= -(16#800000)),
      {b, {Cond,{imm24,Imm24 band 16#FFFFFF}}, OrigI};
    _ -> I
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

is_exported(F, A, Exports) -> lists:member({F,A}, Exports).

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String, Arglist, Options) ->
  ?when_option(pp_asm, Options, io:format(String, Arglist)).

print_insn(Address, Word, I, Options) ->
  ?when_option(pp_asm, Options, print_insn_2(Address, Word, I)).

print_insn_2(Address, Word, {_,_,OrigI}) ->
  io:format("~8.16.0b | ", [Address]),
  print_code_list(word_to_bytes(Word), 0),
  hipe_arm_pp:pp_insn(OrigI).

word_to_bytes(W) ->
  case W of
    [] -> [];	% label or other pseudo instruction
    _ -> [(W bsr 24) band 16#FF, (W bsr 16) band 16#FF,
	  (W bsr 8) band 16#FF, W band 16#FF]
  end.

print_code_list([Byte|Rest], Len) ->
  print_byte(Byte),
  print_code_list(Rest, Len+1);
print_code_list([], Len) ->
  fill_spaces(8-(Len*2)),
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
