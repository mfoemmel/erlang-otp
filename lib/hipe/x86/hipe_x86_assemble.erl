%%% $Id$
%%% hipe_x86_assemble.erl
%%% Copyright (C) 2001 Ulf Magnusson
%%% This is the assembler and linker of the x86 backend
%%% Email: ulf.magnusson@ubm-computing.com
%%% This file given a pseudo x86 code will generate an erlang atom=(object format)
%%% containing the compiled code and patch referances

-module(hipe_x86_assemble).
-export([assemble/3]).
-include("../main/hipe.hrl").
-include("hipe_x86.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").

-record(jmp_label_tagged, {label,tag}).
-record(jcc_tagged, {cond,label,tag}).

% This function takes code and a Const table and creates an object file
% Input: CompiledCode, Closures, Flags
% Generate the data segment
% Generate the code segment
%  preprocess code,
%  calc size, remove labels, (remove comments) for each code seg.
%  Create labeltable (with offset information)
% Generate patchlist

assemble( CompiledCode, Closures, Flags ) ->
    ?when_option(time, Flags, ?start_timer("Assembler start")),
    put(hipe_x86_flags,Flags),
    print("****************** Assembling *******************\n"),
    % Want to have Code = [{MFA,Code,Consttab}|Rest] for compability
    ?when_option(time, Flags, ?start_timer("Converting and running pack constants")),
    NewCode = [{element(1,E), 
		{{hot,element(4,element(2,E))},[]}, 
		element(5,element(2,E))}
	       || E <- CompiledCode],
    Code = [{element(1,D),hipe_x86:defun_code(element(2,D)),[]} || D <- CompiledCode],

    {ConstSize,ConstMap, RefsFromConsts} = hipe_pack_constants:pack_constants( NewCode ),
    ?when_option(time, Flags, ?stop_timer("Converting and running pack constants")),
    ?when_option(time, Flags, ?start_timer("Getting Labels and PSJ")),
    %{Map,ListMap,PSJ} = get_code(NewCode),
    {Map,ListMap,PSJ} = get_code(Code,gb_trees:empty(),[],[],0),
    ?when_option(time, Flags, ?stop_timer("Getting Labels and PSJ")),
    {HAddr,CAddr,AccHCode,AccCCode,AccHRefs,AccCRefs,NewMap,ExportMap} = mk_asmrefs(PSJ,Code,Map,ListMap,ConstMap,Flags),
    print("Total num bytes=~w\n",[HAddr]),
    print("****************Assembler finished*****************\n"),
    AsmCode = {{HAddr,AccHCode,AccHRefs}, % SIZE is changed!!
    	       {CAddr,AccCCode,AccCRefs}}, % There are no cold code!
    ?when_option(time, Flags, ?start_timer("Building object file")),
    Bin = mk_external(ConstSize, ConstMap, RefsFromConsts, NewMap, ExportMap,AsmCode,
		      Closures,Flags),
    ?when_option(time, Flags, ?stop_timer("Building object file")),
    ?when_option(time, Flags, ?stop_timer("Assembler start")),
    Bin.
    
mk_external(ConstSize, ConstMap, RefsFromConsts, NewMap,ExportMap, AsmCode, Closures,Flags) ->
    {{HotSize,AccHCode,AccHRefs},
     {ColdSize,AccCCode,AccCRefs}} = AsmCode,
    ?when_option(time, Flags, ?start_timer("slim constmap")),
    SC = slim_constmap(ConstMap),
    ?when_option(time, Flags, ?stop_timer("slim constmap")),
    ?when_option(time, Flags, ?start_timer("make labelmap")),
    LM = mk_labelmap(RefsFromConsts, NewMap, HotSize ),
    ?when_option(time, Flags, ?stop_timer("make labelmap")),
    ?when_option(time, Flags, ?start_timer("slim sorted exportmap")),
    SSE = slim_sorted_exportmap(ExportMap,Closures),
    ?when_option(time, Flags, ?stop_timer("slim sorted exportmap")),
    %?when_option(time, Flags, ?start_timer("slim refs")),
    %SR = slim_refs(AccHRefs),
    %?when_option(time, Flags, ?stop_timer("slim refs")),
    ?when_option(time, Flags, ?start_timer("term_to_binary")),
    Bin = term_to_binary([?version(),
		    ConstSize,
		    SC,
		    LM,
		    %slim_exportmap(gb_trees:to_list(ExportMap), HotSize, Closures ),
		    SSE,
		    HotSize,AccHCode, %vec_to_list(AccHCode,0,HotSize,[]),
		    AccHRefs,
		    ColdSize,AccCCode,
		    AccCRefs]),
    ?when_option(time, Flags, ?stop_timer("term_to_binary")),
    Bin.

find_short_jumps(Map,[{MFA,Size,L,Instr}|PSJs],AccSJmpTrue) ->
    Address = find( {MFA, L},Map),
    Offset = Address-Size-2,
    case (Offset =< 127) and (Offset >= -128) of
	true ->
	    find_short_jumps(Map,PSJs,[{Size,Instr}|AccSJmpTrue]);
	false ->
	    find_short_jumps(Map,PSJs,AccSJmpTrue)
    end;
find_short_jumps(Map,[],AccSJmpTrue) ->
    AccSJmpTrue.

call_align( [I|Is], Addr, AccCode, SJmpTrue, FakeAddr,AL ) ->
    Type = hipe_x86:insn_type(I),
    case Type of
	call ->
	    % Do call alignment
	    Size = instr_size(I),
	    NumNopsNeeded = (4 - ((Size+Addr) rem 4)) rem 4,
	    ExtraNops = lists:duplicate( NumNopsNeeded, #nop{} ),
	    
	    % the +1 is to make sure we dont align a label at the same pos at the call instr.
	    call_align( Is, Addr+Size+NumNopsNeeded, 
			[I|ExtraNops++AccCode], SJmpTrue, 
			FakeAddr+instr_size(I)+3, [{FakeAddr+NumNopsNeeded+5,NumNopsNeeded-3}|AL] );
	jmp_label ->
	    % We have to tag the jumps explicitly if they are short or long ones
	    case is_member(FakeAddr,SJmpTrue) of
		true ->
		    NewI = #jmp_label_tagged{label=hipe_x86:jmp_label_label(I),tag=short},
		    call_align(Is,Addr+2,[NewI|AccCode], SJmpTrue, FakeAddr+instr_size(I), AL);
		false ->
		    NewI = #jmp_label_tagged{label=hipe_x86:jmp_label_label(I),tag=long},
		    call_align(Is,Addr+5,[NewI|AccCode], SJmpTrue, FakeAddr+instr_size(I), AL)
	    end;
	jcc ->
	    % We have to tag the jumps explicitly if they are short or long ones
	    case is_member(FakeAddr,SJmpTrue) of
		true ->
		    NewI = #jcc_tagged{cond=hipe_x86:jcc_cond(I),label=hipe_x86:jcc_label(I),tag=short},
		    call_align(Is,Addr+2,[NewI|AccCode], SJmpTrue, FakeAddr+instr_size(I), AL);
		false ->
		    NewI = #jcc_tagged{cond=hipe_x86:jcc_cond(I),label=hipe_x86:jcc_label(I),tag=long},
		    call_align(Is,Addr+6,[NewI|AccCode], SJmpTrue, FakeAddr+instr_size(I), AL)
	    end;
	Other ->
	    call_align( Is, Addr+instr_size(I),
			[I|AccCode], SJmpTrue, FakeAddr+instr_size(I), AL )
    end; 
call_align( [], Addr, AccCode, SJmpTrue,FakeAddr, AL ) ->
    {AccCode,AL,Addr,FakeAddr}.

create_alignment_list([{MFA,Hot,Cold}|Rest],HAddr,FakeHAddr,SJmpTrue,AccCode,AL ) ->
    
    % remember that NewHot,ExtraAL is reversed!
    {NewHot,ExtraAL,NewAddr,NewFakeAddr} = call_align(Hot,HAddr,[],SJmpTrue,FakeHAddr,[] ),

    % Do aligning 4 byte, we do padding!
    HAlign = (4 - (NewAddr rem 4)) rem 4,
    ExtraHBytes = lists:duplicate( HAlign, #nop{} ), % insert nops in padding

    create_alignment_list( Rest, NewAddr+HAlign, NewFakeAddr, SJmpTrue, 
			   [{MFA,lists:reverse(NewHot,ExtraHBytes),Cold}|AccCode], [{NewFakeAddr,HAlign},ExtraAL,AL] );
create_alignment_list( [], Addr, FakeAddr, SJmpTrue, AccCode, AL ) ->
    {lists:reverse(AccCode),lists:reverse(lists:flatten(AL)),Addr}.

ival(jcc) -> 4;
ival(jmp) -> 3.

do_aligning( [{AA,A}|AL], [{{MFA,L},MA}|Map], NewMap, AccAlign ) when MA<AA ->
    do_aligning( [{AA,A}|AL], Map, gb_trees:insert({MFA,L},MA+AccAlign,NewMap),AccAlign );
do_aligning( [{AA,A}|AL], [{{MFA,L},MA}|Map], NewMap, AccAlign ) ->
    do_aligning( AL, [{{MFA,L},MA}|Map], NewMap, AccAlign+A );
do_aligning( _, [], NewMap, AccAlign ) ->
    NewMap;
do_aligning( [], [{{MFA,L},MA}|Map], NewMap, AccAlign ) ->
    do_aligning( [], Map, gb_trees:insert({MFA,L},MA+AccAlign,NewMap), AccAlign ).

% return SJmpTrue so that the MAP can be corrected accordingly
% return AlignmentMap=FHN, one entry per mfa with how much alignment it should be corrected with
%                      this includes both mfa and call alignment.
mk_asmrefs(PSJ,Code,Map,ListMap,ConstMap,Flags) ->
    ?when_option(time, Flags, ?start_timer("Finding short jumps")),
    %io:format("PSJ:~w\n",[PSJ]),
    
    SJmpTrue = find_short_jumps(Map,PSJ,[]),
    ?when_option(time, Flags, ?stop_timer("Finding short jumps")),
    %% AL is list [{offset,alignment},{offset,alignment}...]
    ?when_option(time, Flags, ?start_timer("Creating alignment list")),
    {NewCode,AL,Length} = create_alignment_list( Code, 0, 0, SJmpTrue, [],[] ),
    ?when_option(time, Flags, ?stop_timer("Creating alignment list")),
    ?when_option(time, Flags, ?start_timer("Do Aligning")),
    NewAL = merge_al_and_sjmptrue(AL,SJmpTrue,[]),
    %NewMap = do_aligning( AL, lists:reverse(ListMap), SJmpTrue, gb_trees:empty(),0  ),
    NewMap = do_aligning( NewAL, lists:reverse(ListMap), gb_trees:empty(),0 ),
    ?when_option(time, Flags, ?stop_timer("Do Aligning")),
    %io:format("NewMap:~w\n",[NewMap]),
    ?when_option(time, Flags, ?start_timer("Assembling")),
    Array = hipe_bifs:array(Length, 0),
    A = do_mk_asmrefs(NewCode,0,NewMap,ConstMap,Array,[],[]),
    ?when_option(time, Flags, ?stop_timer("Assembling")),
    A.

merge_al_and_sjmptrue( [{AA,A}|AL], [{SA,I}|SJmpTrue], Acc ) when AA=<SA ->
    merge_al_and_sjmptrue(AL, [{SA,I}|SJmpTrue], [{AA,A}|Acc] );
merge_al_and_sjmptrue( [{AA,A}|AL], [{SA,I}|SJmpTrue], Acc ) ->
    merge_al_and_sjmptrue( [{AA,A}|AL], SJmpTrue, [{SA-1,-ival(I)}|Acc] );
merge_al_and_sjmptrue( [],[{SA,I}|SJmpTrue], Acc ) ->
    merge_al_and_sjmptrue( [], SJmpTrue, [{SA-1,I}|Acc] );
merge_al_and_sjmptrue( [{AA,A}|AL], [], Acc ) ->
    merge_al_and_sjmptrue(AL, [], [{AA,A}|Acc] );
merge_al_and_sjmptrue( [],[],Acc ) ->
    lists:reverse(Acc).

do_mk_asmrefs([{MFA,Hot,Cold}|Rest],HAddr,Map,ConstMap,Array,AccHRefs,ExportMap) ->
    print("Generating code for:~w\n",[MFA]),
    %io:format("FakeAddr corrected:~w\n",[FakeHAddr+correct_dist_to_label(0,FakeHAddr,SJmpTrue)]),
    print( "Offset   | Opcode (hex)             | Instruction\n" ),
    {NewHAddr,HRefs} = mk_asmref(Hot,MFA,HAddr,Map,ConstMap,AccHRefs,Array),

    print("Finished.\n\n"),
    {M,F,A} = MFA,
    do_mk_asmrefs(Rest,NewHAddr,Map,ConstMap,Array,HRefs,[{HAddr,M,F,A}|ExportMap]);
do_mk_asmrefs([],HAddr,Map,ConstMap,Array,AccHRefs,ExportMap) ->
  {HAddr,0,Array,[],AccHRefs,[],Map,ExportMap}.

% This little function returns the size of a instruction
% Must check certain parameters for accurate answer.
% hmm, integrate with mk_asmref function possibly for avoiding double work.
% ask encode module of sizes.
% This function does not neccecerely return the final sizes, but more "fake" sizes
% since the short jump optimization is taken care after this.
instr_size( Instr ) ->
    Type = hipe_x86:insn_type(Instr),
    
    case Type of
	call ->
	    {Arg,_} = jmp_resolve_arg( hipe_x86:call_fun(Instr), nomfa, 0, [], [],0,undefined ),
	    hipe_x86_encode:insn_sizeof(call,{Arg});
	comment -> 0;
	jmp_fun -> 	    
	    {Arg,_} = jmp_resolve_arg( hipe_x86:jmp_fun_fun(Instr), nomfa, 0, [], [],0,undefined ),
	    hipe_x86_encode:insn_sizeof(jmp,{Arg});
	jmp_label -> 5; % always rel32 fake version!
	jmp_switch -> 7;
	cmp ->
	    {Arg,_} = resolve_alu_args(hipe_x86:cmp_src(Instr),hipe_x86:cmp_dst(Instr), 0, nopatch,[]),
	    Size = hipe_x86_encode:insn_sizeof(cmp,Arg),
	    Size;
	move ->
	    {Arg,_} = resolve_move_args(hipe_x86:move_src(Instr),hipe_x86:move_dst(Instr), 0, nopatch, nomfa, [] ),
	    hipe_x86_encode:insn_sizeof(mov,Arg);
	ret ->
	    case hipe_x86:ret_npop(Instr) of
		0 -> 1;
		_ -> 3
	    end;
	alu ->
	    {Arg,_} = resolve_alu_args(hipe_x86:alu_src(Instr),hipe_x86:alu_dst(Instr), 0, nopatch,[]),
	    Size = hipe_x86_encode:insn_sizeof(hipe_x86:alu_op(Instr),Arg);
	jcc -> 6;
        nop -> 1;
	push -> 
	    {Arg,_} = resolve_arg(hipe_x86:push_src(Instr),nomfa, 0, [], [], 0, nopatch),
	    NewArg =
		case Arg of
		    {imm8,Imm} ->
			{imm32,Imm};
		    Other ->
			Other
		end,		    
	    Size = hipe_x86_encode:insn_sizeof( push,{NewArg} );
	inc ->
	    {Arg,_} = resolve_arg(hipe_x86:inc_dst(Instr),nomfa, 0, [], [], 0,nopatch),
	    Size = hipe_x86_encode:insn_sizeof( inc,{Arg} );
	dec ->
	    {Arg,_} = resolve_arg(hipe_x86:dec_dst(Instr),nomfa, 0, [], [], 0,nopatch),
	    Size = hipe_x86_encode:insn_sizeof( dec,{Arg} );
	cmovcc ->
	    {Arg,_} = resolve_move_args(hipe_x86:cmovcc_src(Instr),hipe_x86:cmovcc_dst(Instr),
					0, nopatch, nomfa, [] ),
	    NewInstr = {cmovcc, {{cc,hipe_x86_encode:cc(hipe_x86:cmovcc_cond(Instr))}, Arg }},
	    Size = hipe_x86_encode:insn_sizeof(NewInstr);
	lea ->
	    {Arg,Ref} =	resolve_lea_args(hipe_x86:lea_mem(Instr),hipe_x86:lea_temp(Instr)),
	    Size = hipe_x86_encode:insn_sizeof(lea,Arg);
	label ->
	    0;
        Other -> ?EXIT({sizeOfInstructionNotSupported,Other})
    end.

list_to_array( List, Array, Addr ) ->
    lists:foldl( fun(X,I) -> hipe_bifs:array_update(Array,I,X), I+1 end, Addr, List ).
 
% This function looks at a single instruction,
% Assembles and possibly adds a patch to the patchlist
% which the loader will fix later.
% return: {assembled code, size, patches,fakesize}
mk_asmref([I|Is],MFA,Addr,Map,ConstMap,Refs,CodeList) ->
    Type = hipe_x86:insn_type(I),
    print("~8s | ",[hipe_converters:int_to_hex(Addr)]),

    case Type of
	call ->
	    %% XXX: The sdesc should be allocated and relocated like
	    %% other non-code objects. Put it in the ConstMap?
	    SDesc = hipe_x86:call_sdesc(I),
 	    %% #x86_sdesc{exnlab=ExnLab,fsize=FSize,skip=Skip,arity=Arity} = SDesc,
	    %% io:format("\nStack descriptor:\nExnlab:~w\nFSize:~w\nSkip:~w\nArity:~w\n",[ExnLab,FSize,Skip,Arity]),
	    {Arg,Ref} = jmp_resolve_arg( hipe_x86:call_fun(I), MFA, Addr, Map, ConstMap, 0,undefined ),
	    NewSDesc =
		case SDesc#x86_sdesc.exnlab of
		    [] -> SDesc;	% don't cons up a new one
		    ExnLab -> SDesc#x86_sdesc{exnlab=find({MFA,ExnLab},Map)}
		end,
	    Size = hipe_x86_encode:insn_sizeof(call,{Arg}),
	    NewRef = [{?PATCH_TYPE2EXT(sdesc),Addr+Size,NewSDesc} | Ref],
	    Code = hipe_x86_encode:insn_encode(call,{Arg}),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,NewRef++Refs,
		      CodeList);
	comment ->
	    print_code_list([]),
	    print_insn(I),
	    mk_asmref(Is,MFA,Addr,Map,ConstMap,Refs,CodeList);	    
	jmp_fun ->
	    %sometimes via register
	    {Arg,Ref} = jmp_resolve_arg( hipe_x86:jmp_fun_fun(I), MFA, Addr, Map, ConstMap, 0,undefined ),
	    Code = hipe_x86_encode:insn_encode(jmp,{Arg}),
	    Size = hipe_x86_encode:insn_sizeof(jmp,{Arg}),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	jmp_label_tagged ->
	    #jmp_label_tagged{label=Label,tag=Tag} = I,
	    {Arg,Ref} = jmp_resolve_arg( Label, MFA, Addr, Map, ConstMap,0,Tag ),
	    Code = hipe_x86_encode:insn_encode(jmp,{Arg}),
	    Size = hipe_x86_encode:insn_sizeof(jmp,{Arg}),
	    print_code_list(Code),
	    print_insn(#jmp_label{label=Label}),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	jmp_switch ->
	    %First the encoding of the instruction..
	    SINDEX = hipe_x86_encode:sindex( 2, element(2,hipe_x86:jmp_switch_temp(I)) ),
	    EA = hipe_x86_encode:ea_disp32_sindex( 0 ,SINDEX ), % this creates a SIB implicitly
	    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
	    Code = hipe_x86_encode:insn_encode(jmp,{RM32}),
	    Size = hipe_x86_encode:insn_sizeof(jmp,{RM32}),
	    
	    %Then the patching...
	    %io:format("Constant Map:~w\n",[ConstMap]),
	    ConstNo = find_const({MFA,hipe_x86:jmp_switch_jtab(I)},ConstMap),
	    Ref = {?PATCH_TYPE2EXT(load_address),Addr+3,{constant,ConstNo}},
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,[Ref|Refs],CodeList);
	cmp ->
	    {Arg,Ref} = resolve_alu_args(hipe_x86:cmp_src(I),hipe_x86:cmp_dst(I), Addr,patch,ConstMap ),
	    Code = hipe_x86_encode:insn_encode(cmp,Arg),
	    Size = hipe_x86_encode:insn_sizeof(cmp,Arg),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	move ->
	    {Arg,Ref} =
		resolve_move_args(hipe_x86:move_src(I),hipe_x86:move_dst(I),
			    Addr, patch, MFA, ConstMap ),
	    Code = hipe_x86_encode:insn_encode(mov,Arg),
	    Size = hipe_x86_encode:insn_sizeof(mov,Arg),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	ret ->
	    {Code,Size} =
	    case hipe_x86:ret_npop(I) of
		0 ->
		    {hipe_x86_encode:insn_encode( ret,{} ),
		     hipe_x86_encode:insn_sizeof( ret,{} )};
		_ ->
		    {hipe_x86_encode:insn_encode( ret,{{imm16,hipe_x86:ret_npop(I)}} ),
		     hipe_x86_encode:insn_sizeof( ret,{{imm16,hipe_x86:ret_npop(I)}} )}
	    end,
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Refs,CodeList);
	% add,sub,cmp,...
	alu ->
	    {Arg,Ref} = resolve_alu_args( hipe_x86:alu_src(I),hipe_x86:alu_dst(I), Addr, patch, ConstMap ),
	    Code = hipe_x86_encode:insn_encode(hipe_x86:alu_op(I),Arg),
	    Size = hipe_x86_encode:insn_sizeof(hipe_x86:alu_op(I),Arg),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	% Conditinal jumps param = {{cc,CC}, {rel32,Rel32}}
	jcc_tagged ->
	    #jcc_tagged{cond=Cond,label=Label,tag=Tag} = I,
	    %io:format("jcc SJmpTrue:~w FakeAddr:~w\n",[SJmpTrue,FakeAddr]),
	    {Arg,Ref} = jmp_resolve_arg( Label, MFA, Addr, Map, ConstMap,1, Tag),
	    Instr = {jcc, {{cc,hipe_x86_encode:cc(Cond)}, Arg }},
	    Code = hipe_x86_encode:insn_encode( Instr ),
	    Size = hipe_x86_encode:insn_sizeof( Instr ),
	    print_code_list(Code),
	    print_insn(#jcc{cond=Cond,label=Label}),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	nop ->
	    Code = hipe_x86_encode:insn_encode( nop,{} ),
	    Size = hipe_x86_encode:insn_sizeof( nop,{} ),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Refs,CodeList);
	push ->
	    %io:format("push arg:~w\n",[hipe_x86:push_src(I)]),
	    {Arg,Ref} = resolve_arg(hipe_x86:push_src(I),MFA, Addr, Map, ConstMap, 1,patch),
	    % Make Sure we push 32 bit
	    NewArg =
		case Arg of
		    {imm8,Imm} ->
			{imm32,Imm};
		    Other ->
			Other
		end,		    
	    Code = hipe_x86_encode:insn_encode( push,{NewArg} ),
	    Size = hipe_x86_encode:insn_sizeof( push,{NewArg} ),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	inc ->
	    % increase operation
	    {Arg,Ref} = resolve_arg(hipe_x86:inc_dst(I),MFA, Addr, Map, ConstMap, 1,patch),
	    Code = hipe_x86_encode:insn_encode( inc,{Arg} ),
	    Size = hipe_x86_encode:insn_sizeof( inc,{Arg} ),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	dec ->
	    % decrease
	    {Arg,Ref} = resolve_arg(hipe_x86:dec_dst(I),MFA, Addr, Map, ConstMap, 1,patch),
	    Code = hipe_x86_encode:insn_encode( dec,{Arg} ),
	    Size = hipe_x86_encode:insn_sizeof( dec,{Arg} ),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	cmovcc ->
	    % Conditional move
	    {Arg,Ref} =	resolve_move_args(hipe_x86:cmovcc_src(I),hipe_x86:cmovcc_dst(I),
					  Addr, patch, MFA, ConstMap ),
	    Instr = {cmovcc, {{cc,hipe_x86_encode:cc(hipe_x86:cmovcc_cond(I))}, Arg }},
	    Code = hipe_x86_encode:insn_encode(Instr),
	    Size = hipe_x86_encode:insn_sizeof(Instr),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);
	lea ->
	    {Arg,Ref} =	resolve_lea_args(hipe_x86:lea_mem(I),hipe_x86:lea_temp(I)),
	    Code = hipe_x86_encode:insn_encode(lea,Arg),
	    Size = hipe_x86_encode:insn_sizeof(lea,Arg),
	    print_code_list(Code),
	    print_insn(I),
	    list_to_array( Code, CodeList, Addr ),
	    mk_asmref(Is,MFA,Addr+Size,Map,ConstMap,Ref++Refs,CodeList);	    
	label ->
	    print_code_list([]),
	    print_insn(I),
	    mk_asmref(Is,MFA,Addr,Map,ConstMap,Refs,CodeList)
    end;

mk_asmref([],_,Addr,_,_,Refs,Code) ->
    {Addr,Refs}.

create_ea(Base, Off) ->
    if 
	Off =:= 0 ->
	    case Base of
		4 -> %esp, use SIB w/o disp8
		    SIB = hipe_x86_encode:sib(Base),
		    {hipe_x86_encode:ea_sib(SIB), -3};
		5 -> %ebp, use disp8 w/o SIB
		    {hipe_x86_encode:ea_disp8_base(Off, Base), -3};
		_ -> %neither SIB nor disp8 needed
		    {hipe_x86_encode:ea_base(Base), -4}
	    end;
       Off >= -128, Off =< 127 ->
	    case Base of
		4 -> %esp, must use SIB
		    SIB = hipe_x86_encode:sib(Base),
		    {hipe_x86_encode:ea_disp8_sib(Off, SIB), -2};
		_ -> %use disp8 w/o SIB
		    {hipe_x86_encode:ea_disp8_base(Off, Base), -3}
	    end;
       true ->
	    case Base of
		4 -> %esp, must use SIB
		    SIB = hipe_x86_encode:sib(Base),
		    {hipe_x86_encode:ea_disp32_sib(Off, SIB), 1};
		_ ->
		    {hipe_x86_encode:ea_disp32_base(Off, Base), 0}
	    end
    end.

% lea reg, mem
resolve_lea_args( #x86_mem{base=#x86_temp{reg=TempSrc},off=#x86_imm{value=ImmSrc}}, #x86_temp{reg=Temp} ) ->
    {EA,Extra} = create_ea(TempSrc,ImmSrc),
    {{{reg32,Temp},{ea,EA}},[]}.

% mov mem, imm
resolve_move_args( #x86_imm{value=ImmSrc}, #x86_mem{base=#x86_temp{reg=Temp},off=#x86_imm{value=ImmDst}}, 
		   Addr, Patch,MFA, ConstMap ) ->
    {EA,Extra} = create_ea(Temp,ImmDst),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{_,Imm},Ref} = resolve_arg( #x86_imm{value=ImmSrc}, MFA, Addr, [], ConstMap, 6+Extra,Patch ),
    {{RM32,{imm32,Imm}},Ref};
% mov reg,mem
resolve_move_args( #x86_mem{base=#x86_temp{reg=TempSrc},off=#x86_imm{value=ImmSrc}}, #x86_temp{reg=Temp}, 
		   Addr,Patch,MFA, ConstMap ) ->
    {EA,Extra} = create_ea(TempSrc,ImmSrc),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{{reg32,Temp},RM32},[]};
% mov mem,reg
resolve_move_args( #x86_temp{reg=Temp}, #x86_mem{base=#x86_temp{reg=TempDst},off=#x86_imm{value=ImmDst}}, 
		   Addr, Patch,MFA, ConstMap ) ->
    {EA,Extra} = create_ea(TempDst,ImmDst),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{RM32,{reg32,Temp}},[]};
% mov reg,reg
resolve_move_args( #x86_temp{reg=SrcTemp}, #x86_temp{reg=DstTemp}, 
		   Addr, Patch,MFA, ConstMap ) ->
    RM32 = {rm32,hipe_x86_encode:rm_reg( SrcTemp )},
    {{{reg32,DstTemp},RM32},[]};
% mov reg,imm
resolve_move_args( Src=#x86_imm{value=ImmSrc}, Dst=#x86_temp{reg=Temp}, Addr,Patch,MFA, ConstMap ) ->
    {{_,Imm},Ref} = resolve_arg( Src, MFA, Addr, [], ConstMap, 1,Patch ),
    {{{reg32,Temp},{imm32,Imm}},Ref}.

% ALU, takes basically the same variants as the move,
% however due to fewer encoding possibilities this must be a own function.
% alu mem, imm
resolve_alu_args( #x86_imm{value=ImmSrc}, #x86_mem{base=#x86_temp{reg=Temp},off=#x86_imm{value=ImmDst}}, Addr, Patch, ConstMap ) ->
    {EA,Extra} = create_ea(Temp,ImmDst),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{_,Imm},Ref} = resolve_arg( #x86_imm{value=ImmSrc}, nomfa, Addr, [], ConstMap, 6+Extra,Patch ),
    case (Imm =< 127) and (Imm >= -128) of
	true ->
	    {{RM32,{imm8,Imm}},Ref};
	false ->
	    {{RM32,{imm32,Imm}},Ref}
    end;

% alu reg,mem
resolve_alu_args( #x86_mem{base=#x86_temp{reg=TempSrc},off=#x86_imm{value=ImmSrc}}, #x86_temp{reg=Temp}, Addr, Patch, ConstMap ) ->
    {EA,Extra} = create_ea(TempSrc,ImmSrc),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{{reg32,Temp},RM32},[]};
% alu mem,reg
resolve_alu_args( #x86_temp{reg=Temp}, #x86_mem{base=#x86_temp{reg=TempDst},off=#x86_imm{value=ImmDst}}, Addr, Patch, ConstMap ) ->
    {EA,Extra} = create_ea(TempDst,ImmDst),
    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
    {{RM32,{reg32,Temp}},[]};
% alu reg,reg
resolve_alu_args( #x86_temp{reg=SrcTemp}, #x86_temp{reg=DstTemp}, Addr,Patch, ConstMap  ) ->
    RM32 = {rm32,hipe_x86_encode:rm_reg( SrcTemp )},
    {{{reg32,DstTemp},RM32},[]};
% alu reg,imm
resolve_alu_args( Src=#x86_imm{value=ImmSrc}, Dst=#x86_temp{reg=Temp}, Addr, Patch, ConstMap ) ->
    {{ImmSize,Imm},Ref} = resolve_arg( Src, nomfa, Addr, [], ConstMap, 2,Patch ),
    RM32 = {rm32,hipe_x86_encode:rm_reg(Temp)},
    {{RM32,{ImmSize,Imm}},[]}.

is_member(Addr, Instrs) -> lists:keymember(Addr, 1, Instrs).

% When patching is needed be sure to correct the address with short jumps decreases
% The Extra parameter is for jcc jumps which is 6 bytes not 5!
jmp_resolve_arg( Jmp_arg, MFA, Addr, Map, ConstMap, Extra, Tag ) ->
    %io:format("Jmp_arg:~w Map:~w\n",[Jmp_arg,Map]),
    case Jmp_arg of
	#x86_temp{reg=Reg} ->
	    %io:format("Jumping via register\n"),
	    RM32 = {rm32,hipe_x86_encode:rm_reg( Reg )},
	    {RM32,[]};
	#x86_mem{base=#x86_temp{reg=TempDst},off=#x86_imm{value=ImmDst}} ->
	    %io:format("Jumping via memory\n"),
	    {EA,_} = create_ea(TempDst,ImmDst),
	    RM32 = {rm32,hipe_x86_encode:rm_mem(EA)},
	    {RM32,[]};
	{x86_mfa,M,F,A} ->
	    {{rel32,0},[{?PATCH_TYPE2EXT(mfa),Addr+1,{M,F,A}}]};
	{x86_prim,Prim} ->
	    {{rel32,0},[{?PATCH_TYPE2EXT(call_bif),Addr+1,Prim}]};
	_ ->
	    if
	        % This is a label, find it, calculate relative offset and return it.
		integer(Jmp_arg) ->
		    Address = find( {MFA, Jmp_arg},Map),
		    case Tag of
			short ->
			    Offset = Address-Addr-2,
			    {{rel8,Offset},[]};
			long ->
			    Offset = Address-Addr-5-Extra,
			    {{rel32,Offset},[]}
		    end;
		atom(Jmp_arg) ->
		    % This is a bif, add reference to patchlist
		    {{rel32,0},[{?PATCH_TYPE2EXT(call_bif),Addr+1,Jmp_arg}]};
		true ->
		    %This should not happen
		    ?EXIT(notsupportedjmpargument)
	    end
    end.

% return {arg for encoding,Refs}
% BytesToImm32 is the offset from the first byte of the imm32 word.
% e.g call imm32, BytesToImm32=1
% Addr = Offset from byte zero in this module to the imm32 value.
resolve_arg( Arg, MFA, Addr, Map, ConstMap, BytesToImm32,Patch ) ->
    case Arg of
	{reg32,Reg32} ->
	    % %if it is a reg no patching is needed
	    {Arg,[]};
        #x86_imm{value=Imm} ->
	    if
		atom(Imm) ->
		    %print("Atom:~w added to patchlist at addr:~w - ",[Imm,Addr+BytesToImm32]),
		    {{imm32,9},[{?PATCH_TYPE2EXT(load_atom),Addr+BytesToImm32,Imm}]};
		integer(Imm) ->
		    case (Imm < 128) and (Imm > -127) of % XXX: -128 <= Imm <= 127 ???
			true ->
			    {{imm8,Imm},[]};
			false ->
			    {{imm32,Imm},[]}
		    end;
		true ->
		    % Ok, this is the case when {label,type} (type=label,catch,constant)
		    case Patch of
			nopatch ->
			    {{imm32,0},[]};
			patch ->
			    case element(2,Imm) of
				% label wrong FIXME
				label -> % type
				    ?EXIT(notsupportinglabelcatch);
				% Constant
				constant -> % type
				    ConstNo = find_const({MFA,element(1,Imm)},ConstMap),
				    {{imm32,0},[{?PATCH_TYPE2EXT(load_address),Addr+BytesToImm32,{constant,ConstNo}}]};
				closure ->
				    {{imm32,0},[{?PATCH_TYPE2EXT(load_address),Addr+BytesToImm32,
						{closure,element(1,Imm)}}]};
				Other ->
				    ?EXIT({unsupportedimm32value,Other})
			    end
		    end
	    end;
        #x86_temp{reg=Temp} ->
	    {{reg32,Temp},[]};
	% Push uses this, and goes via ESP so the SIB byte stays...
	#x86_mem{base=#x86_temp{reg=Temp},off=#x86_imm{value=Imm32}} ->
	    {EA,Extra} = create_ea( Temp, Imm32 ),
	    DST = {rm32,hipe_x86_encode:rm_mem(EA)},
	    {DST,[]}
    end.

get_code([{MFA,Hot,Cold}|Rest], Map, ListMap, PSJ, Pos) ->
    {NewSize,NewMap,NewListMap,NewPSJ} = process_instr(Hot,Pos,gb_trees:insert({MFA,entry},Pos,Map),[{{MFA,entry},Pos}|ListMap],MFA,PSJ),
    get_code(Rest, NewMap,NewListMap,NewPSJ,NewSize);
get_code( [], Map, ListMap, PSJ, Pos ) ->
    {Map,ListMap,PSJ}.

process_instr([I|Is],Size,Map,ListMap,MFA,PossibleShortJumps) ->
    %io:format("~w\n",[I]),
    case hipe_x86:insn_type(I) of
	jmp_label ->
	    L = hipe_x86:jmp_label_label(I),
	    if
		integer(L) ->
		    % Possible two byte
		    process_instr(Is,Size+instr_size(I),Map,ListMap,
				  MFA,[{MFA,Size,L,jmp}|PossibleShortJumps]);
		true ->
		    process_instr(Is,Size+instr_size(I),Map,ListMap,
				  MFA,PossibleShortJumps)
	    end;
	jcc ->
	    L = hipe_x86:jcc_label(I),
	    if
		integer(L) ->
		    % Possible two byte
		    process_instr(Is,Size+instr_size(I),Map,ListMap,
				  MFA,[{MFA,Size,L,jcc}|PossibleShortJumps]);
		true ->
		    process_instr(Is,Size+instr_size(I),Map,ListMap,
				  MFA,PossibleShortJumps)
	    end;
	label ->
	    %print("Found label:{~w,~w}, at pos:~w adding to Map\n",[MFA,hipe_x86:label_label(I),Size]),
	    process_instr(Is,Size,gb_trees:insert({MFA,hipe_x86:label_label(I)},Size,Map),
			  [{{MFA,hipe_x86:label_label(I)},Size}|ListMap],MFA,PossibleShortJumps);
	call ->
	    % reserve space for possible three nops
	    process_instr(Is,Size+instr_size(I)+3,Map,ListMap,MFA,PossibleShortJumps);
	Other ->
	    process_instr(Is,Size+instr_size(I),Map,ListMap,MFA,PossibleShortJumps)
    end;
process_instr([],Size,Map,ListMap,_,PSJ) ->
    {Size,Map,ListMap,PSJ}.

slim_constmap(Map) ->
  slim_constmap(Map,[]).
slim_constmap([{MFA, Label, ConstNo,
		Offset, Need, Type, Term, Export}|Rest],Acc) ->
  slim_constmap(Rest, [ConstNo, Offset, Need, Type, Export, Term| Acc]);
slim_constmap([],Acc) -> Acc.

mk_labelmap(Map, ExportMap, HotSize ) ->
  LblMap = lists:flatten(mk_labelmap(Map, ExportMap,[],HotSize)).

mk_labelmap([{MFA, Labels}| Rest], ExportMap, Acc, HotSize ) ->
    Map = [case Label of
	       {L,Pos} ->
		   Offset = find({MFA,L}, ExportMap),
		   {Pos,Offset};
	       {sorted,Base,OrderedLabels} ->
		   {sorted, Base, [begin
				       Offset = find({MFA,L},ExportMap),
				       {Order, Offset}
				   end
				   || {L,Order} <- OrderedLabels]}
	   end
	   || Label <- Labels],
    %% msg("Map: ~w Map\n",[Map]),
    mk_labelmap(Rest, ExportMap, [Map,Acc], HotSize );

mk_labelmap([],_,Acc,_) -> Acc.

slim_sorted_exportmap([{Adr,M,F,A}|Rest], Closures) ->
  IsClosure = lists:member({M,F,A}, Closures),
  [Adr, M,F,A, IsClosure | slim_sorted_exportmap(Rest, Closures)];
slim_sorted_exportmap([],_) -> [].

find({MFA,L},Map) ->
    gb_trees:get({MFA,L},Map).

print( String ) ->
    Flags = get(hipe_x86_flags),
    ?when_option(pp_asm, Flags,io:format(String,[])).
print( String, Arglist ) ->
    Flags = get(hipe_x86_flags),
    ?when_option(pp_asm, Flags,io:format(String,Arglist)).

print_insn(I) ->
    Flags = get(hipe_x86_flags),
    ?when_option(pp_asm, Flags,hipe_x86_pp:pp_insn(I)).
   
fill_spaces( 0 ) ->
    ok;
fill_spaces( Num ) ->
    io:format(" "),
    fill_spaces( Num-1 ).

print_code_list(Code) ->
    Flags = get(hipe_x86_flags),
    ?when_option(pp_asm, Flags, 
		 lists:foreach( 
		   fun(B) -> if B<0 ->
				     io:format("~s",[hipe_converters:int_to_hex(256+B)]);
				true ->
				     if B<16 ->
					     io:format("0~s",[hipe_converters:int_to_hex(B)]);
					true ->
					     io:format("~s",[hipe_converters:int_to_hex(B)])
				     end
			     end
		   end,
		   Code)),
    % Print the rest 12-size(code) with spaces. and a space and a | and a space
    ?when_option(pp_asm, Flags,fill_spaces( 24-(length(Code)*2) ) ),
    ?when_option(pp_asm, Flags,io:format(" | ") ).

find_const({MFA,Label},[{MFA,Label,ConstNo,_,_, _, _,_}|_])->
  ConstNo;
find_const(N,[M|R]) ->
  find_const(N,R);
find_const(C,[]) ->
  ?EXIT({constant_not_found,C}).

