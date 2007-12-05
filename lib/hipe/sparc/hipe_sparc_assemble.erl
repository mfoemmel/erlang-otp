%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_assemble.erl
%%  Module   :	hipe_sparc_assemble
%%  Purpose  :  Writes SPARC-code to memory. 
%%  Notes    :  Not all SPARC instructions are handled.
%%  History  :	* 1998-06-18 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%     $Id$
%% ====================================================================
%% @doc
%%
%% Module that assembles SPARC code and returns it in the form of
%% HiPE's external format.
%%
%% The external format for SPARC code is a binary representation of an
%% Erlang list of the form:
%% <pre>
%%    [Version::version(),
%%     ConstSize::size(), ConstMap::constmap(), LabelMap::labelmap(),
%%     ExportMap::exportmap(),
%%     HotSize::size(),   HotCode::code(),   HotRefs::refs(),
%%     ColdSize::size(),  ColdCode::code(),  ColdRefs::refs()
%%    ]
%% </pre>
%% where
%% <ul>
%%   <li><code> version():   {VERSION::string(), SYSTEM-CHECKSUM} </code></li>
%%   <li><code> size():      non_neg_integer() </code></li>
%%   <li><code> constmap():  [ConstNo::integer(), Offset::integer(),
%%                            Need::integer(), Type::consttype(),
%%                            Exported::bool(), Data::term()
%%                            | constmap] </code></li>
%%   <li><code> labelmap():  [{DataOffset:integer, CodeOffset:integer}
%%                            | labelmap] </code></li>
%%   <li><code> exportmap(): [Offset::integer(), Module::atom(),
%%                            Function::atom(), Arity::integer()
%%                            | exportmap()] </code>
%%                                A list sorted on <code>Offset</code>. </li>
%%   <li><code> code():      [B4::byte(), B3::byte(), B2::byte(), B1::byte()
%%                            | code()] </code></li>
%%   <li><code> refs():      [{RefType:integer, Reflist:reflist}
%%                            | refs()] </code></li>
%%
%%   <li><code> reflist():   [{Data::term(),Offsets::offests()}
%%                            | reflist()] </code></li>
%%   <li><code> offsets():   [Offset::integer() | offsets()] </code></li>
%%
%%   <li><code> constype():  0 | 1 </code> (0 -> term (arbitrary erlang term), 
%%                                          1 -> block (a list of bytes)) </li>
%%   <li><code> bool():      0 | 1 </code> (false, true) </li>
%%   <li><code> mode():      hot | cold</code></li>
%% </ul>
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_assemble).
-export([assemble/4]).

%-ifndef(DEBUG).
%-define(DEBUG,true).
%-endif.
-define(DO_ASSERT,true).

-include("../../kernel/src/hipe_ext_format.hrl").
-include("../main/hipe.hrl").
-include("hipe_sparc.hrl").
-include("../rtl/hipe_literals.hrl").

%%---------------------------------------------------------------------
%% @spec assemble(term(), term(), [terms()], [Option]) -> binary()
%%     Option = term()
%% @doc  Assembles the compiled code in a SPARC-specific way and
%%       returns a binary according to HiPE's external format.
%% @end
%%---------------------------------------------------------------------
 
assemble(CompiledCode, Closures, Exports, Options) ->
  ?when_option(time, Options, ?start_timer("SPARC assembler")),
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(CompiledCode,
				       hipe_sparc_registers:alignment()),
  %% io:format("Const Size ~w\n",[ConstSize]),
  {CodeSize,ExportMap,Code} = get_code(CompiledCode),
  {AccHCode,AccHRefs} = linker(Code,ExportMap,ConstMap),
  CodeBinary = mk_code_binary(AccHCode),
  Bin = term_to_binary([{?VERSION_STRING(),?HIPE_SYSTEM_CRC},
			ConstAlign, ConstSize,
			hipe_pack_constants:slim_constmap(ConstMap),
			mk_labelmap(RefsFromConsts, ExportMap),
			slim_exportmap(ExportMap, Closures, Exports),
			CodeSize,CodeBinary,
			hipe_pack_constants:slim_refs(AccHRefs),
			0,[] % ColdCodeSize, SlimColdRefs
		       ]),
  ?when_option(time, Options, ?stop_timer("SPARC assembler")),
  Bin.

mk_code_binary(AccHCode) ->
  list_to_binary(words32towords8(AccHCode)).

words32towords8(List) ->
  lists:foldr(fun word32towords8/2, [], List).

word32towords8(X1, Acc) ->
  X2 = X1 bsr 8,
  X3 = X2 bsr 8, 
  X4 = X3 bsr 8,
  [X4, (X3 band 16#ff), X2 band 16#ff, X1 band 16#ff | Acc].

mk_labelmap(Map, ExportMap) ->
  %% msg("Map: ~w Map\n",[Map]),
  LblMap = lists:flatten(mk_labelmap(Map, ExportMap, [])),
  %% msg("LblMap: ~w Map\n",[LblMap]),
  LblMap.

mk_labelmap([{MFA, Labels}| Rest], ExportMap, Acc) ->
  Map = 
    lists:map(
      fun 
	({L,Pos}) ->
	  {Pos,find_offset({MFA,L}, ExportMap)};
	({sorted,Base,OrderedLabels}) ->
	  {sorted, Base, [{Order, find_offset({MFA,L}, ExportMap)}
			  || {L,Order} <- OrderedLabels]
	  }
      end,
      Labels),
  %% msg("Map: ~w Map\n",[Map]),
  mk_labelmap(Rest, ExportMap, [Map,Acc]);
mk_labelmap([],_,Acc) -> Acc.

find_offset({MFA,L},[{{MFA,L},hot,Adr}|_Rest]) ->
  Adr;
find_offset(L,[_|Rest]) ->
  find_offset(L,Rest);
find_offset(L,[]) ->
  ?EXIT({label_not_found,L}).


slim_exportmap(Map, Closures, Exports) ->
  SortedMap = lists:sort(slim_exportmap1(Map, [])),
  slim_sorted_exportmap(SortedMap, Closures, Exports).

slim_exportmap1([{{{M,F,A},entry},hot,Adr}|Rest], Acc) ->
  slim_exportmap1(Rest, [{Adr,M,F,A}|Acc]);
slim_exportmap1([_|Rest], Acc) ->
  slim_exportmap1(Rest, Acc);
slim_exportmap1([], Acc) ->
  Acc.

slim_sorted_exportmap([{Addr,M,F,A}|Rest], Closures, Exports) ->
  IsClosure = lists:member({M,F,A}, Closures),
  IsExported = is_exported(F, A, Exports),
  [Addr,M,F,A,IsClosure,IsExported | slim_sorted_exportmap(Rest, Closures, Exports)];
slim_sorted_exportmap([],_,_) -> [].

is_exported(F, A, Exports) -> lists:member({F,A}, Exports).

%%---------------------------------------------------------------------
%% assemble_instr(Instr)
%%     Returns: 	
%%   Arguments:	Instr - The complete instruction.
%% Description:	Returns the 32-bit SPARC code for the instruction Instr. 
%%---------------------------------------------------------------------

assemble_instr(I) ->
  case I of 
%%  #move{} ->
%%    assemble_move(I);
    #alu{} ->     
      assemble_alu(I);
%%  #alu_cc{} ->
%%    assemble_alu_cc(I);
%%  #store{} ->
%%     assemble_store(I);
%%  #load{} ->
%%    assemble_load(I);
    #goto{} ->
      assemble_goto(I);
    #b{} ->
      assemble_b(I);
%%  #br{} ->
%%    assemble_br(I);
    #call_link{} ->
      assemble_call_link(I);
    #jmp_link{} ->
      assemble_jmp_link(I);
%%  #jmp{} ->
%%    assemble_jmp(I);
    #sethi{} ->
      assemble_sethi(I)
%%  #load_fp{} ->
%%    assemble_load_fp(I);
%%  #store_fp{} ->
%%    assemble_store_fp(I);
%%  #fb{} ->
%%    assemble_fb(I);
%%  #fop{} ->
%%    assemble_fop(I);
%%  #fcmp{} ->
%%    assemble_fcmp(I);
%%  #fmove{} ->
%%    assemble_fmove(I);
%%  #conv_fp{} ->
%%    assemble_conv_fp(I);
%%  #nop{} ->
%%    assemble_nop(I)
  end.

check_simm13(Val,I) ->
  case hipe_sparc:is_imm(Val) of
    true -> 
      V = hipe_sparc:imm_value(Val),
      if 
	V > 4095 -> %% 12 bits
	  exit([{problem,{too_big_imm,Val}},{at,I}]);
	V < -4096 ->
	  exit([{problem,{too_small_imm,Val}},{at,I}]);
	true ->
	  true
      end;
    _ -> true
  end.

check_imm22(Val,I) ->
  case hipe_sparc:is_imm(Val) of
    true -> 
      V = hipe_sparc:imm_value(Val),
      if 
	V > 4194303 ->  %% 22 bits
	  exit([{problem,{too_big_imm,Val}},{at,I}]);
	V < -4194303 ->
	  exit([{problem,{too_small_imm,Val}},{at,I}]);
	true ->
	  true
      end;
    _ -> true
  end.

%%     Src Dst
%% mov reg reg  
%% mov imm reg
assemble_move(Instr) ->
  % move is a syntetic instruction, implemented with 'or'.
  Dst = hipe_sparc:reg_nr(hipe_sparc:move_dest(Instr)),
  Src = hipe_sparc:move_src(Instr),

  %% Check if the source is a register.
  case hipe_sparc:is_reg(Src) of
    false ->
      %% Check if source is an immediate.
      case hipe_sparc:is_imm(Src) of
	true -> 
	  check_simm13(Src,Instr), %% Throws exception...
	  hipe_sparc_op:ori(0,hipe_sparc:imm_value(Src),Dst);
	_ -> exit([{problem,{not_handled_operand,Src}},{at,Instr}])
      end;
    true ->
      hipe_sparc_op:or_op(0,hipe_sparc:reg_nr(Src),Dst)
  end.

%%     Dst Src1 AluOp Src2
%% alu reg reg  op    reg
%% alu reg reg  op    imm
assemble_alu(Instr) ->
  Dst = hipe_sparc:reg_nr(hipe_sparc:alu_dest(Instr)),
  Src1 = hipe_sparc:reg_nr(hipe_sparc:alu_src1(Instr)),
  SymSrc2 = hipe_sparc:alu_src2(Instr),
  AluOp = hipe_sparc:alu_operator(Instr),
  case hipe_sparc:is_reg(SymSrc2) of
    false -> 
      case hipe_sparc:is_imm(SymSrc2) of
	true -> 
	  check_simm13(SymSrc2,Instr), %% Throws exception...
	  Src2 = hipe_sparc:imm_value(SymSrc2),
	  case AluOp of
	    '+' -> hipe_sparc_op:addi(Src1,Src2,Dst);
	    '-' -> hipe_sparc_op:subi(Src1,Src2,Dst);
	    '+c' -> hipe_sparc_op:addci(Src1,Src2,Dst);
	    '-c' -> hipe_sparc_op:subci(Src1,Src2,Dst);
	    'and' -> hipe_sparc_op:andi(Src1,Src2,Dst);
	    'andn' -> hipe_sparc_op:andni(Src1,Src2,Dst);
	    'or' -> hipe_sparc_op:ori(Src1,Src2,Dst);
	    'xor' -> hipe_sparc_op:xori(Src1,Src2,Dst);
	    'xnor' -> hipe_sparc_op:xnori(Src1,Src2,Dst);
	    '>>' -> hipe_sparc_op:srli(Src1,Src2,Dst);
	    '>>64' -> hipe_sparc_op:srlix(Src1,Src2,Dst);
	    '>>?' -> hipe_sparc_op:srai(Src1,Src2,Dst);
	    '>>?64' -> hipe_sparc_op:sraix(Src1,Src2,Dst);
	    '<<' -> hipe_sparc_op:slli(Src1,Src2,Dst);
	    '<<64' -> hipe_sparc_op:sllix(Src1,Src2,Dst);
	    'smul' -> hipe_sparc_op:smuli(Src1,Src2,Dst);
	    _ -> exit([{problem,{not_handled,{aluop,AluOp}}},{at,Instr}])
	  end;
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_operand,SymSrc2}},{at,Instr}])
      end;
	      
    true -> %% Reg2
      Src2 = hipe_sparc:reg_nr(SymSrc2),
      case AluOp of
	'+' -> hipe_sparc_op:add(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:sub(Src1,Src2,Dst);
	'+c' -> hipe_sparc_op:addc(Src1,Src2,Dst);
	'-c' -> hipe_sparc_op:subc(Src1,Src2,Dst);
	'and' -> hipe_sparc_op:and_op(Src1,Src2,Dst);
	'andn' -> hipe_sparc_op:andn(Src1,Src2,Dst);
	'or' -> hipe_sparc_op:or_op(Src1,Src2,Dst);
	'xor' -> hipe_sparc_op:xor_op(Src1,Src2,Dst);
	'xnor' -> hipe_sparc_op:xnor_op(Src1,Src2,Dst);
	'>>' -> hipe_sparc_op:srl(Src1,Src2,Dst);
	'>>?' -> hipe_sparc_op:sra(Src1,Src2,Dst);
	'<<' -> hipe_sparc_op:sll(Src1,Src2,Dst);
	'>>64' -> hipe_sparc_op:srlx(Src1,Src2,Dst);
	'>>?64' -> hipe_sparc_op:srax(Src1,Src2,Dst);
	'<<64' -> hipe_sparc_op:sllx(Src1,Src2,Dst);
	'smul' -> hipe_sparc_op:smul(Src1,Src2,Dst);
	_ -> exit([{problem,{not_handled,{aluop,AluOp}}},{at,Instr}])
      end
  end.

%%     Dst Src1 AluOp Src2
%% alu reg reg  op    reg
%% alu reg reg  op    imm
assemble_alu_cc(Instr) ->
  Dst = hipe_sparc:reg_nr(hipe_sparc:alu_cc_dest(Instr)),
  Src1 = hipe_sparc:reg_nr(hipe_sparc:alu_cc_src1(Instr)),
  SymSrc2 = hipe_sparc:alu_cc_src2(Instr),
  AluOp = hipe_sparc:alu_cc_operator(Instr),
  case hipe_sparc:is_reg(hipe_sparc:alu_cc_src2(Instr)) of
    false ->
      case hipe_sparc:is_imm(SymSrc2) of
	true -> 
	  check_simm13(SymSrc2,Instr), %% Throws exception...
	  Src2 = hipe_sparc:imm_value(SymSrc2),
	  case AluOp of
	    '+' -> hipe_sparc_op:addicc(Src1,Src2,Dst);
	    '-' -> hipe_sparc_op:subicc(Src1,Src2,Dst);
	    'and' -> hipe_sparc_op:andicc(Src1,Src2,Dst);
	    'andn' -> hipe_sparc_op:andnicc(Src1,Src2,Dst);
	    'or' -> hipe_sparc_op:oricc(Src1,Src2,Dst);
	    'xor' -> hipe_sparc_op:xoricc(Src1,Src2,Dst);
	    _ -> exit([{problem,{not_handled,{aluccop,AluOp}}},
		       {at,Instr}])
	  end;
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_operand,SymSrc2}},{at,Instr}])
      end;

    true ->
      Src2 = hipe_sparc:reg_nr(SymSrc2),
      case AluOp of
	'+' -> hipe_sparc_op:addcc(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:subcc(Src1,Src2,Dst);
	'and' -> hipe_sparc_op:andcc(Src1,Src2,Dst);
	'andn' -> hipe_sparc_op:andncc(Src1,Src2,Dst);
	'or' -> hipe_sparc_op:orcc(Src1,Src2,Dst);
	'xor' -> hipe_sparc_op:xorcc(Src1,Src2,Dst);
	_ -> exit([{problem,{not_handled,{aluccop,AluOp}}},{at,Instr}])
      end
  end.

%%       Src Off Dst
%% store reg reg reg
%% store reg imm reg
assemble_store(Store) ->
  Dst = hipe_sparc:reg_nr(hipe_sparc:store_dest(Store)),
  Off = hipe_sparc:store_off(Store),
  Type = hipe_sparc:store_type(Store),
  Src = hipe_sparc:reg_nr(hipe_sparc:store_src(Store)),
  case hipe_sparc:is_reg(Off) of 
    false -> 
      case hipe_sparc:is_imm(Off) of
	true -> 
	  check_simm13(Off,Store), %% Throws exception...
	  ImmOff = hipe_sparc:imm_value(Off),
	  case Type of
	    w -> hipe_sparc_op:stwi(Src,Dst,ImmOff);
	    b -> hipe_sparc_op:stbi(Src,Dst,ImmOff);
	    h -> hipe_sparc_op:sthi(Src,Dst,ImmOff);
	    x -> hipe_sparc_op:stxi(Src,Dst,ImmOff)
	  end;
	false -> %% Not register or immediate
	  exit([{problem,{not_handled_offset,Off}},{at,Store}])
      end;
    true -> 
      case Type of
	w -> hipe_sparc_op:stw(Src,Dst,hipe_sparc:reg_nr(Off));
	b -> hipe_sparc_op:stb(Src,Dst,hipe_sparc:reg_nr(Off));
	h -> hipe_sparc_op:sth(Src,Dst,hipe_sparc:reg_nr(Off));
	x -> hipe_sparc_op:stx(Src,Dst,hipe_sparc:reg_nr(Off))
      end
  end.

%%       Src Off Dst
%% load  reg reg reg
%% load  reg imm reg
assemble_load(Load) ->
  case hipe_sparc:is_reg(hipe_sparc:load_dest(Load)) of
    true ->
      case hipe_sparc:is_reg(hipe_sparc:load_src(Load)) of
	true ->
	  Dst = hipe_sparc:reg_nr(hipe_sparc:load_dest(Load)),
	  Src = hipe_sparc:reg_nr(hipe_sparc:load_src(Load)),
	  Off = hipe_sparc:load_off(Load),
	  Type = hipe_sparc:load_type(Load),
	  case hipe_sparc:is_reg(Off) of 
	    false -> 
	      case hipe_sparc:is_imm(Off) of
		true -> 
		  check_simm13(Off,Load), %% Throws exception...
		  ImmOff = hipe_sparc:imm_value(Off),
		  case Type of
		    uw -> hipe_sparc_op:ldi(Src,ImmOff,Dst);
		    sb -> hipe_sparc_op:ldsbi(Src,ImmOff,Dst);
		    sh -> hipe_sparc_op:ldshi(Src,ImmOff,Dst);
		    sw -> hipe_sparc_op:ldswi(Src,ImmOff,Dst);
		    ub -> hipe_sparc_op:ldubi(Src,ImmOff,Dst);
		    uh -> hipe_sparc_op:lduhi(Src,ImmOff,Dst);
		    x -> hipe_sparc_op:ldxi(Src,ImmOff,Dst)
		  end;
		false -> %% Not register or immediate
		  exit([{problem,{not_handled_offset,Off}},{at,Load}])
	      end;
	    true -> 
	      OffReg = hipe_sparc:reg_nr(Off),
	      case Type of
		uw -> hipe_sparc_op:ld(Src,OffReg,Dst);
		sb -> hipe_sparc_op:ldsb(Src,OffReg,Dst);
		sh -> hipe_sparc_op:ldsh(Src,OffReg,Dst);
		sw -> hipe_sparc_op:ldsw(Src,OffReg,Dst);
		ub -> hipe_sparc_op:ldub(Src,OffReg,Dst);
		uh -> hipe_sparc_op:lduh(Src,OffReg,Dst);
		x ->  hipe_sparc_op:ldx(Src,OffReg,Dst)
	      end
	  end;
	false ->
	  exit([{problem,load_src_not_reg},{at,Load}])
      end;
    false ->
      exit([{problem,load_dst_not_reg},{at,Load}])
  end.

%% goto is a synthetic instruction implemented with ba
%% we're only doing SPARC V9 now, so replace "ba" with "ba,pt"
assemble_goto(Goto) ->
  Disp = hipe_sparc:goto_label(Goto),
  Cond = hipe_sparc_op:cc_bits('a'),
  Pred = hipe_sparc_op:predicate_bit(true),
  Annul = hipe_sparc_op:annul_bit('na'),
  hipe_sparc_op:bpcc(Cond, Annul, Pred, Disp).

%%
%%
assemble_b(B) ->
  Disp = hipe_sparc:b_label(B),
  Cond = hipe_sparc_op:cc_bits(hipe_sparc:b_cond(B)),
  Pred = hipe_sparc_op:predicate_bit(hipe_sparc:b_taken(B)),
  Annul = hipe_sparc_op:annul_bit(hipe_sparc:b_annul(B)),
  hipe_sparc_op:bpcc(Cond, Annul, Pred, Disp).

%% assemble_br(BR) -> 
%%   Disp = hipe_sparc:br_label(BR),
%%   Reg = hipe_sparc:reg_nr(hipe_sparc:br_reg(BR)),
%%   RCond = hipe_sparc_op:rcc_bits(hipe_sparc:br_regcond(BR)),
%%   Pred = hipe_sparc_op:predicate_bit(hipe_sparc:br_taken(BR)),
%%   Annul = hipe_sparc_op:annul_bit(hipe_sparc:br_annul(BR)),
%%   hipe_sparc_op:bpr(RCond, Annul, Pred, Reg, Disp).

assemble_call_link(Call_link) ->
  case 
    hipe_sparc:reg_nr(hipe_sparc:call_link_link(Call_link)) =/= 
    hipe_sparc_registers:return_address() of
    true ->
      exit([{problem,{call_link_not_to_CP}},{at,Call_link}]);
    false ->
      hipe_sparc_op:call(hipe_sparc:call_link_target(Call_link))
  end.

assemble_jmp_link(Jmp_link) ->
  Target = hipe_sparc:reg_nr(hipe_sparc:jmp_link_target(Jmp_link)),
  Off = hipe_sparc:jmp_link_off(Jmp_link),
  Link = hipe_sparc:reg_nr(hipe_sparc:jmp_link_link(Jmp_link)),
  assemble_jlink(Jmp_link, Target, Off, Link).

assemble_jlink(I, Target, Off, Link) ->
  case hipe_sparc:is_reg(Off) of 
    true -> 
      exit([{problem,{not_handled,{jmp_link,reg_reg_reg}}},{at,I}]);
    false -> 
      case hipe_sparc:is_imm(Off) of
	true -> 
	  check_simm13(Off,I), %% Throws exception...
	  ImmOff = hipe_sparc:imm_value(Off),
	  hipe_sparc_op:jumpli(Target,ImmOff,Link);
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_offset,Off}},{at,I}])
      end
  end.  

assemble_jmp(Jmp)->
  Target = hipe_sparc:reg_nr(hipe_sparc:jmp_target(Jmp)),
  Link = hipe_sparc_registers:zero(),
  Off = hipe_sparc:jmp_off(Jmp),
  assemble_jlink(Jmp, Target, Off, Link).

assemble_rdy(Instr) ->
  Dest = hipe_sparc:reg_nr(hipe_sparc:rdy_dest(Instr)),
  hipe_sparc_op:rdy(Dest).

assemble_sethi(Instr)->
  Dest = hipe_sparc:reg_nr(hipe_sparc:sethi_dest(Instr)),
  Val = hipe_sparc:sethi_const(Instr),
  case hipe_sparc:is_imm(Val) of
    true -> 
      check_imm22(Val,Instr), %% Throws exception...
      ImmVal = hipe_sparc:imm_value(Val),
      hipe_sparc_op:sethi(ImmVal,Dest);
    false -> 
      exit([{problem,{not_an_integer,Val}},{at,Instr}])
  end.

assemble_nop(_Instr) ->
  hipe_sparc_op:nop().

%%---------------------------------------------------------------------
%% FP - ops
%%---------------------------------------------------------------------

%%          Src Off Dst
%% load_fp  reg reg fpreg
%% load_fp  reg imm fpreg
assemble_load_fp(Load) ->
  case hipe_sparc:is_fpreg(hipe_sparc:load_fp_dest(Load)) of
    true ->
      case hipe_sparc:is_reg(hipe_sparc:load_fp_src(Load)) of
	true ->
	  Type = hipe_sparc:load_fp_type(Load),
	  Dst = encode_5bit_fpreg(hipe_sparc:fpreg_nr(hipe_sparc:load_fp_dest(Load)),
				  Type),
	  Src = hipe_sparc:reg_nr(hipe_sparc:load_fp_src(Load)),
	  Off = hipe_sparc:load_fp_off(Load),
	  case hipe_sparc:is_reg(Off) of 
	    false -> 
	      case hipe_sparc:is_imm(Off) of
		true -> 
		  check_simm13(Off,Load), %% Throws exception...
		  ImmOff = hipe_sparc:imm_value(Off),
		  case Type of
		    single ->  hipe_sparc_op:ldfi(Dst,Src,ImmOff);
		    double ->  hipe_sparc_op:lddfi(Dst,Src,ImmOff);
		    quad ->  hipe_sparc_op:ldqfi(Dst,Src,ImmOff)
		  end;
		false -> %% Not reg or imm
		  exit([{problem,{not_handled_offset,Off}},{at,Load}])
	      end;
	    true ->
	      case Type of
		single -> hipe_sparc_op:ldf(Dst,Src,hipe_sparc:reg_nr(Off));
		double -> hipe_sparc_op:lddf(Dst,Src,hipe_sparc:reg_nr(Off));
		  quad -> hipe_sparc_op:ldqf(Dst,Src,hipe_sparc:reg_nr(Off))
	      end
	  end;
	false ->
	  exit([{problem,load_fp_src_not_reg},{at,Load}])
      end;
    false ->
      exit([{problem,load_fp_dst_not_fpreg},{at,Load}])
  end.

%%          Src   Off Dst
%% store_fp fpreg reg reg
%% store_fp reg   imm reg
assemble_store_fp(Store) ->
  Dst = hipe_sparc:reg_nr(hipe_sparc:store_fp_dest(Store)),
  Off = hipe_sparc:store_fp_off(Store),
  Type = hipe_sparc:store_fp_type(Store),
  Src = encode_5bit_fpreg(hipe_sparc:fpreg_nr(hipe_sparc:store_fp_src(Store)), 
			  Type),
  case hipe_sparc:is_reg(Off) of 
    false -> 
      case hipe_sparc:is_imm(Off) of
	true -> 
	  check_simm13(Off,Store), %% Throws exception...
	  ImmOff = hipe_sparc:imm_value(Off),
	  case Type of
	    single -> hipe_sparc_op:stfi(Src,Dst,ImmOff);
	    double -> hipe_sparc_op:stdfi(Src,Dst,ImmOff);
	    quad ->   hipe_sparc_op:stqfi(Src,Dst,ImmOff)
	  end;
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_offset,Off}},{at,Store}])
      end;
    true -> 
      case Type of
	single -> hipe_sparc_op:stf(Src,Dst,hipe_sparc:reg_nr(Off));
	double -> hipe_sparc_op:stdf(Src,Dst,hipe_sparc:reg_nr(Off));
	quad ->   hipe_sparc_op:stqf(Src,Dst,hipe_sparc:reg_nr(Off))
      end
  end.

%% %% fb<Cond>[,a][,pt] <%fccN>,<L>
%% %%
%% assemble_fb(B) ->
%%   Disp = hipe_sparc:fb_label(B),
%%   Cond = hipe_sparc_op:fcc_bits(hipe_sparc:fb_cond(B)),
%%   Pred = hipe_sparc_op:predicate_bit(hipe_sparc:fb_taken(B)),
%%   Annul = hipe_sparc_op:annul_bit(hipe_sparc:fb_annul(B)),
%%   Fcc = hipe_sparc:fb_fcc_reg(B),
%%   hipe_sparc_op:fbpfcc(Fcc, Cond, Annul, Pred, Disp).


%%     Dst   Src1   FOp Src2
%%     fpreg fpreg  op  fpreg
assemble_fop(Instr) ->
  D = hipe_sparc:fpreg_nr(hipe_sparc:fop_dest(Instr)),
  S1 = hipe_sparc:fpreg_nr(hipe_sparc:fop_src1(Instr)),
  S2 = hipe_sparc:fpreg_nr(hipe_sparc:fop_src2(Instr)),
  Type = hipe_sparc:fop_type(Instr),
  Src1 = encode_5bit_fpreg(S1, Type),
  Src2 = encode_5bit_fpreg(S2, Type),
  Dst = encode_5bit_fpreg(D, Type),
  
  FOp = hipe_sparc:fop_operator(Instr),

  case Type of
    double ->
      case FOp of
	'+' -> hipe_sparc_op:faddd(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:fsubd(Src1,Src2,Dst);
	'*' -> hipe_sparc_op:fmuld(Src1,Src2,Dst);
	'/' -> hipe_sparc_op:fdivd(Src1,Src2,Dst);
	_ -> ?EXIT([{problem,{not_handled,{fop,FOp}}},
		    {at,Instr}])
      end;
    single ->
      case FOp of
	'+' -> hipe_sparc_op:fadds(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:fsubs(Src1,Src2,Dst);
	'*' -> hipe_sparc_op:fmuls(Src1,Src2,Dst);
	'/' -> hipe_sparc_op:fdivs(Src1,Src2,Dst);
	_ -> ?EXIT([{problem,{not_handled,{fop,FOp}}},
		    {at,Instr}])
      end;
    quad ->
      case FOp of
	'+' -> hipe_sparc_op:faddq(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:fsubq(Src1,Src2,Dst);
	'*' -> hipe_sparc_op:fmulq(Src1,Src2,Dst);
	'/' -> hipe_sparc_op:fdivq(Src1,Src2,Dst);
	_ -> ?EXIT([{problem,{not_handled,{fop,FOp}}},
		    {at,Instr}])
      end
  end.
      
%% %%     Dst   Src1   Src2
%% %%     fccN  fpreg  fpreg
%% assemble_fcmp(Instr) ->
%%   Fcc = hipe_sparc:fcmp_fcc_reg(Instr),
%%   Src1 = hipe_sparc:fpreg_nr(hipe_sparc:fcmp_src1(Instr)),
%%   Src2 = hipe_sparc:fpreg_nr(hipe_sparc:fcmp_src2(Instr)),
%%   Exception = hipe_sparc:fcmp_exception(Instr),
%%   Type = hipe_sparc:fcmp_type(Instr),
%%   %% XXX: Is this the right way to code the regs?
%%   RS1 = encode_5bit_fpreg(Src1, Type),
%%   RS2 = encode_5bit_fpreg(Src2, Type),
%%   case Type of
%%     double ->
%%       case Exception of
%% 	true -> hipe_sparc_op:fcmped(Fcc,RS1,RS2);
%% 	false -> hipe_sparc_op:fcmpd(Fcc,RS1,RS2)
%%       end;
%%     single ->
%%       case Exception of
%% 	true -> hipe_sparc_op:fcmpes(Fcc,RS1,RS2);
%% 	false -> hipe_sparc_op:fcmps(Fcc,RS1,RS2)
%%       end;
%%     quad ->
%%       case Exception of
%% 	true -> hipe_sparc_op:fcmpeq(Fcc,RS1,RS2);
%% 	false -> hipe_sparc_op:fcmpq(Fcc,RS1,RS2)
%%       end
%%   end.

%%     Dst   Src
%%     fpreg fpreg
assemble_fmove(Instr) ->
  D = hipe_sparc:fpreg_nr(hipe_sparc:fmove_dest(Instr)),
  S = hipe_sparc:fpreg_nr(hipe_sparc:fmove_src(Instr)),
  Type = hipe_sparc:fmove_type(Instr),
  Neg = hipe_sparc:fmove_negate(Instr),
  Abs = hipe_sparc:fmove_abs(Instr),
  Src = encode_5bit_fpreg(S, Type),
  Dst = encode_5bit_fpreg(D, Type),
  
  case Type of
    double ->
      case {Abs, Neg} of
	{true, false} -> hipe_sparc_op:fabsd(Src,Dst);
	{false, true} -> hipe_sparc_op:fnegd(Src,Dst);
	{false, false} -> hipe_sparc_op:fmovd(Src,Dst);
	_ -> ?EXIT([{problem,{not_handled,{negate, absolute_value}}},
		    {at,Instr}])
      end;
    single ->
      case {Abs, Neg} of
	{true, false} -> hipe_sparc_op:fabss(Src,Dst);
	{false, true} -> hipe_sparc_op:fnegs(Src,Dst);
	{false, false} -> hipe_sparc_op:fmovs(Src,Dst);
	_ -> ?EXIT([{problem,{not_handled,{negate,Neg,absolute_value,Abs}}},
		    {at,Instr}])
      end;
    quad ->
      case {Abs, Neg} of
	{true, false} -> hipe_sparc_op:fabsq(Src,Dst);
	{false, true} -> hipe_sparc_op:fnegq(Src,Dst);
	{false, false} -> hipe_sparc_op:fmovq(Src,Dst);
	_ -> ?EXIT([{problem,{not_handled,{negate, absolute_value}}},
		    {at,Instr}])
      end
  end.

%%     Dst   Src
%%     fpreg reg
assemble_conv_fp(Instr) ->
  D = hipe_sparc:fpreg_nr(hipe_sparc:conv_fp_dest(Instr)),
  S = hipe_sparc:fpreg_nr(hipe_sparc:conv_fp_src(Instr)),
  DestType = hipe_sparc:conv_fp_dest_type(Instr),
  Dst = encode_5bit_fpreg(D, DestType),
  Src = encode_5bit_fpreg(S, single),
  
  case DestType of
    single ->
      hipe_sparc_op:fitos(Src, Dst);
    double ->
      hipe_sparc_op:fitod(Src, Dst);
    quad ->
      hipe_sparc_op:fitoq(Src, Dst)
  end.

%%---------------------------------------------------------------------
%% 
encode_5bit_fpreg(Reg, Type) ->
  case Type of
    single -> 
      ?ASSERT(Reg < 32),
      Reg;
    double -> 
      ?ASSERT(Reg < 63),
      ?ASSERT(Reg band 2#1 =:= 0),
      ((Reg bsr 5) bor (Reg band 2#11110));
    quad ->
      ?ASSERT(Reg < 63),
      ?ASSERT(Reg band 2#11 =:= 0),
      ((Reg bsr 5) bor (Reg band 2#11110))
  end.

%% TODO: implement if needed:
%% encode_6bit_fpreg(Reg, Type)

%%=====================================================================
%% A linker for SPARC code appears below
%%=====================================================================

linker(Code, Map, ConstMap) ->
  link9(Code,0,
	init_export_map(Map),      %% Convert to more efficient
	init_const_map(ConstMap),  %% data structures.
	[],[]).

link9([{MFA,Hot}|Rest], HAddr, Map, ConstMap,
      AccHCode, AccHRefs) ->
  %% io:format("Assembling ~w\n",[MFA]),
  {HCode,NewHAddr,HRefs} =
    link8(Hot , MFA, HAddr, local_labels(MFA,Map), ConstMap, AccHRefs, []),
  link9(Rest, NewHAddr, Map, ConstMap, AccHCode++HCode, HRefs);
link9([],_HAddr,_Map,_ConstMap,AccHCode,AccHRefs) ->
  {AccHCode,AccHRefs}.

link8([I|Is],MFA,Addr,Map,ConstMap,Refs,Code) ->
  case  
    case I of
      #call_link{} -> resolve_call_link(I,Addr,Refs,Map);
      #b{} -> resolve_b(I,Addr,Refs,Map);
      #goto{} -> resolve_goto(I,Addr,Refs,Map);
      #load_address{} -> resolve_load_address(I,Addr,Refs,MFA,Map,ConstMap);
      #load_atom{} -> resolve_load_atom(I,Addr,Refs);
%%    #load_word_index{} -> resolve_load_word_index(I,Addr,Refs,MFA,Map,ConstMap);
      #alu{} -> {assemble_alu(I),Addr+4,Refs};
      #alu_cc{} -> {assemble_alu_cc(I),Addr+4,Refs};
      #store{} -> {assemble_store(I),Addr+4,Refs};
      #move{} -> {assemble_move(I),Addr+4,Refs};
      #load{} -> {assemble_load(I),Addr+4,Refs};
      #store_fp{} -> {assemble_store_fp(I),Addr+4,Refs};
      #load_fp{} -> {assemble_load_fp(I),Addr+4,Refs};
%%    #fb{} -> {assemble_fb(I),Addr+4,Refs};
      #fop{} -> {assemble_fop(I),Addr+4,Refs};
%%    #fcmp{} -> {assemble_fcmp(I),Addr+4,Refs};
      #fmove{} -> {assemble_fmove(I),Addr+4,Refs};
      #conv_fp{} -> {assemble_conv_fp(I),Addr+4,Refs};
      #jmp{} -> {assemble_jmp(I),Addr+4,Refs};
      #nop{} -> {assemble_nop(I),Addr+4,Refs};
      #rdy{} -> {assemble_rdy(I),Addr+4,Refs};
      #sethi{} -> {assemble_sethi(I),Addr+4,Refs};
      Other -> exit({bad_sparc_instruction,Other})
    end of
    {[I1,I2],NewAddr,NewRefs} ->
      link8(Is,MFA,NewAddr,Map,ConstMap,NewRefs,[I1,I2|Code]);
    {C,NewAddr,NewRefs}  ->
      link8(Is,MFA,NewAddr,Map,ConstMap,NewRefs,[C|Code])
  end;
link8([],_MFA,Addr,_Map,_ConstMap,Refs,Code) ->
  {lists:reverse(Code),Addr,Refs}.

resolve_load_address(Instr,Addr,Refs,MFA,_Map,ConstMap)->
  Dest = hipe_sparc:load_address_dest(Instr),
  Address = hipe_sparc:load_address_address(Instr),
  Ref = 
    case hipe_sparc:load_address_type(Instr) of
%%      label ->
%%	{hot,Offset} = find(Address, Map),
%%	[{?LOAD_ADDRESS,Addr,{label,Offset}}];
      local_function -> 
	[{?LOAD_ADDRESS,Addr,{local_function,Address}}];
      remote_function -> 
	[{?LOAD_ADDRESS,Addr,{remote_function,Address}}];
      constant ->
	ConstNo = find_const({MFA,Address},ConstMap),
	[{?LOAD_ADDRESS,Addr,{constant,ConstNo}}];
      closure ->
	[{?LOAD_ADDRESS,Addr,{closure,Address}}];
      c_const ->
	[{?LOAD_ADDRESS,Addr,{c_const,Address}}];
      Type ->     
	exit([{problem,{not_handled,{address,Type}}},{at,Instr}])
    end,
  Hi = hipe_sparc:mk_imm(0),
  Lo = hipe_sparc:mk_imm(0),
  I1 = hipe_sparc:sethi_create(Dest,Hi),
  I2 = hipe_sparc:alu_create(Dest,Dest,'or',Lo),
  {[assemble_instr(I2),assemble_instr(I1)],Addr+8,Ref++Refs}.

resolve_goto(I,Addr,Refs,Map) ->
  Dest = hipe_sparc:goto_label(I),
  {hot,Address} = find(Dest,Map),
  RelDest = (Address - Addr) div 4,
  case valid_branch_length(RelDest) of
    true -> true;
    false -> exit({too_long_branch,{address,Addr}})
  end,
  NewI = hipe_sparc:goto_label_update(I,RelDest),
  Code = assemble_instr(NewI),
  {Code,Addr+4,Refs}.

resolve_b(I,Addr,Refs,Map) ->
  Dest = hipe_sparc:b_label(I),
  {hot,Address} = find(Dest,Map),
  RelDest = (Address - Addr) div 4,
  case valid_branch_length(RelDest) of
    true -> true;
    false -> exit({too_long_branch,{address,Addr}})
  end,
  NewI = hipe_sparc:b_label_update(I,RelDest),
  Code = assemble_instr(NewI),
  {Code,Addr+4,Refs}.

resolve_load_atom(Instr,Addr,Refs)->
  Atom = hipe_sparc:load_atom_atom(Instr),
  Dest = hipe_sparc:load_atom_dest(Instr),
  I1 = hipe_sparc:sethi_create(Dest, hipe_sparc:mk_imm(0)),
  I2 = hipe_sparc:alu_create(Dest, Dest, 'or', hipe_sparc:mk_imm(0)),
  {[assemble_instr(I2),assemble_instr(I1)],
   Addr+8,[{?LOAD_ATOM, Addr, Atom}|Refs]}.

%%resolve_load_word_index(_Instr,_Addr,_Refs,_MFA,_Map,_ConstMap) ->
%%  ?EXIT({nyi,resolve_load_word_index}).
%%  Index =hipe_sparc:load_word_index_index(Instr),
%%  Block =hipe_sparc:load_word_index_block(Instr),
%%  Dest = hipe_sparc:load_word_index_dest(Instr),
%%  ConstNo = find_const({MFA,Block},ConstMap),
%%  I1 = hipe_sparc:sethi_create(Dest, hipe_sparc:mk_imm(0)),
%%  I2 = hipe_sparc:alu_create(Dest,Dest, 'or', hipe_sparc:mk_imm(0)),
%%  {[assemble_instr(I2),assemble_instr(I1)],
%%   Addr+8,[{?PATCH_TYPE2EXT(load_word_index),Addr, {word_index, ConstNo, Index}}|Refs]}.

resolve_call_link(Instr,Addr,OldRefs,Map)->
  Target = hipe_sparc:call_link_target(Instr),
  ExnLab = hipe_sparc:call_link_fail(Instr),
  %% Get the stack descriptor information
  SD = hipe_sparc:call_link_stack_desc(Instr),
  FSize = hipe_sparc:sdesc_size(SD),
  Live = list_to_tuple(hipe_sparc:sdesc_live_slots(SD)),
  Arity = case hipe_sparc:sdesc_arity(SD) of
	    N when N > ?SPARC_NR_ARG_REGS -> N - ?SPARC_NR_ARG_REGS;
	    _ -> 0
	  end,
  ExnRA = case ExnLab of
	    [] -> [];	% don't cons up a new one
	    _ -> {hot,V} = find(ExnLab,Map), V
	  end,
  %% The stack descriptor needs to be inserted into the system at load-time.
  Refs = [{?SDESC,
	   Addr,
	   ?STACK_DESC(ExnRA, FSize, Arity, Live)} | OldRefs],
  case hipe_sparc:call_link_is_known(Instr) of
    false -> 
      NewI = hipe_sparc:jmp_link_create(
	       hipe_sparc:call_link_target(Instr),
	       hipe_sparc:mk_imm(0), 
	       hipe_sparc:mk_reg(hipe_sparc_registers:return_address()),
	       hipe_sparc:call_link_args(Instr)),
      Code = assemble_instr(NewI),
      {Code,Addr+4, Refs};
    true -> 
      Patch =
	case hipe_sparc:call_link_type(Instr) of
	  remote -> ?CALL_REMOTE;
	  not_remote -> ?CALL_LOCAL
	end,
      NewRefs = [{Patch,Addr,Target} | Refs],
      NewI = hipe_sparc:call_link_target_update(Instr,0),
      Code = assemble_instr(NewI),
      {Code,Addr+4,NewRefs}
  end.

%% ------------------------------------------------------------------
  
get_code(CompiledCode) -> % -> {CodeSize,ExportMap,NewCode}
  get_code(CompiledCode, 0, [], []).

get_code([{MFA,Insns,_Data}|Rest], Address, Map, AccCode) ->
  {NewInsns,NewAddress,NewMap} = preprocess(Insns,MFA,Address,Map),
  get_code(Rest, NewAddress, NewMap, [{MFA,NewInsns}|AccCode]);
get_code([], Address, Map, AccCode) ->
  {Address,Map,lists:reverse(AccCode)}.

preprocess(Entry, MFA, Address, Map) ->
  %% io:format("~w at ~w ~w\n",[MFA,Address]),
  process_instr(Entry, Address,
		[{{MFA,entry},hot,Address}|Map],
		MFA, []).

process_instr([I|Is], Address, Map, MFA, AccCode) ->
  case I of
    #label{} ->
      process_instr(Is, Address,
		    [{{MFA,hipe_sparc:label_name(I)},hot,Address}|Map],
		    MFA, AccCode);
    #comment{} ->
      process_instr(Is, Address, Map, MFA, AccCode);
    #load_address{} ->
      process_instr(Is, Address+8, Map, MFA, [I|AccCode]);
    #load_atom{} ->
      process_instr(Is, Address+8, Map, MFA, [I|AccCode]);
    #load_word_index{} ->
      process_instr(Is, Address+8, Map, MFA, [I|AccCode]);
    _Other ->
      process_instr(Is, Address+4, Map, MFA, [I|AccCode])
  end;
process_instr([], Address, Map, _MFA, AccCode) ->
  {lists:reverse(AccCode),Address,Map}.

%% ------------------------------------------------------------------
%%
%% Constant map
%%

find_const(Name, Tree) ->
  case gb_trees:lookup(Name,Tree) of
    {value,V} -> V;
    none -> ?EXIT({could_not_find_constant, Name, Tree})
  end.

init_const_map(List) ->
  init_const_map(List, gb_trees:empty()).
init_const_map([{pcm_entry,MFA,Label,ConstNo,_,_,_} | List], Tree) ->
  init_const_map(List,gb_trees:insert({MFA,Label}, ConstNo ,Tree));
init_const_map([], Tree) ->
  Tree.

%% ------------------------------------------------------------------
%%
%% Label map: represented with double gbtrees.
%%

local_labels(MFA, Map) ->
  case gb_trees:lookup(MFA, Map) of
    {value,T} -> T;
    none -> ?EXIT({mfa_not_in_map,MFA,Map})
  end.
  
find(L, Tree) ->
  case gb_trees:lookup(L, Tree) of
    {value,V} -> V;
    none -> ?EXIT({label_not_in_map,L,Tree})
  end.

init_export_map(List) ->
  init_export_map(List, gb_trees:empty()).

init_export_map([{{M,L},Seg,Addr}|List], Tree) ->
  init_export_map(List, init_m(M, L, {Seg,Addr}, Tree));
init_export_map([], Tree) -> Tree.

init_m(M, L, Data, Tree) ->
  case gb_trees:lookup(M, Tree) of
    {value,T2} ->
      gb_trees:update(M, gb_trees:insert(L, Data, T2), Tree);
    none ->
      gb_trees:insert(M, gb_trees:insert(L, Data, gb_trees:empty()), Tree)
  end.

%% %%--------------------------------------------------------------------
%% %%
%% %% check_immediates asserts that all immediates are less than 13 bits
%% %% Returns true if the code is ok.
%% %% Exception: {'EXIT',{immediate_too_large, [{Instr1,[Imm1,Imm2]},{Instr2,[Imm3|...]}|...]}}
%% %%  if Imm1 and Imm2 are too large.
%% %%
%% check_immediates(Instrs) -> check_immediates(Instrs,[]).
%% 
%% check_immediates([],[]) -> true;
%% check_immediates([],Problems) -> exit({immediate_too_large,Problems});
%% check_immediates([Instr|Rest],Problems) ->
%%   case hipe_sparc:is_sethi(Instr) of
%%     true -> check_immediates(Rest,Problems);
%%     _ -> 
%%       case check_imm(hipe_sparc:imm_uses(Instr),[]) of
%%      true ->
%%        check_immediates(Rest,Problems);
%%      MoreProblems -> 
%%        check_immediates(Rest,[{Instr,MoreProblems}|Problems])
%%       end
%%   end.
%% 
%% check_imm([],[]) -> true;
%% check_imm([],Problems) -> Problems;
%% check_imm([Use|Rest],Problems) ->
%%   case hipe_sparc:is_imm(Use) of 
%%     true -> 
%%       Val = hipe_sparc:imm_value(Use),
%%       if 
%%      Val > 4095 -> %% Largest possible immediate.
%%        check_imm(Rest,[Val|Problems]);
%%      true -> 
%%        check_imm(Rest,Problems)
%%       end;
%%     _ ->     
%%       check_imm(Rest,Problems)
%%   end.

%% ------------------------------------------------------------------
%% valid_branch_length(relativeDestination) 
%%  Validates that the destination is within reach.
%%
%% XXX: this only works for disp19-type branches.
%% XXX: update for disp16 (BPr) and disp22 (Bicc) type branches
%%

valid_branch_length(Dest) ->
  if (abs(Dest) band (16#FFF80000)) > 0 ->
      false;
    true -> true
  end.

