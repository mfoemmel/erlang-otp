%% ====================================================================
%%  Filename : 	hipe_sparc_assemble.erl
%%  Module   :	hipe_sparc_assemble
%%  Purpose  :  To write SPARC-code to memory. 
%%  Notes    :  Not all SPARC instructions are handled.
%%  History  :	* 1998-06-18 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: happi $
%%    $Date: 2002/05/13 10:13:07 $
%%    $Revision: 1.19 $
%% ====================================================================
%% Exported functions (short description):
%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_assemble).
-export([assemble_and_write/1,assemble/2, assemble_instr/2]).
-export([assemble_move/1,
	 assemble_alu/1,
	 assemble_alu_cc/1,
	 assemble_store/1,
	 assemble_load/1,
	 assemble_goto/1,
	 assemble_b/1,
	 assemble_br/1,
	 assemble_call_link/1,
	 assemble_jmp_link/1,
	 assemble_jmp/1,
	 assemble_sethi/1,
	 assemble_store_fp/1,
	 assemble_load_fp/1,
	 assemble_fb/1,
	 assemble_fop/1,
	 assemble_fmov/1,
	 assemble_fcmp/1,
	 assemble_conv_fp/1,
	 assemble_nop/1]).

-define(DO_ASSERT,true).
-include("../main/hipe.hrl").


%% ____________________________________________________________________
%%  assemble_and_write(CodeList)    
%% Returns: 	true
%%              (Or exits...)
%% Arguments:	CodeList - a list of tuples {Code,Address}
%%              where Code is pure sparc-code and Address
%%              is the memory address to write the code to.
%% Description:	 
%% ____________________________________________________________________
assemble_and_write([{Code,Address}|Rest]) ->
  assemble(Code,Address),
  assemble_and_write(Rest);
assemble_and_write([]) -> true.

assemble([Instr|Rest],Address) -> 

  case hipe_sparc:type(Instr) of
    % Comments and labels generates no code.
    comment -> 
      %% io:format("          ",[]),
      %% pp_instr(Instr,Address),
      assemble(Rest,Address);
    label ->   
      assemble(Rest,Address);
    Type ->   
      % Assemble the instruction and write it to memory.
      write(Address,
	    case catch(assemble_instr(Type,Instr)) of
	      {'EXIT',R} ->
	       io:format("I: ~w\n",[Instr]),
	       exit(R);
	      O -> O
	    end),
      assemble(Rest,Address+4) % Each instruction is 4 bytes.
  end;
assemble([],_) -> true.



%% ____________________________________________________________________
%%  assemble_instr(Type,Instr)    
%% Returns: 	
%% Arguments:	Type - The type of the instruction.
%%              Instr - The complete instruction.
%% Description:	Returns the 32-bit sparccode for the instruction Instr. 
%% ____________________________________________________________________

assemble_instr(Type, I) ->
  case Type of 
    move ->
      assemble_move(I);
    alu ->     
      assemble_alu(I);
    alu_cc ->
      assemble_alu_cc(I);
    store ->
      assemble_store(I);
    load ->
      assemble_load(I);
    goto ->
      assemble_goto(I);
    b ->
      assemble_b(I);
    br ->
      assemble_br(I);
    call_link ->
      assemble_call_link(I);
    jmp_link ->
      assemble_jmp_link(I);
    jmp ->
      assemble_jmp(I);
    sethi ->
      assemble_sethi(I);
    load_fp ->
      assemble_load_fp(I);
    store_fp ->
      assemble_store_fp(I);
    fb ->
      assemble_fb(I);
    fop ->
      assemble_fop(I);
    fcmp ->
      assemble_fcmp(I);
    fmov ->
      assemble_fmov(I);
    conv_fp ->
      assemble_conv_fp(I);
    nop ->
      assemble_nop(I);

    T ->
      exit([{problem,{not_handled,instruction, T}},{at,I}])
  end.
	 
check_simm13(Val,I) ->
  case hipe_sparc:is_imm(Val) of
    true -> 
      V = hipe_sparc:imm_value(Val),
      if 
	V > 4095 -> %% 12 bits
	  exit([{problem,{to_big_imm,Val}},{at,I}]);
	V < -4096 ->
	  exit([{problem,{to_small_imm,Val}},{at,I}]);
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
	  exit([{problem,{to_big_imm,Val}},{at,I}]);
	V < -4194303 ->
	  exit([{problem,{to_small_imm,Val}},{at,I}]);
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

  %% Check if the sorce is a register.
  case hipe_sparc:is_reg(Src) of
    false ->
      %% Check if source is an imm.
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
	    _ -> exit([{problem,{not_handled,{aluop,AluOp}}},
		       {at,Instr}])
	  end;
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_operand,SymSrc2}},
		{at,Instr}])
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
	  exit([{problem,{not_handled_operand,SymSrc2}},
		{at,Instr}])
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
	_ -> exit([{problem,{not_handled,{aluccop,AluOp}}},
		   {at,Instr}])
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
	    
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_offset,Off}},
		{at,Store}])
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
		false -> %% Not reg or imm
		  exit([{problem,{not_handled_offset,Off}},
			{at,Load}])
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

assemble_br(BR) -> 
  Disp = hipe_sparc:br_label(BR),
  Reg = hipe_sparc:reg_nr(hipe_sparc:br_reg(BR)),
  RCond = hipe_sparc_op:rcc_bits(hipe_sparc:br_regcond(BR)),
  Pred = hipe_sparc_op:predicate_bit(hipe_sparc:br_taken(BR)),
  Annul = hipe_sparc_op:annul_bit(hipe_sparc:br_annul(BR)),
  hipe_sparc_op:bpr(RCond, Annul, Pred, Reg, Disp).

assemble_call_link(Call_link) ->
  case 
    hipe_sparc:reg_nr(hipe_sparc:call_link_link(Call_link)) =/= 
    hipe_sparc_registers:return_address() of
    true -> exit([{problem,{call_link_not_to_CP}},
		  {at,Call_link}]);
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
      exit([{problem,{not_handled,{jmp_link,reg_reg_reg}}},
	    {at,I}]);
    false -> 
      case hipe_sparc:is_imm(Off) of
	true -> 
	  check_simm13(Off,I), %% Throws exception...
	  ImmOff = hipe_sparc:imm_value(Off),
	  hipe_sparc_op:jumpli(Target,ImmOff,Link);
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_offset,Off}},
		{at,I}])
      end
  end.  

assemble_jmp(Jmp)->
  Target = hipe_sparc:reg_nr(hipe_sparc:jmp_target(Jmp)),
  Link = hipe_sparc_registers:zero(),
  Off = hipe_sparc:jmp_off(Jmp),
  assemble_jlink(Jmp, Target, Off, Link).


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


%% ____________________________________________________________________
%% FP - ops
%%

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
		    single ->  hipe_sparc_op:ldf(Dst,Src,hipe_sparc:reg_nr(Off));
		    double ->  hipe_sparc_op:lddf(Dst,Src,hipe_sparc:reg_nr(Off));
		    quad ->  hipe_sparc_op:ldqf(Dst,Src,hipe_sparc:reg_nr(Off))
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
  Src =
    encode_5bit_fpreg(hipe_sparc:fpreg_nr(hipe_sparc:store_fp_src(Store)), 
		      Type),
  case hipe_sparc:is_reg(Off) of 
    false -> 
      case hipe_sparc:is_imm(Off) of
	true -> 
	  check_simm13(Off,Store), %% Throws exception...
	  ImmOff = hipe_sparc:imm_value(Off),
	  case Type of
	    single ->  hipe_sparc_op:stfi(Src,Dst,ImmOff);
	    double ->  hipe_sparc_op:stdfi(Src,Dst,ImmOff);
	    quad ->    hipe_sparc_op:stqfi(Src,Dst,ImmOff)
	  end;
	false -> %% Not reg or imm
	  exit([{problem,{not_handled_offset,Off}},
		{at,Store}])
      end;
    true -> 
      case Type of
	single ->  hipe_sparc_op:stf(Src,Dst,hipe_sparc:reg_nr(Off));
	double ->  hipe_sparc_op:stdf(Src,Dst,hipe_sparc:reg_nr(Off));
	quad ->    hipe_sparc_op:stqf(Src,Dst,hipe_sparc:reg_nr(Off))
      end
  end.


%% fb<Cond>[,a][,pt] <%fccN>,<L>
%%
assemble_fb(B) ->
  Disp = hipe_sparc:fb_label(B),
  Cond = hipe_sparc_op:fcc_bits(hipe_sparc:fb_cond(B)),
  Pred = hipe_sparc_op:predicate_bit(hipe_sparc:fb_taken(B)),
  Annul = hipe_sparc_op:annul_bit(hipe_sparc:fb_annul(B)),
  Fcc = hipe_sparc:fb_fcc_reg(B),
  hipe_sparc_op:fbpfcc(Fcc, Cond, Annul, Pred, Disp).



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
      
%%     Dst   Src1   Src2
%%     fccN  fpreg  fpreg
assemble_fcmp(Instr) ->
  Fcc = hipe_sparc:fcmp_fcc_reg(Instr),
  Src1 = hipe_sparc:fpreg_nr(hipe_sparc:fcmp_src1(Instr)),
  Src2 = hipe_sparc:fpreg_nr(hipe_sparc:fcmp_src2(Instr)),
  Exception = hipe_sparc:fcmp_exception(Instr),
  Type = hipe_sparc:fcmp_type(Instr),
  %% XXX: Is this the right way to code the regs?
  RS1 = encode_5bit_fpreg(Src1, Type),
  RS2 = encode_5bit_fpreg(Src2, Type),
  case Type of
    double ->
      case Exception of
	true -> hipe_sparc_op:fcmped(Fcc,RS1,RS2);
	false -> hipe_sparc_op:fcmpd(Fcc,RS1,RS2)
      end;
    single ->
      case Exception of
	true -> hipe_sparc_op:fcmpes(Fcc,RS1,RS2);
	false -> hipe_sparc_op:fcmps(Fcc,RS1,RS2)
      end;
    quad ->
      case Exception of
	true -> hipe_sparc_op:fcmpeq(Fcc,RS1,RS2);
	false -> hipe_sparc_op:fcmpq(Fcc,RS1,RS2)
      end
  end.

%%     Dst   Src
%%     fpreg fpreg
assemble_fmov(Instr) ->
  D = hipe_sparc:fpreg_nr(hipe_sparc:fmov_dest(Instr)),
  S = hipe_sparc:fpreg_nr(hipe_sparc:fmov_src(Instr)),
  Type = hipe_sparc:fmov_type(Instr),
  Neg = hipe_sparc:fmov_negate(Instr),
  Abs = hipe_sparc:fmov_abs(Instr),
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

      
%% ____________________________________________________________________
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
      
   



%% ____________________________________________________________________
%%  write(Address,Op)    
%% Returns: 	true
%% Arguments:	Address - A smallint pointing to memory
%%              Op - A 32-bit opcode
%% Description:	Writes Op to memory at Address. 
%% ____________________________________________________________________
write(Address,Op) ->
  hipe_bifs:write_u32(Address,Op).
