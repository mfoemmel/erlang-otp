%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/02/12 13:44:36 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_assemble.erl
%%  Module   :	hipe_sparc_assemble
%%  Purpose  :  To write SPARC-code to memory. 
%%  Notes    :  Not all SPARC instructions are handled.
%%  History  :	* 1998-06-18 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: richardc $
%%    $Date: 2001/03/26 18:37:08 $
%%    $Revision: 1.1.1.1 $
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
	 assemble_nop/1]).
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
    nop ->
      assemble_nop(I);

    T ->
      exit([{problem,{not_handled,instruction, T}},{at,I}])
  end.
	 

%%     Src Dst
%% mov reg reg  
%% mov imm reg
assemble_move(Instr) ->
  % move is a syntetic instruction, implemented with 'or'.
  Dst = hipe_sparc:reg_nr(hipe_sparc:move_dest(Instr)),
  Src = hipe_sparc:move_src(Instr),
  
  % Check if the sorce is a register.
  case hipe_sparc:is_reg(Src) of
    false ->
      % If the sorce isn't a register, than it should be an immidiate.
      case hipe_sparc:is_imm(Src) of
	true -> 
	  V = hipe_sparc:imm_value(Src),
	  if 
	    V > 4095 -> 
	      exit([{problem,{to_big_integer,Src}},{at,Instr}]);
	    true ->
	      hipe_sparc_op:ori(0,hipe_sparc:imm_value(Src),Dst)
	  end;
	_ -> exit([{problem,{not_an_integer,Src}},{at,Instr}])
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
  AluOp = hipe_sparc:alu_operator(Instr),
  case hipe_sparc:is_reg(hipe_sparc:alu_src2(Instr)) of
    false -> 
      Src2 = hipe_sparc:imm_value(hipe_sparc:alu_src2(Instr)),
      case AluOp of
	'+' -> hipe_sparc_op:addi(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:subi(Src1,Src2,Dst);
	'+c' -> hipe_sparc_op:addicc(Src1,Src2,Dst);
	'-c' -> hipe_sparc_op:subicc(Src1,Src2,Dst);
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
	_ -> exit([{problem,{not_handled,{aluop,AluOp}}},{at,Instr}])
      end;
    true -> 
      Src2 = hipe_sparc:reg_nr(hipe_sparc:alu_src2(Instr)),
      case AluOp of
	'+' -> hipe_sparc_op:add(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:sub(Src1,Src2,Dst);
	'+c' -> hipe_sparc_op:addcc(Src1,Src2,Dst);
	'-c' -> hipe_sparc_op:subcc(Src1,Src2,Dst);
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
  AluOp = hipe_sparc:alu_cc_operator(Instr),
  case hipe_sparc:is_reg(hipe_sparc:alu_cc_src2(Instr)) of
    false ->
      Src2 = hipe_sparc:imm_value(hipe_sparc:alu_cc_src2(Instr)),
      case AluOp of
	'+' -> hipe_sparc_op:addicc(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:subicc(Src1,Src2,Dst);
	'and' -> hipe_sparc_op:andicc(Src1,Src2,Dst);
	'andn' -> hipe_sparc_op:andnicc(Src1,Src2,Dst);
	'or' -> hipe_sparc_op:oricc(Src1,Src2,Dst);
	'xor' -> hipe_sparc_op:xoricc(Src1,Src2,Dst);
	%% '>>' -> hipe_sparc_op:srlicc(Src1,Src2,Dst);
	%% '>>?' -> hipe_sparc_op:sraicc(Src1,Src2,Dst);
	%% '<<' -> hipe_sparc_op:sllicc(Src1,Src2,Dst);
	_ -> exit([{problem,{not_handled,{aluccop,AluOp}}},{at,Instr}])
      end;
    true ->
      Src2 = hipe_sparc:reg_nr(hipe_sparc:alu_cc_src2(Instr)),
      case AluOp of
	'+' -> hipe_sparc_op:addcc(Src1,Src2,Dst);
	'-' -> hipe_sparc_op:subcc(Src1,Src2,Dst);
	'and' -> hipe_sparc_op:andcc(Src1,Src2,Dst);
	'andn' -> hipe_sparc_op:andncc(Src1,Src2,Dst);
	'or' -> hipe_sparc_op:orcc(Src1,Src2,Dst);
	'xor' -> hipe_sparc_op:xorcc(Src1,Src2,Dst);
	%% '>>' -> hipe_sparc_op:srlcc(Src1,Src2,Dst);
	%% '>>?' -> hipe_sparc_op:sracc(Src1,Src2,Dst);
	%% '<<' -> hipe_sparc_op:sllcc(Src1,Src2,Dst);
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
    false -> hipe_sparc_op:sti(Src,Dst,hipe_sparc:imm_value(Off));
    true -> hipe_sparc_op:st(Src,Dst,hipe_sparc:reg_nr(Off))
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
	  case hipe_sparc:is_reg(Off) of 
	    false -> hipe_sparc_op:ldi(Src,hipe_sparc:imm_value(Off),Dst);
	    true -> hipe_sparc_op:ld(Src,hipe_sparc:reg_nr(Off),Dst)
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
  Pred = hipe_sparc_op:predicate_bit(hipe_sparc:br_pred(BR)),
  Annul = hipe_sparc_op:annul_bit(hipe_sparc:br_annul(BR)),
  hipe_sparc_op:bpr(RCond, Annul, Pred, Reg, Disp).

assemble_call_link(Call_link) ->
  hipe_sparc_op:call(hipe_sparc:call_link_target(Call_link)).

assemble_jmp_link(Jmp_link) ->
  Target = hipe_sparc:reg_nr(hipe_sparc:jmp_link_target(Jmp_link)),
  Off = hipe_sparc:jmp_link_off(Jmp_link),
  Link = hipe_sparc:reg_nr(hipe_sparc:jmp_link_link(Jmp_link)),
  case hipe_sparc:is_reg(Off) of 
    true -> exit([{problem,{not_handled,{jmp_link,reg_reg_reg}}},{at,Jmp_link}]);
    false -> hipe_sparc_op:jumpli(Target,hipe_sparc:imm_value(Off),Link)
  end.

assemble_jmp(Jmp)->
  Target = hipe_sparc:reg_nr(hipe_sparc:jmp_target(Jmp)),
  Off = hipe_sparc:jmp_off(Jmp),
  case hipe_sparc:is_reg(Off) of 
    true ->  exit([{problem,{not_handled,{jmp,reg_reg}}},{at,Jmp}]);
    false -> hipe_sparc_op:jumpli(Target,hipe_sparc:imm_value(Off),0)
  end.

assemble_sethi(Instr)->
  Dest = hipe_sparc:reg_nr(hipe_sparc:sethi_dest(Instr)),
  Val = hipe_sparc:imm_value(hipe_sparc:sethi_const(Instr)), % Fix to work for Const-expr
  if integer(Val) -> 
      hipe_sparc_op:sethi(Val,Dest);
     true -> 
      exit([{problem,{not_an_integer,Val}},{at,Instr}])
  end.

assemble_nop(Instr) ->
  hipe_sparc_op:nop().



%% ____________________________________________________________________
%%  write(Address,Op)    
%% Returns: 	true
%% Arguments:	Address - A smallint pointing to memory
%%              Op - A 32-bit opcode
%% Description:	Writes Op to memory at Address. 
%% ____________________________________________________________________
write(Address,Op) ->
  hipe_bifs:write_u32(Address,Op).





