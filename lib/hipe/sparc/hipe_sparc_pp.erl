%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_pp.erl
%%  Module   :	hipe_sparc_pp
%%  Purpose  :  Pretty printer for sparc code
%%  Notes    : 
%%  History  :	* 2001-10-25 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2004/10/28 05:13:11 $
%%              $Revision: 1.22 $
%% ====================================================================
%%  Exports  :
%%              pp/1,        Pretty prints linear SPARC code.
%%	        pp/2,        -- "" -- To a file
%%              pp_instr/1,  Pretty prints a SPARC instruction.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_pp).
-export([pp/1,         %% Pretty prints linear SPARC code.
	 pp/2,         %% -- "" -- To a device (file)
	 pp_instr/1]). %% Pretty prints a SPARC instruction.
-include("../main/hipe.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Pretty printer
%%
%% - pp/1: pretty prints linear SPARC code
%%

pp(Sparc) ->
  pp(Sparc, standard_io).

pp(Sparc, Dev) ->
  {M, F, A} = hipe_sparc:sparc_fun(Sparc),
  Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
   case hipe_sparc:sparc_is_closure(Sparc) of
     true ->
       io:format(Dev, "! Closure\n", []);
     _ -> ok
   end,
   case hipe_sparc:sparc_is_leaf(Sparc) of
     true ->
       io:format(Dev, "! Leaf function\n", []);
     _ -> ok
   end,
  io:format(Dev, ".section    \".text\"~n", []),
  io:format(Dev, "    .align 4~n", []),
  io:format(Dev, "    .global ", []),
  io:format(Dev, "~s~n", [Fname]),
  io:format(Dev, ".section    \".data\"\n", []),
  hipe_data_pp:pp(Dev, hipe_sparc:sparc_data(Sparc), sparc, Fname), 
  io:format(Dev, ".section    \".code\"\n", []),
  io:format(Dev, "~s:~n", [Fname]),
  pp_instrs(hipe_sparc:sparc_code(Sparc), Dev, Fname),
  io:format(Dev, "~n~n", []).

pp_instrs([], _Dev, _Fname) ->
  ok;
pp_instrs([I|Is], Dev, Fname) ->
  pp_instr(I, Dev, Fname),
  pp_instrs(Is, Dev, Fname).


pp_instr(I) ->
  pp_instr(I, standard_io, "").

pp_instr(I, Dev, Pre) ->
  case hipe_sparc:type(I) of
    pseudo_return ->
      io:format(Dev, "    retl ! ", []),
      pp_args(Dev, hipe_sparc:pseudo_return_regs(I)),
      io:format(Dev, "~n", []);
    pseudo_enter ->
      io:format(Dev, "!    pseudo_enter ", []),
      pp_target(Dev, 
		hipe_sparc:pseudo_enter_target(I),
		hipe_sparc:pseudo_enter_is_known(I)),
      io:format(Dev, " ! (",[]),
      pp_args(Dev, hipe_sparc:pseudo_enter_args(I)),
      io:format(Dev, ")~n", []);
%     pseudo_push ->
%       io:format(Dev, "!    pseudo_push ", []),
%       pp_arg(Dev, hipe_sparc:pseudo_push_reg(I)),
%       io:format(Dev, "~n", []);
    pseudo_spill ->
      io:format(Dev, "!    pseudo_spill ", []),
      pp_arg(Dev, hipe_sparc:pseudo_spill_reg(I)),
      io:format(Dev, ", SP<", []),
      pp_arg(Dev, hipe_sparc:pseudo_spill_pos(I)),
      io:format(Dev, ">~n", []);
    pseudo_unspill ->
      io:format(Dev, "!    pseudo_unspill ", []),
      pp_arg(Dev, hipe_sparc:pseudo_unspill_reg(I)),
      io:format(Dev, ", SP<", []),
      pp_arg(Dev, hipe_sparc:pseudo_unspill_pos(I)),
      io:format(Dev, ">~n", []);
    pseudo_pop ->
      io:format(Dev, "!    pseudo_get_arg ", []),
      pp_arg(Dev, hipe_sparc:pseudo_pop_index(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:pseudo_pop_reg(I)),

      io:format(Dev, "~n", []);
    label ->
      io:format(Dev, ".~s_~w:~n", [Pre, hipe_sparc:label_name(I)]);
    comment ->
      io:format(Dev, "    ! ~p~n", [hipe_sparc:comment_text(I)]);
    nop ->
      io:format(Dev, "    nop~n", []);
    move ->
      io:format(Dev, "    mov ", []),
      pp_arg(Dev, hipe_sparc:move_src(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:move_dest(I)),
      io:format(Dev, "~n", []);
    multimove ->
      Srcs = hipe_sparc:multimove_src(I),
      Dsts = hipe_sparc:multimove_dest(I),
      case length(Srcs) of
	1 ->
	  io:format(Dev, "    mov ", []),
	  pp_arg(Dev, hd(Srcs)),
	  io:format(Dev, ", ", []),
	  pp_arg(Dev, hd(Dsts)),
	  io:format(Dev, " ! mmove ~n", []);
	_ ->
	  io:format(Dev, "    ! Multimove   !~n",[]),
	  pp_mmoves(Dev, Srcs, Dsts),
	  io:format(Dev, "    ! End mmove   !~n",[])
      end;
    alu ->
      io:format(Dev, "    ", []),
      pp_alu_op(Dev, hipe_sparc:alu_operator(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:alu_src1(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:alu_src2(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:alu_dest(I)),
      io:format(Dev, "~n", []);
    alu_cc ->
      io:format(Dev, "    ", []),
      pp_alu_op(Dev, hipe_sparc:alu_cc_operator(I)),
      io:format(Dev, "cc ", []),
      pp_arg(Dev, hipe_sparc:alu_cc_src1(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:alu_cc_src2(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:alu_cc_dest(I)),
      io:format(Dev, "~n", []);
%%     br ->
%%       io:format(Dev, "    br", []),
%%       pp_regcc(Dev, hipe_sparc:br_regcond(I)),
%%       pp_annul(Dev, hipe_sparc:br_annul(I)),
%%       pp_pred(Dev, hipe_sparc:br_taken(I)),
%%       io:format(Dev, " ", []),
%%       pp_arg(Dev, hipe_sparc:br_reg(I)),
%%       pp_target(Dev, 
%% 		hipe_sparc:br_true_label(I),
%% 		hipe_sparc:br_false_label(I),
%% 		Pre),
%%       io:format(Dev, "~n", []);
    b ->
      io:format(Dev, "    b", []),
      pp_cc(Dev, hipe_sparc:b_cond(I)),
      pp_annul(Dev, hipe_sparc:b_annul(I)),
      pp_pred(Dev, hipe_sparc:b_taken(I)),
      io:format(Dev, " %icc",[]),
      pp_target(Dev, 
		hipe_sparc:b_true_label(I),
		hipe_sparc:b_false_label(I),
		Pre),
      io:format(Dev, "~n", []);
    goto ->
      io:format(Dev, "    ba .~s_~w~n", [Pre, hipe_sparc:goto_label(I)]);
    jmp ->
      io:format(Dev, "    jmpl ", []),
      pp_arg(Dev, hipe_sparc:jmp_target(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, hipe_sparc:jmp_off(I)),
      io:format(Dev, ", %g0 ! (", []),
      pp_args(Dev, hipe_sparc:jmp_args(I)),
      io:format(Dev, ") ", []),
      case hipe_sparc:jmp_destinations(I) of
	[] -> io:format(Dev, "~n", []);
	Lbls -> pp_switch_labels(Dev,Lbls, Pre),
		io:format(Dev, "~n", [])
      end;

    %% jmp_link ->
    call_link ->
      io:format(Dev, "    call ", []),
      pp_target(Dev, 
		hipe_sparc:call_link_target(I),
		hipe_sparc:call_link_is_known(I)),
      io:format(Dev, " ! (",[]),
      pp_args(Dev, hipe_sparc:call_link_args(I)),
      io:format(Dev, ") <", []),
      pp_args(Dev, hipe_sparc:call_link_dests(I)),
      io:format(Dev, ">", []),
      case hipe_sparc:call_link_fail(I) of
	[] -> true;
	L ->
	  io:format(Dev, " fail to ~s_~w", [Pre,L])
      end,
      %% io:format(Dev, "~n", []),
      case hipe_sparc:call_link_continuation(I) of
	[] -> ok;
	CL -> io:format(Dev, "   (~s_~w)", [Pre,CL])
      end,
      hipe_sparc:pp_sdesc(Dev, hipe_sparc:call_link_stack_desc(I)),
      io:format(Dev, "~n", []);

    load ->
      io:format(Dev, "    ", []),
      pp_load_op(Dev, hipe_sparc:load_type(I)),
      io:format(Dev, " [", []),
      pp_arg(Dev, hipe_sparc:load_src(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, hipe_sparc:load_off(I)),
      io:format(Dev, "], ", []),
      pp_arg(Dev, hipe_sparc:load_dest(I)),
      io:format(Dev, "~n", []);
    load_atom ->
      Atom = hipe_sparc:load_atom_atom(I),
      io:format(Dev, "   sethi %hi(~w), ", [hipe_bifs:atom_to_word(Atom)]),
      pp_arg(Dev, hipe_sparc:load_atom_dest(I)),
      io:format(Dev, "~n   or ",[]),
      pp_arg(Dev, hipe_sparc:load_atom_dest(I)),
      io:format(Dev, ",  %lo(~w), ", [hipe_bifs:atom_to_word(Atom)]),
      pp_arg(Dev, hipe_sparc:load_atom_dest(I)),
      io:format(Dev, " ! load_atom('~w') ~n", [Atom]);
    load_word_index ->
      io:format(Dev, "    mov word_index(~s_dl_~w, ~w), ", 
		[Pre, hipe_sparc:load_word_index_block(I), hipe_sparc:load_word_index_index(I)]),
      pp_arg(Dev, hipe_sparc:load_word_index_dest(I)),
      io:format(Dev, "~n",[]);
    load_address ->
      Address = hipe_sparc:load_address_address(I),
      Type = hipe_sparc:load_address_type(I), 
      case Type of
	function ->
	  case Address of
	    {M, F, A} -> 
	      io:format(Dev, "    sethi %hi(~w_~w_~w), ", [M, F, A]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, "~n    or  ", []),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, ", %lo(~w_~w_~w), ", [M, F, A]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I));
	    {F, A} -> 
	      io:format(Dev, "    sethi %hi(~w_~w), ", [F, A]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, "~n    or  ", []),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, ", %lo(~w_~w), ", [F, A]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I));
	    F -> 
	      io:format(Dev, "    sethi %hi( ", []),
	      io:format(Dev, "~w), ", [F]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, "~n    or  ", []),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	      io:format(Dev, ", %lo(~w), ", [F]),
	      pp_arg(Dev, hipe_sparc:load_address_dest(I))
	  end;
	constant ->
	  io:format(Dev, "    sethi %hi( ", []),
	  io:format(Dev, "~s_dl_~w), ", [Pre, Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, "~n    or  ", []),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, ", %lo(~s_dl_~w), ", [Pre, Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I));
	c_const ->
	  io:format(Dev, "    sethi %hi(", []),
	  io:format(Dev, "~w), ", [Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, "~n    or  ", []),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, ", %lo(~w), ", [Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I));
	label ->
	  io:format(Dev, "    sethi %hi( ", []),
	  io:format(Dev, "~s_~w), ", [Pre, Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, "~n    or", []),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, ", %lo(~s_dl_~w), ", [Pre, Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I));
	closure ->
	  io:format(Dev, "    lda ", []),
	  io:format(Dev, "~s_~w, ", [Pre, Address]),
	  pp_arg(Dev, hipe_sparc:load_address_dest(I)),
	  io:format(Dev, " ! [closure]",[])
      end,
      io:format(Dev, "~n", []);
    store ->
      io:format(Dev, "    ", []),
      pp_store_op(Dev, hipe_sparc:store_type(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:store_src(I)),
      io:format(Dev, ", [", []),
      pp_arg(Dev, hipe_sparc:store_dest(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, hipe_sparc:store_off(I)),
      io:format(Dev, "]~n", []);
    sethi ->
      io:format(Dev, "    sethi ", []),
      pp_arg(Dev, hipe_sparc:sethi_const(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:sethi_dest(I)),
      io:format(Dev, "~n", []);
    load_fp ->
      io:format(Dev, "    ", []),
      pp_load_fp_op(Dev, hipe_sparc:load_fp_type(I)),
      io:format(Dev, " [", []),
      pp_arg(Dev, hipe_sparc:load_fp_src(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, hipe_sparc:load_fp_off(I)),
      io:format(Dev, "], ", []),
      pp_arg(Dev, hipe_sparc:load_fp_dest(I)),
      io:format(Dev, "~n", []);
    store_fp ->
      io:format(Dev, "    ", []),
      pp_store_fp_op(Dev, hipe_sparc:store_fp_type(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:store_fp_src(I)),
      io:format(Dev, ", [", []),
      pp_arg(Dev, hipe_sparc:store_fp_dest(I)),
      io:format(Dev, "+", []),
      pp_arg(Dev, hipe_sparc:store_fp_off(I)),
      io:format(Dev, "]~n", []);

%%     fb ->
%%       io:format(Dev, "    fb", []),
%%       pp_fcc(Dev, hipe_sparc:fb_cond(I)),
%%       pp_annul(Dev, hipe_sparc:fb_annul(I)),
%%       pp_pred(Dev, hipe_sparc:fb_taken(I)),
%%       io:format(Dev, " %fcc~w",[hipe_sparc:fb_fcc_reg(I)]),
%%       pp_target(Dev, 
%% 		hipe_sparc:fb_true_label(I),
%% 		hipe_sparc:fb_false_label(I),
%% 		Pre),
%%       io:format(Dev, "~n", []);

    fop ->
      io:format(Dev, "    ", []),
      pp_fop_op(Dev, hipe_sparc:fop_operator(I)),
      pp_fp_type(Dev,hipe_sparc:fop_type(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:fop_src1(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:fop_src2(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:fop_dest(I)),
      io:format(Dev, "~n", []);
%%     fcmp ->
%%       io:format(Dev, "    fcmp", []),
%%       case hipe_sparc:fcmp_exception(I) of
%% 	true -> io:format(Dev, "e",[]);
%% 	_ -> ok
%%       end,
%%       pp_fp_type(Dev,hipe_sparc:fcmp_type(I)),
%%       io:format(Dev, " ", []),
%%       io:format(Dev, " %fcc~w",[hipe_sparc:fcmp_fcc_reg(I)]),
%%       io:format(Dev, ",", []),
%%       pp_arg(Dev, hipe_sparc:fcmp_src1(I)),
%%       io:format(Dev, ", ", []),
%%       pp_arg(Dev, hipe_sparc:fcmp_src2(I)),
%%       io:format(Dev, "~n", []);
    fmove ->
      case {hipe_sparc:fmove_negate(I),hipe_sparc:fmove_abs(I)} of
	{true, false} ->
	  io:format(Dev, "    fneg", []);
	{false, true} ->
	  io:format(Dev, "    fabs", []);
	{false, false} ->
	  io:format(Dev, "    fmov", []);
	_ ->
	  ?EXIT({"Illegal SPARC fmov instruction", I})
      end,
      pp_fp_type(Dev,hipe_sparc:fmove_type(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:fmove_src(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:fmove_dest(I)),
      io:format(Dev, "~n", []);
    conv_fp ->
      io:format(Dev, "    ", []),
      io:format(Dev, "fito", []),
      pp_fp_type(Dev, hipe_sparc:conv_fp_dest_type(I)),
      io:format(Dev, " ", []),
      pp_arg(Dev, hipe_sparc:conv_fp_src(I)),
      io:format(Dev, ", ", []),
      pp_arg(Dev, hipe_sparc:conv_fp_dest(I)),
      io:format(Dev, "~n", []);

    X ->
      ?EXIT({"unknown sparc instruction", X})
  end.

pp_mmoves(Dev, [Src|Srcs], [Dst| Dsts]) ->
  io:format(Dev, "    mov ", []),
  pp_arg(Dev, Src),
  io:format(Dev, ", ", []),
  pp_arg(Dev, Dst),
  io:format(Dev, " !~n", []),
  pp_mmoves(Dev, Srcs, Dsts);
pp_mmoves(_,[],[]) -> ok.


pp_alu_op(Dev, Op) ->
  Str = case Op of
	  '+' -> "add";
	  '-' -> "sub";
	  '>>' -> "srl";
	  '>>64' -> "srlx";
	  '>>?' -> "sra";
	  '>>?64' -> "srax";
	  '<<' -> "sll";
	  '<<64' -> "sllx";
	  'and' -> "and";
	  'or' -> "or";
	  'xor' -> "xor";
	  '+c' -> "addc";
	  '-c' -> "subc";
	  'andn' -> "andn";
	  'xnor' ->  "xnor";
	  X -> exit({sparc, {"unkown alu-op", X}}), ""
	end,
  io:format(Dev, "~s", [Str]).

pp_fop_op(Dev, Op) ->
  Str = case Op of
	  '+' -> "fadd";
	  '-' -> "fsub";
	  '*' -> "fmul";
	  '/' -> "fdiv";
	  X -> exit({sparc, {"unkown floating point-op", X}}), ""
	end,
  io:format(Dev, "~s", [Str]).

pp_fp_type(Dev,Type) ->
  Str = case Type of
	  single -> "s";
	  double -> "d";
	  quad -> "q"
	end,
  io:format(Dev, "~s", [Str]).

pp_load_fp_op(Dev, Type) ->
  Str = case Type of
	  single -> "ld";
	  double -> "ldd";
	  quad -> "ldq"
	end,
  io:format(Dev, "~s", [Str]).

pp_store_fp_op(Dev, Type) ->
  Str = case Type of
	  single -> "st";
	  double -> "std";
	  quad -> "stdq"
	end,
  io:format(Dev, "~s", [Str]).

pp_load_op(Dev, Type) ->
  Str = case Type of
	  ub -> "ldub";
	  sb -> "ldsb";
	  uh -> "lduh";
	  sh -> "ldsh";
	  sw -> "ldsw";
	  uw -> "lduw";
	  xw -> "ldx"
	end,
  io:format(Dev, "~s", [Str]).

pp_store_op(Dev, Type) ->
  Str = case Type of
	  b -> "stb";
	  h -> "sth";
	  w -> "stw";
	  x -> "stx"
	end,
  io:format(Dev, "~s", [Str]).


%% pp_regcc(Dev, CC) ->
%%   io:format(Dev, "~s", [CC]).

pp_cc(Dev, CC) ->
  io:format(Dev, "~s", [CC]).

%% pp_fcc(Dev, FCC) ->
%%   io:format(Dev, "~s", [FCC]).

pp_arg(Dev, Arg) ->
  case  hipe_sparc:is_reg(Arg) of
    true ->
      io:format(Dev, "~s", 
		[hipe_sparc_registers:reg_name( hipe_sparc:reg_nr(Arg))]);
    false ->
      case hipe_sparc:is_imm(Arg) of
	true ->
	  io:format(Dev, "~w", [ hipe_sparc:imm_value(Arg)]);
	false ->
	  case hipe_sparc:is_fpreg(Arg) of
	    true ->
	      io:format(Dev, "~s", 
			[hipe_sparc_registers:fpreg_name( hipe_sparc:fpreg_nr(Arg))]);
	    false ->
	      case hipe_sparc:is_spill(Arg) of
		true ->
		  io:format(Dev, "~w", [ hipe_sparc:spill_pos(Arg)]);
		false ->
		  ?EXIT({bad_sparc_arg,Arg})
	      end
	  end
      end
  end.


pp_args(_Dev, []) ->
  ok;
pp_args(Dev, [A]) ->
  pp_arg(Dev, A);
pp_args(Dev, [A|As]) ->
  pp_arg(Dev, A),
  io:format(Dev, ", ", []),
  pp_args(Dev, As).


pp_switch_labels(Dev,Lbls, Pre) -> 
  pp_switch_labels(Dev,Lbls,1, Pre).

pp_switch_labels(Dev, [L], _Pos, Pre) -> 
  io:format(Dev, "~s_~w", [Pre,L]);
pp_switch_labels(Dev, [L|Ls], Pos,Pre) -> 
  io:format(Dev, "~s_~w, ", [Pre,L]),
  NewPos = 
    case Pos of
      3 -> io:format(Dev, "\n             ! ",[]),
	   0;
      N -> N + 1
    end,
  pp_switch_labels(Dev, Ls, NewPos, Pre);
pp_switch_labels(_Dev, [], _, _) -> ok.


pp_target(Dev,T,_Known=false) ->
  pp_arg(Dev,T);
pp_target(Dev,T,true) ->
  case T of
    {M, F, A} -> io:format(Dev, "~w_~w_~w ", [M, F, A]);
    {F, A} -> io:format(Dev, "~w_~w ", [F, A]);
    F -> io:format(Dev, "~w ", [F])
  end.

pp_target(Dev, Target, [], Pre) ->
  io:format(Dev, ", .~s_~w", [Pre, Target]);
pp_target(Dev, Target, Fail, Pre) ->
  io:format(Dev, ", .~s_~w ! .~s_~w", 
	    [Pre, Target, Pre, Fail]). 

pp_annul(Dev, A) ->
  case A of
    a ->  io:format(Dev, ",a", []);
    na -> ok
  end.


pp_pred(Dev, P) ->
  case P of
    true -> ok;
    false -> io:format(Dev, ",pn", [])
  end.

