%%% $Id$
%%% x86 pretty-printer

-module(hipe_x86_pp).
-export([pp/1, pp/2, pp_insn/1]).
-include("hipe_x86.hrl").

pp(Defun) ->
  pp(standard_io, Defun).

pp(Dev, #defun{mfa=MFA, code=Code, data=Data}) ->
  {M,F,A} = hipe_x86:mfa_mfa(MFA),
  Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
  io:format(Dev, "\t.text\n", []),
  io:format(Dev, "\t.align 4\n", []),
  io:format(Dev, "\t.global ~s\n", [Fname]),
  io:format(Dev, "~s:\n", [Fname]),
  pp_insns(Dev, Code, Fname),
  io:format(Dev, "\t.rodata\n", []),
  io:format(Dev, "\t.align 4\n", []),
  hipe_data_pp:pp(Dev, Data, x86, Fname),
  io:format(Dev, "\n", []).

pp_insns(Dev, [I|Is], Fname) ->
  pp_insn(Dev, I, Fname),
  pp_insns(Dev, Is, Fname);
pp_insns(_, [], _) ->
  [].

pp_insn(I) ->
  pp_insn(standard_io, I, "").

pp_insn(Dev, I, Pre) ->
  case I of
    #alu{aluop=AluOp, src=Src, dst=Dst} ->
      io:format(Dev, "\t~s ", [alu_op_name(AluOp)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #call{'fun'=Fun, sdesc=SDesc} ->
      io:format(Dev, "\tcall ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " #", []),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, "\n", []);
    #cmovcc{cc=Cc, src=Src, dst=Dst} ->
      io:format(Dev, "\tcmov~s ", [cc_name(Cc)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #cmp{src=Src, dst=Dst} ->
      io:format(Dev, "\tcmp ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n", [Term]);
    #dec{dst=Dst} ->
      io:format(Dev, "\tdec ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #inc{dst=Dst} ->
      io:format(Dev, "\tinc ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #jcc{cc=Cc, label=Label} ->
      io:format(Dev, "\tj~s .~s_~w\n", [cc_name(Cc), Pre, Label]);
    #jmp_fun{'fun'=Fun} ->
      io:format(Dev, "\tjmp ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, "\n", []);
    #jmp_label{label=Label} ->
      io:format(Dev, "\tjmp .~s_~w\n", [Pre, Label]);
    #jmp_switch{temp=Temp, jtab=JTab, labels=Labels} ->
      io:format(Dev, "\tjmp *{constant,~w}(,", [JTab]),
      pp_temp(Dev, Temp),
      io:format(Dev, ",4) #", []),
      pp_labels(Dev, Labels, Pre),
      io:format(Dev, "\n", []);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #lea{mem=Mem, temp=Temp} ->
      io:format(Dev, "\tlea ", []),
      pp_mem(Dev, Mem),
      io:format(Dev, ", ", []),
      pp_temp(Dev, Temp),
      io:format(Dev, "\n", []);
    #move{src=Src, dst=Dst} ->
      io:format(Dev, "\tmov ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #movsx{src=Src, dst=Dst} ->
      io:format(Dev, "\tmovsx ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #movzx{src=Src, dst=Dst} ->
      io:format(Dev, "\tmovzx ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #nop{} ->
      io:format(Dev, "\tnop\n", []);
    #prefix_fs{} ->
      io:format(Dev, "\t.byte 0x64 ! FS segment override prefix\n", []);
    #pseudo_call{dsts=Dsts, 'fun'=Fun, arity=Arity, contlab=ContLab, exnlab=ExnLab} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, " # ~w (", [Arity]),
      pp_args(Dev, Dsts),
      io:format(Dev, ")", []),
      io:format(Dev, " contlab .~s_~w", [Pre, ContLab]),
      case ExnLab of
	[] -> [];
	_ -> io:format(Dev, " exnlab .~s_~w", [Pre, ExnLab])
      end,
      io:format(Dev, "\n", []);
    #pseudo_jcc{cc=Cc, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_j~s ", [cc_name(Cc)]),
      io:format(Dev, ".~s_~w # .~s_~w ~.2f\n",
		[Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_tailcall{'fun'=Fun, arity=Arity, stkargs=StkArgs} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_fun(Dev, Fun),
      io:format(Dev, "~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ")\n", []);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #push{src=Src} ->
      io:format(Dev, "\tpush ", []),
      pp_src(Dev, Src),
      io:format(Dev, "\n", []);
    #ret{npop=NPop} ->
      io:format(Dev, "\tret $~s\n", [to_hex(NPop)]);
    #shift{shiftop=ShiftOp, src=Src, dst=Dst} ->
      io:format(Dev, "\t~s ", [alu_op_name(ShiftOp)]),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    #finit{} ->
      io:format(Dev, "\tfinit\n ", []);
    #fp_binop{src=Src, dst=Dst, op=Op} ->
      io:format(Dev, "\t~s ", [Op]),
      pp_dst(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_src(Dev, Src),
      io:format(Dev, "\n", []);
    #fp_unop{arg=Arg, op=Op} ->
      io:format(Dev, "\t~s ", [Op]),
      case Arg of
	[]->
	  io:format(Dev, "\n", []);
	_ ->
	  pp_args(Dev, [Arg]),
	  io:format(Dev, "\n", [])
      end;
    #fmov{src=Src, dst=Dst} ->
      io:format(Dev, "\tfmov ", []),
      pp_src(Dev, Src),
      io:format(Dev, ", ", []),
      pp_dst(Dev, Dst),
      io:format(Dev, "\n", []);
    _ ->
      exit({?MODULE, pp_insn, {"unknown instruction", I}})
  end.

to_hex(N) ->
  case hipe_converters:int_to_hex(N) of
    [$- | Digits] -> [$-, $0, $x | Digits];
    Digits -> [$0, $x | Digits]
  end.

pp_sdesc(Dev, Pre, #x86_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
  pp_sdesc_exnlab(Dev, Pre, ExnLab),
  io:format(Dev, " ~s ~w [", [to_hex(FSize), Arity]),
  pp_sdesc_live(Dev, Live),
  io:format(Dev, "]", []).

pp_sdesc_exnlab(Dev, _, []) -> io:format(Dev, " []", []);
pp_sdesc_exnlab(Dev, Pre, ExnLab) -> io:format(Dev, " .~s_~w", [Pre, ExnLab]).

pp_sdesc_live(_, {}) -> [];
pp_sdesc_live(Dev, Live) -> pp_sdesc_live(Dev, Live, 1).

pp_sdesc_live(Dev, Live, I) ->
  io:format(Dev, "~s", [to_hex(element(I, Live))]),
  if I < size(Live) ->
      io:format(Dev, ",", []),
      pp_sdesc_live(Dev, Live, I+1);
     true -> []
  end.

pp_labels(Dev, [Label|Labels], Pre) ->
  io:format(Dev, " .~s_~w", [Pre, Label]),
  pp_labels(Dev, Labels, Pre);
pp_labels(_, [], _) ->
  [].

pp_fun(Dev, Fun) ->
  case Fun of
    #x86_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #x86_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim]);
    _ ->	% temp or mem
      io:format(Dev, "*", []),
      pp_dst(Dev, Fun)
  end.

alu_op_name(Op) -> Op.

cc_name(Cc) -> Cc.

pp_hard_reg(Dev, Reg) ->
  io:format(Dev, "~s", [hipe_x86_registers:reg_name(Reg)]).

type_tag('tagged') -> "t";
type_tag('untagged') -> "u";
type_tag('double') -> "d".

pp_temp(Dev, #x86_temp{reg=Reg, type=Type}) ->
  case Type of
    double ->
      Tag = type_tag(Type),
      io:format(Dev, "~s~w", [Tag, Reg]);
    _ ->
      case hipe_x86_registers:is_precoloured(Reg) of
	true ->
	  pp_hard_reg(Dev, Reg);
	false ->
	  Tag = type_tag(Type),
	  io:format(Dev, "~s~w", [Tag, Reg])
      end
  end.

pp_fpreg(Dev, #x86_fpreg{reg=Reg, pseudo=Pseudo})->
  case Pseudo of
    true -> io:format(Dev, "pseudo_fp(~w)", [Reg]);
    _ -> io:format(Dev, "st(~w)", [Reg])
  end.

pp_imm(Dev, #x86_imm{value=Value}, Dollar) ->
  if Dollar =:= true -> io:format(Dev, [$$], []);
     true -> []
  end,
  if is_integer(Value) -> io:format(Dev, "~s", [to_hex(Value)]);
     true -> io:format(Dev, "~w", [Value])
  end.

pp_mem(Dev, #x86_mem{base=Base, off=Off}) ->
  pp_off(Dev, Off),
  case Base of
    [] ->
      [];
    _ ->
      io:format(Dev, "(", []),
      pp_temp(Dev, Base),
      io:format(Dev, ")", [])
  end.

pp_off(Dev, Off) ->
  pp_src(Dev, Off, false).

pp_src(Dev, Src) ->
  pp_src(Dev, Src, true).

pp_src(Dev, Src, Dollar) ->
  case Src of
    #x86_temp{} ->
      pp_temp(Dev, Src);
    #x86_imm{} ->
      pp_imm(Dev, Src, Dollar);
    #x86_mem{} ->
      pp_mem(Dev, Src);
    #x86_fpreg{} ->
      pp_fpreg(Dev, Src)
  end.

pp_dst(Dev, Dst) ->
  pp_src(Dev, Dst).

pp_args(Dev, [A|As]) ->
  pp_src(Dev, A),
  pp_comma_args(Dev, As);
pp_args(_, []) ->
  [].

pp_comma_args(Dev, [A|As]) ->
  io:format(Dev, ", ", []),
  pp_src(Dev, A),
  pp_comma_args(Dev, As);
pp_comma_args(_, []) ->
  [].
