%%% $Id$
%%% compute def/use sets for x86 insns
%%%
%%% TODO:
%%% - represent EFLAGS (condition codes) use/def by a virtual reg?
%%% - should push use/def %esp?

-module(hipe_x86_defuse).
-export([insn_def/1, insn_use/1, src_use/1]).
-include("hipe_x86.hrl").

%%%
%%% insn_def(Insn) -- Return set of temps defined by an instruction.
%%%

insn_def(I) ->
    case I of
	#alu{dst=Dst} -> dst_def(Dst);
	#cmovcc{dst=Dst} -> dst_def(Dst);
	#dec{dst=Dst} -> dst_def(Dst);
	#fmov{dst=Dst} -> dst_def(Dst);
	#fop{dst=Dst} -> dst_def(Dst);
	#inc{dst=Dst} -> dst_def(Dst);
	#lea{temp=Temp} -> [Temp];
	#move{dst=Dst} -> dst_def(Dst);
	#movsx{dst=Dst} -> dst_def(Dst);
	#movzx{dst=Dst} -> dst_def(Dst);
	#pseudo_call{} -> call_clobbered();
	#pseudo_tailcall_prepare{} -> tailcall_clobbered();
	%% call, cmp, comment, jcc, jmp_fun, jmp_label, jmp_switch, label
	%% nop, pseudo_jcc, pseudo_tailcall, push, ret
	_ -> []
    end.

dst_def(Dst) ->
    case Dst of
	#x86_temp{} -> [Dst];
	#x86_fpreg{} -> [Dst];
	_ -> []
    end.

call_clobbered() ->
    [hipe_x86:mk_temp(R, T)
     || {R,T} <- hipe_x86_registers:call_clobbered() ++ all_fp_pseudos()].

all_fp_pseudos()->
    [{0,double}, {1,double}, {2,double}, {3,double},
     {4,double}, {5,double}, {6,double}].


tailcall_clobbered() ->
    [hipe_x86:mk_temp(R, T)
     || {R,T} <- hipe_x86_registers:tailcall_clobbered()
	         ++ all_fp_pseudos()].

%%%
%%% insn_use(Insn) -- Return set of temps used by an instruction.
%%%

insn_use(I) ->
    case I of
	#alu{src=Src,dst=Dst} -> 
	    case hipe_x86:is_shift(I) of
		true ->
		    addtemp(Src, addtemp(Dst, [])) ++ [hipe_x86:mk_temp(hipe_x86_registers:ecx(), 'untagged')];
		false ->
		    addtemp(Src, addtemp(Dst, []))
	    end;
	#call{'fun'=Fun} -> addtemp(Fun, []);
	#cmovcc{src=Src, dst=Dst} -> addtemp(Src, dst_use(Dst));
	#cmp{src=Src, dst=Dst} -> addtemp(Src, addtemp(Dst, []));
	#dec{dst=Dst} -> addtemp(Dst, []);
	#fmov{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
	#fop{src=Src,dst=Dst} -> addtemp(Src, addtemp(Dst, []));
	#inc{dst=Dst} -> addtemp(Dst, []);
	#jmp_fun{'fun'=Fun} -> addtemp(Fun, []);
	#jmp_switch{temp=Temp} -> [Temp];
	#lea{mem=Mem} -> addtemp(Mem, []);
	#move{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
	#movsx{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
	#movzx{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
	#pseudo_call{'fun'=Fun,arity=Arity} -> addtemp(Fun, arity_use(Arity));
	#pseudo_tailcall{'fun'=Fun,arity=Arity,stkargs=StkArgs} ->
	    addtemp(Fun, addtemps(StkArgs, addtemps(tailcall_clobbered(), arity_use(Arity))));
	#push{src=Src} -> addtemp(Src, []);
	#ret{} -> [hipe_x86:mk_temp(hipe_x86_registers:eax(), 'tagged')];
	%% comment, jcc, jmp_label, label, nop, pseudo_jcc, pseudo_tailcall_prepare
	_ -> []
    end.

arity_use(Arity) ->
    [hipe_x86:mk_temp(R, 'tagged')
     || R <- hipe_x86_registers:args(Arity)].

dst_use(Dst) ->
    case Dst of
	#x86_mem{base=Base,off=Off} -> addbase(Base, addtemp(Off, []));
	_ -> []
    end.

%%%
%%% src_use(Src) -- Return set of temps used by a source operand.
%%%

src_use(Src) ->
    addtemp(Src, []).

%%%
%%% Auxiliary operations on sets of temps
%%%

addtemps([Arg|Args], Set) ->
    addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
    Set.

addtemp(Arg, Set) ->
    case Arg of
	#x86_temp{} -> add(Arg, Set);
	#x86_mem{base=Base,off=Off} -> addtemp(Off, addbase(Base, Set));
	#x86_fpreg{} -> add(Arg, Set);
	_ -> Set
    end.

addbase(Base, Set) ->
    case Base of
	[] -> Set;
	_ -> addtemp(Base, Set)
    end.

add(Arg, Set) ->
    case lists:member(Arg, Set) of
	false -> [Arg|Set];
	_ -> Set
    end.
