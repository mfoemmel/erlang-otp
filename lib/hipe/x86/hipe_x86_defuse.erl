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
	#inc{dst=Dst} -> dst_def(Dst);
	#lea{temp=Temp} -> [Temp];
	#move{dst=Dst} -> dst_def(Dst);
	#pseudo_call{} -> call_clobbered();
	#pseudo_tailcall{} -> tailcall_clobbered();
	%% call, cmp, comment, jcc, jmp_fun, jmp_label, jmp_switch, label
	%% nop, pseudo_jcc, push, ret
	_ -> []
    end.

dst_def(Dst) ->
    case Dst of
	#x86_temp{} -> [Dst];
	_ -> []
    end.

call_clobbered() ->
    lists:map(fun({R,T}) -> hipe_x86:mk_temp(R, T) end,
	      hipe_x86_registers:call_clobbered()).

tailcall_clobbered() ->
    lists:map(fun({R,T}) -> hipe_x86:mk_temp(R, T) end,
	      hipe_x86_registers:tailcall_clobbered()).

%%%
%%% insn_use(Insn) -- Return set of temps used by an instruction.
%%%

insn_use(I) ->
    case I of
	#alu{src=Src,dst=Dst} -> addtemp(Src, addtemp(Dst, []));
	#call{'fun'=Fun} -> addtemp(Fun, []);
	#cmovcc{src=Src, dst=Dst} -> addtemp(Src, dst_use(Dst));
	#cmp{src=Src, dst=Dst} -> addtemp(Src, addtemp(Dst, []));
	#dec{dst=Dst} -> addtemp(Dst, []);
	#inc{dst=Dst} -> addtemp(Dst, []);
	#jmp_fun{'fun'=Fun} -> addtemp(Fun, []);
	#jmp_switch{temp=Temp} -> [Temp];
	#lea{mem=Mem} -> addtemp(Mem, []);
	#move{src=Src,dst=Dst} -> addtemp(Src, dst_use(Dst));
	#pseudo_call{'fun'=Fun} -> addtemp(Fun, []);
	#pseudo_tailcall{'fun'=Fun,args=Args} -> addtemp(Fun, addtemps(Args, []));
	#push{src=Src} -> addtemp(Src, []);
	#ret{} -> [hipe_x86:mk_temp(hipe_x86_registers:eax(), 'tagged')];
	%% comment, jcc, jmp_label, label, nop, pseudo_jcc
	_ -> []
    end.

dst_use(Dst) ->
    case Dst of
	#x86_mem{base=Base,off=Off} -> addtemp(Base, addtemp(Off, []));
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
	#x86_mem{base=Base,off=Off} -> addtemp(Off, addtemp(Base, Set));
	_ -> Set
    end.

add(Arg, Set) ->
    case lists:member(Arg, Set) of
	false -> [Arg|Set];
	_ -> Set
    end.
