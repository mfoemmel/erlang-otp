%%% $Id$
%%% concrete representation of 2-address pseudo-amd64 code

%%% amd64 operands:
%%%
%%% int32	::= <a 32-bit integer>
%%% reg		::= <token from hipe_amd64_registers module>
%%% type	::= 'tagged' | 'untagged'
%%% label	::= <an integer>
%%% label_type	::= 'label' | 'constant'
%%% aluop	::= <an atom denoting a binary alu op>
%%% term	::= <any Erlang term>
%%% cc		::= <an atom denoting a condition code>
%%% pred	::= <a real number between 0.0 and 1.0 inclusive>
%%% npop	::= <a 32-bit natural number which is a multiple of 4>
%%%
%%% temp	::= {amd64_temp, reg, type, allocatable}
%%% allocatable ::= 'true' | 'false'
%%%
%%% imm		::= {amd64_imm, value}
%%% value	::= int32 | atom | {label, label_type}
%%%
%%% mem		::= {amd64_mem, base, off, type}
%%% base	::= temp | []		(XXX BUG: not quite true before RA)
%%% off		::= imm | temp
%%%
%%% src		::= temp | mem | imm
%%% dst		::= temp | mem
%%% arg		::= src
%%% args	::= <list of arg>
%%%
%%% mfa		::= {amd64_mfa, atom, atom, integer}
%%% prim	::= {amd64_prim, atom}
%%% fun		::= mfa | prim | temp | mem
%%%
%%% jtab	::= label	(equiv. to {amd64_imm,{label,'constant'}})
%%%
%%% sdesc	::= {amd64_sdesc, exnlab, fsize, arity, live}
%%% exnlab	::= [] | label
%%% fsize	::= <int32>		(frame size in words)
%%% live	::= <tuple of int32>	(word offsets)
%%% arity	::= int32

-record(amd64_temp, {reg, type, allocatable}).
-record(amd64_imm, {value}).
-record(amd64_mem, {base, off, type}).
-record(amd64_fpreg, {reg, pseudo}).
-record(amd64_mfa, {m, f, a}).
-record(amd64_prim, {prim}).
-record(amd64_sdesc, {exnlab, fsize, arity, live}).

%%% Basic instructions.
%%% These follow the AT&T convention, i.e. op src,dst (dst := dst op src)
%%% After register allocation, at most one operand in a binary
%%% instruction (alu, cmp, move) may denote a memory cell.
%%% After frame allocation, every temp must denote a physical register.

-record(alu, {aluop, src, dst}).
-record(call, {'fun', sdesc, linkage}).
-record(cmovcc, {cc, src, dst}).
-record(cmp, {src, dst}).       	% a 'sub' alu which doesn't update dst
-record(comment, {term}).
-record(dec, {dst}).
-record(fmove, {src, dst}).
-record(fp_binop, {op, src, dst}).
-record(fp_unop, {op, arg}).
-record(inc, {dst}).
-record(jcc, {cc, label}).
-record(jmp_fun, {'fun', linkage}).	% tailcall, direct or indirect
-record(jmp_label, {label}).		% local jmp, direct
-record(jmp_switch, {temp, jtab, labels}).	% local jmp, indirect
-record(label, {label}).
-record(lea, {mem, temp}).
-record(move, {src, dst}).
-record(move64, {imm, dst}).
-record(movsx, {src, dst}).
-record(movzx, {src, dst}).
-record(nop, {}).
-record(prefix_fs, {}).
-record(pseudo_call, {'fun', sdesc, contlab, linkage}).
-record(pseudo_jcc, {cc, true_label, false_label, pred}).
-record(pseudo_tailcall, {'fun', arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(push, {src}).
-record(pop, {dst}).
-record(ret, {npop}).			% EAX is live-in
-record(shift, {shiftop, src, dst}).
-record(test, {src, dst}).
%%% Function definitions.

-record(defun, {mfa, formals, code, data, isclosure, isleaf,
		var_range, label_range}).
