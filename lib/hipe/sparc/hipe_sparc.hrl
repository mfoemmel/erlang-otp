%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 1997 by the HiPE group.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_sparc.hrl
%%  Module   :	hipe_sparc
%%  Purpose  :  Definition of SPARC three address instuction
%%  History  :	* 1997-04-01 Jan Sjödin (jans@csd.uu.se): Created.
%% ====================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(sparc, {'fun', closure, arity, leaf, code, data, var_range, label_range}).
%% Arity is needed as a separate field since the arity in 'fun' is 
%% incorrect for closures.

-record(pseudo_enter, {target,args,type}).
-record(pseudo_return, {regs}).
-record(pseudo_spill, {source,dest}).
-record(pseudo_unspill, {source,dest}).
-record(pseudo_push, {reg}).
-record(pseudo_pop, {reg,index}).

-record(load_atom, {dst, atom}).
-record(load_address, {dst, address, type}).
-record(load_word_index, {dst,block,index}).

-record(label, {id}).
-record(nop, {}).
-record(block, {size}).
-record(align, {alignment=4}).
-record(comment, {text=""}).
-record(move, {dst, src}).
-record(multimove, {dst, src}).
-record(cmov_cc, {dst, src, cc}).
-record(cmov_r, {dst, src, reg, rcc}).
-record(alu, {dst, src1, op, src2}).
-record(alu_cc, {dst, src1, op, src2}).
-record(sethi, {dst, const}).
-record(load, {dst, type, src, off}).
-record(store, {dst, off, type, src}).
-record(b, {cc, true_label, false_label, pred, annul=na}).
-record(br, {reg, rcc, true_label, false_label, pred, annul=na}).
-record(goto, {label}).
-record(jmp_link, {target, off, link, args, continuation_label,
		   fail_label}).
-record(jmp, {target, off, args, fail_label, destinations=[]}).
-record(call_link, {target, link, dests, args, continuation_label,
		    fail_label, type,
		    stack_descriptor}).
%% Floating point operations
-record(load_fp, {dst, align, type, src, off}).
-record(store_fp, {dst, off, type, align, src}).
-record(fb, {fcc, n=0, true_label, false_label, pred, annul=na}).
-record(fop, {dst, type, src1, fop, src2}).
-record(fcmp, {fccn, src1, type, src2, exception}).
-record(fmove, {dst, type, src, negate, abs}).
-record(conv_fp, {dst, dst_type, src, src_type}).
