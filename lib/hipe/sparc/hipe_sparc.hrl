%% Copyright (c) 1997 by HiPE.  All Rights Reserved 
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

-record(pseudo_enter, {target,args,type,info=[]}).
-record(pseudo_return, {regs,info=[]}).
-record(pseudo_spill, {source,dest,info=[]}).
-record(pseudo_unspill, {source,dest,info=[]}).
-record(pseudo_push, {reg,info=[]}).
-record(pseudo_pop, {reg,index,info=[]}).

-record(load_atom, {dst, atom, info=[]}).
-record(load_address, {dst, address, type, info=[]}).
-record(load_word_index, {dst,block,index,info=[]}).

-record(label, {id,info=[]}).
-record(nop, {info=[]}).
-record(block, {size, info=[]}).
-record(align, {alignment=4, info=[]}).
-record(comment, {text="", info=[]}).
-record(move, {dst, src, info=[]}).
-record(multimove, {dst, src, info=[]}).
-record(cmov_cc, {dst, src, cc, info=[]}).
-record(cmov_r, {dst, src, reg, rcc, info=[]}).
-record(alu, {dst, src1, op, src2, info=[]}).
-record(alu_cc, {dst, src1, op, src2, info=[]}).
-record(sethi, {dst, const, info=[]}).
-record(load, {dst, type, src, off, info=[]}).
-record(store, {dst, off, type, src, info=[]}).
-record(b, {cc, true_label, false_label, pred, annul=na, info=[]}).
-record(br, {reg, rcc, true_label, false_label, pred, annul=na, info=[]}).
-record(goto, {label, info=[]}).
-record(jmp_link, {target, off, link, args, continuation_label,
		   fail_label, info=[]}).
-record(jmp, {target, off, args, fail_label, destinations= [], info=[]}).
-record(call_link, {target, link, dests, args, continuation_label,
		    fail_label, type ,
		    stack_descriptor, info=[]}).
%% Floating point operations
-record(load_fp, {dst, align, type, src, off, info=[]}).
-record(store_fp, {dst, off, type, align, src, info=[]}).
-record(fb, {fcc, n=0, true_label, false_label, pred, annul=na, info=[]}).
-record(fop, {dst, type, src1, fop, src2, info=[]}).
-record(fcmp, {fccn, src1, type, src2, exception, info=[]}).
-record(fmov, {dst, type, src, negate, abs, info=[]}).
-record(conv_fp, {dst, dst_type, src, src_type, info=[]}).
