%% Copyright (c) 1997 by HiPE.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_sparc.hrl
%%  Module   :	hipe_sparc
%%  Group    :  Optimizer
%%  Purpose  :  Definition of SPARC three address instuction
%%  History  :	* 1997-04-01 Jan Sj|din (jans@csd.uu.se): Created.
%% ====================================================================
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(sparc, {'fun', code, data, var_range, label_range}).

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
-record(call_link, {target, link, args, continuation_label,
		    fail_label, type ,info=[]}).
-record(load_atom, {dst, atom, info=[]}).
-record(load_address, {dst, address, type, info=[]}).
-record(load_word_index, {dst,block,index,info=[]}).

