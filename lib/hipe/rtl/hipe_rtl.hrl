%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Provides the abstract datatypes for HiPE's RTL (Register Transfer Language).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(alu, {dst, src1, op, src2}).
-record(alub, {dst, src1, op, src2, 'cond', true_label, false_label, p}).
-record(branch, {src1, src2, 'cond', true_label, false_label, p}).
-record(call, {dstlist, 'fun', arglist, type, continuation, failcontinuation}).
-record(comment, {text}).
-record(enter, {'fun', arglist, type}).
-record(fconv, {dst, src}).
-record(fixnumop, {dst, src, type}).
-record(fload, {dst, src, offset}).
-record(fmove, {dst, src}).
-record(fp, {dst, src1, op, src2}).
-record(fp_unop, {dst, src, op}).
-record(fstore, {base, offset, src}).
-record(gctest, {words}).
-record(goto, {label}).
-record(goto_index, {block, index, labels}).
-record(label, {name}).
-record(load, {dst, src, offset, size, sign}).
-record(load_address, {dst, addr, type}).
-record(load_atom, {dst, atom}).
-record(load_word_index, {dst, block, index}).
-record(move, {dst, src}).
-record(multimove, {dstlist, srclist}).
-record(phi, {dst, id, arglist}).
-record(return, {varlist}).
-record(store, {base, offset, src, size}).
-record(switch, {src, labels, sorted_by=[]}).

%%---------------------------------------------------------------------

%% An efficient macro to convert byte sizes to bit sizes
-define(bytes_to_bits(Bytes), ((Bytes) bsl 3)).  % (N * 8)

%%---------------------------------------------------------------------
