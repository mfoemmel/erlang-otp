
-record(rtl, {'fun',        %% Name of the function (MFA)
	      args,         %% List of argument names (formals)
	      closure,      %% True if this is code for a closure.
	      leaf,         %% True if this is a leaf function.
	      code,         %% Linear list of rtl-instructions.
	      data,         %% Data segment
	      var_range,    %% {Min,Max} First and last name used for
	                    %%           regs fpregs or vars. 
	                    %%           (they use a common namespace)
	      label_range,  %% {Min,Max} First and last name used for labels
	      info=[]       %% A keylist with arbitrary information.
	     }).

-record(move, {dst, src, info}).
-record(multimove, {dst, src, info}).
-record(alu, {dst, src1, op, src2, info}).
-record(load, {dst, src, off, size, sign, info}).
-record(store, {dst, off, src, size, info}).
-record(load_address, {dst, address, type, info}).
-record(branch, {src1, src2, 'cond', true_label, false_label, p, info}).
-record(switch, {src, labels, sorted_by=[], info}).
-record(alub, {dst, src1, op, src2, 'cond', true_label, false_label, p, info}).
-record(call, {dst, 'fun', args, type, continuation, failcontinuation, info}).
-record(enter, {'fun', args, type, info}).
-record(return, {vars, info}).
-record(gctest, {words, info}).
-record(load_atom, {dst, atom, info}).
-record(load_word_index, {dst, block, index, info}).
-record(goto_index, {block, index, labels, info}).
-record(label, {name, info}).
-record(restore_catch, {vars, info}).
-record(comment, {text, info}).
-record(goto, {label, info}).
-record(fail_to, {reason, label, info}).
-record(fload, {dst, src, off, info}).
-record(fstore, {dst, off, src, info}).
-record(fmov, {dst, src, negate, info=[]}).
-record(fp, {dst, src1, op, src2, info}).
-record(fconv, {dst, src, info=[]}).
