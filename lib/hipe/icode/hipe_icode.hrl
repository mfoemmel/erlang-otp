%%
%% This should only be used by hipe_icode.erl and hipe_beam_to_icode.erl!
%%

-record(icode, {'fun', params, closure, closure_arity, leaf, 
		code, data, var_range, label_range, info=[]}).

-record('if', {op, args, true_label, false_label, p}).

-record(switch_val, {arg, fail_label, length, cases}).

-record(switch_tuple_arity, {arg, fail_label, length, cases}).

-record(type, {type, args, true_label, false_label, p}).

-record(goto, {label}).

-record(label, {name}).

-record(move, {dst, src}).

-record(fmove, {dst, src}).

-record(phi, {dst, id, arglist}).

-record(call, {dstlist, 'fun', args, type, continuation, fail_label=[],
	       in_guard=false, dst_type=[]}).

-record(enter, {'fun', args, type}).

-record(return, {vars}).

-record(begin_try, {label, successor}).

-record(end_try, {}).

-record(begin_handler, {dstlist}).

-record(fail, {class, args, fail_label=[]}).

-record(comment, {text}).
