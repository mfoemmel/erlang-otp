
%%
%% Definition of high level three-address instructions
%%

-record(icode, {'fun', params, closure, leaf, 
		code, data, var_range, label_range, info=[]}).

-record('if', {op, args, true_label, false_label, p, info=[]}).
-record(switch_val, {arg, fail_label, length, cases, info=[]}).
-record(switch_tuple_arity, {arg, fail_label, length, cases, info=[]}).
-record(type, {type, var, true_label, false_label, p, info=[]}).
-record(goto, {label, info=[]}).
-record(mov, {dst, src, info=[]}).
-record(fail, {reason,  type, info=[]}).
-record(call, {dst, 'fun', args, type, continuation_label, fail_label=[],
	       in_guard=false, code_change=true, info=[]}).
-record(enter, {'fun', args, type, code_change=true, info=[]}).
-record(return, {vars, info=[]}).
-record(pushcatch, {label, info=[]}).
-record(restore_catch, {dst, id, info=[]}).
-record(remove_catch, {id, info=[]}).
-record(comment, {text, info=[]}).
-record(label, {name, info=[]}).
-record(fclearerror, {info=[]}).
-record(fmov, {dst, src, negate, info=[]}).
-record(unsafe_untag_float, {dst, src, info=[]}).
-record(unsafe_tag_float, {dst, src, info=[]}).
-record(conv_to_float, {dst, src, info=[]}).
-record(phi, {dst, name, args, predList, info=[]}).


