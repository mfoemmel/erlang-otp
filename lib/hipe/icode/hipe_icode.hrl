%%=====================================================================
%%
%% Contains record definitions for all Icode instructions.
%%
%%=====================================================================

-record(icode, {'fun'		:: mfa(),
		params,		% :: [var()],
		is_closure	:: bool(),
		closure_arity	:: byte(),
		is_leaf 	:: bool(),
		code,		% :: [icode_instruction()],
		data,
		var_range	:: {integer(), integer()},
		label_range	:: {integer(), integer()},
		info=[]}).

-record('if', {op, args, true_label, false_label, p :: float()}).

-record(switch_val, {arg, fail_label, length, cases}).

-record(switch_tuple_arity, {arg, fail_label, length, cases}).

-record(type, {type, args, true_label, false_label, p :: float()}).

-record(goto, {label}).

-record(label, {name :: integer()}). % actually non-negative

-record(move, {dst, src}).

-record(fmove, {dst, src}).

-record(phi, {dst, id, arglist}).

-record(call, {dstlist,
	       'fun' :: mfa() | atom() | tuple(),
	       args,
	       type  :: 'local' | 'primop' | 'remote',
	       continuation,
	       fail_label=[],
	       in_guard=false :: bool(),
	       dst_type=[]}).

-record(enter, {'fun',
		args,
		type :: 'local' | 'primop' | 'remote'}).

-record(return, {vars}).

-record(begin_try, {label, successor}).

-record(end_try, {}).

-record(begin_handler, {dstlist}).

-record(fail, {class, args, fail_label=[]}).

-record(comment, {text}).
