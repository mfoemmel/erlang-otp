%%% $Id$
%%%
%%% Translate 3-address RTL code to 2-address pseudo-x86 code.
%%% RTL's frame module MUST NOT have munged the RTL code.

-module(hipe_rtl_to_x86).
-export([translate/1]).

translate(RTL) ->	% RTL function -> x86 defun
    hipe_gensym:set_var(hipe_x86_registers:first_virtual()),
    hipe_gensym:set_label(x86, hipe_gensym:get_label(rtl)),
    Map0 = vmap_empty(),
    {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0, []),
    OldData = hipe_rtl:rtl_data(RTL),
    {Code, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
    {LabelsLo, _} = hipe_rtl:rtl_label_range(RTL),
    NewLabels = {LabelsLo, hipe_gensym:get_label(x86)},
    hipe_x86:mk_defun(conv_mfa(hipe_rtl:rtl_fun(RTL)),
		      Formals,
		      Code,
		      NewData,
		      {1, hipe_gensym:get_var()},
		      NewLabels).

conv_insn_list([H|T], Map, Data) ->
    {NewH, NewMap, NewData1} = conv_insn(H, Map, Data),
    %% io:format("~w \n  ==>\n ~w\n- - - - - - - - -\n",[H,NewH]),
    {NewT, NewData2} = conv_insn_list(T, NewMap, NewData1),
    {NewH ++ NewT, NewData2};
conv_insn_list([], _, Data) ->
    {[], Data}.

conv_insn(I, Map, Data) ->
    case hipe_rtl:type(I) of
	alu ->
	    %% dst = src1 binop src2
	    BinOp = conv_binop(hipe_rtl:alu_op(I)),
	    {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
	    {Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
	    {Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
	    I2 = conv_alu(Dst, Src1, BinOp, Src2, []),
	    {I2, Map2, Data};
	alub ->
	    %% dst = src1 op src2; if COND goto label
	    BinOp = conv_binop(hipe_rtl:alub_op(I)),
	    {Dst, Map0} = conv_dst(hipe_rtl:alub_dst(I), Map),
	    {Src1, Map1} = conv_src(hipe_rtl:alub_src1(I), Map0),
	    {Src2, Map2} = conv_src(hipe_rtl:alub_src2(I), Map1),
	    Cond = conv_cond(hipe_rtl:alub_cond(I)),
	    I1 = [hipe_x86:mk_pseudo_jcc(Cond,
					 hipe_rtl:alub_true_label(I),
					 hipe_rtl:alub_false_label(I),
					 hipe_rtl:alub_pred(I))],
	    I2 = conv_alu(Dst, Src1, BinOp, Src2, I1),
	    {I2, Map2, Data};
	branch ->
	    %% <unused> = src1 - src2; if COND goto label
	    {Src1, Map0} = conv_src(hipe_rtl:branch_src1(I), Map),
	    {Src2, Map1} = conv_src(hipe_rtl:branch_src2(I), Map0),
	    Cond = conv_cond(hipe_rtl:branch_cond(I)),
	    I2 = conv_branch(Src1, Cond, Src2,
			     hipe_rtl:branch_true_label(I),
			     hipe_rtl:branch_false_label(I),
			     hipe_rtl:branch_pred(I)),
	    {I2, Map1, Data};
	call ->
	    %%	push <exn handler>
	    %%	push <arg1>
	    %%	...
	    %%	push <argn>
	    %%	eax := call <Fun>; if exn goto <Fail> else goto Next
	    %% Next:
	    %%	<Dst> := eax
	    %%	goto <Cont>
	    {Args, Map0} = conv_src_list(hipe_rtl:call_args(I), Map),
	    {Dsts, Map1} = conv_dst_list(hipe_rtl:call_dst(I), Map0),
	    {Fun, Map2} = conv_fun(hipe_rtl:call_fun(I), Map1),
	    I2 = conv_call(Dsts, Fun, Args,
			   hipe_rtl:call_continuation(I),
			   hipe_rtl:call_fail(I)),
	    {I2, Map2, Data};
	comment ->
	    I2 = [hipe_x86:mk_comment(hipe_rtl:comment_text(I))],
	    {I2, Map, Data};
	enter ->
	    {Args, Map0} = conv_src_list(hipe_rtl:enter_args(I), Map),
	    {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
	    I2 = [hipe_x86:mk_pseudo_tailcall(Fun, Args)],
	    {I2, Map1, Data};
	fail_to ->		% for SPARC this is eliminated by hipe_frame
	    {Src, Map0} = conv_src(hipe_rtl:fail_to_reason(I), Map),
	    Dst = mk_eax(),
	    I2 = [hipe_x86:mk_move(Src, Dst),
		  hipe_x86:mk_jmp_label(hipe_rtl:fail_to_label(I))],
	    {I2, Map0, Data};
	goto ->
	    I2 = [hipe_x86:mk_jmp_label(hipe_rtl:goto_label(I))],
	    {I2, Map, Data};
	label ->
	    %% Shouldn't the IsFail thing be obsolete now?
	    IsFail = lists:member('entry', hipe_rtl:info(I)),
	    I2 = [hipe_x86:mk_label(hipe_rtl:label_name(I), IsFail)],
	    {I2, Map, Data};
	load ->
	    {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
	    {Src, Map1} = conv_src(hipe_rtl:load_src(I), Map0),
	    {Off, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
	    Type = typeof_dst(Dst),
	    I2 = case hipe_x86:is_imm(Src) of
		     false ->
			 [hipe_x86:mk_move(hipe_x86:mk_mem(Src, Off, Type), Dst)];
		     true ->
			 %% XXX: this is temporary until rtl_prop gets fixed
			 io:format(standard_io, "hipe_rtl_to_x86: ERROR: ignoring bogus RTL load ~w\n", [I]),
			 [hipe_x86:mk_comment(I)]
		 end,
	    {I2, Map2, Data};
	load_address ->
	    {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
	    Addr = hipe_rtl:load_address_address(I),
	    Type = hipe_rtl:load_address_type(I),
	    Src = hipe_x86:mk_imm_from_addr(Addr, Type),
	    I2 = [hipe_x86:mk_move(Src, Dst)],
	    {I2, Map0, Data};
	load_atom ->
	    {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
	    Src = hipe_x86:mk_imm_from_atom(hipe_rtl:load_atom_atom(I)),
	    I2 = [hipe_x86:mk_move(Src, Dst)],
	    {I2, Map0, Data};
	move ->
	    {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
	    {Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
	    I2 = [hipe_x86:mk_move(Src, Dst)],
	    {I2, Map1, Data};
	restore_catch ->	% for SPARC this is eliminated by hipe_frame
	    [Dst0] = hipe_rtl:restore_catch_vars(I),
	    {Dst1,Map1} = conv_dst(Dst0, Map),
	    Src = mk_eax(),
	    {[hipe_x86:mk_move(Src, Dst1)], Map1, Data};
	return ->
	    %% TODO: multiple-value returns
	    {[Arg], Map0} = conv_src_list(hipe_rtl:return_vars(I), Map),
	    Dst = mk_eax(),
	    I2 = [hipe_x86:mk_move(Arg, Dst),
		  hipe_x86:mk_ret(-1)],	% frame will fill in npop later
	    {I2, Map0, Data};
	store ->
	    {Ptr, Map0} = conv_dst(hipe_rtl:store_dst(I), Map),
	    {Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
	    {Off, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
	    Type = typeof_src(Src),
	    I2 = [hipe_x86:mk_move(Src, hipe_x86:mk_mem(Ptr, Off, Type))],
	    {I2, Map2, Data};
	switch ->	% this one also updates Data :-(
	    %% from hipe_rtl2sparc, but we use a hairy addressing mode
	    %% instead of doing the arithmetic manually
	    Labels = hipe_rtl:switch_labels(I),
	    LMap = lists:map(fun (L) -> {label,L} end, Labels),
	    {NewData, JTabLab} =
		case hipe_rtl:switch_sort_order(I) of
		    [] ->
			hipe_consttab:insert_block(Data, 4, word, LMap);
		    SortOrder ->
			hipe_consttab:insert_sorted_block(
			  Data, 4, word, LMap, SortOrder)
		end,
	    %% no immediates allowed here
	    {Index, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
	    %% this is equivalent to "jmp *JTabLab(,Index,4)"
	    %% ("r = Index; r *= 4; r += &JTab; jmp *r" isn't as nice)
	    I2 = [hipe_x86:mk_jmp_switch(Index, JTabLab, Labels)],
	    {I2, Map1, NewData};
	X ->
	    %% gctest??
	    %% jmp, jmp_link, jsr, esr, multimove,
	    %% stackneed, pop_frame, restore_frame, save_frame
	    throw({?MODULE, {"unknown RTL instruction", X}})
    end.

%%% Finalise the conversion of a 3-address ALU operation, taking
%%% care to not introduce more temps and moves than necessary.

conv_alu(Dst, Src1, BinOp, Src2, Tail) ->
    case same_opnd(Dst, Src1) of
	true ->			% x = x op y
	    [hipe_x86:mk_alu(BinOp, Src2, Dst) | Tail];		% x op= y
	false ->		% z = x op y, where z != x
	    case same_opnd(Dst, Src2) of
		false ->	% z = x op y, where z != x && z != y
		    [hipe_x86:mk_move(Src1, Dst),			% z = x
		     hipe_x86:mk_alu(BinOp, Src2, Dst) | Tail];	% z op= y
		true ->		% y = x op y, where y != x
		    case binop_commutes(BinOp) of
			true ->	% y = y op x
			    [hipe_x86:mk_alu(BinOp, Src1, Dst) | Tail]; % y op= x
			false ->% y = x op y, where op doesn't commute
			    Tmp = clone_dst(Dst),
			    [hipe_x86:mk_move(Src1, Tmp),		% t = x
			     hipe_x86:mk_alu(BinOp, Src2, Tmp),	% t op= y
			     hipe_x86:mk_move(Tmp, Dst) | Tail]	% y = t
		    end
	    end
    end.

%%% Finalise the conversion of a conditional branch operation, taking
%%% care to not introduce more temps and moves than necessary.

conv_branch(Src1, Cond, Src2, TrueLab, FalseLab, Pred) ->
    case hipe_x86:is_imm(Src1) of
	false ->
	    mk_branch(Src1, Cond, Src2, TrueLab, FalseLab, Pred);
	true ->
	    case hipe_x86:is_imm(Src2) of
		false ->
		    NewCond = commute_cond(Cond),
		    mk_branch(Src2, NewCond, Src1, TrueLab, FalseLab, Pred);
		true ->
		    %% two immediates, let the optimiser clean it up
		    Tmp = new_untagged_temp(),
		    [hipe_x86:mk_move(Src1, Tmp) |
		     mk_branch(Tmp, Cond, Src2, TrueLab, FalseLab, Pred)]
	    end
    end.

mk_branch(Src1, Cond, Src2, TrueLab, FalseLab, Pred) ->
    %% PRE: not(is_imm(Src1))
    [hipe_x86:mk_cmp(Src2, Src1),
     hipe_x86:mk_pseudo_jcc(Cond, TrueLab, FalseLab, Pred)].

%%% Convert an RTL ALU or ALUB binary operator.

conv_binop(BinOp) ->
    case BinOp of
	'add'	-> 'add';
	'sub'	-> 'sub';
	'or'	-> 'or';
	'and'	-> 'and';
	'xor'	-> 'xor';
	'sll'	-> 'shl';
	'srl'	-> 'shr';
	'sra'	-> 'sar';
	%% mul, andnot ???
	_	-> exit({?MODULE, {"unknown binop", BinOp}})
    end.

binop_commutes(BinOp) ->
    case BinOp of
	'add'	-> true;
	'or'	-> true;
	'and'	-> true;
	'xor'	-> true;
	_	-> false
    end.

%%% Convert an RTL conditional operator.

conv_cond(Cond) ->
    case Cond of
	eq	-> 'e';
	ne	-> 'ne';
	gt	-> 'g';
	gtu	-> 'a';
	ge	-> 'ge';
	geu	-> 'ae';
	lt	-> 'l';
	ltu	-> 'b';
	le	-> 'le';
	leu	-> 'be';
	overflow -> 'o';
	not_overflow -> 'no';
	_	-> exit({?MODULE, {"unknown cond", Cond}})
    end.

commute_cond(Cond) ->	% if x cond y, then y commute_cond(cond) x
    case Cond of
	'e'	-> 'e';		% ==, ==
	'ne'	-> 'ne';	% !=, !=
	'g'	-> 'l';		% >, <
	'a'	-> 'b';		% >u, <u
	'ge'	-> 'le';	% >=, <=
	'ae'	-> 'be';	% >=u, <=u
	'l'	-> 'g';		% <, >
	'b'	-> 'a';		% <u, >u
	'le'	-> 'ge';	% <=, >=
	'be'	-> 'ae';	% <=u, >=u
	%% overflow/not_overflow: n/a
	_	-> exit({?MODULE, {"unknown cond", Cond}})
    end.

%%% Test if Dst and Src are the same operand.

same_opnd(Dst, Src) -> Dst =:= Src.

%%% Finalise the conversion of a call instruction.

conv_call(Dsts, Fun, Args, ContLab, ExnLab) ->
    {RealDsts, RealContLab, Tail} =
	case do_call_results(Dsts) of
	    {[], []} ->
		%% Avoid consing up a dummy basic block if the moves list
		%% is empty, as is typical for calls to suspend/0.
		%% This would be subsumed by a general "optimise the CFG"
		%% module, but we don't have that one yet :-(
		{[], ContLab, []};
	    {Moves, NewDsts} ->
		%% Change the call to continue at a new basic block.
		%% In this block, move the result registers to the Dsts,
		%% then continue at the call's original continuation.
		NewContLab = hipe_gensym:get_next_label(x86),
		{NewDsts, NewContLab,
		 [hipe_x86:mk_label(NewContLab, false) |
		  Moves ++
		  [hipe_x86:mk_jmp_label(ContLab)]]}
	end,
    CallInsn = hipe_x86:mk_pseudo_call(RealDsts, Fun, length(Args),
				       RealContLab, ExnLab),
    do_call_args(Args, [CallInsn | Tail]).

do_call_args([Arg|Args], Tail) ->
    [hipe_x86:mk_push(Arg) | do_call_args(Args, Tail)];
do_call_args([], Tail) ->
    Tail.

do_call_results([]) ->
    {[], []};
do_call_results([Dst]) ->
    EAX = hipe_x86:mk_temp(hipe_x86_registers:eax(), 'tagged'),
    MV = hipe_x86:mk_move(EAX, Dst),
    {[MV], [EAX]};
do_call_results(Dsts) ->
    exit({?MODULE,do_call_results,Dsts}).

%%% Convert a 'fun' operand (MFA, prim, or temp)

conv_fun(Fun, Map) ->
    case hipe_rtl:is_var(Fun) of
	true ->
	    conv_dst(Fun, Map);
	false ->
	    case hipe_rtl:is_reg(Fun) of
		true ->
		    conv_dst(Fun, Map);
		false ->
		    case Fun of
			Prim when atom(Prim) ->
			    {hipe_x86:mk_prim(Prim), Map};
			{M,F,A} when atom(M), atom(F), integer(A) ->
			    {hipe_x86:mk_mfa(M,F,A), Map};
			_ ->
			    exit({?MODULE,conv_fun,Fun})
		    end
	    end
    end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) ->
    hipe_x86:mk_mfa(M, F, A).

%%% Convert an RTL source operand (imm/var/reg).

conv_src(Opnd, Map) ->
    case hipe_rtl:is_imm(Opnd) of
	true ->
	    {hipe_x86:mk_imm(hipe_rtl:imm_value(Opnd)), Map};
	false ->
	    conv_dst(Opnd, Map)
    end.

conv_src_list([O|Os], Map) ->
    {V, Map1} = conv_src(O, Map),
    {Vs, Map2} = conv_src_list(Os, Map1),
    {[V|Vs], Map2};
conv_src_list([], Map) ->
    {[], Map}.

%%% Convert an RTL destination operand (var/reg).

conv_dst(Opnd, Map) ->
    {Name, Type} =
	case hipe_rtl:is_var(Opnd) of
	    true ->
		{hipe_rtl:var_name(Opnd), 'tagged'};
	    false ->
		{hipe_rtl:reg_name(Opnd), 'untagged'}
	end,
    case hipe_x86_registers:is_precoloured(Name) of
	true ->
	    case hipe_x86_registers:proc_offset(Name) of
		false ->
		    {hipe_x86:mk_temp(Name, Type), Map};
		Offset ->
		    Preg = hipe_x86_registers:proc_pointer(),
		    Pbase = hipe_x86:mk_temp(Preg, 'untagged'),
		    Poff = hipe_x86:mk_imm(Offset),
		    {hipe_x86:mk_mem(Pbase, Poff, Type), Map}
	    end;
	false ->
	    case vmap_lookup(Map, Opnd) of
		{value, {_, NewTemp}} ->
		    {NewTemp, Map};
		false ->
		    NewTemp = hipe_x86:mk_new_temp(Type),
		    {NewTemp, vmap_bind(Map, Opnd, NewTemp)}
	    end
    end.

conv_dst_list([O|Os], Map) ->
    {Dst, Map1} = conv_dst(O, Map),
    {Dsts, Map2} = conv_dst_list(Os, Map1),
    {[Dst|Dsts], Map2};
conv_dst_list([], Map) ->
    {[], Map}.

conv_formals([O|Os], Map, Res) ->
    Type =
	case hipe_rtl:is_var(O) of
	    true -> 'tagged';
	    false -> 'untagged'
	end,
    Dst = hipe_x86:mk_new_nonallocatable_temp(Type),
    Map1 = vmap_bind(Map, O, Dst),
    conv_formals(Os, Map1, [Dst|Res]);
conv_formals([], Map, Res) ->
    {lists:reverse(Res), Map}.

%%% typeof_src -- what's src's type?

typeof_src(Src) ->
    case hipe_x86:is_imm(Src) of
	true ->
	    'untagged';
	_ ->
	    typeof_dst(Src)
    end.

%%% typeof_dst -- what's dst's type?

typeof_dst(Dst) ->
    case hipe_x86:is_temp(Dst) of
	true ->
	    hipe_x86:temp_type(Dst);
	_ ->
	    hipe_x86:mem_type(Dst)
    end.

%%% clone_dst -- conjure up a scratch reg with same type as dst

clone_dst(Dst) ->
    hipe_x86:mk_new_temp(typeof_dst(Dst)).

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
    hipe_x86:mk_new_temp('untagged').

%%% Cons up a tagged '%eax' Temp.

mk_eax() ->
    hipe_x86:mk_temp(hipe_x86_registers:eax(), 'tagged').

%%% Map from RTL var/reg operands to x86 temps.

vmap_empty() ->
    [].

vmap_lookup(VMap, Opnd) ->
    lists:keysearch(Opnd, 1, VMap).

vmap_bind(VMap, Opnd, Temp) ->
    [{Opnd, Temp} | VMap].
