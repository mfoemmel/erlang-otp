%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%% Cleanups output from compiler before presenting it for the assembler.

-module(v1_cleanup).

-export([module/2]).

-record(state,
	{module,				% Module name (atom).
	 label,					% Next free label.
	 func_info}).				% Label for func_info instruction.


%%
%% Some serious words before we start.
%%
%% This module cleans the code from the compiler to make it acceptable
%% for the assembler.  When doing this, it tries to remove any unreachable
%% code.  To do this with no extra passes over the code, it depends on
%% the property of Erlang properties that all branch instructions go
%% forward in the code, never backwards.  (Receive is an exception, but
%% it is specially treated for other reasons and is no problem to handle.)
%%
%% Therefore, don't introduce any optimizations that generate backward
%% jumps (loops for instance).
%%

module({Mod, Exports, Attr, Code, NumLabels}, Opts) ->
    ets:new(beam_labels, [set, private, named_table]),
    ets:new(beam_case, [set, private, named_table]),
    ets:new(beam_moved, [set, private, named_table]),
    St = #state{module = Mod, label = NumLabels},
    module1(Code, [], St, Exports, Attr).

module1([H|T], Acc, St0, Exports, Attr) ->
    {Code, St} = function(H, St0),
    module1(T, [Code|Acc], St, Exports, Attr);
module1([], Code, #state{module=Mod, label=Lcount}, Exports, Attr) ->
%    Code = remove_unused_funcs(Code0, [], Exports),
    ets:delete(beam_labels),
    ets:delete(beam_case),
    ets:delete(beam_moved),
    {ok,{Mod, Exports, Attr, lists:reverse(Code), Lcount}}.

%remove_unused_funcs([Func|T], Acc, Exports) ->
%    case function_referenced(Func, Exports) of
%	true -> remove_unused_funcs(T, [Func|Acc], Exports);
%	false -> remove_unused_funcs(T, Acc, Exports)
%    end;
%remove_unused_funcs([], Acc, Exports) ->  
%    Acc.

%function_referenced({function, Name, Arity, Entry, Code}, Exports) ->
%    case ets:lookup(beam_labels, Entry) of
%	[] -> lists:member({Name, Arity}, Exports);
%	_ -> true
%    end.

function({code, Mod, Name, Arity, Type, [Label| Code]}, St0) ->
    {Entry, Prefix, St1} = cleanup_label(Label, St0),
    {Code1, St2} = cleanup(Code, Prefix, St1),
    {Lbls, St3} =
	case St2#state.func_info of
	    undefined ->
		{L0, NewState} = next_label(St2),
		{[{label, L0}], NewState};
	    Lb ->
		{Lb, St2}
	end,
    {{function, Name, Arity, Entry, Lbls++Code1}, St3};
function({asm, Mod, Name, Arity, Type, Entry, Code0}, St0) ->
    {Lbl, St1} = next_label(St0),
    Code1 = [{label, Lbl}, {func_info, {atom,Mod}, {atom,Name}, Arity},
	     {label,Entry}|Code0],
    {{function, Name, Arity, Entry, Code1}, St1}.

cleanup_label({label, Name, Arity, Entry, E}, St) ->
    Mod = St#state.module,
    Prefix = [{label,Entry}, {func_info, {atom,Mod}, {atom,Name}, Arity}],
%%    label_is_used(Entry),
    {Entry, Prefix, St#state{func_info=undefined}}.

%%
%% Cleanup code to make it acceptable to assembler.
%%

%% Add labels instead of various end constructs.

cleanup([{label, Lbl}| T], Acc, St) ->		% Added by build_receive.
    cleanup(T, add_label(Lbl, Acc), St);

cleanup([{clause_end, L}|T], Acc, St) ->
    cleanup(T, add_label(L, Acc), St);

cleanup([{commonCl_end, L}|T], Acc, St) ->
    cleanup(T, add_label(L, Acc), St);

cleanup([{commonTestPattern_end, L}|T], Acc, St) ->
    cleanup(T, add_label(L, Acc), St);

cleanup([{testPattern_end, Lb1, 0}|T], Acc, St) ->
    cleanup(T, add_if_not_dead({jump, {f, Lb1}}, Acc), St);

cleanup([{testPattern_end, Lb1, Lb2}|T], Acc, St) ->
    cleanup(T, add_label(Lb2, add_if_not_dead({jump, {f, Lb1}}, Acc)), St);

cleanup([{receiveEnd, Lb}|T], Acc, St) ->
    cleanup(T, add_label(Lb, Acc), St);

cleanup([{fail, Const, W}|T], Acc, St) ->
    cleanup(T, [cleanup_fail(Const, W, St)|Acc], St);

cleanup([{'catch', {catchEnd, Y, Lb}}|T], Acc, St) ->
    cleanup(T, [{catch_end, Y}| add_label(Lb, Acc)], St);

cleanup([{caseEnd, Lb, _, R}|T], Acc, St) ->
    Ce = {case_end, R},
    cleanup(T, add_label(Lb, add_if_not_dead(Ce, Acc)), St);

cleanup([{ifEnd, Lb, _}|T], Acc, St) ->
    cleanup(T, add_label(Lb, add_if_not_dead(if_end, Acc)), St);

cleanup([{switchlabel, N}|T], Acc, St) ->
    cleanup(T, add_label(N, Acc), St);

cleanup([{'receive', Lb, Y}|T], Acc, St0) ->
    {Code, St} = build_receive(T, Lb, Y, [], St0),
    cleanup(Code, add_label(Lb, Acc), St);

%% Remove dead code (everything after, for instance, 'jump' up to the next label).

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == jump ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == badmatch ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == call_last ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == call_only ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == call_ext_last ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [return| Acc], St) ->
    cleanup(T, [return| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == case_end ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == select_val ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [DeadAfter| Acc], St) when element(1, DeadAfter) == wait ->
    cleanup(T, [DeadAfter| Acc], St);

cleanup([H|T], [{call_ext, 1, {extfunc, erlang, exit, 1}}| Acc], St) ->
    cleanup(T, [{call_ext, 1, {extfunc, erlang, exit, 1}}| Acc], St);

cleanup([H|T], [{call_ext, 1, {extfunc, erlang, throw, 1}}| Acc], St) ->
    cleanup(T, [{call_ext, 1, {extfunc, erlang, throw, 1}}| Acc], St);

%% Cleanup other stuff.  Dead stuff has been removed.

cleanup([{select_val, R, Lbl, {list, Cases}}|T], Acc, St) ->
    label_is_used(Lbl),
    lists:foreach(fun ({f, L}) -> label_is_used(L);
		      (_) -> ok end, Cases),
    cleanup(T, [{select_val, R, Lbl, {list, Cases}}|Acc], St);

cleanup([{allocate, Need, Regs}| T], Acc, St) ->
    cleanup_allocation(T, {allocate, Need, Regs}, Need, [], Acc, St);

cleanup([{allocateH, Need, Regs, Heap}|T], Acc, St) ->
    cleanup_allocation(T, {allocate_heap, Need, Regs, Heap}, Need, [], Acc, St);

cleanup([{init, Y}|T], Acc, St) ->
    cleanup(T, [{init, {y, Y}}|Acc], St);

cleanup([{testHeap, Need, Alive}| T], Acc, St) ->
    cleanup(T, [{test_heap, Need, Alive}| Acc], St);

cleanup([{call, Label, Arity, _, _}|T], Acc, St) ->
    label_is_used(Label),
    cleanup(T, [{call, Arity, {f, Label}}|Acc], St);

cleanup([{callOnly, Label, Arity, _, _}|T], Acc, St) ->
    label_is_used(Label),
    cleanup(T, [{call_only, Arity, {f, Label}}|Acc], St);

cleanup([{callLast, Label, Arity, Deallocate, _, _}|T], Acc, St) ->
    label_is_used(Label),
    cleanup(T, [{call_last, Arity, {f, Label}, Deallocate}|Acc], St);

cleanup([{callEx, Label, Module, Function, Arity, _}|T], Acc, St) ->
    cleanup(T, [{call_ext, Arity, {extfunc, Module, Function, Arity}}|Acc], St);

cleanup([{callExLast, Label, Module, Function, Arity, Deallocate, _}|T], Acc, St) ->
    cleanup(T, [{call_ext_last, Arity, {extfunc, Module, Function, Arity},
		 Deallocate}|Acc], St);

cleanup([{return}|T], Acc, St) ->
    case Acc of
	[Move, {label,Lbl}, DeadAfter|Rest] when element(1, Move) == move ->
	    case dead_after(DeadAfter) of
		true ->
		    move_code(Move, Lbl),
		    cleanup(T, [DeadAfter|Rest], St);
		false ->
		    cleanup(T, [return|Acc], St)
	    end;
	_ ->
	    cleanup(T, [return|Acc], St)
    end;

cleanup([removeMessage|T], Acc, St) ->
    cleanup(T, [remove_message|Acc], St);

cleanup([{test, Test, W, _}|T], Acc, St) ->
    cleanup(T, [cleanup_test(Test, W, St)|Acc], St);

cleanup([{bif, {Bif, [], Dest}, _, _}|T], Acc, St) ->
    Args = [Dest],
    cleanup(T, [{bif, Bif, nofail, [], Dest}|Acc], St);

cleanup([{bif, {Bif, Args, Dest}, W, _}|T], Acc, St) ->
    cleanup(T, [{bif, Bif, tag_label(W, St), Args, Dest}|Acc], St);

cleanup([send|T], Acc, St) ->
    cleanup(T, [send|Acc], St);

cleanup([{get, {getList2, R1, R2, R3}}| T], Acc, St) ->
    cleanup(T, [{get_list, R1, R2, R3}| Acc], St);

cleanup([{get, {getTupleElement, Src, Dest, Element}}| T], Acc, St) ->
    cleanup(T, [{get_tuple_element, Src, Element, Dest}| Acc], St);

cleanup([{put, {putTuple, Dest, Args}}| T], Acc0, St) ->
    Acc = make_puts(Args, [{put_tuple, {arity, length(Args)}, Dest}| Acc0]),
    cleanup(T, Acc, St);

cleanup([{put, Put}|T], Acc, St) ->
    cleanup(T, [cleanup_put(Put)| Acc], St);

cleanup([{'catch', {'catch', Y, Lb}}|T], Acc, St) ->
    label_is_used(Lb),
    cleanup(T, [{'catch', Y, {f, Lb}}|Acc], St);

cleanup([{error_func_clause, _}|T], Acc0, St) ->
    {Acc, Lbls} = cleanup_func_clause(Acc0, []),
    cleanup(T, Acc, St#state{func_info=Lbls});

cleanup([{arith, {arith, Op0, S1, S2, Dest}, W, _}|T], Acc, St) ->
    Op = case Op0 of
	     '+'    -> m_plus;
	     '-'    -> m_minus;
	     '*'    -> m_times;
	     '/'    -> m_div;
	     'div'  -> int_div;
	     'rem'  -> int_rem;
	     'band' -> int_band;
	     'bor'  -> int_bor;
	     'bxor' -> int_bxor;
	     'bsl'  -> int_bsl;
	     'bsr'  -> int_bsr
	 end,
    {X, Y} = maybe_reorder(Op0, S1, S2),
    cleanup(T, [{Op, tag_label(W, St), X, Y, Dest}|Acc], St);

cleanup([{arith, {arithBnot, S, Dest}, W, _}|T], Acc, St) ->
    cleanup(T, [{int_bnot, tag_label(W, St), S, Dest}|Acc], St);

cleanup(['if'|T], Acc, St) ->
    cleanup(T, Acc, St);

cleanup([H|T], Acc, St) ->
    case garbage(H) of
	true -> cleanup(T, Acc, St);
	false -> cleanup(T, [H|Acc], St)
    end;

cleanup([], [DeadAfter|Acc], St) ->
%%    io:format("~p\n", [DeadAfter]),
    true = dead_after(DeadAfter),		% Assertion.
    {v1_dead:remove_dead_code([DeadAfter|Acc], insert_moved_code()), St}.

cleanup_func_clause([{label, Lb}|T], Acc) ->
    cleanup_func_clause(T, [{label, Lb}|Acc]);
cleanup_func_clause(T, Acc) ->
    {T, Acc}.

cleanup_put({putList2, Dest, H, T}) ->
    {put_list, H, T, Dest};
cleanup_put({putString, Dest, _, Str}) ->
    {put_string, length(Str), {string, Str}, Dest}.

make_puts([H|T], Acc) ->
    make_puts(T, [{put, H}|Acc]);
make_puts([], Acc) ->
    Acc.

%% Cleanup test instructions.

cleanup_test({test, tuple, R}, W, St) ->
    {is_tuple, tag_label(W, St), R};
cleanup_test({test, binary, R}, W, St) ->
    {is_binary, tag_label(W, St), R};
cleanup_test({test, nil, R}, W, St) ->
    {is_nil, tag_label(W, St), R};
cleanup_test({test, constant, R}, W, St) ->
    {is_constant, tag_label(W, St), R};
cleanup_test({test, atom, R}, W, St) ->
    {is_atom, tag_label(W, St), R};
cleanup_test({test, number, R}, W, St) ->
    {is_number, tag_label(W, St), R};
cleanup_test({test, integer, R}, W, St) ->
    {is_integer, tag_label(W, St), R};
cleanup_test({test, float, R}, W, St) ->
    {is_float, tag_label(W, St), R};
cleanup_test({test, pid, R}, W, St) ->
    {is_pid, tag_label(W, St), R};
cleanup_test({test, reference, R}, W, St) ->
    {is_ref, tag_label(W, St), R};
cleanup_test({test, port, R}, W, St) ->
    {is_port, tag_label(W, St), R};
cleanup_test({test, list, R}, W, St) ->
    {is_list, tag_label(W, St), R};
cleanup_test({test, nonEmptyList, R}, W, St) ->
    {is_nonempty_list, tag_label(W, St), R};
cleanup_test({equal_atom, R1, Atom}, W, St) ->
    {is_eq_exact, tag_label(W, St), R1, Atom};
cleanup_test({equal_int, R1, Int}, W, St) ->
    {is_eq_exact, tag_label(W, St), R1, {i, Int}};
cleanup_test({equal, _, R1, R2}, W, St) ->
    {is_eq_exact, tag_label(W, St), R1, R2};
cleanup_test({nEqual, _, R1, R2}, W, St) ->
    {is_ne_exact, tag_label(W, St), R1, R2};
cleanup_test({testTuple, R1, R2}, W, St) ->
    {is_tuple_of_arity, tag_label(W, St), R1, R2};
cleanup_test({testTupleArity, R1, R2}, W, St) ->
    {test_arity, tag_label(W, St), R1, R2};
cleanup_test({test, function, Src}, W, St) ->
    {is_function, tag_label(W, St), Src};
cleanup_test({Test, R1, R2}, W, St) ->
    {Test, tag_label(W, St), R1, R2};
cleanup_test({intComp, Op, R1, R2}, W, St) ->
    cleanup_test({comp, Op, R1, R2}, W, St);
cleanup_test({comp, Op, R1, R2}, W, St) ->
    case {cleanup_comp(Op), tag_label(W, St), R1, R2} of
	{is_le, L, X, Y} ->
	    {is_ge, L, Y, X};
	{is_gt, L, X, Y} ->
	    {is_lt, L, Y, X};
	Other ->
	    Other
    end.

cleanup_comp('>')   -> is_gt;
cleanup_comp('<')   -> is_lt;
cleanup_comp('>=')  -> is_ge;
cleanup_comp('=<')  -> is_le;
cleanup_comp('==')  -> is_eq;
cleanup_comp('/=')  -> is_ne;
cleanup_comp('=:=') -> is_eq_exact;
cleanup_comp('=/=') -> is_ne_exact.

cleanup_fail(Const, {head, L}, _) -> {jump, {f, L}};
cleanup_fail(Const, {head_case, L}, _) -> {jump, {f, L}};
cleanup_fail(Const, body, St) -> {badmatch, Const};
cleanup_fail(Const, body_case, St) -> {badmatch, Const}.

tag_label({head, L}, _) ->
    label_is_used(L),
    {f, L};
tag_label({head_case, L}, _) ->
    label_is_used(L),
    {f, L};
tag_label(body, St) ->
    {f, 0};
tag_label(body_case, St) ->
    {f, 0}.

add_if_not_dead(New, [Old|T]) ->
    case dead_after(Old) of
	true ->
	    [Old|T];
	false ->
	    case [New, Old|T] of
		[{jump, {f, Lb1}}, {label, Lb2}|More] ->
		    move_label(Lb2, Lb1),
		    add_if_not_dead(New, More);
		[{jump, {_, Lbl}}|_] ->
		    label_is_used(Lbl),
		    [New, Old|T];
		[{case_end, Reg}|More] ->
		    move_case_end(Reg, More);
		[if_end|More] ->
		    move_if_end(More)
	    end
    end.

dead_after(return) -> true;
dead_after(if_end) -> true;
dead_after(function_clause_error) -> true;
dead_after({case_end, _}) -> true;
dead_after({badmatch, _}) -> true;
dead_after({call_ext_last, Ar, ExtFunc, D}) -> true;
dead_after({call_last, Ar, Lbl, D}) -> true;
dead_after({call_only, Ar, Lbl}) -> true;
dead_after({jump, Lbl}) -> true;
dead_after({call_ext, 1, {extfunc, erlang, exit, 1}}) -> true;
dead_after({call_ext, 1, {extfunc, erlang, throw, 1}}) -> true;
dead_after({select_val, R, Lbl, Cases}) -> true;
dead_after({wait, _}) -> true;
dead_after(_) -> false.

cleanup_allocation([{init, Y}| T], Alloc, Need, Inits, Acc, St) ->
    cleanup_allocation(T, Alloc, Need, [{init, {y, Y}}| Inits], Acc, St);
cleanup_allocation(T, Alloc, Need, [], Acc, St) ->
    cleanup(T, [Alloc| Acc], St);
cleanup_allocation(T, Alloc, Need, Inits, Acc, St) when Need > length(Inits)*2 ->
    cleanup(T, Inits ++ [Alloc| Acc], St);
cleanup_allocation(T, Alloc, Need, Inits, Acc, St) ->
    cleanup(T, [zero_alloc(Alloc)| Acc], St).

zero_alloc({allocate, Need, Regs}) ->
    {allocate_zero, Need, Regs};
zero_alloc({allocate_heap, Need, Regs, Heap}) ->
    {allocate_heap_zero, Need, Regs, Heap}.

%% Returns true if a {Garbage, ...} tuple is not useful for the assembler.

garbage(Tuple) when tuple(Tuple) ->
    garbage1(element(1, Tuple));
garbage(_) ->
    false.

garbage1(clause_header) -> true;
garbage1(commonCl) -> true;
garbage1(testPattern) -> true;
garbage1(commonTestPattern) -> true;
garbage1(waitTimeOutEnd) -> true;
garbage1(receiveEnd) -> true;
garbage1('case') -> true;
garbage1(_) -> false.

%% Generates a receive statement.  The skeleton looks like this:
%%
%%      L1:           <-------------------+
%%                    <-----------+       |
%%     	       	       	       	  |    	  |
%%              loop_rec L2 ------+---+   |
%%              ...               |   |   |
%%              ...	          |   |   |
%%		...	          |   |   |
%%		Loop_rec_end L1 --+   |   |
%%      L2:           <---------------+   |
%%	   	wait L1  -----------------+	%% or wait_timeout
%%		timeout
%%
%%

build_receive([{wait, Lb, W}|T], Lb, Y, Acc, St) ->
    {Lb3, St2} = next_label(St),
    label_is_used(Lb),
    label_is_used(Lb3),
    LoopRec = {loop_rec, {f, Lb3}, {y, Y}},
    LoopRecEnd = {loop_rec_end, {f, Lb}},
    Wait = {wait, {f, Lb}},
    End = lists:reverse(Acc, [LoopRecEnd, {label, Lb3}, Wait|T]),
    Code = [LoopRec | End],
    {Code, St2};
build_receive([{waitTimeOut, Lb, {i, 0}, W}|T], Lb, Y, [], St) ->
    %% Nothing more than 'receive after 0 -> expr... end' -- no effect.
    {T, St};
build_receive([{waitTimeOut, Lb, {atom, infinity}, W}|T], Lb, Y, [], St) ->
    %% Nothing more than 'receive after infinity -> expr... end' -- infinite wait.
    label_is_used(Lb),
    {[{wait, {f, Lb}}|T], St};
build_receive([{waitTimeOut, Lb, Time, W}|T], Lb, Y, [], St) ->
    %% Nothing more than 'receive after X -> expr... end'.
    label_is_used(Lb),
    {[{wait_timeout, {f, Lb}, Time}, timeout|T], St};
build_receive([{waitTimeOut, Lb, Time, W}|T], Lb, Y, Acc, St) ->
    {Lb3, St2} = next_label(St),
    label_is_used(Lb),
    label_is_used(Lb3),
    LoopRec = {loop_rec, {f, Lb3}, {y, Y}},
    LoopRecEnd = {loop_rec_end, {f, Lb}},

    %% Optimize some degenerate cases:
    %%
    %% 'after 0' => timeout
    %% 'after infinity' => wait Lbl
    %% 'after Time' => wait_timeout Lbl Time, timeout

    WaitCode = case Time of
		   {i, 0} -> [timeout|T];
		   {atom, infinity} -> [{wait, {f, Lb}}|T];
		   _ -> [{wait_timeout, {f, Lb}, Time}, timeout|T]
	       end,
    
    End = lists:reverse(Acc, [LoopRecEnd, {label, Lb3}|WaitCode]),
    Code = [LoopRec | End],
    {Code, St2};
build_receive([H|T], Lb, Y, Acc, St) ->
    build_receive(T, Lb, Y, [H|Acc], St).

next_label(St0) ->
    Label = St0#state.label,
    {Label, St0#state{label = Label+1}}.

maybe_reorder('+', {i, N}, X) -> {X, {i, N}};
maybe_reorder('*', {i, N}, X) -> {X, {i, N}};
maybe_reorder('band', {i, N}, X) -> {X, {i, N}};
maybe_reorder('bor', {i, N}, X) -> {X, {i, N}};
maybe_reorder('bxor', {i, N}, X) -> {X, {i, N}};
maybe_reorder(_, X, Y) -> {X, Y}.

%%%
%%% Label utilities.
%%%

%%
%% Save a note that there are references to this label.
%%

label_is_used(Lbl) when integer(Lbl) ->
    case ets:lookup(beam_labels, Lbl) of
	[] -> ets:insert(beam_labels, {Lbl, []});
	_ -> ok
    end;
label_is_used({f, 0}) ->
    ok;
label_is_used({f, Lbl}) ->
    label_is_used(Lbl).

%%
%% Save a note that the From label should be defined at the same place at To.
%% When To label later is defined, From will be defined to.
%% This is implies that the To label is used (no need to call label_is_used(To).
%%

move_label(From, To) ->
    OldList =
	case ets:lookup(beam_labels, To) of
	    [{To, Labels}] -> Labels;
	    [] -> []
	end,
    ets:insert(beam_labels, {To, [From|OldList]}).

%%
%% Possibly add label instructions to the instruction stream.
%%
%% If the given label has not been referenced, add no instruction.
%%
%% Add the label instruction if the label has been referenced.
%% Also add all labels that have been moved to the same place as
%% this one.
%%
%% Also remove a jump to the given label immediately before the label.
%%

add_label(0, Acc) ->
    Acc;
add_label(Lbl, [{jump, {f, Lbl}}| Acc]) ->
    add_label(Lbl, Acc);
add_label(Lbl, Acc) ->
    case ets:lookup(beam_labels, Lbl) of
	[] ->
	    Acc;
	[{Lbl, List}] ->
	    add_moved_labels(List, [{label, Lbl}| Acc])
    end.

add_moved_labels([Lbl|T], Acc) ->
    add_moved_labels(T, [{label, Lbl}|Acc]);
add_moved_labels([], Acc) ->
    Acc.

%%
%% Support for moving case_end and if_end.
%%

move_if_end(List) ->
    move_case_end(if_end, List).

move_case_end(Reg, [{label, Lbl}|More]) ->
    OldLabels =
	case ets:lookup(beam_case, Reg) of
	    [{Reg, Labels}] -> Labels;
	    [] -> []
	end,
    move_case_end(Reg, More, [Lbl|OldLabels]);
move_case_end(if_end, Instr) ->
    [if_end|Instr];
move_case_end(Reg, Instr) ->
    [{case_end, Reg}|Instr].

move_case_end(Reg, [{label, Lbl}|T], Acc) ->
    move_case_end(Reg, T, [Lbl|Acc]);
move_case_end(Reg, [Instr|T], Acc) ->
    true = dead_after(Instr),			% Assertion.
    true = ets:insert(beam_case, {Reg, Acc}),
    [Instr|T].

insert_moved_code() ->
    Acc = insert_case_end(ets:tab2list(beam_case), []),
    insert_moved_code(ets:tab2list(beam_moved), Acc).

insert_case_end([{if_end, Labels}|T], Acc) ->
    true = ets:delete(beam_case, if_end),
    insert_case_end(T, add_moved_labels(Labels, [if_end|Acc]));
insert_case_end([{Reg, Labels}|T], Acc) ->
    true = ets:delete(beam_case, Reg),
    insert_case_end(T, add_moved_labels(Labels, [{case_end, Reg}|Acc]));
insert_case_end([], Acc) ->
    Acc.

%%
%% Move code of the type: [{label,L}, {move,S,D}, return]
%%

move_code(Move, Lbl) ->
    OldLabels =
	case ets:lookup(beam_moved, Move) of
	    [{Move, Labels}] -> Labels;
	    [] -> []
	end,
    true = ets:insert(beam_moved, {Move, [Lbl|OldLabels]}).
    
insert_moved_code([{Move, Labels}|T], Acc) ->
    true = ets:delete(beam_moved, Move),
    insert_moved_code(T, add_moved_labels(Labels, [Move, return|Acc]));
insert_moved_code([], Acc) ->
    Acc.
