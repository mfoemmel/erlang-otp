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
%%% Purpose : Removes any dead instructions.

-module(v1_dead).

-export([remove_dead_code/2]).

%%
%% This function removes instructions without side effects whose result
%% is not used.
%%
%% The input code in Code must be in reverse execution order.  The result will be
%% in execution order.
%%

remove_dead_code(Code, Acc) ->
    ets:new(beam_dead, [set, private, named_table]),
    NewCode = remove_dead(Code, Acc),
    true = ets:delete(beam_dead),
    NewCode.

%%
%% In this function, we look at instructions where it is clear which
%% registers are live and which are dead.  As long as no such instructions
%% are seen, we'll just reverse the code stream.
%%

remove_dead([return|T], Acc) ->
    remove_dead(T, [return|Acc], new_set(1, 0));
remove_dead([send|T], Acc) ->
    remove_dead(T, [send|Acc], new_set(2, all));
remove_dead([{call,Arity,Func}|T], Acc) ->
    remove_dead(T, [{call,Arity,Func}|Acc], new_set(Arity, all));
remove_dead([{call_last,Arity,Func,D}|T], Acc) ->
    remove_dead(T, [{call_last,Arity,Func,D}|Acc], new_set(Arity, D));
remove_dead([{call_only,Arity,Func}|T], Acc) ->
    remove_dead(T, [{call_only,Arity,Func}|Acc], new_set(Arity, 0));
remove_dead([{call_ext,Arity,Func}|T], Acc) ->
    remove_dead(T, [{call_ext,Arity,Func}|Acc], new_set(Arity, all));
remove_dead([{call_ext_last,Arity,Func,D}|T], Acc) ->
    remove_dead(T, [{call_ext_last,Arity,Func,D}|Acc], new_set(Arity, D));
remove_dead([{call_fun,Arity}|T], Acc) ->
    remove_dead(T, [{call_fun,Arity}|Acc], new_set(Arity+1, all));
remove_dead([{make_fun,Lbl,Uniq,NumFree}|T], Acc) ->
    remove_dead(T, [{make_fun,Lbl,Uniq,NumFree}|Acc], new_set(NumFree, all));
remove_dead([{jump, {f,Lbl}}|T], Acc) ->
    Instr = {jump, {f,Lbl}},
    case ets:lookup(beam_dead, Lbl) of
	[{Lbl, Live}] ->
	    remove_dead(T, [Instr|Acc], Live);
	[] ->
	    remove_dead(T, [Instr|Acc])
    end;
remove_dead([H|T], Acc) ->
    remove_dead(T, [H|Acc]);
remove_dead([], Acc) ->
    Acc.

%%
%% In this function, we know the liveness of each register.  For each instruction,
%% we'll update the liveness information.  We'll remove an instruction if it has
%% no side effects and its result is not used.  We'll throw away the liveness
%% information and go back to remove_dead/2 if we encounter an unknown
%% instruction.
%%

remove_dead([H|T], Acc, Live) ->
    remove_dead(H, H, T, Acc, Live).

%% The following instructions are okay to remove if their result is not used,
%% since they have no side effects.

remove_dead({bif,Name,nofail,[],Dst}, Instr, Rest, Acc, Live) ->
    update_live_info([], [Dst], Instr, Rest, Acc, Live);
remove_dead({move, Src, Dst}, Instr, Rest, Acc, Live) ->
    update_live_info(Src, [Dst], Instr, Rest, Acc, Live);
remove_dead({put_list, S1, S2, Dst}, Instr, Rest, Acc, Live) ->
    update_live_info([S1, S2], [Dst], Instr, Rest, Acc, Live);
remove_dead({get_tuple_element, Src, Pos, Dst}, Instr, Rest, Acc, Live) ->
    update_live_info(Src, [Dst], Instr, Rest, Acc, Live);

remove_dead({jump, {f, Lbl}}, Instr, Rest, Acc, Live) ->
    case dead_jump(Lbl, Acc) of
	true ->
	    remove_dead(Rest, [{'%', dead, Instr}|Acc], Live);
	false ->
	    remove_dead([Instr|Rest], Acc)
    end;

%% Neutral instructions (no change to live set).

remove_dead({test_heap, Need, 0}, Instr, Rest, Acc, Live) ->
    remove_dead(Rest, [Instr|Acc], Live);
remove_dead({test_heap, Need, Regs}, Instr, Rest, Acc, Live) ->
    case is_live({x,Regs-1}, Live) of
	true ->
	    remove_dead(Rest, [Instr|Acc], Live);
	false ->
	    NewInstr = {test_heap, Need, Regs-1},
	    remove_dead(NewInstr, NewInstr, Rest, [{'%', test_heap_modified}|Acc], Live)
    end;
remove_dead({deallocate, D}, Instr, Rest, Acc, Live) ->
    remove_dead(Rest, [Instr|Acc], Live);
remove_dead({label, Lbl}, Instr, Rest, Acc, Live) ->
    ets:insert(beam_dead, {Lbl, Live}),
    remove_dead(Rest, [Instr|Acc], Live);

%% The following instruction must not be removed (because they may
%% generate an exception if given invalid arguments), but we must update
%% the live set.

remove_dead({bif, Name, {f,_}, Sources, Dst}, Instr, Rest, Acc, Live) ->
    NewLive = set_live(Sources, kill(Dst, Live)),
    remove_dead(Rest, [Instr|Acc], NewLive);
remove_dead({put_tuple, Arity, Dst}, Instr, Rest, Acc, Live) ->
    remove_dead(Rest, [Instr|Acc], kill(Dst, Live));
remove_dead({put, Src}, Instr, Rest, Acc, Live) ->
    remove_dead(Rest, [Instr|Acc], set_live(Src, Live));
remove_dead({Arith, Fail, S1, S2, Dst}, Instr, Rest, Acc, Live) ->
    case is_arith(Arith) of
	true -> remove_dead(Rest, [Instr|Acc], set_live([S1, S2], Live));
	false -> remove_dead([Instr|Rest], Acc)
    end;

remove_dead(Instr, Instr, Rest, Acc, Live) ->
    remove_dead([Instr|Rest], Acc).

%%
%% Update the liveness information, and remove the instructions if it turns
%% out that the result is not used.
%%

update_live_info(Src, [Dst], Instr, Rest, Acc, Live) ->
    case is_live(Dst, Live) of
	true ->
	    NewLive = set_live(Src, kill(Dst, Live)),
	    remove_dead(Rest, [Instr|Acc], NewLive);
	false ->
	    remove_dead(Rest, [{'%', dead, Instr}|Acc], Live)
    end.

%%
%% Returns true if a jump instruction can be eliminated, false otherwise.
%%

dead_jump(Lbl, [{label, Lbl}|_]) ->
    true;
dead_jump(Lbl, [{label, _}|T]) ->
    dead_jump(Lbl, T);
dead_jump(Lbl, [Comment|T]) when element(1, Comment) == '%' ->
    dead_jump(Lbl, T);
dead_jump(_, _) ->
    false.

%% Arithmetic operation?

is_arith(m_plus) -> true;
is_arith(m_minus) -> true;
is_arith(m_times) -> true;
is_arith(m_div) -> true;
is_arith(int_div) -> true;
is_arith(int_rem) -> true;
is_arith(int_band) -> true;
is_arith(int_bor) -> true;
is_arith(int_bxor) -> true;
is_arith(int_bsl) -> true;
is_arith(int_bsr) -> true;
is_arith(_) -> false.


%%%
%%% Routines to keep track of the set of live registers.
%%%

%% Creates a new set.  X = # living x register, Y = # living y registers.

new_set(all, Y) ->
    new_set(1024, Y);
new_set(X, all) ->
    new_set(X, 1024);
new_set(X, Y) ->
    {1 bsl X - 1, 1 bsl Y - 1}.


%%
%% set_live(Reg, Set) -> Set'
%%
%% Mark that the given register as live.
%%

set_live({x, N}, {X, Y}) ->
    {X bor (1 bsl N), Y};
set_live({y, N}, {X, Y}) ->
    {X, Y bor (1 bsl N)};
set_live([H|T], Set) ->
    set_live(T, set_live(H, Set));
set_live(_, Set) ->
    Set.

%% is_live(Reg, Set) -> true|false

is_live({x,N}, {X, _}) when (1 bsl N) band X =/= 0 ->
    true;
is_live({y,N}, {_, Y}) when (1 bsl N) band Y =/= 0 ->
    true;
is_live(_, _) ->
    false.

%%
%% set_live(Reg, Set) -> Set'
%%
%% Mark that the given register as dead.
%%

kill({x,N}, {X, Y}) ->
    {X band bnot(1 bsl N), Y};
kill({y,N}, {X, Y}) ->
    {X, Y band bnot(1 bsl N)};
kill([H|T], Set) ->
    kill(T, kill(H, Set));
kill([], Set) ->
    Set.
