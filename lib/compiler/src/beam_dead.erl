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
%% Portions created by Ericsson are Copyright 2002, Ericsson AB.
%% All Rights Reserved.''
%% 
%%     $Id$
%%

-module(beam_dead).

-export([module/2]).

%%% The following optimisations are done:
%%%
%%% (1) In this code
%%%
%%%     	move DeadValue {x,0}
%%%     	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	move Anything {x,0}
%%%        .
%%%        .
%%%        .
%%%
%%%     the first assignment to {x,0} has no effect (is dead),
%%%     so it can be removed. Besides removing a move instruction,
%%%     if the move was preceeded by a label, the resulting code
%%%	will look this
%%%
%%%     L1:	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	move Anything {x,0}
%%%        .
%%%        .
%%%        .
%%%
%%%	which can be further optimized by the jump optimizer (beam_jump).
%%%
%%% (2) In this code
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump L2
%%%        .
%%%        .
%%%        .
%%%     L2:	test is_atom ALabel {x,0}
%%%    		select_val {x,0}, ALabel [... AtomLiteral => L3...]
%%%        .
%%%        .
%%%        .
%%%	L3:	...
%%%
%%%	the first code fragment can be changed to
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump L3
%%%
%%%	The move instruction will be removed by optmization (1) above,
%%%	if the code following the L3 label overwrites {x,0}.
%%%
%%% 	The code following the L2 label will be kept, but it will be removed later
%%%	by the jump optimizer.
%%%
%%% (3) In this code
%%%
%%%     	test is_eq_exact ALabel Src Dst
%%%     	move Src Dst
%%%
%%%	the move instruction can be removed.

-import(lists, [map/2,reverse/1,keysearch/3]).

module({Mod,Exp,Attr,Fs,Lc}, _Opt) ->
    {ok,{Mod,Exp,Attr,map(fun function/1, Fs),Lc}}.

function({function,Name,Arity,CLabel,Asm0}) ->
    Asm = dead_opt(Asm0),
    {function,Name,Arity,CLabel,Asm}.

dead_opt(Is) ->
    %% Scan instructions from the end.
    dead_opt(reverse(Is), gb_trees:empty(), []).

dead_opt([{select_val,Reg,{f,Flbl}=Fail,{list,List0}},
	  {test,is_atom,_,_}=Test|Is], D, Acc) ->
    List = rewrite_select_list(List0, Reg, D, []),
    Alias = {select_val,Reg,Flbl,collect_alias_list(List)},
    Sel = {select_val,Reg,Fail,{list,List}},
    dead_opt_lbl_alias(Is, Alias, D, [Test,Sel|Acc]);
dead_opt([{jump,{f,To}}=Jump|[{label,_}|_]=Is], D, Acc) ->
    Alias = case gb_trees:lookup(To, D) of
		none -> {label_alias,To};
		{value,Val} -> Val
	    end,
    dead_opt_lbl_alias(Is, Alias, D, [Jump|Acc]);
dead_opt([{jump,{f,To0}},
	  {block,[{set,[Reg],[{atom,Val}],move},{'%live',_}]}=Block|Is], D, Acc) ->
    To = find_substitution(Reg, Val, D, To0),
    Jump = {jump,{f,To}},
    case is_reg_killed(Reg, To, D) of
	false -> dead_opt([Block|Is], D, [Jump|Acc]);
	true -> dead_opt([Jump|Is], D, Acc)
    end;
dead_opt([{block,Bl}=Block|[{label,L}|_]=Is], D, Acc0) ->
    Acc = [Block|Acc0],
    case beam_block:is_killed({x,0}, Bl) of
	false -> dead_opt(Is, D, Acc);
	true -> dead_opt(Is, gb_trees:insert(L, x0_killed, D), Acc)
    end;
dead_opt([{block,[{set,[Dst],[Src],move}|Bl]}|
	  [{test,is_eq_exact,_,[Src,Dst]}|_]=Is], D, Acc) ->
    dead_opt([{block,Bl}|Is], D, Acc);
dead_opt([{block,[{set,[Dst],[Src],move}|Bl]}|
	  [{test,is_eq_exact,_,[Dst,Src]}|_]=Is], D, Acc) ->
    dead_opt([{block,Bl}|Is], D, Acc);
dead_opt([{block,[{'%live',_}]}|Is], D, Acc) ->
    %% A redudant block could prevent some jump optimizations in beam_jump.
    %% Ge rid of it.
    dead_opt(Is, D, Acc);
dead_opt([I|Is], D, Acc) ->
    dead_opt(Is, D, [I|Acc]);
dead_opt([], _D, Acc) ->
    %%io:format("~p\n", [gb_trees:to_list(_D)]),
    Acc.

dead_opt_lbl_alias([{label,L}=Lbl|Is], Alias, D, Acc) ->
    dead_opt_lbl_alias(Is, Alias, gb_trees:insert(L, Alias, D), [Lbl|Acc]);
dead_opt_lbl_alias(Is, _, D, Acc) -> dead_opt(Is, D, Acc).

rewrite_select_list([{atom,Val}=Atom,{f,To0}|T], Reg, D, Acc) ->
    To = find_substitution(Reg, Val, D, To0),
    rewrite_select_list(T, Reg, D, [{f,To},Atom|Acc]);
rewrite_select_list([], _, _, Acc) -> reverse(Acc).
    
collect_alias_list([{atom,Atom},{f,Lbl}|T]) ->
    [{Atom,Lbl}|collect_alias_list(T)];
collect_alias_list([]) -> [].

is_reg_killed({x,0}=R, Lbl, D) ->
    case gb_trees:lookup(Lbl, D) of
	{value,x0_killed} -> true;
	{value,{select_val,_,Fail,List}} ->
	    all_path_kills(R, [{dummy,Fail}|List], D);
	{value,_} -> false;
	none -> false
    end;
is_reg_killed(_, _, _) -> false.

all_path_kills(R, [{_,Lbl}|T], D) ->
    is_reg_killed(R, Lbl, D) andalso all_path_kills(R, T, D);
all_path_kills(_, [], _) -> true.

find_substitution(Reg, Val, D, To0) ->
    case gb_trees:lookup(To0, D) of
	{value,{select_val,Reg,_Fail,List}} ->
	    case keysearch(Val, 1, List) of
		false -> To0;
		{value,{Val,To}} -> To
	    end;
	{value,{label_alias,To}} -> To;
	{value,_} -> To0;
	none -> To0
    end.
