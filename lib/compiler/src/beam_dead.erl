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
%%%     L2:	test is_atom FailLabel {x,0}
%%%    		select_val {x,0}, FailLabel [... AtomLiteral => L3...]
%%%        .
%%%        .
%%%        .
%%%	L3:	...
%%%
%%%     FailLabel: ...
%%%
%%%	the first code fragment can be changed to
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump L3
%%%
%%%     If the literal is not included in the table of literals in the
%%%     select_val instruction, the first code fragment will instead be
%%%     rewritten as:
%%%
%%%     L1:	move AtomLiteral {x,0}
%%%     	jump FailLabel
%%%
%%%	The move instruction will be removed by optimization (1) above,
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
%%%     Same thing for
%%%
%%%     	test is_nil ALabel Dst
%%%     	move Src Dst
%%%
%%%
%%% (4) In this code
%%%
%%%    		select_val {x,Reg}, ALabel [... Literal => L1...]
%%%        .
%%%        .
%%%        .
%%%	L1:	move Literal {x,Reg}
%%%
%%%     we can remove the move instruction.
%%%


-import(lists, [map/2,mapfoldl/3,reverse/1,reverse/2,keysearch/3,foreach/2]).

module({Mod,Exp,Attr,Fs0,Lc0}, Opts) ->
    Fs1 = map(fun split_blocks/1, Fs0),
    ModCode = {Mod,Exp,Attr,Fs1,Lc0},
    {ok,{Mod,Exp,Attr,Fs2,Lc}} = beam_clean:module(ModCode, Opts),
    Fs = map(fun function/1, Fs2),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    Is1 = beam_jump:remove_unused_labels(Is0),

    %% Optimize away dead code.
    Is2 = forward(Is1),
    Is = backward(Is2),
    {function,Name,Arity,CLabel,Is}.

%% We must split the basic block when we encounter instructions with labels,
%% such as catches and BIFs. All labels must be visible outside the blocks.
%% Also remove empty blocks.

split_blocks({function,Name,Arity,CLabel,Is0}) ->
    Is = split_blocks(Is0, []),
    {function,Name,Arity,CLabel,Is}.

split_blocks([{block,[{'%live',_}=Live]}|Is], Acc) ->
    split_blocks(Is, [Live|Acc]);
split_blocks([{block,[]}|Is], Acc) ->
    split_blocks(Is, Acc);
split_blocks([{block,Bl}|Is], Acc0) ->
    Acc = split_block(Bl, [], Acc0),
    split_blocks(Is, Acc);
split_blocks([I|Is], Acc) ->
    split_blocks(Is, [I|Acc]);
split_blocks([], Acc) -> reverse(Acc).

split_block([{set,[R],[_,_,_]=As,{bif,internal_is_record,{f,Lbl}}}|Is], Bl, Acc) ->
    %% internal_is_record/3 must be translated by beam_clean; therefore,
    %% it must be outside of any block.
    split_block(Is, [], [{bif,internal_is_record,{f,Lbl},As,R}|make_block(Bl, Acc)]);
split_block([{set,[R],As,{bif,N,{f,Lbl}}}|Is], Bl, Acc) when Lbl =/= 0 ->
    split_block(Is, [], [{bif,N,{f,Lbl},As,R}|make_block(Bl, Acc)]);
split_block([{set,[R],[],{'catch',L}}|Is], Bl, Acc) ->
    split_block(Is, [], [{'catch',R,L}|make_block(Bl, Acc)]);
split_block([I|Is], Bl, Acc) ->
    split_block(Is, [I|Bl], Acc);
split_block([], Bl, Acc) -> make_block(Bl, Acc).

make_block([], Acc) -> Acc;
make_block(Bl, Acc) -> [{block,reverse(Bl)}|Acc].


%%%
%%% Scan instructions in execution order and remove dead code.
%%%

forward(Is) ->
    forward(Is, gb_trees:empty(), []).

forward([{select_val,Reg,_,{list,List}}=I|Is], D0, Acc) ->
    D = update_value_dict(List, Reg, D0),
    forward(Is, D, [I|Acc]);
forward([{label,Lbl}=LblI,{block,[{set,[Dst],[Lit],move}|BlkIs]}=Blk|Is], D, Acc) ->
    Block = case gb_trees:lookup({Lbl,Dst}, D) of
		{value,Lit} ->
		    %% The move instruction seems to be redundant, but also make
		    %% sure that the instruction preceeding the label
		    %% cannot fall through to the move instruction.
		    case is_unreachable_after(Acc) of
			false -> Blk;	  %Must keep move instruction.
			true ->  {block,BlkIs}	%Safe to remove move instruction.
		    end;
		_ -> Blk		       %Keep move instruction.
	    end,
    forward([Block|Is], D, [LblI|Acc]);
forward([I|Is], D, Acc) ->
    forward(Is, D, [I|Acc]);
forward([], _, Acc) -> Acc.

update_value_dict([Lit,{f,Lbl}|T], Reg, D0) ->
    Key = {Lbl,Reg},
    D = case gb_trees:lookup(Key, D0) of
	    none -> gb_trees:insert(Key, Lit, D0); %New.
	    {value,Lit} -> D0;			%Already correct.
	    {value,inconsistent} -> D0;		%Inconsistent.
	    {value,_} -> gb_trees:update(Key, inconsistent, D0)
	end,
    update_value_dict(T, Reg, D);
update_value_dict([], _, D) -> D.

is_unreachable_after([I|_]) ->
    beam_jump:is_unreachable_after(I).

%%%
%%% Scan instructions in reverse execution order and remove dead code.
%%%

backward(Is) ->
    backward(Is, gb_trees:empty(), []).

backward([{select_val,Reg,{f,Flbl}=Fail,{list,List0}},
	  {test,is_atom,_,_}=Test|Is], D, Acc) ->
    List = rewrite_select_list(List0, Reg, D, []),
    Alias = {select_val,Reg,Flbl,collect_alias_list(List)},
    Sel = {select_val,Reg,Fail,{list,List}},
    backward_lbl_alias(Is, Alias, D, [Test,Sel|Acc]);
backward([{jump,{f,To}}=Jump|[{label,_}|_]=Is], D, Acc) ->
    Alias = case gb_trees:lookup(To, D) of
		none -> {label_alias,To};
		{value,Val} -> Val
	    end,
    backward_lbl_alias(Is, Alias, D, [Jump|Acc]);
backward([{jump,{f,To0}},
	  {block,[{set,[Reg],[{atom,Val}],move},{'%live',_}]}=Block|Is], D, Acc) ->
    To = find_substitution(Reg, Val, D, To0),
    Jump = {jump,{f,To}},
    case is_reg_killed(Reg, To, D) of
	false -> backward([Block|Is], D, [Jump|Acc]);
	true -> backward([Jump|Is], D, Acc)
    end;
backward([{block,[{'%live',_}]}|Is], D, Acc) ->
    %% A redudant block could prevent some jump optimizations in beam_jump.
    %% Ge rid of it.
    backward(Is, D, Acc);
backward([{'%live',_}|Is], D, Acc) ->
    backward(Is, D, Acc);
backward([{block,Bl}=Block|[{label,L}|_]=Is], D, Acc0) ->
    Acc = [Block|Acc0],
    case beam_block:is_killed({x,0}, Bl) of
	false -> backward(Is, D, Acc);
	true -> backward(Is, gb_trees:insert(L, x0_killed, D), Acc)
    end;
backward([{block,[{set,[Dst],[Src],move}|Bl]}|
	  [{test,is_eq_exact,_,[Src,Dst]}|_]=Is], D, Acc) ->
    backward([{block,Bl}|Is], D, Acc);
backward([{block,[{set,[Dst],[Src],move}|Bl]}|
	  [{test,is_eq_exact,_,[Dst,Src]}|_]=Is], D, Acc) ->
    backward([{block,Bl}|Is], D, Acc);
backward([{block,[{set,[Dst],[nil],move}|Bl]}|
	  [{test,is_nil,_,[Dst]}|_]=Is], D, Acc) ->
    backward([{block,Bl}|Is], D, Acc);
backward([I|Is], D, Acc) ->
    backward(Is, D, [I|Acc]);
backward([], _D, Acc) ->
    %%io:format("~p\n", [gb_trees:to_list(_D)]),
    Acc.

backward_lbl_alias([{label,L}=Lbl|Is], Alias, D, Acc) ->
    backward_lbl_alias(Is, Alias, gb_trees:insert(L, Alias, D), [Lbl|Acc]);
backward_lbl_alias(Is, _, D, Acc) -> backward(Is, D, Acc).

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
	{value,{select_val,Reg,Fail,List}} ->
	    case keysearch(Val, 1, List) of
		false -> Fail;
		{value,{Val,To}} -> To
	    end;
	{value,{label_alias,To}} -> To;
	{value,_} -> To0;
	none -> To0
    end.
