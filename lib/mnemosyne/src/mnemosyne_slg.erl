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
-module(mnemosyne_slg).
-export([slg_init/0,
	 slg/2]).
	 
%%-define(debug,2).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").


%%%----------------------------------------------------------------
%%% 		Exports

slg_init() ->
    create_table_stack().

slg(A, Asker) ->
    Akey = key(A),
    case in_table(Akey) of
	false ->
	    PosMin = PosLink = DFN = ref_step_counter(),
	    NegMin = NegLink = maxint,
	    new_table_entry(Akey, [], []),
	    push({Akey,A,DFN,PosLink,NegLink}),
	    set_ans_pid(Akey, Asker),
	    slg_subgoal(A,Akey, {PosMin,NegMin}),
	    un_X(anss(Akey));
	true ->
	    case comp(Akey) of
		false -> 
		    exit({'Cannot be true!!!!',Akey,A}); %% unless // accesses
		true -> 
		    un_X(anss(Akey))
	    end
    end.

%%%----------------------------------------------------------------
%%% 		Private

send_ans({Head,[],[]}, {Caller,Bindings,Stack}) ->
    Caller ! {reply,Head,Bindings,Stack}.


un_X(L) -> un_X(L,[]).

un_X([{H,[],[]}|Xs], Acc) -> un_X(Xs, [H|Acc]);
un_X([], L) -> L.

%%%----------------------------------------------------------------
slg_subgoal(A,Akey,PosNegMin) -> 
    slg_complete(Akey, slg_newclauses(Akey, slg_resolvents_P_A(A), PosNegMin)).


%%%----------------------------------------------------------------
slg_newclauses(Akey, [G|Gs], PosNegMin) -> 
    slg_newclauses(Akey, Gs, slg_newclause(Akey,G,PosNegMin));
slg_newclauses(Akey, [], PosNegMin) -> 
    PosNegMin.
    
call_slg_newclause([{Akeyp,Gp}|L], PosNegMin) ->
    call_slg_newclause(L, slg_newclause(Akeyp,Gp,PosNegMin));
call_slg_newclause([], PosNegMin) ->
    PosNegMin.

slg_newclause(Akey, G, PosNegMin) ->
    case body(G) of
	[] ->		%% G has no body literal to the right of |
	    slg_answer(Akey, G, PosNegMin);
	Body ->
	    case select_literal(G) of
		{'#not',C,B} ->  %% G has a selected literal ~B
		    case mnemosyne_unify:variables(B) of
			[] ->  %% ground negative literal
			    slg_negative(Akey, G, B, PosNegMin);
			_ ->   %% non-ground negative literal
			    exit({'selected non-ground negative literal',
				  {'#not',C,B},Akey,G,PosNegMin})
		    end;
		B ->  %% G have a selected positive atom B
		    slg_positive(Akey, G, B, PosNegMin)
	    end
    end.

%%%----------------------------------------------------------------
slg_answer(Akey, G, PosNegMin) -> 
    case subsumed_by_any_anss(Akey,G) of
%% 	true ->
%% 	    PosNegMin;
	false -> 
	    add_to_anss(Akey, G),
	    case delayed_literals(G) of
		[] ->
		    reset_negs(Akey),
		    slg_answer_cont(poss(Akey), G, no_delayed, PosNegMin);
		_  ->
		    case 'no other has the same head'(anss(Akey),G) of
			true -> 
			    slg_answer_cont(poss(Akey), G, delayed, PosNegMin);
			false ->
			    PosNegMin
		    end
	    end
    end.
		    

slg_answer_cont([{B,H,Li}|BHs], G, ND, PosNegMin) ->
    NewPosNegMin = 
	slg_newclause(key(B), 
		      case ND of
			  no_delayed -> slg_resolvent(H,Li,G);
			  delayed -> slg_factor(H,Li,G)
		      end,
		      PosNegMin),
    slg_answer_cont(BHs, G, ND, NewPosNegMin);
slg_answer_cont([], _, _, PosNegMin) ->
    PosNegMin.


%%%----------------------------------------------------------------
slg_positive(Akey, G, B, PosNegMin) -> %% B is the selected literal in G
    Bkey = key(B),
    case in_table(Bkey) of
	false ->
	    new_table_entry(Bkey, [{Akey,G,B}], []),
	    PosLink = DFN = ref_step_counter(),
	    NegLink = maxint,
	    push({Bkey,B,DFN,PosLink,NegLink}),
	    BPosNegMin = {DFN,maxint},
	    update_solution(Akey, Bkey, pos, PosNegMin,
			    slg_subgoal(B, Bkey, BPosNegMin));
	true ->
	    NewPosNegMin = case comp(Bkey) of
			       false -> 
				   add_to_poss(Bkey,{Akey,G,B}),
				   update_lookup(Akey, Bkey, pos, PosNegMin);
			       true ->
				   PosNegMin
			   end,
	    L = pos_res_factor(anss(Bkey), Bkey, B, Akey, G, []),
	    call_slg_newclause(L, NewPosNegMin)
    end.
	    

pos_res_factor([B_Ans|B_Anss], Bkey, B, Akey, G, L) ->
    Bprim = head(B_Ans),
    Gprim = 
	case ets:match(get(table_ref), {{anss,Bkey},{Bprim,[],[]}}) of
	    [[]|_] ->  %% found
		slg_resolvent(G, B, {Bprim,[],[]});
	    [] -> %% not_found
		case  ets:match(get(table_ref),
				{{anss,Bkey},{Bprim,'$1','$2'}}) of
		    [[Dh,Bh]|_] ->
			slg_factor(G, B, {Bprim,Dh,Bh});
		    [] -> %% not_found
			exit({strange, pos_resolvent_list})
		end
	end,
    pos_res_factor(B_Anss, Bkey, B, Akey, G, [{Akey,Gprim}|L]);
pos_res_factor([], _, _, _, _, L) ->
    L.


update_lookup(Akey, Bkey, pos, {PosMin,NegMin}) ->
    PosLink_B = poslink(Bkey),
    NegLink_B = neglink(Bkey),
    set_poslink(Akey, min(poslink(Akey),PosLink_B)),
    set_neglink(Akey, min(neglink(Akey),NegLink_B)),
    {min(PosMin,PosLink_B), min(NegMin,NegLink_B)}.
%% update_lookup(Akey, Bkey, neg, {PosMin,NegMin}) ->
%%     PosLink_B = poslink(Bkey),
%%     NegLink_B = neglink(Bkey),
%%     set_neglink(Akey, min(neglink(Akey),PosLink_B)),
%%     {PosMin, min(NegMin,PosLink_B,NegLink_B)}.


%% Returns  {New_PosMin, New_NegMin}.

update_solution(Akey, Bkey, Sign, PosNegMin, BPosNegMin) ->
    {PosMin,NegMin} = PosNegMin,
    {BPosMin,BNegMin} = PosNegMin,
    case {comp(Bkey),Sign} of
	{true,_} ->  set_neglink(Akey, min(neglink(Akey),BNegMin)),
		     set_poslink(Akey, min(poslink(Akey),BPosMin)),
		     {min(PosMin,BPosMin), min(NegMin,BNegMin)};

	{_,pos} ->   PosLinkB = poslink(Bkey),
		     NegLinkB = neglink(Bkey),
		     set_neglink(Akey, min(neglink(Akey),NegLinkB)),
		     set_poslink(Akey, min(poslink(Akey),PosLinkB)),
		     {min(PosMin,PosLinkB), min(NegMin,NegLinkB)}
	
%% 	{_,neg} ->   PosLinkB   = poslink(Bkey),
%% 		     NegLinkB   = neglink(Bkey),
%% 		     set_neglink(Akey, min(neglink(Akey),PosLinkB,NegLinkB)),
%% 		     {PosMin,  min(NegMin,PosLinkB,NegLinkB)}
    end.

%%%----------------------------------------------------------------
slg_negative(Akey, G, B, PosNegMin) ->
    exit({not_yet, slg_negative, {Akey,G,B,PosNegMin}}).


%%%----------------------------------------------------------------
slg_complete(Akey, PosNegMin) ->
    {PosMin,NegMin} = PosNegMin,
    set_poslink(Akey, PosLink_A = min(poslink(Akey),PosMin)),
    set_neglink(Akey, NegLink_A = min(neglink(Akey),NegMin)),
    DFN_A = dfn(Akey),
    if PosLink_A==DFN_A -> 
	    if NegLink_A==maxint ->
%%		    pop_and_call(Akey, PosNegMin);
		    SccRev = pop_all(Akey),
		    L = complete_make_newgoals(SccRev),
		    call_slg_newclause(L, {maxint,maxint});
		NegLink_A >= DFN_A ->
		    exit({not_yet,
			  'slg_complete_continue clause 2',
			  {NegLink_A, DFN_A, Akey, PosNegMin}});
		true ->
		    PosNegMin
	    end;
	true ->
	    PosNegMin
    end.

pop_all(Akey) ->
    pop_all(pop(), Akey, []).

pop_all({Bkey,B,DFN_B,PosLink_B,NegLink_B}, Akey, Scc) ->
    if  Bkey==Akey -> 
	    [{Bkey,B}|Scc];
	true ->
	    pop_all(pop(), Akey, [{Bkey,B}|Scc])
    end.


complete_make_newgoals(SccRev) ->
    complete_make_newgoals(SccRev, []).

complete_make_newgoals([{Bkey,B}|Scc], L) ->
    Negs = negs(Bkey),
    set_comp(Bkey, true),
    reset_poss(Bkey),
    reset_negs(Bkey),
    %% slg_simplify(Bkey),
    complete_make_newgoals(Scc, inner_loop(Negs,Bkey,B,L));
complete_make_newgoals([], L) ->
    L.

inner_loop([{Akeyp,G}|Negs], Bkey, B, L0) ->
    L = case anss(Bkey) of
	    [] ->
		[{Akeyp,delete({'#not',1,B},G)}|L0];
	    _ ->
		case ets:match(get(table_ref), {{anss,Bkey},{B,[],[]}}) of
		    [] ->  %% Not found
			[{Akeyp,delay({'#not',1,B},G)}|L0];
		    [[]] -> %% Found
			L0
		end
	end,
    inner_loop(Negs, Bkey, B, L);
inner_loop([], _, _, L) ->
    L.


%%%----------------------------------------------------------------
%%%
%%% Support Routines
%%%

slg_factor(H, Li, G) -> 
    exit({not_yet,slg_factor,{H,Li,G}}).

slg_resolvent(G, Li, C) -> 
    {A,D,Ls} = G,
    Cp = mnemosyne_unify:rename_variables(C),
    Ap = head(Cp),
    Bnds = mnemosyne_unify:unify(Li,Ap),
    mnemosyne_unify:instantiate({A,D,replace(Ls,Li,body(Cp))}, Bnds).
    
replace([H|T], X, New) when H==X ->  lists:append(New,T);
replace([H|T], X, New) -> [H | replace(T,X,New)].

%%----
subsumed_by_any_anss(Akey, G) -> subsumed_by_any_anss1(head(G), anss(Akey)).

subsumed_by_any_anss1(H, [L|Ls]) ->
    exit({mnemosyne_bug});
%    case mnemosyne_unify:subsumes_chk(H, head(L)) of
%	true -> true;
%	false -> subsumed_by_any_anss1(H,Ls)
%    end;
subsumed_by_any_anss1(_, []) ->
    false.


%%----
'no other has the same head'(Anss, G) ->
    exit({not_yet, 'no other has the same head', {Anss,G}}).


%%----
select_literal({H,D,[B|Bs]}) -> B.

head({H,D,B}) -> H.
delayed_literals({H,D,B}) -> D.
body({H,D,B}) -> B.

delete(X, {H,D,B}) -> {H,D,lists:delete(X,B)}.
delay(X, {H,D,B}) -> {H,[X|D],lists:delete(X,B)}.

%%%----
%% returns a list of slg-resolvents of
%% A:-A with all C in program P such that A unifies C
%% A little short-cut wich just fetches matching clauses from the DB...

slg_resolvents_P_A(P) when record(P,pred_sym) ->
    case P#pred_sym.type of
	rule ->
	    lists:map(fun(Body) -> {P,[],Body} end,
		      mnemosyne_lib:db_read_clauses(P));
	table -> 
	    ?not_yet({?MODULE,slg_resolvents_P_A,table,P#pred_sym.functor})
    end.


key(A) when record(A,pred_sym) -> mnemosyne_unify:numbervars(A#pred_sym.args).

min(A,B) when A<B -> A;
min(_,B) -> B.

%%min(A,B,C) -> min(A,min(B,C)).

%%%----------------------------------------------------------------
%%%
%%% Access routines for the global table, the stack and the counter
%%%

%%% Table:

create_table_stack() ->
    put(table_ref, Tab=ets:new(table,[bag,protected])),
    ets:insert(Tab, {stack,[]}),
    ets:insert(Tab, {counter,1}),
    ok.
    
in_table(Key) ->
    case table_lookup({comp,Key}) of
	[] -> false;
	L when list(L) -> true
    end.

new_table_entry(Key, Poss, Negs) ->
    Tab = get(table_ref),
    case ets:lookup(Tab,{comp,Key}) of
	[] -> 
	    ets:insert(Tab, {{comp,Key},false}),
	    add_to({negs,Key}, Negs, Tab),
	    add_to({poss,Key}, Poss, Tab);
	_ ->
	    exit({already_in_table,Key})
    end.

anss(Key) -> table_lookup({anss,Key}).
poss(Key) -> table_lookup({poss,Key}).
negs(Key) -> table_lookup({negs,Key}).
comp(Key) -> hd(table_lookup({comp,Key})).
ans_pid(Key) ->
    case table_lookup({ans_pid,Key}) of
	[none] -> false;
	[Asker] -> Asker;
	_ -> false
    end.

add_to_anss(Key,Val) -> 
    table_add_to({anss,Key},Val),
    case ans_pid(Key) of
	false -> ok;
	Asker -> send_ans(Val,Asker)
    end.

add_to_poss(Key,Val) -> table_add_to({poss,Key},Val).
%%-add_to_negs(Key,Val) -> table_add_to({negs,Key},Val).

%%-set_anss(Key,Val) -> table_set({anss,Key},Val).
%%-set_poss(Key,Val) -> table_set({poss,Key},Val).
%%-set_negs(Key,Val) -> table_set({negs,Key},Val).
set_comp(Key,Val) -> table_set({comp,Key},Val).
set_ans_pid(Key,Val) -> table_set({ans_pid,Key},Val).

reset_poss(Key) -> table_reset({poss,Key}).
reset_negs(Key) -> table_reset({negs,Key}).

%%% Stack:
%%%    A stack item: {Akey,A,DFN,PosLink,NegLink}

push(Item) -> 
    Tab = get(table_ref),
    [{stack,Stack}] = ets:lookup(Tab,stack),
    ets:delete(Tab, stack),
    ets:insert(Tab, {stack,[Item|Stack]}).

pop() -> 
    Tab = get(table_ref),
    case  ets:lookup(get(table_ref),stack) of
	[] -> 
	    exit(stack_underflow);
	[{stack,[Top|Stack]}] -> 
	    ets:delete(Tab, stack),
	    ets:insert(Tab, {stack,Stack}),
	    Top
    end.

dfn(Akey) -> stack_lookup(Akey,3).
poslink(Akey) -> stack_lookup(Akey,4).
neglink(Akey) -> stack_lookup(Akey,5).

set_poslink(Akey,Val) -> stack_set(Akey,4,Val).
set_neglink(Akey,Val) -> stack_set(Akey,5,Val).

%%% Counter, PosMin and NegMin:

%%-set_posmin(Val) -> table_set(posmin,Val).
%%-set_negmin(Val) -> table_set(negmin,Val).

%%-posmin() -> table_single_val(posmin).
%%-negmin() -> table_single_val(negmin).
%%-counter() -> table_single_val(counter).

ref_step_counter() ->
    Tab = get(table_ref),
    [{counter,Counter}] = ets:lookup(Tab,counter),
    ets:delete(Tab, counter),
    ets:insert(Tab, {counter,Counter+1}),
    Counter.

%%---- table access support routines

table_lookup(Key) -> getval(ets:lookup(get(table_ref),Key), []).

getval([{_,Val}|T],L) -> getval(T,[Val|L]);
getval([], L) -> L.

%%-table_single_val(Key) ->
%%-    [{_,Val}] = ets:lookup(get(table_ref),Key),
%%-    Val.

table_add_to(Key,Val) -> ets:insert(get(table_ref), {Key,Val}).

table_set(Key,Val) ->
    Tab = get(table_ref),
    ets:delete(Tab, Key),
    ets:insert(Tab, {Key,Val}).

table_reset(Key) -> ets:delete(get(table_ref), Key).

add_to(Key, [Val|Vals], Tab) ->
    ets:insert(Tab, {Key,Val}),
    add_to(Key, Vals, Tab);
add_to(_, [], _) ->
    ok.


%%---- stack access support routines

stack_lookup(Key, N) ->
    [{stack, Stack}] = ets:lookup(get(table_ref),stack),
    {value, Tup} = lists:keysearch(Key,1,Stack),
    element(N, Tup).

stack_set(Key, N, Val) ->
    Tab = get(table_ref),
    [{stack, Stack}] = ets:lookup(Tab,stack),
    ets:delete(Tab, stack),
    ets:insert(Tab, {stack,item_replace(Stack,Key,N,Val)}).
    
item_replace([I|Is], Key, N, Val) when element(1,I)==Key ->
    [setelement(N,I,Val) | Is];
item_replace([I|Is], Key, N, Val) ->
    [I | item_replace(Is,Key,N,Val)].
