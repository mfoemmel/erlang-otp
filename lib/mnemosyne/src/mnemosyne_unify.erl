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
-module(mnemosyne_unify).
-export([add_bind_trigger/3, bindings_to_list/1, count_bs_vars/1, 
	 bs_union/2, bs_vars/1, bs_vars_value/1, check_triggers/1,
	 count_bs_vars/2, count_variables/1, count_variables/2, 
	 delete_bindings/3, deref_bindings/1, empty_bindings/0, instantiate/2, 
	 list_to_bindings/1, numbervars/1, numbervars/2, 
	 rename_variables/1, sing_vars/2,
	 unif/3, unify/2, unify/3, variables/1, variables_and_annonymous/1,
	 ground/1]).

%%%================================================================
%%% 		Exports


%%---- Unify the two terms U and V, possibly with the bindings Bs in effect.
%%     Returns false or new bindings


unify(U, V) -> catch unif(U, V, []).

unify(U, V, Bs) -> catch unif(U, V, Bs).


%%---- returns an ordset with the variable names in U

ground(U) ->
    case variables_and_annonymous(U) of
	[] -> true;
	_ -> false
    end.

variables_and_annonymous(U) -> 
    variables(U, ordsets:new()).

variables(U) -> 
    ordsets:del_element('_', variables(U, ordsets:new())).
    

%%---- returns a {Var,Num} list of the frequency of the variables in U

count_variables(U) -> count_variables(U,[]).

%%---- returns a {Var,1} list of the variables (Keys) in Bs
count_bs_vars(Bs) -> count_bs_vars1(Bs,[]).
count_bs_vars(Bs, Dict) -> count_bs_vars1(Bs, Dict).


sing_vars([{V,1}|T], Acc) -> sing_vars(T, [V|Acc]);
sing_vars([_|T], Acc) -> sing_vars(T, Acc);
sing_vars([], Acc) -> Acc.


%%---- returns all (key) variables and all (key) variables bound to a value
%%     respecitvely

bs_vars(Bs) -> get_bound_vars(Bs,all).

bs_vars_value(Bs) -> get_bound_vars(Bs,'#value').


%%---- replace variables by numbers (as in prolog)

numbervars(T) ->  numbervars(T,0).

numbervars(T, N) -> numbervars(T, N, variables(T)).

%%---- returns U but with all variables renamed to new ones

rename_variables(U) ->
    {V,_} = rename_variables(U, []),
    V.

%%---- Binds all variables in U to the value in the bindings Bs

instantiate(U, []) -> U;
instantiate(U, Bs) -> inst(U, Bs).

deref_bindings(Bs) -> deref_bs(Bs,Bs).
%%    list_to_bindings( deref_bs(bindings_to_list(Bs),Bs,[]) ).

%%---- keep or remove the bindings which have the variable in OrdSet

delete_bindings(Tree, KeepRemove, OrdSet) when is_tuple(Tree) ->
    {L,Key,KeyVal,R} = Tree,
    case {KeepRemove,ordsets:is_element(Key,OrdSet)} of
	{keep, false} -> remove(L,R,KeepRemove,OrdSet);
	{remove,true} -> remove(L,R,KeepRemove,OrdSet);
	_ -> {delete_bindings(L, KeepRemove, OrdSet),
	      Key,KeyVal,
	      delete_bindings(R, KeepRemove, OrdSet)}
    end;

delete_bindings([], _, _) ->
    [].
	    
%%---- The empty bindings

empty_bindings() -> [].    

%%---- Conversion    

bindings_to_list(Bs) ->
    bindings_to_list(Bs,[]).

list_to_bindings(L) ->
    list_to_bindings(L, empty_bindings()).

%%---- combine two bindings into one

bs_union(Bs1, []) -> Bs1;
bs_union([], Bs2) -> Bs2;
bs_union(Bs1, Bs2) ->
    list_to_bindings(bindings_to_list(Bs1) ++ bindings_to_list(Bs2)).

%%---- Check triggers, IN COMPILE-TIME

check_triggers(Bs) -> 
    put(when_called, compile_time),
    R = (catch check_triggers(Bs, Bs)),
    erase(when_called),
    case R of
	fail ->  throw(fail);
	Others -> Others
    end.

check_triggers({L,K,V,R}, Bs) ->
    {check_triggers(L,Bs),
     K,
     case V of
	 {'#bind_trigger',Ts} -> 
	     {'#bind_trigger', apply_bind_triggers(Ts,Bs)}; % can throw(fail)
	 _ ->
	     V
     end,
     check_triggers(R,Bs)
    };
check_triggers([],Bs) ->
    [].
    

%%%================================================================
%%% 		Private

%%%----------------------------------------------------------------
unif(X, X, Bs) ->
    Bs;

%% Annonymous variables unifyies with anything:
unif({'#var','_'}, _, Bs) -> Bs;
unif(_, {'#var','_'}, Bs) -> Bs;

unif({'#var',U}, {'#var',V}, Bs) -> 
    case deref(var_val,U,Bs) of 
	{'#var',V} ->				% U is bound to V already
	    Bs;

	{'#var',X} ->				% U is bound to unbound var X
	    bind(X,deref(var_val,V,Bs),Bs);	%   (which might be U)

	{'#value',U_value} ->			% U is bound to non-var already
	    unif({'#var',V}, U_value, Bs)
    end;

unif({'#var',U}, V, Bs) ->		% V is non-var
    bind(U, V, Bs);

unif(U, {'#var',V}, Bs) ->		% U is non-var
    bind(V, U, Bs);

unif([U|Us], [V|Vs], Bs) -> 
    unif(Us, Vs, unif(U,V,Bs));

unif(U, V, Bs) when is_tuple(U), is_tuple(V), size(U)==size(V) ->
    unif(tuple_to_list(U), 
	 tuple_to_list(V),
	 Bs);

unif(_, _, _) ->
    throw(fail).

%%%----------------------------------------------------------------
%%% bind(VariableNAME, Value, Bindings)

bind('_', _, Bs) ->
    Bs;

bind(U, {'#var',V}, Bs) ->
    {'#var',Ulast} = deref(var, U, Bs),
    {'#var',Vlast} = deref(var, V, Bs),
    bind(Ulast, deref(any,Ulast,Bs), Vlast, deref(any,Vlast,Bs), Bs);

bind(U, {'#value',NonVar}, Bs) ->
    {'#var',Ulast} = deref(var, U, Bs),
    bind(Ulast, deref(any,Ulast,Bs), [], {'#value',NonVar}, Bs);

bind(U, NonVar, Bs) ->
    {'#var',Ulast} = deref(var, U, Bs),
    bind(Ulast, deref(any,Ulast,Bs), [], {'#value',NonVar}, Bs).



bind(Ulast, _, Ulast, _, Bs) ->			% Don't bind V to itself!! 
    Bs;

bind(Ulast, {'#var',Ulast}, Vlast, {'#bind_trigger',Cv}, Bs) ->
    mk_binding(Ulast, {'#var',Vlast}, Bs);

bind(Ulast, {'#var',Ulast}, Vlast, Vany, Bs) ->
    mk_binding(Ulast, Vany, Bs);

bind(Ulast, {'#bind_trigger',Cu}, Vlast, VlastAny, Bs) ->
    case VlastAny of
	{'#bind_trigger',Cv} ->
	    rebind(Ulast, {'#var',Vlast}, 
		   rebind(Vlast, 
			  {'#bind_trigger',
			   ordsets:from_list(lists:append(Cv,Cu))},
			  Bs));

	{'#var',Vlast} ->
	    mk_binding(Vlast, {'#var',Ulast}, Bs);

	{'#value',Value} ->
	    NewBs = rebind(Ulast, {'#value',Value}, Bs),
	    apply_bind_triggers(Cu,NewBs),		% can throw(fail)
	    NewBs
    end;

bind(Ulast, {'#value',Uvalue}, Vlast, {'#value',Vvalue}, Bs) ->
    unif(Uvalue, Vvalue, Bs);

bind(Ulast, Uany, Vlast, Vany, Bs) ->
    throw(fail).



%%%----------------------------------------------------------------
%%%
%%%  Triggers
%%%

%%--- returns false or the bindings with the trigger added

add_bind_trigger([], Bind_Trigger, Bs) ->
    apply_bind_triggers([Bind_Trigger], Bs),
    Bs;

add_bind_trigger(Ts, Bind_Trigger, Bs) ->
    add_bind_trigger1(Ts, Bind_Trigger, Bs).



add_bind_trigger1([V|Vs], Bind_Trigger, Bs) ->
    add_bind_trigger1(Vs, 
		     Bind_Trigger,
		     add_bind_trigger1(V,Bind_Trigger,Bs));

add_bind_trigger1([], _, Bs) ->
    Bs;

add_bind_trigger1('_', Bind_Trigger, Bs) ->
    Bs;

add_bind_trigger1(VarName, Bind_Trigger, Bs)  ->
    {'#var', LastVar} = deref(var,VarName,Bs),
    case deref(any,LastVar,Bs) of
	{'#bind_trigger',Tr} ->			% VarName has already triggers
	    rebind(LastVar, 
		   {'#bind_trigger', ordsets:from_list([Bind_Trigger|Tr])},
		   Bs);

	{'#var', LastVar} ->			% set first trigger
	    mk_binding(LastVar, 
		       {'#bind_trigger',ordsets:from_list([Bind_Trigger])}, 
		       Bs);

	{'#value',Value} ->			% Too late! Just check
	    apply_bind_triggers([Bind_Trigger], Bs),%can throw(fail)
	    Bs
    end.

%%---- run when a variable is bound to Value
%%     returns the list of still valid triggers or throw(fail)

apply_bind_triggers(Triggers, Bs) ->
    apply_bind_triggers(Triggers, Bs, ordsets:new()).


apply_bind_triggers([H|T], Bs, KeptTriggers) when is_list(H) ->
    apply_bind_triggers(T, Bs, apply_bind_triggers(H,Bs,KeptTriggers));

apply_bind_triggers([T|Ts], Bs, KeptTriggers) ->
    {M,F,A} = T,
    case apply(M,F,[Bs|A]) of	% can throw(fail)
	remove_trigger ->
	    apply_bind_triggers(Ts, Bs, KeptTriggers);

	keep_trigger ->
	    apply_bind_triggers(Ts, Bs, 
				ordsets:add_element(T,KeptTriggers))
    end;

apply_bind_triggers([], Bs, KeptTriggers) ->
    KeptTriggers.

	
%%%----------------------------------------------------------------
variables({'#var',U}, Set) ->
    ordsets:add_element(U, Set);

variables([U|Us], Set) -> 
    variables(Us, variables(U,Set));

variables(U, Set) when is_tuple(U) ->
    variables(tuple_to_list(U), Set);

variables(_, Set) ->
    Set.

%%%----------------------------------------------------------------
count_variables({'#var','_'}, Dict) ->
    Dict;

count_variables({'#var',V}, Dict) ->
    insert_count_dict(V, Dict);

count_variables([U|Us], Dict) -> 
    count_variables(Us, count_variables(U,Dict));

count_variables(U, Dict) when is_tuple(U) ->
    count_variables(tuple_to_list(U), Dict);

count_variables(_, Dict) ->
    Dict.


insert_count_dict(V, Dict) ->
    case lists:keysearch(V,1,Dict) of
	{value, {V,N0}} ->
	    lists:keyreplace(V,1,Dict,{V,N0+1});
	false ->
	    [{V,1}|Dict]
    end.

%%%----------------------------------------------------------------

rename_variables({'#var','_'}, S) -> 
    {{'#var','_'}, S};

rename_variables({'#var',X}, S) -> 
    case lists:keysearch(X,1,S) of
	{value,{X,V}} -> 
	    {V,S};
	false -> 
	    V = mnemosyne_lib:unique_var(X),
	    {V,[{X,V}|S]}
    end;

rename_variables([H|T], S) ->
    {Hp,Hs} = rename_variables(H, S),
    {Tp,Ts} = rename_variables(T, Hs),
    {[Hp|Tp], Ts};

rename_variables(T, S) when is_tuple(T) ->
    {Tp,Ts} = rename_variables(tuple_to_list(T), S),
    {list_to_tuple(Tp), Ts};

rename_variables(X, S) -> 
    {X,S}.


%%%----------------------------------------------------------------
deref_bs({L,K,V0,R}, Bs) ->
    V = 
	case V0 of
	    {'#var',_} ->
		case deref(any,K,Bs) of
		    {'#value',Val} -> {'#value',inst(Val,Bs)};
		    Others -> Others
		end;
	    {'#bind_trigger',Ts} ->
		{'#bind_trigger', apply_bind_triggers(Ts,Bs)};
	    Others ->
		inst(V0,Bs)
	end,
    {deref_bs(L,Bs), K, V, deref_bs(R,Bs)};
deref_bs([], Bs) ->
    [].


count_bs_vars1({L,V,_,R}, Dict) ->
    count_bs_vars1(L, count_bs_vars1(R,insert_count_dict(V,Dict)));
count_bs_vars1([], Dict) ->
    Dict.

%count_bs_vars1([{V,_}|Vs], Dict) -> 
%    count_bs_vars1(Vs, insert_count_dict(V,Dict));
%count_bs_vars1([], Dict) -> Dict.

%deref_bs([{U,X}|L], Bs, Acc) ->
%    New = 
%	case X of
%	    {'#var',V} ->
%		case deref(any,U,Bs) of
%		    {'#value',Val} -> {'#value',inst(Val,Bs)};
%		    Others -> Others
%		end;
	    
%	    {'#bind_trigger',Ts} ->
%		{'#bind_trigger', apply_bind_triggers(Ts,Bs)};
	    
%	    Others ->
%		inst(X,Bs)
%	end,
%    deref_bs(L, Bs, [{U,New}|Acc]);

%deref_bs([], _, Acc) ->
%    Acc.


inst([H|T], Bs) -> 
    [inst(H,Bs) | inst(T,Bs)];

inst({'#var',V}, Bs) ->
    %% return last variable in chain or value but not a bind_trigger
    case deref(var_val, V, Bs) of
	{'#value',Val} -> inst(Val,Bs);
	Var -> Var
    end;

inst(X, Bs) when is_tuple(X) -> 
    list_to_tuple(inst(tuple_to_list(X), Bs));

inst(X, Bs) ->
    X.


get_bound_vars(Bs, What) -> get_bound_vars(Bs, Bs, What, ordsets:new()).

get_bound_vars({L,V,{Type,_},R}, Bs, What, Set) ->
    get_bound_vars(L, Bs, What, 
		   get_bound_vars(R, Bs, What,
				  update_bound_set(Type,Bs,What,V,Set)));
get_bound_vars([], _, _, Set) ->
    Set.

update_bound_set(Type, Bs, all, V, Set) ->
    ordsets:add_element(V,Set);
update_bound_set(Type, Bs, What, V, Set) ->
    case Type of
	What -> 
	    ordsets:add_element(V,Set);
	'#var' ->
	    case deref(any,V,Bs) of
		{What,_} -> ordsets:add_element(V,Set);
		_ -> Set
	    end;
	_ ->
	    Set
    end.


%%%----------------------------------------------------------------
%%% deref(Type, VariableNAME, Bindings)  find out what VariableNAME is bound to
%%%    follows chains like X->Y->.....->Z->VAL
%%%                        VAL = {'#value',    Value}
%%%                              {'#var',      Name}
%%%                              {'#bind_trigger',Bind_Trigger}
%%%    if Type = var_val : returns last '#var' or '#value'
%%%       Type = var     : returns last '#var'
%%%       Type = any     : returns VAL


deref(Type, Var, Bs) ->
    case BoundTo=lookup_binding(Var,Bs) of
	'#false' -> {'#var',Var};

	{'#var',X} when X=/=Var -> deref(Type, X, Bs);

	{'#value',Value}  when Type== var -> {'#var',Var};
	{'#value',Value}                  -> BoundTo;

	{'#bind_trigger',C} when Type==var_val ->  {'#var',Var};
	{'#bind_trigger',C} when Type==var     ->  {'#var',Var};
	{'#bind_trigger',C} when Type==any     ->  BoundTo
    end.

%%%----------------------------------------------------------------
remove(L, R, KeepRemove, OrdSet) ->
    %% No common keys in L and R
    insert_bs_bs(delete_bindings(L, KeepRemove, OrdSet),
		 delete_bindings(R, KeepRemove, OrdSet)).
    

%%% returns Bs2 + those bindings in Bs1 that are not present in Bs2

insert_bs_bs(Bs1, Bs2) ->
    insert_bs_bs(bindings_to_list(Bs1), bindings_to_list(Bs2), 
		 empty_bindings()).

insert_bs_bs([{K1,V1}|L1], [{K2,V2}|L2], Acc) ->
    if
	K1 < K2 -> insert_bs_bs(L1, [{K2,V2}|L2], mk_binding(K1,V1,Acc));
	K1 > K2 -> insert_bs_bs([{K1,V1}|L1], L2, mk_binding(K2,V2,Acc));
	K1 == K2-> insert_bs_bs(L1, L2, mk_binding(K2,V2,Acc))
    end;
insert_bs_bs([], [], Acc) -> Acc;
insert_bs_bs([], [{K,V}|L], Acc) -> insert_bs_bs([], L, mk_binding(K,V,Acc));
insert_bs_bs([{K,V}|L], [], Acc) -> insert_bs_bs(L, [], mk_binding(K,V,Acc)).

%%%================================================================
%%%
%%% Access functions for the Bindings data structure
%%%
%%%

lookup_binding(Var, {L,Key,Value,R}) ->
    if
	Var<Key  -> lookup_binding(Var, L);
	Var>Key  -> lookup_binding(Var, R);
	Var==Key -> Value
    end;
lookup_binding(Var, []) -> 
    '#false'.


%% When the Var already is bound to Value there will be some unnecessary
%% setelement

mk_binding(Var, Value, Tree) when is_tuple(Tree) ->
    {L,Key,KeyVal,R} = Tree,
    if
	Var<Key -> setelement(1, Tree, mk_binding(Var,Value,L));
	Var>Key -> setelement(4, Tree, mk_binding(Var,Value,R));
	Key==Var, KeyVal==Value -> Tree;
	Key==Var -> throw(fail)
    end;
mk_binding(Var, Value, []) -> 
    {[],Var,Value,[]}.



rebind(Var, Value, Tree) when is_tuple(Tree) ->
    {L,Key,KeyVal,R} = Tree,
    if
	Var<Key  -> setelement(1, Tree, rebind(Var,Value,L));
	Var>Key  -> setelement(4, Tree, rebind(Var,Value,R));
	Key==Var -> setelement(3, Tree, Value)
    end;
rebind(Var, Value, []) -> 
    {[],Var,Value,[]}.


bindings_to_list({L,K,V,R}, Acc) ->
    bindings_to_list(L, [{K,V}|bindings_to_list(R,Acc)]);
bindings_to_list([], Acc) ->
    Acc.


list_to_bindings([{V,Val}|L], Bs) ->
    list_to_bindings(L, mk_binding(V,Val,Bs));
list_to_bindings([], Bs) ->
    Bs.


%%%----------------------------------------------------------------

numbervars(T, N, Vars) when Vars==[] -> 
    T;
numbervars(T, N, Vars) ->
    numbervars1(T, assign_numbers(Vars,N,[])).

numbervars1({'#variable',V}, D) ->
    {'#variable',{'#var',element(2,ok(lists:keysearch(V,1,D)))}};
numbervars1(T, D) when is_tuple(T) ->
    list_to_tuple(numbervars1(tuple_to_list(T), D));
numbervars1([H|T], D) ->
    [numbervars1(H,D) | numbervars1(T,D)];
numbervars1(T, _) ->
    T.

ok({value,V}) -> V.

assign_numbers([V|Vs], N, L) -> assign_numbers(Vs, N+1, [{V,N}|L]);
assign_numbers([],_,L) -> L.

