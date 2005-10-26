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
-module(mnemosyne_optimizer).
-export([format_error/1, phase1/2, phase2_no_statistics/1, phase2/1]).

%%-define(debug,2).
-define(no_not_yet,yes).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%================================================================
%%% 		Exports

phase1(ListOfGoalsLists, Pattern) ->
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(ListOfGoalsLists)]),
    {Disj0, FnAssignVars} = separate_constraints_and_fns(ListOfGoalsLists),
    WantedVariables = ordsets:union(mnemosyne_unify:variables(Pattern),
				    FnAssignVars),
    Disj1 = deduce_bindings_bind(Disj0, WantedVariables),
%    Disj2 = remove_obsolete_subgoals(Disj1, WantedVariables),
    Disj3 = remove_obsolete_clauses(Disj1),
    {CommonBs, Disj}  = extract_common_bindings(Disj3), 
    
    NewPattern = mnemosyne_unify:instantiate(Pattern, CommonBs),
    ?debugmsg(3, "New Pattern: ~s\n", [mnemosyne_pp:e(NewPattern)]),
    test_range_restricted(NewPattern, Disj),
    KeepVars = 
	ordsets:union(
	  ordsets:intersection(mnemosyne_unify:bs_vars(CommonBs), 
			       mnemosyne_unify:variables({NewPattern,Disj})),
	  FnAssignVars),
    R =how_to_eval(#optimizer_result{pattern=NewPattern,
				     common_bs=
				     mnemosyne_unify:delete_bindings(CommonBs,
								     keep,
								     KeepVars),
				     code=Disj}),
    ?debugmsg(2, "Phase 1 result (~w):  ~s | ~s\n", 
	      [R#optimizer_result.how_to_eval,
	       mnemosyne_pp:e(R#optimizer_result.pattern), 
	       mnemosyne_pp:body(R)]),
    R.


phase2(Opr0) ->
    case Opr0#optimizer_result.how_to_eval of
	query_eval ->
	    Opr1 = subgoal_ordering(Opr0),
	    phase2_no_statistics(Opr1);
	_->
	    Opr0
    end.

phase2_no_statistics(Opr1) ->
    Opr2 = adornments(Opr1),
    Opr3 = push_bindings(Opr2),
    Opr4 = push_function_calls(Opr3),
    Opr = build_tree(Opr4),
    ?debugmsg(2, "Opt 2 goal:  ~s | ~s\n", 
	      [mnemosyne_pp:e(Opr#optimizer_result.pattern),
	       mnemosyne_pp:body(Opr#optimizer_result.code)]),
    Opr.


format_error(Msg) ->
    case Msg of
	
	{not_range_restricted,Vars,Goals} when length(Vars)>1 ->
	    io_lib:format("The variables ~w is not instantiated in "
			  "the goal path ~s",
			  [Vars, mnemosyne_pp:body(Goals)]);
	
	{not_range_restricted,[Var],Goals} ->
	    io_lib:format("The variable ~w is not instantiated in "
			  "the goal path ~s",
			  [Var, mnemosyne_pp:body(Goals)])
    end.



%%%================================================================
%%% 		Private

%%%----------------------------------------------------------------
%% ListOfGoalsList is list of list of Goals.
%% returns a list of the record disj_alt.

separate_constraints_and_fns({'#or',_,Alts}) ->
    Res = lists:map(
	    fun(Alt) ->
		    separate_constraints(Alt#disj_alt.conj)
	    end, Alts),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Res)]),
    {Res, fnvarset(Res)};
separate_constraints_and_fns(Goals) ->
    Res = [separate_constraints(Goals)],
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Res)]),
    {Res, fnvarset(Res)}.


fnvarset(Ds) when list(Ds) ->
    lists:foldl(
      fun(D,S) when record(D,disj_alt) ->
	      lists:foldl(
		fun(F,S1) when record(F,fn) ->
			ordsets:add_element(F#fn.alias_var, S1)
		end, S, D#disj_alt.fncalls)
      end, ordsets:new(), Ds).


separate_constraints(Goals) ->
    separate_constraints(Goals, #disj_alt{}, false).

separate_constraints(Goals, Acc, Invert) ->
    lists:foldl(
      fun(Goal,Accu) ->
	      separate_one(Goal,Accu,Invert)
      end, 
      Acc, Goals).



separate_one({'#not',C, NGs}, Acc, Invert) ->
    Nacc = separate_constraints(NGs, Acc#disj_alt{conj=[]}, inv(Invert)),
    Nacc#disj_alt{conj=
		  case Nacc#disj_alt.conj of
		      [] -> Acc#disj_alt.conj;
		      NewNGs ->[{'#not',C,NewNGs} | Acc#disj_alt.conj]
		  end};

separate_one(C, Acc, Invert) when record(C,constraint), Invert==false ->
    {Csep, FnCalls} = separate_fns(C, Acc#disj_alt.fncalls),
    case Csep#constraint.op of
	'=' ->  
	    Acc#disj_alt{alias = [Csep|Acc#disj_alt.alias],
			 fncalls = FnCalls};
	_ ->
	    Acc#disj_alt{constraints = [Csep|Acc#disj_alt.constraints],
				       fncalls = FnCalls}
    end;

separate_one(E, Acc, Invert) when record(E,erl_expr) ->
    {Esep, FnCalls} = separate_fns(E, Acc#disj_alt.fncalls),
    Acc#disj_alt{fncalls = FnCalls,
		 conj = [Esep | Acc#disj_alt.conj]};

separate_one(C, Acc, Invert) when record(C,constraint), Invert==true ->
    separate_one(mnemosyne_constraint:invert_constraint(C), Acc, false);

separate_one(Goal, Acc, Invert) ->
    Acc#disj_alt{conj=[Goal|Acc#disj_alt.conj]}.



separate_fns({'#funcall',M,F,Args0}, Acc0) ->
    {Args,Acc} = separate_fns(Args0,Acc0),
    FunCall = {'#funcall',M,F,Args},
    case lists:keysearch(FunCall,#fn.fndef,Acc) of
	{value, Fn} ->
	    {Fn#fn.alias_var, Acc};
	false ->
	    GenVar = mnemosyne_lib:unique_var('FnTmp'),
	    Fn = #fn{alias_var = GenVar,
		     fndef = FunCall},
	    {GenVar, [Fn|Acc]}
    end;

separate_fns([H0|T0], Acc0) ->
    {H, Acc1} = separate_fns(H0, Acc0),
    {T, Acc} = separate_fns(T0, Acc1),
    {[H|T], Acc};

separate_fns(T0, Acc0) when tuple(T0) -> 
    {T, Acc} = separate_fns(tuple_to_list(T0), Acc0),
    {list_to_tuple(T), Acc};
	    
separate_fns(X, Acc) ->
    {X, Acc}.


inv(true) -> false;
inv(false) -> true.

%%%----------------------------------------------------------------
%% WantedVariables is ordset of variable names.
%% returns list of the record disj_alt.

-record(dbd, {bs=[],
	      constr_vars=ordsets:new_set(),
	      remove_vars=ordsets:new_set(),
	      wanted_vars,
	      goals=[]}).


deduce_bindings_bind(Disj, WantedVariables) ->
    Res = deduce_bindings_bind(Disj, WantedVariables, []),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Res)]),
    Res.


deduce_bindings_bind([Conj|Conjs], Vars, Acc) when record(Conj,disj_alt) ->
    deduce_bindings_bind(Conjs, Vars,
			 case catch deduce_bindings_bind1(Conj,Vars) of
			     fail -> Acc;
			     New ->  [New|Acc]
			 end);
deduce_bindings_bind([], _, []) ->  throw(fail);
deduce_bindings_bind([], _, Acc) -> Acc.


deduce_bindings_bind1(Conj, Vars) ->
    Bs00 = alias_to_bindings(Conj#disj_alt.alias,
			     mnemosyne_unify:empty_bindings(), 
			     Vars),
    Bs01 = mnemosyne_unify:deref_bindings(Bs00),
    Bs02 = prefer_wanted_vars(Bs01, Vars),
    Bs0 = mnemosyne_unify:deref_bindings(Bs02),
    Bs1 = constraints_bindings(
	    mnemosyne_unify:instantiate(Conj#disj_alt.constraints ++
					Conj#disj_alt.fncalls,
					Bs0),
	    Bs0),
    
    InstGoals = mnemosyne_unify:instantiate(Conj#disj_alt.conj, Bs1),
    FnCalls = mnemosyne_unify:instantiate(Conj#disj_alt.fncalls, Bs1),
    KeepVars = 
	ordsets:union(Vars,  mnemosyne_unify:variables([InstGoals,FnCalls])),
    Bs = mnemosyne_unify:delete_bindings(Bs1, keep, KeepVars),
    Conj#disj_alt{bs = Bs,
		  conj = InstGoals,
		  fncalls = FnCalls,
		  alias = [],
		  constraints = []}.


prefer_wanted_vars(Bs, Vars) ->
    mnemosyne_unify:list_to_bindings(
      prefer_wanted_vars(mnemosyne_unify:bindings_to_list(Bs), Vars, [])
     ).

prefer_wanted_vars([{V,{'#var',V1}}|L], Vars, Acc) ->
    case ordsets:is_element(V1,Vars) of
	false ->
	    case ordsets:is_element(V,Vars) of
		true ->
		    prefer_wanted_vars(L, 
				       ordsets:del_element(V1,Vars), 
				       [{V1,{'#var',V}}|Acc]);
		false ->
		    prefer_wanted_vars(L, Vars, [{V,{'#var',V1}}|Acc])
	    end;
	true ->
	    prefer_wanted_vars(L, Vars, [{V,{'#var',V1}}|Acc])
    end;
prefer_wanted_vars([X|L], Vars, Acc) ->
    prefer_wanted_vars(L, Vars, [X|Acc]);
prefer_wanted_vars([], Vars, Acc) ->
    Acc.



alias_to_bindings([C|Cs], Bs0, Vars) ->
    Bs = 
	case {C#constraint.exprL, C#constraint.exprR} of
	    { {'#var',Vl}, {'#var',Vr} } ->
		case ordsets:is_element(Vl,Vars) of
		    true -> 
			mnemosyne_unify:unif({'#var',Vr}, {'#var',Vl}, Bs0);
		    false -> 
			mnemosyne_unify:unif({'#var',Vl}, {'#var',Vr}, Bs0)
		end;
	    
	    {El, Er} ->
		mnemosyne_unify:unif(El, Er, Bs0)
	end,
    alias_to_bindings(Cs, Bs, Vars);
alias_to_bindings([], Bs, _) ->
    Bs.



constraints_bindings([C|Cs], Bs0) when record(C, constraint) ->
    Bs = mnemosyne_unify:add_bind_trigger(mnemosyne_unify:variables(C),
					  {mnemosyne_constraint,check,[C]},
					  Bs0),
    constraints_bindings(Cs, Bs);
constraints_bindings([F|Fs], Bs0) when record(F, fn) ->
    constraints_bindings(Fs, Bs0);
%    Bs = mnemosyne_unify:add_bind_trigger(
%	   mnemosyne_unify:variables(F),
%	   {mnemosyne_constraint, apply_fn, [F#fn.fundef,F#fn.alias_var]},
%	   Bs0),
%    constraints_bindings(Fs, Bs);
constraints_bindings([], Bs) ->
    Bs.

%%%----------------------------------------------------------------
%% remove_obsolete_subgoals(Disj, WantedVariables) ->
%%     Res = 
%% 	remove_obsolete_subgoals(Disj, WantedVariables, ordsets:new(), []),
%%     ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Res)]),
%%     Res.

%% remove_obsolete_subgoals([Conj|Conjs], WantedVariables, Dset, Acc) 
%% 						when record(Conj,disj_alt) ->
%%     {C,Set} = remove_obsolete_subgoals1(Conj#disj_alt.conj),
%%     Sign = mk_sign(Conj#disj_alt.bs, 
%% 		   ordsets:union(mnemosyne_unify:variables(C),WantedVariables),
%% 		   Set),
%%     case Conj#disj_alt.fncalls of
%% 	[] ->
%% 	    case ordsets:is_element(Sign,Dset) of
%% 		true ->
%% 		    remove_obsolete_subgoals(Conjs, WantedVariables, Dset, Acc);
%% 		false ->
%% 		    remove_obsolete_subgoals(Conjs, WantedVariables, 
%% 					     ordsets:add_element(Sign,Dset),
%% 					     [Conj#disj_alt{conj=C,sign=Sign}|Acc])
%% 	    end;
%% 	_ ->
%% 	    remove_obsolete_subgoals(Conjs, WantedVariables, 
%% 					     ordsets:add_element(Sign,Dset),
%% 				     [Conj#disj_alt{conj=C,sign=Sign}|Acc])
%%     end;
%% remove_obsolete_subgoals([], WantedVariables, Dset, Acc) ->
%%     remove_parts(Acc).


%% remove_obsolete_subgoals1(Goals) ->
%%     remove_obsolete_subgoals1(Goals, ordsets:new(), []).

%% remove_obsolete_subgoals1([Goal|Goals], Set, Acc) when record(Goal,pred_sym) ->
%%     test_obsolete_goon({Goal#pred_sym.functor,
%% 			Goal#pred_sym.module,
%% 			Goal#pred_sym.args},
%% 		       Goal, Goals, Set, Acc);

%% remove_obsolete_subgoals1([{'#not',C,Gs0}|Goals], Set, Acc) ->
%%     {Gs,G_set} = remove_obsolete_subgoals1(Gs0),
%%     test_obsolete_goon({'#not',C, G_set}, {'#not',C,Gs}, Goals, Set, Acc);

%% remove_obsolete_subgoals1([Goal|Goals], Set, Acc) ->
%%     remove_obsolete_subgoals1(Goals, Set, [Goal|Acc]);

%% remove_obsolete_subgoals1([], Set, Acc) ->
%%     {Acc, Set}.



%%%----
%% test_obsolete_goon(Key, Goal, Goals, Set, Acc) ->
%%     case ordsets:is_element(Key,Set) of
%% 	true ->					% Already there, remove
%% 	    remove_obsolete_subgoals1(Goals, Set, Acc);
%% 	false ->
%% 	    remove_obsolete_subgoals1(Goals, ordsets:add_element(Key,Set),
%% 				      [Goal|Acc])
%%     end.


%%%----
%% mk_sign(Bs, Vars, Set) -> 
%%     mk_sign1(mnemosyne_unify:bindings_to_list(Bs), Vars, Set).

%% mk_sign1([B|Bs], Vars, Set) -> 
%%     case ordsets:is_element(element(1,B),Vars) of
%% 	true ->
%% 	    mk_sign1(Bs, Vars, add_sign(B,Set));
%% 	false ->
%% 	    mk_sign1(Bs, Vars, Set)
%%     end;
%% mk_sign1([], Vars, Set) -> 
%%     Set.

%% add_sign({V, {'#bind_trigger',L}}, Set) ->
%%     add_signL(V, L, Set);
%% add_sign(B, Set) ->
%%     ordsets:add_element({'#binding',B},Set).

%% add_signL(V, [T|Ts], Set) ->
%%     add_signL(V, Ts, 
%% 	      ordsets:add_element({'#binding',{V,{'#bind_trigger',T}}}, Set));
%% add_signL(V, [], Set) ->
%%     Set.

%%%----
%% remove_parts(Conjs) -> 
%%     lists:map(fun(C) ->
%% 		      C#disj_alt{sign=[]}
%% 	      end, Conjs).  %%%% arg to map was: remove_parts(Conjs, [])).

%% remove_parts([Conj|Post], Pre) ->
%%     NewPre = test_more_general(Conj, Pre),
%%     NewPost = test_more_general(Conj, Post),
%%     remove_parts(NewPost, [Conj|NewPre]);
%% remove_parts([], Pre) ->
%%     Pre.

%% test_more_general(Conj, [C|Cs]) ->
%%     case ordsets:is_subset(Conj#disj_alt.sign, C#disj_alt.sign) of
%% 	true -> %% C is more restricted, remove it
%% 	    test_more_general(Conj, Cs);
%% 	false -> %% Keep C
%% 	    [C | test_more_general(Conj, Cs)]
%%     end;
%% test_more_general(Conj, []) ->
%%     [].

%%%----------------------------------------------------------------
%% Now check if the query could be run as a mnesia:match_object operation or if
%% the full query evaluator must be started.

how_to_eval(Z) -> 
    %% First run the rest of the optimization (excl. reordering) just to
    %% see where we we will end
    Z1 = phase2_no_statistics(Z),
    how_to_eval(Z1#optimizer_result.code, Z1#optimizer_result.pattern, Z1, Z).
    

how_to_eval([D], Pattern, Z1, Z) when record(D,disj_alt) ->
    %% Promising, just one alternative in a disjunction. Check that one.
    how_to_eval(D#disj_alt.conj, Pattern, Z1, Z);

how_to_eval([P0], Pattern, Z1, Z) when record(P0,pred_sym),
				       P0#pred_sym.type==table ->
    %% Yes! Just one db-access in the query. Use mnesia:match_object instead.
    P = mnemosyne_exec:mk_patterns(P0),
    ?debugmsg(1,
	      "Use mnesia:match_object(~w) and return ~w. Back-pattern=~w\n", 
	      [P#pred_sym.pattern, Pattern, P#pred_sym.back_pattern]),
    Z1#optimizer_result{code = [P], how_to_eval = mnesia_match_build};

how_to_eval(_, _, _, Z) ->
    %% Hurray!!! Can't use basic mnesia-functions!! 
    Z#optimizer_result{how_to_eval = query_eval}.

%%%----------------------------------------------------------------

test_range_restricted(_,_) ->
    ?not_yet(remove_obsolete_clauses),
    ok.
%% test_range_restricted(Pattern, [C|Cs]) ->
%%     BoundPattern = mnemosyne_unify:instantiate(Pattern, C#disj_alt.bs),
%%     case catch test_range_restricted(BoundPattern, C) of
%% 	{error, {Line,Mod,{not_range_restricted,Vars}}} ->
%% 	    throw({error,
%% 		   {0,?MODULE, {not_range_restricted,Vars,C#disj_alt.conj}}});
%% 	{error, Others} ->
%% 	    throw({error,Others});
%% 	Others ->
%% 	    Others
%%     end,
%%     test_range_restricted(Pattern, Cs);
%% test_range_restricted(_, []) ->
%%     ok.


%%%----------------------------------------------------------------
remove_obsolete_clauses(Disj0) ->
    Disj = check_constraints(Disj0, []),
    ?not_yet(remove_obsolete_clauses),
    %% ...
    if  Disj==[] -> 
	    throw(fail);
	true -> 
	    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Disj)]),
	    Disj
    end.

check_constraints([C|Cs], Acc) when record(C,disj_alt) ->
    case catch mnemosyne_unify:check_triggers(C#disj_alt.bs) of
	fail ->
	    check_constraints(Cs, Acc);
	NewBs ->
	    check_constraints(Cs, [C#disj_alt{bs=NewBs}|Acc])
    end;
check_constraints([], Acc) ->
    Acc.


%%%----------------------------------------------------------------
extract_common_bindings(Disj) when length(Disj)>0 ->
    case extract(tl(Disj), disj_bs_to_set(hd(Disj))) of
	[] -> 
	    {[], Disj};
	Bs_Common ->
	    ?debugmsg(3, "Common bs : ~s\n", 
		      [mnemosyne_pp:e({'#bindings', 1,
				       mnemosyne_unify:list_to_bindings(Bs_Common)})]),
	    remove_common_set(Disj, Bs_Common, [])
    end;
extract_common_bindings([]) ->
    {[],[]}.


extract([C|Cs], Set) when record(C,disj_alt) ->
    extract(Cs, ordsets:intersection(disj_bs_to_set(C),Set));
extract([], Set) ->
    Set.

remove_common_set([C|Cs], Set, Acc) ->
    Bs = disj_bs_to_set(C),
    NewBs = ordsets:subtract(Bs,Set),
    remove_common_set(Cs, Set, 
		      [C#disj_alt{bs=mnemosyne_unify:list_to_bindings(NewBs)}
		       |Acc]);
remove_common_set([], Set, Acc) ->
    {mnemosyne_unify:list_to_bindings(Set), Acc}.


disj_bs_to_set(C) ->
    Bs = 
	case C of
	    {'#bindings',_,X} -> X;
	    _ when record(C,disj_alt) -> C#disj_alt.bs
	end,
    ordsets:from_list( mnemosyne_unify:bindings_to_list(Bs) ).

%%%----------------------------------------------------------------
subgoal_ordering(Opr) ->
    BoundVars = mnemosyne_unify:bs_vars_value(Opr#optimizer_result.common_bs),
    Opr#optimizer_result{code = ordering(Opr#optimizer_result.code,BoundVars)}.


ordering(Disjs, BoundVars) ->
    ordering(Disjs, BoundVars, 
	     fun(P,BV,N) -> mnemosyne_cost:selective_cost(P,BV) end,
	     []).


ordering([D|Ds], BoundVars0, CostFn, Acc) when record(D,disj_alt) ->
    BoundVars = ordsets:union(BoundVars0, 
			      mnemosyne_unify:bs_vars_value(D#disj_alt.bs)),
    {Gs1,Ntuples} = order_goals(D#disj_alt.conj, BoundVars, CostFn, [], 1),
    ordering(Ds, BoundVars, CostFn, [D#disj_alt{conj=Gs1,
						size_estimate=Ntuples}|Acc]);
ordering([], _, _, Acc) ->
    Acc.


order_goals([G0|Gs], BoundVars, CostFn, Acc, Ntup0) ->
    G1 = select_idx(G0, BoundVars),
    {G, Grest} = 
	cheapest(Gs, BoundVars, G1, CostFn(G1,BoundVars,Ntup0), 
		 CostFn, [], Ntup0),
    Ntup = Ntup0 * mnemosyne_cost:output_size(G, BoundVars),
    order_goals(Grest, newbind(G,BoundVars), CostFn, [G|Acc], Ntup);
order_goals([], _, _, Acc, Ntup) ->
    {lists:reverse(Acc), Ntup}.



cheapest([G0|Gs], BoundVars, Gmin, Cmin, CostFn, Acc, N) ->
    G = select_idx(G0, BoundVars),
    Cg = CostFn(G, BoundVars, N),
    if
	Cg<Cmin -> 
	    cheapest(Gs, newbind(G,BoundVars), G, Cg, CostFn, [Gmin|Acc], N);
	true -> 
	    cheapest(Gs, newbind(G,BoundVars), Gmin, Cmin, CostFn, [G|Acc], N)
    end;
cheapest([], BoundVars, Gmin, Cmin, CostFn, Acc, _) ->
    ?debugmsg(2, "Cost=~w Goal=~s\n", [Cmin,mnemosyne_pp:e(Gmin)]),
    {Gmin, Acc}.


select_idx(P, BoundVars) when record(P,pred_sym) ->
    case P#pred_sym.type of
	rule when P#pred_sym.recursive==recursive -> 
	    P;
	table -> 
	    mnemosyne_lib:assert_table_defined(P#pred_sym.functor,
					       P#pred_sym.line),
	    case bound_key_or_index(P,BoundVars) of
		none -> P;
		1 -> P#pred_sym{idx_method=key};
		I when integer(I) ->  P#pred_sym{idx_method={stat_idx,I}}
	    end
    end;
select_idx(P, BoundVars) ->
    P.

%% returns  Pos (integer) if bound index or key.
%%          'none' if neither
bound_key_or_index(P, BoundVars) ->
%%    Indices = mnesia_lib:val({P#pred_sym.functor,index}),
%%    Indices = mnesia_lib:readable_indecies(P#pred_sym.functor),
    Indices = mnesia:table_info(P#pred_sym.functor, index),
    [Tup] = P#pred_sym.args,
    select_ground_index_arg([2|Indices], Tup, BoundVars).


select_ground_index_arg([I|Is], Tup, BoundVars) ->
    FirstArgVars = mnemosyne_unify:variables_and_annonymous(element(I,Tup)),
    case ordsets:subtract(FirstArgVars, BoundVars) of
	[] -> I-1; %% arg number I is ground and indexed
	_ -> select_ground_index_arg(Is, Tup, BoundVars)
    end;
select_ground_index_arg([], _, _) ->
    none.


newbind(G, Vars) -> ordsets:union(mnemosyne_unify:variables(G), Vars).

%%%----------------------------------------------------------------
push_bindings(Opr) when record(Opr,optimizer_result) ->
    Disj = push_bindings(Opr#optimizer_result.code, []),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Disj)]),
    Opr#optimizer_result{code=Disj}.


push_bindings([C|Cs], Acc) ->
    Conj = push_bindings1(C#disj_alt.conj,
			  C#disj_alt.bs, 
			  mnemosyne_unify:bs_vars(C#disj_alt.bs)),
    push_bindings(Cs, [C#disj_alt{conj = Conj,bs = []}|Acc]);
push_bindings([], Acc) ->
    Acc.


push_bindings1(Gs, [], BsVars) ->
    Gs;
push_bindings1([G|Gs], Bs, BsVars) ->
    case ordsets:intersection(BsVars,mnemosyne_unify:variables(G)) of
	[] -> [G|push_bindings1(Gs,Bs,BsVars)];
	_ -> [{'#bindings',1,Bs},G|Gs]
    end;
push_bindings1([], Bs, BsVars) ->
    [{'#bindings',1,Bs}].

%%%----------------------------------------------------------------
push_function_calls(Opr) when record(Opr,optimizer_result) ->
    Disjs = 
	lists:map(fun(D) when record(D,disj_alt) ->
			  D#disj_alt{conj =  push_function_calls1(D),
				     fncalls = []}
		  end, 
		  Opr#optimizer_result.code),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Disjs)]),
    Opr#optimizer_result{code=Disjs}.


push_function_calls1(D) ->
    push_function_calls1(D#disj_alt.fncalls,
			 [],
			 D#disj_alt.conj,
			 mnemosyne_unify:bs_vars_value(D#disj_alt.bs)).



push_function_calls1([FnCall|FnCalls], FnRest, Gs, VarsWithValue) ->
    case ordsets:is_subset(mnemosyne_unify:variables(FnCall#fn.fndef), 
			VarsWithValue) of
	true -> %% All function arguments are bound, insert fn call
	    [FnCall |
	     push_function_calls1(FnCalls, FnRest, Gs,
				  ordsets:add_element(FnCall#fn.alias_var,
						      VarsWithValue))];
	false -> %% Wait a little
	    push_function_calls1(FnCalls, [FnCall|FnRest], Gs,VarsWithValue)
    end;

push_function_calls1([], [], Gs, VarsWithValue) ->
    Gs;

push_function_calls1([], FnRest, [G|Gs], VarsWithValue) ->
    [G | push_function_calls1(FnRest, [],
			      Gs,
			      ordsets:union(VarsWithValue,
					    mnemosyne_unify:variables(G)))];

push_function_calls1([], FnRest, [], VarsWithValue) ->
    FnRest.



%%%----------------------------------------------------------------

build_tree(Opr) ->
    Code = build_tree1(Opr#optimizer_result.code),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Code)]),
    Opr#optimizer_result{code=Code}.


build_tree1([{'#bindings',C,Bs}|DisjBs]) ->
    [{'#bindings',C,Bs} | build_tree1(DisjBs)];
build_tree1([C|Alts]) when record(C,disj_alt) -> 
    build_tree1(Alts, prepare(C));
build_tree1([[]|Alts]) ->
    build_tree1(Alts).


build_tree1([C|Alts], Tree) when record(C,disj_alt) -> 
    build_tree1(Alts,  ins(prepare(C),Tree));
build_tree1([], Tree) -> Tree.


prepare(C) when record(C,disj_alt) ->
    case  C#disj_alt.bs of
	[] -> C#disj_alt.conj;
	Bs -> [{'#bindings',1,Bs} | C#disj_alt.conj]
    end.


ins([X|Xs], [Y|Ys]) ->
    Xkey = ins_key(X),
    Ykey = ins_key(Y),
    case Y of
	{'#or',C, Alts} -> [{'#or',C,insa([X|Xs],Alts)}|Ys];	
	_  when Xkey==Ykey ->  [X | ins(Xs,Ys)];
	_  when Xkey < Ykey -> [{'#or',1,[[X|Xs],[Y|Ys]]}];
	_  -> [{'#or',1,[[Y|Ys],[X|Xs]]}]  %% when X > Y 
    end;
ins([], _) -> [];	%% true OR Y
ins(_, []) -> [].	%% X OR true


%%ins_key(X) when record(X,pred_sym) -> X#pred_sym.functor;
ins_key(X) -> X.


insa([X|Xs], [[X|As]|Alts]) -> [[X|ins(Xs,As)]|Alts];
insa([], Alts) -> Alts;
insa(L, [Alt|Alts]) -> [Alt|insa(L,Alts)];
insa(L, []) -> [L].

%%%----------------------------------------------------------------
adornments(Opr) when record(Opr,optimizer_result) ->
    WantedVars = mnemosyne_unify:variables(Opr#optimizer_result.pattern),
    CommonBs = Opr#optimizer_result.common_bs,
    Ncommonvars = 
	mnemosyne_unify:count_bs_vars(
	  CommonBs, 
	  mnemosyne_unify:count_variables(CommonBs)),
    Code = adornments_disj(Opr#optimizer_result.code, 
			   Ncommonvars,
			   WantedVars,
			   []),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Code)]),
    Opr#optimizer_result{code=Code}.


adornments_disj([D|Ds], Ncommonvars, WantedVars, Acc) when record(D,disj_alt)->
    S1 = mnemosyne_unify:count_variables(
	   {D#disj_alt.conj,
	    D#disj_alt.fncalls},
	   mnemosyne_unify:count_bs_vars(D#disj_alt.bs, Ncommonvars)),

    S2 = ordsets:from_list(mnemosyne_unify:sing_vars(S1, [])),
    SingVars = ordsets:subtract(S2, WantedVars),
    DefVars = ordsets:new(),
    adornments_disj(Ds, Ncommonvars, WantedVars, 
		    [D#disj_alt{conj=adornments(D#disj_alt.conj,
						DefVars,SingVars)}
		     | Acc]);
adornments_disj([], _, _, Acc) ->
    Acc.


adornments([G|Gs], DefVars, SingeltonVars) when record(G,pred_sym) ->
    Vars = ordsets:union(mnemosyne_unify:variables(G), DefVars),
    G_singeltons = ordsets:intersection(Vars,SingeltonVars),
    [G#pred_sym{defvars=DefVars, singelvars=G_singeltons} |
     adornments(Gs,Vars,SingeltonVars)];
adornments([{'#not',C,NGs}|Gs], DefVars, SingeltonVars) ->
    [{'#not', C, adornments(NGs,DefVars,SingeltonVars)} |
     adornments(Gs, DefVars, SingeltonVars)];
adornments([G|Gs], DefVars, SingeltonVars) ->
    [G | adornments(Gs, DefVars, SingeltonVars)];
adornments([], DefVars, SingeltonVars) ->
    [].

%%%----------------------------------------------------------------

