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
-module(mnemosyne_lc).

%% Purpose : A List Comprehension transformer of Mnemosyne

-export([parse_transform/2, format_error/1, the_query/1,
	 one_lc_to_handle/1]).

%%-export([abstract_keep_vars/2]).

%%-define(debug, 3).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").


%% This module transforms Mnemosyne things in the erlang source
%% module. Queries are transformed into function calls:
%%
%%           query [ P || B ] end
%%           
%%  --->     mnemosyne_lc:the_query({query,1,P',B',Data})
%%
%%           
%%  Rules are transformed as:
%%
%%           P(PV1) :- PB1;
%%           P(PV2) :- PB2;
%%		 ...
%%           P(PVn) :- PBn.
%%           
%%
%%           S(SV1) :- SB1;
%%           S(SV2) :- SB2;
%%		 ...
%%           S(SVm) :- SBm.
%%           
%%  --->     'MNEMOSYNE RULE'(P) -> [{PV1', PB1'},
%%				     {PV2', PB2'},
%%				       ...
%%				     {PVn', PBn'}];
%%
%%           'MNEMOSYNE RULE'(S) -> [{SV1', SB1'},
%%				     {SV2', SB2'},
%%				       ...
%%				     {SVm', SBm'}].
%%




-record(s, {options = [],
	    recdefs = [],
	    argtypes = [],
	    module = ?NO_MODULE,
	    line = none,
	    erl_vars = ordsets:new(),
	    db_vars = ordsets:new(),
	    allow_erl_vars = false,
	    allow_db_vars = true,
	    allow_fun_calls = false,
	    rules = [],
	    recdeffun = [],
	    var_types= [],
	    mquery = [],
	    mquery_next = 0
	    }).

-record(rec_def, {name, fields=[]}).  %% Field <- {name, init}

format_error(Msg) ->
    lists:concat(
      case Msg of
	  mnesia_not_running ->
	      ["Mnesia is not running"];

	  {no_erl_var,Name} ->
	      ["no Erlang variable (",Name,") at this place in a database "
	       "list comprehension"];

%	  {unbound_var,Name} ->
%	      ["variable '",Name,"' in list comprehension is unbound"];
	  
	  {illegal_lc_generator,table,too_few_arguments} ->
	      ["Table needs at least one argument."];

	  {illegal_lc_generator,table,too_many_arguments} ->
	      ["Table cannot have more than one argument."];

	  {illegal_lc_generator, table, ArgNo, Arg} ->
	      ["Table can only have atom or variable as ", ArgNo, "argument"] 
		  ++ case lists:flatten(erl_pp:expr(Arg, 
						    fun(_,_,_,_) -> "" end)) of
			 "" ->
			     ["."];
			 Str ->
			     [", not ", Str, "."]
		     end;
	  
	       
	  {illegal_lc_generator, Side, X} ->
	      case lists:flatten(erl_pp:expr(X, fun(_,_,_,_) -> "" end)) of
		  "" ->
		      ["illegal ",Side,
		       " side in database list comprehension generator"];
		  Str ->
		      ["illegal ",Side,
		       " side in database list comprehension generator: ",
		      Str]
	      end;
	  
	  {illegal_lc_body,X} ->
	      case lists:flatten(erl_pp:expr(X, fun(_,_,_,_) -> "" end)) of
		  "" ->
		      ["illegal body in database list comprehension"];
		  Str ->
		      ["illegal body \"",
		       Str, "\" in database list comprehension"]
	      end;
	  
	  {illegal_expr,X} ->
	      case lists:flatten(erl_pp:expr(X, fun(_,_,_,_) -> "" end)) of
		  "" ->
		      ["illegal expression in database list comprehension"];
		  Str ->
		      ["illegal expression \"", Str,
		       "\" in database list comprehension"]
	      end;
	  
	  {illegal_op,Op} ->
	      ["illegal operation (",Op,") in database list comprehension"];
	  
	  {illegal_field,Name} ->
	      ["illegal database record field value for \"",Name, "\""];

	  {undefined_field,RecType,Field} ->
	      ["undefined database record field \"",Field,
	       "\" in record \"", RecType, "\""];
	  
	  {illegal_record_init,Name,Field} ->
	      ["database record \"",Name,
	       "\" has illegal initialization of the field \"", Field, "\""];
	  
	  {illegal_rule_arity,Name,Arity} ->
	      ["the rule \"",Name,"\" has arity =/= 1"];

	  {already_defined,What,Name} ->
	      ["the ", What," \"", Name, "\" is already defined"];

	  {undefined,What,Name} ->
	      ["the ", What," \"", Name, "\" is undefined"];


	  {type_error, VName, Type1, Type2} ->
	      ["The variable ", VName, " cannot be both ", Type1,
	       " and ", Type2, "."];

	  {illegal_rule_clause_header, Args} ->
	      ["Illegal clause header of rule"]
		  ++ case lists:flatten(erl_pp:expr(Args, fun(_,_,_,_) -> "" end)) of
			 "" ->
			     ["."];
			 Str ->
			     [": ", Str, "."]
		     end;

	  {rule_type_error, VName, Type1, Type2} ->
	      ["The rule \"", VName, "\" cannot return both \"", Type1,
	       "\" and \"", Type2, "\"."];

	  {rule_arity_error, Name, A1, A2} ->
	      ["The rule \"", Name, "\" cannot have arity ", 
	       A1, " and ", A2, "."];

	  {variable_types_dont_match, V1, V2} ->
	      ["Variables ", V1, " and ", V2, " have different types"];

	  Other ->
	      [io_lib:write(Other)]
      end).


parse_transform(Forms, Options) ->
    case catch pass2( pass1(Forms,Options) ) of
	{error, E} -> [{error, E}];
	R -> R
    end.



%%%----------------------------------------------------------------
one_lc_to_handle([Expr]) ->
    TabAttrs =
	lists:map(
	  fun(Tab) ->
		  case catch mnesia:table_info(Tab, attributes) of
		      {'EXIT',{aborted,no_exists}} ->
			  throw({error, atom_to_list(Tab)++" does not exist"});
		      Attrs when list(Attrs) ->
			  {Tab,Attrs}
		  end
	  end, find_tables(Expr)),

    FakedFunction = {function,0,hale_bopp,0,[{clause,0,[],[],[Expr]}]},
    FakedModule = mk_rec_defs(TabAttrs) ++ [FakedFunction, {eof,0}] ,
    TransformedFakedModule = 
	parse_transform(FakedModule, [report_errors,report_warnings]),
    
    the_query(
      hd(lists:foldl(
	   fun({function,0,hale_bopp,0,
		[{clause,0,[],[],
		  [{call,1,{remote,1,{atom,1,mnemosyne_lc},{atom,1,the_query}},
		    [X]}]}]}, Acc) -> [erl_parse:normalise(X)|Acc];
	      (_, Acc) ->
		   Acc
	   end, [], TransformedFakedModule))).



mk_rec_defs(TabAttrs) ->
    lists:map(fun({Tab,Attrs}) ->
		      {attribute,0,record,
		       {Tab, lists:map(fun(Attr) ->
					       {record_field,0,{atom,0,Attr}}
				       end, Attrs)}}
	      end, TabAttrs).

find_tables(E) -> find_tables(E,[]).


find_tables({generate,_,{var,_,_},{call,_,{atom,_,table},[{atom,_,Table}]}},
	    Acc) ->  [Table | Acc];
find_tables({generate,_,{var,_,_},_}, Acc) ->
    throw({error, "Illegal generator"});
find_tables(T, Acc) when tuple(T) ->  find_tables(tuple_to_list(T), Acc);
find_tables([H|T], Acc) ->  find_tables(T, find_tables(H,Acc));
find_tables(_, Acc) ->   Acc.
    




%%%----------------------------------------------------------------
%%%----------------
pass1(Forms, Options) ->
    lists:foldl(
      fun
	  ({attribute,Line,record,{Name,Defs0}}, {Acc,S}) ->
	      case catch record_defs(Defs0, Name) of
		  {error, E} ->
		      {[{error,E}|Acc], S};
		  {'EXIT', Cause} ->
		      throw({'EXIT', Cause});
		  {ok, Def} ->
		      {[{attribute,Line,record,{Name,Defs0}}|Acc], 
		       S#s{recdefs=[Def|S#s.recdefs],
			   recdeffun=add_rec_clause(S#s.recdeffun, Line, 
						    Def)}}
	      end;
      
	  ({attribute,Line,module,Name}, {Acc,S}) ->
	      {[{attribute,Line,module,Name}|Acc], S#s{module=Name}};
      
	  ({attribute,Line,argtype,{RuleName,RecordName}}, {Acc,S}) ->
	      case lists:keysearch(RuleName,1,S#s.argtypes) of
		  {value, _} ->
		      {[{error, 
			 {Line,?MODULE,
			  {already_defined,"argument type of",RuleName}}} |
			Acc], S};
		  false ->
		      {Acc, S#s{argtypes=[{RuleName,RecordName}|S#s.argtypes]}}
	      end;
      
	  ({rule,Line,Name,Arity,Clauses}, {Acc,S}) when Arity=<2, Arity > 0 ->
	      case lists:keysearch(Name,1,S#s.rules) of
		  {value,_} ->
		      {[{error, {Line,?MODULE,{already_defined,rule,Name}}} |
			Acc], S};
		  false -> 
		      {Acc, S#s{rules=[{Name,Line,Clauses}|S#s.rules]}}
	      end;
      
	  ({rule,Line,Name,Arity,_}, {Acc,S})  ->
	      {[{error, 
		 {Line,?MODULE,{illegal_rule_arity,Name,Arity}}}|Acc], S};
      
	  (F, {Acc,S}) ->
	      {[F|Acc], S}
	       
      end, {[],#s{options=Options}}, Forms).


%% This adds one case to the set of clauses in a case-set.
add_rec_clause (Clauses, Line, {Name, Names}) ->
    Clauses ++ [{clause, Line,
		 [{atom, Line, Name}],           %% pattern
		 [],                             %% no guards
		 [{record, Line, Name, []}]}].   %% action: empty record #Name


pass2({[{eof,EOFline}|Forms],S0}) -> %% Forms are the forms in reverse order
    S1 = S0#s{mquery = mk_initial_mquery ()},
    {RulePart, S3} =
	case mk_rules_def_fkn(S1) of
	    {ok, FnDefL, S2} ->
		{FnDefL, S2};
	    {error, ErrLst} ->
		{ErrLst, S1#s{rules=nope}} %% prevent export decl of 
		                           %% 'MNEMOSYNE RULE' if needed
	end,

    {RecFunDefList, S4} =
	case S3#s.recdeffun of
	    [] ->
		{[], S3};
	    RecList ->
		{make_recdeffun (RecList), S3#s{recdeffun=[]}}
	end,
    

    %%    Following two cases prepare needed export declarations 
    Export1 = [{'MNEMOSYNE RULE',1}, {'MNEMOSYNE QUERY', 2}],
    Export =
	case RecFunDefList of
	    [] ->
		Export1;
	    _ ->
		[{'MNEMOSYNE RECFUNDEF',1}] ++ Export1
	end,
    
    {NewForms,S5} = 
	lists:foldl(    
	  fun
	      ({attribute,Line,'export',L}, {Acc,S}) when S#s.rules == true -> 
		  {[{attribute,Line,'export', Export ++ L}
		    | Acc],
		   S#s{rules=false}}; %% prevent further export decls.
			  
	      ({function,Line,Name,Arity,Clauses}, {Acc,S}) -> 
		  {FunDef, S6} =
		      case catch fn_clauses(Clauses,S) of
			  {error, E} ->
			      {{error,E}, S};
			  {'EXIT',Cause} ->
			      throw({'EXIT',Cause});
			  {TClauses, S5} ->
			      {{function,Line,Name,Arity,TClauses}, S5}
		      end,
		  {[FunDef | Acc], S6};

	      (F, {Acc,S}) ->
		  {[F|Acc], S}
	  end, {[],S4#s{rules=true}}, Forms),

    NewForms ++ RecFunDefList ++ S5#s.mquery ++ RulePart ++ [{eof,EOFline}];

pass2({L, S}) ->
    L.

make_recdeffun (RecFunDefList) ->
    [{function, 0, 'MNEMOSYNE RECFUNDEF', 1,     %% name and arity line=0
      [{clause, 0, 
	[{var, 0, 'X'}],                         %% clause takes var X
	[],                                      %% no guards
	[{'case', 0, {var, 0, 'X'},              %% 'case X of'
	  RecFunDefList}]}]}].                   %% our list of cases


%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *parser*!

record_defs(Defs, Name) ->
    R = record_defs(Defs, Name, []),
    ?debugmsg(3, "RecDef = ~w\n", [R]),
    {ok,R}.

record_defs([{record_field,Line,{atom,La,A},Val0}|Defs], Name, Fs) ->
%    case valid_record_def_inits(Val0) of
%	true ->
	    record_defs(Defs, Name, [A|Fs]);
%	false ->
%	    throw({error, {La,?MODULE,{illegal_record_init,Name,A}}})
%    end;

record_defs([{record_field,Line,{atom,La,A}}|Defs], Name, Fs) ->
    record_defs(Defs, Name, [A|Fs]);
record_defs([], Name, Fs) ->
    {Name, lists:reverse(Fs)}.


%%%----------------------------------------------------------------
mk_rules_def_fkn(S) ->
    {Rules0, S1} = lists:foldl(fun one_rule/2, {[], S}, S#s.rules),
    case mnemosyne_compiler:pass2_rules(Rules0, 
					S1#s.module, 
					S1#s.recdefs) of
	{ok, CompiledRules} ->
	    {ok, [{function,0,'MNEMOSYNE RULE',1,
		   mk_fn_clauses(CompiledRules)++mk_data_clauses(S1)}],
	     S1};

	{error, L} ->
	    {error, L}
    end.


one_rule({Name,Line,Clauses}, {Accu0, S0}) ->
    {Arity, Rettype, S1, Clauses1} = rule_check_arity_type (Name, Clauses, S0),
    lists:foldl(
      fun({clause,CLine,[Arg0],[],LCBody0}, {Accu,S}) ->
	      S2 = S#s{db_vars=db_vars(LCBody0), var_types=[]},
	      {Body0, S3} = tr_body(LCBody0, S2),
	      {Arg,Body} =
		  case tr_pattern(Arg0,S3) of
		      {'#var',V} ->
			  {{'#var',V}, Body0};
		      NonVar ->
			  NewArg = mnemosyne_lib:unique_var(rule_arg),
			  {NewArg, [#constraint{exprL = NewArg, 
						op    = '=', 
						exprR = NonVar, 
						line  = CLine} | Body0]}
		  end,
	      OrigVars = mnemosyne_unify:variables_and_annonymous(Arg),
	      Head = #pred_sym{module       = S#s.module,
			       functor      = Name,
			       line	    = CLine,
			       type	    = rule,
			       record_def   = arg_record_def(rule,Name,S3),
			       record_type  = Rettype,
			       args	    = [Arg],
			       original_args_vars = OrigVars
			      },
	      {[{rule,Head,Body} | Accu], S3}
      end, {Accu0, S1}, Clauses1).


rule_check_arity_type (Name, [H], S) ->
    {A,T,S1,H1} = rule_check_arity_type1 (Name, H, S),
    {A,T,S1, [H1]};
rule_check_arity_type (Name, [H|T], S) ->
    {Arity1, Type1, S1, H1} = rule_check_arity_type1 (Name, H, S),
    {Arity2, Type2, S2, T1} = rule_check_arity_type  (Name, T, S1),
    case {Arity1, Type1} of
	{Arity2, Type2} ->
	    {Arity1, Type2, S2, [H1 | T1]};
	{Arity2, X} ->
	    throw({error, {S#s.line, ?MODULE, 
			   {rule_type_error, Name, Type1, Type2}}});
	_ ->
	    throw({error, {S#s.line, ?MODULE, 
			   {rule_arity_error, Name, Arity1, Arity2}}})
    end.


rule_check_arity_type1 (Name, {clause,L, [A0,{atom,L2,Type}], [], Body}, S) ->
    S1 = add_argtype (Name, Type, S, L),
    {2, Type, S1, {clause, L, [A0], [], Body}}
;
rule_check_arity_type1 (Name, {clause, L, [A0], [], Body}, S) ->
    case lists:keysearch (Name, 1, S#s.recdefs) of
	{value, _} ->
	    S1 = add_argtype (Name, Name, S, L),
	    {1, Name, S1, {clause, L, [A0], [], Body}};
	_ ->
	    case lists:keysearch (Name, 1, S#s.argtypes) of
		{value, {_, Type}} ->
		    {1, Type, S, {clause, L, [A0], [], Body}};
		_ ->
		    throw ({error, {S#s.line, ?MODULE, 
				    {undefined, record, Name}}})
	    end
	end;
rule_check_arity_type1 (Name, {clause, L, Args, _, _}, S) ->
    throw ({error, {L, ?MODULE, {illegal_rule_clause_header, Args}}}).

add_argtype (Name, Type, S, L) ->
    case lists:keysearch (Name, 1, S#s.argtypes) of
	{value, {_, Type}} ->
	    S;
	{value, {_, OtherType}} ->
	    throw ({error, {L, ?MODULE, 
			    {rule_type_error, Name, Type, OtherType}}});
	false ->
	    S#s{argtypes=[{Name, Type} | S#s.argtypes]}
    end.

arg_record_def(What, Name, S) when record(S,s) ->
    arg_record_def(What, Name, Name, S#s.argtypes, S#s.recdefs, S#s.line).

arg_record_def(rule, Name, Type, ArgTypes, RecDefs, Line) ->
    arg_record_def(table,
		   Type,
		   case lists:keysearch(Name,1,ArgTypes) of
		       {value,{Name,R_name}} -> R_name;
		       false -> Name
		   end, ArgTypes, RecDefs, Line);

arg_record_def(table, Name, Type, ArgTypes, RecDefs, Line) ->
    arg_record_def(get_def, Name, Type, ArgTypes, RecDefs, Line);

arg_record_def(get_def, Name, Type, ArgTypes, RecDefs, Line) ->
    case lists:keysearch(Type,1,RecDefs) of
	{value,Def} -> Def;
	false -> throw({error, {Line,?MODULE,{undefined,record,Type}}})
    end.



mk_data_clauses(S) ->
    lists:map(fun({Name,Val}) ->
		      {clause, 
		       0, 
		       [abstract_keep_vars({data,Name})], 
		       [], 
		       [abstract_keep_vars(Val)]}
	      end, [{record_defs,  S#s.recdefs},
		    {argtypes, S#s.argtypes}]).



mk_fn_clauses(Rs) -> mk_fn_clauses(Rs, '', [], []).


mk_fn_clauses([{rule,Head,Body}|Rs], Doing, Acc, BigAcc) 
					when Head#pred_sym.functor == Doing ->
    mk_fn_clauses(Rs, Doing, [{rule,Head,Body}|Acc], BigAcc);

mk_fn_clauses([{rule,Head,Body}|Rs], _, Acc, BigAcc) -> %% New functor
    mk_fn_clauses([{rule,Head,Body}|Rs], Head#pred_sym.functor, [], 
	       new_bigacc(Acc,BigAcc));

mk_fn_clauses([], _, Acc, BigAcc) ->
    new_bigacc(Acc,BigAcc).
    


new_bigacc([], BigAcc) -> 
    BigAcc;

new_bigacc(Acc,BigAcc) ->
    [{rule,Head,_}|_] = Acc,
    [{clause, 
      Head#pred_sym.line, 
      [abstract_keep_vars(Head#pred_sym.functor)], 
      [],
      [abstract_keep_vars(Acc)]} 
     | BigAcc].

%%%----------------------------------------------------------------
fn_clauses(Cs, S) ->
    lists:mapfoldl(fun({clause,Line,H,G,B0}, S1) ->
			   {Form, S2} = 
			       replace_lcs(B0,S1#s{line=Line, 
						   erl_vars=vars(H)}),
			   {{clause,Line,H,G,Form}, S2}
		   end, S, Cs).

the_query(Data) ->
    case catch the_query1(Data) of
	{'EXIT', Cause} ->
	    case Cause of
		{undef,{Module,'MNEMOSYNE RULE',[_]}} ->
		    throw({'EXIT', {mnemosyne, 
				    {no_rules_in_module,Module}}});
		{undef,[{Module,'MNEMOSYNE RULE',[_]}|_]} ->
		    throw({'EXIT', {mnemosyne, 
				    {no_rules_in_module,Module}}});

		{function_clause,{Module,'MNEMOSYNE RULE',[RuleName]}} ->
		    throw({'EXIT', {mnemosyne, 
				    {no_such_rule,Module,RuleName}}});

		{function_clause,[{Module,'MNEMOSYNE RULE',[RuleName]}|_]} ->
		    throw({'EXIT', {mnemosyne, 
				    {no_such_rule,Module,RuleName}}});

		Others ->
		    throw({'EXIT',Others})
	    end;
	Others ->
	    Others
    end.

the_query1({Pattern,Goal0,S}) ->
    ?debugmsg(2, 
	      "Calling mnemosyne_compiler:lc_query1 line ~w\n"
	      "   Pattern : ~s\n   Goal    : ~s\n", 
	      [S#s.line,
	       mnemosyne_pp:e(Pattern),
	       mnemosyne_pp:body(Goal0)]),
    Goal = runtime_resolve_tables(Goal0, S),
    Call = mnemosyne_compiler:lc_query1(Pattern,Goal,S#s.recdefs,S#s.module),
    ?debugmsg(1, "Call line ~w = ~s\n", [S#s.line,mnemosyne_pp:body(Call)]),
    Opt = mnemosyne_optimizer:phase2(Call),
    ?debugmsg(1, "Optimized call: ~s\n", [mnemosyne_pp:body(Opt)]),
    {call,Call,Opt}.


vars(X) -> vars(X,ordsets:new()).

vars({var,_,V}, S) -> ordsets:add_element(V,S);
vars({cons,_,H,T}, S) -> vars(T, vars(H,S));
vars({tuple,_,As}, S) -> vars(As, S);
vars(L, S) when list(L) -> lists:foldr(fun vars/2, S, L);
vars(T, S) when tuple(T) -> vars(tuple_to_list(T), S);
vars(_, S) -> S.


runtime_resolve_tables(Goal, S) ->
    lists:map(
      fun
	  (P) when record(P,pred_sym), P#pred_sym.record_def == ?UNKNOWN ->
	      P#pred_sym{record_def = 
			 arg_record_def(P#pred_sym.type,
					P#pred_sym.functor,
					P#pred_sym.record_type,
					mnemosyne_lib:db_data(
					  argtypes,  P#pred_sym.module),
					mnemosyne_lib:db_data(
					  record_defs, P#pred_sym.module),
					P#pred_sym.line)};
	  (Other) ->
	      Other
      end, Goal).




replace_lcs({'query',_,{lc,Line,E0,Qs0}}, S0) ->
    %% dynamically compiled query
    DbVars = db_vars(Qs0),
    S = S0#s{erl_vars = ordsets:subtract(S0#s.erl_vars, DbVars),
	     db_vars = DbVars,
	     var_types = []},
    Pattern0 = tr_pattern(E0, S#s{line=Line}),
    {Goal0, S1} = tr_body(Qs0, S#s{line=Line}),
    MkGetRecDefs = fun() ->
			   {call,0,{atom,0,'MNEMOSYNE RULE'},
			    [erl_parse:abstract({data,record_defs})]}
		   end,
    S_temp = S#s{mquery=[],options=[],recdefs=MkGetRecDefs},
    {mk_call(?MODULE, the_query, [{Pattern0,Goal0,S_temp}], Line), S1};

replace_lcs({var,L,V}, S) ->
    {{var,L,V}, S#s{erl_vars=ordsets:add_element(V,S#s.erl_vars)}};

replace_lcs([H|T], S0) -> %% mapfoldl doesn't handle non-proper lists
    {H1,S1} = replace_lcs(H, S0),
    {T1,S2} = replace_lcs(T, S1),
    {[H1|T1], S2};

replace_lcs(T, S0) when tuple(T) -> 
    {F, S} = replace_lcs(tuple_to_list(T), S0),
    {list_to_tuple(F), S};

replace_lcs(F, S) ->
    {F, S}.

mk_call(Mod, Fun, Args, Line) ->
    Call = {call,Line,{remote,Line,
		       {atom,Line,Mod},
		       {atom,Line,Fun}},
	    lists:map(fun abstract_keep_vars/1, Args)},
    ?debugmsg(3, "mk_call = ~p\n", [Call]),
    Call.
    


abstract_keep_vars(X) ->
    element(1, abstract_keep_vars(X, {[],0})).


abstract_keep_vars({'#erl', X}, L) -> 
    {X, L};

abstract_keep_vars(Fun, L) when function(Fun) ->
    {Fun(), L};
    
abstract_keep_vars([H0|T0], L0) ->
    {H, L1} = abstract_keep_vars(H0, L0),
    {T, L} = abstract_keep_vars(T0, L1),
    {{cons,0,H,T}, L};

abstract_keep_vars(Tuple, L0) when tuple(Tuple) ->
    {T,L} = lists:mapfoldl(
	      fun(A,B)->
		      abstract_keep_vars(A,B) 
	      end, L0, tuple_to_list(Tuple)),
    {{tuple,0,T}, L};

abstract_keep_vars(R0, {L0,N0}) when reference(R0) ->
  case lists:keysearch(R0,1,L0) of
	{value,{R0,R}} ->
	    {erl_parse:abstract(R), {L0,N0}};
	false ->
	    {erl_parse:abstract(N0), {[{R0,N0}|L0], N0+1}}
    end;

abstract_keep_vars(X, L0) ->
    {erl_parse:abstract(X), L0}.



tr_body(L, S0) when list(L) ->
    {Body, NewS} = 
	lists:mapfoldl (
	  fun
	      %% DbVar <- Expr
	      ({generate,Line,Left, Right}, S) ->
		  generate_generator(Left, Right, S#s{line=Line});
	      
	      %% Expr Op Expr
	      ({op,Line,Op,Left,Right}, S) ->
		  generate_constraint(Left, Right, Op, S#s{line=Line});

	      %% Expr = Expr
	      ({match,Line,Left,Right}, S) ->
		  generate_constraint(Left, Right, '=', S#s{line=Line});
	      
	      %% Fn(Args)
	      ({call,Line,B,Args}, S) ->
		  generate_constraint({atom,Line,true}, 
				      {call,Line,B,Args},
				      '=',
				      S#s{line=Line});

	      (Others,S) ->
		  case is_compilable (Others, S) of
		      {true, Expr} ->
			  generate_general_constraint (Expr, S);
		      {false, _} ->
			  throw({error, {S#s.line,?MODULE,
					 {illegal_lc_body,Others}}})
		  % XXX
		  end
	  end, S0, L).


db_vars(L) ->
    lists:foldl(
      fun
	  ({generate,_,{var,_,V},_}, VarSet) -> ordsets:add_element(V, VarSet);
	  (_, VarSet) -> VarSet
      end, ordsets:new(), L).
      

generate_constraint(Left, Right, Op, S0) ->
    S = S0#s{allow_erl_vars=true,
	     allow_fun_calls=true},

    NewOp = 
	case Op of 
	    '=' -> '==';
	    _   -> Op 
	end,

    {IsComp, {L2,R2}} = is_compilable({Left, Right}, S),

    case {is_simple_expr (L2), is_simple_expr (R2)} of
	{true, true} ->
	    { #constraint{exprL = expr(L2,S),
			  exprR = expr(R2,S),
			  op    = constr_op(Op,S#s.line),
			  line  = S#s.line},
	      S};
	_ ->
	    case IsComp of
		true ->
		    generate_general_constraint ({op, S#s.line,
						  NewOp,
						  L2, R2}, S);
		false ->
		    {#constraint{exprL = expr(L2,S),
				 exprR = expr(R2,S),
				 op    = constr_op(Op,S#s.line),
				 line  = S#s.line},
			 S}
	    end
    end.

generate_general_constraint (Constr, S) ->
    Vars = all_vars (Constr, ordsets:new()),
    Vars2 = make_replacements (Vars),
    Code  = exchange_vars (Constr, Vars2),
    QueryNo = S#s.mquery_next,
    S1 = add_query (Code, Vars2, S),
    FunVars = lists:map (fun(X) -> {var, S#s.line, X} end, 
			 var_names(Vars)),
    {#constraint{exprL = expr({atom, S1#s.line, true}, S1),
		 exprR = expr ({call,
			       S1#s.line,
			       {atom, S1#s.line, 'MNEMOSYNE QUERY'},
			       [{integer, S1#s.line, QueryNo}, 
				{tuple, S1#s.line, 
				 FunVars}]},
			      S1#s{allow_erl_vars=true,
				   allow_fun_calls=true}),
		 op = '=',
		 line = S1#s.line},
     S1}.

is_simple_expr ({var, L, Var}) -> 
    true;
is_simple_expr ({record_field, Line,{var,L1,Var},Name,{atom,L3,Field}}) ->
    true;
is_simple_expr ({record_field, Line,{var,L1,Var},{atom,L3,Field}}) ->
    true;
is_simple_expr({integer,_,I}) -> 
    true;
is_simple_expr({float,_,F}) -> 
    true;
is_simple_expr({atom,_,A}) -> 
    true;
is_simple_expr({string,_,Str}) -> 
    true;
is_simple_expr (_) ->
    false.

    

generate_generator({var,VLL,VL}, {var,VRL,VR}, S) ->
    S1 = 
	case {lists:keysearch (VL, 1, S#s.var_types),
	      lists:keysearch (VR, 1, S#s.var_types)} of
	    {false, false} ->
		S;
	    {false, {value, {_, Type}}} ->
		S#s{var_types = [{VL, Type} | S#s.var_types]};
	    {{value, {_, Type}}, false} ->
		S#s{var_types = [{VR, Type} | S#s.var_types]};
	    {{value, {_, Type}}, {value, {_, Type}}} ->
		S;
	    _ ->
		throw({error, {S#s.line, S#s.module, 
			       {variable_types_dont_match, VR, VL}}})
	end,
    {#erl_expr{alias_var = expr_var({var,VLL,VL}, S#s{allow_erl_vars=true,
						      line=VLL}),
	       expr = expr_var({var,VRL,VR}, S#s{allow_erl_vars=true,
						 allow_db_vars=false,
						 line=VRL})},
     S1};


generate_generator({var,VLL,VL}, Right, S) ->
    Var = expr_var({var,VLL,VL}, S#s{line=VLL}),
    
    {IsCompilable, NewExpr} = is_compilable (Right, S),
    
    {Generator, S3} = 
    case Right of
	%% Var <- table(Name) 
	{call,_,{atom,_,table}, Args} ->
	    generate_table_generator (Var, VL, Args, S);

	%% Var <- rule(Name)  (i.e. locally defined rule)
	{call,_,{atom,_,rule}, [{atom,LineP,PredName}]}->
	    Args = [Var],
	    RecDef = arg_record_def(rule,PredName,S),
	    {#pred_sym{module = S#s.module,
		       functor = PredName,
		       line = LineP,
		       type = rule,
		       record_def = RecDef,
		       recursive = non_recursive, % Hypothesis
		       args = Args,
		       original_args_vars = 
		             mnemosyne_unify:variables_and_annonymous(Args)
		      },
	     add_var_type (VL, RecDef, S)};

	%% Var <- rule(Mod:Name)
	{call,_,{atom,_,rule}, [{remote,LineP,{atom,_,Mod},{atom,_,PredName}}]}->
	    Args = [Var],
	    RecDef = if 
			 Mod == S#s.module -> 
			     arg_record_def(rule,PredName,S);
			 true ->
			     ?UNKNOWN
		     end,
	    {#pred_sym{module = Mod,
		       functor = PredName,
		       line = LineP,
		       type = rule,
		       record_def = RecDef,
		       recursive = non_recursiv, % Hypothesis
		       args = Args,
		       original_args_vars = 
		       mnemosyne_unify:variables_and_annonymous(Args)
		      },
	     if 
		 RecDef == ?UNKNOWN -> S;
		 true -> add_var_type (VL, RecDef, S)
	     end
	    };

	%% Var <- rule(Others)  ** Illegal
	{call,Line,{atom,_,rule}, _}->
	    throw({error, {Line,?MODULE,
			   {illegal_lc_generator,right,Right}}});


	%% all other remaining cases: try to generate a function
	%% with the given code. put the current line as line numbers
	%% for the given code so compilation errors in the expression
	%% will be reported at correct place

	SomeExpr when IsCompilable == true ->
	    
	    Vars = all_vars (NewExpr, ordsets:new()),
	    Vars2 = make_replacements (Vars),
	    Code  = exchange_vars (NewExpr, Vars2),
	    QueryNo = S#s.mquery_next,
	    S1 = add_query (Code, Vars2, S),
	    FunVars = lists:map (fun(X) -> {var, S#s.line, X} end, 
				 var_names(Vars)),
	    {#erl_expr{alias_var = Var,
		       expr= expr ({call,
				    S1#s.line,
				    {atom, S1#s.line, 'MNEMOSYNE QUERY'},
				    [{integer, S1#s.line, QueryNo}, 
				     {tuple, S1#s.line, 
				       FunVars}]},
				   S1#s{allow_erl_vars=true,
					allow_fun_calls=true})},
	     S1};


	%% Var <- Name(Args)
	{call,FL,F,Args} ->
	    #erl_expr{alias_var = Var,
		      expr = expr(Right,
				  S#s{allow_erl_vars = true,
				      allow_fun_calls = true})};
	%% Var <- List
	{cons,Line,H,T} ->
	    #erl_expr{alias_var = Var,
		      expr = expr(Right,
				  S#s{allow_erl_vars = true,
				      allow_fun_calls=true})};
	X ->
	    throw({error, 
		   {S#s.line,?MODULE, {illegal_lc_generator,right,X}}})
    end,
    {Generator, S3};

generate_generator({op,NL,'not',{var,VLL,VL}}, Right, S) ->
    {G, S1} = generate_generator({var,VLL,VL}, Right, S#s{line=NL}),
    {{'#not', 1, [G]}, S1};

generate_generator(Left, _, S) ->
    throw({error, {S#s.line,?MODULE, {illegal_lc_generator,left,Left}}}).
    

generate_table_generator (Var, VL, Right, S) ->
    {NameField, TypeField} = 
	case length(Right) of
	    0 ->
		throw({error, 
		       {S#s.line,?MODULE, 
			{illegal_lc_generator,table,too_few_arguments}}});
	    1 ->
		{hd(Right), []};
	    2 ->
		{hd(Right), hd(tl(Right))};
	    _ ->
		throw({error, 
		       {S#s.line,?MODULE, 
			{illegal_lc_generator,table,too_many_arguments}}})
	end,
    
    Name = 
	case NameField of
	    {atom, VTL, Name1} ->
		Name1;
	    {var,VTL, VName} ->
		{'#erl', {var,VTL,VName}};
	    Other1 ->
		throw ({error, 
			{S#s.line,?MODULE, 
			 {illegal_lc_generator,
			  table, "first",
			  Other1}}})
	end,
    
    {RecDef, Type} = 
	case TypeField of
	    [] when atom(Name) ->			% one arg only
		{arg_record_def(table,Name,S), Name};
	    []  ->			                % one arg only
		{[], Name};
	    {atom, TL, TypeName} ->
		{arg_record_def(table,TypeName,S), TypeName};
	    {var, TL, TypeName} ->
		{[], {'#erl', {var, TL,TypeName}}};
	    Other2 ->
		throw ({error, 
			{S#s.line,?MODULE, 
			 {illegal_lc_generator,
			  table, "second",
			  Other2}}})
	end,

    Args = [Var],
    P = #pred_sym{module      = S#s.module,
		  functor     = Name,
		  line	      = S#s.line,
		  type	      = table,
		  record_type = Type,
		  recursive   = non_recursive,
		  args	      = Args,
		  original_args_vars = 
		  mnemosyne_unify:variables_and_annonymous(Args)
		 },
    case RecDef of
	[] ->
	    {P, S};
	_  ->
	    { P#pred_sym{record_def = RecDef},
	      add_var_type(VL, RecDef, S)}
    end.

%%% The <pattern> part in a list comprehension
tr_pattern(P, S) ->
    expr(P, S#s{allow_erl_vars=true,
		allow_fun_calls=false}).

%%% data without function calls like {a,X,[#r{},1]}. Not: NOT 1+4.

expr({record_field,Line,{var,L1,Var},Name,{atom,L3,Field}},
     S)->
    %% X#r.a
    case expr_var(L1,Var,S) of
	{'#erl', V} ->
	    {'#erl', 
	     {record_field,Line,V,Name,{atom,L3,Field}}};
	V ->
	    #rec_f{var = V,
		   name = Name,
		   line = Line,
		   field = Field}
    end;	    

expr({record_field,Line,{var,L1,Var},{atom,L2,Field}}, S) ->
    %% X.a
    case expr_var(L1,Var,S) of
	{'#erl', V} ->
	    {'#erl', {record_field,Line,V,{atom,L2,Field}}};

	V ->
	    #rec_f{var = V,
		   line = Line,
		   field = Field}
    end;	    

expr({record,Line,Name,Fields}, S) ->
    %% #r{a=..., ...}
    Fs = lists:map(fun({record_field,Lf,{atom,_,Nf},Vf}) ->
			   {Nf,Lf,expr(Vf,S#s{line=Lf})};
		       (Other) ->
			   throw({error,
				  {Line,?MODULE,{illegal_field,Name}}})
		   end, Fields),
    #rec_c{line=Line,
	   name=Name,
	   fields=Fs};

expr({var,Line,Name}, S) -> expr_var(Line, Name, S);
expr({integer,_,I}, S) -> I;
expr({float,_,F}, S) -> F;
expr({atom,_,A}, S) -> 
    case atom_to_list(A) of
	[$#|T] -> list_to_atom([$#,$#|T]);
	_ -> A
    end;
expr({string,_,Str}, S) -> Str;
expr({nil,_}, S) -> [];

expr({cons,Line,H0,T0}, S0) -> 
    S = S0#s{line=Line},
    [expr(H0,S) | expr(T0,S)];

expr({tuple,Line,Es0}, S0) -> 
    S = S0#s{line=Line},
    list_to_tuple( lists:map(fun (E) ->
				     expr(E,S)
			     end,
			     Es0) );
expr({call,Line,B,Args}, S0) when S0#s.allow_fun_calls==true ->
    Exp = {call,Line,B,Args},
    case B of
	{remote, _, M, F} -> f_call(M,F,Args,S0#s{line=Line});
	{atom,_,F} -> f_call({atom,0,S0#s.module},B,Args,S0#s{line=Line});
	{var,_,F} -> f_call({atom,0,S0#s.module},B,Args,S0#s{line=Line});
	_ ->
	    ?debugmsg(2, "B=~w\n", [B]),
	    throw({error, {S0#s.line,?MODULE,{illegal_expr,Exp}}})
    end;

expr(X, S) ->
    ?debugmsg(2, "~p\n~p\n", [X,S]),
    throw({error, {S#s.line,?MODULE,{illegal_expr,X}}}).



expr_var({var,Line,Name}, S) -> expr_var(Line,Name,S).

expr_var(Line, Name, S) ->
    case ordsets:is_element(Name,S#s.erl_vars) of
	true when S#s.allow_erl_vars==true ->
	    {'#erl',{var,Line,Name}};
	true when S#s.allow_erl_vars==false ->
	    throw({error, {Line,?MODULE,{no_erl_var,Name}}});
	false ->
	    {'#var',Name}
    end.


f_call(M, F, Args0, S0) ->
    C = {'#funcall', 
	 expr(M,S0),
	 expr(F,S0),
	 lists:map(fun (A) ->
			   expr(A, S0)
		   end, Args0)
	},
    ?debugmsg(2, "~p\n", [C]),
    C.



constr_op('=',_) -> '=';
constr_op('/=',_) -> '!=';
constr_op('<',_) -> '<';
constr_op('>',_) -> '>';
constr_op('=<',_) -> '=<';
constr_op('>=',_) -> '>=';
constr_op(Op, Line) -> throw({error, {Line,?MODULE,{illegal_op,Op}}}).



%% Help functions for function generation (especially creation
%% of 'MNEMOSYNE QUERY' functions).

all_vars ({var, _, '_'}, Res) ->
    Res
;
all_vars ({var, Line, Name}, Res) ->
    ordsets:add_element ({var, '_', Name}, Res)
;
all_vars (X, Res) when tuple (X) ->
    all_vars (tuple_to_list (X), Res)
;
all_vars ([H|T], Res) ->
    ordsets:union ([all_vars (H, ordsets:new()), 
		    all_vars (T, ordsets:new()), Res])
;
all_vars (_, Res) ->
    Res.

make_replacements (Vars) ->
    make_replacements (Vars, 0, []).

make_replacements ([], _, Res) ->
    Res;
make_replacements ([H|T], Next, Res) ->
    make_replacements (T, Next+1, [make_new_var (H, Next) | Res]).

make_new_var (Var, Next) ->
    {Var, {var, 0, list_to_atom ("MNEMOSYNE VAR " ++ io_lib:write (Next))}}.


exchange_vars ([H|T], Vars) ->
    [exchange_vars(H, Vars) | exchange_vars (T, Vars)];

exchange_vars ([], _) ->
    [];
exchange_vars ({record_field,Line,Var,Type,Field}, Vars) ->
    {record_field, Line, exchange_vars (Var, Vars), Type, Field};
exchange_vars ({record_field,Line, Field, Var}, Vars) ->
    {record_field, Line, exchange_vars(Field, Vars), exchange_vars (Var, Vars)};

exchange_vars ({var, L, '_'}, Vars) ->
    {var, L, '_'};
exchange_vars ({var, Line, Name}, Vars) ->
    case exchange_var (Name, Vars) of
	false ->
	    {var, Line, Name};
	NewVar ->
	    NewVar
    end
;
exchange_vars (X, Vars) when tuple (X) ->
    list_to_tuple (exchange_vars (tuple_to_list (X), Vars))
;
exchange_vars (X, _) ->
    X.

exchange_var (Name, VarList) ->
    case lists:keysearch ({var, '_', Name}, 1, VarList) of
	{value, {_, NewVar}} ->
	    NewVar;
	false ->
	    false
    end.



add_query (Code, Vars, S) when tuple (Code) ->
    add_query ([Code], Vars, S)
;
add_query (Code, Vars, S) ->
    [{function, Line, Name, Arity, PrevClauses}] =  S#s.mquery,
    Clause = {clause, Line, [{integer, Line, S#s.mquery_next},
			     {tuple, Line, variables (Vars, [])}],
			     [], 
			     Code},
    S2 = S#s{mquery = [{function, Line, Name, Arity, [Clause | PrevClauses]}],
	     mquery_next = S#s.mquery_next + 1}.
	     
    

variables ([], Res) -> Res
;
variables ([{Orig, Transformed}|T], Res) ->
    variables (T, [Transformed|Res]).

var_names([]) -> []
;
var_names([{var,_,Name}|T]) ->
    [Name | var_names (T)].


%% contain_db_var ({record_field, L, Var, Type, Field}, DBvars) ->
%%     contain_db_var (Var, DBvars);
%% contain_db_var ({var, L, Name}, DBvars) ->
%%     ordsets:is_element ({var, '_', Name}, DBvars);
%% contain_db_var (X, DBvars) when tuple (X) ->
%%     contain_db_var (tuple_to_list (X), DBvars);
%% contain_db_var ([H|T], DBvars) ->
%%     case contain_db_var (H, DBvars) of
%% 	true ->
%% 	    true;
%% 	false ->
%% 	    contain_db_var (T, DBvars)
%%     end;
%% contain_db_var (_, _) ->
%%     false.

mk_initial_mquery () ->
    [{function,0,'MNEMOSYNE QUERY',2,
      [mk_mquery_catch ()]}].


mk_mquery_catch () ->
    {clause, 
     0, 
     [{var, 0, '_'}, {var, 0, '_'}],
     [], 
     [{atom, 0, true}]}.


add_var_type (Name, Def, S) when atom(Name), tuple(Def) ->
    Type = element (1, Def),
    case lists:keysearch (Name, 1, S#s.var_types) of
	{value, {_, Type}} ->
	    S;
	{value, {_, OtherType}} ->
	    throw ({error, {S#s.line, S#s.module, 
			    {type_error, Name, Type, OtherType}}});
	false ->
	    S#s{var_types=[{Name, Type} | S#s.var_types]}
    end.

is_compilable(Expr0, S) ->
    case catch is_compilable2(Expr0, S) of
	{true,Expr} -> {true,Expr};
	unknown_record -> {false,Expr0};
	{error,E} -> throw({error,E})
    end.

is_compilable2 ({var, L, '_'}, S) ->
    {true, {var, L, '_'}};
is_compilable2 ({var, L, Name}, S) ->
    case lists:member (Name, S#s.erl_vars) of
	true ->
	    {true, {var, L, Name}};
	false ->
	    case lists:member (Name, S#s.db_vars) of
		true ->
		    {true, {var, L, Name}};
		false ->
		    throw(unknown_record)
	    end
    end;
is_compilable2({record_field,Line,{var,L1,Var},{atom,L2,Field}}, S) ->
    %% X.a
    is_compilable2({var,L1,Var}, S),  %% might throw
    case lists:keysearch (Var, 1, S#s.var_types) of
	{value, {_, Type}} ->
	    case lists:keysearch(Type, 1, S#s.recdefs) of
		{value, {Type, FList}} ->
		    case lists:member(Field, FList) of
			true ->
			    {true, {record_field,Line,{var,L1,Var},Type,
				    {atom,L2,Field}}};
			false ->
			    throw({error, {Line, ?MODULE, {undefined_field,Type,Field}}})
		    end
	    end;
	false ->
	    throw(unknown_record)
    end;	    
is_compilable2(X,S) when tuple(X) ->
    {true, NewExp} = is_compilable2 (tuple_to_list (X), S),
    {true, list_to_tuple(NewExp)};
is_compilable2 ([H|T], S) ->
    {true, NewH} = is_compilable2 (H,S),
    {true, NewT} = is_compilable2 (T,S),
    {true, [NewH|NewT]};
is_compilable2 (X,S) ->
    {true, X}.
