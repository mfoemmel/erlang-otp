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
-module(mnemosyne_transform).
-export([unfold/2,
	 expand_records/4,
	 format_error/1]).

%%-define(debug,1).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%================================================================
%%% 		Exports


unfold(Goals, RecordDefinitions) -> 
    {_, Alts, RecDefs} =
	unfold(Goals, RecordDefinitions, mnemosyne_lib:new()),
    {case Alts of
	 [Alt] when list(Alt) -> 
	     Alt;
	 Alts when list(Alts) -> 
	     {'#or', 1, lists:map(fun(A) -> #disj_alt{conj=A} end, Alts)}
     end,
     RecDefs}.


expand_records(Goals, VarTypes, RecDefs, Mod) ->
    expand_record(Goals, VarTypes, RecDefs, Mod).


format_error(Msg) ->
    case Msg of
	{undefined,{record_fields,RecName,Var,FieldNames}} ->
	    io_lib:format("Undefined record fields in record ~w (variable ~w): ~p",
			  [RecName,Var,FieldNames]);

	{undefined,{record_fields,RecName,FieldNames}} ->
	    io_lib:format("Undefined record fields in record ~w: ~p",
			  [RecName,FieldNames]);

	{undefined,{record,RecName}} ->
	    io_lib:format("Undefined record ~w", [RecName])
    end.

%%%================================================================
%%% 		Private


unfold(Goals, RecordDefinitions, SymTab) ->
    unfold(Goals, RecordDefinitions, SymTab, [[]]).



unfold([{'#or',_,Alts0}|Goals], RecDefs0, SymTab0, Acc) ->
    {SymTab, Alts, RecDefs} = unfold_each_alt(Alts0, RecDefs0, SymTab0),
    unfold(Goals, RecDefs, SymTab, append_clauses(Alts,Acc));

unfold([Goal|Goals], RecDefs0, SymTab0, Acc) when record(Goal,pred_sym) ->
    RecDefs = visible_record_definitions(RecDefs0, Goal#pred_sym.module),
    {RecName,_} = Goal#pred_sym.record_def,
    case lists:keysearch(RecName,1,RecDefs) of
	{value, Def} ->
	    SymTab = symtab(Goal, SymTab0),
	    Recursive = recursive(Goal,SymTab),
	    unfold_pred_sym_goal(Goal#pred_sym.type, Recursive,
				 Goal#pred_sym{
%					       record_def=Def,
					       recursive=Recursive},
				 Goals, RecDefs, SymTab, Acc);
	false ->
	    throw({error, {Goal#pred_sym.line, ?MODULE, 
			   {undefined,{record,RecName}}}})
    end;

unfold([{'#not',C,NegGoals0}|Goals], RecDefs0, SymTab0, Acc) ->
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body([{'#not',C,NegGoals0}])]),
    %% Input: ~(p,q,...) 
    {SymTab, NegGoals, RecDefs} = unfold(NegGoals0, RecDefs0, SymTab0),
?debugmsg(4, "~w\n", [NegGoals0]),
?debugmsg(4, "~w\n", [NegGoals]),
    %% NegGoals = (p';q';...)
    NegAlts = 
	lists:map(fun(Alt) ->
			  lists:map(fun(Goal) ->
					    {'#not',C,[Goal]} 
				    end,
				    Alt)
		  end,
		  NegGoals),

?debugmsg(4, "~w\n", [NegAlts]),
    %% NegAlts = (~p';~q';~...)
    %% Now we have transformed ~(p,q,...) into (~p';~q';~...)
    %%   (partial evaluation of the '#or' clause above):
    Acc1 = append_clauses(NegAlts,Acc),
    ?debugmsg(3, "~s\n", [mnemosyne_pp:body(Acc1)]),
    unfold(Goals, RecDefs, SymTab, append_clauses(NegAlts,Acc));

unfold([Goal|Goals], RecDefs, SymTab, Acc) ->
    unfold(Goals, RecDefs, SymTab, append_clauses([[Goal]],Acc));

unfold([], RecDefs, SymTab, Acc) ->
    {SymTab,Acc,RecDefs}.
		


unfold_pred_sym_goal(rule, non_recursive, Goal, Goals, RecDefs, SymTab, Acc) ->
    unfold([{'#or', 1, mnemosyne_lib:db_read_clauses(Goal)}|Goals],
	   RecDefs, SymTab, Acc);	
unfold_pred_sym_goal(_, _, Goal, Goals, RecDefs, SymTab, Acc) ->
    unfold(Goals, RecDefs, SymTab, append_clauses([[Goal]],Acc)).



unfold_each_alt(Alts, RecDefs, SymTab) ->
    unfold_each_alt(Alts, RecDefs, SymTab, []).

unfold_each_alt([Goals|GoalsL], RecDefs0, SymTab0, Acc) ->
    {SymTab1, Alts, RecDefs} = unfold(Goals, RecDefs0, SymTab0, [[]]),
    unfold_each_alt(GoalsL, RecDefs, SymTab1, [Alts|Acc]);
unfold_each_alt([], RecDefs, SymTab, Acc) ->
    {SymTab, lists:append(Acc), RecDefs}.



append_clauses([C|Cs], Acc) ->
    lists:append(lists:map({lists,append},[C],Acc),
		 append_clauses(Cs, Acc));
append_clauses([], Acc) ->
    [].


%%%----------------
visible_record_definitions(RecDefs, ?NO_MODULE) ->
    RecDefs;
visible_record_definitions(RecDefs, Module) ->
    ModuleLocalDefs = 
	lists:flatten(mnemosyne_lib:db_data(record_defs, Module)),
    compose_rec_defs(ModuleLocalDefs, RecDefs).

%%%----------------------------------------------------------------
%%% Compose local record declarations with those already present.

compose_rec_defs([D|Ds], Defs) ->
    {D_Name,_} = D,
    compose_rec_defs(Ds, 
		     case lists:keysearch(D_Name,1,Defs) of
			 {value, _} -> Defs;
			 false -> [D|Defs]
		     end);
compose_rec_defs([], Defs) ->
    Defs.

%%%----------------
symtab(Goal, SymTab) when Goal#pred_sym.type == rule ->
    case mnemosyne_lib:lookup(Goal#pred_sym.functor, SymTab) of
	false ->
	    analyse(mnemosyne_lib:db_read_clauses(Goal),
		    ordsets:list_to_set([Goal#pred_sym.functor]),
		    mnemosyne_lib:insert(Goal#pred_sym.functor, 
					 non_recursive, % Might change later
					 SymTab));	% in the analyze
	_ ->
	    SymTab
    end;
symtab(_, SymTab) ->
    SymTab.


recursive(Goal, SymTab) when Goal#pred_sym.type == rule ->
    {true, R} = mnemosyne_lib:lookup( Goal#pred_sym.functor, SymTab),
    R;
recursive(Goal, SymTab) when Goal#pred_sym.type == table ->
    non_recursive.
    


analyse([Clause|Clauses], Trace, SymTab) ->
    analyse(Clauses, Trace, analyse_clause(Clause,Trace,SymTab));
analyse([], Trace, SymTab) ->
    SymTab.


analyse_clause([Goal|Goals], Trace, SymTab) ->
    analyse_clause(Goals, Trace, analyse_goal(Goal,Trace,SymTab));
analyse_clause([], _, SymTab) ->
    SymTab.


analyse_goal(Goal, Trace, SymTab0) when record(Goal,pred_sym), 
					Goal#pred_sym.type == rule ->
    SymTab = symtab(Goal, SymTab0),
    case recursive(Goal,SymTab) of
	R when R =/= recursive ->
	    case ordsets:is_element(Goal#pred_sym.functor, Trace) of
		true ->
		    mnemosyne_lib:insert(Goal#pred_sym.functor, 
					 recursive,
					 SymTab);
		false ->
		    analyse(mnemosyne_lib:db_read_clauses(Goal),
			    ordsets:add_element(Goal#pred_sym.functor,Trace), 
			    mnemosyne_lib:insert(Goal#pred_sym.functor, 
						 non_recursive, % Might change
						 SymTab)        %in later stage
			   )
	    end;
	_ ->
	    SymTab
    end;

analyse_goal({'#not',C,NegGoals}, Trace, SymTab) -> 
    analyse_clause(NegGoals, Trace, SymTab);

analyse_goal(_, Trace, SymTab) -> 
    SymTab.

	
%%%----------------------------------------------------------------
expand_record(R, VarTypes, RecordDefs, Mod) when record(R,rec_f) ->
    %% X.f
    {'#var',Var} = R#rec_f.var,
    case mnemosyne_compiler:lookup_record_def(R#rec_f.name, RecordDefs) of
	{value, {_,FieldNames}} ->
	    Field = R#rec_f.field,
	    case lists:member(Field,FieldNames) of
		true ->  
		    {'#var',{Var,Field}};
		false ->
		    throw({error, {R#rec_f.line,?MODULE, 
				   {undefined,{record_fields,
					       R#rec_f.name,Var,[Field]}}}})
	    end;

	false ->
	    throw({error, 
		   {R#rec_f.line,?MODULE,{undefined,{record,R#rec_f.name}}}})
    end;


expand_record(R, VarTypes, RecordDefs, Mod) when record(R,rec_c) ->
    %% #{a=.., b=.. ...}
    case mnemosyne_compiler:lookup_record_def(R#rec_c.name, RecordDefs) of
	{value, {RecName,FieldNames}} ->
	    Empty = ordsets:new_set(),
	    case ordsets:subtract(ordsets:list_to_set(
				    mnemosyne_lib:elements(1,R#rec_c.fields)
				   ),
				  ordsets:list_to_set(FieldNames)) of
		Empty -> 
		    InitialValue =
			case catch Mod:'MNEMOSYNE RECFUNDEF'(RecName) of
			    {'EXIT', Reason} ->
				throw ({error, 
					{R#rec_c.line, ?MODULE,
					 {internal, {record,R#rec_f.name}}}});
			    Initial ->
				Initial
			end,
		    lists:foldl(
		      fun({FieldName,_,FieldVal}, Val) ->
			      setelement(1+mnemosyne_lib:list_pos(FieldName,
								  FieldNames),
					 Val,
					 expand_record(FieldVal,
						       VarTypes,
						       RecordDefs,Mod))
			      end, InitialValue, R#rec_c.fields);
		
		S -> %% Using undeclared fields in a declared record 
		    throw({error, 
			   {R#rec_c.line,?MODULE,
			    {undefined,{record_fields,R#rec_c.name,
					ordsets:set_to_list(S)}}}})
	    end;

	false ->
	    throw({error, 
		   {R#rec_c.line,?MODULE,{undefined,{record,R#rec_c.name}}}})
	    
    end;

expand_record({'#var',V}, VarTypes, RecDefs, Mod) ->
    case mnemosyne_compiler:var_type(V,VarTypes) of
	{value, {V,Type}} ->
	    case  mnemosyne_compiler:lookup_record_def(Type, RecDefs) of
		{value, {_,FieldNames}} ->
		    expand_record(var_to_record(Type, FieldNames, [], V),
				  VarTypes,
				  RecDefs, Mod);
		false ->
		    io:format('Why (V1) ? ~w, ~w\n', [V,VarTypes]),
		    {'#var',V}
	    end;
	false ->
	    {'#var',V}
    end;


expand_record([H|T], VarTypes, RecDefs, Mod) ->
    [  expand_record(H,VarTypes,RecDefs, Mod) 
     | expand_record(T,VarTypes,RecDefs, Mod)];

expand_record(Tuple, VarTypes, RecordDefs, Mod) when tuple(Tuple) ->
    list_to_tuple( expand_record(tuple_to_list(Tuple), VarTypes, 
				 RecordDefs, Mod));

expand_record(Term, VarTypes, RecordDefs, Mod) ->
    Term.


    
var_to_record(Type, Names, Fields, Var) ->
    list_to_tuple(
      [Type | lists:foldr(
		fun(Name,T) ->
			case lists:keysearch(Name,1,Fields) of
			    {value, {FieldName,Line,FieldVal}} ->
				[FieldVal|T];
			    false ->
				[{'#var',{Var,Name}}|T]
			end
		end,
		[], Names)]).

