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
-module(mnemosyne_compiler).

-export([format_error/1, lc_query1/4, pass2_rules/3]).

-export([var_type/2, lookup_record_def/2]).

-define(PARSER_MODULE, mnemosyne_parser).

%%-define(debug, 2).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%================================================================
%%% 		Exports


%%% Entry point for the run-time compilation of queries

lc_query1(Pattern0, Goal0, RecDefs0, Mod) ->
    ?debugmsg(2, "lc-query ~s ~s\n", 
	      [mnemosyne_pp:e(Pattern0), mnemosyne_pp:e(Goal0)]),
    {Goal1,RecDefs} = mnemosyne_transform:unfold(Goal0, RecDefs0),
    VarTypes = get_var_types([Pattern0,Goal1]),
    ?debugmsg(2, "print ~s | ~s -->> ~w  ~w\n", 
	      [mnemosyne_pp:e(Pattern0),
	       mnemosyne_pp:e(Goal1), VarTypes,  RecDefs]),

    PG1 = set_record_type([Pattern0|Goal1], VarTypes, RecDefs),

    [Pattern2|Goal3] = 
	mnemosyne_transform:expand_records(PG1, VarTypes, RecDefs, Mod),

    ?debugmsg(2, "lc ~s | ~s\n", 
	      [mnemosyne_pp:e(Pattern2), mnemosyne_pp:e(Goal3)]),
    R1 = mnemosyne_optimizer:phase1(Goal3, Pattern2).


%% Semantic checks of rules at (erlang) compile time
%% returns {ok, [{rule,H,B}...]}  or  {error, [errmsg...]}

pass2_rules(Rules, Module, Record_defs) ->
    R = lists:map(
	  fun({rule,Head0,Body0}) ->
		  catch
		      begin
			  Types = 
			      get_var_types([Head0|Body0]),

			  [Head|Body] = 
			      set_record_type([Head0|Body0],
					      Types,
					      Record_defs),
			  {rule, Head, Body}
		      end
	  end, Rules),
    
    case lists:foldl(
	   fun
	       ({error,Error}, Acc) -> [{error,Error} | Acc];
	       (_, Acc) -> Acc
	   end, [], R) of
	[] -> {ok, R};
	L when list(L) -> {error, L}
    end.
	   

format_error(Msg) ->
    case Msg of
	{already_defined,{type,Var,OtherType}} ->
	    ["\"", atom_to_list(Var), 
	     "\" is already associated with the record \"",
	     atom_to_list(OtherType), "\""];

	must_give_record_name ->
	    "Record-name must be explicitly given";

	{must_give_record_name,Name} ->
	    ["Record-name must be explicitly given for variable \"",
	     atom_to_list(Name), "\""];

	{record_name_missmatch, Has, Expect} ->
	    io_lib:format("Record name missmatch, found \"~w\", expected \"~w\"",
			  [Has, Expect]);

	{undefined,{record_fields,RecName,Var,FieldNames}} ->
	    io_lib:format("Undefined record fields in record \"~w\" (variable ~w): ~p",
			  [RecName,Var,FieldNames]);

	{undefined,{record_fields,RecName,FieldNames}} ->
	    io_lib:format("Undefined record fields in record \"~w\": ~p",
			  [RecName,FieldNames]);

	{undefined,{record,RecName}} ->
	    io_lib:format("Undefined record \"~w\"", [RecName]);

	{undefined,What} when atom(What) ->
	    io_lib:format("No ~w definition", [What]);

	{cannot_find_type,Var} ->
	    io_lib:format("Can't deduce the corresponding record for \"~w\"",
			  [Var]);

	{not_found, Paths} ->
	    io_lib:format("No file found, tried \"~p\"", [Paths]);

	{transaction_aborted,Reason} ->
	    io_lib:format("Transaction aborted with reason \"~p\"", [Reason]);

	{nyi, What} ->
	    io_lib:format("\"~s\" not yet implemented", [What]);

	{unknown_form, What} ->
	    io_lib:format("\"~w\" unknown", [What]);

	{illegal_pattern, Type} ->
	    io_lib:format("Illegal pattern for a \"~w\"", [Type]);

	{illegal_form,Type,FileType} ->
	    io_lib:format("No ~s in a ~s file", [Type,FileType]);

	{illegal_declaration,Found,Expect} ->
	    io_lib:format("Illegal declaration, found \"~w\", expected \"~w\"",
			  [Found,Expect])
    end.


%%% Look up the type of a variable

var_type({'#var',Var}, VTs) -> var_type(Var, VTs);
var_type(Var, VTs) -> lists:keysearch(Var,1,VTs).


%%% Look up a record definition

lookup_record_def(Name, RecordDefs) -> lists:keysearch(Name,1,RecordDefs).

%%%================================================================
%%% 		Private

%%%----------------------------------------------------------------
%% fix-point iteration to find to which records variables are associated
%% Returns a list of {VarName,RecordName}

%% get_var_types/2 calls get_var_types/3 wich iterates until no further
%% information is gathered.

%% example 1: The GoalList is A=B, A=AA, B=BB, p(AA), q(BB).
%%		Iteration	Deduced information in each iteration
%%		    1		AA - p   BB - q
%%		    2		A - p	 B - q
%%		[   3		 *error* A=B => p=q which is obviously wrong]
 
%% example 2: The GoalList is A=C.a, A=AA, B=BB, p(AA), q(BB).
%%		Iteration	Deduced information in each iteration
%%		    1		AA - p   BB - q
%%		    2		A - p	 B - q
%%		    3		
%%		[ check		*error* type of C unknown ]

get_var_types(Gs) -> get_var_types(Gs, get_var_types(Gs,[]), []).


get_var_types(Gs, VT, VT) ->  VT;
get_var_types(Gs, VT, _) -> get_var_types(Gs, get_var_types(Gs,VT), VT).

get_var_types(P, VTs) when record(P,pred_sym),
			   P#pred_sym.record_def =/= ?UNKNOWN ->
    %% Rule or fact call whos type is fully known
    [{'#var',V}] = P#pred_sym.args,
    {Name,Fs} = P#pred_sym.record_def,
    var_type(Name, V, P#pred_sym.line, VTs);

get_var_types(P, VTs) when record(P,pred_sym) ->
    VTs;

get_var_types(C, VTs) when record(C,constraint) ->
    get_var_types(C#constraint.exprL, get_var_types(C#constraint.exprR,VTs));

get_var_types({'#or', C, Alts}, VTs) ->
    get_var_types(Alts, VTs);

get_var_types({'#not', C, Gs}, VTs) ->
    get_var_types(Gs, C, VTs);

%%% This is not exactly right ...
get_var_types(D, VTs) when record(D,disj_alt) ->
    get_var_types(D#disj_alt.conj,VTs);

get_var_types([H|T], VTs) ->
    get_var_types(T, get_var_types(H,VTs));

get_var_types(R, VTs) when record(R,rec_f) ->
    var_type(R#rec_f.name, R#rec_f.var, R#rec_f.line, VTs);

get_var_types(T, VTs) when tuple(T) ->
    get_var_types(tuple_to_list(T), VTs);

get_var_types(_, VTs) ->
    VTs.




%%% Enter the type of a variable
var_type(_, [], _, VTs) ->
    VTs;

var_type(Type, {'#var',Var}, Line, VTs) ->
    var_type(Type, Var, Line, VTs);

var_type(?UNKNOWN, Var, Line, VTs) ->
    VTs;

var_type(Type, Var, Line, VTs) ->
    case var_type(Var,VTs) of
	{value, {Var,Type}} ->			% already there
	    VTs;
	{value, {Var,OtherType}} ->		% already there WITH OTHER TYPE
	    throw({error, 
		   {Line,?MODULE,{already_defined,{type,Var,OtherType}}}});
	false ->
	    [{Var,Type}|VTs]
    end.


%%%----------------------------------------------------------------
%%% Adds the deduced record type to all record references and checks that
%%% used fields in declared records are declared. Also checks that explicit
%%% record names (like 'r' in X#r.a) is the same as the deduced type.


set_record_type(P, VarTypes, RecordDefs) when record(P,pred_sym) ->
    P;

set_record_type(C, VarTypes, RecordDefs) when record(C,constraint),
					      C#constraint.op == '='  ->
    case {C#constraint.exprL, C#constraint.exprR} of
	{{'#var',Vc}, R} when record(R,rec_c) ->
	    Rnew=
		case var_type(Vc,VarTypes) of
		    
		    {value, {_,RecName}} when R#rec_c.name == ?UNKNOWN ->
			R#rec_c{name=RecName};
		    
		    {value, {_,RecName}} when R#rec_c.name == RecName ->
			R;
		    
		    {value, {_,RecName}} when R#rec_c.name =/= RecName ->
			throw({error, {C#constraint.line, ?MODULE, 
				       {already_defined,{type,Vc,RecName}}}});
		    
		    false when R#rec_c.name == ?UNKNOWN ->
			throw({error, {C#constraint.line, ?MODULE, 
				       {must_give_record_name,Vc}}});
		    
		    false when R#rec_c.name =/= ?UNKNOWN ->
			R
		end,
	    C#constraint{exprR = set_record_type(Rnew,VarTypes,RecordDefs)};
	
	{R, {'#var',Vc}} when record(R,rec_c) ->
	    set_record_type(C#constraint{exprL={'#var',Vc},exprR=R}, 
			 VarTypes, RecordDefs);

	{L, R} when record(L,rec_c), record(R,rec_c) ->
	    if
		L#rec_c.name == R#rec_c.name ->
		    C#constraint{exprL=set_record_type(L,VarTypes,RecordDefs),
				 exprR=set_record_type(R,VarTypes,RecordDefs)};

		L#rec_c.name =/= R#rec_c.name ->
		    throw({error, {C#constraint.line, ?MODULE, 
				   {record_name_missmatch, R#rec_c.name,
				    L#rec_c.name} }})
	    end;

	{L,R} ->
	    C#constraint{exprL=set_record_type(L,VarTypes,RecordDefs),
			 exprR=set_record_type(R,VarTypes,RecordDefs)}
    end;

set_record_type(R, VarTypes, RecordDefs) when record(R,rec_f) ->
    %% X.f of X#r.f
    {'#var',Var} = R#rec_f.var,
    R1 =
	case var_type(Var,VarTypes) of
	    {value, {_,RecName}} when R#rec_f.name == ?UNKNOWN ->
		R#rec_f{name=RecName};
	    
	    {value, {_,RecName}} when R#rec_f.name == RecName ->
		R;
	    
	    {value, {_,RecName}} when R#rec_f.name =/= RecName ->
		throw({error, {R#rec_f.line, ?MODULE, 
		       {already_defined,{type,Var,RecName}}}});
	    
	    false when R#rec_f.name == ?UNKNOWN ->
		throw({error, {R#rec_f.line, ?MODULE, 
			       {must_give_record_name,Var}}});
	    
	    false when R#rec_f.name =/= ?UNKNOWN ->
		R
	end,
    case lookup_record_def(R1#rec_f.name, RecordDefs) of
	{value, {_,FieldNames}} ->
	    Field = R1#rec_f.field,
	    case lists:member(Field,FieldNames) of
		true ->  
		    R1;
%%		    {'#var',{Var,Field}};
		false ->
		    throw({error, 
			   {R1#rec_f.line, ?MODULE, 
			    {undefined,{record_fields,
					R1#rec_f.name,Var,[Field]}}}})
		    end;
	
	false -> %% No record declaration, reported later
	    R1
    end;
    

set_record_type(R, VarTypes, RecordDefs) when record(R,rec_c), 
					   R#rec_c.name == ?UNKNOWN ->
    %% #{a=.., b=.. ...}
    throw({error, {R#rec_c.line, ?MODULE, must_give_record_name}});

set_record_type(R, VarTypes, RecordDefs) when record(R,rec_c), 
					   R#rec_c.name =/= ?UNKNOWN ->
    %% #r{a=.., b=.. ...}
    case lookup_record_def(R#rec_c.name, RecordDefs) of
	{value, {RecName,FieldNames}} ->
	    Empty = ordsets:new_set(),
	    case ordsets:subtract(ordsets:list_to_set(
				    mnemosyne_lib:elements(1,R#rec_c.fields)
				   ),
				  ordsets:list_to_set(FieldNames)) of
		Empty ->
		    R#rec_c{fields = set_record_type(R#rec_c.fields,
						  VarTypes, RecordDefs)};

		S -> %% Using undeclared fields in a declared record 
		    throw({error, 
			   {R#rec_c.line,?MODULE,
			    {undefined,{record_fields,R#rec_c.name,
					ordsets:set_to_list(S)}}}})
	    end;

	false -> %% No record declaration, reported later
	    R
    end;

set_record_type({'#not', C, Gs}, VarTypes, RecordDefs) ->
    {'#not', C, set_record_type(Gs,VarTypes,RecordDefs)};

set_record_type([H|T], VarTypes, RecordDefs) ->
    [set_record_type(H,VarTypes,RecordDefs) |
     set_record_type(T,VarTypes,RecordDefs)];

set_record_type(Tuple, VarTypes, RecordDefs) when tuple(Tuple) ->
    list_to_tuple( 
      set_record_type(tuple_to_list(Tuple), VarTypes, RecordDefs)
     );

set_record_type(Term, VarTypes, RecordDefs) ->
    Term.






