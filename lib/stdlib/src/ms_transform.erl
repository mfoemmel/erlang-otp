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
-module(ms_transform).

-export([format_error/1,transform_from_shell/3,parse_transform/2]).

%% Error codes.
-define(ERROR_BASE_GUARD,0).
-define(ERROR_BASE_BODY,100).
-define(ERR_NOFUN,1).
-define(ERR_ETS_HEAD,2).
-define(ERR_DBG_HEAD,3).
-define(ERR_HEADMATCH,4).
-define(ERR_SEMI_GUARD,5).
-define(ERR_UNBOUND_VARIABLE,6).
-define(ERR_HEADBADREC,7).
-define(ERR_HEADBADFIELD,8).
-define(ERR_GENMATCH,16).
-define(ERR_GENLOCALCALL,17).
-define(ERR_GENELEMENT,18).
-define(ERR_GENBADFIELD,19).
-define(ERR_GENBADREC,20).
-define(ERR_GUARDMATCH,?ERR_GENMATCH+?ERROR_BASE_GUARD).
-define(ERR_BODYMATCH,?ERR_GENMATCH+?ERROR_BASE_BODY).
-define(ERR_GUARDLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_GUARD).
-define(ERR_BODYLOCALCALL,?ERR_GENLOCALCALL+?ERROR_BASE_BODY).
-define(ERR_GUARDELEMENT,?ERR_GENELEMENT+?ERROR_BASE_GUARD).
-define(ERR_BODYELEMENT,?ERR_GENELEMENT+?ERROR_BASE_BODY).
-define(ERR_GUARDBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_GUARD).
-define(ERR_BODYBADFIELD,?ERR_GENBADFIELD+?ERROR_BASE_BODY).
-define(ERR_GUARDBADREC,?ERR_GENBADREC+?ERROR_BASE_GUARD).
-define(ERR_BODYBADREC,?ERR_GENBADREC+?ERROR_BASE_BODY).

%%
%% Called by compiler or ets/dbg:fun2ms when errors occur
%%
format_error(?ERR_NOFUN) ->	    
    "Parameter of ets/dbg:fun2ms/1 is not a literal fun";
format_error(?ERR_ETS_HEAD) ->	    
    "ets:fun2ms requires fun with single variable or tuple parameter";
format_error(?ERR_DBG_HEAD) ->	    
    "dbg:fun2ms requires fun with single variable or list parameter";
format_error(?ERR_HEADMATCH) ->	    
    "fun with head matching ('=' in head) cannot be translated into match_spec";
format_error(?ERR_SEMI_GUARD) ->	    
    "fun with semicolon (;) in guard cannot be translated into match_spec";
format_error(?ERR_GUARDMATCH) ->	    
    "fun with guard matching ('=' in guard) is illegal as match_spec as well";
format_error({?ERR_GUARDLOCALCALL, Name}) ->	    
    lists:flatten(io_lib:format("fun containing local erlang function calls "
				"('~w' called in guard) "
				"cannot be translated into match_spec",
				[Name]));
format_error({?ERR_GUARDELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~s (in guard) cannot be translated "
		    "into match_spec", [Str]));
format_error(?ERR_BODYMATCH) ->	    
    "fun with body matching ('=' in body) is illegal as match_spec";
format_error({?ERR_BODYLOCALCALL, Name}) ->	    
    lists:flatten(io_lib:format("fun containing local erlang function calls "
				"('~w' called in body) "
				"cannot be translated into match_spec",
				[Name]));
format_error({?ERR_BODYELEMENT, Str}) ->
    lists:flatten(
      io_lib:format("the language element ~s (in body) cannot be translated "
		    "into match_spec", [Str]));
format_error({?ERR_UNBOUND_VARIABLE, Str}) ->
    lists:flatten(
      io_lib:format("the variable ~s is unbound, cannot translate "
		    "into match_spec", [Str]));
format_error({?ERR_HEADBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun head contains unknown record type ~w",[Name]));
format_error({?ERR_HEADBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun head contains reference to unknown field ~w in "
		    "record type ~w",[FName, RName]));
format_error({?ERR_GUARDBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains unknown record type ~w",[Name]));
format_error({?ERR_GUARDBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun guard contains reference to unknown field ~w in "
		    "record type ~w",[FName, RName]));
format_error({?ERR_BODYBADREC,Name}) ->	    
    lists:flatten(
      io_lib:format("fun body contains unknown record type ~w",[Name]));
format_error({?ERR_BODYBADFIELD,RName,FName}) ->	    
    lists:flatten(
      io_lib:format("fun body contains reference to unknown field ~w in "
		    "record type ~w",[FName, RName]));
format_error(Else) ->
    lists:flatten(io_lib:format("Unknown error code ~w",[Else])).

%%
%% Called when translating in shell
%%
transform_from_shell(Dialect, Clauses, BoundEnvironment) ->
    SaveFilename = setup_filename(),
    case catch ms_clause_list(1,Clauses,Dialect) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,Line,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{Line, ?MODULE, R}]}], []};
	Else ->
            case (catch fixup_environment(Else,BoundEnvironment)) of
                {error,Line1,R1} ->
                    {error, [{cleanup_filename(SaveFilename),
                             [{Line1, ?MODULE, R1}]}], []}; 
                Else1 ->
		    Ret = normalise(Else1),
                    cleanup_filename(SaveFilename),
		    Ret
            end
    end.
    

%%
%% Called when translating during compiling
%%
parse_transform(Forms, _Options) ->
    SaveFilename = setup_filename(),
    %erlang:display(Forms),
    case catch forms(Forms) of
	{'EXIT',Reason} ->
	    cleanup_filename(SaveFilename),
	    exit(Reason);
	{error,Line,R} ->
	    {error, [{cleanup_filename(SaveFilename),
		      [{Line, ?MODULE, R}]}], []};
	Else ->
	    cleanup_filename(SaveFilename),
	    Else
    end.

setup_filename() ->
    {erase(filename),erase(records)}.

put_filename(Name) ->
    put(filename,Name).

put_records(R) ->
    put(records,R),
    ok.
get_records() ->
    case get(records) of
	undefined ->
	    [];
	Else ->
	    Else
    end.
cleanup_filename({Old,OldRec}) ->
    Ret = case erase(filename) of
	      undefined ->
		  "TOP_LEVEL";
	      X ->
		  X
	  end,
    case OldRec of
	undefined ->
	    erase(records);
	Rec ->
	    put(records,Rec)
    end,
    case Old of
	undefined ->
	    Ret;
	Y ->
	    put(filename,Y),
	    Ret
    end.

add_record_definition({Name,FieldList}) ->
    {KeyList,_} = lists:foldl(
		    fun({record_field,_,{atom,Line0,FieldName}},{L,C}) ->
			    {[{FieldName,C,{atom,Line0,undefined}}|L],C+1};
		       ({record_field,_,{atom,_,FieldName},Def},{L,C}) ->
			    {[{FieldName,C,Def}|L],C+1}
		    end,
		    {[],2},
		    FieldList),
    put_records([{Name,KeyList}|get_records()]).

forms([F0|Fs0]) ->
    F1 = form(F0),
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

form({attribute,_,file,{Filename,_}}=Form) ->
    put_filename(Filename),
    Form;
form({attribute,_,record,Definition}=Form) -> 
    add_record_definition(Definition),
    Form;
form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
form(AnyOther) ->
    AnyOther.
function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name,Arity,Clauses1}.
clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].
clause({clause,Line,H0,G0,B0}) ->
    B1 = copy(B0),
    {clause,Line,H0,G0,B1}.

copy({call,Line,{remote,_Line2,{atom,_Line3,ets},{atom,_Line4,fun2ms}},
      As0}) ->
    transform_call(ets,Line,As0);
copy({call,Line,{remote,_Line2,{atom,_Line3,dbg},{atom,_Line4,fun2ms}},
      As0}) ->
    transform_call(dbg,Line,As0);
copy(T) when is_tuple(T) ->
    list_to_tuple(copy_list(tuple_to_list(T)));
copy(L) when is_list(L) ->
    copy_list(L);
copy(AnyOther) ->
    AnyOther.

copy_list([H|T]) ->
    [copy(H)|copy_list(T)];
copy_list([]) ->
    [].

transform_call(Type,_Line,[{'fun',Line2,{clauses, ClauseList}}]) ->
    ms_clause_list(Line2, ClauseList,Type);
transform_call(_Type,Line,_NoAbstractFun) ->
    throw({error,Line,?ERR_NOFUN}).

ms_clause_list(Line,[H|T],Type) ->
    {cons, Line, ms_clause(H,Type), ms_clause_list(Line, T,Type)};
ms_clause_list(Line,[],_) ->
    {nil,Line}.
ms_clause({clause, Line, Parameters, Guards, Body},Type) ->
    check_type(Line,Parameters,Type),
    {MSHead,Bindings} = transform_head(Parameters),
    MSGuards = transform_guards(Line, Guards, Bindings),
    MSBody = transform_body(Line,Body,Bindings),
    {tuple, Line, [MSHead,MSGuards,MSBody]}.


check_type(_,[{var,_,_}],_) ->
    ok;
check_type(_,[{tuple,_,_}],ets) ->
    ok;
check_type(_,[{record,_,_,_}],ets) ->
    ok;
check_type(_,[{cons,_,_,_}],dbg) ->
    ok;
check_type(Line,_Type,ets) ->
    throw({error,Line,?ERR_ETS_HEAD});
check_type(Line,_,dbg) ->
    throw({error,Line,?ERR_DBG_HEAD}).

-record(tgd,{ b, %Bindings 
	      p, %Part of spec
	      eb %Error code base, 0 for guards, 100 for bodies
	     }).

transform_guards(Line,[],_Bindings) ->
    {nil,Line};
transform_guards(Line,[G],Bindings) ->
    B = #tgd{b = Bindings, p = guard, eb = ?ERROR_BASE_GUARD},
    tg0(Line,G,B);
transform_guards(Line,_,_) ->
    throw({error,Line,?ERR_SEMI_GUARD}).
    
transform_body(Line,Body,Bindings) ->
    B = #tgd{b = Bindings, p = body, eb = ?ERROR_BASE_BODY},
    tg0(Line,Body,B).
    

tg0(Line,[],_) ->
    {nil,Line};
tg0(Line,[H|T],B) ->
    {cons,Line, tg(H,B), tg0(Line,T,B)}.
    

tg({match,Line,_,_},B) -> 
    throw({error,Line,?ERR_GENMATCH+B#tgd.eb});
tg({op, Line, Operator, O1, O2}, B) ->
    {tuple, Line, [{atom, Line, Operator}, tg(O1,B), tg(O2,B)]};
tg({op, Line, Operator, O1}, B) ->
    {tuple, Line, [{atom, Line, Operator}, tg(O1,B)]};
tg({call, _Line, {atom, Line2, bindings},[]},_B) ->
    	    {atom, Line2, '$*'};
tg({call, _Line, {atom, Line2, object},[]},_B) ->
    	    {atom, Line2, '$_'};
tg({call, Line, {atom, _, is_record}=Call,[Object, {atom,Line3,RName}=R]},B) ->
    MSObject = tg(Object,B),
    RDefs = get_records(),
    case lists:keysearch(RName,1,RDefs) of
	{value, {RName, FieldList}} ->
	    RSize = length(FieldList)+1,
	    {tuple, Line, [Call, MSObject, R, {integer, Line3, RSize}]};
	_ ->
	    throw({error,Line3,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;
tg({call, Line, {atom, Line2, FunName},ParaList},B) ->
    case is_ms_function(FunName,B#tgd.p) of
	true ->
	    {tuple, Line, [{atom, Line2, FunName} | 
			   lists:map(fun(X) -> tg(X,B) end, ParaList)]};
	_ ->
	    throw({error,Line,{?ERR_GENLOCALCALL+B#tgd.eb,FunName}}) 
    end;
tg({cons,Line, H, T},B) -> 
    {cons, Line, tg(H,B), tg(T,B)};
tg({nil, Line},_B) ->
    {nil, Line};
tg({tuple,Line,L},B) ->
    {tuple,Line,[{tuple,Line,lists:map(fun(X) -> tg(X,B) end, L)}]};
tg({integer,Line,I},_) ->
    {integer,Line,I};
tg({float, Line,F},_) ->
    {float,Line,F};
tg({atom,Line,A},_) ->
    {atom,Line,A};
tg({string,Line,S},_) ->
    {string,Line,S};
tg({var,Line,VarName},B) ->
    case lkup_bind(VarName, B#tgd.b) of
	undefined ->
	    {tuple, Line,[{atom, Line, 'const'},{var,Line,VarName}]};
	AtomName ->
	    {atom, Line, AtomName}
    end;
tg({record_field,Line,Object,RName,{atom,_Line1,KeyName}},B) ->
    RDefs = get_records(),
    case lists:keysearch(RName,1,RDefs) of
	{value, {RName, FieldList}} ->
	    case lists:keysearch(KeyName,1, FieldList) of
		{value, {KeyName,Position,_}} ->
		    NewObject = tg(Object,B),
		    {tuple, Line, [{atom, Line, 'element'}, 
				   {integer, Line, Position}, NewObject]};
		_ ->
		    throw({error,Line,{?ERR_GENBADFIELD+B#tgd.eb, RName, 
				       KeyName}})
	    end;
	_ ->
	    throw({error,Line,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;
tg({record,Line,RName,RFields},B) ->
    RDefs = get_records(),
    KeyList = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    (_,_) ->
					 throw({error,Line,?ERR_HEADBADREC})
				 end,
				 [],
				 RFields),
    case lists:keysearch(RName,1,RDefs) of
	{value, {RName, FieldList0}} ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,Def},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						Def
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_undef_field(RName,Line,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Line,[{tuple,Line,[{atom,Line,RName}|FieldList1]}]};
	_ ->
	    throw({error,Line,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;

tg({record,Line,{var,Line2,_VName}=AVName, RName,RFields},B) ->
    RDefs = get_records(),
    MSVName = tg(AVName,B),
    KeyList = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     L) ->
					 NV = tg(Value,B),
					 [{Key,NV}|L];
				    (_,_) ->
					 throw({error,Line,?ERR_HEADBADREC})
				 end,
				 [],
				 RFields),
    case lists:keysearch(RName,1,RDefs) of
	{value, {RName, FieldList0}} ->
	    FieldList1 = lists:foldl(
			   fun({FN,Pos,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						{tuple, Line2, 
						 [{atom, Line2, element},
						  {integer, Line2, Pos},
						  MSVName]}
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_undef_field(RName,Line,KeyList,FieldList0,
			      ?ERR_GENBADFIELD+B#tgd.eb),
	    {tuple,Line,[{tuple,Line,[{atom,Line,RName}|FieldList1]}]};
	_ ->
	    throw({error,Line,{?ERR_GENBADREC+B#tgd.eb,RName}})
    end;
    
tg(T,B) when is_tuple(T), size(T) >= 2 ->
    Element = element(1,T),
    Line = element(2,T),
    throw({error,Line,{?ERR_GENELEMENT+B#tgd.eb,
		       translate_language_element(Element)}}); 
tg(Other,B) ->
    Element = io_lib:format("unknown element ~w", [Other]),
    throw({error,unknown,{?ERR_GENELEMENT+B#tgd.eb,Element}}).

transform_head([V]) ->
    th(V,cre_bind()).

th({record,Line,RName,RFields},B) ->
    % youch...
    RDefs = get_records(),
    {KeyList,NewB} = lists:foldl(fun({record_field,_,{atom,_,Key},Value},
				     {L,B0}) ->
					 {NV,B1} = th(Value,B0),
					 {[{Key,NV}|L],B1};
				    (_,_) ->
					 throw({error,Line,{?ERR_HEADBADREC,
							    RName}})
				 end,
				 {[],B},
				 RFields),
    case lists:keysearch(RName,1,RDefs) of
	{value, {RName, FieldList0}} ->
	    FieldList1 = lists:foldl(
			   fun({FN,_,_},Acc) ->
				   El = case lists:keysearch(FN,1,KeyList) of
					    {value, {FN, X0}} ->
						X0;
					    _ ->
						{atom,Line,'_'}
					end,
				   [El | Acc]
			   end,
			   [],
			   FieldList0),
	    check_undef_field(RName,Line,KeyList,FieldList0,
			      ?ERR_HEADBADFIELD),
	    {{tuple,Line,[{atom,Line,RName}|FieldList1]},NewB};
	_ ->
	    throw({error,Line,{?ERR_HEADBADREC,RName}})
    end;

th({match,Line,_,_},_) -> 
    throw({error,Line,?ERR_HEADMATCH});
th({var,Line,Name},B) ->
    case lkup_bind(Name,B) of
	undefined ->
	    NewB = new_bind(Name,B),
	    {{atom,Line,lkup_bind(Name,NewB)},NewB};
	Trans ->
	    {{atom,Line,Trans},B}
    end;
th([H|T],B) ->
    {NH,NB} = th(H,B),
    {NT,NNB} = th(T,NB),
    {[NH|NT],NNB};
th(T,B) when is_tuple(T) ->
    {L,NB} = th(tuple_to_list(T),B),
    {list_to_tuple(L),NB};
th(Nonstruct,B) ->
    {Nonstruct,B}.
check_undef_field(_,_,[],_,_) ->
    ok;
check_undef_field(RName, Line, [{Key,_}|T], FieldList, ErrCode) ->
    case lists:keysearch(Key,1,FieldList) of
	{value,_} ->
	    check_undef_field(RName, Line, T, FieldList, ErrCode); 
	_ ->
	    throw({error,Line,{ErrCode,RName,Key}})
    end.

cre_bind() ->
    {1,[{'_','_'}]}.
lkup_bind(Name,{_,List}) ->
    case lists:keysearch(Name,1,List) of
	{value, {Name, Trans}} ->
	    Trans;
	_ ->
	    undefined
    end.
new_bind(Name,{Next,L}) ->
    Trans = list_to_atom([$$|integer_to_list(Next)]),
    {Next+1,[{Name,Trans}|L]}.

translate_language_element(Atom) ->
    Transtab = [
		{lc,"list comprehension"},
		{block, "begin/end block"},
		{'if', "if"},
		{'case', "case"},
		{'receive', "receive"},
		{'try', "try"},
		{'catch', "catch"},
		{'match', "match (=)"},
		{remote, "external function call"}
	       ],
    case lists:keysearch(Atom,1,Transtab) of
	{value,{Atom, String}} ->
	    String;
	_ ->
	    atom_to_list(Atom)
    end.

guard_function() ->
    BoolTest = [
		is_atom, 
		is_constant, 
		is_float, 
		is_integer, 
		is_list, 
		is_number, 
		is_pid, 
		is_port, 
		is_reference, 
		is_tuple, 
		is_binary, 
		is_function, 
		is_record, 
		is_seq_trace
	       ],
    BoolFunction = ['xor' | BoolTest], 
    GuardFunction = [
		     abs, 
		     element, 
		     hd, 
		     length, 
		     node, 
		     round, 
		     size, 
		     tl, 
		     trunc, 
		     self, 
		     get_tcw |
		     BoolFunction
		    ],
    GuardFunction.
		 
is_ms_function(X,body) ->
    ActionFunction = [
		      set_seq_token,
		      get_seq_token,
		      message,
		      return_trace,
		      process_dump,
		      enable_trace,
		      disable_trace,
		      display,
		      caller,
		      set_tcw,
		      silent |
		      guard_function()
		      ],
    lists:member(X,ActionFunction);

is_ms_function(X,guard) ->
    lists:member(X,guard_function()).



fixup_environment(L,B) when is_list(L) ->    
    lists:map(fun(X) ->
		      fixup_environment(X,B) 
	      end,
	      L);
fixup_environment({var,Line,Name},B) ->
    case lists:keysearch(Name,1,B) of
	{value,{Name,Value}} -> 
	    freeze(Line,Value);
	_ ->
	    throw({error,Line,{?ERR_UNBOUND_VARIABLE,atom_to_list(Name)}})
    end;
fixup_environment(T,B) when is_tuple(T) ->
    list_to_tuple(
      lists:map(fun(X) ->
			fixup_environment(X,B) 
		end,
		tuple_to_list(T)));
fixup_environment(Other,_B) ->
    Other.
    
freeze(Line,Term) ->
    {frozen,Line,Term}.

% Most of this is bluntly stolen from erl_parse.

normalise({frozen,_,Term}) ->
    Term;
normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F.

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].


% exprs([E0|Es]) ->
%     E1 = expr(E0),
%     [E1|exprs(Es)];
% exprs([]) -> [].

% %% -type expr(Expression) -> Expression.

% expr({var,Line,V}) -> {var,Line,V};
% expr({integer,Line,I}) -> {integer,Line,I};
% expr({float,Line,F}) -> {float,Line,F};
% expr({atom,Line,A}) -> {atom,Line,A};
% expr({string,Line,S}) -> {string,Line,S};
% expr({nil,Line}) -> {nil,Line};
% expr({cons,Line,H0,T0}) ->
%     H1 = expr(H0),
%     T1 = expr(T0),				%They see the same variables
%     {cons,Line,H1,T1};
% expr({lc,Line,E0,Qs0}) ->
%     Qs1 = lc_quals(Qs0),
%     E1 = expr(E0),
%     {lc,Line,E1,Qs1};
% expr({tuple,Line,Es0}) ->
%     Es1 = expr_list(Es0),
%     {tuple,Line,Es1};
% %%expr({struct,Line,Tag,Es0}) ->
% %%    Es1 = pattern_list(Es0),
% %%    {struct,Line,Tag,Es1};
% expr({record_index,Line,Name,Field0}) ->
%     Field1 = expr(Field0),
%     {record_index,Line,Name,Field1};
% expr({record,Line,Name,Inits0}) ->
%     Inits1 = record_inits_updates(Inits0),
%     {record,Line,Name,Inits1};
% expr({record_field,Line,Rec0,Name,Field0}) ->
%     Rec1 = expr(Rec0),
%     Field1 = expr(Field0),
%     {record_field,Line,Rec1,Name,Field1};
% expr({record,Line,Rec0,Name,Upds0}) ->
%     Rec1 = expr(Rec0),
%     Upds1 = record_inits_updates(Upds0),
%     {record,Line,Rec1,Name,Upds1};
% expr({record_field,Line,Rec0,Field0}) ->
%     Rec1 = expr(Rec0),
%     Field1 = expr(Field0),
%     {record_field,Line,Rec1,Field1};
% expr({block,Line,Es0}) ->
%     %% Unfold block into a sequence.
%     Es1 = exprs(Es0),
%     {block,Line,Es1};
% expr({'if',Line,Cs0}) ->
%     Cs1 = fun_icr_clauses(Cs0),
%     {'if',Line,Cs1};
% expr({'case',Line,E0,Cs0}) ->
%     E1 = expr(E0),
%     Cs1 = fun_icr_clauses(Cs0),
%     {'case',Line,E1,Cs1};
% expr({'receive',Line,Cs0}) ->
%     Cs1 = fun_icr_clauses(Cs0),
%     {'receive',Line,Cs1};
% expr({'receive',Line,Cs0,To0,ToEs0}) ->
%     To1 = expr(To0),
%     ToEs1 = exprs(ToEs0),
%     Cs1 = fun_icr_clauses(Cs0),
%     {'receive',Line,Cs1,To1,ToEs1};
% expr({'fun',Line,Body}) ->
%     case Body of
% 	{clauses,Cs0} ->
% 	    Cs1 = fun_icr_clauses(Cs0),
% 	    {'fun',Line,{clauses,Cs1}};
% 	{function,F,A} ->
% 	    {'fun',Line,{function,F,A}};
% 	{function,M,F,A} ->			%This is an error in lint!
% 	    {'fun',Line,{function,M,F,A}}
%     end;
% expr({call,Line,F0,As0}) ->
%     %% N.B. If F an atom then call to local function or BIF, if F a
%     %% remote structure (see below) then call to other module,
%     %% otherwise apply to "function".
%     F1 = expr(F0),
%     As1 = expr_list(As0),
%     {call,Line,F1,As1};
% expr({'catch',Line,E0}) ->
%     %% No new variables added.
%     E1 = expr(E0),
%     {'catch',Line,E1};
% expr({'query', Line, E0}) ->
%     %% lc expression
%     E = expr(E0),
%     {'query', Line, E};
% expr({match,Line,P0,E0}) ->
%     E1 = expr(E0),
%     P1 = pattern(P0),
%     {match,Line,P1,E1};
% expr({bin,Line,Fs}) ->
%     Fs2 = pattern_grp(Fs),
%     {bin,Line,Fs2};
% expr({op,Line,Op,A0}) ->
%     A1 = expr(A0),
%     {op,Line,Op,A1};
% expr({op,Line,Op,L0,R0}) ->
%     L1 = expr(L0),
%     R1 = expr(R0),				%They see the same variables
%     {op,Line,Op,L1,R1};
% %% The following are not allowed to occur anywhere!
% expr({remote,Line,M0,F0}) ->
%     M1 = expr(M0),
%     F1 = expr(F0),
%     {remote,Line,M1,F1}.


% expr_list([E0|Es]) ->
%     E1 = expr(E0),
%     [E1|expr_list(Es)];
% expr_list([]) -> [].


% record_inits_updates([{record_field,Lf,{atom,La,F},Val0}|Is]) ->
%     Val1 = expr(Val0),
%     [{record_field,Lf,{atom,La,F},Val1}|record_inits_updates(Is)];
% record_inits_updates([]) -> 
%     [].


% %% -type lc_quals([Qualifier]) -> [Qualifier].
% %%  Allow filters to be both guard tests and general expressions.

% lc_quals([{generate,Line,P0,E0}|Qs]) ->
%     E1 = expr(E0),
%     P1 = pattern(P0),
%     [{generate,Line,P1,E1}|lc_quals(Qs)];
% lc_quals([E0|Qs]) ->
%     E1 = expr(E0),
%     [E1|lc_quals(Qs)];
% lc_quals([]) -> [].

% %% -type fun_clauses([Clause]) -> [Clause].

% fun_icr_clauses([C0|Cs]) ->
%     C1 = clause(C0),
%     [C1|fun_icr_clauses(Cs)];
% fun_icr_clauses([]) -> [].



% %%
% %% -type patterns([Pattern]) -> [Pattern].
% %%  These patterns are processed "sequentially" for purposes of variable
% %%  definition etc.

% patterns([P0|Ps]) ->
%     P1 = pattern(P0),
%     [P1|patterns(Ps)];
% patterns([]) -> [].

% %% -type pattern(Pattern) -> Pattern.
% %%  N.B. Only valid patterns are included here.

% string_to_conses([], Line, Tail) ->
%     Tail;
% string_to_conses([E|Rest], Line, Tail) ->
%     {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

% pattern({var,Line,V}) -> {var,Line,V};
% pattern({match,Line,L0,R0}) ->
%     L1 = pattern(L0),
%     R1 = pattern(R0),
%     {match,Line,L1,R1};
% pattern({integer,Line,I}) -> {integer,Line,I};
% pattern({float,Line,F}) -> {float,Line,F};
% pattern({atom,Line,A}) -> {atom,Line,A};
% pattern({string,Line,S}) -> {string,Line,S};
% pattern({nil,Line}) -> {nil,Line};
% pattern({cons,Line,H0,T0}) ->
%     H1 = pattern(H0),
%     T1 = pattern(T0),
%     {cons,Line,H1,T1};
% pattern({tuple,Line,Ps0}) ->
%     Ps1 = pattern_list(Ps0),
%     {tuple,Line,Ps1};
% %%pattern({struct,Line,Tag,Ps0}) ->
% %%    Ps1 = pattern_list(Ps0),
% %%    {struct,Line,Tag,Ps1};
% pattern({record,Line,Name,Pfs0}) ->
%     Pfs1 = pattern_fields(Pfs0),
%     {record,Line,Name,Pfs1};
% %% record_field occurs in query expressions
% pattern({record_field,Line,Rec0,Field0}) ->
%     Rec1 = expr(Rec0),
%     Field1 = expr(Field0),
%     {record_field,Line,Rec1,Field1};
% pattern({bin,Line,Fs}) ->
%     Fs2 = pattern_grp(Fs),
%     {bin,Line,Fs2};
% pattern({op,Line,'++',{nil,_},R}) ->
%     pattern(R);
% pattern({op,Line,'++',{cons,Li,{integer,L2,I},T},R}) ->
%     pattern({cons,Li,{integer,L2,I},{op,Li,'++',T,R}});
% pattern({op,Line,'++',{string,Li,L},R}) ->
%     pattern(string_to_conses(L, Li, R));
% pattern({op,Line,Op,A}) ->
%     {op,Line,Op,A};
% pattern({op,Line,Op,L,R}) ->
%     {op,Line,Op,L,R}.

% pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
%     S2 = case S1 of
% 	     default ->
% 		 default;
% 	     _ ->
% 		 expr(S1)
% 	 end,
%     T2 = case T1 of
% 	     default ->
% 		 default;
% 	     _ ->
% 		 bit_types(T1)
% 	 end,
%     [{bin_element,L1,expr(E1),S2,T2} | pattern_grp(Fs)];
% pattern_grp([]) ->
%     [].

% bit_types([]) ->
%     [];
% bit_types([Atom | Rest]) when atom(Atom) ->
%     [Atom | bit_types(Rest)];
% bit_types([{Atom, Integer} | Rest]) when atom(Atom), integer(Integer) ->
%     [{Atom, Integer} | bit_types(Rest)].



% %% -type pattern_list([Pattern]) -> [Pattern].
% %%  These patterns are processed "in parallel" for purposes of variable
% %%  definition etc.

% pattern_list([P0|Ps]) ->
%     P1 = pattern(P0),
%     [P1|pattern_list(Ps)];
% pattern_list([]) -> [].

% %% -type pattern_fields([Field]) -> [Field].
% %%  N.B. Field names are full expressions here but only atoms are allowed
% %%  by the *linter*!.

% pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
%     P1 = pattern(P0),
%     [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
% pattern_fields([]) -> [].




