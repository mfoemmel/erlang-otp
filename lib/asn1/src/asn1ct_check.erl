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
-module(asn1ct_check).

%% Main Module for ASN.1 compile time functions

%-compile(export_all).
-export([check/2,storeindb/1]).
-include("asn1_records.hrl").

-record(newt,{type=unchanged,tag=unchanged,constraint=unchanged}). % used in check_type to update type and tag
-record(newv,{type=unchanged,value=unchanged}). % used in check_value to update type and value
 
check(S,{Types,Values,ParameterizedTypes}) ->
    Terror= checkt(S,Types,[]),
    Verror= checkv(S,Values,[]),
    case {Terror,Verror} of
	{[],[]} -> ok;
	L ->{error,L}
    end.

checkt(S,[Name|T],Acc) ->
    %io:format("check_typedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    error({type,{internal_error,'???'},S});
	Type when record(Type,typedef) ->
	    NewS = S#state{type=Type,tname=Name},
	    case catch(check_type(NewS,Type,Type#typedef.typespec)) of
		{error,Reason} ->
		    error({type,Reason,NewS});
		{'EXIT',Reason} ->
		    error({type,{internal_error,Reason},NewS});
		Ts ->
		    NewType = Type#typedef{typespec = Ts},
		    asn1_db:dbput(NewS#state.mname,Name,NewType), % update the type
		    ok
	    end
	     end,
    case Result of
	ok ->
	    checkt(S,T,Acc);
	_ ->
	    checkt(S,T,[Result|Acc])
    end;
checkt(S,[],Acc) ->
    lists:reverse(Acc).

checkv(S,[Name|T],Acc) ->
						%io:format("check_valuedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
		 undefined -> error({value,{internal_error,'???'},S});
		 Value when record(Value,valuedef) ->
		     NewS = S#state{value=Value},
		     case catch(check_value(NewS,Value)) of
			 {error,Reason} ->
			     error({type,Reason,NewS});
			 {'EXIT',Reason} ->
			     error({type,{internal_error,Reason},NewS});
			 V ->
			     %% update the valuedef
			     asn1_db:dbput(NewS#state.mname,Name,V), 
			     ok
		     end
	     end,
    case Result of
	ok ->
	    checkv(S,T,Acc);
	_ ->
	    checkv(S,T,[Result|Acc])
    end;
checkv(Module,[],Acc) ->
    lists:reverse(Acc).

check_value(OldS,V) ->
    #valuedef{name=Name,type=Vtype,value=Value} = V,
    case Value of
	{'ASN1_OK',_} -> 
	    V;
	{error,_} ->
	    V;
	_ ->
	    Def = Vtype#type.def,
	    Constr = Vtype#type.constraint,
	    S = OldS#state{type=Vtype,tname=Def,value=V,vname=Name},
	    NewDef = 
		case Def of
		    Tref when record(Tref,typereference) ->
			Type = get_referenced_type(S,Tref),
			check_value(S,V#valuedef{type=Type#typedef.typespec}),
			#newv{};
		    Ext when record(Ext,'Externaltypereference') ->
			Type = get_referenced_type(S,Ext),
			check_value(S,V#valuedef{type=Type#typedef.typespec}),
			#newv{};
		    'ANY' ->
			throw({error,{asn1,{'cant check value of type',Def}}});
		    'INTEGER' ->
			#newv{value=validate_integer(S,Value,[],Constr)};
		    {'INTEGER',NamedNumberList} ->
			#newv{value=validate_integer(S,Value,NamedNumberList,Constr)};
		    {'BIT STRING',NamedNumberList} ->
			#newv{value=validate_bitstring(S,Value,NamedNumberList,Constr)};
		    'NULL' ->
			validate_null(S,Value,Constr),
			#newv{};
		    'OBJECT IDENTIFIER' ->
			#newv{value = validate_objectidentifier(S,Value,Constr)};
		    'ObjectDescriptor' ->
			validate_objectdescriptor(S,Value,Constr),
			#newv{};
		    {'ENUMERATED',NamedNumberList} ->
			#newv{value=validate_enumerated(S,Value,NamedNumberList,Constr)};
		    'BOOLEAN'->
			#newv{value=validate_boolean(S,Value,Constr)};
		    'OCTET STRING' ->
			#newv{value=validate_octetstring(S,Value,Constr)};
		    'NumericString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'TeletexString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'VideotexString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'UTCTime' ->
			exit({'cant check value of type' ,Def});
		    'GeneralizedTime' ->
			exit({'cant check value of type' ,Def});
		    'GraphicString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'VisibleString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'GeneralString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'PrintableString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'IA5String' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    'BMPString' ->
			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    {'SEQUENCE',Components} ->
			#newv{value=validate_sequence(S,Value,Components,Constr)};
		    {'SEQUENCE OF',Components} ->
			#newv{value=validate_sequenceof(S,Value,Components,Constr)};
		    {'CHOICE',Components} ->
			#newv{value=validate_choice(S,Value,Components,Constr)};
		    {'SET',Components} ->
			#newv{value=validate_set(S,Value,Components,Constr)};
		    {'SET OF',Components} ->
			#newv{value=validate_setof(S,Value,Components,Constr)};
		    Other ->
			exit({'cant check value of type' ,Other})
		end,
	    case NewDef#newv.value of
		unchanged ->
		    V#valuedef{value={'ASN1_OK',Value}};
		ok ->
		    V#valuedef{value={'ASN1_OK',Value}};
		{error,Reason} ->
		    V#valuedef{value={error,Reason}};
		_V ->
		    V#valuedef{value={'ASN1_OK',_V}}
	    end
    end.

validate_integer(S,{identifier,Pos,Id},NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown NamedNumber",S})
    end;
validate_integer(S,Value,NamedNumberList,Constr) when integer(Value) ->
    check_integer_range(Value,Constr).

check_integer_range(Int,Constr) when list(Constr) ->
    NewConstr = [X || #constraint{c=X} <- Constr],
    check_constr(Int,NewConstr);

check_integer_range(Int,Constr) ->
    %%io:format("~p~n",[Constr]),
    ok.

check_constr(Int,[{'ValueRange',Lb,Ub}|T]) when Int >= Lb, Int =< Ub ->
    check_constr(Int,T);
check_constr(Int,[]) ->
    ok.

validate_bitstring(S,Value,NamedNumberList,Constr) ->
    ok.

validate_null(S,'NULL',Constr) ->
    ok.


is_space_list([H],Acc) ->
    lists:reverse([H|Acc]);
is_space_list([H,space|T],Acc) ->
    is_space_list(T,[H|Acc]);
is_space_list([],Acc) ->
    lists:reverse(Acc);
is_space_list([H|T],_) ->
    {error,H}.

validate_objectidentifier(S,L,_) ->
    case is_space_list(L,[]) of
	NewL when list(NewL) ->
	    case validate_objectidentifier1(S,NewL) of
		NewL2 when list(NewL2) ->
		    list_to_tuple(NewL2);
		Other -> Other
	    end;
	{error,Where} ->
	    error({value, "illegal OBJECT IDENTIFIER", S})
    end.

validate_objectidentifier1(S, [Id|T]) when record(Id,identifier) ->
    case catch get_referenced_type(S,Id) of
	V when record(V,valuedef) -> 
	    case NewV = check_value(S,V) of
		#valuedef{type=#type{def='OBJECT IDENTIFIER'},
			  value={'ASN1_OK',Value}} when tuple(Value) ->
		    validate_objectid(S, T, lists:reverse(tuple_to_list(Value)));
		_ -> 
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end;
	Error ->
	    validate_objectid(S, [Id|T], [])
    end;
validate_objectidentifier1(S,V) ->
    validate_objectid(S,V,[]).

validate_objectid(_, [], Acc) ->
    lists:reverse(Acc);
validate_objectid(S, [Value|Vrest], Acc) when integer(Value) ->
    validate_objectid(S, Vrest, [Value|Acc]);
validate_objectid(S, [{'NamedNumber',_Name,Value}|Vrest], Acc) 
  when integer(Value) ->
    validate_objectid(S, Vrest, [Value|Acc]);
validate_objectid(S, [Id|Vrest], Acc) 
  when record(Id,identifier) ->
    case catch get_referenced_type(S, Id) of
	V when record(V,valuedef) -> 
	    case NewV = check_value(S, V) of
		#valuedef{value={'ASN1_OK',Value}} when integer(Value) ->
		    validate_objectid(S, Vrest, [Value|Acc]);
		_ -> 
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end;
	Error ->
	    case reserved_objectid(Id#identifier.val, Acc) of
		Value when integer(Value) ->
		    validate_objectid(S, Vrest, [Value|Acc]);
		false ->
		    error({value, "illegal OBJECT IDENTIFIER", S})
	    end
    end;
validate_objectid(S, V, Acc) ->
    error({value, "illegal OBJECT IDENTIFIER",S}).


%% ITU-T Rec. X.680 Annex B - D
reserved_objectid('itu-t',[]) -> 0;
reserved_objectid('ccitt',[]) -> 0;
%% arcs below "itu-t" 
reserved_objectid('recommendation',[0]) -> 0;
reserved_objectid('question',[0]) -> 1;
reserved_objectid('administration',[0]) -> 2;
reserved_objectid('network-operator',[0]) -> 3;
reserved_objectid('identified-organization',[0]) -> 4;

reserved_objectid(iso,0) -> 1;
%% arcs below "iso", note that number 1 is not used
reserved_objectid('standard',[1]) -> 0;
reserved_objectid('member-body',[1]) -> 2;
reserved_objectid('identified-organization',[1]) -> 3;

reserved_objectid('joint-iso-itu-t',0) -> 2;
reserved_objectid('joint-iso-ccitt',0) -> 2;

reserved_objectid(_,_) -> false.


		 
	    

validate_objectdescriptor(S,Value,Constr) ->
    ok.

validate_enumerated(S,{identifier,Pos,Id},NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end.

validate_boolean(S,Value,Constr) ->
    ok.

validate_octetstring(S,Value,Constr) ->
    ok.

validate_restrictedstring(S,Value,Def,Constr) ->
    ok.

validate_sequence(S,Value,Components,Constr) ->
    ok.

validate_sequenceof(S,Value,Components,Constr) ->
    ok.

validate_choice(S,Value,Components,Constr) ->
    ok.

validate_set(S,Value,Components,Constr) ->
    ok.

validate_setof(S,Value,Components,Constr) ->
    ok.

check_type(S,Type,{'CLASS',Fields,Syntax}) ->
    {'CLASS',Fields,Syntax};
check_type(S,Type,Ts) when record(Ts,type) ->
    Tag = Ts#type.tag,
    Def = match_parameters(Ts#type.def,S#state.parameters),
    Constr = Ts#type.constraint,
    TestFun = 
	fun(Tref) ->
		MaybeChoice = get_referenced_type(S,Tref),
		case (MaybeChoice#typedef.typespec)#type.def of
		    {'CHOICE',_} ->
			case Tag of
			    #tag{type='IMPLICIT'} ->
				throw({error,{asn1,{implicit_tag_before_choice}}});
			    #tag{type={default,_}} ->
				Tag#tag{type='EXPLICIT'}; % X.680 28.6 c
			    _ ->
				unchanged
			end;
		    _ ->
			unchanged
		end
	end,
    NewDef= 
	case Def of 
	    Tref when record(Tref,typereference) ->
		Ct = TestFun(Tref),
		#newt{type=check_typereference(S,Tref),tag=Ct};
	    Ext when record(Ext,'Externaltypereference') ->
		Ct = TestFun(Ext),
		#newt{type=check_externaltypereference(S,Ext),tag=Ct};
	    'ANY' ->
		exit({'cant check',Def});
	    'INTEGER' ->
		check_integer(S,[],Constr),
		#newt{};
	    {'INTEGER',NamedNumberList} ->
		#newt{type={'INTEGER',check_integer(S,NamedNumberList,Constr)}};
	    {'BIT STRING',NamedNumberList} ->
		#newt{type={'BIT STRING',check_bitstring(S,NamedNumberList,Constr)}};
	    'NULL' ->
		#newt{};
	    'OBJECT IDENTIFIER' ->
		check_objectidentifier(S,Constr),
		#newt{};
	    'ObjectDescriptor' ->
		#newt{};
	    {'ENUMERATED',NamedNumberList} ->
		#newt{type={'ENUMERATED',check_enumerated(S,NamedNumberList,Constr)}};
	    'BOOLEAN'->
		check_boolean(S,Constr),
		#newt{};
	    'OCTET STRING' ->
		check_octetstring(S,Constr),
		#newt{};
	    'NumericString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'TeletexString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'VideotexString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'UTCTime' ->
		#newt{};
	    'GeneralizedTime' ->
		#newt{};
	    'GraphicString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'VisibleString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'GeneralString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'PrintableString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'IA5String' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'BMPString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    'UniversalString' ->
		check_restrictedstring(S,Def,Constr),
		#newt{};
	    {'SEQUENCE',Components} ->
		#newt{type={'SEQUENCE',check_sequence(S,Type,Components)}};
	    {'SEQUENCE OF',Components} ->
		#newt{type={'SEQUENCE OF',check_sequenceof(S,Type,Components)}};
	    {'CHOICE',Components} ->
		Ct = case Tag of
			 #tag{type='IMPLICIT'} ->
			     throw({error,{asn1,{implicit_tag_before_choice}}});
			 #tag{type={default,_}} -> 
			     Tag#tag{type='EXPLICIT'}; % X.680 28.6 c
			 _ -> 
			     unchanged
		end,
		#newt{type={'CHOICE',check_choice(S,Type,Components)},tag=Ct};
	    {'SET',Components} ->
		#newt{type={'SET',check_set(S,Type,Components)}};
	    {'SET OF',Components} ->
		#newt{type={'SET OF',check_setof(S,Type,Components)}};
	    {{typereference,_,'TYPE-IDENTIFIER'},{typefieldreference,_,'Type'}} ->
		#newt{type='ASN1_OPEN_TYPE'};
	    {pt,Ptype,ParaList} ->
		Instance = instantiate_ptype(S,Ptype,ParaList),
		#newt{type=Instance#type.def,
		      constraint=Instance#type.constraint};
	    Other ->
		exit({'cant check' ,Other})
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts;
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    T3 = case NewDef of
	#newt{tag=unchanged} ->
	    case Tag of
		#tag{type={default,TT}} ->
		    Ts2#type{tag=(Tag#tag{type=TT})};
		_ ->
		    Ts2
	    end;
	#newt{tag=TT} ->
	    Ts2#type{tag=TT}
    end,
    T4 = case NewDef of
	     #newt{constraint=unchanged} ->
		 T3#type{constraint=Constr};
	     #newt{constraint=NewConstr} -> 
		 T3#type{constraint=NewConstr}
	 end,
    T4#type{constraint=check_constraints(S,T4#type.constraint)}.

% fix me
instantiate_ptype(S,Ptype,ParaList) ->
    Ptypedef = get_referenced_type(S, Ptype),
    #ptypedef{args=Args,typespec=Type} = Ptypedef,
    MatchedArgs = match_args(Args, ParaList, []),
    NewS = S#state{type=Type,parameters=MatchedArgs},
    check_type(NewS, Ptypedef, Type).

match_args([FormArg|Ft], [ActArg|At], Acc) ->
    match_args(Ft, At, [{FormArg,ActArg}|Acc]);
match_args([], [], Acc) ->
    lists:reverse(Acc);
match_args(_, _, _) ->
    throw({error,{asn1,{wrong_number_of_arguments}}}).
    
check_constraints(S,C) when list(C) -> 
    check_constraints(S, C, []);
check_constraints(S,C) when record(C,constraint) -> 
    check_constraints(S, C#constraint.c, []).


%%%-----------------------------------------
%% If the constraint value is a defined value the valuename
%% is replaced by the actual value
%% 
resolv_value(S,{identifier,Pos,Name}) ->
    case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    case imported(S,Name) of 
		{ok,Imodule} ->
		    V2 = get_referenced(S,Imodule,Name),
		    V2#valuedef.value;
		_  ->
		    throw({error,{asn1,{undefined_type_or_value,Name}}})
	    end;
	Val ->
	    Val#valuedef.value
    end;
resolv_value(_,V) ->
    V.

check_constraints(S,[C | Rest], Acc) ->
    check_constraints(S,Rest,[check_constraint(S,C)| Acc]);
check_constraints(S,[],Acc) ->
    Acc.

check_constraint(S,{'SingleValue', V}) ->
    {'SingleValue',resolv_value(S,V)};

check_constraint(S,{'ValueRange', {Lb, Ub}}) ->
    {'ValueRange',{resolv_value(S,Lb),resolv_value(S,Ub)}};

% else keep the constraint unchanged
check_constraint(S,Any) ->
    Any.


check_imported(S,Imodule,Name) ->
    case asn1_db:dbget(Imodule,'MODULE') of
	undefined ->
	    io:format("~s.asn1db not found~n",[Imodule]),
	    io:format("Type ~s imported from non existing module ~s~n",[Name,Imodule]);
	Im when record(Im,module) ->
	    case is_exported(Im,Name) of
		false ->
		    io:format("Imported type ~s not exported from module ~s~n",[Name,Imodule]);
		_ ->
		    ok
	    end
    end,
    ok.

is_exported(Module,Name) when record(Module,module) ->
    {exports,Exports} = Module#module.exports,
    case Exports of
	all ->
	    true;
	[] ->
	    false;
	L when list(L) -> 
	    case lists:keysearch(Name,3,Exports) of
		false -> false;
		_ -> true
	    end
    end.
    
check_externaltypereference(S,Etref) when record(Etref,'Externaltypereference') ->
    Emod = Etref#'Externaltypereference'.module,
    Etype = Etref#'Externaltypereference'.type,
    Pos = Etref#'Externaltypereference'.pos,
    Currmod = S#state.mname,
    case Emod of
	Currmod ->
	    %% reference to current module
	    check_typereference(S,#typereference{val=Etype,pos=Pos});
	 _ ->
	    %% io:format("Type ~s IMPORTED FROM ~s~n",[Etype,Emod]),
	    Etref
    end.

check_typereference(S,Tref) when record(Tref,typereference) ->
    Name = Tref#typereference.val,
    case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    case imported(S,Name) of
		{ok,Imodule} ->
		    %% io:format("Type ~s IMPORTED FROM ~s~n",[Name,Imodule]),
		    check_imported(S,Imodule,Name),
		    #'Externaltypereference'{module=Imodule,type=Name};
		_ ->
		    io:format("Type ~s is not defined~n",[Name]),
		    Tref
	    end;
	_ ->
	    Tref    
    end.


get_referenced_type(S,Ext) when record(Ext,'Externaltypereference') ->
    case match_parameters(Ext, S#state.parameters) of
	Ext -> 
	    #'Externaltypereference'{pos=Pos,module=Emod,type=Etype} = Ext,
	    case S#state.mname of
		Emod -> % a local reference in this module
		    get_referenced1(S,Etype,Pos);
		_ ->
		    get_referenced(S,Emod,Etype)
	    end;
	#typereference{val=Name,pos=Pos} ->
	    get_referenced1(S,Name,Pos);
	Other ->
	    Other
    end;
get_referenced_type(S,Tref) when record(Tref,typereference) ->
    case match_parameters(Tref, S#state.parameters) of
	Tref -> 
	    #typereference{val=Name,pos=Pos} = Tref,
	    get_referenced1(S,Name,Pos);
	#typereference{val=Name,pos=Pos} ->
	    get_referenced1(S,Name,Pos);
	Other ->
	    Other
    end;
get_referenced_type(S,#'Externalvaluereference'{module=Emod,value=Eval}) ->
    get_referenced(S,Emod,Eval);
get_referenced_type(S,#identifier{val=Name,pos=Pos}) ->
    get_referenced1(S,Name,Pos);
get_referenced_type(S,Type) ->
    Type.


get_referenced(S,Emod,Ename) ->
    case asn1_db:dbget(Emod,Ename) of
	undefined ->
	    throw({error,{asn1,{undefined_type_or_value,{Emod,Ename}}}});
	T when record(T,typedef) ->
	    Spec = T#typedef.typespec,
	    case Spec#type.def of
		Tref when record(Tref,typereference) ->
		    Def = #'Externaltypereference'{module=Emod,
					     type=Tref#typereference.val,
					     pos=Tref#typereference.pos},
		    
		    
		    T#typedef{typespec=Spec#type{def=Def}};
		_ ->
		    T % should add check that T is exported here
	    end;
	V -> V
    end.

get_referenced1(S,Name,Pos) ->
    case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    case imported(S,Name) of
		{ok,Imodule} ->
		    case asn1_db:dbget(Imodule,'MODULE') of
			undefined ->
			    throw({error,{asn1,{module_not_found,Imodule}}});
			Im when record(Im,module) ->
			    case is_exported(Im,Name) of
				false ->
				    throw({error,
					   {asn1,{not_exported,{Im,Name}}}});
				_ ->
				    get_referenced_type(S,
							#'Externaltypereference'
							{module=Imodule,
							 type=Name,pos=Pos})
			    end
		    end;
		_ ->
		    throw({error,{asn1,{undefined_type,Name}}})
	    end;
	T ->
	    T
    end.

match_parameters(Name,[]) ->
    Name;

match_parameters(#'Externaltypereference'{type=Name},[{#typereference{val=Name},NewName}|T]) ->
    NewName;
match_parameters(#typereference{val=Name},[{#typereference{val=Name},NewName}|T]) ->
    NewName;
match_parameters(#identifier{val=Name},[{#identifier{val=Name},NewName}|T]) ->
    NewName;
match_parameters(Name, [H|T]) ->
    match_parameters(Name,T).

imported(S,Name) ->
    {imports,Ilist} = (S#state.module)#module.imports,
    imported1(Name,Ilist).

imported1(Name,
	  [{'SymbolsFromModule',Symlist,
	    {typereference,_,ModuleName}}|T]) ->
    case lists:keysearch(Name,3,Symlist) of
	{value,V} ->
	    {ok,ModuleName};
	_ ->
	    imported1(Name,T)
    end;
imported1(Name,[]) ->
    false.
	

check_integer(S,[],C) ->
    ok;
check_integer(S,NamedNumberList,C) ->
    case check_unique(NamedNumberList,2) of
	[] -> 
	    check_int(S,NamedNumberList,[]);
	L when list(L) ->
	    error({type,{duplicates,L},S}),
	    unchanged
		   
    end.

check_int(S,[{'NamedNumber',Id,Num}|T],Acc) when integer(Num) ->
    check_int(S,T,[{Id,Num}|Acc]);
check_int(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_int(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_int(S,[],Acc) ->
    lists:keysort(2,Acc).


check_bitstring(S,NamedNumberList,Constr) ->
    NamedNumberList.
    


%% Check ENUMERATED
%% ****************************************
%% Check that all values are unique
%% assign values to un-numbered identifiers
%% check that the constraints are allowed and correct
%% put the updated info back into database
check_enumerated(S,[{Name,Number}|Rest],Constr) when atom(Name), integer(Number)->
    %% already checked , just return the same list
    [{Name,Number}|Rest]; 
check_enumerated(S,NamedNumberList,Constr) ->
    check_enum(S,NamedNumberList,[],[]).

%% identifiers are put in Acc2
%% returns either [{Name,Number}] or {[{Name,Number}],[{ExtName,ExtNumber}]}
%% the latter is returned if the ENUMERATION contains EXTENSIONMARK
check_enum(S,[{'NamedNumber',Id,Num}|T],Acc1,Acc2) when integer(Num) ->
    check_enum(S,T,[{Id,Num}|Acc1],Acc2);
check_enum(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc1,Acc2) ->
    Val = dbget_ex(S,S#state.mname,Name),
    check_enum(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc1,Acc2);
check_enum(S,['EXTENSIONMARK'|T],Acc1,Acc2) ->
    NewAcc2 = lists:keysort(2,Acc1),
    NewList = enum_number(Acc2,NewAcc2,0,[]),
    { NewList, check_enum(S,T,[],[])};
check_enum(S,[Id|T],Acc1,Acc2) when atom(Id) ->
    check_enum(S,T,Acc1,[Id|Acc2]);
check_enum(S,[],Acc1,Acc2) ->
    NewAcc2 = lists:keysort(2,Acc1),
    enum_number(Acc2,NewAcc2,0,[]).


% assign numbers to identifiers , numbers from 0 ... but must not
% be the same as already assigned to NamedNumbers
enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num > Cnt ->
    enum_number(T,[{Id,Num}|T2],Cnt+1,[{H,Cnt}|Acc]);
enum_number([H|T],[{Id,Num}|T2],Cnt,Acc) when Num < Cnt -> % negative Num
    enum_number(T,T2,Cnt+1,[{H,Cnt},{Id,Num}|Acc]);
enum_number([],L2,Cnt,Acc) ->
    lists:concat([lists:reverse(Acc),L2]);
enum_number(L,[{Id,Num}|T2],Cnt,Acc) -> % Num == Cnt
    enum_number(L,T2,Cnt+1,[{Id,Num}|Acc]);
enum_number([H|T],[],Cnt,Acc) ->
    enum_number(T,[],Cnt+1,[{H,Cnt}|Acc]).
    

check_boolean(S,Constr) ->
    ok.

check_octetstring(S,Constr) ->
    ok.

% check all aspects of a SEQUENCE
% - that all component names are unique
% - that all TAGS are ok (when TAG default is applied)
% - that each component is of a valid type
% - that the extension marks are valid

check_sequence(S,Type,Comps)  ->
    Components = expand_components(S,Comps),    
    case check_unique([C||C <- Components ,record(C,'ComponentType')]
		      ,#'ComponentType'.name) of
	[] ->
	    %%    sort_canonical(Components),
	    case check_each_component(S,Type,Components) of
		NewComponents when list(NewComponents) ->
		    check_unique_sequence_tags(S,NewComponents),
		    NewComponents;
		{NewComponents,NewEcomps} ->
		    check_unique_sequence_tags(S,NewEcomps),
		    {NewComponents, NewEcomps}
	    end;
	Dupl ->
		throw({error,{asn1,{duplicate_components,Dupl}}})
    end.

expand_components(S, [{'COMPONENTS OF',Type}|T]) ->
    CompList = case get_referenced_type(S,Type#type.def) of
		   #typedef{typespec=#type{def={'SEQUENCE',{Root,Ext}}}} -> Root;
		   #typedef{typespec=#type{def={'SEQUENCE',Root}}} -> Root;
		   Err -> throw({error,{asn1,{illegal_COMPONENTS_OF,Err}}})
	       end,
    expand_components(S,CompList) ++ expand_components(S,T);
expand_components(S,[H|T]) ->
    [H|expand_components(S,T)];
expand_components(_,[]) ->
    [].
    
check_unique_sequence_tags(S,[#'ComponentType'{prop=mandatory}|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(S,[C|Rest]) when record(C,'ComponentType') ->
    check_unique_sequence_tags1(S,Rest,[C]);% optional or default
check_unique_sequence_tags(S,[ExtensionMarker|Rest]) ->
    check_unique_sequence_tags(S,Rest);
check_unique_sequence_tags(S,[]) ->
    true.

check_unique_sequence_tags1(S,[C|Rest],Acc) when record(C,'ComponentType') ->
    case C#'ComponentType'.prop of
	mandatory ->
	    check_unique_tags(S,lists:reverse([C|Acc])),
	    check_unique_sequence_tags(S,Rest);
	_  ->
	    check_unique_sequence_tags1(S,Rest,[C|Acc]) % default or optional
    end;
check_unique_sequence_tags1(S,[H|Rest],Acc) ->
    check_unique_sequence_tags1(S,Rest,[H|Acc]);
check_unique_sequence_tags1(S,[],Acc) ->
    check_unique_tags(S,lists:reverse(Acc)).

check_sequenceof(S,Type,Component) when record(Component,type) ->
    check_type(S,Type,Component).

check_set(S,Type,Components) ->
    check_sequence(S,Type,Components).

check_setof(S,Type,Component) when record(Component,type) ->
    check_type(S,Type,Component).

check_restrictedstring(S,Def,Constr) ->
    ok.

check_objectidentifier(S,Constr) ->
    ok.

% check all aspects of a CHOICE
% - that all alternative names are unique
% - that all TAGS are ok (when TAG default is applied)
% - that each alternative is of a valid type
% - that the extension marks are valid
check_choice(S,Type,Components) when list(Components) ->
    case check_unique([C||C <- Components,
			  record(C,'ComponentType')],#'ComponentType'.name) of
	[] -> 
    %%    sort_canonical(Components),
	    case check_each_alternative(S,Type,Components) of
		{NewComponents,NewEcomps} ->
		    check_unique_tags(S,NewComponents ++ NewEcomps),
		    {NewComponents,NewEcomps};
		NewComponents ->
		    check_unique_tags(S,NewComponents),
		    NewComponents
	    end;
	Dupl ->
	    throw({error,{asn1,{duplicate_choice_alternatives,Dupl}}})
    end;
check_choice(S,Type,[]) -> 
    [].

check_unique_tags(S,C) ->
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    true;
	_ ->
	    collect_and_sort_tags(C,[])
    end.

collect_and_sort_tags([C|Rest],Acc) when record(C,'ComponentType') ->
    collect_and_sort_tags(Rest,C#'ComponentType'.tags ++ Acc);
collect_and_sort_tags([H|Rest],Acc) ->
    collect_and_sort_tags(Rest,Acc);
collect_and_sort_tags([],Acc) ->
    {Dupl,_}= lists:mapfoldl(fun(El,El)->{{dup,El},El};(El,Prev)-> {El,El} end,notag,lists:sort(Acc)),
    Dupl2 = [Dup|| {dup,Dup} <- Dupl],
    if 
	length(Dupl2) > 0 ->
	    throw({error,{asn1,{duplicates_of_the_tags,Dupl2}}});
	true ->
	    true
    end.

check_unique(L,Pos) ->
    Slist = lists:keysort(Pos,L),
    check_unique2(Slist,Pos,[]).

check_unique2([A,B|T],Pos,Acc) when element(Pos,A) == element(Pos,B) ->
    check_unique2([B|T],Pos,[element(Pos,B)|Acc]);
check_unique2([H|T],Pos,Acc) ->
    check_unique2(T,Pos,Acc);
check_unique2([],Pos,Acc) ->
    lists:reverse(Acc).

check_each_component(S,Type,{Rlist,ExtList}) ->
    {check_each_component(S,Type,Rlist),
     check_each_component(S,Type,ExtList)};
check_each_component(S,Type,Components) ->
    check_each_component(S,Type,Components,[],[],noext).

check_each_component(S,Type,[C|Ct],Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
    NewTags = get_taglist(S,Ts),
    NewC = C#'ComponentType'{typespec=check_type(S,Type,Ts),tags=NewTags},
    case Ext of
	noext ->
	    check_each_component(S,Type,Ct,[NewC|Acc],Extacc,Ext);
	ext ->
	    check_each_component(S,Type,Ct,Acc,[NewC|Extacc],Ext)
    end;
check_each_component(S,Type,[C|Ct],Acc,Extacc,noext) -> % skip 'EXTENSIONMARK'
    check_each_component(S,Type,Ct,Acc,Extacc,ext);
check_each_component(S,Type,[C|Ct],Acc,Extacc,ext) -> % skip 'EXTENSIONMARK'
    throw({error,{asn1,{too_many_extension_marks}}});
check_each_component(S,Type,[],Acc,Extacc,ext) ->
    {lists:reverse(Acc),lists:reverse(Extacc)};
check_each_component(S,Type,[],Acc,Extacc,noext) ->
    lists:reverse(Acc).

check_each_alternative(S,Type,{Rlist,ExtList}) ->
    {check_each_alternative(S,Type,Rlist),
     check_each_alternative(S,Type,ExtList)};
check_each_alternative(S,Type,[C|Ct]) ->
    check_each_alternative(S,Type,[C|Ct],[],[],noext).

check_each_alternative(S,Type,[C|Ct],Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
    NewTags = get_taglist(S,Ts),
    NewC = C#'ComponentType'{typespec=check_type(S,Type,Ts),tags=NewTags},
    case Ext of
	noext ->
	    check_each_alternative(S,Type,Ct,[NewC|Acc],Extacc,Ext);
	ext ->
	    check_each_alternative(S,Type,Ct,Acc,[NewC|Extacc],Ext)
    end;
	    
check_each_alternative(S,Type,[C|Ct],Acc,Extacc,noext) -> % skip 'EXTENSIONMARK'
    check_each_alternative(S,Type,Ct,Acc,Extacc,ext);
check_each_alternative(S,Type,[C|Ct],Acc,Extacc,ext) -> % skip 'EXTENSIONMARK'
    throw({error,{asn1,{too_many_extension_marks}}});
check_each_alternative(S,Type,[],Acc,Extacc,ext) ->
    {lists:reverse(Acc),lists:reverse(Extacc)};
check_each_alternative(S,Type,[],Acc,Extacc,noext) ->
    lists:reverse(Acc).

get_taglist(#state{erule=per},_) ->
    [];
get_taglist(S,Ext) when record(Ext,'Externaltypereference') ->
    T = get_referenced_type(S,Ext),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Tref) when record(Tref,typereference) ->
    T = get_referenced_type(S,Tref),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Type) when record(Type,type) ->
    case Type#type.tag of
	undefined ->
	    get_taglist(S,Type#type.def);
	Tag  ->
	    [asn1ct_gen:def_to_tag(Tag)]
    end;
get_taglist(S,{'CHOICE',Components}) ->
    get_taglist1(S,Components);
get_taglist(S,Def) ->
    [asn1ct_gen:def_to_tag(Def)].

get_taglist1(S,[#'ComponentType'{name=Cname,typespec=Ts,tags=TagL}|Rest]) when list(TagL) -> 
    %% tag_list has been here , just return TagL and continue with next alternative
    TagL ++ get_taglist1(S,Rest);
get_taglist1(S,[#'ComponentType'{name=Cname,typespec=Ts,tags=undefined}|Rest]) ->
    get_taglist(S,Ts) ++ get_taglist1(S,Rest);
get_taglist1(S,[H|Rest]) -> % skip EXTENSIONMARK
    get_taglist1(S,Rest);
get_taglist1(S,[]) ->
    [].


dbget_ex(S,Module,Key) ->
    case asn1_db:dbget(Module,Key) of
	undefined ->
	    
	    throw({error,{asn1,{undefined,{Module,Key}}}}); % this is catched on toplevel type or value
	T -> T
    end.


storeindb(M) when record(M,module) ->
    TVlist = M#module.typeorval,
    NewM = M#module{typeorval=findtypes_and_values(TVlist,[],[],[])},
    asn1_db:dbnew(NewM#module.name),
    asn1_db:dbput(NewM#module.name,'MODULE',  NewM),
    storeindb(NewM#module.name,TVlist,[]).

storeindb(Module,[H|T],ErrAcc) when record(H,typedef) ->
    storeindb(Module,H#typedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,valuedef) ->
    storeindb(Module,H#valuedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,ptypedef) ->
    storeindb(Module,H#ptypedef.name,H,T,ErrAcc);
storeindb(_,[],[]) -> ok;
storeindb(_,[],ErrAcc) -> 
    {error,ErrAcc}.

storeindb(Module,Name,H,T,ErrAcc) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    asn1_db:dbput(Module,Name,H),
	    storeindb(Module,T,ErrAcc);
	OldName -> 
	    case H of 
		Type when record(H,typedef) ->
		    error({type,"already defined",#state{mname=Module,type=H,tname=Name}});
		Type when record(H,valuedef) ->
		    error({value,"already defined",#state{mname=Module,value=H,vname=Name}});
		Type when record(H,ptypedef) ->
		    error({ptype,"already defined",#state{mname=Module,value=H,vname=Name}})
	    end,
	    storeindb(Module,T,[H|ErrAcc])
    end.

findtypes_and_values([H|T],Tacc,Vacc,Pacc) when record(H,typedef) ->
    findtypes_and_values(T,[H#typedef.name|Tacc],Vacc,Pacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc) when record(H,valuedef) ->
    findtypes_and_values(T,Tacc,[H#valuedef.name|Vacc],Pacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc) when record(H,ptypedef) ->
    findtypes_and_values(T,Tacc,Vacc,[H#ptypedef.name|Pacc]);
findtypes_and_values([],Tacc,Vacc,Pacc) ->
    {lists:reverse(Tacc),lists:reverse(Vacc),lists:reverse(Pacc)}.
    

error({type,Msg,#state{mname=Mname,type=Type,tname=Typename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Type#typedef.pos,Mname,Typename,Msg]),
    {error,{type,Type#typedef.pos,Mname,Typename,Msg}};
error({type,Msg,#state{mname=Mname,value=Value,vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{type,Value#valuedef.pos,Mname,Valuename,Msg}};
error({value,Msg,#state{mname=Mname,value=Value,vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{value,Value#valuedef.pos,Mname,Valuename,Msg}};
error({Other,Msg,#state{mname=Mname,value=Value,vname=Valuename}}) ->
    io:format("asn1error:~p:~p:~p ~p~n",[Value#valuedef.pos,Mname,Valuename,Msg]),
    {error,{Other,Value#valuedef.pos,Mname,Valuename,Msg}}.
	    











