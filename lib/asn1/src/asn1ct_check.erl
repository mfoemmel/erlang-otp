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
 
check(S,{Types,Values,ParameterizedTypes,Classes,Objects,ObjectSets}) ->
    TupleIs = fun(X,T) -> case X of 
			    {T,_} ->
				true;
			    _ ->
				false
			end
	    end,
    IsClass = fun(X) -> TupleIs(X,asn1_class) end,
    IsPObjSet = fun(X) -> TupleIs(X,pobjectsetdef) end,
    Perror = checkp(S,ParameterizedTypes,[]), % must do this before the templates are used
    Terror = checkt(S,Types,[]),
    Verror = checkv(S,Values,[]),
    
    %% get information object classes wrongly sent to checkt/3
    ClNameTuples = lists:filter(IsClass,Terror),
    AddClasses = lists:map(fun(X)->element(2,X) end,ClNameTuples),
    %% remove faults due to classes from Terror
    NewTerror = lists:subtract(Terror,ClNameTuples),
    NewClasses = Classes++AddClasses,
    Cerror = checkc(S,NewClasses,[]),

    %% get object sets incorrectly sent to checkv/3
    ObjSetNameTuples = lists:filter(IsPObjSet,Verror),
    ObjSetNames = lists:map(fun(X)->element(2,X) end,ObjSetNameTuples),
    %% remove faults due to objects in Verror
    NewVerror = lists:subtract(Verror,ObjSetNameTuples),
    {Oerror,ExclO,ExclOS} = checko(S,Objects++ObjectSets++ObjSetNames,
				   [],[],[]),
    Exporterror = check_exports(S,S#state.module),
    case {NewTerror,NewVerror,Cerror,Oerror,Exporterror} of
	{[],[],[],[],[]} -> 
	    NewTypes = lists:subtract(Types,AddClasses),
	    {ok,
	     {NewTypes,Values,ParameterizedTypes,
	      NewClasses,Objects,ObjectSets},
	     {NewTypes,Values,ParameterizedTypes,NewClasses,
	      lists:subtract(Objects,ExclO),lists:subtract(ObjectSets,ExclOS)}};
	L ->{error,{asn1,L}}
    end.

check_exports(S,Module = #module{name=MName}) ->
    case Module#module.exports of
	{exports,[]} ->
	    [];
	{exports,all} ->
	    [];
	{exports,ExportList} when list(ExportList) ->
	    IsNotDefined = 
		fun(X) ->
			case catch get_referenced_type(S,X) of
			    {error,{asn1,Err}} ->
				true;
			    _ -> false
			end
		end,
	    case lists:filter(IsNotDefined,ExportList) of
		[] ->
		    [];
		NoDefExp ->
		    GetName =
			fun(#'Externaltypereference'{type=N})-> 
				{exported,undefined,entity,N} end,
		    lists:map(GetName,NoDefExp)
	    end
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
		{asn1_class,ClassDef} ->
		    {asn1_class,Name};
		Ts ->
		    NewType = Type#typedef{checked=true,typespec = Ts},
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
    %%io:format("check_valuedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
		 undefined -> error({value,{internal_error,'???'},S});
		 Value when record(Value,valuedef);
			    record(Value,pvaluesetdef) ->
		     NewS = S#state{value=Value},
		     case catch(check_value(NewS,Value)) of
			 {error,Reason} ->
			     error({type,Reason,NewS});
			 {'EXIT',Reason} ->
			     error({type,{internal_error,Reason},NewS});
			 {pobjectsetdef} ->
			     {pobjectsetdef,Name};
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
checkv(S,[],Acc) ->
    lists:reverse(Acc).


checkp(S,[Name|T],Acc) ->
    %io:format("check_ptypedef:~p~n",[Name]),
    Result = case asn1_db:dbget(S#state.mname,Name) of
	undefined ->
	    error({type,{internal_error,'???'},S});
	Type when record(Type,ptypedef) ->
	    NewS = S#state{type=Type,tname=Name},
	    case catch(check_ptype(NewS,Type,Type#ptypedef.typespec)) of
		{error,Reason} ->
		    error({type,Reason,NewS});
		{'EXIT',Reason} ->
		    error({type,{internal_error,Reason},NewS});
		{asn1_class,ClassDef} ->
		    {asn1_class,Name};
		Ts ->
		    NewType = Type#ptypedef{checked=true,typespec = Ts},
		    asn1_db:dbput(NewS#state.mname,Name,NewType), % update the type
		    ok
	    end
	     end,
    case Result of
	ok ->
	    checkp(S,T,Acc);
	_ ->
	    checkp(S,T,[Result|Acc])
    end;
checkp(S,[],Acc) ->
    lists:reverse(Acc).




checkc(S,[Name|Cs],Acc) ->
    Result = 
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({class,{internal_error,'???'},S});
	    Class  ->
		ClassSpec = if
			       record(Class,classdef) ->
				   Class#classdef.typespec;
			       record(Class,typedef) ->
				   Class#typedef.typespec
			   end,
		NewS = S#state{type=Class,tname=Name},
		case catch(check_class(NewS,ClassSpec)) of
		    {error,Reason} ->
			error({class,Reason,NewS});
		    {'EXIT',Reason} ->
			error({class,{internal_error,Reason},NewS});
		    C ->
			%% update the classdef
			NewClass = 
			    if
				record(Class,classdef) ->
				    Class#classdef{checked=true,typespec=C};
				record(Class,typedef) ->
				    #classdef{checked=true,name=Name,typespec=C}
			    end,
			asn1_db:dbput(NewS#state.mname,Name,NewClass),
			ok
		end
	end,
    case Result of
	ok ->
	    checkc(S,Cs,Acc);
	_ ->
	    checkc(S,Cs,[Result|Acc])
    end;
checkc(S,[],Acc) ->
%%    include_default_class(S#state.mname),
    lists:reverse(Acc).
    
checko(S,[Name|Os],Acc,ExclO,ExclOS) ->
    Result = 
	case asn1_db:dbget(S#state.mname,Name) of
	    undefined ->
		error({type,{internal_error,'???'},S});
	    Object when record(Object,typedef) ->
		NewS = S#state{type=Object,tname=Name},
		case catch(check_object(NewS,Object,Object#typedef.typespec)) of
		    {error,Reason} ->
			error({type,Reason,NewS});
		    {'EXIT',Reason} ->
			error({type,{internal_error,Reason},NewS});
		    {asn1,Reason} ->
			error({type,Reason,NewS});
		    O ->
			NewObj = Object#typedef{checked=true,typespec=O},
			asn1_db:dbput(NewS#state.mname,Name,NewObj),
			if
			    record(O,'Object') ->
				case O#'Object'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,[Name|ExclO],ExclOS}
				end;
			    record(O,'ObjectSet') ->
				case O#'ObjectSet'.gen of
				    true ->
					{ok,ExclO,ExclOS};
				    false ->
					{ok,ExclO,[Name|ExclOS]}
				end
			end
		end
	end,
    case Result of
	{ok,NewExclO,NewExclOS} ->
	    checko(S,Os,Acc,NewExclO,NewExclOS);
	_ ->
	    checko(S,Os,[Result|Acc],ExclO,ExclOS)
    end;
checko(S,[],Acc,ExclO,ExclOS) ->
    {lists:reverse(Acc),lists:reverse(ExclO),lists:reverse(ExclOS)}.

check_class(S,ClassSpec) when record(ClassSpec,type) ->
    Def = ClassSpec#type.def,
    case Def of
	Tref when record(Tref,typereference);
		  record(Tref,'Externaltypereference') ->
	    {_,RefType} = get_referenced_type(S,Tref),
	    case RefType of
		RefClass when record(RefClass,classdef) ->
		    NewClassSpec = check_class(S,RefClass#classdef.typespec)
	    end
    end;
check_class(S,{objectclassname,ModuleName,ClassName}) when atom(ModuleName),atom(ClassName) ->
    'fix this';
check_class(S,C) when record(C,objectclass) ->
    NewFieldSpec = check_class_fields(S,C#objectclass.fields),
    C#objectclass{fields=NewFieldSpec};
check_class(S,{objectclassname,ClassName}) ->
    {ClassRef,Def} = get_referenced_class(S,ClassName),
    case Def of
	ClassDef when record(ClassDef,classdef) ->
	    case ClassDef#classdef.checked of
		true ->
		    ClassDef#classdef.typespec;
		false ->
		    NewClassSpec = check_class(S,ClassDef#classdef.typespec)
	    end
    end;
check_class(S,{poc,ObjSet,Params}) ->
    'fix this later'.

check_class_fields(S,Fields) ->
    check_class_fields(S,Fields,[]).

check_class_fields(S,[F|Fields],Acc) ->
    NewField = 
	case element(1,F) of
	    fixedtypevaluefield ->
		{_,Name,Type,Unique,OSpec} = F,
		RefType = check_type(S,#typedef{typespec=Type},Type),
		{fixedtypevaluefield,Name,RefType,Unique,OSpec};
%%		    case Type#type.def of
%%			Def when record(Def,typereference);
%%				 record(Def,'Externaltypereference') ->
%%			    get_referenced_type(S,Def);
%%			Other ->
%%			    get_referenced_type(S,#typereference{val=Other})
%%		    end,
%%		case RefType#typedef.checked of
%%		    true -> 
%%			{fixedtypevaluefield,Name,RefType#typedef.typespec,
%%			 Unique,OSpec};
%%		    false ->
%%			{fixedtypevaluefield,Name,
%%			 check_type(S,RefType,RefType#typedef.typespec),
%%			 Unique,OSpec}
%%		end;
	    object_or_fixedtypevalue_field ->
		{_,Name,Type,Unique,OSpec} = F,
		Cat = 
		    case asn1ct_gen:type(asn1ct_gen:get_inner(Type#type.def)) of
			Def when record(Def,typereference);
				 record(Def,'Externaltypereference') ->
			    {_,D} = get_referenced_type(S,Def),
			    D;
			{undefined,user} ->
			    {_,D} = get_referenced_type(S,#typereference{val=Type#type.def}),
			    D;
			Other ->
			    Type
		    end,
		case Cat of
		    Class when record(Class,classdef) ->
			{objectfield,Name,Type,Unique,OSpec};
		    _ ->
			RefType = check_type(S,#typedef{typespec=Type},Type),	
			{fixedtypevaluefield,Name,RefType,Unique,OSpec}
%%			case RefType#typedef.checked of
%%			    true ->
%%				{fixedtypevaluefield,Name,
%%				 RefType#typedef.typespec,
%%				 Unique,OSpec};
%%			    false ->
%%				{fixedtypevaluefield,Name,
%%				 check_type(S,RefType,
%%					    RefType#typedef.typespec),
%%				 Unique,OSpec}
%%			end
		end;
	    objectset_or_fixedtypevalueset_field ->
		{_,Name,Type,OSpec} = F,
		RefType = check_type(S,#typedef{typespec=Type},Type),
%%		    case Type#type.def of
%%			Def when record(Def,typereference);
%%				 record(Def,'Externaltypereference') ->
%%			    get_referenced_type(S,Def);
%%			Other ->
%%			    get_referenced_type(S,#typereference{val=Other})
%%		    end,
		if
		    record(RefType,classdef) ->
			{objectsetfield,Name,Type,OSpec};
		    true ->
			{fixedtypevaluesetfield,Name,RefType,OSpec}
%%			case RefType#typedef.checked of
%%			    true ->
%%				{fixedtypevaluesetfield,Name,
%%				 RefType#typedef.typespec,OSpec};
%%			    false ->
%%				{fixedtypevaluesetfield,Name,
%%				 RefType#typedef.typespec,OSpec}
%%			end
		end;
	    _ -> F
	end,
    check_class_fields(S,Fields,[NewField|Acc]);
check_class_fields(S,[],Acc) ->
    lists:reverse(Acc).

check_object(S,ObjDef,ObjSpec) when (ObjDef#typedef.checked == true) ->
    ObjSpec;
check_object(S,ObjDef,
	     #'Object'{classname={objectclassname,ClassRef},def=ObjectDef}) ->
    {_,ClassDef} = get_referenced_class(S,ClassRef),
    NewObj =
	case ObjectDef of
	    Def when tuple(Def), (element(1,Def)==object) ->
		NewSettingList = check_objectdefn(S,Def,ClassDef),
		#'Object'{classname=ClassRef,def=NewSettingList};
	    Def when tuple(Def), (element(1,Def)=='ObjectFromObject') ->
		fixa;
	    Def when tuple(Def), (element(1,Def)==po) ->
		fixa;
	    _  ->
		exit({error,{no_object,ObjectDef},S})
	end,
    Gen = gen_incl(NewObj#'Object'.def,
		   (ClassDef#classdef.typespec)#objectclass.fields),
    NewObj#'Object'{gen=Gen};

check_object(S,
	     ObjSetDef,
	     ObjSet=#'ObjectSet'{class={objectclassname,ClassRef}}) ->
    ClassInf = get_referenced_class(S,ClassRef),
    {ClassName,ClassDef} = ClassInf,
    UniqueFieldName = get_unique_fieldname(ClassDef),
    NewObjSet=
	case ObjSet#'ObjectSet'.set of
	    {'SingleValue',Set} when list(Set) ->
		CheckedSet = check_object_list(S,ClassRef,Set),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet};
	    {'SingleValue',Object={definedvalue,ObjName}} ->
		{_,ObjDef} = get_referenced_type(S,#identifier{val=ObjName}),
		#'Object'{def=CheckedObj} = 
		    check_object(S,ObjDef,ObjDef#typedef.typespec),
		NewSet = get_unique_valuelist(S,[{ObjName,CheckedObj}],
					      UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet};
	    {'SingleValue',Object=#'Externalvaluereference'{value=ObjName}} ->
		{_,ObjDef} = get_referenced_type(S,#identifier{val=ObjName}),
		#'Object'{def=CheckedObj} = 
		    check_object(S,ObjDef,ObjDef#typedef.typespec),
		NewSet = get_unique_valuelist(S,[{ObjName,CheckedObj}],
					      UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet};
	    ['EXTENSIONMARK'] ->
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=['EXTENSIONMARK']};
	    Set when list(Set) ->
		CheckedSet = check_object_list(S,ClassRef,Set),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet};
	    {Set,Ext} when list(Set) ->
		CheckedSet = check_object_list(S,ClassRef,Set++Ext),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet++['EXTENSIONMARK']};
	    {{'SingleValue',Set},Ext} when list(Set) ->
		CheckedSet = check_object_list(S,ClassRef,Set++Ext),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet++['EXTENSIONMARK']};
	    {{'SingleValue',Object},Ext} ->
		CheckedSet = check_object_list(S,ClassRef,
					       [{'SingleValue',Object}|Ext]),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet++['EXTENSIONMARK']};
	    {Type,{'EXCEPT',Exclusion}} when record(Type,type) ->
		{_,TDef} = get_referenced_type(S,Type#type.def),
		OS = TDef#typedef.typespec,
		NewSet = reduce_objectset(OS#'ObjectSet'.set,Exclusion),
		NewOS = OS#'ObjectSet'{set=NewSet},
		CheckedSet = check_object(S,TDef#typedef{typespec=NewOS},
					  NewOS);
	    {ObjDef={object,definedsyntax,ObjFields},Ext} ->
		CheckedSet = check_object_list(S,ClassRef,[ObjDef]),
		NewSet = get_unique_valuelist(S,CheckedSet,UniqueFieldName),
		#'ObjectSet'{class=ClassInf,
			     uniquefname=UniqueFieldName,
			     set=NewSet++['EXTENSIONMARK']}
	end,
    Gen = gen_incl_set(NewObjSet#'ObjectSet'.set,
		       (ClassDef#classdef.typespec)#objectclass.fields),
    NewObjSet#'ObjectSet'{gen=Gen}.

reduce_objectset(ObjectSet,Exclusion) ->
    case Exclusion of
	{'SingleValue',#'Externalvaluereference'{value=Name}} ->
	    case lists:keysearch(Name,1,ObjectSet) of
		{value,El} ->
		    lists:subtract(ObjectSet,[El]);
		_ ->
		    ObjectSet
	    end
    end.
	    
%% Checks a list of objects or object sets and returns a list of selected
%% information for the code generation.
check_object_list(S,ClassRef,ObjectList) ->
    check_object_list(S,ClassRef,ObjectList,[]).

check_object_list(S,ClassRef,[ObjOrSet|Objs],Acc) ->
    case ObjOrSet of
	ObjDef when tuple(ObjDef),(element(1,ObjDef)==object) ->
	    Def = 
		check_object(S,#typedef{typespec=ObjDef},
			     #'Object'{classname={objectclassname,ClassRef},
				       def=ObjDef}),
	    check_object_list(S,ClassRef,Objs,[{no_name,Def#'Object'.def}|Acc]);
	{'SingleValue',{definedvalue,ObjName}} ->
	    {_,ObjectDef} = get_referenced_type(S,#identifier{val=ObjName}),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,[{ObjName,Def}|Acc]);
	{'SingleValue',Ref = #'Externalvaluereference'{value=ObjName}} ->
	    {_,ObjectDef} = get_referenced_type(S,Ref),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,[{ObjName,Def}|Acc]);
	ObjRef when record(ObjRef,'Externalvaluereference') ->
	    {_,ObjectDef} = get_referenced_type(S,ObjRef),
	    #'Object'{def=Def} = check_object(S,ObjectDef,ObjectDef#typedef.typespec),
	    check_object_list(S,ClassRef,Objs,
			      [{ObjRef#'Externalvaluereference'.value,Def}|Acc]);
	ObjSet when record(ObjSet,type) ->
	    ObjSetDef = 
		case ObjSet#type.def of
		    Ref when record(Ref,typereference);
			     record(Ref,'Externaltypereference') ->
			{_,D} = get_referenced_type(S,ObjSet#type.def),
			D;
		    Other ->
			throw({asn1_error,{'unknown objecset',Other,S}})
		end,
	    #'ObjectSet'{set=ObjectsInSet} = 
		check_object(S,ObjSetDef,ObjSetDef#typedef.typespec),
	    AccList = transform_set_to_object_list(ObjectsInSet,[]),
	    check_object_list(S,ClassRef,Objs,AccList++Acc);
	Other ->
	    exit({error,{'unknown object',Other},S})
    end;
%% Finally reverse the accumulated list and if there are any extension
%% marks in the object set put one indicator of that in the end of the
%% list.
check_object_list(_,_,[],Acc) ->
    RevAcc = lists:reverse(Acc).
%%    case lists:member('EXTENSINMARK',RevAcc) of
%%	true ->
%%	    ExclRevAcc = lists:filter(fun(X)->X /= 'EXTENSIONMARK' end,
%%				      RevAcc),
%%	    ExclRevAcc ++ ['EXTENSIONMARK'];
%%	false ->
%%	    RevAcc
%%    end.
	    

transform_set_to_object_list([{Name,UVal,Fields}|Objs],Acc) ->
    transform_set_to_object_list(Objs,[{Name,{object,generatesyntax,Fields}}|Acc]);
transform_set_to_object_list(['EXTENSIONMARK'|Objs],Acc) ->
%%    transform_set_to_object_list(Objs,['EXTENSIONMARK'|Acc]);
    transform_set_to_object_list(Objs,Acc);
transform_set_to_object_list([],Acc) ->
    Acc.

get_unique_valuelist(S,ObjSet,UFN) ->
    get_unique_vlist(S,ObjSet,UFN,[]).

get_unique_vlist(S,[],_,Acc) ->
    case catch check_uniqueness(Acc) of
	{asn1_error,Reason} ->
%	    exit({error,Reason,S});
	    error({'ObjectSet',"not unique objects in object set",S});
	true ->
	    lists:reverse(Acc)
    end;
get_unique_vlist(S,[{ObjName,Obj}|Rest],UniqueFieldName,Acc) ->
    {_,_,Fields} = Obj,
    VDef = get_unique_value(S,Fields,UniqueFieldName),
    get_unique_vlist(S,Rest,UniqueFieldName,
		     [{ObjName,VDef#valuedef.value,Fields}|Acc]).

get_unique_value(S,Fields,UniqueFieldName) ->
    Module = S#state.mname,
    case lists:keysearch(UniqueFieldName,1,Fields) of
	{value,Field} ->
	    case element(2,Field) of
		VDef when record(VDef,valuedef) ->
		    VDef;
		{definedvalue,ValName} ->
		    ValueDef = asn1_db:dbget(Module,ValName),
		    case ValueDef of
			VDef when record(VDef,valuedef) ->
			    ValueDef;
			undefined -> 
			    #valuedef{value=ValName}
		    end;
		{'ValueFromObject',Object,Name} ->
		    case Object of
			{object,Ext} when record(Ext,'Externaltypereference') ->
			    OtherModule = Ext#'Externaltypereference'.module,
			    ExtObjName = Ext#'Externaltypereference'.type,
			    ObjDef = asn1_db:dbget(OtherModule,ExtObjName),
			    ObjSpec = ObjDef#typedef.typespec,
			    get_unique_value(OtherModule,element(3,ObjSpec),Name);
			{object,{_,_,ObjName}} ->
			    ObjDef = asn1_db:dbget(Module,ObjName),
			    ObjSpec = ObjDef#typedef.typespec,
			    get_unique_value(Module,element(3,ObjSpec),Name);
			{po,Object,Params} ->
			    exit({error,{'parameterized object not implemented yet',
					 Object},S})
		    end;
		Value when atom(Value);number(Value) ->
		    #valuedef{value=Value};
		{'CHOICE',{_,Value}} when atom(Value);number(Value) ->
		    #valuedef{value=Value}
	    end;
	false ->
	    exit({error,{'no unique value',Fields,UniqueFieldName},S})
    end.

check_uniqueness(NameValueList) ->
    check_uniqueness1(lists:keysort(2,NameValueList)).

check_uniqueness1([]) ->
    true;
check_uniqueness1([T]) ->
    true;
check_uniqueness1([{_,N,_},{_,N,_}|Rest]) ->
    throw({asn1_error,{'objects in set must have unique values in UNIQUE fields',N}});
check_uniqueness1([H|Rest]) ->
    check_uniqueness1(Rest).

%% gen_incl -> boolean()
%% If object with Fields has any of the corresponding class' typefields
%% then return value is true otherwise it is false.
gen_incl({_,_,Fields},CFields)->
    gen_incl1(Fields,CFields).

gen_incl1(_,[]) ->
    false;
gen_incl1(Fields,[C|CFields]) ->
    case element(1,C) of
	typefield ->
	    case lists:keymember(element(2,C),1,Fields) of
		true ->
		    true;
		false ->
		    gen_incl1(Fields,CFields)
	    end;
	_ -> 
	    gen_incl1(Fields,CFields)
    end.

gen_incl_set([],CFields)->
    false;
gen_incl_set(['EXTENSIONMARK'],_) ->
    true;
gen_incl_set([{_,_,Fields}|Rest],CFields)->
    case gen_incl1(Fields,CFields) of
	true ->
	    true;
	false ->
	    gen_incl_set(Rest,CFields)
    end.

check_objectdefn(S,Def,CDef) when record(CDef,classdef) ->
    WithSyntax = (CDef#classdef.typespec)#objectclass.syntax,
    ClassFields = (CDef#classdef.typespec)#objectclass.fields,
    case Def of
	{object,defaultsyntax,Fields} ->
	    check_defaultfields(S,Fields,ClassFields);
	{object,definedsyntax,Fields} ->
	    {_,WSSpec} = WithSyntax,
	    NewFields = 
		case catch( convert_definedsyntax(S,Fields,WSSpec,
						  ClassFields,[])) of
		    {asn1,{ErrorType,ObjToken,ClassToken}} ->
			throw({asn1,{'match error in object',ObjToken,
				     'found in object',ClassToken,'found in class'}});
		    Err={asn1,Reason} -> throw(Err);
		    Err={'EXIT',Reason} -> throw(Err);
		    DefaultFields when list(DefaultFields) ->
			DefaultFields
		end,
	    {object,defaultsyntax,NewFields};
	{object,ObjectId} -> % This is a DefinedObject
	    fixa;
	Other ->
	    exit({error,{objectdefn,Other}})
    end.

check_defaultfields(S,Fields,ClassFields) ->
    check_defaultfields(S,Fields,ClassFields,[]).

check_defaultfields(S,[],ClassFields,Acc) ->
    {object,defaultsyntax,lists:reverse(Acc)};
check_defaultfields(S,[{FName,Spec}|Fields],ClassFields,Acc) ->
    case lists:keysearch(FName,2,ClassFields) of
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,FName,Spec,CField),
	    check_defaultfields(S,Fields,ClassFields,[NewField|Acc]);
	_ ->
	    throw({error,{asn1,{'unvalid field in object',FName}}})
    end.
%%    {object,defaultsyntax,Fields}.

convert_definedsyntax(S,[],[],ClassFields,Acc) ->
    lists:reverse(Acc);
convert_definedsyntax(S,Fields,WithSyntax,ClassFields,Acc) ->
    case match_field(S,Fields,WithSyntax,ClassFields) of
	{MatchedField,RestFields,RestWS} ->
	    if
		list(MatchedField) ->
		    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
					  lists:append(MatchedField,Acc));
		true ->
		    convert_definedsyntax(S,RestFields,RestWS,ClassFields,
					  [MatchedField|Acc])
	    end
%%	    throw({error,{asn1,{'unvalid syntax in object',WorS}}})
    end.

match_field(S,Fields,WithSyntax,ClassFields) ->
    match_field(S,Fields,WithSyntax,ClassFields,[]).

match_field(S,Fields,[W|Ws],ClassFields,Acc) when list(W) ->
    case catch(match_optional_field(S,Fields,W,ClassFields,[])) of
	{'EXIT',Reason} ->
	    match_field(Fields,Ws,ClassFields,Acc); %% add S
	{[Result],RestFields} ->
	    {Result,RestFields,Ws};
	Other ->
	    match_field(S,Fields,Ws,ClassFields,Acc)
    end;
match_field(S,Fields,WithSyntax,ClassFields,Acc) ->
    match_mandatory_field(S,Fields,WithSyntax,ClassFields,[]).

match_optional_field(S,RestFields,[],_,Ret) ->
    {Ret,RestFields};
match_optional_field(S,Fields,[W|Ws],ClassFields,Ret) when list(W) ->
    {OptionalField,RestFields} = match_optional_field(S,Fields,W,ClassFields,[]),
    match_optional_field(S,RestFields,Ws,ClassFields,lists:append(OptionalField,Ret));
%% identify and skip word
%match_optional_field(S,[#'Externaltypereference'{type=WorS}|Rest],
match_optional_field(S,[{_,_,WorS}|Rest],
		     [WorS|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
%% identify and skip comma
match_optional_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_optional_field(S,Rest,Ws,ClassFields,Ret);
%% identify and save field data
match_optional_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Ret) ->
    WorS =
	case Setting of
	    Type when record(Type,type) -> Type;
	    #'Externalvaluereference'{value=WordOrSetting} -> WordOrSetting;
	    {_,_,WordOrSetting} -> WordOrSetting;
	    Atom when atom(Atom) -> Atom
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{optional_matcherror,WorS,W}});
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,W,WorS,CField),
	    match_optional_field(S,Rest,Ws,ClassFields,[NewField|Ret])
    end;
match_optional_field(S,[WorS|Rest],[W|Ws],ClassFields,Ret) ->
    throw({asn1,{optional_matcherror,WorS,W}}).

match_mandatory_field(S,[],[],_,[Acc]) ->
    {Acc,[],[]};
match_mandatory_field(S,[],[],_,Acc) ->
    {Acc,[],[]};
match_mandatory_field(S,[],[H|T],CF,Acc) when list(H) ->
    match_mandatory_field(S,[],T,CF,Acc);
match_mandatory_field(S,[],WithSyntax,_,Acc) ->
    throw({asn1,{mandatory_matcherror,[],WithSyntax}});
match_mandatory_field(S,Fields,WithSyntax=[W|Ws],ClassFields,[Acc]) when list(W) ->
    {Acc,Fields,WithSyntax};
%% identify and skip word
%match_mandatory_field(S,[#'Externaltypereference'{type=WorS}|Rest],
match_mandatory_field(S,[{_,_,WorS}|Rest],
		      [WorS|Ws],ClassFields,Acc) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Acc);
%% identify and skip comma
match_mandatory_field(S,[{WorS,_}|Rest],[{WorS,_}|Ws],ClassFields,Ret) ->
    match_mandatory_field(S,Rest,Ws,ClassFields,Ret);
%% identify and save field data
% match_mandatory_field(S,[#'Externalvaluereference'{value=WorS}|Rest],
% %match_mandatory_field(S,[{_,_,WorS}|Rest],
% 		      [{_,W}|Ws],ClassFields,Acc) ->
%     case lists:keysearch(W,2,ClassFields) of
% 	false ->
% 	    throw({asn1,{mandatory_matcherror,WorS,W}});
% 	{value,CField} ->
% 	    NewField = convert_to_defaultfield(S,W,WorS,CField),
% 	    match_mandatory_field(S,Rest,Ws,ClassFields,[NewField|Acc])
%     end;
% match_mandatory_field(S,[WorS|Rest],[{typefieldreference,W}|Ws],
% 		      ClassFields,Acc) when record(WorS,type)->
%     case lists:keysearch(W,2,ClassFields) of
% 	false ->
% 	    throw({asn1,{mandatory_matcherror,WorS,W}});
% 	{value,CField} ->
% 	    NewField = convert_to_defaultfield(S,W,WorS,CField),
%     	    match_mandatory_field(S,Rest,Ws,ClassFields,[NewField|Acc])
%     end;
% match_mandatory_field(S,[WorS|Rest],[{valuefieldreference,W}|Ws],
% 		      ClassFields,Acc) ->
%     case lists:keysearch(W,2,ClassFields) of
% 	false ->
% 	    throw({asn1,{mandatory_matcherror,WorS,W}});
% 	{value,CField} ->
% 	    NewField = convert_to_defaultfield(S,W,WorS,CField),
% 	    match_mandatory_field(S,Rest,Ws,ClassFields,[NewField|Acc])
%     end;
match_mandatory_field(S,[Setting|Rest],[{_,W}|Ws],ClassFields,Acc) ->
    WorS = 
	case Setting of
	    Atom when atom(Atom) -> Atom;
	    #'Externalvaluereference'{value=WordOrSetting} -> WordOrSetting;
	    {_,_,WordOrSetting} -> WordOrSetting;
	    Type when record(Type,type) -> Type;
	    Other -> throw({asn1,{mandatory_matcherror,Other,W}})
	end,
    case lists:keysearch(W,2,ClassFields) of
	false ->
	    throw({asn1,{mandatory_matcherror,WorS,W}});
	{value,CField} ->
	    NewField = convert_to_defaultfield(S,W,WorS,CField),
	    match_mandatory_field(S,Rest,Ws,ClassFields,[NewField|Acc])
    end;
 	    
match_mandatory_field(S,[WorS|Rest],[W|Ws],ClassFields,Acc) ->
    throw({asn1,{mandatory_matcherror,WorS,W}}).


convert_to_defaultfield(S,ObjFieldName,ObjFieldSetting,CField)->
    CurrMod = S#state.mname,
    case element(1,CField) of
	typefield ->
	    TypeDef=
		case ObjFieldSetting of
		    TypeRec when record(TypeRec,type) -> TypeRec#type.def;
		    _ -> ObjFieldSetting
		end,
	    Type =
		case asn1ct_gen:type(TypeDef) of
		    ERef = #'Externaltypereference'{module=CurrMod} ->
			{_,T} = get_referenced_type(S,ERef),
			T#typedef{checked=true,
				  typespec=check_type(S,T,
						      T#typedef.typespec)};
		    ERef = #'Externaltypereference'{module=ExtMod} ->
			{_,T} = get_referenced_type(S,ERef),
			#typedef{name=Name} = T,
			check_type(S,T,T#typedef.typespec),
			#typedef{checked=true,
				 name={ExtMod,Name},
				 typespec=ERef};
% 		    Ref when record(Ref,typereference);
% 			     record(Ref,'Externaltypereference')->
% 			get_referenced_type(S,Ref);
		    Bif when Bif=={primitive,bif};Bif=={constructed,bif} ->
			T = check_type(S,#typedef{typespec=ObjFieldSetting},
				       ObjFieldSetting),
			#typedef{checked=true,name=Bif,typespec=T};
		    _ ->
			{Mod,T} = 
			    get_referenced_type(S,#typereference{
						  val=ObjFieldSetting}),
			case Mod of
			    CurrMod ->
				T;
			    ExtMod ->
				#typedef{name=Name} = T,
				T#typedef{name={ExtMod,Name}}
			end
		end,
% 	    {ObjFieldName,
% 	     Type#typedef{checked=true,
% 			  typespec=check_type(S,Type,Type#typedef.typespec)}};
	    {ObjFieldName,Type};
	fixedtypevaluefield ->
	    case ObjFieldName of
		Val when atom(Val) ->
		    %% ObjFieldSetting can be a value,an objectidentifiervalue, an element in an enumeration or namednumberlist etc.
		    ValRef =
			case ObjFieldSetting of
			    #'Externalvaluereference'{} -> ObjFieldSetting;
			    _ ->
				#identifier{val=ObjFieldSetting}
			end,
		    ValDef =
			case catch get_referenced_type(S,ValRef) of
			    {error,_} ->
				check_value(S,#valuedef{name=Val,
							type=element(3,CField),
							value=ObjFieldSetting});
			    {_,VDef} when record(VDef,valuedef) ->
				check_value(S,VDef)%% XXX

			    end,
%%		    {ObjFieldName,{definedvalue,ObjFieldSetting}};
		    {ObjFieldName,ValDef};
		Val ->
		    {ObjFieldName,Val}
	    end;
	fixedtypevaluesetfield ->
	    {ObjFieldName,ObjFieldSetting};
	objectfield ->
	    ObjectSpec = 
		case ObjFieldSetting of
		    Ref when record(Ref,typereference);record(Ref,identifier);
			     record(Ref,'Externaltypereference');
			     record(Ref,'Externalvaluereference') ->
			{_,R} = get_referenced_type(S,ObjFieldSetting),
			R;
		    _ ->
			{_,R} = get_referenced_type(S,#typereference{val=ObjFieldSetting}),
			R
		end,
	    {ObjFieldName,
	     ObjectSpec#typedef{checked=true,
		      typespec=check_object(S,ObjectSpec,ObjectSpec#typedef.typespec)}};
	variabletypevaluefield ->
	    {ObjFieldName,ObjFieldSetting};
	variabletypevaluesetfield ->
	    {ObjFieldName,ObjFieldSetting};
	objectsetfield ->
	    {_,ObjSetSpec} = 
		case ObjFieldSetting of
		    Ref when record(Ref,typereference);
			     record(Ref,identifier);
			     record(Ref,'Externaltypereference');
			     record(Ref,'Externalvaluereference') ->
			get_referenced_type(S,ObjFieldSetting);
		    _ ->
			get_referenced_type(S,#typereference{val=ObjFieldSetting})
		end,
	    {ObjFieldName,
	     ObjSetSpec#typedef{checked=true,
				typespec=check_object(S,ObjSetSpec,ObjSetSpec#typedef.typespec)}}
    end.

check_value(OldS,V) when record(V,pvaluesetdef) ->
    #pvaluesetdef{name=Name,checked=Checked,args=Args,
		  type=Type,valueset=Value} = V,
    case Checked of
	true -> V;
	{error,_} -> V;
	false ->
	    case get_referenced_type(OldS,Type#type.def) of
		{_,Class} when record(Class,classdef) ->
		    throw({pobjectsetdef});
		_ -> continue
	    end
    end;
check_value(OldS,V) when record(V,valuedef) ->
    #valuedef{name=Name,checked=Checked,type=Vtype,value=Value} = V,
    case Checked of
	true -> 
	    V;
	{error,_} ->
	    V;
	false ->
	    Def = Vtype#type.def,
	    Constr = Vtype#type.constraint,
	    S = OldS#state{type=Vtype,tname=Def,value=V,vname=Name},
	    NewDef = 
		case Def of
		    Tref when record(Tref,typereference) ->
			{_,Type} = get_referenced_type(S,Tref),
			check_value(S,V#valuedef{type=Type#typedef.typespec}),
			#newv{};
		    Ext when record(Ext,'Externaltypereference') ->
			{_,Type} = get_referenced_type(S,Ext),
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
%%		    'UniversalString' -> %added 6/12 -00
%%			#newv{value=validate_restrictedstring(S,Value,Def,Constr)};
		    Seq when record(Seq,'SEQUENCE') ->
			#newv{value=
			      validate_sequence(S,
						Value,
						Seq#'SEQUENCE'.components,
						Constr)};
		    {'SEQUENCE OF',Components} ->
			#newv{value=validate_sequenceof(S,Value,Components,Constr)};
		    {'CHOICE',Components} ->
			#newv{value=validate_choice(S,Value,Components,Constr)};
		    Set when record(Set,'SET') ->
			#newv{value=
			      validate_set(S,
					   Value,
					   Set#'SET'.components,
					   Constr)};
		    {'SET OF',Components} ->
			#newv{value=validate_setof(S,Value,Components,Constr)};
		    Other ->
			exit({'cant check value of type' ,Other})
		end,
	    case NewDef#newv.value of
		unchanged ->
		    V#valuedef{checked=true,value=Value};
		ok ->
		    V#valuedef{checked=true,value=Value};
		{error,Reason} ->
		    V#valuedef{checked={error,Reason},value=Value};
		_V ->
		    V#valuedef{checked=true,value=_V}
	    end
    end.

validate_integer(S,{identifier,Pos,Id},NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown NamedNumber",S})
    end;
validate_integer(S=#state{mname=M},
		 #'Externalvaluereference'{module=M,value=Id},
		 NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown NamedNumber",S})
    end;
validate_integer(S,Id,NamedNumberList,Constr) when atom(Id) ->
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

%%------------
%% This can be removed when the old parser is removed
%% The function removes 'space' atoms from the list

is_space_list([H],Acc) ->
    lists:reverse([H|Acc]);
is_space_list([H,space|T],Acc) ->
    is_space_list(T,[H|Acc]);
is_space_list([],Acc) ->
    lists:reverse(Acc);
is_space_list([H|T],Acc) ->
    is_space_list(T,[H|Acc]).

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
	{_,V} when record(V,valuedef) -> 
	    case NewV = check_value(S,V) of
		#valuedef{type=#type{def='OBJECT IDENTIFIER'},
			  checked=true,value=Value} when tuple(Value) ->
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
	{_,V} when record(V,valuedef) -> 
	    case NewV = check_value(S, V) of
		#valuedef{checked=true,value=Value} when integer(Value) ->
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

validate_enumerated(S,Id,NamedNumberList,Constr) when atom(Id) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end;
validate_enumerated(S,{identifier,Pos,Id},NamedNumberList,Constr) ->
    case lists:keysearch(Id,1,NamedNumberList) of
	{value,_} -> ok;
	false -> error({value,"unknown ENUMERATED",S})
    end;
validate_enumerated(S,#'Externalvaluereference'{value=Id},
		    NamedNumberList,Constr) ->
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



check_ptype(S,Type,Ts) when record(Ts,type) ->
    Tag = Ts#type.tag,
    Constr = Ts#type.constraint,
    Def = Ts#type.def,
    NewDef= 
	case Def of 
	    Seq when record(Seq,'SEQUENCE') ->
		#newt{type=Seq#'SEQUENCE'{pname=Type#ptypedef.name}};
	    Set when record(Set,'SET') ->
		#newt{type=Set#'SET'{pname=Type#ptypedef.name}};
	    Other ->
		#newt{}
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts;
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    Ts2.


check_type(S,Type,ObjSpec={{objectclassname,_},_}) ->
    check_class(S,ObjSpec);
check_type(S,Type,Ts) when record(Type,typedef),(Type#typedef.checked==true) ->
    Ts;
check_type(S,Type,Ts) when record(Ts,type) ->
    {Def,Tag,Constr} = 
	case match_parameters(Ts#type.def,S#state.parameters) of
	    #type{constraint=Ctmp,def=Dtmp} ->
		{Dtmp,Ts#type.tag,Ts#type.constraint};
	    Dtmp ->
		{Dtmp,Ts#type.tag,Ts#type.constraint}
	end,
    TestFun = 
	fun(Tref) ->
		{_,MaybeChoice} = get_referenced_type(S,Tref),
		case catch((MaybeChoice#typedef.typespec)#type.def) of
		    {'CHOICE',_} ->
			case Tag of
			    [#tag{type='IMPLICIT'}] ->
				throw({error,{asn1,{implicit_tag_before_choice}}});
			    [TTag = #tag{type={default,_}}] ->
				[TTag#tag{type='EXPLICIT'}]; % X.680 28.6 c
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
		{_,RefType} = get_referenced_type(S,Tref),
		case RefType of
		    Class when record(Class,classdef) ->
			throw({asn1_class,Class});
		    _ -> ok
		end,
%%		case asn1ct_gen:get_inner(
%%		       asn1ct_gen:prim_bif((RefType#typedef.typespec)#type.def)) of
		case asn1ct_gen:prim_bif(
		       asn1ct_gen:get_inner(
			 (RefType#typedef.typespec)#type.def)) of
		    true -> %% this might be a class, an objectset, an object etc. FIX 
			RefType1 = check_type(S,RefType,RefType#typedef.typespec),
			#newt{type=RefType1#type.def, 
			      tag=merge_tags(Tag,
					     RefType1#type.tag),
			      constraint=
			      merge_constraints(check_constraints(S,Constr),
						RefType1#type.constraint)};
		    _ ->
			#newt{type=check_typereference(S,Tref),tag=Ct}
		end;
	    Ext when record(Ext,'Externaltypereference') ->
		Ct = TestFun(Ext),
		{_,RefType} = get_referenced_type(S,Ext),
		case RefType of
		    Class when record(Class,classdef) ->
			throw({asn1_class,Class});
		    _ -> ok
		end,
%%		case asn1ct_gen:get_inner(asn1ct_gen:prim_bif((RefType#typedef.typespec)#type.def)) of
		case asn1ct_gen:prim_bif(asn1ct_gen:get_inner((RefType#typedef.typespec)#type.def)) of
		    true ->
			RefType1 = check_type(S,RefType,RefType#typedef.typespec),
			#newt{type=RefType1#type.def, 
			      tag=merge_tags(Tag,
					     RefType1#type.tag),
			      constraint=
			      merge_constraints(check_constraints(S,Constr),
						RefType1#type.constraint)};
		    _ ->
			#newt{type=check_externaltypereference(S,Ext),tag=Ct}
		end;
	    'ANY' ->
		#newt{type='ASN1_OPEN_TYPE'};
	    {'ANY_DEFINED_BY',_} ->
		#newt{type='ASN1_OPEN_TYPE'};
	    'INTEGER' ->
		check_integer(S,[],Constr),
		#newt{};
	    {'INTEGER',NamedNumberList} ->
		#newt{type={'INTEGER',check_integer(S,NamedNumberList,Constr)}};
	    {'BIT STRING',NamedNumberList} ->
		NewL = check_bitstring(S,NamedNumberList,Constr),
%%		erlang:display({asn1ct_check,NamedNumberList,NewL}),
		#newt{type={'BIT STRING',NewL}};
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
	    Seq when record(Seq,'SEQUENCE') ->
		{TableCInf,Components} =
		    check_sequence(S,Type,Seq#'SEQUENCE'.components),
		#newt{type=Seq#'SEQUENCE'{tablecinf=TableCInf,
					  components=Components}};
	    {'SEQUENCE OF',Components} ->
		#newt{type={'SEQUENCE OF',check_sequenceof(S,Type,Components)}};
	    {'CHOICE',Components} ->
		Ct = case Tag of
			 [#tag{type='IMPLICIT'}] ->
			     throw({error,{asn1,{implicit_tag_before_choice}}});
			 [ChTag = #tag{type={default,_}}] -> 
			     [ChTag#tag{type='EXPLICIT'}]; % X.680 28.6 c
			 _ -> 
			     unchanged
		end,
		#newt{type={'CHOICE',check_choice(S,Type,Components)},tag=Ct};
	    Set when record(Set,'SET') ->
		{TableCInf,Components} =
		    check_set(S,Type,Set#'SET'.components),
		#newt{type=Set#'SET'{tablecinf=TableCInf,
				     components=Components}};
	    {'SET OF',Components} ->
		#newt{type={'SET OF',check_setof(S,Type,Components)}};
	    %% This is a temporary hack until the full Information Obj Spec
	    %% in X.681 is supported
	    {{typereference,_,'TYPE-IDENTIFIER'},[{typefieldreference,_,'Type'}]} ->
		#newt{type='ASN1_OPEN_TYPE'};
	    {#'Externaltypereference'{type='TYPE-IDENTIFIER'},[{typefieldreference,_,'Type'}]} ->
		#newt{type='ASN1_OPEN_TYPE'};
	    {pt,Ptype,ParaList} ->
		NewParaList = [match_parameters(TmpParam,S#state.parameters)|| TmpParam <- ParaList],
		Instance = instantiate_ptype(S,Ptype,NewParaList),
		#newt{type=Instance#type.def,
		      constraint=Instance#type.constraint};
	    {ClRef,FieldRefList} when tuple(ClRef),(element(1,ClRef)==objectclassname)->
		%% this case occures in a SEQUENCE when 
		%% tableconstraints are used
		ClassSpec = check_class(S,ClRef),
		#newt{type={ClassSpec,FieldRefList}};
	    {valueset,Vtype} ->
		#newt{type={valueset,check_type(S,Type,Vtype)}};
	    Other ->
		exit({'cant check' ,Other})
	end,
    Ts2 = case NewDef of
	      #newt{type=unchanged} ->
		  Ts;
	      #newt{type=TDef}->
		  Ts#type{def=TDef}
	  end,
    NewTag = case NewDef of
		 #newt{tag=unchanged} ->
		     Tag;
		 #newt{tag=TT} ->
		     TT
	     end,
    T3 = Ts2#type{tag = lists:map(fun(TempTag = #tag{type={default,TTx}}) ->
					  TempTag#tag{type=TTx};
				     (Else) -> Else end, NewTag)},
    T4 = case NewDef of
	     #newt{constraint=unchanged} ->
		 T3#type{constraint=Constr};
	     #newt{constraint=NewConstr} -> 
		 T3#type{constraint=NewConstr}
	 end,
    T4#type{constraint=check_constraints(S,T4#type.constraint)}.



% fix me
instantiate_ptype(S,Ptype,ParaList) ->
    {_,Ptypedef} = get_referenced_type(S, Ptype),
    #ptypedef{args=Args,typespec=Type} = Ptypedef,
    MatchedArgs = match_args(Args, ParaList, []),
    %%io:format("instantiate_ptype Ptype = ~p~nMatchedArgs = ~p~n",[Ptype,MatchedArgs]),
%%%     {Ename,ExtMod} = get_referenced_module(S,Ptype),
%%%     NewMod = merge_imports({S#state.mname,S#state.module},
%%% 			   {Ename,ExtMod}),
%%%     NewS = S#state{module=NewMod,
%%% 		   type=Type,parameters=MatchedArgs},
    NewS = S#state{type=Type,parameters=MatchedArgs},
    check_type(NewS, Ptypedef, Type).

% merge_imports({Name,Module},{Name,_}) ->
%     Module;
% merge_imports({Name,Module},{EName,EModule}) ->
%     {imports,ImportList} = Module#module.imports,
%     {imports,EImportList} = EModule#module.imports,
%     NewImports = merge_imports1(ImportList,EImportList,[]),
%     Module#module{imports={imports,NewImports}}.

% merge_imports1([Imp = #'SymbolsFromModule'{symbols=Symbols,
% 					   module=#typereference{val=Name}
% 					  }|Rest],
% 	       EImports,Acc) ->
%     case import_from_module(Name,EImports) of
% 	[] ->
% 	    merge_imports1(Rest,EImports,[Imp|Acc]);
% 	ESymbols -> 
% 	    MergedSymbs = merge_import_refs(Symbols,ESymbols,Symbols),
% 	    merge_imports1(Rest,EImports,
% 			   [Imp#'SymbolsFromModule'{symbols=MergedSymbs}|Acc])
%     end;
% merge_imports1([],_,Acc) ->
%     Acc.

% import_from_module(Name,ImportList) ->
%     Pred = fun(#'SymbolsFromModule'{module=#typereference{val=N}}) when N == Name ->
% 		   true;
% 	      (_) ->
% 		   false
% 	   end,
%     case lists:filter(Pred,ImportList) of
% 	[] ->
% 	    [];
% 	List when list(List) ->
% 	    lists:flatten(lists:map(fun(#'SymbolsFromModule'{symbols=S})->S end,
% 		      List))
%     end.

% merge_import_refs(Symbols,[ESymb|ESymbs],Acc) ->
%     Fun = 
% 	case ESymb of
% 	    #typereference{val=Name} ->
% 		fun(#typereference{val=N}) when N==Name -> true;
% 		   (_) -> false
% 		end;
% 	    #identifier{val=Name} ->
% 		fun(#identifier{val=N}) when N==Name -> true;
% 		   (_) -> false
% 		end
% 	end,
%     case lists:any(Fun,Symbols) of
% 	true -> merge_import_refs(Symbols,ESymbs,Acc);
% 	false -> merge_import_refs(Symbols,ESymbs,[ESymb|Acc])
%     end;
% merge_import_refs(_,[],Acc) ->
%     Acc.

	       
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
resolv_value(S,Val) ->
    case match_parameters(Val, S#state.parameters) of
	Id -> % unchanged 
	    resolv_value1(S,Id);
	Other ->
	    resolv_value(S,Other)
    end.

% resolv_value1(S = #state{mname=M},
% 	      V=#'Externalvaluereference'{module=M,value=Name}) ->
%     case asn1_db:dbget(M,Name) of
% 	undefined ->
% 	    case imported(S,Name) of 
% 		{ok,Imodule} ->
% 		    V2 = get_referenced(S,Imodule,Name),
% 		    V2#valuedef.value;
% 		_  ->
% 		    throw({error,{asn1,{undefined_type_or_value,Name}}})
% 	    end;
% 	Val ->
% 	    Val#valuedef.value
%    end;
resolv_value1(S = #state{mname=M,inputmodules=InpMods},
	      V=#'Externalvaluereference'{module=ExtM,value=Name}) ->
    case ExtM of
	M -> resolv_value2(S,M,Name);
	_ ->
	    case lists:member(ExtM,InpMods) of
		true ->
		    resolv_value2(S,M,Name);
		false ->
		    V
	    end
    end;    
resolv_value1(S,{gt,V}) ->
    case V of
	Int when integer(Int) ->
	    V + 1;
	#valuedef{value=Int} ->
	    1 + resolv_value(S,Int);
	Other ->
	    throw({error,{asn1,{undefined_type_or_value,Other}}})
    end;
resolv_value1(S,{lt,V}) ->
    case V of
	Int when integer(Int) ->
	    V - 1;
	#valuedef{value=Int} ->
	    resolv_value(S,Int) - 1;
	Other ->
	    throw({error,{asn1,{undefined_type_or_value,Other}}})
    end;
resolv_value1(_,V) ->
    V.

resolv_value2(S,ModuleName,Name) ->
    case asn1_db:dbget(ModuleName,Name) of
	undefined ->
	    case imported(S,Name) of 
		{ok,Imodule} ->
		    {_,V2} = get_referenced(S,Imodule,Name),
		    V2#valuedef.value;
		_  ->
		    throw({error,{asn1,{undefined_type_or_value,Name}}})
	    end;
	Val ->
	    Val#valuedef.value
    end.

check_constraints(S,[{'ContainedSubtype',Type} | Rest], Acc) ->
    {_,CTDef} = get_referenced_type(S,Type#type.def),
    CType = check_type(S,S#state.tname,CTDef#typedef.typespec),    
    check_constraints(S,Rest,CType#type.constraint ++ Acc);
check_constraints(S,[C | Rest], Acc) ->
    check_constraints(S,Rest,[check_constraint(S,C)| Acc]);
check_constraints(S,[],Acc) ->
    lists:flatten(Acc).

check_constraint(S,Ext) when record(Ext,'Externaltypereference') ->
    check_externaltypereference(S,Ext);


check_constraint(S,{'SizeConstraint',{Lb,Ub}}) ->
    case {resolv_value(S,Lb),resolv_value(S,Ub)} of
	{FixV,FixV} ->
	    {'SizeConstraint',FixV};
	{Low,High} when Low < High ->
	    {'SizeConstraint',{Low,High}};
	Err ->
	    throw({error,{asn1,{illegal_size_constraint,Err}}})
    end;
check_constraint(S,{'SizeConstraint',Lb}) ->
    {'SizeConstraint',resolv_value(S,Lb)};

check_constraint(S,{'SingleValue', L}) when list(L) ->
    F = fun(A) -> resolv_value(S,A) end,
    {'SingleValue',lists:map(F,L)};
    
check_constraint(S,{'SingleValue', V}) when integer(V) ->
    Val = resolv_value(S,V),
    [{'SingleValue',Val},{'ValueRange',{Val,Val}}];
check_constraint(S,{'SingleValue', V}) ->
    {'SingleValue',resolv_value(S,V)};

check_constraint(S,{'ValueRange', {Lb, Ub}}) ->
    {'ValueRange',{resolv_value(S,Lb),resolv_value(S,Ub)}};

%%check_constraint(S,{'ContainedSubtype',Type}) ->
%%    #typedef{typespec=TSpec} = 
%%	check_type(S,S#state.tname,get_referenced_type(S,Type#type.def)),
%%    [C] = TSpec#type.constraint,
%%    C;

check_constraint(S,{valueset,Type}) ->
    {valueset,check_type(S,S#state.tname,Type)};

check_constraint(S,{simpletable,Type}) ->
    {simpletable,check_type(S,S#state.tname,Type)};

check_constraint(S,{componentrelation,{objectset,Opos,Objset},Id}) ->
    #type{def=Def} = check_type(S,S#state.tname,#type{def=Objset}),
    {componentrelation,{objectset,Opos,Def},Id};

check_constraint(S,Type) when record(Type,type) ->
    #type{def=Def} = check_type(S,S#state.tname,Type),
    Def;

% else keep the constraint unchanged
check_constraint(S,Any) ->
    io:format("Constraint = ~p~n",[Any]),
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
	    case lists:keysearch(Name,#'Externaltypereference'.type,Exports) of
		false -> false;
		_ -> true
	    end
    end.
    
check_externaltypereference(S,Etref=#'Externaltypereference'{pos=Pos,
							     module=Emod,
							     type=Etype})->
    Currmod = S#state.mname,
    MergedMods = S#state.inputmodules,
    case Emod of
	Currmod ->
	    %% reference to current module or to imported reference
		check_typereference(S,#typereference{val=Etype,pos=Pos});
	 _ ->
	    %% io:format("Type ~s IMPORTED FROM ~s~n",[Etype,Emod]),
	    case lists:member(Emod,MergedMods) of
		true ->
		    check_typereference(S,#typereference{val=Etype,pos=Pos});
		false ->
		    Etref
	    end
    end.

check_typereference(S,#typereference{pos=Pos,val=Name}) ->
    ModName = S#state.mname,
    case asn1_db:dbget(ModName,Name) of
	undefined ->
	    case imported(S,Name) of
		{ok,Imodule} ->
		    %% io:format("Type ~s IMPORTED FROM ~s~n",[Name,Imodule]),
		    check_imported(S,Imodule,Name),
		    #'Externaltypereference'{module=Imodule,type=Name};
		_ ->
		    throw({error,{asn1,{undefined_type,Name}}})
		    %io:format("Type ~s is not defined~n",[Name]),
		    %Tref
	    end;
	_ ->
%	    Tref
	    #'Externaltypereference'{pos=Pos,module=ModName,type=Name}
    end.

get_referenced_class(S,ClassRef={Module,ClassName}) ->
    {_,Def} = get_referenced_type(S,#'Externaltypereference'{module=Module,type=ClassName}),
    {ClassRef,Def};
get_referenced_class(S,ClassName) ->
    {_,Def} = get_referenced_type(S,#typereference{val=ClassName}),
    {ClassName,Def}.

get_referenced_type(S,Ext) when record(Ext,'Externaltypereference') ->
    case match_parameters(Ext, S#state.parameters) of
	Ext -> 
	    #'Externaltypereference'{pos=Pos,module=Emod,type=Etype} = Ext,
	    case S#state.mname of
		Emod -> % a local reference in this module
		    get_referenced1(S,Etype,Pos);
		OtherMod ->
		    case lists:member(Emod,S#state.inputmodules) of
			true ->
			    get_referenced1(S,Etype,Pos);
			false ->
			    get_referenced(S,Emod,Etype)
		    end
	    end;
	#typereference{val=Name,pos=Pos} ->
	    get_referenced1(S,Name,Pos);
	Other ->
	    {undefined,Other}
    end;
get_referenced_type(S,Tref) when record(Tref,typereference) ->
    case match_parameters(Tref, S#state.parameters) of
	Tref -> 
	    #typereference{val=Name,pos=Pos} = Tref,
	    get_referenced1(S,Name,Pos);
	#typereference{val=Name,pos=Pos} ->
	    get_referenced1(S,Name,Pos);
	Other ->
	    {undefined,Other}
    end;
get_referenced_type(S=#state{mname=Emod},
		    #'Externalvaluereference'{pos=P,module=Emod,value=Eval}) ->
    get_referenced1(S,Eval,P);
get_referenced_type(S,#'Externalvaluereference'{pos=Pos,module=Emod,
						value=Eval}) ->
    case lists:member(Emod,S#state.inputmodules) of
	true ->
	    get_referenced1(S,Eval,Pos);
	false ->
	    get_referenced(S,Emod,Eval)
    end;
get_referenced_type(S,#identifier{val=Name,pos=Pos}) ->
    get_referenced1(S,Name,Pos);
get_referenced_type(S,Type) ->
    {undefined,Type}.


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
		    
		    
		    {Emod,T#typedef{typespec=Spec#type{def=Def}}};
		_ ->
		    {Emod,T} % should add check that T is exported here
	    end;
% 	P when record(P,ptypedef) ->
% 	    Spec = P#ptypedef.typespec,
% 	    case Spec#type.def of
% 		Tref when record(Tref,typereference) ->
% 		    Def = #'Externaltypereference'{module=Emod,
% 					 type=Tref#typereference.val,
% 					 pos=Tref#typereference.pos},
% 		    P#ptypedef{typespec=Spec#type{def=Def}};
% 		_ ->
% 		    P
% 	    end;
	V -> {Emod,V}
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
	    {S#state.mname,T}
    end.

%  get_referenced_module(S=,#'Externaltypereference'{module=Ename}) ->
%      ExtMod = asn1_db:dbget(Ename,'MODULE'),
%      Ename;
%  get_referenced_module(S,Tref) when record(Tref,typereference) ->
%     case 

match_parameters(Name,[]) ->
    Name;

match_parameters(#'Externaltypereference'{type=Name},[{#'Externaltypereference'{type=Name},NewName}|T]) ->
    NewName;
match_parameters(#'Externaltypereference'{type=Name},[{{_,#'Externaltypereference'{type=Name}},NewName}|T]) ->
    NewName;
% match_parameters(#'Externaltypereference'{type=Name},[{#typereference{val=Name},NewName}|T]) ->
%     NewName;
% match_parameters(#'Externaltypereference'{type=Name},[{{_,#typereference{val=Name}},NewName}|T]) ->
%     NewName;
%match_parameters(#typereference{val=Name},[{#typereference{val=Name},NewName}|T]) ->
%    NewName;
match_parameters(#'Externalvaluereference'{value=Name},[{#'Externalvaluereference'{value=Name},NewName}|T]) ->
    NewName;
match_parameters(#'Externalvaluereference'{value=Name},[{{_,#'Externalvaluereference'{value=Name}},NewName}|T]) ->
    NewName;
% match_parameters(#identifier{val=Name},[{#identifier{val=Name},NewName}|T]) ->
%     NewName;
% match_parameters(#identifier{val=Name},[{{_,#identifier{val=Name}},NewName}|T]) ->
%     NewName;
match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},{valueset,#type{def=NewName}}}|T]) ->
    NewName;
match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
		 [{{_,#'Externaltypereference'{type=Name}},NewName}|T]) ->
    NewName;
% match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
% 		 [{{_,#typereference{val=Name}},{valueset,#type{def=NewName}}}|T]) ->
%     NewName;
% match_parameters({valueset,#type{def=#'Externaltypereference'{type=Name}}},
% 		 [{{_,#typereference{val=Name}},NewName}|T]) ->
%     NewName;

match_parameters(Name, [H|T]) ->
    %%io:format("match_parameters(~p,~p)~n",[Name,[H|T]]),
    match_parameters(Name,T).

imported(S,Name) ->
    {imports,Ilist} = (S#state.module)#module.imports,
    imported1(Name,Ilist).

imported1(Name,
	  [#'SymbolsFromModule'{symbols=Symlist,
				module=#'Externaltypereference'{type=ModuleName}}|T]) ->
    case lists:keysearch(Name,#'Externaltypereference'.type,Symlist) of
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



check_bitstring(S,[],Constr) ->
    [];
check_bitstring(S,NamedNumberList,Constr) ->
    case check_unique(NamedNumberList,2) of
	[] ->
	    check_bitstr(S,NamedNumberList,[]);
	L when list(L) ->
	    error({type,{duplicates,L},S}),
	    unchanged
    end.

check_bitstr(S,[{'NamedNumber',Id,Num}|T],Acc)when integer(Num) ->
    check_bitstr(S,T,[{Id,Num}|Acc]);
check_bitstr(S,[{'NamedNumber',Id,Name}|T],Acc) when atom(Name) ->
%%check_bitstr(S,[{'NamedNumber',Id,{identifier,_,Name}}|T],Acc) -> 
%%    io:format("asn1ct_check:check_bitstr/3 hej hop ~w~n",[Name]),
    Val = dbget_ex(S,S#state.mname,Name),
%%    io:format("asn1ct_check:check_bitstr/3: ~w~n",[Val]),
    check_bitstr(S,[{'NamedNumber',Id,Val#valuedef.value}|T],Acc);
check_bitstr(S,[],Acc) ->
    case check_unique(Acc,2) of
	[] ->
	    lists:keysort(2,Acc);
	L when list(L) ->
	    error({type,{duplicate_values,L},S}),
	    unchanged
    end.
    
%%check_bitstring(S,NamedNumberList,Constr) ->
%%    NamedNumberList.
    


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
    NewList = enum_number(lists:reverse(Acc2),NewAcc2,0,[]),
    { NewList, check_enum(S,T,[],[])};
check_enum(S,[Id|T],Acc1,Acc2) when atom(Id) ->
    check_enum(S,T,Acc1,[Id|Acc2]);
check_enum(S,[],Acc1,Acc2) ->
    NewAcc2 = lists:keysort(2,Acc1),
    enum_number(lists:reverse(Acc2),NewAcc2,0,[]).


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
	    Components2 = maybe_automatic_tags(S,Components),
	    %% check the table constraints from here. The outermost type
	    %% is Type, the innermost is Comps (the list of components)
	    NewComps = 
		case check_each_component(S,Type,Components2) of
		    NewComponents when list(NewComponents) ->
			check_unique_sequence_tags(S,NewComponents),
			NewComponents;
		    Ret = {NewComponents,NewEcomps} ->
			check_unique_sequence_tags(S,NewEcomps),
			Ret
		end,
	    {CRelInf,NewComps2} = componentrelation_leadingattr(S,NewComps),
	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps2),
	    {CRelInf,CompListWithTblInf};
	Dupl ->
		throw({error,{asn1,{duplicate_components,Dupl}}})
    end.

expand_components(S, [{'COMPONENTS OF',Type}|T]) ->
    CompList = 
	case get_referenced_type(S,Type#type.def) of
	    {_,#typedef{typespec=#type{def=Seq}}} when record(Seq,'SEQUENCE') -> 
		case Seq#'SEQUENCE'.components of
		    {Root,Ext} -> Root;
		    Root -> Root
		end;
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
    case check_type(S,Type,Component) of
	T = #type{def={'ENUMERATED',CheckedComponent}} ->
	    case Component#type.def of
		#'Externaltypereference'{type=RefName} ->
		    T#type{def={'ENUMERATED',RefName,CheckedComponent}};
		_ -> T
	    end;
	Other -> Other
    end.

% check_sequenceof(S,Type,Component) when record(Component,type) ->
%     check_type(S,Type,Component).

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
	    Components2 = maybe_automatic_tags(S,Components),
	    NewComps =
		case check_each_alternative(S,Type,Components2) of
		    {NewComponents,NewEcomps} ->		    
			check_unique_tags(S,NewComponents ++ NewEcomps),
			{NewComponents,NewEcomps};
		    NewComponents ->
			check_unique_tags(S,NewComponents),
			NewComponents
		end;
%%	    CompListWithTblInf = get_tableconstraint_info(S,Type,NewComps);
	Dupl ->
	    throw({error,{asn1,{duplicate_choice_alternatives,Dupl}}})
    end;
check_choice(S,Type,[]) -> 
    [].

%% probably dead code that should be removed
%%maybe_automatic_tags(S,{Rc,Ec}) ->
%%    {maybe_automatic_tags1(S,Rc,0),maybe_automatic_tags1(S,Ec,length(Rc))};
maybe_automatic_tags(#state{erule=per},C) ->
    C;
maybe_automatic_tags(S,C) ->
    maybe_automatic_tags1(S,C,0).

maybe_automatic_tags1(S,C,TagNo) ->
    case (S#state.module)#module.tagdefault of
	'AUTOMATIC' ->
	    generate_automatic_tags(S,C,TagNo);
	_ ->
	    C
    end.

generate_automatic_tags(S,C,TagNo) ->
    case any_manual_tag(C) of
	true ->
	    C;
	false ->
	    generate_automatic_tags1(C,TagNo)
    end.

generate_automatic_tags1([H|T],TagNo) when record(H,'ComponentType') ->
    #'ComponentType'{typespec=Ts} = H,
    NewTs = Ts#type{tag=[#tag{class='CONTEXT',
			     number=TagNo,
			     type={default,'IMPLICIT'},
			     form= 0 }]}, % PRIMITIVE
    [H#'ComponentType'{typespec=NewTs}|generate_automatic_tags1(T,TagNo+1)];
generate_automatic_tags1([ExtMark|T],TagNo) -> % EXTENSIONMARK
    [ExtMark | generate_automatic_tags1(T,TagNo)];
generate_automatic_tags1([],_) ->
    [].

any_manual_tag([#'ComponentType'{typespec=#type{tag=[]}}|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([{'EXTENSIONMARK',_,_}|Rest]) ->
    any_manual_tag(Rest);
any_manual_tag([H|Rest]) ->
    true;
any_manual_tag([]) ->
    false.
	    

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

check_each_component(S = #state{abscomppath=Path},Type,
		     [C|Ct],Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
    NewTags = get_taglist(S,Ts),
%    CheckedTs = check_tableconstraint(S,Type,check_type(S,Type,Ts)),
    CheckedTs = check_type(S#state{abscomppath=[Cname|Path]},Type,Ts),
%    TblConsInf = extract_tableconstraint_info(S,Type,CheckedTs),
    NewC = C#'ComponentType'{typespec=CheckedTs,tags=NewTags},
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

check_each_alternative(S=#state{abscomppath=Path},Type,[C|Ct],
		       Acc,Extacc,Ext) when record(C,'ComponentType') ->
    #'ComponentType'{name=Cname,typespec=Ts,prop=Prop} = C,
    NewTags = get_taglist(S,Ts),
    NewC = C#'ComponentType'{typespec=check_type(S#state{abscomppath=[Cname|Path]},
						 Type,Ts),tags=NewTags},
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

componentrelation_leadingattr(S,CompList) ->
    {Cs1,Cs2} =
	case CompList of
	    {Components,EComponents} when list(Components) ->
		{Components,Components};
	    CompList when list(CompList) ->
		{CompList,CompList}
	end,
    case any_simple_table(S,Cs1,[]) of
	[] -> {false,CompList};
	STList ->
	    componentrelation_leadingattr(S,Cs1,Cs2,STList,[],[],[])
    end.

componentrelation_leadingattr(_,[],CompList,_,[],_,NewCompList) ->
    {false,lists:reverse(NewCompList)};
componentrelation_leadingattr(_,[],CompList,_,LeadingAttr,CRelInfo,NewCompList) ->
%%    NewCompList = insert_objfun_paths(CompList,CRelInfo,[]),
    {lists:last(LeadingAttr),lists:reverse(NewCompList)}; %send all info in Ts later
componentrelation_leadingattr(S,[C|Cs],CompList,STList,Acc,CRIAcc,CompAcc) ->
    case catch componentrelation1(C#'ComponentType'.typespec,
				  [C#'ComponentType'.name]) of
	{'EXIT',Reason} ->
	    componentrelation_leadingattr(S,Cs,CompList,STList,Acc,CRIAcc,[C|CompAcc]);
	{CRI=[{_,_,_,_}|Rest],NewTSpec} ->
	    case leading_attr_index(CompList,CRI,
				    lists:reverse(S#state.abscomppath),[]) of
		[] ->
		    componentrelation_leadingattr(S,Cs,CompList,STList,Acc,
						  CRIAcc,[C|CompAcc]);
		NewCRI=[{ObjSet,Attr,N,ClassDef,Path}|NewRest] ->
		    OS = if 
			     atom(ObjSet) ->
				 ObjSet;
			     record(ObjSet,typereference) ->
				 ObjSet#typereference.val;
			     record(ObjSet,'Externaltypereference'),
			     ObjSet#'Externaltypereference'.module == S#state.mname ->
				 ObjSet#'Externaltypereference'.type;
			     record(ObjSet,'Externaltypereference') ->
				 case lists:member(ObjSet#'Externaltypereference'.module,
						   S#state.inputmodules) of
				     true -> 
					 ObjSet#'Externaltypereference'.type;
				     false ->
					 {ObjSet#'Externaltypereference'.module,
					  ObjSet#'Externaltypereference'.type}
				 end
			 end,
		    UniqueFieldName = 
			get_unique_fieldname(#classdef{typespec=ClassDef}),
%%	    case leading_attr_index(CompList,CRI) of
%%		[] ->
%%		    componentrelation_leadingattr(S,Cs,CompList,STList,Acc,
%%						  CRIAcc,[C|CompAcc]);
%%		[{ObjSet,AttrN,N}|CRIs] ->
		    Res = {OS,Attr,N,UniqueFieldName},
		    componentrelation_leadingattr(S,Cs,CompList,STList,[Res|Acc],
					 [{C#'ComponentType'.name,NewCRI}|CRIAcc],
				         [C#'ComponentType'{typespec=NewTSpec}|
					  CompAcc])
	    end;
	_ ->
	    componentrelation_leadingattr(S,Cs,CompList,STList,
					  Acc,CRIAcc,[C|CompAcc])
    end.

any_simple_table(S = #state{mname=M,abscomppath=Path},
		 [#'ComponentType'{name=Name,typespec=Type}|Cs],Acc) ->
    Constraint = Type#type.constraint,
    case lists:keysearch(simpletable,1,Constraint) of
	{value,{_,#type{def=Ref}}} ->
	    ST = 
		case Ref of
		    #'Externaltypereference'{module=M,type=ObjSetName} ->
			{true,[Name|Path],ObjSetName};
		    _ ->
			{true,[Name|Path],Ref}
		end,
	    any_simple_table(S,Cs,[ST|Acc]);
	false ->
	    any_simple_table(S,Cs,Acc)
    end;
any_simple_table(_,[],Acc) ->
    lists:reverse(Acc);
any_simple_table(S,[Other|Cs],Acc) ->
    any_simple_table(S,Cs,Acc).


%% componentrelation1/1 identifies all componentrelation constraints
%% that exist in C or in the substructure of C, info about the found constraints
%% are returned in a list
componentrelation1(C = #type{constraint=Constraint,tablecinf=TCI},Path) ->
    Ret =
	case C#type.constraint of
	    [{componentrelation,{_,_,ObjectSet},AtList}|Rest] ->		
		[{_,AL=[#'Externalvaluereference'{value=Attr}|R1]}|R2] = AtList,
		{ClassDef,_} = C#type.def,
		AttrPath = 
		    lists:map(fun(#'Externalvaluereference'{value=V})->V end,
			      AL),
		{[{ObjectSet,AttrPath,ClassDef,Path}],C#type.def};
	    Other ->
		innertype_comprel(C#type.def,Path)
	end,
    case Ret of
	nofunobj ->
	    nofunobj;
	{CRelI=[{ObjSet,_,_,_}],NewDef} ->
	    TCItmp = lists:subtract(TCI,[{objfun,ObjSet}]),
	    {CRelI,C#type{tablecinf=[{objfun,ObjSet}|TCItmp],def=NewDef}};
	{CompRelInf,NewDef} -> 
	    TCItmp = lists:subtract(TCI,[{objfun,anyset}]),
	    {CompRelInf,C#type{tablecinf=[{objfun,anyset}|TCItmp],def=NewDef}}
    end.

innertype_comprel({'SEQUENCE OF',Type},Path) ->
    case innertype_comprel1(Type,Path) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewType} ->
	    {CompRelInf,{'SEQUENCE OF',NewType}}
    end;
innertype_comprel({'SET OF',Type},Path) ->
    case innertype_comprel1(Type,Path) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewType} ->
	    {CompRelInf,{'SET OF',NewType}}
    end;
innertype_comprel({'CHOICE',CTypeList},Path) ->
    case componentlist_comprel(CTypeList,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,{'CHOICE',NewCs}}
    end;
innertype_comprel(S = #'SEQUENCE'{components=Cs},Path) ->
    case componentlist_comprel(Cs,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,S#'SEQUENCE'{components=NewCs}}
    end;
innertype_comprel(S = #'SET'{components=Cs},Path) ->
    case componentlist_comprel(Cs,[],Path,[]) of
	nofunobj ->
	    nofunobj;
	{CompRelInf,NewCs} ->
	    {CompRelInf,S#'SET'{components=NewCs}}
    end;
innertype_comprel(_,_) ->
%%    [].
    nofunobj.

componentlist_comprel([C = #'ComponentType'{name=Name,typespec=Type}|Cs],
		      Acc,Path,NewCL) ->
    case catch componentrelation1(Type,Path++[Name]) of
	{'EXIT',_} ->
	    componentlist_comprel(Cs,Acc,Path,[C|NewCL]);
	nofunobj ->
	    componentlist_comprel(Cs,Acc,Path,[C|NewCL]);
	{CRelInf,NewType} ->
	    componentlist_comprel(Cs,CRelInf++Acc,Path,
				  [C#'ComponentType'{typespec=NewType}|NewCL])
    end;
componentlist_comprel([],Acc,_,NewCL) ->
    case Acc of
	[] ->
	    nofunobj;
	_ ->
	    {Acc,lists:reverse(NewCL)}
    end.

innertype_comprel1(T = #type{def=Def,constraint=Cons,tablecinf=TCI},Path) ->
    Ret =
	case Cons of
	    [{componentrelation,{_,_,ObjectSet},AtList}|Rest] ->		
		[{_,AL=[#'Externalvaluereference'{value=Attr}|R1]}|R2] = AtList,
		{ClassDef,_} = Def,
		AttrPath = 
		    lists:map(fun(#'Externalvaluereference'{value=V})->V end,
			      AL),
		[{ObjectSet,AttrPath,ClassDef,Path}];
	    Other ->
		innertype_comprel(Def,Path)
	end,
    case Ret of
	nofunobj -> nofunobj;
	L = [{ObjSet,_,_,_}] ->
	    TCItmp = lists:subtract(TCI,[{objfun,ObjSet}]),
	    {L,T#type{tablecinf=[{objfun,ObjSet}|TCItmp]}};
	{CRelInf,NewDef} ->
	    TCItmp = lists:subtract(TCI,[{objfun,anyset}]),
	    {CRelInf,T#type{def=NewDef,tablecinf=[{objfun,anyset}|TCItmp]}}
    end.


leading_attr_index(Cs,[H={_,AttrP,_,_}|T],AbsP,Acc) ->
    Attr = 
	case lists:prefix(AbsP,AttrP) of
	    true ->
		hd(lists:subtract(AttrP,AbsP));
	    false ->
		hd(AttrP)
	end,
    case leading_attr_index1(Cs,H,Attr,1) of
	0 ->
	    leading_attr_index(Cs,T,AbsP,Acc);
	Res ->
	    leading_attr_index(Cs,T,AbsP,[Res|Acc])
    end;
leading_attr_index(Cs,[],_,Acc) ->
    lists:reverse(Acc).

leading_attr_index1([],_,_,N) ->
    0;
leading_attr_index1([C|Cs],Arg={ObjectSet,AttrPath,CDef,P},Attr,N) ->
    case C#'ComponentType'.name of
	Attr ->
	    {ObjectSet,Attr,N,CDef,P};
	_ ->
	    leading_attr_index1(Cs,Arg,Attr,N+1)
    end.

get_unique_fieldname(ClassDef) ->
    {_,Fields,_} = ClassDef#classdef.typespec,
    get_unique_fieldname(Fields,[]).

get_unique_fieldname([],[]) ->
    '__undefined_';
get_unique_fieldname([],[Name]) ->
    Name;
get_unique_fieldname([],Acc) ->
    throw({asn1,'only one UNIQUE field is allowed in CLASS',Acc});
get_unique_fieldname([{fixedtypevaluefield,Name,_,'UNIQUE',_}|Rest],Acc) ->
    get_unique_fieldname(Rest,[Name|Acc]);
get_unique_fieldname([H|T],Acc) ->
    get_unique_fieldname(T,Acc).

get_tableconstraint_info(S,Type,{CheckedTs,EComps}) ->
    {get_tableconstraint_info(S,Type,CheckedTs,[]),
     get_tableconstraint_info(S,Type,EComps,[])};
get_tableconstraint_info(S,Type,CheckedTs) ->
    get_tableconstraint_info(S,Type,CheckedTs,[]).

get_tableconstraint_info(S,Type,[],Acc) ->
    lists:reverse(Acc);
get_tableconstraint_info(S,Type,[C|Cs],Acc) ->
    CheckedTs = C#'ComponentType'.typespec,
    AccComp = 
	case CheckedTs#type.def of 
	    {{objectclass,Fields,_},FieldRef} ->
		AType = get_ObjectClassFieldType(S,Fields,FieldRef),
%%		AType = asn1ct_gen:get_inner(CheckedTs#type.def),
		RefedFieldName = get_referencedclassfield(CheckedTs#type.def),%is probably obsolete
%%		C#'ComponentType'{typespec={tableconstraint_info,
%%					    AType,
%%					    RefedFieldName}};
		C#'ComponentType'{typespec=
				  CheckedTs#type{def=AType,
						 constraint={tableconstraint_info,
							     RefedFieldName}}};
	    {'SEQUENCE OF',SOType} when record(SOType,type),
					(element(1,SOType#type.def) == 'CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList = get_tableconstraint_info(S,Type,CTypeList,[]),
		C#'ComponentType'{typespec=
				  CheckedTs#type{def={'SEQUENCE OF',
						      SOType#type{def={'CHOICE',
								       NewInnerCList}}}}};
	    {'SET OF',SOType} when record(SOType,type),
				   (element(1,SOType#type.def) == 'CHOICE') ->
		CTypeList = element(2,SOType#type.def),
		NewInnerCList = get_tableconstraint_info(S,Type,CTypeList,[]),
		C#'ComponentType'{typespec=
				  CheckedTs#type{def={'SET OF',
						      SOType#type{def={'CHOICE',
							     NewInnerCList}}}}};
	    _ ->
%%		C#'ComponentType'{typespec={no_info,CheckedTs}}
		C
	end,
    get_tableconstraint_info(S,Type,Cs,[AccComp|Acc]).

get_referencedclassfield(Def) ->
    case (catch lists:last(element(2,Def))) of
	{'EXIT',_} ->
	    {notype,Def};
	Normal ->
	    Normal
    end.

get_ObjectClassFieldType(S,Fields,L=[PrimFieldName1|Rest]) ->
    check_PrimitiveFieldNames(S,Fields,L),
    get_OCFType(Fields,PrimFieldName1,Rest).

check_PrimitiveFieldNames(S,Fields,L) ->
    ok.

get_OCFType([],PrimFieldName,Rest) ->
    {no_type,no_name};
get_OCFType([F|Fields],PFN={FieldType,PrimFieldName},Rest) ->
    case element(2,F) of
	PrimFieldName ->
	    case element(1,F) of
		fixedtypevaluefield ->
		    {fixedtypevaluefield,PrimFieldName,element(3,F)};
		objectfield ->
		    {objectfield,PrimFieldName,
		     lists:map(fun(X) -> element(2,X) end,Rest)};
		Other ->
		    {Other,PrimFieldName}
	    end;
	_  ->
	    get_OCFType(Fields,PFN,Rest)
    end.
    
%check_tableconstraint(S,OuterType,Component) when record(Component,type) ->
%    case Component#type.def of
%	{{objectclass,Fields,_},RefList} ->
%	    case lists:last(RefList) of
%		{valuefieldreference,ClFieldName} ->
		    
		    
get_taglist(#state{erule=per},_) ->
    [];
get_taglist(S,Ext) when record(Ext,'Externaltypereference') ->
    {_,T} = get_referenced_type(S,Ext),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Tref) when record(Tref,typereference) ->
    {_,T} = get_referenced_type(S,Tref),
    get_taglist(S,T#typedef.typespec);
get_taglist(S,Type) when record(Type,type) ->
    case Type#type.tag of
	[] ->
	    get_taglist(S,Type#type.def);
	Tags  ->
	    lists:map(fun(Tx) -> asn1ct_gen:def_to_tag(Tx) end,Tags)
    end;
get_taglist(S,{'CHOICE',{Rc,Ec}}) ->
    get_taglist(S,{'CHOICE',Rc ++ Ec});
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

merge_tags(T1, T2) ->
    merge_tags2(T1 ++ T2, []).

merge_tags2([T1= #tag{type='IMPLICIT'}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([T1= #tag{type={default,'IMPLICIT'}}, T2 |Rest], Acc) ->
    merge_tags2([T1#tag{type=T2#tag.type, form=T2#tag.form}|Rest],Acc);
merge_tags2([H|T],Acc) ->
    merge_tags2(T, [H|Acc]);
merge_tags2([], Acc) ->
    lists:reverse(Acc).

merge_constraints(C1, []) ->
    C1;
merge_constraints([], C2) ->
    C2;
merge_constraints(C1, C2) ->
    {SList,VList,Rest} = splitlist(C1++C2,[],[],[]),
    SizeC = merge_constraints(SList),
    ValueC = merge_constraints(VList),
    case Rest of
        [] ->
            SizeC ++ ValueC;
        _ ->
            throw({error,{asn1,{not_implemented,{merge_constraints,Rest}}}})
    end.
    
merge_constraints([]) -> [];
merge_constraints([C1 = {_,{Low1,High1}},{_,{Low2,High2}}|Rest]) when Low1 >= Low2,
                                                                      High1 =< High2 ->
    merge_constraints([C1|Rest]);
merge_constraints([C1 = {_,{Low1,High1}},C2 = {_,{Low2,High2}}|Rest]) ->
    throw({error,asn1,{conflicting_constraints,{C1,C2}}});
merge_constraints([C]) ->
    [C].

splitlist([C={'SizeConstraint',_}|Rest],Sacc,Vacc,Restacc) ->
    splitlist(Rest,[C|Sacc],Vacc,Restacc);
splitlist([C={'ValueRange',_}|Rest],Sacc,Vacc,Restacc) ->
    splitlist(Rest,Sacc,[C|Vacc],Restacc);
splitlist([C|Rest],Sacc,Vacc,Restacc) ->
    splitlist(Rest,Sacc,Vacc,[C|Restacc]);
splitlist([],Sacc,Vacc,Restacc) ->
    {lists:reverse(Sacc),
     lists:reverse(Vacc),
     lists:reverse(Restacc)}.



storeindb(M) when record(M,module) ->
    TVlist = M#module.typeorval,
    NewM = M#module{typeorval=findtypes_and_values(TVlist)},
    asn1_db:dbnew(NewM#module.name),
    asn1_db:dbput(NewM#module.name,'MODULE',  NewM),
    Res = storeindb(NewM#module.name,TVlist,[]),
    include_default_class(NewM#module.name),
    Res.

storeindb(Module,[H|T],ErrAcc) when record(H,typedef) ->
    storeindb(Module,H#typedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,valuedef) ->
    storeindb(Module,H#valuedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,ptypedef) ->
    storeindb(Module,H#ptypedef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,classdef) ->
    storeindb(Module,H#classdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,pvaluesetdef) ->
    storeindb(Module,H#pvaluesetdef.name,H,T,ErrAcc);
storeindb(Module,[H|T],ErrAcc) when record(H,pobjectdef) ->
    storeindb(Module,H#pobjectdef.name,H,T,ErrAcc);
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
		    error({type,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		Type when record(H,valuedef) ->
		    error({value,"already defined",
			   #state{mname=Module,value=H,vname=Name}});
		Type when record(H,ptypedef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		Type when record(H,pobjectdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		Type when record(H,pvaluesetdef) ->
		    error({ptype,"already defined",
			   #state{mname=Module,type=H,tname=Name}});
		Type when record(H,classdef) ->
		    error({class,"already defined",
			   #state{mname=Module,value=H,vname=Name}})
	    end,
	    storeindb(Module,T,[H|ErrAcc])
    end.

findtypes_and_values(TVList) ->
    findtypes_and_values(TVList,[],[],[],[],[],[]).%% Types,Values,
%% Parameterizedtypes,Classes,Objects and ObjectSets

findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,typedef),record(H#typedef.typespec,'Object') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#typedef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,typedef),record(H#typedef.typespec,'ObjectSet') ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#typedef.name|OSacc]);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,typedef) ->
    findtypes_and_values(T,[H#typedef.name|Tacc],Vacc,Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,valuedef) ->
    findtypes_and_values(T,Tacc,[H#valuedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,ptypedef) ->
    findtypes_and_values(T,Tacc,Vacc,[H#ptypedef.name|Pacc],Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) 
  when record(H,classdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,[H#classdef.name|Cacc],Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pvaluedef) ->
    findtypes_and_values(T,Tacc,[H#pvaluedef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pvaluesetdef) ->
    findtypes_and_values(T,Tacc,[H#pvaluesetdef.name|Vacc],Pacc,Cacc,Oacc,OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pobjectdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,[H#pobjectdef.name|Oacc],OSacc);
findtypes_and_values([H|T],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc)
  when record(H,pobjectsetdef) ->
    findtypes_and_values(T,Tacc,Vacc,Pacc,Cacc,Oacc,[H#pobjectsetdef.name|OSacc]);
findtypes_and_values([],Tacc,Vacc,Pacc,Cacc,Oacc,OSacc) ->
    {lists:reverse(Tacc),lists:reverse(Vacc),lists:reverse(Pacc),
     lists:reverse(Cacc),lists:reverse(Oacc),lists:reverse(OSacc)}.
    

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


include_default_class(Module) ->
    NameAbsList = default_class_list(),
    include_default_class1(Module,NameAbsList).

include_default_class1(Module,[]) ->
    ok;
include_default_class1(Module,[{Name,TS}|Rest]) ->
    case asn1_db:dbget(Module,Name) of
	undefined ->
	    C = #classdef{checked=true,name=Name,
			  typespec=TS},
	    asn1_db:dbput(Module,Name,C);
	_ -> ok
    end.

default_class_list() ->
    [{'TYPE-IDENTIFIER',
      {objectclass,
       [{fixedtypevaluefield,
	 id,
	 {type,[],'OBJECT IDENTIFIER',[]},
	 'UNIQUE',
	 'MANDATORY'},
	{typefield,'Type','MANDATORY'}],
       {'WITH SYNTAX',
	[{typefieldreference,'Type'},
	 'IDENTIFIED',
	 'BY',
	 {valuefieldreference,id}]}}},
     {'ABSTRACT-SYNTAX',
      {objectclass,
       [{fixedtypevaluefield,
	 id,
	 {type,[],'OBJECT IDENTIFIER',[]},
	 'UNIQUE',
	 'MANDATORY'},
	{typefield,'Type','MANDATORY'},
	{fixedtypevaluefield,
	 property,
	 {type,
	  [],
	  {'BIT STRING',[]},
	  []},
	 undefined,
	 {'DEFAULT',
	  [0,1,0]}}],
       {'WITH SYNTAX',
	[{typefieldreference,'Type'},
	 'IDENTIFIED',
	 'BY',
	 {valuefieldreference,id},
	 ['HAS',
	  'PROPERTY',
	  {valuefieldreference,property}]]}}}].

