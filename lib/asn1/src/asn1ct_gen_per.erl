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
-module(asn1ct_gen_per).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").
%-compile(export_all).

-export([pgen/4,gen_dec_prim/3,gen_encode_prim/4]).
-export([gen_obj_code/3,gen_objectset_code/2]).
-export([gen_decode/2, gen_decode/3]).
-export([gen_encode/2, gen_encode/3]).
-import(asn1ct_gen, [emit/1,demit/1]).

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    asn1ct_gen:pgen_module(OutFile,Erules,Module,TypeOrVal,true).


%% Generate ENCODING ******************************
%%****************************************x


gen_encode(Erules,Type) when record(Type,typedef) ->
    gen_encode_user(Erules,Type).
%%    case Type#typedef.typespec of
%%	Def when record(Def,type) ->	    
%%	    gen_encode_user(Erules,Type);
%%	Def when tuple(Def),(element(1,Def) == 'Object') ->
%%	    gen_encode_object(Erules,Type);
%%	Other ->
%%	    exit({error,{asn1,{unknown,Other}}})
%%    end.

gen_encode(Erules,Typename,#'ComponentType'{name=Cname,typespec=Type,prop=Prop}) ->
    NewTypename = [Cname|Typename],
    gen_encode(Erules,NewTypename,Type);

gen_encode(Erules,Typename,Type) when record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    ObjFun =
	case lists:keysearch(objfun,1,Type#type.tablecinf) of
	    {value,{_,Name}} ->
%%		lists:concat([", ObjFun",Name]);
		", ObjFun";
	    false ->
		""
	end,
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    case InnerType of
		'SET' ->
		    true;
		'SEQUENCE' ->
		    true;
		_ ->
		    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'({'",asn1ct_gen:list2name(Typename),
			  "',Val}",ObjFun,") ->",nl}),
		    emit({"'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val",ObjFun,");",nl,nl})
	    end,
	    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val",ObjFun,
		  ") ->",nl}),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.


gen_encode_user(Erules,D) when record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' -> true;
	'SEQUENCE' -> true;
	_ ->
	    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),"'({'",asn1ct_gen:list2name(Typename),"',Val}) ->",nl}),
	    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val);",nl,nl})
    end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val) ->",nl}),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_encode_prim(Erules,Def,"false"),
	    emit({".",nl});
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"false"),
	    emit({".",nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'enc_",Etype,"'(Val).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'enc_",Etype,"'(Val).",nl,nl});
	#typereference{val=Ename} ->
	    emit({"'enc_",Ename,"'(Val).",nl,nl});
	{notype,_} ->
	    emit({"'enc_",InnerType,"'(Val).",nl,nl})
    end.


gen_encode_prim(Erules,D,DoTag) ->
    Value = case asn1ct_name:active(val) of
		true ->
		    asn1ct_gen:mk_var(asn1ct_name:curr(val));
		false ->
		    "Val"
	    end,
    gen_encode_prim(Erules,D,DoTag,Value).

gen_encode_prim(Erules,D,DoTag,Value) when record(D,type) ->
    Constraint = D#type.constraint,
    case D#type.def of
	'INTEGER' ->
	    emit({"?RT_PER:encode_integer(", %fel
		  {asis,Constraint},",",Value,")"});
	{'INTEGER',NamedNumberList} ->
	    emit({"?RT_PER:encode_integer(",
		  {asis,Constraint},",",Value,",",
		  {asis,NamedNumberList},")"});
	{'ENUMERATED',{Nlist1,Nlist2}} ->
	    NewList = lists:concat([[{0,X}||{X,Y} <- Nlist1],['EXT_MARK'],[{1,X}||{X,Y} <- Nlist2]]),
	    NewC = [{'ValueRange',{0,length(Nlist1)-1}}],
	    emit(["case (case ",Value," of {_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NewC, NewList++[{asn1_enum,length(Nlist1)-1}], 0);
	{'ENUMERATED',NamedNumberList} ->
	    NewList = [X||{X,Y} <- NamedNumberList],
	    NewC = [{'ValueRange',{0,length(NewList)-1}}],
	    emit(["case (case ",Value," of {_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NewC, NewList, 0);
	{'BIT STRING',NamedNumberList} ->
	    emit({"?RT_PER:encode_bit_string(",
		  {asis,Constraint},",",Value,",",
		  {asis,NamedNumberList},")"});
	'NULL' ->
	    emit({"?RT_PER:encode_null(",Value,")"});
	'OBJECT IDENTIFIER' ->
	    emit({"?RT_PER:encode_object_identifier(",Value,")"});
	'ObjectDescriptor' ->
	    emit({"?RT_PER:encode_ObjectDescriptor(",{asis,Constraint},
		  ",",Value,")"});
	'BOOLEAN' ->
	    emit({"?RT_PER:encode_boolean(",Value,")"});
	'OCTET STRING' ->
	    emit({"?RT_PER:encode_octet_string(",{asis,Constraint},",",Value,")"});
	'NumericString' ->
	    emit({"?RT_PER:encode_NumericString(",{asis,Constraint},",",Value,")"});
	'TeletexString' ->
	    emit({"?RT_PER:encode_TeletexString(",{asis,Constraint},",",Value,")"});
	'VideotexString' ->
	    emit({"?RT_PER:encode_VideotexString(",{asis,Constraint},",",Value,")"});
	'UTCTime' ->
	    emit({"?RT_PER:encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GeneralizedTime' ->
	    emit({"?RT_PER:encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GraphicString' ->
	    emit({"?RT_PER:encode_GraphicString(",{asis,Constraint},",",Value,")"});
	'VisibleString' ->
	    emit({"?RT_PER:encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GeneralString' ->
	    emit({"?RT_PER:encode_GeneralString(",{asis,Constraint},",",Value,")"});
	'PrintableString' ->
	    emit({"?RT_PER:encode_PrintableString(",{asis,Constraint},",",Value,")"});
	'IA5String' ->
	    emit({"?RT_PER:encode_IA5String(",{asis,Constraint},",",Value,")"});
	'BMPString' ->
	    emit({"?RT_PER:encode_BMPString(",{asis,Constraint},",",Value,")"});
	'UniversalString' ->
	    emit({"?RT_PER:encode_UniversalString(",{asis,Constraint},",",Value,")"});
	'ANY' ->
	    emit(["?RT_PER:encode_open_type(", {asis,Constraint}, ",", 
		  Value, ")"]);
	'ASN1_OPEN_TYPE' ->
	    NewValue = case Constraint of
			   [#'Externaltypereference'{type=Tname}] ->
			     io_lib:format(
			       "?RT_PER:complete(enc_~s(~s))",[Tname,Value]);
			   [#type{def=#'Externaltypereference'{type=Tname}}] ->
			       io_lib:format(
				 "?RT_PER:complete(enc_~s(~s))",[Tname,Value]);
			 _ -> Value
		     end,
	    emit(["?RT_PER:encode_open_type(", {asis,Constraint}, ",", 
		  NewValue, ")"]);
	XX ->
	    exit({asn1_error,nyi,XX})
    end.

emit_enc_enumerated_cases(C, [H], Count) ->
    emit_enc_enumerated_case(C, H, Count),
    emit([";",nl,"EnumVal -> exit({error,{asn1, {enumerated_not_in_range, EnumVal}}})"]),
    emit([nl,"end"]);
emit_enc_enumerated_cases(C, ['EXT_MARK'|T], Count) ->
    emit_enc_enumerated_cases(C, T, 0);
emit_enc_enumerated_cases(C, [H1,H2|T], Count) ->
    emit_enc_enumerated_case(C, H1, Count),
    emit([";",nl]),
    emit_enc_enumerated_cases(C, [H2|T], Count+1).
    


emit_enc_enumerated_case(C, {asn1_enum,High}, _) ->
    emit([
	  "{asn1_enum,EnumV} when integer(EnumV), EnumV > ",High," -> ",
	  "[{bit,1},?RT_PER:encode_small_number(EnumV)]"]);
emit_enc_enumerated_case(C, 'EXT_MARK', Count) ->
    true;
emit_enc_enumerated_case(C, {1,EnumName}, Count) ->
    emit(["'",EnumName,"' -> [{bit,1},?RT_PER:encode_small_number(",Count,")]"]);
emit_enc_enumerated_case(C, {0,EnumName}, Count) ->
    emit(["'",EnumName,"' -> [{bit,0},?RT_PER:encode_integer(",{asis,C},", ",Count,")]"]);
emit_enc_enumerated_case(C, EnumName, Count) ->
    emit(["'",EnumName,"' -> ?RT_PER:encode_integer(",{asis,C},", ",Count,")"]).


%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(Erules,Module,Obj) when record(Obj,typedef) ->
    ObjName = Obj#typedef.name,
    Def = Obj#typedef.typespec,
    #'Externaltypereference'{module=Mod,type=ClassName} = 
	Def#'Object'.classname,
    Class = asn1_db:dbget(Mod,ClassName),
%     Class = 
% 	case Def#'Object'.classname of 
% 	    {OtherModule,ClName} ->
% 		asn1_db:dbget(OtherModule,ClName);
% 	    ClName ->
% 		asn1_db:dbget(Module,ClName)
% 	end,
    {object,_,Fields} = Def#'Object'.def,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjName}),
    emit({nl,"%%================================",nl}),
    EncConstructed =
	gen_encode_objectfields(Class#classdef.typespec,ObjName,Fields,[]),
    emit(nl),
    gen_encode_constr_type(EncConstructed),
    emit(nl),
    DecConstructed =
	gen_decode_objectfields(Class#classdef.typespec,ObjName,Fields,[]),
    emit(nl),
    gen_decode_constr_type(DecConstructed),
    emit(nl);
gen_obj_code(Erules,Module,Obj) when record(Obj,pobjectdef) ->
    ok.

gen_encode_objectfields(Class,ObjName,[{FieldName,Type}|Rest],ConstrAcc) ->
    Fields = Class#objectclass.fields,

    MaybeConstr =
    case is_typefield(Fields,FieldName) of
	true ->
	    Def = Type#typedef.typespec,
	    emit({"'enc_",ObjName,"'(",{asis,FieldName},
		  ", Val, Dummy) ->",nl}),

	    CAcc =
	    case Type#typedef.name of
		{primitive,bif} ->
		    gen_encode_prim(per,Def,"false","Val"),
		    [];
		{constructed,bif} ->
		    emit({"   'enc_",ObjName,'_',FieldName,
			  "'(Val)"}),
			[{['enc_',ObjName,'_',FieldName],Def}];
		{ExtMod,TypeName} ->
		    emit({"   '",ExtMod,"':'enc_",TypeName,"'(Val)"}),
		    [];
		TypeName ->
		    emit({"   'enc_",TypeName,"'(Val)"}),
		    []
	    end,
	    case more_genfields(Fields,Rest) of
		true ->
		    emit({";",nl});
		false ->
		    emit({".",nl})
	    end,
	    CAcc;
	{false,objectfield} ->
	    emit({"'enc_",ObjName,"'(",{asis,FieldName},
		  ", Val, [H|T]) ->",nl}),
	    case Type#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'enc_",TypeName,
			  "'(H, Val, T)"});
		TypeName ->
		    emit({indent(3),"'enc_",TypeName,"'(H, Val, T)"})
	    end,
	    case more_genfields(Fields,Rest) of
		true ->
		    emit({";",nl});
		false ->
		    emit({".",nl})
	    end,
	    [];
	{false,_} -> []
    end,
    gen_encode_objectfields(Class,ObjName,Rest,MaybeConstr ++ ConstrAcc);
gen_encode_objectfields(C,O,[H|T],Acc) ->
    gen_encode_objectfields(C,O,T,Acc);
gen_encode_objectfields(_,_,[],Acc) ->
    Acc.

gen_encode_constr_type([{Name,Def}|Rest]) ->
    emit({Name,"(Val) ->",nl}),
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    asn1ct_gen:gen_encode_constructed(per,Name,InnerType,Def),
    gen_encode_constr_type(Rest);
gen_encode_constr_type([]) ->
    ok.

gen_decode_objectfields(Class,ObjName,[{FieldName,Type}|Rest],ConstrAcc) ->
    Fields = Class#objectclass.fields,

    MaybeConstr =
    case is_typefield(Fields,FieldName) of
	true ->
	    Def = Type#typedef.typespec,
	    emit({"'dec_",ObjName,"'(",{asis,FieldName},
		  ", Val, Telltype, RestPrimFieldName) ->",nl}),

	    CAcc =
	    case Type#typedef.name of
		{primitive,bif} ->
		    gen_dec_prim(per,Def,"Val"),
		    [];
		{constructed,bif} ->
		    emit({"   'dec_",ObjName,'_',FieldName,
			  "'(Val, Telltype)"}),
		    [{['dec_',ObjName,'_',FieldName],Def}];
		{ExtMod,TypeName} ->
		    emit({"   '",ExtMod,"':'dec_",TypeName,
			  "'(Val, Telltype)"}),
		    [];
		TypeName ->
		    emit({"   'dec_",TypeName,"'(Val, Telltype)"}),
		    []
	    end,
	    case more_genfields(Fields,Rest) of
		true ->
		    emit({";",nl});
		false ->
		    emit({".",nl})
	    end,
	    CAcc;
	{false,objectfield} ->
	    emit({"'dec_",ObjName,"'(",{asis,FieldName},
		  ", Val, Telltype, [H|T]) ->",nl}),
	    case Type#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Val, Telltype, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,
			  "'(H, Val, Telltype, T)"})
	    end,
	    case more_genfields(Fields,Rest) of
		true ->
		    emit({";",nl});
		false ->
		    emit({".",nl})
	    end,
	    [];
	{false,_} ->
	    []
    end,
    gen_decode_objectfields(Class,ObjName,Rest,MaybeConstr ++ ConstrAcc);
gen_decode_objectfields(C,O,[H|T],CAcc) ->
    gen_decode_objectfields(C,O,T,CAcc);
gen_decode_objectfields(_,_,[],CAcc) ->
    CAcc.

gen_decode_constr_type([{Name,Def}|Rest]) ->
    emit({Name,"(Bytes,Telltype) ->",nl}),
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    asn1ct_gen:gen_decode_constructed(ber,Name,InnerType,Def),
    gen_decode_constr_type(Rest);
gen_decode_constr_type([]) ->
    ok.

more_genfields(Fields,[]) ->
    false;
more_genfields(Fields,[{FieldName,_}|T]) ->
    case is_typefield(Fields,FieldName) of
	true -> true;
	{false,objectfield} -> true;
	{false,_} -> more_genfields(Fields,T)
    end.

is_typefield(Fields,FieldName) ->
    case lists:keysearch(FieldName,2,Fields) of
	{value,Field} ->
	    case element(1,Field) of
		typefield ->
		    true;
		Other ->
		    {false,Other}
	    end;
	_ ->
	    false
    end.
%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
%%    {ClassName,ClassDef} = Def#'ObjectSet'.class,
    #'Externaltypereference'{module=ClassModule,
			     type=ClassName} = Def#'ObjectSet'.class,
    ClassDef = asn1_db:dbget(ClassModule,ClassName),
    UniqueFName = Def#'ObjectSet'.uniquefname,
    Set = Def#'ObjectSet'.set,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjSetName}),
    emit({nl,"%%================================",nl}),
    case ClassName of
	{Module,ExtClassName} ->
	    gen_objset_code(ObjSetName,UniqueFName,Set,ExtClassName,ClassDef);
	_ ->
	    gen_objset_code(ObjSetName,UniqueFName,Set,ClassName,ClassDef)
    end,
    emit(nl).

gen_objset_code(ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = (ClassDef#classdef.typespec)#objectclass.fields,
    gen_objset_enc(ObjSetName,UniqueFName,Set,ClassName,ClassFields),
    gen_objset_dec(ObjSetName,UniqueFName,Set,ClassName,ClassFields).

gen_objset_enc(_,{unique,undefined},_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_enc(ObjSName,UniqueName,
	       [{ObjName,Val,Fields},T|Rest],ClName,ClFields)->
%%    Value = 
%%	case Val of
%%	    {_,V} -> V;
%%	    V -> V
%%	end,
    emit({"'getenc_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_enc_funs(Fields,ClFields);
	Other ->
	    emit({"    fun 'enc_",ObjName,"'/3"})
    end,
    emit({";",nl}),
    gen_objset_enc(ObjSName,UniqueName,[T|Rest],ClName,ClFields);
gen_objset_enc(ObjSetName,UniqueName,
	       [{ObjName,Val,Fields}],ClName,ClFields) ->
%%    Value = 
%%	case Val of
%%	    {asn1_OK,V} -> V;
%%	    V -> V
%%	end,
    emit({"'getenc_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_enc_funs(Fields,ClFields);
	Other ->
	    emit({"    fun 'enc_",ObjName,"'/3"})
    end,
    emit({".",nl,nl}),
    ok;
gen_objset_enc(ObjSetName,UniqueName,['EXTENSIONMARK'],ClName,ClFields) ->
    emit({"'getenc_",ObjSetName,"'(Any1, Any2) ->",nl}),
    emit({indent(3),"fun(Attr, Val, Dummy) ->",nl}),
    emit({indent(6),"[{octets,Val}]",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_enc(_,_,[],_,_) ->
    ok.

gen_inlined_enc_funs(Fields,[{typefield,Name,_}|Rest]) ->
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, Dummy) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit_inner_of_fun(Type),
	    gen_inlined_enc_funs1(Fields,Rest);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, Dummy) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_fun(Type),
	    gen_inlined_enc_funs1(Fields,Rest);
	false ->
	    gen_inlined_enc_funs(Fields,Rest)
    end;
gen_inlined_enc_funs(Fields,[H|Rest]) ->
    gen_inlined_enc_funs(Fields,Rest);
gen_inlined_enc_funs(_,[]) ->
    ok.

gen_inlined_enc_funs1(Fields,[{typefield,Name,_}|Rest]) ->
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({";",nl}),
	    emit_inner_of_fun(Type);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({";",nl,indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_fun(Type);
	false ->
	    ok
    end,
    gen_inlined_enc_funs1(Fields,Rest);
gen_inlined_enc_funs1(Fields,[H|Rest])->
    gen_inlined_enc_funs1(Fields,Rest);
gen_inlined_enc_funs1(_,[]) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}).

emit_inner_of_fun(#typedef{name={ExtMod,Name}}) ->
    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val)"});
emit_inner_of_fun(#typedef{name=Name}) ->
    emit({indent(12),"'enc_",Name,"'(Val)"});
emit_inner_of_fun(Type) when record(Type,type) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_encode_prim(erules,Type,dotag,"Val");
	TRef when record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val)"})
    end.

indent(N) ->
    lists:duplicate(N,32). % 32 = space


gen_objset_dec(_,{unique,undefined},_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_dec(ObjSName,UniqueName,[{ObjName,Val,Fields},T|Rest],ClName,ClFields)->
%%    Value = 
%%	case Val of
%%	    {_,V} -> V;
%%	    V -> V
%%	end,
    emit({"'getdec_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_dec_funs(Fields,ClFields);
	Other ->
	    emit({"    fun 'dec_",ObjName,"'/4"})
    end,
    emit({";",nl}),
    gen_objset_dec(ObjSName,UniqueName,[T|Rest],ClName,ClFields);
gen_objset_dec(ObjSetName,UniqueName,[{ObjName,Val,Fields}],ClName,ClFields) ->
%%    Value = 
%%	case Val of
%%	    {_,V} -> V;
%%	    V -> V
%%	end,
    emit({"'getdec_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_dec_funs(Fields,ClFields);
	Other ->
	    emit({"    fun 'dec_",ObjName,"'/4"})
    end,
    emit({".",nl,nl}),
    ok;
gen_objset_dec(ObjSetName,UniqueName,['EXTENSIONMARK'],ClName,ClFields) ->
    emit({"'getdec_",ObjSetName,"'(Any1, Any2) ->",nl}),
    emit({indent(3),"fun(Attr1, Bytes, Attr3, Dummy) ->",nl}),
%%    emit({indent(6),"?RT_PER:decode_open_type(Bytes,[])",nl}),
    emit({indent(6),"{Bytes,Attr1}",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_dec(_,_,[],_,_) ->
    ok.

gen_inlined_dec_funs(Fields,[{typefield,Name,_}|Rest]) ->
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, Telltype, Dummy) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit_inner_of_decfun(Type),
	    gen_inlined_dec_funs1(Fields,Rest);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, Telltype, Dummy) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_decfun(Type),
	    gen_inlined_dec_funs1(Fields,Rest);
	false ->
	    gen_inlined_dec_funs(Fields,Rest)
    end;
gen_inlined_dec_funs(Fields,[H|Rest]) ->
    gen_inlined_dec_funs(Fields,Rest);
gen_inlined_dec_funs(_,[]) ->
    ok.

gen_inlined_dec_funs1(Fields,[{typefield,Name,_}|Rest]) ->
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({";",nl}),
	    emit_inner_of_decfun(Type);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({";",nl,indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_decfun(Type);
	false ->
	    ok
    end,
    gen_inlined_dec_funs1(Fields,Rest);
gen_inlined_dec_funs1(Fields,[H|Rest])->
    gen_inlined_dec_funs1(Fields,Rest);
gen_inlined_dec_funs1(_,[]) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}).

emit_inner_of_decfun(#typedef{name={ExtName,Name}}) ->
    emit({indent(12),"'",ExtName,"':'dec_",Name,"'(Val, Telltype)"});
emit_inner_of_decfun(#typedef{name=Name}) ->
    emit({indent(12),"'dec_",Name,"'(Val, Telltype)"});
emit_inner_of_decfun(Type) when record(Type,type) ->
    CurrMod = get(currmod),
    case Type#type.def of
	Def when atom(Def) ->
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_dec_prim(erules,Type,"Val");
	TRef when record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
		  T,"'(Val)"})
    end.

%% DECODING *****************************
%%***************************************


gen_decode(Erules,Type) when record(Type,typedef) ->
    D = Type,
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes,Telltype) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,D).

gen_decode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type,prop=Prop}) ->
    NewTname = [Cname|Tname],
    gen_decode(Erules,NewTname,Type);

gen_decode(Erules,Typename,Type) when record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    ObjFun =
		case Type#type.tablecinf of
		    [{objfun,_}|R] ->
			", ObjFun";
		    _ ->
			""
		end,
	    emit({nl,"'dec_",asn1ct_gen:list2name(Typename),
		  "'(Bytes,Telltype",ObjFun,") ->",nl}),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end.

dbdec(Type) when list(Type)->
    demit({"io:format(\"decoding: ",asn1ct_gen:list2name(Type),"~w~n\",[Bytes]),",nl});
dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).

gen_decode_user(Erules,D) when record(D,typedef) ->
    CurrMod = get(currmod),
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit({".",nl,nl});
	'ASN1_OPEN_TYPE' ->
	    gen_dec_prim(Erules,Def#type{def='ASN1_OPEN_TYPE'},"Bytes"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,Telltype)"}),
	    emit({".",nl,nl});
	#'Externaltypereference'{module=CurrMod,type=Etype} ->
	    emit({"'dec_",Etype,"'(Bytes,Telltype).",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'dec_",Etype,"'(Bytes,Telltype).",nl,nl});
	Other ->
	    exit({error,{asn1,{unknown,Other}}})
    end.


gen_dec_prim(Erules,Att,BytesVar) ->
    Typename = Att#type.def,
    Constraint = Att#type.constraint,
    case Typename of
	'INTEGER' ->
	    emit({"?RT_PER:decode_integer(",BytesVar,",",
		  {asis,Constraint},")"});
	{'INTEGER',NamedNumberList} ->
	    emit({"?RT_PER:decode_integer(",BytesVar,",",
		  {asis,Constraint},",",
		  {asis,NamedNumberList},")"});
	{'BIT STRING',NamedNumberList} ->
	    case get(compact_bit_string) of
		true ->
		    emit({"?RT_PER:decode_compact_bit_string(",
			  BytesVar,",",{asis,Constraint},",",
			  {asis,NamedNumberList},")"});
		_ ->
		    emit({"?RT_PER:decode_bit_string(",BytesVar,",",
			  {asis,Constraint},",",
			  {asis,NamedNumberList},")"})
	    end;
	'NULL' ->
	    emit({"?RT_PER:decode_null(",
		  BytesVar,")"});
	'OBJECT IDENTIFIER' ->
	    emit({"?RT_PER:decode_object_identifier(",
		  BytesVar,")"});
	'ObjectDescriptor' ->
	    emit({"?RT_PER:decode_ObjectDescriptor(",
		  BytesVar,")"});
	{'ENUMERATED',{NamedNumberList1,NamedNumberList2}} ->
	    NewTup = {list_to_tuple([X||{X,Y} <- NamedNumberList1]),
		      list_to_tuple([X||{X,Y} <- NamedNumberList2])},
	    NewC = [{'ValueRange',{0,size(element(1,NewTup))-1}}],
	    emit({"?RT_PER:decode_enumerated(",BytesVar,",",
		  {asis,NewC},",",
		  {asis,NewTup},")"});
	{'ENUMERATED',NamedNumberList} ->
	    NewTup = list_to_tuple([X||{X,Y} <- NamedNumberList]),
	    NewC = [{'ValueRange',{0,size(NewTup)-1}}],
	    emit({"?RT_PER:decode_enumerated(",BytesVar,",",
		  {asis,NewC},",",
		  {asis,NewTup},")"});
	'BOOLEAN'->
	    emit({"?RT_PER:decode_boolean(",BytesVar,")"});
	'OCTET STRING' ->
	    emit({"?RT_PER:decode_octet_string(",BytesVar,",",
		  {asis,Constraint},")"});
	'NumericString' ->
	    emit({"?RT_PER:decode_NumericString(",BytesVar,",",
		  {asis,Constraint},")"});
	'TeletexString' ->
	    emit({"?RT_PER:decode_TeletexString(",BytesVar,",",
		  {asis,Constraint},")"});
	'VideotexString' ->
	    emit({"?RT_PER:decode_VideotexString(",BytesVar,",",
		  {asis,Constraint},")"});
	'UTCTime' ->
	    emit({"?RT_PER:decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GeneralizedTime' ->
	    emit({"?RT_PER:decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GraphicString' ->
	    emit({"?RT_PER:decode_GraphicString(",BytesVar,",",
		  {asis,Constraint},")"});
	'VisibleString' ->
	    emit({"?RT_PER:decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GeneralString' ->
	    emit({"?RT_PER:decode_GeneralString(",BytesVar,",",
		  {asis,Constraint},")"});
	'PrintableString' ->
	    emit({"?RT_PER:decode_PrintableString(",BytesVar,",",{asis,Constraint},")"});
	'IA5String' ->
	    emit({"?RT_PER:decode_IA5String(",BytesVar,",",{asis,Constraint},")"});
	'BMPString' ->
	    emit({"?RT_PER:decode_BMPString(",BytesVar,",",{asis,Constraint},")"});
	'UniversalString' ->
	    emit({"?RT_PER:decode_UniversalString(",BytesVar,",",{asis,Constraint},")"});
	'ANY' ->
	    emit(["?RT_PER:decode_open_type(",BytesVar,",", 
		  {asis,Constraint}, ")"]); 
	'ASN1_OPEN_TYPE' ->
	    case Constraint of
		[#'Externaltypereference'{type=Tname}] ->
		    emit(["fun(FBytes) ->",nl,
			  "   {XTerm,XBytes} = "]),
		    emit(["?RT_PER:decode_open_type(",BytesVar,",[]),",nl]),
		    emit(["   {YTerm,_} = dec_",Tname,"(XTerm,mandatory),",nl]),
		    emit(["   {YTerm,XBytes} end(",BytesVar,")"]);
		[#type{def=#'Externaltypereference'{type=Tname}}] ->
		    emit(["fun(FBytes) ->",nl,
			  "   {XTerm,XBytes} = "]),
		    emit(["?RT_PER:decode_open_type(",BytesVar,",[]),",nl]),
		    emit(["   {YTerm,_} = dec_",Tname,"(XTerm,mandatory),",nl]),
		    emit(["   {YTerm,XBytes} end(",BytesVar,")"]);
		_ ->
		    emit(["?RT_PER:decode_open_type(",BytesVar,",[])"])
	    end;
	Other ->
	    exit({'cant decode' ,Other})
    end.


    
		     
    
