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
-module(asn1ct_gen_ber_bin_v2).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([pgen/4]).
-export([decode_class/1, decode_type/1]).
-export([add_removed_bytes/0]).
-export([gen_encode/2,gen_encode/3,gen_decode/2,gen_decode/3]).
-export([gen_encode_prim/4]).
-export([gen_dec_prim/7]).
-export([gen_objectset_code/2, gen_obj_code/3]).
-export([encode_tag_val/3]).

-import(asn1ct_gen, [emit/1,demit/1]).

						% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

						% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).


-define(T_ObjectDescriptor, ?UNIVERSAL bor ?PRIMITIVE bor 7).
						% restricted character string types
-define(T_NumericString,    ?UNIVERSAL bor ?PRIMITIVE bor 18). %can be constructed
-define(T_PrintableString,  ?UNIVERSAL bor ?PRIMITIVE bor 19). %can be constructed
-define(T_TeletexString,    ?UNIVERSAL bor ?PRIMITIVE bor 20). %can be constructed
-define(T_VideotexString,   ?UNIVERSAL bor ?PRIMITIVE bor 21). %can be constructed
-define(T_IA5String,        ?UNIVERSAL bor ?PRIMITIVE bor 22). %can be constructed
-define(T_GraphicString,    ?UNIVERSAL bor ?PRIMITIVE bor 25). %can be constructed
-define(T_VisibleString,    ?UNIVERSAL bor ?PRIMITIVE bor 26). %can be constructed
-define(T_GeneralString,    ?UNIVERSAL bor ?PRIMITIVE bor 27). %can be constructed

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList,PTypeList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    asn1ct_gen:pgen_module(OutFile,Erules,Module,TypeOrVal,true).


%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Generate ENCODING
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% encode #{typedef, {pos, name, typespec}}
%%===============================================================================

gen_encode(Erules,Type) when record(Type,typedef) ->
    gen_encode_user(Erules,Type).

%%===============================================================================
%% encode #{type, {tag, def, constraint}}
%%===============================================================================

gen_encode(Erules,Typename,Type) when record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    ObjFun =
	case lists:keysearch(objfun,1,Type#type.tablecinf) of
	    {value,{_,_Name}} ->
		", ObjFun";
	    false ->
		""
	end,

    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit([nl,nl,nl,"%%================================"]),
	    emit([nl,"%%  ",asn1ct_gen:list2name(Typename)]),
	    emit([nl,"%%================================",nl]),
	    case length(Typename) of
		1 -> % top level type
		    emit(["'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val",ObjFun,") ->",nl]),
		    emit(["    'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, ", {asis,lists:reverse(Type#type.tag)},ObjFun,").",nl,nl]);
		_ -> % embedded type with constructed name
		    true
	    end,
	    case lists:member(InnerType,['SET','SEQUENCE']) of
		true -> 
		    case get(asn_keyed_list) of
			true ->
			    CompList = 
				case Type#type.def of
				    #'SEQUENCE'{components=Cl} -> Cl;
				    #'SET'{components=Cl} -> Cl
				end,
			    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
				  "'(Val, TagIn",ObjFun,
				  ") when list(Val) ->",nl]),
			    emit(["    'enc_",asn1ct_gen:list2name(Typename),
				  "'(?RT_BER:fixoptionals(",
				  {asis,optionals(CompList)},
				  ",Val), TagIn",ObjFun,");",nl,nl]);
			_ -> true
		    end;
		_ ->
		    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'({'",asn1ct_gen:list2name(Typename),
			  "',Val}, TagIn",ObjFun,") ->",nl]),
		    emit(["   'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, TagIn",ObjFun,");",nl,nl])
	    end,
	    emit(["'enc_",asn1ct_gen:list2name(Typename),
		  "'(Val, TagIn",ObjFun,") ->",nl,"   "]),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;

%%===============================================================================
%% encode ComponentType
%%===============================================================================

gen_encode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,_Prop,_Tags}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_encode(Erules,NewTname,NewType).

gen_encode_user(Erules,D) when record(D,typedef) ->
    Typename = [D#typedef.name],
    Type = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    OTag = Type#type.tag,
    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    emit([nl,nl,"%%================================"]),
    emit([nl,"%%  ",Typename]),
    emit([nl,"%%================================",nl]),
    emit(["'enc_",asn1ct_gen:list2name(Typename),
	  "'(Val",") ->",nl]),
    emit(["    'enc_",asn1ct_gen:list2name(Typename),
	  "'(Val, ", {asis,lists:reverse(Tag)},").",nl,nl]),

    case lists:member(InnerType,['SET','SEQUENCE']) of
	true -> 
	    case get(asn_keyed_list) of
		true ->
		    CompList = 
			case Type#type.def of
			    #'SEQUENCE'{components=Cl} -> Cl;
			    #'SET'{components=Cl} -> Cl
			end,

		    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),
			  "'(Val, TagIn) when list(Val) ->",nl]),
		    emit(["    'enc_",asn1ct_gen:list2name(Typename),
			  "'(?RT_BER:fixoptionals(",
			  {asis,optionals(CompList)},
			  ",Val), TagIn);",nl,nl]);
		_ -> true
	    end;
	_ ->
	    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),
		  "'({'",asn1ct_gen:list2name(Typename),"',Val}, TagIn) ->",nl}),
	    emit({"   'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn);",nl,nl})
    end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn) ->",nl}),
    CurrentMod = get(currmod),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	{primitive,bif} ->
	    gen_encode_prim(ber,Type,"TagIn","Val"),
	    emit([".",nl]);
	#typereference{val=Ename} ->
	    emit(["   'enc_",Ename,"'(Val, TagIn).",nl]);
	#'Externaltypereference'{module=CurrentMod,type=Etype} ->
	    emit(["   'enc_",Etype,"'(Val, TagIn).",nl]);
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, TagIn).",nl]);
	'ASN1_OPEN_TYPE' ->
	    emit(["%% OPEN TYPE",nl]),
	    gen_encode_prim(ber,
			    Type#type{def='ASN1_OPEN_TYPE'},
			    "TagIn","Val"),
	    emit([".",nl])
    end.

gen_encode_prim(_Erules,D,DoTag,Value) when record(D,type) ->
    
%%% Constraint is currently not used for BER (except for BitString) and therefore replaced
%%% with [] as a placeholder
    BitStringConstraint = D#type.constraint,
    Constraint = [],
    asn1ct_name:new(enumval),
    case D#type.def of
	'BOOLEAN' ->
	    emit_encode_func('boolean',Value,DoTag);
	'INTEGER' ->
	    emit_encode_func('integer',Constraint,Value,DoTag);
	{'INTEGER',NamedNumberList} ->
	    emit_encode_func('integer',Constraint,Value,
			     NamedNumberList,DoTag);
	{'ENUMERATED',NamedNumberList={_,_}} ->
	    
	    emit(["case (case ",Value," of {asn1_enum,_}->",Value,";{_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);
	{'ENUMERATED',NamedNumberList} ->
	    
	    emit(["case (case ",Value," of {_,_}->element(2,",Value,");_->",
		  Value," end) of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);

	{'BIT STRING',NamedNumberList} ->
	    emit_encode_func('bit_string',BitStringConstraint,Value,
			     NamedNumberList,DoTag);
	'ANY' ->
	    emit_encode_func('open_type', Value,DoTag);
	'NULL' ->
	    emit_encode_func('null',Value,DoTag);
	'OBJECT IDENTIFIER' ->
	    emit_encode_func("object_identifier",Value,DoTag);
	'ObjectDescriptor' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_ObjectDescriptor,DoTag);
	'OCTET STRING' ->
	    emit_encode_func('octet_string',Constraint,Value,DoTag);
	'NumericString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_NumericString,DoTag);
	'TeletexString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_TeletexString,DoTag);
	'VideotexString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_VideotexString,DoTag);
	'GraphicString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_GraphicString,DoTag);
	'VisibleString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_VisibleString,DoTag);
	'GeneralString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_GeneralString,DoTag);
	'PrintableString' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_PrintableString,DoTag);
	'IA5String' ->
	    emit_encode_func('restricted_string',Constraint,Value,
			     ?T_IA5String,DoTag);
	'UniversalString' ->
	    emit_encode_func('universal_string',Constraint,Value,DoTag);
	'BMPString' ->
	    emit_encode_func('BMP_string',Constraint,Value,DoTag);
	'UTCTime' ->
	    emit_encode_func('utc_time',Constraint,Value,DoTag);
	'GeneralizedTime' ->
	    emit_encode_func('generalized_time',Constraint,Value,DoTag);
	'ASN1_OPEN_TYPE' ->
	    emit_encode_func('open_type', Value,DoTag);
	XX ->
	    exit({'can not encode' ,XX})
    end.


emit_encode_func(Name,Value,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Value,Tags);
emit_encode_func(Name,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Constraint,Value,Tags);
emit_encode_func(Name,Constraint,Value,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",{asis,Constraint},", ",Value,", ",Tags,")"]).

emit_encode_func(Name,Constraint,Value,Asis,Tags) when atom(Name) ->
    emit_encode_func(atom_to_list(Name),Constraint,Value,Asis,Tags);
emit_encode_func(Name,Constraint,Value,Asis,Tags) ->
    Fname = "?RT_BER:encode_" ++ Name,
    emit([Fname,"(",{asis,Constraint},", ",Value,
	  ", ",{asis,Asis},
	  ", ",Tags,")"]).
    
emit_enc_enumerated_cases({L1,L2}, Tags) ->
    emit_enc_enumerated_cases(L1++L2, Tags, ext);
emit_enc_enumerated_cases(L, Tags) ->
    emit_enc_enumerated_cases(L, Tags, noext).

emit_enc_enumerated_cases([{EnumName,EnumVal},H2|T], Tags, Ext) ->
    emit([{asis,EnumName}," -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,");",nl]),
%%    emit(["'",{asis,EnumName},"' -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,");",nl]),
    emit_enc_enumerated_cases([H2|T], Tags, Ext);
emit_enc_enumerated_cases([{EnumName,EnumVal}], Tags, Ext) ->
    emit([{asis,EnumName}," -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,")"]),
%%    emit(["'",{asis,EnumName},"' -> ?RT_BER:encode_enumerated(",EnumVal,",",Tags,")"]),
    case Ext of
	noext -> emit([";",nl]);
	ext -> 
	    emit([";",nl,"{asn1_enum,",{curr,enumval},"} -> ",
		     "?RT_BER:encode_enumerated(",{curr,enumval},",",Tags,");",nl]),
	    asn1ct_name:new(enumval)
    end,
    emit([{curr,enumval}," -> exit({error,{asn1, {enumerated_not_in_range,",{curr, enumval},"}}})"]),
    emit([nl,"end"]).


%%===============================================================================
%%===============================================================================
%%===============================================================================
%% Generate DECODING
%%===============================================================================
%%===============================================================================
%%===============================================================================

%%===============================================================================
%% decode #{typedef, {pos, name, typespec}}
%%===============================================================================

gen_decode(Erules,Type) when record(Type,typedef) ->
    Def = Type#typedef.typespec,
%    InnerType = asn1ct_gen:get_inner(Def#type.def),
    InnerTag = Def#type.tag ,
%    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- InnerTag],
    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- InnerTag],
%    BytesVar = "Tlv",
    emit({nl,nl}),
    emit(["'dec_",Type#typedef.name,"'(Tlv) ->",nl]),
    emit(["   'dec_",Type#typedef.name,"'(Tlv, ",{asis,Tag},").",nl,nl]),
    emit(["'dec_",Type#typedef.name,"'(Tlv, TagIn) ->",nl]),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,Type).


%%===============================================================================
%% decode #{type, {tag, def, constraint}}
%%===============================================================================

gen_decode(Erules,Tname,Type) when record(Type,type) ->
    Typename = Tname,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    ObjFun =
		case Type#type.tablecinf of
		    [{objfun,_}|_R] ->
			", ObjFun";
		    _ ->
			""
		end,
	    emit(["'dec_",asn1ct_gen:list2name(Typename),"'(Tlv, TagIn",ObjFun,") ->",nl]),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;


%%===============================================================================
%% decode ComponentType
%%===============================================================================

gen_decode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,_Prop,_Tags}) ->
    NewTname = [Cname|Tname],
    %% The tag is set to [] to avoid that it is
    %% taken into account twice, both as a component/alternative (passed as
    %% argument to the encode decode function and within the encode decode
    %% function it self.
    NewType = Type#type{tag=[]},
    gen_decode(Erules,NewTname,NewType).


gen_decode_user(Erules,D) when record(D,typedef) ->
    Typename = [D#typedef.name],
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    BytesVar = "Tlv",
    case asn1ct_gen:type(InnerType) of
	'ASN1_OPEN_TYPE' ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def#type{def='ASN1_OPEN_TYPE'}, 
			 BytesVar,{string,"TagIn"}, [] , 
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{primitive,bif} ->
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def, BytesVar,{string,"TagIn"},[] , 
			 ?PRIMITIVE,"OptOrMand"),
	    emit([".",nl,nl]);
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	TheType ->
	    DecFunName = mkfuncname(TheType,dec),
	    emit([DecFunName,"(",BytesVar,
		  ", TagIn)"]),
	    emit([".",nl,nl])
    end.


gen_dec_prim(_Erules,Att,BytesVar,DoTag,_TagIn,_Form,_OptOrMand) ->
    Typename = Att#type.def,
%% Currently not used for BER replaced with [] as place holder
%%    Constraint = Att#type.constraint,
%% Constraint = [],
    Constraint = 
	case get_constraint(Att#type.constraint,'SizeConstraint') of
	    no -> [];
	    Tc -> Tc
	end,
    ValueRange = 
	case get_constraint(Att#type.constraint,'ValueRange') of
	    no -> [];
	    Tv -> Tv
	end,
    SingleValue = 
	case get_constraint(Att#type.constraint,'SingleValue') of
	    no -> [];
	    Sv -> Sv
	end,
    AsBin = case get(binary_strings) of
		true -> "_as_bin";
		_ -> ""
	    end,
    NewTypeName = case Typename of
		      'ANY' -> 'ASN1_OPEN_TYPE';
		      _ -> Typename
		  end,
    DoLength = 
	case NewTypeName of
	    'BOOLEAN'->
		emit({"?RT_BER:decode_boolean(",BytesVar,","}),
		add_func({decode_boolean,2});
	    'INTEGER' ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,int_constr(SingleValue,ValueRange)},","}),
		add_func({decode_integer,3});
	    {'INTEGER',NamedNumberList} ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,int_constr(SingleValue,ValueRange)},",",
		      {asis,NamedNumberList},","}),
		add_func({decode_integer,4});
	    {'ENUMERATED',NamedNumberList} ->
		emit({"?RT_BER:decode_enumerated(",BytesVar,",",
		      {asis,Constraint},",",
		      {asis,NamedNumberList},","}),
		add_func({decode_enumerated,4});
	    {'BIT STRING',NamedNumberList} ->
		case get(compact_bit_string) of
		    true ->
			emit({"?RT_BER:decode_compact_bit_string(",
			      BytesVar,",",{asis,Constraint},",",
			      {asis,NamedNumberList},","}),
			add_func({decode_compact_bit_string,4});
		    _ ->
			emit({"?RT_BER:decode_bit_string(",BytesVar,",",
			      {asis,Constraint},",",
			      {asis,NamedNumberList},","}),
			add_func({decode_bit_string,4})
		end;
	    'NULL' ->
		emit({"?RT_BER:decode_null(",BytesVar,","}),
		add_func({decode_null,2});
	    'OBJECT IDENTIFIER' ->
		emit({"?RT_BER:decode_object_identifier(",BytesVar,","}),
		add_func({decode_object_identifier,2});
	    'ObjectDescriptor' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_ObjectDescriptor},","}),
		add_func({decode_restricted_string,4});
	    'OCTET STRING' ->
		emit({"?RT_BER:decode_octet_string",AsBin,"(",BytesVar,",",{asis,Constraint},","}),
		add_func({decode_octet_string,3});
	    'NumericString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_NumericString},","}),
		add_func({decode_restricted_string,4});
	    'TeletexString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_TeletexString},","}),
		add_func({decode_restricted_string,4});
	    'VideotexString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VideotexString},","}),
		add_func({decode_restricted_string,4});
	    'GraphicString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GraphicString},","}),
		add_func({decode_restricted_string,4});
	    'VisibleString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VisibleString},","}),
		add_func({decode_restricted_string,4});
	    'GeneralString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GeneralString},","}),
		add_func({decode_restricted_string,4});
	    'PrintableString' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_PrintableString},","}),
		add_func({decode_restricted_string,4});
	    'IA5String' ->
		emit({"?RT_BER:decode_restricted_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_IA5String},","}),
		add_func({decode_restricted_string,4}) ;
	    'UniversalString' ->
		emit({"?RT_BER:decode_universal_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		add_func({decode_universal_string,3});
	    'BMPString' ->
		emit({"?RT_BER:decode_BMP_string",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		add_func({decode_BMP_string,3});
	    'UTCTime' ->
		emit({"?RT_BER:decode_utc_time",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		add_func({decode_utc_time,3});
	    'GeneralizedTime' ->
		emit({"?RT_BER:decode_generalized_time",AsBin,"(",
		      BytesVar,",",{asis,Constraint},","}),
		add_func({decode_generalized_time,3});
	    'ASN1_OPEN_TYPE' ->
		emit(["?RT_BER:decode_open_type_as_binary(",
		      BytesVar,","]),
		add_func({decode_open_type_as_binary,2});
	    Other ->
		exit({'can not decode' ,Other})
	end,
    
    case {DoTag,NewTypeName} of
	{{string,TagStr},'ASN1_OPEN_TYPE'} ->
	    emit([TagStr,")"]);
	{_,'ASN1_OPEN_TYPE'} ->
	    emit([{asis,DoTag},")"]);
	{{string,TagStr},_} ->
	    emit([TagStr,")"]);
	_ when list(DoTag) ->
	    emit([{asis,DoTag},")"])
    end.


int_constr([],[]) ->
    [];
int_constr([],ValueRange) ->
    ValueRange;
int_constr(SingleValue,[]) ->
    SingleValue;
int_constr(SV,VR) ->
    [SV,VR].
    
%% Object code generating for encoding and decoding
%% ------------------------------------------------

gen_obj_code(_Erules,_Module,Obj) when record(Obj,typedef) ->
    ObjName = Obj#typedef.name,
    Def = Obj#typedef.typespec,
    #'Externaltypereference'{module=M,type=ClName} = Def#'Object'.classname,
    Class = asn1_db:dbget(M,ClName),
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
    gen_decode_constr_type(DecConstructed);
gen_obj_code(_Erules,_Module,Obj) when record(Obj,pobjectdef) ->
    ok.

gen_encode_objectfields(Class,ObjName,[{FieldName,Type}|Rest],ConstrAcc) ->
    Fields = Class#objectclass.fields,
    MaybeConstr=
	case is_typefield(Fields,FieldName) of
	    true ->
		Def = Type#typedef.typespec,
%		OTag = Def#type.tag,
% remove	Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%		Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| 
%			  X <- OTag],
		emit({"'enc_",ObjName,"'(",{asis,FieldName},
		      ", Val, RestPrimFieldName) ->",nl}),
		CAcc=
		case Type#typedef.name of
		    {primitive,bif} -> %%tag should be the primitive tag
			OTag = Def#type.tag,
			Tag = [encode_tag_val(decode_class(X#tag.class),
					      X#tag.form,X#tag.number)||
				  X <- OTag],
			gen_encode_prim(ber,Def,{asis,lists:reverse(Tag)},
					"Val"),
			[];
		    {constructed,bif} ->
			%%InnerType = asn1ct_gen:get_inner(Def#type.def),
			%%asn1ct_gen:gen_encode_constructed(ber,[ObjName],
			%%                            InnerType,Def);
			emit({"   'enc_",ObjName,'_',FieldName,
			      "'(Val)"}),
			[{['enc_',ObjName,'_',FieldName],Def}];
		    {ExtMod,TypeName} ->
			emit({"   '",ExtMod,"':'enc_",TypeName,
			      "'(Val)"}),
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
		  ", Val,[H|T]) ->",nl}),
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
gen_encode_objectfields(C,O,[_|T],Acc) ->
    gen_encode_objectfields(C,O,T,Acc);
gen_encode_objectfields(_,_,[],Acc) ->
    Acc.

gen_encode_constr_type([{Name,Def}|Rest]) ->
    emit({Name,"(Val,TagIn) ->",nl}),
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    asn1ct_gen:gen_encode_constructed(ber,Name,InnerType,Def),
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
		  ", Bytes, RestPrimFieldName) ->",nl}),
	    OTag = Def#type.tag,
%	    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
	    Tag = [(decode_class(X#tag.class) bsl 10) + X#tag.number || X <- OTag],
	    Prop = 
		case get_optionalityspec(Fields,FieldName) of
		    'OPTIONAL' -> opt_or_default;
		    {'DEFAULT',_} -> opt_or_default;
		    _ -> mandatory
		end,
	    CAcc =
	    case Type#typedef.name of
		{primitive,bif} ->
		    gen_dec_prim(ber,Def,"Bytes",Tag,"TagIn",
				 ?PRIMITIVE,Prop),
		    [];
		{constructed,bif} ->
%		    emit({"   'dec_",ObjName,'_',FieldName,"'(Bytes,",
%			  {asis,Prop},")"}),
		    emit(["   'dec_",ObjName,'_',FieldName,"'(Bytes)"]),
		    [{['dec_',ObjName,'_',FieldName],Def}];
		{ExtMod,TypeName} ->
% 		    emit({"   '",ExtMod,"':'dec_",TypeName,"'(Bytes, ",
% 			  {asis,Prop},")"}),
		    emit(["   '",ExtMod,"':'dec_",TypeName,"'(Bytes)"]),
		    [];
		TypeName ->
% 		    emit({"   'dec_",TypeName,"'(Bytes, ",{asis,Prop},
% 			  ")"}),
		    emit(["   'dec_",TypeName,"'(Bytes)"]),
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
		  ", Bytes, [H|T]) ->",nl}),
	    case Type#typedef.name of
		{ExtMod,TypeName} ->
		    emit({indent(3),"'",ExtMod,"':'dec_",TypeName,
			  "'(H, Bytes, T)"});
		TypeName ->
		    emit({indent(3),"'dec_",TypeName,
			  "'(H, Bytes, T)"})
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
gen_decode_objectfields(C,O,[_|T],CAcc) ->
    gen_decode_objectfields(C,O,T,CAcc);
gen_decode_objectfields(_,_,[],CAcc) ->
    CAcc.

gen_decode_constr_type([{Name,Def}|Rest]) ->
    emit([Name,"(Bytes, TagIn) ->",nl]),
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    asn1ct_gen:gen_decode_constructed(ber,Name,InnerType,Def),
    gen_decode_constr_type(Rest);
gen_decode_constr_type([]) ->
    ok.
    
more_genfields(_Fields,[]) ->
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

get_optionalityspec(Fields,FieldName) ->
    case lists:keysearch(FieldName,2,Fields) of
	{value,Field} when tuple(Field) ->
	    element(size(Field),Field);
	_ -> false
    end.


%% Object Set code generating for encoding and decoding
%% ----------------------------------------------------
gen_objectset_code(Erules,ObjSet) ->
    ObjSetName = ObjSet#typedef.name,
    Def = ObjSet#typedef.typespec,
%    {ClassName,ClassDef} = Def#'ObjectSet'.class,
    #'Externaltypereference'{module=ClassModule,
			     type=ClassName} = Def#'ObjectSet'.class,
    ClassDef = asn1_db:dbget(ClassModule,ClassName),
    UniqueFName = Def#'ObjectSet'.uniquefname,
    Set = Def#'ObjectSet'.set,
    emit({nl,nl,nl,"%%================================"}),
    emit({nl,"%%  ",ObjSetName}),
    emit({nl,"%%================================",nl}),
    case ClassName of
	{_Module,ExtClassName} ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ExtClassName,ClassDef);
	_ ->
	    gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)
    end,
    emit(nl).

gen_objset_code(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassDef)->
    ClassFields = (ClassDef#classdef.typespec)#objectclass.fields,
    gen_objset_enc(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassFields),
    gen_objset_dec(Erules,ObjSetName,UniqueFName,Set,ClassName,ClassFields).

gen_objset_enc(_,_,{unique,undefined},_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_enc(Erules,ObjSName,UniqueName,
	       [{ObjName,Val,Fields},T|Rest],ClName,ClFields)->
    emit({"'getenc_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_enc_funs(Fields,ClFields);
	_ ->
	    emit({"    fun 'enc_",ObjName,"'/3"})
    end,
    emit({";",nl}),
    gen_objset_enc(Erules,ObjSName,UniqueName,[T|Rest],ClName,ClFields);
gen_objset_enc(_,ObjSetName,UniqueName,
	       [{ObjName,Val,Fields}],_ClName,ClFields) ->
    emit({"'getenc_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_enc_funs(Fields,ClFields);
	_ ->
	    emit({"    fun 'enc_",ObjName,"'/3"})
    end,
    emit({".",nl,nl}),
    ok;
%% See X.681 Annex E for the following case
gen_objset_enc(_,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,_ClFields) ->
    emit({"'getenc_",ObjSetName,"'(Any1, Any2) ->",nl}),
    emit({indent(3),"fun(Attr, Val, RestPrimFieldName) ->",nl}),
    emit({indent(6),"Len = case Val of",nl,indent(9),
 	  "Bin when binary(Bin) -> size(Bin);",nl,indent(9),
 	  "_ -> length(Val)",nl,indent(6),"end,"}),
    emit({indent(6),"{Val,Len}",nl}),
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_enc(_,_,_,[],_,_) ->
    ok.

gen_inlined_enc_funs(Fields,[{typefield,Name,_}|Rest]) ->
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Val, RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit_inner_of_fun(Type),
	    gen_inlined_enc_funs1(Fields,Rest);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Val, RestPrimFieldName) ->",nl,
		  indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_fun(Type),
	    gen_inlined_enc_funs1(Fields,Rest);
	false ->
	    gen_inlined_enc_funs(Fields,Rest)
    end;
gen_inlined_enc_funs(Fields,[_|Rest]) ->
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
gen_inlined_enc_funs1(Fields,[_|Rest])->
    gen_inlined_enc_funs1(Fields,Rest);
gen_inlined_enc_funs1(_,[]) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}).

emit_inner_of_fun(#typedef{name={ExtMod,Name}}) ->
%    OTag = Type#type.tag,
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    emit({indent(12),"'",ExtMod,"':'enc_",Name,"'(Val)"});
emit_inner_of_fun(#typedef{name=Name}) ->
%    OTag = Type#type.tag,
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    emit({indent(12),"'enc_",Name,"'(Val)"});
emit_inner_of_fun(Type) when record(Type,type) ->
    CurrMod = get(currmod),
%    OTag = Type#type.tag,
% remove    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
%    Tag = [encode_tag_val(decode_class(X#tag.class),X#tag.form,X#tag.number)|| X <- OTag],
    case Type#type.def of
	Def when atom(Def) ->
	    OTag = Type#type.tag,
	    Tag = [encode_tag_val(decode_class(X#tag.class),
				  X#tag.form,X#tag.number)||X <- OTag],
	    emit({indent(9),Def," ->",nl,indent(12)}),
	    gen_encode_prim(ber,Type,{asis,lists:reverse(Tag)},"Val");
	TRef when record(TRef,typereference) ->
	    T = TRef#typereference.val,
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),"'enc_",T,
		  "'(Val)"});
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit({indent(9),T," ->",nl,indent(12),ExtMod,":'enc_",
		  T,"'(Val)"})
    end.

indent(N) ->
    lists:duplicate(N,32). % 32 = space


gen_objset_dec(_,_,{unique,undefined},_,_,_) ->
    %% There is no unique field in the class of this object set
    %% don't bother about the constraint
    ok;
gen_objset_dec(Erules,ObjSName,UniqueName,[{ObjName,Val,Fields},T|Rest],ClName,ClFields)->
    emit({"'getdec_",ObjSName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_dec_funs(Fields,ClFields);
	_ ->
	    emit({"    fun 'dec_",ObjName,"'/3"})
    end,
    emit({";",nl}),
    gen_objset_dec(Erules,ObjSName,UniqueName,[T|Rest],ClName,ClFields);
gen_objset_dec(_,ObjSetName,UniqueName,[{ObjName,Val,Fields}],_ClName,ClFields) ->
    emit({"'getdec_",ObjSetName,"'(",{asis,UniqueName},",",{asis,Val},") ->",nl}),
    case ObjName of
	no_name ->
	    gen_inlined_dec_funs(Fields,ClFields);
	_ ->
	    emit({"    fun 'dec_",ObjName,"'/3"})
    end,
    emit({".",nl,nl}),
    ok;
gen_objset_dec(Erules,ObjSetName,_UniqueName,['EXTENSIONMARK'],_ClName,_ClFields) ->
    emit({"'getdec_",ObjSetName,"'(_Any1, _Any2) ->",nl}),
    emit({indent(3),"fun(_Attr1,Bytes, _RestPrimFieldName) ->",nl}),
    case Erules of
	ber_bin_v2 ->
	    emit({indent(6),"?RT_BER:encode(Bytes)",nl});
	_ ->
	    emit({indent(6),"Len = case Bytes of",nl,indent(9),
		  "Bin when binary(Bin) -> size(Bin);",nl,indent(9),
		  "_ -> length(Bytes)",nl,indent(6),"end,"}),
	    emit({indent(6),"{Bytes,[],Len}",nl})
    end,
    emit({indent(3),"end.",nl,nl}),
    ok;
gen_objset_dec(_,_,_,[],_,_) ->
    ok.

gen_inlined_dec_funs(Fields,[{typefield,Name,Prop}|Rest]) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({indent(3),"fun(Type, Bytes, RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl}),
	    emit_inner_of_decfun(Type,DecProp),
	    gen_inlined_dec_funs1(Fields,Rest);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({indent(3),"fun(Type, Bytes, RestPrimFieldName) ->",
		  nl,indent(6),"case Type of",nl}),
	    emit({indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_decfun(Type,DecProp),
	    gen_inlined_dec_funs1(Fields,Rest);
	false ->
	    gen_inlined_dec_funs(Fields,Rest)
    end;
gen_inlined_dec_funs(Fields,[_H|Rest]) ->
    gen_inlined_dec_funs(Fields,Rest);
gen_inlined_dec_funs(_,[]) ->
    ok.

gen_inlined_dec_funs1(Fields,[{typefield,Name,Prop}|Rest]) ->
    DecProp = case Prop of
		  'OPTIONAL' -> opt_or_default;
		  {'DEFAULT',_} -> opt_or_default;
		  _ -> mandatory
	      end,
    case lists:keysearch(Name,1,Fields) of
	{value,{_,Type}} when record(Type,type) ->
	    emit({";",nl}),
	    emit_inner_of_decfun(Type,DecProp);
	{value,{_,Type}} when record(Type,typedef) ->
	    emit({";",nl,indent(9),{asis,Name}," ->",nl}),
	    emit_inner_of_decfun(Type,DecProp);
	false ->
	    ok
    end,
    gen_inlined_dec_funs1(Fields,Rest);
gen_inlined_dec_funs1(Fields,[_|Rest])->
    gen_inlined_dec_funs1(Fields,Rest);
gen_inlined_dec_funs1(_,[]) ->
    emit({nl,indent(6),"end",nl}),
    emit({indent(3),"end"}).

emit_inner_of_decfun(#typedef{name={ExtName,Name}},_Prop) ->
    emit([indent(12),"'",ExtName,"':'dec_",Name,"'(Bytes)"]);
emit_inner_of_decfun(#typedef{name=Name},_Prop) ->
    emit([indent(12),"'dec_",Name,"'(Bytes)"]);
emit_inner_of_decfun(Type,Prop) when record(Type,type) ->
    OTag = Type#type.tag,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    CurrMod = get(currmod),
    Def = Type#type.def,
    InnerType = asn1ct_gen:get_inner(Def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} -> 
	    emit([indent(9),Def," ->",nl,indent(12)]),
	    gen_dec_prim(ber,Type,"Bytes",Tag,"TagIn",
			 ?PRIMITIVE,Prop);
%	TRef when record(TRef,typereference) ->
%	    T = TRef#typereference.val,
%	    emit({indent(9),T," ->",nl,indent(12),"'dec_",T,"'(Val)"});
	#'Externaltypereference'{module=CurrMod,type=T} ->
	    emit([indent(9),T," ->",nl,indent(12),"'dec_",T,
%		  "'(Bytes, ",Prop,")"]);
		  "'(Bytes)"]);
	#'Externaltypereference'{module=ExtMod,type=T} ->
	    emit([indent(9),T," ->",nl,indent(12),ExtMod,":'dec_",
%		  T,"'(Bytes, ",Prop,")"])
		  T,"'(Bytes)"])
    end.


dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).


decode_class('UNIVERSAL') ->
    ?UNIVERSAL;
decode_class('APPLICATION') ->
    ?APPLICATION;
decode_class('CONTEXT') ->
    ?CONTEXT;
decode_class('PRIVATE') ->
    ?PRIVATE.

decode_type('BOOLEAN') -> 1;
decode_type('INTEGER') -> 2;
decode_type('BIT STRING') -> 3; 
decode_type('OCTET STRING') -> 4; 
decode_type('NULL') -> 5;
decode_type('OBJECT IDENTIFIER') -> 6;
decode_type('OBJECT DESCRIPTOR') -> 7;
decode_type('EXTERNAL') -> 8;
decode_type('REAL') -> 9;
decode_type('ENUMERATED') -> 10;
decode_type('EMBEDDED_PDV') -> 11;
decode_type('SEQUENCE') -> 16;
decode_type('SEQUENCE OF') -> 16;
decode_type('SET') -> 17;
decode_type('SET OF') -> 17;
decode_type('NumericString') -> 18;  
decode_type('PrintableString') -> 19;  
decode_type('TeletexString') -> 20;  
decode_type('VideotexString') -> 21;  
decode_type('IA5String') -> 22;  
decode_type('UTCTime') -> 23;  
decode_type('GeneralizedTime') -> 24;  
decode_type('GraphicString') -> 25;  
decode_type('VisibleString') -> 26;  
decode_type('GeneralString') -> 27;  
decode_type('UniversalString') -> 28;  
decode_type('BMPString') -> 30;
decode_type('CHOICE') -> 'CHOICE'; % choice gets the tag from the actual alternative  
decode_type(Else) -> exit({error,{asn1,{unrecognized_type,Else}}}).

add_removed_bytes() ->
    asn1ct_name:delete(rb),
    add_removed_bytes(asn1ct_name:all(rb)).

add_removed_bytes([H,T1|T]) ->
    emit({{var,H},"+"}),
    add_removed_bytes([T1|T]);
add_removed_bytes([H|T]) ->
    emit({{var,H}}),
    add_removed_bytes(T);
add_removed_bytes([]) ->
    true.

mkfuncname(WhatKind,DecOrEnc) ->
    case WhatKind of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    CurrMod = get(currmod),
	    case CurrMod of
		Mod ->
		    lists:concat(["'",DecOrEnc,"_",EType,"'"]);
		_ ->
		    io:format("CurrMod: ~p, Mod: ~p~n",[CurrMod,Mod]),
		    lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"])
	    end;
	#'typereference'{val=EType} ->
	    lists:concat(["'",DecOrEnc,"_",EType,"'"]);
	'ASN1_OPEN_TYPE' ->
	    lists:concat(["'",DecOrEnc,"_",WhatKind,"'"])
	    
    end.

optionals(L) -> optionals(L,[],1).

optionals([{'EXTENSIONMARK',_,_}|Rest],Acc,Pos) ->
    optionals(Rest,Acc,Pos); % optionals in extension are currently not handled
optionals([#'ComponentType'{name=Name,prop='OPTIONAL'}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{name=Name,prop={'DEFAULT',_}}|Rest],Acc,Pos) ->
		 optionals(Rest,[{Name,Pos}|Acc],Pos+1);
optionals([#'ComponentType'{}|Rest],Acc,Pos) ->
		 optionals(Rest,Acc,Pos+1);
optionals([],Acc,_) ->
    lists:reverse(Acc).

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V
    end.

%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->  
%%     8bit Int | binary 
encode_tag_val(Class, Form, TagNo) when (TagNo =< 30) -> 
    <<(Class bsr 6):2,(Form bsr 5):1,TagNo:5>>;

encode_tag_val(Class, Form, TagNo) -> 
    {Octets,_Len} = mk_object_val(TagNo),
    BinOct = list_to_binary(Octets),
    <<(Class bsr 6):2, (Form bsr 5):1, 31:5,BinOct/binary>>.

%%%%%%%%%%% 
%% mk_object_val(Value) -> {OctetList, Len} 
%% returns a Val as a list of octets, the 8 bit is allways set to one except 
%% for the last octet, where its 0 
%% 

 
mk_object_val(Val) when Val =< 127 -> 
    {[255 band Val], 1}; 
mk_object_val(Val) -> 
    mk_object_val(Val bsr 7, [Val band 127], 1).  
mk_object_val(0, Ack, Len) -> 
    {Ack, Len}; 
mk_object_val(Val, Ack, Len) -> 
    mk_object_val(Val bsr 7, [((Val band 127) bor 128) | Ack], Len + 1). 

add_func(F={_Func,_Arity}) ->
    ets:insert(asn1_functab,{F}).






