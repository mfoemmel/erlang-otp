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
-module(asn1ct_gen_ber).

%% Generate erlang module which handles (PER) encode and decode for
%% all types in an ASN.1 module

-include("asn1_records.hrl").

-export([pgen/4]).
-export([decode_class/1, decode_type/1]).
-export([add_removed_bytes/0]).
-export([gen_encode/2,gen_encode/3,gen_decode/2,gen_decode/3]).
-export([gen_encode_prim/4]).
-export([gen_dec_prim/8]).

-import(asn1ct_gen, [emit/1,demit/1]).

						% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

						% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).

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
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit([nl,nl,nl,"%%================================"]),
	    emit([nl,"%%  ",asn1ct_gen:list2name(Typename)]),
	    emit([nl,"%%================================",nl]),
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
		    emit([nl,"'enc_",asn1ct_gen:list2name(Typename),"'({'",asn1ct_gen:list2name(Typename),"',Val}, TagIn) ->",nl]),
		    emit(["   'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn);",nl,nl])
	    end,
	    emit(["'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn) ->",nl,"   "]),
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;

%%===============================================================================
%% encode ComponentType
%%===============================================================================

gen_encode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,Prop,Tags}) ->
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
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- OTag],
    emit([nl,nl,"%%================================"]),
    emit([nl,"%%  ",Typename]),
    emit([nl,"%%================================",nl]),
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
	    emit({nl,"'enc_",asn1ct_gen:list2name(Typename),"'({'",asn1ct_gen:list2name(Typename),"',Val}, TagIn) ->",nl}),
	    emit({"   'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn);",nl,nl})
    end,
    emit({"'enc_",asn1ct_gen:list2name(Typename),"'(Val, TagIn) ->",nl}),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    asn1ct_gen:gen_encode_constructed(Erules,Typename,InnerType,D);
	{primitive,bif} ->
	    asn1ct_gen_ber:gen_encode_prim(ber,Type,["TagIn ++ ",
						     {asis,Tag}],"Val"),
	    emit([".",nl]);
	#typereference{val=Ename} ->
	    emit(["   'enc_",Ename,"'(Val, TagIn ++ ",{asis,Tag},").",nl]);
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit(["   '",Emod,"':'enc_",Etype,"'(Val, TagIn ++ ",
		  {asis,Tag},").",nl]);
	'ASN1_OPEN_TYPE' ->
	    emit(["%% OPEN TYPE",nl]),
	    asn1ct_gen_ber:gen_encode_prim(ber,
					   Type#type{def='ASN1_OPEN_TYPE'},
					   ["TagIn ++ ",
					    {asis,Tag}],"Val"),
	    emit([".",nl])
    end.



gen_encode_prim(Erules,D,DoTag,Value) when record(D,type) ->
    
%%% Currently not used for BER (except for BitString) and therefore replaced
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
	{'ENUMERATED',NamedNumberList} ->
	    emit(["case ",Value," of",nl]),
	    emit_enc_enumerated_cases(NamedNumberList,DoTag);

%%%	    emit_encode_func('enumerated',Constraint,Value,
%%%			     NamedNumberList,DoTag);
	{'BIT STRING',NamedNumberList} ->
%%	    NewList = [{X,Y}||{_,X,Y} <- NamedNumberList],
%%	    emit_encode_func('bit_string',BitStringConstraint,Value,
%%			     NewList,DoTag);
	    emit_encode_func('bit_string',BitStringConstraint,Value,
			     NamedNumberList,DoTag);
	'ANY' ->
	    exit({'can not encode' ,'ANY'});
	'NULL' ->
	    emit_encode_func("null",Value,DoTag);
	'OBJECT IDENTIFIER' ->
	    emit_encode_func("object_identifier",Value,DoTag);
	'ObjectDescriptor' ->
	    emit_encode_func('object_descriptor',Constraint,Value,DoTag);
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
	    emit_encode_func("universal_string",Constraint,Value,DoTag);
	'BMPString' ->
	    emit_encode_func('BMP_string',Constraint,Value,DoTag);
	'UTCTime' ->
	    emit_encode_func(utc_time,Constraint,Value,DoTag);
	'GeneralizedTime' ->
	    emit_encode_func(generalized_time,Constraint,Value,DoTag);
	'ASN1_OPEN_TYPE' ->
	    emit_encode_func(open_type, Value, DoTag);
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
    D = Type,
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes, OptOrMand) ->",nl}),
    emit({"   'dec_",Type#typedef.name,"'(Bytes, OptOrMand, []).",nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes, OptOrMand, TagIn) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user(Erules,D).


%%===============================================================================
%% decode #{type, {tag, def, constraint}}
%%===============================================================================

gen_decode(Erules,Tname,Type) when record(Type,type) ->
    Typename = Tname,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit({"'dec_",asn1ct_gen:list2name(Typename),"'(Bytes, OptOrMand, TagIn) ->",nl}),
	    dbdec(Typename),
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,Type);
	_ ->
	    true
    end;


%%===============================================================================
%% decode ComponentType
%%===============================================================================

gen_decode(Erules,Tname,{'ComponentType',_Pos,Cname,Type,Prop,Tags}) ->
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
    InnerTag = Def#type.tag ,
    Tag = [X#tag{class=decode_class(X#tag.class)}|| X <- InnerTag],
    case asn1ct_gen:type(InnerType) of
	'ASN1_OPEN_TYPE' ->
	    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
	    LenVar = asn1ct_gen:mk_var(asn1ct_name:curr(len)),
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def#type{def='ASN1_OPEN_TYPE'}, 
			 BytesVar, Tag, "TagIn",no_length, 
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{primitive,bif} ->
	    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
	    LenVar = asn1ct_gen:mk_var(asn1ct_name:curr(len)),
	    asn1ct_name:new(len),
	    gen_dec_prim(ber, Def, BytesVar, Tag, "TagIn",no_length, 
			 ?PRIMITIVE,"OptOrMand"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    asn1ct_gen:gen_decode_constructed(Erules,Typename,InnerType,D);
	TheType ->
	    DecFunName = mkfuncname(TheType,dec),
	    emit({DecFunName,"(",{curr,bytes},
		  ", OptOrMand, TagIn++",{asis,Tag},")"}),
	    emit({".",nl,nl})
    end.


gen_dec_prim(Erules,Att,BytesVar,DoTag,TagIn,Length,Form,OptOrMand) ->
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
    
    DoLength = 
	case Typename of
	    'BOOLEAN'->
		emit({"?RT_BER:decode_boolean(",BytesVar,","}),
		false;
	    'INTEGER' ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,ValueRange},","}),
		false;
	    {'INTEGER',NamedNumberList} ->
		emit({"?RT_BER:decode_integer(",BytesVar,",",
		      {asis,ValueRange},",",
		      {asis,NamedNumberList},","}),
		false;
	    {'ENUMERATED',NamedNumberList} ->
		emit({"?RT_BER:decode_enumerated(",BytesVar,",",
		      {asis,Constraint},",",
		      {asis,NamedNumberList},","}),
		false;
	    {'BIT STRING',NamedNumberList} ->
%%		NewList = [{X,Y}||{_,X,Y} <- NamedNumberList],
		case get(compact_bit_string) of
		    true ->
			emit({"?RT_BER:decode_compact_bit_string(",
			      BytesVar,",",{asis,Constraint},",",
			      {asis,NamedNumberList},","});
		    _ ->
			emit({"?RT_BER:decode_bit_string(",BytesVar,",",
			      {asis,Constraint},",",
			      {asis,NamedNumberList},","})
			%%		      {asis,NewList},","}),
		end,
		true;
	    'NULL' ->
		emit({"?RT_BER:decode_null(",BytesVar,","}),
		false;
	    'OBJECT IDENTIFIER' ->
		emit({"?RT_BER:decode_object_identifier(",BytesVar,","}),
		false;
	    'ObjectDescriptor' ->
		emit({"?RT_BER:decode_object_descriptor(",
		      BytesVar,")"}),
		false;
	    'OCTET STRING' ->
		emit({"?RT_BER:decode_octet_string(",BytesVar,",",{asis,Constraint},","}),
		true;
	    'NumericString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_NumericString},","}),true;
	    'TeletexString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_TeletexString},","}),
		true;
	    'VideotexString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VideotexString},","}),
		true;
	    'GraphicString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GraphicString},","})
		    ,true;
	    'VisibleString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_VisibleString},","}),
		true;
	    'GeneralString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_GeneralString},","}),
		true;
	    'PrintableString' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_PrintableString},","}),
		true;
	    'IA5String' ->
		emit({"?RT_BER:decode_restricted_string(",
		      BytesVar,",",{asis,Constraint},",",{asis,?T_IA5String},","}),
		true;
	    'UniversalString' ->
		emit({"?RT_BER:decode_universal_string(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'BMPString' ->
		emit({"?RT_BER:decode_BMP_string(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'UTCTime' ->
		emit({"?RT_BER:decode_utc_time(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'GeneralizedTime' ->
		emit({"?RT_BER:decode_generalized_time(",
		      BytesVar,",",{asis,Constraint},","}),
		true;
	    'ASN1_OPEN_TYPE' ->
		emit(["?RT_BER:decode_open_type(",
		      BytesVar, ","]),
		false;
	    'ANY' ->
		emit(["?RT_BER:decode_open_type(",
		      BytesVar, ","]),
		false;
	    Other ->
		exit({'can not decode' ,Other})
	end,
    
    NewLength = case DoLength of
		    true -> [", ", Length];
		    false -> ""
		end,
    NewOptOrMand = case OptOrMand of
		       _ when list(OptOrMand) -> OptOrMand;
		       mandatory -> {asis,mandatory};
		       _ -> {asis,opt_or_default}
		   end,
    case TagIn of
	[] ->
	    emit([{asis,DoTag},NewLength,", ",NewOptOrMand,")"]);
	_ when list(TagIn) ->
	    emit([TagIn,"++",{asis,DoTag},NewLength,", ",NewOptOrMand,")"])
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
	    lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"]);
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








