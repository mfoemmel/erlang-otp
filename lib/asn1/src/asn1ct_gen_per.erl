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

-export([pgen/4,mk_var/1,gen_dec_prim/3,gen_encode_prim/4]).
-import(asn1ct_gen, [emit/1,demit/1]).

%% pgen(Erules, Module, TypeOrVal)
%% Generate Erlang module (.erl) and (.hrl) file corresponding to an ASN.1 module
%% .hrl file is only generated if necessary
%% Erules = per | ber
%% Module = atom()
%% TypeOrVal = {TypeList,ValueList}
%% TypeList = ValueList = [atom()]

pgen(OutFile,Erules,Module,TypeOrVal) ->
    pgen_module(OutFile,Erules,Module,TypeOrVal,true).


pgen_module(OutFile,Erules,Module,TypeOrVal,Indent) ->
    put(outfile,OutFile),
    HrlGenerated = asn1ct_gen:pgen_hrl(Erules,Module,TypeOrVal,Indent),
    asn1ct_name:start(),
    ErlFile = lists:concat([OutFile,".erl"]),
    Fid = asn1ct_gen:fopen(ErlFile,write),
    put(gen_file_out,Fid),
    asn1ct_gen:gen_head(Erules,Module,HrlGenerated),
    asn1ct_gen:pgen_exports(Erules,Module,TypeOrVal),
    pgen_typeorval(Erules,Module,TypeOrVal),
% gen_vars(asn1_db:mod_to_vars(Module)),
% gen_tag_table(AllTypes),
    file:close(Fid),
    io:format("--~p--~n",[{generated,ErlFile}]).


pgen_typeorval(Erules,Module,{Types,Values,Ptypes}) ->
    pgen_types(Erules,Module,Types),
    pgen_values(Erules,Module,Values).

pgen_values(_,_,[]) ->
    true;
pgen_values(Erules,Module,[H|T]) ->
    Valuedef = asn1_db:dbget(Module,H),
    gen_value(Valuedef),
    pgen_values(Erules,Module,T).

pgen_types(_,_,[]) ->
    true;

pgen_types(Erules,Module,[H|T]) ->
    asn1ct_name:clear(),
    Typedef = asn1_db:dbget(Module,H),
    gen_encode(Erules,Typedef),
    asn1ct_name:clear(),
    gen_decode(Erules,Typedef),
    pgen_types(Erules,Module,T).


gen_types(Erules,Tname,{RootList,ExtList}) when list(RootList) ->
    gen_types(Erules,Tname,RootList),
    gen_types(Erules,Tname,ExtList);
gen_types(Erules,Tname,[{'EXTENSIONMARK',_,_}|Rest]) ->
    gen_types(Erules,Tname,Rest);
gen_types(Erules,Tname,[ComponentType|Rest]) ->
    asn1ct_name:clear(),
    gen_encode(Erules,Tname,ComponentType),
    asn1ct_name:clear(),
    gen_decode(Erules,Tname,ComponentType),
    gen_types(Erules,Tname,Rest);
gen_types(_,Tname,[]) ->
    true;
gen_types(Erules,Tname,Type) when record(Type,type) ->
    asn1ct_name:clear(),
    gen_encode(Erules,Tname,Type),
    asn1ct_name:clear(),
    gen_decode(Erules,Tname,Type).


%% VARIOUS GENERATOR STUFF 
%% *************************************************
%%**************************************************

mk_var(X) when atom(X) ->
    list_to_atom(mk_var(atom_to_list(X)));

mk_var([H|T]) ->
    [H-32|T].


%% Generate value functions ***************
%% ****************************************
%% Generates a function 'V'/0 for each Value V defined in the ASN.1 module
%% the function returns the value in an Erlang representation which can be
%% used as  input to the runtime encode functions

gen_value(Value) when record(Value,valuedef) ->
%%    io:format(" ~w ",[Value#valuedef.name]),
    emit({"'",Value#valuedef.name,"'() ->",nl}),
    {'ASN1_OK',V} = Value#valuedef.value,
    emit([{asis,V},".",nl,nl]).


%% Generate ENCODING ******************************
%%****************************************x


gen_encode(Erules,Type) when record(Type,typedef) ->
    put(ctype,Type#typedef.name),
    gen_encode_user(Erules,Type).

gen_encode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type,prop=Prop}) ->
    NewTname = list_to_atom(lists:concat([Tname,"_",Cname])),
    gen_encode(Erules,NewTname,Type);

gen_encode(Erules,Tname,Type) when record(Type,type) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    case InnerType of
		'SET' ->
		    true;
		'SEQUENCE' ->
		    true;
		_ ->
		    emit({nl,"'enc_",Tname,"'({'",Tname,"',Val}) ->",nl}),
		    emit({"'enc_",Tname,"'(Val);",nl,nl})
	    end,
	    emit({"'enc_",Tname,"'(Val) ->",nl}),
	    gen_encode_constructed_notag(Erules,Tname,InnerType,Type);
	_ ->
	    true
    end.


gen_encode_user(Erules,D) when record(D,typedef) ->
    Typename = D#typedef.name,
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case InnerType of
	'SET' -> true;
	'SEQUENCE' -> true;
	_ ->
	    emit({nl,"'enc_",Typename,"'({'",Typename,"',Val}) ->",nl}),
	    emit({"'enc_",Typename,"'(Val);",nl,nl})
    end,
    emit({"'enc_",Typename,"'(Val) ->",nl}),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_encode_prim(Erules,Def,"false"),
	    emit({".",nl});
	'ASN1_OPEN_TYPE' ->
	    gen_encode_prim(Erules,Def,"false"),
	    emit({".",nl});
	{constructed,bif} ->
	    gen_encode_constructed_notag(Erules,Typename,InnerType,D);
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'enc_",Etype,"'(Val).",nl,nl});
	#typereference{val=Ename} ->
	    emit({"'enc_",Ename,"'(Val).",nl,nl});
	{notype,_} ->
	    emit({"'enc_",InnerType,"'(Val).",nl,nl})
    end.


gen_encode_constructed_notag(Erules,Typename,InnerType,D) when record(D,type) ->

    Rtmod = list_to_atom(lists:concat(["asn1ct_constructed_",Erules])),
    case InnerType of
	'SET' ->
	    Rtmod:gen_encode_constructed_notag(Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'SEQUENCE' ->
	    Rtmod:gen_encode_constructed_notag(Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'CHOICE' ->
	    Rtmod:gen_encode_choice_notag(Typename,D),
	    {_,Components} = D#type.def,
	    gen_types(Erules,Typename,Components);
	'SEQUENCE OF' ->
	    Rtmod:gen_encode_seqof(Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    gen_types(Erules,list_to_atom(lists:concat([Typename,"_","SEQOF"])),Type);
	'SET OF' ->
	    Rtmod:gen_encode_seqof(Typename,InnerType,D),
	    {_,Type} = D#type.def,
	    gen_types(Erules,list_to_atom(lists:concat([Typename,"_","SETOF"])),Type);
	Other ->
	    exit({nyi,InnerType})
    end;


gen_encode_constructed_notag(Erules,Typename,InnerType,D) when record(D,typedef) ->
    gen_encode_constructed_notag(Erules,Typename,InnerType,D#typedef.typespec).

gen_encode_prim(Erules,D,DoTag) ->
    Value = case asn1ct_name:active(val) of
		true ->
		    mk_var(asn1ct_name:curr(val));
		false ->
		    "Val"
	    end,
    gen_encode_prim(Erules,D,DoTag,Value).

gen_encode_prim(Erules,D,DoTag,Value) when record(D,type) ->
    Constraint = D#type.constraint,
    Rtmod = list_to_atom(?RT_PER),
    case D#type.def of
	'INTEGER' ->
	    emit({{asis,Rtmod},":encode_integer(", %fel
		  {asis,Constraint},",",Value,")"});
	{'INTEGER',NamedNumberList} ->
	    emit({{asis,Rtmod},":encode_integer(",
		  {asis,Constraint},",",Value,",",
		  {asis,NamedNumberList},")"});
	{'ENUMERATED',{Nlist1,Nlist2}} ->
	    NewList = lists:concat([[{0,X}||{X,Y} <- Nlist1],['EXT_MARK'],[{1,X}||{X,Y} <- Nlist2]]),
	    NewC = [{'ValueRange',{0,length(Nlist1)-1}}],
	    emit(["case ",Value," of",nl]),
	    emit_enc_enumerated_cases(NewC, NewList++[{asn1_enum,length(Nlist1)-1}], 0);
%%%	    emit([{asis,Rtmod},":encode_enumerated(",
%%%		  {asis,NewC},",",Value,",",
%%%		  {asis,NewList},")"]);
	{'ENUMERATED',NamedNumberList} ->
	    NewList = [X||{X,Y} <- NamedNumberList],
	    NewC = [{'ValueRange',{0,length(NewList)-1}}],
	    emit(["case ",Value," of",nl]),
	    emit_enc_enumerated_cases(NewC, NewList, 0);
%%%	    emit({{asis,Rtmod},":encode_enumerated(",
%%%		  {asis,NewC},",",Value,",",
%%%		  {asis,NewList},")"});
	{'BIT STRING',NamedNumberList} ->
%%	    NewList = [{X,Y}||{_,X,Y} <- NamedNumberList],
	    emit({{asis,Rtmod},":encode_bit_string(",
		  {asis,Constraint},",",Value,",",
%%		  {asis,NewList},")"});
		  {asis,NamedNumberList},")"});
	'NULL' ->
	    emit({{asis,Rtmod},":encode_null(",Value,")"});
	'OBJECT IDENTIFIER' ->
	    emit({{asis,Rtmod},":encode_object_identifier(",Value,")"});
	'ObjectDescriptor' ->
	    emit({"asn1_encode:e_object_descriptor(",Value,",",false,")"});
	'BOOLEAN' ->
	    emit({{asis,Rtmod},":encode_boolean(",Value,")"});
	'OCTET STRING' ->
	    emit({{asis,Rtmod},":encode_octet_string(",{asis,Constraint},",",Value,")"});
	'NumericString' ->
	    emit({{asis,Rtmod},":encode_NumericString(",{asis,Constraint},",",Value,")"});
	'TeletexString' ->
	    emit({{asis,Rtmod},":encode_TeletexString(",{asis,Constraint},",",Value,")"});
	'VideotexString' ->
	    emit({{asis,Rtmod},":encode_VideotexString(",{asis,Constraint},",",Value,")"});
	'UTCTime' ->
	    emit({{asis,Rtmod},":encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GeneralizedTime' ->
	    emit({{asis,Rtmod},":encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GraphicString' ->
	    emit({{asis,Rtmod},":encode_GraphicString(",{asis,Constraint},",",Value,")"});
	'VisibleString' ->
	    emit({{asis,Rtmod},":encode_VisibleString(",{asis,Constraint},",",Value,")"});
	'GeneralString' ->
	    emit({{asis,Rtmod},":encode_GeneralString(",{asis,Constraint},",",Value,")"});
	'PrintableString' ->
	    emit({{asis,Rtmod},":encode_PrintableString(",{asis,Constraint},",",Value,")"});
	'IA5String' ->
	    emit({{asis,Rtmod},":encode_IA5String(",{asis,Constraint},",",Value,")"});
	'BMPString' ->
	    emit({{asis,Rtmod},":encode_BMPString(",{asis,Constraint},",",Value,")"});
	'UniversalString' ->
	    emit({{asis,Rtmod},":encode_UniversalString(",{asis,Constraint},",",Value,")"});
	'ANY' ->
	    emit([{asis,Rtmod},":encode_open_type(", {asis,Constraint}, ",", 
		  Value, ")"]);
	'ASN1_OPEN_TYPE' ->
	    NewValue = case Constraint of
			 [#typereference{val=Tname}] ->
			     io_lib:format(
			       "?RT_PER:complete(enc_~s(~s))",[Tname,Value]);
			 _ -> Value
		     end,
	    emit([{asis,Rtmod},":encode_open_type(", {asis,Constraint}, ",", 
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



%% DECODING *****************************
%%***************************************


gen_decode(Erules,Type) when record(Type,typedef) ->
    D = Type,
    put(ctype,Type#typedef.name),
    emit({nl,nl}),
    emit({"'dec_",Type#typedef.name,"'(Bytes,Telltype) ->",nl}),
    dbdec(Type#typedef.name),
    gen_decode_user_notag(Erules,D).

gen_decode(Erules,Tname,#'ComponentType'{name=Cname,typespec=Type,prop=Prop}) ->
    NewTname = list_to_atom(lists:concat([Tname,"_",Cname])),
    gen_decode(Erules,NewTname,Type);

gen_decode(Erules,Tname,Type) when record(Type,type) ->
    Tname,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	{constructed,bif} ->
	    emit({nl,"'dec_",Tname,"'(Bytes,Telltype) ->",nl}),
	    dbdec(Tname),
	    gen_decode_constructed_notag(Erules,Tname,InnerType,Type);
	_ ->
	    true
    end.

dbdec(Type) ->
    demit({"io:format(\"decoding: ",{asis,Type},"~w~n\",[Bytes]),",nl}).

gen_decode_user_notag(Erules,D) when record(D,typedef) ->
    Typename = D#typedef.name,
    Def = D#typedef.typespec,
    InnerType = asn1ct_gen:get_inner(Def#type.def),
    case asn1ct_gen:type(InnerType) of
	{primitive,bif} ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit({".",nl,nl});
	'ASN1_OPEN_TYPE' ->
	    gen_dec_prim(Erules,Def,"Bytes"),
	    emit({".",nl,nl});
	{constructed,bif} ->
	    gen_decode_constructed_notag(Erules,Typename,InnerType,D);
	#typereference{val=Dname} ->
	    emit({"'dec_",Dname,"'(Bytes,Telltype)"}),
	    emit({".",nl,nl});
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    emit({"'",Emod,"':'dec_",Etype,"'(Bytes,Telltype).",nl,nl});
	Other ->
	    exit({error,{asn1,{unknown,Other}}})
    end.

gen_decode_constructed_notag(Erules,Typename,InnerType,D) when record(D,type) ->
    Rtmod = list_to_atom(lists:concat(["asn1ct_constructed_",Erules])),
    case InnerType of
	'SET' ->
	    Rtmod:gen_decode_constructed_notag(Typename,D);
	'SEQUENCE' ->
	    Rtmod:gen_decode_constructed_notag(Typename,D);
	'CHOICE' ->
	    Rtmod:gen_decode_choice_notag(Typename,D);
	'SEQUENCE OF' ->
	    Rtmod:gen_decode_seqof(Typename,InnerType,D);
	
	'SET OF' ->
	    Rtmod:gen_decode_seqof(Typename,InnerType,D);
	Other ->
	    exit({nyi,InnerType})
    end;


gen_decode_constructed_notag(Erules,Typename,InnerType,D) when record(D,typedef) ->
    gen_decode_constructed_notag(Erules,Typename,InnerType,D#typedef.typespec).


gen_dec_prim(Erules,Att,BytesVar) ->
    Typename = Att#type.def,
    Constraint = Att#type.constraint,
    Rtmod = list_to_atom(?RT_PER),
    case Typename of
	'INTEGER' ->
	    emit({{asis,Rtmod},":decode_integer(",BytesVar,",",
		  {asis,Constraint},")"});
	{'INTEGER',NamedNumberList} ->
	    emit({{asis,Rtmod},":decode_integer(",BytesVar,",",
		  {asis,Constraint},",",
		  {asis,NamedNumberList},")"});
	{'BIT STRING',NamedNumberList} ->
%%	    NewList = [{X,Y}||{_,X,Y} <- NamedNumberList],
	    emit({{asis,Rtmod},":decode_bit_string(",BytesVar,",",
		  {asis,Constraint},",",
%%		  {asis,NewList},")"});
		  {asis,NamedNumberList},")"});
	'NULL' ->
	    emit({{asis,Rtmod},":decode_null(",
		  BytesVar,")"});
	'OBJECT IDENTIFIER' ->
	    emit({{asis,Rtmod},":decode_object_identifier(",
		  BytesVar,")"});
	'ObjectDescriptor' ->
	    emit({{asis,Rtmod},":decode_object_descriptor(",
		  BytesVar,")"});
	{'ENUMERATED',{NamedNumberList1,NamedNumberList2}} ->
	    NewTup = {list_to_tuple([X||{X,Y} <- NamedNumberList1]),
		      list_to_tuple([X||{X,Y} <- NamedNumberList2])},
	    NewC = [{'ValueRange',{0,size(element(1,NewTup))-1}}],
	    emit({{asis,Rtmod},":decode_enumerated(",BytesVar,",",
		  {asis,NewC},",",
		  {asis,NewTup},")"});
	{'ENUMERATED',NamedNumberList} ->
	    NewTup = list_to_tuple([X||{X,Y} <- NamedNumberList]),
	    NewC = [{'ValueRange',{0,size(NewTup)-1}}],
	    emit({{asis,Rtmod},":decode_enumerated(",BytesVar,",",
		  {asis,NewC},",",
		  {asis,NewTup},")"});
	'BOOLEAN'->
	    emit({{asis,Rtmod},":decode_boolean(",BytesVar,")"});
	'OCTET STRING' ->
	    emit({{asis,Rtmod},":decode_octet_string(",BytesVar,",",
		  {asis,Constraint},")"});
	'NumericString' ->
	    emit({{asis,Rtmod},":decode_NumericString(",BytesVar,",",
		  {asis,Constraint},")"});
	'TeletexString' ->
	    emit({{asis,Rtmod},":decode_TeletexString(",BytesVar,",",
		  {asis,Constraint},")"});
	'VideotexString' ->
	    emit({{asis,Rtmod},":decode_VideotexString(",BytesVar,",",
		  {asis,Constraint},")"});
	'UTCTime' ->
	    emit({{asis,Rtmod},":decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GeneralizedTime' ->
	    emit({{asis,Rtmod},":decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GraphicString' ->
	    emit({{asis,Rtmod},":decode_GraphicString(",BytesVar,",",
		  {asis,Constraint},")"});
	'VisibleString' ->
	    emit({{asis,Rtmod},":decode_VisibleString(",BytesVar,",",
		  {asis,Constraint},")"});
	'GeneralString' ->
	    emit({{asis,Rtmod},":decode_GeneralString(",BytesVar,",",
		  {asis,Constraint},")"});
	'PrintableString' ->
	    emit({{asis,Rtmod},":decode_PrintableString(",BytesVar,",",{asis,Constraint},")"});
	'IA5String' ->
	    emit({{asis,Rtmod},":decode_IA5String(",BytesVar,",",{asis,Constraint},")"});
	'BMPString' ->
	    emit({{asis,Rtmod},":decode_BMPString(",BytesVar,",",{asis,Constraint},")"});
	'UniversalString' ->
	    emit({{asis,Rtmod},":decode_UniversalString(",BytesVar,",",{asis,Constraint},")"});
	'ANY' ->
	    emit([{asis,Rtmod},":decode_open_type(",BytesVar,",", 
		  {asis,Constraint}, ")"]); 
	'ASN1_OPEN_TYPE' ->
	    case Constraint of
		[#typereference{val=Tname}] ->
		    emit(["fun(FBytes) ->",nl,
			  "   {XTerm,XBytes} = "]),
		    emit([{asis,Rtmod},":decode_open_type(",BytesVar,",[]),",nl]),
		    emit(["{YTerm,_} = dec_",Tname,"(XTerm,mandatory),",nl]),
		    emit(["{YTerm,XBytes} end(",BytesVar,")"]);
		_ ->
		    emit([{asis,Rtmod},":decode_open_type(",BytesVar,",[])"])
	    end;
	Other ->
	    exit({'cant decode' ,Other})
    end.






















