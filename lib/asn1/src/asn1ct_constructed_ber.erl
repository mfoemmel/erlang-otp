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
-module(asn1ct_constructed_ber).

-export([gen_encode_sequence/2]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/2]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/3]).
-export([gen_decode_sof/3]).
-export([gen_encode_choice/2]).
-export([gen_decode_choice/3]).
-export([gen_enc_line/6, gen_enc_line/7]).

-include("asn1_records.hrl").

%-compile(export_all).

-import(asn1ct_gen, [emit/1,demit/1]).

% the encoding of class of tag bits 8 and 7
-define(UNIVERSAL,   0).
-define(APPLICATION, 16#40).
-define(CONTEXT,     16#80).
-define(PRIVATE,     16#C0).

% primitive or constructed encoding % bit 6
-define(PRIMITIVE,   0).
-define(CONSTRUCTED, 2#00100000).




%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sequence(Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    {SeqOrSet,CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_sequence_call(Typename,CompList1,1,Ext),

    MyTag = 
	case D#type.tag of
	     undefined -> []; 
	     Tag -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	end ++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(SeqOrSet),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit([nl,"   BytesSoFar = ["]),
    mkvlist(asn1ct_name:all(encBytes)),
    emit(["],",nl]),
    emit("LenSoFar = "),
    case asn1ct_name:all(encLen) of
	[] -> emit("0");
	AllLengths -> 
	    mkvplus(AllLengths)
    end,
    emit([",",nl]),
    emit(["{TagBytes,Len} = ?RT_BER:encode_tags(TagIn ++ ",
	  {asis,MyTag},", BytesSoFar, LenSoFar).",nl]).

gen_decode_sequence(Module,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    {'SEQUENCE',TCompList} = D#type.def,
    Ext = extensible(TCompList),
    CompList = case TCompList of
		   {Rl,El}  -> Rl ++ El;
		   _ -> TCompList
	       end,

    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),

    asn1ct_name:new(rb),
    MyTag = 
	case D#type.tag of
	     undefined -> []; 
	     Tag -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	end ++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type('SEQUENCE'),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),

    case CompList of
	[] -> true;
	_ ->
	    emit({"{",{next,bytes},
		  ",RemBytes} = ?RT_BER:split_list(",
		  {curr,bytes},
		  ",", {prev,len},"),",nl}),
	    asn1ct_name:new(bytes)
    end,
    
    case gen_dec_sequence_call(Module,Typename,CompList,Ext) of
	no_terms -> 	    
	    emit({nl,nl}),
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit({"   {{",{asis,Typename},"}, ",{curr,bytes},",",nl,"    "}),
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit({"}.",nl});
	_ ->
	    emit({com,nl,nl}),
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit({"   {{",{asis,Typename},", "}),
	    mkvlist(asn1ct_name:all(term)),
	    case Ext of
		{ext,_,_} ->
		    emit({"}, ?RT_BER:restbytes(RemBytes, ",
			  {next,bytes},",ext),",nl,"    "});
		noext ->
		    emit({"}, ?RT_BER:restbytes(RemBytes, ",
			  {next,bytes},",noext),",nl,"    "})
	    end,
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit({"}.",nl})
    end.



%%============================================================================
%%  Encode/decode SET
%%
%%============================================================================

gen_encode_set(Typename,D) when record(D,type) ->
    gen_encode_sequence(Typename,D).

gen_decode_set(Module,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    {'SET',TCompList} = D#type.def,
    Ext = extensible(TCompList),
    CompList = case TCompList of
		   {Rl,El} -> Rl ++ El;
		   _ -> TCompList
	       end,

    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),

    asn1ct_name:new(rb),
    MyTag = 
	case D#type.tag of
	     undefined -> []; 
	     Tag -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	end ++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type('SET'),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),

    emit({"   {SetTerm, SetBytes} = 'dec_",Typename,"_set_loop'(",
	  {curr,bytes},", OptOrMand, [],",{prev,len},"),",nl}),
    
    asn1ct_name:new(rb),
    emit({"   'dec_",Typename,"_result'(lists:sort(SetTerm), SetBytes, "}),
    asn1ct_gen_ber:add_removed_bytes(),
    emit({"+",{prev,len},").",nl,nl,nl}),

    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Set loop for ",{asis,Typename},nl}),
    emit({"%%-------------------------------------------------",nl}),

    emit({"'dec_",Typename,"_set_loop'(Bytes, OptOrMand,  S_Term, S_Rb) when S_Rb < 0 ->",nl}),
    emit({"   exit({error,{asn1,{'SET_length_error',{remaining_bytes,Bytes}}}});",nl,nl}),
    emit({"'dec_",Typename,"_set_loop'([], OptOrMand,  S_Term, S_Rb) when S_Rb > 0 ->",nl}),
    emit({"   exit({error,{asn1,{'SET_length_error',{no_of_missing_bytes, S_Rb}}}});",nl,nl}),
    emit({"'dec_",Typename,"_set_loop'(Bytes, OptOrMand,  S_Term, 0) ->",nl}),
    emit({"   {S_Term, Bytes};",nl,nl}),
    asn1ct_name:clear(),
    asn1ct_name:new(term),
    emit({"'dec_",Typename,"_set_loop'(",{curr,bytes},
	  ", OptOrMand,  S_Term, S_Rb) ->",nl}),
    
    asn1ct_name:new(bytes),
    gen_dec_set(Module,Typename,CompList,1,Ext),

    emit({"      %% tag not found",nl}),
    emit({indent(6),"_ -> {S_Term, Bytes}",nl}),
    emit({indent(3),"end.",nl,nl,nl}),


    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Result ",{asis,Typename},nl}),
    emit({"%%-------------------------------------------------",nl}),

    asn1ct_name:clear(),
    emit({"'dec_",Typename,"_result'(TermList, Bytes, Rb) ->",nl}),
    
    case gen_dec_set_result(Module,Typename,CompList) of
	no_terms -> 	    
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit({"   {{",{asis,Typename},"}, Bytes, Rb}.",nl});
	_ ->
	    emit({nl,"   case ",{curr,termList}," of",nl}),
	    emit({"      [] -> {{",{asis,Typename},", "}),
	    mkvlist(asn1ct_name:all(term)),
	    emit({"}, Bytes, Rb};",nl}),    
	    emit({"      ExtraAtt -> exit({error,{asn1,{too_many_attributes, ExtraAtt}}})",nl}),
	    emit({"   end.",nl}),
	    emit({nl,nl,nl})
    end.


%%===============================================================================
%%===============================================================================
%%===============================================================================
%%  Encode/decode SEQUENCE OF and SET OF
%%===============================================================================
%%===============================================================================
%%===============================================================================

gen_encode_sof(Typename,InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, TypeTag, TypeNameSuffix, Cont} = 
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',"SETOF",_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',"SEQOF",_Cont}
	end,

    emit({"   {EncBytes,EncLen} = 'enc_",Typename,"_components'(Val,[],0),",nl}),

    MyTag = 
	case D#type.tag of
	     undefined -> []; 
	     Tag -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	end ++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(TypeTag),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   ?RT_BER:encode_tags(TagIn ++ ",
	  {asis,MyTag},", EncBytes, EncLen).",nl,nl]),
    
    

%    case D#type.tag of
%	{tag,Class,TagNr,'EXPLICIT'} ->
%	    emit({"   InnerTag = "}),
%	    enc_tag({tag, 'UNIVERSAL', 
%		     asn1ct_gen_ber:decode_type(TypeTag), 
%		     'IMPLICIT'},
%		    TypeTag),
%	    emit({"   InnerLen = length(lists:flatten(SOfEnc)),",nl}),
%	    emit({"   InnerLenEnc = ?RT_BER:encode_length(InnerLen),",nl}),
%	    emit({"   InnerTagLen = length(InnerTag),",nl}),
%	    emit({"   InnerLenEncLen = length(InnerLenEnc),",nl}),
%	    emit({"   ["}),
%	    enc_tag(D#type.tag,TypeTag),
%	    emit({"    ?RT_BER:encode_length(InnerTagLen + InnerLenEncLen + InnerLen), ",nl}),
%	    emit({"    InnerTag, InnerLenEnc, SOfEnc].",nl,nl,nl});
%	%% no tag ->
%	%% implicit ->
%	_ ->
%	    emit({indent(3),"["}),
%	    enc_tag(D#type.tag,TypeTag),
%	    emit({indent(4),"?RT_BER:encode_length(length(lists:flatten(SOfEnc)))",",SOfEnc].",nl,nl,nl})
%    end,

    emit(["'enc_",Typename,"_components'([], AccBytes, AccLen) -> ",nl,
	  indent(3),"{lists:reverse(AccBytes),AccLen};",nl,nl]),
    emit(["'enc_",Typename,"_components'([H|T], AccBytes, AccLen) ->",nl]),
    asn1ct_constructed_ber:gen_enc_line(Typename,TypeNameSuffix,Cont,"H",0,
					mandatory,"{EncBytes,EncLen} = "),
    emit([",",nl]),
    emit([indent(3),"'enc_",Typename,"_components'(T,"]), 
    emit(["[EncBytes|AccBytes], AccLen + EncLen).",nl,nl]).

    
%%% the component encoder
%    Conttype = asn1ct_gen:get_inner(Cont#type.def),
%    case asn1ct_gen:type(Conttype) of
%	{primitive,bif} ->
%	    asn1ct_gen_ber:gen_encode_prim(ber,Cont,tag,"H");
%	{constructed,bif} ->
%	    NewTypename = list_to_atom(lists:concat([Typename,TypeNameSuffix])),
%	    emit({"'enc_",NewTypename,"'(H)"});
%	{Form,user} ->
%	    emit({"'enc_",Conttype,"'(H)"});
%	#'Externaltypereference'{module=EMod,type=EType} ->
%	    emit({"'",EMod,"':'enc_",EType,"'(H)"});
%	#'typereference'{val = EType} ->
%	    emit({"'enc_",EType,"'(H)"})
%    end,
% emit({" | Acc]).",nl,nl}).


gen_decode_sof(Typename,InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, TypeTag, TypeNameSuffix, Cont} = 
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',"SETOF",_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',"SEQOF",_Cont}
	end,
    
    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),
    
    asn1ct_name:new(rb),
    MyTag = 
	case D#type.tag of
	     undefined -> []; 
	     Tag -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	end ++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(TypeTag),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),

    emit(["   ?RT_BER:decode_components(",{curr,rb}]),
    InnerType = asn1ct_gen:get_inner(Cont#type.def),
    ContName = case asn1ct_gen:type(InnerType) of
		   Atom when atom(Atom) -> Atom;
		   _ -> TypeNameSuffix
	       end,
    emit([", Len, ",{next,bytes},", "]),
    gen_dec_line_sof(Typename,ContName,Cont),
    emit([", []).",nl,nl,nl]).
    
%    emit(["'dec_",Typename,
%	  "_components'(Rb,Num,Bytes,OptOrMand,Acc) when Num == 0 ->",nl,
%	  indent(3),
%	  "{lists:reverse(Acc),Bytes,Rb};",nl,nl]),
%    emit(["'dec_",Typename,
%	  "_components'(Rb,Num,Bytes,OptOrMand,Acc) when Num < 0 ->",nl,
%	  indent(3),
%	  "exit({error,{asn1,{length_error,",{asis,SeqOrSetOf},"}}});",nl,nl]),
%    emit(["'dec_",Typename,
%	  "_components'(Rb,Num,Bytes,OptOrMand,Acc) ->",nl]),
%    emit([indent(3),"{Term, Remain, Rb1} = "]),
%    InnerType = asn1ct_gen:get_inner(Cont#type.def),
%    ContName = case asn1ct_gen:type(InnerType) of
%		   Atom when atom(Atom) -> Atom;
%		   _ -> TypeNameSuffix
%	       end,
%    gen_dec_line(Typename,ContName,Cont,mandatory),
%    emit([",",nl]),
%    emit([indent(3),"'dec_",Typename,
%	  "_components'(Rb+Rb1, Num-Rb1, Remain, OptOrMand, [Term|Acc]).",nl]). 



%%============================================================================
%%  Encode/decode CHOICE
%%
%%============================================================================

gen_encode_choice(Typename,D) when record(D,type) ->
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_choice(Typename,ChoiceTag,CompList1,Ext),
    emit({nl,nl}).

gen_decode_choice(Module,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_dec_choice(Module,Typename,ChoiceTag,CompList1,Ext),
    emit({".",nl}).


%%============================================================================
%%  Encode SEQUENCE
%%
%%============================================================================

gen_enc_sequence_call(TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Pos,Ext) ->
    asn1ct_name:new(encBytes),
    asn1ct_name:new(encLen),
%    asn1ct_name:new(att),
    Element = io_lib:format("?RT_BER:cindex(~w,Val,~w)",[Pos+1,Cname]),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Prop),
    gen_enc_line(TopType,Cname,Type,Element,3,Prop),
%    case Prop of
%	mandatory ->
%	    gen_enc_line(TopType,Cname,Type,Element,3,true);
%	_ ->
%	    emit({"   ",{curr,att}," = ",nl}),
%	    emit({"      case ",Element," of",nl}),
%	    case Prop of
%		'OPTIONAL' -> emit({indent(9),"asn1_NOVALUE -> [];",nl});
%		{'DEFAULT',_} -> emit({indent(9),"asn1_DEFAULT -> [];",nl})
%	    end,
%	    emit({indent(9),"_ ->",nl}),
%	    gen_enc_line(TopType,Cname,Type,Element,12,false),
%	    emit({nl,indent(7),"end"})
%    end,
    case Rest of
	[] ->
	    emit({com,nl});
	_ ->
	    emit({com,nl}),
	    gen_enc_sequence_call(TopType,Rest,Pos+1,Ext)
    end;

gen_enc_sequence_call(_TopType,[],_Num,_) ->
	true.

%%============================================================================
%%  Decode SEQUENCE
%%
%%============================================================================

gen_dec_sequence_call(Module,TopType,CompList,Ext) ->
    gen_dec_sequence_call1(Module, TopType, CompList, 1, Ext).


gen_dec_sequence_call1(Module,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Num,Ext) ->
    gen_dec_component(Module,TopType,Cname,Type,Num,Prop,Ext),
    case Rest of
	[] ->
	    true;
	_ ->
	    emit({com,nl}),
	    asn1ct_name:new(term),
	    asn1ct_name:new(bytes),
	    gen_dec_sequence_call1(Module,TopType,Rest,Num+1,Ext)
    end;

gen_dec_sequence_call1(Module,_TopType,[],1,_) ->
    no_terms;
gen_dec_sequence_call1(Module,_TopType,[],Num,_) ->
    true.



%%----------------------------
%%SEQUENCE mandatory
%%----------------------------

gen_dec_component(Module,TopType,Cname,Type,Pos,Prop,Ext) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    Prop1 = case {Prop,Ext} of
		{mandatory,{ext,Epos,_}} when Pos >= Epos ->
		    'OPTIONAL';
		_ ->
		    Prop
	    end,
    print_attribute_comment(InnerType,Pos,Prop1),
    emit("   "),
	
    emit({"{",{curr,term},",",
	  {next,bytes},",",
	  {next,rb},"} = "}),
    asn1ct_name:new(rb),
    gen_dec_line(TopType,Cname,Type,Prop1),
    asn1ct_name:new(form).


%%-------------------------------------
%%  Decode SET
%%-------------------------------------

gen_dec_set(Module,TopType,CompList,Pos,Ext) ->
    TagList = get_all_choice_tags(CompList),
    emit({indent(3),
	  {curr,tagList}," = ",{asis,TagList},",",nl}),
    emit({indent(3),
	  "case ?RT_BER:check_if_valid_tag(Bytes, ",
	  {curr,tagList},", OptOrMand) of",nl}),
    asn1ct_name:new(tagList),
    asn1ct_name:new(rbCho),
    asn1ct_name:new(choTags),
    gen_dec_set_cases(Module,TopType,CompList,TagList,Pos),
    asn1ct_name:new(tag),
    asn1ct_name:new(bytes).



gen_dec_set_cases(Module,TopType,[],List,Pos) ->
    ok;
gen_dec_set_cases(Module,TopType,[H|T],List,Pos) ->
    case H of
	{'EXTENSIONMARK', _, _} ->
	    gen_dec_set_cases(Module,TopType,T,List,Pos);
	_ ->
	    Name = H#'ComponentType'.name,
	    Type = H#'ComponentType'.typespec,
	    TypeDef = Type#type.def,    
	    Tags = H#'ComponentType'.tags,

	    emit({indent(6),"'",Name,"' ->",nl}),
	    case Type#type.def of
		{'CHOICE',NewCompList} -> 
		    gen_dec_set_cases_choice(Module,TopType,H,Pos);
		_ ->
		    gen_dec_set_cases_type(Module,TopType,H,Pos)
	    end,
	    gen_dec_set_cases(Module,TopType,T,List,Pos+1)
    end.


gen_dec_set_cases_choice(Module,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    emit({"'dec_",TopType,"_",Cname,"'(Bytes,OptOrMand),",nl}),
    emit({indent(9),"{{",{asis,Cname},", Dec}, Rest, "}),
    emit({{curr,rbCho},"};",nl,nl}).


gen_dec_set_cases_type(Module,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    %% always use Prop = mandatory here    Prop = H#'ComponentType'.prop,
%    {Enc_type, Class, Number} = asn1ct_gen_ber:check_tag(Type#type.tag, Type#type.def),

    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    asn1ct_name:delete(bytes),
    %% we have already seen the tag so now we must find the value 
    %% that why we always use 'mandatory' here
    gen_dec_line(TopType,Cname,Type,mandatory), 
    asn1ct_name:new(bytes),
    
    emit({com,nl,indent(9),"'dec_",TopType,"_set_loop'(Rest, OptOrMand, ",
	  "S_Term++[{",Pos,", Dec}], S_Rb-"}),
    emit({{curr,rbCho},");",nl,nl}).


%%---------------------------------
%%  Decode SET result
%%---------------------------------

gen_dec_set_result(Module,TopType,{CompList,ExtList}) ->
    gen_dec_set_result1(Module, TopType, CompList, 1);
gen_dec_set_result(Module,TopType,CompList) ->
    gen_dec_set_result1(Module, TopType, CompList, 1).

gen_dec_set_result1(Module,TopType,
		   [#'ComponentType'{name=Cname,
				     typespec=Type,
				     prop=Prop,tags=Tags}|Rest],Num) ->
    gen_dec_set_component(Module,TopType,Cname,Type,Num,Prop),
    case Rest of
	[] ->
	    true;
	_ ->
	    gen_dec_set_result1(Module,TopType,Rest,Num+1)
    end;

gen_dec_set_result1(_Module,_TopType,[],1) ->
    no_terms;
gen_dec_set_result1(_Module,_TopType,[],Num) ->
    true.


gen_dec_set_component(Module,TopType,Cname,Type,Pos,Prop) ->
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Prop),
    emit({"   {",{next,term},com,{next,termList},"} =",nl}),
    emit({"      case ",{curr,termList}," of",nl}),
    emit({"          [{",Pos,com,{curr,termTmp},"}|",
	  {curr,rest},"] -> "}),
    emit({"{",{curr,termTmp},com,
	  {curr,rest},"};",nl}),
    case Prop of
	'OPTIONAL' ->
	    emit([indent(10),"_ -> {asn1_NOVALUE, ",{curr,termList},"}",nl]);
	{'DEFAULT', DefVal} ->
	    emit([indent(10),
		  "_ -> {",{asis,DefVal},", ",{curr,termList},"}",nl]);
	mandatory ->
	    emit([indent(10),
		  "_ -> exit({error,{asn1,{mandatory_attribute_no, ",
		  Pos,", missing}}})",nl])
    end,
    emit([indent(6),"end,",nl]),
    asn1ct_name:new(rest),
    asn1ct_name:new(term),
    asn1ct_name:new(termList),    
    asn1ct_name:new(termTmp).    
    

%%---------------------------------------------
%%  Encode CHOICE
%%---------------------------------------------
%% for BER we currently do care (a little) if the choice has an EXTENSIONMARKER


gen_enc_choice(TopType,Tag,CompList,_Ext) ->
    gen_enc_choice1(TopType,Tag,CompList,_Ext).

gen_enc_choice1(TopType,Tag,CompList,_Ext) ->
    asn1ct_name:clear(),
    emit({"   {EncBytes,EncLen} = case element(1,Val) of",nl}),
    gen_enc_choice2(TopType,CompList),
    emit([nl,"   end,",nl,nl]),
    NewTag = case Tag of
		 undefined -> [];
		 _ -> [Tag#tag{class=asn1ct_gen_ber:decode_class(Tag#tag.class)}]
	     end,
    emit(["?RT_BER:encode_tags(TagIn ++",{asis,NewTag},", EncBytes, EncLen).",nl]).
%    emit({"   ["}),
%    emit({"?RT_BER:encode_tag_val({",
%	  {asis,asn1ct_gen_ber:decode_class(Class)},",",
%	  {asis,?CONSTRUCTED},",",
%	  {asis,TagNr},"}), ",nl}),

%    emit({"    ?RT_BER:encode_length(length(lists:flatten(Att))), Att].",nl,nl}).




gen_enc_choice2(TopType,[H1|T]) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({"      ",{asis,Cname}," ->",nl}),    
    gen_enc_line(TopType,Cname,Type,"element(2,Val)",9,mandatory,[]),
    emit({";",nl}),
    case T of 
	[] ->
	    emit([indent(6), "Else -> ",nl,
		  indent(9),"exit({error,{asn1,{invalid_choice_type,Else}}})"]);
	_ ->
	    true
    end,
    gen_enc_choice2(TopType,T);

gen_enc_choice2(TopType,[])  ->
    true.




%%--------------------------------------------
%%  Decode CHOICE
%%--------------------------------------------

gen_dec_choice(Module, TopType, ChTag, CompList, Ext) ->
    asn1ct_name:delete(bytes),
%%    TagList = get_all_choice_tags(CompList),
%%    emit({indent(3),
%%	  {curr,tagList}," = ",{asis,TagList},",",nl,nl}),

    Tags = case ChTag of
	       undefined -> [];
	       _T -> 
		   [ChTag#tag{
		      class=asn1ct_gen_ber:decode_class(ChTag#tag.class)}]
	   end,
    emit(["   {FormLen,",{next,bytes},
	  ", RbExp} = ?RT_BER:check_tags(TagIn++",
	  {asis,Tags},", ",
	  {curr,bytes},", OptOrMand),",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),
    emit({indent(3),
	  "case (catch ?RT_BER:peek_tag(",{curr,bytes},")) of",nl}),
    asn1ct_name:new(tagList),
    asn1ct_name:new(choTags),
    gen_dec_choice_cases(Module,TopType,CompList),
    emit({indent(6), {curr,else}," -> ",nl,
	  indent(9),"case OptOrMand of",nl,
	  indent(12),"mandatory ->","exit({error,{asn1,{invalid_choice_tag,",{curr,else},"}}});",nl,
	  indent(12),"_ ->","exit({error,{asn1,{no_optional_tag,",{curr,else},"}}})",nl,
	  indent(9),"end",nl}),
    emit({indent(3),"end"}),
    asn1ct_name:new(tag),
    asn1ct_name:new(else).


gen_dec_choice_cases(Module, TopType, []) ->
    ok;
gen_dec_choice_cases(Module, TopType, [H|T]) ->
    asn1ct_name:push(rbCho),
    Name = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    TypeDef = Type#type.def,
    emit([nl,"%% '",Name,"'",nl]),
    Fcases  = fun([T1,T2|Tail],Fun) ->		      
		      emit([indent(6),{asis,encode_tag_val(T1)}," ->",nl]),
		      gen_dec_choice_cases_type(Module, TopType, H),
		      Fun([T2|Tail],Fun);
		 ([T1],_) ->
		      emit([indent(6),{asis,encode_tag_val(T1)}," ->",nl]),
		      gen_dec_choice_cases_type(Module, TopType, H)
	      end,
    Fcases(H#'ComponentType'.tags,Fcases),
%%    emit([indent(6),{asis,encode_tag_val(H#'ComponentType'.tags)}," ->",nl]),
%%    gen_dec_choice_cases_type(Module, TopType, H),
    asn1ct_name:pop(rbCho),
    gen_dec_choice_cases(Module, TopType, T).


gen_dec_choice_cases_type(Module,TopType,H) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    Prop = H#'ComponentType'.prop,
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    gen_dec_line(TopType,Cname,Type,Prop),
    emit([",",nl,indent(9),"{{",{asis,Cname},
	  ", Dec}, Rest, RbExp + ",
	  {curr,rbCho},"};",nl,nl]).

encode_tag_val({Class,TagNo}) when integer(TagNo) ->
    asn1rt_ber_v1:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,TagNo});
encode_tag_val({Class,TypeName}) ->
    asn1rt_ber_v1:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,asn1ct_gen_ber:decode_type(TypeName)}).


get_all_choice_tags(ComponentTypeList) ->
    get_all_choice_tags(ComponentTypeList,[]).

get_all_choice_tags([],TagList) ->
    TagList;
get_all_choice_tags([H|T],TagList)  ->
    Tags = H#'ComponentType'.tags,
    Type = H#'ComponentType'.typespec,
    get_all_choice_tags(T, TagList ++ [{H#'ComponentType'.name, Tags}]).


%%---------------------------------------
%% Generate the encode/decode code 
%%---------------------------------------

gen_enc_line(TopType,Cname,Type,Element,Indent,OptOrMand) when list(Element) ->
    gen_enc_line(TopType,Cname,Type,Element,Indent,OptOrMand,
		 ["{",{curr,encBytes},",",{curr,encLen},"} = "]).

gen_enc_line(TopType,Cname,Type,Element,Indent,OptOrMand,Assign) when list(Element) ->
    IndDeep = indent(Indent),

%    {TopEnc_type, Top_class, Top_tagNr} = get_top_enc_type(Type#type.tag),
%    AttString = fun(true,I,A) -> lists:concat([I,A," = "]);
%		   (_,I,A) -> I end (Att,IndDeep,
%				     asn1ct_gen_ber:mk_var(asn1ct_name:curr(att))),
    Tag = case _T1 = Type#type.tag of
	      undefined -> [];
	      _ -> [_T1#tag{class=asn1ct_gen_ber:decode_class(_T1#tag.class)}]
	  end,
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    emit(IndDeep),
    emit(Assign),
    case OptOrMand of
	mandatory -> true; % generate nothing
	_ ->
	    emit({" case ",Element," of",nl}),
	    case OptOrMand of
		'OPTIONAL' -> emit({indent(9),"asn1_NOVALUE -> {[],0};",nl});
		{'DEFAULT',_} -> emit({indent(9),"asn1_DEFAULT -> {[],0};",nl})
	    end,
	    emit({indent(9),"_ ->",nl})
    end,
    case WhatKind of
	{primitive,bif} ->
	    asn1ct_gen_ber:gen_encode_prim(ber,Type,{asis,Tag},Element);
	_ ->
	    {EncFunName, EncMod, EncFun} = 
		mkfuncname(TopType,Cname,WhatKind,enc),
	    emit([EncFunName,"(",Element,", ",{asis,Tag},")"])
    end,
    case OptOrMand of
	mandatory -> true;
	_ ->
	    emit({nl,indent(7),"end"})
    end.

gen_dec_line_sof(TopType,Cname,Type) ->
    OptOrMand = mandatory,
    BytesVar = asn1ct_gen_ber:mk_var(asn1ct_name:curr(bytes)),
    Tag = case _T1 = Type#type.tag of
	      undefined -> [];
	      _ -> [_T1#tag{class=asn1ct_gen_ber:decode_class(_T1#tag.class)}]
	  end,
%    {TopEnc_type, Top_class, Top_tagNr} = get_top_enc_type(Type#type.tag),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} ->
	    asn1ct_name:delete(len),
	    Length = asn1ct_gen_ber:mk_var(asn1ct_name:curr(len)),
	    asn1ct_name:new(len),
	    emit(["fun(FBytes,_,_)->",nl]),
	    asn1ct_gen_ber:gen_dec_prim(ber,Type,"FBytes",Tag,
					[],no_length,?PRIMITIVE,
					mandatory),
	    emit([nl,"end, []"]);
	_ ->
	    {DecFunName, DecMod, DecFun} = 
		mkfunname(TopType,Cname,WhatKind,dec),
	    emit([DecFunName,", ",{asis,Tag}])
    end.
    

gen_dec_line(TopType,Cname,Type,OptOrMand)  ->
    BytesVar = asn1ct_gen_ber:mk_var(asn1ct_name:curr(bytes)),
    Tag = case _T1 = Type#type.tag of
	      undefined -> [];
	      _ -> [_T1#tag{class=asn1ct_gen_ber:decode_class(_T1#tag.class)}]
	  end,
%    {TopEnc_type, Top_class, Top_tagNr} = get_top_enc_type(Type#type.tag),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} ->
	    asn1ct_name:delete(len),
	    Length = asn1ct_gen_ber:mk_var(asn1ct_name:curr(len)),
	    asn1ct_name:new(len),
	    asn1ct_gen_ber:gen_dec_prim(ber,Type,BytesVar,Tag,
					[],no_length,?PRIMITIVE,
					OptOrMand);
	_ ->
	    {DecFunName, DecMod, DecFun} = 
		mkfuncname(TopType,Cname,WhatKind,dec),
	    case OptOrMand of
		mandatory ->
		    emit({DecFunName,"(",{curr,bytes},
			  ",mandatory, ",{asis,Tag},")"});
		_ -> %optional or default
		    emit("case (catch "),
		    emit({DecFunName,"(",{curr,bytes},
			  ",opt_or_default, ",{asis,Tag},")"}),
		    emit([") of",nl]),
		    case OptOrMand of
			{'DEFAULT', Def} ->
			    emit(["{'EXIT',{error,{asn1,{no_optional_tag,_}}}}",
				  " -> {",{asis,Def},",",
				  BytesVar,", 0 };",nl]);
			'OPTIONAL' ->
			    emit(["{'EXIT',{error,{asn1,{no_optional_tag,_}}}}",
				  " -> { asn1_NOVALUE, ",
				  BytesVar,", 0 };",nl])
		    end,
		    asn1ct_name:new(casetmp),
		    emit([{curr,casetmp},"-> ",{curr,casetmp},nl,"end"])
	    end
    end.


%%------------------------------------------------------
%% General and special help functions (not exported)
%%------------------------------------------------------


indent(N) ->
    lists:duplicate(N,32). % 32 = space


mkvlist([H,T1|T], Sep) -> % Sep is a string e.g ", " or "+ "
    emit([{var,H},Sep]),
    mkvlist([T1|T], Sep);
mkvlist([H|T], Sep) ->
    emit([{var,H}]),
    mkvlist(T, Sep);
mkvlist([], _) ->
    true.

mkvlist(L) ->
    mkvlist(L,", ").

mkvplus(L) ->
    mkvlist(L," + ").

extensible(CompList) when list(CompList) ->
    noext;
extensible({RootList,ExtList}) ->
    {ext,length(RootList)+1,length(ExtList)}.


print_attribute_comment(InnerType,Pos,Prop) ->
    CommentLine = "%%-------------------------------------------------",
    emit([nl,CommentLine]),
    case InnerType of
	{typereference,_,Name} -> 
	    emit([nl,"%% attribute number ",Pos," with type ",Name]);
	{'Externaltypereference',_,XModule,Name} -> 
	    emit([nl,"%% attribute number ",Pos,"   External ",XModule,":",Name]);
	_ ->
	    emit([nl,"%% attribute number ",Pos," with type ",InnerType])
    end,
    case Prop of
	mandatory ->
	    continue;
	{'DEFAULT', Def} ->
	    emit([" DEFAULT = ",{asis,Def}]);
	'OPTIONAL' ->
	    emit([" OPTIONAL"])
    end,
    emit([nl,CommentLine,nl]).


mk_listhead(L) when list(L), length(L) > 0  ->
    [$[|mk_listhead1(L)].

mk_listhead1([H,H1|T]) ->
    integer_to_list(H) ++ "," ++ mk_listhead([H1|T]);
mk_listhead1([H]) ->
    integer_to_list(H).
    
mkfuncname(TopType,Cname,WhatKind,DecOrEnc) ->
    case WhatKind of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"]),Mod,
	     lists:concat(["'",DecOrEnc,"_",EType,"'"])};
	#'typereference'{val=EType} ->
	    F = lists:concat(["'",DecOrEnc,"_",EType,"'"]),
	    {F, "?MODULE", F};
	{constructed,bif} ->
	    F = lists:concat(["'",DecOrEnc,"_",TopType,"_",Cname,"'"]),
	    {F, "?MODULE", F}
    end.

mkfunname(TopType,Cname,WhatKind,DecOrEnc) ->
    case WhatKind of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["{'",Mod,"','",DecOrEnc,"_",EType,"'}"]),Mod,
	     lists:concat(["'",DecOrEnc,"_",EType,"'"])};
	#'typereference'{val=EType} ->
	    F = lists:concat(["fun '",DecOrEnc,"_",EType,"'/3"]),
	    {F, "?MODULE", F};
	{constructed,bif} ->
	    F = lists:concat(["fun '",DecOrEnc,"_",TopType,"_",Cname,"'/3"]),
	    {F, "?MODULE", F}
    end.
