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

-export([gen_encode_sequence/3]).
-export([gen_decode_sequence/3]).
-export([gen_encode_set/3]).
-export([gen_decode_set/3]).
-export([gen_encode_sof/4]).
-export([gen_decode_sof/4]).
-export([gen_encode_choice/3]).
-export([gen_decode_choice/3]).


-include("asn1_records.hrl").

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

gen_encode_sequence(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(bytes),
    {SeqOrSet,CompList} = 
	case D#type.def of
	    #'SEQUENCE'{components=CL} -> {'SEQUENCE',CL};
	    #'SET'{components=CL} -> {'SET',CL}
	end,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_sequence_call(Erules,Typename,CompList1,1,Ext),

    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++ 
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

gen_decode_sequence(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    #'SEQUENCE'{tablecinf=TCI,components=TCompList} = D#type.def,
    Ext = extensible(TCompList),
    CompList = case TCompList of
		   {Rl,El}  -> Rl ++ El;
		   _ -> TCompList
	       end,

    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),

    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++ 
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
    
    case gen_dec_sequence_call(Erules,Typename,CompList,Ext) of
	no_terms -> 	    
	    emit({nl,nl}),
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit(["   {{'",asn1ct_gen:list2rname(Typename),"'}, ",{curr,bytes},",",nl,"    "]),
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit({"}.",nl});
	_ ->
	    emit({com,nl,nl}),
	    demit({"Result = "}), %dbg
	    %% return value as record
	    asn1ct_name:new(rb),
	    asn1ct_name:new(bytes),
	    ExtStatus = case Ext of
			    {ext,_,_} -> ext;
			    noext -> noext
			end,
	    emit(["{",{next,bytes},",",{curr,rb},"} = ?RT_BER:restbytes2(RemBytes, ",
		  {curr,bytes},",",ExtStatus,"),",nl]),
	    asn1ct_name:new(rb),
	    emit(["   {{'",asn1ct_gen:list2rname(Typename),"', "]),
	    mkvlist(asn1ct_name:all(term)),
	    emit(["}, ",{next,bytes},", "]),
	    asn1ct_gen_ber:add_removed_bytes(),
	    emit({"}.",nl})
    end.



%%============================================================================
%%  Encode/decode SET
%%
%%============================================================================

gen_encode_set(Erules,Typename,D) when record(D,type) ->
    gen_encode_sequence(Erules,Typename,D).

gen_decode_set(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(term),
    asn1ct_name:new(tag),
    #'SET'{tablecinf=TCI,components=TCompList} = D#type.def,
    Ext = extensible(TCompList),
    CompList = case TCompList of
		   {Rl,El} -> Rl ++ El;
		   _ -> TCompList
	       end,

    emit(["   %%-------------------------------------------------",nl]),
    emit(["   %% decode tag and length ",nl]),
    emit(["   %%-------------------------------------------------",nl]),

    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type('SET'),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   {{_,Len},",{next,bytes},",",{curr,rb},
	  "} = ?RT_BER:check_tags(TagIn ++ ",{asis,MyTag},", ",
	  {curr,bytes},", OptOrMand), ",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),
    asn1ct_name:new(rb),

    emit(["   {SetTerm, SetBytes, ",{curr,rb},"} = ?RT_BER:decode_set(0, Len, ",
	  {curr,bytes},", OptOrMand, ",
	  "fun 'dec_",asn1ct_gen:list2name(Typename),"_fun'/2, []),",nl]),
    
    asn1ct_name:new(rb),
    emit(["   'dec_",asn1ct_gen:list2name(Typename),"_result'(lists:sort(SetTerm), SetBytes, "]),
    asn1ct_gen_ber:add_removed_bytes(),
    emit([").",nl,nl,nl]),

    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Set loop fun for ",asn1ct_gen:list2name(Typename),nl}),
    emit({"%%-------------------------------------------------",nl}),

    asn1ct_name:clear(),
    asn1ct_name:new(term),
    emit(["'dec_",asn1ct_gen:list2name(Typename),"_fun'(",{curr,bytes},
	  ", OptOrMand) ->",nl]),
    
    asn1ct_name:new(bytes),
    gen_dec_set(Erules,Typename,CompList,1,Ext),

    emit(["      %% tag not found, if extensionmark we should skip bytes here",nl]),
    emit([indent(6),"_ -> {[], Bytes,0}",nl]),
    emit([indent(3),"end.",nl,nl,nl]),


    emit({"%%-------------------------------------------------",nl}),
    emit({"%% Result ",asn1ct_gen:list2name(Typename),nl}),
    emit({"%%-------------------------------------------------",nl}),

    asn1ct_name:clear(),
    emit({"'dec_",asn1ct_gen:list2name(Typename),"_result'(TermList, Bytes, Rb) ->",nl}),
    
    case gen_dec_set_result(Erules,Typename,CompList) of
	no_terms -> 	    
	    %% return value as record
	    asn1ct_name:new(rb),
	    emit({"   {{'",asn1ct_gen:list2rname(Typename),"'}, Bytes, Rb}.",nl});
	_ ->
	    emit({nl,"   case ",{curr,termList}," of",nl}),
	    emit({"      [] -> {{'",asn1ct_gen:list2rname(Typename),"', "}),
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

gen_encode_sof(Erules,Typename,InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, TypeTag, Cont} = 
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',_Cont}
	end,
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),

    emit({"   {EncBytes,EncLen} = 'enc_",asn1ct_gen:list2name(Typename),"_components'(Val,[],0),",nl}),

    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++ 
	[#tag{class = asn1ct_gen_ber:decode_class('UNIVERSAL'),
	      number = asn1ct_gen_ber:decode_type(TypeTag),
	      form = ?CONSTRUCTED,
	      type = 'IMPLICIT'}],
    emit(["   ?RT_BER:encode_tags(TagIn ++ ",
	  {asis,MyTag},", EncBytes, EncLen).",nl,nl]),
    
    emit(["'enc_",asn1ct_gen:list2name(Typename),"_components'([], AccBytes, AccLen) -> ",nl,
	  indent(3),"{lists:reverse(AccBytes),AccLen};",nl,nl]),
    emit(["'enc_",asn1ct_gen:list2name(Typename),"_components'([H|T], AccBytes, AccLen) ->",nl]),
    {NewCont,Element} = 
	case Cont#type.def of
	    {'ENUMERATED',RefName,Component} -> 
		emit(["   Element2IfTuple = ",nl,"      fun(X)->case X of",nl,
		     "         {",{asis,RefName},",V} -> V;",nl,
		     "         _ -> X",nl,"      end",nl,"   end,",nl]),
		{Cont#type{def={'ENUMERATED',Component}},
		 "Element2IfTuple(H)"};
	    _ -> {Cont,"H"}
	end,
    gen_enc_line(Erules,Typename,TypeNameSuffix,NewCont,Element,0,
		 mandatory,"{EncBytes,EncLen} = "),
%     gen_enc_line(Erules,Typename,TypeNameSuffix,Cont,"H",0,
% 		 mandatory,"{EncBytes,EncLen} = "),
    emit([",",nl]),
    emit([indent(3),"'enc_",asn1ct_gen:list2name(Typename),"_components'(T,"]), 
    emit(["[EncBytes|AccBytes], AccLen + EncLen).",nl,nl]).

    

gen_decode_sof(Erules,Typename,InnerTypename,D) when record(D,type) ->
    asn1ct_name:start(),
    {SeqOrSetOf, TypeTag, Cont} = 
	case D#type.def of
	    {'SET OF',_Cont} -> {'SET OF','SET',_Cont};
	    {'SEQUENCE OF',_Cont} -> {'SEQUENCE OF','SEQUENCE',_Cont}
	end,
    TypeNameSuffix = asn1ct_gen:constructed_suffix(SeqOrSetOf,Cont#type.def),
    
    emit({"   %%-------------------------------------------------",nl}),
    emit({"   %% decode tag and length ",nl}),
    emit({"   %%-------------------------------------------------",nl}),
    
    asn1ct_name:new(rb),
    MyTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- D#type.tag]
	++ 
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
    NewCont = 
	case Cont#type.def of
	    {'ENUMERATED',_,Components}-> 
		Cont#type{def={'ENUMERATED',Components}};
	    _ -> Cont
	end,
    gen_dec_line_sof(Erules,Typename,ContName,NewCont),
    emit([", []).",nl,nl,nl]).
    


%%============================================================================
%%  Encode/decode CHOICE
%%
%%============================================================================

gen_encode_choice(Erules,Typename,D) when record(D,type) ->
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_enc_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit({nl,nl}).

gen_decode_choice(Erules,Typename,D) when record(D,type) ->
    asn1ct_name:start(),
    asn1ct_name:new(bytes),
    ChoiceTag = D#type.tag,
    {'CHOICE',CompList} = D#type.def,
    Ext = extensible(CompList),
    CompList1 = case CompList of
		    {Rl,El} -> Rl ++ El;
		    _ -> CompList
		end,
    gen_dec_choice(Erules,Typename,ChoiceTag,CompList1,Ext),
    emit({".",nl}).


%%============================================================================
%%  Encode SEQUENCE
%%
%%============================================================================

gen_enc_sequence_call(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Pos,Ext) ->
    asn1ct_name:new(encBytes),
    asn1ct_name:new(encLen),
    Element = io_lib:format("?RT_BER:cindex(~w,Val,~w)",[Pos+1,Cname]),
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    print_attribute_comment(InnerType,Pos,Prop),
    gen_enc_line(Erules,TopType,Cname,Type,Element,3,Prop),
    case Rest of
	[] ->
	    emit({com,nl});
	_ ->
	    emit({com,nl}),
	    gen_enc_sequence_call(Erules,TopType,Rest,Pos+1,Ext)
    end;

gen_enc_sequence_call(_Erules,_TopType,[],_Num,_) ->
	true.

%%============================================================================
%%  Decode SEQUENCE
%%
%%============================================================================

gen_dec_sequence_call(Erules,TopType,CompList,Ext) ->
    gen_dec_sequence_call1(Erules,TopType, CompList, 1, Ext).


gen_dec_sequence_call1(Erules,TopType,[#'ComponentType'{name=Cname,typespec=Type,prop=Prop,tags=Tags}|Rest],Num,Ext) ->
    gen_dec_component(Erules,TopType,Cname,Tags,Type,Num,Prop,Ext),
    case Rest of
	[] ->
	    true;
	_ ->
	    emit({com,nl}),
	    asn1ct_name:new(term),
	    asn1ct_name:new(bytes),
	    gen_dec_sequence_call1(Erules,TopType,Rest,Num+1,Ext)
    end;

gen_dec_sequence_call1(Erules,_TopType,[],1,_) ->
    no_terms;
gen_dec_sequence_call1(Erules,_TopType,[],Num,_) ->
    true.



%%----------------------------
%%SEQUENCE mandatory
%%----------------------------

gen_dec_component(Erules,TopType,Cname,CTags,Type,Pos,Prop,Ext) ->
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
    gen_dec_line(Erules,TopType,Cname,CTags,Type,Prop1),
    asn1ct_name:new(form).


%%-------------------------------------
%%  Decode SET
%%-------------------------------------

gen_dec_set(Erules,TopType,CompList,Pos,Ext) ->
    TagList = get_all_choice_tags(CompList),
    emit({indent(3),
	  {curr,tagList}," = ",{asis,TagList},",",nl}),
    emit({indent(3),
	  "case ?RT_BER:check_if_valid_tag(Bytes, ",
	  {curr,tagList},", OptOrMand) of",nl}),
    asn1ct_name:new(tagList),
    asn1ct_name:new(rbCho),
    asn1ct_name:new(choTags),
    gen_dec_set_cases(Erules,TopType,CompList,TagList,Pos),
    asn1ct_name:new(tag),
    asn1ct_name:new(bytes).



gen_dec_set_cases(Erules,TopType,[],List,Pos) ->
    ok;
gen_dec_set_cases(Erules,TopType,[H|T],List,Pos) ->
    case H of
	{'EXTENSIONMARK', _, _} ->
	    gen_dec_set_cases(Erules,TopType,T,List,Pos);
	_ ->
	    Name = H#'ComponentType'.name,
	    Type = H#'ComponentType'.typespec,
	    TypeDef = Type#type.def,    
	    Tags = H#'ComponentType'.tags,

	    emit({indent(6),"'",Name,"' ->",nl}),
	    case Type#type.def of
		{'CHOICE',NewCompList} -> 
		    gen_dec_set_cases_choice(Erules,TopType,H,Pos);
		_ ->
		    gen_dec_set_cases_type(Erules,TopType,H,Pos)
	    end,
	    gen_dec_set_cases(Erules,TopType,T,List,Pos+1)
    end.


gen_dec_set_cases_choice(Erules,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    emit({"'dec_",asn1ct_gen:list2name([Cname|TopType]),"'(Bytes,OptOrMand),",nl}),
    emit([",",nl]),
    emit(["{{",Pos,",Dec}, Rest, ",{curr,rbCho},"}"]),
    emit([";",nl,nl]).


gen_dec_set_cases_type(Erules,TopType,H,Pos) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    %% always use Prop = mandatory here    Prop = H#'ComponentType'.prop,

    asn1ct_name:new(rbCho),
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    asn1ct_name:delete(bytes),
    %% we have already seen the tag so now we must find the value 
    %% that why we always use 'mandatory' here
    gen_dec_line(Erules,TopType,Cname,[],Type,mandatory), 
    asn1ct_name:new(bytes),
    
    emit([",",nl]),
    emit(["{{",Pos,",Dec}, Rest, ",{curr,rbCho},"}"]),
    emit([";",nl,nl]).


%%---------------------------------
%%  Decode SET result
%%---------------------------------

gen_dec_set_result(Erules,TopType,{CompList,ExtList}) ->
    gen_dec_set_result1(Erules,TopType, CompList, 1);
gen_dec_set_result(Erules,TopType,CompList) ->
    gen_dec_set_result1(Erules,TopType, CompList, 1).

gen_dec_set_result1(Erules,TopType,
		   [#'ComponentType'{name=Cname,
				     typespec=Type,
				     prop=Prop,tags=Tags}|Rest],Num) ->
    gen_dec_set_component(Erules,TopType,Cname,Type,Num,Prop),
    case Rest of
	[] ->
	    true;
	_ ->
	    gen_dec_set_result1(Erules,TopType,Rest,Num+1)
    end;

gen_dec_set_result1(Erules,_TopType,[],1) ->
    no_terms;
gen_dec_set_result1(Erules,_TopType,[],Num) ->
    true.


gen_dec_set_component(Erules,TopType,Cname,Type,Pos,Prop) ->
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


gen_enc_choice(Erules,TopType,Tag,CompList,_Ext) ->
    gen_enc_choice1(Erules,TopType,Tag,CompList,_Ext).

gen_enc_choice1(Erules,TopType,Tag,CompList,_Ext) ->
    asn1ct_name:clear(),
    emit({"   {EncBytes,EncLen} = case element(1,Val) of",nl}),
    gen_enc_choice2(Erules,TopType,CompList),
    emit([nl,"   end,",nl,nl]),
    NewTag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- Tag],
    
    emit(["?RT_BER:encode_tags(TagIn ++",{asis,NewTag},", EncBytes, EncLen).",nl]).


gen_enc_choice2(Erules,TopType,[H1|T]) when record(H1,'ComponentType') ->
    Cname = H1#'ComponentType'.name,
    Type = H1#'ComponentType'.typespec,
    emit({"      ",{asis,Cname}," ->",nl}),    
    gen_enc_line(Erules,TopType,Cname,Type,"element(2,Val)",9,mandatory,[]),
    emit({";",nl}),
    case T of 
	[] ->
	    emit([indent(6), "Else -> ",nl,
		  indent(9),"exit({error,{asn1,{invalid_choice_type,Else}}})"]);
	_ ->
	    true
    end,
    gen_enc_choice2(Erules,TopType,T);

gen_enc_choice2(Erules,TopType,[])  ->
    true.




%%--------------------------------------------
%%  Decode CHOICE
%%--------------------------------------------

gen_dec_choice(Erules,TopType, ChTag, CompList, Ext) ->
    asn1ct_name:delete(bytes),
    Tags = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}|| X <- ChTag],
    
    emit(["   {FormLen,",{next,bytes},
	  ", RbExp} = ?RT_BER:check_tags(TagIn++",
	  {asis,Tags},", ",
	  {curr,bytes},", OptOrMand),",nl]),
    asn1ct_name:new(bytes),
    asn1ct_name:new(len),
    case Erules of
	ber_bin ->
	    emit([indent(3),"case ",{curr,bytes}," of",nl]);
	ber ->
	    emit([indent(3),
		  "case (catch ?RT_BER:peek_tag(",{curr,bytes},")) of",nl])
    end,
    asn1ct_name:new(tagList),
    asn1ct_name:new(choTags),
    gen_dec_choice_cases(Erules,TopType,CompList),
    emit({indent(6), {curr,else}," -> ",nl,
	  indent(9),"case OptOrMand of",nl,
	  indent(12),"mandatory ->","exit({error,{asn1,{invalid_choice_tag,",{curr,else},"}}});",nl,
	  indent(12),"_ ->","exit({error,{asn1,{no_optional_tag,",{curr,else},"}}})",nl,
	  indent(9),"end",nl}),
    emit({indent(3),"end"}),
    asn1ct_name:new(tag),
    asn1ct_name:new(else).


gen_dec_choice_cases(Erules,TopType, []) ->
    ok;
gen_dec_choice_cases(Erules,TopType, [H|T]) ->
    asn1ct_name:push(rbCho),
    Name = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    TypeDef = Type#type.def,
    emit([nl,"%% '",Name,"'",nl]),
    Fcases  = fun([T1,T2|Tail],Fun) ->		      
		      emit([indent(6),match_tag(Erules,T1)," ->",nl]),
		      gen_dec_choice_cases_type(Erules,TopType, H),
		      Fun([T2|Tail],Fun);
		 ([T1],_) ->
		      emit([indent(6),match_tag(Erules,T1)," ->",nl]),
		      gen_dec_choice_cases_type(Erules,TopType, H)
	      end,
    Fcases(H#'ComponentType'.tags,Fcases),
    asn1ct_name:pop(rbCho),
    gen_dec_choice_cases(Erules,TopType, T).


gen_dec_choice_cases_type(Erules,TopType,H) ->
    Cname = H#'ComponentType'.name,
    Type = H#'ComponentType'.typespec,
    Prop = H#'ComponentType'.prop,
    emit({indent(9),"{Dec, Rest, ",{curr,rbCho},"} = "}),
    gen_dec_line(Erules,TopType,Cname,[],Type,Prop),
    emit([",",nl,indent(9),"{{",{asis,Cname},
	  ", Dec}, Rest, RbExp + ",
	  {curr,rbCho},"};",nl,nl]).

encode_tag_val(Erules,{Class,TagNo}) when integer(TagNo) ->
    Rtmod = rtmod(Erules),
    Rtmod:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,TagNo});
encode_tag_val(Erules,{Class,TypeName}) ->
    Rtmod = rtmod(Erules),
    Rtmod:encode_tag_val({asn1ct_gen_ber:decode_class(Class),
				  0,asn1ct_gen_ber:decode_type(TypeName)}).


match_tag(ber_bin,Arg) ->
    match_tag_with_bitsyntax(Arg);
match_tag(Erules,Arg) ->
    io_lib:format("~p",[encode_tag_val(Erules,Arg)]).

match_tag_with_bitsyntax({Class,TagNo}) when integer(TagNo) ->
    match_tag_with_bitsyntax1({asn1ct_gen_ber:decode_class(Class),
				  0,TagNo});
match_tag_with_bitsyntax({Class,TypeName}) ->
    match_tag_with_bitsyntax1({asn1ct_gen_ber:decode_class(Class),
				  0,asn1ct_gen_ber:decode_type(TypeName)}).

match_tag_with_bitsyntax1({Class, Form, TagNo}) when (TagNo =< 30) -> 
    io_lib:format("<<~p:2,_:1,~p:5,_/binary>>",[Class bsr 6,TagNo]);

match_tag_with_bitsyntax1({Class, Form, TagNo}) -> 
    {Octets,Len} = mk_object_val(TagNo),
    OctForm = case Len of
		 1 -> "~p";
		 2 -> "~p,~p";
		 3 -> "~p,~p,~p";
		 4 -> "~p,~p,~p,~p"
	     end,
    io_lib:format("<<~p:2,_:1,31:5," ++ OctForm ++ ",_/binary>>",
		  [Class bsr 6] ++ Octets). 

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

gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand) when list(Element) ->
    gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,
		 ["{",{curr,encBytes},",",{curr,encLen},"} = "]).

gen_enc_line(Erules,TopType,Cname,Type,Element,Indent,OptOrMand,Assign) when list(Element) ->
    IndDeep = indent(Indent),

    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    emit(IndDeep),
    emit(Assign),
    case OptOrMand of
	mandatory -> true; % generate nothing
	_ ->
	    emit({" case ",Element," of",nl}),
	    case OptOrMand of
		'OPTIONAL' -> emit({indent(9),"asn1_NOVALUE -> {",empty_lb(Erules),",0};",nl});
		{'DEFAULT',_} -> emit({indent(9),"asn1_DEFAULT -> {",empty_lb(Erules),",0};",nl})
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

gen_dec_line_sof(Erules,TopType,Cname,Type) ->
    OptOrMand = mandatory,
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],    
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    case WhatKind of
	{primitive,bif} ->
	    asn1ct_name:delete(len),
	    Length = asn1ct_gen:mk_var(asn1ct_name:curr(len)),
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
    

gen_dec_line(Erules,TopType,Cname,CTags,Type,OptOrMand)  ->
    BytesVar = asn1ct_gen:mk_var(asn1ct_name:curr(bytes)),
    Tag = [X#tag{class=asn1ct_gen_ber:decode_class(X#tag.class)}
	   || X <- Type#type.tag],
    InnerType = asn1ct_gen:get_inner(Type#type.def),
    WhatKind = asn1ct_gen:type(InnerType),
    case OptOrMand of
	mandatory ->
	    case WhatKind of
		{primitive,bif} ->
		    asn1ct_gen_ber:gen_dec_prim(Erules,Type,BytesVar,Tag,
						[],no_length,?PRIMITIVE,
						mandatory);
		_ ->
		    {DecFunName, DecMod, DecFun} = 
			mkfuncname(TopType,Cname,WhatKind,dec),
		    emit({DecFunName,"(",{curr,bytes},
			  ",mandatory, ",{asis,Tag},")"})
	    end;
	_ -> %optional or default
	    case {CTags,Erules} of
		{[CTag],ber_bin} ->
		    emit(["case ",{curr,bytes}," of",nl]),
		    emit([match_tag(Erules,CTag)," ->",nl]),
		    case WhatKind of
			{primitive,bif} ->
			    asn1ct_gen_ber:gen_dec_prim(Erules,Type,BytesVar,Tag,
							[],no_length,?PRIMITIVE,
							mandatory),
			    emit([";",nl]);
			_ ->
			    {DecFunName, DecMod, DecFun} = 
				mkfuncname(TopType,Cname,WhatKind,dec),
			    emit([DecFunName,"(",{curr,bytes},
				  ",opt_or_default, ",{asis,Tag},");",nl])
		    end,
		    emit(["_ ->",nl]),
		    case OptOrMand of
			{'DEFAULT', Def} ->
			    emit(["{",{asis,Def},",",
				  BytesVar,", 0 }",nl]);
			'OPTIONAL' ->
			    emit(["{ asn1_NOVALUE, ",
				  BytesVar,", 0 }",nl])
		    end,
		    emit("end");
		_ ->
		    emit("case (catch "),
		    case WhatKind of
			{primitive,bif} ->
			    asn1ct_gen_ber:gen_dec_prim(Erules,Type,BytesVar,Tag,
							[],no_length,?PRIMITIVE,
							OptOrMand);
			_ ->
			    {DecFunName, DecMod, DecFun} = 
				mkfuncname(TopType,Cname,WhatKind,dec),
			    emit({DecFunName,"(",{curr,bytes},
				  ",opt_or_default, ",{asis,Tag},")"})
		    end,
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


mkfuncname(TopType,Cname,WhatKind,DecOrEnc) ->
    case WhatKind of
	#'Externaltypereference'{module=Mod,type=EType} ->
	    {lists:concat(["'",Mod,"':'",DecOrEnc,"_",EType,"'"]),Mod,
	     lists:concat(["'",DecOrEnc,"_",EType,"'"])};
	#'typereference'{val=EType} ->
	    F = lists:concat(["'",DecOrEnc,"_",EType,"'"]),
	    {F, "?MODULE", F};
	{constructed,bif} ->
	    F = lists:concat(["'",DecOrEnc,"_",asn1ct_gen:list2name([Cname|TopType]),"'"]),
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
	    F = lists:concat(["fun '",DecOrEnc,"_",asn1ct_gen:list2name([Cname|TopType]),"'/3"]),
	    {F, "?MODULE", F}
    end.

empty_lb(ber) ->
    "[]";
empty_lb(ber_bin) ->
    "<<>>".

rtmod(ber) ->
    list_to_atom(?RT_BER);
rtmod(ber_bin) ->
    list_to_atom(?RT_BER_BIN).




