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
-module(asn1rt_ber_v1).
 
%% encoding / decoding of BER  
 
-export([fixoptionals/2,split_list/2,cindex/3,
	 restbytes/3, % remove this soon but change name on asn1rt_ber then
	 restbytes2/3,list_to_record/2,
	 encode_tag_val/1,decode_tag/1,peek_tag/1,
	 check_tags/3, encode_tags/3]).
-export([encode_boolean/2,decode_boolean/3,
	 encode_integer/3,encode_integer/4,
	 decode_integer/4,decode_integer/5, encode_enumerated/2,
	 encode_enumerated/4,decode_enumerated/5,
	 encode_real/2,decode_real/4,
	 encode_bit_string/4,decode_bit_string/6,
	 decode_compact_bit_string/6,
	 encode_octet_string/3,decode_octet_string/5,
	 encode_null/2,decode_null/3,
	 encode_object_identifier/2,decode_object_identifier/3,
	 encode_restricted_string/4,decode_restricted_string/6,
	 encode_universal_string/3,decode_universal_string/5,
	 encode_BMP_string/3,decode_BMP_string/5,
	 encode_generalized_time/3,decode_generalized_time/5,
	 encode_utc_time/3,decode_utc_time/5,
	 encode_length/1,decode_length/1,
	 remove_length/1,check_if_valid_tag/3,
	 decode_tag_and_length/1, decode_components/6, decode_set/6]).

-export([encode_open_type/1, decode_open_type/1]).
-export([skipvalue/1, skipvalue/2]).
 
-include("asn1_records.hrl"). 
 
% the encoding of class of tag bits 8 and 7 
-define(UNIVERSAL,   0). 
-define(APPLICATION, 16#40). 
-define(CONTEXT,     16#80). 
-define(PRIVATE,     16#C0). 
 
%%% primitive or constructed encoding % bit 6 
-define(PRIMITIVE,   0). 
-define(CONSTRUCTED, 2#00100000). 

%%% The tag-number for universal types
-define(N_BOOLEAN, 1). 
-define(N_INTEGER, 2). 
-define(N_BIT_STRING, 3).
-define(N_OCTET_STRING, 4).
-define(N_NULL, 5). 
-define(N_OBJECT_IDENTIFIER, 6). 
-define(N_OBJECT_DESCRIPTOR, 7). 
-define(N_EXTERNAL, 8). 
-define(N_REAL, 9). 
-define(N_ENUMERATED, 10). 
-define(N_EMBEDDED_PDV, 11). 
-define(N_SEQUENCE, 16). 
-define(N_SET, 17). 
-define(N_NumericString, 18).
-define(N_PrintableString, 19).
-define(N_TeletexString, 20).
-define(N_VideotexString, 21).
-define(N_IA5String, 22).
-define(N_UTCTime, 23). 
-define(N_GeneralizedTime, 24). 
-define(N_GraphicString, 25).
-define(N_VisibleString, 26).
-define(N_GeneralString, 27).
-define(N_UniversalString, 28).
-define(N_BMPString, 30).

 
% the complete tag-word of built-in types 
-define(T_BOOLEAN,          ?UNIVERSAL bor ?PRIMITIVE bor 1). 
-define(T_INTEGER,          ?UNIVERSAL bor ?PRIMITIVE bor 2). 
-define(T_BIT_STRING,       ?UNIVERSAL bor ?PRIMITIVE bor 3). % can be CONSTRUCTED 
-define(T_OCTET_STRING,     ?UNIVERSAL bor ?PRIMITIVE bor 4). % can be CONSTRUCTED 
-define(T_NULL,             ?UNIVERSAL bor ?PRIMITIVE bor 5). 
-define(T_OBJECT_IDENTIFIER,?UNIVERSAL bor ?PRIMITIVE bor 6). 
-define(T_OBJECT_DESCRIPTOR,?UNIVERSAL bor ?PRIMITIVE bor 7). 
-define(T_EXTERNAL,         ?UNIVERSAL bor ?PRIMITIVE bor 8). 
-define(T_REAL,             ?UNIVERSAL bor ?PRIMITIVE bor 9). 
-define(T_ENUMERATED,       ?UNIVERSAL bor ?PRIMITIVE bor 10). 
-define(T_EMBEDDED_PDV,     ?UNIVERSAL bor ?PRIMITIVE bor 11). 
-define(T_SEQUENCE,         ?UNIVERSAL bor ?CONSTRUCTED bor 16). 
-define(T_SET,              ?UNIVERSAL bor ?CONSTRUCTED bor 17). 
-define(T_NumericString,    ?UNIVERSAL bor ?PRIMITIVE bor 18). %can be constructed 
-define(T_PrintableString,  ?UNIVERSAL bor ?PRIMITIVE bor 19). %can be constructed 
-define(T_TeletexString,    ?UNIVERSAL bor ?PRIMITIVE bor 20). %can be constructed 
-define(T_VideotexString,   ?UNIVERSAL bor ?PRIMITIVE bor 21). %can be constructed 
-define(T_IA5String,        ?UNIVERSAL bor ?PRIMITIVE bor 22). %can be constructed 
-define(T_UTCTime,          ?UNIVERSAL bor ?PRIMITIVE bor 23). 
-define(T_GeneralizedTime,  ?UNIVERSAL bor ?PRIMITIVE bor 24). 
-define(T_GraphicString,    ?UNIVERSAL bor ?PRIMITIVE bor 25). %can be constructed 
-define(T_VisibleString,    ?UNIVERSAL bor ?PRIMITIVE bor 26). %can be constructed 
-define(T_GeneralString,    ?UNIVERSAL bor ?PRIMITIVE bor 27). %can be constructed 
-define(T_UniversalString,  ?UNIVERSAL bor ?PRIMITIVE bor 28). %can be constructed 
-define(T_BMPString,        ?UNIVERSAL bor ?PRIMITIVE bor 30). %can be constructed 
 
 
%%%%%%%%%%%%%
% split_list(List,HeadLen) -> {HeadList,TailList}
%
% splits List into HeadList (Length=HeadLen) and TailList
% if HeadLen == indefinite -> return {List,indefinite}
split_list(List,indefinite) ->
    {List, indefinite};
split_list(List,Len) ->
    {lists:sublist(List,Len),lists:nthtail(Len,List)}.

%%% left here for backwards compatibility 
restbytes(indefinite,[0,0|RemBytes],_) ->
    exit({error,{asn1, {"recompile with asn1-1.2.9.5 or later"}}});
restbytes(indefinite,RemBytes,ext) ->
    exit({error,{asn1, {"recompile with asn1-1.2.9.5 or later"}}});
restbytes(RemBytes,[],_) ->
    RemBytes;
restbytes(RemBytes,Bytes,noext) ->
    exit({error,{asn1, {unexpected,Bytes}}});
restbytes(RemBytes,Bytes,ext) ->
    RemBytes.

%%% new function which fixes a bug regarding indefinite length decoding
restbytes2(indefinite,[0,0|RemBytes],_) ->
    {RemBytes,2};
restbytes2(indefinite,RemBytes,ext) ->
    skipvalue(indefinite,RemBytes);
restbytes2(RemBytes,[],_) ->
    {RemBytes,0};
restbytes2(RemBytes,Bytes,noext) ->
    exit({error,{asn1, {unexpected,Bytes}}});
restbytes2(RemBytes,Bytes,ext) ->
    {RemBytes,0}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% skipvalue(Length, Bytes) -> {RemainingBytes, RemovedNumberOfBytes}
%%
%% skips the one complete (could be nested) TLV from Bytes
%% handles both definite and indefinite length encodings
%%
skipvalue(L, Bytes) ->
    skipvalue(L, Bytes, 0).

skipvalue(indefinite, Bytes, Rb) ->
    {T,Bytes2,R2} = decode_tag(Bytes),
    {L,Bytes3,R3} = decode_length(Bytes2),
    {Bytes4,Rb4} = case L of
		 indefinite ->
		     skipvalue(indefinite,Bytes3,R2+R3);
		 _ ->
		     lists:nthtail(L,Bytes3)
	     end,
    case Bytes4 of
	[0,0|Bytes5] ->
	    {Bytes5,Rb4+2};
	_  -> skipvalue(indefinite,Bytes4,Rb4)
    end;
skipvalue(L, Bytes, Rb) ->
    {lists:nthtail(L,Bytes),Rb+L}.
    
skipvalue(Bytes) ->
    {T,Bytes2,R2} = decode_tag(Bytes),
    {L,Bytes3,R3} = decode_length(Bytes2),
    skipvalue(L,Bytes3,R2+R3).
	

cindex(Ix,Val,Cname) -> 
    case element(Ix,Val) of 
	{Cname,Val2} -> Val2; 
	X -> X 
    end. 
 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
%% Optionals, preset not filled optionals with asn1_NOVALUE 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
 
% converts a list to a record if necessary 
list_to_record(Name,List) when list(List) -> 
    list_to_tuple([Name|List]); 
list_to_record(Name,Tuple) when tuple(Tuple) -> 
    Tuple. 
 
 
fixoptionals(OptList,Val) when list(Val) -> 
    fixoptionals(OptList,Val,1,[],[]). 
 
fixoptionals([{Name,Pos}|Ot],[{Name,Val}|Vt],Opt,Acc1,Acc2) -> 
    fixoptionals(Ot,Vt,Pos+1,[1|Acc1],[{Name,Val}|Acc2]); 
fixoptionals([{Name,Pos}|Ot],V,Pos,Acc1,Acc2) -> 
    fixoptionals(Ot,V,Pos+1,[0|Acc1],[asn1_NOVALUE|Acc2]); 
fixoptionals(O,[Vh|Vt],Pos,Acc1,Acc2) -> 
    fixoptionals(O,Vt,Pos+1,Acc1,[Vh|Acc2]); 
fixoptionals([],[Vh|Vt],Pos,Acc1,Acc2) -> 
    fixoptionals([],Vt,Pos+1,Acc1,[Vh|Acc2]); 
fixoptionals([],[],_,Acc1,Acc2) -> 
    % return Val as a record 
    list_to_tuple([asn1_RECORDNAME|lists:reverse(Acc2)]).
 
 
%%encode_tag(TagClass(?UNI, APP etc), Form (?PRIM etx), TagInteger) ->  
%%     8bit Int | [list of octets] 
encode_tag_val({Class, Form, TagNo}) when (TagNo =< 30) -> 
    [Class bor Form bor TagNo]; 
encode_tag_val({Class, Form, TagNo}) -> 
    {Octets,L} = mk_object_val(TagNo),
    [Class bor Form bor 31 | Octets]; 
 
%% asumes whole correct tag bitpattern, multiple of 8 
encode_tag_val(Tag) when (Tag =< 255) -> Tag; 
%% asumes correct bitpattern of 0-5 
encode_tag_val(Tag) -> encode_tag_val2(Tag,[]). 
 
encode_tag_val2(Tag, OctAck) when (Tag =< 255) -> 
    [Tag | OctAck]; 
encode_tag_val2(Tag, OctAck) -> 
    encode_tag_val2(Tag bsr 8, [255 band Tag | OctAck]). 
 
 
%%============================================================================\%% Peek on the initial tag
%% peek_tag(Bytes) -> TagBytes
%% interprets the first byte and possible  second, third and fourth byte as
%% a tag and returns all the bytes comprising the tag, the constructed/primitive bit (6:th bit of first byte) is normalised to 0
%%

peek_tag([Tag|Buffer]) when (Tag band 31) == 31 -> 
    [Tag band 2#11011111 | peek_tag(Buffer,[])];
%% single tag (tagno < 31)
peek_tag([Tag|Buffer]) ->
    [Tag band 2#11011111].

peek_tag([PartialTag|Buffer], TagAck) when (PartialTag < 128 ) ->
    lists:reverse([PartialTag|TagAck]);
peek_tag([PartialTag|Buffer], TagAck) ->
    peek_tag(Buffer,[PartialTag|TagAck]);
peek_tag(Buffer,TagAck) ->
    exit({error,{asn1, {invalid_tag,lists:reverse(TagAck)}}}). 
    
%%=============================================================================== 
%% Decode a tag 
%% 
%% decode_tag(OctetListBuffer) -> {{Class, Form, TagNo}, RestOfBuffer, RemovedBytes} 
%%=============================================================================== 
 
%% multiple octet tag 
decode_tag([Tag|Buffer]) when ( Tag band 31 ) == 31  -> 
    Class  = Tag band 2#11000000, 
    Form   = Tag band 2#00100000, 
    {TagNo, Buffer1, RemovedBytes} = decode_tag(Buffer, 0, 1), 
    {{Class, Form, TagNo}, Buffer1, RemovedBytes}; 
%% single tag (< 31 tags) 
decode_tag([Tag|Buffer]) -> 
    Class = Tag band 2#11000000, 
    Form  = Tag band 2#00100000, 
    TagNo = Tag band 2#00011111, 
    {{Class, Form, TagNo}, Buffer, 1}. 
     
%% last partial tag 
decode_tag([PartialTag|Buffer], TagAck, RemovedBytes) when (PartialTag band 128) == 0 -> 
    TagNo = (TagAck bsl 7) bor PartialTag, 
    {TagNo, Buffer, RemovedBytes+1}; 
% more tags 
decode_tag([PartialTag|Buffer], TagAck, RemovedBytes) -> 
    TagAck1 = (TagAck bsl 7) bor (PartialTag band 127), 
    decode_tag(Buffer, TagAck1, RemovedBytes+1). 
 
 
check_tags(Tags, Buffer) ->
    check_tags(Tags, Buffer, mandatory).

check_tags(Tags, Buffer, OptOrMand) ->
    check_tags(Tags, Buffer, 0, OptOrMand).

check_tags([Tag1,Tag2|TagRest], Buffer, Rb, OptOrMand) 
  when Tag1#tag.type == 'IMPLICIT' ->
    check_tags([Tag1#tag{type=Tag2#tag.type}|TagRest], Buffer, Rb, OptOrMand);

check_tags([Tag1|TagRest], Buffer, Rb, OptOrMand) ->
    {Form_Length,Buffer2,Rb1} = check_one_tag(Tag1, Buffer, OptOrMand),
    case TagRest of
	[] -> {Form_Length, Buffer2, Rb + Rb1};
	_ -> check_tags(TagRest, Buffer2, Rb + Rb1, mandatory)
    end;

check_tags([], Buffer, Rb, _) ->
    {{0,0},Buffer,Rb}.

check_one_tag(Tag, Buffer, OptOrMand) ->
    case catch decode_tag(Buffer) of
	{'EXIT',Reason} -> 
	    tag_error(no_data,Tag,Buffer,OptOrMand);
    {{Class,Form,TagNo},Buffer2,Rb} ->
	    Expected = {Tag#tag.class,Tag#tag.number},
	    case {Class,TagNo} of
		Expected ->
		    {{L,Buffer3},RemBytes2} = decode_length(Buffer2),
		    {{Form,L}, Buffer3, RemBytes2+Rb};
		ErrorTag ->
		    tag_error(ErrorTag, Tag, Buffer, OptOrMand)
	    end
    end.
	    
tag_error(ErrorTag, Tag, Buffer, OptOrMand) ->
    case OptOrMand of
	mandatory ->
	    exit({error,{asn1, {invalid_tag, 
				{ErrorTag, Tag, Buffer}}}});
	_ ->
	    exit({error,{asn1, {no_optional_tag, 
				{ErrorTag, Tag, Buffer}}}})
    end.    
%%=======================================================================
%%
%% Encode all tags in the list Tags and return a possibly deep list of
%% bytes with tag and length encoded
%%
%% prepend_tags(Tags, BytesSoFar, LenSoFar) -> {Bytes, Len}
encode_tags(Tags, BytesSoFar, LenSoFar) ->
    NewTags = encode_tags1(Tags, []), 
    %% NewTags contains the resulting tags in reverse order
    encode_tags2(NewTags, BytesSoFar, LenSoFar).

%encode_tags2([#tag{class=?UNIVERSAL,number=No}|Trest], BytesSoFar, LenSoFar) ->
%    {Bytes2,L2} = encode_length(LenSoFar),
%    encode_tags2(Trest,[[No|Bytes2],BytesSoFar], LenSoFar + 1 + L2);
encode_tags2([Tag|Trest], BytesSoFar, LenSoFar) ->
    {Bytes1,L1} = encode_one_tag(Tag),
    {Bytes2,L2} = encode_length(LenSoFar),
    encode_tags2(Trest, [Bytes1,Bytes2,BytesSoFar], LenSoFar + L1 + L2);
encode_tags2([], BytesSoFar, LenSoFar) ->
    {BytesSoFar,LenSoFar}.

encode_tags1([Tag1, Tag2| Trest], Acc) 
  when Tag1#tag.type == 'IMPLICIT' ->
    encode_tags1([Tag1#tag{type=Tag2#tag.type,form=Tag2#tag.form}|Trest],Acc);
encode_tags1([Tag1 | Trest], Acc) -> 
    encode_tags1(Trest, [Tag1|Acc]);
encode_tags1([], Acc) ->
    Acc. % the resulting tags are returned in reverse order
    
encode_one_tag(#tag{class=Class,number=No,type=Type, form = Form}) ->			  
    NewForm = case Type of
	       'EXPLICIT' ->
		   ?CONSTRUCTED;
	       _ ->
		   Form
	   end,
    Bytes = encode_tag_val({Class,NewForm,No}),
    {Bytes,length(Bytes)}.
	    
   
%%=============================================================================== 
%% Change the tag (used when an implicit tagged type has a reference to something else) 
%% The constructed bit in the tag is taken from the tag to be replaced.
%% 
%% change_tag(NewTag,[Tag,Buffer]) -> [NewTag,Buffer] 
%%=============================================================================== 
 
%change_tag({NewClass,NewTagNr}, Buffer) ->
%    {{OldClass, OldForm, OldTagNo}, Buffer1, RemovedBytes} = decode_tag(lists:flatten(Buffer)),
%    [encode_tag_val({NewClass, OldForm, NewTagNr}) | Buffer1].
    
  
 
 
 
 
 
%%=============================================================================== 
%% 
%% This comment is valid for all the encode/decode functions 
%% 
%% C = Constraint -> typically {'ValueRange',LowerBound,UpperBound} 
%%     used for PER-coding but not for BER-coding. 
%% 
%% Val = Value.  If Val is an atom then it is a symbolic integer value  
%%       (i.e the atom must be one of the names in the NamedNumberList). 
%%       The NamedNumberList is used to translate the atom to an integer value 
%%       before encoding. 
%% 
%%=============================================================================== 
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_open_type(Value) -> CompleteList
%% Value = list of bytes of an already encoded value (the list must be flat)
%%         | binary
%%
encode_open_type(Val) when list(Val) -> 
    {Val,length(Val)};
encode_open_type(Val) when binary(Val) ->
    {binary_to_list(Val), size(Val)}. 
%% binary_to_list is not optimal but compatible with the current solution


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_open_type(Buffer) -> Value
%% Bytes = [byte] with PER encoded data 
%% Value = [byte] with decoded data (which must be decoded again as some type)
%%
decode_open_type(Bytes) ->
    {Tag, Len, RemainingBuffer, RemovedBytes} = decode_tag_and_length(Bytes),
    case split_list(Bytes, Len + RemovedBytes) of
	{Val, indefinite} ->
	    %% not implemented yet
	    exit({error,{asn1, {indefinite_length_open_type,Bytes}}});
	{Val, RemainingBytes} ->
	    {Val, RemainingBytes, Len + RemovedBytes}
    end.
 
 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
%% Boolean, ITU_T X.690 Chapter 8.2 
%%=============================================================================== 
%%=============================================================================== 
%%=============================================================================== 
 
%%=============================================================================== 
%% encode_boolean(Integer, tag | notag) -> [octet list] 
%%=============================================================================== 
 
encode_boolean({Name, Val}, DoTag) when atom(Name) -> 
    dotag(DoTag, ?N_BOOLEAN, encode_boolean(Val)); 
encode_boolean(Val, DoTag) -> 
    dotag(DoTag, ?N_BOOLEAN, encode_boolean(Val)). 

%% encode_boolean(Boolean) -> [Len, Boolean] = [1, $FF | 0] 
encode_boolean(true)   ->    {[16#FF],1}; 
encode_boolean(false)  ->    {[0],1}; 
encode_boolean(X) -> exit({error,{asn1, {encode_boolean, X}}}). 
 
 
%%=============================================================================== 
%% decode_boolean(BuffList, HasTag, TotalLen) -> {true, Remain, RemovedBytes} |  
%%                                               {false, Remain, RemovedBytes} 
%%=============================================================================== 
 
decode_boolean(Buffer, HasTag, OptOrMand) ->
    {Buffer2,RemovedBytes} = 
	case HasTag of 
	    Tags when list(Tags) ->
		{{_,1},Buff,RemBytes} = 
		    check_tags(Tags ++ [#tag{class=?UNIVERSAL,number=?N_BOOLEAN}],
			       Buffer, OptOrMand),
		{Buff,RemBytes};
	    {notag,Form} ->
		{Buffer,0}
	end,
    decode_boolean2(Buffer2, RemovedBytes).
 
decode_boolean2([0 | Buffer], RemovedBytes) ->
    {false, Buffer, RemovedBytes + 1};
decode_boolean2([NotZero | Buffer], RemovedBytes) when NotZero =< 255 ->
    {true, Buffer, RemovedBytes + 1};
decode_boolean2(Buffer, RemovedBytes) ->  
    exit({error,{asn1, {decode_boolean, Buffer}}}). 
 
  

%%=========================================================================== 
%% Integer, ITU_T X.690 Chapter 8.3 

%% encode_integer(Constraint, Value, Tag) -> [octet list] 
%% encode_integer(Constraint, Name, NamedNumberList, Tag) -> [octet list] 
%%    Value = INTEGER | {Name,INTEGER}  
%%    Tag = tag | notag 
%%=========================================================================== 
 
encode_integer(C, Val, Tag) when integer(Val) -> 
    dotag(Tag, ?N_INTEGER, encode_integer(C, Val)); 
encode_integer(C,{Name,Val},Tag) when atom(Name) ->
    encode_integer(C,Val,Tag);
encode_integer(C, Val, Tag) -> 
    exit({error,{asn1, {encode_integer, Val}}}). 
 
 
 
encode_integer(C, Val, NamedNumberList, Tag) when atom(Val) -> 
    case lists:keysearch(Val, 1, NamedNumberList) of 
	{value,{_, NewVal}} ->  
	    dotag(Tag, ?N_INTEGER, encode_integer(C, NewVal)); 
	_ ->  
	    exit({error,{asn1, {encode_integer_namednumber, Val}}}) 
    end; 
encode_integer(C,{Name,Val},NamedNumberList,Val) ->
    encode_integer(C,Val,NamedNumberList,Val);
encode_integer(C, Val, NamedNumberList, Tag) -> 
    dotag(Tag, ?N_INTEGER, encode_integer(C, Val)). 
 
 
 
 
%%encode_integer( C, {Name, Val}, Nn) when atom(Name) -> 
%%    encode_integer(C, Val, Nn); 
 
%%encode_integer(C, Val, Nn) when atom(Val) -> 
%%    case lists:keysearch(1, Val, Nn) of 
%%	{value, {_, NewVal}} -> 
%%	    encode_integer(C, NewVal, []); 
%%	_ -> 
%%	    exit({error,{asn1,{'integer not in range', Val, Nn}}}) 
%%    end; 
 
 
encode_integer(C, Val) -> 
    Bytes =  
	if  
	    Val >= 0 -> 
		encode_integer_pos(Val, []); 
	    true -> 
		encode_integer_neg(Val, []) 
	end, 
    {Bytes,length(Bytes)}. 
 
encode_integer_pos(0, [B|Acc]) when B < 128 ->
    [B|Acc];
encode_integer_pos(N, Acc) ->
    encode_integer_pos(N bsr 8, [N band 16#ff| Acc]).

encode_integer_neg(-1, [B1|T]) when B1 > 127 ->
    [B1|T];
encode_integer_neg(N, Acc) ->
    encode_integer_neg(N bsr 8, [N band 16#ff|Acc]).

 
%%=============================================================================== 
%% decode integer  
%%    (Buffer, Range, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%=============================================================================== 
 
decode_integer(Buffer, Range, Tags, OptOrMand) -> 
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_INTEGER}),
    Result = {Val, Buffer2, RemovedBytes} = 
	decode_integer(Buffer,NewTags, OptOrMand),
    case Range of
	[] -> % No length constraint
	    Result;
	{Lb,Ub} when Val >= Lb, Ub >= Val -> % variable length constraint
	    Result;
	Val -> % fixed value constraint
	    Result;
	{Lb,Ub} -> 
	    exit({error,{asn1,{integer_range,Range,Val}}});
	SingleValue when integer(SingleValue) ->
	    exit({error,{asn1,{integer_range,Range,Val}}});
	_ -> % some strange constraint that we don't support yet
	    Result
    end.
 
decode_integer(Buffer, Range, NamedNumberList, Tags, OptOrMand) -> 
    NewTags = new_tags(Tags,#tag{class=?UNIVERSAL,number=?N_INTEGER}),
    {Val, Buffer2, RemovedBytes} = 
	decode_integer(Buffer, NewTags, OptOrMand),
    case Range of
	[] -> % No length constraint
	    ok;
	{Lb,Ub} when Val >= Lb, Ub >= Val -> % variable length constraint
	    ok;
	Val -> % fixed value constraint
	    ok;
	{Lb,Ub} -> 
	    exit({error,{asn1,{integer_range,Range,Val}}});
	SingleValue when integer(SingleValue) ->
	    exit({error,{asn1,{integer_range,Range,Val}}});
	_ -> % some strange constraint that we don't support yet
	    ok
    end,  
    %% if its an named integer - return it 
    NewVal = case lists:keysearch(Val, 2, NamedNumberList) of 
		 {value,{NamedVal, _}} -> 
		     NamedVal; 
		 _ -> 
		     Val 
	     end, 
    {NewVal, Buffer2, RemovedBytes}. 
 

%%============================================================================ 
%% Enumerated value, ITU_T X.690 Chapter 8.4 

%% encode enumerated value 
%%============================================================================ 
encode_enumerated(Val, DoTag) when integer(Val) ->
    dotag(DoTag, ?N_ENUMERATED, encode_integer(false,Val));
encode_enumerated({Name,Val}, DoTag) when atom(Name)->
    encode_enumerated(Val, DoTag).

%% The encode_enumerated functions below this line can be removed when the
%% new code generation is stable. (the functions might have to be kept here
%% a while longer for compatibility reasons)

encode_enumerated(C, Val, {NamedNumberList,ExtList}, DoTag) when atom(Val) ->
    case catch encode_enumerated(C, Val, NamedNumberList, DoTag) of
	{'EXIT',_} -> encode_enumerated(C, Val, ExtList, DoTag);
	Result -> Result
    end;
 
encode_enumerated(C, Val, NamedNumberList, DoTag) when atom(Val) -> 
    case lists:keysearch(Val, 1, NamedNumberList) of 
	{value, {_, NewVal}} -> 
	    dotag(DoTag, ?N_ENUMERATED, encode_integer(C, NewVal)); 
	_ -> 
	    exit({error,{asn1, {enumerated_not_in_range, Val}}}) 
    end; 

encode_enumerated(C, {asn1_enum, Val}, {_,_}, DoTag) when integer(Val) ->
    dotag(DoTag, ?N_ENUMERATED, encode_integer(C,Val)); 

encode_enumerated(C, {Name,Val}, NamedNumberList, DoTag) when atom(Name) -> 
    encode_enumerated(C, Val, NamedNumberList, DoTag);

encode_enumerated(C, Val, NamedNumberList, DoTag) -> 
    exit({error,{asn1, {enumerated_not_namednumber, Val}}}). 
 
 
 
%%============================================================================ 
%% decode enumerated value 
%%   (Buffer, Range, NamedNumberList, HasTag, TotalLen) ->  
%%                                    {Value, RemainingBuffer, RemovedBytes} 
%%===========================================================================
decode_enumerated(Buffer, Range, {NamedNumberList,ExtList}, Tags, OptOrMand) ->
    {Val, Buffer2, RemovedBytes} = 
	decode_integer(Buffer, Tags ++
		       [#tag{class=?UNIVERSAL,number=?N_ENUMERATED}], 
		       OptOrMand), 
    case decode_enumerated1(Val, NamedNumberList) of
	{asn1_enum,Val} ->
	    {decode_enumerated1(Val,ExtList), Buffer2, RemovedBytes};
	Result ->
	    {Result, Buffer2, RemovedBytes}
    end;
 
decode_enumerated(Buffer, Range, NamedNumberList, Tags, OptOrMand) -> 
    {Val, Buffer2, RemovedBytes} = 
	decode_integer(Buffer, Tags ++
		       [#tag{class=?UNIVERSAL,number=?N_ENUMERATED}], 
		       OptOrMand), 
    case decode_enumerated1(Val, NamedNumberList) of
	{asn1_enum,_} ->
	    exit({error,{asn1, {illegal_enumerated, Val}}});
	Result ->
	    {Result, Buffer2, RemovedBytes}
    end.

decode_enumerated1(Val, NamedNumberList) ->     
    %% it must be a named integer 
    case lists:keysearch(Val, 2, NamedNumberList) of 
	{value,{NamedVal, _}} -> 
	    NamedVal; 
	_ -> 
	    {asn1_enum,Val}
    end.
 

%%============================================================================ 
%%
%% Real value, ITU_T X.690 Chapter 8.5 
%%============================================================================ 
%%
%% encode real value 
%%============================================================================ 
 
%% only base 2 internally so far!! 
encode_real(0, DoTag) ->			  
    dotag(DoTag, ?N_REAL, {[],0}); 
encode_real('PLUS-INFINITY', DoTag) ->			  
    dotag(DoTag, ?N_REAL, {[64],1}); 
encode_real('MINUS-INFINITY', DoTag) -> 
    dotag(DoTag, ?N_REAL, {[65],1}); 
encode_real(Val, DoTag) when tuple(Val)-> 
    dotag(DoTag, ?N_REAL, encode_real(Val)). 
 
%%%%%%%%%%%%%% 
%% not optimal efficient..  
%% only base 2 of Mantissa encoding! 
%% only base 2 of ExpBase encoding! 
encode_real({Man, Base, Exp}) -> 
%    io:format("Mantissa: ~w Base: ~w, Exp: ~w~n",[Man, Base, Exp]), 
     
    OctExp = if Exp >= 0 -> encode_integer_pos(Exp, []); 
		true     -> encode_integer_neg(Exp, []) 
	     end, 
%    ok = io:format("OctExp: ~w~n",[OctExp]), 
    SignBitMask = if  Man > 0 -> 2#00000000;  % bit 7 is pos or neg, no Zeroval 
		      true    -> 2#01000000 
		  end, 
%    ok = io:format("SignBitMask: ~w~n",[SignBitMask]), 
    InternalBaseMask = if  Base =:= 2 -> 2#00000000;   % bit 6,5: only base 2 this far! 
			   true -> 
			       exit({error,{asn1, {encode_real_non_supported_encodeing, Base}}}) 
		       end, 
    ScalingFactorMask =2#00000000,   % bit 4,3: no scaling since only base 2 
    OctExpLen = length(OctExp), 
    if OctExpLen > 255  -> 
	    exit({error,{asn1, {to_big_exp_in_encode_real, OctExpLen}}}); 
       true  -> true %% make real assert later.. 
    end,  
    {LenMask, EOctets} = case OctExpLen of   % bit 2,1  
			     1 -> {0, OctExp}; 
			     2 -> {1, OctExp}; 
			     3 -> {2, OctExp}; 
			     _ -> {3, [OctExpLen, OctExp]}  
			 end, 
    FirstOctet = (SignBitMask bor InternalBaseMask bor 
		  ScalingFactorMask bor LenMask bor 
		  2#10000000), % bit set for binary mantissa encoding! 
    OctMantissa = if Man > 0 -> minimum_octets(Man); 
		     true    -> minimum_octets(-(Man)) % signbit keeps track of sign 
		  end, 
%    ok = io:format("LenMask: ~w EOctets: ~w~nFirstOctet: ~w OctMantissa: ~w OctExpLen: ~w~n", [LenMask, EOctets, FirstOctet, OctMantissa, OctExpLen]), 
     {[FirstOctet, EOctets, OctMantissa],
      length(OctMantissa) + 
      (if OctExpLen > 3 -> 
	       OctExpLen + 2; 
	  true -> 
	       OctExpLen + 1 
       end) 
     }. 
 
 
 
%%============================================================================ 
%% decode real value 
%% 
%% decode_real([OctetBufferList], tuple|value, tag|notag) -> 
%%  {{Mantissa, Base, Exp} | realval | PLUS-INFINITY | MINUS-INFINITY | 0, 
%%     RestBuff} 
%%  
%% only for base 2 decoding sofar!! 
%%============================================================================ 
 
decode_real(Buffer, Form , Tags, OptOrMand) -> 
    {{_,Len}, [First | Buffer2], RemBytes1} = 
	check_tags(Tags ++ [#tag{class=?UNIVERSAL,number=?N_REAL}], 
		   Buffer, OptOrMand), 
    if 
	First =:= 2#01000000 -> {'PLUS-INFINITY', Buffer2}; 
	First =:= 2#01000001 -> {'MINUS-INFINITY', Buffer2}; 
	First =:= 2#00000000 -> {0, Buffer2}; 
	true -> 
	    %% have some check here to verify only supported bases (2) 
	    Sign = 
		if  % bit 7 
		    (First band 2#01000000) =:= 0 -> 0;           % plus 
		    true                          -> 2#01000000   % minus 
		end, 
	    Base = 
		case First band 2#00110000 of  % bit 6,5 
		    0 -> 2;  % base 2, only one so far 
		    _ -> exit({error,{asn1, {non_supported_base, First}}}) 
		end, 
	    ScalingFactor = 
		case First band 2#00001100 of % bit 4,3 
		    0 -> 0;  % no scaling so far  
		    _ -> exit({error,{asn1, {non_supported_scaling, First}}}) 
		end, 
%	    ok = io:format("Buffer2: ~w~n",[Buffer2]), 
	    {FirstLen, {Exp, Buffer3}, RemBytes2} = 
		case First band 2#00000011 of % bit 2,1 
		    0 -> {2, decode_integer2(1, Buffer2, 0, RemBytes1), RemBytes1+1}; 
		    1 -> {3, decode_integer2(2, Buffer2, 0, RemBytes1), RemBytes1+2}; 
		    2 -> {4, decode_integer2(3, Buffer2, 0, RemBytes1), RemBytes1+3}; 
		    3 -> 
			ExpLen1 = hd(Buffer2), 
			{ ExpLen1 + 2, 
			 decode_integer2(ExpLen1, tl(Buffer2), 0, RemBytes1), RemBytes1+ExpLen1} 
		end, 
%	    io:format("FirstLen: ~w, Exp: ~w, Buffer3: ~w ~n",[FirstLen, Exp, Buffer3]), 
	    {{Mantissa, Buffer4}, RemBytes3} = 
		if Sign =:= 0 -> 
%			io:format("sign plus~n"), 
			dec_long_length(Len - FirstLen, Buffer3, 0, 1); 
		   true -> 
%			io:format("sign minus~n"), 
			-(dec_long_length(Len - FirstLen, Buffer3, 0, 1)) 
		end, 
%	    io:format("Form: ~w~n",[Form]), 
	    case Form of 
		tuple -> 
		    {Val,Buf,RemB} = Exp, 
		    {{Mantissa, Base, {Val,Buf}}, Buffer4, RemBytes2+RemBytes3};  
		_value -> 
		    comming 
	    end 
    end. 
 
%%============================================================================ 
%% Bitstring value, ITU_T X.690 Chapter 8.6 
%%
%% encode bitstring value 
%% 
%% bitstring NamedBitList 
%% Val can be  of: 
%% - [identifiers] where only named identifers are set to one,  
%%   the Constraint must then have some information of the  
%%   bitlength. 
%% - [list of ones and zeroes] all bits  
%% - integer value representing the bitlist 
%% C is constrint Len, only valid when identifiers 
%%============================================================================ 
 
encode_bit_string(C,Bin={Unused,BinBits},NamedBitList,DoTag) when integer(Unused), binary(BinBits) ->
    encode_bin_bit_string(C,Bin,NamedBitList,DoTag);
encode_bit_string(C, BitList=[FirstVal | RestVal], NamedBitList, DoTag) when atom(FirstVal) -> 
    encode_bit_string_named(C, BitList, NamedBitList, DoTag); 

encode_bit_string(C, BitList=[{bit,X} | RestVal], NamedBitList, DoTag) -> 
    encode_bit_string_named(C, BitList, NamedBitList, DoTag); 
 
encode_bit_string(C, BitList=[FirstVal| RestVal], NamedBitList, DoTag) when integer(FirstVal) -> 
    encode_bit_string_bits(C, BitList, NamedBitList, DoTag); 
 
encode_bit_string(C, 0, NamedBitList, DoTag) -> 
    dotag(DoTag, ?N_BIT_STRING, {[0],1}); 

encode_bit_string(C, [], NamedBitList, DoTag) -> 
    dotag(DoTag, ?N_BIT_STRING, {[0],1}); 

encode_bit_string(C, IntegerVal, NamedBitList, DoTag) when integer(IntegerVal) -> 
    BitListVal = int_to_bitlist(IntegerVal),
    encode_bit_string_bits(C, BitListVal, NamedBitList, DoTag);

encode_bit_string(C, {Name,BitList}, NamedBitList, DoTag) when atom(Name) ->
    encode_bit_string(C, BitList, NamedBitList, DoTag).
  
 
 
int_to_bitlist(0) ->
    [];
int_to_bitlist(Int) when integer(Int), Int >= 0 ->
    [Int band 1 | int_to_bitlist(Int bsr 1)].
 
%%================================================================= 
%% Encode named bits 
%%================================================================= 
 
encode_bit_string_named(C, [FirstVal | RestVal], NamedBitList, DoTag) -> 
    case get_constraint(C,'SizeConstraint') of 
 
	no -> 
	    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []), 
	    BitList = make_and_set_list(lists:max(ToSetPos)+1, ToSetPos, 0), 
	    {Len, Unused, OctetList} = encode_bitstring(BitList),  
	    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1});  
 
	{Min,Max} -> 
	    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []), 
	    BitList = make_and_set_list(Max, ToSetPos, 0), 
	    {Len, Unused, OctetList} = encode_bitstring(BitList),  
	    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1});  
	Size -> 
	    ToSetPos = get_all_bitposes([FirstVal | RestVal], NamedBitList, []), 
	    BitList = make_and_set_list(Size, ToSetPos, 0), 
	    {Len, Unused, OctetList} = encode_bitstring(BitList),  
	    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1})  
 
    end. 
	 
%%---------------------------------------- 
%% get_all_bitposes([list of named bits to set], named_bit_db, []) -> 
%%   [sorted_list_of_bitpositions_to_set] 
%%---------------------------------------- 

get_all_bitposes([{bit,ValPos}|Rest], NamedBitList, Ack) ->
    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack ]);
get_all_bitposes([Val | Rest], NamedBitList, Ack) when atom(Val) -> 
    case lists:keysearch(Val, 1, NamedBitList) of 
	{value, {_ValName, ValPos}} -> 
	    get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]); 
	_ -> 
	    exit({error,{asn1, {bitstring_namedbit, Val}}}) 
    end; 
get_all_bitposes([], _NamedBitList, Ack) -> 
    lists:sort(Ack). 
 
 
%%---------------------------------------- 
%% make_and_set_list(Len of list to return, [list of positions to set to 1])-> 
%% returns list of Len length, with all in SetPos set. 
%% in positioning in list the first element is 0, the second 1 etc.., but 
%% Len will make a list of length Len, not Len + 1. 
%%    BitList = make_and_set_list(C, ToSetPos, 0), 
%%---------------------------------------- 
 
make_and_set_list(0, [], _) -> []; 
make_and_set_list(0, _, _) ->  
    exit({error,{asn1,bitstring_sizeconstraint}}); 
make_and_set_list(Len, [XPos|SetPos], XPos) -> 
    [1 | make_and_set_list(Len - 1, SetPos, XPos + 1)]; 
make_and_set_list(Len, [Pos|SetPos], XPos) -> 
    [0 | make_and_set_list(Len - 1, [Pos | SetPos], XPos + 1)]; 
make_and_set_list(Len, [], XPos) -> 
    [0 | make_and_set_list(Len - 1, [], XPos + 1)]. 
 
 
 
 
%%================================================================= 
%% Encode bit string for tuple {unused bits, binary}
%%================================================================= 
encode_bin_bit_string(C,BitS={Unused,BinBits},NamedBitList,DoTag) ->
    case get_constraint(C,'SizeConstraint') of
	no ->
	    remove_unused_then_dotag(DoTag,?N_BIT_STRING,
				     Unused,BinBits);
% 	    case Unused of
% 		0 ->
% 		    dotag(DoTag,?N_BIT_STRING,
% 			  {[Unused,binary_to_list(BinBits)],1+size(BinBits)});
% 		Num ->
% 		    N = (size(BinBits)-1),
% 		    N1 = 8 - Num,
% 		    <<BBits:N/binary,LastByte:N1,_:Num>> = BinBits,
% 		    dotag(DoTag,?N_BIT_STRING,
% 			  {[Unused,binary_to_list(BBits)++[LastByte bsl Num]],1+size(BinBits)})
% 	    end;
	{Min,Max} ->
	    BBLen = (size(BinBits)*8)-Unused,
	    if  
		BBLen > Max -> 
		    exit({error,{asn1,{bitstring_length,{{was,BBLen},{maximum,Max}}}}});
		true ->
		    remove_unused_then_dotag(DoTag,?N_BIT_STRING,
					     Unused,BinBits)
%		    dotag(DoTag,?N_BIT_STRING,
%			  {[Unused,binary_to_list(BinBits)],size(BinBits)+1})
	    end;
	Size ->
	    case ((size(BinBits)*8)-Unused) of
		BBSize when BBSize =< Size ->
		    remove_unused_then_dotag(DoTag,?N_BIT_STRING,
					     Unused,BinBits);
%		    dotag(DoTag, ?N_BIT_STRING,
%			  {[Unused,binary_to_list(BinBits)],size(BinBits)+1});
		BBSize ->
		    exit({error,{asn1,  
				 {bitstring_length, 
				  {{was,BBSize},{should_be,Size}}}}})
	    end
    end.
	    
remove_unused_then_dotag(DoTag,StringType,Unused,BinBits) ->
    case Unused of
	0 when (size(BinBits) == 0) ->
	    dotag(DoTag,StringType,{[0],1});
	0 ->
	    dotag(DoTag,StringType,{[Unused,binary_to_list(BinBits)],
				    size(BinBits) + 1});
	Num ->
	    N = (size(BinBits)-1),
	    <<BBits:N/binary,LastByte>> = BinBits,
	    dotag(DoTag,StringType,
		  {[Unused,binary_to_list(BBits)++
		    [(LastByte bsr Num) bsl Num]],1+size(BinBits)})
    end.


%%================================================================= 
%% Encode bit string for lists of ones and zeroes 
%%================================================================= 
encode_bit_string_bits(C, BitListVal, NamedBitList, DoTag) when list(BitListVal) -> 
    case get_constraint(C,'SizeConstraint') of 
	no -> 
	    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
	    %%add unused byte to the Len 
	    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1});
	{Min,Max} -> 
	    BitLen = length(BitListVal), 
	    if  
		BitLen > Max -> 
		    exit({error,{asn1, {bitstring_length, {{was,BitLen},{maximum,Max}}}}}); 
		true -> 
		    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
		    %%add unused byte to the Len 
		    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1})  
	    end; 
 
	Size ->
	    case length(BitListVal) of
		BitSize when BitSize =< Size ->
		    {Len, Unused, OctetList} = encode_bitstring(BitListVal),  
		    %%add unused byte to the Len 
		    dotag(DoTag, ?N_BIT_STRING, {[Unused, OctetList],Len+1});  
		BitSize -> 
		    exit({error,{asn1,  
			  {bitstring_length, {{was,BitSize},{should_be,Size}}}}}) 
	    end 
 
    end. 
 
 
%%================================================================= 
%% Do the actual encoding 
%%     ([bitlist]) -> {ListLen, UnusedBits, OctetList} 
%%================================================================= 
encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest]) -> 
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor 
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1, 
    encode_bitstring(Rest, [Val], 1); 
encode_bitstring(Val) -> 
    {Unused, Octet} = unused_bitlist(Val, 7, 0), 
    {1, Unused, Octet}. 
 
encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest], Ack, Len) -> 
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor 
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1, 
    encode_bitstring(Rest, [Ack | [Val]], Len + 1); 
%%even multiple of 8 bits.. 
encode_bitstring([], Ack, Len) -> 
    {Len, 0, Ack}; 
%% unused bits in last octet 
encode_bitstring(Rest, Ack, Len) -> 
%    io:format("uneven ~w ~w ~w~n",[Rest, Ack, Len]), 
    {Unused, Val} = unused_bitlist(Rest, 7, 0), 
    {Len + 1, Unused, [Ack | [Val]]}. 
 
%%%%%%%%%%%%%%%%%% 
%% unused_bitlist([list of ones and zeros <= 7], 7, []) -> 
%%  {Unused bits, Last octet with bits moved to right} 
unused_bitlist([], Trail, Ack) -> 
    {Trail + 1, Ack}; 
unused_bitlist([Bit | Rest], Trail, Ack) -> 
%%    io:format("trail Bit: ~w Rest: ~w Trail: ~w Ack:~w~n",[Bit, Rest, Trail, Ack]), 
    unused_bitlist(Rest, Trail - 1, (Bit bsl Trail) bor Ack). 
 
 
%%============================================================================ 
%% decode bitstring value 
%%    (Buffer, Range, NamedNumberList, HasTag, TotalLen) -> {Integer, Remain, RemovedBytes}  
%%============================================================================ 
 
decode_bit_string(Buffer,Range,NamedNumberList,HasTag,LenIn,OptOrMand) -> 
    decode_restricted_string(Buffer, Range, ?N_BIT_STRING, HasTag, LenIn, NamedNumberList, OptOrMand,oldformat). 
 
decode_compact_bit_string(Buffer,Range,NamedNumberList,HasTag,LenIn,OptOrMand)->
    decode_restricted_string(Buffer,Range,?N_BIT_STRING,HasTag,LenIn, 
			     NamedNumberList,OptOrMand,binformat).
 
 
 
 
decode_bit_string2(1,[0 | Buffer],NamedNumberList,RemovedBytes,BinOrOld) -> 
    case BinOrOld of
	binformat ->
	    {{0,<<>>}, Buffer, RemovedBytes}; 
	_ ->
	    {[], Buffer, RemovedBytes}
    end;
decode_bit_string2(Len, [Unused | Buffer], NamedNumberList, RemovedBytes,
		  BinOrOld) -> 
    
    case NamedNumberList of 
	[] -> 
	    case BinOrOld of
		binformat ->
		    {{Unused,list_to_binary(lists:sublist(Buffer,Len-1))},
		     lists:nthtail(Len-1, Buffer), RemovedBytes};
		_ ->
		    BitString = decode_bitstring2(Len - 1, Unused, Buffer), 
		    {BitString, lists:nthtail(Len-1, Buffer), RemovedBytes}
	    end;
	_ ->
	    BitString = decode_bitstring2(Len - 1, Unused, Buffer), 
	    {decode_bitstring_NNL(BitString,NamedNumberList), 
	     lists:nthtail(Len-1, Buffer),  
	     RemovedBytes} 
    end. 
 
%%---------------------------------------- 
%% Decode the in buffer to bits 
%%---------------------------------------- 
decode_bitstring2(1, Unused, Buffer) -> 
    make_bits_of_int(hd(Buffer), 128, 8-Unused); 
decode_bitstring2(Len, Unused, [BitVal | Buffer]) -> 
    [B7, B6, B5, B4, B3, B2, B1, B0] = make_bits_of_int(BitVal, 128, 8), 
    [B7, B6, B5, B4, B3, B2, B1, B0 | 
     decode_bitstring2(Len - 1, Unused, Buffer)]. 
 
 
make_bits_of_int(_, _, 0) -> 
    []; 
make_bits_of_int(BitVal, MaskVal, Unused) when Unused > 0 -> 
    X = case MaskVal band BitVal of 
	    0 -> 0 ; 
	    _ -> 1 
	end, 
    [X | make_bits_of_int(BitVal, MaskVal bsr 1, Unused - 1)]. 
 
 
 
%%---------------------------------------- 
%% Decode the bitlist to names 
%%---------------------------------------- 
decode_bitstring_NNL(BitList,NamedNumberList) -> 
    decode_bitstring_NNL(BitList,NamedNumberList,0,[]). 
 
 
decode_bitstring_NNL([],_,No,Result) -> 
    lists:reverse(Result); 

decode_bitstring_NNL([B|BitList],[{Name,No}|NamedNumberList],No,Result) -> 
    if 
	B == 0 -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result); 
	true -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[Name|Result]) 
    end; 
decode_bitstring_NNL([1|BitList],NamedNumberList,No,Result) -> 
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,[{bit,No}|Result]); 
decode_bitstring_NNL([0|BitList],NamedNumberList,No,Result) ->
	    decode_bitstring_NNL(BitList,NamedNumberList,No+1,Result). 
 
 
%%============================================================================ 
%% Octet string, ITU_T X.690 Chapter 8.7 
%%
%% encode octet string 
%% The OctetList must be a flat list of integers in the range 0..255
%% the function does not check this because it takes to much time
%%============================================================================ 
encode_octet_string(C, OctetList, DoTag) when list(OctetList) -> 
    dotag(DoTag, ?N_OCTET_STRING, {OctetList,length(OctetList)});
encode_octet_string(C, {Name,OctetList}, DoTag) when atom(Name) -> 
    encode_octet_string(C, OctetList, DoTag).
 
%%============================================================================ 
%% decode octet string  
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%% 
%% Octet string is decoded as a restricted string 
%%============================================================================ 
decode_octet_string(Buffer, Range, HasTag, TotalLen, OptOrMand) -> 
    decode_restricted_string(Buffer, Range, ?N_OCTET_STRING, 
			     HasTag, TotalLen, OptOrMand). 

%%============================================================================ 
%% Null value, ITU_T X.690 Chapter 8.8 
%%
%% encode NULL value 
%%============================================================================ 
 
encode_null({Name, Val}, DoTag) when atom(Name) -> 
    dotag(DoTag, ?N_NULL, {[],0});
encode_null(V, DoTag) -> 
    dotag(DoTag, ?N_NULL, {[],0}). 
 
%%============================================================================ 
%% decode NULL value 
%%    (Buffer, HasTag, TotalLen) -> {NULL, Remain, RemovedBytes}  
%%============================================================================ 
decode_null(Buffer, Tags, OptOrMand) -> 
    {{_,0}, Buffer1, RemovedBytes} = 
	check_tags(Tags ++ [#tag{class=?UNIVERSAL,number=?N_NULL}], 
		   Buffer, OptOrMand), 
    {'NULL', Buffer1, RemovedBytes}. 
 
 
%%============================================================================ 
%% Object identifier, ITU_T X.690 Chapter 8.19 
%%
%% encode Object Identifier value 
%%============================================================================ 
 
encode_object_identifier({Name,Val}, DoTag) when atom(Name) -> 
    encode_object_identifier(Val, DoTag);
encode_object_identifier(Val, DoTag) -> 
    dotag(DoTag, ?N_OBJECT_IDENTIFIER, e_object_identifier(Val)).
 
e_object_identifier({'OBJECT IDENTIFIER', V}) -> 
    e_object_identifier(V); 
e_object_identifier({Cname, V}) when atom(Cname), tuple(V) -> 
    e_object_identifier(tuple_to_list(V)); 
e_object_identifier({Cname, V}) when atom(Cname), list(V) -> 
    e_object_identifier(V); 
e_object_identifier(V) when tuple(V) -> 
    e_object_identifier(tuple_to_list(V)); 
 
%%%%%%%%%%%%%%% 
%% e_object_identifier([List of Obect Identifiers]) -> 
%% {[Encoded Octetlist of ObjIds], IntLength} 
%% 
e_object_identifier([E1, E2 | Tail]) -> 
    Head = 40*E1 + E2,  % wow! 
    F = fun(Val, AckLen) -> 
		{L, Ack} = mk_object_val(Val), 
		{L, Ack + AckLen} 
	end, 
    {Octets, Len} = lists:mapfoldl(F, 0, [Head | Tail]). 
 
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
 
 
 
%%============================================================================ 
%% decode Object Identifier value   
%%    (Buffer, HasTag, TotalLen) -> {{ObjId}, Remain, RemovedBytes}  
%%============================================================================ 
 
decode_object_identifier(Buffer, Tags, OptOrMand) -> 
    {{_,Len}, Buffer2, RemovedBytes} = 
	check_tags(Tags ++ [#tag{class=?UNIVERSAL,
				 number=?N_OBJECT_IDENTIFIER}],
		   Buffer, OptOrMand), 
    {[AddedObjVal|ObjVals],Buffer4} = dec_subidentifiers(Buffer2,0,[],Len), 
    {Val1, Val2} = if 
		       AddedObjVal < 40 -> 
			   {0, AddedObjVal}; 
		       AddedObjVal < 80 -> 
			   {1, AddedObjVal - 40}; 
		       true -> 
			   {2, AddedObjVal - 80} 
		   end, 
    {list_to_tuple([Val1, Val2 | ObjVals]), Buffer4, RemovedBytes+Len}. 
 
dec_subidentifiers(Buffer,Av,Al,0) -> 
    {lists:reverse(Al),Buffer}; 
dec_subidentifiers([H|T],Av,Al,Len) when H >=16#80 -> 
    dec_subidentifiers(T,(Av bsl 7) + (H band 16#7F),Al,Len-1); 
dec_subidentifiers([H|T],Av,Al,Len) -> 
    dec_subidentifiers(T,0,[(Av bsl 7) + H |Al],Len-1). 
 
 
%%============================================================================ 
%% Restricted character string types, ITU_T X.690 Chapter 8.20 
%%
%% encode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings 
%%============================================================================ 
encode_restricted_string(C,{Name,OctetL},StringType,DoTag) when atom(Name)-> 
    encode_restricted_string(C, OctetL, StringType, DoTag);
encode_restricted_string(C, OctetList, StringType, DoTag) -> 
    dotag(DoTag, StringType, {OctetList, length(OctetList)}). 
 
%%============================================================================ 
%% decode Numeric Printable Teletex Videotex Visible IA5 Graphic General strings 
%%    (Buffer, Range, StringType, HasTag, TotalLen) -> 
%%                                  {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_restricted_string(Buffer,Range,StringType,TagsIn,LenIn,
			 OptOrMand) -> 
    decode_restricted_string(Buffer,Range,StringType,TagsIn,LenIn,
			     no_named_number_list,OptOrMand,oldformat). 
 
decode_restricted_string(Buffer,Range,StringType,TagsIn,
			 LenIn,NamedNumberList,OptOrMand,BinOrOld) -> 
    %%----------------------------------------------------------- 
    %% Get inner (the implicit tag or no tag) and  
    %%     outer (the explicit tag) lengths. 
    %%----------------------------------------------------------- 
    Tags = TagsIn ++ [#tag{class=?UNIVERSAL,number=StringType}],
    {{Form,InnerLen}, Buffer2, Rb} = check_tags(Tags, Buffer, OptOrMand),
    OuterLen = LenIn, 
    Result = {Str,Buffer3,Rbtot} = 
	decode_restricted(Buffer2,OuterLen,InnerLen,Form, 
			  StringType,NamedNumberList,BinOrOld,{[],Rb}),
    StrLen = case InnerLen of
		 indefinite -> 
		     case string_type(Str) of
			 atom_list -> length(NamedNumberList);
			 number_list -> length(Str);
			 compact_bit_string ->
			     {Unused,Bin} = Str,
			     (size(Bin)*8) - Unused
		     end;
		 _ when StringType /= ?N_BIT_STRING -> InnerLen;
		 _ when list(Str), StringType == ?N_BIT_STRING  -> 
		     case string_type(Str) of
			 atom_list -> length(NamedNumberList);
			 number_list -> length(Str);
			 compact_bit_string -> % obsolete case
			     {Unused,Bin} = Str,
			     (size(Bin)*8) - Unused
		     end;
		 _ when tuple(Str), StringType == ?N_BIT_STRING -> 
		     (size(element(2,Str))*8) - element(1,Str)
	     end,
    case Range of
	[] -> % No length constraint
	    Result;
	{Lb,Ub} when StrLen >= Lb, Ub >= StrLen -> % variable length constraint
	    Result;
	StrLen -> % fixed length constraint
	    Result;
	{Lb,Ub} -> 
	    exit({error,{asn1,{length,Range,Str}}});
	Len when integer(Len) ->
	    exit({error,{asn1,{length,Range,Str}}});
	_ -> % some strange constraint that we don't support yet
	    Result
    end. 
 
string_type(L) when list(L) -> 
    case lists:all(fun(X)-> case X of
				A when atom(A)-> true;
				_ -> false
			    end
		   end,
		   L) of
	true -> atom_list;
	false -> number_list
    end;
string_type({Int,Bin}) when integer(Int),binary(Bin) ->
    compact_bit_string;
string_type(Other) ->
    unknown.

decode_restricted(Buffer,OuterLen,InnerLen,Form,StringType,
		  NamedNumberList,BinOrOld,{ResultSoFar,RbSoFar}) -> 
    
    {NewRes, NewBuffer, NewRb} =  
	case {InnerLen, Form, StringType} of 
	    {InnerLen, ?PRIMITIVE, ?N_BIT_STRING} when integer(InnerLen) ->  
		decode_bit_string2(InnerLen,Buffer,NamedNumberList,
				   InnerLen,BinOrOld); 
 
	    {indefinite, ?CONSTRUCTED, ?N_UniversalString} ->  
		{Result,Buffer3,Rb} =  
		    decode_constr_indefinite(Buffer,StringType,
					     NamedNumberList,BinOrOld,0,[]), 
		{mk_universal_string(Result), Buffer3, Rb}; 
	    {InnerLen, ?PRIMITIVE, ?N_UniversalString} when integer(InnerLen) ->  
		UniString = mk_universal_string(lists:sublist(Buffer, InnerLen)), 
		{UniString,lists:nthtail(InnerLen, Buffer),InnerLen}; 
	    {InnerLen, ?CONSTRUCTED, ?N_UniversalString} when integer(InnerLen) ->  
		{Result,Buffer1} =  
		    decode_constr_definite(Buffer,StringType,NamedNumberList,
					   BinOrOld,InnerLen, []), 
		{mk_universal_string(Result), Buffer1, InnerLen}; 
 
	    {indefinite, ?CONSTRUCTED, ?N_BMPString} ->  
		{Result,Buffer3,Rb} =  
		    decode_constr_indefinite(Buffer,StringType,
					     NamedNumberList,BinOrOld,
					     0, []), 
		{mk_BMP_string(Result), Buffer3, Rb}; 
	    {InnerLen, ?PRIMITIVE, ?N_BMPString} when integer(InnerLen) ->  
		BMP = mk_BMP_string(lists:sublist(Buffer, InnerLen)), 
		{BMP,lists:nthtail(InnerLen, Buffer),InnerLen}; 
	    {InnerLen, ?CONSTRUCTED, ?N_BMPString} when integer(InnerLen) ->  
		{Result,Buffer1} =  
		    decode_constr_definite(Buffer,StringType,NamedNumberList,
					   BinOrOld,InnerLen, []), 
		{mk_BMP_string(Result), Buffer1, InnerLen}; 
 
	    {indefinite, ?CONSTRUCTED, _} ->  
		decode_constr_indefinite(Buffer,StringType,NamedNumberList,
					 BinOrOld,0,[]); 
	    {InnerLen, ?PRIMITIVE, _} when integer(InnerLen) ->  
		{lists:sublist(Buffer, InnerLen),lists:nthtail(InnerLen, Buffer),InnerLen}; 
	    {InnerLen, ?CONSTRUCTED, _} when integer(InnerLen) ->  
		{Result,Buffer1} =  
		    decode_constr_definite(Buffer,StringType,NamedNumberList,
					   BinOrOld,InnerLen, []), 
		{Result, Buffer1, InnerLen}; 
	    _ ->  
		exit({error,{asn1, length_error}}) 
	end, 
     
 
    %%------------------------------------------- 
    %% At least one byte must have been decoded 
    %% if there were bytes to decode 

    case {NewRb, OuterLen, InnerLen} of  
	_ when NewRb > 0 ->  
	    continue; 
	{0, no_length, 0} -> 
	    %% There were no bytes to decode 
	    continue; 
	{0, _, 0} when integer(OuterLen), (OuterLen - RbSoFar) == 0 -> 
	    %% There were no bytes to decode 
	    continue; 
	_ -> 
	    %% inhibits indefinite loops 
	    exit({error,{asn1, cannot_resolve_the_buffer}}) 
    end,     
 
    %%------------------------------ 
    %% Check if more bytes to decode 
    %%------------------------------ 
    case {OuterLen, NewBuffer} of 
	{no_length, _} -> 
	    {concat_string(ResultSoFar,NewRes), NewBuffer, RbSoFar+NewRb}; 
	{indefinite, []} -> 
	    exit({error,{asn1, {indefinite_legth_error,'EOC_missing'}}}); 
	{indefinite, [A]} -> 
	    exit({error,{asn1, {indefinite_legth_error,'EOC_missing'}}}); 
	{indefinite, [0,0|RestOfBuffer]} -> 
	    {concat_string(ResultSoFar,NewRes), RestOfBuffer, RbSoFar+NewRb+2}; 
	{indefinite, _} -> 
	    decode_restricted(NewBuffer, OuterLen, InnerLen, Form, 
			      StringType, NamedNumberList,BinOrOld,
			      {concat_string(ResultSoFar,NewRes), RbSoFar+NewRb}); 
	_ -> 
	    if 
		OuterLen - RbSoFar - NewRb < 0 -> 
		    exit({error,{asn1, 
				 {explicit_tag_length_error,bytes_missing}}}); 
		OuterLen - RbSoFar - NewRb == 0 -> 
		    {concat_string(ResultSoFar,NewRes), NewBuffer, 
		     RbSoFar+NewRb}; 
		true -> 
		    decode_restricted(NewBuffer, OuterLen, InnerLen, Form,  
				      StringType, NamedNumberList,BinOrOld,
				      {concat_string(ResultSoFar,NewRes), 
				       RbSoFar+NewRb}) 
	    end 
    end. 
 
 
%%----------------------------------- 
%% Decode definite length strings 
%%----------------------------------- 

decode_constr_definite(Buffer, StringType, NamedNumberList,_,0,
		       Result) -> 
    {Result,Buffer}; 
decode_constr_definite(Buffer, StringType, NamedNumberList,_,
		       LenIn, Result) 
  when LenIn < 0 -> 
    exit({error,{asn1error, {definite_length, bytes_missing}}}); 
decode_constr_definite(Buffer, StringType, NamedNumberList,BinOrOld,
		       LenIn, Result) 
  when LenIn >= 0 -> 

    {{Form, Len}, Buffer2, RbTag} = 
	check_tags([#tag{class=?UNIVERSAL,number=StringType}], Buffer), 
    case {Form, StringType} of 
	{?CONSTRUCTED, StringType} when integer(Len) ->  
	    {ResultDef,BufferDef} =  
		decode_constr_definite(Buffer2, StringType, 
				       NamedNumberList,BinOrOld,
				       Len, Result),
	    decode_constr_definite(BufferDef,StringType, NamedNumberList,
				   BinOrOld, LenIn - RbTag - Len, 
				   concat_string(Result,ResultDef)); 
	     
	{?CONSTRUCTED, StringType} when Len == indefinite ->  
	    {ResultInd, BufferInd, RbInd} =  
		decode_constr_indefinite(Buffer2, StringType, 
					 NamedNumberList,BinOrOld,
					 0,Result), 
	    decode_constr_indefinite(BufferInd,StringType,NamedNumberList,  
				     BinOrOld,LenIn - RbTag - RbInd, 
				     concat_string(Result,ResultInd)); 
	     
	{?PRIMITIVE, ?N_OCTET_STRING} when integer(Len) ->  
	    decode_constr_definite(lists:nthtail(Len, Buffer2), 
				   StringType, NamedNumberList,  
				   BinOrOld,LenIn - RbTag - Len, 
				   Result ++ lists:sublist(Buffer2, Len)); 
 
	{?PRIMITIVE, ?N_BIT_STRING} when integer(Len) ->  
	    {ResultAdd, Buffer3, Rb} =  
		decode_bit_string2(Len, Buffer2, NamedNumberList,
				   Len,BinOrOld),
	    decode_constr_definite(Buffer3, 
				   StringType, NamedNumberList,  
				   BinOrOld,LenIn - RbTag - Len, 
				   concat_string(Result,ResultAdd)); 
	_ ->  
	    exit({error,{asn1,{illegal_tag,Buffer}}}) 
    end. 
     

concat_string([],CompactBitString) ->
    CompactBitString;
concat_string(L1,L2) when list(L1),list(L2) ->
    L1++L2;
concat_string({0,Bin1},{Unused2,Bin2}) ->
    {Unused2,concat_binary([Bin1,Bin2])};
concat_string({Unused1,Bin1},{Unused2,Bin2}) ->
    {Bin1Bytes,<<Bin1LastByte>>} = split_binary(Bin1,size(Bin1)-1),
    N1 = 8 - Unused1,
    {Unused1+Unused2,
     <<Bin1Bytes/binary,(Bin1LastByte bsr Unused1):N1,Bin2/binary,0:Unused1>>}.


%%----------------------------------- 
%% Decode indefinite length strings 
%%----------------------------------- 

decode_constr_indefinite([], _, _,_,RemovedBytes, Result) -> 
    {Result, [], RemovedBytes}; 
decode_constr_indefinite([A], _, _,_,RemovedBytes, Result) -> 
    {Result, [A], RemovedBytes}; 
decode_constr_indefinite([0,0], _,_,_, RemovedBytes, Result) -> 
    {Result, [], RemovedBytes+2}; 
decode_constr_indefinite([0,0|Buffer], _, _,_,RemovedBytes, Result) -> 
    {Result, Buffer, RemovedBytes+2}; 
decode_constr_indefinite(Buffer, StringType, NamedNumberList,BinOrOld,
			 RbIn, Result) -> 
    {{Form,Len}, Buffer2, RbTag} = 
	check_tags([#tag{class=?UNIVERSAL,number=StringType}],Buffer), 
    case {Form, StringType} of 
	{?CONSTRUCTED, StringType} when integer(Len) ->  
	    {ResultDef,BufferDef} =  
		decode_constr_definite(Buffer2, StringType, 
				       NamedNumberList,BinOrOld,
				       Len, Result), 
	    decode_constr_indefinite(BufferDef,StringType,NamedNumberList,  
				     BinOrOld,RbIn + RbTag + Len, 
				     concat_string(Result,ResultDef)); 
	     
	{?CONSTRUCTED, StringType} when Len == indefinite ->  
	    {ResultInd, BufferInd, RbInd} =  
		decode_constr_indefinite(Buffer2, StringType, 
					 NamedNumberList,BinOrOld,0,Result), 
	    case BufferInd of 
		[] -> 
		    {concat_string(Result, ResultInd), BufferInd,
		     RbIn + RbTag + RbInd}; 
		[X] -> 
		    {concat_string(Result, ResultInd), BufferInd,
		     RbIn + RbTag + RbInd}; 
		[0,0|X] -> 
		    {concat_string(Result,ResultInd), BufferInd, RbIn + RbTag + RbInd}; 
		_ -> 
		    decode_constr_indefinite(BufferInd, StringType, 
					     NamedNumberList,BinOrOld,  
					     RbIn + RbTag + RbInd, 
					     concat_string(Result,ResultInd)) 
	    end; 
	     
	{?PRIMITIVE, ?N_OCTET_STRING} when integer(Len) ->  
	    decode_constr_indefinite(lists:nthtail(Len, Buffer2), 
				     StringType, NamedNumberList,BinOrOld,  
				     RbIn + RbTag + Len, 
				     Result ++ lists:sublist(Buffer2, Len)); 
 
	{?PRIMITIVE, ?N_BIT_STRING} when integer(Len) ->  
	    {ResultAdd, Buffer3, Rb} =  
		decode_bit_string2(Len,Buffer2,NamedNumberList,Len,BinOrOld), 
	    decode_constr_indefinite(Buffer3, 
				     StringType, NamedNumberList,  
				     BinOrOld,RbIn + RbTag + Len, 
				     concat_string(Result,ResultAdd)); 
	_ ->  
	    exit({error,{asn1, {illegal_tag, Buffer}}}) 
    end. 
 
%%============================================================================ 
%% encode Universal string 
%%============================================================================ 
encode_universal_string(C, {Name, Universal}, DoTag) when atom(Name) -> 
    encode_universal_string(C, Universal, DoTag);
encode_universal_string(C, Universal, DoTag) -> 
    OctetList = mk_uni_list(Universal), 
    Len = integer_to_list(length(OctetList)), 
    dotag(DoTag, ?N_UniversalString, {OctetList,length(OctetList)}). 
 
mk_uni_list(In) ->  
    mk_uni_list(In,[]). 
 
mk_uni_list([],List) ->  
    List; 
mk_uni_list([{A,B,C,D}|T],List) ->  
    mk_uni_list(T,List++[A,B,C,D]); 
mk_uni_list([H|T],List) ->  
    mk_uni_list(T,List++[0,0,0,H]). 
 
%%=========================================================================== 
%% decode Universal strings  
%%    (Buffer, Range, StringType, HasTag, LenIn) -> 
%%                           {String, Remain, RemovedBytes}  
%%=========================================================================== 

decode_universal_string(Buffer, Range, HasTag, LenIn, OptOrMand) -> 
    decode_restricted_string(Buffer, Range, ?N_UniversalString, 
			     HasTag, LenIn, OptOrMand). 
 
mk_universal_string(In) -> 
    mk_universal_string(In,[]). 
 
mk_universal_string([],Acc) -> 
    lists:reverse(Acc); 
mk_universal_string([0,0,0,D|T],Acc) -> 
    mk_universal_string(T,[D|Acc]); 
mk_universal_string([A,B,C,D|T],Acc) -> 
    mk_universal_string(T,[{A,B,C,D}|Acc]). 
 
 
%%============================================================================ 
%% encode BMP string 
%%============================================================================ 

encode_BMP_string(C, {Name,BMPString}, DoTag) when atom(Name)-> 
    encode_BMP_string(C, BMPString, DoTag);
encode_BMP_string(C, BMPString, DoTag) -> 
    OctetList = mk_BMP_list(BMPString), 
    dotag(DoTag, ?N_BMPString, {OctetList,length(OctetList)}). 
 
mk_BMP_list(In) ->  
    mk_BMP_list(In,[]). 
 
mk_BMP_list([],List) ->  
    List; 
mk_BMP_list([{0,0,C,D}|T],List) ->  
    mk_BMP_list(T,List++[C,D]); 
mk_BMP_list([H|T],List) ->  
    mk_BMP_list(T,List++[0,H]). 
 
%%============================================================================ 
%% decode (OctetList, Range(ignored), tag|notag) -> {ValList, RestList} 
%%    (Buffer, Range, StringType, HasTag, TotalLen) -> 
%%                               {String, Remain, RemovedBytes}  
%%============================================================================ 
decode_BMP_string(Buffer, Range, HasTag, LenIn, OptOrMand) -> 
    decode_restricted_string(Buffer, Range, ?N_BMPString, 
			     HasTag, LenIn, OptOrMand). 
 
 
mk_BMP_string(In) -> 
    mk_BMP_string(In,[]). 
 
mk_BMP_string([],US) -> 
    lists:reverse(US); 
mk_BMP_string([0,B|T],US) -> 
    mk_BMP_string(T,[B|US]); 
mk_BMP_string([C,D|T],US) -> 
    mk_BMP_string(T,[{0,0,C,D}|US]). 
 
 
%%============================================================================ 
%% Generalized time, ITU_T X.680 Chapter 39 
%%
%% encode Generalized time 
%%============================================================================ 
encode_generalized_time(C, {Name,OctetList}, DoTag) when atom(Name) -> 
    encode_generalized_time(C, OctetList, DoTag);
encode_generalized_time(C, OctetList, DoTag) -> 
    Len = integer_to_list(length(OctetList)), 
    dotag(DoTag, ?N_GeneralizedTime, 
	  {OctetList,length(OctetList)}). 
 
%%============================================================================ 
%% decode Generalized time  
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_generalized_time(Buffer, Range, Tags, TotalLen, OptOrMand) -> 
    {{Form, Len}, Buffer2, RemovedBytes} =  
	check_tags(Tags ++ 
		   [#tag{class=?UNIVERSAL,
			 number=?N_GeneralizedTime}],Buffer, OptOrMand), 
    {lists:sublist(Buffer2, Len), 
     lists:nthtail(Len, Buffer2), RemovedBytes+Len}.

%%============================================================================ 
%% Universal time, ITU_T X.680 Chapter 40 
%%
%% encode UTC time 
%%============================================================================ 

encode_utc_time(C, {Name,OctetList}, DoTag) when atom(Name) -> 
    encode_utc_time(C, OctetList, DoTag);
encode_utc_time(C, OctetList, DoTag) -> 
    Len = integer_to_list(length(OctetList)), 
    dotag(DoTag, ?N_UTCTime, {OctetList,length(OctetList)}).
 
%%============================================================================ 
%% decode UTC time 
%%    (Buffer, Range, HasTag, TotalLen) -> {String, Remain, RemovedBytes}  
%%============================================================================ 

decode_utc_time(Buffer, Range, Tags, TotalLen, OptOrMand) -> 
    {{Form, Len}, Buffer2, RemovedBytes} =  
	check_tags(Tags ++ [#tag{class=?UNIVERSAL,
				 number=?N_UTCTime}], Buffer, OptOrMand),
    {lists:sublist(Buffer2, Len), 
     lists:nthtail(Len, Buffer2), RemovedBytes+Len}. 
 
%%============================================================================ 
%% Length handling  
%%
%% Encode length 
%% 
%% encode_length(Int | indefinite) -> 
%%          [<127]| [128 + Int (<127),OctetList] | [16#80] 
%%============================================================================ 
 
encode_length(indefinite) -> 
    {[16#80],1}; % 128 
encode_length(L) when L =< 16#7F -> 
    {[L],1}; 
encode_length(L) -> 
    Oct = minimum_octets(L), 
    Len = length(Oct), 
    if 
	Len =< 126 -> 
	    {[ (16#80+Len) | Oct ],Len+1}; 
	true -> 
	    exit({error,{asn1, to_long_length_oct, Len}}) 
    end. 
 
 
minimum_octets(Val) -> 
    minimum_octets(Val,[]). 
 
minimum_octets(Val, Acc) when Val > 0 ->  
    minimum_octets((Val bsr 8),[Val band 16#FF | Acc]); 
minimum_octets(0,Acc) -> 
    Acc. 
 
%%=========================================================================== 
%% Decode length 
%% 
%% decode_length(OctetList) -> {{indefinite, RestOctetsL}, NoRemovedBytes} |  
%%                             {{Length, RestOctetsL}, NoRemovedBytes} 
%%=========================================================================== 
     
decode_length([128 | T]) -> 
    {{indefinite, T},1}; 
decode_length([H | T]) when H =< 127 -> 
    {{H, T},1}; 
decode_length([H | T]) -> 
    dec_long_length(H band 16#7F, T, 0, 1). 
 
dec_long_length(0, Buffer, Acc, Len) -> 
    {{Acc, Buffer},Len}; 
dec_long_length(Bytes, [H | T], Acc, Len) -> 
    dec_long_length(Bytes - 1, T, (Acc bsl 8) + H, Len+1). 
 
%%===========================================================================
%% Decode tag and length
%%
%% decode_tag_and_length(Buffer) -> {Tag, Len, RemainingBuffer, RemovedBytes}
%% 
%%===========================================================================

decode_tag_and_length(Buffer) ->
    {Tag, Buffer2, RemBytesTag} = decode_tag(Buffer), 
    {{Len, Buffer3}, RemBytesLen} = decode_length(Buffer2), 
    {Tag, Len, Buffer3, RemBytesTag+RemBytesLen}. 
 
%%============================================================================ 
%% Remove the length indicator 
%% 
%% remove_length(OctetList) -> [RestOctetsL] 
%%============================================================================ 
     
remove_length([128 | T]) -> 
    {indefinite, T}; 
remove_length([H | T]) when H =< 127 -> 
    T; 
remove_length([H | T]) -> 
    remove_long_length(H band 16#7F, T, 0). 
 
remove_long_length(0, Buffer, Acc) -> 
    Buffer; 
remove_long_length(Bytes, [H | T], Acc) -> 
    remove_long_length(Bytes - 1, T, (Acc bsl 8) + H). 
 
 
%%============================================================================ 
%% Check if valid tag 
%% 
%% check_if_valid_tag(Tag, List_of_valid_tags, OptOrMand) -> name of the tag 
%%=============================================================================== 

check_if_valid_tag([0,0|_],_,_) ->
    asn1_EOC;
check_if_valid_tag([], _, OptOrMand) -> 
    check_if_valid_tag2(false,[],[],OptOrMand);
check_if_valid_tag(Bytes, ListOfTags, OptOrMand) when list(Bytes) ->
    {Tag, _, _} = decode_tag(Bytes),
    check_if_valid_tag(Tag, ListOfTags, OptOrMand);

%% This alternative should be removed in the near future
%% Bytes as input should be the only necessary call
check_if_valid_tag(Tag, ListOfTags, OptOrMand) -> 
    {Class, Form, TagNo} = Tag, 
    C = code_class(Class), 
    T = case C of 
	    'UNIVERSAL' -> 
		code_type(TagNo); 
	    _ -> 
		TagNo 
	end, 
    check_if_valid_tag2({C,T}, ListOfTags, Tag, OptOrMand). 

check_if_valid_tag2(Class_TagNo, [], Tag, mandatory) -> 
    exit({error,{asn1,{invalid_tag,Tag}}}); 
check_if_valid_tag2(Class_TagNo, [], Tag, _) -> 
    exit({error,{asn1,{no_optional_tag,Tag}}}); 
 
check_if_valid_tag2(Class_TagNo, [{TagName,TagList}|T], Tag, OptOrMand) -> 
    case check_if_valid_tag_loop(Class_TagNo, TagList) of 
	true -> 
	    TagName; 
	false -> 
	    check_if_valid_tag2(Class_TagNo, T, Tag, OptOrMand) 
    end. 
 
check_if_valid_tag_loop(Class_TagNo,[]) -> 
    false; 
check_if_valid_tag_loop(Class_TagNo,[H|T]) -> 
    %% It is not possible to distinguish between SEQUENCE OF and SEQUENCE, and
    %% between SET OF and SET because both are coded as 16 and 17, respectively.
    H_without_OF = case H of
		       {C, 'SEQUENCE OF'} ->
			   {C, 'SEQUENCE'};
		       {C, 'SET OF'} ->
			   {C, 'SET'};
		       Else ->
			   Else
		   end,

    case H_without_OF of 
	Class_TagNo -> 
	    true; 
	{_,_} -> 
	    check_if_valid_tag_loop(Class_TagNo,T); 
	_ -> 
	    check_if_valid_tag_loop(Class_TagNo,H), 
	    check_if_valid_tag_loop(Class_TagNo,T) 
    end. 
 
 
 
code_class(0) -> 'UNIVERSAL'; 
code_class(16#40) -> 'APPLICATION'; 
code_class(16#80) -> 'CONTEXT'; 
code_class(16#C0) -> 'PRIVATE'. 
 
 
code_type(1) -> 'BOOLEAN'; 
code_type(2) -> 'INTEGER'; 
code_type(3) -> 'BIT STRING';  
code_type(4) -> 'OCTET STRING';  
code_type(5) -> 'NULL'; 
code_type(6) -> 'OBJECT IDENTIFIER'; 
code_type(7) -> 'OBJECT DESCRIPTOR'; 
code_type(8) -> 'EXTERNAL'; 
code_type(9) -> 'REAL'; 
code_type(10) -> 'ENUMERATED'; 
code_type(11) -> 'EMBEDDED_PDV'; 
code_type(16) -> 'SEQUENCE'; 
code_type(16) -> 'SEQUENCE OF'; 
code_type(17) -> 'SET'; 
code_type(17) -> 'SET OF'; 
code_type(18) -> 'NumericString';   
code_type(19) -> 'PrintableString';   
code_type(20) -> 'TeletexString';   
code_type(21) -> 'VideotexString';   
code_type(22) -> 'IA5String';   
code_type(23) -> 'UTCTime';   
code_type(24) -> 'GeneralizedTime';   
code_type(25) -> 'GraphicString';   
code_type(26) -> 'VisibleString';   
code_type(27) -> 'GeneralString';   
code_type(28) -> 'UniversalString';   
code_type(30) -> 'BMPString';   
code_type(Else) -> exit({error,{asn1,{unrecognized_type,Else}}}). 
 
%sequence_optional(TagType, Bytes, {Class, TagNr}, 
%		  Fun, OptOrDefault) ->
%    ReturnValue = case OptOrDefault of
%		      'OPTIONAL' -> asn1_NOVALUE;
%		      {'DEFAULT',DefValue} -> DefValue
%		  end,
%    case {TagType,Bytes} of
%	{_,[]} -> 
%	    {ReturnValue, Bytes, 0};
%	{universal, _} -> % no_tag
%            case catch Fun(Bytes, 'OPTIONAL', {Class, TagNr}) of
%               {'EXIT', {error,{asn1, {no_optional_tag, _}}}} -> 
%                  {ReturnValue, Bytes, 0};
%               {Term,RestOfBytes,Rb} -> 
%                  {Term,RestOfBytes,Rb}; 
%               {'EXIT',Reason} -> exit(Reason)
%            end;
%	{implicit, _} ->
%	    case catch decode_tag(Bytes) of
%		{'EXIT',_} ->
%		    {ReturnValue,Bytes,0};
%		{Dtag,Bytes1,Rb1} ->
%		    case Dtag of
%			{Class,Form,TagNr} ->
%			    case 
%				catch Fun(Bytes, 'OPTIONAL', {Class, TagNr}) of
%				{'EXIT', 
%				 {error,{asn1, {no_optional_tag, _}}}} -> 
%				    {ReturnValue, Bytes, 0};
%				{Term,RestOfBytes,Rb} -> 
%				    {Term,RestOfBytes,Rb}; 
%				{'EXIT',Reason} -> exit(Reason)
%			    end;
%			_ ->
%			    {ReturnValue,Bytes,0}
%		    end
%	    end;
%	{explicit, _} ->
%	    {Tag, Bytes1, Rb1} = decode_tag(Bytes),
%	    case Tag of
%		{Class, Form, TagNr} ->
%		    {{Length, Bytes2}, Rb2} = decode_length(Bytes1),
%		    {Term, RestOfBytes, Rb3} = 
%			Fun(Bytes2, 'OPTIONAL',{Class, TagNr}),
%		    {Term, RestOfBytes, Rb1 + Rb2 + Rb3};
%		_ ->
%		    {ReturnValue, Bytes, 0}
%	    end
%    end.

%%-------------------------------------------------------------------------
%% decoding of the components of a SET
%%-------------------------------------------------------------------------

decode_set(Rb, indefinite, [0,0|Bytes], OptOrMand, _Fun3, Acc) ->
   {lists:reverse(Acc),Bytes,Rb+2};

decode_set(Rb, indefinite, Bytes, OptOrMand, Fun3, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, OptOrMand),
   decode_set(Rb+Rb1, indefinite, Remain, OptOrMand, Fun3, [Term|Acc]);

decode_set(Rb, Num, Bytes, OptOrMand, _Fun3, Acc) when Num == 0 ->
   {lists:reverse(Acc), Bytes, Rb};

decode_set(Rb, Num, Bytes, OptOrMand, _Fun3, Acc) when Num < 0 ->
   exit({error,{asn1,{length_error,'SET'}}});

decode_set(Rb, Num, Bytes, OptOrMand, Fun3, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, OptOrMand),
   decode_set(Rb+Rb1, Num-Rb1, Remain, OptOrMand, Fun3, [Term|Acc]).



%%-------------------------------------------------------------------------
%% decoding of SEQUENCE OF and SET OF
%%-------------------------------------------------------------------------

decode_components(Rb, indefinite, [0,0|Bytes], _Fun3, _TagIn, Acc) ->
   {lists:reverse(Acc),Bytes,Rb+2};

decode_components(Rb, indefinite, Bytes, Fun3, TagIn, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, mandatory, TagIn),
   decode_components(Rb+Rb1, indefinite, Remain, Fun3, TagIn, [Term|Acc]);

decode_components(Rb, Num, Bytes, _Fun3, _TagIn, Acc) when Num == 0 ->
   {lists:reverse(Acc), Bytes, Rb};

decode_components(Rb, Num, Bytes, _Fun3, _TagIn, Acc) when Num < 0 ->
   exit({error,{asn1,{length_error,'SET/SEQUENCE OF'}}});

decode_components(Rb, Num, Bytes, Fun3, TagIn, Acc) ->
   {Term, Remain, Rb1} = Fun3(Bytes, mandatory, TagIn),
   decode_components(Rb+Rb1, Num-Rb1, Remain, Fun3, TagIn, [Term|Acc]).
    

%%-------------------------------------------------------------------------
%% INTERNAL HELPER FUNCTIONS (not exported)
%%-------------------------------------------------------------------------

 
%%========================================================================== 
%% Encode tag 
%% 
%% dotag(tag | notag, TagValpattern | TagValTuple, [Length, Value]) -> [Tag]  
%% TagValPattern is a correct bitpattern for a tag 
%% TagValTuple is a tuple of three bitpatterns, Class, Form and TagNo where 
%%     Class = UNIVERSAL | APPLICATION | CONTEXT | PRIVATE 
%%     Form  = Primitive | Constructed 
%%     TagNo = Number of tag 
%%========================================================================== 
 

dotag(Tags, Tag, {Bytes,Len}) ->
    encode_tags(Tags ++ [#tag{class=?UNIVERSAL,number=Tag,form=?PRIMITIVE}], Bytes, Len).

%dotag(tag, Tag, LenVal) -> 
%    [encode_tag_val(Tag) | LenVal]; 
%dotag(_, _, LenVal) -> 
%    LenVal. 

    
%decode_tag_length(Buffer,HasTag,LengthIn) -> 
%    case HasTag of
%	WantedTag when tuple(WantedTag), size(WantedTag) == 3 ->
%	    case catch decode_tag(Buffer) of
%		{WantedTag, Buffer2, RemBytesTag} ->
%		    {{Len, Buffer3}, RemBytesLen} = decode_length(Buffer2),
%		    {WantedTag, Len, Buffer3, RemBytesTag + RemBytesLen};
%		WT ->
%		    exit({error,{asn1, {invalid_tag, {WT, WantedTag,Buffer}}}})
%	    end;
%	undefined -> 
%	    {Tag, Buffer2, RemBytesTag} = decode_tag(Buffer), 
%	    {{Len, Buffer3}, RemBytesLen} = decode_length(Buffer2), 
%	    {Tag, Len, Buffer3, RemBytesTag+RemBytesLen}; 
 
%	{notag, Form} -> 
%	    {notag, LengthIn, Buffer, 0} 
%    end. 
 

decode_integer(Buffer, Tags, OptOrMand) -> 
    {{_,Len}, Buffer2, RemovedBytes} = check_tags(Tags, Buffer, OptOrMand), 
    decode_integer2(Len, Buffer2, 0, RemovedBytes+Len). 
 
decode_integer2(Len,Buffer,Acc,RemovedBytes) when (hd(Buffer) band 16#FF) =< 16#7F ->  
    {decode_integer_pos(Buffer, 8 * (Len - 1)),skip(Buffer,Len),RemovedBytes}; 
decode_integer2(Len,Buffer,Acc,RemovedBytes)  -> 
    {decode_integer_neg(Buffer, 8 * (Len - 1)),skip(Buffer,Len),RemovedBytes}. 
 
 
 
 
decode_integer_pos([Byte|Tail], Shift) -> 
    (Byte bsl Shift) bor decode_integer_pos(Tail, Shift-8); 
decode_integer_pos([], _) -> 0. 
 
decode_integer_neg([Byte|Tail], Shift) -> 
    (-128 + (Byte band 127) bsl Shift) bor decode_integer_pos(Tail, Shift-8). 
 

get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V
    end.
 
skip(Buffer, 0) -> 
    Buffer; 
skip([H | T], Len) -> 
    skip(T, Len-1). 
     
new_tags([],LastTag) ->
    [LastTag];
new_tags(Tags=[#tag{type='IMPLICIT'}],LastTag) ->
    Tags;
new_tags(Tags,LastTag) ->
    case lists:last(Tags) of
	#tag{type='IMPLICIT'} ->
	    Tags;
	_ ->
	    Tags ++ [LastTag]
    end.
