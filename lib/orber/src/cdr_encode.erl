%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: cdr_encode.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains all encoding functions for the CDR
%%    format.
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(cdr_encode).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([enc_giop_msg_type/1, enc_request/8, enc_reply/6,
	 enc_type/3, enc_type/5, enc_cancel_request/2,
	 enc_locate_request/3, enc_locate_reply/3, enc_close_connection/1,
	 enc_message_error/1, enc_fragment/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: enc_giop_message_header/5
%%-----------------------------------------------------------------
%% The header size is known so we know that the size will be aligned.
%% MessSize already includes the header length.
%%-----------------------------------------------------------------
enc_giop_message_header({1,0}, MessType, Flags, MessSize, Message) ->
    ?PRINTDEBUG("Encode GIOP header version == 1.0"),
    Message1 = cdrlib:enc_unsigned_long(MessSize, Message), 
    {Message2, _}  = enc_type('tk_octet', {1, 0}, enc_giop_msg_type(MessType),
			      Message1, 0),
    {Message3, _} = enc_byte_order({1, 0}, Message2),
    Message4 = enc_giop_version({1, 0}, Message3),
    enc_magic(Message4);
enc_giop_message_header(Version, MessType, Flags, MessSize, Message) ->
    ?PRINTDEBUG("Encode GIOP header version > 1.0"),
    Message1 = cdrlib:enc_unsigned_long(MessSize, Message), 
    {Message2, _}  = enc_type('tk_octet', Version, enc_giop_msg_type(MessType),
			      Message1, 0),
    {Message3, _} = enc_flags(Version, Flags, Message2),
    Message4 = enc_giop_version(Version, Message3),
    enc_magic(Message4).

enc_magic(Message) ->
    [$G, $I, $O, $P | Message].

enc_giop_version({Major, Minor}, Message) ->
    [Major, Minor | Message].

enc_byte_order(Version, Message) ->
    enc_type('tk_boolean', Version, 'false', Message, 0).

enc_flags(Version, Flags, Message) ->
    enc_type('tk_octet', Version, 0, Message, Flags). %%% LTH Skall fixas ordentligt 

%%-----------------------------------------------------------------
%% Func: enc_parameters/2
%%-----------------------------------------------------------------
enc_parameters(_, [], [], Message, Len) ->
    {Message, Len};
enc_parameters(_, [], _, _, _) -> 
    corba:raise(#'MARSHAL'{minor=103, completion_status=?COMPLETED_MAYBE});
enc_parameters(Version, [PT1 |TypeList], [ P1 | Parameters], Message, Len) ->
    {Message1, Len1} = enc_type(PT1, Version, P1, Message, Len),
    enc_parameters(Version, TypeList, Parameters, Message1, Len1).

%%-----------------------------------------------------------------
%% Func: enc_request/8
%%-----------------------------------------------------------------
enc_request(Version, ObjectKey, RequestId, ResponseExpected, Op, Parameters,
	    Context, TypeCodes) ->
    Flags = 1, %% LTH Not correct, just placeholder
    Operation = atom_to_list(Op),
    {Message0, Len0} = enc_service_context(Version, Context, [],
					   ?GIOP_HEADER_SIZE),
    {Message, Len} = enc_request_id(Version, RequestId, Message0, Len0),
    {Message1, Len1} = enc_response(Version, ResponseExpected, Message, Len),
    {Message1b, Len1b} =
	if
	    Version /= {1,0} ->
		enc_reserved(Version, {0,0,0}, Message1, Len1);
	    true ->
		{Message1, Len1}
	end,
    {Message2, Len2} = enc_object_key(Version, ObjectKey, Message1b, Len1b),
    {Message3, Len3} = enc_operation(Version, Operation, Message2, Len2),
    {Message4, Len4} = enc_principal(Version, Message3, Len3),
    {Message5, Len5} = enc_request_body(Version, TypeCodes, Parameters,
					Message4, Len4),
    enc_giop_message_header(Version, 'request', Flags, Len5 - ?GIOP_HEADER_SIZE,
			    lists:reverse(Message5)).

enc_principal(Version, Mess, Len) ->
    enc_type({'tk_string', 0}, Version, atom_to_list(node()), Mess, Len).
    
enc_operation(Version, Op, Mess, Len) ->
    enc_type({'tk_string', 0}, Version, Op, Mess, Len).
    
enc_object_key(Version, ObjectKey, Mess, Len) ->
    enc_type({'tk_sequence', 'tk_octet', 0}, Version, ObjectKey, Mess, Len).

enc_reserved(Version, Reserved, Mess, Len) ->
    enc_type({'tk_array', 'tk_octet', 3}, Version, Reserved, Mess, Len).

enc_response(Version, ResponseExpected, Mess, Len) ->
    enc_type('tk_boolean', Version, ResponseExpected, Mess, Len).
    
enc_request_id(Version, RequestId, Mess, Len) ->  
    enc_type('tk_ulong', Version, RequestId, Mess, Len).

enc_service_context(Version, Context, Message, Len) ->
    enc_type({'tk_sequence',
	      {'tk_struct', 0, 'ServiceContext',
	       [{"context_id", 'tk_long'},
		{"context_data",
		 {'tk_sequence', 'tk_octet', 0}}]}, 0},
	     Version, Context, Message, Len).
    
%%-----------------------------------------------------------------
%% Func: enc_request_body/5
%%-----------------------------------------------------------------
enc_request_body(Version, {RetType, InParameters, OutParameters}, Parameters,
		 Message, Len) ->
    enc_parameters(Version, InParameters, Parameters, Message, Len).

%%-----------------------------------------------------------------
%% Func: enc_reply/6
%%-----------------------------------------------------------------
enc_reply(Version, ReqId, RepStatus, TypeCodes, Result, OutParameters) ->
    Flags = 1, %% LTH Not correct, just placeholder
    {Message, Len} = enc_service_context(Version, [], [],
					?GIOP_HEADER_SIZE),
    {Message1, Len1} = enc_request_id(Version, ReqId, Message, Len), 
    {Message2, Len2} = enc_reply_status(Version, RepStatus, Message1, Len1),
    {Message3, Len3} = enc_reply_body(Version, TypeCodes, Result, OutParameters,
				      Message2, Len2),
    enc_giop_message_header(Version, 'reply', Flags, Len3 - ?GIOP_HEADER_SIZE,
			    lists:reverse(Message3)).

enc_reply_status(Version, Status, Mess, Len) ->
    L = enc_giop_reply_status_type(Status),
    enc_type('tk_ulong', Version, L, Mess, Len).

%%-----------------------------------------------------------------
%% Func: enc_reply_body/6
%%-----------------------------------------------------------------
enc_reply_body(Version, {RetType, InParameters, OutParameters}, Result, Parameters,
	       Message, Len) ->
    {Message1, Len1}  = enc_type(RetType, Version, Result, Message, Len),
    enc_parameters(Version, OutParameters, Parameters, Message1, Len1).

%%-----------------------------------------------------------------
%% Func: enc_cancel_request/2
%%-----------------------------------------------------------------
enc_cancel_request(Version, RequestId) ->
    Flags = 1, %% LTH Not correct, just placeholder
    {Message, Len} = enc_request_id(Version, RequestId, [],
				   ?GIOP_HEADER_SIZE),
    enc_giop_message_header(Version, 'cancel_request', Flags, Len - ?GIOP_HEADER_SIZE,
			    lists:reverse(Message)).

%%-----------------------------------------------------------------
%% Func: enc_locate_request/3
%%-----------------------------------------------------------------
enc_locate_request(Version, ObjectKey, RequestId) ->
    Flags = 1, %% LTH Not correct, just placeholder
    {Message, Len} = enc_request_id(Version, RequestId, [],
				   ?GIOP_HEADER_SIZE), 
    {Message1, Len1} = enc_object_key(Version, ObjectKey, Message, Len),
    enc_giop_message_header(Version, 'locate_request', Flags, Len1 - ?GIOP_HEADER_SIZE, 
			    lists:reverse(Message1)).

%%-----------------------------------------------------------------
%% Func: enc_locate_reply/3
%%-----------------------------------------------------------------
enc_locate_reply(Version, RequestId, LocStatus) ->
    Flags = 1, %% LTH Not correct, just placeholder
    {Message, Len} = enc_request_id(Version, RequestId, [],
				   ?GIOP_HEADER_SIZE), 
    {Message1, Len1} = enc_locate_status(Version, LocStatus, Message, Len),
    enc_giop_message_header(Version, 'locate_reply', Flags, Len1 - ?GIOP_HEADER_SIZE, 
			    lists:reverse(Message1)).

enc_locate_status(Version, Status, Mess, Len) ->
    L = enc_giop_locate_status_type(Status),
    enc_type('tk_ulong', Version, L, Mess, Len).
%%-----------------------------------------------------------------
%% Func: enc_close_connection/1
%%-----------------------------------------------------------------
enc_close_connection(Version) ->
    Flags = 1, %% LTH Not correct, just placeholder
    enc_giop_message_header(Version, 'close_connection', Flags, 0, []).

%%-----------------------------------------------------------------
%% Func: enc_message_error/1
%%-----------------------------------------------------------------
enc_message_error(Version) ->
    Flags = 1, %% LTH Not correct, just placeholder
    enc_giop_message_header(Version, 'message_error', Flags, 0, []).

%%-----------------------------------------------------------------
%% Func: enc_fragment/1
%%-----------------------------------------------------------------
enc_fragment(Version) ->
    Flags = 1, %% LTH Not correct, just placeholder
    enc_giop_message_header(Version, 'message_error', Flags, 0, []).

%%-----------------------------------------------------------------
%% Func: enc_giop_msg_type
%% Args: An integer message type code
%% Returns: An atom which is the message type code name
%%-----------------------------------------------------------------
enc_giop_msg_type('request') ->  %% LTH lägga in version ??
    0;
enc_giop_msg_type('reply') ->
    1;
enc_giop_msg_type('cancel_request') ->
    2;
enc_giop_msg_type('locate_request') ->
    3;
enc_giop_msg_type('locate_reply') ->
    4;
enc_giop_msg_type('close_connection') ->
    5;
enc_giop_msg_type('message_error') ->
    6;
enc_giop_msg_type('fragment') ->
    7.


%%-----------------------------------------------------------------
%% Func: enc_giop_reply_status_type
%% Args: An atom which is the reply status
%% Returns: An integer status code
%%-----------------------------------------------------------------
enc_giop_reply_status_type(?NO_EXCEPTION) ->
    0;
enc_giop_reply_status_type(?USER_EXCEPTION) ->
    1;
enc_giop_reply_status_type(?SYSTEM_EXCEPTION) ->
    2;
enc_giop_reply_status_type('location_forward') ->
    3.

%%-----------------------------------------------------------------
%% Func: enc_giop_locate_status_type
%% Args: An integer status code
%% Returns: An atom which is the reply status
%%-----------------------------------------------------------------
enc_giop_locate_status_type('unknown_object') ->
    0;
enc_giop_locate_status_type('object_here') ->
    1;
enc_giop_locate_status_type('object_forward') ->
    2.

%%-----------------------------------------------------------------
%% Func: enc_type/3
%%-----------------------------------------------------------------
enc_type(Version, TypeCode, Value) ->
    {Bytes, Len} = enc_type(TypeCode, Version, Value, [], 0),
    Bytes1 = lists:reverse(Bytes),
    lists:flatten(Bytes1).
%%-----------------------------------------------------------------
%% Func: enc_type/5
%%-----------------------------------------------------------------
enc_type('tk_null', Version, null, Bytes, Len) ->
    {Bytes, Len}; 
enc_type('tk_void', Version, ok, Bytes, Len) ->
    {Bytes, Len}; 
enc_type('tk_short', Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 2),
    {cdrlib:enc_r_short(Value, Rest), Len1 + 2};
enc_type('tk_long', Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 4),
    {cdrlib:enc_r_long(Value, Rest ), Len1 + 4};
enc_type('tk_ushort', Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 2),
    {cdrlib:enc_r_unsigned_short(Value, Rest), Len1 + 2};
enc_type('tk_ulong', Version, Value, Bytes, Len) -> 
    {Rest, Len1} = enc_align(Bytes, Len, 4),
    {cdrlib:enc_r_unsigned_long(Value, Rest), Len1 + 4};
enc_type('tk_float', Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 4),
    {cdrlib:enc_r_float(Value, Rest), Len1 + 4};
enc_type('tk_double', Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 8),
    {cdrlib:enc_r_double(Value, Rest), Len1 + 8};
enc_type('tk_boolean', Version, Value, Bytes, Len) ->
    {cdrlib:enc_bool(Value, Bytes), Len + 1};
enc_type('tk_char', Version, Value, Bytes, Len) ->
    {cdrlib:enc_char(Value, Bytes), Len + 1};
enc_type('tk_octet', Version, Value, Bytes, Len) ->
    {cdrlib:enc_octet(Value, Bytes), Len + 1};
enc_type('tk_any', Version, Any, Bytes, Len) when record(Any, any) ->
    {Rest, Len1} = enc_type('tk_TypeCode', Version, Any#any.typecode, Bytes, Len),
    enc_type(Any#any.typecode, Version, Any#any.value, Rest, Len1);
enc_type('tk_TypeCode', Version, Value, Bytes, Len) ->
    enc_type_code(Value, Version, Bytes, Len);
enc_type('tk_Principal', Version, Value, Bytes, Len) ->
    enc_sequence(Version, Value, 'tk_octet', Bytes, Len);
enc_type({'tk_objref', IFRId, Name}, Version, Value, Bytes, Len) ->
    enc_objref(Version, Name,Value, Bytes, Len);
enc_type({'tk_struct', IFRId, Name, ElementList}, Version, Value, Bytes, Len) -> 
    enc_struct(Version, Value, ElementList, Bytes, Len);
enc_type({'tk_union', IFRId, Name, DiscrTC, Default, ElementList},
	Version, Value, Bytes, Len) ->
    enc_union(Version, Value, DiscrTC, ElementList, Bytes, Len);
enc_type({'tk_enum', IFRId, Name, ElementList}, Version, Value, Bytes, Len) ->
    {Rest, Len1} = enc_align(Bytes, Len, 4),
    {cdrlib:enc_r_enum(atom_to_list(Value), ElementList, Rest), Len1 + 4};
enc_type({'tk_string', MaxLength}, Version, Value, Bytes, Len) -> % MaxLength not used
    enc_string(Version, Value, Bytes, Len);
enc_type({'tk_sequence', ElemTC, MaxLength}, Version, Value, Bytes, Len) -> % MaxLength not used
    enc_sequence(Version, Value, ElemTC, Bytes, Len);
enc_type({'tk_array', ElemTC, Size}, Version, Value, Bytes, Len) -> 
    enc_array(Version, Value, Size, ElemTC, Bytes, Len);
enc_type({'tk_alias', IFRId, Name, TC}, Version, Value, Bytes, Len) ->
    enc_type(Version, TC, Value, Bytes, Len);
enc_type({'tk_except', IFRId, Name, ElementList}, Version, Value, Bytes, Len) ->
    enc_exception(Version, Name, IFRId, Value, ElementList, Bytes, Len);
enc_type(_, _, _, _, _) ->
    corba:raise(#'MARSHAL'{minor=104, completion_status=?COMPLETED_MAYBE}).

%%-----------------------------------------------------------------
%% Func: enc_sequence/5
%%-----------------------------------------------------------------
enc_sequence(Version, Sequence, TypeCode, Bytes, Len) ->
    {ByteSequence, Len1} = enc_align(Bytes, Len, 4),
    ByteSequence1 = cdrlib:enc_r_unsigned_long(length(Sequence), ByteSequence),
    enc_sequence1(Version, Sequence, TypeCode, ByteSequence1, Len1 + 4).

%%-----------------------------------------------------------------
%% Func: enc_sequence1/4
%%-----------------------------------------------------------------
enc_sequence1(Version, [], TypeCode, Bytes, Len) ->
    {Bytes, Len};
enc_sequence1(Version, CharSeq, 'tk_char', Bytes, Len) -> 
    {[CharSeq |Bytes], Len + length(CharSeq)};
enc_sequence1(Version, OctetSeq, 'tk_octet', Bytes, Len) -> 
    {[OctetSeq |Bytes], Len + length(OctetSeq)};
enc_sequence1(Version, [Object| Rest], TypeCode, Bytes, Len) -> 
    {ByteSequence, Len1} = enc_type(TypeCode, Version, Object, Bytes, Len),
    enc_sequence1(Version, Rest, TypeCode, ByteSequence, Len1).

%%-----------------------------------------------------------------
%% Func: enc_array/4
%%-----------------------------------------------------------------
enc_array(Version, Array, Size, TypeCode, Bytes, Len) when size(Array) == Size ->
    Sequence = tuple_to_list(Array),
    enc_sequence1(Version, Sequence, TypeCode, Bytes, Len);
enc_array(_,_, _, _, _, _) ->
    corba:raise(#'MARSHAL'{minor=105, completion_status=?COMPLETED_MAYBE}).

%%-----------------------------------------------------------------
%% Func: enc_string/3
%%-----------------------------------------------------------------
enc_string(Version, String, Bytes, Len) ->
    {ByteSequence, Len1} = enc_align(Bytes, Len, 4),
    ByteSequence1 = cdrlib:enc_r_unsigned_long(length(String) + 1,
					       ByteSequence),
    {cdrlib:enc_octet(0, [String | ByteSequence1]), Len1 + length(String) + 5}.

%%-----------------------------------------------------------------
%% Func: enc_union/5
%%-----------------------------------------------------------------
enc_union(Version, {_, Label, Value}, DiscrTC, TypeCodeList, Bytes, Len) ->
    {ByteSequence, Len1} = enc_type(DiscrTC, Version, Label, Bytes, Len),
    Label2 = stringify_enum(DiscrTC,Label),
    enc_union(Version, {Label2, Value},TypeCodeList, ByteSequence, Len1).

enc_union(_,_, [], _, _) -> corba:raise(#'MARSHAL'{minor=106, completion_status=?COMPLETED_MAYBE});
enc_union(Version, {Label,Value} ,[{Label, Name, Type} |List], Bytes, Len) ->
    enc_type(Type, Version, Value, Bytes, Len);
enc_union(Version, Union,[_ | List], Bytes, Len) ->
    enc_union(Version, Union, List, Bytes, Len).

stringify_enum({tk_enum, _,_,_}, Label) ->
    atom_to_list(Label);
stringify_enum(_, Label) ->
    Label.
%%-----------------------------------------------------------------
%% Func: enc_struct/4
%%-----------------------------------------------------------------
enc_struct(Version, Struct, TypeCodeList, Bytes, Len) ->
    [Name | StructList] = tuple_to_list(Struct),
    enc_struct1(Version, StructList, TypeCodeList, Bytes, Len).

enc_struct1(Version, [], [], Bytes, Len) ->
    {Bytes, Len};
enc_struct1(Version, [Object | Rest], [{ElemName, ElemType} | TypeCodeList], Bytes,
	    Len) ->
    {ByteSequence, Len1} = enc_type(ElemType, Version, Object, Bytes, Len),
    enc_struct1(Version, Rest, TypeCodeList, ByteSequence, Len1).

%%-----------------------------------------------------------------
%% Func: enc_objref/4
%%-----------------------------------------------------------------
enc_objref(Version, Name, Value, Bytes, Len) ->
     iop_ior:code(Version, Value, Bytes, Len).
      
%%-----------------------------------------------------------------
%% Func: enc_exception/5
%%-----------------------------------------------------------------
enc_exception(Version, Name, IFRId, Value, ElementList, Bytes, Len) ->
    [Name1, TypeId | Args] = tuple_to_list(Value),
    {Bytes1, Len1} = enc_type({'tk_string', 0}, Version, IFRId , Bytes, Len),
    enc_exception_1(Version, Args, ElementList, Bytes1, Len1).

enc_exception_1(Version, [], [], Bytes, Len) ->
    {Bytes, Len};
enc_exception_1(Version, [Arg |Args], [{ElemName, ElemType} |ElementList],
		Bytes, Len) ->
    {Bytes1, Len1} = enc_type(ElemType, Version, Arg, Bytes, Len),
    enc_exception_1(Version, Args, ElementList, Bytes1, Len1).
    

%%-----------------------------------------------------------------
%% Func: enc_type_code/3
%%-----------------------------------------------------------------
enc_type_code('tk_null', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 0, Message, Len);
enc_type_code('tk_void', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 1, Message, Len);
enc_type_code('tk_short', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 2, Message, Len);
enc_type_code('tk_long', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 3, Message, Len);
enc_type_code('tk_ushort', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 4, Message, Len);
enc_type_code('tk_ulong', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 5, Message, Len);
enc_type_code('tk_float', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 6, Message, Len);
enc_type_code('tk_double', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 7, Message, Len);
enc_type_code('tk_boolean', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 8, Message, Len);
enc_type_code('tk_char', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 9, Message, Len);
enc_type_code('tk_octet', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 10, Message, Len);
enc_type_code('tk_any', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 11, Message, Len);
enc_type_code('tk_TypeCode', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 12, Message, Len);
enc_type_code('tk_Principal', Version, Message, Len) ->
    enc_type('tk_ulong', Version, 13, Message, Len);
enc_type_code({'tk_objref', RepId, Name}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 14, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				    {"name", {'tk_string', 0}}]},
				     Version,
				     {"", RepId, Name},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_struct', RepId, SimpleName, ElementList}, Version, Message, Len) ->
    Name = ifrid_to_name(RepId),
    {Message1, Len1} = enc_type('tk_ulong', Version, 15, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				    {"name", {'tk_string', 0}},
				    {"element list",
				     {'tk_sequence', {'tk_struct', "","",
						      [{"member name", {'tk_string', 0}},
						       {"member type", 'tk_TypeCode'}]},
				      0}}]},
				     Version,
				     {"", RepId, Name,
				      lists:map(fun({N,T}) -> {"",N,T} end, ElementList)},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_union', RepId, Name, DiscrTC, Default, ElementList},
	      Version, Message, Len) ->
    NewElementList =
	case check_enum(DiscrTC) of
	    true ->
		lists:map(fun({L,N,T}) -> {"",list_to_atom(L),N,T} end, ElementList);
	    false ->
		lists:map(fun({L,N,T}) -> {"",L,N,T} end, ElementList)
	end,
    {Message1, Len1} = enc_type('tk_ulong', Version, 16, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				    {"name", {'tk_string', 0}},
				    {"discriminant type", 'tk_TypeCode'},
				    {"default used", 'tk_long'},
				    {"element list",
				     {'tk_sequence', {'tk_struct', "","",
						      [{"label value", DiscrTC},
						       {"memeber name", {'tk_string', 0}},
						       {"memver type", 'tk_TypeCode'}]},
				      0}}]},
				     Version,
				     {"", RepId, Name, DiscrTC, Default, NewElementList},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_enum', RepId, Name, ElementList}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 17, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				    {"name", {'tk_string', 0}},
				    {"element list",
				     {'tk_sequence', {'tk_string', 0}, 0}}]},
				     Version,
				     {"", RepId, Name, ElementList},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_string', MaxLength}, Version, Message, Len) ->
     enc_type({'tk_struct', "", "", [{"TCKind", 'tk_ulong'},
				    {"max length", 'tk_ulong'}]},
	      Version,
	      {"", 18, MaxLength},
	      Message, Len);
enc_type_code({'tk_sequence', ElemTC, MaxLength}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 19, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
     {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"element type", 'tk_TypeCode'},
				    {"max length", 'tk_ulong'}]},
				      Version,
				      {"", ElemTC, MaxLength},
				      Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_array', ElemTC, Length}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 20, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"element type", 'tk_TypeCode'},
				    {"length", 'tk_ulong'}]},
				     Version,
				     {"", ElemTC, Length},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_alias', RepId, Name, TC}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 21, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				     {"name", {'tk_string', 0}},
				     {"TypeCode", 'tk_TypeCode'}]},
				     Version,
				     {"", RepId, Name, TC},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'tk_except', RepId, Name, ElementList}, Version, Message, Len) ->
    {Message1, Len1} = enc_type('tk_ulong', Version, 22, Message, Len),
    {Message2, _} = enc_byte_order(Version, []),
    {ComplexParams, Len2} = enc_type({'tk_struct', "", "", [{"repository ID", {'tk_string', 0}},
				    {"name", {'tk_string', 0}},
				    {"element list",
				     {'tk_sequence',
				      {'tk_struct', "", "",
				       {"member name", {'tk_string', 0}},
				       {"member type", 'tk_TypeCode'}}, 0}}]},
				     Version,
				     {"", RepId, Name,
				      lists:map(fun({N,T}) -> {"",N,T} end, ElementList)},
				     Message2, 1),
    encode_complex_tc_paramters(lists:reverse(ComplexParams), Len2, Message1, Len1);
enc_type_code({'none', Indirection}, Version, Message, Len) ->  %% placeholder
     enc_type({'tk_struct', "", "", [{"TCKind", 'tk_ulong'},
				    {"indirection", 'tk_ulong'}]},
	      Version,
	      {"", 16#ffffffff, Indirection},
	      Message, Len).


check_enum({'tk_enum', _, _, _}) ->
    true;
check_enum(_) ->
    false.

encode_complex_tc_paramters(Value, Value_length, Message, Len) ->
    {Message1, Len1} = enc_align(Message, Len, 4),
    Message2 = cdrlib:enc_r_unsigned_long(Value_length, Message1),
    {[Value |Message2], Len+Value_length+4}.


ifrid_to_name([]) ->
        corba:raise(#'MARSHAL'{minor=107, completion_status=?COMPLETED_MAYBE});
ifrid_to_name(Id) ->
    Rep = orber_ifr:find_repository(),
    Key = orber_ifr:'Repository_lookup_id'(Rep, Id),
    [$:, $: |N] = orber_ifr:'Contained__get_absolute_name'(Key),
    change_colons_to_underscore(N, []).

change_colons_to_underscore([$:, $: | T], Acc) ->
    change_colons_to_underscore(T, [$_ |Acc]);
change_colons_to_underscore([H |T], Acc) ->
    change_colons_to_underscore(T, [H |Acc]);
change_colons_to_underscore([], Acc) ->
    lists:reverse(Acc).

%%-----------------------------------------------------------------
%% Func: enc_align/1
%%-----------------------------------------------------------------
enc_align(R, Len, Alignment) ->
    Rem = Len rem Alignment,
    if Rem == 0 ->
	    {R, Len};
       true ->
	    {add_bytes(R, Alignment - Rem), Len + (Alignment - Rem)}
    end.

add_bytes(R, 0) ->
    R;
add_bytes(R, 1) ->
    [16#01 | R];
add_bytes(R, 2) ->
    [16#02, 16#02 | R];
add_bytes(R, 3) ->
    [16#03, 16#03, 16#03 | R];
add_bytes(R, 4) ->
    [16#04, 16#04, 16#04, 16#04 | R];
add_bytes(R, 5) ->
    [16#05, 16#05, 16#05, 16#05, 16#05 | R];
add_bytes(R, 6) ->
    [16#06, 16#06, 16#06, 16#06, 16#06, 16#06 | R];
add_bytes(R, 7) ->
    [16#07, 16#07, 16#07, 16#07, 16#07, 16#07, 16#07 | R];
add_bytes(R,N) ->
    add_bytes([16#08 | R], N - 1).

