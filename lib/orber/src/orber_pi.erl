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
%%----------------------------------------------------------------------
%% File    : orber_pi.erl
%% Purpose : 
%% Created : 20 Sep 2000
%% Comments:
%% * Each Interceptor is represented as {Module, Args} where
%%              Module - refers to a module which must export the functions:
%%                       (1)  receive_request
%%                       (2)  send_other
%%                       (3)  receive_service_contexts
%%                       (4)  send_reply
%%                       (5)  send_exception
%%                       (6)  send_request
%%                       (7)  send_poll
%%                       (8)  receive_reply
%%                       (9)  receive_exception
%%                       (10) receive_other
%%                       (11) preinvoke
%%                       (12) postinvoke
%%              Args - any argument to be passed on to any of the
%%                     functions above, i.e., (1)-(12).
%% 
%%----------------------------------------------------------------------

-module(orber_pi).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/orber_pi.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%--------------- EXPORTS-------------------------------------
%% API external
-export([server_start_receive/7, 
	 server_start_send/2,
	 preinvoke/2,
	 postinvoke/2,
	 client_receive/2, 
	 client_send/2, 
	 codefactory_create_codec/1,
	 codec_encode/2, 
	 codec_encode_value/2,
	 codec_decode/2, 
	 codec_decode_value/3,
	 %% RequestInfo
	 '_get_request_id'/1,
	 '_get_operation'/1,
	 '_get_arguments'/1,
	 '_get_exceptions'/1,
	 '_get_contexts'/1,
	 '_get_operation_context'/1,
	 '_get_result'/1,
	 '_get_response_expected'/1,
	 '_get_sync_scope'/1,
	 '_get_reply_status'/1,
	 '_get_forward_reference'/1,
	 get_slot/2,
	 get_request_service_context/2,
	 get_reply_service_context/2,
	 %% ClientRequestInfo (inherrits RequestInfo)
	 '_get_target'/1,
	 '_get_effective_target'/1,
	 '_get_effective_profile'/1,
	 '_get_received_exception'/1,
	 '_get_received_exception_id'/1,
	 get_effective_component/2,
	 get_effective_components/2,
	 get_request_policy/2,
	 add_request_service_policy/3,
	 %% ServerRequestInfo (inherrits RequestInfo)
	 '_get_sending_exception'/1,
	 '_get_object_id'/1,
	 '_get_adapter_id'/1,
	 '_get_target_most_derived_interface'/1,
	 get_server_policy/2,
	 set_slot/3,
	 target_is_a/2,
	 add_reply_service_context/3]).

%%=============== DATA STRUCTURES ============================
%%--------------- ClientRequestInfo --------------------------
-record('ClientRequestInfo', 
	{request_id,
	 operation,
	 arguments,
	 exceptions,
	 contexts,
	 operation_context,
	 result,
	 response_expected,
	 sync_scope = 'SYNC_NONE',
	 reply_status,
	 forward_reference,
	 endian,
	 target,
	 effective_target,
	 effective_profile,
	 received_exception,
	 received_exception_id}).

-define(createInitCRI(_ReqID, _Op, _Args, _Ctxs, _OpCtx, _RespExp, _Target, 
		      _ETarget, _EProf),
	#'ClientRequestInfo'{request_id        = _ReqID,
			     operation         = _Op,
			     arguments         = _Args,
			     contexts          = _Ctxs,
			     operation_context = _OpCtx,
			     response_expected = _RespExp,
			     target            = _Target,
			     effective_target  = _ETarget,
			     effective_profile = _EProf}).


%%--------------- ServerRequestInfo --------------------------
-record('ServerRequestInfo',
	{request_id,
	 operation,
	 arguments,
	 exceptions,
	 contexts,
	 operation_context,
	 result,
	 response_expected,
	 sync_scope = 'SYNC_NONE',
	 reply_status,
	 forward_reference,
	 endian,
	 sending_exception,
	 object_id,
	 adapter_id,
	 target_most_derived_interface}).

-define(createInitSRI(_ReqID, _Op, _RespExp, _SyncScoope),
	#'ServerRequestInfo'{request_id        = _ReqID,
			     operation         = _Op,
			     response_expected = _RespExp}).


%%--------------- DEFINES ------------------------------------
-define(is_SystemExc(S), corba:check_exception_type(S) == ?SYSTEM_EXCEPTION).

-define(write_ErrorMsg(Txt, Arg),
error_logger:error_msg("========== PortableInterceptors ===========~n"
		       Txt
		       "===========================================~n",
		       Arg)).

-ifdef(debug).
-define(debug_print(F,A),
        io:format("[LINE: ~p MODULE: ~p] "++F,[?LINE, ?MODULE]++A)).
-else.
-define(debug_print(F,A), ok).
-endif.    

%%------------------------------------------------------------
%% function : codefactory_create_codec
%% Arguments: #IOP_N_Encoding{}
%% Returns  : CodecRef
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codefactory_create_codec(#'IOP_N_Encoding'{format = 'IOP_N_ENCODING_CDR_ENCAPS', 
					   major_version = Major, 
					   minor_version = Minor}) 
  when integer(Major), integer(Minor) ->
    {Major, Minor};
codefactory_create_codec(_) ->
    corba:raise(#'IOP_N_CodecFactory_UnknownEncoding'{}).

%%------------------------------------------------------------
%% function : codec_encode
%% Arguments: Version - GIOP version
%%            Bytes - Any - #any{}
%% Returns  : CORBA::OctetSeq
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_encode(Version, Any) when record(Any, any) ->
    cdr_encode:enc_type(Version, 'tk_any', Any);
codec_encode(Version, Any) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_encode_value
%% Arguments: Version - GIOP version
%%            Bytes - Any - #any{}
%% Returns  : CORBA::OctetSeq
%% Exception: 
%% Effect   : Encode the Any#any.value only.
%%------------------------------------------------------------
codec_encode_value(Version, #any{typecode = TC, value = Val}) ->
    cdr_encode:enc_type(Version, TC, Val);
codec_encode_value(Version, NotAnAny) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_decode
%% Arguments: Version - GIOP version
%%            Bytes - CORBA::OctetSeq
%% Returns  : Any - #any{}
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_decode(Version, Bytes) when list(Bytes) ->
    case catch cdr_decode:dec_type('tk_any', Version, Bytes, 0, big) of
	{Any, [], _} ->
	    Any;
	_->
	    corba:raise(#'IOP_N_Codec_FormatMismatch'{})
    end;
codec_decode(Version, Any) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%% function : codec_decode_value
%% Arguments: Version - GIOP version
%%            Bytes - CORBA::OctetSeq
%%            TypeCode - CORBA::TypeCode
%% Returns  : Any - #any{}
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
codec_decode_value(Version, Bytes, TypeCode) when list(Bytes) ->
    case catch cdr_decode:dec_type(TypeCode, Version, Bytes, 0, big) of
	{Val, [], _} ->
	    #any{typecode = TypeCode, value = Val};
	_->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
codec_decode_value(Version, Bytes, TypeCode) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%------------------------------------------------------------
%%------------- SERVER SIDE FUNCTIONS ------------------------
%%------------------------------------------------------------
%% To make a long story short, you find an conceptual description
%% of how, and in which order, the different functions is 
%% supposed to be invoked.
%%
%%request_from_iiop(Bytes) -> 
%%    Reply = 
%%	case receive_service_contexts(ServerRequestInfo) of 
%%	    SYSTEM EXC -> 
%%		send_exception(..);
%%	    ForwardRequest EXC -> 
%%		send_other(..);
%%	    NoEXC -> 
%%		case receive_request(..) of
%%		    SYSTEM EXC -> 
%%			send_exception(..);
%%		    ForwardRequest EXC -> 
%%			send_other(..);
%%		    No EXC -> 
%%			InvokeServer
%%		end
%%	end,
%%    case Reply of
%%	EXC -> 
%%	    send_exception(..);
%%	No EXC, Normal Reply -> 
%%	    case send_reply(..) of
%%		SYSTEM EXC -> 
%%		    send_exception(..);
%%		ForwardRequest EXC -> 
%%		    send_other(..);
%%		No Exc -> 
%%		    Done
%%	    end;
%%	No EXC, LOCATION_FORWARD ->  
%%	    send_other(..) 
%%    end.
%% 
%% 
%%------------------------------------------------------------
%% function : server_start_receive
%% Arguments: Msg - #giop_message{}
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
server_start_receive({basic, PIs}, Version, ReqHdr, Rest, Len, ByteOrder, Msg) ->
    %% Invoke the Interceptors so they can manipulate the message.
    NewRest = preinvoke(Rest, PIs),
    cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg);
server_start_receive({portable, PIs}, Version, ReqHdr, Rest, Len, ByteOrder, Msg) ->
    cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg);
server_start_receive({both, PIs}, Version, ReqHdr, Rest, Len, ByteOrder, Msg) ->
    %% Invoke the Interceptors so they can manipulate the message.
    NewRest = preinvoke(Rest, PIs),
    {Version, ReqHdr, Parameters, TypeCodes} = 
	cdr_decode:dec_request_body(Version, ReqHdr, NewRest, Len, ByteOrder, Msg),

    %% FIX ME!!! Create a request ID (cannot use 0).
    SRI = ?createInitSRI(0,0,0,0),
    server_receive(receive_service_contexts, SRI, PIs, [], PIs).

server_receive(receive_service_contexts, SRI, [], Acc, PIs) ->
    server_receive(receive_request, SRI, PIs, [], PIs);
server_receive(receive_service_contexts, SRI, [H|T], Acc, PIs) ->
    case catch receive_service_contexts(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_receive(receive_service_contexts, SRI, T, Acc, PIs)
    end;
server_receive(receive_request, SRI, [], Acc, PIs) ->
    %% Done with receive interceptors, now we can call the server.
    SRI;
server_receive(receive_request, SRI, [H|T], Acc, PIs)  ->
    case catch receive_request(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_receive(receive_request, SRI, T, Acc, PIs)
    end.


%%------------------------------------------------------------
%% function : server_start_send
%% Arguments: SRI - ServerRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------
server_start_send({basic, PIs}, SRI) ->
    SRI;
server_start_send(PIs, SRI) ->
    case SRI#'ServerRequestInfo'.reply_status of
	'PortableInterceptor_SUCCESSFUL' ->
	    server_send(send_reply, SRI, PIs, [], PIs);
	'PortableInterceptor_SYSTEM_EXCEPTION' ->
	    server_send(send_exception, SRI, PIs, [], PIs);
	'PortableInterceptor_USER_EXCEPTION' ->
	    server_send(send_exception, SRI, PIs, [], PIs);
	_ ->
	    server_send(send_other, SRI, PIs, [], PIs)
    end.

server_send(_, SRI, [], Acc, PIs) ->
    %% Done
    SRI;
server_send(send_exception, SRI, [H|T], Acc, PIs) ->
    case catch send_exception(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    server_send(send_other, SRI, Acc, [], PIs);
	{'EXCEPTION', E} ->
	    server_send(send_exception, SRI, Acc, [], PIs);
	_ ->
	    server_send(send_exception, SRI, T, Acc, PIs)
    end;
server_send(send_other, SRI, [H|T], Acc, PIs) ->
    case catch send_other(SRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    server_send(send_other, SRI, T, Acc, PIs);
	{'EXCEPTION', E} ->
	    server_send(send_exception, SRI, T, Acc, PIs);
	_ ->
	    server_send(send_other, SRI, T, Acc, PIs)
    end;
server_send(send_reply, SRI, [H|T], Acc, PIs) ->
    case catch send_reply(SRI, H) of
	{'EXCEPTION', E} ->
	    server_send(send_exception, SRI, T, Acc, PIs);
	_ ->
	    server_send(send_reply, SRI, T, Acc, PIs)
    end.

receive_request(SRI, Mod) ->
    apply(Mod, receive_request, SRI).

send_other(SRI, Mod) ->
    apply(Mod, send_other, SRI).

receive_service_contexts(SRI, Mod) ->
    apply(Mod, receive_service_contexts, SRI).

send_reply(SRI, Mod) ->
    apply(Mod, send_reply, SRI).

send_exception(SRI, Mod) ->
    apply(Mod, send_exception, SRI).

preinvoke([], Msg) ->
    Msg;
preinvoke([Mod|T], Msg) ->
    NewMsg = apply(Mod, preinvoke, [Msg]),
    preinvoke(T, NewMsg).

postinvoke([], Msg) ->
    Msg;
postinvoke([Mod|T], Msg) ->
    NewMsg = apply(Mod, postinvoke, [Msg]),
    postinvoke(T, NewMsg).

%%------------------------------------------------------------
%%------------- CLIENT SIDE FUNCTIONS ------------------------
%%------------------------------------------------------------
%% To make a long story short, you find an conceptual description
%% of how, and in which order, the different functions is 
%% supposed to be invoked.
%%
%%request(Data) -> 
%%    Reply = 
%%	case send_request(CRI) of 
%%	    SYSTEM EXC -> 
%%		receive_exception(..);
%%	    ForwardRequest EXC -> 
%%		receive_other(..);
%%	    NoEXC -> 
%%		IIOP-send
%%	end,
%%    case Reply of
%%	EXC -> 
%%	    receive_exception(..); May raise system exc => receive_other(..);
%%	No EXC, Normal Reply -> 
%%	    receive_reply(..) May raise system exc => receive_exception(..);
%%	Non-normal reply (e.g. LOCATION_FORWARD) ->  
%%	    receive_other(..) May raise system exc => receive_exception(..);
%%    end.
%%------------------------------------------------------------
%% function : client_send
%% Arguments: CRI - ClientRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

client_send(CRI, PIs) ->
    client_send(send_request, CRI, PIs, [], PIs).

client_send(send_request, CRI, [], _, _) ->
    CRI;
client_send(send_request, CRI, [H|T], Acc, PIs) ->
    case catch send_request(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', E} ->
	    client_receive(receive_exception, CRI, Acc, [], PIs);
	_ ->
	    client_send(send_request, CRI, T, Acc, PIs)
    end.
	


%%------------------------------------------------------------
%% function : client_receive
%% Arguments: CRI - ClientRequestInfo
%%            PIs - a list of Interceptors (see 'Comments' in the module header)
%% Returns  : 
%% Exception: 
%% Effect   : 
%%------------------------------------------------------------

client_receive(CRI, PIs) ->
    case CRI#'ClientRequestInfo'.reply_status of
	'PortableInterceptor_SUCCESSFUL' ->
	    client_receive(receive_reply, CRI, PIs, [], PIs);
	'PortableInterceptor_SYSTEM_EXCEPTION' ->
	    client_receive(receive_exception, CRI, PIs, [], PIs);
	'PortableInterceptor_USER_EXCEPTION' ->
	    client_receive(receive_exception, CRI, PIs, [], PIs);
	_ ->
	    client_receive(receive_other, CRI, PIs, [], PIs)
    end.

client_receive(_, CRI, [], _, _) ->
    %% Done
    CRI;
client_receive(receive_reply, CRI, [H|T], Acc, PIs) ->
    case catch receive_reply(CRI, H) of
	{'EXCEPTION', E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_reply, CRI, T, [H|Acc], PIs)
    end;
client_receive(receive_exception, CRI, [H|T], Acc, PIs) ->
    case catch receive_exception(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs)
    end;
client_receive(receive_other, CRI, [H|T], Acc, PIs) ->
    case catch receive_other(CRI, H) of
	{'EXCEPTION', #'PortableInterceptor_ForwardRequest'{forward=Obj, 
							    permanent=Bool}} ->
	    client_receive(receive_other, CRI, T, [], PIs);
	{'EXCEPTION', E} ->
	    client_receive(receive_exception, CRI, T, [H|Acc], PIs);
	_ ->
	    client_receive(receive_other, CRI, T, [H|Acc], PIs)
    end.



send_request(CRI, Mod) ->
    apply(Mod, send_request, CRI).

receive_reply(CRI, Mod) ->
    apply(Mod, receive_reply, CRI).

receive_other(CRI, Mod) ->
    apply(Mod, receive_other, CRI).

receive_exception(CRI, Mod) ->
    apply(Mod, receive_exception, CRI).

%%------------------------------------------------------------
%% Functions for retrieving info from RequestInfo
%% ServerRequestInfo and ClientRequestInfo. The ones matching
%% both ServerRequestInfo and ClientRequestInfo eq. RequestInfo.
%% Note, RequestInfo is inherrited by the others.
%%------------------------------------------------------------
%%-----------------------------------------------------------%
%% function : _get_request_id
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : ulong()
%%------------------------------------------------------------
'_get_request_id'(#'ClientRequestInfo'{request_id = ID}) ->
    ID;
'_get_request_id'(#'ServerRequestInfo'{request_id = ID}) ->
    ID.

%%-----------------------------------------------------------%
%% function : _get_operation
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : string()
%%------------------------------------------------------------
'_get_operation'(#'ClientRequestInfo'{operation = Op}) ->
    Op;
'_get_operation'(#'ServerRequestInfo'{operation = Op}) ->
    Op.

%%-----------------------------------------------------------%
%% function : _get_arguments
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of #'Dynamic_Parameter'{}
%%------------------------------------------------------------
'_get_arguments'(#'ClientRequestInfo'{arguments = Args}) ->
    Args;
'_get_arguments'(#'ServerRequestInfo'{arguments = Args}) ->
    Args.

%%-----------------------------------------------------------%
%% function : _get_exceptions
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::TypeCode
%%------------------------------------------------------------
'_get_exceptions'(#'ClientRequestInfo'{exceptions = Exc}) ->
    Exc;
'_get_exceptions'(#'ServerRequestInfo'{exceptions = Exc}) ->
    Exc.

%%-----------------------------------------------------------%
%% function : _get_contexts
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::StringSeq
%%------------------------------------------------------------
'_get_contexts'(#'ClientRequestInfo'{contexts = Ctx}) ->
    Ctx;
'_get_contexts'(#'ServerRequestInfo'{contexts = Ctx}) ->
    Ctx.

%%-----------------------------------------------------------%
%% function : _get_operation_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : A list of CORBA::StringSeq
%%------------------------------------------------------------
'_get_operation_context'(#'ClientRequestInfo'{operation_context = OpCtx}) ->
    OpCtx;
'_get_operation_context'(#'ServerRequestInfo'{operation_context = OpCtx}) ->
    OpCtx.

%%-----------------------------------------------------------%
%% function : _get_result
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_result'(#'ClientRequestInfo'{result = Res}) ->
    Res;
'_get_result'(#'ServerRequestInfo'{result = Res}) ->
    Res.

%%-----------------------------------------------------------%
%% function : _get_response_expected
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : boolean()
%%------------------------------------------------------------
'_get_response_expected'(#'ClientRequestInfo'{response_expected = Bool}) ->
    Bool;
'_get_response_expected'(#'ServerRequestInfo'{response_expected = Bool}) ->
    Bool.

%%-----------------------------------------------------------%
%% function : _get_sync_scope
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : Messaging::SyncScoope ('SYNC_NONE', 'SYNC_WITH_TRANSPORT', 
%%            'SYNC_WITH_SERVER', 'SYNC_WITH_TARGET')
%%------------------------------------------------------------
'_get_sync_scope'(#'ClientRequestInfo'{sync_scope = SS}) ->
    SS;
'_get_sync_scope'(#'ServerRequestInfo'{sync_scope = SS}) ->
    SS.

%%-----------------------------------------------------------%
%% function : _get_reply_status
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : ReplyStatus (short), defined in orber_pi.hrl
%%------------------------------------------------------------
'_get_reply_status'(#'ClientRequestInfo'{reply_status = RS}) ->
    RS;
'_get_reply_status'(#'ServerRequestInfo'{reply_status = RS}) ->
    RS.

%%-----------------------------------------------------------%
%% function : _get_forward_reference
%% Arguments: ClientRequestInfo or ServerRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_forward_reference'(#'ClientRequestInfo'{forward_reference = FR}) ->
    FR;
'_get_forward_reference'(#'ServerRequestInfo'{forward_reference = FR}) ->
    FR.

%%------------------------------------------------------------
%% function : get_slot
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            SlotId - ulong()
%% Returns  : {'EXCEPTION', #'PortableInterceptor_InvalidSlot'{}}
%%------------------------------------------------------------
get_slot(XRI, SlotId) ->
    corba:raise(#'PortableInterceptor_InvalidSlot'{}).

%%------------------------------------------------------------
%% function : get_request_service_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            ServiceId - IOP::ServiceId (defined in orber_iiop.hrl)
%% Returns  : IOP::ServiceContext
%%------------------------------------------------------------
get_request_service_context(#'ClientRequestInfo'{contexts = Ctx}, ServiceId) ->
    Ctx;
get_request_service_context(#'ServerRequestInfo'{contexts = Ctx}, ServiceId) ->
    Ctx.

%%------------------------------------------------------------
%% function : get_reply_service_context
%% Arguments: ClientRequestInfo or ServerRequestInfo
%%            ServiceId - IOP::ServiceId (defined in orber_iiop.hrl)
%% Returns  :  IOP::ServiceContext
%%------------------------------------------------------------
get_reply_service_context(#'ClientRequestInfo'{contexts = Ctx}, ServiceId) ->
    Ctx;
get_reply_service_context(#'ServerRequestInfo'{contexts = Ctx}, ServiceId) ->
    Ctx.
    
%%------------------------------------------------------------
%%-------------- ClientRequestInfo only ----------------------
%%-----------------------------------------------------------%
%% function : _get_target
%% Arguments: ClientRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_target'(#'ClientRequestInfo'{target = Target}) ->
    Target.

%%-----------------------------------------------------------%
%% function : _get_effective_target
%% Arguments: ClientRequestInfo
%% Returns  : Object
%%------------------------------------------------------------
'_get_effective_target'(#'ClientRequestInfo'{effective_target = ET}) ->
    ET.

%%-----------------------------------------------------------%
%% function : _get_effective_profile
%% Arguments: ClientRequestInfo
%% Returns  : IOP:TaggedProfile
%%------------------------------------------------------------
'_get_effective_profile'(#'ClientRequestInfo'{effective_profile = EP}) ->
    EP.

%%-----------------------------------------------------------%
%% function : _get_received_exception
%% Arguments: ClientRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_received_exception'(#'ClientRequestInfo'{received_exception = RE}) ->
    RE.

%%-----------------------------------------------------------%
%% function : _get_received_exception
%% Arguments: ClientRequestInfo
%% Returns  : CORBA::RepositoryId
%%------------------------------------------------------------
'_get_received_exception_id'(#'ClientRequestInfo'{received_exception_id = REId}) ->
    REId.

%%------------------------------------------------------------
%% function : get_effective_component
%% Arguments: ClientRequestInfo
%% Returns  : IOR::TaggedComponent
%%------------------------------------------------------------
get_effective_component(#'ClientRequestInfo'{target = Target}, Id) ->
    Target.

%%------------------------------------------------------------
%% function : get_effective_components
%% Arguments: ClientRequestInfo
%%            Id -IOP::ComponentId (ulong())
%% Returns  : IOP_N::TaggedComponentSeq
%%------------------------------------------------------------
get_effective_components(#'ClientRequestInfo'{target = Target}, Id) ->
    Target.

%%------------------------------------------------------------
%% function : get_request_policy
%% Arguments: ClientRequestInfo
%%            Type - CORBA::PolicyType
%% Returns  : IOP_N::TaggedComponentSeq
%%------------------------------------------------------------
get_request_policy(#'ClientRequestInfo'{target = Target}, Type) ->
    Target.

%%------------------------------------------------------------
%% function : add_request_service_context
%% Arguments: ClientRequestInfo
%%            Ctx - IOP::ServiceContext
%%            Replace - boolean()
%% Returns  : -
%%------------------------------------------------------------
add_request_service_policy(#'ClientRequestInfo'{target = Target}, Ctx, Replace) ->
    ok.

%%------------------------------------------------------------
%%-------------- ServerRequestInfo only ----------------------
%%-----------------------------------------------------------%
%% function : _get_sending_exception
%% Arguments: ServerRequestInfo
%% Returns  : #any{}
%%------------------------------------------------------------
'_get_sending_exception'(#'ServerRequestInfo'{sending_exception = Exc}) ->
    Exc.

%%-----------------------------------------------------------%
%% function : _get_object_id
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::OctetSeq
%%------------------------------------------------------------
'_get_object_id'(#'ServerRequestInfo'{object_id = OI}) ->
    OI.

%%-----------------------------------------------------------%
%% function : _get_adapter_id
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::OctetSeq
%%------------------------------------------------------------
'_get_adapter_id'(#'ServerRequestInfo'{adapter_id = AI}) ->
    AI.

%%-----------------------------------------------------------%
%% function : _get_target_most_derived_interface
%% Arguments: ServerRequestInfo
%% Returns  : CORBA::RepositoryId
%%------------------------------------------------------------
'_get_target_most_derived_interface'(#'ServerRequestInfo'
				     {target_most_derived_interface = TMDI}) ->
    TMDI.

%%------------------------------------------------------------
%% function : get_server_policy
%% Arguments: ServerRequestInfo
%%            PolicyType - CORBA::PolicyType
%% Returns  : CORBA::Policy
%%------------------------------------------------------------
get_server_policy(#'ServerRequestInfo'{contexts = Ctxs}, PolicyType) ->
    Ctxs.

%%------------------------------------------------------------
%% function : set_slot
%% Arguments: ServerRequestInfo
%%            SlotId - ulong()
%%            Data - #any{}
%% Returns  : {'EXCEPTION', #'PortableInterceptor_InvalidSlot'{}}
%%------------------------------------------------------------
set_slot(SRI, SlotId, Data) ->
    corba:raise(#'PortableInterceptor_InvalidSlot'{}).

%%------------------------------------------------------------
%% function : target_is_a
%% Arguments: ServerRequestInfo
%%            IFRId - CORBA::RepositoryId
%% Returns  : boolean()
%%------------------------------------------------------------
target_is_a(#'ServerRequestInfo'{object_id = ObjId}, IFRId) ->
    corba_object:is_a(ObjId, IFRId).

%%------------------------------------------------------------
%% function : add_reply_service_context
%% Arguments: ServerRequestInfo
%%            Ctx - IOP::ServiceContext
%%            Replace - boolean()
%% Returns  : -
%%------------------------------------------------------------
add_reply_service_context(#'ServerRequestInfo'{contexts = Ctxs}, Ctx, Replace) ->
    Ctxs.


%%--------------- END OF MODULE ------------------------------
