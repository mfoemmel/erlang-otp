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
%%--------------------------------------------------------------------
%% File: orber_iiop_inrequest.erl
%% 
%% Description:
%%    This file contains the handling of incomming requests
%%
%% Creation date: 990426
%%
%%-----------------------------------------------------------------
-module(orber_iiop_inrequest).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/orber_pi.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/8, start_fragment_collector/11]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_message/8, fragment_collector/11]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(GIOPHdr, Message, Type, Socket, Interceptors, SSLPort, PartialSec, Flags) ->
    spawn_link(orber_iiop_inrequest, handle_message, 
	       [GIOPHdr, Message, Type, Socket, Interceptors, SSLPort, 
		PartialSec, Flags]).

start_fragment_collector(GIOPHdr, Message, Type, Socket, Interceptors, 
			 ReqId, Proxy, SSLPort, PartialSec, MaxFrags, Flags) ->
    spawn_link(orber_iiop_inrequest, fragment_collector, 
	       [GIOPHdr, Message, Type, Socket, Interceptors, 
		ReqId, Proxy, SSLPort, PartialSec, MaxFrags, Flags]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: fragment_collector/4
%%-----------------------------------------------------------------
fragment_collector(GIOPHdr, Bytes, SocketType, Socket, Interceptors, 
		   ReqId, Proxy, SSLPort, PartialSec, MaxFrags, Flags) ->
    case catch collect(Proxy, [], GIOPHdr#giop_message.byte_order, ReqId, 
		       MaxFrags, 0) of
	{ok, Buffer} ->
	    NewGIOP = GIOPHdr#giop_message
			{message = list_to_binary([GIOPHdr#giop_message.message|Buffer])},
	    %% NOTE, the third argument to dec_message_header must be complete
	    %% message (i.e. AllBytes), otherwise we cannot handle indirection.
	    case handle_message(NewGIOP, list_to_binary([Bytes| Buffer]), 
				SocketType, Socket, Interceptors, SSLPort, 
				PartialSec, Flags) of
		message_error ->
		    Proxy ! {message_error, self(), ReqId},
		    ok;
		_ ->
		    ok
	    end;
	ok ->
	    ok;
	{'EXCEPTION', E} ->
	    Proxy ! {message_error, self(), ReqId},
	    Reply = marshal_exception(GIOPHdr#giop_message.giop_version, ReqId,
				      E, enc_reply, []),
	    orber_socket:write(SocketType, Socket, Reply)
    end.



collect(_Proxy, _Buffer, _ByteOrder, _ReqId, MaxFrags, MaxFrags) ->
    orber:dbg("[~p] ~p:collect(~p)~nMax fragments limit reached.", 
	      [?LINE, ?MODULE, MaxFrags], ?DEBUG_LEVEL),
    {'EXCEPTION', #'IMP_LIMIT'{completion_status=?COMPLETED_NO}};
collect(Proxy, Buffer, ByteOrder, ReqId, MaxFrags, FragCounter) ->
    receive
        {Proxy, #giop_message{byte_order = ByteOrder,
			      message    = Message,
			      fragments  = true} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
            collect(Proxy, [FragBody | Buffer], ByteOrder, ReqId, 
		    MaxFrags, FragCounter+1);
	{Proxy, #giop_message{byte_order = ByteOrder,
			      message = Message,
			      fragments = false} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
	    {ok, lists:reverse([FragBody | Buffer])};
	{Proxy, GIOPHdr, _Data, _} ->
	    orber:dbg("[~p] orber_iiop_inrequest:collect(~p, ~p)~n"
		      "Incorrect Fragment. Might be different byteorder.", 
		      [?LINE, ByteOrder, GIOPHdr], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'MARSHAL'{completion_status=?COMPLETED_NO}};
	{Proxy, cancel_request_header} ->
	    ok;
	Other ->
	    orber:dbg("[~p] ~p:collect(~p)~n"
		      "Unable to collect all fragments: ~p", 
		      [?LINE, ?MODULE, Buffer, Other], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'MARSHAL'{completion_status=?COMPLETED_NO}}
    end.


%%-----------------------------------------------------------------
%% Func: handle_message/4
%%-----------------------------------------------------------------
handle_message(GIOPHdr, Message, SocketType, Socket, Interceptors, SSLPort, 
	       PartialSec, Flags) ->
    %% Warning. We shouldn't set the flags like this here. But, for now, we'll
    %% do it due to performance reasons.
    put(oe_orber_flags, Flags),
    case catch cdr_decode:dec_message_header(null, GIOPHdr, Message) of
	Hdr when record(Hdr, cancel_request_header) ->
            %% We just skips this message for the moment, the standard require that 
	    %% the client handles the reply anyway.
	    message_error;
	{location_forward, Object, ReqId, Version, OldObj} ->
	    Reply = call_interceptors_out(Version, ReqId, [Object], OldObj,
					  Interceptors, 'location_forward', 
					  "location_forward",
					  {{'tk_objref', "", ""}, [],[]}, []),
	    orber_socket:write(SocketType, Socket, Reply);
	{object_forward, Object, ReqId, Version, _OldObj} ->
	    Reply = handle_locate_request(Version, 
					  {object_forward, Object, ReqId}),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, Hdr} when record(Hdr, locate_request_header) ->
	    Reply = handle_locate_request(Version, Hdr),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, ReqHdr, Rest, Len, ByteOrder} when record(ReqHdr, request_header) ->
	    handle_request(Interceptors, Version, ReqHdr, Rest, Len, ByteOrder, 
			   SocketType, Socket, Message, SSLPort, PartialSec);
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Decoding Msg Header failed: ~p", 
		      [?LINE, Message, Other], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply),
	    message_error
   end.


send_reply(oneway, _SocketType, _Socket) ->
    ok;
send_reply(Reply, SocketType, Socket) ->
    orber_socket:write(SocketType, Socket, Reply).

%%-----------------------------------------------------------------
%% Func: handle_request
%%-----------------------------------------------------------------
handle_request(false, Version, ReqHdr, Rest, Len, ByteOrder, SocketType, Socket,
	       Message, SSLPort, PartialSec) ->
    Ctx = check_context(ReqHdr#request_header.service_context, []),
    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, 
		     enc_reply, Ctx) of
	{error, E} ->
	    orber_socket:write(SocketType, Socket, E);
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, SSLPort, PartialSec),
	    Reply  = evaluate(Version, Hdr, Result, TypeCodes, 
			      enc_reply, 'no_exception', Ctx),
	    send_reply(Reply, SocketType, Socket)
    end;
handle_request(Interceptors, Version, ReqHdr, Rest, Len, ByteOrder, 
	       SocketType, Socket, Message, SSLPort, PartialSec) ->
    Ctx = check_context(ReqHdr#request_header.service_context, []),
    case catch call_interceptors(SocketType, Interceptors, Version, ReqHdr, 
				 Rest, Len, ByteOrder, Message, SSLPort, 
				 PartialSec, Ctx) of
	{error, E} ->
	    %% Failed to decode body.
	    orber_socket:write(SocketType, Socket, E);
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Invoking the interceptors resulted in: ~p", 
		      [?LINE, Message, Exc], ?DEBUG_LEVEL),
	    Reply = marshal_exception(Version, 
				      ReqHdr#request_header.request_id, 
				      Exc, enc_reply, Ctx),
	    orber_socket:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)~n"
		      "Invoking the interceptors resulted in: ~p", 
		      [?LINE, ReqHdr, R], ?DEBUG_LEVEL),
	    Reply = marshal_exception(Version, 
				      ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_MAYBE}, 
				      enc_reply, Ctx),
	    orber_socket:write(SocketType, Socket, Reply);
	Reply ->
	    send_reply(Reply, SocketType, Socket)
    end.

check_context([], Acc) ->
    Acc;
check_context([#'CSI_SASContextBody'
	       {label = ?CSI_MsgType_MTEstablishContext, 
		value = #'CSI_EstablishContext'
		{client_context_id = _Id, 
		 authorization_token = _AuthToken,
		 identity_token = _IdToken, 
		 client_authentication_token = _CAuthToken}}|Rest], Acc) ->
    check_context(Rest, [#'IOP_ServiceContext'
			 {context_id=?IOP_SecurityAttributeService,
			  context_data = #'CSI_SASContextBody'
			  {label = ?CSI_MsgType_MTCompleteEstablishContext, 
			   value = #'CSI_CompleteEstablishContext'
			   {client_context_id = 0, 
			    context_stateful = false,
			    final_context_token = [0,255]}}}|Acc]);
check_context([_|Rest], Acc) ->
    check_context(Rest, Acc).
    

%%-----------------------------------------------------------------
%% Func: call_interceptors
%%-----------------------------------------------------------------
call_interceptors(SocketType, {native, Ref, PIs}, Version, ReqHdr, Rest, 
		  Len, ByteOrder, Msg, SSLPort, PartialSec, Ctx) ->
    NewRest = orber_pi:in_request_enc(PIs, ReqHdr, Ref, Rest),
    case decode_body(Version, ReqHdr, NewRest, Len, ByteOrder, Msg, enc_reply, Ctx) of
	{Version, Hdr, Par, TypeCodes} ->
	    NewPar = orber_pi:in_request(PIs, ReqHdr, Ref, Par),
	    ResultInv = invoke_request(Hdr, NewPar, SocketType, TypeCodes, SSLPort, 
				       PartialSec),
	    Result = orber_pi:out_reply(PIs, ReqHdr, Ref, ResultInv, Ctx),
	    
            case evaluate(Version, ReqHdr, Result, TypeCodes, enc_reply_split,
			  'no_exception', Ctx) of
		{ReplyHdr, Reply, HdrL, _BodyL, Flags} ->
		    NewReply = orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply, Ctx),
		    MessSize = HdrL+size(NewReply),
		    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
						       MessSize, [ReplyHdr|NewReply]);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end;
call_interceptors(SocketType, {portable, _PIs}, Version, ReqHdr, Rest, Len, 
		  ByteOrder, Msg, SSLPort, PartialSec, Ctx) ->
    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg, enc_reply, Ctx) of
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, SSLPort, 
				    PartialSec),
	    evaluate(Version, ReqHdr, Result, TypeCodes, enc_reply, 'no_exception', Ctx);
	Other ->
	    Other
    end.

%%-----------------------------------------------------------------
%% Func: call_interceptors_out
%%-----------------------------------------------------------------
call_interceptors_out(Version, ReqId, Result, Obj, {native, Ref, PIs}, Type,
		      Operation, TypeCodes, Ctx) ->
    ReqHdr = #request_header{object_key = Obj,
			     service_context = Ctx,
			     response_expected = true,
			     request_id = ReqId,
			     operation = Operation},
    NewResult = (catch orber_pi:out_reply(PIs, ReqHdr, Ref, Result, Ctx)),
    {ReplyHdr, Reply, HdrL, _BodyL, Flags} = 
	evaluate(Version, ReqHdr, NewResult, TypeCodes, enc_reply_split, Type, Ctx),
    NewReply = 
	case catch orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply, Ctx) of
	    {'EXCEPTION', Exception} ->
		%% Since evaluate don't need TypeCodes or Status no need to supply
		%% them.
		evaluate(Version, ReqHdr, {'EXCEPTION', Exception}, undefined, 
			 enc_reply_split, undefined, Ctx);
	    {'EXIT', E} ->
		orber:dbg("[~p] orber_iiop_inrequest:handle_location_forward(~p)~n"
			  "Resulted in exit: ~p", [?LINE, PIs, E], ?DEBUG_LEVEL),
		marshal_exception(Version, ReqId,
				  #'MARSHAL'{completion_status=?COMPLETED_NO}, 
				  enc_reply, Ctx);
	    R ->
		R
	end,
    MessSize = HdrL+size(NewReply),
    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
				       MessSize, [ReplyHdr|NewReply]);
call_interceptors_out(Version, ReqId, Result, _Obj, {portable, _PIs}, 
		      Type, _, TypeCodes, Ctx) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Version, Hdr, Result, TypeCodes, enc_reply, Type, Ctx);
call_interceptors_out(Version, ReqId, Result, _Obj, _, Type, _, TypeCodes, Ctx) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Version, Hdr, Result, TypeCodes, enc_reply, Type, Ctx).


%%-----------------------------------------------------------------
%% Func: decode_body/2
%%-----------------------------------------------------------------
decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, Func, Ctx) ->
    case catch cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, 
					   ByteOrder, Message) of
	{Version, ReqHdr, Par, TypeCodes} ->
	    {Version, ReqHdr, Par, TypeCodes};
	{'EXCEPTION', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:decode_body(~p, ~p)~n"
		      "Failed decoding request body: ~p", 
		      [?LINE, ReqHdr, Message, E], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, ReqHdr#request_header.request_id,
				      E, Func, Ctx)};
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:decode_body(~p, ~p)~n"
		      "Failed decoding request body: ~p", 
		      [?LINE, ReqHdr, Message, Other], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO},
				      Func, Ctx)}
    end.
    

%%-----------------------------------------------------------------
%% Func: handle_locate_request/2
%%-----------------------------------------------------------------
handle_locate_request(Version, {object_forward, Object, ReqId}) ->
    case catch cdr_encode:enc_locate_reply(Version, ReqId, 'object_forward', 
					   {'tk_objref', "", ""}, Object) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)~n"
		      "Raised the exception: ~p", [?LINE, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, ReqId, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)~n"
		      "Resulted in exit: ~p", [?LINE, E], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, ReqId,
				     #'MARSHAL'{completion_status=?COMPLETED_NO});
	R ->
	    R
    end;
handle_locate_request(Version, Hdr) ->
    Location = orber_objectkeys:check(Hdr#locate_request_header.object_key),
    case catch cdr_encode:enc_locate_reply(Version, Hdr#locate_request_header.request_id,
					   Location) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)~n"
		      "Raised the exception: ~p", 
		      [?LINE, Location, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, Hdr#locate_request_header.request_id, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)~n"
		      "Resulted in exit: ~p", [?LINE, Location, E], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, Hdr#locate_request_header.request_id,
				     #'MARSHAL'{completion_status=?COMPLETED_NO});
	R ->
	    R
    end.
  
%%-----------------------------------------------------------------
%% Func: invoke_request/2
%%-----------------------------------------------------------------
invoke_request(Hdr, Par, normal, TypeCodes, SSLPort, _PartialSec) ->
    Result = 
	case SSLPort of
	    -1 ->
		corba:request_from_iiop(Hdr#request_header.object_key,
					Hdr#request_header.operation,
					Par, [], Hdr#request_header.response_expected,
					Hdr#request_header.service_context);
	    _ ->
		case Hdr#request_header.object_key of
		    {_,registered,orber_init,_,_,_} ->
			corba:request_from_iiop(Hdr#request_header.object_key,
						Hdr#request_header.operation,
						Par, [], 
						Hdr#request_header.response_expected,
						Hdr#request_header.service_context);
%		    {_,_,_,_,_,Flags} when PartialSec == true, 
%					   ?ORB_FLAG_TEST(Flags, ?ORB_NO_SECURITY) == true ->
%			corba:request_from_iiop(Hdr#request_header.object_key,
%						Hdr#request_header.operation,
%						Par, [], 
%						Hdr#request_header.response_expected,
%						Hdr#request_header.service_context);
		    _ ->
			orber:dbg("[~p] orber_iiop_inrequest:invoke_request(~p)~n"
				  "SSL do not permit", 
				  [?LINE, Hdr#request_header.object_key], ?DEBUG_LEVEL),
			{'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}
		end
	end,
    result_to_list(Result, TypeCodes);	
invoke_request(Hdr, Par, ssl, TypeCodes, _SSLPort, _) ->
    Result = corba:request_from_iiop(Hdr#request_header.object_key,
				     Hdr#request_header.operation,
				     Par, [], Hdr#request_header.response_expected,
				     Hdr#request_header.service_context),
    result_to_list(Result, TypeCodes).

%%-----------------------------------------------------------------
%% Func: evaluate/4
%%-----------------------------------------------------------------
evaluate(_, Hdr,_,_,_,_,_) when Hdr#request_header.response_expected == 'false' ->
    oneway;
evaluate(Version, Hdr, _, _, Func, _, Ctx) 
  when Hdr#request_header.response_expected == 'true_oneway' ->
    %% Special case which only occurs when usin IIOP-1.2
    cdr_encode:Func(Version, Hdr#request_header.request_id,
		    'no_exception', {tk_null,[],[]}, null, [], Ctx);
evaluate(Version, Hdr, {'EXCEPTION', Exc}, _, Func, _, Ctx) ->
    %% The exception can be user defined. Hence, we must check the result.
    case catch marshal_exception(Version, Hdr#request_header.request_id, Exc, Func, Ctx) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
		      "Encoding (reply) exception: ~p", 
		      [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func, Ctx);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
		      "Encode (reply) resulted in: ~p", 
		      [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func, Ctx);
	R ->
	    R	
    end;
evaluate({1,2}, Hdr, {'location_forward_perm', NewIOR}, _, Func, _,Ctx)->
    case catch cdr_encode:Func({1,2},
			       Hdr#request_header.request_id,
			       'location_forward_perm',
			       {{'tk_objref', "", ""}, [],[]},
			       NewIOR, [], Ctx) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p) " ++
		      "Encoding (reply) exception: ~p",
		      [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception({1,2}, Hdr#request_header.request_id,
			      Exception, Func, Ctx);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p) " ++
		      "Encode (reply) resulted in: ~p", 
		      [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception({1,2}, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, 
			      Func, Ctx);
	R ->
	    R
    end;
evaluate(Version, Hdr, [Res |OutPar], TypeCodes, Func, Type, Ctx) ->
    case catch cdr_encode:Func(Version, 
			       Hdr#request_header.request_id,
			       Type,
			       TypeCodes,
			       Res, OutPar, Ctx) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)~n"
		      "Encode exception: ~p", 
		      [?LINE, Hdr, Res, OutPar, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func, Ctx);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)~n"
		      "Encode exit: ~p", 
		      [?LINE, Hdr, Res, OutPar, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func, Ctx);
	R ->
	    R
    end;
evaluate(Version, Hdr, What, TypeCodes, Func, _, Ctx) ->
    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)~n"
	      "Bad reply: ~p~n"
	      "Should be: ~p~n"
	      "Context  : ~p", [?LINE, Hdr, What, TypeCodes, Ctx], ?DEBUG_LEVEL),
    marshal_exception(Version, Hdr#request_header.request_id, 
		      #'INTERNAL'{completion_status=?COMPLETED_MAYBE}, Func, Ctx).

%%-----------------------------------------------------------------
%% Utility Functions
%%-----------------------------------------------------------------
result_to_list({'oe_location_forward_perm', NewIOR}, _) ->
    {'location_forward_perm', NewIOR};
result_to_list({'EXCEPTION', E}, _) ->
    {'EXCEPTION', E};
result_to_list(Result, {_TkRes, _, []}) ->
    [Result];
result_to_list(Result, {_TkRes, _, _TkOut}) ->
    tuple_to_list(Result).

marshal_exception(Version, Id, Exception, Func, Ctx) ->
    {TypeOfException, ExceptionTypeCode, NewExc} =
	orber_exceptions:get_def(Exception),
    cdr_encode:Func(Version, Id, TypeOfException, {ExceptionTypeCode, [], []}, 
		    NewExc, [], Ctx).

marshal_locate_exception({1,2}, Id, Exception) ->
    case orber_exceptions:get_def(Exception) of
	{?SYSTEM_EXCEPTION, ExceptionTypeCode, NewExc} ->
	    cdr_encode:enc_locate_reply({1,2}, Id, 'loc_system_exception', 
					ExceptionTypeCode, NewExc);
	_ ->
	    %% This case is impossible (i.e. Orber only throws system
	    %% exceptions). But to be on the safe side...
	    marshal_locate_exception({1,2}, Id, #'MARSHAL'
				     {completion_status=?COMPLETED_YES})
    end;
marshal_locate_exception(Version, _Id, _Exception) ->
    %% There is no way to define an exception for IIOP-1.0/1.1 in a 
    %% locate_reply.
    cdr_encode:enc_message_error(Version).
