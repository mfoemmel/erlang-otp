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
-include_lib("orber/src/orber_debug.hrl").
-include_lib("orber/include/orber_pi.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/6, start_fragment_collector/8]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_message/6, fragment_collector/8]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(GIOPHdr, Message, Type, Socket, Interceptors, SSLPort) ->
    spawn_link(orber_iiop_inrequest, handle_message, 
	       [GIOPHdr, Message, Type, Socket, Interceptors, SSLPort]).

start_fragment_collector(GIOPHdr, Message, Type, Socket, Interceptors, 
			 ReqId, Proxy, SSLPort) ->
    spawn_link(orber_iiop_inrequest, fragment_collector, 
	       [GIOPHdr, Message, Type, Socket, Interceptors, 
		ReqId, Proxy, SSLPort]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: fragment_collector/4
%%-----------------------------------------------------------------
fragment_collector(GIOPHdr, Bytes, SocketType, Socket, Interceptors, 
		   ReqId, Proxy, SSLPort) ->
    case catch collect(Proxy, [], GIOPHdr#giop_message.byte_order, ReqId) of
	{ok, Buffer} ->
	    NewGIOP = GIOPHdr#giop_message
			{message = list_to_binary([GIOPHdr#giop_message.message|Buffer])},
	    %% NOTE, the third argument to dec_message_header must be complete
	    %% message (i.e. AllBytes), otherwise we cannot handle indirection.
	    case handle_message(NewGIOP, list_to_binary([Bytes| Buffer]), 
				SocketType, Socket, Interceptors, SSLPort) of
		message_error ->
		    Proxy ! {message_error, self(), ReqId},
		    ok;
		_ ->
		    ok
	    end;
	ok ->
	    ok;
	Other ->
	    Proxy ! {message_error, self(), ReqId},
	    orber:dbg("[~p] orber_iiop_inrequest:fragment_collector(~p)
Unable to collect all fragments: ~p", [?LINE, Bytes, Other], ?DEBUG_LEVEL),
	    Reply = marshal_exception(GIOPHdr#giop_message.giop_version, ReqId,
				      #'MARSHAL'{completion_status=?COMPLETED_NO}, 
				      enc_reply),
	    orber_socket:write(SocketType, Socket, Reply)
    end.



collect(Proxy, Buffer, ByteOrder, ReqId) ->
    receive
        {Proxy, #giop_message{byte_order = ByteOrder,
			      message    = Message,
			      fragments  = true} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
            collect(Proxy, [FragBody | Buffer], ByteOrder, ReqId);
	{Proxy, #giop_message{byte_order = ByteOrder,
			      message = Message,
			      fragments = false} = GIOPHdr} ->
	    {_, #fragment_header{request_id=ReqId}, FragBody, _, _} = 
		cdr_decode:dec_message_header(null, GIOPHdr, Message),
	    {ok, lists:reverse([FragBody | Buffer])};
	{Proxy, GIOPHdr, Data, _} ->
	    orber:dbg("[~p] orber_iiop_inrequest:collect(~p, ~p)
Incorrect Fragment. Might be different byteorder.", 
		      [?LINE, ByteOrder, GIOPHdr], ?DEBUG_LEVEL),
	    "Incorrect Fragment";
	{Proxy, cancel_request_header} ->
	    ok;
	_ ->
	    error
    end.


%%-----------------------------------------------------------------
%% Func: handle_message/4
%%-----------------------------------------------------------------
handle_message(GIOPHdr, Message, SocketType, Socket, Interceptors, SSLPort) ->
    case catch cdr_decode:dec_message_header(null, GIOPHdr, Message) of
	Hdr when record(Hdr, cancel_request_header) ->
            %% We just skips this message for the moment, the standard require that 
	    %% the client handles the reply anyway.
	    message_error;
	{location_forward, Object, ReqId, Version, OldObj} ->
	    Reply = call_interceptors_out(Version, ReqId, [Object], OldObj,
					  Interceptors, 'location_forward', 
					  "location_forward",
					  {{'tk_objref', "", ""}, [],[]}),
	    orber_socket:write(SocketType, Socket, Reply);
	{object_forward, Object, ReqId, Version, OldObj} ->
	    Reply = handle_locate_request(Version, 
					  {object_forward, Object, ReqId}),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, Hdr} when record(Hdr, locate_request_header) ->
	    Reply = handle_locate_request(Version, Hdr),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, ReqHdr, Rest, Len, ByteOrder} when record(ReqHdr, request_header) ->
	    handle_request(Interceptors, Version, ReqHdr, Rest, Len, 
			   ByteOrder, SocketType, Socket, Message, SSLPort);
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)
Decoding Msg Header failed: ~p", [?LINE, Message, Other], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply),
	    message_error
   end.


send_reply(oneway, SocketType, Socket) ->
    ok;
send_reply(Reply, SocketType, Socket) ->
    orber_socket:write(SocketType, Socket, Reply).

%%-----------------------------------------------------------------
%% Func: handle_request
%%-----------------------------------------------------------------
handle_request(false, Version, ReqHdr, Rest, Len, ByteOrder, SocketType, Socket,
	       Message, SSLPort) ->
    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, enc_reply) of
	{error, E} ->
	    orber_socket:write(SocketType, Socket, E);
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, SSLPort),
	    Reply  = evaluate(Version, Hdr, Result, TypeCodes, 
			      enc_reply, 'no_exception'),
	    send_reply(Reply, SocketType, Socket)
    end;
handle_request(Interceptors, Version, ReqHdr, Rest, Len, ByteOrder, 
	       SocketType, Socket, Message, SSLPort) ->
    case catch call_interceptors(SocketType, Interceptors, Version, ReqHdr, 
				 Rest, Len, ByteOrder, Message, SSLPort) of
	{error, E} ->
	    %% Failed to decode body.
	    orber_socket:write(SocketType, Socket, E);
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)
Invoking the interceptors resulted in: ~p", [?LINE, Message, Exc], ?DEBUG_LEVEL),
	    Reply = marshal_exception(Version, 
				      ReqHdr#request_header.request_id, 
				      Exc, enc_reply),
	    orber_socket:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_message(~p)
Invoking the interceptors resulted in: ~p", [?LINE, ReqHdr, R], ?DEBUG_LEVEL),
	    Reply = marshal_exception(Version, 
				      ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_MAYBE}, enc_reply),
	    orber_socket:write(SocketType, Socket, Reply);
	Reply ->
	    send_reply(Reply, SocketType, Socket)
    end.



%%-----------------------------------------------------------------
%% Func: call_interceptors_out
%%-----------------------------------------------------------------
call_interceptors(SocketType, {native, Ref, PIs},
		  Version, ReqHdr, Rest, Len, ByteOrder, Msg, SSLPort) ->
    NewRest = orber_pi:in_request_enc(PIs, ReqHdr, Ref, Rest),
    case decode_body(Version, ReqHdr, NewRest, Len, ByteOrder, Msg, enc_reply) of
	{Version, Hdr, Par, TypeCodes} ->
	    NewPar = orber_pi:in_request(PIs, ReqHdr, Ref, Par),
	    ResultInv = invoke_request(Hdr, NewPar, SocketType, TypeCodes, SSLPort),
	    Result = orber_pi:out_reply(PIs, ReqHdr, Ref, ResultInv),
	    
            case evaluate(Version, ReqHdr, Result, TypeCodes, enc_reply_split,
			  'no_exception') of
		{ReplyHdr, Reply, HdrL, BodyL, Flags} ->
		    NewReply = orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply),
		    MessSize = HdrL+size(NewReply),
		    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
						       MessSize, [ReplyHdr|NewReply]);
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end;
call_interceptors(SocketType, {portable, PIs}, Version, ReqHdr, Rest, Len, 
		  ByteOrder, Msg, SSLPort) ->
    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg, enc_reply) of
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes, SSLPort),
	    evaluate(Version, ReqHdr, Result, TypeCodes, enc_reply, 'no_exception');
	Other ->
	    Other
    end.

%%-----------------------------------------------------------------
%% Func: call_interceptors_out
%%-----------------------------------------------------------------
call_interceptors_out(Version, ReqId, Result, Obj, {native, Ref, PIs}, Type,
		      Operation, TypeCodes) ->
    ReqHdr = #request_header{object_key = Obj,
			     service_context = [],
			     response_expected = true,
			     request_id = ReqId,
			     operation = Operation},
    NewResult = (catch orber_pi:out_reply(PIs, ReqHdr, Ref, Result)),
    {ReplyHdr, Reply, HdrL, BodyL, Flags} = 
	evaluate(Version, ReqHdr, NewResult, TypeCodes, enc_reply_split, Type),
    NewReply = 
	case catch orber_pi:out_reply_enc(PIs, ReqHdr, Ref, Reply) of
	    {'EXCEPTION', Exception} ->
		%% Since evaluate don't need TypeCodes or Status no need to supply
		%% them.
		evaluate(Version, ReqHdr, {'EXCEPTION', Exception}, undefined, 
			 enc_reply_split, undefined);
	    {'EXIT', E} ->
		orber:dbg("[~p] orber_iiop_inrequest:handle_location_forward(~p)
Resulted in exit: ~p", [?LINE, PIs, E], ?DEBUG_LEVEL),
		marshal_exception(Version, ReqId,
				  #'MARSHAL'{completion_status=?COMPLETED_NO}, enc_reply);
	    R ->
		R
	end,
    MessSize = HdrL+size(NewReply),
    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
				       MessSize, [ReplyHdr|NewReply]);
call_interceptors_out(Version, ReqId, Result, Obj, {portable, PIs}, 
		      Type, _, TypeCodes) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Version, Hdr, Result, TypeCodes, enc_reply, Type);
call_interceptors_out(Version, ReqId, Result, Obj, _, Type, _, TypeCodes) ->
    Hdr = #request_header{response_expected = true,
			  request_id = ReqId},
    evaluate(Version, Hdr, Result, TypeCodes, enc_reply, Type).


%%-----------------------------------------------------------------
%% Func: decode_body/2
%%-----------------------------------------------------------------
decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, Func) ->
    case catch cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, 
					   ByteOrder, Message) of
	{Version, ReqHdr, Par, TypeCodes} ->
	    {Version, ReqHdr, Par, TypeCodes};
	Other ->
	    %% This cluase takes care of all erranous messages.
	    orber:dbg("[~p] orber_iiop_inrequest:decode_body(~p, ~p)
Failed decoding request body: ~p", [?LINE, ReqHdr, Message, Other], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO},
				      Func)}
    end.
    

%%-----------------------------------------------------------------
%% Func: handle_locate_request/2
%%-----------------------------------------------------------------
handle_locate_request(Version, {object_forward, Object, ReqId}) ->
    case catch cdr_encode:enc_locate_reply(Version, ReqId, 'object_forward', 
					   {'tk_objref', "", ""}, Object) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)
Raised the exception: ~p", [?LINE, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, ReqId, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(object_forward)
Resulted in exit: ~p", [?LINE, E], ?DEBUG_LEVEL),
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
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)
Raised the exception: ~p", [?LINE, Location, Exception], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, Hdr#locate_request_header.request_id, Exception);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:handle_locate_request(~p)
Resulted in exit: ~p", [?LINE, Location, E], ?DEBUG_LEVEL),
	    marshal_locate_exception(Version, Hdr#locate_request_header.request_id,
				     #'MARSHAL'{completion_status=?COMPLETED_NO});
	R ->
	    R
    end.
  
%%-----------------------------------------------------------------
%% Func: invoke_request/2
%%-----------------------------------------------------------------
invoke_request(Hdr, Par, normal, TypeCodes, SSLPort) ->
    Result = 
	case SSLPort of
	    -1 ->
		corba:request_from_iiop(Hdr#request_header.object_key,
					list_to_atom(Hdr#request_header.operation),
					Par, [], Hdr#request_header.response_expected);
	    _ ->
		case Hdr#request_header.object_key of
		    {_,registered,orber_init,_,_,_} ->
			corba:request_from_iiop(Hdr#request_header.object_key,
						list_to_atom(Hdr#request_header.operation),
						Par, [], Hdr#request_header.response_expected);
		    _ ->
			orber:dbg("[~p] orber_iiop_inrequest:invoke_request(~p)
SSL do not permit", [?LINE, Hdr#request_header.object_key], ?DEBUG_LEVEL),
			{'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}
		end
	end,
    result_to_list(Result, TypeCodes);	
invoke_request(Hdr, Par, ssl, TypeCodes, SSLPort) ->
    Result = corba:request_from_iiop(Hdr#request_header.object_key,
				     list_to_atom(Hdr#request_header.operation),
				     Par, [], Hdr#request_header.response_expected),
    result_to_list(Result, TypeCodes).

%%-----------------------------------------------------------------
%% Func: evaluate/4
%%-----------------------------------------------------------------
evaluate(_, Hdr,_,_,_,_) when Hdr#request_header.response_expected == 'false' ->
    oneway;
evaluate(Version, Hdr, _, _, Func, _) 
  when Hdr#request_header.response_expected == 'true_oneway' ->
    %% Special case which only occurs when usin IIOP-1.2
    cdr_encode:Func(Version, Hdr#request_header.request_id,
		    'no_exception', {tk_null,[],[]}, null, []);
evaluate(Version, Hdr, {'EXCEPTION', Exc}, _, Func, _) ->
    %% The exception can be user defined. Hence, we must check the result.
    case catch marshal_exception(Version, Hdr#request_header.request_id, Exc, Func) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)
Encoding (reply) exception: ~p", [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)
Encode (reply) resulted in: ~p", [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R	
    end;
evaluate(Version, Hdr, [Res |OutPar], TypeCodes, Func, Type) ->
    case catch cdr_encode:Func(Version, 
			       Hdr#request_header.request_id,
			       Type,
			       TypeCodes,
			       Res, OutPar) of
	{'EXCEPTION', Exception} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)
Encode exception: ~p", [?LINE, Hdr, Res, OutPar, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p, ~p, ~p)
Encode exit: ~p", [?LINE, Hdr, Res, OutPar, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R
    end;
evaluate(Version, Hdr, What, TypeCodes, Func, _) ->
    orber:dbg("[~p] orber_iiop_inrequest:evaluate(~p)
Bad reply: ~p
Should be: ~p", [?LINE, Hdr, What, TypeCodes], ?DEBUG_LEVEL),
    marshal_exception(Version, Hdr#request_header.request_id, 
		      #'INTERNAL'{completion_status=?COMPLETED_MAYBE}, Func).

%%-----------------------------------------------------------------
%% Utility Functions
%%-----------------------------------------------------------------
result_to_list({'EXCEPTION', E}, _) ->
    {'EXCEPTION', E};
result_to_list(Result, {TkRes, _, []}) ->
    [Result];
result_to_list(Result, {TkRes, _, TkOut}) ->
    tuple_to_list(Result).

marshal_exception(Version, Id, Exception, Func) ->
    {TypeOfException, ExceptionTypeCode} =
	orber_typedefs:get_exception_def(Exception),
    cdr_encode:Func(Version, Id, TypeOfException, {ExceptionTypeCode, [], []}, 
		    Exception, []).

marshal_locate_exception({1,2}, Id, Exception) ->
    case orber_typedefs:get_exception_def(Exception) of
	{?SYSTEM_EXCEPTION, ExceptionTypeCode} ->
	    cdr_encode:enc_locate_reply({1,2}, Id, 'loc_system_exception', 
					ExceptionTypeCode, Exception);
	_ ->
	    %% This case is impossible (i.e. Orber only throws system
	    %% exceptions). But to be on the safe side...
	    marshal_locate_exception({1,2}, Id, #'MARSHAL'
				     {completion_status=?COMPLETED_YES})
    end;
marshal_locate_exception(Version, Id, Exception) ->
    %% There is no way to define an exception for IIOP-1.0/1.1 in a 
    %% locate_reply.
    cdr_encode:enc_message_error(Version).
