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
-export([start/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_message/4]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Message, Type, Socket, Interceptors) ->
    spawn_link(orber_iiop_inrequest, handle_message, 
	       [Message, Type, Socket, Interceptors]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: handle_message/2
%%-----------------------------------------------------------------
handle_message(Message, SocketType, Socket, Interceptors) ->
    case catch cdr_decode:dec_message_header(null, Message) of
	{'EXCEPTION', DecodeException} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exception(~p)", 
				    [?LINE, Message, DecodeException], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exit(~p)", 
				    [?LINE, Message, R], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply);
	'close_connection' ->
            %% This message can only be sent from the server, so we can skip the case.
	    ok;
	'message_error' ->
	    %% This message can only be sent from the server, so we can skip the case.
	    ok;
	'fragment' ->
	    %% We don't support fragmented messages.
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); fragmented messages not supported.", 
				    [?LINE, Message], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply);
	Hdr when record(Hdr, cancel_request_header) ->
            %% We just skips this message for the moment, the standard require that 
	    %% the client handles the reply anyway.
	    ok;
	{Version, Hdr} when record(Hdr, locate_request_header) ->
	    Reply = handle_locate_request(Version, Hdr),
	    orber_socket:write(SocketType, Socket, Reply);
	{Version, ReqHdr, Rest, Len, ByteOrder} when record(ReqHdr, request_header), 
						     Interceptors == false ->
	    ?PRINTDEBUG2("CONTEXT: ~p~n",[ReqHdr#request_header.service_context]),
	    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, enc_reply) of
		{error, E} ->
		    orber_socket:write(SocketType, Socket, E);
		{Version, Hdr, Par, TypeCodes} ->
		    Result = invoke_request(Hdr, Par, SocketType, TypeCodes),
		    Reply  = evaluate_request(Version, Hdr, Result, TypeCodes, enc_reply),
		    send_reply(Reply, SocketType, Socket)
	    end;
	{Version, ReqHdr, Rest, Len, ByteOrder} when record(ReqHdr, request_header) ->
	    ?PRINTDEBUG2("CONTEXT: ~p~n",[ReqHdr#request_header.service_context]),
	    case catch call_interceptors(SocketType, Interceptors, Version, ReqHdr, 
					 Rest, Len, ByteOrder, Message) of
		{error, E} ->
		    %% Failed to decode body.
		    orber_socket:write(SocketType, Socket, E);
		{'EXCEPTION', Exc} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exception(~p)", 
					    [?LINE, Message, Exc], ?DEBUG_LEVEL),
		    Reply = marshal_exception(Version, 
					      ReqHdr#request_header.request_id, 
					      Exc, enc_reply),
		    orber_socket:write(SocketType, Socket, Reply);
		{'EXIT', R} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exit(~p)", 
					    [?LINE, ReqHdr, R], ?DEBUG_LEVEL),
		    Reply = marshal_exception(Version, 
					      ReqHdr#request_header.request_id,
					      #'MARSHAL'{completion_status=?COMPLETED_MAYBE}, enc_reply),
		    orber_socket:write(SocketType, Socket, Reply);
		Reply ->
		    orber_socket:write(SocketType, Socket, Reply)
	    end;
	Other ->
             %% This cluase takes care of all erranous messages.
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); unknown", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, Reply)
   end.


send_reply(oneway, SocketType, Socket) ->
    ok;
send_reply(Reply, SocketType, Socket) ->
    orber_socket:write(SocketType, Socket, Reply).



call_interceptors(SocketType, {basic, PIs},
		  Version, ReqHdr, Rest, Len, ByteOrder, Msg) ->
    NewRest = orber_pi:preinvoke(PIs, Rest),
    case decode_body(Version, ReqHdr, NewRest, Len, ByteOrder, Msg, enc_reply_split) of
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes),
	    {ReplyHdr, Reply, HdrL, BodyL, Flags} = 
		evaluate_request(Version, ReqHdr, Result, TypeCodes, enc_reply_split),
	    NewReply = orber_pi:postinvoke(PIs, Reply),
	    MessSize = HdrL+length(NewReply)-BodyL,
	    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
					       MessSize, [ReplyHdr|NewReply]);
	Other ->
	    Other
    end;
call_interceptors(SocketType, {portable, PIs}, Version, ReqHdr, Rest, Len, 
		  ByteOrder, Msg) ->
    case decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Msg, enc_reply) of
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes),
	    evaluate_request(Version, ReqHdr, Result, TypeCodes, enc_reply);
	Other ->
	    Other
    end;
call_interceptors(SocketType, {both, PIs}, Version, ReqHdr, Rest, Len, 
		  ByteOrder, Msg) ->
    NewRest = orber_pi:preinvoke(PIs, Rest),
    case decode_body(Version, ReqHdr, NewRest, Len, ByteOrder, Msg, enc_reply_split) of
	{Version, Hdr, Par, TypeCodes} ->
	    Result = invoke_request(Hdr, Par, SocketType, TypeCodes),
	    {ReplyHdr, Reply, HdrL, BodyL, Flags} = 
		evaluate_request(Version, ReqHdr, Result, TypeCodes, enc_reply_split),
	    NewReply = orber_pi:postinvoke(PIs, Reply),
	    MessSize = HdrL+length(NewReply)-BodyL,
	    cdr_encode:enc_giop_message_header(Version, 'reply', Flags, 
					       MessSize, [ReplyHdr|NewReply]);
	Other ->
	    Other
    end.


%%-----------------------------------------------------------------
%% Func: decode_body/2
%%-----------------------------------------------------------------
decode_body(Version, ReqHdr, Rest, Len, ByteOrder, Message, Func) ->
    case catch cdr_decode:dec_request_body(Version, ReqHdr, Rest, Len, 
					   ByteOrder, Message) of
	{'EXCEPTION', Exception} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:decode_body(~p); exception(~p)", 
				    [?LINE, ReqHdr, Exception], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, 
				      ReqHdr#request_header.request_id, 
				      Exception, Func)};
	{'EXIT', Reason} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:decode_body(~p); exit(~p)", 
				    [?LINE, ReqHdr, Reason], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, 
				      ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO}, Func)};
	{Version, ReqHdr, Par, TypeCodes} ->
	    {Version, ReqHdr, Par, TypeCodes};
	Other ->
             %% This cluase takes care of all erranous messages.
	    orber:debug_level_print("[~p] orber_iiop_inrequest:decode_body(~p); unknown", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    {error, marshal_exception(Version, 
				      ReqHdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO}, Func)}
    end.
    

%%-----------------------------------------------------------------
%% Func: handle_locate_request/2
%%-----------------------------------------------------------------
handle_locate_request(Version, Hdr) ->
    Location = orber_objectkeys:check(Hdr#locate_request_header.object_key),
    case catch cdr_encode:enc_locate_reply(Version, Hdr#locate_request_header.request_id,
					   Location) of
	{'EXCEPTION', Exception} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_locate_request(~p); exception(~p)", 
				    [?LINE, Location, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#locate_request_header.request_id, Exception, enc_reply);
	{'EXIT', E} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_locate_request(~p); exit(~p)", 
				    [?LINE, Location, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#locate_request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_NO}, enc_reply);
	R ->
	    R
    end.
  
%%-----------------------------------------------------------------
%% Func: invoke_request/2
%%-----------------------------------------------------------------
invoke_request(Hdr, Par, normal, TypeCodes) ->
    Result = 
	case orber:iiop_ssl_port() of
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
			orber:debug_level_print("[~p] orber_iiop_inrequest:invoke_request(~p); SSL do not permit", 
						[?LINE, Hdr#request_header.object_key], ?DEBUG_LEVEL),
			{'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}
		end
	end,
    result_to_list(Result, TypeCodes);	
invoke_request(Hdr, Par, ssl, TypeCodes) ->
    Result = corba:request_from_iiop(Hdr#request_header.object_key,
				     list_to_atom(Hdr#request_header.operation),
				     Par, [], Hdr#request_header.response_expected),
    result_to_list(Result, TypeCodes).

%%-----------------------------------------------------------------
%% Func: evaluate_request/4
%%-----------------------------------------------------------------
evaluate_request(_, Hdr,_,_,_) when Hdr#request_header.response_expected == 'false' ->
    oneway;
evaluate_request(Version, Hdr, _, _, Func) 
  when Hdr#request_header.response_expected == 'true_oneway' ->
    %% Special case which only occurs when usin IIOP-1.2
    cdr_encode:Func(Version, Hdr#request_header.request_id,
			 'no_exception', {tk_null,[],[]}, null, []);
evaluate_request(Version, Hdr, {'EXCEPTION', Exc}, TypeCodes, Func) ->
    case catch orber_typedefs:get_exception_def(Exc) of
	{'EXCEPTION', Exception} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); IFR exception(~p);", 
				    [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); exit(~p); exception not found in IFR?", 
				    [?LINE, Hdr, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'INTF_REPOS'{completion_status=?COMPLETED_NO}, Func);
	{TypeOfException, ExceptionTypeCode} ->
	    case catch cdr_encode:Func(Version,
				       Hdr#request_header.request_id,
				       TypeOfException,
				       {ExceptionTypeCode, [], []}, 
				       Exc, []) of
		{'EXCEPTION', Exception} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); exception(~p); encode reply.", 
					    [?LINE, Hdr, Exception], ?DEBUG_LEVEL),
		    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func);
		{'EXIT', E} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); exit(~p); encode reply.", 
					    [?LINE, Hdr, E], ?DEBUG_LEVEL),
		    marshal_exception(Version, Hdr#request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_NO}, Func);
		R ->
		    R	
	    end;
	Why ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); IFR reply unknown(~p)", 
				    [?LINE, Hdr, Why], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'INTF_REPOS'{completion_status=?COMPLETED_NO}, Func)
    end;
evaluate_request(Version, Hdr, [Res |OutPar], TypeCodes, Func) ->
    case catch cdr_encode:Func(Version, 
			       Hdr#request_header.request_id,
			       'no_exception',
			       TypeCodes,
			       Res, OutPar) of
	{'EXCEPTION', Exception} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p, ~p); encode exception(~p)", 
				    [?LINE, Res, OutPar, Exception], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id, Exception, Func);
	{'EXIT', E} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p, ~p); encode exit(~p)", 
				    [?LINE, Res, OutPar, E], ?DEBUG_LEVEL),
	    marshal_exception(Version, Hdr#request_header.request_id,
			      #'MARSHAL'{completion_status=?COMPLETED_YES}, Func);
	R ->
	    R
    end;
evaluate_request(Version, Hdr, What, TypeCodes, Func) ->
    orber:debug_level_print("[~p] orber_iiop_inrequest:evaluate_request(~p); bad reply(~p)", 
			    [?LINE, Hdr, What], ?DEBUG_LEVEL),
    E = #'INTERNAL'{completion_status=?COMPLETED_MAYBE},
    {TypeOfException, ExceptionTypeCode} =
	orber_typedefs:get_exception_def(E),
    cdr_encode:Func(Version,
		    Hdr#request_header.request_id,
		    TypeOfException,
		    {ExceptionTypeCode, [], []}, 
		    E, []).

%%-----------------------------------------------------------------
%% Utility Functions
%%-----------------------------------------------------------------
result_to_list({'EXCEPTION', E}, _) ->
    {'EXCEPTION', E};
result_to_list(Result, {TkRes, _, TkOut}) ->
   case length(TkOut) of
       0 ->
	   [Result];
       N ->
	   tuple_to_list(Result)
   end.

marshal_exception(Version, Id, Exception, Func) ->
    {TypeOfException, ExceptionTypeCode} =
	orber_typedefs:get_exception_def(Exception),
    cdr_encode:Func(Version,
		    Id,
		    TypeOfException,
		    {ExceptionTypeCode, [], []}, 
		    Exception, []).

