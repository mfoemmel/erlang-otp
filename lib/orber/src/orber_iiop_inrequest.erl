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

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([handle_message/3]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Message, Type, Socket) ->
    spawn_link(orber_iiop_inrequest, handle_message, [Message, Type, Socket]).

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: handle_message/2
%%-----------------------------------------------------------------
-ifdef(interceptors).

handle_message(Message, SocketType, Socket) ->
    %%?PRINTDEBUG2("proxy ~p started", [self()]),
    case catch cdr_decode:dec_message(null,
				      orber_interceptors:call_receive_message_interceptors(Message)) of
	{'EXCEPTION', DecodeException} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exception(~p)", 
				    [?LINE, Message, DecodeException], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    ?PRINTDEBUG2("Exception ~p", [DecodeException]),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exit(~p)", 
				    [?LINE, Message, R], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    ?PRINTDEBUG2("Exception ~p", [R]),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
	F ->
	    execute_call(F, SocketType, Socket)
    end.

-else.

handle_message(Message, SocketType, Socket) ->
    %%?PRINTDEBUG2("proxy ~p started", [self()]),
    case catch cdr_decode:dec_message(null, Message) of
	{'EXCEPTION', DecodeException} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exception(~p)", 
				    [?LINE, Message, DecodeException], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    ?PRINTDEBUG2("Exception ~p", [DecodeException]),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
	{'EXIT', R} ->
	    orber:debug_level_print("[~p] orber_iiop_inrequest:handle_message(~p); exit(~p)", 
				    [?LINE, Message, R], ?DEBUG_LEVEL),
	    Reply = cdr_encode:enc_message_error(orber:giop_version()),
	    ?PRINTDEBUG2("Exception ~p", [R]),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
	F ->
	    execute_call(F, SocketType, Socket)
    end.

-endif.

%%-----------------------------------------------------------------
%% Func: execute_call/2
%%-----------------------------------------------------------------
execute_call({Version, Hdr, Par, TypeCodes}, SocketType, Socket) when record(Hdr, request_header) ->
    Result = 
	case SocketType of
	    normal ->
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
				orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); SSL do not permit", 
							[?LINE, Hdr#request_header.object_key], ?DEBUG_LEVEL),
				{'EXCEPTION', #'NO_PERMISSION'{completion_status=?COMPLETED_NO}}
			end
		end;			
	    ssl ->
		corba:request_from_iiop(Hdr#request_header.object_key,
					list_to_atom(Hdr#request_header.operation),
					Par, [], Hdr#request_header.response_expected)
%% If/when implement other than ssl and we need a security context to be included in the 
%% call switch to the following:
%		ETS = ets:new(oe_request_data, [set, public]),
%		%% Set default values, received attributes, implicit request context
%		%% et.c. here.
%		ets:insert(ETS, {{in, ?ORB_SEC_ATTRIBUTES}, [ssl]}),
%		Obj = iop_ior:set_orbfield(Hdr#request_header.object_key,
%					   term_to_binary({ETS, 
%							   ?ORB_SET_TRUE(?ORB_INIT_FLAGS, ?ORB_SEC_ATTRIBUTES)})),
%		corba:request_from_iiop(Obj,
%					list_to_atom(Hdr#request_header.operation),
%					Par, [], Hdr#request_header.response_expected)
	end,
    case Hdr#request_header.response_expected of
	'true' ->
	    Reply = case result_to_list(Result, TypeCodes) of
		{'EXCEPTION', Exception} ->
		    case catch orber_typedefs:get_exception_def(Exception) of
			{'EXCEPTION', Exception} ->
			    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); IFR exception(~p);", 
						    [?LINE, Result, Exception], ?DEBUG_LEVEL),
			    marshal_exception(Version, Hdr#request_header.request_id, Exception);
			{'EXIT', E} ->
			    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); exit(~p); exception not found in IFR?", 
						    [?LINE, Result, E], ?DEBUG_LEVEL),
			    marshal_exception(Version, Hdr#request_header.request_id,
					      #'INTF_REPOS'{completion_status=?COMPLETED_NO});
			{TypeOfException, ExceptionTypeCode} ->
			    case catch cdr_encode:enc_reply(Version,
							    Hdr#request_header.request_id,
							    TypeOfException,
							    {ExceptionTypeCode, [], []}, 
							    Exception, []) of
				{'EXCEPTION', Exception} ->
				    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); exception(~p); encode reply.", 
							    [?LINE, Result, Exception], ?DEBUG_LEVEL),
				    marshal_exception(Version, Hdr#request_header.request_id, Exception);
				{'EXIT', E} ->
				    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); exit(~p); encode reply.", 
							    [?LINE, Result, E], ?DEBUG_LEVEL),
				    marshal_exception(Version, Hdr#request_header.request_id,
						      #'MARSHAL'{completion_status=?COMPLETED_NO});
				R ->
				    R	
			    end;
			Why ->
			    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); IFR reply unknown(~p)", 
						    [?LINE, Result, Why], ?DEBUG_LEVEL),
			    marshal_exception(Version, Hdr#request_header.request_id,
					      #'INTF_REPOS'{completion_status=?COMPLETED_NO})
		    end;
  		[Res |OutPar] ->
		    case catch cdr_encode:enc_reply(Version, 
					 Hdr#request_header.request_id,
					 'no_exception',
					 TypeCodes,
					 Res, OutPar) of
			{'EXCEPTION', Exception} ->
			    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p, ~p); encode exception(~p)", 
						    [?LINE, Res, OutPar, Exception], ?DEBUG_LEVEL),
			    marshal_exception(Version, Hdr#request_header.request_id, Exception);
			{'EXIT', E} ->
			    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p, ~p); encode exit(~p)", 
						    [?LINE, Res, OutPar, E], ?DEBUG_LEVEL),
			    marshal_exception(Version, Hdr#request_header.request_id,
					      #'MARSHAL'{completion_status=?COMPLETED_YES});
			R ->
			    R
		    end;
		What ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); bad reply(~p)", 
					    [?LINE, Result, What], ?DEBUG_LEVEL),
		    E = #'INTERNAL'{completion_status=?COMPLETED_MAYBE},
		    {TypeOfException, ExceptionTypeCode} =
			orber_typedefs:get_exception_def(E),
		    cdr_encode:enc_reply(Version,
					 Hdr#request_header.request_id,
					 TypeOfException,
					 {ExceptionTypeCode, [], []}, 
					 E, [])
	    end, 
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
	'false' ->
	    %% ONEWAY requests
	    ok
    end;
execute_call('close_connection', _, _) -> 
    %% This message can only be sent from the server, so we can skip the case.
    ok;
execute_call('message_error', _, _) -> 
    %% This message can only be sent from the server, so we can skip the case.
    ok;
execute_call({Version, Hdr}, SocketType, Socket) when record(Hdr,locate_request_header) ->
    Location = orber_objectkeys:check(Hdr#locate_request_header.object_key),
    Reply = case catch cdr_encode:enc_locate_reply(Version, Hdr#locate_request_header.request_id,
						   Location) of
		{'EXCEPTION', Exception} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); exception(~p)", 
					    [?LINE, Location, Exception], ?DEBUG_LEVEL),
		    marshal_exception(Version, Hdr#locate_request_header.request_id, Exception);
		{'EXIT', E} ->
		    orber:debug_level_print("[~p] orber_iiop_inrequest:execute_call(~p); exit(~p)", 
					    [?LINE, Location, E], ?DEBUG_LEVEL),
		    marshal_exception(Version, Hdr#locate_request_header.request_id,
				      #'MARSHAL'{completion_status=?COMPLETED_YES});
		R ->
		    R
	    end,
    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply);
execute_call(Hdr, _, _) when record(Hdr,cancel_request_header) ->
    %% We just skips this message for the moment, the standard require that the client handles the reply anyway.
    ok;
execute_call(_, SocketType, Socket) ->
    %% This cluase takes care of all erranous messages.
    Reply = cdr_encode:enc_message_error(orber:giop_version()),
    ?IIOP_SOCKET_MOD:write(SocketType, Socket, Reply).



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

marshal_exception(Version, Id, Exception) ->
    {TypeOfException, ExceptionTypeCode} =
	orber_typedefs:get_exception_def(Exception),
    cdr_encode:enc_reply(Version,
			 Id,
			 TypeOfException,
			 {ExceptionTypeCode, [], []}, 
			 Exception, []).

