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
%% File: orber_iiop.erl
%% Description:
%%    This file contains the interface to the iiop operations
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(orber_iiop).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-behaviour(supervisor).
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start_sup/1, request/8, locate/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3]).

%%-----------------------------------------------------------------
%% Internal defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start_sup/1
%%-----------------------------------------------------------------
start_sup(Opts) ->
    supervisor:start_link({local, orber_iiop_sup}, ?MODULE,
			  {orber_iiop_sup, Opts}).

%%%-----------------------------------------------------------------
%%% Func: connect/1
%%%-----------------------------------------------------------------
%connect(OrbName) ->
%    orber_iiop_net:connect(OrbName).

%%%-----------------------------------------------------------------
%%% Func: request/5
%%%-----------------------------------------------------------------
request({Host, Port, InitObjkey, Index, TaggedProfile, HostData}, 
	Op, Parameters, TypeCodes, ResponseExpected, Timeout, IOR, UserCtx) ->
    {{Proxy, SysCtx, Interceptors}, ObjKey, Version} =
	connect(Host, Port, InitObjkey, Timeout, [Index], HostData,
		TaggedProfile, IOR),
    Ctx = add_user_context(SysCtx, UserCtx),
    RequestId = orber_request_number:get(),
    Message = encode_request(Interceptors, Version, ObjKey, RequestId, 
			     ResponseExpected, Op, Parameters, Ctx, TypeCodes),
    case catch orber_iiop_outproxy:request(Proxy, ResponseExpected, Timeout, 
					   Message, RequestId) of 
	{'EXCEPTION', MsgExc} ->
	    corba:raise(MsgExc);
	_ when ResponseExpected == false ->
	    ok;
	{reply, ReplyHeader, Rest, Len, ByteOrder, Bytes} ->
	    case catch decode_reply_body(Interceptors, ObjKey, Op, ReplyHeader, 
					 Version, TypeCodes, Rest, Len, ByteOrder, 
					 Bytes) of
		{'EXCEPTION', DecodeException} ->
		    %% We cannot log this exception since it may be a correct exception.
		    corba:raise(DecodeException);
		{'EXIT', message_error} ->
		    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
			      "Got exit(message_error)", 
			      [?LINE, Rest, Version, TypeCodes], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		{'EXIT', Why} ->
		    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
			      "Got exit(~p)", 
			      [?LINE, Rest, Version, TypeCodes, Why], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		'message_error' ->
		    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p);~n"
			      "Got message_error", 
			      [?LINE, Rest, Version, TypeCodes], ?DEBUG_LEVEL),
                    %% Perhaps a resend should be done when a message error occurs
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		{Result, Par} ->
                    %% Check request id 
		    case ReplyHeader#reply_header.reply_status of
			'no_exception' ->
			    case Par of
				[] ->
				    Result;
				_ ->
				    list_to_tuple([Result | Par])
			    end;
			'system_exception' ->
			    corba:raise(Result);
			'user_exception' ->
			    corba:raise(Result);
			'location_forward' ->
			    case get(orber_forward_notify) of
				true ->
				   {location_forward, Result};
				_ ->
				    case catch corba:call(Result, Op, Parameters, 
							  TypeCodes, 
							  [{timeout, Timeout},
							   {context, UserCtx}]) of
					{'EXCEPTION', E} ->
					    corba:raise(E);
					{'EXIT', Reason} ->
					    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
						      "location_forward resulted in exit(~p)", 
						      [?LINE, Rest, Version, TypeCodes, Reason], ?DEBUG_LEVEL),
					    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
					NewResult ->
					    NewResult 
				    end
			    end;
			'location_forward_perm' ->
                            %% We should notify the client in this case.
			    case get(orber_forward_notify) of
				true ->
				   {location_forward, Result};
				_ ->
				    case catch corba:call(Result, Op, Parameters, 
                                                          TypeCodes,
							  [{timeout, Timeout},
							   {context, UserCtx}]) of
					{'EXCEPTION', E} ->
					    corba:raise(E);
					{'EXIT', Reason} ->
					    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
						      "location_forward_perm resulted in exit(~p)", 
						      [?LINE, Rest, Version, TypeCodes, Reason], ?DEBUG_LEVEL),
					    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
					NewResult ->
					    NewResult
				    end
			    end;
			'needs_addressing_mode' ->
			    orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
				      "needs_addressing_mode not supported.", 
				      [?LINE, Rest, Version, TypeCodes], ?DEBUG_LEVEL),
			    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
		    end
	    end;
        What ->
            orber:dbg("[~p] orber_iiop:request(reply, ~p, ~p, ~p)~n"
		      "outproxy-request: ~p", [?LINE, Message, Version, TypeCodes, What], ?DEBUG_LEVEL),
            corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.


encode_request(false, Version, ObjKey, RequestId, ResponseExpected, Op, Parameters,
	       Ctx, TypeCodes) ->
    case catch cdr_encode:enc_request(Version, ObjKey, RequestId, ResponseExpected, 
				      Op, Parameters, Ctx, TypeCodes) of
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop:request( ~p, ~p, ~p)~n"
		      "Got exception(~p)", 
		      [?LINE, Op, Parameters, TypeCodes, Exc], ?DEBUG_LEVEL),
	    corba:raise(Exc);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop:request:( ~p, ~p, ~p)~n"
		      "Got exit(~p)", 
		      [?LINE, Op, Parameters, TypeCodes, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	Msg ->
	    Msg
    end;
encode_request({native, Ref, PIs}, Version, ObjKey, RequestId, ResponseExpected, Op, 
	       Params, Ctx, TypeCodes) ->
    Parameters = orber_pi:out_request(PIs, ObjKey, Ctx, Op, Ref, Params),
    case catch cdr_encode:enc_request_split(Version, ObjKey, RequestId, 
					    ResponseExpected, Op, Parameters,
					    Ctx, TypeCodes) of
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop:request( ~p, ~p, ~p); exception(~p)", 
				    [?LINE, Op, Parameters, TypeCodes, Exc], ?DEBUG_LEVEL),
	    corba:raise(Exc);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop:request:( ~p, ~p, ~p); got exit(~p)", 
				    [?LINE, Op, Parameters, TypeCodes, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	{Hdr, Body, HdrLen, _, Flags} ->
	    NewBody = orber_pi:out_request_enc(PIs, ObjKey, Ctx, Op, Ref, Body),
	    cdr_encode:enc_giop_message_header(Version, 'request', Flags, 
					       HdrLen+size(NewBody), 
					       [Hdr|NewBody])
    end;
encode_request({_Type, _PIs}, Version, ObjKey, RequestId, ResponseExpected, Op, 
	       Parameters, Ctx, TypeCodes) ->
    case catch cdr_encode:enc_request(Version, ObjKey, RequestId, ResponseExpected, 
				      Op, Parameters, Ctx, TypeCodes) of
	{'EXCEPTION', Exc} ->
	    orber:dbg("[~p] orber_iiop:request( ~p, ~p, ~p); exception(~p)", 
		      [?LINE, Op, Parameters, TypeCodes, Exc], ?DEBUG_LEVEL),
	    corba:raise(Exc);
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop:request:( ~p, ~p, ~p); got exit(~p)", 
		      [?LINE, Op, Parameters, TypeCodes, R], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	Msg ->
	    Msg
    end.

%%-----------------------------------------------------------------
%% Func: locate/1
%%-----------------------------------------------------------------
locate({Host, Port, InitObjkey, Index, TaggedProfile, HostData},
       Timeout, IOR) ->
    {{Proxy, _Ctx, _Interceptors}, ObjKey, Version} =
	connect(Host, Port, InitObjkey, Timeout, [Index], HostData,
		TaggedProfile, IOR),
    RequestId = orber_request_number:get(),
    Result = 
	case catch cdr_encode:enc_locate_request(Version, ObjKey,  RequestId) of
	    {'EXCEPTION', EncE} ->
		orber:dbg("[~p] orber_iiop:locate(~p); exception(~p)", 
			  [?LINE, ObjKey, EncE], ?DEBUG_LEVEL),
		corba:raise(EncE);
	    {'EXIT', EncR} ->
		orber:dbg("[~p] orber_iiop:locate(~p); exit(~p)", 
			  [?LINE, ObjKey, EncR], ?DEBUG_LEVEL),
		corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO});
	    Request ->
		(catch orber_iiop_outproxy:request(Proxy, true, Timeout, 
						   Request, RequestId))
	end,
    case Result of
	{'EXCEPTION', MsgExc} ->
	    corba:raise(MsgExc);
	{locate_reply, ReplyHeader, Rest, Len, ByteOrder} ->
	    case catch cdr_decode:dec_locate_reply_body(Version, 
							ReplyHeader#locate_reply_header.locate_status,
							Rest, Len, ByteOrder) of
		{'EXCEPTION', DecodeException} ->
		    orber:dbg("[~p] orber_iiop:locate(locate_reply, ~p, ~p); exception(~p)", 
			      [?LINE, Rest, Version, DecodeException], ?DEBUG_LEVEL),
		    corba:raise(DecodeException);
		{'EXIT', message_error} ->
		    orber:dbg("[~p] orber_iiop:locate(locate_reply, ~p, ~p); exit(message_error)", 
					    [?LINE, Rest, Version], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		{'EXIT', R} ->
		    orber:dbg("[~p] orber_iiop:locate(locate_reply, ~p, ~p); exit(~p)", 
			      [?LINE, Rest, Version, R], ?DEBUG_LEVEL),
		    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
		[] ->
		    ReplyHeader#locate_reply_header.locate_status;
		ObjRef ->
		    {ReplyHeader#locate_reply_header.locate_status, ObjRef}
	    end;
	Other ->
	    orber:dbg("[~p] orber_iiop:locate(~p); exit(~p)", 
		      [?LINE, ObjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'MARSHAL'{completion_status=?COMPLETED_NO})
    end.
 	    
%%%-----------------------------------------------------------------
%%% Func: cancel/1
%%%-----------------------------------------------------------------
%cancel(X) ->
%	ok.

%%%-----------------------------------------------------------------
%%% Func: message_error/1
%%%-----------------------------------------------------------------
%message_error(X) ->
%	ok.

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({orber_iiop_sup, Opts}) ->
    IIOP_port      =  orber:iiop_port(),
    Bootstrap_port =  orber:bootstrap_port(),
    SSL_port       =  orber:iiop_ssl_port(),
    SupFlags       = {one_for_one, 5, 1000},	%Max 5 restarts in 1 second
    PortList = if
		   SSL_port > -1 ->
		       [{port, ssl, SSL_port}];
		   true ->
		       []
		 end,
    ChildSpec = 
	case orber:is_lightweight() of
	    true ->
		[
		 {orber_iiop_outsup, {orber_iiop_outsup, start,
				      [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_outsup]},
		 {orber_iiop_pm, {orber_iiop_pm, start,
				  [Opts]},
		  permanent, 10000, worker, [orber_iiop_pm]}
		];
	    false ->
		ChildSpec1 = 
		    if
			Bootstrap_port == IIOP_port ->
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, normal, IIOP_port} | PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]}];
			Bootstrap_port < 1024 ->
			%% Used for testing without being root
                        %% Bootstrap_port < 2024 -> 
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, normal, IIOP_port}| PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]},
			     {orber_bootstrap, {orber_bootstrap,
						start, [{port, normal, Bootstrap_port}]}, permanent, 
			      10000, worker, [orber_bootstrap]}];
			true ->
			    [{orber_iiop_net, {orber_iiop_net, start,
					       [[{port, normal, IIOP_port},
						 {port, normal, Bootstrap_port}| PortList]]},
			      permanent, 10000, worker, [orber_iiop_net]}]
		    end,
		[
		 {orber_iiop_outsup, {orber_iiop_outsup, start,
				      [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_outsup]},
		 {orber_iiop_pm, {orber_iiop_pm, start,
				  [Opts]},
		  permanent, 10000, worker, [orber_iiop_pm]},
		 {orber_iiop_insup, {orber_iiop_insup, start,
				     [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_insup]},
		 {orber_iiop_socketsup, {orber_iiop_socketsup, start,
					 [sup, Opts]},
		  permanent, 10000, supervisor, [orber_iiop_socketsup]} | 
		 ChildSpec1
		]
	end,
    {ok, {SupFlags, ChildSpec}}.





%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(_Req, _From, State) ->
    {reply, ok, State}.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
add_user_context([], UserCtx) -> UserCtx;
add_user_context(SysCtx, []) -> SysCtx;
add_user_context(SysCtx, UserCtx) -> SysCtx ++ UserCtx.

decode_reply_body(false, _ObjKey, _Op, ReplyHeader, Version, TypeCodes, 
		  Rest, Len, ByteOrder, Bytes) ->
    case ReplyHeader#reply_header.reply_status of
	'no_exception' ->
	    {R, P, _} = cdr_decode:dec_reply_body(Version, TypeCodes, Rest, Len, ByteOrder, Bytes),
	    {R, P};
	'system_exception' ->
	    {R, _} = cdr_decode:dec_system_exception(Version, Rest, Len, ByteOrder),
	    {R, []};
	'user_exception' ->
	    {R, _} = cdr_decode:dec_user_exception(Version, Rest, Len, ByteOrder),
	    {R, []};
	'location_forward' ->
	    {R, _, _} = cdr_decode:dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
						  Rest, Len, ByteOrder, Bytes),
	    {R, []};
	'location_forward_perm' ->
	    {R, _, _} = cdr_decode:dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
						  Rest, Len, ByteOrder, Bytes),
	    {R, []};
	'needs_addressing_mode' ->
	    {R, _, _} = cdr_decode:dec_reply_body(Version, {'tk_short', [],[]}, 
						  Rest, Len, ByteOrder, Bytes),
	    {R, []}
    end;
decode_reply_body(Interceptors, ObjKey, Op, ReplyHeader, Version, TypeCodes, 
		  RestIn, Len, ByteOrder, Bytes) ->
    Rest = 
        case Interceptors of
            {portable, _PIs} ->
                RestIn;
            {native, Ref, PIs} ->
                orber_pi:in_reply_enc(PIs, ObjKey, 
				      ReplyHeader#reply_header.service_context, 
				      Op, Ref, RestIn)
        end,
    Reply = 
	case ReplyHeader#reply_header.reply_status of
	    'no_exception' ->
		{R, P, _} = cdr_decode:dec_reply_body(Version, TypeCodes, Rest, Len, ByteOrder, Bytes),
		{R, P};
	    'system_exception' ->
		{R, _} = cdr_decode:dec_system_exception(Version, Rest, Len, ByteOrder),
		{R, []};
	    'user_exception' ->
		{R, _} = cdr_decode:dec_user_exception(Version, Rest, Len, ByteOrder),
		{R, []};
	    'location_forward' ->
		{R, _, _} = cdr_decode:dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
						      Rest, Len, ByteOrder, Bytes),
		{R, []};
	    'location_forward_perm' ->
		{R, _, _} = cdr_decode:dec_reply_body(Version, {{'tk_objref', "", ""}, [],[]}, 
						      Rest, Len, ByteOrder, Bytes),
		{R, []};
	    'needs_addressing_mode' ->
		{R, _, _} = cdr_decode:dec_reply_body(Version, {'tk_short', [],[]}, 
						      Rest, Len, ByteOrder, Bytes),
		{R, []}
	end,
        case Interceptors of
            {portable, _PI} ->
                Reply;
            {native, Refs, PI} ->
                orber_pi:in_reply(PI, ObjKey, 
				  ReplyHeader#reply_header.service_context, 
				  Op, Refs, Reply)
        end.
    
%% "Plain" TCP/IP.
connect(Host, Port, Objkey, Timeout, Index, 
	#host_data{protocol = normal, csiv2_mech = undefined} = HostData, 
	TaggedProfile, IOR) ->
    connect2([{Host, Port}], Objkey, Timeout, Index, HostData, 
	     TaggedProfile, IOR, []);
%% "Plain" SSL
connect(Host, _, Objkey, Timeout, Index, 
	#host_data{protocol = ssl, 
		   ssl_data = #'SSLIOP_SSL'{port = Port}, 
		   csiv2_mech = undefined} = HostData,
	TaggedProfile, IOR) ->
    connect2([{Host, Port}], Objkey, Timeout, Index, HostData, 
	     TaggedProfile, IOR, []);
%% CSIv2 over SSL (TAG_TLS_SEC_TRANS) using the SAS protocol. Note port must equal 0.
connect(_Host, 0, Objkey, Timeout, Index, 
	#host_data{protocol = ssl, 
		   csiv2_mech = 
		   #'CSIIOP_CompoundSecMech'{target_requires = _TR} = _Mech,
		   csiv2_addresses = Addresses} = HostData,
	TaggedProfile, IOR) ->
    Ctx = [#'IOP_ServiceContext'
	   {context_id=?IOP_SecurityAttributeService,
	    context_data = #'CSI_SASContextBody'
	    {label = ?CSI_MsgType_MTEstablishContext, 
	     value = #'CSI_EstablishContext'
	    {client_context_id = 0, %% Always 0 when stateless.
	     authorization_token = 
	     [#'CSI_AuthorizationElement'{the_element = []}],
	     identity_token = 
	     #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTAbsent, 
				  value = true}, 
	     client_authentication_token = []}}}],
    connect2(Addresses, Objkey, Timeout, Index, HostData, 
	     TaggedProfile, IOR, Ctx);
%% CSIv2 over SSL (TAG_NULL_TAG) using the SAS protocol.
connect(Host, _, Objkey, Timeout, Index, 
	#host_data{protocol = ssl, 
		   ssl_data = #'SSLIOP_SSL'{port = Port}, 
		   csiv2_mech = Mech} = HostData,
	TaggedProfile, IOR) when record(Mech, 'CSIIOP_CompoundSecMech') ->
    connect2([{Host, Port}], Objkey, Timeout, Index, HostData, 
	     TaggedProfile, IOR, []);
%% CSIv2 over TCP (TAG_NULL_TAG) using the SAS protocol.
connect(Host, Port, Objkey, Timeout, Index, 
	#host_data{protocol = normal, 
		   csiv2_mech = Mech} = HostData,
	TaggedProfile, IOR) when record(Mech, 'CSIIOP_CompoundSecMech') ->
    connect2([{Host, Port}], Objkey, Timeout, Index, HostData, 
	     TaggedProfile, IOR, []);
connect(_Host, _Port, _Objkey, _Timeout, _Index, HostData, _TaggedProfile, IOR) ->
    orber:dbg("[~p] orber_iiop:connect(~p)~n"
	      "Unable to use the supplied IOR.~n"
	      "Connection Data: ~p", [?LINE, IOR, HostData], ?DEBUG_LEVEL),
    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO}).



connect2(HostPort, Objkey, Timeout, Index, HostData, TaggedProfile, IOR, Ctx) ->
    case try_connect(HostPort, HostData#host_data.protocol, Timeout, HostData, Ctx) of
	error ->
	    Alts = iop_ior:get_alt_addr(TaggedProfile),
	    case try_connect(Alts, HostData#host_data.protocol, Timeout, HostData, Ctx) of
		error ->
		    case iop_ior:get_key(IOR, Index) of
			undefined ->
			    corba:raise(#'COMM_FAILURE'{completion_status = ?COMPLETED_NO});
			{'external', {NewHost, NewPort, NewObjkey, NewIndex,
				      NewTaggedProfile, NewHostData}} ->
			    connect(NewHost, NewPort, NewObjkey, Timeout, [NewIndex|Index], 
				    NewHostData, NewTaggedProfile, IOR);
			_What ->
			    orber:dbg("[~p] orber_iiop:connect2(~p)~n"
				      "Illegal IOR; contains a mixture of local and external profiles.", 
				      [?LINE, IOR], ?DEBUG_LEVEL),
			    corba:raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
		    end;
		X ->
		    {X, Objkey, HostData#host_data.version}
	    end;
	X ->
	    {X, Objkey, HostData#host_data.version}
    end.

try_connect([], _, _, _, _) ->
    error;
try_connect([{Host, Port}|T], SocketType, Timeout, HostData, Ctx) ->
    case catch orber_iiop_pm:connect(Host, Port, SocketType, Timeout,
				     HostData#host_data.charset,
				     HostData#host_data.wcharset) of
	{'EXCEPTION', _PMExc} ->
	    try_connect(T, SocketType, Timeout, HostData, Ctx);
	{'EXIT',{timeout,_}} ->
	    orber:dbg("[~p] orber_iiop:try_connect(~p, ~p, ~p)~n"
		      "Connect attempt timed out", 
		      [?LINE, Host, Port, Timeout], ?DEBUG_LEVEL),
	    try_connect(T, SocketType, Timeout, HostData, Ctx);
	{'EXIT', What} ->
	    orber:dbg("[~p] orber_iiop:try_connect(~p, ~p, ~p)~n"
		      "Connect attempt resulted in: ~p", 
		      [?LINE, Host, Port, Timeout, What], ?DEBUG_LEVEL),
	    try_connect(T, SocketType, Timeout, HostData, Ctx);
	{P, [], Int} ->
	    {P, Ctx, Int};
	{P, Ctx2, Int} when Ctx == [] ->
	    {P, Ctx2, Int};
	{P, Ctx2, Int} ->
	    {P, Ctx++Ctx2, Int}
    end.
    
