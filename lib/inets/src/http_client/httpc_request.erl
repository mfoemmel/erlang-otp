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

-module(httpc_request).

-include("http.hrl").

%%% Client API
-export([send/3, is_idempotent/1, is_client_closing/1]).

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
%%-------------------------------------------------------------------------
%% send(MaybeProxy, Request) ->
%%      MaybeProxy - {Host, Port}
%%      Host = string()
%%      Port = integer()
%%	Request - #request{}
%%	Socket - socket()
%%      CookieSupport - enabled | disabled | verify
%%                                   
%% Description: Composes and sends a HTTP-request.
%%-------------------------------------------------------------------------
send(SendAddr, #request{method = Method, scheme = Scheme,
			  path = Path, pquery = Query, headers = Headers,
			  content = Content, address = Address, 
			  abs_uri = AbsUri},
     Socket) -> 
    
    {NewHeaders, Body} = post_data(Method, Headers, Content),
    
    Uri = case Address of
	      SendAddr ->
		  Path ++ Query;
	      _Proxy ->
		  AbsUri
	  end,
    
    Message = 
	lists:append([method(Method), " ", Uri, " HTTP/1.1", ?CRLF, 
		      http_request:http_headers(NewHeaders), ?CRLF, 
		      Body]),
    
    http_transport:send(Scheme, Socket, Message).

is_idempotent(head) -> 
    true;
is_idempotent(get) ->
    true;
is_idempotent(_) ->
    false.

is_client_closing(Headers) ->
    case Headers#http_request_h.connection of
	"close" ->
	    true;
	 _ ->
	    false
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
post_data(Method, Headers, {ContentType, Body}) 
  when Method == post; Method == put ->
    ContentLength = length(Body),
    NewBody = case Headers#http_request_h.expect of
		  "100-continue" ->
		      "";
		  _ ->
		      Body
	      end,
    {Headers#http_request_h{'content-type' = ContentType, 
			    'content-length' = ContentLength},
     NewBody};
     
post_data(_, Headers, _) ->
    {Headers, ""}.

method(Method) ->
    http_util:to_upper(atom_to_list(Method)).
