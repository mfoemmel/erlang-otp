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
%% Parts of this module is loosely based on code initially developed by 
%% Johan Blom at Mobile Arts AB

-module(http_response).

-include("http.hrl").

%% API
-export([parse/1, headers/2, result/3, send/2, error/2]).

%% Callback API - used for example if the header/body is received a
%% little at a time on a socket. 
-export([parse_version/1, parse_status_code/1, parse_reason_phrase/1,
	 parse_headers/1, whole_body/1, whole_body/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

parse([Bin, MaxHeaderSize]) ->
    parse_version(Bin, [], MaxHeaderSize, []).

whole_body([Bin, Body, Length])  ->
    whole_body(<<Body/binary, Bin/binary>>, Length).

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
parse_version([Bin, Version, MaxHeaderSize, Result]) ->
    parse_version(Bin, Version, MaxHeaderSize, Result).

parse_status_code([Bin, Code, MaxHeaderSize, Result]) ->
    parse_status_code(Bin, Code, MaxHeaderSize, Result).

parse_reason_phrase([Bin, Phrase, MaxHeaderSize, Result]) ->
    parse_reason_phrase(Bin, Phrase, MaxHeaderSize,Result).

parse_headers([Bin, Header, Headers, MaxHeaderSize, Result]) ->
    parse_headers(Bin, Header, Headers, MaxHeaderSize, Result).
    
whole_body(Body, Length) ->
    case size(Body) of
	N when N < Length, N > 0->
	    {?MODULE, whole_body, [Body, Length]};
	Length when Length > 0 ->
	    {ok, Body};
	_ ->
	    {?MODULE, whole_body, [Body, Length]}  
	end.

%%-------------------------------------------------------------------------
%% headers(HeaderList, #http_response_h{}) -> #http_response_h{}
%%   HeaderList - ["HeaderField:Value"]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a http_response_h-record used internally to
%%              handle http-headers.
%%-------------------------------------------------------------------------
headers([], Headers) ->
    Headers;

headers([Header | Tail], Headers) ->  
    {Key, [$: | Value]} =
	lists:splitwith(fun($:) -> false; (_) -> true end, Header), 
    headers(Tail, headers(httpd_util:to_lower(string:strip(Key)), 
			  string:strip(Value), Headers)).

%%-------------------------------------------------------------------------
%% result(Response, Request, Session) ->
%%   Response -
%%   Request - 
%%   Session -
%%                                   
%% Description: Checks the status code ...
%%-------------------------------------------------------------------------
result({{_,100,_}, _, _}, Request, Session) ->
    status_continue(Request, Session);
result(Response = {{_, 300, _}, _, _}, Request, Session) ->
    status_multiple_choices(Response, Request, Session);
result(Response = {{_, 301, _}, _, _}, Request, Session) ->
    status_moved_permanently(Response, Request,Session);
result(Response = {{_, 302, _}, _, _}, Request, Session) ->
    status_found(Response, Request, Session);
result(Response = {{_, 303, _}, _, _}, Request, Session) ->
    status_see_other(Response, Request, Session);
result(Response = {{_,304,_}, _, _}, Request, Session) ->
    status_not_modified(Response, Request,Session);
result(Response = {{_,305,_}, _, _}, Request, Session) ->
    status_use_proxy(Response, Request, Session);
result(Response = {{_,307,_}, _, _}, Request, Session) ->
    status_temporary_redirect(Response, Request,Session);
result(Response = {{_,500,_}, _, _}, Request, Session) ->
     status_server_error_50x(Response, Request, Session);
result(Response = {{_,501,_}, _, _}, Request, Session) ->
     status_server_error_50x(Response, Request, Session);
result(Response = {{_,502,_}, _, _}, Request, Session) ->
     status_server_error_50x(Response, Request, Session);
result(Response = {{_,503,_}, _, _}, Request, Session) ->
    status_service_unavailable(Response, Request, Session);
result(Response = {{_,504,_}, _, _}, Request, Session) ->
    status_server_error_50x(Response, Request, Session);
result(Response = {{_,505,_}, _, _}, Request, Session) ->
    status_server_error_50x(Response, Request, Session);
result(Response, Request, Session) ->
    transparent(Response, Request, Session).

send(To, Msg) -> 
    To ! {http, Msg}.

%%%========================================================================
%%% Internal functions
%%%========================================================================
parse_version(<<>>, Version, MaxHeaderSize, Result) ->
    {?MODULE, parse_version, [Version, MaxHeaderSize,Result]};
parse_version(<<?SP, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_status_code(Rest, [], MaxHeaderSize,
		      [lists:reverse(Version) | Result]);	
parse_version(<<Octet, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_version(Rest, [Octet | Version], MaxHeaderSize,Result).

parse_status_code(<<>>, StatusCodeStr, MaxHeaderSize, Result) -> 
    {?MODULE, parse_status_code, [StatusCodeStr, MaxHeaderSize, Result]};
parse_status_code(<<?SP, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize, Result) -> 
    parse_reason_phrase(Rest, [], MaxHeaderSize, 
			[list_to_integer(lists:reverse(StatusCodeStr)) | 
			 Result]);
parse_status_code(<<Octet, Rest/binary>>, StatusCodeStr, 
		  MaxHeaderSize,Result) ->
    parse_status_code(Rest, [Octet | StatusCodeStr], MaxHeaderSize, Result).

parse_reason_phrase(<<>>, Phrase, MaxHeaderSize, Result) ->
    {?MODULE, parse_reason_phrase, [Phrase, MaxHeaderSize,Result]};
parse_reason_phrase(<<?CR, ?LF, Rest/binary>>, Phrase, 
		    MaxHeaderSize, Result) ->
    parse_headers(Rest, [], [], MaxHeaderSize,
		  [lists:reverse(Phrase) | Result]);  
parse_reason_phrase(<<Octet, Rest/binary>>, Phrase, MaxHeaderSize, Result) ->
    parse_reason_phrase(Rest, [Octet | Phrase], MaxHeaderSize, Result).

parse_headers(<<>>, Header, Headers, MaxHeaderSize, Result) -> 
    {?MODULE, parse_headers, [Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    HTTPHeaders = [lists:reverse(Header) | Headers],
    Length = lists:foldl(fun(H, Acc) -> length(H) + Acc end,
			   0, HTTPHeaders),
    case ((Length =< MaxHeaderSize) or (MaxHeaderSize == nolimit)) of
 	true ->   
	    ResponseHeaderRcord = headers(HTTPHeaders, #http_response_h{}),
	    {ok, list_to_tuple(
		   lists:reverse([Body, ResponseHeaderRcord | Result]))};
 	false ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}})
    end;
parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet], 
		  [lists:reverse(Header) | Headers], MaxHeaderSize, Result);
parse_headers(<<Octet, Rest/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet | Header], Headers, MaxHeaderSize, Result).

headers("connection", Value, Headers) ->
    Headers#http_response_h{connection = Value};
headers("content-length", Value, Headers) ->
    Headers#http_response_h{content_length = Value};
headers("content-type", Value, Headers) ->
    Headers#http_response_h{content_type = Value};
headers("transfer-encoding", Value, Headers) ->
    Headers#http_response_h{transfer_encoding = Value};
headers("location", Value, Headers) ->
    Headers#http_response_h{location = Value};
headers("retry-after", Value, Headers) ->
    Headers#http_response_h{retry_after = Value};
headers(Key, Value, Headers) ->
    Headers#http_response_h{other=
			    [{Key, Value} | Headers#http_response_h.other]}.

%%% RFC2616, Section 10.1.1
%%% Note:
%%% - Only act on the 100 status if the request included the
%%%   "Expect:100-continue" header, otherwise just ignore this response.
status_continue(Req = #http_request_h{expect="100-continue"}, Session) ->
    {_, Body} = Req#request.content,
    http_transport:send(Session#tcp_session.scheme, 
			Session#tcp_session.socket, Body),
    ok;

status_continue(_Req, _Session) ->
    ok.

%%% RFC2616, Section 10.3.1: Multiple Choices 
%%% Note: - If response to a HEAD request, the Content-Type/Body both
%%% should be empty. The behaviour on an empty Content-Type or Body
%%% is unspecified.  However, e.g. "Apache/1.3" servers returns both
%%% empty if the header 'if-modified-since: Date' was sent in the
%%% request and the content is "not modified" (instead of 304). Thus
%%% implicitly giving the cache as the only choice.
status_multiple_choices(Response = {_, Headers, _}, 
			Request = #request{settings = 
					   #http_options{autoredirect =
							    true}
					  }, Session) ->
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_multiple_choices/3,
	    ServerClose);

status_multiple_choices(Response = {_, Headers, _}, Req, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = connection_status(Session#tcp_session.client_close, 
					 ServerClose),
    {ConnectionStatus, {Req#request.id, format_response(Response)}}.

%%% RFC2616, Section 10.3.2: Moved Permanently
status_moved_permanently(Response = {_, Headers, _}, 
			 Request = #request{settings = 
					    #http_options{autoredirect 
							     = true}
					   }, Session) ->
    
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_moved_permanently/3,
	     ServerClose);  

status_moved_permanently(Response =  {_, Headers, _}, Request, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = 
	connection_status(Session#tcp_session.client_close, ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.

%%% RFC2616, Section 10.3.3: Found
status_found(Response = {_, Headers, _}, 
	     Request = #request{settings = 
				#http_options{autoredirect = true}}, 
	     Session) ->
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_found/3,
	     ServerClose);  

status_found(Response = {_, Headers, _}, Request, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = connection_status(Session#tcp_session.client_close, 
					 ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.

%%% RFC2616,  Section 10.3.4: See Other
status_see_other(Response = {_, Headers, _}, 
		 Request = #request{settings = 
				    #http_options{autoredirect = true}}, 
		 Session) ->
    
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_see_other/3,
	    ServerClose);  

status_see_other(Response = {_, Headers, _},Request, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus =
	connection_status(Session#tcp_session.client_close, ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.

%%% RFC2616,  Section 10.3.5: Not Modified
status_not_modified(Response = {_, Headers, _}, 
		    Request =  #request{settings = 
					#http_options{autoredirect 
							 = true}}, 
		    Session) ->
    
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_see_other/3,
	    ServerClose);  
   
status_not_modified(Response = {_, Headers, _}, Request, Session) ->
    ServerClose=is_server_closing(Headers),
    ConnectionStatus = connection_status(Session#tcp_session.client_close,
					 ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.


%%% RFC2616, Section 10.3.6: Use Proxy
status_use_proxy(Response = {_, Headers, _}, 
		 Request = #request{settings = 
				    #http_options{autoredirect = true}}, 
		 Session) ->
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_use_proxy/3,
	     ServerClose);  
    
status_use_proxy(Response = {_, Headers, _},Request, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = 
	connection_status(Session#tcp_session.client_close, ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.


%%% RFC2616, Section 10.3.8: Temporary Redirect
status_temporary_redirect(Response = {_, Headers, _}, 
			  Request = #request{settings = 
					     #http_options{autoredirect = 
							      true}}, 
			  Session) ->
    
    ServerClose = is_server_closing(Headers),
    redirect(Response, Request, Session, fun status_temporary_redirect/3,
	    ServerClose);

status_temporary_redirect(Response = {_, Headers, _}, Request, Session) ->
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = connection_status(Session#tcp_session.client_close, 
					 ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.

%%% RFC2616, Section 10.5.4: Service Unavailable
%%%    The server is currently unable to handle the request due to a
%%%    temporary overloading or maintenance of the server. The implication
%%%    is that this is a temporary condition which will be alleviated after
%%%    some delay. If known, the length of the delay MAY be indicated in a
%%%    Retry-After header. If no Retry-After is given, the client SHOULD
%%%    handle the response as it would for a 500 response.
%% Note:
%% - This session is now considered busy, thus cancel any requests in the
%%   pipeline and close the session.
%% FIXME! Implement a user option to automatically retry if the 'Retry-After'
%%        header is given.
status_service_unavailable(Resp, Req, _Session) ->
    {stop, {Req#request.id,Resp}}.

%%% Received a 50x Status code (~ "Service Error")
%%%   Response status codes beginning with the digit "5" indicate cases in
%%%   which the server is aware that it has erred or is incapable of
%%%   performing the request.
status_server_error_50x(Resp, Req, _Session) ->
    {stop,{Req#request.id, Resp}}.


redirect(Response = {StatusCode, Headers, Body}, Req, Session, Fun, 
	 ServerClose) ->
   
    case Headers#http_response_h.location of
	undefined ->
	    ConnectionStatus = 
		connection_status(Session#tcp_session.client_close, 
				  ServerClose),
	    {ConnectionStatus, {Req#request.id, format_response(Response)}};
	RedirUrl ->
	    Scheme = Session#tcp_session.scheme,
	    case http_uri:parse(RedirUrl) of
		{error, no_scheme} when
		      (Req#request.settings)#http_options.relaxed ->
		    NewLocation = fix_relative_uri(Req,RedirUrl),
		    Fun({StatusCode, Headers#http_response_h{
				       location = NewLocation},
			 Body},Req,Session);
		{error, Reason} ->
		    {error,  Reason};
		{Scheme, Host, Port, Path,  Query} -> % Automatic redirection
		    NewHeaders = (Req#request.headers)#http_request_h{host 
								      = Host},
		    NewReq = Req#request{redircount = Req#request.redircount+1,
					 headers = NewHeaders,
					 address = {Host,Port},
					 path = Path,
					 pquery = Query},
		    handle_redirect(Session#tcp_session.client_close,
				    ServerClose,
				    NewReq,Session)
	    end
    end.

%%% Guessing that we received a relative URI, fix it to become an absoluteURI
fix_relative_uri(Request, RedirUrl) ->
    {Server, Port} = Request#request.address,
    Path = Request#request.path,
    atom_to_list(Request#request.scheme) ++ "://" ++ Server ++ ":" ++ Port
	++ Path ++ RedirUrl.
    

%%% Handles requests for redirects
%%% The redirected request might be:
%%% - FIXME! on another TCP session, another scheme
%%% - on the same TCP session, same scheme
%%% - on another TCP session , same scheme
%%% However, in all cases treat it as a new request, with redircount updated.
%%%
%%% The redirect may fail, but this not a reason to close this session.
%%% Instead return a error for this request, and continue as ok.
handle_redirect(ClientClose, ServerClose, Request, _Session) ->
    case httpc_manager:request(Request) of
	{ok, _} -> 
	    ConnectionStatus =
		connection_status(ClientClose, ServerClose),
	    {ConnectionStatus, ""};
	{error,Reason} ->
	    Error = error(Request, Reason),
	    ConnectionStatus = 
		connection_status(ClientClose, ServerClose),
	    {ConnectionStatus, Error}
    end.

%%% Check if the persistent connection flag is false (ie client request
%%% non-persistive connection), or if the server requires a closed connection
%%% (by sending a "Connection: close" header). If the connection required
%%% non-persistent, we may close the connection immediately.
connection_status(ClientClose, ServerClose) ->
    case {ClientClose, ServerClose} of
	{false,false} ->
	    ok;
	{false,true} -> % The server requests this session to be closed.
	    stop;
	{true,_} -> % The client requested a non-persistent connection
	    stop
    end.


error(#request{id = Id}, Reason) ->
    {Id, {error, Reason}}.

transparent(Response = {_, Headers, _}, Request, Session) ->    
    ServerClose = is_server_closing(Headers),
    ConnectionStatus = 
	connection_status(Session#tcp_session.client_close,
				    ServerClose),
    {ConnectionStatus, {Request#request.id, format_response(Response)}}.


%%% Server response:
%%%    Check "Connection" header if server requests session to be closed.
%%%    No 'close' means returns false
is_server_closing(Headers) when record(Headers,http_response_h) ->
    case Headers#http_response_h.connection of
	"close" ->
	    true;
	"keep-alive" ->
	    false;
	Value when list(Value) ->
	    true;
	_ ->
	    false
    end.

format_response({StatusLine, Headers, Body}) ->
    {StatusLine, header_list(Headers), Body}.

header_list(Headers) ->
    HeaderFields = record_info(fields, http_response_h) -- [other],
    HeaderList = lists:foldl(fun(Key, Acc) -> 
				     case key_value_tuple(Key, Headers) of
					 undefined ->
					     Acc;
					 Tuple ->
					     [Tuple | Acc]
				     end
			     end,
			     [], HeaderFields),
    lists:reverse(HeaderList) ++ Headers#http_response_h.other.

key_value_tuple(cache_control, Headers) ->
    key_value_tuple("cache-control", Headers#http_response_h.cache_control);
key_value_tuple(connection, Headers) ->
    key_value_tuple("connection", Headers#http_response_h.connection);
key_value_tuple(date, Headers) ->
    key_value_tuple("date", Headers#http_response_h.date);
key_value_tuple(pragma, Headers) ->
    key_value_tuple("pragma", Headers#http_response_h.pragma);
key_value_tuple(trailer, Headers) ->
    key_value_tuple("trailer", Headers#http_response_h.trailer);
key_value_tuple(transfer_encoding, Headers) ->
    key_value_tuple("transfer-encoding", 
		    Headers#http_response_h.transfer_encoding);
key_value_tuple(upgrade, Headers) ->
    key_value_tuple("upgrade", Headers#http_response_h.upgrade);
key_value_tuple(via, Headers) ->
    key_value_tuple("via", Headers#http_response_h.via);
key_value_tuple(warning, Headers) ->
    key_value_tuple("warning", Headers#http_response_h.warning);
key_value_tuple(accept_ranges, Headers) ->
    key_value_tuple("accept-ranges", Headers#http_response_h.accept_ranges);
key_value_tuple(age, Headers) ->
    key_value_tuple("age", Headers#http_response_h.age);
key_value_tuple(etag, Headers) ->
    key_value_tuple("etag", Headers#http_response_h.etag);
key_value_tuple(location, Headers) ->
    key_value_tuple("location", Headers#http_response_h.location);
key_value_tuple(proxy_authenticate, Headers) ->
    key_value_tuple("proxy_authenticate",
		    Headers#http_response_h.proxy_authenticate);
key_value_tuple(retry_after, Headers) ->
    key_value_tuple("retry-after", Headers#http_response_h.retry_after);
key_value_tuple(server, Headers) ->
    key_value_tuple("server", Headers#http_response_h.server);
key_value_tuple(vary, Headers) ->
    key_value_tuple("vary", Headers#http_response_h.vary);
key_value_tuple(www_authenticate, Headers) ->
    key_value_tuple("www-authentication", 
		    Headers#http_response_h.www_authenticate);
key_value_tuple(allow, Headers) ->
    key_value_tuple("allow", Headers#http_response_h.allow);
key_value_tuple(content_encoding, Headers) ->
    key_value_tuple("content-encoding", 
		    Headers#http_response_h.content_encoding);
key_value_tuple(content_language, Headers) ->
    key_value_tuple("content-language", 
		    Headers#http_response_h.content_language);
key_value_tuple(content_length, Headers) ->
    case Headers#http_response_h.content_length of
	"0" ->
	    undefined;
	_ -> 
	    key_value_tuple("content-length", 
			    Headers#http_response_h.content_length)
    end;
key_value_tuple(content_location, Headers) ->
    key_value_tuple("content-location", 
		    Headers#http_response_h.content_location);
key_value_tuple(content_md5, Headers) ->
    key_value_tuple("content-md5", 
		    Headers#http_response_h.content_md5);
key_value_tuple(content_range, Headers) ->
    key_value_tuple("content-range", Headers#http_response_h.content_range);
key_value_tuple(content_type, Headers) ->
    key_value_tuple("content-type", Headers#http_response_h.content_type);
key_value_tuple(expires, Headers) ->
    key_value_tuple("expires", Headers#http_response_h.expires);
key_value_tuple(last_modified, Headers) ->
    key_value_tuple("last-modified", Headers#http_response_h.last_modified);
key_value_tuple(_, undefined) ->
    undefined;
key_value_tuple(Key, Value) ->
    {Key, Value}.

