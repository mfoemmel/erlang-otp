% ``The contents of this file are subject to the Erlang Public License,
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
%% This module is very loosely based on code initially developed by 
%% Johan Blom at Mobile Arts AB
%% Description:
%%% This version of the HTTP/1.1 client supports:
%%%      - RFC 2616 HTTP 1.1 client part
%%%      - RFC 2818 HTTP Over TLS

-module(http).

%% API
-export([request/4, cancel_request/1, set_options/1]).

-include("http.hrl").

%%%=========================================================================
%%%  API
%%%=========================================================================

%%--------------------------------------------------------------------------
%% request(Method, Request, HTTPOptions, Options) ->
%%           {ok, {StatusLine, Headers, Body}} | {ok, {Status, Body}} |
%%           {ok, RequestId} | {error,Reason} 
%%
%%	Method - atom() = head | get | put | post | trace | options| delete 
%%	Request - {Url, Headers} | {Url, Headers, ContentType, Body} 
%%	Url - string() 
%%	HTTPOptions - [HttpOption]
%%	HTTPOption - {timeout, Time} | {ssl, SSLOptions} 
%%	SSLOptions = [SSLOption]
%%	SSLOption =  {verify, code()} | {depth, depth()} | {certfile, path()} |
%%	{keyfile, path()} | {password, string()} | {cacertfile, path()} |
%%	{ciphers, string()} 
%%	Options - [Option]
%%	Option - {sync, Boolean} | {body_format, BodyFormat} | 
%%	{full_result, Boolean}
%%	StatusLine = {HTTPVersion, StatusCode, ReasonPhrase}</v>
%%	HTTPVersion = string()
%%	StatusCode = integer()
%%	ReasonPhrase = string()
%%	Headers = [Header]
%%      Header = {Field, Value}
%%	Field = string()
%%	Value = string()
%%	Body = string() | binary() - HTLM-code
%%
%% Description: Sends a HTTP-request. The function can be both
%% syncronus and asynchronous in the later case the function will
%% return {ok, RequestId} and later on a message will be sent to the
%% calling process on the format {http, {RequestId, {StatusLine,
%% Headers, Body}}} or {http, {RequestId, {error, Reason}}}
%% %%--------------------------------------------------------------------------
request(Method, {Url, Headers}, HTTPOptions, Options) 
  when Method==options;Method==get;Method==head;Method==delete;Method==trace ->
    case http_uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    handle_request(Method, {ParsedUrl, Headers, [], []}, 
			   HTTPOptions, Options)
    end;
     
request(Method, {Url,Headers,ContentType,Body}, HTTPOptions, Options) 
  when Method==post;Method==put ->
    case http_uri:parse(Url) of
	{error,Reason} ->
	    {error,Reason};
	ParsedUrl ->
	    handle_request(Method, {ParsedUrl, Headers, ContentType, Body}, 
			   HTTPOptions, Options)
    end.

%%--------------------------------------------------------------------------
%% request(RequestId) -> ok
%%   RequestId - As returned by request/4  
%%                                 
%% Description: Cancels a HTTP-request.
%%-------------------------------------------------------------------------
cancel_request(RequestId) ->
    ok = httpc_manager:cancel_request(RequestId), 
    receive  
	%% If the request was allready fullfilled throw away the 
	%% answer as the request has been canceled.
	{http, {RequestId, _}} ->
	    ok 
    after 0 ->
	    ok
    end.

%%--------------------------------------------------------------------------
%% set_options(Options) ->
%%   Options - [Option]
%%   Option - {proxy, {Proxy, NoProxy}} 
%%   Proxy - {Host, Port}
%%   NoProxy - [Domain | HostName | IPAddress]                              
%% Description: Informs the httpc_manager of the new settings. Only the
%% proxy option is currently supported.  
%%-------------------------------------------------------------------------
set_options(Options) ->
    ensure_started(no_scheme),
    httpc_manager:set_options(Options).

%%%========================================================================
%%% Internal functions
%%%========================================================================
handle_request(Method, {{Scheme, Host, Port, Path, Query},
			Headers, ContentType, Body}, HTTPOptions, Options)->
    HTTPRecordOptions = http_options(HTTPOptions, #http_options{}), 
    NewHeaders = lists:map(fun({Key, Val}) -> 
				   {httpd_util:to_lower(Key), Val} end,
			   Headers),
    RecordHeaders = header_record(NewHeaders, #http_request_h{}, Host), 
    Request = #request{from = self(),
		       scheme = Scheme, address = {Host,Port},
		       path = Path, pquery = Query, method = Method,
		       headers = RecordHeaders, content = {ContentType,Body},
		       settings = HTTPRecordOptions},
    
    ensure_started(Scheme),
    
    case httpc_manager:request(Request) of
	{ok, RequestId} ->
	    Sync = httpd_util:key1search(Options, sync, true),
	    handle_answer(RequestId, Sync, Options);
	{error, Reason} ->
	    {error, Reason}
    end.

handle_answer(RequestId, false, _) ->
    {ok, RequestId};
handle_answer(RequestId, true, Options) ->
    receive
	{http, {RequestId, {StatusLine, Headers, BinBody}}} ->
	    Body = 
		case httpd_util:key1search(Options, body_format, string) of
		    string ->
			binary_to_list(BinBody);
		    _ ->
			BinBody
		end,
	    case httpd_util:key1search(Options, full_result, true) of
		true ->
		    {ok, {StatusLine, Headers, Body}};
		false ->
		    {_, Status, _} = StatusLine,
		    {ok, {Status, Body}}
	    end;
	{http, {RequestId, {error, Reason}}} ->
	    {error, Reason}
    end.
 
http_options([], Acc) ->
    Acc;
http_options([{timeout, Val} | Settings], Acc) 
  when is_integer(Val), Val >= 0->
    http_options(Settings, Acc#http_options{timeout = Val});
http_options([{timeout, infinity} | Settings], Acc) ->
    http_options(Settings, Acc#http_options{timeout = infinity});
http_options([{autoredirect, Val} | Settings], Acc)   
  when Val == true; Val == false ->
    http_options(Settings, Acc#http_options{autoredirect = Val});
http_options([{ssl, Val} | Settings], Acc) ->
    http_options(Settings, Acc#http_options{ssl = Val});
http_options([{relaxed, Val} | Settings], Acc)
  when Val == true; Val == false ->
    http_options(Settings, Acc#http_options{relaxed = Val});
http_options([{cookie, Val} | Settings], Acc) when 
  Val == true; Val == false ->
    http_options(Settings, Acc#http_options{cookie = Val});
http_options([Option | Settings], Acc) ->
    error_logger:info_report("Invalid option ignored ~p~n", [Option]),
    http_options(Settings, Acc).

header_record([], RequestHeaders, Host) ->
    validate_headers(RequestHeaders, Host);
header_record([{"expect", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{expect = Val}, Host);
header_record([{"host", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{host = Val}, Host);
header_record([{"te", Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{te = Val}, Host);  

%% All other headers are  not processed by the client so
%% put them in other even if they might not belong here ...
%% Probably a good idea to skip the record altogether in
%% this case, think about this for next version.
header_record([{Key, Val} | Rest], RequestHeaders, Host) ->
    header_record(Rest, RequestHeaders#http_request_h{
			  other = [{Key, Val} |
				   RequestHeaders#http_request_h.other]}, 
		  Host).

validate_headers(RequestHeaders = #http_request_h{te = undefined}, Host) ->
    validate_headers(RequestHeaders#http_request_h{te = ""}, Host);
validate_headers(RequestHeaders = #http_request_h{host = undefined}, Host) ->
    validate_headers(RequestHeaders#http_request_h{host = Host}, Host);
validate_headers(RequestHeaders, _) ->
    RequestHeaders.

ensure_started(Scheme) ->
    %% Start of the inets application should really be handled by the 
    %% application using inets. 
    case application:start(inets) of
	{error,{already_started,inets}} ->
	    ok;
	ok ->
	    error_logger:info_report("The inets application was not started."
				     " Has now been started as a temporary" 
				     " application.")
    end,
    
    case Scheme of
	https ->
	    %% Start of the ssl application should really be handled by the 
	    %% application using inets. 
	    case application:start(ssl) of
		{error,{already_started,ssl}} ->
		    ok;
		ok ->
		    error_logger:info_report("The ssl application was not "
					     "started. Has now been started " 
					     "as a temporary application.")
	    end;
	_ ->
	    ok
    end.
