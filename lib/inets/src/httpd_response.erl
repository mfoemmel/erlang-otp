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
-module(httpd_response).
-export([send/5,send_status/5]).

-include("httpd.hrl").

-define(VMODULE,"RESPONSE").
-include("httpd_verbosity.hrl").

%% send

send(SocketType, Socket, Request, ConfigDB, InitData) ->
    ?vtrace("send -> Request: ~p", [Request]),
    case httpd_parse:request(Request) of
	{not_implemented,RequestLine,Method,RequestURI,HTTPVersion} ->
	    send_status(SocketType,Socket,501,{Method,RequestURI,HTTPVersion},
			ConfigDB);
	{bad_request, {forbidden, URI}} ->
	    send_status(SocketType, Socket, 403, URI, ConfigDB);
	{bad_request, Reason} ->
	    send_status(SocketType, Socket, 400, none, ConfigDB);
	{ok,[Method,RequestURI,HTTPVersion,RequestLine,ParsedHeader,EntityBody]} ->
	    ?vtrace("send -> request parsed:"
		"~n    Method:      ~p"
		"~n    RequestURI:  ~p"
		"~n    HTTPVersion: ~p"
		"~n    RequestLine: ~p",
		[Method, RequestURI, HTTPVersion, RequestLine]),

	    Modules = httpd_util:lookup(ConfigDB,
					modules,
					[mod_get, mod_head, mod_log]),
	    KeepAlive =
		case httpd_util:lookup(ConfigDB, keep_alive, close) of
		    close ->
			close;
		    Set ->
			case httpd_util:key1search(ParsedHeader,
						   "Connection", 
						   close) of
			    "Keep-Alive" ->
				keep_alive;
			    "keep-alive" ->
				keep_alive;
			    _ ->
				close
			end
		end,
	    Info = #mod{init_data     = InitData,
			data          = [],
			socket_type   = SocketType,
			socket        = Socket,
			config_db     = ConfigDB,
			method        = Method,
			request_uri   = RequestURI,
			http_version  = HTTPVersion,
			request_line  = RequestLine,
			parsed_header = ParsedHeader,
			entity_body   = EntityBody,
			connection    = KeepAlive},
	    case traverse_modules(Info,Modules) of
		done ->
		    Info;
		{proceed,Data} ->
		    case httpd_util:key1search(Data, status) of
			{StatusCode,PhraseArgs,Reason} ->
			    send_status(SocketType, Socket,
					StatusCode, PhraseArgs, ConfigDB);
			undefined ->
			    case httpd_util:key1search(Data,response) of
				{already_sent,StatusCode,Size} ->
				    Info;
				{StatusCode,Response} ->
				    send_response(SocketType, Socket,
						  StatusCode, Response);
				undefined ->
				    send_status(SocketType, Socket,
						500, none, ConfigDB),
				    Info
			    end
		    end
	    end
    end.

%% traverse_modules

traverse_modules(Info,[]) ->
    {proceed,Info#mod.data};
traverse_modules(Info,[Module|Rest]) ->
    case catch apply(Module,do,[Info]) of
	{'EXIT',Reason} ->
	    ?vlog("traverse_modules -> exit reason: ~p",[Reason]),
	    error_logger:error_report(Reason),
	    done;
	done ->
	    done;
	{break,NewData} ->
	    {proceed,NewData};
	{proceed,NewData} ->
	    traverse_modules(Info#mod{data=NewData},Rest)
    end.

%% send_status %%

send_status(SocketType,Socket,StatusCode,PhraseArgs,ConfigDB) ->
    ?DEBUG("send_status -> entry with"
	"~n   StatusCode: ~p"
	"~n   PhraseArgs: ~p",
	[StatusCode,PhraseArgs]),
    Header = httpd_util:header(StatusCode, "text/html")++"\r\n\r\n",
    ReasonPhrase = httpd_util:reason_phrase(StatusCode),
    Message = httpd_util:message(StatusCode,PhraseArgs,ConfigDB),
    Body = "<HTML>
<HEAD>
<TITLE>"++ReasonPhrase++"</TITLE>
</HEAD>
<BODY>
<H1>"++ReasonPhrase++"</H1>\n"++Message++"\n</BODY>
</HTML>\n",
    httpd_socket:deliver(SocketType,Socket,[Header,Body]).

%% send_response

send_response(SocketType,Socket,StatusCode,Response) ->
    ?DEBUG("send_status -> entry with"
	"~n   StatusCode: ~p"
	"~n   Response:   ~p",
	[StatusCode,Response]),
    Header = httpd_util:header(StatusCode),
    httpd_socket:deliver(SocketType,Socket,[Header,Response]).
