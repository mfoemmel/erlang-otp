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
-export([send/1, send_status/3, send_status/5]).

%%code is the key for the statuscode ex: 200 404 ...
-define(HTTP_11_HEADER_FIELDS,
	[content_length, accept_ranges, cache_control, date,
	 pragma, trailer, transfer_encoding, etag, location,
	 retry_after, server, allow, 
	 content_encoding, content_language, 
	 content_location, content_MD5, content_range,
	 content_type, expires, last_modified]).

-define(HTTP_10_HEADER_FIELDS,
	[content_length, date, 
	 pragma, location, 
	 server, allow, 
	 content_encoding, 
	 content_type, last_modified]).

-define(PROCEED_RESPONSE(StatusCode, Info), 
	{proceed, 
	 [{response,{already_sent, StatusCode, 
		     httpd_util:key1search(Info#mod.data,content_length)}}]}).


-include("httpd.hrl").

-define(VMODULE,"RESPONSE").
-include("httpd_verbosity.hrl").

%% send

send(#mod{config_db = ConfigDB} = Info) ->
    ?vtrace("send -> Request line: ~p", [Info#mod.request_line]),
    Modules = httpd_util:lookup(ConfigDB,modules,[mod_get, mod_head, mod_log]),
    case traverse_modules(Info, Modules) of
	done ->
	    Info;
	{proceed, Data} ->
	    case httpd_util:key1search(Data, status) of
		{StatusCode, PhraseArgs, Reason} ->
		    ?vdebug("send -> proceed/status: ~n"
			    "~n   StatusCode: ~p"
			    "~n   PhraseArgs: ~p"
			    "~n   Reason:     ~p",
			    [StatusCode, PhraseArgs, Reason]),
		    send_status(Info, StatusCode, PhraseArgs),
		    Info;
		
		undefined ->
		    case httpd_util:key1search(Data, response) of
			{already_sent, StatusCode, Size} ->
			    ?vtrace("send -> already sent: "
				    "~n   StatusCode: ~p"
				    "~n   Size:       ~p", 
				    [StatusCode, Size]),
			    Info;
			{response, Header, Body} -> %% New way
			    send_response(Info, Header, Body),
			    Info;
			{StatusCode, Response} ->   %% Old way
			    send_response_old(Info, StatusCode, Response),
			    Info;
			undefined ->
			    ?vtrace("send -> undefined response", []),
			    send_status(Info, 500, none),
			    Info
		    end
	    end
    end.


%% traverse_modules

traverse_modules(Info,[]) ->
    {proceed,Info#mod.data};
traverse_modules(Info,[Module|Rest]) ->
    case (catch apply(Module,do,[Info])) of
	{'EXIT', Reason} ->
	    ?vlog("traverse_modules -> exit reason: ~p",[Reason]),
	    String = 
		lists:flatten(
		  io_lib:format("traverse exit from apply: ~p:do => ~n~p",
				[Module, Reason])),
	    report_error(mod_log, Info#mod.config_db, String),
	    report_error(mod_disk_log, Info#mod.config_db, String),
	    done;
	done ->
	    done;
	{break,NewData} ->
	    {proceed,NewData};
	{proceed,NewData} ->
	    traverse_modules(Info#mod{data=NewData},Rest)
    end.

%% send_status %%


send_status(#mod{socket_type = SocketType, 
		 socket      = Socket, 
		 connection  = Conn} = _Info, 100, _PhraseArgs) ->
    Header = httpd_util:header(100, Conn),
    httpd_socket:deliver(SocketType, Socket,
			 [Header, "Content-Length:0\r\n\r\n"]);

send_status(#mod{socket_type = SocketType, 
		 socket      = Socket, 
		 config_db   = ConfigDB} = _Info, StatusCode, PhraseArgs) ->
    send_status(SocketType, Socket, StatusCode, PhraseArgs, ConfigDB).

send_status(SocketType, Socket, StatusCode, PhraseArgs, ConfigDB) ->
    Header       = httpd_util:header(StatusCode, "text/html", false),
    ReasonPhrase = httpd_util:reason_phrase(StatusCode),
    Message      = httpd_util:message(StatusCode, PhraseArgs, ConfigDB),
    Body         = get_body(ReasonPhrase, Message),
    Header1 = 
	Header ++ 
	"Content-Length:" ++ 
	integer_to_list(length(Body)) ++
	"\r\n\r\n",
    httpd_socket:deliver(SocketType, Socket, [Header1, Body]).


get_body(ReasonPhrase, Message)->
    "<HTML>
       <HEAD>
           <TITLE>"++ReasonPhrase++"</TITLE>
      </HEAD>
      <BODY>
      <H1>"++ReasonPhrase++"</H1>\n"++Message++"\n</BODY>
      </HTML>\n".


%%% Create a response from the Key/Val tuples In the Head  List
%%% Body is a tuple {body,Fun(),Args}

%% send_response
%% Allowed Fields 

% HTTP-Version StatusCode Reason-Phrase
% *((general-headers
%   response-headers
%    entity-headers)CRLF)
%  CRLF
% ?(BODY)

% General Header fields
% ======================
% Cache-Control cache_control
% Connection %%Is set dependiong on the request
% Date
% Pramga
% Trailer
% Transfer-Encoding

% Response Header field
% =====================
% Accept-Ranges
% (Age) Mostly for proxys
% Etag
% Location
% (Proxy-Authenticate) Only for proxies
% Retry-After
% Server
% Vary
% WWW-Authenticate
%
% Entity Header Fields
% ====================
% Allow
% Content-Encoding
% Content-Language
% Content-Length
% Content-Location
% Content-MD5
% Content-Range
% Content-Type
% Expires
% Last-Modified
 

send_response(Info, Header, Body) ->
    ?vtrace("send_response -> (new) entry with"
	    "~n   Header:       ~p", [Header]),
    case httpd_util:key1search(Header, code) of
	undefined ->
	    %% No status code 
	    %% Ooops this must be very bad:
	    %% generate a 404 content not availible
	    send_status(Info, 404, "The file is not availible");
	StatusCode ->
	    case send_header(Info, StatusCode, Header) of
		ok ->
		    send_body(Info, StatusCode, Body);
		Error ->
		    ?vlog("head delivery failure: ~p", [Error]),
		    done   
	    end
    end.


send_header(#mod{config_db = Db,
		 socket_type  = Type, socket     = Sock, 
		 http_version = Ver,  connection = Conn} = _Info, 
	    StatusCode, Head0) ->
    ?vtrace("send_header -> entry with"
	    "~n   Ver:  ~p"
	    "~n   Conn: ~p", [Ver, Conn]),
    Head1 = create_header(Db, Ver, Head0),
    StatusLine = [Ver, " ",
		  io_lib:write(StatusCode), " ",
		  httpd_util:reason_phrase(StatusCode), "\r\n"],
    Connection = get_connection(Conn, Ver),
    Head = list_to_binary([StatusLine, Head1, Connection,"\r\n"]),
    ?vtrace("deliver head", []),
    httpd_socket:deliver(Type, Sock, Head).


send_body(_, _, nobody) ->
    ?vtrace("send_body -> no body", []),
    ok;

send_body(#mod{socket_type = Type, socket = Sock}, 
	  _StatusCode, Body) when list(Body) ->
    ?vtrace("deliver body of size ~p", [length(Body)]),
    httpd_socket:deliver(Type, Sock, Body);

send_body(#mod{socket_type = Type, socket = Sock} = Info, 
	  StatusCode, {Fun, Args}) ->
    case (catch apply(Fun, Args)) of
	close ->
	    httpd_socket:close(Type, Sock),
	    done;

	sent ->
	    ?PROCEED_RESPONSE(StatusCode, Info);
	
	{ok, Body} ->
	    ?vtrace("deliver body", []),
	    case httpd_socket:deliver(Type, Sock, Body) of
		ok ->
		    ?PROCEED_RESPONSE(StatusCode, Info);
		Error ->
		    ?vlog("body delivery failure: ~p", [Error]),
		    done
	    end;	    

	Error ->
	    ?vlog("failure of apply(~p,~p): ~p", [Fun, Args, Error]),
	    done
    end;
send_body(I, S, B) ->
    ?vinfo("BAD ARGS: "
	   "~n   I: ~p"
	   "~n   S: ~p"
	   "~n   B: ~p", [I, S, B]),
    exit({bad_args, {I, S, B}}).
    

%% Return a HTTP-header field that indicates that the 
%% connection will be inpersistent
get_connection(true,"HTTP/1.0")->
    "Connection:close\r\n";
get_connection(false,"HTTP/1.1") ->
    "Connection:close\r\n";
get_connection(_,_) ->
    "".


disable_chunked_send(Db) ->
    httpd_util:lookup(Db, disable_chunked_transfer_encoding_send, false).

create_header(Db, "HTTP/1.1", Data) ->
    DisableChunkedSend = disable_chunked_send(Db),
    Disable = [{transfer_encoding, DisableChunkedSend}],
    create_header1(?HTTP_11_HEADER_FIELDS, Data, Disable);
create_header(_, _, Data) ->
    create_header1(?HTTP_10_HEADER_FIELDS, Data, []).

create_header1(Fields, Data, Disable) ->
    Fun = 
	fun(Field) ->
		transform(Field, 
			  httpd_util:key1search(Data, Field),
			  Disable)
	end,
    mapfilter(Fun, Fields, undefined).


%% Do a map and removes the values that evaluates to RemoveVal
mapfilter(Fun, List, RemoveVal) ->	
    mapfilter(Fun,List,[],RemoveVal).

mapfilter(_Fun,[],[RemoveVal|Acc],RemoveVal) ->
    Acc;
mapfilter(_Fun,[],Acc,_RemoveVal) ->
    Acc;
			     
mapfilter(Fun, [Elem|Rest], [RemoveVal| Acc], RemoveVal) ->
    mapfilter(Fun,Rest,[Fun(Elem)|Acc],RemoveVal);
mapfilter(Fun,[Elem|Rest],Acc,RemoveVal)->
    mapfilter(Fun,Rest,[Fun(Elem)|Acc],RemoveVal).
 

transform(content_type, undefined, _Disable) ->
    ["Content-Type:text/plain\r\n"];

transform(date, undefined, _Disable) ->
    ["Date:",httpd_util:rfc1123_date(),"\r\n"];

transform(date,RFCDate, _Disable) ->
    ["Date:",RFCDate,"\r\n"];


transform(_Key, undefined, _Disable) ->
    undefined;
transform(accept_ranges, Value, _Disable) ->
    ["Accept-Ranges:",Value,"\r\n"];
transform(cache_control, Value, _Disable) ->
    ["Cache-Control:",Value,"\r\n"];
transform(pragma, Value, _Disable) ->
    ["Pragma:",Value,"\r\n"];
transform(trailer, Value, _Disable) ->
    ["Trailer:",Value,"\r\n"];
transform(transfer_encoding, Value, Disable) ->
    case httpd_util:key1search(Disable,
			       disable_chunked_transfer_encoding_send) of
	true ->
	    "";
	_ ->
	    ["Transfer-encoding:",Value,"\r\n"]
    end;
transform(etag, Value, _Disable) ->
    ["ETag:",Value,"\r\n"];
transform(location, Value, _Disable) ->
    ["Retry-After:",Value,"\r\n"];
transform(server, Value, _Disable) ->
    ["Server:",Value,"\r\n"];
transform(allow, Value, _Disable) ->
    ["Allow:",Value,"\r\n"];
transform(content_encoding, Value, _Disable) ->
    ["Content-Encoding:",Value,"\r\n"];
transform(content_language, Value, _Disable) ->
    ["Content-Language:",Value,"\r\n"];
transform(retry_after, Value, _Disable) ->
    ["Retry-After:",Value,"\r\n"];
transform(server, Value, _Disable) ->
    ["Server:",Value,"\r\n"];
transform(allow, Value, _Disable) ->
    ["Allow:",Value,"\r\n"];
transform(content_encoding, Value, _Disable) ->
    ["Content-Encoding:",Value,"\r\n"];
transform(content_language, Value, _Disable) ->
    ["Content-Language:",Value,"\r\n"];
transform(content_location, Value, _Disable) ->
    ["Content-Location:",Value,"\r\n"];
transform(content_length, Value, _Disable) ->
    ["Content-Length:",Value,"\r\n"];
transform(content_MD5, Value, _Disable) ->
    ["Content-MD5:",Value,"\r\n"];
transform(content_range, Value, _Disable) ->
    ["Content-Range:",Value,"\r\n"];
transform(content_type, Value, _Disable) ->
    ["Content-Type:",Value,"\r\n"];
transform(expires, Value, _Disable) ->
    ["Expires:",Value,"\r\n"];
transform(last_modified, Value, _Disable) ->
    ["Last-Modified:",Value,"\r\n"].



%%----------------------------------------------------------------------
%% This is the old way of sending data it is strongly encouraged to 
%% Leave this method and go on to the newer form of response
%% OTP-4408
%%----------------------------------------------------------------------

send_response_old(#mod{socket_type = Type, 
		       socket      = Sock, 
		       method      = "HEAD"} = Info,
		  StatusCode, Response) ->
    ?vtrace("send_response_old(HEAD) -> entry with"
	    "~n   StatusCode: ~p"
	    "~n   Response:   ~p",
	    [StatusCode,Response]),
    case httpd_util:split(lists:flatten(Response),"\r\n\r\n|\n\n",2) of
	{ok, [Head, Body]} ->
	    Header = 
		httpd_util:header(StatusCode,Info#mod.connection) ++
		"Content-Length:" ++ content_length(Body), 
	    httpd_socket:deliver(Type, Sock, [Header,Head,"\r\n"]);

	_Error ->
	    send_status(Info, 500, "Internal Server Error")
    end;

send_response_old(#mod{socket_type = Type, 
		       socket      = Sock} = Info,
		  StatusCode, Response) ->
    ?vtrace("send_response_old -> entry with"
	    "~n   StatusCode: ~p"
	    "~n   Response:   ~p",
	    [StatusCode,Response]),
    case httpd_util:split(lists:flatten(Response),"\r\n\r\n|\n\n",2) of
	{ok, [_Head, Body]} ->
	    Header = 
		httpd_util:header(StatusCode,Info#mod.connection) ++
		"Content-Length:" ++ content_length(Body), 
	    httpd_socket:deliver(Type, Sock, [Header, Response]);

	{ok, Body} ->
	    Header = 
		httpd_util:header(StatusCode,Info#mod.connection) ++
		"Content-Length:" ++ content_length(Body) ++ "\r\n", 
	    httpd_socket:deliver(Type, Sock, [Header, Response]);

	{error, _Reason} ->
	    send_status(Info, 500, "Internal Server Error")
    end.

content_length(Body)->
    integer_to_list(httpd_util:flatlength(Body))++"\r\n".


report_error(Mod, ConfigDB, Error) ->
    Modules = httpd_util:lookup(ConfigDB, modules,
				[mod_get, mod_head, mod_log]),
    case lists:member(Mod, Modules) of
	true ->
	    Mod:report_error(ConfigDB, Error);
	_ ->
	    ok
    end.
