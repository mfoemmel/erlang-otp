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

-module(http_request).

-include("http.hrl").
-include("httpd.hrl").

%%% Client API
-export([send/2]).

%%% Server API
-export([parse/1, whole_body/2, validate/3, headers/2, mod_data/10]).

%% Callback API - used for example if the header/body is received a
%% little at a time on a socket. 
-export([parse_method/1, parse_uri/1, parse_version/1, parse_headers/1,
	 whole_body/1]).

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
%%% Client side
%%-------------------------------------------------------------------------
%% send(Request) ->
%%	Request - #request{}
%%	Socket - socket()
%%                                   
%% Description: Composes and sends a HTTP-request.
%%-------------------------------------------------------------------------
send(#request{method = Method, scheme = Scheme, address = {Host,Port},
	      path = Path, pquery = Query, headers = Headers,
	      content = Content,
	      settings = Settings},
     Socket) -> 

    PostData = post_data(Method, Headers, Content),
	
    Message = method(Method) ++ " " ++ Path ++ Query ++ " HTTP/1.1" ++ ?CRLF ++
	headers(Headers) ++
	http_cookie:header(Host, Port, Path, Query, 
			   Settings#http_options.cookie) ++
	PostData,
    
    http_transport:send(Scheme, Socket, Message).

%% Sever side

parse([Bin, MaxHeaderSize]) ->
    parse_method(Bin, [], MaxHeaderSize, []).

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
parse_method([Bin, Method, MaxHeaderSize, Result]) ->
    parse_method(Bin, Method, MaxHeaderSize, Result).

parse_uri([Bin, URI, MaxHeaderSize, Result]) ->
    parse_uri(Bin, URI, MaxHeaderSize, Result).

parse_version([Bin, Version, MaxHeaderSize, Result]) ->
    parse_version(Bin, Version, MaxHeaderSize, Result).

parse_headers([Bin, Header, Headers, MaxHeaderSize, Result]) ->
    parse_headers(Bin, Header, Headers, MaxHeaderSize, Result).

whole_body([Bin, Body, Length])  ->
    whole_body(<<Body/binary, Bin/binary>>, Length).
    
%%-------------------------------------------------------------------------
%% validate(Method, Uri, Version) -> ok | {error, {bad_request, Reason} |
%%			     {error, {not_implemented, {Method, Uri, Version}}
%%      Method = "HEAD" | "GET" | "POST" | "TRACE"
%%      Uri = uri()
%%      Version = "HTTP/N.M"      
%% Description: Checks that HTTP-request-line is valid.
%%------------------------------------------------------------------------- 
validate("HEAD", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("GET", Uri, "HTTP/0.9") ->
    validate_uri(Uri);
validate("GET", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("POST", Uri, "HTTP/1." ++ _N) ->
    validate_uri(Uri);
validate("TRACE", Uri, "HTTP/1." ++ N) when hd(N) >= $1 ->
    validate_uri(Uri);
validate(Method, Uri, Version = "HTTP/1." ++ _N) ->
    {error, {not_implemented, {Method, Uri, Version}}}.

%%-------------------------------------------------------------------------
%% headers(HeaderList, #http_request_h{}) -> #http_request_h{}
%%   HeaderList - ["HeaderField:Value"]     	
%%   HeaderField - string()
%%   Value - string()	
%%                                   
%% Description: Creates a http_request_h-record used internally to
%%              handle http-headers.
%%-------------------------------------------------------------------------
headers([], Headers) ->
    Headers;
headers([Header | Tail], Headers) ->  
    case lists:splitwith(fun($:) -> false; (_) -> true end, Header) of
	{Key, [$: | Value]}  ->
	    headers(Tail, headers(httpd_util:to_lower(string:strip(Key)), 
				  string:strip(Value), Headers));
	{_, []} -> 
	    error_logger:error_report("Ignored invalid HTTP-header: ~p~n", 
				      [Header]),
	    headers(Tail, Headers)
    end.

%%----------------------------------------------------------------------
%% The request is passed through the server as a record of type mod 
%% create it.
%% ----------------------------------------------------------------------
mod_data(Socket, SocketType, ConfigDB, Method, RequestURI,
	 HTTPVersion, RequestLine, Headers, EntityBody,InitData)-> 
    ParsedHeaders =  tagup_header(Headers),
    PersistentConn = get_persistens(HTTPVersion, ParsedHeaders, ConfigDB),
    {ok, #mod{init_data = InitData,
	      data = [],
	      socket_type = SocketType,
	      socket = Socket,
	      config_db = ConfigDB,
	      method = Method,
	      absolute_uri = formatAbsoluteURI(RequestURI, ParsedHeaders),
	      request_uri = formatRequestUri(RequestURI),
	      http_version = HTTPVersion,
	      request_line = RequestLine,
	      parsed_header = ParsedHeaders,
	      entity_body = maybe_remove_nl(ParsedHeaders, EntityBody),
	      connection = PersistentConn}}.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%%% Client side
post_data(Method, Headers, {ContentType, Body}) 
  when Method == post; Method == put ->
    
    content_type_header(ContentType) ++
	content_length_header(length(Body)) ++ 
	?CRLF ++
	case Headers#http_request_h.expect of
	    "100-continue" ->
		"";
	    _ ->
		Body
	end;

post_data(_, _, _) ->
    ?CRLF.


%% Transform from #http_request_h record to HTTP syntax 
%% Note: - Headers have been validated in http.erl, thus we know
%% that the Host and TE field exists.

headers(#http_request_h{expect = Expect,
			host = Host,
			te = TE,
			other = Other}) ->
    HostH = ["Host: ", Host,  ?CRLF],
    ExpectH = case Expect of
		  undefined ->[];
		  _ -> ["Expect: ",Expect, ?CRLF]
	      end,
    TEH = ["TE: ",TE, ?CRLF],
    [HostH, ExpectH, TEH | headers_other(Other)].

headers_other([]) ->
    [];
headers_other([{Key,Value} | Rest]) when atom(Key) ->
    Head = atom_to_list(Key) ++ ": " ++ Value ++ ?CRLF,
    Head ++ headers_other(Rest);

headers_other([{Key,Value} | Rest]) ->
    Head = Key ++ ": " ++ Value ++ ?CRLF,
    Head ++ headers_other(Rest).

content_type_header(ContentType) ->
    "Content-Type: " ++ ContentType ++  ?CRLF.
content_length_header(ContentLength) ->
    "Content-Length: "++integer_to_list(ContentLength) ++ ?CRLF.
    
method(Method) ->
    httpd_util:to_upper(atom_to_list(Method)).

%%% Server side
parse_method(<<>>, Method, MaxHeaderSize, Result) ->
    {?MODULE, parse_method, [Method, MaxHeaderSize, Result]};
parse_method(<<?SP, Rest/binary>>, Method, MaxHeaderSize, Result) ->
    parse_uri(Rest, [], MaxHeaderSize,
	      [string:strip(lists:reverse(Method)) | Result]);
parse_method(<<Octet, Rest/binary>>, Method, MaxHeaderSize, Result) ->
    parse_method(Rest, [Octet | Method], MaxHeaderSize, Result).

parse_uri(<<>>, URI, MaxHeaderSize, Result) ->
    {?MODULE, parse_uri, [URI, MaxHeaderSize, Result]};
parse_uri(<<?SP, Rest/binary>>, URI, MaxHeaderSize, Result) -> 
    parse_version(Rest, [], MaxHeaderSize, 
		  [string:strip(lists:reverse(URI)) | Result]);
parse_uri(<<Octet, Rest/binary>>, URI, MaxHeaderSize, Result) ->
    parse_uri(Rest, [Octet | URI], MaxHeaderSize, Result).

parse_version(<<>>, Version, MaxHeaderSize, Result) ->
    {?MODULE, parse_version, [Version, MaxHeaderSize, Result]};
parse_version(<<?CR, ?LF, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_headers(Rest, [], [], MaxHeaderSize, 
		  [string:strip(lists:reverse(Version)) | Result]);
parse_version(<<Octet, Rest/binary>>, Version, MaxHeaderSize, Result) ->
    parse_version(Rest, [Octet | Version], MaxHeaderSize, Result).

parse_headers(<<>>, Header, Headers, MaxHeaderSize, Result) ->
    {?MODULE, parse_headers, [Header, Headers, MaxHeaderSize, Result]};
parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, [], [], _, Result) ->
     NewResult = list_to_tuple(lists:reverse([Body, {#http_request_h{}, []} |
					      Result])),
    {ok, NewResult};
parse_headers(<<?CR,?LF,?CR,?LF,Body/binary>>, Header, Headers,
	      MaxHeaderSize, Result) ->
    HTTPHeaders = [lists:reverse(Header) | Headers],
    Length =   lists:foldl(fun(H, Acc) -> length(H) + Acc end,
			   0, HTTPHeaders),
    case ((Length > MaxHeaderSize) or (MaxHeaderSize == nolimit)) of
	true ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}});
	false ->
	    RequestHeaderRcord = headers(HTTPHeaders, #http_request_h{}),
	    NewResult = 
		list_to_tuple(lists:reverse([Body, {RequestHeaderRcord, 
						    HTTPHeaders} | Result])),
	    {ok, NewResult}
    end;
parse_headers(<<?CR,?LF>>, [], [], _, Result) ->
     NewResult = list_to_tuple(lists:reverse([<<>>, {#http_request_h{}, []} |
					      Result])),
    {ok, NewResult};
parse_headers(<<?CR,?LF, Octet, Rest/binary>>, Header, Headers, 
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet], [lists:reverse(Header) | Headers], 
		  MaxHeaderSize, Result);
parse_headers(<<Octet, Rest/binary>>, Header, Headers, 
	      MaxHeaderSize, Result) ->
    parse_headers(Rest, [Octet | Header], Headers, MaxHeaderSize, Result).


whole_body(Body, Length) ->
    case size(Body) of
	N when N < Length ->
	  {?MODULE, whole_body, [Body, Length]};
	0 ->
	    {ok, Body};
	_ ->
	    %% PotentialGarbage will normaly be <<>> but some badly
	    %% implemented http clients may have some trailing garbage.
	    %% See also: OTP-4550
	    <<NewBody:Length/binary, _PotentialGarbage/binary>> = Body,
	    {ok, NewBody}
    end.

%%% --- Request headers
headers("accept", Value, Headers) ->
    Headers#http_request_h{accept = Value};
headers("accept-charset", Value, Headers) ->
    Headers#http_request_h{accept_charset = Value};
headers("accept-encoding", Value, Headers) ->
    Headers#http_request_h{accept_encoding = Value};
headers("accept-language", Value, Headers) ->
    Headers#http_request_h{accept_language = Value};
headers("authorization", Value, Headers) ->
    Headers#http_request_h{authorization = Value};
headers("expect", Value, Headers) ->
    Headers#http_request_h{expect = Value};
headers("from", Value, Headers) ->
    Headers#http_request_h{from = Value};
headers("host", Value, Headers) ->
    Headers#http_request_h{host = Value};
headers("if-match", Value, Headers) ->
    Headers#http_request_h{if_match = Value};
headers("if-modified-since", Value, Headers) ->
    Headers#http_request_h{if_modified_since = Value};
headers("if-none-match", Value, Headers) ->
    Headers#http_request_h{if_none_match = Value};
headers("if-range", Value, Headers) ->
    Headers#http_request_h{if_range = Value};
headers("if-unmodified-since", Value, Headers) ->
    Headers#http_request_h{if_unmodified_since = Value};
headers("max-forwards", Value, Headers) ->
    Headers#http_request_h{max_forwards = Value};
headers("proxy-authorization", Value, Headers) ->
    Headers#http_request_h{proxy_authenticate = Value};
headers("range", Value, Headers) ->
    Headers#http_request_h{range = Value};
headers("referer", Value, Headers) ->
    Headers#http_request_h{referer = Value};
headers("te", Value, Headers) ->
    Headers#http_request_h{te = Value};
headers("user-agent", Value, Headers) ->
    Headers#http_request_h{user_agent = Value};

%% General-Headers
headers("cache-control", Value, Headers) ->
    Headers#http_request_h{cache_control = Value};
headers("connection", Value, Headers) ->
    Headers#http_request_h{connection = Value};
headers("date", Value, Headers) ->
    Headers#http_request_h{date = Value};
headers("pragma", Value, Headers) ->
    Headers#http_request_h{pragma = Value};
headers("trailer", Value, Headers) ->
    Headers#http_request_h{trailer = Value};
headers("transfer-encoding", Value, Headers) ->
    Headers#http_request_h{transfer_encoding = Value};
headers("upgrade", Value, Headers) ->		
    Headers#http_request_h{upgrade = Value};
headers("via", Value, Headers) ->
    Headers#http_request_h{via = Value};
headers("waring", Value, Headers) ->
    Headers#http_request_h{warning = Value};

%% Entity header
headers("allow", Value, Headers) ->
    Headers#http_request_h{allow = Value};
headers("content-encoding", Value, Headers) ->
    Headers#http_request_h{content_encoding = Value};
headers("content-language", Value, Headers) ->
    Headers#http_request_h{content_language = Value};
headers("content-length", Value, Headers) ->
    Headers#http_request_h{content_length = Value};
headers("content-location", Value, Headers) ->
    Headers#http_request_h{content_location = Value};
headers("content-md5", Value, Headers) ->
    Headers#http_request_h{content_md5 = Value};
headers("content-range", Value, Headers) ->
    Headers#http_request_h{content_range = Value};
headers("content-type", Value, Headers) ->
    Headers#http_request_h{content_type = Value};
headers("expires", Value, Headers) ->
    Headers#http_request_h{expires = Value};
headers("last-modified", Value, Headers) ->
    Headers#http_request_h{last_modified = Value};
headers(Key, Value, Headers) ->
    Headers#http_request_h{other=
			    [{Key, Value} | Headers#http_request_h.other]}.

%% Prevent people from trying to access directories/files
%% relative to the ServerRoot.
validate_uri(RequestURI) ->
    NewRequestURI = 
	case string:str(RequestURI, "?") of
	    0 ->
		httpd_util:decode_hex(RequestURI);
	    Ndx ->
		httpd_util:decode_hex(string:left(RequestURI, Ndx))	
	end,
    
    validate_uri(string:tokens(NewRequestURI, "/"), 0, RequestURI).
    
validate_uri([], _, _) ->
    ok;
validate_uri([".." | _], 0, RequestURI) ->
    {bad_request, {forbidden, RequestURI}};
validate_uri([".." | Rest], N, RequestURI) ->
    validate_uri(Rest, N - 1, RequestURI);
validate_uri([_ | Rest], N, RequestURI) ->
    validate_uri(Rest, N + 1, RequestURI).

%%----------------------------------------------------------------------
%% There are 3 possible forms of the reuqest URI 
%%
%%  1. * When the request is not for a special assset. is is instead
%%     to the server itself
%%
%%  2. absoluteURI the whole servername port and asset is in the request
%%
%%  3. The most common form that http/1.0 used abs path that is a path
%%     to the requested asset.
%5----------------------------------------------------------------------
formatRequestUri("*")->
    "*";
formatRequestUri([$h,$t,$t,$p,$:,$\/,$\/|ServerAndPath]) ->
   removeServer(ServerAndPath);

formatRequestUri([$H,$T,$T,$P,$:,$\/,$\/|ServerAndPath]) ->
    removeServer(ServerAndPath);

formatRequestUri(ABSPath) ->
    ABSPath.

removeServer([$\/|Url])->
    case Url of
	[]->
	    "/";
        _->
	    [$\/|Url]
    end;
removeServer([_|Url]) ->
    removeServer(Url).


formatAbsoluteURI("http://"++ Uri, _)->
    "HTTP://" ++ Uri;

formatAbsoluteURI(OrigUri = "HTTP://" ++ _, _)->
    OrigUri;

formatAbsoluteURI(Uri,ParsedHeader)->
    case httpd_util:key1search(ParsedHeader,"host") of
	undefined ->
	    nohost;
	Host ->
	    Host++Uri
    end.

get_persistens(HTTPVersion,ParsedHeader,ConfigDB)->
    case httpd_util:lookup(ConfigDB, persistent_conn, true) of
	true->
	    case HTTPVersion of
		%%If it is version prio to 1.1 kill the conneciton
		"HTTP/1." ++ NList ->
		    case httpd_util:key1search(ParsedHeader,
					       "connection", "keep-alive") of       
			%%if the connection isnt ordered to go down
			%%let it live The keep-alive value is the
			%%older http/1.1 might be older Clients that
			%%use it.
			"keep-alive" when hd(NList) >= 49 ->
			    ?DEBUG("CONNECTION MODE: ~p",[true]),  
			    true;
			"close" ->
			    ?DEBUG("CONNECTION MODE: ~p",[false]),  
			    false;
			_Connect ->
  			    ?DEBUG("CONNECTION MODE: ~p VALUE: ~p",
				   [false, _Connect]),  
			    false
		    end; 
		_ ->
		    ?DEBUG("CONNECTION MODE: ~p VERSION: ~p",
			   [false, HTTPVersion]),  
		    false
	    end;
	_ ->
	    false
    end.

%%----------------------------------------------------------------------
%% Control whether the last newline of the body is a part of the message or
%%it is a part of the multipart message.
%%----------------------------------------------------------------------
maybe_remove_nl(Header,Rest) ->
    case find_content_type(Header) of
	false ->
	    {ok,EntityBody,_}=regexp:sub(Rest,"\r\n\$",""),
	    EntityBody;
	{ok, Value} ->
	    case string:str(Value, "multipart/form-data") of
		0 ->
		    {ok,EntityBody,_}=regexp:sub(Rest,"\r\n\$",""),
		    EntityBody;
		_ ->
		    Rest
	    end
    end.

%%----------------------------------------------------------------------
%% tagup_header
%%
%% Parses the header of a HTTP request and returns a key,value tuple 
%% list containing Name and Value of each header directive as of:
%%
%% Content-Type: multipart/mixed -> {"Content-Type", "multipart/mixed"}
%%
%% But in http/1.1 the field-names are case insencitive so now it must be 
%% Content-Type: multipart/mixed -> {"content-type", "multipart/mixed"}
%% The standard furthermore says that leading and traling white space 
%% is not a part of the fieldvalue and shall therefore be removed.
%%----------------------------------------------------------------------
tagup_header([]) ->          [];
tagup_header([Line|Rest]) -> [tag(Line, [])|tagup_header(Rest)].

tag([], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), ""};
tag([$:|Rest], Tag) ->
    {httpd_util:to_lower(lists:reverse(Tag)), string:strip(Rest)};
tag([Chr|Rest], Tag) ->
    tag(Rest, [Chr|Tag]).

find_content_type([]) ->
    false;
find_content_type([{Name,Value}|Tail]) ->
    case httpd_util:to_lower(Name) of
	"content-type" ->
	    {ok, Value};
	_ ->
	    find_content_type(Tail)
    end.
