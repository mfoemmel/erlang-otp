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
-module(httpd_parse).
-export([request/1, hsplit/2]).


find_content_type([]) ->
    false;
find_content_type([{Name,Value}|Tail]) ->
    case httpd_util:to_lower(Name) of
	"content-type" ->
	    {ok, Value};
	_ ->
	    find_content_type(Tail)
    end.

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

hsplit(Accu,[]) ->
    {lists:reverse(Accu), []};
hsplit(Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    {lists:reverse(Accu), Tail}; 
hsplit(Accu, [H|T]) ->
    hsplit([H|Accu],T).

%% request
%%
%% Input: The request as sent from the client (list of characters) (may include part
%%        of the entity body)
%%
%% Returns:
%%   {ok, [Method, RequestURI, HTTPVersion, RequestLine, ParsedHeader, EntityBody]}
%%   {not_implemented, RequestLine, Method, RequestURI, HTTPVersion}
%%   {bad_request, Reason}
%%
%% Where:
%% Method      - string()
%% RequestURI  - string()
%% HTTPVersion - "HTTP/X.X"
%% RequestLine - string()
%% EntityBody  - string()
%% Reason      - string()
%%		    
request(Request) ->
    {BeforeEntityBody, Rest} = hsplit([], Request),
    [RequestLine|Header] = split_lines(BeforeEntityBody),
    ParsedHeader = tagup_header(Header),
    EntityBody = maybe_remove_nl(ParsedHeader,Rest),
    case verify_request(string:tokens(RequestLine," ")) of
	["HEAD", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["HEAD", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader, EntityBody]};
	["GET", RequestURI, "HTTP/0.9"] ->
	    {ok, ["GET", RequestURI, "HTTP/0.9", RequestLine, ParsedHeader,
		 EntityBody]};
	["GET", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["GET", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader,EntityBody]};
	["POST", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N]] ->
	    {ok, ["POST", RequestURI, [$H,$T,$T,$P,$/,$1,$.,N], RequestLine,
		 ParsedHeader, EntityBody]};
	[Method, RequestURI] ->
	    {not_implemented, RequestLine, Method, RequestURI, "HTTP/0.9"};
	[Method, RequestURI, HTTPVersion] ->
	    {not_implemented, RequestLine, Method, RequestURI, HTTPVersion};	
	{bad_request, Reason} ->
	    {bad_request, Reason};
	Reason ->
	    {bad_request, "Unknown request method"}
    end.	


split_lines(Request) ->
    split_lines(Request, [], []).
split_lines([], CAcc, Acc) ->
    lists:reverse([lists:reverse(CAcc)|Acc]);
split_lines([$\r, $\n|Rest], CAcc, Acc) ->
    split_lines(Rest, [], [lists:reverse(CAcc)|Acc]);
split_lines([Chr|Rest], CAcc, Acc) ->
    split_lines(Rest, [Chr|CAcc], Acc).


%%
%% This is a 'hack' to stop people from trying to access directories/files
%% relative to the ServerRoot.
%%

verify_request([Request, RequestURI]) ->
    verify_request([Request, RequestURI, "HTTP/0.9"]);
verify_request([Request, RequestURI, Protocol]) ->
    NewRequestURI = 
	case string:str(RequestURI, "?") of
	    0 ->
		RequestURI;
	    Ndx ->
		string:left(RequestURI, Ndx)
	end,
    case string:str(NewRequestURI, "..") of
	0 ->
	    [Request, RequestURI, Protocol];
	_ ->
	    {bad_request, {forbidden, RequestURI}}
    end;
verify_request(Request) ->
    Request.
		      

%% tagup_header
%%
%% Parses the header of a HTTP request and returns a two-tuple
%% list containing Name and Value of each header directive as of:
%%
%% Content-Type: multipart/mixed -> {"Content-Type", "multipart/mixed"}
%%
tagup_header([]) ->          [];
tagup_header([Line|Rest]) -> [tag(Line, [])|tagup_header(Rest)].

tag([], Tag) ->
    {lists:reverse(Tag), ""};
tag([$:|Rest], Tag) ->
    {lists:reverse(Tag), get_value(Rest)};
tag([Chr|Rest], Tag) ->
    tag(Rest, [Chr|Tag]).


get_value([]) ->        [];
get_value([32|Line]) -> get_value2(Line);
get_value([C|Line]) ->  get_value(Line).

get_value2([]) ->     [];
get_value2([L|Ls]) -> [L|get_value2(Ls)].
