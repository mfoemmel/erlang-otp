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
%% Description: Implements chunked transfer encoding see RFC2616 section
%% 3.6.1
-module(http_chunk).

-include("http.hrl").

%% API
-export([decode/3, handle_headers/2]).
%% Callback API - used for example if the chunkedbody is received a
%% little at a time on a socket. 
-export([decode_size/1, ignore_extensions/1, decode_data/1, decode_trailer/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================
%%-------------------------------------------------------------------------
%% decode(ChunkedBody, MaxBodySize, MaxHeaderSize) -> 
%%       {ok, {Headers, Body}} | {Module, Function, Args}
%%
%%      Headers = ["Header:Value"]
%%      ChunkedBody = binary()
%%      MaxBodySize = integer()
%%      MaxHeaderSize = integer()
%%                                    
%% Description: Decodes a body encoded by the chunked transfer
%% encoding. If the ChunkedBody is not compleate it returns {Module,
%% Function, Args} so that decoding can be continued when more of the
%% data has been received by calling Module:Function([NewData | Args]).
%%-------------------------------------------------------------------------
decode(ChunkedBody, MaxBodySize, MaxHeaderSize) ->
    %% Note decode_size will call decode_data.
    decode_size([ChunkedBody, [], {MaxBodySize, <<>>, 0, MaxHeaderSize}]).

%%-------------------------------------------------------------------------
%% decode(HeaderRecord, ChunkedHeaders) -> NewHeaderRecord
%%
%%	HeaderRecord = NewHeaderRecord = #http_request_h{} | #http_response_h{}
%%      ChunkedHeaders = ["Header:Value"] as returnde by http_chunk:decode/3
%%                                    
%% Description: Removes chunked from the header as we now have decode
%% the body and adds a content-length header and any other headers
%% found in the chunked trail.
%%-------------------------------------------------------------------------
handle_headers(RequestHeaderRecord = #http_request_h{}, ChunkedHeaders) ->
    NewHeaders = http_request:headers(ChunkedHeaders, RequestHeaderRecord),
    TransferEncoding = 
	case NewHeaders#http_request_h.transfer_encoding -- "chunked" of
	    ""  ->
		undefined;
	    Other ->
		Other
	end,
    NewHeaders#http_request_h{transfer_encoding = TransferEncoding};

handle_headers(ResponseHeaderRecord = #http_response_h{},  ChunkedHeaders) ->
    NewHeaders = http_response:headers(ChunkedHeaders, ResponseHeaderRecord),
    TransferEncoding = 
	case NewHeaders#http_response_h.transfer_encoding -- "chunked" of
	    ""  ->
		undefined;
	    Other ->
		Other
	end,
    NewHeaders#http_response_h{transfer_encoding = TransferEncoding}.

%% Functions that may be returned during the decoding process
%% if the input data is incompleate. 
decode_size([Bin, HexList, Info]) ->
    decode_size(Bin, HexList, Info).

ignore_extensions([Bin, NextFunction]) ->
    ignore_extensions(Bin, NextFunction).

decode_data([Bin, ChunkSize, TotalChunk, Info]) ->
    decode_data(ChunkSize, <<TotalChunk/binary, Bin/binary>>, Info).

decode_trailer([Bin, Header, Headers, MaxHeaderSize, Body, BodyLength]) ->
    decode_trailer(Bin, Header, Headers, MaxHeaderSize, Body, BodyLength).

%%%========================================================================
%%% Internal functions
%%%========================================================================
decode_size(<<>>, HexList, Info) ->
    {?MODULE, decode_size, [HexList, Info]};
decode_size(<<?CR, ?LF, ChunkRest/binary>>, HexList, {MaxBodySize, Body, 
						      AccLength,
						      MaxHeaderSize}) ->
    ChunkSize =  httpd_util:hexlist_to_integer(lists:reverse(HexList)),
    %% Note decode_data may call decode_size again if there
    %% is more than one chunk, hence here is where the last parameter
    %% to this function comes in.
    decode_data(ChunkSize, ChunkRest, {MaxBodySize, Body, 
				       ChunkSize + AccLength , MaxHeaderSize});
decode_size(<<";", Rest/binary>>, HexList, Info) ->
    %% Note ignore_extensions will call decode_size again when
    %% it ignored all extensions.
    ignore_extensions(Rest, {?MODULE, decode_size, [HexList, Info]});
decode_size(<<Octet, Rest/binary>>, HexList, Info) ->
    decode_size(Rest, [Octet | HexList], Info).

%% "All applications MUST ignore chunk-extension extensions they
%% do not understand.", see RFC 2616 Section 3.6.1 We don't
%% understand any extension...
ignore_extensions(<<>>, NextFunction) ->
    {?MODULE, ignore_extensions, [NextFunction]};
ignore_extensions(<<?CR, ?LF, ChunkRest/binary>>, {Module, Function, Args}) ->
    Module:Function([ChunkRest | Args]);
ignore_extensions(<<_Octet, Rest/binary>>, NextFunction) ->
    ignore_extensions(Rest, NextFunction).

decode_data(ChunkSize, TotalChunk,
	    Info = {MaxBodySize, BodySoFar, AccLength, MaxHeaderSize}) 
  when ChunkSize =< size(TotalChunk) ->
    case TotalChunk of
	%% Last chunk
	<<Data:ChunkSize/binary, ?CR, ?LF, "0", Rest/binary>> ->
	    %% Note ignore_extensions will call decode_trailer
	    %% once it ignored all extensions.
	    ignore_extensions(Rest, {?MODULE, decode_trailer, 
				     [[],[], MaxHeaderSize,
				      <<BodySoFar/binary, Data/binary>>,
				     integer_to_list(AccLength + ChunkSize)]});
	%% There are more chunks, so here we go agin...
	<<Data:ChunkSize/binary, ?CR, ?LF, Rest/binary>> 
	when (AccLength < MaxBodySize) or (MaxBodySize == nolimit)  ->
	    decode_size(Rest, [], 
			{MaxBodySize, <<BodySoFar/binary, Data/binary>>,
			 AccLength, MaxHeaderSize});
	<<_Data:ChunkSize/binary, ?CR, ?LF, _Rest/binary>> ->
	    throw({error, body_too_big});
	_ ->
	    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}
    end;	
decode_data(ChunkSize, TotalChunk, Info) ->
    {?MODULE, decode_data, [ChunkSize, TotalChunk, Info]}.

decode_trailer(<<>>, Header, Headers, MaxHeaderSize, Body, BodyLength) ->
    {?MODULE, decode_trailer, [Header, Headers, MaxHeaderSize, Body, 
			       BodyLength]};
decode_trailer(<<?CR,?LF>>, [], [], _, Body, BodyLength) ->
    {ok, {["content-length:" ++ BodyLength], Body}};
decode_trailer(<<?CR,?LF>>, Header, Headers, MaxHeaderSize, Body, 
	       BodyLength) ->
    NewHeaders = [lists:reverse(Header) | Headers],
    Length =  length(NewHeaders), 
    case Length > MaxHeaderSize of
	true ->
	    throw({error, {header_too_long, MaxHeaderSize, 
			   MaxHeaderSize-Length}});
	false ->
	    {ok, {["content-length:" ++ BodyLength, NewHeaders], Body}}
    end;
decode_trailer(<<?CR, ?LF, Rest/binary>>, Header, Headers, 
	       MaxHeaderSize, Body, BodyLength) ->
    decode_trailer(Rest, [], [lists:reverse(Header) | Headers], 
		   MaxHeaderSize, Body, BodyLength);
decode_trailer(<<Octet, Rest/binary>>, Header, Headers, MaxHeaderSize, Body,
	       BodyLength) ->
    decode_trailer(Rest, [Octet | Header], Headers, MaxHeaderSize, 
		   Body, BodyLength).





