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
-module(httpd_request).
-include("httpd.hrl").
-export([read/5]).

%% read

read(SocketType,Socket,ConfigDB,InitData, Timeout) ->
  read(SocketType,Socket,[],ConfigDB,InitData, Timeout).

read(SocketType,Socket,Data,ConfigDB,InitData, Timeout) ->
    ?DEBUG("read -> socket ~p", [Socket]),
    case read_header(Socket, Data, Timeout) of
	{Header,EntityBodyPart} ->
	    ContentLength=content_length(Header)-length(EntityBodyPart),
	    ?DEBUG("read -> ContentLength: ~p", [ContentLength]),
	    case read_entity_body(Socket,EntityBodyPart,ContentLength) of
		socket_closed ->
		    ?DEBUG("read -> Socket closed while reading entity_body", []),
		    socket_close;
		EntityBody ->
		    ?DEBUG("httpd_request:read -> Entity body received ok", []),
		    Request=lists:append(Header,EntityBody),
		    httpd_response:send(SocketType,Socket,Request,ConfigDB,InitData)
	    end;
	socket_closed ->
	    ?DEBUG("read -> Socket closed while reading header", []),
	    socket_close
    end.

%% read_header

read_header(Socket, SoFar, Timeout) ->
    case terminated_header(SoFar) of
	{true,Header,EntityBodyPart} ->
	    {Header,EntityBodyPart};
	false ->
	    receive
		{tcp,Socket,Data} ->
		    read_header(Socket,[SoFar,Data], Timeout);
		{tcp_closed,Socket} ->
		    socket_closed;
		{tcp_error,Socket,Reason} ->
		    socket_closed;
		{ssl,Socket,Data} ->
		    read_header(Socket,[SoFar,Data], Timeout);
		{ssl_closed,Socket} ->
		    socket_closed;
		{ssl_error,Socket,Reason} ->
		    socket_closed
	    after Timeout ->
		    socket_closed
	    end
    end.

hsplit(Accu,[]) ->
    not_terminated;
hsplit(Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    [lists:reverse(Accu), Tail];
hsplit(Accu, [H|T]) ->
    hsplit([H|Accu],T).

terminated_header(Data) ->
    case hsplit([],lists:flatten(Data)) of
	not_terminated ->
	    false;
	[Header,EntityBodyPart] ->
	    {true, Header++"\r\n\r\n",EntityBodyPart}
    end.

%% content_length

content_length([$C,$o,$n,$t,$e,$n,$t,$-,$l,$e,$n,$g,$t,$h,$:,$ | Length]) ->
    list_to_integer(cut(Length));
content_length([$C,$o,$n,$t,$e,$n,$t,$-,$L,$e,$n,$g,$t,$h,$:,$ | Length]) ->
    list_to_integer(cut(Length));
content_length([_|Rest]) ->
    content_length(Rest);
content_length([]) ->
    0.

cut([$\r,$\n|_]) ->
  [];
cut([N|Rest]) ->
  [N|cut(Rest)].

%% read_entity_body

read_entity_body(Socket,EntityBody,ContentLength) when ContentLength < 1 ->
    lists:flatten(EntityBody);
read_entity_body(Socket,SoFar,ContentLength) ->
    receive
	{tcp,Socket,Data} ->
	    read_entity_body(Socket,[SoFar,Data],ContentLength-length(Data));
	{tcp_closed,Socket} ->
	    socket_closed;
	{tcp_error,Socket,Reason} ->
	    socket_closed;
	{ssl,Socket,Data} ->
	    read_entity_body(Socket,[SoFar,Data],ContentLength-length(Data));
	{ssl_closed,Socket} ->
	    socket_closed;
	{ssl_error,Socket,Reason} ->
	    socket_closed
    end.
