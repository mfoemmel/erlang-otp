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
    ?DEBUG("read -> entry with"
	   "~n   Socket: ~p", [Socket]),
    MaxHdrSz = httpd_util:lookup(ConfigDB,max_header_size,256),
    ?DEBUG("read -> MaxHdrSz: ~p", [MaxHdrSz]),
    case read_header(Socket, MaxHdrSz, Data, Timeout) of
	{socket_closed,Reason} ->
	    ?DEBUG("read -> Socket closed while reading header: ~p", 
		   [Reason]),
	    socket_close;
	{Header,EntityBodyPart} ->
	    MaxBodySz = httpd_util:lookup(ConfigDB,max_body_size,2048),
	    ContentLength = content_length(Header) - length(EntityBodyPart),
	    ?DEBUG("read -> ContentLength: ~p", [ContentLength]),
	    case read_entity_body(Socket,MaxBodySz,
				  EntityBodyPart,ContentLength) of
		{socket_closed,Reason} ->
		    ?DEBUG("read -> "
			   "Socket closed while reading entity body: ~p", 
			   [Reason]),
		    socket_close;
		EntityBody ->
		    ?DEBUG("read -> Entity body received ok", []),
		    Request=lists:append(Header,EntityBody),
		    httpd_response:send(SocketType,Socket,Request,
					ConfigDB,InitData)
	    end
    end.

%% read_header

read_header(Socket, MaxHdrSz, SoFar, Timeout) ->
    ?DEBUG("read_header() -> entry when size(SoFar): ~p",[sz(SoFar)]),
    case terminated_header(MaxHdrSz,SoFar) of
	{true,Header,EntityBodyPart} ->
	    ?DEBUG("read_header -> done when"
		   "~n   Header:         ~p"
		   "~n   EntityBodyPart: ~p", [Header,EntityBodyPart]),
	    {Header,EntityBodyPart};
	false ->
	    ?DEBUG("read_header -> await message...",[]),
	    receive
		{tcp,Socket,Data} ->
		    ?DEBUG("read_header(tcp) -> got some data: ~p",[sz(Data)]),
		    read_header(Socket, MaxHdrSz, [SoFar,Data], Timeout);
		{tcp_closed,Socket} ->
		    ?DEBUG("read_header(tcp) -> socket closed",[]),
		    {socket_closed,normal};
		{tcp_error,Socket,Reason} ->
		    {socket_closed,Reason};
		{ssl,Socket,Data} ->
		    ?DEBUG("read_header(ssl) -> got some data: ~p",[sz(Data)]),
		    read_header(Socket, MaxHdrSz, [SoFar,Data], Timeout);
		{ssl_closed,Socket} ->
		    ?DEBUG("read_header(ssl) -> socket closed",[]),
		    {socket_closed,normal};
		{ssl_error,Socket,Reason} ->
		    {socket_closed,Reason}
	    after Timeout ->
		    {socket_closed,timeout}
	    end
    end.

hsplit(_MaxHdrSz, Accu,[]) ->
    ?DEBUG("hsplit -> not terminated",[]),
    not_terminated;
hsplit(_MaxHdrSz, Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    ?DEBUG("hsplit -> new-line when:"
	   "~n   Accu length: ~p"
	   "~n   Tail length: ~p",[sz(Accu),sz(Tail)]),
    [lists:reverse(Accu), Tail];
hsplit(nolimit, Accu, [H|T]) ->
    hsplit(nolimit,[H|Accu],T);
hsplit(MaxHdrSz, Accu, [H|T]) when length(Accu) < MaxHdrSz ->
    hsplit(MaxHdrSz,[H|Accu],T);
hsplit(MaxHdrSz, Accu, D) ->
    throw({error,{header_too_long,length(Accu),length(D)}}).

terminated_header(MaxHdrSz, Data) ->
    D1 = lists:flatten(Data),
    ?DEBUG("terminated_header -> Data size: ~p",[sz(D1)]),
    case hsplit(MaxHdrSz,[],D1) of
	not_terminated ->
	    ?DEBUG("terminated_header -> not terminated",[]),
	    false;
	[Header,EntityBodyPart] ->
	    ?DEBUG("terminated_header -> done",[]),
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

read_entity_body(Socket,MaxBodySz,EntityBody,ContentLength) 
    when MaxBodySz < ContentLength ->
    throw({error,{body_too_long,MaxBodySz,ContentLength}});
read_entity_body(Socket,MaxBodySz,EntityBody,ContentLength) 
    when ContentLength < 1 ->
    lists:flatten(EntityBody);
read_entity_body(Socket,MaxBodySz,SoFar,ContentLength) ->
    receive
	{tcp,Socket,Data} ->
	    read_entity_body(Socket,MaxBodySz,[SoFar,Data],
			     ContentLength-length(Data));
	{tcp_closed,Socket} ->
	    {socket_closed,normal};
	{tcp_error,Socket,Reason} ->
	    {socket_closed,Reason};
	{ssl,Socket,Data} ->
	    read_entity_body(Socket,MaxBodySz,[SoFar,Data],
			     ContentLength-length(Data));
	{ssl_closed,Socket} ->
	    {socket_closed,normal};
	{ssl_error,Socket,Reason} ->
	    {socket_closed,Reason}
    end.


sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


