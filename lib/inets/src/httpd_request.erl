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

-export([read/5]).

-include("httpd.hrl").

-define(VMODULE,"REQUEST").
-include("httpd_verbosity.hrl").


%% read

read(SocketType, Socket, ConfigDB, InitData, Timeout) ->
  read(SocketType, Socket, [], ConfigDB, InitData, Timeout).

read(SocketType, Socket, Data, ConfigDB, InitData, Timeout) ->
    ?vdebug("read from socket ~p with Timeout ~p",[Socket,Timeout]),
    MaxHdrSz = httpd_util:lookup(ConfigDB,max_header_size,10240),
    case read_header(SocketType, Socket, Timeout, MaxHdrSz, []) of
	{socket_closed, Reason} ->
	    ?vlog("Socket closed while reading request header: "
		  "~n   ~p", [Reason]),
	    socket_close;
	{ok, Header, EntityBodyPart} ->
	    MaxBodySz     = httpd_util:lookup(ConfigDB,max_body_size,nolimit),
	    ContentLength = content_length(Header) - length(EntityBodyPart),
	    ?vtrace("ContentLength: ~p", [ContentLength]),
	    case read_entity_body(SocketType, Socket, Timeout,
				  MaxBodySz, ContentLength, EntityBodyPart) of
		{socket_closed, Reason} ->
		    ?vlog("Socket closed while reading request body: "
			  "~n   ~p", [Reason]),
		    socket_close;
		{ok, EntityBody} ->
		    Request = lists:append(Header, EntityBody),
		    httpd_response:send(SocketType, Socket, Request,
					ConfigDB, InitData)
	    end
    end.

%% read_header

%% This algorithm rely on the buffer size of the inet driver together
%% with the {active, once} socket option. Atmost one message of this 
%% size will be received at a give time. When a full header has been 
%% read, the body is read with the recv function (the body size is known). 
%%
read_header(_SocketType, _Socket, Timeout, _MaxHdrSz, _SoFar) 
  when Timeout < 0 ->
    {socket_closed, timeout};
    
read_header(SocketType, Socket, Timeout, MaxHdrSz, SoFar) ->
    T = t(),
    case terminated_header(MaxHdrSz, SoFar) of
	{true, Header, EntityBodyPart} ->
	    ?vdebug("read_header(~p) -> done reading header: "
		    "~n   length(Header):         ~p"
		    "~n   length(EntityBodyPart): ~p", 
		    [SocketType, length(Header), length(EntityBodyPart)]),
	    {ok, Header, EntityBodyPart};
	false ->
	    ?vtrace("read_header(~p) -> "
		    "~n   set active = 'once' and "
		    "await a chunk of the header", [SocketType]),
	    httpd_socket:active_once(SocketType, Socket),
	    receive
		%% 
		%% TCP
		%% 
		{tcp, Socket, Data} ->
		    ?vtrace("read_header(ip) -> got some data: ~p bytes", 
			    [sz(Data)]),
		    read_header(SocketType, Socket, 
				Timeout-(t()-T), MaxHdrSz, [SoFar,Data]);
		{tcp_closed, Socket} ->
		    ?vtrace("read_header(ip) -> socket closed",[]),
		    {socket_closed, normal};
		{tcp_error, Socket, Reason} ->
		    ?vtrace("read_header(ip) -> socket error: ~p", [Reason]),
		    {socket_closed, Reason};

		%% 
		%% SSL
		%% 
		{ssl, Socket, Data} ->
		    ?vtrace("read_header(ssl) -> got some data: ~p bytes", 
			    [sz(Data)]),
		    read_header(SocketType, Socket, 
				Timeout-(t()-T), MaxHdrSz, [SoFar,Data]);
		{ssl_closed, Socket} ->
		    ?vtrace("read_header(ssl) -> socket closed",[]),
		    {socket_closed, normal};
		{ssl_error, Socket, Reason} ->
		    ?vtrace("read_header(ssl) -> socket error: ~p", [Reason]),
		    {socket_closed, Reason}

	    after Timeout ->
		    ?vlog("read_header(~p) -> timeout",[SocketType]),
		    {socket_closed, timeout}
	    end
    end.

    
terminated_header(MaxHdrSz, Data) ->
    D1 = lists:flatten(Data),
    ?vtrace("terminated_header -> Data size: ~p",[sz(D1)]),
    case hsplit(MaxHdrSz,[],D1) of
	not_terminated ->
	    false;
	[Header, EntityBodyPart] ->
	    {true, Header++"\r\n\r\n",EntityBodyPart}
    end.


hsplit(_MaxHdrSz, Accu,[]) ->
    not_terminated;
hsplit(_MaxHdrSz, Accu, [ $\r, $\n, $\r, $\n | Tail]) ->
    [lists:reverse(Accu), Tail];
hsplit(nolimit, Accu, [H|T]) ->
    hsplit(nolimit,[H|Accu],T);
hsplit(MaxHdrSz, Accu, [H|T]) when length(Accu) < MaxHdrSz ->
    hsplit(MaxHdrSz,[H|Accu],T);
hsplit(MaxHdrSz, Accu, D) ->
    throw({error,{header_too_long,length(Accu),length(D)}}).


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

read_entity_body(_SocketType, _Socket, _Timeout, _Max, 0, BodyPart) ->
    {ok, BodyPart};

read_entity_body(_SocketType, _Socket, _Timeout, _Max, Len, BodyPart) 
  when Len < 0 ->
    %% Ouch, this means we got the entire body when reading the header
    %% and _then_ some garbage bytes. So, the ContentLength is less then 
    %% the length of the BodyPart => Len < 0
    ?vlog("negative body part length:"
	  "~n   Len: ~p"
	  "~n   BodyPart: ~p", [Len, BodyPart]),
    {ok, strip_trailing(Len, BodyPart)};

read_entity_body(_SocketType, _Socket, _Timeout, Max, Len, BodyPart) 
  when Max < Len ->
    ?vlog("body to long: "
	  "~n   Max: ~p"
	  "~n   Len: ~p", [Max,Len]),
    throw({error,{body_too_long,Max,Len}});

read_entity_body(_SocketType, _Socket, Timeout, _Max, _Len, _BodyPart) 
  when Timeout < 0 ->
    {socket_closed, timeout};

read_entity_body(SocketType, Socket, Timeout, Max, Len, BodyPart) ->
    L = Len-length(BodyPart),
    case httpd_socket:recv(SocketType, Socket, Len, Timeout) of
	{ok, Body} ->
	    {ok, lists:flatten([BodyPart, Body])};
	{error, closed} ->
	    {socket_closed, normal};
	{error, etimedout} ->
	    ?vinfo("read_entity_body -> timeout", []),
	    {socket_closed, timeout};
	{error, Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed, Other}
    end.


%% Note: N < 0; abs(N) >= length(Body)
strip_trailing(N, Body) ->
    lists:sublist(Body, length(Body) + N).


%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.

