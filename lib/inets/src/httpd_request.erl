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
	{socket_closed,Reason} ->
	    ?vlog("Socket closed while reading request header: "
		  "~n   ~p", [Reason]),
	    socket_close;
	{ok,Header} ->
	    MaxBodySz     = httpd_util:lookup(ConfigDB,max_body_size,nolimit),
	    ContentLength = content_length(Header),
	    ?vtrace("ContentLength: ~p", [ContentLength]),
	    case read_entity_body(SocketType,Socket,Timeout,
				  MaxBodySz,ContentLength) of
		{socket_closed,Reason} ->
		    ?vlog("Socket closed while reading request body: "
			  "~n   ~p", [Reason]),
		    socket_close;
		{ok,EntityBody} ->
		    Request = lists:append(Header,EntityBody),
		    httpd_response:send(SocketType,Socket,Request,
					ConfigDB,InitData)
	    end
    end.

%% read_header

read_header(_,_,_,MaxHdrSz,Bs) when MaxHdrSz < length(Bs) ->
    ?vlog("header to long: "
	  "~n   MaxHdrSz:   ~p"
	  "~n   length(Bs): ~p", [MaxHdrSz,length(Bs)]),
    throw({error,{header_too_long,MaxHdrSz,length(Bs)}});
read_header(SocketType,Socket,Timeout,MaxHdrSz,[$\n, $\r, $\n, $\r | Rest]) ->
    {ok,lists:reverse([$\n, $\r, $\n, $\r | Rest])};
read_header(SocketType,Socket,Timeout,MaxHdrSz,Bs) ->
    %% ?vlog("read_header -> entry with Timeout: ~p",[Timeout]),
    T = t(),
    case (catch httpd_socket:recv(SocketType,Socket,1,Timeout)) of
	{ok,[B]} ->
	    read_header(SocketType,Socket,Timeout-(t()-T),MaxHdrSz,[B|Bs]);
	{error,closed} ->
	    {socket_closed,normal};
	{error,etimedout} ->
	    {socket_closed, timeout};
	{error,Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed,Other}
    end.

read_entity_body(_SocketType,_Socket,_Timeout,_Max,0) ->
    {ok,[]};
read_entity_body(_SocketType,_Socket,_Timeout,Max,Len) when Max < Len ->
    ?vlog("body to long: "
	  "~n   Max: ~p"
	  "~n   Len: ~p", [Max,Len]),
    throw({error,{body_too_long,Max,Len}});
read_entity_body(SocketType,Socket,Timeout,Max,Len) ->
    T = t(),
    case httpd_socket:recv(SocketType,Socket,Len,Timeout-(t()-T)) of
	{ok,Body} ->
	    {ok,Body};
	{error,closed} ->
	    {socket_closed,normal};
	{error,etimedout} ->
	    {socket_closed, timeout};
	{error,Reason} ->
	    {socket_closed, Reason};
	Other ->
	    {socket_closed,Other}
    end.



%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


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

