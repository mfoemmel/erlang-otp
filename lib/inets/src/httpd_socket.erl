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
-module(httpd_socket).

%% API  (document close ?)
-export([deliver/3,  peername/2, resolve/0,  close/2]).

-include("httpd.hrl").

-define(VMODULE,"SOCKET").
-include("httpd_verbosity.hrl").
-include_lib("kernel/include/inet.hrl").

deliver(SocketType, Socket, IOListOrBinary)  ->
    case http_transport:send(SocketType, Socket, IOListOrBinary) of
	{error, _Reason} ->
	    ?vlog("deliver(~p) failed for reason:"
		  "~n   Reason: ~p",[SocketType,_Reason]),
	    (catch close(SocketType, Socket)), 
	    socket_closed;
	_ ->
	    ok
    end.

peername(SocketType, Socket) ->
    http_transport:peername(SocketType, Socket).

resolve() ->
   http_transport:resolve().

close(SocketType, Socket) ->
    Res = 
	case (catch http_transport:close(SocketType, Socket)) of
	    ok ->                  ok;
	    {error,Reason} ->      {error,Reason};
	    {'EXIT',{noproc,_}} -> {error,closed};
	    {'EXIT',Reason} ->     {error,Reason};
	    Otherwise ->           {error,Otherwise}
	end,
    ?vtrace("close(~p) result: ~p",[SocketType, Res]),
    Res.

-ifdef(inets_debug).
data_size(L) when list(L) -> 
    httpd_util:flatlength(L);
data_size(B) when binary(B) ->
    size(B);
data_size(O) ->
    {unknown_size,O}.
-endif.
