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

%% op codes commands are in capital and reply codes in lower case 

-define(CONNECT,	1).
-define(CONNECT_WAIT,	2).
-define(CONNECT_REP,	3).
-define(CONNECT_ERR,	4).

-define(TERMINATE,	5).
-define(CLOSE,		6).

-define(LISTEN,		7).
-define(LISTEN_REP,	8).
-define(LISTEN_ERR,	9).

-define(ACCEPT,		10).
-define(NOACCEPT,	11).
-define(ACCEPT_REP,  	12).
-define(ACCEPT_ERR,  	13).

-define(FROMNET_CLOSE,	14).

-define(CONNECT_SYNC_ERR, 15).
-define(LISTEN_SYNC_ERR, 16).

-define(PROXY_PORT,	23).
-define(PROXY_JOIN,	24).
-define(PROXY_JOIN_REP,	25).
-define(PROXY_JOIN_ERR,	26).

-define(SET_SOCK_OPT,	27).
-define(IOCTL_OK,	28).
-define(IOCTL_ERR,	29).

-define(GETPEERNAME,	30).
-define(GETPEERNAME_REP, 31).
-define(GETPEERNAME_ERR, 32).

-define(GETSOCKNAME,	33).
-define(GETSOCKNAME_REP, 34).
-define(GETSOCKNAME_ERR, 35).

%% Set socket options codes  'SET_SOCK_OPT' 
-define(SET_TCP_NODELAY, 1).

-define(DEF_BACKLOG, 5).

-define(DEF_TIMEOUT, 10000).

-record(sslsocket, { fd = nil, pid = nil}).
