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

%% Definition of macros and constants for socksv5

-define(SOCKS5_VER, 16#05).

-define(SOCKS5_AUTH_NONE,   16#00).
-define(SOCKS5_AUTH_GSSAPI, 16#01).
-define(SOCKS5_AUTH_USER,   16#02).
-define(SOCKS5_AUTH_ERR,    16#ff).

-define(SOCKS5_REQ_CONNECT,  16#01).
-define(SOCKS5_REQ_BIND,     16#02).
-define(SOCKS5_REQ_UDP_ASSOC,16#03).

-define(SOCKS5_ATYP_V4,  16#01).
-define(SOCKS5_ATYP_DOM, 16#03).
-define(SOCKS5_ATYP_V6,  16#04).

-define(SOCKS5_REP_OK,   16#00).
-define(SOCKS5_REP_FAIL, 16#01).
-define(SOCKS5_REP_NOT_ALLOWED, 16#02).
-define(SOCKS5_REP_NET_UNREACHABLE, 16#03).
-define(SOCKS5_REP_HOST_UNREACHABLE, 16#04).
-define(SOCKS5_REP_REFUSED, 16#05).
-define(SOCKS5_REP_TTL_EXPIRED, 16#06).
-define(SOCKS5_REP_CMD_NOT_SUPPORTED, 16#07).
-define(SOCKS5_REP_ATYP_NOT_SUPPORTED, 16#08).

