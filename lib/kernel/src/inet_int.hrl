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

%% family codes to open
-define(INET_AF_INET,          1).
-define(INET_AF_INET6,         2).

%% type codes (gettype)
-define(INET_TYPE_STREAM,     1).
-define(INET_TYPE_DGRAM,      2).

%% socket modes
-define(INET_MODE_LIST,   0).
-define(INET_MODE_BINARY, 1).

%% deliver mode
-define(INET_DELIVER_PORT,    0).
-define(INET_DELIVER_TERM,    1).

%% 5 secs need more ???
-define(INET_CLOSE_TIMEOUT, 5000).

%% state codes (getstate)
-define(INET_F_OPEN,         16#0001).
-define(INET_F_BOUND,        16#0002).
-define(INET_F_ACTIVE,       16#0004).
-define(INET_F_LISTEN,       16#0008).
-define(INET_F_CON,          16#0010).
-define(INET_F_ACC,          16#0020).
-define(INET_F_LST,          16#0040).
-define(INET_F_BUSY,         16#0080).

%% interface stuff
-define(INET_IFNAMSIZ,          16).
-define(INET_IFF_UP,            16#0001).
-define(INET_IFF_BROADCAST,     16#0002).
-define(INET_IFF_LOOPBACK,      16#0004).
-define(INET_IFF_POINTTOPOINT,  16#0008).
-define(INET_IFF_RUNNING,       16#0010).
-define(INET_IFF_MULTICAST,     16#0020).

-define(INET_IFF_DOWN,          16#0100).
-define(INET_IFF_NBROADCAST,    16#0200).
-define(INET_IFF_NPOINTTOPOINT, 16#0800).


%% request codes
-define(INET_REQ_OPEN,          1).
-define(INET_REQ_CLOSE,         2).
-define(INET_REQ_CONNECT,       3).
-define(INET_REQ_PEER,          4).
-define(INET_REQ_NAME,          5).
-define(INET_REQ_BIND,          6).
-define(INET_REQ_SETOPTS,       7).
-define(INET_REQ_GETOPTS,       8).
-define(INET_REQ_GETIX,         9).
%% -define(INET_REQ_GETIF,         10). OBSOLETE
-define(INET_REQ_GETSTAT,       11).
-define(INET_REQ_GETHOSTNAME,   12).
-define(INET_REQ_FDOPEN,        13).
-define(INET_REQ_GETFD,         14).
-define(INET_REQ_GETTYPE,       15).
-define(INET_REQ_GETSTATUS,     16).
-define(INET_REQ_GETSERVBYNAME, 17).
-define(INET_REQ_GETSERVBYPORT, 18).
-define(INET_REQ_SETNAME,       19).
-define(INET_REQ_SETPEER,       20).
-define(INET_REQ_GETIFLIST,     21).
-define(INET_REQ_IFGET,         22).
-define(INET_REQ_IFSET,         23).
-define(INET_REQ_SUBSCRIBE,     24).

%% subscribe codes
-define(INET_SUBS_EMPTY_OUT_Q,  1).

%% reply codes
-define(INET_REP_ERROR,    0).
-define(INET_REP_OK,       1).
-define(INET_REP_DATA,     100).

%% tcp requests
-define(TCP_REQ_ACCEPT,    30).
-define(TCP_REQ_LISTEN,    31).
-define(TCP_REQ_RECV,      32).
-define(TCP_REQ_UNRECV,    33).


-define(LISTEN_BACKLOG, 5).     %% default backlog

%% udp requests
-define(UDP_REQ_RECV,      30).


%% options
-define(INET_OPT_REUSEADDR,      0).
-define(INET_OPT_KEEPALIVE,      1).
-define(INET_OPT_DONTROUTE,      2).
-define(INET_OPT_LINGER,         3).
-define(INET_OPT_BROADCAST,      4).
-define(INET_OPT_OOBINLINE,      5).
-define(INET_OPT_SNDBUF,         6).
-define(INET_OPT_RCVBUF,         7).
-define(TCP_OPT_NODELAY,         10).
-define(UDP_OPT_MULTICAST_IF,    11).
-define(UDP_OPT_MULTICAST_TTL,   12).
-define(UDP_OPT_MULTICAST_LOOP,  13).
-define(UDP_OPT_ADD_MEMBERSHIP,  14).
-define(UDP_OPT_DROP_MEMBERSHIP, 15).
-define(INET_LOPT_BUFFER,        20).
-define(INET_LOPT_HEADER,        21).
-define(INET_LOPT_ACTIVE,        22).
-define(INET_LOPT_PACKET,        23).
-define(INET_LOPT_MODE,          24).
-define(INET_LOPT_DELIVER,       25).
-define(INET_LOPT_EXITONCLOSE,   26).
-define(INET_LOPT_TCP_HIWTRMRK,  27).
-define(INET_LOPT_TCP_LOWTRMRK,  28).
-define(INET_LOPT_BIT8,          29).
-define(INET_LOPT_TCP_SEND_TIMEOUT, 30).

%% interface options
-define(INET_IFOPT_ADDR,      1).
-define(INET_IFOPT_BROADADDR, 2).
-define(INET_IFOPT_DSTADDR,   3).
-define(INET_IFOPT_MTU,       4).
-define(INET_IFOPT_NETMASK,   5).
-define(INET_IFOPT_FLAGS,     6).
-define(INET_IFOPT_HWADDR,    7). %% where support (e.g linux)

%% packet byte values
-define(TCP_PB_RAW,     0).
-define(TCP_PB_1,       1).
-define(TCP_PB_2,       2).
-define(TCP_PB_4,       3).
-define(TCP_PB_ASN1,    4).
-define(TCP_PB_RM,      5).
-define(TCP_PB_CDR,     6).
-define(TCP_PB_FCGI,    7).
-define(TCP_PB_LINE_LF, 8).
-define(TCP_PB_TPKT,    9).
-define(TCP_PB_HTTP,    10).
-define(TCP_PB_HTTPH,   11).

%% bit options
-define(INET_BIT8_CLEAR, 0).
-define(INET_BIT8_SET,   1).
-define(INET_BIT8_ON,    2).
-define(INET_BIT8_OFF,   3).


-define(INET_STAT_RECV_CNT,  1).
-define(INET_STAT_RECV_MAX,  2).
-define(INET_STAT_RECV_AVG,  3).
-define(INET_STAT_RECV_DVI,  4).
-define(INET_STAT_SEND_CNT,  5).
-define(INET_STAT_SEND_MAX,  6).
-define(INET_STAT_SEND_AVG,  7).
-define(INET_STAT_SEND_PEND, 8).
-define(INET_STAT_RECV_OCT,  9).
-define(INET_STAT_SEND_OCT,  10).

%%
%% Port/socket numbers: network standard functions
%%
-define(IPPORT_ECHO,             7).
-define(IPPORT_DISCARD,          9).
-define(IPPORT_SYSTAT,           11).
-define(IPPORT_DAYTIME,          13).
-define(IPPORT_NETSTAT,          15).
-define(IPPORT_FTP,              21).
-define(IPPORT_TELNET,           23).
-define(IPPORT_SMTP,             25).
-define(IPPORT_TIMESERVER,       37).
-define(IPPORT_NAMESERVER,       42).
-define(IPPORT_WHOIS,            43).
-define(IPPORT_MTP,              57).

%%
%% Port/socket numbers: host specific functions
%%
-define(IPPORT_TFTP,             69).
-define(IPPORT_RJE,              77).
-define(IPPORT_FINGER,           79).
-define(IPPORT_TTYLINK,          87).
-define(IPPORT_SUPDUP,           95).

%%
%% UNIX TCP sockets
%%
-define(IPPORT_EXECSERVER,       512).
-define(IPPORT_LOGINSERVER,      513).
-define(IPPORT_CMDSERVER,        514).
-define(IPPORT_EFSSERVER,        520).

%%
%% UNIX UDP sockets
%%
-define(IPPORT_BIFFUDP,          512).
-define(IPPORT_WHOSERVER,        513).
-define(IPPORT_ROUTESERVER,      520). %% 520+1 also used


%%
%% Ports < IPPORT_RESERVED are reserved for
%% privileged processes (e.g. root).
%% Ports > IPPORT_USERRESERVED are reserved
%% for servers, not necessarily privileged.
%%
-define(IPPORT_RESERVED,         1024).
-define(IPPORT_USERRESERVED,     5000).

%% standard port for socks
-define(IPPORT_SOCKS,           1080).

%%
%% Int to bytes
%%
-define(int8(X), [(X) band 16#ff]).

-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int24(X), [((X) bsr 16) band 16#ff,
		   ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
	[((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff]).

%% Bytes to unsigned
-define(u64(X7,X6,X5,X4,X3,X2,X1,X0), 
	( ((X7) bsl 56) bor ((X6) bsl 48) bor ((X5) bsl 40) bor
	  ((X4) bsl 32) bor ((X3) bsl 24) bor ((X2) bsl 16) bor 
	  ((X1) bsl 8) bor (X0)  )).

-define(u32(X3,X2,X1,X0), 
	(((X3) bsl 24) bor ((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u24(X2,X1,X0),
	(((X2) bsl 16) bor ((X1) bsl 8) bor (X0))).

-define(u16(X1,X0),
	(((X1) bsl 8) bor (X0))).
 
-define(u8(X0), (X0)).

%% Bytes to signed
-define(i32(X3,X2,X1,X0),
        (?u32(X3,X2,X1,X0) - 
         (if (X3) > 127 -> 16#100000000; true -> 0 end))).

-define(i24(X2,X1,X0),
        (?u24(X2,X1,X0) - 
         (if (X2) > 127 -> 16#1000000; true -> 0 end))).
	
-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(i8(X0),
	(?u8(X0) -
	 (if (X0) > 127 -> 16#100; true -> 0 end))).

%% macro for use in guard for checking ip address {A,B,C,D}
-define(ip(A,B,C,D),
	(((A) bor (B) bor (C) bor (D)) band (bnot 16#ff)) == 0).

-define(ip6(A,B,C,D,E,F,G,H), 
	(((A) bor (B) bor (C) bor (D) bor (E) bor (F) bor (G) bor (H)) 
	 band (bnot 16#ffff)) == 0).

%% default options (when inet_drv port is started)
%%
%% bufsz   = INET_MIN_BUFFER (8K)
%% header  = 0
%% packet  = 0 (raw)
%% mode    = list
%% deliver = term
%% active  = false
%%

-record(connect_opts, 
	{ 
	  ifaddr = any,     %% bind to interface address
	  port   = 0,       %% bind to port (default is dynamic port)
	  fd      = -1,     %% fd >= 0 => already bound
	  opts   = [{active,true}]
	 }).

-record(listen_opts, 
	{ 
	  ifaddr = any,              %% bind to interface address
	  port   = 0,                %% bind to port (default is dynamic port)
	  backlog = ?LISTEN_BACKLOG, %% backlog
	  fd      = -1,              %% %% fd >= 0 => already bound
	  opts   = [{active,true}]
	 }).

-record(udp_opts,
	{
	  ifaddr = any,
	  port   = 0,
	  fd     = -1,
	  opts   = [{active,true}]
	 }).
