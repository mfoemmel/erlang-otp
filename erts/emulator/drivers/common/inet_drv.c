/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
/*
** Purpose : Tcp/Udp linked in driver
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

/*
 * Define SOFTDEBUG to get "hard" debug output on errors only.
 * Most useful on VxWorks.
 */
/*#define SOFTDEBUG*/

/* All platforms fail on malloc errors. */
#define FATAL_MALLOC


#ifdef __WIN32__

#define INCL_WINSOCK_API_TYPEDEFS 1

#include <winsock2.h>
#include <windows.h>

#include <winsock_func.h>

int send_error_to_logger();

#include <Ws2tcpip.h>
#define HAVE_MULTICAST_SUPPORT
#if 0
#define in6_addr in_addr6      /* reversed ??? */
#define HAVE_IN6
#endif /* 0 */

#include "driver.h"

#undef WANT_NONBLOCKING
#include "sys.h"


#define ERRNO_BLOCK             WSAEWOULDBLOCK

#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#define ENOTSOCK                WSAENOTSOCK
#define EDESTADDRREQ            WSAEDESTADDRREQ
#define EMSGSIZE                WSAEMSGSIZE
#define EPROTOTYPE              WSAEPROTOTYPE
#define ENOPROTOOPT             WSAENOPROTOOPT
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              WSAEOPNOTSUPP
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#define EADDRINUSE              WSAEADDRINUSE
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#define ENETDOWN                WSAENETDOWN
#define ENETUNREACH             WSAENETUNREACH
#define ENETRESET               WSAENETRESET
#define ECONNABORTED            WSAECONNABORTED
#define ECONNRESET              WSAECONNRESET
#define ENOBUFS                 WSAENOBUFS
#define EISCONN                 WSAEISCONN
#define ENOTCONN                WSAENOTCONN
#define ESHUTDOWN               WSAESHUTDOWN
#define ETOOMANYREFS            WSAETOOMANYREFS
#define ETIMEDOUT               WSAETIMEDOUT
#define ECONNREFUSED            WSAECONNREFUSED
#define ELOOP                   WSAELOOP
#undef ENAMETOOLONG
#define ENAMETOOLONG            WSAENAMETOOLONG
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
#undef ENOTEMPTY
#define ENOTEMPTY               WSAENOTEMPTY
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

#define INVALID_EVENT           WSA_INVALID_EVENT

WinSockFuncs winSock;

#define sock_open(af, type, proto) \
    make_noninheritable_handle((*winSock.socket)(af, type, proto))
#define sock_close(s)              (*winSock.closesocket)(s)
#define sock_accept(s, addr, len) \
    make_noninheritable_handle((*winSock.accept)(s, addr, len))
#define sock_connect(s, addr, len) (*winSock.connect)(s, addr, len)
#define sock_listen(s, b)          (*winSock.listen)(s, b)
#define sock_bind(s, addr, len)    (*winSock.bind)(s, addr, len)
#define sock_getopt(s,t,n,v,l)     (*winSock.getsockopt)(s,t,n,v,l)
#define sock_setopt(s,t,n,v,l)     (*winSock.setsockopt)(s,t,n,v,l)
#define sock_name(s, addr, len)    (*winSock.getsockname)(s, addr, len)
#define sock_peer(s, addr, len)    (*winSock.getpeername)(s, addr, len)
#define sock_ntohs(x)              (*winSock.ntohs)(x)
#define sock_ntohl(x)              (*winSock.ntohl)(x)
#define sock_htons(x)              (*winSock.htons)(x)
#define sock_htonl(x)              (*winSock.htonl)(x)
#define sock_send(s,buf,len,flag)  (*winSock.send)(s,buf,len,flag)
#define sock_sendv(s, vec, size, np, flag) \
                (*winSock.WSASend)(s, (WSABUF *)(vec), size, np, \
				   flag, NULL, NULL)
#define sock_recv(s,buf,len,flag)  (*winSock.recv)(s, buf, len, flag)

#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                (*winSock.recvfrom)(s,buf,blen,flag,addr,alen)
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                (*winSock.sendto)(s,buf,blen,flag,addr,alen)
#define sock_hostname(buf, len)    (*winSock.gethostname)(buf, len)

#define sock_errno()               (*winSock.WSAGetLastError)()
#define sock_create_event(d)       (*winSock.WSACreateEvent)()
#define sock_close_event(e)        (*winSock.WSACloseEvent)(e)
#define sock_select(D, Flags, OnOff) winsock_event_select(D, Flags, OnOff)
#define SET_BLOCKING(s)           (*winSock.ioctlsocket)(s, FIONBIO, &zero_value)
#define SET_NONBLOCKING(s)        (*winSock.ioctlsocket)(s, FIONBIO, &one_value)

static unsigned long zero_value = 0;
static unsigned long one_value = 1;

#else /* if ! __WIN32__ */

#ifdef VXWORKS
#include <sockLib.h>
#include <sys/times.h>
#include <iosLib.h>
#include <taskLib.h>
#include <selectLib.h>
#include <ioLib.h>
#else
#include <sys/time.h>
#include <netdb.h>
#endif

#include <sys/socket.h>
#include <netinet/in.h>

#ifdef VXWORKS
#include <rpc/rpctypes.h>
#endif
#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif

#include <netinet/tcp.h>
#include <arpa/inet.h>


/* includes for getif */
#ifndef VXWORKS
#include <sys/param.h>
#endif
#include <sys/ioctl.h>

#include <net/if.h>
#ifndef VXWORKS
#include <arpa/nameser.h>
#endif
#include <arpa/inet.h>

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"
#include "driver.h"

#define INVALID_SOCKET -1
#define INVALID_EVENT  -1
#define SOCKET_ERROR   -1
#define SOCKET int
#define HANDLE int
#define FD_READ    DO_READ
#define FD_WRITE   DO_WRITE
#define FD_CLOSE   0
#define FD_CONNECT DO_WRITE
#define FD_ACCEPT  DO_READ

extern int close();
extern int gethostname();

#define sock_open(af, type, proto)  socket(af, type, proto)
#define sock_close(s)               close(s)
#define sock_accept(s, addr, len)   accept(s, addr, len)
#define sock_connect(s, addr, len)  connect(s, addr, len)
#define sock_listen(s, b)           listen(s, b)
#define sock_bind(s, addr, len)     bind(s, addr, len)
#ifdef VXWORKS
#define sock_getopt(s,t,n,v,l)      wrap_sockopt(&getsockopt,\
						 s,t,n,v,(unsigned int)(l))
#define sock_setopt(s,t,n,v,l)      wrap_sockopt(&setsockopt,\
						 s,t,n,v,(unsigned int)(l))
#else
#define sock_getopt(s,t,n,v,l)      getsockopt(s,t,n,v,l)
#define sock_setopt(s,t,n,v,l)      setsockopt(s,t,n,v,l)
#endif
#define sock_name(s, addr, len)     getsockname(s, addr, len)
#define sock_peer(s, addr, len)     getpeername(s, addr, len)
#define sock_ntohs(x)               ntohs(x)
#define sock_ntohl(x)               ntohl(x)
#define sock_htons(x)               htons(x)
#define sock_htonl(x)               htonl(x)
#define sock_send(s,buf,len,flag)   send(s,buf,len,flag)
#define sock_sendv(s, vec, size, np, flag) \
		(*(np) = writev(s, vec, size))
#define sock_recv(s,buf,len,flag)   recv(s,buf,len,flag)
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                recvfrom(s,buf,blen,flag,addr,alen)
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto(s,buf,blen,flag,addr,alen)
#define sock_hostname(buf, len)    gethostname(buf, len)

#define sock_errno()                errno
#define sock_create_event(d)        (d)->s   /* return file descriptor */
#define sock_close_event(e)                  /* do nothing */

#define sock_select(d, flags, onoff) do { \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        driver_select((d)->port, (d)->event, (flags), (onoff)); \
   } while(0)

#endif /* end of non-WIN32 system specific includes and defines */

/* To avoid warnings about implicit casts in ioctl */
#ifdef VXWORKS
#define sock_ioctl(A,B,C) ioctl((A),(B),(int) (C))
#else
#define sock_ioctl(A,B,C) ioctl(A,B,C)
#endif

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define get_little_int32(s) ((((unsigned char*) (s))[3] << 24) | \
			     (((unsigned char*) (s))[2] << 16)  | \
			     (((unsigned char*) (s))[1] << 8) | \
			     (((unsigned char*) (s))[0]))

#define INET_AF_INET  1
#define INET_AF_INET6 2

#define INET_F_OPEN         0x0001
#define INET_F_BOUND        0x0002
#define INET_F_ACTIVE       0x0004
#define INET_F_LISTEN       0x0008
#define INET_F_CON          0x0010
#define INET_F_ACC          0x0020
#define INET_F_LST          0x0040
#define INET_F_BUSY         0x0080 

#define INET_STATE_CLOSED    0
#define INET_STATE_OPEN      (INET_F_OPEN)
#define INET_STATE_BOUND     (INET_STATE_OPEN | INET_F_BOUND)
#define INET_STATE_CONNECTED (INET_STATE_BOUND | INET_F_ACTIVE)

#define IS_OPEN(d) \
 (((d)->state & INET_F_OPEN) == INET_F_OPEN)

#define IS_BOUND(d) \
 (((d)->state & INET_F_BOUND) == INET_F_BOUND)

#define IS_CONNECTED(d) \
  (((d)->state & INET_STATE_CONNECTED) == INET_STATE_CONNECTED)

#define IS_BUSY(d) \
  (((d)->state & INET_F_BUSY) == INET_F_BUSY)

#define INET_REQ_OPEN        1
#define INET_REQ_CLOSE       2
#define INET_REQ_CONNECT     3
#define INET_REQ_PEER        4
#define INET_REQ_NAME        5
#define INET_REQ_BIND        6
#define INET_REQ_SETOPTS     7
#define INET_REQ_GETOPTS     8
#define INET_REQ_IX          9
#define INET_REQ_GETIF       10
#define INET_REQ_GETSTAT     11
#define INET_REQ_GETHOSTNAME 12
#define INET_REQ_FDOPEN      13

#define INET_REP_OPEN        1
#define INET_REP_CLOSE       2
#define INET_REP_CONNECT     3
#define INET_REP_PEER        4
#define INET_REP_NAME        5
#define INET_REP_BIND        6
#define INET_REP_SETOPTS     7
#define INET_REP_GETOPTS     8
#define INET_REP_IX          9
#define INET_REP_GETIF       10
#define INET_REP_GETSTAT     11
#define INET_REP_GETHOSTNAME 12
#define INET_REP_FDOPEN      13

#define INET_REP_ERROR       0
#define INET_REP_DATA        100

#define INET_OPT_REUSEADDR  0   /* enable/disable local address reuse */
#define INET_OPT_KEEPALIVE  1   /* enable/disable keep connections alive */
#define INET_OPT_DONTROUTE  2   /* enable/disable routing for messages */
#define INET_OPT_LINGER     3   /* linger on close if data is present */
#define INET_OPT_BROADCAST  4   /* enable/disable transmission of broadcase */
#define INET_OPT_OOBINLINE  5   /* enable/disable out-of-band data in band */
#define INET_OPT_SNDBUF     6   /* set send buffer size */
#define INET_OPT_RCVBUF     7   /* set receive buffer size */
#define TCP_OPT_NODELAY     10  /* don't delay send to coalesce packets */
#define UDP_OPT_MULTICAST_IF 11  /* set/get IP multicast interface */
#define UDP_OPT_MULTICAST_TTL 12 /* set/get IP multicast timetolive */
#define UDP_OPT_MULTICAST_LOOP 13 /* set/get IP multicast loopback */
#define UDP_OPT_ADD_MEMBERSHIP 14 /* add an IP group membership */
#define UDP_OPT_DROP_MEMBERSHIP 15 /* drop an IP group membership */
/* LOPT is local options */
#define INET_LOPT_BUFFER    20  /* min buffer size hint */
#define INET_LOPT_HEADER    21  /* list header size */
#define INET_LOPT_ACTIVE    22  /* enable/disable active receive */
#define INET_LOPT_PACKET    23  /* packet header type (TCP) */

/* Enumerate the statistics ops */
#define INET_STAT_RECV_CNT  1
#define INET_STAT_RECV_MAX  2
#define INET_STAT_RECV_AVG  3
#define INET_STAT_RECV_DVI  4
#define INET_STAT_SEND_CNT  5
#define INET_STAT_SEND_MAX  6
#define INET_STAT_SEND_AVG  7
#define INET_STAT_SEND_PND  8

#define INET_MIN_BUFFER (1024*8)   /* guaranteed minimum buffer size */
#define INET_MAX_BUFFER (1024*64)  /* maximum buffer ever needed */

#define INET_HIGH_WATERMARK (1024*8) /* 8k pending high => busy */
#define INET_LOW_WATERMARK  (1024*4) /* 4k pending => allow more */

#define INET_INFINITY  0xffffffff  /* infinity value */

/* INET_UDP_POLL could be an option !! */
#define INET_UDP_POLL   5        /* maximum number of packets to poll */

#define BIN_REALLOC_LIMIT(x)  (((x)*3)/4)  /* 75% */

#define TCP_REQ_OPEN      INET_REQ_OPEN
#define TCP_REQ_FDOPEN    INET_REQ_FDOPEN
#define TCP_REQ_CLOSE     INET_REQ_CLOSE
#define TCP_REQ_CONNECT   INET_REQ_CONNECT
#define TCP_REQ_PEER      INET_REQ_PEER
#define TCP_REQ_NAME      INET_REQ_NAME
#define TCP_REQ_BIND      INET_REQ_BIND
#define TCP_REQ_SETOPTS   INET_REQ_SETOPTS
#define TCP_REQ_GETOPTS   INET_REQ_GETOPTS
#define TCP_REQ_IX        INET_REQ_IX
#define TCP_REQ_ACCEPT    20
#define TCP_REQ_LISTEN    21
#define TCP_REQ_ASEND     22  /* async send used by distribution */
#define TCP_REQ_RECV      23
#define TCP_REQ_SEND      24  /* sync send */

#define TCP_REP_ERROR     INET_REP_ERROR
#define TCP_REP_OPEN      INET_REP_OPEN
#define TCP_REP_FDOPEN    INET_REP_FDOPEN
#define TCP_REP_CLOSE     INET_REP_CLOSE
#define TCP_REP_CONNECT   INET_REP_CONNECT
#define TCP_REP_PEER      INET_REP_PEER
#define TCP_REP_NAME      INET_REP_NAME
#define TCP_REP_BIND      INET_REP_BIND
#define TCP_REP_SETOPTS   INET_REP_SETOPTS
#define TCP_REP_GETOPTS   INET_REP_GETOPTS
#define TCP_REP_IX        INET_REP_IX
#define TCP_REP_ACCEPT    20
#define TCP_REP_LISTEN    21
#define TCP_REP_SEND      24

#define TCP_REP_DATA        INET_REP_DATA
#define TCP_REP_ERROR       INET_REP_ERROR


#define TCP_STATE_CLOSED     INET_STATE_CLOSED
#define TCP_STATE_OPEN       (INET_F_OPEN)
#define TCP_STATE_BOUND      (TCP_STATE_OPEN | INET_F_BOUND)
#define TCP_STATE_CONNECTED  (TCP_STATE_BOUND | INET_F_ACTIVE)
#define TCP_STATE_LISTEN     (TCP_STATE_BOUND | INET_F_LISTEN)
#define TCP_STATE_LISTENING  (TCP_STATE_LISTEN | INET_F_LST)
#define TCP_STATE_CONNECTING (TCP_STATE_BOUND | INET_F_CON)
#define TCP_STATE_ACCEPTING  (INET_F_ACC)

#define TCP_PB_RAW  0
#define TCP_PB_1    1
#define TCP_PB_2    2
#define TCP_PB_4    3
#define TCP_PB_ASN1 4
#define TCP_PB_RM   5
#define TCP_PB_CDR  6
#define TCP_PB_FCGI 7

#define TCP_MAX_RECV        1024       /* < INET_MIN_BUFFER+TCP_BYTE_GAP */
#define TCP_MAX_PACKET_SIZE 0x1000000  /* 16 M */
#define TCP_MAX_HDR         16         /* for asn1 tag etc */
#define TCP_BYTE_GAP        1          /* number of byte free before buffer */

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */
#define UDP_REQ_OPEN         INET_REQ_OPEN
#define UDP_REQ_FDOPEN       INET_REQ_FDOPEN
#define UDP_REQ_CLOSE        INET_REQ_CLOSE
#define UDP_REQ_CONNECT      INET_REQ_CONNECT
#define UDP_REQ_PEER         INET_REQ_PEER
#define UDP_REQ_NAME         INET_REQ_NAME
#define UDP_REQ_BIND         INET_REQ_BIND
#define UDP_REQ_SETOPTS      INET_REQ_SETOPTS
#define UDP_REQ_GETOPTS      INET_REQ_GETOPTS
#define UDP_REQ_IX           INET_REQ_IX
#define UDP_REQ_SEND         20
#define UDP_REQ_SENDTO       21
#define UDP_REQ_RECV         22

#define UDP_REP_OPEN         INET_REP_OPEN
#define UDP_REP_FDOPEN       INET_REP_FDOPEN
#define UDP_REP_CLOSE        INET_REP_CLOSE
#define UDP_REP_CONNECT      INET_REP_CONNECT
#define UDP_REP_NAME         INET_REP_NAME
#define UDP_REP_PEER         INET_REP_PEER
#define UDP_REP_BIND         INET_REP_BIND
#define UDP_REP_SETOPTS      INET_REP_SETOPTS
#define UDP_REP_GETOPTS      INET_REP_GETOPTS
#define UDP_REP_IX           INET_REP_IX
#define UDP_REP_DATA         INET_REP_DATA
#define UDP_REP_ERROR        INET_REP_ERROR

#define UDP_STATE_CLOSED     INET_STATE_CLOSE
#define UDP_STATE_OPEN       (INET_F_OPEN)
#define UDP_STATE_BOUND      (UDP_STATE_OPEN | INET_F_BOUND)
#define UDP_STATE_CONNECTED  (UDP_STATE_BOUND | INET_F_ACTIVE)

/* for AF_INET & AF_INET6 */
#define inet_address_port(x) ((x)->sai.sin_port)

/* The general purpose sockaddr */
typedef union {
    struct sockaddr sa;
    struct sockaddr_in sai;
#ifdef HAVE_IN6
    struct sockaddr_in6 sai6;
#endif
} inet_address;

typedef struct {
    SOCKET s;                   /* the socket or INVALID_SOCKET if not open */
    HANDLE event;               /* Event handle (same as s in unix) */
    long  event_mask;           /* current FD events */
    long  port;                 /* the port identifier */
    int   state;                /* status */
    int   prebound;             /* only set when opened with inet_fdopen */
    int   active;               /* active/passive mode */
    int   stype;                /* socket type SOCK_STREAM/SOCK_DGRAM */
    int   sfamily;              /* address family */
    int   htype;                /* header type (tcp only?) */
    int   ix;                   /* descriptor index */
    inet_address remote;        /* remote address (when connected) */

    int   bufsz;                /* minimum buffer constraint */
    unsigned int hsz;           /* the list header size, -1 is large !!! */
    /* statistics */
    unsigned long recv_cnt;     /* number of packets received */
    unsigned long recv_max;     /* maximum packet size received */
    double recv_avg;            /* average packet size received */
    double recv_dvi;            /* avarage deviation from avg_size */
    unsigned long send_cnt;     /* number of packets sent */
    unsigned long send_max;     /* maximum packet send */
    double send_avg;            /* average packet size sent */
} inet_descriptor;

typedef struct {
    inet_descriptor inet;  /* common data structre (DONT MOVE) */
    char  h_store[TCP_MAX_HDR];   /* storage for split packet header */
    int   h_len;           /* number of bytes in h_store */
    int   i_ix;            /* accept descriptor index / read indicator */
    int   i_length;        /* input request length */
    char* i_buffer;        /* current input buffer */
    char* i_ptr;           /* current pos in input buffer */
    int   i_remain;        /* remaining length of input */
} tcp_descriptor;

typedef struct {
    inet_descriptor inet;   /* common data structre (DONT MOVE) */
} udp_descriptor;

/* convert descriptor poiner to inet_descriptor pointer */
#define INETP(d) (&(d)->inet)

static int tcp_inet_init(void);
static int tcp_inet_stop(tcp_descriptor *);
static int tcp_inet_command(tcp_descriptor *, char *, int);
static int tcp_inet_input(tcp_descriptor *, HANDLE);
static int tcp_inet_output(tcp_descriptor *, HANDLE);
static long tcp_inet_start(long, char *);
static int  tcp_inet_ctl(tcp_descriptor *, int, char*, int, char**, int);
static int  tcp_inet_timeout(tcp_descriptor *);
#ifdef __WIN32__
static int tcp_inet_event(tcp_descriptor *, HANDLE);
#endif

static struct driver_entry tcp_inet_driver_entry = 
{
    tcp_inet_init,  /* inet_init will add this driver !! */
    tcp_inet_start, 
    tcp_inet_stop, 
    tcp_inet_command,
#ifdef __WIN32__
    tcp_inet_event,
    null_func,
#else
    tcp_inet_input,
    tcp_inet_output,
#endif
    "tcp_inet",
    NULL,
    NULL,
    tcp_inet_ctl,
    tcp_inet_timeout,
    NULL
};


static int udp_inet_init(void);
static int udp_inet_stop(udp_descriptor *);
static int udp_inet_command(udp_descriptor *, char *, int);
static int udp_inet_input(udp_descriptor *, HANDLE);
static long udp_inet_start(long, char *);
static int  udp_inet_ctl(udp_descriptor *, int, char *, int, char **, int);
static int  udp_inet_timeout(udp_descriptor* );
#ifdef __WIN32__
static int udp_inet_event(udp_descriptor *, HANDLE);
static SOCKET make_noninheritable_handle(SOCKET s);
static int winsock_event_select(inet_descriptor *, int, int);
#endif

static struct driver_entry udp_inet_driver_entry = 
{
    udp_inet_init,  /* inet_init will add this driver !! */
    udp_inet_start,
    udp_inet_stop,
    udp_inet_command,
#ifdef __WIN32__
    udp_inet_event,
#else
    udp_inet_input,
#endif
    null_func, 
    "udp_inet",
    NULL,
    NULL,
    udp_inet_ctl,
    udp_inet_timeout,
    NULL
};

/* send function */
static int tcp_send(tcp_descriptor *, char *, int);
/* passive receive function */
static int tcp_recv_passive(tcp_descriptor *, int, unsigned long);   
/* receive function */
static int tcp_recv(tcp_descriptor *);           
/* Helper for setopts */
static int tcp_maybe_deliver(tcp_descriptor* desc);

static inet_descriptor** inet_desc_table;
static int inet_desc_ix;
static int inet_desc_size;         /* number of descriptors */

#ifndef INET_MAX_DESCRIPTOR
#define INET_MAX_DESCRIPTOR sys_max_files()
#endif

static int inet_buf_size = -1;     /* current buffer size */
static char* inet_buf;             /* dynamic buffer */

/* For debugging purpouses, the line numper is always sent to do_inet_error */
#define inet_error(A,B) do_inet_error(A,B,__LINE__)

static int do_inet_error(inet_descriptor*, int , int);
static int inet_init(void);

struct driver_entry inet_driver_entry = 
{
    inet_init,  /* inet_init will add tcp and udp drivers */
    (long (*)()) null_func,   /* cast to avoid warnings */
    null_func,
    null_func,
    null_func,
    null_func,
    "inet"
};

/*
 * Malloc wrapper, we could actually use safe_alloc instead, but
 * we would like to change the behaviour for different 
 * systems here.
 */

#ifdef FATAL_MALLOC

/* erl_exit0 is not necessarily available to ddll drivers, not the
   macros which are supposed to be used for calling it (in global.h) so
   it is not defined in driver.h. This is a statically linked in 
   driver though... */
extern void erl_exit0(char *, int, int, char *, ...);

static void *alloc_wrapper(size_t size){
    void *ret = sys_alloc_from(190, size);
    if(ret == NULL) 
	erl_exit0(__FILE__,__LINE__,1,"Out of virtual memory in malloc");
    return ret;
}

static void *realloc_wrapper(void *current, size_t size){
    void *ret = sys_realloc(current,size);
    if(ret == NULL) 
	erl_exit0(__FILE__,__LINE__,1,"Out of virtual memory in realloc");
    return ret;
}

#define ALLOC(X) alloc_wrapper(X)
#define REALLOC(X,Y) realloc_wrapper(X,Y)
#else
#define ALLOC(X) sys_alloc_from(190, X)
#define REALLOC(X,Y) sys_realloc(X,Y)

#endif /* FATAL_MALLOC */

#ifdef __WIN32__

static int dummy_start(long port, int size)
{
    return port;
}

static int dummy_command(int port, char *buf, int len)
{
    static char code[1] = { INET_REP_ERROR };
    static char error[] = "no_winsock2";

    driver_output2(port, code, 1, error, sizeof(error) - 1);
    driver_failure_atom(error);
    return 0;
}

static struct driver_entry dummy_tcp_driver_entry = 
{
    null_func,			/* init */
    dummy_start,		/* start */
    null_func,			/* stop */
    dummy_command,		/* command */
    null_func,			/* input */
    null_func,			/* output */
    "tcp_inet",			/* name */
};

static struct driver_entry dummy_udp_driver_entry = 
{
    null_func,			/* init */
    dummy_start,		/* start */
    null_func,			/* stop */
    dummy_command,		/* command */
    null_func,			/* input */
    null_func,			/* output */
    "udp_inet",			/* name */
};

#endif

#if defined(HARDDEBUG)
#undef DEBUGF
#define DEBUGF(X) (debug_line = __LINE__, mydebug X)
#define SOFTDEBUG
#endif
#if defined(SOFTDEBUG)
#define DEBUGF2(X) (debug_line = __LINE__, mydebug X)

#include <stdarg.h>

static int debug_line;

static int mydebug(char *format, ...){
    va_list ap;
    va_start(ap,format);
    fprintf(stderr,"INET_DEBUG(%d): ",debug_line);
    vfprintf(stderr,format,ap);
    fprintf(stderr,"\r");
    va_end(ap);
    return 0;
}
#else /* not SOFTDEBUG */
#define DEBUGF2(X) DEBUGF(X)
#endif

#if (defined(SOFTDEBUG) || defined(HARDDEBUG)) && defined(VXWORKS)
#include <symLib.h>
extern SYMTAB_ID sysSymTbl;
static void do_vx_show_routine(char *routine_name)
{
    int (*show)(void);
    SYM_TYPE ignore;
    /* Look routine up */
    if (symFindByName(sysSymTbl, routine_name, (char **) &show, &ignore) != OK) {
	DEBUGF2(("Routine %s is not present in system.\n", routine_name));
	return;
    }
    /* And run it... */
    DEBUGF2(("%s() -> \n", routine_name));
    (*show)();
    return;
}
#if CPU == CPU32
#define DO_VXWORKS_SHOW(Routine) do_vx_show_routine("_" #Routine)
#else
#define DO_VXWORKS_SHOW(Routine) do_vx_show_routine(#Routine)
#endif
#else
#define DO_VXWORKS_SHOW(Ignored) /* Nothing */
#endif /* (defined(SOFTDEBUG) || defined(HARDDEBUG)) && defined(VXWORKS) */   


/* ----------------------------------------------------------------------------

   INET

---------------------------------------------------------------------------- */


static int inet_init(void)
{
    int sz;

#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;

    wVersionRequested = MAKEWORD(2,0);
    if (!tcp_lookup_functions())
	goto error;
    if ((*winSock.WSAStartup)(wVersionRequested, &wsaData) != 0)
	goto error;
    if ((LOBYTE(wsaData.wVersion) != 2) || (HIBYTE(wsaData.wVersion) != 0))
	goto error;
#endif

    inet_desc_size = INET_MAX_DESCRIPTOR;
    sz = sizeof(inet_descriptor*) * inet_desc_size;

    if ((inet_desc_table = (inet_descriptor**) ALLOC(sz)) == NULL)
	goto error;
    if ((inet_buf = (char*) ALLOC(INET_MIN_BUFFER)) == NULL) {
	sys_free(inet_desc_table);
	goto error;
    }
    inet_buf_size = INET_MIN_BUFFER;
    sys_memzero(inet_desc_table, sz);
    inet_desc_ix = 0;

    /* add tcp and udp drivers */
    add_driver_entry(&tcp_inet_driver_entry);
    add_driver_entry(&udp_inet_driver_entry);
    /* remove the dummy inet driver */
    remove_driver_entry(&inet_driver_entry);
    return 0;

 error:
#ifdef __WIN32__
    if (winSock.WSACleanup) {
	(*winSock.WSACleanup)();
    }
    sys_printf(CBUF,
	       "Failed to load or start Winsock2 (ws2_32.dll).\n\n"
	       "Erlang modules using TCP/IP (gen_tcp, ftp, and others),\n"
	       "will not work.  Furthermore, this Erlang node will not be able to\n"
	       "communicate with other Erlang nodes.\n\n"
	       "Refer to the Installation Guide for instructions about installing\n"
	       "Winsock2 on Windows 95.\n\n");
    send_error_to_logger(0);
    add_driver_entry(&dummy_tcp_driver_entry);
    add_driver_entry(&dummy_udp_driver_entry);
#endif
    remove_driver_entry(&inet_driver_entry);
    return -1;
}

/* XXX add this as a finish routine */
#if 0
static void inet_finish(void *arg)
{
    int i;
    int found = 0;
    inet_descriptor* desc;

    for (i = 0; i < inet_desc_size; i++) {
	if ((desc = inet_desc_table[i]) != NULL) {
	    found++;
	    if (desc->stype == SOCK_STREAM)
		tcp_inet_stop((tcp_descriptor*) desc);
	    else if (desc->stype == SOCK_DGRAM)
		udp_inet_stop((tcp_descriptor*) desc);
	}
    }
    sys_free(inet_desc_table);
    sys_free(inet_buf);
}
#endif

/*
 * Sends back an error reply to Erlang.
 */


static int do_inet_error(inet_descriptor* desc, int err, int line)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;


    DEBUGF2(("Inet_error called from %d\n",line));

    DEBUGF2(("h_len = %d\n", ((tcp_descriptor *) desc)->h_len));

    /*
     * Contents of buffer sent back:
     *
     * +-----------------------------------------+
     * | INET_REP_ERROR | Posix error id string  |
     * +-----------------------------------------+
     */

    response[0] = INET_REP_ERROR;
    for (s = erl_errno_id(err), t = response+1; *s; s++, t++)
	*t = tolower(*s);
#if (defined(SOFTDEBUG) || defined(HARDDEBUG)) && defined(VXWORKS)
    /* Well... What coused the error on VxWorks? 
       Lets look at the system tables. */
    DO_VXWORKS_SHOW(tcpstatShow);
    DO_VXWORKS_SHOW(iosFdShow);
    DO_VXWORKS_SHOW(mbufShow);
    DO_VXWORKS_SHOW(netStackDataPoolShow);
    DO_VXWORKS_SHOW(netStackSysPoolShow);
#endif
    *t = '\0'; /* For debug output below only */
    DEBUGF2(("inet_error: %s\n", (response+1)));
    driver_output2(desc->port, response, t - response, NULL, 0);
    return 0;
}

/* Send a string error message close, timeout etc (non-posix error) */
static int inet_report(inet_descriptor* desc,  char* mesg)
{
    char response[256];
    int len = strlen(mesg);
    int n = ((len > 255) ? 255 : len);

    response[0] = INET_REP_ERROR;
    
    memcpy(response+1, mesg, n);
    driver_output2(desc->port, response, n+1, NULL, 0);
    return 0;
}

/* reply with [R0] */
static void inet_reply(inet_descriptor* desc, int rep)
{
    char reply = rep;
    driver_output2(desc->port, &reply, 1, NULL, 0);
}


/* reply with [INET_REP_DATA, H1,..Hsz | Data] */
static void inet_reply_data(inet_descriptor* desc, char* buf, int len)
{
    char reply;
    int hsz = desc->hsz;

    if (hsz > 0) {
	*(buf-1) = INET_REP_DATA;
	if (hsz > len)  /* no binary included  */
	    driver_output2(desc->port, buf-1, 1+len, NULL, 0);
	else  /* partial binary included */
	    driver_output2(desc->port, buf-1, 1+hsz, buf+hsz, len-hsz);
    }
    else {
	reply = INET_REP_DATA;
	driver_output2(desc->port, &reply, 1, buf, len);
    }
}

/*
** Set a inaddr structure:
**  src = [P1,P0,X1,X2,.....]
**  dst points to a structure large enugh to keep any kind
**  of inaddr.
** *len is set to length of src on call
** and is set to actual length of dst on return
** return NULL on error and ptr after port address on success
*/
static char* inet_set_address(int family, inet_address* dst, 
			      char* src, int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= 6)) {
	sys_memzero((char*)dst, sizeof(struct sockaddr_in));
	port = get_int16(src);
	dst->sai.sin_family = family;
	dst->sai.sin_port   = sock_htons(port);
	sys_memcpy(&dst->sai.sin_addr, src+2, sizeof(struct in_addr));
	*len = sizeof(struct sockaddr_in);
	return src + 6;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= 18)) {
	sys_memzero((char*)dst, sizeof(struct sockaddr_in6));
	port = get_int16(src);
	dst->sai6.sin6_family = family;
	dst->sai6.sin6_port   = sock_htons(port);
	dst->sai6.sin6_flowinfo = 0;   /* XXX this may be set as well ?? */
	sys_memcpy(&dst->sai6.sin6_addr, src+2, sizeof(struct in6_addr));
	*len = sizeof(struct sockaddr_in6); 
	return src + 18;
    }
#endif
    return NULL;
}


/* Get a inaddr structure
** src = inaddr structure
** *len is the lenght of structure
** dst is filled with [P1,P0,X1,....] 
** and *len is the length of dst on return 
** (suitable to deliver to erlang)
*/
static int inet_get_address(int family, char* dst, inet_address* src, int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= sizeof(struct sockaddr_in))) {
	port = sock_ntohs(src->sai.sin_port);
	put_int16(port, dst);
	sys_memcpy(dst+2, (char*)&src->sai.sin_addr, sizeof(struct in_addr));
	*len = 6;
	return 0;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= sizeof(struct sockaddr_in6))) {
	port = sock_ntohs(src->sai6.sin6_port);
	put_int16(port, dst);
	sys_memcpy(dst+2, (char*)&src->sai6.sin6_addr,sizeof(struct in6_addr));
	*len = 18;
	return 0;
    }
#endif
    return -1;
}


/* Reply the local socket name */
/* get local socket name P1 P0 A B C D */
static int inet_local_name(inet_descriptor* desc)
{
    char buf[sizeof(inet_address)+1];
    inet_address name;
    int sz = sizeof(name);

    if (!IS_BOUND(desc))
	return inet_error(desc, EINVAL); /* address is not valid */

    if (sock_name(desc->s, (struct sockaddr*) &name, &sz) == SOCKET_ERROR)
	return inet_error(desc, sock_errno());
    if (inet_get_address(desc->sfamily, buf+1, &name, &sz) < 0)
	return inet_error(desc, EINVAL);
    buf[0] = INET_REP_NAME;
    driver_output2(desc->port, buf, sz+1, NULL, 0);
    return 0;
}

/* Return the peer name P1 P0 A B C D */
static int inet_peer_name(inet_descriptor* desc)
{
    char buf[sizeof(inet_address)+1];
    inet_address peer;
    int sz = sizeof(peer);

    if (!(desc->state & INET_F_ACTIVE))
	return inet_error(desc, ENOTCONN);

    if (sock_peer(desc->s, (struct sockaddr*)&peer,&sz) == SOCKET_ERROR)
	return inet_error(desc, sock_errno());
    if (inet_get_address(desc->sfamily, buf+1, &peer, &sz) < 0)
	return inet_error(desc, EINVAL);
    buf[0] = INET_REP_PEER;
    driver_output2(desc->port, buf, sz+1, NULL, 0);
    return 0;
}

/* Bind local address to a port */
/* ptr must point on buffer with [P1,P0,X1,X2,X3....] */

static int inet_bind(inet_descriptor* desc, char* ptr, int len)
{
    char buf[3];
    inet_address local;
    short port;

    if (desc->state == INET_STATE_CLOSED)
	return inet_error(desc, ENOTSOCK);
    if (desc->state != INET_STATE_OPEN)
	return inet_error(desc, EADDRINUSE);

    if (inet_set_address(desc->sfamily, &local, ptr, &len) == NULL)
	return inet_error(desc, EINVAL);

    if (sock_bind(desc->s,(struct sockaddr*) &local, len) == SOCKET_ERROR)
	return inet_error(desc, sock_errno());

    desc->state = INET_STATE_BOUND;

    if ((port = inet_address_port(&local)) == 0) {
	int length = sizeof(local);
	sock_name(desc->s, (struct sockaddr*) &local, &length);
	port = inet_address_port(&local);
    }
    port = sock_ntohs(port);
    buf[0] = INET_REP_BIND;
    put_int16(port, buf+1);
    driver_output2(desc->port, buf, 3, NULL, 0);
    return 0;
}

/* resize the inet_buf (just grow, fix later) */
static int inet_set_buffer(inet_descriptor* desc, int size)
{
    char* buf;

    if (size < 0)
	return -1;
    if (size > INET_MAX_BUFFER)
	size = INET_MAX_BUFFER;

    if (size > inet_buf_size) {
	if ((buf = (char*) REALLOC(inet_buf, size)) == NULL)
	    return -1;
	inet_buf = buf;
	inet_buf_size = size;
    }
    desc->bufsz = size;
    return size;
}


static void inet_close(inet_descriptor* desc)
{
  if ((desc->prebound == 0) && (desc->state & INET_F_OPEN)) {      

#ifdef __WIN32__
      driver_select(desc->port, desc->event, DO_READ, 0);
#endif

      sock_select(desc, FD_READ | FD_WRITE | FD_CLOSE, 0);
      sock_close(desc->s);
      sock_close_event(desc->event);
      desc->s = INVALID_SOCKET;
      desc->event = INVALID_EVENT;
      desc->event_mask = 0;
      desc->state = INET_STATE_CLOSED;

    }
}

static int inet_open(inet_descriptor* desc, int domain, int type)
{
    char reply;

    if (desc->state != INET_STATE_CLOSED)
	return inet_error(desc, EADDRINUSE);
    if ((desc->s = sock_open(domain, type, 0)) == INVALID_SOCKET)
	return inet_error(desc, sock_errno());
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return inet_error(desc, sock_errno());
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 1);
#endif
    desc->state = INET_STATE_OPEN;
    desc->stype = type;
    desc->sfamily = domain;
    reply = INET_REP_OPEN;
    driver_output2(desc->port, &reply, 1, NULL, 0);
    return 0;
}

/* as inet_open but pass in an open socket (MUST BE OF RIGHT TYPE) */
static int inet_fdopen(inet_descriptor* desc, int domain, int type, SOCKET s)
{
    char reply;
    inet_address name;
    int sz = sizeof(name);

    /* check that it is a socket and that the socket is bound */
    if (sock_name(s, (struct sockaddr*) &name, &sz) == SOCKET_ERROR)
	return inet_error(desc, sock_errno());
    desc->s = s;
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return inet_error(desc, sock_errno());
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 1);
#endif
    desc->state = INET_STATE_BOUND; /* always bound ? */
    desc->prebound = 1; /* used to prevent a real close */
    desc->stype = type;
    desc->sfamily = domain;
    reply = INET_REP_FDOPEN;
    driver_output2(desc->port, &reply, 1, NULL, 0);
    return 0;
}


static int inet_ix(inet_descriptor* desc)
{
    char abuf[3];

    abuf[0] = INET_REP_IX;
    put_int16(desc->ix, abuf+1);
    driver_output2(desc->port, abuf, 3, NULL, 0);
    return 0;
}


struct addr_if {
    struct in_addr addr;   /* interface address */
    struct in_addr baddr;  /* broadcast address */
    struct in_addr mask;   /* netmask */
};

static struct in_addr net_mask(struct in_addr in)
{
    register u_long i = sock_ntohl(in.s_addr);

    if (IN_CLASSA(i))
	in.s_addr = sock_htonl(IN_CLASSA_NET);
    else if (IN_CLASSB(i))
	in.s_addr = sock_htonl(IN_CLASSB_NET);
    else
	in.s_addr = sock_htonl(IN_CLASSC_NET);
    return in;
}


#if defined(__WIN32__) && 0

static int inet_getif(inet_descriptor* desc)
{
    char  buf[BUFSIZ];
    int   err;
    LPINTERFACE_INFO ifp;
    int ix;
    int i;
    DWORD len;
    int n;
    struct addr_if* addrv;
    struct sockaddr_in* sinp;
    struct in_addr sina;
    int max_ix;
    char reply[2];

    reply[0] = INET_REP_GETIF;
    reply[1] = desc->sfamily == AF_INET ? INET_AF_INET : INET_AF_INET6;

    addrv = (struct addr_if*) inet_buf;
    max_ix = inet_buf_size / sizeof(struct addr_if);
    ix = 0;

    ifp = (LPINTERFACE_INFO) buf;
    err = WSAIoctl(desc->s, SIO_GET_INTERFACE_LIST, NULL, 0,
		   (LPVOID) ifp, BUFSIZ, (LPDWORD) &len, NULL, NULL);
    if (err == SOCKET_ERROR)
	return inet_error(desc, sock_errno());
    n = (len + sizeof(INTERFACE_INFO) - 1) / sizeof(INTERFACE_INFO);
    while(n-- && (ix < max_ix)) {
	if (ifp->iiAddress.AddressIn.sin_family == desc->sfamily) {
	    sina = ifp->iiAddress.AddressIn.sin_addr;
	    for (i = 0; i < ix; i++) {
		if (sina.s_addr == addrv[i].addr.s_addr)
		    break;
	    }
	    if (i == ix) {
		addrv[ix].addr = sina;
		addrv[ix].mask = ifp->iiNetmask.AddressIn.sin_addr;
		addrv[ix].baddr = ifp->iiBroadcastAddress.AddressIn.sin_addr;
		ix++;
	    }
	}
	ifp++;
    }
    driver_output2(desc->port, reply, 2,
		   inet_buf, ix * sizeof(struct addr_if));
    return 0;
}

#elif defined(SIOCGIFCONF)

/* get all network interface addresses and masks */
static int inet_getif(inet_descriptor* desc)
{
    struct ifconf ifc;
    struct ifreq ifreq;
    struct ifreq *ifr;
    char buf[BUFSIZ];
    char* cp;
    char* cplim;
    int ix;
    int i;
    struct addr_if* addrv;
    struct in_addr ia;
    struct in_addr im;
    struct in_addr bm;
    int max_ix;
    char reply[2];

    reply[0] = INET_REP_GETIF;
    reply[1] = desc->sfamily == AF_INET ? INET_AF_INET : INET_AF_INET6;

    addrv = (struct addr_if*) ((void *)inet_buf);
    max_ix = inet_buf_size / sizeof(struct addr_if);
    ix = 0;

    ifc.ifc_len = sizeof(buf);
    ifc.ifc_buf = buf;
    if (sock_ioctl(desc->s, SIOCGIFCONF, (char *)&ifc) < 0)
	return inet_error(desc, sock_errno());

/* XXX What is this about? How can the size be different? */

#if defined(AF_LINK) && !defined(NO_SA_LEN)
#ifdef max /* VxWorks 5.3 with SENS 1.1... */
#undef max
#endif
#define max(a, b) (a > b ? a : b)
#define size(p)	max((p).sa_len, sizeof(p))
#else
#define size(p) (sizeof (p))
#endif
    cplim = buf + ifc.ifc_len; /* skip over if's with big ifr_addr's */
    for (cp = buf; (cp < cplim) && (ix < max_ix);
	 cp += sizeof(ifr->ifr_name) + size(ifr->ifr_addr)) {
#undef size
	ifr = (struct ifreq *)((void *)cp);
	if (ifr->ifr_addr.sa_family != desc->sfamily)
	    continue;
	ifreq = *ifr;

	if (sock_ioctl(desc->s, SIOCGIFADDR, (char *)&ifreq) < 0)
	    continue;   /* just ignore and continue */

	ia = ((struct sockaddr_in*) ((void *)&ifreq.ifr_addr))->sin_addr;

	/* Remove duplicates */
	for (i = 0; i < ix; i++) {
	    if (addrv[i].addr.s_addr == ia.s_addr)
		break;
	}
	if (i < ix)  /* found */
	    continue;
	addrv[ix].addr = ia; /* got it */

	/* Get netmask */
#ifdef SIOCGIFNETMASK
	if (sock_ioctl(desc->s, SIOCGIFNETMASK, (char *)&ifreq) == 0)
	    im = ((struct sockaddr_in *) 
		  ((void *)&ifreq.ifr_addr))->sin_addr;
	else 
	    im = net_mask(ia);
#else
	im = net_mask(ia);
#endif
	if (ia.s_addr == sock_htonl(INADDR_LOOPBACK))
	    im.s_addr = 0xffffffff;
	else if ((ifreq.ifr_flags & IFF_POINTOPOINT)) {
	    if (sock_ioctl(desc->s, SIOCGIFDSTADDR, (char *)&ifreq) == 0)
		im.s_addr = 0xffffffff;
	}
	addrv[ix].mask = im;

	/* Get broadcast address */
#ifdef SIOCGIFBRDADDR
	if (sock_ioctl(desc->s, SIOCGIFNETMASK, (char *)&ifreq) < 0)
	    bm = ia;   /* no broadcast */
	else
	    bm = ((struct sockaddr_in *)
		  ((void *)&ifreq.ifr_broadaddr))->sin_addr;
#else
	bm = ia;
#endif
	addrv[ix].baddr = bm;
	ix++;
    }
    /* NOTE: We may emit the structures since the data structures
       in addrv are in net work byte order (i.e defined byte order) */
    driver_output2(desc->port, reply, 2, 
		   inet_buf, ix * sizeof(struct addr_if));
    return 0;
}

#else

static int inet_getif(inet_descriptor* desc)
{
    char reply = INET_REP_GETIF;
    driver_output2(desc->port, &reply, 1, NULL, 0);
    return 0;
}

#endif


#ifdef VXWORKS
/*
** THIS is a terrible creature, a bug in the tcp part
** of the old VxWorks stack (non SENS) created a race.
** If (and only if?) a socket got closed from the other
** end and we tried a set/getsockopt on the TCP level,
** the task would generate a bus error...
*/
static STATUS wrap_sockopt(STATUS (*function)() /* Yep, no parameter
						   check */,
			   int s, int level, int optname,
			   char *optval, unsigned int optlen 
			   /* optlen is a pointer if function 
			      is getsockopt... */)
{
    fd_set rs;
    struct timeval timeout;
    int to_read;
    int ret;

    FD_ZERO(&rs);
    FD_SET(s,&rs);
    memset(&timeout,0,sizeof(timeout));
    if (level == IPPROTO_TCP) {
	taskLock();
	if (select(s+1,&rs,NULL,NULL,&timeout)) {
	    if (ioctl(s,FIONREAD,(int)&to_read) == ERROR ||
		to_read == 0) { /* End of file, other end closed? */
		errno = EBADF;
		taskUnlock();
		return ERROR;
	    }
	}
	ret = (*function)(s,level,optname,optval,optlen);
	taskUnlock();
    } else {
	ret = (*function)(s,level,optname,optval,optlen);
    }
    return ret;
}
#endif
    
								


/* set socket options */
static int inet_set_opts(inet_descriptor* desc, char* ptr, int len)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
#ifdef HAVE_MULTICAST_SUPPORT
    struct ip_mreq mreq_val;
#endif
    int ival;
    char* arg_ptr;
    int arg_sz;

    while(len >= 5) {
	opt = *ptr++;
	ival = get_int32(ptr);
	ptr += 4;
	len -= 5;
	arg_ptr = (char*) &ival;
	arg_sz = sizeof(ival);
	proto = SOL_SOCKET;

	switch(opt) {
	case INET_LOPT_HEADER:
	    desc->hsz = ival;
	    continue;
	case INET_LOPT_BUFFER:
	    desc->bufsz = ival;
	    continue;
	case INET_LOPT_ACTIVE:
	    if ((desc->stype == SOCK_STREAM) && IS_CONNECTED(desc) && 
		!desc->active && ival && desc->htype != TCP_PB_RAW /*?*/) {
		tcp_maybe_deliver((tcp_descriptor *)desc);
	    }
	    desc->active = ival;
	    /* Maybe we made one socket active with buffered data */
	    if ( ((desc->stype == SOCK_STREAM) && IS_CONNECTED(desc)) ||
		((desc->stype == SOCK_DGRAM) && IS_OPEN(desc)))
		sock_select(desc, (FD_READ|FD_CLOSE), ival);
	    continue;
	case INET_LOPT_PACKET:
	    desc->htype = ival;
	    continue;
	case INET_OPT_REUSEADDR: 
#ifdef __WIN32__
	    continue;  /* Bjorn says */
#else
	    type = SO_REUSEADDR;
	    DEBUGF(("Descriptor %d: SO_REUSEADDR(%d)\n",desc->s,ival));
	    break;
#endif
	case INET_OPT_KEEPALIVE: type = SO_KEEPALIVE;
	    DEBUGF(("Descriptor %d: SO_KEEPALIVE(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_DONTROUTE: type = SO_DONTROUTE;
	    DEBUGF(("Descriptor %d: SO_DONTROUTE(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_BROADCAST: type = SO_BROADCAST;
	    DEBUGF(("Descriptor %d: SO_BROADCAST(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_OOBINLINE: type = SO_OOBINLINE; 
	    DEBUGF(("Descriptor %d: SO_OOBINLINE(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_SNDBUF:    type = SO_SNDBUF; 
	    DEBUGF(("Descriptor %d: SO_SNDBUF(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_RCVBUF:    type = SO_RCVBUF; 
	    DEBUGF(("Descriptor %d: SO_RCVBUF(%d)\n",desc->s,ival));
	    break;
	case INET_OPT_LINGER:    type = SO_LINGER; 
	    if (len < 4) {
		inet_reply(desc, INET_REP_SETOPTS);
		return 0;
	    }
	    li_val.l_onoff = ival;
	    li_val.l_linger = get_int32(ptr);
	    ptr += 4;
	    len -= 4;
	    arg_ptr = (char*) &li_val;
	    arg_sz = sizeof(li_val);
	    DEBUGF(("Descriptor %d: SO_LINGER(%d,%d)",desc->s,li_val.l_onoff,li_val.l_linger));
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP; 
	    type = TCP_NODELAY; 
	    DEBUGF(("Descriptor %d: [IPPROTO_TCP] TCP_NODELAY(%d)\n",desc->s,ival));
	    break;

#ifdef HAVE_MULTICAST_SUPPORT
        case UDP_OPT_MULTICAST_TTL:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_TTL;
            DEBUGF(("Descriptor %d: [IPPROTO_IP] IP_MULTICAST_TTL(%d)\n",desc->s,ival));
            break;

        case UDP_OPT_MULTICAST_LOOP:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_LOOP;
            DEBUGF(("Descriptor %d:  [IPPROTO_IP] IP_MULTICAST_LOOP(%d)\n",desc->s,ival));
            break;

        case UDP_OPT_MULTICAST_IF:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_IF;
            DEBUGF(("Descriptor %d:  [IPPROTO_IP] IP_MULTICAST_IF(%d)\n",desc->s,ival));
            break;

        case UDP_OPT_ADD_MEMBERSHIP:
            proto = IPPROTO_IP;
            type = IP_ADD_MEMBERSHIP;
            DEBUGF(("Descriptor %d:  [IPPROTO_IP] IP_ADD_MEMBERSHIP(%d)\n",desc->s,ival));
            goto L_set_mreq;
            
        case UDP_OPT_DROP_MEMBERSHIP:
            proto = IPPROTO_IP;
            type = IP_DROP_MEMBERSHIP;
            DEBUGF(("Descriptor %d:  [IPPROTO_IP] IP_DROP_MEMBERSHIP(%d)\n",desc->s,ival));
        L_set_mreq:
            mreq_val.imr_multiaddr.s_addr = ival;
            mreq_val.imr_interface.s_addr = get_int32(ptr);
            ptr += 4;
            len -= 4;
            arg_ptr = (char*)&mreq_val;
            arg_sz = sizeof(mreq_val);
            break;
#endif	/* HAVE_MULTICAST_SUPPORT */

	default:
	    return inet_error(desc, EINVAL);
	}
	if ((type == SO_SNDBUF) || (type == SO_RCVBUF)) {
	    /* make sure we have a buffer that match */
	    if ((ival = inet_set_buffer(desc, ival)) < 0)
		continue;
	}
#ifdef DEBUG
	{ int res =
#endif
	sock_setopt(desc->s, proto, type, arg_ptr, arg_sz);
#ifdef DEBUG
	DEBUGF(("Descriptor %d: setsockopt returned %s\n",desc->s,res));
	}
#endif
    }
    inet_reply(desc, INET_REP_SETOPTS);
    return 0;
}

/* load all option values into the inet_buf and reply */

static int inet_get_opts(inet_descriptor* desc, char* buf, int len)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
    int ival;
    char* arg_ptr;
    int arg_sz;
    char* ptr = inet_buf+1;

    while(len--) {
	opt = *buf++;
	proto = SOL_SOCKET;
	arg_sz = sizeof(ival);
	arg_ptr = (char*) &ival;

	switch(opt) {
	case INET_LOPT_BUFFER:
	    *ptr++ = opt;
	    put_int32(desc->bufsz, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_HEADER:
	    *ptr++ = opt;
	    put_int32(desc->hsz, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_ACTIVE:
	    *ptr++ = opt;
	    put_int32(desc->active, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_PACKET:
	    *ptr++ = opt;
	    put_int32(desc->htype, ptr);
	    ptr += 4;
	    continue;
	case INET_OPT_REUSEADDR: 
	    type = SO_REUSEADDR; 
	    break;
	case INET_OPT_KEEPALIVE: 
	    type = SO_KEEPALIVE; 
	    break;
	case INET_OPT_DONTROUTE: 
	    type = SO_DONTROUTE; 
	    break;
	case INET_OPT_BROADCAST: 
	    type = SO_BROADCAST;
	    break;
	case INET_OPT_OOBINLINE: 
	    type = SO_OOBINLINE; 
	    break;
	case INET_OPT_SNDBUF:    
	    type = SO_SNDBUF; 
	    break;
	case INET_OPT_RCVBUF:    
	    type = SO_RCVBUF; 
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP; 
	    type = TCP_NODELAY; 
	    break;
#ifdef HAVE_MULTICAST_SUPPORT
        case UDP_OPT_MULTICAST_TTL:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_TTL;
            break;
        case UDP_OPT_MULTICAST_LOOP:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_LOOP;
            break;
        case UDP_OPT_MULTICAST_IF:
            proto = IPPROTO_IP;
            type = IP_MULTICAST_IF;
            break;
#endif	/* HAVE_MULTICAST_SUPPORT */
	case INET_OPT_LINGER:
	    arg_sz = sizeof(li_val);
	    arg_ptr = (char*) &li_val;	    
	    type = SO_LINGER; 
	    break;
	default:
	    return inet_error(desc, EINVAL);
	}
	if (sock_getopt(desc->s,proto,type,arg_ptr,&arg_sz) == SOCKET_ERROR)
	    continue;
	*ptr++ = opt;
	if (arg_ptr == (char*)&ival) {
	    put_int32(ival, ptr);
	    ptr += 4; 
	}
	else {
	    put_int32(li_val.l_onoff, ptr);
	    ptr += 4; 
	    put_int32(li_val.l_linger, ptr);
	    ptr += 4; 
	}
    }
    inet_buf[0] = INET_REP_GETOPTS;
    len = ptr - inet_buf;
    driver_output2(desc->port, inet_buf, len, NULL, 0);
    return 0;
}


/* fill statistics reply, op codes from src and result in dest
** dst area must be a least 5*len + 1 bytes
*/
static int inet_fill_stat(inet_descriptor* desc, char* src, int len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_GETSTAT;     /* put reply code */
    while (len--) { 
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_STAT_RECV_CNT:  val = desc->recv_cnt;  break;
	case INET_STAT_RECV_MAX:  val = desc->recv_max;  break;
	case INET_STAT_RECV_AVG:  
	    val = (unsigned long) desc->recv_avg;  
	    break;
	case INET_STAT_RECV_DVI:  
	    val = (unsigned long ) fabs(desc->recv_dvi); 
	    break;
	case INET_STAT_SEND_CNT:  val = desc->send_cnt; break;
	case INET_STAT_SEND_MAX:  val = desc->send_max; break;
	case INET_STAT_SEND_AVG:  
	    val = (unsigned long) desc->send_avg; 
	    break;
	case INET_STAT_SEND_PND:  val = driver_sizeq(desc->port); break;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}


/* socket statitics */
static int inet_get_stat(inet_descriptor* desc, char* buf, int len)
{
    int outlen = len*5 + 1;   /* number of bytes in reply 5 bytes/reply */
    int n;

    /* ensure we fit in inet_buf */
    if (outlen > INET_MAX_BUFFER)
	return inet_error(desc, EINVAL);
    else if (outlen > inet_buf_size) {
	if (inet_set_buffer(desc, outlen) < 0)
	    return inet_error(desc, ENOMEM);
    }
    if ((n = inet_fill_stat(desc, buf, len, inet_buf)) < 0)
	return inet_error(desc, EINVAL);

    driver_output2(desc->port, inet_buf, n, NULL, 0);
    return 0;
}


#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

static int inet_hostname(inet_descriptor* desc)
{
    char buf[MAXHOSTNAMELEN+1];
    int len;

    if (sock_hostname(buf+1, MAXHOSTNAMELEN) == SOCKET_ERROR)
	return inet_error(desc, sock_errno());
    buf[0] = INET_REP_GETHOSTNAME;
    len = strlen(buf+1);
    driver_output2(desc->port, buf, len+1, NULL, 0);
    return 0;
}


/* Terminate socket */
static void inet_stop(inet_descriptor* desc)
{
    inet_close(desc);
    inet_desc_table[desc->ix] = NULL;  /* detach from table */
    sys_free(desc);
}


/* Allocate descriptor */
static inet_descriptor* inet_start(long port, int size)
{
    inet_descriptor* desc;
    int ix = inet_desc_ix;
    int save_ix = ix;

    do {
	if (inet_desc_table[ix] == NULL)
	    break;
	ix = (ix + 1) % inet_desc_size;
    } while(ix != save_ix);

    if (inet_desc_table[ix] != NULL) {
	DEBUGF(("ran out of inet descriptors max = %d\r\n", inet_desc_size));
	return NULL;
    }
    inet_desc_ix = (ix + 1) % inet_desc_size;

    if ((desc = (inet_descriptor*) ALLOC(size)) == NULL)
	return NULL;

    desc->s = INVALID_SOCKET;
    desc->event = INVALID_EVENT;
    desc->event_mask = 0;
    desc->port = port;
    desc->state = INET_STATE_CLOSED;
    desc->prebound = 0;
    desc->ix = ix;
    desc->bufsz = INET_MIN_BUFFER; 
    desc->hsz = 0;                 /* list header size */
    desc->htype = TCP_PB_RAW;      /* default packet type */
    desc->stype = -1;              /* bad stype */
    desc->sfamily = -1;
    desc->active = 0;              /* start inactive */

    desc->recv_cnt = 0;
    desc->recv_max = 0;    
    desc->recv_avg = 0.0;
    desc->recv_dvi = 0.0;
    desc->send_cnt = 0;
    desc->send_max = 0;
    desc->send_avg = 0.0;

    sys_memzero((char *)&desc->remote,sizeof(desc->remote));

    inet_desc_table[ix] = desc;

    return desc;
}

static int inet_command(inet_descriptor* desc, char* buf, int len)
{
    char* ptr = buf;
    
    len--;
    switch(*ptr++) {
    case INET_REQ_CLOSE:
	inet_close(desc);
	inet_reply(desc, INET_REP_CLOSE);
	DEBUGF(("driver_failure_eof(%d) in %s, line %d\n",
		desc->port, __FILE__, __LINE__));
	driver_failure_eof(desc->port);
	return 0;

    case INET_REQ_IX:
	return inet_ix(desc);

    case INET_REQ_NAME:
	return inet_local_name(desc);

    case INET_REQ_PEER:
	return inet_peer_name(desc);

    case INET_REQ_BIND:
	if (len < 2)
	    return inet_error(desc, EINVAL);
	return inet_bind(desc, ptr, len);

    case INET_REQ_SETOPTS:
	if (!(desc->state & INET_F_OPEN))
	    return inet_error(desc, ENOTSOCK);
	return inet_set_opts(desc, ptr, len);

    case INET_REQ_GETOPTS:
	if (!(desc->state & INET_F_OPEN))
	    return inet_error(desc, ENOTSOCK);
	return inet_get_opts(desc, ptr, len);

    case INET_REQ_GETIF:
	if (!(desc->state & INET_F_OPEN))
	    return inet_error(desc, ENOTSOCK);
	return inet_getif(desc);

    case INET_REQ_GETSTAT: /* get statitics */
	return inet_get_stat(desc, ptr, len);

    case INET_REQ_GETHOSTNAME: /* get host name */
	if (len != 0)
	    return inet_error(desc, EINVAL);
	return inet_hostname(desc);

    default:
	/*
	 * Ignore anything else -- let the caller hang.
	 */
	return 0;
    }
}

/* common tcp/udp control command
** only GETSTAT MUST be executed directly without wait on busy port,
** else the tick oprtation in distribution cant not tell if there is
** pending data on output. Since if port may be busy the answer will probably
** be that there is never any data pending.
*/
static int inet_ctl(inet_descriptor* desc, int cmd, char* buf, int len, 
		    char** res_buf, int res_size)
{
    switch (cmd) {
    case INET_REQ_GETSTAT: {  /* only option avail now */
	  char* dst;
	  int dstlen = len*5 + 1;  /* resulting length */

	  if (dstlen > INET_MAX_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > res_size) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *res_buf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *res_buf;  /* ok we fit in buffer given */
	  return inet_fill_stat(desc, buf, len, dst);
      }
    default:
	return 0;
    }
}

/* update statistics on output packets */
static void inet_output_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->send_cnt + 1;
    double avg = desc->send_avg;

    if (n == 0) /* WRAP, use old avg as input to a new sequence */
	n = 1;
    desc->send_avg += (len - avg) / n;
    if ((unsigned int) len > desc->send_max)
	desc->send_max = len;
    desc->send_cnt = n;
}

/* update statistics on input packets */
static void inet_input_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->recv_cnt + 1;
    double avg = desc->recv_avg;
    double dvi;

    if (n == 0) /* WRAP */
	n = 1;

    /* average packet length */
    avg = avg + (len - avg) / n;
    desc->recv_avg = avg;

    if ((unsigned int) len > desc->recv_max)
	desc->recv_max = len;

    /* average deviation from average packet length */
    dvi = desc->recv_dvi;
    desc->recv_dvi = dvi + ((len - avg) - dvi) / n;
    desc->recv_cnt = n;
}

/*----------------------------------------------------------------------------

   TCP

-----------------------------------------------------------------------------*/

/* clear CURRENT input buffer */
static void tcp_clear_input(tcp_descriptor* desc)
{
    if (desc->i_buffer != NULL)
	sys_free(desc->i_buffer);
    desc->h_len = 0;
    desc->i_buffer = NULL;
    desc->i_remain = 0;
    desc->i_length = 0;
}

static int tcp_inet_init(void)
{
    return 0;
}


/* initialize the tcp descriptor */

static long tcp_inet_start(long port, char *args)
{
    tcp_descriptor* desc;

    desc = (tcp_descriptor*)inet_start(port, sizeof(tcp_descriptor));
    if (desc == NULL)
	return -1;

    desc->h_len = 0;
    desc->i_ix = -1;
    desc->i_length = 0;
    desc->i_buffer = NULL;
    desc->i_ptr = NULL;
    desc->i_remain = 0;
    return (long) desc;
}


/*
** Given a accepting port, restore the state on the listener port
*/
static void tcp_clear_listener(tcp_descriptor* a_desc)
{
    int i;
    int ix = a_desc->inet.ix;
    tcp_descriptor* l_desc;

    ASSERT(a_desc->inet.state ==  TCP_STATE_ACCEPTING);

    for (i = 0; i < inet_desc_size; i++) {
	inet_descriptor* inp = inet_desc_table[i];
	if ((inp != NULL) && (inp->stype == SOCK_STREAM)) {
	    l_desc = (tcp_descriptor*) inp;
	    if (l_desc->i_ix == ix) {
		if (l_desc->inet.state == TCP_STATE_LISTENING) {
		    l_desc->inet.state = TCP_STATE_LISTEN;
		    sock_select(INETP(l_desc), FD_ACCEPT, 0);
		}
		l_desc->i_ix = -1;
		return;
	    }
	}
    }
}

/*
** Check Speical cases:
** 1. we are a listener doing nb accept -> report error on accept !
** 2. we are doing accept -> restore listener state
*/
static void tcp_close_check(tcp_descriptor* desc)
{
    int ix;
    tcp_descriptor* a_desc;


    if (desc->inet.state == TCP_STATE_LISTENING) {
	if (((ix = desc->i_ix) != -1) && 
	    ((a_desc = (tcp_descriptor*) inet_desc_table[ix]) != NULL) &&
	    (a_desc->inet.state == TCP_STATE_ACCEPTING)) {
	    DEBUGF(("driver_failure_eof(%d) in %s, line %d\n",
		    a_desc->inet.port, __FILE__, __LINE__));
	    driver_failure_posix(a_desc->inet.port, EINTR ); /* the accept
								is interrupted */
	}
    }
    else if (desc->inet.state == TCP_STATE_ACCEPTING) {
	tcp_clear_listener(desc);
    }
}

/*
**
**
*/
static int tcp_inet_stop(tcp_descriptor* desc)
{
    tcp_close_check(desc);
    /* free input buffer & output buffer */
    if (desc->i_buffer != NULL)
	sys_free(desc->i_buffer);
    inet_stop(&desc->inet);
    return 0;
}


static int tcp_inet_ctl(tcp_descriptor* desc, int cmd, char* buf, int len, 
			char** res_buf, int res_size)
{
    return inet_ctl(INETP(desc), cmd, buf, len, res_buf, res_size);
}

/*
** tcp_inet_timeout:
** called when timer expire:
** tcp socket may be:
**
** a)  receiving   -- deseclect
** b)  connecting  -- close socket
** c)  accepting   -- reset listener
**
*/

static int tcp_inet_timeout(tcp_descriptor* desc)
{
    int state = desc->inet.state;

    if ((state & TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {
	/* assume recv timeout */
	ASSERT(!desc->inet.active);
	sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
    }
    else if ((state & TCP_STATE_CONNECTING) == TCP_STATE_CONNECTING) {
	/* assume connect timeout */
	/* close the socket since it's not usable (see man pages) */
	inet_close(INETP(desc));
    }
    else if ((state & TCP_STATE_ACCEPTING) == TCP_STATE_ACCEPTING) {
	/* assume accept timeout */
	tcp_clear_listener(desc);
    }
    return inet_report(INETP(desc), "timeout");
}

/* input: REQ ARG(S) */
/* Handle close on listen socket doing accept !!!??? */
/* Add commandv later !!! */

static int tcp_inet_command(tcp_descriptor* desc, char *buf, int len)
{
    int             ix;
    int             code;
    int             backlog;
    unsigned long   timeout;
    tcp_descriptor* l_desc;
    char*           ptr = buf;
    SOCKET s;

    if (len < 1)
	return 0; /* let caller hang */
    len--;

    switch (*ptr++) {
    case TCP_REQ_OPEN:   /* open socket and return internal index */
	if ((len == 1) && (*ptr == INET_AF_INET))
	    return inet_open(INETP(desc), AF_INET, SOCK_STREAM);
#if defined(HAVE_IN6) && defined(AF_INET6)
	else if ((len == 1) && (*ptr == INET_AF_INET6))
	    return inet_open(INETP(desc), AF_INET6, SOCK_STREAM);
#endif
	else
	    return inet_error(INETP(desc), EINVAL);

    case TCP_REQ_FDOPEN:   /* pass in an open (and bound) socket */
	if ((len == 5) && (*ptr == INET_AF_INET))
	    return inet_fdopen(INETP(desc), AF_INET, SOCK_STREAM,
			       (SOCKET) get_int32(ptr+1));
#if defined(HAVE_IN6) && defined(AF_INET6)
	else if ((len == 5) && (*ptr == INET_AF_INET6))
	    return inet_fdopen(INETP(desc), AF_INET6, SOCK_STREAM,
			       (SOCKET) get_int32(ptr+1));
#endif
	else
	    return inet_error(INETP(desc), EINVAL);

    case TCP_REQ_CLOSE:
	tcp_close_check(desc);
	inet_close(INETP(desc));
	inet_reply(INETP(desc), TCP_REP_CLOSE);
	DEBUGF(("driver_failure_eof(%d) in %s, line %d\n",
		desc->inet.port, __FILE__, __LINE__));
	driver_failure_eof(desc->inet.port);
	return 0;

    case TCP_REQ_CONNECT:  /* non-blocking connect */
	/* INPUT: Timeout(4), Port(2), Address(N) */
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), ENOTSOCK);
	if (IS_CONNECTED(INETP(desc)))
	    return inet_error(INETP(desc), EISCONN);
	if (!IS_BOUND(INETP(desc)))
	    return inet_error(INETP(desc), EADDRINUSE);
	if (len < 6)
	    return inet_error(INETP(desc), EINVAL);
	timeout = get_int32(ptr);
	ptr += 4;
	if (inet_set_address(desc->inet.sfamily, &desc->inet.remote,
			     ptr, &len) == NULL)
	    return inet_error(INETP(desc), EINVAL);
	sock_select(INETP(desc), FD_CONNECT, 1);
	code = sock_connect(desc->inet.s, 
			    (struct sockaddr*) &desc->inet.remote, len);
	if ((code == SOCKET_ERROR) && 
		((sock_errno() == ERRNO_BLOCK) ||  /* Winsock2 */
		 (sock_errno() == EINPROGRESS))) {	/* Unix !! */
	    desc->inet.state = TCP_STATE_CONNECTING;
	    if (timeout != INET_INFINITY)
		driver_set_timer(desc->inet.port, timeout);
	}
	else if (code == 0) { /* ok we are connected */
	    sock_select(INETP(desc), FD_CONNECT, 0);
	    desc->inet.state = TCP_STATE_CONNECTED;
	    if (desc->inet.active)
		sock_select(INETP(desc), (FD_READ|FD_CLOSE), 1);
	    inet_reply(INETP(desc), TCP_REP_CONNECT);
	}
	else {
	    sock_select(INETP(desc), FD_CONNECT, 0);
	    return inet_error(INETP(desc), sock_errno());
	}
	return 0;

    case TCP_REQ_LISTEN:  /* argument backlog */
	if (desc->inet.state == TCP_STATE_CLOSED)
	    return inet_error(INETP(desc), ENOTSOCK);
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), EINVAL);
	if (!IS_BOUND(INETP(desc)))
	    return inet_error(INETP(desc), EADDRINUSE);
	if (len != 2)
	    return inet_error(INETP(desc), EINVAL);
	backlog = get_int16(ptr);
	if (sock_listen(desc->inet.s, backlog) == SOCKET_ERROR)
	    return inet_error(INETP(desc), sock_errno());
	desc->inet.state = TCP_STATE_LISTEN;
	inet_reply(INETP(desc), TCP_REP_LISTEN);
	return 0;

    case TCP_REQ_ACCEPT: 
	/* INPUT: Timeout(4), ListenIndex(2) */
	if (desc->inet.state != TCP_STATE_CLOSED)
	    return inet_error(INETP(desc), EADDRINUSE);
	if (len != 6)
	    return inet_error(INETP(desc), EINVAL);
	timeout = get_int32(ptr);
	ptr += 4;
	ix = get_int16(ptr);
	if ((ix >= inet_desc_size) || 
	    ((l_desc = (tcp_descriptor*)inet_desc_table[ix]) == NULL))
	    return inet_error(INETP(desc), EINVAL);
	if (l_desc->inet.state != TCP_STATE_LISTEN)
	    return inet_error(INETP(desc), EINVAL);
	/* Some flags must be inherited at this point */
	desc->inet.active = l_desc->inet.active;
	desc->inet.htype = l_desc->inet.htype; 
	desc->inet.hsz = l_desc->inet.hsz;
	desc->inet.stype = l_desc->inet.stype;
	desc->inet.sfamily = l_desc->inet.sfamily;

	len = sizeof(desc->inet.remote);
	s = sock_accept(l_desc->inet.s, 
			(struct sockaddr*) &desc->inet.remote, &len);
	if (s == INVALID_SOCKET) {
	    if (sock_errno() == ERRNO_BLOCK) {
		desc->inet.state = TCP_STATE_ACCEPTING;
		l_desc->inet.state = TCP_STATE_LISTENING;
		l_desc->i_ix = desc->inet.ix;
		sock_select(INETP(l_desc),FD_ACCEPT,1);
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout);
	    }
	    else
		return inet_error(INETP(desc), sock_errno());
	}
	else {
	    desc->inet.s = s;
	    if ((desc->inet.event = sock_create_event(INETP(desc))) == 
		INVALID_EVENT)
		return inet_error(INETP(desc), sock_errno());
	    SET_NONBLOCKING(desc->inet.s);
#ifdef __WIN32__
	driver_select(desc->inet.port, desc->inet.event, DO_READ, 1);
#endif
	    desc->inet.state = TCP_STATE_CONNECTED;
	    /* A sock_select will be activated if active == true by the
	       INET_REQ_SETOPTS that will immediately follow this
	       */
	    inet_reply(INETP(desc), TCP_REP_ACCEPT);
	}
	return 0;

    case TCP_REQ_ASEND:  /* args: buffer async send is used by distribution */
	if (!IS_CONNECTED(INETP(desc)))
	    return inet_error(INETP(desc), ENOTCONN);
	tcp_send(desc, ptr, len);
	return 0;

    case TCP_REQ_SEND:  /* args: buffer */
	if (!IS_CONNECTED(INETP(desc)))
	    return inet_error(INETP(desc), ENOTCONN);
	if (tcp_send(desc, ptr, len) == 0)
	    inet_reply(INETP(desc), TCP_REP_SEND);
	return 0;

    case TCP_REQ_RECV:
	/* INPUT: Timeout(4),  Length(4) */
	if (!IS_CONNECTED(INETP(desc)))
	    return inet_error(INETP(desc), ENOTCONN);
	if (desc->inet.active || (len != 8))
	    return inet_error(INETP(desc), EINVAL);
	timeout = get_int32(ptr);
	ptr += 4;
	len = get_int32(ptr);
	if ((desc->inet.htype != TCP_PB_RAW) && (len != 0))
	    return inet_error(INETP(desc), EINVAL);
	if (len > TCP_MAX_PACKET_SIZE)
	    return inet_error(INETP(desc), ENOMEM);
	return tcp_recv_passive(desc, len, timeout);

    default:
	return inet_command(INETP(desc), ptr - 1, len+1);
    }
}

/*
** check if we have info enough to determine the length
** of the asn1 packet:
** return length of the header (and total length int plen)
** return -1 otheriwse
**
*/
static int asn1_header(char* ptr, int len, int* plen)
{
    char* qtr = ptr;
    int length;
    int n = len;

    if (n < 2)
	return -1;

    n--;
    if ((*ptr++ & 0x1f) == 0x1f) { /* Long tag format */
	while(n && ((*ptr & 0x80) == 0x80)) {
	    ptr++; 
	    n--;
	}
	if (n < 2)
	    return -1;
	ptr++;
	n--;
    }

    /* ptr now point to length field and n characters remain */
    length = *ptr & 0x7f;
    if ((*ptr & 0x80) == 0x80) {   /* Long length format */
	ptr++;
	n--;
	if (length > 4) return -2; /* way too long */
	if (n < length) return -1; /* need more chars*/
	switch(length) {
	case 0: return -2;
	case 1: *plen = get_int8(ptr); ptr++; break;
	case 2: *plen = get_int16(ptr); ptr += 2; break;
	case 3: *plen = get_int24(ptr); ptr += 3; break;
	case 4: *plen = get_int32(ptr); ptr += 4; break;
	}
    }
    else {
	ptr++;
	*plen = length;
    }
    return (ptr - qtr);
}

/*
** check if we have a CDR message header
**
**    struct MessageHeader {
**        char magic[4];               // 4 bytes must be 'GIOP'
**        Version GIOP_version;        // 2 bytes major minor
**        boolean byte_order;          // 1 byte (0 == big, 1 == little)
**        octet message_type;          // 1 byte message type
**        unsigned long message_size;  // 4 byte (in byte_order ordering)
**    }
*/
static int cdr_header(char* ptr, int len, int* plen)
{
    if (len < 4)
	return -1;
    if (sys_memcmp(ptr, "GIOP", 4) != 0) {
	*plen = len;
	return 0;
    }
    if (len < 12) 
	return -1;
    if (ptr[6])
	*plen = get_little_int32(ptr+8);
    else
	*plen = get_int32(ptr+8);
    return 12;
}

/*
** The fcgi header is 8 bytes. After that comes the data and
** possibly some padding.
** return length of the header (and total length int plen)
** return -1 when not enough bytes
** return -2 when error
*/

struct fcgi_head {
  unsigned char version;
  unsigned char type;
  unsigned char requestIdB1;
  unsigned char requestIdB0;
  unsigned char contentLengthB1;
  unsigned char contentLengthB0;
  unsigned char paddingLength;
  unsigned char reserved;
};

#define FCGI_VERSION_1		1

static int fcgi_header(char* ptr, int len, int* plen)
{
  struct fcgi_head* hp;

  if (len < sizeof(struct fcgi_head))
    return -1;			/* Not enough data for reading header */

  hp = (struct fcgi_head*)((void *)ptr);

  if (hp->version != FCGI_VERSION_1)
    return -2;			/* ERROR, unknown header version */

  *plen = ((hp->contentLengthB1 << 8) | hp->contentLengthB0)
    + hp->paddingLength;

  return sizeof(struct fcgi_head);
}

/*
** Send back all of it minus the padding characters.
** This mean that the 8 bit header is sent before the data
** so that the erlang part of the FCGI package can read the
** type and request id.
*/

static void fcgi_reply_data(inet_descriptor* desc, char* buf, int len)
{
  struct fcgi_head* hp = (struct fcgi_head*)((void *)buf);

  inet_reply_data(desc, buf, len - hp->paddingLength);
}

/*
** calculate the header length and total length
** return -1 when not enough bytes
** return -2 when error
*/
static int tcp_header(tcp_descriptor* desc, char* ptr, int len, int* tlen)
{
    int p_length;
    int h_length;

    switch(desc->inet.htype) {
    case TCP_PB_RAW:
	if (len <= 0) return -1;
	p_length = len;
	h_length = 0;
	break;
    case TCP_PB_1:
	if (len < 1) return -1;
	p_length = get_int8(ptr);
	h_length = 1;
	break;
    case TCP_PB_2:
	if (len < 2) return -1;
	p_length = get_int16(ptr);
	h_length = 2;
	break;
    case TCP_PB_4:
	if (len < 4) return -1;
	p_length = get_int32(ptr);
	h_length = 4;
	break;
    case TCP_PB_RM:
	if (len < 4) return -1;
	p_length = get_int32(ptr) & 0x7fffffff;
	h_length = 4;
	break;
    case TCP_PB_ASN1:
	if ((h_length = asn1_header(ptr, len, &p_length)) < 0)
	    return h_length;
	break;
    case TCP_PB_CDR:
	if ((h_length = cdr_header(ptr, len, &p_length)) < 0)
	    return h_length;
	break;
    case TCP_PB_FCGI:
	if ((h_length = fcgi_header(ptr, len, &p_length)) < 0)
	    return h_length;
	break;
    default:
	DEBUGF(("Error: desc->inet.htype of unknown value 0x%08d "
		"in tcp_header()!\n", desc->inet.htype));
	return -1;
    }
    /* sanity check (we do not want to fill entire system) */
    if (p_length >= TCP_MAX_PACKET_SIZE || p_length < 0)
	return -2;
    *tlen = p_length + h_length;
    return h_length;
}

/* Deliver the packet
** ptr : pointer to the start of packet including header
** len : length of packet including header 
** clear event after send.
*/
static int tcp_recv_deliver(tcp_descriptor* desc, char* ptr, int len)
{
    inet_input_count(INETP(desc), len);
    switch(desc->inet.htype) {
    case TCP_PB_1:
	DEBUGF(("port %d: Delivering %d bytes\n", desc->inet.port, len-1));
	inet_reply_data(INETP(desc), ptr+1, len-1);
	break;
    case TCP_PB_2:
	inet_reply_data(INETP(desc), ptr+2, len-2);
	break;
    case TCP_PB_4:
	inet_reply_data(INETP(desc), ptr+4, len-4);
	break;
    case TCP_PB_RAW:
    case TCP_PB_RM:
    case TCP_PB_ASN1:
    case TCP_PB_CDR:
	DEBUGF(("port %d: Delivering %d bytes\n", desc->inet.port, len));
	inet_reply_data(INETP(desc), ptr, len);
	break;
    case TCP_PB_FCGI:
	fcgi_reply_data(INETP(desc), ptr, len);
	break;
    default:
	ASSERT(0);
    }
    return 0;
}


/* The socket has closed, cleanup and send event */
static int tcp_recv_closed(tcp_descriptor* desc)
{
    inet_close(INETP(desc));
    tcp_clear_input(desc);

    if (desc->inet.active) {
	DEBUGF(("driver_failure_eof(%d) in %s, line %d\n",
		desc->inet.port, __FILE__, __LINE__));
	driver_failure_eof(desc->inet.port);
    }
    else {
	/* We must cancel any timer here ! */
	driver_cancel_timer(desc->inet.port);	
	/* passive mode do not terminate port ! */
	inet_report(INETP(desc), "closed");
	DEBUGF(("closed(%d) in %s, line %d\n",
		desc->inet.port, __FILE__, __LINE__));
	/* next time ENOTSOCK will be delivered  */
    }
    return -1;
}


/* We have a read error determine the action */
static int tcp_recv_error(tcp_descriptor* desc, int err)
{
    if (err != ERRNO_BLOCK) {
	if (!desc->inet.active) {
	    /* We must cancel any timer here ! */
	    driver_cancel_timer(desc->inet.port);	
	    inet_error(INETP(desc), err);  /* emit error string */
	}
	tcp_clear_input(desc); 
	driver_failure_posix(desc->inet.port, err);
	return -1;
    }
    return 0;
}

/*
** Deliver complete packets from i_buffer
** Return number of packets delivered
**
*/

static int tcp_deliver_packets(tcp_descriptor* desc, char* ptr, int len)
{
    int n;
    int t_len;
    int count = 0;
    char* nbuf;
    char* obuf = desc->i_buffer;

    DEBUGF(("port %d: about to deliver read data...\n", desc->inet.port));
    ASSERT(len != 0);
    while ((n = tcp_header(desc, ptr, len, &t_len)) >= 0) {
	if (len < t_len) {	/* Not a complete packet. */
	    DEBUGF(("  ... saving rest\n"));
	    goto save_rest;
	} else {		/* At least one complete packet. */
	    tcp_recv_deliver(desc, ptr, t_len);
	    ptr += t_len;
	    len -= t_len;
	    DEBUGF(("  ... delivered packet of %d bytes (%d bytes left)\n",
		    t_len, len));
	    count++;
	    if (!desc->inet.active) {
		if (len == 0) {
		    break;
		}
		t_len = len;
		DEBUGF(("  ... saving rest (because passive port)\n"));
		goto save_rest;
	    }
	}
    }
    if (n == -2) {
	DEBUGF(("port %d: huge message discarded\n", desc->inet.port));
	return tcp_recv_error(desc, ENOMEM);
    }
    if (len > TCP_MAX_HDR)
	return tcp_recv_error(desc, EINVAL);
    DEBUGF(("  ... saving (incomplete header)\n"));
    sys_memcpy(desc->h_store, ptr, len);
    desc->h_len = len;
    desc->i_ptr = desc->i_buffer = NULL;
    desc->i_remain = desc->i_length = 0;
    if (obuf != NULL)
	sys_free(obuf);
    return count;

 save_rest:
    if ((nbuf = ALLOC(t_len+TCP_BYTE_GAP)) == NULL) {
	driver_failure_posix(desc->inet.port, ENOMEM);
	return -1;
    }
    sys_memcpy(nbuf+TCP_BYTE_GAP, ptr, len);
    desc->i_ptr = nbuf+TCP_BYTE_GAP+len;
    desc->i_length = t_len;
    desc->i_remain = t_len - len;
    desc->i_buffer = nbuf;
    if (obuf != NULL)
	sys_free(obuf);
    return count;
}


/*
** Passive recv (read length)
** We may have pending data that becomes ready:
** example:
** {packet N}, recv(S, 0, T1), {packet,0}, recv(S,Len,T2)
*/

static int tcp_recv_passive(tcp_descriptor* desc, int qlen, 
			    unsigned long timeout)
{
    int code;
    char* ptr;
    char* buf;
    int len;
    int hlen;

    DEBUGF(("port %d: tcp_recv_passive...\n", desc->inet.port));
    if (desc->i_buffer == NULL) {
	/* nothing in buffer but we may have header bytes ! */
	if (qlen != 0) {
	    ASSERT(desc->inet.htype == TCP_PB_RAW);
	    hlen = desc->h_len;
	    len = qlen + hlen + TCP_BYTE_GAP;
	    if ((ptr = ALLOC(len)) == NULL) {
		driver_failure_posix(desc->inet.port, ENOMEM);
		return -1;
	    }
	    desc->i_buffer = ptr;
	    ptr += TCP_BYTE_GAP;
	    /* insert stored header bytes */
	    if (hlen > 0) {
		sys_memcpy(ptr, desc->h_store, hlen);
		ptr += hlen;
	    }
	    desc->i_ptr = ptr;
	    desc->i_length = qlen + hlen; /* total length */
	    desc->i_remain = qlen;        /* remains */
	    desc->h_len = 0;              /* clear since in buffer */
	}
    }
    else {
	/* we have something in buffer */
	ASSERT(desc->h_len == 0);
	ptr = desc->i_buffer + TCP_BYTE_GAP;
	len = desc->i_length - desc->i_remain;

	if (qlen != 0) {  /* only in raw mode */
	    ASSERT(desc->inet.htype == TCP_PB_RAW);
	    if (qlen <= len) {
		tcp_recv_deliver(desc, ptr, qlen);
		len -= qlen;  /* remaining data */
		if (len == 0)
		    tcp_clear_input(desc);
		else {
		    /* move and shrink buffer */
		    sys_memmove(ptr, ptr+qlen, len);
		    buf = REALLOC(desc->i_buffer, len+TCP_BYTE_GAP);
		    ASSERT(buf != NULL);  /* should never occure !! */
		    desc->i_buffer = buf;
		    desc->i_length = len;
		    desc->i_remain = 0;
		    desc->i_ptr = buf + len+TCP_BYTE_GAP;
		}
		return 0;
	    }
	    else {
		/* we have more to read */
		if (qlen > desc->i_length) {
		    /* must grow buffer */
		    buf = REALLOC(desc->i_buffer, qlen+TCP_BYTE_GAP);
		    if (buf == NULL) {
			driver_failure_posix(desc->inet.port, ENOMEM);
			return -1;
		    }
		    desc->i_buffer = buf;
		    desc->i_ptr = buf + len + TCP_BYTE_GAP;
		    desc->i_remain = qlen - desc->i_remain;
		}
		desc->i_length = qlen;
	    }
	}
	else {
	    code = tcp_deliver_packets(desc, ptr, len);
	    if (code > 0)
		return 0;
	    else if (code < 0)
		return -1;
	}
    }
    if (timeout != INET_INFINITY)
	driver_set_timer(desc->inet.port, timeout);
    sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
    return tcp_recv(desc);
}

static int tcp_maybe_deliver(tcp_descriptor* desc)
{
    int code;
    char* ptr;
    int len;
	int delivered = 0;

    if (desc->i_buffer == NULL) {
	return -1;
    }
    else while (desc->i_buffer != NULL) {
	ptr = desc->i_buffer + TCP_BYTE_GAP;
	len = desc->i_length - desc->i_remain;
	code = tcp_deliver_packets(desc, ptr, len);
	DEBUGF2(("port %d: tcp_maybe_deliver... len = %d(%d) code = %d\n", desc->inet.port, len, desc->i_length - desc->i_remain, code));
	if (code < 0)
	    return code;
	else if (code == 0)
	    break;
	delivered += code;
    }
    return delivered;
}

/*
** tcp_recv:
**
** when i_buffer == NULL : start reading
**    h_len: length of header
**  h_store: store of partial header bytes
**
** when i_buffer != NULL : continue reading
**    h_len: length of header
**    i_length: total length of packet including header
**    i_remain: remaining length to read
**    i_ptr: position for read
**
*/
static int tcp_recv(tcp_descriptor* desc)
{
    char* buf;
    int code;
    int len;

    /* Setup the buffer to receive in and the length */
    if (desc->i_buffer != NULL) {
	buf = desc->i_ptr;
	len = desc->i_remain;
	ASSERT(len != 0);
    }
    else {
	buf = inet_buf+TCP_BYTE_GAP;
	if (desc->h_len > 0) {
	    sys_memcpy(buf, desc->h_store, desc->h_len);
	    buf += desc->h_len;
	}
	len = TCP_MAX_RECV;
    }

    /* Receive and check for errors */
    DEBUGF(("port %d: about to read %d bytes...\n", desc->inet.port, len));
    if ((code = sock_recv(desc->inet.s, buf, len, 0)) == SOCKET_ERROR) {
	DEBUGF(("  ... error: %d\n", sock_errno()));
        if (sock_errno() == ECONNRESET) {
	    DEBUGF2(("port %d: detected close in %s, line %d\n",
		    desc->inet.port, __FILE__, __LINE__));
	    return tcp_recv_closed(desc);
	}
        else
	    return tcp_recv_error(desc, sock_errno());
    }
    else if (code == 0) {
	DEBUGF2(("  ... detected close in %s, line %d, len %d\n", __FILE__, __LINE__, len));
	return tcp_recv_closed(desc);
    }

    if (desc->i_buffer != NULL) {
	DEBUGF(("  code = %d (into existing buffer)\n", code));
	/* Complete or Partial packets */
	if (len == code) {
	    /* Complete packet */
	    tcp_recv_deliver(desc, desc->i_buffer+TCP_BYTE_GAP,desc->i_length);
	    sys_free(desc->i_buffer);
	    desc->i_buffer = NULL;
	    if (!desc->inet.active) {
		driver_cancel_timer(desc->inet.port);
		sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    }
	}
	else {
	    desc->i_ptr = buf + code;
	    desc->i_remain = len - code;
	}
    }
    else {
	DEBUGF(("  code = %d\n", code));
	len = code + desc->h_len;  /* length of data in buffer */
	buf -= desc->h_len;        /* start of data including header */
	desc->h_len = 0;
	code = tcp_deliver_packets(desc, buf, len);
	if (code > 0) {
	    if (!desc->inet.active) {
		driver_cancel_timer(desc->inet.port);
		sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    }
	    return 0;
	}
	return code;
    }
    return 0;
}

#ifdef __WIN32__

static int winsock_event_select(inet_descriptor *desc, int flags, int on)
{
    int save_event_mask = desc->event_mask;
    if (on) 
	desc->event_mask |= flags;
    else
	desc->event_mask &= (~flags);
    DEBUGF(("port %d: winsock_event_select: "
	    "flags=%02X, on=%d, event_mask=%02X\n", 
	    desc->port, flags, on, desc->event_mask));
    /* The RIGHT WAY (TM) to do this is to make sure:
       A) The cancelling of all network events is done with
          NULL as the event parameter (bug in NT's winsock),
       B) The actual event handle is reset so that it is only
          raised if one of the requested network events is active,
       C) Avoid race conditions by making sure that the event cannot be set
          while we are preparing to set the correct network event mask.
       The simplest way to do it is to turn off all events, reset the
       event handle and then, if event_mask != 0, turn on the appropriate
       events again. */
    if ((*winSock.WSAEventSelect)(desc->s, NULL, 0) != 0) {
	DEBUGF(("port %d: winsock_event_select: "
		"WSAEventSelect returned error, code %d.\n", 
		sock_errno()));
	desc->event_mask = save_event_mask;
	return -1;
    }
    if (!ResetEvent(desc->event)) {
	DEBUGF(("port %d: winsock_event_select: "
		"ResetEvent returned error, code %d.\n", 
		GetLastError()));
	desc->event_mask = 0;
	return -1;
    }
    if (desc->event_mask != 0) {
	if ((*winSock.WSAEventSelect)(desc->s, 
				      desc->event, 
				      desc->event_mask) != 0) {
	    DEBUGF(("port %d: winsock_event_select: "
		    "WSAEventSelect returned error, code %d.\n", 
		    sock_errno()));
	    desc->event_mask = 0;
	    return -1;
	}
    }
    return 0;
}
	    
	    

static int tcp_inet_event(tcp_descriptor* desc, HANDLE event)
{
    WSANETWORKEVENTS netEv;
    int err;

    DEBUGF(("port %d: tcp_inet_event...\n", desc->inet.port));
    if ((*winSock.WSAEnumNetworkEvents)(desc->inet.s, desc->inet.event,
					&netEv) != 0)
	return inet_error(INETP(desc), sock_errno());

    DEBUGF(("port %d:   ... event=%02X, mask=%02X\n",
	    desc->inet.port, netEv.lNetworkEvents, desc->inet.event_mask));

    /*
     * Calling WSAEventSelect with a mask of 0 doesn't always turn off
     * all events.  To avoid acting on events we don't want, we mask
     * the events with mask for the events we really want.
     */

#ifdef DEBUG
    if ((netEv.lNetworkEvents & ~(desc->inet.event_mask)) != 0) {
	DEBUGF(("port %d:  ... unexpected event: %d\n",
		desc->inet.port, netEv.lNetworkEvents & 
		~(desc->inet.event_mask)));
    }
#endif
    netEv.lNetworkEvents &= desc->inet.event_mask;

    /* Loop handling read and close events only on active connections,
       otherwise only handle one read event. */
    if (netEv.lNetworkEvents & FD_READ) {
	do {
	    if (tcp_inet_input(desc, event) < 0)
		return -1;
#ifdef DEBUG
	    if ((netEv.lNetworkEvents & FD_CLOSE) && INETP(desc)->active) {
		DEBUGF(("Retrying read due to closed port"));
	    }
#endif
	} while ((netEv.lNetworkEvents & FD_CLOSE) && INETP(desc)->active);
    }
    if (netEv.lNetworkEvents & FD_WRITE) {
	if (tcp_inet_output(desc, event) < 0)
	    return -1;
    }
    if (netEv.lNetworkEvents & FD_CONNECT) {
	if ((err = netEv.iErrorCode[FD_CONNECT_BIT]) != 0)
	    return inet_error(INETP(desc), err);
	return tcp_inet_output(desc, event);
    } else if (netEv.lNetworkEvents & FD_ACCEPT) {
	if ((err = netEv.iErrorCode[FD_ACCEPT_BIT]) != 0)
	    return inet_error(INETP(desc), err);
	return tcp_inet_input(desc, event);
    }
    /* Handle close only if we either have an active connection
       or we have a passive connection with no read event set. */
    if (netEv.lNetworkEvents & FD_CLOSE && 
	! ((netEv.lNetworkEvents & FD_READ) && !(INETP(desc)->active))) {
	/* error in err = netEv.iErrorCode[FD_CLOSE_BIT] */
	DEBUGF(("Detected close in %s, line %d\n", __FILE__, __LINE__));
	return tcp_recv_closed(desc);
    }
    return 0;
}

#endif


/* socket has input:
** 1. TCP_STATE_ACCEPTING  => non block accept ? 
** 2. TCP_STATE_CONNECTED => read input
*/
static int tcp_inet_input(tcp_descriptor* desc, HANDLE event)
{
    int len;
    int ix;
    SOCKET s;
    tcp_descriptor* a_desc;

    if (desc->inet.state == TCP_STATE_LISTENING) {
	sock_select(INETP(desc),FD_ACCEPT,0);
	ix = desc->i_ix;
	desc->inet.state = TCP_STATE_LISTEN; /* restore state */
	desc->i_ix = -1;

	/* closed ? */
	if (((a_desc = (tcp_descriptor*)inet_desc_table[ix]) == NULL) || 
	    (a_desc->inet.state != TCP_STATE_ACCEPTING))
	    return 0;

	len = sizeof(a_desc->inet.remote);
	s = sock_accept(desc->inet.s, 
			(struct sockaddr*) &a_desc->inet.remote, &len);

	driver_cancel_timer(a_desc->inet.port); /* posssibly cancel a timer */

	if (s == INVALID_SOCKET) {
	    a_desc->inet.state = TCP_STATE_CLOSED;
	    return inet_error(INETP(a_desc), sock_errno());
	}
	else {
	    a_desc->inet.s = s;
	    if ((a_desc->inet.event = sock_create_event(INETP(a_desc))) ==
		INVALID_EVENT)
		return inet_error(INETP(a_desc), sock_errno());
	    SET_NONBLOCKING(a_desc->inet.s);
#ifdef __WIN32__
	driver_select(a_desc->inet.port, a_desc->inet.event, DO_READ, 1);
#endif
	    a_desc->inet.state = TCP_STATE_CONNECTED;
	    /* A sock_select will be activated if active == true by the
	       INET_REQ_SETOPTS that will immediately follow this
	       */
	    inet_reply(INETP(a_desc), TCP_REP_ACCEPT);
	}
    }
    else if (IS_CONNECTED(INETP(desc)))
	return tcp_recv(desc);
    else {
	/* maybe a close op from connection attempt?? */
	sock_select(INETP(desc),FD_ACCEPT,0);
	DEBUGF(("bad state in tcp_inet_input: %04x\n\r", desc->inet.state));
    }
    return 0;
}

static int tcp_send_error(tcp_descriptor* desc, int err)
{
    inet_address other;
    int sz = sizeof(other);
    int code;

    code = sock_peer(desc->inet.s,(struct sockaddr*) &other,&sz);
    if ((code == SOCKET_ERROR) && (sock_errno() == ENOTCONN ||
				   sock_errno() == EPIPE)) {
	DEBUGF(("driver_failure_eof(%d) in %s, line %d\n",
		desc->inet.port, __FILE__, __LINE__));
	driver_failure_eof(desc->inet.port);
    } else {
	driver_failure_posix(desc->inet.port, sock_errno());
    }
    return -1;
}

/*
** Length of header when sending
*/
static int tcp_header_length(int type)
{
    switch(type) {
    case TCP_PB_1:   return 1;
    case TCP_PB_2:   return 2;
    case TCP_PB_4:   return 4;
    default:         return 0;
    }
}

/*
** Send non blocking data
*/
static int tcp_send(tcp_descriptor* desc, char* ptr, int len)
{
    int sz;
    char buf[4];
    int h_len;
    int n;
    int ix = desc->inet.port;
    SysIOVec iov[2];

    h_len = tcp_header_length(desc->inet.htype);
    if ((len == 0) && (h_len == 0))
	return 0;

    inet_output_count(INETP(desc), len+h_len);

    switch(h_len) {
    case 0: break;
    case 1: put_int8(len, buf); break;
    case 2: put_int16(len, buf); break;
    case 4: put_int32(len, buf); break;
    }

    if ((sz = driver_sizeq(ix)) > 0) {
	if (h_len > 0)
	    driver_enq(ix, buf, h_len);
	driver_enq(ix, ptr, len);
	if (sz >= INET_HIGH_WATERMARK) {
	    desc->inet.state |= INET_F_BUSY;  /* mark for low-watermark */
	    set_busy_port(desc->inet.port, 1);
	}
    }
    else {
	iov[0].iov_base = buf;
	iov[0].iov_len = h_len;
	iov[1].iov_base = ptr;
	iov[1].iov_len = len;

	DEBUGF(("port %d: about to send %d,%d bytes\n",
		desc->inet.port, h_len, len));
	if (sock_sendv(desc->inet.s, iov, 2, &n, 0) == SOCKET_ERROR) {
	    if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		DEBUGF(("sock_sendv(size=2) failed, errno = %d\n",
			sock_errno()));
		return tcp_send_error(desc, sock_errno());
	    }
	    n = 0;
	}
	else if (n == len+h_len)
	    return 0;

	DEBUGF(("port %d: Send failed, queuing", desc->inet.port));

	if (n < h_len) {
	    driver_enq(ix, buf+n, h_len-n);
	    driver_enq(ix, ptr, len);
	}
	else {
	    n -= h_len;
	    driver_enq(ix, ptr+n, len-n);
	}
	sock_select(INETP(desc),(FD_WRITE|FD_CLOSE), 1);
    }
    return 0;
}

/* socket has ouput:
** 1. TCP_STATE_CONNECTING => non block connect ?
** 2. TCP_STATE_CONNECTED  => write output
*/
static int tcp_inet_output(tcp_descriptor* desc, HANDLE event)
{
    int ix = desc->inet.port;

    if (desc->inet.state == TCP_STATE_CONNECTING) {
	sock_select(INETP(desc),FD_CONNECT,0);

	driver_cancel_timer(ix);  /* posssibly cancel a timer */
#ifndef __WIN32__
	/*
	 * XXX This is strange.  This *should* work on Windows NT too,
	 * but doesn't.  An bug in Winsock 2.0 for Windows NT?
	 *
	 * See "Unix Netwok Programming", W.R.Stevens, p 412 for a
	 * discussion about Unix portability and non blocking connect.
	 */

#ifndef SOCKOPT_CONNECT_STAT
	{
	    int sz = sizeof(desc->inet.remote);
	    int code = sock_peer(desc->inet.s,
				 (struct sockaddr*) &desc->inet.remote, &sz);

	    if (code == SOCKET_ERROR) {
		desc->inet.state = TCP_STATE_BOUND;  /* restore state */
		return inet_error(INETP(desc), sock_errno());
	    }
	}
#else
	{
	    int error = 0;	/* Has to be initiated, we check it */
	    int sz = sizeof(error); /* even if we get -1 */
	    int code = sock_getopt(desc->inet.s, SOL_SOCKET, SO_ERROR, 
				   &error, &sz);

	    if ((code < 0) || error) {
		desc->inet.state = TCP_STATE_BOUND;  /* restore state */
		return inet_error(INETP(desc), sock_errno());
	    }
	}
#endif /* SOCKOPT_CONNECT_STAT */
#endif /* !__WIN32__ */

	desc->inet.state = TCP_STATE_CONNECTED;
	if (desc->inet.active)
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
	inet_reply(INETP(desc), TCP_REP_CONNECT);
    }
    else if (IS_CONNECTED(INETP(desc))) {
	for (;;) {
	    int vsize;
	    int n;
	    SysIOVec* iov;

	    if ((iov = driver_peekq(ix, &vsize)) == NULL) {
		sock_select(INETP(desc), FD_WRITE, 0);
		return 0;
	    }
	    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
	    DEBUGF(("About to send %d items on port %d\n", vsize, desc->inet.port));
	    if (sock_sendv(desc->inet.s, iov, vsize, &n, 0)==SOCKET_ERROR) {
		if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		    DEBUGF(("sock_sendv(vsize=%d) failed, errno = %d\n",
			    vsize, sock_errno()));
		    return tcp_send_error(desc, sock_errno());
		}
		return 0;
	    }
	    if (driver_deq(ix, n) <= INET_LOW_WATERMARK) {
		if (IS_BUSY(INETP(desc))) {
		    desc->inet.state &= ~INET_F_BUSY;
		    set_busy_port(desc->inet.port, 0);
		}
	    }
	}
    }
    else {
	sock_select(INETP(desc),FD_CONNECT,0);
	DEBUGF(("bad state in tcp_inet_output: %04x\n\r", desc->inet.state));
    }
    return 0;
}

/*-----------------------------------------------------------------------------

   UDP

-----------------------------------------------------------------------------*/

static int udp_inet_init(void)
{
    return 0;
}


static long udp_inet_start(long port, char *args)
{
    udp_descriptor* desc;
    
    desc = (udp_descriptor*) inet_start(port, sizeof(udp_descriptor));
    if (desc == NULL)
	return -1;
    return (long) desc;
}

static int udp_inet_stop(udp_descriptor *desc)
{
    inet_stop(&desc->inet);
    return 0;
}

static int udp_error(udp_descriptor *desc, int err)
{
    if (!desc->inet.active)
	inet_error(INETP(desc), err);
    driver_failure_posix(desc->inet.port, err);
    return -1;
}


static int udp_inet_ctl(udp_descriptor* desc, int cmd, char* buf, int len, 
			char** res_buf, int res_size)
{
    return inet_ctl(INETP(desc), cmd, buf, len, res_buf, res_size);
}

static int udp_inet_timeout(udp_descriptor* desc)
{
    if (!desc->inet.active)
	sock_select(INETP(desc),FD_READ,0);
    return inet_report(INETP(desc), "timedout");
}


static int udp_inet_command(udp_descriptor* desc, char *buf, int len)
{
    int code;
    char* ptr = buf;
    char* qtr;
    int sz;
    inet_address other;
    unsigned long timeout;

    if (len < 1)
	return 0; /* let caller hang */
    len--;

    switch (*ptr++) {
    case UDP_REQ_OPEN:   /* open socket and return internal index */
	if ((len == 1) && (*ptr == INET_AF_INET))
	    code = inet_open(INETP(desc), AF_INET, SOCK_DGRAM);
#if defined(AF_INET6) && defined(AF_INET6)
	else if ((len == 1) && (*ptr == INET_AF_INET6))
	    code = inet_open(INETP(desc), AF_INET6, SOCK_DGRAM);
#endif
	else
	    return inet_error(INETP(desc), EINVAL);

	if (code == 0) {
	    if (desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
	}
#ifdef HAVE_SO_BSDCOMPAT
	/*
	 * Make sure that sending udp packets to a non existing port on an
	 * existing machine doesn't close the socket. (Linux behaves this
	 * way)
	 */
	{
	    int one = 1;
	    /* Ignore errors */
	    sock_setopt(desc->inet.s, SOL_SOCKET, SO_BSDCOMPAT, &one,
			sizeof(one));
	}
#endif
	return code;

    case UDP_REQ_FDOPEN:   /* pass in an open (and bound) socket */
	if ((len == 5) && (*ptr == INET_AF_INET))
	    code = inet_fdopen(INETP(desc), AF_INET, SOCK_DGRAM,
			       (SOCKET)get_int32(ptr+1));
#if defined(HAVE_IN6) && defined(AF_INET6)
	else if ((len == 5) && (*ptr == INET_AF_INET6))
	    code = inet_fdopen(INETP(desc), AF_INET6, SOCK_DGRAM,
			       (SOCKET)get_int32(ptr+1));
#endif
	else
	    return inet_error(INETP(desc), EINVAL);
	if (code == 0) {
	    if (desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
	}
	return code;

    case UDP_REQ_CONNECT: 
	/* XXX this code is not used but leave it we may use it AGAIN ? */
	/* input should: P1 P0 A B C D (no input => dissolve) */
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), ENOTSOCK);

	if (len == 0) {
	    sock_connect(desc->inet.s, (struct sockaddr*) NULL, 0);
	    desc->inet.state &= ~INET_F_ACTIVE;
	}
	else {
	    if (inet_set_address(desc->inet.sfamily, 
				 &desc->inet.remote, ptr, &len) == NULL)
		return inet_error(INETP(desc), EINVAL);
	    code = sock_connect(desc->inet.s,
				(struct sockaddr*) &desc->inet.remote,
				len);
	    if (code == SOCKET_ERROR)
		return inet_error(INETP(desc), sock_errno());
	    else /* ok we are connected */
		desc->inet.state |= INET_F_ACTIVE;
	}
	inet_reply(INETP(desc), UDP_REP_CONNECT);
	return 0;

    case UDP_REQ_SEND:  /* send on connected socket input is buffer */
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), ENOTSOCK);
	if (!IS_BOUND(INETP(desc)))
	    return inet_error(INETP(desc), EINVAL);
	if (!(desc->inet.state & INET_F_ACTIVE))
	    return inet_error(INETP(desc), ENOTCONN);
	/* ignore errno = EMSGSIZE ?? */
	sock_send(desc->inet.s, ptr, len, 0);
	return 0;
	
    case UDP_REQ_SENDTO: /* input should: P1 P0 A B C D & buffer */
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), ENOTSOCK);
	if (!IS_BOUND(INETP(desc)))
	    return inet_error(INETP(desc), EINVAL);

	sz = len;
	qtr = inet_set_address(desc->inet.sfamily, &other, ptr, &sz);
	if (qtr == NULL)
	    return inet_error(INETP(desc), EINVAL);
	len -= (qtr - ptr);
	ptr = qtr;
	inet_output_count(INETP(desc), len);

	/* ignore errno = EMSGSIZE ?? */
	sock_sendto(desc->inet.s, ptr, len, 0, (struct sockaddr*)&other, sz);
	return 0;

    case UDP_REQ_RECV:
	/* INPUT: Timeout(4), Length(4) */
	if (!IS_OPEN(INETP(desc)))
	    return inet_error(INETP(desc), ENOTSOCK);
	if (!IS_BOUND(INETP(desc)))
	    return inet_error(INETP(desc), EINVAL);
	if (desc->inet.active || (len != 8))
	    return inet_error(INETP(desc), EINVAL);
	timeout = get_int32(ptr);
	ptr += 4;
	len = get_int32(ptr);	 /* ignore ?? */
	if (timeout != INET_INFINITY)
	    driver_set_timer(desc->inet.port, timeout);
	return udp_inet_input(desc, desc->inet.event);

    default:
	return inet_command(INETP(desc), ptr - 1, len+1);
    }
}

#ifdef __WIN32__

static int udp_inet_event(udp_descriptor* desc, HANDLE event)
{
    WSANETWORKEVENTS netEv;

    if ((winSock.WSAEnumNetworkEvents)(desc->inet.s, desc->inet.event,
				       &netEv) != 0)
	return inet_error(INETP(desc), sock_errno());
 
    if (netEv.lNetworkEvents == 0)  /* NOTHING */
		return 0;
    if (netEv.lNetworkEvents & FD_READ)
		udp_inet_input(desc, event);
    return 0;
}

#endif


static int udp_inet_input(udp_descriptor* desc, HANDLE event)
{
    int n;
    int len;
    inet_address other;
    char buf[sizeof(inet_address)];  /* buffer address */
    int sz;
    int hsz;
    int offs;
    char* ptr;
    DriverBinary* bin; /* binary */
    int packet_count = INET_UDP_POLL;

    while(packet_count--) {
	len = sizeof(other);
	sz = desc->inet.bufsz;
	/* Allocate space for message and address and reply code */
	if ((bin = driver_alloc_binary(sz+len+1)) == NULL)
	    return udp_error(desc, ENOMEM);
	ptr = bin->orig_bytes + len + 1;  /* point to message part */

	/* Note: On Windows NT, recvfrom() fails if the socket is connected. */
	if (desc->inet.state & INET_F_ACTIVE) {
	    n = sock_recv(desc->inet.s, ptr, sz, 0);
	    other = desc->inet.remote;
	}
	else
	    n = sock_recvfrom(desc->inet.s, ptr, sz, 0,
				 (struct sockaddr*)&other, &len);
	if (n == SOCKET_ERROR) {
	    driver_free_binary(bin);
	    if (sock_errno() != ERRNO_BLOCK) {
		return udp_error(desc, sock_errno());
	    }
	    if (!desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
	    return 0;		/* strange, not ready */
	}
	else {
	    inet_input_count(INETP(desc), n);

	    /* XXX no error check (what to do) */
	    /* format address into temparay buffer */
	    inet_get_address(desc->inet.sfamily, buf, &other, &len);

	    /* copy formatted adderss to ptr len is actual length */
	    sys_memcpy(ptr - len, buf, len);
	    ptr -= (len+1);
	    ptr[0] = INET_REP_DATA;

	    hsz = desc->inet.hsz;
	    if (hsz > n) /* head includes complete packet */
		driver_output2(desc->inet.port, ptr, n+len+1, NULL, 0);
	    else {
		offs = ptr - bin->orig_bytes; /* inital pointer offset */

		if (n < BIN_REALLOC_LIMIT(sz)) {
		    DriverBinary* tbin;
		    if ((tbin = driver_realloc_binary(bin,n+sizeof(other)+1))!=NULL) {
			bin = tbin;
			ptr = tbin->orig_bytes + offs;
		    }
		}
		driver_output_binary(desc->inet.port,
				     ptr, hsz+len+1,
				     bin, sizeof(other)+1+hsz, n-hsz);
	    }
	    driver_free_binary(bin);
	    if (!desc->inet.active) {
		driver_cancel_timer(desc->inet.port); /* possibly cancel */
		sock_select(INETP(desc),FD_READ,0);
		return 0;  /* passive mode (read one packet only) */
	    }
	}
    }
    return 0;
}


#ifdef __WIN32__

int tcp_lookup_functions(void)
{
    static char dll_name[] = "ws2_32";
    HMODULE module;
    int i;

    if (winSock.WSAStartup != NULL)
	return TRUE;

    module = LoadLibrary(dll_name);

    if (module == NULL) {
	return FALSE;
    }
    winSock.WSAStartup = (LPFN_WSASTARTUP) 
	GetProcAddress(module, "WSAStartup");
    winSock.WSACleanup = (LPFN_WSACLEANUP)
	GetProcAddress(module, "WSACleanup");
    winSock.WSAGetLastError = (LPFN_WSAGETLASTERROR)
	GetProcAddress(module, "WSAGetLastError");
    winSock.WSAWaitForMultipleEvents = (LPFN_WSAWAITFORMULTIPLEEVENTS)
	GetProcAddress(module, "WSAWaitForMultipleEvents");
    winSock.WSACreateEvent = (LPFN_WSACREATEEVENT) 
	GetProcAddress(module, "WSACreateEvent");
    winSock.WSACloseEvent = (LPFN_WSACLOSEEVENT) 
	GetProcAddress(module, "WSACloseEvent");
    winSock.WSAResetEvent = (LPFN_WSARESETEVENT)
	GetProcAddress(module, "WSAResetEvent");
    winSock.WSAEventSelect = (LPFN_WSAEVENTSELECT)
	GetProcAddress(module, "WSAEventSelect");
    winSock.WSASetEvent = (LPFN_WSASETEVENT)
	GetProcAddress(module, "WSASetEvent");
    winSock.WSAEnumNetworkEvents = (LPFN_WSAENUMNETWORKEVENTS)
	GetProcAddress(module, "WSAEnumNetworkEvents");
    winSock.WSASend = (LPFN_WSASEND) 
	GetProcAddress(module, "WSASend");
    winSock.accept = (LPFN_ACCEPT)
	GetProcAddress(module, "accept");
    winSock.bind = (LPFN_BIND) 
	GetProcAddress(module, "bind");
    winSock.closesocket = (LPFN_CLOSESOCKET)
	GetProcAddress(module, "closesocket");
    winSock.connect = (LPFN_CONNECT) 
	GetProcAddress(module, "connect");
    winSock.ioctlsocket = (LPFN_IOCTLSOCKET)
	GetProcAddress(module, "ioctlsocket");
    winSock.getsockopt = (LPFN_GETSOCKOPT)
	GetProcAddress(module, "getsockopt");
    winSock.htonl = (LPFN_HTONL) 
	GetProcAddress(module, "htonl");
    winSock.htons = (LPFN_HTONS)
	GetProcAddress(module, "htons");
    winSock.inet_addr = (LPFN_INET_ADDR)
	GetProcAddress(module, "inet_addr");
    winSock.inet_ntoa = (LPFN_INET_NTOA)
	GetProcAddress(module, "inet_ntoa");
    winSock.listen = (LPFN_LISTEN)
	GetProcAddress(module, "listen");
    winSock.ntohs = (LPFN_NTOHS)
	GetProcAddress(module, "ntohs");
    winSock.ntohl = (LPFN_NTOHL) 
	GetProcAddress(module, "ntohl");
    winSock.recv = (LPFN_RECV) 
	GetProcAddress(module, "recv");
    winSock.send = (LPFN_SEND) 
	GetProcAddress(module, "send");
    winSock.recvfrom = (LPFN_RECVFROM)
	GetProcAddress(module, "recvfrom");
    winSock.sendto = (LPFN_SENDTO) 
	GetProcAddress(module, "sendto");
    winSock.setsockopt = (LPFN_SETSOCKOPT)
	GetProcAddress(module, "setsockopt");
    winSock.shutdown = (LPFN_SHUTDOWN) 
	GetProcAddress(module, "shutdown");
    winSock.socket = (LPFN_SOCKET) 
	GetProcAddress(module, "socket");
    winSock.gethostbyaddr = (LPFN_GETHOSTBYADDR)
	GetProcAddress(module, "gethostbyaddr");
    winSock.gethostbyname = (LPFN_GETHOSTBYNAME)
	GetProcAddress(module, "gethostbyname");
    winSock.gethostname = (LPFN_GETHOSTNAME) 
	GetProcAddress(module, "gethostname");
    winSock.getservbyname = (LPFN_GETSERVBYNAME)
	GetProcAddress(module, "getservbyname");
    winSock.getsockname = (LPFN_GETSOCKNAME) 
	GetProcAddress(module, "getsockname");
    winSock.getpeername = (LPFN_GETPEERNAME) 
	GetProcAddress(module, "getpeername");

    /*
     * Check that all of the pointers got a non-NULL value.
     */
    for (i = 0; i < sizeof(WinSockFuncs)/sizeof(void*); i++) {
	if (((char **)&winSock)[i] == NULL) {
	    DEBUGF(("Function %d not initialized\n", i));
	    return FALSE;
	}
    }

    return TRUE;
}

/*
 * We must make sure that the socket handles are not inherited
 * by port programs (if there are inherited, the sockets will not
 * get closed when the emulator terminates, and epmd and other Erlang
 * nodes will not notice that we have exited).
 *
 * XXX It is not clear whether this works/is necessary in Windows 95.
 * There could also be problems with Winsock implementations from other
 * suppliers than Microsoft.
 */

static SOCKET
make_noninheritable_handle(SOCKET s)
{
    HANDLE non_inherited;
    HANDLE this_process = GetCurrentProcess();

    if (s == INVALID_SOCKET) {
	return s;
    } else if (DuplicateHandle(this_process, (HANDLE) s,
			       this_process, &non_inherited, 0,
			       FALSE, DUPLICATE_SAME_ACCESS)) {
	(*winSock.closesocket)(s);
	return (SOCKET) non_inherited;
    } else {
	/*
	 * Silently use the old handle.
	 */
	return s;
    }
}

#endif
