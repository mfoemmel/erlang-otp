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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

#ifndef _OSE_
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#endif

/* use http processing */
#define USE_HTTP 
/* All platforms fail on malloc errors. */
#define FATAL_MALLOC


#include "erl_driver.h"

#ifdef __WIN32__
#define  STRNCASECMP strncasecmp

#define INCL_WINSOCK_API_TYPEDEFS 1

#include <winsock2.h>
#include <windows.h>

#include <winsock_func.h>

#include <Ws2tcpip.h>   /* NEED VC 6.0 !!! */

#undef WANT_NONBLOCKING
#include "sys.h"

#define HAVE_MULTICAST_SUPPORT

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
    make_noninheritable_handle((*winSock.socket)((af), (type), (proto)))
#define sock_close(s)              (*winSock.closesocket)((s))
#define sock_shutdown(s, how)      (*winSock.shutdown)((s), (how))

#define sock_accept(s, addr, len) \
    make_noninheritable_handle((*winSock.accept)((s), (addr), (len)))
#define sock_connect(s, addr, len) (*winSock.connect)((s), (addr), (len))
#define sock_listen(s, b)          (*winSock.listen)((s), (b))
#define sock_bind(s, addr, len)    (*winSock.bind)((s), (addr), (len))
#define sock_getopt(s,t,n,v,l)     (*winSock.getsockopt)((s),(t),(n),(v),(l))
#define sock_setopt(s,t,n,v,l)     (*winSock.setsockopt)((s),(t),(n),(v),(l))
#define sock_name(s, addr, len)    (*winSock.getsockname)((s), (addr), (len))
#define sock_peer(s, addr, len)    (*winSock.getpeername)((s), (addr), (len))
#define sock_ntohs(x)              (*winSock.ntohs)((x))
#define sock_ntohl(x)              (*winSock.ntohl)((x))
#define sock_htons(x)              (*winSock.htons)((x))
#define sock_htonl(x)              (*winSock.htonl)((x))
#define sock_send(s,buf,len,flag)  (*winSock.send)((s),(buf),(len),(flag))
#define sock_sendv(s, vec, size, np, flag) \
                (*winSock.WSASend)((s),(WSABUF*)(vec),\
				   (size),(np),(flag),NULL,NULL)
#define sock_recv(s,buf,len,flag)  (*winSock.recv)((s),(buf),(len),(flag))

#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                (*winSock.recvfrom)((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                (*winSock.sendto)((s),(buf),(blen),(flag),(addr),(alen))
#define sock_hostname(buf, len)    (*winSock.gethostname)((buf), (len))

#define sock_getservbyname(name,proto) (*winSock.getservbyname)((name),(proto))
#define sock_getservbyport(port,proto) (*winSock.getservbyport)((port),(proto))

#define sock_errno() \
              (winSock.WSAGetLastError ? (*winSock.WSAGetLastError)() : 0)
#define sock_create_event(d)       (*winSock.WSACreateEvent)()
#define sock_close_event(e)        (*winSock.WSACloseEvent)(e)

#define sock_select(D, Flags, OnOff) winsock_event_select(D, Flags, OnOff)

#define SET_BLOCKING(s)           (*winSock.ioctlsocket)(s, FIONBIO, &zero_value)
#define SET_NONBLOCKING(s)        (*winSock.ioctlsocket)(s, FIONBIO, &one_value)

static unsigned long zero_value = 0;
static unsigned long one_value = 1;

#else

#ifdef VXWORKS
#include <sockLib.h>
#include <sys/times.h>
#include <iosLib.h>
#include <taskLib.h>
#include <selectLib.h>
#include <ioLib.h>
#else
#include <sys/time.h>
#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
#endif
#include <netdb.h>
#endif

#ifndef _OSE_
#include <sys/socket.h>
#include <netinet/in.h>
#else
/* datatypes and macros from Solaris socket.h */
struct  linger {
        int     l_onoff;                /* option on/off */
        int     l_linger;               /* linger time */
};
#define SO_OOBINLINE    0x0100          /* leave received OOB data in line */
#define SO_LINGER       0x0080          /* linger on close if data present */
#endif

#ifdef VXWORKS
#include <rpc/rpctypes.h>
#endif
#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif

#ifndef _OSE_
#include <netinet/tcp.h>
#include <arpa/inet.h>
#endif

#if (!defined(VXWORKS) && !defined(_OSE_))
#include <sys/param.h>
#ifdef HAVE_ARPA_NAMESER_H
#include <arpa/nameser.h>
#endif
#endif

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#ifndef _OSE_
#include <net/if.h>
#else
#define IFF_MULTICAST 0x00000800
#endif

#ifdef _OSE_
#include "inet.h"
#include "ineterr.h"
#include "ose_inet_drv.h"
#include "nameser.h" 
#include "resolv.h"
#define SET_ASYNC(s) setsockopt((s), SOL_SOCKET, SO_OSEEVENT, (&(s)), sizeof(int))

extern void select_release(void);

#endif /* _OSE_ */

/* Solaris headers, only to be used with SFK */
#ifdef _OSE_SFK_
#include <ctype.h>
#include <string.h>
#endif

#ifndef WANT_NONBLOCKING
#define WANT_NONBLOCKING
#endif
#include "sys.h"

/* #define INET_DRV_DEBUG 1 */
#ifdef INET_DRV_DEBUG
#define DEBUG 1
#undef DEBUGF
#define DEBUGF(X) printf X
#endif

#if !defined(__WIN32__) && !defined(HAVE_STRNCASECMP)
#define STRNCASECMP my_strncasecmp

static int my_strncasecmp(const char *s1, const char *s2, size_t n)
{
    int i;

    for (i=0;i<n-1 && s1[i] && s2[i] && toupper(s1[i]) == toupper(s2[i]);++i)
	;
    return (toupper(s1[i]) - toupper(s2[i]));
}
	

#else
#define  STRNCASECMP strncasecmp
#endif

#define INVALID_SOCKET -1
#define INVALID_EVENT  -1
#define SOCKET_ERROR   -1
#define SOCKET int
#define HANDLE long int
#define FD_READ    DO_READ
#define FD_WRITE   DO_WRITE
#define FD_CLOSE   0
#define FD_CONNECT DO_WRITE
#define FD_ACCEPT  DO_READ

#define sock_connect(s, addr, len)  connect((s), (addr), (len))
#define sock_listen(s, b)           listen((s), (b))
#define sock_bind(s, addr, len)     bind((s), (addr), (len))
#ifdef VXWORKS
#define sock_getopt(s,t,n,v,l)      wrap_sockopt(&getsockopt,\
                                                 s,t,n,v,(unsigned int)(l))
#define sock_setopt(s,t,n,v,l)      wrap_sockopt(&setsockopt,\
                                                 s,t,n,v,(unsigned int)(l))
#else
#define sock_getopt(s,t,n,v,l)      getsockopt((s),(t),(n),(v),(l))
#define sock_setopt(s,t,n,v,l)      setsockopt((s),(t),(n),(v),(l))
#endif
#define sock_name(s, addr, len)     getsockname((s), (addr), (len))
#define sock_peer(s, addr, len)     getpeername((s), (addr), (len))
#define sock_ntohs(x)               ntohs((x))
#define sock_ntohl(x)               ntohl((x))
#define sock_htons(x)               htons((x))
#define sock_htonl(x)               htonl((x))

#ifdef _OSE_
#define sock_accept(s, addr, len)   ose_inet_accept((s), (addr), (len))
#define sock_send(s,buf,len,flag)   ose_inet_send((s),(buf),(len),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                ose_inet_sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendv(s, vec, size, np, flag) \
		(*(np) = ose_inet_sendv((s), (SysIOVec*)(vec), (size)))
#define sock_open(af, type, proto)  ose_inet_socket((af), (type), (proto))
#define sock_close(s)               ose_inet_close((s))
#define sock_hostname(buf, len)     ose_gethostname((buf), (len))
#define sock_getservbyname(name,proto) ose_getservbyname((name), (proto))
#define sock_getservbyport(port,proto) ose_getservbyport((port), (proto))

#else
#define sock_accept(s, addr, len)   accept((s), (addr), (len))
#define sock_send(s,buf,len,flag)   send((s),(buf),(len),(flag))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendv(s, vec, size, np, flag) \
		(*(np) = writev((s), (struct iovec*)(vec), (size)))
#define sock_open(af, type, proto)  socket((af), (type), (proto))
#define sock_close(s)               close((s))
#define sock_shutdown(s, how)       shutdown((s), (how))

#define sock_hostname(buf, len)     gethostname((buf), (len))
#define sock_getservbyname(name,proto) getservbyname((name), (proto))
#define sock_getservbyport(port,proto) getservbyport((port), (proto))
#endif /* _OSE_ */

#define sock_recv(s,buf,len,flag)   recv((s),(buf),(len),(flag))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                recvfrom((s),(buf),(blen),(flag),(addr),(alen))

#define sock_errno()                errno
#define sock_create_event(d)        ((d)->s) /* return file descriptor */
#define sock_close_event(e)                  /* do nothing */

#ifdef _OSE_
#define inet_driver_select(port, e, mode, on) \
                                    ose_inet_select(port, e, mode, on)
#else
#define inet_driver_select(port, e, mode, on) \
                                    driver_select(port, e, mode, on)
#endif /* _OSE_ */

#define sock_select(d, flags, onoff) do { \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        DEBUGF(("sock_select(%ld): flags=%02X, onoff=%d, event_mask=%02lX\r\n", 		(long) (d)->port, (flags), (onoff), (unsigned long) (d)->event_mask)); \
        inet_driver_select((d)->port, (ErlDrvEvent)(long)(d)->event, (flags), (onoff)); \
   } while(0)


#endif /* __WIN32__ */

#define get_int24(s) ((((unsigned char*) (s))[0] << 16) | \
                      (((unsigned char*) (s))[1] << 8)  | \
                      (((unsigned char*) (s))[2]))

#define get_little_int32(s) ((((unsigned char*) (s))[3] << 24) | \
			     (((unsigned char*) (s))[2] << 16)  | \
			     (((unsigned char*) (s))[1] << 8) | \
			     (((unsigned char*) (s))[0]))

#define INET_AF_INET        1
#define INET_AF_INET6       2

#define INET_TYPE_STREAM    1
#define INET_TYPE_DGRAM     2

#define INET_MODE_LIST      0
#define INET_MODE_BINARY    1

#define INET_DELIVER_PORT   0
#define INET_DELIVER_TERM   1

#define INET_PASSIVE        0
#define INET_ACTIVE         1
#define INET_ONCE           2  /* active once then passive */

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

#define IS_CONNECTING(d) \
  (((d)->state & INET_F_CON) == INET_F_CON)

#define IS_BUSY(d) \
  (((d)->state & INET_F_BUSY) == INET_F_BUSY)

#define INET_REQ_OPEN          1
#define INET_REQ_CLOSE         2
#define INET_REQ_CONNECT       3
#define INET_REQ_PEER          4
#define INET_REQ_NAME          5
#define INET_REQ_BIND          6
#define INET_REQ_SETOPTS       7
#define INET_REQ_GETOPTS       8
#define INET_REQ_GETIX         9
/* #define INET_REQ_GETIF         10 REPLACE BY NEW STUFF */
#define INET_REQ_GETSTAT       11
#define INET_REQ_GETHOSTNAME   12
#define INET_REQ_FDOPEN        13
#define INET_REQ_GETFD         14
#define INET_REQ_GETTYPE       15
#define INET_REQ_GETSTATUS     16
#define INET_REQ_GETSERVBYNAME 17
#define INET_REQ_GETSERVBYPORT 18
#define INET_REQ_SETNAME       19
#define INET_REQ_SETPEER       20
#define INET_REQ_GETIFLIST     21
#define INET_REQ_IFGET         22
#define INET_REQ_IFSET         23
#define INET_REQ_SUBSCRIBE     24

#define INET_SUBS_EMPTY_OUT_Q  1

#define INET_REP_ERROR       0
#define INET_REP_OK          1
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
#define INET_LOPT_BUFFER      20  /* min buffer size hint */
#define INET_LOPT_HEADER      21  /* list header size */
#define INET_LOPT_ACTIVE      22  /* enable/disable active receive */
#define INET_LOPT_PACKET      23  /* packet header type (TCP) */
#define INET_LOPT_MODE        24  /* list or binary mode */
#define INET_LOPT_DELIVER     25  /* port or term delivery */
#define INET_LOPT_EXITONCLOSE 26  /* exit port on active close or not ! */
#define INET_LOPT_TCP_HIWTRMRK     27  /* set local high watermark */
#define INET_LOPT_TCP_LOWTRMRK     28  /* set local low watermark */
#define INET_LOPT_BIT8             29  /* set 8 bit detection */
#define INET_LOPT_TCP_SEND_TIMEOUT 30      /* set send timeout */
#define INET_LOPT_TCP_DELAY_SEND   31      /* Delay sends until next poll */
#define INET_LOPT_PACKET_SIZE      32      /* Max packet size */

#define INET_IFOPT_ADDR       1
#define INET_IFOPT_BROADADDR  2
#define INET_IFOPT_DSTADDR    3
#define INET_IFOPT_MTU        4
#define INET_IFOPT_NETMASK    5
#define INET_IFOPT_FLAGS      6
#define INET_IFOPT_HWADDR     7

#define INET_BIT8_CLEAR 0
#define INET_BIT8_SET   1
#define INET_BIT8_ON    2
#define INET_BIT8_OFF   3

/* Enumerate the statistics ops */
#define INET_STAT_RECV_CNT   1
#define INET_STAT_RECV_MAX   2
#define INET_STAT_RECV_AVG   3
#define INET_STAT_RECV_DVI   4
#define INET_STAT_SEND_CNT   5
#define INET_STAT_SEND_MAX   6
#define INET_STAT_SEND_AVG   7
#define INET_STAT_SEND_PND   8
#define INET_STAT_RECV_OCT   9      /* received octets */ 
#define INET_STAT_SEND_OCT   10     /* sent octets */


#define INET_DEF_BUFFER     1024        /* default buffer size */
#define INET_MIN_BUFFER     1           /* internal min buffer */
#define INET_MAX_BUFFER     (1024*64)   /* internal max buffer */

/* Note: INET_HIGH_WATERMARK MUST be less than 2*INET_MAX_BUFFER */
#define INET_HIGH_WATERMARK (1024*8) /* 8k pending high => busy  */
/* Note: INET_LOW_WATERMARK MUST be less than INET_MAX_BUFFER and
** less than INET_HIGH_WATERMARK
*/
#define INET_LOW_WATERMARK  (1024*4) /* 4k pending => allow more */

#define INET_INFINITY  0xffffffff  /* infinity value */

#define INET_MAX_ASYNC 1           /* max number of async queueu ops */

/* INET_UDP_POLL could be an option !! */
#define INET_UDP_POLL   5        /* maximum number of packets to poll */

/* Max interface name */
#define INET_IFNAMSIZ          16

/* Erlang version of flags */
#define INET_IFF_UP            0x0001
#define INET_IFF_BROADCAST     0x0002
#define INET_IFF_LOOPBACK      0x0004
#define INET_IFF_POINTTOPOINT  0x0008
#define INET_IFF_RUNNING       0x0010
#define INET_IFF_MULTICAST     0x0020

/* Complement flags for turning them off */
#define INET_IFF_DOWN            0x0100
#define INET_IFF_NBROADCAST      0x0200
/* #define INET_IFF_NLOOPBACK    0x0400 */
#define INET_IFF_NPOINTTOPOINT   0x0800
/* #define INET_IFF_NRUNNING     0x1000 */
/* #define INET_IFF_NMULTICAST   0x2000 */


#define BIN_REALLOC_LIMIT(x)  (((x)*3)/4)  /* 75% */

/* The general purpose sockaddr */
typedef union {
    struct sockaddr sa;
    struct sockaddr_in sai;
#ifdef HAVE_IN6
    struct sockaddr_in6 sai6;
#endif
} inet_address;


/* for AF_INET & AF_INET6 */
#define inet_address_port(x) ((x)->sai.sin_port)

#if defined(HAVE_IN6) && defined(AF_INET6)
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : \
    ((family == AF_INET6) ? sizeof(struct in6_addr) : 0))
#else
#define addrlen(family) \
   ((family == AF_INET) ? sizeof(struct in_addr) : 0)
#endif

typedef struct {
    int            id;      /* id used to identify reply */
    ErlDrvTermData caller;  /* recipient of async reply */
    int            req;     /* Request id (CONNECT/ACCEPT/RECV) */
    unsigned       timeout; /* Request timeout (since op issued,not started) */
} inet_async_op;

typedef struct subs_list_ {
  ErlDrvTermData subscriber;
  struct subs_list_ *next;
} subs_list;

#define NO_PROCESS 0
#define NO_SUBSCRIBERS(SLP) ((SLP)->subscriber == NO_PROCESS)
static void send_to_subscribers(ErlDrvPort, subs_list *, int,
				ErlDrvTermData [], int);
static void free_subscribers(subs_list*);
static int save_subscriber(subs_list *, ErlDrvTermData);

typedef struct {
    SOCKET s;                   /* the socket or INVALID_SOCKET if not open */
    HANDLE event;               /* Event handle (same as s in unix) */
    long  event_mask;           /* current FD events */
    ErlDrvPort  port;           /* the port identifier */
    ErlDrvTermData dport;       /* the port identifier as DriverTermData */
    int   state;                /* status */
    int   prebound;             /* only set when opened with inet_fdopen */
    int   mode;                 /* BINARY | LIST
				   (affect how to interpret hsz) */
    int   exitf;                /* exit port on close or not */
    int   bit8f;                /* check if data has bit number 7 set */
    int   deliver;              /* Delivery mode, TERM or PORT */

    ErlDrvTermData caller;      /* recipient of sync reply */
    ErlDrvTermData busy_caller; /* recipient of sync reply when caller busy.
				 * Only valid while INET_F_BUSY. */

    inet_async_op* oph;          /* queue head or NULL */
    inet_async_op* opt;          /* queue tail or NULL */
    inet_async_op  op_queue[INET_MAX_ASYNC];  /* call queue */

    int   active;               /* 0 = passive, 1 = active, 2 = active once */
    int   stype;                /* socket type SOCK_STREAM/SOCK_DGRAM */
    int   sfamily;              /* address family */
    int   htype;                /* header type (tcp only?) */
    unsigned int psize;         /* max packet size (tcp only?) */
    int   ix;                   /* descriptor index */
    int   bit8;                 /* set if bit8f==true and data some data
				   seen had the 7th bit set */
    inet_address remote;        /* remote address for connected sockets */
    inet_address peer_addr;     /* fake peer address */
    inet_address name_addr;     /* fake local address */

    inet_address* peer_ptr;    /* fake peername or NULL */
    inet_address* name_ptr;    /* fake sockname or NULL */

    int   bufsz;                /* minimum buffer constraint */
    unsigned int hsz;           /* the list header size, -1 is large !!! */
    /* statistics */
    unsigned long recv_oct[2];  /* number of received octets >= 64 bits */
    unsigned long recv_cnt;     /* number of packets received */
    unsigned long recv_max;     /* maximum packet size received */
    double recv_avg;            /* average packet size received */
    double recv_dvi;            /* avarage deviation from avg_size */
    unsigned long send_oct[2];  /* number of octets sent >= 64 bits */
    unsigned long send_cnt;     /* number of packets sent */
    unsigned long send_max;     /* maximum packet send */
    double send_avg;            /* average packet size sent */

    subs_list empty_out_q_subs; /* Empty out queue subscribers */
} inet_descriptor;


#define TCP_REQ_ACCEPT    30
#define TCP_REQ_LISTEN    31
#define TCP_REQ_RECV      32
#define TCP_REQ_UNRECV    33
#define TCP_REQ_SHUTDOWN  34


#define TCP_STATE_CLOSED     INET_STATE_CLOSED
#define TCP_STATE_OPEN       (INET_F_OPEN)
#define TCP_STATE_BOUND      (TCP_STATE_OPEN | INET_F_BOUND)
#define TCP_STATE_CONNECTED  (TCP_STATE_BOUND | INET_F_ACTIVE)
#define TCP_STATE_LISTEN     (TCP_STATE_BOUND | INET_F_LISTEN)
#define TCP_STATE_LISTENING  (TCP_STATE_LISTEN | INET_F_LST)
#define TCP_STATE_CONNECTING (TCP_STATE_BOUND | INET_F_CON)
#define TCP_STATE_ACCEPTING  (INET_F_ACC)

#define TCP_PB_RAW     0
#define TCP_PB_1       1
#define TCP_PB_2       2
#define TCP_PB_4       3
#define TCP_PB_ASN1    4
#define TCP_PB_RM      5
#define TCP_PB_CDR     6
#define TCP_PB_FCGI    7
#define TCP_PB_LINE_LF 8
#define TCP_PB_TPKT    9
#define TCP_PB_HTTP    10
#define TCP_PB_HTTPH   11


#define TCP_MAX_PACKET_SIZE 0x1000000  /* 16 M */

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */

static int tcp_inet_init(void);
static void tcp_inet_stop(ErlDrvData);
static void tcp_inet_command(ErlDrvData, char*, int);
static void tcp_inet_commandv(ErlDrvData, ErlIOVec*);
static void tcp_inet_drv_input(ErlDrvData, ErlDrvEvent);
static void tcp_inet_drv_output(ErlDrvData data, ErlDrvEvent event);
static ErlDrvData tcp_inet_start(ErlDrvPort, char* command);
static int tcp_inet_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void  tcp_inet_timeout(ErlDrvData);
#ifdef __WIN32__
static void tcp_inet_event(ErlDrvData, ErlDrvEvent);
#endif

static struct erl_drv_entry tcp_inet_driver_entry = 
{
    tcp_inet_init,  /* inet_init will add this driver !! */
    tcp_inet_start, 
    tcp_inet_stop, 
    tcp_inet_command,
#ifdef __WIN32__
    tcp_inet_event,
    NULL,
#else
    tcp_inet_drv_input,
    tcp_inet_drv_output,
#endif
    "tcp_inet",
    NULL,
    NULL,
    tcp_inet_ctl,
    tcp_inet_timeout,
    tcp_inet_commandv
};

#define UDP_REQ_RECV         30

#define UDP_STATE_CLOSED     INET_STATE_CLOSE
#define UDP_STATE_OPEN       (INET_F_OPEN)
#define UDP_STATE_BOUND      (UDP_STATE_OPEN | INET_F_BOUND)
#define UDP_STATE_CONNECTED  (UDP_STATE_BOUND | INET_F_ACTIVE)

static int udp_inet_init(void);
static void udp_inet_stop(ErlDrvData);
static void udp_inet_command(ErlDrvData, char*, int);
static void udp_inet_drv_input(ErlDrvData e, ErlDrvEvent event);
static ErlDrvData udp_inet_start(ErlDrvPort, char* command);
static int udp_inet_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void udp_inet_timeout(ErlDrvData);
#ifdef __WIN32__
static void udp_inet_event(ErlDrvData, ErlDrvEvent);
static SOCKET make_noninheritable_handle(SOCKET s);
static int winsock_event_select(inet_descriptor *, int, int);
#endif

static struct erl_drv_entry udp_inet_driver_entry = 
{
    udp_inet_init,  /* inet_init will add this driver !! */
    udp_inet_start,
    udp_inet_stop,
    udp_inet_command,
#ifdef __WIN32__
    udp_inet_event,
#else
    udp_inet_drv_input,
#endif
    NULL, 
    "udp_inet",
    NULL,
    NULL,
    udp_inet_ctl,
    udp_inet_timeout,
    NULL
};

typedef struct {
    inet_descriptor inet;       /* common data structure (DON'T MOVE) */
    int   high;                 /* high watermark */
    int   low;                  /* low watermark */
    int   send_timeout;         /* timeout to use in send */
    int   busy_on_send;         /* busy on send with timeout! */
    int   i_ix;                 /* accept descriptor index / read indicator */
    int   i_bufsz;              /* current input buffer size (<= bufsz) */
    ErlDrvBinary* i_buf;        /* current binary buffer */
    char*         i_ptr;        /* current pos in buf */
    char*         i_ptr_start;  /* packet start pos in buf */
    int           i_remain;     /* remaining chars to read */
    int   fdelay_send;           /* Delay all sends until next poll (exp) */
#ifdef USE_HTTP
    int           http_state;   /* 0 = response|request  1=headers fields */
#endif
} tcp_descriptor;

/* send function */
static int tcp_send(tcp_descriptor* desc, char* ptr, int len);
static int tcp_sendv(tcp_descriptor* desc, ErlIOVec* ev);
static int tcp_recv(tcp_descriptor* desc, int request_len);
static int tcp_deliver(tcp_descriptor* desc, int len);

static int tcp_inet_output(tcp_descriptor* desc, HANDLE event);
static int tcp_inet_input(tcp_descriptor* desc, HANDLE event);

typedef struct {
    inet_descriptor inet;   /* common data structure (DON'T MOVE) */
} udp_descriptor;

static int udp_inet_input(udp_descriptor* desc, HANDLE event);

/* convert descriptor poiner to inet_descriptor pointer */
#define INETP(d) (&(d)->inet)

static inet_descriptor** inet_desc_table;
static int inet_desc_ix;
static int inet_desc_size;         /* number of descriptors */

#ifndef INET_MAX_DESCRIPTOR
#define INET_MAX_DESCRIPTOR sys_max_files()
#endif

static int async_ref = 0;          /* async reference id generator */
#define NEW_ASYNC_ID() ((async_ref++) & 0xffff)


static ErlDrvTermData am_ok;
static ErlDrvTermData am_tcp;
static ErlDrvTermData am_udp;
static ErlDrvTermData am_error;
static ErlDrvTermData am_inet_async;
static ErlDrvTermData am_inet_reply;
static ErlDrvTermData am_timeout;
static ErlDrvTermData am_closed;
static ErlDrvTermData am_tcp_closed;
static ErlDrvTermData am_tcp_error;
static ErlDrvTermData am_udp_closed;
static ErlDrvTermData am_udp_error;
static ErlDrvTermData am_empty_out_q;

/* speical errors for bad ports and sequences */
#define EXBADPORT "exbadport"
#define EXBADSEQ  "exbadseq"


static int inet_init(void);
static int ctl_reply(int, char*, int, char**, int);

struct erl_drv_entry inet_driver_entry = 
{
    inet_init,  /* inet_init will add tcp and udp drivers */
    NULL, /* start */
    NULL, /* stop */
    NULL, /* output */
    NULL, /* ready_input */
    NULL, /* ready_output */
    "inet"
};

/* XXX: is this a driver interface function ??? */
extern void erl_exit(int n, char*, _DOTS_);
extern int send_error_to_logger(int);

/*
 * Malloc wrapper,
 * we would like to change the behaviour for different 
 * systems here.
 */

#ifdef FATAL_MALLOC

static void *alloc_wrapper(size_t size){
    void *ret = driver_alloc(size);
    if(ret == NULL) 
	erl_exit(1,"Out of virtual memory in malloc (%s)", __FILE__);
    return ret;
}
#define ALLOC(X) alloc_wrapper(X)

static void *realloc_wrapper(void *current, size_t size){
    void *ret = driver_realloc(current,size);
    if(ret == NULL) 
	erl_exit(1,"Out of virtual memory in realloc (%s)", __FILE__);
    return ret;
}
#define REALLOC(X,Y) realloc_wrapper(X,Y)
#define FREE(P) driver_free((P))
#else /* FATAL_MALLOC */

#define ALLOC(X) driver_alloc((X))
#define REALLOC(X,Y) driver_realloc((X), (Y))
#define FREE(P) driver_free((P))

#endif /* FATAL_MALLOC */


#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  (i+2))

#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  (i+2))

#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  (i+2))

#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  (i+2))

#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  (i+4))

#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  (i+3))

#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  (i+2))

#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  (i+2))


static int load_ip_port(ErlDrvTermData* spec, int i, char* buf)
{
    spec[i++] = ERL_DRV_INT;
    spec[i++] = (ErlDrvTermData) get_int16(buf);
    return i;
}

static int load_ip_address(ErlDrvTermData* spec, int i, int family, char* buf)
{
    if (family == AF_INET) {
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[0]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[1]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[2]);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) ((unsigned char)buf[3]);
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 4;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (family == AF_INET6) {
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+2);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+4);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+6);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+8);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+10);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+12);
	spec[i++] = ERL_DRV_INT;
	spec[i++] = (ErlDrvTermData) get_int16(buf+14);
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 8;
    }
#endif
    else {
	spec[i++] = ERL_DRV_TUPLE;
	spec[i++] = 0;
    }
    return i;
}


/*
** Binary Buffer Managment
** We keep a stack of usable buffers 
*/
#define BUFFER_STACK_SIZE 16

static ErlDrvBinary* buffer_stack[BUFFER_STACK_SIZE];
static int buffer_stack_pos = 0;

#ifdef DEBUG
static int tot_buf_allocated = 0;  /* memory in use for i_buf */
static int tot_buf_stacked = 0;   /* memory on stack */
static int max_buf_allocated = 0; /* max allocated */

#define COUNT_BUF_ALLOC(sz) do { \
  tot_buf_allocated += (sz); \
  if (tot_buf_allocated > max_buf_allocated) \
    max_buf_allocated = tot_buf_allocated; \
} while(0)

#define COUNT_BUF_FREE(sz) do { tot_buf_allocated -= (sz); } while(0)

#define COUNT_BUF_STACK(sz) do { tot_buf_stacked += (sz); } while(0)

#else

#define COUNT_BUF_ALLOC(sz)
#define COUNT_BUF_FREE(sz)
#define COUNT_BUF_STACK(sz)

#endif

static ErlDrvBinary* alloc_buffer(long minsz)
{
    ErlDrvBinary* buf = NULL;

    DEBUGF(("alloc_buffer: sz = %ld, tot = %d, max = %d\r\n", 
	    minsz, tot_buf_allocated, max_buf_allocated));

    if (buffer_stack_pos > 0) {
	int origsz;

	buf = buffer_stack[--buffer_stack_pos];
	origsz = buf->orig_size;
	COUNT_BUF_STACK(-origsz);
	if (origsz < minsz) {
	    if ((buf = driver_realloc_binary(buf, minsz)) == NULL)
		return NULL;
	    COUNT_BUF_ALLOC(buf->orig_size - origsz);
	}
    }
    else {
	if ((buf = driver_alloc_binary(minsz)) == NULL)
	    return NULL;
	COUNT_BUF_ALLOC(buf->orig_size);
    }
    return buf;
}

/*
** Max buffer memory "cached" BUFFER_STACK_SIZE * INET_MAX_BUFFER
** (16 * 64k ~ 1M)
*/
/*#define CHECK_DOUBLE_RELEASE 1*/
static void release_buffer(ErlDrvBinary* buf)
{
    DEBUGF(("release_buffer: %ld\r\n", (buf==NULL) ? 0 : buf->orig_size));
    if (buf == NULL)
	return;
    if ((buf->orig_size > INET_MAX_BUFFER) || 
	(buffer_stack_pos >= BUFFER_STACK_SIZE)) {
	COUNT_BUF_FREE(buf->orig_size);
	driver_free_binary(buf);
    }
    else {
#ifdef CHECK_DOUBLE_RELEASE
#warning CHECK_DOUBLE_RELEASE is enabled, this is a custom build emulator
	int i;
	for (i = 0; i < buffer_stack_pos; ++i) {
	    if (buffer_stack[i] == buf) {
		erl_exit(1,"Multiple buffer release in inet_drv, this is a "
			 "bug, save the core and send it to "
			 "support@erlang.ericsson.se!");
	    }
	}
#endif
	buffer_stack[buffer_stack_pos++] = buf;
	COUNT_BUF_STACK(buf->orig_size);
    }
}

static ErlDrvBinary* realloc_buffer(ErlDrvBinary* buf, long newsz)
{
    ErlDrvBinary* bin;
#ifdef DEBUG
    long orig_size =  buf->orig_size;
#endif

    if ((bin = driver_realloc_binary(buf,newsz)) != NULL) {
	COUNT_BUF_ALLOC(newsz - orig_size);
	;
    }
    return bin;
}

/* use a TRICK, access the refc field to see if any one else has
 * a ref to this buffer then call driver_free_binary else 
 * release_buffer instead
 */
static void free_buffer(ErlDrvBinary* buf)
{
    DEBUGF(("free_buffer: %ld\r\n", (buf==NULL) ? 0 : buf->orig_size));

    if (buf != NULL) {
	if (buf->refc == 1)
	    release_buffer(buf);
	else {
	    COUNT_BUF_FREE(buf->orig_size);
	    driver_free_binary(buf);
	}
    }
}


#ifdef __WIN32__

static ErlDrvData dummy_start(ErlDrvPort port, char* command)
{
    return (ErlDrvData)port;
}

static int dummy_ctl(ErlDrvData data, unsigned int cmd, char* buf, int len,
		     char** rbuf, int rsize)
{
    static char error[] = "no_winsock2";

    driver_failure_atom((ErlDrvPort)data, error);
    return ctl_reply(INET_REP_ERROR, error, sizeof(error), rbuf, rsize);
}

static void dummy_command(ErlDrvData data, char* buf, int len)
{
}

static struct erl_drv_entry dummy_tcp_driver_entry = 
{
    NULL,			/* init */
    dummy_start,		/* start */
    NULL,			/* stop */
    dummy_command,		/* command */
    NULL,			/* input */
    NULL,			/* output */
    "tcp_inet",			/* name */
    NULL,
    NULL,
    dummy_ctl,
    NULL,
    NULL
};

static struct erl_drv_entry dummy_udp_driver_entry = 
{
    NULL,			/* init */
    dummy_start,		/* start */
    NULL,			/* stop */
    dummy_command,		/* command */
    NULL,			/* input */
    NULL,			/* output */
    "udp_inet",			/* name */
    NULL,
    NULL,
    dummy_ctl,
    NULL,
    NULL
};

#endif

/* general control reply function */
static int ctl_reply(int rep, char* buf, int len, char** rbuf, int rsize)
{
    char* ptr;

    if ((len+1) > rsize) {
	ptr = ALLOC(len+1);
	*rbuf = ptr;
    }
    else
	ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

/* general control error reply function */
static int ctl_error(int err, char** rbuf, int rsize)
{
    char response[256];		/* Response buffer. */
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = response; *s; s++, t++)
	*t = tolower(*s);
    return ctl_reply(INET_REP_ERROR, response, t-response, rbuf, rsize);
}

static int ctl_xerror(char* xerr, char** rbuf, int rsize)
{
    int n = strlen(xerr);
    return ctl_reply(INET_REP_ERROR, xerr, n, rbuf, rsize);
}


static ErlDrvTermData error_atom(int err)
{
    char errstr[256];
    char* s;
    char* t;

    for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
	*t = tolower(*s);
    *t = '\0';
    return driver_mk_atom(errstr);
}

/* setup a new async id + caller (format async_id into buf) */
static int enq_async(inet_descriptor* desc, char* buf, int req)
{
    int id = NEW_ASYNC_ID();
    inet_async_op* opp;

    if ((opp = desc->oph) == NULL)            /* queue empty */
	opp = desc->oph = desc->opt = desc->op_queue;
    else if (desc->oph == desc->opt) { /* queue full */ 
	DEBUGF(("enq(%ld): queue full\r\n", (long)desc->port));
	return -1;
    }

    opp->id = id;
    opp->caller = driver_caller(desc->port);
    opp->req = req;

    DEBUGF(("enq(%ld): %d %ld %d\r\n", 
	    (long) desc->port, opp->id, opp->caller, opp->req));

    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->oph = desc->op_queue;
    else
	desc->oph = opp;

    if (buf != NULL)
	put_int16(id, buf);
    return 0;
}

static int deq_async(inet_descriptor* desc, int* ap, ErlDrvTermData* cp, int* rp)
{
    inet_async_op* opp;

    if ((opp = desc->opt) == NULL) {  /* queue empty */
	DEBUGF(("deq(%ld): queue empty\r\n", (long)desc->port));
	return -1;
    }
    *ap = opp->id;
    *cp = opp->caller;
    *rp = opp->req;
    
    DEBUGF(("deq(%ld): %d %ld %d\r\n", 
	    (long)desc->port, opp->id, opp->caller, opp->req));
    
    opp++;
    if (opp >= desc->op_queue + INET_MAX_ASYNC)
	desc->opt = desc->op_queue;
    else
	desc->opt = opp;

    if (desc->opt == desc->oph)
	desc->opt = desc->oph = NULL;
    return 0;
}


/* send message:
**     {inet_async, Port, Ref, ok} 
*/
static int 
send_async_ok(ErlDrvPort port, ErlDrvTermData sender, int aid, ErlDrvTermData recipient)
{
    ErlDrvTermData spec[10];
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, sender);
    i = LOAD_INT(spec, i, aid);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 4);

    ASSERT(i == 10);

    return driver_send_term(port, recipient, spec, i);
}

/* send message:
**      {inet_async, Port, Ref, {error,Reason}}
*/
static int
send_async_error(ErlDrvPort port, ErlDrvTermData Port, int Ref,
		 ErlDrvTermData recipient, ErlDrvTermData Reason)
{
    ErlDrvTermData spec[14];
    int i = 0;

    i = 0;
    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, Port);
    i = LOAD_INT(spec, i, Ref);
    {
	i = LOAD_ATOM(spec, i, am_error);
	i = LOAD_ATOM(spec, i, Reason);
	i = LOAD_TUPLE(spec, i, 2);
    }
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i == 14);
    DEBUGF(("send_async_error %ld %ld\r\n", recipient, Reason));
    return driver_send_term(port, recipient, spec, i);
}


static int async_ok(inet_descriptor* desc)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_ok(desc->port, desc->dport, aid, caller);
}


static int async_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;
    return send_async_error(desc->port, desc->dport, aid, caller,
			    reason);
}

/* dequeue all operations */
static int async_error_am_all(inet_descriptor* desc, ErlDrvTermData reason)
{
    int req;
    int aid;
    ErlDrvTermData caller;

    while (deq_async(desc, &aid, &caller, &req) == 0) {
	send_async_error(desc->port, desc->dport, aid, caller,
			 reason);
    }
    return 0;
}


static int async_error(inet_descriptor* desc, int err)
{
    return async_error_am(desc, error_atom(err));
}

/* send:
**   {inet_reply, S, ok} 
*/

static int inet_reply_ok(inet_descriptor* desc)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData caller = desc->caller;
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i == 8);

    desc->caller = 0;
    return driver_send_term(desc->port, caller, spec, i);    
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error_am(inet_descriptor* desc, ErlDrvTermData reason)
{
    ErlDrvTermData spec[12];
    ErlDrvTermData caller = desc->caller;
    int i = 0;

    i = LOAD_ATOM(spec, i, am_inet_reply);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i == 12);
    desc->caller = 0;

    DEBUGF(("inet_reply_error_am %ld %ld\r\n", caller, reason));
    return driver_send_term(desc->port, caller, spec, i);
}

/* send:
**   {inet_reply, S, {error, Reason}} 
*/
static int inet_reply_error(inet_descriptor* desc, int err)
{
    return inet_reply_error_am(desc, error_atom(err));
}

/* 
** Deliver port data from buffer 
*/
static int inet_port_data(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF(("inet_port_data(%ld): len = %d\r\n", (long)desc->port, len));

    if ((desc->mode == INET_MODE_LIST) || (hsz > len))
	return driver_output2(desc->port, buf, len, NULL, 0);
    else if (hsz > 0)
	return driver_output2(desc->port, buf, hsz, buf+hsz, len-hsz);
    else
	return driver_output(desc->port, buf, len);
}

/* 
** Deliver port data from binary
*/
static int
inet_port_binary_data(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;

    DEBUGF(("inet_port_binary_data(%ld): offs=%d, len = %d\r\n", 
	    (long)desc->port, offs, len));

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) 
	return driver_output2(desc->port, bin->orig_bytes+offs, len, NULL, 0);
    else 
	return driver_output_binary(desc->port, bin->orig_bytes+offs, hsz,
				    bin, offs+hsz, len-hsz);
}

#ifdef USE_HTTP

#define HTTP_HDR_HASH_SIZE  53
#define HTTP_METH_HASH_SIZE 13

static char tspecial[128];

static char* http_hdr_strings[] = {
  "Cache-Control",
  "Connection",
  "Date",
  "Pragma",
  "Transfer-Encoding",
  "Upgrade",
  "Via",
  "Accept",
  "Accept-Charset",
  "Accept-Encoding",
  "Accept-Language",
  "Authorization",
  "From",
  "Host",
  "If-Modified-Since",
  "If-Match",
  "If-None-Match",
  "If-Range",
  "If-Unmodified-Since",
  "Max-Forwards",
  "Proxy-Authorization",
  "Range",
  "Referer",
  "User-Agent",
  "Age",
  "Location",
  "Proxy-Authenticate",
  "Public",
  "Retry-After",
  "Server",
  "Vary",
  "Warning",
  "Www-Authenticate",
  "Allow",
  "Content-Base",
  "Content-Encoding",
  "Content-Language",
  "Content-Length",
  "Content-Location",
  "Content-Md5",
  "Content-Range",
  "Content-Type",
  "Etag",
  "Expires",
  "Last-Modified",
  "Accept-Ranges",
  "Set-Cookie",
  "Set-Cookie2",
  "X-Forwarded-For",
  "Cookie",
  "Keep-Alive",
  "Proxy-Connection",
    NULL
};


static char* http_meth_strings[] = {
  "OPTIONS",
  "GET",
  "HEAD",
  "POST",
  "PUT",
  "DELETE",
  "TRACE",
    NULL
};

typedef struct http_atom {
  struct http_atom* next;   /* next in bucket */
  unsigned long h;          /* stored hash value */
  char* name;
  int   len;
  int index;                /* index in table + bit-pos */
  ErlDrvTermData atom;      /* erlang atom rep */
} http_atom_t;

static http_atom_t http_hdr_table[sizeof(http_hdr_strings)/sizeof(char*)];
static http_atom_t http_meth_table[sizeof(http_meth_strings)/sizeof(char*)];

static http_atom_t* http_hdr_hash[HTTP_HDR_HASH_SIZE];
static http_atom_t* http_meth_hash[HTTP_METH_HASH_SIZE];

static ErlDrvTermData am_http_eoh;
static ErlDrvTermData am_http_header;
static ErlDrvTermData am_http_request;
static ErlDrvTermData am_http_response;
static ErlDrvTermData am_http_error;
static ErlDrvTermData am_abs_path;
static ErlDrvTermData am_absoluteURI;
static ErlDrvTermData am_star;
static ErlDrvTermData am_undefined;
static ErlDrvTermData am_http;
static ErlDrvTermData am_https;
static ErlDrvTermData am_scheme;



#define CRNL(ptr) (((ptr)[0] == '\r') && ((ptr)[1] == '\n'))
#define NL(ptr)   ((ptr)[0] == '\n')
#define SP(ptr)   (((ptr)[0] == ' ') || ((ptr)[0] == '\t'))
#define is_tspecial(x) ((((x) > 32) && ((x) < 128)) ? tspecial[(x)] : 1)

#define hash_update(h,c) do { \
    unsigned long __g; \
    (h) = ((h) << 4) + (c); \
    if ((__g = (h) & 0xf0000000)) { \
       (h) ^= (__g >> 24); \
       (h) ^= __g; \
    } \
 } while(0)

static void http_hash(char* name, http_atom_t* entry,
		      http_atom_t** hash, int hsize)
{
  unsigned long h = 0;
  unsigned char* ptr = (unsigned char*) name;
  int ix;
  int len = 0;

  while(*ptr != '\0') {
    hash_update(h, *ptr);
    ptr++;
    len++;
  }
  ix = h % hsize;

  entry->next = hash[ix];
  entry->h    = h;
  entry->name = name;
  entry->len  = len;
  entry->atom = driver_mk_atom(name);
    
  hash[ix] = entry;
}

static http_atom_t* http_hash_lookup(unsigned char* name, int len,
				     unsigned long h,
				     http_atom_t** hash, int hsize)
{
  int ix = h % hsize;
  http_atom_t* ap = hash[ix];

  while (ap != NULL) {
    if ((ap->h == h) && (ap->len == len) && 
	(strncmp(ap->name, name, len) == 0))
      return ap;
    ap = ap->next;
  }
  return NULL;
}
     


static int http_init(void)
{
  int i;
  unsigned char* ptr;

  for (i = 0; i < 33; i++)
    tspecial[i] = 1;
  for (i = 33; i < 127; i++)
    tspecial[i] = 0;
  for (ptr = "()<>@,;:\\\"/[]?={} \t"; *ptr != '\0'; ptr++)
    tspecial[*ptr] = 1;

  am_http_eoh      = driver_mk_atom("http_eoh");
  am_http_header   = driver_mk_atom("http_header");
  am_http_request  = driver_mk_atom("http_request");
  am_http_response = driver_mk_atom("http_response");
  am_http_error    = driver_mk_atom("http_error");
  am_star          = driver_mk_atom("*");
  am_undefined     = driver_mk_atom("undefined");
  am_abs_path      = driver_mk_atom("abs_path");
  am_absoluteURI   = driver_mk_atom("absoluteURI");
  am_http          = driver_mk_atom("http");
  am_https         = driver_mk_atom("https");
  am_scheme        = driver_mk_atom("scheme");

  for (i = 0; i < HTTP_HDR_HASH_SIZE; i++)
    http_hdr_hash[i] = NULL;
  for (i = 0; http_hdr_strings[i] != NULL; i++) {
    http_hdr_table[i].index = i;
    http_hash(http_hdr_strings[i], 
	      &http_hdr_table[i], 
	      http_hdr_hash, HTTP_HDR_HASH_SIZE);
  }

  for (i = 0; i < HTTP_METH_HASH_SIZE; i++)
    http_meth_hash[i] = NULL;
  for (i = 0; http_meth_strings[i] != NULL; i++) {
    http_meth_table[i].index = i;
    http_hash(http_meth_strings[i],
	      &http_meth_table[i], 
	      http_meth_hash, HTTP_METH_HASH_SIZE);
  }
  return 0;
}

static int
http_response_message(tcp_descriptor* desc, int major, int minor, int status,
		      char* phrase, int phrase_len)
{
  int i = 0;
  ErlDrvTermData spec[27];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,{http_response,Version,Status,Phrase}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = LOAD_STRING(spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 4);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_response, S, Version, Status, Phrase} */
    i = LOAD_ATOM(spec, i,  am_http_response);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_INT(spec, i, status);
    i = LOAD_STRING(spec, i, phrase, phrase_len);
    i = LOAD_TUPLE(spec, i, 5);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

/*
** Handle URI syntax:
**
**  Request-URI    = "*" | absoluteURI | abs_path
**  absoluteURI    = scheme ":" *( uchar | reserved )
**  net_path       = "//" net_loc [ abs_path ]
**  abs_path       = "/" rel_path
**  rel_path       = [ path ] [ ";" params ] [ "?" query ]
**  path           = fsegment *( "/" segment )
**  fsegment       = 1*pchar
**  segment        = *pchar
**  params         = param *( ";" param )
**  param          = *( pchar | "/" )
**  query          = *( uchar | reserved )
**
**  http_URL       = "http:" "//" host [ ":" port ] [ abs_path ]
**
**  host           = <A legal Internet host domain name
**                   or IP address (in dotted-decimal form),
**                   as defined by Section 2.1 of RFC 1123>
**  port           = *DIGIT
**
**  {absoluteURI, <scheme>, <host>, <port>, <path+params+query>}
**       when <scheme> = http | https
**  {scheme, <scheme>, <chars>}
**       wheb <scheme> is something else then http or https
**  {abs_path,  <path>}
**
**  <string>  (unknown form)
**
*/

/* host [ ":" port ] [ abs_path ] */
static int
http_load_absoluteURI(ErlDrvTermData* spec, int i, ErlDrvTermData scheme,
		      char* uri_ptr, int uri_len)
{
  char* p;
  char* abs_path_ptr;
  int   abs_path_len;

  if ((p = memchr(uri_ptr, '/', uri_len)) == NULL) {
    /* host [":" port] */
    abs_path_ptr = "/";
    abs_path_len = 1;
  }
  else {
    int n = (p - uri_ptr);

    abs_path_ptr = p;
    abs_path_len = uri_len - n;
    uri_len = n;
  }
  i = LOAD_ATOM(spec, i, am_absoluteURI);
  i = LOAD_ATOM(spec, i, scheme);

  /* host[:port]  */
  if ((p = memchr(uri_ptr, ':', uri_len)) == NULL) {
    i = LOAD_STRING(spec, i, uri_ptr, uri_len);
    i = LOAD_ATOM(spec, i, am_undefined);
  }
  else {
    int n = (p - uri_ptr);
    int port = 0;

    i = LOAD_STRING(spec, i, uri_ptr, n);
    n = uri_len - (n+1);
    p++;
    while(n && isdigit((int) *p)) {
      port = port*10 + (*p - '0');
      n--;
      p++;
    }
    if ((n != 0) || (port == 0))
      i = LOAD_ATOM(spec, i, am_undefined);
    else
      i = LOAD_INT(spec, i, port);
  }
  i = LOAD_STRING(spec, i, abs_path_ptr, abs_path_len);
  i = LOAD_TUPLE(spec, i, 5);
  return i;
}

static int http_load_uri(ErlDrvTermData* spec, int i, char* uri_ptr, int uri_len)
{
  if ((uri_len == 1) && (uri_ptr[0] == '*'))
    i = LOAD_ATOM(spec, i, am_star);
  else if ((uri_len <= 1) || (uri_ptr[0] == '/')) {
    i = LOAD_ATOM(spec, i, am_abs_path);
    i = LOAD_STRING(spec, i, uri_ptr, uri_len);
    i = LOAD_TUPLE(spec, i, 2);
  }
  else {
    if ((uri_len>=7) && (STRNCASECMP(uri_ptr, "http://", 7) == 0)) {
      uri_len -= 7;
      uri_ptr += 7;
      return http_load_absoluteURI(spec, i, am_http, uri_ptr, uri_len);
    }
    else if ((uri_len>=8) && (STRNCASECMP(uri_ptr, "https://", 8) == 0)) {
      uri_len -= 8;
      uri_ptr += 8;    
      return http_load_absoluteURI(spec, i, am_https, uri_ptr,uri_len);
    }
    else {
      char* ptr;
      if ((ptr = memchr(uri_ptr, ':', uri_len)) == NULL)
	i = LOAD_STRING(spec, i, uri_ptr, uri_len);
      else {
	int slen = ptr - uri_ptr;
	i = LOAD_ATOM(spec, i, am_scheme);
	i = LOAD_STRING(spec, i, uri_ptr, slen);
	i = LOAD_STRING(spec, i, uri_ptr+(slen+1), uri_len-(slen+1));
	i = LOAD_TUPLE(spec, i, 3);
      }
    }
  }
  return i;
}

static int
http_request_message(tcp_descriptor* desc, http_atom_t* meth, char* meth_ptr,
		     int meth_len, char* uri_ptr, int uri_len,
		     int major, int minor)
{
  int i = 0;
  ErlDrvTermData spec[43];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async, S, Ref, {ok,{http_request,Meth,Uri,Version}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_request);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = LOAD_STRING(spec, i, meth_ptr, meth_len);
    i = http_load_uri(spec, i, uri_ptr, uri_len);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 43);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_request, S, Meth, Uri, Version} */
    i = LOAD_ATOM(spec, i,  am_http_request);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    if (meth != NULL)
      i = LOAD_ATOM(spec, i, meth->atom);
    else
      i = LOAD_STRING(spec, i, meth_ptr, meth_len);
    i = http_load_uri(spec, i, uri_ptr, uri_len);
    i = LOAD_INT(spec, i, major);
    i = LOAD_INT(spec, i, minor);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 5);
    ASSERT(i <= 43);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int
http_header_message(tcp_descriptor* desc, http_atom_t* name, char* name_ptr,
		    int name_len, char* value_ptr, int value_len)
{
  int i = 0;
  ErlDrvTermData spec[26];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,{http_header,Bit,Name,IValue,Value}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_header);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = LOAD_STRING(spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = LOAD_STRING(spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 5);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 26);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_header,S,Bit,Name,Code,Value} */
    i = LOAD_ATOM(spec, i,  am_http_header);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    if (name != NULL) {
      i = LOAD_INT(spec, i,  name->index+1);
      i = LOAD_ATOM(spec, i, name->atom);
    }
    else {
      i = LOAD_INT(spec, i,  0);
      i = LOAD_STRING(spec, i, name_ptr, name_len);
    }
    i = LOAD_ATOM(spec, i, am_undefined);
    i = LOAD_STRING(spec, i, value_ptr, value_len);
    i = LOAD_TUPLE(spec, i, 6);
    ASSERT(i <= 26);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int http_eoh_message(tcp_descriptor* desc)
{
  int i = 0;
  ErlDrvTermData spec[14];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{ok,http_eoh}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_ok);
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 14);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_eoh,S} */
    i = LOAD_ATOM(spec, i,  am_http_eoh);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_TUPLE(spec, i, 2);
    ASSERT(i <= 14);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

static int http_error_message(tcp_descriptor* desc, char* buf, int len)
{
  int i = 0;
  ErlDrvTermData spec[19];

  if (desc->inet.active == INET_PASSIVE) {
    /* {inet_async,S,Ref,{error,{http_error,Line}}} */
    int req;
    int aid;
    ErlDrvTermData caller;

    if (deq_async(INETP(desc), &aid, &caller, &req) < 0)
      return -1;
    i = LOAD_ATOM(spec, i,  am_inet_async);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_INT(spec, i,   aid);
    i = LOAD_ATOM(spec, i,  am_error);
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = LOAD_STRING(spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 19);
    return driver_send_term(desc->inet.port, caller, spec, i);
  }
  else {
    /* {http_error,S,Line} */
    i = LOAD_ATOM(spec, i,  am_http_error);
    i = LOAD_PORT(spec, i,  desc->inet.dport);
    i = LOAD_STRING(spec, i, buf, len);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 19);
    return driver_output_term(desc->inet.port, spec, i);
  }
}

/*
** load http message:
**  {http_eoh, S}                          - end of headers
**  {http_header,   S, Key, Value}         - Key = atom() | string()
**  {http_request,  S, Method,Url,Version}
**  {http_response, S, Version, Status, Message}
**  {http_error,    S, Error-Line}
*/
static int http_message(tcp_descriptor* desc, char* buf, int len)
{
  char* ptr = buf;
  int n = len;

  /* remove trailing CRNL (accept NL as well) */
  if ((n >= 2) && (buf[n-2] == '\r'))
    n -= 2;
  else if ((n >= 1) && (buf[n-1] == '\n'))
    n -= 1;

  if (desc->http_state == 0) {
    unsigned long h;
    http_atom_t* meth;
    char* meth_ptr;
    int   meth_len;
    int c;
    /* start-line = Request-Line | Status-Line */
    if (n == 0)
	return -1;
    h = 0;
    meth_ptr = ptr;
    while (n && !is_tspecial((unsigned char)*ptr)) {
      c = *ptr;
      hash_update(h, c);
      ptr++;
      n--;
    }
    if ((meth_len = (ptr - meth_ptr)) == 0)
      return -1;
    meth = http_hash_lookup(meth_ptr, meth_len, h,
			    http_meth_hash, HTTP_METH_HASH_SIZE);
    if (n) {
      if ((*ptr == '/') && (strncmp(buf, "HTTP", 4) == 0)) {
	int major  = 0;
	int minor  = 0;
	int status = 0;
	/* Status-Line = HTTP-Version SP 
	 *              Status-Code SP Reason-Phrase 
	 *              CRNL
	 * HTTP-Version   = "HTTP" "/" 1*DIGIT "." 1*DIGIT
	 */
	ptr++;
	n--;
	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  major = 10*major + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || (*ptr != '.'))
	  return -1;
	ptr++;
	n--;
	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  minor = 10*minor + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || !SP(ptr))
	  return -1;

	while(n && SP(ptr)) { ptr++; n--; }

	while(n && isdigit((int) *ptr)) {
	  status = 10*status + (*ptr - '0');
	  ptr++;
	  n--;
	}
	if (!n || !SP(ptr))
	  return -1;

	while(n && SP(ptr)) { ptr++; n--; }

	/* NOTE: the syntax allows empty reason phrases */
	desc->http_state++;

	return http_response_message(desc, major, minor, status,
				     (char*)ptr, n);
      }
      else if (SP(ptr)) {
	/* Request-Line = Method SP Request-URI SP HTTP-Version CRLF */
	char* uri_ptr;
	int   uri_len;
	int major  = 0;
	int minor  = 0;
	
	while(n && SP(ptr)) { ptr++; n--; }
	uri_ptr = ptr;
	while(n && !SP(ptr)) { ptr++; n--; }
	if ((uri_len = (ptr - uri_ptr)) == 0)
	  return -1;
	while(n && SP(ptr)) { ptr++; n--; }
	if (n == 0) {
	  desc->http_state++;
	  return http_request_message(desc, meth,
				      meth_ptr, meth_len,
				      uri_ptr, uri_len,
				      0, 9);
	}
	if (n < 8)
	  return -1;
	if (strncmp(ptr, "HTTP/", 5) != 0)
	  return -1;
	ptr += 5;
	n   -= 5;

	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  major = 10*major + (*ptr - '0');
	  ptr++;
	  n--;
	}

	if (!n || (*ptr != '.'))
	  return -1;
	ptr++;
	n--;

	if (!n || !isdigit((int) *ptr)) return -1;
	while(n && isdigit((int) *ptr)) {
	  minor = 10*minor + (*ptr - '0');
	  ptr++;
	  n--;
	}
	desc->http_state++;
	return http_request_message(desc, meth,
				    meth_ptr, meth_len,
				    uri_ptr, uri_len,
				    major, minor);
      }
    }
    return -1;
  }
  else {
    int up = 1;      /* make next char uppercase */
    http_atom_t* name;
    char* name_ptr;
    int   name_len;
    unsigned long h;

    if (n == 0) {
      /* end of headers */
      desc->http_state = 0;  /* reset state (for next request) */
      return http_eoh_message(desc);
    }
    h = 0;
    while(n && !is_tspecial((unsigned char)*ptr)) {
      int c = *ptr;
      if (up) {
	if (islower(c)) {
	  c = toupper(c);
	}
	up = 0;
      }
      else {
	if (isupper(c))
	  c = tolower(c);
	else if (c == '-')
	  up = 1;
      }
      *ptr = c;
      hash_update(h, c);
      ptr++;
      n--;
    }
    if (*ptr != ':') {
      /* Error case */
      return -1;
    }
    name_ptr = buf;
    name_len = (ptr - buf);
    name = http_hash_lookup(name_ptr, name_len, h,
			    http_hdr_hash, HTTP_HDR_HASH_SIZE);
    ptr++;
    n--;
    /* Skip white space */
    while(n && SP(ptr)) { ptr++; n--; }

    return http_header_message(desc, name, name_ptr, name_len,
			       ptr, n);
  }
}
#endif

/* 
** passive mode reply:
**        {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int inet_async_data(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller;
    int req;
    int aid;
    int i = 0;

    DEBUGF(("inet_async_data(%ld): len = %d\r\n", (long)desc->port, len));

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i, aid);

    i = LOAD_ATOM(spec, i, am_ok);
    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ASSERT(i == 15);
	desc->caller = 0;
	return driver_send_term(desc->port, caller, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	ErlDrvBinary* bin;
	int sz = len - hsz;
	int code;

	if ((bin = driver_alloc_binary(sz)) == NULL)
	    return async_error(desc, ENOMEM);
	memcpy(bin->orig_bytes, buf+hsz, sz);
	i = LOAD_BINARY(spec, i, bin, 0, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 2);
	i = LOAD_TUPLE(spec, i, 4);
	ASSERT(i <= 20);
	desc->caller = 0;
	code = driver_send_term(desc->port, caller, spec, i);
	driver_free_binary(bin);  /* must release binary */
	return code;
    }
}

/* 
** passive mode reply:
**        {inet_async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int 
inet_async_binary_data(inet_descriptor* desc, unsigned int phsz,
		       ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz + phsz;
    ErlDrvTermData spec[20];
    ErlDrvTermData caller = desc->caller;
    int aid;
    int req;
    int i = 0;

    DEBUGF(("inet_async_binary_data(%ld): offs=%d, len = %d\r\n", 
	    (long)desc->port, offs, len));

    if (deq_async(desc, &aid, &caller, &req) < 0)
	return -1;

    i = LOAD_ATOM(spec, i, am_inet_async);
    i = LOAD_PORT(spec, i, desc->dport);
    i = LOAD_INT(spec, i,  aid);

    i = LOAD_ATOM(spec, i, am_ok);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;
	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    ASSERT(i <= 20);    
    desc->caller = 0;
    return driver_send_term(desc->port, caller, spec, i);
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int tcp_message(inet_descriptor* desc, char* buf, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF(("tcp_message(%ld): len = %d\r\n", (long)desc->port, len));    

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	i = LOAD_STRING(spec, i, buf, len); /* => [H1,H2,...Hn] */ 
	i = LOAD_TUPLE(spec, i, 3);
	ASSERT(i <= 20);
	return driver_output_term(desc->port, spec, i);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	ErlDrvBinary* bin;
	int sz = len - hsz;
	int code;

	if ((bin = driver_alloc_binary(sz)) == NULL)
	    return async_error(desc, ENOMEM);
	memcpy(bin->orig_bytes, buf+hsz, sz);
	i = LOAD_BINARY(spec, i, bin, 0, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, buf, hsz);
	i = LOAD_TUPLE(spec, i, 3);
	ASSERT(i <= 20);
	code = driver_output_term(desc->port, spec, i);
	driver_free_binary(bin);  /* must release binary */
	return code;
    }
}

/* 
** active mode message:
**        {tcp, S, [H1,...Hsz | Data]}
*/
static int
tcp_binary_message(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[20];
    int i = 0;

    DEBUGF(("tcp_binary_message(%ld): len = %d\r\n", (long)desc->port, len)); 

    i = LOAD_ATOM(spec, i, am_tcp);
    i = LOAD_PORT(spec, i, desc->dport);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;

	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 20);
    return driver_output_term(desc->port, spec, i);
}

/*
** send:  active mode  {tcp_closed, S}
*/
static int tcp_closed_message(tcp_descriptor* desc)
{
    ErlDrvTermData spec[6];
    int i = 0;

    DEBUGF(("tcp_closed_message(%ld):\r\n", (long)desc->inet.port)); 

    i = LOAD_ATOM(spec, i, am_tcp_closed);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_TUPLE(spec, i, 2);
    ASSERT(i <= 6);
    return driver_output_term(desc->inet.port, spec, i);
}

/*
** send active message {tcp_error, S, Error}
*/
static int tcp_error_message(tcp_descriptor* desc, int err)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData am_err = error_atom(err);
    int i = 0;

    DEBUGF(("tcp_error_message(%ld): %d\r\n", (long)desc->inet.port, err)); 

    i = LOAD_ATOM(spec, i, am_tcp_error);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_ATOM(spec, i, am_err);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 8);
    return driver_output_term(desc->inet.port, spec, i);
}

/* 
** active mode message:
**        {udp, S, IP, Port, [H1,...Hsz | Data]}
*/
static int
udp_binary_message(inet_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    unsigned int hsz = desc->hsz;
    ErlDrvTermData spec[30];
    int i = 0;
    int alen;

    DEBUGF(("udp_binary_message(%ld): len = %d\r\n", (long)desc->port, len));

    i = LOAD_ATOM(spec, i, am_udp);
    i = LOAD_PORT(spec, i, desc->dport);
    
    alen = addrlen(desc->sfamily);
    i = load_ip_address(spec, i, desc->sfamily, bin->orig_bytes+offs+3);
    i = load_ip_port(spec, i, bin->orig_bytes+offs+1);
    
    offs += (alen + 3);
    len  -= (alen + 3);

    if ((desc->mode == INET_MODE_LIST) || (hsz > len)) {
	/* INET_MODE_LIST => [H1,H2,...Hn] */
	i = LOAD_STRING(spec, i, bin->orig_bytes+offs, len);
    }
    else {
	/* INET_MODE_BINARY => [H1,H2,...HSz | Binary] */
	int sz = len - hsz;

	i = LOAD_BINARY(spec, i, bin, offs+hsz, sz);
	if (hsz > 0)
	    i = LOAD_STRING_CONS(spec, i, bin->orig_bytes+offs, hsz);
    }
    i = LOAD_TUPLE(spec, i, 5);
    ASSERT(i <= 30);
    return driver_output_term(desc->port, spec, i);
}

/*
** send active message {udp_error, S, Error}
*/
static int udp_error_message(udp_descriptor* desc, int err)
{
    ErlDrvTermData spec[8];
    ErlDrvTermData am_err = error_atom(err);
    int i = 0;

    DEBUGF(("udp_error_message(%ld): %d\r\n", (long)desc->inet.port, err)); 

    i = LOAD_ATOM(spec, i, am_udp_error);
    i = LOAD_PORT(spec, i, desc->inet.dport);
    i = LOAD_ATOM(spec, i, am_err);
    i = LOAD_TUPLE(spec, i, 3);
    ASSERT(i <= 8);
    return driver_output_term(desc->inet.port, spec, i);
}

/*
** The fcgi header is 8 bytes. After that comes the data and
** possibly some padding.
** return length of the header (and total length int plen)
** return -1 when not enough bytes
** return -2 when error
*/
#define FCGI_VERSION_1		1

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


#define CDR_MAGIC "GIOP"

struct cdr_head {
    unsigned char magic[4];        /* 4 bytes must be 'GIOP' */
    unsigned char major;           /* major version */ 
    unsigned char minor;           /* minor version */
    unsigned char flags;           /* bit 0: 0 == big endian, 1 == little endian 
				      bit 1: 1 == more fragments follow
				   */
    unsigned char message_type;    /* message type ... */
    unsigned char message_size[4]; /* size in (flags bit 0 byte order) */
};


#define TPKT_VRSN 3

struct tpkt_head {
    unsigned char vrsn;             /* contains TPKT_VRSN */
    unsigned char reserved;
    unsigned char packet_length[2]; /* size incl header, big-endian (?) */
};


/* scan buffer for bit 7 */
static void scanbit8(inet_descriptor* desc, char* buf, int len)
{
    int c;

    if (!desc->bit8f || desc->bit8) return;
    c = 0;
    while(len--) c |= *buf++;
    desc->bit8 = ((c & 0x80) != 0);
}

/* 
** active=TRUE:
**  (NOTE! distribution MUST use active=TRUE, deliver=PORT)
**       deliver=PORT  {S, {data, [H1,..Hsz | Data]}}
**       deliver=TERM  {tcp, S, [H1..Hsz | Data]}
**
** active=FALSE:
**       {async, S, Ref, {ok,[H1,...Hsz | Data]}}
*/
static int tcp_reply_data(tcp_descriptor* desc, char* buf, int len)
{
    int code;

    /* adjust according to packet type */
    switch(desc->inet.htype) {
    case TCP_PB_1:  buf += 1; len -= 1; break;
    case TCP_PB_2:  buf += 2; len -= 2; break;
    case TCP_PB_4:  buf += 4; len -= 4; break;
    case TCP_PB_FCGI:
	len -= ((struct fcgi_head*)buf)->paddingLength;
	break;
    }

    scanbit8(INETP(desc), buf, len);

    if (desc->inet.deliver == INET_DELIVER_PORT)
        code = inet_port_data(INETP(desc), buf, len);
#ifdef USE_HTTP
    else if ((desc->inet.htype == TCP_PB_HTTP) ||
	     (desc->inet.htype == TCP_PB_HTTPH)) {
        if ((code = http_message(desc, buf, len)) < 0)
	    http_error_message(desc, buf, len);
	code = 0;
    }
#endif    
    else if (desc->inet.active == INET_PASSIVE)
        return inet_async_data(INETP(desc), buf, len);
    else
        code = tcp_message(INETP(desc), buf, len);

    if (code < 0)
	return code;
    if (desc->inet.active == INET_ONCE)
	desc->inet.active = INET_PASSIVE;
    return code;
}

static int
tcp_reply_binary_data(tcp_descriptor* desc, ErlDrvBinary* bin, int offs, int len)
{
    int code;

    /* adjust according to packet type */
    switch(desc->inet.htype) {
    case TCP_PB_1:  offs += 1; len -= 1; break;
    case TCP_PB_2:  offs += 2; len -= 2; break;
    case TCP_PB_4:  offs += 4; len -= 4; break;
    case TCP_PB_FCGI:
	len -= ((struct fcgi_head*)(bin->orig_bytes+offs))->paddingLength;
	break;
    }

    scanbit8(INETP(desc), bin->orig_bytes+offs, len);

    if (desc->inet.deliver == INET_DELIVER_PORT)
        code = inet_port_binary_data(INETP(desc), bin, offs, len);
#ifdef USE_HTTP
    else if ((desc->inet.htype == TCP_PB_HTTP) ||
	     (desc->inet.htype == TCP_PB_HTTPH)) {
        if ((code = http_message(desc, bin->orig_bytes+offs, len)) < 0)
	    http_error_message(desc, bin->orig_bytes+offs, len);
	code = 0;
    }
#endif
    else if (desc->inet.active == INET_PASSIVE)
	return inet_async_binary_data(INETP(desc), 0, bin, offs, len);
    else
	code = tcp_binary_message(INETP(desc), bin, offs, len);
    if (code < 0)
	return code;
    if (desc->inet.active == INET_ONCE)
	desc->inet.active = INET_PASSIVE;
    return code;
}


static int
udp_reply_binary_data(inet_descriptor* desc, unsigned int hsz,
		      ErlDrvBinary* bin, int offs, int len)
{
    int code;

    scanbit8(desc, bin->orig_bytes+offs, len);

    if (desc->active == INET_PASSIVE)
	return inet_async_binary_data(desc, hsz, bin, offs, len);
    else if (desc->deliver == INET_DELIVER_PORT)
	code = inet_port_binary_data(desc, bin, offs, len);
    else
	code = udp_binary_message(desc, bin, offs, len);
    if (code < 0)
	return code;
    if (desc->active == INET_ONCE)
	desc->active = INET_PASSIVE;
    return code;
}

/* ----------------------------------------------------------------------------

   INET

---------------------------------------------------------------------------- */

static int
sock_init(void) /* May be called multiple times. */
{
#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    static int res = -1; /* res < 0 == initialization never attempted */

    if (res >= 0)
	return res;

    wVersionRequested = MAKEWORD(2,0);
    if (!tcp_lookup_functions())
	goto error;
    if ((*winSock.WSAStartup)(wVersionRequested, &wsaData) != 0)
	goto error;
    if ((LOBYTE(wsaData.wVersion) != 2) || (HIBYTE(wsaData.wVersion) != 0))
	goto error;

    return res = 1;

 error:

    if (winSock.WSACleanup) {
	(*winSock.WSACleanup)();
    }
    winSock.WSACleanup = NULL;
    return res = 0;

#else
    return 1;
#endif
}


static int inet_init()
{
    int sz;

    if (!sock_init())
	goto error;

    buffer_stack_pos = 0;

#ifdef DEBUG
    tot_buf_allocated = 0;
    max_buf_allocated = 0;
    tot_buf_stacked = 0;
#endif
    am_ok           = driver_mk_atom("ok");
    am_tcp          = driver_mk_atom("tcp");
    am_tcp_closed   = driver_mk_atom("tcp_closed");
    am_tcp_error    = driver_mk_atom("tcp_error");
    am_udp          = driver_mk_atom("udp");
    am_udp_closed   = driver_mk_atom("udp_closed");
    am_udp_error    = driver_mk_atom("udp_error");
    am_error        = driver_mk_atom("error");
    am_inet_async   = driver_mk_atom("inet_async");
    am_inet_reply   = driver_mk_atom("inet_reply");
    am_timeout      = driver_mk_atom("timeout");
    am_closed       = driver_mk_atom("closed");
    am_empty_out_q  = driver_mk_atom("empty_out_q");

    inet_desc_size = INET_MAX_DESCRIPTOR;
    sz = sizeof(inet_descriptor*) * inet_desc_size;

    if ((inet_desc_table = (inet_descriptor**) ALLOC(sz)) == NULL)
	goto error;

    sys_memzero(inet_desc_table, sz);
    inet_desc_ix = 0;

    /* add tcp and udp drivers */
#ifdef _OSE_
    add_ose_tcp_drv_entry(&tcp_inet_driver_entry);
    add_ose_udp_drv_entry(&udp_inet_driver_entry);
#else
    add_driver_entry(&tcp_inet_driver_entry);
    add_driver_entry(&udp_inet_driver_entry);
#endif /* _OSE_ */
    /* remove the dummy inet driver */
    remove_driver_entry(&inet_driver_entry);
#ifdef USE_HTTP
    http_init();
#endif
    return 0;

 error:
#ifdef __WIN32__
    if (winSock.WSACleanup) {
	(*winSock.WSACleanup)();
    }
    sys_printf(CBUF,
	       "Failed to load or start Winsock2 (ws2_32.dll).\r\n\r\n"
	       "Erlang modules using TCP/IP (gen_tcp, ftp, and others),\r\n"
	       "will not work.  Furthermore, this Erlang node will not be able to\r\n"
	       "communicate with other Erlang nodes.\r\n\r\n"
	       "Refer to the Installation Guide for instructions about installing\r\n"
	       "Winsock2 on Windows 95.\r\n\r\n");
    send_error_to_logger(0);
    add_driver_entry(&dummy_tcp_driver_entry);
    add_driver_entry(&dummy_udp_driver_entry);
#endif
    remove_driver_entry(&inet_driver_entry);
    return -1;
}

/* XXX add this as a finish routine */
#if 0
static void inet_finish(arg)
void* arg;
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
    FREE(inet_desc_table);
}
#endif

/*
** Set a inaddr structure:
**  src = [P1,P0,X1,X2,.....]
**  dst points to a structure large enugh to keep any kind
**  of inaddr.
** *len is set to length of src on call
** and is set to actual length of dst on return
** return NULL on error and ptr after port address on success
*/
static char* inet_set_address(int family, inet_address* dst, char* src, int* len)
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
** dst is filled with [F,P1,P0,X1,....] 
** where F is the family code (coded)
** and *len is the length of dst on return 
** (suitable to deliver to erlang)
*/
static int inet_get_address(int family, char* dst, inet_address* src, int* len)
{
    short port;

    if ((family == AF_INET) && (*len >= sizeof(struct sockaddr_in))) {
	dst[0] = INET_AF_INET;
	port = sock_ntohs(src->sai.sin_port);
	put_int16(port, dst+1);
	sys_memcpy(dst+3, (char*)&src->sai.sin_addr, sizeof(struct in_addr));
	*len = 3 + sizeof(struct in_addr);
	return 0;
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if ((family == AF_INET6) && (*len >= sizeof(struct sockaddr_in6))) {
	dst[0] = INET_AF_INET6;
	port = sock_ntohs(src->sai6.sin6_port);
	put_int16(port, dst+1);
	sys_memcpy(dst+3, (char*)&src->sai6.sin6_addr,sizeof(struct in6_addr));
	*len = 3 + sizeof(struct in6_addr);
	return 0;
    }
#endif
    return -1;
}

static void desc_close(inet_descriptor* desc)
{
    if (desc->s != INVALID_SOCKET) {
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 0);
#endif
	sock_select(desc, FD_READ | FD_WRITE | FD_CLOSE, 0);
	sock_close(desc->s);
	desc->s = INVALID_SOCKET;
	sock_close_event(desc->event);
	desc->event = INVALID_EVENT;
	desc->event_mask = 0;
    }
}

static void desc_close_read(inet_descriptor* desc)
{
    if (desc->s != INVALID_SOCKET) {
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 0);
#endif
	sock_select(desc, FD_READ | FD_CLOSE, 0);
    }
}


static int erl_inet_close(inet_descriptor* desc)
{
    free_subscribers(&desc->empty_out_q_subs);
    if ((desc->prebound == 0) && (desc->state & INET_F_OPEN)) {
	desc_close(desc);
	desc->state = INET_STATE_CLOSED;
    } else if (desc->prebound && (desc->s != INVALID_SOCKET)) {
	sock_select(desc, FD_READ | FD_WRITE | FD_CLOSE, 0);
	desc->event_mask = 0;
    }
    return 0;
}


static int inet_ctl_open(inet_descriptor* desc, int domain, int type,
			 char** rbuf, int rsize)
{
    if (desc->state != INET_STATE_CLOSED)
	return ctl_xerror(EXBADSEQ, rbuf, rsize);
    if ((desc->s = sock_open(domain, type, 0)) == INVALID_SOCKET)
	return ctl_error(sock_errno(), rbuf, rsize);
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return ctl_error(sock_errno(), rbuf, rsize);
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 1);
#endif
    desc->state = INET_STATE_OPEN;
    desc->stype = type;
    desc->sfamily = domain;
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}


/* as inet_open but pass in an open socket (MUST BE OF RIGHT TYPE) */
static int inet_ctl_fdopen(inet_descriptor* desc, int domain, int type,
			   SOCKET s, char** rbuf, int rsize)
{
    inet_address name;
    int sz = sizeof(name);

    /* check that it is a socket and that the socket is bound */
    if (sock_name(s, (struct sockaddr*) &name, &sz) == SOCKET_ERROR)
	return ctl_error(sock_errno(), rbuf, rsize);
    desc->s = s;
    if ((desc->event = sock_create_event(desc)) == INVALID_EVENT)
	return ctl_error(sock_errno(), rbuf, rsize);
    SET_NONBLOCKING(desc->s);
#ifdef __WIN32__
	driver_select(desc->port, desc->event, DO_READ, 1);
#endif
    desc->state = INET_STATE_BOUND; /* assume bound */
    if (type == SOCK_STREAM) { /* check if connected */
	sz = sizeof(name);
	if (sock_peer(s, (struct sockaddr*) &name, &sz) != SOCKET_ERROR)
	    desc->state = INET_STATE_CONNECTED;
    }

    desc->prebound = 1; /* used to prevent a real close since
			 * the fd probably comes from an 
			 * external wrapper program, so it is
			 * not certain that we can open it again */
    desc->stype = type;
    desc->sfamily = domain;
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}

/*
**  store interface info as: (bytes)
**  [Len] Name(Len) Flags(1) addr(4) baddr(4) mask(4) bw(4)
*/
struct addr_if {
    char name[INET_IFNAMSIZ];
    long           flags;        /* coded flags */
    struct in_addr addr;         /* interface address */
    struct in_addr baddr;        /* broadcast address */
    struct in_addr mask;         /* netmask */
};


#ifndef SIOCGIFNETMASK
static struct in_addr net_mask(in)
struct in_addr in;
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
#endif

#if defined(__WIN32__) && defined(SIO_GET_INTERFACE_LIST)

/* format address in dot notation */
static char* fmt_addr(unsigned long x, char* ptr)
{
    int i;
    for (i = 0; i < 4; i++) {
	int nb[3];
	int y = (x >> 24) & 0xff;
	x <<= 8;
	nb[2] = y % 10; y /= 10;
	nb[1] = y % 10; y /= 10;
	nb[0] = y % 10; y /= 10;
	switch((nb[2]>0 ? 3 : (nb[1]>0 ? 2 : 1))) {
	case 3:  *ptr++ = nb[2] + '0';
	case 2:  *ptr++ = nb[1] + '0';
	case 1:  *ptr++ = nb[0] + '0';
	}
	*ptr++ = '.';
    }
    *(ptr-1) = '\0';
    return ptr;
}

static int parse_addr(char* ptr, int n, long* x)
{
    long addr = 0;
    int  dots = 0;
    int  digs = 0;

    while(n--) {
	switch(*ptr) {
	case '0': case '1': case '2':case '3':case '4':case '5':
	case '6': case '7': case '8':case '9':
	    n = n*10 + *ptr++ - '0';
	    digs++;
	    break;
	case '.':
	    if ((dots>2) || (digs==0) || (digs > 3) || (n > 0xff)) return -1;
	    dots++;
	    digs = 0;
	    addr = (addr << 8) | n;
	    n = 0;
	    ptr++;
	    break;
	default:
	    return -1;
	}
    }
    if ((dots != 3) || (digs==0) || (digs > 3) || (n > 0xff)) return -1;
    addr = (addr << 8) | n;
    *x = addr;
    return 0;
}

#endif

#define buf_check(ptr, end, n) \
do { if ((end)-(ptr) < (n)) goto error; } while(0)

static char* sockaddr_to_buf(struct sockaddr* addr, char* ptr, char* end)
{
    if (addr->sa_family == AF_INET || addr->sa_family == 0) {
	struct in_addr a;
	buf_check(ptr,end,sizeof(struct in_addr));
	a = ((struct sockaddr_in*) addr)->sin_addr;
	sys_memcpy(ptr, (char*)&a, sizeof(struct in_addr));
	return ptr + sizeof(struct in_addr);
    }
#if defined(HAVE_IN6) && defined(AF_INET6)
    else if (addr->sa_family == AF_INET6) {
	struct in6_addr a;
	buf_check(ptr,end,sizeof(struct in6_addr));
	a = ((struct sockaddr_in6*) addr)->sin6_addr;
	sys_memcpy(ptr, (char*)&a, sizeof(struct in6_addr));
	return ptr + sizeof(struct in6_addr);
    }
#endif
 error:
    return NULL;

}

static char* buf_to_sockaddr(char* ptr, char* end, struct sockaddr* addr)
{
    buf_check(ptr,end,sizeof(struct in_addr));
    sys_memcpy((char*) &((struct sockaddr_in*)addr)->sin_addr, ptr,
	       sizeof(struct in_addr));
    addr->sa_family = AF_INET;
    return ptr +  sizeof(struct in_addr);

 error:
    return NULL;
}



#if defined(__WIN32__) && defined(SIO_GET_INTERFACE_LIST)

static int inet_ctl_getiflist(inet_descriptor* desc, char** rbuf, int rsize)
{
    char  buf[BUFSIZ];
    char sbuf[BUFSIZ];
    char* sptr;
    INTERFACE_INFO* ifp;
    DWORD len;
    int n;
    int err;

    ifp = (INTERFACE_INFO*) buf;
    err = (*winSock.WSAIoctl)(desc->s, SIO_GET_INTERFACE_LIST, NULL, 0,
			      (LPVOID) ifp, BUFSIZ, (LPDWORD) &len,
			      NULL, NULL);
    if (err == SOCKET_ERROR)
	return ctl_error(sock_errno(), rbuf, rsize);

    n = (len + sizeof(INTERFACE_INFO) - 1) / sizeof(INTERFACE_INFO);
    sptr = sbuf;

    while(n--) {
	if (((struct sockaddr*)&ifp->iiAddress)->sa_family == desc->sfamily) {
	    struct in_addr sina = ((struct sockaddr_in*)&ifp->iiAddress)->sin_addr;
	    /* discard INADDR_ANY interface address */
	    if (sina.s_addr != INADDR_ANY)
		sptr = fmt_addr(sina.s_addr, sptr);
	}
	ifp++;
    }
    return ctl_reply(INET_REP_OK, sbuf, sptr - sbuf, rbuf, rsize);
}


/* input is an ip-address in string format i.e A.B.C.D 
** scan the INTERFACE_LIST to get the options 
*/
static int inet_ctl_ifget(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{

    char sbuf[BUFSIZ];
    char* sptr;
    char* s_end = sbuf + BUFSIZ;
    int namlen;
    int   err;
    INTERFACE_INFO* ifp;
    int n;
    long namaddr;

    if ((len == 0) || ((namlen = buf[0]) > len))
	goto error;
    if (parse_addr(buf+1, namlen, &namaddr) < 0)
	goto error;
    buf += (namlen+1);
    len -= (namlen+1);

    ifp = (INTERFACE_INFO*) buf;
    err = (*winSock.WSAIoctl)(desc->s, SIO_GET_INTERFACE_LIST, NULL, 0,
			      (LPVOID) ifp, BUFSIZ, (LPDWORD) &len, 
			      NULL, NULL);
    if (err == SOCKET_ERROR)
	return ctl_error(sock_errno(), rbuf, rsize);

    n = (len + sizeof(INTERFACE_INFO) - 1) / sizeof(INTERFACE_INFO);

    /* find interface */
    while(n--) {
	if (((struct sockaddr_in*)&ifp->iiAddress)->sin_addr.s_addr == namaddr)
	    break;
	ifp++;
    }
    if (n == 0)
	goto error;

    sptr = sbuf;

    while (len--) {
	switch(*buf++) {
	case INET_IFOPT_ADDR:
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_ADDR;
	    if ((sptr = sockaddr_to_buf((struct sockaddr *)&ifp->iiAddress,
					sptr, s_end)) == NULL)
		goto error;
	    break;

	case INET_IFOPT_HWADDR:
	    break;

	case INET_IFOPT_BROADADDR:
#ifdef SIOCGIFBRDADDR
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_BROADADDR;
	    *sptr++ = INET_IFOPT_BROADADDR;
	    if ((sptr=sockaddr_to_buf((struct sockaddr *)
				      &ifp->iiBroadcastAddress,sptr,s_end))
		== NULL)
		goto error;
#endif
	    break;
	    
	case INET_IFOPT_DSTADDR:
	    break;

	case INET_IFOPT_NETMASK:
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_NETMASK;
	    if ((sptr = sockaddr_to_buf((struct sockaddr *)
					&ifp->iiNetmask,sptr,s_end)) == NULL)
		goto error;
	    break;

	case INET_IFOPT_MTU:
	    break;

	case INET_IFOPT_FLAGS: {
	    long eflags = 0;
	    int flags = ifp->iiFlags;
	    /* just enumerate the interfaces (no names) */

	    /* translate flags */
	    if (flags & IFF_UP)
		eflags |= INET_IFF_UP;
	    if (flags & IFF_BROADCAST)
		eflags |= INET_IFF_BROADCAST;
	    if (flags & IFF_LOOPBACK)
		eflags |= INET_IFF_LOOPBACK;
	    if (flags & IFF_POINTTOPOINT)
		eflags |= INET_IFF_POINTTOPOINT;
	    if (flags & IFF_UP) /* emulate runnign ? */
		eflags |= INET_IFF_RUNNING;
	    if (flags & IFF_MULTICAST)
		eflags |= INET_IFF_MULTICAST;

	    buf_check(sptr, s_end, 5);
	    *sptr++ = INET_IFOPT_FLAGS;
	    put_int32(eflags, sptr);
	    sptr += 4;
	    break;
	}
	default:
	    goto error;
	}
    }
    return ctl_reply(INET_REP_OK, sbuf, sptr - sbuf, rbuf, rsize);

 error:
    return ctl_error(EINVAL, rbuf, rsize);
}

/* not supported */
static int inet_ctl_ifset(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}


#elif defined(SIOCGIFCONF) && defined(SIOCSIFFLAGS)
/* cygwin has SIOCGIFCONF but not SIOCSIFFLAGS (Nov 2002) */

#define VOIDP(x) ((void*)(x))
#if defined(AF_LINK) && !defined(NO_SA_LEN)
#define SIZEA(p) (((p).sa_len > sizeof(p)) ? (p).sa_len : sizeof(p))
#else
#define SIZEA(p) (sizeof (p))
#endif


static int inet_ctl_getiflist(inet_descriptor* desc, char** rbuf, int rsize)
{
    struct ifconf ifc;
    struct ifreq *ifr;
    char *buf;
    char *sbuf;
    char* cp;
    char* cplim;
    char* sp;
    int buflen;
    int res;
    
    buf = NULL;
    buflen = 0;
    
    /* Courtesy of Per Bergqvist */
    
    /* loop until we get all available interfaces */
    do {
	buflen += 4096;
	buf = (char *)REALLOC(buf,buflen);
	
	ifc.ifc_len = buflen;
	ifc.ifc_buf = buf;
	
	if (ioctl(desc->s, SIOCGIFCONF, (char *)&ifc) < 0) {
	    FREE(buf);
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
    } while ( (buflen-ifc.ifc_len) < sizeof(struct ifreq) );
    
    sp = sbuf = ALLOC(buflen);
    cplim = buf + ifc.ifc_len; /* skip over if's with big ifr_addr's */
    for (cp = buf; (cp < cplim); 
	 cp += sizeof(ifr->ifr_name)+SIZEA(ifr->ifr_addr)) {
	ifr = (struct ifreq *) VOIDP(cp);
	strncpy(sp, ifr->ifr_name, IFNAMSIZ);
	sp[IFNAMSIZ] = '\0';
	while (*sp != '\0')
	    sp++;
	sp++;
    }
    res = ctl_reply(INET_REP_OK, sbuf, sp - sbuf, rbuf, rsize);
    FREE(buf);
    FREE(sbuf);
    return res;
}



static int inet_ctl_ifget(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{
    char sbuf[BUFSIZ];
    char* sptr;
    char* s_end = sbuf + BUFSIZ;
    struct ifreq ifreq;
    int namlen;

    if ((len == 0) || ((namlen = buf[0]) > len))
	goto error;
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);
    sys_memcpy(ifreq.ifr_name, buf+1, 
	       (namlen > IFNAMSIZ) ? IFNAMSIZ : namlen);
    buf += (namlen+1);
    len -= (namlen+1);
    sptr = sbuf;

    while (len--) {
	switch(*buf++) {
	case INET_IFOPT_ADDR:
	    if (ioctl(desc->s, SIOCGIFADDR, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_ADDR;
	    if ((sptr = sockaddr_to_buf(&ifreq.ifr_addr, sptr, s_end)) == NULL)
		goto error;
	    break;

	case INET_IFOPT_HWADDR: {
#ifdef SIOCGIFHWADDR
	    if (ioctl(desc->s, SIOCGIFHWADDR, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 1+IFHWADDRLEN);
	    *sptr++ = INET_IFOPT_HWADDR;
	    /* raw memcpy (fix include autoconf later) */
	    sys_memcpy(sptr, (char*)(&ifreq.ifr_hwaddr.sa_data), IFHWADDRLEN);
	    sptr += IFHWADDRLEN;
#endif
	    break;
	}


	case INET_IFOPT_BROADADDR:
#ifdef SIOCGIFBRDADDR
	    if (ioctl(desc->s, SIOCGIFBRDADDR, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_BROADADDR;
	    if ((sptr=sockaddr_to_buf(&ifreq.ifr_broadaddr,sptr,s_end)) == NULL)
		goto error;
#endif
	    break;
	    
	case INET_IFOPT_DSTADDR:
#ifdef SIOCGIFDSTADDR	    
	    if (ioctl(desc->s, SIOCGIFDSTADDR, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_DSTADDR;
	    if ((sptr = sockaddr_to_buf(&ifreq.ifr_dstaddr,sptr,s_end)) == NULL)
		goto error;
#endif
	    break;

	case INET_IFOPT_NETMASK:
#if defined(SIOCGIFNETMASK)
	    if (ioctl(desc->s, SIOCGIFNETMASK, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 1);
	    *sptr++ = INET_IFOPT_NETMASK;
#if defined(ifr_netmask)
	    sptr = sockaddr_to_buf(&ifreq.ifr_netmask,sptr,s_end);
#else
	    /* SIOCGNETMASK exist but not macro ??? */
	    sptr = sockaddr_to_buf(&ifreq.ifr_addr,sptr,s_end);
#endif
	    if (sptr == NULL)
		goto error;
#else
	    if (ioctl(desc->s, SIOCGIFADDR, (char *)&ifreq) < 0)
		break;
	    else {
		struct sockadd_in* ap;
		/* emulate netmask,
		 * (wasted stuff since noone uses classes)
		 */
		buf_check(sptr, s_end, 1);
		*sptr++ = INET_IFOPT_NETMASK;
		ap = (struct sockaddr_in*) VOIDP(&ifreq.ifr_addr);
		ap->sin_addr = net_mask(ap->sin_addr);
		if ((sptr = sockaddr_to_buf(&ifreq.ifr_addr,sptr,s_end)) == NULL)
		    goto error;
	    }
#endif
	    break;

	case INET_IFOPT_MTU: {
#if defined(SIOCGIFMTU) && defined(ifr_mtu)
	    int n;

	    if (ioctl(desc->s, SIOCGIFMTU, (char *)&ifreq) < 0)
		break;
	    buf_check(sptr, s_end, 5);
	    *sptr++ = INET_IFOPT_MTU;
	    n = ifreq.ifr_mtu;
	    put_int32(n, sptr);
	    sptr += 4;
#endif
	    break;
	}

	case INET_IFOPT_FLAGS: {
	    int flags;
	    int eflags = 0;

	    if (ioctl(desc->s, SIOCGIFFLAGS, (char*)&ifreq) < 0)
		flags = 0;
	    else
		flags = ifreq.ifr_flags;
	    /* translate flags */
	    if (flags & IFF_UP)
		eflags |= INET_IFF_UP;
	    if (flags & IFF_BROADCAST)
		eflags |= INET_IFF_BROADCAST;
	    if (flags & IFF_LOOPBACK)
		eflags |= INET_IFF_LOOPBACK;	
	    if (flags & IFF_POINTOPOINT)
		eflags |= INET_IFF_POINTTOPOINT;
	    if (flags & IFF_RUNNING)
		eflags |= INET_IFF_RUNNING;
	    if (flags & IFF_MULTICAST)
		eflags |= INET_IFF_MULTICAST;

	    buf_check(sptr, s_end, 5);
	    *sptr++ = INET_IFOPT_FLAGS;
	    put_int32(eflags, sptr);
	    sptr += 4;
	    break;
	}
	default:
	    goto error;
	}
    }
    return ctl_reply(INET_REP_OK, sbuf, sptr - sbuf, rbuf, rsize);

 error:
    return ctl_error(EINVAL, rbuf, rsize);
}

/* FIXME: temporary hack */
#ifndef IFHWADDRLEN
#define IFHWADDRLEN 6
#endif

static int inet_ctl_ifset(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{
    struct ifreq ifreq;
    int namlen;
    char* b_end = buf + len;

    if ((len == 0) || ((namlen = buf[0]) > len))
	goto error;
    sys_memset(ifreq.ifr_name, '\0', IFNAMSIZ);
    sys_memcpy(ifreq.ifr_name, buf+1, 
	       (namlen > IFNAMSIZ) ? IFNAMSIZ : namlen);
    buf += (namlen+1);
    len -= (namlen+1);

    while(buf < b_end) {
	switch(*buf++) {
	case INET_IFOPT_ADDR:
	    if ((buf = buf_to_sockaddr(buf, b_end, &ifreq.ifr_addr)) == NULL)
		goto error;
	    (void) ioctl(desc->s, SIOCSIFADDR, (char*)&ifreq);
	    break;

	case INET_IFOPT_HWADDR: 
	    buf_check(buf, b_end, IFHWADDRLEN);
#ifdef SIOCSIFHWADDR
	    /* raw memcpy (fix include autoconf later) */
	    sys_memcpy((char*)(&ifreq.ifr_hwaddr.sa_data), buf, IFHWADDRLEN);

	    (void) ioctl(desc->s, SIOCSIFHWADDR, (char *)&ifreq);
#endif
	    buf += IFHWADDRLEN;
	    break;


	case INET_IFOPT_BROADADDR:
#ifdef SIOCSIFBRDADDR
	    if ((buf = buf_to_sockaddr(buf, b_end, &ifreq.ifr_broadaddr)) == NULL)
		goto error;
	    (void) ioctl(desc->s, SIOCSIFBRDADDR, (char *)&ifreq); 
#endif
	    break;

	case INET_IFOPT_DSTADDR:
#ifdef SIOCSIFDSTADDR
	    if ((buf = buf_to_sockaddr(buf, b_end, &ifreq.ifr_dstaddr)) == NULL)
		goto error;
	    (void) ioctl(desc->s, SIOCSIFDSTADDR, (char *)&ifreq);
#endif
	    break;

	case INET_IFOPT_NETMASK:
#ifdef SIOCSIFNETMASK

#if defined(ifr_netmask)
	    buf = buf_to_sockaddr(buf,b_end, &ifreq.ifr_netmask);
#else
	    buf = buf_to_sockaddr(buf,b_end, &ifreq.ifr_addr);
#endif
	    if (buf == NULL)
		goto error;
	    (void) ioctl(desc->s, SIOCSIFNETMASK, (char *)&ifreq);
#endif
	    break;

	case INET_IFOPT_MTU:
	    buf_check(buf, b_end, 4);
#if defined(SIOCSIFMTU) && defined(ifr_mtu)
	    ifreq.ifr_mtu = get_int32(buf);
	    (void) ioctl(desc->s, SIOCSIFMTU, (char *)&ifreq);
#endif
	    buf += 4;
	    break;

	case INET_IFOPT_FLAGS: {
	    int flags0;
	    int flags;
	    int eflags;

	    buf_check(buf, b_end, 4);
	    eflags = get_int32(buf);

	    /* read current flags */
	    if (ioctl(desc->s, SIOCGIFFLAGS, (char*)&ifreq) < 0)
		flags0 = flags = 0;
	    else
		flags0 = flags = ifreq.ifr_flags;

	    /* update flags */
	    if (eflags & INET_IFF_UP)            flags |= IFF_UP;
	    if (eflags & INET_IFF_DOWN)          flags &= ~IFF_UP;
	    if (eflags & INET_IFF_BROADCAST)     flags |= IFF_BROADCAST;
	    if (eflags & INET_IFF_NBROADCAST)    flags &= ~IFF_BROADCAST;
	    if (eflags & INET_IFF_POINTTOPOINT)  flags |= IFF_POINTOPOINT;
	    if (eflags & INET_IFF_NPOINTTOPOINT) flags &= ~IFF_POINTOPOINT;

	    if (flags != flags0) {
		ifreq.ifr_flags = flags;
		(void) ioctl(desc->s, SIOCSIFFLAGS, (char*)&ifreq);
	    }
	    buf += 4;
	    break;
	}

	default:
	    goto error;
	}
    }
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);

 error:
    return ctl_error(EINVAL, rbuf, rsize);
}

#else


static int inet_ctl_getiflist(inet_descriptor* desc, char** rbuf, int rsize)
{
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}


static int inet_ctl_ifget(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
}


static int inet_ctl_ifset(inet_descriptor* desc, char* buf, int len,
			  char** rbuf, int rsize)
{
    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
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

/* set socket options:
** return -1 on error
**         0 if ok
**         1 if ok force deliver of queued data
*/
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
    int old_htype = desc->htype;
    int old_active = desc->active;

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
	    DEBUGF(("inet_set_opts(%ld): s=%d, HEADER=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    desc->hsz = ival;
	    continue;

	case INET_LOPT_MODE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, MODE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->mode = ival;
	    continue;

	case INET_LOPT_DELIVER:
	    DEBUGF(("inet_set_opts(%ld): s=%d, DELIVER=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->deliver = ival;
	    continue;
	    
	case INET_LOPT_BUFFER:
	    DEBUGF(("inet_set_opts(%ld): s=%d, BUFFER=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    if (ival > INET_MAX_BUFFER)  ival = INET_MAX_BUFFER;
	    else if (ival < INET_MIN_BUFFER) ival = INET_MIN_BUFFER;
	    desc->bufsz = ival;
	    continue;

	case INET_LOPT_ACTIVE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, ACTIVE=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    desc->active = ival;
	    continue;

	case INET_LOPT_PACKET:
	    DEBUGF(("inet_set_opts(%ld): s=%d, PACKET=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->htype = ival;
	    continue;

	case INET_LOPT_PACKET_SIZE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, PACKET_SIZE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->psize = (unsigned int)ival;
	    continue;

	case INET_LOPT_EXITONCLOSE:
	    DEBUGF(("inet_set_opts(%ld): s=%d, EXITONCLOSE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    desc->exitf = ival;
	    continue;

	case INET_LOPT_BIT8:
	    DEBUGF(("inet_set_opts(%ld): s=%d, BIT8=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    switch(ival) {
	    case INET_BIT8_ON:
		desc->bit8f = 1;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_OFF:
		desc->bit8f = 0;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_CLEAR:
		desc->bit8f = 1;
		desc->bit8  = 0;
		break;
	    case INET_BIT8_SET:
		desc->bit8f = 1;
		desc->bit8  = 1;
		break;
	    }
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		if (ival < 0) ival = 0;
		else if (ival > INET_MAX_BUFFER*2) ival = INET_MAX_BUFFER*2;
		if (tdesc->low > ival)
		    tdesc->low = ival;
		tdesc->high = ival;
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		if (ival < 0) ival = 0;
		else if (ival > INET_MAX_BUFFER) ival = INET_MAX_BUFFER;
		if (tdesc->high < ival)
		    tdesc->high = ival;
		tdesc->high = ival;
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		tdesc->send_timeout = ival;
	    }
	    continue;

	case INET_LOPT_TCP_DELAY_SEND:
	    if (desc->stype == SOCK_STREAM) {
		tcp_descriptor* tdesc = (tcp_descriptor*) desc;
		tdesc->fdelay_send = ival;
	    }
	    continue;
	    

	case INET_OPT_REUSEADDR: 
#ifdef __WIN32__
	    continue;  /* Bjorn says */
#else
	    type = SO_REUSEADDR;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_REUSEADDR=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    break;
#endif
	case INET_OPT_KEEPALIVE: type = SO_KEEPALIVE;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_KEEPALIVE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_DONTROUTE: type = SO_DONTROUTE;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_DONTROUTE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_BROADCAST: type = SO_BROADCAST;
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_BROADCAST=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    break;
	case INET_OPT_OOBINLINE: type = SO_OOBINLINE; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_OOBINLINE=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;
	case INET_OPT_SNDBUF:    type = SO_SNDBUF; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_SNDBUF=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    /* 
	     * Setting buffer sizes in VxWorks gives unexpected results
	     * our workaround is to leave it at default.
	     */
#ifdef VXWORKS
	    goto skip_os_setopt;
#else
	    break;
#endif
	case INET_OPT_RCVBUF:    type = SO_RCVBUF; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_RCVBUF=%d\r\n",
		    (long)desc->port, desc->s, ival));
#ifdef VXWORKS
	    goto skip_os_setopt;
#else
	    break;
#endif
	case INET_OPT_LINGER:    type = SO_LINGER; 
	    if (len < 4)
		return -1;
	    li_val.l_onoff = ival;
	    li_val.l_linger = get_int32(ptr);
	    ptr += 4;
	    len -= 4;
	    arg_ptr = (char*) &li_val;
	    arg_sz = sizeof(li_val);
	    DEBUGF(("inet_set_opts(%ld): s=%d, SO_LINGER=%d,%d",
		    (long)desc->port, desc->s, li_val.l_onoff,li_val.l_linger));
	    break;
	case TCP_OPT_NODELAY:
	    proto = IPPROTO_TCP; 
	    type = TCP_NODELAY; 
	    DEBUGF(("inet_set_opts(%ld): s=%d, TCP_NODELAY=%d\r\n",
		    (long)desc->port, desc->s, ival));
	    break;

#ifdef HAVE_MULTICAST_SUPPORT

	case UDP_OPT_MULTICAST_TTL:
	    proto = IPPROTO_IP;
	    type = IP_MULTICAST_TTL;
	    DEBUGF(("inet_set_opts(%ld): s=%d, IP_MULTICAST_TTL=%d\r\n",
		    (long)desc->port,desc->s,ival));
	    break;

	case UDP_OPT_MULTICAST_LOOP:
	    proto = IPPROTO_IP;
	    type = IP_MULTICAST_LOOP;
	    DEBUGF(("inet_set_opts(%ld): s=%d, IP_MULTICAST_LOOP=%d\r\n",
		    (long)desc->port,desc->s,ival));
	    break;

	case UDP_OPT_MULTICAST_IF:
	    proto = IPPROTO_IP;
	    type = IP_MULTICAST_IF;
	    DEBUGF(("inet_set_opts(%ld): s=%d, IP_MULTICAST_IF=%x\r\n",
		    (long)desc->port, desc->s, ival));
	    ival = sock_htonl(ival);
	    break;

	case UDP_OPT_ADD_MEMBERSHIP:
	    proto = IPPROTO_IP;
	    type = IP_ADD_MEMBERSHIP;
	    DEBUGF(("inet_set_opts(%ld): s=%d, IP_ADD_MEMBERSHIP=%d\r\n",
		    (long)desc->port, desc->s,ival));
	    goto L_set_mreq;
	    
	case UDP_OPT_DROP_MEMBERSHIP:
	    proto = IPPROTO_IP;
	    type = IP_DROP_MEMBERSHIP;
	    DEBUGF(("inet_set_opts(%ld): s=%d, IP_DROP_MEMBERSHIP=%x\r\n",
		    (long)desc->port, desc->s, ival));
	L_set_mreq:
	    mreq_val.imr_multiaddr.s_addr = sock_htonl(ival);
	    ival = get_int32(ptr);
	    mreq_val.imr_interface.s_addr = sock_htonl(ival);
	    ptr += 4;
	    len -= 4;
	    arg_ptr = (char*)&mreq_val;
	    arg_sz = sizeof(mreq_val);
	    break;

#endif /* HAVE_MULTICAST_SUPPORT */

	default:
	    return -1;
	}

#ifdef DEBUG
	{ 
	    int res =
#endif
		sock_setopt(desc->s, proto, type, arg_ptr, arg_sz);
#ifdef DEBUG
	    DEBUGF(("inet_set_opts(%ld): s=%d returned %d\r\n",
		    (long)desc->port, desc->s, res));
	}
#endif
#ifdef VXWORKS
skip_os_setopt:
#endif
	if (type == SO_RCVBUF) {
	    /* make sure we have desc->bufsz >= SO_RCVBUF */
	    if (ival > desc->bufsz)
		desc->bufsz = ival;
	}
    }

    if ( ((desc->stype == SOCK_STREAM) && IS_CONNECTED(desc)) ||
	((desc->stype == SOCK_DGRAM) && IS_OPEN(desc))) {

	if (desc->active != old_active)
	    sock_select(desc, (FD_READ|FD_CLOSE), (desc->active>0));

	if ((desc->stype==SOCK_STREAM) && desc->active) {
	    if (!old_active) return 1;  /* passive => active change */
	    if (desc->htype != old_htype) return 2;  /* header type change */
	    return 0;
	}
    }
    return 0;
}

/* load all option values into the buf and reply 
** return total length of reply filled into ptr
** ptr should point to a buffer with 9*len +1 to be safe!!
*/

static int inet_fill_opts(inet_descriptor* desc,
			  char* buf, int len, char* ptr)
{
    int type;
    int proto;
    int opt;
    struct linger li_val;
    int ival;
    char* arg_ptr;
    int arg_sz;
    char* save_ptr = ptr;

    *ptr++ = INET_REP_OK;

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
	case INET_LOPT_MODE:
	    *ptr++ = opt;
	    put_int32(desc->mode, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_DELIVER:
	    *ptr++ = opt;
	    put_int32(desc->deliver, ptr);
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
	case INET_LOPT_PACKET_SIZE:
	    *ptr++ = opt;
	    put_int32(desc->psize, ptr);
	    ptr += 4;
	    continue;
	case INET_LOPT_EXITONCLOSE:
	    *ptr++ = opt;
	    put_int32(desc->exitf, ptr);
	    ptr += 4;
	    continue;

	case INET_LOPT_BIT8:
	    *ptr++ = opt;
	    if (desc->bit8f) {
		put_int32(desc->bit8, ptr);
	    }
	    else {
		put_int32(INET_BIT8_OFF, ptr);
	    }
	    ptr += 4;
	    continue;

	case INET_LOPT_TCP_HIWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->high;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_LOPT_TCP_LOWTRMRK:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->low;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_LOPT_TCP_SEND_TIMEOUT:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->send_timeout;
		put_int32(ival, ptr);
		ptr += 4;
	    }
	    continue;

	case INET_LOPT_TCP_DELAY_SEND:
	    if (desc->stype == SOCK_STREAM) {
		*ptr++ = opt;
		ival = ((tcp_descriptor*)desc)->fdelay_send;
		put_int32(ival, ptr);
		ptr += 4;
	    }
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
	case INET_OPT_LINGER:
	    arg_sz = sizeof(li_val);
	    arg_ptr = (char*) &li_val;	    
	    type = SO_LINGER; 
	    break;
#endif /* HAVE_MULTICAST_SUPPORT */

	default:
	    return -1;
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
    return (ptr - save_ptr);
}


/* fill statistics reply, op codes from src and result in dest
** dst area must be a least 5*len + 1 bytes
*/
static int inet_fill_stat(inet_descriptor* desc, char* src, int len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_STAT_RECV_CNT:  
	    val = desc->recv_cnt;    
	    break;
	case INET_STAT_RECV_MAX:  
	    val = (unsigned long) desc->recv_max;    
	    break;
	case INET_STAT_RECV_AVG:  
	    val = (unsigned long) desc->recv_avg;    
	    break;
	case INET_STAT_RECV_DVI:  
	    val = (unsigned long) fabs(desc->recv_dvi); 
	    break;
	case INET_STAT_SEND_CNT:  
	    val = desc->send_cnt; 
	    break;
	case INET_STAT_SEND_MAX:  
	    val = desc->send_max; 
	    break;
	case INET_STAT_SEND_AVG: 
	    val = (unsigned long) desc->send_avg;
	    break;
	case INET_STAT_SEND_PND:  
	    val = driver_sizeq(desc->port); 
	    break;
	case INET_STAT_RECV_OCT:
	    put_int32(desc->recv_oct[1], dst);   /* write high 32bit */
	    put_int32(desc->recv_oct[0], dst+4); /* write low 32bit */
	    dst += 8;
	    continue;
	case INET_STAT_SEND_OCT:
	    put_int32(desc->send_oct[1], dst);   /* write high 32bit */
	    put_int32(desc->send_oct[0], dst+4); /* write low 32bit */
	    dst += 8;
	    continue;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

static void
send_empty_out_q_msgs(inet_descriptor* desc)
{
  ErlDrvTermData msg[6];
  int msg_len = 0;

  if(NO_SUBSCRIBERS(&desc->empty_out_q_subs))
    return;

  msg_len = LOAD_ATOM(msg, msg_len, am_empty_out_q);
  msg_len = LOAD_PORT(msg, msg_len, desc->dport);
  msg_len = LOAD_TUPLE(msg, msg_len, 2);

  ASSERT(msg_len == 6);

  send_to_subscribers(desc->port,
		      &desc->empty_out_q_subs,
		      1,
		      msg,
		      msg_len);
}

/* subscribe and fill subscription reply, op codes from src and
** result in dest dst area must be a least 5*len + 1 bytes
*/
static int inet_subscribe(inet_descriptor* desc, char* src, int len, char* dst)
{
    unsigned long val;
    int op;
    char* dst_start = dst;

    *dst++ = INET_REP_OK;     /* put reply code */
    while (len--) {
	op = *src++;
	*dst++ = op;  /* copy op code */
	switch(op) {
	case INET_SUBS_EMPTY_OUT_Q:  
	  val = driver_sizeq(desc->port);
	  if(val > 0)
	    if(!save_subscriber(&desc->empty_out_q_subs,
				driver_caller(desc->port)))
	      return 0;
	  break;
	default: return -1; /* invalid argument */
	}
	put_int32(val, dst);  /* write 32bit value */
	dst += 4;
    }
    return dst - dst_start;  /* actual length */
}

/* Terminate socket */
static void inet_stop(inet_descriptor* desc)
{
    erl_inet_close(desc);
    inet_desc_table[desc->ix] = NULL;  /* detach from table */
    FREE(desc);
}


/* Allocate descriptor */
static ErlDrvData inet_start(ErlDrvPort port, int size)
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
	DEBUGF(("ran out of inet descriptors max = %d\r\r\n", inet_desc_size));
	errno = EMFILE;
	return NULL;
    }
    inet_desc_ix = (ix + 1) % inet_desc_size;

    if ((desc = (inet_descriptor*) ALLOC(size)) == NULL)
	return NULL;

    desc->s = INVALID_SOCKET;
    desc->event = INVALID_EVENT;
    desc->event_mask = 0;
    desc->port = port;
    desc->dport = driver_mk_port(port);
    desc->state = INET_STATE_CLOSED;
    desc->prebound = 0;
    desc->ix = ix;
    desc->bufsz = INET_DEF_BUFFER; 
    desc->hsz = 0;                     /* list header size */
    desc->htype = TCP_PB_RAW;          /* default packet type */
    desc->psize = 0;                   /* no size check */
    desc->stype = -1;                  /* bad stype */
    desc->sfamily = -1;
    desc->mode    = INET_MODE_LIST;    /* list mode */
    desc->exitf   = 1;                 /* exit port when close on active 
					  socket */
    desc->bit8f   = 0;
    desc->bit8    = 0;
    desc->deliver = INET_DELIVER_TERM; /* standard term format */
    desc->active  = INET_PASSIVE;      /* start passive */
    desc->oph = NULL;
    desc->opt = NULL;

    desc->peer_ptr = NULL;
    desc->name_ptr = NULL;

    desc->recv_oct[0] = desc->recv_oct[1] = 0;
    desc->recv_cnt = 0;
    desc->recv_max = 0;    
    desc->recv_avg = 0.0;
    desc->recv_dvi = 0.0;
    desc->send_oct[0] = desc->send_oct[1] = 0;
    desc->send_cnt = 0;
    desc->send_max = 0;
    desc->send_avg = 0.0;
    desc->empty_out_q_subs.subscriber = NO_PROCESS;
    desc->empty_out_q_subs.next = NULL;

    sys_memzero((char *)&desc->remote,sizeof(desc->remote));

    inet_desc_table[ix] = desc;

    return (ErlDrvData)desc;
}


#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/*
** common tcp/udp control command
*/
static int inet_ctl(inet_descriptor* desc, int cmd, char* buf, int len,
		    char** rbuf, int rsize)
{
    switch (cmd) {

    case INET_REQ_GETSTAT: {
	  char* dst;
	  int i;
	  int dstlen = 1;  /* Reply code */

	  for (i = 0; i < len; i++) {
	      switch(buf[i]) {
	      case INET_STAT_SEND_OCT: dstlen += 9; break;
	      case INET_STAT_RECV_OCT: dstlen += 9; break;
	      default: dstlen += 5; break;
	      }
	  }
	  DEBUGF(("inet_ctl(%ld): GETSTAT\r\n", (long) desc->port)); 
	  if (dstlen > INET_MAX_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_fill_stat(desc, buf, len, dst);
      }

    case INET_REQ_SUBSCRIBE: {
	  char* dst;
	  int dstlen = 1 /* Reply code */ + len*5;
	  DEBUGF(("inet_ctl(%ld): INET_REQ_SUBSCRIBE\r\n", (long) desc->port)); 
	  if (dstlen > INET_MAX_BUFFER) /* sanity check */
	      return 0;
	  if (dstlen > rsize) {
	      if ((dst = (char*) ALLOC(dstlen)) == NULL)
		  return 0;
	      *rbuf = dst;  /* call will free this buffer */
	  }
	  else
	      dst = *rbuf;  /* ok we fit in buffer given */
	  return inet_subscribe(desc, buf, len, dst);
      }

    case INET_REQ_GETOPTS: {    /* get options */
	char* dst;
	int dstlen = len*9 + 1;  /* max length of reply */
	int replen;

	DEBUGF(("inet_ctl(%ld): GETOPTS\r\n", (long)desc->port)); 
	if (dstlen > INET_MAX_BUFFER)
	    return 0;
	if (dstlen > rsize) {
	    if ((dst = (char*) ALLOC(dstlen)) == NULL)
		return 0;
	    *rbuf = dst;  /* call will free this buffer */	    
	}
	else
	    dst = *rbuf;
	if ((replen = inet_fill_opts(desc, buf, len, dst)) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return replen;
    }

    case INET_REQ_GETIFLIST: {
	DEBUGF(("inet_ctl(%ld): GETIFLIST\r\n", (long)desc->port)); 
	if (!IS_OPEN(desc))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	return inet_ctl_getiflist(desc, rbuf, rsize);
    }

    case INET_REQ_IFGET: {
	DEBUGF(("inet_ctl(%ld): IFGET\r\n", (long)desc->port)); 	
	if (!IS_OPEN(desc))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	return inet_ctl_ifget(desc, buf, len, rbuf, rsize);
    }

    case INET_REQ_IFSET: {
	DEBUGF(("inet_ctl(%ld): IFSET\r\n", (long)desc->port));
	if (!IS_OPEN(desc))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	return inet_ctl_ifset(desc, buf, len, rbuf, rsize);
    }

    case INET_REQ_SETOPTS:  {   /* set options */
	DEBUGF(("inet_ctl(%ld): SETOPTS\r\n", (long)desc->port)); 
	switch(inet_set_opts(desc, buf, len)) {
	case -1: 
	    return ctl_error(EINVAL, rbuf, rsize);
	case 0: 
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	default:  /* active/passive change!! */
	    /*
	     * Let's hope that the descriptor really is a tcp_descriptor here.
	     */
	    tcp_deliver((tcp_descriptor *) desc, 0);
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
    }

    case INET_REQ_GETIX: {          /* get internal index (listen/accept) */
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETIX\r\n", (long)desc->port)); 
	put_int32(desc->ix, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }

    case INET_REQ_GETSTATUS: {
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETSTATUS\r\n", (long)desc->port)); 
	put_int32(desc->state, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }

    case INET_REQ_GETTYPE: {
	char tbuf[8];

	DEBUGF(("inet_ctl(%ld): GETTYPE\r\n", (long)desc->port)); 
	if (desc->sfamily == AF_INET) {
	    put_int32(INET_AF_INET, &tbuf[0]);
	}
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if (desc->sfamily == AF_INET6) {
	    put_int32(INET_AF_INET6, &tbuf[0]);
	}
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

	if (desc->stype == SOCK_STREAM) {
	    put_int32(INET_TYPE_STREAM, &tbuf[4]);
	}
	else if (desc->stype == SOCK_DGRAM) {
	    put_int32(INET_TYPE_DGRAM, &tbuf[4]);
	}
	else
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, 8, rbuf, rsize);
    }


    case INET_REQ_GETFD: {
	char tbuf[4];

	DEBUGF(("inet_ctl(%ld): GETFD\r\n", (long)desc->port)); 
	if (!IS_OPEN(desc))
	    return ctl_error(EINVAL, rbuf, rsize);
	put_int32((long)desc->s, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 4, rbuf, rsize);
    }
	
    case INET_REQ_GETHOSTNAME: { /* get host name */
	char tbuf[MAXHOSTNAMELEN];

	DEBUGF(("inet_ctl(%ld): GETHOSTNAME\r\n", (long)desc->port)); 
	if (len != 0)
	    return ctl_error(EINVAL, rbuf, rsize);

	if (sock_hostname(tbuf, MAXHOSTNAMELEN) == SOCKET_ERROR)
	    return ctl_error(sock_errno(), rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, strlen(tbuf), rbuf, rsize);
    }

    case INET_REQ_PEER:  {      /* get peername */
	char tbuf[sizeof(inet_address)];
	inet_address peer;
	inet_address* ptr;
	int sz = sizeof(peer);

	DEBUGF(("inet_ctl(%ld): PEER\r\n", (long)desc->port)); 

	if (!(desc->state & INET_F_ACTIVE))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	if ((ptr = desc->peer_ptr) == NULL) {
	    ptr = &peer;
	    if (sock_peer(desc->s, (struct sockaddr*)ptr,&sz) == SOCKET_ERROR)
		return ctl_error(sock_errno(), rbuf, rsize);
	}
	if (inet_get_address(desc->sfamily, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETPEER: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->peer_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_address(desc->sfamily, &desc->peer_addr,
				  buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->peer_ptr = &desc->peer_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_NAME:  {      /* get sockname */
	char tbuf[sizeof(inet_address)];
	inet_address name;
	inet_address* ptr;
	int sz = sizeof(name);

	DEBUGF(("inet_ctl(%ld): NAME\r\n", (long)desc->port)); 

	if (!IS_BOUND(desc))
	    return ctl_error(EINVAL, rbuf, rsize); /* address is not valid */

	if ((ptr = desc->name_ptr) == NULL) {
	    ptr = &name;
	    if (sock_name(desc->s, (struct sockaddr*)ptr, &sz) == SOCKET_ERROR)
		return ctl_error(sock_errno(), rbuf, rsize);
	}
	if (inet_get_address(desc->sfamily, tbuf, ptr, &sz) < 0)
	    return ctl_error(EINVAL, rbuf, rsize);
	return ctl_reply(INET_REP_OK, tbuf, sz, rbuf, rsize);
    }

    case INET_REQ_SETNAME: { /* set fake peername Port Address */
	if (len == 0) {
	    desc->name_ptr = NULL;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	}
	else if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);	    
	else if (inet_set_address(desc->sfamily, &desc->name_addr,
				  buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    desc->name_ptr = &desc->name_addr;
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);	    
	}
    }

    case INET_REQ_BIND:  {      /* bind socket */
	char tbuf[2];
	inet_address local;
	short port;

	DEBUGF(("inet_ctl(%ld): BIND\r\n", (long)desc->port)); 

	if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	if (desc->state != INET_STATE_OPEN)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);

	if (inet_set_address(desc->sfamily, &local, buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);

	if (sock_bind(desc->s,(struct sockaddr*) &local, len) == SOCKET_ERROR)
	    return ctl_error(sock_errno(), rbuf, rsize);

	desc->state = INET_STATE_BOUND;

	if ((port = inet_address_port(&local)) == 0) {
	    len = sizeof(local);
	    sock_name(desc->s, (struct sockaddr*) &local, &len);
	    port = inet_address_port(&local);
	}
	port = sock_ntohs(port);
	put_int16(port, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

#ifndef VXWORKS

    case INET_REQ_GETSERVBYNAME: { /* L1 Name-String L2 Proto-String */
	char namebuf[256];
	char protobuf[256];
	char tbuf[2];
	struct servent* srv;
	short port;
	int n;

	if (len < 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	n = buf[0]; buf++; len--;
	if (n >= len) /* the = sign makes the test inklude next length byte */
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(namebuf, buf, n);
	namebuf[n] = '\0';
	len -= n; buf += n;
	n = buf[0]; buf++; len--;
	if (n > len)
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(protobuf, buf, n);
	protobuf[n] = '\0';
	if ((srv = sock_getservbyname(namebuf, protobuf)) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	port = sock_ntohs(srv->s_port);
	put_int16(port, tbuf);
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_GETSERVBYPORT: { /* P1 P0 L1 Proto-String */
	char protobuf[256];
	unsigned short port;
	int n;
	struct servent* srv;

	if (len < 3)
	    return ctl_error(EINVAL, rbuf, rsize);
	port = get_int16(buf);
	port = sock_htons(port);
	buf += 2;
	n = buf[0]; buf++; len -= 3;
	if (n > len)
	    return ctl_error(EINVAL, rbuf, rsize);
	memcpy(protobuf, buf, n);
	protobuf[n] = '\0';
	if ((srv = sock_getservbyport(port, protobuf)) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	len = strlen(srv->s_name);
	return ctl_reply(INET_REP_OK, srv->s_name, len, rbuf, rsize);
    }
	
#endif /* !VXWORKS */	

    default:
	return ctl_xerror(EXBADPORT, rbuf, rsize);
    }
}

/* update statistics on output packets */
static void inet_output_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->send_cnt + 1;
    unsigned long t = desc->send_oct[0] + len;
    int c = (t < desc->send_oct[0]);
    double avg = desc->send_avg;

    /* at least 64 bit octet count */
    desc->send_oct[0] = t;
    desc->send_oct[1] += c;

    if (n == 0) /* WRAP, use old avg as input to a new sequence */
	n = 1;
    desc->send_avg += (len - avg) / n;
    if (len > desc->send_max)
	desc->send_max = len;
    desc->send_cnt = n;
}

/* update statistics on input packets */
static void inet_input_count(inet_descriptor* desc, int len)
{
    unsigned long n = desc->recv_cnt + 1;
    unsigned long t = desc->recv_oct[0] + len;
    int c = (t < desc->recv_oct[0]);
    double avg = desc->recv_avg;
    double dvi;

    /* at least 64 bit octet count */
    desc->recv_oct[0] = t;
    desc->recv_oct[1] += c;

    if (n == 0) /* WRAP */
	n = 1;

    /* average packet length */
    avg = avg + (len - avg) / n;
    desc->recv_avg = avg;

    if (len > desc->recv_max)
	desc->recv_max = len;

    /* average deviation from average packet length */
    dvi = desc->recv_dvi;
    desc->recv_dvi = dvi + ((len - avg) - dvi) / n;
    desc->recv_cnt = n;
}

/*----------------------------------------------------------------------------

   TCP

-----------------------------------------------------------------------------*/

/*
** Set new size on buffer, used when packet size is determined
** and the buffer is to small.
** buffer must have a size of at least len bytes (counting from ptr_start!)
*/
static int tcp_expand_buffer(tcp_descriptor* desc, int len)
{
    ErlDrvBinary* bin;
    int offs1;
    int offs2;
    int used = desc->i_ptr_start - desc->i_buf->orig_bytes;
    int ulen = used + len;

    if (desc->i_bufsz >= ulen) /* packet will fit */
	return 0;
    else if (desc->i_buf->orig_size >= ulen) { /* buffer is large enough */
	desc->i_bufsz = ulen;  /* set "virtual" size */
	return 0;
    }

    DEBUGF(("tcp_expand_buffer(%ld): s=%d, from %ld to %d\r\n",
	    (long)desc->inet.port, desc->inet.s, desc->i_buf->orig_size, ulen));

    offs1 = desc->i_ptr_start - desc->i_buf->orig_bytes;
    offs2 = desc->i_ptr - desc->i_ptr_start;

    if ((bin = driver_realloc_binary(desc->i_buf, ulen)) == NULL)
	return -1;

    desc->i_buf = bin;
    desc->i_ptr_start = bin->orig_bytes + offs1;
    desc->i_ptr       = desc->i_ptr_start + offs2;
    desc->i_bufsz     = ulen;
    return 0;
}

/* push data into i_buf  */
static int tcp_push_buffer(tcp_descriptor* desc, char* buf, int len)
{
    ErlDrvBinary* bin;

    if (desc->i_buf == NULL) {
	bin = alloc_buffer(len);
	sys_memcpy(bin->orig_bytes, buf, len);
	desc->i_buf = bin;
	desc->i_bufsz = len;
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + len;
    }
    else {
	char* start =  desc->i_buf->orig_bytes;
	int sz_before = desc->i_ptr_start - start;
	int sz_filled = desc->i_ptr - desc->i_ptr_start;
	
	if (len <= sz_before) {
	    sys_memcpy(desc->i_ptr_start - len, buf, len);
	    desc->i_ptr_start -= len;
	}
	else {
	    bin = alloc_buffer(desc->i_bufsz+len);
	    sys_memcpy(bin->orig_bytes, buf, len);
	    sys_memcpy(bin->orig_bytes+len, desc->i_ptr_start, sz_filled);
	    free_buffer(desc->i_buf);
	    desc->i_bufsz += len;
	    desc->i_buf = bin;
	    desc->i_ptr_start = bin->orig_bytes;
	    desc->i_ptr = desc->i_ptr_start + sz_filled + len;
	}
    }
    desc->i_remain = 0;	
    return 0;
}

/* clear CURRENT input buffer */
static void tcp_clear_input(tcp_descriptor* desc)
{
    if (desc->i_buf != NULL)
	free_buffer(desc->i_buf);
    desc->i_buf = NULL;
    desc->i_remain    = 0;
    desc->i_ptr       = NULL;
    desc->i_ptr_start = NULL;
    desc->i_bufsz     = 0;
}

/* clear QUEUED output */
static void tcp_clear_output(tcp_descriptor* desc)
{
    ErlDrvPort ix  = desc->inet.port;
    int qsz = driver_sizeq(ix);

    driver_deq(ix, qsz);
    send_empty_out_q_msgs(INETP(desc));
}


/* Move data so that ptr_start point at buf->orig_bytes */
static void tcp_restart_input(tcp_descriptor* desc)
{
    if (desc->i_ptr_start != desc->i_buf->orig_bytes) {
	int n = desc->i_ptr - desc->i_ptr_start;

	DEBUGF(("tcp_restart_input: move %d bytes\r\n", n));
	sys_memmove(desc->i_buf->orig_bytes, desc->i_ptr_start, n);
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start + n;
    }
}


static int tcp_inet_init(void)
{
    DEBUGF(("tcp_inet_init() {}\r\n"));
    return 0;
}

/* initialize the tcp descriptor */

static ErlDrvData tcp_inet_start(ErlDrvPort port, char* args)
{
    tcp_descriptor* desc;
    DEBUGF(("tcp_inet_start(%ld) {\r\n", (long)port));

    desc = (tcp_descriptor*)inet_start(port, sizeof(tcp_descriptor));
    if (desc == NULL)
	return ERL_DRV_ERROR_ERRNO;
    desc->high = INET_HIGH_WATERMARK;
    desc->low  = INET_LOW_WATERMARK;
    desc->send_timeout = INET_INFINITY;
    desc->busy_on_send = 0;
    desc->i_ix = -1;
    desc->i_buf = NULL;
    desc->i_ptr = NULL;
    desc->i_ptr_start = NULL;
    desc->i_remain = 0;
    desc->i_bufsz = 0;
    desc->fdelay_send = 0;
#ifdef USE_HTTP
    desc->http_state = 0;
#endif
    DEBUGF(("tcp_inet_start(%ld) }\r\n", (long)port));
    return (ErlDrvData) desc;
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
** Check Special cases:
** 1. we are a listener doing nb accept -> report error on accept !
** 2. we are doing accept -> restore listener state
*/
static void tcp_close_check(tcp_descriptor* desc)
{
    if ((desc->inet.state == TCP_STATE_LISTENING) && (desc->i_ix != -1)) {
	tcp_descriptor* a_desc = 
	    (tcp_descriptor*) inet_desc_table[desc->i_ix];
	if ((a_desc != NULL) && (a_desc->inet.state == TCP_STATE_ACCEPTING)) {
	    DEBUGF(("tcp_close_check(%ld): s=%d\r\n",
		    (long)a_desc->inet.port, a_desc->inet.s));
	    async_error_am(INETP(a_desc), am_closed);
	}
    }
    else if (desc->inet.state == TCP_STATE_ACCEPTING) {
	tcp_clear_listener(desc);
	async_error_am(INETP(desc), am_closed);
    }
    else if (desc->inet.state == TCP_STATE_CONNECTING) {
	async_error_am(INETP(desc), am_closed);
    }
    else if (desc->inet.state == TCP_STATE_CONNECTED) {
	async_error_am_all(INETP(desc), am_closed);
    }
}

/*
** Cleanup & Free
*/
static void tcp_inet_stop(ErlDrvData e)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    DEBUGF(("tcp_inet_stop(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s));
    tcp_close_check(desc);
    /* free input buffer & output buffer */
    if (desc->i_buf != NULL)
	release_buffer(desc->i_buf);
    desc->i_buf = NULL; /* net_mess2 may call this function recursively when 
			   faulty messages arrive on dist ports*/
    DEBUGF(("tcp_inet_stop(%ld) }\r\n", (long)desc->inet.port));
    inet_stop(INETP(desc));
}


/* tcp requests from Erlang */
static int tcp_inet_ctl(ErlDrvData e, unsigned int cmd, char* buf, int len,
			char** rbuf, int rsize)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    switch(cmd) {
    case INET_REQ_OPEN:   /* open socket and return internal index */
	DEBUGF(("tcp_inet_ctl(%ld): OPEN\r\n", (long)desc->inet.port));
	if ((len == 1) && (buf[0] == INET_AF_INET))
	    return inet_ctl_open(INETP(desc),AF_INET,SOCK_STREAM,rbuf,rsize);
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if ((len == 1) && (buf[0] == INET_AF_INET6))
	    return inet_ctl_open(INETP(desc),AF_INET6,SOCK_STREAM,rbuf,rsize);
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

    case INET_REQ_FDOPEN:   /* pass in an open socket */
	DEBUGF(("tcp_inet_ctl(%ld): FDOPEN\r\n", (long)desc->inet.port)); 
	if ((len == 5) && (buf[0] == INET_AF_INET))
	    return inet_ctl_fdopen(INETP(desc), AF_INET, SOCK_STREAM,
			       (SOCKET) get_int32(buf+1), rbuf, rsize);
#if defined(HAVE_IN6) && defined(AF_INET6)
        else if ((len == 5) && (buf[0] == INET_AF_INET6))
	    return inet_ctl_fdopen(INETP(desc), AF_INET6, SOCK_STREAM,
				   (SOCKET) get_int32(buf+1), rbuf, rsize);
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

    case TCP_REQ_LISTEN: { /* argument backlog */

	int backlog;
	DEBUGF(("tcp_inet_ctl(%ld): LISTEN\r\n", (long)desc->inet.port)); 
	if (desc->inet.state == TCP_STATE_CLOSED)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (len != 2)
	    return ctl_error(EINVAL, rbuf, rsize);
	backlog = get_int16(buf);
	if (sock_listen(desc->inet.s, backlog) == SOCKET_ERROR)
	    return ctl_error(sock_errno(), rbuf, rsize);
	desc->inet.state = TCP_STATE_LISTEN;
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }


    case INET_REQ_CONNECT: {   /* do async connect */
	int code;
	char tbuf[2];
	unsigned timeout;

	DEBUGF(("tcp_inet_ctl(%ld): CONNECT\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4), Port(2), Address(N) */

	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (IS_CONNECTED(INETP(desc)))
	    return ctl_error(EISCONN, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_xerror(EXBADSEQ, rbuf, rsize);
	if (IS_CONNECTING(INETP(desc)))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (len < 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	len -= 4;
	if (inet_set_address(desc->inet.sfamily, &desc->inet.remote,
			     buf, &len) == NULL)
	    return ctl_error(EINVAL, rbuf, rsize);
	
	sock_select(INETP(desc), FD_CONNECT, 1);
	code = sock_connect(desc->inet.s, 
			    (struct sockaddr*) &desc->inet.remote, len);
	if ((code == SOCKET_ERROR) && 
		((sock_errno() == ERRNO_BLOCK) ||  /* Winsock2 */
		 (sock_errno() == EINPROGRESS))) {	/* Unix & OSE!! */
	    desc->inet.state = TCP_STATE_CONNECTING;
	    if (timeout != INET_INFINITY)
		driver_set_timer(desc->inet.port, timeout);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	}
	else if (code == 0) { /* ok we are connected */
	    sock_select(INETP(desc), FD_CONNECT, 0);
	    desc->inet.state = TCP_STATE_CONNECTED;
	    if (desc->inet.active)
		sock_select(INETP(desc), (FD_READ|FD_CLOSE), 1);
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	    async_ok(INETP(desc));
	}
	else {
	    sock_select(INETP(desc), FD_CONNECT, 0);
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case TCP_REQ_ACCEPT: {  /* do async accept */
	char tbuf[2];
	unsigned timeout;
	int ix;
	tcp_descriptor* l_desc;
	int n;
	SOCKET s;


	DEBUGF(("tcp_inet_ctl(%ld): ACCEPT\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4), ListenIndex(2) */

	if (desc->inet.state != TCP_STATE_CLOSED)
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (len != 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	ix = get_int16(buf);
	if ((ix >= inet_desc_size) || 
	    ((l_desc = (tcp_descriptor*)inet_desc_table[ix]) == NULL))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (l_desc->inet.state != TCP_STATE_LISTEN)
	    return ctl_error(EINVAL, rbuf, rsize);
	/* Some flags must be inherited at this point */
	desc->inet.mode    = l_desc->inet.mode;
	desc->inet.exitf   = l_desc->inet.exitf;
	desc->inet.bit8f   = l_desc->inet.bit8f;
	desc->inet.deliver = l_desc->inet.deliver;
	desc->inet.htype   = l_desc->inet.htype; 
	desc->inet.psize   = l_desc->inet.psize; 
	desc->inet.stype   = l_desc->inet.stype;
	desc->inet.sfamily = l_desc->inet.sfamily;
	desc->inet.hsz     = l_desc->inet.hsz;
	desc->inet.bufsz   = l_desc->inet.bufsz;
	desc->high         = l_desc->high;
	desc->low          = l_desc->low;
	desc->send_timeout = l_desc->send_timeout;

	n = sizeof(desc->inet.remote);
	s = sock_accept(l_desc->inet.s, 
			(struct sockaddr*) &desc->inet.remote, &n);
	if (s == INVALID_SOCKET) {
	    if (sock_errno() == ERRNO_BLOCK) {
		enq_async(INETP(desc), tbuf, TCP_REQ_ACCEPT);

		desc->inet.state = TCP_STATE_ACCEPTING;
		l_desc->inet.state = TCP_STATE_LISTENING;
		l_desc->i_ix = desc->inet.ix;
		sock_select(INETP(l_desc),FD_ACCEPT,1);
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout);
	    }
	    else
		return ctl_error(sock_errno(), rbuf, rsize);
	}
	else {
	    desc->inet.s = s;
	    if ((desc->inet.event = sock_create_event(INETP(desc))) == 
		INVALID_EVENT)
		return ctl_error(sock_errno(), rbuf, rsize);
	    SET_NONBLOCKING(desc->inet.s);
#ifdef __WIN32__
	    driver_select(desc->inet.port, desc->inet.event, DO_READ, 1);
#endif
	    desc->inet.state = TCP_STATE_CONNECTED;
	    enq_async(INETP(desc), tbuf, TCP_REQ_ACCEPT);
	    async_ok(INETP(desc));
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case INET_REQ_CLOSE:
	DEBUGF(("tcp_inet_ctl(%ld): CLOSE\r\n", (long)desc->inet.port)); 
	tcp_close_check(desc);
	erl_inet_close(INETP(desc));
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);


    case TCP_REQ_RECV: {
	unsigned timeout;
	char tbuf[2];
	int n;

	DEBUGF(("tcp_inet_ctl(%ld): RECV\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4),  Length(4) */
	if (!IS_CONNECTED(INETP(desc)))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	if (desc->inet.active || (len != 8))
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	n = get_int32(buf);
	DEBUGF(("tcp_inet_ctl(%ld) timeout = %d, n = %d\r\n",
		(long)desc->inet.port,timeout,n));
	if ((desc->inet.htype != TCP_PB_RAW) && (n != 0))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (n > TCP_MAX_PACKET_SIZE)
	    return ctl_error(ENOMEM, rbuf, rsize);
	if (enq_async(INETP(desc), tbuf, TCP_REQ_RECV) < 0)
	    return ctl_error(EALREADY, rbuf, rsize);

	if (tcp_recv(desc, n) == 0) {
	    if (timeout == 0)
		async_error_am(INETP(desc), am_timeout);
	    else {
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout); 
		sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
	    }
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case TCP_REQ_UNRECV: {
	DEBUGF(("tcp_inet_ctl(%ld): UNRECV\r\n", (long)desc->inet.port)); 
	if (!IS_CONNECTED(INETP(desc)))
	    return ctl_error(ENOTCONN, rbuf, rsize);
	tcp_push_buffer(desc, buf, len);
	if (desc->inet.active)
	    tcp_deliver(desc, 0);
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
    }
#ifndef _OSE_
    case TCP_REQ_SHUTDOWN: {
	int how;
	DEBUGF(("tcp_inet_ctl(%ld): FDOPEN\r\n", (long)desc->inet.port)); 
	if (!IS_CONNECTED(INETP(desc))) {
	    return ctl_error(ENOTCONN, rbuf, rsize);
	}
	if (len != 1) {
	    return ctl_error(EINVAL, rbuf, rsize);
	}
	how = buf[0];
	if (sock_shutdown(INETP(desc)->s, how) == 0) {
	    return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	} else {
	    return ctl_error(sock_errno(), rbuf, rsize);
	}
    }
#endif
    default:
	DEBUGF(("tcp_inet_ctl(%ld): %u\r\n", (long)desc->inet.port, cmd)); 
	return inet_ctl(INETP(desc), cmd, buf, len, rbuf, rsize);
    }

}

/*
** tcp_inet_timeout:
** called when timer expire:
** tcp socket may be:
**
** a)  receiving   -- deselect
** b)  connecting  -- close socket
** c)  accepting   -- reset listener
**
*/

static void tcp_inet_timeout(ErlDrvData e)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    int state = desc->inet.state;

    DEBUGF(("tcp_inet_timeout(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s)); 
    if ((state & TCP_STATE_CONNECTED) == TCP_STATE_CONNECTED) {
	if (desc->busy_on_send) {
	    desc->busy_on_send = 0;
	    inet_reply_error_am(INETP(desc), am_timeout);
	}
	else {
	    /* assume recv timeout */
	    ASSERT(!desc->inet.active);
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    desc->i_remain = 0;
	    async_error_am(INETP(desc), am_timeout);
	}
    }
    else if ((state & TCP_STATE_CONNECTING) == TCP_STATE_CONNECTING) {
	/* assume connect timeout */
	/* close the socket since it's not usable (see man pages) */
	erl_inet_close(INETP(desc));
	async_error_am(INETP(desc), am_timeout);
    }
    else if ((state & TCP_STATE_ACCEPTING) == TCP_STATE_ACCEPTING) {
	/* timer is set on accepting port */
	tcp_clear_listener(desc);
	async_error_am(INETP(desc), am_timeout);
    }
    DEBUGF(("tcp_inet_timeout(%ld) }\r\n", (long)desc->inet.port)); 
}

/*
** command:
**   output on a socket only !
**   a reply code will be sent to connected (caller later)
**   {inet_reply, S, Status}
** NOTE! normal sockets use the the tcp_inet_commandv
** but distribution still uses the tcp_inet_command!!
*/

static void tcp_inet_command(ErlDrvData e, char *buf, int len)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    DEBUGF(("tcp_inet_command(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s)); 
    if (!IS_CONNECTED(INETP(desc)))
	inet_reply_error(INETP(desc), ENOTCONN);
    else if (tcp_send(desc, buf, len) == 0)
	inet_reply_ok(INETP(desc));
    DEBUGF(("tcp_inet_command(%ld) }\r\n", (long)desc->inet.port)); 
}


static void tcp_inet_commandv(ErlDrvData e, ErlIOVec* ev)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    desc->inet.caller = driver_caller(desc->inet.port);

    DEBUGF(("tcp_inet_commanv(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s)); 
    if (!IS_CONNECTED(INETP(desc)))
	inet_reply_error(INETP(desc), ENOTCONN);
    else if (tcp_sendv(desc, ev) == 0)
	inet_reply_ok(INETP(desc));
    DEBUGF(("tcp_inet_commandv(%ld) }\r\n", (long)desc->inet.port)); 
}


/* The socket has closed, cleanup and send event */
static int tcp_recv_closed(tcp_descriptor* desc)
{
    DEBUGF(("tcp_recv_closed(%ld): s=%d, in %s, line %d\r\n",
	    (long)desc->inet.port, desc->inet.s, __FILE__, __LINE__));
    if (IS_BUSY(INETP(desc))) {
	/* A send is blocked */
	desc->inet.caller = desc->inet.busy_caller;
	tcp_clear_output(desc);
	if (desc->busy_on_send) {
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;
	    DEBUGF(("tcp_recv_closed(%ld): busy on send\r\n", 
		    (long)desc->inet.port));
	}
	desc->inet.state &= ~INET_F_BUSY;
	set_busy_port(desc->inet.port, 0);
	inet_reply_error_am(INETP(desc), am_closed);
	DEBUGF(("tcp_recv_closed(%ld): busy reply 'closed'\r\n", 
		(long)desc->inet.port));
    }
    if (!desc->inet.active) {
	/* We must cancel any timer here ! */
	driver_cancel_timer(desc->inet.port);
	/* passive mode do not terminate port ! */
	tcp_clear_input(desc);
	if (desc->inet.exitf) {
	    desc_close(INETP(desc));
	} else {
	    desc_close_read(INETP(desc));
	}
	async_error_am_all(INETP(desc), am_closed);
	/* next time EXBADSEQ will be delivered  */
	DEBUGF(("tcp_recv_closed(%ld): passive reply all 'closed'\r\n", 
		(long)desc->inet.port));
    } else {
	tcp_clear_input(desc);
	tcp_closed_message(desc);
	if (desc->inet.exitf) {
	    driver_exit(desc->inet.port, 0);
	} else {
	    desc_close_read(INETP(desc));
	}
	DEBUGF(("tcp_recv_closed(%ld): active close\r\n", 
		(long)desc->inet.port));
    }
    DEBUGF(("tcp_recv_closed(%ld): done\r\n", (long)desc->inet.port));
    return -1;
}


/* We have a read error determine the action */
static int tcp_recv_error(tcp_descriptor* desc, int err)
{
    if (err != ERRNO_BLOCK) {
	if (IS_BUSY(INETP(desc))) {
	    /* A send is blocked */
	    desc->inet.caller = desc->inet.busy_caller;
	    tcp_clear_output(desc);
	    if (desc->busy_on_send) {
		driver_cancel_timer(desc->inet.port);
		desc->busy_on_send = 0;
	    }
	    desc->inet.state &= ~INET_F_BUSY;
	    set_busy_port(desc->inet.port, 0);
	    inet_reply_error_am(INETP(desc), am_closed);
	}
	if (!desc->inet.active) {
	    /* We must cancel any timer here ! */
	    driver_cancel_timer(desc->inet.port);
	    tcp_clear_input(desc);
	    if (desc->inet.exitf) {
		desc_close(INETP(desc));
	    } else {
		desc_close_read(INETP(desc));
	    }
	    async_error_am_all(INETP(desc), error_atom(err));
	} else {
	    tcp_clear_input(desc);
	    tcp_error_message(desc, err); /* first error */
	    tcp_closed_message(desc);     /* then closed */
	    if (desc->inet.exitf)
		driver_exit(desc->inet.port, err);
	    else
		desc_close(INETP(desc));
	}
	return -1;
    }
    return 0;
}



/*
** Calculate number of bytes that remain to read before deliver
** Assume buf, ptr_start, ptr has been setup
**
** return  > 0 if more to read
**         = 0 if holding complete packet
**         < 0 on error
**
** if return value == 0 then *len will hold the length of the first packet
**    return value > 0 then if *len == 0 then value means upperbound
**                             *len > 0  then value means exact
**
*/
static int tcp_remain(tcp_descriptor* desc, int* len)
{
    char* ptr = desc->i_ptr_start;
    int nfill = (desc->i_ptr - desc->i_buf->orig_bytes); /* filled */
    int nsz   = desc->i_bufsz - nfill;                   /* remain */
    int n = desc->i_ptr - ptr;  /* number of bytes read */
    int plen;
    int hlen;

    DEBUGF(("tcp_remain(%ld): s=%d, n=%d, nfill=%d nsz=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s, n, nfill, nsz));

    switch(desc->inet.htype) {
    case TCP_PB_RAW:
	if (n == 0) goto more;
	else {
	    *len = n;
	    DEBUGF((" => nothing remain packet=%d\r\n", n));	    
	    return 0;  /* deliver */
	}

    case TCP_PB_1:
	/* TCP_PB_1:    [L0 | Data] */
	hlen = 1;
	if (n < hlen) goto more;
	plen = get_int8(ptr);
	goto remain;

    case TCP_PB_2:
	/* TCP_PB_2:    [L1,L0 | Data] */
	hlen = 2;
	if (n < hlen) goto more;
	plen = get_int16(ptr);
	goto remain;

    case TCP_PB_4:
	/* TCP_PB_4:    [L3,L2,L1,L0 | Data] */
	hlen = 4;
	if (n < hlen) goto more;
	plen = get_int32(ptr);
	goto remain;

    case TCP_PB_RM:
	/* TCP_PB_RM:    [L3,L2,L1,L0 | Data] 
	 ** where MSB (bit) is used to signal end of record
	 */
	hlen = 4;
	if (n < hlen) goto more;
	plen = get_int32(ptr) & 0x7fffffff;
	goto remain;

    case TCP_PB_LINE_LF: {
	/* TCP_PB_LINE_LF:  [Data ... \n]  */
	char* ptr2;
	if  ((ptr2 = memchr(ptr, '\n', n)) == NULL) {
	    if ((nsz == 0) && (nfill == n)) { /* buffer full */
		*len = n;
		DEBUGF((" => line buffer full (no NL)=%d\r\n", n));
		return 0;
	    }
	    goto more;
	}
	else {
	    *len = (ptr2 - ptr) + 1;  /* include newline */
	    DEBUGF((" => nothing remain packet=%d\r\n", *len));
	    return 0;
	}
    }

    case TCP_PB_ASN1: {
	/* TCP_PB_ASN1: handles long (4 bytes) or short length format */
	char* tptr = ptr;
	int length;
	int nn = n;

	if (n < 2) goto more;
	nn--;
	if ((*tptr++ & 0x1f) == 0x1f) { /* Long tag format */
	    while(nn && ((*tptr & 0x80) == 0x80)) {
		tptr++;
		nn--;
	    }
	    if (nn < 2) goto more;
	    tptr++;
	    nn--;
	}

	/* tptr now point to length field and n characters remain */
	length = *tptr & 0x7f;
	if ((*tptr & 0x80) == 0x80) {   /* Long length format */
	    tptr++;
	    nn--;
	    if (nn < length) goto more;
	    switch(length) {
	    case 0: plen = 0; break;
	    case 1: plen = get_int8(tptr);  tptr += 1; break;
	    case 2: plen = get_int16(tptr); tptr += 2; break;
	    case 3: plen = get_int24(tptr); tptr += 3; break;
	    case 4: plen = get_int32(tptr); tptr += 4; break;
	    default: goto error; /* error */
	    }
	}
	else {
	    tptr++;
	    plen = length;
	}
	hlen = (tptr-ptr);
	goto remain;
    }


    case TCP_PB_CDR: {
	struct cdr_head* hp;
	hlen = sizeof(struct cdr_head);
	if (n < hlen) goto more;
	hp = (struct cdr_head*) ptr;
	if (sys_memcmp(hp->magic, CDR_MAGIC, 4) != 0)
	    goto error;
	if (hp->flags & 0x01) /* Byte ordering flag */
	    plen = get_little_int32(hp->message_size);
	else
	    plen = get_int32(hp->message_size);
	goto remain;
    }

    case TCP_PB_FCGI: {
	struct fcgi_head* hp;
	hlen = sizeof(struct fcgi_head);
	if (n < hlen) goto more;
	hp = (struct fcgi_head*) ptr;
	if (hp->version != FCGI_VERSION_1)
	    goto error;			/* ERROR, unknown header version */
	plen = ((hp->contentLengthB1 << 8) | hp->contentLengthB0)
	    + hp->paddingLength;
	goto remain;
    }
#ifdef USE_HTTP
    case TCP_PB_HTTPH:
	desc->http_state = 1;
    case TCP_PB_HTTP: {
        /* TCP_PB_HTTP:  data \r\n(SP data\r\n)*  */
        plen = n;
	if (((plen == 1) && NL(ptr)) || ((plen == 2) && CRNL(ptr)))
	    goto done;
	else {
	    char* ptr1 = ptr;
	    int   len = plen;

	    while(1) {
	      char* ptr2 = memchr(ptr1, '\n', len);

	      if  (ptr2 == NULL) {
		  if ((nsz == 0) && (nfill == n)) { /* buffer full */
		      plen = n;
		      goto done;
		  }
		  goto more;
	      }
	      else {
  		  plen = (ptr2 - ptr) + 1;

		  if (desc->http_state == 0) 
		      goto done;
	        
		  if (plen < n) {
		      if (SP(ptr2+1)) {
			  ptr1 = ptr2+1;
			  len -= plen;
		      }
		      else
			  goto done;
		  }
		  else
		      goto more;
	      }
	    }
	}
    }
#endif
    case TCP_PB_TPKT: {
	struct tpkt_head* hp;
	hlen = sizeof(struct tpkt_head);
	if (n < hlen) 
	    goto more;
	hp = (struct tpkt_head*) ptr;
	if (hp->vrsn == TPKT_VRSN) {
	    plen = get_int16(hp->packet_length) - hlen;
	    if (plen < 0)
		goto error;
	} else
	    goto error;
	goto remain;
    }

    default:  /* this can not occure (make compiler happy) */
	DEBUGF((" => case error\r\n"));
	return -1;
    }

 done: {
      *len = plen;
      DEBUGF((" => nothing remain packet=%d\r\n", plen));
      return 0;
    }

 remain: {
     int tlen, remain;
     if (desc->inet.psize != 0 && 
	 ((unsigned int)plen) > desc->inet.psize) goto error;
     tlen = plen + hlen;
     remain = tlen - n;
     if (remain <= 0) {
	 *len = tlen;
	 DEBUGF((" => nothing remain packet=%d\r\n", tlen));
	 return 0;
     }
     else {
	 if (tcp_expand_buffer(desc, tlen) < 0)
	     return -1;
	 DEBUGF((" => remain=%d\r\n", remain));
	 *len = remain;
	 return remain;
     }
 }

 more:
    *len = 0;
    if (nsz == 0) {
	if (nfill == n)
	    goto error;
	DEBUGF((" => restart more=%d\r\n", nfill - n));
	return nfill - n;
    }
    else {
	DEBUGF((" => more=%d \r\n", nsz));
	return nsz;
    }

 error:
    DEBUGF((" => packet error\r\n"));
    return -1;
}

/*
** Deliver all packets ready 
** if len == 0 then check start with a check for ready packet
*/
static int tcp_deliver(tcp_descriptor* desc, int len)
{
    int count = 0;
    int n;

    /* Poll for ready packet */
    if (len == 0) {
	/* empty buffer or waiting for more input */
	if ((desc->i_buf == NULL) || (desc->i_remain > 0))
	    return count;
	if ((n = tcp_remain(desc, &len)) != 0) {
	    if (n < 0) /* packet error */
		return n;
	    if (len > 0)  /* more data pending */
		desc->i_remain = len;
	    return count;
	}
    }

    while (len > 0) {
	int code = 0;

	inet_input_count(INETP(desc), len);

	/* deliver binary? */
	if (len*4 >= desc->i_buf->orig_size*3) { /* >=75% */
	    /* something after? */
	    if (desc->i_ptr_start + len == desc->i_ptr) { /* no */
		code = tcp_reply_binary_data(desc, desc->i_buf,
					     (desc->i_ptr_start -
					      desc->i_buf->orig_bytes),
					     len);
		tcp_clear_input(desc);
	    }
	    else { /* move trail to beginning of a new buffer */
		ErlDrvBinary* bin;
		char* ptr_end = desc->i_ptr_start + len;
		int sz = desc->i_ptr - ptr_end;

		bin = alloc_buffer(desc->i_bufsz);
		memcpy(bin->orig_bytes, ptr_end, sz);

		code = tcp_reply_binary_data(desc, desc->i_buf,
					     (desc->i_ptr_start-
					      desc->i_buf->orig_bytes),
					     len);
		free_buffer(desc->i_buf);
		desc->i_buf = bin;
		desc->i_ptr_start = desc->i_buf->orig_bytes;
		desc->i_ptr = desc->i_ptr_start + sz;
		desc->i_remain = 0;
	    }
	}
	else {
	    code = tcp_reply_data(desc, desc->i_ptr_start, len);
	    desc->i_ptr_start += len;
	    if (desc->i_ptr_start == desc->i_ptr)
		tcp_clear_input(desc);
	    else
		desc->i_remain = 0;
	}

	if (code < 0)
	    return code;

	count++;
	len = 0;

	if (!desc->inet.active) {
	    driver_cancel_timer(desc->inet.port);
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),0);
	    if (desc->i_buf != NULL)
		tcp_restart_input(desc);
	}
	else if (desc->i_buf != NULL) {
	    if ((n = tcp_remain(desc, &len)) != 0) {
		if (n < 0) /* packet error */
		    return n;
		tcp_restart_input(desc);
		if (len > 0)
		    desc->i_remain = len;
		len = 0;
	    }
	}
    }
    return count;
}


static int tcp_recv(tcp_descriptor* desc, int request_len)
{
    int n;
    int len;
    int nread;

    if (desc->i_buf == NULL) {  /* allocte a read buffer */
	int sz = (request_len > 0) ? request_len : desc->inet.bufsz;

	if ((desc->i_buf = alloc_buffer(sz)) == NULL)
	    return -1;
	/* XXX: changing bufsz during recv SHOULD/MAY? affect 
	 * ongoing operation but is not now 
	 */
	desc->i_bufsz = sz; /* use i_bufsz not i_buf->orig_size ! */
	desc->i_ptr_start = desc->i_buf->orig_bytes;
	desc->i_ptr = desc->i_ptr_start;
	nread = sz;
	if (request_len > 0)
	    desc->i_remain = request_len;
	else
	    desc->i_remain = 0;
    }
    else if (request_len > 0) { /* we have a data in buffer and a request */
	n = desc->i_ptr - desc->i_ptr_start;
	if (n >= request_len)
	    return tcp_deliver(desc, request_len);
	else if (tcp_expand_buffer(desc, request_len) < 0)
	    return tcp_recv_error(desc, ENOMEM);
	else
	    desc->i_remain = nread = request_len - n;
    }
    else if (desc->i_remain == 0) {  /* poll remain from buffer data */
	if ((nread = tcp_remain(desc, &len)) < 0)
	    return tcp_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return tcp_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
    }
    else  /* remain already set use it */
	nread = desc->i_remain;
    
    DEBUGF(("tcp_recv(%ld): s=%d about to read %d bytes...\r\n",  
	    (long)desc->inet.port, desc->inet.s, nread));

    n = sock_recv(desc->inet.s, desc->i_ptr, nread, 0);

    if (n == SOCKET_ERROR) {
	int err = sock_errno();
	if (err == ECONNRESET) {
	    DEBUGF((" => detected close (connreset)\r\n"));
	    return tcp_recv_closed(desc);
	}
	if (err == ERRNO_BLOCK) {
	    DEBUGF((" => would block\r\n"));
	    return 0;
	}
	else {
	    DEBUGF((" => error: %d\r\n", err));
	    return tcp_recv_error(desc, err);
	}
    }
    else if (n == 0) {
	DEBUGF(("  => detected close\r\n"));
	return tcp_recv_closed(desc);
    }

    DEBUGF((" => got %d bytes\r\n", n));
    desc->i_ptr += n;
    if (desc->i_remain > 0) {
	desc->i_remain -= n;
	if (desc->i_remain == 0)
	    return tcp_deliver(desc, desc->i_ptr - desc->i_ptr_start);
    }
    else {
	if ((nread = tcp_remain(desc, &len)) < 0)
	    return tcp_recv_error(desc, EMSGSIZE);
	else if (nread == 0)
	    return tcp_deliver(desc, len);
	else if (len > 0)
	    desc->i_remain = len;  /* set remain */
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

static void tcp_inet_event(ErlDrvData e, ErlDrvEvent event)
{
    tcp_descriptor* desc = (tcp_descriptor*)e;
    WSANETWORKEVENTS netEv;
    int err;

    DEBUGF(("tcp_inet_event(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s));
    if ((*winSock.WSAEnumNetworkEvents)(desc->inet.s, desc->inet.event,
					&netEv) != 0) {
	DEBUGF((" => EnumNetworkEvents = %d\r\n", sock_errno() ));
	goto error;
    }

    DEBUGF((" => event=%02X, mask=%02X\r\n",
	    netEv.lNetworkEvents, desc->inet.event_mask));

    /*
     * Calling WSAEventSelect with a mask of 0 doesn't always turn off
     * all events.  To avoid acting on events we don't want, we mask
     * the events with mask for the events we really want.
     */

#ifdef DEBUG
    if ((netEv.lNetworkEvents & ~(desc->inet.event_mask)) != 0) {
	DEBUGF(("port %d:  ... unexpected event: %d\r\n",
		desc->inet.port, netEv.lNetworkEvents & ~(desc->inet.event_mask)));
    }
#endif
    netEv.lNetworkEvents &= desc->inet.event_mask;

    if (netEv.lNetworkEvents & FD_READ) {
	do {
	    if (tcp_inet_input(desc, event) < 0)
		goto error;
	    if (netEv.lNetworkEvents & FD_CLOSE) {
		DEBUGF(("Retrying read due to closed port\r\n"));
	    }
	} while (netEv.lNetworkEvents & FD_CLOSE);
    }
    if (netEv.lNetworkEvents & FD_WRITE) {
	if (tcp_inet_output(desc, event) < 0)
	    goto error;
    }
    if (netEv.lNetworkEvents & FD_CONNECT) {
	if ((err = netEv.iErrorCode[FD_CONNECT_BIT]) != 0) {
	    async_error(INETP(desc), err);
	} else
	    tcp_inet_output(desc, event);
    } else if (netEv.lNetworkEvents & FD_ACCEPT) {
	if ((err = netEv.iErrorCode[FD_ACCEPT_BIT]) != 0)
	    async_error(INETP(desc), err);
	else
	    tcp_inet_input(desc, event);
    }
    if (netEv.lNetworkEvents & FD_CLOSE) {
	/* error in err = netEv.iErrorCode[FD_CLOSE_BIT] */
	DEBUGF(("Detected close in %s, line %d\r\n", __FILE__, __LINE__));
	tcp_recv_closed(desc);
    }
    return;
    DEBUGF(("tcp_inet_event(%ld) }\r\n", (long)desc->inet.port));
 error:
    DEBUGF(("tcp_inet_event(%ld) error}\r\n", (long)desc->inet.port));
    return; /*-1;*/
}

#endif /* WIN32 */


/* socket has input:
** 1. TCP_STATE_ACCEPTING  => non block accept ? 
** 2. TCP_STATE_CONNECTED => read input
*/
static int tcp_inet_input(tcp_descriptor* desc, HANDLE event)
{
    int ret = 0;
    int len;
    int ix;
    SOCKET s;
    tcp_descriptor* a_desc;

    DEBUGF(("tcp_inet_input(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s));
    if (desc->inet.state == TCP_STATE_LISTENING) {
	sock_select(INETP(desc),FD_ACCEPT,0);
	ix = desc->i_ix;
	desc->inet.state = TCP_STATE_LISTEN; /* restore state */
	desc->i_ix = -1;

	/* port closed? */
	if (((a_desc = (tcp_descriptor*)inet_desc_table[ix]) == NULL) || 
	    (a_desc->inet.state != TCP_STATE_ACCEPTING))
	    goto done;

	len = sizeof(a_desc->inet.remote);
	s = sock_accept(desc->inet.s, 
			(struct sockaddr*) &a_desc->inet.remote, &len);

	driver_cancel_timer(a_desc->inet.port); /* posssibly cancel a timer */

	if (s == INVALID_SOCKET) {
	    a_desc->inet.state = TCP_STATE_CLOSED;
	    ret = async_error(INETP(a_desc), sock_errno());
	    goto done;
	}
	else {
	    a_desc->inet.s = s;
	    if ((a_desc->inet.event = sock_create_event(INETP(a_desc))) ==
		INVALID_EVENT) {
		sock_close(s);
		a_desc->inet.s = INVALID_SOCKET;
		ret =  async_error(INETP(a_desc), sock_errno());
		goto done;
	    }
	    SET_NONBLOCKING(a_desc->inet.s);
#ifdef __WIN32__
	    driver_select(a_desc->inet.port, a_desc->inet.event, DO_READ, 1);
#endif
	    a_desc->inet.state = TCP_STATE_CONNECTED;
	    ret =  async_ok(INETP(a_desc));
	    goto done;
	}
    }
    else if (IS_CONNECTED(INETP(desc))) {
	ret = tcp_recv(desc, 0);
	goto done;
    }
    else {
	/* maybe a close op from connection attempt?? */
	sock_select(INETP(desc),FD_ACCEPT,0);
	DEBUGF(("tcp_inet_input(%ld): s=%d bad state: %04x\r\n", 
		(long)desc->inet.port, desc->inet.s, desc->inet.state));
    }
 done:
    DEBUGF(("tcp_inet_input(%ld) }\r\n", (long)desc->inet.port));
    return ret;
}

static int tcp_send_error(tcp_descriptor* desc, int err)
{
    inet_address other;
    int sz = sizeof(other);
    int code;

    if (IS_BUSY(INETP(desc))) {
	desc->inet.caller = desc->inet.busy_caller;
	if (desc->busy_on_send) {
	    driver_cancel_timer(desc->inet.port);
	    desc->busy_on_send = 0;	
	}
	desc->inet.state &= ~INET_F_BUSY;
	set_busy_port(desc->inet.port, 0);
    }

    code = sock_peer(desc->inet.s,(struct sockaddr*) &other,&sz);
    if ((code == SOCKET_ERROR) && (sock_errno() == ENOTCONN ||
				   sock_errno() == EPIPE)) {
	DEBUGF(("driver_failure_eof(%ld) in %s, line %d\r\n",
		(long)desc->inet.port, __FILE__, __LINE__));
	if (desc->inet.active) {
	    tcp_closed_message(desc);
	    inet_reply_error_am(INETP(desc), am_closed);
	    if (desc->inet.exitf)
		driver_exit(desc->inet.port, 0);
	    else
		desc_close(INETP(desc));
	}
	else {
	    tcp_clear_output(desc);
	    tcp_clear_input(desc);
	    tcp_close_check(desc);
	    erl_inet_close(INETP(desc));
	    inet_reply_error_am(INETP(desc), am_closed);
	}
    }
    else  {
	inet_reply_error(INETP(desc), sock_errno());
    }
    return -1;
}

/*
** Send non-blocking vector data
*/
static int tcp_sendv(tcp_descriptor* desc, ErlIOVec* ev)
{
    int sz;
    char buf[4];
    int h_len;
    int n;
    ErlDrvPort ix = desc->inet.port;
    int len = ev->size;

    switch(desc->inet.htype) {
    case TCP_PB_1: 
	put_int8(len, buf);
	h_len = 1;
	break;
    case TCP_PB_2: 
	put_int16(len, buf);
	h_len = 2; 
	break;
    case TCP_PB_4: 
	put_int32(len, buf);
	h_len = 4; 
	break;
    default:
	if (len == 0)
	    return 0;
	h_len = 0;
	break;
    }

    inet_output_count(INETP(desc), len+h_len);

    if (h_len > 0) {
	ev->iov[0].iov_base = buf;
	ev->iov[0].iov_len = h_len;
	ev->size += h_len;
    }

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enqv(ix, ev, 0);
	if (sz+ev->size >= desc->high) {
	    DEBUGF(("tcp_sendv(%ld): s=%d, sender forced busy\r\n",
		    (long)desc->inet.port, desc->inet.s));
	    desc->inet.state |= INET_F_BUSY;  /* mark for low-watermark */
	    desc->inet.busy_caller = desc->inet.caller;
	    set_busy_port(desc->inet.port, 1);
	    if (desc->send_timeout != INET_INFINITY) {
		desc->busy_on_send = 1;
		driver_set_timer(desc->inet.port, desc->send_timeout);
	    }
	    return 1;
	}
    }
    else {
	int vsize = (ev->vsize > MAX_VSIZE) ? MAX_VSIZE : ev->vsize;
	
	DEBUGF(("tcp_sendv(%ld): s=%d, about to send %d,%d bytes\r\n",
		(long)desc->inet.port, desc->inet.s, h_len, len));
	if (desc->fdelay_send) {
	    n = 0;
	} else if (sock_sendv(desc->inet.s, ev->iov, vsize, &n, 0) 
		   == SOCKET_ERROR) {
	    if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		int err = sock_errno();
		DEBUGF(("tcp_sendv(%ld): s=%d, "
			"sock_sendv(size=2) errno = %d\r\n",
			(long)desc->inet.port, desc->inet.s, err));
		return tcp_send_error(desc, err);
	    }
	    n = 0;
	}
	else if (n == ev->size) {
	    ASSERT(NO_SUBSCRIBERS(&INETP(desc)->empty_out_q_subs));
	    return 0;
	}

	DEBUGF(("tcp_sendv(%ld): s=%d, Send failed, queuing\r\n", 
		(long)desc->inet.port, desc->inet.s));
	driver_enqv(ix, ev, n); 
	sock_select(INETP(desc),(FD_WRITE|FD_CLOSE), 1);
    }
    return 0;
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
    ErlDrvPort ix = desc->inet.port;
    SysIOVec iov[2];

    switch(desc->inet.htype) {
    case TCP_PB_1: 
	put_int8(len, buf);
	h_len = 1;
	break;
    case TCP_PB_2: 
	put_int16(len, buf);
	h_len = 2; 
	break;
    case TCP_PB_4: 
	put_int32(len, buf);
	h_len = 4; 
	break;
    default:
	if (len == 0)
	    return 0;
	h_len = 0;
	break;
    }

    inet_output_count(INETP(desc), len+h_len);


    if ((sz = driver_sizeq(ix)) > 0) {
	if (h_len > 0)
	    driver_enq(ix, buf, h_len);
	driver_enq(ix, ptr, len);
	if (sz+h_len+len >= desc->high) {
	    DEBUGF(("tcp_send(%ld): s=%d, sender forced busy\r\n",
		    (long)desc->inet.port, desc->inet.s));
	    desc->inet.state |= INET_F_BUSY;  /* mark for low-watermark */
	    desc->inet.busy_caller = desc->inet.caller;
	    set_busy_port(desc->inet.port, 1);
	    if (desc->send_timeout != INET_INFINITY) {
		desc->busy_on_send = 1;
		driver_set_timer(desc->inet.port, desc->send_timeout);
	    }
	    return 1;
	}
    }
    else {
	iov[0].iov_base = buf;
	iov[0].iov_len = h_len;
	iov[1].iov_base = ptr;
	iov[1].iov_len = len;

	DEBUGF(("tcp_send(%ld): s=%d, about to send %d,%d bytes\r\n",
		(long)desc->inet.port, desc->inet.s, h_len, len));
	if (desc->fdelay_send) {
	    n = 0;
	} else 	if (sock_sendv(desc->inet.s,iov,2,&n,0) == SOCKET_ERROR) {
	    if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		int err = sock_errno();
		DEBUGF(("tcp_send(%ld): s=%d,sock_sendv(size=2) errno = %d\r\n",
			(long)desc->inet.port, desc->inet.s, err));
		return tcp_send_error(desc, err);
	    }
	    n = 0;
	}
	else if (n == len+h_len) {
	    ASSERT(NO_SUBSCRIBERS(&INETP(desc)->empty_out_q_subs));
	    return 0;
	}

	DEBUGF(("tcp_send(%ld): s=%d, Send failed, queuing", 
		(long)desc->inet.port, desc->inet.s));

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

static void tcp_inet_drv_output(ErlDrvData data, ErlDrvEvent event)
{
    (void)tcp_inet_output((tcp_descriptor*)data, (HANDLE)event);
}

static void tcp_inet_drv_input(ErlDrvData data, ErlDrvEvent event)
{
    (void)tcp_inet_input((tcp_descriptor*)data, (HANDLE)event);
}

/* socket ready for ouput:
** 1. TCP_STATE_CONNECTING => non block connect ?
** 2. TCP_STATE_CONNECTED  => write output
*/
static int tcp_inet_output(tcp_descriptor* desc, HANDLE event)
{
    int ret = 0;
    ErlDrvPort ix = desc->inet.port;

    DEBUGF(("tcp_inet_output(%ld) {s=%d\r\n", 
	    (long)desc->inet.port, desc->inet.s));
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

#ifndef SO_ERROR
	{
	    int sz = sizeof(desc->inet.remote);
	    int code = sock_peer(desc->inet.s,
				 (struct sockaddr*) &desc->inet.remote, &sz);

	    if (code == SOCKET_ERROR) {
		desc->inet.state = TCP_STATE_BOUND;  /* restore state */
		ret =  async_error(INETP(desc), sock_errno());
		goto done;
	    }
	}
#else
	{
	    int error = 0;	/* Has to be initiated, we check it */
	    int sz = sizeof(error); /* even if we get -1 */
	    int code = sock_getopt(desc->inet.s, SOL_SOCKET, SO_ERROR, 
				   (void *)&error, &sz);

	    if ((code < 0) || error) {
		desc->inet.state = TCP_STATE_BOUND;  /* restore state */
		ret = async_error(INETP(desc), error);
		goto done;
	    }
	}
#endif /* SOCKOPT_CONNECT_STAT */
#endif /* !__WIN32__ */

	desc->inet.state = TCP_STATE_CONNECTED;
	if (desc->inet.active)
	    sock_select(INETP(desc),(FD_READ|FD_CLOSE),1);
	async_ok(INETP(desc));
    }
    else if (IS_CONNECTED(INETP(desc))) {
	for (;;) {
	    int vsize;
	    int n;
	    SysIOVec* iov;

	    if ((iov = driver_peekq(ix, &vsize)) == NULL) {
		sock_select(INETP(desc), FD_WRITE, 0);
		send_empty_out_q_msgs(INETP(desc));
		goto done;
	    }
	    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
	    DEBUGF(("tcp_inet_output(%ld): s=%d, About to send %d items\r\n", 
		    (long)desc->inet.port, desc->inet.s, vsize));
	    if (sock_sendv(desc->inet.s, iov, vsize, &n, 0)==SOCKET_ERROR) {
		if ((sock_errno() != ERRNO_BLOCK) && (sock_errno() != EINTR)) {
		    DEBUGF(("tcp_inet_output(%ld): sock_sendv(%d) errno = %d\r\n",
			    (long)desc->inet.port, vsize, sock_errno()));
		    ret =  tcp_send_error(desc, sock_errno());
		    goto done;
		}
		goto done;
	    }
	    if (driver_deq(ix, n) <= desc->low) {
		if (IS_BUSY(INETP(desc))) {
		    desc->inet.caller = desc->inet.busy_caller;
		    desc->inet.state &= ~INET_F_BUSY;
		    set_busy_port(desc->inet.port, 0);
		    /* if we have a timer then cancel and send ok to client */
		    if (desc->busy_on_send) {
			driver_cancel_timer(desc->inet.port);
			desc->busy_on_send = 0;
		    }
		    inet_reply_ok(INETP(desc));
		}
	    }
	}
    }
    else {
	sock_select(INETP(desc),FD_CONNECT,0);
	DEBUGF(("tcp_inet_output(%ld): bad state: %04x\r\n", 
		(long)desc->inet.port, desc->inet.state));
    }
 done:
    DEBUGF(("tcp_inet_output(%ld) }\r\n", (long)desc->inet.port));
    return ret;
}

/*-----------------------------------------------------------------------------

   UDP

-----------------------------------------------------------------------------*/

#if defined(HAVE_SO_BSDCOMPAT)
#if defined(__linux__)
#include <sys/utsname.h>
static int should_use_so_bsdcompat(void)
{
    static int init_done;
    static int so_bsdcompat_is_obsolete;

    if (!init_done) {
	struct utsname utsname;
	unsigned int version, patchlevel;

	init_done = 1;
	if (uname(&utsname) < 0) {
	    fprintf(stderr, "uname: %s\r\n", strerror(errno));
	    return 1;
	}
	/* Format is <version>.<patchlevel>.<sublevel><extraversion>
	   where the first three are unsigned integers and the last
	   is an arbitrary string. We only care about the first two. */
	if (sscanf(utsname.release, "%u.%u", &version, &patchlevel) != 2) {
	    fprintf(stderr, "uname: unexpected release '%s'\r\n",
		    utsname.release);
	    return 1;
	}
	/* SO_BSCOMPAT is deprecated and triggers warnings in 2.5
	   kernels. It is a no-op in 2.4 but not in 2.2 kernels. */
	if (version > 2 || (version == 2 && patchlevel >= 5))
	    so_bsdcompat_is_obsolete = 1;
    }
    return !so_bsdcompat_is_obsolete;
}
#else	/* __linux__ */
#define should_use_so_bsdcompat() 1
#endif	/* __linux__ */
#endif	/* HAVE_SO_BSDCOMPAT */

static int udp_inet_init()
{
    return 0;
}


static ErlDrvData udp_inet_start(ErlDrvPort port, char* args)
{
    udp_descriptor* desc;
  
    desc = (udp_descriptor*) inet_start(port, sizeof(udp_descriptor));
    if (desc == NULL)
	return ERL_DRV_ERROR_ERRNO;
    return (ErlDrvData) desc;
}

static void udp_inet_stop(ErlDrvData e)
{
    /* There should *never* be any "empty out q" subscribers on
       an udp socket! */
    ASSERT(NO_SUBSCRIBERS(&INETP((udp_descriptor*) e)->empty_out_q_subs));
    inet_stop(&((udp_descriptor*)e)->inet);
}

static int udp_error(udp_descriptor* desc, int err)
{
    if (!desc->inet.active)
	async_error(INETP(desc), err);
    driver_failure_posix(desc->inet.port, err);
    return -1;
}

static int udp_inet_ctl(ErlDrvData e, unsigned int cmd, char* buf, int len,
			char** rbuf, int rsize)
{
    int replen;
    udp_descriptor* desc = (udp_descriptor*)e;

    switch(cmd) {
    case INET_REQ_OPEN:   /* open socket and return internal index */
	DEBUGF(("udp_inet_ctl(%ld): OPEN\r\n", (long)desc->inet.port)); 
	if ((len == 1) && (buf[0] == INET_AF_INET))
	    replen = inet_ctl_open(INETP(desc),AF_INET,SOCK_DGRAM,rbuf,rsize);
#if defined(HAVE_IN6) && defined(AF_INET6)
	else if ((len == 1) && (buf[0] == INET_AF_INET6))
	    replen = inet_ctl_open(INETP(desc),AF_INET6,SOCK_DGRAM,rbuf,rsize);
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

	if ((*rbuf)[0] != INET_REP_ERROR) {
	    if (desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
#ifdef HAVE_SO_BSDCOMPAT
	    /*
	     * Make sure that sending udp packets to a non existing port on an
	     * existing machine doesn't close the socket. (Linux behaves this
	     * way)
	     */
	    if (should_use_so_bsdcompat()) {
		int one = 1;
		/* Ignore errors */
		sock_setopt(desc->inet.s, SOL_SOCKET, SO_BSDCOMPAT, &one,
			    sizeof(one));
	    }
#endif
	}
	return replen;


    case INET_REQ_FDOPEN:   /* pass in an open (and bound) socket */
	DEBUGF(("udp_inet_ctl(%ld): FDOPEN\r\n", (long)desc->inet.port)); 
	if ((len == 5) && (buf[0] == INET_AF_INET))
	    replen = inet_ctl_fdopen(INETP(desc), AF_INET, SOCK_DGRAM,
				     (SOCKET)get_int32(buf+1),rbuf,rsize);
#if defined(HAVE_IN6) && defined(AF_INET6)
	else if ((len == 5) && (buf[0] == INET_AF_INET6))
	    replen = inet_ctl_fdopen(INETP(desc), AF_INET6, SOCK_DGRAM,
				 (SOCKET)get_int32(buf+1),rbuf,rsize);
#endif
	else
	    return ctl_error(EINVAL, rbuf, rsize);

	if ((*rbuf)[0] != INET_REP_ERROR) {
	    if (desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
#ifdef HAVE_SO_BSDCOMPAT
	    /*
	     * Make sure that sending udp packets to a non existing port on an
	     * existing machine doesn't close the socket. (Linux behaves this
	     * way)
	     */
	    if (should_use_so_bsdcompat()) {
		int one = 1;
		/* Ignore errors */
		sock_setopt(desc->inet.s, SOL_SOCKET, SO_BSDCOMPAT, &one,
			    sizeof(one));
	    }
#endif
	}
	return replen;


    case INET_REQ_CLOSE:
	DEBUGF(("udp_inet_ctl(%ld): CLOSE\r\n", (long)desc->inet.port)); 
	erl_inet_close(INETP(desc));
	return ctl_reply(INET_REP_OK, NULL, 0, rbuf, rsize);
	return 0;


    case INET_REQ_CONNECT:  {
	int code;
	char tbuf[2];
	unsigned timeout;

	DEBUGF(("udp_inet_ctl(%ld): CONNECT\r\n", (long)desc->inet.port)); 
	
	/* INPUT: [ Timeout(4), Port(2), Address(N) ] */

	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);

	if (len == 0) {
	    sock_connect(desc->inet.s, (struct sockaddr*) NULL, 0);
	    desc->inet.state &= ~INET_F_ACTIVE;
	    enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
	    async_ok(INETP(desc));
	}
	else if (len < 6)
	    return ctl_error(EINVAL, rbuf, rsize);
	else {
	    timeout = get_int32(buf); /* IGNORED */
	    buf += 4;
	    len -= 4;
	    if (inet_set_address(desc->inet.sfamily, 
				 &desc->inet.remote, buf, &len) == NULL)
		return ctl_error(EINVAL, rbuf, rsize);
	    code = sock_connect(desc->inet.s,
				(struct sockaddr*) &desc->inet.remote,
				len);
	    if (code == SOCKET_ERROR) {
		sock_connect(desc->inet.s, (struct sockaddr*) NULL, 0);
		desc->inet.state &= ~INET_F_ACTIVE;
		return ctl_error(sock_errno(), rbuf, rsize);
	    }
	    else /* ok we are connected */ {
		enq_async(INETP(desc), tbuf, INET_REQ_CONNECT);
		desc->inet.state |= INET_F_ACTIVE;
		async_ok(INETP(desc));
	    }
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }

    case UDP_REQ_RECV: {
	unsigned timeout;
	char tbuf[2];

	DEBUGF(("udp_inet_ctl(%ld): RECV\r\n", (long)desc->inet.port)); 
	/* INPUT: Timeout(4), Length(4) */
	if (!IS_OPEN(INETP(desc)))
	    return ctl_xerror(EXBADPORT, rbuf, rsize);
	if (!IS_BOUND(INETP(desc)))
	    return ctl_error(EINVAL, rbuf, rsize);
	if (desc->inet.active || (len != 8))
	    return ctl_error(EINVAL, rbuf, rsize);
	timeout = get_int32(buf);
	buf += 4;
	len = get_int32(buf);	 /* ignore ?? */

	if (enq_async(INETP(desc), tbuf, UDP_REQ_RECV) < 0)
	    return ctl_error(EALREADY, rbuf, rsize);

	if (udp_inet_input(desc, desc->inet.event) == 0) {
	    if (timeout == 0)
		async_error_am(INETP(desc), am_timeout);
	    else {
		if (timeout != INET_INFINITY)
		    driver_set_timer(desc->inet.port, timeout);
	    }
	}
	return ctl_reply(INET_REP_OK, tbuf, 2, rbuf, rsize);
    }
	
    default:
	return inet_ctl(INETP(desc), cmd, buf, len, rbuf, rsize);
    }
}

static void udp_inet_timeout(ErlDrvData e)
{
    if (!((udp_descriptor*)e)->inet.active)
	sock_select(INETP((udp_descriptor*)e),FD_READ,0);
    async_error_am(INETP((udp_descriptor*)e), am_timeout);
}


/* input should: P1 P0 Address buffer */
static void udp_inet_command(ErlDrvData e, char* buf, int len)
{
    udp_descriptor* desc = (udp_descriptor*)e;
    char* ptr = buf;
    char* qtr;
    int sz;
    int code;
    inet_address other;

    desc->inet.caller = driver_caller(desc->inet.port);

    if (!IS_OPEN(INETP(desc))) {
	inet_reply_error(INETP(desc), EINVAL);
	return;
    }
    if (!IS_BOUND(INETP(desc))) {
	inet_reply_error(INETP(desc), EINVAL);
	return;
    }

    sz = len;
    qtr = inet_set_address(desc->inet.sfamily, &other, ptr, &sz);
    if (qtr == NULL) {
	inet_reply_error(INETP(desc), EINVAL);
	return;
    }
    len -= (qtr - ptr);
    ptr = qtr;
    inet_output_count(INETP(desc), len);

    if (desc->inet.state & INET_F_ACTIVE) { /* connected (ignore address) */
	code = sock_send(desc->inet.s, ptr, len, 0);
	if (code == SOCKET_ERROR) {
	    int err = sock_errno();
	    inet_reply_error(INETP(desc), err);
	}
	else
	  inet_reply_ok(INETP(desc));	
    }
    else {
	code = sock_sendto(desc->inet.s, ptr, len, 0,
			   (struct sockaddr*)&other, sz);
	if (code == SOCKET_ERROR) {
	    int err = sock_errno();
	    inet_reply_error(INETP(desc), err);
	}
	else
	  inet_reply_ok(INETP(desc));
    }
}


#ifdef __WIN32__

static void udp_inet_event(ErlDrvData e, ErlDrvEvent event)
{
    udp_descriptor* desc = (udp_descriptor*)e;
    WSANETWORKEVENTS netEv;

    if ((winSock.WSAEnumNetworkEvents)(desc->inet.s, desc->inet.event,
				       &netEv) != 0) {
	DEBUGF(( "port %d: EnumNetwrokEvents = %d\r\n", 
		desc->inet.port, sock_errno() ));
	return; /* -1; */
    }
    if (netEv.lNetworkEvents == 0)  /* NOTHING */
	return; /* 0; */
    if (netEv.lNetworkEvents & FD_READ)
	udp_inet_input(desc, (HANDLE)event);
}

#endif

static void udp_inet_drv_input(ErlDrvData e, ErlDrvEvent event)
{
    (void)udp_inet_input((udp_descriptor*)e, (HANDLE)event);
}

static int udp_inet_input(udp_descriptor* desc, HANDLE event)
{
    int n;
    int len;
    inet_address other;
    char abuf[sizeof(inet_address)];  /* buffer address */
    int sz;
    char* ptr;
    ErlDrvBinary* buf; /* binary */
    int packet_count = INET_UDP_POLL;
    int count = 0;   /* number of packets delivered to owner */

    while(packet_count--) {
	len = sizeof(other);
	sz = desc->inet.bufsz;
	/* Allocate space for message and address */
	if ((buf = alloc_buffer(sz+len)) == NULL)
	    return udp_error(desc, ENOMEM);
	ptr = buf->orig_bytes + len;  /* point to message part */

	/* Note: On Windows NT, recvfrom() fails if the socket is connected. */
	if (desc->inet.state & INET_F_ACTIVE) {
	    n = sock_recv(desc->inet.s, ptr, sz, 0);
	    other = desc->inet.remote;
	}
	else
	    n = sock_recvfrom(desc->inet.s, ptr, sz, 0,
			      (struct sockaddr*)&other, &len);
	if (n == SOCKET_ERROR) {
	    int err = sock_errno();
	    release_buffer(buf);
	    if (err != ERRNO_BLOCK) {
		if (!desc->inet.active) {
		    async_error(INETP(desc), err);
		    driver_cancel_timer(desc->inet.port);
		    sock_select(INETP(desc),FD_READ,0);
		}
		else {
		    udp_error_message(desc, err);
		}
	    }
	    else if (!desc->inet.active)
		sock_select(INETP(desc),FD_READ,1);
	    return count;		/* strange, not ready */
	}
	else {
	    int offs;
	    int nsz;
	    int code;

	    inet_input_count(INETP(desc), n);

	    inet_get_address(desc->inet.sfamily, abuf, &other, &len);

	    /* copy formatted address to ptr len is actual length */
	    sys_memcpy(ptr - len, abuf, len); 
	    ptr -= len;
	    nsz = n + len;                /* nsz = data + address */
	    offs = ptr - buf->orig_bytes; /* initial pointer offset */

	    /* check if we need to reallocate binary */
	    if ((desc->inet.mode == INET_MODE_BINARY) &&
		(desc->inet.hsz < n) && (nsz < BIN_REALLOC_LIMIT(sz))) {
		ErlDrvBinary* tmp;
		if ((tmp = realloc_buffer(buf,nsz+offs)) != NULL)
		    buf = tmp;
	    }
	    code = udp_reply_binary_data(INETP(desc),(unsigned int)len,buf,offs,nsz);
	    free_buffer(buf);
	    if (code < 0)
		return count;
	    count++;
	    if (!desc->inet.active) {
		driver_cancel_timer(desc->inet.port); /* possibly cancel */
		sock_select(INETP(desc),FD_READ,0);
		return count;  /* passive mode (read one packet only) */
	    }
	}
    }
    return count;
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
    winSock.getservbyport = (LPFN_GETSERVBYPORT)
	GetProcAddress(module, "getservbyport");
    winSock.getsockname = (LPFN_GETSOCKNAME) 
	GetProcAddress(module, "getsockname");
    winSock.getpeername = (LPFN_GETPEERNAME) 
	GetProcAddress(module, "getpeername");
    winSock.WSAIoctl = (LPFN_WSAIOCTL)
	GetProcAddress(module, "WSAIoctl");
    /*
     * Check that all of the pointers got a non-NULL value.
     */
    for (i = 0; i < sizeof(WinSockFuncs)/sizeof(void*); i++) {
	if (((char **)&winSock)[i] == NULL) {
	    DEBUGF(("Function %d not initialized\r\n", i));
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

/*-----------------------------------------------------------------------------

   Subscription

-----------------------------------------------------------------------------*/

static int
save_subscriber(subs, subs_pid)
subs_list *subs; ErlDrvTermData subs_pid;
{
  subs_list *tmp;

  if(NO_SUBSCRIBERS(subs)) {
    subs->subscriber = subs_pid;
    subs->next = NULL;
  }
  else {
    tmp = subs->next;
    subs->next = ALLOC(sizeof(subs_list));
    if(subs->next == NULL) {
      subs->next = tmp;
      return 0;
    }
    subs->next->subscriber = subs_pid;
    subs->next->next = tmp;
  }
  return 1;
}

static void
free_subscribers(subs)
subs_list *subs;
{
  subs_list *this;
  subs_list *next;

  this = subs->next;
  while(this) {
    next = this->next;
    FREE((void *) this);
    this = next;
  }

  subs->subscriber = NO_PROCESS;
  subs->next = NULL;
}

static void
send_to_subscribers(port, subs, free_subs, msg, msg_len)
ErlDrvPort port;
subs_list *subs;
int free_subs;
ErlDrvTermData msg[];
int msg_len;
{
  subs_list *this;
  subs_list *next;
  int first = 1;

  if(NO_SUBSCRIBERS(subs))
    return;

  this = subs;
  while(this) {
    
    (void) driver_send_term(port, this->subscriber, msg, msg_len);

    if(free_subs && !first) {
      next = this->next;
      FREE((void *) this);
      this = next;
    }
    else
      this = this->next;
    first = 0;
  }

  if(free_subs) {
    subs->subscriber = NO_PROCESS;
    subs->next = NULL;
  }

}

/*
 * A *very* limited socket interface. Used by the memory tracer
 * (erl_mtrace.c).
 */
#include "erl_sock.h"

erts_sock_t erts_sock_open(void)
{
    SOCKET s;
    
    if(!sock_init())
	return ERTS_SOCK_INVALID_SOCKET;

    s = sock_open(AF_INET, SOCK_STREAM, 0);

    if (s == INVALID_SOCKET)
	return ERTS_SOCK_INVALID_SOCKET;

    return (erts_sock_t) s;
}

void erts_sock_close(erts_sock_t socket)
{
    if (socket != ERTS_SOCK_INVALID_SOCKET)
	sock_close((SOCKET) socket);
}


int erts_sock_connect(erts_sock_t socket, byte *ip_addr, int len, Uint16 port)
{
    SOCKET s = (SOCKET) socket;
    byte buf[2 + 4];
    int blen = 6;
    inet_address addr;

    if (socket == ERTS_SOCK_INVALID_SOCKET || len != 4)
	return 0;

    put_int16(port, buf);
    memcpy((void *) (buf + 2), (void *) ip_addr, 4);

    if (!inet_set_address(AF_INET, (inet_address *) &addr, buf, &blen))
	return 0;

    if (SOCKET_ERROR == sock_connect(s,
				     (struct sockaddr *) &addr,
				     sizeof(struct sockaddr_in)))
	return 0;
    return 1;
}

Sint erts_sock_send(erts_sock_t socket, const void *buf, Sint len)
{
    return (Sint) sock_send((SOCKET) socket, buf, (size_t) len, 0);
}

int erts_sock_errno()
{
    return sock_errno();
}
