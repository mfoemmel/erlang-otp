/*
** Definition of all erlang commands
**
*/
#ifndef __ERLTK_H__
#define __ERLTK_H__

#define ERL_TK_BIND          1
#define ERL_TK_DESTROY       2
#define ERL_TK_LOWER         3
#define ERL_TK_RAISE         4
#define ERL_TK_BELL          5
#define ERL_TK_BUTTON        6
#define ERL_TK_CHECKBUTTON   7
#define ERL_TK_RADIOBUTTON   8
#define ERL_TK_LABEL         9
#define ERL_TK_UPDATE        11
#define ERL_TK_WINFO         12
#define ERL_TK_WM            13
#define ERL_TK_BINDTAGS      14
#define ERL_TK_CANVAS        17
#define ERL_TK_CLIPBOARD     18
#define ERL_TK_ENTRY         19
#define ERL_TK_FRAME         20
#define ERL_TK_FOCUS         21
#define ERL_TK_GRAB          22
#define ERL_TK_IMAGE         23
#define ERL_TK_LISTBOX       24
#define ERL_TK_MENU          25
#define ERL_TK_MENUBUTTON    26
#define ERL_TK_MESSAGE       27
#define ERL_TK_OPTION        28
#define ERL_TK_PACK          29
#define ERL_TK_PLACE         30
#define ERL_TK_SCALE         31
#define ERL_TK_SCROLLBAR     32
#define ERL_TK_SELECTION     33
#define ERL_TK_TEXT          34
#define ERL_TK_TK            35
#define ERL_TK_CMD           36
#define ERL_TK_GRID          37
#define ERL_TK_TOPLEVEL      38

#define ERL_TK_SETVAR        46
#define ERL_TK_GETVAR        47
#define ERL_TK_EVENT         48
#define ERL_TK_WLINK         49
#define ERL_TK_OPERATION     50


/* Reply codes */
#define TK_ERL_OK            0
#define TK_ERL_ERROR         1
#define TK_ERL_EVENT         2
#define TK_ERL_INVOKE        3
#define TK_ERL_TKERROR       4
#define TK_ERL_TKSCREEN      5
#define TK_ERL_OPERATION     6


#define TK_OP_OFFSET         0  /* argc is offset into buffer */
#define TK_OP_OPTION         1  /* argc is offset into tkopt[] */
#define TK_OP_STRING         2  /* argc is offset into tkstr[] */

#define GET_INT8(p)  ((p)[0])
#define GET_INT16(p) (((p)[0]<<8) | (p)[1])
#define GET_INT24(p) (((p)[0]<<16) | ((p)[1]<<8) | (p)[2])
#define GET_INT32(p) (((p)[0]<<24) | ((p)[1]<<16) | ((p)[2]<<8) | (p)[3])

#define PUT_INT8(p, x) ((p)[0] = (x))

#define PUT_INT16(p, x) (((p)[0] = ((x) >> 8) & 0xff), \
			 ((p)[1] = (x) & 0xff))

#define PUT_INT24(p, x) (((p)[0] = ((x) >> 16) & 0xff), \
			 ((p)[1] = ((x) >> 8) & 0xff), \
			 ((p)[2] = (x) & 0xff))

#define PUT_INT32(p, x) (((p)[0] = ((x) >> 24) & 0xff), \
			 ((p)[1] = ((x) >> 16) & 0xff), \
			 ((p)[2] = ((x) >> 8) & 0xff), \
			 ((p)[3] = (x) & 0xff))

#ifdef __WIN32__

#define INCL_WINSOCK_API_TYPEDEFS 1
#include <winsock2.h>
#include <windows.h>
#include <process.h>



typedef struct {
  LPFN_WSASTARTUP			WSAStartup;
  LPFN_WSACLEANUP			WSACleanup;
  LPFN_WSAGETLASTERROR			WSAGetLastError;
  LPFN_WSAWAITFORMULTIPLEEVENTS         WSAWaitForMultipleEvents;
  LPFN_WSACREATEEVENT			WSACreateEvent;
  LPFN_WSACLOSEEVENT			WSACloseEvent;
  LPFN_WSARESETEVENT			WSAResetEvent;
  LPFN_WSAEVENTSELECT			WSAEventSelect;
  LPFN_WSAENUMNETWORKEVENTS		WSAEnumNetworkEvents;
  LPFN_WSASEND				WSASend;
  LPFN_ACCEPT				accept;
  LPFN_BIND				bind;
  LPFN_CLOSESOCKET			closesocket;
  LPFN_CONNECT				connect;
  LPFN_IOCTLSOCKET			ioctlsocket;
  LPFN_GETSOCKOPT			getsockopt;
  LPFN_HTONL				htonl;
  LPFN_HTONS				htons;
  LPFN_INET_ADDR			inet_addr;
  LPFN_INET_NTOA			inet_ntoa;
  LPFN_LISTEN				listen;
  LPFN_NTOHS				ntohs;
  LPFN_NTOHL				ntohl;
  LPFN_RECV				recv;
  LPFN_SEND				send;
  LPFN_RECVFROM				recvfrom;
  LPFN_SENDTO				sendto;
  LPFN_SETSOCKOPT			setsockopt;
  LPFN_SHUTDOWN				shutdown;
  LPFN_SOCKET				socket;
  LPFN_GETHOSTBYNAME			gethostbyname;
  LPFN_GETHOSTBYADDR			gethostbyaddr;
  LPFN_GETHOSTNAME			gethostname;
  LPFN_GETSERVBYNAME			getservbyname;
  LPFN_GETSOCKNAME			getsockname;
  LPFN_GETPEERNAME			getpeername;
} Ws2Funcs;

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
#define ENAMETOOLONG            WSAENAMETOOLONG
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
#define ENOTEMPTY               WSAENOTEMPTY
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

#define INVALID_EVENT           WSA_INVALID_EVENT

typedef WSABUF sock_iovec;
#define iov_base buf
#define iov_len  len

static Ws2Funcs Ws2;

#define sock_open(af, type, proto) (*Ws2.socket)(af, type, proto)
#define sock_close(s)              (*Ws2.closesocket)(s)
#define sock_shutdown(s,how)       (*Ws2.shutdown)(s,how)
#define sock_accept(s, addr, len)  (*Ws2.accept)(s, addr, len)
#define sock_connect(s, addr, len) (*Ws2.connect)(s, addr, len)
#define sock_listen(s, b)          (*Ws2.listen)(s, b)
#define sock_bind(s, addr, len)    (*Ws2.bind)(s, addr, len)
#define sock_getopt(s,t,n,v,l)     (*Ws2.getsockopt)(s,t,n,v,l)
#define sock_setopt(s,t,n,v,l)     (*Ws2.setsockopt)(s,t,n,v,l)
#define sock_name(s, addr, len)    (*Ws2.getsockname)(s, addr, len)
#define sock_peer(s, addr, len)    (*Ws2.getpeername)(s, addr, len)
#define sock_ntohs(x)              (*Ws2.ntohs)(x)
#define sock_ntohl(x)              (*Ws2.ntohl)(x)
#define sock_htons(x)              (*Ws2.htons)(x)
#define sock_htonl(x)              (*Ws2.htonl)(x)
#define sock_send(s,buf,len,flag)  (*Ws2.send)((s),(buf),(len),(flag))
#define sock_write(s,buf,len)      (*Ws2.send)((s),(buf),(len),0)
#define sock_sendv(s, vec, size, np, flag) \
                (*Ws2.WSASend)(s,vec,size,np,flag,NULL,NULL)
#define sock_recv(s,buf,len,flag)  (*Ws2.recv)((s),(buf),(len),(flag))
#define sock_read(s,buf,len)       (*Ws2.recv)((s),(buf),(len),0)

#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                (*Ws2.recvfrom)(s,buf,blen,flag,addr,alen)
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                (*Ws2.sendto)(s,buf,blen,flag,addr,alen)
#define sock_hostname(buf, len)    (*Ws2.gethostname)(buf, len)

#define sock_errno()               (*Ws2.WSAGetLastError)()
#define sock_create_event(d)       (*Ws2.WSACreateEvent)()
#define sock_close_event(e)        (*Ws2.WSACloseEvent)(e)

#define sock_select(d, flags, onoff) do { \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        (*Ws2.WSAEventSelect)((d)->s, (d)->event, (d)->event_mask); \
  } while(0)

#define SET_BLOCKING(s)           (*Ws2.ioctlsocket)(s, FIONBIO, &zero_value)
#define SET_NONBLOCKING(s)        (*Ws2.ioctlsocket)(s, FIONBIO, &one_value)

static unsigned long zero_value = 0;
static unsigned long one_value = 1;

#else

#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#include <rpc/types.h>
#endif
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/uio.h>   /* for writev */
#include <netdb.h>

#ifdef USE_FIONBIO
#include <sys/ioctl.h>
static const int zero_value = 0, one_value = 1;
#define SET_BLOCKING(fd)        ioctl((fd), FIONBIO, &zero_value)
#define SET_NONBLOCKING(fd)     ioctl((fd), FIONBIO, &one_value)
#define ERRNO_BLOCK EWOULDBLOCK
#else

#include <fcntl.h>
#ifdef NB_O_NDELAY              /* Nothing needs this? */
#   define NB_FLAG O_NDELAY
#   ifndef ERRNO_BLOCK          /* allow override (e.g. EAGAIN) via Makefile */
#      define ERRNO_BLOCK EWOULDBLOCK
#   endif
#else  /* !NB_O_NDELAY */       /* The True Way - POSIX!:-) */
#   define NB_FLAG O_NONBLOCK
#   define ERRNO_BLOCK EAGAIN
#endif /* !NB_O_NDELAY */
#define SET_BLOCKING(fd)        fcntl((fd), F_SETFL, \
                                      fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#define SET_NONBLOCKING(fd)     fcntl((fd), F_SETFL, \
                                      fcntl((fd), F_GETFL, 0) | NB_FLAG)
#endif

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

#ifndef SD_SEND
#define SD_RECV 0
#define SD_SEND 1
#define SD_BOTH 2
#endif

typedef struct iovec sock_iovec;

extern int close();
extern int gethostname();

#define sock_open(af, type, proto)  socket(af, type, proto)
#define sock_close(s)               close(s)
#define sock_shutdown(s,how)        shutdown(s,how)
#define sock_accept(s, addr, len)   accept(s, addr, len)
#define sock_connect(s, addr, len)  connect(s, addr, len)
#define sock_listen(s, b)           listen(s, b)
#define sock_bind(s, addr, len)     bind(s, addr, len)
#define sock_getopt(s,t,n,v,l)      getsockopt((s),(t),(n),(v),(l))
#define sock_setopt(s,t,n,v,l)      setsockopt((s),(t),(n),(v),(l))
#define sock_name(s, addr, len)     getsockname((s), (addr), (len))
#define sock_peer(s, addr, len)     getpeername((s), (addr), (len))
#define sock_ntohs(x)               ntohs(x)
#define sock_ntohl(x)               ntohl(x)
#define sock_htons(x)               htons(x)
#define sock_htonl(x)               htonl(x)
#define sock_send(s,buf,len,flag)   send((s),(buf),(len),(flag))
#define sock_write(s,buf,len)       write((s),(buf),(len))
#define sock_sendv(s, vec, size, np, flag) \
		(*(np) = writev(s, vec, size))
#define sock_recv(s,buf,len,flag)   recv((s),(buf),(len),(flag))
#define sock_read(s,buf,len)        read((s),(buf),(len))
#define sock_recvfrom(s,buf,blen,flag,addr,alen) \
                recvfrom((s),(buf),(blen),(flag),(addr),(alen))
#define sock_sendto(s,buf,blen,flag,addr,alen) \
                sendto((s),(buf),(blen),(flag),(addr),(alen))
#define sock_hostname(buf, len)    gethostname((buf), (len))

#define sock_errno()                errno
#define sock_create_event(d)        (d)->s   /* return file descriptor */
#define sock_close_event(e)                  /* do nothing */

#define sock_select(d, flags, onoff) do { \
        (d)->event_mask = (onoff) ? \
                 ((d)->event_mask | (flags)) : \
                 ((d)->event_mask & ~(flags)); \
        driver_select((d)->port, (d)->event, (flags), (onoff)); \
   } while(0)

#endif


#endif
