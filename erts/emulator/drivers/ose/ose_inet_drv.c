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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#undef WANT_NONBLOCKING

#undef ERL_SYS_DRV
#include "erl_driver.h"

#include "inet.h"
#include "inet.sig"
#include "nameser.h" 
#include "resolv.h"
#include "netdb.h"
#include "efs.h"

#include "sys.h"

#include "ose_inet_drv.h"
#include "erl_inet.sig"

#define INET_SIG_TIMEOUT 60000

union SIGNAL {
  SIGSELECT sig_no;
};

static int ose_inet_init(void);
static ErlDrvData ose_inet_start(ErlDrvPort, char*);
static void ose_inet_stop(ErlDrvData);
static void ose_inet_command(ErlDrvData, char*, int);
static void ose_inet_input(ErlDrvData, ErlDrvEvent);

struct erl_drv_entry ose_inet_driver_entry = 
{
    ose_inet_init,  
    ose_inet_start,		/* start */
    ose_inet_stop,		/* port closed */
    ose_inet_command,		/* data from erlang */
    ose_inet_input,		/* data in message queue */
    NULL,			/* output, not used */
    "ose_inet",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

static int ose_tcp_inet_init(void);
static void ose_tcp_inet_stop(ErlDrvData);
static void ose_tcp_inet_command(ErlDrvData, char*, int);
static void ose_tcp_inet_commandv(ErlDrvData, ErlIOVec*);
static void ose_tcp_inet_drv_input(ErlDrvData, ErlDrvEvent);
static ErlDrvData ose_tcp_inet_start(ErlDrvPort, char* command);
static int ose_tcp_inet_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void  ose_tcp_inet_timeout(ErlDrvData);

static struct erl_drv_entry ose_tcp_inet_driver_entry = 
{
    ose_tcp_inet_init,
    ose_tcp_inet_start, 
    ose_tcp_inet_stop, 
    ose_tcp_inet_command,
    NULL,			/* tcp input function called from ose_inet_input */
    NULL,			/* ready_output not used (this driver never does
				   driver_select(DO_WRITE) */
    "tcp_inet",
    NULL,
    NULL,
    ose_tcp_inet_ctl,
    ose_tcp_inet_timeout,
    ose_tcp_inet_commandv
};

static int ose_udp_inet_init(void);
static void ose_udp_inet_stop(ErlDrvData);
static void ose_udp_inet_command(ErlDrvData, char*, int);
static void ose_udp_inet_drv_input(ErlDrvData, ErlDrvEvent);
static ErlDrvData ose_udp_inet_start(ErlDrvPort, char*);
static int ose_udp_inet_ctl(ErlDrvData, unsigned int, char*, int, char**, int);
static void ose_udp_inet_timeout(ErlDrvData);

static struct erl_drv_entry ose_udp_inet_driver_entry = 
{
    ose_udp_inet_init,  
    ose_udp_inet_start,
    ose_udp_inet_stop,
    ose_udp_inet_command,
    NULL,			/* udp input function called from ose_inet_input */
    NULL,			/* ready_output not used (this driver never does
				   driver_select(DO_WRITE) */
    "udp_inet",
    NULL,
    NULL,
    ose_udp_inet_ctl,
    ose_udp_inet_timeout,
    NULL
};

/* pointers to the inet_drv entries */
static struct erl_drv_entry *tcp_inet_driver_entry;
static struct erl_drv_entry *udp_inet_driver_entry;

#define SELECTED           0
#define NOT_SELECTED       1

struct sockState {
  int read;			/* indicates if driver has selected socket for input */
  int write;			/* indicates if driver has selected socket for output */
};

typedef struct inet_drv_data {
  struct erl_drv_entry *entry;
  void* erl_drv_data;
} InetDrvData;

extern Uint erts_max_ports;

static PROCESS inet_, erl_sock_select_;
static ErlDrvPort reserved_port;
static InetDrvData **driver_data; /* port -> driver data (array of size erts_max_ports) */
static ErlDrvPort ports[MAX_SOCKS];  /* socket -> port */
static struct sockState* sock_status[MAX_SOCKS];

union eventSig {
  SIGSELECT sig_no;
  struct SockSelect      sock_select;
  struct InetEventRead   inet_ev_read;
  struct InetEventWrite  inet_ev_write;
  struct SockSelectError sock_select_error;
  struct InetEventAck    inet_ev_ack;
};

union iSIGNAL {
  SIGSELECT sig_no;
  struct InetIfUp      up;
  struct InetRouteAdd  route;
};

extern PROCESS start_sock_select();
extern PROCESS ose_inet_;
extern void stop_sock_select(PROCESS);
extern void select_release(void);

/* forward declarations */
static void send_sock_select(int, int, int);
static void send_event_ack(PROCESS, int);

/************************** GEN FUNCS ***************************/
void add_ose_inet_drv_entry() {
  add_driver_entry(&ose_inet_driver_entry);
}

static int ose_inet_init(void) {
  printf("ose inet drv init (max sockets = %d, max ports = %d)\n", MAX_SOCKS, erts_max_ports);
  driver_data = (InetDrvData **)sys_alloc(erts_max_ports*sizeof(InetDrvData *));
  memset((void*)driver_data, 0, erts_max_ports*sizeof(InetDrvData *));
  memset((void*)ports, 0, MAX_SOCKS*sizeof(ErlDrvPort));
  return 0;
}

/* setup default erlang port for inet signals */
static ErlDrvData ose_inet_start(ErlDrvPort res_port, char* command) {
#ifdef DEBUG
  printf("ose_inet starts! port: %d\n", (int)res_port);
#endif
  /* "hunt" for ose_inet (i.e. find pid given name */
  if(hunt("ose_inet", 0, &inet_, NULL)) {
    reserved_port = res_port;
    /* make sure any signals from ose_inet is handled by this driver */
    driver_select(res_port, (ErlDrvEvent)inet_, DO_START, 1);
    driver_select(res_port, (ErlDrvEvent)inet_, DO_READ, 1);
    /* find or start sock_select process */
    if (hunt("erl_sock_select", 0, &erl_sock_select_, NULL)) 
      stop_sock_select(erl_sock_select_);
    erl_sock_select_ = start_sock_select();
    /* register port with rescheduler process and get ready to receive */
    driver_select(res_port, (ErlDrvEvent)erl_sock_select_, DO_START, 1);
    driver_select(res_port, (ErlDrvEvent)erl_sock_select_, DO_READ, 1);
    /* make it possible for the inet driver to send signals to itself */
    driver_select(res_port, (ErlDrvEvent)current_process(), DO_START, 1);
    driver_select(res_port, (ErlDrvEvent)current_process(), DO_READ, 1);
    return (ErlDrvData)NULL;
  }
  else
    return ERL_DRV_ERROR_GENERAL;
}

static void ose_inet_stop(ErlDrvData e) {
  int i;
  InetDrvData *dd;

#ifdef DEBUG
  printf("ose_inet stopped!\n");
#endif
  /* unregister port processes */
  driver_select(reserved_port, (ErlDrvEvent)erl_sock_select_, DO_STOP, 1);
  driver_select(reserved_port, (ErlDrvEvent)inet_, DO_STOP, 1);

  /* clear memory */
  for(i = 0; i < erts_max_ports; i++) {
    if((dd = driver_data[i]) != NULL)
      sys_free(dd);
  }

  kill_proc(erl_sock_select_);
}

/* called from erlang to send data on socket (not used) */
static void ose_inet_command(ErlDrvData e, char *buf, int len) {
  fprintf(stderr, "ose_inet command called!\n");
}

/* driver receives event signal (from inet or erl_sock_select) */
static void ose_inet_input(ErlDrvData empty, ErlDrvEvent sig) {
  union eventSig *inet_sig = (union eventSig *)sig;
  InetDrvData *dd;
  int sock;
  struct sockState *ss;
  
  switch(inet_sig->sig_no) {
    
    /* data available on socket */
  case INET_EVENT_READ:
    sock = inet_sig->inet_ev_read.sock;
    if((ss = sock_status[sock]) == NULL) {
      /* socket has been closed, discard signal */
      fprintf(stderr, "Data on socket %d (unknown socket, port %d)\n", sock, ports[sock]);
      send_sock_select(sock, DO_READ, 0);
    } else    
      /* check sock status if driver is ready to receive, else ignore indication */
      if(ss->read == SELECTED) {
	/* note: sock->port has been registered, since read == SELECTED */
	dd = driver_data[(int)(ports[sock])];
#ifdef DEBUG
	printf("Port %d has data on socket %d (read)\n", ports[sock], sock);
#endif    
	(*((dd->entry)->ready_input))((ErlDrvData)(dd->erl_drv_data), 
				      (ErlDrvEvent)sock);
      }
    send_event_ack(sender((union SIGNAL **)&inet_sig), sock);    
    free_buf((union SIGNAL **)&inet_sig);
    return;

  case INET_EVENT_WRITE:  
    sock = inet_sig->inet_ev_write.sock;
    if((ss = sock_status[sock]) == NULL) {
      /* socket has been closed, discard signal */
      fprintf(stderr, "Output ready on socket %d (unknown socket, port %d)\n", sock, ports[sock]);
      send_sock_select(sock, DO_WRITE, 0);
    } else
      /* check sock status if driver wants to write, else ignore indication */
      if(ss->write == SELECTED) {
	/* note: sock->port has been registered, since write == SELECTED */
	dd = driver_data[(int)(ports[sock])];
#ifdef DEBUG
	printf("Port %d ready to write on socket %d\n", ports[sock], sock);
#endif  
	(*((dd->entry)->ready_output))((ErlDrvData)(dd->erl_drv_data), 
				       (ErlDrvEvent)sock);
      }
    send_event_ack(sender((union SIGNAL **)&inet_sig), sock); 
    free_buf((union SIGNAL **)&inet_sig);
    return;  

  case SOCK_SELECT_ERROR:
    sock = inet_sig->sock_select_error.sock;
    fprintf(stderr, "Select error %d! Bad socket: %d (port %d)\n", sock, 
	    inet_sig->sock_select_error.error, ports[sock]);
    /* what to do here? exit!? */
    send_event_ack(sender((union SIGNAL **)&inet_sig), sock); 
    free_buf((union SIGNAL **)&inet_sig);
    return;

  default:
    sock = inet_sig->sock_select_error.sock;
    fprintf(stderr, "Unknown signal %li from process: %li\n", 
	    inet_sig->sig_no, sender((union SIGNAL **)&inet_sig));
    send_event_ack(sender((union SIGNAL **)&inet_sig), sock); 
    free_buf((union SIGNAL **)&inet_sig);
  }
}

int ose_inet_select(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{
  /* extern void send_error(int sock, int error); */
  extern void send_write_event(int s);
  extern void send_read_event(int s);
  int sock = (int)e;

  ports[sock] = ix;

  if (mode & DO_READ) {
    if(on) sock_status[sock]->read = SELECTED; else sock_status[sock]->read = NOT_SELECTED;
  } 
  else if (mode & DO_WRITE) {
    if(on) sock_status[sock]->write = SELECTED; else sock_status[sock]->write = NOT_SELECTED;
  }
  send_sock_select(sock, mode, on);
  return 0;
}

int ose_inet_socket(int domain, int type, int protocol) {
  int sock;
  struct sockState *ss;

  if((sock = socket(domain, type, protocol)) >= 0) {
    ss = sys_alloc(sizeof(struct sockState));
    ss->read = ss->write = NOT_SELECTED;
    sock_status[sock] = ss;
  }
  return sock;
}

int ose_inet_close(int sock) {
  int res;
#ifdef DEBUG
  printf("\nClosing socket %d (port %d)\n", sock, ports[sock]);
#endif
  send_sock_select(sock, (DO_READ | DO_WRITE), 0);
  sys_free(sock_status[sock]);
  sock_status[sock] = NULL;
  ports[sock] = 0;
  if((res = close(sock)) == -1)
    fprintf(stderr, "Error: close fails: %d\n", errno);
  return res;
}

int ose_inet_accept(int s, struct sockaddr *addr, int *lenp) {
  struct sockState *ss;
  int sock = accept(s, addr, lenp);
  if(sock != -1) {
    ss = sys_alloc(sizeof(struct sockState));
    ss->read = ss->write = NOT_SELECTED;
    sock_status[sock] = ss;
  }
  return sock;
}

int ose_inet_send(int s, const void *msg, int len, int flags)
{
  int result;

  if((result = inet_send(s, msg, len, flags)) < 0)
    if(errno == ENOBUFS)
      result = 0;		/* will cause retry */

  return result;
}

/* call send for each vector separately */
int ose_inet_sendv(int s, SysIOVec *iov, int size)
{
  int cnt = 0;
  int sofar = 0;
  int w = 0;

  while (cnt < size) {    
    if (iov[cnt].iov_base && (iov[cnt].iov_len > 0)) {
      /* Non-empty vector */
      if((w = inet_send(s, iov[cnt].iov_base, iov[cnt].iov_len, 0)) < 0) {
	if((errno == ENOBUFS) || (errno == EINTR) || (errno == EWOULDBLOCK)) {
	  return sofar;
	}
	return w;
      }
      if(w < iov[cnt].iov_len) {
	return sofar + w;
      }
    }
    sofar += w;
    cnt++;
  }
  return sofar;
}
      
int ose_inet_sendto(int s, const void *msg, int len, int flags, 
		    const struct sockaddr *to, int tolen) {
  int result;

  if((result = sendto(s, msg, len, flags, to, tolen)) < 0)
    if(errno == ENOBUFS)
      result = 0;		/* will cause retry */

  return result;
}

static int wait_for_if(const char *ifname)
{
  union iSIGNAL   *sig;
  static const    SIGSELECT sel[2] = {1, INET_IF_UP_REPLY};

  sig = (union iSIGNAL *)alloc(sizeof(struct InetIfUp), INET_IF_UP_REQUEST);

  /* No interface specified, pick the first one. */
  if(ifname == 0 || *ifname == '\0')
    strcpy(sig->up.ifName, "*");
  else
    strcpy(sig->up.ifName, ifname);
      
  /* Request a notification signal when the interface is up. */
  send((union SIGNAL **)&sig, ose_inet_);
  sig = (union iSIGNAL *)receive((SIGSELECT *)sel);
      
  /* Verify the INET reply status value. */
  if(sig->up.status != 0)
    fprintf(stderr, "Bad status code for INET_IF_UP_REQUEST\n");

  /* Don't forget to release the buffer. */
  free_buf((union SIGNAL **)&sig);

  return 0;
}

#define MAX_IF_NUM 4
#define IP_LEN(s) (strlen((s)) > 16 ? 16 : 4)
/* macro for IPv4 only */
#define IPSHIFT(cp, l)  ((l) = (unsigned long)(((unsigned long)((cp)[3]) << 24) | \
                                               ((unsigned long)((cp)[2]) << 16) | \
                                               ((unsigned long)((cp)[1]) << 8)  | \
                                                (unsigned long)((cp)[0])))
  

int ose_gethostname(char *name, int namelen) {
  char *hostname;
  /* check first if the hostname is specified by the HOSTNAME
     environment variable, if not, try DNS lookup */
  if((hostname = get_env(get_bid(current_process()), "HOSTNAME")) != NULL) {
    if(namelen >= strlen(hostname)) {
      strcpy(name, hostname);
      return 0;
    } 
    return -1;
  }
  else {    
    int sock;
    int ifnum = 0;
    char *ifreq_buf;   
    int buflen = 0;
    struct ifconf ifconf;
    struct ifreq *ifr;
    struct sockaddr_in *sockaddr;
    struct hostent *host;
    struct in_addr inaddr;
    int result, len;
    unsigned char *cp;

    wait_for_if(0);
    
    if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
      error(errno);
      return -1;
    }

    buflen = MAX_IF_NUM * SDL_IFREQSIZE;
    ifreq_buf = (char *)sys_alloc(buflen * sizeof(char));
    ifconf.ifc_len = buflen;
    ifconf.ifc_buf = ifreq_buf;
    if(ioctl(sock, SIOCGIFCONF, (char *)&ifconf) == -1)
      error(errno);
    if(close(sock) == -1)
      fprintf(stderr, "Error: close fails: %d\n", errno);
    ifnum = ifconf.ifc_len / SDL_IFREQSIZE;
    ifr = (struct ifreq *)(ifconf.ifc_req);

#ifdef DEBUG
    printf("Found %d interfaces\n", ifnum);
#endif

    if(ifnum < 2) {		/* only loopback configured */
      if(namelen >= 7) {
	strcpy(name, "nohost");
	return 0;
      }
      return -1;
    }

    ifr = (struct ifreq *)((char *)ifr + SDL_IFREQSIZE); /* point to if1 */
    if((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1) { 
      sys_free(ifreq_buf);
      error(errno);
      return -1;
    }
    if(ioctl(sock, SIOCGIFADDR, (char *)ifr) == -1) { 
      sys_free(ifreq_buf);
      error(errno);
      return -1;
    }
    sockaddr = (struct sockaddr_in *)&(ifr->ifr_addr);
    inaddr = (sockaddr->sin_addr);
    len = IP_LEN(inet_ntoa(inaddr));
    sys_free(ifreq_buf);

#ifdef DEBUG
    printf("Attempting to resolv address: %s\n", inet_ntoa(inaddr));
#endif
    /* resolve IP adress for if1 */
    if((host = gethostbyaddr_r((const char *)&inaddr, len, AF_INET)) == NULL) {
    /* Could be that the OSE resolver doesn't reverse the IP string (compiled 
       for LITTLE ENDIAN!?), change the byte order and try again */
      cp = (unsigned char*)&(inaddr.s_addr);
      IPSHIFT(cp, inaddr.s_addr);
      if((host = gethostbyaddr_r((const char *)&inaddr, len, AF_INET)) == NULL) {
	if(namelen >= 7) {
	  strcpy(name, "nohost");
	  return 0;
	}
	return -1;
      }
    }
    if(namelen >= strlen(host->h_name)) {
      strcpy(name, host->h_name);
#ifdef DEBUG
      printf("HOSTNAME: %s\n", name);
#endif
      result = 0;
    }
    else result = -1;
    free_buf((union SIGNAL **)&host);
    return result;
  }
}

/* getservbyname & getservbyport, dummies for now */
struct servent *ose_getservbyname(const char *name,  const  char *proto) {
  return NULL;
}

struct servent *ose_getservbyport(int port, const char *proto) {
  return NULL;
}


/****************************** TCP ******************************/

void add_ose_tcp_drv_entry(struct erl_drv_entry *entry) {
  tcp_inet_driver_entry = entry;
  add_driver_entry(&ose_tcp_inet_driver_entry);
}

static int ose_tcp_inet_init(void) {
  return (*tcp_inet_driver_entry->init)();
}

/* new inet port started */
static ErlDrvData ose_tcp_inet_start(ErlDrvPort port, char* command) {
  void *result;
  InetDrvData *dd;

  /* save port->driver_data mapping here, will also be mapped to socket later */
  if((result = (void *)((*tcp_inet_driver_entry->start)(port, command))) > 0) {
    /* memory for a driver_data entry cannot be deallocated when driver 
       is stopped, therefore check here if position has been previously 
       used and if so, clear it */ 
    if((dd = driver_data[(int)port]) != NULL) {
#ifdef DEBUG
      printf("\nDriver data in slot %d, clearing!\n", (int)port);
#endif
      sys_free(dd);
    }
    dd = (InetDrvData *)sys_alloc(sizeof(InetDrvData));
    dd->erl_drv_data = result;
    dd->entry = tcp_inet_driver_entry;
    driver_data[(int)port] = dd;
  }
  return (ErlDrvData)result;
}

/* called from erlang to send data on socket */
static void ose_tcp_inet_command(ErlDrvData e, char *buf, int len) {
  (*tcp_inet_driver_entry->output)(e, buf, len);
}

/* called from erlang to send data on socket */
static void ose_tcp_inet_commandv(ErlDrvData e, ErlIOVec* iovec) {
  (*tcp_inet_driver_entry->outputv)(e, iovec);
}

static int ose_tcp_inet_ctl(ErlDrvData e, unsigned int cmd, char *buf, 
			    int len, char **rbuf, int rsize) {
  return (*tcp_inet_driver_entry->control)(e, cmd, buf, len, rbuf, rsize);
}

static void ose_tcp_inet_timeout(ErlDrvData e) {
  (*tcp_inet_driver_entry->timeout)(e);
}

static void ose_tcp_inet_stop(ErlDrvData e) {
  (*tcp_inet_driver_entry->stop)(e);
}

/****************************** UDP ******************************/


void add_ose_udp_drv_entry(struct erl_drv_entry *entry) {
  udp_inet_driver_entry = entry;
  add_driver_entry(&ose_udp_inet_driver_entry);
}

static int ose_udp_inet_init(void) {
  return (*udp_inet_driver_entry->init)();
}

/* new inet port started */
static ErlDrvData ose_udp_inet_start(ErlDrvPort port, char* command) {
  void *result;
  InetDrvData *dd;

  /* save port->driver_data mapping here, will also be mapped to socket later */
  if((result = (void *)((*udp_inet_driver_entry->start)(port, command))) > 0) {
    /* memory for a driver_data entry cannot be deallocated when driver 
       is stopped, therefore check here if position has been previously 
       used and if so, clear it */ 
    if((dd = driver_data[(int)port]) != NULL)
      sys_free(dd);
    dd = (InetDrvData *)sys_alloc(sizeof(InetDrvData));
    dd->erl_drv_data = result;
    dd->entry = udp_inet_driver_entry;
    driver_data[(int)port] = dd;
  }
  return (ErlDrvData)result;
}

static void ose_udp_inet_command(ErlDrvData e, char *buf, int len) {
  (*udp_inet_driver_entry->output)(e, buf, len);
}

static int ose_udp_inet_ctl(ErlDrvData e, unsigned int cmd, char *buf, 
			    int len, char **rbuf, int rsize) {
  return (*udp_inet_driver_entry->control)(e, cmd, buf, len, rbuf, rsize);
}

static void ose_udp_inet_timeout(ErlDrvData e) {
  (*udp_inet_driver_entry->timeout)(e);
}

static void ose_udp_inet_stop(ErlDrvData e) {
  (*udp_inet_driver_entry->stop)(e);
}


/************************** HELP FUNCS ***************************/

static void send_event_ack(PROCESS to_, int sock) {
  union eventSig *sig;
  if(to_ != current_process()) { /* don't ack yourself */
    sig = (union eventSig *)alloc(sizeof(struct InetEventAck), INET_EVENT_ACK);
    sig->inet_ev_ack.sock = sock;
    send((union SIGNAL **)&sig, to_);
  }
}

static void send_sock_select(int sock, int mode, int on) {
  union eventSig *sig;

  sig = (union eventSig *)alloc(sizeof(struct SockSelect), SOCK_SELECT);
  sig->sock_select.sock = sock;
  sig->sock_select.mode = mode;
  sig->sock_select.on = on;
  send((union SIGNAL **)&sig, erl_sock_select_);
  /* wake select process */
  select_release();
}



