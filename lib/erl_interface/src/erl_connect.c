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
 * Purpose: Connect to any node at any host.
 */

#ifdef HAVE_CONFIG_H
# include "config.h"	      /* FIXME: Autoconf Info prefers <config.h> */
#endif

# define HAVE_STRDUP 1	      /* we, uh, know that everyone's got strdup() */
#ifdef VXWORKS
# undef HAVE_STRDUP	      /* ...'cept for VxWorks */
#endif

#include <stdlib.h>
#include <sys/types.h>
#include <stdarg.h>
#include <fcntl.h>

#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <timers.h> 

/* #include "netdb.h" */ /* local file */
#include "erl_error.h"

#define getpid() taskIdSelf()
extern int h_errno;

#else /* some other unix */
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <arpa/inet.h>
#include <netdb.h>
#include <sys/utsname.h>  /* for gen_challenge (NEED FIX?) */
#endif

/* common includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "erl_interface.h"
#include "erl_config.h"
#include "putget.h"
#include "ei.h"

#if !defined(__WIN32__) && !defined(VXWORKS) 
extern int gethostname();
#endif

extern int erl_read_fill(int,char*,int);
extern int erl_write_fill(int,char*,int);

extern int erl_encode3(ETERM*,unsigned char*, int);
extern int erl_term_len2(ETERM*, int);

#define SELF(fd) erl_mk_pid(ec.thisnodename, fd, 0, ec.creation)

#define COOKIE_FILE "/.erlang.cookie"

#ifdef __WIN32__
static void initWinSock(void);
#endif /* __WIN32__ */

typedef unsigned short  uint16;

#ifndef MAX_COOKIE_SIZE
#define MAX_COOKIE_SIZE 512
#endif

/* rpc_from() uses a buffer this size */
#ifndef MAX_RECEIVE_BUF
#define MAX_RECEIVE_BUF 32*1024
#endif

/* Distribution capability flags */
#define DFLAG_PUBLISHED           1
#define DFLAG_ATOM_CACHE          2
#define DFLAG_EXTENDED_REFERENCES 4
#define DFLAG_DIST_MONITOR        8
#define DFLAG_FUN_TAGS            16

/* some send() functions use buffer on heap for "small" messages */
/* messages larger than this require call to malloc() */
#define SMALLBUF 2048

#define DEFBUF_SIZ 100

typedef struct {
  char thishostname[MAXHOSTLEN+1];
  char thisnodename[MAXHOSTLEN+1+MAXALIVELEN+1];
  char thisalivename[MAXALIVELEN+1];
  struct in_addr this_ipaddr;             /* stored in network byte order */
  char erl_connect_cookie[MAX_COOKIE_SIZE+1];
  short creation;
  int conns[FD_SETSIZE];
  char *cookies[FD_SETSIZE];
  erlang_pid self;
} internal_stuff;

int ei_trace_distribution = 0;

internal_stuff ec;
static int init_done = 0; /* write-once flag for above global data */

static char *null_cookie = "";

/* forwards */
static unsigned gen_challenge(void);
static void gen_digest(unsigned challenge, char cookie[], 
		       unsigned char digest[16]);
static int send_status(int fd, char *status);
static int recv_status(int fd);
static int send_challenge(int fd, char *nodename, 
			  unsigned challenge, unsigned version);
static int recv_challenge(int fd, unsigned *challenge, 
			  unsigned *version,
			  unsigned *flags, ErlConnect *namebuf);
static int send_challenge_reply(int fd, char digest[16], 
				unsigned challenge);
static int recv_challenge_reply(int fd, 
				unsigned our_challenge,
				char cookie[], 
				unsigned *her_challenge);
static int send_challenge_ack(int fd, char digest[16]);
static int recv_challenge_ack(int fd, 
			      unsigned our_challenge,
			      char cookie[]);
static int send_name(int fd, char *nodename, 
		     unsigned version); 
/* Common for both handshake types */
static int recv_name(int fd, 
		     unsigned *version,
		     unsigned *flags, ErlConnect *namebuf);

int erl_distversion(int fd)
{
  return ec.conns[fd];
}

const char *erl_thisnodename(void)
{
  return (const char *) ec.thisnodename;
}

const char *erl_thishostname(void)
{
  return (const char *) ec.thishostname;
}

const char *erl_thisalivename(void)
{
  return (const char *) ec.thisalivename;
}

const char *erl_thiscookie(void)
{
  return (const char *) ec.erl_connect_cookie;
}

short erl_thiscreation(void)
{
  return ec.creation;
}

void erl_set_thiscreation(short creation)
{
  ec.creation = creation;
  return;
}

erlang_pid *erl_self(void)
{
  return &ec.self;
}

/* two internal functions that will let us support different cookies
 * (to be able to connect to other nodes that don't have the same
 * cookie as each other or us)
 */
const char *erl_getfdcookie(int fd)
{
  return (const char *) 
    (ec.cookies[fd] ? ec.cookies[fd] : ec.erl_connect_cookie);
}

/* call with cookie to set value to use on descriptor fd,
 * or specify NULL to use default
 */
int 
erl_setfdcookie (int fd, char *cookie)
{
  if (cookie) {
    char *tmp = malloc(strlen(cookie)+1);

    if (tmp == NULL) {
	erl_errno = ENOMEM;
	return -1;
    }
    strcpy(tmp, cookie);
    if (ec.cookies[fd] != ec.erl_connect_cookie) free(ec.cookies[fd]);
    ec.cookies[fd] = tmp;
  }
  else {
    if (ec.cookies[fd] != ec.erl_connect_cookie) free(ec.cookies[fd]);
    ec.cookies[fd] = ec.erl_connect_cookie;
  }
  return 0;
}

static int 
get_int32 (unsigned char *s)
{
  return ((s[0] << 24) | (s[1] << 16) | (s[2] << 8) | (s[3] ));
}

static int 
get_home (char *buf, int size)
{
  char* homedrive;
  char* homepath;

#ifdef __WIN32__
  homedrive = getenv("HOMEDRIVE");
  homepath = getenv("HOMEPATH");
#else
  homedrive = "";
  homepath = getenv("HOME");
#endif

  if (!homedrive || !homepath) {
    buf[0] = '.';
    buf[1] = '\0';
    return 1;
  } else if (strlen(homedrive)+strlen(homepath) < size-1) {
    strcpy(buf, homedrive);
    strcat(buf, homepath);
    return 1;
  }

  return 0;
}

static void 
get_cookie (void)
{
  char fname[1024+sizeof(COOKIE_FILE)+1];
  int fd;
  int len;
    
  if (!get_home(fname, sizeof(fname))) {
    erl_err_sys("<ERROR> too long home");
  }
  strcat(fname, COOKIE_FILE);
  if ((fd = open(fname, O_RDONLY,0777)) < 0)
    erl_err_sys("<ERROR> open cookie file");
    

  if ((len = read(fd, ec.erl_connect_cookie,
		  sizeof(ec.erl_connect_cookie)-1)) < 0)
    erl_err_sys("<ERROR> reading cookie file (1)");
  else if (len == BUFSIZ)
    erl_err_sys("<ERROR> reading cookie file (2)");
  ec.erl_connect_cookie[len] = '\0';
  if (ec.erl_connect_cookie[len-1] == '\n')
    ec.erl_connect_cookie[len-1] = '\0';
}

#ifdef __WIN32__
static void 
win32_error (char *buf, int buflen)
{
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		0,	/* n/a */
		WSAGetLastError(), /* error code */
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* language */
		buf,
		buflen,
		NULL);
  return;
}

static void 
initWinSock (void)
{
  WORD wVersionRequested;  
  WSADATA wsaData; 
  int i; 
  static int initialized;

  wVersionRequested = MAKEWORD(1, 1); 
  if (!initialized) {
    initialized = 1;
    if ((i = WSAStartup(wVersionRequested, &wsaData))) {
      erl_err_msg("<ERROR> erl_connect_init: Can't initialize windows sockets: %d",i);
    }
  
    if ( LOBYTE( wsaData.wVersion ) != 1 || HIBYTE( wsaData.wVersion ) != 1 ) { 
      erl_err_msg("<ERROR> erl_connect_init: This version of windows sockets "
		  "not supported");
      WSACleanup(); 
    }
  }
}
#endif

/*
 * Perhaps run this routine instead of erl_connect_init/2 ?
 * Initailize by setting:
 * thishostname, thisalivename, thisnodename and thisipaddr
 */
int 
erl_connect_xinit (char *thishostname, char *thisalivename, char *thisnodename,
		   Erl_IpAddr thisipaddr, char *cookie, short creation)
{
  int i=0;
  char *dbglevel;
  
  if (init_done) return 1;
  init_done = 1;

  ec.creation = creation;

#ifdef __WIN32__
  initWinSock();
#endif

  if (!cookie) get_cookie();
  else if (strlen(cookie) >= sizeof(ec.erl_connect_cookie)) 
    erl_err_quit("<ERROR> erl_connect_xinit: Cookie size too large");
  else strcpy(ec.erl_connect_cookie, cookie);

  if (strlen(thishostname) >= sizeof(ec.thishostname)) 
    erl_err_quit("<ERROR> erl_connect_init: Thishostname too long");
  strcpy(ec.thishostname, thishostname);

  if (strlen(thisalivename) >= sizeof(ec.thisalivename)) 
    erl_err_quit("<ERROR> erl_connect_init: Thisalivename too long");
  strcpy(ec.thisalivename, thisalivename);

  if (strlen(thisnodename) >= sizeof(ec.thisnodename)) 
    erl_err_quit("<ERROR> erl_connect_init: Thisnodename too long");
  strcpy(ec.thisnodename, thisnodename);

  memmove(&ec.this_ipaddr, thisipaddr, sizeof(ec.this_ipaddr)); 

  strcpy(ec.self.node,thisnodename);
  ec.self.num = 0;
  ec.self.serial = 0;
  ec.self.creation = creation;

  memset(&ec.conns,0,sizeof(ec.conns));
  for (i=0; i<FD_SETSIZE; i++) ec.cookies[i] = ec.erl_connect_cookie;

  if ((dbglevel=getenv("ERL_DEBUG_DIST"))) 
      ei_trace_distribution = atoi(dbglevel);

  return 1;
}

int 
erl_connect_init_ex (int this_node_number, char *cookie,
		     short creation, int use_long_name)
{
  struct hostent *hp;
  char* ct;
  char thishostname[MAXHOSTLEN+1];
  char thisnodename[MAXHOSTLEN+1+MAXALIVELEN+1];
  char thisalivename[MAXALIVELEN+1];

#ifdef __WIN32__
  initWinSock();
#endif /* win32 */

  if (gethostname(thishostname, MAXHOSTLEN) ==  -1) {
#ifdef __WIN32__
    erl_err_quit("erl_connect_init: Failed to get host name: %d",
		 WSAGetLastError());
#else
    erl_err_quit("erl_connect_init: Failed to get host name: %d",
		 errno);
#endif /* win32 */
  }

  sprintf(thisalivename, "c%d",
	  this_node_number < 0 ?  (int) getpid() : this_node_number);
  
  if ((hp = erl_gethostbyname(thishostname)) == 0) {
#ifdef __WIN32__
    char reason[1024];

    win32_error(reason,sizeof(reason));
    erl_err_msg("erl_connect_init: Can't get ip address for host %s: %s\n",
		thishostname, reason);
#else
    erl_err_msg("erl_connect_init: Can't get ip address for host %s: %d\n",
		thishostname,h_errno);
#endif /* win32 */
    return 0;
  }
  
  if ((use_long_name == 0) &&  /* shortnames */
      ((ct = strchr(hp->h_name, '.')) != NULL)) *ct = '\0';

  strcpy(thishostname, hp->h_name);
  sprintf(thisnodename, "%s@%s", thisalivename, hp->h_name);

  return erl_connect_xinit(thishostname, thisalivename, thisnodename,
			   (struct in_addr *)*hp->h_addr_list, cookie, creation);
}

/*
 * Initialize by set: thishostname, thisalivename, 
 * thisnodename and thisipaddr. At success return 1,
 * otherwise return 0.
 */
int 
erl_connect_init (int this_node_number, char *cookie, short creation)
{
    return erl_connect_init_ex(this_node_number, cookie, creation, 0);
}


/* connects to port at ip-address ip_addr 
 * and returns fd to socket 
 * port has to be in host byte order 
 */
static int 
cnct (uint16 port, struct in_addr *ip_addr, int addr_len)
{
  int s;
  struct sockaddr_in iserv_addr;

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0)
  {
      erl_errno = errno;
      return ERL_ERROR;
  }
  
  memset((char*)&iserv_addr, 0, sizeof(struct sockaddr_in));
  memcpy((char*)&iserv_addr.sin_addr, (char*)ip_addr, addr_len);
  iserv_addr.sin_family = AF_INET;
  iserv_addr.sin_port = htons(port);

  if (connect(s, (struct sockaddr*)&iserv_addr, sizeof(iserv_addr)) < 0) {
      erl_errno = errno;
    closesocket(s);
    return ERL_ERROR;
  }

  return s;
} /* cnct */


/* 
 * Set up a connection to a given Node, and 
 * interchange hand shake messages with it.
 * Returns a valid file descriptor at success,
 * otherwise a negative error code.
 */
int 
erl_connect (char *nodename)
{
  char *hostname, alivename[BUFSIZ];
  struct hostent *hp;
#if !defined (__WIN32__) 
  /* these are needed for the call to gethostbyname_r */
  struct hostent host;
  char buffer[1024];
  int h_errno;
#endif /* !win32 */

  /* extract the host and alive parts from nodename */
  if (!(hostname = strchr(nodename,'@'))) return ERL_ERROR;
  else {
    strncpy(alivename, nodename, hostname - nodename);
    alivename[hostname - nodename] = 0x0;
    hostname++;
  }

#ifndef __WIN32__
  if ((hp = erl_gethostbyname_r(hostname,&host,buffer,1024,&h_errno)) == NULL) {
    erl_err_msg("<ERROR> erl_connect: Can't find host for %s: %d\n", nodename, h_errno);
    erl_errno = EHOSTUNREACH;
    return ERL_ERROR;
  }

#else /* __WIN32__ */
  if ((hp = erl_gethostbyname(hostname)) == NULL) {
    char reason[1024];
    win32_error(reason,sizeof(reason));
    erl_err_msg("<ERROR> erl_connect: Can't find host for %s: %s\n", nodename, reason);
    erl_errno = EHOSTUNREACH;
    return ERL_ERROR;
  }
#endif /* win32 */

  return erl_xconnect((Erl_IpAddr) *hp->h_addr_list, alivename);
} /* erl_connect */


/* ip_addr is now in network byte order 
 *
 * first we have to get hold of the portnumber to
 *  the node through epmd at that host 
 *
 */
int 
erl_xconnect (Erl_IpAddr adr, char *alivename)
{
  struct in_addr *ip_addr=(struct in_addr *) adr;
  int rport = 0;
  int sockd;
  int one = 1;
  int dist = 0;
  ErlConnect her_name;
  unsigned her_flags, her_version;

#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> CONNECT attempt to connect to %s\n",alivename);
    }
#endif

    if ((rport = erl_epmd_port(ip_addr,alivename,&dist)) < 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> CONNECT can't get remote port\n");
    }
#endif
    return ERL_NO_PORT;
  }


  /* we now have port number to enode, try to connect */
  if((sockd = cnct(rport, ip_addr, sizeof(struct in_addr))) < 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> CONNECT socket connect failed\n");
    }
#endif
    return ERL_CONNECT_FAIL;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> CONNECT connected to remote\n");
  }
#endif

    if (dist <= 4) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> CONNECT remote version not compatible\n");
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  else {
    unsigned our_challenge, her_challenge;
    unsigned char our_digest[16];

    if (send_name(sockd, ec.thisnodename, (unsigned) dist))
      goto error;
    if (recv_status(sockd))
      goto error;
    if (recv_challenge(sockd, &her_challenge, &her_version,
		       &her_flags, &her_name))
      goto error;
    our_challenge = gen_challenge();
    gen_digest(her_challenge, ec.erl_connect_cookie, our_digest);
    if (send_challenge_reply(sockd, our_digest, our_challenge))
      goto error;
    if (recv_challenge_ack(sockd, our_challenge, 
			   ec.erl_connect_cookie))
      goto error;
    ec.cookies[sockd] = null_cookie;
    ec.conns[sockd] = dist;
      erl_errno = EIO;
  }

  setsockopt(sockd, IPPROTO_TCP, TCP_NODELAY, (char *)&one, sizeof(one));
  setsockopt(sockd, SOL_SOCKET, SO_KEEPALIVE, (char *)&one, sizeof(one));

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> CONNECT (ok) remote = %s\n",alivename);
  }
#endif

  return sockd;

 error:
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> CONNECT failed\n");
  }
#endif
  closesocket(sockd);
  return ERL_ERROR;
} /* erl_xconnect */

/* 
 * For symmetry reasons
 */
int 
erl_close_connection (int fd)
{
  return closesocket(fd);
} /* erl_close_connection */


/*
 * Accept and initiate a connection from an other
 * Erlang node. Return a file descriptor at success,
 * otherwise -1;
 */
int 
erl_accept (int lfd, ErlConnect *conp)
{
  int fd;
  struct sockaddr_in cli_addr;
  int cli_addr_len=sizeof(struct sockaddr_in);
  unsigned her_version, her_flags;
  ErlConnect her_name;

#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- ACCEPT waiting for connection\n");
    }
#endif

  if ((fd = accept(lfd, (struct sockaddr*) &cli_addr, 
		   &cli_addr_len )) < 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- ACCEPT socket accept failed\n");
    }
#endif
    goto error;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- ACCEPT connected to remote\n");
  }
#endif

  if (recv_name(fd, &her_version, &her_flags, &her_name)) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- ACCEPT initial ident failed\n");
    }
#endif
    goto error;
  }

  if (her_version <= 4) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- ACCEPT remote version not compatible\n");
    }
#endif
    goto error;
  }
  else {
    unsigned our_challenge;
    unsigned her_challenge;
    unsigned char our_digest[16];

    if (send_status(fd,"ok"))
      goto error;
    our_challenge = gen_challenge();
    if (send_challenge(fd, ec.thisnodename, 
		       our_challenge, her_version))
      goto error;
    if (recv_challenge_reply(fd, our_challenge, 
			     ec.erl_connect_cookie, 
			     &her_challenge))
      goto error;
    gen_digest(her_challenge, ec.erl_connect_cookie, our_digest);
    if (send_challenge_ack(fd, our_digest))
      goto error;
    ec.cookies[fd] = null_cookie;
  }
  if (conp) 
    *conp = her_name;
  ec.conns[fd] = her_version;

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- ACCEPT (ok) remote = %s\n",her_name.nodename);
  }
#endif
  return fd;

 error:

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- ACCEPT failed\n");
  }
#endif
  closesocket(fd);
  erl_errno = EIO;
  return ERL_ERROR;
} /* erl_accept */


/* Receives a message from an Erlang socket.
 * If the message was a TICK it is immediately
 * answered. Returns: ERL_ERROR, ERL_TICK or
 * the number of bytes read.
 */
int 
erl_receive (int s, unsigned char *bufp, int bufsize) 
{
  int len;
  unsigned char fourbyte[4]={0,0,0,0};
  
  if (erl_read_fill(s, (char *) bufp, 4)  != 4) {
      erl_errno = EIO;
      return ERL_ERROR;
  }

  /* Tick handling */
  if ((len = get_int32(bufp)) == ERL_TICK) 
  {
    erl_write_fill(s, (char *) fourbyte, 4);
    erl_errno = EAGAIN;
    return ERL_TICK;
  }
  else if (len > bufsize) 
  {
      /* FIXME: We should drain the message. */
      erl_errno = EMSGSIZE;
      return ERL_ERROR;
  }
  else if (erl_read_fill(s, (char *) bufp, len) != len)
  {
      erl_errno = EIO;
      return ERL_ERROR;
  }

  return len;

} /* erl_receive */

/* 
 * Send an Erlang message to a registered process
 * at the Erlang node, connected with a socket.
 */
int 
erl_reg_send (int fd, char *server_name, ETERM *msg)
{
  char sbuf[SMALLBUF]; /* use this for short messages */
  char *dbuf = NULL;   /* use this for longer ones */
  char *msgbuf;
  int msglen;
  erlang_pid *self = erl_self();

  /* get large enough buffer */
  if ((msglen = erl_term_len2(msg,erl_distversion(fd))) > SMALLBUF)
    if (!(dbuf = malloc(msglen)))
    {
	erl_errno = ENOMEM;
	return -1;
    }
  msgbuf = (dbuf ? dbuf : sbuf);

  if (!erl_encode3(msg,msgbuf,erl_distversion(fd)))
  {
      erl_errno = EIO;
      return -1;
  }

  self->num = fd;
  if (ei_send_reg_encoded(fd,self,server_name,msgbuf,msglen)) {
    if (dbuf) free(dbuf);
    erl_errno = EIO;
    return -1;
  }

  if (dbuf) free(dbuf);
  return 1;
}

/* 
 * Sends an Erlang message to a process at an Erlang node
 */
int 
erl_send (int fd, ETERM *to ,ETERM *msg)
{
  char sbuf[SMALLBUF]; /* use this for short messages */
  char *dbuf = NULL;   /* use this for longer ones */
  char *msgbuf = NULL;
  int msglen;
  erlang_pid topid;

  /* make the to-pid */
  if (!ERL_IS_PID(to))
  {
      erl_errno = EINVAL;
      return -1;
  }
  
  strcpy(topid.node, ERL_PID_NODE(to));
  topid.num = ERL_PID_NUMBER(to);
  topid.serial = ERL_PID_SERIAL(to);
  topid.creation = ERL_PID_CREATION(to);
  
  if ((msglen = erl_term_len2(msg,erl_distversion(fd)))  >  SMALLBUF) 
    if ((dbuf = malloc(msglen)) == NULL) 
    {
	erl_errno = ENOMEM;
	return -1;
    }
  msgbuf = (dbuf != NULL) ? dbuf : sbuf;
  
  if (!erl_encode3(msg, msgbuf, erl_distversion(fd)))
  {
      erl_errno = EIO;
      return -1;
  }

  if (ei_send_encoded(fd,&topid,msgbuf,msglen)) 
  {
      if (dbuf) free(dbuf);
      erl_errno = EIO;
      return -1;
  }

  if (dbuf) free(dbuf);
  return 1;
}

extern int ei_recv_internal(int fd, char **mbufp, int *bufsz, erlang_msg *msg, int *msglen, int staticbufp);

/* 
 * Try to receive an Erlang message on a given socket. Returns
 * ERL_TICK, ERL_MSG, or ERL_ERROR. Sets `erl_errno' on ERL_ERROR and
 * ERL_TICK (to EAGAIN in the latter case).
 */
static int 
do_receive_msg (int fd, 
		int staticbuffer_p, unsigned char **buf, int *bufsize,
		ErlMessage *emsg)
{
  int msglen;
  erlang_msg msg;
  int i;
  
  if (!(i=ei_recv_internal(fd, (char**)buf, bufsize, &msg, &msglen, 
			   staticbuffer_p))) 
  {
      erl_errno = EAGAIN;
      return ERL_TICK;
  }
  if (i<0) 
  {
      /* erl_errno set by ei_recv_internal() */
      return ERL_ERROR;
  }
  if (staticbuffer_p && msglen > *bufsize)
  {
      erl_errno = EMSGSIZE;
      return ERL_ERROR;
  }
  
  emsg->type = msg.msgtype;
  emsg->to_name[0] = 0x0;
  
  switch (msg.msgtype) {
  case ERL_SEND:
    emsg->to = erl_mk_pid(msg.to.node,msg.to.num,msg.to.serial,msg.to.creation);
    emsg->from = NULL;
    emsg->msg = erl_decode(*buf);
    return ERL_MSG;
    break;
    
  case ERL_REG_SEND:
    emsg->from = erl_mk_pid(msg.from.node,msg.from.num,msg.from.serial,msg.from.creation);
    emsg->to = NULL;
    emsg->msg = erl_decode(*buf);
    strcpy(emsg->to_name,msg.toname);
    return ERL_MSG;
    break;

  case ERL_LINK:
  case ERL_UNLINK:
  case ERL_GROUP_LEADER:
    emsg->from = erl_mk_pid(msg.from.node,msg.from.num,msg.from.serial,msg.from.creation);
    emsg->to = erl_mk_pid(msg.to.node,msg.to.num,msg.to.serial,msg.to.creation);
    emsg->msg = NULL;
    return ERL_MSG;
    

  case ERL_EXIT:
  case ERL_EXIT2:
    emsg->from = erl_mk_pid(msg.from.node,msg.from.num,msg.from.serial,msg.from.creation);
    emsg->to = erl_mk_pid(msg.to.node,msg.to.num,msg.to.serial,msg.to.creation);
    emsg->msg = erl_decode(*buf); /* contains reason */
    return ERL_MSG;

  case ERL_NODE_LINK:
    emsg->to = NULL;
    emsg->from = NULL;
    emsg->msg = NULL;
    return ERL_MSG;

  default:
      if (emsg->to) erl_free_term(emsg->to);
      if (emsg->from) erl_free_term(emsg->from);
      if (emsg->msg) erl_free_term(emsg->msg);
      emsg->to = NULL;
      emsg->from = NULL;
      emsg->msg = NULL;

      erl_errno = EIO;
      return ERL_ERROR;
  }
} /* do_receive_msg */


int 
erl_receive_msg (int fd, unsigned char *buf, int bufsize, ErlMessage *emsg)
{
    return do_receive_msg(fd, 1, &buf, &bufsize, emsg);
}

int 
erl_xreceive_msg (int fd, unsigned char **buf, int *bufsize,
		  ErlMessage *emsg)
{
    return do_receive_msg(fd, 0, buf, bufsize, emsg);
}

/* 
 * The RPC consists of two parts, send and receive.
 * Here is the send part ! 
 * { PidFrom, { call, Mod, Fun, Args, user }} 
 */
/*
 * Now returns non-negative number for success, negative for failure.
 */
int
erl_rpc_to (int fd, char *mod, char *fun, ETERM *args)
{
  char sbuf[SMALLBUF];
  char *dbuf = NULL;
  char *msgbuf = NULL;
  erlang_pid *self = erl_self();
  int index = 0;
  int msglen = 0;
  char tmp;

  msglen = erl_term_len2(args,erl_distversion(fd));

  /* determine how much buffer we need for this */
  index = 0;
  ei_encode_version(NULL,&index);
  ei_encode_tuple_header(NULL,&index,2);
  
  self->num = fd;
  ei_encode_pid(NULL,&index,self);

  ei_encode_tuple_header(NULL,&index,5);
  ei_encode_atom(NULL,&index,"call");
  ei_encode_atom(NULL,&index,mod);
  ei_encode_atom(NULL,&index,fun);
  index += msglen-1; /* see coment below for explanation */
  ei_encode_atom(NULL,&index,"user");

  /* get a large enough buffer to encode this into */
  if (index > SMALLBUF)
    if (!(dbuf = malloc(index)))
    {
	erl_errno = ENOMEM;
	return -1;
    }
  msgbuf = (dbuf ? dbuf : sbuf);
  
  /* now encode the message into the message buffer */
  index = 0;
  ei_encode_version(msgbuf,&index);
  ei_encode_tuple_header(msgbuf,&index,2);
  
  self->num = fd;
  ei_encode_pid(msgbuf,&index,self);

  ei_encode_tuple_header(msgbuf,&index,5);
  ei_encode_atom(msgbuf,&index,"call");
  ei_encode_atom(msgbuf,&index,mod);
  ei_encode_atom(msgbuf,&index,fun);
  /* erl_encode would write an unwanted version byte at msgbuf[index], so
   * we pass it msgbuf[index-1] instead, saving the byte at that position
   * first and restoring it afterwards.
   */
  /*
   * ugh!
   */
  tmp = msgbuf[index-1];
  erl_encode3(args,msgbuf+index-1,erl_distversion(fd));
  msgbuf[index-1] = tmp;
  index += msglen-1;
  ei_encode_atom(msgbuf,&index,"user");

  ei_send_reg_encoded(fd, self, "rex", msgbuf, index);

  if (dbuf) free(dbuf);

  return 0;
} /* rpc_to */

/*
 * And here is the rpc receiving part. A negative
 * timeout means 'infinity'. Returns either of: ERL_MSG,
 * ERL_TICK, ERL_ERROR or ERL_TIMEOUT.
 */
int 
erl_rpc_from (int fd, int timeout, ErlMessage *emsg) 
{
  char rbuf[MAX_RECEIVE_BUF];
  fd_set readmask;
  struct timeval tv;
  struct timeval *t = NULL;

  if (timeout >= 0) {
    tv.tv_sec = timeout / 1000;
    tv.tv_usec = (timeout % 1000) * 1000;
    t = &tv;
  }

  FD_ZERO(&readmask);
  FD_SET(fd,&readmask);

  switch (select(FD_SETSIZE, &readmask, NULL, NULL, t)) 
  {
  case -1: 
      erl_errno = EIO;
      return ERL_ERROR;

  case 0:
      erl_errno = ETIMEDOUT;
    return ERL_TIMEOUT;

  default:
      if (FD_ISSET(fd, &readmask)) 
	  return erl_receive_msg(fd, (unsigned char *) rbuf, sizeof(rbuf), emsg);
      else
      {
	  erl_errno = EIO;
	  return ERL_ERROR;
      }
  }
} /* rpc_from */

/*
 * A true RPC. It return a NULL pointer
 * in case of failure, otherwise a valid
 * (ETERM *) pointer containing the reply
 */
ETERM *
erl_rpc (int fd, char *mod, char *fun, ETERM *args)
{
  int i;
  ETERM *ep;
  ErlMessage emsg;

  if (erl_rpc_to(fd, mod, fun, args) < 0)
  {
      return NULL;
  }
  while ((i=erl_rpc_from(fd, ERL_NO_TIMEOUT, &emsg)) == ERL_TICK);

  if (i == ERL_ERROR)  return NULL;

  ep = erl_element(2,emsg.msg); /* {RPC_Tag, RPC_Reply} */
  erl_free_term(emsg.msg);
  erl_free_term(emsg.to);
  return ep;
} /* rpc */

  
/*
** Handshake
*/


/* FROM RTP RFC 1889  (except that we use all bits, bug in RFC?) */
static unsigned int
md_32 (char* string, int length)
{
  MD5_CTX ctx;
  union {
    char c[16];
    unsigned x[4];
  } digest;
  ei_MD5Init(&ctx);
  ei_MD5Update(&ctx, (unsigned char *) string, 
	       (unsigned) length);
  ei_MD5Final((unsigned char *) digest.c, &ctx);
  return (digest.x[0] ^ digest.x[1] ^ digest.x[2] ^ digest.x[3]);
}

#if defined(__WIN32__)
static unsigned int 
gen_challenge (void)
{
  struct {
    SYSTEMTIME tv;
    DWORD cpu;
    int pid;
  } s;
  GetSystemTime(&s.tv);
  s.cpu  = GetTickCount();
  s.pid  = getpid();
  return md_32((char*) &s, sizeof(s));
}
#elif  defined(VXWORKS)
static unsigned int 
gen_challenge (void)
{
  struct {
    struct timespec tv;
    clock_t cpu;
    int pid;
  } s;
  s.cpu  = clock();
  clock_gettime(CLOCK_REALTIME, &s.tv);
  s.pid = getpid();
  return md_32((char*) &s, sizeof(s));
}

#else  /* some unix */
static unsigned int 
gen_challenge (void)
{
  struct {
    struct timeval tv;
    clock_t cpu;
    pid_t pid;
    u_long hid;
    uid_t uid;
    gid_t gid;
    struct utsname name;
  } s;
  long gethostid();
  gettimeofday(&s.tv, 0);
  uname(&s.name);
  s.cpu  = clock();
  s.pid  = getpid();
  s.hid  = gethostid();
  s.uid  = getuid();
  s.gid  = getgid();
  return md_32((char*) &s, sizeof(s));
}
#endif

static void 
gen_digest (unsigned challenge, char cookie[], 
	    unsigned char digest[16])
{
  MD5_CTX c;

  char chbuf[20];

  sprintf(chbuf,"%u", challenge);
  ei_MD5Init(&c);
  ei_MD5Update(&c, (unsigned char *) cookie, 
	       (unsigned) strlen(cookie));
  ei_MD5Update(&c, (unsigned char *) chbuf, 
	       (unsigned) strlen(chbuf));
  ei_MD5Final(digest, &c);
}


#ifdef DEBUG_DIST
static char *
hex (char digest[16])
{
  unsigned char *d = (unsigned char *) digest;
  static char buff[sizeof(digest)*2 + 1];
  char *p = buff;
  static char tab[] = "0123456789abcdef";
  int i;

  for (i = 0; i < sizeof(digest); ++i) {
    *p++ = tab[(int)((*d) >> 4)];
    *p++ = tab[(int)((*d++) & 0xF)];
  }
  *p = '\0';
  return buff;
}
#endif
    
static int 
read_2byte_package (int fd, char **buf, int *buflen, 
		    int *is_static)
{
  unsigned char nbuf[2];
  unsigned char *x = nbuf;
  unsigned len;

  if(erl_read_fill(fd, nbuf, 2) != 2)
  {
      erl_errno = EIO;
      return -1;
  }
  len = get16be(x);

  if (len > *buflen) {
    if (*is_static) {
      char *tmp = malloc(len);
      if (!tmp)
      {
	  erl_errno = ENOMEM;
	  return -1;
      }
      *buf = tmp;
      *is_static = 0;
      *buflen = len;
    } else {
      char *tmp = realloc(*buf, len);
      if (!tmp)
      {
	  erl_errno = ENOMEM;
	  return -1;
      }
      *buf = tmp;
      *buflen = len;
    }
  }
  if (erl_read_fill(fd, *buf, len) != len)
  {
      erl_errno = EIO;
      return -1;
  }
  return len;
}
    

static int 
send_status (int fd, char *status)
{
  char *buf, *s;
  char dbuf[DEFBUF_SIZ];
  int siz = strlen(status) + 1 + 2;
  buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
  s = buf;
  put16be(s,siz - 2);
  put8(s, 's');
  memcpy(s, status, strlen(status));
  if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> SEND_STATUS socket write failed\n");
    }
#endif
    if (buf != dbuf)
      free(buf);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> SEND_STATUS (%s)\n",status);
  }
#endif
  if (buf != dbuf)
    free(buf);
  return 0;
}

static int 
recv_status (int fd)
{
  char dbuf[DEFBUF_SIZ];
  char *buf = dbuf;
  int is_static = 1;
  int buflen = DEFBUF_SIZ;
  int rlen;

  if ((rlen = read_2byte_package(fd, &buf, &buflen, &is_static)) <= 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_STATUS socket read failed (%d)\n", rlen);
    }
#endif
    goto error;
  }
  if (rlen == 3 && buf[0] == 's' && buf[1] == 'o' && 
      buf[2] == 'k') {
    if (!is_static)
      free(buf);
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_STATUS (ok)\n");
    }
#endif
    return 0;
  }
 error:
  if (!is_static)
    free(buf);
  return -1;
}

static int 
send_challenge (int fd, char *nodename, 
		unsigned challenge, unsigned version) 
{
  char *buf, *s;
  char dbuf[DEFBUF_SIZ];
  int siz = 2 + 1 + 2 + 4 + 4 + strlen(nodename);
  buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
  s = buf;
  put16be(s,siz - 2);
  put8(s, 'n');
  put16be(s, version);
  put32be(s, (DFLAG_EXTENDED_REFERENCES));
  put32be(s, challenge);
  memcpy(s, nodename, strlen(nodename));

  if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> SEND_CHALLENGE socket write failed\n");
    }
#endif
    if (buf != dbuf)
      free(buf);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> SEND_CHALLENGE (ok) challenge = %d, "
	    "version = %d, "
	    "nodename = %s\n",
	    challenge, version, nodename);
  }
#endif

  if (buf != dbuf)
    free(buf);
  return 0;
}

static int 
recv_challenge (int fd, unsigned *challenge, 
		unsigned *version,
		unsigned *flags, ErlConnect *namebuf)
{
  char dbuf[DEFBUF_SIZ];
  char *buf = dbuf;
  int is_static = 1;
  int buflen = DEFBUF_SIZ;
  int rlen;
  char *s;
  struct sockaddr_in sin;
  int sin_len = sizeof(sin);
  char tag;

  if ((rlen = read_2byte_package(fd, &buf, &buflen, &is_static)) <= 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE socket read failed (%d)\n",rlen);
    }
#endif
    goto error;
  }
  if ((rlen - 11) > MAXNODELEN) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE nodename too long (%d)\n",rlen - 11);
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  s = buf;
  if ((tag = get8(s)) != 'n') {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE incorrect tag, expected 'n' got '%c' (%u)\n",tag,tag);
    }
#endif
    goto error;
  }
  *version = get16be(s);
  *flags = get32be(s);
  *challenge = get32be(s);
  if (getpeername(fd, (struct sockaddr *) &sin, &sin_len) < 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE can't get peername\n");
    }
#endif
    erl_errno = errno;
    goto error;
  }
  memcpy(namebuf->ipadr, &(sin.sin_addr.s_addr), 
	 sizeof(sin.sin_addr.s_addr));
  memcpy(namebuf->nodename, s, rlen - 11);
  namebuf->nodename[rlen - 11] = '\0';
  if (!is_static)
    free(buf);
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- RECV_CHALLENGE (ok) node = %s, "
	    "version = %u, "
	    "flags = %u, "
	    "challenge = %d\n",
	    namebuf->nodename,
	    *version,
	    *flags,
	    *challenge
	    );
  }
#endif
  return 0;
 error:
  if (!is_static)
    free(buf);
  return -1;
}

static int 
send_challenge_reply (int fd, char digest[16], 
		      unsigned challenge) 
{
  char *s;
  char buf[DEFBUF_SIZ];
  int siz = 2 + 1 + 4 + 16;
  s = buf;
  put16be(s,siz - 2);
  put8(s, 'r');
  put32be(s, challenge);
  memcpy(s, digest, 16);

  if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> SEND_CHALLENGE_REPLY socket write failed\n");
    }
#endif
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> SEND_CHALLENGE_REPLY (ok) challenge = %d, "
	    "digest = %s\n",
	    challenge,hex(digest));
  }
#endif
  return 0;
}

static int 
recv_challenge_reply (int fd, 
		      unsigned our_challenge,
		      char cookie[], 
		      unsigned *her_challenge)
{
  char dbuf[DEFBUF_SIZ];
  char *buf = dbuf;
  int is_static = 1;
  int buflen = DEFBUF_SIZ;
  int rlen;
  char *s;
  char tag;
  char her_digest[16], expected_digest[16];

  if ((rlen = read_2byte_package(fd, &buf, &buflen, &is_static)) != 21) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_REPLY socket read failed (%d)\n",rlen);
    }
#endif
    goto error;
  }
    
  s = buf;
  if ((tag = get8(s)) != 'r') {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_REPLY incorrect tag, expected 'r' got '%c' (%u)\n",tag,tag);
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  *her_challenge = get32be(s);
  memcpy(her_digest, s, 16);
  gen_digest(our_challenge, cookie, expected_digest);
  if (memcmp(her_digest, expected_digest, 16)) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_REPLY authorization failure\n");
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  if (!is_static)
    free(buf);
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- RECV_CHALLENGE_REPLY (ok) challenge = %u, "
	    "digest = %s\n",
	    *her_challenge,hex(her_digest));
  }
#endif
  return 0;

 error:
  if (!is_static)
    free(buf);
  return -1;
}

static int 
send_challenge_ack (int fd, char digest[16]) 
{
  char *s;
  char buf[DEFBUF_SIZ];
  int siz = 2 + 1 + 16;
  s = buf;

  put16be(s,siz - 2);
  put8(s, 'a');
  memcpy(s, digest, 16);

  if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> SEND_CHALLENGE_ACK socket write failed\n");
    }
#endif
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> SEND_CHALLENGE_ACK (ok) "
	    "digest = %s\n",
	    hex(digest));
  }
#endif

  return 0;
}

static int 
recv_challenge_ack (int fd, 
		    unsigned our_challenge,
		    char cookie[])
{
  char dbuf[DEFBUF_SIZ];
  char *buf = dbuf;
  int is_static = 1;
  int buflen = DEFBUF_SIZ;
  int rlen;
  char *s;
  char tag;
  char her_digest[16], expected_digest[16];

  if ((rlen = read_2byte_package(fd, &buf, &buflen, &is_static)) != 17) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_ACK socket read failed (%d)\n",rlen);
    }
#endif
    goto error;
  }
    
  s = buf;
  if ((tag = get8(s)) != 'a') {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_ACK incorrect tag, expected 'a' got '%c' (%u)\n",tag,tag);
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  memcpy(her_digest, s, 16);
  gen_digest(our_challenge, cookie, expected_digest);
  if (memcmp(her_digest, expected_digest, 16)) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_CHALLENGE_ACK authorization failure\n");
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  if (!is_static)
    free(buf);
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- RECV_CHALLENGE_ACK (ok) "
	    "digest = %s\n",
	    hex(her_digest));
  }
#endif
  return 0;
 error:
  if (!is_static)
    free(buf);
  return -1;
}

static int 
send_name (int fd, char *nodename, 
	   unsigned version) 
{
  char *buf, *s;
  char dbuf[DEFBUF_SIZ];
  int siz = 2 + 1 + 2 + 4 + strlen(nodename);
  buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
  s = buf;
  put16be(s,siz - 2);
  put8(s, 'n');
  put16be(s, version);
  put32be(s, (DFLAG_EXTENDED_REFERENCES));
  memcpy(s, nodename, strlen(nodename));

  if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"-> SEND_NAME socket write failed\n");
    }
#endif
    if (buf != dbuf)
      free(buf);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"-> SEND_NAME (ok) "
	    "version = %d, "
	    "nodename = %s\n",
	    version, nodename);
  }
#endif

  if (buf != dbuf)
    free(buf);
  return 0;
}


static int 
recv_name (int fd, 
	   unsigned *version,
	   unsigned *flags, ErlConnect *namebuf)
{
  char dbuf[DEFBUF_SIZ];
  char *buf = dbuf;
  int is_static = 1;
  int buflen = DEFBUF_SIZ;
  int rlen;
  char *s;
  struct sockaddr_in sin;
  int sin_len = sizeof(sin);
  char tag;

  if ((rlen = read_2byte_package(fd, &buf, &buflen, &is_static)) <= 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_NAME socket read failed (%d)\n",rlen);
    }
#endif
    goto error;
  }
  if ((rlen - 7) > MAXNODELEN) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_NAME nodename too long (%d)\n",rlen - 7);
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  s = buf;
  tag = get8(s);
  if (tag != 'n') {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_NAME incorrect tag, expected 'n' got '%c' (%u)\n",tag,tag);
    }
#endif
    erl_errno = EIO;
    goto error;
  }
  *version = get16be(s);
  *flags = get32be(s);
  if (getpeername(fd, (struct sockaddr *) &sin, &sin_len) < 0) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- RECV_NAME can't get peername\n");
    }
#endif
    erl_errno = errno;
    goto error;
  }
  memcpy(namebuf->ipadr, &(sin.sin_addr.s_addr), 
	 sizeof(sin.sin_addr.s_addr));
  memcpy(namebuf->nodename, s, rlen - 7);
  namebuf->nodename[rlen - 7] = '\0';
  if (!is_static)
    free(buf);
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) {
    fprintf(stderr,"<- RECV_NAME (ok) node = %s, "
	    "version = %u, "
	    "flags = %u\n",
	    namebuf->nodename,
	    *version,
	    *flags
	    );
  }
#endif
  return 0;

 error:
  if (!is_static)
    free(buf);
  return -1;
}
