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
* Purpose: Connect to any node at any host. (EI version)
*/

#ifdef HAVE_CONFIG_H
# include "config.h"	      /* FIXME: Autoconf Info prefers <config.h> */
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
#include <time.h>
#endif

/* common includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "erl_interface.h"
#include "erl_config.h"
#include "putget.h"
#include "ei.h"
#include "ei_connect.h"
#include "eisend.h"
#include "erl_locking.h"

#if !defined(__WIN32__) && !defined(VXWORKS) 
extern int gethostname();
#endif

extern int erl_read_fill(int,char*,int);
extern int erl_write_fill(int,char*,int);

extern int erl_encode3(ETERM*,unsigned char*, int);
extern int erl_term_len2(ETERM*, int);

/*#define SELF(fd) erl_mk_pid(ec.thisnodename, fd, 0, ec.creation)*/

#define COOKIE_FILE "/.erlang.cookie"

#ifdef __WIN32__
void initWinSock(void);
#endif /* __WIN32__ */

/* Distribution capability flags */
#define DFLAG_PUBLISHED           1
#define DFLAG_ATOM_CACHE          2
#define DFLAG_EXTENDED_REFERENCES 4
#define DFLAG_DIST_MONITOR        8
#define DFLAG_FUN_TAGS            16
#define DFLAG_NEW_FUN_TAGS        0x80

int ei_trace_distribution = 0;

/* static int init_done = 0; / * write-once flag for above global data */

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

int ei_send_reg_encoded_timeout(int fd, const erlang_pid *from,
				const char *to, const char *msg, int msglen,
				int timeout);

/* Common for both handshake types */
static int recv_name(int fd, 
		     unsigned *version,
		     unsigned *flags, ErlConnect *namebuf);


typedef struct ei_socket_info_s {
    int socket;
    int dist_version;
    char cookie[MAX_COOKIE_SIZE+1];
} ei_socket_info;

int ei_n_sockets = 0, ei_sz_sockets = 0;
ei_socket_info *ei_sockets = NULL;
erl_mutex_t* ei_sockets_lock = NULL;

int put_ei_socket_info(int fd, int dist_version, char* cookie)
{
    int i;

    erl_mutex_lock(ei_sockets_lock, 0);
    for (i = 0; i < ei_n_sockets; ++i) {
	if (ei_sockets[i].socket == fd) {
	    if (dist_version == -1) {
		memmove(&ei_sockets[i], &ei_sockets[i+1],
			sizeof(ei_sockets[0])*(ei_n_sockets-i-1));
	    } else {
		ei_sockets[i].dist_version = dist_version;
		strcpy(ei_sockets[i].cookie, cookie);
	    }
	    erl_mutex_unlock(ei_sockets_lock);
	    return 0;
	}
    }
    if (ei_n_sockets == ei_sz_sockets) {
	ei_sz_sockets += 5;
	ei_sockets = realloc(ei_sockets,
			     sizeof(ei_sockets[0])*ei_sz_sockets);
	if (ei_sockets == NULL) {
	    ei_sz_sockets = ei_n_sockets = 0;
	    erl_mutex_unlock(ei_sockets_lock);
	    return -1;
	}
	ei_sockets[ei_n_sockets].socket = fd;
	ei_sockets[ei_n_sockets].dist_version = dist_version;
	strcpy(ei_sockets[ei_n_sockets].cookie, cookie);
	++ei_n_sockets;
    }
    erl_mutex_unlock(ei_sockets_lock);
    return 0;
}
			  
int remove_ei_socket_info(int fd, int dist_version, char* cookie)
{
    return put_ei_socket_info(fd, -1, NULL);
}

ei_socket_info* get_ei_socket_info(int fd)
{
    int i;
    erl_mutex_lock(ei_sockets_lock, 0);
    for (i = 0; i < ei_n_sockets; ++i)
	if (ei_sockets[i].socket == fd) {
	    /*fprintf("get_ei_socket_info %d  %d \"%s\" \n\r",
		    fd, ei_sockets[i].dist_version, ei_sockets[i].cookie);*/
	    erl_mutex_unlock(ei_sockets_lock);
	    return &ei_sockets[i];
	}
    erl_mutex_unlock(ei_sockets_lock);
    return NULL;
}

int ei_distversion(int fd)
{
    ei_socket_info* e = get_ei_socket_info(fd);
    if (e == NULL)
	return -1;
    else
	return e->dist_version;
}

const char* ei_cookie(int fd)
{
    ei_socket_info* e = get_ei_socket_info(fd);
    if (e == NULL)
	return NULL;
    else
	return e->cookie;
}

const char *ei_thisnodename(const ei_cnode* ec)
{
    return ec->thisnodename;
}

const char *ei_thishostname(const ei_cnode* ec)
{
    return ec->thishostname;
}

const char *ei_thisalivename(const ei_cnode* ec)
{
    return ec->thisalivename;
}

/*
const char *erl_thiscookie(void)
{
    return (const char *) ec.ei_connect_cookie;
}

short erl_thiscreation(void)
{
    return ec.creation;
}

void erl_set_thiscreation(short creation)
{
    ec.creation = creation;
    return;
}*/

erlang_pid *ei_self(ei_cnode* ec)
{
    return &ec->self;
}

/* two internal functions that will let us support different cookies
* (to be able to connect to other nodes that don't have the same
* cookie as each other or us)
*/
const char *ei_getfdcookie(int fd)
{
    const char* r = ei_cookie(fd);
    if (r == NULL) r = "";
    return r;
}

/* call with cookie to set value to use on descriptor fd,
* or specify NULL to use default
*/
int ei_setfdcookie(ei_cnode* ec, int fd, char *cookie)
{
    int dist_version = ei_distversion(fd);

    if (cookie == NULL)
	cookie = ec->ei_connect_cookie;
    return put_ei_socket_info(fd, dist_version, cookie);
}

static int get_int32 (unsigned char *s)
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

static void get_cookie(ei_cnode* ec)
{
    char fname[1024+sizeof(COOKIE_FILE)+1];
    int fd;
    int len;
    
    if (!get_home(fname, sizeof(fname))) {
	erl_err_sys("<ERROR> too long home");
    }
    strcat(fname, COOKIE_FILE);
    if ((fd = open(fname, O_RDONLY, 0777)) < 0)
	erl_err_sys("<ERROR> open cookie file");
    
    
    if ((len = read(fd, ec->ei_connect_cookie,
	sizeof(ec->ei_connect_cookie)-1)) < 0)
	erl_err_sys("<ERROR> reading cookie file (1)");
    else if (len == BUFSIZ)
	erl_err_sys("<ERROR> reading cookie file (2)");
    ec->ei_connect_cookie[len] = '\0';
    if (isspace(ec->ei_connect_cookie[len-1]))	/* allow both '\n' and '\r' */
	ec->ei_connect_cookie[len-1] = '\0';
}

#ifdef __WIN32__
void win32_error(char *buf, int buflen)
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

void initWinSock(void)
{
    WORD wVersionRequested;  
    WSADATA wsaData; 
    int i; 
    static int initialized = 0;
    
    wVersionRequested = MAKEWORD(1, 1); 
    if (!initialized) {
	initialized = 1;
	if ((i = WSAStartup(wVersionRequested, &wsaData))) {
	    erl_err_msg("<ERROR> ei_connect_init: Can't initialize windows sockets: %d",i);
	}
	
	if ( LOBYTE( wsaData.wVersion ) != 1 || HIBYTE( wsaData.wVersion ) != 1 ) { 
	    erl_err_msg("<ERROR> ei_connect_init: This version of windows sockets "
		"not supported");
	    WSACleanup(); 
	}
    }
}
#endif

/*
* Perhaps run this routine instead of ei_connect_init/2 ?
* Initailize by setting:
* thishostname, thisalivename, thisnodename and thisipaddr
*/
int ei_connect_xinit (ei_cnode* ec, const char *thishostname,
		      const char *thisalivename, const char *thisnodename,
		      Erl_IpAddr thisipaddr, const char *cookie,
		      const short creation)
{
    char *dbglevel;
    
    if (ec->init_done) 
	return 0;
    ec->init_done = 1;
    
    if (ei_sockets_lock == NULL) {
	erl_init_locking();
	ei_sockets_lock = erl_mutex_create();
    }
    ec->creation = creation;
    
    if (!cookie) 
	get_cookie(ec);
    else if (strlen(cookie) >= sizeof(ec->ei_connect_cookie)) 
	erl_err_quit("<ERROR> ei_connect_xinit: Cookie size too large");
    else 
	strcpy(ec->ei_connect_cookie, cookie);
    
    if (strlen(thishostname) >= sizeof(ec->thishostname)) 
	erl_err_quit("<ERROR> ei_connect_init: Thishostname too long");
    strcpy(ec->thishostname, thishostname);
    
    if (strlen(thisalivename) >= sizeof(ec->thisalivename)) 
	erl_err_quit("<ERROR> ei_connect_init: Thisalivename too long");
    strcpy(ec->thisalivename, thisalivename);
    
    if (strlen(thisnodename) >= sizeof(ec->thisnodename)) 
	erl_err_quit("<ERROR> ei_connect_init: Thisnodename too long");
    strcpy(ec->thisnodename, thisnodename);
    
    memmove(&ec->this_ipaddr, thisipaddr, sizeof(ec->this_ipaddr)); 
    
    strcpy(ec->self.node,thisnodename);
    ec->self.num = 0;
    ec->self.serial = 0;
    ec->self.creation = creation;
    
    if ((dbglevel = getenv("ERL_DEBUG_DIST")) != NULL)
	ei_trace_distribution = atoi(dbglevel);
    return 0;
}

int ei_connect_init_ex(ei_cnode* ec, const char* this_node_name,
		       const char *cookie, short creation, int use_long_name)
{
    struct hostent *hp;
    char* ct;
    char thishostname[MAXHOSTLEN+1];
    char thisnodename[MAXHOSTLEN+1+MAXALIVELEN+1];
    char thisalivename[MAXALIVELEN+1];

    ec->init_done = 0;
#ifdef __WIN32__
    initWinSock();
#endif /* win32 */
    if (ei_sockets_lock == NULL) {
	erl_init_locking();
	ei_sockets_lock = erl_mutex_create();
    }
    
    if (gethostname(thishostname, MAXHOSTLEN) ==  -1) {
#ifdef __WIN32__
	erl_err_quit("ei_connect_init: Failed to get host name: %d",
	    WSAGetLastError());
#else
	erl_err_quit("ei_connect_init: Failed to get host name: %d",
	    errno);
#endif /* win32 */
    }
    
    if (this_node_name == NULL)
	sprintf(thisalivename, "c%d", (int) getpid());
    else
	strcpy(thisalivename, this_node_name);
    
    if ((hp = erl_gethostbyname(thishostname)) == 0) {
#ifdef __WIN32__
	char reason[1024];
	
	win32_error(reason,sizeof(reason));
	erl_err_msg("ei_connect_init: Can't get ip address for host %s: %s\n",
	    thishostname, reason);
#else
	erl_err_msg("ei_connect_init: Can't get ip address for host %s: %d\n",
	    thishostname,h_errno);
#endif /* win32 */
	return -1;
    }
    
    if ((use_long_name == 0) &&  /* shortnames */
	((ct = strchr(hp->h_name, '.')) != NULL)) *ct = '\0';
    
    strcpy(thishostname, hp->h_name);
    sprintf(thisnodename, "%s@%s", this_node_name, hp->h_name);
    
    return ei_connect_xinit(ec, thishostname, thisalivename, thisnodename,
	(struct in_addr *)*hp->h_addr_list, cookie, creation);
}

/*
* Initialize by set: thishostname, thisalivename, 
* thisnodename and thisipaddr. At success return 0,
* otherwise return -1.
*/
int ei_connect_init(ei_cnode* ec, const char* this_node_name,
		    const char *cookie, short creation)
{
    return ei_connect_init_ex(ec, this_node_name, cookie, creation, 0);
}


/* connects to port at ip-address ip_addr 
* and returns fd to socket 
* port has to be in host byte order 
*/
static int cnct(uint16 port, struct in_addr *ip_addr, int addr_len)
{
    int s;
    struct sockaddr_in iserv_addr;
    
    if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
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
int ei_connect(ei_cnode* ec, char *nodename)
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
    
    return ei_xconnect(ec, (Erl_IpAddr) *hp->h_addr_list, alivename);
} /* erl_connect */


  /* ip_addr is now in network byte order 
  *
  * first we have to get hold of the portnumber to
  *  the node through epmd at that host 
  *
*/
int ei_xconnect(ei_cnode* ec, Erl_IpAddr adr, char *alivename)
{
    struct in_addr *ip_addr=(struct in_addr *) adr;
    int rport = 0; /*uint16 rport = 0;*/
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
    if((sockd = cnct((uint16)rport, ip_addr, sizeof(struct in_addr))) < 0) {
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
	
	if (send_name(sockd, ec->thisnodename, (unsigned) dist))
	    goto error;
	if (recv_status(sockd))
	    goto error;
	if (recv_challenge(sockd, &her_challenge, &her_version,
	    &her_flags, &her_name))
	    goto error;
	our_challenge = gen_challenge();
	gen_digest(her_challenge, ec->ei_connect_cookie, our_digest);
	if (send_challenge_reply(sockd, our_digest, our_challenge))
	    goto error;
	if (recv_challenge_ack(sockd, our_challenge, 
	    ec->ei_connect_cookie))
	    goto error;
	put_ei_socket_info(sockd, dist, null_cookie);
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
int ei_close_connection (int fd)
{
    return closesocket(fd);
} /* erl_close_connection */


  /*
  * Accept and initiate a connection from an other
  * Erlang node. Return a file descriptor at success,
  * otherwise -1;
*/
int ei_accept(ei_cnode* ec, int lfd, ErlConnect *conp)
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
	if (send_challenge(fd, ec->thisnodename, 
	    our_challenge, her_version))
	    goto error;
	if (recv_challenge_reply(fd, our_challenge, 
	    ec->ei_connect_cookie, 
	    &her_challenge))
	    goto error;
	gen_digest(her_challenge, ec->ei_connect_cookie, our_digest);
	if (send_challenge_ack(fd, our_digest))
	    goto error;
	put_ei_socket_info(fd, her_version, null_cookie);
    }
    if (conp) 
	*conp = her_name;
    
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
int ei_receive(int fd, unsigned char *bufp, int bufsize) 
{
    int len;
    unsigned char fourbyte[4]={0,0,0,0};
    
    if (erl_read_fill(fd, (char *) bufp, 4)  != 4) {
	erl_errno = EIO;
	return ERL_ERROR;
    }
    
    /* Tick handling */
    if ((len = get_int32(bufp)) == ERL_TICK) 
    {
	erl_write_fill(fd, (char *) fourbyte, 4);
	erl_errno = EAGAIN;
	return ERL_TICK;
    }
    else if (len > bufsize) 
    {
	/* FIXME: We should drain the message. */
	erl_errno = EMSGSIZE;
	return ERL_ERROR;
    }
    else if (erl_read_fill(fd, (char *) bufp, len) != len)
    {
	erl_errno = EIO;
	return ERL_ERROR;
    }
    
    return len;
    
}

/* 
 * Send an Erlang message to a registered process
 * at the Erlang node, connected with a socket.
 */
int ei_reg_send_timeout(ei_cnode* ec, int fd, char *server_name,
			char* buf, int len, int timout)
{
    erlang_pid *self = ei_self(ec);
    self->num = fd;
    return ei_send_reg_encoded_timeout(fd, self, server_name,
				       buf, len, timout);
}

int ei_reg_send(ei_cnode* ec, int fd, char *server_name, char* buf, int len)
{
    erlang_pid *self = ei_self(ec);
    self->num = fd;
    if (ei_send_reg_encoded(fd, self, server_name, buf, len)) {
	erl_errno = EIO;
	return -1;
    }
    return 0;
}

/* 
* Sends an Erlang message to a process at an Erlang node
*/
int ei_send(int fd, erlang_pid* to, char* buf, int len)
{
    if (ei_send_encoded(fd, to, buf, len) != 0) {
	erl_errno = EIO;
	return -1;
    }
    return 0;
}

extern int ei_recv_internal(int fd, char **mbufp, int *bufsz, erlang_msg *msg, int *msglen, int staticbufp);

/* 
* Try to receive an Erlang message on a given socket. Returns
* ERL_TICK, ERL_MSG, or ERL_ERROR. Sets `erl_errno' on ERL_ERROR and
* ERL_TICK (to EAGAIN in the latter case).
*/

int ei_do_receive_msg (int fd, 
			   int staticbuffer_p, 
			   erlang_msg* msg, ei_x_buff* x)
{
    int msglen;
    int i;
    
    if (!(i=ei_recv_internal(fd, &x->buff, &x->buffsz, msg, &msglen, 
	staticbuffer_p))) {
	erl_errno = EAGAIN;
	return ERL_TICK;
    }
    if (i<0) {
	/* erl_errno set by ei_recv_internal() */
	return ERL_ERROR;
    }
    if (staticbuffer_p && msglen > x->buffsz)
    {
	erl_errno = EMSGSIZE;
	return ERL_ERROR;
    }
    x->index = x->buffsz;
    switch (msg->msgtype) {
    case ERL_SEND:
	return ERL_MSG;
	break;
	
    case ERL_REG_SEND:
	return ERL_MSG;
	break;
	
    case ERL_LINK:
    case ERL_UNLINK:
    case ERL_GROUP_LEADER:
	return ERL_MSG;
	
	
    case ERL_EXIT:
    case ERL_EXIT2:
	return ERL_MSG;
	
    case ERL_NODE_LINK:
	return ERL_MSG;
	
    default:
	/*if (emsg->to) erl_free_term(emsg->to);
	  if (emsg->from) erl_free_term(emsg->from);
	  if (emsg->msg) erl_free_term(emsg->msg);
	  emsg->to = NULL;
	  emsg->from = NULL;
	  emsg->msg = NULL;*/
	
	erl_errno = EIO;
	return ERL_ERROR;
    }
} /* do_receive_msg */


int ei_receive_msg (int fd, erlang_msg* msg, ei_x_buff* x)
{
    return ei_do_receive_msg(fd, 1, msg, x);
}

int ei_xreceive_msg (int fd, erlang_msg *msg, ei_x_buff *x)
{
    return ei_do_receive_msg(fd, 0, msg, x);
}

/* 
* The RPC consists of two parts, send and receive.
* Here is the send part ! 
* { PidFrom, { call, Mod, Fun, Args, user }} 
*/
/*
* Now returns non-negative number for success, negative for failure.
*/
int ei_rpc_to(ei_cnode *ec, int fd, char *mod, char *fun,
	      const char *buf, int len)
{

    ei_x_buff x;
    erlang_pid *self = ei_self(ec);
    self->num = fd;

    /* encode header */
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);  /* A */
    
    self->num = fd;
    ei_x_encode_pid(&x, self);	      /* A 1 */
    
    ei_x_encode_tuple_header(&x, 5);  /* B A 2 */
    ei_x_encode_atom(&x, "call");     /* B 1 */
    ei_x_encode_atom(&x, mod);	      /* B 2 */
    ei_x_encode_atom(&x, fun);	      /* B 3 */
    ei_x_append_buf(&x, buf, len);    /* B 4 */
    ei_x_encode_atom(&x, "user");     /* B 5 */

    /* ei_x_encode_atom(&x,"user"); */
    ei_send_reg_encoded(fd, self, "rex", x.buff, x.index);
    ei_x_free(&x);
	
    return 0;
} /* rpc_to */

  /*
  * And here is the rpc receiving part. A negative
  * timeout means 'infinity'. Returns either of: ERL_MSG,
  * ERL_TICK, ERL_ERROR or ERL_TIMEOUT.
*/
int ei_rpc_from(ei_cnode *ec, int fd, int timeout, erlang_msg *msg,
		ei_x_buff *x) 
{
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
    
    switch (select(FD_SETSIZE, &readmask, NULL, NULL, t)) {
    case -1: 
	erl_errno = EIO;
	return ERL_ERROR;
	
    case 0:
	erl_errno = ETIMEDOUT;
	return ERL_TIMEOUT;
	
    default:
	if (FD_ISSET(fd, &readmask)) {
	    return ei_xreceive_msg(fd, msg, x);
	} else {
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
int ei_rpc(ei_cnode* ec, int fd, char *mod, char *fun,
	    const char* inbuf, int inbuflen, ei_x_buff* x)
{
    int i, index;
    ei_term t;
    erlang_msg msg;
    char rex[MAXATOMLEN];

    if (ei_rpc_to(ec, fd, mod, fun, inbuf, inbuflen) < 0) {
	return -1;
    }
    while ((i = ei_rpc_from(ec, fd, ERL_NO_TIMEOUT, &msg, x)) == ERL_TICK)
	;

    if (i == ERL_ERROR)  return -1;
    /*ep = erl_element(2,emsg.msg);*/ /* {RPC_Tag, RPC_Reply} */
    index = 0;
    if (ei_decode_version(x->buff, &index, &i) < 0
	|| ei_decode_ei_term(x->buff, &index, &t) < 0)
	return -1;
    if (t.ei_type == ERL_SMALL_TUPLE_EXT && t.arity == 2)
	if (ei_decode_atom(x->buff, &index, rex) < 0)
	    return -1;
    /* remove header */
    x->index -= index;
    memmove(x->buff, &x->buff[index], x->index);
    return 0;
}


  /*
  ** Handshake
*/


/* FROM RTP RFC 1889  (except that we use all bits, bug in RFC?) */
unsigned int
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
unsigned int gen_challenge(void)
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
static unsigned int gen_challenge(void)
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
    
    if(erl_read_fill(fd, nbuf, 2) != 2) {
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
	    if (!tmp) {
		erl_errno = ENOMEM;
		return -1;
	    }
	    *buf = tmp;
	    *buflen = len;
	}
    }
    if (erl_read_fill(fd, *buf, len) != len) {
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

static int send_name_or_challenge (int fd, char *nodename,
				   int f_chall,
				   unsigned challenge,
				   unsigned version) 
{
    char *buf, *s;
    char dbuf[DEFBUF_SIZ];
    int siz = 2 + 1 + 2 + 4 + strlen(nodename);
#ifdef DEBUG_DIST
    const char* function[] = {"SEND_NAME", "SEND_CHALLENGE"};
#endif
    if (f_chall)
	siz += 4;
    buf = (siz > DEFBUF_SIZ) ? malloc(siz) : dbuf;
    s = buf;
    put16be(s,siz - 2);
    put8(s, 'n');
    put16be(s, version);
    put32be(s, (DFLAG_EXTENDED_REFERENCES|DFLAG_FUN_TAGS|DFLAG_NEW_FUN_TAGS));
    if (f_chall)
	put32be(s, challenge);
    memcpy(s, nodename, strlen(nodename));
    
    if (erl_write_fill(fd, buf, siz) != siz) {
#ifdef DEBUG_DIST
	if (ei_trace_distribution > 2)
	    fprintf(stderr,"-> %s socket write failed\n", function[f_chall]);
#endif
	if (buf != dbuf)
	    free(buf);
	erl_errno = EIO;
	return -1;
    }
    
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
	fprintf(stderr,"-> %s (ok) challenge = %d, "
	    "version = %d, "
	    "nodename = %s\n", function[f_chall],
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

static int send_name (int fd, char *nodename, unsigned version) 
{
    return send_name_or_challenge(fd, nodename, 0, 0, version);
}

static int send_challenge(int fd, char *nodename, 
			  unsigned challenge, unsigned version)
{
    return send_name_or_challenge(fd, nodename, 1, challenge, version);
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
