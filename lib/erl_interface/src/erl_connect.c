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

#include <stdlib.h>
#include <sys/types.h>
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
#include "ei_connect.h"

#if !defined(__WIN32__) && !defined(VXWORKS) 
extern int gethostname();
#endif

extern int erl_read_fill(int,char*,int);
extern int erl_write_fill(int,char*,int);

extern int erl_encode3(ETERM*,unsigned char*, int);
extern int erl_term_len2(ETERM*, int);

#if defined(__WIN32__)
void win32_error(char *buf, int buflen);
void initWinSock (void);
#endif

#define SELF(fd) erl_mk_pid(ec.thisnodename, fd, 0, ec.creation)

#define COOKIE_FILE "/.erlang.cookie"

#ifdef __WIN32__
void initWinSock(void);
#endif /* __WIN32__ */

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

extern int ei_trace_distribution; /* in ei_connect */

static int init_done = 0; /* write-once flag for above global data */

ei_cnode ec;

int erl_distversion(int fd)
{
    return ei_distversion(fd);
}

/*int erl_distversion(int fd)
{
    return ec.conns[fd];
}*/

const char *erl_thisnodename(void)
{
    return ei_thisnodename(&ec);
}

const char *erl_thishostname(void)
{
    return ei_thishostname(&ec);
}

const char *erl_thisalivename(void)
{
    return ei_thisalivename(&ec);
}

/*const char *erl_thiscookie(void)
{
    return (const char *) ec.erl_connect_cookie;
}*/

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
/*const char *erl_getfdcookie(int fd)
{
    return (const char *) 
	(ec.cookies[fd] ? ec.cookies[fd] : ec.erl_connect_cookie);
}*/

/* call with cookie to set value to use on descriptor fd,
* or specify NULL to use default
*/
/*int 
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
}*/

/*static int get_int32 (unsigned char *s)
{
    return ((s[0] << 24) | (s[1] << 16) | (s[2] << 8) | (s[3] ));
}*/

static int get_home (char *buf, int size)
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

static void get_cookie(void)
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
    
    
    if ((len = read(fd, ec.ei_connect_cookie,
	sizeof(ec.ei_connect_cookie)-1)) < 0)
	erl_err_sys("<ERROR> reading cookie file (1)");
    else if (len == BUFSIZ)
	erl_err_sys("<ERROR> reading cookie file (2)");
    ec.ei_connect_cookie[len] = '\0';
    if (ec.ei_connect_cookie[len-1] == '\n')
	ec.ei_connect_cookie[len-1] = '\0';
}

/*
* Perhaps run this routine instead of erl_connect_init/2 ?
* Initailize by setting:
* thishostname, thisalivename, thisnodename and thisipaddr
*/
int 
erl_connect_xinit (char *thishostname, char *thisalivename, char *thisnodename,
		   Erl_IpAddr thisipaddr, char *cookie, short creation)
{
    extern erl_mutex_t* ei_sockets_lock;
    char *dbglevel;
    
    if (init_done) return 1;
    init_done = 1;
    
    ec.creation = creation;
    
#ifdef __WIN32__
    initWinSock();
#endif
    
    if (ei_sockets_lock == NULL) {
	erl_init_locking();
	ei_sockets_lock = erl_mutex_create();
    }
    if (!cookie) get_cookie();
    else if (strlen(cookie) >= sizeof(ec.ei_connect_cookie)) 
	erl_err_quit("<ERROR> erl_connect_xinit: Cookie size too large");
    else strcpy(ec.ei_connect_cookie, cookie);
    
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
    
    /*memset(&ec.conns,0,sizeof(ec.conns));
    for (i=0; i<FD_SETSIZE; i++) ec.cookies[i] = ec.ei_connect_cookie;*/
    
    if ((dbglevel=getenv("ERL_DEBUG_DIST"))) 
	ei_trace_distribution = atoi(dbglevel);
    
    return 1;
}

int erl_connect_init_ex(int this_node_number, char *cookie,
			short creation, int use_long_name)
{
    char nn[MAXATOMLEN];
    sprintf(nn, "c%d", this_node_number);
    return ei_connect_init_ex(&ec, nn, cookie, creation, use_long_name) == 0;
}

/*
* Initialize by set: thishostname, thisalivename, 
* thisnodename and thisipaddr. At success return 1,
* otherwise return 0.
*/
int erl_connect_init(int this_node_number, char *cookie, short creation)
{
    char nn[MAXATOMLEN];
    sprintf(nn, "c%d", this_node_number);
    return ei_connect_init(&ec, nn, cookie, creation) == 0;
}

/* 
 * Set up a connection to a given Node, and 
 * interchange hand shake messages with it.
 * Returns a valid file descriptor at success,
 * otherwise a negative error code.
 */
int erl_connect (char *nodename)
{
    return ei_connect(&ec, nodename);
}

/* ip_addr is now in network byte order 
 *
 * first we have to get hold of the portnumber to
 *  the node through epmd at that host 
 *
 */
int erl_xconnect (Erl_IpAddr adr, char *alivename)
{
    return ei_xconnect(&ec, adr, alivename);
}

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
int erl_accept (int lfd, ErlConnect *conp)
{
    return ei_accept(&ec, lfd, conp);
}


/* Receives a message from an Erlang socket.
 * If the message was a TICK it is immediately
 * answered. Returns: ERL_ERROR, ERL_TICK or
 * the number of bytes read.
 */
int erl_receive (int s, unsigned char *bufp, int bufsize) 
{
    return ei_receive(s, bufp, bufsize);
}

/* 
 * Send an Erlang message to a registered process
 * at the Erlang node, connected with a socket.
 */
int erl_reg_send(int fd, char *server_name, ETERM *msg)
{
    ei_x_buff x;
    int r;

    ei_x_new_with_version(&x);
    if (ei_x_encode_term(&x, msg) < 0) {
	erl_errno = EINVAL;
	r = 0;
    } else {
	r = ei_reg_send(&ec, fd, server_name, x.buff, x.index);
    }
    ei_x_free(&x);
    return r == 0;
}

/* 
 * Sends an Erlang message to a process at an Erlang node
 */
int erl_send(int fd, ETERM *to ,ETERM *msg)
{
    erlang_pid topid;
    ei_x_buff x;
    int r;

    ei_x_new_with_version(&x);
    ei_x_encode_term(&x, msg);
    /* make the to-pid */
    if (!ERL_IS_PID(to)) {
	erl_errno = EINVAL;
	return -1;
    }
    
    strcpy(topid.node, ERL_PID_NODE(to));
    topid.num = ERL_PID_NUMBER(to);
    topid.serial = ERL_PID_SERIAL(to);
    topid.creation = ERL_PID_CREATION(to);
    r = ei_send(fd, &topid, x.buff, x.index);
    ei_x_free(&x);
    return r == 0;
}

int erl_do_receive_msg(int fd, ei_x_buff* x, ErlMessage* emsg)
{
    erlang_msg msg;

    int r;
    msg.from.node[0] = msg.to.node[0] = '\0';
    r = ei_do_receive_msg(fd, 0, &msg, x);

    if (r == ERL_MSG) {
	int index = 0;
	emsg->type = msg.msgtype;

	/*
	  We can't call ei_decode_term for cases where there are no
	  data following the type information. If there are other
	  types added later where there are data this case has to be
	  extended.
	*/

	switch (msg.msgtype) {
	case ERL_SEND:
	case ERL_REG_SEND:
	  if (ei_decode_term(x->buff, &index, &emsg->msg) < 0)
	    r = ERL_ERROR;
	  break;
	default:
	  emsg->msg = NULL;	/* Not needed but may avoid problems for unsafe caller  */
	  break;
	}
    } else
	emsg->msg = NULL;
    if (msg.from.node[0] != '\0')
	emsg->from = erl_mk_pid(msg.from.node, msg.from.num, msg.from.serial, msg.from.creation);
    if (msg.to.node[0] != '\0')
	emsg->to = erl_mk_pid(msg.to.node, msg.to.num, msg.to.serial, msg.to.creation);
    return r;
}

int erl_receive_msg (int fd, unsigned char *buf, int bufsize, ErlMessage *emsg)
{
    ei_x_buff x;
    int r;

    ei_x_new(&x);
    r = erl_do_receive_msg(fd, &x, emsg);
    if (bufsize > x.index)
	bufsize = x.index;
    memcpy(buf, x.buff, bufsize);
    ei_x_free(&x);
    return r;
}

int erl_xreceive_msg (int fd, unsigned char **buf, int *bufsize,
		  ErlMessage *emsg)
{
    ei_x_buff x;
    int r;

    ei_x_new(&x);
    r = erl_do_receive_msg(fd, &x, emsg);
    if (*bufsize < x.index)
	*buf = realloc(*buf, x.index);
    *bufsize = x.index;
    memcpy(*buf, x.buff, *bufsize);
    ei_x_free(&x);
    return r;
}

/* 
 * The RPC consists of two parts, send and receive.
 * Here is the send part ! 
 * { PidFrom, { call, Mod, Fun, Args, user }} 
 */
/*
 * Now returns non-negative number for success, negative for failure.
 */
int erl_rpc_to (int fd, char *mod, char *fun, ETERM *args)
{
    int r;
    ei_x_buff x;

    ei_x_new(&x);
    ei_x_encode_term(&x, args);
    r = ei_rpc_to(&ec, fd, mod, fun, x.buff, x.index);
    ei_x_free(&x);
    return r;
} /* rpc_to */

  /*
  * And here is the rpc receiving part. A negative
  * timeout means 'infinity'. Returns either of: ERL_MSG,
  * ERL_TICK, ERL_ERROR or ERL_TIMEOUT.
*/
int erl_rpc_from (int fd, int timeout, ErlMessage *emsg) 
{
    fd_set readmask;
    struct timeval tv;
    struct timeval *t = NULL;
    unsigned char rbuf[MAX_RECEIVE_BUF];    
    
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
	if (FD_ISSET(fd, &readmask)) 
	    return erl_receive_msg(fd, rbuf, MAX_RECEIVE_BUF, emsg);
	else {
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
ETERM *erl_rpc(int fd, char *mod, char *fun, ETERM *args)
{
    int i;
    ETERM *ep;
    ErlMessage emsg;
    
    if (erl_rpc_to(fd, mod, fun, args) < 0) {
	return NULL; }
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

int erl_publish(int port)
{
    return erl_epmd_publish(port, erl_thisalivename());
} 

