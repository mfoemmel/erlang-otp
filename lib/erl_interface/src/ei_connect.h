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

/* some send() functions use buffer on heap for "small" messages */
/* messages larger than this require call to malloc() */

#ifndef EI_CONNECT_H
#define EI_CONNECT_H

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
#include <ioLib.h>

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


#ifdef HAVE_CONFIG_H
# include "config.h"	      /* FIXME: Autoconf Info prefers <config.h> */
#endif

#include "ei.h"

#define SMALLBUF 2048

#define DEFBUF_SIZ 100

typedef unsigned short  uint16;

#ifndef MAX_COOKIE_SIZE
# define MAX_COOKIE_SIZE 512
#endif

/* rpc_from() uses a buffer this size */
#ifndef MAX_RECEIVE_BUF
# define MAX_RECEIVE_BUF 32*1024
#endif

/* Distribution capability flags */
#define DFLAG_PUBLISHED           1
#define DFLAG_ATOM_CACHE          2
#define DFLAG_EXTENDED_REFERENCES 4
#define DFLAG_DIST_MONITOR        8
#define DFLAG_FUN_TAGS            16

#ifndef ERL_ERROR	/* these are also defined in erl_connect */
# define ERL_ERROR -1           /* Error of some kind */
# define ERL_NO_DAEMON -2       /* No contact with EPMD */
# define ERL_NO_PORT -3         /* No port received from EPMD */   
# define ERL_CONNECT_FAIL -4    /* Connect to Erlang Node failed */
# define ERL_TIMEOUT -5         /* A timeout has expired */
# define ERL_NO_REMOTE -6       /* Cannot execute rsh */

# define ERL_TICK 0    
# define ERL_MSG 1    

# define ERL_NO_TIMEOUT -1
#endif

/* these are also defined in erl_connect.h */
#ifndef MAXHOSTLEN
# define MAXHOSTLEN 255
#endif
#ifndef MAXALIVELEN
# define MAXALIVELEN 63
#endif
#define MAXNODELEN MAXHOSTLEN+1+MAXALIVELEN+1

#ifndef MAXREGLEN 
# define MAXREGLEN 255  /* max length of registered (atom) name */
#endif

typedef struct ei_cnode_s {
    char thishostname[MAXHOSTLEN+1];
    char thisnodename[MAXHOSTLEN+1+MAXALIVELEN+1];
    char thisalivename[MAXALIVELEN+1];
    struct in_addr this_ipaddr;             /* stored in network byte order */
    char ei_connect_cookie[MAX_COOKIE_SIZE+1];
    short creation;
    erlang_pid self;
    int init_done;
} ei_cnode;

#ifndef _ERL_CONNECT_H /* these are also defined in erl_connect.h */
typedef struct in_addr *Erl_IpAddr; 
typedef struct {
  char ipadr[4];             /* stored in network byte order */
  char nodename[MAXNODELEN+1];
} ErlConnect;
#endif

int ei_connect_init(ei_cnode* ec, const char* this_node_name,
		    const char *cookie, short creation);
int ei_xconnect (ei_cnode* ec, Erl_IpAddr adr, char *alivename);
int ei_init(void);
int ei_send(int fd, erlang_pid* to, char* buf, int len);
int ei_send_timeout(ei_cnode* ec, int fd, erlang_pid* to,
		    char* buf, int len, int timeout);
int ei_reg_send(ei_cnode* ec, int fd, char *server_name, char* buf, int len);
int ei_reg_send_timeout(ei_cnode* ec, int fd, char *server_name,
			char* buf, int len, int timeout);
int ei_receive_msg(int fd, erlang_msg* msg, ei_x_buff* x);
int ei_xreceive_msg(int fd, erlang_msg* msg, ei_x_buff* x);
int ei_connect(ei_cnode* ec, char *nodename);
int ei_accept(ei_cnode* ec, int lfd, ErlConnect *conp);
int ei_publish(ei_cnode* ec, int port);
int ei_unpublish(ei_cnode* ec);

int ei_connect_init_ex(ei_cnode* ec, const char* this_node_name,
		       const char *cookie, short creation, int use_long_name);
int ei_receive(int fd, unsigned char *bufp, int bufsize);

const char *ei_thisalivename(const ei_cnode* ec);
const char *ei_thisnodename(const ei_cnode* ec);
const char *ei_thishostname(const ei_cnode* ec);

const char* ei_getfdcookie(int fd);

int ei_do_receive_msg(int fd, int staticbuffer_p, 
		      erlang_msg* msg, ei_x_buff* x);

int ei_distversion(int fd);
int ei_rpc_to(ei_cnode* ec, int fd, char *mod, char *fun,
	      const char* buf, int len);
int ei_rpc_from(ei_cnode* ec, int fd, int timeout, erlang_msg* msg,
		ei_x_buff* x);
int ei_rpc(ei_cnode* ec, int fd, char *mod, char *fun,
	    const char* inbuf, int inbuflen, ei_x_buff* x);

#endif
