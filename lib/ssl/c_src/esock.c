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
 * Purpose:  Implementation of Secure Socket Layer (SSL).
 *
 * This is an "SSL proxy" for Erlang. The implementation has borrowed 
 * somewhat from the original implementation of `socket' by Claes Wikström,
 * and a former implementation of `ssl_socket' by Helen Ariyan. 
 *
 * About closing: We close a file descriptor that Erlang know about only
 * after we have got a close message from Erlang. If we have not got a close
 * message from Erlang at the point in time when all has been cleaned-up
 * for a connection, we send a FROMNET_CLOSE, otherwise not.
 *
 * XXX There are many uneccessary `break;'s in the code.
 *
 */

#ifdef __WIN32__
#include <winsock2.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include <sys/types.h>
#include <errno.h>

#ifdef __WIN32__
#include <process.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/time.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <fcntl.h>
#endif

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff  /* Should be in <netinet/in.h>.  */
#endif

#include "esock.h"
#include "debuglog.h"
#include "esock_utils.h"
#include "esock_ssl.h"
#include "esock_osio.h"
#include "esock_posix_str.h"

#define MAJOR_VERSION   2
#define MINOR_VERSION   0
#define MAXCOMMAND	256
#define RWBUFLEN	4096
#define IS_CLIENT       0
#define IS_SERVER       1
#define SELECT_TIMEOUT  2	/* seconds */

#define errstr()	esock_posix_str(sock_errno())
#define ssl_errstr()	esock_ssl_err_str

#define ENDPOINT_VALID(p)  	(!(p)->eof && !(p)->bp)
#define WQ_NONEMPTY(p) 		((p)->wq.len > 0)

#define PROXY_TO_SSL_VALID(cp) (ENDPOINT_VALID(cp) && \
				(WQ_NONEMPTY(cp) || ENDPOINT_VALID(cp->proxy)))

#define SSL_TO_PROXY_VALID(cp) (ENDPOINT_VALID(cp->proxy) && \
				(WQ_NONEMPTY(cp->proxy) || ENDPOINT_VALID(cp)))

#define JOINED_STATE_VALID(cp) (PROXY_TO_SSL_VALID(cp) && \
				SSL_TO_PROXY_VALID(cp))

static int read_loop(void);
static void try_ssl_closing(Connection *cp);
static void do_ssl_closing(Connection *cp);
static int reply(int cmd, char *fmt, ...);
static int input(char *fmt, ...);
static int put_pars(unsigned char *buf, char *fmt, va_list args);
static int get_pars(unsigned char *buf, char *fmt, va_list args);
static FD do_connect(char *ipstring, int port);
static FD do_listen(char *ipstring, int lport, int backlog, int *aport);
static void print_connections(void);
static void safe_close(FD fd);
static Connection *new_connection(int state, FD fd);
static void remove_connection(FD fd);
static Proxy *get_proxy_by_peerport(int port);
static Proxy *new_proxy(FD fd);
static void remove_proxy(FD fd);
static void ensure_write_queue(WriteQueue *wq, int size);
static void clean_up(void);

static Connection  *connections = NULL;
static fd_set readmask, writemask, exceptmask;
static Proxy *proxies = NULL;
static int proxy_listensock = INVALID_FD;
static int proxy_listenport = 0;
static int proxy_backlog = 5;
static char rwbuf[RWBUFLEN];
static unsigned char *ebuf = NULL; /* Set by read_ctrl() */
static unsigned long one = 1;
static unsigned char command[MAXCOMMAND];
static struct timeval timeout = {SELECT_TIMEOUT, 0};

static char *connstr[] = {
    "STATE_NONE", 
    "ACTIVE_LISTENING",
    "PASSIVE_LISTENING",
    "CONNECTED",
    "WAIT_CONNECT",
    "SSL_CONNECT",
    "SSL_ACCEPT",
    "JOINED",
    "SSL_CLOSING"
};

int main(int argc, char **argv) 
{
    char *logfile = NULL;
    int i;
#ifdef __WIN32__
    int pid;
    WORD version;
    WSADATA wsa_data;

    set_binary_mode();
    setvbuf(stderr, NULL, _IONBF, 0);
#else
    pid_t pid;
#endif

    pid = getpid();
    i = 1;
    while (i < argc) {
	if (strcmp(argv[i], "-d") == 0) {
	    debug = 1;
	    i++;
	} else if (strcmp(argv[i], "-dm") == 0) {
	    debugmsg = 1;
	    i++;
	} else if (strcmp(argv[i], "-pp") == 0) {
	    i++;
	    proxy_listenport = atoi(argv[i]);
	    i++;
	} else if (strcmp(argv[i], "-pb") == 0) {
	    i++;
	    proxy_backlog = atoi(argv[i]);
	    i++;
	} else if (strcmp(argv[i], "-dd") == 0) {
	    i++;
	    logfile = esock_malloc(strlen(argv[i]) + 64);
	    sprintf(logfile, "%s/ssl_esock.%d.log", argv[i], (int)pid);
	    i++;
	}
    }
    if (debug || debugmsg) {
	DEBUGF(("Starting ssl_esock\n"));
	if (logfile) open_syslog(logfile);
	atexit(close_syslog);
	DEBUGF(("pid = %d\n", getpid()));
    }
    if (esock_ssl_init() < 0)
	exit(EXIT_FAILURE);
    atexit(esock_ssl_finish);

#ifdef __WIN32__
    /* Start Windows' sockets */
    version = MAKEWORD(MAJOR_VERSION, MINOR_VERSION);
    if (WSAStartup(version, &wsa_data) != 0) {
	fprintf(stderr, "Could not start-up Windows' sockets\n");
	exit(EXIT_FAILURE);
    }
    atexit((void (*)(void))WSACleanup);
    if (LOBYTE(wsa_data.wVersion) < MAJOR_VERSION ||
	(LOBYTE(wsa_data.wVersion) == MAJOR_VERSION && 
	 HIBYTE(wsa_data.wVersion) < MINOR_VERSION)) {
	fprintf(stderr, "Windows socket version error. Requested version:"
		"%d.%d, version found: %d.%d\n", MAJOR_VERSION, 
		MINOR_VERSION, LOBYTE(wsa_data.wVersion), 
		HIBYTE(wsa_data.wVersion));
	exit(EXIT_FAILURE);
    }
    DEBUGF(("Using Windows socket version: %d.%d\n", 
	   LOBYTE(wsa_data.wVersion), HIBYTE(wsa_data.wVersion)));
    DEBUGF(("Maximum number of sockets available: %d\n", 
	    wsa_data.iMaxSockets));
 
    if (esock_osio_init() < 0) {
	fprintf(stderr, "Could not init osio\n");
	exit(EXIT_FAILURE);
    }
    atexit(esock_osio_finish);
#endif

    /* Create the local proxy listen socket and set it to non-blocking */
    /* XXX Check backlog */
    proxy_listensock = do_listen("127.0.0.1", proxy_listenport, 
				 proxy_backlog, &proxy_listenport);
    if (proxy_listensock == INVALID_FD) {
	DEBUGF(("Cannot create local listen socket\n"));
	exit(EXIT_FAILURE);
    }
    SET_NONBLOCKING(proxy_listensock);
    DEBUGF(("Local proxy listen socket: fd = %d, port = %d\n", 
	   proxy_listensock, proxy_listenport));

    /* Report the port number of the local proxy listen socket */
    reply(ESOCK_PROXY_PORT, "24", proxy_listenport, (int)pid);

    atexit(clean_up);

    read_loop();

    esock_free(logfile);
    exit(EXIT_SUCCESS);
}


Connection *get_connection(FD fd)
{
    Connection *cp = connections;
    
    while(cp) {
	if(cp->fd == fd)
	    return cp;
	cp = cp->next;
    }
    return NULL;
}

/*
 * Local functions
 *
 */

static int read_loop(void)
{
    FD fd, msgsock, listensock, connectsock, proxysock;
    int cc, wc, cport, lport, pport, length, backlog, intref, op;
    char value;
    char *ipstring;
    char *flags;
    struct sockaddr_in iserv_addr;
    int sret = 1, j;
    Connection *cp, *cpnext, *newcp;
    Proxy *pp;
    struct timeval tv;
    time_t last_time = 0, now = 0;
    int set_wq_fds;

    while(1) {

	FD_ZERO(&readmask);
	FD_ZERO(&writemask);
	FD_ZERO(&exceptmask);
	FD_SET(local_read_fd, &readmask);
	FD_SET(proxy_listensock, &readmask);
	tv = timeout;		/* select() might change tv */
	set_wq_fds = 0;

	cc = esock_ssl_set_masks(connections, &readmask, &writemask, 
				 &exceptmask, sret) + 1;

	if (sret) {		/* sret == 1 the first time. */
	    DEBUGF(("__________READ-LOOP_____________\n"));
	    print_connections();
	    DEBUGF(("server before select cc = %d \n", cc));
	}
	sret = select(FD_SETSIZE, &readmask, &writemask, &exceptmask, &tv);

	if (sret < 0) {
	    DEBUGF(("select error: %s\n", errstr()));
	    continue;
	} else if (sret == 0) {
	    FD_ZERO(&readmask);
	    FD_ZERO(&writemask);
	    FD_ZERO(&exceptmask);
	}

	if (sret) {
	    DEBUGF(("server after select: %d descriptor%s: ", sret, 
		    (sret == 1) ? "" : "s"));
#ifndef __WIN32__
	    for (j = 0; j < FD_SETSIZE; j++) {
		if (FD_ISSET(j, &readmask) || 
		    FD_ISSET(j, &writemask) ||
		    FD_ISSET(j, &exceptmask))
		    DEBUGF(("%d ", j));
	    }
#else
	    /* XXX Make this better */
	    DEBUGF(("(not shown)"));
#endif
	    DEBUGF(("\n"));
	}
	
	time(&now);
	if (now >= last_time + SELECT_TIMEOUT) {
	    set_wq_fds = 1;
	    last_time = now;
	}
	/*
	 * First accept as many connections as possible on the
	 * proxy listen socket. We record the peer port, which
	 * is later used as a reference for joining a proxy 
	 * connection with a network connection.
	 */
	if (FD_ISSET(proxy_listensock, &readmask)) {
	    while (1) {
		length = sizeof(iserv_addr);
		proxysock = accept(proxy_listensock, 
				   (struct sockaddr *)&iserv_addr, 
				   (int*)&length);
		if(proxysock == INVALID_FD) {
		    if (sock_errno() != ERRNO_BLOCK) {
			DEBUGF(("accept error (proxy_listensock): %s\n", 
				errstr()));
		    }
		    break;
		} else {
		    /* Get peer port number */
		    length = sizeof(iserv_addr);
		    if (getpeername(proxysock, (struct sockaddr *)&iserv_addr, 
				    &length) < 0) {
			DEBUGF(("Can't get peername of proxy socket"));
			safe_close(proxysock);
		    } else {
			/* Add to pending proxy connections */
			pp = new_proxy(proxysock);
			pp->peer_port = ntohs(iserv_addr.sin_port);
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("[PROXY_LISTEN_SOCK] conn accepted: fd = %d, "
			       "peer port = %d\n", proxysock, pp->peer_port));
		    }
		}
	    }
	}

	/* 
	 * Read control messages from Erlang
	 */
	if (FD_ISSET(local_read_fd, &readmask)) {  

	    cc = read_ctrl(&ebuf);
	    if ( cc < 0 ) {
		DEBUGF(("Read loop -1 or 0\n"));
		return -1;
	    } else if (cc == 0) {  /* not eof  */
		DEBUGF(("GOT empty string \n"));

	    } else {

		switch((int)*ebuf) {

		case ESOCK_GETPEERNAME:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    cp = get_connection(fd);
		    length = sizeof(iserv_addr);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETPEERNAME_ERR, "4s", fd, errstr());
		    } else if (getpeername(fd, 
					   (struct sockaddr *) &iserv_addr, 
					   &length) < 0) {
			reply(ESOCK_GETPEERNAME_ERR, "4s", fd, errstr());
		    } else {
			/*
			 * reply  = {cmd(1), fd(4), port(2), 
			 * 	    ipstring(N), 0(1)}
			 */
			reply(ESOCK_GETPEERNAME_REP, "42s", fd, 
			      ntohs(iserv_addr.sin_port), 
			      inet_ntoa(iserv_addr.sin_addr));
		    }
		    break;

		case ESOCK_GETSOCKNAME:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    cp = get_connection(fd);
		    length = sizeof(iserv_addr);
		    if (!cp) {
			sock_set_errno(ERRNO_NOTSOCK);
			reply(ESOCK_GETSOCKNAME_ERR, "4s", fd, errstr());
		    } else if (getsockname(fd, 
					   (struct sockaddr *)&iserv_addr, 
					   &length) < 0) {
			reply(ESOCK_GETSOCKNAME_ERR, "4s", fd, errstr());
		    } else {
			/*
			 * reply  = {cmd(1), fd(4), port(2), 
			 * 	    ipstring(N), 0(1)}
			 */
			reply(ESOCK_GETSOCKNAME_REP, "42s", fd, 
			      ntohs(iserv_addr.sin_port),
			      inet_ntoa(iserv_addr.sin_addr));
		    }
		    break;

		case ESOCK_CONNECT:   /* request to connect */
		    /* 
		     * ebuf = {cmd(1), intref(4), port(2), ipstring(N), 0(1), 
		     * 	       flags(N), 0(1)}
		     */
		    input("42ss", &intref, &cport, &ipstring, &flags);
		    DEBUGF(("[ERLANG_CONNECT] intref = %d, ipstring = %s "
			   "port = %d, flags = %s\n", intref, ipstring, 
			   cport, flags));
		    connectsock = do_connect(ipstring, cport);
		    if(connectsock == INVALID_FD) {
			reply(ESOCK_CONNECT_SYNC_ERR, "4s", intref, errstr());
			break;
		    }
		    DEBUGF(("  fd = %d\n", connectsock));
		    cp = new_connection(ESOCK_WAIT_CONNECT, connectsock);
		    cp->origin = "connect";
		    length = strlen(flags);
		    cp->flags = esock_malloc(length + 1);
		    strcpy(cp->flags, flags);
		    DEBUGF(("-> WAIT_CONNECT fd = %d\n", connectsock));
		    reply(ESOCK_CONNECT_WAIT, "44", intref, connectsock);
		    break;
		    
		case ESOCK_TERMINATE:
		    /* 
		     * ebuf = {cmd(1)}
		     */
		    exit(EXIT_SUCCESS);
		    break;

		case ESOCK_CLOSE:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    if ((cp = get_connection(fd))) {
			DEBUGF(("%s[ERLANG_CLOSE]: fd = %d\n", 
			       connstr[cp->state], fd));
			switch (cp->state) {
			case ESOCK_JOINED:
			case ESOCK_SSL_CLOSING:
			    cp->close = 1;
			    break;
			default:
			    remove_connection(fd);
			    safe_close(fd);
			}
		    } else {
			DEBUGF(("[ERLANG_CLOSE]: (no cp found)\n"));
			safe_close(fd);
		    }
		    break;

		case ESOCK_SET_SOCK_OPT:
		    /* 
		     * ebuf = {cmd(1), fd(4), op(1), on(1)}
		     */
		    input("411", &fd, &op, &value);
		    switch(op) {
		    case ESOCK_SET_TCP_NODELAY:
			if(setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, 
				      &value, sizeof(value)) < 0) {
			    DEBUGF(("Error: setsockopt TCP_NODELAY\n"));
			    reply(ESOCK_IOCTL_ERR, "4s", fd, errstr());
			} else {
			    reply(ESOCK_IOCTL_OK, "4", fd);
			}
			break;
		    default:
			DEBUGF(("Error: set_sock_opt - Not implemented\n"));
			sock_set_errno(ERRNO_OPNOTSUPP);
			reply(ESOCK_IOCTL_ERR, "4", fd, errstr());
			break;
		    }
		    break;

		case ESOCK_LISTEN:
		    /* 
		     * ebuf = {cmd(1), intref(4), lport(2), ipstring(N), 0(1),
		     * 	       backlog(2), flags(N), 0(1)}
		     */
		    input("42s2s", &intref, &lport, &ipstring, &backlog,
			  &flags);
		    DEBUGF(("[ERLANG_LISTEN] intref = %d, port = %d, "
			   "ipstring = %s, backlog = %d, flags = %s\n", 
			   intref, lport, ipstring, backlog, flags));
		    
		    listensock = do_listen(ipstring, lport, backlog, &lport);
		    if(listensock == INVALID_FD) {
			reply(ESOCK_LISTEN_SYNC_ERR, "4s", intref, errstr());
			break;
		    }
		    cp = new_connection(ESOCK_PASSIVE_LISTENING, listensock);
		    /* Flags may be an empty string */
		    if ((length = strlen(flags)) > 0) {
			cp->flags = esock_malloc(length + 1);
			strcpy(cp->flags, flags);
		    }
		    if (esock_ssl_listen_init(cp) < 0) {
			DEBUGF(("esock_ssl_listen_init() failed.\n"));
			reply(ESOCK_LISTEN_SYNC_ERR, "4s", intref, 
			      ssl_errstr());
			break;
		    }
		    DEBUGF(("-> PASSIVE_LISTENING (fd = %d)\n", listensock));
		    reply(ESOCK_LISTEN_REP, "442", intref, listensock,
			  ntohs(iserv_addr.sin_port));
		    break;

		case ESOCK_ACCEPT:
		    /* 
		     * ebuf =  { op(1), fd(4), flags(N), 0(1)} 
		     */
		    input("4s", &fd, &flags);
		    DEBUGF(("[ERLANG_ACCEPT] fd = %d, flags = %s\n", fd, 
			   flags));
		    cp = get_connection(fd);
		    if (cp) {
			 /* We store the flags in the listen socket's 
			  * connection, and overwrite previous flags.
			  */
			if ((length = strlen(flags)) > 0) {
			    if (cp->flags)
				cp->flags = esock_realloc(cp->flags, 
							  length + 1);
			    else
				cp->flags = esock_malloc(length + 1);
			    strcpy(cp->flags, flags);
			}
			if (cp->flags && cp->flags[0] != '\0') {
			    cp->acceptors++;
			    cp->state = ESOCK_ACTIVE_LISTENING; 
			    DEBUGF(("-> ACTIVE_LISTENING\n"));
			    break;
			}
			DEBUGF(("ERROR: flags empty\n"));
		    }
		    reply(ESOCK_ACCEPT_ERR, "4s", fd, "ebadf");
		    break;

		case ESOCK_NOACCEPT:
		    /* 
		     * ebuf = {cmd(1), fd(4)}
		     */
		    input("4", &fd);
		    DEBUGF(("[ERLANG_NOACCEPT] fd = %d\n", fd));
		    cp = get_connection(fd);
		    if (cp && (--cp->acceptors <= 0)) {
			cp->acceptors = 0;
			cp->state = ESOCK_PASSIVE_LISTENING;
			DEBUGF(("-> PASSIVE_LISTENING\n"));
		    }
		    break;

		case ESOCK_PROXY_JOIN:
		    /*
		     * ebuf = {cmd(1), fd(4), portnum(2)}
		     *
		     * fd      - file descriptor of a connection in state
		     *           CONNECTED
		     * portnum - port number of the Erlang proxy peer 
		     */
		    input("42", &fd, &pport);
		    DEBUGF(("CONNECTED[ERLANG_PROXY_JOIN] fd = %d "
			   "portnum = %d\n", fd, pport));
		    cp = get_connection(fd);
		    pp = get_proxy_by_peerport(pport);
		    if (cp && cp->state == ESOCK_CONNECTED && pp) {
			cp->proxy = pp;
			pp->conn = cp;
			reply(ESOCK_PROXY_JOIN_REP, "4", fd);
			cp->state = ESOCK_JOINED;
			DEBUGF(("-> JOINED\n"));
			break;
		    }
		    if (!cp) {
			DEBUGF(("ERROR: No connection with fd = %d\n", fd));
			reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, "ebadsocket");
		    } else if (cp->state != ESOCK_CONNECTED) {
			DEBUGF(("ERROR: Bad state: fd = %d\n", fd));
			reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, "ebadstate");
		    } else {
			DEBUGF(("ERROR: No proxy: fd = %d, pport = %d\n",
			       fd, pport));
			reply(ESOCK_PROXY_JOIN_ERR, "4s", fd, 
			      "enoproxysocket");
		    }
		    break;

		default:
		    DEBUGF(("default value in read_loop %c\n", *ebuf));
		    exit(EXIT_FAILURE);
		    break;
		    
		}
	    }
	}
	/* not from erlang */

	/* Note: We may remove the current connection (cp). Thus we must
	 * be careful not to read cp->next after cp has been removed.
	 */
	for (cp = esock_ssl_read_masks(connections, &cpnext, &readmask, 
				       &writemask, &exceptmask, set_wq_fds); 
	     cp != NULL; 
	     cp = esock_ssl_read_masks(cpnext, &cpnext, &readmask, 
				       &writemask, &exceptmask, set_wq_fds)
	    ) {

	    switch(cp->state) {
		
	    case ESOCK_ACTIVE_LISTENING:
		/* new connect from network */
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("ACTIVE_LISTENING - trying to accept on %d\n", 
		       cp->fd));
		length = sizeof(iserv_addr);
		msgsock = accept(cp->fd, (struct sockaddr*)&iserv_addr, 
				 (int*)&length);
		if(msgsock == INVALID_FD)  {
		    DEBUGF(("accept error: %s\n", errstr()));
		    reply(ESOCK_ACCEPT_ERR, "4s", cp->fd, errstr());
		    break;
		}
		SET_NONBLOCKING(msgsock);
		if (--cp->acceptors <= 0) {
		    cp->acceptors = 0;
		    cp->state = ESOCK_PASSIVE_LISTENING;
		    DEBUGF(("-> PASSIVE_LISTENING\n"));
		}
		DEBUGF(("server accepted connection on fd %d\n", msgsock));
		newcp = new_connection(ESOCK_SSL_ACCEPT, msgsock);
		newcp->origin = "accept";
		DEBUGF(("(new) -> SSL_ACCEPT on fd %d\n", msgsock));
		newcp->listen_fd = cp->fd; /* Needed for ESOCK_ACCEPT_ERR  */
		length = strlen(cp->flags);
		newcp->flags = esock_malloc(length + 1);
		strcpy(newcp->flags, cp->flags); /* XXX Why? */
		if (esock_ssl_accept_init(newcp) < 0) {
		    /* N.B.: The *listen fd* is reported. */
		    reply(ESOCK_ACCEPT_ERR, "4s", newcp->listen_fd,
			  ssl_errstr());
		    remove_connection(msgsock);
		    safe_close(msgsock);
		    break;
		}
		newcp->ssl_want = ESOCK_SSL_WANT_READ;
		break;

	    case ESOCK_PASSIVE_LISTENING:
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("Got connect request while PASSIVE\n"));
		exit(EXIT_FAILURE);
		break;

	    case ESOCK_JOINED:
		/* 
		 * Proxy to SSL 
		 */
		if (FD_ISSET(cp->fd, &writemask)) {
		    /* If there is a write queue, write to ssl only */
		    if (cp->wq.len > 0) { 
			/* The write retry semantics of SSL_write in
			 * the SSLeay package is strange. Partial
			 * writes never occur, only complete writes or
			 * failures.  A failure, however, still
			 * consumes all data written, although not all
			 * encrypted data could be written to the
			 * underlying socket. To retry a write we have
			 * to provide the same buf and length as in
			 * the original call, in our case rwbuf and
			 * the original buffer length. Hence the
			 * strange memcpy(). Note that wq.offset will
			 * always be zero when we use SSLeay.  
			 */
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("JOINED: writing to ssl "
				"fd = %d, from write queue only, wc = %d\n", 
				cp->fd, cp->wq.len - cp->wq.offset));
			memcpy(rwbuf, cp->wq.buf, cp->wq.len - cp->wq.offset);
			wc = esock_ssl_write(cp, rwbuf, 
					     cp->wq.len - cp->wq.offset);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken pipe */
				cp->bp = 1;
				DEBUGF(("broken SSL pipe\n"));
				if (!JOINED_STATE_VALID(cp)) {
				    try_ssl_closing(cp);
				    break;
				}
			    }
			} else {
			    cp->wq.offset += wc;
			    if (cp->wq.offset == cp->wq.len)
				cp->wq.len = 0;
			}
		    }
		} else if (FD_ISSET(cp->proxy->fd, &readmask)) {
		    /* Read from proxy and write to SSL */
		    DEBUGF(("-----------------------------------\n"));
		    DEBUGF(("JOINED: reading from proxy, "
			   "fd = %d\n", cp->proxy->fd));
		    cc = sock_read(cp->proxy->fd, rwbuf, RWBUFLEN); 
		    DEBUGF(("read from proxy fd = %d, cc = %d\n", 
			   cp->proxy->fd, cc));
		    if (cc > 0) {
			wc = esock_ssl_write(cp, rwbuf, cc);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
			    /* Assume broken pipe */
				cp->bp = 1;
				DEBUGF(("broken SSL pipe\n"));
				if (!JOINED_STATE_VALID(cp)) {
				    try_ssl_closing(cp);
				    break;
				}
			    } else {
				/* add to write queue */
				DEBUGF(("adding all to write queue "
					"%d bytes\n", cc));
				ensure_write_queue(&cp->wq, cc);
				memcpy(cp->wq.buf, rwbuf, cc);
				cp->wq.len = cc;
				cp->wq.offset = 0;
			    }
			} else if (wc < cc) {
			    /* add to write queue */
			    DEBUGF(("adding remainder to write queue "
				    "%d bytes\n", cc - wc));
			    ensure_write_queue(&cp->wq, cc - wc);
			    memcpy(cp->wq.buf, rwbuf + wc, cc - wc);
			    cp->wq.len = cc - wc;
			    cp->wq.offset = 0;
			} 
		    } else if (cc == 0) {
			/* EOF proxy */
			DEBUGF(("proxy eof\n"));
			cp->proxy->eof = 1;
			if (!JOINED_STATE_VALID(cp)) {
			    try_ssl_closing(cp);
			    break;
			}
		    } else {
			/* This should not happen */
			DEBUGF(("ERROR: proxy readmask set, cc < 0,  fd = %d"
			       " proxyfd = %d\n", cp->fd, cp->proxy->fd));
		    }
		}
		/* 
		 * SSL to proxy 
		 */
		if (FD_ISSET(cp->proxy->fd, &writemask)) {
		    /* If there is a write queue, write to proxy only */
		    if (cp->proxy->wq.len > 0) {
			DEBUGF(("-----------------------------------\n"));
			DEBUGF(("JOINED: writing to proxy "
				"fd = %d, from write queue only, wc = %d\n", 
				cp->proxy->fd, cp->proxy->wq.len - 
				cp->proxy->wq.offset));
			wc = sock_write(cp->proxy->fd, cp->proxy->wq.buf + 
					cp->proxy->wq.offset,
					cp->proxy->wq.len - 
					cp->proxy->wq.offset);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken pipe */
				cp->proxy->bp = 1;
				DEBUGF(("broken proxy pipe\n"));
				if (!JOINED_STATE_VALID(cp)) {
				    try_ssl_closing(cp);
				    break;
				}
			    }
			} else {
			    cp->proxy->wq.offset += wc;
			    if (cp->proxy->wq.offset == cp->proxy->wq.len)
				cp->proxy->wq.len = 0;
			}
		    }
		} else if (FD_ISSET(cp->fd, &readmask)) {
		    /* Read from SSL and write to proxy */
		    DEBUGF(("-----------------------------------\n"));
		    DEBUGF(("JOINED: read from ssl fd = %d\n",
			   cp->fd));
		    cc = esock_ssl_read(cp, rwbuf, RWBUFLEN);
		    DEBUGF(("read from fd = %d, cc = %d\n", cp->fd, cc));
		    if (cc > 0) {
			wc = sock_write(cp->proxy->fd, rwbuf, cc);
			if (wc < 0) {
			    if (sock_errno() != ERRNO_BLOCK) {
				/* Assume broken pipe */
				cp->proxy->bp = 1;
				DEBUGF(("broken proxy pipe\n"));
				if (!JOINED_STATE_VALID(cp)) {
				    try_ssl_closing(cp);
				    break;
				}
			    } else {
				/* add to write queue */
				DEBUGF(("adding to write queue %d bytes\n", 
					cc));
				ensure_write_queue(&cp->proxy->wq, cc);
				memcpy(cp->proxy->wq.buf, rwbuf, cc);
				cp->proxy->wq.len = cc;
				cp->proxy->wq.offset = 0;
			    }
			} else if (wc < cc) {
			    /* add to write queue */
			    DEBUGF(("adding to write queue %d bytes\n",
				    cc - wc));
			    ensure_write_queue(&cp->proxy->wq, cc - wc);
			    memcpy(cp->proxy->wq.buf, rwbuf + wc, cc - wc);
			    cp->proxy->wq.len = cc - wc;
			    cp->proxy->wq.offset = 0;
			} 
		    } else if (cc == 0) {
			/* SSL eof */
			DEBUGF(("SSL eof\n"));
			cp->eof = 1;
			if (!JOINED_STATE_VALID(cp)) {
			    try_ssl_closing(cp);
			    break;
			}
		    } else {
			/* This may very well happen when reading from SSL. */
			DEBUGF(("NOTE: readmask set, cc < 0,  fd = %d, "
				"is ok\n", cp->fd));
		    }
		}
		break;

	    case ESOCK_WAIT_CONNECT:
		/* New connection attempt */
		connectsock = cp->fd;
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("WAIT_CONNECT fd = %d\n", connectsock));

		/* If the connection did succeed it's possible to fetch
		 * the peer name (UNIX); or Failure shows in exceptmask
		 * (WIN32). Sorry for the mess, but we have to have 
		 * balanced paren's in #ifdefs in order not to confuse
		 * Emacs' indentation.
		 */
		length = sizeof(iserv_addr);
		if (
#ifdef __WIN32__
		    FD_ISSET(connectsock, &exceptmask)
#else
 		    getpeername(connectsock, (struct sockaddr *)&iserv_addr, 
				&length) < 0 
#endif
		    ) {
		    DEBUGF(("connect error: %s\n", errstr()));
		    reply(ESOCK_CONNECT_ERR, "4s", connectsock, errstr());
		    remove_connection(connectsock);
		    break;
		}
		if (esock_ssl_connect_init(cp) < 0) {
		    DEBUGF(("esock_ssl_connect_init() failed\n"));
		    reply(ESOCK_CONNECT_ERR, "4s", connectsock, ssl_errstr());
		    remove_connection(connectsock);
		    break;
		}
		DEBUGF(("-> SSL_CONNECT\n"));
		cp->state = ESOCK_SSL_CONNECT;
		cp->ssl_want = ESOCK_SSL_WANT_WRITE;
		break;

	    case ESOCK_SSL_CONNECT:
		connectsock = cp->fd;
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_CONNECT fd = %d\n", connectsock));
		if (esock_ssl_connect(cp) < 0) {
		    if (sock_errno() != ERRNO_BLOCK) {
			/* Handshake failed */
			reply(ESOCK_CONNECT_ERR, "4s", connectsock,
			      ssl_errstr());
			DEBUGF(("ERROR: handshake: %s\n", ssl_errstr()));
			remove_connection(connectsock);
		    }
		} else {
		    /* SSL handshake successful */
		    reply(ESOCK_CONNECT_REP, "4", connectsock);
		    DEBUGF(("-> CONNECTED\n"));
		    cp->state = ESOCK_CONNECTED;
		}
		break;

	    case ESOCK_SSL_ACCEPT:
		/* Erlang does not know of msgsock yet */
		msgsock = cp->fd;
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_ACCEPT fd = %d\n", msgsock));
		if (esock_ssl_accept(cp) < 0) {
		    if (sock_errno() != ERRNO_BLOCK) {
			/* Handshake failed. */
			reply(ESOCK_ACCEPT_ERR, "4s", cp->listen_fd,
			      ssl_errstr());
			DEBUGF(("ERROR: handshake: %s\n", ssl_errstr()));
			remove_connection(msgsock);
			safe_close(msgsock);
		    }
		} else {
		    /* SSL handshake successful */
		    reply(ESOCK_ACCEPT_REP, "44", cp->listen_fd, msgsock);
		    DEBUGF(("-> CONNECTED\n"));
		    cp->state = ESOCK_CONNECTED;
		}
		break;

	    case ESOCK_SSL_CLOSING:
		DEBUGF(("-----------------------------------\n"));
		DEBUGF(("SSL_CLOSING: fd = %d\n", cp->fd));
		do_ssl_closing(cp);
		break;

	    default:
		DEBUGF(("ERROR: Connection in unknown state.\n"));
	    }
	}
    }
}

static void try_ssl_closing(Connection *cp)
{
    DEBUGF(("-> SSL_CLOSING (or removal)\n"));
    cp->state = ESOCK_SSL_CLOSING;
    cp->ssl_want = ESOCK_SSL_WANT_WRITE;
    do_ssl_closing(cp);
}

static void do_ssl_closing(Connection *cp)
{
    if (esock_ssl_close(cp) < 0 && sock_errno() == ERRNO_BLOCK)
	return;
    esock_ssl_free(cp);
    if (cp->close)
	safe_close(cp->fd);
    else {
	DEBUGF(("sending FROMNET_CLOSE fd = %d\n", cp->fd));
	reply(ESOCK_FROMNET_CLOSE, "4", cp->fd);
    }
    remove_connection(cp->fd);
}

static int reply(int cmd, char *fmt, ...)
{
    va_list args;
    int len;

    PUT_INT8(cmd, command);
    va_start(args, fmt);
    len = put_pars(command + 1, fmt, args);
    va_end(args);
    write_ctrl(command, len + 1);
    return len + 1;
}

static int input(char *fmt, ...)
{
    va_list args;
    int len;

    va_start(args, fmt);
    len = get_pars(ebuf + 1, fmt, args);
    va_end(args);
    return len + 1;
}

static int put_pars(unsigned char *buf, char *fmt, va_list args)
{
    char *s, *str;
    int val, pos = 0;

    s = fmt;
    while (*s) {
	switch (*s) {
	case '1':
	    val = va_arg(args, int);
	    PUT_INT8(val, buf + pos);
	    pos++;
	    break;
	case '2':
	    val = va_arg(args, int);
	    PUT_INT16(val, buf + pos);
	    pos += 2;
	    break;
	case '4':
	    val = va_arg(args, int);
	    PUT_INT32(val, buf + pos);
	    pos += 4;
	    break;
	case 's':
	    str = va_arg(args, char *);
	    strcpy(buf + pos, str);
	    pos += strlen(str) + 1;
	    break;
	default:
	    fprintf(stderr, "Invalid format character: %c\n", *s);
	    exit(EXIT_FAILURE);
	    break;
	}
	s++;
    }
    return pos;
}


static int get_pars(unsigned char *buf, char *fmt, va_list args)
{
    int *ip;
    char *s, **strp;
    int pos = 0;

    s = fmt;
    while (*s) {
	switch (*s) {
	case '1':
	    ip = va_arg(args, int *);
	    *ip = GET_INT8(buf + pos);
	    pos++;
	    break;
	case '2':
	    ip = va_arg(args, int *);
	    *ip = GET_INT16(buf + pos);
	    pos += 2;
	    break;
	case '4':
	    ip = va_arg(args, int *);
	    *ip = GET_INT32(buf + pos);
	    pos += 4;
	    break;
	case 's':
	    strp = va_arg(args, char **);
	    *strp = buf + pos;
	    pos += strlen(*strp) + 1;
	    break;
	default:
	    fprintf(stderr, "Invalid format character: %c\n", *s);
	    exit(EXIT_FAILURE);
	    break;
	}
	s++;
    }
    return pos;
}

static FD do_connect(char *ipstring, int port)
{
    struct sockaddr_in sock_addr;
    long inaddr;
    FD fd;
   
    if((fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_FD) {
	DEBUGF(("Error calling socket()\n"));
	return fd;
    }
    DEBUGF(("  fd = %d\n", fd));
    if ((inaddr = inet_addr(ipstring)) == INADDR_NONE) {
	DEBUGF(("Error in inet_addr(): ipstring = %s\n", ipstring));
	safe_close(fd);
	sock_set_errno(ERRNO_ADDRNOTAVAIL);
	return INVALID_FD;
    }
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_addr.s_addr = inaddr;
    sock_addr.sin_port = htons(port);
    SET_NONBLOCKING(fd);
    if(connect(fd, (struct sockaddr*)&sock_addr, sizeof(sock_addr)) < 0) {
	if (sock_errno() != ERRNO_PROGRESS && /* UNIX */
	    sock_errno() != ERRNO_BLOCK) { /* WIN32 */
	    DEBUGF(("Error in connect()\n"));
	    safe_close(fd);
	    return INVALID_FD;
	}
    }
    return fd;
}

static FD do_listen(char *ipstring, int lport, int backlog, int *aport)
{
    struct sockaddr_in sock_addr;
    long inaddr;
    int length;
    FD fd;

    
    if((fd = socket(AF_INET, SOCK_STREAM, 0)) == INVALID_FD) {
	DEBUGF(("Error calling socket()\n"));
	return fd;
    }
    DEBUGF(("  fd = %d\n", fd));
    if ((inaddr = inet_addr(ipstring)) == INADDR_NONE) {
	DEBUGF(("Error in inet_addr(): ipstring = %s\n", ipstring));
	safe_close(fd);
	sock_set_errno(ERRNO_ADDRNOTAVAIL);
	return INVALID_FD;
    }
    memset(&sock_addr, 0, sizeof(sock_addr));
    sock_addr.sin_family = AF_INET;
    sock_addr.sin_addr.s_addr = inaddr;
    sock_addr.sin_port = htons(lport);

    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, (char *)&one, sizeof(one));

    if(bind(fd, (struct sockaddr*) &sock_addr, sizeof(sock_addr)) < 0) {
	DEBUGF(("Error in bind()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    if (listen(fd, backlog) < 0) {
	DEBUGF(("Error in listen()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    /* find out assigned local port number */
    length = sizeof(sock_addr);
    if (getsockname(fd, (struct sockaddr *)&sock_addr, &length) < 0) {
	DEBUGF(("Error in getsockname()\n"));
	safe_close(fd);
	return INVALID_FD;
    }
    if (aport)
	*aport = ntohs(sock_addr.sin_port);
    return fd;
}


static Connection *new_connection(int state, FD fd)
{
    Connection *cp;
    
    if (!(cp = esock_malloc(sizeof(Connection))))
	return NULL;
    cp->state = state;
    cp->acceptors = 0;
    cp->fd = fd;
    cp->listen_fd = INVALID_FD;
    cp->proxy = NULL;
    cp->opaque = NULL;
    cp->ssl_want = 0;
    cp->ssl_verify_depth = 0;
    cp->eof = 0;
    cp->bp = 0;
    cp->close = 0;
    cp->origin = "";
    cp->flags = NULL;
    cp->logfp = NULL;
    cp->wq.size = 0;
    cp->wq.buf = NULL;
    cp->wq.len = 0;
    cp->wq.offset = 0;
    cp->next = connections;
    connections = cp;
    return cp;
}


static void print_connections(void)
{
    if(debug) {
    
	Connection *cp = connections;
	DEBUGF(("CONNECTIONS:\n"));
	while (cp) {
	    if (cp->state == ESOCK_JOINED) {
		DEBUGF((" - %s [%8p] (origin = %s)\n"
			"       (fd = %d, eof = %d, wq = %d, bp = %d)\n"
			"       (proxyfd = %d, eof = %d, wq = %d, bp = %d)\n", 
		       connstr[cp->state], cp, cp->origin,
			cp->fd, cp->eof, cp->wq.len, cp->bp,
			cp->proxy->fd, cp->proxy->eof, cp->proxy->wq.len, 
			cp->proxy->bp));
	    } else if (cp->state == ESOCK_ACTIVE_LISTENING) {
		DEBUGF((" - %s [%8p] (fd = %d, acceptors = %d)\n", 
		       connstr[cp->state], cp, cp->fd, cp->acceptors));
	    } else {
 		DEBUGF((" - %s [%8p] (fd = %d)\n", connstr[cp->state], cp, 
		       cp->fd));
	    }
	    cp= cp->next;
	}
    }
}


/* 
 * Remove a connection from the list of connection, close the proxy
 * socket and free all resources. The main socket (fd) is *not* 
 * closed here, because the closing of that socket has to be synchronized
 * with the Erlang process controlling this port program.
 */
static void remove_connection(FD fd)
{
    Connection **prev = &connections;
    Connection *cp = connections; 
    
    while (cp) {
	if(cp->fd == fd) {
	    DEBUGF(("remove_connection: fd = %d\n", fd));
	    esock_ssl_free(cp);
	    esock_free(cp->flags);
	    closelog(cp->logfp);
	    esock_free(cp->wq.buf);
	    if (cp->proxy) {
		safe_close(cp->proxy->fd);
		remove_proxy(cp->proxy->fd);
	    }
	    *prev = cp->next;
	    esock_free(cp);
	    return;
	}
	prev = &cp->next;
	cp = cp->next;
    }
}

static Proxy *get_proxy_by_peerport(int port)
{
    Proxy *p = proxies;

    while(p) {
	if (p->peer_port == port)
	    return p;
	p = p->next;
    }
    return NULL;
}

static Proxy *new_proxy(FD fd)
{
    Proxy *p;

    if (!(p = esock_malloc(sizeof(Proxy))))
	return NULL;

    p->fd = fd;
    p->peer_port = -1;
    p->eof = 0;
    p->bp = 0;
    p->conn = NULL;
    p->wq.size = 0;
    p->wq.buf = NULL;
    p->wq.len = 0;
    p->wq.offset = 0;
    p->next = proxies;
    proxies = p;
    return p;
}

static void remove_proxy(FD fd)
{
    Proxy *p = proxies, **pp = &proxies;

    while(p) {
	if (p->fd == fd) {
	    DEBUGF(("remove_proxy: fd = %d\n", fd));
	    esock_free(p->wq.buf);
	    *pp = p->next;
	    esock_free(p);
	    return;
	}
	pp = &p->next;
	p = p->next;
    }
}

static void safe_close(FD fd)
{
    int err;

    err = sock_errno();
    DEBUGF(("safe_close fd = %d\n", fd));
    if (sock_close(fd) < 0) {
	DEBUGF(("sock_close close failed\n"));
    }
    sock_set_errno(err);
}

static void clean_up(void)
{
    Connection *cp, *cpnext;
    Proxy *pp, *ppnext;

    cp = connections;
    while (cp) {
	safe_close(cp->fd);
	cpnext = cp->next;
	remove_connection(cp->fd);
	cp = cpnext;
    }
    
    pp = proxies;
    while (pp) {
	safe_close(pp->fd);
	ppnext = pp->next;
	remove_proxy(pp->fd);
	pp = ppnext;
    }
}

static void ensure_write_queue(WriteQueue *wq, int size)
{
    if (wq->size < size) {
	wq->buf = esock_realloc(wq->buf, size);
	wq->size = size;
    }
}







