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
#  include "config.h"
#endif

/*
** Native gethostbyname and gethostbyaddr
** Compile with -lnsl on solaris 
*/

/* This file don't depend on "sys.h" so we have to do some target
   definitions ourselves */

#ifdef __WIN32__
#define NO_SYSLOG
#define NO_SYSCONF
#define NO_DAEMON
#define read _read
#define write _write
#endif

#ifdef VXWORKS
#define NO_SYSLOG
#define NO_SYSCONF
#define NO_DAEMON
#define NO_FCNTL
#define DONT_USE_MAIN
#endif

/* ************************************************************************ */
/* Standard includes                                                        */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __WIN32__
#  include <io.h>
#  include <winsock2.h>
#  include <windows.h>
#  include <process.h>
#endif

#include <sys/types.h>
#include <fcntl.h>

#ifdef VXWORKS
#  include <sys/times.h>
#  include <time.h>
#  include <selectLib.h>
#  include <sockLib.h>
#  include <ioLib.h>
#  include <rpc/rpc.h>
#else /* ! VXWORKS */
#ifndef __WIN32__
#  include <sys/time.h>
#endif
#endif /* ! VXWORKS */


#ifndef __WIN32__
#  include <netinet/in.h>
#  include <sys/socket.h>

#  ifdef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H
#    include <rpc/types.h>
#  endif

#  include <arpa/inet.h>
#  include <netinet/tcp.h>
#  include <netdb.h>
#endif /* ! WIN32 */


#include <ctype.h>
#include <errno.h>
#include <signal.h>

#ifndef NO_SYSLOG
#  include <syslog.h>
#endif

#ifdef SYS_SELECT_H
#  include <sys/select.h>
#endif

#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif

#if defined(__STDC__) || defined(_MSC_VER)
#  include <stdarg.h>
#else
#  include <varargs.h>
#  define const
#endif


#define GETHOSTBYNAME 1
#define GETHOSTBYADDR 2

#define REPLY_OK    0
#define REPLY_ERROR 1

/* we MUST map address families since they are different between platforms */
#define INET_AF_INET   1
#define INET_AF_INET6  2  /* not yet */

#ifndef MAXHOSTNAME
#define MAXHOSTNAME 256
#endif

#define BUFFER_SIZE (MAXHOSTNAME+3)

#define FDIN  0
#define FDOUT 1

#ifdef DEBUG
/*#define DEBUGF(args)  fprintf(args)*/
#define DEBUGF(args)
#else
#define DEBUGF(args)
#endif


#define getint16(ptr) \
    ( (((unsigned char)((ptr)[0])) << 8) + \
      (unsigned char)((ptr)[1]))

#define getint32(ptr) \
    ( (((unsigned char)((ptr)[0])) << 24) + \
      (((unsigned char)((ptr)[1])) << 16) + \
      (((unsigned char)((ptr)[2])) << 8) + \
      (unsigned char)((ptr)[3]))


static int read_fill(int fd, char* buf, int n)
{
    int k;

    while(n) {
	if ((k = read(fd, buf, n)) < 0) {
	    if (errno == EINTR)
		continue;
	    DEBUGF((stderr, "read error %d\n", errno));
	    exit(1);
	}
	buf += k;
	n -= k;
    }
    return 0;
}

/* skip n bytes */
static void read_sync(int fd, int n)
{
    char buf[256];
    int k;

    while(n) {
	k = (n > 256) ? 256 : n;
	if ((k = read(fd, buf, k)) < 0) {
	    if (errno == EINTR)
		continue;
	    DEBUGF((stderr, "read error %d\n", errno));
	    exit(1);
	}
	n -= k;
    }
}

static void reply(int fd, int code, char* buf, int n)
{
    char pb[3];
    int m = n + 1;

    pb[0] = m / 256;
    pb[1] = m % 256;
    pb[2] = code;

    write(fd, pb, 3);
    write(fd, buf, n);
}

static int reply_host(int fd, struct hostent* hp)
{
    char   buf[1024];
    char*  ptr;
    char** p;
    char** q;
    int    n;
    int    len;
    int    buflen;

    if (hp == NULL) {

	reply(fd, REPLY_ERROR, "notfound", 8);
	return -1;
    }
    else {
	if (hp->h_addrtype != AF_INET) {
	    reply(fd, REPLY_ERROR, "notfound", 8);
	    return -1;
	}
	ptr = buf;
	/* put address length */
	buf[0] = (char) hp->h_length;    /* max 255 (check?) */
	buf[1] = INET_AF_INET;
	/* put addresses */
	buf[2] = 0;  /* number of addresses */
	buf[3] = 0;  /* number of aliases */
	buflen = 4;  /* sofar */
	ptr = buf + 4;
	len = hp->h_length;
	n = 0;
	for (p = hp->h_addr_list; *p != 0; p++) {
	    buflen += len;
	    if (buflen > 1024) goto overflow;
	    memcpy(ptr, *p, len);
	    ptr += len;
	    n++;
	}
	buf[2] = n;  /* patch number of addresses */
	n = 0;
	for (q = hp->h_aliases; *q != 0; q++) {
	    len = strlen(*q);
	    buflen += (len+1);
	    if (buflen > 1024) goto overflow;
	    memcpy(ptr, *q, len+1);
	    ptr += len+1;
	    n++;
	}
	buf[3] = n;
	len = strlen(hp->h_name);
	buflen += len;
	if (buflen > 1024) goto overflow;
	strcpy(ptr, hp->h_name);
	reply(fd, REPLY_OK, buf, buflen);
	return 0;
    }
 overflow:
    reply(fd, REPLY_ERROR, "einval", 8);
    return -1;
}



int main(int argc, char** argv)
{
    int n;
    struct hostent* hp;
    char buf[BUFFER_SIZE+1];  /* include '\0' termination */

#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    int err;

    wVersionRequested = MAKEWORD(1, 1);

    err = WSAStartup(wVersionRequested, &wsaData);
    if (err != 0)
        exit(1);

    if (LOBYTE(wsaData.wVersion) != 1 || HIBYTE(wsaData.wVersion ) != 1) {
        WSACleanup();
    	exit(1);
    }
    _setmode(FDIN,_O_BINARY);
    _setmode(FDOUT,_O_BINARY);
#endif

    while(1) {
	if ((n = read(FDIN, buf, 2)) != 2) {
	    if (n == 0)
		return 0;  /* eof */
	    return -1;     /* read error */
	}
	n = getint16(buf);
	if (n > BUFFER_SIZE) {
	    read_sync(FDIN, n);
	    reply(FDOUT, REPLY_ERROR, "einval", 6);
	    continue;
	}
	read_fill(FDIN, buf, n);
	switch(buf[0]) {
	case GETHOSTBYNAME:
	    buf[n] = '\0';   /* null terminate */
	    if (buf[1] == INET_AF_INET) {
		hp = gethostbyname(buf+2);
		reply_host(FDOUT, hp);
	    }
	    else
		reply(FDOUT, REPLY_ERROR, "einval", 6);
	    break;
	    
	case GETHOSTBYADDR:
	    if ((buf[1] == INET_AF_INET) & (n == 6)) {
		struct in_addr addr;
		memcpy(&addr.s_addr,buf+2,4);
		hp = gethostbyaddr((char*)&addr, 4, AF_INET);
		reply_host(FDOUT, hp);
	    }
	    else
		reply(FDOUT, REPLY_ERROR, "einval", 6);
	    break;

	default:
	    reply(FDOUT, REPLY_ERROR, "einval", 6);
	}
    }
    return 0;
}






