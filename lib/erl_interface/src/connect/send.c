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

#ifdef __WIN32__

# include <winsock2.h>
# include <windows.h>
# include <winbase.h>

#elif VXWORKS

# include <sys/types.h>
# include <unistd.h>
# include <sysLib.h>
# include <tickLib.h>

#else /* unix */

# include <sys/types.h>
# include <unistd.h>
# include <sys/uio.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#endif

#include <string.h>

#include "eidef.h"
#include "eiext.h"
#include "eisend.h"
#include "putget.h"
#include "ei_connect_int.h"
#include "ei_internal.h"
#include "ei_trace.h"
#include "show_msg.h"

static int ei_set_nonblock(int fd, int f_nonblock)
{
#ifndef __WIN32__
#ifdef VXWORKS
    return ioctl((fd), FIONBIO, (int) &f_nonblock);
#else
    int val = fcntl(fd, F_GETFL, 0);
    if (f_nonblock)
	val |= O_NONBLOCK;
    else
	val &= ~O_NONBLOCK;
    return fcntl(fd, F_SETFL, val);
#endif
#else
    return ioctlsocket(fd, FIONBIO, (u_long*)&f_nonblock);
#endif
}

#ifdef __WIN32__
typedef EI_LONGLONG hrtime_t;

/* FIXME problem for threaded ? */
static hrtime_t wrap = 0;
static DWORD last_tick_count = 0;

static hrtime_t gethrtime(void) 
{
    DWORD ticks = (hrtime_t) (GetTickCount() & 0x7FFFFFFF);
    if (ticks < (hrtime_t) last_tick_count) {
	wrap += 1i64 << 31;
    }
    last_tick_count = ticks;
    return ((((hrtime_t) ticks) + wrap) * 1000000i64);
}

#elif defined(VXWORKS)

typedef ULONG hrtime_t;

#define MY_TIME_MASK 0xFFFFFFF

static hrtime_t gethrtime(void) 
{
    return (tickGet() & MY_TIME_MASK);
}

static hrtime_t diff_hrtime(hrtime_t last, hrtime_t first) 
{
    if (last < first) {
	/* Wrapped */
	return (MY_TIME_MASK - first) + last;
    } else {
	return last - first;
    }
}

static void dec_timeval(struct timeval* tv, hrtime_t h)
{
    ULONG rate = sysClkRateGet();
    ULONG sec = h / rate;
    ULONG usec = (h % rate) * (1000000/rate);
    tv->tv_usec -= usec;
    if (tv->tv_usec < 0) {
	--tv->tv_sec;
	tv->tv_usec += 1000000;
    }
    tv->tv_sec -= sec;    
}

#else /* Unix */

#if !HAVE_GETHRTIME

typedef EI_LONGLONG hrtime_t;

#ifdef NO_SYSCONF
# define SYS_CLK_TCK	HZ
#else
# define SYS_CLK_TCK	sysconf(_SC_CLK_TCK)
#endif

struct tms dummy_tms;

#define KERNEL_TICKS() (sys_times(&dummy_tms) &  \
			((1UL << ((sizeof(clock_t) * 8) - 1)) - 1)) 

#define TICK_MS (1000 / SYS_CLK_TCK)

static hrtime_t gethrtime(void)
{
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return ((hrtime_t)tv.tv_sec*1000000+tv.tv_usec)*1000;
}
#endif /* !HAVE_GETHRTIME */
#endif /* __WIN32__ */

#ifndef VXWORKS

#define diff_hrtime(Last, First) ((Last)-(First))

static void dec_timeval(struct timeval* tv, hrtime_t h)
{
    h /= 1000;
    tv->tv_usec -= (int)(h % 1000000);
    if (tv->tv_usec < 0) {
	--tv->tv_sec;
	tv->tv_usec += 1000000;
    }
    tv->tv_sec -= (int)(h / 1000000);    
}
#endif

int ei_rw_timeout(int fd, int f_write, int* timeout, char* buf, int len)
{
    fd_set fds, *wr = NULL, *re = NULL;
    struct timeval timeout_tv;
    hrtime_t now, then;
    int r;

    ei_set_nonblock(fd, 1);
    now = gethrtime();
    timeout_tv.tv_sec = *timeout / 1000;
    timeout_tv.tv_usec = (*timeout % 1000) * 1000;
    while (len > 0 && *timeout > 0) {
	FD_ZERO(&fds);
	FD_SET(fd, &fds);
	if (f_write)
	    wr = &fds;
	else
	    re = &fds;
	r = select(1, re, wr, NULL, &timeout_tv);
	if (r <= 0) {
	    if (r == 0)
		erl_errno = ETIMEDOUT;
	    else
		erl_errno = errno;
	    return -1;
	}
	if (f_write)
	    r = write(fd, buf, len);
	else
	    r = read(fd, buf, len);
	then = gethrtime();
	dec_timeval(&timeout_tv, diff_hrtime(then,now));
	*timeout = timeout_tv.tv_sec*1000 + timeout_tv.tv_usec / 1000;
	if (r == 0) {				      /* normally eof */
	    erl_errno = errno;
	    return -1;
	}
	if (*timeout <= 0) {	
	    erl_errno = ETIMEDOUT;
	    return -1;
	}
	len -= r;
	buf += r;
    }
    return 0;
}

/* length (4), PASS_THROUGH (1), header, message */
static int ei_send_encoded_timeout(int fd, const erlang_pid *to,
				   const char *msg, int msglen, int timeout)
{
    char *s, header[1200]; /* see size calculation below */
    erlang_trace *token = NULL;
    int index = 5; /* reserve 5 bytes for control message */
#ifdef HAVE_WRITEV
    struct iovec v[2];
#endif

    /* are we tracing? */
    /* check that he can receive trace tokens first */
    if (ei_distversion(fd) > 0) token = ei_trace(0,NULL);
    
    /* header = SEND, cookie, to                      max sizes: */
    ei_encode_version(header,&index);		      /*   1 */
    if (token) {
	ei_encode_tuple_header(header,&index,4);      /*   2 */
	ei_encode_long(header,&index,ERL_SEND_TT);    /*   2 */
    } else {
	ei_encode_tuple_header(header,&index,3);
	ei_encode_long(header,&index,ERL_SEND); 
    }
    ei_encode_atom(header,&index,ei_getfdcookie(fd)); /* 258 */
    ei_encode_pid(header,&index,to);		      /* 268 */
    
    if (token) ei_encode_trace(header,&index,token);  /* 534 */
    
    /* control message (precedes header actually) */
    /* length = 1 ('p') + header len + message len */
    s = header;
    put32be(s, index + msglen - 4);		      /*   4 */
    put8(s, ERL_PASS_THROUGH);			      /*   1 */
				/*** sum: 1070 */

    /* FIXME incorrect level */
    if (ei_tracelevel > 0)
	ei_show_sendmsg(stderr,header,msg);

    if (timeout > 0) {
	int r = ei_rw_timeout(fd, 1, &timeout, header, index);
	if (r == 0)
	    r = ei_rw_timeout(fd, 1, &timeout, (char*)msg, msglen);
	return r;
    }
	
#ifdef HAVE_WRITEV
    
    v[0].iov_base = (char *)header;
    v[0].iov_len = index;
    v[1].iov_base = (char *)msg;
    v[1].iov_len = msglen;
    
    if (writev(fd,v,2) != index+msglen) {
	erl_errno = EIO;
	return -1;
    }
  
#else  /* !HAVE_WRITEV */
  
    if (writesocket(fd,header,index) != index) { 
	erl_errno = EIO;
	return -1;
    }
    if (writesocket(fd,msg,msglen) != msglen) { 
	erl_errno = EIO;
	return -1;
    }

#endif  /* !HAVE_WRITEV */

    return 0;
}

int ei_send_encoded(int fd, const erlang_pid *to, const char *msg, int msglen)
{
    return ei_send_encoded_timeout(fd, to, msg, msglen, 0);
}
