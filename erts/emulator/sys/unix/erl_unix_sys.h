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
 *
 * This file handles differences between different Unix systems.
 * This should be the only place with conditional compilation
 * depending on the type of OS.
 */

#ifndef _ERL_UNIX_SYS_H
#define _ERL_UNIX_SYS_H

#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifndef QNX
#include <memory.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>


#if HAVE_SOCKETIO_H
#   include <sys/socketio.h>
#endif
#if HAVE_SOCKIO_H
#   include <sys/sockio.h>
#endif

#ifdef HAVE_NET_ERRNO_H
#include <net/errno.h>
#endif

#ifdef HAVE_DIRENT_H
#  include <dirent.h>
#endif

#ifdef HAVE_UNISTD_H
#   include <unistd.h>
#endif

#ifndef HAVE_MMAP
#   define HAVE_MMAP 0
#endif

#if HAVE_MMAP
#   include <sys/mman.h>
#endif

#if TIME_WITH_SYS_TIME
#   include <sys/time.h>
#   include <time.h>
#else
#   if HAVE_SYS_TIME_H
#       include <sys/time.h>
#   else
#       include <time.h>
#   endif
#endif

#include <sys/times.h>

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#ifdef QNX
#include <process.h>
#include <sys/qnx_glob.h>
#endif

#include <pwd.h>

#ifndef HZ
#define HZ 60
#endif

#include <netdb.h>

/*
 * Make sure that MAXPATHLEN is defined.
 */

#ifndef MAXPATHLEN
#   ifdef PATH_MAX
#       define MAXPATHLEN PATH_MAX
#   else
#       define MAXPATHLEN 2048
#   endif
#endif

/*
 * Make sure that ENOTSUP is defined.
 */

#ifndef ENOTSUP
#define	ENOTSUP		-1738659
#endif
/*
** For the erl_timer_sup module.
*/

#if defined(SIZEOF_LONG) && (SIZEOF_LONG == 8)
typedef long Sint64;
#else
typedef long long Sint64;
#endif

typedef struct timeval SysTimeval;

#define sys_gettimeofday(Arg) ((void) gettimeofday((Arg), NULL))

typedef struct tms SysTimes;

extern int erts_ticks_per_sec;

#define SYS_CLK_TCK (erts_ticks_per_sec)

#define sys_times(Arg) times(Arg)

#ifdef HAVE_GETHRTIME
typedef hrtime_t SysHrTime;

#define sys_gethrtime() gethrtime()
#define sys_init_hrtime() /* Nothing */
#endif /* HAVE_GETHRTIME */

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1

/* These are defined in sys.c */
extern RETSIGTYPE (*sys_sigset())();
extern void sys_sigrelease(int);
extern void sys_sigblock(int);
extern void sys_stop_cat(void);

#endif
