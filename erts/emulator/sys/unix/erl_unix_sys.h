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

#if defined(__linux__) && defined(__GNUC__)
#   define _GNU_SOURCE 1 /* we want to turn some features */
#endif

#include <stdio.h>
#include <math.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifndef QNX
#include <memory.h>
#endif

#if defined(__sun__) && defined(__SVR4) && !defined(__EXTENSIONS__)
#   define __EXTENSIONS__
#   include <sys/types.h>
#   undef __EXTENSIONS__
#else
#   include <sys/types.h>
#endif
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>


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

#ifdef NETDB_H_NEEDS_IN_H
#include <netinet/in.h>
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

typedef struct timeval SysTimeval;

#define sys_gettimeofday(Arg) ((void) gettimeofday((Arg), NULL))

typedef struct tms SysTimes;

extern int erts_ticks_per_sec;

#define SYS_CLK_TCK (erts_ticks_per_sec)

#define sys_times(Arg) times(Arg)

#define ERTS_WRAP_SYS_TIMES 1
extern int erts_ticks_per_sec_wrap;
#define SYS_CLK_TCK_WRAP (erts_ticks_per_sec_wrap)
extern clock_t sys_times_wrap(void);

#ifdef HAVE_GETHRTIME
typedef hrtime_t SysHrTime;

#define sys_gethrtime() gethrtime()
#define sys_init_hrtime() /* Nothing */

#endif /* HAVE_GETHRTIME */

#ifdef HAVE_GETHRVTIME
#  define sys_gethrvtime() gethrvtime()
int sys_start_hrvtime(void);
int sys_stop_hrvtime(void);
#endif /* HAVE_GETHRVTIME */

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1

/* These are defined in sys.c */
#if defined(SIG_SIGSET)		/* Old SysV */
RETSIGTYPE (*sys_sigset())();
#elif defined(SIG_SIGNAL)	/* Old BSD */
RETSIGTYPE (*sys_sigset())();
#else
RETSIGTYPE (*sys_sigset(int, RETSIGTYPE (*func)(int)))(int);
#endif
extern void sys_sigrelease(int);
extern void sys_sigblock(int);
extern void sys_stop_cat(void);

/*
 * Handling of floating point exceptions.
 */

extern volatile int erl_fp_exception;

#ifdef USE_ISINF_ISNAN		/* simulate finite() */
#  define finite(f) (!isinf(f) && !isnan(f))
#  define HAVE_FINITE
#endif

#ifdef NO_FPE_SIGNALS
#  define ERTS_FP_CHECK_INIT() do {} while (0)
#  define ERTS_FP_ERROR(f, Action) if (!finite(f)) { Action; } else {}
#  define ERTS_SAVE_FP_EXCEPTION()
#  define ERTS_RESTORE_FP_EXCEPTION()
#else
#  define ERTS_FP_CHECK_INIT() do {erl_fp_exception = 0;} while (0)
#  if defined(__i386__) && defined(__GNUC__)
extern void erts_restore_x87(void);
static __inline__ int erts_check_x87(double f)
{
    __asm__ __volatile__("fwait" : "=m"(erl_fp_exception) : "m"(f));
    if( !erl_fp_exception )
       return 0;
    erts_restore_x87();
    return 1;
}
#  define ERTS_FP_ERROR(f, Action) do { if( erts_check_x87((f)) ) { Action; } } while (0)
#  else
static __inline__ int erts_check_fpe(double f)
{
    __asm__ __volatile__("" : "=m"(erl_fp_exception) : "g"(f));
    return erl_fp_exception;
}
#  define ERTS_FP_ERROR(f, Action) do { if( erts_check_fpe((f)) ) { Action; } } while (0)
#  endif
#  define ERTS_SAVE_FP_EXCEPTION() int old_erl_fp_exception = erl_fp_exception
#  define ERTS_RESTORE_FP_EXCEPTION() \
              do {erl_fp_exception = old_erl_fp_exception;} while (0)
#endif


#ifdef NEED_CHILD_SETUP_DEFINES
/* The child setup argv[] */
#define CS_ARGV_PROGNAME_IX	0		/* Program name		*/
#define CS_ARGV_WD_IX		1		/* Working directory	*/
#define CS_ARGV_CMD_IX		2		/* Command		*/
#define CS_ARGV_FD_CR_IX	3		/* Fd close range	*/
#define CS_ARGV_DUP2_OP_IX(N)	((N) + 4)	/* dup2 operations	*/

#define CS_ARGV_NO_OF_DUP2_OPS	3		/* Number of dup2 ops	*/
#define CS_ARGV_NO_OF_ARGS	7		/* Number of arguments	*/
#endif /* #ifdef NEED_CHILD_SETUP_DEFINES */

/* Threads */
#ifdef USE_THREADS
extern int init_async(int);
extern int exit_async(void);
#endif

#endif /* #ifndef _ERL_UNIX_SYS_H */
