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

#ifndef _ERL_OSE_SYS_H
#define _ERL_OSE_SYS_H

#ifdef __ERL_TERM_H		/* def of NIL as empty list */
#define E_NIL NIL
#endif
#undef NIL			

#ifdef start
#define e_start start
#endif
#undef start

#include "ose.h"		/* redefines NIL as "empty signal" */
#define OSE_NIL NIL		/* erl ose apps uses OSE_NIL instead */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
#include <dirent.h>
#include <sys/time.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef _OSE_PPC750_
#include <string.h>
#endif

#undef  exit
#define exit(a)   kill_proc(current_process())

#undef NIL			/* undef ose NIL */
#undef start
#ifdef E_NIL
#define NIL E_NIL		/* redefine erl NIL */
#endif

#undef getenv
#define getenv(v) get_env(get_bid(current_process()), (v))

#ifdef _OSE_SFK_
#undef free
#undef malloc
#undef realloc
#undef free			/* avoid clash in hash.c */
#define sys_ose_free(p)          zzzfree(p) 
#define sys_ose_calloc(n, size)  zzzcalloc(n, size)
#define sys_ose_malloc(size)     zzzmalloc(size)
#define sys_ose_realloc(p, size) zzzrealloc(p, size)
#else
#define sys_ose_free(p)          free(p)
#define sys_ose_calloc(n, size)  calloc(n, size)
#define sys_ose_malloc(size)     malloc(size)
#define sys_ose_realloc(p, size) realloc(p, size)
#endif

/* the name of the erts OSE process */
#define ERTS_OSE_PROC_NAME "erts"
/* erl_block should be defined in erl.exec.c */
extern PROCESS erl_block;

#ifndef HZ
#define HZ 60
#endif

#include <netdb.h>

/* Standard definitions missing from OSE headers */
#define INT_MAX         2147483647
#define UINT_MAX        4294967295U

#ifdef _OSE_SFK_
void *memcpy(void *s1, const void *s2, size_t n);
#endif

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

/********************* useful definitions ********************/
#ifndef _OSE_
#define _OSE_
#endif

/* many of the functions in unistd & fcntl are redefined in inet.h,
   do we need them for anything else!? Yes: e.g. in break.c */
#define HAVE_F_DUPFD
#define NO_FTRUNCATE
#define DONT_USE_MAIN
#define NO_UMASK
#define NO_DEVICE_FILES
#define NO_UID
#define NO_ACCESS
#define NO_SYSLOG
#define NO_SYSCONF
#define NO_PWD		
#define NO_DAEMON

/* valid for PPC750!!? */
#define HAVE_CONFLICTING_FREAD_DECLARATION

/* inet.h defines e.g. read, write and close. If the inet functions are
   to be used, inet.h must be included before sys.h in the file. */
#ifndef _INET_H			
#include <unistd.h>
#include <fcntl.h>
#endif

/*************** floating point exception handling ***********/

/* for now we'll only handle soft float */

#include <math.h>
#include <ieeefp.h>
#include <ieee.h>

#define NO_FPE_SIGNALS
#define ERTS_FP_CHECK_INIT() do {} while (0)
#define ERTS_SAVE_FP_EXCEPTION()
#define ERTS_RESTORE_FP_EXCEPTION()
#define ERTS_FP_ERROR(f, Action) if (_isinf(f) || _isnan(f)) { Action; } else {}

#define asinh undef_math_func_1
#define acosh undef_math_func_1
#define atanh undef_math_func_1

/********************* time support ********************/

#define HAVE_GETHRTIME

typedef long long SysHrTime;
typedef struct timeval SysTimeval;

typedef struct _tms {
    clock_t tms_utime;
    clock_t tms_stime;
    clock_t tms_cutime;
    clock_t tms_cstime;
} SysTimes;

extern int sys_clk_tck;
#define SYS_CLK_TCK (sys_clk_tck)

void sys_gettimeofday(SysTimeval *tvp);
int sys_init_hrtime(void);
SysHrTime sys_gethrtime(void);
clock_t sys_times(SysTimes *t);

/* No use in having other resolutions than 1 Ms. */
#define SYS_CLOCK_RESOLUTION 1


/********************* sizes ****************************/

#define CHAR_BIT       8	/* missing in limits.h */
#define SIZEOF_SHORT   2
#define SIZEOF_INT     4
#define SIZEOF_LONG    4
#define SIZEOF_VOID_P  4
#define SIZEOF_SIZE_T  4
#define SIZEOF_OFF_T   4

/***************** trace & debug ***********************/

/* only for testing, use compile flag later */
#define TRACE_OSE_SIG_ALLOC 1

#ifdef TRACE_OSE_SIG_ALLOC
union SIGNAL *ose_sig_alloc(OSBUFSIZE size, SIGSELECT signo);
void ose_sig_free_buf(union SIGNAL **sig);
#else
#define ose_sig_alloc alloc
#define ose_sig_free_buf free_buf
#endif

/*************** program registration *****************/
#define PGM_SERVER "erl_sys_pgm_server"
#define REG_PGM    1
#define GET_PGM    2
#define DEL_PGM    3
#define ADD_HND    4

typedef struct pgm_entry {
  SIGSELECT sigNo;
  void *hnd;
  void *entrypoint;
  int is_static;
  char name[1];
} pgmEntry;



#endif

