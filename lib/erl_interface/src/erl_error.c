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
 * Function: Some nice error routines taken from:
 *    "Advanced Programming in the UNIX Environment",
 *    by W.Richard Stevens
 *   
 *   void erl_err_sys(const char *fmt, ... ) fatal, sys-error 
 *   void erl_err_ret(const char *fmt, ... ) non-fatal, sys-error
 *   void erl_err_quit(const char *fmt, ...) fatal, non-sys-error
 *   void erl_err_msg(const char *fmt, ... ) non-fatal, non-sys-error
 */

#ifdef HAVE_CONFIG_H
# include "config.h"		/* FIXME: Autoconf Info prefers <config.h> */
#else
# define HAVE_STRERROR 1
# ifdef NO_STRERROR
#  undef HAVE_STRERROR
# endif
# if defined(VXWORKS)
#  define HAVE_STRERROR_R 1
# endif
#endif




#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef VRTX			/* What's VRIX? [sverkerw] */
#define __READY_EXTENSIONS__
#endif
#include <errno.h>

#if defined(VXWORKS)
#include <taskLib.h>
#include <taskVarLib.h>
#endif

#include "erl_error.h"

#ifdef SUNOS4
extern char *vsprintf();
extern fflush();
extern fputs();
#endif

/* Forward */
static void err_doit(int, const char*, va_list); 
/*    __attribute__ ((format (printf, 2, 0)))*/

/*
 * Some thoughts on flushing stdout/stderr:
 *
 * The defaults are reasonable (linebuffered stdout, unbuffered
 * stderr). If they are in effect (the user neither knows nor cares),
 * there's no need to flush.
 *
 * If the user changes these defaults (and knows what he's doing, so
 * he knows and cares) we shouldn't surprise him by
 * second-guessing. So there's a need to not flush.
 *
 * If the user doesn't know what he's doing, he's hosed anyway.
 */

/* Fatal error related to a system call.
 * Print a message and terminate.
 */
void 
erl_err_sys (const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(1, fmt, ap);
  va_end(ap);
  exit(1);
} /* erl_err_sys */

/* Nonfatal error related to a system call.
 * Print a message and return
 */
void 
erl_err_ret (const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(1, fmt, ap);
  va_end(ap);
  return;
} /* erl_err_ret */

/* Nonfatal error unrelated to a system call.
 * Print a message and return
 */
void 
erl_err_msg (const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(0, fmt, ap);
  va_end(ap);
  return;
} /* erl_err_msg */

/* Fatal error unrelated to a system call.
 * Print a message and terminate
 */
void 
erl_err_quit (const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(0, fmt, ap);
  va_end(ap);
  exit(1);
} /* erl_err_quit */



/* 
 * For example on SunOS we don't have the ANSI C strerror.
 * 
 * maybe move to a convenince lib     [sverkerw]
 */
#ifndef HAVE_STRERROR

/* CONFIG: probe for sys_nerr/_sys_nerr */
extern int sys_nerr;

/* CONFIG: probe for sys_errlist/_sys_errlist and maybe for const-ness */
#ifdef FREEBSD
extern const char * const sys_errlist[];
#else
extern char * sys_errlist[];
#endif

/* Should be in string.h */
/* Is supposed to return 'char *' (no const-ness in ANSI's prototype),
   but if you rewrite the returned string in place you deserve to
   lose. */
const char * const
strerror (int errnum)
{
    if (errnum >= 0 && errnum < sys_nerr) 
	return sys_errlist[errnum];
    else 
    {
	/* Enough buffer for 64 bits of error. It should last a while. */
	static char b[] = "(error -9223372036854775808)";
	sprintf(b, "(error %d)", errnum);
	buf[sizeof(b)-1] = '\0';
	return b;
    }
}
#endif /* !HAVE_STRERROR */

#ifndef HAVE_STRERROR_R
/*
 * A reentrant form of strerror. It's a GNU extension originally.
 */
char *
strerror_r (int errnum, char * buf, size_t n)
{
    if(n == 0) return buf;	/* zero size buffer */

#ifndef HAVE_SYS_ERRLIST	/* CONFIGURE: probe */
    /*
     * Not necessarily reentrant --- it might fool strerror() into
     * overwriting internal buffers. Silver lining: It's portable.
     */
    strncpy(buf, strerror(errnum), n-1);
#else  /* HAVE_SYS_ERRLIST */
    /*
     * Non-portable, but perfectly reentrant. Use this if possible.
     */
    {
	const char * const ptr;

	if (errnum >= 0 && errnum < sys_nerr) 
	    ptr = sys_errlist[errnum];
	else 
	{
	    /* Buffer big enough for 64 bits. It should last a
	       while. */
	    char b[] = "(error -9223372036854775808)";
	    sprintf(b, "(error %d)", errnum);
	    ptr = b;
	}

	strncpy(b, ptr, n-1);
    }
#endif /* HAVE_SYS_ERRLIST */

    buf[n-1] = '\0';
    return buf;
}
#endif /* !HAVE_STRERROR_R */


/* Print a message and return to caller.
 * Caller specifies "errnoflag".
 */
static void 
err_doit (int errnoflag, const char *fmt, va_list ap)
{
#ifndef NO_ERR_MSG
  int errno_save;

  errno_save = errno;

  vfprintf(stderr, fmt, ap);
  if (errnoflag)
  {
      fputs(": ", stderr);
      fputs(strerror(errno_save), stderr);
  }
  fputs("\n", stderr);
#endif

  return;
} /* err_doit */

void
erl_assert_error (char* expr, char* file, int line)
{
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    abort();
}


/* Define (and initialize) the variable __erl_errno */
volatile __declspec(thread) int __erl_errno = 0;

#if defined(VXWORKS)

/* 
   Moved to each of the erl_*threads.c files, as they seem to know how
   to get thread-safety. 
*/

volatile int * 
__erl_errno_place (void)
{
    /* This check is somewhat insufficient, double task var entries will occur
       if __erl_errno is actually -1, which on the other hand is an invalid 
       error code. */
    if (taskVarGet(taskIdSelf(), &__erl_errno) == ERROR) {
	taskVarAdd(taskIdSelf(), &__erl_errno);
    }
    return &__erl_errno;
}
#endif /* VXWORKS */

#if defined(__WIN32__)

volatile int *
__erl_errno_place (void)
{
    return &__erl_errno;
}

#endif /* __WIN32__ */


