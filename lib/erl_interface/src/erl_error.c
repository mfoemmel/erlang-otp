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
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef VRTX
#define __READY_EXTENSIONS__
#include <errno.h>
#else
#include <errno.h>
#endif

#ifdef SUNOS4
extern char *vsprintf();
extern fflush();
extern fputs();
#endif

#ifndef MAXLINE
#define MAXLINE 4096
#endif

/* Forward */
static void err_doit(int, const char*, va_list); 

/* Fatal error related to a system call.
 * Print a message and terminate
 */
void erl_err_sys(const char *fmt, ... )
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
void erl_err_ret(const char *fmt, ... )
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
void erl_err_msg(const char *fmt, ... )
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
void erl_err_quit(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  err_doit(0, fmt, ap);
  va_end(ap);
  exit(1);
} /* erl_err_quit */


#ifdef NO_STRERROR
/* For example on SunOS we don't have the ANSI C strerror 
 */
extern int sys_nerr;
#ifdef FREEBSD
extern const char * const sys_errlist[];
#else
extern char *sys_errlist[];
#endif

/* Should be in string.h */
char *strerror(int errnum)
{
  static char *emsg[1024];

  if (errnum != 0) {
    if (errnum > 0 && errnum < sys_nerr) 
      sprintf((char *) &emsg[0], "(%s)", sys_errlist[errnum]);
    else 
      sprintf((char *) &emsg[0], "errnum = %d ", errnum);
  }
  else {
    emsg[0] = '\0';
  }
  return (char *) &emsg[0];
}
#endif


/* Print a message and return to caller.
 * Caller specifies "errnoflag".
 */
static void err_doit(int errnoflag, const char *fmt, va_list ap)
{
  int errno_save;
  char buf[MAXLINE];

#ifndef NO_ERR_MSG
  errno_save = errno;
  vsprintf(buf, fmt, ap);
  if (errnoflag)
    sprintf(buf+strlen(buf), ": %s", strerror(errno_save));
  strcat(buf,"\n");
  fflush(stdout);     /* In case stdout and stderr are the same */
  fputs(buf, stderr); 
#endif
  return;
} /* err_doit */

void
erl_assert_error(char* expr, char* file, int line)
{
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
    abort();
}

