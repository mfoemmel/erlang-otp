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
#ifndef _ERL_ERROR_H
#define _ERL_ERROR_H

#include <errno.h>

#include "portability.h"

/*
 * MS C uses __declspec() to do its magic.
 */
#if !defined(__WIN32__)
# define __declspec(foo) /* nothing */
#endif


__ERL_BEGIN_DECL

/*
 * Define the 'erl_errno' facility, unless already done. 
 */

/* 'erl_errno' as a function return value */
extern volatile int * 
__erl_errno_place __ERL_P((void)) __attribute__ ((__const__));

/* 'erl_errno' as a variable. */
extern __declspec(thread) volatile int __erl_errno;

#if 1				/* use the function */
# define erl_errno (*__erl_errno_place ())
#endif

#ifndef erl_errno		/* backstop: use the variable */
# define erl_errno __erl_errno
#endif

/*
 * Some error codes might be missing, so here's a backstop definitions
 * of the ones we use with `erl_errno': 
 */

#ifndef EMSGSIZE		/* Message too long */
#define EMSGSIZE        EIO
#endif

#ifndef ETIMEDOUT		/* Connection timed out */
#define ETIMEDOUT       EIO
#endif

#ifndef EHOSTUNREACH		/* No route to host */
#define EHOSTUNREACH    EIO
#endif


/* Initialize thread/task-safe erl_errno handling */
extern void erl_init_errno __ERL_P((void));

/* Report system/libc error to stderr. */
extern void erl_err_ret __ERL_P((const char * __template __ERL_DOTS )) __attribute__ ((__format__ (printf, 1, 2)));

/* Report system/libc error to stderr and die. */
extern void erl_err_sys __ERL_P((const char * __template __ERL_DOTS )) __attribute__ ((__format__ (printf, 1, 2), __noreturn__));

/* Report generic error to stderr. */
extern void erl_err_msg __ERL_P((const char * __template __ERL_DOTS )) __attribute__ ((__format__ (printf, 1, 2)));

/* Report generic error to stderr and die. */
extern void erl_err_quit __ERL_P((const char * __template __ERL_DOTS )) __attribute__ ((__format__ (printf, 1, 2), __noreturn__));

__ERL_END_DECL

#endif
