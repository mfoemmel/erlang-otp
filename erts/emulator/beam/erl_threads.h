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
 * Small thread function, wrapping threads on solaris,
 * emulating some pthread calls on Win32
 */

#ifndef __ERL_THREADS_H__
#define __ERL_THREADS_H__

#ifndef EXTERN
#ifdef __cplusplus
#   define EXTERN extern "C"
#else
#   define EXTERN extern
#endif
#endif

/* opaque data */
typedef struct _erts_mutex_t* erts_mutex_t;
typedef struct _erts_cond_t* erts_cond_t;
typedef struct _erts_thread_t* erts_thread_t;

EXTERN erts_mutex_t erts_mutex_create (void);
EXTERN int erts_mutex_destroy (erts_mutex_t);
EXTERN int erts_mutex_lock (erts_mutex_t);
EXTERN int erts_mutex_unlock (erts_mutex_t);

EXTERN erts_cond_t erts_cond_create (void);
EXTERN int erts_cond_destroy (erts_cond_t);
EXTERN int erts_cond_signal (erts_cond_t);
EXTERN int erts_cond_broadcast (erts_cond_t);
EXTERN int erts_cond_wait (erts_cond_t, erts_mutex_t);
EXTERN int erts_cond_timedwait (erts_cond_t, erts_mutex_t, long);

EXTERN int erts_thread_create (erts_thread_t*,
			      void* (*func)(void*),
			      void* arg,
			      int detached);
EXTERN erts_thread_t erts_thread_self (void);
EXTERN void erts_thread_exit (void*);
EXTERN int  erts_thread_join (erts_thread_t, void**);
EXTERN int  erts_thread_kill (erts_thread_t);

#ifdef ERL_THREADS_EMU_INTERNAL__

#define ERTS_MUTEX_SYS_ELIB_MALLOC		0
#define ERTS_MUTEX_SYS_CHILD_STATUS		1
#define ERTS_MUTEX_SYS_INSTR			2
#define ERTS_MUTEX_SYS_INSTR_X			3
#define ERTS_MUTEX_SYS_MTRACE			4
#define ERTS_MUTEX_SYS_MSEG			5
#define ERTS_MUTEX_SYS_TEMP_ALLOC		6
#define ERTS_MUTEX_SYS_SL_ALLOC			7
#define ERTS_MUTEX_SYS_STD_ALLOC		8
#define ERTS_MUTEX_SYS_LL_ALLOC			9
#define ERTS_MUTEX_SYS_EHEAP_ALLOC		10
#define ERTS_MUTEX_SYS_BINARY_ALLOC		11
#define ERTS_MUTEX_SYS_ETS_ALLOC		12

#define ERTS_MAX_SYS_MUTEX			13

EXTERN erts_mutex_t erts_mutex_sys(int);
EXTERN int erts_atfork_sys(void (*)(void), void (*)(void), void (*)(void));
EXTERN int erts_mutex_set_default_atfork(erts_mutex_t mtx);
EXTERN int erts_mutex_unset_default_atfork(erts_mutex_t mtx);
#ifdef UNIX
EXTERN int erts_thread_sigmask(int, const sigset_t *, sigset_t *);
EXTERN int erts_thread_sigwait(const sigset_t *, int *);
#endif
EXTERN void erts_sys_threads_init(void);
#endif /* #ifdef ERL_THREADS_EMU_INTERNAL__ */

#endif /* #ifndef __ERL_THREADS_H__ */


