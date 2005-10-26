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
 * Description: Thread library for use in the ERTS and other OTP
 *              applications.
 * Author: Rickard Green
 */

#ifndef ETHREAD_H__
#define ETHREAD_H__

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#include "ethread_header_config.h"
#endif

#include <stdlib.h>
#include <errno.h>

typedef struct {
    long tv_sec;
    long tv_nsec;
} ethr_timeval;

#if defined(ETHR_PTHREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The pthread implementation                                                *
\*                                                                           */

#if defined(__linux__) && !defined(_GNU_SOURCE)
#error "_GNU_SOURCE not defined. Please, compile all files with -D_GNU_SOURCE."
#endif

#if defined(ETHR_HAVE_MIT_PTHREAD_H)
#include <pthread/mit/pthread.h>
#elif defined(ETHR_HAVE_PTHREAD_H)
#include <pthread.h>
#endif

/* Types */

typedef pthread_t ethr_tid;

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    pthread_mutex_t pt_mtx;
    int is_rec_mtx;
    ethr_mutex *prev;
    ethr_mutex *next;
};

typedef pthread_cond_t ethr_cond;

/* Static initializers */
#define ETHR_MUTEX_INITER {PTHREAD_MUTEX_INITIALIZER, 0, NULL, NULL}
#define ETHR_COND_INITER PTHREAD_COND_INITIALIZER

#if defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE) \
    || defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
#  define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1
#  ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#    define ETHR_REC_MUTEX_INITER \
            {PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP, 1, NULL, NULL}
#  endif
#else
#  undef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif

#ifndef ETHR_HAVE_PTHREAD_ATFORK
#  define ETHR_NO_FORKSAFETY 1
#endif

#define ETHR_HAVE_ETHR_SIG_FUNCS 1

#elif defined(ETHR_WIN32_THREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The native win32 threads implementation                                   *
\*                                                                           */

#include <windows.h>

#ifndef EWOULDBLOCK
#  define EWOULDBLOCK (10035) /* WSAEWOULDBLOCK */
#endif
#ifndef ETIMEDOUT
#  define ETIMEDOUT (10060) /* WSAETIMEDOUT */
#endif

/* Types */
typedef long ethr_tid; /* thread id type */
typedef struct {
    int initialized;
    CRITICAL_SECTION cs;
} ethr_mutex;

typedef struct cnd_wait_event__ cnd_wait_event_;

typedef struct {
    int initialized;
    CRITICAL_SECTION cs;
    cnd_wait_event_ *queue;
    cnd_wait_event_ *queue_end;
} ethr_cond;

/* Static initializers */

#define ETHR_MUTEX_INITER {0}
#define ETHR_COND_INITER {0}

#define ETHR_REC_MUTEX_INITER ETHR_MUTEX_INITER

#define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1

#undef ETHR_HAVE_ETHR_SIG_FUNCS

#else /* No supportet thread lib found */

#ifdef ETHR_NO_SUPP_THR_LIB_NOT_FATAL
#define ETHR_NO_THREAD_LIB
#else
#error "No supported thread lib found"
#endif

#endif

#ifndef EWOULDBLOCK
#  define EWOULDBLOCK EAGAIN
#endif
#ifndef ETIMEDOUT
#  define ETIMEDOUT EAGAIN
#endif
/* ENOTSUP: same as in sys.h */
#ifndef ENOTSUP
#  ifdef EOPNOTSUPP
#    define ENOTSUP EOPNOTSUPP
#  else
#    define ENOTSUP -1738659
#  endif
#endif

typedef struct {
    void *(*thread_create_prepare_func)(void);
    void (*thread_create_parent_func)(void *);
    void (*thread_create_child_func)(void *);
} ethr_init_data;

#define ETHR_INIT_DATA_DEFAULT_INITER {0}

int ethr_init(ethr_init_data *);
int ethr_thr_create(ethr_tid *, void * (*)(void *), void *, int);
int ethr_thr_join(ethr_tid, void **);
int ethr_thr_detach(ethr_tid);
void ethr_thr_exit(void *);
ethr_tid ethr_self(void);
int ethr_equal_tids(ethr_tid, ethr_tid);
int ethr_mutex_init(ethr_mutex *);
#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
int ethr_rec_mutex_init(ethr_mutex *);
#endif
int ethr_mutex_destroy(ethr_mutex *);
int ethr_mutex_set_forksafe(ethr_mutex *);
int ethr_mutex_unset_forksafe(ethr_mutex *);
int ethr_mutex_lock(ethr_mutex *);
int ethr_mutex_unlock(ethr_mutex *);
int ethr_cond_init(ethr_cond *);
int ethr_cond_destroy(ethr_cond *);
int ethr_cond_signal(ethr_cond *);
int ethr_cond_broadcast(ethr_cond *);
int ethr_cond_wait(ethr_cond *, ethr_mutex *);
int ethr_cond_timedwait(ethr_cond *, ethr_mutex *, ethr_timeval *);
int ethr_time_now(ethr_timeval *);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#include <signal.h>
int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset);
int ethr_sigwait(const sigset_t *set, int *sig);
#endif

#endif /* #ifndef ETHREAD_H__ */
