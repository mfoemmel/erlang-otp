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

/* Description: Error checking thread interface to the ethread library.
 *              All functions terminates the emulator on failure.
 * Author: Rickard Green
 */

#ifndef ERL_THREAD_H__
#define ERL_THREAD_H__

#ifdef USE_THREADS

#include "ethread.h"
void erts_thr_fatal_error(int, char *); /* implemented in erl_init.c */

#else /* #ifdef USE_THREADS */

typedef int ethr_init_data;
typedef int ethr_tid;
typedef int ethr_mutex;
typedef int ethr_cond;

typedef struct {
    long tv_sec;
    long tv_nsec;
} ethr_timeval;

#define ETHR_REC_MUTEX_INITER		0
#define ETHR_MUTEX_INITER		0
#define ETHR_COND_INITER		0
#define ETHR_INIT_DATA_DEFAULT_INITER 	0

#endif /* #ifdef USE_THREADS */

static ERTS_INLINE void
erts_thr_init(ethr_init_data *id)
{
#ifdef USE_THREADS
    int res = ethr_init(id);
    if (res)
	erts_thr_fatal_error(res, "initialize thread library");
#endif
}

static ERTS_INLINE void
erts_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		   int detached)
{
#ifdef USE_THREADS
    int res = ethr_thr_create(tid, func, arg, detached);
    if (res)
	erts_thr_fatal_error(res, "create thread");
#endif
}

static ERTS_INLINE void
erts_thr_join(ethr_tid tid, void **thr_res)
{
#ifdef USE_THREADS
    int res = ethr_thr_join(tid, thr_res);
    if (res)
	erts_thr_fatal_error(res, "join thread");
#endif
}


static ERTS_INLINE void
erts_thr_detach(ethr_tid tid)
{
#ifdef USE_THREADS
    int res = ethr_thr_detach(tid);
    if (res)
	erts_thr_fatal_error(res, "detach thread");
#endif
}


static ERTS_INLINE void
erts_thr_exit(void *res)
{
#ifdef USE_THREADS
    ethr_thr_exit(res);
    erts_thr_fatal_error(0, "terminate thread");
#endif
}


static ERTS_INLINE ethr_tid
erts_thr_self(void)
{
#ifdef USE_THREADS
    return ethr_self();
#else
    return 0;
#endif
}


static ERTS_INLINE int
erts_equal_tids(ethr_tid x, ethr_tid y)
{
#ifdef USE_THREADS
    return ethr_equal_tids(x, y);
#else
    return 1;
#endif
}

static ERTS_INLINE void
erts_mtx_init(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#endif
}

static ERTS_INLINE void
erts_mtx_destroy(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_destroy(mtx);
    if (res)
	erts_thr_fatal_error(res, "destroy mutex");
#endif
}

static ERTS_INLINE void
erts_mtx_set_forksafe(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_set_forksafe(mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "set mutex forksafe");
#endif
}

static ERTS_INLINE void
erts_mtx_unset_forksafe(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_unset_forksafe(mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "unset mutex forksafe");
#endif
}

static ERTS_INLINE void
erts_mtx_lock(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_lock(mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#endif
}

static ERTS_INLINE void
erts_mtx_unlock(ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_unlock(mtx);
    if (res)
	erts_thr_fatal_error(res, "unlock mutex");
#endif
}

static ERTS_INLINE void
erts_cnd_init(ethr_cond *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_init(cnd);
    if (res)
	erts_thr_fatal_error(res, "initialize condition variable");
#endif
}

static ERTS_INLINE void
erts_cnd_destroy(ethr_cond *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_destroy(cnd);
    if (res)
	erts_thr_fatal_error(res, "destroy condition variable");
#endif
}

static ERTS_INLINE void
erts_cnd_wait(ethr_cond *cnd, ethr_mutex *mtx)
{
#ifdef USE_THREADS
    int res;
    do {
	res = ethr_cond_wait(cnd, mtx);
    } while (res == EINTR);
    if (res)
	erts_thr_fatal_error(res, "wait on condition variable");
#endif
}

static ERTS_INLINE int
erts_cnd_timedwait(ethr_cond *cnd, ethr_mutex *mtx, ethr_timeval *time)
{
#ifdef USE_THREADS
    int res;
    do {
	res = ethr_cond_timedwait(cnd, mtx, time);
    } while (res == EINTR);
    if (res != 0 && res != ETIMEDOUT)
	erts_thr_fatal_error(res,
				 "wait with timeout on condition variable");
    return res;
#else
    return 0;
#endif
}

static ERTS_INLINE void
erts_cnd_signal(ethr_cond *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_signal(cnd);
    if (res)
	erts_thr_fatal_error(res, "signal on condition variable");
#endif
}


static ERTS_INLINE void
erts_cnd_broadcast(ethr_cond *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_broadcast(cnd);
    if (res)
	erts_thr_fatal_error(res, "broadcast on condition variable");
#endif
}

static ERTS_INLINE void
erts_thr_time_now(ethr_timeval *time)
{
#ifdef USE_THREADS
    int res = ethr_time_now(time);
    if (res)
	erts_thr_fatal_error(res, "get current time");
#endif
}

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS

static ERTS_INLINE void
erts_thr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#ifdef USE_THREADS
    int res = ethr_sigmask(how, set, oset);
    if (res)
	erts_thr_fatal_error(res, "get or set signal mask");
#endif
}

static ERTS_INLINE void
erts_thr_sigwait(const sigset_t *set, int *sig)
{
#ifdef USE_THREADS
    int res;
    do {
	res = ethr_sigwait(set, sig);
    } while (res == EINTR);
    if (res)
	erts_thr_fatal_error(res, "to wait for signal");
#endif
}

#endif /* #ifdef HAVE_ETHR_SIG_FUNCS */

#endif /* #ifndef ERL_THREAD_H__ */
