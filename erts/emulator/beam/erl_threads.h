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

#include "sys.h"
#ifdef USE_THREADS

#define ETHR_TRY_INLINE_FUNCS
#include "ethread.h"
#include "erl_lock_check.h"
#include "erl_term.h"
#define ERTS_THR_OPTS_DEFAULT_INITER ETHR_THR_OPTS_DEFAULT_INITER
typedef ethr_thr_opts erts_thr_opts_t;
typedef ethr_init_data erts_thr_init_data_t;
typedef ethr_tid erts_tid_t;
typedef struct {
    ethr_mutex mtx;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock_t lc;
#endif
} erts_mtx_t;
typedef ethr_cond erts_cnd_t;
typedef ethr_tsd_key erts_tsd_key_t;
typedef ethr_timeval erts_thr_timeval_t;
void  __noreturn erts_thr_fatal_error(int, char *); /* implemented in
						       erl_init.c */

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_REC_MTX_INITER \
  {ETHR_REC_MUTEX_INITER, \
   ERTS_LC_LOCK_INIT(-1,THE_NON_VALUE,ERTS_LC_FLG_LT_MUTEX)}
#define ERTS_MTX_INITER \
  {ETHR_MUTEX_INITER, \
   ERTS_LC_LOCK_INIT(-1, THE_NON_VALUE, ERTS_LC_FLG_LT_MUTEX)}
#else
#define ERTS_REC_MTX_INITER		{ETHR_REC_MUTEX_INITER}
#define ERTS_MTX_INITER			{ETHR_MUTEX_INITER}
#endif
#define ERTS_CND_INITER			ETHR_COND_INITER
#define ERTS_THR_INIT_DATA_DEF_INITER	ETHR_INIT_DATA_DEFAULT_INITER

#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#  define ERTS_HAVE_REC_MTX_INIT	ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif
#ifdef ETHR_HAVE_ETHR_MUTEX_TRYLOCK
#  define ERTS_HAVE_MTX_TRYLOCK		ETHR_HAVE_ETHR_MUTEX_TRYLOCK
#endif


#else /* #ifdef USE_THREADS */

#define ERTS_THR_OPTS_DEFAULT_INITER 0
typedef int erts_thr_opts_t;
typedef int erts_thr_init_data_t;
typedef int erts_tid_t;
typedef int erts_mtx_t;
typedef int erts_cnd_t;
typedef int erts_tsd_key_t;
typedef struct {
    long tv_sec;
    long tv_nsec;
} erts_thr_timeval_t;

#define ERTS_REC_MTX_INITER		0
#define ERTS_MTX_INITER			0
#define ERTS_CND_INITER			0
#define ERTS_THR_INIT_DATA_DEF_INITER	0

#define ERTS_HAVE_REC_MTX_INIT		1
#define ERTS_HAVE_MTX_TRYLOCK		1

#endif /* #ifdef USE_THREADS */

ERTS_GLB_INLINE void erts_thr_init(erts_thr_init_data_t *id);
ERTS_GLB_INLINE void erts_thr_create(erts_tid_t *tid, void * (*func)(void *),
				     void *arg, erts_thr_opts_t *opts);
ERTS_GLB_INLINE void erts_thr_join(erts_tid_t tid, void **thr_res);
ERTS_GLB_INLINE void erts_thr_detach(erts_tid_t tid);
ERTS_GLB_INLINE void erts_thr_exit(void *res);
ERTS_GLB_INLINE void erts_thr_install_exit_handler(void (*exit_handler)(void));
ERTS_GLB_INLINE erts_tid_t erts_thr_self(void);
ERTS_GLB_INLINE int erts_equal_tids(erts_tid_t x, erts_tid_t y);
#ifdef ERTS_HAVE_REC_MTX_INIT
ERTS_GLB_INLINE void erts_rec_mtx_init(erts_mtx_t *mtx);
#endif
ERTS_GLB_INLINE void erts_mtx_init_x(erts_mtx_t *mtx, char *name, Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init_locked_x(erts_mtx_t *mtx,
					    char *name,
					    Eterm extra);
ERTS_GLB_INLINE void erts_mtx_init(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_init_locked(erts_mtx_t *mtx, char *name);
ERTS_GLB_INLINE void erts_mtx_destroy(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_mtx_set_forksafe(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_mtx_unset_forksafe(erts_mtx_t *mtx);
#ifdef ERTS_HAVE_MTX_TRYLOCK
ERTS_GLB_INLINE int erts_mtx_trylock(erts_mtx_t *mtx);
#endif /* #ifdef ETHR_HAVE_ETHR_MTX_TRYLOCK */
ERTS_GLB_INLINE void erts_mtx_lock(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_mtx_unlock(erts_mtx_t *mtx);
ERTS_GLB_INLINE int erts_lc_mtx_is_locked(erts_mtx_t *mtx);
ERTS_GLB_INLINE void erts_cnd_init(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_destroy(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_wait(erts_cnd_t *cnd, erts_mtx_t *mtx);
ERTS_GLB_INLINE int erts_cnd_timedwait(erts_cnd_t *cnd, erts_mtx_t *mtx,
				       erts_thr_timeval_t *time);
ERTS_GLB_INLINE void erts_cnd_signal(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_cnd_broadcast(erts_cnd_t *cnd);
ERTS_GLB_INLINE void erts_thr_time_now(erts_thr_timeval_t *time);
ERTS_GLB_INLINE void erts_tsd_key_create(erts_tsd_key_t *keyp);
ERTS_GLB_INLINE void erts_tsd_key_delete(erts_tsd_key_t key);
ERTS_GLB_INLINE void erts_tsd_set(erts_tsd_key_t key, void *value);
ERTS_GLB_INLINE void * erts_tsd_get(erts_tsd_key_t key);
#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#define ERTS_THR_HAVE_SIG_FUNCS 1
ERTS_GLB_INLINE void erts_thr_sigmask(int how, const sigset_t *set,
				      sigset_t *oset);
ERTS_GLB_INLINE void erts_thr_sigwait(const sigset_t *set, int *sig);
#endif /* #ifdef HAVE_ETHR_SIG_FUNCS */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_thr_init(erts_thr_init_data_t *id)
{
#ifdef USE_THREADS
    int res = ethr_init(id);
    if (res)
	erts_thr_fatal_error(res, "initialize thread library");
#endif
}

ERTS_GLB_INLINE void
erts_thr_create(erts_tid_t *tid, void * (*func)(void *), void *arg,
		erts_thr_opts_t *opts)
{
#ifdef USE_THREADS
    int res = ethr_thr_create(tid, func, arg, opts);
    if (res)
	erts_thr_fatal_error(res, "create thread");
#endif
}

ERTS_GLB_INLINE void
erts_thr_join(erts_tid_t tid, void **thr_res)
{
#ifdef USE_THREADS
    int res = ethr_thr_join(tid, thr_res);
    if (res)
	erts_thr_fatal_error(res, "join thread");
#endif
}


ERTS_GLB_INLINE void
erts_thr_detach(erts_tid_t tid)
{
#ifdef USE_THREADS
    int res = ethr_thr_detach(tid);
    if (res)
	erts_thr_fatal_error(res, "detach thread");
#endif
}


ERTS_GLB_INLINE void
erts_thr_exit(void *res)
{
#ifdef USE_THREADS
    ethr_thr_exit(res);
    erts_thr_fatal_error(0, "terminate thread");
#endif
}

ERTS_GLB_INLINE void
erts_thr_install_exit_handler(void (*exit_handler)(void))
{
#ifdef USE_THREADS
    int res = ethr_install_exit_handler(exit_handler);
    if (res != 0)
	erts_thr_fatal_error(res, "install thread exit handler");
#endif
}

ERTS_GLB_INLINE erts_tid_t
erts_thr_self(void)
{
#ifdef USE_THREADS
    return ethr_self();
#else
    return 0;
#endif
}


ERTS_GLB_INLINE int
erts_equal_tids(erts_tid_t x, erts_tid_t y)
{
#ifdef USE_THREADS
    return ethr_equal_tids(x, y);
#else
    return 1;
#endif
}


#ifdef ERTS_HAVE_REC_MTX_INIT
ERTS_GLB_INLINE void
erts_rec_mtx_init(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_rec_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize recursive mutex");
#endif
}
#endif

ERTS_GLB_INLINE void
erts_mtx_init_x(erts_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX, extra);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init_locked_x(erts_mtx_t *mtx, char *name, Eterm extra)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock_x(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX, extra);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &mtx->lc);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init(erts_mtx_t *mtx, char *name)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_init_locked(erts_mtx_t *mtx, char *name)
{
#ifdef USE_THREADS
    int res = ethr_mutex_init(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "initialize mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_init_lock(&mtx->lc, name, ERTS_LC_FLG_LT_MUTEX);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &mtx->lc);
#endif
#endif
}

ERTS_GLB_INLINE void
erts_mtx_destroy(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_destroy_lock(&mtx->lc);
#endif
    res = ethr_mutex_destroy(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "destroy mutex");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_set_forksafe(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_set_forksafe(&mtx->mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "set mutex forksafe");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_unset_forksafe(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_unset_forksafe(&mtx->mtx);
    if (res != 0 && res != ENOTSUP)
	erts_thr_fatal_error(res, "unset mutex forksafe");
#endif
}

#ifdef ERTS_HAVE_MTX_TRYLOCK

ERTS_GLB_INLINE int
erts_mtx_trylock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (erts_lc_trylock_force_busy(&mtx->lc))
	return EBUSY; /* Make sure caller can handle the situation without
			 causing a lock order violation */
#endif

    res = ethr_mutex_trylock(&mtx->mtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(res == 0, &mtx->lc);
#endif

    if (res != 0 && res != EBUSY)
	erts_thr_fatal_error(res, "try lock mutex");
    
    return res;
#else
    return 0;
#endif

}

#endif /* #ifdef ETHR_HAVE_ETHR_MTX_TRYLOCK */

ERTS_GLB_INLINE void
erts_mtx_lock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_lock(&mtx->lc);
#endif
    res = ethr_mutex_lock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "lock mutex");
#endif
}

ERTS_GLB_INLINE void
erts_mtx_unlock(erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_unlock(&mtx->lc);
#endif
    res = ethr_mutex_unlock(&mtx->mtx);
    if (res)
	erts_thr_fatal_error(res, "unlock mutex");
#endif
}

ERTS_GLB_INLINE int
erts_lc_mtx_is_locked(erts_mtx_t *mtx)
{
#if defined(USE_THREADS) && defined(ERTS_ENABLE_LOCK_CHECK)
    int res;
    erts_lc_lock_t lc = mtx->lc;
    lc.flags = 0;
    erts_lc_have_locks(&res, &lc, 1);
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_cnd_init(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_init(cnd);
    if (res)
	erts_thr_fatal_error(res, "initialize condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_cnd_destroy(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_destroy(cnd);
    if (res)
	erts_thr_fatal_error(res, "destroy condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_cnd_wait(erts_cnd_t *cnd, erts_mtx_t *mtx)
{
#ifdef USE_THREADS
    int res = ethr_cond_wait(cnd, &mtx->mtx);
    if (res != 0 && res != EINTR)
	erts_thr_fatal_error(res, "wait on condition variable");
#endif
}

ERTS_GLB_INLINE int
erts_cnd_timedwait(erts_cnd_t *cnd, erts_mtx_t *mtx, erts_thr_timeval_t *time)
{
#ifdef USE_THREADS
    int res = ethr_cond_timedwait(cnd, &mtx->mtx, time);
    if (res != 0 && res != EINTR && res != ETIMEDOUT)
	erts_thr_fatal_error(res,
			     "wait with timeout on condition variable");
    return res;
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_cnd_signal(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_signal(cnd);
    if (res)
	erts_thr_fatal_error(res, "signal on condition variable");
#endif
}


ERTS_GLB_INLINE void
erts_cnd_broadcast(erts_cnd_t *cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_broadcast(cnd);
    if (res)
	erts_thr_fatal_error(res, "broadcast on condition variable");
#endif
}

ERTS_GLB_INLINE void
erts_thr_time_now(erts_thr_timeval_t *time)
{
#ifdef USE_THREADS
    int res = ethr_time_now(time);
    if (res)
	erts_thr_fatal_error(res, "get current time");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_key_create(erts_tsd_key_t *keyp)
{
#ifdef USE_THREADS
    int res = ethr_tsd_key_create(keyp);
    if (res)
	erts_thr_fatal_error(res, "create thread specific data key");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_key_delete(erts_tsd_key_t key)
{
#ifdef USE_THREADS
    int res = ethr_tsd_key_delete(key);
    if (res)
	erts_thr_fatal_error(res, "delete thread specific data key");
#endif
}

ERTS_GLB_INLINE void
erts_tsd_set(erts_tsd_key_t key, void *value)
{
#ifdef USE_THREADS
    int res = ethr_tsd_set(key, value);
    if (res)
	erts_thr_fatal_error(res, "set thread specific data");
#endif
}

ERTS_GLB_INLINE void *
erts_tsd_get(erts_tsd_key_t key)
{
#ifdef USE_THREADS
    return ethr_tsd_get(key);
#else
    return NULL;
#endif
}

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS

ERTS_GLB_INLINE void
erts_thr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
#ifdef USE_THREADS
    int res = ethr_sigmask(how, set, oset);
    if (res)
	erts_thr_fatal_error(res, "get or set signal mask");
#endif
}

ERTS_GLB_INLINE void
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

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifndef ERL_THREAD_H__ */
