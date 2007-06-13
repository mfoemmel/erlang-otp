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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "../obsolete/driver.h"

#include "sys.h"
#include "erl_alloc.h"
#ifdef USE_THREADS
#include "ethread.h"
#endif

/* We declare these prototypes since we can't include global.h
   (as we should). This because global.h conflicts with
   ../obsolete/driver.h. */
void erts_init_obsolete(void);
void erl_exit(int n, char*, ...);

#define DO_ABORT erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__)

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                           *
 * ------------------------- OBSOLETE! DO NOT USE! ------------------------- *
 *                                                                           *
\*                                                                           */

#ifdef USE_THREADS

typedef struct {
    int detached;
    ethr_tid tid;
    void* (*func)(void*);
    void* arg;
} erl_thread_t_;

static ethr_tsd_key tid_key;

static void
thread_exit_handler(void)
{
    void *etid = ethr_tsd_get(tid_key);
    if (etid && ((erl_thread_t_ *) etid)->detached)
	driver_free(etid);
}

static void *
thread_wrapper_func(void *vetid)
{
    erl_thread_t_ *etid = (erl_thread_t_ *) vetid;
    if (ethr_tsd_set(tid_key, vetid) != 0)
	DO_ABORT;
    return (*etid->func)(etid->arg);
}

#endif /* #ifdef USE_THREADS */

void erts_init_obsolete(void)
{
#ifdef USE_THREADS
    int res;
    res = ethr_tsd_key_create(&tid_key);
    if (res != 0)
	DO_ABORT;
    res = ethr_install_exit_handler(thread_exit_handler);
    if (res != 0)
	DO_ABORT;
#endif

}

/*
 * These functions implement the thread interface in ../obsolete/driver.h.
 * Do *not* use this interface from within the emulator; instead, use the
 * erl_threads.h interface or the ethread.h interface.
 */

erl_mutex_t
erts_mutex_create(void)
{
#ifdef USE_THREADS
    ethr_mutex *mtx = (ethr_mutex *) driver_alloc(sizeof(ethr_mutex));
    if (mtx) {
	if (ethr_mutex_init(mtx) != 0) {
	    driver_free((void *) mtx);
	    mtx = NULL;
	}
    }
    return (erl_mutex_t) mtx;
#else
    return NULL;
#endif
}

int
erts_mutex_destroy(erl_mutex_t mtx)
{
#ifdef USE_THREADS
    int res = ethr_mutex_destroy((ethr_mutex *) mtx);
    if (res)
	return res;
    driver_free((void *) mtx);
    return res;
#else
    return ENOTSUP;
#endif
}

int
erts_mutex_lock(erl_mutex_t mtx)
{
#ifdef USE_THREADS
    return ethr_mutex_lock((ethr_mutex *) mtx);
#else
    return ENOTSUP;
#endif
}

int
erts_mutex_unlock(erl_mutex_t mtx)
{
#ifdef USE_THREADS
    return ethr_mutex_unlock((ethr_mutex *) mtx);
#else
    return ENOTSUP;
#endif
}

erl_cond_t
erts_cond_create(void)
{
#ifdef USE_THREADS
    ethr_cond *cnd = (ethr_cond *) driver_alloc(sizeof(ethr_cond));
    if (cnd) {
	if (ethr_cond_init(cnd) != 0) {
	    driver_free((void *) cnd);
	    cnd = NULL;
	}
    }
    return (erl_cond_t) cnd;
#else
    return NULL;
#endif
}

int
erts_cond_destroy(erl_cond_t cnd)
{
#ifdef USE_THREADS
    int res = ethr_cond_destroy((ethr_cond *) cnd);
    if (res)
	return res;
    driver_free((void *) cnd);
    return res;
#else
    return ENOTSUP;
#endif
}


int
erts_cond_signal(erl_cond_t cnd)
{
#ifdef USE_THREADS
    return ethr_cond_signal((ethr_cond *) cnd);
#else
    return ENOTSUP;
#endif
}

int
erts_cond_broadcast(erl_cond_t cnd)
{
#ifdef USE_THREADS
    return ethr_cond_broadcast((ethr_cond *) cnd);
#else
    return ENOTSUP;
#endif
}


int
erts_cond_wait(erl_cond_t cnd, erl_mutex_t mtx)
{
#ifdef USE_THREADS
    return ethr_cond_wait((ethr_cond *) cnd, (ethr_mutex *) mtx);
#else
    return ENOTSUP;
#endif
}

int
erts_cond_timedwait(erl_cond_t cnd, erl_mutex_t mtx, long ms)
{
#ifdef USE_THREADS
    int res;
    ethr_timeval tv;

    res = ethr_time_now(&tv);
    if (res)
	return res;

    tv.tv_sec += ms / 1000;
    tv.tv_nsec += (ms % 1000) * 1000000;
    if (tv.tv_nsec >= 1000000000) {
	tv.tv_sec++;
	tv.tv_nsec -= 1000000000;
	ASSERT(tv.tv_nsec < 1000000000);
    }

    return ethr_cond_timedwait((ethr_cond *) cnd,
			       (ethr_mutex *) mtx,
			       &tv);
#else
    return ENOTSUP;
#endif
}

int
erts_thread_create(erl_thread_t *tid,
		   void* (*func)(void*),
		   void* arg,
		   int detached)
{
#ifdef USE_THREADS
    int res;
    erl_thread_t_ *etid = driver_alloc(sizeof(erl_thread_t_));
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    opts.detached = detached;

    if (!etid)
	return ENOMEM;

    etid->func = func;
    etid->arg = arg;
    etid->detached = detached;

    res = ethr_thr_create(&etid->tid, thread_wrapper_func, etid, &opts);

    if (res != 0) {
	driver_free(etid);
	return res;
    }

    *tid = (erl_thread_t) etid;
    return 0;
#else
    return ENOTSUP;
#endif
}

erl_thread_t
erts_thread_self(void)
{
#ifdef USE_THREADS
    erl_thread_t_ *etid = ethr_tsd_get(tid_key);
    if (!etid) {
	/* This is a thread not spawned by this interface. thread_exit_handler()
	   will clean it up when it terminates. */
	etid = driver_alloc(sizeof(erl_thread_t_));
	if (!etid)
	    erts_alloc_enomem(ERTS_ALC_T_DRV, sizeof(erl_thread_t_));
	etid->detached = 1; /* Detached from threads using this interface. */
	etid->tid = ethr_self();
	etid->func = NULL;
	etid->arg = NULL;
	if (ethr_tsd_set(tid_key, (void *) etid) != 0)
	    DO_ABORT;
    }
    return (erl_thread_t) etid;
#else
    return NULL;
#endif
}

void
erts_thread_exit(void *res)
{
#ifdef USE_THREADS
    ethr_thr_exit(res);
    DO_ABORT;
#else
    erl_exit(0, "");
#endif
}

int
erts_thread_join(erl_thread_t tid, void **respp)
{
#ifdef USE_THREADS
    int res;
    erl_thread_t_ *etid = (erl_thread_t_ *) tid;

    ASSERT(etid);

    if (etid->detached)
	return EINVAL;

    res = ethr_thr_join(etid->tid, respp);
    if (res == 0)
	driver_free(etid);
    return res;
#else
    return ENOTSUP;
#endif
}

int
erts_thread_kill(erl_thread_t tid)
{
    return ENOTSUP;
}

