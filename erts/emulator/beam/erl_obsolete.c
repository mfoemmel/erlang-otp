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
#include "erl_threads.h"
#include "erl_alloc.h"

/* We declare these prototypes since we can't include global.h
   (as we should). This because global.h conflicts with
   ../obsolete/driver.h. */
void erts_init_obsolete(void);
void erl_exit(int n, char*, ...);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                                           *
 * ------------------------- OBSOLETE! DO NOT USE! ------------------------- *
 *                                                                           *
\*                                                                           */


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

#ifdef USE_THREADS

typedef struct thread_id_elem__ thread_id_elem_;

struct thread_id_elem__ {
    thread_id_elem_ *prev;
    thread_id_elem_ *next;
    int in_list;
    int detached;
    ethr_tid tid;
    void* (*func)(void*);
    void* arg;
};

static thread_id_elem_ *tid_list;
static ethr_mutex tid_list_mtx;

static void free_tid_el(thread_id_elem_ *tid_el)
{
    if (tid_el) {
	if (tid_el->in_list) {
	    if (tid_el->prev)
		tid_el->prev->next = tid_el->next;
	    else
		tid_list = tid_el->next;
	    if (tid_el->next)
		tid_el->next->prev = tid_el->prev;
	}
	driver_free((void *) tid_el);
    }
}

static void *
thread_wrapper_func(void *vtid_el)
{
    thread_id_elem_ *tid_el = (thread_id_elem_ *) vtid_el;
    erts_thread_exit((*tid_el->func)(tid_el->arg));
    return NULL;
}

#endif /* #ifdef USE_THREADS */

int
erts_thread_create(erl_thread_t *tid,
		   void* (*func)(void*),
		   void* arg,
		   int detached)
{
#ifdef USE_THREADS
    int res;
    thread_id_elem_ *tid_el;

    erts_mtx_lock(&tid_list_mtx);

    tid_el = (thread_id_elem_ *) driver_alloc(sizeof(thread_id_elem_));
    if (!tid_el) {
	res = ENOMEM;
    error:
	free_tid_el(tid_el);
	(void) ethr_mutex_unlock(&tid_list_mtx);
	return res;
    }


    tid_el->in_list = 0;
    tid_el->func = func;
    tid_el->arg = arg;
    tid_el->detached = detached;

    res = ethr_thr_create(&tid_el->tid,
			  thread_wrapper_func,
			  (void *) tid_el,
			  detached);

    if (res)
	goto error;

    tid_el->next = tid_list;
    tid_el->prev = NULL;
    tid_el->in_list = 1;

    if (tid_list)
	tid_list->prev = tid_el;

    tid_list = tid_el;

    *tid = (erl_thread_t) tid_el;

    erts_mtx_unlock(&tid_list_mtx);
    return 0;
#else
    return ENOTSUP;
#endif
}

erl_thread_t
erts_thread_self(void)
{
#ifdef USE_THREADS
    thread_id_elem_ *tid_el;

    erts_mtx_lock(&tid_list_mtx);

    for (tid_el = tid_list; tid_el; tid_el = tid_el->next) {
	ethr_tid tid = ethr_self();
	if (ethr_equal_tids(tid_el->tid, tid))
	    break;
    }

    erts_mtx_unlock(&tid_list_mtx);
    ASSERT(tid_el);
    return (erl_thread_t) tid_el;
#else
    return NULL;
#endif
}

void
erts_thread_exit(void *res)
{
#ifdef USE_THREADS
    thread_id_elem_ *tid_el;

    erts_mtx_lock(&tid_list_mtx);

    for (tid_el = tid_list; tid_el; tid_el = tid_el->next) {
	ethr_tid tid = ethr_self();
	if (ethr_equal_tids(tid_el->tid, tid))
	    break;
    }

    ASSERT(tid_el);

    if (tid_el->detached)
	free_tid_el(tid_el);

    erts_mtx_unlock(&tid_list_mtx);

    erts_thr_exit(res);
#else
    erl_exit(0, "");
#endif
}

int
erts_thread_join(erl_thread_t tid, void **respp)
{
#ifdef USE_THREADS
    int res;
    thread_id_elem_ *tid_el = (thread_id_elem_ *) tid;

    ASSERT(tid_el);

    if (tid_el->detached)
	return EINVAL;

    res = ethr_thr_join(tid_el->tid, respp);
    if (res == 0) {
	erts_mtx_lock(&tid_list_mtx);
	free_tid_el(tid_el);
	erts_mtx_unlock(&tid_list_mtx);
    }
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

void erts_init_obsolete(void)
{
#ifdef USE_THREADS
    tid_list = (thread_id_elem_ *) driver_alloc(sizeof(thread_id_elem_));
    if (!tid_list)
	erts_alloc_enomem(ERTS_ALC_T_DRV, sizeof(thread_id_elem_));

    tid_list->prev = NULL;
    tid_list->next = NULL;
    tid_list->in_list = 1;
    tid_list->detached = 1;
    tid_list->tid = ethr_self();

    erts_mtx_init(&tid_list_mtx);
    erts_mtx_set_forksafe(&tid_list_mtx);
#endif

}
