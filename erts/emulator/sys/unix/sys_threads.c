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
** Thread functions:
**
** supported are pthread and solaris threads
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_alloc.h"
#include "erl_driver.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#include <signal.h>

#if defined(POSIX_THREADS)
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#elif defined(SOLARIS_THREADS)
#include <thread.h>
#include <synch.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#endif

struct _thread_data
{
    void* (*func)(void*);
    void* arg;
};

/**************************************************************************/
#if defined(POSIX_THREADS)

void __noreturn erl_exit(int n, char*, ...);

typedef struct Mutex_t_ Mutex_t;

#ifndef HAVE_PTHREAD_ATFORK
#define HAVE_PTHREAD_ATFORK 0
#endif

struct Mutex_t_ {
    pthread_mutex_t mtx;
#if HAVE_PTHREAD_ATFORK
    Mutex_t *prev;
    Mutex_t *next;
    int default_atfork;
#endif
};

#if HAVE_PTHREAD_ATFORK
static struct {
    Mutex_t *start;
    Mutex_t *end;
    pthread_mutex_t mtx;
} def_atfork_list = {NULL, NULL, PTHREAD_MUTEX_INITIALIZER};
#else
#warning "pthread_atfork() is missing! Cannot enforce fork safety"
#endif

static Mutex_t sys_mutex[ERTS_MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create(void)
{
    Mutex_t* mp = (Mutex_t *) erts_alloc_fnf(ERTS_ALC_T_MUTEX,
					     sizeof(Mutex_t));
    if ((mp != NULL) && pthread_mutex_init(&mp->mtx,NULL)) {
	erts_free(ERTS_ALC_T_MUTEX, (void *) mp);
	return NULL;
    }
#if HAVE_PTHREAD_ATFORK
    mp->prev = NULL;
    mp->next = NULL;
    mp->default_atfork = 0;
#endif
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{
    Mutex_t* mp;    
    if (mno >= ERTS_MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    if (pthread_mutex_init(&mp->mtx,NULL))
	return NULL;
#if HAVE_PTHREAD_ATFORK
    mp->prev = NULL;
    mp->next = NULL;
    mp->default_atfork = 0;
#endif
    return (erts_mutex_t) mp;
}

#ifndef HAVE_PTHREAD_ATFORK
#define HAVE_PTHREAD_ATFORK 0
#endif

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
#if HAVE_PTHREAD_ATFORK
  return pthread_atfork(prepare, parent, child) == 0 ? 0 : -1;
#else
  return -1;
#endif
}

#if HAVE_PTHREAD_ATFORK
static void lock_mutexes(void)
{
    int res;
    Mutex_t *m;

    res = pthread_mutex_lock(&def_atfork_list.mtx);
    if (res != 0) {
    error:
	erl_exit(1, "Failed to lock mutex when forking\n");
    }
    for (m = def_atfork_list.start; m; m = m->next) {
	res = pthread_mutex_lock(&m->mtx);
	if (res != 0)
	    goto error;
    }
}

static void unlock_mutexes(void)
{
    int res;
    Mutex_t *m;

    for (m = def_atfork_list.end; m; m = m->prev) {
	res = pthread_mutex_unlock(&m->mtx);
	if (res != 0) 
	    goto error;
    }

    res = pthread_mutex_unlock(&def_atfork_list.mtx);
    if (res != 0) {
    error:
	erl_exit(1, "Failed to unlock mutex when forking\n");
    }
}

#if INIT_MUTEX_IN_CHILD_AT_FORK

static void reinit_mutexes(void)
{
    int res;
    Mutex_t *mp;
    for (mp = def_atfork_list.end; mp; mp = mp->prev) {
	res = pthread_mutex_init(&mp->mtx, NULL);
	if (res != 0)
	    goto error;
    }

    res = pthread_mutex_init(&def_atfork_list.mtx, NULL);
    if (res != 0) {
    error:
	    erl_exit(1, "Failed to reinitialize mutex when forking\n");
    }
}

#endif

int erts_mutex_set_default_atfork(erts_mutex_t mtx)
{
    Mutex_t *mp = (Mutex_t *) mtx;
    static int need_init = 1;
    int res;

    if (mp->default_atfork)
	return 0;

    res = pthread_mutex_lock(&def_atfork_list.mtx);
    if (res != 0)
	return res;

    if (need_init) {
	
	res = pthread_atfork(lock_mutexes,
			     unlock_mutexes,
#if INIT_MUTEX_IN_CHILD_AT_FORK
			     reinit_mutexes
#else
			     unlock_mutexes
#endif
	    );
	if (res != 0) {
	    pthread_mutex_unlock(&def_atfork_list.mtx);
	    return res;
	}
	need_init = 0;
    }

    mp->prev = NULL;
    if (def_atfork_list.start) {
	mp->next = def_atfork_list.start;
	def_atfork_list.start->prev = mp;
	def_atfork_list.start = mp;
    }
    else {
	mp->next = NULL;
	def_atfork_list.start = def_atfork_list.end = mp;
    }
    mp->default_atfork = 1;

    return pthread_mutex_unlock(&def_atfork_list.mtx);
}

int erts_mutex_unset_default_atfork(erts_mutex_t mtx)
{
    Mutex_t *mp = (Mutex_t *) mtx;
    if (mp->default_atfork) {

	pthread_mutex_lock(&def_atfork_list.mtx);

	if (mp->prev) {
	    ASSERT(def_atfork_list.start != mp);
	    mp->prev->next = mp->next;
	}
	else {
	    ASSERT(def_atfork_list.start == mp);
	    def_atfork_list.start = mp->next;
	}
	if (mp->next) {
	    ASSERT(def_atfork_list.end != mp);
	    mp->next->prev = mp->prev;
	}
	else {
	    ASSERT(def_atfork_list.end == mp);
	    def_atfork_list.end = mp->prev;
	}

	pthread_mutex_unlock(&def_atfork_list.mtx);
    }
    return 0;
}

#else /* #if HAVE_PTHREAD_ATFORK */

int erts_mutex_set_default_atfork(erts_mutex_t mtx)
{
    return 0;
}

int erts_mutex_unset_default_atfork(erts_mutex_t mtx)
{
    return 0;
}

#endif


int erts_mutex_destroy(erts_mutex_t mtx)
{
    if (mtx != NULL) {
	Mutex_t *mp = (Mutex_t *) mtx;
	int res;
	erts_mutex_unset_default_atfork(mtx);
	res = pthread_mutex_destroy(&mp->mtx);
	erts_free(ERTS_ALC_T_MUTEX, (void *) mtx);
	return res;
    }
    return -1;
}

int erts_mutex_lock (erts_mutex_t mtx)
{
    return pthread_mutex_lock(&((Mutex_t*) mtx)->mtx);
}

int erts_mutex_unlock (erts_mutex_t mtx)
{
    return pthread_mutex_unlock(&((Mutex_t*) mtx)->mtx);
}

erts_cond_t erts_cond_create()
{
    pthread_cond_t* cv = (pthread_cond_t*)
	erts_alloc_fnf(ERTS_ALC_T_COND_VAR, sizeof(pthread_cond_t));
    if ((cv != NULL) && pthread_cond_init(cv,NULL)) {
	erts_free(ERTS_ALC_T_COND_VAR, (void *) cv);
	return NULL;
    }
    return (erts_cond_t) cv;
}

int erts_cond_destroy(erts_cond_t cv)
{
    if (cv != NULL) {
	int code = pthread_cond_destroy((pthread_cond_t*)cv);
	erts_free(ERTS_ALC_T_COND_VAR, (void *) cv);
	return code;
    }
    return -1;
}

int erts_cond_signal(erts_cond_t cv)
{
    return pthread_cond_signal((pthread_cond_t*) cv);
}

int erts_cond_broadcast (erts_cond_t cv)
{
    return pthread_cond_broadcast((pthread_cond_t*) cv);
}

int erts_cond_wait(erts_cond_t cv, erts_mutex_t mtx)
{
    int res;
    do {
      res = pthread_cond_wait((pthread_cond_t*) cv, &((Mutex_t*) mtx)->mtx);
    } while(res == EINTR);
    return res;
}

int erts_cond_timedwait(erts_cond_t cv, erts_mutex_t mtx, long time)
{
    SysTimeval tv;
    struct timespec ts;
    long us;
    long s;
    int res;
    /* convert relative time to struct timespec *abstime */
    sys_gettimeofday(&tv);
    us = (tv.tv_usec + (time % 1000)*1000);
    s  = tv.tv_sec + (time/1000);
    if (us >= 1000000) {
	us -= 1000000;
	ts.tv_sec = s+1;
    }
    else
	ts.tv_sec = s;
    ts.tv_nsec = us*1000;
    do {
      res = pthread_cond_timedwait((pthread_cond_t*) cv, 
				   &((Mutex_t*) mtx)->mtx,
				   &ts);
    } while(res == EINTR);
    return res;
}

static void* erts_thread_func_wrap(void* args)
{
    struct _thread_data td;
    sigset_t new;

    td.func = ((struct _thread_data*) args)->func;
    td.arg  = ((struct _thread_data*) args)->arg;
    erts_free(ERTS_ALC_T_THREAD_REF, (void *) args);
    
    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block child signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    pthread_sigmask(SIG_BLOCK, &new, NULL);

    return (*td.func)(td.arg);
}


int erts_thread_create(erts_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    pthread_t thr;
    pthread_attr_t attr;
    int code;
    struct _thread_data* td;

    td = (struct _thread_data*)
	erts_alloc_fnf(ERTS_ALC_T_THREAD_REF, sizeof(struct _thread_data));
    if (td == NULL)
	return -1;
    td->func = func;
    td->arg  = arg;
    pthread_attr_init(&attr);
    pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
    pthread_attr_setdetachstate(&attr, detached ?
				PTHREAD_CREATE_DETACHED :
				PTHREAD_CREATE_JOINABLE);
    if ((code = pthread_create(&thr, &attr, erts_thread_func_wrap,
			       (void*) td)) == 0)
	*tpp = (erts_thread_t) thr;
    else
	erts_free(ERTS_ALC_T_THREAD_REF, (void *) td);
    pthread_attr_destroy(&attr);
    return code;
}

erts_thread_t erts_thread_self()
{
    return (erts_thread_t) pthread_self();
}

void erts_thread_exit(void* val)
{
    pthread_exit(val);
}

int erts_thread_join(erts_thread_t tp, void** vp)
{
    return pthread_join((pthread_t)tp, vp);
}

int erts_thread_kill(erts_thread_t tp)
{
    return pthread_kill((pthread_t)tp, SIGINT);
}

int erts_thread_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
  return pthread_sigmask(how, set, oset);
}

int erts_thread_sigwait(const sigset_t *set, int *sig)
{
  return sigwait(set, sig);
}

void
erts_sys_threads_init(void)
{
    /* NOTE: erts_sys_threads_init() is called before allocators are
     * initialized; therefore, it's not allowed to call erts_alloc()
     * (and friends) from here.
     */
    
}
/**************************************************************************/

#elif defined(SOLARIS_THREADS)

static mutex_t sys_mutex[ERTS_MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create()
{
    mutex_t* mp = (mutex_t*)
	erts_alloc_fnf(ERTS_ALC_T_MUTEX, sizeof(mutex_t));
    if ((mp != NULL) && mutex_init(mp, USYNC_THREAD, NULL)) {
	erts_free(ERTS_ALC_T_MUTEX, (void *) mp);
	return NULL;
    }
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{
    mutex_t* mp;    
    if (mno >= ERTS_MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    if (mutex_init(mp, USYNC_THREAD, NULL))
	return NULL;
    return (erts_mutex_t) mp;
}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
  /* Solaris threads use fork-all; no need for atfork */
  return 0;
}

int erts_mutex_set_default_atfork(erts_mutex_t mtx)
{
    return 0;
}

int erts_mutex_unset_default_atfork(erts_mutex_t mtx)
{
    return 0;
}

int erts_mutex_destroy(erts_mutex_t mtx)
{
    if (mtx != NULL) {
	int code = mutex_destroy((mutex_t*)mtx);
	erts_free(ERTS_ALC_T_MUTEX, (void *) mtx);
	return code;
    }
    return -1;
}

int erts_mutex_lock (erts_mutex_t mtx)
{
    return mutex_lock((mutex_t*) mtx);
}

int erts_mutex_unlock (erts_mutex_t mtx)
{
    return mutex_unlock((mutex_t*) mtx);
}

erts_cond_t erts_cond_create()
{
    cond_t* cv = (cond_t*) erts_alloc_fnf(ERTS_ALC_T_COND_VAR, sizeof(cond_t));
    if ((cv != NULL) && cond_init(cv,USYNC_THREAD,NULL)) {
	erts_free(ERTS_ALC_T_COND_VAR, (void *) cv);
	return NULL;
    }
    return (erts_cond_t) cv;
}

int erts_cond_destroy(erts_cond_t cv)
{
    if (cv != NULL) {
	int code = cond_destroy((cond_t*)cv);
	erts_free(ERTS_ALC_T_COND_VAR, (void *) cv);
	return code;
    }
    return -1;
}

int erts_cond_signal(erts_cond_t cv)
{
    return cond_signal((cond_t*) cv);
}

int erts_cond_broadcast (erts_cond_t cv)
{
    return cond_broadcast((cond_t*) cv);
}

int erts_cond_wait(erts_cond_t cv, erts_mutex_t mtx)
{
    int res;
    do {
      res = cond_wait((cond_t*) cv, (mutex_t*) mtx);
    } while(res == EINTR);
    return res;
}

int erts_cond_timedwait(erts_cond_t cv, erts_mutex_t mtx, long time)
{
    SysTimeval tv;
    struct timespec ts;
    long us;
    long s;
    int res;

    /* convert relative time to struct timespec *abstime */
    sys_gettimeofday(&tv);
    us = (tv.tv_usec + (time % 1000)*1000);
    s  = tv.tv_sec + (time/1000);
    if (us >= 1000000) {
	us -= 1000000;
	ts.tv_sec = s+1;
    }
    else
	ts.tv_sec = s;
    ts.tv_nsec = us*1000;
    do {
      res = cond_timedwait((cond_t*) cv, (mutex_t*) mtx, &ts);
    } while(res == EINTR);
    return res;
}

static void* erts_thread_func_wrap(void* args)
{
    struct _thread_data td;
    sigset_t new;

    td.func = ((struct _thread_data*) args)->func;
    td.arg  = ((struct _thread_data*) args)->arg;
    erts_free(ERTS_ALC_T_THREAD_REF, (void *) args);

    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block child signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    thr_sigsetmask(SIG_BLOCK, &new, NULL);

    return (td.func)(td.arg);
}


int erts_thread_create(erts_thread_t* tpp,
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    thread_t thr;
    int code;
    struct _thread_data* td;

    td = (struct _thread_data*)
	erts_alloc(ERTS_ALC_T_THREAD_REF, sizeof(struct _thread_data));
    if (td == NULL)
	return -1;    
    td->func = func;
    td->arg  = arg;

    if ((code = thr_create(NULL, 0, 
			   erts_thread_func_wrap, (void*) td, 
			   detached ? THR_DETACHED : 0, &thr)) == 0)
	*tpp = (erts_thread_t) thr;
    else
	erts_free(ERTS_ALC_T_THREAD_REF, (void *) td);
    return code;
}

erts_thread_t erts_thread_self()
{
    return (erts_thread_t) thr_self();
}

void erts_thread_exit(void* val)
{
    thr_exit(val);
}

int erts_thread_join(erts_thread_t tp, void** vp)
{
    return thr_join((thread_t)tp, NULL, vp);
}

int erts_thread_kill(erts_thread_t tp)
{
    return thr_kill((thread_t)tp, SIGINT);
}

int erts_thread_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
  return thr_sigsetmask(how, set, oset);
}

int erts_thread_sigwait(const sigset_t *set, int *sig)
{
  *sig = sigwait(set);
  return (*sig >= 0) ? 0 : -1;
}


void
erts_sys_threads_init(void)
{
    /* NOTE: erts_sys_threads_init() is called before allocators are
     * initialized; therefore, it's not allowed to call erts_alloc()
     * (and friends) from here.
     */
    
}

#else

erts_mutex_t erts_mutex_create(void)
{
    return NULL;
}

erts_mutex_t erts_mutex_sys(int mno)
{
    return NULL;
}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
    return -1;
}

int erts_mutex_destroy(erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_lock (erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_unlock (erts_mutex_t mtx)
{
    return -1;
}

erts_cond_t erts_cond_create()
{
    return NULL;
}

int erts_cond_destroy(erts_cond_t cv)
{
    return -1;
}

int erts_cond_signal(erts_cond_t cv)
{
    return -1;
}

int erts_cond_broadcast (erts_cond_t cv)
{
    return -1;
}

int erts_cond_wait(erts_cond_t cv, erts_mutex_t mtx)
{
    return -1;
}

int erts_cond_timedwait(erts_cond_t cp, erts_mutex_t mp, long time)
{
    return -1;
}

int erts_thread_create(erts_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    return -1;
}

erts_thread_t erts_thread_self()
{
    return NULL;
}

void erts_thread_exit(void* val)
{
}

int erts_thread_join(erts_thread_t tp, void** vp)
{
    return -1;
}

int erts_thread_kill(erts_thread_t tp)
{
    return -1;
}

int erts_thread_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
  return -1;
}

int erts_thread_sigwait(const sigset_t *set, int *sig)
{
  return -1;
}

void
erts_sys_threads_init(void)
{
    /* NOTE: erts_sys_threads_init() is called before allocators are
     * initialized; therefore, it's not allowed to call erts_alloc()
     * (and friends) from here.
     */
    
}

#endif
