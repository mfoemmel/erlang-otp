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
#include "erl_driver.h"
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

#define MAX_SYS_MUTEX 10

struct _thread_data
{
    void* (*func)(void*);
    void* arg;
};

/**************************************************************************/
#if defined(POSIX_THREADS)

static pthread_mutex_t sys_mutex[MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create()
{
    pthread_mutex_t* mp = (pthread_mutex_t*)
	sys_alloc(sizeof(pthread_mutex_t));
    if ((mp != NULL) && pthread_mutex_init(mp,NULL)) {
	sys_free((void*)mp);
	return NULL;
    }
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{
    pthread_mutex_t* mp;    
    if (mno >= MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    if (pthread_mutex_init(mp,NULL))
	return NULL;
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
#warning "pthread_atfork() is missing! Cannot enforce fork safety"
  return -1;
#endif
}


int erts_mutex_destroy(erts_mutex_t mtx)
{
    if (mtx != NULL) {
	int code = pthread_mutex_destroy((pthread_mutex_t*)mtx);
	sys_free(mtx);
	return code;
    }
    return -1;
}

int erts_mutex_lock (erts_mutex_t mtx)
{
    return pthread_mutex_lock((pthread_mutex_t*) mtx);
}

int erts_mutex_unlock (erts_mutex_t mtx)
{
    return pthread_mutex_unlock((pthread_mutex_t*) mtx);
}

erts_cond_t erts_cond_create()
{
    pthread_cond_t* cv = (pthread_cond_t*)
	sys_alloc(sizeof(pthread_cond_t));
    if ((cv != NULL) && pthread_cond_init(cv,NULL)) {
	sys_free((void*)cv);
	return NULL;
    }
    return (erts_cond_t) cv;
}

int erts_cond_destroy(erts_cond_t cv)
{
    if (cv != NULL) {
	int code = pthread_cond_destroy((pthread_cond_t*)cv);
	sys_free(cv);
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
      res = pthread_cond_wait((pthread_cond_t*) cv, (pthread_mutex_t*) mtx);
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
				   (pthread_mutex_t*) mtx,
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
    sys_free(args);
    
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

    td = (struct _thread_data*)sys_alloc(sizeof(struct _thread_data));
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
	sys_free(td);
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

/**************************************************************************/

#elif defined(SOLARIS_THREADS)

static mutex_t sys_mutex[MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create()
{
    mutex_t* mp = (mutex_t*)
	sys_alloc(sizeof(mutex_t));
    if ((mp != NULL) && mutex_init(mp, USYNC_THREAD, NULL)) {
	sys_free((void*)mp);
	return NULL;
    }
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{
    mutex_t* mp;    
    if (mno >= MAX_SYS_MUTEX || mno < 0)
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

int erts_mutex_destroy(erts_mutex_t mtx)
{
    if (mtx != NULL) {
	int code = mutex_destroy((mutex_t*)mtx);
	sys_free(mtx);
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
    cond_t* cv = (cond_t*)
	sys_alloc(sizeof(cond_t));
    if ((cv != NULL) && cond_init(cv,USYNC_THREAD,NULL)) {
	sys_free((void*)cv);
	return NULL;
    }
    return (erts_cond_t) cv;
}

int erts_cond_destroy(erts_cond_t cv)
{
    if (cv != NULL) {
	int code = cond_destroy((cond_t*)cv);
	sys_free(cv);
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
    sys_free(args);

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

    td = (struct _thread_data*)sys_alloc(sizeof(struct _thread_data));
    if (td == NULL)
	return -1;    
    td->func = func;
    td->arg  = arg;

    if ((code = thr_create(NULL, 0, 
			   erts_thread_func_wrap, (void*) td, 
			   detached ? THR_DETACHED : 0, &thr)) == 0)
	*tpp = (erts_thread_t) thr;
    else
	sys_free(td);
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

#else

erts_mutex_t erts_mutex_create()
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

#endif
