/*
** Thread functions:
**
** supported are pthread and solaris threads
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "driver.h"


#if defined(POSIX_THREADS)
#include <pthread.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

#elif defined(SOLARIS_THREADS)
#include <thread.h>
#include <synch.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include <signal.h>

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

erl_mutex_t erl_mutex_create()
{
    pthread_mutex_t* mp = (pthread_mutex_t*)
	sys_alloc(sizeof(pthread_mutex_t));
    if ((mp != NULL) && pthread_mutex_init(mp,NULL)) {
	sys_free((void*)mp);
	return NULL;
    }
    return (erl_mutex_t) mp;
}

erl_mutex_t erl_mutex_sys(int mno)
{
    pthread_mutex_t* mp;    
    if (mno >= MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    if (pthread_mutex_init(mp,NULL))
	return NULL;
    return (erl_mutex_t) mp;
}

int erl_mutex_destroy(erl_mutex_t mtx)
{
    if (mtx != NULL) {
	int code = pthread_mutex_destroy((pthread_mutex_t*)mtx);
	sys_free(mtx);
	return code;
    }
    return -1;
}

int erl_mutex_lock (erl_mutex_t mtx)
{
    return pthread_mutex_lock((pthread_mutex_t*) mtx);
}

int erl_mutex_unlock (erl_mutex_t mtx)
{
    return pthread_mutex_unlock((pthread_mutex_t*) mtx);
}

erl_cond_t erl_cond_create()
{
    pthread_cond_t* cv = (pthread_cond_t*)
	sys_alloc(sizeof(pthread_cond_t));
    if ((cv != NULL) && pthread_cond_init(cv,NULL)) {
	sys_free((void*)cv);
	return NULL;
    }
    return (erl_cond_t) cv;
}

int erl_cond_destroy(erl_cond_t cv)
{
    if (cv != NULL) {
	int code = pthread_cond_destroy((pthread_cond_t*)cv);
	sys_free(cv);
	return code;
    }
    return -1;
}

int erl_cond_signal(erl_cond_t cv)
{
    return pthread_cond_signal((pthread_cond_t*) cv);
}

int erl_cond_broadcast (erl_cond_t cv)
{
    return pthread_cond_broadcast((pthread_cond_t*) cv);
}

int erl_cond_wait(erl_cond_t cv, erl_mutex_t mtx)
{
    return pthread_cond_wait((pthread_cond_t*) cv, (pthread_mutex_t*) mtx);
}

int erl_cond_timedwait(erl_cond_t cv, erl_mutex_t mtx, long time)
{
    SysTimeval tv;
    struct timespec ts;
    long us;
    long s;
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
    return pthread_cond_timedwait((pthread_cond_t*) cv, 
				  (pthread_mutex_t*) mtx,
				  &ts);
}

static void* erl_thread_func_wrap(void* args)
{
    struct _thread_data* td = (struct _thread_data*) args;
    void* res;
    sigset_t new;

    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block pipe signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    pthread_sigmask(SIG_BLOCK, &new, NULL);

    /* FIXME handle cancelations !! */
    res = (*td->func)(td->arg);
    sys_free(td);
    return res;
}


int erl_thread_create(erl_thread_t* tpp, 
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
    if ((code = pthread_create(&thr, &attr, erl_thread_func_wrap,
			       (void*) td)) == 0)
	*tpp = (erl_thread_t) thr;
    else
	sys_free(td);
    return code;
}

erl_thread_t erl_thread_self()
{
    return (erl_thread_t) pthread_self();
}

void erl_thread_exit(void* val)
{
    pthread_exit(val);
}

int erl_thread_join(erl_thread_t tp, void** vp)
{
    return pthread_join((pthread_t)tp, vp);
}

int erl_thread_kill(erl_thread_t tp)
{
    return pthread_kill((pthread_t)tp, SIGINT);
}

/**************************************************************************/

#elif defined(SOLARIS_THREADS)

static mutex_t sys_mutex[MAX_SYS_MUTEX];

erl_mutex_t erl_mutex_create()
{
    mutex_t* mp = (mutex_t*)
	sys_alloc(sizeof(mutex_t));
    if ((mp != NULL) && mutex_init(mp, USYNC_THREAD, NULL)) {
	sys_free((void*)mp);
	return NULL;
    }
    return (erl_mutex_t) mp;
}

erl_mutex_t erl_mutex_sys(int mno)
{
    mutex_t* mp;    
    if (mno >= MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    if (mutex_init(mp, USYNC_THREAD, NULL))
	return NULL;
    return (erl_mutex_t) mp;
}

int erl_mutex_destroy(erl_mutex_t mtx)
{
    if (mtx != NULL) {
	int code = mutex_destroy((mutex_t*)mtx);
	sys_free(mtx);
	return code;
    }
    return -1;
}

int erl_mutex_lock (erl_mutex_t mtx)
{
    return mutex_lock((mutex_t*) mtx);
}

int erl_mutex_unlock (erl_mutex_t mtx)
{
    return mutex_unlock((mutex_t*) mtx);
}

erl_cond_t erl_cond_create()
{
    cond_t* cv = (cond_t*)
	sys_alloc(sizeof(cond_t));
    if ((cv != NULL) && cond_init(cv,USYNC_THREAD,NULL)) {
	sys_free((void*)cv);
	return NULL;
    }
    return (erl_cond_t) cv;
}

int erl_cond_destroy(erl_cond_t cv)
{
    if (cv != NULL) {
	int code = cond_destroy((cond_t*)cv);
	sys_free(cv);
	return code;
    }
    return -1;
}

int erl_cond_signal(erl_cond_t cv)
{
    return cond_signal((cond_t*) cv);
}

int erl_cond_broadcast (erl_cond_t cv)
{
    return cond_broadcast((cond_t*) cv);
}

int erl_cond_wait(erl_cond_t cv, erl_mutex_t mtx)
{
    return cond_wait((cond_t*) cv, (mutex_t*) mtx);
}

int erl_cond_timedwait(erl_cond_t cv, erl_mutex_t mtx, long time)
{
    SysTimeval tv;
    struct timespec ts;
    long us;
    long s;

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
    return cond_timedwait((cond_t*) cv, 
			  (mutex_t*) mtx,
			  &ts);
}

static void* erl_thread_func_wrap(void* args)
{
    struct _thread_data* td = (struct _thread_data*) args;
    void* res;
    sigset_t new;

    sigemptyset(&new);
    sigaddset(&new, SIGINT);   /* block interrupt */
    sigaddset(&new, SIGCHLD);  /* block pipe signals */
    sigaddset(&new, SIGUSR1);  /* block user defined signal */
    thr_sigsetmask(SIG_BLOCK, &new, NULL);

    /* FIXME handle cancelations !! */
    res = (td->func)(td->arg);
    sys_free(td);
    return res;
}


int erl_thread_create(erl_thread_t* tpp,
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

    if ((code = thr_create(&thr, 0, 
			   erl_thread_func_wrap, (void*) td, 
			   detached ? THR_DETACHED : 0, &thr)) == 0)
	*tpp = (erl_thread_t) thr;
    else
	sys_free(td);
    return code;
}

erl_thread_t erl_thread_self()
{
    return (erl_thread_t) thr_self();
}

void erl_thread_exit(void* val)
{
    thr_exit(val);
}

int erl_thread_join(erl_thread_t tp, void** vp)
{
    return thr_join((thread_t)tp, NULL, vp);
}

int erl_thread_kill(erl_thread_t tp)
{
    return thr_kill((thread_t)tp, SIGINT);
}


#else

erl_mutex_t erl_mutex_create()
{
    return NULL;
}

erl_mutex_t erl_mutex_sys(int mno)
{
    return NULL;
}

int erl_mutex_destroy(erl_mutex_t mtx)
{
    return -1;
}

int erl_mutex_lock (erl_mutex_t mtx)
{
    return -1;
}

int erl_mutex_unlock (erl_mutex_t mtx)
{
    return -1;
}

erl_cond_t erl_cond_create()
{
    return NULL;
}

int erl_cond_destroy(erl_cond_t cv)
{
    return -1;
}

int erl_cond_signal(erl_cond_t cv)
{
    return -1;
}

int erl_cond_broadcast (erl_cond_t cv)
{
    return -1;
}

int erl_cond_wait(erl_cond_t cv, erl_mutex_t mtx)
{
    return -1;
}

int erl_cond_timedwait(erl_cond_t cp, erl_mutex_t mp, long time)
{
    return -1;
}

int erl_thread_create(erl_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    return -1;
}

erl_thread_t erl_thread_self()
{
    return NULL;
}

void erl_thread_exit(void* val)
{
}

int erl_thread_join(erl_thread_t tp, void** vp)
{
    return -1;
}

int er_thread_kill(erl_thread_t tp)
{
    return -1;
}


#endif
