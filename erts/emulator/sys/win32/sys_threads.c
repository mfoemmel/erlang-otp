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
 *
 */

#include <windows.h>
#include <process.h>

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sys.h"
#include "erl_alloc.h"
#include "erl_driver.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"

static DWORD tl_wait_ix;
typedef struct _erts_wait_t {
    HANDLE event;
    struct _erts_wait_t *next;
    struct _erts_wait_t *prev;
} _erts_wait_t;

typedef struct _erts_cond_t {
    CRITICAL_SECTION cs;
    struct _erts_wait_t *waiters;
} _erts_cond_t;

static _erts_wait_t main_thread_wait;

static CRITICAL_SECTION sys_mutex[ERTS_MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create()
{
    CRITICAL_SECTION* mp = (CRITICAL_SECTION*)
	erts_alloc_fnf(ERTS_ALC_T_MUTEX, sizeof(CRITICAL_SECTION));
    if (mp != NULL)
	InitializeCriticalSection(mp);
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{   
    CRITICAL_SECTION* mp; 
    if (mno >= ERTS_MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    InitializeCriticalSection(mp);
    return (erts_mutex_t) mp;

}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
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
	DeleteCriticalSection((CRITICAL_SECTION*)mtx);
	erts_free(ERTS_ALC_T_MUTEX, mtx);
	return 0;
    }
    return -1;
}

int erts_mutex_lock (erts_mutex_t mtx)
{
    EnterCriticalSection((CRITICAL_SECTION*) mtx);
    return 0;
}

int erts_mutex_unlock (erts_mutex_t mtx)
{
    LeaveCriticalSection((CRITICAL_SECTION*) mtx);
    return 0;
}

erts_cond_t erts_cond_create()
{
    _erts_cond_t* cvp = (_erts_cond_t*)
 	erts_alloc_fnf(ERTS_ALC_T_COND_VAR, sizeof(_erts_cond_t));
    InitializeCriticalSection(&cvp->cs);
    cvp->waiters = NULL;
    return (erts_cond_t) cvp;
}

int erts_cond_destroy(erts_cond_t cv)
{
    _erts_cond_t* cvp = (_erts_cond_t*) cv;
    if (cvp != NULL) {
	DeleteCriticalSection(&cvp->cs);
	erts_free(ERTS_ALC_T_COND_VAR, cvp);
	return 0;
    }
    return -1;
}

int erts_cond_signal(erts_cond_t cv)
{
    _erts_cond_t* cvp = (_erts_cond_t*) cv;
    EnterCriticalSection(&cvp->cs);
    if (cvp->waiters) {
        SetEvent(cvp->waiters->event);
	cvp->waiters = cvp->waiters->next;
	if (cvp->waiters)
	    cvp->waiters->prev = NULL;
    }
    LeaveCriticalSection(&cvp->cs);
    return 0;
}

int erts_cond_broadcast (erts_cond_t cv)
{
    struct _erts_wait_t *wp;
    _erts_cond_t* cvp = (_erts_cond_t*) cv;

    /* signal every event in waiting queue */
    EnterCriticalSection(&cvp->cs);
    for (wp = cvp->waiters; wp; wp = wp->next)
	SetEvent(wp->event);
    cvp->waiters = NULL;
    LeaveCriticalSection(&cvp->cs);
    return 0;
}

int erts_cond_wait(erts_cond_t cv, erts_mutex_t mtx)
{
    return erts_cond_timedwait(cv, mtx, INFINITE);
}

int erts_cond_timedwait(erts_cond_t cv, erts_mutex_t mtx, long time)
{
    _erts_cond_t* cvp = (_erts_cond_t*) cv;
    _erts_wait_t *wp;
    DWORD code;

    EnterCriticalSection(&cvp->cs);

    wp = (_erts_wait_t *) TlsGetValue(tl_wait_ix);
    ASSERT(wp);

    wp->prev = NULL;
    wp->next = cvp->waiters;
    if (wp->next)
	wp->next->prev = wp;
    cvp->waiters = wp;
    LeaveCriticalSection(&cvp->cs);

    /* wait for event to signal */
    LeaveCriticalSection((CRITICAL_SECTION*) mtx);
    code = WaitForSingleObject(wp->event, time);
    EnterCriticalSection((CRITICAL_SECTION*)mtx);

    if (code != WAIT_OBJECT_0) {
	/* remove from wait list */
	EnterCriticalSection(&cvp->cs);
	if (wp->prev)
	    wp->prev->next = wp->next;
	else
	    cvp->waiters = wp->next;
	if (wp->next)
	    wp->next->prev = wp->prev;
	LeaveCriticalSection(&cvp->cs);
    }
    /* else: we was removed from the list by a signal or broadcast */

    /* resume processing */
    return code != WAIT_OBJECT_0;
}

typedef struct {
    void* arg;
    void* (*func)(void*);
    _erts_wait_t *pwait;
    int res;
} thread_data__;

static unsigned thread_wrapper(void* args)
{
    void* (*func)(void*);
    void *arg;
    _erts_wait_t wait;
    _erts_wait_t *pwait;

    func  = ((thread_data__ *) args)->func;
    arg   = ((thread_data__ *) args)->arg;
    pwait = ((thread_data__ *) args)->pwait;

    wait.event = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!wait.event || !TlsSetValue(tl_wait_ix, (LPVOID) &wait)) {
	if (wait.event)
	    CloseHandle(wait.event);
	((thread_data__ *) args)->res = -1;
	SetEvent(pwait->event);
	_endthreadex((unsigned) 1);
	return (unsigned) 1;
    }

    ASSERT(&wait == (_erts_wait_t *) TlsGetValue(tl_wait_ix));
    ((thread_data__ *) args)->res = 0;
    SetEvent(pwait->event);

    (void) /* FIXME: Implement propagation of threads result */
	(*func)(arg);

    CloseHandle(wait.event);
    _endthreadex(0);

    return 0;
}


int erts_thread_create(erts_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    thread_data__ td;
    HANDLE h;
    DWORD code;
    DWORD ID;

    td.func = func;
    td.arg = arg;
    td.res = -1;
    td.pwait = (_erts_wait_t *) TlsGetValue(tl_wait_ix);
    ASSERT(td.pwait);

    h = (HANDLE) _beginthreadex(NULL,
				0,
				(LPTHREAD_START_ROUTINE) thread_wrapper, 
				(LPVOID) &td,
				0,
				&ID);
    if (h == INVALID_HANDLE_VALUE)
	return -1;
    code = WaitForSingleObject(td.pwait->event, INFINITE);
    if (detached)
	CloseHandle(h);
    *tpp = (erts_thread_t)h;
    if (code != WAIT_OBJECT_0) {
	ASSERT(0);
	return -1;
    }
    return td.res;
}

erts_thread_t erts_thread_self()
{
    return GetCurrentThread();
}

void erts_thread_exit(void* val)
{
    _erts_wait_t *wp = (_erts_wait_t *) TlsGetValue(tl_wait_ix);
    CloseHandle(wp->event);
    /* FIXME: Implement propagation of threads result */
    _endthreadex(0);
}

int erts_thread_join(erts_thread_t tp, void** vp)
{
    /* FIXME: Implement propagation of threads result */
    DWORD code;
    code = WaitForSingleObject((HANDLE)tp, INFINITE); /* FIX ERRORS */
    CloseHandle(tp);
    return code != WAIT_OBJECT_0;
}

int erts_thread_kill(erts_thread_t tp)
{
    return -1;
}

void __noreturn erl_exit(int n, char*, ...);

void
erts_sys_threads_init(void)
{
    /* NOTE: erts_sys_threads_init() is called before allocators are
     * initialized; therefore, it's not allowed to call erts_alloc()
     * (and friends) from here.
     */

    tl_wait_ix = TlsAlloc();
    if (tl_wait_ix == -1)
	erl_exit(1, "Failed to allocate thread local wait index\n");
    if (!TlsSetValue(tl_wait_ix, (LPVOID) &main_thread_wait))
	erl_exit(1, "Failed to set main thread wait object\n");
    main_thread_wait.event = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (!main_thread_wait.event)
	erl_exit(1, "Failed to create main thread wait event\n");

    ASSERT(&main_thread_wait == (_erts_wait_t *) TlsGetValue(tl_wait_ix));
}

