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
#include "erl_driver.h"
#include "erl_threads.h"

#define MAX_SYS_MUTEX 10

typedef struct _erts_wait_t {
    HANDLE event;
    struct _erts_wait_t *next;
} _erts_wait_t;

typedef struct _erts_cond_t {
    CRITICAL_SECTION cs;
    struct _erts_wait_t *waiters;
} _erts_cond_t;

static CRITICAL_SECTION sys_mutex[MAX_SYS_MUTEX];

erts_mutex_t erts_mutex_create()
{
    CRITICAL_SECTION* mp = (CRITICAL_SECTION*)
	sys_alloc(sizeof(CRITICAL_SECTION));
    if (mp != NULL)
	InitializeCriticalSection(mp);
    return (erts_mutex_t) mp;
}

erts_mutex_t erts_mutex_sys(int mno)
{   
    CRITICAL_SECTION* mp; 
    if (mno >= MAX_SYS_MUTEX || mno < 0)
	return NULL;
    mp = &sys_mutex[mno];
    InitializeCriticalSection(mp);
    return (erts_mutex_t) mp;

}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
    return 0; /* -1 ? */
}

int erts_mutex_destroy(erts_mutex_t mtx)
{
    if (mtx != NULL) {
	DeleteCriticalSection((CRITICAL_SECTION*)mtx);
	sys_free(mtx);
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
    _erts_cond_t* cvp = (_erts_cond_t*)sys_alloc(sizeof(_erts_cond_t));
    
    InitializeCriticalSection(&cvp->cs);
    cvp->waiters = NULL;
    return (erts_cond_t) cvp;
}

int erts_cond_destroy(erts_cond_t cv)
{
    _erts_cond_t* cvp = (_erts_cond_t*) cv;
    if (cvp != NULL) {
	DeleteCriticalSection(&cvp->cs);
	sys_free(cvp);
	return 0;
    }
    return -1;
}

int erts_cond_signal(erts_cond_t cv)
{
    _erts_cond_t* cvp = (_erts_cond_t*) cv;
    EnterCriticalSection(&cvp->cs);
    if (cvp->waiters != NULL) {
        SetEvent(cvp->waiters->event);
	cvp->waiters = cvp->waiters->next;
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
    _erts_wait_t* wp;
    DWORD code;
#define N_CACHED_EVENTS 10
    static HANDLE* event_cache = NULL;
    static HANDLE cached_events[N_CACHED_EVENTS+1];

    /* set up an event cache */
    if (event_cache == NULL) {
	event_cache = &cached_events[0];
	sys_memset(cached_events, 0, sizeof(cached_events)); 
    }
    /* create event and put in wait list */
    wp = (_erts_wait_t*)sys_alloc(sizeof(_erts_wait_t));
    EnterCriticalSection(&cvp->cs);
    if (*event_cache == NULL)
	wp->event = CreateEvent(NULL, FALSE, FALSE, NULL);
    else {
	wp->event = *event_cache;
	*event_cache-- = NULL;
    }

    wp->next = cvp->waiters;
    cvp->waiters = wp;
    LeaveCriticalSection(&cvp->cs);

    /* wait for event to signal */
    LeaveCriticalSection((CRITICAL_SECTION*) mtx);
    code = WaitForSingleObject(wp->event, time);

    /* delete event and remove from wait list */
    EnterCriticalSection(&cvp->cs);
    if (event_cache == &cached_events[N_CACHED_EVENTS+1])
	CloseHandle(wp->event);
    else
	*++event_cache = wp->event;
    cvp->waiters = wp->next;
    sys_free(wp);
    LeaveCriticalSection(&cvp->cs);

    /* resume processing */
    EnterCriticalSection((CRITICAL_SECTION*)mtx);
    return code == WAIT_OBJECT_0;
}


int erts_thread_create(erts_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    HANDLE h;
    DWORD ID;
    h = (HANDLE) _beginthreadex(NULL, 0, (LPTHREAD_START_ROUTINE) func, 
				(LPVOID)arg, 0, &ID);
    if (h == INVALID_HANDLE_VALUE)
	return -1;
    if (detached)
	CloseHandle(h);
    *tpp = (erts_thread_t)h;
    return 0;
}

erts_thread_t erts_thread_self()
{
    return GetCurrentThread();
}

void erts_thread_exit(void* val)
{
    _endthreadex((unsigned)val);
}

int erts_thread_join(erts_thread_t tp, void** vp)
{
    DWORD code;
    code = WaitForSingleObject((HANDLE)tp, INFINITE); /* FIX ERRORS */
    CloseHandle(tp);
    return code != WAIT_OBJECT_0;
}

int erts_thead_kill(erts_thread_t tp)
{
    return -1;
}
