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

#include "driver.h"
#include "sys.h"

typedef struct _erl_wait_t {
    HANDLE event;
    struct _erl_wait_t *next;
} _erl_wait_t;

typedef struct _erl_cond_t {
    CRITICAL_SECTION cs;
    struct _erl_wait_t *waiters;
} _erl_cond_t;


erl_mutex_t erl_mutex_create()
{
    CRITICAL_SECTION* mp = (CRITICAL_SECTION*)
	sys_alloc(sizeof(CRITICAL_SECTION));
    if (mp != NULL)
	InitializeCriticalSection(mp);
    return (erl_mutex_t) mp;
}

int erl_mutex_destroy(erl_mutex_t mtx)
{
    if (mtx != NULL) {
	DeleteCriticalSection((CRITICAL_SECTION*)mtx);
	sys_free(mtx);
	return 0;
    }
    return -1;
}

int erl_mutex_lock (erl_mutex_t mtx)
{
    EnterCriticalSection((CRITICAL_SECTION*) mtx);
    return 1;
}

int erl_mutex_unlock (erl_mutex_t mtx)
{
    LeaveCriticalSection((CRITICAL_SECTION*) mtx);
    return 1;
}

erl_cond_t erl_cond_create()
{
    _erl_cond_t* cvp = (_erl_cond_t*)sys_alloc(sizeof(_erl_cond_t));
    
    InitializeCriticalSection(&cvp->cs);
    cvp->waiters = NULL;
    return (erl_cond_t) cvp;
}

int erl_cond_destroy(erl_cond_t cv)
{
    _erl_cond_t* cvp = (_erl_cond_t*) cv;
    if (cvp != NULL) {
	DeleteCriticalSection(&cvp->cs);
	sys_free(cvp);
	return 0;
    }
    return -1;
}

int erl_cond_signal(erl_cond_t cv)
{
    _erl_cond_t* cvp = (_erl_cond_t*) cv;
    EnterCriticalSection(&cvp->cs);
    if (cvp->waiters != NULL) {
        SetEvent(cvp->waiters->event);
	cvp->waiters = cvp->waiters->next;
    }
    LeaveCriticalSection(&cvp->cs);
    return 0;
}

int erl_cond_broadcast (erl_cond_t cv)
{
    struct _erl_wait_t *wp;
    _erl_cond_t* cvp = (_erl_cond_t*) cv;

    /* signal every event in waiting queue */
    EnterCriticalSection(&cvp->cs);
    for (wp = cvp->waiters; wp; wp = wp->next)
	SetEvent(wp->event);
    cvp->waiters = NULL;
    LeaveCriticalSection(&cvp->cs);
    return 0;
}

int erl_cond_wait(erl_cond_t cv, erl_mutex_t mtx)
{
    return erl_cond_timedwait(cv, mtx, INFINITE);
}

int erl_cond_timedwait(erl_cond_t cv, erl_mutex_t mtx, long time)
{
    _erl_cond_t* cvp = (_erl_cond_t*) cv;
    _erl_wait_t* wp;
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
    wp = (_erl_wait_t*)sys_alloc(sizeof(_erl_wait_t));
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


int erl_thread_create(erl_thread_t* tpp, 
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
    *tpp = (erl_thread_t)h;
    return 0;
}

erl_thread_t erl_thread_self()
{
    return GetCurrentThread();
}

void erl_thread_exit(void* val)
{
    _endthreadex((unsigned)val);
}

int erl_thread_join(erl_thread_t tp, void** vp)
{
    DWORD code;
    code = WaitForSingleObject((HANDLE)tp, INFINITE); /* FIX ERRORS */
    CloseHandle(tp);
    return code != WAIT_OBJECT_0;
}

int er_thead_kill(erl_thread_t tp)
{
    return -1;
}
