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

#include "sys.h"
#include "driver.h"

#define MAX_SYS_MUTEX 10

struct _thread_data
{
    void* (*func)(void*);
    void* arg;
};

erl_mutex_t erts_mutex_create()
{
    return NULL;
}

erl_mutex_t erts_mutex_sys(int mno)
{
    return NULL;
}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void))
{
    return -1;
}

int erts_mutex_destroy(erl_mutex_t mtx)
{
    return -1;
}

int erts_mutex_lock (erl_mutex_t mtx)
{
    return -1;
}

int erts_mutex_unlock (erl_mutex_t mtx)
{
    return -1;
}

erl_cond_t erts_cond_create()
{
    return NULL;
}

int erts_cond_destroy(erl_cond_t cv)
{
    return -1;
}

int erts_cond_signal(erl_cond_t cv)
{
    return -1;
}

int erts_cond_broadcast (erl_cond_t cv)
{
    return -1;
}

int erts_cond_wait(erl_cond_t cv, erl_mutex_t mtx)
{
    return -1;
}

int erts_cond_timedwait(erl_cond_t cp, erl_mutex_t mp, long time)
{
    return -1;
}

int erts_thread_create(erl_thread_t* tpp, 
		      void* (*func)(void*),
		      void* arg,
		      int detached)
{
    return -1;
}

erl_thread_t erts_thread_self()
{
    return NULL;
}

void erts_thread_exit(void* val)
{
}

int erts_thread_join(erl_thread_t tp, void** vp)
{
    return -1;
}

int er_thread_kill(erl_thread_t tp)
{
    return -1;
}
