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
#include "erl_alloc.h"
#include "erl_driver.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"

struct _thread_data
{
    void* (*func)(void*);
    void* arg;
};

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

int erts_mutex_set_default_atfork(erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_unset_default_atfork(erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_destroy(erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_lock(erts_mutex_t mtx)
{
    return -1;
}

int erts_mutex_unlock(erts_mutex_t mtx)
{
    return -1;
}

erts_cond_t erts_cond_create(void)
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

int erts_cond_broadcast(erts_cond_t cv)
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

int er_thread_kill(erts_thread_t tp)
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
