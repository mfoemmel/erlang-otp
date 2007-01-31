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
 * Description:	Scheduling of port tasks
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_USE_PORT_TASKS
#undef ERL_PORT_TASK_H__
#define ERL_PORT_TASK_H__
#endif

#ifndef ERL_PORT_TASK_H__
#define ERL_PORT_TASK_H__

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#if (defined(ERL_PROCESS_C__) \
     || defined(ERL_PORT_TASK_C__) \
     || defined(ERL_IO_C__) \
     || (ERTS_GLB_INLINE_INCL_FUNC_DEF \
	 && defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)))
#define ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif

#include "erl_sys_driver.h"
#include "erl_smp.h"

typedef enum {
    ERTS_PORT_TASK_FREE,
    ERTS_PORT_TASK_INPUT,
    ERTS_PORT_TASK_OUTPUT,
    ERTS_PORT_TASK_EVENT,
    ERTS_PORT_TASK_TIMEOUT
} ErtsPortTaskType;

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
/* NOTE: Do not access any of the exported variables directly */
extern erts_smp_spinlock_t erts_port_tasks_lock;
extern long erts_port_run_q_len;
extern Port *erts_port_run_q;
extern long erts_port_task_outstanding_io_tasks;
extern int erts_ports_executing;
#endif

typedef erts_smp_atomic_t ErtsPortTaskHandle;
typedef struct ErtsPortTask_ ErtsPortTask;
typedef struct ErtsPortTaskQueue_ ErtsPortTaskQueue;

typedef struct {
    Port *next;
    Port *prev;
    ErtsPortTaskQueue *taskq;
    ErtsPortTaskQueue *exe_taskq;
} ErtsPortTaskSched;

ERTS_GLB_INLINE void erts_port_task_handle_init(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE int erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp);
ERTS_GLB_INLINE void erts_port_task_init_sched(ErtsPortTaskSched *ptpp);
#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
ERTS_GLB_INLINE void erts_smp_tasks_lock(void);
ERTS_GLB_INLINE void erts_smp_tasks_unlock(void);
ERTS_GLB_INLINE long erts_port_task_port_queue_len(void);
ERTS_GLB_INLINE long erts_port_task_ports_executing(void);
ERTS_GLB_INLINE int erts_port_task_have_outstanding_io_tasks(void);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_port_task_handle_init(ErtsPortTaskHandle *pthp)
{
    erts_smp_atomic_init(pthp, (long) NULL);
}

ERTS_GLB_INLINE int
erts_port_task_is_scheduled(ErtsPortTaskHandle *pthp)
{
    return ((void *) erts_smp_atomic_read(pthp)) != NULL;
}

ERTS_GLB_INLINE void
erts_port_task_init_sched(ErtsPortTaskSched *ptsp)
{
    ptsp->next = NULL;
    ptsp->prev = NULL;
    ptsp->taskq = NULL;
    ptsp->exe_taskq = NULL;
}

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

ERTS_GLB_INLINE void
erts_smp_tasks_lock(void)
{
#ifdef ERTS_SMP
    erts_smp_spin_lock(&erts_port_tasks_lock);
#endif
}

ERTS_GLB_INLINE void
erts_smp_tasks_unlock(void)
{
#ifdef ERTS_SMP
    erts_smp_spin_unlock(&erts_port_tasks_lock);
#endif
}

ERTS_GLB_INLINE long
erts_port_task_port_queue_len(void)
{
    long res;
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
    erts_smp_spin_lock(&erts_port_tasks_lock);
    res = erts_port_run_q_len;
    ASSERT(res >= 0);
    erts_smp_spin_unlock(&erts_port_tasks_lock);
    return res;
}

ERTS_GLB_INLINE long
erts_port_task_ports_executing(void)
{
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
    return (long) erts_ports_executing;
}

ERTS_GLB_INLINE int
erts_port_task_have_outstanding_io_tasks(void)
{
    int res;
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
    erts_smp_spin_lock(&erts_port_tasks_lock);
    res = erts_port_task_outstanding_io_tasks != (long) 0;
    erts_smp_spin_unlock(&erts_port_tasks_lock);
    return res;
}

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

#endif

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
int erts_port_task_execute(void);
void erts_port_task_init(void);
#endif

int erts_port_task_abort(ErtsPortTaskHandle *);
int erts_port_task_schedule(Eterm,
			    ErtsPortTaskHandle *,
			    ErtsPortTaskType,
			    ErlDrvEvent,
			    ErlDrvEventData);
void erts_port_task_free_port(Port *);

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif /* ERL_PORT_TASK_H__ */






