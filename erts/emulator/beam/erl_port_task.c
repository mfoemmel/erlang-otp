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

#define ERL_PORT_TASK_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "erl_port_task.h"

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

#define ERTS_PORT_TASK_INVALID_PORT(P, ID) \
  ((erts_port_status_get((P)) & ERTS_PORT_SFLGS_DEAD) || (P)->id != (ID))

erts_smp_spinlock_t erts_port_tasks_lock;

Port *erts_port_run_q;
static Port *port_run_q_end;
long erts_port_run_q_len;
long erts_port_task_outstanding_io_tasks;
int erts_ports_executing;

struct ErtsPortTaskQueue_ {
    ErtsPortTask *first;
    ErtsPortTask *last;
    Port *port;
};

struct ErtsPortTask_ {
    ErtsPortTask *prev;
    ErtsPortTask *next;
    ErtsPortTaskQueue *queue;
    ErtsPortTaskHandle *handle;
    ErtsPortTaskType type;
    ErlDrvEvent event;
    ErlDrvEventData event_data;
};

#ifdef HARD_DEBUG
#define ERTS_PT_CHK_PORTQ() check_port_queue(NULL, 0)
#define ERTS_PT_CHK_PRES_PORTQ(PP) check_port_queue((PP), -1)
#define ERTS_PT_CHK_IN_PORTQ(PP) check_port_queue((PP), 1)
#define ERTS_PT_CHK_NOT_IN_PORTQ(PP) check_port_queue((PP), 0)
#define ERTS_PT_CHK_TASKQ(Q) check_task_queue((Q), NULL, 0)
#define ERTS_PT_CHK_IN_TASKQ(Q, T) check_task_queue((Q), (T), 1)
#define ERTS_PT_CHK_NOT_IN_TASKQ(Q, T) check_task_queue((Q), (T), 0)
static void
check_port_queue(Port *chk_pp, int inq);
static void
check_task_queue(ErtsPortTaskQueue *ptqp,
		 ErtsPortTask *chk_ptp,
		 int inq);
#else
#define ERTS_PT_CHK_PORTQ()
#define ERTS_PT_CHK_PRES_PORTQ(PP)
#define ERTS_PT_CHK_IN_PORTQ(PP)
#define ERTS_PT_CHK_NOT_IN_PORTQ(PP)
#define ERTS_PT_CHK_TASKQ(Q)
#define ERTS_PT_CHK_IN_TASKQ(Q, T)
#define ERTS_PT_CHK_NOT_IN_TASKQ(Q, T)
#endif

static void handle_remaining_tasks(Port *pp);

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_task,
				 ErtsPortTask,
				 200,
				 ERTS_ALC_T_PORT_TASK)
ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(port_taskq,
				 ErtsPortTaskQueue,
				 50,
				 ERTS_ALC_T_PORT_TASKQ)

/*
 * Task handle manipulation.
 */

static ERTS_INLINE ErtsPortTask *
handle2task(ErtsPortTaskHandle *pthp)
{
    return (ErtsPortTask *) erts_smp_atomic_read(pthp);
}

static ERTS_INLINE void
reset_handle(ErtsPortTask *ptp)
{
    if (ptp->handle) {
	ASSERT(ptp == handle2task(ptp->handle));
	erts_smp_atomic_set(ptp->handle, (long) NULL);
    }
}

static ERTS_INLINE void
set_handle(ErtsPortTask *ptp, ErtsPortTaskHandle *pthp)
{
    ptp->handle = pthp;
    if (pthp) {
	erts_smp_atomic_set(pthp, (long) ptp);
	ASSERT(ptp == handle2task(ptp->handle));
    }
}

/*
 * Port queue operations
 */

static ERTS_INLINE void
enqueue_port(Port *pp)
{
    pp->sched.next = NULL;
    pp->sched.prev = port_run_q_end;
    if (port_run_q_end) {
	ASSERT(erts_port_run_q);
	port_run_q_end->sched.next = pp;
    }
    else {
	ASSERT(!erts_port_run_q);
	erts_port_run_q = pp;
    }

    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked()); /* for increase in len */
    erts_port_run_q_len++;
    port_run_q_end = pp;
    ASSERT(erts_port_run_q && port_run_q_end);
}

static ERTS_INLINE void
dequeue_port(Port *pp)
{
    if (pp->sched.next)
	pp->sched.next->sched.prev = pp->sched.prev;
    else {
	ASSERT(port_run_q_end == pp);
	port_run_q_end = pp->sched.prev;
    }
    if (pp->sched.prev)
	pp->sched.prev->sched.next = pp->sched.next;
    else {
	ASSERT(erts_port_run_q == pp);
	erts_port_run_q = pp->sched.next;
    }

    erts_port_run_q_len--;
    ASSERT(erts_port_run_q || !port_run_q_end);
    ASSERT(port_run_q_end || !erts_port_run_q);
}

static ERTS_INLINE Port *
pop_port(void)
{
    Port *pp = erts_port_run_q;
    if (!pp) {
	ASSERT(!port_run_q_end);
    }
    else {
	erts_port_run_q = erts_port_run_q->sched.next;
	if (erts_port_run_q)
	    erts_port_run_q->sched.prev = NULL;
	else {
	    ASSERT(port_run_q_end == pp);
	    port_run_q_end = NULL;
	}
	erts_port_run_q_len--;
    }

    ASSERT(erts_port_run_q || !port_run_q_end);
    ASSERT(port_run_q_end || !erts_port_run_q);
    return pp;
}


#ifdef HARD_DEBUG

static void
check_port_queue(Port *chk_pp, int inq)
{
    Port *pp;
    Port *last_pp;
    Port *first_pp = erts_port_run_q;
    int no_forward = 0, no_backward = 0;
    int found_forward = 0, found_backward = 0;
    if (!first_pp) {
	ASSERT(!port_run_q_end);
    }
    else {
	ASSERT(!first_pp->sched.prev);
	for (pp = first_pp; pp; pp = pp->sched.next) {
	    ASSERT(pp->sched.taskq);
	    if (pp->sched.taskq->first)
		no_forward++;
	    if (chk_pp == pp)
		found_forward = 1;
	    if (!pp->sched.prev) {
		ASSERT(first_pp == pp);
	    }
	    if (!pp->sched.next) {
		ASSERT(port_run_q_end == pp);
		last_pp = pp;
	    }
	}
	for (pp = last_pp; pp; pp = pp->sched.prev) {
	    ASSERT(pp->sched.taskq);
	    if (pp->sched.taskq->last)
		no_backward++;
	    if (chk_pp == pp)
		found_backward = 1;
	    if (!pp->sched.prev) {
		ASSERT(first_pp == pp);
	    }
	    if (!pp->sched.next) {
		ASSERT(port_run_q_end == pp);
	    }
	    check_task_queue(pp->sched.taskq, NULL, 0);
	}
	ASSERT(no_forward == no_backward);
    }
    ASSERT(no_forward == erts_port_run_q_len);
    if (chk_pp) {
	if (chk_pp->sched.taskq || chk_pp->sched.exe_taskq) {
	    ASSERT(chk_pp->sched.taskq != chk_pp->sched.exe_taskq);
	}
	ASSERT(!chk_pp->sched.taskq || chk_pp->sched.taskq->first);
	if (inq < 0)
	    inq = chk_pp->sched.taskq && !chk_pp->sched.exe_taskq;
	if (inq) {
	    ASSERT(found_forward && found_backward);
	}
	else {
	    ASSERT(!found_forward && !found_backward);
	}
    }
}

#endif

/*
 * Task queue operations
 */

static ERTS_INLINE ErtsPortTaskQueue *
port_taskq_init(ErtsPortTaskQueue *ptqp, Port *pp)
{
    if (ptqp) {
	ptqp->first = NULL;
	ptqp->last = NULL;
	ptqp->port = pp;
    }
    return ptqp;
}

static ERTS_INLINE void
enqueue_task(ErtsPortTaskQueue *ptqp, ErtsPortTask *ptp)
{
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    ptp->next = NULL;
    ptp->prev = ptqp->last;
    ptp->queue = ptqp;
    if (ptqp->last) {
	ASSERT(ptqp->first);
	ptqp->last->next = ptp;
    }
    else {
	ASSERT(!ptqp->first);
	ptqp->first = ptp;
    }
    ptqp->last = ptp;
    ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
}

static ERTS_INLINE void
push_task(ErtsPortTaskQueue *ptqp, ErtsPortTask *ptp)
{
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    ptp->next = ptqp->first;
    ptp->prev = NULL;
    ptp->queue = ptqp;
    if (ptqp->first) {
	ASSERT(ptqp->last);
	ptqp->first->prev = ptp;
    }
    else {
	ASSERT(!ptqp->last);
	ptqp->last = ptp;
    }
    ptqp->first = ptp;
    ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
}

static ERTS_INLINE void
dequeue_task(ErtsPortTask *ptp)
{
    ASSERT(ptp);
    ASSERT(ptp->queue);
    ERTS_PT_CHK_IN_TASKQ(ptp->queue, ptp);
    if (ptp->next)
	ptp->next->prev = ptp->prev;
    else {
	ASSERT(ptp->queue->last == ptp);
	ptp->queue->last = ptp->prev;
    }
    if (ptp->prev)
	ptp->prev->next = ptp->next;
    else {
	ASSERT(ptp->queue->first == ptp);
	ptp->queue->first = ptp->next;
    }

    ASSERT(ptp->queue->first || !ptp->queue->last);
    ASSERT(ptp->queue->last || !ptp->queue->first);
    ERTS_PT_CHK_NOT_IN_TASKQ(ptp->queue, ptp);
}

static ERTS_INLINE ErtsPortTask *
pop_task(ErtsPortTaskQueue *ptqp)
{
    ErtsPortTask *ptp = ptqp->first;
    if (!ptp) {
	ASSERT(!ptqp->last);
    }
    else {
	ERTS_PT_CHK_IN_TASKQ(ptqp, ptp);
	ASSERT(!ptp->prev);
	ptqp->first = ptp->next;
	if (ptqp->first)
	    ptqp->first->prev = NULL;
	else {
	    ASSERT(ptqp->last == ptp);
	    ptqp->last = NULL;
	}
	ASSERT(ptp->queue->first || !ptp->queue->last);
	ASSERT(ptp->queue->last || !ptp->queue->first);
    }
    ERTS_PT_CHK_NOT_IN_TASKQ(ptqp, ptp);
    return ptp;
}

#ifdef HARD_DEBUG

static void
check_task_queue(ErtsPortTaskQueue *ptqp,
		 ErtsPortTask *chk_ptp,
		 int inq)
{
    ErtsPortTask *ptp;
    ErtsPortTask *last_ptp;
    ErtsPortTask *first_ptp = ptqp->first;
    int found_forward = 0, found_backward = 0;
    if (!first_ptp) {
	ASSERT(!ptqp->last);
    }
    else {
	ASSERT(!first_ptp->prev);
	for (ptp = first_ptp; ptp; ptp = ptp->next) {
	    ASSERT(ptp->queue == ptqp);
	    if (chk_ptp == ptp)
		found_forward = 1;
	    if (!ptp->prev) {
		ASSERT(first_ptp == ptp);
	    }
	    if (!ptp->next) {
		ASSERT(ptqp->last == ptp);
		last_ptp = ptp;
	    }
	}
	for (ptp = last_ptp; ptp; ptp = ptp->prev) {
	    ASSERT(ptp->queue == ptqp);
	    if (chk_ptp == ptp)
		found_backward = 1;
	    if (!ptp->prev) {
		ASSERT(first_ptp == ptp);
	    }
	    if (!ptp->next) {
		ASSERT(ptqp->last == ptp);
	    }
	}
    }
    if (chk_ptp) {
	if (inq) {
	    ASSERT(found_forward && found_backward);
	}
	else {
	    ASSERT(!found_forward && !found_backward);
	}
    }
}
#endif

/*
 * Abort a scheduled task.
 */

int
erts_port_task_abort(ErtsPortTaskHandle *pthp)
{
    ErtsPortTaskQueue *ptqp;
    ErtsPortTask *ptp;
    Port *pp;
    int port_is_dequeued = 0;

    erts_smp_tasks_lock();

    ptp = handle2task(pthp);

    if (!ptp) {
	erts_smp_tasks_unlock();
	return 1;
    }

    ASSERT(ptp->handle == pthp);
    ptqp = ptp->queue;
    pp = ptqp->port;

    ERTS_PT_CHK_PRES_PORTQ(pp);
    ASSERT(ptqp);
    ASSERT(ptqp->first);

    dequeue_task(ptp);
    reset_handle(ptp);

    switch (ptp->type) {
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT:
    case ERTS_PORT_TASK_EVENT:
	ASSERT(erts_port_task_outstanding_io_tasks > 0);
	erts_port_task_outstanding_io_tasks--;
	break;
    default:
	break;
    }

    ASSERT(ptqp == pp->sched.taskq || ptqp == pp->sched.exe_taskq);

    if (ptqp->first || pp->sched.taskq != ptqp)
	ptqp = NULL;
    else {
	pp->sched.taskq = NULL;
	if (!pp->sched.exe_taskq) {
	    dequeue_port(pp);
	    port_is_dequeued = 1;
	}
    }

    ERTS_PT_CHK_PRES_PORTQ(pp);

    erts_smp_tasks_unlock();
    if (erts_system_profile_flags.runnable_ports && port_is_dequeued) {
    	profile_runnable_port(pp, am_inactive);
    }

    port_task_free(ptp);
    if (ptqp)
	port_taskq_free(ptqp);

    return 0;
}

/*
 * Schedule a task.
 */

int
erts_port_task_schedule(Eterm id,
			ErtsPortTaskHandle *pthp,
			ErtsPortTaskType type,
			ErlDrvEvent event,
			ErlDrvEventData event_data)
{
    Port *pp;
    ErtsPortTaskQueue *ptqp = NULL;
    ErtsPortTask *ptp;
    int sched_lock = 0, enq_port = 0;

    /*
     * NOTE:	We might not have the port lock here. We are only
     *		allowed to access the 'sched', 'tab_status',
     *          and 'id' fields of the port struct while
     *          tasks_lock is held.
     */

    if (pthp && erts_port_task_is_scheduled(pthp)) {
	ASSERT(0);
	erts_port_task_abort(pthp);
    }

    ptp = port_task_alloc();

    erts_smp_tasks_lock();

    ASSERT(is_internal_port(id));
    pp = &erts_port[internal_port_index(id)];

    if (ERTS_PORT_TASK_INVALID_PORT(pp, id)) {
	erts_smp_tasks_unlock();
	return -1;
    }

    ASSERT(!erts_port_task_is_scheduled(pthp));

    ERTS_PT_CHK_PRES_PORTQ(pp);
#ifdef DEBUG
    /* If we have a taskq and not executing, we should be in port run q */  
    if (pp->sched.taskq && !pp->sched.exe_taskq) {
	ASSERT(pp->sched.prev || erts_port_run_q == pp);
    }
#endif

    if (!pp->sched.taskq) {
	erts_smp_tasks_unlock();
	ptqp = port_taskq_init(port_taskq_alloc(), pp);
	erts_smp_sched_lock();
	sched_lock = 1;
	erts_smp_tasks_lock();

	/* Port might have died while tasks_lock was unlocked... */
	if (ERTS_PORT_TASK_INVALID_PORT(pp, id)) {
	    erts_smp_tasks_unlock();
	    erts_smp_sched_unlock();
	    port_task_free(ptp);
	    port_taskq_free(ptqp);
	    return -1;
	}

	/* A queue might have appeared while tasks_lock was unlocked... */
	if (!pp->sched.taskq) {
	    pp->sched.taskq = ptqp;
	    enq_port = !pp->sched.exe_taskq;
	    ptqp = NULL;
	}
    }

    ASSERT(pp->sched.taskq);
    ASSERT(ptp);

    ptp->type = type;
    ptp->event = event;
    ptp->event_data = event_data;

    set_handle(ptp, pthp);

    switch (type) {
    case ERTS_PORT_TASK_FREE:
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_port_task_schedule(): Cannot schedule free task\n");
	break;
    case ERTS_PORT_TASK_INPUT:
    case ERTS_PORT_TASK_OUTPUT:
    case ERTS_PORT_TASK_EVENT:
	erts_port_task_outstanding_io_tasks++;
	/* Fall through... */
    default:
	enqueue_task(pp->sched.taskq, ptp);
	break;
    }

#if defined(HARD_DEBUG)
    if (pp->sched.exe_taskq || enq_port)
	ERTS_PT_CHK_NOT_IN_PORTQ(pp);
    else
	ERTS_PT_CHK_IN_PORTQ(pp);
#elif defined(DEBUG)
    if (!enq_port && !pp->sched.exe_taskq) {
	/* We should be in port run q */
	ASSERT(pp->sched.prev || erts_port_run_q == pp);
    }
#endif

    if (!sched_lock) {
	ERTS_PT_CHK_PRES_PORTQ(pp);
	erts_smp_tasks_unlock();
    }
    else {
	if (!enq_port) {
	    ERTS_PT_CHK_PRES_PORTQ(pp);
	    erts_smp_tasks_unlock();
	}
	else {
	    enqueue_port(pp);
	    ERTS_PT_CHK_PRES_PORTQ(pp);
	    erts_smp_tasks_unlock();
	    
	    if (erts_system_profile_flags.runnable_ports) {
    		profile_runnable_port(pp, am_active);
    	    }
	    
	    erts_smp_notify_inc_runq();
	}
	erts_smp_sched_unlock();
    }

    if (ptqp)
	port_taskq_free(ptqp);
    return 0;
}

void
erts_port_task_free_port(Port *pp)
{
    int port_is_dequeued = 0;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
    ASSERT(!(pp->status & ERTS_PORT_SFLGS_DEAD));
    erts_smp_sched_lock();
    erts_smp_tasks_lock();
    ERTS_PT_CHK_PRES_PORTQ(pp);
    if (pp->sched.exe_taskq) {
	/* I (this thread) am currently executing this port, free it
	   when scheduled out... */
	ErtsPortTask *ptp;
	erts_smp_tasks_unlock();
	ptp = port_task_alloc();
	erts_smp_tasks_lock();
	erts_smp_port_tab_lock();
	ASSERT(erts_ports_alive > 0);
	erts_ports_alive--;
	pp->status &= ~ERTS_PORT_SFLG_CLOSING;
	pp->status |= ERTS_PORT_SFLG_FREE_SCHEDULED;
	erts_smp_port_tab_unlock();
	ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 1);
	ptp->type = ERTS_PORT_TASK_FREE;
	ptp->event = (ErlDrvEvent) -1;
	ptp->event_data = NULL;
	set_handle(ptp, NULL);
	push_task(pp->sched.exe_taskq, ptp);
	ERTS_PT_CHK_PRES_PORTQ(pp);
	erts_smp_tasks_unlock();
	erts_smp_sched_unlock();
    }
    else {
	ErtsPortTaskQueue *ptqp = pp->sched.taskq;
	if (ptqp) {
	    dequeue_port(pp);
	    port_is_dequeued = 1;
	}
	erts_smp_port_tab_lock();
	erts_ports_alive--;
	pp->status &= ~ERTS_PORT_SFLG_CLOSING;
	pp->status |= ERTS_PORT_SFLG_FREE_SCHEDULED;
	erts_smp_port_tab_unlock();
#ifdef ERTS_SMP
	erts_smp_atomic_dec(&pp->refc); /* Not alive */
#endif
	ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 0); /* Lock */
	erts_smp_sched_unlock();
	handle_remaining_tasks(pp); /* May release tasks lock */
	ASSERT(!pp->sched.exe_taskq && (!ptqp || !ptqp->first));
	pp->sched.taskq = NULL;
	ERTS_PT_CHK_PRES_PORTQ(pp);
#ifndef ERTS_SMP
	ASSERT(pp->status & ERTS_PORT_SFLG_PORT_DEBUG);
	erts_port_status_set(pp, ERTS_PORT_SFLG_FREE);
#endif
	erts_smp_tasks_unlock();
	if (erts_system_profile_flags.runnable_ports && port_is_dequeued) {
    	    profile_runnable_port(pp, am_inactive);
    	}

	if (ptqp)
	    port_taskq_free(ptqp);
    }
}

static void
prepare_for_block(void *unused)
{
    erts_smp_sched_unlock();
}

static void
resume_after_block(void *vresp)
{
    erts_smp_sched_lock();
    if (vresp) {
	erts_smp_tasks_lock();
	*((int *) vresp) = erts_port_task_outstanding_io_tasks != (long) 0;
	erts_smp_tasks_unlock();
    }
}

/*
 * Run all scheduled tasks for the first port in run queue. If
 * new tasks appear while running reschedule port (free task is
 * an exception; it is always handled instantly).
 *
 * erts_port_task_execute() is called by scheduler threads between
 * scheduleing of processes. Sched lock should be held by caller.
 */

int
erts_port_task_execute(Port **curr_port_pp)
{
    int port_was_enqueued = 0;
    Port *pp;
    ErtsPortTaskQueue *ptqp;
    ErtsPortTask *ptp;
    int res = 0;
    long io_tasks_executed = 0;

    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());

    erts_smp_activity_begin(ERTS_ACTIVITY_IO,
			    prepare_for_block,
			    resume_after_block,
			    NULL);

    erts_smp_tasks_lock();
    ERTS_PT_CHK_PORTQ();

    ASSERT(erts_port_run_q_len
	   || erts_ports_executing
	   || !erts_port_task_outstanding_io_tasks);

    pp = pop_port();
    if (!pp) {
	erts_smp_tasks_unlock();
	res = 0;
	goto done;
    }

    *curr_port_pp = pp;

    ASSERT(pp->sched.taskq);
    ASSERT(pp->sched.taskq->first);
    ptqp = pp->sched.taskq;
    pp->sched.taskq = NULL;

    ASSERT(!pp->sched.exe_taskq);
    pp->sched.exe_taskq = ptqp;

    erts_ports_executing++;
    erts_smp_tasks_unlock();

    erts_smp_sched_unlock();

    erts_smp_port_lock(pp);
    
    /* trace port scheduling, in */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
	trace_sched_ports(pp, am_in);
    }

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

    erts_smp_tasks_lock();

    ERTS_PT_CHK_PRES_PORTQ(pp);
    ptp = pop_task(ptqp);

    erts_block_fpe();

    while (ptp) {
	ASSERT(pp->sched.taskq != pp->sched.exe_taskq);

	reset_handle(ptp);
	erts_smp_tasks_unlock();

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));
	ERTS_SMP_CHK_NO_PROC_LOCKS;
	ASSERT(pp->drv_ptr);

	switch (ptp->type) {
	case ERTS_PORT_TASK_FREE: /* May be pushed in q at any time */
	    erts_smp_tasks_lock();
	    if (io_tasks_executed) {
		ASSERT(erts_port_task_outstanding_io_tasks >= io_tasks_executed);
		erts_port_task_outstanding_io_tasks -= io_tasks_executed;
	    }
	    erts_unblock_fpe();
	    goto free_port;
	case ERTS_PORT_TASK_TIMEOUT:
	    if (!(pp->status & ERTS_PORT_SFLGS_DEAD))
		(*pp->drv_ptr->timeout)((ErlDrvData) pp->drv_data);
	    break;
	case ERTS_PORT_TASK_INPUT:
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    /* NOTE some windows drivers use ->ready_input for input and output */
	    (*pp->drv_ptr->ready_input)((ErlDrvData) pp->drv_data, ptp->event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_OUTPUT:
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    (*pp->drv_ptr->ready_output)((ErlDrvData) pp->drv_data, ptp->event);
	    io_tasks_executed++;
	    break;
	case ERTS_PORT_TASK_EVENT:
	    ASSERT((pp->status & ERTS_PORT_SFLGS_DEAD) == 0);
	    (*pp->drv_ptr->event)((ErlDrvData) pp->drv_data, ptp->event, ptp->event_data);
	    io_tasks_executed++;
	    break;
	default:
	    erl_exit(ERTS_ABORT_EXIT,
		     "Invalid port task type: %d\n",
		     (int) ptp->type);
	    break;
	}

	if ((pp->status & ERTS_PORT_SFLG_CLOSING) && erts_is_port_ioq_empty(pp))
	    erts_terminate_port(pp);

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

#ifdef ERTS_SMP
	if (pp->xports)
	    erts_smp_xports_unlock(pp);
	ASSERT(!pp->xports);
#endif

	ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

	port_task_free(ptp);

	erts_smp_tasks_lock();

	ptp = pop_task(ptqp);
    }

    erts_unblock_fpe();

    if (io_tasks_executed) {
	ASSERT(erts_port_task_outstanding_io_tasks >= io_tasks_executed);
	erts_port_task_outstanding_io_tasks -= io_tasks_executed;
    }

    if (erts_smp_sched_trylock() == EBUSY) {
	erts_smp_tasks_unlock();
	erts_smp_sched_lock();
	erts_smp_tasks_lock();
	ptp = pop_task(ptqp);
	if (ptp) {
	    ErtsPortTaskQueue *ptq2p;
	    /*
	     * A free task was added to the queue while
	     * we had the task lock unlocked; handle it...
	     */
	    ASSERT(ptp->type == ERTS_PORT_TASK_FREE);
	    erts_smp_sched_unlock();

	free_port:
	    ASSERT(pp->status & ERTS_PORT_SFLG_FREE_SCHEDULED);
	    if (ptqp->first || (pp->sched.taskq && pp->sched.taskq->first))
		handle_remaining_tasks(pp);
	    ASSERT(!ptqp->first
		   && (!pp->sched.taskq || !pp->sched.taskq->first));
#ifdef ERTS_SMP
	    erts_smp_atomic_dec(&pp->refc); /* Not alive */
	    ERTS_SMP_LC_ASSERT(erts_smp_atomic_read(&pp->refc) > 0); /* Lock */
#else
	    erts_port_status_bor_set(pp, ERTS_PORT_SFLG_FREE);
#endif
	    ptq2p = pp->sched.taskq;	
	    pp->sched.taskq = NULL;
	    erts_smp_tasks_unlock();

	    port_task_free(ptp);
	    if (ptq2p)
		port_taskq_free(ptq2p);

	    erts_smp_sched_lock();
	    erts_smp_tasks_lock();
	}
    }

    *curr_port_pp = NULL;

    if (pp->sched.taskq) {
	ASSERT(!(pp->status & ERTS_PORT_SFLGS_DEAD));
	ASSERT(pp->sched.taskq->first);
	enqueue_port(pp);
	port_was_enqueued = 1;

	/* 
	   erts_smp_notify_inc_runq();

	 * No need to notify schedulers about the increase in run
	 * queue length since at least this thread, which is a
	 * scheduler, will discover that the port run queue isn't
	 * empty before trying to go to sleep.
	 */
    }

    ASSERT(erts_ports_executing > 0);
    erts_ports_executing--;

    ASSERT(pp->sched.exe_taskq);
    pp->sched.exe_taskq = NULL;

    res = erts_port_task_outstanding_io_tasks != (long) 0;

    ERTS_PT_CHK_PRES_PORTQ(pp);
    erts_smp_tasks_unlock();

    port_taskq_free(ptqp);

    if (erts_system_profile_flags.runnable_ports && (port_was_enqueued != 1)) {
    	profile_runnable_port(pp, am_inactive);
    }

    /* trace port scheduling, out */
    if (IS_TRACED_FL(pp, F_TRACE_SCHED_PORTS)) {
    	trace_sched_ports(pp, am_out);
    }
#ifndef ERTS_SMP
    erts_port_release(pp);
#else
    {
	long refc = erts_smp_atomic_dectest(&pp->refc);
	ASSERT(refc >= 0);
	if (refc > 0)
	    erts_smp_mtx_unlock(pp->lock);
	else {
	    erts_smp_sched_unlock();
	    erts_port_cleanup(pp); /* Might aquire sched lock */
	    erts_smp_sched_lock();
	    erts_smp_tasks_lock();
	    res = erts_port_task_outstanding_io_tasks != (long) 0;
	    erts_smp_tasks_unlock();
	}
    }
#endif

 done:

    erts_smp_activity_end(ERTS_ACTIVITY_IO,
			  prepare_for_block,
			  resume_after_block,
			  (void *) &res);
    return res;
}

/*
 * Handle remaining tasks after a free task.
 */

static void
handle_remaining_tasks(Port *pp)
{
    int i;
    ErtsPortTask *ptp;
    ErtsPortTaskQueue *ptqps[] = {pp->sched.exe_taskq, pp->sched.taskq};

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(pp));

    for (i = 0; i < sizeof(ptqps)/sizeof(ErtsPortTaskQueue *); i++) {
	if (!ptqps[i])
	    continue;

	ptp = pop_task(ptqps[i]);
	while (ptp) {
	    reset_handle(ptp);
	    erts_smp_tasks_unlock();

	    switch (ptp->type) {
	    case ERTS_PORT_TASK_FREE:
	    case ERTS_PORT_TASK_TIMEOUT:
		break;
	    case ERTS_PORT_TASK_INPUT:
		erts_stale_drv_select(pp->id, ptp->event, DO_READ, 1);
		break;
	    case ERTS_PORT_TASK_OUTPUT:
		erts_stale_drv_select(pp->id, ptp->event, DO_WRITE, 1);
		break;
	    case ERTS_PORT_TASK_EVENT:
		erts_stale_drv_select(pp->id, ptp->event, 0, 1);
		break;
	    default:
		erl_exit(ERTS_ABORT_EXIT,
			 "Invalid port task type: %d\n",
			 (int) ptp->type);
	    }

	    port_task_free(ptp);

	    erts_smp_tasks_lock();
	    ptp = pop_task(ptqps[i]);
	}
    }

    ASSERT(!pp->sched.taskq);
}

int
erts_port_is_scheduled(Port *pp)
{
    int res;
    erts_smp_spin_lock(&erts_port_tasks_lock);
    res = pp->sched.taskq || pp->sched.exe_taskq;
    erts_smp_spin_unlock(&erts_port_tasks_lock);
    return res;
}

/*
 * Initialize the module.
 */

void
erts_port_task_init(void)
{
    erts_port_task_outstanding_io_tasks = (long) 0;
    erts_smp_spinlock_init(&erts_port_tasks_lock, "port_tasks_lock");
    init_port_task_alloc();
    init_port_taskq_alloc();
    erts_port_run_q = NULL;
    port_run_q_end = NULL;
    erts_port_run_q_len = 0;
    erts_ports_executing = 0;
}
