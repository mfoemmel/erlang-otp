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

#define ERL_PROCESS_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_nmgc.h"
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_instrument.h"
#include "erl_threads.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_init_process() */
#include "hipe_signal.h"	/* for hipe_thread_signal_init() */
#endif

#define MAX_BIT       (1 << PRIORITY_MAX)
#define HIGH_BIT      (1 << PRIORITY_HIGH)
#define NORMAL_BIT    (1 << PRIORITY_NORMAL)
#define LOW_BIT       (1 << PRIORITY_LOW)

#define	DECR_PROC_COUNT(prio)               \
    if ((prio) == PRIORITY_LOW) {           \
        if (--queued_low < 1) {             \
	   ASSERT(queued_low == 0);         \
	   qmask &= ~(1 << PRIORITY_LOW);   \
        }                                   \
    } else if ((prio) == PRIORITY_NORMAL) { \
	if (--queued_normal < 1) {          \
	   ASSERT(queued_normal == 0);      \
	   qmask &= ~(1 << PRIORITY_NORMAL);\
        }                                   \
    }				            

#define ASSERT_NORMAL_Q_EMPTY()                       \
    ASSERT((((qmask >> PRIORITY_LOW) & 1) == 0) &&    \
	   (((qmask >> PRIORITY_NORMAL) & 1) == 0) && \
           (queued_low == 0) &&                       \
           (queued_normal == 0))

extern Eterm beam_apply[];
extern Eterm beam_exit[];

static Sint p_last;
static Sint p_next;
static Sint p_serial;
static Uint p_serial_mask;
static Uint p_serial_shift;

Uint erts_no_of_schedulers;
#ifdef ERTS_SMP
Uint used_schedulers;
erts_smp_mtx_t msched_blk_mtx;
erts_smp_cnd_t msched_blk_cnd;
#endif
Uint erts_max_processes = ERTS_DEFAULT_MAX_PROCESSES;
Uint erts_process_tab_index_mask;

erts_smp_atomic_t erts_tot_proc_mem; /* in bytes */

int block_multi_scheduling;
int is_setting_block_multi_scheduling;
ProcessList *block_multi_scheduling_procs;

#ifdef USE_THREADS
static erts_tsd_key_t sched_data_key;
#endif

erts_smp_mtx_t schdlq_mtx;
static erts_smp_mtx_t proc_tab_mtx;

#ifdef ERTS_SMP
static erts_smp_cnd_t schdlq_cnd;
int erts_all_schedulers_waiting;
#ifndef ERTS_SMP_USE_IO_THREAD
static int doing_sys_schedule;
static int waiting_in_sys_schedule = 0;
#endif
#endif
#ifndef ERTS_SMP_USE_IO_THREAD
static int function_calls = 0;
#endif

#ifdef ERTS_SMP
erts_proc_lock_t erts_proc_locks[ERTS_PROC_LOCKS_NO_OF];

static ErtsSchedulerData *schedulers;
static Uint schedulers_waiting_on_runq;
static ProcessList *pending_exiters;

#ifdef ERTS_ENABLE_LOCK_CHECK
static struct {
    Sint16 proc_lock_main;
    Sint16 proc_lock_link;
    Sint16 proc_lock_msgq;
    Sint16 proc_lock_status;
} lc_id;
#endif
#else /* !ERTS_SMP */
ErtsSchedulerData erts_scheduler_data;
#endif

static void init_sched_thr_data(ErtsSchedulerData *esdp, Uint id);

typedef struct schedule_q {
    Process* first;
    Process* last;
} ScheduleQ;

/* we use the same queue for low and normal prio processes */
static ScheduleQ queue[NPRIORITY_LEVELS-1];
static unsigned qmask;

static Uint queued_low;
static Uint queued_normal;
static Sint runq_len;

#ifndef BM_COUNTERS
static int processes_busy;
#endif


Process**  process_tab;
static Uint context_switches;		/* no of context switches */
static Uint reductions;		/* total number of reductions */
static Uint last_reds;
static Uint last_exact_reds;
Uint erts_default_process_flags;
Eterm erts_system_monitor;
Eterm erts_system_monitor_long_gc;
Eterm erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t erts_system_monitor_flags;

#ifdef HYBRID
Uint erts_num_active_procs;
Process** erts_active_procs;
#endif

static erts_smp_atomic_t process_count;

#define ERTS_MAX_MISC_OPS 5

typedef struct ErtsMiscOpList_ ErtsMiscOpList;
struct ErtsMiscOpList_ {
    ErtsMiscOpList *next;
    void (*func)(void *arg);
    void *arg;
};

static ErtsMiscOpList *misc_op_queue;
static ErtsMiscOpList *misc_op_queue_end;

ERTS_QUALLOC_IMPL(misc_op_list, ErtsMiscOpList, 10, ERTS_ALC_T_MISC_OP_LIST)

/*
 * Local functions.
 */
static void exec_misc_ops(void);
static void print_function_from_pc(int to, void *to_arg, Eterm* x);
static int stack_element_dump(int to, void *to_arg, Process* p, Eterm* sp,
			      int yreg);
#ifdef ERTS_SMP
static void handle_pending_exiters(ProcessList *);
#endif

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)

void
erts_proc_lc_lock(Process *p, Uint32 locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_lock(&lck);
    }
}

void
erts_proc_lc_trylock(Process *p, Uint32 locks, int locked)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_trylock(locked, &lck);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_trylock(locked, &lck);
    }
}

void
erts_proc_lc_unlock(Process *p, Uint32 locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_unlock(&lck);
    }
}

int
erts_proc_lc_trylock_force_busy(Process *p, Uint32 locks)
{
    if (locks & ERTS_PROC_LOCKS_ALL) {
	erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					       p->id,
					       ERTS_LC_FLG_LT_PROCLOCK);

	if (locks & ERTS_PROC_LOCK_MAIN)
	    lck.id = lc_id.proc_lock_main;
	else if (locks & ERTS_PROC_LOCK_LINK)
	    lck.id = lc_id.proc_lock_link;
	else if (locks & ERTS_PROC_LOCK_MSGQ)
	    lck.id = lc_id.proc_lock_msgq;
	else if (locks & ERTS_PROC_LOCK_STATUS)
	    lck.id = lc_id.proc_lock_status;
	else
	    erts_lc_fail("Unknown proc lock found");

	return erts_lc_trylock_force_busy(&lck);
    }
    return 0;
}

void erts_proc_lc_chk_only_proc_main(Process *p)
{
    erts_lc_lock_t proc_main = ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK);
    erts_lc_check_exact(&proc_main, 1);
}

#define ERTS_PROC_LC_EMPTY_LOCK_INIT \
  ERTS_LC_LOCK_INIT(-1, THE_NON_VALUE, ERTS_LC_FLG_LT_PROCLOCK)

void
erts_proc_lc_chk_have_proc_locks(Process *p, Uint32 locks)
{
    int have_locks_len = 0;
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->id;
    }

    erts_lc_check(have_locks, have_locks_len, NULL, 0);
}

void
erts_proc_lc_chk_proc_locks(Process *p, Uint32 locks)
{
    int have_locks_len = 0;
    int have_not_locks_len = 0;
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    erts_lc_lock_t have_not_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT};

    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_main;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_link;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_msgq;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_status;
	have_not_locks[have_not_locks_len++].extra = p->id;
    }

    erts_lc_check(have_locks, have_locks_len,
		  have_not_locks, have_not_locks_len);
}

Uint32
erts_proc_lc_my_proc_locks(Process *p)
{
    int resv[4];
    erts_lc_lock_t locks[4] = {ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_link,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_msgq,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_status,
						 p->id,
						 ERTS_LC_FLG_LT_PROCLOCK)};

    Uint32 res = 0;

    erts_lc_have_locks(resv, locks, 4);
    if (resv[0])
	res |= ERTS_PROC_LOCK_MAIN;
    if (resv[1])
	res |= ERTS_PROC_LOCK_LINK;
    if (resv[2])
	res |= ERTS_PROC_LOCK_MSGQ;
    if (resv[3])
	res |= ERTS_PROC_LOCK_STATUS;

    return res;
}

void
erts_proc_lc_chk_no_proc_locks(char *file, int line)
{
    int resv[4];
    int ids[4] = {lc_id.proc_lock_main,
		  lc_id.proc_lock_link,
		  lc_id.proc_lock_msgq,
		  lc_id.proc_lock_status};
    erts_lc_have_lock_ids(resv, ids, 4);
    if (resv[0] || resv[1] || resv[2] || resv[3]) {
	erts_lc_fail("%s:%d: Thread has process locks locked when expected "
		     "not to have any process locks locked",
		     file, line);
    }
}

int erts_smp_is_sched_locked(void)
{
    return erts_smp_lc_mtx_is_locked(&schdlq_mtx);
}

#endif /* #if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK) */

void
erts_pre_init_process(void)
{
#ifdef USE_THREADS
    erts_tsd_key_create(&sched_data_key);
#endif    
#ifdef ERTS_SMP
    pending_exiters = NULL;
#endif
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
    lc_id.proc_lock_main	= erts_lc_get_lock_order_id("proc_main");
    lc_id.proc_lock_link	= erts_lc_get_lock_order_id("proc_link");
    lc_id.proc_lock_msgq	= erts_lc_get_lock_order_id("proc_msgq");
    lc_id.proc_lock_status	= erts_lc_get_lock_order_id("proc_status");
#endif
}

/* initialize the scheduler */
void
erts_init_process(void)
{
    int i;
    Uint proc_bits = ERTS_PROC_BITS;
#ifndef ERTS_SMP
    ErtsSchedulerData *esdp;
#endif

#ifdef ERTS_USE_PORT_TASKS
    erts_port_task_init();
#endif

    init_misc_op_list_alloc();

    misc_op_queue = NULL;
    misc_op_queue_end = NULL;

    erts_smp_atomic_init(&process_count, 0);

    if (erts_use_r9_pids_ports) {
	proc_bits = ERTS_R9_PROC_BITS;
	ASSERT(erts_max_processes <= (1 << ERTS_R9_PROC_BITS));
    }

    erts_smp_atomic_init(&erts_tot_proc_mem, 0L);

    process_tab = (Process**) erts_alloc(ERTS_ALC_T_PROC_TABLE,
					 erts_max_processes*sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    sys_memzero(process_tab, erts_max_processes * sizeof(Process*));
#ifdef HYBRID
    erts_active_procs = (Process**)
        erts_alloc(ERTS_ALC_T_ACTIVE_PROCS,
                   erts_max_processes * sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    erts_num_active_procs = 0;
#endif

#ifndef ERTS_SMP_USE_IO_THREAD
    function_calls = 0;
#endif

    block_multi_scheduling = 0;
    is_setting_block_multi_scheduling = 0;
    block_multi_scheduling_procs = NULL;

#ifdef ERTS_SMP
    erts_no_of_schedulers = 0;
    used_schedulers = 0;
    erts_smp_mtx_init(&msched_blk_mtx, "multi_scheduling_block");
    erts_smp_cnd_init(&msched_blk_cnd);
    erts_all_schedulers_waiting = 0;
#ifndef ERTS_SMP_USE_IO_THREAD
    doing_sys_schedule = 0;
    waiting_in_sys_schedule = 0;
#endif
    erts_smp_mtx_init(&schdlq_mtx, "schdlq");
    erts_smp_cnd_init(&schdlq_cnd);

    schedulers_waiting_on_runq = 0;

    schedulers = NULL;

    for (i = 0; i < ERTS_PROC_LOCKS_NO_OF; i++) {
	erts_smp_mtx_init(&erts_proc_locks[i].mtx,
			  "proc_main" /* Con the lock checker */);
#ifdef ERTS_ENABLE_LOCK_CHECK
	erts_proc_locks[i].mtx.lc.id = -1; /* Dont want lock checking on
					      these mutexes */
#endif
	erts_smp_cnd_init(&erts_proc_locks[i].cnd);
    }

#else /* !ERTS_SMP */

    erts_no_of_schedulers = 1;
    esdp = &erts_scheduler_data;

#ifdef USE_THREADS
    erts_tsd_set(sched_data_key, (void *) esdp);
#endif

    init_sched_thr_data(esdp, 1);

#endif

    erts_smp_mtx_init(&proc_tab_mtx, "proc_tab");
    p_last = -1;
    p_next = 0;
    p_serial = 0;

    p_serial_shift = erts_fit_in_bits(erts_max_processes - 1);
    p_serial_mask = ((~(~((Uint) 0) << proc_bits)) >> p_serial_shift);
    erts_process_tab_index_mask = ~(~((Uint) 0) << p_serial_shift);

    /* mark the schedule queue as empty */
    for(i = 0; i < NPRIORITY_LEVELS - 1; i++)
	queue[i].first = queue[i].last = (Process*) 0;
    qmask = 0;
    queued_low = 0;
    queued_normal = 0;
    runq_len = 0;
#ifndef BM_COUNTERS
    processes_busy = 0;
#endif
    context_switches = 0;
    reductions = 0;
    last_reds = 0;
    last_exact_reds = 0;
    erts_default_process_flags = 0;
}

#ifdef ERTS_SMP

void
erts_wake_one_scheduler(void)
{
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
#ifndef ERTS_SMP_USE_IO_THREAD
    if (schedulers_waiting_on_runq == 1 && waiting_in_sys_schedule)
	erts_sys_schedule_interrupt(1);
    else
#endif
	erts_smp_cnd_signal(&schdlq_cnd);
}

static void
wake_all_schedulers(void)
{
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
#ifndef ERTS_SMP_USE_IO_THREAD
    erts_sys_schedule_interrupt(1);
#endif
    erts_smp_cnd_broadcast(&schdlq_cnd);
}

#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
void
erts_smp_notify_check_children_needed(void)
{
    ErtsSchedulerData *esdp;
    erts_smp_sched_lock();
    for (esdp = schedulers; esdp; esdp = esdp->next)
	esdp->check_children = 1;
    if (block_multi_scheduling) {
	/* Also blocked schedulers need to check children */
	erts_smp_mtx_lock(&msched_blk_mtx);
	for (esdp = schedulers; esdp; esdp = esdp->next)
	    esdp->blocked_check_children = 1;
	erts_smp_cnd_broadcast(&msched_blk_cnd);
	erts_smp_mtx_unlock(&msched_blk_mtx);
    }
    wake_all_schedulers();
    erts_smp_sched_unlock();
}
#endif

static void
prepare_for_block(void *c_p)
{
    erts_smp_sched_unlock();
    if (c_p)
	erts_smp_proc_unlock((Process *) c_p, ERTS_PROC_LOCK_MAIN);
}

static void
resume_after_block(void *c_p)
{
    if (c_p)
	erts_smp_proc_lock((Process *) c_p, ERTS_PROC_LOCK_MAIN);
    erts_smp_sched_lock();
}

#endif /* #ifdef ERTS_SMP */

static void
init_sched_thr_data(ErtsSchedulerData *esdp, Uint id)
{
#ifdef ERTS_SMP
    erts_bits_init_state(&esdp->erl_bits_state);
    esdp->match_pseudo_process = NULL;
    esdp->no = id;
    esdp->free_process = NULL;
#endif
    esdp->current_process = NULL;
#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
    esdp->check_children = 0;
#endif

}

#ifdef USE_THREADS

ErtsSchedulerData *
erts_get_scheduler_data(void)
{
    return (ErtsSchedulerData *) erts_tsd_get(sched_data_key);
}

#endif

static int remove_proc_from_sched_q(Process *p);

static ERTS_INLINE void
suspend_process(Process *p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());

    p->rcount++;  /* count number of suspend */
#ifdef ERTS_SMP
    ASSERT(!(p->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED)
	   || p == erts_get_current_process());
    ASSERT(p->status != P_RUNNING
	   || p->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED);
    if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ)
	goto runable;
#endif
    switch(p->status) {
    case P_SUSPENDED:
	break;
    case P_RUNABLE:
#ifdef ERTS_SMP
    runable:
        if (!ERTS_PROC_PENDING_EXIT(p)) 
#endif
	    remove_proc_from_sched_q(p);
	/* else:
	 * leave process in schedq so it will discover the pending exit
	 */
	p->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_RUNNING:
	p->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_WAITING:
	p->rstatus = P_WAITING; /* wakeup as waiting */
	break;
    case P_EXITING:
	return; /* ignore this */
    case P_GARBING:
    case P_FREE:
	erl_exit(1, "bad state in suspend_process()\n");
    }
    p->status = P_SUSPENDED;
}

static ERTS_INLINE void
resume_process(Process *p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    /* We may get called from trace([suspend], false) */
    if (p->status != P_SUSPENDED)
	return;
    ASSERT(p->rcount > 0);

    if (--p->rcount > 0)  /* multiple suspend i.e trace and busy port */
	return;
    switch(p->rstatus) {
    case P_RUNABLE:
	p->status = P_WAITING;  /* make add_to_schedule_q work */
	add_to_schedule_q(p);
	break;
    case P_WAITING:
	p->status = P_WAITING;
	break;
    default:
	erl_exit(1, "bad state in resume_process()\n");
    }
    p->rstatus = P_FREE;    
}

#ifdef ERTS_SMP

static void
block_multi_scheduling_block(ErtsSchedulerData *esdp)
{
    used_schedulers--;
    if (used_schedulers == 1)
	erts_wake_one_scheduler(); /* The one performing the block */
    while (block_multi_scheduling) {
#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
	if (esdp->check_children) {
	    esdp->check_children = 0;
	    erts_smp_sched_unlock();
	    erts_check_children();
	    erts_smp_sched_lock();
	}
#endif
	erts_smp_sched_unlock();
	erts_smp_activity_begin(ERTS_ACTIVITY_WAIT, NULL, NULL, NULL);
	erts_smp_mtx_lock(&msched_blk_mtx);
	while (block_multi_scheduling
#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
	       && !esdp->blocked_check_children
#endif
	    ) {
	    erts_smp_cnd_wait(&msched_blk_cnd, &msched_blk_mtx);
	}
#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
	esdp->blocked_check_children = 0;
#endif
	erts_smp_mtx_unlock(&msched_blk_mtx);
	erts_smp_activity_end(ERTS_ACTIVITY_WAIT, NULL, NULL, NULL);
	erts_smp_sched_lock();
    }
    used_schedulers++;
    erts_all_schedulers_waiting = 0;
}

/*
 * Return values:
 *  < 0: reschedule operation
 *  0:   multi-scheduling
 *  > 1: multi-scheduling blocked
 */

int
erts_block_multi_scheduling(Process *p, Uint32 plocks, int on, int all)
{
    int res;
    ProcessList *plp;
    int have_unlocked_plocks = 0;

    erts_smp_sched_lock();
    if (is_setting_block_multi_scheduling) {
	p->freason = RESCHEDULE;
	res = -1; /* Reschedule */
    }
    else if (on) {
	res = 1;

	if (block_multi_scheduling) {
	    plp = erts_alloc(ERTS_ALC_T_PROC_LIST, sizeof(ProcessList));
	    plp->pid = p->id;
	    plp->next = block_multi_scheduling_procs;
	    block_multi_scheduling_procs = plp;
	    p->flags |= F_HAVE_BLCKD_MSCHED;
	}
	else {
	    is_setting_block_multi_scheduling = 1;
	    erts_smp_mtx_lock(&msched_blk_mtx);
	    block_multi_scheduling = 1;
	    erts_smp_mtx_unlock(&msched_blk_mtx);
	    p->flags |= F_HAVE_BLCKD_MSCHED;
	    if (plocks) {
		have_unlocked_plocks = 1;
		erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	    }
	    erts_smp_activity_begin(ERTS_ACTIVITY_WAIT,
				    prepare_for_block,
				    resume_after_block,
				    NULL);
	    wake_all_schedulers();
	    while (used_schedulers != 1)
		erts_smp_cnd_wait(&schdlq_cnd, &schdlq_mtx);
	    erts_smp_activity_end(ERTS_ACTIVITY_WAIT,
				  prepare_for_block,
				  resume_after_block,
				  NULL);
	    plp = erts_alloc(ERTS_ALC_T_PROC_LIST, sizeof(ProcessList));
	    plp->pid = p->id;
	    plp->next = block_multi_scheduling_procs;
	    block_multi_scheduling_procs = plp;
	    is_setting_block_multi_scheduling = 0;
	}
    }
    else {
	if (!block_multi_scheduling)
	    res = 0;
	else {
	    Eterm pid = p->id;
	    ProcessList **plpp = &block_multi_scheduling_procs;
	    plp = block_multi_scheduling_procs;

	    while (plp) {
		if (plp->pid != pid){
		    plpp = &plp->next;
		    plp = plp->next;
		}
		else {
		    *plpp = plp->next;
		    erts_free(ERTS_ALC_T_PROC_LIST, plp);
		    if (!all)
			break;
		    plp = *plpp;
		}
	    }

	    if (block_multi_scheduling_procs)
		res = 1;
	    else {
		res = 0;
		erts_smp_mtx_lock(&msched_blk_mtx);
		block_multi_scheduling = 0;
		erts_smp_cnd_broadcast(&msched_blk_cnd);
		erts_smp_mtx_unlock(&msched_blk_mtx);
	    }
	}
    }

    erts_smp_sched_unlock();
    if (have_unlocked_plocks)
	erts_smp_proc_lock(p, plocks);
    return res;
}


int
erts_is_multi_scheduling_blocked(void)
{
    int res;
    erts_smp_sched_lock();
    res = block_multi_scheduling && used_schedulers == 1;
    erts_smp_sched_unlock();
    return res;
}

Eterm
erts_multi_scheduling_blockers(Process *p)
{
    Eterm res = NIL;
    erts_smp_sched_lock();

    if (!block_multi_scheduling || used_schedulers != 1) {
	ASSERT(!block_multi_scheduling_procs);
    }
    else {
	Eterm *hp, *hp_end;
	ProcessList *plp1, *plp2;
	Uint max_size;
	ASSERT(block_multi_scheduling_procs);
	for (max_size = 0, plp1 = block_multi_scheduling_procs;
	     plp1;
	     plp1 = plp1->next) {
	    max_size += 2;
	}
	ASSERT(max_size);
	hp = HAlloc(p, max_size);
	hp_end = hp + max_size;
	for (plp1 = block_multi_scheduling_procs; plp1; plp1 = plp1->next) {
	    for (plp2 = block_multi_scheduling_procs;
		 plp2->pid != plp1->pid;
		 plp2 = plp2->next);
	    if (plp2 == plp1) {
		res = CONS(hp, plp1->pid, res);
		hp += 2;
	    }
	    /* else: already in result list */
	}
	HRelease(p, hp_end, hp);
    }
    erts_smp_sched_unlock();
    return res;
}

static void
exit_sched_thr(ErtsSchedulerData *esdp, int schdlq_mtx_locked)
{
    ASSERT(esdp);
    if (!schdlq_mtx_locked)
	erts_smp_sched_lock();
    if (esdp->prev)
	esdp->prev->next = esdp->next;
    else
	schedulers = esdp->next;
    if (esdp->next)
	esdp->next->prev = esdp->prev;
    erts_bits_destroy_state(&esdp->erl_bits_state);
    erts_free(ERTS_ALC_T_SCHDLR_DATA, (void *) esdp);
    wake_all_schedulers();
    used_schedulers--;
    erts_smp_sched_unlock();
    erts_thr_exit(NULL);
}

static void *
sched_thread_func(void *vesdp)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	Uint no = ((ErtsSchedulerData *) vesdp)->no;
	erts_snprintf(&buf[0], 31, "scheduler %bpu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
    erts_tsd_set(sched_data_key, vesdp);
    erts_register_blockable_thread();
#ifdef HIPE
    hipe_thread_signal_init();
#endif
    erts_thread_init_float();
    process_main();
    exit_sched_thr((ErtsSchedulerData *) vesdp, 0);
    return NULL;
}

#ifdef ERTS_SMP

void
erts_start_schedulers(Uint wanted_no_of_schedulers)
{
    int res = 0;
    Uint actual = 0;
    Uint wanted = wanted_no_of_schedulers;
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    opts.detached = 1;

    if (wanted < 1)
	wanted = 1;
    if (wanted > ERTS_MAX_NO_OF_SCHEDULERS) {
	wanted = ERTS_MAX_NO_OF_SCHEDULERS;
	res = ENOTSUP;
    }

    erts_smp_sched_lock();

    while (actual < wanted) {
	ErtsSchedulerData *esdp;
	int cres;
	esdp = erts_alloc_fnf(ERTS_ALC_T_SCHDLR_DATA, sizeof(ErtsSchedulerData));
	if (!esdp) {
	    res = ENOMEM;
	    break;
	}
	actual++;
	init_sched_thr_data(esdp, actual);
	cres = ethr_thr_create(&esdp->tid,sched_thread_func,(void*)esdp,&opts);
	if (cres != 0) {
	    res = cres;
	    erts_free(ERTS_ALC_T_SCHDLR_DATA, (void *) esdp);
	    actual--;
	    break;
	}

	if (schedulers)
	    schedulers->prev = esdp;
	esdp->next = schedulers;
	esdp->prev = NULL;
	schedulers = esdp;
    }

    erts_no_of_schedulers = actual;
    used_schedulers = actual;

    erts_smp_sched_unlock();

    if (actual < 1)
	erl_exit(1,
		 "Failed to create any scheduler-threads: %s (%d)\n",
		 erl_errno_id(res),
		 res);
    if (res != 0) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	ASSERT(actual != wanted_no_of_schedulers);
	erts_dsprintf(dsbufp,
		      "Failed to create %bpu scheduler-threads (%s:%d); "
		      "only %bpu scheduler-thread%s created.\n",
		      wanted_no_of_schedulers, erl_errno_id(res), res,
		      actual, actual == 1 ? " was" : "s were");
	erts_send_error_to_logger_nogl(dsbufp);
    }
}

#endif

static void
add_to_proc_list(ProcessList** plpp, Eterm pid)
{
    ProcessList* plp;

    /* Add at the end of the list */
    for (; *plpp; plpp = &(*plpp)->next) {
	ASSERT((*plpp)->pid != pid);
    }

    plp = (ProcessList *) erts_alloc(ERTS_ALC_T_PROC_LIST, sizeof(ProcessList));
    plp->pid = pid;
    plp->next = NULL;

    *plpp = plp;
}

#if 0
static void
remove_from_proc_list(ProcessList** plpp, Eterm pid)
{
    for (; *plpp; plpp = &(*plpp)->next) {
	if ((*plpp)->pid == pid) {
	    ProcessList* plp = *plpp;
	    *plpp = plp->next;
	    erts_free(ERTS_ALC_T_PROC_LIST, (void *) plp);
#ifdef DEBUG
	    for (plp = *plpp; plp; plp = plp->next) {
		ASSERT(plp->pid != pid);
	    }
#endif
	    return;
	}
    }
    ASSERT(0);
}
#endif

static void
handle_pending_suspend(Process *p, Uint32 p_locks)
{
    ProcessList *plp;
    int do_suspend;
    Eterm suspendee;

    ASSERT(p->pending_suspenders);

    if (ERTS_PROC_IS_EXITING(p)) {
	do_suspend = 0;
	suspendee = NIL;
    }
    else {
	do_suspend = 1;
	suspendee = p->id;
    }

    plp = p->pending_suspenders; 
    while (plp) {
	ProcessList *free_plp;
	Process *rp = erts_pid2proc(p, p_locks,
				    plp->pid, ERTS_PROC_LOCK_STATUS);
	if (rp) {
	    ASSERT(is_nil(rp->suspendee));
	    rp->suspendee = suspendee;
	    if (do_suspend) {
		erts_smp_sched_lock();
		suspend_process(p);
		erts_smp_sched_unlock();
		do_suspend = 0;
	    }
	    /* rp is suspended waiting for p to suspend: resume rp */
	    resume_process(rp);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	}
	free_plp = plp;
	plp = plp->next;
	erts_free(ERTS_ALC_T_PROC_LIST, (void *) free_plp);
    }
    p->pending_suspenders = NULL;
}

static ERTS_INLINE void
cancel_suspend_of_suspendee(Process *p, Uint32 p_locks)
{
    if (is_not_nil(p->suspendee)) {
	Process *rp;
	if (!(p_locks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
	rp = erts_pid2proc(p, p_locks|ERTS_PROC_LOCK_STATUS,
			   p->suspendee, ERTS_PROC_LOCK_STATUS);
	if (rp)
	    erts_resume(rp, ERTS_PROC_LOCK_STATUS);
	if (!(p_locks & ERTS_PROC_LOCK_STATUS))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
	p->suspendee = NIL;
    }
}

Process *
erts_suspend_another_process(Process *c_p, Uint32 c_p_locks,
			     Eterm suspendee, Uint32 suspendee_locks)
{
    Process *rp;
    int unlock_c_p_status;

    ASSERT(c_p->id != suspendee);

    ERTS_SMP_LC_ASSERT(c_p_locks == erts_proc_lc_my_proc_locks(c_p));

    c_p->freason = EXC_NULL;

    if (c_p_locks & ERTS_PROC_LOCK_STATUS)
	unlock_c_p_status = 0;
    else {
	unlock_c_p_status = 1;
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
    }

    if (c_p->suspendee == suspendee) {
    suspended:
	if (unlock_c_p_status)
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
	return erts_pid2proc(c_p, c_p_locks, suspendee, suspendee_locks);
    }
    
    rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
		       suspendee, ERTS_PROC_LOCK_STATUS);

    if (rp) {
	erts_smp_sched_lock();
	if (!(rp->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED)) {
	    Uint32 need_locks = suspendee_locks & ~ERTS_PROC_LOCK_STATUS;
	    suspend_process(rp);
	    erts_smp_sched_unlock();
	    c_p->suspendee = suspendee;
	    if (need_locks && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		goto suspended;
	    }
	}
	else {
	    /* Mark rp pending for suspend by c_p */
	    add_to_proc_list(&rp->pending_suspenders, c_p->id);
	    ASSERT(is_nil(c_p->suspendee));

	    /* Suspend c_p (caller is assumed to return to process_main
	       immediately). When rp is suspended c_p will be resumed. */
	    suspend_process(c_p);
	    erts_smp_sched_unlock();
	    c_p->freason = RESCHEDULE;
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	    rp = NULL;
	}
    }

    if (rp && !(suspendee_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
    if (unlock_c_p_status)
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);

    return rp;
}

/*
 * Like erts_pid2proc() but:
 *
 * * At least ERTS_PROC_LOCK_MAIN have to be held on c_p.
 * * At least ERTS_PROC_LOCK_MAIN have to be taken on pid.
 * * It also waits for proc to be in a state != running and garbing.
 * * If NULL is returned, process might have to be rescheduled.
 *   Use ERTS_SMP_BIF_CHK_RESCHEDULE(P) to check this.
 */


Process *
erts_pid2proc_not_running(Process *c_p, Uint32 c_p_locks,
			  Eterm pid, Uint32 pid_locks)
{
    Process *rp;
    int unlock_c_p_status;

    ERTS_SMP_LC_ASSERT(c_p_locks == erts_proc_lc_my_proc_locks(c_p));

    ASSERT(pid_locks & (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS));

    c_p->freason = EXC_NULL;

    if (c_p->id == pid)
	return erts_pid2proc(c_p, c_p_locks, pid, pid_locks);

    if (c_p_locks & ERTS_PROC_LOCK_STATUS)
	unlock_c_p_status = 0;
    else {
	unlock_c_p_status = 1;
	erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_STATUS);
    }

    if (c_p->suspendee == pid) {
	/* Process previously suspended by c_p (below)... */
	Uint32 rp_locks = pid_locks|ERTS_PROC_LOCK_STATUS;
	rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS, pid, rp_locks);
	c_p->suspendee = NIL;
	if (rp)
	    resume_process(rp);
    }
    else {

	rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
			   pid, ERTS_PROC_LOCK_STATUS);

	if (!rp)
	    goto done;

	erts_smp_sched_lock();
	if (rp->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED) {
	scheduled:
	    /* Phiu... */

	    /* Mark rp pending for suspend by c_p */
	    add_to_proc_list(&rp->pending_suspenders, c_p->id);
	    ASSERT(is_nil(c_p->suspendee));

	    /* Suspend c_p (caller is assumed to return to process_main
	       immediately). When rp is suspended c_p will be resumed. */
	    suspend_process(c_p);
	    c_p->freason = RESCHEDULE;
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
	    rp = NULL;
	}
	else {
	    Uint32 need_locks = pid_locks & ~ERTS_PROC_LOCK_STATUS;
	    if (need_locks && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		erts_smp_sched_unlock();
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		rp = erts_pid2proc(c_p, c_p_locks|ERTS_PROC_LOCK_STATUS,
				   pid, pid_locks|ERTS_PROC_LOCK_STATUS);
		if (!rp)
		    goto done;
		erts_smp_sched_lock();
		if (rp->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED) {
		    /* Ahh... */
		    erts_smp_proc_unlock(rp,
					 pid_locks & ~ERTS_PROC_LOCK_STATUS);
		    goto scheduled;
		}
	    }

	    /* rp is not scheduled and we got the locks we want... */
	}
	erts_smp_sched_unlock();
    }

 done:
    if (rp && !(pid_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
    if (unlock_c_p_status)
	erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_STATUS);
    return rp;
}

/*
 * erts_proc_get_locks() assumes that lckp->mtx is locked by calling
 * thread and that one or more locks have been taken by other threads.
 * erts_proc_get_locks() returns when all locks in lock_flags
 * have been acquired if wait_for_locks != 0; otherwise, when
 * as many locks as possible have been acquired.
 */

Uint32
erts_proc_get_locks(Process *p,
		    erts_proc_lock_t *lckp,
		    Uint32 lock_flags,
		    int wait_for_locks)
{
    int i;
    Uint32 got_locks = 0;
    Uint32 need_locks = lock_flags & ERTS_PROC_LOCKS_ALL;
    ASSERT(need_locks & (p->lock_flags & ERTS_PROC_LOCKS_ALL));

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (wait_for_locks)
	erts_proc_lc_lock(p, need_locks);
#endif

    /*
     * Need to lock as many locks as possible (according to lock order)
     * in order to avoid starvation.
     */
    i = 0;
    while (1) {
	Uint32 lock = (1 << i);
	if (lock & need_locks) {
	check_lock_again:
	    if (lock & p->lock_flags) {
		if (wait_for_locks) {
		    p->lock_flags |= ERTS_PROC_LOCK_FLAG_WAITERS;
		    erts_smp_cnd_wait(&lckp->cnd, &lckp->mtx);
		}
		else
		    return got_locks;
		if (!(need_locks & p->lock_flags)) {
		    p->lock_flags |= need_locks; /* Got them all at once... */
#ifdef ERTS_ENABLE_LOCK_CHECK
		    if (!wait_for_locks)
			erts_proc_lc_lock(p, need_locks);
#endif
		    got_locks |= need_locks;
		    ASSERT(got_locks == (lock_flags & ERTS_PROC_LOCKS_ALL));
		    return got_locks;
		}
		goto check_lock_again;
	    }
	    else {
		p->lock_flags |= lock;
#ifdef ERTS_ENABLE_LOCK_CHECK
		if (!wait_for_locks)
		    erts_proc_lc_lock(p, lock);
#endif
		got_locks |= lock;
		need_locks &= ~lock;
		if (!need_locks) {
		    ASSERT(got_locks == (lock_flags & ERTS_PROC_LOCKS_ALL));
		    return got_locks;
		}
	    }
	}
	i++;
    }
}

/*
 * proc_safelock_aux() is a helper function for erts_proc_safelock().
 *
 * If no locks are held, process might have become exiting since the
 * last time we looked at it; therefore, we must check that process
 * is not exiting each time we acquires the lckp->mtx if no locks
 * were held.
 */
static int
proc_safelock_aux(Process *p, Uint pid, erts_proc_lock_t *lckp,
		  Uint32 *have_locks, Uint32 *need_locks,
		  Uint32 get_locks, Uint32 allow_exiting)
{
#define SAME_PROC(PID, PIX, PROC) \
  ((PROC) == process_tab[(PIX)] && (PROC)->id == (PID))
#define EXITING_PROC(PROC) \
  ((PROC)->lock_flags & ERTS_PROC_LOCK_FLAG_EXITING)
    int res = 0;
    Uint pix = internal_pid_index(pid);
    int check_same_proc = !*have_locks && pid != ERTS_INVALID_PID;
    int check_exiting_proc = (!allow_exiting && !*have_locks);
    Uint32 got_locks = 0;

    ASSERT((*have_locks & get_locks) == 0);
    ASSERT((*have_locks & *need_locks) == 0);
    ASSERT((*need_locks & get_locks) != 0);

    erts_smp_mtx_lock(&lckp->mtx);
    if (check_same_proc && (!SAME_PROC(pid, pix, p)
			    || (check_exiting_proc && EXITING_PROC(p))))
	goto done;

 do_get_locks:
    if (p->lock_flags & get_locks) {
	Uint32 locks = erts_proc_get_locks(p, lckp, get_locks, 0);
	get_locks &= ~locks;
	got_locks |= locks;
	if (get_locks) {
	    p->lock_flags |= ERTS_PROC_LOCK_FLAG_WAITERS;
	    erts_smp_cnd_wait(&lckp->cnd, &lckp->mtx);
	    if (check_same_proc
		&& (check_same_proc = !got_locks)
		&& (!SAME_PROC(pid, pix, p)
		    || (check_exiting_proc
			&& (check_exiting_proc = !got_locks)
			&& EXITING_PROC(p))))
		goto done;
	    goto do_get_locks;
	}
    }
    else {
	p->lock_flags |= get_locks; /* Got them all at once... */
#ifdef ERTS_ENABLE_LOCK_CHECK
	erts_proc_lc_lock(p, get_locks);
#endif
	got_locks |= get_locks;
	/* get_locks = 0; */
    }
    res = 1;

 done:
    erts_smp_mtx_unlock(&lckp->mtx);
    *have_locks |= got_locks;
    *need_locks &= ~got_locks;
    return res;
#undef SAME_PROC
#undef EXITING_PROC
}

/*
 * erts_proc_safelock() locks process locks on two processes. this_proc
 * should be the currently running process. In order to avoid a deadlock,
 * erts_proc_safelock() unlocks those locks that needs to be unlocked,
 * and then acquires locks in lock order (including the previously unlocked
 * ones).
 *
 * If other_proc becomes invalid during the locking NULL is returned,
 * this_proc's lock state is restored, and all locks on other_proc are
 * left unlocked.
 *
 * If allow_this_exiting is true this_proc is allowed to become invalid
 * (exiting); otherwise if this_proc becomes invalid, NULL is returned
 * and both processes lock states are restored.
 */

int
erts_proc_safelock(Process * this_proc,
		   Uint32 this_have_locks,
		   Uint32 this_need_locks,
		   int allow_this_exiting,
		   Uint32 other_pid,
		   Process *other_proc,
		   Uint32 other_have_locks,
		   Uint32 other_need_locks,
		   int allow_other_exiting)
{
    Process *p1, *p2, *exiting_p;
    Eterm pid1, pid2;
    Uint32 need_locks1, have_locks1, need_locks2, have_locks2;
    Uint32 unlock_mask, ax1, ax2;
    erts_proc_lock_t *lckp1, *lckp2;
    int lock_no, res;

    ASSERT(other_proc);


    /* Determine inter process lock order...
     * Locks with the same lock order should be locked on p1 before p2.
     */
    if (this_proc) {
	if (this_proc->id < other_pid) {
	    p1 = this_proc;
	    pid1 = this_proc->id;
	    need_locks1 = this_need_locks;
	    have_locks1 = this_have_locks;
	    lckp1 = &erts_proc_locks[ERTS_PID2LOCKIX(pid1)];
	    ax1 = allow_this_exiting;
	    p2 = other_proc;
	    pid2 = other_pid;
	    need_locks2 = other_need_locks;
	    have_locks2 = other_have_locks;
	    lckp2 = &erts_proc_locks[ERTS_PID2LOCKIX(pid2)];
	    ax2 = allow_other_exiting;
	}
	else if (this_proc->id > other_pid) {
	    p1 = other_proc;
	    pid1 = other_pid;
	    need_locks1 = other_need_locks;
	    have_locks1 = other_have_locks;
	    lckp1 = &erts_proc_locks[ERTS_PID2LOCKIX(pid1)];
	    ax1 = allow_other_exiting;
	    p2 = this_proc;
	    pid2 = this_proc->id;
	    need_locks2 = this_need_locks;
	    have_locks2 = this_have_locks;
	    lckp2 = &erts_proc_locks[ERTS_PID2LOCKIX(pid2)];
	    ax2 = allow_this_exiting;
	}
	else {
	    ASSERT(this_proc == other_proc);
	    ASSERT(this_proc->id == other_pid);
	    p1 = this_proc;
	    pid1 = this_proc->id;
	    need_locks1 = this_need_locks | other_need_locks;
	    have_locks1 = this_have_locks | other_have_locks;
	    lckp1 = &erts_proc_locks[ERTS_PID2LOCKIX(pid1)];
	    ax1 = allow_this_exiting || allow_other_exiting;
	    p2 = NULL;
	    pid2 = 0;
	    need_locks2 = 0;
	    have_locks2 = 0;
	    lckp2 = NULL;
	    ax2 = 0;
	}
    }
    else {
	p1 = other_proc;
	pid1 = other_pid;
	need_locks1 = other_need_locks;
	have_locks1 = other_have_locks;
	lckp1 = &erts_proc_locks[ERTS_PID2LOCKIX(pid1)];
	ax1 = allow_other_exiting;
	p2 = NULL;
	pid2 = 0;
	need_locks2 = 0;
	have_locks2 = 0;
	lckp2 = NULL;
	ax2 = 0;
#ifdef ERTS_ENABLE_LOCK_CHECK
	this_need_locks = 0;
	this_have_locks = 0;
#endif
    }

    res = 1; /* Prepare for success... */

 start_restore:


#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);

    if ((need_locks1 & have_locks1) != have_locks1)
	erts_lc_fail("Thread tries to release process lock(s) "
		     "on %T via erts_proc_safelock().", pid1);
    if ((need_locks2 & have_locks2) != have_locks2)
	erts_lc_fail("Thread tries to release process lock(s) "
		     "on %T via erts_proc_safelock().",
		     pid2);
#endif


    need_locks1 &= ~have_locks1;
    need_locks2 &= ~have_locks2;

    /* Figure out the range of locks that needs to be unlocked... */
    unlock_mask = ERTS_PROC_LOCKS_ALL;
    for (lock_no = 0;
	 lock_no <= ERTS_PROC_LOCK_MAX_BIT;
	 lock_no++) {
	Uint32 lock = (1 << lock_no);
	if (lock & need_locks1)
	    break;
	unlock_mask &= ~lock;
	if (lock & need_locks2)
	    break;
    }

    /* ... and unlock locks in that range... */
    if (have_locks1 || have_locks2) {
	Uint32 unlock_locks;
	unlock_locks = unlock_mask & have_locks1;
	if (unlock_locks) {
	    have_locks1 &= ~unlock_locks;
	    need_locks1 |= unlock_locks;
	    erts_proc_unlock(p1, unlock_locks);
	}
	unlock_locks = unlock_mask & have_locks2;
	if (unlock_locks) {
	    have_locks2 &= ~unlock_locks;
	    need_locks2 |= unlock_locks;
	    erts_proc_unlock(p2, unlock_locks);
	}
    }

    /*
     * lock_no equals the number of the first lock to lock on
     * either p1 *or* p2.
     */


#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);
#endif

    /* Lock locks in lock order... */
    while (lock_no <= ERTS_PROC_LOCK_MAX_BIT) {
	Uint32 locks;
	Uint32 lock = (1 << lock_no);
	Uint32 lock_mask = 0;
	if (need_locks1 & lock) {
	    do {
		lock = (1 << lock_no++);
		lock_mask |= lock;
	    } while (lock_no <= ERTS_PROC_LOCK_MAX_BIT
		     && !(need_locks2 & lock));
	    if (need_locks2 & lock)
		lock_no--;
	    locks = need_locks1 & lock_mask;
	    if (!proc_safelock_aux(p1, pid1, lckp1,
				   &have_locks1, &need_locks1,
				   locks, ax1)) {
		exiting_p = p1;
		goto exiting_proc;
	    }
	}
	else if (need_locks2 & lock) {
	    while (lock_no <= ERTS_PROC_LOCK_MAX_BIT
		   && !(need_locks1 & lock)) {
		lock_mask |= lock;
		lock = (1 << ++lock_no);
	    }
	    locks = need_locks2 & lock_mask;
	    if (!proc_safelock_aux(p2, pid2, lckp2,
				   &have_locks2, &need_locks2,
				   locks, ax2)) {
		exiting_p = p2;
		goto exiting_proc;
	    }
	}
	else
	    lock_no++;
    }

 done:


#ifdef ERTS_ENABLE_LOCK_CHECK
    if (p1)
	erts_proc_lc_chk_proc_locks(p1, have_locks1);
    if (p2)
	erts_proc_lc_chk_proc_locks(p2, have_locks2);

    if (p1 && p2) {
	if (p1 == this_proc) {
	    ERTS_SMP_LC_ASSERT(this_need_locks == have_locks1);
	    ERTS_SMP_LC_ASSERT(other_need_locks == have_locks2);
	}
	else {
	    ERTS_SMP_LC_ASSERT(this_need_locks == have_locks2);
	    ERTS_SMP_LC_ASSERT(other_need_locks == have_locks1);
	}
    }
    else {
	ERTS_SMP_LC_ASSERT(p1);
	if (this_proc) {
	    ERTS_SMP_LC_ASSERT(have_locks1
			       == (this_need_locks
				   | other_need_locks));
	}
	else {
	    ERTS_SMP_LC_ASSERT(have_locks1 == other_need_locks);
	}
    }
#endif


    return res;

 exiting_proc:
    res = 0;
    /*
     * Note: We may end up here two times if this_proc gets exiting
     *       the first time we try to lock, and other_proc gets exiting
     *       when we try to restore the lock states. This is no problem
     *       and will work out fine.
     */

    /*
     * We have no locks on the proc that got exiting.
     */
    if (this_proc) {
	/* Piuhhhh!!! Fix the mess... */
	Uint32 restore_locks1, restore_locks2;
	Uint32 unlock_locks;
	if (this_proc == exiting_p) {
	    /* Restore locks on both procs */
	    if (this_proc == p1) {
		ASSERT(!have_locks1);
		restore_locks1 = this_have_locks;
		restore_locks2 = other_have_locks;
		ax1 = 1;
	    }
	    else {
		ASSERT(this_proc == p2);
		ASSERT(!have_locks2);
		restore_locks1 = other_have_locks;
		restore_locks2 = this_have_locks;
		ax2 = 1;
	    }
#ifdef ERTS_ENABLE_LOCK_CHECK
	    this_need_locks = this_have_locks;
	    other_need_locks = other_have_locks;
#endif
	}
	else {
	    /* Restore locks on this_proc */
	    if (this_proc == p1) {
		ASSERT(!have_locks2);
		restore_locks1 = this_have_locks;
		restore_locks2 = 0;
		ax1 = 1;
	    }
	    else {
		ASSERT(this_proc == p2);
		ASSERT(!have_locks1);
		restore_locks1 = 0;
		restore_locks2 = this_have_locks;
		ax2 = 1;
	    }
#ifdef ERTS_ENABLE_LOCK_CHECK
	    this_need_locks = this_have_locks;
	    other_need_locks = 0;
#endif
	}

	unlock_locks = have_locks1 & ~restore_locks1;
	if (unlock_locks) {
	    erts_proc_unlock(p1, unlock_locks);
	    have_locks1 &= ~unlock_locks;
	}
	need_locks1 = restore_locks1;

	unlock_locks = have_locks2 & ~restore_locks2;
	if (unlock_locks) {
	    erts_proc_unlock(p2, unlock_locks);
	    have_locks2 &= ~unlock_locks;
	}
	need_locks2 = restore_locks2;

	if (need_locks1 != have_locks1 || need_locks2 != have_locks1)
	    goto start_restore;
    }
    else {
	ASSERT(exiting_p == other_proc);
	/* No this_proc and other_proc exiting == we are done */
#ifdef ERTS_ENABLE_LOCK_CHECK
	need_locks1 = have_locks1 = need_locks2 = have_locks2
	    = this_need_locks = other_need_locks = 0;
#endif
    }

    goto done;
}

#endif /* ERTS_SMP */

int
sched_q_len(void)
{
#ifdef DEBUG
    int i;
#endif
    Sint len = 0;

    erts_smp_sched_lock();

#ifdef DEBUG
    for (i = 0; i < NPRIORITY_LEVELS - 1; i++) {
	Process* p;

	for (p = queue[i].first; p != NULL; p = p->next) {
	    len++;
	}
    }
    ASSERT(len == runq_len);
#endif

    len = runq_len;

    erts_smp_sched_unlock();

    return (int) len;
}

#ifdef HARDDEBUG
static int
is_proc_in_schdl_q(Process *p)
{
    int i;
    for (i = 0; i < NPRIORITY_LEVELS - 1; i++) {
	Process* rp;
	for (rp = queue[i].first; rp; rp = rp->next) {
	    if (rp == p)
		return 1;
	}
    }
    return 0;
}
#endif

/* schedule a process */
static ERTS_INLINE void
internal_add_to_schedule_q(Process *p)
{
    /*
     * ERTS_SMP: internal_add_to_schuduleq should only be used from:
     *           - add_to_scheduleq()
     *           - schedule() when schdlq_mtx and scheduler is about
     *             to schedule a new process.
     */
    ScheduleQ* sq;

#ifdef ERTS_SMP

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());

    if (p->status_flags & ERTS_PROC_SFLG_INRUNQ)
	return;
    else if (p->scheduler_flags & ERTS_PROC_SCHED_FLG_SCHEDULED) {
	ASSERT(p->status != P_SUSPENDED);
#ifdef HARDDEBUG
	ASSERT(!is_proc_in_schdl_q(p));
#endif
	p->status_flags |= ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	return;
    }
    ASSERT(!p->scheduler_data);
#endif

#ifdef HARDDEBUG
    ASSERT(!is_proc_in_schdl_q(p));
#endif

    switch (p->prio) {
    case PRIORITY_LOW:
      queued_low++;
      sq = &queue[PRIORITY_NORMAL];
      break;
    case PRIORITY_NORMAL:
      queued_normal++;
    default:
      sq = &queue[p->prio];      
    }

#ifndef ERTS_SMP
    /* Never schedule a suspended process (ok in smp case) */
    ASSERT(p->status != P_SUSPENDED);
#endif

    qmask |= (1 << p->prio);

    p->next = NULL;
    if (sq->first == (Process *) 0)
	sq->first = p;
    else
	sq->last->next = p;
    sq->last = p;
    if (p->status != P_EXITING) {
	p->status = P_RUNABLE;
    }

    runq_len++;
#ifdef ERTS_SMP
    p->status_flags |= ERTS_PROC_SFLG_INRUNQ;
#endif

}


void
add_to_schedule_q(Process *p)
{
    erts_smp_sched_lock();
    internal_add_to_schedule_q(p);
    erts_smp_notify_inc_runq();
    erts_smp_sched_unlock();
}

/* Possibly remove a scheduled process we need to suspend */

static int
remove_proc_from_sched_q(Process *p)
{
    Process *tmp, *prev;
    int res, i;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS & erts_proc_lc_my_proc_locks(p));

#ifdef ERTS_SMP
    if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ) {
	p->status_flags &= ~ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	ASSERT(!remove_proc_from_sched_q(p));
	return 1;
    }
#endif

    res = 0;

    for(i = 0; i < NPRIORITY_LEVELS - 1; i++) {
	ScheduleQ *sq = &queue[i];

	if (sq->first == (Process*) NULL)
	    continue;
	if (sq->first == sq->last && sq->first == p) {
	    sq->first = sq->last = NULL;

	    if (i == PRIORITY_NORMAL) {
	       qmask &= ~(1 << PRIORITY_NORMAL) & ~(1 << PRIORITY_LOW);
	       queued_low = 0;
	       queued_normal = 0; 
	    }
	    else
	       qmask &= ~(1 << p->prio);

	    ASSERT(runq_len > 0);
	    res = 1;
	    goto done;
	}
	if (sq->first == p) {
	    sq->first = sq->first->next;
	    DECR_PROC_COUNT(p->prio);
	    ASSERT(runq_len > 0);
	    res = 1;
	    goto done;
	}
	tmp = sq->first->next;
	prev = sq->first;
	while (tmp) {
	    if (tmp == p) {
		prev->next = tmp->next;
		DECR_PROC_COUNT(p->prio);
		if (p == sq->last)
		    sq->last = prev;
		ASSERT(runq_len > 0);
		res = 1;
		goto done;
	    }
	    prev = tmp;
	    tmp = tmp->next;
	}
    }

 done:

    if (res) {
#ifdef ERTS_SMP
	p->status_flags &= ~ERTS_PROC_SFLG_INRUNQ;
#endif
	runq_len--;
    }
#ifdef ERTS_SMP
    ASSERT(!(p->status_flags & ERTS_PROC_SFLG_INRUNQ));
#endif
#ifdef HARDDEBUG
    ASSERT(!is_proc_in_schdl_q(p));
#endif
    return res;
}


Eterm
erts_process_status(Process *c_p, Uint32 c_p_locks,
		    Process *rp, Eterm rpid)
{
    Eterm res = am_undefined;
    Process *p;

    if (rp) {
	ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_STATUS
			   & erts_proc_lc_my_proc_locks(rp));
	p = rp;
    }
    else {
	p = erts_pid2proc_opt(c_p, c_p_locks,
			      rpid, ERTS_PROC_LOCK_STATUS,
			      ERTS_P2P_FLG_ALLOW_OTHER_X);
    }

    if (p) {
	switch (p->status) {
	case P_RUNABLE:
	    res = am_runnable;
	    break;
	case P_WAITING:
	    res = am_waiting;
	    break;
	case P_RUNNING:
	    res = am_running;
	    break;
	case P_EXITING:
	    res = am_exiting;
	    break;
	case P_GARBING:
	    res = am_garbage_collecting;
	    break;
	case P_SUSPENDED:
	    res = am_suspended;
	    break;
	case P_FREE:	/* We cannot look up a process in P_FREE... */
	default:	/* Not a valid status... */
	    erl_exit(1, "Bad status (%b32u) found for process %T\n",
		     p->status, p->id);
	    break;
	}

#ifdef ERTS_SMP
	if (!rp && (p != c_p || !(ERTS_PROC_LOCK_STATUS & c_p_locks)))
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    }
    else {
	ErtsSchedulerData *esdp;
	erts_smp_sched_lock();
	for (esdp = schedulers; esdp; esdp = esdp->next) {
	    if (esdp->free_process && esdp->free_process->id == rpid) {
		res = am_free;
		break;
	    }
	}
	erts_smp_sched_unlock();
#endif

    }

    return res;
}

/*
** Suspend a process 
** If we are to suspend on a port the busy_port is the thing
** otherwise busy_port is NIL
*/

void
erts_suspend(Process* process, Uint32 process_locks, Port *busy_port)
{

    ERTS_SMP_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(process, ERTS_PROC_LOCK_STATUS);

    erts_smp_sched_lock();

    suspend_process(process);

    erts_smp_sched_unlock();

    if (busy_port)
	erts_wake_process_later(busy_port, process);

    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(process, ERTS_PROC_LOCK_STATUS);

}

void
erts_resume(Process* process, Uint32 process_locks)
{
    ERTS_SMP_LC_ASSERT(process_locks == erts_proc_lc_my_proc_locks(process));
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_lock(process, ERTS_PROC_LOCK_STATUS);
    resume_process(process);
    if (!(process_locks & ERTS_PROC_LOCK_STATUS))
	erts_smp_proc_unlock(process, ERTS_PROC_LOCK_STATUS);
}

Eterm
erts_get_process_priority(Process *p)
{
    Eterm value;
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p));
    erts_smp_sched_lock();
    switch(p->prio) {
    case PRIORITY_MAX:		value = am_max;			break;
    case PRIORITY_HIGH:		value = am_high;		break;
    case PRIORITY_NORMAL:	value = am_normal;		break;
    case PRIORITY_LOW:		value = am_low;			break;
    default: ASSERT(0);		value = am_undefined;		break;
    }
    erts_smp_sched_unlock();
    return value;
}

Eterm
erts_set_process_priority(Process *p, Eterm new_value)
{
    Eterm old_value;
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p));
    erts_smp_sched_lock();
    switch(p->prio) {
    case PRIORITY_MAX:		old_value = am_max;		break;
    case PRIORITY_HIGH:		old_value = am_high;		break;
    case PRIORITY_NORMAL:	old_value = am_normal;		break;
    case PRIORITY_LOW:		old_value = am_low;		break;
    default: ASSERT(0);		old_value = am_undefined;	break;
    }
    switch (new_value) {
    case am_max:		p->prio = PRIORITY_MAX;		break;
    case am_high:		p->prio = PRIORITY_HIGH;	break;
    case am_normal:		p->prio = PRIORITY_NORMAL;	break;
    case am_low:		p->prio = PRIORITY_LOW;		break;
    default:			old_value = THE_NON_VALUE;	break;
    }
    erts_smp_sched_unlock();
    return old_value;
}

/* note that P_RUNNING is only set so that we don't try to remove
** running processes from the schedule queue if they exit - a running
** process not being in the schedule queue!! 
** Schedule for up to INPUT_REDUCTIONS context switches,
** return 1 if more to do.
*/

/*
 * schedule() is called from BEAM (process_main()) or HiPE
 * (hipe_mode_switch()) when the current process is to be
 * replaced by a new process. 'calls' is the number of reduction
 * steps the current process consumed.
 * schedule() returns the new process, and the new process'
 * ->fcalls field is initialised with its allowable number of
 * reduction steps.
 *
 * When no process is runnable, or when sufficiently many reduction
 * steps have been made, schedule() calls erl_sys_schedule() to
 * schedule system-level activities.
 *
 * We use the same queue for normal and low prio processes.
 * We reschedule low prio processes a certain number of times 
 * so that normal processes get to run more frequently. 
 */

Process *schedule(Process *p, int calls)
{
    ScheduleQ *sq;
    long dt;
    ErtsSchedulerData *esdp;
    int context_reds;
    int input_reductions;

    if (ERTS_USE_MODIFIED_TIMING()) {
	context_reds = ERTS_MODIFIED_TIMING_CONTEXT_REDS;
	input_reductions = ERTS_MODIFIED_TIMING_INPUT_REDS;
    }
    else {
	context_reds = CONTEXT_REDS;
	input_reductions = INPUT_REDUCTIONS;
    }

    ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);

    /*
     * Clean up after the process being suspended.
     */
    if (!p) {	/* NULL in the very first schedule() call */
	esdp = erts_get_scheduler_data();
	ASSERT(esdp);
	erts_smp_sched_lock();
    } else {
#ifdef ERTS_SMP
	esdp = p->scheduler_data;
	ASSERT(esdp->current_process == p
	       || esdp->free_process == p);
#else
	esdp = &erts_scheduler_data;
	ASSERT(esdp->current_process == p);
#endif
#ifndef ERTS_SMP_USE_IO_THREAD
	function_calls += calls;
#endif
	reductions += calls;
	ASSERT(esdp && esdp == erts_get_scheduler_data());

	p->reds += calls;

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);

#ifdef ERTS_SMP
	if (ERTS_PROC_PENDING_EXIT(p)) {
	    erts_handle_pending_exit(p,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    p->status_flags |= ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	}

	if (p->pending_suspenders) {
	    handle_pending_suspend(p,
				   ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	    ASSERT(!(p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ)
		   || p->status != P_SUSPENDED);
	}
#endif
	erts_smp_sched_lock();

	/* Rule of thumb, only trace when we have a valid current_process */
	if (p->status != P_FREE && IS_TRACED_FL(p, F_TRACE_SCHED)) {
	    trace_sched(p, am_out);
	}

	esdp->current_process = NULL;
#ifdef ERTS_SMP
	p->scheduler_data = NULL;
	p->scheduler_flags &= ~ERTS_PROC_SCHED_FLG_SCHEDULED;
	p->status_flags &= ~ERTS_PROC_SFLG_SCHEDULED;

	if (p->status_flags & ERTS_PROC_SFLG_PENDADD2SCHEDQ) {
	    p->status_flags &= ~ERTS_PROC_SFLG_PENDADD2SCHEDQ;
	    internal_add_to_schedule_q(p);
	}
#endif


	if (p->status == P_FREE) {
	    ERTS_PROC_LESS_MEM(sizeof(Process));
#ifdef ERTS_SMP
	    ASSERT(esdp->free_process == p);
	    esdp->free_process = NULL;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
	    /* No need to unlock unless we are checking locks */
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
#endif
	    erts_free(ERTS_ALC_T_PROC, (void *) p);
	} else {
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	}

#ifdef ERTS_SMP
	ASSERT(!esdp->free_process);
#endif
	ASSERT(!esdp->current_process);

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	dt = do_time_read_and_reset();
	if (dt) {
	    erts_smp_sched_unlock();
	    bump_timer(dt);
	    erts_smp_sched_lock();
	}
	BM_STOP_TIMER(system);

    }

    ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
 check_activities_to_run: {
#ifdef ERTS_USE_PORT_TASKS
	long port_runq_len;
#endif
	long tot_runq_len;
	
	ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
	ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());

#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
	if (esdp->check_children) {
	    esdp->check_children = 0;
	    erts_smp_sched_unlock();
	    erts_check_children();
	    erts_smp_sched_lock();
	}
#endif

	if (misc_op_queue)
	    exec_misc_ops();

	ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);
	ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());

#ifdef ERTS_SMP
	if (block_multi_scheduling && used_schedulers != 1)
	    block_multi_scheduling_block(esdp);
	else
	    erts_smp_chk_system_block(prepare_for_block,
				      resume_after_block,
				      NULL);
#endif

	tot_runq_len = runq_len;

#ifdef ERTS_USE_PORT_TASKS
	port_runq_len = erts_port_task_port_queue_len();
	tot_runq_len += port_runq_len;
#endif


#ifdef ERTS_SMP
	if (schedulers_waiting_on_runq && tot_runq_len > 1)
	    erts_wake_one_scheduler();

	if (tot_runq_len == 0) {
	empty_runq:
	    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
	    if (block_multi_scheduling && used_schedulers != 1)
		goto check_activities_to_run;
	    schedulers_waiting_on_runq++;
	    if (used_schedulers == schedulers_waiting_on_runq)
		erts_all_schedulers_waiting = 1;
#ifndef ERTS_SMP_USE_IO_THREAD
	    if (!doing_sys_schedule
#ifdef ERTS_USE_PORT_TASKS
		&& !erts_port_task_have_outstanding_io_tasks()
#endif
		) {
		function_calls = 0;
		waiting_in_sys_schedule = doing_sys_schedule = 1;
		erts_sys_schedule_interrupt(0);
		erts_smp_sched_unlock();
		erl_sys_schedule(0);
		dt = do_time_read_and_reset();
		if (dt) bump_timer(dt);
		erts_smp_sched_lock();
		waiting_in_sys_schedule = doing_sys_schedule = 0;
	    }
	    else
#endif
	    {
#ifndef ERTS_SMP_USE_IO_THREAD
		/* If all schedulers are waiting, one of them *should*
		   be waiting in erl_sys_schedule() */
		ASSERT(!erts_all_schedulers_waiting
		       || waiting_in_sys_schedule);
#endif

		erts_smp_activity_begin(ERTS_ACTIVITY_WAIT,
					prepare_for_block,
					resume_after_block,
					NULL);
		erts_smp_cnd_wait(&schdlq_cnd, &schdlq_mtx);
		erts_smp_activity_end(ERTS_ACTIVITY_WAIT,
				      prepare_for_block,
				      resume_after_block,
				      NULL);
	    }
	    if (erts_all_schedulers_waiting)
		erts_all_schedulers_waiting = 0;
	    ASSERT(schedulers_waiting_on_runq > 0);
	    schedulers_waiting_on_runq--;
	    goto check_activities_to_run;
	}
#endif /* #ifdef ERTS_SMP */
#if defined(ERTS_SMP) && !defined(ERTS_SMP_USE_IO_THREAD)
	else
#endif
#ifndef ERTS_SMP_USE_IO_THREAD
	if (function_calls > input_reductions
#ifdef ERTS_SMP
	    && !doing_sys_schedule
#endif
#ifdef ERTS_USE_PORT_TASKS
	    && !erts_port_task_have_outstanding_io_tasks()
#endif
	    ) {
	    int runnable;

#ifdef ERTS_SMP
	    runnable = 1;
#else
	    runnable = tot_runq_len != 0;
	do_sys_schedule:
#endif

	    /*
	     * Schedule system-level activities.
	     */

	    function_calls = 0;
#ifdef ERTS_USE_PORT_TASKS
	    ASSERT(!erts_port_task_have_outstanding_io_tasks());
#endif
#ifdef ERTS_SMP
	    /* erts_sys_schedule_interrupt(0); */
	    doing_sys_schedule = 1;
#endif
	    erts_smp_sched_unlock();
	    erl_sys_schedule(runnable);
	    dt = do_time_read_and_reset();
	    if (dt) bump_timer(dt);
	    erts_smp_sched_lock();
#ifdef ERTS_SMP
	    doing_sys_schedule = 0;
#endif
	    goto check_activities_to_run;
#ifndef ERTS_SMP
	empty_runq:
	    runnable = 0;
	    goto do_sys_schedule;
#endif
	}
#endif /* !ERTS_SMP_USE_IO_THREAD */

#ifdef ERTS_USE_PORT_TASKS
	/*
	 * Find a new port to run.
	 */

	if (port_runq_len) {
	    int have_outstanding_io = erts_port_task_execute();
	    if (have_outstanding_io && function_calls > 2*input_reductions) {
		/*
		 * If we have performed more than 2*INPUT_REDUCTIONS since
		 * last call to erl_sys_schedule() and we still haven't
		 * handled all I/O tasks we stop running processes and
		 * focus completely on ports.
		 *
		 * One could argue that this is a strange behavior. The
		 * reason for doing it this way is that it is similar
		 * to the behavior before port tasks were introduced.
		 * We don't want to change the behavior too much, at
		 * least not at the time of writing. This behavior
		 * might change in the future.
		 *
		 * /rickard
		 */
		goto check_activities_to_run;
	    }
	}
#endif

	/*
	 * Find a new process to run.
	 */
 pick_next_process:

      switch (qmask) {
	case MAX_BIT:
	case MAX_BIT|HIGH_BIT:
	case MAX_BIT|NORMAL_BIT:
	case MAX_BIT|LOW_BIT:
	case MAX_BIT|HIGH_BIT|NORMAL_BIT:
	case MAX_BIT|HIGH_BIT|LOW_BIT:
	case MAX_BIT|NORMAL_BIT|LOW_BIT:
	case MAX_BIT|HIGH_BIT|NORMAL_BIT|LOW_BIT:
	    sq = &queue[PRIORITY_MAX];
	    break;
	case HIGH_BIT:
	case HIGH_BIT|NORMAL_BIT:
	case HIGH_BIT|LOW_BIT:
	case HIGH_BIT|NORMAL_BIT|LOW_BIT:
	    sq = &queue[PRIORITY_HIGH];
	    break;
        case NORMAL_BIT:
	    sq = &queue[PRIORITY_NORMAL];
	    break;
        case LOW_BIT:
	    sq = &queue[PRIORITY_NORMAL];
	    break;
	case NORMAL_BIT|LOW_BIT:	  
	    sq = &queue[PRIORITY_NORMAL];
	    ASSERT(sq->first != NULL);
	    p = sq->first;
	    if (p->prio == PRIORITY_LOW) {
	      if ((p != sq->last) && (p->skipped < RESCHEDULE_LOW-1)) { /* reschedule */
		p->skipped++;
		/* put last in queue */
		sq->first = p->next;
		p->next = NULL;
		(sq->last)->next = p;
		sq->last = p;
		goto pick_next_process;
	      } else {
		p->skipped = 0;
	      }
	    }
	    break;
        case 0:			/* No process at all */
	default:
	    ASSERT(qmask == 0);
	    ASSERT(runq_len == 0);
#ifdef ERTS_USE_PORT_TASKS
	    if (erts_port_task_port_queue_len())
		goto check_activities_to_run;
#endif
	    goto empty_runq;
	}

        BM_START_TIMER(system);

	/*
	 * Take the chosen process out of the queue.
	 */
	ASSERT(sq->first != NULL); /* Wrong bitmask in qmask? */
	p = sq->first;
	sq->first = p->next;
	
	if (p->prio == PRIORITY_LOW) {
	  if (--queued_low == 0) {
	    qmask &= ~(1 << PRIORITY_LOW);
	    if (sq->first == NULL) {
	      sq->last = NULL;
	      ASSERT_NORMAL_Q_EMPTY();
	    } else
	      ASSERT((queued_normal > 0) && ((qmask >> PRIORITY_NORMAL) & 1));
	  }
	} else if (p->prio == PRIORITY_NORMAL) {
	  if (--queued_normal == 0) {
	    qmask &= ~(1 << PRIORITY_NORMAL);
	    if (sq->first == NULL) {
	      sq->last = NULL;
	      ASSERT_NORMAL_Q_EMPTY();
	    } else
	      ASSERT((queued_low > 0) && ((qmask >> PRIORITY_LOW) & 1));
	  }
	} else {
	  if (sq->first == NULL) {
	    sq->last = NULL;
	    qmask &= ~(1 << p->prio);
	  }
	}

	ASSERT(runq_len > 0);
	runq_len--;

	context_switches++;

#ifdef ERTS_SMP
	p->scheduler_flags |= ERTS_PROC_SCHED_FLG_SCHEDULED;
#endif
	
#ifdef HARDDEBUG
	ASSERT(!is_proc_in_schdl_q(p));
#endif

	esdp->current_process = p;

#ifdef ERTS_SMP
	{
	    ProcessList *pnd_xtrs = pending_exiters;
	    pending_exiters = NULL;
	    erts_smp_sched_unlock();

	    if (pnd_xtrs)
		handle_pending_exiters(pnd_xtrs);
	}

	ERTS_SMP_CHK_NO_PROC_LOCKS;

	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);

	p->status_flags |= ERTS_PROC_SFLG_SCHEDULED;
	p->status_flags &= ~ERTS_PROC_SFLG_INRUNQ;
	if (ERTS_PROC_PENDING_EXIT(p)) {
	    erts_handle_pending_exit(p,
				     ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS);
	}
	ASSERT(!p->scheduler_data);
	p->scheduler_data = esdp;

#endif
	ASSERT(p->status != P_SUSPENDED); /* Never run a suspended process */

        ACTIVATE(p);
	calls = context_reds;
	if (p->status != P_EXITING) {
	    if (IS_TRACED_FL(p, F_TRACE_SCHED)) {
		trace_sched(p, am_in);
	    }
	    p->status = P_RUNNING;
	}

	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

#ifdef ERTS_SMP
	if (is_not_nil(p->tracer_proc))
	    erts_check_my_tracer_proc(p);
#endif

	if (((MBUF_SIZE(p) + MSO(p).overhead) * MBUF_GC_FACTOR) >= HEAP_SIZE(p)) {
	    calls -= erts_garbage_collect(p, 0, p->arg_reg, p->arity);
	    if (calls < 0) {
		calls = 1;
	    }
	}

	p->fcalls = calls;
	ASSERT(IS_ACTIVE(p));
	return p;
    }
}

/*
 * Scheduling of misc stuff
 */

void
erts_schedule_misc_op(void (*func)(void *), void *arg)
{
    ErtsMiscOpList *molp;
    erts_smp_sched_lock();
    molp = misc_op_list_alloc();
    molp->next = NULL;
    molp->func = func;
    molp->arg = arg;
    if (misc_op_queue_end)
	misc_op_queue_end->next = molp;
    else
	misc_op_queue = molp;
    misc_op_queue_end = molp;
    erts_smp_notify_inc_runq();
    erts_smp_sched_unlock();
}

static void
exec_misc_ops(void)
{
    int i;
    ErtsMiscOpList *molp = misc_op_queue;
    ErtsMiscOpList *tmp_molp = molp;

    for (i = 0; i < ERTS_MAX_MISC_OPS-1; i++) {
	if (!tmp_molp) 
	    goto mtq;
	tmp_molp = tmp_molp->next;
    }
    
    if (!tmp_molp) {
    mtq:
	misc_op_queue = NULL;
	misc_op_queue_end = NULL;
    }
    else {
	misc_op_queue = tmp_molp->next;
	tmp_molp->next = NULL;
	if (!misc_op_queue)
	    misc_op_queue_end = NULL;
    }

    tmp_molp = molp;

    erts_smp_sched_unlock();

    while (molp) {
	(*molp->func)(molp->arg);
	molp = molp->next;
    }

    erts_smp_sched_lock();

    molp = tmp_molp;

    while (molp) {
	tmp_molp = molp;
	molp = molp->next;
	misc_op_list_free(tmp_molp); /* need sched lock */
    }
}


Uint erts_get_tot_proc_mem(void)
{
    return (Uint) erts_smp_atomic_read(&erts_tot_proc_mem);
}

Uint
erts_get_total_context_switches(void)
{
    Uint res;
    erts_smp_sched_lock();
    res = context_switches;
    erts_smp_sched_unlock();
    return res;
}

void
erts_get_total_reductions(Uint *redsp, Uint *diffp)
{
    Uint reds;
    erts_smp_sched_lock();
    reds = reductions;
    if (redsp)
	*redsp = reds;
    if (diffp)
	*diffp = reds - last_reds;
    last_reds = reds;
    erts_smp_sched_unlock();
}

/*
 * Current process might be exiting after call to
 * erts_get_total_reductions().
 */
void
erts_get_exact_total_reductions(Process *c_p, Uint *redsp, Uint *diffp)
{
    Uint reds = erts_current_reductions(c_p, c_p);
    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
    /*
     * Wait for other schedulers to schedule out their processes
     * and update 'reductions'.
     */
    erts_smp_block_system(ERTS_ACTIVITY_IO); /*erts_smp_sched_lock();*/
    reds += reductions;
    if (redsp)
	*redsp = reds;
    if (diffp)
	*diffp = reds - last_exact_reds;
    last_exact_reds = reds;
    erts_smp_release_system(); /*erts_smp_sched_unlock();*/
    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
}

/*
 * erts_test_next_pid() is only used for testing.
 */
Sint
erts_test_next_pid(int set, Uint next)
{
    Sint res;
    Sint p_prev;

    erts_smp_mtx_lock(&proc_tab_mtx);

    if (!set) {
	res = p_next < 0 ? -1 : (p_serial << p_serial_shift | p_next);
    }
    else {
	erts_smp_proc_tab_lock();

	p_serial = (Sint) ((next >> p_serial_shift) & p_serial_mask);
	p_next = (Sint) (erts_process_tab_index_mask & next);

	if (p_next >= erts_max_processes) {
	    p_next = 0;
	    p_serial++;
	    p_serial &= p_serial_mask;
	}

	p_prev = p_next;

	do {
	    if (!process_tab[p_next])
		break;
	    p_next++;
	    if(p_next >= erts_max_processes) {
		p_next = 0;
		p_serial++;
		p_serial &= p_serial_mask;
	    }
	} while (p_prev != p_next);

	res = process_tab[p_next] ? -1 : (p_serial << p_serial_shift | p_next);

	erts_smp_proc_tab_unlock();
    }

    erts_smp_mtx_unlock(&proc_tab_mtx);

    return res;

}

Uint erts_process_count(void)
{
    long res = erts_smp_atomic_read(&process_count);
    ASSERT(res >= 0);
    return (Uint) res;
}

/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(void)
{
    erts_smp_mtx_t *ptabix_mtxp;
    Process* p;
    int p_prev;

    erts_smp_mtx_lock(&proc_tab_mtx);

    if (p_next == -1) {
	p = NULL;
	goto error; /* Process table full! */
    }

    p = (Process*) erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	goto error; /* ENOMEM */ 

    p_last = p_next;

#ifdef ERTS_SMP
    ptabix_mtxp = &erts_proc_locks[ERTS_PIX2LOCKIX(p_next)].mtx;
#else
    ptabix_mtxp = NULL;
#endif

    erts_smp_mtx_lock(ptabix_mtxp);

    process_tab[p_next] = p;
    erts_smp_atomic_inc(&process_count);
    ERTS_PROC_MORE_MEM(sizeof(Process));
    p->id = make_internal_pid(p_serial << p_serial_shift | p_next);
    if (p->id == ERTS_INVALID_PID) {
	/* Do not use the invalid pid; change serial */
	p_serial++;
	p_serial &= p_serial_mask;
	p->id = make_internal_pid(p_serial << p_serial_shift | p_next);
	ASSERT(p->id != ERTS_INVALID_PID);
    }
    ASSERT(internal_pid_serial(p->id) <= (erts_use_r9_pids_ports
					  ? ERTS_MAX_PID_R9_SERIAL
					  : ERTS_MAX_PID_SERIAL));

#ifdef ERTS_SMP
    p->lock_flags = ERTS_PROC_LOCKS_ALL;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, ERTS_PROC_LOCKS_ALL, 1);
#endif
#endif

    p->rstatus = P_FREE;
    p->rcount = 0;


    erts_smp_mtx_unlock(ptabix_mtxp);

    /*
     * set p_next to the next available slot
     */

    p_prev = p_next;

    while (1) {
	p_next++;
	if(p_next >= erts_max_processes) {
	    p_serial++;
	    p_serial &= p_serial_mask;
	    p_next = 0;
	}

	if (p_prev == p_next) {
	    p_next = -1;
	    break; /* Table full! */
	}

	if (!process_tab[p_next])
	    break; /* found a free slot */
    }

 error:

    erts_smp_mtx_unlock(&proc_tab_mtx);

    return p;

}

Eterm
erl_create_process(Process* parent, /* Parent of process (default group leader). */
		   Eterm mod,	/* Tagged atom for module. */
		   Eterm func,	/* Tagged atom for function. */
		   Eterm args,	/* Arguments for function (must be well-formed list). */
		   ErlSpawnOpts* so) /* Options for spawn. */
{
    Process *p;
    Sint arity;			/* Number of arguments. */
#ifndef HYBRID
    Uint arg_size;		/* Size of arguments. */
#endif
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
    ScheduleQ* sq;
    Eterm res = THE_NON_VALUE;

#ifdef ERTS_SMP
    erts_smp_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

#ifdef HYBRID
    /*
     * Copy the arguments to the global heap
     * Since global GC might occur we want to do this before adding the
     * new process to the process_tab.
     */
    BM_SWAP_TIMER(system,copy);
    LAZY_COPY(parent,args);
    BM_SWAP_TIMER(copy,system);
    heap_need = 0;
#endif /* HYBRID */
    /*
     * Check for errors.
     */

    if (is_not_atom(mod) || is_not_atom(func) || ((arity = list_length(args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }
    p = alloc_process(); /* All proc locks are locked by this thread
			    on success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

    processes_busy++;
    BM_COUNT(processes_spawned);

#ifndef HYBRID
    BM_SWAP_TIMER(system,size);
    arg_size = size_object(args);
    BM_SWAP_TIMER(size,system);
    heap_need = arg_size;
#endif

    p->flags = erts_default_process_flags;

    /* Scheduler queue mutex should be locked when changeing
     * prio. In this case we don't have to lock it, since
     * noone except us has access to the process.
     */
    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size = so->min_heap_size;
	p->prio = so->priority;
	p->max_gen_gcs = so->max_gen_gcs;
    } else {
	p->min_heap_size = H_MIN_SIZE;
	p->prio = PRIORITY_NORMAL;
	p->max_gen_gcs = (Uint16) erts_smp_atomic_read(&erts_max_gen_gcs);
    }
    p->skipped = 0;
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

    /*
     * Must initialize binary lists here before copying binaries to process.
     */
    p->off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    p->off_heap.funs = NULL;
#endif
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;

    heap_need +=
	IS_CONST(parent->group_leader) ? 0 : NC_HEAP_SIZE(parent->group_leader);

    if (heap_need < p->min_heap_size) {
	sz = heap_need = p->min_heap_size;
    } else {
	sz = erts_next_heap_size(heap_need, 0);
    }

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif

    p->heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*sz);
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
#ifdef INCREMENTAL
    p->scan_top = p->high_water;
#endif
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
#if !defined(HEAP_FRAG_ELIM_TEST)
    p->arith_avail = 0;		/* No arithmetic heap. */
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
#endif
    p->catches = 0;

    /* No need to initialize p->fcalls. */

    p->current = p->initial+INITIAL_MOD;

    p->i = (Eterm *) beam_apply;
    p->cp = (Eterm *) beam_apply+1;

    p->arg_reg = p->def_arg_reg;
    p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
    p->arg_reg[0] = mod;
    p->arg_reg[1] = func;
    BM_STOP_TIMER(system);
    BM_MESSAGE(args,p,parent);
    BM_START_TIMER(system);
#ifdef HYBRID
    p->arg_reg[2] = args;
#ifdef INCREMENTAL
    p->active = 0;
    if (ptr_val(args) >= inc_fromspc && ptr_val(args) < inc_fromend)
        INC_ACTIVATE(p);
#endif
#else
    BM_SWAP_TIMER(system,copy);
    p->arg_reg[2] = copy_struct(args, arg_size, &p->htop, &p->off_heap);
    BM_MESSAGE_COPIED(arg_size);
    BM_SWAP_TIMER(copy,system);
#endif
    p->arity = 3;

    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->reds = 0;

#ifdef ERTS_SMP
    p->ptimer = NULL;
#else
    sys_memset(&p->tm, 0, sizeof(ErlTimer));
#endif

    p->reg = NULL;
    p->dist_entry = NULL;
    p->error_handler = am_error_handler;    /* default */
    p->nlinks = NULL;
    p->monitors = NULL;
    p->nodes_monitors = NULL;
    p->ct = NULL;

    ASSERT(is_pid(parent->group_leader));

    if (parent->group_leader == ERTS_INVALID_PID)
	p->group_leader = p->id;
    else {
	/* Needs to be done after the heap has been set up */
	p->group_leader =
	    IS_CONST(parent->group_leader)
	    ? parent->group_leader
	    : STORE_NC(&p->htop, &p->off_heap.externals, parent->group_leader);
    }

    erts_get_default_tracing(&p->trace_flags, &p->tracer_proc);

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifdef ERTS_SMP
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
#endif
    p->bif_timers = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->dictionary = NULL;
    p->debug_dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
    p->parent = parent->id == ERTS_INVALID_PID ? NIL : parent->id;
    p->started = erts_get_time();

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif

    INIT_HOLE_CHECK(p);

    if (IS_TRACED(parent)) {
	if (parent->trace_flags & F_TRACE_SOS) {
	    p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	}
	if (ARE_TRACE_FLAGS_ON(parent, F_TRACE_PROCS)) {
	    trace_proc_spawn(parent, p->id, mod, func, args);
	}
	if (parent->trace_flags & F_TRACE_SOS1) { /* Overrides TRACE_CHILDREN */
	    p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	    p->trace_flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	    parent->trace_flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	}
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
#ifdef DEBUG
	int ret;
#endif
	if (IS_TRACED_FL(parent, F_TRACE_PROCS)) {
	    trace_proc(parent, parent, am_link, p->id);
	}

#ifdef DEBUG
	ret = erts_add_link(&(parent->nlinks),  LINK_PID, p->id);
	ASSERT(ret == 0);
	ret = erts_add_link(&(p->nlinks), LINK_PID, parent->id);
	ASSERT(ret == 0);
#else	
	erts_add_link(&(parent->nlinks), LINK_PID, p->id);
	erts_add_link(&(p->nlinks), LINK_PID, parent->id);
#endif

	if (IS_TRACED(parent)) {
	    if (parent->trace_flags & (F_TRACE_SOL|F_TRACE_SOL1))  {
		p->trace_flags |= (parent->trace_flags & TRACEE_FLAGS);
		p->tracer_proc = parent->tracer_proc;    /* maybe steal */

		if (parent->trace_flags & F_TRACE_SOL1)  { /* maybe override */
		    p ->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    parent->trace_flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
	    }
	}
    }

    /*
     * Test whether this process should be initially monitored by its parent.
     */
    if (so->flags & SPO_MONITOR) {
	Eterm mref;

	mref = erts_make_ref(parent);
	erts_add_monitor(&(parent->monitors), MON_ORIGIN, mref, p->id, NIL);
	erts_add_monitor(&(p->monitors), MON_TARGET, mref, parent->id, NIL);
	so->mref = mref;
    }

#ifdef HYBRID
    /*
     * Add process to the array of active processes.
     */
    ACTIVATE(p);
    p->active_index = erts_num_active_procs++;
    erts_active_procs[p->active_index] = p;
#endif

    /*
     * Schedule process for execution.
     */

    erts_smp_sched_lock();

    qmask |= (1 << p->prio);

    switch (p->prio) {
    case PRIORITY_LOW:
      queued_low++;
      sq = &queue[PRIORITY_NORMAL];
      break;
    case PRIORITY_NORMAL:
      queued_normal++;
    default:
      sq = &queue[p->prio];      
    }

    runq_len++;

    p->next = NULL;
    if (!sq->first)
	sq->first = p;
    else
	sq->last->next = p;
    sq->last = p;

    p->status = P_RUNABLE;

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->is_exiting = 0;
    p->status_flags = ERTS_PROC_SFLG_INRUNQ;
    p->scheduler_flags = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
#endif

#if !defined(NO_FPE_SIGNALS)
    p->fp_exception = 0;
#endif

    erts_smp_notify_inc_runq();
    erts_smp_sched_unlock();

    res = p->id;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);

    VERBOSE(DEBUG_PROCESSES, ("Created a new process: %T\n",p->id));

 error:

    erts_smp_proc_unlock(parent, ERTS_PROC_LOCKS_ALL_MINOR);

    return res;
}

/*
 * Initiates a pseudo process that can be used
 * for arithmetic BIFs.
 */

void erts_init_empty_process(Process *p)
{
    p->htop = NULL;
    p->stop = NULL;
    p->hend = NULL;
    p->heap = NULL;
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
    p->min_heap_size = 0;
    p->status = P_RUNABLE;
    p->rstatus = P_RUNABLE;
    p->rcount = 0;
    p->id = ERTS_INVALID_PID;
    p->prio = PRIORITY_NORMAL;
    p->reds = 0;
    p->error_handler = am_error_handler;
    p->tracer_proc = NIL;
    p->trace_flags = F_INITIAL_TRACE_FLAGS;
    p->group_leader = ERTS_INVALID_PID;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->fcalls = 0;
    p->dist_entry = NULL;
#ifdef ERTS_SMP
    p->ptimer = NULL;
#else
    memset(&(p->tm), 0, sizeof(ErlTimer));
#endif
    p->next = NULL;
    p->off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    p->off_heap.funs = NULL;
#endif
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;
    p->reg = NULL;
    p->heap_sz = 0;
    p->high_water = NULL;
#ifdef INCREMENTAL
    p->scan_top = NULL;
#endif
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->monitors = NULL;
    p->nlinks = NULL;         /* List of links */
    p->nodes_monitors = NULL;
    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
    p->bif_timers = NULL;
    p->dictionary = NULL;
    p->debug_dictionary = NULL;
    p->ct = NULL;
    p->seq_trace_clock = 0;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_token = NIL;
    p->initial[0] = 0;
    p->initial[1] = 0;
    p->initial[2] = 0;
    p->catches = 0;
    p->cp = NULL;
    p->i = NULL;
    p->current = NULL;

    /*
     * Secondary heap for arithmetic operations.
     */
#if !defined(HEAP_FRAG_ELIM_TEST)
    p->arith_heap = NULL;
    p->arith_avail = 0;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
#endif

    /*
     * Saved x registers.
     */
    p->arity = 0;
    p->arg_reg = NULL;
    p->max_arg_reg = 0;
    p->def_arg_reg[0] = 0;
    p->def_arg_reg[1] = 0;
    p->def_arg_reg[2] = 0;
    p->def_arg_reg[3] = 0;
    p->def_arg_reg[4] = 0;
    p->def_arg_reg[5] = 0;

    p->parent = NIL;
    p->started = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif

    ACTIVATE(p);

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif
    INIT_HOLE_CHECK(p);

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->is_exiting = 0;
    p->status_flags = 0;
    p->scheduler_flags = 0;
    p->lock_flags = 0;
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
#endif

#if !defined(NO_FPE_SIGNALS)
    p->fp_exception = 0;
#endif

}    

#ifdef DEBUG

void
erts_debug_verify_clean_empty_process(Process* p)
{
    /* Things that erts_cleanup_empty_process() will *not* cleanup... */
    ASSERT(p->htop == NULL);
    ASSERT(p->stop == NULL);
    ASSERT(p->hend == NULL);
    ASSERT(p->heap == NULL);
    ASSERT(p->id == ERTS_INVALID_PID);
    ASSERT(p->tracer_proc == NIL);
    ASSERT(p->trace_flags == F_INITIAL_TRACE_FLAGS);
    ASSERT(p->group_leader == ERTS_INVALID_PID);
    ASSERT(p->dist_entry == NULL);
    ASSERT(p->next == NULL);
    ASSERT(p->reg == NULL);
    ASSERT(p->heap_sz == 0);
    ASSERT(p->high_water == NULL);
#ifdef INCREMENTAL
    ASSERT(p->scan_top == NULL);
#endif
    ASSERT(p->old_hend == NULL);
    ASSERT(p->old_htop == NULL);
    ASSERT(p->old_heap == NULL);

    ASSERT(p->monitors == NULL);
    ASSERT(p->nlinks == NULL);
    ASSERT(p->nodes_monitors == NULL);
    ASSERT(p->msg.first == NULL);
    ASSERT(p->msg.len == 0);
    ASSERT(p->bif_timers == NULL);
    ASSERT(p->dictionary == NULL);
    ASSERT(p->debug_dictionary == NULL);
    ASSERT(p->ct == NULL);
    ASSERT(p->catches == 0);
    ASSERT(p->cp == NULL);
    ASSERT(p->i == NULL);
    ASSERT(p->current == NULL);

    ASSERT(p->parent == NIL);

#ifdef ERTS_SMP
    ASSERT(p->msg_inq.first == NULL);
    ASSERT(p->msg_inq.len == 0);
    ASSERT(p->suspendee == NIL);
    ASSERT(p->pending_suspenders == NULL);
    ASSERT(p->pending_exit.reason == THE_NON_VALUE);
    ASSERT(p->pending_exit.bp == NULL);
#endif

    /* Thing that erts_cleanup_empty_process() cleans up */

    ASSERT(p->off_heap.mso == NULL);
#ifndef HYBRID /* FIND ME! */
    ASSERT(p->off_heap.funs == NULL);
#endif
    ASSERT(p->off_heap.externals == NULL);
    ASSERT(p->off_heap.overhead == 0);

#if !defined(HEAP_FRAG_ELIM_TEST)
    ASSERT(p->arith_avail == 0);
    ASSERT(p->arith_heap == NULL);
#ifdef DEBUG
    ASSERT(p->arith_check_me == NULL);
#endif
#endif
    ASSERT(p->mbuf == NULL);
}

#endif

void
erts_cleanup_empty_process(Process* p)
{
    ErlHeapFragment* mbufp;

    /* We only check fields that are known to be used... */

    erts_cleanup_offheap(&p->off_heap);
    p->off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    p->off_heap.funs = NULL;
#endif
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;

#if !defined(HEAP_FRAG_ELIM_TEST)
    p->arith_avail = 0;
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
#endif

    mbufp = p->mbuf;
    while (mbufp) {
	ErlHeapFragment *next = mbufp->next;
	free_message_buffer(mbufp);
	mbufp = next;
    }
    p->mbuf = NULL;

#ifdef DEBUG
    erts_debug_verify_clean_empty_process(p);
#endif
}

/*
 * p must be the currently executing process.
 */
static void
delete_process(Process* p)
{
    ErlMessage* mp;
    ErlHeapFragment* bp;

    VERBOSE(DEBUG_PROCESSES, ("Removing process: %T\n",p->id));

    /* Clean binaries and funs */
    erts_cleanup_offheap(&p->off_heap);

    /*
     * The mso list should not be used anymore, but if it is, make sure that
     * we'll notice.
     */
    p->off_heap.mso = (void *) 0x8DEFFACD;

    if (p->arg_reg != p->def_arg_reg) {
	ERTS_PROC_LESS_MEM(p->max_arg_reg * sizeof(p->arg_reg[0]));
	erts_free(ERTS_ALC_T_ARG_REG, p->arg_reg);
    }

    /*
     * Release heaps. Clobber contents in DEBUG build.
     */


#ifdef DEBUG
    sys_memset(p->heap, DEBUG_BAD_BYTE, p->heap_sz*sizeof(Eterm));
#endif

#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP, (void*) p->heap, p->heap_sz*sizeof(Eterm));
    if (p->old_heap != NULL) {

#ifdef DEBUG
	sys_memset(p->old_heap, DEBUG_BAD_BYTE,
                   (p->old_hend-p->old_heap)*sizeof(Eterm));
#endif
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       p->old_heap,
		       (p->old_hend-p->old_heap)*sizeof(Eterm));
    }

    /*
     * Free all pending message buffers.
     */
    bp = p->mbuf;
    while (bp != NULL) {
	ErlHeapFragment* next_bp = bp->next;
	free_message_buffer(bp);
	bp = next_bp;
    }

    erts_erase_dicts(p);

    /* free all pending messages */
    mp = p->msg.first;
    while(mp != NULL) {
	ErlMessage* next_mp = mp->next;
#ifdef ERTS_SMP
	if (mp->bp)
	    free_message_buffer(mp->bp);
#endif
	free_message(mp);
	mp = next_mp;
    }

    ASSERT(!p->monitors);
    ASSERT(!p->nlinks);
    ASSERT(!p->nodes_monitors);

    if (p->ct != NULL) {
	ERTS_PROC_LESS_MEM((sizeof(struct saved_calls)
			    + (p->ct->len - 1) * sizeof(Export *)));
        erts_free(ERTS_ALC_T_CALLS_BUF, (void *) p->ct);
    }

    if(p->dist_entry) {
	erts_deref_dist_entry(p->dist_entry);
	p->dist_entry = NULL;
    }

    p->fvalue = NIL;
    
#ifdef HYBRID
    erts_active_procs[p->active_index] =
        erts_active_procs[--erts_num_active_procs];
    erts_active_procs[p->active_index]->active_index = p->active_index;
#ifdef INCREMENTAL
    if (INC_IS_ACTIVE(p))
         INC_DEACTIVATE(p);
#endif

    if (p->rrma != NULL) {
        erts_free(ERTS_ALC_T_ROOTSET,p->rrma);
        erts_free(ERTS_ALC_T_ROOTSET,p->rrsrc);
        ERTS_PROC_LESS_MEM(sizeof(Eterm) * p->rrsz * 2);
    }
#endif

}

static ERTS_INLINE void
set_proc_exiting(Process *p, Eterm reason, ErlHeapFragment *bp)
{
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCKS_ALL);
    /*
     * You are required to have all locks when going to status P_EXITING,
     * This makes it is enough to take any lock when looking up a process
     * (pid2proc()) to prevent the looked up process from exiting until
     * the lock has been released.
     */

#ifdef ERTS_SMP
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_FLAG_EXITING);
    p->is_exiting = 1;
#endif
    p->status = P_EXITING;
    p->fvalue = reason;
    if (bp)
	erts_link_mbuf_to_proc(p, bp);
    p->freason = EXC_EXIT;
    KILL_CATCHES(p);
    cancel_timer(p);
    p->i = (Eterm *) beam_exit;
}


#ifdef ERTS_SMP

void
erts_handle_pending_exit(Process *c_p, Uint32 locks)
{
    Uint32 xlocks;
    ASSERT(is_value(c_p->pending_exit.reason));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == locks);
    ERTS_SMP_LC_ASSERT(locks & ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_LC_ASSERT(c_p->status != P_EXITING);
    ERTS_SMP_LC_ASSERT(c_p->status != P_FREE);

    /* Ensure that all locks on c_p are locked before proceeding... */
    if (locks == ERTS_PROC_LOCKS_ALL)
	xlocks = 0;
    else {
	xlocks = ~locks & ERTS_PROC_LOCKS_ALL;
	if (erts_smp_proc_trylock(c_p, xlocks) == EBUSY) {
	    erts_smp_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }

    set_proc_exiting(c_p, c_p->pending_exit.reason, c_p->pending_exit.bp);
    c_p->pending_exit.reason = THE_NON_VALUE;
    c_p->pending_exit.bp = NULL;

    if (xlocks)
	erts_smp_proc_unlock(c_p, xlocks);
}

static void
handle_pending_exiters(ProcessList *pnd_xtrs)
{
    ProcessList *plp = pnd_xtrs;
    ProcessList *free_plp;
    while (plp) {
	Process *p = erts_pid2proc(NULL, 0, plp->pid, ERTS_PROC_LOCKS_ALL);
	if (p && !(p->status_flags & ERTS_PROC_SFLG_SCHEDULED)) {
	    ASSERT(p->status_flags & ERTS_PROC_SFLG_INRUNQ);
	    ASSERT(ERTS_PROC_PENDING_EXIT(p));
	    erts_handle_pending_exit(p, ERTS_PROC_LOCKS_ALL);
	}
	if (p)
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
	free_plp = plp;
	plp = plp->next;
	erts_free(ERTS_ALC_T_PROC_LIST, (void *) free_plp);
    }
}

static void
save_pending_exiter(Eterm pid)
{
    ProcessList *plp;

    plp = erts_alloc(ERTS_ALC_T_PROC_LIST, sizeof(ProcessList));
    plp->pid = pid;
    erts_smp_sched_lock();
    plp->next = pending_exiters;
    pending_exiters = plp;
    erts_smp_sched_unlock();
}

#endif

/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

static ERTS_INLINE void
send_exit_message(Process *to, Uint32 *to_locksp,
		  Eterm exit_term, Uint term_size, Eterm token)
{
    if (token == NIL) {
	Eterm* hp;
	Eterm mess;
	ErlHeapFragment* bp;
	ErlOffHeap *ohp;

	hp = erts_alloc_message_heap(term_size, &bp, &ohp, to, to_locksp);
	mess = copy_struct(exit_term, term_size, &hp, ohp);
	erts_queue_message(to, *to_locksp, bp, mess, NIL);
    } else {
	ErlHeapFragment* bp;
	Eterm* hp;
	Eterm mess;
	Eterm temp_token;
	Uint sz_token;

	ASSERT(is_tuple(token));
	sz_token = size_object(token);
	bp = new_message_buffer(term_size+sz_token);
	hp = bp->mem;
	mess = copy_struct(exit_term, term_size, &hp, &bp->off_heap);
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, mess, SEQ_TRACE_SEND, to->id, NULL);
	temp_token = copy_struct(token, sz_token, &hp, &bp->off_heap);
	erts_queue_message(to, *to_locksp, bp, mess, temp_token);
    }
}

/*
 *
 * *** Exit signal behavior ***
 *
 * Exit signals are asynchronous (truly asynchronous in the
 * SMP emulator). When the signal is received the receiver receives an
 * 'EXIT' message if it is trapping exits; otherwise, it will either
 * ignore the signal if the exit reason is normal, or go into an
 * exiting state (status P_EXITING). When a process has gone into the
 * exiting state it will not execute any more Erlang code, but it might
 * take a while before it actually exits. The exit signal is being
 * received when the 'EXIT' message is put in the message queue, the
 * signal is dropped, or when it changes state into exiting. The time it
 * is in the exiting state before actually exiting is undefined (it
 * might take a really long time under certain conditions). The
 * receiver of the exit signal does not break links or trigger monitors
 * until it actually exits.
 *
 * Exit signals and other signals, e.g. messages, have to be received
 * by a receiver in the same order as sent by a sender.
 *
 *
 *
 * Exit signal implementation in the SMP emulator:
 *
 * If the receiver is trapping exits, the signal is transformed
 * into an 'EXIT' message and sent as a normal message, if the
 * reason is normal the signal is dropped; otherwise, the process
 * is determined to be exited. The interesting case is when the
 * process is to be exited and this is what is described below.
 *
 * If it is possible, the receiver is set in the exiting state straight
 * away and we are done; otherwise, the sender places the exit reason
 * in the pending_exit field of the process struct and if necessary
 * adds the receiver to the run queue. It is typically not possible
 * to set a scheduled process or a process which we cannot get all locks
 * on without releasing locks on it in an exiting state straight away.
 *
 * The receiver will poll the pending_exit field when it reach certain
 * places during it's execution. When it discovers the pending exit
 * it will change state into the exiting state. If the receiver wasn't
 * scheduled when the pending exit was set, the first scheduler that
 * schedules a new process will set the receiving process in the exiting
 * state just before it schedules next process.
 * 
 * When the exit signal is placed in the pending_exit field, the signal
 * is considered as being in transit on the Erlang level. The signal is
 * actually in some kind of semi transit state, since we have already
 * determined how it should be received. It will exit the process no
 * matter what if it is received (the process may exit by itself before
 * reception of the exit signal). The signal is received when it is
 * discovered in the pending_exit field by the receiver.
 *
 * The receiver have to poll the pending_exit field at least before:
 * - moving messages from the message in queue to the private message
 *   queue. This in order to preserve signal order.
 * - unlink. Otherwise the process might get exited on a link that
 *   have been removed.
 * - changing the trap_exit flag to true. This in order to simplify the
 *   implementation; otherwise, we would have to transform the signal
 *   into an 'EXIT' message when setting the trap_exit flag to true. We
 *   would also have to maintain a queue of exit signals in transit.
 * - being scheduled in or out.
 */

static ERTS_INLINE int
send_exit_signal(Process *c_p,		/* current process if and only
					   if reason is stored on it */
		 Eterm from,		/* Id of sender of signal */
		 Process *rp,		/* receiving process */
		 Uint32 *rp_locks,	/* current locks on receiver */
		 Eterm reason,		/* exit reason */
		 Eterm exit_tuple,	/* Prebuild exit tuple
					   or THE_NON_VALUE */
		 Uint exit_tuple_sz,	/* Size of prebuilt exit tuple
					   (if exit_tuple != THE_NON_VALUE) */
		 Eterm token,		/* token */
		 Process *token_update, /* token updater */
		 Uint32 flags		/* flags */
    )		
{
    Eterm rsn = reason == am_kill ? am_killed : reason;

    ERTS_SMP_LC_ASSERT(*rp_locks == erts_proc_lc_my_proc_locks(rp));
    ERTS_SMP_LC_ASSERT((*rp_locks & ERTS_PROC_LOCKS_XSIG_SEND)
		       == ERTS_PROC_LOCKS_XSIG_SEND);

    ASSERT(reason != THE_NON_VALUE);

    if (ERTS_PROC_IS_TRAPPING_EXITS(rp)
	&& (reason != am_kill || (flags & ERTS_XSIG_FLG_IGN_KILL))) {
	if (is_not_nil(token) && token_update)
	    seq_trace_update_send(token_update);
	if (is_value(exit_tuple))
	    send_exit_message(rp, rp_locks, exit_tuple, exit_tuple_sz, token);
	else
	    erts_deliver_exit_message(from, rp, rp_locks, rsn, token);
	return 1; /* Receiver will get a message */
    }
    else if (reason != am_normal || (flags & ERTS_XSIG_FLG_NO_IGN_NORMAL)) {
#ifdef ERTS_SMP
	if (!ERTS_PROC_PENDING_EXIT(rp) && !rp->is_exiting) {
	    ASSERT(rp->status != P_EXITING);
	    ASSERT(rp->status != P_FREE);
	    ASSERT(!rp->pending_exit.bp);

	    if (rp == c_p && (*rp_locks & ERTS_PROC_LOCK_MAIN)) {
		/* Ensure that all locks on c_p are locked before
		   proceeding... */
		if (*rp_locks != ERTS_PROC_LOCKS_ALL) {
		    Uint32 need_locks = ~(*rp_locks) & ERTS_PROC_LOCKS_ALL;
		    if (erts_smp_proc_trylock(c_p, need_locks) == EBUSY) {
			erts_smp_proc_unlock(c_p,
					     *rp_locks & ~ERTS_PROC_LOCK_MAIN);
			erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
		    }
		    *rp_locks = ERTS_PROC_LOCKS_ALL;
		}
		set_proc_exiting(c_p, rsn, NULL);
	    }
	    else if (!(rp->status_flags & ERTS_PROC_SFLG_SCHEDULED)) {
		/* Process not scheduled ... */
		Uint32 need_locks = ~(*rp_locks) & ERTS_PROC_LOCKS_ALL;
		if (need_locks
		    && erts_smp_proc_trylock(rp, need_locks) == EBUSY) {
		    /* ... but we havn't got all locks on it ... */
		    save_pending_exiter(rp->id);
		    /*
		     * The pending exit will be discovered when next
		     * process is scheduled in
		     */
		    goto set_pending_exit;
		}
		else {
		    /* ...and we have all locks on it... */
		    *rp_locks = ERTS_PROC_LOCKS_ALL;
		    set_proc_exiting(rp,
				     (is_immed(rsn)
				      ? rsn
				      : copy_object(rsn, rp)),
				     NULL);
		}
	    }
	    else { /* Process scheduled... */

		/*
		 * The pending exit will be discovered when the process
		 * is scheduled out if not discovered earlier.
		 */

	    set_pending_exit:
		if (is_immed(rsn)) {
		    rp->pending_exit.reason = rsn;
		}
		else {
		    Eterm *hp;
		    Uint sz = size_object(rsn);
		    ErlHeapFragment *bp = new_message_buffer(sz);

		    hp = &bp->mem[0];
		    rp->pending_exit.reason = copy_struct(rsn,
							  sz,
							  &hp,
							  &bp->off_heap);
		    rp->pending_exit.bp = bp;
		}
		ASSERT(ERTS_PROC_PENDING_EXIT(rp));
	    }
	    if (!(rp->status_flags
		  & (ERTS_PROC_SFLG_INRUNQ|ERTS_PROC_SFLG_SCHEDULED)))
		add_to_schedule_q(rp);
	}
	/* else:
	 *
	 *    The receiver already has a pending exit (or is exiting)
	 *    so we drop this signal.
	 *
	 *    NOTE: dropping this exit signal is based on the assumption
	 *          that the receiver *will* exit; either on the pending
	 *          exit or by itself before seeing the pending exit.
	 */
#else /* !ERTS_SMP */
	if (c_p == rp) {
	    rp->status = P_EXITING;
	    c_p->fvalue = rsn;
	}
	else if (rp->status != P_EXITING) { /* No recursive process exits /PaN */
	    Eterm old_status = rp->status;
	    set_proc_exiting(rp,
			     is_immed(rsn) ? rsn : copy_object(rsn, rp),
			     NULL);
	    ACTIVATE(rp);
	    if (old_status != P_RUNABLE && old_status != P_RUNNING)
		add_to_schedule_q(rp);
	}
#endif
	return -1; /* Receiver will exit */
    }

    return 0; /* Receiver unaffected */
}


int
erts_send_exit_signal(Process *c_p,
		      Eterm from,
		      Process *rp,
		      Uint32 *rp_locks,
		      Eterm reason,
		      Eterm token,
		      Process *token_update,
		      Uint32 flags)
{
    return send_exit_signal(c_p,
			    from,
			    rp,
			    rp_locks,
			    reason,
			    THE_NON_VALUE,
			    0,
			    token,
			    token_update,
			    flags);
}

typedef struct {
    Eterm reason;
    Process *p;
} ExitMonitorContext;

static void doit_exit_monitor(ErtsMonitor *mon, void *vpcontext)
{
    ExitMonitorContext *pcontext = vpcontext;
    DistEntry *dep;
    ErtsMonitor *rmon;
    Process *rp;

    if (mon->type == MON_ORIGIN) {
	/* We are monitoring someone else, we need to demonitor that one.. */
	if (is_atom(mon->pid)) { /* remote by name */
	    ASSERT(is_node_name_atom(mon->pid));
	    dep = erts_sysname_to_connected_dist_entry(mon->pid);
	    if (dep) {
		erts_dist_op_prepare(dep, NULL, 0);
		rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		if (rmon) {
		    dist_demonitor(NULL,0,dep,rmon->pid,mon->name,mon->ref,1);
		    erts_destroy_monitor(rmon);
		}
		erts_dist_op_finalize(dep);
		erts_deref_dist_entry(dep);
	    }
	} else {
	    ASSERT(is_pid(mon->pid));
	    if (is_internal_pid(mon->pid)) { /* local by pid or name */
		rp = erts_pid2proc(NULL, 0, mon->pid, ERTS_PROC_LOCK_LINK);
		if (!rp) {
		    goto done;
		}
		rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
		if (rmon == NULL) {
		    goto done;
		}
		erts_destroy_monitor(rmon);
	    } else { /* remote by pid */
		ASSERT(is_external_pid(mon->pid));
		dep = external_pid_dist_entry(mon->pid);
		ASSERT(dep != NULL);
		if (dep) {
		    erts_dist_op_prepare(dep, NULL, 0);
		    rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		    if (rmon) {
			dist_demonitor(NULL,0,dep,rmon->pid,mon->pid,mon->ref,1);
			erts_destroy_monitor(rmon);
		    }
		    erts_dist_op_finalize(dep);
		}
	    }
	}
    } else { /* type == MON_TARGET */
	ASSERT(mon->type == MON_TARGET);
	ASSERT(is_pid(mon->pid) || is_internal_port(mon->pid));
	if (is_internal_port(mon->pid)) {
	    Port *prt = erts_id2port(mon->pid, NULL, 0);
	    if (prt == NULL) {
		goto done;
	    }
	    erts_fire_port_monitor(prt, mon->ref);
	    erts_port_release(prt); 
	} else if (is_internal_pid(mon->pid)) {/* local by name or pid */
	    Eterm watched;
	    Eterm lhp[3];
	    Uint32 rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_MSG_SEND;
	    rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);
	    if (rp == NULL) {
		goto done;
	    }
	    rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	    if (rmon) {
		erts_destroy_monitor(rmon);
		watched = (is_atom(mon->name)
			   ? TUPLE2(lhp, mon->name, 
				    erts_this_dist_entry->sysname)
			   : pcontext->p->id);
		erts_queue_monitor_message(rp, &rp_locks, mon->ref, am_process, 
					   watched, pcontext->reason);
	    }
	    /* else: demonitor while we exited, i.e. do nothing... */
	    erts_smp_proc_unlock(rp, rp_locks);
	} else { /* external by pid or name */
	    ASSERT(is_external_pid(mon->pid));    
	    dep = external_pid_dist_entry(mon->pid);
	    ASSERT(dep != NULL);
	    if (dep) {
		erts_dist_op_prepare(dep, NULL, 0);
		rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		if (rmon) {
		    dist_m_exit(NULL, 0,
				dep, mon->pid, (rmon->name != NIL) 
				? rmon->name : rmon->pid,
				mon->ref, pcontext->reason);
		    erts_destroy_monitor(rmon);
		}
		erts_dist_op_finalize(dep);
	    }
	}
    }
 done:
    /* As the monitors are previously removed from the process, 
       distribution operations will not cause monitors to disappear,
       we can safely delete it. */
       
    erts_destroy_monitor(mon);
}

typedef struct {
    Process *p;
    Eterm reason;
    Eterm exit_tuple;
    Uint exit_tuple_sz;
} ExitLinkContext;

static void doit_exit_link(ErtsLink *lnk, void *vpcontext)
{
    ExitLinkContext *pcontext = vpcontext;
    /* Unpack context, it's readonly */
    Process *p = pcontext->p;
    Eterm reason = pcontext->reason;
    Eterm exit_tuple = pcontext->exit_tuple;
    Uint exit_tuple_sz = pcontext->exit_tuple_sz;
    Eterm item = lnk->pid;
    ErtsLink *rlnk;
    DistEntry *dep;
    Process *rp;

    switch(lnk->type) {
    case LINK_PID:
	if(is_internal_port(item)) {
	    Port *prt = erts_id2port(item, NULL, 0);
	    if (prt) {
		rlnk = erts_remove_link(&prt->nlinks, p->id);
		if (rlnk)
		    erts_destroy_link(rlnk);
		erts_do_exit_port(prt, p->id, reason);
		erts_port_release(prt);
	    }
	}
	else if(is_external_port(item)) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Erroneous link between %T and external port %T "
			  "found\n",
			  p->id,
			  item);
	    erts_send_error_to_logger_nogl(dsbufp);
	    ASSERT(0); /* It isn't possible to setup such a link... */
	}
	else if (is_internal_pid(item)) {
	    Uint32 rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
	    rp = erts_pid2proc(NULL, 0, item, rp_locks);
	    if (rp) {
		rlnk = erts_remove_link(&(rp->nlinks), p->id);
		/* If rlnk == NULL, we got unlinked while exiting,
		   i.e., do nothing... */
		if (rlnk) {
		    int xres;
		    erts_destroy_link(rlnk);
		    xres = send_exit_signal(NULL,
					    p->id,
					    rp,
					    &rp_locks, 
					    reason,
					    exit_tuple,
					    exit_tuple_sz,
					    SEQ_TRACE_TOKEN(p),
					    p,
					    ERTS_XSIG_FLG_IGN_KILL);
		    if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
			/* We didn't exit the process and it is traced */
			if (IS_TRACED_FL(rp, F_TRACE_PROCS)) {
			    trace_proc(p, rp, am_getting_unlinked, p->id);
			}
		    }
		}
		ASSERT(rp != p);
		erts_smp_proc_unlock(rp, rp_locks);
	    }
	}
	else if (is_external_pid(item)) {
	    dep = external_pid_dist_entry(item);
	    if(dep != erts_this_dist_entry) {
		erts_dist_op_prepare(dep, NULL, 0);
		if (SEQ_TRACE_TOKEN(p) != NIL) {
		    seq_trace_update_send(p);
		}
		dist_exit_tt(NULL,0,dep,p->id,item,reason,SEQ_TRACE_TOKEN(p));
		erts_dist_op_finalize(dep);
	    }
	}
	break;
    case LINK_NODE:
	ASSERT(is_node_name_atom(item));
	dep = erts_sysname_to_connected_dist_entry(item);
	if(dep) {
	    /* dist entries have node links in a separate structure to 
	       avoid confusion */
	    erts_smp_dist_entry_lock(dep);
	    rlnk = erts_remove_link(&(dep->node_links), p->id);
	    erts_smp_dist_entry_unlock(dep);
	    if (rlnk != NULL) {
		erts_destroy_link(rlnk);
	    }
	    erts_deref_dist_entry(dep);
	} else {
#ifndef ERTS_SMP
	    /* XXX Is this possible? Shouldn't this link
	       previously have been removed if the node
	       had previously been disconnected. */
	    ASSERT(0);
#endif
	    /* This is possible when smp support has been enabled,
	       and dist port and process exits simultaneously. */
	}
	break;
	
    default:
	erl_exit(1, "bad type in link list\n");
	break;
    }
    erts_destroy_link(lnk);
}


/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
erts_do_exit_process(Process* p, Eterm reason)
{
    ErtsLink* lnk;
    ErtsMonitor *mon;

    p->arity = 0;		/* No live registers */
    p->fvalue = reason;
    
#ifdef ERTS_SMP
    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p->id);
    /* By locking all locks (main lock is already locked) when going
       to status P_EXITING, it is enough to take any lock when
       looking up a process (erts_pid2proc()) to prevent the looked up
       process from exiting until the lock has been released. */
    erts_smp_proc_lock(p,
		       ERTS_PROC_LOCKS_ALL_MINOR|ERTS_PROC_LOCK_FLAG_EXITING);
    p->is_exiting = 1;
#endif

    p->status = P_EXITING;

#ifdef ERTS_SMP

    if (ERTS_PROC_PENDING_EXIT(p)) {
	/* Process exited before pending exit was received... */
	p->pending_exit.reason = THE_NON_VALUE;
	if (p->pending_exit.bp) {
	    free_message_buffer(p->pending_exit.bp);
	    p->pending_exit.bp = NULL;
	}
    }

    cancel_suspend_of_suspendee(p, ERTS_PROC_LOCKS_ALL); 

    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
#endif

    if (IS_TRACED_FL(p,F_TRACE_PROCS))
	trace_proc(p, p, am_exit, reason);

    erts_trace_check_exiting(p->id);

    ASSERT((p->trace_flags & F_INITIAL_TRACE_FLAGS) == F_INITIAL_TRACE_FLAGS);

    cancel_timer(p);		/* Always cancel timer just in case */

    if (p->bif_timers)
	erts_cancel_bif_timers(p, ERTS_PROC_LOCKS_ALL);

    if (p->flags & F_USING_DB)
	db_proc_dead(p->id);

#ifdef ERTS_SMP
    if (p->flags & F_HAVE_BLCKD_MSCHED)
	erts_block_multi_scheduling(p, ERTS_PROC_LOCKS_ALL, 0, 1);
#endif

    if (p->flags & F_USING_DDLL) {
	erts_ddll_proc_dead(p, ERTS_PROC_LOCKS_ALL);
    }

    if (p->nodes_monitors)
	erts_delete_nodes_monitors(p, ERTS_PROC_LOCKS_ALL);

    /*
     * The registered name *should* be the last "erlang resource" to
     * cleanup.
     */
    if (p->reg)
	(void) erts_unregister_name(p, ERTS_PROC_LOCKS_ALL, NULL, p->reg->name);

    {
	int pix;
	erts_smp_mtx_t *ptabix_mtxp;
#ifdef ERTS_SMP
	ptabix_mtxp = &(erts_proc_locks[ERTS_PID2LOCKIX(p->id)].mtx);
#else
	ptabix_mtxp = NULL;
#endif
	
	ASSERT(internal_pid_index(p->id) < erts_max_processes);
	pix = internal_pid_index(p->id);

	erts_smp_mtx_lock(&proc_tab_mtx);
	erts_smp_sched_lock();
	erts_smp_mtx_lock(ptabix_mtxp);

#ifdef ERTS_SMP
	ASSERT(p->scheduler_data);
	ASSERT(p->scheduler_data->current_process == p);
	ASSERT(p->scheduler_data->free_process == NULL);

	p->scheduler_data->current_process = NULL;
	p->scheduler_data->free_process = p;
	p->status_flags = 0;
#endif
	process_tab[pix] = NULL; /* Time of death! */
	ASSERT(erts_smp_atomic_read(&process_count) > 0);
	erts_smp_atomic_dec(&process_count);

	erts_smp_mtx_unlock(ptabix_mtxp);
	erts_smp_sched_unlock();

	if (p_next < 0) {
	    if (p_last >= p_next) {
		p_serial++;
		p_serial &= p_serial_mask;
	    }
	    p_next = pix;
	}

	erts_smp_mtx_unlock(&proc_tab_mtx);
    }

    /*
     * All "erlang resources" have to be deallocated before this point,
     * e.g. registered name, so monitoring and linked processes can
     * be sure that all interesting resources have been deallocated
     * when the monitors and/or links hit.
     */

    mon = p->monitors;
    p->monitors = NULL; /* to avoid recursive deletion during traversal */

    lnk = p->nlinks;
    p->nlinks = NULL;
    p->status = P_FREE;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    processes_busy--;

    if ((p->flags & F_DISTRIBUTION) && p->dist_entry)
	erts_do_net_exits(p->dist_entry, reason);

    /*
     * Pre-build the EXIT tuple if there are any links.
     */
    if (lnk) {
	Eterm tmp_heap[4];
	Eterm exit_tuple;
	Uint exit_tuple_sz;
	Eterm* hp;

	hp = &tmp_heap[0];

	exit_tuple = TUPLE3(hp, am_EXIT, p->id, reason);

	exit_tuple_sz = size_object(exit_tuple);

	{
	    ExitLinkContext context = {p, reason, exit_tuple, exit_tuple_sz};
	    erts_sweep_links(lnk, &doit_exit_link, &context);
	}
    }

    {
	ExitMonitorContext context = {reason, p};
	erts_sweep_monitors(mon,&doit_exit_monitor,&context);
    }

    delete_process(p);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN); /* Make process_main() happy */
#endif
}

/* Callback for process timeout */
static void
timeout_proc(Process* p)
{
    p->i = (Eterm *) p->def_arg_reg[0];
    p->flags |= F_TIMO;
    p->flags &= ~F_INSLPQUEUE;

    if (p->status == P_WAITING)
	add_to_schedule_q(p); 
    if (p->status == P_SUSPENDED)
	p->rstatus = P_RUNABLE;   /* MUST set resume status to runnable */
}


void
cancel_timer(Process* p)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    p->flags &= ~(F_INSLPQUEUE|F_TIMO);
#ifdef ERTS_SMP
    erts_cancel_smp_ptimer(p->ptimer);
#else
    erl_cancel_timer(&p->tm);
#endif
}

/*
 * Insert a process into the time queue, with a timeout 'timeout' in ms.
 */
void
set_timer(Process* p, Uint timeout)
{
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));

    /* check for special case timeout=0 DONT ADD TO time queue */
    if (timeout == 0) {
	p->flags |= F_TIMO;
	return;
    }
    p->flags |= F_INSLPQUEUE;
    p->flags &= ~F_TIMO;

#ifdef ERTS_SMP
    erts_create_smp_ptimer(&p->ptimer,
			   p->id,
			   (ErlTimeoutProc) timeout_proc,
			   timeout);
#else
    erl_set_timer(&p->tm,
		  (ErlTimeoutProc) timeout_proc,
		  NULL,
		  (void*) p,
		  timeout);
#endif
}

/*
 * Stack dump functions follow.
 */

void
erts_stack_dump(int to, void *to_arg, Process *p)
{
    Eterm* sp;
    int yreg = -1;

    if (p->trace_flags & F_SENSITIVE) {
	return;
    }
    erts_program_counter_info(to, to_arg, p);
    for (sp = p->stop; sp < STACK_START(p); sp++) {
        yreg = stack_element_dump(to, to_arg, p, sp, yreg);
    }
}

void
erts_program_counter_info(int to, void *to_arg, Process *p)
{
    int i;

    erts_print(to, to_arg, "Program counter: %p (", p->i);
    print_function_from_pc(to, to_arg, p->i);
    erts_print(to, to_arg, ")\n");
    erts_print(to, to_arg, "CP: %p (", p->cp);
    print_function_from_pc(to, to_arg, p->cp);
    erts_print(to, to_arg, ")\n");
    if (!((p->status == P_RUNNING) || (p->status == P_GARBING))) {
        erts_print(to, to_arg, "arity = %d\n",p->arity);
        for (i = 0; i < p->arity; i++)
            erts_print(to, to_arg, "   %T\n", p->arg_reg[i]);
    }
}

static void
print_function_from_pc(int to, void *to_arg, Eterm* x)
{
    Eterm* addr = find_function_from_pc(x);
    if (addr == NULL) {
        if (x == beam_exit) {
            erts_print(to, to_arg, "<terminate process>");
        } else if (x == beam_apply+1) {
            erts_print(to, to_arg, "<terminate process normally>");
        } else {
            erts_print(to, to_arg, "unknown function");
        }
    } else {
	erts_print(to, to_arg, "%T:%T/%d + %d",
		   addr[0], addr[1], addr[2], ((x-addr)-2) * sizeof(Eterm));
    }
}

static int
stack_element_dump(int to, void *to_arg, Process* p, Eterm* sp, int yreg)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erts_print(to, to_arg, "\n%p ", sp);
    } else {
        char sbuf[16];
        sprintf(sbuf, "y(%d)", yreg);
        erts_print(to, to_arg, "%-8s ", sbuf);
        yreg++;
    }

    if (is_CP(x)) {
        erts_print(to, to_arg, "Return addr %p (", (Eterm *) x);
        print_function_from_pc(to, to_arg, cp_val(x));
        erts_print(to, to_arg, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        erts_print(to, to_arg, "Catch %p (", catch_pc(x));
        print_function_from_pc(to, to_arg, catch_pc(x));
        erts_print(to, to_arg, ")\n");
    } else {
	erts_print(to, to_arg, "%T\n", x);
    }
    return yreg;
}


