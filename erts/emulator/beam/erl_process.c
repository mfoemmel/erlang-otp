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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_instrument.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_init_process() */
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

static Sint p_next;
static Sint p_serial;
static Uint p_serial_mask;
static Uint p_serial_shift;

Uint erts_max_processes = ERTS_DEFAULT_MAX_PROCESSES;
Uint erts_process_tab_index_mask;

Uint erts_tot_proc_mem; /* in bytes */

typedef struct schedule_q {
    Process* first;
    Process* last;
} ScheduleQ;

/* we use the same queue for low and normal prio processes */
static ScheduleQ queue[NPRIORITY_LEVELS-1];
static unsigned qmask;

static Uint queued_low;
static Uint queued_normal;

#ifndef BM_COUNTERS
static int processes_busy;
#endif


Process**  process_tab;
Uint context_switches;		/* no of context switches */
Uint reductions;		/* total number of reductions */
Uint last_reds;			/* used in process info */
Uint erts_default_process_flags;
Eterm erts_default_tracer;
Eterm erts_system_monitor;
Eterm erts_system_monitor_long_gc;
Eterm erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t erts_system_monitor_flags;

const struct trace_pattern_flags erts_trace_pattern_flags_off = {0, 0, 0, 0};

int                         erts_default_trace_pattern_is_on;
Binary                     *erts_default_match_spec;
Binary                     *erts_default_meta_match_spec;
struct trace_pattern_flags  erts_default_trace_pattern_flags;
Eterm                       erts_default_meta_tracer_pid;

#if defined(SHARED_HEAP) || defined(HYBRID)
Uint erts_num_active_procs;
Process** erts_active_procs;
#endif

/*
 * Local functions.
 */
static void delete_process(Process* p);
static void print_function_from_pc(Eterm* x, CIO fd);
static int stack_element_dump(Process* p, Eterm* sp, int yreg, CIO fd);

/* initialize the scheduler */
void
init_scheduler(void)
{
    int i;
    Uint proc_bits = ERTS_PROC_BITS;

    if (erts_use_r9_pids_ports) {
	proc_bits = ERTS_R9_PROC_BITS;
	ASSERT(erts_max_processes <= (1 << ERTS_R9_PROC_BITS));
    }

    erts_tot_proc_mem = 0;

    process_tab = (Process**) erts_alloc(ERTS_ALC_T_PROC_TABLE,
					 erts_max_processes*sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    sys_memzero(process_tab, erts_max_processes * sizeof(Process*));
#if defined(SHARED_HEAP) || defined(HYBRID)
    erts_active_procs = (Process**)
        erts_alloc(ERTS_ALC_T_ACTIVE_PROCS,
                   erts_max_processes * sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    erts_num_active_procs = 0;
#endif

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
#ifndef BM_COUNTERS
    processes_busy = 0;
#endif
    context_switches = 0;
    reductions = 0;
    last_reds = 0;
    erts_default_process_flags = 0;
    erts_default_tracer = NIL;
    erts_system_monitor_clear();
    
    erts_default_trace_pattern_is_on = 0;
    erts_default_match_spec = NULL;
    erts_default_meta_match_spec = NULL;
    erts_default_trace_pattern_flags = erts_trace_pattern_flags_off;
    erts_default_meta_tracer_pid = NIL;
}

int
sched_q_len(void)
{
    int i;
    int len = 0;

    for (i = 0; i < NPRIORITY_LEVELS - 1; i++) {
	Process* p;

	for (p = queue[i].first; p != NULL; p = p->next) {
	    len++;
	}
    }
    return len;
}

/* schedule a process */
void
add_to_schedule_q(Process *p)
{
    ScheduleQ* sq;

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

    /* Never schedule a suspended process */
    ASSERT(p->status != P_SUSPENDED);

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
}

/* Possibly remove a scheduled process we need to suspend */

int
remove_proc_from_sched_q(Process *p)
{
    Process *tmp, *prev;
    int i;

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

	    return 1;
	}
	if (sq->first == p) {
	    sq->first = sq->first->next;
	    DECR_PROC_COUNT(p->prio);
	    return 1;
	}
	tmp = sq->first->next;
	prev = sq->first;
	while (tmp) {
	    if (tmp == p) {
		prev->next = tmp->next;
		DECR_PROC_COUNT(p->prio);
		if (p == sq->last)
		    sq->last = prev;
		return 1;
	    }
	    prev = tmp;
	    tmp = tmp->next;
	}
    }
    return 0;
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
    static int function_calls;

    /*
     * Clean up after the process being suspended.
     */
    if (p) {	/* NULL in the very first schedule() call */
	function_calls += calls;
	reductions += calls;

#ifdef SHARED_HEAP
        ASSERT(p->heap && p->htop && p->hend && p->heap_sz);
        ASSERT(!(global_heap || global_htop || global_hend || global_heap_sz));
        global_htop = p->htop;
        global_heap = p->heap;
        global_hend = p->hend;
        global_heap_sz = p->heap_sz;
        p->htop = NULL;
        p->heap = NULL;
        p->hend = NULL;
        p->heap_sz = 0;
#endif

	ERTS_INSTR_RESET_CURR_PROC();

	p->reds += calls;
	if (p->status == P_FREE) {
	    ERTS_PROC_LESS_MEM(sizeof(Process));
	    erts_free(ERTS_ALC_T_PROC, (void *) p);
	} else if (IS_TRACED_FL(p, F_TRACE_SCHED)) {
	    trace_sched(p, am_out);
	}

	if (do_time) {
	    bump_timer();
	}
        BM_STOP_TIMER(system);
    }

    /*
     * Find a new process to run.
     */
 pick_next_process:
    if (function_calls <= INPUT_REDUCTIONS) { 
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
	    goto do_sys_schedule;
#ifdef DEBUG
	default:
	    ASSERT(0);
#else
	default:
	    goto do_sys_schedule; /* Should not happen ... */
#endif
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

	ASSERT(p->status != P_SUSPENDED); /* Never run a suspended process */
        ACTIVATE(p);
#ifdef SHARED_HEAP
        ASSERT(!p->heap && !p->htop && !p->hend && !p->heap_sz);
        p->htop = global_htop;
        p->heap = global_heap;
        p->hend = global_hend;
        p->heap_sz = global_heap_sz;
        global_htop = NULL;
        global_heap = NULL;
        global_hend = NULL;
        global_heap_sz = 0;
#endif
	context_switches++;
	calls = CONTEXT_REDS;
	if (p->status != P_EXITING) {
	    if (IS_TRACED_FL(p, F_TRACE_SCHED)) {
		trace_sched(p, am_in);
	    }
	    p->status = P_RUNNING;
	}

	if (((MBUF_SIZE(p) + MSO(p).overhead) * MBUF_GC_FACTOR) >= HEAP_SIZE(p)) {
	    calls -= erts_garbage_collect(p, 0, p->arg_reg, p->arity);
	    if (calls < 0) {
		calls = 1;
	    }
	}

	ERTS_INSTR_SET_CURR_PROC(p->id);

	p->fcalls = calls;
	ASSERT(IS_ACTIVE(p));
	return p;
    }

    /*
     * Schedule system-level activities.
     */
 do_sys_schedule:
    erl_sys_schedule(qmask);
    function_calls = 0;
    if (do_time) {
	bump_timer();
    }
    goto pick_next_process;
}


/*
 * erts_test_next_pid() is only used for testing.
 */
Sint
erts_test_next_pid(int set, Uint next)
{
    Sint p_prev;

    if (!set) {
	if (p_next < 0)
	    return -1;
	return (p_serial << p_serial_shift | p_next);
    }

    p_serial = (Sint) ((next >> p_serial_shift) & p_serial_mask);
    p_next = (Sint) (erts_process_tab_index_mask & next);

    if(p_next >= erts_max_processes) {
	p_next = 0;
	p_serial = (p_serial+1) & p_serial_mask;
    }

    p_prev = p_next;

    do {
	if (!process_tab[p_next])
	    break;
	p_next++;
	if(p_next >= erts_max_processes) {
	    p_next = 0;
	    p_serial = (p_serial+1) & p_serial_mask;
	}
    } while(p_prev != p_next);

    if (process_tab[p_next])
	return -1;

    return (p_serial << p_serial_shift | p_next);
}

/*
** Fix allocate a process
*/
static Process*
alloc_process(void)
{
    Process* p;
    int p_prev;

    if (p_next == -1)
	return NULL;

    p = (Process*) erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;
    ERTS_PROC_MORE_MEM(sizeof(Process));
    p->id = make_internal_pid(p_serial << p_serial_shift | p_next);
    ASSERT(internal_pid_serial(p->id) <= (erts_use_r9_pids_ports
					  ? ERTS_MAX_PID_R9_SERIAL
					  : ERTS_MAX_PID_SERIAL));
    p->rstatus = P_FREE;
    p->rcount = 0;

    /* set p_next to the next available slot */
    p_prev = p_next;

    p_next++;
    if(p_next >= erts_max_processes)
	p_next = 0;

    while(p_prev != p_next) {
	if (p_next == 0)
	    p_serial = (p_serial+1) & p_serial_mask;
	if (process_tab[p_next] == NULL)
	    /* found a free slot */
	    return p;
	p_next++;
	if(p_next >= erts_max_processes)
	    p_next = 0;
    }

    p_next = -1;
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
#if !(defined(SHARED_HEAP) || defined(HYBRID))
    Uint arg_size;		/* Size of arguments. */
#endif
#ifndef SHARED_HEAP
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
#endif
    ScheduleQ* sq;

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
	return THE_NON_VALUE;
    }
    if ((p = alloc_process()) == NULL) {
	cerr_pos = 0;
	erl_printf(CBUF, "Too many processes\n");
	send_error_to_logger(parent->group_leader);
	so->error_code = SYSTEM_LIMIT;
	return THE_NON_VALUE;
    }

    processes_busy++;
    BM_COUNT(processes_spawned);

#if !(defined(SHARED_HEAP) || defined(HYBRID))
    BM_SWAP_TIMER(system,size);
    arg_size = size_object(args);
    BM_SWAP_TIMER(size,system);
    heap_need = arg_size;
#endif

    p->flags = erts_default_process_flags;

    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size = so->min_heap_size;
	p->prio = so->priority;
#ifndef SHARED_HEAP
	p->max_gen_gcs = so->max_gen_gcs;
#endif
    } else {
	p->min_heap_size = H_MIN_SIZE;
	p->prio = PRIORITY_NORMAL;
#ifndef SHARED_HEAP
	p->max_gen_gcs = erts_max_gen_gcs;
#endif
    }
    p->skipped = 0;
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

#ifndef SHARED_HEAP
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
#ifdef HEAP_FRAG_ELIM_TEST
    p->arith_lowest_htop = (Eterm *) 0;
    p->halloc_mbuf = NULL;
#endif
#endif


#ifdef HIPE
    hipe_init_process(&p->hipe);
#endif

#ifdef SHARED_HEAP
    p->send = (Eterm *) ERTS_STACK_ALLOC(sizeof(Eterm) * S_DEFAULT_SIZE);
    p->stop = p->stack = p->send + S_DEFAULT_SIZE;
    p->stack_sz = S_DEFAULT_SIZE;
    p->htop = NULL;
    p->hend = NULL;
    p->heap = NULL;
    p->heap_sz = 0;
#else
    p->heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*sz);
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
#ifdef INCREMENTAL_GC
    p->scan_top = p->high_water;
#endif
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
    p->arith_avail = 0;		/* No arithmetic heap. */
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
#ifdef HEAP_FRAG_ELIM_TEST
    p->saved_htop = NULL;
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
#if defined(SHARED_HEAP) || defined(HYBRID)
    p->arg_reg[2] = args;
#ifdef INCREMENTAL_GC
    p->active = 0;
#endif
    INC_ACTIVATE(p);
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
    sys_memset(&p->tm, 0, sizeof(ErlTimer));

    p->reg = NULL;
    p->dist_entry = NULL;
    p->error_handler = am_error_handler;    /* default */
    p->nlinks = NULL;
    p->monitors = NULL;
    p->ct = NULL;

#ifdef SHARED_HEAP
    p->group_leader = parent->group_leader;
#else
    /* Needs to be done after the heap has been set up */
    p->group_leader =
	IS_CONST(parent->group_leader)
	? parent->group_leader
	: STORE_NC(&p->htop, &p->off_heap.externals, parent->group_leader);
#endif
    ASSERT(IS_CONST(erts_default_tracer));
    p->tracer_proc = erts_default_tracer;

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifndef SHARED_HEAP
    p->mbuf = NULL;
    p->mbuf_sz = 0;
#endif
    p->dictionary = NULL;
    p->debug_dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
    p->parent = parent->id;
    p->started = erts_get_time();

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif

    process_tab[internal_pid_index(p->id)] = p;

    if (IS_TRACED(parent)) {
	if (parent->flags & F_TRACE_SOS) {
	    p->flags |= (parent->flags & TRACE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	}
	if (parent->flags & F_TRACE_PROCS) 
	    trace_proc_spawn(parent, p->id, mod, func, args);
	if (parent->flags & F_TRACE_SOS1) { /* Overrides TRACE_CHILDREN */
	    p->flags |= (parent->flags & TRACE_FLAGS);
	    p->tracer_proc = parent->tracer_proc;
	    p->flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	    parent->flags &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	}
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
#ifdef DEBUG
	int ret;
#endif
	if (IS_TRACED(parent) && (parent->flags & F_TRACE_PROCS) != 0) {
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
	    if (parent->flags & F_TRACE_SOL)  {
		p->flags |= (parent->flags & TRACE_FLAGS);
		p->tracer_proc = parent->tracer_proc;    /* maybe steal */
	    }
	    if (parent->flags & F_TRACE_SOL1)  { /* maybe override */
		p->flags |= (parent->flags & TRACE_FLAGS);
		p->tracer_proc = parent->tracer_proc;   
		p ->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		parent->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
	    }
	}
    }

#if defined(SHARED_HEAP) || defined(HYBRID)
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

    p->next = NULL;
    if (sq->first == (Process *) 0)
	sq->first = p;
    else
	sq->last->next = p;
    sq->last = p;
    p->status = P_RUNABLE;
    return p->id;
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
#ifdef SHARED_HEAP
    p->stack = NULL;
    p->send  = NULL;
    p->stack_sz = 0;
#else
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
#ifdef HEAP_FRAG_ELIM_TEST
    p->arith_lowest_htop = (Eterm *) 0;
    p->saved_htop = NULL;
    p->halloc_mbuf = NULL;
#endif
#endif
    p->min_heap_size = 0;
    p->status = P_RUNABLE;
    p->rstatus = P_RUNABLE;
    p->rcount = 0;
    p->id = NIL;
    p->prio = PRIORITY_NORMAL;
    p->reds = 0;
    p->error_handler = am_error_handler;
    p->tracer_proc = NIL;
    p->group_leader = NIL;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->fcalls = 0;
    p->dist_entry = NULL;
    memset(&(p->tm), 0, sizeof(ErlTimer));
    p->next = NULL;
#ifndef SHARED_HEAP
    p->off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    p->off_heap.funs = NULL;
#endif
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;
#endif
    p->reg = NULL;
    p->heap_sz = 0;
#ifndef SHARED_HEAP
    p->high_water = NULL;
#ifdef INCREMENTAL_GC
    p->scan_top = NULL;
#endif
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
#endif
    p->nlinks = NULL;         /* List of links */
    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
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
#ifndef SHARED_HEAP
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
#endif

    ACTIVATE(p);

#ifdef HYBRID
    p->rrma  = NULL;
    p->rrsrc = NULL;
    p->nrr   = 0;
    p->rrsz  = 0;
#endif
}    

void
erts_cleanup_empty_process(Process* p)
{
#ifdef SHARED_HEAP
    ;
#else
    ErlHeapFragment* ptr = p->mbuf;

    erts_cleanup_offheap(&p->off_heap);
    while (ptr) {
	ErlHeapFragment*next = ptr->next;
	free_message_buffer(ptr);
	ptr = next;
    }
#endif
}

static void delete_a_monitor(ErtsMonitor *mon, void *dummy)
{
    erts_destroy_monitor(mon);
}

static void delete_a_link(ErtsLink *lnk, void *dummy)
{
    erts_destroy_link(lnk);
}

static void
delete_process0(Process* p, int do_delete)
{
    ErlMessage* mp;
#ifndef SHARED_HEAP
    ErlHeapFragment* bp;
#endif
    int i;

#ifndef SHARED_HEAP
    /* Clean binaries and funs */
    erts_cleanup_offheap(&p->off_heap);

    /*
     * The mso list should not be used anymore, but if it is, make sure that
     * we'll notice.
     */
    p->off_heap.mso = (void *) 0x8DEFFACD;
#endif

    if (p->arg_reg != p->def_arg_reg) {
	ERTS_PROC_LESS_MEM(p->max_arg_reg * sizeof(p->arg_reg[0]));
	erts_free(ERTS_ALC_T_ARG_REG, p->arg_reg);
    }

    /*
     * Release heaps. Clobber contents in DEBUG build.
     */

#ifdef SHARED_HEAP
#ifdef DEBUG
    sys_memset(p->send, 0xfb, p->stack_sz*sizeof(Eterm));
#endif
#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif
    ERTS_STACK_FREE((void*) p->send, p->stack_sz * sizeof(Eterm));
#else /* not SHARED_HEAP */
#ifdef DEBUG
    sys_memset(p->heap, 0xfb, p->heap_sz*sizeof(Eterm));
#endif
#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif
    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP, (void*) p->heap, p->heap_sz*sizeof(Eterm));
    if (p->old_heap != NULL) {
#ifdef DEBUG
	sys_memset(p->old_heap, 0xfb, (p->old_hend-p->old_heap)*sizeof(Eterm));
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
#endif /* SHARED_HEAP */

    erts_erase_dicts(p);

    /* free all pending messages */
    mp = p->msg.first;
    while(mp != NULL) {
	ErlMessage* next_mp = mp->next;
	free_message(mp);
	mp = next_mp;
    }
    /* Free all monitors */
    erts_sweep_monitors(p->monitors,&delete_a_monitor, NULL);
    p->monitors = NULL;

    /* free all links */
    erts_sweep_links(p->nlinks, &delete_a_link, NULL);
    p->nlinks = NULL;

    if (p->ct != NULL) {
	ERTS_PROC_LESS_MEM((sizeof(struct saved_calls)
			    + (p->ct->len - 1) * sizeof(Export *)));
        erts_free(ERTS_ALC_T_CALLS_BUF, (void *) p->ct);
    }

    if (p->flags & F_USING_DB)
	db_proc_dead(p->id);

    ASSERT(internal_pid_index(p->id) < erts_max_processes);
    i = internal_pid_index(p->id);

    process_tab[i] = NULL;
    if (p_next == -1)
	p_next = i;

    if(p->dist_entry) {
	DEREF_DIST_ENTRY(p->dist_entry);
	p->dist_entry = NULL;
    }

    p->fvalue = NIL;
    
#if defined(SHARED_HEAP) || defined(HYBRID)
    erts_active_procs[p->active_index] =
        erts_active_procs[--erts_num_active_procs];
    erts_active_procs[p->active_index]->active_index = p->active_index;
#ifdef INCREMENTAL_GC
    if (INC_IS_ACTIVE(p))
         INC_DEACTIVATE(p);
#endif
#endif

#ifdef HYBRID
    if (p->rrma != NULL) {
        erts_free(ERTS_ALC_T_ROOTSET,p->rrma);
        erts_free(ERTS_ALC_T_ROOTSET,p->rrsrc);
        ERTS_PROC_LESS_MEM(sizeof(Eterm) * p->rrsz * 2);
    }
#endif

    /*
     * Don't free it here, just mark it.
     */
    if (do_delete) {
	ERTS_PROC_LESS_MEM(sizeof(Process));
        erts_free(ERTS_ALC_T_PROC, (void *) p);
    } else {
        p->status = P_FREE;
    }
    processes_busy--;
}

/*
 * p must be the currently executing process.
 */
static void
delete_process(Process* p)
{
   if (p->reg != NULL)
      unregister_name(p, p->reg->name);

   cancel_timer(p);		/* Always cancel timer just in case */
   delete_process0(p, 0);
}

void
schedule_exit(Process *p, Eterm reason)
{
    Eterm copy;
    Uint32 status = p->status;

    /*
     * If this is the currently running process, we'll only change its
     * status to P_EXITING, and do nothing more.  It's the responsibility
     * of the caller to make the current process exit.
     */
    p->status = P_EXITING;
    if (status == P_RUNNING)
	return;

    copy = copy_object(reason, p);
    
    ACTIVATE(p);
    p->fvalue = copy;
    cancel_timer(p);
    p->freason = EXC_EXIT;
    KILL_CATCHES(p);
    p->i = (Eterm *) beam_exit;
    if (status != P_RUNABLE) {
	add_to_schedule_q(p);
    }
}

/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

static void
send_exit_message(Process *to, Eterm exit_term, Uint term_size, Eterm token)
{
#ifdef SHARED_HEAP
    if (token != NIL) {
	ASSERT(token == NIL);
    } else {
	queue_message_tt(to, NULL, exit_term, NIL);
    }
#else
    if (token == NIL) {
	Eterm* hp;
	Eterm mess;

	hp = HAlloc(to, term_size);
	mess = copy_struct(exit_term, term_size, &hp, &MSO(to));
	queue_message_tt(to, NULL, mess, NIL);
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
	mess = copy_struct(exit_term, term_size, &hp, &MSO(to));
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, mess, SEQ_TRACE_SEND, to->id, NULL);
	temp_token = copy_struct(token, sz_token, &hp, &MSO(to));
	queue_message_tt(to, bp, mess, temp_token);
    }
#endif
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
	    if (!dep) {
		goto done;
	    }
	    rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
	    if (!rmon) {
		goto done;
	    }
	    dist_demonitor(dep, rmon->pid, mon->name, mon->ref, 1);
	    erts_destroy_monitor(rmon);
	} else {
	    ASSERT(is_pid(mon->pid));
	    if (is_internal_pid(mon->pid)) { /* local by pid or name */
		if ((rp = pid2proc(mon->pid)) == NULL) {
		    goto done;
		}
		rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
		if (rmon == NULL) {
		    goto done;
		}
		erts_destroy_monitor(rmon);
	    } else { /* remote by pid */
		ASSERT(is_external_pid(mon->pid));
		dep = external_pid_dist_entry(mon->pid);
		ASSERT(dep != NULL);
		if (dep == NULL) {
		    goto done;
		}
		rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
		if (rmon == NULL) {
		    goto done;
		}
		dist_demonitor(dep, rmon->pid, mon->pid, mon->ref, 1);
		erts_destroy_monitor(rmon);
	    }
	}
    } else { /* type == MON_TARGET */
	ASSERT(mon->type == MON_TARGET && is_pid(mon->pid));
	if (is_internal_pid(mon->pid)) {/* local by name or pid */
	    Eterm watched;
	    Eterm lhp[3];
	    rp = pid2proc(mon->pid);
	    if (rp == NULL) {
		goto done;
	    }
	    rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	    /* rmon might be NULL, when the sequende eixt(Pid,...),
	       demonitor(Ref) has been called without context switch, 
	       there's a testcase for this. monitor_SUITE:demon_2 */
	    if (rmon != NULL) {
		erts_destroy_monitor(rmon);
	    }
	    watched = (is_atom(mon->name)
		       ? TUPLE2(lhp, mon->name, 
				erts_this_dist_entry->sysname)
		       : pcontext->p->id);
	    queue_monitor_message(rp, mon->ref, am_process, 
				  watched, pcontext->reason);
	} else { /* external by pid or name */
	    ASSERT(is_external_pid(mon->pid));
	    dep = external_pid_dist_entry(mon->pid);
	    ASSERT(dep != NULL);
	    if (dep == NULL) {
		goto done;
	    }
	    rmon = erts_remove_monitor(&(dep->monitors), mon->ref);
	    if (rmon == NULL) {
		goto done;
	    }
	    dist_m_exit(dep, mon->pid, (rmon->name != NIL) 
			? rmon->name : rmon->pid, mon->ref, pcontext->reason);
	    erts_destroy_monitor(rmon);
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
    int ix;
    ErtsLink *rlnk;
    DistEntry *dep;
    Process *rp;

    switch(lnk->type) {
    case LINK_PID:
	if(is_internal_port(item)) {
	    ix = internal_port_index(item);
	    if (! INVALID_PORT(erts_port+ix, item)) {
		rlnk = erts_remove_link(&(erts_port[ix].nlinks),
					p->id);
		if (rlnk != NULL) {
		    erts_destroy_link(rlnk);
		}
		do_exit_port(item, p->id, reason);
	    }
	}
	else if(is_external_port(item)) {
	    dep = external_port_dist_entry(item);
	    if(dep != erts_this_dist_entry)
		dist_exit(dep, p->id, item, reason);
	}
	else if (is_internal_pid(item)) {
	    if ((rp = pid2proc(item)) != NULL) {
		rlnk = erts_remove_link(&(rp->nlinks), p->id);
		if (rlnk != NULL) {
		    erts_destroy_link(rlnk);
		}
		if (rp->flags & F_TRAPEXIT) {
		    if (SEQ_TRACE_TOKEN(p) != NIL ) {
			seq_trace_update_send(p);
		    }
		    send_exit_message(rp, exit_tuple, exit_tuple_sz,
				      SEQ_TRACE_TOKEN(p));
		    if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlnk != NULL) {
			trace_proc(p, rp, am_getting_unlinked, p->id);
		    }
		} else if (reason == am_normal) {
		    if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlnk != NULL) {
			trace_proc(p, rp, am_getting_unlinked, p->id);
		    }
		} else {
		    schedule_exit(rp, reason);
		} 
	    }
	}
	else if (is_external_pid(item)) {
	    dep = external_pid_dist_entry(item);
	    if(dep != erts_this_dist_entry) {
		if (SEQ_TRACE_TOKEN(p) != NIL) {
		    seq_trace_update_send(p);
		}
		dist_exit_tt(dep, p->id, item, reason, SEQ_TRACE_TOKEN(p));
	    }
	}
	break;
    case LINK_NODE:
	ASSERT(is_node_name_atom(item));
	dep = erts_sysname_to_connected_dist_entry(item);
	if(dep) {
	    /* dist entries have node links in a separate structure to 
	       avoid confusion */
	    rlnk = erts_remove_link(&(dep->node_links), p->id);
	    if (rlnk != NULL) {
		erts_destroy_link(rlnk);
	    }
	} else {
	    /* XXX Is this possible? Shouldn't this link
	       previously have been removed if the node
	       had previously been disconnected. */
	    ASSERT(0);
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
do_exit(Process* p, Eterm reason)
{
    ErtsLink* lnk;
    Eterm exit_tuple = NIL;
    Uint exit_tuple_sz = 0;
    ErtsMonitor *mon;

    p->arity = 0;		/* No live registers */
    p->fvalue = reason;
    p->status = P_EXITING;
    
    if (IS_TRACED_FL(p,F_TRACE_PROCS))
	trace_proc(p, p, am_exit, reason);
    
    if (EQ(erts_system_monitor, p->id)) {
	erts_system_monitor_clear();
    }
    
    if (p->flags & F_TRACER) {
	if (EQ(erts_default_tracer, p->id)) {
	    erts_default_tracer = NIL;
	    erts_default_process_flags &= ~TRACE_FLAGS;
	}
    }
    
    mon = p->monitors;
    p->monitors = NULL; /* to avoid recursive deletion during traversal */
    {
	ExitMonitorContext context = {reason,p};
	erts_sweep_monitors(mon,&doit_exit_monitor,&context);
    }

    lnk = p->nlinks;
    p->nlinks = NULL;
    
    /*
     * Pre-build the EXIT tuple if there are any links.
     */
    if (lnk != NULL) {
	Eterm* hp;
	if (HEAP_LIMIT(p) - HEAP_TOP(p) <= 4) {
	    (void) erts_garbage_collect(p, 4, NULL, 0);
	    reason = p->fvalue;
	}
	hp = HEAP_TOP(p);
	HEAP_TOP(p) += 4;
	exit_tuple = TUPLE3(hp, am_EXIT, p->id, reason);
#ifndef SHARED_HEAP
	exit_tuple_sz = size_object(exit_tuple);
#endif
    }

    {
	ExitLinkContext context = {p, reason, exit_tuple, exit_tuple_sz};
	erts_sweep_links(lnk, &doit_exit_link, &context);
    }

    if ((p->flags & F_DISTRIBUTION) && p->dist_entry)
	do_net_exits(p->dist_entry);

    delete_process(p);
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
    erl_cancel_timer(&p->tm);
    p->flags &= ~(F_INSLPQUEUE|F_TIMO);
}

/*
 * Insert a process into the time queue, with a timeout 'timeout' in ms.
 */
void
set_timer(Process* p, Uint timeout)
{
    /* check for special case timeout=0 DONT ADD TO time queue */
    if (timeout == 0) {
	p->flags |= F_TIMO;
	return;
    }
    erl_set_timer(&p->tm,
		  (ErlTimeoutProc) timeout_proc,
		  NULL,
		  (void*) p,
		  timeout);
    p->flags |= F_INSLPQUEUE;
    p->flags &= ~F_TIMO;
}

/*
 * Stack dump functions follow.
 */

void
erts_stack_dump(Process *p, CIO fd)
{
    Eterm* sp;
    int yreg = -1;

    erts_program_counter_info(p, fd);
    for (sp = p->stop; sp < STACK_START(p); sp++) {
        yreg = stack_element_dump(p, sp, yreg, fd);
    }
}

void
erts_program_counter_info(Process *p, CIO fd)
{
    int i;

    erl_printf(fd, "Program counter: 0x%x (", p->i);
    print_function_from_pc(p->i, fd);
    erl_printf(fd, ")\n");
    erl_printf(fd, "CP: 0x%x (", p->cp);
    print_function_from_pc(p->cp, fd);
    erl_printf(fd, ")\n");
    if (!((p->status == P_RUNNING) || (p->status == P_GARBING))) {
        erl_printf(fd, "arity = %d\n",p->arity);
        for (i = 0; i < p->arity; i++) {
            erl_printf(fd, "   ");
            display(p->arg_reg[i], fd);
            erl_printf(fd, "\n");
        }
    }
}

static void
print_function_from_pc(Eterm* x, CIO fd)
{
    Eterm* addr = find_function_from_pc(x);
    if (addr == NULL) {
        if (x == beam_exit) {
            sys_printf(fd, "<terminate process>");
        } else if (x == beam_apply+1) {
            sys_printf(fd, "<terminate process normally>");
        } else {
            sys_printf(fd, "unknown function");
        }
    } else {
        display(addr[0], fd);
        sys_printf(fd, ":");
        display(addr[1], fd);
        sys_printf(fd, "/%d", addr[2]);
        sys_printf(fd, " + %d", ((x-addr)-2) * sizeof(Eterm));
    }
}

static int
stack_element_dump(Process* p, Eterm* sp, int yreg, CIO fd)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erl_printf(fd, "\n%-8p ", sp);
    } else {
        char sbuf[16];
        sprintf(sbuf, "y(%d)", yreg);
        sys_printf(fd, "%-8s ", sbuf);
        yreg++;
    }

    if (is_CP(x)) {
        sys_printf(fd, "Return addr 0x%X (", (Eterm *) x);
        print_function_from_pc(cp_val(x), fd);
        sys_printf(fd, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        sys_printf(fd, "Catch 0x%X (", catch_pc(x));
        print_function_from_pc(catch_pc(x), fd);
        sys_printf(fd, ")\n");
    } else {
        display(x, fd);
        erl_putc('\n', fd);
    }
    return yreg;
}
