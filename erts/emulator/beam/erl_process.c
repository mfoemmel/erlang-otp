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
#include "error.h"
#include "beam_catches.h"
#include "erl_instrument.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_init_process() */
#endif

#define MAX_BIT          (1 << PRIORITY_MAX)
#define HIGH_BIT         (1 << PRIORITY_HIGH)
#define NORMAL_BIT       (1 << PRIORITY_NORMAL)
#define LOW_BIT          (1 << PRIORITY_LOW)

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

static ScheduleQ queue[NPRIORITY_LEVELS];
static int bg_count;
static unsigned qmask;
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

#ifdef SHARED_HEAP
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

    erts_tot_proc_mem = 0;

    process_tab = (Process**) erts_alloc(ERTS_ALC_T_PROC_TABLE,
					 erts_max_processes*sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    sys_memzero(process_tab, erts_max_processes * sizeof(Process*));
#ifdef SHARED_HEAP
    erts_active_procs = (Process**)
        erts_alloc(ERTS_ALC_T_ACTIVE_PROCS, erts_max_processes*sizeof(Process*));
    ERTS_PROC_MORE_MEM(erts_max_processes * sizeof(Process*));
    erts_num_active_procs = 0;
#endif

    p_next = 0;
    p_serial = 0;

    p_serial_shift = erts_fit_in_bits(erts_max_processes - 1);
    p_serial_mask = ((~(~((Uint) 0) << ERTS_PROCESSES_BITS)) >> p_serial_shift);
    erts_process_tab_index_mask = ~(~((Uint) 0) << p_serial_shift);

    /* mark the schedule queue as empty */
    for(i = 0; i < NPRIORITY_LEVELS; i++)
	queue[i].first = queue[i].last = (Process*) 0;
    qmask = 0;
#ifndef BM_COUNTERS
    processes_busy = 0;
#endif
    bg_count = 0;
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

    for (i = 0; i < NPRIORITY_LEVELS; i++) {
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
    ScheduleQ* sq = &queue[p->prio];

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

    for(i = 0; i < NPRIORITY_LEVELS; i++) {
	ScheduleQ *sq = &queue[i];

	if (sq->first == (Process*) NULL)
	    continue;
	if (sq->first == sq->last && sq->first == p) {
	    sq->first = sq->last = NULL;
	    qmask &= ~(1 << p->prio);
	    return 1;
	}
	if (sq->first == p) {
	    sq->first = sq->first->next;
	    return 1;
	}
	tmp = sq->first->next;
	prev = sq->first;
	while (tmp) {
	    if (tmp == p) {
		prev->next = tmp->next;
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
	case NORMAL_BIT|LOW_BIT:
	    bg_count++;
	    if ((bg_count % BG_PROPORTION) == 0)
		sq = &queue[PRIORITY_LOW];
	    else
		sq = &queue[PRIORITY_NORMAL];
	    break;
	case LOW_BIT:
	    bg_count++;
	    sq = &queue[PRIORITY_LOW];
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
	if (sq->first == NULL) {
	    sq->last = NULL;
	    qmask &= ~(1 << p->prio);
	}

	ASSERT(p->status != P_SUSPENDED); /* Never run a suspended process */
#ifdef SHARED_HEAP
        p->active = 1;
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
#ifdef SHARED_HEAP
	ASSERT(p->active);
#endif
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
#ifndef SHARED_HEAP
    Uint arg_size;		/* Size of arguments. */
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
#endif
    ScheduleQ* sq;

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

#ifndef SHARED_HEAP
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
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

#ifndef SHARED_HEAP
    /*
     * Must initialize binary lists here before copying binaries to process.
     */
    p->off_heap.mso = NULL;
    p->off_heap.funs = NULL;
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;

    heap_need +=
	IS_CONST(parent->group_leader) ? 0 : NC_HEAP_SIZE(parent->group_leader);

    if (heap_need < p->min_heap_size) {
	sz = heap_need = p->min_heap_size;
    } else {
	sz = erts_next_heap_size(heap_need, 0);
    }
    p->arith_lowest_htop = (Eterm *) 0;
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
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
    p->arith_avail = 0;		/* No arithmetic heap. */
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
#endif
    p->saved_htop = NULL;
    p->fvalue = NIL;
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
#ifdef SHARED_HEAP
    p->arg_reg[2] = args;
#else
    BM_SWAP_TIMER(system,copy);
    p->arg_reg[2] = copy_struct(args, arg_size, &p->htop, &p->off_heap);
    BM_MESSAGE_COPIED(arg_size);
    BM_SWAP_TIMER(copy,system);
#endif
    p->arity = 3;

    p->freason = 0;
    p->reds = 0;
    sys_memset(&p->tm, 0, sizeof(ErlTimer));

    p->reg = NULL;
    p->dist_entry = NULL;
    p->error_handler = am_error_handler;    /* default */
    p->links = NULL;
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
    p->halloc_mbuf = NULL;
    p->mbuf_sz = 0;
#endif
    p->dictionary = NULL;
    p->debug_dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
#ifdef HEAP_FRAG_ELIM_TEST
    p->ssb = NULL;
#endif
    p->parent = parent->id;
    p->started = erts_get_time();

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
	if (IS_TRACED(parent) && (parent->flags & F_TRACE_PROCS) != 0) {
	    trace_proc(parent, parent, am_link, p->id);
	}
	parent->links = new_link(parent->links, LNK_LINK, p->id, NIL);
	p->links = new_link(p->links, LNK_LINK, parent->id, NIL);
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

#ifdef SHARED_HEAP
    /*
     * Add process to the array of active processes.
     */
    p->active = 1;
    erts_active_procs[erts_num_active_procs++] = p;
#endif

    /*
     * Schedule process for execution.
     */
    sq = &queue[p->prio];
    qmask |= (1 << p->prio);
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
    p->arith_lowest_htop = (Eterm *) 0;
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
    p->saved_htop = NULL;
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
    p->freason = 0;
    p->fcalls = 0;
    p->dist_entry = NULL;
    memset(&(p->tm), 0, sizeof(ErlTimer));
    p->next = NULL;
#ifndef SHARED_HEAP
    p->off_heap.mso = NULL;
    p->off_heap.funs = NULL;
    p->off_heap.externals = NULL;
    p->off_heap.overhead = 0;
#endif
    p->reg = NULL;
    p->heap_sz = 0;
#ifndef SHARED_HEAP
    p->high_water = NULL;
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->halloc_mbuf = NULL;
    p->mbuf_sz = 0;
#endif
    p->links = NULL;         /* List of links */
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
#ifdef HEAP_FRAG_ELIM_TEST
    p->ssb = NULL;
#endif

    p->parent = NIL;
    p->started = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
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

static void
delete_process0(Process* p, int do_delete)
{
    ErlLink* lnk;
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

#ifdef HEAP_FRAG_ELIM_TEST
    if (p->ssb) {
	erts_free(ERTS_ALC_T_SSB, p->ssb);
    }
#ifdef DEBUG
    p->ssb = (void *) 0x7DEFFACD;
#endif
#endif

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

    /* free all links */
    lnk = p->links;
    while(lnk != NULL) {
	ErlLink* next_link = lnk->next;
	del_link(&lnk);
	lnk = next_link;
    }

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
    
#ifdef SHARED_HEAP
    for (i = 0; i < erts_num_active_procs; i++) {
	if (erts_active_procs[i] == p) {
	    erts_active_procs[i] = erts_active_procs[--erts_num_active_procs];
	    break;
	}
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
    
#ifdef SHARED_HEAP
    p->active = 1;
#endif

    p->fvalue = copy;
    cancel_timer(p);
    p->freason = USER_EXIT;
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

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
do_exit(Process* p, Eterm reason)
{
    Process *rp;
    ErlLink* lnk;
    Eterm item;
    DistEntry *dep;
    int ix;
    Eterm ref;
    Eterm exit_tuple = NIL;
    Uint exit_tuple_sz = 0;

    p->arity = 0;		/* No live registers */
    p->fvalue = reason;
    p->status = P_EXITING;

    if (IS_TRACED_FL(p,F_TRACE_PROCS))
	trace_proc(p, p, am_exit, reason);

    if (p->flags & F_TRACER) {
	if (EQ(erts_default_tracer, p->id)) {
	    erts_default_tracer = NIL;
	    erts_default_process_flags &= ~TRACE_FLAGS;
	}
	if (EQ(erts_system_monitor, p->id)) {
	    erts_system_monitor_clear();
	}
    }

    lnk = p->links;
    p->links = NULL;
    
    
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

    while (lnk != NULL) {
	item = lnk->item;
	switch(lnk->type) {
	case LNK_LINK:
	    if(is_internal_port(item)) {
		ix = internal_port_index(item);
		if (! INVALID_PORT(erts_port+ix, item)) {
		    del_link(find_link(&erts_port[ix].links,LNK_LINK,
				       p->id,NIL));
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
		    ErlLink **rlinkpp = 
			find_link(&rp->links, LNK_LINK, p->id, NIL);
		    del_link(rlinkpp);
		    if (rp->flags & F_TRAPEXIT) {
			if (SEQ_TRACE_TOKEN(p) != NIL ) {
			    seq_trace_update_send(p);
			}
			send_exit_message(rp, exit_tuple, exit_tuple_sz,
					  SEQ_TRACE_TOKEN(p));
			if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
			    trace_proc(p, rp, am_getting_unlinked, p->id);
			}
		    } else if (reason == am_normal) {
			if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
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
	case LNK_LINK1:
	    ref = lnk->ref;
	    if (item == p->id) {
		/* We are monitoring 'data' */
		if (is_atom(lnk->data)) {
		    /* Monitoring a name on this node */
		    ASSERT(is_node_name_atom(lnk->data));
		    dep = erts_sysname_to_connected_dist_entry(lnk->data);
		    if(!dep)
			break;
		}
		else {
		    ASSERT(is_pid(lnk->data));
		    dep = pid_dist_entry(lnk->data);
		}
		if (dep != erts_this_dist_entry) {
		    ErlLink** lnkp;
		    lnkp = find_link_by_ref(&dep->links, ref);
		    if (lnkp != NULL) {
			/* Force send, use the atom in dist slot 
			 * link list as data for the message.
			 */
			dist_demonitor(dep, item, (*lnkp)->data, ref, 1);
			/* dist_demonitor() may have removed the link;
			   therefore, look it up again. */
			lnkp = find_link_by_ref(&dep->links, ref);
			del_link(lnkp);
		    }
		} else {
		    if ((rp = pid2proc(lnk->data)) != NULL)
			del_link(find_link_by_ref(&rp->links, ref));
		}
	    } else {
		/* 'Item' is monitoring us */
		if (is_internal_pid(item)) {
		    if ((rp = pid2proc(item)) != NULL) {
			Eterm lhp[3];
			Eterm item = (is_atom(lnk->data)
				      ? TUPLE2(&lhp[0],
					       lnk->data,
					       erts_this_dist_entry->sysname)
				      : lnk->data);
			ASSERT(lnk->data == p->id || is_atom(lnk->data));
			queue_monitor_message(rp, ref, am_process,
					      item, reason);
			del_link(find_link_by_ref(&rp->links, ref));
		    }
		} else if (is_external_pid(item)) {
		    dep = external_pid_dist_entry(item);
		    if(dep != erts_this_dist_entry)
			dist_m_exit(dep, item, lnk->data, ref, reason);
		}
		else {
		    ASSERT(0);
		}
	    }

	    break;
	case LNK_NODE:
	    ASSERT(is_node_name_atom(item));
	    dep = erts_sysname_to_connected_dist_entry(item);
	    if(dep)
		del_link(find_link(&dep->links,LNK_NODE,p->id,NIL));
	    else {
		/* XXX Is this possible? Shouldn't this link
		   previously have been removed if the node
		   had previously been disconnected. */
		ASSERT(0);
	    }
	    break;

	case LNK_OMON:
	case LNK_TMON:
	default:
	    erl_exit(1, "bad type in link list\n");
	    break;
	}
	del_link(&lnk);		/* will set lnk to next as well !! */
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
