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

#define MAX_BIT          (1 << PRIORITY_MAX)
#define HIGH_BIT         (1 << PRIORITY_HIGH)
#define NORMAL_BIT       (1 << PRIORITY_NORMAL)
#define LOW_BIT          (1 << PRIORITY_LOW)

extern Eterm* beam_apply[];
extern Eterm* beam_exit[];

#define KEEP_ZOMBIE

static int p_next;
static int p_serial;

typedef struct schedule_q {
    Process* first;
    Process* last;
} ScheduleQ;

static ScheduleQ queue[NPRIORITY_LEVELS];
static int processes_busy;
static int bg_count;
static unsigned qmask = 0;

Process**  process_tab;
Uint context_switches;		/* no of context switches */
Uint reductions;		/* total number of reductions */
Uint last_reds;			/* used in process info */
Uint erts_default_process_flags = 0;
Eterm erts_default_tracer = NIL;

#ifdef UNIFIED_HEAP
  Process** active_procs;
  #define HEAP_SIZE global_heap_sz
  #define MBUF_SIZE global_mbuf_sz
  #define UH_FLAGS  global_gc_flags
#else
  #define HEAP_SIZE p->heap_sz
  #define MBUF_SIZE p->mbuf_sz
  #define UH_FLAGS  p->flags
#endif

/* initialize the scheduler */
void init_scheduler()
{
    int i;

    process_tab = (Process**) erts_definite_alloc(max_process*sizeof(Process*));
    if(!process_tab) {
	process_tab = (Process**) 
	    safe_alloc_from(91, max_process * sizeof(Process*));
    }
    sys_memzero(process_tab, max_process * sizeof(Process*));
#ifdef UNIFIED_HEAP
    active_procs = (Process**)
        safe_alloc_from(92, max_process * sizeof(Process*));
    active_procs[0] = NULL;
#endif

    p_next = 0;
    p_serial = 0;

    /* mark the schedule queue as empty */
    for(i = 0; i < NPRIORITY_LEVELS; i++)
	queue[i].first = queue[i].last = (Process*) 0;
    qmask = 0;
    processes_busy = 0;
    bg_count = 0;
    context_switches = 0;
    reductions = 0;
    last_reds = 0;
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
void add_to_schedule_q(p)
Process *p;
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

int remove_proc_from_sched_q(p)
Process *p;
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

#ifdef INSTRUMENT
Eterm current_process = THE_NON_VALUE;
#endif

int
schedule(void)
{
    Process *p;
    ScheduleQ *sq;
    int function_calls = 0;
    int calls;

    if (do_time) {
	bump_timer();
    }
    
    do {
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
	    return 0;
#ifdef DEBUG
	default:
	    ASSERT(0);
#else
	default:
	    return 0; /* Should not happen ... */
#endif
	}

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

#ifdef UNIFIED_HEAP
        p->active = 1;
#endif
	context_switches++;
	calls = CONTEXT_REDS;
	if (p->status != P_EXITING) {
	    if (IS_TRACED_FL(p, F_TRACE_SCHED)) {
		trace_sched(p, am_in);
	    }
	    if (((MBUF_SIZE + p->off_heap.overhead)*MBUF_GC_FACTOR) >= HEAP_SIZE) {
		calls -= erts_garbage_collect(p, 0, p->arg_reg, p->arity);
		if (calls < 0) {
		    calls = 1;
		}
	    }
	    p->status = P_RUNNING;
	}

#ifdef INSTRUMENT
	current_process = p->id;
#endif

	calls = process_main(p, calls);
	function_calls += calls;
	reductions += calls;

#ifdef INSTRUMENT
	current_process = THE_NON_VALUE;
#endif

	p->reds += calls;
	if (p->status == P_FREE) {
	    fix_free(process_desc, (Eterm *) p);
	} else if (IS_TRACED_FL(p, F_TRACE_SCHED)) {
	    trace_sched(p, am_out);
	}

	if (do_time) {
	    bump_timer();
	}
    } while (function_calls <= INPUT_REDUCTIONS);
    
    return qmask;
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

    if ((p = (Process*) fix_alloc_from(92,process_desc)) == NULL)
	return NULL;
    p->id = make_pid(p_serial, THIS_NODE, p_next);
    p->rstatus = P_FREE;
    p->rcount = 0;

    /* set p_next to the next available slot */
    p_prev = p_next;
    p_next = (p_next+1) % max_process;

    while(p_prev != p_next) {
	if (p_next == 0)
	    p_serial = (p_serial+1) % MAX_SERIAL;
	if (process_tab[p_next] == NULL) /* found a free slot */
	    return p;
	p_next = (p_next+1) % max_process;
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
    Uint arg_size;		/* Size of arguments. */
    Uint sz;			/* Needed words on heap. */
    Sint arity;			/* Number of arguments. */
    Uint heap_need;		/* Size needed on heap. */
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
    arg_size = size_object(args);
    heap_need = arg_size;
    p->group_leader = parent->group_leader;
    p->flags = erts_default_process_flags;

    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size = so->min_heap_size;
	p->prio = so->priority;
#ifndef UNIFIED_HEAP
	p->max_gen_gcs = so->max_gen_gcs;
#endif
    } else {
	p->min_heap_size = H_MIN_SIZE;
	p->prio = PRIORITY_NORMAL;
#ifndef UNIFIED_HEAP
	p->max_gen_gcs = erts_max_gen_gcs;
#endif
    }
    ASSERT(p->min_heap_size == next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

    /*
     * Must initialize binary lists here before copying binaries to process.
     */
    p->off_heap.mso = NULL;
    p->off_heap.funs = NULL;
    p->off_heap.overhead = 0;

    if (heap_need < p->min_heap_size) {
	sz = heap_need = p->min_heap_size;
    } else {
	sz = next_heap_size(heap_need, 0);
    }

#ifdef HIPE
    hipe_init_process(&p->hipe);
#endif

#ifdef UNIFIED_HEAP
    p->send = (Eterm *) safe_alloc_from(7, sizeof(Eterm) * S_DEFAULT_SIZE);
    p->stop = p->stack = p->send + S_DEFAULT_SIZE;
    p->stack_sz = S_DEFAULT_SIZE;
    p->htop = NULL;
    p->hend = NULL;
#else
    p->heap = (Eterm *) erts_safe_sl_alloc_from(8, sizeof(Eterm)*sz);
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
    p->gen_gcs = 0;
#endif
    p->arith_avail = 0;		/* No arithmetic heap. */
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif

#ifndef UNIFIED_HEAP
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
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
#ifdef UNIFIED_HEAP
    p->arg_reg[2] = args;
#else
    p->arg_reg[2] = copy_struct(args, arg_size, &p->htop, &p->off_heap);
#endif
    p->arity = 3;

    p->freason = 0;
    p->reds = 0;
    sys_memset(&p->tm, 0, sizeof(ErlTimer));

    p->reg = NULL;
    p->reg_atom = THE_NON_VALUE;
    p->dslot = -1;
    p->error_handler = am_error_handler;    /* default */
    p->tracer_proc = erts_default_tracer;
    p->links = NULL;
    p->ct = NULL;

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifndef UNIFIED_HEAP
    p->mbuf = NULL;
    p->mbuf_sz = 0;
#endif
    p->dictionary = NULL;
    p->debug_dictionary = NULL;
    p ->seq_trace_lastcnt = 0;
    p ->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;

    process_tab[pid_number(p->id)] = p;

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

#ifdef UNIFIED_HEAP
    ADD_TO_ROOTSET(p);
    p->active = 1;
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
#ifdef UNIFIED_HEAP
    p->stack = NULL;
    p->send  = NULL;
    p->stack_sz = 0;
#else
    p->heap = NULL;
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
#endif
    p->min_heap_size = 0;
    p->status = P_RUNABLE;
    p->rstatus = P_RUNABLE;
    p->rcount = 0;
    p->id = 0;
    p->prio = PRIORITY_NORMAL;
    p->reds = 0;
    p->error_handler = am_error_handler;
    p->tracer_proc = NIL;
    p->group_leader = NIL;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = 0;
    p->fcalls = 0;
    p->dslot = -1;
    memset(&(p->tm), 0, sizeof(ErlTimer));
    p->next = NULL;
    p->off_heap.mso = NULL;
    p->off_heap.funs = NULL;
    p->off_heap.overhead = 0;
    p->reg = NULL;
    p->reg_atom = THE_NON_VALUE;
#ifndef UNIFIED_HEAP
    p->heap_sz = 0;
    p->high_water = NULL;
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
#endif
    p->links = NULL;         /* List of links */
    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifndef UNIFIED_HEAP
    p->mbuf = NULL;
    p->mbuf_sz = 0;
#endif
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
    p->arith_heap = NULL;
    p->arith_avail = 0;
#ifdef DEBUG
    p->arith_file = NULL;
    p->arith_line = 0;
    p->arith_check_me = NULL;
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
#ifdef HIPE
    hipe_init_process(&p->hipe);
#endif
}    

void
erts_cleanup_empty_process(Process* p)
{
#ifdef UNIFIED_HEAP
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
#ifndef UNIFIED_HEAP
    ErlMessage* mp;
    ErlHeapFragment* bp;
#endif
    int i;

#ifndef UNIFIED_HEAP
    /* Clean binaries and funs */
    erts_cleanup_offheap(&p->off_heap);

    /*
     * The mso list should not be used anymore, but if it is, make sure that
     * we'll notice.
     */
    p->off_heap.mso = (void *) 0x8DEFFACD;
#endif
    
    if (p->arg_reg != p->def_arg_reg)
	sys_free(p->arg_reg);

    /*
     * Release heaps. Clobber contents in DEBUG build.
     */

#ifdef UNIFIED_HEAP
#ifdef DEBUG
    sys_memset(p->send, 0xfb, p->stack_sz*sizeof(Eterm));
#endif
#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif
    sys_free((void*) p->send);
#else
#ifdef DEBUG
    sys_memset(p->heap, 0xfb, p->heap_sz*sizeof(Eterm));
#endif
#ifdef HIPE
    hipe_delete_process(&p->hipe);
#endif
    erts_sl_free((void*) p->heap);
    if (p->old_heap != NULL) {
#ifdef DEBUG
	sys_memset(p->old_heap, 0xfb, (p->old_hend-p->old_heap)*sizeof(Eterm));
#endif
	erts_sl_free(p->old_heap);
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
#endif

    if (p->dictionary != NULL) {
	sys_free(p->dictionary);
	p->dictionary = NULL;
    }

    if (p->debug_dictionary != NULL) {
	sys_free(p->debug_dictionary);
	p->debug_dictionary = NULL;
    }

#ifndef UNIFIED_HEAP
    /* free all pending messages */
    mp = p->msg.first;
    while(mp != NULL) {
	ErlMessage* next_mp = mp->next;
	free_message(mp);
	mp = next_mp;
    }
#endif

    /* free all links */
    lnk = p->links;
    while(lnk != NULL) {
	ErlLink* next_link = lnk->next;
	fix_free(link_desc, (Eterm*)lnk);
	lnk = next_link;
    }

    if (p->ct != NULL)
       sys_free(p->ct);

    if (p->flags & F_USING_DB)
	db_proc_dead(p->id);

    i = pid_number(p->id);
    process_tab[i] = NULL;
    if (p_next == -1)
	p_next = i;

    /*
     * Don't free it here, just mark it.
     */
    if (do_delete) {
       fix_free(process_desc, (Eterm *) p);
    } else {
       p->status = P_FREE;
   }

#ifdef UNIFIED_HEAP
    REMOVE_FROM_ROOTSET(p);
#endif
    processes_busy--;
}

#ifdef KEEP_ZOMBIE
#define SAVE_KILLED	100
static Process *last_killed[SAVE_KILLED];
static int last_killed_no = 0;
static int keep_killed = 0;
#endif

/* p is supposed to be the currently executing process 
 */
void delete_process(p)
Process* p;
{
#ifdef KEEP_ZOMBIE
   Process *kp;
   int gap = SAVE_KILLED - keep_killed;
   int i;
#endif

   if (p->reg != NULL)
      unregister_name(p, p->reg->name);

   cancel_timer(p);		/* Always cancel timer just in case */

#ifdef KEEP_ZOMBIE
   if (p->freason != EXC_NORMAL
/*       && p->freason != USER_EXIT */
       && (keep_killed > 0 ||
	   (keep_killed < 0 && last_killed_no < -keep_killed)))
   {
      i = (last_killed_no + gap) % SAVE_KILLED;
      kp = last_killed[i];
      if (kp != NULL)
      {
	 delete_process0(kp, 1);
	 last_killed[i] = NULL;
      }
      last_killed[last_killed_no] = p;
      last_killed_no = (last_killed_no + 1) % SAVE_KILLED;

      if (p->flags & F_USING_DB)
	 db_proc_dead(p->id);
      UH_FLAGS |= F_NEED_FULLSWEEP;
      erts_garbage_collect(p, 0, p->arg_reg, p->arity);
   }
   else
      delete_process0(p, 0);
#else
   delete_process0(p, 0);
#endif
}

int keep_zombies(int z, int *oldz)
{
   Process *kp;
   int i;

   *oldz = keep_killed;

   if (z < -SAVE_KILLED || z > SAVE_KILLED)
      return 0;

   for (i = 0; i < SAVE_KILLED; i++)
   {
      kp = last_killed[i];
      if (kp != NULL)
      {
	 delete_process0(kp, 1);
	 last_killed[i] = NULL;
      }
   }
   last_killed_no = 0;

   keep_killed = z;
   return 1;
}


/* Copied from 'process_info' in break.c and modified */
void process_info_zombies(to)
CIO to;
{
    int i;
    int gap = 0;
    int k;
    Process *p;

    if (keep_killed < 0)
    {
       k = -keep_killed;
    }
    else
    {
       gap = SAVE_KILLED - keep_killed;
       k = keep_killed;
    }

    erl_printf(to,"\nZombie Process Information\n");
    erl_printf(to,"Processes kept: ");
    if (keep_killed > 0)
       erl_printf(to,"the %d most recent\n", k);
    else
       erl_printf(to,"%d\n", k);
    erl_printf(to,"--------------------------------------------------\n");
    for (i = 0; i < k; i++) {
       if (keep_killed < 0)
	  p = last_killed[i];
       else
	  p = last_killed[(last_killed_no + gap + i) % SAVE_KILLED];
       if ((p != NULL) && (p->i != ENULL)) {
	   print_process_info(p,to);
       }
    }
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

    copy_object(reason, p, 0, &copy, (Process*)0);
    p->fvalue = copy;
    cancel_timer(p);
    p->freason = USER_EXIT;
    KILL_CATCHES(p);
    p->i = (Eterm *) beam_exit;
    if (status != P_RUNABLE) {
	add_to_schedule_q(p);
    }
}

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
do_exit(Process* p, Eterm reason)
{
   Process *rp;
   ErlLink* lnk;
   Eterm item;
   int slot;
   int ix;
   Ref *ref;

   p->status = P_EXITING;

   if (IS_TRACED_FL(p,F_TRACE_PROCS))
      trace_proc(p, p, am_exit, reason);

   if (p->flags & F_DEFAULT_TRACER) {
       if (erts_default_tracer == p->id) {
	   erts_default_tracer = NIL;
	   erts_default_process_flags &= ~TRACE_FLAGS;
       }
   }

   lnk = p->links;
   p->links = NULL;

   while(lnk != NULL) {
      item = lnk->item;
      switch(lnk->type) {
       case LNK_LINK:
	 if (is_port(item)) {
	    if ((slot = port_node(item)) != THIS_NODE)
	       dist_exit(slot, p->id, item, reason);
	    else {
	       ix = port_index(item);
	       if (erts_port[ix].status != FREE) {
		   del_link(find_link(&erts_port[ix].links,LNK_LINK,
				      p->id,NIL));
		   do_exit_port(item, p->id, reason);
	       }
	    }
	 }
	 else if (is_pid(item)) {
	    if ((slot = pid_node(item)) != THIS_NODE) {
		if (SEQ_TRACE_TOKEN(p) != NIL) {
		    seq_trace_update_send(p);
		}
		dist_exit_tt(slot, p->id, item, reason, SEQ_TRACE_TOKEN(p));
	    }
	    else {
	       if ((rp = pid2proc(item)) != NULL) {
		   ErlLink **rlinkpp = 
		       find_link(&rp->links, LNK_LINK, p->id, NIL);
		   del_link(rlinkpp);
		   if (rp->flags & F_TRAPEXIT) {
		       if (SEQ_TRACE_TOKEN(p) != NIL ) {
			   seq_trace_update_send(p);
		       }
		       deliver_exit_message_tt(p->id, rp, reason, 
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
	}
	 break;
     case LNK_LINK1:
	 ref = &lnk->ref;
	 if (item == p->id) {
	    /* We are monitoring 'data' */
	    if (is_small(lnk->data))
	       slot = unsigned_val(lnk->data); /* Monitor name */
	    else
	       slot = pid_node(lnk->data);
	    if (slot != THIS_NODE) {
	       ErlLink** lnkp;
	       lnkp = find_link_by_ref(&dist_addrs[slot].links, ref);
	       if (lnkp != NULL) {
		  /* Force send, use the atom in dist slot 
		   * link list as data for the message.
		   */
		  dist_demonitor(slot, item, (*lnkp)->data, ref, 1);
		  /* dist_demonitor() may have removed the link;
		     therefore, look it up again. */
		  lnkp = find_link_by_ref(&dist_addrs[slot].links, ref);
		  del_link(lnkp);
	       }
	    } else {
	       if ((rp = pid2proc(lnk->data)) != NULL)
		  del_link(find_link_by_ref(&rp->links, ref));
	    }
	 } else {
	    /* 'Item' is monitoring us */
	    if (pid_node(lnk->item) != THIS_NODE) {
	       dist_m_exit(pid_node(item), item, lnk->data, ref, reason);
	    } else {
	       if ((rp = pid2proc(lnk->item)) != NULL) {
		  queue_monitor_message(rp, ref, am_process, p->id, reason);
		  del_link(find_link_by_ref(&rp->links, ref));
	       }
	    }
	 }
	 break;
       case LNK_NODE:
	 del_link(find_link(&dist_addrs[lnk->data].links,LNK_NODE,
			    p->id,NIL));
	 break;

#ifdef MONITOR_ENHANCE
       case LNK_NODE1:
	 del_link(find_link(&dist_addrs[lnk->data].links,LNK_NODE1,
			    p->id,NIL));
	 break;
#endif

       case LNK_OMON:
       case LNK_TMON:
       default:
	 erl_exit(1, "bad type in link list\n");
	 break;
      }
      del_link(&lnk);		/* will set lnk to next as well !! */
   }

   if ((p->flags & F_DISTRIBUTION) && (p->dslot != -1))
      do_net_exits(p->dslot);

   /* Save the exit reason, for post-mortem debugging */
   p->fvalue = reason;
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
