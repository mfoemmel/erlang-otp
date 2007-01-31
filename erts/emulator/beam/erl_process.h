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

#ifndef __PROCESS_H__
#define __PROCESS_H__

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS
#if (defined(ERL_PROCESS_C__) \
     || defined(ERL_PORT_TASK_C__) \
     || (ERTS_GLB_INLINE_INCL_FUNC_DEF \
	 && defined(ERTS_DO_INCL_GLB_INLINE_FUNC_DEF)))
#define ERTS_INCLUDE_SCHEDULER_INTERNALS
#endif

#include "sys.h"
#include "erl_vm.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#include "erl_time.h"

#ifdef ERTS_SMP
#include "erl_vm.h"
#include "erl_term.h"
#include "erl_threads.h"
#endif

#ifdef HIPE
#include "hipe_process.h"
#endif

struct port;

#define ERTS_MAX_NO_OF_SCHEDULERS 1024

#define ERTS_DEFAULT_MAX_PROCESSES (1 << 15)

Uint erts_get_tot_proc_mem(void);

#define ERTS_PROC_MORE_MEM(Size) \
  (erts_smp_atomic_add(&erts_tot_proc_mem, (long) (Size)))

#define ERTS_PROC_LESS_MEM(Size) \
  (ASSERT_EXPR(erts_smp_atomic_read(&erts_tot_proc_mem) >= (long) (Size)), \
   erts_smp_atomic_add(&erts_tot_proc_mem, -((long) (Size))))

#define ERTS_HEAP_ALLOC(Type, Size)					\
    (ERTS_PROC_MORE_MEM((Size)),					\
     erts_alloc((Type), (Size)))

#define ERTS_HEAP_REALLOC(Type, Ptr, OldSize, NewSize)			\
    (ERTS_PROC_LESS_MEM((OldSize)),					\
     ERTS_PROC_MORE_MEM((NewSize)),					\
     erts_realloc((Type), (Ptr), (NewSize)))

#define ERTS_HEAP_FREE(Type, Ptr, Size)					\
    (ERTS_PROC_LESS_MEM((Size)),					\
     erts_free((Type), (Ptr)))

#define INITIAL_MOD 0
#define INITIAL_FUN 1
#define INITIAL_ARI 2

struct saved_calls {
   int len;
   int n;
   int cur;
   Export *ct[1];
};

extern Export exp_send, exp_receive, exp_timeout;
extern Uint erts_no_of_schedulers;

#ifdef ERTS_SMP

/* XXX: mutually recursive .h files break when abstract types are 'typedefs'.
   This #define kludge works around this particular cycle, but spelling out
   'struct foo*' in more places works better in the long run IMO. */
struct process;
#define Process struct process
#include "erl_bits.h"
#undef Process

#endif

typedef struct ErtsSchedulerData_ ErtsSchedulerData;
struct ErtsSchedulerData_ {

#ifdef ERTS_SMP
    ErtsSchedulerData *next;	/* Next scheduler data */
    ErtsSchedulerData *prev;	/* Prev scheduler data */
    ethr_tid tid;		/* Thread id */
    Uint no;			/* Scheduler number */
    Eterm save_reg[ERTS_X_REGS_ALLOCATED]; /* X registers */
    FloatDef freg[MAX_REG];	/* Floating point registers. */
    struct erl_bits_state erl_bits_state; /* erl_bits.c state */
    void *match_pseudo_process; /* erl_db_util.c:db_prog_match() */
    struct process *free_process;
#endif

    struct process *current_process;
};

#ifndef ERTS_SMP
extern ErtsSchedulerData erts_scheduler_data;
#endif

typedef struct _process_list {
    Eterm pid;			/* Waiting process. (internal pid) */
    struct _process_list* next;	/* Next waiting process. */
} ProcessList;

typedef struct {
    Eterm reason;
    ErlHeapFragment *bp;
} ErtsPendExit;

typedef struct process {
    /* All fields in the PCB that differs between different heap
     * architectures, have been moved to the end of this struct to
     * make sure that as few offsets as possible differ. Different
     * offsets between memory architectures in this struct, means that
     * native code have to use functions instead of constants.
     */

    Eterm* htop;		/* Heap top */
    Eterm* stop;		/* Stack top */
    Eterm* heap;		/* Heap start */
    Eterm* hend;		/* Heap end */
    Uint heap_sz;		/* Size of heap in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */

#if !defined(NO_FPE_SIGNALS)
    volatile int fp_exception;
#endif

#ifdef HIPE
    /* HiPE-specific process fields. Put it early in struct process,
       to enable smaller & faster addressing modes on the x86. */
    struct hipe_process_state hipe;
#endif

    /*
     * Saved x registers.
     */
    Uint arity;			/* Number of live argument registers (only valid
				 * when process is *not* running).
				 */
    Eterm* arg_reg;		/* Pointer to argument registers. */
    unsigned max_arg_reg;	/* Maximum number of argument registers available. */
    Eterm def_arg_reg[6];	/* Default array for argument registers. */

    Eterm* cp;			/* Continuation pointer (for threaded code). */
    Eterm* i;			/* Program counter for threaded code. */
    Sint catches;		/* Number of catches on stack */
    Sint fcalls;		/* 
				 * Number of reductions left to execute.
				 * Only valid for the current process.
				 */
    Uint32 status;		/* process STATE */
    Uint32 rstatus;		/* process resume STATE */
    int rcount;			/* suspend count */
    Eterm id;			/* The pid of this process */
    int  prio;			/* Priority of process */
    int  skipped;		/* Times a low prio process has been rescheduled */
    Uint reds;			/* No of reductions for this process  */
    Eterm error_handler;	/* Module atom for the error handler */
    Eterm tracer_proc;		/* If proc is traced, this is the tracer
				   (can NOT be boxed) */
    Uint trace_flags;		/* Trace flags (used to be in flags) */
    Eterm group_leader;		/* Pid in charge
				   (can be boxed) */
    Uint flags;			/* Trap exit, etc (no trace flags anymore) */
    Eterm fvalue;		/* Exit & Throw value (failure reason) */
    Uint freason;		/* Reason for detected failure */
    Eterm ftrace;		/* Latest exception stack trace dump */
    DistEntry *dist_entry;	/* Distribution slot to use if F_DISTRIBUTION */

    struct process *next;	/* Pointer to next process in list */

    struct reg_proc *reg;	/* NULL iff not registered */
    ErtsLink *nlinks;
    ErtsMonitor *monitors;      /* The process monitors, both ends */

    ErlMessageQueue msg;	/* Message queue */

    ErtsBifTimer *bif_timers;	/* Bif timers aiming at this process */

    ProcDict  *dictionary;       /* Process dictionary, may be NULL */
    ProcDict  *debug_dictionary; /* Process dictionary-like debugging 
				  * information, private to OTP applications */
    struct saved_calls *ct;

    Uint seq_trace_clock;
    Uint seq_trace_lastcnt;
    Eterm seq_trace_token;	/* Sequential trace token (tuple size 5 see below) */

    Eterm initial[3];		/* Initial module(0), function(1), arity(2) */
    Eterm* current;		/* Current Erlang function:
				 * module(0), function(1), arity(2)
				 * (module and functions are tagged atoms;
				 * arity an untagged integer).
				 */

    /*
     * Information mainly for post-mortem use (erl crash dump).
     */
    Eterm parent;		/* Pid of process that created this process. */
    long started;		/* Time when started. */


    /* This is the place, where all fields that differs between memory
     * architectures, have gone to.
     */

    Eterm *high_water;
    Eterm *old_hend;            /* Heap pointers for generational GC. */
    Eterm *old_htop;
    Eterm *old_heap;
    Uint16 gen_gcs;		/* Number of (minor) generational GCs. */
    Uint16 max_gen_gcs;		/* Max minor gen GCs before fullsweep. */
    ErlOffHeap off_heap;	/* Off-heap data updated by copy_struct(). */
    ErlHeapFragment* mbuf;	/* Pointer to message buffer list */
    Uint mbuf_sz;		/* Size of all message buffers */

#if !defined(HEAP_FRAG_ELIM_TEST)
    /*
     * Secondary heap for arithmetic operations.
     */

    Eterm* arith_heap;		/* Current heap pointer. */
    Uint arith_avail;		/* Available space on arithmetic heap. */

#if (defined(DEBUG) || defined(PURIFY))
    char* arith_file;
    int arith_line;
    Eterm* arith_check_me;	/* Address to check for overwrite. */
#endif
#endif

#ifdef ERTS_SMP
    ErtsSmpPTimer *ptimer;
#else
    ErlTimer tm;		/* Timer entry */
#endif

#ifdef ERTS_SMP
    ErtsSchedulerData *scheduler_data;
    int is_exiting;
    Uint32 scheduler_flags;
    Uint32 status_flags;
    Uint32 lock_flags;
    ErlMessageInQueue msg_inq;
    Eterm suspendee;
    ProcessList *pending_suspenders;
    ErtsPendExit pending_exit;
#ifdef HIPE
    struct hipe_process_state_smp hipe_smp;
#endif
#endif

#ifdef HYBRID
    Eterm *rrma;                /* Remembered roots to Message Area */
    Eterm **rrsrc;              /* The source of the root */
    Uint nrr;                   /* Number of remembered roots */
    Uint rrsz;                  /* Size of root array */
#endif

#ifdef HYBRID
    Uint active;                /* Active since last major collection? */
    Uint active_index;          /* Index in the active process array */
#endif
 
#ifdef INCREMENTAL
    struct process *active_next; /* Active processes to scan for roots */
    struct process *active_prev; /* in collection of the message area  */
    Eterm *scan_top;
#endif

#ifdef CHECK_FOR_HOLES
    Eterm* last_htop;		/* No need to scan the heap below this point. */
    ErlHeapFragment* last_mbuf;	/* No need to scan beyond this mbuf. */
#endif
} Process;

#ifdef CHECK_FOR_HOLES
# define INIT_HOLE_CHECK(p)			\
do {						\
  (p)->last_htop = 0;				\
  (p)->last_mbuf = 0;				\
} while (0)

# define ERTS_HOLE_CHECK(p) erts_check_for_holes((p))
void erts_check_for_holes(Process* p);
#else
# define INIT_HOLE_CHECK(p)
# define ERTS_HOLE_CHECK(p)
#endif

/*
 * The MBUF_GC_FACTOR decides how easily a process is subject to GC 
 * due to message buffers allocated outside the heap.
 * The larger the factor, the easier the process gets GCed.
 * On a small memory system with lots of processes, this makes a significant 
 * difference, especially since the GCs help fragmentation quite a bit too.
 */
#if defined(SMALL_MEMORY)
#define MBUF_GC_FACTOR 4
#else
#define MBUF_GC_FACTOR 1
#endif

/*
 * The weight of binaries outside the heap (for p->overhead calculation).
 */

#define BINARY_OVERHEAD_FACTOR 16

/*
 * Force a garbage collection for the given process.
 */

#define FORCE_GC(p) ((p)->off_heap.overhead = p->heap_sz)

#define SEQ_TRACE_TOKEN(p)  ((p)->seq_trace_token)

/* The sequential tracing token is a tuple of size 5:
 *
 *    {Flags, Label, Serial, Sender}
 */

#define SEQ_TRACE_TOKEN_ARITY(p)    (arityval(*(tuple_val(SEQ_TRACE_TOKEN(p)))))
#define SEQ_TRACE_TOKEN_FLAGS(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 1))
#define SEQ_TRACE_TOKEN_LABEL(p)    (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 2))
#define SEQ_TRACE_TOKEN_SERIAL(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 3))
#define SEQ_TRACE_TOKEN_SENDER(p)   (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 4))
#define SEQ_TRACE_TOKEN_LASTCNT(p)  (*(tuple_val(SEQ_TRACE_TOKEN(p)) + 5))

/* used when we have unit32 token */
#define SEQ_TRACE_T_ARITY(token)    (arityval(*(tuple_val(token))))
#define SEQ_TRACE_T_FLAGS(token)    (*(tuple_val(token) + 1))
#define SEQ_TRACE_T_LABEL(token)    (*(tuple_val(token) + 2))
#define SEQ_TRACE_T_SERIAL(token)   (*(tuple_val(token) + 3))
#define SEQ_TRACE_T_SENDER(token)   (*(tuple_val(token) + 4))
#define SEQ_TRACE_T_LASTCNT(token)  (*(tuple_val(token) + 5))

/*
 * Possible flags for the flags field in ErlSpawnOpts below.
 */

#define SPO_LINK 1
#define SPO_USE_ARGS 2
#define SPO_MONITOR 4

/*
 * The following struct contains options for a process to be spawned.
 */
typedef struct {
    Uint flags;
    int error_code;		/* Error code returned from create_process(). */
    Eterm mref;			/* Monitor ref returned (if SPO_MONITOR was given). */

    /*
     * The following items are only initialized if the SPO_USE_ARGS flag is set.
     */
    Uint min_heap_size;		/* Minimum heap size (must be a valued returned
				 * from next_heap_size()).
				 */
    int priority;		/* Priority for process. */
    Uint16 max_gen_gcs;		/* Maximum number of gen GCs before fullsweep. */
} ErlSpawnOpts;

/*
 * The KILL_CATCHES(p) macro kills pending catches for process p.
 */

#define KILL_CATCHES(p) (p)->catches = -1

void erts_arith_shrink(Process* p, Eterm* hp);
Eterm* erts_heap_alloc(Process* p, Uint need);
#ifdef CHECK_FOR_HOLES
Eterm* erts_set_hole_marker(Eterm* ptr, Uint sz);
#endif

extern Process** process_tab;
#ifdef HYBRID
extern Uint erts_num_active_procs;
extern Process** erts_active_procs;
#endif
extern Uint erts_max_processes;
extern Uint erts_process_tab_index_mask;
extern Uint erts_default_process_flags;
/* If any of the erts_system_monitor_* variables are set (enabled),
** erts_system_monitor must be != NIL, to allow testing on just
** the erts_system_monitor_* variables.
*/
extern Eterm erts_system_monitor;
extern Uint erts_system_monitor_long_gc;
extern Uint erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t {
    unsigned int busy_port : 1;
    unsigned int busy_dist_port : 1;
};
extern struct erts_system_monitor_flags_t erts_system_monitor_flags;
extern erts_smp_atomic_t erts_tot_proc_mem;

#define INVALID_PID(p, pid)	((p) == NULL				\
				 || (p)->id != (pid)			\
				 || (p)->status == P_EXITING)
 
#define IS_TRACED(p)        ( (p)->tracer_proc != NIL)
#define IS_TRACED_FL(p,tf)  ( IS_TRACED(p) \
			      && ( ((p)->trace_flags & (tf)) == (tf)) )

/* process priorities */
#define PRIORITY_MAX          0
#define PRIORITY_HIGH         1
#define PRIORITY_NORMAL       2
#define PRIORITY_LOW          3
#define NPRIORITY_LEVELS      4

/* times to reschedule low prio process before running */
#define RESCHEDULE_LOW        8

/* process flags */
#define F_TRAPEXIT           (1 << 0)
#define F_INSLPQUEUE         (1 << 1) /* Set if in timer queue */
#define F_TIMO               (1 << 2) /* Set if timeout */
#define F_USING_DB           (1 << 3) /* If have created tables */
#define F_DISTRIBUTION       (1 << 16)  /* Process used in distribution */
#define F_HEAP_GROW          (1 << 17)
#define F_NEED_FULLSWEEP     (1 << 18) /* If process has old binaries & funs. */
#define F_USING_DDLL         (1 << 24) /* Process has used the DDLL interface */
#define F_HAVE_BLCKD_MSCHED  (1 << 25) /* Process have blocked multi-scheduling */

/* process trace_flags */
#define F_TRACE_SEND         (1 << 4)   
#define F_TRACE_RECEIVE      (1 << 5)
#define F_TRACE_SOS          (1 << 6) /* Set on spawn       */
#define F_TRACE_SOS1         (1 << 7) /* Set on first spawn */
#define F_TRACE_SOL          (1 << 8) /* Set on link        */
#define F_TRACE_SOL1         (1 << 9) /* Set on first link  */
#define F_TRACE_CALLS        (1 << 10)
#define F_TIMESTAMP          (1 << 11)
#define F_TRACE_PROCS        (1 << 12)
#define F_TRACE_FIRST_CHILD  (1 << 13)
#define F_TRACE_SCHED        (1 << 14)
#define F_TRACE_GC           (1 << 15)
#define F_TRACE_ARITY_ONLY   (1 << 19)
#define F_TRACE_RETURN_TO    (1 << 20) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       (1 << 21) /* No call trace msg suppress */
#define F_TRACER             (1 << 22) /* May be (has been) tracer */
#define F_EXCEPTION_TRACE    (1 << 23) /* May have exception trace on stack */

#define TRACEE_FLAGS (  F_TRACE_PROCS | F_TRACE_CALLS \
		     | F_TRACE_SOS |  F_TRACE_SOS1| F_TRACE_RECEIVE  \
		     | F_TRACE_SOL | F_TRACE_SOL1 | F_TRACE_SEND | \
		     F_TRACE_SCHED | F_TIMESTAMP | F_TRACE_GC  | \
		     F_TRACE_ARITY_ONLY | F_TRACE_RETURN_TO | \
                     F_TRACE_SILENT)

/* Sequential trace flags */
#define SEQ_TRACE_SEND     (1 << 0)
#define SEQ_TRACE_RECEIVE  (1 << 1)
#define SEQ_TRACE_PRINT    (1 << 2)
#define SEQ_TRACE_TIMESTAMP (1 << 3)

#ifdef ERTS_SMP
/* Status flags ... */
#define ERTS_PROC_SFLG_PENDADD2SCHEDQ	(((Uint32) 1) << 0)	/* Pending
								   add to
								   schedule q */
#define ERTS_PROC_SFLG_INRUNQ		(((Uint32) 1) << 1)	/* Process is
								   in run q */
#define ERTS_PROC_SFLG_TRAPEXIT		(((Uint32) 1) << 2)	/* Process is
								   trapping
								   exit */
#define ERTS_PROC_SFLG_SCHEDULED	(((Uint32) 1) << 3)	/* Process is
								   scheduled */
/* Scheduler flags in process struct... */
#define ERTS_PROC_SCHED_FLG_SCHEDULED	(((Uint32) 1) << 0)	/* Process is
								   scheduled */

#endif


#ifdef ERTS_SMP
#define ERTS_PROC_IS_TRAPPING_EXITS(P)					\
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P))			\
		      & ERTS_PROC_LOCK_STATUS),				\
   (P)->status_flags & ERTS_PROC_SFLG_TRAPEXIT)

#define ERTS_PROC_SET_TRAP_EXIT(P)					\
  (ERTS_SMP_LC_ASSERT(((ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)	\
		       & erts_proc_lc_my_proc_locks((P)))		\
		      == (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)),	\
   (P)->status_flags |= ERTS_PROC_SFLG_TRAPEXIT,			\
   (P)->flags |= F_TRAPEXIT,						\
   1)

#define ERTS_PROC_UNSET_TRAP_EXIT(P)					\
  (ERTS_SMP_LC_ASSERT(((ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)	\
		       & erts_proc_lc_my_proc_locks((P)))		\
		      == (ERTS_PROC_LOCK_MAIN|ERTS_PROC_LOCK_STATUS)),	\
   (P)->status_flags &= ~ERTS_PROC_SFLG_TRAPEXIT,			\
   (P)->flags &= ~F_TRAPEXIT,						\
   0)
#else
#define ERTS_PROC_IS_TRAPPING_EXITS(P) ((P)->flags & F_TRAPEXIT)
#define ERTS_PROC_SET_TRAP_EXIT(P) ((P)->flags |= F_TRAPEXIT, 1)
#define ERTS_PROC_UNSET_TRAP_EXIT(P) ((P)->flags &= ~F_TRAPEXIT, 0)
#endif

/* Option flags to erts_send_exit_signal() */
#define ERTS_XSIG_FLG_IGN_KILL		(((Uint32) 1) << 0)
#define ERTS_XSIG_FLG_NO_IGN_NORMAL	(((Uint32) 1) << 1)


/* Process status values */
#define P_FREE      0
#define P_RUNABLE   1
#define P_WAITING   2
#define P_RUNNING   3
#define P_EXITING   4
#define P_GARBING   5
#define P_SUSPENDED 6

#define CANCEL_TIMER(p) \
    do { \
	if ((p)->flags & (F_INSLPQUEUE)) \
	    cancel_timer(p); \
	else \
	    (p)->flags &= ~F_TIMO; \
    } while (0)

void erts_pre_init_process(void);
#ifdef ERTS_SMP
int erts_block_multi_scheduling(Process *, Uint32, int, int);
int erts_is_multi_scheduling_blocked(void);
Eterm erts_multi_scheduling_blockers(Process *);
void erts_start_schedulers(Uint);
#endif
void erts_init_process(void);
Eterm erts_process_status(Process *, Uint32, Process *, Eterm);
int  sched_q_len(void);
void add_to_schedule_q(Process*);
Process *schedule(Process*, int);
void erts_schedule_misc_op(void (*)(void *), void *);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_do_exit_process(Process*, Eterm);
void set_timer(Process*, Uint);
void cancel_timer(Process*);
Uint erts_process_count(void);

void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);
#ifdef DEBUG
void erts_debug_verify_clean_empty_process(Process* p);
#endif
void erts_stack_dump(int to, void *to_arg, Process *);
void erts_program_counter_info(int to, void *to_arg, Process *);

Eterm erts_get_process_priority(Process *p);
Eterm erts_set_process_priority(Process *p, Eterm prio);

Uint erts_get_total_context_switches(void);
void erts_get_total_reductions(Uint *, Uint *);
void erts_get_exact_total_reductions(Process *, Uint *, Uint *);

void erts_suspend(Process*, Uint32, struct port*);
void erts_resume(Process*, Uint32);
int erts_send_exit_signal(Process *, Eterm, Process *, Uint32 *, Eterm, Eterm,
			  Process *, Uint32);
#ifdef ERTS_SMP
void erts_handle_pending_exit(Process *, Uint32);
#define ERTS_PROC_PENDING_EXIT(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) & ERTS_PROC_LOCK_STATUS),\
   (P)->pending_exit.reason != THE_NON_VALUE)
#else
#define ERTS_PROC_PENDING_EXIT(P) 0
#endif

#ifdef ERTS_SMP
Process *erts_suspend_another_process(Process *c_p, Uint32 c_p_locks,
				      Eterm suspendee, Uint32 suspendee_locks);
#endif
void erts_deep_process_dump(int, void *);

Sint erts_test_next_pid(int, Uint);

#ifdef ERTS_SMP
#  define ERTS_GET_SCHEDULER_DATA_FROM_PROC(PROC) ((PROC)->scheduler_data)
#else
#  define ERTS_GET_SCHEDULER_DATA_FROM_PROC(PROC) (&erts_scheduler_data)
#endif

#if defined(ERTS_SMP) || defined(USE_THREADS)
ErtsSchedulerData *erts_get_scheduler_data(void);
#else
ERTS_GLB_INLINE ErtsSchedulerData *erts_get_scheduler_data(void);
#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
ErtsSchedulerData *erts_get_scheduler_data(void)
{
    return &erts_scheduler_data;
}
#endif
#endif

ERTS_GLB_INLINE Process *erts_get_current_process(void);
ERTS_GLB_INLINE Eterm erts_get_current_pid(void);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE
Process *erts_get_current_process(void)
{
    ErtsSchedulerData *esdp = erts_get_scheduler_data();
    return esdp ? esdp->current_process : NULL;
}

ERTS_GLB_INLINE
Eterm erts_get_current_pid(void)
{
    Process *proc = erts_get_current_process();
    return proc ? proc->id : THE_NON_VALUE;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

/*
 * Process locks.
 */

#define ERTS_PROC_LOCK_MAX_BIT 3

/* Process lock flags */

/*
 * Main lock:
 *   The main lock is held by the scheduler running a process. It
 *   is used to protect all fields in the process structure except
 *   for those fields protected by other process locks (follows).
 */
#define ERTS_PROC_LOCK_MAIN		(((Uint32) 1) << 0)

/*
 * Link lock:
 *   Protects the following fields in the process structure:
 *   * nlinks
 *   * monitors
 */
#define ERTS_PROC_LOCK_LINK		(((Uint32) 1) << 1)

/*
 * Message queue lock:
 *   Protects the following fields in the process structure:
 *   * msg_inq
 *   * bif_timers
 */
#define ERTS_PROC_LOCK_MSGQ		(((Uint32) 1) << 2)

/*
 * Status lock:
 *   Protects the following fields in the process structure:
 *   * status
 *   * rstatus
 *   * status_flags
 */
#define ERTS_PROC_LOCK_STATUS		(((Uint32) 1) << ERTS_PROC_LOCK_MAX_BIT)

/*
 * Special fields:
 *
 *   The following fields are read only and can be read if at
 *   least one process lock (whichever one doesn't matter)
 *   is held:
 *     * id
 *
 *   The following fields are only allowed to be written if
 *   all process locks are held, and are allowed to be read if
 *   at least one process lock (whichever one doesn't matter)
 *   is held:
 *     * tracer_proc
 *     * tracer_flags
 *
 *   The following fields are only allowed to be accessed if
 *   both the schedule queue lock and at least one process lock
 *   (whichever one doesn't matter) are held:
 *     * prio
 *     * next
 *     * scheduler_flags
 */

/*
 * Other rules regarding process locking:
 *
 * Exiting processes:
 *   When changing status to P_EXITING on a process, you are required
 *   to take all process locks (ERTS_PROC_LOCKS_ALL). Thus, by holding
 *   at least one process lock (whichever one doesn't matter) you
 *   are guaranteed that the process won't exit until the lock you are
 *   holding has been released.
 *     At the same time as status is changed to P_EXITING, also the
 *   field 'is_exiting' in the process structure is set to a value != 0
 *   and the lock flag ERTS_PROC_LOCK_FLAG_EXITING is set.
 *
 * Lock order:
 *   Process locks with low numeric values has to be locked before
 *   process locks with high numeric values. E.g., main locks has
 *   to be locked before message queue locks.
 *
 *   When process locks with the same numeric value are to be locked
 *   on multiple processes, locks on processes with low process ids
 *   have to be locked before locks on processes with high process
 *   ids. E.g., if the main and the message queue locks are to be
 *   locked on processes p1 and p2 and p1->id < p2->id, then locks
 *   should be locked in the following order:
 *     1. main lock on p1
 *     2. main lock on p2
 *     3. message queue lock on p1
 *     4. message queue lock on p2
 */

/* Other lock flags */
#define ERTS_PROC_LOCK_FLAG_EXITING	(((Uint32) 1) << 31)
#define ERTS_PROC_LOCK_FLAG_WAITERS	(((Uint32) 1) << 30)

/* ERTS_PROC_LOCKS_* are combinations of process locks */

#define ERTS_PROC_LOCKS_MSG_RECEIVE	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_MSG_SEND	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_XSIG_SEND	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)

#define ERTS_PROC_LOCKS_ALL \
  ((((Uint32) 1) << (ERTS_PROC_LOCK_MAX_BIT + 1)) - 1)

#define ERTS_PROC_LOCKS_ALL_MINOR	(ERTS_PROC_LOCKS_ALL \
					 & ~ERTS_PROC_LOCK_MAIN)

#define ERTS_CHK_HAVE_NO_PROC_LOCKS
#define ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(PID)

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
#define ERTS_SMP_CHK_NO_PROC_LOCKS \
  erts_proc_lc_chk_no_proc_locks(__FILE__, __LINE__)
#else
#define ERTS_SMP_CHK_NO_PROC_LOCKS
#endif

#ifdef ERTS_SMP

#ifdef ERTS_ENABLE_LOCK_CHECK
void erts_proc_lc_lock(Process *p, Uint32 locks);
void erts_proc_lc_trylock(Process *p, Uint32 locks, int locked);
void erts_proc_lc_unlock(Process *p, Uint32 locks);
void erts_proc_lc_chk_have_proc_locks(Process *p, Uint32 locks);
void erts_proc_lc_chk_proc_locks(Process *p, Uint32 locks);
void erts_proc_lc_chk_only_proc_main(Process *p);
void erts_proc_lc_chk_no_proc_locks(char *file, int line);
Uint32 erts_proc_lc_my_proc_locks(Process *p);
int erts_proc_lc_trylock_force_busy(Process *p, Uint32 locks);
#endif

#define ERTS_PROC_LOCKS_BITS		7
#define ERTS_PROC_LOCKS_NO_OF		(1 << ERTS_PROC_LOCKS_BITS)

#define ERTS_PIX2LOCKIX(PIX) \
  ((PIX) & ((1 << ERTS_PROC_LOCKS_BITS) - 1))
#define ERTS_PID2LOCKIX(PID) \
  (ERTS_PIX2LOCKIX(internal_pid_data((PID))))

typedef struct {
    erts_smp_mtx_t	mtx;
    erts_smp_cnd_t	cnd;
} erts_proc_lock_t;

extern erts_proc_lock_t erts_proc_locks[ERTS_PROC_LOCKS_NO_OF];

Uint32 erts_proc_get_locks(Process *, erts_proc_lock_t *, Uint32, int);

ERTS_GLB_INLINE void erts_proc_lock(Process *p, Uint32 lock_flags);
ERTS_GLB_INLINE int erts_proc_trylock(Process *p, Uint32 lock_flags);
ERTS_GLB_INLINE void erts_proc_unlock(Process *p, Uint32 lock_flags);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_proc_lock(Process *p, Uint32 lock_flags)
{
    Uint32 locks = lock_flags & ERTS_PROC_LOCKS_ALL;
    erts_proc_lock_t *lckp = &erts_proc_locks[ERTS_PID2LOCKIX(p->id)];
    erts_smp_mtx_lock(&lckp->mtx);

    if (p->lock_flags & locks)
	(void) erts_proc_get_locks(p, lckp, lock_flags, 1);
#ifdef ERTS_ENABLE_LOCK_CHECK
    else
	erts_proc_lc_lock(p, locks);
#endif

    p->lock_flags |= lock_flags;

    erts_smp_mtx_unlock(&lckp->mtx);
}

ERTS_GLB_INLINE int
erts_proc_trylock(Process *p, Uint32 lock_flags)
{
    int res;
    Uint32 locks = lock_flags & ERTS_PROC_LOCKS_ALL;
    erts_proc_lock_t *lckp = &erts_proc_locks[ERTS_PID2LOCKIX(p->id)];
    erts_smp_mtx_lock(&lckp->mtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (erts_proc_lc_trylock_force_busy(p, locks)) {
	res = EBUSY; /* Make sure caller can handle the situation without
			causing a lock order violation to occur */
    }
    else
#endif

    if (p->lock_flags & locks) {
	res = EBUSY;
    }
    else {
	p->lock_flags |= lock_flags;
	res = 0;
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, locks, res == 0);
#endif

    erts_smp_mtx_unlock(&lckp->mtx);
    return res;
}

ERTS_GLB_INLINE void
erts_proc_unlock(Process *p, Uint32 lock_flags)
{
    erts_proc_lock_t *lckp = &erts_proc_locks[ERTS_PID2LOCKIX(p->id)];
    erts_smp_mtx_lock(&lckp->mtx);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_unlock(p, lock_flags & ERTS_PROC_LOCKS_ALL);
#endif

    p->lock_flags &= ~lock_flags;

    if (p->lock_flags & ERTS_PROC_LOCK_FLAG_WAITERS) {
	erts_smp_cnd_broadcast(&lckp->cnd); /* erts_smp_cnd_signal() won't do
					       since mtx, cnd pair is used for
					       multiple locks. */
	p->lock_flags &= ~ERTS_PROC_LOCK_FLAG_WAITERS;
    }
    erts_smp_mtx_unlock(&lckp->mtx);
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

int
erts_proc_safelock(Process * this_proc,
		   Uint32 this_have_locks,
		   Uint32 this_need_locks,
		   int allow_this_exiting,
		   Uint32 other_pid,
		   Process *other_proc,
		   Uint32 other_have_locks,
		   Uint32 other_need_locks,
		   int allow_other_exiting);

#endif /* #ifdef ERTS_SMP */

#define ERTS_P2P_FLG_ALLOW_OTHER_X	(((Uint32) 1) <<  0)

#ifdef ERTS_SMP
/*
 * erts_pid2proc_opt():
 *
 * Looks up the process structure of a pid and at the same time
 * acquires process locks. Locks on currently executing process and
 * looked up process are taken according to the lock order, i.e.,
 * locks on currently executing process may have be released and
 * reacquired. Since all locks on currently executing process may
 * have to be released, it may become exiting.
 *
 *   Arguments:
 *	this_proc:		Normally currently executing process,
 *				but it can be any proc.
 *	this_have_locks:	currently aqired locks on this_proc
 *	pid:			pid of process to lookup
 *	pid_need_locks:		locks to acquire on looked up process
 *	allow_this_exiting:	If != 0, ignore if this_proc becomes
 *				exiting.
 *
 *   Return value:		A pointer to process struct of
 *				pid.
 *				  If a value != NULL is returned,
 *				this_have_locks are acquired on this_proc
 *				and pid_need_locks on looked up process.
 *				Both processes are valid processes,
 *				i.e. not in the states exiting or free.
 *				  If NULL is returned, either this_proc
 *				became exiting, or no process could be
 *				looked up. this_have_locks are acquired,
 *				on this_proc.
 *				  If allow_other_exiting != 0, lookup
 *				will proceed even if this_proc becomes
 *				exiting, i.e., this_proc may have become
 *				exiting	even if a value != NULL is
 *				returned.
 */

ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *c_p, Uint32 c_p_have_locks,
		  Eterm pid, Uint32 pid_need_locks, int flags);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF
ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *c_p, Uint32 c_p_have_locks,
		  Eterm pid, Uint32 pid_need_locks, int flags)
{
    erts_proc_lock_t *lckp;
    Uint32 need_locks;
    Uint pix;
    Process *proc;

    if (is_not_internal_pid(pid))
	return NULL;
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes)
	return NULL;

    lckp = &erts_proc_locks[ERTS_PID2LOCKIX(pid)];
    erts_smp_mtx_lock(&lckp->mtx);

    need_locks = pid_need_locks & ERTS_PROC_LOCKS_ALL;
    if (c_p && c_p->id == pid) {
	need_locks &= ~c_p_have_locks;
	proc = c_p;
	ASSERT(c_p->id != ERTS_INVALID_PID);
	ASSERT(c_p == process_tab[pix]);
    }
    else
	proc = process_tab[pix];
    if (!proc
	|| proc->id != pid
	|| ((proc->lock_flags & ERTS_PROC_LOCK_FLAG_EXITING)
	    && !(flags & ERTS_P2P_FLG_ALLOW_OTHER_X))) {
	proc = NULL;
	goto done;
    }

    if (
#ifdef ERTS_ENABLE_LOCK_CHECK
	erts_proc_lc_trylock_force_busy(proc, need_locks) ||
	/* Make sure erts_proc_safelock() is enough to handle
	   a potential lock order violation situation... */
#endif
	(proc->lock_flags & need_locks)) {
	erts_smp_mtx_unlock(&lckp->mtx);
	if (!erts_proc_safelock(c_p,
				c_p_have_locks,
				c_p_have_locks,
				1,
				pid,
				proc,
				0,
				need_locks,
				flags & ERTS_P2P_FLG_ALLOW_OTHER_X))
	    proc = NULL;
    }
    else {
	/* Got them all at once... */
	proc->lock_flags |= need_locks;

#ifdef ERTS_ENABLE_LOCK_CHECK
	if (need_locks)
	    erts_proc_lc_trylock(proc, need_locks, 1);
#endif

    done:
	erts_smp_mtx_unlock(&lckp->mtx);
    }

    return proc;
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

Process *erts_pid2proc_not_running(Process *, Uint32, Eterm, Uint32);

#ifdef DEBUG
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P) \
  do { ASSERT(!(P)->is_exiting); } while (0)
#else
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)
#endif

/* NOTE: At least one process lock has to be held on P! */
#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_PROC_IS_EXITING(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) != 0), \
   (P)->is_exiting)
#else
#define ERTS_PROC_IS_EXITING(P) ((P)->is_exiting)
#endif

#else /* !ERTS_SMP */

#define ERTS_PROC_IS_EXITING(P) ((P)->status == P_EXITING)

#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)

#define erts_pid2proc_not_running erts_pid2proc

#endif

#define erts_pid2proc(PROC, HL, PID, NL) \
  erts_pid2proc_opt((PROC), (HL), (PID), (NL), 0)

ERTS_GLB_INLINE Process *
#ifdef ERTS_SMP
erts_pid2proc_unlocked_opt(Eterm pid, int flags);
#else
erts_pid2proc_opt(Process *c_p_unused,
		  Uint32 c_p_have_locks_unused,
		  Eterm pid,
		  Uint32 pid_need_locks_unused,
		  int flags);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Process *
#ifdef ERTS_SMP
erts_pid2proc_unlocked_opt(Eterm pid, int flags)
#else
erts_pid2proc_opt(Process *c_p_unused,
		  Uint32 c_p_have_locks_unused,
		  Eterm pid,
		  Uint32 pid_need_locks_unused,
		  int flags)
#endif
{
    Uint pix;
    Process *proc;

    if (is_not_internal_pid(pid))
	return NULL;
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes)
	return NULL;
    proc = process_tab[pix];
    if (proc) {
	if (proc->id != pid
	    || (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
		&& proc->status == P_EXITING))
	    proc = NULL;
    }
    return proc;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERTS_SMP
#define erts_pid2proc_unlocked(PID) erts_pid2proc_unlocked_opt((PID), 0)
#else
#define erts_pid2proc_unlocked(PID) erts_pid2proc_opt(NULL, 0, (PID), 0, 0)
#endif

/* Minimum NUMBER of processes for a small system to start */
#ifdef ERTS_SMP
#define ERTS_MIN_PROCESSES		ERTS_PROC_LOCKS_BITS
#else
#define ERTS_MIN_PROCESSES		16
#endif

/* Process locks */

ERTS_GLB_INLINE void erts_smp_proc_lock(Process *p, Uint32 locks);
ERTS_GLB_INLINE void erts_smp_proc_unlock(Process *p, Uint32 locks);
ERTS_GLB_INLINE int  erts_smp_proc_trylock(Process *p, Uint32 locks);
ERTS_GLB_INLINE void erts_smp_proc_tab_lock(void);
ERTS_GLB_INLINE void erts_smp_proc_tab_unlock(void);

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS
/* Scheduling lock */
extern erts_smp_mtx_t schdlq_mtx;
#ifdef ERTS_SMP
extern int erts_all_schedulers_waiting;
#endif
ERTS_GLB_INLINE int  erts_smp_sched_trylock(void);
ERTS_GLB_INLINE void erts_smp_sched_lock(void);
ERTS_GLB_INLINE void erts_smp_sched_unlock(void);
ERTS_GLB_INLINE void erts_smp_notify_inc_runq(void);
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int erts_smp_is_sched_locked(void);
#endif
void erts_wake_one_scheduler(void);
#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_smp_proc_lock(Process *p, Uint32 locks)
{
#ifdef ERTS_SMP
    erts_proc_lock(p, locks);
#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock(Process *p, Uint32 locks)
{
#ifdef ERTS_SMP
    erts_proc_unlock(p, locks);
#endif
}

ERTS_GLB_INLINE int
erts_smp_proc_trylock(Process *p, Uint32 locks)
{
#ifdef ERTS_SMP
    return erts_proc_trylock(p, locks);
#else
    return 0;
#endif
}


ERTS_GLB_INLINE void
erts_smp_proc_tab_lock(void)
{
#ifdef ERTS_SMP
    int i;
    for (i = 0; i < ERTS_PROC_LOCKS_NO_OF; i++)
	erts_smp_mtx_lock(&erts_proc_locks[i].mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_tab_unlock(void)
{
#ifdef ERTS_SMP
    int i;
    for (i = ERTS_PROC_LOCKS_NO_OF - 1; i >= 0; i--)
	erts_smp_mtx_unlock(&erts_proc_locks[i].mtx);
#endif
}

#ifdef ERTS_INCLUDE_SCHEDULER_INTERNALS

ERTS_GLB_INLINE int
erts_smp_sched_trylock(void)
{
#ifdef ERTS_SMP
    return erts_smp_mtx_trylock(&schdlq_mtx);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE void
erts_smp_sched_lock(void)
{
#ifdef ERTS_SMP
    erts_smp_mtx_lock(&schdlq_mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_sched_unlock(void)
{
#ifdef ERTS_SMP
    erts_smp_mtx_unlock(&schdlq_mtx);
#endif
}

ERTS_GLB_INLINE void
erts_smp_notify_inc_runq(void)
{
#ifdef ERTS_SMP
    ERTS_SMP_LC_ASSERT(erts_smp_is_sched_locked());
    if (erts_all_schedulers_waiting)
	erts_wake_one_scheduler();
#endif
}

#endif /* ERTS_INCLUDE_SCHEDULER_INTERNALS */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS

#endif



