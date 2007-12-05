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

typedef struct process Process;

#include "sys.h"

#define ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#include "erl_process_lock.h" /* Only pull out important types... */
#undef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__

#include "erl_vm.h"
#include "erl_smp.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#include "erl_time.h"

#ifdef HIPE
#include "hipe_process.h"
#endif

struct ErtsNodesMonitor_;
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

#include "export.h"

struct saved_calls {
   int len;
   int n;
   int cur;
   Export *ct[1];
};

extern Export exp_send, exp_receive, exp_timeout;
extern Uint erts_no_of_schedulers;

#ifdef ERTS_SMP
#include "erl_bits.h"
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
    Process *free_process;
#endif

    Process *current_process;

#ifdef ERTS_SMP_SCHEDULERS_NEED_TO_CHECK_CHILDREN
    /* NOTE: These fields are modified under held mutexes by other threads */
    int check_children; /* schdlq mutex */
    int blocked_check_children; /* multi_scheduling_block mutex */
#endif
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

#ifdef ERTS_SMP

typedef struct ErtsPendingSuspend_ ErtsPendingSuspend;
struct ErtsPendingSuspend_ {
    ErtsPendingSuspend *next;
    ErtsPendingSuspend *end;
    Eterm pid;
    void (*handle_func)(Process *suspendee,
			ErtsProcLocks suspendee_locks,
			int suspendee_alive,
			Eterm pid);
};

#endif

/* Defines to ease the change of memory architecture */
#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop
#  define HEAP_LIMIT(p)     (p)->stop
#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
#  define STACK_START(p)    (p)->hend
#  define STACK_TOP(p)      (p)->stop
#  define STACK_END(p)      (p)->htop
#  define HIGH_WATER(p)     (p)->high_water
#  define OLD_HEND(p)       (p)->old_hend
#  define OLD_HTOP(p)       (p)->old_htop
#  define OLD_HEAP(p)       (p)->old_heap
#  define GEN_GCS(p)        (p)->gen_gcs
#  define MAX_GEN_GCS(p)    (p)->max_gen_gcs
#  define FLAGS(p)          (p)->flags
#  define MBUF(p)           (p)->mbuf
#  define HALLOC_MBUF(p)    (p)->halloc_mbuf
#  define MBUF_SIZE(p)      (p)->mbuf_sz
#  define MSO(p)            (p)->off_heap
#  define MIN_HEAP_SIZE(p)  (p)->min_heap_size

struct process {
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
    Uint32 rcount;		/* suspend count */
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

    Process *next;		/* Pointer to next process in list */

    struct reg_proc *reg;	/* NULL iff not registered */
    ErtsLink *nlinks;
    ErtsMonitor *monitors;      /* The process monitors, both ends */

    struct ErtsNodesMonitor_ *nodes_monitors;

    ErtsSuspendMonitor *suspend_monitors; /* Processes suspended by
					     this process via
					     erlang:suspend_process/1 */

    ErlMessageQueue msg;	/* Message queue */

    ErtsBifTimer *bif_timers;	/* Bif timers aiming at this process */

    ProcDict  *dictionary;       /* Process dictionary, may be NULL */
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

#ifdef ERTS_SMP
    ErtsSmpPTimer *ptimer;
#else
    ErlTimer tm;		/* Timer entry */
#endif

#ifdef ERTS_SMP
    erts_proc_lock_t lock;
    ErtsSchedulerData *scheduler_data;
    int is_exiting;
    Uint32 scheduler_flags;
    Uint32 status_flags;
    ErlMessageInQueue msg_inq;
    Eterm suspendee;
    ErtsPendingSuspend *pending_suspenders;
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
    Process *active_next; /* Active processes to scan for roots */
    Process *active_prev; /* in collection of the message area  */
    Eterm *scan_top;
#endif

#ifdef CHECK_FOR_HOLES
    Eterm* last_htop;		/* No need to scan the heap below this point. */
    ErlHeapFragment* last_mbuf;	/* No need to scan beyond this mbuf. */
#endif
};

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

/* system_profile, same rules as for system_monitor.
	erts_profile must be != NIL when 
	erts_profile_* is set. */

extern Eterm erts_system_profile;
struct erts_system_profile_flags_t {
    unsigned int scheduler : 1;
    unsigned int runnable_procs : 1;
    unsigned int runnable_ports : 1;
    unsigned int exclusive : 1;
};
extern struct erts_system_profile_flags_t erts_system_profile_flags;

extern erts_smp_atomic_t erts_tot_proc_mem;

#define INVALID_PID(p, pid)	((p) == NULL				\
				 || (p)->id != (pid)			\
				 || (p)->status == P_EXITING)
 
#define IS_TRACED(p)             ( (p)->tracer_proc != NIL )
#define ARE_TRACE_FLAGS_ON(p,tf) ( ((p)->trace_flags & (tf|F_SENSITIVE)) == (tf) )
#define IS_TRACED_FL(p,tf)       ( IS_TRACED(p) && ARE_TRACE_FLAGS_ON(p,tf) )

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
#define F_HEAP_GROW          (1 << 3)
#define F_NEED_FULLSWEEP     (1 << 4) /* If process has old binaries & funs. */
#define F_USING_DB           (1 << 5) /* If have created tables */
#define F_DISTRIBUTION       (1 << 6) /* Process used in distribution */
#define F_USING_DDLL         (1 << 7) /* Process has used the DDLL interface */
#define F_HAVE_BLCKD_MSCHED  (1 << 8) /* Process has blocked multi-scheduling */
#define F_P2PNR_RESCHED      (1 << 9) /* Process has been rescheduled via
					 erts_pid2proc_not_running() */

/* process trace_flags */
#define F_SENSITIVE          (1 << 0)
#define F_TRACE_SEND         (1 << 1)   
#define F_TRACE_RECEIVE      (1 << 2)
#define F_TRACE_SOS          (1 << 3) /* Set on spawn       */
#define F_TRACE_SOS1         (1 << 4) /* Set on first spawn */
#define F_TRACE_SOL          (1 << 5) /* Set on link        */
#define F_TRACE_SOL1         (1 << 6) /* Set on first link  */
#define F_TRACE_CALLS        (1 << 7)
#define F_TIMESTAMP          (1 << 8)
#define F_TRACE_PROCS        (1 << 9)
#define F_TRACE_FIRST_CHILD  (1 << 10)
#define F_TRACE_SCHED        (1 << 11)
#define F_TRACE_GC           (1 << 12)
#define F_TRACE_ARITY_ONLY   (1 << 13)
#define F_TRACE_RETURN_TO    (1 << 14) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       (1 << 15) /* No call trace msg suppress */
#define F_TRACER             (1 << 16) /* May be (has been) tracer */
#define F_EXCEPTION_TRACE    (1 << 17) /* May have exception trace on stack */

/* port trace flags, currently the same as process trace flags */
#define F_TRACE_SCHED_PORTS  (1 << 18) /* Trace of port scheduling */
#define F_TRACE_SCHED_PROCS  (1 << 19) /* With virtual scheduling */
#define F_TRACE_PORTS	     (1 << 20) /* Ports equivalent to F_TRACE_PROCS */
#define F_TRACE_SCHED_NO     (1 << 21) /* Trace with scheduler id */

#define F_NUM_FLAGS          22
#ifdef DEBUG
#  define F_INITIAL_TRACE_FLAGS (5 << F_NUM_FLAGS)
#else
#  define F_INITIAL_TRACE_FLAGS 0
#endif



#define TRACEE_FLAGS (  F_TRACE_PROCS | F_TRACE_CALLS \
		     | F_TRACE_SOS |  F_TRACE_SOS1| F_TRACE_RECEIVE  \
		     | F_TRACE_SOL | F_TRACE_SOL1 | F_TRACE_SEND | \
		     F_TRACE_SCHED | F_TIMESTAMP | F_TRACE_GC  | \
		     F_TRACE_ARITY_ONLY | F_TRACE_RETURN_TO | \
                     F_TRACE_SILENT | F_TRACE_SCHED_PROCS | F_TRACE_PORTS | \
		     F_TRACE_SCHED_PORTS | F_TRACE_SCHED_NO)

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
int erts_block_multi_scheduling(Process *, ErtsProcLocks, int, int);
int erts_is_multi_scheduling_blocked(void);
Eterm erts_multi_scheduling_blockers(Process *);
void erts_start_schedulers(Uint);
void erts_smp_notify_check_children_needed(void);
#endif
void erts_init_process(void);
Eterm erts_process_status(Process *, ErtsProcLocks, Process *, Eterm);
int  sched_q_len(void);
void add_to_schedule_q(Process*);
Process *schedule(Process*, int);
void erts_schedule_misc_op(void (*)(void *), void *);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void erts_do_exit_process(Process*, Eterm);
void set_timer(Process*, Uint);
void cancel_timer(Process*);
/* Begin System profile */
Uint erts_runnable_process_count(void);
Uint erts_process_count(void);
/* End System profile */
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

void erts_free_proc(Process *);

void erts_suspend(Process*, ErtsProcLocks, struct port*);
void erts_resume(Process*, ErtsProcLocks);
int erts_send_exit_signal(Process *,
			  Eterm,
			  Process *,
			  ErtsProcLocks *,
			  Eterm,
			  Eterm,
			  Process *,
			  Uint32);
#ifdef ERTS_SMP
void erts_handle_pending_exit(Process *, ErtsProcLocks);
#define ERTS_PROC_PENDING_EXIT(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) & ERTS_PROC_LOCK_STATUS),\
   (P)->pending_exit.reason != THE_NON_VALUE)
#else
#define ERTS_PROC_PENDING_EXIT(P) 0
#endif

#ifdef ERTS_SMP
Process *erts_suspend_another_process(Process *c_p, ErtsProcLocks c_p_locks,
				      Eterm suspendee, ErtsProcLocks suspendee_locks);
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

#ifdef ERTS_SMP

Process *erts_pid2proc_not_running(Process *,
				   ErtsProcLocks,
				   Eterm,
				   ErtsProcLocks);

#ifdef DEBUG
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P) \
  do { ASSERT(!(P)->is_exiting); } while (0)
#else
#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)
#endif

/* NOTE: At least one process lock has to be held on P! */
#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_PROC_IS_EXITING(P) \
  (ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks((P)) != 0 \
		      || erts_lc_pix_lock_is_locked(ERTS_PID2PIXLOCK((P)->id))),\
   (P)->is_exiting)
#else
#define ERTS_PROC_IS_EXITING(P) ((P)->is_exiting)
#endif

#else /* !ERTS_SMP */

#define ERTS_PROC_IS_EXITING(P) ((P)->status == P_EXITING)

#define ERTS_SMP_ASSERT_IS_NOT_EXITING(P)

#define erts_pid2proc_not_running erts_pid2proc

#endif

/* Minimum NUMBER of processes for a small system to start */
#ifdef ERTS_SMP
#define ERTS_MIN_PROCESSES		ERTS_NO_OF_PIX_LOCKS
#else
#define ERTS_MIN_PROCESSES		16
#endif

void erts_smp_proc_tab_lock(void);
void erts_smp_proc_tab_unlock(void);

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

#include "erl_process_lock.h"

#undef ERTS_INCLUDE_SCHEDULER_INTERNALS

#endif



