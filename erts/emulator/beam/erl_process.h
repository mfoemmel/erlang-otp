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

#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_tables.h"

#ifdef HIPE
#include "hipe_process.h"
#endif

#define ERTS_DEFAULT_MAX_PROCESSES (1 << 15)


extern Uint erts_tot_proc_mem;


#define ERTS_PROC_MORE_MEM(Size) (erts_tot_proc_mem += (Size))

#define ERTS_PROC_LESS_MEM(Size) \
    (ASSERT_EXPR(erts_tot_proc_mem >= (Size)), erts_tot_proc_mem -= (Size))

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

#ifdef SHARED_HEAP

#define ERTS_STACK_ALLOC(Size)						\
    (ERTS_PROC_MORE_MEM((Size)),					\
     erts_alloc(ERTS_ALC_T_STACK, (Size)))

#define ERTS_STACK_REALLOC(Ptr, OldSize, NewSize)			\
    (ERTS_PROC_LESS_MEM((OldSize)),					\
     ERTS_PROC_MORE_MEM((NewSize)),					\
     erts_realloc(ERTS_ALC_T_STACK, (Ptr), (OldSize), (NewSize)))

#define ERTS_STACK_FREE(Ptr, Size)					\
    (ERTS_PROC_LESS_MEM((Size)),					\
     erts_free(ERTS_ALC_T_STACK, (Ptr)))
#endif

/*
** Timer entry:
*/
typedef struct erl_timer {
    struct erl_timer* next;	/* next entry tiw slot or chain */
    Uint slot;			/* slot in timer wheel */
    Uint count;			/* number of loops remaining */
    int    active;		/* 1=activated, 0=deactivated */
    /* called when timeout */
    FUNCTION(void, (*timeout), (void*));
    /* called when cancel (may be NULL) */
    FUNCTION(void, (*cancel), (void*));
    void* arg;        /* argument to timeout/cancel procs */
} ErlTimer;

typedef FUNCTION(void, (*ErlTimeoutProc), (void*));
typedef FUNCTION(void, (*ErlCancelProc), (void*));


#define INITIAL_MOD 0
#define INITIAL_FUN 1
#define INITIAL_ARI 2

struct saved_calls {
   int len;
   int n;
   int cur;
   Export *ct[1];
};

#ifndef HEAP_FRAG_ELIM_TEST
# define ERTS_SSB_PUT(p, a) do { } while(0)
#else
/*
 * Sequentical Store Buffer used to implement a write barrier for vector
 * operations.
 */
typedef struct erl_ssb {
    Eterm** next;
    Eterm** end;
    Eterm* buf[1];
} ErlSSB;

#define ERTS_SSB_PUT(p, a)			\
  if ((p)->ssb->next < (p)->ssb->end) {		\
    *(p)->ssb->next++ = (a);			\
  } else {					\
    erts_ssb_expand_put((p), (a));		\
  }
#endif

extern Export exp_send, exp_receive, exp_timeout;

typedef struct process {
    Eterm* htop;		/* Heap top */
    Eterm* stop;		/* Stack top */
    Eterm* heap;		/* Heap start */
    Eterm* hend;		/* Heap end */
    Uint heap_sz;		/* Size of heap in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */
#ifdef SHARED_HEAP
    Eterm* stack;               /* Stack start */
    Eterm* send;                /* Stack end */
    Uint stack_sz;              /* Size of stack in words */
    Uint active;                /* Active since last fullsweep? */
#else
    Uint16 gen_gcs;		/* Number of (minor) generational GCs. */
    Uint16 max_gen_gcs;		/* Max minor gen GCs before fullsweep. */
#endif

#ifdef HIPE
    /* HiPE-specific process fields. Put it early in struct process,
       to enable smaller & faster addressing modes on the x86. */
    struct hipe_process_state hipe;
#endif

    /* XXX: It would be highly advantageous for HiPE/x86 to also have
       the arity, def_arg_reg, i, and fcalls fields here at low offsets. */

#ifndef SHARED_HEAP
    /*
     * Heap pointers for generational GC.
     */

    Eterm *high_water;
    Eterm *old_hend;
    Eterm *old_htop;
    Eterm *old_heap;
#endif

    Uint32 status;		/* process STATE */
    Uint32 rstatus;		/* process resume STATE */
    int rcount;			/* suspend count */

    Eterm id;			/* The pid of this process */
    int    prio;		/* Priority of process */
    Uint reds;			/* No of reductions for this process  */
    Eterm error_handler;	/* Module atom for the error handler */
    Eterm tracer_proc;		/* If proc is traced, this is the tracer
				   (can NOT be boxed) */
    Eterm group_leader;		/* Pid in charge
				   (can be boxed) */
    Uint flags;			/* Trap exit, trace  flags etc */
    Eterm fvalue;		/* Exit & Throw value (failure reason) */
    Uint freason;		/* Reason for detected failure */
    Sint fcalls;		/* 
				 * Number of reductions left to execute.
				 * Only valid for the current process.
				 */
    DistEntry *dist_entry;	/* Distribution slot to use if F_DISTRIBUTION */

    ErlTimer tm;		/* Timer entry */

    struct process *next;	/* Pointer to next process in list */
#ifndef SHARED_HEAP
    ErlOffHeap off_heap;	/* Off-heap data updated by copy_struct(). */
#endif
    struct reg_proc *reg;	/* NULL iff not registered */
    struct erl_link* links;	/* List of links */

    ErlMessageQueue msg;	/* Message queue */
#ifndef SHARED_HEAP
    ErlHeapFragment* mbuf;	/* Pointer to message buffer list */
    ErlHeapFragment* halloc_mbuf; /* Pointer to first HAlloc() mbuf */
    Uint mbuf_sz;		/* Size of all message buffers */
#endif

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

    Eterm* cp;			/* Continuation pointer (for threaded code). */
    Eterm* i;			/* Program counter for threaded code. */
    Sint catches;		/* Number of catches on stack */

    /*
     * Secondary heap for arithmetic operations.
     */
#ifndef HEAP_FRAG_ELIM_TEST
    Eterm* saved_htop;		/* Saved HTOP. */
#endif

#ifndef SHARED_HEAP
    Eterm* arith_heap;		/* Current heap pointer. */
    Uint arith_avail;		/* Available space on arithmetic heap. */
    Eterm* arith_lowest_htop;
#endif
#if (defined(DEBUG) || defined(PURIFY)) && !defined(SHARED_HEAP)
    char* arith_file;
    int arith_line;
    Eterm* arith_check_me;	/* Address to check for overwrite. */
#endif

    /*
     * Saved x registers.
     */
    Uint arity;			/* Number of live argument registers (only valid
				 * when process is *not* running).
				 */
    Eterm* arg_reg;		/* Pointer to argument registers. */
    unsigned max_arg_reg;	/* Maximum number of argument registers available. */
#if defined(HIPE) && defined(__sparc__)
    Eterm def_arg_reg[16];	/* Default array for argument registers. */
#else
    Eterm def_arg_reg[6];	/* Default array for argument registers. */
#endif

#ifdef HEAP_FRAG_ELIM_TEST
    ErlSSB* ssb;		/*
				 * Sequential store buffer. NULL if no vectors
				 * in this process.
				 */
#endif

    /*
     * Information mainly for post-mortem use (erl crash dump).
     */
    Eterm parent;		/* Pid of process that created this process. */
    long started;		/* Time when started. */
} Process;

#ifdef HEAP_FRAG_ELIM_TEST
void erts_ensure_ssb(Process* p);
void erts_ssb_expand_put(Process* p, Eterm* addr);
#else
# define erts_ensure_ssb(p) (FLAGS(p) |= F_NEED_FULLSWEEP)
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

/*
 * The following struct contains options for a process to be spawned.
 */
typedef struct {
    Uint flags;
    int error_code;		/* Error code returned from create_process(). */

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

Eterm* erts_arith_alloc(Process* p, Eterm* htop, Uint need);
Eterm* erts_heap_alloc(Process* p, Uint need);
#ifdef SHARED_HEAP
Eterm* erts_global_alloc(Uint need);
#endif

extern Process** process_tab;
#ifdef SHARED_HEAP
extern Uint erts_num_active_procs;
extern Process** erts_active_procs;
#endif
extern Uint erts_max_processes;
Uint erts_process_tab_index_mask;
extern Uint erts_default_process_flags;
extern Eterm erts_default_tracer;
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

#define INVALID_PID(p, pid)	((p) == NULL				\
				 || (p)->id != (pid)			\
				 || (p)->status == P_EXITING)


#define INVALID_PORT(port, port_id) \
((port)->status == FREE || \
 (port)->id != (port_id) || \
 ((port)->status & CLOSING))

/* Invalidate trace port if anything suspicious, for instance
 * that the port is a distribution port or it is busy.
 */
#define INVALID_TRACER_PORT(port, port_id) \
((port) == NULL || \
 (port)->id != (port_id) || \
 (port)->status == FREE || \
 ((port)->status & (EXITING|CLOSING|PORT_BUSY|DISTRIBUTION)))
 
#define IS_TRACED(p)        ( (p)->tracer_proc != NIL)
#define IS_TRACED_FL(p,tf)  ( IS_TRACED(p) && ( ((p)->flags & (tf)) == (tf)) )

/* process priorities */
#define PRIORITY_MAX          0
#define PRIORITY_HIGH         1
#define PRIORITY_NORMAL       2
#define PRIORITY_LOW          3
#define NPRIORITY_LEVELS      4

/* process flags */
#define F_TRAPEXIT           (1 << 0)
#define F_INSLPQUEUE         (1 << 1) /* Set if in timer queue */
#define F_TIMO               (1 << 2) /* Set if timeout */
#define F_USING_DB           (1 << 3) /* If have created tables */
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
#define F_DISTRIBUTION       (1 << 16)  /* Process used in distribution */
#define F_HEAP_GROW          (1 << 17)
#define F_NEED_FULLSWEEP     (1 << 18) /* If process has old binaries & funs. */
#define F_TRACE_ARITY_ONLY   (1 << 19)
#define F_TRACE_RETURN_TO    (1 << 20) /* Return_to trace when breakpoint tracing */
#define F_TRACE_SILENT       (1 << 21) /* No call trace msg suppress */
#define F_TRACER             (1 << 22) /* May be (has been) tracer */

#define TRACE_FLAGS (  F_TRACE_PROCS | F_TRACE_CALLS \
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
	    erl_cancel_timer(&(p)->tm); \
	(p)->flags &= ~(F_INSLPQUEUE|F_TIMO); \
    } while (0)

void init_scheduler(void);
int  sched_q_len(void);
void add_to_schedule_q(Process*);
int  remove_proc_from_sched_q(Process*);
Process *schedule(Process*, int);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void schedule_exit(Process*, Eterm);
void do_exit(Process*, Eterm);
void set_timer(Process*, Uint);
void cancel_timer(Process*);

void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);
void erts_stack_dump(Process *, CIO);
void erts_program_counter_info(Process *, CIO);

void erts_deep_process_dump(CIO);

#endif
