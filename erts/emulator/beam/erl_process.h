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

#ifdef HIPE
#include "hipe_process.h"
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

extern Export exp_send, exp_receive, exp_timeout;

typedef struct process {
    Eterm* htop;		/* Heap top */
    Eterm* stop;		/* Stack top */
#ifdef UNIFIED_HEAP
    Eterm* hend;                /* Heap end */
    Eterm* stack;               /* Stack start */
    Eterm* send;                /* Stack end */
    Uint stack_sz;              /* Size of stack in words */
    Uint min_heap_size;         /* Minimum size of heap (in words). */
    Uint active;                /* Active since last fullsweep? */
#else
    Eterm* heap;		/* Heap start */
    Eterm* hend;		/* Heap end */
    Uint heap_sz;		/* Size of heap in words */
    Uint min_heap_size;		/* Minimum size of heap (in words). */
    Uint16 gen_gcs;		/* Number of (minor) generational GCs. */
    Uint16 max_gen_gcs;		/* Max minor gen GCs before fullsweep. */
#endif

#ifdef HIPE
    /* HiPE-specific process fields. Put it early in struct process,
       to enable smaller & faster addressing modes on the x86. */
    struct hipe_process_state hipe;
#endif

#ifndef UNIFIED_HEAP
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
    Eterm tracer_proc;		/* If proc is traced, this is the tracer */
    Eterm group_leader;		/* Pid in charge */
    Uint flags;			/* Trap exit, trace  flags etc */
    Eterm fvalue;		/* Exit & Throw value (failure reason) */
    Uint freason;	    /* Reason for detected failure */
    Sint fcalls;		/* 
				 * Number of reductions left to execute.
				 * Only valid for the current process.
				 */
    int    dslot;		/* Distribution slot to use if F_DISTRIBUTION */

    ErlTimer tm;		/* Timer entry */

    struct process *next;	/* Pointer to next process in list */
    ErlOffHeap off_heap;	/* Off-heap data updated by copy_struct(). */
    struct reg_proc *reg;	/* NULL iff not registered */
    Eterm reg_atom;		/* atom for formerly registered name, */
				/* when exiting */

    struct erl_link* links;         /* List of links */

    ErlMessageQueue msg;	/* Message queue */
#ifndef UNIFIED_HEAP
    ErlHeapFragment* mbuf;	/* Pointer to message buffer list */
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
    Eterm* arith_heap;		/* Current heap pointer. */
    Uint arith_avail;		/* Available space on arithmetic heap. */
#ifdef DEBUG
    char* arith_file;		/* Filename of last ArithAlloc(). */
    int arith_line;		/* And linenumber. */
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
    Eterm def_arg_reg[6];	/* Default array for argument registers. */
} Process;

#ifdef UNIFIED_HEAP
/*
 * Macros to store live processes in the active_procs array.
 * The array is used in GC and the macros are called from erl_message.c
 * and erl_process.c
 */

#define ADD_TO_ROOTSET(p) \
do { \
    int active_index = 0; \
    while ((active_procs[active_index] != NULL) && \
           (active_procs[active_index] != (Process*)NIL) && \
           (active_procs[active_index] != p)) \
    { \
        active_index++; \
    }  \
    if (active_procs[active_index] != p) \
    { \
        if (active_procs[active_index] == NULL) \
            active_procs[active_index+1] = NULL; \
        active_procs[active_index] = p; \
    } \
} while(0)

#define REMOVE_FROM_ROOTSET(p) \
do { \
    int active_index = 0; \
    while ((active_procs[active_index] != NULL) && \
           (active_procs[active_index] != p)) \
    { \
        active_index++;  \
    } \
    if (active_procs[active_index] != NULL) \
      active_procs[active_index] = (Process*)NIL; \
} while (0)
#endif

/*
 * The MBUF_GC_FACTOR descides how easily a process is subject to GC 
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

extern Eterm* arith_alloc(Process* p, Uint need);

extern Process** process_tab;
#ifdef UNIFIED_HEAP
extern Process** active_procs;
#endif
extern Uint max_process;
extern Uint erts_default_process_flags;
extern Eterm erts_default_tracer;

#define INVALID_PID(p, pid) (p == NULL || \
			     p->id != pid || \
			     p->status == P_EXITING)

/* Invalidate trace port if anything suspicious, for instance
 * that the port is a distribution port or it is busy.
 */
#define INVALID_TRACER_PORT(port, port_id) \
(port == NULL || \
 port->id != port_id || \
 port->status == FREE || \
 (port->status & (EXITING|CLOSING|PORT_BUSY|DISTRIBUTION)))

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
#define F_DEFAULT_TRACER     (1 << 22) /* May be (has been) default tracer */

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
int  schedule(void);
Eterm erl_create_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void delete_process(Process*);
void schedule_exit(Process*, Eterm);
void do_exit(Process*, Eterm);
void set_timer(Process*, Uint);
void cancel_timer(Process*);
int keep_zombies(int, int*);
void process_info_zombies(CIO);

void erts_init_empty_process(Process *p);
void erts_cleanup_empty_process(Process* p);

#endif
