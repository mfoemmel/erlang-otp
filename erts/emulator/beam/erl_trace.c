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
 * Support functions for tracing.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "dist.h"
#include "beam_bp.h"
#include "error.h"

#if 0
#define DEBUG_PRINTOUTS
#else
#undef DEBUG_PRINTOUTS
#endif

extern Eterm beam_return_to_trace[1]; /* OpCode(i_return_to_trace) */
extern Eterm beam_return_trace[1];    /* OpCode(i_return_trace) */

/* Pseudo export entries. Never filled in with data, only used to
   yield unique pointers of the correct type. */
Export exp_send, exp_receive, exp_timeout;

static Eterm system_seq_tracer;
static Uint default_trace_flags;
static Eterm default_tracer;

static Eterm system_monitor;

#ifdef HAVE_ERTS_NOW_CPU
int erts_cpu_timestamp;
#endif

static erts_smp_mtx_t smq_mtx;
static erts_smp_mtx_t sys_trace_mtx;

enum ErtsSysMsgType {
    SYS_MSG_TYPE_UNDEFINED,
    SYS_MSG_TYPE_TRACE,
    SYS_MSG_TYPE_SEQTRACE,
    SYS_MSG_TYPE_SYSMON,
    SYS_MSG_TYPE_ERRLGR,
    SYS_MSG_TYPE_PROC_MSG
};

#ifdef ERTS_SMP
static void enqueue_sys_msg_unlocked(enum ErtsSysMsgType type,
				     Eterm from,
				     Eterm to,
				     Eterm msg,
				     ErlHeapFragment *bp);
static void enqueue_sys_msg(enum ErtsSysMsgType type,
			    Eterm from,
			    Eterm to,
			    Eterm msg,
			    ErlHeapFragment *bp);
static void init_sys_msg_dispatcher(void);
#endif

void erts_init_trace(void) {
    erts_smp_mtx_init(&sys_trace_mtx, "sys_tracers");
#ifdef HAVE_ERTS_NOW_CPU
    erts_cpu_timestamp = 0;
#endif
    erts_bif_trace_init();
    erts_system_monitor_clear(NULL);
    default_trace_flags = 0;
    default_tracer = NIL;
    system_seq_tracer = am_false;
#ifdef ERTS_SMP
    init_sys_msg_dispatcher();
#endif
}

static Eterm system_seq_tracer;

void
erts_trace_check_exiting(Eterm exiting)
{
    erts_smp_mtx_lock(&sys_trace_mtx);
    if (exiting == default_tracer) {
	default_tracer = NIL;
	default_trace_flags &= TRACEE_FLAGS;
    }
    if (exiting == system_seq_tracer) {
#ifdef DEBUG_PRINTOUTS
	erts_fprintf(stderr, "seq tracer %T exited\n", exiting);
#endif
	system_seq_tracer = am_false;
    }
    if (exiting == system_monitor) {
#ifdef ERTS_SMP
	system_monitor = NIL;
	/* Let the trace message dispatcher clear flags, etc */
#else
	erts_system_monitor_clear(NULL);
#endif
    }
    erts_smp_mtx_unlock(&sys_trace_mtx);
}

Eterm
erts_set_system_seq_tracer(Process *c_p, Uint32 c_p_locks, Eterm new)
{
    Eterm old = THE_NON_VALUE;

    if (new != am_false) {
	if (!erts_pid2proc(c_p, c_p_locks, new, 0)
	    && !erts_is_valid_tracer_port(new)) {
	    return old;
	}
    }

    erts_smp_mtx_lock(&sys_trace_mtx);
    old = system_seq_tracer;
    system_seq_tracer = new;

#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "set seq tracer new=%T old=%T\n", new, old);
#endif
    erts_smp_mtx_unlock(&sys_trace_mtx);
    return old;
}

Eterm
erts_get_system_seq_tracer(void)
{
    Eterm st;
    erts_smp_mtx_lock(&sys_trace_mtx);
    st = system_seq_tracer;
#ifdef DEBUG_PRINTOUTS
    erts_fprintf(stderr, "get seq tracer %T\n", st);
#endif
    erts_smp_mtx_unlock(&sys_trace_mtx);
    return st;
}

static ERTS_INLINE void
get_default_tracing(Uint *flagsp, Eterm *tracerp)
{
    if (!(default_trace_flags & TRACEE_FLAGS))
	default_tracer = NIL;

    if (is_nil(default_tracer)) {
	default_trace_flags &= ~TRACEE_FLAGS;
    } else if (is_internal_pid(default_tracer)) {
	if (!erts_pid2proc(NULL, 0, default_tracer, 0)) {
	reset_tracer:
	    default_trace_flags &= ~TRACEE_FLAGS;
	    default_tracer = NIL;
	}
    } else {
	ASSERT(is_internal_port(default_tracer));
	if (!erts_is_valid_tracer_port(default_tracer))
	    goto reset_tracer;
    }

    if (flagsp)
	*flagsp = default_trace_flags;
    if (tracerp)
	*tracerp = default_tracer;
}

void
erts_change_default_tracing(int setflags, Uint *flagsp, Eterm *tracerp)
{
    erts_smp_mtx_lock(&sys_trace_mtx);
    if (flagsp) {
	if (setflags)
	    default_trace_flags |= *flagsp;
	else
	    default_trace_flags &= ~(*flagsp);
    }
    if (tracerp)
	default_tracer = *tracerp;
    get_default_tracing(flagsp, tracerp);
    erts_smp_mtx_unlock(&sys_trace_mtx);
}

void
erts_get_default_tracing(Uint *flagsp, Eterm *tracerp)
{
    erts_smp_mtx_lock(&sys_trace_mtx);
    get_default_tracing(flagsp, tracerp);
    erts_smp_mtx_unlock(&sys_trace_mtx);
}

void
erts_set_system_monitor(Eterm monitor)
{
    erts_smp_mtx_lock(&sys_trace_mtx);
    system_monitor = monitor;
    erts_smp_mtx_unlock(&sys_trace_mtx);
}

Eterm
erts_get_system_monitor(void)
{
    Eterm monitor;
    erts_smp_mtx_lock(&sys_trace_mtx);
    monitor = system_monitor;
    erts_smp_mtx_unlock(&sys_trace_mtx);
    return monitor;
}

#ifdef HAVE_ERTS_NOW_CPU
#  define GET_NOW(m, s, u) \
do { \
    if (erts_cpu_timestamp) \
	erts_get_now_cpu(m, s, u); \
    else \
	get_now(m, s, u); \
} while (0)
#else
#  define GET_NOW(m, s, u) do {get_now(m, s, u);} while (0)
#endif



static Eterm* patch_ts(Eterm tuple4, Eterm* hp);

#ifdef ERTS_SMP
static void
do_send_to_port(Eterm to,
		Port* unused_port,
		Eterm from,
		enum ErtsSysMsgType type,
		Eterm message)
{
    Uint sz = size_object(message);
    ErlHeapFragment *bp = new_message_buffer(sz);
    Uint *hp = bp->mem;
    Eterm msg = copy_struct(message, sz, &hp, &bp->off_heap);

    enqueue_sys_msg_unlocked(type, from, to, msg, bp);
}

#define WRITE_SYS_MSG_TO_PORT write_sys_msg_to_port
#else
#define WRITE_SYS_MSG_TO_PORT do_send_to_port
#endif

static void
WRITE_SYS_MSG_TO_PORT(Eterm unused_to,
		      Port* trace_port,
		      Eterm unused_from,
		      enum ErtsSysMsgType unused_type,
		      Eterm message) {
    byte *buffer;
    byte *ptr;
    unsigned size;

    size = encode_size_struct(message, TERM_TO_BINARY_DFLAGS);
    buffer = (byte *) erts_alloc(ERTS_ALC_T_TMP, size);

    ptr = buffer;
    erts_to_external_format(NULL, message, &ptr, NULL, NULL);
    if (!(ptr <= buffer+size)) {
	erl_exit(1, "Internal error in do_send_to_port: %d\n", ptr-buffer);
    }

#ifndef ERTS_SMP
    if (!INVALID_TRACER_PORT(trace_port, trace_port->id)) {
#endif
	dist_port_command(trace_port, buffer, ptr-buffer);
#ifndef ERTS_SMP
	erts_port_release(trace_port);
    }
#endif

    erts_free(ERTS_ALC_T_TMP, (void *) buffer);
}


#ifndef ERTS_SMP
/* Send        {trace_ts, Pid, out, 0, Timestamp}
 * followed by {trace_ts, Pid, in, 0, NewTimestamp}
 *
 * 'NewTimestamp' is fetched from GET_NOW() through patch_ts().
 */
static void 
do_send_schedfix_to_port(Port *trace_port, Eterm pid, Eterm timestamp) {
    Eterm local_heap[4+5+5];
    Eterm message;
    Eterm *hp;
    Eterm mfarity;

    ASSERT(is_pid(pid));
    ASSERT(is_tuple(timestamp));
    ASSERT(*tuple_val(timestamp) == make_arityval(3));
    
    hp = local_heap;
    mfarity = make_small(0);
    message = TUPLE5(hp, am_trace_ts, pid, am_out, mfarity, timestamp);
    /* Note, hp is deliberately NOT incremented since it will be reused */

    do_send_to_port(trace_port->id,
		    trace_port,
		    pid,
		    SYS_MSG_TYPE_UNDEFINED,
		    message);

    message = TUPLE4(hp, am_trace_ts, pid, am_in, mfarity);
    hp += 5;
    hp = patch_ts(message, hp);

    do_send_to_port(trace_port->id,
		    trace_port,
		    pid,
		    SYS_MSG_TYPE_UNDEFINED,
		    message);
}
#endif

/* If (c_p != NULL), a fake schedule out/in message pair will be sent,
 * if the driver so requests. 
 * It is assumed that 'message' is not an 'out' message.
 *
 * 'c_p' is the currently executing process, "tracee" is the traced process
 * which 'message' concerns => if (*tracee_flags & F_TIMESTAMP), 
 * 'message' must contain a timestamp.
 */
static void
send_to_port(Process *c_p, Eterm message, 
	     Eterm *tracer_pid, Uint *tracee_flags) {
    Port* trace_port;
#ifndef ERTS_SMP
    Eterm ts, local_heap[4], *hp;
#endif

    ASSERT(is_internal_port(*tracer_pid));
#ifdef ERTS_SMP
    if (is_not_internal_port(*tracer_pid))
	return;

    trace_port = NULL;
#else
    if (is_not_internal_port(*tracer_pid))
	goto invalid_tracer_port;

    trace_port = &erts_port[internal_port_index(*tracer_pid)];

    if (INVALID_TRACER_PORT(trace_port, *tracer_pid)) {
    invalid_tracer_port:
	*tracee_flags &= ~TRACEE_FLAGS;
	*tracer_pid = NIL;
	return;
    }

    /*
     * Make a fake schedule only if the current process is traced
     * with 'running' and 'timestamp'.
     */

    if (c_p == NULL || (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) {
#endif
	do_send_to_port(*tracer_pid,
			trace_port,
			c_p ? c_p->id : NIL,
			SYS_MSG_TYPE_TRACE,
			message);
#ifndef ERTS_SMP
	return;
    }

    /*
     * Note that the process being traced for some type of trace messages
     * (e.g. getting_linked) need not be the current process. That other
     * process might not have timestamps enabled.
     */
    if (*tracee_flags & F_TIMESTAMP) {
	ASSERT(is_tuple(message));
	hp = tuple_val(message);
	ts = hp[arityval(hp[0])];
    } else {
	/* A fake schedule might be needed,
	 * but this message does not contain a timestamp.
	 * Create a dummy trace message with timestamp to be
	 * passed to do_send_schedfix_to_port().
	 */
	Uint ms,s,us;
	GET_NOW(&ms, &s, &us);
	hp = local_heap;
	ts = TUPLE3(hp, make_small(ms), make_small(s), make_small(us));
	hp += 4;
    }

    trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
    do_send_to_port(*tracer_pid,
		    trace_port,
		    c_p ? c_p->id : NIL,
		    SYS_MSG_TYPE_TRACE,
		    message);

    if (trace_port->control_flags & PORT_CONTROL_FLAG_HEAVY) {
	/* The driver has just informed us that the last write took a 
	 * non-neglectible amount of time.
	 *
	 * We need to fake some trace messages to compensate for the time the
	 * current process had to sacrifice for the writing of the previous
	 * trace message. We pretend that the process got scheduled out
	 * just after writning the real trace message, and now gets scheduled
	 * in again.
	 */
	do_send_schedfix_to_port(trace_port, c_p->id, ts);
    }
#endif
}

/* A fake schedule out/in message pair will be sent,
 * if the driver so requests.
 * If (timestamp == NIL), one is fetched from GET_NOW().
 *
 * 'c_p' is the currently executing process, may be NULL.
 */
static void
seq_trace_send_to_port(Process *c_p,
		       Eterm seq_tracer,
		       Eterm message,
		       Eterm timestamp)
{
    Port* trace_port;
#ifndef ERTS_SMP
    Eterm ts, local_heap[4], *hp;
#endif

    ASSERT(is_internal_port(seq_tracer));
#ifdef ERTS_SMP
    if (is_not_internal_port(seq_tracer))
	return;

    trace_port = NULL;
#else
    if (is_not_internal_port(seq_tracer))
	goto invalid_tracer_port;

    trace_port = &erts_port[internal_port_index(seq_tracer)];

    if (INVALID_TRACER_PORT(trace_port, seq_tracer)) {
    invalid_tracer_port:
	system_seq_tracer = am_false;
	return;
    }

    if (c_p == NULL
	|| (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) {
#endif
	do_send_to_port(seq_tracer,
			trace_port,
			c_p ? c_p->id : NIL,
			SYS_MSG_TYPE_SEQTRACE,
			message);

#ifndef ERTS_SMP
	return;
    }
    /* Make a fake schedule only if the current process is traced
     * with 'running' and 'timestamp'.
     */

    if (timestamp != NIL) {
	ts = timestamp;
    } else {
	/* A fake schedule might be needed,
	 * but this message does not contain a timestamp.
	 * Create a dummy trace message with timestamp to be
	 * passed to do_send_schedfix_to_port().
	 */
	Uint ms,s,us;
	GET_NOW(&ms, &s, &us);
	hp = local_heap;
	ts = TUPLE3(hp, make_small(ms), make_small(s), make_small(us));
	hp += 4;
    }

    trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
    do_send_to_port(seq_tracer,
		    trace_port,
		    c_p ? c_p->id : NIL,
		    SYS_MSG_TYPE_SEQTRACE,
		    message);

    if (trace_port->control_flags & PORT_CONTROL_FLAG_HEAVY) {
	/* The driver has just informed us that the last write took a 
	 * non-neglectible amount of time.
	 *
	 * We need to fake some trace messages to compensate for the time the
	 * current process had to sacrifice for the writing of the previous
	 * trace message. We pretend that the process got scheduled out
	 * just after writning the real trace message, and now gets scheduled
	 * in again.
	 */
	do_send_schedfix_to_port(trace_port, c_p->id, ts);
    }
#endif
}

#define TS_SIZE(p) (((p)->trace_flags & F_TIMESTAMP) ? 5 : 0)

/*
 * Patch a timestamp into a tuple.  The tuple must be the last thing
 * built on the heap.
 *
 * Returns the new hp pointer.
*/
static Eterm*
patch_ts(Eterm tuple, Eterm* hp)
{
    Uint ms, s, us;
    Eterm* ptr = tuple_val(tuple);
    int arity = arityval(*ptr);

    ASSERT((ptr+arity+1) == hp);
    ptr[0] = make_arityval(arity+1);
    ptr[1] = am_trace_ts;
    GET_NOW(&ms, &s, &us);
    *hp = TUPLE3(hp+1, make_small(ms), make_small(s), make_small(us));
    return hp+5;
}

/* Send {trace_ts, Pid, What, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, What, {Mod, Func, Arity}}
 *
 * where 'What' is supposed to be 'in' or 'out'.
 */
void
trace_sched(Process *p, Eterm what)
{
    Eterm tmp;
    Eterm mess;
    Eterm* hp;

    if (is_internal_port(p->tracer_proc)) {
	Eterm local_heap[4+5+5];
	hp = local_heap;

	if (p->current == NULL) {
	    p->current = find_function_from_pc(p->i);
	}
	if (p->current == NULL) {
	    tmp = make_small(0);
	} else {
	    tmp = TUPLE3(hp, p->current[0], p->current[1], make_small(p->current[2]));
	    hp += 4;
	}
	mess = TUPLE4(hp, am_trace, p->id, what, tmp);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	if (what != am_out) {
	    send_to_port(p, mess, &p->tracer_proc, &p->trace_flags);
	} else {
	    send_to_port(NULL, mess, &p->tracer_proc, &p->trace_flags);
	}
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

#ifdef ERTS_SMP
	bp = new_message_buffer(9 + TS_SIZE(p));
	hp = bp->mem;
#else
	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc) 
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}

	hp = HAlloc(tracer, 9 + TS_SIZE(p));

#endif
	
	if (p->current == NULL) {
	    p->current = find_function_from_pc(p->i);
	}
	if (p->current == NULL) {
	    tmp = make_small(0);
	} else {
	    tmp = TUPLE3(hp, p->current[0], p->current[1], make_small(p->current[2]));
	    hp += 4;
	}
	mess = TUPLE4(hp, am_trace,p->id/* Local pid */, what, tmp);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 p->id, p->tracer_proc, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif

    }
}


/* Send {trace_ts, Pid, Send, Msg, DestPid, Timestamp}
 * or   {trace, Pid, Send, Msg, DestPid}
 *
 * where 'Send' is 'send' or 'send_to_non_existing_process'.
 */
void
trace_send(Process *p, Eterm to, Eterm msg)
{
    Eterm operation;
    unsigned sz_msg;
    unsigned sz_to;
    Eterm* hp;
    Eterm mess;
    
    if (!(p->trace_flags & F_TRACE_SEND)) {
	return;
    }

    operation = am_send;
    if (is_internal_pid(to)) {
	if (!erts_pid2proc(p, ERTS_PROC_LOCK_MAIN, to, 0))
	    goto send_to_non_existing_process;
    }
    else if(is_external_pid(to)
	    && external_pid_dist_entry(to) == erts_this_dist_entry) {
	char *s;
    send_to_non_existing_process:
	s = "send_to_non_existing_process";
	operation = am_atom_put(s, sys_strlen(s));
    }

    if (is_internal_port(p->tracer_proc)) {
	Eterm local_heap[11];
	hp = local_heap;
	mess = TUPLE5(hp, am_trace, p->id, operation, msg, to);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &p->tracer_proc, &p->trace_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif

	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(p->tracer_proc)];

	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
#endif

	sz_msg = size_object(msg);
	sz_to  = size_object(to);


#ifdef ERTS_SMP
	bp = new_message_buffer(sz_msg + sz_to + 6 + TS_SIZE(p));
	hp = bp->mem;
#else
	hp = HAlloc(tracer, sz_msg + sz_to + 6 + TS_SIZE(p));
#endif

	to = copy_struct(to,
			 sz_to,
			 &hp,
#ifdef ERTS_SMP
			 &bp->off_heap
#else
			 &MSO(tracer)
#endif
	    );
	msg = copy_struct(msg,
			  sz_msg,
			  &hp,
#ifdef ERTS_SMP
			  &bp->off_heap
#else
			  &MSO(tracer)
#endif
	    );
	mess = TUPLE5(hp, am_trace, p->id/* Local pid */, operation, msg, to);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (p->trace_flags & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE, p->id, p->tracer_proc, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif

    }
}

/* Send {trace_ts, Pid, receive, Msg, Timestamp}
 * or   {trace, Pid, receive, Msg}
 */
void
trace_receive(Process *rp, Eterm msg)
{
    Eterm mess;
    size_t sz_msg;
    Eterm* hp;

    if (is_internal_port(rp->tracer_proc)) {
	Eterm local_heap[10];
	hp = local_heap;
	mess = TUPLE4(hp, am_trace, rp->id, am_receive, msg);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (rp->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(rp, mess, &rp->tracer_proc, &rp->trace_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Uint hsz;
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif

	ASSERT(is_internal_pid(rp->tracer_proc)
	       && internal_pid_index(rp->tracer_proc) < erts_max_processes);

#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(rp->tracer_proc)];
	if (INVALID_PID(tracer, rp->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    rp->trace_flags &= ~TRACEE_FLAGS;
	    rp->tracer_proc = NIL;
	    return;
	}
#endif

	sz_msg = size_object(msg);

	hsz = sz_msg + 5 + TS_SIZE(rp);

#ifdef ERTS_SMP
	bp = new_message_buffer(hsz);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, hsz);
#endif

	msg = copy_struct(msg,
			  sz_msg,
			  &hp,
#ifdef ERTS_SMP
			  &bp->off_heap
#else
			  &MSO(tracer)
#endif
	    );
	mess = TUPLE4(hp, am_trace, rp->id/* Local pid */, am_receive, msg);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (rp->trace_flags & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				   rp->id, rp->tracer_proc, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
}

int seq_trace_update_send(p)
Process *p;
{
    Eterm seq_tracer = erts_get_system_seq_tracer();
    ASSERT((is_tuple(SEQ_TRACE_TOKEN(p)) || is_nil(SEQ_TRACE_TOKEN(p))));
    if ( (p->id == seq_tracer) || (SEQ_TRACE_TOKEN(p) == NIL))
	return 0;

    SEQ_TRACE_TOKEN_SENDER(p) = p->id; /* Internal pid */
    SEQ_TRACE_TOKEN_SERIAL(p) = 
	make_small(++(p -> seq_trace_clock));
    SEQ_TRACE_TOKEN_LASTCNT(p) = 
	make_small(p -> seq_trace_lastcnt);
    return 1;
}


/* Send a sequential trace message to the sequential tracer.
 * p is the caller (which contains the trace token), 
 * msg is the original message, type is trace type (SEQ_TRACE_SEND etc),
 * and receiver is the receiver of the message.
 *
 * The message to be received by the sequential tracer is:
 * 
 *    TraceMsg = 
 *   {seq_trace, Label, {Type, {Lastcnt, Serial}, Sender, Receiver, Msg} [,Timestamp] }
 *
 */
void 
seq_trace_output_generic(Eterm token, Eterm msg, Uint type,
			 Eterm receiver, Process *process, Eterm exitfrom)
{
    Eterm mess;
    ErlHeapFragment* bp;
    Eterm* hp;
    Eterm label;
    Eterm lastcnt_serial;
    Eterm type_atom;
    int sz_exit;
    Eterm seq_tracer;

    seq_tracer = erts_get_system_seq_tracer();

    ASSERT(is_tuple(token) || is_nil(token));
    if ( (SEQ_TRACE_T_SENDER(token) == seq_tracer) || (token == NIL))
	return;

    switch (type) {
    case SEQ_TRACE_SEND:    type_atom = am_send; break;
    case SEQ_TRACE_PRINT:   type_atom = am_print; break;
    case SEQ_TRACE_RECEIVE: type_atom = am_receive; break;
    default:
	erl_exit(1, "invalid type in seq_trace_output_generic: %d:\n", type);
	return;			/* To avoid warning */
    }

    if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & type) == 0) {
	/* No flags set, nothing to do */
	return;
    }

    if (seq_tracer == am_false) {
	return;			/* no need to send anything */
    }

    if (is_internal_port(seq_tracer)) {
	Eterm local_heap[64];
	hp = local_heap;
	label = SEQ_TRACE_T_LABEL(token);
	lastcnt_serial = TUPLE2(hp, SEQ_TRACE_T_LASTCNT(token),
				SEQ_TRACE_T_SERIAL(token));
	hp += 3;
	if (exitfrom != NIL) {
	    msg = TUPLE3(hp, am_EXIT, exitfrom, msg);
	    hp += 4;
	}
	mess = TUPLE5(hp, type_atom, lastcnt_serial, SEQ_TRACE_T_SENDER(token),
		      receiver, msg);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & SEQ_TRACE_TIMESTAMP) == 0) {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	    seq_trace_send_to_port(NULL, seq_tracer, mess, NIL);
	} else {
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	    seq_trace_send_to_port(process, seq_tracer, mess, ts);
	}
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifndef ERTS_SMP
	Process* tracer;
#endif
	Eterm sender_copy;
	Eterm receiver_copy;
	Eterm m2;
	Uint sz_label, sz_lastcnt_serial, sz_msg, sz_ts, sz_sender,
	    sz_exitfrom, sz_receiver;

	ASSERT(is_internal_pid(seq_tracer)
	       && internal_pid_index(seq_tracer) < erts_max_processes);

#ifndef ERTS_SMP

	tracer = process_tab[internal_pid_index(seq_tracer)];
	if (INVALID_PID(tracer, tracer->id)) {
	    system_seq_tracer = am_false;
	    return; /* no need to send anything */
	}
#endif
	if (receiver == seq_tracer) {
	    return; /* no need to send anything */
	}

	sz_label = size_object(SEQ_TRACE_T_LABEL(token));
	sz_sender = size_object(SEQ_TRACE_T_SENDER(token));
	sz_receiver = size_object(receiver);
	sz_lastcnt_serial = 3; /* TUPLE2 */
	sz_msg = size_object(msg);

	sz_ts = ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & SEQ_TRACE_TIMESTAMP) ? 
		 5 : 0);
	if (exitfrom != NIL) {
	    sz_exit = 4; /* create {'EXIT',exitfrom,msg} */
	    sz_exitfrom = size_object(exitfrom);
	}
	else {
	    sz_exit = 0;
	    sz_exitfrom = 0;
	}
	bp = new_message_buffer(4 /* TUPLE3 */ + sz_ts + 6 /* TUPLE5 */ 
				+ sz_lastcnt_serial + sz_label + sz_msg
				+ sz_exit + sz_exitfrom
				+ sz_sender + sz_receiver);
	hp = bp->mem;
	label = copy_struct(SEQ_TRACE_T_LABEL(token), sz_label, &hp, &bp->off_heap);
	lastcnt_serial = TUPLE2(hp,SEQ_TRACE_T_LASTCNT(token),SEQ_TRACE_T_SERIAL(token));
	hp += 3;
	m2 = copy_struct(msg, sz_msg, &hp, &bp->off_heap);
	if (sz_exit) {
	    Eterm exitfrom_copy = copy_struct(exitfrom,
					      sz_exitfrom,
					      &hp,
					      &bp->off_heap);
	    m2 = TUPLE3(hp, am_EXIT, exitfrom_copy, m2);
	    hp += 4;
	}
	sender_copy = copy_struct(SEQ_TRACE_T_SENDER(token),
				  sz_sender,
				  &hp,
				  &bp->off_heap);
	receiver_copy = copy_struct(receiver,
				    sz_receiver,
				    &hp,
				    &bp->off_heap);
	mess = TUPLE5(hp,
		      type_atom,
		      lastcnt_serial,
		      sender_copy,
		      receiver_copy,
		      m2);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (sz_ts) {/* timestamp should be included */
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	} else {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_SEQTRACE, NIL, NIL, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, bp, mess, NIL); /* trace_token must be NIL here */
#endif
    }
}

/* Send {trace_ts, Pid, return_to, {Mod, Func, Arity}, Timestamp}
 * or   {trace, Pid, return_to, {Mod, Func, Arity}}
 */
void 
erts_trace_return_to(Process *p, Uint *pc)
{
    Eterm* hp;
    Eterm mfa;
    Eterm mess;
    Eterm local_heap[4+5+5];

    Eterm *code_ptr = find_function_from_pc(pc);

    hp = local_heap;

    if (!code_ptr) {
	mfa = am_undefined;
    } else {
	mfa = TUPLE3(hp, code_ptr[0], code_ptr[1], make_small(code_ptr[2]));
	hp += 4;
    }
	
    mess = TUPLE4(hp, am_trace, p->id, am_return_to, mfa);
    hp += 5;

    erts_smp_mtx_lock(&smq_mtx);

    if (p->trace_flags & F_TIMESTAMP) {
	hp = patch_ts(mess, hp);
    }

    if (is_internal_port(p->tracer_proc)) {
	send_to_port(p, mess, &p->tracer_proc, &p->trace_flags);
    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	unsigned size;

	/*
	 * Find the tracer.
	 */
	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(p->tracer_proc)];

	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
	
#endif

	size = size_object(mess);

#ifdef ERTS_SMP
	bp = new_message_buffer(size);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, size);
#endif
	
	/*
	 * Copy the trace message into the buffer and enqueue it.
	 */
	mess = copy_struct(mess, size, &hp,
#ifdef ERTS_SMP
			   &bp->off_heap
#else
			   &MSO(tracer)
#endif
	    );
#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 p->id, p->tracer_proc, mess, bp);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
    erts_smp_mtx_unlock(&smq_mtx);
}


/* Send {trace_ts, Pid, return_from, {Mod, Name, Arity}, Retval, Timestamp}
 * or   {trace, Pid, return_from, {Mod, Name, Arity}, Retval}
 */
void
erts_trace_return(Process* p, Eterm* fi, Eterm retval, Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa;
    Eterm mess;
    Eterm mod, name;
    int arity;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &p->tracer_proc;
    }
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->id) {
	/* Do not generate trace messages to oneself */
	return;
    }
    if (tracer_pid == &p->tracer_proc) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &p->trace_flags;
#ifdef ERTS_SMP
	tracee = p->id;
#endif
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }
    if (! (*tracee_flags & F_TRACE_CALLS)) {
	return;
    }
    
    mod = fi[0];
    name = fi[1];
    arity = fi[2];
    
    if (is_internal_port(*tracer_pid)) {
	Eterm local_heap[4+6+5];
	hp = local_heap;
	mfa = TUPLE3(hp, mod, name, make_small(arity));
	hp += 4;
	mess = TUPLE5(hp, am_trace, p->id, am_return_from, mfa, retval);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	unsigned size;
	unsigned retval_size;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid)
	       && internal_pid_index(*tracer_pid) < erts_max_processes);
#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(*tracer_pid)];
	if (INVALID_PID(tracer, *tracer_pid)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
	    return;
	}
#endif
	
	retval_size = size_object(retval);
	size = 6 + 4 + retval_size;
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1+4;
	}

#ifdef ERTS_SMP
	bp = new_message_buffer(size);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, size);
#endif
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the trace tuple and put it into receive queue of the tracer process.
	 */
	
	mfa = TUPLE3(hp, mod, name, make_small(arity));
	hp += 4;
	retval = copy_struct(retval, retval_size, &hp,
#ifdef ERTS_SMP
			     &bp->off_heap
#else
			     &MSO(tracer)
#endif
	    );
	mess = TUPLE5(hp, am_trace, p->id/* Local pid */, am_return_from, mfa, retval);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 tracee, *tracer_pid, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
}

/* Send {trace_ts, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 * or   {trace, Pid, exception_from, {Mod, Name, Arity}, {Class,Value}, 
 *       Timestamp}
 *
 * Where Class is atomic but Value is any term.
 */
void
erts_trace_exception(Process* p, Eterm mfa[3], Eterm class, Eterm value, 
		     Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa_tuple;
    Eterm cv;
    Eterm mess;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &p->tracer_proc;
    }
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->id) {
	/* Do not generate trace messages to oneself */
	return;
    }
    if (tracer_pid == &p->tracer_proc) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &p->trace_flags;
#ifdef ERTS_SMP
	tracee = p->id;
#endif
	if (! (*tracee_flags & F_TRACE_CALLS)) {
	    return;
	}
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }
    
    if (is_internal_port(*tracer_pid)) {
	Eterm local_heap[4+3+6+5];
	hp = local_heap;
	mfa_tuple = TUPLE3(hp, mfa[0], mfa[1], make_small(mfa[2]));
	hp += 4;
	cv = TUPLE2(hp, class, value);
	hp += 3;
	mess = TUPLE5(hp, am_trace, p->id, am_exception_from, mfa_tuple, cv);
	hp += 6;
	ASSERT((hp - local_heap)*sizeof(*hp) <= sizeof(local_heap));
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp); /* hp += 5 */
	    ASSERT((hp - local_heap)*sizeof(*hp) == sizeof(local_heap));
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	unsigned size;
	unsigned value_size;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid)
	       && internal_pid_index(*tracer_pid) < erts_max_processes);
#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(*tracer_pid)];
	if (INVALID_PID(tracer, *tracer_pid)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
	    return;
	}
#endif
	
	value_size = size_object(value);
	size = 6 + 4 + 3 + value_size;
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1+4;
	}

#ifdef ERTS_SMP
	bp = new_message_buffer(size);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, size);
#endif
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the trace tuple and put it into receive queue of the tracer process.
	 */
	
	mfa_tuple = TUPLE3(hp, mfa[0], mfa[1], make_small(mfa[2]));
	hp += 4;
	value = copy_struct(value, value_size, &hp,
#ifdef ERTS_SMP
			     &bp->off_heap
#else
			     &MSO(tracer)
#endif
	    );
	cv = TUPLE2(hp, class, value);
	hp += 3;
	mess = TUPLE5(hp, am_trace, p->id/* Local pid */, 
		      am_exception_from, mfa_tuple, cv);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 tracee, *tracer_pid, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
}

/*
 * This function implements the new call trace.
 *
 * Send {trace_ts, Pid, call, {Mod, Func, A}, PamResult, Timestamp}
 * or   {trace_ts, Pid, call, {Mod, Func, A}, Timestamp}
 * or   {trace, Pid, call, {Mod, Func, A}, PamResult}
 * or   {trace, Pid, call, {Mod, Func, A}
 *
 * where 'A' is arity or argument list depending on trace flag 'arity'.
 *
 * If *tracer_pid is am_true, it is a breakpoint trace that shall use
 * the process tracer, if it is NIL no trace message is generated, 
 * if it is a pid or port we do a meta trace.
 */
Uint32
erts_call_trace(Process* p, Eterm mfa[3], Binary *match_spec, 
		Eterm* args, int local, Eterm *tracer_pid)
{
    Eterm* hp;
    Eterm mfa_tuple;
    int arity;
    int i;
    Uint32 return_flags;
    Eterm pam_result = am_true;
    Eterm mess;
    Uint meta_flags, *tracee_flags;
#ifdef ERTS_SMP
    Eterm tracee;
#endif
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &p->tracer_proc;
    } 
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return 0;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->id) {
	/* Do not generate trace messages to oneself */
	return 0;
    }
    if (tracer_pid == &p->tracer_proc) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &p->trace_flags;
#ifdef ERTS_SMP
	tracee = p->id;
#endif
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
#ifdef ERTS_SMP
	tracee = NIL;
#endif
    }

    if (is_internal_port(*tracer_pid)) {
	Eterm local_heap[64+MAX_ARG];
	hp = local_heap;

	if (!erts_is_valid_tracer_port(*tracer_pid)) {
#ifdef ERTS_SMP
	    ASSERT(is_nil(tracee) || tracer_pid == &p->tracer_proc);
	    if (is_not_nil(tracee))
		erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
#ifdef ERTS_SMP
	    if (is_not_nil(tracee))
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    return 0;
	}
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 *
	 * Some precedence rules:
	 * 
	 * - No proc flags, e.g 'silent' or 'return_to' 
	 *   has any effect on meta trace.
	 * - The 'silent' process trace flag silences all call 
	 *   related messages, e.g 'call', 'return_to' and 'return_from'.
	 * - The {message,_} PAM function does not affect {return_trace}.
	 * - The {message,false} PAM function shall give the same
	 *   'call' trace message as no PAM match.
	 * - The {message,true} PAM function shall give the same
	 *   'call' trace message as a nonexistent PAM program.
	 */
	
	/* BEGIN this code should be the same for port and pid trace */
	return_flags = 0;
	arity = mfa[2];
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    &return_flags);
	    if (is_non_value(pam_result)) {
		erts_match_set_release_result(p);
		return 0;
	    }
	}
	if (tracee_flags == &meta_flags) {
	    /* Meta trace */
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		return return_flags;
	    }
	} else {
	    /* Non-meta trace */
	    if (*tracee_flags & F_TRACE_SILENT) { 
		erts_match_set_release_result(p);
		return 0;
	    }
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		return return_flags;
	    }
	    if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
		return_flags |= MATCH_SET_RETURN_TO_TRACE;
	    }
	}
	/* END this code should be the same for port and pid trace */
	
	/*
	 * Build the the {M,F,A} tuple in the local heap. 
	 * (A is arguments or arity.)
	 */
	
	if (*tracee_flags & F_TRACE_ARITY_ONLY) {
	    mfa_tuple = make_small(arity);
	} else {
	    mfa_tuple = NIL;
	    for (i = arity-1; i >= 0; i--) {
		mfa_tuple = CONS(hp, args[i], mfa_tuple);
		hp += 2;
	    }
	}
	mfa_tuple = TUPLE3(hp, mfa[0], mfa[1], mfa_tuple);
	hp += 4;
	
	/*
	 * Build the trace tuple and send it to the port.
	 */
	
	mess = TUPLE4(hp, am_trace, p->id, am_call, mfa_tuple);
	hp += 5;
	if (pam_result != am_true) {
	    hp[-5] = make_arityval(5);
	    *hp++ = pam_result;
	}
	erts_smp_mtx_lock(&smq_mtx);
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	erts_smp_mtx_unlock(&smq_mtx);
	erts_match_set_release_result(p);
	return *tracer_pid == NIL ? 0 : return_flags;

    } else {
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
	Eterm tpid;
#endif
	Process *tracer;
	unsigned size;
	unsigned sizes[256];
	unsigned pam_result_size = 0;
	int invalid_tracer;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid)
	       && internal_pid_index(*tracer_pid) < erts_max_processes);
	
	tracer = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
			       *tracer_pid, ERTS_PROC_LOCK_STATUS);
	if (!tracer)
	    invalid_tracer = 1;
	else {
	    invalid_tracer = (tracer->trace_flags & F_TRACER) == 0;
	    erts_smp_proc_unlock(tracer, ERTS_PROC_LOCK_STATUS);
	}

	if (invalid_tracer) {
#ifdef ERTS_SMP
	    ASSERT(is_nil(tracee) || tracer_pid == &p->tracer_proc);
	    if (is_not_nil(tracee))
		erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    *tracee_flags &= ~TRACEE_FLAGS;
	    *tracer_pid = NIL;
#ifdef ERTS_SMP
	    if (is_not_nil(tracee))
		erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif
	    return 0;
	}

#ifdef ERTS_SMP
	tpid = *tracer_pid; /* Need to save tracer pid,
			       since *tracer_pid might
			       be reset by erts_match_set_run() */
#endif
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 *
	 * See the rules above in the port trace code.
	 */
	
	/* BEGIN this code should be the same for port and pid trace */
	return_flags = 0;
	arity = mfa[2];
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    &return_flags);
	    if (is_non_value(pam_result)) {
		erts_match_set_release_result(p);
		return 0;
	    }
	}
	if (tracee_flags == &meta_flags) {
	    /* Meta trace */
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		return return_flags;
	    }
	} else {
	    /* Non-meta trace */
	    if (*tracee_flags & F_TRACE_SILENT) { 
		erts_match_set_release_result(p);
		return 0;
	    }
	    if (pam_result == am_false) {
		erts_match_set_release_result(p);
		return return_flags;
	    }
	    if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
		return_flags |= MATCH_SET_RETURN_TO_TRACE;
	    }
	}
	/* END this code should be the same for port and pid trace */
	
	/*
	 * Calculate number of words needed on heap.
	 */
	
	size = 4 + 5;		/* Trace tuple + MFA tuple. */
	if (! (*tracee_flags & F_TRACE_ARITY_ONLY)) {
	    size += 2*arity;
	    for (i = arity-1; i >= 0; i--) {
		sizes[i] = size_object(args[i]);
		size += sizes[i];
	    }
	}
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1 + 4;
	    /* One element in trace tuple + timestamp tuple. */
	}
	if (pam_result != am_true) {
	    pam_result_size = size_object(pam_result);
	    size += 1 + pam_result_size;
	    /* One element in trace tuple + term size. */
	}
	
#ifdef ERTS_SMP
	bp = new_message_buffer(size);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, size);
#endif
#ifdef DEBUG
	limit = hp + size;
#endif

	/*
	 * Build the the {M,F,A} tuple in the message buffer. 
	 * (A is arguments or arity.)
	 */
	
	if (*tracee_flags & F_TRACE_ARITY_ONLY) {
	    mfa_tuple = make_small(arity);
	} else {
	    mfa_tuple = NIL;
	    for (i = arity-1; i >= 0; i--) {
		Eterm term = copy_struct(args[i], sizes[i], &hp,
#ifdef ERTS_SMP
					 &bp->off_heap
#else
					 &MSO(tracer)
#endif
);
		mfa_tuple = CONS(hp, term, mfa_tuple);
		hp += 2;
	    }
	}
	mfa_tuple = TUPLE3(hp, mfa[0], mfa[1], mfa_tuple);
	hp += 4;
	
	/*
	 * Copy the PAM result (if any) onto the heap.
	 */
	
	if (pam_result != am_true) {
	    pam_result = copy_struct(pam_result, pam_result_size, &hp,
#ifdef ERTS_SMP
				     &bp->off_heap
#else
				     &MSO(tracer)
#endif
		);
	}

	erts_match_set_release_result(p);

	/*
	 * Build the trace tuple and enqueue it.
	 */
	
	mess = TUPLE4(hp, am_trace, p->id/* Local pid */, am_call, mfa_tuple);
	hp += 5;
	if (pam_result != am_true) {
	    hp[-5] = make_arityval(5);
	    *hp++ = pam_result;
	}

	erts_smp_mtx_lock(&smq_mtx);

	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

	ASSERT(hp == limit);
#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE, tracee, tpid, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
	return return_flags;
    }
}

/* Sends trace message:
 *    {trace_ts, ProcessPid, What, Data, Timestamp}
 * or {trace, ProcessPid, What, Data}
 *
 * 'what' must be atomic, 'data' may be a deep term.
 * 'c_p' is the currently executing process, may be NULL.
 * 't_p' is the traced process.
 */
void
trace_proc(Process *c_p, Process *t_p, Eterm what, Eterm data)
{
    Eterm mess;
    Eterm* hp;
    int need;

    if (is_internal_port(t_p->tracer_proc)) {
	Eterm local_heap[5+5];
	hp = local_heap;
	mess = TUPLE4(hp, am_trace, t_p->id, what, data);
	hp += 5;
	erts_smp_mtx_lock(&smq_mtx);
	if (t_p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(
#ifndef ERTS_SMP
	    /* No fake schedule out and in again after an exit */
	    what == am_exit ? NULL : c_p,
#else
	    /* Fake schedule out and in are never sent when smp enabled */
	    c_p,
#endif
	    mess, &t_p->tracer_proc, &t_p->trace_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Eterm tmp;
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	size_t sz_data;

	ASSERT(is_internal_pid(t_p->tracer_proc)
	       && internal_pid_index(t_p->tracer_proc) < erts_max_processes);

#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(t_p->tracer_proc)];
	if (INVALID_PID(tracer, t_p->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    t_p->trace_flags &= ~TRACEE_FLAGS;
	    t_p->tracer_proc = NIL;
	    return;
	}
#endif

	sz_data = size_object(data);

	need = sz_data + 5 + TS_SIZE(t_p);

#ifdef ERTS_SMP
	bp = new_message_buffer(need);
	hp = bp->mem;
#else
	hp = HAlloc(tracer, need);
#endif

	tmp = copy_struct(data, sz_data, &hp,
#ifdef ERTS_SMP
			  &bp->off_heap
#else
			  &MSO(tracer)
#endif
	    );
	mess = TUPLE4(hp, am_trace, t_p->id/* Local pid */, what, tmp);
	hp += 5;

	erts_smp_mtx_lock(&smq_mtx);

	if (t_p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 t_p->id, t_p->tracer_proc, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
}


/* Sends trace message:
 *    {trace_ts, ParentPid, spawn, ChildPid, {Mod, Func, Args}, Timestamp}
 * or {trace, ParentPid, spawn, ChildPid, {Mod, Func, Args}}
 *
 * 'pid' is the ChildPid, 'mod' and 'func' must be atomic,
 * and 'args' may be a deep term.
 */
void
trace_proc_spawn(Process *p, Eterm pid, 
		 Eterm mod, Eterm func, Eterm args)
{
    Eterm mfa;
    Eterm mess;
    Eterm* hp;

    if (is_internal_port(p->tracer_proc)) {
	Eterm local_heap[4+6+5];
	hp = local_heap;
	mfa = TUPLE3(hp, mod, func, args);
	hp += 4;
	mess = TUPLE5(hp, am_trace, p->id, am_spawn, pid, mfa);
	hp += 6;
	erts_smp_mtx_lock(&smq_mtx);
	if (p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &p->tracer_proc, &p->trace_flags);
	erts_smp_mtx_unlock(&smq_mtx);
    } else {
	Eterm tmp;
#ifdef ERTS_SMP
	ErlHeapFragment *bp;
#else
	Process *tracer;
#endif
	size_t sz_args, sz_pid;

	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

#ifndef ERTS_SMP
	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
#endif

	sz_args = size_object(args);
	sz_pid = size_object(pid);

#ifdef ERTS_SMP
	bp = new_message_buffer(sz_args + 4 + 6 + TS_SIZE(p));
	hp = bp->mem;
#else
	hp = HAlloc(tracer, sz_args + 4 + 6 + TS_SIZE(p));
#endif

	tmp = copy_struct(args, sz_args, &hp,
#ifdef ERTS_SMP
			  &bp->off_heap
#else
			  &MSO(tracer)
#endif
	    );
	mfa = TUPLE3(hp, mod, func, tmp);
	hp += 4;
	tmp = copy_struct(pid, sz_pid, &hp,
#ifdef ERTS_SMP
			  &bp->off_heap
#else
			  &MSO(tracer)
#endif
	    );
	mess = TUPLE5(hp, am_trace, p->id, am_spawn, tmp, mfa);
	hp += 6;

	erts_smp_mtx_lock(&smq_mtx);

	if (p->trace_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}

#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 p->id, p->tracer_proc, mess, bp);
	erts_smp_mtx_unlock(&smq_mtx);
#else
	erts_queue_message(tracer, 0, NULL, mess, NIL);
#endif
    }
}

void save_calls(Process *p, Export *e)
{
   if (p->ct) {
     Export **ct = &p->ct->ct[0];
     int len = p->ct->len;

     ct[p->ct->cur] = e;
     if (++p->ct->cur >= len) {
       p->ct->cur = 0;
     }
     if (p->ct->n < len) {
       p->ct->n++;
     }
   }
}

/* 
 * Entry point called by the trace wrap functions in erl_bif_wrap.c
 *
 * The trace wrap functions are themselves called through the export
 * entries instead of the original BIF functions.
 */
Eterm
erts_bif_trace(int bif_index, Process* p, 
	       Eterm arg1, Eterm arg2, Eterm arg3, Uint *I)
{
    Eterm result;
    int meta = !!(erts_bif_trace_flags[bif_index] & BIF_TRACE_AS_META);

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p->id);

    if ((p->trace_flags & F_TRACE_CALLS) == 0 && (! meta)) {
	/* Warning! This is an Optimization. 
	 *
	 * If neither meta trace is active nor process trace flags then 
	 * no tracing will occur. Doing the whole else branch will 
	 * also do nothing, only slower.
	 */
	Eterm (*func)(Process*, Eterm, Eterm, Eterm, Uint*) = bif_table[bif_index].f;
	result = func(p, arg1, arg2, arg3, I);
    } else {
	Eterm (*func)(Process*, Eterm, Eterm, Eterm, Uint*);
	Export* ep = bif_export[bif_index];
	Uint32 flags = 0, flags_meta = 0;
	int global = !!(erts_bif_trace_flags[bif_index] & BIF_TRACE_AS_GLOBAL);
	int local  = !!(erts_bif_trace_flags[bif_index] & BIF_TRACE_AS_LOCAL);
	Eterm meta_tracer_pid = NIL;
	int applying = (I == &(ep->code[3])); /* Yup, the apply code for a bif
					       * is actually in the 
					       * export entry */
	Eterm *cp = p->cp;
	
#ifndef _OSE_
	Eterm args[3] = {arg1, arg2, arg3};
#else
	Eterm args[3];
	args[0] = arg1;
	args[1] = arg2;
	args[2] = arg3;
#endif
	
	/* 
	 * Make continuation pointer OK, it is not during direct BIF calls,
	 * but it is correct during apply of bif.
	 */
	if (!applying) { 
	    p->cp = I;
	}
	if (global || local) {
	    flags = erts_call_trace(p, ep->code, ep->match_prog_set, args, 
				       local, &p->tracer_proc);
	}
	if (meta) {
	    flags_meta = erts_bif_mtrace(p, ep->code+3, args, local, 
					 &meta_tracer_pid);
	}
	/* Restore original continuation pointer (if changed). */
	p->cp = cp;
	
	func = bif_table[bif_index].f;

	result = func(p, arg1, arg2, arg3, I);
	
	if (applying && (flags & MATCH_SET_RETURN_TO_TRACE)) {
	    Uint i_return_trace = beam_return_trace[0];
	    Uint i_return_to_trace = beam_return_to_trace[0];
	    Eterm *cpp;
	    /* Maybe advance cp to skip trace stack frames */
	    for (cpp = p->stop;  ;  cp = cp_val(*cpp++)) {
		ASSERT(is_CP((Eterm) cp));
		if (*cp_val((Eterm) cp) == i_return_trace) {
		    /* Skip stack frame variables */
		    while (is_not_CP(*cpp)) cpp++;
		    cpp += 2; /* Skip return_trace parameters */
		} else if (*cp_val((Eterm) cp) == i_return_to_trace) {
		    /* A return_to trace message is going to be generated
		     * by normal means, so we do not have to.
		     */
		    cp = NULL;
		    break;
		} else break;
	    }
	}
	
	/* Try to get these in the order 
	 * they usually appear in normal code... */
	if (is_non_value(result)) {
	    Uint reason = p->freason;
	    if (reason != TRAP && reason != RESCHEDULE) {
		Eterm class;
		Eterm value = p->fvalue;
		Eterm nocatch[3];
		/* Expand error value like in handle_error() */
		if (reason & EXF_ARGLIST) {
		    Eterm *tp;
		    ASSERT(is_tuple(value));
		    tp = tuple_val(value);
		    value = tp[1];
		}
		if ((reason & EXF_THROWN) && (p->catches <= 0)) {
		    value = TUPLE2(nocatch, am_nocatch, value);
		    reason = EXC_ERROR;
		}
		/* Note: expand_error_value() could theoretically 
		 * allocate on the heap, but not for any error
		 * returned by a BIF, and it would do no harm,
		 * just be annoying.
		 */
		value = expand_error_value(p, reason, value);
		class = exception_tag[GET_EXC_CLASS(reason)];
		
		if (flags_meta & MATCH_SET_EXCEPTION_TRACE) {
		    erts_trace_exception(p, ep->code, class, value,
					 &meta_tracer_pid);
		}
		if (flags & MATCH_SET_EXCEPTION_TRACE) {
		    erts_trace_exception(p, ep->code, class, value,
					 &p->tracer_proc);
		}
		if ((flags & MATCH_SET_RETURN_TO_TRACE) && p->catches > 0) {
		    /* can only happen if(local)*/
		    Eterm *ptr = p->stop;
		    ASSERT(is_CP(*ptr));
		    ASSERT(ptr <= STACK_START(p));
		    /* Search the nearest stack frame for a catch */
		    while (++ptr < STACK_START(p)) {
			if (is_CP(*ptr)) break;
			if (is_catch(*ptr)) {
			    if (applying) {
				/* Apply of BIF, cp is in calling function */
				if (cp) erts_trace_return_to(p, cp);
			    } else {
				/* Direct bif call, I points into 
				 * calling function */
				erts_trace_return_to(p, I);
			    }
			}
		    }
		}
		if ((flags_meta|flags) & MATCH_SET_EXCEPTION_TRACE) {
		    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
		    p->trace_flags |= F_EXCEPTION_TRACE;
		    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
		}
	    }
	} else {
	    if (flags_meta & MATCH_SET_RX_TRACE) {
		erts_trace_return(p, ep->code, result, &meta_tracer_pid);
	    }
	    /* MATCH_SET_RETURN_TO_TRACE cannot occur if(meta) */
	    if (flags & MATCH_SET_RX_TRACE) {
		erts_trace_return(p, ep->code, result, &p->tracer_proc);
	    }
	    if (flags & MATCH_SET_RETURN_TO_TRACE) { 
		/* can only happen if(local)*/
		if (applying) {
		    /* Apply of BIF, cp is in calling function */
		    if (cp) erts_trace_return_to(p, cp);
		} else {
		    /* Direct bif call, I points into calling function */
		    erts_trace_return_to(p, I);
		}
	    }
	}
    }
    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p->id);
    return result;
}

/* Sends trace message:
 *    {trace_ts, Pid, What, Msg, Timestamp}
 * or {trace, Pid, What, Msg}
 *
 * where 'What' must be atomic and 'Msg' is: 
 * [{heap_size, HeapSize}, {old_heap_size, OldHeapSize}, 
 *  {stack_size, StackSize}, {recent_size, RecentSize}, 
 *  {mbuf_size, MbufSize}]
 *
 * where 'HeapSize', 'OldHeapSize', 'StackSize', 'RecentSize and 'MbufSize'
 * are all small (atomic) integers.
 */
void
trace_gc(Process *p, Eterm what)
{
#define HEAP_WORDS_NEEDED 40
#ifdef ERTS_SMP
    ErlHeapFragment *bp = NULL;
#else
    Process* tracer = NULL;	/* Initialized to eliminate compiler warning */
#endif
    Eterm* hp;
    Eterm msg = NIL;
    Eterm tuple;
    Uint size = HEAP_WORDS_NEEDED + TS_SIZE(p);
#ifdef DEBUG
    Eterm* limit;
#endif


#define CONS_PAIR(key, val) \
    tuple = TUPLE2(hp, key, val); hp += 3; \
    msg = CONS(hp, tuple, msg); hp += 2

    if (is_internal_port(p->tracer_proc)) {
	Eterm local_heap[HEAP_WORDS_NEEDED+5];
	ASSERT(size <= sizeof(local_heap) / sizeof(local_heap[0]));
	hp = local_heap;
    } else {
	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

#ifdef ERTS_SMP
	bp = new_message_buffer(size);
	hp = bp->mem;
#else
	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->trace_flags & F_TRACER) == 0) {
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
	hp = HAlloc(tracer, size);
#endif
    }
#undef HEAP_WORDS_NEEDED
#ifdef DEBUG
    limit = hp + size;
#endif

    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
    CONS_PAIR(am_old_heap_size, make_small(OLD_HTOP(p) - OLD_HEAP(p)));
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_recent_size, make_small(HIGH_WATER(p) - HEAP_START(p)));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
    CONS_PAIR(am_old_heap_block_size, make_small(OLD_HEAP(p)
						 ? OLD_HEND(p) - OLD_HEAP(p)
						 : 0));

    msg = TUPLE4(hp, am_trace, p->id/* Local pid */, what, msg);
    hp += 5;

    erts_smp_mtx_lock(&smq_mtx);

    if (p->trace_flags & F_TIMESTAMP) {
	hp = patch_ts(msg, hp);
    }
    ASSERT(hp == limit);
    if (is_internal_port(p->tracer_proc)) {
	send_to_port(p, msg, &p->tracer_proc, &p->trace_flags);
    } else {
#ifdef ERTS_SMP
	enqueue_sys_msg_unlocked(SYS_MSG_TYPE_TRACE,
				 p->id, p->tracer_proc, msg, bp);
#else
	erts_queue_message(tracer, 0, NULL, msg, NIL);
#endif
    }
    erts_smp_mtx_unlock(&smq_mtx);
#undef CONS_PAIR
}



void
monitor_long_gc(Process *p, Uint time) {
#ifdef ERTS_SMP
    ErlHeapFragment *bp;
#else
    Process *monitor_p;
#endif
    Uint hsz;
    Eterm *hp, timeout, tuple, list, msg;

#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor)
	   && internal_pid_index(system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(system_monitor)];
    if (INVALID_PID(monitor_p, system_monitor) || p == monitor_p) {
	return;
    }
#endif

#define CONS_PAIR(key, val) \
    tuple = TUPLE2(hp, (key), (val)); hp += 3; \
    list = CONS(hp, tuple, list); hp += 2

    hsz = 30;
    if (!IS_USMALL(0, time)) {
	hsz += BIG_UINT_HEAP_SIZE;
    }
#ifdef ERTS_SMP
    bp = new_message_buffer(hsz);
    hp = bp->mem;
#else
    hp = HAlloc(monitor_p, hsz);
#endif
    if (IS_USMALL(0, time)) {
	timeout = make_small(time);
    } else {
	timeout = uint_to_big(time, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }

    list = NIL;
    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
    CONS_PAIR(am_timeout, timeout);
    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, am_long_gc, list); 
    hp += 5;

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, 0, NULL, msg, NIL);
#endif
#undef CONS_PAIR
}

void
monitor_large_heap(Process *p) {
#ifdef ERTS_SMP
    ErlHeapFragment *bp;
#else
    Process *monitor_p;
#endif
    Eterm *hp, tuple, list, msg;
   
#ifndef ERTS_SMP 
    ASSERT(is_internal_pid(system_monitor)
	   && internal_pid_index(system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(system_monitor)];
    if (INVALID_PID(monitor_p, system_monitor) || p == monitor_p) {
	return;
    }
#endif

#define CONS_PAIR(key, val) \
    tuple = TUPLE2(hp, (key), (val)); hp += 3; \
    list = CONS(hp, tuple, list); hp += 2


#ifdef ERTS_SMP
    bp = new_message_buffer(25);
    hp = bp->mem;
#else
    /* XXX Multi-thread note: Perhaps allocating on another process's heap. */
    hp = HAlloc(monitor_p, 25);
#endif

    list = NIL;
    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, am_large_heap, list); 
    hp += 5;

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, 0, NULL, msg, NIL);
#endif
#undef CONS_PAIR
}

void
monitor_generic(Process *p, Eterm type, Eterm spec) {
#ifdef ERTS_SMP
    ErlHeapFragment *bp;
#else
    Process *monitor_p;
#endif
    Eterm *hp, msg;

#ifndef ERTS_SMP
    ASSERT(is_internal_pid(system_monitor)
	   && internal_pid_index(system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(system_monitor)];
    if (INVALID_PID(monitor_p, system_monitor) || p == monitor_p) {
	return;
    }
#endif

#ifdef ERTS_SMP
    bp = new_message_buffer(5);
    hp = bp->mem;
#else
    hp = HAlloc(monitor_p, 5);
#endif

    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, type, spec); 
    hp += 5;

#ifdef ERTS_SMP
    enqueue_sys_msg(SYS_MSG_TYPE_SYSMON, p->id, NIL, msg, bp);
#else
    erts_queue_message(monitor_p, 0, NULL, msg, NIL);
#endif

}


#ifdef ERTS_SMP

void
erts_check_my_tracer_proc(Process *p)
{
    if (is_internal_pid(p->tracer_proc)) {
	Process *tracer = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
					p->tracer_proc, ERTS_PROC_LOCK_STATUS);
	int invalid_tracer = !tracer || !(tracer->trace_flags & F_TRACER);
	if (tracer)
	    erts_smp_proc_unlock(tracer, ERTS_PROC_LOCK_STATUS);
	if (invalid_tracer) {
	    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	    p->trace_flags &= ~TRACEE_FLAGS;
	    p->tracer_proc = NIL;
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }
}


typedef struct ErtsSysMsgQ_ ErtsSysMsgQ;
struct  ErtsSysMsgQ_ {
    ErtsSysMsgQ *next;
    enum ErtsSysMsgType type;
    Eterm from;
    Eterm to;
    Eterm msg;
    ErlHeapFragment *bp;
};

static ErtsSysMsgQ *sys_message_queue;
static ErtsSysMsgQ *sys_message_queue_end;

static erts_tid_t sys_msg_dispatcher_tid;
static erts_cnd_t smq_cnd;

static int dispatcher_waiting;

ERTS_QUALLOC_IMPL(smq_element, ErtsSysMsgQ, 20, ERTS_ALC_T_SYS_MSG_Q)

static void
enqueue_sys_msg_unlocked(enum ErtsSysMsgType type,
			 Eterm from,
			 Eterm to,
			 Eterm msg,
			 ErlHeapFragment *bp)
{
    ErtsSysMsgQ *smqp;

    smqp	= smq_element_alloc();
    smqp->next	= NULL;
    smqp->type	= type;
    smqp->from	= from;
    smqp->to	= to;
    smqp->msg	= msg;
    smqp->bp	= bp;
    
    if (sys_message_queue_end) {
	ASSERT(sys_message_queue);
	sys_message_queue_end->next = smqp;
    }
    else {
	ASSERT(!sys_message_queue);
	sys_message_queue = smqp;
    }
    sys_message_queue_end = smqp;
    erts_smp_cnd_signal(&smq_cnd);
}

static void
enqueue_sys_msg(enum ErtsSysMsgType type,
		Eterm from,
		Eterm to,
		Eterm msg,
		ErlHeapFragment *bp)
{
    erts_smp_mtx_lock(&smq_mtx);
    enqueue_sys_msg_unlocked(type, from, to, msg, bp);
    erts_smp_mtx_unlock(&smq_mtx);
}

static void
prepare_for_block(void *unused)
{
    erts_smp_mtx_unlock(&smq_mtx);
}

static void
resume_after_block(void *unused)
{
    erts_smp_mtx_lock(&smq_mtx);
}

void
erts_queue_error_logger_message(Eterm from, Eterm msg, ErlHeapFragment *bp)
{
    enqueue_sys_msg(SYS_MSG_TYPE_ERRLGR, from, am_error_logger, msg, bp);
}

void
erts_send_sys_msg_proc(Eterm from, Eterm to, Eterm msg, ErlHeapFragment *bp)
{
    ASSERT(is_internal_pid(to));
    enqueue_sys_msg(SYS_MSG_TYPE_PROC_MSG, from, to, msg, bp);
}

#ifdef DEBUG_PRINTOUTS
static void
print_msg_type(ErtsSysMsgQ *smqp)
{
    switch (smqp->type) {
    case SYS_MSG_TYPE_TRACE:
	erts_fprintf(stderr, "TRACE ");
	break;
    case SYS_MSG_TYPE_SEQTRACE:
	erts_fprintf(stderr, "SEQTRACE ");
	break;
    case SYS_MSG_TYPE_SYSMON:
	erts_fprintf(stderr, "SYSMON ");
	break;
    case SYS_MSG_TYPE_ERRLGR:
	erts_fprintf(stderr, "ERRLGR ");
	break;
    case SYS_MSG_TYPE_PROC_MSG:
	erts_fprintf(stderr, "PROC_MSG ");
	break;
    default:
	erts_fprintf(stderr, "??? ");
	break;
    }
}
#endif

static void
sys_msg_disp_failure(ErtsSysMsgQ *smqp, Eterm receiver)
{
    switch (smqp->type) {
    case SYS_MSG_TYPE_TRACE:
	/* Invalid tracer_proc's are removed when processes
	   are scheduled in. */
	break;
    case SYS_MSG_TYPE_SEQTRACE:
	/* Reset seq_tracer if it hasn't changed */
	erts_smp_mtx_lock(&sys_trace_mtx);
	if (system_seq_tracer == receiver)
	    system_seq_tracer = am_false;
	erts_smp_mtx_unlock(&sys_trace_mtx);
	break;
    case SYS_MSG_TYPE_SYSMON:
	if (receiver == NIL
	    && !erts_system_monitor_long_gc
	    && !erts_system_monitor_large_heap
	    && !erts_system_monitor_flags.busy_port
	    && !erts_system_monitor_flags.busy_dist_port)
	    break; /* Everything is disabled */
	erts_smp_block_system(ERTS_BS_FLG_ALLOW_GC);
	if (system_monitor == receiver || receiver == NIL)
	    erts_system_monitor_clear(NULL);
	erts_smp_release_system();
	break;
    case SYS_MSG_TYPE_ERRLGR: {
	char *no_elgger = "(no error logger present)";
	Eterm *tp;
	Eterm tag;
	if (is_not_tuple(smqp->msg)) {
	unexpected_elmsg:
	    erts_fprintf(stderr,
			 "%s unexpected error logger message: %T\n",
			 no_elgger,
			 smqp->msg);
	}

	tp = tuple_val(smqp->msg);
	if (arityval(tp[0]) != 2)
	    goto unexpected_elmsg;
	if (is_not_tuple(tp[2]))
	    goto unexpected_elmsg;
	tp = tuple_val(tp[2]);
	if (arityval(tp[0]) != 3)
	    goto unexpected_elmsg;
	tag = tp[1];
	if (is_not_tuple(tp[3]))
	    goto unexpected_elmsg;
	tp = tuple_val(tp[3]);
	if (arityval(tp[0]) != 3)
	    goto unexpected_elmsg;
	if (is_not_list(tp[3]))
	    goto unexpected_elmsg;
	erts_fprintf(stderr, "%s %T: %T\n",
		     no_elgger, tag, CAR(list_val(tp[3])));
	break;
    }
    case SYS_MSG_TYPE_PROC_MSG:
	break;
    default:
	ASSERT(0);
    }
}

static void *
sys_msg_dispatcher_func(void *unused)
{
    ErtsSysMsgQ *local_sys_message_queue = NULL;

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("system message dispatcher");
#endif

    erts_register_blockable_thread();
    erts_smp_activity_begin(ERTS_ACTIVITY_IO, NULL, NULL, NULL);

    while (1) {
	ErtsSysMsgQ *smqp;

	ERTS_SMP_LC_ASSERT(!ERTS_LC_IS_BLOCKING);

	erts_smp_mtx_lock(&smq_mtx);

	/* Free previously used queue ... */
	while (local_sys_message_queue) {
	    smqp = local_sys_message_queue;
	    local_sys_message_queue = smqp->next;
	    smq_element_free(smqp);
	}

	/* Fetch current trace message queue ... */
	erts_smp_activity_change(ERTS_ACTIVITY_IO,
				 ERTS_ACTIVITY_WAIT,
				 prepare_for_block,
				 resume_after_block,
				 NULL);
	dispatcher_waiting = 1;
	while (!sys_message_queue)
	    erts_smp_cnd_wait(&smq_cnd, &smq_mtx);
	dispatcher_waiting = 0;
	erts_smp_activity_change(ERTS_ACTIVITY_WAIT,
				 ERTS_ACTIVITY_IO,
				 prepare_for_block,
				 resume_after_block,
				 NULL);

	local_sys_message_queue = sys_message_queue;
	sys_message_queue = NULL;
	sys_message_queue_end = NULL;

	erts_smp_mtx_unlock(&smq_mtx);

	/* Send trace messages ... */

	ASSERT(local_sys_message_queue);

	for (smqp = local_sys_message_queue; smqp; smqp = smqp->next) {
	    Eterm receiver;
	    Uint32 proc_locks = ERTS_PROC_LOCKS_MSG_SEND;
	    Process *proc = NULL;
	    Port *port = NULL;

#ifdef DEBUG_PRINTOUTS
	    print_msg_type(smqp);
#endif
	    switch (smqp->type) {
	    case SYS_MSG_TYPE_TRACE:
	    case SYS_MSG_TYPE_PROC_MSG:
		receiver = smqp->to;
		break;
	    case SYS_MSG_TYPE_SEQTRACE:
		receiver = erts_get_system_seq_tracer();
		break;
	    case SYS_MSG_TYPE_SYSMON:
		receiver = erts_get_system_monitor();
		if (smqp->from == receiver) {
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "MSG=%T to %T... ",
				 smqp->msg, receiver);
#endif
		    goto drop_sys_msg;
		}
		break;
	    case SYS_MSG_TYPE_ERRLGR:
		receiver = am_error_logger;
		break;
	    default:
		receiver = NIL;
		break;
	    }

#ifdef DEBUG_PRINTOUTS
	    erts_fprintf(stderr, "MSG=%T to %T... ", smqp->msg, receiver);
#endif

	    if (is_internal_pid(receiver)) {
		proc = erts_pid2proc(NULL, 0, receiver, proc_locks);
		if (!proc
		    || (smqp->type == SYS_MSG_TYPE_TRACE
			&& !(proc->trace_flags & F_TRACER))) {
		    /* Bad tracer */
#ifdef DEBUG_PRINTOUTS
		    if (smqp->type == SYS_MSG_TYPE_TRACE && proc)
			erts_fprintf(stderr,
				     "<tracer alive but missing  "
				     "F_TRACER flag> ");
#endif
		    goto failure;
		}
		else {
		queue_proc_msg:
		    erts_queue_message(proc,proc_locks,smqp->bp,smqp->msg,NIL);
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "delivered\n");
#endif
		    erts_smp_proc_unlock(proc, proc_locks);
		}
	    }
	    else if (receiver == am_error_logger) {
		proc = erts_whereis_process(NULL,0,receiver,proc_locks,0);
		if (!proc)
		    goto failure;
		else if (smqp->from == proc->id)
		    goto drop_sys_msg;
		else
		    goto queue_proc_msg;
	    }
	    else if (is_internal_port(receiver)) {
		port = erts_id2port(receiver, NULL, 0);
		if (INVALID_TRACER_PORT(port, receiver)) {
		    if (port)
			erts_port_release(port);
		    goto failure;
		}
		else {
		    write_sys_msg_to_port(receiver,
					  port,
					  smqp->from,
					  smqp->type,
					  smqp->msg);
		    if (port->control_flags & PORT_CONTROL_FLAG_HEAVY)
			port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
#ifdef DEBUG_PRINTOUTS
		    erts_fprintf(stderr, "delivered\n");
#endif
		    erts_port_release(port);
		    if (smqp->bp)
			free_message_buffer(smqp->bp);
		}
	    }
	    else {
	    failure:
		sys_msg_disp_failure(smqp, receiver);
	    drop_sys_msg:
		if (proc)
		    erts_smp_proc_unlock(proc, proc_locks);
		if (smqp->bp)
		    free_message_buffer(smqp->bp);
#ifdef DEBUG_PRINTOUTS
		erts_fprintf(stderr, "dropped\n");
#endif
	    }
	}
    }

    erts_smp_activity_end(ERTS_ACTIVITY_IO, NULL, NULL, NULL);
    return NULL;
}

void
erts_foreach_sys_msg_in_q(void (*func)(Eterm,
				       Eterm,
				       Eterm,
				       ErlHeapFragment *))
{
    ErtsSysMsgQ *sm;
    erts_smp_mtx_lock(&smq_mtx);
    for (sm = sys_message_queue; sm; sm = sm->next) {
	Eterm to;
	switch (sm->type) {
	case SYS_MSG_TYPE_TRACE:
	    to = sm->to;
	    break;
	case SYS_MSG_TYPE_SEQTRACE:
	    to = erts_get_system_seq_tracer();
	    break;
	case SYS_MSG_TYPE_SYSMON:
	    to = erts_get_system_monitor();
	    break;
	case SYS_MSG_TYPE_ERRLGR:
	    to = am_error_logger;
	    break;
	default:
	    to = NIL;
	    break;
	}
	(*func)(sm->from, to, sm->msg, sm->bp);
    }
    erts_smp_mtx_unlock(&smq_mtx);
}


static void
init_sys_msg_dispatcher(void)
{
    init_smq_element_alloc();
    sys_message_queue = NULL;
    sys_message_queue_end = NULL;
    erts_smp_cnd_init(&smq_cnd);
    erts_smp_mtx_init(&smq_mtx, "sys_msg_q");
    dispatcher_waiting = 0;
    erts_smp_thr_create(&sys_msg_dispatcher_tid,
			sys_msg_dispatcher_func,
			NULL,
			1);
}

#endif
