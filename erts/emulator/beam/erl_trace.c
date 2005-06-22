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

/* Pseudo export entries. Never filled in with data, only used to
   yield unique pointers of the correct type. */
Export exp_send, exp_receive, exp_timeout;

#ifdef HAVE_ERTS_NOW_CPU
int erts_cpu_timestamp;
#endif

void erts_init_trace(void) {
#ifdef HAVE_ERTS_NOW_CPU
    erts_cpu_timestamp = 0;
#endif
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

static void
do_send_to_port(Port* trace_port, Eterm message) {
    byte *buffer;
    byte *ptr;
    unsigned size;

    buffer = tmp_buf;
    size = encode_size_struct(message, TERM_TO_BINARY_DFLAGS);
    if (size >= TMP_BUF_SIZE) {
	buffer = (byte *) erts_alloc(ERTS_ALC_T_TMP, size);
    }

    ptr = buffer;
    erts_to_external_format(NULL, message, &ptr);
    if (!(ptr <= buffer+size)) {
	erl_exit(1, "Internal error in do_send_to_port: %d\n", ptr-buffer);
    }

    dist_port_command(trace_port, buffer, ptr-buffer);

    if (buffer != tmp_buf) {
	erts_free(ERTS_ALC_T_TMP, (void *) buffer);
    }
}

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

    do_send_to_port(trace_port, message);

    message = TUPLE4(hp, am_trace_ts, pid, am_in, mfarity);
    hp += 5;
    hp = patch_ts(message, hp);

    do_send_to_port(trace_port, message);
}

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
    Eterm ts, local_heap[4], *hp;

    ASSERT(is_internal_port(*tracer_pid));
    if (is_not_internal_port(*tracer_pid))
	goto invalid_tracer_port;

    trace_port = &erts_port[internal_port_index(*tracer_pid)];

    if (INVALID_TRACER_PORT(trace_port, *tracer_pid)) {
    invalid_tracer_port:
	*tracee_flags &= ~TRACE_FLAGS;
	*tracer_pid = NIL;
	return;
    }

    if (c_p == NULL
	|| (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) {
	do_send_to_port(trace_port, message);
	return;
    }
    /* Make a fake schedule only if the current process is traced
     * with 'running' and 'timestamp'.
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

    do_send_to_port(trace_port, message);

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
	trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
	do_send_schedfix_to_port(trace_port, c_p->id, ts);
    }
}

/* A fake schedule out/in message pair will be sent,
 * if the driver so requests.
 * If (timestamp == NIL), one is fetched from GET_NOW().
 *
 * 'c_p' is the currently executing process, may be NULL.
 */
static void
seq_trace_send_to_port(Process *c_p, Eterm message, Eterm timestamp)
{
    Port* trace_port;
    Eterm ts, local_heap[4], *hp;

    ASSERT(is_internal_port(system_seq_tracer));
    if (is_not_internal_port(system_seq_tracer))
	goto invalid_tracer_port;

    trace_port = &erts_port[internal_port_index(system_seq_tracer)];

    if (INVALID_TRACER_PORT(trace_port, system_seq_tracer)) {
    invalid_tracer_port:
	system_seq_tracer = NIL;
	return;
    }

    if (c_p == NULL
	|| (! IS_TRACED_FL(c_p, F_TRACE_SCHED | F_TIMESTAMP))) {
	do_send_to_port(trace_port, message);
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

    do_send_to_port(trace_port, message);

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
	trace_port->control_flags &= ~PORT_CONTROL_FLAG_HEAVY;
	do_send_schedfix_to_port(trace_port, c_p->id, ts);
    }
}

/*
** Suspend a process 
** If we are to suspend on a port the busy_port is the thing
** otherwise busy_port is NIL
*/
void
erl_suspend(Process* process, Eterm busy_port)
{
    process->rcount++;  /* count number of suspend */
    switch(process->status) {
    case P_SUSPENDED:
	break;
    case P_RUNABLE:
	remove_proc_from_sched_q(process);
	process->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_RUNNING:
	process->rstatus = P_RUNABLE; /* wakeup as runnable */
	break;
    case P_WAITING:
	process->rstatus = P_WAITING; /* wakeup as waiting */
	break;
    case P_EXITING:
	return; /* ignore this */
    case P_GARBING:
    case P_FREE:
	erl_exit(1, "bad state in erl_suspend\n");
    }
    process->status = P_SUSPENDED;
    if (busy_port != NIL)
	wake_process_later(busy_port, process);
}


void erl_resume(process)
Process* process;
{
    /* We may get called from trace([suspend], false) */
    if (process->status != P_SUSPENDED)
	return;
    ASSERT(process->rcount > 0);

    if (--process->rcount > 0)  /* multiple suspend i.e trace and busy port */
	return;
    switch(process->rstatus) {
    case P_RUNABLE:
	process->status = P_WAITING;  /* make add_to_schedule_q work */
	add_to_schedule_q(process);
	break;
    case P_WAITING:
	process->status = P_WAITING;
	break;
    default:
	erl_exit(1, "bad state in erl_resume\n");
    }
    process->rstatus = P_FREE;
}


#define TS_SIZE(p) (((p)->flags & F_TIMESTAMP) ? 5 : 0)

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
    Process *tracer;
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
	if (p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	if (what != am_out) {
	    send_to_port(p, mess, &p->tracer_proc, &p->flags);
	} else {
	    send_to_port(NULL, mess, &p->tracer_proc, &p->flags);
	}
    } else {
	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);
	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc) 
	    || (tracer->flags & F_TRACER) == 0) {
	    p->flags &= ~TRACE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, 9 + TS_SIZE(p));
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

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
	if (p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	queue_message_tt(tracer, NULL, mess, NIL);
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
    Process *rp;
    Eterm operation;
    unsigned sz_msg;
    unsigned sz_to;
    Eterm* hp;
    Eterm mess;
    
    if (!(p->flags & F_TRACE_SEND)) {
	return;
    }

    operation = am_send;
    if (is_internal_pid(to)) {
	if(internal_pid_index(to) >= erts_max_processes)
	    goto send_to_non_existing_process;
	rp = process_tab[internal_pid_index(to)];
	if (INVALID_PID(rp, to))
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
	if (p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &p->tracer_proc, &p->flags);
    } else {
	Process *tracer;

	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

	tracer = process_tab[internal_pid_index(p->tracer_proc)];

	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    p->flags &= ~TRACE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
	sz_msg = size_object(msg);
	sz_to  = size_object(to);


#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, sz_msg + sz_to + 6 + TS_SIZE(p));
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

	hp = HAlloc(tracer, sz_msg + sz_to + 6 + TS_SIZE(p));
#endif
	to = copy_struct(to, sz_to, &hp, &MSO(tracer));
	msg = copy_struct(msg, sz_msg, &hp, &MSO(tracer));
	mess = TUPLE5(hp, am_trace, p->id/* Local pid */, operation, msg, to);
	hp += 6;
	if (p->flags & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}
	queue_message_tt(tracer, NULL, mess, NIL);
    }
}

/* Send {trace_ts, Pid, receive, Msg, Timestamp}
 * or   {trace, Pid, receive, Msg}
 */
void
trace_receive(Process *rp, Eterm msg)
{
    Process *tracer;
    Eterm mess;
    size_t sz_msg;
    Eterm* hp;

    if (is_internal_port(rp->tracer_proc)) {
	Eterm local_heap[10];
	hp = local_heap;
	mess = TUPLE4(hp, am_trace, rp->id, am_receive, msg);
	hp += 5;
	if (rp->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(rp, mess, &rp->tracer_proc, &rp->flags);
    } else {
	ASSERT(is_internal_pid(rp->tracer_proc)
	       && internal_pid_index(rp->tracer_proc) < erts_max_processes);

	tracer = process_tab[internal_pid_index(rp->tracer_proc)];
	if (INVALID_PID(tracer, rp->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    rp->flags &= ~TRACE_FLAGS;
	    rp->tracer_proc = NIL;
	    return;
	}
	sz_msg = size_object(msg);

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(rp, sz_msg + 5 + TS_SIZE(rp));
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

	hp = HAlloc(tracer, sz_msg + 5 + TS_SIZE(rp));
#endif
	msg = copy_struct(msg, sz_msg, &hp, &MSO(tracer));
	mess = TUPLE4(hp, am_trace, rp->id/* Local pid */, am_receive, msg);
	hp += 5;
	if (rp->flags & F_TIMESTAMP) {
	    patch_ts(mess, hp);
	}
	queue_message_tt(tracer, NULL, mess, NIL);
    }
}

int seq_trace_update_send(p)
Process *p;
{
    ASSERT((is_tuple(SEQ_TRACE_TOKEN(p)) || is_nil(SEQ_TRACE_TOKEN(p))));
    if ( (p->id == system_seq_tracer) || (SEQ_TRACE_TOKEN(p) == NIL))
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

    ASSERT(is_tuple(token) || is_nil(token));
    if ( (SEQ_TRACE_T_SENDER(token) == system_seq_tracer) || (token == NIL))
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

    if (is_nil(system_seq_tracer)) {
	return;			/* no need to send anything */
    }

    if (is_internal_port(system_seq_tracer)) {
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
	if ((unsigned_val(SEQ_TRACE_T_FLAGS(token)) & SEQ_TRACE_TIMESTAMP) == 0) {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	    seq_trace_send_to_port(NULL, mess, NIL);
	} else {
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	    seq_trace_send_to_port(process, mess, ts);
	}
    } else {
	Process* tracer;
	Eterm sender_copy;
	Eterm receiver_copy;
	Eterm m2;
	Uint sz_label, sz_lastcnt_serial, sz_msg, sz_ts, sz_sender,
	    sz_exitfrom, sz_receiver;

	ASSERT(is_internal_pid(system_seq_tracer)
	       && internal_pid_index(system_seq_tracer) < erts_max_processes);

	tracer = process_tab[internal_pid_index(system_seq_tracer)];
	if (INVALID_PID(tracer, tracer->id)) {
	    system_seq_tracer = NIL;
	    return; /* no need to send anything */
	}
	if (receiver == system_seq_tracer) {
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
	if (sz_ts) {/* timestamp should be included */
	    Uint ms,s,us,ts;
	    GET_NOW(&ms, &s, &us);
	    ts = TUPLE3(hp, make_small(ms),make_small(s), make_small(us));
	    hp += 4;
	    mess = TUPLE4(hp, am_seq_trace, label, mess, ts);
	} else {
	    mess = TUPLE3(hp, am_seq_trace, label, mess);
	}
	queue_message_tt(tracer, bp, mess, NIL); /* trace_token must be NIL here */
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

    if (p->flags & F_TIMESTAMP) {
	hp = patch_ts(mess, hp);
    }

    if (is_internal_port(p->tracer_proc)) {
	send_to_port(p, mess, &p->tracer_proc, &p->flags);
    } else {
	Process *tracer;
	unsigned size;

	/*
	 * Find the tracer.
	 */
	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

	tracer = process_tab[internal_pid_index(p->tracer_proc)];

	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    p->flags &= ~TRACE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
	
	size = size_object(mess);

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, size);
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */
	
	hp = HAlloc(tracer, size);
#endif
	
	/*
	 * Copy the trace message into the buffer and enqueue it.
	 */
	mess = copy_struct(mess, size, &hp, &MSO(tracer));
	queue_message_tt(tracer, NULL, mess, NIL);
    }
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
	tracee_flags = &p->flags;
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
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
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
    } else {
	Process *tracer;
	unsigned size;
	unsigned retval_size;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid)
	       && internal_pid_index(*tracer_pid) < erts_max_processes);
	tracer = process_tab[internal_pid_index(*tracer_pid)];
	if (INVALID_PID(tracer, *tracer_pid)
	    || (tracer->flags & F_TRACER) == 0) {
	    *tracee_flags &= ~TRACE_FLAGS;
	    *tracer_pid = NIL;
	    return;
	}
	
	retval_size = size_object(retval);
	size = 6 + 4 + retval_size;
	if (*tracee_flags & F_TIMESTAMP) {
	    size += 1+4;
	}

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, size);
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

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
	retval = copy_struct(retval, retval_size, &hp, &MSO(tracer));
	mess = TUPLE5(hp, am_trace, p->id/* Local pid */, am_return_from, mfa, retval);
	hp += 6;
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	ASSERT(hp == limit);
	queue_message_tt(tracer, NULL, mess, NIL);
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
    Uint32 return_flags = 0;
    Eterm pam_result = am_true;
    Eterm mess;
    Uint meta_flags, *tracee_flags;
    
    ASSERT(tracer_pid);
    if (*tracer_pid == am_true) {
	/* Breakpoint trace enabled without specifying tracer =>
	 *   use process tracer and flags
	 */
	tracer_pid = &p->tracer_proc;
    } 
    if (is_nil(*tracer_pid)) {
	/* Trace disabled */
	return return_flags;
    }
    ASSERT(is_internal_pid(*tracer_pid) || is_internal_port(*tracer_pid));
    if (*tracer_pid == p->id) {
	/* Do not generate trace messages to oneself */
	return return_flags;
    }
    if (tracer_pid == &p->tracer_proc) {
	/* Tracer specified in process structure =>
	 *   non-breakpoint trace =>
	 *     use process flags
	 */
	tracee_flags = &p->flags;
    } else {
	/* Tracer not specified in process structure =>
	 *   tracer specified in breakpoint =>
	 *     meta trace =>
	 *       use fixed flag set instead of process flags
	 */	    
	meta_flags = F_TRACE_CALLS | F_TIMESTAMP;
	tracee_flags = &meta_flags;
    }

    if (is_internal_port(*tracer_pid)) {
	Eterm local_heap[64+MAX_ARG];
	Port *tracer_port;
	hp = local_heap;

	ASSERT(internal_port_index(*tracer_pid) < erts_max_ports);
	
	tracer_port = &erts_port[internal_port_index(*tracer_pid)];
	if (INVALID_TRACER_PORT(tracer_port, *tracer_pid)) {
	    *tracee_flags &= ~TRACE_FLAGS;
	    *tracer_pid = NIL;
	    return return_flags;
	}
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 */
	
	arity = mfa[2];
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    &return_flags);
	    if (is_non_value(pam_result)) {
		return return_flags;
	    }
	}
	
	if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
	    return_flags |= MATCH_SET_RETURN_TO_TRACE;
	}
	
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
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, tracer_pid, tracee_flags);
	return *tracer_pid == NIL ? 0 : return_flags;

    } else {
	Process *tracer;
	unsigned size;
	unsigned sizes[256];
	unsigned pam_result_size = 0;
#ifdef DEBUG
	Eterm* limit;
#endif

	ASSERT(is_internal_pid(*tracer_pid)
	       && internal_pid_index(*tracer_pid) < erts_max_processes);
	
	tracer = process_tab[internal_pid_index(*tracer_pid)];
	if (INVALID_PID(tracer, *tracer_pid)
	    || (tracer->flags & F_TRACER) == 0) {
	    *tracee_flags &= ~TRACE_FLAGS;
	    *tracer_pid = NIL;
	    return return_flags;
	}
	
	/*
	 * If there is a PAM program, run it.  Return if it fails.
	 */
	
	arity = mfa[2];
	if (match_spec) {
	    pam_result = erts_match_set_run(p, match_spec, args, arity,
					    &return_flags);
	    if (is_non_value(pam_result)) {
		return return_flags;
	    }
	}
	
	if (local && (*tracee_flags & F_TRACE_RETURN_TO)) {
	    return_flags |= MATCH_SET_RETURN_TO_TRACE;
	}
	
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
	
#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, size);
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */
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
		Eterm term = copy_struct(args[i], sizes[i], &hp, &MSO(tracer));
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
	    pam_result = copy_struct(pam_result, pam_result_size, &hp, &MSO(tracer));
	}
	
	/*
	 * Build the trace tuple and enqueue it.
	 */
	
	mess = TUPLE4(hp, am_trace, p->id/* Local pid */, am_call, mfa_tuple);
	hp += 5;
	if (pam_result != am_true) {
	    hp[-5] = make_arityval(5);
	    *hp++ = pam_result;
	}
	if (*tracee_flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	ASSERT(hp == limit);
	queue_message_tt(tracer, NULL, mess, NIL);
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
	if (t_p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(c_p, mess, &t_p->tracer_proc, &t_p->flags);
    } else {
	Eterm tmp;
	Process *tracer;
	size_t sz_data;

	ASSERT(is_internal_pid(t_p->tracer_proc)
	       && internal_pid_index(t_p->tracer_proc) < erts_max_processes);

	tracer = process_tab[internal_pid_index(t_p->tracer_proc)];
	if (INVALID_PID(tracer, t_p->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    t_p->flags &= ~TRACE_FLAGS;
	    t_p->tracer_proc = NIL;
	    return;
	}
	sz_data = size_object(data);

	need = sz_data + 5 + TS_SIZE(t_p);
#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
        if (c_p != NULL) {
	    hp = HAlloc(c_p, need);
	} else {
	    hp = erts_global_alloc(need);
	}
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

	hp = HAlloc(tracer, need);
#endif
	tmp = copy_struct(data, sz_data, &hp, &MSO(tracer));
	mess = TUPLE4(hp, am_trace, t_p->id/* Local pid */, what, tmp);
	hp += 5;
	if (t_p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	queue_message_tt(tracer, NULL, mess, NIL);
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
	if (p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	send_to_port(p, mess, &p->tracer_proc, &p->flags);
    } else {
	Eterm tmp;
	Process *tracer;
	size_t sz_args, sz_pid;

	ASSERT(is_internal_pid(p->tracer_proc)
	       && internal_pid_index(p->tracer_proc) < erts_max_processes);

	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    p->flags &= ~TRACE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}
	sz_args = size_object(args);
	sz_pid = size_object(pid);

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, sz_args + 4 + 6 + TS_SIZE(p));
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */

	hp = HAlloc(tracer, sz_args + 4 + 6 + TS_SIZE(p));
#endif
	tmp = copy_struct(args, sz_args, &hp, &MSO(tracer));
	mfa = TUPLE3(hp, mod, func, tmp);
	hp += 4;
	tmp = copy_struct(pid, sz_pid, &hp, &MSO(tracer));
	mess = TUPLE5(hp, am_trace, p->id, am_spawn, tmp, mfa);
	hp += 6;
	if (p->flags & F_TIMESTAMP) {
	    hp = patch_ts(mess, hp);
	}
	queue_message_tt(tracer, NULL, mess, NIL);
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
    int meta = !!(erts_bif_trace_flags[bif_index] & BIF_TRACE_AS_META);
    
    if ((p->flags & F_TRACE_CALLS) == 0 && (! meta)) {
	/* Warning! This is an Optimization. 
	 *
	 * If neither meta trace is active nor process trace flags then 
	 * no tracing will occur. Doing the whole else branch will 
	 * also do nothing, only slower.
	 */
	Eterm (*func)(Process*, Eterm, Eterm, Eterm, Uint*) = bif_table[bif_index].f;
	return func(p, arg1, arg2, arg3, I);
    } else {
	Eterm result;
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
	
	/* Try to get these in the order 
	 * they usually appear in normal code... */
	if (is_non_value(result)) {
	    return result;
	}
	if ((flags_meta & MATCH_SET_RETURN_TRACE) && is_value(result)) {
	    erts_trace_return(p, ep->code, result, &meta_tracer_pid);
	}
	/* MATCH_SET_RETURN_TO_TRACE cannot occur if(meta) */
	if ((flags & MATCH_SET_RETURN_TRACE) && is_value(result)) {
	    erts_trace_return(p, ep->code, result, &p->tracer_proc);
	}
	if (flags & MATCH_SET_RETURN_TO_TRACE) { 
	    /* can only happen if(local)*/
	    if (applying) {
		/* Apply of BIF, cp is in calling function */
		erts_trace_return_to(p, cp);
	    } else {
		/* Direct bif call, I points into calling function */
		erts_trace_return_to(p, I);
	    }
	}
	return result;
    }
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
    Process* tracer = NULL;	/* Initialized to eliminate compiler warning */
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

	tracer = process_tab[internal_pid_index(p->tracer_proc)];
	if (INVALID_PID(tracer, p->tracer_proc)
	    || (tracer->flags & F_TRACER) == 0) {
	    p->flags &= ~TRACE_FLAGS;
	    p->tracer_proc = NIL;
	    return;
	}

#ifdef SHARED_HEAP
        /* The running process has the updated heap pointers! */
	hp = HAlloc(p, size);
#else
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */
	hp = HAlloc(tracer, size);
#endif
    }
#undef HEAP_WORDS_NEEDED
#ifdef DEBUG
    limit = hp + size;
#endif

    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
#if (defined(NOMOVE) && defined(SHARED_HEAP))
    CONS_PAIR(am_old_heap_size, make_small(0));
#else
    CONS_PAIR(am_old_heap_size, make_small(OLD_HTOP(p) - OLD_HEAP(p)));
#endif
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_recent_size, make_small(HIGH_WATER(p) - HEAP_START(p)));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
#if (defined(NOMOVE) && defined(SHARED_HEAP))
    CONS_PAIR(am_old_heap_block_size, make_small(0));
#else
    CONS_PAIR(am_old_heap_block_size, make_small(OLD_HEAP(p)
						 ? OLD_HEND(p) - OLD_HEAP(p)
						 : 0));
#endif

    msg = TUPLE4(hp, am_trace, p->id/* Local pid */, what, msg);
    hp += 5;
    if (p->flags & F_TIMESTAMP) {
	hp = patch_ts(msg, hp);
    }
    ASSERT(hp == limit);
    if (is_internal_port(p->tracer_proc)) {
	send_to_port(p, msg, &p->tracer_proc, &p->flags);
    } else {
	queue_message_tt(tracer, NULL, msg, NIL);
    }
#undef CONS_PAIR
}



void
monitor_long_gc(Process *p, Uint time) {
    Process *monitor_p;
    Eterm *hp, timeout, tuple, list, msg;

    ASSERT(is_internal_pid(erts_system_monitor)
	   && internal_pid_index(erts_system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(erts_system_monitor)];
    if (INVALID_PID(monitor_p, erts_system_monitor) || p == monitor_p) {
	return;
    }
#define CONS_PAIR(key, val) \
    tuple = TUPLE2(hp, (key), (val)); hp += 3; \
    list = CONS(hp, tuple, list); hp += 2
#ifdef SHARED_HEAP
    /* Not very interesting to trace long_gc for shared heap.
     * Furthermore all allocations are scary since the GC is not done yet. 
     */
    return;
#else
    /* XXX Multi-thread note: Perhaps allocating on another process's heap. */
    timeout = make_small_or_big(time, monitor_p); /* Hidden ArithAlloc() */
    hp = HAlloc(monitor_p, 30);
    list = NIL;
    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
    CONS_PAIR(am_timeout, timeout);
    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, am_long_gc, list); 
    hp += 5;
    queue_message_tt(monitor_p, NULL, msg, NIL);
#endif
#undef CONS_PAIR
}

void
monitor_large_heap(Process *p) {
    Process *monitor_p;
    Eterm *hp, tuple, list, msg;
    
    ASSERT(is_internal_pid(erts_system_monitor)
	   && internal_pid_index(erts_system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(erts_system_monitor)];
    if (INVALID_PID(monitor_p, erts_system_monitor) || p == monitor_p) {
	return;
    }
#define CONS_PAIR(key, val) \
    tuple = TUPLE2(hp, (key), (val)); hp += 3; \
    list = CONS(hp, tuple, list); hp += 2
#ifdef SHARED_HEAP
    /* Not very interesting to trace large_heap for shared heap.
     * Furthermore all allocations are scary since the GC is not done yet. 
     */
    return;
#else
    /* XXX Multi-thread note: Perhaps allocating on another process's heap. */
    hp = HAlloc(monitor_p, 25);
    list = NIL;
    CONS_PAIR(am_heap_size, make_small(HEAP_TOP(p) - HEAP_START(p)));
    CONS_PAIR(am_stack_size, make_small(STACK_START(p) - p->stop));
    CONS_PAIR(am_mbuf_size, make_small(MBUF_SIZE(p)));
    CONS_PAIR(am_heap_block_size, make_small(HEAP_SIZE(p)));
    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, am_large_heap, list); 
    hp += 5;
    queue_message_tt(monitor_p, NULL, msg, NIL);
#endif
#undef CONS_PAIR
}

void
monitor_generic(Process *p, Eterm type, Eterm spec) {
    Process *monitor_p;
    Eterm *hp, msg;
    
    ASSERT(is_internal_pid(erts_system_monitor)
	   && internal_pid_index(erts_system_monitor) < erts_max_processes);
    monitor_p = process_tab[internal_pid_index(erts_system_monitor)];
    if (INVALID_PID(monitor_p, erts_system_monitor) || p == monitor_p) {
	return;
    }
#ifdef SHARED_HEAP
    /* The running process has the updated heap pointers! */
    hp = HAlloc(p, 5);
#else
    /* XXX Multi-thread note: Perhaps allocating on another process's heap. */
    hp = HAlloc(monitor_p, 5);
#endif
    msg = TUPLE4(hp, am_monitor, p->id/* Local pid */, type, spec); 
    hp += 5;
    queue_message_tt(monitor_p, NULL, msg, NIL);
}
