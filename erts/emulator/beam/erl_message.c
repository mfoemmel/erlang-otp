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
 * Message passing primitives.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_message.h"
#include "erl_process.h"

static ErlMessage erl_message_buf[ERL_MESSAGE_BUF_SZ];
static ErlMessage *erl_message_buf_end;
static ErlMessage *free_erl_message = NULL;

void
init_message(void)
{
    int i;

    erl_message_buf_end = erl_message_buf + ERL_MESSAGE_BUF_SZ;

    /* Setup free list */
    free_erl_message = &erl_message_buf[0];
    for (i = 0; i < ERL_MESSAGE_BUF_SZ - 1; i++)
	erl_message_buf[i].next = &erl_message_buf[i + 1];
    erl_message_buf[ERL_MESSAGE_BUF_SZ - 1].next = NULL;
    ASSERT(&erl_message_buf[ERL_MESSAGE_BUF_SZ - 1] < erl_message_buf_end);
}

static ErlMessage *
alloc_message(void)
{
    ErlMessage *res;

    if (free_erl_message) {
	res = free_erl_message;
	free_erl_message = free_erl_message->next;
    }
    else
	res = (ErlMessage *) erts_safe_sl_alloc_from(34, sizeof(ErlMessage));

    return res;
}

void
free_message(ErlMessage* mp)
{
    if (mp < erl_message_buf_end && mp >= erl_message_buf) {
	mp->next = free_erl_message;
	free_erl_message = mp;
    }
    else
	erts_sl_free((void *) mp);
}

/* Allocate message buffer (size in words) */
ErlHeapFragment*
new_message_buffer(Uint size)
{
    ErlHeapFragment* bp;

    bp = (ErlHeapFragment*) erts_safe_sl_alloc_from(33,
						    sizeof(ErlHeapFragment) +
						    ((size-1)*sizeof(Eterm)));
    bp->next = NULL;
    bp->size = size;
    bp->off_heap.mso = NULL;
#ifndef SHARED_HEAP
    bp->off_heap.funs = NULL;
#endif
    bp->off_heap.externals = NULL;
    bp->off_heap.overhead = 0;
    return bp;
}

void
erts_cleanup_offheap(ErlOffHeap *offheap)
{
    if (offheap->mso) {
	erts_cleanup_mso(offheap->mso);
    }
#ifndef SHARED_HEAP
    if (offheap->funs) {
	erts_cleanup_funs(offheap->funs);
    }
#endif
    if (offheap->externals) {
	erts_cleanup_externals(offheap->externals);
    }
}

void
free_message_buffer(ErlHeapFragment* bp)
{
    erts_cleanup_offheap(&bp->off_heap);
    erts_sl_free((void *) bp);
}

/* Add a message last in message queue */
void
queue_message_tt(Process* receiver, ErlHeapFragment* bp,
		 Eterm message, Eterm seq_trace_token)
{
    ErlMessage* mp;

    mp = alloc_message();
    ERL_MESSAGE_TERM(mp) = message;
    ERL_MESSAGE_TOKEN(mp) = seq_trace_token;
    mp->next = NULL;
    LINK_MESSAGE(receiver, mp);

#ifdef SHARED_HEAP
    receiver->active = 1;
#endif

    if (bp != NULL) {
	/* Link the message buffer */
        bp->next = MBUF(receiver);
        MBUF(receiver) = bp;
        MBUF_SIZE(receiver) += bp->size;
	MSO(receiver).overhead +=
	    (sizeof(ErlHeapFragment) / sizeof(Eterm) - 1);
	if (ARITH_LOWEST_HTOP(receiver) == NULL) {
	    ARITH_LOWEST_HTOP(receiver) = receiver->htop;
	}

	/* Move any binaries into the process */
	if (bp->off_heap.mso != NULL) {
	    ProcBin** next_p = &bp->off_heap.mso;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(receiver).mso;
	    MSO(receiver).mso = bp->off_heap.mso;
	    bp->off_heap.mso = NULL;
	    MSO(receiver).overhead += bp->off_heap.overhead;
	}

	/* Move any funs into the process */
#ifndef SHARED_HEAP
	if (bp->off_heap.funs != NULL) {
	    ErlFunThing** next_p = &bp->off_heap.funs;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(receiver).funs;
	    MSO(receiver).funs = bp->off_heap.funs;
	    bp->off_heap.funs = NULL;
	}
#endif

	/* Move any external things into the process */
	if (bp->off_heap.externals != NULL) {
	    ExternalThing** next_p = &bp->off_heap.externals;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(receiver).externals;
	    MSO(receiver).externals = bp->off_heap.externals;
	    bp->off_heap.externals = NULL;
	}
    }

    if (receiver->status == P_WAITING) {
	add_to_schedule_q(receiver);
    } else if (receiver->status == P_SUSPENDED) {
	receiver->rstatus = P_RUNABLE;
    }

    if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	trace_receive(receiver, message);
    }
}

/*
 * Send a local message when sender & receiver processes are known.
 */

void
send_message(Process* sender, Process* receiver, Eterm message)
{
    Eterm* hp;
    ErlHeapFragment* bp = NULL;
    Uint msize;
    Eterm token = NIL;
    NEW_TIMER(send);
    NEW_TIMER(copy);
    NEW_TIMER(size);

    STOP_TIMER(system);
#ifdef __BENCHMARK__
    messages_sent++;
#endif

#ifdef CALCULATE_MESSAGE_SIZES
    msize = size_object(message);
    if (msize < 1000) message_sizes[msize]++;
    else message_sizes[999]++;
#endif

#ifndef SHARED_HEAP
    START_TIMER(size);
    msize = size_object(message);
    STOP_TIMER(size);
#endif
    START_TIMER(send);
    if (SEQ_TRACE_TOKEN(sender) != NIL) {
#ifdef SHARED_HEAP
        SWAP_TIMER(send,size);
        msize = size_object(message);
        SWAP_TIMER(size,send);
#endif
	seq_trace_update_send(sender);
	seq_trace_output(SEQ_TRACE_TOKEN(sender), message, SEQ_TRACE_SEND, 
			 receiver->id, sender);
	bp = new_message_buffer(msize + 6 /* TUPLE5 */);
	hp = bp->mem;
        SWAP_TIMER(send,copy);
	token = copy_struct(SEQ_TRACE_TOKEN(sender), 6 /* TUPLE5 */, 
			    &hp, &MSO(receiver));
	message = copy_struct(message, msize, &hp, &MSO(receiver));
	SWAP_TIMER(copy,send);
	queue_message_tt(receiver, bp, message, token);
	SWAP_TIMER(send,system);
#ifdef SHARED_HEAP
    } else {
        ErlMessage* mp = alloc_message();
        ERL_MESSAGE_TERM(mp) = message;
        ERL_MESSAGE_TOKEN(mp) = NIL;
        mp->next = NULL;
        LINK_MESSAGE(receiver, mp);
        receiver->active = 1;
	
        if (receiver->status == P_WAITING) {
            add_to_schedule_q(receiver);
        } else if (receiver->status == P_SUSPENDED) {
            receiver->rstatus = P_RUNABLE;
        }
        if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
            trace_receive(receiver, message);
        }
        SWAP_TIMER(send,system);
        return;
#else
    } else if (sender == receiver) {
	ErlMessage* mp = alloc_message();
	ERL_MESSAGE_TERM(mp) = message;
	ERL_MESSAGE_TOKEN(mp) = NIL;
	mp->next = NULL;
	LINK_MESSAGE(receiver, mp);

	if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	    trace_receive(receiver, message);
	}
        SWAP_TIMER(send,system);
	return;
    } else {
	ErlMessage* mp = alloc_message();

	if (receiver->stop - receiver->htop <= msize) {
            STOP_TIMER(send);
	    erts_garbage_collect(receiver, msize, receiver->arg_reg, receiver->arity);
            START_TIMER(send);
	}
	hp = receiver->htop;
	receiver->htop = hp + msize;
        SWAP_TIMER(send,copy);
	message = copy_struct(message, msize, &hp, &receiver->off_heap);
        SWAP_TIMER(copy,send);
	ERL_MESSAGE_TERM(mp) = message;
	ERL_MESSAGE_TOKEN(mp) = NIL;
	mp->next = NULL;
	LINK_MESSAGE(receiver, mp);

	if (receiver->status == P_WAITING) {
	    add_to_schedule_q(receiver);
	} else if (receiver->status == P_SUSPENDED) {
	    receiver->rstatus = P_RUNABLE;
	}
	if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	    trace_receive(receiver, message);
	}
        SWAP_TIMER(send,system);
	return;
#endif /* SHARED_HEAP */
    }
}

/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

void
deliver_exit_message_tt(Eterm from, Process *to, Eterm reason, Eterm token)
{
    Eterm mess;
    Eterm save;
    Eterm from_copy;
    Uint sz_reason;
    Uint sz_token;
    Uint sz_from;
    Eterm* hp;
    Eterm temptoken;

    if (token != NIL) {
	ErlHeapFragment* bp;

	ASSERT(is_tuple(token));
	sz_reason = size_object(reason);
	sz_token = size_object(token);
	sz_from = size_object(from);
	bp = new_message_buffer(sz_reason + sz_from + sz_token + 4);
	hp = bp->mem;
	mess = copy_struct(reason, sz_reason, &hp, &MSO(to));
	from_copy = copy_struct(from, sz_from, &hp, &MSO(to));
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	hp += 4;
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->id, NULL);
	temptoken = copy_struct(token, sz_token, &hp, &MSO(to));
	queue_message_tt(to, bp, save, temptoken);
    } else {
	sz_reason = size_object(reason);
	sz_from = IS_CONST(from) ? 0 : size_object(from);
#ifdef SHARED_HEAP
	hp = erts_global_alloc(sz_reason+sz_from+4);
#else
	hp = HAlloc(to, sz_reason+sz_from+4);
#endif
	mess = copy_struct(reason, sz_reason, &hp, &MSO(to));
	from_copy = (IS_CONST(from)
		     ? from
		     : copy_struct(from, sz_from, &hp, &MSO(to)));
	save = TUPLE3(hp, am_EXIT, from_copy, mess);
	queue_message_tt(to, NULL, save, NIL);
    }
}

void
deliver_result(Eterm sender, Eterm pid, Eterm res)
{
    Eterm tuple;
    Process *rp;
    Eterm* hp;
    Uint sz_res;

    ASSERT(is_internal_port(sender)
	   && is_internal_pid(pid)
	   && internal_pid_index(pid) < erts_max_processes);

    rp = process_tab[internal_pid_index(pid)];

    if (!INVALID_PID(rp, pid)) {
	sz_res = size_object(res);
	hp = HAlloc(rp, sz_res + 3);
	res = copy_struct(res, sz_res, &hp, &MSO(rp));
	tuple = TUPLE2(hp, sender, res);
	queue_message_tt(rp, NULL, tuple, NIL);
    }
}
