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
**     Message passing primitives
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_message.h"
#include "erl_process.h"

void
init_message(void)
{
}

void free_message(mp)
ErlMessage* mp;
{
    fix_free(mesg_desc, (uint32*) mp);
}

/* Allocate message buffer (size in words) */
ErlHeapFragment* new_message_buffer(size)
uint32 size;
{
    ErlHeapFragment* bp;

    bp = (ErlHeapFragment*) safe_alloc_from(33,
					    sizeof(ErlHeapFragment) +
					    ((size-1)*sizeof(Eterm)));
    bp->next = NULL;
    bp->size = size;
    bp->off_heap.mso = NULL;
    bp->off_heap.funs = NULL;
    bp->off_heap.overhead = 0;
    return bp;
}

void
free_message_buffer(ErlHeapFragment* bp)
{
    if (bp->off_heap.mso) {
	erts_cleanup_mso(bp->off_heap.mso);
    }
    sys_free(bp);
}

/* Add a message last in message queue */
void
queue_message_tt(Process* receiver, ErlHeapFragment* bp,
		 Eterm message, Eterm seq_trace_token)
{
    ErlMessage* mp;

    mp = (ErlMessage*) fix_alloc(mesg_desc);
    mp->mesg = message;
    mp->next = NULL;
    mp->seq_trace_token = seq_trace_token;
    LINK_MESSAGE(receiver, mp);

    if (bp != NULL) {
	/* Link the message buffer */
	bp->next = receiver->mbuf;
	receiver->mbuf = bp;
	receiver->mbuf_sz += bp->size;
	receiver->off_heap.overhead += (sizeof(ErlHeapFragment)/sizeof(Eterm) - 1); 

	/* Move any binaries into the process */
	if (bp->off_heap.mso != NULL) {
	    ProcBin** next_p = &bp->off_heap.mso;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = receiver->off_heap.mso;
	    receiver->off_heap.mso = bp->off_heap.mso;
	    bp->off_heap.mso = NULL;
	    receiver->off_heap.overhead += bp->off_heap.overhead;
	}

	/* Move any funs into the process */
	if (bp->off_heap.funs != NULL) {
	    ErlFunThing** next_p = &bp->off_heap.funs;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = receiver->off_heap.funs;
	    receiver->off_heap.funs = bp->off_heap.funs;
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
    uint32 msize;
    Eterm token = NIL;

    msize = size_object(message);
    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	seq_trace_output(SEQ_TRACE_TOKEN(sender), message, SEQ_TRACE_SEND, 
			 receiver->id, sender);
	bp = new_message_buffer(msize + 6 /* TUPLE5 */);
	hp = bp->mem;
	token = copy_struct(SEQ_TRACE_TOKEN(sender), 6 /* TUPLE5 */, 
			    &hp, &receiver->off_heap);
    } else if (msize <= 64) {
	ErlMessage* mp = (ErlMessage*) fix_alloc(mesg_desc);

	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */
	hp = HAlloc(receiver, msize);
	message = copy_struct(message, msize, &hp, &receiver->off_heap);
	mp->mesg = message;
	mp->next = NULL;
	mp->seq_trace_token = NIL;
	LINK_MESSAGE(receiver, mp);

	if (receiver->status == P_WAITING) {
	    add_to_schedule_q(receiver);
	} else if (receiver->status == P_SUSPENDED) {
	    receiver->rstatus = P_RUNABLE;
	}
	if (IS_TRACED_FL(receiver, F_TRACE_RECEIVE)) {
	    trace_receive(receiver, message);
	}
	return;
    } else {
	bp = new_message_buffer(msize);
	hp = bp->mem;
    }
    message = copy_struct(message, msize, &hp, &receiver->off_heap);
    queue_message_tt(receiver, bp, message, token);
}

/*
 * This function delivers an EXIT message to a process
 * which is trapping EXITs.
 */

void
deliver_exit_message_tt(Eterm from, Process *to, Eterm reason, uint32 token)
{
    uint32 mess;
    uint32 save;
    uint32 sz_reason;
    ErlHeapFragment* bp;
    Eterm* hp;
    uint32 sz_token;
    uint32 temptoken;

    if (token != NIL) {
	ASSERT(is_tuple(token));
	sz_reason = size_object(reason);
	sz_token = size_object(token);
	bp = new_message_buffer(sz_reason + 4 + sz_token);
	hp = bp->mem;
	mess = copy_struct(reason, sz_reason, &hp, &to->off_heap);
	save = TUPLE3(hp, am_EXIT, from, mess);
	hp += 4;
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->id, NULL);
	temptoken = copy_struct(token, sz_token, &hp, &to->off_heap);
	queue_message_tt(to, bp, save, temptoken);
    } else {
	sz_reason = size_object(reason);
	bp = new_message_buffer(sz_reason + 4);
	hp = bp->mem;
	mess = copy_struct(reason, sz_reason, &hp, &to->off_heap);
	save = TUPLE3(hp, am_EXIT, from, mess);
	hp += 4;
	queue_message_tt(to, bp, save, NIL);
    }
}

void
deliver_result(Eterm sender, Eterm pid, Eterm res)
{
    Eterm tuple;
    Process *rp;
    ErlHeapFragment* bp;
    Eterm* hp;
    uint32 sz_res;

    rp = process_tab[pid_number(pid)];
    if (!INVALID_PID(rp, pid)) {
	sz_res = size_object(res);
	bp = new_message_buffer(sz_res + 3);
	hp = bp->mem;
	res = copy_struct(res, sz_res, &hp, &rp->off_heap);
	tuple = TUPLE2(hp, sender, res);
	hp += 3;
	queue_message_tt(rp, bp, tuple, NIL);
    }
}
