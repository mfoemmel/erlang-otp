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

static int  d_4;
static int  d_8;
static int d_16;
static int d_32;
static int d_64;
static int d_buf[64+1];

/* initialize messages */
void init_message()
{
    int i;

    /* Handle free lists etc */
    d_4 = new_fix_size(sizeof(ErlHeapFragment) + sizeof(uint32)*(4-1));
    d_8 = new_fix_size(sizeof(ErlHeapFragment) + sizeof(uint32)*(8-1));
    d_16 = new_fix_size(sizeof(ErlHeapFragment) + sizeof(uint32)*(16-1));
    d_32 = new_fix_size(sizeof(ErlHeapFragment) + sizeof(uint32)*(32-1));
    d_64 = new_fix_size(sizeof(ErlHeapFragment) + sizeof(uint32)*(64-1));

    for (i = 0; i <= 4; i++) {
	d_buf[i] = d_4;
    }
    for (i = 5; i <= 8; i++) {
	d_buf[i] = d_8;
    }
    for (i = 9; i <= 16; i++) {
	d_buf[i] = d_16;
    }
    for (i = 17; i <= 32; i++) {
	d_buf[i] = d_32;
    }
    for (i = 33; i <= 64; i++) {
	d_buf[i] = d_64;
    }
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

#ifdef INSTRUMENT
    alloc_from(33);
#endif

    if (size > 64) {
	bp = (ErlHeapFragment*) safe_alloc(sizeof(ErlHeapFragment) +
					    ((size-1)*sizeof(uint32)));
    } else {
	bp = (ErlHeapFragment*) fix_alloc(d_buf[size]);
    }
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

    /*
     * XXX Change this when we introduce heap binaries.
     */
    if (bp->size > 64)
	sys_free(bp);
    else
	fix_free(d_buf[bp->size], (uint32*) bp);
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
	receiver->off_heap.overhead +=  (sizeof(ErlHeapFragment)/sizeof(Eterm) - 1); 
	/* mbuf_struct_sz is the administrative overhead caused by 
	 * message buffers, used together with mbuf_sz to indicate 
	 * that GC is needed
	 */ 

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
	seq_trace_output(SEQ_TRACE_TOKEN(sender), message, SEQ_TRACE_SEND, receiver->id);
	bp = new_message_buffer(msize + 6 /* TUPLE5 */);
	hp = bp->mem;
	token = copy_struct(SEQ_TRACE_TOKEN(sender), 6 /* TUPLE5 */, 
			    &hp, &receiver->off_heap);
    } else {
	bp = new_message_buffer(msize);
	hp = bp->mem;
    }
    message = copy_struct(message, msize, &hp, &receiver->off_heap);
    queue_message_tt(receiver, bp, message, token);
}

/* Called from dist */
void
send_msg(Eterm to, Eterm message)
{
    Process* rp = process_tab[get_number(to)];

    if (!INVALID_PID(rp, to)) {
	Eterm* hp;
	unsigned size = size_object(message);
	ErlHeapFragment* bp = new_message_buffer(size);

	hp = bp->mem;
	message = copy_struct(message, size, &hp, &rp->off_heap);
	queue_message_tt(rp, bp, message, NIL);
    }
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
	/* the trace token must in this case be updated by the caller */
	seq_trace_output(token, save, SEQ_TRACE_SEND, to->id);
	temptoken = copy_struct(token, sz_token, &hp, &to->off_heap);
	queue_message_tt(to, bp, save, temptoken);
    } else {
	sz_reason = size_object(reason);
	bp = new_message_buffer(sz_reason + 4);
	hp = bp->mem;
	mess = copy_struct(reason, sz_reason, &hp, &to->off_heap);
	save = TUPLE3(hp, am_EXIT, from, mess);
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

    rp = process_tab[get_number(pid)];
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
