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

#ifndef __ERL_MESSAGE_H__
#define __ERL_MESSAGE_H__

struct proc_bin;

typedef struct erl_mesg {
    struct erl_mesg* next;  /* Next message */
    Eterm mesg;            /* The message (in buffer or heap) */
    Eterm seq_trace_token; /* Token: NIL if not used */
} ErlMessage;

typedef struct {
    ErlMessage* first;
    ErlMessage** last;  /* point to the last next pointer */
    ErlMessage** save;
    int len;            /* quaue length */
} ErlMessageQueue;

/*
 * This struct represents data that must be updated by structure copy,
 * but is stored outside of any heap.
 */

typedef struct erl_off_heap {
    struct proc_bin* mso;	/* List of associated binaries. */
    struct erl_fun_thing* funs;	/* List of funs. */
    int overhead;		/* Administrative overhead (used to force GC). */
} ErlOffHeap;

/*
 * This struct represents a heap fragment, which is used when there
 * isn't sufficient room in the process heap and we can't do a GC.
 */

typedef struct erl_heap_fragment ErlHeapFragment;
struct erl_heap_fragment {
    ErlHeapFragment* next;	/* Next heap fragment */
    ErlOffHeap off_heap;	/* Offset heap data. */
    unsigned size;		/* Size in words of mem */
    Eterm mem[1];		/* Data */
};

/* Get "current" message */
#define PEEK_MESSAGE(p)  (*(p)->msg.save)

/* Add message last in message queue */
#define LINK_MESSAGE(p, mp) do { \
    *(p)->msg.last = (mp); \
    (p)->msg.last = &(mp)->next; \
    (p)->msg.len++; \
} while(0)

/* Unlink current message */
#define UNLINK_MESSAGE(p,msgp) do { \
     ErlMessage* __mp = (msgp)->next; \
     *(p)->msg.save = __mp; \
     (p)->msg.len--; \
     if (__mp == NULL) \
         (p)->msg.last = (p)->msg.save; \
} while(0)

/* Reset message save point (after receive match) */
#define JOIN_MESSAGE(p) \
     (p)->msg.save = &(p)->msg.first

/* Save current message */
#define SAVE_MESSAGE(p) \
     (p)->msg.save = &(*(p)->msg.save)->next

struct process;

EXTERN_FUNCTION(void, init_message, (_VOID_));
EXTERN_FUNCTION(void, free_message, (ErlMessage*));
EXTERN_FUNCTION(ErlHeapFragment*, new_message_buffer, (uint32));
EXTERN_FUNCTION(void, free_message_buffer, (ErlHeapFragment*));
EXTERN_FUNCTION(ErlMessage*, new_message, (uint32, uint32));
EXTERN_FUNCTION(void, queue_message_tt, (struct process*, ErlHeapFragment*,
					 uint32, uint32));
#define queue_message(a, b, c) queue_message_tt(a, b, c, NIL)
EXTERN_FUNCTION(void, deliver_exit_message_tt, (uint32, struct process*, uint32, uint32));
#define deliver_exit_message(a, b, c) deliver_exit_message_tt(a, b, c, NIL)
void send_message(struct process*, struct process*, Eterm);

EXTERN_FUNCTION(void, deliver_result, (uint32, uint32, uint32));

#endif
