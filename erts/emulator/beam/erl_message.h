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
struct external_thing_;

typedef struct erl_mesg {
    struct erl_mesg* next;	/* Next message */
    Eterm m[2];			/* m[0] = message, m[1] = seq trace token */
} ErlMessage;

#define ERL_MESSAGE_TERM(mp) ((mp)->m[0])
#define ERL_MESSAGE_TOKEN(mp) ((mp)->m[1])

/* Size of default message buffer (erl_message.c) */
#define ERL_MESSAGE_BUF_SZ 500

typedef struct {
    ErlMessage* first;
    ErlMessage** last;  /* point to the last next pointer */
    ErlMessage** save;
    int len;            /* queue length */
} ErlMessageQueue;

/*
 * This struct represents data that must be updated by structure copy,
 * but is stored outside of any heap.
 */

typedef struct erl_off_heap {
    struct proc_bin* mso;	/* List of associated binaries. */
#ifndef SHARED_HEAP
    struct erl_fun_thing* funs;	/* List of funs. */
#endif
    struct external_thing_* externals; /* List of external things. */
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
    ERTS_PROC_MORE_MEM(sizeof(ErlMessage)); \
} while(0)

/* Unlink current message */
#define UNLINK_MESSAGE(p,msgp) do { \
     ErlMessage* __mp = (msgp)->next; \
     *(p)->msg.save = __mp; \
     (p)->msg.len--; \
     if (__mp == NULL) \
         (p)->msg.last = (p)->msg.save; \
     ERTS_PROC_LESS_MEM(sizeof(ErlMessage)); \
} while(0)

/* Reset message save point (after receive match) */
#define JOIN_MESSAGE(p) \
     (p)->msg.save = &(p)->msg.first

/* Save current message */
#define SAVE_MESSAGE(p) \
     (p)->msg.save = &(*(p)->msg.save)->next

struct process;

void init_message(void);
void free_message(ErlMessage *);
ErlHeapFragment* new_message_buffer(Uint);
void free_message_buffer(ErlHeapFragment *);
void queue_message_tt(struct process*, ErlHeapFragment*, Eterm, Eterm);
void deliver_exit_message_tt(Eterm, struct process*, Eterm, Eterm);
#define deliver_exit_message(a, b, c) deliver_exit_message_tt(a, b, c, NIL)
void send_message(struct process*, struct process*, Eterm);

void deliver_result(Eterm, Eterm, Eterm);

#endif
