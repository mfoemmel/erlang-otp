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

/*
 * This struct represents data that must be updated by structure copy,
 * but is stored outside of any heap.
 */

typedef struct erl_off_heap {
    struct proc_bin* mso;	/* List of associated binaries. */
#ifndef HYBRID /* FIND ME! */
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

#define ERTS_SET_MBUF_HEAP_END(BP, HENDP)				\
do {									\
    unsigned real_size__ = (BP)->size;					\
    ASSERT((BP)->mem <= (HENDP) && (HENDP) <= (BP)->mem + real_size__);	\
    (BP)->size = (HENDP) - (BP)->mem;					\
    ERTS_PROC_LESS_MEM(real_size__ - (BP)->size);			\
    /* We do not reallocate since buffer *might* be moved.	*/	\
    /* FIXME: Memory count is wrong, but at least it's almost	*/	\
    /*        right...						*/	\
} while (0)

typedef struct erl_mesg {
    struct erl_mesg* next;	/* Next message */
#if defined(ERTS_SMP) || defined(HEAP_FRAG_ELIM_TEST)
    ErlHeapFragment *bp;
#endif
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

#ifdef ERTS_SMP

typedef struct {
    ErlMessage* first;
    ErlMessage** last;  /* point to the last next pointer */
    int len;            /* queue length */
} ErlMessageInQueue;

#endif

/* Get "current" message */
#define PEEK_MESSAGE(p)  (*(p)->msg.save)


/* Add message last in private message queue */
#define LINK_MESSAGE_PRIVQ(p, mp) do { \
    *(p)->msg.last = (mp); \
    (p)->msg.last = &(mp)->next; \
    (p)->msg.len++; \
    ERTS_PROC_MORE_MEM(sizeof(ErlMessage)); \
} while(0)


#ifdef ERTS_SMP

/* Move in message queue to end of private message queue */
#define ERTS_SMP_MSGQ_MV_INQ2PRIVQ(P)			\
do {							\
    if ((P)->msg_inq.first) {				\
	*(P)->msg.last = (P)->msg_inq.first;		\
	(P)->msg.last = (P)->msg_inq.last;		\
	(P)->msg.len += (P)->msg_inq.len;		\
	(P)->msg_inq.first = NULL;			\
	(P)->msg_inq.last = &(P)->msg_inq.first;	\
	(P)->msg_inq.len = 0;				\
    }							\
} while (0)

/* Add message last in message queue */
#define LINK_MESSAGE(p, mp) do { \
    *(p)->msg_inq.last = (mp); \
    (p)->msg_inq.last = &(mp)->next; \
    (p)->msg_inq.len++; \
    ERTS_PROC_MORE_MEM(sizeof(ErlMessage)); \
} while(0)

#else

#define ERTS_SMP_MSGQ_MV_INQ2PRIVQ(P)

/* Add message last in message queue */
#define LINK_MESSAGE(p, mp) LINK_MESSAGE_PRIVQ((p), (mp))

#endif

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

#define ERTS_SND_FLG_NO_SEQ_TRACE		(((unsigned) 1) << 0)

void init_message(void);
void free_message(ErlMessage *);
ErlHeapFragment* new_message_buffer(Uint);
ErlHeapFragment* erts_resize_message_buffer(ErlHeapFragment *, Uint,
					    Eterm *, Uint);
void free_message_buffer(ErlHeapFragment *);
void erts_queue_message(struct process*, Uint32, ErlHeapFragment*, Eterm, Eterm);
void erts_deliver_exit_message(Eterm, struct process*, Uint32 *, Eterm, Eterm);
void erts_send_message(struct process*, struct process*, Uint32 *, Eterm, unsigned);
void erts_link_mbuf_to_proc(struct process *proc, ErlHeapFragment *bp);

#if defined(ERTS_SMP) || defined(HEAP_FRAG_ELIM_TEST)
void erts_move_msg_mbuf_to_heap(Eterm**, ErlOffHeap*, ErlMessage *);
void erts_move_msg_mbuf_to_proc_mbufs(struct process*, ErlMessage *);
#endif

#endif
