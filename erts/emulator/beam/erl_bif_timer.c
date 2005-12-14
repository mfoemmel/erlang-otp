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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_bif_timer.h"
#include "global.h"
#include "bif.h"
#include "error.h"
#include "big.h"

/****************************************************************************
** BIF Timer support
****************************************************************************/

#define BTM_FLG_SL_TIMER	(((Uint32) 1) << 0)
#define BTM_FLG_CANCELED	(((Uint32) 1) << 1)
#define BTM_FLG_HEAD		(((Uint32) 1) << 2)
#define BTM_FLG_BYNAME		(((Uint32) 1) << 3)
#define BTM_FLG_WRAP		(((Uint32) 1) << 4)

struct ErtsBifTimer_ {
    struct {
	union {
	    ErtsBifTimer **head;
	    ErtsBifTimer *prev;
	} u;
	ErtsBifTimer *next;
    } tab;
    union {
	Eterm name;
	struct {
	    ErtsBifTimer *prev;
	    ErtsBifTimer *next;
	    Process *ess;
	} proc;
    } receiver;
    ErlTimer tm;
    ErlHeapFragment* bp;
    Uint32 flags;
    Eterm message;
    Uint32 ref_numbers[ERTS_REF_NUMBERS];
};

#ifdef SMALL_MEMORY
#define TIMER_HASH_VEC_SZ 3331
#else
#define TIMER_HASH_VEC_SZ 10007
#endif
static ErtsBifTimer **bif_timer_tab;  
static Uint no_bif_timers;

static ERTS_INLINE int
get_index(Uint32 *ref_numbers, Uint32 len)
{
    Uint32 hash;
    /* len can potentially be larger than ERTS_REF_NUMBERS
       if it has visited another node... */
    if (len > ERTS_REF_NUMBERS)
	len = ERTS_REF_NUMBERS;

#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    switch (len) {
    case 3: if (!ref_numbers[2]) len = 2;
    case 2: if (!ref_numbers[1]) len = 1;
    default:  break;
    }

    ASSERT(1 <= len && len <= ERTS_REF_NUMBERS);

    hash = block_hash((byte *) ref_numbers, len * sizeof(Uint32), 0x08d12e65);
    return (int) (hash % ((Uint32) TIMER_HASH_VEC_SZ));
}

static Eterm
create_ref(Uint *hp, Uint32 *ref_numbers, Uint32 len)
{
    Uint32 *datap;
    int i;


    if (len > ERTS_MAX_REF_NUMBERS) {
	/* Such large refs should no be able to appear in the emulator */
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    }

#ifdef ARCH_64
    hp[0] = make_ref_thing_header(len/2 + 1);
    datap = (Uint32 *) &hp[1];
    *(datap++) = len;
#else
    hp[0] = make_ref_thing_header(len);
    datap = (Uint32 *) &hp[1];
#endif

    for (i = 0; i < len; i++)
	datap[i] = ref_numbers[i];

    return make_internal_ref(hp);
}

static int
eq_non_standard_ref_numbers(Uint32 *rn1, Uint32 len1, Uint32 *rn2, Uint32 len2)
{
#ifdef ARCH_64
#define MAX_REF_HEAP_SZ (1+(ERTS_MAX_REF_NUMBERS/2+1))
#else
#define MAX_REF_HEAP_SZ (1+ERTS_MAX_REF_NUMBERS)
#endif
    Uint r1_hp[MAX_REF_HEAP_SZ];
    Uint r2_hp[MAX_REF_HEAP_SZ];

    return eq(create_ref(r1_hp, rn1, len1), create_ref(r2_hp, rn2, len2));
#undef MAX_REF_HEAP_SZ
}

static ERTS_INLINE int
eq_ref_numbers(Uint32 *rn1, Uint32 len1, Uint32 *rn2, Uint32 len2)
{
    int res;
    if (len1 != ERTS_REF_NUMBERS || len2 != ERTS_REF_NUMBERS) {
	/* Can potentially happen, but will never... */
	return eq_non_standard_ref_numbers(rn1, len1, rn2, len2);
    }

#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    res = rn1[0] == rn2[0] && rn1[1] == rn2[1] && rn1[2] == rn2[2];

    ASSERT(res
	   ? eq_non_standard_ref_numbers(rn1, len1, rn2, len2)
	   : !eq_non_standard_ref_numbers(rn1, len1, rn2, len2));

    return res;
}

static ERTS_INLINE ErtsBifTimer *
tab_find(Eterm ref)
{
    Uint32 *ref_numbers = internal_ref_numbers(ref);
    Uint32 ref_numbers_len = internal_ref_no_of_numbers(ref);
    int ix = get_index(ref_numbers, ref_numbers_len);
    ErtsBifTimer* btm;

    for (btm = bif_timer_tab[ix]; btm; btm = btm->tab.next)
	if (eq_ref_numbers(ref_numbers, ref_numbers_len,
			   btm->ref_numbers, ERTS_REF_NUMBERS))
	    return btm;
    return NULL;
}

static ERTS_INLINE void
tab_remove(ErtsBifTimer* btm)
{
    if (btm->flags & BTM_FLG_HEAD) {
	*btm->tab.u.head = btm->tab.next;
	if (btm->tab.next) {
	    btm->tab.next->flags |= BTM_FLG_HEAD;
	    btm->tab.next->tab.u.head = btm->tab.u.head;
	}
    }
    else {
	btm->tab.u.prev->tab.next = btm->tab.next;
	if (btm->tab.next)
	    btm->tab.next->tab.u.prev = btm->tab.u.prev;
    }
    btm->flags |= BTM_FLG_CANCELED;
    ASSERT(no_bif_timers > 0);
    no_bif_timers--;
}

static ERTS_INLINE void
tab_insert(ErtsBifTimer* btm)
{
    int ix = get_index(btm->ref_numbers, ERTS_REF_NUMBERS);
    ErtsBifTimer* btm_list = bif_timer_tab[ix];

    if (btm_list) {
	btm_list->flags &= ~BTM_FLG_HEAD;
	btm_list->tab.u.prev = btm;
    }

    btm->flags |= BTM_FLG_HEAD;
    btm->tab.u.head = &bif_timer_tab[ix];
    btm->tab.next = btm_list;
    bif_timer_tab[ix] = btm;
    no_bif_timers++;
}

static ERTS_INLINE void
link_proc(Process *p, ErtsBifTimer* btm)
{
    btm->receiver.proc.ess = p;
    btm->receiver.proc.prev = NULL;
    btm->receiver.proc.next = p->bif_timers;
    if (p->bif_timers)	
	p->bif_timers->receiver.proc.prev = btm;
    p->bif_timers = btm;
}

static ERTS_INLINE void
unlink_proc(ErtsBifTimer* btm)
{
    if (btm->receiver.proc.prev)
	btm->receiver.proc.prev->receiver.proc.next = btm->receiver.proc.next;
    else
	btm->receiver.proc.ess->bif_timers = btm->receiver.proc.next;
    if (btm->receiver.proc.next)
	btm->receiver.proc.next->receiver.proc.prev = btm->receiver.proc.prev;
}

static void
bif_timer_cleanup(ErtsBifTimer* btm)
{
    ASSERT(btm);

    if (btm->bp)
	free_message_buffer(btm->bp);

    ERTS_PROC_LESS_MEM(sizeof(ErtsBifTimer));
    if (btm->flags & BTM_FLG_SL_TIMER)
	erts_free(ERTS_ALC_T_SL_BIF_TIMER, (void *) btm);
    else
	erts_free(ERTS_ALC_T_LL_BIF_TIMER, (void *) btm);
}

static void
bif_timer_timeout(ErtsBifTimer* btm)
{
    ASSERT(btm);

    if (btm->flags & BTM_FLG_CANCELED) {
	ASSERT(0);
    }
    else {
	Process* rp;

	tab_remove(btm);

	if (btm->flags & BTM_FLG_BYNAME)
	    rp = whereis_process(btm->receiver.name);
	else {
	    rp = btm->receiver.proc.ess;
	    if (rp->status == P_EXITING)
		rp = NULL;
	    unlink_proc(btm);
	}

	if (rp) {
	    Eterm message;

	    if (!(btm->flags & BTM_FLG_WRAP))
		message = btm->message;
	    else {
#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
		Eterm ref;
		Uint *hp = HAlloc(rp, REF_THING_SIZE + 4);
		write_ref_thing(hp,
				btm->ref_numbers[0],
				btm->ref_numbers[1],
				btm->ref_numbers[2]);
		ref = make_internal_ref(hp);
		hp += REF_THING_SIZE;
		message = TUPLE3(hp, am_timeout, ref, btm->message);
	    }

	    queue_message_tt(rp, btm->bp, message, NIL);
	    btm->bp = NULL; /* Prevent cleanup of message buffer... */
	}
    }
    bif_timer_cleanup(btm);
}

static Eterm
setup_bif_timer(Uint32 xflags,
		Process *c_p,
		Eterm time,
		Eterm receiver,
		Eterm message)
{
    Process *rp;
    ErtsBifTimer* btm;
    Uint timeout;
    Eterm ref;
    Uint32 *ref_numbers;
    
    if (!term_to_Uint(time, &timeout))
	return THE_NON_VALUE;
#ifdef ARCH_64
    if ((timeout >> 32) != 0)
	return THE_NON_VALUE;
#endif
    if (is_not_internal_pid(receiver) && is_not_atom(receiver))
	return THE_NON_VALUE;

    ref = erts_make_ref(c_p);

    if (is_atom(receiver))
	rp = NULL;
    else {
	rp = pid2proc(receiver);
	if (!rp)
	    return ref;
    }

    ERTS_PROC_MORE_MEM(sizeof(ErtsBifTimer));
    if (timeout < ERTS_ALC_MIN_LONG_LIVED_TIME) {
	btm = (ErtsBifTimer *) erts_alloc(ERTS_ALC_T_SL_BIF_TIMER,
					 sizeof(ErtsBifTimer));
	btm->flags = BTM_FLG_SL_TIMER;
    }
    else {
	btm = (ErtsBifTimer *) erts_alloc(ERTS_ALC_T_LL_BIF_TIMER,
					 sizeof(ErtsBifTimer));
	btm->flags = 0;
    }

    btm->flags |= xflags;

    ref_numbers = internal_ref_numbers(ref);
    ASSERT(internal_ref_no_of_numbers(ref) == 3);
#if ERTS_REF_NUMBERS != 3
#error "ERTS_REF_NUMBERS changed. Update me..."
#endif
    btm->ref_numbers[0] = ref_numbers[0];
    btm->ref_numbers[1] = ref_numbers[1];
    btm->ref_numbers[2] = ref_numbers[2];

    ASSERT(eq_ref_numbers(btm->ref_numbers, ERTS_REF_NUMBERS,
			  ref_numbers, ERTS_REF_NUMBERS));

    if (is_immed(message)) {
	btm->bp = NULL;
	btm->message = message;
    }
    else {
	ErlHeapFragment* bp;
	Eterm* hp;
	Uint size;

	size = size_object(message);
	btm->bp = bp = new_message_buffer(size);
	hp = bp->mem;
	btm->message = copy_struct(message, size, &hp, &bp->off_heap);
    }

    if (rp)
	link_proc(rp, btm);
    else {
	ASSERT(is_atom(receiver));
	btm->receiver.name = receiver;
	btm->flags |= BTM_FLG_BYNAME;
    }
    tab_insert(btm);
    ASSERT(btm == tab_find(ref));
    btm->tm.active = 0; /* MUST be initalized */
    erl_set_timer(&btm->tm,
		  (ErlTimeoutProc) bif_timer_timeout,
		  (ErlCancelProc) bif_timer_cleanup,
		  (void *) btm,
		  timeout);
    return ref;
}

/* send_after(Time, Pid, Message) -> Ref */
BIF_RETTYPE send_after_3(BIF_ALIST_3)
{
    Eterm res;

    res = setup_bif_timer(0, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    else {
	ASSERT(is_internal_ref(res));
	BIF_RET(res);
    }
}

/* start_timer(Time, Pid, Message) -> Ref */
BIF_RETTYPE start_timer_3(BIF_ALIST_3)
{
    Eterm res;

    res = setup_bif_timer(BTM_FLG_WRAP, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    else {
	ASSERT(is_internal_ref(res));
	BIF_RET(res);
    }
}

/* cancel_timer(Ref) -> false | RemainingTime */
BIF_RETTYPE cancel_timer_1(BIF_ALIST_1)
{
    Eterm res;
    ErtsBifTimer *btm;

    if (is_not_internal_ref(BIF_ARG_1)) {
	if (is_ref(BIF_ARG_1)) {
	    BIF_RET(am_false);
	}
	BIF_ERROR(BIF_P, BADARG);
    }

    btm = tab_find(BIF_ARG_1);
    if (!btm || btm->flags & BTM_FLG_CANCELED) {
	res = am_false;
    }
    else {
	Uint left = time_left(&btm->tm);
	if (!(btm->flags & BTM_FLG_BYNAME))
	    unlink_proc(btm);
	tab_remove(btm);
	ASSERT(!tab_find(BIF_ARG_1));
	erl_cancel_timer(&btm->tm);
	res = make_small_or_big(left, BIF_P);
    }

    BIF_RET(res);
}

/* read_timer(Ref) -> false | RemainingTime */
BIF_RETTYPE read_timer_1(BIF_ALIST_1)
{
    Eterm res;
    ErtsBifTimer *btm;

    if (is_not_internal_ref(BIF_ARG_1)) {
	if (is_ref(BIF_ARG_1)) {
	    BIF_RET(am_false);
	}
	BIF_ERROR(BIF_P, BADARG);
    }

    btm = tab_find(BIF_ARG_1);
    if (!btm || btm->flags & BTM_FLG_CANCELED) {
	res = am_false;
    }
    else {
	Uint left = time_left(&btm->tm);
	res = make_small_or_big(left, BIF_P);
    }

    BIF_RET(res);
}

void
erts_print_bif_timer_info(CIO to)
{
    int i;

    for (i = 0; i < TIMER_HASH_VEC_SZ; i++) {
	ErtsBifTimer *btm;
	for (btm = bif_timer_tab[i]; btm; btm = btm->tab.next) {
	    erl_printf(to, "=timer:");
	    display(btm->flags & BTM_FLG_BYNAME
		    ? btm->receiver.name
		    : btm->receiver.proc.ess->id, to);
	    erl_printf(to, "\n");
	    erl_printf(to, "Message: ");
	    display(btm->message, to);
	    erl_printf(to, "\n");
	    erl_printf(to, "Time left: %d ms\n", time_left(&btm->tm));
	}
    }
}


void
erts_cancel_bif_timers(struct process *p)
{
    ErtsBifTimer *btm;

    btm = p->bif_timers;
    while (btm) {
	ErtsBifTimer *tmp_btm;
	ASSERT(!(btm->flags & BTM_FLG_CANCELED));
	tab_remove(btm);
	tmp_btm = btm;
	btm = btm->receiver.proc.next;
	erl_cancel_timer(&tmp_btm->tm);
    }

    p->bif_timers = NULL;
}

void erts_bif_timer_init(void)
{
    int i;
    no_bif_timers = 0;
    bif_timer_tab = erts_alloc(ERTS_ALC_T_BIF_TIMER_TABLE,
			       sizeof(ErtsBifTimer *)*TIMER_HASH_VEC_SZ);
    for (i = 0; i < TIMER_HASH_VEC_SZ; ++i)
	bif_timer_tab[i] = NULL;
}

Uint
erts_bif_timer_memory_size(void)
{
    Uint res;

    res = (sizeof(ErtsBifTimer *)*TIMER_HASH_VEC_SZ
	   + no_bif_timers*sizeof(ErtsBifTimer));

    return res;
}

