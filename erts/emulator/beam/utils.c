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

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_db.h"
#include "erl_threads.h"
#include "register.h"
#include "erl_vector.h"
#include "dist.h"

#undef M_TRIM_THRESHOLD
#undef M_TOP_PAD
#undef M_MMAP_THRESHOLD
#undef M_MMAP_MAX

#if !defined(ELIB_ALLOC_IS_CLIB) && defined(__GLIBC__) && defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#if defined(ELIB_ALLOC_IS_CLIB) || !defined(HAVE_MALLOPT)
#undef  HAVE_MALLOPT
#define HAVE_MALLOPT 0
#endif

/* Forward */
static int is_printable_string(Eterm);

static Eterm*
do_alloc(Process* p, Eterm* last_htop, Uint need)
{
    ErlHeapFragment* bp;
    Uint n;
#ifdef DEBUG
    Uint i;
#endif

    /*
     * Check if there is any space left in the previous heap fragment.
     */

    if (need <= ARITH_AVAIL(p)) {
	Eterm* hp = ARITH_HEAP(p);

	ARITH_HEAP(p) += need;
	ARITH_AVAIL(p) -= need;
	return hp;
    }

    /*
     * Allocate a new arith heap.
     */

#ifdef SHARED_HEAP
    n = need;
#else
    /*
     * Find a new suitable size.
     */

    n = need;
    if (ARITH_AVAIL(p) < 16 || n < 64) {
	ARITH_AVAIL(p) = 0;
	n = p->min_heap_size/2 + need;
	if (n > 16*1024 && n > 2*need) {
	    n = 2*need;
	}
    }
#endif

#ifdef DEBUG
    n++;
#endif

    bp = (ErlHeapFragment*)
	ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG,
			sizeof(ErlHeapFragment) + ((n-1)*sizeof(Eterm)));
#ifdef DEBUG
    n--;
#endif
    if (ARITH_AVAIL(p) == 0) {
	ARITH_AVAIL(p) = n - need;
	ARITH_HEAP(p) = bp->mem + need;
    }
#ifdef DEBUG
    for (i = 0; i <= n; i++) {
	bp->mem[i] = ARITH_MARKER;
    }
    ARITH_CHECK_ME(p) = ARITH_HEAP(p);
#endif
#ifdef HEAP_FRAG_ELIM_TEST
    if (ARITH_LOWEST_HTOP(p) == NULL) {
	if (SAVED_HEAP_TOP(p) != NULL) {
	    last_htop = SAVED_HEAP_TOP(p);
	}
	if (last_htop != NULL) {
	    ARITH_LOWEST_HTOP(p) = last_htop;
	}
    }
#endif
    bp->next = MBUF(p);
    MBUF(p) = bp;
    bp->size = n;
    MBUF_SIZE(p) += n;
    bp->off_heap.mso = NULL;
#ifndef SHARED_HEAP
    bp->off_heap.funs = NULL;
#endif
    bp->off_heap.externals = NULL;
    bp->off_heap.overhead = 0;

#ifdef HEAP_FRAG_ELIM_TEST
    /*
     * Unconditionally force a garbage collection.
     */
    MSO(p).overhead = HEAP_SIZE(p);
    BUMP_ALL_REDS(p);
#else
    /*
     * Test if time to do GC; if so bump the reduction count to force
     * a context switch.
     */
    MSO(p).overhead += (sizeof(ErlHeapFragment)/sizeof(Eterm) - 1); 
    if (((MBUF_SIZE(p) + MSO(p).overhead)*MBUF_GC_FACTOR) >= HEAP_SIZE(p)) {
	BUMP_ALL_REDS(p);
    }
#endif
    return bp->mem;
}

Eterm*
erts_arith_alloc(Process* p, Eterm* last_htop, Uint need)
{
    return do_alloc(p, last_htop, need);
}

Eterm*
erts_heap_alloc(Process* p, Uint need)
{
    Eterm* hp;

#ifdef HEAP_FRAG_ELIM_TEST
    if (need <= ARITH_AVAIL(p)) {
	Eterm* hp = ARITH_HEAP(p);

	ARITH_HEAP(p) += need;
	ARITH_AVAIL(p) -= need;
	return hp;
    }
#endif

#ifdef SHARED_HEAP
    if (p->htop == NULL) {
	if (need <= global_hend - global_htop) {
	    hp = global_htop;
	    global_htop += need;
	    return hp;
	} else if (global_htop != NULL) {
	    /*
	     * Garbage collect the global heap.
	     */
	    Process p;

	    p.htop = global_htop;
	    p.heap = global_heap;
	    p.hend = global_hend;
	    p.heap_sz = global_heap_sz;
	    p.send = NULL;
	    p.stop = NULL;
	    p.fvalue = NIL;
	    p.group_leader = NIL;
	    p.seq_trace_token = NIL;
	    p.dictionary = NULL;
	    p.debug_dictionary = NULL;
#ifdef HEAP_FRAG_ELIM_TEST
	    p.ssb = NULL;
#endif
	    p.status = 0;
	    p.flags = 0;
	    p.tracer_proc = NIL;
	    (void) erts_garbage_collect(&p, need, NULL, 0);
	    global_htop = p.htop;
	    global_heap = p.heap;
	    global_hend = p.hend;
	    global_heap_sz = p.heap_sz;
	    hp = global_htop;
	    global_htop += need;
	    return hp;
	} else {
	    return erts_global_alloc(need);
	}
    }
#endif
    hp = do_alloc(p, p->htop, need);

#ifdef HEAP_FRAG_ELIM_TEST
    if (SAVED_HEAP_TOP(p) == NULL) {
	SAVED_HEAP_TOP(p) = HEAP_TOP(p);
	HEAP_TOP(p) = HEAP_LIMIT(p);
	MSO(p).overhead = HEAP_SIZE(p);
	HALLOC_MBUF(p) = MBUF(p);
    }
    ARITH_AVAIL(p) = 0;
    ARITH_HEAP(p) = NULL;
#endif
    return hp;
}

#ifdef SHARED_HEAP
Eterm*
erts_global_alloc(Uint need)
{
    if (need <= global_hend - global_htop) {
	Eterm* hp = global_htop;
	global_htop += need;
	return hp;
    } else {
	/*
	 * Either there is no enough room on the global heap, or the heap pointers
	 * are "owned" by the running process.
	 */
	ErlHeapFragment* bp = (ErlHeapFragment*)
	    ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG,
			    sizeof(ErlHeapFragment) + ((need-1)*sizeof(Eterm)));
	bp->next = MBUF(dummy);
	MBUF(dummy) = bp;
	if (HALLOC_MBUF(dummy) == NULL) {
	    HALLOC_MBUF(dummy) = bp;
	}
	bp->size = need;
	MBUF_SIZE(dummy) += need;
	bp->off_heap.mso = NULL;
	bp->off_heap.externals = NULL;
	bp->off_heap.overhead = 0;
	MSO(dummy).overhead += (sizeof(ErlHeapFragment)/sizeof(Eterm) - 1); 
	return bp->mem;
    }
}
#endif

#if 0 /* XXX Not used! */
/* Does what arith_alloc does, but only ensures that the space is
   allocated; doesn't actually start using it. */
void arith_ensure_alloc(Process* p, Uint need)
{
    ErlHeapFragment* bp;
    Uint n;
    Eterm* hp;
#ifdef DEBUG
    Uint i;
#endif

    if (ARITH_AVAIL(p) >= need)
	return;

    n = (need < 128) ? 128 : need;
    bp = new_message_buffer(n+1);
    bp->next = MBUF(p);
    MBUF(p) = bp;
    MBUF_SIZE(p) += n+1;
    ARITH_AVAIL(p) = n;
    hp = bp->mem;
#ifdef DEBUG
    for (i = 0; i <= n; i++) {
	hp[i] = ARITH_MARKER;
    }
#endif
    ARITH_HEAP(p) = hp;
#ifdef DEBUG
    ARITH_CHECK_ME(p) = ARITH_HEAP(p);
#endif
}
#endif

/*
 * Helper function for the ESTACK macros defined in global.h.
 */

Eterm*
erl_grow_stack(Eterm* ptr, size_t new_size)
{
    if (new_size > 2 * DEF_ESTACK_SIZE) {
	return erts_realloc(ERTS_ALC_T_ESTACK, (void *) ptr, new_size);
    } else {
	Eterm* new_ptr = erts_alloc(ERTS_ALC_T_ESTACK, new_size);
	sys_memcpy(new_ptr, ptr, new_size/2);
	return new_ptr;
    }
}

/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/*
 * Generate the integer part from a double.
 */
Eterm
double_to_integer(Process* p, double x)
{
    int is_negative;
    int ds;
    digit_t* xp;
    int i;
    Eterm res;
    size_t sz;
    Eterm* hp;

    if ((x < (double) (MAX_SMALL+1)) && (x > (double) (MIN_SMALL-1))) {
	Sint xi = x;
	return make_small(xi);
    }

    if (x >= 0) {
	is_negative = 0;
    } else {
	is_negative = 1;
	x = -x;
    }

    /* Unscale & (calculate exponent) */
    ds = 0;
    while(x >= 1.0) {
	x /= D_BASE;         /* "shift" right */
	ds++;
    }
    sz = BIG_NEED_SIZE(ds);          /* number of words */

    /*
     * Beam note: This function is called from guard bifs (round/1 and trunc/1),
     * which are not allowed to build anything at all on the heap.
     * Therefore it is essential to use the ArithAlloc() macro instead of HAlloc().
     */
    hp = ArithAlloc(p, sz+1);
    res = make_big(hp);
    xp = (digit_t*) (hp + 1);

    for (i = ds-1; i >= 0; i--) {
	digit_t d;

	x *= D_BASE;      /* "shift" left */
	d = x;            /* trunc */
	xp[i] = d;        /* store digit */
	x -= d;           /* remove integer part */
    }
    while ((ds & (BIG_DIGITS_PER_WORD-1)) != 0) {
	xp[ds++] = 0;
    }

    if (is_negative) {
	*hp = make_neg_bignum_header(sz-1);
    } else {
	*hp = make_pos_bignum_header(sz-1);
    }
    return res;
}

Uint erts_tot_link_lh_size;

/*
 * Create a new link with ref.
 *
 * item: pid, port, atom, small, or NIL.
 * data: pid, port, atom, small, or NIL.
 * ref:  reference, or NIL
 */
ErlLink*
new_ref_link(ErlLink* next, ErlLinkType type, Eterm item, Eterm data, Eterm ref)
{
    /* item, data, and ref are allowed to be pids, ports, refs, or any
       immediate Erlang term */

#define CP_LINK_VAL(To, Hp, From)					\
do {									\
    if (IS_CONST(From))							\
	(To) = (From);							\
    else {								\
	Uint i__;							\
	Uint len__;							\
	ASSERT((Hp));							\
	ASSERT(is_internal_ref((From)) || is_external((From)));		\
	(To) = make_boxed((Hp));					\
	len__ = thing_arityval(*boxed_val((From))) + 1;			\
	for(i__ = 0; i__ < len__; i__++)				\
	    (*((Hp)++)) = boxed_val((From))[i__];			\
	if (is_external((To))) {					\
	    external_thing_ptr((To))->next = NULL;			\
	    external_thing_ptr((To))->node->refc++;			\
	}								\
    }									\
} while (0)

    ErlLink* lnk;
    Uint *hp;
    Uint link_size = ERL_LINK_SIZE;

    if(!IS_CONST(item))
	link_size += NC_HEAP_SIZE(item);
    if(!IS_CONST(data))
	link_size += NC_HEAP_SIZE(data);
    if(!IS_CONST(ref))
	link_size += NC_HEAP_SIZE(ref);

    ASSERT(link_size >= ERL_LINK_SIZE);

    ERTS_PROC_MORE_MEM(link_size);

    if (link_size == ERL_LINK_SIZE) {
	lnk = (ErlLink*) erts_alloc(ERTS_ALC_T_LINK,
				    link_size*sizeof(Uint));
	hp = NULL;
    }
    else if (link_size <= ERL_LINK_SH_SIZE) {
	lnk = (ErlLink*) erts_alloc(ERTS_ALC_T_LINK_SH,
				    link_size*sizeof(Uint));
	hp = lnk->heap;
    }
    else {
	lnk = (ErlLink*) erts_alloc(ERTS_ALC_T_LINK_LH,
				    link_size*sizeof(Uint));
	erts_tot_link_lh_size += link_size*sizeof(Uint);
	hp = lnk->heap;
    }

    lnk->next = next;
    lnk->type = type;

    CP_LINK_VAL(lnk->item, hp, item);
    CP_LINK_VAL(lnk->data, hp, data);
    CP_LINK_VAL(lnk->ref,  hp, ref);

    ASSERT(!hp || (Uint) hp <= (Uint) (lnk + link_size));
    ASSERT(next == lnk->next);
    ASSERT(type == lnk->type);
    ASSERT(EQ(item, lnk->item));
    ASSERT(EQ(data, lnk->data));
    ASSERT(EQ(ref,  lnk->ref));

    return lnk;

#undef CP_LINK_VAL

}

/*
 * Create a new link.
 */
ErlLink*
new_link(ErlLink* next, ErlLinkType type, Eterm item, Eterm data)
{
   return new_ref_link(next, type, item, data, NIL);
}

/*
** Delete an old link (and relink)
*/
void del_link(lnk)
ErlLink** lnk;
{
    ErlLink* tlink;
    ErlNode *node;
    Uint link_size;

    if (lnk != NULL) {
	tlink = *lnk;
	*lnk = tlink->next;
	link_size = ERL_LINK_SIZE;

	if (!IS_CONST(tlink->item)) {
	    link_size += NC_HEAP_SIZE(tlink->item);
	    if(is_external(tlink->item)) {
		node = external_thing_ptr(tlink->item)->node;
		DEREF_ERL_NODE(node);
	    }
	}
	if (!IS_CONST(tlink->data)) {
	    link_size += NC_HEAP_SIZE(tlink->data);
	    if(is_external(tlink->data)) {
		node = external_thing_ptr(tlink->data)->node;
		DEREF_ERL_NODE(node);
	    }
	}
	if (!IS_CONST(tlink->ref)) {
	    link_size += NC_HEAP_SIZE(tlink->ref);
	    if(is_external(tlink->ref)) {
		node = external_thing_ptr(tlink->ref)->node;
		DEREF_ERL_NODE(node);
	    }
	}

#ifdef DEBUG
	ASSERT(link_size >= ERL_LINK_SIZE);
	sys_memset((void *) tlink, 0x0f, link_size*sizeof(Uint));
#endif

	ERTS_PROC_LESS_MEM(link_size);

	if (link_size == ERL_LINK_SIZE)
	    erts_free(ERTS_ALC_T_LINK, (void *) tlink);
	else if (link_size <= ERL_LINK_SH_SIZE)
	    erts_free(ERTS_ALC_T_LINK_SH, (void *) tlink);
	else {
	    erts_tot_link_lh_size -= link_size*sizeof(Uint);
	    erts_free(ERTS_ALC_T_LINK_LH, (void *) tlink);
	}
    }
}

#ifdef DEBUG

static ErlLink** 
not_opt_find_link_by_ref(ErlLink** first, Eterm ref)
{
    ErlLink *lnk, *prev;

    ASSERT(is_ref(ref));

    for (prev = NULL, lnk = *first; lnk; prev = lnk, lnk = lnk->next)
	if (eq(lnk->ref, ref))
	    return (prev == NULL) ? first : &prev->next;
    return NULL;
}

#endif

/*
** Find a link, given the value of the ref field
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink** 
find_link_by_ref(ErlLink** first, Eterm ref)
{
    ErlLink *lnk, *prev;
    ErlNode *anode;
    Uint alen, blen;
    Uint32 *anum, *bnum;
    ErlLink **res;
#ifdef DEBUG
    ErlLink **not_opt_res = not_opt_find_link_by_ref(first, ref);
#endif

    if (is_internal_ref(ref)) {
	anode = internal_ref_node(ref);
	alen = internal_ref_no_of_numbers(ref);
	anum = internal_ref_numbers(ref);
    }
    else {
	ASSERT(is_external_ref(ref));
	anode = external_ref_node(ref);
	alen = external_ref_no_of_numbers(ref);
	anum = external_ref_numbers(ref);
    }

    ASSERT(alen > 0);

    for (prev = NULL, lnk = *first; lnk; prev = lnk, lnk = lnk->next) {
	/* Inlining of
	 * "if (eq(lnk->ref, ref))
	 *      return (prev == NULL) ? first : &prev->next;"
	 */

	if (is_nil(lnk->ref))
	    continue;

	if (is_internal_ref(lnk->ref)) {
	    ASSERT(internal_ref_no_of_numbers(lnk->ref) > 0);


	    bnum = internal_ref_numbers(lnk->ref);
	    if (anum[0] != bnum[0])
		continue;
	    if (anode != internal_ref_node(lnk->ref))
		continue;
	    blen = internal_ref_no_of_numbers(lnk->ref);
	}
	else {
	    ASSERT(is_external_ref(lnk->ref));
	    ASSERT(external_ref_no_of_numbers(lnk->ref) > 0);


	    bnum = external_ref_numbers(lnk->ref);
	    if (anum[0] != bnum[0])
		continue;
	    if (anode != external_ref_node(lnk->ref))
		continue;
	    blen = external_ref_no_of_numbers(lnk->ref);
	}


	if (alen == 3 && blen == 3) { /* Most refs are of length 3 */

	    if (anum[1] != bnum[1] || anum[2] != bnum[2])
		continue;

	found_it:
	    res = (prev == NULL) ? first : &prev->next;
	    ASSERT(not_opt_res == res);
	    return res;

	}
	else {
	    Uint common_len;
	    Uint i;

	    common_len = alen;
	    if (blen < alen)
		common_len = blen;

	    for (i = 1; i < common_len; i++)
		if (anum[i] != bnum[i])
		    continue;

	    if(alen != blen) {

		if (alen > blen) {
		    for (i = common_len; i < alen; i++)
			if (anum[i] != 0)
			    continue;
		}
		else {
		    for (i = common_len; i < blen; i++)
			if (bnum[i] != 0)
			    continue;
		}
		
	    }

	    goto found_it;
	}
    }

    ASSERT(not_opt_res == NULL);
    return NULL;
}

static ERTS_INLINE ErlLink**
gen_find_link(ErlLink** first, ErlLinkType type, Eterm item, Eterm data)
{
    ErlLink *lnk;
    ErlLink *prev;

    for (lnk = *first, prev = NULL; lnk; prev = lnk, lnk = lnk->next) {
	if (lnk->type != type)
	    continue;
	if (!EQ(lnk->item, item))
	    continue;
	if (is_nil(data) || EQ(lnk->data, data))
	    return (prev == NULL) ? first : &prev->next;
    }
    return NULL;
}

/*
** Find a link.
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink**
find_link(ErlLink** first, ErlLinkType type, Eterm item, Eterm data)
{
    ErlLink *lnk = *first;
    ErlLink *prev = NULL;
    ErlLink **res;

    /* First a couple of optimized special cases that are common... */
    
    if (is_immed(item)) {
	if (is_immed(data) || is_external_pid(data)) {
	    /* Local links, node monitors, and remote links in a dist entries
	       will be handled here. */
	    for (; lnk; prev = lnk, lnk = lnk->next) {
		if (lnk->type != type)
		    continue;
		if (lnk->item != item)
		    continue;
		if (is_nil(data)) {
		found_it:
		    res = (prev == NULL) ? first : &prev->next;
		    ASSERT(res == gen_find_link(first, type, item, data));
		    return res;
		}
		if (data == lnk->data)
		    goto found_it;
		if (is_not_external_pid(lnk->data))
		    continue;
		if (external_node(data) != external_node(lnk->data))
		    continue;
		if (external_pid_data(data) != external_pid_data(lnk->data))
		    continue;
		goto found_it;
	    }
	not_found:
	    ASSERT(NULL == gen_find_link(first, type, item, data));
	    return NULL;
	}
    }
    else if (is_external_pid(item) && is_nil(data)) {
	/* Remote links stored in process structs will be handled here. */
	for (; lnk; prev = lnk, lnk = lnk->next) {
	    if (lnk->type != type)
		continue;
	    if (is_not_external_pid(lnk->item))
		continue;
	    if (external_node(item) != external_node(lnk->item))
		continue;
	    if (external_pid_data(item) != external_pid_data(lnk->item))
		continue;
	    goto found_it;
	}
	goto not_found;
    }

    /* ... and then the general case. */
    return gen_find_link(first, type, item, data);
}

Uint
erts_link_size(ErlLink* elp)
{
    Uint size;
    if (!elp)
	return 0;
    size = ERL_LINK_SIZE*sizeof(Uint);
    if(!IS_CONST(elp->item))
	size += NC_HEAP_SIZE(elp->item)*sizeof(Uint);
    if(!IS_CONST(elp->data))
	size += NC_HEAP_SIZE(elp->data)*sizeof(Uint);
    if(!IS_CONST(elp->ref))
	size += NC_HEAP_SIZE(elp->ref)*sizeof(Uint);
    return size;
}

/*
 * Calculate length of a list.
 * Returns -1 if not a proper list (i.e. not terminated with NIL)
 */
int
list_length(Eterm list)
{
    int i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
	return -1;
    }
    return i;
}

Uint erts_fit_in_bits(Uint n)
{
   Uint i;

   i = 0;
   while (n > 0) {
      i++;
      n >>= 1;
   }
   return i;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Some Erlang term building utility functions (to be used when performance  *
 * isn't critical).                                                          *
 *                                                                           *
 * Add more functions like these here (and function prototypes in global.h)  *
 * when needed.                                                              *
 *                                                                           *
\*                                                                           */

Eterm
erts_bld_uint(Uint **hpp, Uint *szp, Uint ui)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, ui)) {
	if (hpp)
	    res = make_small(ui);
    }
    else {
	if (szp)
	    *szp += BIG_UINT_HEAP_SIZE;
	if (hpp) {
	    res = uint_to_big(ui, *hpp);
	    *hpp += BIG_UINT_HEAP_SIZE;
	}
    }
    return res;
}

Eterm
erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 2;
    if (hpp) {
	res = CONS(*hpp, car, cdr);
	*hpp += 2;
    }
    return res;
}

Eterm
erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...)
{
    Eterm res = THE_NON_VALUE;

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {

	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    va_list argp;

	    va_start(argp, arity);
	    for (i = 0; i < arity; i++)
		*((*hpp)++) = va_arg(argp, Eterm);
	    va_end(argp);
	}
    }
    return res;
}


Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[])
{
    Eterm res = THE_NON_VALUE;

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {

	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    for (i = 0; i < arity; i++)
		*((*hpp)++) = terms[i];
	}
    }
    return res;
}

Eterm
erts_bld_string(Uint **hpp, Uint *szp, char *str)
{
    Eterm res = THE_NON_VALUE;
    Sint i = strlen(str);
    if (szp)
	*szp += i*2;
    if (hpp) {
	res = NIL;
	while (--i >= 0) {
	    res = CONS(*hpp, make_small(str[i]), res);
	    *hpp += 2;
	}
    }
    return res;
}

Eterm
erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[])
{
    Eterm list = THE_NON_VALUE;
    if (szp)
	*szp += 2*length;
    if (hpp) {
	Sint i = length;
	list = NIL;

	while (--i >= 0) {
	    list = CONS(*hpp, terms[i], list);
	    *hpp += 2;
	}
    }
    return list;
}

Eterm
erts_bld_2tup_list(Uint **hpp, Uint *szp,
		   Sint length, Eterm terms1[], Uint terms2[])
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 5*length;
    if (hpp) {
	Sint i = length;
	res = NIL;

	while (--i >= 0) {
	    res = CONS(*hpp+3, TUPLE2(*hpp, terms1[i], terms2[i]), res);
	    *hpp += 5;
	}
    }
    return res;
}


/*                                                                           *\
 *                                                                           *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* make a hash index from an erlang term */

/*
** There are three hash functions.
** make_broken_hash: the one used for backward compatibility
** is called from the bif erlang:hash/2. Should never be used
** as it a) hashes only a part of binaries, b) hashes bignums really poorly,
** c) hashes bignums differently on different endian processors and d) hashes 
** small integers with different weights on different bytes.
**
** make_hash: A hash function that will give the same values for the same
** terms regardless of the internal representation. Small integers are 
** hashed using the same algorithm as bignums and bignums are hashed 
** independent of the CPU endianess. 
** Make_hash also hashes pids, ports and references like 32 bit numbers 
** (but with different constants). 
** make_hash() is called from the bif erlang:phash/2
**
** The idea behind the hash algorithm is to produce values suitable for 
** linear dynamic hashing. We cannot choose the range at all while hashing 
** (it's not even supplied to the hashing functions). The good old algorithm
** [H = H*C+X mod M, where H is the hash value, C is a "random" constant(or M),
** M is the range, preferably a prime, and X is each byte value] is therefore 
** modified to:
** H = H*C+X mod 2^32, where C is a large prime. This gives acceptable 
** "spreading" of the hashes, so that later modulo calculations also will give
** acceptable "spreading" in the range. 
** We really need to hash on bytes, otherwise the 
** upper bytes of a word will be less significant than the lower ones. That's 
** not acceptable at all. For internal use one could maybe optimize by using
** another hash function, that is less strict but faster. That is, however, not
** implemented.
**
** Short semi-formal description of make_hash:
**
** In make_hash, the number N is treated like this:
**  Abs(N) is hashed bytewise with the least significant byte, B(0), first.
**  The number of bytes (J) to calculate hash on in N is 
**  (the number of _32_ bit words needed to store the unsigned 
**   value of abs(N)) * 4.
**  X = FUNNY_NUMBER2
**  If N < 0, Y = FUNNY_NUMBER4 else Y = FUNNY_NUMBER3.
**  The hash value is Y*h(J) mod 2^32 where h(J) is calculated like
**  h(0) = <initial hash> 
**  h(i) = h(i-i)*X + B(i-1)
** The above should hold regardless of internal representation.
** Pids are hashed like small numbers but with differrent constants, as are
** ports.
** References are hashed like ports but only on the least significant byte.
** Binaries are hashed on all bytes (not on the 15 first as in 
** make_broken_hash()).
** Bytes in lists (possibly text strings) use a simpler multiplication inlined
** in the handling of lists, that is an optimization.
** Everything else is like in the old hash (make_broken_hash()).
**
** make_hash2() is faster than make_hash, in particular for bignums
** and binaries, and produces better hash values. 
*/

/* some prime numbers just above 2 ^ 28 */

#define FUNNY_NUMBER1  268440163
#define FUNNY_NUMBER2  268439161
#define FUNNY_NUMBER3  268435459
#define FUNNY_NUMBER4  268436141
#define FUNNY_NUMBER5  268438633
#define FUNNY_NUMBER6  268437017
#define FUNNY_NUMBER7  268438039
#define FUNNY_NUMBER8  268437511
#define FUNNY_NUMBER9  268439627
#define FUNNY_NUMBER10 268440479
#define FUNNY_NUMBER11 268440577

Uint32
make_hash(Eterm term, Uint32 hash)
{
    Uint32 x;
    Sint32 y;

    /* 
    ** Convenience macro for calculating a bytewise hash on an unsigned 32 bit 
    ** integer.
    ** If the endianess is known, we could be smarter here, 
    ** but that gives no significant speedup (on a sparc at least) 
    */
#define UINT32_HASH_RET(Expr, Prime1, Prime2)   			\
	do {								\
	    x = (Uint32) (Expr);	                                \
	    return							\
		(((((hash)*(Prime1) + (x & 0xFF)) * (Prime1) + 	        \
		((x >> 8) & 0xFF)) * (Prime1) + 			\
		((x >> 16) & 0xFF)) * (Prime1) + 			\
		 (x >> 24)) * (Prime2);				        \
	} while(0)

#define SINT32_HASH_RET(Expr, Prime1, Prime2, Prime3)	\
	do {						\
	    y = (Sint32) Expr;				\
	    if (y < 0) {				\
		UINT32_HASH_RET(-y, Prime1, Prime3);	\
	    } 						\
	    UINT32_HASH_RET(y, Prime1, Prime2);		\
	} while(0)
		
	    
    /* 
    ** Significant additions needed for real 64 bit port with larger fixnums.
    */	    
    ASSERT(SMALL_BITS <= 32);
    /* 
    ** Note, for the simple 64bit port, not utilizing the 
    ** larger word size this function will work without modification. 
    */

    switch (tag_val_def(term)) {
    case NIL_DEF:
	return hash*FUNNY_NUMBER3 + 1;

    case ATOM_DEF:
	return hash*FUNNY_NUMBER1 + 
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);

    case SMALL_DEF:
	SINT32_HASH_RET(signed_val(term), FUNNY_NUMBER2,
			FUNNY_NUMBER3, FUNNY_NUMBER4);

    case BINARY_DEF:
	{
	    byte* ptr;
	    unsigned sz = binary_size(term);
	    int i = sz;

	    GET_BINARY_BYTES(term, ptr);
	    while (i--) {
		hash = hash*FUNNY_NUMBER1 + *ptr++;
	    }
	    return hash*FUNNY_NUMBER4 + sz;
	}

    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;
	    Uint i;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_index;
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_uniq;
	    for (i = 0; i < num_free; i++) {
		hash = make_hash(funp->env[i], hash);
	    }
	    return hash;
	}

    case PID_DEF:
	UINT32_HASH_RET(internal_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);
    case EXTERNAL_PID_DEF:
	UINT32_HASH_RET(external_pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);

    case PORT_DEF:
	UINT32_HASH_RET(internal_port_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);
    case EXTERNAL_PORT_DEF:
	UINT32_HASH_RET(external_port_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);

    case REF_DEF:
	UINT32_HASH_RET(internal_ref_numbers(term)[0],FUNNY_NUMBER9,FUNNY_NUMBER10);
    case EXTERNAL_REF_DEF:
	UINT32_HASH_RET(external_ref_numbers(term)[0],FUNNY_NUMBER9,FUNNY_NUMBER10);

    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    return hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case LIST_DEF:
	{
	    Eterm* list = list_val(term);
	    while(1) {
		if (is_byte(*list)) {
		    /* Optimization for strings. 
		    ** Note that this hash is different from a 'small' hash,
		    ** as multiplications on a Sparc is so slow.
		    */

		    hash = hash*FUNNY_NUMBER2 + unsigned_val(*list);
		} else {
		    hash = make_hash(*list, hash);
		}
		if (is_not_list(CDR(list)))
		    return make_hash(CDR(list),hash)*FUNNY_NUMBER8;
		list = list_val(CDR(list));
	    }
	}
	break;

    case BIG_DEF:
	/* Note that this is the exact same thing as the hashing of smalls.*/
	{
	  /* FIXED: __alpha__ this was remade to be backwards +
	   * compaible with 32 bit implmentation 
	   */
	    Eterm* ptr  = big_val(term);
	    Uint n = BIG_SIZE(ptr);
	    int is_neg = BIG_SIGN(ptr);
	    Uint i;

	    if (n & 1) /* emulate 32 bit behaviour (add a MSB 0 :-( )*/
	      n++;

	    for (i = 0; i < n; i++)  {
	      digit_t d = BIG_DIGIT(ptr, i);
	    
	      hash = (hash*FUNNY_NUMBER2 + (d & 0xff))* FUNNY_NUMBER2 +
		(d >> 8);
	    }
	    if (is_neg) {
		return hash*FUNNY_NUMBER4;
	    }
	    else {
	      return hash*FUNNY_NUMBER3;
	    }
	}
	break;

    case TUPLE_DEF: 
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);
	    int i = arity;

	    ptr++;
	    while(i--)
		hash = make_hash(*ptr++, hash);
	    return hash*FUNNY_NUMBER9 + arity;
	}
	break;

    case VECTOR_DEF:
	{
	    Uint siz = (Uint) VECTOR_SIZE(term);
	    int i = 1;

	    for(i=1;i<=siz;++i)
		hash = make_hash(erts_unchecked_vector_get(i,term), hash);
	    return hash*FUNNY_NUMBER11 + siz;
	}
	break;
	

    default:
	erl_exit(1, "Invalid tag in make_hash(0x%X)\n", term);
	return 0;
    }
#undef UINT32_HASH_RET
#undef SINT32_HASH_RET
}

/* Hash function suggested by Bob Jenkins. */

#define MIX(a,b,c)                 \
do {                               \
  a -= b; a -= c; a ^= (c>>13);    \
  b -= c; b -= a; b ^= (a<<8);     \
  c -= a; c -= b; c ^= (b>>13);    \
  a -= b; a -= c; a ^= (c>>12);    \
  b -= c; b -= a; b ^= (a<<16);    \
  c -= a; c -= b; c ^= (b>>5);     \
  a -= b; a -= c; a ^= (c>>3);     \
  b -= c; b -= a; b ^= (a<<10);    \
  c -= a; c -= b; c ^= (b>>15);    \
} while(0)

#define HCONST 0x9e3779b9UL /* the golden ratio; an arbitrary value */

Uint32
block_hash(byte *k, unsigned length, Uint32 initval)
{
   Uint32 a,b,c;
   unsigned len;

   /* Set up the internal state */
   len = length;
   a = b = HCONST;
   c = initval;           /* the previous hash value */

   while (len >= 12)
   {
      a += (k[0] +((Uint32)k[1]<<8) +((Uint32)k[2]<<16) +((Uint32)k[3]<<24));
      b += (k[4] +((Uint32)k[5]<<8) +((Uint32)k[6]<<16) +((Uint32)k[7]<<24));
      c += (k[8] +((Uint32)k[9]<<8) +((Uint32)k[10]<<16)+((Uint32)k[11]<<24));
      MIX(a,b,c);
      k += 12; len -= 12;
   }

   c += length;
   switch(len)              /* all the case statements fall through */
   {
   case 11: c+=((Uint32)k[10]<<24);
   case 10: c+=((Uint32)k[9]<<16);
   case 9 : c+=((Uint32)k[8]<<8);
      /* the first byte of c is reserved for the length */
   case 8 : b+=((Uint32)k[7]<<24);
   case 7 : b+=((Uint32)k[6]<<16);
   case 6 : b+=((Uint32)k[5]<<8);
   case 5 : b+=k[4];
   case 4 : a+=((Uint32)k[3]<<24);
   case 3 : a+=((Uint32)k[2]<<16);
   case 2 : a+=((Uint32)k[1]<<8);
   case 1 : a+=k[0];
     /* case 0: nothing left to add */
   }
   MIX(a,b,c);
   return c;
}

Uint32
make_hash2(Eterm term)
{
    Uint32 hash;

/* (HCONST * {2, ..., 13}) mod 2^32 */
#define HCONST_2 0x3c6ef372UL
#define HCONST_3 0xdaa66d2bUL
#define HCONST_4 0x78dde6e4UL
#define HCONST_5 0x1715609dUL
#define HCONST_6 0xb54cda56UL
#define HCONST_7 0x5384540fUL
#define HCONST_8 0xf1bbcdc8UL
#define HCONST_9 0x8ff34781UL
#define HCONST_10 0x2e2ac13aUL
#define HCONST_11 0xcc623af3UL
#define HCONST_12 0x6a99b4acUL
#define HCONST_13 0x08d12e65UL

#define UINT32_HASH_2(Expr1, Expr2, AConst)       \
         do {                                     \
	    Uint32 a,b;                           \
	    a = AConst + (Uint32) (Expr1);        \
	    b = AConst + (Uint32) (Expr2);        \
	    MIX(a,b,hash);                        \
	 } while(0)

#define UINT32_HASH(Expr, AConst) UINT32_HASH_2(Expr, 0, AConst)

#define SINT32_HASH(Expr, AConst)                 \
	do {					  \
            Sint32 y = (Sint32) (Expr);           \
	    if (y < 0) {			  \
		UINT32_HASH(-y, AConst);          \
	    } 					  \
	    UINT32_HASH(y, AConst);          	  \
	} while(0)

    ASSERT(SMALL_BITS <= 32);

    /* Optimization. Simple cases before declaration of estack. */
    if (primary_tag(term) == TAG_PRIMARY_IMMED1) {
	switch (term & _TAG_IMMED1_MASK) {
	case _TAG_IMMED1_IMMED2:
	    switch (term & _TAG_IMMED2_MASK) {
	    case _TAG_IMMED2_ATOM:
		return atom_tab(atom_val(term))->slot.bucket.hvalue;
	    }
	    break;
	case _TAG_IMMED1_SMALL:
	    hash = 0;
	    SINT32_HASH(signed_val(term), HCONST);
	    return hash;
	}
    };
    {
    Eterm tmp;
    DECLARE_ESTACK(s);

    hash = 0;
    for (;;) {
	switch (primary_tag(term)) {
	case TAG_PRIMARY_LIST:
	{
	    int c = 0;
	    Uint32 s = 0;
	    Eterm* ptr = list_val(term);
	    while (is_byte(*ptr)) {
		/* Optimization for strings. */
		s = (s << 8) + unsigned_val(*ptr);
		if (c == 3) {
		    UINT32_HASH(s, HCONST_4);
		    c = s = 0;
		} else {
		    c++;
		}
		term = CDR(ptr);
		if (is_not_list(term))
		    break;
		ptr = list_val(term);
	    }
	    if (c > 0)
		UINT32_HASH(s, HCONST_4);
	    if (is_list(term)) {
		term = *ptr;
		tmp = *++ptr;
		ESTACK_PUSH(s, tmp);	    
	    }
	}
	break;
	case TAG_PRIMARY_BOXED:
	{
	    Eterm hdr = *boxed_val(term);
	    ASSERT(is_header(hdr));
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
	    {
		int i;
		int arity = header_arity(hdr);
		Eterm* elem = tuple_val(term);
		UINT32_HASH(arity, HCONST_9);
		if (arity == 0) /* Empty tuple */ 
		    goto hash2_common;
		for (i = arity; i >= 2; i--) {
		    tmp = elem[i];
		    ESTACK_PUSH(s, tmp);
		}
		term = elem[1];
	    }
	    break;
	    case VECTOR_SUBTAG:
	    {
		int i;
		int siz = VECTOR_SIZE(term);

		UINT32_HASH(siz, HCONST_8);
		if (siz == 0) 
		    goto hash2_common;
		for (i=siz; i > 1; i--) {
		    tmp = erts_unchecked_vector_get(i, term);
		    ESTACK_PUSH(s, tmp);
		}
		term = erts_unchecked_vector_get(1, term);
	    }
	    break;
	    case FUN_SUBTAG:
	    {
		ErlFunThing* funp = (ErlFunThing *) fun_val(term);
		Uint num_free = funp->num_free;

		UINT32_HASH_2
		    (num_free, 
		     atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue,
		     HCONST);
		UINT32_HASH_2
		    (funp->fe->old_index, funp->fe->old_uniq, HCONST);
		if (num_free == 0) {
		    goto hash2_common;
		} else {
		    Eterm* bptr = funp->env + num_free - 1;
		    while (num_free-- > 1) {
			term = *bptr--;
			ESTACK_PUSH(s, term);
		    }
		    term = *bptr;
		}
	    }
	    break;
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
	    {
		byte* bptr;
		unsigned sz = binary_size(term);
		Uint32 con = HCONST_13 + hash;
			
		GET_BINARY_BYTES(term, bptr);
		hash = (sz == 0) ? con : block_hash(bptr, sz, con);
		goto hash2_common;
	    }
	    break;
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
	    {
		Eterm* ptr = big_val(term);
		Uint i = 0;
		Uint n = BIG_SIZE(ptr);
		Uint32 con = BIG_SIGN(ptr) ? HCONST_10 : HCONST_11;

		do {
		    digit_t d1 = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    digit_t d2 = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    digit_t d3 = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    digit_t d4 = i < n ? BIG_DIGIT(ptr, i++) : 0;
		    UINT32_HASH_2((((Uint32) d2) << 16) + (Uint32) d1,
				  (((Uint32) d4) << 16) + (Uint32) d3, con);
		} while (i < n);
		goto hash2_common;
	    }
	    break;
	    case REF_SUBTAG:
		UINT32_HASH(internal_ref_numbers(term)[0], HCONST_7);
		goto hash2_common;
		break;
	    case EXTERNAL_REF_SUBTAG:
		UINT32_HASH(external_ref_numbers(term)[0], HCONST_7);
		goto hash2_common;
		break;
	    case EXTERNAL_PID_SUBTAG:
		UINT32_HASH(external_pid_number(term), HCONST_5);
		goto hash2_common;
	    case EXTERNAL_PORT_SUBTAG:
		UINT32_HASH(external_port_number(term), HCONST_6);
		goto hash2_common;
	    case FLOAT_SUBTAG:
	    {
		FloatDef ff;
		GET_DOUBLE(term, ff);
#if defined(WORDS_BIGENDIAN)
		UINT32_HASH_2(ff.fw[0], ff.fw[1], HCONST_12);
#else
		UINT32_HASH_2(ff.fw[1], ff.fw[0], HCONST_12);
#endif
		goto hash2_common;
	    }
	    break;
		    
	    default:
		erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
	    }
	}
	break;
	case TAG_PRIMARY_IMMED1:
	    switch (term & _TAG_IMMED1_MASK) {
	    case _TAG_IMMED1_PID:
		UINT32_HASH(internal_pid_number(term), HCONST_5);
		goto hash2_common;
	    case _TAG_IMMED1_PORT:
		UINT32_HASH(internal_port_number(term), HCONST_6);
		goto hash2_common;
	    case _TAG_IMMED1_IMMED2:
		switch (term & _TAG_IMMED2_MASK) {
		case _TAG_IMMED2_ATOM:
		    if (hash == 0)
			hash = atom_tab(atom_val(term))->slot.bucket.hvalue;
		    else
			UINT32_HASH(atom_tab(atom_val(term))->slot.bucket.hvalue,
				    HCONST_3);
		    goto hash2_common;
		case _TAG_IMMED2_NIL:
		    if (hash == 0)
			hash = 3468870702UL;
		    else
			UINT32_HASH(NIL_DEF, HCONST_2);
		    goto hash2_common;
		default:
		    erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
		}
	    case _TAG_IMMED1_SMALL:
		SINT32_HASH(signed_val(term), HCONST);
		goto hash2_common;
	    }
	    break;
	default:
	    erl_exit(1, "Invalid tag in make_hash2(0x%X)\n", term);
	hash2_common:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		return hash;
	    }
	    term = ESTACK_POP(s);
	}
    }
    }
#undef UINT32_HASH_2
#undef UINT32_HASH
#undef SINT32_HASH
}

#undef HCONST
#undef MIX

#ifdef ARCH_64
Uint32
make_broken_hash(Eterm term, Uint32 hash)
#else
Uint
make_broken_hash(Eterm term, Uint hash)
#endif
{
    switch (tag_val_def(term)) {
    case NIL_DEF:
	return hash*FUNNY_NUMBER3 + 1;
    case ATOM_DEF:
	return hash*FUNNY_NUMBER1 + 
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);
    case SMALL_DEF:
	return hash*FUNNY_NUMBER2 + unsigned_val(term);
    case BINARY_DEF:
	{
	    byte* ptr;
	    size_t sz = binary_size(term);
	    size_t i = (sz < 15) ? sz : 15;

	    GET_BINARY_BYTES(term, ptr);
	    while (i-- != 0) {
		hash = hash*FUNNY_NUMBER1 + *ptr++;
	    }
	    return hash*FUNNY_NUMBER4 + sz;
	}
    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;
	    Uint i;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(atom_val(funp->fe->module))->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_index;
	    hash = hash*FUNNY_NUMBER2 + funp->fe->old_uniq;
	    for (i = 0; i < num_free; i++) {
		hash = make_broken_hash(funp->env[i], hash);
	    }
	    return hash;
	}

    case PID_DEF:
	return hash*FUNNY_NUMBER5 + internal_pid_number(term);
    case EXTERNAL_PID_DEF:
	return hash*FUNNY_NUMBER5 + external_pid_number(term);

    case PORT_DEF:
	return hash*FUNNY_NUMBER9 + internal_port_number(term);
    case EXTERNAL_PORT_DEF:
	return hash*FUNNY_NUMBER9 + external_port_number(term);

    case REF_DEF:
	return hash*FUNNY_NUMBER9 + internal_ref_numbers(term)[0];
    case EXTERNAL_REF_DEF:
	return hash*FUNNY_NUMBER9 + external_ref_numbers(term)[0];

    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    return hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case LIST_DEF:
	{
	    Eterm* list = list_val(term);

	    while(1) {
		hash = make_broken_hash(*list, hash);
		if (is_not_list(CDR(list)))
		    return make_broken_hash(CDR(list),hash)*FUNNY_NUMBER8;
		list = list_val(CDR(list));
	    }
	}
	break;

    case BIG_DEF:
	{
	    Eterm* ptr  = big_val(term);
	    int is_neg = BIG_SIGN(ptr);
	    Uint arity = BIG_ARITY(ptr);
	    
#ifdef ARCH_64
	    Uint i = 0;
	    Uint n = BIG_SIZE(ptr);
	    ptr++;
	    arity = (n + 1) >> 1;

	    do {
#if defined(WORDS_BIGENDIAN)
		hash = hash*FUNNY_NUMBER2 + (Uint32) (((Uint) *ptr) >> 32);
		i += 2;
		if (i < n) {
		    hash = hash*FUNNY_NUMBER2 + (Uint32) *ptr;
		    i += 2;
		}
#else
		hash = hash*FUNNY_NUMBER2 + (Uint32) *ptr;
		i += 2;
		if (i < n) {
		    hash = hash*FUNNY_NUMBER2 + (Uint32) (((Uint) *ptr) >> 32);
		    i += 2;
		}
#endif
		ptr++; 
	    } while (i < n);
#else
	    int i = arity;

	    ptr++;
	    while (i--) {
		hash = hash*FUNNY_NUMBER2 + *ptr++;
	    }
#endif

	    if (is_neg)
		return hash*FUNNY_NUMBER3 + arity;
	    else
		return hash*FUNNY_NUMBER2 + arity;
	}
	break;

    case TUPLE_DEF: 
	{
	    Eterm* ptr = tuple_val(term);
	    Uint arity = arityval(*ptr);
	    int i = arity;

	    ptr++;
	    while(i--)
		hash = make_broken_hash(*ptr++, hash);
	    return hash*FUNNY_NUMBER9 + arity;
	}
	break;

    case VECTOR_DEF:
	{
	    Uint siz = (Uint) VECTOR_SIZE(term);
	    int i = 1;

	    for(i=1;i<=siz;++i)
		hash = make_broken_hash(erts_unchecked_vector_get(i,term), 
					hash);
	    return hash*FUNNY_NUMBER11 + siz;
	}
	break;
	
    default:
	erl_exit(1, "Invalid tag in make_broken_hash\n");
	return 0;
    }
}

static int do_send_to_logger(char *tag, Eterm gleader, char *buf, int len)
{
    /* error_logger ! 
       {notify,{info_msg,gleader,{emulator,"~s~n",[<message as list>]}}} |
       {notify,{error,gleader,{emulator,"~s~n",[<message as list>]}}} |
       {notify,{warning_msg,gleader,{emulator,"~s~n",[<message as list>}]}} */
    Process *p;
    Eterm atom_tag, atom_notify;
    Eterm* hp;
    Uint sz;
    Uint gl_sz;
    Eterm gl;
    Eterm list,plist,format,tuple1,tuple2,tuple3;
    
    if (len <= 0) {
	return -1;
    }
    if ((p = whereis_process(am_error_logger)) == NULL ||
	p->status == P_EXITING || p->status == P_RUNNING)  {
	/* Now, buf might not be null-terminated and it might be tmp_buf... */
	if (len >= TMP_BUF_SIZE) {
	    len = TMP_BUF_SIZE - 1;
	}
	sys_memmove(tmp_buf,buf,len);
	tmp_buf[len] = '\0';
	erl_printf(CERR,"(no error logger present) %s: %s\r\n",
		   tag,tmp_buf);
	return 0;
    }

    /* So we have an error logger, lets build the message */
    atom_tag = am_atom_put(tag,strlen(tag));
    atom_notify = am_atom_put("notify",6);
    gl_sz = IS_CONST(gleader) ? 0 : size_object(gleader);
    sz = len * 2 /* message list */+ 2 /* cons surrounding message list */
	+ gl_sz + 
	3 /*outher 2-tuple*/ + 4 /* middle 3-tuple */ + 4 /*inner 3-tuple */ +
	8 /* "~s~n" */;
    hp = HAlloc(p,sz);
    gl = (is_nil(gleader)
	  ? am_noproc
	  : (IS_CONST(gleader)
	     ? gleader
	     : copy_struct(gleader,gl_sz,&hp,&MSO(p))));
    list = buf_to_intlist(&hp, buf, len, NIL);
    plist = CONS(hp,list,NIL);
    hp += 2;
    format = buf_to_intlist(&hp, "~s~n", 4, NIL);
    tuple1 = TUPLE3(hp, am_emulator, format, plist);
    hp += 4;
    tuple2 = TUPLE3(hp, atom_tag, gl, tuple1);
    hp += 4;
    tuple3 = TUPLE2(hp, atom_notify, tuple2);
#ifdef HARDDEBUG
    display(tuple3,CERR);
#endif
    queue_message_tt(p, NULL, tuple3, NIL);
    return 0;
}

int erts_send_info_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger("info_msg",gleader,buf,len);
}

int erts_send_warning_to_logger(Eterm gleader, char *buf, int len) 
{
    char *tag;
    switch (erts_error_logger_warnings) {
    case am_info:
	tag = "info_msg";
	break;
    case am_warning:
	tag = "warning_msg";
	break;
    default:
	tag = "error";
	break;
    }
    return do_send_to_logger(tag,gleader,buf,len);
}

int erts_send_error_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger("error",gleader,buf,len);
}

/* To be removed, old obsolete interface */
int send_error_to_logger(Eterm gleader)
{
    return erts_send_error_to_logger(gleader,tmp_buf,cerr_pos) == 0;
}
/* eq and cmp are written as separate functions a eq is a little faster */

/*
 * Test for equality of two terms.
 * Returns 0 if not equal, or a non-zero value otherwise.
 */

int
eq(Eterm a, Eterm b)
{
 tailrecur:
    if (a == b) 
	return 1;

    switch (primary_tag(a)) {
    case TAG_PRIMARY_BOXED:
	{	
	    Eterm hdr = *boxed_val(a);
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    Eterm* aa;
		    Eterm* bb;
		    Sint i;
  
		    aa = tuple_val(a);
		    if (!is_boxed(b) || *boxed_val(b) != *aa)
			return 0;
		    bb = tuple_val(b);
		    i = arityval(*aa); /* get the arity*/
		    while (i--) {
			Eterm atmp = *++aa;
			Eterm btmp = *++bb;
			if (!EQ(atmp, btmp))
			    return 0;
		    }
		    return 1;
		}
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
		{
		    Uint size;
		    byte* a_ptr;
		    byte* b_ptr;

		    if (is_not_binary(b)) {
			return 0;
		    }
		    size = binary_size(a);
		    if (size != binary_size(b)) {
			return 0;
		    }
		    GET_BINARY_BYTES(a, a_ptr);
		    GET_BINARY_BYTES(b, b_ptr);
		    return sys_memcmp(a_ptr, b_ptr, size) == 0;
		}
	    case FUN_SUBTAG:
		{
		    ErlFunThing* f1;
		    ErlFunThing* f2;
		    int num_free;
		    int i;
  
		    if (is_not_fun(b))
			return 0;
		    f1 = (ErlFunThing *) fun_val(a);
		    f2 = (ErlFunThing *) fun_val(b);
		    if (f1->fe->module != f2->fe->module ||
			f1->fe->old_index != f2->fe->old_index ||
			f1->fe->old_uniq != f2->fe->old_uniq ||
			f1->num_free != f2->num_free) {
			return 0;
		    }
		    num_free = f1->num_free;
		    for (i = 0; i < num_free; i++) {
			if (!EQ(f1->env[i], f2->env[i])) {
			    return 0;
			}
		    }
		    return 1;
		}

	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG: {
		ExternalThing *ap;
		ExternalThing *bp;

		if(is_not_external(b))
		    return 0;

		ap = external_thing_ptr(a);
		bp = external_thing_ptr(b);

		if(ap->header != bp->header)
		    return 0;
		if(ap->node != bp->node)
		    return 0;

		ASSERT(1 == external_data_words(a));
		ASSERT(1 == external_data_words(b));

		if(ap->data[0] != bp->data[0])
		    return 0;
		return 1;
	    }
	    case EXTERNAL_REF_SUBTAG: {
		/*
		 * Observe!
		 *  When comparing refs we need to compare ref numbers
		 * (32-bit words) *not* ref data words.
		 */
		Uint32 *anum;
		Uint32 *bnum;
		Uint common_len;
		Uint alen;
		Uint blen;
		Uint i;

		if(is_not_external_ref(b))
		    return 0;

		if(external_node(a) != external_node(b))
		    return 0;

		anum = external_ref_numbers(a);
		bnum = external_ref_numbers(b);
		alen = external_ref_no_of_numbers(a);
		blen = external_ref_no_of_numbers(b);

		goto ref_common;
	    case REF_SUBTAG:
  
		    if (is_not_internal_ref(b))
			return 0;
		    alen = internal_ref_no_of_numbers(a);
		    blen = internal_ref_no_of_numbers(b);
		    anum = internal_ref_numbers(a);
		    bnum = internal_ref_numbers(b);

	    ref_common:
		    ASSERT(alen > 0 && blen > 0);

		    if (anum[0] != bnum[0])
			return 0;

		    if (alen == 3 && blen == 3) {
			/* Most refs are of length 3 */
			if (anum[1] == bnum[1] && anum[2] == bnum[2])
			    return 1;
			return 0;
		    }

		    common_len = alen;
		    if (blen < alen)
			common_len = blen;

		    for (i = 1; i < common_len; i++)
			if (anum[i] != bnum[i])
			    return 0;

		    if(alen != blen) {

			if (alen > blen) {
			    for (i = common_len; i < alen; i++)
				if (anum[i] != 0)
				    return 0;
			}
			else {
			    for (i = common_len; i < blen; i++)
				if (bnum[i] != 0)
				    return 0;
			}
			
		    }

		    return 1;
	    }
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
		{
		    Eterm* aa;
		    Eterm* bb;
		    int i;
  
		    if (is_not_big(b))
			return 0;
		    aa = big_val(a); /* get pointer to thing */
		    bb = big_val(b);
		    if (*aa != *bb)
			return 0;
		    i = BIG_ARITY(aa);
		    while(i--) {
			if (*++aa != *++bb)
			    return 0;
		    }
		    return 1;
		}
	    case FLOAT_SUBTAG:
		{
		    FloatDef af;
		    FloatDef bf;
  
		    if (is_not_float(b))
			return 0;
		    GET_DOUBLE(a, af);
		    GET_DOUBLE(b, bf);
		    return (af.fd == bf.fd) ? 1 : 0;
		}
	    case VECTOR_SUBTAG:
		{
		    int i;
		    int n;
		    if (is_not_vector(b)) {
			return 0;
		    }
		    n = VECTOR_SIZE(a);
		    if (n != VECTOR_SIZE(b)) {
			return 0;
		    }
		    for (i = 1; i <= n; i++) {
			Eterm atmp = erts_unchecked_vector_get(i, a);
			Eterm btmp = erts_unchecked_vector_get(i, b);
			if (!EQ(atmp, btmp)) {
			    return 0;
			}
		    }
		    return 1;
		}
	    }
	    break;
	}
    case TAG_PRIMARY_LIST:
	{
	    Eterm atmp;
	    Eterm btmp;

	    if (is_not_list(b))
		return 0;
	    atmp = CAR(list_val(a));
	    btmp = CAR(list_val(b));
	    if (!EQ(atmp, btmp))
		return 0;
	    a = CDR(list_val(a));
	    b = CDR(list_val(b));
	    goto tailrecur;
	}
    }
    return 0;
}

/* 
 * Lexically compare two strings of bytes (string s1 length l1 and s2 l2).
 *
 *	s1 < s2	return -1
 *	s1 = s2	return  0
 *	s1 > s2 return +1
 */
static int cmpbytes(byte *s1, int l1, byte *s2, int l2)
{
    int i;
    i = 0;
    while((i < l1) && (i < l2)) {
	if (s1[i] < s2[i]) return(-1);
	if (s1[i] > s2[i]) return(1);
	i++;
    }
    if (l1 < l2) return(-1);
    if (l1 > l2) return(1);
    return(0);
}


/*
 * Compare objects.
 * Returns 0 if equal, a negative value if a < b, or a positive number a > b.
 *
 * According to the Erlang Standard, types are orderered as follows:
 *   numbers < (characters) < atoms < refs < funs < ports < pids <
 *   tuples < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

static int cmp_atoms(Eterm a, Eterm b)
{
    Atom *aa = atom_tab(atom_val(a));
    Atom *bb = atom_tab(atom_val(b));
    int diff = aa->ord0 - bb->ord0;
    if (diff)
	return diff;
    return cmpbytes(aa->name+3, aa->len-3,
		    bb->name+3, bb->len-3);
}

int
cmp(Eterm a, Eterm b)
{
    Eterm* aa;
    Eterm* bb;
    int i;
    int j;
    int n;
    Eterm big_buf[2];
    int a_tag;
    int b_tag;

#undef  CMP_NODES
#define CMP_NODES(AN, BN)						\
    do {								\
	if((AN) != (BN)) {						\
	    if((AN)->sysname != (BN)->sysname)				\
		return cmp_atoms((AN)->sysname, (BN)->sysname);		\
	    ASSERT((AN)->creation != (BN)->creation);			\
	    return ((AN)->creation < (BN)->creation) ? -1 : 1;		\
	}								\
    } while (0)


 tailrecur:
    if (a == b) {		/* Equal values or pointers. */
	return 0;
    }

    /* deal with majority (?) cases by brute-force */
    if (is_atom(a)) {
	if (is_atom(b))
	    return cmp_atoms(a, b);
    } else if (is_both_small(a, b)) {
	return signed_val(a) - signed_val(b);
    }

    /*
     * Take care of cases where the types are the same.
     */

    a_tag = tag_val_def(a);
    switch (a_tag) {
    case TUPLE_DEF:
	if (is_not_tuple(b))
	    break;
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	i = arityval(*aa);	/* get the arity*/
	if (i < arityval(*bb)) return(-1);
	if (i > arityval(*bb)) return(1);
	while (i--) {
	    if ((j = cmp(*++aa, *++bb)) != 0) 
		return j;
	}
	return 0;
    case LIST_DEF:
	if (is_not_list(b))
	    break;
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = cmp(*aa++, *bb++)) != 0) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb)) {
		a = *aa;
		b = *bb;
		goto tailrecur;
	    }
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    case FLOAT_DEF:
	if (is_not_float(b))
	    break;
	{
	    FloatDef af;
	    FloatDef bf; 

	    GET_DOUBLE(a, af);
	    GET_DOUBLE(b, bf);
	    return float_comp(af.fd, bf.fd);
	}
    case REF_DEF: {
	/*
	 * Observe!
	 *  When comparing refs we need to compare ref numbers (32-bit words)
	 *  *not* ref data words.
	 */
	Uint alen;
	Uint blen;
	Sint i;
	Uint32 *anum;
	Uint32 *bnum;
	ErlNode *anode;
	ErlNode *bnode;

	if (is_internal_ref(b)) {
	    bnode = erts_this_node;
	    bnum = internal_ref_numbers(b);
	    blen = internal_ref_no_of_numbers(b);
	}
	else if(is_external_ref(b)) {
	    bnode = external_ref_node(b);
	    bnum = external_ref_numbers(b);
	    blen = external_ref_no_of_numbers(b);
	}
	else
	    break;

	anode = erts_this_node;
	anum = internal_ref_numbers(a);
	alen = internal_ref_no_of_numbers(a);

    ref_common:

	CMP_NODES(anode, bnode);

	ASSERT(alen > 0 && blen > 0);

	if(alen != blen) {
	    
	    if (alen > blen) {
		do {
		    if (anum[alen - 1] != 0)
			return 1;
		    alen--;
		} while (alen > blen);
	    }
	    else {
		do {
		    if (bnum[blen - 1] != 0)
			return -1;
		    blen--;
		} while (alen < blen);
	    }
	    
	}

	ASSERT(alen == blen);

	for (i = (Sint) alen - 1; i >= 0; i--)
	    if (anum[i] != bnum[i])
		return anum[i] < bnum[i] ? -1 : 1;

	return 0;

    case EXTERNAL_REF_DEF:

	if (is_internal_ref(b)) {
	    bnode = erts_this_node;
	    bnum = internal_ref_numbers(b);
	    blen = internal_ref_no_of_numbers(b);
	}
	else if(is_external_ref(b)) {
	    bnode = external_ref_node(b);
	    bnum = external_ref_numbers(b);
	    blen = external_ref_no_of_numbers(b);
	}
	else
	    break;

	anode = external_ref_node(a);
	anum = external_ref_numbers(a);
	alen = external_ref_no_of_numbers(a);

	goto ref_common;
    }
    case BIG_DEF:
	if (is_not_big(b))
	    break;
	return big_comp(a, b);
    case BINARY_DEF:
	if (is_not_binary(b))
	    break;
	{
	    Uint a_size = binary_size(a);
	    Uint b_size = binary_size(b);
	    Uint min_size;
	    int cmp;
	    byte* a_ptr;
	    byte* b_ptr;

	    min_size = (a_size < b_size) ? a_size : b_size;
	    GET_BINARY_BYTES(a, a_ptr);
	    GET_BINARY_BYTES(b, b_ptr);
	    if ((cmp = sys_memcmp(a_ptr, b_ptr, min_size)) != 0) {
		return cmp;
	    } else {
		return a_size - b_size;
	    }
	}
    case FUN_DEF:
	if (is_not_fun(b))
	    break;
	{
	    ErlFunThing* f1 = (ErlFunThing *) fun_val(a);
	    ErlFunThing* f2 = (ErlFunThing *) fun_val(b);
	    int num_free;
	    int diff;

	    diff = cmpbytes(atom_tab(atom_val(f1->fe->module))->name,
			    atom_tab(atom_val(f1->fe->module))->len,
			    atom_tab(atom_val(f2->fe->module))->name,
			    atom_tab(atom_val(f2->fe->module))->len);
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->fe->old_index - f2->fe->old_index;
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->fe->old_uniq - f2->fe->old_uniq;
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->num_free - f2->num_free;
	    if (diff != 0) {
		return diff;
	    }
	    num_free = f1->num_free;
	    for (i = 0; i < num_free; i++) {
		if ((diff = cmp(f1->env[i], f2->env[i])) != 0) {
		    return diff;
		}
	    }
	    return 0;
	}

    case PID_DEF: {
	Uint adata;
	Uint bdata;
	ErlNode *anode;
	ErlNode *bnode;

	if(is_internal_pid(b)) {
	    bnode = erts_this_node;
	    bdata = internal_pid_data(b);
	}
	else if (is_external_pid(b)) {
	    bnode = external_pid_node(b);
	    bdata = external_pid_data(b);
	}
	else
	    break;
	
	anode = erts_this_node;
	adata = internal_pid_data(a);

    pid_common:

	if (adata != bdata)
	    return adata < bdata ? -1 : 1;

	CMP_NODES(anode, bnode);

	return 0;

    case EXTERNAL_PID_DEF:

	if(is_internal_pid(b)) {
	    bnode = erts_this_node;
	    bdata = internal_pid_data(b);
	}
	else if (is_external_pid(b)) {
	    bnode = external_pid_node(b);
	    bdata = external_pid_data(b);
	}
	else
	    break;

	anode = external_pid_node(a);
	adata = external_pid_data(a);

	goto pid_common;

    case PORT_DEF:

	if(is_internal_port(b)) {
	    bnode = erts_this_node;
	    bdata = internal_port_data(b);
	}
	else if (is_external_port(b)) {
	    bnode = external_port_node(b);
	    bdata = external_port_data(b);
	}
	else
	    break;
	
	anode = erts_this_node;
	adata = internal_port_data(a);


    port_common:

	CMP_NODES(anode, bnode);

	if (adata != bdata)
	    return adata < bdata ? -1 : 1;

	return 0;

    case EXTERNAL_PORT_DEF:

	if(is_internal_port(b)) {
	    bnode = erts_this_node;
	    bdata = internal_port_data(b);
	}
	else if (is_external_port(b)) {
	    bnode = external_port_node(b);
	    bdata = external_port_data(b);
	}
	else
	    break;

	anode = external_port_node(a);
	adata = external_port_data(a);

	goto port_common;

    }
    case VECTOR_DEF:
	if (is_not_vector(b))
	    break;
	n = VECTOR_SIZE(a);
	if (n < VECTOR_SIZE(b)) return(-1);
	if (n > VECTOR_SIZE(b)) return(1);
	for (i = 1; i <= n; i++) {
	    Eterm atmp = erts_unchecked_vector_get(i, a);
	    Eterm btmp = erts_unchecked_vector_get(i, b);
	    if ((j = cmp(atmp, btmp)) != 0) {
		return j;
	    }
	}
	return 0;
    }

    b_tag = tag_val_def(b);

    /*
     * Take care of the case that the tags are different.
     */

    {
	FloatDef f1, f2;
	Eterm big;

	switch(_NUMBER_CODE(a_tag, b_tag)) {
	case SMALL_BIG:
	    big = small_to_big(signed_val(a), big_buf);
	    return big_comp(big, b);
	case SMALL_FLOAT:
	    f1.fd = signed_val(a);
	    GET_DOUBLE(b, f2);
	    return float_comp(f1.fd, f2.fd);
	case BIG_SMALL:
	    big = small_to_big(signed_val(b), big_buf);
	    return big_comp(a, big);
	case BIG_FLOAT:
	    if (big_to_double(a, &f1.fd) < 0) {
		return big_sign(a) ? -1 : 1;
	    }
	    GET_DOUBLE(b, f2);
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_SMALL:
	    GET_DOUBLE(a, f1);
	    f2.fd = signed_val(b);
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_BIG:
	    if (big_to_double(b, &f2.fd) < 0) {
		return big_sign(b) ? 1 : -1;
	    }
	    GET_DOUBLE(a, f1);
	    return float_comp(f1.fd, f2.fd);
	default:
	    return b_tag - a_tag;
	}
    }

#undef CMP_NODES

}

Process*
pid2proc(Eterm pid)
{
    Uint pix;
    Process *rp;
    if (is_not_internal_pid(pid))
	return NULL;
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes)
	return NULL;
    rp = process_tab[pix];
    if (INVALID_PID(rp, pid))
	return NULL;
    return rp;
}

void
erts_cleanup_externals(ExternalThing *etp)
{
    ExternalThing *tetp;

    tetp = etp;

    while(tetp) {
	DEREF_ERL_NODE(tetp->node);
	tetp = tetp->next;
    }
}

Eterm
store_external_or_ref_(Uint **hpp, ExternalThing **etpp, Eterm ns)
{
    Uint i;
    Uint size;
    Uint *from_hp;
    Uint *to_hp = *hpp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    if(is_external(ns)) {
	from_hp = external_val(ns);
	size = thing_arityval(*from_hp) + 1;
	*hpp += size;

	for(i = 0; i < size; i++)
	    to_hp[i] = from_hp[i];

	((ExternalThing *) to_hp)->node->refc++;

	((ExternalThing *) to_hp)->next = *etpp;
	*etpp = (ExternalThing *) to_hp;

	return make_external(to_hp);
    }

    /* Internal ref */
    from_hp = internal_ref_val(ns);

    size = thing_arityval(*from_hp) + 1;

    *hpp += size;

    for(i = 0; i < size; i++)
	to_hp[i] = from_hp[i];

    return make_internal_ref(to_hp);
}

Eterm
store_external_or_ref_in_proc_(Process *proc, Eterm ns)
{
    Uint sz;
    Uint *hp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    sz = NC_HEAP_SIZE(ns);
    ASSERT(sz > 0);
    hp = HAlloc(proc, sz);
    return store_external_or_ref_(&hp, &MSO(proc).externals, ns);
}

static int dcount;

/* 
 * Display a term.
 */

static int 
display1(Eterm obj, CIO fd)
{
    int i, k;
    Uint32 *ref_num;
    Eterm* nobj;

    if (dcount-- <= 0) return(1);

    if (is_CP(obj)) {
	erl_printf(fd, "<cp/header:%08lX>", (unsigned long) obj);
	return 0;
    }

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	erl_printf(fd, "[]");
	break;
    case ATOM_DEF:
	print_atom((int)atom_val(obj),fd);
	break;
    case SMALL_DEF:
	erl_printf(fd, "%d", signed_val(obj));
	break;
    case BIG_DEF:
	nobj = big_val(obj);
	i = BIG_SIZE(nobj);
	if (BIG_SIGN(nobj)) {
	    erl_printf(fd, "-16#", i);
	} else {
	    erl_printf(fd, "16#", i);
	}
	for (k = i-1; k >= 0; k--) {
	    erl_printf(fd, "%0X", BIG_DIGIT(nobj, k));
	}
	break;
    case REF_DEF:
	erl_printf(fd, "#Ref<%lu", internal_ref_channel_no(obj));
	ref_num = internal_ref_numbers(obj);
	for (i = internal_ref_no_of_numbers(obj)-1; i >= 0; i--)
	    erl_printf(fd, ".%lu", (unsigned long) ref_num[i]);
	erl_printf(fd, ">");
	break;
    case EXTERNAL_REF_DEF:
	erl_printf(fd, "#Ref<%lu", external_ref_channel_no(obj));
	ref_num = external_ref_numbers(obj);
	for (i = external_ref_no_of_numbers(obj)-1; i >= 0; i--)
	    erl_printf(fd, ".%lu", (unsigned long) ref_num[i]);
	erl_printf(fd, ">");
	break;
    case PID_DEF:
    case EXTERNAL_PID_DEF:
	erl_printf(fd, "<%lu.%lu.%lu>",
		   (unsigned long) pid_channel_no(obj),
		   (unsigned long) pid_number(obj),
		   (unsigned long) pid_serial(obj));
	break;
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:
	erl_printf(fd, "#Port<%lu.%lu>",
		   (unsigned long) port_channel_no(obj),
		   (unsigned long) port_number(obj));
	break;
    case LIST_DEF:
	if (is_printable_string(obj)) {
	   int c;
	   erl_putc('"', fd);
	   nobj = list_val(obj);
	   while (1) {
	      if (dcount-- <= 0) return(1);
	      c = signed_val(*nobj++);
	      if (c == '\n') {
		 erl_putc('\\', fd);
		 erl_putc('n', fd);
	      } else {
		 if (c == '"')
		    erl_putc('\\', fd);
		 erl_putc(c, fd);
	      }
	      if (is_not_list(*nobj)) break;
	      nobj = list_val(*nobj);
	   }
	   erl_putc('"', fd);
	} else {
	   erl_putc('[', fd);
	   nobj = list_val(obj);
	   while (1) {
	      if (display1(*nobj++, fd) != 0) return(1);
	      if (is_not_list(*nobj)) break;
	      erl_putc(',',fd);
	      nobj = list_val(*nobj);
	   }
	   if (is_not_nil(*nobj)) {
	      erl_putc('|', fd);
	      if (display1(*nobj, fd) != 0) return(1);
	   }
	   erl_putc(']', fd);
	}
	break;
    case TUPLE_DEF:
	nobj = tuple_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	erl_putc('{', fd);
	while (i--) {
	    if (display1(*++nobj,fd) != 0) return(1);
	    if (i >= 1) erl_putc(',',fd);
	}
	erl_putc('}',fd);
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
#ifdef _OSE_
	    erl_printf(fd, "%e", ff.fd);
#else
	    erl_printf(fd, "%e", ff.fd);
#endif
	}
	break;
    case BINARY_DEF:
	{
	    ProcBin* pb = (ProcBin *) binary_val(obj);
	    erl_printf(fd, pb->size == 1 ? "<<%lu byte>>" : "<<%lu bytes>>",
		       (unsigned long) pb->size);
	}
	break;
    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    Atom* ap;

	    erl_printf(fd, "#Fun<");
	    ap = atom_tab(atom_val(funp->fe->module));
	    for (i = 0; i < ap->len; i++) {
		erl_putc(ap->name[i], fd);
	    }
	    erl_printf(fd, ".%d.%d>", funp->fe->old_index,
		       funp->fe->old_uniq);
	}
	break;
    case VECTOR_DEF:
	erl_printf(fd, "#Vector<%ld>", (long) signed_val(vector_val(obj)[1]));
	break;
    default:
	erl_printf(fd, "<unknown:%lx>", (unsigned long) obj);
    }
    return(0);
}


/*
 * Display a term on file fd.
 * Only used by debugging rountines as Erlang formatting is 
 * done in the io module.
 */

void
display(Eterm obj, CIO fd)
{
    dcount = 100000;
    display1(obj, fd);
}


/* as above, but limit the number of items printed */
void ldisplay(Eterm obj, CIO fd, int count)
{
    dcount = count;
    display1(obj, fd);
    if (dcount <= 0) erl_printf(fd, "... "); /* Show that more items exist */
}


/* print a name doing what quoting is necessary */
static void print_name(byte *s, int n, CIO fd)
{
    
    int need_quote;
    int pos;
    byte *cpos;
    int c;

    if (n == 0) {
	erl_printf(fd, "''");
	return;
    }

    need_quote = 0;
    cpos = s;
    pos = n - 1;

    c = *cpos++;
    if (!IS_LOWER(c))
	need_quote++;
    else {
	while (pos--) {
	    c = *cpos++;
	    if (!IS_ALNUM(c) && (c != '_')) {
		need_quote++;
		break;
	    }
	}
    }
    cpos = s;
    pos = n;
    if (need_quote)
	erl_putc('\'',fd);
    while(pos--) {
	c = *cpos++;
	switch(c) {
	case '\'': erl_printf(fd, "\\'"); break;
	case '\\': erl_printf(fd, "\\\\"); break;
	case '\n': erl_printf(fd, "\\n"); break;
	case '\f': erl_printf(fd, "\\f"); break;
	case '\t': erl_printf(fd, "\\t"); break;
	case '\r': erl_printf(fd, "\\r"); break;
	case '\b': erl_printf(fd, "\\b"); break;
	case '\v': erl_printf(fd, "\\v"); break;
	default:
	    if (IS_CNTRL(c))
		erl_printf(fd, "\\%03o", c);
	    else
		erl_putc(c, fd);
	    break;
	}
    }
    if (need_quote) 
	erl_putc('\'',fd);
}

/* print the text of an atom with number i on open file descriptor fd */
void
print_atom(int i, CIO fd)
{
    if ((i < 0) || (i >= atom_table_size) ||  (atom_tab(i) == NULL)) {
	erl_printf(fd, "<bad atom index: %d>", i);
    }
    print_name(atom_tab(i)->name, atom_tab(i)->len, fd);
    dcount -= atom_tab(i)->len;
}

/* 
 *  member(X,Y)
 *  returns 0 if X is a member of list Y
 *  returns 1 if X is not a member of list Y
 *  returns 2 if Y is not a list or is a badly formed list
 */

int
member(Eterm x, Eterm y)
{
    Eterm* z;
    if (is_nil(y)) return(1); /* empty list */
    if (is_not_list(y)) return(2); /* bad argument */
    z = list_val(y);
    for (;;) {
	if (eq(*z, x)) return(0);
	if (is_nil(*(z + 1))) return(1); /* end of list */
	if (is_not_list(*(z + 1))) return(2); /* badly formed list */
	z = list_val(*(z + 1));
    }
}

void bin_write(fp,buf,sz)
CIO fp; byte* buf;
int sz;
{
    int i;

    for (i=0;i<sz;i++) {
	if (IS_DIGIT(buf[i]))
	    erl_printf(fp, "%d,", buf[i]);
	else if (IS_PRINT(buf[i])) {
	    erl_putc(buf[i],fp);
	    erl_putc(',',fp);
	}
	else
	    erl_printf(fp,"%d,", buf[i]);
    }
    erl_putc('\n',fp);
}

/* Fill buf with the contents of bytelist list 
   return number of chars in list or -1 for error */

int
intlist_to_buf(Eterm list, byte *buf, int len)
{
    Eterm* listptr;
    int sz = 0;

    if (is_nil(list)) 
	return 0;
    if (is_not_list(list))
	return -1;
    listptr = list_val(list);

    while (sz < len) {
	if (!is_byte(*listptr)) 
	    return -1;
	buf[sz++] = unsigned_val(*listptr);
	if (is_nil(*(listptr + 1)))
	    return(sz);
	if (is_not_list(*(listptr + 1))) 
	    return -1;
	listptr = list_val(*(listptr + 1));
    }
    return -1;			/* not enough space */
}

/*
** Convert an integer to a byte list buf must have at least 12 bytes avaiable
** return pointer to converted stuff (need not to be at start of buf!)
*/
char* int_to_buf(n, buf)
int n; char* buf;
{
    char* p = buf+11;
    int sign = 0;

    *p-- = '\0'; /* null terminate */
    if (n == 0)
	*p-- = '0';
    else if (n < 0) {
	sign = 1;
	n = -n;
    }

    while (n != 0) {
	*p-- = (n % 10) + '0';
	n /= 10;
    }
    if (sign)
	*p-- = '-';
    return p+1;
}

/* Build a list of integers in some safe memory area
** Memory must be pre allocated prio call 2*len in size
** hp is a pointer to the "heap" pointer on return
** this pointer is updated to point after the list
*/

Eterm
buf_to_intlist(Eterm** hpp, byte *buf, int len, Eterm tail)
{
    Eterm* hp = *hpp;

    buf += (len-1);
    while(len > 0) {
	tail = CONS(hp, make_small((byte)*buf), tail);
	hp += 2;
	buf--;
	len--;
    }
    *hpp = hp;
    return tail;
}

/*
** write io list in to a buffer.
**
** A iolist is defined as:
**
** iohead ::= Binary
**        |   Byte (i.e integer in range [0..255]
**        |   iolist
**        ;
**
** iotail ::= []
**        |   Binary  (added by tony)
**        |   iolist
**        ;
**
** iolist ::= []
**        |   Binary
**        |   [ iohead | iotail]
**        ;
** 
** Return remaing bytes in buffer on succsess
**        -1 on overflow
**        -2 on type error
*/

int io_list_to_buf(Eterm obj, char* buf, int len)
{
    Eterm* objp;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		len--;
	    } else if (is_binary(obj)) {
		byte* bytes;
		size_t size = binary_size(obj);
		if (len < size) {
		    goto L_overflow;
		}
		GET_BINARY_BYTES(obj, bytes);
		sys_memcpy(buf, bytes, size);
		buf += size;
		len -= size;
	    }
	    else if (is_nil(obj)) {
		;
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    }
	    else
		goto L_type_error;

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		byte* bytes;
		size_t size = binary_size(obj);
		if (len < size) {
		    goto L_overflow;
		}
		GET_BINARY_BYTES(obj, bytes);
		sys_memcpy(buf, bytes, size);
		buf += size;
		len -= size;
	    }
	    else if (is_nil(obj))
		;
	    else
		goto L_type_error;
	} else if (is_binary(obj)) {
	    byte* bytes;
	    size_t size = binary_size(obj);
	    if (len < size) {
		goto L_overflow;
	    }
	    GET_BINARY_BYTES(obj, bytes);
	    sys_memcpy(buf, bytes, size);
	    buf += size;
	    len -= size;
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

int io_list_len(Eterm obj)
{
    Eterm* objp;
    int len = 0;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    /* Head */
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		len++;
	    } else if (is_binary(obj)) {
		len += binary_size(obj);
	    } else if (is_nil(obj)) {
		;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else {
		goto L_type_error;
	    }
	    /* Tail */
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		len += binary_size(obj);
	    } else if (is_nil(obj)) {
		;
	    } else {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) { /* Tail was binary */
	    len += binary_size(obj);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -1;
}
/*
int
io_list_len(Eterm list)
{
    int len = 0;
    int i;

    while (is_list(list)) {
	Eterm* cons = list_val(list);
	Eterm obj = CAR(cons);

	list = CDR(cons);
	if (is_byte(obj)) {
	    len++;
	} else if (is_list(obj)) {
	    if ((i = io_list_len(obj)) < 0)
		return i;
	    len += i;
	} else if (is_binary(obj)) {
	    len += binary_size(obj);
	} else if (!is_nil(obj)) {
	    return -1;
	}
    }
    if (is_nil(list))
	return len;
    else if (is_binary(list)) {
	return len + binary_size(list);
    } else {
	return -1;
    }
}
*/
/* return 0 if item is not a non-empty flat list of bytes */

int
is_string(Eterm list)
{
    int len = 0;

    while(is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

/* return 0 if item is not a non-empty flat list of printable characters */

static int
is_printable_string(Eterm list)
{
    int len = 0;
    int c;

    while(is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	c = signed_val(hd);
	/* IS_PRINT || IS_SPACE would be another way to put it */
	if (IS_CNTRL(c) && !IS_SPACE(c))
	   return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

Uint erts_sys_misc_mem_sz;

static Sint trim_threshold;
static Sint top_pad;
static Sint mmap_threshold;
static Sint mmap_max;

Uint tot_bin_allocated;

void erts_init_utils(void)
{

}

void erts_init_utils_mem(void) 
{
    erts_sys_misc_mem_sz = 0;
    trim_threshold = -1;
    top_pad = -1;
    mmap_threshold = -1;
    mmap_max = -1;
}

int
sys_alloc_opt(int opt, int value)
{
#if HAVE_MALLOPT
  Sint m_opt;
  Sint *curr_val;

  switch(opt) {
  case SYS_ALLOC_OPT_TRIM_THRESHOLD:
#ifdef M_TRIM_THRESHOLD
    m_opt = M_TRIM_THRESHOLD;
    curr_val = &trim_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_TOP_PAD:
#ifdef M_TOP_PAD
    m_opt = M_TOP_PAD;
    curr_val = &top_pad;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_THRESHOLD:
#ifdef M_MMAP_THRESHOLD
    m_opt = M_MMAP_THRESHOLD;
    curr_val = &mmap_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_MAX:
#ifdef M_MMAP_MAX
    m_opt = M_MMAP_MAX;
    curr_val = &mmap_max;
    break;
#else
    return 0;
#endif
  default:
    return 0;
  }

  if(mallopt(m_opt, value)) {
    *curr_val = (Sint) value;
    return 1;
  }

#endif /* #if HAVE_MALLOPT */

  return 0;
}

void
sys_alloc_stat(SysAllocStat *sasp)
{
   sasp->trim_threshold = trim_threshold;
   sasp->top_pad        = top_pad;
   sasp->mmap_threshold = mmap_threshold;
   sasp->mmap_max       = mmap_max;

}

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(buf,sz)
byte* buf;
int sz;
{
    bin_write(CERR,buf,sz);
}

void pat(Eterm atom)
{
    upp(atom_tab(atom_val(atom))->name,
	atom_tab(atom_val(atom))->len);
}


void pinfo()
{
    process_info(COUT);
}


void pp(p)
Process *p;
{
    if(p)
	print_process_info(p,CERR);
}
    
void ppi(Eterm pid)
{
    pp(pid2proc(pid));
}

void td(Eterm x)
{
    display(x, CERR);
    erl_putc('\n', CERR);
}

void
ps(Process* p, Eterm* stop)
{
    Eterm* sp = STACK_START(p) - 1;

    if (stop <= STACK_END(p)) {
        stop = STACK_END(p) + 1;
    }

    while(sp >= stop) {
	erl_printf(COUT,"%08lx: ", (unsigned long) (Eterm) sp);
	ldisplay(*sp, COUT, 75);
	erl_putc('\r', COUT);
	erl_putc('\n', COUT);
	sp--;
    }
}
#endif
