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
#include "erl_db.h"
#include "beam_catches.h"
#include "erl_binary.h"
#include "erl_nmgc.h"

/*
 * Returns number of elements in an array.
 */
#define ALENGTH(a) (sizeof(a)/sizeof(a[0]))

#ifdef HEAP_FRAG_ELIM_TEST

#define IS_MOVED(x)	(!is_header((x)))

#define MOVE_CONS(PTR,CAR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
									\
    HTOP[0] = CAR;		/* copy car */				\
    HTOP[1] = PTR[1];		/* copy cdr */				\
    gval = make_list(HTOP);	/* new location */			\
    *ORIG = gval;		/* redirect original reference */	\
    PTR[0] = THE_NON_VALUE;	/* store forwarding indicator */	\
    PTR[1] = gval;		/* store forwarding address */		\
    HTOP += 2;			/* update tospace htop */		\
} while(0)

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
    Sint nelts;								\
									\
    ASSERT(is_header(HDR));						\
    gval = make_boxed(HTOP);						\
    *ORIG = gval;							\
    *HTOP++ = HDR;							\
    *PTR++ = gval;							\
    nelts = header_arity(HDR);						\
    switch ((HDR) & _HEADER_SUBTAG_MASK) {				\
    case SUB_BINARY_SUBTAG: nelts++; break;				\
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR-1))->num_free+1; break;	\
    }									\
    while (nelts--)							\
	*HTOP++ = *PTR++;						\
} while(0)

#define in_area(ptr,start,nbytes) \
 ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

#ifdef SHARED_HEAP
# define STACK_SZ_ON_HEAP(p) 0
# define OverRunCheck() \
    if (HEAP_END(p) < HEAP_TOP(p)) { \
        erl_exit(1, "%s: Overrun heap at line %d\n", print_pid(p),__LINE__); \
    }
#else
# define STACK_SZ_ON_HEAP(p) ((p)->hend - (p)->stop)
# define OverRunCheck() \
    if (p->stop < p->htop) { \
        erl_exit(1, "%s: Overrun stack and heap at line %d\n", print_pid(p),__LINE__); \
    }
#endif

/*
 * This structure describes the rootset for the GC.
 */
typedef struct roots {
    Eterm* v;		/* Pointers to vectors with terms to GC
			 * (e.g. the stack).
			 */
    Uint sz;		/* Size of each vector. */
} Roots;

typedef struct {
    Roots def[32];		/* Default storage. */
    Roots* roots;		/* Pointer to root set array. */
    Uint size;			/* Storage size. */
} Rootset;

static Uint setup_rootset(Process*, Eterm*, int, Rootset*);
static void cleanup_rootset(Rootset *rootset);
static void remove_message_buffers(Process* p);
static int major_collection(Process* p, int need, Eterm* objv, int nobj);
static int minor_collection(Process* p, int need, Eterm* objv, int nobj);
static void do_minor(Process *p, int new_sz, Eterm* objv, int nobj);
static Eterm* sweep_one_area(Eterm* n_hp, Eterm* n_htop, char* src, Uint src_size);
static Eterm* sweep_old_heap(Process* p, Eterm* n_hp, Eterm* n_htop,
			     char* src, Uint src_size);
static Eterm* collect_heap_frags(Process* p, Eterm* heap,
				 Eterm* htop, Eterm* objv, int nobj);
static void adjust_after_fullsweep(Process *p, int size_before,
				   int need, Eterm *objv, int nobj);
static void grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj);
static void sweep_proc_bins(Process *p, int fullsweep);
static void sweep_proc_funs(Process *p, int fullsweep);
static void sweep_proc_externals(Process *p, int fullsweep);
static void offset_heap(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size);
static void offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size);
static void offset_rootset(Process *p, int offs, char* area, Uint area_size,
			   Eterm* objv, int nobj);
static void offset_off_heap(Process* p, int offs, char* area, Uint area_size);
static void offset_mqueue(Process *p, Sint offs, char* area, Uint area_size);
static char* print_pid(Process *p);
#ifdef DEBUG
static int within(Eterm *ptr, Process *p);
#endif
#endif /* HEAP_FRAG_ELIM_TEST */

#ifdef ARCH_64
# define MAX_HEAP_SIZES 150
#else
# define MAX_HEAP_SIZES 50
#endif

static Sint heap_sizes[MAX_HEAP_SIZES];	/* Suitable heap sizes. */
static int num_heap_sizes;	/* Number of heap sizes. */

/*
 * Initialize GC global data.
 */
void
erts_init_gc(void)
{
    int i = 0;

    switch (heap_series) {
    case HS_FIBONACCI:
	heap_sizes[0] = 34;
	heap_sizes[1] = 55;
	for (i = 2; i < ALENGTH(heap_sizes) && heap_sizes[i-1] < MAX_SMALL; i++) {
	    heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2];
	}
	break;
 
    case HS_FIBONACCI_SLOW:
	{
	    /*
	     * Fib growth is not really ok for really large heaps, for
	     * example is fib(35) == 14meg, whereas fib(36) == 24meg;
	     * we really don't want that growth when the heaps are that big.
	     */
	    
	    heap_sizes[0] = 34;
	    heap_sizes[1] = 55;
	    for (i = 2; i < 23; i++) {
		heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2];
	    }
	    
	    /* At 1.3 mega words heap, we start to slow down. */
	    for (i = 23; i < ALENGTH(heap_sizes) && heap_sizes[i-1] < MAX_SMALL; i++) {
		heap_sizes[i] = 5*heap_sizes[i-1]/4;
		if (heap_sizes[i] < 0) {
		    /* Size turned negative. Discard this last size. */
		    i--;
		    break;
		}
	    }
	}
	break;
    case HS_POWER_TWO:
	heap_sizes[0] = 32;
	for (i = 1; i < ALENGTH(heap_sizes) && heap_sizes[i-1] < MAX_SMALL; i++) {
	    heap_sizes[i] = 2 * heap_sizes[i-1];
	}
	break;
    case HS_POWER_TWO_MINUS_ONE:
	heap_sizes[0] = 31;
	for (i = 1; i < ALENGTH(heap_sizes) && heap_sizes[i-1] < MAX_SMALL; i++) {
	    heap_sizes[i] = 2 * (heap_sizes[i-1]+1) - 1;
	}
	break;
    }
    num_heap_sizes = i;
}

/*
 * Find the next heap size equal to or greater than the given size (if offset == 0).
 *
 * If offset is 1, the next higher heap size is returned (always greater than size).
 */
Uint
erts_next_heap_size(Uint size, Uint offset)
{
    if (size < heap_sizes[0]) {
	return heap_sizes[0];
    } else {
	Sint* low = heap_sizes;
	Sint* high = heap_sizes + num_heap_sizes;
	Sint* mid;

	while (low < high) {
	    mid = low + (high-low) / 2;
	    if (size < mid[0]) {
		high = mid;
	    } else if (size == mid[0]) {
		ASSERT(mid+offset-heap_sizes < num_heap_sizes);
		return mid[offset];
	    } else if (size < mid[1]) {
		ASSERT(mid[0] < size && size <= mid[1]);
		ASSERT(mid+offset-heap_sizes < num_heap_sizes);
		return mid[offset+1];
	    } else {
		low = mid + 1;
	    }
	}
	erl_exit(1, "no next heap size found: %d, offset %d\n", size, offset);
    }
    return 0;
}

Eterm
erts_heap_sizes(Process* p)
{
    int i;
    Eterm res = NIL;
    Eterm* hp = HAlloc(p, num_heap_sizes * 2);

    for (i = num_heap_sizes-1; i >= 0; i--) {
        res = CONS(hp, make_small(heap_sizes[i]), res);
        hp += 2;
    }
    return res;
}

#ifdef HEAP_FRAG_ELIM_TEST

/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of Eterm words needed on the heap.
 * objv: Array of terms to add to rootset; that is to preserve.
 * nobj: Number of objects in objv.
 */
int
erts_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    int done = 0;
    Uint saved_status = p->status;
    Uint ms1, s1, us1;

    p->status = P_GARBING;
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_start);
    }
    if (erts_system_monitor_long_gc != 0) get_now(&ms1, &s1, &us1);
    if (SAVED_HEAP_TOP(p) != NULL) {
	HEAP_TOP(p) = SAVED_HEAP_TOP(p);
	SAVED_HEAP_TOP(p) = NULL;
    }
    OverRunCheck();
    if (GEN_GCS(p) >= MAX_GEN_GCS(p)) {
        FLAGS(p) |= F_NEED_FULLSWEEP;
    }

    /*
     * Test which type of GC to do.
     */
    while (!done) {
	if ((FLAGS(p) & F_NEED_FULLSWEEP) != 0) {
	    done = major_collection(p, need, objv, nobj);
	} else {
	    done = minor_collection(p, need, objv, nobj);
	}
    }

    p->status = saved_status;
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_end);
    }
    if (erts_system_monitor_long_gc != 0) {
	Uint ms2, s2, us2;
	Sint t;
	get_now(&ms2, &s2, &us2);
	t = ms2 - ms1;
	t = t*1000000 + s2 - s1;
	t = t*1000 + (us2 - us1)/1000;
	if (t > 0 && (Uint)t > erts_system_monitor_long_gc) {
	    monitor_long_gc(p, t);
	}
    }

    garbage_cols++;
    ARITH_LOWEST_HTOP(p) = (Eterm *) 0;
    ARITH_AVAIL(p) = 0;
    ARITH_HEAP(p) = NULL;
    MSO(p).overhead = 0;
#ifdef DEBUG
    ARITH_CHECK_ME(p) = NULL;
#endif
    return ((int) (HEAP_TOP(p) - HEAP_START(p)) / 10);
}

static int
minor_collection(Process* p, int need, Eterm* objv, int nobj)
{
    /*
     * Allocate an old heap if we don't have one and if we'll need one.
     */

    if (OLD_HEAP(p) == NULL && HIGH_WATER(p) != HEAP_START(p)) {
        Eterm* n_old;

        /* Note: We choose a larger heap size than strictly needed,
         * which seems to reduce the number of fullsweeps.
         * This improved Estone by more than 1200 estones on my computer
         * (Ultra Sparc 10).
         */
        size_t new_sz = erts_next_heap_size(HEAP_TOP(p) - HEAP_START(p), 1);

        /* Create new, empty old_heap */
        n_old = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
					  sizeof(Eterm)*new_sz);

        OLD_HEND(p) = n_old + new_sz;
        OLD_HEAP(p) = OLD_HTOP(p) = n_old;
    }

    /*
     * Do a minor collection if the old heap is large enough.
     */

    if (HEAP_TOP(p) - HEAP_START(p) <= OLD_HEND(p) - OLD_HTOP(p)) {
	Uint size_after;
	Uint need_after;
	Uint stack_size = STACK_SZ_ON_HEAP(p);
	Uint size_before = MBUF_SIZE(p) + (HEAP_TOP(p) - HEAP_START(p));

        do_minor(p, erts_next_heap_size(HEAP_SIZE(p) + MBUF_SIZE(p), 0),
		 objv, nobj);
        GEN_GCS(p)++;
        size_after = HEAP_TOP(p) - HEAP_START(p);
        need_after = size_after + need + stack_size;
        reclaimed += (size_before - size_after);
	
        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */

        if ((HEAP_SIZE(p) > 300) && (4 * need_after < HEAP_SIZE(p)) &&
            ((HEAP_SIZE(p) > 8000) ||
             (HEAP_SIZE(p) > (OLD_HEND(p) - OLD_HEAP(p))))) {
	    Uint wanted = 3 * need_after;
            if (wanted < MIN_HEAP_SIZE(p)) {
                wanted = MIN_HEAP_SIZE(p);
            } else {
                wanted = erts_next_heap_size(wanted, 0);
            }
            if (wanted < HEAP_SIZE(p)) {
                erts_shrink_new_heap(p, wanted, objv, nobj);
            }
            ASSERT(HEAP_SIZE(p) == erts_next_heap_size(HEAP_SIZE(p), 0));
	    return 1;		/* We are done. */
        }

        if (HEAP_SIZE(p) >= need_after) {
	    /*
	     * The heap size turned out to be just right. We are done.
	     */
            ASSERT(HEAP_SIZE(p) == erts_next_heap_size(HEAP_SIZE(p), 0));
            return 1;
        }
    }

    /*
     * Still not enough room after minor collection. Must force a major collection.
     */
    FLAGS(p) |= F_NEED_FULLSWEEP;
    return 0;
}

static void
do_minor(Process *p, int new_sz, Eterm* objv, int nobj)
{
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Roots* roots;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    char* heap = (char *) HEAP_START(p);
    Uint heap_size = (char *) HEAP_TOP(p) - heap;
    Uint mature_size = (char *) HIGH_WATER(p) - heap;
    Eterm* old_htop = OLD_HTOP(p);
    Eterm* n_heap;

    n_htop = n_heap = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
					       sizeof(Eterm)*new_sz);

    if (MBUF(p) != NULL) {
	n_htop = collect_heap_frags(p, n_heap, n_htop, objv, nobj);
    }

    n = setup_rootset(p, objv, nobj, &rootset);
    roots = rootset.roots;

    while (n--) {
        Eterm* g_ptr = roots->v;
        Uint g_sz = roots->sz;

	roots++;
        while (g_sz--) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
                val = *ptr;
                if (IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr++ = val;
                } else if (in_area(ptr, heap, mature_size)) {
                    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
                } else if (in_area(ptr, heap, heap_size)) {
                    ASSERT(within(ptr, p));
                    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
                } else {
		    g_ptr++;
		}
                break;
	    }

	    case TAG_PRIMARY_LIST: {
                ptr = list_val(gval);
                val = *ptr;
                if (is_non_value(val)) { /* Moved */
                    *g_ptr++ = ptr[1];
                } else if (in_area(ptr, heap, mature_size)) {
                    MOVE_CONS(ptr,val,old_htop,g_ptr++);
                } else if (in_area(ptr, heap, heap_size)) {
                    ASSERT(within(ptr, p));
                    MOVE_CONS(ptr,val,n_htop,g_ptr++);
                } else {
		    g_ptr++;
		}
		break;
	    }

	    default:
                g_ptr++;
		break;
            }
        }
    }

    cleanup_rootset(&rootset);

    /*
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */

    if (mature_size == 0) {
	n_htop = sweep_one_area(n_heap, n_htop, heap, heap_size);
    } else {
	Eterm* n_hp = n_heap;

	while (n_hp != n_htop) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *n_hp;

	    switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else if (in_area(ptr, heap, mature_size)) {
		    MOVE_BOXED(ptr,val,old_htop,n_hp++);
		} else if (in_area(ptr, heap, heap_size)) {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (is_non_value(val)) {
		    *n_hp++ = ptr[1];
		} else if (in_area(ptr, heap, mature_size)) {
		    MOVE_CONS(ptr,val,old_htop,n_hp++);
		} else if (in_area(ptr, heap, heap_size)) {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_HEADER: {
		if (header_is_thing(gval))
		    n_hp += (thing_arityval(gval)+1);
		else
		    n_hp++;
		break;
	    }
	    default:
		n_hp++;
		break;
	    }
	}
    }

    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) new_heap.
     */

    if (OLD_HTOP(p) < old_htop) {
	old_htop = sweep_old_heap(p, OLD_HTOP(p), old_htop, heap, heap_size);
    }
    OLD_HTOP(p) = old_htop;
    HIGH_WATER(p) = (HEAP_START(p) != HIGH_WATER(p)) ? n_heap : n_htop;

    if (MSO(p).mso) {
        sweep_proc_bins(p, 0);
    }

    if (MSO(p).funs) {
        sweep_proc_funs(p, 0);
    }
    if (MSO(p).externals) {
        sweep_proc_externals(p, 0);
    }
    remove_message_buffers(p);

    /* Copy stack to end of new heap */
    n = p->hend - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
    p->stop = n_heap + new_sz - n;

#ifdef DEBUG
    sys_memset(HEAP_START(p), 0xf7, HEAP_SIZE(p) * sizeof(Eterm));
#endif
    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void*)HEAP_START(p),
		   HEAP_SIZE(p) * sizeof(Eterm));
    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_heap + new_sz;
}

static int
major_collection(Process* p, int need, Eterm* objv, int nobj)
{
    Rootset rootset;
    Roots* roots;
    int size_before;
    Eterm* n_heap;
    Eterm* n_htop;
    Eterm* old_heap = NULL;
    Eterm* old_htop = NULL;
    char* src = (char *) HEAP_START(p);
    Uint src_size = (char *) HEAP_TOP(p) - src;
    char* oh = (char *) OLD_HEAP(p);
    Uint oh_size = (char *) OLD_HTOP(p) - oh;
    Uint new_oh_size = 0;
    int n;
    Uint new_sz;

    size_before = MBUF_SIZE(p) + (HEAP_TOP(p) - HEAP_START(p));

    /*
     * Do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    new_sz = HEAP_SIZE(p) + MBUF_SIZE(p);
    new_sz += STACK_SZ_ON_HEAP(p);
    new_sz = erts_next_heap_size(new_sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (new_sz == HEAP_SIZE(p) && FLAGS(p) & F_HEAP_GROW) {
        new_sz = erts_next_heap_size(HEAP_SIZE(p), 1);
    }
    FLAGS(p) &= ~(F_HEAP_GROW|F_NEED_FULLSWEEP);
    n_htop = n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
						sizeof(Eterm)*new_sz);

    /*
     * Allocate a new old_heap if there are any data on the old old_heap.
     */

    if (oh_size != 0) {
	new_oh_size = erts_next_heap_size(oh_size+new_sz, 0);
	old_heap = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
					    sizeof(Eterm)*new_oh_size);
	old_htop = old_heap;
    }

    /*
     * Get rid of heap fragments.
     */

    if (MBUF(p) != NULL) {
	n_htop = collect_heap_frags(p, n_heap, n_htop, objv, nobj);
    }

    n = setup_rootset(p, objv, nobj, &rootset);
    roots = rootset.roots;
    while (n--) {
	Eterm* g_ptr = roots->v;
	Eterm g_sz = roots->sz;

	roots++;
	while (g_sz--) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *g_ptr;
	    
	    switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *g_ptr++ = val;
		} else if (in_area(ptr, src, src_size)) {
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
		} else if (in_area(ptr, oh, oh_size)) {
		    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
		} else {
		    g_ptr++;
		}
		continue;
	    }

	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (is_non_value(val)) {
		    *g_ptr++ = ptr[1];
		} else if (in_area(ptr, src, src_size)) {
		    ASSERT(within(ptr, p));
		    MOVE_CONS(ptr,val,n_htop,g_ptr++);
		} else if (in_area(ptr, oh, oh_size)) {
		    MOVE_CONS(ptr,val,old_htop,g_ptr++);
		} else {
		    g_ptr++;
		}
		continue;
	    }

	    default: {
		g_ptr++;
		continue;
	    }
	    }
	}
    }

    cleanup_rootset(&rootset);

    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
     */

    if (oh_size == 0) {
	n_htop = sweep_one_area(n_heap, n_htop, src, src_size);
    } else {
	Eterm* n_hp = n_heap;

	while (n_hp != n_htop) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *n_hp;

	    switch (primary_tag(gval)) {
	    case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else if (in_area(ptr, src, src_size)) {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
		} else if (in_area(ptr, oh, oh_size)) {
		    MOVE_BOXED(ptr,val,old_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (is_non_value(val)) {
		    *n_hp++ = ptr[1];
		} else if (in_area(ptr, src, src_size)) {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		} else if (in_area(ptr, oh, oh_size)) {
		    MOVE_CONS(ptr,val,old_htop,n_hp++);
		} else {
		    n_hp++;
		}
		break;
	    }
	    case TAG_PRIMARY_HEADER: {
		if (header_is_thing(gval))
		    n_hp += (thing_arityval(gval)+1);
		else
		    n_hp++;
		break;
	    }
	    default:
		n_hp++;
		break;
	    }
	}
    }
    
    if (oh_size != 0) {
	old_htop = sweep_one_area(old_heap, old_htop, oh, oh_size);
    }

    if (MSO(p).mso) {
	sweep_proc_bins(p, 1);
    }
    if (MSO(p).funs) {
	sweep_proc_funs(p, 1);
    }
    if (MSO(p).externals) {
	sweep_proc_externals(p, 1);
    }
    remove_message_buffers(p);

    if (OLD_HEAP(p) != NULL) {
#ifdef DEBUG
	sys_memset(OLD_HEAP(p), 0xf5,
		   (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
#endif
	ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       OLD_HEAP(p),
		       (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
	OLD_HEAP(p) = old_heap;
	OLD_HTOP(p) = old_htop;
	OLD_HEND(p) = old_heap + new_oh_size;
    }

    /* Move the stack to the end of the heap */
    n = HEAP_END(p) - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
    p->stop = n_heap + new_sz - n;

#ifdef DEBUG
    sys_memset(HEAP_START(p), 0xf3,
	       (HEAP_END(p) - HEAP_START(p)) * sizeof(Eterm));
#endif
    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   (HEAP_END(p) - HEAP_START(p)) * sizeof(Eterm));

    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_heap + new_sz;
    GEN_GCS(p) = 0;
    HIGH_WATER(p) = HEAP_TOP(p);

    adjust_after_fullsweep(p, size_before, need, objv, nobj);
    OverRunCheck();
    return 1;			/* We are done. */
}

static void
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
    int stack_size = STACK_SZ_ON_HEAP(p);
    
    size_after = (HEAP_TOP(p) - HEAP_START(p));
    reclaimed += (size_before - size_after);
    
    /*
     * Resize the heap if needed.
     */
    
    need_after = size_after + need + stack_size;
    if (HEAP_SIZE(p) < need_after) {
        /* Too small - grow to match requested need */
        sz = erts_next_heap_size(need_after, 0);
        grow_new_heap(p, sz, objv, nobj);
    } else if (3 * HEAP_SIZE(p) < 4 * need_after){
        /* Need more than 75% of current, postpone to next GC.*/
        FLAGS(p) |= F_HEAP_GROW;
    } else if (4 * need_after < HEAP_SIZE(p) && HEAP_SIZE(p) > H_MIN_SIZE){
        /* We need less than 25% of the current heap, shrink.*/
        /* XXX - This is how it was done in the old GC:
           wanted = 4 * need_after;
           I think this is better as fullsweep is used mainly on
           small memory systems, but I could be wrong... */
        wanted = 2 * need_after;
        if (wanted < p->min_heap_size) {
            sz = p->min_heap_size;
        } else {
            sz = erts_next_heap_size(wanted, 0);
        }
        if (sz < HEAP_SIZE(p)) {
            erts_shrink_new_heap(p, sz, objv, nobj);
        }
    }
}

/*
 * Remove all message buffers.
 */
static void
remove_message_buffers(Process* p)
{
    ErlHeapFragment* bp = MBUF(p);

    MBUF(p) = NULL;
    HALLOC_MBUF(p) = NULL;
    MBUF_SIZE(p) = 0;
    while (bp != NULL) {
	ErlHeapFragment* next_bp = bp->next;
#ifdef DEBUG
	sys_memset(bp->mem, 0xf9, bp->size*sizeof(Eterm));
#endif 
	free_message_buffer(bp);
	bp = next_bp;
    }
}

/*
 * Go through one root set array, move everything that it is one of the
 * heap fragments to our new heap.
 */
static Eterm*
collect_root_array(Process* p, Eterm* n_htop, Eterm* objv, int nobj)
{
    ErlHeapFragment* qb;
    char* heap_part = (char *) ARITH_LOWEST_HTOP(p);
    Uint heap_part_size;
    Eterm gval;
    Eterm* ptr;
    Eterm val;

    ASSERT(p->htop != NULL);
    ASSERT(heap_part != NULL);
    heap_part_size = (char *)p->htop - heap_part;

    while (nobj--) {
	gval = *objv;
	
	switch (primary_tag(gval)) {

	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED(val)) {
		ASSERT(is_boxed(val));
		*objv++ = val;
	    } else {
		if (in_area(ptr, heap_part, heap_part_size)) {
		    MOVE_BOXED(ptr,val,n_htop,objv);
		} else {
		    for (qb = MBUF(p); qb != NULL; qb = qb->next) {
			if (in_area(ptr, qb->mem, qb->size*sizeof(Eterm))) {
			    MOVE_BOXED(ptr,val,n_htop,objv);
			    break;
			}
		    }
		}
		objv++;
	    }
	    break;
	}

	case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (is_non_value(val)) {
		*objv++ = ptr[1];
	    } else {
		if (in_area(ptr, heap_part, heap_part_size)) {
		    MOVE_CONS(ptr,val,n_htop,objv);
		} else {
		    for (qb = MBUF(p); qb != NULL; qb = qb->next) {
			if (in_area(ptr, qb->mem, qb->size*sizeof(Eterm))) {
			    MOVE_CONS(ptr,val,n_htop,objv);
			    break;
			}
		    }
		}
		objv++;
	    }
	    break;
	}

	default: {
	    objv++;
	    break;
	}
	}
    }
    return n_htop;
}

static Eterm*
sweep_one_area(Eterm* n_hp, Eterm* n_htop, char* src, Uint src_size)
{
    while (n_hp != n_htop) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *n_hp;

	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED(val)) {
		ASSERT(is_boxed(val));
		*n_hp++ = val;
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
	    } else {
		n_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (is_non_value(val)) {
		*n_hp++ = ptr[1];
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_CONS(ptr,val,n_htop,n_hp++);
	    } else {
		n_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
	    if (header_is_thing(gval))
		n_hp += (thing_arityval(gval)+1);
	    else
		n_hp++;
	    break;
	}
	default:
	    n_hp++;
	    break;
	}
    }
    return n_htop;
}

static Eterm*
sweep_old_heap(Process* p, Eterm* o_hp, Eterm* o_htop, char* src, Uint src_size)
{
    while (o_hp != o_htop) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *o_hp;

	switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (IS_MOVED(val)) {
		ASSERT(is_boxed(val));
		*o_hp++ = val;
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_BOXED(ptr,val,o_htop,o_hp++);
	    } else {
		o_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (is_non_value(val)) {
		*o_hp++ = ptr[1];
	    } else if (in_area(ptr, src, src_size)) {
		MOVE_CONS(ptr,val,o_htop,o_hp++);
	    } else {
		o_hp++;
	    }
	    break;
	}
	case TAG_PRIMARY_HEADER: {
	    if (header_is_thing(gval))
		o_hp += (thing_arityval(gval)+1);
	    else
		o_hp++;
	    break;
	}
	default:
	    o_hp++;
	    break;
	}
    }
    return o_htop;
}

/*
 * Collect heap fragments and check that they point in the correct direction.
 */

static Eterm*
collect_heap_frags(Process* p, Eterm* n_hstart, Eterm* n_htop,
		   Eterm* objv, int nobj)
{
    ErlHeapFragment* qb;
    char* frag_begin;
    Uint frag_size;
    ErlMessage* mp;
    ErlHeapFragment* halloc_mbuf;

    /*
     * Go through the root set, move everything that it is in one of the
     * heap fragments to our new heap.
     */

    if (nobj != 0) {
	n_htop = collect_root_array(p, n_htop, objv, nobj);
    }
    if (is_not_immed(p->fvalue)) {
	n_htop = collect_root_array(p, n_htop, &p->fvalue, 1);
    }
    if (is_not_immed(p->ftrace)) {
	n_htop = collect_root_array(p, n_htop, &p->ftrace, 1);
    }
    if (is_not_immed(p->seq_trace_token)) {
	n_htop = collect_root_array(p, n_htop, &p->seq_trace_token, 1);
    }
    if (is_not_immed(p->group_leader)) {
	n_htop = collect_root_array(p, n_htop, &p->group_leader, 1);
    }
    if (p->dictionary != NULL) {
	n_htop = collect_root_array(p, n_htop,
				    p->dictionary->data,
				    p->dictionary->used);
    }
    if (p->debug_dictionary != NULL) {
	n_htop = collect_root_array(p, n_htop,
				    p->debug_dictionary->data,
				    p->debug_dictionary->used);
    }

    n_htop = collect_root_array(p, n_htop, p->stop, STACK_START(p) - p->stop);

    /*
     * Go through the message queue, move everything that it is in one of the
     * heap fragments to our new heap.
     */

    for (mp = p->msg.first; mp != NULL; mp = mp->next) {
	if (is_not_immed(ERL_MESSAGE_TERM(mp)) || is_not_immed(ERL_MESSAGE_TOKEN(mp))) {
	    n_htop = collect_root_array(p, n_htop, mp->m, 2);
	}
    }

    /*
     * Now all references in the root set point to the new heap. However,
     * many references on the new heap point to heap fragments.
     *
     * We must scan the heap once for every heap framgent. Order:
     *
     * 1. All heap fragments allocated by HAlloc().
     * 2. The part of the heap that may contain pointers into ArithAlloc'ed
     *    heap fragments.
     * 3. All heap fragments allocated by ArithAlloc().
     */

    ASSERT(ARITH_LOWEST_HTOP(p) <= p->htop);
    qb = MBUF(p);
    if ((halloc_mbuf = HALLOC_MBUF(p)) != NULL) {
	/*
	 * Sweep using all heap fragments allocated by HAlloc().
	 */
	for (;;) {
	    frag_begin = (char *) qb->mem;
	    frag_size = qb->size * sizeof(Eterm);
	    n_htop = sweep_one_area(n_hstart, n_htop, frag_begin, frag_size);
	    if (qb == halloc_mbuf) {
		qb = qb->next;
		break;
	    }
	    qb = qb->next;
	}
    }

    /*
     * Sweep using part of the heap as source.
     */

    frag_begin = (char *) ARITH_LOWEST_HTOP(p);
    if (frag_begin == NULL) {
	frag_size = 0;
    } else {
	frag_size = (char *)p->htop - frag_begin;
    }
    if (frag_size != 0) {
	n_htop = sweep_one_area(n_hstart, n_htop, frag_begin, frag_size);
    }

    /*
     * Sweep using the remaining heap fragments (allocated by ArithAlloc()).
     */

    while (qb != NULL) {
	frag_begin = (char *) qb->mem;
	frag_size = qb->size * sizeof(Eterm);
	if (frag_size != 0) {
	    n_htop = sweep_one_area(n_hstart, n_htop, frag_begin, frag_size);
	}
	qb = qb->next;
    }
    return n_htop;
}

static Uint ERTS_INLINE
add_to_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset, Uint n)
{
    Uint avail;
    Roots* roots;
    ErlMessage* mp;

    roots = rootset->roots;
    if (rootset->size < n+7) {
	Uint new_size = 2*rootset->size;
	if (roots == rootset->def) {
	    roots = erts_alloc(ERTS_ALC_T_ROOTSET, new_size*sizeof(Roots));
	    sys_memcpy(roots, rootset->def, sizeof(rootset->def));
	} else {
	    roots = erts_realloc(ERTS_ALC_T_ROOTSET,
				 (void *) roots,
				 new_size*sizeof(Roots));
	}
	rootset->size = new_size;
    }

    roots[n].v  = p->stop;
    roots[n].sz = STACK_START(p) - p->stop;
    ++n;

    if (p->dictionary != NULL) {
        roots[n].v = p->dictionary->data;
        roots[n].sz = p->dictionary->used;
        ++n;
    }
    if (p->debug_dictionary != NULL) {
        roots[n].v  = p->debug_dictionary->data;
        roots[n].sz = p->debug_dictionary->used;
        ++n;
    }
    if (nobj > 0) {
        roots[n].v  = objv;
        roots[n].sz = nobj;
        ++n;
    }

    ASSERT((is_nil(p->seq_trace_token) ||
	    is_tuple(p->seq_trace_token) ||
	    is_atom(p->seq_trace_token)));
    if (is_not_immed(p->seq_trace_token)) {
	roots[n].v = &p->seq_trace_token;
	roots[n].sz = 1;
	n++;
    }

    ASSERT(is_nil(p->tracer_proc) ||
	   is_internal_pid(p->tracer_proc) ||
	   is_internal_port(p->tracer_proc));

    ASSERT(is_pid(p->group_leader));
    if (is_not_immed(p->group_leader)) {
	roots[n].v  = &p->group_leader;
	roots[n].sz = 1;
	n++;
    }

    /*
     * The process may be garbage-collected while it is terminating.
     * (fvalue contains the EXIT reason and ftrace the saved stack trace.)
     */
    if (is_not_immed(p->fvalue)) {
	roots[n].v  = &p->fvalue;
	roots[n].sz = 1;
	n++;
    }
    if (is_not_immed(p->ftrace)) {
	roots[n].v  = &p->ftrace;
	roots[n].sz = 1;
	n++;
    }

    mp = p->msg.first;
    avail = rootset->size - n;
    while (mp != NULL) {
	if (avail == 0) {
	    Uint new_size = 2*rootset->size;
	    if (roots == rootset->def) {
		roots = erts_alloc(ERTS_ALC_T_ROOTSET,
				   new_size*sizeof(Roots));
		sys_memcpy(roots, rootset->def, sizeof(rootset->def));
	    } else {
		roots = erts_realloc(ERTS_ALC_T_ROOTSET,
				     (void *) roots,
				     new_size*sizeof(Roots));
	    }
	    rootset->size = new_size;
	    avail = new_size - n;
	}
	roots[n].v = mp->m;
	roots[n].sz = 2;
	n++;
        mp = mp->next;
	avail--;
    }
    rootset->roots = roots;
    return n;
}

static Uint
setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset)
{
    rootset->roots = rootset->def;
    rootset->size = ALENGTH(rootset->def);
    return add_to_rootset(p, objv, nobj, rootset, 0);
}

static
void cleanup_rootset(Rootset* rootset)
{
    if (rootset->roots != rootset->def) {
        erts_free(ERTS_ALC_T_ROOTSET, rootset->roots);
    }
}

static void
grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP(p) - HEAP_START(p);
    int stack_size = p->hend - p->stop;
    Sint32 offs;

    ASSERT(HEAP_SIZE(p) < new_sz);
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void *) HEAP_START(p),
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);

    if ((offs = new_heap - HEAP_START(p)) == 0) { /* No move. */
        HEAP_END(p) = new_heap + new_sz;
        sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
        p->stop = p->hend - stack_size;
    } else {
	char* area = (char *) HEAP_START(p);
	Uint area_size = (char *) HEAP_TOP(p) - area;
        Eterm* prev_stop = p->stop;

        offset_heap(new_heap, heap_size, offs, area, area_size);
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));

        HEAP_END(p) = new_heap + new_sz;
        prev_stop = new_heap + (p->stop - p->heap);
        p->stop = p->hend - stack_size;
        sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));

        offset_rootset(p, offs, area, area_size, objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
}

void
erts_shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP(p) - HEAP_START(p);
    Sint offs;

    int stack_size = p->hend - p->stop;

    ASSERT(new_sz < p->heap_sz);
    sys_memmove(p->heap + new_sz - stack_size, p->stop, stack_size *
                                                        sizeof(Eterm));
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)p->heap,
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;

    if ((offs = new_heap - HEAP_START(p)) != 0) {
	char* area = (char *) HEAP_START(p);
	Uint area_size = (char *) HEAP_TOP(p) - area;

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * know on some strange embedded systems...  Or when using purify.
         */

        offset_heap(new_heap, heap_size, offs, area, area_size);
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));
        offset_rootset(p, offs, area, area_size, objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
}


static void
sweep_proc_externals(Process *p, int fullsweep)
{
    ExternalThing** prev;
    ExternalThing* ptr;
    char* oh = 0;
    Uint oh_size = 0;

    if (fullsweep == 0) {
	oh = (char *) OLD_HEAP(p);
	oh_size = (char *) OLD_HEND(p) - oh;
    }

    prev = &MSO(p).externals;
    ptr = MSO(p).externals;

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (IS_MOVED(*ppt)) {        /* Object is alive */
            ExternalThing* ro = external_thing_ptr(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (in_area(ppt, oh, oh_size)) {
            /*
             * Object resides on old heap, and we just did a
             * generational collection - keep object in list.
             */
            prev = &ptr->next;
            ptr = ptr->next;
        } else {                /* Object has not been moved - deref it */
	    DEREF_ERL_NODE(ptr->node);
            *prev = ptr = ptr->next;
        }
    }
    ASSERT(*prev == NULL);
}

static void
sweep_proc_funs(Process *p, int fullsweep)
{
    ErlFunThing** prev;
    ErlFunThing* ptr;
    char* oh = 0;
    Uint oh_size = 0;

    if (fullsweep == 0) {
	oh = (char *) OLD_HEAP(p);
	oh_size = (char *) OLD_HEND(p) - oh;
    }
		      
    prev = &MSO(p).funs;
    ptr = MSO(p).funs;

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (IS_MOVED(*ppt)) {        /* Object is alive */
            ErlFunThing* ro = (ErlFunThing *) fun_val(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (in_area(ppt, oh, oh_size)) {
            /*
             * Object resides on old heap, and we just did a
             * generational collection - keep object in list.
             */
            prev = &ptr->next;
            ptr = ptr->next;
        } else {                /* Object has not been moved - deref it */
            ErlFunEntry* fe = ptr->fe;

            *prev = ptr = ptr->next;
            if (--(fe->refc) == 0) {
                erts_erase_fun_entry(fe);
            }
        }
    }
    ASSERT(*prev == NULL);
}

static void
sweep_proc_bins(Process *p, int fullsweep)
{
    ProcBin** prev;
    ProcBin* ptr;
    Binary* bptr;
    char* oh = 0;
    Uint oh_size = 0;

    if (fullsweep == 0) {
	oh = (char *) OLD_HEAP(p);
	oh_size = (char *) OLD_HEND(p) - oh;
    }

    prev = &MSO(p).mso;
    ptr = MSO(p).mso;

    /*
     * Note: In R7 we no longer force a fullsweep when we find binaries
     * on the old heap. The reason is that with the introduction of the
     * bit syntax we can expect binaries to be used a lot more. Note that
     * in earlier releases a brand new binary (or any other term) could
     * be put on the old heap during a gen-gc fullsweep, but this is
     * no longer the case in R7.
     */
    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (IS_MOVED(*ppt)) {        /* Object is alive */
            ProcBin* ro = (ProcBin*) binary_val(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (in_area(ppt, oh, oh_size)) {
            /*
             * Object resides on old heap, and we just did a
             * generational collection - keep object in list.
             */
            prev = &ptr->next;
            ptr = ptr->next;
        } else {                /* Object has not been moved - deref it */
            *prev = ptr->next;
            bptr = ptr->val;
            bptr->refc--;
            if (bptr->refc == 0) {
                if (bptr->flags & BIN_FLAG_MATCH_PROG) {
                    erts_match_set_free(bptr);
                } else {
                    erts_bin_free(bptr);
                }
            }
            ptr = *prev;
        }
    }
    ASSERT(*prev == NULL);
}

/*
 * Offset pointers into the heap (not stack).
 */

static void 
offset_heap(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED:
	      if (in_area(ptr_val(val), area, area_size)) {
		  *hp = offset_ptr(val, offs);
	      }
	      hp++;
	      break;
	  case TAG_PRIMARY_HEADER: {
	      Uint tari;

	      if (header_is_transparent(val)) {
		  hp++;
		  continue;
	      }
	      tari = thing_arityval(val);
	      switch (thing_subtag(val)) {
	      case REFC_BINARY_SUBTAG:
		  {
		      ProcBin* pb = (ProcBin*) hp;
		      Eterm** uptr = (Eterm **) &pb->next;

		      if (*uptr && in_area((Eterm *)pb->next, area, area_size)) {
			  *uptr += offs; /* Patch the mso chain */
		      }
		      sz -= tari;
		      hp += tari + 1;
		  }
		  break;
	      case FUN_SUBTAG:
		  {
		      ErlFunThing* funp = (ErlFunThing *) hp;
		      Eterm** uptr = (Eterm **) &funp->next;

		      if (*uptr && in_area((Eterm *)funp->next, area, area_size)) {
			  *uptr += offs;
		      }
		      sz -= tari;
		      hp += tari + 1;
		  }
		  break;
	      case EXTERNAL_PID_SUBTAG:
	      case EXTERNAL_PORT_SUBTAG:
	      case EXTERNAL_REF_SUBTAG:
		  {
		      ExternalThing* etp = (ExternalThing *) hp;
		      Eterm** uptr = (Eterm **) &etp->next;

		      if (*uptr && in_area((Eterm *)etp->next, area, area_size)) {
			  *uptr += offs;
		      }
		      sz -= tari;
		      hp += tari + 1;
		  }
		  break;
	      default:
		  sz -= tari;
		  hp += tari + 1;
	      }
	      break;
	  }
	  default:
	      hp++;
	      continue;
	}
    }
}

/*
 * Offset pointers to heap from stack.
 */

static void 
offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, char* area, Uint area_size)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (in_area(ptr_val(val), area, area_size)) {
		*hp = offset_ptr(val, offs);
	    }
	    hp++;
	    break;
	default:
	    hp++;
	    break;
	}
    }
}

static void
offset_off_heap(Process* p, int offs, char* area, Uint area_size)
{
    if (MSO(p).mso && in_area((Eterm *)MSO(p).mso, area, area_size)) {
        Eterm** uptr = (Eterm**) &MSO(p).mso;
        *uptr += offs;
    }

    if (MSO(p).funs && in_area((Eterm *)MSO(p).funs, area, area_size)) {
        Eterm** uptr = (Eterm**) &MSO(p).funs;
        *uptr += offs;
    }

    if (MSO(p).externals && in_area((Eterm *)MSO(p).externals, area, area_size)) {
        Eterm** uptr = (Eterm**) &MSO(p).externals;
        *uptr += offs;
    }
}

/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, Sint offs, char* area, Uint area_size)
{
    ErlMessage* mp = p->msg.first;

    while (mp != NULL) {
        Eterm mesg = ERL_MESSAGE_TERM(mp);
	switch (primary_tag(mesg)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (in_area(ptr_val(mesg), area, area_size)) {
		ERL_MESSAGE_TERM(mp) = offset_ptr(mesg, offs);
	    }
            break;
        }

        ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		is_atom(ERL_MESSAGE_TOKEN(mp))));
	mesg = ERL_MESSAGE_TOKEN(mp);
	if (is_tuple(mesg) && in_area(tuple_val(mesg), area, area_size)) {
	    ERL_MESSAGE_TOKEN(mp) = offset_ptr(mesg, offs);
        }
        mp = mp->next;
    }
}

static void ERTS_INLINE
offset_one_rootset(Process *p, int offs, char* area, Uint area_size,
	       Eterm* objv, int nobj)
{
    if (p->dictionary)  {
	offset_heap(p->dictionary->data, 
		    p->dictionary->used, 
		    offs, area, area_size);
    }
    if (p->debug_dictionary) {
	offset_heap(p->debug_dictionary->data, 
		    p->debug_dictionary->used, 
		    offs, area, area_size);
    }
    offset_heap(&p->fvalue, 1, offs, area, area_size);
    offset_heap(&p->ftrace, 1, offs, area, area_size);
    offset_heap(&p->seq_trace_token, 1, offs, area, area_size);
    offset_heap(&p->group_leader, 1, offs, area, area_size);
    offset_mqueue(p, offs, area, area_size);
    offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, area, area_size);
    if (nobj > 0) {
	offset_heap(objv, nobj, offs, area, area_size);
    }
    offset_off_heap(p, offs, area, area_size);
}

static void
offset_rootset(Process *p, int offs, char* area, Uint area_size,
	       Eterm* objv, int nobj)
{
    offset_one_rootset(p, offs, area, area_size, objv, nobj);
}

static char*
print_pid(Process *p)
{
    char static buf[64];

    Eterm obj = p->id;
    sprintf(buf,
	    "<%lu.%lu.%lu>",
	    internal_pid_channel_no(obj),
	    internal_pid_number(obj),
	    internal_pid_serial(obj));
    return buf;
}

#ifdef DEBUG
static int
within(Eterm *ptr, Process *p)
{
    ErlHeapFragment* bp = MBUF(p);

    if (OLD_HEAP(p) && (OLD_HEAP(p) <= ptr && ptr < OLD_HEND(p))) {
        return 1;
    }
    if (HEAP_START(p) <= ptr && ptr < HEAP_TOP(p)) {
        return 1;
    }
    while (bp != NULL) {
        if (bp->mem <= ptr && ptr < bp->mem + bp->size) {
            return 1;
        }
        bp = bp->next;
    }
    return 0;
}
#endif

#endif /* HEAP_FRAG_ELIM_TEST */
