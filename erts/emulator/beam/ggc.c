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
#if HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif0.h"
#include "hipe_stack.h"
#endif

#ifdef BENCH_STAT
/* #define CALCULATE_HEAP_SIZES */
/* #define SAVE_GC_STATISTICS */
#ifdef SAVE_GC_STATISTICS
    #define SAVE_GC_FILENAME "/tmp/gc.log"
    uint oldptr, newptr;
#endif
#endif

#ifdef UNIFIED_HEAP
  #define STACK_BEGIN    p->stack
  #define STACK_END      p->send
  #define HEAP_START     global_heap
  #define HEAP_TOP       global_htop
  #define HEAP_END       global_hend
  #define HEAP_SIZE      global_heap_sz
  #define HIGH_WATER     global_high_water
  #define OLD_HEND       global_old_hend
  #define OLD_HTOP       global_old_htop
  #define OLD_HEAP       global_old_heap
  #define GEN_GCS        global_gen_gcs
  #define MAX_GEN_GCS    global_max_gen_gcs
  #define FLAGS          global_gc_flags
  #define MBUF           global_mbuf
  #define MBUF_SIZE      global_mbuf_sz
  char gc_all_processes;   /* Flag to make the gen_gc look at all processes
                            * first time after fullsweep.
                            */
#else
  #define STACK_BEGIN    p->hend
  #define STACK_END      p->htop
  #define HEAP_START     p->heap
  #define HEAP_TOP       p->htop
  #define HEAP_END       p->hend
  #define HEAP_SIZE      p->heap_sz
  #define HIGH_WATER     p->high_water
  #define OLD_HEND       p->old_hend
  #define OLD_HTOP       p->old_htop
  #define OLD_HEAP       p->old_heap
  #define GEN_GCS        p->gen_gcs
  #define MAX_GEN_GCS    p->max_gen_gcs
  #define FLAGS          p->flags
  #define MBUF           p->mbuf
  #define MBUF_SIZE      p->mbuf_sz
#endif

#define MY_IS_MOVED(x)	(!is_header((x)))

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
    case FUN_SUBTAG: nelts += ((ErlFunThing*)(PTR-1))->num_free; break;	\
    }									\
    while (nelts--)							\
	*HTOP++ = *PTR++;						\
} while(0)

/*
 * Returns number of elements in an array.
 */
#define ALENGTH(a) (sizeof(a)/sizeof(a[0]))

#ifdef DEBUG
/* #define HARDDEBUG  1 */
/* #define GC_HEAP_TRACE 1 */
/* #define GC_SWITCH_TRACE 1 */
/* #define OLD_HEAP_CREATION_TRACE 1 */
#endif

/*
 * This structure describes the rootset for the GC.
 */
typedef struct {
    Eterm* v[7];		/* Pointers to vectors with terms to GC
				 * (e.g. the stack).
				 */
    Uint sz[7];			/* Size of each vector. */
    Eterm* v_msg;		/* Pointer to messages to GC. */
    Eterm def_msg[32];		/* Default storage for messages (to avoid malloc). */
#ifdef UNIFIED_HEAP
    Uint n;
#endif
} Rootset;

/*
 * Used for printing beatiful stack dumps.
 */
extern Eterm beam_apply[];
extern Eterm beam_exit[];

static int setup_rootset(Process*, Eterm*, int, Rootset*);
static void gen_gc(Process*, int, Eterm*, int);
static char* print_pid(Process* p);
static void sweep_proc_bins(Process *p, int fullsweep);
static void sweep_proc_funs(Process *p, int fullsweep);

#ifdef HARDDEBUG
static void check_stack(Process*, char*);
static int within(Eterm*, Process*);
void check_bins(Process *p);
int chk_sys(void);

#define CHECK(p) \
    check_stack((p), "check"); \
    check_bins(p);
#else

# define within(x,y) 1
# define CHECK(p) ((void) 1)

#endif

#define ptr_within(ptr, low, high) ((ptr) < (high) && (ptr) >= (low))

#if HIPE
/* efficient range check */
#define in_area(ptr,start,nbytes) ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))
#endif

static int heap_sizes[64];	/* Suitable heap sizes. */
static int num_heap_sizes;	/* Number of heap sizes. */

/*
** Initialize GC global data
*/
void init_gc(void)
{
    int i = 0;
#ifdef UNIFIED_HEAP
    gc_all_processes = 0;
#endif

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
	     * example is fib(35) == 14meg, whereas fib(36) == 24meg,
	     * we really don't want that growth when the heaps are that big.
	     */
	    
	    double grow_factor = 1.25;  /* instead of fib */
	    
	    heap_sizes[0] = 34;
	    heap_sizes[1] = 55;
	    for (i = 2; i < 23; i++) {
		heap_sizes[i] = heap_sizes[i-1] + heap_sizes[i-2];
	    }
	    
	    /* At 1.3 mega words heap, we start to slow down */
	    for (i = 23; i < ALENGTH(heap_sizes) && heap_sizes[i-1] < MAX_SMALL; i++) {
		heap_sizes[i] = (int) (grow_factor * heap_sizes[i-1]);
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
 * Offset pointers to heap from stack.
 */

static void 
offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, 
		Eterm* low, Eterm* high)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED: {
	      if (ptr_within(ptr_val(val), low, high)) {
		  *hp = offset_ptr(val, offs);
	      }
	      hp++;
	      continue;
	  }
	  default: {
	      hp++;
	      continue;
	  }
	}
    }
}

/*
 * Offset pointers into the heap (not stack).
 * Only offset pointers that point into the interval of low and high.
 */

static void 
offset_heap(Eterm* hp, Uint sz, Sint offs,
	    Eterm* low, Eterm* high)
{
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED: {
	      if (ptr_within(ptr_val(val), low, high)) {
		  *hp = offset_ptr(val, offs);
	      }
	      hp++;
	      continue;
	  }
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

		      if (*uptr && ptr_within((Eterm *)pb->next, low, high)) {
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

		      if (*uptr && ptr_within((Eterm *)funp->next, low, high)) {
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
	      continue;
	  }
	  default: {
	      hp++;
	      continue;
	  }
	}
    }
}


/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, Sint offs, Eterm* low, Eterm* high) 
{
    ErlMessage* mp = p->msg.first;

    while (mp != NULL) {
        Eterm mesg = mp->mesg;
	switch (primary_tag(mesg)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (ptr_within(ptr_val(mesg), low, high)) {
		mp->mesg = offset_ptr(mesg, offs);
	    }
            break;
        }

	ASSERT((is_nil(mp->seq_trace_token)
		|| is_tuple(mp->seq_trace_token)
		|| is_atom(mp->seq_trace_token)));
	mesg = mp->seq_trace_token;
	if (is_tuple(mesg) && ptr_within(tuple_val(mesg), low, high)) {
	    mp->seq_trace_token = offset_ptr(mesg, offs);
        }
        mp = mp->next;
    }
}

#ifdef UNIFIED_HEAP
#define CURRENT_ROOTSET(flupp) rootset[r_index].flupp
static int
setup_rootset(Process *c_p, Eterm* objv, int nobj, Rootset rootset[])
{
    Uint p_index = -1, r_index = 0;

    while(active_procs[++p_index]) {
        Process *p = active_procs[p_index];
#else
#define CURRENT_ROOTSET(flupp) rootset->flupp
static int
setup_rootset(Process* p, Eterm* objv, int nobj, Rootset *rootset) 
{
#endif
    int n;
    ErlMessage* mp;
    Eterm* v_ptr;
    int v_msg_len;

#ifdef UNIFIED_HEAP
    if ((p == (Process*)NIL) || !(gc_all_processes || p->active))
        continue;
#endif
    v_msg_len = 2 * p->msg.len;
    /*
     * Move pointers for all messages into an array pointed to by p->v_msg.
     */
    if (v_msg_len > ALENGTH(CURRENT_ROOTSET(def_msg))) {
        CURRENT_ROOTSET(v_msg) = (Eterm *)
	    erts_safe_sl_alloc_from(210, sizeof(Eterm) * v_msg_len);
    } else {
        CURRENT_ROOTSET(v_msg) = CURRENT_ROOTSET(def_msg);
    }
    mp = p->msg.first;
    v_ptr = CURRENT_ROOTSET(v_msg);
    while (mp != NULL) {
        *v_ptr++ = mp->mesg;
	ASSERT((is_nil(mp->seq_trace_token)
		|| is_tuple(mp->seq_trace_token)
		|| is_atom(mp->seq_trace_token)));
        *v_ptr++ = mp->seq_trace_token;
        mp = mp->next;
    }

    n = 0;
    CURRENT_ROOTSET(v[n])  = p->stop;
    CURRENT_ROOTSET(sz[n]) = STACK_BEGIN - p->stop;
    ++n;

    if (p->dictionary != NULL) {
        CURRENT_ROOTSET(v[n])  = p->dictionary->data;
        CURRENT_ROOTSET(sz[n]) = p->dictionary->used;
        ++n;
    }
    if (p->debug_dictionary != NULL) {
        CURRENT_ROOTSET(v[n])  = p->debug_dictionary->data;
        CURRENT_ROOTSET(sz[n]) = p->debug_dictionary->used;
        ++n;
    }
    CURRENT_ROOTSET(v[n])  = CURRENT_ROOTSET(v_msg);
    CURRENT_ROOTSET(sz[n]) = v_msg_len;
    ++n;
#ifdef UNIFIED_HEAP
    if (c_p != p)
    {
        if (p->arity > 0)
        {
            CURRENT_ROOTSET(v[n])  = p->arg_reg;
            CURRENT_ROOTSET(sz[n]) = p->arity;
            ++n;
        }
    }
    else
    {
#endif
    if (nobj > 0) {
        CURRENT_ROOTSET(v[n])  = objv;
        CURRENT_ROOTSET(sz[n]) = nobj;
        ++n;
    }
#ifdef UNIFIED_HEAP
    }
#endif

    ASSERT((is_nil(p->seq_trace_token)
	    || is_tuple(p->seq_trace_token)
	    || is_atom(p->seq_trace_token)));
    CURRENT_ROOTSET(v[n]) = &p->seq_trace_token;
    CURRENT_ROOTSET(sz[n]) = 1;
    n++;

#if HIPE
    hipe_clean_nstack(p);
    CURRENT_ROOTSET(v[n]) = hipe_nstack_start(p);
    CURRENT_ROOTSET(sz[n]) = hipe_nstack_used(p);
    ++n;
#endif

    ASSERT(n <= ALENGTH(CURRENT_ROOTSET(v)));
#ifdef UNIFIED_HEAP
        CURRENT_ROOTSET(n) = n;
        r_index++;
    }
    return r_index;
#else
    return n;
#endif
}

#ifdef UNIFIED_HEAP
static void
restore_rootset(Rootset *rootset)
{
    Uint p_index = -1;
    Uint r_index = 0;
#else
static void
restore_rootset(Process *p, Rootset *rootset)
{
#endif
    Eterm* v_ptr;
    ErlMessage* mp;
    ErlHeapFragment* bp;

#ifdef UNIFIED_HEAP
    while(active_procs[++p_index]) {
        Process *p = active_procs[p_index];
        if ((p == (Process*)NIL) || !(gc_all_processes || p->active))
            continue;
#endif
    /*
     * Restore all message pointers.
     */
    mp = p->msg.first;
    v_ptr = CURRENT_ROOTSET(v_msg);
    while (mp != NULL) {
	mp->mesg = *v_ptr++;
	ASSERT((is_nil(*v_ptr)
		|| is_tuple(*v_ptr)
		|| is_atom(*v_ptr)));
	mp->seq_trace_token = *v_ptr++;
	mp = mp->next;
    }
    
    if (CURRENT_ROOTSET(v_msg) != CURRENT_ROOTSET(def_msg)) {
        erts_sl_free(CURRENT_ROOTSET(v_msg));
    }
#ifdef UNIFIED_HEAP
        r_index++;
    }
#endif

    /*
     * Remove all message buffers.
     */
    bp = MBUF;
    MBUF = NULL;
    MBUF_SIZE = 0;
    while (bp != NULL) {
	ErlHeapFragment* next_bp = bp->next;
#ifdef DEBUG
	sys_memset(bp->mem, 0xff, bp->size*sizeof(Eterm));
#endif 
	free_message_buffer(bp);
	bp = next_bp;
    }
}



/* 
** This function is used when using fullsweep gc.
** This replaces gen_gc for processes that use the
** fullsweep algorithm and don't have an old_heap.
** Parameters:
** p: The process who is being garbage collected.
** new_sz: The wanted size of the heap after collecting.
** objv: Vector of "extra" objects to be "saved".
** nobj: Number of objects in objv.
*/
static void
fullsweep_heap(Process *p, int new_sz, Eterm* objv, int nobj)
{
#ifdef UNIFIED_HEAP
    Rootset rootset[max_process];
#else
    Rootset rootset;
#endif
    Eterm* n_hstart;		/* Start of new heap */
    Eterm* n_htop;		/* Top of new heap */
    Eterm* n_heap;		/* The new heap */
    Eterm* n_hp;
    int n;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif

    /* Create new, empty heap */
#ifdef UNIFIED_HEAP
    n_heap = (Eterm *) safe_alloc_from(803, sizeof(Eterm)*new_sz);
#else
    n_heap = (Eterm *) erts_safe_sl_alloc_from(803, sizeof(Eterm)*new_sz);
#endif
    n_hstart = n_htop = n_heap;
    FLAGS &= ~F_NEED_FULLSWEEP;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Fullsweep GC proc: %d\n", print_pid(p));
#endif
#ifdef UNIFIED_HEAP
    gc_all_processes = 1;
    n = setup_rootset(p, objv, nobj, rootset);

    while (n--) {
	while (rootset[n].n--) {
	  Eterm* g_ptr = rootset[n].v[rootset[n].n];
	  Uint  g_sz = rootset[n].sz[rootset[n].n];
#else
	  n = setup_rootset(p, objv, nobj, &rootset);

        while (n--) {
	     Eterm* g_ptr = rootset.v[n];
	     Eterm g_sz = rootset.sz[n];
#endif
	
	while(g_sz--) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *g_ptr;

	    switch (primary_tag(gval)) {

	      case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (MY_IS_MOVED(val))
		    *g_ptr++ = val;
#if HIPE
		else if( in_area(ptr, const_start, const_size) )
		    ++g_ptr;
#endif
		else {
		    ASSERT(within(ptr, p));
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
		}
		continue;
	      }

	      case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (is_non_value(val))
		    *g_ptr++ = ptr[1];
#if HIPE
		else if( in_area(ptr, const_start, const_size) )
		    ++g_ptr;
#endif
		else {
		    ASSERT(within(ptr, p));
		    MOVE_CONS(ptr,val,n_htop,g_ptr++);
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
#ifdef UNIFIED_HEAP
    }
#endif


    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
     */

    n_hp = n_hstart;
    
    while (n_hp != n_htop) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *n_hp;

	switch (primary_tag(gval)) {

	  case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (MY_IS_MOVED(val))
		*n_hp++ = val;
#if HIPE
	    else if( in_area(ptr, const_start, const_size) )
		++n_hp;
#endif
	    else {
		ASSERT(within(ptr, p));
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
	    }
	    continue;
	  }

	  case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (is_non_value(val))
		*n_hp++ = ptr[1];
#if HIPE
	    else if( in_area(ptr, const_start, const_size) )
		++n_hp;
#endif
	    else {
		ASSERT(within(ptr, p));
		MOVE_CONS(ptr,val,n_htop,n_hp++);
	    }
	    continue;
	  }

	  case TAG_PRIMARY_HEADER: {
	      if (header_is_thing(gval))
		  n_hp += (thing_arityval(gval)+1);
	      else
		  n_hp++;
	      continue;
	  }

	  default: {
	    n_hp++;
	    continue;
	  }
	}
    }

#ifdef UNIFIED_HEAP
    {
        Uint p_index = -1;
        while (active_procs[++p_index]) {
            Process * p = active_procs[p_index];
            if(p == (Process*)NIL)
                continue;
#endif
    if (p->off_heap.mso) {
	sweep_proc_bins(p, 1);
    }
    if (p->off_heap.funs) {
	sweep_proc_funs(p, 1);
    }

#ifdef UNIFIED_HEAP
            p->active = 0;
        }
    }
    restore_rootset(rootset);
#else
    restore_rootset(p, &rootset);
#endif

    if (OLD_HEAP != NULL) {
#ifdef DEBUG
        sys_memset(OLD_HEAP, 0xff, (OLD_HEND - OLD_HEAP)*sizeof(Eterm));
#endif    
#ifdef UNIFIED_HEAP
        sys_free(OLD_HEAP);
#else
        erts_sl_free(OLD_HEAP);
#endif
        OLD_HEAP = OLD_HTOP = OLD_HEND = NULL;
    }

#ifndef UNIFIED_HEAP
    /* Move the stack, the beam stack is "in the heap" */
    n = p->hend - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
#endif
#ifdef DEBUG
    sys_memset(HEAP_START, 0xff, (HEAP_END - HEAP_START)*sizeof(Eterm));
#endif    
    HEAP_END = n_heap + new_sz;
#ifndef UNIFIED_HEAP
    p->stop = p->hend - n;
#endif
#ifdef UNIFIED_HEAP
    sys_free(HEAP_START);
#else
    erts_sl_free(HEAP_START);
#endif

    HEAP_START = n_heap;
    HEAP_TOP = n_htop;
    HEAP_SIZE = new_sz;
    GEN_GCS = 0;

    /*
     * Should we set p->high_water to p->heap or p->htop?
     *
     * Setting it to p->htop means that any surviving data will be
     * placed on the old heap in the next garbage collection.
     * This setting gives a better estone value, but one can assume
     * that more garbage are placed on the old heap.
     *
     * Setting it to p->heap means that two more garbage collections
     * are needed to move data to the old heap.
     */

    HIGH_WATER = HEAP_TOP;
}

/*
 * Find the next heap size equal to or greater than the given size (if offset == 0).
 *
 * If offset is 1, the next higher heap size is returned (always greater than size).
 */
int
next_heap_size(int size, int offset)
{
    if (size < heap_sizes[0]) {
	return heap_sizes[0];
    } else {
	int* low = heap_sizes;
	int* high = heap_sizes + num_heap_sizes;
	int* mid;

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

static void
offset_off_heap(Process* p, Eterm* low, Eterm* high, int offs)
{
    if (p->off_heap.mso && ptr_within((Eterm *)p->off_heap.mso, low, high)) {
        Eterm** uptr = (Eterm**) &p->off_heap.mso;
        *uptr += offs;
    }

    if (p->off_heap.funs && ptr_within((Eterm *)p->off_heap.funs, low, high)) {
        Eterm** uptr = (Eterm**) &p->off_heap.funs;
        *uptr += offs;
    }
}

#ifdef UNIFIED_HEAP
static void offset_rootset(int offs, Eterm* low, Eterm* high,
                           Eterm* objv, int nobj)
{
    Uint p_index = -1;
    while (active_procs[++p_index]) {
        Process * p = active_procs[p_index];
        if(p == (Process*)NIL)
            continue;
#else
static void
offset_rootset(Process *p, int offs, 
	       Eterm* low, Eterm* high, 
	       Eterm* objv, int nobj)
{
#endif
    if (p->dictionary) 
	offset_heap(p->dictionary->data, 
		    p->dictionary->used, 
		    offs, low, high);
    if (p->debug_dictionary) 
	offset_heap(p->debug_dictionary->data, 
		    p->debug_dictionary->used, 
		    offs, low, high);
    offset_heap(&p->seq_trace_token, 1, offs, low, high);
    offset_mqueue(p, offs, low, high);
    offset_heap_ptr(p->stop, (STACK_BEGIN - p->stop), offs, low, high);
#if HIPE
    offset_heap_ptr(hipe_nstack_start(p), hipe_nstack_used(p), offs, low, high);
#endif
#ifdef UNIFIED_HEAP
    }
#endif
    if (nobj > 0) {
	offset_heap(objv, nobj, offs, low, high);
    }
#ifdef UNIFIED_HEAP
    p_index = -1;
    while (active_procs[++p_index])
    {
        Process * p = active_procs[p_index];
        if(p == (Process*)NIL)
            continue;
#endif
    offset_off_heap(p, low, high, offs);
#ifdef UNIFIED_HEAP
    }
#endif
}



/*
 * Grow the new heap size to 'new_sz'.
 */
static void
grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP - HEAP_START;
#ifndef UNIFIED_HEAP
    int stack_size = p->hend - p->stop;
#endif
    sint32 offs;

    ASSERT(HEAP_SIZE < new_sz);
#ifdef UNIFIED_HEAP
    new_heap = (Eterm *) safe_realloc((void*)HEAP_START, sizeof(Eterm)*new_sz);
#else
    new_heap = (Eterm *) erts_safe_sl_realloc((void*)HEAP_START,
					 sizeof(Eterm)*(HEAP_SIZE),
					 sizeof(Eterm)*new_sz);
#endif

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "grow_new_heap: GREW (%d) FROM %d UPTO %d (used %d)\n",
            pid_number(p->id), HEAP_SIZE, new_sz, heap_size);
#endif

    if ((offs = new_heap - HEAP_START) == 0) { /* No move. */
        HEAP_END = new_heap + new_sz;
#ifndef UNIFIED_HEAP
        sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
        p->stop = p->hend - stack_size;
#endif
    } else {
#ifndef UNIFIED_HEAP
        Eterm* prev_stop = p->stop;
#endif

        offset_heap(new_heap, heap_size, offs, HEAP_START, HEAP_TOP);
        HIGH_WATER = new_heap + (HIGH_WATER - HEAP_START);

        HEAP_END = new_heap + new_sz;
#ifdef UNIFIED_HEAP
        offset_rootset(offs, HEAP_START, HEAP_TOP, objv, nobj);
#else
        prev_stop = new_heap + (p->stop - p->heap);
        p->stop = p->hend - stack_size;
        sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));
        offset_rootset(p, offs, p->heap, p->htop, objv, nobj);
#endif
        HEAP_TOP = new_heap + heap_size;
        HEAP_START = new_heap;
    }
    HEAP_SIZE = new_sz;
}

static void
shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = HEAP_TOP - HEAP_START;
#ifdef UNIFIED_HEAP
    sint32 offs;

    ASSERT(new_sz < HEAP_SIZE);
    new_heap = (Eterm *) safe_realloc((void*)HEAP_START, sizeof(Eterm)*new_sz);
    HEAP_END = new_heap + new_sz;
#else
    int stack_size = p->hend - p->stop;
    sint32 offs;

    ASSERT(new_sz < p->heap_sz);
    sys_memmove(p->heap + new_sz - stack_size, p->stop, stack_size *
                                                        sizeof(Eterm));
    new_heap = (Eterm *) erts_safe_sl_realloc((void*)p->heap,
					      sizeof(Eterm)*(HEAP_SIZE),
					      sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;
#endif

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "shrink_new_heap: SHRINKED (%d) FROM %d DOWNTO %d (used %d)\n",
            pid_number(p->id), HEAP_SIZE, new_sz, heap_size);
#endif

    if ((offs = new_heap - HEAP_START) != 0) {

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * knows on some strange embedded systems...  Or when using purify.
         */

        offset_heap(new_heap, heap_size, offs, HEAP_START, HEAP_TOP);
        HIGH_WATER = new_heap + (HIGH_WATER - HEAP_START);
#ifdef UNIFIED_HEAP
        offset_rootset(offs, HEAP_START, HEAP_TOP, objv, nobj);
#else
        offset_rootset(p, offs, p->heap, p->htop, objv, nobj);
#endif
        HEAP_TOP = new_heap + heap_size;
        HEAP_START = new_heap;
    }
    HEAP_SIZE = new_sz;
}

static void
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
#ifndef UNIFIED_HEAP
    int stack_size =  p->hend - p->stop;
#endif

    size_after = (HEAP_TOP - HEAP_START);
    reclaimed += (size_before - size_after);

     /*
      * Resize the heap if needed.
      */

#ifdef UNIFIED_HEAP
    need_after = size_after + need;
#else
    need_after = size_after + need + stack_size;
#endif
    if (HEAP_SIZE < need_after) {
        /* Too small - grow to match requested need */
        sz = next_heap_size(need_after, 0);
        grow_new_heap(p, sz, objv, nobj);
    } else if (3 * HEAP_SIZE < 4 * need_after){
        /* Need more than 75% of current, postpone to next GC.*/
        FLAGS |= F_HEAP_GROW;
    } else if (4 * need_after < HEAP_SIZE && HEAP_SIZE > H_MIN_SIZE){
        /* We need less than 25% of the current heap, shrink.*/
        /* XXX - This is how it was done in the old GC:
           wanted = 4 * need_after;
           I think this is better as fullsweep is used mainly on
           small memory systems, but I could be wrong... */
        wanted = 2 * need_after;
        if (wanted < p->min_heap_size) {
            sz = p->min_heap_size;
        } else {
            sz = next_heap_size(wanted, 0);
        }
        if (sz < HEAP_SIZE) {
            shrink_new_heap(p, sz, objv, nobj);
        }
    }
}

/*
** Garbage collect a process.
** Parameters:
** p: Pointer to the process structure.
** need: Number of (erlang) words needed on the heap.
** objv: Array of terms to add to rootset, that is to preserve.
** nobj: Number of objects in objv.
*/

int
erts_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    int size_before;
    int size_after;
    int need_after;
    Uint saved_status;
    int wanted;
#ifdef UNIFIED_HEAP
    Uint p_index;
#else
    int stack_size;             /* Size of stack ON HEAP. */
#endif
    int sz;
#ifdef BENCH_STAT
    hrtime_t gctime_start;
    uint was_this_major = 0;
#endif

#ifdef SAVE_GC_STATISTICS
    {
        FILE *file = fopen(SAVE_GC_FILENAME,"a");

        if (file)
        {
            int mbufs = 0, numprocs = 0;
            int i, totalheap = 0;
            ErlHeapFragment *bp = MBUF;

#ifdef UNIFIED_HEAP
            totalheap = global_heap_sz;
#endif
            for (i = 0; i < max_process; i++)
            {
                Process *p;
                if ((p = process_tab[i]) != NULL)
                {
                    numprocs++;
#ifndef UNIFIED_HEAP
                    totalheap += p->heap_sz;
#endif
                }
            }
            if (totalheap > largestheapsofar) largestheapsofar = totalheap;
            while (bp != NULL)
            {
                mbufs++;
                bp = bp->next;
            }
            fprintf(file,"BEFORE %ld %d %d %ld %d %d %d\n",HEAP_SIZE,
                                                   HEAP_TOP - HEAP_START,
                                                   need,
                                                   MBUF_SIZE,
                                                   mbufs,
                                                   largestheapsofar,
                                                   numprocs);
            fclose(file);
        }
        else
            printf("Error when saving to %s!\n\r",SAVE_GC_FILENAME);
    }
#endif
#ifdef CALCULATE_HEAP_SIZES
    {
        double total_used_heap = 0;
#ifdef UNIFIED_HEAP
        total_used_heap = HEAP_TOP - HEAP_START;
#else
        int i;
        for (i=0; i<max_process; i++)
        {
            Process *p = process_tab[i];
            if (p == NULL) continue;
            total_used_heap += p->htop - p->heap;
        }
#endif
        if (total_used_heap > biggest_heap_size_ever)
            biggest_heap_size_ever = total_used_heap;
        gctime_start = sys_gethrtime();
    }
#endif

#ifdef UNIFIED_HEAP
#define OverRunCheck() \
    if (HEAP_END < HEAP_TOP) { \
        erl_exit(1, "%s: Overrun heap at line %d\n", print_pid(p),__LINE__); \
    }
#else
#define OverRunCheck() \
    if (p->stop < p->htop) { \
        erl_exit(1, "%s: Overrun stack and heap at line %d\n", print_pid(p),__LINE__); \
    }
#endif

    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_start);
    }
    saved_status = p->status;
    p->status = P_GARBING;
    CHECK(p);
    OverRunCheck();
    if (GEN_GCS >= MAX_GEN_GCS) {
        FLAGS |= F_NEED_FULLSWEEP;
    }

#ifdef UNIFIED_HEAP
    p_index = -1;
    while (active_procs[++p_index] != NULL)
    {
        Process *p = active_procs[p_index];
        if(p == (Process*)NIL)
            continue;
#endif
    p->arith_avail = 0;
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
    p->off_heap.overhead = 0;
#ifdef UNIFIED_HEAP
    }
#endif
    garbage_cols++;

#ifndef UNIFIED_HEAP
    stack_size = p->hend - p->stop;
#endif

    /* Size of heap before first GC */
    size_before = MBUF_SIZE + (HEAP_TOP - HEAP_START);

    /*
     * Generational GC from here on. We need an old heap.
     */

    if (OLD_HEAP == NULL && HIGH_WATER != HEAP_START &&
        (FLAGS & F_NEED_FULLSWEEP) == 0) {
        Eterm* n_old;
        /* Note: We choose a larger heap size than strictly needed,
         * which seems to reduce the number of fullsweeps.
         * This improved Estone by more than 1200 estones on my computer
         * (Ultra Sparc 10).
         */
        size_t new_sz = next_heap_size(HIGH_WATER - HEAP_START, 1);

        /* Create new, empty old_heap */
#ifdef UNIFIED_HEAP
        n_old = (Eterm *) safe_alloc_from(801, sizeof(Eterm)*new_sz);
#else
        n_old = (Eterm *) erts_safe_sl_alloc_from(801, sizeof(Eterm)*new_sz);
#endif

        OLD_HEND = n_old + new_sz;
        OLD_HEAP = OLD_HTOP = n_old;
#ifdef OLD_HEAP_CREATION_TRACE
#ifdef UNIFIED_HEAP
        fprintf(stderr, "Created a global old_heap because of %s.\r\n",
            print_pid(p));
#else
        fprintf(stderr, "Created old_heap for %s.\r\n", print_pid(p));
#endif
#endif
    }

    /*
     * Try a generational GC if the old heap is big enough and
     * there are no old binaries on it.
     */

    if ((FLAGS & F_NEED_FULLSWEEP) == 0 &&
        HIGH_WATER - HEAP_START <= OLD_HEND - OLD_HTOP) {

        /*
         * There is space enough in old_heap for everything
         * below the high water mark.  Do a generational GC.
         */

        gen_gc(p, next_heap_size(HEAP_SIZE + MBUF_SIZE, 0), objv, nobj);
        GEN_GCS++;
        size_after = HEAP_TOP - HEAP_START;
#ifdef UNIFIED_HEAP
        need_after = size_after + need;
#else
        need_after = size_after + need + stack_size;
#endif
        reclaimed += (size_before - size_after);

        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */

        if ((HEAP_SIZE > 300) && (4 * need_after < HEAP_SIZE) &&
            ((HEAP_SIZE > 8000) ||
             (HEAP_SIZE > (OLD_HEND - OLD_HEAP)))) {
            wanted = 3 * need_after;
#ifdef UNIFIED_HEAP
            if (wanted < H_MIN_SIZE) {
                wanted = H_MIN_SIZE;
#else
            if (wanted < p->min_heap_size) {
                wanted = p->min_heap_size;
#endif
            } else {
                wanted = next_heap_size(wanted, 0);
            }
            if (wanted < HEAP_SIZE) {
                shrink_new_heap(p, wanted, objv, nobj);
            }
            ASSERT(HEAP_SIZE == next_heap_size(HEAP_SIZE, 0));
            goto done;
        }

        /*
         * The heap size turned out to be just right. We are done.
         */

        if (HEAP_SIZE >= need_after) {
            ASSERT(HEAP_SIZE == next_heap_size(HEAP_SIZE, 0));
            goto done;
        }
#ifdef GC_HEAP_TRACE
        fprintf(stderr, "Did a gen_gc, still not enough room\n");
#endif
    }

    /*
     * The previous generational GC did not leave enough free heap space.
     * We must do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    sz = HEAP_SIZE + MBUF_SIZE  + (OLD_HTOP - OLD_HEAP);
#ifndef UNIFIED_HEAP
    sz += p->hend - p->stop;
#endif
    sz = next_heap_size(sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (sz == HEAP_SIZE && FLAGS & F_HEAP_GROW) {
        sz = next_heap_size(HEAP_SIZE, 1);
    }
    FLAGS &= ~F_HEAP_GROW;

    fullsweep_heap(p, sz, objv, nobj);
    CHECK(p);
    adjust_after_fullsweep(p, size_before, need, objv, nobj);
#ifdef BENCH_STAT
    {
        hrtime_t this_time = sys_gethrtime() - gctime_start; 
        major_garbage_cols++;
        was_this_major = 1;
        live_major_sum += HEAP_TOP - HEAP_START;
        major_gc_time += this_time;
        if (this_time > max_major_time)
            max_major_time = this_time;
    }
#endif

 done:
#ifdef BENCH_STAT
    if (was_this_major == 0)
    {
        hrtime_t this_time = sys_gethrtime() - gctime_start; 
        minor_garbage_cols++;
        live_minor_sum += HEAP_TOP - HEAP_START;
        minor_gc_time += this_time;
        if (this_time > max_minor_time)
            max_minor_time = this_time;
    }
#ifdef CALCULATE_HEAP_SIZES
    {
        double total_used_heap = 0;
        double total_allocated_heap = 0;
#ifdef UNIFIED_HEAP
        total_used_heap = HEAP_TOP - HEAP_START;
        total_allocated_heap = HEAP_SIZE;
#else
        int i;
        for (i=0; i<max_process; i++)
        {
            Process *p = process_tab[i];
            if (p == NULL) continue;
            total_used_heap += p->htop - p->heap;
            total_allocated_heap += p->heap_sz;
        }
#endif
        if (total_used_heap > biggest_heap_size_ever)
            biggest_heap_size_ever = total_used_heap;
        if (total_allocated_heap > max_allocated_heap)
            max_allocated_heap = total_allocated_heap;
    }
#endif
#endif
    CHECK(p);
    OverRunCheck();
    p->status = saved_status;
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_end);
    }
#ifdef SAVE_GC_STATISTICS
    {
        FILE *file = fopen(SAVE_GC_FILENAME,"a");
        uint sz_after;

        if (file)
        {
            fprintf(file,"AFTER %ld %d %d %d %d %ld %lf\n",HEAP_SIZE,
                                              sz_after = HEAP_TOP - HEAP_START,
                                              size_before - sz_after,
                                              ptrs_to_old,
                                              ptrs_to_young,
                                              garbage_cols,
                                              major_garbage_cols);
            fclose(file);
        }
        else
          printf("Error when saving to %s!\n\r",SAVE_GC_FILENAME);
    }
#endif
    return ((int) (HEAP_TOP - HEAP_START) / 10);
#undef OverRunCheck
}

/*
 * This function sweeps both the remainder of the new heap
 * as well as the remainder of the old heap after the first pass
 * of the generational collector gen_gc().
 */
static Eterm*
gen_cheney(Process *p, Eterm* from, Eterm** to,
           Eterm* low_water, Eterm* high_water, Eterm* old_htop)
{
    Eterm* n_hp;
    Eterm* n_htop;
    Eterm* oh_start;
    Eterm* oh_end;

    Eterm* ptr;
    Eterm val;
    Eterm gval;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif

    n_hp = from;
    n_htop = *to;
    oh_start = OLD_HEAP;
    oh_end = OLD_HEND;

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
          case TAG_PRIMARY_BOXED: {
            ptr = boxed_val(gval);
            val = *ptr;
            if (ptr_within(ptr, oh_start, oh_end))
#ifdef BENCH_STAT
            {
                ptrs_to_old++;
#endif
                n_hp++;
#ifdef BENCH_STAT
            }
#endif
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
#ifdef BENCH_STAT
            {
                ptrs_to_old++;
#endif
                ++n_hp;
#ifdef BENCH_STAT
            }
#endif
#endif
            else if (MY_IS_MOVED(val))
                *n_hp++ = val;
            else if (ptr_within(ptr, low_water, high_water)) {
                /* Make object old */
                MOVE_BOXED(ptr,val,old_htop,n_hp++);
#ifdef BENCH_STAT
                ptrs_to_young++;
#endif
            } else {
                ASSERT(within(ptr, p));
                MOVE_BOXED(ptr,val,n_htop,n_hp++);
#ifdef BENCH_STAT
                ptrs_to_young++;
#endif
            }
            continue;
          }
          case TAG_PRIMARY_LIST: {
            ptr = list_val(gval);
            val = *ptr;
            if (ptr_within(ptr, oh_start, oh_end))
#ifdef BENCH_STAT
            {
                ptrs_to_old++;
#endif
                n_hp++;
#ifdef BENCH_STAT
            }
#endif
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
#ifdef BENCH_STAT
            {
                ptrs_to_old++;
#endif
                ++n_hp;
#ifdef BENCH_STAT
            }
#endif
#endif
            else if (is_non_value(val))
                *n_hp++ = ptr[1];
            else if (ptr_within(ptr, low_water, high_water)) {
                /* Make object old */
                MOVE_CONS(ptr,val,old_htop,n_hp++);
#ifdef BENCH_STAT
                ptrs_to_young++;
#endif
            } else {
                ASSERT(within(ptr, p));
                MOVE_CONS(ptr,val,n_htop,n_hp++);
#ifdef BENCH_STAT
                ptrs_to_young++;
#endif
            }
            continue;
          }
          case TAG_PRIMARY_HEADER: {
              if (header_is_thing(gval))
                  n_hp += (thing_arityval(gval)+1);
              else
                  n_hp++;
              continue;
          }
          default: {
            n_hp++;
            continue;
          }
        }
    }

    /* Now set the parameter pointers for the caller */
    *to = n_htop;
    return old_htop;
}

/*
 * Garbage collect the heap. However, all objects pointing
 * to the old generation heap may be left as is.
 * Every other turn, we remember the position on the heap
 * that turned out to be the heap top, every other second turn
 * we tenure all live objects that reside below that water mark.
 * This means that we only tenure objects that have survived at
 * least two collections.
*/
static void
gen_gc(Process *p, int new_sz, Eterm* objv, int nobj)
{
#ifdef UNIFIED_HEAP
    Rootset rootset[max_process];
#else
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
#endif
    Eterm* n_hstart;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    Eterm* oh_start = OLD_HEAP;
    Eterm* oh_end = OLD_HEND;
    Eterm* low_water = HEAP_START;
    Eterm* high_water = HIGH_WATER;
    Eterm* old_htop = OLD_HTOP;
    Eterm* tmp;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Generational GC (proc = %d) ", (int)pid_number(p->id));
#endif
    /* If flip is true, we need to tenure all (live) objects */
    /* within the watermarks, if flip is 0, we need to alloc a */
    /* new new_heap and copy all live objects to the new new_heap */
    /* that is to not tenure any objects at all */

#ifdef UNIFIED_HEAP
    n_hstart = (Eterm*) safe_alloc_from(805, sizeof(Eterm)*new_sz);
#else
    n_hstart = (Eterm*) erts_safe_sl_alloc_from(805, sizeof(Eterm)*new_sz);
#endif
    n_htop = n_hstart;
#ifdef UNIFIED_HEAP
    n = setup_rootset(p, objv, nobj, rootset);

    while (n--) {
      while (rootset[n].n--) {
        Eterm* g_ptr = rootset[n].v[rootset[n].n];
        Uint g_sz = rootset[n].sz[rootset[n].n];
#else
    n = setup_rootset(p, objv, nobj, &rootset);

    while (n--) {
        Eterm* g_ptr = rootset.v[n];
        Uint g_sz = rootset.sz[n];
#endif

        while (g_sz--) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {

              case TAG_PRIMARY_BOXED: {
                ptr = boxed_val(gval);
                val = *ptr;
                if (ptr_within(ptr, oh_start, oh_end))
#ifdef BENCH_STAT
                {
                    ptrs_to_old++;
#endif
                    g_ptr++;
#ifdef BENCH_STAT
                }
#endif
#if HIPE
                else if( in_area(ptr, const_start, const_size) )
#ifdef BENCH_STAT
                {
                    ptrs_to_old++;
#endif
                    ++g_ptr;
#ifdef BENCH_STAT
                }
#endif
#endif
                else if (MY_IS_MOVED(val))
                    *g_ptr++ = val;
                else if (ptr_within(ptr, low_water, high_water)) {
                    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
#ifdef BENCH_STAT
                    ptrs_to_young++;
#endif
                } else {
                    ASSERT(within(ptr, p));
                    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
#ifdef BENCH_STAT
                    ptrs_to_young++;
#endif
                }
                continue;
              }

              case TAG_PRIMARY_LIST: {
                ptr = list_val(gval);
                val = *ptr;
                if (ptr_within(ptr, oh_start, oh_end))
#ifdef BENCH_STAT
                {
                    ptrs_to_old++;
#endif
                    g_ptr++;
#ifdef BENCH_STAT
                }
#endif
#if HIPE
                else if( in_area(ptr, const_start, const_size) )
#ifdef BENCH_STAT
                {
                    ptrs_to_old++;
#endif
                    ++g_ptr;
#ifdef BENCH_STAT
                }
#endif
#endif
                else if (is_non_value(val))
                    *g_ptr++ = ptr[1];
                else if (ptr_within(ptr, low_water, high_water)) {
                    MOVE_CONS(ptr,val,old_htop,g_ptr++);
#ifdef BENCH_STAT
                    ptrs_to_young++;
#endif
                } else {
                    ASSERT(within(ptr, p));
                    MOVE_CONS(ptr,val,n_htop,g_ptr++);
#ifdef BENCH_STAT
                    ptrs_to_young++;
#endif
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
#ifdef UNIFIED_HEAP
    }
#endif

    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */

    tmp = n_htop;
    old_htop = gen_cheney(p, n_hstart, &tmp, low_water, high_water, old_htop);
    n_htop = tmp;

    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) heap.
     */

    if (OLD_HTOP < old_htop) {
        tmp = old_htop;
        (void) gen_cheney(p, OLD_HTOP, &tmp, OLD_HEAP, OLD_HEAP, NULL);
        old_htop = tmp;
    }
    OLD_HTOP = old_htop;
    HIGH_WATER = (HEAP_START != HIGH_WATER) ? n_hstart : n_htop;

#ifndef UNIFIED_HEAP
    /*
     * Now we got to move the stack to the top of the new heap...
     */
    n = p->hend - p->stop;
    sys_memcpy(n_hstart + new_sz - n, p->stop, n * sizeof(Eterm));
#endif
    HEAP_END = n_hstart + new_sz;
#ifndef UNIFIED_HEAP
    p->stop = p->hend - n;
#endif

#ifdef UNIFIED_HEAP
    {
        Uint p_index = -1;
        while (active_procs[++p_index] != NULL) {
            Process * p = active_procs[p_index];
            if(p == (Process*)NIL)
                continue;
#endif
    if (p->off_heap.mso) {
        sweep_proc_bins(p, 0);
    }
    if (p->off_heap.funs) {
        sweep_proc_funs(p, 0);
    }
#ifdef UNIFIED_HEAP
        }
    }
#endif

#ifdef DEBUG
    sys_memset(HEAP_START, 0xff, HEAP_SIZE*sizeof(Eterm));
#endif
#ifdef UNIFIED_HEAP
    sys_free((void*)HEAP_START);
#else
    erts_sl_free((void*)HEAP_START);
#endif

#ifdef UNIFIED_HEAP
    restore_rootset(rootset);
    gc_all_processes = 0;
#else
    restore_rootset(p, &rootset);
#endif

    HEAP_START = n_hstart;
    HEAP_END = n_hstart + new_sz;
    HEAP_TOP = n_htop;
    HEAP_SIZE = new_sz;
}

static void
sweep_proc_funs(Process *p, int fullsweep)
{
    ErlFunThing** prev;
    ErlFunThing* ptr;

    Eterm* bot = OLD_HEAP;
    Eterm* top = OLD_HEND;

    prev = &p->off_heap.funs;
    ptr = p->off_heap.funs;

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (MY_IS_MOVED(*ppt)) {        /* Object is alive */
            ErlFunThing* ro = (ErlFunThing *) fun_val(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {

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

    Eterm* bot = OLD_HEAP;
    Eterm* top = OLD_HEND;

    prev = &p->off_heap.mso;
    ptr = p->off_heap.mso;

    /*
     * Note: In R7 we now longer force a fullsweep when we find binaries
     * on the old heap. The reason is that with the introduction of the
     * bit syntax we can expect binaries to be used a lot more. Note that
     * in earlier releases a brand new binary (or any other term) could
     * be put on the old heap during a gen-gc fullsweep, but this is
     * no longer the case in R7.
     */
    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (MY_IS_MOVED(*ppt)) {        /* Object is alive */
            ProcBin* ro = (ProcBin*) binary_val(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {

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
                    tot_bin_allocated -= bptr->orig_size;
                    sys_free((char*)bptr);
                }
            }
            ptr = *prev;
        }
    }
    ASSERT(*prev == NULL);
}


/************************************************************** */
/*  DEBUG routines                                              */
/****************************************************************/


#ifdef HARDDEBUG

static int
within(Eterm *ptr, Process *p)
{
    ErlHeapFragment* bp = MBUF;

    if (OLD_HEAP && (OLD_HEAP <= ptr && ptr < OLD_HEND)) {
        return 1;
    }
    if (HEAP_START <= ptr && ptr < HEAP_TOP) {
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

static void
check_pointer(Process* p, Eterm obj, int back_pointers_allowed, char *msg)
{
    if (back_pointers_allowed && !within(ptr_val(obj), p)) {
        erl_exit(1, "%s: %s, line %d: %s: bad address %x\n",
                 print_pid(p), __FILE__, __LINE__, msg, obj);
    } else if (!back_pointers_allowed &&
             !ptr_within(ptr_val(obj), OLD_HEAP, OLD_HEND)) {
        if (within(ptr_val(obj), p)) {
            erl_exit(1, "%s: %s, line %d: %s: back pointer %x\n",
                     print_pid(p), __FILE__, __LINE__, msg, obj);
        } else {
            erl_exit(1, "%s: %s, line %d: %s: bad address %x\n",
                     print_pid(p), __FILE__, __LINE__, msg, obj);
        }
    }
}


static void
check_binary(Process *p, Eterm obj, char* msg)
{
    Eterm* ptr = binary_val(obj);
    int btype = thing_subtag(*ptr);

    ASSERT(is_thing(*ptr));
    ASSERT(btype == REFC_BINARY_SUBTAG);
    if (*ptr == 0xffffffff) {
        erl_exit(1, "%s: Clobbered binary left\n", print_pid(p));
    }
    check_pointer(p, obj, 1, msg);

    switch (btype) {
    case REFC_BINARY_SUBTAG:
        {
            ProcBin *bp = (ProcBin*)binary_val(obj);

            if (bp->size > 0xffffffff) {
                erl_exit(1, "%s: check_binary: %s: LARGE binary found %x\n",
                         print_pid(p), msg, obj);
            }
            if (bp->bytes == 0) {
                erl_exit(1, "%s: check_binary: %s: NULL binary found %x\n",
                         print_pid(p), msg, obj);
            }
            if (bp->val->refc <= 0) {
                erl_exit(1, "Bad refc in binary\n");
            }
        }
        break;
    default:
        erl_exit(1, "Unknown subtag in thing binary \n");
    }
}

static void
stack_element_check(Process *p, char *msg, Eterm x)
{
    switch (primary_tag(x)) {
    case TAG_PRIMARY_HEADER:    /* Continuation pointer */
        break;
    case TAG_PRIMARY_LIST:
        {
            Eterm hdr = *list_val(x);

            if (!within(list_val(x), p)) {
                erl_exit(1, "%s: check_stack: %s: bad list address %x\n",
                         print_pid(p), msg, x);
            }
            if (is_header(hdr)) {
                erl_exit(1, "%s: check_stack: "
                         "list pointer %p points to header word %08X\n",
                         print_pid(p), x, hdr);
            }
        }
        break;
    case TAG_PRIMARY_IMMED1:
        if (is_atom(x) && atom_val(x) >= atom_table_size) {
            erl_exit(1, "%s: check_stack: bad atom on stack %d\n",
                     print_pid(p), atom_val(x));
        }
        break;
    case TAG_PRIMARY_BOXED:
        {
            Eterm hdr = *boxed_val(x);

            if (!within(ptr_val(x), p)) {
                erl_exit(1, "%s: check_stack: %s: bad box address %x\n",
                         print_pid(p), msg, x);
            }
            if (!is_header(hdr)) {
                erl_exit(1, "%s: check_stack: "
                         "box pointer %p points to bad header word %08X\n",
                         print_pid(p), x, hdr);
            }
            if (is_binary_header(hdr)) {
                check_binary(p, x, msg);
            }
        }
        break;
    }
}

static void
check_stack(Process *p, char *msg)
{
    Eterm* sp;
#ifdef UNIFIED_HEAP
    if (p->stop < p->send) {
        erl_exit(1, "%s: Overrun stack\n", print_pid(p));
    }

    for (sp = p->stop; sp < p->stack; sp++) {
        if (is_not_catch(*sp)) {
            stack_element_check(p, msg, *sp);
        }
    }
#else
    Eterm* stack_end;
    int stack_size;
    int sz;

    sp = p->stop;
    stack_end = p->hend;
    sz = p->hend - p->htop;

    stack_size = stack_end - sp;
    if (sp < p->htop) {
        erl_exit(1, "%s: Overrun stack and heap\n", print_pid(p));
    }

    for (sp = p->stop; sp < p->hend; sp++) {
        if (is_not_catch(*sp)) {
            stack_element_check(p, msg, *sp);
        }
    }
#endif
}

int
chk_sys(void)
{
    Process *p = *process_tab;
    int i, res = 0;
    for (i = 0; i < max_process; i++) {
        if ((p = process_tab[i]) != NULL) {
            res++;
            check_stack(p, "chk");
        }
    }
    return res;
}

void
check_bins(Process *p)
{
    ProcBin *pb = p->off_heap.mso;
    while (pb) {
        check_binary(p, make_binary((Eterm*) pb), "CHECK");
        pb = pb->next;
    }
    return;
}

#endif  /* HARDDEBUG  */

static void
print_function_from_pc(Eterm* x, CIO fd)
{
    Eterm* addr = find_function_from_pc(x);
    if (addr == NULL) {
        if (x == beam_exit) {
            sys_printf(fd, "<terminate process>");
        } else if (x == beam_apply+1) {
            sys_printf(fd, "<terminate process normally>");
        } else {
            sys_printf(fd, "unknown function");
        }
    } else {
        display(addr[0], fd);
        sys_printf(fd, ":");
        display(addr[1], fd);
        sys_printf(fd, "/%d", addr[2]);
        sys_printf(fd, " + %d", ((x-addr)-2) * sizeof(Eterm));
    }
}

static int
stack_element_dump(Process* p, Eterm* sp, int yreg, CIO fd)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erl_printf(fd, "\n%-8p ", sp);
    } else {
        char sbuf[16];
        sprintf(sbuf, "y(%d)", yreg);
        sys_printf(fd, "%-8s ", sbuf);
        yreg++;
    }

    if (is_CP(x)) {
        sys_printf(fd, "Return addr 0x%X (", (Eterm *) x);
        print_function_from_pc(cp_val(x), fd);
        sys_printf(fd, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        sys_printf(fd, "Catch 0x%X (", catch_pc(x));
        print_function_from_pc(catch_pc(x), fd);
        sys_printf(fd, ")\n");
    } else {
        display(x, fd);
        erl_putc('\n', fd);
    }
    return yreg;
}

void
stack_dump2(Process *p, CIO fd)
{
    Eterm* sp;
    int i;
    int yreg = -1;

    erl_printf(fd, "program counter = 0x%x (", p->i);
    print_function_from_pc(p->i, fd);
    erl_printf(fd, ")\n");
    erl_printf(fd, "cp = 0x%x (", p->cp);
    print_function_from_pc(p->cp, fd);
    erl_printf(fd, ")\n");
    if (!((p->status == P_RUNNING) || (p->status == P_GARBING))) {
        erl_printf(fd, "arity = %d\n",p->arity);
        for (i = 0; i < p->arity; i++) {
            erl_printf(fd, "   ");
            display(p->arg_reg[i], fd);
            erl_printf(fd, "\n");
        }
    }
    for (sp = p->stop; sp < STACK_BEGIN; sp++) {
        yreg = stack_element_dump(p, sp, yreg, fd);
    }
}

static char*
print_pid(Process *p)
{
    char static buf[64];

    Eterm obj = p->id;
    sprintf(buf, "<%ld.%ld.%ld>",
            pid_node(obj), pid_number(obj), pid_serial(obj));
    return buf;
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
