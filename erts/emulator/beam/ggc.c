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
#include "jtab.h"
#include "beam_catches.h"

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
    uint32* v[6];		/* Pointers to vectors with terms to GC
				 * (e.g. the stack).
				 */
    uint32 sz[6];		/* Size of each vector. */
    uint32* v_msg;		/* Pointer to messages to GC. */
    uint32 def_msg[32];		/* Default storage for messages (to avoid malloc). */
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

static int heap_sizes[64];	/* Suitable heap sizes. */
static int num_heap_sizes;	/* Number of heap sizes. */

/*
** Initialize GC global data
*/
void init_gc(void)
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
offset_heap_ptr(uint32 *hp, uint32 sz, sint32 offs, 
		uint32 *low, uint32 *high)
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
offset_heap(Eterm* hp, uint32 sz, sint32 offs,
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
offset_mqueue(Process *p, sint32 offs, uint32 *low, uint32 *high) 
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

	ASSERT((is_nil(mp->seq_trace_token) || is_tuple(mp->seq_trace_token)));
	mesg = mp->seq_trace_token;
	if (is_tuple(mesg) && ptr_within(tuple_val(mesg), low, high)) {
	    mp->seq_trace_token = offset_ptr(mesg, offs);
        }
        mp = mp->next;
    }
}

static int
setup_rootset(Process* p, Eterm* objv, int nobj, Rootset *rootset) 
{
    int n;
    ErlMessage* mp;
    uint32* v_ptr;
    int v_msg_len = 2 * p->msg.len;

    /*
     * Move pointers for all messages into an array pointed to by p->v_msg.
     */
    if (v_msg_len > ALENGTH(rootset->def_msg)) {
	rootset->v_msg = (uint32 *)
	   safe_alloc_from(210, sizeof(uint32) * v_msg_len);
    } else {
	rootset->v_msg = rootset->def_msg;
    }
    mp = p->msg.first;
    v_ptr = rootset->v_msg;
    while (mp != NULL) {
        *v_ptr++ = mp->mesg;
	ASSERT((is_nil(mp->seq_trace_token) || is_tuple(mp->seq_trace_token)));
	*v_ptr++ = mp->seq_trace_token;
        mp = mp->next;
    }

    n = 0;
    rootset->v[n]  = p->stop;
    rootset->sz[n] = p->hend - p->stop;
    ++n;

    if (p->dictionary != NULL) {
	rootset->v[n]  = p->dictionary->data;
	rootset->sz[n] = p->dictionary->used;
	++n;
    }
    if (p->debug_dictionary != NULL) {
	rootset->v[n]  = p->debug_dictionary->data;
	rootset->sz[n] = p->debug_dictionary->used;
	++n;
    }
    rootset->v[n]  = rootset->v_msg;
    rootset->sz[n] = v_msg_len;
    ++n;
    if (nobj > 0) {
	rootset->v[n]  = objv;
	rootset->sz[n] = nobj;
	++n;
    }

    ASSERT((is_nil(p->seq_trace_token) || is_tuple(p->seq_trace_token)));
    rootset->v[n] = &p->seq_trace_token;
    rootset->sz[n] = 1;
    n++;

    ASSERT(n <= ALENGTH(rootset->v));
    return n;
}

static void
restore_rootset(Process *p, Rootset *rootset)
{
    uint32 *v_ptr;
    ErlMessage* mp;
    ErlHeapFragment* bp;

    /*
     * Restore all message pointers.
     */
    mp = p->msg.first;
    v_ptr = rootset->v_msg;
    while (mp != NULL) {
	mp->mesg = *v_ptr++;
	ASSERT((is_nil(*v_ptr) || is_tuple(*v_ptr)));
	mp->seq_trace_token = *v_ptr++;
	mp = mp->next;
    }
    
    if (rootset->v_msg != rootset->def_msg) {
	sys_free(rootset->v_msg);
    }

    /*
     * Remove all message buffers.
     */
    bp = p->mbuf;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    while (bp != NULL) {
	ErlHeapFragment* next_bp = bp->next;
#ifdef DEBUG
	sys_memset(bp->mem, 0xff, bp->size*sizeof(uint32));
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
    Rootset rootset;
    Eterm* n_hstart;		/* Start of new heap */
    Eterm* n_htop;		/* Top of new heap */
    Eterm* n_heap;		/* The new heap */
    Eterm* n_hp;
    int n;

    /* Create new, empty heap */
    n_heap = (uint32 *) safe_sl_alloc_from(803, sizeof(uint32)*new_sz);
    n_hstart = n_htop = n_heap;
    p->flags &= ~F_NEED_FULLSWEEP;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Fullsweep GC proc: %d\n", print_pid(p));
#endif
    n = setup_rootset(p, objv, nobj, &rootset);

    while (n--) {
	uint32* g_ptr = rootset.v[n];
	uint32 g_sz = rootset.sz[n];
	
	while(g_sz--) {
	    uint32 *ptr;
	    uint32 val;
	    uint32 gval = *g_ptr;

	    switch (primary_tag(gval)) {

	      case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (MY_IS_MOVED(val))
		    *g_ptr++ = val;
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


    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
     */

    n_hp = n_hstart;
    
    while (n_hp != n_htop) {
	uint32 *ptr;
	uint32 val;
	uint32 gval = *n_hp;

	switch (primary_tag(gval)) {

	  case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (MY_IS_MOVED(val))
		*n_hp++ = val;
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

    if (p->off_heap.mso) {
	sweep_proc_bins(p, 1);
    }
    if (p->off_heap.funs) {
	sweep_proc_funs(p, 1);
    }

    restore_rootset(p, &rootset);

    if (p->old_heap != NULL) {
#ifdef DEBUG
	sys_memset(p->old_heap, 0xff, (p->old_hend - p->old_heap)*sizeof(Eterm));
#endif    
	sys_sl_free(p->old_heap);
	p->old_heap = p->old_htop = p->old_hend = NULL;
    }

    /* Move the stack, the beam stack is "in the heap" */
    n = p->hend - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
#ifdef DEBUG
    sys_memset(p->heap, 0xff, (p->hend - p->heap)*sizeof(Eterm));
#endif    
    p->hend = n_heap + new_sz;
    p->stop = p->hend - n;
    sys_sl_free(p->heap);

    p->heap = n_heap;
    p->htop = n_htop;
    p->heap_sz = new_sz;
    p->gen_gcs = 0;

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

    p->high_water = p->htop;
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

static void
offset_rootset(Process *p, int offs, 
	       uint32 *low, uint32 *high, 
	       uint32 *objv, int nobj)
{
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
    offset_heap_ptr(p->stop, (p->hend - p->stop), offs, low, high);
    if (nobj > 0) {
	offset_heap(objv, nobj, offs, low, high);
    }
    offset_off_heap(p, low, high, offs);
}



/*
 * Grow the new heap size to 'new_sz'.
 */
static void
grow_new_heap(Process *p, uint32 new_sz, Eterm* objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = p->htop - p->heap;
    int stack_size = p->hend - p->stop;
    sint32 offs;

    ASSERT(p->heap_sz < new_sz);
    new_heap = (Eterm *) safe_sl_realloc((void*)p->heap,
					 sizeof(Eterm)*(p->heap_sz),
					 sizeof(Eterm)*new_sz);

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "grow_new_heap: GREW (%d) FROM %d UPTO %d (used %d)\n",
	    pid_number(p->id), p->heap_sz, new_sz, heap_size);
#endif

    if ((offs = new_heap - p->heap) == 0) { /* No move. */
	p->hend = new_heap + new_sz;
	sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
	p->stop = p->hend - stack_size;
    } else {
	Eterm* prev_stop = p->stop;

	offset_heap(new_heap, heap_size, offs, p->heap, p->htop);
	p->high_water = new_heap + (p->high_water - p->heap);

	prev_stop = new_heap + (p->stop - p->heap);
	p->hend = new_heap + new_sz;
	p->stop = p->hend - stack_size;
	sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));
	offset_rootset(p, offs, p->heap, p->htop, objv, nobj);
	p->htop = new_heap + heap_size;
	p->heap = new_heap;
    }
    p->heap_sz = new_sz;
}

static void
shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = p->htop - p->heap;
    int stack_size = p->hend - p->stop;
    sint32 offs;

    ASSERT(new_sz < p->heap_sz);
    sys_memmove(p->heap + new_sz - stack_size, p->stop, stack_size * sizeof(Eterm));
    new_heap = (Eterm *) safe_sl_realloc((void*)p->heap,
					 sizeof(Eterm)*(p->heap_sz),
					 sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "shrink_new_heap: SHRINKED (%d) FROM %d DOWNTO %d (used %d)\n",
	    pid_number(p->id), p->heap_sz, new_sz, heap_size);
#endif

    if ((offs = new_heap - p->heap) != 0) {

	/*
	 * Normally, we don't expect a shrunk heap to move, but you never
	 * knows on some strange embedded systems...  Or when using purify.
	 */

	offset_heap(new_heap, heap_size, offs, p->heap, p->htop);
	p->high_water = new_heap + (p->high_water - p->heap);
	offset_rootset(p, offs, p->heap, p->htop, objv, nobj);
	p->htop = new_heap + heap_size;
	p->heap = new_heap;
    }
    p->heap_sz = new_sz;
}

static void 
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
    int stack_size =  p->hend - p->stop;

    size_after = (p->htop - p->heap);
    reclaimed += (size_before - size_after);
     
     /*
      * Resize the heap if needed.
      */
     
    need_after = size_after + need + stack_size;
    if (p->heap_sz < need_after) {
	/* Too small - grow to match requested need */
	sz = next_heap_size(need_after, 0);
	grow_new_heap(p, sz, objv, nobj);
    } else if (3 * p->heap_sz < 4 * need_after){
	/* Need more than 75% of current, postpone to next GC.*/
	p->flags |= F_HEAP_GROW;
    } else if (4 * need_after < p->heap_sz && p->heap_sz > H_MIN_SIZE){
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
	if (sz < p->heap_sz) {
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
    int stack_size;		/* Size of stack ON HEAP. */
    int sz;

#define OverRunCheck() \
    if (p->stop < p->htop) { \
	erl_exit(1, "%s: Overrun stack and heap at line %d\n", print_pid(p),__LINE__); \
    } 
	 
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
	trace_gc(p, am_gc_start);
    }
    saved_status = p->status;
    p->status = P_GARBING;
    CHECK(p);
    OverRunCheck();
    if (p->gen_gcs >= p->max_gen_gcs) {
	p->flags |= F_NEED_FULLSWEEP;
    }
    p->arith_avail = 0;
    p->arith_heap = NULL;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
    p->off_heap.overhead = 0;
    garbage_cols++;

    stack_size = p->hend - p->stop;

    /* Size of heap before first GC */
    size_before = p->mbuf_sz + (p->htop - p->heap);

    /*
     * Should we grow although we don't actually need to?
     */

    if (sz == p->heap_sz && p->flags & F_HEAP_GROW) {
	sz = next_heap_size(p->heap_sz, 1);
	p->flags &= ~F_HEAP_GROW;
    }

    /*
     * Generational GC from here on. We need an old heap.
     */

    if (p->old_heap == NULL && p->high_water != p->heap &&
	(p->flags & F_NEED_FULLSWEEP) == 0) {
	Eterm* n_old;
	/* Note: We choose a larger heap size than strictly needed,
	 * which seems to reduce the number of fullsweeps.
	 * This improved Estone by more than 1200 estones on my computer
	 * (Ultra Sparc 10).
	 */
	size_t new_sz = next_heap_size(p->high_water - p->heap, 1);

	/* Create new, empty old_heap */
	n_old = (uint32 *) safe_sl_alloc_from(801, sizeof(Eterm)*new_sz);

	p->old_hend = n_old + new_sz;
	p->old_heap = p->old_htop = n_old;
#ifdef OLD_HEAP_CREATION_TRACE
	fprintf(stderr, "Created old_heap for %s.\r\n", print_pid(p));
#endif
    }

    /*
     * Try a generational GC if the old heap is big enough and
     * there are no old binaries on it.
     */

    if ((p->flags & F_NEED_FULLSWEEP) == 0 &&
	p->high_water - p->heap <= p->old_hend - p->old_htop) {

	/*
	 * There is space enough in old_heap for everything
	 * below the high water mark.  Do a generational GC.
	 */
	
	gen_gc(p, next_heap_size(p->heap_sz + p->mbuf_sz, 0), objv, nobj);
	p->gen_gcs++;
	size_after = p->htop - p->heap;
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

  	if ((p->heap_sz > 300) && (4 * need_after < p->heap_sz) &&
 	    ((p->heap_sz > 8000) ||
 	     (p->heap_sz > (p->old_hend - p->old_heap)))) {
 	    wanted = 3 * need_after;
	    if (wanted < p->min_heap_size) {
		wanted = p->min_heap_size;
	    } else {
		wanted = next_heap_size(wanted, 0);
	    }
	    if (wanted < p->heap_sz) {
		shrink_new_heap(p, wanted, objv, nobj);
	    }
	    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
	    goto done;
	}

	/*
	 * The heap size turned out to be just right. We are done.
	 */

	if (p->heap_sz >= need_after) {
	    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
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
    
    sz = p->heap_sz + p->mbuf_sz  + (p->old_htop - p->old_heap);
    sz += p->hend - p->stop;
    sz = next_heap_size(sz, 0);
    fullsweep_heap(p, sz, objv, nobj);
    CHECK(p);
    adjust_after_fullsweep(p, size_before, need, objv, nobj);
	
 done:
    CHECK(p);
    OverRunCheck();
    p->status = saved_status;
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
	trace_gc(p, am_gc_end);
    }
    return ((int) (p->htop - p->heap) / 10);
#undef OverRunCheck()
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
    uint32 *n_hp, *n_htop;
    uint32 *oh_start, *oh_end;

    uint32 *ptr;
    uint32 val;
    uint32 gval;

    n_hp = from; 
    n_htop = *to; 
    oh_start = p->old_heap;
    oh_end = p->old_hend;

    while (n_hp != n_htop) {
	gval = *n_hp;

	switch (primary_tag(gval)) {
	  case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (MY_IS_MOVED(val))
		*n_hp++ = val;
	    else if (ptr_within(ptr, low_water, high_water)) {
		/* Make object old */
		MOVE_BOXED(ptr,val,old_htop,n_hp++);
	    } else {
		ASSERT(within(ptr, p));
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
	    }
	    continue;
	  }
	  case TAG_PRIMARY_LIST: {
	    ptr = list_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_non_value(val))
		*n_hp++ = ptr[1];
	    else if (ptr_within(ptr, low_water, high_water)) {
		/* Make object old */
		MOVE_CONS(ptr,val,old_htop,n_hp++);
	    } else {
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
    Rootset rootset;		/* Rootset for GC (stack, dictionary, etc). */
    Eterm* n_hstart;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    Eterm* oh_start = p->old_heap;
    Eterm* oh_end = p->old_hend;
    Eterm* low_water = p->heap;
    Eterm* high_water = p->high_water;
    Eterm* old_htop = p->old_htop;
    Eterm* tmp;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Generational GC (proc = %d) ", (int)pid_number(p->id));
#endif

    /* If flip is true, we need to tenure all (live) objects */
    /* within the watermarks, if flip is 0, we need to alloc a */
    /* new new_heap and copy all live objects to the new new_heap */
    /* that is to not tenure any objects at all */

    n_hstart = (Eterm*) safe_sl_alloc_from(805, sizeof(Eterm)*new_sz);
    n_htop = n_hstart;
    n = setup_rootset(p, objv, nobj, &rootset);

    while (n--) {
	Eterm* g_ptr = rootset.v[n];
	Uint g_sz = rootset.sz[n];
	
	while (g_sz--) {
	    gval = *g_ptr;

	    switch (primary_tag(gval)) {

	      case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (MY_IS_MOVED(val))
		    *g_ptr++ = val;
		else if (ptr_within(ptr, low_water, high_water)) {
		    MOVE_BOXED(ptr,val,old_htop,g_ptr++);
		} else {
		    ASSERT(within(ptr, p));
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
		}
		continue;
	      }

	      case TAG_PRIMARY_LIST: {
		ptr = list_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_non_value(val))
		    *g_ptr++ = ptr[1];
		else if (ptr_within(ptr, low_water, high_water)) {
		    MOVE_CONS(ptr,val,old_htop,g_ptr++);
		} else {
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

    if (p->old_htop < old_htop) {
	tmp = old_htop;
	(void) gen_cheney(p, p->old_htop, &tmp, p->old_heap, p->old_heap, NULL);
	old_htop = tmp;
    }
    p->old_htop = old_htop;
    p->high_water = (p->heap != p->high_water) ? n_hstart : n_htop;

    /* 
     * Now we got to move the stack to the top of the new heap...
     */
    n = p->hend - p->stop;
    sys_memcpy(n_hstart + new_sz - n, p->stop, n * sizeof(Eterm));
    p->hend = n_hstart + new_sz;
    p->stop = p->hend - n;

    if (p->off_heap.mso) {
	sweep_proc_bins(p, 0);
    }
    if (p->off_heap.funs) {
	sweep_proc_funs(p, 0);
    }

#ifdef DEBUG
    sys_memset(p->heap, 0xff, p->heap_sz*sizeof(Eterm));
#endif
    sys_sl_free((void*)p->heap);

    restore_rootset(p, &rootset);
    
    p->heap = n_hstart;
    p->hend = n_hstart + new_sz;
    p->htop = n_htop;
    p->heap_sz = new_sz;
}

static void
sweep_proc_funs(Process *p, int fullsweep)
{
    ErlFunThing** prev;
    ErlFunThing* ptr;

    Eterm* bot = p->old_heap;
    Eterm* top = p->old_hend;

    prev = &p->off_heap.funs;
    ptr = p->off_heap.funs;

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;
            
	if (MY_IS_MOVED(*ppt)) {	/* Object is alive */
            ErlFunThing* ro = (ErlFunThing *) fun_val(*ppt);

            *prev = ro;		/* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {

            /*
	     * Object resides on old heap, and we just did a
	     * generational collection - keep object in list.
	     */
            prev = &ptr->next;
            ptr = ptr->next;
	} else {		/* Object has not been moved - deref it */
            *prev = ptr = ptr->next;
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

    Eterm* bot = p->old_heap;
    Eterm* top = p->old_hend;

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
            
	if (MY_IS_MOVED(*ppt)) {	/* Object is alive */
            ProcBin* ro = (ProcBin*) binary_val(*ppt);

            *prev = ro;		/* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {

            /*
	     * Object resides on old heap, and we just did a
	     * generational collection - keep object in list.
	     */
            prev = &ptr->next;
            ptr = ptr->next;
	} else {		/* Object has not been moved - deref it */
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
    ErlHeapFragment* bp = p->mbuf;

    if (p->old_heap && (p->old_heap <= ptr && ptr < p->old_hend)) {
	return 1;
    }
    if (p->heap <= ptr && ptr < p->htop) {
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
             !ptr_within(ptr_val(obj), p->old_heap, p->old_hend)) {
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
    case TAG_PRIMARY_HEADER:	/* Continuation pointer */
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
        check_binary(p, make_binary((uint32*) pb), "CHECK");
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
    for (sp = p->stop; sp < p->hend; sp++) {
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
