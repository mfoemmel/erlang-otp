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

static int setup_rootset(Process*, Eterm*, int, Rootset*);
static void gen_gc(Process*, int, Eterm*, int);
static char* print_pid(Process* p);
static void fullsweep_gc_binary(Process *p, Eterm *g_ptr, Eterm **n_htop_p);
static void gen_gc_binary(Process *p, uint32 *g_ptr, Eterm** n_htop_p,
			  Eterm** old_htop_p, int tenure, uint32* lw,
			  Eterm* hw, Eterm* ohs, Eterm* ohe);
static void sweep_proc_bins(Process *p, int fullsweep);
static void sweep_proc_funs(Process *p, int fullsweep);

#ifdef HARDDEBUG
static void check_stack(Process*, char*);
static int within(Eterm*, Process*);
static void heap_dump(Process *p, Eterm* from, Eterm* to);
static int where_pointer(Process *p, Eterm* from, Eterm* to, Eterm obj, CIO fd);

void check_bins(Process *p);
int chk_sys(void);


#define CHECK(p) \
    check_stack((p), "check"); \
    check_bins(p);
#else
# define within(x,y) 1
# define CHECK(p) ((void) 1)
#endif


#define ptr_within(ptr, x, y) (((ptr) >= (x) && (ptr) < (y)))

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
    DECL_JVALUE(v, CP0_DEF)
    DECL_JVALUE(v, CP4_DEF)
    DECL_JVALUE(v, CP8_DEF)
    DECL_JVALUE(v, CP12_DEF)
    DECL_JVALUE(v, SMALL_DEF)
    DECL_JVALUE(v, BIG_DEF)
    DECL_JVALUE(v, FLOAT_DEF)
    DECL_JVALUE(v, ATOM_DEF)
    DECL_JVALUE(v, REFER_DEF)
    DECL_JVALUE(v, PORT_DEF)
    DECL_JVALUE(v, PID_DEF)
    DECL_JVALUE(v, TUPLE_DEF)
    DECL_JVALUE(v, CATCH_DEF)
    DECL_JVALUE(v, LIST_DEF)
    DECL_JVALUE(v, BLANK_DEF)
    DECL_JVALUE(v, BINARY_DEF)
    DECL_JTABLE(v, 16)

    int ignore_hilo = (high == 0 && low == 0);

    if (JTABLE_NEED_INIT(v)) {
	DEFINE_LOCATION(v, CP0_DEF);
	DEFINE_LOCATION(v, CP4_DEF);
	DEFINE_LOCATION(v, CP8_DEF);
	DEFINE_LOCATION(v, CP12_DEF);
	DEFINE_LOCATION(v, SMALL_DEF);
	DEFINE_LOCATION(v, BIG_DEF);
	DEFINE_LOCATION(v, FLOAT_DEF);
	DEFINE_LOCATION(v, ATOM_DEF);
	DEFINE_LOCATION(v, REFER_DEF);
	DEFINE_LOCATION(v, PORT_DEF);
	DEFINE_LOCATION(v, PID_DEF);
	DEFINE_LOCATION(v, TUPLE_DEF);
	DEFINE_LOCATION(v, CATCH_DEF);
	DEFINE_LOCATION(v, LIST_DEF);
	DEFINE_LOCATION(v, BLANK_DEF);
	DEFINE_LOCATION(v, BINARY_DEF);
	DEFINE_JTABLE(v);
    }

    while (sz--) {
	Eterm val = *hp;
	JUMP(v, tag_val_def(val));

	LOCATION(v, BIG_DEF);
	LOCATION(v, REFER_DEF);
	LOCATION(v, FLOAT_DEF);
	LOCATION(v, LIST_DEF);
	LOCATION(v, BINARY_DEF);
	LOCATION(v, TUPLE_DEF) {
	    if (ignore_hilo || ptr_within(ptr_val(val), low, high)) {
		*hp = offset_ptr(val, offs);
	    }
	    hp++;
	    continue;
	}
	LOCATION(v, SMALL_DEF);
	LOCATION(v, CP0_DEF);
	LOCATION(v, CP4_DEF);
	LOCATION(v, CP8_DEF);
	LOCATION(v, CP12_DEF);
	LOCATION(v, CATCH_DEF);
	LOCATION(v, ATOM_DEF);
	LOCATION(v, PID_DEF);
	LOCATION(v, PORT_DEF);
	LOCATION(v, BLANK_DEF) {
	    hp++;
	    continue;
	}
	JUMP_END
    }
}

/*
** Offset pointers into the heap (not stack).
** Only offset pointers that point into the interval of low and high 
** unless both low and high == 0, then we offset all pointers
*/

static void 
offset_heap(Eterm* hp, uint32 sz, sint32 offs,
	    Eterm* low, Eterm* high)
{
    DECL_JVALUE(v, SMALL_DEF)
    DECL_JVALUE(v, BIG_DEF)
    DECL_JVALUE(v, FLOAT_DEF)
    DECL_JVALUE(v, ATOM_DEF)
    DECL_JVALUE(v, REFER_DEF)
    DECL_JVALUE(v, PORT_DEF)
    DECL_JVALUE(v, PID_DEF)
    DECL_JVALUE(v, TUPLE_DEF)
    DECL_JVALUE(v, LIST_DEF)
    DECL_JVALUE(v, ARITYVAL_DEF)
    DECL_JVALUE(v, MOVED_DEF)
    DECL_JVALUE(v, THING_DEF)
    DECL_JVALUE(v, BINARY_DEF)
    DECL_JTABLE(v, 16)

    int ignore_hilo = (high == 0 && low == 0);

    if (JTABLE_NEED_INIT(v)) {
	DEFINE_LOCATION(v, SMALL_DEF);
	DEFINE_LOCATION(v, BIG_DEF);
	DEFINE_LOCATION(v, FLOAT_DEF);
	DEFINE_LOCATION(v, ATOM_DEF);
	DEFINE_LOCATION(v, REFER_DEF);
	DEFINE_LOCATION(v, PORT_DEF);
	DEFINE_LOCATION(v, PID_DEF);
	DEFINE_LOCATION(v, TUPLE_DEF);
	DEFINE_LOCATION(v, LIST_DEF);
	DEFINE_LOCATION(v, ARITYVAL_DEF);
	DEFINE_LOCATION(v, MOVED_DEF);
	DEFINE_LOCATION(v, THING_DEF);
	DEFINE_LOCATION(v, BINARY_DEF);
	DEFINE_JTABLE(v);
    }

    while (sz--) {
	Eterm val = *hp;
	JUMP(v, tag_val_def(val));

	LOCATION(v,REFER_DEF);
	LOCATION(v,BIG_DEF);
	LOCATION(v,FLOAT_DEF);
	LOCATION(v,LIST_DEF);
	LOCATION(v,BINARY_DEF);
	LOCATION(v,TUPLE_DEF) {
	    if (ignore_hilo || ptr_within(ptr_val(val), low, high)) {
		*hp = offset_ptr(val, offs);
	    }
	    hp++;
	    continue;
	}
	LOCATION(v,THING_DEF) {
	    Uint tari = thing_arityval(val);

            switch thing_subtag(val) {
            case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb = (ProcBin*) hp;
		    Eterm** uptr = (Eterm **) &pb->next;
		    
		    if (*uptr &&
			(ignore_hilo || ptr_within((Eterm *)pb->next, low, high))) {
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

		    if (*uptr && (ignore_hilo ||
				  ptr_within((Eterm *)funp->next, low, high))) {
			*uptr += offs;
		    }
		    sz -= tari;
		    hp += tari + 1;
		}
		break;
	    case SUB_BINARY_SUBTAG:
		erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
			 __FILE__, __LINE__);
		break;
	    case HEAP_BINARY_SUBTAG:
		erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
			 __FILE__, __LINE__);
		break;
	    default:
		sz -= tari;
                hp += tari + 1;
            }
	    continue;
	}
	LOCATION(v, SMALL_DEF);
	LOCATION(v, ATOM_DEF);
	LOCATION(v, PID_DEF);
	LOCATION(v, PORT_DEF);
	LOCATION(v, ARITYVAL_DEF) {
	    hp++;
	    continue;
	}
	LOCATION(v, MOVED_DEF) {
	    erl_exit(1, "move mark found: 0x%08x at 0x%08x\n",
		     val, hp);
	}
	JUMP_END
    }
}


/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, sint32 offs, uint32 *low, uint32 *high) 
{
    ErlMessage* mp = p->msg.first;
    int ignore_hilo = (high == 0 && low == 0);

    while (mp != NULL) {
        uint32 mesg = mp->mesg;
        switch(tag_val_def(mesg)) {
        case REFER_DEF:
        case BINARY_DEF:
        case BIG_DEF:
        case FLOAT_DEF:
        case LIST_DEF:
        case TUPLE_DEF:
	    if (ignore_hilo || (ptr_within(ptr_val(mesg), low, high)))
		mp->mesg = offset_ptr(mesg, offs);
            break;
        }

	ASSERT((is_nil(mp->seq_trace_token) || is_tuple(mp->seq_trace_token)));
	mesg = mp->seq_trace_token;
	if (is_tuple(mesg) && (ignore_hilo || (ptr_within(ptr_val(mesg), low, high)))) {
	    mp->seq_trace_token = offset_ptr(mesg, offs);
        }
        mp = mp->next;
    }
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
**  Garbage collect a heap, copy all data to a new fresh heap
**  Add garbage collect for objects in
**  objv as well. And update them.
**  Returns 0 if OK, <0 on error.
**  Parameters:
**  p: The process to build rootset from.
**  n_hstart: The start of the new heap (where saved objects 
**  are moved).
**  pn_htop: [in-out] Address of pointer to the new heap top,
**  should contain new value upon return from this function.
**  objv: Vector of "extra" objects to be "saved".
**  nobj: Number of objects in objv. 
*/

static void
do_fullsweep_gc(Process *p, uint32 *n_hstart, uint32 **pn_htop, 
		uint32 *objv, int nobj)
{
    /* Stack scan jump table */
    DECL_JVALUE(vs, CP0_DEF)
    DECL_JVALUE(vs, CP4_DEF)
    DECL_JVALUE(vs, CP8_DEF)
    DECL_JVALUE(vs, CP12_DEF)
    DECL_JVALUE(vs, BLANK_DEF)
    DECL_JVALUE(vs, SMALL_DEF)
    DECL_JVALUE(vs, BIG_DEF)
    DECL_JVALUE(vs, FLOAT_DEF)
    DECL_JVALUE(vs, ATOM_DEF)
    DECL_JVALUE(vs, REFER_DEF)
    DECL_JVALUE(vs, PORT_DEF)
    DECL_JVALUE(vs, PID_DEF)
    DECL_JVALUE(vs, TUPLE_DEF)
    DECL_JVALUE(vs, LIST_DEF)
    DECL_JVALUE(vs, CATCH_DEF)
    DECL_JVALUE(vs, BINARY_DEF)

    /* Heap scan jump table */
    DECL_JVALUE(hs, CP0_DEF)
    DECL_JVALUE(hs, CP4_DEF)
    DECL_JVALUE(hs, CP8_DEF)
	/* DECL_JVALUE(hs, CP12_DEF) same as MOVED_DEF */
    DECL_JVALUE(hs, SMALL_DEF)
    DECL_JVALUE(hs, BIG_DEF)
    DECL_JVALUE(hs, FLOAT_DEF)
    DECL_JVALUE(hs, ATOM_DEF)
    DECL_JVALUE(hs, REFER_DEF)
    DECL_JVALUE(hs, PORT_DEF)
    DECL_JVALUE(hs, PID_DEF)
    DECL_JVALUE(hs, TUPLE_DEF)
    DECL_JVALUE(hs, LIST_DEF)
    DECL_JVALUE(hs, ARITYVAL_DEF)
    DECL_JVALUE(hs, MOVED_DEF)
    DECL_JVALUE(hs, THING_DEF)
    DECL_JVALUE(hs, BINARY_DEF)

    DECL_JTABLE(vs, 16)
    DECL_JTABLE(hs, 16)

    Rootset rootset;
    uint32 *n_htop;
    uint32 *n_hp;
    int tmp;
    int n;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Fullsweep GC proc: %d\n", (int)get_number(p->id));
#endif
    if (JTABLE_NEED_INIT(vs) || JTABLE_NEED_INIT(hs)) {
	DEFINE_LOCATION(vs, CP0_DEF);
	DEFINE_LOCATION(vs, CP4_DEF);
	DEFINE_LOCATION(vs, CP8_DEF);
	DEFINE_LOCATION(vs, CP12_DEF);
	DEFINE_LOCATION(vs, BLANK_DEF);
	DEFINE_LOCATION(vs, SMALL_DEF);
	DEFINE_LOCATION(vs, BIG_DEF);
	DEFINE_LOCATION(vs, FLOAT_DEF);
	DEFINE_LOCATION(vs, ATOM_DEF);
	DEFINE_LOCATION(vs, REFER_DEF);
	DEFINE_LOCATION(vs, PORT_DEF);
	DEFINE_LOCATION(vs, PID_DEF);
	DEFINE_LOCATION(vs, TUPLE_DEF);
	DEFINE_LOCATION(vs, LIST_DEF);
	DEFINE_LOCATION(vs, CATCH_DEF);
	DEFINE_LOCATION(vs, BINARY_DEF);
	DEFINE_JTABLE(vs);

	DEFINE_LOCATION(hs, CP0_DEF);
	DEFINE_LOCATION(hs, CP4_DEF);
	DEFINE_LOCATION(hs, CP8_DEF);
	/* DEFINE_LOCATION(hs, CP12_DEF); Same as MOVED_DEF */
	DEFINE_LOCATION(hs, SMALL_DEF);
	DEFINE_LOCATION(hs, BIG_DEF);
	DEFINE_LOCATION(hs, FLOAT_DEF);
	DEFINE_LOCATION(hs, ATOM_DEF);
	DEFINE_LOCATION(hs, REFER_DEF);
	DEFINE_LOCATION(hs, PORT_DEF);
	DEFINE_LOCATION(hs, PID_DEF);
	DEFINE_LOCATION(hs, TUPLE_DEF);
	DEFINE_LOCATION(hs, LIST_DEF);
	DEFINE_LOCATION(hs, ARITYVAL_DEF);
	DEFINE_LOCATION(hs, MOVED_DEF);
	DEFINE_LOCATION(hs, THING_DEF);
	DEFINE_LOCATION(hs, BINARY_DEF);
	DEFINE_JTABLE(hs);
    }
    n_htop = *pn_htop; /* Hopefully optimized into register */
    n = setup_rootset(p, objv, nobj, &rootset);

    while (n--) {
	uint32* g_ptr = rootset.v[n];
	uint32 g_sz = rootset.sz[n];
	
	while(g_sz--) {
	    uint32 *ptr;
	    uint32 val;
	    uint32 gval = *g_ptr;

	    JUMP(vs, tag_val_def(gval));

	    LOCATION(vs, FLOAT_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (is_moved(val))
		    *g_ptr++ = make_float(ptr_val(val));
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_float(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    *n_htop++ = *ptr++;          /* Copy float part 1 */
		    *n_htop++ = *ptr;            /* Copy float part 2 */
		}
		continue;
	    }

	    LOCATION(vs, REFER_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (is_moved(val))
		    *g_ptr++ = make_refer(ptr_val(val));
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_refer(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(vs, BIG_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (is_moved(val))
		    *g_ptr++ = make_big(ptr_val(val));
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_big(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(vs, TUPLE_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (is_moved(val))
		    *g_ptr++ = make_tuple(ptr_val(val));
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_arity_value(val));
		    *g_ptr++ = gval = make_tuple(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store arity val */
		    tmp = arityval(val);          /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(vs, LIST_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (is_moved(val))
		    *g_ptr++ = make_list(ptr_val(val));
		else {
		    ASSERT(within(ptr, p));
		    *g_ptr++ = gval = make_list(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store head */
		    *n_htop++ = *ptr;            /* Store tail */
		}
		continue;
	    }

	    LOCATION(vs, CP0_DEF);
	    LOCATION(vs, CP4_DEF);
	    LOCATION(vs, CP8_DEF);
	    LOCATION(vs, CP12_DEF);
	    LOCATION(vs, BLANK_DEF);
	    LOCATION(vs, CATCH_DEF);
	    LOCATION(vs, SMALL_DEF);
	    LOCATION(vs, ATOM_DEF);
	    LOCATION(vs, PID_DEF);
	    LOCATION(vs, PORT_DEF) {
		g_ptr++;
		continue;
	    }

	    LOCATION(vs, BINARY_DEF) {
		fullsweep_gc_binary(p, g_ptr, &n_htop);
		g_ptr++;
		continue;
	    }
	    JUMP_END;
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

	JUMP(hs, tag_val_def(gval));
	LOCATION(hs, FLOAT_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (is_moved(val))
		*n_hp++ = make_float(ptr_val(val));
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_float(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		*n_htop++ = *ptr++;          /* Copy float part 1 */
		*n_htop++ = *ptr;            /* Copy float part 2 */
	    }
	    continue;
	}

	LOCATION(hs, REFER_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (is_moved(val))
		*n_hp++ = make_refer(ptr_val(val));
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_refer(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(hs, BIG_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (is_moved(val))
		*n_hp++ = make_big(ptr_val(val));
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_big(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(hs, TUPLE_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (is_moved(val))
		*n_hp++ = make_tuple(ptr_val(val));
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_arity_value(val));
		*n_hp++ = gval = make_tuple(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store arity val */
		tmp = arityval(val);          /* Get arity value */
		while(tmp--)
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(hs, LIST_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (is_moved(val))
		*n_hp++ = make_list(ptr_val(val));
	    else {
		ASSERT(within(ptr, p));
		*n_hp++ = gval = make_list(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store head */
		*n_htop++ = *ptr;            /* Store tail */
	    }
	    continue;
	}

	LOCATION(hs, THING_DEF) {
	    n_hp += (thing_arityval(gval)+1);
	    continue;
	}

	LOCATION(hs, SMALL_DEF);
	LOCATION(hs, ATOM_DEF);
	LOCATION(hs, PID_DEF);
	LOCATION(hs, PORT_DEF);
	LOCATION(hs, ARITYVAL_DEF) {
	    n_hp++;
	    continue;
	}
	LOCATION(hs, BINARY_DEF) {
	    fullsweep_gc_binary(p, n_hp, &n_htop);
	    n_hp++;
	    continue;
	}

	LOCATION(hs, CP0_DEF);
	LOCATION(hs, CP4_DEF);
	LOCATION(hs, CP8_DEF);
	/* LOCATION(hs, CP12_DEF);  Same as MOVED_DEF */
	LOCATION(hs, MOVED_DEF) {
	    display(p->id, CERR);
	    erl_exit(1, "%s: GC: bad data on heap - pass 2 0x%08x at 0x%08x\n",
		     print_pid(p), gval, n_hp);
	}
	JUMP_END;
    }

    if (p->off_heap.mso) {
	sweep_proc_bins(p, 1);
    }
    if (p->off_heap.funs) {
	sweep_proc_funs(p, 1);
    }

    restore_rootset(p, &rootset);
    *pn_htop = n_htop;
}


static int
create_old_heap(Process *p, int new_sz)
{
    uint32 *n_old;
   
    /* Create new,empty old_heap */
    n_old = (uint32 *) safe_alloc_from(801,sizeof(uint32)*new_sz);

    p->old_hend = n_old + new_sz;
    p->old_heap = n_old;
    p->old_htop = n_old;
    return(0);
}
/* 
** Set up parameters and call do_fullsweep_gc.
** This function is used when using gen_gc and the old_heap
** needs garbing.
** Parameters:
** p: The process who is being garbage collected.
** new_sz: The wanted size of the old_heap after collecting.
** objv: Vector of "extra" objects to be "saved".
** nobj: Number of objects in objv.
*/
static void
fullsweep_old_heap(Process* p, int new_sz, Eterm* objv, int nobj)
{
    Eterm* n_hstart;     /* Start of new (old_)heap */
    Eterm* n_htop;       /* Top of new (old_)heap */
    Eterm* n_old;
    uint32 saved_status;  /* Save the process status.*/
   
    /* Create new, empty old_heap */
    n_old = (uint32 *) safe_alloc_from(802,sizeof(uint32)*new_sz);
    /* high_water, low_water and hend are set up after fullsweep, 
       when new_heap is empty. */

    n_hstart = n_htop = n_old;
    saved_status = p->status;
    p->status = P_GARBING;
    p->flags &= ~F_NEED_FULLSWEEP;
    do_fullsweep_gc(p, n_hstart, &n_htop, objv, nobj);
    if (p->off_heap.mso) {
	p->flags |= F_NEED_FULLSWEEP;
    }
    
    /* new_heap is empty and old old_heap is to be discarded. */
    p->htop = p->heap;

    if (p->old_heap != NULL){
#ifdef DEBUG
	sys_memset(p->old_heap, 0xff,
		   (p->old_hend - p->old_heap)*sizeof(Eterm));
#endif
	sys_free(p->old_heap);
    }
    p->old_hend = n_old + new_sz;
    p->old_heap = n_old;
    p->old_htop = n_htop;
    p->low_water = p->high_water = p->heap;
    p->flags &= ~F_GCFLIP; /* Reset generational state
			      whatever it was before this. */

    p->status = saved_status;
}

/* 
** Set up parameters and call do_fullsweep_gc.
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
    uint32 *n_hstart;		/* Start of new heap */
    uint32 *n_htop;		/* Top of new heap */
    uint32 *n_heap;		/* The new heap */
    uint32 saved_status;	/* Save the process status.*/
    int n;

    /* Create new, empty heap */
    n_heap = (uint32 *) safe_alloc_from(803,sizeof(uint32)*new_sz);

    n_hstart = n_htop = n_heap;
    saved_status = p->status;
    p->status = P_GARBING;
    do_fullsweep_gc(p, n_hstart, &n_htop, objv, nobj);

    /* Move the stack, the beam stack is "in the heap" */
    n = p->hend - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
    p->hend = n_heap + new_sz;
    p->stop = p->hend - n;
    
#ifdef DEBUG
    sys_memset(p->heap, 0xff, (p->hend - p->heap)*sizeof(Eterm));
#endif    
    sys_free(p->heap);
    p->heap = n_heap;
    p->htop = n_htop;
    p->heap_sz = new_sz;

    p->low_water = n_heap;    /* These are used to switch */
    p->high_water = p->htop;  /* GC algorithm */

    p->status = saved_status;
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
	erl_exit(1, "no next heap size found: %d, offset %d\n");
    }
    return 0;
}

static void
offset_off_heap(Process* p, Eterm* low, Eterm* high, int offs)
{
    int ignore_hilo = (high == 0 && low == 0);

    if (p->off_heap.mso &&
	(ignore_hilo || ptr_within((Eterm *)p->off_heap.mso, low, high))) {
        Eterm** uptr = (Eterm**) &p->off_heap.mso;
        *uptr += offs;
    }

    if (p->off_heap.funs &&
	(ignore_hilo || ptr_within((Eterm *)p->off_heap.funs, low, high))) {
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
    new_heap = (Eterm *) safe_realloc((void*)p->heap, sizeof(Eterm)*new_sz);

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "grow_new_heap: GREW (%d) FROM %d UPTO %d (used %d)\n",
	    get_number(p->id), p->heap_sz, new_sz, heap_size);
#endif

    if ((offs = new_heap - p->heap) == 0) { /* No move. */
	p->hend = new_heap + new_sz;
	sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
	p->stop = p->hend - stack_size;
    } else {
	uint32 low_water;
	uint32 high_water;
	Eterm* prev_stop = p->stop;

	offset_heap(new_heap, heap_size, offs, p->heap, p->htop);

	/*
	 * Even if we are using fullsweep, we need to keep the
	 * water marks OK, we DO use them when switching...
	 */
	low_water = p->low_water - p->heap;
	high_water = p->high_water - p->heap;
	p->low_water = new_heap + low_water;
	p->high_water = new_heap + high_water;

	prev_stop = new_heap + (p->stop - p->heap);
	p->hend = new_heap + new_sz;
	p->stop = p->hend - stack_size;
	sys_memmove(p->stop, prev_stop, stack_size * sizeof(uint32));
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
    new_heap = (Eterm *) safe_realloc((void*)p->heap, sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "shrink_new_heap: SHRINKED (%d) FROM %d DOWNTO %d (used %d)\n",
	    get_number(p->id), p->heap_sz, new_sz, heap_size);
#endif

    if ((offs = new_heap - p->heap) != 0) {

	/*
	 * Normally, we don't expect a shrunk heap to move, but you never
	 * knows on some strange embedded systems...  Or when using purify.
	 */

	offset_heap(new_heap, heap_size, offs, p->heap, p->htop);

	/*
	 * Even if we are using fullsweep, we need to keep the
	 * water marks OK, we DO use them when switching...
	 */

	p->low_water = new_heap + (p->low_water - p->heap);
	p->high_water = new_heap + (p->high_water - p->heap);
	offset_rootset(p, offs, p->heap, p->htop, objv, nobj);
	p->htop = new_heap + heap_size;
	p->heap = new_heap;
    }
    p->heap_sz = new_sz;
}


/* New heap is empty when this function gets called */

static void 
shrink_old_heap(Process *p, uint32 new_sz, uint32 *objv, int nobj)
{
    uint32* new_heap;
    sint32 used, offs;
    uint32 *old = p->old_heap;
    uint32 *old_heap_ptr = p->old_heap;
#ifdef GC_HEAP_TRACE
    uint32 heap_size = (p->old_hend - p->old_heap);
#endif

    used = p->old_htop - p->old_heap;

    new_heap = (Eterm *) safe_realloc((void*) old, sizeof(uint32) * new_sz);
    offs = new_heap - old_heap_ptr;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "OLDHEAP: SHRINK (%d) FROM %d DOWNTO %d\n",
	    get_number(p->id), heap_size, new_sz);
#endif
    if (offs != 0) {
	ASSERT(p->htop == p->heap); /* Empty heap? */
	offset_heap(new_heap, used, offs, 0, 0);
	offset_rootset(p, offs, 0, 0, objv, nobj);
    }
    p->old_hend = new_heap + new_sz;
    p->old_htop = new_heap + used;
    p->old_heap = new_heap;
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

    int old_heap_need;
    int old_heap_sz;

    int wanted;
    int stack_size;		/* Size of stack ON HEAP. */
    int sz;

    /* This macro is used before return from do_gc2 in order to
     * detect as early as possible if something has gone wrong with 
     * stop or htop.
     */
#define OverRunCheck() \
    if (p->stop < p->htop) { \
	erl_exit(1, "%s: Overrun stack and heap at line %d\n", print_pid(p),__LINE__); \
    } 


    if (IS_TRACED_FL(p, F_TRACE_GC)) {
	trace_gc(p, am_gc_start);
    }

    p->arith_avail = 0;
#ifdef DEBUG
    p->arith_check_me = NULL;
#endif
    p->off_heap.overhead = 0;
    garbage_cols++;
    CHECK(p);

#ifdef DEBUG
    { 
	int acc = 0;
	static int maxwrong = 0;
	ErlHeapFragment* mb= p->mbuf;
	while (mb) {
	    acc+=mb->size;
	    mb=mb->next;
	}
	if (acc != p->mbuf_sz) {
	    if (acc - p->mbuf_sz > maxwrong){
		maxwrong = acc - p->mbuf_sz;
		erl_printf(CERR,"%s: Messagebuffer size"
			   " miscalculation shouldbe %d was %d\n", 
			   print_pid(p),acc,p->mbuf_sz);
	    }
	}
    }
#endif /* DEBUG */
    stack_size = p->hend - p->stop;
    if (p->stop < p->htop) {
	erl_exit(1, "%s: Overrun stack and heap\n", print_pid(p));
    }

    /* Size of heap before first GC */
    size_before = p->mbuf_sz + (p->htop - p->heap);
	    

    /*
     * Fullsweep GC.
     */

    if (!IS_GEN_GC(p)) {
	if (p->high_water - p->low_water < p->gc_switch) {

	    /*
	     * How big must the heap be to fit all potentially live data?
	     */

	    sz = next_heap_size(p->heap_sz + p->mbuf_sz, 0);

	    /*
	     * Should we grow although we don't actually need to?
	     */

	    if (sz == p->heap_sz && p->flags & F_HEAP_GROW) {
		sz = next_heap_size(p->heap_sz, 1);
		p->flags &= ~F_HEAP_GROW;
	    }

	    /*
	     * Do the fullsweep garbage collection.
	     */

	    fullsweep_heap(p, sz, objv, nobj);
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

	    /* Done. */
	    p->flags &= ~F_NEED_FULLSWEEP;
	    CHECK(p);
	    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
	    if (IS_TRACED_FL(p, F_TRACE_GC)) {
		trace_gc(p, am_gc_end);
	    }
	    OverRunCheck();
	    return ((int) (p->htop - p->heap) / 10);
	} else {
	    /* Migrate GC algorithm */
	    if(p->high_water != p->low_water) {
		p->flags |= F_GCFLIP; /* Tenure now. */
	    }
	    SET_GEN_GC(p);
#if defined(GC_SWITCH_TRACE)
	    fprintf(stderr, 
		    "Switched gc for %s, live data = %d "
		    "(switch at %d).\r\n",
		    print_pid(p), 
		    (int) (p->high_water - p->low_water),
		    (int) p->gc_switch);
#endif
	    /* Fall through to generational GC */
	}
    }

    /*
     * Generational GC from here on. We must have an old heap.
     */

    if (p->old_heap == NULL && (p->flags & F_GCFLIP) != 0) {
	create_old_heap(p, next_heap_size(p->high_water - p->low_water,0));
#ifdef OLD_HEAP_CREATION_TRACE
	fprintf(stderr,"Created old_heap for %s.\r\n",
		print_pid(p));
#endif
    }

    /*
     * Try a generational GC if the old heap is big enough and
     * there are no old binaries on it.
     */

    if (p->high_water - p->low_water <= p->old_hend - p->old_htop &&
	(p->flags & F_NEED_FULLSWEEP) == 0) {

	/*
	 * There is space enough in old_heap for everything
	 * below the high water mark.  Do a generational GC.
	 */
	
	gen_gc(p, next_heap_size(p->heap_sz + p->mbuf_sz, 0), objv, nobj);
	size_after = p->htop - p->heap;
	need_after = size_after + need + stack_size;
	reclaimed += (size_before - size_after);

	/* Don't even bother on reasonable small heaps */
	/* The reason for this is that after tenuring, we often */
	/* use a really small portion of new heap, therefore, unless */
	/* the heap size is substantial, we don't want to shrink */

	if ((p->heap_sz > 300) && (4 * need_after < p->heap_sz) &&
	    (p->heap_sz > (p->old_hend - p->old_heap))) {
	    if ((3 * need_after) > (p->old_hend - p->old_heap))
		wanted = 3 * need_after;
	    else
		wanted = (p->old_hend - p->old_heap);
	    if (wanted < p->min_heap_size) {
		wanted = p->min_heap_size;
	    } else {
		wanted = next_heap_size(wanted, 0);
	    }
	    if (wanted < p->heap_sz) {
		shrink_new_heap(p, wanted, objv, nobj);
	    }
	    CHECK(p);
	    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
	    if (IS_TRACED_FL(p, F_TRACE_GC)) {
		trace_gc(p, am_gc_end);
	    }
	    OverRunCheck();
	    return ((int) (p->htop - p->heap) / 10);
	}

	if (p->heap_sz >= need_after) {
	    CHECK(p);
	    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
	    if (IS_TRACED_FL(p, F_TRACE_GC)) {
		trace_gc(p, am_gc_end);
	    }
	    OverRunCheck();
	    return ((int) (p->htop - p->heap) / 10);
	}
#ifdef GC_HEAP_TRACE
	fprintf(stderr, "Did a gen_gc, still not enough room\n");
#endif
    }

    /*
     * The previous generational GC did not leave enough free heap space.
     * We must do a fullsweep gc: we garb everything to the
     * old heap, and continue with a fresh (empty) new heap.
     * First we need to figure out how big the (new) old_heap is
     * going to be.
     */

    old_heap_need = p->heap_sz + p->mbuf_sz  + (p->old_htop - p->old_heap);
    old_heap_sz = next_heap_size(old_heap_need, 0);
    fullsweep_old_heap(p, old_heap_sz, objv, nobj);

    /* We have an empty new heap and all live data now resides on old */
    /* We first possibly adjust the size of old heap */

    size_after = (p->old_htop - p->old_heap);
    reclaimed += (size_before - size_after);

    /* Shrink old if we're using less than 50 % of old heap */

    if ((2 * (p->old_htop - p->old_heap)) < old_heap_sz && 
	old_heap_sz != H_MIN_SIZE) {
	int nsz = next_heap_size(2 * (p->old_htop - p->old_heap), 0);  /* shrink */
	shrink_old_heap(p, nsz, objv, nobj);
    }

    /*
     * That was old heap, now check that the new heap has a reasonable size.
     * The new heap is empty, since we have just done a full sweep.
     * We want the new heap to be appr. the size of live data on old heap.
     */

    need += stack_size;
    wanted = next_heap_size((p->old_hend - p->old_heap) + need, 0);
    if (wanted > p->heap_sz) {
	grow_new_heap(p, wanted, objv, nobj);
    } else if (2 * wanted < p->heap_sz) {
	shrink_new_heap(p, wanted, objv, nobj);
    }
    CHECK(p);
    ASSERT(p->heap_sz == next_heap_size(p->heap_sz, 0));
    if (IS_TRACED_FL(p, F_TRACE_GC)) {
	trace_gc(p, am_gc_end);
    }
    OverRunCheck();
    return ((int) (p->htop - p->heap) / 10);
}
    

/*
** This function is used to sweep both the remainer of the new heap
** as well as the remainer of the old heap after the first pass
** of the generational collector gen_gc().
** Parameters:
** p: Pointer to the process structure
** from: Pointer into heap, sweep from here.
** to: [out] Pointer to pointer that gets set to the new heap top. 
** tenure: Flag that indicates if objects should be tenured, that 
** is saved on the old heap.
** low_water: Pointer into new heap, start of preserved data.
** high_water: Pointer into new heap, end of preserved data.
** old_htop_ptr: [out] Pointer to pointer that gets set to the
** top of the old heap.
** sweeping_old_heap: True if we're sweeping the new heap.
*/
static void
gen_cheney(Process *p, uint32 *from, uint32 **to,
	   int tenure,
	   uint32 *low_water, uint32 *high_water, 
	   uint32 **old_htop_ptr, 
	   int sweeping_new_heap) 
{

    /* Generation Heap scan jump table */
    DECL_JVALUE(ghs, CP0_DEF)
    DECL_JVALUE(ghs, CP4_DEF)
    DECL_JVALUE(ghs, CP8_DEF)
	/* DECL_JVALUE(ghs, CP12_DEF) save as MOVED_DEF */
    DECL_JVALUE(ghs, SMALL_DEF)
    DECL_JVALUE(ghs, BIG_DEF)
    DECL_JVALUE(ghs, FLOAT_DEF)
    DECL_JVALUE(ghs, ATOM_DEF)
    DECL_JVALUE(ghs, REFER_DEF)
    DECL_JVALUE(ghs, PORT_DEF)
    DECL_JVALUE(ghs, PID_DEF)
    DECL_JVALUE(ghs, TUPLE_DEF)
    DECL_JVALUE(ghs, LIST_DEF)
    DECL_JVALUE(ghs, ARITYVAL_DEF)
    DECL_JVALUE(ghs, MOVED_DEF)
    DECL_JVALUE(ghs, THING_DEF)
    DECL_JVALUE(ghs, BINARY_DEF)
    
    DECL_JTABLE(ghs, 16)

    uint32 *old_htop, *n_hp, *n_htop;
    uint32 *oh_start, *oh_end;

    uint32 tmp;
    uint32 *ptr;
    uint32 val;
    uint32 gval;
    uint32 do_tenure = tenure && sweeping_new_heap;

    n_hp = from; 
    n_htop = *to; 
    old_htop = *old_htop_ptr;
    oh_start = p->old_heap;
    oh_end = p->old_hend;
    old_htop = *old_htop_ptr;

    if (JTABLE_NEED_INIT(ghs)) {
	DEFINE_LOCATION(ghs, CP0_DEF);
	DEFINE_LOCATION(ghs, CP4_DEF);
	DEFINE_LOCATION(ghs, CP8_DEF);
	/* DEFINE_LOCATION(ghs, CP12_DEF); Same as MOVED_DEF */
	DEFINE_LOCATION(ghs, SMALL_DEF);
	DEFINE_LOCATION(ghs, BIG_DEF);
	DEFINE_LOCATION(ghs, FLOAT_DEF);
	DEFINE_LOCATION(ghs, ATOM_DEF);
	DEFINE_LOCATION(ghs, REFER_DEF);
	DEFINE_LOCATION(ghs, PORT_DEF);
	DEFINE_LOCATION(ghs, PID_DEF);
	DEFINE_LOCATION(ghs, TUPLE_DEF);
	DEFINE_LOCATION(ghs, LIST_DEF);
	DEFINE_LOCATION(ghs, ARITYVAL_DEF);
	DEFINE_LOCATION(ghs, MOVED_DEF);
	DEFINE_LOCATION(ghs, THING_DEF);
	DEFINE_LOCATION(ghs, BINARY_DEF);
	DEFINE_JTABLE(ghs);
    }

    while (n_hp != n_htop) {
	
	gval = *n_hp;

	JUMP(ghs, tag_val_def(gval));
	LOCATION(ghs, FLOAT_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_moved(val))
		*n_hp++ = make_float(ptr_val(val));
	    else if (do_tenure && ptr_within(ptr, low_water, high_water)) {
		/* Make object old */
		*n_hp++ = gval = make_float(old_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*old_htop++ = val;             /* Store thing */
		*old_htop++ = *ptr++;          /* Copy float part 1 */
		*old_htop++ = *ptr;            /* Copy float part 2 */
	    }
	    /* else our pointers are already on old_heap, we may continue */
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_float(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		*n_htop++ = *ptr++;          /* Copy float part 1 */
		*n_htop++ = *ptr;            /* Copy float part 2 */
	    }
	    continue;
	}

	LOCATION(ghs, REFER_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_moved(val))
		*n_hp++ = make_refer(ptr_val(val));
	    else if (do_tenure && ptr_within(ptr, low_water, high_water)) {
		*n_hp++ = gval = make_refer(old_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*old_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *old_htop++ = *ptr++;
	    }
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_refer(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(ghs, BIG_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_moved(val))
		*n_hp++ = make_big(ptr_val(val));
	    else if (do_tenure && ptr_within(ptr, low_water, high_water)) {
		*n_hp++ = gval = make_big(old_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*old_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *old_htop++ = *ptr++;
	    }
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_thing(val));
		*n_hp++ = gval = make_big(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store thing */
		tmp = thing_arityval(val);    /* Get arity value */
		while(tmp--) 
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(ghs, TUPLE_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_moved(val))
		*n_hp++ = make_tuple(ptr_val(val));
	    else if (do_tenure && ptr_within(ptr, low_water, high_water)) {
		/* Make object old */
		*n_hp++ = gval = make_tuple(old_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*old_htop++ = val;             /* Store arity val */
		tmp = arityval(val);          /* Get arity value */
		while(tmp--)
		    *old_htop++ = *ptr++;
	    }
	    else {
		ASSERT(within(ptr, p));
		ASSERT(is_arity_value(val));
		*n_hp++ = gval = make_tuple(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store arity val */
		tmp = arityval(val);          /* Get arity value */
		while(tmp--)
		    *n_htop++ = *ptr++;
	    }
	    continue;
	}

	LOCATION(ghs, LIST_DEF) {
	    ptr = ptr_val(gval);
	    val = *ptr;
	    if (ptr_within(ptr, oh_start, oh_end))
		n_hp++;
	    else if (is_moved(val))
		*n_hp++ = make_list(ptr_val(val));
	    else if (do_tenure && ptr_within(ptr, low_water, high_water)) {
		/* Make object old */
		*n_hp++ = gval = make_list(old_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*old_htop++ = val;             /* Store head */
		*old_htop++ = *ptr;            /* Store tail */
	    }
	    else {
		ASSERT(within(ptr, p));
		*n_hp++ = gval = make_list(n_htop);
		*ptr++ = make_moved(gval);   /* Store move address */
		*n_htop++ = val;             /* Store head */
		*n_htop++ = *ptr;            /* Store tail */
	    }
	    continue;
	}

	LOCATION(ghs, THING_DEF) {
	    n_hp += (thing_arityval(gval)+1);
	    continue;
	}

	LOCATION(ghs, SMALL_DEF);
	LOCATION(ghs, ATOM_DEF);
	LOCATION(ghs, PID_DEF);
	LOCATION(ghs, PORT_DEF);
	LOCATION(ghs, ARITYVAL_DEF) {
	    n_hp++;
	    continue;
	}
	LOCATION(ghs, BINARY_DEF) {
	    gen_gc_binary(p, n_hp, &n_htop, &old_htop, do_tenure,
			  low_water, high_water, oh_start, oh_end);
	    n_hp++;
	    continue;
	}

	LOCATION(ghs, CP0_DEF);
	LOCATION(ghs, CP4_DEF);
	LOCATION(ghs, CP8_DEF);
	/* LOCATION(ghs, CP12_DEF);  Same as MOVED_DEF */
	LOCATION(ghs, MOVED_DEF) {
	    erl_exit(1, "%s: GC: bad data on heap - pass 2 0x%08x at 0x%08x\n",
		     print_pid(p), gval, n_hp);
	}
	JUMP_END;
    }

    /* Now set the parameter pointers for the caller */
    *to = n_htop;
    if (sweeping_new_heap) {
	*old_htop_ptr = old_htop;
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

/* 
** Garbage collect the heap, however all objects pointing 
** to the old generation heap may be left as is 
** Every second turn, we remember the position on the heap
** that turned out to be the heap top, every other second turn
** we tenure all live objects that reside below that water mark
** This means that we only tenure objects that have survived at 
** least one collection 
*/
static void
gen_gc(Process *p, int new_sz, uint32 *objv, int nobj)
{
    /* Stack scan jump table */
    DECL_JVALUE(gvs, CP0_DEF)
    DECL_JVALUE(gvs, CP4_DEF)
    DECL_JVALUE(gvs, CP8_DEF)
    DECL_JVALUE(gvs, CP12_DEF)
    DECL_JVALUE(gvs, BLANK_DEF)
    DECL_JVALUE(gvs, SMALL_DEF)
    DECL_JVALUE(gvs, BIG_DEF)
    DECL_JVALUE(gvs, FLOAT_DEF)
    DECL_JVALUE(gvs, ATOM_DEF)
    DECL_JVALUE(gvs, REFER_DEF)
    DECL_JVALUE(gvs, PORT_DEF)
    DECL_JVALUE(gvs, PID_DEF)
    DECL_JVALUE(gvs, TUPLE_DEF)
    DECL_JVALUE(gvs, LIST_DEF)
    DECL_JVALUE(gvs, CATCH_DEF)
    DECL_JVALUE(gvs, BINARY_DEF)

    DECL_JTABLE(gvs, 16)

    uint32 *n_hstart;
    uint32 *n_htop;
    uint32 saved_status;
    int tmp;
    Rootset rootset;		/* Rootset for GC (stack, dictionary, etc). */
    int tenure, n;
    uint32 *ptr;
    uint32 val;
    uint32 gval;
    uint32* low_water;
    uint32* high_water;
    uint32* oh_start;
    uint32* old_htop;
    uint32* oh_end;
    uint32* prev_old_htop;

    oh_start = p->old_heap;
    oh_end = p->old_hend;
    low_water = p->low_water;
    high_water = p->high_water;
    tenure = (p->flags & F_GCFLIP);
    prev_old_htop = p->old_htop;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Generational GC (proc = %d) ", (int)get_number(p->id));
#endif

    /* If flip is true, we need to tenure all (live) objects */
    /* within the watermarks, if flip is 0, we need to alloc a */
    /* new new_heap and copy all live objects to the new new_heap */
    /* that is to not tenure any objects at all */

    if (JTABLE_NEED_INIT(gvs)) {
	DEFINE_LOCATION(gvs, CP0_DEF);
	DEFINE_LOCATION(gvs, CP4_DEF);
	DEFINE_LOCATION(gvs, CP8_DEF);
	DEFINE_LOCATION(gvs, CP12_DEF);
	DEFINE_LOCATION(gvs, BLANK_DEF);
	DEFINE_LOCATION(gvs, SMALL_DEF);
	DEFINE_LOCATION(gvs, BIG_DEF);
	DEFINE_LOCATION(gvs, FLOAT_DEF);
	DEFINE_LOCATION(gvs, ATOM_DEF);
	DEFINE_LOCATION(gvs, REFER_DEF);
	DEFINE_LOCATION(gvs, PORT_DEF);
	DEFINE_LOCATION(gvs, PID_DEF);
	DEFINE_LOCATION(gvs, TUPLE_DEF);
	DEFINE_LOCATION(gvs, LIST_DEF);
	DEFINE_LOCATION(gvs, CATCH_DEF);
	DEFINE_LOCATION(gvs, BINARY_DEF);
	DEFINE_JTABLE(gvs);
    }

    n_hstart = (uint32*) safe_alloc_from(805,sizeof(uint32)*new_sz);
    n_htop = n_hstart;
    old_htop = p->old_htop;
    saved_status = p->status;
    p->status = P_GARBING;
    n = setup_rootset(p, objv, nobj, &rootset);

    while (n--) {
	uint32* g_ptr = rootset.v[n];
	uint32 g_sz = rootset.sz[n];
	
	while (g_sz--) {
	    gval = *g_ptr;

	    JUMP(gvs, tag_val_def(gval));

	    LOCATION(gvs, FLOAT_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_moved(val))
		    *g_ptr++ = make_float(ptr_val(val));
		else if (tenure && ptr_within(ptr, low_water, high_water)) { /* copy to old*/
		    *g_ptr++ = gval = make_float(old_htop);
		    *ptr++ = make_moved(gval);
		    *old_htop++ = val;             /* Store thing */
		    *old_htop++ = *ptr++;          /* Copy float part 1 */
		    *old_htop++ = *ptr;            /* Copy float part 2 */
		}
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_float(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    *n_htop++ = *ptr++;          /* Copy float part 1 */
		    *n_htop++ = *ptr;            /* Copy float part 2 */
		}
		continue;
	    }

	    LOCATION(gvs, REFER_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_moved(val))
		    *g_ptr++ = make_refer(ptr_val(val));
		else if (tenure && ptr_within(ptr, low_water, high_water)) {
		    *g_ptr++ = gval = make_refer(old_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *old_htop++ = val;
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*old_htop++ = *ptr++;
		}
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_refer(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(gvs, BIG_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_moved(val))
		    *g_ptr++ = make_big(ptr_val(val));
		else if (tenure && ptr_within(ptr, low_water, high_water)) {
		    *g_ptr++ = gval = make_big(old_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *old_htop++ = val;
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*old_htop++ = *ptr++;
		}
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_thing(val));
		    *g_ptr++ = gval = make_big(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store thing */
		    tmp = thing_arityval(val);    /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(gvs, TUPLE_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_moved(val))
		    *g_ptr++ = make_tuple(ptr_val(val));
		else if (tenure && ptr_within(ptr, low_water, high_water)) {
		    *g_ptr++ = gval = make_tuple(old_htop);
		    *ptr++ = make_moved(gval);
		    *old_htop++ = val;             /* Store arity val */
		    tmp = arityval(val);          /* Get arity value */
		    while(tmp--)
			*old_htop++ = *ptr++;
		}
		else {
		    ASSERT(within(ptr, p));
		    ASSERT(is_arity_value(val));
		    *g_ptr++ = gval = make_tuple(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store arity val */
		    tmp = arityval(val);          /* Get arity value */
		    while(tmp--)
			*n_htop++ = *ptr++;
		}
		continue;
	    }

	    LOCATION(gvs, LIST_DEF) {
		ptr = ptr_val(gval);
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		    g_ptr++;
		else if (is_moved(val))
		    *g_ptr++ = make_list(ptr_val(val));
		else if (tenure && ptr_within(ptr, low_water, high_water)) {
		    *g_ptr++ = gval = make_list(old_htop);
		    *ptr++ = make_moved(gval);     /* Store move address */
		    *old_htop++ = val;             /* Store head */
		    *old_htop++ = *ptr;            /* Store tail */
		}
		else {
		    ASSERT(within(ptr, p));
		    *g_ptr++ = gval = make_list(n_htop);
		    *ptr++ = make_moved(gval);   /* Store move address */
		    *n_htop++ = val;             /* Store head */
		    *n_htop++ = *ptr;            /* Store tail */
		}
		continue;
	    }

	    LOCATION(gvs, CP0_DEF);
	    LOCATION(gvs, CP4_DEF);
	    LOCATION(gvs, CP8_DEF);
	    LOCATION(gvs, CP12_DEF);
	    LOCATION(gvs, BLANK_DEF);
	    LOCATION(gvs, CATCH_DEF);
	    LOCATION(gvs, SMALL_DEF);
	    LOCATION(gvs, ATOM_DEF);
	    LOCATION(gvs, PID_DEF);
	    LOCATION(gvs, PORT_DEF) {
		g_ptr++;
		continue;
	    }

	    LOCATION(gvs, BINARY_DEF) {
		gen_gc_binary(p, g_ptr, &n_htop, &old_htop, tenure,
			      low_water, high_water, oh_start, oh_end);
		g_ptr++;
		continue;
	    }

	    JUMP_END;
	}
    }

    /* 
     * Now we got to move the stack to the top of the new heap...
     */
    n = p->hend - p->stop;

    sys_memcpy(n_hstart + new_sz - n, p->stop, n * sizeof(uint32));

    p->hend = n_hstart + new_sz;
    p->stop = p->hend - n;

    /* 
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */
    
    gen_cheney(p, n_hstart, &n_htop, tenure, low_water, 
	       high_water, &old_htop, 1);
    
    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) heap.
     */

    if (tenure) {
	gen_cheney(p, prev_old_htop, &old_htop, tenure, low_water, 
		   high_water, &old_htop, 0);
    }

    if (p->off_heap.mso) {
	sweep_proc_bins(p, 0);
    }
    if (p->off_heap.funs) {
	sweep_proc_funs(p, 0);
    }

#ifdef GC_HEAP_TRACE
    if (tenure) 
	fprintf(stderr,"Tenured %d words Kept %d words\n", 
		old_htop - prev_old_htop,
		n_htop - n_hstart);
    else
	fprintf(stderr, "No tenuring Kept %d words\n",
		n_htop - n_hstart);
#endif

    /* Clobber the old heap */
#ifdef DEBUG
    sys_memset(p->heap, 0xff, p->heap_sz*sizeof(uint32));
#endif
    sys_free((void*)p->heap);

    restore_rootset(p, &rootset);
    
    p->heap = n_hstart;
    p->hend = n_hstart + new_sz;
    p->htop = n_htop;
    p->heap_sz = new_sz;

    p->old_htop = old_htop;
    p->status = saved_status;
    
    if (tenure) {
	p->low_water = p->high_water = n_hstart;
	p->flags &= ~F_GCFLIP;
    } else {
	p->low_water = n_hstart;
	p->high_water = n_htop;
	p->flags |= F_GCFLIP;
    }
}

static void
fullsweep_gc_binary(Process* p, Eterm* g_ptr, Eterm** n_htop_p)
{
    Eterm* n_htop = *n_htop_p;
    Eterm* ptr = ptr_val(*g_ptr);
    Eterm val = *ptr;

    if (is_moved(val)) {
	*g_ptr = make_binary(ptr_val(val));
	return;
    }
    ASSERT(within(ptr, p));
    ASSERT(is_thing(val));

    switch thing_subtag(val) {
    case REFC_BINARY_SUBTAG:
	{
	    int n;
	    Eterm gval;

	    *g_ptr = gval = make_binary(n_htop);
	    *ptr++ = make_moved(gval); /* Store move address */
	    *n_htop++ = val;	/* Store thing */
	    n = PROC_BIN_SIZE-1;
	    ASSERT(n == thing_arityval(val));
	    while (n--) {
		*n_htop++ = *ptr++;
	    }
	}
	break;
    case FUN_SUBTAG:
	{
	    int n;
	    Eterm gval;
	    ErlFunThing* funp = (ErlFunThing *) ptr;

	    *g_ptr = gval = make_binary(n_htop);
	    *ptr++ = make_moved(gval); /* Store move address */
	    *n_htop++ = val;	/* Store thing */
	    n = thing_arityval(val) + funp->num_free;
	    while (n--) {
		*n_htop++ = *ptr++;
	    }
	}
	break;
    case HEAP_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case SUB_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    default:
	erl_exit(1, "%s, line %d: Bad subtag: %d",
		 __FILE__, __LINE__, thing_subtag(val));
    }
    *n_htop_p = n_htop;
}

static void
gen_gc_binary(Process *p, Eterm* g_ptr, Eterm** n_htop_p, Eterm** old_htop_p,
	      int tenure, Eterm* lw, Eterm* hw, Eterm* ohs, Eterm* ohe)
{
    Eterm* n_htop = *n_htop_p;
    Eterm *old_htop = *old_htop_p;
    Eterm gval = *g_ptr;
    Eterm *ptr = ptr_val(gval);
    Eterm val = *ptr;

    if (is_moved(val)) {
	*g_ptr = make_binary(ptr_val(val));
	return;
    }
    ASSERT(within(ptr, p));
    ASSERT(is_thing(val));

    if (ptr_within(ptr, ohs, ohe)) 
	return;

    switch (thing_subtag(val)) {
    case REFC_BINARY_SUBTAG:
	if (tenure && ptr_within(ptr, lw, hw)) {
	    int n;

	    *g_ptr = gval = make_binary(old_htop);
	    *ptr++ = make_moved(gval);   /* Store move address */
	    *old_htop++ = val;             /* Store thing */
	    n = thing_arityval(val);    /* Get arity value */
	    while (n--) {
		*old_htop++ = *ptr++;
	    }
	 } else {
	     int n;

	     *g_ptr = gval = make_binary(n_htop);
	     *ptr++ = make_moved(gval);   /* Store move address */
	     *n_htop++ = val;             /* Store thing */
	     n = thing_arityval(val);    /* Get arity value */
	     while (n--) {
		 *n_htop++ = *ptr++;
	     }
	 }
	break;
    case FUN_SUBTAG:
	if (tenure && ptr_within(ptr, lw, hw)) {
	    int n;
	    ErlFunThing* funp = (ErlFunThing *) ptr;

	    *g_ptr = gval = make_binary(old_htop);
	    *ptr++ = make_moved(gval);   /* Store move address */
	    *old_htop++ = val;             /* Store thing */
	    n = thing_arityval(val) + funp->num_free;
	    while (n--) {
		*old_htop++ = *ptr++;
	    }
	 } else {
	     int n;
	     ErlFunThing* funp = (ErlFunThing *) ptr;

	     *g_ptr = gval = make_binary(n_htop);
	     *ptr++ = make_moved(gval);   /* Store move address */
	     *n_htop++ = val;             /* Store thing */
	     n = thing_arityval(val) + funp->num_free;
	     while (n--) {
		 *n_htop++ = *ptr++;
	     }
	 }
	break;
    case HEAP_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case SUB_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    default:
	erl_exit(1, "%s, line %d: Bad subtag: %d",
		 __FILE__, __LINE__, thing_subtag(val));
    }
    *n_htop_p = n_htop;
    *old_htop_p = old_htop;
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
            
        if (is_moved(*ppt)) {	/* Object is alive */
            ErlFunThing* ro = (ErlFunThing *) ptr_val(*ppt);

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

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;
            
        if (is_moved(*ppt)) {	/* Object is alive */
            ProcBin* ro = (ProcBin*) ptr_val(*ppt);

            *prev = ro;		/* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
	    if (fullsweep == 0 && ptr_within((Eterm *)ro, bot, top)) {
		p->flags |= F_NEED_FULLSWEEP;
	    }
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {

            /*
	     * Object resides on old heap, and we just did a
	     * generational collection - keep object in list.
	     */
            prev = &ptr->next;
            ptr = ptr->next;
	    p->flags |= F_NEED_FULLSWEEP;
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

    if (IS_GEN_GC(p) && (p->old_heap <= ptr && ptr < p->old_hend)) {
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
    Eterm* ptr = ptr_val(obj);
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
	    ProcBin *bp = (ProcBin*)ptr_val(obj);

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

static int 
check_heap_obj(Process *p, char *msg, uint32 obj, int back_pointers_allowed)
{
    int tmp;

    switch (tag_val_def(obj)) {
    case ATOM_DEF:
	if (unsigned_val(obj) >= atom_table_size) {
	    erl_exit(1, "%s: check_heap: bad atom on heap %d\n",
		     print_pid(p), unsigned_val(obj));
	}
	return 0;
    case SMALL_DEF:
    case ARITYVAL_DEF:
    case PID_DEF:
    case PORT_DEF:
	return 0;
    case BINARY_DEF:
	check_binary(p, obj, msg);
	return 0;
    case THING_DEF:
	tmp = thing_arityval(obj); /* get thing value */
	return tmp;
    case REFER_DEF:
    case BIG_DEF:
    case FLOAT_DEF:
    case LIST_DEF:
    case TUPLE_DEF:
	check_pointer(p, obj, back_pointers_allowed, msg);
	return 0;
    default:
	erl_exit(1, "%s: check_heap: %s: bad tag %x\n",
		 print_pid(p), msg, obj);
    }
    return 0;
}
static void 
stack_element_check(Process *p, char *msg, uint32 x)
{
    switch (tag_val_def(x)) {
    case ATOM_DEF:
	if (unsigned_val(x) >= atom_table_size) {
	    erl_exit(1, "%s: check_stack: bad atom on stack %d\n",
		     print_pid(p), unsigned_val(x));
	}
	return;
    case SMALL_DEF:
    case PID_DEF:
    case CP0_DEF:
    case CP4_DEF:
    case CP8_DEF:
    case CP12_DEF:
    case BLANK_DEF:
    case PORT_DEF:
	return;
    case BINARY_DEF:
	check_binary(p, x, msg);
	return;
    case REFER_DEF:
    case BIG_DEF:
    case FLOAT_DEF:
    case LIST_DEF:
    case TUPLE_DEF:
	if (!within(ptr_val(x), p)) {
	    erl_exit(1, "%s: check_stack: %s: bad address %x\n",
		     print_pid(p), msg, x);
	}
	return;
    default:
	erl_printf(1, "%s: stack_check: %s: bad tag %x\n",
		   print_pid(p), msg, x);
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
	if (!is_catch(*sp)) {
	    stack_element_check(p, msg, *sp);
	}
    }
}

static void
heap_dump(Process *p, Eterm* from, Eterm* to)
{
    uint32 *hp;
    uint32 obj;
    int tmp;

    for (hp = from; hp < to; hp++) {
	obj = *hp;
	fprintf(stderr,"%x :", (unsigned int) hp);
	switch (tag_val_def(obj)) {
	case ATOM_DEF:
	    if (unsigned_val(obj) >= atom_table_size) {
		erl_printf(CERR, "heap_dump: bad atom on heap %d\n",
			   unsigned_val(obj));
	    }
	case SMALL_DEF:
	case PID_DEF:
	case REFER_DEF:
	case PORT_DEF:
	case BINARY_DEF:
	    td(obj);
	    break;

	case THING_DEF:
	    tmp = thing_arityval(obj); /* get thing value */
	    fprintf(stderr, "THING size = %d\r\n", tmp);
	    hp += tmp;	/* skip the binary data */
	    /* note the hp++ in the "for" will also add 1 */
	    break;
	
	case BIG_DEF:
	case FLOAT_DEF:
	    fprintf(stderr,"Float pointer %x ", 
		    (unsigned int) ptr_val(obj));
	    where_pointer(p, from, to, obj, CERR);
	    break;
	case LIST_DEF:
	    fprintf(stderr,"List pointer %x", 
		    (unsigned int)ptr_val(obj)); 
	    where_pointer(p, from, to, obj, CERR);
	    break;
	case TUPLE_DEF:
	    fprintf(stderr,"Tuple pointer %x ", 
		    (unsigned int)ptr_val(obj)); 
	    where_pointer(p, from, to, obj, CERR);
	    break;
	case ARITYVAL_DEF:
	    fprintf(stderr,"Arity val = %d \r\n", (int)arityval(obj));
	    break;
	default:
	    fprintf(stderr, "Unknown object on heap \r\n");
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

static int 
where_pointer(Process *p, Eterm* from, Eterm* to, Eterm obj, CIO fd)
{
    int count = 0;
    uint32 *ptr = ptr_val(obj);

    erl_printf(fd, " at ");
    if (ptr_within(ptr, from, to)) 
	erl_printf(fd,"This interval   ");
    if (IS_GEN_GC(p) && ptr_within(ptr, p->old_heap, p->old_htop)) {
	erl_printf(fd, "Old heap\n");
	count++;
    }
    if (ptr_within(ptr, p->heap, p->htop)) {
	count++;
	erl_printf(fd, "New heap\n");
    }
    if (count == 0) 
	erl_printf(fd, "Unknown pointer %x !!!!\n", (unsigned int) ptr);
    return 1;
}

#endif  /* HARDDEBUG  */

static void print_function_from_pc(uint32 *x, CIO fd)
{
   uint32* addr = find_function_from_pc(x);
   if (addr) {
      sys_printf(fd, "%x: ", addr);
      display(addr[0], fd);
      sys_printf(fd, ":");
      display(addr[1], fd);
      sys_printf(fd, "/%d", addr[2]);
   }
}

static void stack_element_dump(Process *p, uint32 *sp, CIO fd)
{
    uint32 x = *sp;
    erl_printf(fd,"%x\t", (unsigned int) sp);
    
    switch (tag_val_def(x)) {
    case ATOM_DEF:
	if (unsigned_val(x) >= atom_table_size) {
	    erl_exit(1, "%s: check_stack: bad atom on stack %d\n",
		     print_pid(p), unsigned_val(x));
	}
	display(x, fd);
	erl_putc('\n', fd);
	return;
    case SMALL_DEF:
    case PID_DEF:
    case REFER_DEF:
	display(x, fd);
	erl_putc('\n', fd);
	break;
    case CP0_DEF:
    case CP4_DEF:
    case CP8_DEF:
    case CP12_DEF:
	erl_printf(fd, "Continuation pointer %p, ", (uint32*) x);
	print_function_from_pc(cp_ptr_val(x), fd);
	sys_printf(fd, "\n");
	break;
    case BLANK_DEF:
	erl_printf(fd,"Blank %x\n", (uint32*) x);
	break;
    case PORT_DEF:
    case BINARY_DEF:
	display(x, fd);
	erl_putc('\n', fd);
	break;
    case BIG_DEF:
    case FLOAT_DEF:
#if 0
	erl_printf(fd,"Float pointer %x ", 
		(unsigned int) ptr_val(x));
	where_pointer(p, 0, 0, x, fd);
#endif
	display(x, fd);
	erl_putc('\n', fd);
	break;
    case LIST_DEF:
#if 0
	erl_printf(fd,"List pointer %x", 
		(unsigned int)ptr_val(x)); 
	where_pointer(p, 0, 0, x, fd);
#endif
	display(x, fd);
	erl_putc('\n', fd);
	break;
    case TUPLE_DEF:
#if 0
	erl_printf(fd,"Tuple pointer %x ", 
		(unsigned int)ptr_val(x)); 
	where_pointer(p, 0, 0 , x, fd);
#endif
	display(x, fd);
	erl_putc('\n', fd);
	break;
    case CATCH_DEF:
	erl_printf(fd, "Catch %x, ", ptr_val(x));
	print_function_from_pc(ptr_val(x), fd);
	sys_printf(fd, "\n");
	break;
    default:
	erl_printf(fd, "stack_dump: : bad tag %x\n", x);
	break;
    }
}

void stack_dump2(Process *p, CIO fd)
{
    uint32 *sp;
    int i;

    for (sp = p->hend-1; sp >= p->stop; sp--) {
       stack_element_dump(p, sp, fd);
   }
    erl_printf(fd, "i = %#x, cp = %#x, arity = %d, ",
	       p->i, p->cp, p->arity);
    print_function_from_pc(cp_ptr_val(p->i), fd);
    erl_printf(fd, ", ");
    print_function_from_pc(cp_ptr_val(p->cp), fd);
    erl_printf(fd, "\n");
    for (i = 0; i < p->arity; i++) {
	erl_printf(fd, "   ");
	display(p->arg_reg[i], fd);
	erl_printf(fd, "\n");
    }
}

static char*
print_pid(Process *p)
{
    char static buf[64];

    uint32 obj = p->id;
    sprintf(buf, "<%ld.%ld.%ld>",
	    get_node(obj), get_number(obj), get_serial(obj));
    return buf;
}
