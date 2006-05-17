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
#include "erl_bits.h"
#include "ggc.h"
#include "erl_nmgc.h"
#if HIPE
#include "hipe_bif0.h" /* for hipe_constants_{start,next} */
#include "hipe_stack.h"
#endif

#ifndef HEAP_FRAG_ELIM_TEST

static erts_smp_spinlock_t info_lck;
static Uint garbage_cols;		/* no of garbage collections */
static Uint reclaimed;			/* no of words reclaimed in GCs */

static void remove_message_buffers(Process* p);

/*
 * Returns number of elements in an array.
 */
#define ALENGTH(a) (sizeof(a)/sizeof(a[0]))

/*
 * Used for printing beautiful stack dumps.
 */
extern Eterm beam_apply[];
extern Eterm beam_exit[];

static void gen_gc(Process*, int, Eterm*, int);
static void sweep_proc_bins(Process *p, int fullsweep);
#ifndef HYBRID /* FIND ME! */
static void sweep_proc_funs(Process *p, int fullsweep);
#endif
static void sweep_proc_externals(Process *p, int fullsweep);

#ifdef HARDDEBUG
static void check_stack(Process*, char*);
void check_bins(Process *p);
int chk_sys(void);

#define CHECK(p)                \
    erts_check_stack(p);        \
    check_bins(p);

#else
# define CHECK(p) ((void) 1)
#endif /* HARDDEBUG */

#if defined(HYBRID)
char ma_gc_flags = 0;
#endif

/*
 * Return the next heap size to use. Make sure we never return
 * a smaller heap size than the minimum heap size for the process.
 * (Use of the erlang:hibernate/3 BIF could have shrinked the
 * heap below the minimum heap size.)
 */
static Uint
next_heap_size(Process* p, Uint size, Uint offset)
{
    size = erts_next_heap_size(size, offset);
    return size < p->min_heap_size ? p->min_heap_size : size;
}

/*
 * Offset pointers to heap from stack.
 */

static void 
offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, 
		Eterm* low, Eterm* high)
{
    char* water_start = (char *)low;
    Uint water_size = (char *)high - water_start;

    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED: {
	      if (in_area(ptr_val(val), water_start, water_size)) {
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

void 
erts_offset_heap_ptr(Eterm* hp, Uint sz, Sint offs, 
		     Eterm* low, Eterm* high)
{
    offset_heap_ptr(hp, sz, offs, low, high);
}

/*
 * Offset pointers into the heap (not stack).
 * Only offset pointers that point into the interval of low and high.
 */

static void 
offset_heap(Eterm* hp, Uint sz, Sint offs, Eterm* low, Eterm* high)
{
    char* water_start = (char *)low;
    Uint water_size = (char *)high - water_start;
    
    
    while (sz--) {
	Eterm val = *hp;
	switch (primary_tag(val)) {
	  case TAG_PRIMARY_LIST:
	  case TAG_PRIMARY_BOXED: {
	      if (in_area(ptr_val(val), water_start, water_size)) {
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

		      if (*uptr && in_area((Eterm *)pb->next,
					   water_start, water_size)) {
			  *uptr += offs; /* Patch the mso chain */
		      }
		      sz -= tari;
		      hp += tari + 1;
		  }
		  break;
	      
	      case BIN_MATCHSTATE_SUBTAG:
		{	
		  ErlBinMatchState *ms = (ErlBinMatchState*) hp;
		  ErlBinMatchBuffer *mb = &(ms->mb);
		  if (in_area(ptr_val(mb->orig), water_start, water_size)) {
		      mb->orig = offset_ptr(mb->orig, offs);
		      mb->base = binary_bytes(mb->orig);
		  }
		  sz -= tari;
		  hp += tari + 1;
		}
		break;
		  
	      case FUN_SUBTAG:
		  {
#ifndef HYBRID /* FIND ME! */
		      ErlFunThing* funp = (ErlFunThing *) hp;
		      Eterm** uptr = (Eterm **) &funp->next;

		      if (*uptr && in_area((Eterm *)funp->next,
					   water_start, water_size)) {
			  *uptr += offs;
		      }
#endif
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

		      if (*uptr && in_area((Eterm *)etp->next,
					   water_start, water_size)) {
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


void 
erts_offset_heap(Eterm* hp, Uint sz, Sint offs, Eterm* low, Eterm* high)
{
    offset_heap(hp, sz, offs, low, high);
}

/*
 * Offset pointers in message queue.
 */
static void
offset_mqueue(Process *p, Sint offs, Eterm* low, Eterm* high) 
{
    ErlMessage* mp = p->msg.first;
    char* water_start = (char *)low;
    Uint water_size = (char *)high - water_start;

    while (mp != NULL) {
#if defined(ERTS_SMP) && !defined(HEAP_FRAG_ELIM_TEST)
	if (!mp->bp)
#endif
	{
	    Eterm mesg = ERL_MESSAGE_TERM(mp);
	    switch (primary_tag(mesg)) {
	    case TAG_PRIMARY_LIST:
	    case TAG_PRIMARY_BOXED:
		if (in_area(ptr_val(mesg), water_start, water_size)) {
		    ERL_MESSAGE_TERM(mp) = offset_ptr(mesg, offs);
		}
		break;
	    }
	    mesg = ERL_MESSAGE_TOKEN(mp);
	    if (is_boxed(mesg)
		&& in_area(ptr_val(mesg), water_start, water_size)) {
		ERL_MESSAGE_TOKEN(mp) = offset_ptr(mesg, offs);
	    }
	    ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		    is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		    is_atom(ERL_MESSAGE_TOKEN(mp))));
	}
        mp = mp->next;
    }
}

/*
 * HiPE native code stack scanning procedures:
 * - fullsweep_nstack()
 * - gensweep_nstack()
 * - offset_nstack()
 */
#if defined(HIPE)

#define GENSWEEP_NSTACK(p,old_htop,n_htop,objv,nobj)             	\
	do {								\
		Eterm *tmp_old_htop = old_htop;				\
		Eterm *tmp_n_htop = n_htop;				\
		gensweep_nstack((p), &tmp_old_htop, &tmp_n_htop,objv,nobj); \
		old_htop = tmp_old_htop;				\
		n_htop = tmp_n_htop;					\
	} while(0)

/*
 * offset_nstack() can ignore the descriptor-based traversal the other
 * nstack procedures use and simply call offset_heap_ptr() instead.
 * This relies on two facts:
 * 1. The only live non-Erlang terms on an nstack are return addresses,
 *    and they will be skipped thanks to the low/high range check.
 * 2. Dead values, even if mistaken for pointers into the low/high area,
 *    can be offset safely since they won't be dereferenced.
 *
 * XXX: WARNING: If HiPE starts storing other non-Erlang values on the
 * nstack, such as floats, then this will have to be changed.
 */
#define offset_nstack(p,offs,low,high) offset_heap_ptr(hipe_nstack_start((p)),hipe_nstack_used((p)),(offs),(low),(high))

#else /* !HIPE */

#define fullsweep_nstack(p,n_htop)		(n_htop)
#define GENSWEEP_NSTACK(p,old_htop,n_htop,objv,nobj)	do{}while(0)
#define offset_nstack(p,offs,low,high)		do{}while(0)

#endif /* HIPE */

int setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset)
{
    int n;
    ErlMessage* mp;
    Eterm* v_ptr;
    int v_msg_len;

    v_msg_len = 2 * p->msg.len;

    /*
     * Move pointers for all messages into an array pointed to by p->v_msg.
     */
    if (v_msg_len > ALENGTH(rootset->def_msg)) {
        rootset->v_msg = (Eterm *)
	    erts_alloc(ERTS_ALC_T_MSG_ROOTS, sizeof(Eterm) * v_msg_len);
    } else {
        rootset->v_msg = rootset->def_msg;
    }
    mp = p->msg.first;
    v_ptr = rootset->v_msg;
    while (mp != NULL) {
#if defined(ERTS_SMP) && !defined(HEAP_FRAG_ELIM_TEST)
	if (mp->bp) {
	    *v_ptr++ = NIL;
	    *v_ptr++ = NIL;
	}
	else
#endif
	{
	    *v_ptr++ = ERL_MESSAGE_TERM(mp);
	    ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		    is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		    is_atom(ERL_MESSAGE_TOKEN(mp))));
	    *v_ptr++ = ERL_MESSAGE_TOKEN(mp);
	}
        mp = mp->next;
    }

    n = 0;
    rootset->v[n]  = p->stop;
    rootset->sz[n] = STACK_START(p) - p->stop;
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

    ASSERT((is_nil(p->seq_trace_token) 
	    || is_tuple(p->seq_trace_token)
	    || is_atom(p->seq_trace_token)));
    rootset->v[n] = &p->seq_trace_token;
    rootset->sz[n] = 1;
    n++;

    ASSERT(is_nil(p->tracer_proc)
	   || is_internal_pid(p->tracer_proc)
	   || is_internal_port(p->tracer_proc));

    ASSERT(is_pid(p->group_leader));
    rootset->v[n]  = &p->group_leader;
    rootset->sz[n] = 1;
    ++n;

    /*
     * The process may be garbage-collected while it is terminating.
     * (fvalue contains EXIT reason and ftrace the saved stack trace.)
     */
    rootset->v[n]  = &p->fvalue;
    rootset->sz[n] = 1;
    n++;
    rootset->v[n]  = &p->ftrace;
    rootset->sz[n] = 1;
    n++;

#ifdef HYBRID
    if ((ma_gc_flags & GC_GLOBAL) && (p->nrr != 0))
    {
        rootset->v[n]  = p->rrma;
        rootset->sz[n] = p->nrr;
        n++;
    }
#endif

#if HIPE && defined(HYBRID)
    rootset->p = p;
#endif

    ASSERT(n <= ALENGTH(rootset->v));
    return n;
}

#if defined(HYBRID)
Uint collect_roots(Process* current, Eterm *objv, int nobj, Rootset rootset[])
{
    Process* p;
    Uint i, j = 0;
    Uint n = erts_num_active_procs;

    for (i = 0; i < n; i++) {
        p = erts_active_procs[i];
	if ((IS_ACTIVE(p) && INC_IS_ACTIVE(p)) ||
            ma_gc_flags & GC_INCLUDE_ALL) {
	    if (p == current) {
		rootset[j].n = setup_rootset(p, objv, nobj, &rootset[j]);
	    } else {
		rootset[j].n = setup_rootset(p, p->arg_reg, p->arity, &rootset[j]);
	    }
	    j++;
	}
    }
    return j;
}
#endif

static ERTS_INLINE
void restore_this_rootset(Process *p, Rootset *rootset)
{
    Eterm* v_ptr;
    ErlMessage* mp;

#ifdef HYBRID
    /*
     * Restore remembered rootset of this process.
     */
    if(!(ma_gc_flags & GC_GLOBAL)) {
        /*
         * If this was a collection of a private heap, make sure to
         * strike out pointers to the message area that was dead.
         */
        int i;
        for (i = 0; i < p->nrr; i++) {
            if (ptr_within(p->rrsrc[i],p->heap,p->hend)) {
                RRMA_REMOVE(p,i);
            }
        }
    }
    else {
        /*
         * If this was a collection of the message area, update
         * pointers in private heaps to point to the new message area.
         */
        int i;
        for (i = 0; i < p->nrr; i++) {
            ASSERT(p->rrsrc[i] != NULL);
            *(p->rrsrc[i]) = p->rrma[i];
        }
    }
#endif

    /*
     * Restore all message pointers.
     */
    mp = p->msg.first;
    v_ptr = rootset->v_msg;
    while (mp != NULL) {
#if defined(ERTS_SMP) && !defined(HEAP_FRAG_ELIM_TEST)
	if (mp->bp) {
	    v_ptr += 2;
	}
	else
#endif
	{
	    ERL_MESSAGE_TERM(mp) = *v_ptr++;
	    ASSERT((is_nil(*v_ptr) || is_tuple(*v_ptr) || is_atom(*v_ptr)));
	    ERL_MESSAGE_TOKEN(mp) = *v_ptr++;
	}
	mp = mp->next;
    }
    
    if (rootset->v_msg != rootset->def_msg) {
        erts_free(ERTS_ALC_T_MSG_ROOTS, rootset->v_msg);
    }
}

#if defined(HYBRID)
#ifdef INCREMENTAL
void restore_one_rootset(Process *p, Rootset *rootset)
{
    restore_this_rootset(p, rootset);
}
#endif
void restore_rootset(Process *p, Rootset *rootset)
{
    if (ma_gc_flags & GC_GLOBAL)
    {
        Uint i, j = 0;
        Uint n = erts_num_active_procs;

        for (i = 0; i < n; i++) {
            p = erts_active_procs[i];
            if ((IS_ACTIVE(p) && INC_IS_ACTIVE(p)) ||
                ma_gc_flags & GC_INCLUDE_ALL) {
                restore_this_rootset(p, &rootset[j++]);
            }
        }
    }
    else
        restore_this_rootset(p, rootset);
}
#else
void restore_rootset(Process *p, Rootset *rootset)
{
    restore_this_rootset(p, rootset);
}
#endif /* HYBRID */

/*
 * Remove all message buffers.
 */
static void
remove_message_buffers(Process* p)
{
    ErlHeapFragment* bp = MBUF(p);

    MBUF(p) = NULL;
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
** This function is used when using fullsweep gc.
** This replaces gen_gc for processes that use the
** fullsweep algorithm and don't have an old_heap.
** Parameters:
** p: The process who is being garbage collected.
** new_sz: The wanted size of the heap after collecting.
** objv: Vector of "extra" objects to be "saved".
** nobj: Number of objects in objv.
*/
static void fullsweep_heap(Process *p, int new_sz, Eterm* objv, int nobj)
{	
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Eterm* n_hstart;		/* Start of new heap */
    Eterm* n_htop;		/* Top of new heap */
    Eterm* n_heap;		/* The new heap */
    Eterm* n_hp;
    int n;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif
#ifdef HYBRID
    Eterm *g_heap  = global_heap;
    Eterm *g_htop  = global_htop;
    Eterm *go_heap = global_old_heap;
    Eterm *go_htop = global_old_hend;
#endif
#ifdef INCREMENTAL
    Eterm *i_heap = inc_fromspc;
    Eterm *i_hend = inc_fromend;
#endif

    /* Create new, empty heap */
    n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);
    n_hstart = n_htop = n_heap;
    FLAGS(p) &= ~F_NEED_FULLSWEEP;

    VERBOSE(DEBUG_PRIVATE_GC,("Fullsweep GC\n"));

#ifdef HYBRID
    p->nrr = 0;
#endif

    n = setup_rootset(p, objv, nobj, &rootset);
    n_htop = fullsweep_nstack(p, n_htop);
    while (n--) {
        Eterm* g_ptr = rootset.v[n];
        Eterm g_sz = rootset.sz[n];
	while(g_sz--) {
	    Eterm* ptr;
	    Eterm val;
	    Eterm gval = *g_ptr;

	    switch (primary_tag(gval)) {

	      case TAG_PRIMARY_BOXED: {
		ptr = boxed_val(gval);
		val = *ptr;
		if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *g_ptr++ = val;
                }
#ifdef HYBRID
                else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                         ptr_within(ptr, i_heap, i_hend) ||
#endif
                         ptr_within(ptr, go_heap, go_htop) )
                {
                    ++g_ptr;
                }
#endif
#if HIPE
		else if( in_area(ptr, const_start, const_size) ) {
		    ++g_ptr;
                }
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
#ifdef HYBRID
                else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                         ptr_within(ptr, i_heap, i_hend) ||
#endif
                         ptr_within(ptr, go_heap, go_htop) )
                {
                    ++g_ptr;
                }
#endif
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

    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap, so the
     * next stage is to scan through the new heap evacuating data from
     * the old heap until all is copied.
     */
    n_hp = n_heap;
    
    while (n_hp != n_htop) {
	Eterm* ptr;
	Eterm val;
	Eterm gval = *n_hp;

	switch (primary_tag(gval)) {

	case TAG_PRIMARY_BOXED: {
	    ptr = boxed_val(gval);
	    val = *ptr;
	    if (MY_IS_MOVED(val)) {
		ASSERT(is_boxed(val));
		*n_hp++ = val;
            }
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
		    ptr_within(ptr, i_heap, i_hend) ||
#endif
		    ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
            }
#endif
#if HIPE
	    else if( in_area(ptr, const_start, const_size) ) {
		++n_hp;
            }
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
	    if (is_non_value(val)) {
		*n_hp++ = ptr[1];
            }
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
		    ptr_within(ptr, i_heap, i_hend) ||
#endif
		    ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
            }
#endif
#if HIPE
	    else if( in_area(ptr, const_start, const_size) ) {
		++n_hp;
            }
#endif
	    else {
		ASSERT(within(ptr, p));
		MOVE_CONS(ptr,val,n_htop,n_hp++);
	    }
	    continue;
	}

	case TAG_PRIMARY_HEADER: {
	    if (header_is_thing(gval)) {
		if (header_is_bin_matchstate(gval)) {
		    ErlBinMatchState *ms = (ErlBinMatchState*) n_hp;
		    ErlBinMatchBuffer *mb = &(ms->mb);
		    Eterm* origptr;	
		    origptr = &(mb->orig);
		    ptr = boxed_val(*origptr);
		    val = *ptr;
		    if (MY_IS_MOVED(val)) {
			*origptr = val;
			mb->base = binary_bytes(*origptr);
		    }
#if HIPE
		    else if( in_area(ptr, const_start, const_size) )
		    {} /* Orig is in constant area no changes needed*/
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                     ptr_within(ptr, i_heap, i_hend) ||
#endif
                     ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
            }
#endif
#endif	 
		    else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,origptr); 
			mb->base=binary_bytes(*origptr);
			ptr = boxed_val(*origptr);
			val = *ptr;
		    }
		}
		n_hp += (thing_arityval(gval)+1);
	    }
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

    restore_rootset(p, &rootset);
    if (MSO(p).mso) {
	sweep_proc_bins(p, 1);
    }

#ifndef HYBRID /* FIND ME! */
    if (MSO(p).funs) {
	sweep_proc_funs(p, 1);
    }
#endif

    if (MSO(p).externals) {
	sweep_proc_externals(p, 1);
    }

    remove_message_buffers(p);

    if (OLD_HEAP(p) != NULL) {
#ifdef DEBUG
        sys_memset(OLD_HEAP(p), DEBUG_BAD_BYTE,
                   (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
#endif
        ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       OLD_HEAP(p),
		       (OLD_HEND(p)-OLD_HEAP(p))*sizeof(Eterm));
        OLD_HEAP(p) = OLD_HTOP(p) = OLD_HEND(p) = NULL;
    }

    /* Move the stack, the beam stack is "in the heap" */
    n = HEAP_END(p) - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));

#ifdef DEBUG
    sys_memset(HEAP_START(p), DEBUG_BAD_BYTE, 
               (HEAP_END(p) - HEAP_START(p)) * sizeof(Eterm));
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   HEAP_SIZE(p)*sizeof(Eterm));

    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_heap + new_sz;
    p->stop = HEAP_END(p) - n;
    GEN_GCS(p) = 0;

    /*
     * Should we set p->high_water to p->heap or p->htop?
     *
     * Setting it to p->htop means that any surviving data will be
     * placed on the old heap in the next garbage collection.
     * This setting gives a better estone value, but one can assume
     * that more garbage is placed on the old heap.
     *
     * Setting it to p->heap means that two more garbage collections
     * are needed to move data to the old heap.
     */

    HIGH_WATER(p) = HEAP_TOP(p);
#ifdef INCREMENTAL
    p->scan_top = HEAP_TOP(p);
#endif
}

void
erts_offset_off_heap(ErlOffHeap *ohp, Sint offs, Eterm* low, Eterm* high)
{
    if (ohp->mso && ptr_within((Eterm *)ohp->mso, low, high)) {
        Eterm** uptr = (Eterm**) &ohp->mso;
        *uptr += offs;
    }

#ifndef HYBRID /* FIND ME! */
    if (ohp->funs && ptr_within((Eterm *)ohp->funs, low, high)) {
        Eterm** uptr = (Eterm**) &ohp->funs;
        *uptr += offs;
    }
#endif

    if (ohp->externals && ptr_within((Eterm *)ohp->externals, low, high)) {
        Eterm** uptr = (Eterm**) &ohp->externals;
        *uptr += offs;
    }
}

static void
offset_off_heap(Process* p, Eterm* low, Eterm* high, Sint offs)
{
    erts_offset_off_heap(&MSO(p), offs, low, high);
}

static void
offset_rootset(Process *p, Sint offs, 
	       Eterm* low, Eterm* high, 
	       Eterm* objv, int nobj)
{
    if (p->dictionary) 
	offset_heap(p->dictionary->data, 
		    p->dictionary->used, 
		    offs, low, high);
    if (p->debug_dictionary) 
	offset_heap(p->debug_dictionary->data, 
		    p->debug_dictionary->used, 
		    offs, low, high);
    offset_heap(&p->fvalue, 1, offs, low, high);
    offset_heap(&p->ftrace, 1, offs, low, high);
    offset_heap(&p->group_leader, 1, offs, low, high);
    offset_heap(&p->seq_trace_token, 1, offs, low, high);
    offset_mqueue(p, offs, low, high);
    offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, low, high);
    offset_nstack(p, offs, low, high);
    if (nobj > 0) {
	offset_heap_ptr(objv, nobj, offs, low, high);
    }
    offset_off_heap(p, low, high, offs);

#ifdef HYBRID
    if (p->nrr > 0)
    {
        int i;
        for (i = 0; i < p->nrr; i++) {
            if (ptr_within(p->rrsrc[i],low,high))
                p->rrsrc[i] += offs;
        }
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
    int heap_size = HEAP_TOP(p) - HEAP_START(p);
    int stack_size = p->hend - p->stop;
    Sint offs;
    
    ASSERT(HEAP_SIZE(p) < new_sz);
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)HEAP_START(p),
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);

    VERBOSE(DEBUG_PRIVATE_GC,
            ("grow_new_heap: FROM %d UPTO %d (used %d)\n",
             HEAP_SIZE(p), new_sz, heap_size));

    if ((offs = new_heap - HEAP_START(p)) == 0) { /* No move. */
        HEAP_END(p) = new_heap + new_sz;
        sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
        p->stop = p->hend - stack_size;
    } else {
        Eterm* prev_stop = p->stop;
        offset_heap(new_heap, heap_size, offs, HEAP_START(p), HEAP_TOP(p));
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));

#ifdef INCREMENTAL
        p->scan_top = new_heap + (p->scan_top - HEAP_START(p));
#endif

        HEAP_END(p) = new_heap + new_sz;
        prev_stop = new_heap + (p->stop - p->heap);
        p->stop = p->hend - stack_size;
        sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));
        offset_rootset(p, offs, HEAP_START(p), HEAP_TOP(p), objv, nobj);
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

    ASSERT(new_sz != 0);
    ASSERT(new_sz < p->heap_sz);
    sys_memmove(p->heap + new_sz - stack_size, p->stop, stack_size *
                                                        sizeof(Eterm));
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)p->heap,
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);
    p->hend = new_heap + new_sz;
    p->stop = p->hend - stack_size;

    VERBOSE(DEBUG_PRIVATE_GC,
            ("shrink_new_heap: FROM %d DOWNTO %d (used %d)\n",
             HEAP_SIZE(p), new_sz, heap_size));

    if ((offs = new_heap - HEAP_START(p)) != 0) {

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * know on some strange embedded systems...  Or when using purify.
         */

        offset_heap(new_heap, heap_size, offs, HEAP_START(p), HEAP_TOP(p));
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));
#ifdef INCREMENTAL
        p->scan_top = new_heap + (p->scan_top - HEAP_START(p));
#endif
        offset_rootset(p, offs, HEAP_START(p), HEAP_TOP(p), objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
}

static Uint
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
    int stack_size =  p->hend - p->stop;
    Uint reclaimed_now;
    
    size_after = (HEAP_TOP(p) - HEAP_START(p));
    reclaimed_now = (size_before - size_after);
    
    /*
     * Resize the heap if needed.
     */

    need_after = size_after + need + stack_size;
    if (HEAP_SIZE(p) < need_after) {
        /* Too small - grow to match requested need */
        sz = next_heap_size(p, need_after, 0);
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
            sz = next_heap_size(p, wanted, 0);
        }
        if (sz < HEAP_SIZE(p)) {
            erts_shrink_new_heap(p, sz, objv, nobj);
        }
    }

    return reclaimed_now;
}

/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of (erlang) words needed on the heap.
 * objv: Array of terms to add to rootset, that is to preserve.
 * nobj: Number of objects in objv.
 */
int
erts_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    Uint reclaimed_now = 0;
    int size_before;
    int size_after;
    int need_after;
    Uint saved_status;
    int wanted;
    int stack_size;             /* Size of stack ON HEAP. */
    int sz;
#ifdef __BENCHMARK__
    uint this_was_major = 0;
#endif
    Uint ms1, s1, us1;

    BM_STOP_TIMER(system);
    VERBOSE(DEBUG_PRIVATE_GC, ("Heap GC START Proc: %T\n", p->id));

#ifdef BM_HEAP_SIZES
    {
        double total_used_heap = 0;
        int i;
        for (i = 0; i < erts_max_processes; i++)
        {
            Process *cp = process_tab[i];
            if (cp == NULL) continue;

            total_used_heap += (cp->htop - cp->heap) + cp->mbuf_sz +
                               (cp->old_htop - cp->old_heap);
        }
        if (total_used_heap > max_used_heap)
            max_used_heap = total_used_heap;
    }
#endif /* BM_HEAP_SIZES */

    BM_RESET_TIMER(gc);
    BM_START_TIMER(gc);

#ifdef HEAP_FRAG_ELIM_TEST
    if (SAVED_HEAP_TOP(p) != NULL) {
	HEAP_TOP(p) = SAVED_HEAP_TOP(p);
	SAVED_HEAP_TOP(p) = NULL;
    }
#endif

#define OverRunCheck()                                                       \
    if (HEAP_LIMIT(p) < HEAP_TOP(p)) {                                       \
        erl_exit(1, "%T: Heap-top passed heap limit at line %d\n",           \
                 p->id, __LINE__);                                           \
    }

    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_start);
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);

    if (erts_system_monitor_long_gc != 0) {
	get_now(&ms1, &s1, &us1);
    }
    saved_status = p->status;
    p->status = P_GARBING;

    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    erts_smp_locked_activity_begin(ERTS_ACTIVITY_GC);

#if defined(ERTS_SMP) && defined(HEAP_FRAG_ELIM_TEST)
    {
	ErlMessage *msgp;
	for (msgp = p->msg.first; msgp; msgp = msgp->next) {
	    if (msgp->bp)
		erts_move_msg_mbuf_to_proc_mbufs(p, msgp);
	}
    }
#endif

    CHECK(p);
    OverRunCheck();


    if (GEN_GCS(p) >= MAX_GEN_GCS(p)) {
        FLAGS(p) |= F_NEED_FULLSWEEP;
    }

#ifdef HYBRID
    if (p->rrma == NULL) {
        p->nrr = 0;
        p->rrsz = RRMA_DEFAULT_SIZE;
        p->rrma  = erts_alloc(ERTS_ALC_T_ROOTSET,
                              RRMA_DEFAULT_SIZE * sizeof(Eterm));
        p->rrsrc = erts_alloc(ERTS_ALC_T_ROOTSET,
                              RRMA_DEFAULT_SIZE * sizeof(Eterm));
        ERTS_PROC_MORE_MEM(sizeof(Eterm) * p->rrsz * 2);
    }
#endif

    stack_size = p->hend - p->stop;

    MSO(p).overhead = 0;

    /* Size of heap before first GC */
    size_before = MBUF_SIZE(p) + (HEAP_TOP(p) - HEAP_START(p));

    /*
     * Generational GC from here on. We need an old heap.
     */

    if (OLD_HEAP(p) == NULL && HIGH_WATER(p) != HEAP_START(p) &&
        (FLAGS(p) & F_NEED_FULLSWEEP) == 0) {
        Eterm* n_old;
        /* Note: We choose a larger heap size than strictly needed,
         * which seems to reduce the number of fullsweeps.
         * This improved Estone by more than 1200 estones on my computer
         * (Ultra Sparc 10).
         */
        size_t new_sz = next_heap_size(p, HIGH_WATER(p) - HEAP_START(p), 1);

        /* Create new, empty old_heap */
        n_old = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
					  sizeof(Eterm)*new_sz);

        OLD_HEND(p) = n_old + new_sz;
        OLD_HEAP(p) = OLD_HTOP(p) = n_old;
        VERBOSE(DEBUG_PRIVATE_GC,("Created an old_heap\n"));
    }

    /*
     * Try a generational GC if the old heap is large enough.
     */

    if ((FLAGS(p) & F_NEED_FULLSWEEP) == 0 &&
        HIGH_WATER(p) - HEAP_START(p) <= OLD_HEND(p) - OLD_HTOP(p)) {
        /*
         * There is space enough in old_heap for everything
         * below the high water mark.  Do a generational GC.
         */

        gen_gc(p, next_heap_size(p, HEAP_SIZE(p) + MBUF_SIZE(p), 0),
               objv, nobj);

        GEN_GCS(p)++;
        size_after = HEAP_TOP(p) - HEAP_START(p);
        need_after = size_after + need + stack_size;
        reclaimed_now += (size_before - size_after);

        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */
        if ((HEAP_SIZE(p) > 300) && (4 * need_after < HEAP_SIZE(p)) &&
            ((HEAP_SIZE(p) > 8000)
             || (HEAP_SIZE(p) > (OLD_HEND(p) - OLD_HEAP(p))))) {
            wanted = 3 * need_after;
            if (wanted < p->min_heap_size) {
                wanted = p->min_heap_size;
            } else {
                wanted = next_heap_size(p, wanted, 0);
            }
            if (wanted < HEAP_SIZE(p)) {
                erts_shrink_new_heap(p, wanted, objv, nobj);
            }
            ASSERT(HEAP_SIZE(p) == p->min_heap_size
		   || HEAP_SIZE(p) == next_heap_size(p, HEAP_SIZE(p), 0));
            goto done;
        }

        /*
         * The heap size turned out to be just right. We are done.
         */

        if (HEAP_SIZE(p) >= need_after) {
            ASSERT(HEAP_SIZE(p) == next_heap_size(p, HEAP_SIZE(p), 0));
            goto done;
        }
        VERBOSE(DEBUG_PRIVATE_GC,
                ("Did a gen_gc, still not enough room\n"));
    }

    /*
     * The previous generational GC did not leave enough free heap space.
     * We must do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    sz = HEAP_SIZE(p) + MBUF_SIZE(p) + (OLD_HTOP(p) - OLD_HEAP(p));
    sz += p->hend - p->stop;
    sz = next_heap_size(p, sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (sz == HEAP_SIZE(p) && FLAGS(p) & F_HEAP_GROW) {
        sz = next_heap_size(p, HEAP_SIZE(p), 1);
    }
    FLAGS(p) &= ~F_HEAP_GROW;


    fullsweep_heap(p, sz, objv, nobj);

    CHECK(p);
    reclaimed_now += adjust_after_fullsweep(p, size_before, need, objv, nobj);

#ifdef __BENCHMARK__
    this_was_major = 1;
#endif


 done:

    erts_smp_spin_lock(&info_lck);
    garbage_cols++;
    reclaimed += reclaimed_now;
    erts_smp_spin_unlock(&info_lck);

    CHECK(p);
    OverRunCheck();

    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    p->status = saved_status;
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);

    if (IS_TRACED_FL(p, F_TRACE_GC)) {
        trace_gc(p, am_gc_end);
    }
    BM_STOP_TIMER(gc);

    erts_smp_locked_activity_end(ERTS_ACTIVITY_GC);

    if (erts_system_monitor_long_gc != 0) {
	Uint ms2, s2, us2;
	Sint t;
	get_now(&ms2, &s2, &us2);
	t = ms2 - ms1;
	t = t*1000000 + s2 - s1;
	t = t*1000 + ((Sint)(us2 - us1))/1000;
	if (t > 0 && (Uint)t >= erts_system_monitor_long_gc) {
	    monitor_long_gc(p, t);
	}
    }
    if (erts_system_monitor_large_heap != 0
	&& HEAP_SIZE(p) >= erts_system_monitor_large_heap) {
	monitor_large_heap(p);
    }

#ifdef __BENCHMARK__
#ifdef BM_TIMERS
    local_pause_times[(((gc_time * 1000) < MAX_PAUSE_TIME) ?
                       (int)(gc_time * 1000) :
                       MAX_PAUSE_TIME - 1)]++;
#endif
    if (this_was_major == 1) {
        BM_COUNT(major_gc);
#ifdef BM_TIMERS
        major_gc_time += gc_time;
        if (gc_time > max_major_time)
            max_major_time = gc_time;
#endif
    }
    else
    {
        BM_COUNT(minor_gc);
#ifdef BM_TIMERS
        minor_gc_time += gc_time;
        if (gc_time > max_minor_time)
            max_minor_time = gc_time;
#endif
    }

#ifdef BM_HEAP_SIZES
    {
        double total_used_heap = 0;
        double total_allocated_heap = 0;
        int i;
        for (i = 0; i < erts_max_processes; i++)
        {
            Process *cp = process_tab[i];
            if (cp == NULL) continue;

            total_used_heap += (cp->htop - cp->heap) +
                               (cp->old_htop - cp->old_heap);
            total_allocated_heap += cp->heap_sz +
                                   (cp->old_hend - cp->old_heap);
        }
        if (total_used_heap > max_used_heap)
            max_used_heap = total_used_heap;
        if (total_allocated_heap > max_allocated_heap)
            max_allocated_heap = total_allocated_heap;

#ifdef BM_TIMERS_MAXALLOC
        {
            int min,sec,milli,micro;
            BM_TIMER_T tmp = ((BM_TIMER_T)vperfctr_read_tsc(system_clock) / cpu_khz) * 1000;
            micro = (uint)(tmp - ((int)(tmp / 1000)) * 1000);
            tmp /= 1000;
            milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000);
            tmp /= 1000;
            sec = (uint)(tmp - ((int)(tmp / 60)) * 60);
            min = (uint)tmp / 60;
            erts_fprintf(stderr,"%4d:%02d.%03d %03d : Footprint: %d\n",
                         min,sec,milli,micro,(ulong)total_used_heap);
        }
#endif
    }
#endif /* BM_HEAP_SIZES */

    BM_START_TIMER(system);

#endif /* __BENCHMARK__ */

    ARITH_AVAIL(p) = 0;
    ARITH_HEAP(p) = NULL;
#ifdef DEBUG
    ARITH_CHECK_ME(p) = NULL;
#endif

#ifdef CHECK_FOR_HOLES
    /*
     * We intentionally do not rescan the areas copied by the GC.
     * We trust the GC not to leave any holes.
     */
    {
	Eterm* start = p->htop;
	Eterm* stop = p->stop;
	p->last_htop = p->htop;
	p->last_mbuf = 0;
	while (start < stop) {
	    *start++ = ERTS_HOLE_MARKER;
	}
    }
#endif    

#ifdef HYBRID
#ifdef DEBUG
    {
        int i;
        for (i = 0; i < p->nrr; i++) {
            Eterm *ptr = p->rrsrc[i];
            ASSERT(ptr_within(ptr,HEAP_START(p),HEAP_TOP(p)) ||
                   ptr_within(ptr,OLD_HEAP(p),OLD_HTOP(p)));
        }
    }
#endif
#endif

    VERBOSE(DEBUG_PRIVATE_GC,("Heap GC END\n"));

    return ((int) (HEAP_TOP(p) - HEAP_START(p)) / 10);
#undef OverRunCheck
}

static Eterm*
gen_cheney(Process *p, Eterm* low, Eterm* high, Eterm* n_hp, Eterm* n_htop)
{
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    char* water_start = (char *)low;
    Uint water_size = (char *)high - water_start;
#ifdef HYBRID
    Eterm *g_start = global_heap;
    Eterm *g_end = global_htop;
    Eterm *go_start = global_old_heap;
    Eterm *go_end = global_old_hend;
#endif
#ifdef INCREMENTAL
    Eterm *i_heap = inc_fromspc;
    Eterm *i_hend = inc_fromend;
#endif

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
            ptr = boxed_val(gval);
            val = *ptr;
	    if (in_area(ptr, water_start, water_size)) {
		if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
                }
#ifdef HYBRID
            } else if (ptr_within(ptr,g_start,g_end) ||
#ifdef INCREMENTAL
                       ptr_within(ptr, i_heap, i_hend) ||
#endif
                       ptr_within(ptr,go_start,go_end)) {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
#endif
            } else {
                ASSERT(within(ptr, p));
                ++n_hp;
            }
            continue;
	}
	case TAG_PRIMARY_LIST: {
            ptr = list_val(gval);
            val = *ptr;
	    if (in_area(ptr, water_start, water_size)) {
		if (is_non_value(val)) {
		    *n_hp++ = ptr[1];
		} else {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		}
#ifdef HYBRID
            } else if (ptr_within(ptr,g_start,g_end) ||
#ifdef INCREMENTAL
                       ptr_within(ptr, i_heap, i_hend) ||
#endif
                       ptr_within(ptr,go_start,go_end)) {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
#endif
            } else {
                ASSERT(within(ptr, p));
                ++n_hp;
            }
            continue;
          }
          case TAG_PRIMARY_HEADER: {
	    if (header_is_thing(gval)) {
	      if (header_is_bin_matchstate(gval)) {
		ErlBinMatchState *ms = (ErlBinMatchState *) n_hp;
		ErlBinMatchBuffer *mb = &(ms->mb);
		Eterm* orig;
		orig = &(mb->orig);
		ptr = boxed_val((mb->orig));
		val = *ptr; 
		if (in_area(ptr, water_start, water_size)) {
		  /* Orig is in high water*/
		  if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *orig = val; 
		    mb->base=binary_bytes((mb->orig));
		  }
		  else {
		   MOVE_BOXED(ptr,val,n_htop,orig);
		   mb->base=binary_bytes((mb->orig));
		  } 
		}
#ifdef HYBRID
	       else if (ptr_within(ptr,g_start,g_end) ||
#ifdef INCREMENTAL
			 ptr_within(ptr, i_heap, i_hend) ||
#endif
			 ptr_within(ptr,go_start,go_end)) {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
	       }
#endif
		else {
		  ASSERT(within(ptr, p));		  	
		}
	      }
	      n_hp += (thing_arityval(gval)+1);
	    }
	    else
	      n_hp++;
	    continue;
          }
          default: {
            ++n_hp;
            continue;
          }
        }
    }

    return n_htop;
}

/*
 * This function sweeps both the remainder of the new heap
 * as well as the remainder of the old heap after the first pass
 * of the generational collector gen_gc().
 */
static Eterm*
gen_cheney_old(Process *p, Eterm* from, Eterm** to,
	       Eterm* low, Eterm* high, Eterm* old_htop,
	       Eterm* objv, int nobj)
{
 /*  printf("small New heap: 0x%08lx Old heap: 0x%08lx\n", (Uint)from,(Uint)OLD_HEAP(p)); */
    Eterm* n_hp;
    Eterm* n_htop;
    Eterm* oh_start;
    Eterm* oh_end;

    char* water_start = (char *)low;
    Uint water_size = (char *)high - water_start;

    Eterm* ptr;
    Eterm val;
    Eterm gval;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif
#ifdef HYBRID
    Eterm *g_heap  = global_heap;
    Eterm *g_htop  = global_htop;
    Eterm *go_heap = global_old_heap;
    Eterm *go_htop = global_old_hend;
#endif
#ifdef INCREMENTAL
    Eterm *i_heap = inc_fromspc;
    Eterm *i_hend = inc_fromend;
#endif

    n_hp = from;
    n_htop = *to;
    oh_start = OLD_HEAP(p);
    oh_end = OLD_HEND(p);

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
          case TAG_PRIMARY_BOXED: {
            ptr = boxed_val(gval);
            val = *ptr;
            if (ptr_within(ptr, oh_start, oh_end))
            {
                n_hp++;
            }
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                     ptr_within(ptr, i_heap, i_hend) ||
#endif
                     ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
            }
#endif
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
            {
                ++n_hp;
            }
#endif
            else if (MY_IS_MOVED(val)) {
		ASSERT(is_boxed(val));
                *n_hp++ = val;
            } else if (in_area(ptr, water_start, water_size)) {
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
            {
                n_hp++;
            }
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                     ptr_within(ptr, i_heap, i_hend) ||
#endif
                     ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
                ++n_hp;
            }
#endif
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
            {
                ++n_hp;
            }
#endif
            else if (is_non_value(val)) {
                *n_hp++ = ptr[1];
            } else if (in_area(ptr, water_start, water_size)) {
                /* Make object old */
                MOVE_CONS(ptr,val,old_htop,n_hp++);
            } else {
                ASSERT(within(ptr, p));
                MOVE_CONS(ptr,val,n_htop,n_hp++);
            }
            continue;
          }
          case TAG_PRIMARY_HEADER: {
	    if (header_is_thing(gval)) {
	      if (header_is_bin_matchstate(gval)) {
		ErlBinMatchState *ms = (ErlBinMatchState *) n_hp;
		ErlBinMatchBuffer *mb = &(ms->mb);
		Eterm* orig;
		orig = &(mb->orig);
		ptr = boxed_val((mb->orig));
		val = *ptr;
		if (ptr_within(ptr, oh_start, oh_end))
		  {} /* Orig is in old heap no changes needed*/
#ifdef HYBRID
            else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                     ptr_within(ptr, i_heap, i_hend) ||
#endif
                     ptr_within(ptr, go_heap, go_htop) )
            {
                RRMA_STORE(p,gval,n_hp);
               
            }
#endif

#if HIPE
		else if( in_area(ptr, const_start, const_size) )
		  {} /* Orig is in constant area no changes needed*/
#endif
		else if (MY_IS_MOVED(val)) {
		  *orig = val;
		  mb->base=binary_bytes(val);
		}
		else if (in_area(ptr, water_start, water_size)) {
		  MOVE_BOXED(ptr,val,old_htop,orig);
		  mb->base=binary_bytes((mb->orig));
		} 
		else {
		  ASSERT(within(ptr, p));		  
		  MOVE_BOXED(ptr,val,n_htop,orig);
		  mb->base=binary_bytes(*orig);		
		}
	      }
	      n_hp += (thing_arityval(gval)+1);
	    }
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
#include "hipe_debug.h"
static void
gen_gc(Process *p, int new_sz, Eterm* objv, int nobj)
{
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
    Eterm* n_hstart;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    Eterm* oh_start = OLD_HEAP(p);
    Eterm* oh_end = OLD_HEND(p);
    Eterm* old_htop = OLD_HTOP(p);
    Eterm* low_water = HEAP_START(p);
    Eterm* high_water = HIGH_WATER(p);
    Eterm* tmp;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif
#ifdef HYBRID
    Eterm *g_heap  = global_heap;
    Eterm *g_htop  = global_htop;
    Eterm *go_heap = global_old_heap;
    Eterm *go_htop = global_old_hend;
#endif
#ifdef INCREMENTAL
    Eterm *i_heap = inc_fromspc;
    Eterm *i_hend = inc_fromend;
#endif

    VERBOSE(DEBUG_PRIVATE_GC,("Generational GC\n"));

    /* If flip is true, we need to tenure all (live) objects */
    /* within the watermarks, if flip is 0, we need to alloc a */
    /* new new_heap and copy all live objects to the new new_heap */
    /* that is to not tenure any objects at all */

    n_hstart = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);
    n_htop = n_hstart;
    n = setup_rootset(p, objv, nobj, &rootset);

    GENSWEEP_NSTACK(p, old_htop, n_htop, objv, nobj);
    while (n--) {
        Eterm* g_ptr = rootset.v[n];
        Uint g_sz = rootset.sz[n];

        while (g_sz--) {
            gval = *g_ptr;

            switch (primary_tag(gval)) {

	    case TAG_PRIMARY_BOXED: {
                ptr = boxed_val(gval);
                val = *ptr;
                if (ptr_within(ptr, oh_start, oh_end)) {
                    g_ptr++;
                }
#ifdef HYBRID
                else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                         ptr_within(ptr, i_heap, i_hend) ||
#endif
                         ptr_within(ptr, go_heap, go_htop) )
                {
                    ++g_ptr; 
                }
#endif
#if HIPE
                else if( in_area(ptr, const_start, const_size) )
                {
                    ++g_ptr;
                }
#endif
                else if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
                    *g_ptr++ = val;
                } else if (ptr_within(ptr, low_water, high_water)) {
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
                {
                    g_ptr++;
                }
#ifdef HYBRID
                else if( ptr_within(ptr, g_heap, g_htop) ||
#ifdef INCREMENTAL
                         ptr_within(ptr, i_heap, i_hend) ||
#endif
                         ptr_within(ptr, go_heap, go_htop) )
                {
                    ++g_ptr;
                }
#endif
#if HIPE
                else if( in_area(ptr, const_start, const_size) )
                {
                    ++g_ptr;
                }
#endif
                else if (is_non_value(val)) {
                    *g_ptr++ = ptr[1];
                }
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
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */
    tmp = n_htop;
    old_htop = gen_cheney_old(p, n_hstart, &tmp, low_water, high_water, old_htop, objv, nobj);
    n_htop = tmp;

    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) heap.
     */

    if (OLD_HTOP(p) < old_htop) {
      if (MBUF(p) == NULL) {
          old_htop = gen_cheney(p, HEAP_START(p), HEAP_END(p), OLD_HTOP(p), old_htop);
      } else {
          tmp = old_htop;
          (void) gen_cheney_old(p, OLD_HTOP(p), &tmp, OLD_HEAP(p),
                                OLD_HEAP(p), NULL, NULL, 0);
          old_htop = tmp;
      }
    }
    OLD_HTOP(p) = old_htop;

    restore_rootset(p, &rootset);

    if (MSO(p).mso) {
        sweep_proc_bins(p, 0);
    }

#ifndef HYBRID /* FIND ME! */
    if (MSO(p).funs) {
        sweep_proc_funs(p, 0);
    }
#endif

    if (MSO(p).externals) {
	sweep_proc_externals(p, 0);
    }

    remove_message_buffers(p);

    HIGH_WATER(p) = (HEAP_START(p) != HIGH_WATER(p)) ? n_hstart : n_htop;

    /*
     * Now we got to move the stack to the top of the new heap...
     */
    n = HEAP_END(p) - p->stop;
    sys_memcpy(n_hstart + new_sz - n, p->stop, n * sizeof(Eterm));

#ifdef DEBUG
    sys_memset(HEAP_START(p), DEBUG_BAD_BYTE, HEAP_SIZE(p) * sizeof(Eterm));
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   HEAP_SIZE(p)*sizeof(Eterm));

    HEAP_START(p) = n_hstart;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
    HEAP_END(p) = n_hstart + new_sz;
    p->stop = HEAP_END(p) - n;
#ifdef INCREMENTAL
    p->scan_top = HEAP_TOP(p);
#endif
}

static void
sweep_proc_externals(Process *p, int fullsweep)
{
    ExternalThing** prev;
    ExternalThing* ptr;
    Eterm* bot;
    Eterm* top;

#ifdef HYBRID
    if (ma_gc_flags & GC_GLOBAL) {
        bot = global_old_heap;
        top = global_old_hend;
        prev = &erts_global_offheap.externals;
        ptr = erts_global_offheap.externals;
    }
    else
#endif
    {
        bot = OLD_HEAP(p);
        top = OLD_HEND(p);
        prev = &MSO(p).externals;
        ptr = MSO(p).externals;
    }

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (MY_IS_MOVED(*ppt)) {        /* Object is alive */
            ExternalThing* ro = external_thing_ptr(*ppt);

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
#ifdef HYBRID
	} else if (fullsweep == 0 &&
		   (!(ma_gc_flags & GC_GLOBAL) &&
		    (ptr_within(ppt, global_heap, global_hend) ||
		     ptr_within(ppt, global_old_heap, global_old_hend)))) {
	    /*
	     * An entry in the list of externals for a PRIVATE heap points to
	     * the shared message area. This is WRONG!
	     */
	    abort();
#endif
        } else {                /* Object has not been moved - deref it */
	    erts_deref_node_entry(ptr->node);
            *prev = ptr = ptr->next;
        }
    }
    ASSERT(*prev == NULL);
}

#ifndef HYBRID /* FIND ME! */
static void
sweep_proc_funs(Process *p, int fullsweep)
{
    ErlFunThing** prev;
    ErlFunThing* ptr;
    Eterm* bot;
    Eterm* top;

#ifdef HYBRID
    if (ma_gc_flags & GC_GLOBAL)
    {
        bot = global_old_heap;
        top = global_old_hend;
        prev = &erts_global_offheap.funs;
        ptr = erts_global_offheap.funs;
    }
    else
#endif
    {
        bot = OLD_HEAP(p);
        top = OLD_HEND(p);
        prev = &MSO(p).funs;
        ptr = MSO(p).funs;
    }

    while (ptr) {
        Eterm* ppt = (Eterm *) ptr;

        if (MY_IS_MOVED(*ppt)) {        /* Object is alive */
            ErlFunThing* ro = (ErlFunThing *) fun_val(*ppt);

            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 &&
                   ( ptr_within(ppt, bot, top)
#ifdef HYBRID
                     || (!(ma_gc_flags & GC_GLOBAL) &&
                         ( ptr_within(ppt, global_heap, global_hend) ||
                           ptr_within(ppt, global_old_heap, global_old_hend)))
#endif
                     )) {

            /*
             * Object resides on old heap, and we just did a
             * generational collection - keep object in list.
             */
            prev = &ptr->next;
            ptr = ptr->next;
        } else {                /* Object has not been moved - deref it */
            ErlFunEntry* fe = ptr->fe;

            *prev = ptr = ptr->next;
	    if (erts_refc_dectest(&fe->refc, 0) == 0) {
                erts_erase_fun_entry(fe);
            }
        }
    }
    ASSERT(*prev == NULL);
}
#endif

static void
sweep_proc_bins(Process *p, int fullsweep)
{
    ProcBin** prev;
    ProcBin* ptr;
    Binary* bptr;
    Eterm* bot;
    Eterm* top;

#ifdef HYBRID
    if (ma_gc_flags & GC_GLOBAL)
    {
        bot  = global_old_heap;
        top  = global_old_hend;
        prev = &erts_global_offheap.mso;
        ptr  = erts_global_offheap.mso;
    }
    else
#endif
    {
        bot = OLD_HEAP(p);
        top = OLD_HEND(p);
        prev = &MSO(p).mso;
        ptr  = MSO(p).mso;
    }

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

        if (MY_IS_MOVED(*ppt)) {        /* Object is alive */
            ProcBin* ro = (ProcBin*) binary_val(*ppt);
            *prev = ro;         /* Patch to moved pos */
            prev = &ro->next;
            ptr = ro->next;
        } else if (fullsweep == 0 && ptr_within(ppt, bot, top)) {
            /*
             * Object resides on the old heap, and we just did a
             * generational collection - keep object in list.
             */
            prev = &ptr->next;
            ptr = ptr->next;
#ifdef HYBRID
	} else if (fullsweep == 0 &&
		   (!(ma_gc_flags & GC_GLOBAL) &&
		    (ptr_within(ppt, global_heap, global_hend) ||
		     ptr_within(ppt, global_old_heap, global_old_hend)))) {
	    /*
	     * An entry in the list of binaries for a PRIVATE heap points
	     * to the shared message area. This is WRONG!
	     */
	    abort();
#endif
        } else {                /* Object has not been moved - deref it */
            *prev = ptr->next;
            bptr = ptr->val;
            if (erts_refc_dectest(&bptr->refc, 0) == 0) {
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

void
erts_gc_info(ErtsGCInfo *gcip)
{
    if (gcip) {
	erts_smp_spin_lock(&info_lck);
	gcip->garbage_collections = garbage_cols;
	gcip->reclaimed = reclaimed;
	erts_smp_spin_unlock(&info_lck);
    }
}

extern void erts_init_ggc(void); /* Avoid warning... */
				    
void
erts_init_ggc(void) /*called from erl_gc.c:erts_init_gc() */
{
    erts_smp_spinlock_init(&info_lck, "gc_info");
    garbage_cols = 0;
    reclaimed = 0;
}

/************************************************************** */
/*  DEBUG routines                                              */
/****************************************************************/


#ifdef HARDDEBUG

int within(Eterm *ptr, Process *p)
{
    ErlHeapFragment* bp = MBUF(p);

    if (HEAP_START(p) <= ptr && ptr < HEAP_TOP(p)) {
        return 1;
    }

    if (OLD_HEAP(p) && (OLD_HEAP(p) <= ptr && ptr < OLD_HEND(p))) {
        return 1;
    }

#ifdef HYBRID
    if (global_heap <= ptr && ptr < global_htop) {
        return 1;
    }

    if (global_old_heap <= ptr && ptr < global_old_hend) {
        return 1;
    }

#ifdef INCREMENTAL
    if (inc_fromspc <= ptr && ptr < inc_fromend) {
        return 1;
    }
#endif
#endif

    while (bp != NULL) {
        if (bp->mem <= ptr && ptr < bp->mem + bp->size) {
            return 1;
        }
        bp = bp->next;
    }

#if HIPE
    if(in_area(ptr,(char*)hipe_constants_start,
               (char*)hipe_constants_next - (char*)hipe_constants_start))
        return 1;
#endif

    return 0;
}

static void
check_pointer(Process* p, Eterm obj, int back_pointers_allowed, char *msg)
{
    if (back_pointers_allowed && !within(ptr_val(obj), p)) {
        erl_exit(1, "%T: %s, line %d: %s: bad address %x\n",
                 p->id, __FILE__, __LINE__, msg, obj);
    } else if (!back_pointers_allowed &&
               !ptr_within(ptr_val(obj), OLD_HEAP(p), OLD_HEND(p))
               ) {
        if (within(ptr_val(obj), p)) {
            erl_exit(1, "%T: %s, line %d: %s: back pointer %x\n",
                     p->id, __FILE__, __LINE__, msg, obj);
        } else {
            erl_exit(1, "%T: %s, line %d: %s: bad address %x\n",
                     p->id, __FILE__, __LINE__, msg, obj);
        }
    }
}


static void
check_binary(Process *p, Eterm obj, char* msg)
{
    Eterm* ptr = binary_val(obj);
    int btype = thing_subtag(*ptr);

    ASSERT(is_thing(*ptr));

    if (*ptr == DEBUG_BAD_WORD) {
        erl_exit(1, "%T: Clobbered binary left\n", p->id);
    }
    check_pointer(p, obj, 1, msg);

    switch (btype) {
    case REFC_BINARY_SUBTAG:
        {
            ProcBin *bp = (ProcBin*)binary_val(obj);

            if (bp->size > 0xffffffff) {
                erl_exit(1, "%T: check_binary: %s: LARGE binary found %x\n",
                         p->id, msg, obj);
            }
            if (bp->bytes == 0) {
                erl_exit(1, "%T: check_binary: %s: NULL binary found %x\n",
                         p->id, msg, obj);
            }
            if (erts_refc_read(&bp->val->refc, 0) <= 0) {
                erl_exit(1, "Bad refc in binary\n");
            }
        }
        break;
    case HEAP_BINARY_SUBTAG:
	/* TODO */
	break;
    case SUB_BINARY_SUBTAG:
	/* TODO */
	break;
    default:
        erl_exit(1, "Unknown subtag in thing binary \n");
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
        erl_exit(1, "%T: Overrun stack and heap\n", p->id);
    }

    for (sp = p->stop; sp < p->hend; sp++) {
        if (is_not_catch(*sp)) {
            ;
        }
    }
}

int
chk_sys(void)
{
    Process *p = *process_tab;
    int i, res = 0;
    for (i = 0; i < erts_max_processes; i++) {
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
    ProcBin *pb = MSO(p).mso;
    while (pb) {
        check_binary(p, make_binary((Eterm*) pb), "CHECK");
        pb = pb->next;
    }
    return;
}

#endif  /* HARDDEBUG  */

#endif /* HEAP_FRAG_ELIM_TEST */


#if defined(HYBRID) && defined(DEBUG)
static void print_roots(Rootset *rootset, int n) /* FIND ME! */
{
    fprintf(stderr,"-- Rootset --\n\r");
    while (n--)
    {
        int i = rootset[n].n;
        while (i--)
        {
            Eterm* g_ptr = rootset[n].v[i];
            Uint g_sz = rootset[n].sz[i];
            while (g_sz--)
            {
                Eterm gval = *g_ptr++;
                fprintf(stderr,"Root: 0x%08x\n\r",(int)gval);
            }
        }
    }
    fprintf(stderr,"-------------\n\r");
}
#endif

#if defined(HYBRID) && !defined(INCREMENTAL)
/***************************************************************************
 *                                                                         *
 *                Garbage collection of the Message Area                   *
 *                                                                         *
 ***************************************************************************/

static void ma_gen_gc(Process*, int, Eterm*, int);


static void
ma_offset_off_heap(Process* p, Eterm* low, Eterm* high, int offs)
{
    if (erts_global_offheap.mso &&
        ptr_within((Eterm *)erts_global_offheap.mso, low, high)) {
        Eterm** uptr = (Eterm**) &erts_global_offheap.mso;
        *uptr += offs;
    }

#ifndef HYBRID /* FIND ME! */
    if (erts_global_offheap.funs &&
        ptr_within((Eterm *)erts_global_offheap.funs, low, high)) {
        Eterm** uptr = (Eterm**) &erts_global_offheap.funs;
        *uptr += offs;
    }
#endif

    if (erts_global_offheap.externals &&
        ptr_within((Eterm *)erts_global_offheap.externals, low, high)) {
        Eterm** uptr = (Eterm**) &erts_global_offheap.externals;
        *uptr += offs;
    }
}


static void
ma_offset_rootset(Process *cp, Sint offs, 
	       Eterm* low, Eterm* high, 
	       Eterm* objv, int nobj)
{
    Uint i;
    Uint n = erts_num_active_procs;
    Process *p;

    for (i = 0; i < n; i++) {
        p = erts_active_procs[i];
        if (p->dictionary) {
            offset_heap(p->dictionary->data, 
                        p->dictionary->used, 
                        offs, low, high);
	}
        if (p->debug_dictionary) {
            offset_heap(p->debug_dictionary->data, 
                        p->debug_dictionary->used, 
                        offs, low, high);
	}
	offset_heap(&p->fvalue, 1, offs, low, high);
	offset_heap(&p->ftrace, 1, offs, low, high);
	offset_heap(&p->group_leader, 1, offs, low, high);
        offset_heap(&p->seq_trace_token, 1, offs, low, high);
        offset_mqueue(p, offs, low, high);
        offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, low, high);
        offset_heap(p->heap, p->htop - p->heap, offs, low, high);
        offset_heap_ptr(p->rrma, p->nrr, offs, low, high);
        offset_nstack(p, offs, low, high);
	offset_off_heap(p, low, high, offs);
	if (p->old_heap)
            offset_heap(p->old_heap, p->old_htop - p->old_heap,
                        offs, low, high);
	if (p != cp ) {
            if (p->arity > 0)
                offset_heap_ptr(p->arg_reg, p->arity, offs, low, high);
	} else if (nobj > 0) {
	    offset_heap_ptr(objv, nobj, offs, low, high);
	}

#ifdef DEBUG
        if (p->nrr > 0) {
            int i;
            for (i = 0; i < p->nrr; i++) {
                if (p->rrsrc[i] != NULL) {
                    ASSERT(*(p->rrsrc[i]) == p->rrma[i]);
                }
            }
        }
#endif
    }
    ma_offset_off_heap(p, low, high, offs);
}

#if defined(HIPE)

#define MA_GENSWEEP_NSTACK(p,old_htop,n_htop,objv,nobj)	                \
	do {								\
		Eterm *tmp_old_htop = old_htop;				\
		Eterm *tmp_n_htop = n_htop;				\
		ma_gensweep_nstack((p),&tmp_old_htop,&tmp_n_htop,objv,nobj); \
		old_htop = tmp_old_htop;				\
		n_htop = tmp_n_htop;					\
	} while(0)

/*
 * offset_nstack() can ignore the descriptor-based traversal the other
 * nstack procedures use and simply call offset_heap_ptr() instead.
 * This relies on two facts:
 * 1. The only live non-Erlang terms on an nstack are return addresses,
 *    and they will be skipped thanks to the low/high range check.
 * 2. Dead values, even if mistaken for pointers into the low/high area,
 *    can be offset safely since they won't be dereferenced.
 *
 * XXX: WARNING: If HiPE starts storing other non-Erlang values on the
 * nstack, such as floats, then this will have to be changed.
 */

#else /* !HIPE */

#define ma_fullsweep_nstack(p,n_htop)		(n_htop)
#define MA_GENSWEEP_NSTACK(p,old_htop,n_htop,objv,nobj)   do{}while(0)
#define ma_offset_nstack(p,offs,low,high)                 do{}while(0)

#endif /* HIPE */


/* A general Cheney-style sweep of given memory area. Copies objects
 * between low and high.
 */

static Eterm *cheneyScan(Eterm *from, Eterm *to, Eterm *low, Eterm *high)
{
    Eterm *n_hp;
    Eterm *n_htop;
    Eterm *ptr;
    Eterm val;
    Eterm gval;

    n_hp     = from;
    n_htop   = to;

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
          case TAG_PRIMARY_BOXED: {
            ptr = boxed_val(gval);
            val = *ptr;
            if (MY_IS_MOVED(val))
                *n_hp++ = val;
            else if (ptr_within(ptr,low,high))
                MOVE_BOXED(ptr,val,n_htop,n_hp++);
            else
                ++n_hp;
            continue;
          }
          case TAG_PRIMARY_LIST: {
            ptr = list_val(gval);
            val = *ptr;
            if (is_non_value(val))
                *n_hp++ = ptr[1];
            else if (ptr_within(ptr,low,high))
                MOVE_CONS(ptr,val,n_htop,n_hp++);
            else
                ++n_hp;
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
    return n_htop;
}


static Eterm *rescueHeap(Eterm *start, Eterm *end, Eterm *top)
{
    Eterm *g_heap = global_heap;
    Eterm *g_hend = global_hend;
    Eterm *oh_start = global_old_heap;
    Eterm *oh_end = global_old_hend;
    Eterm *hp = start;

    while (hp != end) {
        Eterm gval = *hp;

        switch (primary_tag(gval)) {

        case TAG_PRIMARY_BOXED: {
	    Eterm *ptr = boxed_val(gval);
	    Eterm val = *ptr;
	    if (MY_IS_MOVED(val))
            {
		ASSERT(is_boxed(val));
 		*hp++ = val;
	    }
            else if( ptr_within(ptr, g_heap, g_hend) )
            {
		MOVE_BOXED(ptr,val,top,hp++);
            }
            else if( ptr_within(ptr, oh_start, oh_end) )
            {
		MOVE_BOXED(ptr,val,top,hp++);
            }
	    else {
                hp++;
	    }
	    continue;
        }

        case TAG_PRIMARY_LIST: {
	    Eterm *ptr = list_val(gval);
	    Eterm val = *ptr;
	    if (is_non_value(val))
                *hp++ = ptr[1];
            else if( ptr_within(ptr, g_heap, g_hend) )
            {
		MOVE_CONS(ptr,val,top,hp++);
            }
            else if( ptr_within(ptr, oh_start, oh_end) )
            {
		MOVE_CONS(ptr,val,top,hp++);
            }
	    else {
                hp++;
	    }
	    continue;
        }
            
        case TAG_PRIMARY_HEADER: {
            if (header_is_thing(gval))
                hp += (thing_arityval(gval)+1);
            else
                hp++;
            continue;
        }
            
        default:
	    hp++;
	    continue;
        }
    }

    return top;
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

static void ma_fullsweep_heap(Process *p, int new_sz, Eterm* objv, int nobj)
{
    Rootset *rootset = erts_alloc(ERTS_ALC_T_ROOTSET,
				  sizeof(Rootset)*erts_max_processes);
    Eterm *n_htop;		/* Top of new heap */
    Eterm *n_heap;		/* The new heap */
    Eterm *n_hp;
    Eterm *g_heap   = global_heap;
    Eterm *g_hend   = global_htop;
    Eterm *oh_start = global_old_heap;
    Eterm *oh_end   = global_old_htop;
    int n;

    /* Create new, empty heap */
    n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);
    n_htop = n_heap;
    global_gc_flags &= ~F_NEED_FULLSWEEP;
    VERBOSE(DEBUG_HYBRID_GC,("Fullsweep GC\n"));

    ma_gc_flags |= GC_INCLUDE_ALL;
    n = collect_roots(p, objv, nobj, rootset);
    while (n--)
    {
      n_htop = ma_fullsweep_nstack(rootset[n].p, n_htop);
      while (rootset[n].n--) {
        Eterm* g_ptr = rootset[n].v[rootset[n].n];
        Uint  g_sz = rootset[n].sz[rootset[n].n];
	while(g_sz--) {
	    Eterm gval = *g_ptr;
	    switch (primary_tag(gval)) {

	      case TAG_PRIMARY_BOXED: {
		Eterm *ptr = boxed_val(gval);
		Eterm val = *ptr;
		if (MY_IS_MOVED(val))
		{
		    ASSERT(is_boxed(val));
		    *g_ptr++ = val;
		}
                else if( ptr_within(ptr, g_heap, g_hend) )
                {
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
                }
                else if( ptr_within(ptr, oh_start, oh_end) )
                {
		    MOVE_BOXED(ptr,val,n_htop,g_ptr++);
                }
		else {
		    g_ptr++;
		}
		continue;
	      }

	      case TAG_PRIMARY_LIST: {
		Eterm *ptr = list_val(gval);
		Eterm val = *ptr;
		if (is_non_value(val))
		    *g_ptr++ = ptr[1];
                else if( ptr_within(ptr, g_heap, g_hend) )
                {
		    MOVE_CONS(ptr,val,n_htop,g_ptr++);
                }
                else if( ptr_within(ptr, oh_start, oh_end) )
                {
		    MOVE_CONS(ptr,val,n_htop,g_ptr++);
                }
		else {
                    ++g_ptr;
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
    }

    /*
     * Scan through the youngest generation of all processes to find
     * pointers to the global heap.
     */
    {
        Uint i;
        Uint n = erts_num_active_procs;

        for (i = 0; i < n; i++) {
            Process *cp = erts_active_procs[i];
            ErlHeapFragment* bp = MBUF(cp);

            n_htop = rescueHeap(cp->high_water,cp->htop,n_htop);

            while (bp) {
                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    n_htop = rescueHeap(bp->mem,ARITH_HEAP(cp),n_htop);
                } else {
                    n_htop = rescueHeap(bp->mem,bp->mem + bp->size,n_htop);
                }
                bp = bp->next;
            }
        }
    }

    /*
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
     */
    n_hp = n_heap;
    while (n_hp != n_htop) {
	Eterm gval = *n_hp;

	switch (primary_tag(gval)) {

	  case TAG_PRIMARY_BOXED: {
	    Eterm *ptr = boxed_val(gval);
	    Eterm val = *ptr;
	    if (MY_IS_MOVED(val))
	    {
		ASSERT(is_boxed(val));
		*n_hp++ = val;
	    }
            else if( ptr_within(ptr, g_heap, g_hend) )
            {
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
            }
            else if( ptr_within(ptr, oh_start, oh_end) )
            {
		MOVE_BOXED(ptr,val,n_htop,n_hp++);
            }
	    else {
                ++n_hp;
	    }
	    continue;
	  }

	  case TAG_PRIMARY_LIST: {
	    Eterm *ptr = list_val(gval);
	    Eterm val = *ptr;
	    if (is_non_value(val))
		*n_hp++ = ptr[1];
            else if( ptr_within(ptr, g_heap, g_hend) )
            {
		MOVE_CONS(ptr,val,n_htop,n_hp++);
            }
            else if( ptr_within(ptr, oh_start, oh_end) )
            {
		MOVE_CONS(ptr,val,n_htop,n_hp++);
            }
	    else {
                ++n_hp;
	    }
	    continue;
	  }

	  case TAG_PRIMARY_HEADER: {
            if (header_is_thing(gval)) {
		  n_hp += (thing_arityval(gval)+1);
            }
            else {
		  n_hp++;
            }
	      continue;
	  }

	  default: {
	    n_hp++;
	    continue;
	  }
	}
    }

    global_high_water = n_htop;

    restore_rootset(NULL,rootset);
    erts_free(ERTS_ALC_T_ROOTSET,(void*)rootset);

    if (erts_global_offheap.mso) {
	sweep_proc_bins(NULL,1);
    }

#ifndef HYBRID /* FIND ME! */
    if (erts_global_offheap.funs) {
	sweep_proc_funs(NULL,1);
    }
#endif

    if (erts_global_offheap.externals) {
        sweep_proc_externals(NULL,1);
    }

    if (global_old_heap != NULL) {
#ifdef DEBUG
        sys_memset(global_old_heap, DEBUG_BAD_BYTE,
                   (global_old_hend - global_old_heap)*sizeof(Eterm));
#endif
        ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
                       (void*)global_old_heap,
                       (global_old_hend - global_old_heap) * sizeof(Eterm));
        global_old_heap = global_old_htop = global_old_hend = NULL;
    }

#ifdef DEBUG
    sys_memset(global_heap, DEBUG_BAD_BYTE,
               (global_hend - global_heap)*sizeof(Eterm));
#endif    
    global_hend = n_heap + new_sz;
    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
                   (void*)global_heap,global_heap_sz * sizeof(Eterm));

    global_heap = n_heap;
    global_htop = n_htop;
    global_heap_sz = new_sz;
    global_gen_gcs = 0;
}

/*
 * Grow the new heap size to 'new_sz'.
 */
static void
ma_grow_new_heap(Process *p, Uint new_sz, Eterm* objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = global_htop - global_heap;
    Sint offs;

    ASSERT(global_heap_sz < new_sz);
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
                                           (void*)global_heap,
                                           sizeof(Eterm)*global_heap_sz,
                                           sizeof(Eterm)*new_sz);
    VERBOSE(DEBUG_HYBRID_GC,
            ("ma_grow_new_heap: FROM %d UPTO %d (used %d)\n",
             global_heap_sz, new_sz, heap_size));

    global_hend = new_heap + new_sz;

    if ((offs = new_heap - global_heap) != 0) {
        VERBOSE(DEBUG_HYBRID_GC,
                ("ma_grow_new_heap: HEAP HAD TO BE MOVED\n"));
        offset_heap(new_heap, heap_size, offs, global_heap, global_htop);
        global_high_water = new_heap + (global_high_water - global_heap);
        ma_offset_rootset(p, offs, global_heap, global_htop, objv, nobj);

        global_htop = new_heap + heap_size;
        global_heap = new_heap;
    }
    global_heap_sz = new_sz;
}

static void
ma_shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj)
{
    Eterm* new_heap;
    int heap_size = global_htop - global_heap;
    Sint offs;

    ASSERT(new_sz < global_heap_sz);
    ASSERT(new_sz >= global_heap_min_sz);

    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
                                           (void*)global_heap,
                                           sizeof(Eterm) * global_heap_sz,
                                           sizeof(Eterm) * new_sz);
    global_hend = new_heap + new_sz;

    VERBOSE(DEBUG_HYBRID_GC,
            ("ma_shrink_new_heap: FROM %d DOWNTO %d (used %d)\n",
             global_heap_sz, new_sz, heap_size));

    if ((offs = new_heap - global_heap) != 0) {

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * knows on some strange embedded systems...  Or when using purify.
         */
        VERBOSE(DEBUG_HYBRID_GC,
                ("ma_shrink_new_heap: HEAP HAD TO BE MOVED\n"));
        offset_heap(new_heap, heap_size, offs, global_heap, global_htop);
        global_high_water = new_heap + (global_high_water - global_heap);
        ma_offset_rootset(p, offs, global_heap, global_htop, objv, nobj);

        global_htop = new_heap + heap_size;
        global_heap = new_heap;
    }
    global_heap_sz = new_sz;
}

static Uint
ma_adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
    Uint reclaimed_now;

    size_after = (global_htop - global_heap);
    reclaimed_now = (size_before - size_after);

     /*
      * Resize the heap if needed.
      */

    need_after = size_after + need;
    if (global_heap_sz < need_after) {
        /* Too small - grow to match requested need */
        sz = erts_next_heap_size(need_after, 0);
        ma_grow_new_heap(p, sz, objv, nobj);
    } else if (3 * global_heap_sz < 4 * need_after){
        /* Need more than 75% of current, postpone to next GC.*/
        global_gc_flags |= F_HEAP_GROW;
    } else if (2 * need_after < global_heap_sz &&
               global_heap_sz > SH_DEFAULT_SIZE) {
        /* We need less than 25% of the current heap, shrink.*/
        /* XXX - This is how it was done in the old GC:
           wanted = 4 * need_after;
           I think this is better as fullsweep is used mainly on
           small memory systems, but I could be wrong... */
        wanted = 2 * need_after;
        if (wanted < global_heap_min_sz) {
            sz = global_heap_min_sz;
        } else {
            sz = erts_next_heap_size(wanted, 0);
        }
        if (sz < global_heap_sz) {
            ma_shrink_new_heap(p, sz, objv, nobj);
        }
    }

    return reclaimed_now;
}

/*
** Garbage collect the message area.
** Parameters:
** p: Pointer to the calling process structure.
** need: Number of (erlang) words needed on the heap.
** objv: Array of terms to add to rootset, that is to preserve.
** nobj: Number of objects in objv.
*/
int
erts_global_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    Uint reclaimed_now = 0;
    int size_before;
    int size_after;
    int need_after;
    Uint saved_status = 0;
    int wanted;
    int sz;
#ifdef __BENCHMARK__
    uint this_was_major = 0;
#endif

    BM_STOP_TIMER(system);

    VERBOSE(DEBUG_HYBRID_GC,
            ("MessArea GC START  Caused by: %T  Current size: %d\n",
             p->id,global_heap_sz));
    VERBOSE(DEBUG_HYBRID_GC,
            ("Young generation: 0x%08x - 0x%08x - 0x%08x (%d used, %d allocated)\n",
             global_heap,global_htop,global_hend,
             global_htop - global_heap,global_hend - global_heap));
    VERBOSE(DEBUG_HYBRID_GC,
            ("Old generation: 0x%08x - 0x%08x - 0x%08x (%d used, %d allocated)\n",
             global_old_heap,global_old_htop,global_old_hend,
             global_old_htop - global_old_heap,
             global_old_hend - global_old_heap));

#ifdef BM_HEAP_SIZES
    {
        unsigned long total_used_heap = (global_htop - global_heap) +
          (global_old_htop - global_old_heap);
        unsigned long total_allocated_heap = global_heap_sz +
          (global_old_hend - global_old_heap);

        if (total_used_heap > max_used_global_heap)
            max_used_global_heap = total_used_heap;
        if (total_allocated_heap > max_allocated_global_heap)
            max_allocated_global_heap = total_allocated_heap;
    }
#endif /* BM_HEAP_SIZES */

    BM_RESET_TIMER(gc);
    BM_START_TIMER(gc);

    /*
    if (global_saved_htop != NULL) {
	global_htop = global_saved_htop;
	global_saved_htop = NULL;
    }
    */

#ifdef DEBUG
#define OverRunCheck()                                          \
    if (global_hend < global_htop) {                            \
        erl_exit(1, "%T: Overrun message area at line %d\n",    \
                 p->id,__LINE__);                               \
    }

#ifdef DEBUG_PARANOID
    {
        Uint i;
	for (i = 0; i < erts_num_active_procs; i++)
            CHECK_HEAP(erts_active_procs[i]);
    }
#endif
#else
#define OverRunCheck()
#endif

    if (p) {
        if (IS_TRACED_FL(p, F_TRACE_GC)) {
            trace_gc(p, am_gc_start);
        }
        saved_status = p->status;
        p->status = P_GARBING;
        CHECK(p);
        OverRunCheck();
    }

    if (global_gen_gcs >= global_max_gen_gcs) {
        global_gc_flags |= F_NEED_FULLSWEEP;
    }

    garbage_cols++;

    /* Size of heap before first GC */
    size_before = global_htop - global_heap;

    /* To prevent shrinking of the message area */
    global_heap_min_sz = global_heap_sz;

    /*
     * Generational GC from here on. We need an old heap.
     */
    if (global_old_heap == NULL && global_high_water != global_heap &&
        (global_gc_flags & F_NEED_FULLSWEEP) == 0) {
        Eterm* n_old;
        size_t new_sz = erts_next_heap_size(global_high_water -
                                            global_heap, 1);

        /* Create new, empty old_heap */
        n_old = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
                                          sizeof(Eterm)*new_sz);

        global_old_hend = n_old + new_sz;
        global_old_heap = global_old_htop = n_old;
        VERBOSE(DEBUG_HYBRID_GC,("Created an old_heap\n"));
    }

    /*
     * Make sure functions shared between local and global collection
     * knows this is a global collection.
     */
    ma_gc_flags |= GC_GLOBAL;

    /*
     * Try a generational GC if the old heap is big enough.
     */
    if ((global_gc_flags & F_NEED_FULLSWEEP) == 0 &&
        global_high_water - global_heap <= global_old_hend - global_old_htop)
    {

        /*
         * There is space enough in old_heap for everything
         * below the high water mark.  Do a generational GC.
         */
        ma_gen_gc(p, erts_next_heap_size(global_heap_sz, 0), objv, nobj);

        global_gen_gcs++;
        size_after = global_htop - global_heap;
        need_after = size_after + need;
        reclaimed_now += (size_before - size_after);

        /*
         * Excessively large heaps should be shrunk, but
         * don't even bother on reasonable small heaps.
         *
         * The reason for this is that after tenuring, we often
         * use a really small portion of new heap, therefore, unless
         * the heap size is substantial, we don't want to shrink.
         */

        /*
        if ((global_heap_sz > 300) && (4 * need_after < global_heap_sz) &&
            ((global_heap_sz > 8000)
             || (global_heap_sz > (global_old_hend - global_old_heap))
             )) {
        */
        if ((global_heap_sz > 300) && (4 * need_after < global_heap_sz)) {
            wanted = 3 * need_after;
            if (wanted < global_heap_min_sz) {
                wanted = global_heap_min_sz;
            } else {
                wanted = erts_next_heap_size(wanted, 0);
            }
            if (wanted < global_heap_sz) {
	        ma_shrink_new_heap(p, wanted, objv, nobj);
            }
            ASSERT(global_heap_sz == erts_next_heap_size(global_heap_sz, 0));
            goto ma_done;
        }

        /*
         * The heap size turned out to be just right. We are done.
         */

        if (global_heap_sz >= need_after) {
            ASSERT(global_heap_sz == erts_next_heap_size(global_heap_sz, 0));
            goto ma_done;
        }
        VERBOSE(DEBUG_HYBRID_GC,
                ("Did a gen_gc, still not enough room!\n"));
    }

    /*
     * The previous generational GC did not leave enough free heap space.
     * We must do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    sz = global_heap_sz + (global_old_htop - global_old_heap);
    sz = erts_next_heap_size(sz, 0);

    /*
     * Should we grow although we don't actually need to?
     */

    if (sz == global_heap_sz && global_gc_flags & F_HEAP_GROW) {
        sz = erts_next_heap_size(global_heap_sz, 1);
    }

    global_gc_flags &= ~F_HEAP_GROW;
    ma_fullsweep_heap(p, sz, objv, nobj);
    CHECK(p);
    reclaimed_now += ma_adjust_after_fullsweep(p, size_before, need, objv, nobj);

#ifdef __BENCHMARK__
    this_was_major = 1;
#endif

 ma_done:

    erts_smp_spin_lock(&info_lck);
    garbage_cols++;
    reclaimed += reclaimed_now;
    erts_smp_spin_unlock(&info_lck);

    if (p) {
        CHECK(p);
        OverRunCheck();
        ACTIVATE(p);
        p->status = saved_status;
        if (IS_TRACED_FL(p, F_TRACE_GC)) {
            trace_gc(p, am_gc_end);
        }
    }
    ma_gc_flags &= ~GC_GLOBAL;

    BM_STOP_TIMER(gc);

#ifdef __BENCHMARK__

    if (this_was_major == 1)
    {
        BM_COUNT(major_global_gc);
#ifdef BM_TIMERS
        major_global_gc_time += gc_time;
        if (gc_time > max_global_major_time)
            max_global_major_time = gc_time;
#endif
    }
    else
    {
        BM_COUNT(minor_global_gc);
#ifdef BM_TIMERS
        minor_global_gc_time += gc_time;
        if (gc_time > max_global_minor_time)
            max_global_minor_time = gc_time;
#endif
    }

#ifdef BM_TIMERS
    pause_times[(((gc_time * 1000) < MAX_PAUSE_TIME) ?
                 (int)(gc_time * 1000) :
                 MAX_PAUSE_TIME - 1)]++;
#endif
#endif /* __BENCHMARK__ */

#ifdef BM_HEAP_SIZES
    {
        unsigned long total_used_heap = (global_htop - global_heap) +
          (global_old_htop - global_old_heap);
        unsigned long total_allocated_heap = global_heap_sz +
          (global_old_hend - global_old_heap);

        if (total_used_heap > max_used_global_heap)
            max_used_global_heap = total_used_heap;
        if (total_allocated_heap > max_allocated_global_heap)
            max_allocated_global_heap = total_allocated_heap;
    }
#endif /* BM_HEAP_SIZES */

    VERBOSE(DEBUG_HYBRID_GC,("MessArea GC END\n"));

#ifdef DEBUG_PARANOID
    {
        Uint i;
	for (i = 0; i < erts_num_active_procs; i++)
            CHECK_HEAP(erts_active_procs[i]);
    }
#endif

    BM_START_TIMER(system);
    return ((int) (global_htop - global_heap) / 10);
#undef OverRunCheck
}

/*
 * This function sweeps both the remainder of the new heap
 * as well as the remainder of the old heap after the first pass
 * of the generational collector ma_gen_gc().
 */
static Eterm *ma_gen_cheney(Eterm *from, Eterm **to, Eterm *low_water,
                            Eterm *high_water, Eterm *surface, Eterm *old_htop,
                            Eterm *objv, int nobj)
{
    Eterm *n_hp;
    Eterm *n_htop;
    Eterm gval;

    n_hp     = from;
    n_htop   = *to;

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
          case TAG_PRIMARY_LIST: {
            Eterm *ptr = list_val(gval);
            Eterm val = *ptr;
            if (is_non_value(val)) {
                *n_hp++ = ptr[1];
            }
            else if (ptr_within(ptr,high_water,surface)) {
                MOVE_CONS(ptr,val,n_htop,n_hp++);
            }
            else if (ptr_within(ptr,low_water,high_water)) {
                MOVE_CONS(ptr,val,old_htop,n_hp++);
            }
            else
                ++n_hp;
            continue;
          }
          case TAG_PRIMARY_BOXED: {
            Eterm *ptr = boxed_val(gval);
            Eterm val = *ptr;
            if (MY_IS_MOVED(val)) {
                *n_hp++ = val;
            }
            else if (ptr_within(ptr,high_water,surface)) {
                INC_MARK_FORWARD(ptr);
                MOVE_BOXED(ptr,val,n_htop,n_hp++);
            }
            else if (ptr_within(ptr,low_water,high_water)) {
                MOVE_BOXED(ptr,val,old_htop,n_hp++);
            }
            else
                ++n_hp;
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

static Eterm *rescueHeapGen(Process *p, Eterm *start, Eterm *end,
                            Eterm *top, Eterm **old_top, Eterm *objv, int nobj)
{
    Eterm *low_water  = global_heap;
    Eterm *high_water = global_high_water;
    Eterm *oh_top     = *old_top;
    Eterm *surface    = global_htop;
    Eterm *g_ptr      = start;

    while (g_ptr != end) {
        Eterm gval = *g_ptr;

        switch (primary_tag(gval))
            {
            case TAG_PRIMARY_LIST:
            {
	        Eterm *ptr = list_val(gval);
                if (ptr_within(ptr,low_water,high_water))
                {
                    Eterm val = *ptr;
                    if (is_non_value(val))
                        *g_ptr++ = ptr[1];
                    else {
                        MOVE_CONS(ptr,val,oh_top,g_ptr++);
                    }
                }
                else if (ptr_within(ptr,high_water,surface))
                {
                    Eterm val = *ptr;
                    if (is_non_value(val))
                        *g_ptr++ = ptr[1];
                    else
                        MOVE_CONS(ptr,val,top,g_ptr++);
                }
                else
                    ++g_ptr;
                continue;
            }

            case TAG_PRIMARY_BOXED:
            {
	        Eterm *ptr = boxed_val(gval);
                if (ptr_within(ptr,low_water,high_water))
                {
                    Eterm val = *ptr;
                    if (MY_IS_MOVED(val))
                        *g_ptr++ = val;
                    else {
                        MOVE_BOXED(ptr,val,oh_top,g_ptr++);
                    }
                }
                else if (ptr_within(ptr,high_water,surface))
                {
                    Eterm val = *ptr;
                    if (MY_IS_MOVED(val))
                        *g_ptr++ = val;
                    else {
                        INC_MARK_FORWARD(ptr);
                        MOVE_BOXED(ptr,val,top,g_ptr++);
                    }
                }
                else
                    ++g_ptr;
                continue;
            }

            case TAG_PRIMARY_HEADER:
            {
                if (header_is_thing(gval))
                    g_ptr += (thing_arityval(gval) + 1);
                else
                    g_ptr++;
                continue;
            }

            default:
                g_ptr++;
                continue;
            }
    }
    *old_top = oh_top;
    return top;
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
ma_gen_gc(Process *p, int new_sz, Eterm* objv, int nobj)
{
    Rootset *rootset = erts_alloc(ERTS_ALC_T_ROOTSET,
				  sizeof(Rootset)*erts_max_processes);
    Eterm *n_hstart;
    Eterm *n_htop;
    int n;
    Eterm *low_water  = global_heap;
    Eterm *high_water = global_high_water;
    Eterm *old_htop   = global_old_htop;

    /* We accually only want this to be up to where the message starts,
     * not all the way up to htop. But there shouldn't be any pointers to
     * the message we are copying reachable from the rootset. As long as
     * the first entry in the dst-stack is a local c variable, this will hold.
     */
    Eterm *surface    = global_htop;
    Eterm *tmp;

    VERBOSE(DEBUG_HYBRID_GC,("Generational GC\n"));

    n_hstart = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);
    n_htop = n_hstart;
    n = collect_roots(p, objv, nobj, rootset);
    while (n--) {
      MA_GENSWEEP_NSTACK(rootset[n].p, old_htop, n_htop, objv, nobj);

      while (rootset[n].n--) {
        Eterm *g_ptr = rootset[n].v[rootset[n].n];
        Uint g_sz = rootset[n].sz[rootset[n].n];

        while (g_sz--) {
            Eterm gval = *g_ptr;
            switch (primary_tag(gval))
            {
            case TAG_PRIMARY_LIST:
              {
	        Eterm *ptr = list_val(gval);
		if (ptr_within(ptr,low_water,high_water))
                {
                    Eterm val = *ptr;
                    if (is_non_value(val))
                        *g_ptr++ = ptr[1];
                    else {
                        MOVE_CONS(ptr,val,old_htop,g_ptr++);
                    }
                }
                else if (ptr_within(ptr,high_water,surface))
                {
                    Eterm val = *ptr;
                    if (is_non_value(val))
                        *g_ptr++ = ptr[1];
                    else
                        MOVE_CONS(ptr,val,n_htop,g_ptr++);
                }
                else
                    ++g_ptr;
                continue;
              }

            case TAG_PRIMARY_BOXED:
              {
	        Eterm *ptr = boxed_val(gval);
                if (ptr_within(ptr,low_water,high_water))
                {
                    Eterm val = *ptr;
                    if (MY_IS_MOVED(val))
                        *g_ptr++ = val;
                    else {
                        MOVE_BOXED(ptr,val,old_htop,g_ptr++);
                    }
                }
                else if (ptr_within(ptr,high_water,surface))
                {
                    Eterm val = *ptr;
                    if (MY_IS_MOVED(val))
                        *g_ptr++ = val;
                    else {
                        INC_MARK_FORWARD(ptr);
                        MOVE_BOXED(ptr,val,n_htop,g_ptr++);
                    }
                }
                else
                    ++g_ptr;
                continue;
              }

            default:
              {
                g_ptr++;
                continue;
              }
            }
         }
      }
    }

    /* Scan through the young generation of all processes to find
     * pointers to the global heap.
     */
    {
        Uint i;
        Uint n = erts_num_active_procs;

        for (i = 0; i < n; i++) {
            Process *cp = erts_active_procs[i];
            ErlHeapFragment* bp = MBUF(cp);

            if (cp->high_water != cp->htop) {
                n_htop = rescueHeapGen(cp,cp->high_water,cp->htop,n_htop,&old_htop,objv,nobj);
            }

            while (bp) {
                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    n_htop = rescueHeapGen(cp,bp->mem,ARITH_HEAP(cp),n_htop,&old_htop,objv,nobj);
                } else {
                    n_htop = rescueHeapGen(cp,bp->mem,bp->mem + bp->size,n_htop,&old_htop,objv,nobj);
                }
                bp = bp->next;
            }
        }
    }

    /*
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */

    tmp = n_htop;
    old_htop = ma_gen_cheney(n_hstart, &tmp, low_water, high_water, surface,
                             old_htop, objv, nobj);
    n_htop = tmp;

    /*
     * And also if we have been tenuring, references on the second generation
     * may point to the old (soon to be deleted) heap.
     */

    if (global_old_htop < old_htop) {
        old_htop = cheneyScan(global_old_htop, old_htop, low_water, high_water);
    }
    global_old_htop = old_htop;

    /* FIND ME! global_high_water = (global_heap != global_high_water) ? n_hstart : n_htop; */
    global_high_water = n_htop;

    if (erts_global_offheap.mso) {
        sweep_proc_bins(NULL,0);
    }

#ifndef HYBRID /* FIND ME! */
    if (erts_global_offheap.funs) {
        sweep_proc_funs(NULL,0);
    }
#endif

    if (erts_global_offheap.externals) {
        sweep_proc_externals(NULL,0);
    }

#ifdef DEBUG
    sys_memset(global_heap, DEBUG_BAD_BYTE, global_heap_sz*sizeof(Eterm));
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
                   (void*)global_heap,global_heap_sz * sizeof(Eterm));
    restore_rootset(NULL,rootset);
    erts_free(ERTS_ALC_T_ROOTSET,(void*)rootset);
    ma_gc_flags &= ~GC_INCLUDE_ALL;

    global_heap = n_hstart;
    global_hend = n_hstart + new_sz;
    global_htop = n_htop;
    global_heap_sz = new_sz;
}
#endif /* HYBRID and no INCREMENTAL */
