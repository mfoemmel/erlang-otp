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
#if HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif0.h"
#include "hipe_stack.h"
#endif

#ifndef HEAP_FRAG_ELIM_TEST
static void remove_message_buffers(Process* p);

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
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR-1))->num_free+1; break;	\
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
    Eterm* v[9];		/* Pointers to vectors with terms to GC
				 * (e.g. the stack).
				 */
    Uint sz[9];			/* Size of each vector. */
    Eterm* v_msg;		/* Pointer to messages to GC. */
    Eterm def_msg[32];		/* Default storage for messages (to avoid malloc). */
#ifdef SHARED_HEAP
    Uint n;
#if defined(HIPE)
    Process *p;			/* For scanning the nstack. */
#endif
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
#ifndef SHARED_HEAP
static void sweep_proc_funs(Process *p, int fullsweep);
#endif
static void sweep_proc_externals(Process *p, int fullsweep);

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

/* efficient range check */
#define in_area(ptr,start,nbytes) ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

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
#ifndef SHARED_HEAP
		      ErlFunThing* funp = (ErlFunThing *) hp;
		      Eterm** uptr = (Eterm **) &funp->next;

		      if (*uptr && ptr_within((Eterm *)funp->next, low, high)) {
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

		      if (*uptr && ptr_within((Eterm *)etp->next, low, high)) {
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
        Eterm mesg = ERL_MESSAGE_TERM(mp);
	switch (primary_tag(mesg)) {
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    if (ptr_within(ptr_val(mesg), low, high)) {
		ERL_MESSAGE_TERM(mp) = offset_ptr(mesg, offs);
	    }
            break;
        }
	mesg = ERL_MESSAGE_TOKEN(mp);
	if (is_boxed(mesg) && ptr_within(ptr_val(mesg), low, high)) {
	    ERL_MESSAGE_TOKEN(mp) = offset_ptr(mesg, offs);
        }
        ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		is_atom(ERL_MESSAGE_TOKEN(mp))));
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

#if defined(__i386__)
#include "hipe_x86_asm.h"	/* for X86_NR_ARG_REGS */

static Eterm *fullsweep_nstack(Process *p, Eterm *n_htop)
{
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    struct sdesc sdesc0;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    char *const_start;
    unsigned long const_size;
    unsigned int nstkarity;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstgraylim;
    if( nsp_end ) {
	/* remove gray/white boundary trap */
	for(;;) {
	    --nsp_end;
	    if( nsp_end[0] == (unsigned long)nbif_stack_trap_ra ) {
		nsp_end[0] = (unsigned long)p->hipe.ngra;
		break;
	    }
	}
    }
    nsp_end = p->hipe.nstend;

    nstkarity = p->hipe.narity - X86_NR_ARG_REGS;
    if( (int)nstkarity < 0 )
	nstkarity = 0;
    sdesc0.summary = (0 << 9) | (0 << 8) | nstkarity;
    sdesc0.livebits[0] = ~1; /* all but RA, which is first since no locals */
    sdesc = &sdesc0;

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;

    for(;;) {
	if( nsp >= nsp_end ) {
	    if( nsp == nsp_end ) {
		if( nsp ) {
		    /* see the HIGH_WATER update in fullsweep_heap() */
		    p->hipe.nstblacklim = p->hipe.nstend;
		    hipe_update_stack_trap(p, &sdesc0);
		}
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm gval = nsp[i];
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			nsp[i] = val;
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,&nsp[i]);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			nsp[i] = ptr[1];
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,&nsp[i]);
		    }
		}
	    }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	ra = nsp[sdesc_fsize(sdesc)];
	sdesc = hipe_find_sdesc(ra);
	nsp += sdesc_size;
    }
    abort();
}

static void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop)
{
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    struct sdesc sdesc0;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    Eterm *oh_start, *oh_end;
    Eterm *low_water, *high_water;
    Eterm *n_htop, *old_htop;
    char *const_start;
    unsigned long const_size;
    unsigned int nstkarity;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstgraylim;
    if( nsp_end ) {
	/* if gray limit passed black limit, reset black limit */
	if( nsp_end > p->hipe.nstblacklim )
	    p->hipe.nstblacklim = nsp_end;
	/* remove gray/white boundary trap */
	for(;;) {
	    --nsp_end;
	    if( nsp_end[0] == (unsigned long)nbif_stack_trap_ra ) {
		nsp_end[0] = (unsigned long)p->hipe.ngra;
		break;
	    }
	}
	nsp_end = p->hipe.nstblacklim;
    } else
	nsp_end = p->hipe.nstend;

    nstkarity = p->hipe.narity - X86_NR_ARG_REGS;
    if( (int)nstkarity < 0 )
	nstkarity = 0;
    sdesc0.summary = (0 << 9) | (0 << 8) | nstkarity;
    sdesc0.livebits[0] = ~1; /* all but RA, which is first since no locals */
    sdesc = &sdesc0;

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;
    oh_start = OLD_HEAP(p);
    oh_end = OLD_HEND(p);
    low_water = HEAP_START(p);
    high_water = HIGH_WATER(p);
    old_htop = *ptr_old_htop;
    n_htop = *ptr_n_htop;

    for(;;) {
	if( nsp >= nsp_end ) {
	    if( nsp == nsp_end ) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		if( nsp ) {
		    /* see the HIGH_WATER update in gen_gc() */
		    if( HEAP_START(p) != HIGH_WATER(p) ) {
			p->hipe.nstblacklim =
			    p->hipe.nstgraylim
			    ? p->hipe.nstgraylim
			    : p->hipe.nstend;
		    } else {
			/* blacklim = graylim ? blacklim : end */
			if( !p->hipe.nstgraylim )
			    p->hipe.nstblacklim = p->hipe.nstend;
		    }
		    hipe_update_stack_trap(p, &sdesc0);
		}
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm gval = nsp[i];
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
		    } else if( MY_IS_MOVED(val) ) {
			nsp[i] = val;
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_BOXED(ptr,val,old_htop,&nsp[i]);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,&nsp[i]);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
		    } else if( is_non_value(val) ) {
			nsp[i] = ptr[1];
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_CONS(ptr,val,old_htop,&nsp[i]);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,&nsp[i]);
		    }
		}
	    }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	nsp += sdesc_size;
	ra = nsp[-(1+sdesc_arity(sdesc))];
	sdesc = hipe_find_sdesc(ra);
    }
    abort();
}

#endif	/* __i386__ */

#if defined(__sparc__)

extern void hipe_update_stack_trap(Process*, const struct sdesc*);
extern void nbif_stack_trap_ra(void);

static Eterm *fullsweep_nstack(Process *p, Eterm *n_htop)
{
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc0;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    char *const_start;
    unsigned long const_size;

    ra = (unsigned long)p->hipe.nra;
    if (!ra) return n_htop;

    nsp = p->hipe.nsp-1;
    nsp_end =  p->hipe.nstgraylim;

    if( nsp_end ) {
      /* Check if the trap frame is on the stack or in ra */
      if( ra == (unsigned long)nbif_stack_trap_ra ) {
	p->hipe.nra = p->hipe.ngra;
	ra = (unsigned long)p->hipe.nra;
      } else { /* The trap is on the stack, remove it. */
	/* remove gray/white boundary trap */
	for(;;) {
	  ++nsp_end;
	  /* Sanity check */
	  if(nsp_end > nsp) abort();
	  if( nsp_end[0] == (unsigned long)nbif_stack_trap_ra ) {
	    nsp_end[0] = (unsigned long)p->hipe.ngra;
	    break;
	  }
	}
      }
    }
    nsp_end = p->hipe.nstack;

    sdesc = hipe_find_sdesc(ra);
    sdesc0 = sdesc;

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;

    for(;;) {
	if( nsp <= nsp_end ) {
	    if( nsp == nsp_end ) {
		if( nsp ) {
		    /* see the HIGH_WATER update in fullsweep_heap() */
		    p->hipe.nstblacklim = p->hipe.nstack;
		    hipe_update_stack_trap(p, sdesc0);
		}
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = sdesc_fsize(sdesc) + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm gval = nsp[-i];
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			nsp[-i] = val;
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,&nsp[-i]);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			nsp[-i] = ptr[1];
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,&nsp[-i]);
		    }
		}
	    }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	ra = nsp[1-sdesc_fsize(sdesc)];
	sdesc = hipe_find_sdesc(ra);
	nsp -= sdesc_size;
    }
    abort();
}

static void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop)
{
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc0;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    Eterm *oh_start, *oh_end;
    Eterm *low_water, *high_water;
    Eterm *n_htop, *old_htop;
    char *const_start;
    unsigned long const_size;

    nsp = p->hipe.nsp-1;
    nsp_end = p->hipe.nstgraylim;
    ra = (unsigned long)p->hipe.nra;
    if (!ra) return;


    if( nsp_end ) {
      /* if gray limit passed black limit, reset black limit */
      if( nsp_end < p->hipe.nstblacklim )
	p->hipe.nstblacklim = nsp_end;

      /* Check if the trap frame is on the stack or in ra */
      if( ra == (unsigned long)nbif_stack_trap_ra ) {
	p->hipe.nra = p->hipe.ngra;
	ra = (unsigned long)p->hipe.nra;
      } else {
	/* remove gray/white boundary trap */
	for(;;) {
	  ++nsp_end;
	  /* Sanity check */
	  if(nsp_end > nsp) abort();
	  if( nsp_end[0] == (unsigned long)nbif_stack_trap_ra ) {
	    nsp_end[0] = (unsigned long)p->hipe.ngra;
	    break;
	  }
	}
      }
      nsp_end = p->hipe.nstblacklim;
    } else
      nsp_end = p->hipe.nstack;

    sdesc = hipe_find_sdesc(ra);
    sdesc0 = sdesc;

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;
    oh_start = OLD_HEAP(p);
    oh_end = OLD_HEND(p);
    low_water = HEAP_START(p);
    high_water = HIGH_WATER(p);
    old_htop = *ptr_old_htop;
    n_htop = *ptr_n_htop;

    for(;;) {
	if( nsp <= nsp_end ) {
	    if( nsp == nsp_end ) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		if( nsp ) {
		    /* see the HIGH_WATER update in gen_gc() */
		    if( HEAP_START(p) != HIGH_WATER(p) ) {
			p->hipe.nstblacklim =
			    p->hipe.nstgraylim
			    ? p->hipe.nstgraylim
			    : p->hipe.nstack;
		    } else {
			/* blacklim = graylim ? blacklim : end */
			if( !p->hipe.nstgraylim )
			    p->hipe.nstblacklim = p->hipe.nstack;
		    }
		    hipe_update_stack_trap(p, sdesc0);
		}
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);

	    break;
	}
	sdesc_size = sdesc_fsize(sdesc) + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm gval = nsp[-i];
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
		    } else if( MY_IS_MOVED(val) ) {
			nsp[-i] = val;
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_BOXED(ptr,val,old_htop,&nsp[-i]);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,&nsp[-i]);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
		    } else if( is_non_value(val) ) {
			nsp[-i] = ptr[1];
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_CONS(ptr,val,old_htop,&nsp[-i]);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,&nsp[-i]);
		    }
		}
	    }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	nsp -= sdesc_size;
	ra = nsp[1+sdesc_arity(sdesc)];
	sdesc = hipe_find_sdesc(ra);
    }
    abort();
}

#endif	/* __sparc__ */

#define GENSWEEP_NSTACK(p,old_htop,n_htop)				\
	do {								\
		Eterm *tmp_old_htop = old_htop;				\
		Eterm *tmp_n_htop = n_htop;				\
		gensweep_nstack((p), &tmp_old_htop, &tmp_n_htop);	\
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
#define GENSWEEP_NSTACK(p,old_htop,n_htop)	do{}while(0)
#define offset_nstack(p,offs,low,high)		do{}while(0)

#endif /* HIPE */

static int
setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset)
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
        *v_ptr++ = ERL_MESSAGE_TERM(mp);
        ASSERT((is_nil(ERL_MESSAGE_TOKEN(mp)) ||
		is_tuple(ERL_MESSAGE_TOKEN(mp)) ||
		is_atom(ERL_MESSAGE_TOKEN(mp))));
        *v_ptr++ = ERL_MESSAGE_TOKEN(mp);
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
     * (fvalue contains EXIT reason.)
     */
    rootset->v[n]  = &p->fvalue;
    rootset->sz[n] = 1;
    n++;

#if HIPE && defined(SHARED_HEAP)
    rootset->p = p;
#endif

    ASSERT(n <= ALENGTH(rootset->v));
    return n;
}

#ifdef SHARED_HEAP
static Uint
collect_roots(Process* current, Eterm *objv, int nobj, Rootset rootset[])
{
    Process* p;
    Uint i;
    Uint n = erts_num_active_procs;

    for (i = 0; i < n; i++) {
        p = erts_active_procs[i];
	if (p->active) {
	    if (p == current) {
		rootset[i].n = setup_rootset(p, objv, nobj, &rootset[i]);
	    } else {
		rootset[i].n = setup_rootset(p, p->arg_reg, p->arity, &rootset[i]);
	    }
	}
    }
    return n;
}
#endif

static ERTS_INLINE
void restore_this_rootset(Process *p, Rootset *rootset)
{
    Eterm* v_ptr;
    ErlMessage* mp;

    /*
     * Restore all message pointers.
     */
    mp = p->msg.first;
    v_ptr = rootset->v_msg;
    while (mp != NULL) {
	ERL_MESSAGE_TERM(mp) = *v_ptr++;
	ASSERT((is_nil(*v_ptr) || is_tuple(*v_ptr) || is_atom(*v_ptr)));
	ERL_MESSAGE_TOKEN(mp) = *v_ptr++;
	mp = mp->next;
    }
    
    if (rootset->v_msg != rootset->def_msg) {
        erts_free(ERTS_ALC_T_MSG_ROOTS, rootset->v_msg);
    }
}

static void
restore_rootset(Process *p, Rootset *rootset)
{
#ifdef SHARED_HEAP
    Uint i;
    Uint n = erts_num_active_procs;

    for (i = 0; i < n; i++) {
        p = erts_active_procs[i];
	if (p->active) {
	    restore_this_rootset(p, &rootset[i]);
	}
    }
#else
    restore_this_rootset(p, rootset);
#endif
}

/*
 * Remove all message buffers.
 */
static void
remove_message_buffers(Process* p)
{
    ErlHeapFragment* bp = MBUF(p);

    MBUF(p) = NULL;
#ifndef SHARED_HEAP
    p->halloc_mbuf = NULL;
#endif
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
#ifdef SHARED_HEAP
    Rootset *rootset = erts_alloc(ERTS_ALC_T_ROOTSET,
				  sizeof(Rootset)*erts_max_processes);
#else
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
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
    n_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);

    n_hstart = n_htop = n_heap;
    FLAGS(p) &= ~F_NEED_FULLSWEEP;

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Fullsweep GC proc: %d\n", print_pid(p));
#endif

#ifdef SHARED_HEAP
    n = collect_roots(p, objv, nobj, rootset);

    while (n--) {
	n_htop = fullsweep_nstack(rootset[n].p, n_htop);
      while (rootset[n].n--) {
        Eterm* g_ptr = rootset[n].v[rootset[n].n];
        Uint  g_sz = rootset[n].sz[rootset[n].n];
#else
    n = setup_rootset(p, objv, nobj, &rootset);

    n_htop = fullsweep_nstack(p, n_htop);
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
		if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *g_ptr++ = val;
#if HIPE
		} else if( in_area(ptr, const_start, const_size) ) {
		    ++g_ptr;
#endif
		} else {
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
#ifdef SHARED_HEAP
    }
#endif


    /*
     * Now all references on the stack point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is copied.
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
#if HIPE
	    } else if( in_area(ptr, const_start, const_size) ) {
		++n_hp;
#endif
	    } else {
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
#if HIPE
	    } else if( in_area(ptr, const_start, const_size) ) {
		++n_hp;
#endif
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

#ifdef SHARED_HEAP
    restore_rootset(p, rootset);
    erts_free(ERTS_ALC_T_ROOTSET, (void *) rootset);
#else
    restore_rootset(p, &rootset);
#endif
    if (MSO(p).mso) {
	sweep_proc_bins(p, 1);
    }
#ifndef SHARED_HEAP
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
        sys_memset(OLD_HEAP(p), 0xff,
                   (OLD_HEND(p) - OLD_HEAP(p)) * sizeof(Eterm));
#endif
        ERTS_HEAP_FREE(ERTS_ALC_T_OLD_HEAP,
		       OLD_HEAP(p),
		       (OLD_HEND(p)-OLD_HEAP(p))*sizeof(Eterm));
        OLD_HEAP(p) = OLD_HTOP(p) = OLD_HEND(p) = NULL;
    }

#ifndef SHARED_HEAP
    /* Move the stack, the beam stack is "in the heap" */
    n = HEAP_END(p) - p->stop;
    sys_memcpy(n_heap + new_sz - n, p->stop, n * sizeof(Eterm));
#endif
#ifdef DEBUG
    sys_memset(HEAP_START(p), 0xff,
               (HEAP_END(p) - HEAP_START(p)) * sizeof(Eterm));
#endif
	HEAP_END(p) = n_heap + new_sz;
#ifndef SHARED_HEAP
    p->stop = HEAP_END(p) - n;
#endif

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   HEAP_SIZE(p)*sizeof(Eterm));

    HEAP_START(p) = n_heap;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
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

}

static void
offset_off_heap(Process* p, Eterm* low, Eterm* high, int offs)
{
    if (MSO(p).mso && ptr_within((Eterm *)MSO(p).mso, low, high)) {
        Eterm** uptr = (Eterm**) &MSO(p).mso;
        *uptr += offs;
    }

#ifndef SHARED_HEAP
    if (MSO(p).funs && ptr_within((Eterm *)MSO(p).funs, low, high)) {
        Eterm** uptr = (Eterm**) &MSO(p).funs;
        *uptr += offs;
    }
#endif

    if (MSO(p).externals && ptr_within((Eterm *)MSO(p).externals, low, high)) {
        Eterm** uptr = (Eterm**) &MSO(p).externals;
        *uptr += offs;
    }

}

static void
offset_rootset(Process *p, int offs, 
	       Eterm* low, Eterm* high, 
	       Eterm* objv, int nobj)
{
#ifdef SHARED_HEAP
    Uint i;
    Uint n = erts_num_active_procs;
    Process* current = p;

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
	offset_heap(&p->group_leader, 1, offs, low, high);
        offset_heap(&p->seq_trace_token, 1, offs, low, high);
        offset_mqueue(p, offs, low, high);
        offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, low, high);
        offset_nstack(p, offs, low, high);
	offset_off_heap(p, low, high, offs);
	if (p != current) {
	    offset_heap(p->arg_reg, p->arity, offs, low, high);
	} else if (nobj > 0) {
	    offset_heap(objv, nobj, offs, low, high);
	}
    }
#else
    if (p->dictionary) 
	offset_heap(p->dictionary->data, 
		    p->dictionary->used, 
		    offs, low, high);
    if (p->debug_dictionary) 
	offset_heap(p->debug_dictionary->data, 
		    p->debug_dictionary->used, 
		    offs, low, high);
    offset_heap(&p->fvalue, 1, offs, low, high);
    offset_heap(&p->group_leader, 1, offs, low, high);
    offset_heap(&p->seq_trace_token, 1, offs, low, high);
    offset_mqueue(p, offs, low, high);
    offset_heap_ptr(p->stop, (STACK_START(p) - p->stop), offs, low, high);
    offset_nstack(p, offs, low, high);
    if (nobj > 0) {
	offset_heap(objv, nobj, offs, low, high);
    }
    offset_off_heap(p, low, high, offs);
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
#ifndef SHARED_HEAP
    int stack_size = p->hend - p->stop;
#endif
    Sint offs;

    ASSERT(HEAP_SIZE(p) < new_sz);
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)HEAP_START(p),
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "grow_new_heap: GREW %s FROM %d UPTO %d (used %d)\n",
            print_pid(p), HEAP_SIZE(p), new_sz, heap_size);
#endif

    if ((offs = new_heap - HEAP_START(p)) == 0) { /* No move. */
        HEAP_END(p) = new_heap + new_sz;
#ifndef SHARED_HEAP
        sys_memmove(p->hend - stack_size, p->stop, stack_size * sizeof(Eterm));
        p->stop = p->hend - stack_size;
#endif
    } else {
#ifndef SHARED_HEAP
        Eterm* prev_stop = p->stop;
#endif

        offset_heap(new_heap, heap_size, offs, HEAP_START(p), HEAP_TOP(p));
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));

        HEAP_END(p) = new_heap + new_sz;
#ifndef SHARED_HEAP
        prev_stop = new_heap + (p->stop - p->heap);
        p->stop = p->hend - stack_size;
        sys_memmove(p->stop, prev_stop, stack_size * sizeof(Eterm));
#endif
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

#ifdef SHARED_HEAP
    ASSERT(new_sz < HEAP_SIZE(p));
    new_heap = (Eterm *) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)HEAP_START(p),
					   sizeof(Eterm)*(HEAP_SIZE(p)),
					   sizeof(Eterm)*new_sz);
    HEAP_END(p) = new_heap + new_sz;
#else
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
#endif

#ifdef GC_HEAP_TRACE
    fprintf(stderr,
	    "shrink_new_heap: SHRINKED %s FROM %d DOWNTO %d (used %d)\n",
            print_pid(p), HEAP_SIZE(p), new_sz, heap_size);
#endif

    if ((offs = new_heap - HEAP_START(p)) != 0) {

        /*
         * Normally, we don't expect a shrunk heap to move, but you never
         * know on some strange embedded systems...  Or when using purify.
         */

        offset_heap(new_heap, heap_size, offs, HEAP_START(p), HEAP_TOP(p));
        HIGH_WATER(p) = new_heap + (HIGH_WATER(p) - HEAP_START(p));
        offset_rootset(p, offs, HEAP_START(p), HEAP_TOP(p), objv, nobj);
        HEAP_TOP(p) = new_heap + heap_size;
        HEAP_START(p) = new_heap;
    }
    HEAP_SIZE(p) = new_sz;
}

static void
adjust_after_fullsweep(Process *p, int size_before, int need, Eterm *objv, int nobj)
{
    int wanted, sz, size_after, need_after;
#ifdef SHARED_HEAP
    int stack_size = 0;        /* Size of stack ON HEAP */
#else
    int stack_size =  p->hend - p->stop;
#endif
    
    size_after = (HEAP_TOP(p) - HEAP_START(p));
    reclaimed += (size_before - size_after);
    
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
    int stack_size;             /* Size of stack ON HEAP. */
    int sz;
#ifdef __BENCHMARK__
    uint was_this_major = 0;
#endif
    Uint ms1, s1, us1;

    BM_NEW_TIMER(gc);

    BM_STOP_TIMER(system);

#ifdef BM_HEAP_SIZES
    {
        double total_used_heap = 0;
#ifdef SHARED_HEAP
        total_used_heap = (HEAP_TOP(p) - HEAP_START(p)) +
                          (OLD_HTOP(p) - OLD_HEAP(p));
        if (total_used_heap > max_used_global_heap)
            max_used_global_heap = total_used_heap;
#else
        int i;
        for (i = 0; i < erts_max_processes; i++)
        {
            Process *cp = process_tab[i];
            if (cp == NULL) continue;

            total_used_heap += (cp->htop - cp->heap) +
                               (cp->old_htop - cp->old_heap);
        }
        if (total_used_heap > max_used_heap)
            max_used_heap = total_used_heap;
#endif
    }
#endif /* BM_HEAP_SIZES */

    BM_START_TIMER(gc);

#ifdef SHARED_HEAP
    /* There are collections forced by the sheduler when the size of all
     * heap fragments and the overhead from binaries gets greater than the
     * heap size.
     * This is not the way to do it in the Shared heap...
     */
    if (need == 0 && nobj == 0 && !(FLAGS(p) & F_NEED_FULLSWEEP))
    {
        BM_SWAP_TIMER(gc,system);
        return 0;
    }
#endif

    if (SAVED_HEAP_TOP(p) != NULL) {
	HEAP_TOP(p) = SAVED_HEAP_TOP(p);
	SAVED_HEAP_TOP(p) = NULL;
    }

#ifdef SHARED_HEAP
#define OverRunCheck() \
    if (HEAP_END(p) < HEAP_TOP(p)) { \
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
    if (erts_system_monitor_long_gc != 0) get_now(&ms1, &s1, &us1);
    saved_status = p->status;
    p->status = P_GARBING;
    CHECK(p);
    OverRunCheck();
    if (GEN_GCS(p) >= MAX_GEN_GCS(p)) {
        FLAGS(p) |= F_NEED_FULLSWEEP;
    }

#ifndef SHARED_HEAP
    stack_size = p->hend - p->stop;
#else
    stack_size = 0;		/* No stack on the heap. */
#if 0
    n = erts_num_active_procs;
    for (i = 0; i < n; i++) {
        Process *p = erts_active_procs[i];
        ARITH_AVAIL(p) = 0;
        ARITH_HEAP(p) = NULL;
#ifdef DEBUG
        ARITH_CHECK_ME(p) = NULL;
#endif
    }
#endif
#endif

    MSO(p).overhead = 0;
    garbage_cols++;

    /* Size of heap before first GC */
    size_before = MBUF_SIZE(p) + (HEAP_TOP(p) - HEAP_START(p));

#ifdef SHARED_HEAP
    /* To prevent shrinking of the shared heap */
    p->min_heap_size = HEAP_SIZE(p);
#endif

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
#ifdef OLD_HEAP_CREATION_TRACE
#ifdef SHARED_HEAP
        fprintf(stderr, "Created a global old_heap because of %s.\r\n",
            print_pid(p));
#else
        fprintf(stderr, "Created old_heap for %s.\r\n", print_pid(p));
#endif
#endif
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

        gen_gc(p, next_heap_size(p, HEAP_SIZE(p) + MBUF_SIZE(p), 0), objv, nobj);
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
#ifdef GC_HEAP_TRACE
        fprintf(stderr, "Did a gen_gc, still not enough room\n");
#endif
    }

    /*
     * The previous generational GC did not leave enough free heap space.
     * We must do a fullsweep GC. First figure out the size of the heap
     * to receive all live data.
     */

    sz = HEAP_SIZE(p) + MBUF_SIZE(p) + (OLD_HTOP(p) - OLD_HEAP(p));
#ifndef SHARED_HEAP
    sz += p->hend - p->stop;
#endif
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
    adjust_after_fullsweep(p, size_before, need, objv, nobj);

#ifdef __BENCHMARK__
    was_this_major = 1;
#endif

 done:

    CHECK(p);
    OverRunCheck();
#ifdef SHARED_HEAP
    p->active = 1;
#endif
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
	t = t*1000 + ((Sint)(us2 - us1))/1000;
	if (t > 0 && (Uint)t >= erts_system_monitor_long_gc) {
	    monitor_long_gc(p, t);
	}
    }
    if (erts_system_monitor_large_heap != 0
	&& HEAP_SIZE(p) >= erts_system_monitor_large_heap) {
	monitor_large_heap(p);
    }

    BM_STOP_TIMER(gc);

#ifdef __BENCHMARK__
    if (was_this_major == 1)
    {
#ifdef SHARED_HEAP
        BM_COUNT(major_global_garbage_cols);
#else
        BM_COUNT(major_garbage_cols);
#endif
#ifdef BM_TIMERS
#ifdef SHARED_HEAP
        major_global_gc_time += gc_time;
        if (gc_time > max_global_major_time)
            max_global_major_time = gc_time;
#else
        major_gc_time += gc_time;
        if (gc_time > max_major_time)
            max_major_time = gc_time;
#endif
#endif
    }
    else
    {
#ifdef SHARED_HEAP
        BM_COUNT(minor_global_garbage_cols);
#else
        BM_COUNT(minor_garbage_cols);
#endif
#ifdef BM_TIMERS
#ifdef SHARED_HEAP
        minor_global_gc_time += gc_time;
        if (gc_time > max_global_minor_time)
            max_global_minor_time = gc_time;
#else
        minor_gc_time += gc_time;
        if (gc_time > max_minor_time)
            max_minor_time = gc_time;
#endif
#endif
    }
#endif /* __BENCHMARK__ */

#ifdef BM_HEAP_SIZES
    {
        double total_used_heap = 0;
        double total_allocated_heap = 0;
#ifdef SHARED_HEAP
        total_used_heap = (HEAP_TOP(p) - HEAP_START(p)) +
                          (OLD_HTOP(p) - OLD_HEAP(p));
        total_allocated_heap = HEAP_SIZE(p) + (OLD_HEND(p) - OLD_HEAP(p));
        if (total_used_heap > max_used_global_heap)
            max_used_global_heap = total_used_heap;
        if (total_allocated_heap > max_allocated_global_heap)
            max_allocated_global_heap = total_allocated_heap;
#else
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
#endif
    }
#endif /* BM_HEAP_SIZES */

    BM_START_TIMER(system);

    ARITH_LOWEST_HTOP(p) = (Eterm *) 0;
    ARITH_AVAIL(p) = 0;
    ARITH_HEAP(p) = NULL;
#ifdef DEBUG
    ARITH_CHECK_ME(p) = NULL;
#endif
    return ((int) (HEAP_TOP(p) - HEAP_START(p)) / 10);
#undef OverRunCheck
}

static Eterm*
gen_cheney(Process *p, Eterm* low, Eterm* high, Eterm* n_hp, Eterm* n_htop)
{
    Eterm* ptr;
    Eterm val;
    Eterm gval;

    while (n_hp != n_htop) {
        gval = *n_hp;

        switch (primary_tag(gval)) {
	case TAG_PRIMARY_BOXED: {
            ptr = boxed_val(gval);
            val = *ptr;
	    if (ptr_within(ptr, low, high)) {
		if (MY_IS_MOVED(val)) {
		    ASSERT(is_boxed(val));
		    *n_hp++ = val;
		} else {
		    MOVE_BOXED(ptr,val,n_htop,n_hp++);
		}
            } else {
                ASSERT(within(ptr, p));
                n_hp++;
            }
            continue;
	}
	case TAG_PRIMARY_LIST: {
            ptr = list_val(gval);
            val = *ptr;
	    if (ptr_within(ptr, low, high)) {
		if (is_non_value(val)) {
		    *n_hp++ = ptr[1];
		} else {
		    MOVE_CONS(ptr,val,n_htop,n_hp++);
		}
            } else {
                ASSERT(within(ptr, p));
                n_hp++;
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

    return n_htop;
}

/*
 * This function sweeps both the remainder of the new heap
 * as well as the remainder of the old heap after the first pass
 * of the generational collector gen_gc().
 */
static Eterm*
gen_cheney_old(Process *p, Eterm* from, Eterm** to,
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
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
            {
                ++n_hp;
            }
#endif
            else if (MY_IS_MOVED(val)) {
		ASSERT(is_boxed(val));
                *n_hp++ = val;
            } else if (ptr_within(ptr, low_water, high_water)) {
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
#if HIPE
            else if( in_area(ptr, const_start, const_size) )
            {
                ++n_hp;
            }
#endif
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
#ifdef SHARED_HEAP
    Rootset *rootset = erts_alloc(ERTS_ALC_T_ROOTSET,
				  sizeof(Rootset)*erts_max_processes);
#else
    Rootset rootset;            /* Rootset for GC (stack, dictionary, etc). */
#endif
    Eterm* n_hstart;
    Eterm* n_htop;
    int n;
    Eterm* ptr;
    Eterm val;
    Eterm gval;
    Eterm* oh_start = OLD_HEAP(p);
    Eterm* oh_end = OLD_HEND(p);
    Eterm* low_water = HEAP_START(p);
    Eterm* high_water = HIGH_WATER(p);
    Eterm* old_htop = OLD_HTOP(p);
    Eterm* tmp;
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif

#ifdef GC_HEAP_TRACE
    fprintf(stderr, "Generational GC %s ", print_pid(p));
#endif
    /* If flip is true, we need to tenure all (live) objects */
    /* within the watermarks, if flip is 0, we need to alloc a */
    /* new new_heap and copy all live objects to the new new_heap */
    /* that is to not tenure any objects at all */

    n_hstart = (Eterm*) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*new_sz);

    n_htop = n_hstart;
#ifdef SHARED_HEAP
    n = collect_roots(p, objv, nobj, rootset);

    while (n--) {
      GENSWEEP_NSTACK(rootset[n].p, old_htop, n_htop);
      while (rootset[n].n--) {
        Eterm* g_ptr = rootset[n].v[rootset[n].n];
        Uint g_sz = rootset[n].sz[rootset[n].n];
#else
    n = setup_rootset(p, objv, nobj, &rootset);

    GENSWEEP_NSTACK(p, old_htop, n_htop);
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
                {
                    g_ptr++;
                }
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
#if HIPE
                else if( in_area(ptr, const_start, const_size) )
                {
                    ++g_ptr;
                }
#endif
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
#ifdef SHARED_HEAP
    }
#endif

    /*
     * Now all references in the rootset point to the new heap. However,
     * most references on the new heap point to the old heap so the next stage
     * is to scan through the new heap evacuating data from the old heap
     * until all is changed.
     */

    tmp = n_htop;
    old_htop = gen_cheney_old(p, n_hstart, &tmp, low_water, high_water, old_htop);
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
				  OLD_HEAP(p), NULL);
	    old_htop = tmp;
	}
    }
    OLD_HTOP(p) = old_htop;
    HIGH_WATER(p) = (HEAP_START(p) != HIGH_WATER(p)) ? n_hstart : n_htop;

#ifdef SHARED_HEAP
    HEAP_END(p) = n_hstart + new_sz;
#else
    /*
     * Now we got to move the stack to the top of the new heap...
     */
    n = p->hend - p->stop;
    sys_memcpy(n_hstart + new_sz - n, p->stop, n * sizeof(Eterm));
    HEAP_END(p) = n_hstart + new_sz;
    p->stop = p->hend - n;
#endif

    if (MSO(p).mso) {
        sweep_proc_bins(p, 0);
    }
#ifndef SHARED_HEAP
    if (MSO(p).funs) {
        sweep_proc_funs(p, 0);
    }
#endif
    if (MSO(p).externals) {
	sweep_proc_externals(p, 0);
    }

#ifdef DEBUG
    sys_memset(HEAP_START(p), 0xff, HEAP_SIZE(p) * sizeof(Eterm));
#endif


    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		   (void *) HEAP_START(p),
		   HEAP_SIZE(p)*sizeof(Eterm));

#ifdef SHARED_HEAP
    restore_rootset(p, rootset);
    erts_free(ERTS_ALC_T_ROOTSET, (void *) rootset);
#else
    restore_rootset(p, &rootset);
#endif
    remove_message_buffers(p);

    HEAP_START(p) = n_hstart;
    HEAP_END(p) = n_hstart + new_sz;
    HEAP_TOP(p) = n_htop;
    HEAP_SIZE(p) = new_sz;
}

static void
sweep_proc_externals(Process *p, int fullsweep)
{
    ExternalThing** prev;
    ExternalThing* ptr;

    Eterm* bot = OLD_HEAP(p);
    Eterm* top = OLD_HEND(p);

    prev = &MSO(p).externals;
    ptr = MSO(p).externals;

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
        } else {                /* Object has not been moved - deref it */
	    DEREF_ERL_NODE(ptr->node);
            *prev = ptr = ptr->next;
        }
    }
    ASSERT(*prev == NULL);
}

#ifndef SHARED_HEAP
static void
sweep_proc_funs(Process *p, int fullsweep)
{
    ErlFunThing** prev;
    ErlFunThing* ptr;

    Eterm* bot = OLD_HEAP(p);
    Eterm* top = OLD_HEND(p);

    prev = &MSO(p).funs;
    ptr = MSO(p).funs;

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
#endif

static void
sweep_proc_bins(Process *p, int fullsweep)
{
    ProcBin** prev;
    ProcBin* ptr;
    Binary* bptr;

    Eterm* bot = OLD_HEAP(p);
    Eterm* top = OLD_HEND(p);

    prev = &MSO(p).mso;
    ptr = MSO(p).mso;

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
		    erts_bin_free(bptr);
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

static void
check_pointer(Process* p, Eterm obj, int back_pointers_allowed, char *msg)
{
    if (back_pointers_allowed && !within(ptr_val(obj), p)) {
        erl_exit(1, "%s: %s, line %d: %s: bad address %x\n",
                 print_pid(p), __FILE__, __LINE__, msg, obj);
    } else if (!back_pointers_allowed &&
             !ptr_within(ptr_val(obj), OLD_HEAP(p), OLD_HEND(p))) {
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
#ifdef SHARED_HEAP
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

#endif /* HEAP_FRAG_ELIM_TEST */
