/* $Id$
 * GC support procedures
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "ggc.h"
#include "erl_nmgc.h"

#include "hipe_stack.h"
#include "hipe_gc.h"
#include "hipe_bif0.h"		/* for hipe_constants_{start,next} */

Eterm *fullsweep_nstack(Process *p, Eterm *n_htop)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* fullsweep-specific state */
    char *const_start;
    unsigned long const_size;

    if( !nstack_walk_init_check(p) )
	return n_htop;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = p->hipe.nstgraylim;
    if( nsp_end )
	nstack_walk_kill_trap(p, nsp_end);
    nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		if( nsp ) {
		    /* see the HIGH_WATER update in fullsweep_heap() */
		    p->hipe.nstblacklim = nsp; /* nsp == nsp_end */
		    nstack_walk_update_trap(p, walk_state.sdesc0);
		}
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			*nsp_i = val;
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
#ifdef HYBRID
                    } else if( ptr_within(ptr, global_heap, global_htop) ) {
                    } else if( ptr_within(ptr, global_old_heap, global_old_hend) ) {
#endif
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			*nsp_i = ptr[1];
		    } else if( in_area(ptr, const_start, const_size) ) {
			;
#ifdef HYBRID
                    } else if( ptr_within(ptr, global_heap, global_htop) ) {
                    } else if( ptr_within(ptr, global_old_heap, global_old_hend) ) {
#endif
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,nsp_i);
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
	ra = nstack_walk_frame_ra(nsp, sdesc);
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop, Eterm *objv, int nobj)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* gensweep-specific state */
    Eterm *oh_start, *oh_end;
    Eterm *low_water, *high_water;
    Eterm *n_htop;
    Eterm *old_htop;
    char *const_start;
    unsigned long const_size;

    if( !nstack_walk_init_check(p) )
	return;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = p->hipe.nstgraylim;
    if( nsp_end ) {
	/* if gray limit passed black limit, reset black limit */
	if( nstack_walk_gray_passed_black(nsp_end, p->hipe.nstblacklim) )
	    p->hipe.nstblacklim = nsp_end;
	nstack_walk_kill_trap(p, nsp_end);
	nsp_end = p->hipe.nstblacklim;
    } else
	nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    const_start = (char*)hipe_constants_start;
    const_size = (char*)hipe_constants_next - const_start;
    oh_start = OLD_HEAP(p);
    oh_end = OLD_HEND(p);
    old_htop = *ptr_old_htop;
    low_water = HEAP_START(p);
    high_water = HIGH_WATER(p);
    n_htop = *ptr_n_htop;

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		if( nsp ) {
		    /* see the HIGH_WATER update in gen_gc() */
		    if( HEAP_START(p) != HIGH_WATER(p) ) {
			p->hipe.nstblacklim =
			    p->hipe.nstgraylim
			    ? p->hipe.nstgraylim
			    : nsp; /* nsp == nsp_end */
		    } else {
			/* blacklim = graylim ? blacklim : end */
			if( !p->hipe.nstgraylim )
			    p->hipe.nstblacklim = nsp; /* nsp == nsp_end */
		    }
		    nstack_walk_update_trap(p, walk_state.sdesc0);
		}
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
#ifdef HYBRID
                    } else if( ptr_within(ptr, global_heap, global_htop) ) {
                    } else if( ptr_within(ptr, global_old_heap, global_old_hend) ) {
#endif
		    } else if( MY_IS_MOVED(val) ) {
			*nsp_i = val;
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_BOXED(ptr,val,old_htop,nsp_i);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( ptr_within(ptr, oh_start, oh_end) ) {
		    } else if( in_area(ptr, const_start, const_size) ) {
#ifdef HYBRID
                    } else if( ptr_within(ptr, global_heap, global_htop) ) {
                    } else if( ptr_within(ptr, global_old_heap, global_old_hend) ) {
#endif
		    } else if( is_non_value(val) ) {
			*nsp_i = ptr[1];
		    } else if( ptr_within(ptr, low_water, high_water) ) {
			MOVE_CONS(ptr,val,old_htop,nsp_i);
		    } else {
			ASSERT(within(ptr, p));
			MOVE_CONS(ptr,val,n_htop,nsp_i);
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
	ra = nstack_walk_frame_ra(nsp, sdesc);
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

#ifdef HYBRID

#ifdef INCREMENTAL
Eterm *ma_fullsweep_nstack(Process *p, Eterm *n_htop, Eterm *n_hend)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    if( !nstack_walk_init_check(p) )
	return n_htop;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm val = *nsp_i;
                Eterm *obj_ptr = ptr_val(val);
                switch (primary_tag(val)) {
                    case TAG_PRIMARY_LIST:
                        COPYMARK_CONS(obj_ptr,n_htop,nsp_i,n_hend); break;
                    case TAG_PRIMARY_BOXED:
                        COPYMARK_BOXED(obj_ptr,n_htop,nsp_i,n_hend); break;
                    default: break;
                }
            }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	ra = nstack_walk_frame_ra(nsp, sdesc);
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

void ma_gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop, Eterm *objv, int nobj)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* ma_gensweep-specific state */
    Eterm *low_water, *high_water, *surface;
    Eterm *n_htop;
    Eterm *old_htop;

    if( !nstack_walk_init_check(p) )
	return;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = nstack_walk_nsp_end(p);

    low_water = global_heap;
    //high_water = global_high_water;
    surface = global_htop;

    old_htop = *ptr_old_htop;
    n_htop = *ptr_n_htop;

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			*nsp_i = val;
		    } else if (ptr_within(ptr,low_water,high_water)) {
			MOVE_BOXED(ptr,val,old_htop,nsp_i);
		    } else if (ptr_within(ptr,high_water,surface)) {
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			*nsp_i = ptr[1];
		    } else if (ptr_within(ptr,low_water,high_water)) {
			MOVE_CONS(ptr,val,old_htop,nsp_i);
		    } else if (ptr_within(ptr,high_water,surface)) {
			MOVE_CONS(ptr,val,n_htop,nsp_i);
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
	ra = nstack_walk_frame_ra(nsp, sdesc);
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

#else /* not INCREMENTAL */

Eterm *ma_fullsweep_nstack(Process *p, Eterm *n_htop)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* ma_fullsweep-specific state */
    Eterm *gheap = global_heap;
    Eterm *ghtop = global_htop;
    Eterm *goheap = global_old_heap;
    Eterm *gohtop = global_old_htop;

    if( !nstack_walk_init_check(p) )
	return n_htop;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = nstack_walk_nsp_end(p);

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		return n_htop;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			*nsp_i = val;
		    } else if (ptr_within(ptr, gheap, ghtop)) {
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    } else if (ptr_within(ptr, goheap, gohtop)) {
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			*nsp_i = ptr[1];
		    } else if (ptr_within(ptr, gheap, ghtop)) {
			MOVE_CONS(ptr,val,n_htop,nsp_i);
		    } else if (ptr_within(ptr, gheap, ghtop)) {
			MOVE_CONS(ptr,val,n_htop,nsp_i);
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
	ra = nstack_walk_frame_ra(nsp, sdesc);
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}

void ma_gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop, Eterm *objv, int nobj)
{
    /* known nstack walk state */
    Eterm *nsp;
    Eterm *nsp_end;
    const struct sdesc *sdesc;
    unsigned int sdesc_size;
    unsigned long ra;
    unsigned int i;
    unsigned int mask;
    /* arch-specific nstack walk state */
    struct nstack_walk_state walk_state;

    /* ma_gensweep-specific state */
    Eterm *low_water, *high_water, *surface;
    Eterm *n_htop;
    Eterm *old_htop;

    if( !nstack_walk_init_check(p) )
	return;

    nsp = nstack_walk_nsp_begin(p);
    nsp_end = nstack_walk_nsp_end(p);

    low_water = global_heap;
    high_water = global_high_water;
    surface = global_htop;

    old_htop = *ptr_old_htop;
    n_htop = *ptr_n_htop;

    sdesc = nstack_walk_init_sdesc(p, &walk_state);

    for(;;) {
	if( nstack_walk_nsp_reached_end(nsp, nsp_end) ) {
	    if( nsp == nsp_end ) {
		*ptr_old_htop = old_htop;
		*ptr_n_htop = n_htop;
		return;
	    }
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	sdesc_size = nstack_walk_frame_size(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( mask & 1 ) {
		Eterm *nsp_i = nstack_walk_frame_index(nsp, i);
		Eterm gval = *nsp_i;
		if( is_boxed(gval) ) {
		    Eterm *ptr = boxed_val(gval);
		    Eterm val = *ptr;
		    if( MY_IS_MOVED(val) ) {
			*nsp_i = val;
		    } else if (ptr_within(ptr,low_water,high_water)) {
			MOVE_BOXED(ptr,val,old_htop,nsp_i);
		    } else if (ptr_within(ptr,high_water,surface)) {
			MOVE_BOXED(ptr,val,n_htop,nsp_i);
		    }
		} else if( is_list(gval) ) {
		    Eterm *ptr = list_val(gval);
		    Eterm val = *ptr;
		    if( is_non_value(val) ) {
			*nsp_i = ptr[1];
		    } else if (ptr_within(ptr,low_water,high_water)) {
			MOVE_CONS(ptr,val,old_htop,nsp_i);
		    } else if (ptr_within(ptr,high_water,surface)) {
			MOVE_CONS(ptr,val,n_htop,nsp_i);
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
	ra = nstack_walk_frame_ra(nsp, sdesc);
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	nsp = nstack_walk_next_frame(nsp, sdesc_size);
    }
    abort();
}
#endif /* INCREMENTAL */

#endif /* HYBRID */
