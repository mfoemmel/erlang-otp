/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "bif.h"
#include "hipe_bif0.h"		/* for hipe_find_sdesc() */
#include "hipe_stack.h"

/*
 * These are C-wrappers for non-HiPE BIFs that may trigger a native
 * stack walk with p->hipe.narity != 0.
 */
extern Eterm check_process_code_2(Process*, Eterm, Eterm);
extern Eterm garbage_collect_1(Process*, Eterm);

Eterm hipe_x86_check_process_code_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm ret;

    BIF_P->hipe.narity = 2;
    ret = check_process_code_2(BIF_P, BIF_ARG_1, BIF_ARG_2);
    BIF_P->hipe.narity = 0;
    return ret;
}

Eterm hipe_x86_garbage_collect_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm ret;

    BIF_P->hipe.narity = 1;
    ret = garbage_collect_1(BIF_P, BIF_ARG_1);
    BIF_P->hipe.narity = 0;
    return ret;
}

/*
 * Output a raw nstack dump if a stack walk procedure fails.
 * Can't use hipe_print_nstack() since it assumes the stack
 * is clean and traversable.
 */
static void dump_nstack(Process *p)
{
    Eterm *sp, *end;

    printf(" |      NATIVE  STACK      |\r\n");
    sp = p->hipe.nsp;
    end = p->hipe.nstend;
    printf(" | Address    | Contents   |\r\n");
    while( sp < end ) {
	Eterm val = sp[0];
	printf(" | 0x%08x | 0x%08lx | ", (unsigned int)sp, val);
	ldisplay(val, COUT, 30);
	printf("\r\n");
	sp += 1;
    }
    printf(" |------------|------------|\r\n");
}

/*
 * hipe_find_handler() is called from hipe_handle_exception() to locate
 * the current exception handler's PC and SP.
 * The native stack MUST contain a stack frame as it appears on
 * entry to a function (return address, actuals, caller's frame).
 * p->hipe.narity MUST contain the arity (number of actuals).
 * On exit, p->hipe.ncallee is set to the handler's PC and p->hipe.nsp
 * is set to its SP (low address of its stack frame).
 */
void hipe_find_handler(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra;
    unsigned int arity;
    struct sdesc *sdesc;
    extern void nbif_fail(void);

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    arity = p->hipe.narity;

    while( nsp < nsp_end ) {
	ra = nsp[0];
	sdesc = hipe_find_sdesc(ra);
	if( !sdesc ) {
	    fprintf(stderr, __FUNCTION__ ": ra %#lx at sp %#lx has no sdesc!\r\n",
		    ra, (long)nsp);
	    break;
	}
	/* nsp = nsp + 1 + arity + sdesc->fsize; */
	nsp += 1;		/* skip ra */
	nsp += arity;		/* skip actuals */
	if( sdesc->exnra &&
	    (p->catches >= 0 ||
	     sdesc->exnra == (unsigned long)nbif_fail) ) {
	    p->hipe.ncallee = (void(*)(void)) sdesc->exnra;
	    p->hipe.nsp = nsp;
	    p->hipe.narity = 0;
	    return;
	}
	nsp += sdesc->fsize;
	arity = sdesc->arity;
    }
    fprintf(stderr, __FUNCTION__ ": no native CATCH found!\r\n");
    dump_nstack(p);
    abort();
}

/*
 * hipe_clean_nstack() is called from ggc.c:setup_rootset()
 * to ensure that non-traceable stack slots contain zeros.
 * The native stack MUST contain a stack frame as it appears on
 * entry to a function (return address, actuals, caller's frame).
 * p->hipe.narity MUST contain the arity (number of actuals).
 */
void hipe_clean_nstack(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra;
    unsigned int arity;
    struct sdesc *sdesc;
    unsigned int i;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    arity = p->hipe.narity;

    for(;;) {
	if( nsp >= nsp_end ) {
	    if( nsp == nsp_end )
		return;
	    fprintf(stderr, __FUNCTION__ ": passed end of stack\r\n");
	    break;
	}
	ra = nsp[0];
	sdesc = hipe_find_sdesc(ra);
	if( !sdesc ) {
	    fprintf(stderr, __FUNCTION__ ": ra %#lx at sp %#lx has no sdesc\r\n",
		    ra, (long)nsp);
	    break;
	}
	if( !sdesc->altra )
	    return;
	nsp[0] = sdesc->altra;
	/* nsp = nsp + 1 + arity + sdesc->fsize; */
	nsp += 1;		/* skip ra */
	nsp += arity;		/* skip actuals */
	for(i = 0; i < sdesc->nskip; ++i) {
	    unsigned int off = sdesc->skip[i];
	    nsp[off] = 0;
	}
	nsp += sdesc->fsize;
	arity = sdesc->arity;
    }
    dump_nstack(p);
    abort();
}
