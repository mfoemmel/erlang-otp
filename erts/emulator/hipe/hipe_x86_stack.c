/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "bif.h"
#include "hipe_bif0.h"		/* for hipe_find_sdesc() */
#include "hipe_stack.h"
#include "hipe_x86_asm.h"	/* for X86_NR_ARG_REGS */

extern void nbif_fail(void);
extern void nbif_stack_trap_ra(void);

/*
 * These are C-wrappers for non-HiPE BIFs that may trigger a native
 * stack walk with p->hipe.narity != 0.
 */
extern Eterm check_process_code_2(Process*, Eterm, Eterm);
extern Eterm garbage_collect_1(Process*, Eterm);

Eterm hipe_x86_check_process_code_2(BIF_ALIST_2)
{
    Eterm ret;

    BIF_P->hipe.narity = 2;
    ret = check_process_code_2(BIF_P, BIF_ARG_1, BIF_ARG_2);
    BIF_P->hipe.narity = 0;
    return ret;
}

Eterm hipe_x86_garbage_collect_1(BIF_ALIST_1)
{
    Eterm ret;

    BIF_P->hipe.narity = 1;
    ret = garbage_collect_1(BIF_P, BIF_ARG_1);
    BIF_P->hipe.narity = 0;
    return ret;
}

/*
 * hipe_print_nstack() is called from hipe_bifs:show_nstack/1.
 */
static void print_slot(Eterm *sp, unsigned int live)
{
    Eterm val = *sp;
    printf(" | 0x%08lx | 0x%08lx | ", (long)sp, val);
    if( live )
	ldisplay(val, COUT, 30);
    printf("\r\n");
}

void hipe_print_nstack(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    struct sdesc sdesc0;
    const struct sdesc *sdesc1;
    const struct sdesc *sdesc;
    unsigned long ra;
    unsigned long exnra;
    unsigned int mask;
    unsigned int sdesc_size;
    unsigned int i;
    unsigned int nstkarity;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;

    nstkarity = p->hipe.narity - X86_NR_ARG_REGS;
    if( (int)nstkarity < 0 )
	nstkarity = 0;
    sdesc0.summary = nstkarity;
    sdesc0.livebits[0] = ~1;
    sdesc = &sdesc0;

    printf(" |      NATIVE  STACK      |\r\n");
    printf(" |------------|------------|\r\n");
    printf(" | heap       | 0x%08lx |\r\n", (unsigned long)p->heap);
#ifndef SHARED_HEAP
    printf(" | high_water | 0x%08lx |\r\n", (unsigned long)p->high_water);
#endif
    printf(" | hend       | 0x%08lx |\r\n", (unsigned long)p->htop);
#ifndef SHARED_HEAP
    printf(" | old_heap   | 0x%08lx |\r\n", (unsigned long)p->old_heap);
    printf(" | old_hend   | 0x%08lx |\r\n", (unsigned long)p->old_hend);
#endif
    printf(" | nsp        | 0x%08lx |\r\n", (unsigned long)p->hipe.nsp);
    printf(" | nstend     | 0x%08lx |\r\n", (unsigned long)p->hipe.nstend);
    printf(" | nstblacklim| 0x%08lx |\r\n", (unsigned long)p->hipe.nstblacklim);
    printf(" | nstgraylim | 0x%08lx |\r\n", (unsigned long)p->hipe.nstgraylim);
    printf(" |------------|------------|\r\n");
    printf(" | Address    | Contents   |\r\n");

    for(;;) {
	printf(" |------------|------------|\r\n");
	if( nsp >= nsp_end ) {
	    if( nsp == nsp_end )
		return;
	    fprintf(stderr, "%s: passed end of stack\r\n", __FUNCTION__);
	    break;
	}
	ra = nsp[sdesc_fsize(sdesc)];
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    sdesc1 = hipe_find_sdesc((unsigned long)p->hipe.ngra);
	else
	    sdesc1 = hipe_find_sdesc(ra);
	sdesc_size = sdesc_fsize(sdesc) + 1 + sdesc_arity(sdesc);
	i = 0;
	mask = sdesc->livebits[0];
	for(;;) {
	    if( i == sdesc_fsize(sdesc) ) {
		printf(" | 0x%08lx | 0x%08lx | ", (long)&nsp[i], ra);
		if( ra == (unsigned long)nbif_stack_trap_ra )
		    printf("STACK TRAP, ORIG RA 0x%08lx", (unsigned long)p->hipe.ngra);
		else
		    printf("NATIVE RA");
		if( (exnra = sdesc_exnra(sdesc1)) != 0 )
		    printf(", EXNRA 0x%08lx", exnra);
		printf("\r\n");
	    } else {
		print_slot(&nsp[i], (mask & 1));
	    }
	    if( ++i >= sdesc_size )
		break;
	    if( i & 31 )
		mask >>= 1;
	    else
		mask = sdesc->livebits[i >> 5];
	}
	nsp += sdesc_size;
	sdesc = sdesc1;
    }
    abort();
}

#define MINSTACK	128
#define NSKIPFRAMES	4

void hipe_update_stack_trap(Process *p, const struct sdesc *sdesc)
{
    Eterm *nsp;
    Eterm *nsp_end;
    unsigned long ra;
    int n;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    if( (unsigned)((char*)nsp_end - (char*)nsp) < MINSTACK*sizeof(Eterm*) ) {
	p->hipe.nstgraylim = NULL;
	return;
    }
    n = NSKIPFRAMES;
    for(;;) {
	nsp += sdesc_fsize(sdesc);
	if( nsp >= nsp_end ) {
	    p->hipe.nstgraylim = NULL;
	    return;
	}
	ra = nsp[0];
	if( --n <= 0 )
	    break;
	nsp += 1 + sdesc_arity(sdesc);
	sdesc = hipe_find_sdesc(ra);
    }
    p->hipe.nstgraylim = nsp + 1 + sdesc_arity(sdesc);
    p->hipe.ngra = (void(*)(void))ra;
    nsp[0] = (unsigned long)nbif_stack_trap_ra;
}

/*
 * hipe_handle_stack_trap() is called when the mutator returns to
 * nbif_stack_trap_ra, which marks the gray/white stack boundary frame.
 * The gray/white boundary is moved back one or more frames.
 *
 * The function head below is "interesting".
 */
void (*hipe_handle_stack_trap(Process *p))(void)
{
    void (*ngra)(void) = p->hipe.ngra;
    const struct sdesc *sdesc = hipe_find_sdesc((unsigned long)ngra);
    hipe_update_stack_trap(p, sdesc);
    return ngra;
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
    unsigned long exnra;
    unsigned int arity;
    const struct sdesc *sdesc;
    unsigned int nstkarity;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstend;
    nstkarity = p->hipe.narity - X86_NR_ARG_REGS;
    if( (int)nstkarity < 0 )
	nstkarity = 0;
    arity = nstkarity;

    while( nsp < nsp_end ) {
	ra = nsp[0];
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	/* nsp = nsp + 1 + arity + sdesc_fsize(sdesc); */
	nsp += 1;		/* skip ra */
	nsp += arity;		/* skip actuals */
	if( (exnra = sdesc_exnra(sdesc)) != 0 &&
	    (p->catches >= 0 ||
	     exnra == (unsigned long)nbif_fail) ) {
	    p->hipe.ncallee = (void(*)(void)) exnra;
	    p->hipe.nsp = nsp;
	    p->hipe.narity = 0;
	    /* update the gray/white boundary if we threw past it */
	    if( p->hipe.nstgraylim && nsp >= p->hipe.nstgraylim )
		hipe_update_stack_trap(p, sdesc);
	    return;
	}
	nsp += sdesc_fsize(sdesc);
	arity = sdesc_arity(sdesc);
    }
    fprintf(stderr, "%s: no native CATCH found!\r\n", __FUNCTION__);
    abort();
}
