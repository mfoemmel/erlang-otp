/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "hipe_stack.h"
#include "hipe_process.h"

extern void nbif_return(void);
extern void nbif_fail(void);
extern void nbif_stack_trap_ra(void);

/*
 * hipe_print_nstack() is called from hipe_bifs:show_nstack/1.
 */
static void print_slot(Eterm *sp, unsigned int live)
{
    Eterm val = *sp;
    printf(" | 0x%0*lx | 0x%0*lx | ",
	   2*(int)sizeof(long), (unsigned long)sp,
	   2*(int)sizeof(long), val);
    if (live)
	erts_printf("%.30T", val);
    printf("\r\n");
}

void hipe_print_nstack(Process *p)
{
    Eterm *nsp;
    Eterm *nstart;
    const struct sdesc *sdesc;
    unsigned long ra;
    unsigned long exnra;
    unsigned int arity;
    unsigned int lsize;
    unsigned int i;
    static const char dashes[2*sizeof(long)+5] = {
	[0 ... 2*sizeof(long)+3] = '-'
    };

    printf(" |      NATIVE  STACK      |\r\n");
    printf(" |%s|%s|\r\n", dashes, dashes);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "heap",
	   2*(int)sizeof(long), (unsigned long)p->heap);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "high_water",
	   2*(int)sizeof(long), (unsigned long)p->high_water);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "hend",
	   2*(int)sizeof(long), (unsigned long)p->htop);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "old_heap",
	   2*(int)sizeof(long), (unsigned long)p->old_heap);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "old_hend",
	   2*(int)sizeof(long), (unsigned long)p->old_hend);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nstack",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstack);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nsp",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nsp);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nstend",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstend);
    printf(" | %*s| 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long)+1, "nstblacklim",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstblacklim);
    printf(" | %*s | 0x%0*lx |\r\n",
	   2+2*(int)sizeof(long), "nstgraylim",
	   2*(int)sizeof(long), (unsigned long)p->hipe.nstgraylim);
    printf(" |%s|%s|\r\n", dashes, dashes);
    printf(" | %*s | %*s |\r\n",
	   2+2*(int)sizeof(long), "Address",
	   2+2*(int)sizeof(long), "Contents");
    printf(" |%s|%s|\r\n", dashes, dashes);

    ra = (unsigned long)p->hipe.nra;
    if (!ra)
	return;
    nsp = p->hipe.nsp-1;
    nstart = p->hipe.nstack;

    if (ra == (unsigned long)&nbif_stack_trap_ra)
	ra = (unsigned long)p->hipe.ngra;
    sdesc = hipe_find_sdesc(ra);

    while (nsp > nstart) {
	arity = sdesc_arity(sdesc);
	lsize = sdesc_fsize(sdesc)- 1;
	printf(" | LOCALS %3i : ARITY %4i | \r\n", lsize, arity);
	for(i = 0; i < lsize; i++)
	    print_slot(&nsp[-i], sdesc->livebits[i>>5] & (1<<(i&31)));
	nsp -= lsize;
	ra = nsp[0];
	if (ra == (unsigned long)&nbif_stack_trap_ra)
	    sdesc = hipe_find_sdesc((unsigned long)p->hipe.ngra);
	else
	    sdesc = hipe_find_sdesc(ra);
	printf(" | 0x%0*lx | 0x%0*lx | ",
	       2*(int)sizeof(long), (unsigned long)&nsp[0],
	       2*(int)sizeof(long), ra);
	if (ra == (unsigned long)&nbif_stack_trap_ra)
	    printf("STACK TRAP, ORIG RA 0x%lx", (unsigned long)p->hipe.ngra);
	else if (ra == (unsigned long)&nbif_return)
	    printf("MODE SWITCH");
	else
	    printf("NATIVE RA");
	if ((exnra = sdesc_exnra(sdesc)) != 0)
	    printf(", EXNRA 0x%lx", exnra);
	printf("\r\n");
	nsp -= 1;
	for(i = 0; i < arity; ++i)
	    print_slot(&nsp[-i], 1);
	nsp -= arity;
	printf(" |------------|------------|\r\n");
    }
}

#define MINSTACK	256
#define NSKIPFRAMES	32

void hipe_update_stack_trap(Process *p, const struct sdesc *sdesc)
{
    Eterm *nsp;
    Eterm *nstart;
    unsigned long ra;
    int n;

    nsp = p->hipe.nsp-1;
    nstart = p->hipe.nstack;
    if ((unsigned long)((char*)nsp - (char*)nstart) < MINSTACK*sizeof(Eterm*)) {
	p->hipe.nstgraylim = NULL;
	return;
    }
    n = NSKIPFRAMES;
    for(;;) {
	nsp -= sdesc_fsize(sdesc);
	if (nsp <= nstart) {
	    p->hipe.nstgraylim = NULL;
	    return;
	}
	ra = nsp[1];
	if (--n <= 0)
	    break;
	nsp -= sdesc_arity(sdesc);
	sdesc = hipe_find_sdesc(ra);
    }
    p->hipe.nstgraylim = nsp - sdesc_arity(sdesc);
    p->hipe.ngra = (void(*)(void))ra;
    nsp[1] = (unsigned long)nbif_stack_trap_ra;
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
    p->hipe.nra=p->hipe.ngra;
    hipe_update_stack_trap(p, sdesc);
    return ngra;
}

/*
 * hipe_find_handler() is called from hipe_handle_exception() to locate
 * the current exception handler's PC and SP.
 * On exit, p->hipe.ncallee is set to the handler's PC and p->hipe.nsp
 * is set to its SP.
 */
void hipe_find_handler(Process *p)
{
    Eterm *nsp;
    Eterm *nstart;
    unsigned long ra;
    unsigned long exnra;
    unsigned int arity;
    const struct sdesc *sdesc;

    nsp = p->hipe.nsp;
    nstart = p->hipe.nstack;

    ra = (unsigned long)p->hipe.nra;

    while (nsp > nstart) {
	if (ra == (unsigned long)&nbif_stack_trap_ra)
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	if ((exnra = sdesc_exnra(sdesc)) != 0 &&
	    (p->catches >= 0 ||
	     exnra == (unsigned long)&nbif_fail)) {
	    p->hipe.ncallee = (void(*)(void)) exnra;
	    p->hipe.nsp = nsp;
	    /* update the gray/white boundary if we threw past it */
	    if (p->hipe.nstgraylim && nsp <= p->hipe.nstgraylim)
	      hipe_update_stack_trap(p, sdesc);
	    return;
	}
	nsp -= sdesc_fsize(sdesc);
	ra = nsp[0];
	arity = sdesc_arity(sdesc);
	nsp -= arity;		/* skip actuals on stack */
    }
    fprintf(stderr, "%s: no native CATCH found!\r\n", __FUNCTION__);
    abort();
}
