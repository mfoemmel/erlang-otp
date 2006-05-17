/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "hipe_stack.h"
#include "hipe_process.h"

#define SLOT_TYPE_VAL 0
#define SLOT_TYPE_RA  1
#define SLOT_TYPE_ARG 2
#define SLOT_TYPE_TRAP 3

extern void nbif_return(void);
extern void nbif_stack_trap_ra(void);

/*
 * hipe_print_nstack() is called from hipe_bifs:show_nstack/1.
 */
static void print_slot(Process *p, Eterm *sp, unsigned int live, 
		       unsigned int type, unsigned long exnra)
{
    Eterm val = *sp;
    switch (type) {
    case SLOT_TYPE_RA: /* RA */
      if ((unsigned long) val == (unsigned long)nbif_return) {
	printf(" | 0x%08lx | 0x%08lx | NATIVE RA", (long)sp, val);
	if( exnra != 0 )
	  printf(", EXNRA 0x%08lx\n\r", exnra);      
	else
	  printf(" !! MISSING CATCH !!\n\r");

	printf(" |    MODE    |   SWITCH   |");

      } else {
	printf(" | 0x%08lx | 0x%08lx | NATIVE RA", (long)sp, val);
	if( exnra != 0 )
	  printf(", EXNRA 0x%08lx", exnra);      
      }
      break;

    case SLOT_TYPE_ARG:
      printf(" | 0x%08lx | 0x%08lx | ARG ", (long)sp, val);
      erts_printf("%.30T", val);
      break;

    case SLOT_TYPE_TRAP: /* RA */
      val = (unsigned long)p->hipe.ngra;
      if ((unsigned long) val == (unsigned long)nbif_return) {
	printf(" | 0x%08lx | 0x%08lx | TRAP RA", (long)sp, val);
	if( exnra != 0 )
	  printf(", EXNRA 0x%08lx\n\r", exnra);      
	else
	  printf(" !! MISSING CATCH !!\n\r");

	printf(" |    MODE    |   SWITCH   |");

      } else {
	printf(" | 0x%08lx | 0x%08lx | TRAP RA", (long)sp, val);
	if( exnra != 0 )
	  printf(", EXNRA 0x%08lx", exnra);      
      }
      break;

    case SLOT_TYPE_VAL:
    default:
      printf(" | 0x%08lx | 0x%08lx | ", (long)sp, val);
      if( live )
	erts_printf("%.30T", val);
      else
	printf("DEAD");
      break;
    }
    printf("\r\n");
}

void hipe_print_nstack(Process *p)
{
    Eterm *nsp;
    Eterm *nstart;
    const struct sdesc *sdesc;
    unsigned long ra;
    unsigned int arity;
    unsigned int lsize;
    unsigned int i;

    nsp = p->hipe.nsp-1;
    nstart = p->hipe.nstack;
    ra = (unsigned long)p->hipe.nra; /* XXX: Temp solution to store RA here */


    printf(" |      NATIVE  STACK      |\r\n");
    printf(" |------------|------------|\r\n");
    printf(" | heap       | 0x%08lx |\r\n", (unsigned long)p->heap);
    printf(" | high_water | 0x%08lx |\r\n", (unsigned long)p->high_water);
    printf(" | hend       | 0x%08lx |\r\n", (unsigned long)p->htop);
    printf(" | old_heap   | 0x%08lx |\r\n", (unsigned long)p->old_heap);
    printf(" | old_hend   | 0x%08lx |\r\n", (unsigned long)p->old_hend);
    printf(" | nstack     | 0x%08lx |\r\n", (unsigned long)p->hipe.nstack);
    printf(" | nsp        | 0x%08lx |\r\n", (unsigned long)p->hipe.nsp);
    printf(" | nstend     | 0x%08lx |\r\n", (unsigned long)p->hipe.nstend);
    printf(" | nstblacklim| 0x%08lx |\r\n", (unsigned long)p->hipe.nstblacklim);
    printf(" | nstgraylim | 0x%08lx |\r\n", (unsigned long)p->hipe.nstgraylim);
    printf(" |------------|------------|\r\n");
    printf(" | Address    | Contents   |\r\n");
    printf(" |------------|------------|\r\n");
    if(!ra) return;
    if( ra == (unsigned long)nbif_stack_trap_ra ) {
      sdesc = hipe_find_sdesc((unsigned long)p->hipe.ngra);
    } else {
      sdesc = hipe_find_sdesc(ra);
    }

    while( nsp > nstart  ) {
	arity = sdesc_arity(sdesc);
	lsize = sdesc_fsize(sdesc)- 1;
	printf(" | LOCALS %3i : ARITY %4i | \r\n", lsize, arity);

	for(i = 0; i < lsize; i++)
	    print_slot(p, &nsp[-i], sdesc->livebits[i>>5] & (1<<(i&31)),
		       SLOT_TYPE_VAL,0);
	nsp -= lsize;
	ra = nsp[0];
	if( ra == (unsigned long)nbif_stack_trap_ra ) {
	    sdesc = hipe_find_sdesc((unsigned long)p->hipe.ngra);
	    print_slot(p, nsp, 1, SLOT_TYPE_TRAP, sdesc_exnra(sdesc));
	} else {
	  sdesc = hipe_find_sdesc(ra);
	  print_slot(p, nsp, 1, SLOT_TYPE_RA, sdesc_exnra(sdesc));
	}
	nsp -= 1;
	for(i = 0; i < arity; ++i)
	    print_slot(p, &nsp[-i], 1, SLOT_TYPE_ARG,0);
	nsp -= arity;

	printf(" |------------|------------|\r\n");

    }
    /*   printf(" |------------|------------|\r\n"); */
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

    /* ASSERT that passed sdesc == real sdesc */
    /*    ra = (unsigned long)p->hipe.nra; *//* XXX: Temp solution to store RA here */
/*    if( ra == (unsigned long)nbif_stack_trap_ra ) 
      sdesc = hipe_find_sdesc((unsigned long)p->hipe.ngra);
    else 
      sdesc = hipe_find_sdesc(ra);
*/

    if( (char*)nsp - (char*)nstart < MINSTACK*sizeof(Eterm*) ) {
	p->hipe.nstgraylim = NULL;
	return;
    }
    n = NSKIPFRAMES;

    for(;;) {
	nsp -= sdesc_fsize(sdesc);
	if( nsp <= nstart ) {
	    p->hipe.nstgraylim = NULL;
	    return;
	}
	ra = nsp[1];
	if( --n <= 0 )
	    break;
	nsp -= sdesc_arity(sdesc);
	/* Can this ra be a stack_trap_ra ? */
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
    unsigned int arity;
    extern void nbif_fail(void);
    unsigned long exnra;
    const struct sdesc *sdesc;

    nsp = p->hipe.nsp;
    nstart = p->hipe.nstack;

    ra = (unsigned long)p->hipe.nra; /* XXX: Temp solution to store RA here */

    while( nsp > nstart ) {
	if( ra == (unsigned long)nbif_stack_trap_ra )
	    ra = (unsigned long)p->hipe.ngra;
	sdesc = hipe_find_sdesc(ra);
	if( (exnra = sdesc_exnra(sdesc)) != 0 &&
	    (p->catches >= 0 ||
	     exnra == (unsigned long)nbif_fail) ) {
	    p->hipe.ncallee = (void(*)(void)) exnra;
	    p->hipe.nsp = nsp;
	    /* update the gray/white boundary if we threw past it */
	    if( p->hipe.nstgraylim && nsp <= p->hipe.nstgraylim )
	      hipe_update_stack_trap(p, sdesc);

	    return;
	}
	nsp -= sdesc_fsize(sdesc);
	ra = nsp[0];            

	arity = sdesc_arity(sdesc);
	nsp -= arity;		/* skip actuals on stack */
    }
    fprintf(stderr, "%s: no native CATCH found!\r\n", __FUNCTION__);
    hipe_print_nstack(p);
    abort();
}
