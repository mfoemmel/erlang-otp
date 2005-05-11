/* $Id$
 * hipe_bif2.c
 *
 * Miscellaneous add-ons.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "big.h"
#include "hipe_debug.h"
#include "hipe_mode_switch.h"
#include "hipe_bif0.h" /* hipe_constants_{start,next} */
#include "hipe_arch.h"
#include "hipe_stack.h"

BIF_RETTYPE hipe_bifs_show_estack_1(BIF_ALIST_1)
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_estack(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_heap_1(BIF_ALIST_1)
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_heap(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_nstack_1(BIF_ALIST_1)
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_set_narity(BIF_P, 1);
    hipe_print_nstack(rp);
    hipe_set_narity(BIF_P, 0);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_nstack_used_size_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_nstack_used(BIF_P)));
}

BIF_RETTYPE hipe_bifs_show_pcb_1(BIF_ALIST_1)
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_pcb(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_term_1(BIF_ALIST_1)
{
    Eterm obj = BIF_ARG_1;

    printf("0x%0*lx\r\n", 2*(int)sizeof(long), obj);
    do {
	Eterm *objp;
	int i, ary;

	if( is_list(obj) ) {
	    objp = list_val(obj);
	    ary = 2;
	} else if( is_boxed(obj) ) {
	    Eterm header;

	    objp = boxed_val(obj);
	    header = objp[0];
	    if( is_thing(header) )
		ary = thing_arityval(header);
	    else if( is_arity_value(header) )
		ary = arityval(header);
	    else {
		printf("bad header %#lx\r\n", header);
		break;
	    }
	    ary += 1;
	} else
	    break;
	for(i = 0; i < ary; ++i)
	    printf("0x%0*lx: 0x%0*lx\r\n",
		   2*(int)sizeof(long), (unsigned long)&objp[i],
		   2*(int)sizeof(long), objp[i]);
    } while( 0 );
    display(obj, COUT);
    printf("\r\n");
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_literals_0(BIF_ALIST_0)
{
    Eterm *p;

    p = hipe_constants_start;
    for(; p < hipe_constants_next; ++p)
	printf("0x%0*lx: 0x%0*lx\r\n",
	       2*(int)sizeof(long), (unsigned long)p,
	       2*(int)sizeof(long), *p);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_in_native_0(BIF_ALIST_0)
{
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_modeswitch_debug_on_0(BIF_ALIST_0)
{
    hipe_modeswitch_debug = 1;
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_modeswitch_debug_off_0(BIF_ALIST_0)
{
    hipe_modeswitch_debug = 0;
    BIF_RET(am_true);
}

Eterm hipe_bifs_test_reschedule_1(BIF_ALIST_1)
{
    static unsigned int flag;

    // printf("%s(%#lx), flag %u\r\n", __FUNCTION__, BIF_ARG_1, flag);
    // fflush(stdout);
    if( !flag && BIF_ARG_1 != am_false ) {
	flag = 1;
	erl_suspend(BIF_P, NIL);
	BIF_ERROR(BIF_P, RESCHEDULE);
    }
    flag = !flag;
    BIF_RET(NIL);
}

/* BIFs for handling the message area */

BIF_RETTYPE hipe_bifs_show_message_area_0(BIF_ALIST_0)
{
#ifdef HYBRID
#ifdef DEBUG
    print_message_area();
#else
    printf("Only available in debug compiled emulator\r\n");
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}
