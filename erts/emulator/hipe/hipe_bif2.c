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
#include "hipe_stack.h"

BIF_RETTYPE hipe_bifs_show_estack_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_estack(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_heap_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_heap(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_nstack_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
#ifdef __i386__
    BIF_P->hipe.narity = 1;
#endif
    hipe_print_nstack(rp);
#ifdef __i386__
    BIF_P->hipe.narity = 0;
#endif
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_pcb_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp = pid2proc(BIF_ARG_1);
    if( !rp )
	BIF_ERROR(BIF_P, BADARG);
    hipe_print_pcb(rp);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_show_term_1(BIF_ALIST_1)
BIF_ADECL_1
{
    printf("0x%08x\r\n", (unsigned)BIF_ARG_1);
    display(BIF_ARG_1, COUT);
    printf("\r\n");
    BIF_RET(am_true);
}

extern Eterm *hipe_constants_start;
extern Eterm *hipe_constants_next;

BIF_RETTYPE hipe_bifs_show_literals_0(BIF_ALIST_0)
BIF_ADECL_0
{
  char *i;
  
  printf("0x%08x: ", (unsigned int)hipe_constants_start);

  for (i = (char *)hipe_constants_start; 
       i < (char *)hipe_constants_next; 
       i++) {

    printf("%02x ", (unsigned char)*i);
    if (((int)i-(int)hipe_constants_start) % 8 == 7) 
      printf("\r\n0x%08x: ",(unsigned int)(i+1));
  }

  printf("\r\n");
  BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_in_native_0(BIF_ALIST_0)
BIF_ADECL_0
{ 
  BIF_RET(am_false);
}


BIF_RETTYPE hipe_bifs_heap_architecture_0(BIF_ALIST_0)
BIF_ADECL_0
{ 
#ifdef SHARED_HEAP
  BIF_RET(am_shared);
#else
  BIF_RET(am_private);
#endif
}


BIF_RETTYPE hipe_bifs_modeswitch_debug_on_0(BIF_ALIST_0)
BIF_ADECL_0
{ 
  hipe_modeswitch_debug = 1;
  BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_modeswitch_debug_off_0(BIF_ALIST_0)
BIF_ADECL_0
{ 
  hipe_modeswitch_debug = 0;
  BIF_RET(am_true);
}
