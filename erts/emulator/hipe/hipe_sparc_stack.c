/* $Id$
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "beam_catches.h"
#include "hipe_stack.h"

/*
 * hipe_find_handler() is called from hipe_handle_exception() to locate
 * the current exception handler's PC and SP.
 * On exit, p->hipe.ncallee is set to the handler's PC and p->hipe.nsp
 * is set to its SP.
 */
void hipe_find_handler(Process *p)
{
    Eterm *nsp;
    Eterm *nsp_end;
    extern Eterm sparc_catch_fail;

    nsp = p->hipe.nsp;
    nsp_end = p->hipe.nstack;
    while( nsp > nsp_end ) {
	Eterm x = *--nsp;
	if( is_catch(x) && (p->catches >= 0 || x == sparc_catch_fail) ) {
	    p->hipe.nsp = nsp;
	    p->hipe.ncallee = (void(*)(void)) catch_pc(x);
	    return;
	}
    }
    fprintf(stderr, __FUNCTION__ ": no native CATCH found!\r\n");
    abort();
}
