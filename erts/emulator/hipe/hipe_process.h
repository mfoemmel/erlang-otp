/* $Id$
 * HiPE-specific process fields
 */
#ifndef HIPE_PROCESS_H
#define HIPE_PROCESS_H

#include "erl_alloc.h"

struct hipe_process_state {
    Eterm *nsp;			/* Native stack pointer. */
    Eterm *nstack;		/* Native stack block start. */
    Eterm *nstend;		/* Native stack block end (start+size). */
    /* XXX: ncallee and closure could share space in a union */
    void (*ncallee)(void);	/* Native code callee (label) to invoke. */
    Eterm closure;		/* Used to pass a closure from native code. */
    Eterm *nstgraylim;		/* Gray/white stack boundary. */
    Eterm *nstblacklim;		/* Black/gray stack boundary. Must exist if
				   graylim exists. Ignored if no graylim. */
    void (*ngra)(void);		/* Saved original RA from graylim frame. */
#if defined(__sparc__)
    void (*nra)(void);		/* Native Return Address == where to resume. */
                                /* XXX: Used to store the return address 
                                        of the current bif call.
                                        To find the first stack descriptor
					at GC or exception. */
    Eterm *beami;               
    void (*ncra)(void);		/* C return address for native code. */

#endif
#if defined(__i386__)
    Eterm *ncsp;		/* Saved C stack pointer. */
    unsigned narity;
#endif
};

#define HIPE_SPARC_ARGS_IN_REGS 16	/* Stored in p->def_arg_reg[]. */

/* Guaranteed min stack size for leaf functions on SPARC. */
#define HIPE_SPARC_LEAF_WORDS 20

static __inline__ void hipe_init_process(struct hipe_process_state *p)
{
    p->nsp = NULL;
    p->nstack = NULL;
    p->nstend = NULL;
    p->nstgraylim = NULL;
    p->nstblacklim = NULL;
    p->ngra = NULL;
#if defined(__sparc__)
    p->nra = NULL;
#endif
#if defined(__i386__)
    p->narity = 0;
#endif
}

static __inline__ void hipe_delete_process(struct hipe_process_state *p)
{
    if( p->nstack )
	erts_free(ERTS_ALC_T_HIPE, (void*)p->nstack);
}


#endif /* HIPE_PROCESS_H */
