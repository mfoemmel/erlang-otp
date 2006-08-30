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
#endif
#if defined(__i386__) || defined(__x86_64__)
    Eterm *ncsp;		/* Saved C stack pointer. */
    unsigned int narity;
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    void (*nra)(void);		/* Native code return address. */
    unsigned int narity;	/* Arity of BIF call, for stack walks. */
#endif
};

extern void hipe_arch_print_pcb(struct hipe_process_state *p);

static __inline__ void hipe_init_process(struct hipe_process_state *p)
{
    p->nsp = NULL;
    p->nstack = NULL;
    p->nstend = NULL;
    p->nstgraylim = NULL;
    p->nstblacklim = NULL;
    p->ngra = NULL;
#if defined(__sparc__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    p->nra = NULL;
#endif
#if defined(__i386__) || defined(__x86_64__) || defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__) || defined(__arm__)
    p->narity = 0;
#endif
}

static __inline__ void hipe_delete_process(struct hipe_process_state *p)
{
    if( p->nstack )
	erts_free(ERTS_ALC_T_HIPE, (void*)p->nstack);
}

#ifdef ERTS_SMP
struct hipe_process_state_smp {
    int have_receive_locks;
};

static __inline__ void hipe_init_process_smp(struct hipe_process_state_smp *p)
{
    p->have_receive_locks = 0;
}
#endif

#endif /* HIPE_PROCESS_H */
