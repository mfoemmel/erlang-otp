/* $Id$
 * HiPE-specific process fields
 */
#ifndef HIPE_PROCESS_H
#define HIPE_PROCESS_H

struct hipe_process_state {
    Eterm *nsp;			/* Native stack pointer. */
    Eterm *nstack;		/* Native stack block start. */
    Eterm *nstend;		/* Native stack block end (start+size). */
    void (*ncallee)(void);	/* Native code callee (label) to invoke. */
    Eterm closure;		/* Used to pass a closure from native code. */
#if defined(__sparc__)
#define HIPE_ARGS_IN_REGS 5	/* Stored in p->def_arg_reg[]. */
    void (*nra)(void);		/* Native Return Address == where to resume. */
    void (*ncra)(void);		/* C return address for native code. */
#endif
#if defined(__i386__)
    Eterm *ncsp;		/* Saved C stack pointer. */
    unsigned narity;
#endif
};

/* Guaranteed min stack size for leaf functions on SPARC. */
#define HIPE_SPARC_LEAF_WORDS 20

static __inline__ void hipe_init_process(struct hipe_process_state *p)
{
    p->nsp = NULL;
    p->nstack = NULL;
    p->nstend = NULL;
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
	sys_free((void*)p->nstack);
}

#endif /* HIPE_PROCESS_H */
