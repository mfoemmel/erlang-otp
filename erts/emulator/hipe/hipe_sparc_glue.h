/* $Id$
 * hipe_sparc_glue.h
 */
#ifndef HIPE_SPARC_GLUE_H
#define HIPE_SPARC_GLUE_H

#include "hipe_sparc_asm.h"	/* for NR_ARG_REGS and LEAF_WORDS */

/* tell hipe_mode_switch.c how many argument registers we use */
#define NR_ARG_REGS	SPARC_NR_ARG_REGS

/* Emulated code recursively calls native code.
   The return address is `nbif_return', which is exported so that
   tailcalls from native to emulated code can be identified. */
extern unsigned int sparc_call_to_native(Process*);
extern void nbif_return(void);

/* Native-mode stubs for calling emulated-mode closures. */
extern void nbif_ccallemu0(void);
extern void nbif_ccallemu1(void);
extern void nbif_ccallemu2(void);
extern void nbif_ccallemu3(void);
extern void nbif_ccallemu4(void);
extern void nbif_ccallemu5(void);
extern void nbif_ccallemu6(void);

/* Default exception handler for native code. */
extern void nbif_fail(void);

/* Emulated code returns to its native code caller. */
extern unsigned int sparc_return_to_native(Process*);

/* Emulated code tailcalls native code. */
extern unsigned int sparc_tailcall_to_native(Process*);

/* Emulated code throws an exception to its native code caller. */
extern unsigned int sparc_throw_to_native(Process*);

static __inline__ void hipe_arch_glue_init(void)
{
    static struct sdesc_with_exnra nbif_return_sdesc = {
	.exnra = (unsigned long)&nbif_fail,
	.sdesc = {
	    .bucket = { .hvalue = (unsigned long)&nbif_return },
	    .summary = (1<<9) | (1<<8),
	},
    };
    hipe_init_sdesc_table(&nbif_return_sdesc.sdesc);
}

static __inline__ void hipe_push_sparc_nra_frame(Process *p)
{
    p->hipe.nsp[0] = (Eterm)p->hipe.nra;
    p->hipe.nsp += 1;
}

static __inline__ void hipe_pop_sparc_nra_frame(Process *p)
{
    p->hipe.nra = (void(*)(void))p->hipe.nsp[-1];
    p->hipe.nsp -= 1;
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_write_sparc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for(i = arity; --i >= 0;)
	p->def_arg_reg[i] = reg[i];
#endif
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_read_sparc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for(i = arity; --i >= 0;)
	reg[i] = p->def_arg_reg[i];
#endif
}

static __inline__ void
hipe_push_sparc_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	i = NR_ARG_REGS;
	do {
	    *nsp++ = reg[i++];
	} while (i < arity);
	p->hipe.nsp = nsp;
	i = NR_ARG_REGS;
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_write_sparc_regs(p, i, reg);
}

static __inline__ void
hipe_pop_sparc_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	do {
	    reg[--i] = *--nsp;
	} while (i > NR_ARG_REGS);
	p->hipe.nsp = nsp;
	/* INV: i == NR_ARG_REGS */
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_read_sparc_regs(p, i, reg);
}

/* BEAM recursively calls native code. */
static __inline__ unsigned int
hipe_call_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, nstkargs + 1 + SPARC_LEAF_WORDS);
    hipe_push_sparc_nra_frame(p);		/* needs 1 word */
    hipe_push_sparc_params(p, arity, reg);	/* needs nstkargs words */
    return sparc_call_to_native(p);
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned int
hipe_tailcall_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, nstkargs + SPARC_LEAF_WORDS);
    hipe_push_sparc_params(p, arity, reg);	/* needs nstkargs words */
    return sparc_tailcall_to_native(p);
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p)
{
    hipe_pop_sparc_nra_frame(p);
}

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p)
{
    hipe_pop_sparc_nra_frame(p);
}

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    hipe_pop_sparc_params(p, p->arity, reg);
    if (p->hipe.nra != &nbif_return)
	return 1;
    hipe_pop_sparc_nra_frame(p);
    return 0;
}

/* Native makes a call which needs to unload the parameters.
   This differs from hipe_call_from_native_is_recursive() in
   that it doesn't check for or pop the native-to-BEAM trap frame.
   It's currently only used in the implementation of apply. */
static __inline__ void
hipe_pop_params(Process *p, unsigned int arity, Eterm reg[])
{
    hipe_pop_sparc_params(p, arity, reg);
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned int hipe_return_to_native(Process *p)
{
    return sparc_return_to_native(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned int hipe_throw_to_native(Process *p)
{
    return sparc_throw_to_native(p);
}

/* Native called a BIF which failed with RESCHEDULE.
   Move the arguments to a safe place. */
static __inline__ void hipe_reschedule_from_native(Process *p)
{
#if NR_ARG_REGS == 0
    ASSERT(p->arity == 0);
#else
    if (p->arg_reg != p->def_arg_reg) {
	unsigned int i;
	for(i = 0; i < p->arity; ++i)
	    p->arg_reg[i] = p->def_arg_reg[i];
    }
#endif
}

/* Resume a BIF call which had failed with RESCHEDULE. */
static __inline__ unsigned
hipe_reschedule_to_native(Process *p, unsigned arity, Eterm reg[])
{
#if NR_ARG_REGS == 0
    ASSERT(arity == 0);
    return sparc_tailcall_to_native(p);
#else
    p->arity = 0;
    return hipe_tailcall_to_native(p, arity, reg);
#endif
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ void *hipe_closure_stub_address(unsigned int arity)
{
    switch (arity) {
      case 0:	return nbif_ccallemu0;
      case 1:	return nbif_ccallemu1;
      case 2:	return nbif_ccallemu2;
      case 3:	return nbif_ccallemu3;
      case 4:	return nbif_ccallemu4;
      case 5:	return nbif_ccallemu5;
      default:	return nbif_ccallemu6;
    }
}

#endif /* HIPE_SPARC_GLUE_H */
