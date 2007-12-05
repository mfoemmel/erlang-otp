/* $Id$
 */
#ifndef HIPE_PPC_GLUE_H
#define HIPE_PPC_GLUE_H

#include "hipe_ppc_asm.h"	/* for NR_ARG_REGS, PPC_LEAF_WORDS */

/* Emulated code recursively calls native code.
   The return address is `nbif_return', which is exported so that
   tailcalls from native to emulated code can be identified. */
extern unsigned int hipe_ppc_call_to_native(Process*);
AEXTERN(void,nbif_return,(void));

/* Native-mode stubs for calling emulated-mode closures. */
AEXTERN(void,nbif_ccallemu0,(void));
AEXTERN(void,nbif_ccallemu1,(void));
AEXTERN(void,nbif_ccallemu2,(void));
AEXTERN(void,nbif_ccallemu3,(void));
AEXTERN(void,nbif_ccallemu4,(void));
AEXTERN(void,nbif_ccallemu5,(void));
AEXTERN(void,nbif_ccallemu6,(void));

/* Default exception handler for native code. */
AEXTERN(void,nbif_fail,(void));

/* Emulated code returns to its native code caller. */
extern unsigned int hipe_ppc_return_to_native(Process*);

/* Emulated code tailcalls native code. */
extern unsigned int hipe_ppc_tailcall_to_native(Process*);

/* Emulated code throws an exception to its native code caller. */
extern unsigned int hipe_ppc_throw_to_native(Process*);

static __inline__ unsigned int max(unsigned int x, unsigned int y)
{
    return (x > y) ? x : y;
}

static __inline__ void hipe_arch_glue_init(void)
{
    static struct sdesc_with_exnra nbif_return_sdesc = {
	.exnra = (unsigned long)&nbif_fail,
	.sdesc = {
	    .bucket = { .hvalue = (unsigned long)&nbif_return },
	    .summary = (1<<8),
	},
    };
    hipe_init_sdesc_table(&nbif_return_sdesc.sdesc);
}

static __inline__ void hipe_push_ppc_nra_frame(Process *p)
{
    p->hipe.nsp -= 1;
    p->hipe.nsp[0] = (Eterm)p->hipe.nra;
}

static __inline__ void hipe_pop_ppc_nra_frame(Process *p)
{
    p->hipe.nra = (void(*)(void))p->hipe.nsp[0];
    p->hipe.nsp += 1;
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_write_ppc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for(i = arity; --i >= 0;)
	p->def_arg_reg[i] = reg[i];
#endif
}

/* PRE: arity <= NR_ARG_REGS */
static __inline__ void
hipe_read_ppc_regs(Process *p, unsigned int arity, Eterm reg[])
{
#if NR_ARG_REGS > 0
    int i;
    for(i = arity; --i >= 0;)
	reg[i] = p->def_arg_reg[i];
#endif
}

static __inline__ void
hipe_push_ppc_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	i = NR_ARG_REGS;
	do {
	    *--nsp = reg[i++];
	} while (i < arity);
	p->hipe.nsp = nsp;
	i = NR_ARG_REGS;
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_write_ppc_regs(p, i, reg);
}

static __inline__ void
hipe_pop_ppc_params(Process *p, unsigned int arity, Eterm reg[])
{
    unsigned int i;

    i = arity;
    if (i > NR_ARG_REGS) {
	Eterm *nsp = p->hipe.nsp;
	do {
	    reg[--i] = *nsp++;
	} while (i > NR_ARG_REGS);
	p->hipe.nsp = nsp;
	/* INV: i == NR_ARG_REGS */
    }
    /* INV: i <= NR_ARG_REGS */
    hipe_read_ppc_regs(p, i, reg);
}

/* BEAM recursively calls native code. */
static __inline__ unsigned int
hipe_call_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, max(nstkargs + 1, PPC_LEAF_WORDS));
    hipe_push_ppc_nra_frame(p);			/* needs 1 word */
    hipe_push_ppc_params(p, arity, reg);	/* needs nstkargs words */
    return hipe_ppc_call_to_native(p);
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned int
hipe_tailcall_to_native(Process *p, unsigned int arity, Eterm reg[])
{
    int nstkargs;

    if ((nstkargs = arity - NR_ARG_REGS) < 0)
	nstkargs = 0;
    hipe_check_nstack(p, max(nstkargs, PPC_LEAF_WORDS));
    hipe_push_ppc_params(p, arity, reg);	/* needs nstkargs words */
    return hipe_ppc_tailcall_to_native(p);
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p)
{
    hipe_pop_ppc_nra_frame(p);
}

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p)
{
    hipe_pop_ppc_nra_frame(p);
}

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    hipe_pop_ppc_params(p, p->arity, reg);
    if (p->hipe.nra != (void(*)(void))&nbif_return)
	return 1;
    hipe_pop_ppc_nra_frame(p);
    return 0;
}

/* Native makes a call which needs to unload the parameters.
   This differs from hipe_call_from_native_is_recursive() in
   that it doesn't check for or pop the BEAM-calls-native frame.
   It's currently only used in the implementation of apply. */
static __inline__ void
hipe_pop_params(Process *p, unsigned int arity, Eterm reg[])
{
    hipe_pop_ppc_params(p, arity, reg);
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned int hipe_return_to_native(Process *p)
{
    return hipe_ppc_return_to_native(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned int hipe_throw_to_native(Process *p)
{
    return hipe_ppc_throw_to_native(p);
}

/* Native called a BIF which failed with RESCHEDULE.
   Move the parameters to a safe place. */
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
static __inline__ unsigned int
hipe_reschedule_to_native(Process *p, unsigned arity, Eterm reg[])
{
#if NR_ARG_REGS == 0
    ASSERT(arity == 0);
    return hipe_ppc_tailcall_to_native(p);
#else
    p->arity = 0;
    return hipe_tailcall_to_native(p, arity, reg);
#endif
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ const void *hipe_closure_stub_address(unsigned int arity)
{
#if NR_ARG_REGS == 0
    return &nbif_ccallemu0;
#else	/* > 0 */
    switch (arity) {
      case 0:	return &nbif_ccallemu0;
#if NR_ARG_REGS == 1
      default:	return &nbif_ccallemu1;
#else	/* > 1 */
      case 1:	return &nbif_ccallemu1;
#if NR_ARG_REGS == 2
      default:	return &nbif_ccallemu2;
#else	/* > 2 */
      case 2:	return &nbif_ccallemu2;
#if NR_ARG_REGS == 3
      default:	return &nbif_ccallemu3;
#else	/* > 3 */
      case 3:	return &nbif_ccallemu3;
#if NR_ARG_REGS == 4
      default:	return &nbif_ccallemu4;
#else	/* > 4 */
      case 4:	return &nbif_ccallemu4;
#if NR_ARG_REGS == 5
      default:	return &nbif_ccallemu5;
#else	/* > 5 */
      case 5:	return &nbif_ccallemu5;
#if NR_ARG_REGS == 6
      default:	return &nbif_ccallemu6;
#else
#error "NR_ARG_REGS > 6 NOT YET IMPLEMENTED"
#endif	/* > 6 */
#endif	/* > 5 */
#endif	/* > 4 */
#endif	/* > 3 */
#endif	/* > 2 */
#endif	/* > 1 */
    }
#endif	/* > 0 */
}

#endif /* HIPE_PPC_GLUE_H */
