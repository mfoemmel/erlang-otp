/* $Id$
 * hipe_x86_glue.h
 */
#include "hipe_bif0.h"	/* for stack descriptor stuff */

/* Emulated code recursively calls native code.
   The return address is `nbif_return', which is exported so that
   tailcalls from native to emulated code can be identified. */
extern unsigned x86_call_to_native(Process*);
extern void nbif_return(void);

/* Native-mode stub for calling an emulated-mode closure. */
extern void nbif_closure_call_emu(void);

/* Default exception handler for native code. */
extern void nbif_fail(void);

/* Emulated code returns to its native code caller. */
extern unsigned x86_return_to_native(Process*);

/* Emulated code tailcalls native code. */
extern unsigned x86_tailcall_to_native(Process*);

/* Emulated code throws an exception to its native code caller. */
extern unsigned x86_throw_to_native(Process*);

static __inline__ void hipe_arch_glue_init(void)
{
    static struct sdesc nbif_return_sdesc = {
	.bucket = { .hvalue = (unsigned long)nbif_return },
	.altra = (unsigned long)nbif_return,
	.exnra = (unsigned long)nbif_fail,
    };

    if( (unsigned long)nbif_return & 0x3 )	/* sanity check */
	abort();
    hipe_init_sdesc_table(&nbif_return_sdesc);
}

static __inline__ void
hipe_push_x86_params(Process *p, unsigned arity, Eterm reg[])
{
    Eterm *nsp;
    unsigned i;

    nsp = p->hipe.nsp;
    for(i = 0; i < arity; ++i)
	*--nsp = reg[i];
    p->hipe.nsp = nsp;
}

static __inline__ void
hipe_pop_x86_params(Process *p, unsigned arity, Eterm reg[])
{
    Eterm *nsp;
    int i;

    nsp = p->hipe.nsp;
    for(i = arity; --i >= 0;)
	reg[i] = *nsp++;
    p->hipe.nsp = nsp;
}

/* BEAM recursively calls native code. */
static __inline__ unsigned
hipe_call_to_native(Process *p, unsigned arity, Eterm reg[])
{
    /* Note that call_to_native() needs two words on the stack:
       one for the nbif_return return address, and one for the
       callee's return address should it need to call inc_stack_0. */
    hipe_check_nstack(p, arity+1+1);
    hipe_push_x86_params(p, arity, reg);	/* needs arity words */
    return x86_call_to_native(p);		/* needs 1+1 words */
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned
hipe_tailcall_to_native(Process *p, unsigned arity, Eterm reg[])
{
    hipe_check_nstack(p, arity+1);	/* +1 so callee can call inc_stack_0 */
    if( arity ) {
	Eterm nra;
	nra = *(p->hipe.nsp++);
	hipe_push_x86_params(p, arity, reg);
	*--(p->hipe.nsp) = nra;
    }
    return x86_tailcall_to_native(p);
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p) { }

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p) { }

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    Eterm nra;

    nra = *(p->hipe.nsp++);
    hipe_pop_x86_params(p, p->arity, reg);
    if( nra != (Eterm)nbif_return ) {
	*--(p->hipe.nsp) = nra;
	return 1;
    }
    return 0;
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned hipe_return_to_native(Process *p)
{
    return x86_return_to_native(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned hipe_throw_to_native(Process *p)
{
    return x86_throw_to_native(p);
}

/* Native called a BIF which failed with RESCHEDULE.
   Move the parameters to a safe place. */
static __inline__ void hipe_reschedule_from_native(Process *p)
{
    ASSERT(p->arity == 0);
}

/* Resume a BIF call which had failed with RESCHEDULE. */
static __inline__ unsigned
hipe_reschedule_to_native(Process *p, unsigned arity, Eterm reg[])
{
    ASSERT(arity == 0);
    return x86_tailcall_to_native(p);
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ void *hipe_closure_stub_address(unsigned arity)
{
    return nbif_closure_call_emu;
}
