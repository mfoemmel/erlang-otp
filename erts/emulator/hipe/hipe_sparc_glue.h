/* $Id$
 * hipe_sparc_glue.h
 */

extern unsigned sparc_call_to_native(Process*);
extern unsigned sparc_return_to_native(Process*);
extern unsigned sparc_tailcall_to_native(Process*);
extern unsigned sparc_throw_to_native(Process*);
extern void nbif_fail(void);
extern void nbif_return(void);
extern void nbif_ccallemu0(void);
extern void nbif_ccallemu1(void);
extern void nbif_ccallemu2(void);
extern void nbif_ccallemu3(void);
extern void nbif_ccallemu4(void);
extern void nbif_ccallemu5(void);

Eterm sparc_catch_fail;

static __inline__ void hipe_arch_glue_init(void)
{
    unsigned i;

    i = beam_catches_cons((uint32*)nbif_fail, BEAM_CATCHES_NIL);
    sparc_catch_fail = make_catch(i);
}

static __inline__ void hipe_push_sparc_trap_frame(Process *p)
{
    p->hipe.nsp[0] = (Eterm)p->hipe.nra;
    p->hipe.nsp[1] = sparc_catch_fail;
    p->hipe.nsp += 2;
}

static __inline__ void hipe_pop_sparc_trap_frame(Process *p)
{
    ASSERT(p->hipe.nsp[-1] == sparc_catch_fail);
    p->hipe.nra = (void(*)(void))p->hipe.nsp[-2];
    p->hipe.nsp -= 2;
}

static __inline__ void
hipe_push_sparc_params(Process *p, unsigned arity, Eterm reg[])
{
    unsigned i;

    for(i = 0; i < arity && i < HIPE_ARGS_IN_REGS; ++i)
	p->def_arg_reg[i] = reg[i];
    for(; i < arity; ++i)
	p->hipe.nsp[i - HIPE_ARGS_IN_REGS] = reg[i];
    if( arity > HIPE_ARGS_IN_REGS )
	p->hipe.nsp += arity - HIPE_ARGS_IN_REGS;
}

static __inline__ void
hipe_pop_sparc_params(Process *p, unsigned arity, Eterm reg[])
{
    unsigned i;

    for(i = 0; i < arity && i < HIPE_ARGS_IN_REGS; ++i)
	reg[i] = p->def_arg_reg[i];
    for(; i < arity; ++i)
	reg[i] = p->hipe.nsp[-(arity - i)];
    if( arity > HIPE_ARGS_IN_REGS )
	p->hipe.nsp -= arity - HIPE_ARGS_IN_REGS;
}

/* BEAM recursively calls native code. */
static __inline__ unsigned
hipe_call_to_native(Process *p, unsigned arity, Eterm reg[])
{
    unsigned nstkargs = arity <= HIPE_ARGS_IN_REGS ? 0 : arity-HIPE_ARGS_IN_REGS;
    hipe_check_nstack(p, 2 + nstkargs + HIPE_SPARC_LEAF_WORDS);
    hipe_push_sparc_trap_frame(p);		/* pushes 2 words */
    hipe_push_sparc_params(p, arity, reg);	/* pushes nstkargs words */
    return sparc_call_to_native(p);		/* guaranteed LEAF words */
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned
hipe_tailcall_to_native(Process *p, unsigned arity, Eterm reg[])
{
    unsigned nstkargs = arity <= HIPE_ARGS_IN_REGS ? 0 : arity-HIPE_ARGS_IN_REGS;
    hipe_check_nstack(p, nstkargs + HIPE_SPARC_LEAF_WORDS);
    hipe_push_sparc_params(p, arity, reg);	/* pushes nstkargs words */
    return sparc_tailcall_to_native(p);		/* guaranteed LEAF words */
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p)
{
    hipe_pop_sparc_trap_frame(p);
}

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p)
{
    /* p->hipe.nsp[0] contained sparc_catch_fail */
    p->hipe.nra = (void(*)(void))p->hipe.nsp[-1];
    p->hipe.nsp -= 1;
}

/* BEAM called native, which now calls BEAM.
   Move the parameters to reg[].
   Return zero if this is a tailcall, non-zero if the call is recursive.
   If tailcall, also clean up native stub continuation. */
static __inline__ int
hipe_call_from_native_is_recursive(Process *p, Eterm reg[])
{
    hipe_pop_sparc_params(p, p->arity, reg);
    if( p->hipe.nra != nbif_return )
	return 1;
    hipe_pop_sparc_trap_frame(p);
    return 0;
}

/* Native called BEAM, which now returns back to native. */
static __inline__ unsigned hipe_return_to_native(Process *p)
{
    return sparc_return_to_native(p);
}

/* Native called BEAM, which now throws an exception back to native. */
static __inline__ unsigned hipe_throw_to_native(Process *p)
{
    return sparc_throw_to_native(p);
}

/* Native called a BIF which failed with RESCHEDULE.
   Move the arguments to a safe place. */
static __inline__ void hipe_reschedule_from_native(Process *p)
{
    if( p->arg_reg != p->def_arg_reg ) {
	unsigned i;
	for(i = 0; i < p->arity; ++i)
	    p->arg_reg[i] = p->def_arg_reg[i];
    }
}

/* Resume a BIF call which had failed with RESCHEDULE. */
static __inline__ unsigned
hipe_reschedule_to_native(Process *p, unsigned arity, Eterm reg[])
{
    p->arity = 0;
    return hipe_tailcall_to_native(p, arity, reg);
}

/* Return the address of a stub switching a native closure call to BEAM. */
static __inline__ void *hipe_closure_stub_address(unsigned arity)
{
    switch( arity ) {
      case 0:	return nbif_ccallemu0;
      case 1:	return nbif_ccallemu1;
      case 2:	return nbif_ccallemu2;
      case 3:	return nbif_ccallemu3;
      case 4:	return nbif_ccallemu4;
      default:	return nbif_ccallemu5;
    }
}
