/* $Id$
 * hipe_sparc_glue.h
 */

#include "hipe_bif0.h"	/* for stack descriptor stuff */


extern unsigned sparc_call_to_native(Process*);
extern unsigned sparc_large_call_to_native(Process*);
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
extern void nbif_ccallemu6(void);
extern void nbif_ccallemu7(void);
extern void nbif_ccallemu8(void);
extern void nbif_ccallemu9(void);
extern void nbif_ccallemu10(void);
extern void nbif_ccallemu11(void);
extern void nbif_ccallemu12(void);
extern void nbif_ccallemu13(void);
extern void nbif_ccallemu14(void);
extern void nbif_ccallemu15(void);
extern void nbif_ccallemu16(void);
extern void hipe_stack_marker_ra(void);


static __inline__ void hipe_arch_glue_init(void)
{
    static struct sdesc_with_exnra nbif_return_sdesc = {
	.exnra = (unsigned long)nbif_fail,
	.sdesc = {
	    .bucket = { .hvalue = (unsigned long)nbif_return },
	    .summary = (1<<9) | (1<<8),
	},
    };

    hipe_init_sdesc_table(&nbif_return_sdesc.sdesc);
}

static __inline__ void hipe_push_sparc_trap_frame(Process *p)
{
    p->hipe.nsp[0] = (Eterm)p->hipe.nra;
    p->hipe.nsp += 1;
}

static __inline__ void hipe_pop_sparc_trap_frame(Process *p)
{
    p->hipe.nra = (void(*)(void))p->hipe.nsp[-1];
    p->hipe.nsp -= 1;
}

/* BEAM called native, which has thrown an exception. Clean up. */
static __inline__ void hipe_throw_from_native(Process *p)
{
    p->hipe.nra = (void(*)(void))p->hipe.nsp[-1];
    p->hipe.nsp -= 1;    
}

#define HIPE_CATCH_SIZE 1



static __inline__ void
hipe_push_sparc_params(Process *p, unsigned arity, Eterm reg[])
{
    unsigned i;

    for(i = 0; i < arity && i < HIPE_SPARC_ARGS_IN_REGS; ++i)
	p->def_arg_reg[i] = reg[i];
    for(; i < arity; ++i)
	p->hipe.nsp[i - HIPE_SPARC_ARGS_IN_REGS] = reg[i];
    if( arity > HIPE_SPARC_ARGS_IN_REGS )
	p->hipe.nsp += arity - HIPE_SPARC_ARGS_IN_REGS;
}

static __inline__ void
hipe_pop_sparc_params(Process *p, unsigned arity, Eterm reg[])
{
    unsigned i;

    for(i = 0; i < arity && i < HIPE_SPARC_ARGS_IN_REGS; ++i)
	reg[i] = p->def_arg_reg[i];
    for(; i < arity; ++i)
	reg[i] = p->hipe.nsp[-(arity - i)];
    if( arity > HIPE_SPARC_ARGS_IN_REGS )
	p->hipe.nsp -= arity - HIPE_SPARC_ARGS_IN_REGS;
}

/* BEAM recursively calls native code. */
static __inline__ unsigned
hipe_call_to_native(Process *p, unsigned arity, Eterm reg[])
{
    unsigned nstkargs = arity <= HIPE_SPARC_ARGS_IN_REGS ? 0 : arity-HIPE_SPARC_ARGS_IN_REGS;
    hipe_check_nstack(p, HIPE_CATCH_SIZE + nstkargs + HIPE_SPARC_LEAF_WORDS);
    hipe_push_sparc_trap_frame(p);		/* pushes HIPE_CATCH_SIZE words */
    hipe_push_sparc_params(p, arity, reg);	/* pushes nstkargs words */
                                                /* guaranteed LEAF words */
    if (arity > 3)
      return sparc_large_call_to_native(p);     /* Get all arguments */  
    else
      return sparc_call_to_native(p);		/* Only read 3 argsuments */
}

/* Native called BEAM, which now tailcalls native. */
static __inline__ unsigned
hipe_tailcall_to_native(Process *p, unsigned arity, Eterm reg[])
{
    unsigned nstkargs = arity <= HIPE_SPARC_ARGS_IN_REGS ? 0 : arity-HIPE_SPARC_ARGS_IN_REGS;
    hipe_check_nstack(p, nstkargs + HIPE_SPARC_LEAF_WORDS);
    hipe_push_sparc_params(p, arity, reg);	/* pushes nstkargs words */
    return sparc_tailcall_to_native(p);		/* guaranteed LEAF words */
}

/* BEAM called native, which has returned. Clean up. */
static __inline__ void hipe_return_from_native(Process *p)
{
    hipe_pop_sparc_trap_frame(p);
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
      case 5:	return nbif_ccallemu5;
      case 6:	return nbif_ccallemu6;
      case 7:	return nbif_ccallemu7;
      case 8:	return nbif_ccallemu8;
      case 9:	return nbif_ccallemu9;
      case 10:	return nbif_ccallemu10;
      case 11:	return nbif_ccallemu11;
      case 12:	return nbif_ccallemu12;
      case 13:	return nbif_ccallemu13;
      case 14:	return nbif_ccallemu14;
      case 15:	return nbif_ccallemu15;
      default:	return nbif_ccallemu16;
    }
}
