/* $Id$
 * hipe_mode_switch.c
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "beam_load.h"	/* which includes beam_opcodes.h */
#include "beam_catches.h"
#include "hipe_mode_switch.h"
#include "bif.h"
#include "error.h"
#include "hipe_stack.h"
#include "hipe_bif0.h"	/* hipe_mfa_info_table_init() */

/*
 * Internal debug support.
 * #define HIPE_DEBUG to the desired debug level:
 *	0	no checks
 *	1	check PCB consistency at mode-switches
 *	2	log commands and results at mode-switches
 *	3	log commands, results, and PCB contents at mode-switches
 *
 * TODO: check PCB consistency at native BIF calls
 */
int hipe_modeswitch_debug = 0;

#define HIPE_DEBUG 0

#if HIPE_DEBUG > 1	/* include DPRINTF() logging */

#define DPRINTF(fmt, args...) \
do { \
    if( hipe_modeswitch_debug > 0 ) { \
	printf("%s, line %u: " fmt "\r\n", __FUNCTION__, __LINE__ , ##args); \
	fflush(stdout); \
    } \
} while( 0 )

static const char *code_str(unsigned code)
{
    static const char *cmd_str[] = {
	"call from beam",
	"return from beam",
	"throw from beam",
	"resume from beam",
	"reschedule from beam",
	"return to beam",
	"call to beam",
	"throw to beam",
	"suspend to beam",
	"wait from native",
	"wait_timeout from native",
	"reschedule from native",
	"trap from native",
	"call closure from beam",
	"call closure to beam",
    };
    unsigned cmd = code & 0xFF;

    if( cmd < (sizeof(cmd_str)/sizeof(cmd_str[0])) )
	return cmd_str[cmd];
    else
	return "???";
}

#else	/* HIPE_DEBUG > 1 */

#define DPRINTF(fmt, args...)	do{}while(0)

#endif	/* HIPE_DEBUG > 1 */

#if HIPE_DEBUG > 0	/* include HIPE_ASSERT and PCB checking */

static void __noreturn
hipe_abort(const char *expr, const char *file, unsigned line)
{
    erl_exit(1, "ASSERTION FAILED, file %s, line %u: %s\r\n", file, line, expr);
}

#define HIPE_ASSERT3(expr,file,line) \
do { \
    if( !(expr) ) \
	hipe_abort(#expr, file, line); \
} while( 0 )
#define HIPE_ASSERT(expr)	HIPE_ASSERT3(expr,__FILE__,__LINE__)

void hipe_check_pcb(Process *p, const char *file, unsigned line)
{
#if HIPE_DEBUG > 2
    if( hipe_modeswitch_debug > 0 ) {
        printf("%s, line %u: p %p = {htop %p, stop %p, nstack %p, nsp %p, nstend %p}\r\n", file, line, p, p->htop, p->stop, p->hipe.nstack, p->hipe.nsp, p->hipe.nstend);
    }
#endif
    HIPE_ASSERT3(p != NULL, file, line);
#ifdef SHARED_HEAP
    HIPE_ASSERT3(p->htop <= p->hend, file, line);
    HIPE_ASSERT3(p->stop >= p->send, file, line);
#else
    HIPE_ASSERT3(p->htop <= p->stop, file, line);
#endif
    HIPE_ASSERT3(p->hipe.nstack <= p->hipe.nstend, file, line);
    HIPE_ASSERT3(p->hipe.nsp >= p->hipe.nstack, file, line);
    HIPE_ASSERT3(p->hipe.nsp <= p->hipe.nstend, file, line);
}
#define HIPE_CHECK_PCB(P)	hipe_check_pcb((P),__FILE__,__LINE__)

#else	/* HIPE_DEBUG > 0 */

#define HIPE_ASSERT(expr)	do{}while(0)
#define HIPE_CHECK_PCB(P)	do{}while(0)

#endif	/* HIPE_DEBUG > 0 */

/* ensure that at least nwords words are available on the native stack */
static void hipe_check_nstack(Process *p, unsigned nwords);

#if defined(__sparc__)
#include "hipe_sparc_glue.h"
#elif defined(__i386__)
#include "hipe_x86_glue.h"
#elif defined(__x86_64__)
#include "hipe_amd64_glue.h"
#elif defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_glue.h"
#endif

#define BeamOpCode(Op)		((Uint)BeamOp(Op))

Uint hipe_beam_pc_return[1];	/* needed in hipe_debug.c */
Uint hipe_beam_pc_throw[1];	/* needed in hipe_debug.c */
Uint hipe_beam_pc_resume[1];	/* needed by hipe_set_timeout() */
static Uint hipe_beam_pc_reschedule[1];
static Eterm hipe_beam_catch_throw;

void hipe_mode_switch_init(void)
{
    hipe_arch_glue_init();

    hipe_beam_pc_return[0] = BeamOpCode(op_hipe_trap_return);
    hipe_beam_pc_throw[0] = BeamOpCode(op_hipe_trap_throw);
    hipe_beam_pc_resume[0] = BeamOpCode(op_hipe_trap_resume);
    hipe_beam_pc_reschedule[0] = BeamOpCode(op_hipe_trap_reschedule);

    hipe_beam_catch_throw =
	make_catch(beam_catches_cons(hipe_beam_pc_throw, BEAM_CATCHES_NIL));

    hipe_mfa_info_table_init();
}

void hipe_set_call_trap(Uint *bfun, void *nfun, int is_closure)
{
    HIPE_ASSERT(bfun[-5] == BeamOpCode(op_i_func_info_IaaI));
    bfun[0] =
	is_closure
	? BeamOpCode(op_hipe_trap_call_closure)
	: BeamOpCode(op_hipe_trap_call);
    bfun[-4] = (Uint)nfun;
}

static __inline__ void
hipe_push_beam_trap_frame(Process *p, Eterm reg[], unsigned arity)
{
    /* ensure that at least 2 words are available on the BEAM stack */
#ifdef SHARED_HEAP
    if (p->stop - 2 < p->send) {
      int used_stack = p->stack - p->stop;
      int new_sz = erts_next_heap_size(p->stack_sz + 2, 0);
      Eterm *new_stack =
        (Eterm*) erts_alloc(ERTS_ALC_T_STACK, sizeof(Eterm) * new_sz);
      DPRINTF("Enlarging stack. This is untested in unified heap!");
      sys_memmove((new_stack + new_sz) - used_stack, p->stop,
                  used_stack * sizeof(Eterm));
#ifdef DEBUG
      sys_memset(p->send, 0xff, p->stack_sz * sizeof(Eterm));
#endif
      erts_free(ERTS_ALC_T_STACK, (void *) p->send);
      p->stack_sz = new_sz;
      p->send = new_stack;
      p->stop = new_stack + new_sz - used_stack;
      p->stack = new_stack + new_sz;
    }
#else
    if( (p->stop - 2) < p->htop ) {
	DPRINTF("calling gc to increase BEAM stack size");
	p->fcalls -= erts_garbage_collect(p, 2, reg, arity);
    }
#endif
    p->stop -= 2;
    p->stop[1] = hipe_beam_catch_throw;
    p->stop[0] = make_cp(p->cp);
    ++p->catches;
    p->cp = hipe_beam_pc_return;
}

static __inline__ void hipe_pop_beam_trap_frame(Process *p)
{
    p->cp = cp_val(p->stop[0]);
    --p->catches;
    p->stop += 2;
}

Process *hipe_mode_switch(Process *p, unsigned cmd, Eterm reg[])
{
    unsigned result;
#if NR_ARG_REGS > 5
    /* When NR_ARG_REGS > 5, we need to protect the process' input
       reduction count (which BEAM stores in def_arg_reg[5]) from
       being clobbered by the arch glue code. */
    Eterm reds_in = p->def_arg_reg[5];
#endif
#if NR_ARG_REGS > 4
    Eterm o_reds = p->def_arg_reg[4];
#endif

    p->i = NULL;

    DPRINTF("cmd == %#x (%s)", cmd, code_str(cmd));
    HIPE_CHECK_PCB(p);
    switch( cmd & 0xFF ) {
      case HIPE_MODE_SWITCH_CMD_RESCHEDULE:
	break;
      default:
	p->arity = 0;
    }
    switch( cmd & 0xFF ) {
      case HIPE_MODE_SWITCH_CMD_CALL: {
	  /* BEAM calls a native code function */
	  unsigned arity = cmd >> 8;

	  /* p->hipe.ncallee set in beam_emu */
	  if( p->cp == hipe_beam_pc_return ) {
	    /* Native called BEAM, which now tailcalls native. */
	    hipe_pop_beam_trap_frame(p);
	    result = hipe_tailcall_to_native(p, arity, reg);
	    break;
	  }
	  DPRINTF("calling %#lx/%u", (long)p->hipe.ncallee, arity);
	  result = hipe_call_to_native(p, arity, reg);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_CALL_CLOSURE: {
	  /* BEAM calls a native code closure */
	  unsigned arity = cmd >> 8; /* #formals + #fvs (closure not counted) */
	  Eterm fun;
	  ErlFunThing *funp;

	  /* drop the fvs, move the closure, correct arity */
	  fun = reg[arity];
	  HIPE_ASSERT(is_fun(fun));
	  funp = (ErlFunThing*)fun_val(fun);
	  HIPE_ASSERT(funp->num_free <= arity);
	  arity -= funp->num_free;	/* arity == #formals */
	  reg[arity] = fun;
	  ++arity;	/* correct for having added the closure */
	  /* HIPE_ASSERT(p->hipe.ncallee == (void(*)(void))funp->native_address); */

	  /* just like a normal call from now on */

	  /* p->hipe.ncallee set in beam_emu */
	  if( p->cp == hipe_beam_pc_return ) {
	      /* Native called BEAM, which now tailcalls native. */
	      hipe_pop_beam_trap_frame(p);
	      result = hipe_tailcall_to_native(p, arity, reg);
	      break;
	  }
	  DPRINTF("calling %#lx/%u", (long)p->hipe.ncallee, arity);
	  result = hipe_call_to_native(p, arity, reg);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_THROW: {
	  /* BEAM just executed hipe_beam_pc_throw[] */
	  /* Native called BEAM, which now throws an exception back to native. */
	  DPRINTF("beam throws freason %#lx fvalue %#lx", p->freason, p->fvalue);
	  hipe_pop_beam_trap_frame(p);
	  result = hipe_throw_to_native(p);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_RETURN: {
	  /* BEAM just executed hipe_beam_pc_return[] */
	  /* Native called BEAM, which now returns back to native. */
	  /* pop trap frame off estack */
	  hipe_pop_beam_trap_frame(p);
	  p->def_arg_reg[0] = reg[0];
	  result = hipe_return_to_native(p);
	  break;
      }
    do_resume:
      case HIPE_MODE_SWITCH_CMD_RESUME: {
	  /* BEAM just executed hipe_beam_pc_resume[] */
	  /* BEAM called native, which suspended. */
	  if( p->flags & F_TIMO ) {
	      /* XXX: The process will immediately execute 'clear_timeout',
		 repeating these two statements. Remove them? */
	      p->flags &= ~F_TIMO;
	      JOIN_MESSAGE(p);
	      p->def_arg_reg[0] = 0;	/* make_small(0)? */
	  } else {
	      p->def_arg_reg[0] = 1;	/* make_small(1)? */
	  }
	  result = hipe_return_to_native(p);
	  break;
      }
      case HIPE_MODE_SWITCH_CMD_RESCHEDULE: {
	  /* BEAM just executed hipe_beam_pc_reschedule[] */
	  /* Native called a BIF which failed with RESCHEDULE. Resume it. */
	  /* XXX: this ought to be the same as 'resume' */
	  DPRINTF("rescheduling to 0x%#lx/%u", p->hipe.ncallee, p->arity);
	  result = hipe_reschedule_to_native(p, p->arity, reg);
	  break;
      }
      default:
	erl_exit(1, "hipe_mode_switch: cmd %#x\r\n", cmd);
    }
 do_return_from_native:
    DPRINTF("result == %#x (%s)", result, code_str(result));
    HIPE_CHECK_PCB(p);
    switch( result ) {
      case HIPE_MODE_SWITCH_RES_RETURN: {
	  hipe_return_from_native(p);
	  reg[0] = p->def_arg_reg[0];
	  DPRINTF("returning with r(0) == %#lx", reg[0]);
	  break;
      }
      case HIPE_MODE_SWITCH_RES_THROW: {
	  DPRINTF("native throws freason %#lx fvalue %#lx", p->freason, p->fvalue);
	  hipe_throw_from_native(p);
	  break;
      }
      case HIPE_MODE_SWITCH_RES_TRAP: {
	  /* Native code called a BIF, which "failed" with a TRAP to BEAM.
	   *
	   * p->def_arg_reg[0] is the callee's Export*
	   * p->def_arg_reg[1..] contains the parameters
	   * p->arity is the original BIF's arity
	   * the BIF and the new callee may have different arities
	   */
	  unsigned int i, is_recursive;

	  /* Here p->arity still describes the original BIF's arity.
	     Get rid of any stacked parameters in that call. */
	  /* XXX: hipe_call_from_native_is_recursive() copies data to
	     reg[], which is useless in the TRAP case. Maybe write a
	     specialised hipe_trap_from_native_is_recursive() later. */
	  is_recursive = hipe_call_from_native_is_recursive(p, reg);

	  p->i = ((Export*)(p->def_arg_reg[0]))->address;
	  p->arity = p->i[-1];

	  for(i = 0; i < p->arity; ++i)
	      reg[i] = p->def_arg_reg[i+1];

	  if( is_recursive )
	      hipe_push_beam_trap_frame(p, reg, p->arity);

	  result = HIPE_MODE_SWITCH_RES_CALL;
	  break;
      }
      case HIPE_MODE_SWITCH_RES_CALL: {
	  /* Native code calls or tailcalls BEAM.
	   *
	   * p->i is the callee's BEAM code
	   * p->arity is the callee's arity
	   * p->def_arg_reg[] contains the register parameters
	   * p->hipe.nsp[] contains the stacked parameters
	   */
	  if( hipe_call_from_native_is_recursive(p, reg) ) {
	      /* BEAM called native, which now calls BEAM */
	      hipe_push_beam_trap_frame(p, reg, p->arity);
	  }
	  break;
      }
      case HIPE_MODE_SWITCH_RES_CALL_CLOSURE: {
	  /* Native code calls or tailcalls a closure in BEAM
	   *
	   * In native code a call to a closure of arity n looks like
	   * F(A1, ..., AN, Closure),
	   * Beam expects to get:
	   * F(A1, ..., AN, FV1, ..., FVM, Closure)
	   *  (Where Ai is argument i and FVi is free variable i)
	   *
	   * p->hipe.closure contains the closure
	   * p->def_arg_reg[] contains the parameters (on SPARC)
	   */
	  ErlFunThing *closure;
	  unsigned num_free, arity, i, is_recursive;

	  HIPE_ASSERT(is_fun(p->hipe.closure));
	  closure = (ErlFunThing*)fun_val(p->hipe.closure);
	  num_free = closure->num_free;
	  arity = closure->fe->address[-1] - num_free;

	  /* Store the arity in p->arity for the stack popping. */
	  p->arity = arity+1; /* +1 for the closure */

	  /* Get parameters, don't do GC just yet. */
	  is_recursive = hipe_call_from_native_is_recursive(p, reg);

	  /* Append the free vars to the actual parameters. */
	  for(i = 0; i < num_free; ++i)
	      reg[arity+i] = closure->env[i];
	  /* Put the closure as the last argument. */
	  reg[arity+i] = p->hipe.closure;

	  /* Make a call to the closure's BEAM code. */
	  p->i = closure->fe->address;

	  if( is_recursive ) {
	      /* BEAM called native, which now calls BEAM.
		 Need to put a trap-frame on the beam stack.
		 This may cause GC, which is safe now that
		 the arguments, free vars, and most
		 importantly the closure, all are in reg[]. */
	      hipe_push_beam_trap_frame(p, reg, arity+i);
	  }

	  result = HIPE_MODE_SWITCH_RES_CALL;
	  break;
      }
      case HIPE_MODE_SWITCH_RES_RESCHEDULE: {
	  DPRINTF("native reschedules 0x%#lx/%u\r\n", p->hipe.ncallee, p->arity);
	  hipe_reschedule_from_native(p);
	  p->i = hipe_beam_pc_reschedule;
	  goto do_schedule;
      }
      case HIPE_MODE_SWITCH_RES_SUSPEND: {
	  p->i = hipe_beam_pc_resume;
	  p->arity = 0;
	  add_to_schedule_q(p);
	  goto do_schedule;
      }
      case HIPE_MODE_SWITCH_RES_WAIT:
      case HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT: {
	  /* same semantics, different debug trace messages */
	  p->i = hipe_beam_pc_resume;
	  p->arity = 0;
	  p->status = P_WAITING;
      do_schedule:
	  {
#if !(NR_ARG_REGS > 5)
	      int reds_in = p->def_arg_reg[5];
#endif
	      p = schedule(p, reds_in - p->fcalls);
#ifdef ERTS_SMP
	      reg = p->scheduler_data->save_reg;
#endif
	  }
	  {
	      Eterm *argp;
	      int i;
	  
	      argp = p->arg_reg;
	      for(i = p->arity; --i >= 0;)
		  reg[i] = argp[i];
	  }
	  {
#if !(NR_ARG_REGS > 5)
	      Eterm reds_in;
#endif
#if !(NR_ARG_REGS > 4)
	      Eterm o_reds;
#endif

	      reds_in = p->fcalls;
	      o_reds = 0;
	      if( p->ct != NULL ) {
		  o_reds = reds_in;
		  reds_in = 0;
		  p->fcalls = 0;
	      }
	      p->def_arg_reg[4] = o_reds;
	      p->def_arg_reg[5] = reds_in;
	      if( p->i == hipe_beam_pc_resume ) {
		  p->i = NULL;
		  p->arity = 0;
		  goto do_resume;
	      }
	  }
	  HIPE_CHECK_PCB(p);
	  result = HIPE_MODE_SWITCH_RES_CALL;
	  p->def_arg_reg[3] = result;
	  return p;
      }
      case HIPE_MODE_SWITCH_RES_APPLY: {
	  Eterm mfa[3], args;
	  unsigned int arity;
	  void *address;

	  hipe_pop_params(p, 3, &mfa[0]);

	  /* Unroll the arglist onto reg[]. */
	  args = mfa[2];
	  arity = 0;
	  while( is_list(args) ) {
	      if( arity < 255 ) {
		  reg[arity++] = CAR(list_val(args));
		  args = CDR(list_val(args));
	      } else
		  goto do_apply_fail;
	  }
	  if( is_not_nil(args) )
	      goto do_apply_fail;

	  /* find a native code entry point for {M,F,A} for a remote call */
	  address = hipe_get_remote_na(mfa[0], mfa[1], arity);
	  if (!address)
		  goto do_apply_fail;
	  p->hipe.ncallee = (void(*)(void)) address;
	  result = hipe_tailcall_to_native(p, arity, reg);
	  goto do_return_from_native;
      do_apply_fail:
	  p->freason = BADARG;
	  result = hipe_throw_to_native(p);
	  goto do_return_from_native;
      }
      default:
	erl_exit(1, "hipe_mode_switch: result %#x\r\n", result);
    }
    HIPE_CHECK_PCB(p);
    p->def_arg_reg[3] = result;
#if NR_ARG_REGS > 4
    p->def_arg_reg[4] = o_reds;
#endif
#if NR_ARG_REGS > 5
    p->def_arg_reg[5] = reds_in;
#endif
    return p;
}

#define HIPE_INITIAL_NSTACK_SIZE	128

/* PRE: size is zero or a power of two */
static unsigned hipe_next_nstack_size(unsigned size)
{
    return size ? size * 2 : HIPE_INITIAL_NSTACK_SIZE;
}

#ifdef HIPE_NSTACK_GROWS_UP
#define hipe_nstack_avail(p)	((p)->hipe.nstend - (p)->hipe.nsp)
void hipe_inc_nstack(Process *p)
{
    Eterm *old_nstack = p->hipe.nstack;
    unsigned old_size = p->hipe.nstend - old_nstack;
    unsigned new_size = hipe_next_nstack_size(old_size);
    Eterm *new_nstack = erts_realloc(ERTS_ALC_T_HIPE,
				     (char *) old_nstack,
				     new_size*sizeof(Eterm));
    p->hipe.nstend = new_nstack + new_size;
    if( new_nstack != old_nstack ) {
	p->hipe.nsp = new_nstack + (p->hipe.nsp - old_nstack);
	p->hipe.nstack = new_nstack;
	if( p->hipe.nstgraylim )
	    p->hipe.nstgraylim = 
		new_nstack + (p->hipe.nstgraylim - old_nstack);
	if( p->hipe.nstblacklim )
	    p->hipe.nstblacklim = 
		new_nstack + (p->hipe.nstblacklim - old_nstack);
    }
}
#endif

#ifdef HIPE_NSTACK_GROWS_DOWN
#define hipe_nstack_avail(p)	((unsigned)((p)->hipe.nsp - (p)->hipe.nstack))
void hipe_inc_nstack(Process *p)
{
    unsigned old_size = p->hipe.nstend - p->hipe.nstack;
    unsigned new_size = hipe_next_nstack_size(old_size);
    Eterm *new_nstack = erts_alloc(ERTS_ALC_T_HIPE, new_size*sizeof(Eterm));
    unsigned used_size = p->hipe.nstend - p->hipe.nsp;

    sys_memcpy(new_nstack+new_size-used_size, p->hipe.nsp, used_size*sizeof(Eterm));
    if( p->hipe.nstgraylim )
	p->hipe.nstgraylim = new_nstack + new_size - (p->hipe.nstend - p->hipe.nstgraylim);
    if( p->hipe.nstblacklim )
	p->hipe.nstblacklim = new_nstack + new_size - (p->hipe.nstend - p->hipe.nstblacklim);
    if( p->hipe.nstack )
	erts_free(ERTS_ALC_T_HIPE, p->hipe.nstack);
    p->hipe.nstack = new_nstack;
    p->hipe.nstend = new_nstack + new_size;
    p->hipe.nsp = new_nstack + new_size - used_size;
}
#endif

static void hipe_check_nstack(Process *p, unsigned nwords)
{
    while( hipe_nstack_avail(p) < nwords )
	hipe_inc_nstack(p);
}

void hipe_set_closure_stub(ErlFunEntry *fe, unsigned num_free)
{
    unsigned arity;

    arity = fe->address[-1] - num_free;
    fe->native_address = (Eterm*) hipe_closure_stub_address(arity);
}
