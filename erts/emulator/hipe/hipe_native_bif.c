/* $Id$
 * hipe_native_bif.c
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_arch.h"
#include "hipe_stack.h"

/*
 * This is called when inlined heap allocation in native code fails.
 * The 'need' parameter is the number of heap words needed.
 * The value is tagged as a fixnum to avoid untagged data on
 * the x86 stack while the gc is running.
 */
void hipe_gc(Process *p, Eterm need)
{
    hipe_set_narity(p, 1);
    p->fcalls -= erts_garbage_collect(p, unsigned_val(need), NULL, 0);
    hipe_set_narity(p, 0);
}

/* This is like the OP_setTimeout JAM instruction.
 *  Transformation to the BEAM instruction wait_timeout_fs
 *  has begun.
 * XXX: BUG: native code should check return status
 */
Eterm hipe_set_timeout(Process *p, Eterm timeout_value)
{
#if !defined(ARCH_64)
    Uint time_val;
#endif
    /* XXX: This should be converted to follow BEAM conventions,
     * but that requires some compiler changes.
     *
     * In BEAM, set_timeout saves TWO CP values, and suspends.
     * p->def_arg_reg[0] and p->i are both defined and used.
     * If a message arrives, BEAM resumes at p->i.
     * If a timeout fires, BEAM resumes at p->def_arg_reg[0].
     * (See set_timer() and timeout_proc() in erl_process.c.)
     *
     * Here we set p->def_arg_reg[0] to hipe_beam_pc_resume.
     * Assuming our caller invokes suspend immediately after
     * our return, then hipe_mode_switch() will also set
     * p->i to hipe_beam_pc_resume. Thus we'll resume in the same
     * way regardless of the cause (message or timeout).
     * hipe_mode_switch() checks for F_TIMO and returns a
     * flag to native code indicating the cause.
     */

    /*
     * def_arg_reg[0] is (re)set unconditionally, in case this is the
     * 2nd/3rd/... iteration through the receive loop: in order to pass
     * a boolean flag to native code indicating timeout or new message,
     * our mode switch has to clobber def_arg_reg[0]. This is ok, but if
     * we re-suspend (because we ignored a received message) we also have
     * to reinitialise def_arg_reg[0] with the BEAM resume label.
     *
     * XXX: A better solution would be to pass two parameters to
     * set_timeout: the timeout and the on-timeout resume label.
     * We could put the resume label in def_arg_reg[1] and resume
     * at it without having to load a flag in a register and generate
     * code to test it. Requires a HiPE compiler change though.
     */
    p->def_arg_reg[0] = (Eterm) hipe_beam_pc_resume;

    /*
     * If we have already set the timer, we must NOT set it again.  Therefore,
     * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
     */
    if( p->flags & (F_INSLPQUEUE | F_TIMO) ) {
	return NIL;	/* caller had better call nbif_suspend ASAP! */
    }
    if( is_small(timeout_value) && signed_val(timeout_value) >= 0 &&
#if defined(ARCH_64)
	(unsigned_val(timeout_value) >> 32) == 0
#else
	1
#endif
	) {
	set_timer(p, unsigned_val(timeout_value));
    } else if( timeout_value == am_infinity ) {
	/* p->flags |= F_TIMO; */	/* XXX: nbif_suspend_msg_timeout */
#if !defined(ARCH_64)
    } else if( term_to_Uint(timeout_value, &time_val) ) {
	set_timer(p, time_val);
#endif
    } else {
	BIF_ERROR(p, EXC_TIMEOUT_VALUE);
    }
    return NIL;	/* caller had better call nbif_suspend ASAP! */
}

/* This is like the remove_message BEAM instruction
 */
void hipe_select_msg(Process *p)
{
    ErlMessage *msgp;

    msgp = PEEK_MESSAGE(p);
    UNLINK_MESSAGE(p, msgp);	/* decrements global 'erts_proc_tot_mem' variable */
    JOIN_MESSAGE(p);
    CANCEL_TIMER(p);		/* calls erl_cancel_timer() */
    free_message(msgp);
}

/* Saving a stacktrace from native mode. Right now, we only create a
 *  minimal struct with no fields filled in except freason. The flag
 *  EXF_NATIVE is set, so that build_stacktrace (in beam_emu.c) does not
 *  try to interpret any other field.
 */
static void
hipe_save_stacktrace(Process* c_p, Eterm args) {
    Eterm *hp;
    struct StackTrace* s;
    int sz;
    int depth = 0;    /* max depth (never negative) */

    /* Create a container for the exception data. This must be done just
       as in the save_stacktrace function in beam_emu.c */
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm)*depth
	  + sizeof(Eterm) - 1) / sizeof(Eterm);
    hp = HAlloc(c_p, 2 + 1 + sz);
    s = (struct StackTrace *) (hp + 2);
    c_p->ftrace = CONS(hp, args, make_big((Eterm *) s));
    s->header = make_pos_bignum_header(sz);

    /* All the other fields are inside the bignum */
    s->current = NULL;
    s->pc = NULL;
    s->depth = depth;

    /* Must mark this as a native-code exception. */
    s->freason = NATIVE_EXCEPTION(c_p->freason);
    return;
}

/*
 * hipe_handle_exception() is called from hipe_${ARCH}_glue.S when an
 * exception has been thrown, to expand the exception value, set the
 * stack trace, and locate the current handler.
 */
void hipe_handle_exception(Process *c_p)
{
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */
    ASSERT(c_p->freason != RESCHEDULE); /* Should have been handled earlier. */

    /*
     * Check if we have an arglist for the top level call. If so, this
     * is encoded in Value, so we have to dig out the real Value as well
     * as the Arglist.
     */
    if (c_p->freason & EXF_ARGLIST) {
	  Eterm* tp;
	  ASSERT(is_tuple(Value));
	  tp = tuple_val(Value);
	  Value = tp[1];
	  Args = tp[2];
    }

    /* If necessary, build a stacktrace object. */
    if (c_p->freason & EXF_SAVETRACE) {
        hipe_save_stacktrace(c_p, Args);
    }

    /* Get the fully expanded error term */
    Value = expand_error_value(c_p, c_p->freason, Value);

    /* Save final error term and stabilize the exception flags so no
       further expansion is done. */
    c_p->fvalue = Value;
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Synthesized to avoid having to generate code for it. */
    c_p->def_arg_reg[0] = exception_tag[GET_EXC_CLASS(c_p->freason)];
    
    hipe_find_handler(c_p);
}

/* This is duplicated from beam_emu.c for now */
static struct StackTrace *
get_trace_from_exc(Eterm exc) {
    if (exc == NIL) {
      return NULL;
    } else {
      return (struct StackTrace *) big_val(CDR(list_val(exc)));
    }
}

/*
 * This does what the (misnamed) Beam instruction 'raise_ss' does,
 * namely, a proper re-throw of an exception that was caught by 'try'.
 */
Eterm hipe_rethrow(Process *c_p, Eterm exc, Eterm value)
{
     c_p->fvalue = value;
     if (c_p->freason == EXC_NULL) {
       /* a safety check for the R10-0 case; should not happen */
       c_p->ftrace = NIL;
       BIF_ERROR(c_p, EXC_ERROR);
     }
     /* For R10-0 code, 'exc' might be an atom. In that case, just
	keep the existing c_p->ftrace. */
     switch (exc) {
     case am_throw:
       BIF_ERROR(c_p, (EXC_THROWN & ~EXF_SAVETRACE));
       break;
     case am_error:
       BIF_ERROR(c_p, (EXC_ERROR & ~EXF_SAVETRACE));
       break;
     case am_exit:
       BIF_ERROR(c_p, (EXC_EXIT & ~EXF_SAVETRACE));
       break;
     default:
       {/* R10-1 and later
	   XXX note: should do sanity check on given exception if it can be
	   passed from a user! Currently only expecting generated calls.
	*/
	 struct StackTrace *s;
	 c_p->ftrace = exc;
	 s = get_trace_from_exc(exc);
	 if (s == NULL) {
	   BIF_ERROR(c_p, EXC_ERROR);
	 } else {
	   BIF_ERROR(c_p, PRIMARY_EXCEPTION(s->freason));
	 }
       }
     }
}


/*
 * Support for compiled binary syntax operations.
 */

char *hipe_bs_allocate(int len)
{ 
  Binary* bptr;
  bptr = erts_bin_nrml_alloc(len);
  bptr->flags = 0;
  bptr->orig_size = len;
  bptr->refc = 1;
  return bptr->orig_bytes;
}

int hipe_bs_put_big_integer(Eterm arg, Uint num_bits, byte* base, unsigned offset, unsigned flags)
{ 
  byte* save_bin_buf;
  unsigned save_bin_offset, save_bin_buf_len;
  int res;
  save_bin_buf=erts_bin_buf;
  save_bin_offset=erts_bin_offset;
  save_bin_buf_len=erts_bin_buf_len;
  erts_bin_buf=base;
  erts_bin_offset=offset;
  erts_bin_buf_len=(offset+num_bits+7) >> 3;
  res = erts_bs_put_integer(arg, num_bits, flags);
  erts_bin_buf=save_bin_buf;
  erts_bin_offset=save_bin_offset;
  erts_bin_buf_len=save_bin_buf_len;
  return res;
}
int hipe_bs_put_small_float(Eterm arg, Uint num_bits, byte* base, unsigned offset, unsigned flags)
{ 
  byte* save_bin_buf;
  unsigned save_bin_offset, save_bin_buf_len;
  int res;
  save_bin_buf=erts_bin_buf;
  save_bin_offset=erts_bin_offset;
  save_bin_buf_len=erts_bin_buf_len;
  erts_bin_buf=base;
  erts_bin_offset=offset;
  erts_bin_buf_len=(offset+num_bits+7) >> 3;
  res = erts_bs_put_float(arg, num_bits, flags);
  erts_bin_buf=save_bin_buf;
  erts_bin_offset=save_bin_offset;
  erts_bin_buf_len=save_bin_buf_len;
  return res;
}
