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
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_stack.h"

/*
 * This is called when inlined heap allocation in native code fails.
 * The 'need' parameter is the number of heap words needed.
 * The value is tagged as a fixnum to avoid untagged data on
 * the x86 stack while the gc is running.
 */
void hipe_gc(Process *p, Eterm need)
{
#ifdef __i386__
    p->hipe.narity = 1;
#endif
    p->fcalls -= erts_garbage_collect(p, unsigned_val(need), NULL, 0);
#ifdef __i386__
    p->hipe.narity = 0;
#endif
}

/* This is like the OP_setTimeout JAM instruction.
 *  Transformation to the BEAM instruction wait_timeout_fs
 *  has begun.
 * XXX: BUG: native code should check return status
 */
Eterm hipe_set_timeout(Process *p, Eterm timeout_value)
{
    Uint time_val;
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
    if( is_small(timeout_value) && signed_val(timeout_value) > 0 ) {
	time_val = unsigned_val(timeout_value);
	set_timer(p, time_val);
    } else if( timeout_value == am_infinity ) {
	/* p->flags |= F_TIMO; */	/* XXX: nbif_suspend_msg_timeout */
    } else if( term_to_Uint(timeout_value, &time_val) ) {
	set_timer(p, time_val);
    } else {
	BIF_ERROR(p, EXC_TIMEOUT_VALUE);
    }
    return NIL;	/* caller had better call nbif_suspend ASAP! */
}

/*
 * This is like the timeout BEAM instruction.
 */
Eterm hipe_clear_timeout(Process *p)
{
    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
    if (IS_TRACED_FL(p, F_TRACE_RECEIVE)) {
	trace_receive(p, am_timeout);
    }
    p->flags &= ~F_TIMO;
    JOIN_MESSAGE(p);
    return NIL;
}

int hipe_mbox_empty(Process *p)
{
    return PEEK_MESSAGE(p) == NULL;
}

Eterm hipe_get_msg(Process *p)
{
    ErlMessage *mp = PEEK_MESSAGE(p);
    if (!mp) {
	printf("Getting message from empty message queue (1)!");
	exit(1);
    }
    return ERL_MESSAGE_TERM(mp);
}

/* This is like the loop_rec_end BEAM instruction.
 */
void hipe_next_msg(Process *p)
{
    if (PEEK_MESSAGE(p) == NULL) {
	printf("Getting message from empty message queue (2)!");
	exit(1);
    }
    SAVE_MESSAGE(p);
}

/* This is like the remove_message BEAM instruction 
 */
void hipe_select_msg(Process *p)
{
    ErlMessage *msgp;

    msgp = PEEK_MESSAGE(p);
    if (!msgp) {
	printf("Getting message from empty message queue (3)!");
	exit(1);
    }
    UNLINK_MESSAGE(p, msgp);
    JOIN_MESSAGE(p);
    CANCEL_TIMER(p);
    free_message(msgp);
}

/*
 * hipe_handle_exception() is called from hipe_${ARCH}_glue.S when an
 * exception has been thrown, to "fix up" the exception value and to
 * locate the current handler.
 */
void hipe_handle_exception(Process *c_p)
{
    Eterm* hp;
    Eterm Value;

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */
    ASSERT(c_p->freason != RESCHEDULE); /* Should have been handled earlier. */
    { Uint r = c_p->freason & EXF_INDEXBITS;
      ASSERT(r < NUMBER_EXIT_CODES); /* range check */
      if (r < NUMBER_EXIT_CODES) {
          Value = error_atom[r];
      } else {
	  Value = am_internal_error;
	  c_p->freason = EXC_INTERNAL_ERROR;
      }
    }

    switch (c_p->freason & EXF_INDEXBITS) {
    case (EXC_EXIT & EXF_INDEXBITS):
        /* Primary exceptions use fvalue directly */
        ASSERT(is_value(c_p->fvalue));
        Value = c_p->fvalue;
        break;
    case (EXC_BADMATCH & EXF_INDEXBITS):
    case (EXC_CASE_CLAUSE & EXF_INDEXBITS):
    case (EXC_BADFUN & EXF_INDEXBITS):
	ASSERT(is_value(c_p->fvalue));
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, Value, c_p->fvalue);
	break;
    default:
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, Value, NIL);
	break;
    }

    c_p->freason &= EXF_PRIMARY;   /* index becomes zero */
    c_p->fvalue = Value;

    hipe_find_handler(c_p);
}

/*
 * Support for compiled binary syntax operations.
 */

void *hipe_bs_get_matchbuffer(void)
{
    return &erts_mb.orig;
}
