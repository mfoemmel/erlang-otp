/* $Id$
 * hipe_sparc_glue.S
 *
 * XXX:
 * - Native code doesn't update HP_LIMIT or NSP_LIMIT. So why save them?
 */

#include "hipe_sparc_asm.h"

	.section ".text"

/*
 * int sparc_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.align	4
	.global	sparc_call_to_native
	.type	sparc_call_to_native,#function
	.proc	04
sparc_call_to_native:
	save %sp,-112,%sp
	ld [P+P_FCALLS],REDS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_ARG0],ARG0			! Arg0
	ld [P+P_ARG1],ARG1			! Arg1
	ld [P+P_ARG2],ARG2			! Arg2
	ld [P+P_ARG3],ARG3			! Arg3
	ld [P+P_ARG4],ARG4			! Arg4
	st %i7,[P+P_CRA]			! save C return address
	ld [P+P_NCALLEE],TEMP			! call address
/*
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 * Note: this is SPARC, so the value in the return address register
 * is the address of the call/jmpl instruction itself.
 */
	.global nbif_return
nbif_return:
	jmpl TEMP,RA				! Call native code
	nop
/*
 * This is where native code returns to emulated code.
 */
	st ARG0,[P+P_ARG0]			! save retval
	ba .flush_exit
	mov HIPE_MODE_SWITCH_RES_RETURN,TEMP

/*
 * Native code calls emulated code via a linker-generated
 * stub which should look as follows: (see compiler's sparc_loader.erl)
 *
 * stub for f/N:
 *	sethi %hi(f's BEAM code address), TEMP0
 *	mov RA, TEMP2		! because the call below clobbers RA (%o7)
 *	or TEMP0, %lo(f's BEAM code address), TEMP0
 *	call nbif_callemu	! clobbers RA!
 *	mov N, TEMP1
 */
	.global nbif_callemu
nbif_callemu:
	/* TEMP  contains callee's BEAM code address
	 * TEMP1 contains callee's arity
	 * TEMP2 contains native RA (current RA/%o7 contains junk)
	 */
	st TEMP,[P+P_BEAM_IP]			! callee
	st TEMP1,[P+P_ARITY]			! arity
	st TEMP2,[P+P_NRA]			! native return address
	st ARG0,[P+P_ARG0]			! Arg0
	st ARG1,[P+P_ARG1]			! Arg1
	st ARG2,[P+P_ARG2]			! Arg2
	st ARG3,[P+P_ARG3]			! Arg3
	st ARG4,[P+P_ARG4]			! Arg4
	ba .flush_exit
	mov HIPE_MODE_SWITCH_RES_CALL,TEMP

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first 5 are pushed onto the stack in left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */

	.global nbif_ccallemu5
nbif_ccallemu5:
	ld [NSP-4], TEMP
	ba .args4
	st TEMP,[P+P_CLOSURE]

	.global nbif_ccallemu4
nbif_ccallemu4:
	ba .args3
	st ARG4,[P+P_CLOSURE]

	.global nbif_ccallemu3
nbif_ccallemu3:
	ba .args2
	st ARG3,[P+P_CLOSURE]

	.global nbif_ccallemu2
nbif_ccallemu2:
	ba .args1
	st ARG2,[P+P_CLOSURE]

	.global nbif_ccallemu1
nbif_ccallemu1:
	ba .args0
	st ARG1,[P+P_CLOSURE]

	.global nbif_ccallemu0
nbif_ccallemu0:
	ba .ccall
	st ARG0,[P+P_CLOSURE]

.args4:	st ARG4,[P+P_ARG4]
.args3:	st ARG3,[P+P_ARG3]
.args2:	st ARG2,[P+P_ARG2]
.args1:	st ARG1,[P+P_ARG1]
.args0:	st ARG0,[P+P_ARG0]
.ccall:	ba .suspend_exit
	mov HIPE_MODE_SWITCH_RES_CALL_CLOSURE,TEMP

/*
 * This is where native code suspends.
 */
	.align 4
	.global nbif_suspend_0
nbif_suspend_0:
	ba .suspend_exit
	mov HIPE_MODE_SWITCH_RES_SUSPEND,TEMP

/*
 * Suspend from a receive (waiting for a message)
 */
	.align 4
	.global nbif_suspend_msg
nbif_suspend_msg:
	ba .suspend_exit
	mov HIPE_MODE_SWITCH_RES_WAIT,TEMP

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if( !(p->flags & F_TIMO) ) { suspend }
 *	else { p->flags ^= F_TIMO; return 0; }
 */
	.align 4
	.global nbif_suspend_msg_timeout
nbif_suspend_msg_timeout:
	ld [P+P_FLAGS],TEMP2			! Check if timeout
	!! this relies on F_TIMO (1<<2) fitting in an immediate...
	andcc TEMP2,F_TIMO,%g0			! F_TIMO set?
	bz,a .suspend_exit			! if not set, suspend
	mov HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT,TEMP ! .. and set C retval
	!! timeout has occurred
	xor TEMP2,F_TIMO,TEMP2			! F_TIMO was set, clear it
	st TEMP2,[P+P_FLAGS]
	jmp RA+8				! retl
	mov 0,ARG0

/*
 * Return to the calling C function.
 * The return value is in TEMP.
 *
 * .flush_exit saves REDS, NSP, HP (XXX: NSP_LIMIT, HP_LIMIT?)
 * .suspend_exit also saves RA
 */
.suspend_exit:	! like .flush_exit, but save RA too
	st	RA, [P+P_NRA]
.flush_exit:	! call with P intact and RV in TEMP
	ld	[P+P_CRA], %i7			! restore C return address
!!	st	NSP_LIMIT, [P+P_NSP_LIMIT]
!!	st	HP_LIMIT, [P+P_HP_LIMIT]
	st	REDS, [P+P_FCALLS]
	st	NSP, [P+P_NSP]
	st	HP, [P+P_HP]
	jmp	%i7+8				! ret
	restore	%g0,TEMP,%o0

/*
 * int sparc_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.align	4
	.global	sparc_return_to_native
	.type	sparc_return_to_native,#function
	.proc	04
sparc_return_to_native:
	save %sp,-112,%sp
	ld [P+P_NRA],RA				! XXX: was CALL_ADDRESS
	ld [P+P_FCALLS],REDS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_ARG0],ARG0			! retval
	st %i7,[P+P_CRA]			! save C return address
	jmp RA+8				! retl to native code
	nop

/*
 * int sparc_tailcall_to_native(Process *);
 * Emulated code tailcalls native code.
 */
	.align	4
	.global	sparc_tailcall_to_native
	.type	sparc_tailcall_to_native,#function
	.proc	04
sparc_tailcall_to_native:
	save %sp,-112,%sp
	ld [P+P_FCALLS],REDS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_ARG0],ARG0			! Arg0
	ld [P+P_ARG1],ARG1			! Arg1
	ld [P+P_ARG2],ARG2			! Arg2
	ld [P+P_ARG3],ARG3			! Arg3
	ld [P+P_ARG4],ARG4			! Arg4
	ld [P+P_NRA],RA				! native return address
	ld [P+P_NCALLEE],TEMP			! call address
	st %i7,[P+P_CRA]			! save C return address
	jmp TEMP				! Call native code
	nop

/*
 * int sparc_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.align	4
	.global	sparc_throw_to_native
	.type	sparc_throw_to_native,#function
	.proc	04
sparc_throw_to_native:
	save %sp,-112,%sp
	ld [P+P_FCALLS],REDS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	st %i7,[P+P_CRA]			! save C return address
	ba .find_exn_handler
	nop

/*
 * Find and invoke catch handler (it must exist).
 * The stack/heap registers were just read from P.
 */
.find_exn_handler:
	/* find and prepare to invoke the handler */
	st	NSP, [P+P_NSP]		! XXX: unnecessary?
	call	hipe_handle_exception
	mov	P, ARG0
	ld	[P+P_HP],HP		! hipe_handle_exception() conses :-(
	/* now invoke the handler */
	ld	[P+P_NCALLEE], %o1	! updated by hipe_find_handler()
	ld	[P+P_NSP], NSP		! updated by hipe_find_handler()
	jmp	%o1
	ld	[P+P_FVALUE], ARG0

/*
 * This is the default exception handler for native code.
 */
	.global	nbif_fail
nbif_fail:
	ba	.flush_exit
	mov	HIPE_MODE_SWITCH_RES_THROW, TEMP

/*
 * We end up here when a bif called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * The stack/heap registers were just read from P.
 * %l0 contains the native return address.
 * [%o0 is zero on entry and must be zero on exit.][OBSOLETE?]
 */
	.global	nbif_0_simple_exception
	.global	nbif_1_simple_exception
	.global	nbif_2_simple_exception
	.global	nbif_3_simple_exception
	.align	4
nbif_0_simple_exception:
	ba	.nbif_simple_exception
	mov	0,ARG4
nbif_1_simple_exception:
	ba	.nbif_simple_exception
	mov	1,ARG4
nbif_2_simple_exception:
	ba	.nbif_simple_exception
	mov	2,ARG4
nbif_3_simple_exception:
	mov	3,ARG4
	!! FALLTHROUGH
.nbif_simple_exception:
	ld	[P+P_FREASON],%o1
	cmp	%o1,FREASON_TRAP
	bne	.find_exn_handler
	nop
	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF stored the callee's Export* in p->fvalue
	 * - the BIF stored the actual parameters in p->def_arg_reg[]
	 * - the arity is the same as the BIF's; our glue put it in ARG4
	 * - the native RA was saved in %l0 before the BIF call
	 * - the native heap/stack/reds registers are saved in P
	 */
	mov	HIPE_MODE_SWITCH_RES_TRAP, TEMP
.bif_exit:
	ld	[P+P_CRA], %i7			! restore C return address
	st	ARG4, [P+P_ARITY]
	st	%l0, [P+P_NRA]			! RA
	jmp	%i7+8
	restore	%g0,TEMP,%o0

/*
 * We end up here when a bif called from native signals an
 * exceptional condition, and RESCHEDULE _CAN_ occur.
 * %l0 contains the native return address, and the
 * stack/heap/reds registers were just read from P.
 * TEMP contains the address of the nbif which failed
 * TEMP1 contains the first actual parameter
 * TEMP2 contains the second actual parameter (if it is defined)
 * ARG4 contains the number of parameters (1 or 2)
 */
	.align	4
	.global	nbif_hairy_exception
nbif_hairy_exception:
	ld	[P+P_FREASON],%o1	! must not clobber bif retval in %o0
	cmp	%o1,FREASON_RESCHEDULE
	bne	.nbif_simple_exception
	nop
	/* handle reschedule */
	st	TEMP,[P+P_NCALLEE]
	st	TEMP1,[P+P_ARG0]
	st	TEMP2,[P+P_ARG1]
	ba	.bif_exit
	mov	HIPE_MODE_SWITCH_RES_RESCHEDULE, TEMP

/*
 * nbif_test
 * placeholder for hand-written code to exercise the mode-switch interface
 */
	.global	nbif_test
	.align	4
nbif_test:
	jmp RA+8				! retl
	mov ERL_NIL,%o0
