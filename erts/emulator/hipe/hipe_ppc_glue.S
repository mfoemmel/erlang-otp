/* $Id$
 */
#include "hipe_ppc_asm.h"
#include "hipe_literals.h"
#define ASM
#include "hipe_mode_switch.h"

	.text
	.p2align 2

/*
 * Enter Erlang from C.
 * Create a new frame on the C stack.
 * Save C callee-save registers (r14-r31) in the frame.
 * Save r0 (C return address) in the frame's LR save slot.
 * Return to LR.
 * Do not clobber the C argument registers (r3-r10).
 *
 * Usage: mflr r0 SEMI bl .enter
 */
.enter:
	# A unified Linux/OSX C frame must include:
	# - 24 bytes for AIX/OSX-like linkage area
	# - 28 bytes for AIX/OSX-like parameter area for
	#   recursive C calls with up to 7 parameter words
	# - 76 bytes for saving r14-r31 and LR
	# - padding to make it a multiple of 16 bytes
	# The final size is 128 bytes.
	# stwu is required for atomic alloc+init
	stwu	r1,-128(r1)	/* 0(r1) contains r1+128 */
	stw	r14, 52(r1)
	stw	r15, 56(r1)
	stw	r16, 60(r1)
	stw	r17, 64(r1)
	stw	r18, 68(r1)
	stw	r19, 72(r1)
	stw	r20, 76(r1)
	stw	r21, 80(r1)
	stw	r22, 84(r1)
	stw	r23, 88(r1)
	stw	r24, 92(r1)
	stw	r25, 96(r1)
	stw	r26, 100(r1)
	stw	r27, 104(r1)
	stw	r28, 108(r1)
	stw	r29, 112(r1)
	stw	r30, 116(r1)
	stw	r31, 120(r1)
	stw	r0,  124(r1)	/* caller saved LR in r0 */
	blr

/*
 * Return to the calling C function.
 * The return value is in r3.
 *
 * .nosave_exit saves no state
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	mflr	r0
	stw	r0, P_NRA(P)
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* restore callee-save registers, drop frame, return */
	lwz	r0, 124(r1)
	mtlr	r0
	lwz	r14, 52(r1)
	lwz	r15, 56(r1)
	lwz	r16, 60(r1)
	lwz	r17, 64(r1)
	lwz	r18, 68(r1)
	lwz	r19, 72(r1)
	lwz	r20, 76(r1)
	lwz	r21, 80(r1)
	lwz	r22, 84(r1)
	lwz	r23, 88(r1)
	lwz	r24, 92(r1)
	lwz	r25, 96(r1)
	lwz	r26, 100(r1)
	lwz	r27, 104(r1)
	lwz	r28, 108(r1)
	lwz	r29, 112(r1)	/* kills HP */
	lwz	r30, 116(r1)	/* kills NSP */
	lwz	r31, 120(r1)	/* kills P */
	addi	r1, r1, 128
	blr

/*
 * int hipe_ppc_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	GLOBAL(hipe_ppc_call_to_native)
CSYM(hipe_ppc_call_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* get the process pointer */
	mr	P, r3
	/* prepare to call the target */
	lwz	r0, P_NCALLEE(P)
	mtctr	r0
	/* get argument registers */
	LOAD_ARG_REGS
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* call the target */
	bctrl	/* defines LR (a.k.a. NRA) */
/* FALLTHROUGH
 *
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
	GLOBAL(nbif_return)
CSYM(nbif_return):
	stw	r3, P_ARG0(P)			/* save retval */
	li	r3, HIPE_MODE_SWITCH_RES_RETURN
	b	.flush_exit

/*
 * int hipe_ppc_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	GLOBAL(hipe_ppc_return_to_native)
CSYM(hipe_ppc_return_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* get the process pointer */
	mr	P, r3
	/* restore return address */
	lwz	r0, P_NRA(P)
	mtlr	r0
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* get return value */
	lwz	r3, P_ARG0(P)
	/*
	 * Return using the current return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	blr

/*
 * int hipe_ppc_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	GLOBAL(hipe_ppc_tailcall_to_native)
CSYM(hipe_ppc_tailcall_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* get the process pointer */
	mr	P, r3
	/* prepare to call the target */
	lwz	r0, P_NCALLEE(P)
	mtctr	r0
	/* get argument registers */
	LOAD_ARG_REGS
	/* restore return address */
	lwz	r0, P_NRA(P)
	mtlr	r0
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* call the target */
	bctr

/*
 * int hipe_ppc_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	GLOBAL(hipe_ppc_throw_to_native)
CSYM(hipe_ppc_throw_to_native):
	/* save C context */
	mflr	r0
	bl	.enter
	/* get the process pointer */
	mr	P, r3
	/* cache some P state in registers */
	RESTORE_CACHED_STATE
	/* find and invoke the handler */
	li	r4, 0			/* narity == 0, no args on the stack */
	b	.find_exn_handler_no_lr	/* skip LR reload and save */

/*
 * Find and invoke catch handler (it must exist).
 * The stack/heap registers were just read from P.
 * TEMP_LR should contain the current call's return address.
 * r4 should contain the current call's arity.
 */
.find_exn_handler:
	stw	TEMP_LR, P_NRA(P)
.find_exn_handler_no_lr:
	stw	r4, P_NARITY(P)
	/* find and prepare to invoke the handler */
	mr	r3, P
	bl	CSYM(hipe_handle_exception)	/* Note: hipe_handle_exception() conses */
	/* prepare to invoke handler */
	lwz	r0, P_NCALLEE(P)	/* set by hipe_find_handler() */
	mtctr	r0
	RESTORE_CACHED_STATE		/* NSP updated by hipe_find_handler() */
	/* now invoke the handler */
	bctr

/*
 * Native code calls emulated code via a stub
 * which should look as follows:
 *
 * stub for f/N:
 *	<set r12 to f's BEAM code address>
 *	<set r0 to N>
 *	b nbif_callemu
 *
 * The stub may need to create &nbif_callemu as a 32-bit immediate
 * in a scratch register if the branch needs a trampoline. The code
 * for creating a 32-bit immediate in r0 is potentially slower than
 * for other registers (an add must be replaced by an or, and adds
 * are potentially faster than ors), so it is better to use r0 for
 * the arity (a small immediate), making r11 available for trampolines.
 * (See "The PowerPC Compiler Writer's Guide, section 3.2.3.1.)
 *
 * XXX: Different stubs for different number of register parameters?
 */
	GLOBAL(nbif_callemu)
CSYM(nbif_callemu):
	stw	r12, P_BEAM_IP(P)
	stw	r0, P_ARITY(P)
	STORE_ARG_REGS
	li	r3, HIPE_MODE_SWITCH_RES_CALL
	b	.suspend_exit

/*
 * nbif_apply
 */
	GLOBAL(nbif_apply)
CSYM(nbif_apply):
	STORE_ARG_REGS
	li	r3, HIPE_MODE_SWITCH_RES_APPLY
	b	.suspend_exit

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first few passed in registers are pushed onto the stack in
 * left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */
#if NR_ARG_REGS >= 6
	GLOBAL(nbif_ccallemu6)
CSYM(nbif_ccallemu6):
	stw	ARG5, P_ARG5(P)
#if NR_ARG_REGS > 6
	mr	ARG5, ARG6
#else
	lwz	ARG5, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	GLOBAL(nbif_ccallemu5)
CSYM(nbif_ccallemu5):
	stw	ARG4, P_ARG4(P)
#if NR_ARG_REGS > 5
	mr	ARG4, ARG5
#else
	lwz	ARG4, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	GLOBAL(nbif_ccallemu4)
CSYM(nbif_ccallemu4):
	stw	ARG3, P_ARG3(P)
#if NR_ARG_REGS > 4
	mr	ARG3, ARG4
#else
	lwz	ARG3, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	GLOBAL(nbif_ccallemu3)
CSYM(nbif_ccallemu3):
	stw	ARG2, P_ARG2(P)
#if NR_ARG_REGS > 3
	mr	ARG2, ARG3
#else
	lwz	ARG2, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	GLOBAL(nbif_ccallemu2)
CSYM(nbif_ccallemu2):
	stw	ARG1, P_ARG1(P)
#if NR_ARG_REGS > 2
	mr	ARG1, ARG2
#else
	lwz	ARG1, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	GLOBAL(nbif_ccallemu1)
CSYM(nbif_ccallemu1):
	stw	ARG0, P_ARG0(P)
#if NR_ARG_REGS > 1
	mr	ARG0, ARG1
#else
	lwz	ARG0, 0(NSP)
#endif
	/*FALLTHROUGH*/
#endif

	GLOBAL(nbif_ccallemu0)
CSYM(nbif_ccallemu0):
#if NR_ARG_REGS == 0
	lwz	r4, 0(NSP)		/* get the closure */
#endif
	stw	r4, P_CLOSURE(P)	/* save the closure */
	li	r3, HIPE_MODE_SWITCH_RES_CALL_CLOSURE
	b	.suspend_exit

/*
 * This is where native code suspends.
 */
	GLOBAL(nbif_suspend_0)
CSYM(nbif_suspend_0):
	li	r3, HIPE_MODE_SWITCH_RES_SUSPEND
	b	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	GLOBAL(nbif_suspend_msg)
CSYM(nbif_suspend_msg):
	li	r3, HIPE_MODE_SWITCH_RES_WAIT
	b	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if( !(p->flags & F_TIMO) ) { suspend }
 *	else { p->flags ^= F_TIMO; return 0; }
 */
	GLOBAL(nbif_suspend_msg_timeout)
CSYM(nbif_suspend_msg_timeout):
	lwz	r4, P_FLAGS(P)
	li	r3, HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT
	/* this relies on F_TIMO (1<<2) fitting in a uimm16 */
	andi.	r0, r4, F_TIMO
	/* XXX: The process will immediately execute 'clear_timeout',
	   repeating this statement. Remove it? */
	xori	r4, r4, F_TIMO			/* F_TIMO was set, clear it */
	beq-	.suspend_exit			/* sees the CR state from andi. above */
	/* timeout has occurred */
	stw	r4, P_FLAGS(P)
	li	r3, 0
	blr

/*
 * This is the default exception handler for native code.
 */
	GLOBAL(nbif_fail)
CSYM(nbif_fail):
	li	r3, HIPE_MODE_SWITCH_RES_THROW
	b	.flush_exit	/* no need to save RA */

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * The stack/heap registers were just read from P.
 * TEMP_LR contains a copy of LR
 */
	GLOBAL(nbif_0_simple_exception)
CSYM(nbif_0_simple_exception):
	li	r4, 0
	b	.nbif_simple_exception
	GLOBAL(nbif_1_simple_exception)
CSYM(nbif_1_simple_exception):
	li	r4, 1
	b	.nbif_simple_exception
	GLOBAL(nbif_2_simple_exception)
CSYM(nbif_2_simple_exception):
	li	r4, 2
	b	.nbif_simple_exception
	GLOBAL(nbif_3_simple_exception)
CSYM(nbif_3_simple_exception):
	li	r4, 3
	/*FALLTHROUGH*/
.nbif_simple_exception:
	lwz	r3, P_FREASON(P)
	cmpwi	r3, FREASON_TRAP
	bne+	.find_exn_handler
	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF stored the callee's Export* in p->def_arg_reg[0]
	 * - the BIF stored the actual parameters in p->def_arg_reg[1..]
	 * - the BIF's arity is in r4
	 * - the native RA was saved in TEMP_LR before the BIF call
	 * - the native heap/stack/reds registers are saved in P
	 */
	li	r3, HIPE_MODE_SWITCH_RES_TRAP
.bif_exit:
	stw	r4, P_ARITY(P)
	stw	TEMP_LR, P_NRA(P)
	b	.nosave_exit

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE can occur.
 * r5 contains the address of the nbif which failed, and the
 * stack/heap/reds registers were just read from P.
 * TEMP_ARG0 is the first actual parameter, if PPC_NR_ARG_REGS > 0.
 * TEMP_ARG1 is the second actual parameter (if defined), if PPC_NR_ARG_REGS > 1.
 */
	GLOBAL(nbif_1_hairy_exception)
CSYM(nbif_1_hairy_exception):
	li	r4, 1
	b	.nbif_hairy_exception
	GLOBAL(nbif_2_hairy_exception)
CSYM(nbif_2_hairy_exception):
	li	r4, 2
	/*FALLTHROUGH*/
.nbif_hairy_exception:
	lwz	r3, P_FREASON(P)
	cmpwi	r3, FREASON_RESCHEDULE
	bne+	.nbif_simple_exception
	/* handle reschedule */
	stw	r5, P_NCALLEE(P)
#if PPC_NR_ARG_REGS == 0
	li	r4, 0		/* arity unused -- args on nstack */
#elif PPC_NR_ARG_REGS == 1
	li	r4, 1		/* 1 even if arity is 2 */
	stw	TEMP_ARG0, P_ARG0(P)
#else
	stw	TEMP_ARG0, P_ARG0(P)
	stw	TEMP_ARG1, P_ARG1(P)
#endif
	li	r3, HIPE_MODE_SWITCH_RES_RESCHEDULE
	b	.bif_exit

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	GLOBAL(nbif_stack_trap_ra)
CSYM(nbif_stack_trap_ra):		/* a return address, not a function */
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	stw	r3, P_ARG0(P)		/* save retval */
	stw	NSP, P_NSP(P)
	mr	r3, P
	bl	CSYM(hipe_handle_stack_trap)	/* must not cons */
	mtctr	r3			/* original RA */
	lwz	r3, P_ARG0(P)		/* restore retval */
	bctr				/* resume at original RA */

/*
 * hipe_ppc_inc_stack
 * Caller saved its LR in a C callee-save register before calling us.
 */
	GLOBAL(hipe_ppc_inc_stack)
CSYM(hipe_ppc_inc_stack):
	STORE_ARG_REGS
	mflr	r0
	stw	r0, P_NRA(P)
	SAVE_CACHED_STATE
	mr	r3, P
	bl	CSYM(hipe_inc_nstack)
	lwz	r0, P_NRA(P)
	mtlr	r0
	RESTORE_CACHED_STATE
	LOAD_ARG_REGS
	blr
