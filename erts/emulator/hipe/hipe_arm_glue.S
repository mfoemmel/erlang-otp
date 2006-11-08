/* $Id$
 */
#include "hipe_arm_asm.h"
#include "hipe_literals.h"
#define ASM
#include "hipe_mode_switch.h"

	.text
	.p2align 2

/*
 * Enter Erlang from C.
 * Create a new frame on the C stack.
 * Save C callee-save registers in the frame.
 * Do not clobber the C argument registers.
 * Retrieve the process pointer from the C argument registers.
 *
 * Our C frame includes:
 * - 9*4 == 36 bytes for saving r4-r11 and lr
 * - 2*4 == 8 bytes for calls to hipe_bs_put_{big_integer,small_float}.
 *   They take 5-6 parameter words: 4 in registers and 1-2 on the stack.
 *   (They take 5 regular parameters, and an additional P parameter on SMP.)
 * - 4 bytes to pad the frame size to a multiple of 8
 */
#define ENTER_FROM_C	\
	stmfd	sp!, {r4,r5,r6,r7,r8,r9,r10,r11,lr};	\
	sub	sp, sp, #12; \
	mov	P, r0;	\
	RESTORE_CACHED_STATE

/*
 * Return to the calling C function.
 * The return value is in r0.
 *
 * .nosave_exit saves no state
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	str	lr, [P, #P_NRA]
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* restore callee-save registers, drop frame, return */
	add	sp, sp, #12
	ldmfd	sp!, {r4,r5,r6,r7,r8,r9,r10,r11,pc}

/*
 * int hipe_arm_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.global	hipe_arm_call_to_native
hipe_arm_call_to_native:
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* call the target */
	mov	lr, pc
	ldr	pc, [P, #P_NCALLEE]
/* FALLTHROUGH
 *
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
	.global	nbif_return
nbif_return:
	str	r0, [P, #P_ARG0]			/* save retval */
	mov	r0, #HIPE_MODE_SWITCH_RES_RETURN
	b	.flush_exit

/*
 * int hipe_arm_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.global	hipe_arm_return_to_native
hipe_arm_return_to_native:
	ENTER_FROM_C
	/* get return value */
	ldr	r0, [P, #P_ARG0]
	/*
	 * Return using the current return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	ldr	pc, [P, #P_NRA]

/*
 * int hipe_arm_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	.global	hipe_arm_tailcall_to_native
hipe_arm_tailcall_to_native:
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* restore return address */
	ldr	lr, [P, #P_NRA]
	/* call the target */
	ldr	pc, [P, #P_NCALLEE]

/*
 * int hipe_arm_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.global	hipe_arm_throw_to_native
hipe_arm_throw_to_native:
	ENTER_FROM_C
	/* invoke the handler */
	ldr	pc, [P, #P_NCALLEE]	/* set by hipe_find_handler() */

/*
 * Native code calls emulated code via a stub
 * which should look as follows:
 *
 * stub for f/N:
 *	<set r8 to f's BEAM code address>
 *	<set r0 to N>
 *	b nbif_callemu
 *
 * XXX: Different stubs for different number of register parameters?
 */
	.global	nbif_callemu
nbif_callemu:
	str	r8, [P, #P_BEAM_IP]
	str	r0, [P, #P_ARITY]
	STORE_ARG_REGS
	mov	r0, #HIPE_MODE_SWITCH_RES_CALL
	b	.suspend_exit

/*
 * nbif_apply
 */
	.global	nbif_apply
nbif_apply:
	STORE_ARG_REGS
	mov	r0, #HIPE_MODE_SWITCH_RES_APPLY
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
	.global	nbif_ccallemu6
nbif_ccallemu6:
	str	ARG5, [P, #P_ARG5]
#if NR_ARG_REGS > 6
	mov	ARG5, ARG6
#else
	ldr	ARG5, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	.global	nbif_ccallemu5
nbif_ccallemu5:
	str	ARG4, [P, #P_ARG4]
#if NR_ARG_REGS > 5
	mov	ARG4, ARG5
#else
	ldr	ARG4, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	.global	nbif_ccallemu4
nbif_ccallemu4:
	str	ARG3, [P, #P_ARG3]
#if NR_ARG_REGS > 4
	mov	ARG3, ARG4
#else
	ldr	ARG3, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	.global	nbif_ccallemu3
nbif_ccallemu3:
	str	ARG2, [P, #P_ARG2]
#if NR_ARG_REGS > 3
	mov	ARG2, ARG3
#else
	ldr	ARG2, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	.global	nbif_ccallemu2
nbif_ccallemu2:
	str	ARG1, [P, #P_ARG1]
#if NR_ARG_REGS > 2
	mov	ARG1, ARG2
#else
	ldr	ARG1, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	.global	nbif_ccallemu1
nbif_ccallemu1:
	str	ARG0, [P, #P_ARG0]
#if NR_ARG_REGS > 1
	mov	ARG0, ARG1
#else
	ldr	ARG0, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

	.global	nbif_ccallemu0
nbif_ccallemu0:
	/* We use r1 not ARG0 here because ARG0 is not
	   defined when NR_ARG_REGS == 0. */
#if NR_ARG_REGS == 0
	ldr	r1, [NSP, #0]		/* get the closure */
#endif
	str	r1, [P, #P_CLOSURE]	/* save the closure */
	mov	r0, #HIPE_MODE_SWITCH_RES_CALL_CLOSURE
	b	.suspend_exit

/*
 * This is where native code suspends.
 */
	.global	nbif_suspend_0
nbif_suspend_0:
	mov	r0, #HIPE_MODE_SWITCH_RES_SUSPEND
	b	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	.global	nbif_suspend_msg
nbif_suspend_msg:
	mov	r0, #HIPE_MODE_SWITCH_RES_WAIT
	b	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if (!(p->flags & F_TIMO)) { suspend }
 *	else { return 0; }
 */
	.global	nbif_suspend_msg_timeout
nbif_suspend_msg_timeout:
	ldr	r1, [P, #P_FLAGS]
	mov	r0, #HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT
	/* this relies on F_TIMO (1<<2) fitting in a uimm16 */
	tst	r1, #F_TIMO
	beq	.suspend_exit
	/* timeout has occurred */
	mov	r0, #0
	mov	pc, lr

/*
 * This is the default exception handler for native code.
 */
	.global	nbif_fail
nbif_fail:
	mov	r0, #HIPE_MODE_SWITCH_RES_THROW
	b	.flush_exit	/* no need to save RA */

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * HP was just read from P.
 * NSP has not been saved in P.
 * TEMP_LR contains a copy of LR
 */
	.global	nbif_1_simple_exception
nbif_1_simple_exception:
	mov	r1, #1
	b	.nbif_simple_exception
	.global	nbif_2_simple_exception
nbif_2_simple_exception:
	mov	r1, #2
	b	.nbif_simple_exception
	.global	nbif_3_simple_exception
nbif_3_simple_exception:
	mov	r1, #3
	/*FALLTHROUGH*/
.nbif_simple_exception:
	ldr	r0, [P, #P_FREASON]
.nbif_simple_exception2:
	cmp	r0, #FREASON_TRAP
	beq	.handle_trap
	/*
	 * Find and invoke catch handler (it must exist).
	 * HP was just read from P.
	 * NSP has not been saved in P.
	 * TEMP_LR should contain the current call's return address.
	 * r1 should contain the current call's arity.
	 */
	str	NSP, [P, #P_NSP]
	str	TEMP_LR, [P, #P_NRA]
	str	r1, [P, #P_NARITY]
	/* find and prepare to invoke the handler */
	mov	r0, P
	bl	hipe_handle_exception	/* Note: hipe_handle_exception() conses */
	RESTORE_CACHED_STATE		/* NSP updated by hipe_find_handler() */
	/* now invoke the handler */
	ldr	pc, [P, #P_NCALLEE]	/* set by hipe_find_handler() */

	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF stored the callee's Export* in p->def_arg_reg[3]
	 * - the BIF stored the callee's parameters in p->def_arg_reg[0..2]
	 * - the BIF's arity is in r1
	 * - the native RA was saved in TEMP_LR before the BIF call
	 * - HP was just read from P
	 * - NSP has not been saved in P
	 */
.handle_trap:
	mov	r0, #HIPE_MODE_SWITCH_RES_TRAP
.bif_exit:
	str	NSP, [P, #P_NSP]
	str	r1, [P, #P_ARITY]
	str	TEMP_LR, [P, #P_NRA]
	b	.nosave_exit

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE can occur.
 * r2 contains the address of the nbif which failed.
 * HP was just read from P.
 * NSP has not been saved in P.
 * TEMP_ARG0 is the first actual parameter, if NR_ARG_REGS > 0.
 * TEMP_ARG1 is the second actual parameter (if defined), if NR_ARG_REGS > 1.
 */
	.global	nbif_1_hairy_exception
nbif_1_hairy_exception:
	mov	r1, #1
	b	.nbif_hairy_exception
	.global	nbif_2_hairy_exception
nbif_2_hairy_exception:
	mov	r1, #2
	/*FALLTHROUGH*/
.nbif_hairy_exception:
	ldr	r0, [P, #P_FREASON]
	cmp	r0, #FREASON_RESCHEDULE
	bne	.nbif_simple_exception2
	/* handle reschedule */
	str	r2, [P, #P_NCALLEE]
#if NR_ARG_REGS == 0
	mov	r1, #0		/* arity unused -- args on nstack */
#elif NR_ARG_REGS == 1
	mov	r1, #1		/* 1 even if arity is 2 */
	str	TEMP_ARG0, [P, #P_ARG0]
#else
	str	TEMP_ARG0, [P, #P_ARG0]
	str	TEMP_ARG1, [P, #P_ARG1]
#endif
	mov	r0, #HIPE_MODE_SWITCH_RES_RESCHEDULE
	b	.bif_exit

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	.global	nbif_stack_trap_ra
nbif_stack_trap_ra:		/* a return address, not a function */
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	mov	TEMP_RV, r0		/* save retval */
	str	NSP, [P, #P_NSP]
	mov	r0, P
	bl	hipe_handle_stack_trap	/* must not cons */
	mov	lr, r0			/* original RA */
	mov	r0, TEMP_RV		/* restore retval */
	mov	pc, lr			/* resume at original RA */

/*
 * hipe_arm_inc_stack
 * Caller saved its LR in TEMP_LR (== TEMP1) before calling us.
 */
	.global	hipe_arm_inc_stack
hipe_arm_inc_stack:
	STORE_ARG_REGS
	mov	TEMP_ARG0, lr
	str	NSP, [P, #P_NSP]
	mov	r0, P
	# hipe_inc_nstack reads and writes NSP and NSP_LIMIT,
	# but does not access LR/RA, HP, or FCALLS.
	bl	hipe_inc_nstack
	ldr	NSP, [P, #P_NSP]
	LOAD_ARG_REGS
	# this relies on LOAD_ARG_REGS not clobbering TEMP_ARG0
	mov	pc, TEMP_ARG0
