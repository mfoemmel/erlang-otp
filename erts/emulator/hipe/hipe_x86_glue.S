/*
 * $Id$
 */

#include "hipe_x86_asm.h"
#include "hipe_literals.h"
#define ASM
#include "hipe_mode_switch.h"

/*
 * Set up frame on C stack,
 * save C callee-save registers,
 * retrieve the process pointer from the parameters from C,
 * SWITCH_C_TO_ERLANG.
 */
#define ENTER_FROM_C		\
	/* save C callee-save registers on the C stack */ \
	subl	$16, %esp;	\
	movl	%ebp, 12(%esp);	\
	movl	%ebx, 8(%esp);	\
	movl	%esi, 4(%esp);	\
	movl	%edi, (%esp);	\
	/* get the process pointer */	\
	movl	20(%esp), P;	\
	/* switch to native stack */	\
	SWITCH_C_TO_ERLANG

	.section ".text"

/*
 * int x86_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.align	4
	.global	x86_call_to_native
	.global	nbif_return
x86_call_to_native:
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* call the target */
	NSP_CALL(*P_NCALLEE(P))
/*
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
nbif_return:
	movl	%eax, P_ARG0(P)			# save retval
	movl	$HIPE_MODE_SWITCH_RES_RETURN, %eax
/* FALLTHROUGH to .flush_exit
 *
 * Return to the calling C function with result token in %eax.
 *
 * .nosave_exit saves no state
 * .flush_exit saves cached P state
 * .suspend_exit also saves RA
 */
.suspend_exit:
	/* save RA, no-op on x86 */
.flush_exit:
	/* flush cached P state */
	SAVE_HP
.nosave_exit:
	/* switch to C stack */
	SWITCH_ERLANG_TO_C_QUICK
	/* restore C callee-save registers, drop frame, return */
	movl	(%esp), %edi
	movl	4(%esp), %esi	# kills HP, if HP_IN_ESI is true
	movl	8(%esp), %ebx
	movl	12(%esp), %ebp	# kills P
	addl	$16, %esp
	ret

/*
 * Native code calls emulated code via a linker-generated
 * stub (hipe_x86_loader.erl) which should look as follows:
 *
 * stub for f/N:
 *	movl	$<f's BEAM code address>, P_BEAM_IP(P)
 *	movb	$<N>, P_ARITY(P)
 *	jmp	nbif_callemu
 *
 * XXX: Different stubs for different number of register parameters?
 */
	.align	4
	.global	nbif_callemu
nbif_callemu:
	STORE_ARG_REGS
	movl	$HIPE_MODE_SWITCH_RES_CALL, %eax
	jmp	.suspend_exit

/*
 * nbif_apply
 */
	.align	4
	.global	nbif_apply
nbif_apply:
	STORE_ARG_REGS
	movl	$HIPE_MODE_SWITCH_RES_APPLY, %eax
	jmp	.suspend_exit

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first few passed in registers are pushed onto the stack in
 * left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */
#if X86_NR_ARG_REGS == 5
	.align	4
	.global nbif_ccallemu5
nbif_ccallemu5:
	movl	ARG4, P_ARG4(P)
	movl	4(NSP), ARG4
	/*FALLTHROUGH*/
#endif

#if X86_NR_ARG_REGS >= 4
	.align	4
	.global nbif_ccallemu4
nbif_ccallemu4:
	movl	ARG3, P_ARG3(P)
#if X86_NR_ARG_REGS > 4
	movl	ARG4, ARG3
#else
	movl	4(NSP), ARG3
#endif
	/*FALLTHROUGH*/
#endif

#if X86_NR_ARG_REGS >= 3
	.align	4
	.global nbif_ccallemu3
nbif_ccallemu3:
	movl	ARG2, P_ARG2(P)
#if X86_NR_ARG_REGS > 3
	movl	ARG3, ARG2
#else
	movl	4(NSP), ARG2
#endif
	/*FALLTHROUGH*/
#endif

#if X86_NR_ARG_REGS >= 2
	.align	4
	.global nbif_ccallemu2
nbif_ccallemu2:
	movl	ARG1, P_ARG1(P)
#if X86_NR_ARG_REGS > 2
	movl	ARG2, ARG1
#else
	movl	4(NSP), ARG1
#endif
	/*FALLTHROUGH*/
#endif

#if X86_NR_ARG_REGS >= 1
	.align	4
	.global nbif_ccallemu1
nbif_ccallemu1:
	movl	ARG0, P_ARG0(P)
#if X86_NR_ARG_REGS > 1
	movl	ARG1, ARG0
#else
	movl	4(NSP), ARG0
#endif
	/*FALLTHROUGH*/
#endif

	.align	4
	.global nbif_ccallemu0
nbif_ccallemu0:
#if X86_NR_ARG_REGS == 0
	movl	4(NSP), %eax
#endif
	movl	%eax, P_CLOSURE(P)
	movl	$HIPE_MODE_SWITCH_RES_CALL_CLOSURE, %eax
	jmp	.suspend_exit

/*
 * This is where native code suspends.
 */
	.align	4
	.global	nbif_suspend_0
nbif_suspend_0:
	movl	$HIPE_MODE_SWITCH_RES_SUSPEND, %eax
	jmp	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	.align	4
	.global	nbif_suspend_msg
nbif_suspend_msg:
	movl	$HIPE_MODE_SWITCH_RES_WAIT, %eax
	jmp	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if( !(p->flags & F_TIMO) ) { suspend }
 *	else { p->flags ^= F_TIMO; return 0; }
 */
	.align	4
	.global	nbif_suspend_msg_timeout
nbif_suspend_msg_timeout:
	movl	P_FLAGS(P), %eax
	/* this relies on F_TIMO (1<<2) fitting in a byte */
	testb	$F_TIMO, %al			# F_TIMO set?
	jz	.no_timeout			# if not set, suspend
	/* timeout has occurred */
	/* XXX: The process will immediately execute 'clear_timeout',
	   repeating this statement. Remove it? */
	xorb	$F_TIMO, %al			# F_TIMO was set, clear it
	movl	%eax, P_FLAGS(P)
	xorl	%eax, %eax			# return 0 to signal timeout
	NSP_RET0
.no_timeout:
	movl	$HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT, %eax
	jmp	.suspend_exit

/*
 * int x86_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.align	4
	.global	x86_return_to_native
x86_return_to_native:
	ENTER_FROM_C
	/* get return value */
	movl	P_ARG0(P), %eax
	/*
	 * Return using the stacked return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	NSP_RET0

/*
 * int x86_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	.align	4
	.global	x86_tailcall_to_native
x86_tailcall_to_native:
	ENTER_FROM_C
	/* get argument registers */
	LOAD_ARG_REGS
	/* jump to the target label */
	jmp	*P_NCALLEE(P)

/*
 * int x86_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.align	4
	.global	x86_throw_to_native
x86_throw_to_native:
	ENTER_FROM_C
	/* find and invoke the handler */
	xorl	%eax, %eax	# p->narity == 0
	/*FALLTHROUGH*/

/*
 * Find and invoke catch handler (it must exist).
 * The stack/heap registers were just read from P. (XXX: useless knowledge?)
 * - %eax should contain the current call's arity
 */
	.align	4
.find_exn_handler:
	movl	%eax, P_NARITY(P)
	/* find and prepare to invoke the handler */
	SWITCH_ERLANG_TO_C
	pushl	P
	call	hipe_handle_exception	# Note: hipe_handle_exception() conses
	addl	$4, %esp
	SWITCH_C_TO_ERLANG		# %esp updated by hipe_find_handler()
	/* now invoke the handler */
	jmp	*P_NCALLEE(P)		# set by hipe_find_handler()

/*
 * This is the default exception handler for native code.
 */
	.align	4
	.global	nbif_fail
nbif_fail:
	movl	$HIPE_MODE_SWITCH_RES_THROW, %eax
	jmp	.flush_exit
	
/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * The stack/heap registers were just read from P.
 */
	.global	nbif_0_simple_exception
	.global	nbif_1_simple_exception
	.global	nbif_2_simple_exception
	.global	nbif_3_simple_exception
	.align	4
nbif_0_simple_exception:
	xorl	%eax, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_1_simple_exception:
	movl	$1, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_2_simple_exception:
	movl	$2, %eax
	jmp	.nbif_simple_exception
	.align	4
nbif_3_simple_exception:
	movl	$3, %eax
	/*FALLTHROUGH*/
	.align	4
.nbif_simple_exception:
	cmpl	$FREASON_TRAP, P_FREASON(P)
	jne	.find_exn_handler
	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF stored the callee's Export* in p->def_arg_reg[0]
	 * - the BIF stored the actual parameters in p->def_arg_reg[1..]
	 * - the BIF's arity is in %eax
	 * - the native heap/stack/reds registers are saved in P
	 */
	movl	%eax, P_ARITY(P)
	movl	$HIPE_MODE_SWITCH_RES_TRAP, %eax
	jmp	.nosave_exit

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE can occur.
 * %edx contains the address of the nbif which failed.
 * P_ARG0(P) is the first actual parameter, if X86_NR_ARG_REGS > 0.
 * P_ARG1(P) is the second actual parameter (if defined), if X86_NR_ARG_REGS > 1.
 */
	.global	nbif_1_hairy_exception
	.global	nbif_2_hairy_exception
	.align	4
nbif_1_hairy_exception:
	movl	$1, %eax
	jmp	.nbif_hairy_exception
	.align	4
nbif_2_hairy_exception:
	movl	$2, %eax
	/*FALLTHROUGH*/
	.align	4
.nbif_hairy_exception:
	cmpl	$FREASON_RESCHEDULE, P_FREASON(P)
	jne	.nbif_simple_exception
	/* handle reschedule */
	movl	%edx, P_NCALLEE(P)
#if X86_NR_ARG_REGS == 0
	movl	$0, P_ARITY(P)	# arity unused -- args on nstack
#elif X86_NR_ARG_REGS == 1
	movl	$1, P_ARITY(P)		# 1 even if arity is 2
#else
	movl	%eax, P_ARITY(P)	# 1 or 2
#endif
	movl	$HIPE_MODE_SWITCH_RES_RESCHEDULE, %eax
	jmp	.nosave_exit

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	.global	nbif_stack_trap_ra
	.align	4
nbif_stack_trap_ra:			# a return address, not a function
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	movl	%eax, TEMP_RV		# save retval
	SWITCH_ERLANG_TO_C_QUICK
	pushl	P
	call	hipe_handle_stack_trap	# must not cons; preserves TEMP_RV
	addl	$4, %esp
	movl	%eax, %edx		# original RA
	SWITCH_C_TO_ERLANG_QUICK
	movl	TEMP_RV, %eax		# restore retval
	jmp	*%edx			# resume at original RA

/*
 * nbif_inc_stack_0
 */
	.align	4
	.global	nbif_inc_stack_0
nbif_inc_stack_0:
	SWITCH_ERLANG_TO_C
	NBIF_SAVE_CALLER_SAVE
	pushl	P
	call	hipe_inc_nstack
	NBIF_RESTORE_CALLER_SAVE_1
	SWITCH_C_TO_ERLANG
	NSP_RET0
