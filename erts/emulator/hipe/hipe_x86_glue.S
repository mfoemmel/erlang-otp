/* $Id$
 * hipe_x86_glue.S
 */

#include "hipe_x86_asm.h"

	.section ".text"

/*
 * int x86_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.align	4
	.global	x86_call_to_native
	.global	nbif_return
x86_call_to_native:
	/* save callee-save registers on the C stack */
	subl	$16, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx, 8(%esp)
	movl	%esi, 4(%esp)
	movl	%edi, (%esp)
	/* get the process pointer */
	movl	20(%esp), P
	/* switch to native stack */
	SWITCH_C_TO_ERLANG
	/* call the target */
	movl	P_NCALLEE(P), %eax
	.align	4
	.byte	0x89, 0xDB	# movl %ebx, %ebx (2-byte nop)
	.byte	0xFF, 0xD0	# call *%eax
/*
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
nbif_return:	# this label must be 4-byte aligned
	movl	%eax, P_ARG0(P)			# save retval
	movl	$HIPE_MODE_SWITCH_RES_RETURN, %eax
/* FALLTHROUGH
 *
 * Return to the calling C function with result token in %eax.
 */
.exit:
	/* switch to C stack */
	SWITCH_ERLANG_TO_C
	/* restore callee-save registers, drop frame, return */
	movl	(%esp), %edi
	movl	4(%esp), %esi	# kills HP
	movl	8(%esp), %ebx
	movl	12(%esp), %ebp	# kills P
	addl	$16, %esp
	ret

/*
 * Native code calls emulated code via a linker-generated
 * stub which should look as follows:
 *
 * stub for f/N:
 *	movl	$<f's BEAM code address>, %eax
 *	movl	$<N>, %edx
 *	jmp	nbif_callemu
 */
	.align	4
	.global	nbif_callemu
nbif_callemu:
	movl	%eax, P_BEAM_IP(P)
	movl	%edx, P_ARITY(P)
	movl	$HIPE_MODE_SWITCH_RES_CALL, %eax
	jmp	.exit

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * are pushed in left-to-right order. Hence, the closure parameter is
 * always at the bottom of the stack, just above the return address.
 */
	.align	4
	.global	nbif_closure_call_emu
nbif_closure_call_emu:
	movl	4(%esp), %eax
	movl	%eax, P_CLOSURE(P)
	movl	$HIPE_MODE_SWITCH_RES_CALL_CLOSURE, %eax
	jmp	.exit

/*
 * This is where native code suspends.
 */
	.align	4
	.global	nbif_suspend_0
nbif_suspend_0:
	movl	$HIPE_MODE_SWITCH_RES_SUSPEND, %eax
	jmp	.exit

/*
 * Suspend from a receive (waiting for a message)
 */
	.align	4
	.global	nbif_suspend_msg
nbif_suspend_msg:
	movl	$HIPE_MODE_SWITCH_RES_WAIT, %eax
	jmp	.exit

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
	xorb	$F_TIMO, %al			# F_TIMO was set, clear it
	movl	%eax, P_FLAGS(P)
	xorl	%eax, %eax			# return 0 to signal timeout
	ret
.no_timeout:
	movl	$HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT, %eax
	jmp	.exit

/*
 * int x86_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.align	4
	.global	x86_return_to_native
x86_return_to_native:
	/* save callee-save registers on the C stack */
	subl	$16, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx, 8(%esp)
	movl	%esi, 4(%esp)
	movl	%edi, (%esp)
	/* get the process pointer */
	movl	20(%esp), P
	/* switch to native stack */
	SWITCH_C_TO_ERLANG
	/* get return value */
	movl	P_ARG0(P), %eax
	/*
	 * Return using the stacked return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	ret

/*
 * int x86_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	.align	4
	.global	x86_tailcall_to_native
x86_tailcall_to_native:
	/* save callee-save registers on the C stack */
	subl	$16, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx, 8(%esp)
	movl	%esi, 4(%esp)
	movl	%edi, (%esp)
	/* get the process pointer */
	movl	20(%esp), P
	/* switch to native stack */
	SWITCH_C_TO_ERLANG
	/* jump to the target label */
	jmp	*P_NCALLEE(P)

/*
 * int x86_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.align	4
	.global	x86_throw_to_native
x86_throw_to_native:
	/* save callee-save registers on the C stack */
	subl	$16, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx, 8(%esp)
	movl	%esi, 4(%esp)
	movl	%edi, (%esp)
	/* get the process pointer */
	movl	20(%esp), P
	/* switch to native stack */
	SWITCH_C_TO_ERLANG
	/* find and invoke the handler */
	xorl	%eax, %eax	# p->narity == 0
	jmp	.find_exn_handler

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
	movl	P_FVALUE(P), %eax
	jmp	*P_NCALLEE(P)		# set by hipe_find_handler()

/*
 * This is the default exception handler for native code.
 */
	.align	4
	.global	nbif_fail
nbif_fail:
	movl	$HIPE_MODE_SWITCH_RES_THROW, %eax
	jmp	.exit
	
/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * The stack/heap registers were just read from P. (XXX: useless knowledge?)
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
	 * - the BIF stored the callee's Export* in p->fvalue
	 * - the BIF stored the actual parameters in p->def_arg_reg[]
	 * - the arity is the same as the BIF's; our glue put it in %eax
	 */
	movl	%eax, P_ARITY(P)
	movl	$HIPE_MODE_SWITCH_RES_TRAP, %eax
	jmp	.exit

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition, and RESCHEDULE can occur.
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
	/* handle reschedule: resumption address in %edx, arity in %eax */
	movl	%edx, P_NCALLEE(P)
	movl	$0, P_ARITY(P)	# arity unused -- args on nstack
	movl	$HIPE_MODE_SWITCH_RES_RESCHEDULE, %eax
	jmp	.exit

/*
 * nbif_test: placeholder for hand-written test code
 */
	.global	nbif_test
	.align	4
nbif_test:
	movl	$((42<<4)+15), %eax	# make_small(42)
	ret
