/* $Id$
 * hipe_sparc_glue.S
 *
 */

#include "hipe_sparc_asm.h"

	.section ".text"

/*
 * int sparc_large_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.align	4
	.global	sparc_large_call_to_native
	.type	sparc_large_call_to_native,#function
	.proc	04
sparc_large_call_to_native:
	save %sp,-112,%sp
	st %i7,[P+P_CRA]			! save C return address
	ld [P+P_ARG3],ARG3			! Arg3
	ld [P+P_ARG4],ARG4			! Arg4
	ld [P+P_ARG5],ARG5			! Arg5
	ld [P+P_ARG6],ARG6			! Arg6
	ld [P+P_ARG7],ARG7			! Arg7
	ld [P+P_ARG8],ARG8			! Arg8
	ld [P+P_ARG9],ARG9			! Arg9	
	ld [P+P_ARG10],ARG10			! Arg10
	ld [P+P_ARG11],ARG11			! Arg11
	ld [P+P_ARG12],ARG12			! Arg12
	ld [P+P_ARG13],ARG13			! Arg13
	ld [P+P_ARG14],ARG14			! Arg14
	ba .load_globals
	ld [P+P_ARG15],ARG15			! Arg15

	.align	4
	.global	sparc_call_to_native
	.type	sparc_call_to_native,#function
	.proc	04
sparc_call_to_native:
	save %sp,-112,%sp
	st %i7,[P+P_CRA]			! save C return address
.load_globals:	
	ld [P+P_ARG0],ARG0			! Arg0
	ld [P+P_ARG1],ARG1			! Arg1
	ld [P+P_ARG2],ARG2			! Arg2
	ld [P+P_FCALLS],FCALLS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_NCALLEE],TEMP0			! call address	
/*
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 * Note: this is SPARC, so the value in the return address register
 * is the address of the call/jmpl instruction itself.
 */
	.global nbif_return
nbif_return:
	jmpl TEMP0,RA				! Call native code
	nop
/*
 * This is where native code returns to emulated code.
 * XXX:	Needs fix for multiple return values.	
 */
	st %o0,[P+P_ARG0]			! save retval
	ba .flush_exit
	mov HIPE_MODE_SWITCH_RES_RETURN,TEMP0

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
	/* TEMP0  contains callee's BEAM code address
	 * TEMP1 contains callee's arity
	 * TEMP2 contains native RA (current RA/%o7 contains junk)
	 */
	st TEMP0,[P+P_BEAM_IP]			! callee
	st TEMP1,[P+P_ARITY]			! arity
	st TEMP2,[P+P_NRA]			! native return address
	st ARG0,[P+P_ARG0]			! Arg0
	st ARG1,[P+P_ARG1]			! Arg1
	st ARG2,[P+P_ARG2]			! Arg2
	st ARG3,[P+P_ARG3]			! Arg3
	st ARG4,[P+P_ARG4]			! Arg4
	st ARG5,[P+P_ARG5]			! Arg5
	st ARG6,[P+P_ARG6]			! Arg6
	st ARG7,[P+P_ARG7]			! Arg7
	st ARG8,[P+P_ARG8]			! Arg8
	st ARG9,[P+P_ARG9]			! Arg9
	st ARG10,[P+P_ARG10]			! Arg10
	st ARG11,[P+P_ARG11]			! Arg11
	st ARG12,[P+P_ARG12]			! Arg12
	st ARG13,[P+P_ARG13]			! Arg13
	st ARG14,[P+P_ARG14]			! Arg14	
	st ARG15,[P+P_ARG15]			! Arg15	
	ba .flush_exit
	mov HIPE_MODE_SWITCH_RES_CALL,TEMP0

/*
 * nbif_apply
 */
	.global	nbif_apply
nbif_apply:
	st	ARG0, [P+P_ARG0]
	st	ARG1, [P+P_ARG1]
	st	ARG2, [P+P_ARG2]
	ba	.suspend_exit
	mov	HIPE_MODE_SWITCH_RES_APPLY, TEMP0

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first 5 are pushed onto the stack in left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */

	.global nbif_ccallemu16
	.global nbif_ccallemu15
	.global nbif_ccallemu14
	.global nbif_ccallemu13
	.global nbif_ccallemu12
	.global nbif_ccallemu11
	.global nbif_ccallemu10
	.global nbif_ccallemu9
	.global nbif_ccallemu8
	.global nbif_ccallemu7
	.global nbif_ccallemu6
	.global nbif_ccallemu5
	.global nbif_ccallemu4
	.global nbif_ccallemu3
	.global nbif_ccallemu2
	.global nbif_ccallemu1
	.global nbif_ccallemu0

nbif_ccallemu16:
	ld [NSP-4], TEMP0
	ba .args15
	st TEMP0,[P+P_CLOSURE]
nbif_ccallemu15:
	ba .args14
	st ARG15,[P+P_CLOSURE]
nbif_ccallemu14:
	ba .args13
	st ARG14,[P+P_CLOSURE]
nbif_ccallemu13:
	ba .args12
	st ARG13,[P+P_CLOSURE]
nbif_ccallemu12:
	ba .args11
	st ARG12,[P+P_CLOSURE]
nbif_ccallemu11:
	ba .args10
	st ARG11,[P+P_CLOSURE]
nbif_ccallemu10:
	ba .args9
	st ARG10,[P+P_CLOSURE]
nbif_ccallemu9:
	ba .args8
	st ARG9,[P+P_CLOSURE]
nbif_ccallemu8:
	ba .args7
	st ARG8,[P+P_CLOSURE]
nbif_ccallemu7:
	ba .args6
	st ARG7,[P+P_CLOSURE]
nbif_ccallemu6:
	ba .args5
	st ARG6,[P+P_CLOSURE]
nbif_ccallemu5:
	ba .args4
	st ARG5,[P+P_CLOSURE]	
nbif_ccallemu4:
	ba .args3
	st ARG4,[P+P_CLOSURE]
nbif_ccallemu3:
	ba .args2
	st ARG3,[P+P_CLOSURE]
nbif_ccallemu2:
	ba .args1
	st ARG2,[P+P_CLOSURE]
nbif_ccallemu1:
	ba .args0
	st ARG1,[P+P_CLOSURE]
nbif_ccallemu0:
	ba .ccall
	st ARG0,[P+P_CLOSURE]

.args15:	st ARG15,[P+P_ARG15]	
.args14:	st ARG14,[P+P_ARG14]
.args13:	st ARG13,[P+P_ARG13]
.args12:	st ARG12,[P+P_ARG12]
.args11:	st ARG11,[P+P_ARG11]
.args10:	st ARG10,[P+P_ARG10]
.args9:		st ARG9,[P+P_ARG9]
.args8:		st ARG8,[P+P_ARG8]
.args7:		st ARG7,[P+P_ARG7]
.args6:		st ARG6,[P+P_ARG6]
.args5:		st ARG5,[P+P_ARG5]
.args4:		st ARG4,[P+P_ARG4]
.args3:		st ARG3,[P+P_ARG3]
.args2:		st ARG2,[P+P_ARG2]
.args1:		st ARG1,[P+P_ARG1]
.args0:		st ARG0,[P+P_ARG0]
.ccall:		ba .suspend_exit
		mov HIPE_MODE_SWITCH_RES_CALL_CLOSURE,TEMP0

/*
 * This is where native code suspends.
 */
	.align 4
	.global nbif_suspend_0
nbif_suspend_0:
	ba .suspend_exit
	mov HIPE_MODE_SWITCH_RES_SUSPEND,TEMP0

/*
 * Suspend from a receive (waiting for a message)
 */
	.align 4
	.global nbif_suspend_msg
nbif_suspend_msg:
	ba .suspend_exit
	mov HIPE_MODE_SWITCH_RES_WAIT,TEMP0

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if( !(p->flags & F_TIMO) ) { suspend }
 *	else { p->flags ^= F_TIMO; return 0; }
 */
	.align 4
	.global nbif_suspend_msg_timeout
nbif_suspend_msg_timeout:
	ld [P+P_FLAGS],TEMP1			! Check if timeout
	!! this relies on F_TIMO (1<<2) fitting in an immediate...
	andcc TEMP1,F_TIMO,%g0			! F_TIMO set?
	bz,a .suspend_exit			! if not set, suspend
	mov HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT,TEMP0 ! .. and set C retval
	!! timeout has occurred
	xor TEMP1,F_TIMO,TEMP1			! F_TIMO was set, clear it
	st TEMP1,[P+P_FLAGS]
	jmp RA+8				! retl
	mov 0,%o0

/*
 * Return to the calling C function.
 * The return value is in TEMP0.
 *
 * .flush_exit saves FCALLS, NSP, HP 
 * .suspend_exit also saves RA
 */
.suspend_exit:	! like .flush_exit, but save RA too
	st	RA, [P+P_NRA]
.flush_exit:	! call with P intact and RV in TEMP0
	ld	[P+P_CRA], %i7			! restore C return address
	st	FCALLS, [P+P_FCALLS]
	st	NSP, [P+P_NSP]
	st	HP, [P+P_HP]
	jmp	%i7+8				! ret
	restore	%g0,TEMP0,%o0

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
	st %i7,[P+P_CRA]			! save C return address
	ld [P+P_FCALLS],FCALLS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_ARG0],%o0			! retval
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
	st %i7,[P+P_CRA]			! save C return address
	ld [P+P_NCALLEE],TEMP0			! call address
	ld [P+P_FCALLS],FCALLS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ld [P+P_ARG0],ARG0			! Arg0
	ld [P+P_ARG1],ARG1			! Arg1
	ld [P+P_ARG2],ARG2			! Arg2
	ld [P+P_ARG3],ARG3			! Arg3
	ld [P+P_ARG4],ARG4			! Arg4
	ld [P+P_ARG5],ARG5			! Arg5
	ld [P+P_ARG6],ARG6			! Arg6
	ld [P+P_ARG7],ARG7			! Arg7
	ld [P+P_ARG8],ARG8			! Arg8
	ld [P+P_ARG9],ARG9			! Arg9
	ld [P+P_ARG10],ARG10			! Arg10
	ld [P+P_ARG11],ARG11			! Arg11
	ld [P+P_ARG12],ARG12			! Arg12
	ld [P+P_ARG14],ARG14			! Arg14
	ld [P+P_ARG15],ARG15			! Arg15			
	ld [P+P_NRA],RA				! native return address
	jmp TEMP0				! Call native code
	ld [P+P_ARG13],ARG13			! Arg13


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
	st %i7,[P+P_CRA]			! save C return address
	ld [P+P_FCALLS],FCALLS			! fcalls
	ld [P+P_NSP],NSP			! nstop
	ld [P+P_NSP_LIMIT],NSP_LIMIT		! nstack_max
	ld [P+P_HP],HP				! htop
	ld [P+P_HP_LIMIT],HP_LIMIT		! heap_margin
	ba .find_exn_handler_1
	nop

/*
 * Find and invoke catch handler (it must exist).
 * The stack/heap registers were just read from P.
 */
.find_exn_handler:
	/* find and prepare to invoke the handler */
	st	TEMP3, [P+P_NRA]  	! TEMP3 = RetAdr, save to find current sdesc
.find_exn_handler_1:
	call	hipe_handle_exception
	mov	P, %o0                  ! (delayslot)
	
	ld	[P+P_HP],HP		! hipe_handle_exception() conses
	/* now invoke the handler */
	ld	[P+P_NCALLEE], %o1	! updated by hipe_find_handler()
	ld	[P+P_NSP], NSP		! updated by hipe_find_handler()
	jmp	%o1
	ld	[P+P_FVALUE], %o0

/*
 * This is the default exception handler for native code.
 */
	.global	nbif_fail
nbif_fail:
	ba	.flush_exit
	mov	HIPE_MODE_SWITCH_RES_THROW, TEMP0

/*
 * We end up here when a bif called from native signals an
 * exceptional condition, and RESCHEDULE cannot occur.
 * The stack/heap registers were just read from P.
 * TEMP3 contains the native return address.
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
	ld	[P+P_FREASON],ARG1
	cmp	ARG1,FREASON_TRAP
	bne	.find_exn_handler
	nop
	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF stored the callee's Export* in p->def_arg_reg[0]
	 * - the BIF stored the actual parameters in p->def_arg_reg[1..]
	 * - the BIF's arity is in ARG4
	 * - the native RA was saved in TEMP3 before the BIF call
	 * - the native heap/stack/reds registers are saved in P
	 */
	mov	HIPE_MODE_SWITCH_RES_TRAP, TEMP0
.bif_exit:
	ld	[P+P_CRA], %i7			! restore C return address
	st	ARG4, [P+P_ARITY]
	st	TEMP3, [P+P_NRA]		! RA
	jmp	%i7+8
	restore	%g0,TEMP0,%o0

/*
 * We end up here when a bif called from native signals an
 * exceptional condition, and RESCHEDULE _CAN_ occur.
 * TEMP3 contains the native return address, and the
 * stack/heap/reds registers were just read from P.
 * TEMP0 contains the address of the nbif which failed
 * TEMP1 contains the first actual parameter
 * TEMP2 contains the second actual parameter (if it is defined)
 * ARG4 contains the number of parameters (1 or 2)
 */
	.align	4
	.global	nbif_hairy_exception
nbif_hairy_exception:
	ld	[P+P_FREASON],ARG1	! must not clobber bif retval in %o0
	cmp	ARG1,FREASON_RESCHEDULE
	bne	.nbif_simple_exception
	nop
	/* handle reschedule */
	st	TEMP0,[P+P_NCALLEE]
	st	TEMP1,[P+P_ARG0]
	st	TEMP2,[P+P_ARG1]
	ba	.bif_exit
	mov	HIPE_MODE_SWITCH_RES_RESCHEDULE, TEMP0

/*
 * nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 * XXX:	Fix for multiple returnvalues.	
 */
	.global	nbif_stack_trap_ra
	.align	4
nbif_stack_trap_ra:			! a return address, not a function
	nop                             ! Simulate ret adr
	nop
	st %o0,[P+P_ARG0]
	mov P,%o0
	!! Save registers and call the C function
	st FCALLS,[P+P_FCALLS]
	st HP,[P+P_HP]
	call hipe_handle_stack_trap	! must not cons
	st NSP,[P+P_NSP]                !  (delayslot)
	
                                        ! Returns 
	mov %o0, TEMP0	                ! original RA    
	!! Restore registers and return
	ld [P+P_HP_LIMIT],HP_LIMIT
	ld [P+P_NSP_LIMIT],NSP_LIMIT
	ld [P+P_FCALLS],FCALLS
	ld [P+P_HP],HP
	!! We only restore argument1
	ld [P+P_ARG0], %o0
	jmpl TEMP0+8,%g0                ! resume at original RA
	ld [P+P_NSP],NSP                !  (delayslot)

/*   This procedure is called when nativecode runs out of stack.
 *   There is a special calling convention for this function
 *   in order to minimize code duplication.
 *   Each non-leaf function may have to call this procedure.
 *   The call is done by storing the previous returnaddress
 *   in TEMP2. 
 *   This procedure is responsible of storing the
 *   argumentents in ARG0 to ARG15 and restoring them afterwards.
 *   It assumes that hipe_inc_nstack dont use local reg 
 *   (true as long as hipe_inc_nstack is implemented in C and
 *    uses register windows.)
 *		
 *   !!! THIS procedure may not use TEMP2
 */
	.align	4
	.global	nbif_inc_stack_16args	
	.global	nbif_inc_stack_15args
	.global	nbif_inc_stack_14args
	.global	nbif_inc_stack_13args	
	.global	nbif_inc_stack_12args	
	.global	nbif_inc_stack_11args	
	.global	nbif_inc_stack_10args
	.global	nbif_inc_stack_9args
	.global	nbif_inc_stack_8args	
	.global	nbif_inc_stack_7args	
	.global	nbif_inc_stack_6args	
	.global	nbif_inc_stack_5args
	.global	nbif_inc_stack_4args
	.global	nbif_inc_stack_3args	
	.global	nbif_inc_stack_2args	
	.global	nbif_inc_stack_1args	
	.global	nbif_inc_stack_0args	
	
nbif_inc_stack_16args:	
	st ARG15,[P+P_ARG15]
nbif_inc_stack_15args:	
	st ARG14,[P+P_ARG14]
nbif_inc_stack_14args:	
	st ARG13,[P+P_ARG13]
nbif_inc_stack_13args:	
	st ARG12,[P+P_ARG12]
nbif_inc_stack_12args:
	st ARG11,[P+P_ARG11]
nbif_inc_stack_11args:	
	st ARG10,[P+P_ARG10]
nbif_inc_stack_10args:	
	st ARG9,[P+P_ARG9]
nbif_inc_stack_9args:	
	st ARG8,[P+P_ARG8]
nbif_inc_stack_8args:	
	st ARG7,[P+P_ARG7]
nbif_inc_stack_7args:
	st ARG6,[P+P_ARG6]
nbif_inc_stack_6args:	
	st ARG5,[P+P_ARG5]
nbif_inc_stack_5args:	
	st ARG4,[P+P_ARG4]
nbif_inc_stack_4args:	
	st ARG3,[P+P_ARG3]
nbif_inc_stack_3args:	
	st ARG2,[P+P_ARG2]
nbif_inc_stack_2args:
	st ARG1,[P+P_ARG1]
nbif_inc_stack_1args:	
	st ARG0,[P+P_ARG0]
nbif_inc_stack_0args:
	!! save RA
	mov RA, TEMP1

	!! The argument to hipe_inc_stack is the PCB.
	mov P, %o0	
	!! Save registers and call the C function
	st FCALLS,[P+P_FCALLS]
	st HP,[P+P_HP]

	call hipe_inc_nstack
	st NSP,[P+P_NSP]       !! Delayslot

	!! Restore registers and return
	ld [P+P_HP_LIMIT],HP_LIMIT
	ld [P+P_NSP_LIMIT],NSP_LIMIT
	ld [P+P_FCALLS],FCALLS
	ld [P+P_HP],HP
	!! We restore all arguments, even unused ones. 	
	ld [P+P_ARG15], ARG15
	ld [P+P_ARG14], ARG14
	ld [P+P_ARG13], ARG13
	ld [P+P_ARG12], ARG12
	ld [P+P_ARG11], ARG11
	ld [P+P_ARG10], ARG10
	ld [P+P_ARG9], ARG9
	ld [P+P_ARG8], ARG8
	ld [P+P_ARG7], ARG7
	ld [P+P_ARG6], ARG6
	ld [P+P_ARG5], ARG5
	ld [P+P_ARG4], ARG4
	ld [P+P_ARG3], ARG3
	ld [P+P_ARG2], ARG2
	ld [P+P_ARG1], ARG1
	ld [P+P_ARG0], ARG0
	jmpl TEMP1+8,%g0
	ld [P+P_NSP],NSP
