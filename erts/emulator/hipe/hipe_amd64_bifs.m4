changecom(`/*', `*/')dnl
/*
 * $Id$
 */

include(`hipe/hipe_amd64_asm.m4')
#`include' "hipe_literals.h"

/*
 * XXX:
 * - Does not yet support C BIFs with >6 parameters.
 * - The noproc primops should be compiled to use C's argument
 *   passing convention, to eliminate register moves in the wrappers.
 */

`#if THE_NON_VALUE == 0
#define TEST_GOT_EXN	testq	%rax, %rax
#else
#define TEST_GOT_EXN	cmpq	$THE_NON_VALUE, %rax
#endif'

/*
 * standard_bif_interface_0(nbif_name, cbif_name)
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 0-3 parameters and
 * standard failure mode (may fail, but not with RESCHEDULE).
 */
define(standard_bif_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_0_simple_exception
	NBIF_RET(0)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(standard_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_1_simple_exception
	NBIF_RET(1)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(standard_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_2_simple_exception
	NBIF_RET(2)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(standard_bif_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,3,0)
	NBIF_ARG(%rdx,3,1)
	NBIF_ARG(%rcx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	nbif_3_simple_exception
	NBIF_RET(3)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * expensive_bif_interface_1(nbif_name, cbif_name)
 * expensive_bif_interface_2(nbif_name, cbif_name)
 *
 * Generate native interface for a BIF with 1-2 parameters and
 * an expensive failure mode (may fail with RESCHEDULE).
 */
define(expensive_bif_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* save actual parameters in case we must reschedule */
	NBIF_SAVE_RESCHED_ARGS(1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	1f
	NBIF_RET(1)
1:
	movq	`$'$1, %rdx	/* resumption address */
	jmp	nbif_1_hairy_exception
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(expensive_bif_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* save actual parameters in case we must reschedule */
	NBIF_SAVE_RESCHED_ARGS(2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* throw exception if failure, otherwise return */
	TEST_GOT_EXN
	jz	1f
	NBIF_RET(2)
1:
	movq	`$'$1, %rdx	/* resumption address */
	jmp	nbif_2_hairy_exception
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-2 ordinary parameters and no failure mode.
 * Also used for guard BIFs.
 */
define(nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(0)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(1)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C
	call	$2	
	SWITCH_C_TO_ERLANG

	/* return */
	NBIF_RET(2)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(nocons_nofail_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(0)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nocons_nofail_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(1)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nocons_nofail_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,2,0)
	NBIF_ARG(%rdx,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(2)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nocons_nofail_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,3,0)
	NBIF_ARG(%rdx,3,1)
	NBIF_ARG(%rcx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(3)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(nocons_nofail_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	movq	P, %rdi
	NBIF_ARG(%rsi,5,0)
	NBIF_ARG(%rdx,5,1)
	NBIF_ARG(%rcx,5,2)
	NBIF_ARG(%r8,5,3)
	NBIF_ARG(%r9,5,4)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(5)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/* 
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with no implicit P
 * parameter, 0-3 or 5 ordinary parameters, and no failure mode.
 * The primop cannot CONS or gc.
 */
define(noproc_primop_interface_0,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(0)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(noproc_primop_interface_1,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	NBIF_ARG(%rdi,1,0)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(1)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(noproc_primop_interface_2,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	NBIF_ARG(%rdi,2,0)
	NBIF_ARG(%rsi,2,1)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(2)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(noproc_primop_interface_3,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	NBIF_ARG(%rdi,3,0)
	NBIF_ARG(%rsi,3,1)
	NBIF_ARG(%rdx,3,2)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(3)
	.size	$1,.-$1
	.type	$1,@function
#endif')

define(noproc_primop_interface_5,
`
#ifndef HAVE_$1
#`define' HAVE_$1
	.section ".text"
	.align	4
	.global	$1
$1:
	/* set up the parameters */
	NBIF_ARG(%rdi,5,0)
	NBIF_ARG(%rsi,5,1)
	NBIF_ARG(%rdx,5,2)
	NBIF_ARG(%rcx,5,3)
	NBIF_ARG(%r8,5,4)

	/* make the call on the C stack */
	SWITCH_ERLANG_TO_C_QUICK
	call	$2
	SWITCH_C_TO_ERLANG_QUICK

	/* return */
	NBIF_RET(5)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * AMD64-specific primops.
 */
noproc_primop_interface_0(nbif_handle_fp_exception, erts_restore_fpu)

/*
 * BIFs that may trigger a native stack walk with p->narity != 0.
 * Relevant when NR_ARG_REGS < 2.
 */
standard_bif_interface_2(nbif_check_process_code_2, hipe_x86_check_process_code_2)
standard_bif_interface_1(nbif_garbage_collect_1, hipe_x86_garbage_collect_1)

/*
 * Implement gc_nofail_primop_interface_1 as nofail_primop_interface_1.
 */
define(gc_nofail_primop_interface_1,`nofail_primop_interface_1($1, $2)')

include(`hipe/hipe_bif_list.m4')
