changecom(`/*', `*/')dnl
/*
 * $Id$
 */

include(`hipe/hipe_ppc_asm.m4')
#`include' "hipe_literals.h"

/*
 * XXX: TODO:
 * - Can a BIF with arity 0 fail? beam_emu doesn't think so.
 * - All standard BIF interfaces save RA in the PCB now. This
 *   is needed for those BIFs that can trigger a walk of the
 *   native stack, which includes those that can do a gc.
 *   Ideally this overhead should only be imposed for those BIFs
 *   that actually need it, but that set changes from time to
 *   time, and is difficult for us to track.
 * - Can a GC:ing BIF change P_NRA(P) due to the stack trap thingy?
 */

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
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	r3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(0)
1:	/* workaround for bc:s small offset operand */
	b	nbif_0_simple_exception
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(1)
1:	/* workaround for bc:s small offset operand */
	b	nbif_1_simple_exception
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	r3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(2)
1:	/* workaround for bc:s small offset operand */
	b	nbif_2_simple_exception
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,3,0)
	NBIF_ARG(r5,3,1)
	NBIF_ARG(r6,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	r3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(3)
1:	/* workaround for bc:s small offset operand */
	b	nbif_3_simple_exception
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save actual parameters in case we must reschedule. */
	NBIF_SAVE_RESCHED_ARGS(1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	r3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(1)
1:
	addi	r5, 0, $1@l
	addis	r5, r5, $1@ha
	b	nbif_1_hairy_exception
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save actual parameters in case we must reschedule. */
	NBIF_SAVE_RESCHED_ARGS(2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. Check for exception. */
	cmpwi	r3, THE_NON_VALUE
	RESTORE_CONTEXT
	beq-	1f
	NBIF_RET(2)
1:
	addi	r5, 0, $1@l
	addis	r5, r5, $1@ha
	b	nbif_2_hairy_exception
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
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT
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
	/* Set up C argument registers. */
	mr	r3, P
	NBIF_ARG(r4,2,0)
	NBIF_ARG(r5,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT
	NBIF_RET(2)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with implicit P
 * parameter, 0 ordinary parameters, and no failure mode.
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
	/* Set up C argument registers. */
	mr	r3, P

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
	NBIF_RET(0)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/* 
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 *
 * Generate native interface for a primop with no implicit P
 * parameter, 0-3 ordinary parameters, and no failure mode.
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
	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
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
	/* Set up C argument registers. */
	NBIF_ARG(r3,1,0)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
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
	/* Set up C argument registers. */
	NBIF_ARG(r3,2,0)
	NBIF_ARG(r4,2,1)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
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
	/* Set up C argument registers. */
	NBIF_ARG(r3,3,0)
	NBIF_ARG(r4,3,1)
	NBIF_ARG(r5,3,2)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
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
	/* Set up C argument registers. */
	NBIF_ARG(r3,5,0)
	NBIF_ARG(r4,5,1)
	NBIF_ARG(r5,5,2)
	NBIF_ARG(r6,5,3)
	NBIF_ARG(r7,5,4)

	/* Save caller-save registers and call the C function. */
	SAVE_CONTEXT_QUICK
	bl	$2

	/* Restore registers. */
	RESTORE_CONTEXT_QUICK
	NBIF_RET(5)
	.size	$1,.-$1
	.type	$1,@function
#endif')

/*
 * BIFs that may trigger a native stack walk with p->narity != 0.
 * Relevant on PPC when NR_ARG_REGS < 2.
 */
standard_bif_interface_2(nbif_check_process_code_2, hipe_check_process_code_2)
standard_bif_interface_1(nbif_garbage_collect_1, hipe_garbage_collect_1)

/*
 * Implement gc_nofail_primop_interface_1 as nofail_primop_interface_1.
 */
define(gc_nofail_primop_interface_1,`nofail_primop_interface_1($1, $2)')

include(`hipe/hipe_bif_list.m4')
