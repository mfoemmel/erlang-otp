/* $Id$
 * hipe_x86_asm.h
 */

#define ASM
#include "hipe_mode_switch.h"

/*
 * Reserved registers.
 */
#define P	%ebp
#define HP	%esi

/*
 * Context switching macros.
 */
#define SWITCH_C_TO_ERLANG_QUICK	\
	movl	%esp, P_CSP(P);	\
	movl	P_NSP(P), %esp

#define SWITCH_ERLANG_TO_C_QUICK	\
	movl	%esp, P_NSP(P);	\
	movl	P_CSP(P), %esp

#define SWITCH_C_TO_ERLANG	\
	movl	P_HP(P), HP;	\
	SWITCH_C_TO_ERLANG_QUICK

#define SWITCH_ERLANG_TO_C	\
	movl	HP, P_HP(P);	\
	SWITCH_ERLANG_TO_C_QUICK

/*
 * Stacked argument access macros, used in BIF wrappers to
 * access parameters from the native-code Erlang stack.
 *
 * Usage:
 *	bif(X,Y,Z)
 * The arguments are numbered 0 (X), 1 (Y), 2 (Z), and ARITY is 3.
 * The arguments can be accessed with the syntax:
 *	movl	NSP_ARG(3,0)(%esp), %reg	# X
 *	movl	NSP_ARG(3,1)(%esp), %reg	# Y
 *	movl	NSP_ARG(3,2)(%esp), %reg	# Z
 *
 * Parameters are pushed left-to-right, to match BEAM closure callconv.
 */
#define NSP_ARG(ARITY,ARGNO)	ARITY*4-ARGNO*4

/*
 * Process struct offsets and some constants.
 */
#include "hipe_literals.h"
