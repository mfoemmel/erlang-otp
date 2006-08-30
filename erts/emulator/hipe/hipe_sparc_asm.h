/*
 * $Id$
 */
#ifndef HIPE_SPARC_ASM_H
#define HIPE_SPARC_ASM_H

#define P		%i0
#define HP		%i1
#define HP_LIMIT	%i2
#define NSP		%i3
#define NSP_LIMIT	%i4
#define FCALLS		%i5

#define RA		%o7

#define TEMP0		%g1
#define TEMP1		%l7	/* Should be local registers so that */
#define TEMP2		%l6	/*   they are saved over C-calls.    */
#define TEMP3		%l5	/*                                   */

#define ARG0		%o1
#define ARG1		%o2
#define ARG2		%o3
#define ARG3		%o4
#define ARG4		%o5
#define ARG5		%o0	/* also retval */

#define SPARC_NR_ARG_REGS 6	/* Stored in p->def_arg_reg[]. */

/* Guaranteed min stack size for leaf functions on SPARC. */
#define SPARC_LEAF_WORDS 20

#endif /* HIPE_SPARC_ASM_H */
