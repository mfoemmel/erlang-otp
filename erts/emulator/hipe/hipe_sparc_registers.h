/* Copyright (c) 2002 by Erik Johansson */

/***************************************************************************
*   NAME
*     hipe_sparc_registers.h
*   PURPOSE
*     To provide symbolic names for SPARC registers for both mk_literals 
*     and for use in spac glue code like hipe_sparc_glue.S and 
*     hipe_sparc_bifs.m4.
*
*   NOTES
*     See hipe_sparc_abi.txt for details.
*     Be very careful when editing this file -- subtle aspects of how registers
*     are used may be assumed to hold. (e.g. P is in a C - callee save register.).
*     Once again: read hipe_sparc_abi.txt before editing this file, and 
*     if you edit this file update hipe_sparc_abi.txt !!!
*   
*     All names are defined twice, symbolically for the .S files and numerically 
*     for hipe_mk_literals (which creates hipe_literals.hrl used by 
*     hipe_sparc_registers.erl). 
*     (If you can come up with a better solution requiering only one define 
*     I would be grateful....
*
*   HISTORY
*     Erik Johansson (happi@csd.uu.se) - Mar 07, 2002: Created.
****************************************************************************/

#ifndef __HIPE_SPARC_REGISTERS_H__
#define __HIPE_SPARC_REGISTERS_H__
/* ------------------ Defines ------------------ */
/*
 * Reserved registers
 * See hipe_sparc_abi.txt
 */
#define G0 0
#define G1 1
#define G2 2
#define G3 3
#define G4 4
#define G5 5
#define G6 6
#define G7 7
#define O0 8
#define O1 9
#define O2 10
#define O3 11
#define O4 12
#define O5 13
#define O6 14
#define O7 15
#define L0 16
#define L1 17
#define L2 18
#define L3 19
#define L4 20
#define L5 21
#define L6 22
#define L7 23
#define I0 24
#define I1 25
#define I2 26
#define I3 27
#define I4 28
#define I5 29
#define I6 30
#define I7 31

#define P		%i0
#define HP		%i1	/* XXX: heap pointer, née HEAP */
#define HP_LIMIT	%i2
#define NSP		%i3	/* XXX: stack pointer, née NSTACK */
#define NSP_LIMIT	%i4
#define FCALLS		%i5	


#define RA		%o7	/* XXX: return address, née CP */

#define TEMP0		%g1
#define TEMP1		%l7     /* Should be local registers so that */
#define TEMP2		%l6     /*   they are saved over C-calls.    */
#define TEMP3		%l5     /*                                   */

#define ARG0		%o1
#define ARG1		%o2
#define ARG2		%o3
#define ARG3		%o4
#define ARG4		%o5
#define ARG5		%l0
#define ARG6		%l1
#define ARG7		%l2
#define ARG8		%l3
#define ARG9		%l4
#define ARG10		%g2
#define ARG11		%g3
#define ARG12		%g4
#define ARG13		%g5
#define ARG14		%i7
#define ARG15		%o0


#define P_NR		I0
#define HP_NR		I1	/* XXX: heap pointer, née HEAP */
#define HP_LIMIT_NR	I2
#define NSP_NR		I3	/* XXX: stack pointer, née NSTACK */
#define NSP_LIMIT_NR	I4
#define FCALLS_NR	I5

#define RA_NR		O7	/* XXX: return address, née CP */

#define TEMP0_NR	G1
#define TEMP1_NR	L7     /* Should be local registers so that */
#define TEMP2_NR	L6     /*   they are saved over C-calls.    */
#define TEMP3_NR	L5     /*                                   */

#define ARG0_NR		O1
#define ARG1_NR		O2
#define ARG2_NR		O3
#define ARG3_NR		O4
#define ARG4_NR		O5
#define ARG5_NR		L0
#define ARG6_NR		L1
#define ARG7_NR		L2
#define ARG8_NR		L3
#define ARG9_NR		L4
#define ARG10_NR	G2
#define ARG11_NR	G3
#define ARG12_NR	G4
#define ARG13_NR	G5
#define ARG14_NR	I7
#define ARG15_NR	O0    /* Ret val */

#endif /* __HIPE_SPARC_REGISTERS_H__ */

