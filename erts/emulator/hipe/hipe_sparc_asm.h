/* $Id$
 * hipe_sparc_asm.h
 */

#define ASM
#include "hipe_mode_switch.h"

/*
 * Reserved registers
 */
#define TEMP2		%l4
#define REDS		%l5	/* XXX: should be renamed to FCALLS */
#define NSP		%l6	/* XXX: stack pointer, née NSTACK */
#define NSP_LIMIT	%l7

#define P		%i0
#define HP		%i1	/* XXX: heap pointer, née HEAP */
#define HP_LIMIT	%i2
#define TEMP1		%i3

#define ARG0		%o0
#define ARG1		%o1
#define ARG2		%o2
#define ARG3		%o3
#define ARG4		%o4
#define TEMP		%o5
#define RA		%o7	/* XXX: return address, née CP */

/*
 * Process struct offsets and some constants.
 */
#include "hipe_literals.h"
