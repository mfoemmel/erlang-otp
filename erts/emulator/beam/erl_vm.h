/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */
#ifndef __ERL_VM_H__
#define __ERL_VM_H__

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

#define CONTEXT_REDS 1000	/* Swap process out after this number       */
#define MAX_ARG 256		/* Max number of arguments allowed */
#define MAX_REG 1024            /* Max number of x(N) registers used */

#define INPUT_REDUCTIONS   (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233	/* default (heap + stack) min size */

#define CP_SIZE			1

/* Allocate heap memory */
#define HAlloc(p, sz) \
    (((p)->stop <= ((p)->htop + (sz))) ? \
        arith_alloc((p),(sz)) : ((p)->htop = (p)->htop + (sz), (p)->htop - (sz)))

#define HRelease(p, ptr)				\
  if ((p)->heap <= (ptr) && (ptr) < (p)->htop) {	\
      (p)->htop = (ptr);				\
  } else {}

/* Allocate memory on secondary arithmetic heap. */
#if defined(DEBUG)
#  define ARITH_MARKER 0xaf5e78cc
#  define ArithCheck(p) \
      ASSERT(p->arith_check_me[0] == ARITH_MARKER);
#  define ArithAlloc(p, need) \
    (((p)->arith_avail < (need)) ? \
     arith_alloc((p), (need)) : \
     (((p)->arith_heap += (need)), ((p)->arith_avail -= (need)), \
      ((p)->arith_check_me = (p)->arith_heap), \
      ((p)->arith_heap - (need))))
#else
#  define ArithCheck(p)
#  define ArithAlloc(p, need) \
    (((p)->arith_avail < (need)) ? \
      arith_alloc((p), (need)) : \
      (((p)->arith_heap += (need)), ((p)->arith_avail -= (need)), \
       ((p)->arith_heap - (need))))
#endif

/*
 * Description for each instruction (defined here because the name and
 * count fields are interesting outside the emulator proper).
 */

typedef struct op_entry {
   char* name;			/* Name of instruction. */
   unsigned mask[2];		/* Signature mask. */
   int sz;			/* Number of loaded words. */
   char* pack;			/* Instructions for packing engine. */
   char* sign;			/* Signature string. */
   unsigned count;		/* Number of times executed. */
} OpEntry;

extern OpEntry opc[];		/* Description of all instructions. */
extern int num_instructions;	/* Number of instruction in opc[]. */

extern uint32* ready;

/* some constants for various  table sizes etc */
/* "constants" which are declared as 'extern int' have been moved to
   mkconfig.c, to allow users to set them in config.c */

#define ATOM_TEXT_SIZE  32768	/* Increment for allocating atom text space */

extern int erl_max_ports;		/* Maximum number of ports. */

/*
 * Temporary buffer used in a lot of places.  In some cases, this size
 * will be an absolute resource limit (buffers for pathnames, for instance).
 * In others, memory must be allocated if the buffer is not enough.
 * 
 * Decreasing the size of it below 16384 is not allowed.
 */

#define TMP_BUF_SIZE 65536

#define ITIME 100		/* Number of milliseconds per clock tick    */
#define BG_PROPORTION 8		/* Do bg processes after this # fg          */
#define MAX_PORT_LINK 8		/* Maximum number of links to a port        */

extern int H_MIN_SIZE;		/* minimum (heap + stack) */

#define ORIG_CREATION 0

/* macros for extracting bytes from uint16's */

#define hi_byte(a) ((a) >> 8) 
#define lo_byte(a) ((a) & 255) 

/* macros for combining bytes */

#define make_16(x, y) (((x) << 8) | (y))
#define make_24(x,y,z) (((x) << 16) | ((y) << 8) | (z))
#define make_32(x3,x2,x1,x0) (((x3)<<24) | ((x2)<<16) | ((x1)<<8) | (x0))

#define make_signed_24(x,y,z) ((sint32) (((x) << 24) | ((y) << 16) | ((z) << 8)) >> 8)
#define make_signed_32(x3,x2,x1,x0) ((sint32) (((x3) << 24) | ((x2) << 16) | ((x1) << 8) | (x0)))

#ifdef DEBUG
#define VERBOSE(x) do { if (verbose) x } while(0)
#else
#define VERBOSE(x)
#endif

#define GET_MATH_ARG2(Type,Arg1,Arg2,F1,F2) \
    if (!FP_PRE_CHECK_OK()) \
	BIF_ERROR3(BADARITH, am_math, Type, Arg1, Arg2); \
    if (is_float(Arg1)) { \
	GET_DOUBLE(Arg1,F1); \
    } else if (is_small(Arg1)) \
	F1.fd = signed_val(Arg1); \
    else if (is_big(Arg1)) \
	F1.fd = big_to_double(Arg1); \
    else \
      BIF_ERROR3(BADARG, am_math, Type, Arg1, Arg2); \
    if (is_float(Arg2)) { \
	GET_DOUBLE(Arg2, F2); \
    } else if (is_small(Arg2)) \
	F2.fd = signed_val(Arg2); \
    else if (is_big(Arg2)) \
	F2.fd = big_to_double(Arg2); \
    else \
      BIF_ERROR3(BADARG, am_math, Type, Arg1, Arg2)


#define GET_MATH_ARG1(Type,Arg1,F1) \
    if (!FP_PRE_CHECK_OK()) \
	BIF_ERROR2(BADARITH, am_math, Type, Arg1); \
    if (is_float(Arg1)) { \
	GET_DOUBLE(Arg1, F1); \
    } else if (is_small(Arg1)) \
	F1.fd = signed_val(Arg1); \
    else if (is_big(Arg1)) \
	F1.fd = big_to_double(Arg1); \
    else \
      BIF_ERROR2(BADARG, am_math, Type, Arg1)

#define PUT_MATH_RES2(Res, Type, Arg) \
  { uint32 result, *hp; \
    if (FP_RESULT_OK(Res.fd)) { \
        hp = HAlloc(BIF_P, 3); \
        result = make_float(hp); \
        PUT_DOUBLE(Res, hp); \
	BIF_RET(result); \
    } else { \
	BIF_ERROR2(BADARITH, am_math, Type, Arg); \
    } \
  }

#define PUT_MATH_RES3(Res, Type, Arg0, Arg1) \
  { uint32 result, *hp; \
    if (FP_RESULT_OK(Res.fd)) { \
        hp = HAlloc(BIF_P, 3); \
        result = make_float(hp); \
        PUT_DOUBLE(Res, hp); \
	BIF_RET(result); \
    } else { \
	BIF_ERROR3(BADARITH, am_math, Type, Arg0, Arg1); \
    } \
  }

double big_to_double(uint32);
void load_module(char*, uint32(*)(), uint32(*)());

#include "erl_term.h"

#endif	/* __ERL_VM_H__ */
