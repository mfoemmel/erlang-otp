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

#if defined(HYBRID)
/* # define CHECK_FOR_HOLES */
#endif

#if defined(DEBUG) && !defined(CHECK_FOR_HOLES)
# define CHECK_FOR_HOLES
#endif


/* #define HEAP_FRAG_ELIM_TEST 1 */

#if defined(HYBRID)
/* #  define NOMOVE 1 */
/* #  define INCREMENTAL_GC 1 */ /* Requires NOMOVE */
/* #  define INC_TIME_BASED 1 */ /* Time based incremental GC (vs Work based) */
#endif

#if defined(HEAP_FRAG_ELIM_TEST) && (defined(HIPE) || defined(SHARED_HEAP))
#  undef HEAP_FRAG_ELIM_TEST
#endif

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

#define CONTEXT_REDS 1000	/* Swap process out after this number */
#define MAX_ARG 256		/* Max number of arguments allowed */
#define MAX_REG 1024            /* Max number of x(N) registers used */

#define INPUT_REDUCTIONS   (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233	/* default (heap + stack) min size */

#ifdef SHARED_HEAP
#define S_DEFAULT_SIZE  233     /* default stack size */
#define SH_DEFAULT_SIZE 121393  /* default shared heap min size */
#endif

#ifdef HYBRID
#define SH_DEFAULT_SIZE 121393  /* default message area min size */
#endif

#define CP_SIZE			1

#ifdef DEBUG
/*
 * Debug HAlloc that initialize all memory to bad things.
 */
#define HAlloc(p, sz)                                   \
  (ASSERT_EXPR((sz) >= 0),                              \
   ((((HEAP_LIMIT(p) - HEAP_TOP(p)) <= (sz)))           \
    ? erts_heap_alloc((p),(sz))                         \
    : (memset(HEAP_TOP(p),1,(sz)*sizeof(Eterm*)),       \
       HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#else

/*
 * Allocate heap memory, first on the ordinary heap;
 * failing that, in a heap fragment.
 */
#define HAlloc(p, sz)                                   \
  (ASSERT_EXPR((sz) >= 0),                              \
   ((((HEAP_LIMIT(p) - HEAP_TOP(p)) <= (sz)))           \
    ? erts_heap_alloc((p),(sz))                         \
    : (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#endif /* DEBUG */

#define HRelease(p, endp, ptr)					\
  if ((ptr) == (endp)) {					\
     ;								\
  } else if (HEAP_START(p) <= (ptr) && (ptr) < HEAP_TOP(p)) {	\
     HEAP_TOP(p) = (ptr);					\
  } else {							\
     erts_arith_shrink(p, ptr);					\
  }

#define HeapWordsLeft(p)				\
  (HEAP_LIMIT(p) - HEAP_TOP(p))

#ifdef SHARED_HEAP
#  define ARITH_HEAP(p)     erts_global_arith_heap
#  define ARITH_AVAIL(p)    erts_global_arith_avail
#  define ARITH_LOWEST_HTOP(p) erts_global_arith_lowest_htop
extern Eterm* erts_global_arith_heap;
extern Uint erts_global_arith_avail;
extern Eterm* erts_global_arith_lowest_htop;
#  ifdef DEBUG
#    define ARITH_CHECK_ME(p) erts_global_arith_check_me
extern Eterm* erts_global_arith_check_me;
#  endif
#else
#  define ARITH_HEAP(p)     (p)->arith_heap
#  define ARITH_AVAIL(p)    (p)->arith_avail
#  define ARITH_LOWEST_HTOP(p) (p)->arith_lowest_htop
#  ifdef DEBUG
#    define ARITH_CHECK_ME(p) (p)->arith_check_me
#  endif
#endif

/* Allocate memory on secondary arithmetic heap. */

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
# define ERTS_HOLE_MARKER (((0xcafebabeUL << 24) << 8) | 0xaf5e78ccUL)
#endif


#if defined(DEBUG)
#  define ARITH_MARKER (((0xcafebabeUL << 24) << 8) | 0xaf5e78ccUL)
#  define ArithCheck(p) \
      ASSERT(ARITH_CHECK_ME(p)[0] == ARITH_MARKER);
#  define ArithAlloc(p, need)                                   \
   (ASSERT_EXPR((need) >= 0),                                   \
    ((ARITH_AVAIL(p) < (need)) ?                                \
     erts_arith_alloc((p), (p)->htop, (need)) :                 \
     ((ARITH_HEAP(p) += (need)), (ARITH_AVAIL(p) -= (need)),    \
      (ARITH_CHECK_ME(p) = ARITH_HEAP(p)),                      \
      (ARITH_HEAP(p) - (need)))))
#else
#  define ArithCheck(p)
#  define ArithAlloc(p, need)                       \
    ((ARITH_AVAIL(p) < (need)) ?                    \
      erts_arith_alloc((p), (p)->htop, (need)) :    \
      ((ARITH_HEAP(p) += (need)),                   \
       (ARITH_AVAIL(p) -= (need)),                  \
       (ARITH_HEAP(p) - (need))))
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

/* some constants for various  table sizes etc */

#define ATOM_TEXT_SIZE  32768	/* Increment for allocating atom text space */

/*
 * Temporary buffer used in a lot of places.  In some cases, this size
 * will be an absolute resource limit (buffers for pathnames, for instance).
 * In others, memory must be allocated if the buffer is not enough.
 * 
 * Decreasing the size of it below 16384 is not allowed.
 */

#define TMP_BUF_SIZE 65536

#define ITIME 100		/* Number of milliseconds per clock tick    */
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

int big_to_double(Eterm x, double* resp);

#include "erl_term.h"

#endif	/* __ERL_VM_H__ */
