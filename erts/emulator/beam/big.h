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
#ifndef __BIG_H__
#define __BIG_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __CONFIG_H__
#include "erl_vm.h"
#endif

#ifndef __GLOBAL_H__
#include "global.h"
#endif

/* #define DEBUG_OP */		/* Count arithmetic operations */

#if defined(ARCH_64)
#define D_EXP      32
#else
#define D_EXP      16
#endif
#define D_BASE     (1L<<D_EXP)

#if defined(ARCH_64)
#define D_DECIMAL_EXP	9
#define D_DECIMAL_BASE	1000000000
#else
#define D_DECIMAL_EXP   4           /* 10^4 == 10000 */
#define D_DECIMAL_BASE  10000       /* Max decimal exponent in a digit */
#endif

/* macros for bignum objects */
#define big_v(x)       BIG_V(big_val(x))
#define big_sign(x)    BIG_SIGN(big_val(x))
#define big_arity(x)   BIG_ARITY(big_val(x))
#define big_digit(x,i) BIG_DIGIT(big_val(x),i)
#define big_size(x)    BIG_SIZE(big_val(x))


/* macros for thing pointers */

#define BIG_V(xp)        ((digit_t*)((xp)+1))
#define BIG_SIGN(xp)     (!!bignum_header_is_neg(*xp))
#define BIG_ARITY(xp)    ((Uint)bignum_header_arity(*(xp)))
#define BIG_DIGIT(xp,i)  *(BIG_V(xp)+(i))
#define BIG_DIGITS_PER_WORD (sizeof(Uint)/sizeof(digit_t))

/* FIXME: */
#define BIG_SIZE(xp) \
( 2*BIG_ARITY(xp)  -  ((BIG_DIGIT(xp, 2*BIG_ARITY(xp)-1) == 0) ? 1 : 0))

/* Check for small */
#define IS_USMALL(sgn,x)  ((sgn) ? ((x) <= MAX_SMALL+1) : ((x) <= MAX_SMALL))
#define IS_SSMALL(x)      (((x) >= MIN_SMALL) && ((x) <= MAX_SMALL))

/* The heap size needed for a bignum is
 * Number of digits 'x' in words = (x+1)/2 plus 
 * The thing word
 *
 */
#define BIG_NEED_SIZE(x)  ((((x)+1) >> 1) + 1)

#define BIG_UINT_HEAP_SIZE (1 + 1)	/* always, since sizeof(Uint) <= sizeof(Eterm) */

/* sizeof(digit_t) <= sizeof(D_BASE-1) */

#if defined(ARCH_64)
typedef Uint32 digit_t;
typedef Uint64   reg_t;
#else
typedef Uint32   reg_t;    /* register type 32 bit */
typedef Uint16 digit_t;  /* digit type    16 bit */
#endif
typedef Uint  dsize_t;	 /* Vector size type */


#define ZERO_DIGITS(v, sz) \
  do { \
    dsize_t _t_sz = sz; \
    digit_t* _t_v  = v; \
    while(_t_sz--) *_t_v++ = 0; \
  } while(0)

#define MOVE_DIGITS(dst, src, sz) \
  do { \
    dsize_t _t_sz = sz; \
    digit_t* _t_dst; \
    digit_t* _t_src; \
    if (dst < src) { \
      if (sz > 10 && (src)+(sz) < (dst)) \
        sys_memcpy((void*)(dst), (void*)(src),((size_t)(sz))*sizeof(digit_t)); \
      else { \
         _t_dst = dst; _t_src = src; \
	 while(_t_sz--) *_t_dst++ = *_t_src++; \
      } \
    } \
    else if (dst > src) { \
      if (sz > 10 && (dst)+(sz) < (src)) \
        sys_memcpy((void*)(dst), (void*)(src),((size_t)(sz))*sizeof(digit_t)); \
      else { \
          _t_dst = (dst)+((sz)-1); _t_src = (src)+((sz)-1); \
          while(_t_sz--) *_t_dst-- = *_t_src--; \
      } \
    } \
 } while(0)

/* define digit macros */

#define DLOW(x)        ((digit_t)((x) & (D_BASE-1)))
#define DHIGH(x)       ((digit_t)((x) >> D_EXP))
#define DLOW2HIGH(x)   ((reg_t)((x) << D_EXP))

/*
** Define macros for primitive operations
** DSUM
** DSUMc
** DSUBb
** DMUL
** DMULc
** DDIV
** DREM
*/
#define DSUM(a,b,c1,c0) do { \
     reg_t _t = ((reg_t)(a))+(b); \
     c0 = DLOW(_t); \
     c1 = DHIGH(_t); \
     } while(0)
#define DSUMc(a,b,c,s) do { \
       reg_t _t = ((reg_t)(a))+(b); \
       if (c) _t += (c); \
       s = DLOW(_t); \
       c = DHIGH(_t); \
     }  while(0)
#define DMULc(a,b,c,p) do { \
        reg_t _t = ((reg_t)(a))*(b); \
	if (c) _t += (c); \
	p = DLOW(_t); \
	c = DHIGH(_t); \
     } while(0)
#define DMUL(a,b,c1,c0) do { \
	reg_t _t = ((reg_t)(a))*(b); \
	c0 = DLOW(_t); \
	c1 = DHIGH(_t); \
     } while(0)

#define DSUBb(a,b,r,d) do { \
	 reg_t _t = ((reg_t)(b))+(r); \
	 if ((a) < _t) { \
	    d = (D_BASE-_t)+(a); r = 1; \
	 } \
	 else { \
	    d = (a)-_t; r = 0; \
         } \
     } while(0)
#define DSUB(a,b,r,d) do { \
	reg_t _t = (b); \
	if ((a) < _t) { \
		d = (D_BASE-_t)+(a); r = 1; \
	} \
	else { \
		d = (a)-_t; r = 0; \
	} \
     } while(0)

#define DDIV(a0,a1,b,q) do { \
	reg_t _t = ((reg_t)(a0))*D_BASE+(a1); \
	q = _t / (b); \
     } while(0)

#define DDIV2(a0,a1,b0,b1,q) do { \
	reg_t _t = ((reg_t)(a0))*D_BASE+(a1); \
	q = _t / (((reg_t)(b0))*D_BASE+(b1)); \
     } while(0)

#define DREM(a0,a1,b,r) do { \
	reg_t _t = ((reg_t)(a0))*D_BASE+(a1); \
	r = _t % (b); \
     } while(0)

int big_decimal_estimate(Eterm);
#if 0	/* XXX: unused */
char* big_to_decimal(Eterm, char*, int);
#endif
Eterm erts_big_to_list(Eterm, Eterm**);
char *erts_big_to_string(Eterm x, char *buf, Uint buf_sz);

Eterm big_plus(Eterm, Eterm, Eterm*);
Eterm big_minus(Eterm, Eterm, Eterm*);
Eterm big_times(Eterm, Eterm, Eterm*);
Eterm big_div(Eterm, Eterm, Eterm*);
Eterm big_rem(Eterm, Eterm, Eterm*);
Eterm big_neg(Eterm, Eterm*);

Eterm big_minus_small(Eterm, Uint, Eterm*);
Eterm big_plus_small(Eterm, Uint, Eterm*);
Eterm big_times_small(Eterm, Uint, Eterm*);

Eterm big_band(Eterm, Eterm, Eterm*);
Eterm big_bor(Eterm, Eterm, Eterm*);
Eterm big_bxor(Eterm, Eterm, Eterm*);
Eterm big_bnot(Eterm, Eterm*);

Eterm big_lshift(Eterm, Sint, Eterm*);
int big_comp (Eterm, Eterm);
int big_ucomp (Eterm, Eterm);
int big_to_double(Eterm x, double* resp);
Eterm small_to_big(Sint, Eterm*);
Eterm uint_to_big(Uint, Eterm*);
Eterm erts_make_integer(Uint, Process *);

dsize_t big_bytes(Eterm);
#if 0	/* XXX: unused */
int bytes_eq_big(byte*, dsize_t, int, Eterm);
#endif
Eterm bytes_to_big(byte*, dsize_t, int, Eterm*);
byte* big_to_bytes(Eterm, byte*);

int term_to_Uint(Eterm, Uint*);
int term_to_Sint(Eterm, Sint*);

Uint32 big_to_uint32(Eterm b);
int term_equals_2pow32(Eterm);

#endif

