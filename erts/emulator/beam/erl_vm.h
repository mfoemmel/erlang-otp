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

/* 
 * EXTENDED_MEMORY_RANGE:
 * Assume the lower two bits of any pointer converted to
 * Erlang term is word (32bit) aligned and that we therefore
 * only need to shift pointers two steps before tagging and 
 * after "untagging". When "untagging" the two least significant bits
 * are set back to zero by masking with PTR_TAGMASK
 * the #define is also used in utils.c.
 */

#define BEAM 1
#define EMULATOR "BEAM"
#define SEQ_TRACE 1

/*
 * If ALLOW_FUN_TUPLES is definining, the old tuple representation is
 * accepted. (R5 uses fun represented as tuples.)
 */
#define ALLOW_FUN_TUPLES

#define TAGSIZE 4

#define PTR_TAGSHIFT 2
#define PTR_TAGMASK ~3UL

#define CONTEXT_REDS 1000	/* Swap process out after this number       */
#define MAX_ARG 256		/* Max number of arguments allowed */
#define MAX_REG 1024            /* Max number of x(N) registers used */

#define INPUT_REDUCTIONS   (2 * CONTEXT_REDS)

#define H_DEFAULT_SIZE  233	/* default (heap + stack) min size */

#define CP_SIZE			1

/* Allocate heap memory */
#define HAlloc(p, sz) \
    (((p)->stop <= ((p)->htop + (sz))) ? \
        halloc((p),(sz)) : ((p)->htop = (p)->htop + (sz), (p)->htop - (sz)))

/* Allocate memory on secondary arithmetic heap. */
#if defined(DEBUG)
#  define ARITH_MARKER 0xaf5e78cc
#  define ArithCheck(p) \
      ASSERT(p->arith_check_me[0] == ARITH_MARKER);
#  define ArithAlloc(p, need) \
    ((p)->arith_avail < (need)) ? \
     arith_alloc((p), (need)) : \
     (((p)->arith_heap += (need)), ((p)->arith_avail -= (need)), \
     ((p)->arith_check_me = (p)->arith_heap), \
      ((p)->arith_heap - (need)))
#else
#  define ArithCheck(p)
#  define ArithAlloc(p, need) \
    ((p)->arith_avail < (need)) ? \
     arith_alloc((p), (need)) : \
     (((p)->arith_heap += (need)), ((p)->arith_avail -= (need)), \
       ((p)->arith_heap - (need)))
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

/***** tags in the least significant bits *****/

#define TAGMASK  0xf
#define BODYMASK 0x0fffffff
#define CPMASK  0x3
#define ARITY_MASK     0xfffffff0 /* arity for tuples: max 2^28-1 */

#define TARITY_MASK  0x7ffff000  /* arity for thing max 2^19-1 */
#define TARITY_OFFS    12	/* Offset for arity */
#define THING_SUBTAG_MASK  0x00000ff0 /* 8 bits for subtyped things  */
#define THING_SUBTAG_OFFS  4

#define MAX_SMALL     ((1 << 27)-1)
#define MIN_SMALL     -(1 << 27)

#define BIG_ARITY_MAX ((1 << 19)-1)


/* to avoid confusion with CP pointer (two 00 at the end)
 * do not use flags 0, 4, 8, 12 for objects allocated on stack
 *
 * order of SMALL BIG FLOAT ATOM REFER PORT PID TUPLE LIST is important for
 * lexical comparison (note that object0 > object1 when
 * tag_object0 < tag_object1, done to have LIST equal 1
 * the above is not true for NIL and TUPLE (NIL = 3, TUPLE = 2)
 */ 

#define SMALL          15
#define BIG            11
#define FLOAT           9 /* pointer is always aligned */
#define ATOM	        7
#define REFER   	6
#define PORT    	5
#define PID     	3
#define TUPLE   	2 /* must be one bit to make optimization */
#define NIL            ATOM
#define LIST    	1 /* must be one bit to make optimization */
#define ARITYVAL       10 /* only on heap */
#define MOVED          12 /* used for GC, only on heap */
#define CATCH          THING /* only on stack */
#define THING          13  /* only on heap */
#define BINARY         14
#define BLANK          ARITYVAL /* used to mark blank places on local stack */
#define IC             SMALL /* prgr counter in threaded code, only stack */

#define CP0             0 /* only on stack, CP pointer */
#define CP4             4
#define CP8             8
#define CP12            12

/* on some architectures CP contains labels which are not aligned */
#ifdef NOT_ALIGNED
#define is_CP(X) (((X) & TAGMASK) == CP0) /* to find frames on local stack */
#define is_not_CP(X)     (((X) & TAGMASK) != CP0)
#define make_cp(x)       ((uint32)(x) << TAGSIZE | CP0)
#define cp_ptr_val(x)    ((uint32*)ptr_val(x))
#else
#define is_CP(X) !is_not_CP(X)
#define is_not_CP(X) ((X) & CPMASK)
#define make_cp(x)       ((uint32)(x))
#define cp_ptr_val(x)    ((uint32*)(x))
#endif

#define CP0_DEF		0
#define CP4_DEF		4
#define CP8_DEF		8
#define CP12_DEF	12
#define SMALL_DEF       SMALL
#define BIG_DEF         BIG
#define ATOM_DEF       	ATOM    
#define REFER_DEF      	REFER   
#define PORT_DEF       	PORT    
#define PID_DEF        	PID     
#define TUPLE_DEF      	TUPLE   
#define LIST_DEF       	LIST    
#define ARITYVAL_DEF   	ARITYVAL
#define MOVED_DEF      	MOVED   
#define CATCH_DEF      	CATCH   
#define FLOAT_DEF      	FLOAT   
#define THING_DEF      	THING   
#define BINARY_DEF     	BINARY  
#define BLANK_DEF      	BLANK   
#define IC_DEF         	IC      

#define is_blank(X) ((X) == BLANK)
#define make_blank(X) ((X) = BLANK)

/*
 * Note: List and tuple tests are optimized.  They depend on these tags
 * only having one bit set, and that no other tag is zero.  Unfortunately,
 * a continution pointer with a zero tag will look like a list or tuple.
 * (This is usually not a problem, since continuation pointers never should
 * be tested.)
 *
 * In debug-compiled code, we do a safer test, which will reject a continuation
 * pointer.  Nice for those impossible situations when a function is passed
 * a continuation pointer.
 */

#define is_list(X) !is_not_list(X)
#ifdef DEBUG
#  define is_not_list(X) (((X) & TAGMASK) != LIST)
#else
#  define is_not_list(X) ((X) & (TAGMASK - LIST))
#endif

#define is_tuple(X) !is_not_tuple(X)
#ifdef DEBUG
#  define is_not_tuple(X) (((X) & TAGMASK) != TUPLE)
#else
#  define is_not_tuple(X) ((X) & (TAGMASK - TUPLE))
#endif

#define is_big(X) (((X) & TAGMASK) == BIG)
#define is_not_big(X) (((X) & TAGMASK) != BIG)

#if defined(__WIN32__) && !defined(__GNUC__)
#pragma warning(disable : 4035)
__inline uint32 arityval(register uint32 x)
{
	__asm mov eax, x;
	__asm and eax, ARITY_MASK;
	__asm shr eax, TAGSIZE;
}

__inline uint32 thing_arityval(register uint32 x)
{
	__asm mov eax, x;
	__asm and eax, TARITY_MASK;
	__asm shr eax, TARITY_OFFS;
}
#pragma warning(default : 4035)
#else
#define arityval(X)        (((X) & ARITY_MASK) >> TAGSIZE)
#define thing_arityval(X)  (((X) & TARITY_MASK) >> TARITY_OFFS)
#endif
#if defined(__WIN32__) && !defined(__GNUC__) 
#pragma warning(disable : 4035)
__inline uint32 bval(register uint32 x)
{
	__asm mov eax, x;
	__asm shr eax, TAGSIZE;
}
#pragma warning(default : 4035)
#else
#define bval(X)        ((X) >> TAGSIZE)
#endif

#define tag(X)         ((X) & TAGMASK)

#define tag_val(X)     ((X) & TAGMASK)
#define tag_val_def(X) ((X) & TAGMASK)

#if defined(__WIN32__) && !defined(__GNUC__) 
#pragma warning(disable : 4035)
__inline uint32 unsigned_val(register uint32 x)
{
	__asm mov eax, x;
	__asm shr eax, TAGSIZE;
}

__inline sint32 signed_val(register uint32 x)
{
	__asm mov eax, x;
	__asm sar eax, TAGSIZE;
}
#pragma warning(default : 4035)
#else
#define signed_val(x)    (((sint32) (x)) >> TAGSIZE)
#define unsigned_val(x)  ((uint32) ((x) >> TAGSIZE))
#endif
/* bit version tests */
#define is_type(X, mask)     (((1<<(tag_val_def(X))) & (mask)) != 0)


#define not_eq_tags(X,Y) (((X) ^ (Y)) & TAGMASK)

#define NUMBER_CODE(x, y)   (tag_val_def(x) << 4 | tag_val_def(y))

#define SMALL_SMALL	NUMBER_CODE(SMALL, SMALL)
#define SMALL_BIG 	NUMBER_CODE(SMALL, BIG)
#define SMALL_FLOAT 	NUMBER_CODE(SMALL, FLOAT)
#define BIG_SMALL 	NUMBER_CODE(BIG, SMALL)
#define BIG_BIG 	NUMBER_CODE(BIG, BIG)
#define BIG_FLOAT 	NUMBER_CODE(BIG, FLOAT)
#define FLOAT_SMALL 	NUMBER_CODE(FLOAT, SMALL)
#define FLOAT_BIG 	NUMBER_CODE(FLOAT, BIG)
#define FLOAT_FLOAT 	NUMBER_CODE(FLOAT, FLOAT)

#define REFC_BINARY_SUBTAG        (1 << THING_SUBTAG_OFFS)
#define HEAP_BINARY_SUBTAG        (2 << THING_SUBTAG_OFFS)
#define SUB_BINARY_SUBTAG         (3 << THING_SUBTAG_OFFS)
#define FUN_SUBTAG                (4 << THING_SUBTAG_OFFS)
#define FLOAT_THING_SUBTAG        (5 << THING_SUBTAG_OFFS)
#define POSITIVE_BIG_SUBTAG       (6 << THING_SUBTAG_OFFS)
#define NEGATIVE_BIG_SUBTAG       (7 << THING_SUBTAG_OFFS)
#define REF_THING_SUBTAG          (8 << THING_SUBTAG_OFFS)

#define set_subtag(thing, subtag) (((thing) & ~THING_SUBTAG_MASK) | (subtag))

#  ifdef DEBUG 
#    ifdef __GNUC__
#      define ATTRIBUTE_UNUSED __attribute__ ((unused))
#    else
#      define ATTRIBUTE_UNUSED /* N/A */
#    endif /* __GNUC__ */

     static uint32 do_check_ptr_low(uint32, char *, char *, int) 
       ATTRIBUTE_UNUSED;
     static uint32 do_check_ptr_low(uint32 x, char *what, 
				    char *file, int line) {
	 void erl_exit(int, char *, ...);
	 if (x & ~(PTR_TAGMASK)) {
	     erl_exit(42, "Panic, pointer is not clean (%s) in %s:%d!\n",
		      what,file,line);
	 }
	 return x;
     }
#    define check_ptr_low(X,Y) do_check_ptr_low((X),#Y,__FILE__,__LINE__)
#  else
#    define check_ptr_low(X,Y) (X)
#  endif /* DEBUG */

#define make_refer(x)    (check_ptr_low((uint32)(x),make_refer) \
			  << PTR_TAGSHIFT | REFER)
#define make_big(x)      (check_ptr_low((uint32)(x),make_big) \
			  << PTR_TAGSHIFT | BIG)
#define make_list(x)     (check_ptr_low((uint32)(x),make_list) \
			  << PTR_TAGSHIFT | LIST)
#define make_tuple(x)    (check_ptr_low((uint32)(x),make_tuple) \
			  << PTR_TAGSHIFT | TUPLE)
#define make_moved(x)    (((uint32)(x) & ~TAGMASK) | MOVED)
#define make_catch(x)    (check_ptr_low((uint32)(x), make_catch) \
			  << PTR_TAGSHIFT | CATCH)
#define make_float(x)    (check_ptr_low((uint32)(x),make_float) \
			  << PTR_TAGSHIFT | FLOAT)
#define make_I(x)        (check_ptr_low((uint32)(x),make_I) \
			  << PTR_TAGSHIFT | IC)
#define make_binary(x)   (check_ptr_low((uint32)(x),make_binary) \
			  << PTR_TAGSHIFT | BINARY)

#define make_small(x)    ((x) << TAGSIZE | SMALL)
#define make_atom(x)     ((x) << TAGSIZE | ATOM)
#define make_arityval(x) ((x) << TAGSIZE | ARITYVAL)

#define make_thing(ari, subtype) ((subtype) | ((ari) << TARITY_OFFS) | THING)
#define thing_subtag(x)       (((x) & THING_SUBTAG_MASK))

#ifdef EXTRA_POINTER_BITS
#    define ptr_val(X) ((uint32 *) (((((long)(X)) >> PTR_TAGSHIFT) & \
				    PTR_TAGMASK) | EXTRA_POINTER_BITS))
#    define ptr8_val(X) ((byte *) (((((long)(X)) >> PTR_TAGSHIFT) & \
				    PTR_TAGMASK) | EXTRA_POINTER_BITS))
#elif defined(__WIN32__) && !defined(__GNUC__)
#pragma warning(disable : 4035)
     __inline uint32 *ptr_val(register uint32 x)
     { 
	 __asm mov eax, x;
	 __asm shr eax, PTR_TAGSHIFT;
	 __asm and eax, PTR_TAGMASK;
     }
#  define ptr8_val(X) ((byte *) ptr_val(X))
#pragma warning(default : 4035)
#else
#    define ptr_val(X) ((uint32 *) (((X) >> PTR_TAGSHIFT) & PTR_TAGMASK))
#    define ptr8_val(X) ((byte *) (((X) >> PTR_TAGSHIFT) & PTR_TAGMASK))
#endif

#define TAG_MOVE       4  /* used for for accessing fields in process id's
			     and port and ref */

#define CP0_BIT      (1 << CP0)
#define CP4_BIT      (1 << CP4)
#define CP8_BIT      (1 << CP8)
#define CP12_BIT     (1 << CP12)
#define BLANK_BIT    (1 << BLANK)

#define IS_CONST_GC_S(X)    is_type(X, SMALL_BIT | ATOM_BIT | \
         PORT_BIT | PID_BIT | NIL_BIT | BLANK_BIT | CP0_BIT | CP4_BIT | \
         CP8_BIT | CP12_BIT | CATCH_BIT)

#define IS_CONST_GC_H(X)    is_type(X, SMALL_BIT | ATOM_BIT | \
         PORT_BIT | PID_BIT | NIL_BIT | ARITYVAL_BIT)

#define IS_CONST_GC_CH_S(X)    is_type(X, SMALL_BIT | ATOM_BIT | \
         PORT_BIT | PID_BIT | NIL_BIT | BLANK_BIT | CP0_BIT | CP4_BIT | \
         CP8_BIT | CP12_BIT | CATCH_BIT | BINARY_BIT)

#define IS_CONST_GC_CH_H(X)    is_type(X, SMALL_BIT | ATOM_BIT | \
         PORT_BIT | PID_BIT | NIL_BIT | ARITYVAL_BIT | BINARY_BIT)

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

#define BODY 28
#define C_MASK  0xf0000000

#define SMALL_BIT       (1 << SMALL_DEF)
#define BIG_BIT         (1 << BIG_DEF)
#define FLOAT_BIT       (1 << FLOAT_DEF)
#define ATOM_BIT        (1 << ATOM_DEF)
#define REFER_BIT       (1 << REFER_DEF)
#define PORT_BIT        (1 << PORT_DEF)
#define PID_BIT         (1 << PID_DEF)
#define TUPLE_BIT       (1 << TUPLE_DEF)
#define NIL_BIT         (1 << NIL_DEF)
#define LIST_BIT        (1 << LIST_DEF)
#define ARITYVAL_BIT    (1 << ARITYVAL_DEF)
#define MOVED_BIT       (1 << MOVED_DEF)
#define CATCH_BIT       (1 << CATCH_DEF)
#define THING_BIT       (1 << THING_DEF)
#define BINARY_BIT      (1 << BINARY_DEF)

#define ORIG_CREATION 0


#define is_nil(X) ((X) == NIL)
#define is_not_nil(X) ((X) != NIL)

#define eq_const(X,Y)  ((X) == (ATOM | Y))
#define not_eq_const(X,Y) ((X) != (ATOM | Y))

#define is_atom(X) ((((X) & TAGMASK) == ATOM) && (X) != NIL)
#define is_not_atom(X) (!is_atom(X))

#define eq_small(X,Y) ((X) == (SMALL | Y))
#define not_eq_small(X,Y) ((X) != (SMALL | Y))

#define is_small(X) (((X) & TAGMASK) == SMALL)
#define is_not_small(X) (((X) & TAGMASK) != SMALL)

#define is_arity_value(X) (((X) & TAGMASK) == ARITYVAL)
#define is_not_arity_value(X) (((X) & TAGMASK) != ARITYVAL)

#define is_refer(X) (((X) & TAGMASK) == REFER)
#define is_not_refer(X) (((X) & TAGMASK) != REFER)

#define is_pid(X) (((X) & TAGMASK) == PID)
#define is_not_pid(X) (((X) & TAGMASK) != PID)

#define is_frame(X) (((X) & TAGMASK) == FRAME)
#define is_not_frame(X) (((X) & TAGMASK) != FRAME)

#define is_port(X) (((X) & TAGMASK) == PORT)
#define is_not_port(X) (((X) & TAGMASK) != PORT)

#define is_binary(X) (((X) & TAGMASK) == BINARY)
#define is_not_binary(X) (((X) & TAGMASK) != BINARY)

#define is_moved(X) (((X) & TAGMASK) == MOVED)
#define is_not_moved(X) (((X) & TAGMASK) != MOVED)

#define is_catch(X) (((X) & TAGMASK) == CATCH)
#define is_not_catch(X) (((X) & TAGMASK) != CATCH)

#define is_float(X) (((X) & TAGMASK) == FLOAT)
#define is_not_float(X) (((X) & TAGMASK) != FLOAT)

#define is_thing(X) (((X) & TAGMASK) == THING)
#define is_not_thing(X) (((X) & TAGMASK) != THING)

/* number tests */

#define is_integer(x)      ((is_small(x)) || (is_big(x)))
#define is_not_integer(x)  ((is_not_small(x)) && (is_not_big(x)))

#define is_number(x)       (is_integer(x) || is_float(x))
#define is_not_number(x)   (is_not_integer(x) && is_not_float(x))

#define is_byte(x)         (((x) & 0xfffff00f) == SMALL)

#define is_table_id(x) (is_small(x) || is_atom(x))

#define IS_CONST(X)    is_type(X, SMALL_BIT | ATOM_BIT | PORT_BIT | PID_BIT)

#define IS_ONE_CELL(x) is_type(x, SMALL_BIT | ATOM_BIT | PORT_BIT | PID_BIT)

/* macros for extracting bytes from uint16's */

#define hi_byte(a) ((a) >> 8) 
#define lo_byte(a) ((a) & 255) 

/* macros for combining bytes */

#define make_16(x, y) (((x) << 8) | (y))
#define make_24(x,y,z) (((x) << 16) | ((y) << 8) | (z))
#define make_32(x3,x2,x1,x0) (((x3)<<24) | ((x2)<<16) | ((x1)<<8) | (x0))

#define make_signed_24(x,y,z) ((sint32) (((x) << 24) | ((y) << 16) | ((z) << 8)) >> 8)
#define make_signed_32(x3,x2,x1,x0) ((sint32) (((x3) << 24) | ((x2) << 16) | ((x1) << 8) | (x0)))

#define not_eq_tags(X,Y) (((X) ^ (Y)) & TAGMASK)

#define offset_ptr(x, offs)   (x + ((offs << PTR_TAGSHIFT) * sizeof(uint32))) /* NOT PORTABLE! */
#define SMALL_MINUS_TWO  make_small(-(sint32)2)
#define SMALL_MINUS_ONE  make_small(-(sint32)1)
#define SMALL_ZERO       make_small(0)
#define SMALL_ONE        make_small(1)
#define SMALL_TWO        make_small(2)

/* Float definition for byte and word access */
typedef double ieee754_8;

typedef union float_def
{
    ieee754_8 fd;
    byte   fb[sizeof(ieee754_8)];
    uint16 fs[sizeof(ieee754_8) / sizeof(uint16)];
    uint32 fw[sizeof(ieee754_8) / sizeof(uint32)];
} FloatDef;


#define GET_DOUBLE(x, f) (f).fw[0] = *(ptr_val(x)+1), \
                         (f).fw[1] = *(ptr_val(x)+2)

#define PUT_DOUBLE(f, x)  *(x) = make_thing(2, FLOAT_THING_SUBTAG), \
                          *((x)+1) = (f).fw[0], \
			  *((x)+2) = (f).fw[1]


#define CONS(hp, car, cdr) \
        (*(hp) = (car), \
         *((hp)+1) = (cdr), \
          make_list(hp))

#define CAR(x)  *(x)
#define CDR(x)  *((x)+1)

#define TUPLE0(t) \
        ((t)[0] = make_arityval(0), \
        make_tuple(t))
#define TUPLE1(t,e1) \
        ((t)[0] = make_arityval(1), \
        (t)[1] = (e1), \
        make_tuple(t))
#define TUPLE2(t,e1,e2) \
        ((t)[0] = make_arityval(2), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        make_tuple(t))
#define TUPLE3(t,e1,e2,e3) \
        ((t)[0] = make_arityval(3), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        make_tuple(t))
#define TUPLE4(t,e1,e2,e3,e4) \
        ((t)[0] = make_arityval(4), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        make_tuple(t))
#define TUPLE5(t,e1,e2,e3,e4,e5) \
        ((t)[0] = make_arityval(5), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
        make_tuple(t))
#define TUPLE6(t,e1,e2,e3,e4,e5,e6) \
        ((t)[0] = make_arityval(6), \
        (t)[1] = (e1), \
        (t)[2] = (e2), \
        (t)[3] = (e3), \
        (t)[4] = (e4), \
        (t)[5] = (e5), \
	(t)[6] = (e6), \
        make_tuple(t))

#ifdef DEBUG
#define VERBOSE(x) do { if (verbose) x } while(0)
#else
#define VERBOSE(x)
#endif



#define P_SERIAL 3
#define P_NODE   8
#define P_CREAT  2
#define P_NUMBER 15

/* MAX value for the creation field in pid, port and reference */
#define MAX_CREATION (1 << P_CREAT)

/* Minimum NUMBER of processes for a small system to start */
#define MIN_PROCESS  16

/* Maximum NUMBER of process identifiers */
#define MAX_PROCESS (1 << P_NUMBER)

/* Maximum NUMBER of serial numbers */
#define MAX_SERIAL   (1 << P_SERIAL)

/* pid layout
**
**    Serial  Number     Node    Creat  Tag 
**   +---------------------------------------+
**   |  3  |  15       |   8    |   2 |  4   |
**   +---------------------------------------+
*/

#define make_pid3(Ser,Node,Number,Creation) \
  ((uint32) (PID | ((Ser) << 29) | ((Number) << 14) | \
	     ((Node) << 6) | ((Creation) << 4)))

#define make_pid(Ser,Node,Number) \
  make_pid3(Ser,Node,Number,ORIG_CREATION)

#define GETBITS(X,Pos,Size) (((X) >> (Pos)) & ~(~0 << (Size)))

/* This macro get Size bits starting at low order position Pos
   and adjusts the bits to the right 
   bits are numbered from 1 - 32 */


#define get_serial(Pid)   GETBITS(Pid,29,P_SERIAL)
#define get_node(Pid)     GETBITS(Pid,6,P_NODE)

/* note that the get_creation works on
   ports as well as on pids */
#define get_creation(Pid) GETBITS(Pid,4,P_CREAT)

#define get_number(Pid)   GETBITS(Pid,14,P_NUMBER)

/*
**  Port and ref layout
**
**  Node   Number           Creat   Tag
** +------------------------------------+
** |   8   |  18              | 2 |  4  |
** +------------------------------------+
**
*/
#define R_NUMBER	18

/* Maximum number of references in the system */
#define MAX_REFERENCE  (1 << R_NUMBER)

/* Highest port-ID part in a term of type Port 
   Not necessarily the same as the variable erl_max_ports
   which defines the maximum number of simultaneous Ports
   in the Erlang node. MAX_PORT is a hard upper limit.
*/
#define MAX_PORT       (1 << R_NUMBER)


#if 0
#define get_node_reference0(X)   GETBITS(X,24,P_NODE)
#define get_number_reference0(X) GETBITS(X,6,R_NUMBER)
#endif

#define get_node_reference(X)   GETBITS(ptr_val(X)[1],24,P_NODE)
#define get_number_reference(X) ptr_val(X)[2]

#define get_creation_reference(X) GETBITS(ptr_val(X)[1],4,P_CREAT)

#define get_node_port(X)   GETBITS(X,24,P_NODE)
#define get_number_port(X) GETBITS(X,6,R_NUMBER)

#define make_port3(Node,Number,Creation) \
 ((uint32) (PORT | ((Node) << 24) | ((Number) << 6) | \
           ((Creation) << 4)))

#define make_port2(Node,Number) make_port3(Node,Number,ORIG_CREATION)

#define make_port(Number) make_port2(node, Number)

#define make_refer3(Node,Number,Creation) \
 ((uint32) (REFER | ((Node) << 24) | ((Number)  << 6) | \
           ((Creation) << 4)))

#define make_refer2(Node,Number) make_refer3(Node,Number,ORIG_CREATION)



#define ENULL		  0

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

/*
 * Overloaded tags.
 *
 * SMALL_DEF = 15
 * ATOM_DEF/NIL=7
 *
 * Note that the two least significant bits in SMALL/ATOM/NIL always are 3;
 * thus, we can distinguish register from literals by looking at only these
 * two bits.
 */

#define X_REG_DEF  0
#define Y_REG_DEF  1
#define R_REG_DEF  2

#define beam_reg_tag(X) ((X) & 3)

#define make_rreg()     R_REG_DEF
#define make_xreg(x)    (((x) << 2) | X_REG_DEF)
#define make_yreg(x)    (((x) << 2) | Y_REG_DEF)

#define x_reg_number(E) ((E)-X_REG_DEF)
#define y_reg_number(E) ((E)-Y_REG_DEF)

#endif
