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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "erl_binary.h"

#undef M_TRIM_THRESHOLD
#undef M_TOP_PAD
#undef M_MMAP_THRESHOLD
#undef M_MMAP_MAX

#if !defined(ELIB_ALLOC_IS_CLIB) && defined(__GLIBC__) && defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#ifndef HAVE_MALLOPT
#define HAVE_MALLOPT 0
#endif

/* Imported from drv/gzio.c. Why not in any header file? */
EXTERN_FUNCTION(DriverBinary*, gzinflate_buffer, (char*, int));
#ifdef INSTRUMENT
EXTERN_FUNCTION(erl_mutex_t, erts_mutex_sys, (int mno));
#endif

/* Forward */
static int is_printable_string(uint32);

Eterm*
arith_alloc(Process* p, Uint need)
{
    ErlHeapFragment* bp;
    Uint n;

    if (need <= p->arith_avail) {
	Eterm* hp = p->arith_heap;

	p->arith_heap += need;
	p->arith_avail -= need;
	return hp;
    }

    if (need > 64 && p->arith_avail > 7) {
	/*
	 * Allocate in a new block; don't update the arith heap.
	 */
	n = need;
	bp = (ErlHeapFragment*) safe_alloc(sizeof(ErlHeapFragment) +
					   ((n-1)*sizeof(Eterm)));
    } else {
#ifdef DEBUG
	int i;
#endif
	/*
	 * Allocate a new arith heap.
	 */
	n = p->min_heap_size/2 + need;
#ifdef DEBUG
	n++;
#endif
	bp = (ErlHeapFragment*) safe_alloc(sizeof(ErlHeapFragment) +
					   ((n-1)*sizeof(Eterm)));
#ifdef DEBUG
	n--;
#endif
	p->arith_avail = n - need;
	p->arith_heap = bp->mem + need;
#ifdef DEBUG
	for (i = 0; i <= n; i++) {
	    bp->mem[i] = ARITH_MARKER;
	}
	p->arith_check_me = p->arith_heap;
#endif
    }

    bp->next = p->mbuf;
    p->mbuf = bp;
    bp->size = n;
    p->mbuf_sz += n;
    bp->off_heap.mso = NULL;
    bp->off_heap.funs = NULL;
    bp->off_heap.overhead = 0;

    /*
     * Test if time to do GC; if so bump the reduction count to force
     * a context switch.
     */

    p->off_heap.overhead += (sizeof(ErlHeapFragment)/sizeof(Eterm) - 1); 
    if (((p->mbuf_sz + p->off_heap.overhead)*MBUF_GC_FACTOR) >= p->heap_sz) {
	BUMP_ALL_REDS(p);
    }
    return bp->mem;
}

#ifdef INSTRUMENT
/* Does what arith_alloc does, but only ensures that the space is
   allocated; doesn't actually start using it. */
void arith_ensure_alloc(Process* p, uint32 need)
{
    ErlHeapFragment* bp;
    uint32 n;
    uint32* hp;
#ifdef DEBUG
    uint32 i;
#endif

    if (p->arith_avail >= need)
	return;

    n = (need < 128) ? 128 : need;
    bp = new_message_buffer(n+1);
    bp->next = p->mbuf;
    p->mbuf = bp;
    p->mbuf_sz += n+1;
    p->arith_avail = n;
    hp = bp->mem;
#ifdef DEBUG
    for (i = 0; i <= n; i++) {
	hp[i] = ARITH_MARKER;
    }
#endif
    p->arith_heap = hp;
#ifdef DEBUG
    p->arith_check_me = p->arith_heap;
#endif
}
#endif

/*
 * Helper function for the ESTACK macros defined in global.h.
 */

Eterm*
erl_grow_stack(Eterm* ptr, size_t new_size)
{
    if (new_size > 2 * DEF_ESTACK_SIZE) {
	return safe_realloc((void *) ptr, new_size);
    } else {
	Eterm* new_ptr = safe_alloc_from(4, new_size);
	sys_memcpy(new_ptr, ptr, new_size/2);
	return new_ptr;
    }
}

/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/*
 * Generate the integer part from a double.
 */
Eterm
double_to_integer(Process* p, double x)
{
    int is_negative;
    int ds;
    digit_t* xp;
    int i;
    Eterm res;
    size_t sz;
    Eterm* hp;

    if ((x <= (float) MAX_SMALL) && (x >= (float) MIN_SMALL)) {
	sint32 xi = x;
	return make_small(xi);
    }

    if (x >= 0) {
	is_negative = 0;
    } else {
	is_negative = 1;
	x = -x;
    }

    /* Unscale & (calculate exponent) */
    ds = 0;
    while(x >= 1.0) {
	x /= D_BASE;         /* "shift" right */
	ds++;
    }
    sz = ((ds+1) >> 1);          /* number of words */

    /*
     * Beam note: This function is called from guard bifs (round/1 and trunc/1),
     * which are not allowed to build anything at all on the heap.
     * Therefore it is essential to use the ArithAlloc() macro instead of HAlloc()
     * (on Jam, ArithAlloc() is just an alias for HAlloc()).
     */
    hp = ArithAlloc(p, sz+1);
    res = make_big(hp);
    xp = (digit_t*) (hp + 1);

    for (i = ds-1; i >= 0; i--) {
	digit_t d;

	x *= D_BASE;      /* "shift" left */
	d = x;            /* trunc */
	xp[i] = d;        /* store digit */
	x -= d;           /* remove integer part */
    }
    if (ds & 1)  /* odd ds need to zero high word */
	xp[ds] = 0;

    if (is_negative) {
	*hp = make_neg_bignum_header(sz);
    } else {
	*hp = make_pos_bignum_header(sz);
    }
    hp += (sz + 1);
    return res;
}

/*
** Create a new link with ref
*/
ErlLink* new_ref_link(next, type, item, data, ref)
ErlLink* next; ErlLinkType type; uint32 item; uint32 data; uint32 ref;
{
    ErlLink* lnk = (ErlLink*) fix_alloc(link_desc);

    lnk->next = next;
    lnk->type = type;
    lnk->item = item;
    lnk->data = data;
    if (ref == NIL) {
	lnk->ref.t = NIL;
    } else {
	sys_memcpy(&lnk->ref, ref_ptr(ref), sizeof(Ref));
    }
    return lnk;
}

/*
** Create a new link
*/
ErlLink* new_link(next, type, item, data)
ErlLink* next; ErlLinkType type; uint32 item; uint32 data;
{
   return new_ref_link(next, type, item, data, NIL);
}

/*
** Delete an old link (and relink)
*/
void del_link(lnk)
ErlLink** lnk;
{
    ErlLink* tlink;

    if (lnk != NULL) {
	tlink = *lnk;
	*lnk = tlink->next;
	fix_free(link_desc, (uint32*)tlink);
    }
}

/*
** Find a link, given the value of the ref field
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink** find_link_by_ref(first, ref)
ErlLink** first; Ref *ref;
{
    ErlLink* lnk = *first;
    ErlLink* prev = NULL;

    while (lnk != NULL) {
	if (!is_nil(lnk->ref.t) && eqref(&lnk->ref,ref)) {
	    return (prev == NULL) ? first : &prev->next;
	}
	prev = lnk;
	lnk = lnk->next;
    }
    return NULL;
}

/*
** Find a link.
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink** find_link(first, type, item, data)
ErlLink** first; ErlLinkType type; uint32 item; uint32 data;
{
    ErlLink* lnk = *first;
    ErlLink* prev = NULL;

    while(lnk != NULL) {
	if ((lnk->type == type) && (lnk->item == item)) {
	    if ((data == NIL) || (lnk->data == data))
		return (prev == NULL) ? first : &prev->next;
	}
	prev = lnk;
	lnk = lnk->next;
    }
    return NULL;
}

/*
** Calculate length of a list 
** -1 if not a proper list i.e not terminated with NIL
*/
int list_length(list)
uint32 list;
{
    int i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))
	return -1;
    return i;
}


/* make a hash index from an erlang term */

/*
** There are two hash functions.
** make_broken_hash: the one used for backward compatibility
** is called from the bif erlang:hash/2. Should never be used
** as it a) hashes only a part of binaries, b) hashes bignums really poorly,
** c) hashes bignums differently on different endian processors and d) hashes 
** small integers with different weights on different bytes.
**
** make_hash: A hash function that will give the same values for the same
** terms regardless of the internal representation. Small integers are 
** hashed using the same algorithm as bignums and bignums are hashed 
** independent of the CPU endianess. 
** Make_hash also hashes pids, ports and references like 32 bit numbers 
** (but with different constants). 
** make_hash() is called from the bif erlang:phash/2
**
** The idea behind the hash algorithm is to produce values suitable for 
** linear dynamic hashing. We cannot choose the range at all while hashing 
** (it's not even supplied to the hashing functions). The good old algorithm
** [H = H*C+X mod M, where H is the hash value, C is a "random" constant(or M),
** M is the range, preferably a prime, and X is each byte value] is therefore 
** modified to:
** H = H*C+X mod 2^32, where C is a large prime. This gives acceptable 
** "spreading" of the hashes, so that later modulo calculations also will give
** acceptable "spreading" in the range. 
** We really need to hash on bytes, otherwise the 
** upper bytes of a word will be less significant than the lower ones. That's 
** not acceptable at all. For internal use one could maybe optimize by using
** another hash function, that is less strict but faster. That is, however, not
** implemented.
**
** Short semi-formal description of make_hash:
**
** In make_hash, the number N is treated like this:
**  Abs(N) is hashed bytewise with the least significant byte, B(0), first.
**  The number of bytes (J) to calculate hash on in N is 
**  (the number of _32_ bit words needed to store the unsigned 
**   value of abs(N)) * 4.
**  X = FUNNY_NUMBER2
**  If N < 0, Y = FUNNY_NUMBER4 else Y = FUNNY_NUMBER3.
**  The hash value is Y*h(J) mod 2^32 where h(J) is calculated like
**  h(0) = <initial hash> 
**  h(i) = h(i-i)*X + B(i-1)
** The above should hold regardless of internal representation.
** Pids are hashed like small numbers but with differrent constants, as are
** ports.
** References are hashed like ports but only on the least significant byte.
** Binaries are hashed on all bytes (not on the 15 first as in 
** make_broken_hash()).
** Bytes in lists (possibly text strings) use a simpler multiplication inlined
** in the handling of lists, that is an optimization.
** Everything else is like in the old hash (make_broken_hash()).
**
*/ 

/* some prime numbers just above 2 ^ 28 */

#define FUNNY_NUMBER1  268440163
#define FUNNY_NUMBER2  268439161
#define FUNNY_NUMBER3  268435459
#define FUNNY_NUMBER4  268436141
#define FUNNY_NUMBER5  268438633
#define FUNNY_NUMBER6  268437017
#define FUNNY_NUMBER7  268438039
#define FUNNY_NUMBER8  268437511
#define FUNNY_NUMBER9  268439627
#define FUNNY_NUMBER10 268440479

Uint32
make_hash(Eterm term, Uint32 hash)
{
    Uint32 x;
    Sint32 y;

    /* 
    ** Convenience macro for calculating a bytewise hash on an unsigned 32 bit 
    ** integer.
    ** If the endianess is known, we could be smarter here, 
    ** but that gives no significant speedup (on a sparc at least) 
    */
#define UINT32_HASH_RET(Expr, Prime1, Prime2)   			\
	do {								\
	    x = (Uint32) (Expr);	                                \
	    return							\
		(((((hash)*(Prime1) + (x & 0xFF)) * (Prime1) + 	        \
		((x >> 8) & 0xFF)) * (Prime1) + 			\
		((x >> 16) & 0xFF)) * (Prime1) + 			\
		 (x >> 24)) * (Prime2);				        \
	} while(0)

#define SINT32_HASH_RET(Expr, Prime1, Prime2, Prime3)	\
	do {						\
	    y = (Sint32) Expr;				\
	    if (y < 0) {				\
		UINT32_HASH_RET(-y, Prime1, Prime3);	\
	    } 						\
	    UINT32_HASH_RET(y, Prime1, Prime2);		\
	} while(0)
		
	    
    /* 
    ** Significant additions needed for 64 bit port.
    */	    
    ASSERT(sizeof(Eterm) == 4);

    switch (tag_val_def(term)) {
    case NIL_DEF:
	return hash*FUNNY_NUMBER3 + 1;

    case ATOM_DEF:
	return hash*FUNNY_NUMBER1 + 
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);

    case SMALL_DEF:
	SINT32_HASH_RET(signed_val(term), FUNNY_NUMBER2,
			FUNNY_NUMBER3, FUNNY_NUMBER4);

    case BINARY_DEF:
	{
	    byte* ptr;
	    unsigned sz = binary_size(term);
	    int i = sz;

	    GET_BINARY_BYTES(term, ptr);
	    while (i--) {
		hash = hash*FUNNY_NUMBER1 + *ptr++;
	    }
	    return hash*FUNNY_NUMBER4 + sz;
	}

    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;
	    Uint i;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(funp->modp->module)->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->index;
	    hash = hash*FUNNY_NUMBER2 + unsigned_val(funp->uniq);
	    for (i = 0; i < num_free; i++) {
		hash = make_hash(funp->env[i], hash);
	    }
	    return hash;
	}

    case PID_DEF:
	UINT32_HASH_RET(pid_number(term),FUNNY_NUMBER5,FUNNY_NUMBER6);

    case PORT_DEF:
	UINT32_HASH_RET(port_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);

    case REF_DEF:
	UINT32_HASH_RET(ref_number(term),FUNNY_NUMBER9,FUNNY_NUMBER10);

    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    return hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case LIST_DEF:
	{
	    Eterm* list = list_val(term);
	    while(1) {
		if (is_byte(*list)) {
		    /* Optimization for strings. 
		    ** Note that this hash is different from a 'small' hash,
		    ** as multiplications on a Sparc is so slow.
		    */

		    hash = hash*FUNNY_NUMBER2 + unsigned_val(*list);
		} else {
		    hash = make_hash(*list, hash);
		}
		if (is_not_list(CDR(list)))
		    return make_hash(CDR(list),hash)*FUNNY_NUMBER8;
		list = list_val(CDR(list));
	    }
	}
	break;

    case BIG_DEF:
	/* Note that this is the exact same thing as the hashing of smalls.*/
	{
	    Eterm* ptr  = big_val(term);
	    Uint i = thing_arityval(*ptr);
	    int is_neg = BIG_SIGN(ptr);
	    Uint j;

	    ptr++;
	    while (i--) {
		for (j = 0; j < (sizeof(Eterm) / 2); ++j) {
		    hash =  
			((Uint32) (hash * FUNNY_NUMBER2 + 
				   (((Uint16 *) ptr)[j] & 0xFF))) * 
			FUNNY_NUMBER2 + (((Uint16 *) ptr)[j] >> 8);
		}
		++ptr;
	    }
	    if (is_neg) {
		return hash*FUNNY_NUMBER4;
	    } else {
		return hash*FUNNY_NUMBER3;
	    }
	}
	break;

    case TUPLE_DEF: 
	{
	    Eterm* ptr = tuple_val(term);
	    uint32 arity = arityval(*ptr);
	    int i = arity;

	    ptr++;
	    while(i--)
		hash = make_hash(*ptr++, hash);
	    return hash*FUNNY_NUMBER9 + arity;
	}
	break;

    default:
	erl_exit(1, "Invalid tag in make_hash(0x%X)\n", term);
	return 0;
    }
#undef UINT32_HASH_RET
}

Uint
make_broken_hash(Eterm term, Uint hash)
{
    switch (tag_val_def(term)) {
    case NIL_DEF:
	return hash*FUNNY_NUMBER3 + 1;
    case ATOM_DEF:
	return hash*FUNNY_NUMBER1 + 
	    (atom_tab(atom_val(term))->slot.bucket.hvalue);
    case SMALL_DEF:
	return hash*FUNNY_NUMBER2 + unsigned_val(term);
    case BINARY_DEF:
	{
	    byte* ptr;
	    size_t sz = binary_size(term);
	    size_t i = (sz < 15) ? sz : 15;

	    GET_BINARY_BYTES(term, ptr);
	    while (i-- != 0) {
		hash = hash*FUNNY_NUMBER1 + *ptr++;
	    }
	    return hash*FUNNY_NUMBER4 + sz;
	}
    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(term);
	    Uint num_free = funp->num_free;
	    Uint i;

	    hash = hash * FUNNY_NUMBER10 + num_free;
	    hash = hash*FUNNY_NUMBER1 + 
		(atom_tab(funp->modp->module)->slot.bucket.hvalue);
	    hash = hash*FUNNY_NUMBER2 + funp->index;
	    hash = hash*FUNNY_NUMBER2 + unsigned_val(funp->uniq);
	    for (i = 0; i < num_free; i++) {
		hash = make_broken_hash(funp->env[i], hash);
	    }
	    return hash;
	}

    case PID_DEF:
	return hash*FUNNY_NUMBER5 + pid_number(term);

    case PORT_DEF:
	return hash*FUNNY_NUMBER9 + port_number(term);

    case REF_DEF:
	return hash*FUNNY_NUMBER9 + ref_number(term);

    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    return hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case LIST_DEF:
	{
	    uint32* list = list_val(term);

	    while(1) {
		hash = make_broken_hash(*list, hash);
		if (is_not_list(CDR(list)))
		    return make_broken_hash(CDR(list),hash)*FUNNY_NUMBER8;
		list = list_val(CDR(list));
	    }
	}
	break;

    case BIG_DEF:
      {
	uint32* ptr  = big_val(term);
	uint32 arity = BIG_ARITY(ptr);
	int is_neg = BIG_SIGN(ptr);
	int i = arity;
	
	ptr++;
	while (i--) {
	    hash = hash*FUNNY_NUMBER2 + *ptr++;
	}
	
	if (is_neg)
	    return hash*FUNNY_NUMBER3 + arity;
	else
	    return hash*FUNNY_NUMBER2 + arity;
      }
      break;

    case TUPLE_DEF: 
	{
	    uint32* ptr = tuple_val(term);
	    uint32 arity = arityval(*ptr);
	    int i = arity;

	    ptr++;
	    while(i--)
		hash = make_broken_hash(*ptr++, hash);
	    return hash*FUNNY_NUMBER9 + arity;
	}
	break;

    default:
	erl_exit(1, "Invalid tag in make_broken_hash\n");
	return 0;
    }
}

int send_error_to_logger(Eterm gleader)
{
    Process* p;
    ErlHeapFragment* bp;
    uint32* hp;
    uint32 name;
    uint32 res;
    uint32 gl;
    uint32 list;
    int i;
    
    if (is_nil(gleader))
	gl = am_noproc;
    else
	gl = gleader;
    if ((i = cerr_pos) == 0)
	return 0;
    name = am_error_logger;
    if ((p = whereis_process(name)) == NULL)  {
	erl_printf(CERR,"%s",tmp_buf);
	return(0);
    }
    /* !!!!!!! Uhhh  */
    if (p->status == P_EXITING || p->status == P_RUNNING) {
	erl_printf(CERR,"%s",tmp_buf);
	return(0);
    }
    bp = new_message_buffer(i*2 + 4);
    hp = bp->mem;
    list = buf_to_intlist(&hp, tmp_buf, i, NIL);
    res = TUPLE3(hp, am_emulator, gl, list);
    queue_message(p, bp, res);
    return 1;
}

#ifdef INSTRUMENT

void *safe_alloc_from(int from, unsigned int len)
{
    char *buf;

    if ((buf = sys_alloc_from(from, len)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_realloc_from(int from, void* ptr, unsigned int len)
{
    char *buf;

    if ((buf = sys_realloc_from(from, ptr, len)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_sl_alloc_from(int from, unsigned int len)
{
    char *buf;

    if ((buf = sys_sl_alloc_from(from, len)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_sl_realloc_from(int from, void* ptr, unsigned int save_size, unsigned int size)
{
    char *buf;

    if ((buf = sys_sl_realloc_from(from, ptr, save_size, size)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", size);
    return(buf);
}

void *safe_alloc(unsigned int len)
{
  return safe_alloc_from(-1, len);
}

void *safe_realloc(void *ptr, unsigned int len)
{
  return safe_realloc_from(-1, ptr, len);
}

void *safe_sl_alloc(unsigned int len)
{
  return safe_sl_alloc_from(-1, len);
}

void *safe_sl_realloc(void* ptr, unsigned int save_size, unsigned int size)
{
  return safe_sl_realloc_from(-1, ptr, save_size, size);
}

#else


void *safe_alloc(unsigned int len)
{
    char *buf;

    if ((buf = sys_alloc(len)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_realloc(void *ptr, unsigned int len)
{
    char *buf;

    if ((buf = sys_realloc(ptr, len)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_sl_alloc(unsigned int len)
{
    char *buf;

    if ((buf = sys_sl_alloc(len)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", len);
    return(buf);
}

void *safe_sl_realloc(void* ptr, unsigned int save_size, unsigned int size)
{
    char *buf;

    if ((buf = sys_sl_realloc(ptr, save_size, size)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", size);
    return(buf);
}

#endif

/* eq and cmp are written as separate functions a eq is a little faster */

/*
 * Test for equality of two terms.
 * Returns 0 if not equal, or a non-zero value otherwise.
 */

int
eq(Eterm a, Eterm b)
{
 tailrecur:
    if (a == b) 
	return 1;

    switch (primary_tag(a)) {
    case TAG_PRIMARY_BOXED:
	{	
	    Eterm hdr = *boxed_val(a);
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    Eterm* aa;
		    Eterm* bb;
		    int i;
  
		    aa = tuple_val(a);
		    if (!is_boxed(b) || *boxed_val(b) != *aa)
			return 0;
		    bb = tuple_val(b);
		    i = arityval(*aa); /* get the arity*/
		    while (i--) {
			Eterm atmp = *++aa;
			Eterm btmp = *++bb;
			if (!EQ(atmp, btmp))
			    return 0;
		    }
		    return 1;
		}
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
		{
		    Uint size;
		    byte* a_ptr;
		    byte* b_ptr;

		    if (is_not_binary(b)) {
			return 0;
		    }
		    size = binary_size(a);
		    if (size != binary_size(b)) {
			return 0;
		    }
		    GET_BINARY_BYTES(a, a_ptr);
		    GET_BINARY_BYTES(b, b_ptr);
		    return sys_memcmp(a_ptr, b_ptr, size) == 0;
		}
	    case FUN_SUBTAG:
		{
		    ErlFunThing* f1;
		    ErlFunThing* f2;
		    int num_free;
		    int i;
  
		    if (is_not_fun(b))
			return 0;
		    f1 = (ErlFunThing *) fun_val(a);
		    f2 = (ErlFunThing *) fun_val(b);
		    if (f1->modp != f2->modp || f1->index != f2->index ||
			f1->uniq != f2->uniq || f1->num_free != f2->num_free) {
			return 0;
		    }
		    num_free = f1->num_free;
		    for (i = 0; i < num_free; i++) {
			if (!EQ(f1->env[i], f2->env[i])) {
			    return 0;
			}
		    }
		    return 1;
		}
	    case REF_SUBTAG:
		{
		    int alen;
		    int blen;
		    int len;
		    int i;
  
		    if (is_not_ref(b))
			return 0;
		    alen = ref_arity(a);
		    blen = ref_arity(b);
		    if (ref_ptr(a)->h != ref_ptr(b)->h)
			return 0;
  
		    len = alen;
		    if (len > blen)
			len = blen;
 
		    for (i = len-2; i >= 0; i--) {
			if (ref_ptr(a)->w[i] != ref_ptr(b)->w[i])
			    return 0;
		    }
		    return 1;
		}
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
		{
		    Eterm* aa;
		    Eterm* bb;
		    int i;
  
		    if (is_not_big(b))
			return 0;
		    aa = big_val(a); /* get pointer to thing */
		    bb = big_val(b);
		    if (*aa != *bb)
			return 0;
		    i = BIG_ARITY(aa);
		    while(i--) {
			if (*++aa != *++bb)
			    return 0;
		    }
		    return 1;
		}
	    case FLOAT_SUBTAG:
		{
		    FloatDef af;
		    FloatDef bf;
  
		    if (is_not_float(b))
			return 0;
		    GET_DOUBLE(a, af);
		    GET_DOUBLE(b, bf);
		    return (af.fd == bf.fd) ? 1 : 0;
		}
	    }
	    break;
	}
    case TAG_PRIMARY_LIST:
	{
	    Eterm atmp;
	    Eterm btmp;

	    if (is_not_list(b))
		return 0;
	    atmp = CAR(list_val(a));
	    btmp = CAR(list_val(b));
	    if (!EQ(atmp, btmp))
		return 0;
	    a = CDR(list_val(a));
	    b = CDR(list_val(b));
	    goto tailrecur;
	}
    }
    return 0;
}

/* 
 * Lexically compare two strings of bytes (string s1 length l1 and s2 l2).
 *
 *	s1 < s2	return -1
 *	s1 = s2	return  0
 *	s1 > s2 return +1
 */
static int cmpbytes(s1,l1,s2,l2)
byte *s1,*s2;
int l1,l2;
{
    int i;
    i = 0;
    while((i < l1) && (i < l2)) {
	if (s1[i] < s2[i]) return(-1);
	if (s1[i] > s2[i]) return(1);
	i++;
    }
    if (l1 < l2) return(-1);
    if (l1 > l2) return(1);
    return(0);
}


/*
 * Compare objects.
 * Returns 0 if equal, a negative value if a < b, or a positive number a > b.
 *
 * According to the Erlang Standard, types are orderered as follows:
 *   numbers < (characters) < atoms < refs < funs < ports < pids <
 *   tuples < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

static int cmp_atoms(Eterm a, Eterm b)
{
    Atom *aa = atom_tab(atom_val(a));
    Atom *bb = atom_tab(atom_val(b));
    int diff = aa->ord0 - bb->ord0;
    if (diff)
	return diff;
    return cmpbytes(aa->name+3, aa->len-3,
		    bb->name+3, bb->len-3);
}

int
cmp(Eterm a, Eterm b)
{
    Eterm* aa;
    Eterm* bb;
    int i;
    int j;
    Eterm big_buf[2];
    int a_tag;
    int b_tag;

 tailrecur:
    if (a == b) {		/* Equal values or pointers. */
	return 0;
    }

    /* deal with majority (?) cases by brute-force */
    if (is_atom(a)) {
	if (is_atom(b))
	    return cmp_atoms(a, b);
    } else if (is_both_small(a, b)) {
	return signed_val(a) - signed_val(b);
    }

    /*
     * Take care of cases where the types are the same.
     */

    a_tag = tag_val_def(a);
    switch (a_tag) {
    case TUPLE_DEF:
	if (is_not_tuple(b))
	    break;
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	i = arityval(*aa);	/* get the arity*/
	if (i < arityval(*bb)) return(-1);
	if (i > arityval(*bb)) return(1);
	while (i--) {
	    if ((j = cmp(*++aa, *++bb)) != 0) 
		return j;
	}
	return 0;
    case LIST_DEF:
	if (is_not_list(b))
	    break;
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = cmp(*aa++, *bb++)) != 0) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb)) {
		a = *aa;
		b = *bb;
		goto tailrecur;
	    }
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    case FLOAT_DEF:
	if (is_not_float(b))
	    break;
	{
	    FloatDef af;
	    FloatDef bf; 

	    GET_DOUBLE(a, af);
	    GET_DOUBLE(b, bf);
	    return float_comp(af.fd, bf.fd);
	}
    case REF_DEF:
	if (is_not_ref(b))
	    break;
        {
	    int alen = ref_arity(a);
	    int blen = ref_arity(b);
	    int len;

	    if (ref_ptr(a)->h < ref_ptr(b)->h)
		return -1;
	    else if (ref_ptr(a)->h > ref_ptr(b)->h)
		return 1;

	    len = alen;
	    if (len > blen)
		len = blen;

	    for (i = len-2; i >= 0; i--) {
		if (ref_ptr(a)->w[i] < ref_ptr(b)->w[i])
		    return -1;
		else if (ref_ptr(a)->w[i] > ref_ptr(b)->w[i])
		    return 1;
	    }
	    return 0;
	}
    case BIG_DEF:
	if (is_not_big(b))
	    break;
	return big_comp(a, b);
    case BINARY_DEF:
	if (is_not_binary(b))
	    break;
	{
	    Uint a_size = binary_size(a);
	    Uint b_size = binary_size(b);
	    byte* a_ptr;
	    byte* b_ptr;
	    int diff = a_size - b_size;

	    if (diff != 0) {
		return diff;
	    }
	    GET_BINARY_BYTES(a, a_ptr);
	    GET_BINARY_BYTES(b, b_ptr);
	    return sys_memcmp(a_ptr, b_ptr, a_size);
	}
    case FUN_DEF:
	if (is_not_fun(b))
	    break;
	{
	    ErlFunThing* f1 = (ErlFunThing *) fun_val(a);
	    ErlFunThing* f2 = (ErlFunThing *) fun_val(b);
	    int num_free;
	    int diff;

	    diff = cmpbytes(atom_tab(f1->modp->module)->name,
			    atom_tab(f1->modp->module)->len,
			    atom_tab(f2->modp->module)->name,
			    atom_tab(f2->modp->module)->len);
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->index - f2->index;
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->uniq - f2->uniq;
	    if (diff != 0) {
		return diff;
	    }
	    diff = f1->num_free - f2->num_free;
	    if (diff != 0) {
		return diff;
	    }
	    num_free = f1->num_free;
	    for (i = 0; i < num_free; i++) {
		if ((diff = cmp(f1->env[i], f2->env[i])) != 0) {
		    return diff;
		}
	    }
	    return 0;
	}
    case PID_DEF:
	if (is_not_pid(b))
	    break;
	if ((! (pid_node(a) == pid_node(b))) && /*  different nodes */
	    (pid_number(a) == pid_number(b)) &&
	    (pid_serial(a) == pid_serial(b))) { /* equal numbers */
	    
	    uint32 atoma, atomb;
	    i = pid_node(a); /* index in atom table */
	    j = pid_node(b);
	    atoma = dist_addrs[i].sysname;
	    atomb = dist_addrs[j].sysname;
	    return(cmpbytes(atom_tab(atom_val(atoma))->name,
			    atom_tab(atom_val(atoma))->len,
			    atom_tab(atom_val(atomb))->name,
			    atom_tab(atom_val(atomb))->len));
	}
	return (a < b) ? -1 : 1;
    case PORT_DEF:
	if (is_not_port(b))
	    break;
	return (a < b) ? -1 : 1;
    }

    b_tag = tag_val_def(b);

    /*
     * Take care of the case that the tags are different.
     */

    {
	FloatDef f1, f2;
	Eterm big;

	switch(_NUMBER_CODE(a_tag, b_tag)) {
	case SMALL_BIG:
	    big = small_to_big(signed_val(a), big_buf);
	    return big_comp(big, b);
	case SMALL_FLOAT:
	    f1.fd = signed_val(a);
	    GET_DOUBLE(b, f2);
	    return float_comp(f1.fd, f2.fd);
	case BIG_SMALL:
	    big = small_to_big(signed_val(b), big_buf);
	    return big_comp(a, big);
	case BIG_FLOAT:
	    f1.fd = big_to_double(a);
	    GET_DOUBLE(b, f2);
	    if (!FP_RESULT_OK(f1.fd)) {
		return big_sign(a) ? -1 : 1;
	    }
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_SMALL:
	    GET_DOUBLE(a, f1);
	    f2.fd = signed_val(b);
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_BIG:
	    GET_DOUBLE(a, f1);
	    f2.fd = big_to_double(b);
	    if (!FP_RESULT_OK(f2.fd)) {
	       return big_sign(b) ? 1 : -1;
	    }
	    return float_comp(f1.fd, f2.fd);
	default:
	    return b_tag - a_tag;
	}
    }
}

Process* pid2proc(pid)
uint32 pid;
{
    Process *rp;
    int i;
    int pix;

    if (is_not_pid(pid) || 
	(pid_node(pid) != THIS_NODE) || 
	((pix = pid_number(pid)) >= max_process))
	return NULL;
    i = pid_creation(pid);
    if ((i != this_creation) && (i != 0))
	return NULL;

    rp = process_tab[pix];
    if (INVALID_PID(rp, pid))
	return NULL;
    return rp;
}


static int dcount;

/* 
 * Display a term.
 */

static int 
display1(Eterm obj, CIO fd)
{
    int i, k;
    Eterm* nobj;

    if (dcount-- <= 0) return(1);

    if (is_CP(obj)) {
	erl_printf(fd, "<cp/header:%08X", obj);
	return 0;
    }

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	erl_printf(fd, "[]");
	break;
    case ATOM_DEF:
	print_atom((int)atom_val(obj),fd);
	break;
    case SMALL_DEF:
	erl_printf(fd, "%d", signed_val(obj));
	break;
    case BIG_DEF:
	nobj = big_val(obj);
	i = BIG_SIZE(nobj);
	if (BIG_SIGN(nobj))
	    erl_printf(fd, "-#integer(%d) = {", i);
	else
	    erl_printf(fd, "#integer(%d) = {", i);
	erl_printf(fd, "%d", BIG_DIGIT(nobj, 0));
	for (k = 1; k < i; k++)
	    erl_printf(fd, ",%d", BIG_DIGIT(nobj, k));
	erl_putc('}', fd);
	break;
    case REF_DEF:
	erl_printf(fd, "#Ref<%d", ref_node(obj));
	for (i = ref_arity(obj)-2; i >= 0; i--)
	    erl_printf(fd, ".%lu", ref_ptr(obj)->w[i]);
	erl_printf(fd, ">");
	break;
    case PID_DEF:
	erl_printf(fd, "<%d.%d.%d>",
		   pid_node(obj), pid_number(obj), pid_serial(obj));
	break;
    case PORT_DEF:
	erl_printf(fd, "#Port<%d.%d>", port_node(obj),
		   port_number(obj));
	break;
    case LIST_DEF:
	if (is_printable_string(obj)) {
	   int c;
	   erl_putc('"', fd);
	   nobj = list_val(obj);
	   while (1) {
	      if (dcount-- <= 0) return(1);
	      c = signed_val(*nobj++);
	      if (c == '\n') {
		 erl_putc('\\', fd);
		 erl_putc('n', fd);
	      } else {
		 if (c == '"')
		    erl_putc('\\', fd);
		 erl_putc(c, fd);
	      }
	      if (is_not_list(*nobj)) break;
	      nobj = list_val(*nobj);
	   }
	   erl_putc('"', fd);
	} else {
	   erl_putc('[', fd);
	   nobj = list_val(obj);
	   while (1) {
	      if (display1(*nobj++, fd) != 0) return(1);
	      if (is_not_list(*nobj)) break;
	      erl_putc(',',fd);
	      nobj = list_val(*nobj);
	   }
	   if (is_not_nil(*nobj)) {
	      erl_putc('|', fd);
	      if (display1(*nobj, fd) != 0) return(1);
	   }
	   erl_putc(']', fd);
	}
	break;
    case TUPLE_DEF:
	nobj = tuple_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	erl_putc('{', fd);
	while (i--) {
	    if (display1(*++nobj,fd) != 0) return(1);
	    if (i >= 1) erl_putc(',',fd);
	}
	erl_putc('}',fd);
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
	    erl_printf(fd, "%.20e", ff.fd);
	}
	break;
    case BINARY_DEF:
	{
	    ProcBin* pb = (ProcBin *) binary_val(obj);
	    erl_printf(fd, "<<%d bytes>>", pb->size);
	}
	break;
    case FUN_DEF:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    Atom* ap;

	    erl_printf(fd, "#Fun<");
	    ap = atom_tab(funp->modp->module);
	    for (i = 0; i < ap->len; i++) {
		erl_putc(ap->name[i], fd);
	    }
	    erl_printf(fd, ".%d.%d>", funp->index, unsigned_val(funp->uniq));
	}
	break;
    case VECTOR_DEF:
	erl_printf(fd, "#Vector<%d>", signed_val(vector_val(obj)[1]));
	break;
    default:
	erl_printf(fd, "<unknown:%x>", obj);
    }
    return(0);
}


/*
 * Display a term on file fd.
 * Only used by debugging rountines as Erlang formatting is 
 * done in the io module.
 */

void
display(Eterm obj, CIO fd)
{
    dcount = 100000;
    display1(obj, fd);
}


/* as above, but limit the number of items printed */
void ldisplay(obj, fd, count)
uint32 obj; CIO fd; int count;
{
    dcount = count;
    display1(obj, fd);
    if (dcount <= 0) erl_printf(fd, "... "); /* Show that more items exit */
}


/* print a name doing what quoting is necessary */
static void print_name(s, n, fd)
byte *s; int n; CIO fd;
{
    
    int need_quote;
    int pos;
    byte *cpos;
    int c;

    if (n == 0) {
	erl_printf(fd, "''");
	return;
    }

    need_quote = 0;
    cpos = s;
    pos = n - 1;

    c = *cpos++;
    if (!IS_LOWER(c))
	need_quote++;
    else {
	while (pos--) {
	    c = *cpos++;
	    if (!IS_ALNUM(c) && (c != '_')) {
		need_quote++;
		break;
	    }
	}
    }
    cpos = s;
    pos = n;
    if (need_quote)
	erl_putc('\'',fd);
    while(pos--) {
	c = *cpos++;
	switch(c) {
	case '\'': erl_printf(fd, "\\'"); break;
	case '\\': erl_printf(fd, "\\\\"); break;
	case '\n': erl_printf(fd, "\\n"); break;
	case '\f': erl_printf(fd, "\\f"); break;
	case '\t': erl_printf(fd, "\\t"); break;
	case '\r': erl_printf(fd, "\\r"); break;
	case '\b': erl_printf(fd, "\\b"); break;
	case '\v': erl_printf(fd, "\\v"); break;
	default:
	    if (IS_CNTRL(c))
		erl_printf(fd, "\\%03o", c);
	    else
		erl_putc(c, fd);
	    break;
	}
    }
    if (need_quote) 
	erl_putc('\'',fd);
}

/* print the text of an atom with number i on open file descriptor fd */
void
print_atom(int i, CIO fd)
{
    if ((i < 0) || (i >= atom_table_size) ||  (atom_tab(i) == NULL)) {
	erl_printf(fd, "<bad atom index: %d>", i);
    }
    print_name(atom_tab(i)->name, atom_tab(i)->len, fd);
    dcount -= atom_tab(i)->len;
}

/* 
 *  member(X,Y)
 *  returns 0 if X is a member of list Y
 *  returns 1 if X is not a member of list Y
 *  returns 2 if Y is not a list or is a badly formed list
 */

int member(x,y)
uint32 x,y;
{
    uint32 *z;
    if (is_nil(y)) return(1); /* empty list */
    if (is_not_list(y)) return(2); /* bad argument */
    z = list_val(y);
    for (;;) {
	if (eq(*z, x)) return(0);
	if (is_nil(*(z + 1))) return(1); /* end of list */
	if (is_not_list(*(z + 1))) return(2); /* badly formed list */
	z = list_val(*(z + 1));
    }
}

void bin_write(fp,buf,sz)
CIO fp; byte* buf;
int sz;
{
    int i;

    for (i=0;i<sz;i++) {
	if (IS_DIGIT(buf[i]))
	    erl_printf(fp, "%d,", buf[i]);
	else if (IS_PRINT(buf[i])) {
	    erl_putc(buf[i],fp);
	    erl_putc(',',fp);
	}
	else
	    erl_printf(fp,"%d,", buf[i]);
    }
    erl_putc('\n',fp);
}

/* Fill buf with the contents of bytelist list 
   return number of chars in list or -1 for error */

int intlist_to_buf(list,buf,len)
uint32 list;
byte *buf;
int len;
{
    uint32 *listptr;
    int sz = 0;

    if (is_nil(list)) 
	return 0;
    if (is_not_list(list))
	return -1;
    listptr = list_val(list);

    while (sz < len) {
	if (!is_byte(*listptr)) 
	    return -1;
	buf[sz++] = unsigned_val(*listptr);
	if (is_nil(*(listptr + 1)))
	    return(sz);
	if (is_not_list(*(listptr + 1))) 
	    return -1;
	listptr = list_val(*(listptr + 1));
    }
    return -1;			/* not enough space */
}

/*
** Convert an integer to a byte list buf must have at least 12 bytes avaiable
** return pointer to converted stuff (need not to be at start of buf!)
*/
char* int_to_buf(n, buf)
int n; char* buf;
{
    char* p = buf+11;
    int sign = 0;

    *p-- = '\0'; /* null terminate */
    if (n == 0)
	*p-- = '0';
    else if (n < 0) {
	sign = 1;
	n = -n;
    }

    while (n != 0) {
	*p-- = (n % 10) + '0';
	n /= 10;
    }
    if (sign)
	*p-- = '-';
    return p+1;
}

/* Build a list of integers in some safe memory area
** Memory must be pre allocated prio call 2*len in size
** hp is a pointer to the "heap" pointer on return
** this pointer is updated to point after the list
*/

uint32 buf_to_intlist(hpp, buf, len, tail)
uint32** hpp; byte *buf; int len; uint32 tail;
{
    uint32* hp = *hpp;

    buf += (len-1);
    while(len > 0) {
	tail = CONS(hp, make_small((byte)*buf), tail);
	hp += 2;
	buf--;
	len--;
    }
    *hpp = hp;
    return tail;
}

/*
** write io list in to a buffer.
**
** A iolist is defined as:
**
** iohead ::= Binary
**        |   Byte (i.e integer in range [0..255]
**        |   iolist
**        ;
**
** iotail ::= []
**        |   Binary  (added by tony)
**        |   iolist
**        ;
**
** iolist ::= []
**        |   Binary
**        |   [ iohead | iotail]
**        ;
** 
** Return remaing bytes in buffer on succsess
**        -1 on overflow
**        -2 on type error
*/

int io_list_to_buf(Eterm obj, char* buf, int len)
{
    Eterm* objp;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		if (len == 0)
		    goto L_overflow;
		*buf++ = unsigned_val(obj);
		len--;
	    } else if (is_binary(obj)) {
		byte* bytes;
		size_t size = binary_size(obj);
		if (len < size) {
		    goto L_overflow;
		}
		GET_BINARY_BYTES(obj, bytes);
		sys_memcpy(buf, bytes, size);
		buf += size;
		len -= size;
	    }
	    else if (is_nil(obj)) {
		;
	    }
	    else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    }
	    else
		goto L_type_error;

	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		byte* bytes;
		size_t size = binary_size(obj);
		if (len < size) {
		    goto L_overflow;
		}
		GET_BINARY_BYTES(obj, bytes);
		sys_memcpy(buf, bytes, size);
		buf += size;
		len -= size;
	    }
	    else if (is_nil(obj))
		;
	    else
		goto L_type_error;
	} else if (is_binary(obj)) {
	    byte* bytes;
	    size_t size = binary_size(obj);
	    if (len < size) {
		goto L_overflow;
	    }
	    GET_BINARY_BYTES(obj, bytes);
	    sys_memcpy(buf, bytes, size);
	    buf += size;
	    len -= size;
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -2;

 L_overflow:
    DESTROY_ESTACK(s);
    return -1;
}

int io_list_len(Eterm obj)
{
    Eterm* objp;
    int len = 0;
    DECLARE_ESTACK(s);
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    /* Head */
	    obj = CAR(objp);
	    if (is_byte(obj)) {
		len++;
	    } else if (is_binary(obj)) {
		len += binary_size(obj);
	    } else if (is_nil(obj)) {
		;
	    } else if (is_list(obj)) {
		ESTACK_PUSH(s, CDR(objp));
		goto L_iter_list; /* on head */
	    } else {
		goto L_type_error;
	    }
	    /* Tail */
	    obj = CDR(objp);
	    if (is_list(obj))
		goto L_iter_list; /* on tail */
	    else if (is_binary(obj)) {
		len += binary_size(obj);
	    } else if (is_nil(obj)) {
		;
	    } else {
		goto L_type_error;
	    }
	} else if (is_binary(obj)) { /* Tail was binary */
	    len += binary_size(obj);
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    return len;

 L_type_error:
    DESTROY_ESTACK(s);
    return -1;
}
/*
int
io_list_len(Eterm list)
{
    int len = 0;
    int i;

    while (is_list(list)) {
	Eterm* cons = list_val(list);
	Eterm obj = CAR(cons);

	list = CDR(cons);
	if (is_byte(obj)) {
	    len++;
	} else if (is_list(obj)) {
	    if ((i = io_list_len(obj)) < 0)
		return i;
	    len += i;
	} else if (is_binary(obj)) {
	    len += binary_size(obj);
	} else if (!is_nil(obj)) {
	    return -1;
	}
    }
    if (is_nil(list))
	return len;
    else if (is_binary(list)) {
	return len + binary_size(list);
    } else {
	return -1;
    }
}
*/
/* return 0 if item is not a non-empty flat list of bytes */

int is_string(list)
uint32 list;
{
    int len = 0;

    while(is_list(list)) {
	uint32* consp = list_val(list);
	uint32  hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

/* return 0 if item is not a non-empty flat list of printable characters */

static int is_printable_string(uint32 list)
{
    int len = 0;
    int c;

    while(is_list(list)) {
	uint32* consp = list_val(list);
	uint32  hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	c = signed_val(hd);
	/* IS_PRINT || IS_SPACE would be another way to put it */
	if (IS_CNTRL(c) && !IS_SPACE(c))
	   return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

int
do_load(group_leader, mod, code, size)
    Eterm group_leader;		/* Group leader or NIL if none. */
    uint32 mod;			/* Module name as an atom. */
    byte* code;			/* Points to the code to load */
    int size;			/* Size of code to load. */
{
    DriverBinary* bin;
    int result;

    if ((bin = (DriverBinary *) gzinflate_buffer(code, size)) == NULL) {
	return -1;
    }
    result = bin_load(group_leader, mod, bin->orig_bytes, bin->orig_size);
    driver_free_binary(bin);
    return result;
}

#ifdef INSTRUMENT
typedef union most_strict {
    double x;
    long y;
} Most_strict;

/* Note: sizeof(mem_link) is used as a constant in
   tools/src/instrument.erl; keep it updated if this struct changes. */

typedef struct mem_link
{
   struct mem_link *prev, *next;
   unsigned long size;
   int type;
   Eterm p;			/* which process allocated */
   Most_strict align;
} mem_link;

static mem_link *mem_anchor;
static int need_instr_init;
static erl_mutex_t instr_lck;
static Uint totally_allocated = 0;
static Uint maximum_allocated = 0;

static void instr_init(void);

#endif

static Sint trim_threshold;
static Sint top_pad;
static Sint mmap_threshold;
static Sint mmap_max;

void erts_init_utils(void) 
{
    trim_threshold = -1;
    top_pad = -1;
    mmap_threshold = -1;
    mmap_max = -1;
#ifdef INSTRUMENT
    mem_anchor = NULL;
    need_instr_init = 1;
    totally_allocated = 0;
    maximum_allocated = 0;
#endif
}

int
sys_alloc_opt(int opt, int value)
{
#if HAVE_MALLOPT
  Sint m_opt;
  Sint *curr_val;

  switch(opt) {
  case SYS_ALLOC_OPT_TRIM_THRESHOLD:
#ifdef M_TRIM_THRESHOLD
    m_opt = M_TRIM_THRESHOLD;
    curr_val = &trim_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_TOP_PAD:
#ifdef M_TOP_PAD
    m_opt = M_TOP_PAD;
    curr_val = &top_pad;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_THRESHOLD:
#ifdef M_MMAP_THRESHOLD
    m_opt = M_MMAP_THRESHOLD;
    curr_val = &mmap_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_MAX:
#ifdef M_MMAP_MAX
    m_opt = M_MMAP_MAX;
    curr_val = &mmap_max;
    break;
#else
    return 0;
#endif
  default:
    return 0;
  }

  if(mallopt(m_opt, value)) {
    *curr_val = (Sint) value;
    return 1;
  }

#endif /* #if HAVE_MALLOPT */

  return 0;
}

void
sys_alloc_stat(SysAllocStat *sasp)
{
   sasp->trim_threshold = trim_threshold;
   sasp->top_pad        = top_pad;
   sasp->mmap_threshold = mmap_threshold;
   sasp->mmap_max       = mmap_max;

#ifdef INSTRUMENT
   if (need_instr_init)
       instr_init();

   erts_mutex_lock(instr_lck);

   sasp->total          = totally_allocated;
   sasp->maximum        = maximum_allocated;

   erts_mutex_unlock(instr_lck);
#endif
}

#ifdef INSTRUMENT
extern Eterm current_process;

static void link_in(l, size, from)
mem_link *l;
unsigned size;
int from;
{
   l->next = mem_anchor;
   if (mem_anchor != NULL)
      mem_anchor->prev = l;
   l->prev = NULL;
   l->size = size;
   if (l->type == -1)
      l->type = from;
   mem_anchor = l;

   l->p = current_process;
}

static void link_out(l)
mem_link *l;
{
   mem_link *prev, *next;

   prev = l->prev;
   next = l->next;
   if (prev != NULL)
      prev->next = next;
   else
      mem_anchor = next;

   if (next != NULL)
      next->prev = prev;
}

static void
init_instr_lock(void)
{
  instr_lck = erts_mutex_sys(1);
}

static void
lock_instr_lock(void)
{
  erts_mutex_lock(instr_lck);
}

static void
unlock_instr_lock(void)
{
  erts_mutex_unlock(instr_lck);
}

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void));

#ifndef INIT_MUTEX_IN_CHILD_AT_FORK
#define INIT_MUTEX_IN_CHILD_AT_FORK 0
#endif

static void instr_init(void)
{
    init_instr_lock();
    erts_atfork_sys(lock_instr_lock,
		    unlock_instr_lock,
#if INIT_MUTEX_IN_CHILD_AT_FORK
		    init_instr_lock
#else
		    unlock_instr_lock
#endif
		    );
    totally_allocated = 0;
    maximum_allocated = 0;
    need_instr_init = 0;

}


void *
instr_alloc(int from, void *(*alloc_func)(unsigned int), unsigned int size)
{
   char *p;
   mem_link *l;
   if (need_instr_init)
       instr_init();

   p = (*alloc_func)(size + sizeof(mem_link));
   if (p == NULL) {
     return NULL;
   }

   erts_mutex_lock(instr_lck);
 
   l = (mem_link *) p;
   l->type = -1;
   link_in(l, size, from);

   totally_allocated += size;
   if(totally_allocated >= maximum_allocated)
     maximum_allocated = totally_allocated;

   erts_mutex_unlock(instr_lck);

   return (void *) (p + sizeof(mem_link));
}

void *
instr_realloc(int from,
	      void *(*realloc_func)(void *, unsigned int, unsigned int),
	      void *ptr,
	      unsigned int save_size,
	      unsigned int size)
{
   char *p, *new_p;
   mem_link *l;
   int old_type;
   unsigned old_size;

   if (need_instr_init)
       instr_init();

   erts_mutex_lock(instr_lck);

   p = ((char *) ptr) - sizeof(mem_link);

   l = (mem_link *) p;
   old_size = l->size;
   old_type = l->type;
   link_out(l);
   erts_mutex_unlock(instr_lck);

   new_p = (realloc_func)(p, save_size, size + sizeof(mem_link));
   if (new_p == NULL) {
     erts_mutex_lock(instr_lck);
     link_in(l, old_size, old_type); /* Old memory block is still allocated */
     erts_mutex_unlock(instr_lck);
     return NULL;
   }

   erts_mutex_lock(instr_lck);

   l = (mem_link *) new_p;
   link_in(l, size, from);

   ASSERT(totally_allocated + size >= old_size);
   totally_allocated += size;
   totally_allocated -= old_size;
   if(totally_allocated >= maximum_allocated)
     maximum_allocated = totally_allocated;

   erts_mutex_unlock(instr_lck);

   return (void *) (new_p + sizeof(mem_link));
}

void
instr_free(void (*free_func)(void *), void *ptr)
{
   mem_link *l;
   char *p;

   if (need_instr_init)
       instr_init();

   erts_mutex_lock(instr_lck);

   p = ((char *) ptr) - sizeof(mem_link);

   l = (mem_link *) p;

   ASSERT(totally_allocated >= l->size);
   totally_allocated -= l->size;

   link_out(l);

   erts_mutex_unlock(instr_lck);

   (*free_func)(p);

}

void *
sys_alloc(unsigned int size)
{
  return instr_alloc(-1, sys_alloc2, size);
}

void *
sys_realloc3(void* ptr, unsigned int unused, unsigned int size)
{
  return sys_realloc2(ptr, size);
}

void *
sys_realloc(void* ptr, unsigned int size)
{
  return instr_realloc(-1, sys_realloc3, ptr, 0, size);
}

void
sys_free(void* ptr)
{
  return instr_free(sys_free2, ptr);
}

void *
sys_sl_alloc(unsigned int size)
{
  return instr_alloc(-1, sys_sl_alloc2, size);
}

void *
sys_sl_realloc(void* ptr, unsigned int save_size, unsigned int size)
{
  return instr_realloc(-1, sys_sl_realloc2, ptr, save_size, size);
}

void
sys_sl_free(void* ptr)
{
  return instr_free(sys_sl_free2, ptr);
}

static void dump_memory_to_stream(FILE *f)
{
   mem_link *l;

   l = mem_anchor;

   while (l != NULL)
   {
      if (is_non_value(l->p))
	 fprintf(f, "{%d, %lu, %lu, undefined}.\n",
		 l->type,
		 ((unsigned long) l) + sizeof(mem_link),
		 l->size);
      else
	 fprintf(f, "{%d, %lu, %lu, {%ld,%ld,%ld}}.\n",
		 l->type,
		 ((unsigned long) l) + sizeof(mem_link),
		 l->size,
		 pid_node(l->p),pid_number(l->p),pid_serial(l->p));
      l = l->next;
   }
}

void dump_memory_to_fd(int fd)
{
   char buf[BUFSIZ];
   FILE *f;

   f = fdopen(fd, "w");
   if (f == NULL)
      return;

   /* Avoid allocating memory; we may have run out of it at this point. */
   setbuf(f, buf);

   dump_memory_to_stream(f);
   fflush(f);
}

int dump_memory_data(name)
const char *name;
{
   FILE *f;

   f = fopen(name, "w");
   if (f == NULL)
      return 0;

   dump_memory_to_stream(f);

   fclose(f);
   return 1;
}

uint32 collect_memory(process)
Process *process;
{
   uint32 list, tup;
   uint32 *hp, *end_hp;
   mem_link *l;
   uint32 need, need_big;
   uint32 pid;
   uint32 p;

   list = NIL;

   need = 0;
   need_big = 0;
   l = mem_anchor;
   while (l != NULL)
   {
      if ((unsigned) l > MAX_SMALL)
	  need_big += 1;
      if (l->size > MAX_SMALL)
	  need_big += 1;
      need += 4+2;
      l = l->next;
   }

   /* The "alloc" operation itself is likely to add to the list,
      so add a little. */
   need += 20;

   hp = HAlloc(process, need);
   end_hp = hp + need;
   arith_ensure_alloc(process, 2*need_big); /* 2 = BIG_NEED_SIZE(2) */

   l = mem_anchor;
   while (l != NULL)
   {
      /* If it should turn out we need more than we allocated, jump
	 out, and continue allocating on the heap instead. */
      if (hp >= end_hp - (4+5+2))
	 break;

      if (is_non_value(l->p))
	 pid = am_undefined;
      else
      {
	 pid = TUPLE3(hp,
		      make_small(pid_node(l->p)),
		      make_small(pid_number(l->p)),
		      make_small(pid_serial(l->p)));
	 hp += 4;
      }

      p = ((unsigned) l) + sizeof(mem_link);
      tup = TUPLE4(hp,
		   make_small(l->type),
		   make_small_or_big(p, process),
		   make_small_or_big(l->size, process),
		   pid);
      hp += 5;
      list = CONS(hp, tup, list);
      hp += 2;

      l = l->next;
   }

   while (l != NULL)
   {
      if (is_non_value(l->p))
	 pid = am_undefined;
      else
      {
	 hp = HAlloc(process, 4);
	 pid = TUPLE3(hp,
		      make_small(pid_node(l->p)),
		      make_small(pid_number(l->p)),
		      make_small(pid_serial(l->p)));
      }

      hp = HAlloc(process, 5);
      p = ((unsigned) l) + sizeof(mem_link);
      tup = TUPLE4(hp,
		   make_small(l->type),
		   make_small_or_big(p, process),
		   make_small_or_big(l->size, process),
		   pid);
      hp = HAlloc(process, 2);
      list = CONS(hp, tup, list);

      l = l->next;
   }

   return list;
}

#endif

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(buf,sz)
byte* buf;
int sz;
{
    bin_write(CERR,buf,sz);
}

/* Print an atom as an uint32 or just as an index */     
void pat(Eterm atom)
{
    upp(atom_tab(atom_val(atom))->name,
	atom_tab(atom_val(atom))->len);
}


void pinfo()
{
    process_info(COUT);
}


void pp(p)
Process *p;
{
    print_process_info(p,CERR);
}
    
void ppi(Eterm pid)
{
    pp(process_tab[pid_number(pid)]);
}

void td(x) 
uint32 x;
{
    display(x, CERR);
    erl_putc('\n', CERR);
}

void ps(p, stop)
Process* p; uint32* stop;
{
    uint32* sp = p->hend-1;

    if (stop <= p->htop) {
	stop = p->htop + 1;
    }

    while(sp >= stop) {
	erl_printf(COUT,"%08lx: ", (uint32) sp);
	ldisplay(*sp, COUT, 75);
	erl_putc('\r', COUT);
	erl_putc('\n', COUT);
	sp--;
    }
}
#endif

