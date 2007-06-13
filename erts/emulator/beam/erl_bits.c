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
#include "error.h"
#include "bif.h"
#include "big.h"
#include "erl_bits.h"
#include "erl_binary.h"

#ifdef MAX
#undef MAX
#endif
#define MAX(x,y) (((x)>(y))?(x):(y))
#ifdef MIN
#undef MIN
#endif
#define MIN(x,y) (((x)<(y))?(x):(y))

#if defined(WORDS_BIGENDIAN)
# define BIT_ENDIAN_MACHINE 0
#else
# define BIT_ENDIAN_MACHINE BSF_LITTLE
#endif

#define BIT_IS_MACHINE_ENDIAN(x) (((x)&BSF_LITTLE) == BIT_ENDIAN_MACHINE)

/*
 * MAKE_MASK(n) constructs a mask with n bits.
 * Example: MAKE_MASK(3) returns the binary number 00000111.
 */

#define MAKE_MASK(n) ((1 << (n))-1)

/*
 * MASK_BITS assign src to dst, but preserves the dst bits outside the mask.
 */

#define MASK_BITS(src,dst,mask) (((src) & (mask)) | ((dst) & ~(mask)))

static byte get_bit(byte b, size_t a_offs); 

#if defined(ERTS_SMP)
/* the state resides in the current process' scheduler data */
#elif defined(ERL_BITS_REENTRANT)
/* reentrant API but with a hidden single global state, for testing only */
struct erl_bits_state ErlBitsState_;
#else
/* non-reentrant API with a single global state */
struct erl_bits_state ErlBitsState;
#endif

#define byte_buf	(ErlBitsState.byte_buf_)
#define byte_buf_len	(ErlBitsState.byte_buf_len_)

#ifdef ERTS_SMP
static erts_smp_atomic_t bits_bufs_size;
#endif

Uint
erts_bits_bufs_size(void)
{
#if defined(HEAP_FRAG_ELIM_TEST)
    return 0;
#else
#ifdef ERTS_SMP
    return (Uint) erts_smp_atomic_read(&bits_bufs_size);
#else
    ERL_BITS_DECLARE_STATEP;
    return (Uint) (byte_buf_len + erts_bin_buf_len);
#endif
#endif
}

#if !defined(ERTS_SMP)
static
#endif


void
erts_bits_init_state(ERL_BITS_PROTO_0)
{
    byte_buf_len = 1;
    byte_buf = erts_alloc(ERTS_ALC_T_BITS_BUF, byte_buf_len);

#if !defined(HEAP_FRAG_ELIM_TEST)
    erts_bin_buf_len = 1;
    erts_bin_buf = erts_alloc(ERTS_ALC_T_BITS_BUF, erts_bin_buf_len);
#endif
    erts_bin_offset = 0;
#if defined(ERTS_SMP) && !defined(HEAP_FRAG_ELIM_TEST)
    erts_smp_atomic_add(&bits_bufs_size, byte_buf_len + erts_bin_buf_len);
#endif
}

#if defined(ERTS_SMP)
void
erts_bits_destroy_state(ERL_BITS_PROTO_0)
{
    erts_free(ERTS_ALC_T_BITS_BUF, byte_buf);
#if !defined(HEAP_FRAG_ELIM_TEST)
    erts_free(ERTS_ALC_T_BITS_BUF, erts_bin_buf);
#endif
}
#endif

void
erts_init_bits(void)
{
#if defined(ERTS_SMP)
    erts_smp_atomic_init(&bits_bufs_size, 0);
    /* erl_process.c calls erts_bits_init_state() on all state instances */
#else
    ERL_BITS_DECLARE_STATEP;
    erts_bits_init_state(ERL_BITS_ARGS_0);
#endif
}

#if !defined(HEAP_FRAG_ELIM_TEST)

/****************************************************************
 ***
 *** Matching binaries
 ***
 ****************************************************************/

int
erts_bs_start_match(ERL_BITS_PROTO_1(Eterm Binary))
{
    erts_InitMatchBuf(Binary, return 0);
    return 1;
}

int
erts_bs_skip_bits(ERL_BITS_PROTO_1(Uint num_bits))
{
    Uint new_offset = erts_mb.offset + num_bits;

    if (erts_mb.size < new_offset) {
	return 0;
    } else {
	erts_mb.offset = new_offset;
	return 1;
    }
}

int
erts_bs_skip_bits_all(ERL_BITS_PROTO_0)
{
    erts_mb.offset = erts_mb.size;
    return 1;
}

int
erts_bs_test_tail(ERL_BITS_PROTO_1(Uint num_bits))
{
    return erts_mb.size - erts_mb.offset == num_bits;
}

void
erts_bs_save(ERL_BITS_PROTO_1(int index))
{
    erts_save_mb[index] = erts_mb;
}

void
erts_bs_restore(ERL_BITS_PROTO_1(int index))
{
    erts_mb = erts_save_mb[index];
}

Eterm
erts_bs_get_integer(Process *p, Uint num_bits, unsigned flags)
{
    Uint bytes;
    Uint bits;
    Uint offs;
    byte bigbuf[64];
    byte* LSB;
    byte* MSB;
    Uint* hp;
    Uint v32;
    int sgn = 0;
    Eterm res = THE_NON_VALUE;
    ErlBinMatchBuffer* mb;
    ERL_BITS_DEFINE_STATEP(p);  /* This has to be at the end of the
				   declarations since the macro
				   sometimes expands to nothing.
				   VC++ and old gcc does not like
				   stray ';'s between declarations. */

    mb = &erts_mb;

    if (num_bits == 0) {
	return SMALL_ZERO;
    }

    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    bytes = NBYTES(num_bits);
    if ((bits = BIT_OFFSET(num_bits)) == 0) {  /* number of bits in MSB */
	bits = 8;
    }
    offs = 8 - bits;                  /* adjusted offset in MSB */

    if (bytes <= sizeof bigbuf) {
	LSB = bigbuf;
    } else {
	LSB = erts_alloc(ERTS_ALC_T_TMP, bytes);
    }
    MSB = LSB + bytes - 1;

    /*
     * Move bits to temporary buffer. We want the buffer to be stored in
     * little-endian order, since bignums are little-endian.
     */
    
    if (flags & BSF_LITTLE) {
	erts_copy_bits(mb->base, mb->offset, 1, LSB, 0, 1, num_bits);
	*MSB >>= offs;		/* adjust msb */
    } else {
	*MSB = 0;
	erts_copy_bits(mb->base, mb->offset, 1, MSB, offs, -1, num_bits);
    }
    mb->offset += num_bits;

    /*
     * Get the sign bit.
     */
    sgn = 0;
    if ((flags & BSF_SIGNED) && (*MSB & (1<<(bits-1)))) {
	byte* ptr = LSB; 
	byte c = 1;

	/* sign extend MSB */
	*MSB |= ~MAKE_MASK(bits);

	/* two's complement */
	while (ptr <= MSB) {
	    byte pd = ~(*ptr);
	    byte d = pd + c;
	    c = (d < pd);
	    *ptr++ = d;
	}
	sgn = 1;
    }

    /* normalize */
    while ((*MSB == 0) && (MSB > LSB)) {
	MSB--;
	bytes--;
    }

    /* Check for guaranteed small */
    switch (bytes) {
    case 1:
	v32 = LSB[0];
	goto big_small;
    case 2:
	v32 = LSB[0] + (LSB[1]<<8); 
	goto big_small; 
    case 3: 
	v32 = LSB[0] + (LSB[1]<<8) + (LSB[2]<<16); 
	goto big_small;
    case 4:
	v32 = (Uint32)(LSB[0] + (LSB[1]<<8) + (LSB[2]<<16) + (LSB[3]<<24));
#if !defined(ARCH_64)
	if (!IS_USMALL(sgn, v32)) {
	    hp = ArithAlloc(p, BIG_UINT_HEAP_SIZE);
	    if (sgn) {
		hp[0] = make_neg_bignum_header(1);
	    } else {
		hp[0] = make_pos_bignum_header(1);
	    }
	    BIG_DIGIT(hp,0) = DLOW(v32);
	    BIG_DIGIT(hp,1) = DHIGH(v32);
	    res = make_big(hp);
	    break;
	}
#endif
    big_small:
	if (sgn) {
	    res = make_small(-((Sint)v32));
	} else {
	    res = make_small(v32);
	}
	break;
    default:
	hp = ArithAlloc(p, 1+WSIZE(bytes));
	res = bytes_to_big(LSB, bytes, sgn, hp);
#if !defined(ARCH_64)
	ASSERT(is_big(res));
#else
	/* 64 bit CPU: The result may be small. */
	if (is_small(res)) {
	    erts_arith_shrink(p, hp);
	}
#endif
	break;
    }

    if (LSB != bigbuf) {
	erts_free(ERTS_ALC_T_TMP, (void *) LSB);
    }
    return res;
}

Eterm
erts_bs_get_binary(Process *p, Uint num_bits, unsigned flags)
{
    ErlBinMatchBuffer* mb;
    ErlSubBin* sb;
    ERL_BITS_DEFINE_STATEP(p); /* Has to be at the end of declarations */

    mb = &erts_mb;

    if (num_bits == 0) {		/* Empty binary. */
	return new_binary_arith(p, NULL, 0);
    }
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    /*
     * From now on, we can't fail.
     */
    
    sb = (ErlSubBin *) ArithAlloc(p, ERL_SUB_BIN_SIZE);
    sb->thing_word = HEADER_SUB_BIN;
    sb->orig = mb->orig;
    sb->size = BYTE_OFFSET(num_bits);
    sb->bitsize = BIT_OFFSET(num_bits);
    sb->offs = BYTE_OFFSET(mb->offset);
    sb->bitoffs = BIT_OFFSET(mb->offset);
    mb->offset += num_bits;
    return make_binary(sb);
}

Eterm
erts_bs_get_float(Process *p, Uint num_bits, unsigned flags)
{
    Eterm* hp;
    float f32;
    double f64;
    byte* fptr;
    FloatDef f;
    ErlBinMatchBuffer* mb;
    ERL_BITS_DEFINE_STATEP(p); /* Has to be at the end of declarations */

    mb = &erts_mb;

    if (num_bits == 0) {
	f.fd = 0.0;
	hp = ArithAlloc(p, FLOAT_SIZE_OBJECT);
	PUT_DOUBLE(f, hp);
	ArithCheck(p);
	return make_float(hp);
    }
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (num_bits == 32) {
	fptr = (byte *) &f32;
    } else if (num_bits == 64) {
	fptr = (byte *) &f64;
    } else {
	return THE_NON_VALUE;
    }

    if (BIT_IS_MACHINE_ENDIAN(flags)) {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr, 0, 1,
		  num_bits);
    } else {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr + NBYTES(num_bits) - 1, 0, -1,
		  num_bits);
    }
    ERTS_FP_CHECK_INIT(p);
    if (num_bits == 32) {
	ERTS_FP_ERROR_THOROUGH(p, f32, return THE_NON_VALUE);
	f.fd = f32;
    } else {
	ERTS_FP_ERROR_THOROUGH(p, f64, return THE_NON_VALUE);
	f.fd = f64;
    }
    mb->offset += num_bits;
    hp = ArithAlloc(p, FLOAT_SIZE_OBJECT);
    PUT_DOUBLE(f, hp);
    ArithCheck(p);
    return make_float(hp);
}

Eterm
erts_bs_get_binary_all(Process *p)
{
    ErlSubBin* sb;
    Uint size;
    ERL_BITS_DEFINE_STATEP(p);

    size = erts_mb.size-erts_mb.offset;
    if (BIT_OFFSET(size) == 0) {
	sb = (ErlSubBin *) ArithAlloc(p, ERL_SUB_BIN_SIZE);
	sb->thing_word = HEADER_SUB_BIN;
	sb->size = BYTE_OFFSET(size);
	sb->bitsize = BIT_OFFSET(size);
	sb->offs = BYTE_OFFSET(erts_mb.offset);
	sb->bitoffs = BIT_OFFSET(erts_mb.offset);
	sb->orig = erts_mb.orig;
	erts_mb.offset = erts_mb.size;
	return make_binary(sb);
    }
    return THE_NON_VALUE;
}
#endif

/*****************************************************************
 ***
 *** New matching binaries functions
 ***
 *****************************************************************/

#define HeapOnlyAlloc(p, sz)					\
    (ASSERT_EXPR((sz) >= 0),					\
     (ASSERT_EXPR(((HEAP_LIMIT(p) - HEAP_TOP(p)) >= (sz))),	\
      (HEAP_TOP(p) = HEAP_TOP(p) + (sz), HEAP_TOP(p) - (sz))))

#define ReadToVariable(v64, Buffer, x)		\
  do{						\
    int _i;					\
    v64 = 0;					\
    for(_i = 0; _i < x; _i++) {			\
      v64 = ((Uint)Buffer[_i] <<(8*_i)) + v64;	\
	}					\
  }while(0)					\

Eterm
erts_bs_start_match_2(Process *p, Eterm Binary, Uint Max)
{
    if (!is_binary(Binary)) {
	return THE_NON_VALUE;
    } else { 
	Eterm Orig;
	Uint offs;
	Uint* hp;
	Uint NeededSize;
	ErlBinMatchState *ms;
	Uint bitoffs;
	Uint bitsize;
	Uint total_bin_size;
	total_bin_size = binary_size(Binary);
	if ((total_bin_size >> (8*sizeof(Uint)-3)) != 0) {
	    return THE_NON_VALUE;
	}
	NeededSize = ERL_BIN_MATCHSTATE_SIZE(Max);
	hp = HeapOnlyAlloc(p, NeededSize);
	ms = (ErlBinMatchState *) hp;                         
	ERTS_GET_REAL_BIN(Binary, Orig, offs, bitoffs, bitsize);
	ms->thing_word = HEADER_BIN_MATCHSTATE(Max);
	(ms->mb).orig = Orig;
	(ms->mb).base = binary_bytes(Orig);
	(ms->mb).offset = 8 * offs + bitoffs;
	(ms->mb).size = total_bin_size * 8 + (ms->mb).offset + bitsize;
	return make_matchstate(ms);
    }    
}

Eterm
erts_bs_get_integer_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    Uint bytes;
    Uint bits;
    Uint offs;
    byte bigbuf[64];
    byte* LSB;
    byte* MSB;
    Uint* hp;
    Uint* hp_end;
    Uint words_needed;
    Uint actual;
    Uint v32;
    int sgn = 0;
    Eterm res = THE_NON_VALUE;
	
    if (num_bits == 0) {
	return SMALL_ZERO;
    }
    
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    bytes = NBYTES(num_bits);
    if ((bits = BIT_OFFSET(num_bits)) == 0) {  /* number of bits in MSB */
	bits = 8;
    }
    offs = 8 - bits;                  /* adjusted offset in MSB */

    if (bytes <= sizeof bigbuf) {
	LSB = bigbuf;
    } else {
	LSB = erts_alloc(ERTS_ALC_T_TMP, bytes);
    }
    MSB = LSB + bytes - 1;

    /*
     * Move bits to temporary buffer. We want the buffer to be stored in
     * little-endian order, since bignums are little-endian.
     */
    
    if (flags & BSF_LITTLE) {
	erts_copy_bits(mb->base, mb->offset, 1, LSB, 0, 1, num_bits);
	*MSB >>= offs;		/* adjust msb */
    } else {
	*MSB = 0;
	erts_copy_bits(mb->base, mb->offset, 1, MSB, offs, -1, num_bits);
    }
    mb->offset += num_bits;

    /*
     * Get the sign bit.
     */
    sgn = 0;
    if ((flags & BSF_SIGNED) && (*MSB & (1<<(bits-1)))) {
	byte* ptr = LSB; 
	byte c = 1;

	/* sign extend MSB */
	*MSB |= ~MAKE_MASK(bits);

	/* two's complement */
	while (ptr <= MSB) {
	    byte pd = ~(*ptr);
	    byte d = pd + c;
	    c = (d < pd);
	    *ptr++ = d;
	}
	sgn = 1;
    }

    /* normalize */
    while ((*MSB == 0) && (MSB > LSB)) {
	MSB--;
	bytes--;
    }

    /* check for guaranteed small num */
    switch (bytes) {
    case 1:
	v32 = LSB[0];
	goto big_small;
    case 2:
	v32 = LSB[0] + (LSB[1]<<8); 
	goto big_small; 
    case 3: 
	v32 = LSB[0] + (LSB[1]<<8) + (LSB[2]<<16); 
	goto big_small;
#if !defined(ARCH_64)
    case 4:
	v32 = (LSB[0] + (LSB[1]<<8) + (LSB[2]<<16) + (LSB[3]<<24));
	if (!IS_USMALL(sgn, v32)) {
	  goto make_big;
	}
#else
    case 4:
      ReadToVariable(v32, LSB, 4);					
      goto big_small;
    case 5:	
      ReadToVariable(v32, LSB, 5);					
      goto big_small;
    case 6:	
      ReadToVariable(v32, LSB, 6);
      goto big_small; 
    case 7:
      ReadToVariable(v32, LSB, 7);
      goto big_small; 
    case 8:
      ReadToVariable(v32, LSB, 8);
      if (!IS_USMALL(sgn, v32)) {
	goto make_big;   
	}
#endif   
    big_small:			/* v32 loaded with value which fits in fixnum */
	if (sgn) {
	    res = make_small(-((Sint)v32));
	} else {
	    res = make_small(v32);
	}
	break;
    make_big:
	hp = HeapOnlyAlloc(p, BIG_UINT_HEAP_SIZE);
	if (sgn) {
	  hp[0] = make_neg_bignum_header(1);
	} else {
	  hp[0] = make_pos_bignum_header(1);
	}
	BIG_DIGIT(hp,0) = DLOW(v32);
	BIG_DIGIT(hp,1) = DHIGH(v32);
	res = make_big(hp);
	break;
    default:
	words_needed = 1+WSIZE(bytes);
	hp = HeapOnlyAlloc(p, words_needed);
	hp_end = hp + words_needed;
	res = bytes_to_big(LSB, bytes, sgn, hp); 
	if (is_small(res)) {
	    p->htop = hp;
	} else if ((actual = bignum_header_arity(*hp)+1) < words_needed) {
	    p->htop = hp + actual;
	}
	break;
    }

    if (LSB != bigbuf) {
	erts_free(ERTS_ALC_T_TMP, (void *) LSB);
    }
    return res;
}

Eterm
erts_bs_get_binary_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    ErlSubBin* sb;
    size_t num_bytes;		/* Number of bytes in binary. */

    if (num_bits == 0) {		/* Empty binary. */
	return new_binary(p, NULL, 0);
    }
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    

    /*
     * From now on, we can't fail.
     */

    num_bytes = NBYTES(num_bits);
    sb = (ErlSubBin *) HeapOnlyAlloc(p, ERL_SUB_BIN_SIZE);
    
    sb->thing_word = HEADER_SUB_BIN;
    sb->orig = mb->orig;
    sb->size = BYTE_OFFSET(num_bits);
    sb->bitsize = BIT_OFFSET(num_bits);
    sb->offs = BYTE_OFFSET(mb->offset);
    sb->bitoffs = BIT_OFFSET(mb->offset);
    mb->offset += num_bits;
    
    return make_binary(sb);
}

Eterm
erts_bs_get_float_2(Process *p, Uint num_bits, unsigned flags, ErlBinMatchBuffer* mb)
{
    Eterm* hp;
    float f32;
    double f64;
    byte* fptr;
    FloatDef f;

    if (num_bits == 0) {
	f.fd = 0.0;
	hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
	PUT_DOUBLE(f, hp);
	return make_float(hp);
    }
    if (mb->size - mb->offset < num_bits) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (num_bits == 32) {
	fptr = (byte *) &f32;
    } else if (num_bits == 64) {
	fptr = (byte *) &f64;
    } else {
	return THE_NON_VALUE;
    }

    if (BIT_IS_MACHINE_ENDIAN(flags)) {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr, 0, 1,
		  num_bits);
    } else {
	erts_copy_bits(mb->base, mb->offset, 1,
		  fptr + NBYTES(num_bits) - 1, 0, -1,
		  num_bits);
    }
    ERTS_FP_CHECK_INIT(p);
    if (num_bits == 32) {
	ERTS_FP_ERROR_THOROUGH(p, f32, return THE_NON_VALUE);
	f.fd = f32;
    } else {
	ERTS_FP_ERROR_THOROUGH(p, f64, return THE_NON_VALUE);
	f.fd = f64;
    }
    mb->offset += num_bits;
    hp = HeapOnlyAlloc(p, FLOAT_SIZE_OBJECT);
    PUT_DOUBLE(f, hp);
    return make_float(hp);
}

Eterm
erts_bs_get_binary_all_2(Process *p, ErlBinMatchBuffer* mb)
{
  ErlSubBin* sb;
  Uint size;
  size =  mb->size-mb->offset;
  sb = (ErlSubBin *) HeapOnlyAlloc(p, ERL_SUB_BIN_SIZE);
  sb->thing_word = HEADER_SUB_BIN;
  sb->size = BYTE_OFFSET(size);
  sb->bitsize = BIT_OFFSET(size);
  sb->offs = BYTE_OFFSET(mb->offset);
  sb->bitoffs = BIT_OFFSET(mb->offset);
  sb->orig = mb->orig;
  mb->offset = mb->size;  
  return make_binary(sb);
}

void
erts_bs_save_2(int index, ErlBinMatchState* ms)
{
  ms->save_offset[index] = (ms->mb).offset;
}

void
  erts_bs_restore_2(int index, ErlBinMatchState* ms)
{
  (ms->mb).offset = ms->save_offset[index];
}


/****************************************************************
 ***
 *** Building binaries
 ***
 ****************************************************************/


/* COPY_VAL:
 * copy sz byte from val to dst buffer, 
 * dst, val are updated!!!
 */

#define COPY_VAL(dst,ddir,val,sz) do { \
   Uint __sz = (sz); \
   while(__sz) { \
     switch(__sz) { \
     default: \
     case 4: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 3: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 2: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     case 1: *dst = (val&0xff); dst += ddir; val >>= 8; __sz--; \
     } \
   } \
 } while(0)

/* calculate a - *cp (carry)  (store result in b), *cp is updated! */
#define SUBc(a, cp, b) do { \
   byte __x = (a); \
   byte __y = (__x - (*(cp))); \
   (*cp) = (__y > __x); \
   *(b) = ~__y; \
 } while(0)
  
static int
fmt_int(byte *buf, Uint sz, Eterm val, Uint size, Uint flags)
{
    unsigned long offs;

    if (size == 0) {
	return 0;
    }

    offs = BIT_OFFSET(size);

    if (is_small(val)) {
	Sint v = signed_val(val);
	if (flags & BSF_LITTLE) { /* Little endian */
	    sz--;
	    COPY_VAL(buf,1,v,sz);
	    *buf = offs ? ((v << (8-offs)) & 0xff) : (v & 0xff);
	} else {		/* Big endian */
	    buf += (sz - 1);
	    if (offs) {
		*buf-- = (v << (8-offs)) & 0xff;
		sz--;
		v >>= offs;
	    }
	    COPY_VAL(buf,-1,v,sz);
	}
    } else if (is_big(val)) {
	int sign   = big_sign(val);
	Uint ds  = big_size(val)*sizeof(digit_t);  /* number of digits bytes */
	digit_t* dp = big_v(val);
	int n = MIN(sz,ds);

	if (flags & BSF_LITTLE) {
	    sz -= n;                       /* pad with this amount */
	    if (sign) {
		int c = 1;
		while(n >= sizeof(digit_t)) {
		    digit_t d = *dp++;
		    int i;
		    for(i = 0; i < sizeof(digit_t); ++i) {
			SUBc((d&0xff), &c, buf);
			buf++;
			d >>= 8;
		    }
		    n -= sizeof(digit_t);
		}
		if (n) {
		    digit_t d = *dp;
		    do {
			SUBc((d&0xff), &c, buf);
			buf++;
			d >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz--) {
		    SUBc(0, &c, buf);
		    buf++;
		}
	    }
	    else {
		while(n >= sizeof(digit_t)) {
		    digit_t d = *dp++;
		    int i;
		    for(i = 0; i < sizeof(digit_t); ++i) {
			*buf++ = (d & 0xff);
			d >>= 8;
		    }
		    n -= sizeof(digit_t);
		}
		if (n) {
		    digit_t d = *dp;
		    do {
			*buf++ = (d & 0xff);
			d >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz) {
		    *buf++ = 0;
		    sz--;
		}
	    }
	    /* adjust MSB!!! */
	    if (offs) {
		buf--;
		*buf <<= (8 - offs);
	    }
	}
	else {   /* BIG ENDIAN */
	    Uint acc = 0;	/* acc must be large enough to fit two digits */

	    buf += (sz - 1);              /* end of buffer */
	    sz -= n;                      /* pad with this amount */
	    offs = offs ? (8-offs) : 0;   /* shift offset */

	    if (sign) { /* SIGNED */
		int c = 1;

		while(n >= sizeof(digit_t)) {
		    int i;
		    acc |= (*dp++ << offs);
		    for(i = 0; i < sizeof(digit_t); ++i) {
			SUBc((acc&0xff), &c, buf);
			buf--;
			acc >>= 8;
		    }
		    n -= sizeof(digit_t);
		}
		if (n) {
		    acc |= (*dp << offs);
		    do {
			SUBc((acc & 0xff), &c, buf);
			buf--;
			acc >>= 8;
		    } while (--n > 0);
		}
		/* pad */
		while(sz--) {
		    SUBc((acc & 0xff), &c, buf);
		    buf--;
		    acc >>= 8;
		}
	    }
	    else { /* UNSIGNED */
		while(n >= sizeof(digit_t)) {
		    int i;
		    acc |= (*dp++ << offs);
		    for(i = 0; i < sizeof(digit_t); ++i) {
			*buf-- = (acc & 0xff);
			acc >>= 8;
		    }
		    n -= sizeof(digit_t);
		}
		if (n) {
		    acc |= (*dp << offs);
		    do {
			*buf-- = acc & 0xff;
			acc >>= 8;
		    } while (--n > 0);
		}
		while(sz--) {
		    *buf-- = acc & 0xff;
		    acc >>= 8;
		}
	    }
	}
    } else {			/* Neither small nor big */
	return -1;
    }
    return 0;
}

static void
ERTS_INLINE need_byte_buf(ERL_BITS_PROTO_1(int need))
{
    if (byte_buf_len < need) {
#ifdef ERTS_SMP
	erts_smp_atomic_add(&bits_bufs_size, need - byte_buf_len);
#endif
	byte_buf_len = need;
	byte_buf = erts_realloc(ERTS_ALC_T_BITS_BUF, byte_buf, byte_buf_len);
    }
}

int
erts_new_bs_put_integer(ERL_BITS_PROTO_3(Eterm arg, Uint num_bits, unsigned flags))
{
    unsigned bin_offset = erts_bin_offset;

    if (BIT_OFFSET(bin_offset) == 0) {
	if (fmt_int(erts_current_bin+BYTE_OFFSET(bin_offset),
		    NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
    } else {
	byte *iptr;
	
	need_byte_buf(ERL_BITS_ARGS_1(NBYTES(num_bits)));
	iptr = byte_buf;
	if (fmt_int(iptr, NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
	erts_copy_bits(iptr, 0, 1, erts_current_bin, bin_offset, 1, num_bits);
    }
    erts_bin_offset = bin_offset + num_bits;
    return 1;
}

int
erts_new_bs_put_binary(ERL_BITS_PROTO_2(Eterm arg, Uint num_bits))
{
    byte *bptr;
    Uint bitoffs;
    Uint bitsize; 

    if (!is_binary(arg)) {
	return 0;
    }
    ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
    if (num_bits > 8*binary_size(arg)+bitsize) {
	return 0;
    }
    copy_binary_to_buffer(erts_current_bin, erts_bin_offset, bptr, bitoffs, num_bits);
    erts_bin_offset += num_bits;
    return 1;
}

int
erts_new_bs_put_binary_all(ERL_BITS_PROTO_1(Eterm arg))
{
   byte *bptr;
   Uint bitoffs;
   Uint bitsize;
   Uint num_bits;

   if (!is_binary(arg)) {
       return 0;
   }
   ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
   num_bits = 8*binary_size(arg)+bitsize;
   copy_binary_to_buffer(erts_current_bin, erts_bin_offset, bptr, bitoffs, num_bits);
   erts_bin_offset += num_bits;
   return 1;
}

int
erts_new_bs_put_float(Process *c_p, Eterm arg, Uint num_bits, int flags)
{
    ERL_BITS_DEFINE_STATEP(c_p);

    if (BIT_OFFSET(erts_bin_offset) == 0) {
	Uint32 a;
	Uint32 b;
	
	if (num_bits == 64) {
	    union {
		double f64;
		Uint32 i32[2];
	    } u;

	    if (is_float(arg)) {
		FloatDef *fdp = (FloatDef*)(float_val(arg) + 1);
		a = fdp->fw[0];
		b = fdp->fw[1];
	    } else if (is_small(arg)) {
		u.f64 = (double) signed_val(arg);
		a = u.i32[0];
		b = u.i32[1];
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &u.f64) < 0) {
		    return 0;
		}
		a = u.i32[0];
		b = u.i32[1];
	    } else {
		return 0;
	    }
	} else if (num_bits == 32) {
	    union {
		float f32;
		Uint32 i32;
	    } u;

	    b = 0;
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		u.f32 = f.fd;
		ERTS_FP_ERROR(c_p,u.f32,;);
		a = u.i32;
	    } else if (is_small(arg)) {
		u.f32 = (float) signed_val(arg);
		a = u.i32;
	    } else if (is_big(arg)) {
		double f64;
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
		ERTS_FP_CHECK_INIT(c_p);
		u.f32 = (float) f64;
		ERTS_FP_ERROR(c_p,u.f32,;);
		a = u.i32;
	    } else {
		return 0;
	    }
	} else {
	    return 0;
	}

	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    byte* t = erts_current_bin+BYTE_OFFSET(erts_bin_offset);
#ifdef WORDS_BIGENDIAN
	    t[0] = a >> 24;
	    t[1] = a >> 16;
	    t[2] = a >> 8;
	    t[3] = a;
	    if (num_bits == 64) {
		t[4] = b >> 24;
		t[5] = b >> 16;
		t[6] = b >> 8;
		t[7] = b;
	    }
#else
	    t[3] = a >> 24;
	    t[2] = a >> 16;
	    t[1] = a >> 8;
	    t[0] = a;
	    if (num_bits == 64) {
		t[7] = b >> 24;
		t[6] = b >> 16;
		t[5] = b >> 8;
		t[4] = b;
	    }
#endif
	} else {
	    byte* t = erts_current_bin+BYTE_OFFSET(erts_bin_offset) + NBYTES(num_bits);
#ifdef WORDS_BIGENDIAN
	    t[-1] = a >> 24;
	    t[-2] = a >> 16;
	    t[-3] = a >> 8;
	    t[-4] = a;
	    if (num_bits == 64) {
		t[-5] = b >> 24;
		t[-6] = b >> 16;
		t[-7] = b >> 8;
		t[-8] = b;
	    }
#else
	    t[-1] = a;
	    t[-2] = a >> 8;
	    t[-3] = a >> 16;
	    t[-4] = a >> 24;
	    if (num_bits == 64) {
		t[-5] = b;
		t[-6] = b >> 8;
		t[-7] = b >> 16;
		t[-8] = b >> 24;
	    }
#endif
	}
    } else {
	byte *bptr;
	double f64;
	float f32;
	
	if (num_bits == 64) {
	    if (is_float(arg)) {
		bptr = (byte *) (float_val(arg) + 1);
	    } else if (is_small(arg)) {
		f64 = (double) signed_val(arg);
		bptr = (byte *) &f64;
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
		bptr = (byte *) &f64;
	    } else {
		return 0;
	    }
	} else if (num_bits == 32) {
	    if (is_float(arg)) {
		FloatDef f;
		GET_DOUBLE(arg, f);
		ERTS_FP_CHECK_INIT(c_p);
		f32 = f.fd;
		ERTS_FP_ERROR(c_p,f32,;);
		bptr = (byte *) &f32;
	    } else if (is_small(arg)) {
		f32 = (float) signed_val(arg);
		bptr = (byte *) &f32;
	    } else if (is_big(arg)) {
		if (big_to_double(arg, &f64) < 0) {
		    return 0;
		}
		ERTS_FP_CHECK_INIT(c_p);
		f32 = (float) f64;
		ERTS_FP_ERROR(c_p,f32,;);
		bptr = (byte *) &f32;
	    } else {
		return 0;
	    }
	} else {
	    return 0;
	}
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    erts_copy_bits(bptr, 0, 1,
		      erts_current_bin,
		      erts_bin_offset, 1, num_bits);
	} else {
	    erts_copy_bits(bptr+NBYTES(num_bits)-1, 0, -1,
			   erts_current_bin, erts_bin_offset, 1,
			   num_bits);
	}
    }
    erts_bin_offset += num_bits;
    return 1;
}

void 
erts_new_bs_put_string(ERL_BITS_PROTO_2(byte* iptr, Uint num_bytes))
{
    if (BIT_OFFSET(erts_bin_offset) != 0) {
	erts_copy_bits(iptr, 0, 1, erts_current_bin, erts_bin_offset, 1, num_bytes*8);
    } else {
	sys_memcpy(erts_current_bin+BYTE_OFFSET(erts_bin_offset), iptr, num_bytes);
    }
    erts_bin_offset += num_bytes*8;
}

Eterm
erts_bs_final2(Process* p, Eterm bin)
{ 
    ErlSubBin* sb;
    ERL_BITS_DEFINE_STATEP(p);
    if ((erts_bin_offset & 7) == 0) {return bin;}
    sb = (ErlSubBin *) HeapOnlyAlloc(p, ERL_SUB_BIN_SIZE);
    sb->thing_word = HEADER_SUB_BIN;
    sb->size = (erts_bin_offset - (erts_bin_offset & 7))/8;
    sb->bitsize = erts_bin_offset & 7;
    sb->offs = 0;
    sb->bitoffs = 0;
    sb->orig = bin;
    return make_binary(sb);
}

#if !defined(HEAP_FRAG_ELIM_TEST)

/*
 * Old instructions for building binaries.
 */

static void
ERTS_INLINE need_bin_buf(ERL_BITS_PROTO_1(int need))
{
    if (erts_bin_buf_len < need) {
#ifdef ERTS_SMP
	erts_smp_atomic_add(&bits_bufs_size, need - erts_bin_buf_len);
#endif
	erts_bin_buf_len = need;
	erts_bin_buf = erts_realloc(ERTS_ALC_T_BITS_BUF, erts_bin_buf, erts_bin_buf_len);
    }
}

void
erts_bs_init(ERL_BITS_PROTO_0)
{
    erts_bin_offset = 0;
}


Eterm
erts_bs_final(Process* p)
{
    ERL_BITS_DEFINE_STATEP(p);
    if (erts_bin_offset % 8 != 0) {
	return THE_NON_VALUE;
    }
    return new_binary_arith(p, erts_bin_buf, erts_bin_offset / 8);
}

int
erts_bs_put_integer(ERL_BITS_PROTO_3(Eterm arg, Uint num_bits, unsigned flags))
{
    unsigned bin_offset = erts_bin_offset;

    if (BIT_OFFSET(bin_offset) == 0) {
	need_bin_buf(ERL_BITS_ARGS_1(NBYTES(num_bits+bin_offset)));
	if (fmt_int(erts_bin_buf+BYTE_OFFSET(bin_offset),
		    NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
    } else {
	byte *iptr;

	need_byte_buf(ERL_BITS_ARGS_1(NBYTES(num_bits)));
	iptr = byte_buf;
	if (fmt_int(iptr, NBYTES(num_bits), arg, num_bits, flags) < 0) {
	    return 0;
	}
	need_bin_buf(ERL_BITS_ARGS_1(NBYTES(num_bits+bin_offset)));
	erts_copy_bits(iptr, 0, 1, erts_bin_buf, bin_offset, 1, num_bits);
    }
    erts_bin_offset = bin_offset + num_bits;
    return 1;
}

int
erts_bs_put_binary(ERL_BITS_PROTO_2(Eterm arg, Uint num_bits))
{
    byte *bptr;
    Uint bitoffs;
    Uint bitsize;
    if (!is_binary(arg)) {
	return 0;
    }
    ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
    if (num_bits > 8*binary_size(arg)+bitsize) {
	return 0;
    }
    need_bin_buf(ERL_BITS_ARGS_1(NBYTES(num_bits + erts_bin_offset)));
    if ((BIT_OFFSET(erts_bin_offset) == 0) & (bitoffs == 0) & (BIT_OFFSET(num_bits) == 0)) {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset),
		   bptr, NBYTES(num_bits));
    } else {
	erts_copy_bits(bptr, bitoffs, 1, erts_bin_buf, erts_bin_offset, 1, num_bits);
    }
    erts_bin_offset += num_bits;
    return 1;
}

int
erts_bs_put_binary_all(ERL_BITS_PROTO_1(Eterm arg))
{
    byte *bptr;
    unsigned n;
    Uint bitoffs;
    Uint bitsize;

    if (!is_binary(arg)) {
	return 0;
    }
    ERTS_GET_BINARY_BYTES(arg, bptr, bitoffs, bitsize);
    n = 8*binary_size(arg)+bitsize; 
    need_bin_buf(ERL_BITS_ARGS_1(NBYTES(n + erts_bin_offset)));
    if ((BIT_OFFSET(erts_bin_offset) == 0) & (bitoffs == 0) & (bitsize == 0)) {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset), bptr, NBYTES(n));
    } else {
	erts_copy_bits(bptr, bitoffs, 1, erts_bin_buf, erts_bin_offset, 1, n);
    }
    erts_bin_offset += n;
    return 1;
}

int
erts_bs_put_float(Process *c_p, Eterm arg, Uint num_bits, int flags)
{
    byte *bptr;
    double f64;
    float f32;
    ERL_BITS_DEFINE_STATEP(c_p);

    if (num_bits == 64) {
	if (is_float(arg)) {
	    bptr = (byte *) (float_val(arg) + 1);
	} else if (is_small(arg)) {
	    f64 = (double) signed_val(arg);
	    bptr = (byte *) &f64;
	} else if (is_big(arg)) {
	    if (big_to_double(arg, &f64) < 0) {
		return 0;
	    }
	    bptr = (byte *) &f64;
	} else {
	    return 0;
	}
    } else if (num_bits == 32) {
	if (is_float(arg)) {
	    FloatDef f;
	    GET_DOUBLE(arg, f);
	    ERTS_FP_CHECK_INIT(c_p);
	    f32 = f.fd;
	    ERTS_FP_ERROR(c_p,f32,;);
	    bptr = (byte *) &f32;
	} else if (is_small(arg)) {
	    f32 = (float) signed_val(arg);
	    bptr = (byte *) &f32;
	} else if (is_big(arg)) {
	    if (big_to_double(arg, &f64) < 0) {
		return 0;
	    }
	    ERTS_FP_CHECK_INIT(c_p);
	    f32 = (float) f64;
	    ERTS_FP_ERROR(c_p,f32,;);
	    bptr = (byte *) &f32;
	} else {
	    return 0;
	}
    } else {
	return 0;
    }

    need_bin_buf(ERL_BITS_ARGS_1(NBYTES(num_bits + erts_bin_offset)));

    if (BIT_OFFSET(erts_bin_offset) == 0) {
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    byte* t = erts_bin_buf+BYTE_OFFSET(erts_bin_offset);
	    t[0] = bptr[0];
	    t[1] = bptr[1];
	    t[2] = bptr[2];
	    t[3] = bptr[3];
	    if (num_bits == 64) {
		t[4] = bptr[4];
		t[5] = bptr[5];
		t[6] = bptr[6];
		t[7] = bptr[7];
	    }
	} else {
	    byte* t = erts_bin_buf+BYTE_OFFSET(erts_bin_offset) + NBYTES(num_bits);
	    t[-1] = bptr[0];
	    t[-2] = bptr[1];
	    t[-3] = bptr[2];
	    t[-4] = bptr[3];
	    if (num_bits == 64) {
		t[-5] = bptr[4];
		t[-6] = bptr[5];
		t[-7] = bptr[6];
		t[-8] = bptr[7];
	    }
	}
    } else {
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    erts_copy_bits(bptr, 0, 1,
			   erts_bin_buf,
			   erts_bin_offset, 1, num_bits);
	} else {
	    erts_copy_bits(bptr+NBYTES(num_bits)-1, 0, -1,
			   erts_bin_buf, erts_bin_offset, 1,
			   num_bits);
	}
    }
    erts_bin_offset += num_bits;
    return 1;
}

void 
erts_bs_put_string(ERL_BITS_PROTO_2(byte* iptr, Uint num_bytes))
{
    need_bin_buf(ERL_BITS_ARGS_1(num_bytes + NBYTES(erts_bin_offset)));
    if (BIT_OFFSET(erts_bin_offset) != 0) {
	erts_copy_bits(iptr, 0, 1, erts_bin_buf, erts_bin_offset, 1, num_bytes*8);
    } else {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset), iptr, num_bytes);
    }
    erts_bin_offset += num_bytes*8;
}
#endif

static byte
get_bit(byte b, size_t offs) 
{
    return (b >> (7-offs)) & 1;
}

int
erts_cmp_bits(byte* a_ptr, size_t a_offs, byte* b_ptr, size_t b_offs, size_t size) 
{
    byte a;
    byte b;
    byte a_bit;
    byte b_bit;
    Uint lshift;
    Uint rshift;
    int cmp;
    
    if (((a_offs | b_offs | size) & 7) == 0) {
	int byte_size = size >> 3;
	return sys_memcmp(a_ptr, b_ptr, byte_size);
    }

    /* Compare bit by bit until a_ptr is aligned on byte boundary */
    a = *a_ptr++;
    b = *b_ptr++;
    while (size > 0) {
	a_bit = get_bit(a, a_offs);
	b_bit = get_bit(b, b_offs);
	if ((cmp = (a_bit-b_bit)) != 0) {
	    return cmp;
	}
	size--;
	b_offs++;
	if (b_offs == 8) {
	    b_offs = 0;
	    b = *b_ptr++;
	}
	a_offs++;
	if (a_offs == 8) {
	    a_offs = 0;
	    a = *a_ptr++;
	    break;
	}
    }

    /* Compare byte by byte as long as at least 8 bits remain */
    lshift = b_offs;
    rshift = 8 - lshift;
    while (size >= 8) {
	byte b_cmp = (b << lshift);
	b = *b_ptr++;
	b_cmp |= b >> rshift;
	if ((cmp = (a - b_cmp)) != 0) {
	    return cmp;
	}
	a = *a_ptr++;
	size -= 8;
    }

    /* Compare the remaining bits bit by bit */
    while (size > 0) {
	a_bit = get_bit(a, a_offs);
	b_bit = get_bit(b, b_offs);
	if ((cmp = (a_bit-b_bit)) != 0) {
	    return cmp;
	}
	a_offs++;
	if (a_offs == 8) {
	    a_offs = 0;
	    a = *a_ptr++;
	}
	b_offs++;
	if (b_offs == 8) {
	    b_offs = 0;
	    b = *b_ptr++;
	}
	size--;
    }

    return 0;
}

/*
 * The basic bit copy operation. Copies n bits from the source buffer to
 * the destination buffer. Depending on the directions, it can reverse the
 * copied bits.
 */


void 
erts_copy_bits(byte* src,	/* Base pointer to source. */
	       size_t soffs,	/* Bit offset for source relative to src. */
	       int sdir,	/* Direction: 1 (forward) or -1 (backward). */
	       byte* dst,	/* Base pointer to destination. */
	       size_t doffs,	/* Bit offset for destination relative to dst. */
	       int ddir,	/* Direction: 1 (forward) or -1 (backward). */
	       size_t n)	/* Number of bits to copy. */
{
    Uint lmask;
    Uint rmask;
    Uint count;
    Uint deoffs;

    if (n == 0) {
	return;
    }

    src += sdir*BYTE_OFFSET(soffs);
    dst += ddir*BYTE_OFFSET(doffs);
    soffs = BIT_OFFSET(soffs);
    doffs = BIT_OFFSET(doffs);
    deoffs = BIT_OFFSET(doffs+n);
    lmask = (doffs) ? MAKE_MASK(8-doffs) : 0;
    rmask = (deoffs) ? (MAKE_MASK(deoffs)<<(8-deoffs)) : 0;

    /*
     * Take care of the case that all bits are in the same byte.
     */

    if (doffs+n < 8) {		/* All bits are in the same byte */
	lmask = (lmask & rmask) ? (lmask & rmask) : (lmask | rmask);

	if (soffs == doffs) {
	    *dst = MASK_BITS(*src,*dst,lmask);
	} else if (soffs > doffs) {
	    Uint bits = (*src << (soffs-doffs));
	    if (soffs+n > 8) {
		src += sdir;
		bits |= (*src >> (8-(soffs-doffs)));
	    }
	    *dst = MASK_BITS(bits,*dst,lmask);
	} else {
	    *dst = MASK_BITS((*src >> (doffs-soffs)),*dst,lmask);
	}
	return;			/* We are done! */
    }

    /*
     * At this point, we know that the bits are in 2 or more bytes.
     */

    count = ((lmask) ? (n - (8 - doffs)) : n) >> 3;

    if (soffs == doffs) {
	/*
	 * The bits are aligned in the same way. We can just copy the bytes
	 * (except for the first and last bytes). Note that the directions
	 * might be different, so we can't just use memcpy().
	 */

	if (lmask) {
	    *dst = MASK_BITS(*src, *dst, lmask);
	    dst += ddir;
	    src += sdir;
	}

	while (count--) {
	    *dst = *src;
	    dst += ddir;
	    src += sdir;
	}

	if (rmask) {
	    *dst = MASK_BITS(*src,*dst,rmask);
	}
    } else {
	Uint bits;
	Uint bits1;
	Uint rshift;
	Uint lshift;

	/*
	 * The tricky case. The bits must be shifted into position.
	 */
	
	if (soffs > doffs) {
	    lshift = (soffs - doffs);
	    rshift = 8 - lshift;
	    bits = *src;
	    if (soffs + n > 8) {
		src += sdir;
	    }
	} else {
	    rshift = (doffs - soffs);
	    lshift = 8 - rshift;
	    bits = 0;
	}
	    
	if (lmask) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    bits1 |= (bits >> rshift);
	    *dst = MASK_BITS(bits1,*dst,lmask);
	    dst += ddir;
	}

	while (count--) {
	    bits1 = bits << lshift;
	    bits = *src;
	    src += sdir;
	    *dst = bits1 | (bits >> rshift);
	    dst += ddir;
	}
	
	if (rmask) {
	    bits1 = bits << lshift;
	    if ((rmask << rshift) & 0xff) {
		bits = *src;
		bits1 |= (bits >> rshift);
	    }
	    *dst = MASK_BITS(bits1,*dst,rmask);
	}
    }
}

