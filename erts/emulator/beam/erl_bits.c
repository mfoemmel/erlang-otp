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

typedef unsigned char Uint8;

#define BYTE_OFFSET(ofs) ((unsigned) (ofs) >> 3)
#define BIT_OFFSET(ofs) ((ofs) & 7)

#define MAX(x,y) (((x)>(y))?(x):(y))
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

/*
 * NBYTES(x) returns the number of bytes needed to store x bits.
 */

#define NBYTES(x)  (((x) + 7) >> 3) 

/*
 * Return number of Eterm words needed for allocation with HAlloc(),
 * given a number of bytes.
 */
#define WSIZE(n) ((n + sizeof(Eterm) - 1) / sizeof(Eterm))

static void
copy_bits(Uint8* src, size_t soffs, int sdir,
	  Uint8* dst, size_t doffs, int ddir,
	  size_t n);

Eterm
erts_get_integer(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags)
{
    Uint bytes;
    Uint bits;
    Uint offs;
    Uint8 bigbuf[64];
    Uint8* LSB;
    Uint8* MSB;
    Uint* hp;
    Uint v32;
    int sgn = 0;
    Eterm res = THE_NON_VALUE;

    if (size == 0) {
	return SMALL_ZERO;
    }

    if (mb->size - mb->offset < size) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }

    bytes = NBYTES(size);
    if ((bits = BIT_OFFSET(size)) == 0) {  /* number of bits in MSB */
	bits = 8;
    }
    offs = 8 - bits;                  /* adjusted offset in MSB */

    if (bytes <= sizeof bigbuf) {
	LSB = bigbuf;
    } else if ((LSB = sys_alloc(bytes)) == NULL) {
	erl_exit(1, "Insufficient memory");
    }
    MSB = LSB + bytes - 1;

    /*
     * Move bits to temporary buffer. We want the buffer to be stored in
     * little-endian order, since bignums are little-endian.
     */
    
    if (flags & BSF_LITTLE) {
	copy_bits(mb->base, mb->offset, 1, LSB, 0, 1, size);
	*MSB >>= offs;		/* adjust msb */
    } else {
	*MSB = 0;
	copy_bits(mb->base, mb->offset, 1, MSB, offs, -1, size);
    }
    mb->offset += size;

    /*
     * Get the sign bit.
     */
    sgn = 0;
    if ((flags & BSF_SIGNED) && (*MSB & (1<<(bits-1)))) {
	Uint8* ptr = LSB; 
	Uint8 c = 1;

	/* sign extend MSB */
	*MSB |= ~MAKE_MASK(bits);

	/* two's complement */
	while (ptr <= MSB) {
	    Uint8 pd = ~(*ptr);
	    Uint8 d = pd + c;
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
    case 4:
	v32 = LSB[0] + (LSB[1]<<8) + (LSB[2]<<16) + (LSB[3]<<24);
	if (!IS_USMALL(sgn, v32)) {
	    hp = HAlloc(p, BIG_NEED_SIZE(2));
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
    big_small:			/* v32 loaded with value (24 bit or less) */
	if (sgn) {
	    res = make_small(-((sint32)v32));
	} else {
	    res = make_small(v32);
	}
	break;
    default:
	hp = HAlloc(p, 1+WSIZE(bytes));
	res = bytes_to_big(LSB, bytes, sgn, hp);
	break;
    }

    if (LSB != bigbuf) {
	sys_free(LSB);
    }
    return res;
}

Eterm
erts_get_binary(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags)
{
    size_t num_bytes;		/* Number of bytes in binary. */

    if (size == 0) {		/* Empty binary. */
	return new_binary_arith(p, NULL, 0);
    }
    if (mb->size - mb->offset < size) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (BIT_OFFSET(size) != 0) { /* The size must be byte aligned. */
	return THE_NON_VALUE;
    }

    /*
     * From now on, we can't fail.
     */

    num_bytes = NBYTES(size);
    if (BIT_OFFSET(mb->offset) == 0) {
	ErlSubBin* sb = (ErlSubBin *) ArithAlloc(p, ERL_SUB_BIN_SIZE);

	/*
	 * Get a byte-aligned part of the old binary.
	 */

	sb->thing_word = HEADER_SUB_BIN;
	sb->orig = mb->orig;
	sb->size = num_bytes;
	sb->offs = BYTE_OFFSET(mb->offset);
	mb->offset += size;
	return make_binary(sb);
    } else {
	byte* bytes;
	Eterm bin = new_binary_arith(p, NULL, num_bytes);

	GET_BINARY_BYTES(bin, bytes);
	copy_bits(mb->base, mb->offset, 1, bytes, 0, 1, size);
	mb->offset += size;
	return bin;
    }
}

Eterm
erts_get_float(Process *p, ErlBinMatchBuffer* mb, int size, unsigned flags)
{
    Eterm* hp;
    float f32;
    double f64;
    byte* fptr;
    FloatDef f;
   
    if (size == 0) {
	f.fd = 0.0;
	hp = ArithAlloc(p, 3);
	PUT_DOUBLE(f, hp);
	ArithCheck(p);
	return make_float(hp);
    }
    if (mb->size - mb->offset < size) {	/* Asked for too many bits.  */
	return THE_NON_VALUE;
    }
    if (size == 32) {
	fptr = (byte *) &f32;
    } else if (size == 64) {
	fptr = (byte *) &f64;
    } else {
	return THE_NON_VALUE;
    }

    if (BIT_IS_MACHINE_ENDIAN(flags)) {
	copy_bits(mb->base, mb->offset, 1,
		  fptr, 0, 1,
		  size);
    } else {
	copy_bits(mb->base, mb->offset, 1,
		  fptr + NBYTES(size) - 1, 0, -1,
		  size);
    }
    mb->offset += size;

    if (!FP_PRE_CHECK_OK())
	return THE_NON_VALUE;

    if (size == 32) {
	if (!FP_RESULT_OK(f32)) {
	    return THE_NON_VALUE;
	}
	f.fd = f32;
    } else {
	if (!FP_RESULT_OK(f64)) {
	    return THE_NON_VALUE;
	}
	f.fd = f64;
    }

    hp = ArithAlloc(p, 3);
    PUT_DOUBLE(f, hp);
    ArithCheck(p);
    return make_float(hp);
}

Eterm
erts_get_binary_all(Process *p, ErlBinMatchBuffer* mb)
{
    if (BIT_OFFSET(mb->offset) == 0) {
	ErlSubBin* sb;
	
	sb = (ErlSubBin *) ArithAlloc(p, ERL_SUB_BIN_SIZE);
	sb->thing_word = HEADER_SUB_BIN;
	sb->size = BYTE_OFFSET(mb->size) - BYTE_OFFSET(mb->offset);
	sb->offs = BYTE_OFFSET(mb->offset);
	sb->orig = mb->orig;
	mb->offset = mb->size;
	return make_binary(sb);
    }
    return THE_NON_VALUE;
}


/*
 * The basic bit copy operation. Copies n bits from the source buffer to
 * the destination buffer. Depending on the directions, it can reverse the
 * copied bits.
 */
static void 
copy_bits(Uint8* src,		/* Base pointer to source. */
	  size_t soffs,		/* Bit offset for source relative to src. */
	  int sdir,		/* Direction: 1 (forward) or -1 (backward). */
	  Uint8* dst,		/* Base pointer to destination. */
	  size_t doffs,		/* Bit offset for destination relative to dst. */
	  int ddir,		/* Direction: 1 (forward) or -1 (backward). */
	  size_t n)		/* Number of bits to copy. */
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

static byte *byte_buf;
static int byte_buf_len;

byte* erts_bin_buf;
unsigned erts_bin_buf_len;
unsigned erts_bin_offset;

/* COPY_VAL:
** copy sz byte from val to dst buffer, 
** dst, val are updated!!!
*/
#define COPY_VAL(dst,ddir,val,sz) do { \
   uint32 __sz = (sz); \
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
  
void erts_init_bits(void)
{
    byte_buf = NULL;
    byte_buf_len = 0;
    
    erts_bin_buf = NULL;
    erts_bin_buf_len = 0;
    erts_bin_offset = 0;
}

    
static int
fmt_int(byte *buf, uint32 sz, Eterm val, uint32 size, uint32 flags)
{
    unsigned long offs;

    if (!is_integer(val))
	return -1;

    if (size == 0) return 0;

    offs = BIT_OFFSET(size);

    if (is_small(val)) {
	sint32 v = signed_val(val);
	if (flags & BSF_LITTLE) {
	    sz--;
	    COPY_VAL(buf,1,v,sz);
	    *buf = offs ? ((v << (8-offs)) & 0xff) : (v & 0xff);
	}
	else {  /* big */
	    buf += (sz - 1);
	    if (offs) {
		*buf-- = (v << (8-offs)) & 0xff;
		sz--;
		v >>= offs;
	    }
	    COPY_VAL(buf,-1,v,sz);
	}
    }
    else  { /* is_big(val) */
	int sign   = big_sign(val);
	uint32 ds  = big_size(val)*2;  /* number of digits bytes */
	digit_t* dp = big_v(val);
	int n = MIN(sz,ds);

	if (flags & BSF_LITTLE) {
	    sz -= n;                       /* pad with this amount */
	    if (sign) {
		int c = 1;
		while(n >= 2) {
		    digit_t d = *dp++;
		    SUBc((d&0xff), &c, buf);
		    buf++;

		    SUBc(((d>>8)&0xff), &c, buf);
		    buf++;
		    n -= 2;
		}
		if (n) {
		    digit_t d = *dp;
		    SUBc((d&0xff), &c, buf);
		    buf++;
		}
		/* pad */
		while(sz--) {
		    SUBc(0, &c, buf);
		    buf++;
		}
	    }
	    else {
		while(n >= 2) {
		    digit_t d = *dp++;
		    *buf++ = (d & 0xff);
		    *buf++ = ((d >> 8) & 0xff);
		    n -= 2;
		}
		if (n) 
		    *buf++ = (*dp & 0xff);
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
	    uint32 acc = 0; /* acc must be large enough to fit two digits */

	    buf += (sz - 1);              /* end of buffer */
	    sz -= n;                      /* pad with this amount */
	    offs = offs ? (8-offs) : 0;   /* shift offset */

	    if (sign) { /* SIGNED */
		int c = 1;

		while(n >= 2) {
		    acc |= (*dp++ << offs);

		    SUBc((acc&0xff), &c, buf);
		    buf--;
		    SUBc(((acc>>8)&0xff), &c, buf);
		    buf--;
		    acc >>= 16;
		    n -= 2;
		}
		if (n) {
		    acc |= (*dp << offs);
		    SUBc((acc & 0xff), &c, buf);
		    buf--;
		    acc >>= 8;
		}
		/* pad */
		while(sz--) {
		    SUBc((acc & 0xff), &c, buf);
		    buf--;
		    acc >>= 8;
		}
	    }
	    else { /* UNSIGNED */
		while(n >= 2) {
		    acc |= (*dp++ << offs);
		    *buf-- = (acc & 0xff);
		    *buf-- = (acc >> 8) & 0xff;
		    acc >>= 16;
		    n -= 2;
		}
		if (n) {
		    acc |= (*dp << offs);
		    *buf-- = acc & 0xff;
		    acc >>= 8;
		}
		while(sz--) {
		    *buf-- = acc & 0xff;
		    acc >>= 8;
		}
	    }
	}
    }
    return 0;
}

static void need_buf(byte **p, int *len, int need)
{
    if (*len < need) {
	if (*len == 0)
	    *p = sys_alloc(need);
	else
	    *p = sys_realloc(*p, *len + need);

	if (*p == NULL)
	    ERL_EXIT0(1, "Insufficient memory");
	*len = need;
    }
}

int
erts_put_integer(Eterm arg, unsigned n, unsigned flags)
{
    if (BIT_OFFSET(erts_bin_offset) == 0) {
	need_buf(&erts_bin_buf, &erts_bin_buf_len, NBYTES(n + erts_bin_offset));
	if (fmt_int(erts_bin_buf+BYTE_OFFSET(erts_bin_offset),
		    NBYTES(n), arg, n, flags) < 0) {
	    return 0;
	}
    } else {
	byte *iptr;

	need_buf(&byte_buf, &byte_buf_len, NBYTES(n));
	iptr = byte_buf;
	if (fmt_int(iptr, NBYTES(n), arg, n, flags) < 0) {
	    return 0;
	}
	need_buf(&erts_bin_buf, &erts_bin_buf_len, NBYTES(n + erts_bin_offset));
	copy_bits(iptr, 0, 1, erts_bin_buf, erts_bin_offset, 1, n);
    }
    erts_bin_offset += n;
    return 1;
}

int
erts_put_binary(Eterm arg, unsigned n)
{
    byte *bptr;

    if (!is_binary(arg)) {
	return 0;
    }
    GET_BINARY_BYTES(arg, bptr);
    if (n > 8*binary_size(arg)) {
	return 0;
    }
    need_buf(&erts_bin_buf, &erts_bin_buf_len, NBYTES(n + erts_bin_offset));
    if (BIT_OFFSET(erts_bin_offset) == 0) {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset), bptr, BYTE_OFFSET(n));
    } else {
	copy_bits(bptr, 0, 1, erts_bin_buf, erts_bin_offset, 1, n);
    }
    erts_bin_offset += n;
    return 1;
}

int
erts_put_binary_all(Eterm arg)
{
    byte *bptr;
    unsigned n;

    if (!is_binary(arg)) {
	return 0;
    }
    GET_BINARY_BYTES(arg, bptr);
    n = 8*binary_size(arg);
    need_buf(&erts_bin_buf, &erts_bin_buf_len, NBYTES(n + erts_bin_offset));
    if (BIT_OFFSET(erts_bin_offset) == 0) {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset), bptr, NBYTES(n));
    } else {
	copy_bits(bptr, 0, 1, erts_bin_buf, erts_bin_offset, 1, n);
    }
    erts_bin_offset += n;
    return 1;
}

int
erts_put_float(Eterm arg, Eterm size, int unit, int flags)
{
    int n;
    int sz;
    byte *bptr;
    float f32;

    if (is_not_float(arg)) {
	return 0;
    }
    if (!is_small(size)) {
	return 0;
    }
    sz = signed_val(size);
    if (sz < 0 || sz > 64)
	return 0;
    n = sz * unit;

    if (n == 64) {
	bptr = (byte *) (float_val(arg) + 1);
    } else if (n == 32) {
	FloatDef f;
	GET_DOUBLE(arg, f);
	f32 = f.fd;
	bptr = (byte *) &f32;
    } else {
	return 0;
    }

    need_buf(&erts_bin_buf, &erts_bin_buf_len, NBYTES(n + erts_bin_offset));

    if (BIT_OFFSET(erts_bin_offset) == 0) {
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    byte* t = erts_bin_buf+BYTE_OFFSET(erts_bin_offset);
	    t[0] = bptr[0];
	    t[1] = bptr[1];
	    t[2] = bptr[2];
	    t[3] = bptr[3];
	    if (n == 64) {
		t[4] = bptr[4];
		t[5] = bptr[5];
		t[6] = bptr[6];
		t[7] = bptr[7];
	    }
	} else {
	    byte* t = erts_bin_buf+BYTE_OFFSET(erts_bin_offset) + NBYTES(n);
	    t[-1] = bptr[0];
	    t[-2] = bptr[1];
	    t[-3] = bptr[2];
	    t[-4] = bptr[3];
	    if (n == 64) {
		t[-5] = bptr[4];
		t[-6] = bptr[5];
		t[-7] = bptr[6];
		t[-8] = bptr[7];
	    }
	}
    } else {
	if (BIT_IS_MACHINE_ENDIAN(flags)) {
	    copy_bits(bptr, 0, 1,
		      erts_bin_buf,
		      erts_bin_offset, 1, n);
	} else {
	    copy_bits(bptr+NBYTES(n)-1, 0, -1,
		      erts_bin_buf, erts_bin_offset, 1,
		      n);
	}
    }
    erts_bin_offset += n;
    return 1;
}

void erts_put_string(byte* iptr, size_t n)
{
    need_buf(&erts_bin_buf, &erts_bin_buf_len, n + NBYTES(erts_bin_offset));
    if (BIT_OFFSET(erts_bin_offset) != 0) {
	copy_bits(iptr, 0, 1, erts_bin_buf, erts_bin_offset, 1, n*8);
    } else {
	sys_memcpy(erts_bin_buf+BYTE_OFFSET(erts_bin_offset), iptr, n);
    }
    erts_bin_offset += n*8;
}
