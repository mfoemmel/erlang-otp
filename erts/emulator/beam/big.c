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
#include "big.h"
#include "error.h"
#include "bif.h"
/*
** compare two number vectors
** 32OK
*/
static int I_comp(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl)
{
    if (xl < yl)
	return -1;
    else if (xl > yl)
	return 1;
    else {
	if (x == y)
	    return 0;
	x += (xl-1);
	y += (yl-1);
	while(xl > 0 && (*x == *y)) {
	    x--;
	    y--;
	    xl--;
	}
	if (xl == 0)
	    return 0;
	return (*x < *y) ? -1 : 1;
    }
}

/*
** Add digits in x and y and store them in r
** assumption: (xl >= yl)
** 32OK
*/
static dsize_t I_add(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl, digit_t* r)
{
    dsize_t sz = xl;
    register digit_t yr, xr;
    register digit_t c = 0;

    ASSERT(xl >= yl);

    xl -= yl;
    do {
	xr = *x++ + c;
	yr = *y++;
	c = (xr < c);
	xr = yr + xr;
	c += (xr < yr);
	*r++ = xr;
    } while(--yl);

    while(xl--) {
	xr = *x++ + c;
	c = (xr < c);
	*r++ = xr;
    }
    if (c) {
	*r = 1;
	return sz+1;
    }
    return sz;
}
/*
** Add a digits in v1 and store result in vr
** 32OK
*/
static dsize_t D_add(digit_t* x, dsize_t xl, digit_t c, digit_t* r)
{
    dsize_t sz = xl;
    register digit_t xr;

    while(xl--) {
	xr = *x++ + c;
	c = (xr < c);
	*r++ = xr;
    }
    if (c) {
	*r = 1;
	return sz+1;
    }
    return sz;
}

/*
** Subtract digits v2 from v1 and store result in v3
** Assert  I_comp(x, xl, y, yl) >= 0
**
** 32OK
*/
static dsize_t I_sub(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl, digit_t* r)
{
    digit_t* r0 = r;
    register digit_t yr, xr;
    register digit_t c = 0;

    ASSERT(I_comp(x, xl, y, yl) >= 0);

    xl -= yl;
    do {
	yr = *y++ + c;
	xr = *x++;
	c = (yr < c);
	yr = xr - yr;
	c += (yr > xr);
	*r++ = yr;
    } while(--yl);

    while(xl--) {
	xr = *x++;
	yr = xr - c;
	c = (yr > xr);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);

    return (r - r0) + 1;
}

/*
** Subtract digit d from v1 and store result in vr
** 32OK
*/
static dsize_t D_sub(digit_t* x, dsize_t xl, digit_t c, digit_t* r)
{
    digit_t* r0 = r;
    register digit_t yr, xr;

    ASSERT(I_comp(x, xl, x, 1) >= 0);

    while(xl--) {
	xr = *x++;
	yr = xr - c;
	c = (yr > xr);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);

    return (r - r0) + 1;
}

/*
** subtract Z000...0 - y and store result in r, return new size
** 32OK
*/
static dsize_t Z_sub(digit_t* y, dsize_t yl, digit_t* r)
{
    digit_t* r0 = r;
    register digit_t yr;
    register digit_t c = 0;

    while(yl--) {
	yr = *y++ + c;
	c = (yr < c);
	yr = 0 - yr;
	c += (yr > 0);
	*r++ = yr;
    }
    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/*
** Multiply digits in x with digits in y and store in r
** Assumption: digits in r must be 0 (upto the size of x)
** 32UPDATE
*/
static dsize_t I_mul(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl, digit_t* r)
{
    digit_t* r0 = r;
    digit_t* rt = r;

    while(xl--) {
	digit_t cp = 0;
	digit_t c = 0;
	dsize_t n = yl;
	digit_t* yt = y;
	digit_t d;
	digit_t p;

	d = *x; 
	x++;
	rt = r;

	switch(d) {
	case 0:
	    rt = rt + n;
	    break;
	case 1:
	    while(n--) {
		DSUMc(*yt, *rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	case 2:
	    while(n--) {
		p = *yt;
		DSUMc(p, p, cp, p);
		DSUMc(p, *rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	default:
	    while(n--) {
		DMULc(d,*yt, cp, p);
		DSUMc(p,*rt, c, p);
		*rt++ = p;
		yt++;
	    }
	    break;
	}
	*rt = c + cp;
	r++;
    }
    if (*rt == 0)
	return (rt - r0);
    else
	return (rt - r0) + 1;
}

/*
** Square digits in x store in r (x & r may point into a common area)
** Assumption: x is destroyed if common area and digits in r are zero
**             to the size of xl+1
** 32UPDATE
*/

static dsize_t I_sqr(digit_t* x, dsize_t xl, digit_t* r)
{
    digit_t d_next = *x;
    digit_t d;
    digit_t* r0 = r;
    digit_t* s = r;

    if ((r + xl) == x)	/* "Inline" operation */
	*x = 0;
    x++;
	
    while(xl--) {
	digit_t* y = x;
	digit_t y_0 = 0, y_1 = 0, y_2 = 0, y_3 = 0;
	digit_t b0, b1;
	digit_t z0, z1, z2;
	digit_t t;
	dsize_t y_l = xl;
		
	s = r;
	d = d_next;
	d_next = *x; 
	x++;

	DMUL(d, d, b1, b0);
	DSUMc(*s, b0, y_3, t);
	*s++ = t;
	z1 = b1;
	while(y_l--) {
	    DMUL(d, *y, b1, b0);
	    y++;
	    DSUMc(b0, b0, y_0, z0);
	    DSUMc(z0, z1, y_2, z2);
	    DSUMc(*s, z2, y_3, t);
	    *s++ = t;
	    DSUMc(b1, b1, y_1, z1);
	}
	z0 = y_0;
	DSUMc(z0, z1, y_2, z2);
	DSUMc(*s, z2, y_3, t);
	*s = t;
	if (xl != 0) {
	    s++;
	    t = (y_1+y_2+y_3);
	    *s = t;
	    r += 2;
	}
	else {
	    ASSERT((y_1+y_2+y_3) == 0);
	}
    }
    if (*s == 0)
	return (s - r0);
    else
	return (s - r0) + 1;
}


/*
** Multiply digits d with digits in x and store in r
** 32UPDATE
*/
static dsize_t D_mul(digit_t* x, dsize_t xl, digit_t d, digit_t* r)
{
    digit_t c = 0;
    dsize_t rl = xl;
    digit_t p;

    switch(d) {
    case 0:
	ZERO_DIGITS(r, 1);
	return 1;
    case 1:
	if (x != r)
	    MOVE_DIGITS(r, x, xl);
	return xl;
    case 2:
	while(xl--) {
	    p = *x;
	    DSUMc(p, p, c, p);
	    *r++ = p;
	    x++;
	}
	break;
    default:
	while(xl--) {
	    DMULc(d, *x, c, p);
	    *r++ = p;
	    x++;
	}
	break;
    }
    if (c == 0)
	return rl;
    *r = c;
    return rl+1;
}

/*
** Multiply and subtract
** calculate r(i) = x(i) - d*y(i)
** assumption: xl = yl || xl == yl+1
**
** Return size of r
** 0 means borrow
** 32UPDATE
*/
static dsize_t D_mulsub(digit_t* x, dsize_t xl, digit_t d,
			digit_t* y, dsize_t yl, digit_t* r)
{
    digit_t c = 0;
    digit_t b = 0;
    digit_t c0;
    digit_t* r0 = r;
    digit_t s;

    ASSERT(xl == yl || xl == yl+1);

    xl -= yl;
    while(yl--) {
	DMULc(d, *y, c, c0);
	DSUBb(*x, c0, b, s);
	*r++ = s;
	x++;
	y++;
    }
    if (xl == 0) {
	if (c != 0 || b != 0)
	    return 0;
    }
    else {			/* xl == 1 */
	DSUBb(*x, c, b, s);
	*r++ = s;
    }
    if (b != 0) return 0;

    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/*
** Divide digits in x with a digit,
** quotient is returned in q and remainder digit in r
** x and q may be equal
** 32UPDATE
*/
static dsize_t D_div(digit_t* x, dsize_t xl, digit_t d, digit_t* q, digit_t* r)
{
    digit_t* xp = x + (xl-1);
    digit_t* qp = q + (xl-1);
    dsize_t qsz = xl;
    digit_t a1;
	
    a1 = *xp; 
    xp--;

    if (d > a1) {
	if (xl == 1) {
	    *r = a1;
	    *qp = 0;
	    return 1;
	}
	qsz--;
	qp--;
    }

    do {
	digit_t q0, a0, b1, b0, b;

	if (d > a1) {
	    a0 = *xp; 
	    xp--;
	}
	else {
	    a0 = a1; a1 = 0;
	}
	DDIV(a1, a0, d, q0);
	DMUL(d, q0, b1, b0);
	DSUB(a0,b0, b, a1);
	*qp = q0;
	qp--;
    } while (xp >= x);

    *r = a1;
    return qsz;
}

/*
** Divide digits in x with digits in y and return qutient in q
** and remainder in r
** assume that integer(x) > integer(y)
** Return remainder in x (length int rl)
** Return quotient size
** 32UPDATE
*/

static dsize_t I_div(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl,
		     digit_t* q, digit_t* r, dsize_t* rlp)
{
    digit_t* rp;
    digit_t* qp;
    digit_t b1 = y[yl-1];
    digit_t b2 = y[yl-2];
    digit_t a1;
    digit_t a2;
    int r_signed = 0;
    dsize_t ql;
    dsize_t rl;

    if (x != r)
	MOVE_DIGITS(r, x, xl);
    rp = r + (xl-yl);
    rl = xl;
	
    ZERO_DIGITS(q, xl-yl+1);
    qp = q + (xl-yl);
    ql = 0;
	
    /* Adjust length */
    a1 = rp[yl-1];
    a2 = rp[yl-2];
    if (b1 < a1 || (b1 == a1 && b2 <= a2))
	ql = 1;

    do {
	digit_t q0;
	dsize_t nsz = yl;
	dsize_t nnsz;

	a1 = rp[yl-1];
	a2 = rp[yl-2];

	if (b1 < a1)
	    DDIV2(a1,a2,b1,b2,q0);
	else if (b1 > a1) {
	    DDIV(a1,a2,b1,q0);
	    nsz++;
	    rp--;
	    qp--;
	    ql++;
	}
	else {			/* (b1 == a1) */
	    if (b2 <= a2)
		q0 = 1;
	    else {
		q0 = D_BASE-1;
		nsz++;
		rp--;
		qp--;
		ql++;
	    }
	}

	if (r_signed)
	    ql = D_sub(qp, ql, q0, qp);
	else
	    ql = D_add(qp, ql, q0, qp);

	if ((nnsz = D_mulsub(rp, nsz, q0, y, yl, rp)) == 0) {
	    nnsz = Z_sub(r, rl, r);
	    if (nsz > (rl-nnsz))
		nnsz = nsz - (rl-nnsz);
	    else
		nnsz = 1;
	    r_signed = !r_signed;
	}
		
	if ((nnsz == 1) && (*rp == 0))
	    nnsz = 0;
	rp = rp - (yl-nnsz);
	rl -= (nsz-nnsz);
	qp = qp - (yl-nnsz);
	ql += (yl-nnsz);
    } while (I_comp(r, rl, y, yl) >= 0);

    ql -= (q - qp);
    qp = q;

    if (rl == 0)
	rl = 1;

    if (r_signed && (rl > 1 || *r != 0)) {
	rl = I_sub(y, yl, r, rl, r);
	ql = D_sub(qp, ql, 1, qp);
    }

    *rlp = rl;
    return ql;
}

/*
** Remainder of digits in x and a digit d
** 32UPDATE
*/
static digit_t D_rem(digit_t* x, dsize_t xl, digit_t d)
{
    digit_t rem = 0;

    x += (xl-1);
    do {
	if (rem != 0)
	    DREM(rem, *x, d, rem);
	else
	    DREM(0, *x, d, rem);
	x--;
	xl--;
    } while(xl > 0);
    return rem;
}

/*
** Remainder of x and y
**
** Assumtions: xl >= yl, yl > 1
**			   r must contain at least xl number of digits
** 32UPDATE
*/
static dsize_t I_rem(digit_t* x, dsize_t xl, digit_t* y, dsize_t yl, digit_t* r)
{
    digit_t* rp;
    digit_t b1 = y[yl-1];
    digit_t b2 = y[yl-2];
    digit_t a1;
    digit_t a2;
    int r_signed = 0;
    dsize_t rl;
	
    if (x != r)
	MOVE_DIGITS(r, x, xl);
    rp = r + (xl-yl);
    rl = xl;

    do {
	digit_t q0;
	dsize_t nsz = yl;
	dsize_t nnsz;
		
	a1 = rp[yl-1];
	a2 = rp[yl-2];

	if (b1 < a1)
	    DDIV2(a1,a2,b1,b2,q0);
	else if (b1 > a1) {
	    DDIV(a1,a2,b1,q0);
	    nsz++;
	    rp--;
	}
	else {			/* (b1 == a1) */
	    if (b2 <= a2)
		q0 = 1;
	    else {
		q0 = D_BASE-1;
		nsz++;
		rp--;
	    }
	}

	if ((nnsz = D_mulsub(rp, nsz, q0, y, yl, rp)) == 0) {
	    nnsz = Z_sub(r, rl, r);
	    if (nsz > (rl-nnsz))
		nnsz = nsz - (rl-nnsz);
	    else
		nnsz = 1;
	    r_signed = !r_signed;
	}

	if (nnsz == 1 && *rp == 0)
	    nnsz = 0;

	rp = rp - (yl-nnsz);
	rl -= (nsz-nnsz);
    } while (I_comp(r, rl, y, yl) >= 0);

    if (rl == 0)
	rl = 1;

    if (r_signed && (rl > 1 || *r != 0))
	rl = I_sub(y, yl, r, rl, r);
    return rl;
}

/*
** Remove trailing digits from bitwise operations
** 32UPDATE
*/
static dsize_t I_btrail(digit_t* r0, digit_t* r, short sign)
{
    /* convert negative numbers to one complement */
    if (sign) {
	dsize_t rl;
	digit_t d;

	/* 1 remove all 0xffff words */
	do {
	    r--;
	} while((d = *r) == D_BASE-1 && r != r0);

	/* 2 complement high digit */
	if (d == D_BASE-1)
	    *r = 0;
	else {
	    digit_t prev_mask = 0;
	    digit_t mask = D_BASE >> 1;

	    while((d & mask) == mask) {
		prev_mask = mask;
		mask = (prev_mask >> 1) | (D_BASE>>1);
	    }
	    *r = ~d & ~prev_mask;
	}
	rl = (r - r0) + 1;
	while(r != r0) {
	    r--;
	    *r = ~*r;
	}
	return D_add(r0, rl, 1, r0);
    }

    do {
	r--;
    } while(*r == 0 && r != r0);
    return (r - r0) + 1;
}

/* 
** Bitwise and
** 32UPDATE
*/
static dsize_t I_band(digit_t* x, dsize_t xl, short xsgn,
		      digit_t* y, dsize_t yl, short ysgn, digit_t* r)
{
    digit_t* r0 = r;
    short sign = xsgn && ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ & *y++;
	}
	else {
	    digit_t b;
	    digit_t c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ & ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ & ~c;
		y++;
	    }
	    while (xl--) {
		*r++ = *x++;
	    }
	}
    }
    else {
	if (!ysgn) {
	    digit_t b;
	    digit_t c;

	    DSUB(*x,1,b,c);
	    *r = ~c & *y;
	    x++; y++; r++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c & *y++;
		x++;
	    }
	}
	else {
	    digit_t b1, b2;
	    digit_t c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 & ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 & ~c2;
		x++; y++;
	    }
	    while(xl--)
		*r++ = ~*x++;
	}
    }
    return I_btrail(r0, r, sign);
}

/* 
 * Bitwise 'or'.
 */
static dsize_t
I_bor(digit_t* x, dsize_t xl, short xsgn, digit_t* y,
      dsize_t yl, short ysgn, digit_t* r)
{
    digit_t* r0 = r;
    short sign = xsgn || ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ | *y++;
	    while(xl--)
		*r++ = *x++;
	}
	else {
	    digit_t b;
	    digit_t c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ | ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ | ~c;
		y++;
	    }
	}
    }
    else {
	if (!ysgn) {
	    digit_t b;
	    digit_t c;

	    DSUB(*x,1,b,c);
	    *r++ = ~c | *y++;
	    x++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c | *y++;
		x++;
	    }
	    while(xl--) {
		DSUBb(*x,0,b,c);
 		*r++ = ~c;
 		x++;
	    }
	}
	else {
	    digit_t b1, b2;
	    digit_t c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 | ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 | ~c2;
		x++; y++;
	    }
	}
    }
    return I_btrail(r0, r, sign);
}

/* 
** Bitwise xor
*/
static dsize_t I_bxor(digit_t* x, dsize_t xl, short xsgn,
		      digit_t* y, dsize_t yl, short ysgn, digit_t* r)
{
    digit_t* r0 = r;
    short sign = xsgn != ysgn;

    ASSERT(xl >= yl);

    xl -= yl;

    if (!xsgn) {
	if (!ysgn) {
	    while(yl--)
		*r++ = *x++ ^ *y++;
	    while(xl--)
		*r++ = *x++;
	}
	else {
	    digit_t b;
	    digit_t c;

	    DSUB(*y,1,b,c);
	    *r++ = *x++ ^ ~c;
	    y++;
	    yl--;
	    while(yl--) {
		DSUBb(*y,0,b,c);
		*r++ = *x++ ^ ~c;
		y++;
	    }
	    while(xl--)
		*r++ = ~*x++;
	}
    }
    else {
	if (!ysgn) {
	    digit_t b;
	    digit_t c;

	    DSUB(*x,1,b,c);
	    *r++ = ~c ^ *y++;
	    x++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b,c);
		*r++ = ~c ^ *y++;
		x++;
	    }
	    while(xl--)
		*r++ = ~*x++;
	}
	else {
	    digit_t b1, b2;
	    digit_t c1, c2;

	    DSUB(*x,1,b1,c1);
	    DSUB(*y,1,b2,c2);
	    *r++ = ~c1 ^ ~c2;
	    x++; y++;
	    yl--;
	    while(yl--) {
		DSUBb(*x,0,b1,c1);
		DSUBb(*y,0,b2,c2);
		*r++ = ~c1 ^ ~c2;
		x++; y++;
	    }
	    while(xl--) {
		*r++ = *x++;
	    }
	}
    }
    return I_btrail(r0, r, sign);
}

/*
** Bitwise not  simulated as
** bnot -X  == (X - 1)
** bnot +X  == -(X + 1)
** 32OK
*/
static dsize_t I_bnot(digit_t* x, dsize_t xl, short xsgn, digit_t* r)
{
    if (xsgn)
	return D_add(x, xl, 1, r);
    else
	return D_sub(x, xl, 1, r);
}

/*
** Arithmetic left shift or right
** 32UPDATE
*/
static dsize_t I_lshift(digit_t* x, dsize_t xl, Sint y, 
			short sign, digit_t* r)
{
    if (y == 0) {
	MOVE_DIGITS(r, x, xl);
	return xl;
    }
    else if (xl == 1 && *x == 0) {
	*r = 0;
	return 1;
    }
    else {
	long ay = (y < 0) ? -y : y;
	int bw = ay / D_EXP;
	int sw = ay % D_EXP;
	dsize_t rl;
	reg_t a = 0;

	if (y > 0) {		/* shift left */
	    rl = xl + bw + 1;

	    while(bw--)
		*r++ = 0;
	    while(xl--) {
		a = DHIGH(a) | ((reg_t) *x << sw);
		*r++ = DLOW(a);
		x++;
	    }
	    if (DHIGH(a) == 0)
		return rl-1;
	    *r = DHIGH(a);
	    return rl;
	}
	else {			/* shift right */
	    digit_t* r0 = r;
	    int rw = D_EXP - sw;
	    int add_one = 0;

	    if (xl <= bw) {
		if (sign)
		    *r = 1;
		else
		    *r = 0;
		return 1;
	    }

	    if (sign) {
		int zl = bw;
		digit_t* z = x;

		while(zl--) {
		    if (*z != 0) {
			add_one = 1;
			break;
		    }
		    z++;
		}
	    }

	    rl = xl - bw;
	    x += (xl-1);
	    r += (rl-1);
	    xl -= bw;
	    while(xl--) {
		a = DLOW2HIGH(a) | ((reg_t) *x << rw);
		*r-- = DHIGH(a);
		x--;
	    }

	    if (sign && DLOW(a) != 0)
		add_one = 1;

	    if (r[rl] == 0) {
		if (rl == 1) {
		    if (sign)
			r[1] = 1;
		    return 1;
		}
		rl--;
	    }
	    if (add_one)
		return D_add(r0, rl, 1, r0);
	    return rl;
	}
    }
}

/*
** Return log(x)/log(2)
** 32OK
*/
static int I_lg(digit_t* x, dsize_t xl)
{
    dsize_t sz = xl - 1;
    digit_t d = x[sz];

    sz *= D_EXP;
    while(d != 0) {
	d >>= 1;
	sz++;
    }
    return sz - 1;
}

/*
** Create bigint on heap if necessary. Like the previously existing
** make_small_or_big(), except for a HAlloc() instead of an
** ArithAlloc().
** NOTE: Only use erts_make_integer(), when order of heap fragments is
**       guaranteed to be correct.
*/
Eterm
erts_make_integer(Uint x, Process *p)
{
    Eterm* hp;
    if (IS_USMALL(0,x))
	return make_small(x);
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	return uint_to_big(x,hp);
    }
}

/*
** convert uint32 to bigint
** 32UPDATE (as macro?)
** (must only be used if x is to big to be stored as a small)
*/
Eterm uint_to_big(Uint x, Eterm *y)
{
    *y = make_pos_bignum_header(1);
    BIG_DIGIT(y, 0) = DLOW(x);
    BIG_DIGIT(y, 1) = DHIGH(x);
    return make_big(y);
}


/*
** convert signed int to bigint
** 32UPDATE (as macro?)
*/
Eterm small_to_big(Sint x, Eterm *y)
{
    if (x >= 0) {
	*y = make_pos_bignum_header(1);
    } else {
	x = -x;
	*y = make_neg_bignum_header(1);
    }
    BIG_DIGIT(y, 0) = DLOW(x);
    BIG_DIGIT(y, 1) = DHIGH(x);
    return make_big(y);
}

/*
** Convert a bignum to a double float
** 32OK XXX must check
*/
int
big_to_double(Eterm x, double* resp)
{
    double d = 0.0;
    Eterm* xp = big_val(x);
    dsize_t xl = BIG_SIZE(xp);
    digit_t* s = BIG_V(xp) + xl;
    short xsgn = BIG_SIGN(xp);
    volatile int *fpexnp = erts_get_current_fp_exception();
    __ERTS_SAVE_FP_EXCEPTION(fpexnp);

    __ERTS_FP_CHECK_INIT(fpexnp);
    while (xl--) {
	d = d * D_BASE + *--s;

	__ERTS_FP_ERROR(fpexnp, d, __ERTS_RESTORE_FP_EXCEPTION(fpexnp); return -1);
    }

    *resp = xsgn ? -d : d;
    __ERTS_FP_ERROR(fpexnp,*resp,;);
    __ERTS_RESTORE_FP_EXCEPTION(fpexnp);
    return 0;
}


/*
 ** Estimate the number of decimal digits (include sign)
 */
int big_decimal_estimate(Eterm x)
{
    Eterm* xp = big_val(x);
    int lg = I_lg(BIG_V(xp), BIG_SIZE(xp));
    int lg10 = ((lg+1)*28/93)+1;

    if (BIG_SIGN(xp)) lg10++;	/* add sign */
    return lg10+1;		/* add null */
}

/*
** Convert a bignum into a string of decimal numbers
*/

Eterm big_to_list(Eterm x, Eterm **hp)
{
    Eterm* xp = big_val(x);
    digit_t* dx = BIG_V(xp);
    dsize_t xl = BIG_SIZE(xp);
    short sign = BIG_SIGN(xp);
    digit_t rem;
    Eterm prev = NIL;
    Eterm* curr = *hp;

    if (xl == 1 && *dx < D_DECIMAL_BASE) {
	rem = *dx;
	if (rem == 0) {
	    prev = CONS(curr, make_small('0'), prev);
	    curr += 2;
	}
	else {
	    while(rem) {
		prev = CONS(curr, make_small((rem % 10)+'0'), prev);
		curr += 2;
		rem /= 10;
	    }
	}
    }
    else {
	digit_t* tmp  = (digit_t*) erts_alloc(ERTS_ALC_T_TMP,
					      sizeof(digit_t)*xl);
	dsize_t tmpl = xl;

	MOVE_DIGITS(tmp, dx, xl);

	while(1) {
	    tmpl = D_div(tmp, tmpl, D_DECIMAL_BASE, tmp, &rem);
	    if (tmpl == 1 && *tmp == 0) {
		while(rem) {
		    prev = CONS(curr, make_small((rem % 10)+'0'), prev);
		    curr += 2;
		    rem /= 10;
		}
		break;
	    }
	    else {
		int i = D_DECIMAL_EXP;
		while(i--) {
		    prev = CONS(curr, make_small((rem % 10)+'0'), prev);
		    curr += 2;
		    rem /= 10;
		}
	    }
	}
	erts_free(ERTS_ALC_T_TMP, (void *) tmp);
    }

    if (sign) {
	prev = CONS(curr, make_small('-'), prev);
	curr += 2;
    }
    *hp = curr;
    return prev;
}

/*
** Normalize a bignum given thing pointer length in digits and a sign
** patch zero if odd length
** 32UPDATE  in 32 bit version no patch is needed
*/
static Eterm big_norm(Eterm *x, dsize_t xl, short sign)
{
    Uint arity;

    if (xl == 1) {
	Uint y = BIG_DIGIT(x, 0);

	if (D_EXP < SMALL_BITS || IS_USMALL(sign, y)) {
	    if (sign)
		return make_small(-((Sint)y));
	    else
		return make_small(y);
	}
    }
    else if (xl == 2) {
	Uint y = BIG_DIGIT(x,0) + BIG_DIGIT(x,1)*D_BASE;

	if (IS_USMALL(sign, y)) {
	    if (sign)
		return make_small(-((Sint)y));
	    else
		return make_small(y);
	}
    }

    /* __alpha__: This was fixed */
    if ((arity = BIG_NEED_SIZE(xl)-1) > BIG_ARITY_MAX)
      return NIL;  /* signal error (too big) */

    if (sign) {
      *x = make_neg_bignum_header(arity);
    }
    else {
      *x = make_pos_bignum_header(arity);
    }

    /* Its VERY important to patch a zero if odd number of digits! */
    switch(xl & 1) {
    case 0:
      break;
    case 1:
      BIG_DIGIT(x, xl) = 0;
      break;
    }

    return make_big(x);
}

/*
** Compare bignums
** 32OK
*/
int big_comp(Eterm x, Eterm y)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    if (BIG_SIGN(xp) == BIG_SIGN(yp)) {
	int c = I_comp(BIG_V(xp), BIG_SIZE(xp), BIG_V(yp), BIG_SIZE(yp));
	if (BIG_SIGN(xp))
	    return -c;
	else
	    return c;
    }
    else
	return BIG_SIGN(xp) ? -1 : 1;
}

/*
** Unsigned compare
** 32OK
*/
int big_ucomp(Eterm x, Eterm y)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return I_comp(BIG_V(xp), BIG_SIZE(xp), BIG_V(yp), BIG_SIZE(yp));
}

/*
** Check if bytes in xp corresponds to a bignum y
** 32UPDATE
*/
#if 0	/* XXX: unused */
int bytes_eq_big(byte *xp, dsize_t xsz, int xsgn, Eterm y)
{
    if (is_big(y)) {
	Eterm* yp = big_val(y);

	dsize_t ysz = big_bytes(y); /* ysz in bytes */
	short sgny = BIG_SIGN(yp);
	digit_t* ywp = BIG_V(yp);
	digit_t d;

	if (sgny != xsgn) return 0;
	if (xsz != ysz) return 0;
	while (xsz >= 2) {
	    d = xp[0] | (xp[1] << 8);
	    if (d != *ywp)
		return 0;
	    ywp++;
	    xp += 2;
	    xsz -= 2;
	}
	if (xsz == 1)
	{
	    d = *xp;
	    if (d != *ywp) return 0;
	}
	return 1;
    }
    else if (is_small(y)) {
	Sint yv = signed_val(y);
	Uint xv;

	if (xsz > 4) return 0;
	if ((yv < 0) != xsgn) return 0;

	switch(xsz) {
	case 1:
	    xv = *xp;
	    break;
	case 2:
	    xv = xp[0] | (xp[1] << 8);
	    break;
	case 3:
	    xv = xp[0] | (xp[1] << 8) | (xp[2] << 16);
	    break;
	case 4:
	    xv = xp[0] | (xp[1] << 8) | (xp[2] << 16) | (xp[3] << 24);
	    break;
	default:		/* Silence compiler warning. */
	    xv = 0;
	    break;
	}
	if (!IS_USMALL(xsgn, xv)) return 0;
	if (yv < 0)
	    return (-yv == xv);
	else
	    return (yv == xv);
    }
    return 0;
}
#endif	/* 0 */

/*
** Return number of bytes in the bignum
** 32UPDATE
*/
dsize_t big_bytes(Eterm x)
{
    Eterm* xp = big_val(x);

    dsize_t sz = BIG_SIZE(xp);
    digit_t d = BIG_DIGIT(xp, sz-1);

    sz = (sz-1) * sizeof(digit_t);
    while (d != 0) {
	++sz;
	d >>= 8;
    }
    return sz;
}

/*
** Load a bignum from bytes
** xsz is the number of bytes in xp
** 32UPDATE
*/
Eterm bytes_to_big(byte *xp, dsize_t xsz, int xsgn, Eterm *r)
{
    digit_t* rwp = BIG_V(r);
    dsize_t rsz = 0;
    digit_t d;
    int i;

    while(xsz >= sizeof(digit_t)) {
	d = 0;
	for(i = sizeof(digit_t); --i >= 0;)
	    d = (d << 8) | xp[i];
	*rwp = d;
	rwp++;
	xsz -= sizeof(digit_t);
	xp += sizeof(digit_t);
	rsz++;
    }

    if (xsz > 0) {
	d = 0;
	for(i = xsz; --i >= 0;)
	    d = (d << 8) | xp[i];
	*rwp = d;
	rwp++;
	rsz++;
    }
    return big_norm(r, rsz, (short) xsgn);
}

/*
** Store digits in the array of bytes pointed to by p
** 32UPDATE
*/
byte* big_to_bytes(Eterm x, byte *p)
{
    digit_t* xr = big_v(x);
    dsize_t  xl = big_size(x);
    digit_t d;
    int i;

    while(xl > 1) {
	d = *xr;
	xr++;
	for(i = 0; i < sizeof(digit_t); ++i) {
	    p[i] = d & 0xff;
	    d >>= 8;
	}
	p += sizeof(digit_t);
	xl--;
    }
    d = *xr;
    do {
	*p++ = d & 0xff;
	d >>= 8;
    } while (d != 0);
    return p;
}

/*
 * Converts a positive term (small or bignum) to an Uint.
 *
 * Fails returning 0 if the term is neither a small nor a bignum,
 * if it's negative, or the big number does not fit in an Uint.
 * Otherwise returns a non-zero value.
 */

int
term_to_Uint(Eterm term, Uint *up)
{
    if (is_small(term)) {
	Sint i = signed_val(term);
	if (i < 0) {
	    return 0;
	}
	*up = (Uint) i;
	return 1;
    } else if (is_big(term)) {
	digit_t* xr = big_v(term);
	dsize_t  xl = big_size(term);
	Uint uval = 0;
	int n = 0;
	
	if (big_sign(term) || xl*D_EXP > sizeof(Uint)*CHAR_BIT) {
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((Uint)(*xr++)) << n;
	    n += D_EXP;
	}
	*up = uval;
	return 1;
    } else {
	return 0;
    }
}

int term_to_Sint(Eterm term, Sint *sp)
{
    if (is_small(term)) {
	*sp = signed_val(term);
	return 1;
    } else if (is_big(term)) {
	digit_t* xr = big_v(term);
	dsize_t xl = big_size(term);
	int sign = big_sign(term);
	Uint uval = 0;
	int n = 0;

	if (xl*D_EXP > sizeof(Uint)*CHAR_BIT) {
	    return 0;
	}
	while (xl-- > 0) {
	    uval |= ((Uint)(*xr++)) << n;
	    n += D_EXP;
	}
	if (sign) {
	    uval = -uval;
	    if ((Sint)uval > 0)
		return 0;
	} else {
	    if ((Sint)uval < 0)
		return 0;
	}
	*sp = uval;
	return 1;
    } else {
	return 0;
    }
}

/*
** Add and subtract
** 32OK
*/
static Eterm B_plus_minus(digit_t *x, dsize_t xl, short xsgn, 
			  digit_t *y, dsize_t yl, short ysgn, Eterm *r)
{
    if (xsgn == ysgn) {
	if (xl > yl)
	    return big_norm(r, I_add(x,xl,y,yl,BIG_V(r)), xsgn);
	else
	    return big_norm(r, I_add(y,yl,x,xl,BIG_V(r)), xsgn);
    }
    else {
	int comp = I_comp(x, xl, y, yl);
	if (comp == 0)
	    return make_small(0);
	else if (comp > 0)
	    return big_norm(r, I_sub(x,xl,y,yl,BIG_V(r)), xsgn);
	else
	    return big_norm(r, I_sub(y,yl,x,xl,BIG_V(r)), ysgn);
    }
}

/*
** Add bignums
** 32OK
*/
Eterm big_plus(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),(short) BIG_SIGN(yp), r);
}

/*
** Subtract bignums
** 32OK
*/

Eterm big_minus(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),(short) BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),(short) !BIG_SIGN(yp), r);
}

/*
** Subtract a digit from big number
*/
Eterm big_minus_small(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp), (digit_t) y, BIG_V(r)), 
			(short) BIG_SIGN(xp));
    else
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp), (digit_t) y, BIG_V(r)), 
			(short) BIG_SIGN(xp));
}

/*
** Multiply bignums
** 32OK
*/

Eterm big_times(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t rsz;

    if (ysz == 1)
	rsz = D_mul(BIG_V(xp), xsz, BIG_DIGIT(yp, 0), BIG_V(r));
    else if (xsz == 1)
	rsz = D_mul(BIG_V(yp), ysz, BIG_DIGIT(xp, 0), BIG_V(r));
    else if (xp == yp) {
	ZERO_DIGITS(BIG_V(r), xsz+1);
	rsz = I_sqr(BIG_V(xp), xsz, BIG_V(r));
    }
    else if (xsz >= ysz) {
	ZERO_DIGITS(BIG_V(r), xsz);
	rsz = I_mul(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
    }
    else {
	ZERO_DIGITS(BIG_V(r), ysz);
	rsz = I_mul(BIG_V(yp), ysz, BIG_V(xp), xsz, BIG_V(r));
    }
    return big_norm(r, rsz, sign);
}


/* 
** Divide bignums
** 32UPDATE?
*/

Eterm big_div(Eterm x, Eterm y, Eterm *q)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t qsz;

    if (ysz == 1) {
	digit_t rem;
	qsz = D_div(BIG_V(xp), xsz, BIG_DIGIT(yp,0), BIG_V(q), &rem);
    }
    else {
	Eterm* remp;
	dsize_t rem_sz;

	qsz = xsz - ysz + 1;
	remp = q + BIG_NEED_SIZE(qsz);
	qsz = I_div(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(q), BIG_V(remp),
		    &rem_sz);
    }
    return big_norm(q, qsz, sign);
}

/*
** Remainder
** 32UPDATE?
*/
Eterm big_rem(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (ysz == 1) {
	digit_t rem;
	rem = D_rem(BIG_V(xp), xsz, BIG_DIGIT(yp,0));
	if (sign)
	    return make_small(-(Sint)rem);
	else
	    return make_small(rem);
    }
    else {
	dsize_t rsz = I_rem(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
	return big_norm(r, rsz, sign);
    }
}

Eterm big_neg(Eterm x, Eterm *r)
{
    Eterm* xp = big_val(x);
    dsize_t xsz = BIG_SIZE(xp);
    short xsgn = BIG_SIGN(xp);
    
    MOVE_DIGITS(BIG_V(r), BIG_V(xp), xsz);
    return big_norm(r, xsz, (short) !xsgn);
}

Eterm big_band(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);

    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = xsgn && ysgn;
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_band(BIG_V(xp),xsz,xsgn,
				 BIG_V(yp),ysz,ysgn,
				 BIG_V(r)),sign);
    else
	return big_norm(r,I_band(BIG_V(yp),ysz,ysgn,
				 BIG_V(xp),xsz,xsgn,
				 BIG_V(r)),sign);
}


Eterm big_bor(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = (xsgn || ysgn);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_bor(BIG_V(xp),xsz,xsgn,
				BIG_V(yp),ysz,ysgn,
				BIG_V(r)),sign);
    else
	return big_norm(r,I_bor(BIG_V(yp),ysz,ysgn,
				BIG_V(xp),xsz,xsgn,
				BIG_V(r)),sign);
}


Eterm big_bxor(Eterm x, Eterm y, Eterm *r)
{
    Eterm* xp = big_val(x);
    Eterm* yp = big_val(y);
    short xsgn = BIG_SIGN(xp);
    short ysgn = BIG_SIGN(yp);
    short sign = (xsgn != ysgn);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (xsz >= ysz)
	return big_norm(r,I_bxor(BIG_V(xp),xsz,xsgn,
				 BIG_V(yp),ysz,ysgn,
				 BIG_V(r)),sign);
    else
	return big_norm(r,I_bxor(BIG_V(yp),ysz,ysgn,
				 BIG_V(xp),xsz,xsgn,
				 BIG_V(r)),sign);
}

Eterm big_bnot(Eterm x,  Eterm *r)
{
    Eterm* xp = big_val(x);
    short sign = !BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_bnot(BIG_V(xp), xsz, sign, BIG_V(r)), sign);
}

Eterm big_lshift(Eterm x, Sint y, Eterm *r)
{
    Eterm* xp = big_val(x);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_lshift(BIG_V(xp), xsz, y, sign, BIG_V(r)), sign);
}


/* add unsigned small int y to x */

Eterm big_plus_small(Eterm x, Uint y, Eterm *r)
{
    Eterm* xp = big_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp), (digit_t) y, 
				 BIG_V(r)), (short) BIG_SIGN(xp));
    else
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp), (digit_t) y, 
				 BIG_V(r)), (short) BIG_SIGN(xp));
}

Eterm big_times_small(Eterm x, Uint y, Eterm *r)
{
    Eterm* xp = big_val(x);

    return big_norm(r, D_mul(BIG_V(xp),BIG_SIZE(xp), (digit_t) y, 
			     BIG_V(r)), (short) BIG_SIGN(xp));
}

/*
** Expects the big to fit.
*/
Uint32 big_to_uint32(Eterm b)
{
    Uint u;
    (void) term_to_Uint(b, &u);
    return u;
}

/*
 * Check if a fixnum or bignum equals 2^32.
 */
int term_equals_2pow32(Eterm x)
{
    if (sizeof(Uint) > 4) {
	Uint u;
	if (!term_to_Uint(x, &u))
	    return 0;
	return (u & 0xFFFFFFFF) == 0 && ((u >> 16) >> 16) == 1;
    } else {
	Eterm *bp;
	if (!is_big(x))
	    return 0;
	bp = big_val(x);
	if (BIG_SIZE(bp) == 3 && !BIG_DIGIT(bp,0) && !BIG_DIGIT(bp,1) &&
	    BIG_DIGIT(bp,2) == 1)
	    return 1;
	return 0;
    }
}
