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

/*
** compare two number vectors
** 32OK
*/
static int I_comp(x, xl, y, yl)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl;
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
static dsize_t I_add(x, xl, y, yl, r)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl; digit_t* r;
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
static dsize_t D_add(x, xl, c, r)
digit_t* x; dsize_t xl; digit_t c; digit_t* r;
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
static dsize_t I_sub(x, xl, y, yl, r)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl; digit_t* r;
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
static dsize_t D_sub(x, xl, c, r)
digit_t* x; dsize_t xl; digit_t c; digit_t* r;
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
static dsize_t Z_sub(y, yl, r)
digit_t* y; dsize_t yl; digit_t* r;
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
static dsize_t I_mul(x, xl, y, yl, r)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl; digit_t* r;
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

static dsize_t I_sqr(x, xl, r)
    digit_t* x; dsize_t xl; digit_t* r;
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
static dsize_t D_mul(x, xl, d, r)
digit_t* x; dsize_t xl; digit_t d; digit_t* r;
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
static dsize_t D_mulsub(x, xl, d, y, yl, r)
digit_t* x; dsize_t xl; digit_t d; digit_t* y; dsize_t yl; digit_t* r;
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
static dsize_t D_div(x, xl, d, q, r)
    digit_t* x; dsize_t xl; digit_t d; digit_t* q; digit_t* r;
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

static dsize_t I_div(x, xl, y, yl, q, r, rlp)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl; digit_t* q; 
digit_t* r; dsize_t* rlp;
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
static digit_t D_rem(x, xl, d)
digit_t* x; dsize_t xl; digit_t d;
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
static dsize_t I_rem(x, xl, y, yl, r)
digit_t* x; dsize_t xl; digit_t* y; dsize_t yl; digit_t* r;
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
static dsize_t I_btrail(r0, r, sign)
    digit_t* r0; digit_t* r; short sign;
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
static dsize_t I_band(x, xl, xsgn, y, yl, ysgn, r)
digit_t* x; dsize_t xl; short xsgn; 
digit_t* y; dsize_t yl; short ysgn; digit_t* r;
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
	    while(xl--)
		*r++ = *x++;
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
** Bitwise or
*/
static dsize_t I_bor(x, xl, xsgn, y, yl, ysgn, r)
digit_t* x; dsize_t xl; short xsgn; 
digit_t* y; dsize_t yl; short ysgn; digit_t* r;
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
	    while(xl--)
		*r++ = ~*x++;
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
static dsize_t I_bxor(x, xl, xsgn, y, yl, ysgn, r)
digit_t* x; dsize_t xl; short xsgn; 
digit_t* y; dsize_t yl; short ysgn; digit_t* r;
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
static dsize_t I_bnot(x, xl, xsgn, r)
digit_t* x; dsize_t xl; short xsgn; digit_t* r;
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
static dsize_t I_lshift(x, xl, y, sign, r)
digit_t* x; dsize_t xl; sint32 y; short sign; digit_t* r;
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
static int I_lg(x, xl)
digit_t* x; dsize_t xl;
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
** create bigint on heap if necessary
** or a small_int which uses no heap
** allocates new heap if necssary
*/
uint32 make_small_or_big(x,p)
uint32 x; Process* p;
{
    uint32* hp;
    if (IS_USMALL(0,x))
	return make_small(x);
    else {
	/* Called from guard BIFs - must use ArithAlloc() here */
	hp = ArithAlloc(p,BIG_NEED_SIZE(2));
	return uint32_to_big(x,hp);
    }
}
/*
** convert uint32 to bigint
** 32UPDATE (as macro?)
** (must only be used if x is to big to be stored as a small)
*/
uint32 uint32_to_big(x, y)
uint32 x; uint32* y;
{
    *y = make_thing(1, POSITIVE_BIG_SUBTAG);
    BIG_DIGIT(y, 0) = DLOW(x);
    BIG_DIGIT(y, 1) = DHIGH(x);
    return make_big(y);
}


/*
** convert signed int to bigint
** 32UPDATE (as macro?)
*/
uint32 small_to_big(x, y)
sint32 x; uint32* y;
{
    if (x >= 0) {
	*y = make_thing(1, POSITIVE_BIG_SUBTAG);
    } else {
	x = -x;
	*y = make_thing(1, NEGATIVE_BIG_SUBTAG);
    }
    BIG_DIGIT(y, 0) = DLOW(x);
    BIG_DIGIT(y, 1) = DHIGH(x);
    return make_big(y);
}

/*
** Convert a bignum to a double float
** 32OK XXX must check
*/
double big_to_double(x)
    uint32 x;
{
    double d = 0.0;
    double d_base = 1.0;
    uint32* xp = ptr_val(x);
    digit_t* s = BIG_V(xp);
    dsize_t xl = BIG_SIZE(xp);
    short xsgn = BIG_SIGN(xp);

    while(xl--) {
	digit_t ds = *s;
	double d_next = ds * d_base + d;

	d_base *= D_BASE;

	if (xsgn) {
	    if (d_next == -HUGE_VAL) return -HUGE_VAL;
	}
	else {
	    if (d_next == HUGE_VAL) return HUGE_VAL;
	}
	s++;
	d = d_next;
    }
    return xsgn ? -d : d;
}


/*
 ** Estimate the number of decimal digits (include sign)
 */
int big_decimal_estimate(x)
    uint32 x;
{
    uint32* xp = ptr_val(x);
    int lg = I_lg(BIG_V(xp), BIG_SIZE(xp));
    int lg10 = ((lg+1)*28/93)+1;

    if (BIG_SIGN(xp)) lg10++;	/* add sign */
    return lg10+1;		/* add null */
}

/*
 ** Convert a bignum into a string of decimal numbers
 */
char* big_to_decimal(y, p, n)
uint32 y; char* p; int n;
{
    char* q = p+(n-1);
    uint32* yp = ptr_val(y);
    digit_t* x = BIG_V(yp);
    dsize_t xl = BIG_SIZE(yp);
    short sign = BIG_SIGN(yp);
    digit_t rem;

    *q-- = '\0';

    if (xl == 1 && *x < D_DECIMAL_BASE) {
	rem = *x;
	if (rem == 0)
	    *q-- = '0';
	else {
	    while(rem) {
		*q-- = (rem % 10) + '0';
		rem /= 10;
	    }
	}
    }
    else {
	digit_t* tmp  = (digit_t*) sys_alloc_from(2,sizeof(digit_t)*xl);
	dsize_t tmpl = xl;

	MOVE_DIGITS(tmp, x, xl);

	while(1) {
	    tmpl = D_div(tmp, tmpl, D_DECIMAL_BASE, tmp, &rem);
	    if (tmpl == 1 && *tmp == 0) {
		while(rem) {
		    *q-- = (rem % 10) + '0';
		    rem /= 10;
		}
		break;
	    }
	    else {
		int i = D_DECIMAL_EXP;
		while(i--) {
		    *q-- = (rem % 10) + '0';
		    rem /= 10;
		}
	    }
	}
	sys_free((char*)tmp);
    }
    if (sign)
	*q-- = '-';

    /* move to beginning of p */
    q++;
    if (p != q) {
	char *p0 = p;

	while(*q != 0)
	    *p0++ = *q++;
	*p0 = '\0';
    }
    return p;
}

/*
** Convert a bignum into a string of decimal numbers
*/

uint32 big_to_list(x, hp)
uint32 x; uint32** hp;
{
    uint32* xp = ptr_val(x);
    digit_t* dx = BIG_V(xp);
    dsize_t xl = BIG_SIZE(xp);
    short sign = BIG_SIGN(xp);
    digit_t rem;
    uint32 prev = NIL;
    uint32* curr = *hp;

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
	digit_t* tmp  = (digit_t*) sys_alloc_from(2,sizeof(digit_t)*xl);
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
	sys_free((char*)tmp);
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
static uint32 big_norm(x, xl, sign)
    uint32* x; dsize_t xl; short sign;
{
    uint32 arity;

    if (xl == 1) {
	uint32 y = BIG_DIGIT(x, 0);
	if (sign)
	    return make_small(-y);
	else
	    return make_small(y);
    }
    else if (xl == 2) {
	uint32 y = BIG_DIGIT(x,0) + BIG_DIGIT(x,1)*D_BASE;
	if (IS_USMALL(sign, y)) {
	    if (sign)
		return make_small(-y);
	    else
		return make_small(y);
	}
    }

    if ((arity = ((xl+1) >> 1)) > BIG_ARITY_MAX)
	return NIL;		/* signal error (too big) */
    if (sign) {
	*x = make_thing(arity, NEGATIVE_BIG_SUBTAG);
    } else {
	*x = make_thing(arity, POSITIVE_BIG_SUBTAG);
    }

    /* Its VERY important to patch a zero if odd number of digits! */
    if (xl & 1)
	BIG_DIGIT(x, xl) = 0;
    return make_big(x);
}

/*
** Compare bignums
** 32OK
*/
int big_comp(x, y)
uint32 x; uint32 y;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);

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
int big_ucomp(x, y)
uint32 x; uint32 y;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);

    return I_comp(BIG_V(xp), BIG_SIZE(xp), BIG_V(yp), BIG_SIZE(yp));
}

/*
** Check if bytes in xp corresponds to a bignum y
** 32UPDATE
*/
int bytes_eq_big(xp, xsz, xsgn, y)
byte* xp; dsize_t xsz; short xsgn; uint32 y;
{
    if (is_big(y)) {
	uint32* yp = ptr_val(y);
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
	sint32 yv = signed_val(y);
	uint32 xv;

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

/*
** Return number of bytes in the bignum
** 32UPDATE
*/
dsize_t big_bytes(x)
    uint32 x;
{
    uint32* xp = ptr_val(x);
    dsize_t sz = BIG_SIZE(xp);
    digit_t d = BIG_DIGIT(xp, sz-1);

    if (d < 256)
	return 2*sz - 1;
    return 2*sz;
}

/*
** Load a bignum from bytes
** xsz is the number of bytes in xp
** 32UPDATE
*/
uint32 bytes_to_big(xp, xsz, xsgn, r)
byte* xp; dsize_t xsz; short xsgn; uint32* r;
{
    digit_t* rwp = BIG_V(r);
    dsize_t rsz = 0;
    digit_t d;

    while(xsz >= 2) {
	d = xp[0] | (xp[1] << 8);
	*rwp = d;
	rwp++;
	xsz -= 2;
	xp += 2;
	rsz++;
    }

    if (xsz == 1) {
	d = *xp;
	*rwp = d;
	rwp++;
	rsz++;
    }
    return big_norm(r, rsz, xsgn);
}

/*
** Store digits in the array of bytes pointed to by p
** 32UPDATE
*/
byte* big_to_bytes(x, p)
uint32 x; byte* p;
{
    digit_t* xr = big_v(x);
    dsize_t  xl = big_size(x);
    digit_t d;

    while(xl > 1) {
	d = *xr;
	xr++;
	p[0] = d & 0xff;
	p[1] = (d >> 8) & 0xff;
	p += 2;
	xl--;
    }
    d = *xr;
    *p++ = d & 0xff;
    if (d < 256)
	return p;
    *p++ = (d >> 8) & 0xff;
    return p;
}

/*
** Store digits in the array of bytes pointed to by p
** 32UPDATE
*/
int
big_to_unsigned(uint32 x, unsigned* u)
{
    digit_t* xr = big_v(x);
    dsize_t  xl = big_size(x);
    unsigned uval = 0;
    int n = 0;

    ASSERT(sizeof(unsigned) == 4);
    if (xl > 2 || big_sign(x)) {
	return 0;
    }
    while (xl-- > 0) {
	uval |= *xr++ << n;
	n += 16;
    }
    *u = uval;
    return 1;
}


/*
** Add and subtract
** 32OK
*/
static uint32 B_plus_minus(x, xl, xsgn, y, yl, ysgn, r)
digit_t* x; dsize_t xl; short xsgn;
digit_t* y; dsize_t yl; short ysgn;
uint32* r;
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
uint32 big_plus(x, y, r)
uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),BIG_SIGN(yp), r);
}

/*
** Subtract bignums
** 32OK
*/

uint32 big_minus(x, y, r)
uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);

    return B_plus_minus(BIG_V(xp),BIG_SIZE(xp),BIG_SIGN(xp),
			BIG_V(yp),BIG_SIZE(yp),!BIG_SIGN(yp), r);
}

/*
** Subtract a digit from big number
*/
uint32 big_minus_small(x, y, r)
    uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp),y, BIG_V(r)), 
			BIG_SIGN(xp));
    else
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp),y, BIG_V(r)), 
			BIG_SIGN(xp));
}

/*
** Multiply bignums
** 32OK
*/

uint32 big_times(x, y, r)
uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
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

uint32 big_div(x, y, q)
uint32 x; uint32 y; uint32* q;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
    short sign = BIG_SIGN(xp) != BIG_SIGN(yp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);
    dsize_t qsz;

    if (ysz == 1) {
	digit_t rem;
	qsz = D_div(BIG_V(xp), xsz, BIG_DIGIT(yp,0), BIG_V(q), &rem);
    }
    else {
	uint32* remp;
	dsize_t rem_sz;

	qsz = xsz - ysz + 1;
	remp = q + 1 + ((qsz + 1) >> 1);
	qsz = I_div(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(q), BIG_V(remp),
		    &rem_sz);
    }
    return big_norm(q, qsz, sign);
}

/*
** Remainder
** 32UPDATE?
*/
uint32 big_rem(x, y, r)
    uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);
    dsize_t ysz = BIG_SIZE(yp);

    if (ysz == 1) {
	digit_t rem;
	rem = D_rem(BIG_V(xp), xsz, BIG_DIGIT(yp,0));
	if (sign)
	    return make_small(-rem);
	else
	    return make_small(rem);
    }
    else {
	dsize_t rsz = I_rem(BIG_V(xp), xsz, BIG_V(yp), ysz, BIG_V(r));
	return big_norm(r, rsz, sign);
    }
}

uint32 big_neg(x, r)
uint32 x; uint32* r;
{
    uint32* xp = ptr_val(x);
    dsize_t xsz = BIG_SIZE(xp);
    short xsgn = BIG_SIGN(xp);
    
    MOVE_DIGITS(BIG_V(r), BIG_V(xp), xsz);
    return big_norm(r, xsz, !xsgn);
}


uint32 big_band(x, y, r)
uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
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


uint32 big_bor(x, y, r)
uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
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


uint32 big_bxor(x, y, r)
    uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    uint32* yp = ptr_val(y);
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

uint32 big_bnot(x,  r)
    uint32 x; uint32* r;
{
    uint32* xp = ptr_val(x);
    short sign = !BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_bnot(BIG_V(xp), xsz, sign, BIG_V(r)), sign);
}


uint32 big_lshift(x, y, r)
    uint32 x; sint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);
    short sign = BIG_SIGN(xp);
    dsize_t xsz = BIG_SIZE(xp);

    return big_norm(r, I_lshift(BIG_V(xp), xsz, y, sign, BIG_V(r)), sign);
}


/* add unsigned small int y to x */

uint32 big_plus_small(x, y, r)
    uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);

    if (BIG_SIGN(xp))
	return big_norm(r, D_sub(BIG_V(xp),BIG_SIZE(xp),y, BIG_V(r)), 
			BIG_SIGN(xp));
    else
	return big_norm(r, D_add(BIG_V(xp),BIG_SIZE(xp),y, BIG_V(r)), 
			BIG_SIGN(xp));
}

uint32 big_times_small(x, y, r)
    uint32 x; uint32 y; uint32* r;
{
    uint32* xp = ptr_val(x);

    return big_norm(r, D_mul(BIG_V(xp),BIG_SIZE(xp),y, BIG_V(r)),
		    BIG_SIGN(xp));
}
