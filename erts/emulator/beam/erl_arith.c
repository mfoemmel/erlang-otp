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
/*
** Arithmetic functions formerly found in beam_emu.c
** now available as bifs as erl_db_util and db_match_compile needs
** them.
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
#include "atom.h"

#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

static Eterm shift(Process* p, Eterm arg1, Eterm arg2, int right);

/*
 ** Bif interfaces
 */

BIF_RETTYPE splus_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RET(BIF_ARG_1);
} 

BIF_RETTYPE splus_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(erts_mixed_plus(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE sminus_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RET(erts_mixed_minus(BIF_P, make_small(0), BIF_ARG_1));
} 

BIF_RETTYPE sminus_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(erts_mixed_minus(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE stimes_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(erts_mixed_times(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE sdiv_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(erts_mixed_div(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE div_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (BIF_ARG_2 == SMALL_ZERO) {
	BIF_ERROR(BIF_P, BADARITH);
    }
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(make_small(signed_val(BIF_ARG_1) / signed_val(BIF_ARG_2)));
    } 
    BIF_RET(erts_int_div(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE rem_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (BIF_ARG_2 == SMALL_ZERO) {
	BIF_ERROR(BIF_P, BADARITH);
    }
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	/* Is this really correct? Isn't there a difference between 
	   remainder and modulo that is not defined in C? Well, I don't
	   remember, this is the way it's done in beam_emu anyway... */
	BIF_RET(make_small(signed_val(BIF_ARG_1) % signed_val(BIF_ARG_2)));
    } 
    BIF_RET(erts_int_rem(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE band_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(BIF_ARG_1 & BIF_ARG_2);
    } 
    BIF_RET(erts_band(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bor_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(BIF_ARG_1 | BIF_ARG_2);
    } 
    BIF_RET(erts_bor(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bxor_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (is_both_small(BIF_ARG_1,BIF_ARG_2)){
	BIF_RET(make_small(signed_val(BIF_ARG_1) ^ signed_val(BIF_ARG_2)));
    } 
    BIF_RET(erts_bxor(BIF_P, BIF_ARG_1, BIF_ARG_2));
} 

BIF_RETTYPE bsl_2(Process* p, Eterm arg1, Eterm arg2)
{
    BIF_RET(shift(p, arg1, arg2, 0));
} 

BIF_RETTYPE bsr_2(Process* p, Eterm arg1, Eterm arg2)
{
    BIF_RET(shift(p, arg1, arg2, 1));
} 

static Eterm
shift(Process* p, Eterm arg1, Eterm arg2, int right)
{
    int i;
    int ires;
    Eterm tmp_big1[2];
    Eterm* bigp;
    
    if (right) {
	if (is_small(arg2)) {
	    i = -signed_val(arg2);
	    if (is_small(arg1)) {
		goto small_shift;
	    } else if (is_big(arg1)) {
		if (i == 0) {
		    BIF_RET(arg1);
		}
		goto big_shift;
	    }
	}
    } else {
	if (is_small(arg2)) {
	    i = signed_val(arg2);

	    if (is_small(arg1)) {
	    small_shift:
		ires = signed_val(arg1);
	     
		if (i == 0 || ires == 0) {
		    BIF_RET(arg1);
		} else if (i < 0)  { /* Right shift */
		    i = -i;
		    if (i >= SMALL_BITS-1) {
			arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		    } else {
			arg1 = make_small(ires >> i);
		    }
		    BIF_RET(arg1);
		} else if (i < SMALL_BITS-1) { /* Left shift */
		    if ((ires > 0 && ((-1 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			((-1 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			arg1 = make_small(ires << i);
			BIF_RET(arg1);
		    }
		}
		arg1 = small_to_big(ires, tmp_big1);

	    big_shift:
		if (i > 0) {	/* Left shift. */
		    ires = big_size(arg1) + (i / D_EXP);
		} else {	/* Right shift. */
		    ires = big_size(arg1);
		    if (ires <= (-i / D_EXP))
			ires = 3;
		    else
			ires -= (-i / D_EXP);
		}
		bigp = ArithAlloc(p, BIG_NEED_SIZE(ires+1));

		arg1 = big_lshift(arg1, i, bigp);
		ArithCheck(p);
		if (is_nil(arg1)) {
		    BIF_ERROR(p, SYSTEM_LIMIT);
		}
		BIF_RET(arg1);
	    } else if (is_big(arg1)) {
		if (i == 0) {
		    BIF_RET(arg1);
		}
		goto big_shift;
	    }
	}
    }
    BIF_ERROR(p, BADARITH);
}



/*
** bnot is "inlined" in bif, no other part of
** the runtime need's it, it's too simple....
*/
 
BIF_RETTYPE bnot_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm ret;
    if (is_small(BIF_ARG_1)) {
	ret = make_small(~signed_val(BIF_ARG_1));
    } else if (is_big(BIF_ARG_1)) {
	Eterm* bigp = ArithAlloc(BIF_P, 
				 BIG_NEED_SIZE(big_size(BIF_ARG_1)+1));
	ret = big_bnot(BIF_ARG_1, bigp);
	ArithCheck(BIF_P);
	if (is_nil(ret)) {
	    BIF_ERROR(BIF_P,SYSTEM_LIMIT);
	}
    } else {
	BIF_ERROR(BIF_P,BADARITH);
    }
    BIF_RET(ret);
} 


/*
** Implementation and interfaces for the rest of the runtime system.
**
** NB:
** The global functions named erts_XXX are used by the beam
** emulator loop, do NOT fiddle with these without considering
** that fact, please...
*/
Eterm erts_mixed_plus(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	ires = signed_val(arg1) + signed_val(arg2);
	ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
	if (MY_IS_SSMALL(ires)) {
	    return make_small(ires);
	} else {
	    hp = ArithAlloc(p, 2);
	    res = small_to_big(ires, hp);
	    ArithCheck(p);
	    return res;
	}
	break;
    case SMALL_BIG:
	if (arg1 == SMALL_ZERO)
	    return arg2;
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	goto big_plus;
    case BIG_SMALL:
	if (arg2 == SMALL_ZERO)
	    return(arg1);
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto big_plus;
    case BIG_BIG:
    big_plus:
	sz1 = big_size(arg1);
	sz2 = big_size(arg2);
	sz = MAX(sz1, sz2)+1;
	need_heap = BIG_NEED_SIZE(sz);
	hp = ArithAlloc(p, need_heap);
	res = big_plus(arg1, arg2, hp);
	ArithCheck(p);
	if (is_nil(res)) {
	    p->freason = SYSTEM_LIMIT;
	    return THE_NON_VALUE;
	}
	return res;
    case SMALL_FLOAT:
	f1.fd = signed_val(arg1);
	GET_DOUBLE(arg2, f2);
	goto float_plus;
    case BIG_FLOAT:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	}
	GET_DOUBLE(arg2, f2);
	goto float_plus;
    case FLOAT_SMALL:
        GET_DOUBLE(arg1, f1);
	f2.fd = signed_val(arg2);
	goto float_plus;
    case FLOAT_BIG:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	GET_DOUBLE(arg1, f1);
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd)) {
	    goto badarith;
	}
	goto float_plus;
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, f1);
	GET_DOUBLE(arg2, f2);

    float_plus:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = f1.fd + f2.fd;
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	} else {
	    hp = ArithAlloc(p, 3);
	    res = make_float(hp);
	    ArithCheck(p);
	    PUT_DOUBLE(f1, hp);
	    return res;
	}
	break;
    default:
    badarith:
	p->freason = BADARITH;
	break;
    }
    return THE_NON_VALUE;
}

Eterm erts_mixed_minus(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	ires = signed_val(arg1) - signed_val(arg2);
	ASSERT(MY_IS_SSMALL(ires) == IS_SSMALL(ires));
	if (MY_IS_SSMALL(ires)) {
	    return make_small(ires);
	} else {
	    hp = ArithAlloc(p, 2);
	    res = small_to_big(ires, hp);
	    ArithCheck(p);
	    return res;
	}
	break;
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	goto big_minus;
    case BIG_SMALL:
	if (arg2 == SMALL_ZERO)
	    return(arg1);
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto big_minus;
    case BIG_BIG:
    big_minus:
	sz1 = big_size(arg1);
	sz2 = big_size(arg2);
	sz = MAX(sz1, sz2)+1;
	need_heap = BIG_NEED_SIZE(sz);
	hp = ArithAlloc(p, need_heap);
	res = big_minus(arg1, arg2, hp);
	ArithCheck(p);
	if (is_nil(res)) {
	    p->freason = SYSTEM_LIMIT;
	    return THE_NON_VALUE;
	}
	return res;
    case SMALL_FLOAT:
	f1.fd = signed_val(arg1);
	GET_DOUBLE(arg2, f2);
	goto float_minus;
    case BIG_FLOAT:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	}
	GET_DOUBLE(arg2, f2);
	goto float_minus;
    case FLOAT_SMALL:
        GET_DOUBLE(arg1, f1);
	f2.fd = signed_val(arg2);
	goto float_minus;
    case FLOAT_BIG:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	GET_DOUBLE(arg1, f1);
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd)) {
	    goto badarith;
	}
	goto float_minus;
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, f1);
	GET_DOUBLE(arg2, f2);

    float_minus:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = f1.fd - f2.fd;
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	} else {
	    hp = ArithAlloc(p, 3);
	    res = make_float(hp);
	    ArithCheck(p);
	    PUT_DOUBLE(f1, hp);
	    return res;
	}
	break;
    default:
    badarith:
	p->freason = BADARITH;
	break;
    }
    return THE_NON_VALUE;
}

Eterm erts_mixed_times(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm res;
    FloatDef f1, f2;
    dsize_t sz1, sz2, sz;
    int need_heap;
    Eterm* hp;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	if ((arg1 == SMALL_ZERO) || (arg2 == SMALL_ZERO))
	    return(SMALL_ZERO);
	if (arg1 == SMALL_ONE)
	    return(arg2);
	if (arg2 == SMALL_ONE)
	    return(arg1);
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto big_times;
    case SMALL_BIG:
	if (arg1 == SMALL_ZERO)
	    return(SMALL_ZERO);
	if (arg1 == SMALL_ONE)
	    return(arg2);
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	goto big_times;
    case BIG_SMALL:
	if (arg2 == SMALL_ZERO)
	    return(SMALL_ZERO);
	if (arg2 == SMALL_ONE)
	    return(arg1);
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto big_times;
    case BIG_BIG:
    big_times:
	sz1 = big_size(arg1);
	sz2 = big_size(arg2);
	sz = sz1 + sz2;
	need_heap = BIG_NEED_SIZE(sz);
	hp = ArithAlloc(p, need_heap);
	res = big_times(arg1, arg2, hp);
	ArithCheck(p);
	if (is_nil(res)) {
	    p->freason = SYSTEM_LIMIT;
	    return THE_NON_VALUE;
	}
	return res;
    case SMALL_FLOAT:
	f1.fd = signed_val(arg1);
	GET_DOUBLE(arg2, f2);
	goto float_times;
    case BIG_FLOAT:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	}
	GET_DOUBLE(arg2, f2);
	goto float_times;
    case FLOAT_SMALL:
        GET_DOUBLE(arg1, f1);
	f2.fd = signed_val(arg2);
	goto float_times;
    case FLOAT_BIG:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	GET_DOUBLE(arg1, f1);
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd)) {
	    goto badarith;
	}
	goto float_times;
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, f1);
	GET_DOUBLE(arg2, f2);

    float_times:
	if (!FP_PRE_CHECK_OK()) {
	    goto badarith;
	}
	f1.fd = f1.fd * f2.fd;
	if (!FP_RESULT_OK(f1.fd)) {
	    goto badarith;
	} else {
	    hp = ArithAlloc(p, 3);
	    res = make_float(hp);
	    ArithCheck(p);
	    PUT_DOUBLE(f1, hp);
	    return res;
	}
	break;
    default:
    badarith:
	p->freason = BADARITH;
	break;
    }
    return THE_NON_VALUE;
}

Eterm erts_mixed_div(Process* p, Eterm arg1, Eterm arg2)
{
    FloatDef f1, f2;
    Eterm* hp;

    if (!FP_PRE_CHECK_OK()) {
	goto badarith;
    }

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_SMALL:
	f1.fd = signed_val(arg1);
	f2.fd = signed_val(arg2);
	break;
    case SMALL_BIG:
	f1.fd = signed_val(arg1);
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd))
	    goto badarith;
	break;
    case BIG_SMALL:
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd))
	    goto badarith;
	f2.fd = signed_val(arg2);
	break;
    case BIG_BIG:
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd))
	    goto badarith;
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd))
	    goto badarith;
	break;
    case SMALL_FLOAT:
	f1.fd = signed_val(arg1);
	GET_DOUBLE(arg2, f2);
	break;
    case BIG_FLOAT:
	f1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(f1.fd))
	    goto badarith;
	GET_DOUBLE(arg2, f2);
	break;
    case FLOAT_SMALL:
        GET_DOUBLE(arg1, f1);
	f2.fd = signed_val(arg2);
	break;
    case FLOAT_BIG:
	GET_DOUBLE(arg1, f1);
	f2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(f2.fd))
	    goto badarith;
	break;
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, f1);
	GET_DOUBLE(arg2, f2);
	break;
    default:
    badarith:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }

    if (f2.fd == 0.0) {
	goto badarith;
    }
    f1.fd = f1.fd / f2.fd;
    if (!FP_RESULT_OK(f1.fd)) {
	goto badarith;
    }
    hp = ArithAlloc(p, 3);
    PUT_DOUBLE(f1, hp);
    ArithCheck(p);
    return make_float(hp);
}

Eterm erts_int_div(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big2[2];
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_div;
    case SMALL_BIG:
	return SMALL_ZERO;
    case BIG_BIG:
    L_big_div:
	ires = big_ucomp(arg1, arg2);
	if (ires < 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires == 0) {
	    arg1 = (big_sign(arg1) == big_sign(arg2)) ?
		SMALL_ONE : SMALL_MINUS_ONE;
	} else {
	    Eterm* hp;
	    int i = big_size(arg1);

	    ires = big_size(arg2);
	    hp = ArithAlloc(p, BIG_NEED_SIZE(i-ires+1) + BIG_NEED_SIZE(i));
	    arg1 = big_div(arg1, arg2, hp);
	    ArithCheck(p);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

Eterm erts_int_rem(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big2[2];
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	goto L_big_rem;
    case SMALL_BIG:
	return arg1;
    case BIG_BIG:
    L_big_rem:
	ires = big_ucomp(arg1, arg2);
	if (ires == 0) {
	    arg1 = SMALL_ZERO;
	} else if (ires > 0) {
	    Eterm* hp = ArithAlloc(p, BIG_NEED_SIZE(big_size(arg1)));
	    arg1 = big_rem(arg1, arg2, hp);
	    ArithCheck(p);
	    if (is_nil(arg1)) {
		p->freason = SYSTEM_LIMIT;
		return THE_NON_VALUE;
	    }
	}
	return arg1;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
}

Eterm erts_band(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm* hp;
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    ires = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = ArithAlloc(p, ires);
    arg1 = big_band(arg1, arg2, hp);
    ArithCheck(p);
    ASSERT(is_not_nil(arg1));
    return arg1;
}

Eterm erts_bor(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm* hp;
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    ires = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = ArithAlloc(p, ires);
    arg1 = big_bor(arg1, arg2, hp);
    ArithCheck(p);
    ASSERT(is_not_nil(arg1));
    return arg1;
}

Eterm erts_bxor(Process* p, Eterm arg1, Eterm arg2)
{
    Eterm tmp_big1[2];
    Eterm tmp_big2[2];
    Eterm* hp;
    int ires;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	arg1 = small_to_big(signed_val(arg1), tmp_big1);
	break;
    case BIG_SMALL:
	arg2 = small_to_big(signed_val(arg2), tmp_big2);
	break;
    case BIG_BIG:
	break;
    default:
	p->freason = BADARITH;
	return THE_NON_VALUE;
    }
    ires = BIG_NEED_SIZE(MAX(big_size(arg1), big_size(arg2)) + 1);
    hp = ArithAlloc(p, ires);
    arg1 = big_bxor(arg1, arg2, hp);
    ArithCheck(p);
    ASSERT(is_not_nil(arg1));
    return arg1;
}
