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
**
** Math bifs
**
** Map trigonmetric functions to this one if 
** it it's not in them system
** i.e SYSFLAGS=-Dlgamma=undef_math_func_1 ...
**
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

static double
undef_math_func_1(double x)
{
    return x;
}

static double
undef_math_func_2(double x, double y)
{
    return x+y;
}

static Eterm
math_call_1(Process* p, double (*func)(double), Eterm arg1)
{
    FloatDef a1;
    Eterm res;
    Eterm* hp;

    if (func == undef_math_func_1) {
	p->freason = EXC_UNDEF;
	return THE_NON_VALUE;
    }

    ERTS_FP_CHECK_INIT();
    if (is_float(arg1)) {
	GET_DOUBLE(arg1, a1);
    } else if (is_small(arg1)) {
	a1.fd = signed_val(arg1);
    } else if (is_big(arg1)) {
	if (big_to_double(arg1, &a1.fd) < 0) {
	badarith:
	    p->freason = BADARITH;
	    return THE_NON_VALUE;
	}
    } else {
	p->freason = BADARG;
	return THE_NON_VALUE;
    }
    a1.fd = (*func)(a1.fd);
    ERTS_FP_ERROR(a1.fd, goto badarith);
    hp = HAlloc(p, 3);
    res = make_float(hp);
    PUT_DOUBLE(a1, hp);
    return res;
}


static Eterm
math_call_2(Process* p, double (*func)(double, double), Eterm arg1, Eterm arg2)
{
    FloatDef a1;
    FloatDef a2;
    Eterm res;
    Eterm* hp;

    if (func == undef_math_func_2) {
	p->freason = EXC_UNDEF;
	return THE_NON_VALUE;
    }

    ERTS_FP_CHECK_INIT();
    if (is_float(arg1)) {
	GET_DOUBLE(arg1, a1);
    } else if (is_small(arg1)) {
	a1.fd = signed_val(arg1);
    } else if (is_big(arg1)) {
	if (big_to_double(arg1, &a1.fd) < 0) {
	badarith:
	    p->freason = BADARITH;
	    return THE_NON_VALUE;
	}
    } else {
	p->freason = BADARG;
	return THE_NON_VALUE;
    }

    if (is_float(arg2)) {
	GET_DOUBLE(arg2, a2);
    } else if (is_small(arg2)) {
	a2.fd = signed_val(arg2);
    } else if (is_big(arg2)) {
	if (big_to_double(arg2, &a2.fd) < 0) {
	    goto badarith;
	}
    } else {
	p->freason = BADARG;
	return THE_NON_VALUE;
    }

    a1.fd = (*func)(a1.fd, a2.fd);
    ERTS_FP_ERROR(a1.fd, goto badarith);
    hp = HAlloc(p, 3);
    res = make_float(hp);
    PUT_DOUBLE(a1, hp);
    return res;
}

BIF_RETTYPE math_cos_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, cos, BIF_ARG_1);
}

BIF_RETTYPE math_cosh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, cosh, BIF_ARG_1);
}

BIF_RETTYPE math_sin_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, sin, BIF_ARG_1);
}

BIF_RETTYPE math_sinh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, sinh, BIF_ARG_1);
}

BIF_RETTYPE math_tan_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, tan, BIF_ARG_1);
}


BIF_RETTYPE math_tanh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, tanh, BIF_ARG_1);
}


BIF_RETTYPE math_acos_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, acos, BIF_ARG_1);
}

BIF_RETTYPE math_acosh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, acosh, BIF_ARG_1);
}

BIF_RETTYPE math_asin_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, asin, BIF_ARG_1);
}

BIF_RETTYPE math_asinh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, asinh, BIF_ARG_1);
}

BIF_RETTYPE math_atan_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, atan, BIF_ARG_1);
}

BIF_RETTYPE math_atanh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, atanh, BIF_ARG_1);
}

BIF_RETTYPE math_erf_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, erf, BIF_ARG_1);
}

BIF_RETTYPE math_erfc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, erfc, BIF_ARG_1);
}

BIF_RETTYPE math_exp_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, exp, BIF_ARG_1);
}

BIF_RETTYPE math_log_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, log, BIF_ARG_1);
}


BIF_RETTYPE math_log10_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, log10, BIF_ARG_1);
}

BIF_RETTYPE math_sqrt_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return math_call_1(BIF_P, sqrt, BIF_ARG_1);
}

BIF_RETTYPE math_atan2_2(BIF_ALIST_2)
BIF_ADECL_2
{
    return math_call_2(BIF_P, atan2, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE math_pow_2(BIF_ALIST_2)
BIF_ADECL_2
{
    return math_call_2(BIF_P, pow, BIF_ARG_1, BIF_ARG_2);
}




