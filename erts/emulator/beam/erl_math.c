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

#define MATHERR_BADARG   make_small(BADARG)
#define MATHERR_BADARITH make_small(BADARITH)
#define MATHERR_UNDEF    make_small(UNDEF)

static double undef_math_func_1(x)
double x;
{
    return x;
}

static double undef_math_func_2(x, y)
double x, y;
{
    return x+y;
}

static uint32 math_call_1(p, func, arg1)
Process* p; FUNCTION(double, (*func), (double)); uint32 arg1;
{
    FloatDef a1;
    uint32 res;
    uint32* hp;

    if (func == undef_math_func_1)
	return MATHERR_UNDEF;
    if (!FP_PRE_CHECK_OK())
	return MATHERR_BADARITH;

    if (is_float(arg1))
	GET_DOUBLE(arg1, a1);
    else if (is_small(arg1))
	a1.fd = signed_val(arg1);
    else if (is_big(arg1)) {
	a1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(a1.fd))
	    return MATHERR_BADARITH;
    }
    else
	return MATHERR_BADARG;
    a1.fd = (*func)(a1.fd);
    if (FP_RESULT_OK(a1.fd)) {
	hp = HAlloc(p, 3);
	res = make_float(hp);
	PUT_DOUBLE(a1, hp);
	return res;
    }
    return MATHERR_BADARITH;
}


static uint32 math_call_2(p, func, arg1, arg2)
Process* p; FUNCTION(double, (*func), (double, double));
uint32 arg1; uint32 arg2;
{
    FloatDef a1;
    FloatDef a2;
    uint32 res;
    uint32* hp;

    if (func == undef_math_func_2)
	return MATHERR_UNDEF;
    if (!FP_PRE_CHECK_OK())
	return MATHERR_BADARITH;

    if (is_float(arg1))
	GET_DOUBLE(arg1, a1);
    else if (is_small(arg1))
	a1.fd = signed_val(arg1);
    else if (is_big(arg1)) {
	a1.fd = big_to_double(arg1);
	if (!FP_RESULT_OK(a1.fd))
	    return MATHERR_BADARITH;
    }
    else
	return MATHERR_BADARG;

    if (is_float(arg2))
	GET_DOUBLE(arg2, a2);
    else if (is_small(arg2))
	a2.fd = signed_val(arg2);
    else if (is_big(arg2)) {
	a2.fd = big_to_double(arg2);
	if (!FP_RESULT_OK(a2.fd))
	    return MATHERR_BADARITH;
    }
    else
	return MATHERR_BADARG;

    a1.fd = (*func)(a1.fd,a2.fd);
    if (FP_RESULT_OK(a1.fd)) {
	hp = HAlloc(p, 3);
	res = make_float(hp);
	PUT_DOUBLE(a1, hp);
	BIF_RET(res);
    }
    return MATHERR_BADARITH;
}

BIF_RETTYPE m_cos_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, cos, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_cosh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, cosh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_sin_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, sin, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_sinh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, sinh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_tan_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, tan, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}


BIF_RETTYPE m_tanh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, tanh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}


BIF_RETTYPE m_acos_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, acos, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_acosh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, acosh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_asin_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, asin, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_asinh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, asinh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_atan_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, atan, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_atanh_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, atanh, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_erf_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, erf, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_erfc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, erfc, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_exp_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, exp, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_log_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, log, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}


BIF_RETTYPE m_log10_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, log10, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_sqrt_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res = math_call_1(BIF_P, sqrt, BIF_ARG_1);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}

BIF_RETTYPE m_atan2_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32 res = math_call_2(BIF_P, atan2, BIF_ARG_1, BIF_ARG_2);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res))
    BIF_RET(res);
}

BIF_RETTYPE m_pow_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32 res = math_call_2(BIF_P, pow, BIF_ARG_1, BIF_ARG_2);
    if (is_small(res))
	BIF_ERROR(BIF_P, unsigned_val(res));
    BIF_RET(res);
}




