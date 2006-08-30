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
 * Numeric guard BIFs.
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
#include "erl_binary.h"

#if defined(HEAP_FRAG_ELIM_TEST)
# define ArithAlloc(a, b) (abort(), THE_NON_VALUE)
#endif

/*
 * Guard BIFs MUST allocate memory in heap fragments using ArithAlloc().
 */

BIF_RETTYPE abs_1(BIF_ALIST_1)
{
    Eterm res;
    Sint i0, i;
    Eterm* hp;

    /* integer arguments */
    if (is_small(BIF_ARG_1)) {
	i0 = signed_val(BIF_ARG_1);
	i = labs(i0);
	if (i0 == MIN_SMALL) {
	    hp = ArithAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    BIF_RET(uint_to_big(i, hp));
	} else {
	    BIF_RET(make_small(i));
	}
    }
    else if (is_big(BIF_ARG_1)) {
	if (!big_sign(BIF_ARG_1)) {
	    BIF_RET(BIF_ARG_1);
	} else {
	    int sz = big_arity(BIF_ARG_1) + 1;
	    Uint* x;

	    hp = ArithAlloc(BIF_P, sz);	/* See note at beginning of file */
	    sz--;
	    res = make_big(hp);
	    x = big_val(BIF_ARG_1);
	    *hp++ = make_pos_bignum_header(sz);
	    x++;                          /* skip thing */
	    while(sz--)
		*hp++ = *x++;
	    BIF_RET(res);
	}
    }
    else if (is_float(BIF_ARG_1)) {
	FloatDef f;

	GET_DOUBLE(BIF_ARG_1, f);
	if (f.fd < 0.0) {
	    hp = ArithAlloc(BIF_P, FLOAT_SIZE_OBJECT);
	    f.fd = fabs(f.fd);
	    res = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    BIF_RET(res);
	}
	else
	    BIF_RET(BIF_ARG_1);
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE float_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    FloatDef f;
     
    /* check args */
    if (is_not_integer(BIF_ARG_1)) {
	if (is_float(BIF_ARG_1))  {
	    BIF_RET(BIF_ARG_1);
	} else {
	badarg:
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    if (is_small(BIF_ARG_1)) {
	Sint i = signed_val(BIF_ARG_1);
	f.fd = i;		/* use "C"'s auto casting */
    } else if (big_to_double(BIF_ARG_1, &f.fd) < 0) {
	goto badarg;
    }
    hp = ArithAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

BIF_RETTYPE trunc_1(BIF_ALIST_1)
{
    Eterm res;
    FloatDef f;
     
    /* check arg */
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* truncate it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd >= 0.0) ? floor(f.fd) : ceil(f.fd));
    BIF_RET(res);
}

BIF_RETTYPE round_1(BIF_ALIST_1)
{
    Eterm res;
    FloatDef f;
     
    /* check arg */ 
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
     
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* round it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd > 0.0) ? f.fd + 0.5 : f.fd - 0.5);
    BIF_RET(res);
}

BIF_RETTYPE length_1(BIF_ALIST_1)
{
    Eterm list;
    Uint i;
     
    if (is_nil(BIF_ARG_1)) 
	BIF_RET(SMALL_ZERO);
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    list = BIF_ARG_1;
    i = 0;
    while (is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(i));
}

/* returns the size of a tuple or a binary */

BIF_RETTYPE size_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	Eterm* tupleptr = tuple_val(BIF_ARG_1);

	BIF_RET(make_small(arityval(*tupleptr)));
    } else if (is_binary(BIF_ARG_1)) {
	Uint sz = binary_size(BIF_ARG_1);
	if (IS_USMALL(0, sz)) {
	    return make_small(sz);
	} else {
	    Eterm* hp = ArithAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	    BIF_RET(uint_to_big(sz, hp));
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}
