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
 * Operator BIFs.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"


BIF_RETTYPE and_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE or_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE xor_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_true && BIF_ARG_2 == am_false)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_true)
	BIF_RET(am_true);
    else if (BIF_ARG_1 == am_false && BIF_ARG_2 == am_false)
	BIF_RET(am_false);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE not_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (BIF_ARG_1 == am_true)
	BIF_RET(am_false);
    else if (BIF_ARG_1 == am_false)
	BIF_RET(am_true);
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE sgt_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) > 0 ? am_true : am_false);
}

BIF_RETTYPE sge_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) >= 0 ? am_true : am_false);
}

BIF_RETTYPE slt_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) < 0 ? am_true : am_false);
}

BIF_RETTYPE sle_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) <= 0 ? am_true : am_false);
}

BIF_RETTYPE seq_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(eq(BIF_ARG_1, BIF_ARG_2) ? am_true : am_false);
}

BIF_RETTYPE seqeq_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) == 0 ? am_true : am_false);
}

BIF_RETTYPE sneq_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(eq(BIF_ARG_1, BIF_ARG_2) ? am_false : am_true);
}

BIF_RETTYPE sneqeq_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RET(cmp(BIF_ARG_1, BIF_ARG_2) != 0 ? am_true : am_false);
}

BIF_RETTYPE is_atom_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_atom(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}
	
BIF_RETTYPE is_constant_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_tuple(BIF_ARG_1) || is_list(BIF_ARG_1) || is_nil(BIF_ARG_1)) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE is_float_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_float(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_integer_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_integer(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_list(BIF_ARG_1) || is_nil(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_number_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_number(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}


BIF_RETTYPE is_pid_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_pid(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_port_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_port(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_reference_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_refer(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_tuple_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_tuple(BIF_ARG_1)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_binary_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_binary(BIF_ARG_1) &&
	(thing_subtag(*ptr_val(BIF_ARG_1)) != FUN_SUBTAG)) {
	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}

BIF_RETTYPE is_function_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm *t;

    if (is_binary(BIF_ARG_1) &&
	(thing_subtag(*ptr_val(BIF_ARG_1)) == FUN_SUBTAG)) {
	BIF_RET(am_true);
    }
#ifdef ALLOW_FUN_TUPLES
    else if (is_tuple(BIF_ARG_1) && arityval(*(t = ptr_val(BIF_ARG_1))) == 5
	&& t[1] == am_fun) {
 	BIF_RET(am_true);
    }
#endif
    BIF_RET(am_false);
}

/* Record test cannot actually be a bif. The epp processor is involved in
   the real guard test, we have to add one more parameter, the 
   returnvalue of record_info(size, Rec), which is the arity of the TUPLE.
   This may seem awkward when applied from the shell, where the plain
   tuple test is more understandable, I think... */
BIF_RETTYPE is_record_3(BIF_ALIST_3) 
BIF_ADECL_3
{
    Eterm *t;
    if (is_not_atom(BIF_ARG_2) || is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_tuple(BIF_ARG_1) && 
	arityval(*(t = ptr_val(BIF_ARG_1))) == signed_val(BIF_ARG_3)
	&& t[1] == BIF_ARG_2) {
 	BIF_RET(am_true);
    }
    BIF_RET(am_false);
}
	

    


