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
 *
 * This file implements functional arrays as described in
 *
 *   L.C. Paulson: ML for the Working Programmer (2nd edition), pp. 336-339.
 *
 * The exception list is implemented as five-words structures on the heap:
 *
 * +---------+-------+-------+-------+------+
 * | VECTOR  | Limit | Index | Value | Next |
 * | header  | 	     | 	     | 	     | 	    |
 * +---------+-------+-------+-------+------+
 *
 * The mutable array looks like this:
 *
 * +---------+-------+-------+-------+------+------+------
 * | VECTOR  | El1   | El2   | El3   | El4  | El5  |....
 * | header  | 	     | 	     | 	     | 	    |	   |
 * +---------+-------+-------+-------+------+------+------
 *
 * It always contains at least 5 elements (El1-El5) (some of which
 * may be NIL), so that we can distinguish it from the exception list.
 *
 * We re-root the exception list after each update, so that a newly added entry
 * has shortest exception list.
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
#include "erl_vector.h"

#define is_vector_exception(x) (is_boxed((x)) && *boxed_val((x)) == HEADER_VECTOR)
#define is_not_vector_exception(x) (!is_vector_exception(x))
#define is_vector_array(x) (is_vector(x) && *boxed_val((x)) > HEADER_VECTOR)
#define is_not_vector_array(x) (!is_vector_array(x))
#define make_vector_array(n) _make_header((n),_TAG_HEADER_VECTOR)

static Eterm reroot(Process* p, Eterm Vec);

Eterm
vector_new_2(Process* p, Eterm Sz, Eterm Value)
{
    int n;
    int real_n;
    size_t need;
    Eterm* hp;
    Eterm res;
    Eterm tmp;
    int i;

    if (is_not_small(Sz) || (n = signed_val(Sz)) < 0) {
	BIF_ERROR(p, BADARG);
    }
    real_n = (n < 5) ? 5 : n;
    need = real_n + 1 + 5;
    hp = HAlloc(p, need);
    tmp = make_vector(hp);
    *hp++ = make_vector_array(real_n);
    for (i = 1; i <= n; i++) {
	*hp++ = Value;
    }
    for ( ; i <= real_n; i++) {
	*hp++ = NIL;
    }
    res = make_vector(hp);
    hp[0] = HEADER_VECTOR;
    hp[1] = Sz;
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = tmp;
    return res;
}

Eterm
vector_from_list_1(Process* p, Eterm list)
{
    Eterm* cons;
    size_t need;
    Eterm* hp;
    Eterm tmp;
    Eterm res;
    int len;
    int real_len;
    int n;

    if ((len = list_length(list)) < 0) {
	BIF_ERROR(p, BADARG);
    }

    real_len = (len < 5) ? 5 : len;
    need = real_len + 1 + 5;
    hp = HAlloc(p, need);
    tmp = make_vector(hp);
    *hp++ = make_vector_array(real_len);
    while (is_list(list)) {
	cons = list_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    for (n = 5-len; n > 0; n--) {
	*hp++ = NIL;
    }
    res = make_vector(hp);
    hp[0] = HEADER_VECTOR;
    hp[1] = make_small(len);
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = tmp;
    return res;
}

Eterm
vector_to_list_2(Process* p, Eterm Vec, Eterm List)
{
    Eterm* hp;
    Eterm* listp;
    Eterm* vp;
    Eterm tmp;
    int n;
    int i;

    if (is_not_vector(Vec)) {
	BIF_ERROR(p, BADARG);
    }
    vp = vector_val(Vec);
    n = signed_val(vp[1]);

    /*
     * Build a list skeleton
     */

    listp = hp = HAlloc(p, 2 * n);
    hp = hp + 2 * n;
    while (listp < hp) {
	hp -= 2;
	List = CONS(hp, THE_NON_VALUE, List);
    }

    /*
     * Now walk through the exeception list and put all values encountered
     * into all positions in the list skeleton that haven't got a value yet.
     */

    while (is_vector_exception(tmp = vp[4])) {
	i = signed_val(vp[2]);
	if (is_non_value(listp[2*i-2])) {
	    listp[2*i-2] = vp[3];
	}
	vp = vector_val(tmp);
    }

    /*
     * Now go through the list skeleton and put values from the vector
     * into all positions that still have no value.
     */

    vp = vector_val(tmp);
    for (i = 0; i < n; i++) {
	if (is_non_value(listp[2*i])) {
	    listp[2*i] = vp[i+1];
	}
    }
    return List;
}


Eterm
vector_get_2(Process* p, Eterm Index, Eterm Vec)
{
    int n;

    if (is_not_small(Index) || ((n = signed_val(Index)) < 1)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    if (is_not_vector(Vec) || signed_val(vector_val(Vec)[1]) < n) {
	goto error;
    }

    while (is_vector_exception(vector_val(Vec)[4])) {
	if (vector_val(Vec)[2] == Index) {
	    return vector_val(Vec)[3];
	}
	Vec = vector_val(Vec)[4];
    }
    return vector_val(vector_val(Vec)[4])[n];
}

Eterm
vector_set_3(Process* p, Eterm Index, Eterm Vec, Eterm New)
{
    int n;
    Eterm* vp;
    Eterm* hp;
    Eterm* tp;
    Eterm res;
    Eterm val;

    if (is_not_small(Index) || ((n = signed_val(Index)) < 1)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    if (is_not_vector(Vec)) {
	goto error;
    }
    ASSERT(is_vector_exception(Vec));
    vp = vector_val(Vec);
    if (signed_val(vp[1]) < n) {
	goto error;
    }

    /*
     * If this is not the most recent version, re-root.
     */

    if (is_vector_exception(vp[4])) {
	erts_ensure_ssb(p);
	Vec = reroot(p, Vec);
    }

    /*
     * At this point, we know that the exception list entry points directly
     * to the array. Conceptually, we add an exception, then re-root.
     * This implementation does the re-rooting on the fly.
     *
     * We don't want to have any heap fragments at this point, so we'll
     * do a GC if there is not enough space left on the heap.
     */

#ifndef HEAP_FRAG_ELIM_TEST
    hp = HAlloc(p, 5);
#else
    if (HEAP_LIMIT(p) - HEAP_TOP(p) <= 5 || MBUF(p) != NULL) {
	Eterm rootset[2];
	rootset[0] = Vec;
	rootset[1] = New;
	(void) erts_garbage_collect(p, 5, rootset, 2);
	Vec = rootset[0];
	New = rootset[1];
    }
    hp = HEAP_TOP(p);
    HEAP_TOP(p) += 5;
#endif

    erts_ensure_ssb(p);
    vp = vector_val(Vec);
    tp = vector_val(vp[4]);
    res = make_vector(hp);
    vp[2] = Index;
    vp[3] = val = tp[n];
    if (is_not_immed(val)) {
	ERTS_SSB_PUT(p, vp+3);
    }
    vp[4] = res;
    ERTS_SSB_PUT(p, vp+4);
    tp[n] = New;
    if (is_not_immed(New)) {
	ERTS_SSB_PUT(p, tp+n);
    }
    hp[0] = HEADER_VECTOR;
    hp[1] = vp[1];
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = make_tuple(tp);

    return res;
}

/*
 * This version of vector:get/2 doesn't check its arguments
 * for validity. The caller must ensure validity.
 */
Eterm
erts_unchecked_vector_get(int index, Eterm Vec)
{
    Eterm Index = make_small(index);

    while (is_vector_exception(vector_val(Vec)[4])) {
	if (vector_val(Vec)[2] == Index) {
	    return vector_val(Vec)[3];
	}
	Vec = vector_val(Vec)[4];
    }
    return vector_val(vector_val(Vec)[4])[index];
}

void
erts_flatten_vector(Eterm* array, Eterm vec)
{
    Eterm* vp;
    int i;
    Eterm tmp;
    int n = VECTOR_SIZE(vec);
    int real_n = VECTOR_ARRAY_SIZE(vec);

    for (i = 0; i <= n; i++) {
	array[i] = THE_NON_VALUE;
    }
    for ( ; i <= real_n; i++) {
	array[i] = NIL;
    }

    vp = vector_val(vec);
    while (is_vector_exception(tmp = vp[4])) {
	if (is_small(vp[2])) {
	    i = signed_val(vp[2]);
	    if (is_non_value(array[i])) {
		array[i] = vp[3];
	    }
	}
	vp = vector_val(tmp);
    }

    vp = vector_val(tmp);
    for (i = 1; i <= n; i++) {
	if (is_non_value(array[i])) {
	    array[i] = vp[i];
	}
    }

#ifdef DEBUG
    for (i = 1; i <= real_n; i++) {
	ASSERT(is_value(array[i]));
    }
#endif
}

Eterm*
erts_copy_vector(Eterm vec, Eterm* hp, Eterm* resp)
{
    Eterm* base;
    int array_len;

    array_len = VECTOR_ARRAY_SIZE(vec);
    base = hp;
    erts_flatten_vector(hp, vec);
    *hp = make_vector_array(array_len);
    hp += array_len+1;
    *resp = make_vector(hp);
    hp[0] = HEADER_VECTOR;
    hp[1] = make_small(VECTOR_SIZE(vec));
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = make_vector(base);
    return hp+5;
}

/*
 * "Re-root" so that the given vector will have the shortest exception list.
 */

static Eterm
reroot(Process* p, Eterm Vec)
{
    Eterm* vp = vector_val(Vec);
    Eterm next = vp[4];
    Eterm val;
    Eterm* r;
    Eterm* tp;
    int i;

    if (is_not_vector_exception(next)) {
	ASSERT(is_vector_array(next));
	return Vec;
    }

    r = vector_val(reroot(p, next));
    tp = vector_val(vp[4] = r[4]);
    r[2] = vp[2];
    i = signed_val(vp[2]);
    r[3] = val = tp[i];
    if (is_not_immed(val)) {
	ERTS_SSB_PUT(p, r+3);
    }
    r[4] = Vec;
    ERTS_SSB_PUT(p, r+4);

    tp[i] = val = vp[3];
    if (is_not_immed(val)) {
	ERTS_SSB_PUT(p, tp+i);
    }
    return Vec;
}
