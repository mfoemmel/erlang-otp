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
 * The mutable array (pointed to by the last exception entry) is an
 * an ordinary tuple.
 *
 * We re-root the exception list after each update, so that a newly added entry
 * has shortest exception list.
 *
 * We force fullsweep garbage collection after every update.
 * XXX We should force fullsweep only when we have pointers from the old heap
 * to the new heap or modify the GC to handle pointers from the old heap
 * to the new heap.
 *
 * XXX Partial list of features not working (will crasch emulator):
 *  - Term comparisions of whole vectors.
 *  - Taking hash values of vectors.
 *  - term_to_binary(Vector)
 *  - Creating circular terms will cause an infinite loop in term copying
 *    (and perhaps in other places).
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

static Eterm reroot(Process* p, Eterm Vec);

Eterm
vector_new_2(Process* p, Eterm Sz, Eterm Value)
{
    /*
     * XXX Don't allow creation of vectors since you can easily crasch
     * the emulator by using them.
     */
    BIF_ERROR(p, BADARG);
#if 0
    size_t n;
    size_t need;
    Eterm* hp;
    Eterm res;
    Eterm tmp;

    if (is_not_small(Sz) || (n = signed_val(Sz)) < 0) {
	BIF_ERROR(p, BADARG);
    }
    need = n + 1 + 5;
    hp = HAlloc(p, need);
    tmp = make_tuple(hp);
    *hp++ = make_arityval(n);
    while (n--) {
	*hp++ = Value;
    }
    res = make_vector(hp);
    hp[0] = HEADER_VECTOR;
    hp[1] = Sz;
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = tmp;
    return res;
#endif
}

Eterm
vector_from_list_1(Process* p, Eterm list)
{
    /*
     * XXX Don't allow creation of vectors since you can easily crasch
     * the emulator by using them.
     */
    BIF_ERROR(p, BADARG);

#if 0
    Eterm* cons;
    size_t need;
    Eterm* hp;
    Eterm tmp;
    Eterm res;
    int len;

    if ((len = list_length(list)) < 0) {
	BIF_ERROR(p, BADARG);
    }

    need = len + 1 + 5;
    hp = HAlloc(p, need);
    tmp = make_tuple(hp);
    *hp++ = make_arityval(len);
    while (is_list(list)) {
	cons = list_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    res = make_vector(hp);
    hp[0] = HEADER_VECTOR;
    hp[1] = make_small(len);
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = tmp;
    return res;
#endif
}

Eterm
vector_to_list_2(Process* p, Eterm Vec, Eterm List)
{
    Eterm* hp;
    Eterm* tp;
    size_t n;

    if (is_not_vector(Vec)) {
	BIF_ERROR(p, BADARG);
    }
    if (is_not_tuple(vector_val(Vec)[4])) {
	Vec = reroot(p, Vec);
    }
    tp = tuple_val(vector_val(Vec)[4]) + 1;
    n = signed_val(vector_val(Vec)[1]);
    hp = HAlloc(p, 2 * n);
    while (n--) {
	List = CONS(hp, tp[n], List);
	hp += 2;
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

    while (is_not_tuple(vector_val(Vec)[4])) {
	if (vector_val(Vec)[2] == Index) {
	    return vector_val(Vec)[3];
	}
	Vec = vector_val(Vec)[4];
    }
    return tuple_val(vector_val(Vec)[4])[n];
}

Eterm
vector_set_3(Process* p, Eterm Index, Eterm Vec, Eterm New)
{
    int n;
    Eterm* vp;
    Eterm* hp;
    Eterm* tp;
    Eterm res;

    if (is_not_small(Index) || ((n = signed_val(Index)) < 1)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    if (is_not_vector(Vec)) {
	goto error;
    }
    vp = vector_val(Vec);
    if (signed_val(vp[1]) < n) {
	goto error;
    }

    /*
     * If this is not the most recent version, re-root.
     */

    if (is_not_tuple(vp[4])) {
	Vec = reroot(p, Vec);
	vp = vector_val(Vec);
    }

    /*
     * At this point, we know that the exception list entry points directly
     * to the tuple. Conceptually, we add an exception, then re-root.
     * The implementation does the re-rooting on the fly.
     */

    tp = tuple_val(vp[4]);
    hp = HAlloc(p, 5);
    res = make_vector(hp);
    vp[2] = Index;
    vp[3] = tp[n];
    vp[4] = res;
    tp[n] = New;

    hp[0] = HEADER_VECTOR;
    hp[1] = vp[1];
    hp[2] = NIL;
    hp[3] = NIL;
    hp[4] = make_tuple(tp);

    p->flags |= F_NEED_FULLSWEEP;
    return res;
}

/*
 * "Re-root" so that given vector will have the shortest exception list.
 */

static Eterm
reroot(Process* p, Eterm Vec)
{
    Eterm* vp = vector_val(Vec);
    Eterm next = vp[4];
    Eterm* r;
    Eterm* tp;
    int i;

    if (is_tuple(next)) {
	return Vec;
    }

    r = vector_val(reroot(p, next));
    tp = tuple_val(vp[4] = r[4]);
    r[2] = vp[2];
    i = signed_val(vp[2]);
    r[3] = tp[i];
    r[4] = Vec;

    tp[i] = vp[3];

    p->flags |= F_NEED_FULLSWEEP;
    return Vec;
}

