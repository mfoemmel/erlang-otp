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
 * BIFs logically belonging to the lists module.
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

/*
 * erlang:'++'/2
 */

Eterm
erlang_append_2(Process* p, Eterm A, Eterm B)
{
    return append_2(p, A, B);
}

/*
 * erlang:'--'/2
 */

Eterm
erlang_subtract_2(Process* p, Eterm A, Eterm B)
{
    return subtract_2(p, A, B);
}

BIF_RETTYPE append_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm list;
    Eterm copy;
    Eterm last;
    size_t need;
    Eterm* hp;
    int i;

    if ((i = list_length(BIF_ARG_1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (i == 0) {
	BIF_RET(BIF_ARG_2);
    } else if (is_nil(BIF_ARG_2)) {
	BIF_RET(BIF_ARG_1);
    }

    need = 2*i;
    hp = HAlloc(BIF_P, need); 
    list = BIF_ARG_1;
    copy = last = CONS(hp, CAR(list_val(list)), make_list(hp+2));
    list = CDR(list_val(list));
    hp += 2;
    i--;
    while(i--) {
	Eterm* listp = list_val(list);
	last = CONS(hp, CAR(listp), make_list(hp+2));
	list = CDR(listp);
	hp += 2;
    }
    CDR(list_val(last)) = BIF_ARG_2;
    BIF_RET(copy);
}

BIF_RETTYPE subtract_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm  list;
    Eterm* hp;
    Uint  need;
    Eterm  res;
    Eterm  small_vec[10];	/* Preallocated memory for small lists */
    Eterm* vec_p;
    Eterm* vp;
    int     i;
    int     n;
    int     m;
    
    if ((n = list_length(BIF_ARG_1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((m = list_length(BIF_ARG_2)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    if (n == 0)
	BIF_RET(NIL);
    if (m == 0)
	BIF_RET(BIF_ARG_1);
    
    /* allocate element vector */
    if (n <= sizeof(small_vec)/sizeof(small_vec[0]))
	vec_p = small_vec;
    else
	vec_p = (Eterm*) safe_alloc_from(360, n * sizeof(Eterm));
    
    /* PUT ALL ELEMENTS IN VP */
    vp = vec_p;
    list = BIF_ARG_1;
    i = n;
    while(i--) {
	Eterm* listp = list_val(list);
	*vp++ = CAR(listp);
	list = CDR(listp);
    }
    
    /* UNMARK ALL DELETED CELLS */
    list = BIF_ARG_2;
    m = 0;  /* number of deleted elements */
    while(is_list(list)) {
	Eterm* listp = list_val(list);
	Eterm  elem = CAR(listp);
	i = n;
	vp = vec_p;
	while(i--) {
	    if (is_value(*vp) && eq(*vp, elem)) {
		*vp = THE_NON_VALUE;
		m++;
		break;
	    }
	    vp++;
	}
	list = CDR(listp);
    }
    
    if (m == n)      /* All deleted ? */
	res = NIL;
    else if (m == 0)  /* None deleted ? */
	res = BIF_ARG_1;
    else {			/* REBUILD LIST */
	res = NIL;
	need = 2*(n - m);
	hp = HAlloc(BIF_P, need);
	vp = vec_p + n - 1;
	while(vp >= vec_p) {
	    if (is_value(*vp)) {
		res = CONS(hp, *vp, res);
		hp += 2;
	    }
	    vp--;
	}
    }
    if (vec_p != small_vec)
	sys_free(vec_p);
    BIF_RET(res);
}

BIF_RETTYPE lists_member_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm term;
    Eterm list;
    Eterm item;
    int non_immed_key;
    int max_iter = 10 * CONTEXT_REDS;
    
    if (is_nil(BIF_ARG_2)) {
	BIF_RET(am_false);
    } else if (is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    term = BIF_ARG_1;
    non_immed_key = is_not_immed(term);
    list = BIF_ARG_2;
    while (is_list(list)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(BIF_P);
	    BIF_TRAP2(bif_export[BIF_lists_member_2], BIF_P, term, list);
	}
	item = CAR(list_val(list));
	if ((item == term) || (non_immed_key && eq(item, term))) {
	    BIF_RET2(am_true, CONTEXT_REDS - max_iter/10);
	}
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))  {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET2(am_false, CONTEXT_REDS - max_iter/10);
}

BIF_RETTYPE lists_reverse_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm list;
    Eterm result;
    Eterm* hp;
    Eterm* hend;

    int max_iter = CONTEXT_REDS * 10;
    
    if (is_nil(BIF_ARG_1)) {
	BIF_RET(BIF_ARG_2);
    } else if (is_not_list(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    
    list = BIF_ARG_1;
    result = BIF_ARG_2;
    hp = hend = NULL;
    while (is_list(list)) {
	Eterm* pair = list_val(list);
	if (--max_iter == 0) {
	    BUMP_ALL_REDS(BIF_P);
	    HRelease(BIF_P, hp);
	    BIF_TRAP2(bif_export[BIF_lists_reverse_2], BIF_P, list, result);
	}
	if (hp == hend) {
	    hp = HAlloc(BIF_P, 64);
	    hend = hp + 64;
	}
	result = CONS(hp, CAR(pair), result);
	hp += 2;
	list = CDR(pair);
    }
    if (is_not_nil(list))  {
	goto error;
    }
    HRelease(BIF_P, hp);
    BIF_RET2(result, CONTEXT_REDS - max_iter / 10);
}

BIF_RETTYPE
lists_keymember_3(Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int pos;
    int max_iter = 10 * CONTEXT_REDS;
    Eterm term;
    int non_immed_key;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    non_immed_key = is_not_immed(Key);
    while (is_list(List)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(bif_export[BIF_lists_keymember_3], p, Key, Pos, List);
	}
	term = CAR(list_val(List));
	if (is_tuple(term)) {
	    Eterm *tuple_ptr = tuple_val(term);
	    if (pos <= arityval(*tuple_ptr)) {
		Eterm element = tuple_ptr[pos];
		if ((Key == element) || (non_immed_key && eq(Key, element))) {
		    return am_true;
		}
	    }
	}
	List = CDR(list_val(List));
    }
    if (is_not_nil(List))  {
	BIF_ERROR(p, BADARG);
    }
    return am_false;
}

BIF_RETTYPE
lists_keysearch_3(Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int pos;
    int max_iter = 10 * CONTEXT_REDS;
    Eterm term;
    int non_immed_key;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    non_immed_key = is_not_immed(Key);
    while (is_list(List)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(bif_export[BIF_lists_keysearch_3], p, Key, Pos, List);
	}
	term = CAR(list_val(List));
	if (is_tuple(term)) {
	    Eterm *tuple_ptr = tuple_val(term);
	    if (pos <= arityval(*tuple_ptr)) {
		Eterm element = tuple_ptr[pos];
		if ((Key == element) || (non_immed_key && eq(Key, element))) {
		    Eterm* hp = HAlloc(p, 3);
		    return TUPLE2(hp, am_value, term);
		}
	    }
	}
	List = CDR(list_val(List));
    }
    if (is_not_nil(List))  {
	BIF_ERROR(p, BADARG);
    }
    return am_false;
}
