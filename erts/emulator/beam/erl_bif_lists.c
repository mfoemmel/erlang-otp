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

/* append a list to any object */

BIF_RETTYPE append_2(BIF_ALIST_2)
BIF_ADECL_2
{
     uint32 list;
     uint32 copy;
     uint32 last;
     uint32 need;
     uint32* hp;
     int i;

     if ((i = list_length(BIF_ARG_1)) < 0) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     if (i == 0)
	 BIF_RET(BIF_ARG_2);
     if (is_nil(BIF_ARG_2))
	 BIF_RET(BIF_ARG_1);

     /* XXX HAlloc is MACRO and will use second argument multiple times */
     need = 2*i;
     hp = HAlloc(BIF_P, need); 
     list = BIF_ARG_1;
     copy = last = CONS(hp, CAR(ptr_val(list)), make_list(hp+2));
     list = CDR(ptr_val(list));
     hp += 2;
     i--;
     while(i--) {
	 uint32* listp = ptr_val(list);
	 last = CONS(hp, CAR(listp), make_list(hp+2));
	 list = CDR(listp);
	 hp += 2;
     }
     CDR(ptr_val(last)) = BIF_ARG_2;
     BIF_RET(copy);
 }

BIF_RETTYPE subtract_2(BIF_ALIST_2)
BIF_ADECL_2
{
     uint32  list;
     uint32* hp;
     uint32  need;
     uint32  res;
     uint32  small_vec[10];	/* Preallocated memory for small lists */
     uint32* vec_p;
     uint32* vp;
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
	 vec_p = (uint32*) safe_alloc(n * sizeof(uint32));

     /* PUT ALL ELEMENTS IN VP */
     vp = vec_p;
     list = BIF_ARG_1;
     i = n;
     while(i--) {
	 uint32* listp = ptr_val(list);
	 *vp++ = CAR(listp);
	 list = CDR(listp);
     }

     /* UNMARK ALL DELETED CELLS */
     list = BIF_ARG_2;
     m = 0;  /* number of deleted elements */
     while(is_list(list)) {
	 uint32* listp = ptr_val(list);
	 uint32  elem = CAR(listp);
	 i = n;
	 vp = vec_p;
	 while(i--) {
	     if ((*vp != 0) && eq(*vp, elem)) {
		 *vp = 0;
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
     else { 	 /* REBUILD LIST */
	 res = NIL;
	 need = 2*(n - m);
	 hp = HAlloc(BIF_P, need);
	 vp = vec_p + n - 1;
	 while(vp >= vec_p) {
	     if (*vp != 0) {
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
    int max_iter = 10 * CONTEXT_REDS;
    
    if (is_nil(BIF_ARG_2)) {
	BIF_RET(am_false);
    } else if (is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    term = BIF_ARG_1;
    list = BIF_ARG_2;
    while (is_list(list)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(BIF_P);
	    BIF_TRAP2(bif_export[BIF_lists_member_2], BIF_P, term, list);
	} else if (eq(CAR(ptr_val(list)), term)) {
	    BIF_RET2(am_true, CONTEXT_REDS - max_iter/10);
	}
	list = CDR(ptr_val(list));
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
	BIF_ERROR(BIF_P, BADARG);
    }
    
    list = BIF_ARG_1;
    result = BIF_ARG_2;
    hp = hend = NULL;
    while (is_list(list)) {
	uint32* pair = ptr_val(list);
	if (--max_iter == 0) {
#ifdef DEBUG
	    while (hp < hend) {
		*hp++ = NIL;
	    }
#endif
	    BUMP_ALL_REDS(BIF_P);
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
#ifdef DEBUG
    while (hp < hend) {
	*hp++ = NIL;
    }
#endif
    if (is_not_nil(list))  {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET2(result, CONTEXT_REDS - max_iter / 10);
}

BIF_RETTYPE
lists_keymember_3(Process* p, Eterm Key, Eterm Pos, Eterm List)
{
    int pos;
    int max_iter = 10 * CONTEXT_REDS;
    Eterm term;

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    while (is_list(List)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(bif_export[BIF_lists_keysearch_3], p, Key, Pos, List);
	}
	term = CAR(ptr_val(List));
	if (is_tuple(term)) {
	    Eterm *tuple_ptr = ptr_val(term);
	    if (pos <= arityval(*tuple_ptr)) {
		Eterm element = tuple_ptr[pos];
		if (eq(Key, element)) {
		    return am_true;
		}
	    }
	}
	List = CDR(ptr_val(List));
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

    if (!is_small(Pos) || (pos = signed_val(Pos)) < 1) {
	BIF_ERROR(p, BADARG);
    }

    while (is_list(List)) {
	if (--max_iter < 0) {
	    BUMP_ALL_REDS(p);
	    BIF_TRAP3(bif_export[BIF_lists_keysearch_3], p, Key, Pos, List);
	}
	term = CAR(ptr_val(List));
	if (is_tuple(term)) {
	    Eterm *tuple_ptr = ptr_val(term);
	    if (pos <= arityval(*tuple_ptr)) {
		Eterm element = tuple_ptr[pos];
		if (eq(Key, element)) {
		    Eterm* hp = HAlloc(p, 3);
		    return TUPLE2(hp, am_value, term);
		}
	    }
	}
	List = CDR(ptr_val(List));
    }
    if (is_not_nil(List))  {
	BIF_ERROR(p, BADARG);
    }
    return am_false;
}
