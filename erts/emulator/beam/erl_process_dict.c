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
** Code for process dictionaries.
**
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h" /* Will include erl_process_dict.h */
#include "error.h"
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

/* #define HARDDEBUG */

/*
** Utility macros
*/

/* Error codes from subroutines */
#define PDICT_OK            0
#define PDICT_SYSTEM_LIMIT -1

/* Flags to pd_get_hash */
#define PD_GET_OTHER_PROCESS 1UL

/* Hash constant macros */
#define MAX_HASH            1342177280UL
#define INITIAL_SIZE        10

/* Hash utility macros  */
#define HASH_RANGE(PDict) ((PDict)->homeSize + (PDict)->splitPosition)

#define MAKE_HASH(Term) 				\
((is_small(Term)) ? unsigned_val(Term) :		\
 ((is_atom(Term)) ? 					\
  (atom_tab(unsigned_val(term))->slot.bucket.hvalue) :	\
  make_hash(Term, 0)))


/* Memory allocation macros */
#define PD_ALLOC(Siz) safe_alloc(Siz)
#define PD_FREE(Ptr) sys_free(Ptr)
#define PD_REALLOC(Ptr, Siz) safe_realloc(Ptr, Siz)


#define TCAR(Term) CAR(ptr_val(Term))
#define TCDR(Term) CDR(ptr_val(Term))

/* Array access macro */ 
#define ARRAY_GET(PDict, Index) (((PDict)->size > (Index)) ? \
				 (PDict)->data[Index] : NIL)

/* The tag used by NIL */
#define NIL_TAG ATOM


/*
** Forward decalarations
*/
static int pd_hash_erase(Process *p, Eterm id, Eterm *ret);
static int pd_hash_erase_all(Process *p);
/* NB, this interface is different from other bif implementations,
   this is for speed and the fact that get cannot fail. */
static Eterm pd_hash_get(Process *p, Eterm id); 
static int pd_hash_get_keys(Process *p, Eterm value, Eterm *ret) ;
static int pd_hash_get_all(Process *p, ProcDict *pd, unsigned int flags, Eterm *ret);
static int pd_hash_put(Process *p, Eterm id, Eterm value, Eterm *ret);

static int shrink(Process *p); 
static int grow(Process *p);

static void array_shrink(ProcDict **ppd, unsigned int need);
static Eterm array_put(ProcDict **ppdict, unsigned int ndx, Eterm term);

static unsigned int pd_hash_value(ProcDict *pdict, Eterm term);
static unsigned int next_array_size(unsigned int need);

/*
** Debugging prototypes and macros
*/
#ifdef HARDDEBUG

#include <stdarg.h>

static int hdebugf(char *format, ...);
static char *hdebugf_file = "";
static int hdebugf_line;
#define HDEBUGF(X) ((int) hdebugf_file = __FILE__, hdebugf_line = __LINE__, \
		    hdebugf X)
#ifndef DEBUG
#define DEBUG 1
#endif

#else /* !HARDDEBUG */

#define HDEBUGF(X) /* Nothing */

#endif /* HARDDEBUG (else) */

#ifdef DEBUG 

static void pd_check(ProcDict *pd);
static char* print_pid(Process *p);

#define PD_CHECK(PD) pd_check(PD)

#else /* !DEBUG */

#define PD_CHECK(PD) /* Nothing */

#endif /* DEBUG (else) */

/*
** External interface
*/

/*
** Called from break handler
*/
void dictionary_dump(ProcDict *pd, CIO to)
{
    unsigned int i;
#ifdef DEBUG

    /*PD_CHECK(pd);*/
    if (pd == NULL)
	return;
    erl_printf(to, "(size = %d, used = %d, homeSize = %d, splitPosition = %d, "
	       "numElements = %d)\n", pd->size, pd->used, pd->homeSize, 
	       pd->splitPosition, (unsigned int) pd->numElements);
    for (i = 0; i < HASH_RANGE(pd); ++i) {
	erl_printf(to, "%d: ", i);
	display(ARRAY_GET(pd, i), to);
	erl_printf(to, "\n");
    }

#else /* !DEBUG */

    int written = 0;
    Eterm t;

    erl_printf(to, "[");
    if (pd != NULL) {
	for (i = 0; i < HASH_RANGE(pd); ++i) {
	    t = ARRAY_GET(pd, i);
	    if (is_list(t)) {
		for (; t != NIL; t = TCDR(t)) {
		    if (written)
			erl_printf(to, ",");
		    else
			++written;
		    display(TCAR(t), to);
		}
	    } else if (is_tuple(t)) {
		if (written)
		    erl_printf(to, ",");
		else
		    ++written;
		display(t, to);
	    }
	}
    }
    erl_printf(to, "]");

#endif /* DEBUG (else) */
}

/* 
** Called from process_info
*/
Eterm dictionary_copy(Process *p, ProcDict *pd) 
{
    Eterm ret;
    PD_CHECK(pd);
    if (pd_hash_get_all(p, pd, PD_GET_OTHER_PROCESS, &ret) != PDICT_OK)
	return NIL;
    return ret;
}


/*
** BIF interface
*/
BIF_RETTYPE get_0(BIF_ALIST_0)
BIF_ADECL_0
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    pd_hash_get_all(BIF_P, BIF_P->dictionary, 0UL, &ret); /* Cannot actually 
							     fail */
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    /* The pd_hash_get function can never ever fail, so 
       a simple optimization is to use a simpler interface */
    ret = pd_hash_get(BIF_P, BIF_ARG_1);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_keys_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    if (pd_hash_get_keys(BIF_P, BIF_ARG_1, &ret) != PDICT_OK) {
	PD_CHECK(BIF_P->dictionary);
	BIF_ERROR(BIF_P, BADARG);
    }
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE put_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    if (pd_hash_put(BIF_P, BIF_ARG_1, BIF_ARG_2, &ret) != PDICT_OK) {
	PD_CHECK(BIF_P->dictionary);
	BIF_ERROR(BIF_P, BADARG);
    }
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE erase_0(BIF_ALIST_0)
BIF_ADECL_0
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    pd_hash_get_all(BIF_P, BIF_P->dictionary, 0UL, &ret); /* cannot actually 
							   fail */
    pd_hash_erase_all(BIF_P);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE erase_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    if (pd_hash_erase(BIF_P, BIF_ARG_1, &ret) != PDICT_OK) {
	PD_CHECK(BIF_P->dictionary);
	BIF_ERROR(BIF_P, BADARG);
    }
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

/*
** Debug dictionary interfaces (i.e. $put, $get etc).
** The implementation is kind of sloppy, I switch the dictionary
** to the debug_dictionary and call the usual BIF, then switch
** back. 
*/

BIF_RETTYPE dollar_put_2(BIF_ALIST_2)
BIF_ADECL_2
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = put_2(BIF_P, BIF_ARG_1, BIF_ARG_2);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

BIF_RETTYPE dollar_get_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = get_0(BIF_P);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

BIF_RETTYPE dollar_get_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = get_1(BIF_P, BIF_ARG_1);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

BIF_RETTYPE dollar_get_keys_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = get_keys_1(BIF_P, BIF_ARG_1);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

BIF_RETTYPE dollar_erase_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = erase_0(BIF_P);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

BIF_RETTYPE dollar_erase_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RETTYPE save_ret;
    ProcDict *save_pd = BIF_P->dictionary;
    BIF_P->dictionary = BIF_P->debug_dictionary;
    save_ret = erase_1(BIF_P, BIF_ARG_1);
    BIF_P->debug_dictionary = BIF_P->dictionary;
    BIF_P->dictionary = save_pd;
    BIF_RET(save_ret);
}

/*
** BIF implementations
*/
static int pd_hash_erase(Process *p, Eterm id, Eterm *ret)
{
    unsigned int hval;
    Eterm *hp;
    Eterm old;
    Eterm tmp;
    int i;
    unsigned int range;

    *ret = am_undefined;
    if (p->dictionary == NULL)
	return PDICT_OK;
    hval = pd_hash_value(p->dictionary, id);
    old = ARRAY_GET(p->dictionary, hval);
    switch (tag_val_def(old)) {
    case NIL_TAG:
	break;
    case TUPLE_DEF:
	if (eq(ptr_val(old)[1], id)) {
	    array_put(&(p->dictionary), hval, NIL);
	    --(p->dictionary->numElements);
	    *ret = ptr_val(old)[2];
	}
	break;
    case LIST_DEF:
	/* Find cons cell for identical value */
	i = 0;
	for (tmp = old; tmp != NIL && !eq(ptr_val(TCAR(tmp))[1], id); 
	     tmp = TCDR(tmp))
	    ++i;
	if (tmp != NIL) {
	    Eterm nlist;
	    nlist = TCDR(tmp);
	    hp = HAlloc(p, i*2);
	    while (old != tmp) {
		nlist = CONS(hp, TCAR(old), nlist);
		hp += 2;
		old = TCDR(old);
	    }
	    if (TCDR(nlist) == NIL)
		nlist = TCAR(nlist);
	    array_put(&(p->dictionary), hval, nlist);
	    *ret = ptr_val(TCAR(tmp))[2];
	    --(p->dictionary->numElements);
	}
	break;
    default:
#ifdef DEBUG
	erl_printf(CERR,"Process dictionary for process %s is broken, trying to "
		   "display term found in line %d:\n", print_pid(p),__LINE__);
	display(old,CERR);
	erl_printf(CERR,"\n");
#endif
	erl_exit(1, "Damadged process dictionary found during erase/1.");
    }
    if ((range = HASH_RANGE(p->dictionary)) > INITIAL_SIZE && 
	range / 2  > (p->dictionary->numElements))
	return shrink(p);
    return PDICT_OK;
}

static int pd_hash_erase_all(Process *p)
{
    if (p->dictionary != NULL) {
	PD_FREE(p->dictionary);
	p->dictionary = NULL;
    }
    return PDICT_OK;
}

static Eterm pd_hash_get(Process *p, Eterm id) 
{
    unsigned int hval;
    Eterm tmp;
    ProcDict *pd = p->dictionary;

    if (pd == NULL)
	return am_undefined;
    hval = pd_hash_value(pd, id);
    tmp = ARRAY_GET(pd, hval);
    switch tag_val_def(tmp) {
    case NIL_TAG:
	break;
    case TUPLE_DEF:
	if (eq(ptr_val(tmp)[1], id)) {
	    return ptr_val(tmp)[2];
	}
	break;
    case LIST_DEF:
	for (; tmp != NIL && !eq(ptr_val(TCAR(tmp))[1], id); tmp = TCDR(tmp))
	    ;
	if (tmp != NIL)
	    return ptr_val(TCAR(tmp))[2];
	break;
    default:

#ifdef DEBUG
	erl_printf(CERR,"Process dictionary for process %s is broken, "
		   "trying to "
		   "display term found in line %d:\n", print_pid(p),__LINE__);
	display(tmp,CERR);
	erl_printf(CERR,"\n");
#endif

	erl_exit(1,"Damadged process dictionary foung during get/1.");
    }
    return am_undefined;
}

/*
** I cant help thinking this is a useless interface...
*/
static int pd_hash_get_keys(Process *p, Eterm value, Eterm *ret) 
{
    Eterm *hp;
    Eterm res = NIL;
    ProcDict *pd = p->dictionary;
    unsigned int i, num;
    Eterm tmp, tmp2;

    if (pd == NULL)
	goto done;
    num = HASH_RANGE(pd);
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	switch (tag_val_def(tmp)) {
	case TUPLE_DEF:
	    if (eq(ptr_val(tmp)[2], value)) {
		hp = HAlloc(p, 2);
		res = CONS(hp, ptr_val(tmp)[1], res);
	    }
	    break;
	case LIST_DEF:
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		if (eq(ptr_val(tmp2)[2], value)) {
		    hp = HAlloc(p, 2);
		    res = CONS(hp, ptr_val(tmp2)[1], res);
		}
		tmp = TCDR(tmp);
	    }
	    break;
	default:
	    break;
	}
    }
done:
    *ret = res;
    return PDICT_OK;
}
	

static int pd_hash_get_all(Process *p, ProcDict *pd, unsigned int flags, Eterm *ret)
{
    Eterm *hp, *hp2;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i, num;
    int copy = !!(flags & PD_GET_OTHER_PROCESS);

    if (pd == NULL)
	goto done;
    num = HASH_RANGE(pd);
    hp = HAlloc(p, pd->numElements * 2);

    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	switch (tag_val_def(tmp)) {
	case TUPLE_DEF:
	    if (copy) {
		Uint siz = size_object(tmp);
		hp2 = HAlloc(p, siz);
		tmp = copy_struct(tmp, siz, &hp2, &(p->off_heap));
	    }
	    res = CONS(hp, tmp, res);
	    hp += 2;
	    break;
	case LIST_DEF:
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		if (copy) {
		    Uint siz = size_object(tmp2);
		    hp2 = HAlloc(p, siz);
		    tmp2 = copy_struct(tmp2, siz, &hp2, &(p->off_heap));
		}
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	    break;
	default:
	    break;
	}
    }
done:
    *ret = res;
    return PDICT_OK;
}

static int pd_hash_put(Process *p, Eterm id, Eterm value, Eterm *ret)
{ 
    unsigned int hval;
    Eterm *hp;
    Eterm tpl;
    Eterm old;
    Eterm tmp;
    int i;

    *ret = am_undefined;

    if (p->dictionary == NULL) {
	/* Create it */
	array_put(&(p->dictionary), INITIAL_SIZE - 1, NIL);
	p->dictionary->homeSize = INITIAL_SIZE;
    }	
    hval = pd_hash_value(p->dictionary, id);
    hp = HAlloc(p, 3);
    tpl = TUPLE2(hp, id, value);
    old = ARRAY_GET(p->dictionary, hval);
    switch tag_val_def(old) {
    case NIL_TAG:
	array_put(&(p->dictionary), hval, tpl);
	++(p->dictionary->numElements);
	break;
    case TUPLE_DEF:
	if (eq(ptr_val(old)[1],id)) {
	    array_put(&(p->dictionary), hval, tpl);
	    *ret = ptr_val(old)[2];
	} else {
	    hp = HAlloc(p, 4);
	    tmp = CONS(hp, old, NIL);
	    hp += 2;
	    ++(p->dictionary->numElements);
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, tmp));
	} 
	break;
    case LIST_DEF:
	/* Find cons cell for identical value */
	i = 0;
	for (tmp = old; tmp != NIL && !eq(ptr_val(TCAR(tmp))[1], id); 
	     tmp = TCDR(tmp))
	    ++i;
	if (is_nil(tmp)) {
	    /* Not found */
	    hp = HAlloc(p, 2);
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, old));
	    ++(p->dictionary->numElements);
	} else {
	    /* Remove old value from list */
	    Eterm nlist = TCDR(tmp);
	    Eterm tmp2;
	    hp = HAlloc(p, (i+1)*2);
	    for (tmp2 = old; tmp2 != tmp; tmp2 = TCDR(tmp2)) {
		nlist = CONS(hp, TCAR(tmp2), nlist);
		hp += 2;
	    }
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, nlist));
	    *ret = ptr_val(TCAR(tmp))[2];
	}
	break;
    default:

#ifdef DEBUG
	erl_printf(CERR,"Process dictionary for process %s is broken, trying to "
		   "display term found in line %d:\n", print_pid(p),__LINE__);
	display(old,CERR);
	erl_printf(CERR,"\n");
#endif

	erl_exit(1,"Damadged process dictionary found during put/2.");
    }
    if (HASH_RANGE(p->dictionary) <= p->dictionary->numElements)
	return grow(p);
    return PDICT_OK;
}

/*
** Hash table utilities, rehashing
*/

static int shrink(Process *p) 
{
    unsigned int range = HASH_RANGE(p->dictionary);
    unsigned int steps = (range*3) / 10;
    Eterm hi, lo, tmp;
    unsigned int i,j;
    Eterm *hp;

    if (range - steps < INITIAL_SIZE)
	steps = range - INITIAL_SIZE; 
    
    for (i = 0; i < steps; ++i) {
	ProcDict *pd = p->dictionary;
	if (pd->splitPosition == 0) {
	    pd->homeSize /= 2;
	    pd->splitPosition = pd->homeSize;
	}
	--(pd->splitPosition);
	hi = ARRAY_GET(pd, (pd->splitPosition + pd->homeSize));
	lo = ARRAY_GET(pd, pd->splitPosition);
	if (hi != NIL) {
	    if (lo == NIL) {
		array_put(&(p->dictionary), pd->splitPosition, hi);
	    } else {
		if (is_tuple(lo)) {
		    if (is_tuple(hi)) {
			hp = HAlloc(p, 4);
			tmp = CONS(hp, hi, NIL);
			hp += 2;
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp,lo,tmp));
		    } else { /* hi is a list */
			hp = HAlloc(p, 2);
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp, lo, hi));
		    }
		} else { /* lo is a list */
		    if (is_tuple(hi)) {
			hp = HAlloc(p, 2);
			array_put(&(p->dictionary), pd->splitPosition, 
				  CONS(hp, hi, lo));
		    } else { /* Two lists */
			/* Just select one and count the length */
			j = 0;
			for (tmp = hi; tmp != NIL; tmp = TCDR(tmp))
			    ++j;
			hp = HAlloc(p, j*2);
			for (tmp = hi; tmp != NIL; tmp = TCDR(tmp)) {
			    lo = CONS(hp, TCAR(tmp), lo);
			    hp += 2;
			}
			array_put(&(p->dictionary), pd->splitPosition, lo);
		    }
		}
	    }
	}
	array_put(&(p->dictionary), (pd->splitPosition + pd->homeSize), NIL);
    }
    if (HASH_RANGE(p->dictionary) <= (p->dictionary->size / 4))
	array_shrink(&(p->dictionary), (HASH_RANGE(p->dictionary) * 3) / 2);
    return PDICT_OK;
}

static int grow(Process *p)
{
    unsigned int i,j;
    unsigned int steps = p->dictionary->homeSize / 5;
    Eterm l1,l2;
    Eterm l;
    Eterm *hp;
    unsigned int pos;

    HDEBUGF(("grow: steps = %d", steps));
    if (steps == 0)
	steps = 1;
    /* Dont grow over MAX_HASH */
    if ((MAX_HASH - steps) <= HASH_RANGE(p->dictionary))
	return PDICT_OK;
    for (i = 0; i < steps; ++i) {
	ProcDict *pd = p->dictionary;
	if (pd->splitPosition == pd->homeSize) {
	    pd->homeSize *= 2;
	    pd->splitPosition = 0;
	}
	pos = pd->splitPosition;
	++pd->splitPosition; /* For the hashes */
	l = ARRAY_GET(pd, pos);
	if (is_tuple(l)) {
	    if (pd_hash_value(pd, ptr_val(l)[1]) != pos) {
		array_put(&(p->dictionary), pos + 
			  p->dictionary->homeSize, l);
		array_put(&(p->dictionary), pos, NIL);
	    }
	} else {
	    l2 = NIL;
	    l1 = l;
	    for (j = 0; l1 != NIL; l1 = TCDR(l1))
		++j;
	    hp = HAlloc(p,j*2);
	
	    while (l != NIL) {
		if (pd_hash_value(pd, ptr_val(TCAR(l))[1]) == pos) 
		    l1 = CONS(hp, TCAR(l), l1);
		else
		    l2 = CONS(hp, TCAR(l), l2);
		hp += 2;
		l = TCDR(l);
	    }
	    if (l1 != NIL && TCDR(l1) == NIL)
		l1 = TCAR(l1);
	    if (l2 != NIL && TCDR(l2) == NIL)
		l2 = TCAR(l2);
	    /* After array_put pd is no longer valid */
	    array_put(&(p->dictionary), pos, l1);
	    array_put(&(p->dictionary), pos + 
		      p->dictionary->homeSize, l2);
	}
    }

#ifdef HARDDEBUG
    dictionary_dump(p->dictionary,CERR);
#endif

    return PDICT_OK;
}

/*
** Array oriented operations
*/

static void array_shrink(ProcDict **ppd, unsigned int need) 
{
    unsigned int siz = next_array_size(need);

    HDEBUGF(("array_shrink: size = %d, used = %d, need = %d",
	     (*ppd)->size, (*ppd)->used, need));

    if (siz > (*ppd)->size)
	return; /* Only shrink */

    *ppd = PD_REALLOC(((void *) *ppd), 
		      sizeof(ProcDict) + (siz - 1) * sizeof(Eterm));
    (*ppd)->size = siz;
    if ((*ppd)->size < (*ppd)->used)
	(*ppd)->used = (*ppd)->size;
}
    
			
static Eterm array_put(ProcDict **ppdict, unsigned int ndx, Eterm term)
{
    unsigned int i;
    Eterm ret;
    if (*ppdict == NULL) { 
	Uint siz = next_array_size(ndx+1);
	ProcDict *p;
        p = PD_ALLOC(sizeof(ProcDict) + (siz - 1) * sizeof(Eterm));
	for (i = 0; i < siz; ++i) 
	    p->data[i] = NIL;
	p->size = siz;
	p->homeSize = p->splitPosition = p->numElements = p->used = 0;
	*ppdict = p;
    } else if (ndx >= (*ppdict)->size) {
	Uint osize = (*ppdict)->size;
	Uint nsize = next_array_size(ndx+1);
	*ppdict = PD_REALLOC(((void *) *ppdict), sizeof(ProcDict) + 
			  (nsize - 1) * sizeof(Eterm));
	for (i = osize; i < nsize; ++i)
	    (*ppdict)->data[i] = NIL;
	(*ppdict)->size = nsize;
    }
    ret = (*ppdict)->data[ndx];
    (*ppdict)->data[ndx] = term;
    if ((ndx + 1) > (*ppdict)->used)
	(*ppdict)->used = ndx + 1;
#ifdef HARDDEBUG
    HDEBUGF(("array_put: (*ppdict)->size = %d, (*ppdict)->used = %d, ndx = %d",
	     (*ppdict)->size, (*ppdict)->used, ndx));
    display(term, CERR);
#endif /* HARDDEBUG */
    return ret;
}

/*
** Basic utilities
*/

static unsigned int pd_hash_value(ProcDict *pdict, Eterm term) 
{ 
    Uint hash, high;

    hash = MAKE_HASH(term);
    high = hash % (pdict->homeSize*2);
    if (high >= HASH_RANGE(pdict))
	return hash % pdict->homeSize;
    return high;
}

static unsigned int next_array_size(unsigned int need)
{
    static unsigned int tab[] =
    {
	10UL,
	20UL,
	40UL,
	80UL,
	160UL,
	320UL,
	640UL,
	1280UL,
	2560UL,
	5120UL,
	10240UL,
	20480UL,
	40960UL,
	81920UL,
	163840UL,
	327680UL,
	655360UL,
	1310720UL,
	2621440UL,
	5242880UL,
	10485760UL,
	20971520UL,
	41943040UL,
	83886080UL,
	167772160UL,
	335544320UL,
	671088640UL,
	1342177280UL,
	2684354560UL
    };
    int hi = sizeof(tab) / sizeof(Uint) - 1;
    int lo = 1;
    int cur = 4;

    while (hi >= lo) {
	if (tab[cur] >= need && tab[cur - 1] < need)
	    return tab[cur];
	if (tab[cur] > need)
	    hi = cur - 1;
	else
	    lo = cur + 1;
	cur = (hi + lo) / 2;
    }
    return need;
}


/*
** Debug functions 
*/	    
#ifdef DEBUG

static void pd_check(ProcDict *pd) 
{
    unsigned int i;
    Uint num;
    if (pd == NULL)
	return;
    ASSERT(pd->size >= pd->used);
    ASSERT(HASH_RANGE(pd) <= MAX_HASH);
    for (i = 0, num = 0; i < pd->used; ++i) {
	Eterm t = pd->data[i];
	switch tag_val_def(t) {
	case NIL_TAG:
	    continue;
	case TUPLE_DEF:
	    ++num;
	    ASSERT(arityval(*ptr_val(t)) == 2);
	    continue;
	case LIST_DEF:
	    while (t != NIL) {
		++num;
		ASSERT(is_tuple(TCAR(t)));
		ASSERT(arityval(*(ptr_val(TCAR(t)))) == 2);
		t = TCDR(t);
	    }
	    continue;
	default:
	    erl_exit(1, 
		     "Found tag 0x%08x in process dictionary at position %d",
		     (unsigned long) t, (int) i);
	}
    }
    ASSERT(num == pd->numElements);
    ASSERT(pd->splitPosition <= pd->homeSize);
}



static char*
print_pid(Process *p)
{
    char static buf[64];

    Uint obj = p->id;
    sprintf(buf, "<%ld.%ld.%ld>", get_node(obj), get_number(obj), get_serial(obj));
    return buf;
}

#endif /* DEBUG */


#ifdef HARDDEBUG

static int hdebugf(char *format, ...)
{
    va_list ap;

    va_start(ap, format);
    fprintf(stderr, "DEBUG: %s:%d :", hdebugf_file, hdebugf_line);
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\r\n");
    va_end(ap);
    return 0;
} 

#endif /* HARDDEBUG */

