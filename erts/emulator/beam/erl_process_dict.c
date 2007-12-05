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
 * Code for process dictionaries.
 *
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h" /* Will include erl_process_dict.h */
#include "error.h"
#include "erl_driver.h"
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
  (atom_tab(atom_val(term))->slot.bucket.hvalue) :	\
  make_hash2(Term)))

#define PD_SZ2BYTES(Sz) (sizeof(ProcDict) + ((Sz) - 1)*sizeof(Eterm))

/* Memory allocation macros */
#define PD_ALLOC(Sz)				\
    (ERTS_PROC_MORE_MEM((Sz)),			\
     erts_alloc(ERTS_ALC_T_PROC_DICT, (Sz)))
#define PD_FREE(P, Sz)				\
    (ERTS_PROC_LESS_MEM((Sz)),			\
     erts_free(ERTS_ALC_T_PROC_DICT, (P)))
#define PD_REALLOC(P, OSz, NSz) 		\
    (ERTS_PROC_LESS_MEM((OSz)),			\
     ERTS_PROC_MORE_MEM((NSz)),			\
     erts_realloc(ERTS_ALC_T_PROC_DICT, (P), (NSz)))


#define TCAR(Term) CAR(list_val(Term))
#define TCDR(Term) CDR(list_val(Term))

/* Array access macro */ 
#define ARRAY_GET(PDict, Index) (((PDict)->size > (Index)) ? \
				 (PDict)->data[Index] : NIL)

/*
 * Forward decalarations
 */
static int pd_hash_erase(Process *p, Eterm id, Eterm *ret);
static int pd_hash_erase_all(Process *p);
static int pd_hash_get_keys(Process *p, Eterm value, Eterm *ret) ;
static Eterm pd_hash_get_all(Process *p, ProcDict *pd);
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

#define PD_CHECK(PD) pd_check(PD)

#else /* !DEBUG */

#define PD_CHECK(PD) /* Nothing */

#endif /* DEBUG (else) */

/*
** External interface
*/

/*
 * Called from break handler
 */
void
erts_dictionary_dump(int to, void *to_arg, ProcDict *pd)
{
    unsigned int i;
#ifdef DEBUG

    /*PD_CHECK(pd);*/
    if (pd == NULL)
	return;
    erts_print(to, to_arg, "(size = %d, used = %d, homeSize = %d, "
	       "splitPosition = %d, numElements = %d)\n",
	       pd->size, pd->used, pd->homeSize, 
	       pd->splitPosition, (unsigned int) pd->numElements);
    for (i = 0; i < HASH_RANGE(pd); ++i) {
	erts_print(to, to_arg, "%d: %T\n", i, ARRAY_GET(pd, i));
    }

#else /* !DEBUG */

    int written = 0;
    Eterm t;

    erts_print(to, to_arg, "[");
    if (pd != NULL) {
	for (i = 0; i < HASH_RANGE(pd); ++i) {
	    t = ARRAY_GET(pd, i);
	    if (is_list(t)) {
		for (; t != NIL; t = TCDR(t)) {
		    erts_print(to, to_arg, written++ ? ",%T" : "%T", TCAR(t));
		}
	    } else if (is_tuple(t)) {
		erts_print(to, to_arg, written++ ? ",%T" : "%T", t);
	    }
	}
    }
    erts_print(to, to_arg, "]");

#endif /* DEBUG (else) */
}

void
erts_deep_dictionary_dump(int to, void *to_arg,
			  ProcDict* pd, void (*cb)(int, void *, Eterm))
{
    unsigned int i;
    Eterm t;

    if (pd != NULL) {
	for (i = 0; i < HASH_RANGE(pd); ++i) {
	    t = ARRAY_GET(pd, i);
	    if (is_list(t)) {
		for (; t != NIL; t = TCDR(t)) {
		    (*cb)(to, to_arg, TCAR(t));
		}
	    } else if (is_tuple(t)) {
		(*cb)(to, to_arg, t);
	    }
	}
    }
}

Uint
erts_dicts_mem_size(Process *p)
{
    Uint size = 0;
    if (p->dictionary)
	size += PD_SZ2BYTES(p->dictionary->size);
    return size;
}

void
erts_erase_dicts(Process *p)
{
    if (p->dictionary)
	pd_hash_erase_all(p);
    p->dictionary = NULL;
}

/* 
 * Called from process_info/1,2.
 */
Eterm erts_dictionary_copy(Process *p, ProcDict *pd) 
{
    Eterm* hp;
    Eterm* heap_start;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i, num;

    if (pd == NULL) {
	return res;
    }

    PD_CHECK(pd);
    num = HASH_RANGE(pd);
    heap_start = hp = (Eterm *) erts_alloc(ERTS_ALC_T_TMP,
					   sizeof(Eterm) * pd->numElements * 2);
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    res = CONS(hp, tmp, res);
	    hp += 2;
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	}
    }
    res = copy_object(res, p);
    erts_free(ERTS_ALC_T_TMP, (void *) heap_start);
    return res;
}


/*
** BIF interface
*/
BIF_RETTYPE get_0(BIF_ALIST_0)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_all(BIF_P, BIF_P->dictionary);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_1(BIF_ALIST_1)
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = erts_pd_hash_get(BIF_P, BIF_ARG_1);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE get_keys_1(BIF_ALIST_1)
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
{
    Eterm ret;
    PD_CHECK(BIF_P->dictionary);
    ret = pd_hash_get_all(BIF_P, BIF_P->dictionary);
    pd_hash_erase_all(BIF_P);
    PD_CHECK(BIF_P->dictionary);
    BIF_RET(ret);
}

BIF_RETTYPE erase_1(BIF_ALIST_1)
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
    if (is_boxed(old)) {	/* Tuple */
	ASSERT(is_tuple(old));
	if (EQ(tuple_val(old)[1], id)) {
	    array_put(&(p->dictionary), hval, NIL);
	    --(p->dictionary->numElements);
	    *ret = tuple_val(old)[2];
	}
    } else if (is_list(old)) {
	/* Find cons cell for identical value */
	i = 0;
	for (tmp = old; tmp != NIL && !EQ(tuple_val(TCAR(tmp))[1], id); 
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
	    *ret = tuple_val(TCAR(tmp))[2];
	    --(p->dictionary->numElements);
	}
    } else if (is_not_nil(old)) {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->id, __LINE__, old);
#endif
	erl_exit(1, "Damaged process dictionary found during erase/1.");
    }
    if ((range = HASH_RANGE(p->dictionary)) > INITIAL_SIZE && 
	range / 2  > (p->dictionary->numElements))
	return shrink(p);
    return PDICT_OK;
}

static int pd_hash_erase_all(Process *p)
{
    if (p->dictionary != NULL) {
	PD_FREE(p->dictionary, PD_SZ2BYTES(p->dictionary->size));
	p->dictionary = NULL;
    }
    return PDICT_OK;
}

Eterm erts_pd_hash_get(Process *p, Eterm id) 
{
    unsigned int hval;
    Eterm tmp;
    ProcDict *pd = p->dictionary;

    if (pd == NULL)
	return am_undefined;
    hval = pd_hash_value(pd, id);
    tmp = ARRAY_GET(pd, hval);
    if (is_boxed(tmp)) {	/* Tuple */
	ASSERT(is_tuple(tmp));
	if (EQ(tuple_val(tmp)[1], id)) {
	    return tuple_val(tmp)[2];
	}
    } else if (is_list(tmp)) {
	for (; tmp != NIL && !EQ(tuple_val(TCAR(tmp))[1], id); tmp = TCDR(tmp)) {
	    ;
	}
	if (tmp != NIL) {
	    return tuple_val(TCAR(tmp))[2];
	}
    } else if (is_not_nil(tmp)) {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->id, __LINE__, tmp);
#endif
	erl_exit(1, "Damaged process dictionary found during get/1.");
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
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    if (EQ(tuple_val(tmp)[2], value)) {
		hp = HAlloc(p, 2);
		res = CONS(hp, tuple_val(tmp)[1], res);
	    }
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		if (EQ(tuple_val(tmp2)[2], value)) {
		    hp = HAlloc(p, 2);
		    res = CONS(hp, tuple_val(tmp2)[1], res);
		}
		tmp = TCDR(tmp);
	    }
	}
    }
done:
    *ret = res;
    return PDICT_OK;
}
	

static Eterm
pd_hash_get_all(Process *p, ProcDict *pd)
{
    Eterm* hp;
    Eterm res = NIL;
    Eterm tmp, tmp2;
    unsigned int i, num;

    if (pd == NULL) {
	return res;
    }
    num = HASH_RANGE(pd);
    hp = HAlloc(p, pd->numElements * 2);
    
    for (i = 0; i < num; ++i) {
	tmp = ARRAY_GET(pd, i);
	if (is_boxed(tmp)) {
	    ASSERT(is_tuple(tmp));
	    res = CONS(hp, tmp, res);
	    hp += 2;
	} else if (is_list(tmp)) {
	    while (tmp != NIL) {
		tmp2 = TCAR(tmp);
		res = CONS(hp, tmp2, res);
		hp += 2;
		tmp = TCDR(tmp);
	    }
	}
    }
    return res;
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
    if (is_nil(old)) {
	array_put(&(p->dictionary), hval, tpl);
	++(p->dictionary->numElements);
    } else if (is_boxed(old)) {
	ASSERT(is_tuple(old));
	if (EQ(tuple_val(old)[1],id)) {
	    array_put(&(p->dictionary), hval, tpl);
	    *ret = tuple_val(old)[2];
	} else {
	    hp = HAlloc(p, 4);
	    tmp = CONS(hp, old, NIL);
	    hp += 2;
	    ++(p->dictionary->numElements);
	    array_put(&(p->dictionary), hval, CONS(hp, tpl, tmp));
	}
    } else if (is_list(old)) {
	/* Find cons cell for identical value */
	i = 0;
	for (tmp = old; tmp != NIL && !EQ(tuple_val(TCAR(tmp))[1], id); 
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
	    *ret = tuple_val(TCAR(tmp))[2];
	}
    } else {
#ifdef DEBUG
	erts_fprintf(stderr,
		     "Process dictionary for process %T is broken, trying to "
		     "display term found in line %d:\n"
		     "%T\n", p->id, __LINE__, old);
#endif

	erl_exit(1, "Damaged process dictionary found during put/2.");
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
	    if (pd_hash_value(pd, tuple_val(l)[1]) != pos) {
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
		if (pd_hash_value(pd, tuple_val(TCAR(l))[1]) == pos) 
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
		      PD_SZ2BYTES((*ppd)->size),
		      PD_SZ2BYTES(siz));

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

        p = PD_ALLOC(PD_SZ2BYTES(siz));
	for (i = 0; i < siz; ++i) 
	    p->data[i] = NIL;
	p->size = siz;
	p->homeSize = p->splitPosition = p->numElements = p->used = 0;
	*ppdict = p;
    } else if (ndx >= (*ppdict)->size) {
	Uint osize = (*ppdict)->size;
	Uint nsize = next_array_size(ndx+1);
	*ppdict = PD_REALLOC(((void *) *ppdict),
			     PD_SZ2BYTES(osize),
			     PD_SZ2BYTES(nsize));
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
    erts_fprintf(stderr, "%T", term);
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
	if (is_nil(t)) {
	    continue;
	} else if (is_tuple(t)) {
	    ++num;
	    ASSERT(arityval(*tuple_val(t)) == 2);
	    continue;
	} else if (is_list(t)) {
	    while (t != NIL) {
		++num;
		ASSERT(is_tuple(TCAR(t)));
		ASSERT(arityval(*(tuple_val(TCAR(t)))) == 2);
		t = TCDR(t);
	    }
	    continue;
	} else {
	    erl_exit(1, 
		     "Found tag 0x%08x in process dictionary at position %d",
		     (unsigned long) t, (int) i);
	}
    }
    ASSERT(num == pd->numElements);
    ASSERT(pd->splitPosition <= pd->homeSize);
}

#endif /* DEBUG */


#ifdef HARDDEBUG

static int hdebugf(char *format, ...)
{
    va_list ap;

    erts_fprintf(stderr, "DEBUG: %s:%d :", hdebugf_file, hdebugf_line);
    va_start(ap, format);
    erts_vfprintf(stderr, format, ap);
    va_end(ap);
    erts_fprintf(stderr, "\n");
    return 0;
} 

#endif /* HARDDEBUG */

