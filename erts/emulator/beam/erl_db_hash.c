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
** Implementation of unordered ETS tables.
** The tables are implemented as linear dynamic hash tables.
*/

/*
#ifdef DEBUG
#define HARDDEBUG 1
#endif
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "export.h"

#include "erl_db_hash.h"

/* 
 *The following symbols can be manipulated to "tune" the linear hash array 
 */
#define BASIC_SIZE 64               /* #words for empty array       */
#define CHAIN_LEN 6                 /* Medium bucket chain len      */


#define SZEXP   8
#define SEGSZ   (1 << SZEXP)
#define SZMASK  ((1 << SZEXP)-1)


#define SEG_LEN         256   /* When growing init segs */
#define SEG_INCREAMENT  128   /* Number of segments to grow */

#define BUCKET(tb, i) (tb)->seg[(i) >> SZEXP][(i) & SZMASK]

/* ix is a NAME parameter :-) */
#define HASH(tb, hval, ix) \
  do { \
     if ((ix = ((hval) & (tb)->szm)) < (tb)->p) \
        ix = (hval) & (((tb)->szm << 1) | 1); \
  } while(0)

#define MAX_HASH 0xEFFFFFFFUL
#define INVALID_HASH 0xFFFFFFFFUL

/* optimised version of make_hash (normal case? atomic key) */
#define MAKE_HASH(term) \
    ((is_atom(term) ? (atom_tab(atom_val(term))->slot.bucket.hvalue) : \
      make_hash(term, 0)) % MAX_HASH)

/* 
 *tplp is an untagged pointer to a tuple we know is large enough 
 *and dth is a pointer to a DbTableHash.   
 */
#define GETKEY(dth, tplp)   (*((tplp) +  (dth)->keypos))

/*
** Forward decl's (static functions)
*/
static HashDbTerm** alloc_seg(void);
static int realloc_counter(HashDbTerm** bp, uint32 sz, 
			   eTerm new_counter, int counterpos);
static HashDbTerm* next(DbTableHash *tb, uint32 *iptr, HashDbTerm *list);
static HashDbTerm* search_list(DbTableHash* tb, eTerm key, 
			       HashValue hval, HashDbTerm *list);
static void shrink(DbTableHash* tb);
static void grow(DbTableHash* tb);
static void free_term(HashDbTerm* p);
static eTerm put_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2);
static HashDbTerm* get_term(HashDbTerm* old, 
			    eTerm obj, HashValue hval);

int fixed_deletion_desc; /* Descriptor for fix_alloc */
static BIF_RETTYPE hash_select_continue(Process *, Eterm, 
					Eterm, Eterm);


/*
** Static variables
*/

Export hash_select_continue_exp;
	

/*
** External interface 
*/

/*
** Table interface routines ie what's called by the bif's 
*/

int db_fixtable_hash(Process *p, DbTableHash *tb, eTerm arg) 
{

    if (arg == am_true) {
	return DB_ERROR_NONE;
    } 
    if (arg != am_false) {
	return DB_ERROR_BADPARAM;
    }
    /* arg == am_false */
    while (tb->fixdel != NULL) {
	FixedDeletion *fx = tb->fixdel;
	int ix = fx->slot;
	HashDbTerm **bp = &BUCKET(tb, ix);
	HashDbTerm *b = *bp;

	tb->fixdel = fx->next;
	fix_free(fixed_deletion_desc, (uint32*) fx);

	while (b != NULL) {
	    if (b->hvalue == INVALID_HASH) {
		*bp = b->next;
		free_term(b);
		tb->nitems--;
		b = *bp;
	    } else {
		bp = &b->next;
		b = b->next;
	    }
	}
    }
    return DB_ERROR_NONE;
}

int db_create_hash(Process *p, DbTableHash *tb)
{
    tb->szm = SZMASK;
    tb->nslots = SEGSZ;
    tb->nactive = SEGSZ;
    tb->p = 0;
    tb->nsegs = 1;
    tb->seg = (HashDbTerm***) sys_alloc_from(54,sizeof(HashDbTerm**));
    tb->seg[0] = alloc_seg();
    tb->fixdel = NULL;
    return DB_ERROR_NONE;
}

int db_first_hash(Process *p, DbTableHash *tb, 
		  eTerm *ret)
{
    int i = 0;
    while (i < tb->nactive) {
	HashDbTerm* list = BUCKET(tb, i);
	if (list != 0) {
	    eTerm key = GETKEY(tb, list->dbterm.tpl);

	    COPY_OBJECT(key, p, ret);
	    return DB_ERROR_NONE;
	}
	i++;
    }
    *ret = db_am_eot;
    return DB_ERROR_NONE;
}

int db_next_hash(Process *p, DbTableHash *tb,
		 eTerm key,
		 eTerm *ret)
{
    HashValue hval;
    uint32 ix;
    HashDbTerm* b1;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if (((b1->hvalue == hval) || b1->hvalue == INVALID_HASH) 
	    && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    HashDbTerm* b2 = next(tb, &ix, b1);
	    if ((tb->status & DB_BAG) || (tb->status & DB_DUPLICATE_BAG)) {
		while (b2 != 0) {
		    uint32 key2 = GETKEY(tb, b2->dbterm.tpl);
		    if (EQ(key, key2)) {
			b2 = next(tb, &ix, b2);
			continue;
		    }
		    break;
		}
	    }
	    if (b2 == 0) {
		*ret = db_am_eot;
		return DB_ERROR_NONE;
	    }
	    else {
		COPY_OBJECT(GETKEY(tb, b2->dbterm.tpl), p, ret);
		return DB_ERROR_NONE;
	    }
	}
	b1 = b1->next;
    }
    return DB_ERROR_BADKEY;
}    

int db_update_counter_hash(Process *p, DbTableHash *tb,
			   eTerm key,
			   eTerm incr, 
			   int counterpos,
			   eTerm *ret)
{
    HashDbTerm* b;
    HashDbTerm** bp;
    int ix;
    uint32 hval;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while (b != 0) {
	if ((b->hvalue == hval) && EQ(key,GETKEY(tb, b->dbterm.tpl)))
	    break;
	bp = &b->next;
	b = *bp;
    }

    if (b == 0) 
	return DB_ERROR_BADKEY;

    if (counterpos <= 0)
	counterpos = tb->keypos + 1;

    return db_do_update_counter(p, (void *) bp, b->dbterm.tpl,
				counterpos, 
				(int (*)(void *, uint32, eTerm, int))
				&realloc_counter, incr, ret);
}

int db_put_hash(Process *proc, DbTableHash *tb,
		eTerm obj,
		eTerm *ret)
{
    HashValue hval;
    int ix;
    eTerm key;
    HashDbTerm** bp;
    HashDbTerm* b;
    HashDbTerm* q;

    key = GETKEY(tb, tuple_val(obj));
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if (((b->hvalue == hval) || b->hvalue == INVALID_HASH)
	    && EQ(key, GETKEY(tb, b->dbterm.tpl))) {
	    if (tb->status & DB_SET) {
		HashDbTerm* bnext = b->next;
		q = get_term(b, obj, hval);
		q->next = bnext;
		q->hvalue = hval; /* In case of INVALID_HASH */
		*bp = q;
		*ret = am_true;
		return DB_ERROR_NONE;
	    }
	    else if (tb->status & DB_BAG) {
		HashDbTerm** tp = bp;
                HashDbTerm* p = b;
		
                if (eq(make_tuple(b->dbterm.tpl), obj)) {
		    b->hvalue = hval;
		    *ret = am_true;
		    return DB_ERROR_NONE;
                }
                bp = &b->next;
                b = b->next;
                while ((b != 0) && 
		       ((b->hvalue == hval) || b->hvalue == INVALID_HASH) && 
                       EQ(key, GETKEY(tb, b->dbterm.tpl))) {
                    if (eq(make_tuple(b->dbterm.tpl), obj)) {
			b->hvalue = hval;
			*ret = am_true;
			return DB_ERROR_NONE;
                    }
                    bp = &b->next;
                    b = b->next;
                }

                q = get_term(NULL, obj, hval);
                q->next = p;
                *tp = q;
		goto Lupdate;
	    }
	    else {  /* if (tb->status & DB_DUPLICATE_BAG) */
		q = get_term(NULL, obj, hval);
		q->next = b;
		*bp = q;
		goto Lupdate;
	    }
	}
	bp = &b->next;
	b = b->next;
    }

    q = get_term(NULL, obj, hval);
    q->next = b;
    *bp = q;

 Lupdate:
    tb->nitems++;

    if ( ((tb->nitems / tb->nactive) > CHAIN_LEN) && 
	((tb->status & DB_FIXED) == 0))
	grow(tb);
    CHECK_TABLES();
    *ret = am_true;
    return DB_ERROR_NONE;
}

int db_get_hash(Process *p, DbTableHash *tb,
		eTerm key,
		eTerm *ret)
{
    HashValue hval;
    int ix;
    HashDbTerm* b1;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    HashDbTerm* b2 = b1->next;
	    eTerm copy;

	    if ((tb->status & DB_BAG) || (tb->status & DB_DUPLICATE_BAG)) {
		while((b2 != 0) && (b2->hvalue == hval) &&
		      EQ(key, GETKEY(tb, b2->dbterm.tpl)))
		    b2 = b2->next;
	    }
	    copy = put_term_list(p, b1, b2);
	    CHECK_TABLES();
	    *ret = copy;
	    return DB_ERROR_NONE;
	}
	b1 = b1->next;
    }
    *ret = NIL;
    return DB_ERROR_NONE;
}

int db_get_element_array(DbTableHash *tb, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret)
{
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    int num = 0;
    
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    if ((tb->status & DB_BAG) || (tb->status & DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;

		while((b2 != 0) && (b2->hvalue == hval) &&
		      EQ(key, GETKEY(tb, b2->dbterm.tpl))) {
		    if (ndex > arityval(b2->dbterm.tpl[0]))
			return DB_ERROR_BADITEM;
		    b2 = b2->next;
		}

		b = b1;
		while(b != b2) {
		    if (num < *num_ret) {
			ret[num++] = b->dbterm.tpl[ndex];
		    } else {
			return DB_ERROR_NONE;
		    }
		    b = b->next;
		}
		*num_ret = num;
		return DB_ERROR_NONE;
	    }
	    else {
		ASSERT(*num_ret > 0);
		ret[0] = b1->dbterm.tpl[ndex];
		*num_ret = 1;
		return DB_ERROR_NONE;
	    }
	}
	b1 = b1->next;
    }
    return DB_ERROR_BADKEY;
}
    
    
int db_get_element_hash(Process *p, DbTableHash *tb, 
			eTerm key,
			int ndex, 
			eTerm *ret)
{
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    eTerm copy;

	    if (ndex > arityval(b1->dbterm.tpl[0]))
		return DB_ERROR_BADITEM;

	    if ((tb->status & DB_BAG) || (tb->status & DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;
		eTerm elem_list = NIL;

		while((b2 != 0) && (b2->hvalue == hval) &&
		      EQ(key, GETKEY(tb, b2->dbterm.tpl))) {
		    if (ndex > arityval(b2->dbterm.tpl[0]))
			return DB_ERROR_BADITEM;
		    b2 = b2->next;
		}

		b = b1;
		while(b != b2) {
		    uint32* hp;
		    uint32 sz = size_object(b->dbterm.tpl[ndex])+2;
		    
		    hp = HAlloc(p, sz);
		    copy = copy_struct(b->dbterm.tpl[ndex], sz-2,
				       &hp, &(p->off_heap));
		    elem_list = CONS(hp, copy, elem_list);
		    hp += 2;
		    b = b->next;
		}
		*ret = elem_list;
		return DB_ERROR_NONE;
	    }
	    else {
		COPY_OBJECT(b1->dbterm.tpl[ndex], p, &copy);
		*ret = copy;
		return DB_ERROR_NONE;
	    }
	}
	b1 = b1->next;
    }
    return DB_ERROR_BADKEY;
}

/*
 * Very internal interface, removes elements of arity two from 
 * BAG. Used for the PID meta table
 */
int db_erase_bag_exact2(DbTableHash *tb, 
			Eterm key,
			Eterm value)
{
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    int found = 0;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    ASSERT(!(tb->status & DB_FIXED));
    ASSERT((tb->status & DB_BAG));

    while(b != 0) {
	if ((b->hvalue == hval) && EQ(key, GETKEY(tb, b->dbterm.tpl))) {
	    found = 1;
	    if ((arityval(b->dbterm.tpl[0]) == 2) && 
		EQ(value, b->dbterm.tpl[2])) {
		*bp = b->next;
		free_term(b);
		tb->nitems--;
		b = *bp;
		break;
	    }
	} else if (found) {
		break;
	}
	bp = &b->next;
	b = b->next;
    }

    if (found && ((tb->nitems / tb->nactive) < CHAIN_LEN))
	shrink(tb);
    return DB_ERROR_NONE;
}
	
/*
** NB, this is for the db_erase/2 bif.
*/
int db_erase_hash(Process *p, DbTableHash *tb, 
		  eTerm key,
		  eTerm *ret)
{
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    int found = 0;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if ((b->hvalue == hval) && EQ(key, GETKEY(tb, b->dbterm.tpl))) {
	    if (tb->status & DB_FIXED) {
		/* Pseudo remove */
		FixedDeletion *fixd = (FixedDeletion *) 
		    fix_alloc(fixed_deletion_desc);
		fixd->slot = ix;
		fixd->next = tb->fixdel;
		tb->fixdel = fixd;
		b->hvalue = INVALID_HASH;
		bp = &b->next;
		b = b->next;
	    } else {
		*bp = b->next;
		free_term(b);
		tb->nitems--;
		b = *bp;
	    }
	    found = 1;
	}
	else {
	    if (found)
		break;
	    bp = &b->next;
	    b = b->next;
	}
    }

    if (found && ((tb->nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->status & DB_FIXED) == 0))
	shrink(tb);

    *ret = am_true;
    return DB_ERROR_NONE;
}    

int db_match_erase_hash(Process *p, DbTableHash *tb, 
			eTerm pattern,
			eTerm *ret)
{
    int      j;
    int      ix;
    HashDbTerm** bp;
    HashDbTerm*  b;
    int      found;
    eTerm    bnd[DB_MATCH_NBIND];
    DbBindings bs;

#define RET_TO_BIF(Term) { *ret = (Term); return DB_ERROR_NONE; }


    found = 0;
    bs.ptr = &bnd[0];
    bs.size = DB_MATCH_NBIND;
    bs.sz = NULL;
    if (is_not_atom(pattern) && pattern != am_Underscore && 
	db_is_variable(pattern) == -1) {
	HashValue   hval;
	eTerm       key;

	key = db_getkey(tb->keypos, pattern);
	if (is_non_value(key))
	    RET_TO_BIF(am_true);  /* Can't possibly match anything */

	if (!db_has_variable(key)) {  /* Bound key */
	    hval = MAKE_HASH(key);
	    HASH(tb, hval, ix);
	    bp = &BUCKET(tb, ix); 
	    b = *bp;

	    while (b != 0) {
		ZEROB(bs);

		if (b->hvalue != INVALID_HASH && 
		    db_do_match(make_tuple(b->dbterm.tpl),pattern,
				&bs)) {
		    if (tb->status & DB_FIXED) {
			/* Pseudo remove */
			FixedDeletion *fixd = (FixedDeletion *) 
			    fix_alloc(fixed_deletion_desc);
			fixd->slot = ix;
			fixd->next = tb->fixdel;
			tb->fixdel = fixd;
			b->hvalue = INVALID_HASH;
			bp = &b->next;
			b = b->next;
		    } else {
			*bp = b->next;
			free_term(b);
			tb->nitems--;
			b = *bp;
		    }
		    found = 1;
		    continue;   /* Might be a bag, we continue until NIL */
		}
		else {
		    if (found && (tb->status & DB_SET)) /* Only done if this */
			break;                          /* is a set.         */
		    bp = &b->next;
		    b = b->next;
		}
	    }
	    if (found && ((tb->nitems / tb->nactive) < CHAIN_LEN) &&
		((tb->status & DB_FIXED) == 0))
		shrink(tb);
	    if (bs.size != DB_MATCH_NBIND) 
		sys_free(bs.ptr);
	    RET_TO_BIF(am_true);
	}
    }

    /* We gotta search the entire table and do the thing */

    for (j= 0; j < tb->nactive; j++) {
	bp = &BUCKET(tb, j);
	if ((b = *bp) == 0)
	    continue;

	while (b != 0) {
	    ZEROB(bs);

	    if (b->hvalue != INVALID_HASH && 
		db_do_match(make_tuple(b->dbterm.tpl),pattern, &bs)) {
		if (tb->status & DB_FIXED) {
		    /* Pseudo remove */
		    FixedDeletion *fixd = (FixedDeletion *)
			fix_alloc(fixed_deletion_desc);
		    fixd->slot = j;
		    fixd->next = tb->fixdel;
		    tb->fixdel = fixd;
		    b->hvalue = INVALID_HASH;
		    bp = &b->next;
		    b = b->next;
		} else {
		    *bp = b->next;
		    free_term(b);
		    tb->nitems--;
		    b = *bp;
		}
		found = 1;
		continue;   /* Might be a bag, we continue until NIL */
	    }
	    else {
		bp = &b->next;
		b = b->next;
	    }
	}
    }
    if (found && ((tb->nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->status & DB_FIXED) == 0))
	shrink(tb);

    if (bs.size != DB_MATCH_NBIND) 
	sys_free(bs.ptr);
    RET_TO_BIF(am_true);
#undef RET_TO_BIF
}

int db_slot_hash(Process *p, DbTableHash *tb, 
		  eTerm slot_term,
		  eTerm *ret)
{
    int slot;

    if (is_not_small(slot_term) ||
	((slot = signed_val(slot_term)) < 0) ||
	(slot > tb->nactive))
	return DB_ERROR_BADPARAM;
    
    if (slot == tb->nactive) {
	*ret = db_am_eot;
	return DB_ERROR_NONE;
    }

    *ret = put_term_list(p, BUCKET(tb, slot), 0);

    return DB_ERROR_NONE;
}

int db_match_hash(Process *p, DbTableHash *tb, 
		  eTerm pattern,
		  eTerm *ret)
{
    int i;
    uint32 chain_pos;
    uint32 key_given;
    eTerm key = NIL;		/* suppress use-before-set warning */
    HashDbTerm* list = NULL;	/* suppress use-before-set warning */
    HashValue hval = NIL;	/* suppress use-before-set warning */
    eTerm bnd[DB_MATCH_NBIND];
    Uint size_bnd[DB_MATCH_NBIND];
    DbBindings bs;
    eTerm match_list;

#define RET_TO_BIF(Term) { *ret = (Term); return DB_ERROR_NONE; }

    bs.size = DB_MATCH_NBIND;  /* Initialize bind structure */
    bs.ptr = &bnd[0];
    bs.sz = &size_bnd[0];

    if (pattern == am_Underscore || db_is_variable(pattern) != -1)
	key_given = 0;
    else {
	key = db_getkey(tb->keypos, pattern);
	if (is_non_value(key))
	    RET_TO_BIF(NIL);  /* can't possibly match anything */
	if (!db_has_variable(key)) {   /* Bound key */
	    int ix;
	    hval = MAKE_HASH(key);
	    HASH(tb, hval, ix);
	    
	    if ((list = search_list(tb, key, hval, BUCKET(tb, ix))) == 0)
		RET_TO_BIF(NIL);
	    key_given = 1;
	}
	else
	    key_given = 0;
    }

    if (!key_given) {
	/* Run this code if pattern is variable or GETKEY(pattern)  */
	/* is a variable                                            */
	for(chain_pos = 0; chain_pos < tb->nactive; chain_pos++) {
	    if ((list = BUCKET(tb,chain_pos)) != 0)
		break;
	}
	if (list == 0)
	    RET_TO_BIF(NIL);
    }

    match_list = NIL;

    while(1) {
	ZEROB(bs);

	if (list->hvalue != INVALID_HASH && 
	    db_do_match(make_tuple(list->dbterm.tpl), 
			pattern, &bs) != 0) {
	    uint32 sz = 2;
	    eTerm binding_list;
	    eTerm *hp;

	    for (i = 0; i < bs.size; i++) {
		if (is_value(bs.ptr[i])) {
		    bs.sz[i] = size_object(bs.ptr[i]);
		    sz += bs.sz[i] + 2;
		}
	    }
	    
	    hp = HAlloc(p, sz);
	    binding_list = NIL;

            for (i = bs.size - 1; i >= 0; i--) {
                if (is_value(bs.ptr[i])) {
                    eTerm bound = copy_struct(bs.ptr[i], 
					      bs.sz[i],
					      &hp, &(p->off_heap));
                    binding_list = CONS(hp, bound, binding_list);
                    hp += 2;
                }
            }
            match_list = CONS(hp, binding_list, match_list);
	    hp += 2;
	}

	/* Update the list variable */
        if (key_given) {  /* Key is bound */
	    list = list->next;
	    while (list != NULL && list->hvalue == INVALID_HASH)
		list = list->next;
            if (list == 0 || (hval != list->hvalue) || 
		!EQ(key, GETKEY(tb,list->dbterm.tpl)))
                break;
        }
        else { /* Key is variable */
            if ((list = next(tb, &chain_pos, list)) == 0)
                break;
        }
    }

    /* Possibly free space malloced by match()  */
    if (bs.size != DB_MATCH_NBIND) {
	sys_free(bs.ptr);
	sys_free(bs.sz);
    }

    RET_TO_BIF(match_list);

#undef RET_TO_BIF

}

/*
** This is a BIF, i.e. It is called directly from the 
** emulator loop, but cannot be called without a trap,
** so even though it's all erlang terms, the parameters
** need not be checked.
*/
static BIF_RETTYPE hash_select_continue(Process *p, Eterm accum, 
					Eterm tabinfo, 
					Eterm progbin)
{
    Eterm tabname = tuple_val(tabinfo)[2];
    uint32 chain_pos = unsigned_val(tuple_val(tabinfo)[1]);
    uint32 save_chain_pos;
    int all_objects = (int) unsigned_val(tuple_val(tabinfo)[3]);
    Binary *mp = ((ProcBin *) binary_val(progbin))->val;
    DbTable *ttb;
    DbTableHash *tb;
    int num_left = 1000;
    HashDbTerm *current_list;
    Eterm match_list = accum;
    Uint32 dummy;
    unsigned sz;
    Eterm *hp;
    Eterm ntabinfo;
    Eterm match_res;

    if ((ttb = db_get_table(p, tabname, DB_READ)) == NULL) {
	BIF_RET(accum);
    }
    
    tb = &(ttb->hash);
    
    current_list = BUCKET(tb,chain_pos);
    
    for(;;) {
	if (current_list->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mp,
			   make_tuple(current_list->dbterm.tpl),
			   0,&dummy),
	     is_value(match_res))) {
	    if (all_objects) {
		hp = HAlloc(p, current_list->dbterm.size + 2);
		match_res = copy_shallow(current_list->dbterm.v,
					 current_list->dbterm.size,
					 &hp,
					 &p->off_heap);
	    } else {
		sz = size_object(match_res);
	    
		hp = HAlloc(p, sz + 2);
		match_res = copy_struct(match_res, sz, 
					&hp, &(p->off_heap));
	    }
            match_list = CONS(hp, match_res, match_list);
	}
	--num_left;
	save_chain_pos = chain_pos;
	if ((current_list = 
	     next(tb, &chain_pos, current_list)) == 0) {
	    break;
	}
	if (chain_pos != save_chain_pos && num_left <= 0) {
	    goto trap;
	}
    }
    BUMP_ALL_REDS(p);
    BIF_RET(match_list);
trap:
    hp = HAlloc(p, 4);
    ntabinfo = TUPLE3(hp, make_small(chain_pos), tabname, 
		      make_small(all_objects)); 
    BUMP_ALL_REDS(p);
    BIF_TRAP3(&hash_select_continue_exp, p, 
	      match_list, ntabinfo, progbin);
}

static BIF_RETTYPE bif_trap_3(Export *bif,
			      Process *p, 
			      Eterm p1, Eterm p2, Eterm p3) 
{
    BIF_TRAP3(bif, p, p1, p2, p3);
}
    
int db_select_hash(Process *p, DbTableHash *tb, 
		   eTerm pattern,
		   eTerm *ret)
{
    uint32 chain_pos;
    uint32 key_given;
    uint32 save_chain_pos;
    Eterm key = NIL;	       
    HashDbTerm *dlists[10];
    HashDbTerm **lists = dlists;
    unsigned num_lists = 0;
    HashDbTerm *current_list = 0;
    unsigned current_list_pos = 0;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    HashValue hval = NIL;      
    Eterm match_list;
    Binary *mp = NULL;
    Uint32 dummy;
    Eterm match_res;
    unsigned sz;
    Eterm *hp, *ptpl;
    Eterm lst,tpl, ttpl;
    int something_can_match;
    int num_heads = 0;
    int i;
    int all_objects;
    int num_left = 1000;
    ProcBin *pb;
    Eterm tabinfo;

#define RET_TO_BIF(Term,RetVal) do { 			\
	if (mp != NULL) {			\
	    erts_match_set_free(mp);		\
	}					\
	if (lists != dlists) {                  \
	    sys_free(lists);                    \
	}                                       \
	if (buff != sbuff) {                    \
	    sys_free(buff);                     \
	}                                       \
	*ret = (Term); 				\
	return RetVal; 			\
    } while(0)


    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    }

    if (num_heads > 10) {
	buff = safe_alloc(sizeof(Eterm) * num_heads * 3);
	lists = safe_alloc(sizeof(*lists) *
			   num_heads);
	
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    key_given = 1;
    something_can_match = 0;
    all_objects = 1;
    i = 0;
    for(lst = pattern; is_list(lst); lst = CDR(list_val(lst))) {
	Eterm body;
	ttpl = CAR(list_val(lst));
	if (!is_tuple(ttpl)) {
	    RET_TO_BIF(NIL, DB_ERROR_BADPARAM);
	}
	ptpl = tuple_val(ttpl);
	if (ptpl[0] != make_arityval(3U)) {
	    RET_TO_BIF(NIL, DB_ERROR_BADPARAM);
	}
	matches[i] = tpl = ptpl[1];
	guards[i] = ptpl[2];
	bodies[i] = body = ptpl[3];
	if (!is_list(body) || CDR(list_val(body)) != NIL ||
	    CAR(list_val(body)) != am_DollarUnderscore) {
	    all_objects = 0;
	}
	++i;
	if (!key_given) {
	    continue;
	}
	if (tpl == am_Underscore || db_is_variable(tpl) != -1) {
	    key_given = 0;
	    something_can_match = 1;
	} else {
	    key = db_getkey(tb->keypos, tpl);
	    if (is_value(key)) {
		if (!db_has_variable(key)) {   /* Bound key */
		    int ix;
		    HashDbTerm *tmp;
		    hval = MAKE_HASH(key);
		    HASH(tb, hval, ix);
		    tmp = BUCKET(tb,ix);
		    if (search_list(tb, key, hval, 
				    BUCKET(tb, ix)) != 0) {
			int j;
			for (j = 0; j < num_lists && 
				 lists[j] != tmp; ++j)
			    ;
			if (j == num_lists) {
			    lists[num_lists++] = tmp;
			}
			something_can_match = 1;
		    }
		} else {
		    key_given = 0;
		    something_can_match = 1;
		}
	    }
	}
    }
    if (!something_can_match) {
	RET_TO_BIF(NIL,DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    if ((mp = db_match_compile(matches, guards, bodies,
			       num_heads, DCOMP_BODY_RETURN, NULL)) 
	== NULL) {
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    }
    

    if (!key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
	for(chain_pos = 0; chain_pos < tb->nactive; chain_pos++) {
	    if ((current_list = BUCKET(tb,chain_pos)) != 0)
		break;
	}
	if (current_list == 0) {
	    RET_TO_BIF(NIL,DB_ERROR_NONE);
	} else {
	    BUMP_ALL_REDS(p);
	}
    } else {
	/* We have at least one */
	current_list = lists[current_list_pos++]; 
    }

    match_list = NIL;

    for(;;) {
	if (current_list->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mp,
			   make_tuple(current_list->dbterm.tpl),
			   0,&dummy),
	     is_value(match_res))) {
	    if (all_objects) {
		hp = HAlloc(p, current_list->dbterm.size + 2);
		match_res = copy_shallow(current_list->dbterm.v,
					 current_list->dbterm.size,
					 &hp,
					 &p->off_heap);
	    } else {
		sz = size_object(match_res);
	    
		hp = HAlloc(p, sz + 2);
		match_res = copy_struct(match_res, sz, 
					&hp, &(p->off_heap));
	    }
            match_list = CONS(hp, match_res, match_list);
	}

	/* Update the list variable */
        if (key_given) {  /* Key is bound */
	    current_list = current_list->next;
	    for (;;) {
		while (current_list != NULL && 
		       current_list->hvalue == INVALID_HASH)
		    current_list = current_list->next;
		if (current_list == NULL) {
		    if (current_list_pos == num_lists) {
			goto done;
		    } else {
			current_list = lists[current_list_pos++];
		    }
		} else {
		    break;
		}
	    }
        }
        else { /* Key is variable */
	    --num_left;
	    save_chain_pos = chain_pos;
            if ((current_list = 
		 next(tb, &chain_pos, current_list)) == 0) {
                break;
	    }
	    if (chain_pos != save_chain_pos && num_left <= 0) {
		goto trap;
	    }
        }
    }
done:
    RET_TO_BIF(match_list,DB_ERROR_NONE);
trap:
    mp->refc++;
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = p->off_heap.mso;
    p->off_heap.mso = pb;
    pb->val = mp;
    pb->bytes = mp->orig_bytes;
    mp = NULL; /* Don't free it */
    hp = HAlloc(p, 4);
    tabinfo = TUPLE3(hp, make_small(chain_pos), tb->id, 
		     make_small(all_objects)); 
    RET_TO_BIF(bif_trap_3(&hash_select_continue_exp, p, 
			  match_list, tabinfo, make_binary(pb)), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}



int db_match_object_hash(Process *p, DbTableHash *tb, 
			 eTerm pattern,
			 eTerm state,
			 eTerm *ret)
{
    uint32 chain_pos;
    uint32 key_given;
    eTerm key = NIL;		/* suppress use-before-set warning */
    HashDbTerm* list = NULL;	/* suppress use-before-set warning */
    HashValue hval = NIL;	/* suppress use-before-set warning */
    eTerm bnd[DB_MATCH_NBIND];
    DbBindings bs;
    eTerm match_list = NIL;	/* Resulting match list. */
    uint32 records;		/* Records done so far. */
    uint32 max_counter;		/* Maximum number of slots to search. */
    eTerm result;

#define RET_TO_BIF(Term) { *ret = (Term); return DB_ERROR_NONE; }

    /*
     * Get the third argument, which must either be the maximum number of
     * records to look at in each invocation of this BIF, or a tuple
     * containing the wrapped up status from a previous invocation.
     * This tuple has three elements: {Matches, ChainNumber, Max},
     * where Matches is a list of Matches found earlier, ChainNumber is
     * the number of the next hash chain to look in, and Max is the
     * the maximum number of records to look at before returning.
     *
     * Note that the Max parameter is not exact; we will finish the
     * current chain before returning.
     */

    if (is_small(state)) {
	max_counter = signed_val(state);
	chain_pos = 0;
    } else if (is_tuple(state)) {
	eTerm *tupleptr = tuple_val(state);
	if (arityval(*tupleptr) != 3)
	    return DB_ERROR_BADPARAM;
	match_list = tupleptr[1];
	if (is_not_list(match_list) && is_not_nil(match_list))
	    return DB_ERROR_BADPARAM;
	if (!is_small(tupleptr[2]))
	    return DB_ERROR_BADPARAM;
	chain_pos = signed_val(tupleptr[2]);
	if (chain_pos >= tb->nactive) {
	    /*
	     * Well, probably another process deleted this hash chain.
	     * Return what we have.
	     */

	    RET_TO_BIF(match_list);
	}
	if (!is_small(tupleptr[3]))
	    return DB_ERROR_BADPARAM;
	max_counter = signed_val(tupleptr[3]);
    } else {
	return DB_ERROR_BADPARAM;
    }
    
    if (max_counter < 1)
	return DB_ERROR_BADPARAM;

    /*
     * Find out whether the pattern has a constant key in
     * the key position of the tuple or not.
     */
    
    if (pattern == am_Underscore || db_is_variable(pattern) != -1) {
	key_given = 0;
    } else if (key = db_getkey(tb->keypos, pattern), is_non_value(key)) {
	RET_TO_BIF(match_list);
    } else {
	key_given = !db_has_variable(key);
    }

    /*
     * Locate the first chain to search in.
     */

    if (key_given) {
	int ix;

	hval = MAKE_HASH(key);
	HASH(tb, hval, ix);
	list = search_list(tb, key, hval, BUCKET(tb, ix));
    } else {
	BUMP_ALL_REDS(p);
	for ( ; chain_pos < tb->nactive; chain_pos++) {
	    if ((list = BUCKET(tb, chain_pos)) != 0)
		break;
	}
    }

    if (list == 0)
	RET_TO_BIF(match_list);

    /*
     * Now start matching with all records in the chain given by list.
     */
    bs.size = DB_MATCH_NBIND;
    bs.ptr = &bnd[0];
    bs.sz = NULL;
    records = 0;
    for (;;) {
	eTerm term = make_tuple(list->dbterm.tpl);
	ZEROB(bs);

	/*
	 * See if this record matches, and if so, cons it to the
	 * list of matches.
	 */

	if (list->hvalue != INVALID_HASH &&
	    db_do_match(term, pattern, &bs) != 0) {
	    eTerm copy;
	    eTerm *hp;

	    hp = HAlloc(p, list->dbterm.size+2);
	    copy = copy_struct(term, list->dbterm.size, &hp, &(p->off_heap));
            match_list = CONS(hp, copy, match_list);
            hp += 2;
	}

	/*
	 * Point to the next record in the table.
	 */

	records++;
	list = list->next;
	while (list != NULL && list->hvalue == INVALID_HASH)
	    list = list->next;
        if (key_given) {
	    /*
	     * When given a key, we will collect all records
	     * before returning (they must be in the same chain).
	     */
            if (list == 0 || (hval != list->hvalue) ||
		!EQ(key, GETKEY(tb, list->dbterm.tpl))) {
		result = match_list;
                goto return_result;
	    }
        } else if (list == 0) {
	    /*
	     * We have reached the end of this chain.  Find the next
	     * non-empty chain.  We are done if none left.
	     */
	    do {
		chain_pos++;
		if (chain_pos >= tb->nactive) {
		    /*
		     * No chain left. Done.
		     */
		    result = match_list;
		    goto return_result;
		}
	    } while ((list = BUCKET(tb, chain_pos)) == NULL);

	    /*
	     * We have the pointer to the next chain.  But if we have already
	     * looked at enough records, we must save the state and return
	     * from the BIF.
	     */

	    if (records >= max_counter) {
		eTerm *hp = HAlloc(p, 4);
		
		result = TUPLE3(hp, match_list, make_small(chain_pos),
				make_small(max_counter));
		goto return_result;
	    }
	}
    }

 return_result:
    if (bs.size != DB_MATCH_NBIND) 
	sys_free(bs.ptr);

    RET_TO_BIF(result);
#undef RET_TO_BIF
}


/*
** Other interface routines (not directly coupled to one bif)
*/

void db_initialize_hash(void) {
    extern Eterm* em_apply_bif;
    fixed_deletion_desc = new_fix_size(sizeof(FixedDeletion));
    memset(&hash_select_continue_exp, 0, sizeof(Export));
    hash_select_continue_exp.address = 
	&hash_select_continue_exp.code[3];
    hash_select_continue_exp.code[0] = am_ets;
    hash_select_continue_exp.code[1] = am_select;
    hash_select_continue_exp.code[2] = 3;
    hash_select_continue_exp.code[3] =
	(Eterm) em_apply_bif;
    hash_select_continue_exp.code[4] = 
	(Eterm) &hash_select_continue;
};

/* Get the memory consumption for a hash table */
int  db_info_memory_hash(Process *p, DbTableHash *tb,
			 eTerm *ret, 
			 int *reds)
{
    HashDbTerm* list;
    int tot = 0;
    int i;

    for (i = 0; i < tb->nactive; i++) {
	list = BUCKET(tb,i);
	while(list != 0) {
	    tot += (sizeof(HashDbTerm)/sizeof(eTerm)) + (list->dbterm.size-1);
	    list = list->next;
	}
    }
    tot += sizeof (DbTable) / sizeof (eTerm);
    tot += ((tb->nactive + SEGSZ - 1) / SEGSZ) * SEGSZ;
    tot += tb->nsegs;
    *ret = make_small(tot);
    *reds = (tb->nitems+1)/20;
    return DB_ERROR_NONE;
}

/* Display hash table contents (for dump) */
void db_print_hash(CIO fd, int show, DbTableHash *tb, int *sum)
{
    int i;
    
    *sum = 0;

    erl_printf(fd, "Buckets: %d ", tb->nactive);
    erl_printf(fd, "\n");

    for (i = 0; i < tb->nactive; i++) {
	HashDbTerm* list = BUCKET(tb,i);
	if (list == NULL)
	   continue;
	if (show)
	    erl_printf(fd,"%d: [", i);
	while(list != 0) {
	    *sum += list->dbterm.size;
	    if (show) {
		if (list->hvalue == INVALID_HASH)
		    erl_printf(fd,"*");
		display(make_tuple(list->dbterm.tpl), fd);
		if (list->next != 0)
		    erl_printf(fd, ",");
	    }
	    list = list->next;
	}
	if (show) 
	    erl_printf(fd, "]\n");
    }
}    

/* release all memory occupied by a single table */
void free_hash_table(DbTableHash *tb)
{
    HashDbTerm*** sp = tb->seg;
    int n = tb->nsegs;

    while (tb->fixdel != NULL) {
	FixedDeletion *fx = tb->fixdel;
	tb->fixdel = fx->next;
	fix_free(fixed_deletion_desc, (uint32 *) fx);
    }
    while(n--) {
	HashDbTerm** bp = *sp;
	if (bp != 0) {
	    int m = SEGSZ;

	    while(m--) {
		HashDbTerm* p = *bp++;

		while(p != 0) {
		    HashDbTerm* nxt = p->next;
		    free_term(p);
		    p = nxt;
		}
	    }
	    sys_free(*sp);
	}
	sp++;
    }
    sys_free(tb->seg);
}



/*
** Utility routines. (static)
*/
static HashDbTerm** alloc_seg(void)
{
    HashDbTerm** bp;
    int sz = sizeof(HashDbTerm*)*SEGSZ;

    if ((bp = (HashDbTerm**) sys_alloc_from(51,sz)) == NULL)
	return NULL;
    memset(bp, 0, sz);
    return bp;
}


static HashDbTerm* get_term(HashDbTerm* old, 
			    eTerm obj, HashValue hval) {
    HashDbTerm* p = db_get_term((old != NULL) ? &(old->dbterm) : NULL, 
				((char *) &(old->dbterm)) - ((char *) old),
				obj);
    p->hvalue = hval;
    /*p->next = NULL;*/ /*No Need */
    return p;
}


/*
** Copy terms from ptr1 until ptr2
** works for ptr1 == ptr2 == 0  => []
** or ptr2 == 0
*/
static eTerm put_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2)
{
    int sz = 0;
    HashDbTerm* ptr;
    eTerm list = NIL;
    eTerm copy;
    eTerm *hp;

    ptr = ptr1;
    while(ptr != ptr2) {

	if (ptr->hvalue != INVALID_HASH)
	    sz += ptr->dbterm.size + 2;

	ptr = ptr->next;
    }

    hp = HAlloc(p, sz);

    ptr = ptr1;
    while(ptr != ptr2) {
	if (ptr->hvalue != INVALID_HASH) {
	    copy = copy_shallow(ptr->dbterm.v, ptr->dbterm.size, &hp, &p->off_heap);
	    list = CONS(hp, copy, list);
	    hp  += 2;
	}
	ptr = ptr->next;
    }
    return list;
}

static void free_term(HashDbTerm* p)
{

    db_free_term_data(&(p->dbterm));
    sys_free(p);
}


static void grow(DbTableHash* tb)
{
    HashDbTerm** bp;
    HashDbTerm** bps;
    HashDbTerm* b;
    int ix;
    int nszm = (tb->szm << 1) | 1;

    /* Ensure that that the slot nactive exists */
    if (tb->nactive >= tb->nslots) {
	/* Time to get a new array */    
	if ((tb->nactive & SZMASK) == 0) {
	    int nxt = tb->nactive >> SZEXP;
	    HashDbTerm** new_segment = alloc_seg();
	    HashDbTerm*** new_seg;

	    if (new_segment == NULL)
		return;

	    if (nxt == tb->nsegs) {
		int i, sz;

		if (tb->nsegs == 1)
		    sz = SEG_LEN;
		else
		    sz = tb->nsegs + SEG_INCREAMENT;
		new_seg = (HashDbTerm***) sys_realloc(tb->seg,
						  sizeof(HashDbTerm**)*sz);
		if (new_seg == NULL) {
		    sys_free(new_segment);
		    return;
		}

		tb->seg = new_seg;
		tb->nsegs = sz;
		for (i = nxt+1; i < sz; i++)
		    tb->seg[i] = 0;
	    }
	    tb->seg[nxt] = new_segment;
	    tb->nslots += SEGSZ;
	}
    }

    ix = tb->p;
    bp = &BUCKET(tb, ix);
    ix += (tb->szm+1);
    bps = &BUCKET(tb, ix);
    b = *bp;

    while (b != 0) {
	ix = b->hvalue & nszm;

	if (ix == tb->p)
	    bp = &b->next;
	else {
	    *bp = b->next;  	    /* unlink */
	    b->next = *bps;         /* link */
	    *bps = b;
	}
	b = *bp;
    }

    tb->nactive++;
    if (tb->p == tb->szm) {
	tb->p = 0;
	tb->szm = nszm;
    }
    else
	tb->p++;
}


/*
** Shrink the hash table
** Remove segments if they are empty
** but do not reallocate the segment index table !!!
*/
static void shrink(DbTableHash* tb)
{
    HashDbTerm** bp;

    if (tb->nactive == SEGSZ)
	return;

    tb->nactive--;
    if (tb->p == 0) {
	tb->szm >>= 1;
	tb->p = tb->szm;
    }
    else
	tb->p--;

    bp = &BUCKET(tb, tb->p);
    while(*bp != 0) bp = &(*bp)->next;

    *bp = BUCKET(tb, tb->nactive);
    BUCKET(tb, tb->nactive) = 0;

    if ((tb->nactive & SZMASK) == SZMASK) {
	int six = (tb->nactive >> SZEXP)+1;

	sys_free(tb->seg[six]);
	tb->seg[six] = 0;
	tb->nslots -= SEGSZ;
    }
}


/* Search a list of tuples for a matching key */

static HashDbTerm* search_list(DbTableHash* tb, eTerm key, 
			       HashValue hval, HashDbTerm *list)
{
    while (list != 0) {
	if ((list->hvalue == hval) && EQ(key, GETKEY(tb, list->dbterm.tpl)))
	    return list;
	list = list->next;
    }
    return 0;
}


/* This function is called by the next AND the match BIF */
/* It return the next object in a table                   */

static HashDbTerm* next(DbTableHash *tb, uint32 *iptr, HashDbTerm *list)
{
    int i;

    list = list->next;
    while (list != NULL && list->hvalue == INVALID_HASH)
	list = list->next;

    if (list != NULL)
	return list;
    i = *iptr + 1;
    while (i < tb->nactive) {
	if ((list = BUCKET(tb,i)) != NULL) {
	    while (list != NULL && list->hvalue == INVALID_HASH)
		list = list->next;
	    if (list != NULL) {
		*iptr = i;
		return list;
	    }
	}
	i++;
    }
    *iptr = i;
    return NULL;
}


static int realloc_counter(HashDbTerm** bp, uint32 sz, 
			   eTerm new_counter, int counterpos)
{
    HashDbTerm* b = *bp;
    return db_realloc_counter((void **) bp, &(b->dbterm),
			      ((char *) &(b->dbterm)) - ((char *) b),
			      sz, new_counter, counterpos);
}


#ifdef HARDDEBUG

void db_check_table_hash(DbTableHash *tb)
{
    HashDbTerm* list;
    int j;
    
    for (j = 0; j < tb->nactive; j++) {
	if ((list = BUCKET(tb,j)) != 0) {
	    while (list != 0) {
		if (!is_tuple(make_tuple(list->dbterm.tpl))) {
		    erl_exit(1, "Bad term in slot %d of ets table", j);
		}
		list = list->next;
	    }
	}
    }
}

#endif
