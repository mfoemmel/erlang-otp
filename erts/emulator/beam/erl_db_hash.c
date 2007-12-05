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
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "bif.h"
#include "big.h"
#include "export.h"

#include "erl_db_hash.h"


/* 
 * The following symbols can be manipulated to "tune" the linear hash array 
 */
#define BASIC_SIZE 64               /* #words for empty array       */
#define CHAIN_LEN 6                 /* Medium bucket chain len      */


#define SZEXP   8
#define SEGSZ   (1 << SZEXP)
#define SZMASK  ((1 << SZEXP)-1)


#define SEG_LEN         256   /* When growing init segs */
#define SEG_INCREAMENT  128   /* Number of segments to grow */

#define BUCKET(tb, i) (tb)->seg[(i) >> SZEXP][(i) & SZMASK]

/*
 * When deleting a table, the number of records to delete.
 * Approximate number, because we must delete entire buckets.
 */
#define DELETE_RECORD_LIMIT 10000

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
      make_hash2(term)) % MAX_HASH)

/* 
 * tplp is an untagged pointer to a tuple we know is large enough 
 * and dth is a pointer to a DbTableHash.   
 */
#define GETKEY(dth, tplp)   (*((tplp) +  (dth)->common.keypos))

/*
 * Some special binary flags
 */
#define BIN_FLAG_ALL_OBJECTS         BIN_FLAG_USR1

/*
 * Size calculations
 */
#define SIZ_OVERHEAD ((sizeof(HashDbTerm)/sizeof(Eterm)) - 1)
#define SIZ_DBTERM(HDT) (SIZ_OVERHEAD + (HDT)->dbterm.size)

/*
 * Local types 
 */
struct mp_info {
    int all_objects;		/* True if complete objects are always
				 * returned from the match_spec (can use 
				 * copy_shallow on the return value) */
    int something_can_match;	/* The match_spec is not "impossible" */
    int key_given;
    HashDbTerm **dlists[10];     /* default buffer for list of "pre found"
				  * buckets */
    HashDbTerm ***lists;         /* List if poters to list pointers for the 
				  * buckets to search if keys are given, 
				  * = dlists initially */
    unsigned num_lists;         /* Number of elements in "lists",
				 * = 0 initially */
    Binary *mp;                 /* The compiled match program */
};



/*
** Forward decl's (static functions)
*/
static HashDbTerm** alloc_seg(DbTableHash *tb);
static int realloc_counter(DbTableCommon *tb, HashDbTerm** bp, Uint sz, 
			   Eterm new_counter, int counterpos);
static HashDbTerm* next(DbTableHash *tb, Uint *iptr, HashDbTerm *list);
static HashDbTerm* search_list(DbTableHash* tb, Eterm key, 
			       HashValue hval, HashDbTerm *list);
static void shrink(DbTableHash* tb);
static void grow(DbTableHash* tb);
static void free_term(DbTableHash *tb, HashDbTerm* p);
static Eterm put_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2);
static HashDbTerm* get_term(DbTableHash* tb, HashDbTerm* old, 
			    Eterm obj, HashValue hval);
static int analyze_pattern(DbTableHash *tb, Eterm pattern, 
			   struct mp_info *mpi);
/*
 *  Method interface functions
 */
static int db_first_hash(Process *p, 
			 DbTable *tbl, 
			 Eterm *ret);

static int db_next_hash(Process *p, 
			DbTable *tbl, 
			Eterm key,
			Eterm *ret);

static int db_update_counter_hash(Process *p, 
				  DbTable *tbl, 
				  Eterm key,
				  Eterm incr,
				  int warp,
				  int counterpos,
				  Eterm *ret);


static int db_member_hash(Process *p, DbTable *tbl, 
			  Eterm key, Eterm *ret);

static int db_get_element_hash(Process *p, DbTable *tbl, 
			       Eterm key, int ndex, Eterm *ret);

static int db_erase_object_hash(Process *p, DbTable *tbl, 
				Eterm object,Eterm *ret);

static int db_slot_hash(Process *p, DbTable *tbl, 
			Eterm slot_term, Eterm *ret);

static int db_select_chunk_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Sint chunk_size,
				int reverse, Eterm *ret);
static int db_select_hash(Process *p, DbTable *tbl, 
			  Eterm pattern, int reverse, Eterm *ret);
static int db_select_count_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Eterm *ret);
static int db_select_delete_hash(Process *p, DbTable *tbl, 
				 Eterm pattern, Eterm *ret);

static int db_select_continue_hash(Process *p, DbTable *tbl, 
				   Eterm continuation, Eterm *ret);

static int db_select_count_continue_hash(Process *p, DbTable *tbl, 
					 Eterm continuation, Eterm *ret);

static int db_select_delete_continue_hash(Process *p, DbTable *tbl,
					  Eterm continuation, Eterm *ret);
static void db_print_hash(int to,
			  void *to_arg,
			  int show,
			  DbTable *tbl);
static int db_free_table_hash(DbTable *tbl);

static int db_free_table_continue_hash(DbTable *tbl, int first);


static void db_foreach_offheap_hash(DbTable *,
				    void (*)(ErlOffHeap *, void *),
				    void *);

static int db_delete_all_objects_hash(Process* p, DbTable* tbl);
#ifdef HARDDEBUG
static void db_check_table_hash(DbTableHash *tb);
#endif


/*
** Static variables
*/

/*
** External interface 
*/
DbTableMethod db_hash =
{
    db_create_hash,
    db_first_hash,
    db_next_hash,
    db_first_hash,   /* last == first  */
    db_next_hash,    /* prev == next   */
    db_put_hash,
    db_get_hash,
    db_get_element_hash,
    db_member_hash,
    db_erase_hash,
    db_erase_object_hash,
    db_slot_hash,
    db_update_counter_hash,
    db_select_chunk_hash,
    db_select_hash,
    db_select_delete_hash,
    db_select_continue_hash, /* hmm continue_hash? */
    db_select_delete_continue_hash,
    db_select_count_hash,
    db_select_count_continue_hash,
    db_delete_all_objects_hash,
    db_free_table_hash,
    db_free_table_continue_hash,
    db_print_hash,
    db_foreach_offheap_hash,
#ifdef HARDDEBUG
    db_check_table_hash,
#else
    NULL,
#endif

};

/*
** Table interface routines ie what's called by the bif's 
*/

void db_unfix_table_hash(DbTableHash *tb)
{
    while (tb->fixdel != NULL) {
	FixedDeletion *fx = tb->fixdel;
	int ix = fx->slot;
	HashDbTerm **bp = &BUCKET(tb, ix);
	HashDbTerm *b = *bp;

	tb->fixdel = fx->next;
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));

	while (b != NULL) {
	    if (b->hvalue == INVALID_HASH) {
		*bp = b->next;
		free_term(tb, b);
		b = *bp;
	    } else {
		bp = &b->next;
		b = b->next;
	    }
	}
    }
    tb->common.kept_items = 0;
}

int db_create_hash(Process *p, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;

    tb->szm = SZMASK;
    tb->nslots = SEGSZ;
    tb->nactive = SEGSZ;
    tb->p = 0;
    tb->nsegs = 1;
    tb->seg = (HashDbTerm***) erts_db_alloc(ERTS_ALC_T_DB_SEG_TAB,
					    (DbTable *) tb,
					    sizeof(HashDbTerm**));
    tb->seg[0] = alloc_seg(tb);
    tb->fixdel = NULL;

    return DB_ERROR_NONE;
}

static int db_first_hash(Process *p, DbTable *tbl, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    int i = 0;
    while (i < tb->nactive) {
	HashDbTerm* list = BUCKET(tb, i);
	if (list != 0) {
	    Eterm key = GETKEY(tb, list->dbterm.tpl);

	    COPY_OBJECT(key, p, ret);
	    return DB_ERROR_NONE;
	}
	i++;
    }
    *ret = am_EOT;
    return DB_ERROR_NONE;
}

static int db_next_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    Uint ix;
    HashDbTerm* b1;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if (((b1->hvalue == hval) || b1->hvalue == INVALID_HASH) 
	    && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    HashDbTerm* b2 = next(tb, &ix, b1);
	    if ((tb->common.status & DB_BAG) || 
		(tb->common.status & DB_DUPLICATE_BAG)) {
		while (b2 != 0) {
		    Eterm key2 = GETKEY(tb, b2->dbterm.tpl);
		    if (EQ(key, key2)) {
			b2 = next(tb, &ix, b2);
			continue;
		    }
		    break;
		}
	    }
	    if (b2 == 0) {
		*ret = am_EOT;
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

static int db_update_counter_hash(Process *p, DbTable *tbl,
				  Eterm key,
				  Eterm incr, 
				  int warp,
				  int counterpos,
				  Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* b;
    HashDbTerm** bp;
    int ix;
    HashValue hval;

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
	counterpos = tb->common.keypos + 1;

    return db_do_update_counter(p, (DbTableCommon *) tb,
				(void *) bp, b->dbterm.tpl,
				counterpos, 
				(int (*)(DbTableCommon *,
					 void *,
					 Uint,
					 Eterm,
					 int))
				&realloc_counter, incr, warp, ret);
}

int db_put_hash(Process *proc, DbTable *tbl, Eterm obj, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    Eterm key;
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
	    if (tb->common.status & DB_SET) {
		HashDbTerm* bnext = b->next;
		if (b->hvalue == INVALID_HASH) {
		    tb->common.nitems++;
		}
		q = get_term(tb, b, obj, hval);
		q->next = bnext;
		q->hvalue = hval; /* In case of INVALID_HASH */
		*bp = q;
		*ret = am_true;
		return DB_ERROR_NONE;
	    }
	    else if (tb->common.status & DB_BAG) {
		HashDbTerm** tp = bp;
                HashDbTerm* p = b;
		
                if (eq(make_tuple(b->dbterm.tpl), obj)) {
		    if (b->hvalue == INVALID_HASH) {
			tb->common.nitems++;
		    }
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
			if (b->hvalue == INVALID_HASH) {
			    tb->common.nitems++;
			}
			b->hvalue = hval;
			*ret = am_true;
			return DB_ERROR_NONE;
                    }
                    bp = &b->next;
                    b = b->next;
                }

                q = get_term(tb, NULL, obj, hval);
                q->next = p;
                *tp = q;
		goto Lupdate;
	    }
	    else {  /* if (tb->status & DB_DUPLICATE_BAG) */
		q = get_term(tb, NULL, obj, hval);
		q->next = b;
		*bp = q;
		goto Lupdate;
	    }
	}
	bp = &b->next;
	b = b->next;
    }

    q = get_term(tb, NULL, obj, hval);
    q->next = b;
    *bp = q;

 Lupdate:
    tb->common.nitems++;

    if ( ((tb->common.nitems / tb->nactive) > CHAIN_LEN) && 
	((tb->common.status & DB_FIXED) == 0))
	grow(tb);
    CHECK_TABLES();
    *ret = am_true;
    return DB_ERROR_NONE;
}

int db_get_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    HashDbTerm* b2 = b1->next;
	    Eterm copy;

	    if ((tb->common.status & DB_BAG) || 
		(tb->common.status & DB_DUPLICATE_BAG)) {
		while((b2 != 0) && ((b2->hvalue == hval) || 
				    (b2->hvalue == INVALID_HASH)) &&
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

int db_get_element_array(DbTable *tbl, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    int num = 0;
    
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    if ((tb->common.status & DB_BAG) || 
		(tb->common.status & DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;

		while((b2 != 0) && ((b2->hvalue == hval) || 
				    (b2->hvalue == INVALID_HASH)) &&
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
    
    
static int db_member_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    *ret = am_true;
	    return DB_ERROR_NONE;
	}
	b1 = b1->next;
    }
    *ret = am_false;
    return DB_ERROR_NONE;
}
    
static int db_get_element_hash(Process *p, DbTable *tbl, 
			       Eterm key,
			       int ndex, 
			       Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm* b1;
    
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    b1 = BUCKET(tb, ix);

    while(b1 != 0) {
	if ((b1->hvalue == hval) && EQ(key, GETKEY(tb, b1->dbterm.tpl))) {
	    Eterm copy;

	    if (ndex > arityval(b1->dbterm.tpl[0]))
		return DB_ERROR_BADITEM;

	    if ((tb->common.status & DB_BAG) || 
		(tb->common.status & DB_DUPLICATE_BAG)) {
		HashDbTerm* b;
		HashDbTerm* b2 = b1->next;
		Eterm elem_list = NIL;

		while((b2 != 0) && (b2->hvalue == hval) &&
		      EQ(key, GETKEY(tb, b2->dbterm.tpl))) {
		    if (ndex > arityval(b2->dbterm.tpl[0]))
			return DB_ERROR_BADITEM;
		    b2 = b2->next;
		}

		b = b1;
		while(b != b2) {
		    Eterm *hp;
		    Uint sz = size_object(b->dbterm.tpl[ndex])+2;
		    
		    hp = HAlloc(p, sz);
		    copy = copy_struct(b->dbterm.tpl[ndex], sz-2, &hp, &MSO(p));
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
int db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    int found = 0;

    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    ASSERT(!(tb->common.status & DB_FIXED));
    ASSERT((tb->common.status & DB_BAG));

    while(b != 0) {
	if ((b->hvalue == hval) && EQ(key, GETKEY(tb, b->dbterm.tpl))) {
	    found = 1;
	    if ((arityval(b->dbterm.tpl[0]) == 2) && 
		EQ(value, b->dbterm.tpl[2])) {
		*bp = b->next;
		free_term(tb, b);
		tb->common.nitems--;
		b = *bp;
		break;
	    }
	} else if (found) {
		break;
	}
	bp = &b->next;
	b = b->next;
    }

    if (found && ((tb->common.nitems / tb->nactive) < CHAIN_LEN))
	shrink(tb);
    return DB_ERROR_NONE;
}
	
/*
** NB, this is for the db_erase/2 bif.
*/
int db_erase_hash(Process *p, DbTable *tbl, 
		  Eterm key,
		  Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
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
	    if (tb->common.status & DB_FIXED) {
		/* Pseudo remove */
		FixedDeletion *fixd = (FixedDeletion *) 
		    erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
				  (DbTable *) tb,
				  sizeof(FixedDeletion));
		fixd->slot = ix;
		fixd->next = tb->fixdel;
		tb->fixdel = fixd;
		tb->common.nitems--;
		tb->common.kept_items++;
		b->hvalue = INVALID_HASH;
		bp = &b->next;
		b = b->next;
	    } else {
		*bp = b->next;
		free_term(tb, b);
		tb->common.nitems--;
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

    if (found && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);

    *ret = am_true;
    return DB_ERROR_NONE;
}    

/*
** This is for the ets:delete_object BIF
*/
static int db_erase_object_hash(Process *p, DbTable *tbl, 
				Eterm object, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    HashValue hval;
    int ix;
    HashDbTerm** bp;
    HashDbTerm* b;
    int found = 0;
    Eterm key;

    key = GETKEY(tb, tuple_val(object));
    hval = MAKE_HASH(key);
    HASH(tb, hval, ix);
    bp = &BUCKET(tb, ix);
    b = *bp;

    while(b != 0) {
	if ((b->hvalue == hval) && eq(object, make_tuple(b->dbterm.tpl))) {
	    if (tb->common.status & DB_FIXED) {
		/* Pseudo remove */
		FixedDeletion *fixd = (FixedDeletion *) 
		    erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
				  (DbTable *) tb,
				  sizeof(FixedDeletion));
		fixd->slot = ix;
		fixd->next = tb->fixdel;
		tb->fixdel = fixd;
		tb->common.nitems--;
		tb->common.kept_items++;
		b->hvalue = INVALID_HASH;
		bp = &b->next;
		b = b->next;
	    } else {
		*bp = b->next;
		free_term(tb, b);
		tb->common.nitems--;
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

    if (found && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);

    *ret = am_true;
    return DB_ERROR_NONE;
}    


static int db_slot_hash(Process *p, DbTable *tbl, Eterm slot_term, Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Sint slot;

    if (is_not_small(slot_term) ||
	((slot = signed_val(slot_term)) < 0) ||
	(slot > tb->nactive))
	return DB_ERROR_BADPARAM;
    
    if (slot == tb->nactive) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }

    *ret = put_term_list(p, BUCKET(tb, slot), 0);

    return DB_ERROR_NONE;
}


/*
 * This is just here so I can take care of the return value 
 * that is to be sent during a trap (the BIF_TRAP macros explicitly returns)
 */
static BIF_RETTYPE bif_trap1(Export *bif,
			      Process *p, 
			      Eterm p1) 
{
    BIF_TRAP1(bif, p, p1);
}
    
/*
 * Continue collecting select matches, this may happen either due to a trap
 * or when the user calls ets:select/1
 */
static int db_select_continue_hash(Process *p, 
				   DbTable *tbl, 
				   Eterm continuation, 
				   Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Sint chain_pos; 
    Sint save_chain_pos;
    Sint chunk_size;
    int all_objects;
    Binary *mp;
    int num_left = 1000;
    HashDbTerm *current_list = 0;
    Eterm match_list;
    Uint32 dummy;
    unsigned sz;
    Eterm *hp;
    Eterm match_res;
    Sint got;
    Eterm *tptr;


#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple but not the arity or anything else */

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 6)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    if (!is_small(tptr[2]) || !is_small(tptr[3]) || !is_binary(tptr[4]) || 
	!(is_list(tptr[5]) || tptr[5] == NIL) || !is_small(tptr[6]))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    if ((chain_pos = signed_val(tptr[2])) < 0 || chain_pos > tb->nactive)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    if ((chunk_size = signed_val(tptr[3])) < 0)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    if (!(thing_subtag(*binary_val(tptr[4])) == REFC_BINARY_SUBTAG))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    mp = ((ProcBin *) binary_val(tptr[4]))->val;
    if (!(mp->flags & BIN_FLAG_MATCH_PROG))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    all_objects = mp->flags & BIN_FLAG_ALL_OBJECTS;
    match_list = tptr[5];
    if ((got = signed_val(tptr[6])) < 0)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);


    if (chunk_size && got >= chunk_size) {
	/* Already got it in the match_list */
	goto done;
    }

    for(;;) {
	if (chain_pos == tb->nactive) {
	    goto done;
	}

	if ((current_list = BUCKET(tb,chain_pos)) != NULL) {
	    break;
	}
	++chain_pos;
    }
	
    
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
					 &MSO(p));
	    } else {
		sz = size_object(match_res);
	    
		hp = HAlloc(p, sz + 2);
		match_res = copy_struct(match_res, sz, &hp, &MSO(p));
	    }
            match_list = CONS(hp, match_res, match_list);
	    ++got;
	}
	--num_left;
	save_chain_pos = chain_pos;
	if ((current_list = 
	     next(tb, (Uint*)&chain_pos, current_list)) == 0) {
	    break;
	}
	if (chain_pos != save_chain_pos) { 
	    if (chunk_size && got >= chunk_size) {
		break;
	    }    
	    if (num_left <= 0 || MBUF(p)) {
		/*
		 * We have either reached our limit, or just created some heap fragments.
		 * Since many heap fragments will make the GC slower, trap and GC now.
		 */
		goto trap;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (chunk_size) {
	Eterm continuation;
	Eterm rest = NIL;
	Sint rest_size = 0;

	if (got > chunk_size) { /* Cannot write destructively here, 
				   the list may have 
				   been in user space */
	    rest = NIL;
	    hp = HAlloc(p, (got - chunk_size) * 2); 
	    while (got-- > chunk_size) {
		rest = CONS(hp, CAR(list_val(match_list)), rest);
		hp += 2;
		match_list = CDR(list_val(match_list));
		++rest_size;
	    }
	}
	if (rest != NIL || chain_pos < tb->nactive) {
	    hp = HAlloc(p,3+7);
	    continuation = TUPLE6(hp, tptr[1], make_small(chain_pos), 
				  tptr[3], tptr[4], rest, 
				  make_small(rest_size));
	    hp += 7;
	    RET_TO_BIF(TUPLE2(hp, match_list, continuation),DB_ERROR_NONE);
	} else {
	    if (match_list != NIL) {
		hp = HAlloc(p, 3);
		RET_TO_BIF(TUPLE2(hp, match_list, am_EOT),DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    }
	}
    }
    RET_TO_BIF(match_list,DB_ERROR_NONE);

trap:
    BUMP_ALL_REDS(p);

    hp = HAlloc(p,7);
    continuation = TUPLE6(hp, tptr[1], make_small(chain_pos), tptr[3],
			  tptr[4], match_list, make_small(got));
    RET_TO_BIF(bif_trap1(&ets_select_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

static int db_select_hash(Process *p, DbTable *tbl, 
			  Eterm pattern, int reverse,
			  Eterm *ret)
{
    return db_select_chunk_hash(p, tbl, pattern, 0, reverse, ret);
}

static int db_select_chunk_hash(Process *p, DbTable *tbl, 
				Eterm pattern, Sint chunk_size, 
				int reverse, /* not used */
				Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Uint chain_pos;
    Uint save_chain_pos;
    HashDbTerm *current_list = 0;
    unsigned current_list_pos = 0;
    Eterm match_list;
    Uint32 dummy;
    Eterm match_res;
    unsigned sz;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm mpb;


#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);	\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	if (chunk_size) {
	    RET_TO_BIF(am_EOT, DB_ERROR_NONE); /* We're done */
	}  
	RET_TO_BIF(NIL, DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
	for(chain_pos = 0; chain_pos < tb->nactive; chain_pos++) {
	    if ((current_list = BUCKET(tb,chain_pos)) != 0)
		break;
	}
	if (current_list == 0) {
	    if (chunk_size) {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE); /* We're done */
	    }  
	    RET_TO_BIF(NIL,DB_ERROR_NONE);
	} 
    } else {
	/* We have at least one */
	chain_pos = tb->nactive;
	current_list = *(mpi.lists[current_list_pos++]); 
    }

    match_list = NIL;

    for(;;) {
	if (current_list->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mpi.mp,
			   make_tuple(current_list->dbterm.tpl),
			   0,&dummy),
	     is_value(match_res))) {
	    if (mpi.all_objects) {
		hp = HAlloc(p, current_list->dbterm.size + 2);
		match_res = copy_shallow(current_list->dbterm.v,
					 current_list->dbterm.size,
					 &hp,
					 &MSO(p));
	    } else {
		sz = size_object(match_res);
	    
		hp = HAlloc(p, sz + 2);
		match_res = copy_struct(match_res, sz, &hp, &MSO(p));
	    }
            match_list = CONS(hp, match_res, match_list);
	    ++got;
	}

	/* Update the list variable */
        if (mpi.key_given) {  /* Key is bound */
	    current_list = current_list->next;
	    for (;;) {
		while (current_list != NULL && 
		       current_list->hvalue == INVALID_HASH)
		    current_list = current_list->next;
		if (current_list == NULL) {
		    if (current_list_pos == mpi.num_lists) {
			goto done;
		    } else {
			current_list = *(mpi.lists[current_list_pos++]);
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
	    if (chain_pos != save_chain_pos) {
		if (chunk_size && got >= chunk_size) {
		    break;
		}    
		if (num_left <= 0 || MBUF(p)) {
		    /*
		     * We have either reached our limit, or just created some heap fragments.
		     * Since many heap fragments will make the GC slower, trap and GC now.
		     */
		    goto trap;
		}
	    }
        }
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (chunk_size) {
	Eterm continuation;
	Eterm rest = NIL;
	Sint rest_size = 0;

	if (mpi.all_objects)
	    (mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	if (got > chunk_size) { /* Split list in return value and 'rest' */
	    Eterm tmp = match_list;
	    rest = match_list;
	    while (got-- > chunk_size + 1) { 
		tmp = CDR(list_val(tmp));
		++rest_size;
	    }
	    ++rest_size;
	    match_list = CDR(list_val(tmp));
	    CDR(list_val(tmp)) = NIL; /* Destructive, the list has never 
					 been in 'user space' */ 
	}
	if (rest != NIL || chain_pos < tb->nactive) { /* Need more calls */
	    hp = HAlloc(p,3+7+PROC_BIN_SIZE);
	    mpb =db_make_mp_binary(p,(mpi.mp),&hp);
	    if (mpi.all_objects)
		(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	    continuation = TUPLE6(hp, tb->common.id,make_small(chain_pos), 
				  make_small(chunk_size),  
				  mpb, rest, 
				  make_small(rest_size));
	    mpi.mp = NULL; /*otherwise the return macro will destroy it */
	    hp += 7;
	    RET_TO_BIF(TUPLE2(hp, match_list, continuation),DB_ERROR_NONE);
	} else { /* All data is exhausted */
	    if (match_list != NIL) { /* No more data to search but still a
					result to return to the caller */
		hp = HAlloc(p, 3);
		RET_TO_BIF(TUPLE2(hp, match_list, am_EOT),DB_ERROR_NONE);
	    } else { /* Reached the end of the ttable with no data to return */
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    }
	}
    }
    RET_TO_BIF(match_list,DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (mpi.all_objects)
	(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
    hp = HAlloc(p,7+PROC_BIN_SIZE);
    mpb =db_make_mp_binary(p,(mpi.mp),&hp);
    continuation = TUPLE6(hp, tb->common.id, make_small(chain_pos), 
			  make_small(chunk_size), 
			  mpb, match_list, 
			  make_small(got));
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

static int db_select_count_hash(Process *p, 
				DbTable *tbl, 
				Eterm pattern,
				Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Uint chain_pos = 0;
    HashDbTerm **current_list = NULL;
    unsigned current_list_pos = 0;
    Uint32 dummy;
    Eterm match_res;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm egot;
    Eterm mpb;

#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);	\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0), DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
	for(; chain_pos < tb->nactive; ++chain_pos) {
	    if (BUCKET(tb,chain_pos) != NULL) {
		current_list = &BUCKET(tb,chain_pos);
		break;
	    }
	}
	if (chain_pos == tb->nactive) {
	    RET_TO_BIF(make_small(0),DB_ERROR_NONE);
	} 
    } else {
	/* We have at least one */
	current_list = mpi.lists[current_list_pos++]; 
    }


    for(;;) {
	if ((*current_list)->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mpi.mp,
			   make_tuple((*current_list)->dbterm.tpl),
			   0,&dummy)) == am_true) {
	    ++got;
	}

	--num_left;
	/* Update the list variable */
	current_list = &((*current_list)->next);
	for (;;) {
	    while ((*current_list) != NULL && 
		   (*current_list)->hvalue == INVALID_HASH)
		current_list = &((*current_list)->next);
	    if ((*current_list) == NULL) {
		if (mpi.key_given) {  /* Key is bound */
		    if (current_list_pos == mpi.num_lists) {
			goto done;
		    } else {
			current_list = mpi.lists[current_list_pos++];
		    }
		} else {
		    for(++chain_pos; chain_pos < tb->nactive; ++chain_pos) {
			if (BUCKET(tb,chain_pos) != NULL) {
			    current_list = &BUCKET(tb,chain_pos);
			    break;
			}
		    }
		    if (chain_pos == tb->nactive) {
			goto done;
		    } 
		    if (num_left <= 0) {
			goto trap;
		    }
		}	
	    } else {
		break;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  PROC_BIN_SIZE + 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p,mpi.mp,&hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(chain_pos), 
			  mpb, 
			  egot);
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_count_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

static int db_select_delete_hash(Process *p,
				 DbTable *tbl,
				 Eterm pattern,
				 Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    struct mp_info mpi;
    Uint chain_pos = 0;
    HashDbTerm **current_list = NULL;
    unsigned current_list_pos = 0;
    Uint32 dummy;
    Eterm match_res;
    Eterm *hp;
    int num_left = 1000;
    Uint got = 0;
    Eterm continuation;
    int errcode;
    Eterm mpb;
    Eterm egot;

#define RET_TO_BIF(Term,RetVal) do {		\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);	\
	}					\
	if (mpi.lists != mpi.dlists) {		\
	    erts_free(ERTS_ALC_T_DB_SEL_LIST,	\
		      (void *) mpi.lists);	\
	}					\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)


    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0), DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    if (!mpi.key_given) {
    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
	for(; chain_pos < tb->nactive; ++chain_pos) {
	    if (BUCKET(tb,chain_pos) != NULL) {
		current_list = &BUCKET(tb,chain_pos);
		break;
	    }
	}
	if (chain_pos == tb->nactive) {
	    RET_TO_BIF(make_small(0),DB_ERROR_NONE);
	} 
    } else {
	/* We have at least one */
	current_list = mpi.lists[current_list_pos++]; 
    }


    for(;;) {
	int did_erase = 0;
	if ((*current_list)->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mpi.mp,
			   make_tuple((*current_list)->dbterm.tpl),
			   0,&dummy)) == am_true) {
	    if (tb->common.status & DB_FIXED) {
		FixedDeletion *fixd = (FixedDeletion *)
		    erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
				  (DbTable *) tb,
				  sizeof(FixedDeletion));
		int ix;
		HASH(tb, (*current_list)->hvalue, ix);
		fixd->slot = ix;
		fixd->next = tb->fixdel;
		tb->fixdel = fixd;
		tb->common.kept_items++;
		tb->common.nitems--;
		(*current_list)->hvalue = INVALID_HASH;
	    } else {
		HashDbTerm *del = *current_list;
		*current_list = (*current_list)->next;
		free_term(tb, del);
		tb->common.nitems--;
		did_erase = 1;
	    }
	    ++got;
	}

	--num_left;
	/* Update the list variable */
	if (!did_erase) {
	    current_list = &((*current_list)->next);
	}
	for (;;) {
	    while ((*current_list) != NULL && 
		   (*current_list)->hvalue == INVALID_HASH)
		current_list = &((*current_list)->next);
	    if ((*current_list) == NULL) {
		if (mpi.key_given) {  /* Key is bound */
		    if (current_list_pos == mpi.num_lists) {
			goto done;
		    } else {
			current_list = mpi.lists[current_list_pos++];
		    }
		} else {
		    for(++chain_pos; chain_pos < tb->nactive; ++chain_pos) {
			if (BUCKET(tb,chain_pos) != NULL) {
			    current_list = &BUCKET(tb,chain_pos);
			    break;
			}
		    }
		    if (chain_pos == tb->nactive) {
			goto done;
		    } 
		    if (num_left <= 0) {
			goto trap;
		    }
		}	
	    } else {
		break;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  PROC_BIN_SIZE + 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + PROC_BIN_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    mpb = db_make_mp_binary(p,mpi.mp,&hp);
    continuation = TUPLE4(hp, tb->common.id, make_small(chain_pos), 
			  mpb, 
			  egot);
    mpi.mp = NULL; /*otherwise the return macro will destroy it */
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
/*
** This is called when select_delete traps
*/
static int db_select_delete_continue_hash(Process *p, 
					  DbTable *tbl,
					  Eterm continuation,
					  Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint chain_pos;
    HashDbTerm **current_list = NULL;
    Uint32 dummy;
    Eterm match_res;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    int force_delete;
    int do_delete;
    DbFixation *fix = tb->common.fixations;
    Eterm egot;

    force_delete = (tb->common.status & DB_FIXED) && 
	!ONLY_READER(p,tb) && SOLE_LOCKER(p,fix);
    do_delete = (!(tb->common.status & DB_FIXED) || force_delete);
    

#define RET_TO_BIF(Term,RetVal) do {		\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)

    
    tptr = tuple_val(continuation);
    chain_pos = unsigned_val(tptr[2]);
    mp = ((ProcBin *) binary_val(tptr[3]))->val;
    if (is_big(tptr[4])) {
	got = big_to_uint32(tptr[4]);
    } else {
	got = unsigned_val(tptr[4]);
    }
    

    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
    for(; chain_pos < tb->nactive; ++chain_pos) {
	if (BUCKET(tb,chain_pos) != NULL) {
	    current_list = &BUCKET(tb,chain_pos);
	    break;
	}
    }
    if (chain_pos == tb->nactive) {
	goto done;
    } 

    for(;;) {
	int did_erase = 0;
	if ((*current_list)->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mp,
			   make_tuple((*current_list)->dbterm.tpl),
			   0,&dummy)) == am_true) {
	    if (do_delete) {
		HashDbTerm *del = *current_list;
		*current_list = (*current_list)->next;
		free_term(tb, del);
		tb->common.nitems--;
		did_erase = 1;
	    } else {
		FixedDeletion *fixd = (FixedDeletion *) 
		    erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
				  (DbTable *) tb,
				  sizeof(FixedDeletion));
		fixd->slot = chain_pos;
		fixd->next = tb->fixdel;
		tb->fixdel = fixd;
		tb->common.kept_items++;
		tb->common.nitems--;
		(*current_list)->hvalue = INVALID_HASH;
	    }
	    ++got;
	}

	--num_left;
	/* Update the list variable */
	if (!did_erase) {
	    current_list = &((*current_list)->next);
	}
	for (;;) {
	    while ((*current_list) != NULL && 
		   (*current_list)->hvalue == INVALID_HASH)
		current_list = &((*current_list)->next);
	    if ((*current_list) == NULL) {
		for(++chain_pos; chain_pos < tb->nactive; ++chain_pos) {
		    if (BUCKET(tb,chain_pos) != NULL) {
			current_list = &BUCKET(tb,chain_pos);
			break;
		    }
		}
		if (chain_pos == tb->nactive) {
		    goto done;
		} 
		if (num_left <= 0) {
		    goto trap;
		}
	    } else {
		break;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p,  5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    continuation = TUPLE4(hp, tb->common.id, make_small(chain_pos), 
			  tptr[3], 
			  egot);
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
    
/*
** This is called when select_delete traps
*/
static int db_select_count_continue_hash(Process *p, 
					 DbTable *tbl,
					 Eterm continuation,
					 Eterm *ret)
{
    DbTableHash *tb = &tbl->hash;
    Uint chain_pos;
    HashDbTerm **current_list = NULL;
    Uint32 dummy;
    Eterm match_res;
    Eterm *hp;
    int num_left = 1000;
    Uint got;
    Eterm *tptr;
    Binary *mp;
    Eterm egot;

#define RET_TO_BIF(Term,RetVal) do {		\
	*ret = (Term);				\
	return RetVal;				\
    } while(0)

    
    tptr = tuple_val(continuation);
    chain_pos = unsigned_val(tptr[2]);
    mp = ((ProcBin *) binary_val(tptr[3]))->val;
    if (is_big(tptr[4])) {
	got = big_to_uint32(tptr[4]);
    } else {
	got = unsigned_val(tptr[4]);
    }
    

    /* Run this code if pattern is variable or GETKEY(pattern)  */
    /* is a variable                                            */
    for(; chain_pos < tb->nactive; ++chain_pos) {
	if (BUCKET(tb,chain_pos) != NULL) {
	    current_list = &BUCKET(tb,chain_pos);
	    break;
	}
    }
    if (chain_pos == tb->nactive) {
	goto done;
    } 

    for(;;) {
	if ((*current_list)->hvalue != INVALID_HASH && 
	    (match_res = 
	     db_prog_match(p,mp,
			   make_tuple((*current_list)->dbterm.tpl),
			   0,&dummy)) == am_true) {
	    ++got;
	}

	--num_left;
	/* Update the list variable */
	current_list = &((*current_list)->next);
	for (;;) {
	    while ((*current_list) != NULL && 
		   (*current_list)->hvalue == INVALID_HASH)
		current_list = &((*current_list)->next);
	    if ((*current_list) == NULL) {
		for(++chain_pos; chain_pos < tb->nactive; ++chain_pos) {
		    if (BUCKET(tb,chain_pos) != NULL) {
			current_list = &BUCKET(tb,chain_pos);
			break;
		    }
		}
		if (chain_pos == tb->nactive) {
		    goto done;
		} 
		if (num_left <= 0) {
		    goto trap;
		}
	    } else {
		break;
	    }
	}
    }
done:
    BUMP_REDS(p, 1000 - num_left);
    if (got && ((tb->common.nitems / tb->nactive) < CHAIN_LEN) &&
	((tb->common.status & DB_FIXED) == 0))
	shrink(tb);
    RET_TO_BIF(erts_make_integer(got,p),DB_ERROR_NONE);
trap:
    BUMP_ALL_REDS(p);
    if (IS_USMALL(0, got)) {
	hp = HAlloc(p, 5);
	egot = make_small(got);
    }
    else {
	hp = HAlloc(p, BIG_UINT_HEAP_SIZE + 5);
	egot = uint_to_big(got, hp);
	hp += BIG_UINT_HEAP_SIZE;
    }
    continuation = TUPLE4(hp, tb->common.id, make_small(chain_pos), 
			  tptr[3], 
			  egot);
    RET_TO_BIF(bif_trap1(&ets_select_count_continue_exp, p, 
			 continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}
    
/*
** Other interface routines (not directly coupled to one bif)
*/

void db_initialize_hash(void) {
};

int db_mark_all_deleted_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int i;
    FixedDeletion *fixd;

    for (i = 0; i < tb->nactive; i++) {
	if ((list = BUCKET(tb,i)) != 0) {
	    fixd = (FixedDeletion *)
		erts_db_alloc(ERTS_ALC_T_DB_FIX_DEL,
			      (DbTable *) tb,
			      sizeof(FixedDeletion));
	    fixd->slot = i;
	    fixd->next = tb->fixdel;
	    tb->fixdel = fixd;
	    while(list != 0) {
		list->hvalue = INVALID_HASH;
		list = list->next;
	    }
	}
    }
    tb->common.kept_items = tb->common.nitems;
    tb->common.nitems = 0;
    return DB_ERROR_NONE;
}

/* Display hash table contents (for dump) */
static void db_print_hash(int to, void *to_arg, int show, DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    int i;
    
    erts_print(to, to_arg, "Buckets: %d \n", tb->nactive);
    
    if (show) {
	for (i = 0; i < tb->nactive; i++) {
	    HashDbTerm* list = BUCKET(tb,i);
	    if (list == NULL)
		continue;
	    erts_print(to, to_arg, "%d: [", i);
	    while(list != 0) {
		if (list->hvalue == INVALID_HASH)
		    erts_print(to, to_arg, "*");
		erts_print(to, to_arg, "%T", make_tuple(list->dbterm.tpl));
		if (list->next != 0)
		    erts_print(to, to_arg, ",");
		list = list->next;
	    }
	    erts_print(to, to_arg, "]\n");
	}
    }
}

/* release all memory occupied by a single table */
static int db_free_table_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm*** sp = tb->seg;
    int n = tb->nsegs;

    while (tb->fixdel != NULL) {
	FixedDeletion *fx = tb->fixdel;
	tb->fixdel = fx->next;
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));
    }
    while(n--) {
	HashDbTerm** bp = *sp;
	if (bp != 0) {
	    int m = SEGSZ;

	    while(m--) {
		HashDbTerm* p = *bp++;

		while(p != 0) {
		    HashDbTerm* nxt = p->next;
		    free_term(tb, p);
		    p = nxt;
		}
	    }
	    erts_db_free(ERTS_ALC_T_DB_SEG,
			 (DbTable *) tb,
			 (void *) *sp,
			 sizeof(HashDbTerm*)*SEGSZ);
	}
	sp++;
    }
    erts_db_free(ERTS_ALC_T_DB_SEG_TAB,
		 (DbTable *) tb,
		 (void *) tb->seg,
		 sizeof(HashDbTerm**)*tb->nsegs);

    ASSERT(erts_smp_atomic_read(&tb->common.memory_size) == sizeof(DbTable));
    return 0;
}

static int db_free_table_continue_hash(DbTable *tbl, int first)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm*** sp = tb->seg;
    int n = tb->nsegs;
    int done;

    /*
     * Optimization: tb->p will hold the number of the next
     * bucket to be deleted so that we quickly can skip deleted buckets.
     */
    if (first) {
	tb->p = 0;		/* Initialize. */
    } else {
	/* Skip already deleted buckets. */
	sp += tb->p;
	n -= tb->p;
    }

    done = 0;
    while (tb->fixdel != NULL) {
	FixedDeletion *fx = tb->fixdel;

	tb->fixdel = fx->next;
	erts_db_free(ERTS_ALC_T_DB_FIX_DEL,
		     (DbTable *) tb,
		     (void *) fx,
		     sizeof(FixedDeletion));
	if (++done >= 2*DELETE_RECORD_LIMIT) {
	    return 0;		/* Not done */
	}
    }

    done = done / 2;
    while(n--) {
	HashDbTerm** bp = *sp;
	if (bp != 0) {
	    int m = SEGSZ;

	    while(m--) {
		HashDbTerm* p = *bp++;

		while (p != 0) {
		    HashDbTerm* nxt = p->next;
		    free_term(tb, p);
		    tb->common.nitems--; /* Needed for correct reduction counting */
		    p = nxt;
		}
	    }
	    erts_db_free(ERTS_ALC_T_DB_SEG,
			 (DbTable *) tb,
			 (void *) *sp,
			 sizeof(HashDbTerm*)*SEGSZ);

	    /*
	     * Mark this segment done. (Necessary if the non-interruptible
	     * delete function will be invoked if the process is killed.)
	     */
	    *sp = NULL;

	    /*
	     * If we have done enough work, get out here.
	     */
	    if (++done >= (DELETE_RECORD_LIMIT / CHAIN_LEN / SEGSZ)) {
		tb->p = sp - tb->seg + 1; /* Remember where we stopped. */
		return 0;	/* Not done */
	    }
	}
	sp++;
    }
    erts_db_free(ERTS_ALC_T_DB_SEG_TAB,
		 (DbTable *) tb,
		 (void *) tb->seg,
		 sizeof(HashDbTerm**)*tb->nsegs);

    ASSERT(erts_smp_atomic_read(&tb->common.memory_size) == sizeof(DbTable));
    return 1;			/* Done */
}



/*
** Utility routines. (static)
*/
/*
** For the select functions, analyzes the pattern and determines which
** part of the tree should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableHash *tb, Eterm pattern, 
			   struct mp_info *mpi)
{
    Eterm *ptpl;
    Eterm lst, tpl, ttpl;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    Eterm key = NIL;	       
    HashValue hval = NIL;      
    int num_heads = 0;
    int i;
    
    mpi->lists = mpi->dlists;
    mpi->num_lists = 0;
    mpi->key_given = 1;
    mpi->something_can_match = 0;
    mpi->all_objects = 1;
    mpi->mp = NULL;

    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	return DB_ERROR_BADPARAM;
    }

    if (num_heads > 10) {
	buff = erts_alloc(ERTS_ALC_T_DB_TMP, sizeof(Eterm) * num_heads * 3);
	mpi->lists = erts_alloc(ERTS_ALC_T_DB_SEL_LIST,
				sizeof(*(mpi->lists)) * num_heads);
	
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for(lst = pattern; is_list(lst); lst = CDR(list_val(lst))) {
	Eterm body;
	ttpl = CAR(list_val(lst));
	if (!is_tuple(ttpl)) {
	    if (buff != sbuff) { 
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
	ptpl = tuple_val(ttpl);
	if (ptpl[0] != make_arityval(3U)) {
	    if (buff != sbuff) { 
		erts_free(ERTS_ALC_T_DB_TMP, buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
	matches[i] = tpl = ptpl[1];
	guards[i] = ptpl[2];
	bodies[i] = body = ptpl[3];
	if (!is_list(body) || CDR(list_val(body)) != NIL ||
	    CAR(list_val(body)) != am_DollarUnderscore) {
	    mpi->all_objects = 0;
	}
	++i;
	if (!(mpi->key_given)) {
	    continue;
	}
	if (tpl == am_Underscore || db_is_variable(tpl) != -1) {
	    (mpi->key_given) = 0;
	    (mpi->something_can_match) = 1;
	} else {
	    key = db_getkey(tb->common.keypos, tpl);
	    if (is_value(key)) {
		if (!db_has_variable(key)) {   /* Bound key */
		    int ix;
		    HashDbTerm **tmp;
		    hval = MAKE_HASH(key);
		    HASH(tb, hval, ix);
		    tmp = &BUCKET(tb,ix);
		    if (search_list(tb, key, hval, 
				    BUCKET(tb, ix)) != 0) {
			int j;
			for (j = 0; j < (mpi->num_lists) && 
				 (mpi->lists)[j] != tmp; ++j)
			    ;
			if (j == (mpi->num_lists)) {
			    (mpi->lists)[(mpi->num_lists)++] = tmp;
			}
			mpi->something_can_match = 1;
		    }
		} else {
		    mpi->key_given = 0;
		    mpi->something_can_match = 1;
		}
	    }
	}
    }

    /*
     * It would be nice not to compile the match_spec if nothing could match,
     * but then the select calls would not fail like they should on bad 
     * match specs that happen to specify non existent keys etc.
     */
    if ((mpi->mp = db_match_compile(matches, guards, bodies,
				    num_heads, DCOMP_TABLE, NULL)) 
	== NULL) {
	if (buff != sbuff) { 
	    erts_free(ERTS_ALC_T_DB_TMP, buff);
	}
	return DB_ERROR_BADPARAM;
    }
    if (buff != sbuff) { 
	erts_free(ERTS_ALC_T_DB_TMP, buff);
    }
    return DB_ERROR_NONE;
}

static HashDbTerm** alloc_seg(DbTableHash *tb)
{
    HashDbTerm** bp;
    int sz = sizeof(HashDbTerm*)*SEGSZ;

    bp = (HashDbTerm**) erts_db_alloc_fnf(ERTS_ALC_T_DB_SEG,
					  (DbTable *) tb,
					  sz);
    if (!bp)
	return NULL;
    memset(bp, 0, sz);
    return bp;
}


static HashDbTerm* get_term(DbTableHash* tb, HashDbTerm* old, 
			    Eterm obj, HashValue hval) {
    HashDbTerm* p = db_get_term((DbTableCommon *) tb,
				(old != NULL) ? &(old->dbterm) : NULL, 
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
static Eterm put_term_list(Process* p, HashDbTerm* ptr1, HashDbTerm* ptr2)
{
    int sz = 0;
    HashDbTerm* ptr;
    Eterm list = NIL;
    Eterm copy;
    Eterm *hp;

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
	    copy = copy_shallow(ptr->dbterm.v, ptr->dbterm.size, &hp, &MSO(p));
	    list = CONS(hp, copy, list);
	    hp  += 2;
	}
	ptr = ptr->next;
    }
    return list;
}

static void free_term(DbTableHash *tb, HashDbTerm* p)
{
    db_free_term_data(&(p->dbterm));
    erts_db_free(ERTS_ALC_T_DB_TERM,
		 (DbTable *) tb,
		 (void *) p,
		 SIZ_DBTERM(p)*sizeof(Eterm));
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
	    HashDbTerm** new_segment = alloc_seg(tb);
	    HashDbTerm*** new_seg;

	    if (new_segment == NULL)
		return;

	    if (nxt == tb->nsegs) {
		int i, sz;

		if (tb->nsegs == 1)
		    sz = SEG_LEN;
		else
		    sz = tb->nsegs + SEG_INCREAMENT;
		new_seg = (HashDbTerm***) 
		    erts_db_realloc(ERTS_ALC_T_DB_SEG_TAB,
				    (DbTable *) tb,
				    (void *) tb->seg,
				    sizeof(HashDbTerm**)*tb->nsegs,
				    sizeof(HashDbTerm**)*sz);
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
	    *bps = b;               /* link  *in order due to bags!* */
	    bps = &b->next;
	    b->next = NULL;
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

    if ((tb->nactive & SZMASK) == 0) {
	int six = (tb->nactive >> SZEXP);

	erts_db_free(ERTS_ALC_T_DB_SEG,
		     (DbTable *) tb,
		     (void *) tb->seg[six],
		     sizeof(HashDbTerm*)*SEGSZ);
	tb->seg[six] = 0;
	tb->nslots -= SEGSZ;
    }
}


/* Search a list of tuples for a matching key */

static HashDbTerm* search_list(DbTableHash* tb, Eterm key, 
			       HashValue hval, HashDbTerm *list)
{
    while (list != 0) {
	if ((list->hvalue == hval) && EQ(key, GETKEY(tb, list->dbterm.tpl)))
	    return list;
	list = list->next;
    }
    return 0;
}


/* This function is called by the next AND the select BIF */
/* It return the next object in a table                   */

static HashDbTerm* next(DbTableHash *tb, Uint *iptr, HashDbTerm *list)
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


static int realloc_counter(DbTableCommon *tb, HashDbTerm** bp, Uint sz, 
			   Eterm new_counter, int counterpos)
{
    HashDbTerm* b = *bp;
    return db_realloc_counter(tb, (void **) bp, &(b->dbterm),
			      ((char *) &(b->dbterm)) - ((char *) b),
			      sz, new_counter, counterpos);
}

int db_delete_all_objects_hash(Process* p, DbTable* tbl)
{
    if (tbl->hash.common.status & DB_FIXED) {
	db_mark_all_deleted_hash(tbl);
    } else {
	db_free_table_hash(tbl);
	db_create_hash(p, tbl);
	tbl->hash.common.nitems = 0;
    }
    return 0;
}

void db_foreach_offheap_hash(DbTable *tbl,
			     void (*func)(ErlOffHeap *, void *),
			     void * arg)
{
    DbTableHash *tb = &tbl->hash;
    HashDbTerm* list;
    int i;

    for (i = 0; i < tb->nactive; i++) {
	list = BUCKET(tb,i);
	while(list != 0) {
	    (*func)(&(list->dbterm.off_heap), arg);
	    list = list->next;
	}
    }
}

#ifdef HARDDEBUG

void db_check_table_hash(DbTable *tbl)
{
    DbTableHash *tb = &tbl->hash;
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
