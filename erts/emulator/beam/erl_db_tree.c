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
** Implementation of ordered ETS tables.
** The tables are implemented as AVL trees (Published by Adelson-Velski 
** and Landis). A nice source for learning about these trees is
** Wirth's Algorithms + Datastructures = Programs.
** The implementation here is however not made with recursion
** as the examples in Wirths book are.
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

#include "erl_db_tree.h"

#define GETKEY(dtt, tplp)   (*((tplp) +  (dtt)->keypos))
#define GETKEY_WITH_POS(Keypos, Tplp) (*((Tplp) + Keypos))

/*
** A stack of this size is enough for an AVL tree with more than
** 0xFFFFFFFF elements. May be subject to change if
** the datatype of the element counter is changed to a 64 bit integer.
** The Maximal height of an AVL tree is calculated as:
** h(n) <= 1.4404 * log(n + 2) - 0.328
** Where n denotes the number of nodes, h(n) the height of the tree
** with n nodes and log is the binary logarithm.
*/

#define STACK_NEED 50
#define TREE_MAX_ELEMENTS 0xFFFFFFFFUL

#define PUSH_NODE(Dtt, Tdt)                             \
((Dtt)->stack[(Dtt)->stack_pos++] = Tdt)

#define POP_NODE(Dtt)				\
     ((Dtt->stack_pos) ? 			\
      (Dtt)->stack[--((Dtt)->stack_pos)] : NULL)

#define TOP_NODE(Dtt)                           \
     ((Dtt->stack_pos) ? 			\
      (Dtt)->stack[(Dtt)->stack_pos - 1] : NULL)

#define EMPTY_NODE(Dtt) (TOP_NODE(Dtt) == NULL)

     
/*
** Some macros for "direction stacks"
*/
#define DIR_LEFT 0
#define DIR_RIGHT 1
#define DIR_END 2 

/*
 * Special binary flag
 */
#define BIN_FLAG_ALL_OBJECTS         BIN_FLAG_USR1

/* 
** Debugging
*/
#ifdef HARDDEBUG
static TreeDbTerm *traverse_until(TreeDbTerm *t, int *current, int to);
static void check_slot_pos(DbTableTree *tb);
static void check_saved_stack(DbTableTree *tb);
static int check_table_tree(TreeDbTerm *t);

#define TREE_DEBUG
#endif

#ifdef TREE_DEBUG
/*
** Primitive trace macro
*/
#define DBG erl_printf(CERR,"%d\r\n",__LINE__)

/*
** Debugging dump
*/

static int do_dump_tree2(CIO fd, int show, TreeDbTerm *t, int offset);

#else

#define DBG /* nothing */

#endif

/*
** Datatypes
*/

/* 
 * This structure is filled in by analyze_pattern() for the select 
 * functions.
 */
struct mp_info {
    int all_objects;		/* True if complete objects are always
				 * returned from the match_spec (can use 
				 * copy_shallow on the return value) */
    int something_can_match;	/* The match_spec is not "impossible" */
    int some_limitation;	/* There is some limitation on the search
				 * area, i. e. least and/or most is set.*/
    int got_partial;		/* The limitation has a partially bound
				 * key */
    Eterm least;		/* The lowest matching key (possibly 
				 * partially bound expression) */
    Eterm most;                 /* The highest matching key (possibly 
				 * partially bound expression) */

    TreeDbTerm *save_term;      /* If the key is completely bound, this
				 * will be the Tree node we're searching
				 * for, otherwise it will be useless */
    Binary *mp;                 /* The compiled match program */
};

/*
 * Used by doit_select(_chunk)
 */
struct select_context {
    Process *p;
    Eterm accum;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    Sint32 max;
    int keypos;
    int all_objects;
    Sint got;
    Sint chunk_size;
};

/*
 * Used by doit_select_delete
 */
struct select_delete_context {
    Process *p;
    DbTableTree *tb;
    Uint accum;
    Binary *mp;
    Eterm end_condition;
    int erase_lastterm;
    TreeDbTerm *lastterm;
    Sint32 max;
    int keypos;
};


/*
** Forward declarations 
*/
static TreeDbTerm *linkout_tree(DbTableTree *tb, Eterm key);
static TreeDbTerm *linkout_object_tree(DbTableTree *tb, 
				       Eterm object);
static void do_free_tree(TreeDbTerm *root);
static TreeDbTerm* get_term(TreeDbTerm* old, 
			    Eterm obj);
static void free_term(TreeDbTerm* p);
static int balance_left(TreeDbTerm **this); 
static int balance_right(TreeDbTerm **this); 
static int delsub(TreeDbTerm **this); 
static TreeDbTerm *slot_search(Process *p, DbTableTree *tb, sint32 slot);
static int realloc_counter(TreeDbTerm** bp, Uint sz, 
			   Eterm new_counter, int counterpos);
static TreeDbTerm *find_node(DbTableTree *tb, Eterm key);
static TreeDbTerm **find_node2(DbTableTree *tb, Eterm key);
static TreeDbTerm *find_next(DbTableTree *tb, Eterm key);
static TreeDbTerm *find_prev(DbTableTree *tb, Eterm key);
static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, Eterm key);
static TreeDbTerm *find_prev_from_pb_key(DbTableTree *tb, Eterm key);
static void traverse_backwards(DbTableTree *tb,
			       Eterm lastkey,
			       int (*doit)(TreeDbTerm *, void *, int),
			       void *context); 
static void traverse_forward(DbTableTree *tb,
			     Eterm lastkey,
			     int (*doit)(TreeDbTerm *, void *, int),
			     void *context); 
static int key_given(DbTableTree *tb, Eterm pattern, TreeDbTerm **ret,
		     Eterm *partly_bound_key);
static int cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key);
static int do_cmp_partly_bound(Eterm a, Eterm b, int *done);

static int analyze_pattern(DbTableTree *tb, Eterm pattern, 
			   struct mp_info *mpi);
static int doit_select(TreeDbTerm *this, void *ptr, int forward);
static int doit_select_chunk(TreeDbTerm *this, void *ptr, int forward);
static int doit_select_delete(TreeDbTerm *this, void *ptr, int forward);
static int do_dump_tree(CIO fd, int show, TreeDbTerm *t);

static int partly_bound_can_match_lesser(Eterm partly_bound_1, 
					 Eterm partly_bound_2);
static int partly_bound_can_match_greater(Eterm partly_bound_1, 
					  Eterm partly_bound_2); 
static int do_partly_bound_can_match_lesser(Eterm a, Eterm b, 
					    int *done);
static int do_partly_bound_can_match_greater(Eterm a, Eterm b, 
					     int *done);
static BIF_RETTYPE ets_select_reverse(Process *p, Eterm a1, 
				      Eterm a2, Eterm a3);

/*
** Static variables
*/

Export ets_select_reverse_exp;

/*
** External interface 
*/
void db_initialize_tree(void)
{
    extern Eterm* em_apply_bif;
    memset(&ets_select_reverse_exp, 0, sizeof(Export));
    ets_select_reverse_exp.address = 
	&ets_select_reverse_exp.code[3];
    ets_select_reverse_exp.code[0] = am_ets;
    ets_select_reverse_exp.code[1] = am_reverse;
    ets_select_reverse_exp.code[2] = 3;
    ets_select_reverse_exp.code[3] =
	(Eterm) em_apply_bif;
    ets_select_reverse_exp.code[4] = 
	(Eterm) &ets_select_reverse;
    return;
};

/*
** Table interface routines ie what's called by the bif's 
*/

int db_create_tree(Process *p, DbTableTree *tb)
{
    tb->root = NULL;
    tb->stack = safe_alloc_from(54, sizeof(TreeDbTerm *) * STACK_NEED);
    tb->stack_pos = 0;
    tb->slot_pos = 0;
    return DB_ERROR_NONE;
}

int db_first_tree(Process *p, DbTableTree *tb, Eterm *ret)
{
    TreeDbTerm *this;
    Eterm e;
    Eterm *hp;
    Uint sz;

    /* Walk down to the tree to the left */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    while (this != NULL) {
	PUSH_NODE(tb, this);
	this = this->left;
    }
    this = TOP_NODE(tb);
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    tb->slot_pos = 1; /* Slot pos is position in order, 
			 if it's other than 0 */

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_next_tree(Process *p, DbTableTree *tb, 
		 Eterm key, Eterm *ret)
{
    TreeDbTerm *this;
    Eterm e;
    Eterm *hp;
    Uint sz;

    if (is_atom(key) && key == am_EOT)
	return DB_ERROR_BADKEY;
    if (( this = find_next(tb, key) ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_last_tree(Process *p, DbTableTree *tb, Eterm *ret)
{
    TreeDbTerm *this;
    Eterm e;
    Eterm *hp;
    Uint sz;

    /* Walk down to the tree to the left */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    while (this != NULL) {
	PUSH_NODE(tb, this);
	this = this->right;
    }
    this = TOP_NODE(tb);
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    tb->slot_pos = tb->nitems; /* Slot pos is position in order, 
				  if it's other than 0 */

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_prev_tree(Process *p, DbTableTree *tb, 
		 Eterm key, Eterm *ret)
{
    TreeDbTerm *this;
    Eterm e;
    Eterm *hp;
    Uint sz;

    if (is_atom(key) && key == am_EOT)
	return DB_ERROR_BADKEY;
    if (( this = find_prev(tb, key) ) == NULL) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }
    
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_update_counter_tree(Process *p, DbTableTree *tb, 
			   Eterm key, Eterm incr, int counterpos, Eterm *ret)
{
    TreeDbTerm **bp = find_node2(tb, key);
    TreeDbTerm *b;
    int res;
    if (bp == NULL)
	return DB_ERROR_BADKEY;
    b = *bp;
    if (counterpos <= 0)
	counterpos = tb->keypos + 1;
    res = db_do_update_counter(p, (void *) bp, (*bp)->dbterm.tpl,
			       counterpos, 
			       (int (*)(void *, Uint, Eterm, int))
			       &realloc_counter, incr, ret);
    if (*bp != b) /* May be reallocated in which case 
		     the saved stack is messed up, clear stck if so. */
	tb->stack_pos = tb->slot_pos = 0;
    return res;
}

int db_put_tree(Process *proc, DbTableTree *tb, 
		Eterm obj, Eterm *ret)
{
    /* Non recursive insertion in AVL tree, building our own stack */
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    int c;
    Eterm key;
    int dir;
    TreeDbTerm *p1, *p2, *p;

    key = GETKEY(tb, tuple_val(obj));

    tb->stack_pos = tb->slot_pos = 0;

    dstack[dpos++] = DIR_END;
    for (;;)
	if (!*this) { /* Found our place */
	    state = 1;
	    if (tb->nitems < TREE_MAX_ELEMENTS)
		tb->nitems++;
	    else
		return DB_ERROR_SYSRES;
	    *this = get_term(NULL, obj);
	    (*this)->balance = 0;
	    (*this)->left = (*this)->right = NULL;
	    break;
	} else if ((c = cmp(key,GETKEY(tb,(*this)->dbterm.tpl))) < 0) { 
	    /* go left */
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key and this is a set, replace. */
	    *this = get_term(*this, obj);
	    break;
	}
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	p = *this;
	if (dir == DIR_LEFT) {
	    switch (p->balance) {
	    case 1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = -1;
		break;
	    case -1: /* The icky case */
		p1 = p->left;
		if (p1->balance == -1) { /* Single LL rotation */
		    p->left = p1->right;
		    p1->right = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RR rotation */
		    p2 = p1->right;
		    p1->right = p2->left;
		    p2->left = p1;
		    p->left = p2->right;
		    p2->right = p;
		    p->balance = (p2->balance == -1) ? +1 : 0;
		    p1->balance = (p2->balance == 1) ? -1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0;
		state = 0;
		break;
	    }
	} else { /* dir == DIR_RIGHT */
	    switch (p->balance) {
	    case -1:
		p->balance = 0;
		state = 0;
		break;
	    case 0:
		p->balance = 1;
		break;
	    case 1:
		p1 = p->right;
		if (p1->balance == 1) { /* Single RR rotation */
		    p->right = p1->left;
		    p1->left = p;
		    p->balance = 0;
		    (*this) = p1;
		} else { /* Double RL rotation */
		    p2 = p1->left;
		    p1->left = p2->right;
		    p2->right = p1;
		    p->right = p2->left;
		    p2->left = p;
		    p->balance = (p2->balance == 1) ? -1 : 0;
		    p1->balance = (p2->balance == -1) ? 1 : 0;
		    (*this) = p2;
		}
		(*this)->balance = 0; 
		state = 0;
		break;
	    }
	}
    }
    *ret = am_true;    
    return DB_ERROR_NONE;
}

int db_get_tree(Process *p, DbTableTree *tb, 
		Eterm key, Eterm *ret)
{
    Eterm copy;
    Eterm *hp;
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * The list created around it is purely for interface conformance.
     */
    
    this = find_node(tb,key);
    if (this == NULL) {
	*ret = NIL;
    } else {
	hp = HAlloc(p, this->dbterm.size + 2);
	copy = copy_shallow(this->dbterm.v, 
			    this->dbterm.size, 
			    &hp, 
			    &p->off_heap);
	*ret = CONS(hp, copy, NIL);
    }
    return DB_ERROR_NONE;
}

int db_member_tree(Process *p, DbTableTree *tb, 
		   Eterm key, Eterm *ret)
{
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * The list created around it is purely for interface conformance.
     */
    
    this = find_node(tb,key);
    if (this == NULL) {
	*ret = am_false;
    } else {
	*ret = am_true;
    }
    return DB_ERROR_NONE;
}

int db_get_element_tree(Process *p, DbTableTree *tb, 
			Eterm key, int ndex, Eterm *ret)
{
    /*
     * Look the node up:
     */
    Eterm *hp;
    TreeDbTerm *this;

    /*
     * This is always a set, so we know exactly how large
     * the data is when we have found it.
     * No list is created around elements in set's so there are no list
     * around the element here either.
     */
    
    this = find_node(tb,key);
    if (this == NULL) {
	return DB_ERROR_BADKEY;
    } else {
	Eterm element;
	Uint sz;
	if (ndex > arityval(this->dbterm.tpl[0])) {
	    return DB_ERROR_BADPARAM;
	}
	element = this->dbterm.tpl[ndex];
	sz = size_object(element);
	hp = HAlloc(p, sz);
	*ret = copy_struct(element, 
			   sz, 
			   &hp, 
			   &p->off_heap);
    }
    return DB_ERROR_NONE;
}

int db_erase_tree(Process *p, DbTableTree *tb, 
		  Eterm key, Eterm *ret)
{
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_tree(tb, key)) != NULL) {
	free_term(res);
    }
    return DB_ERROR_NONE;
}

int db_erase_object_tree(Process *p, DbTableTree *tb, 
			 Eterm object, Eterm *ret)
{
    TreeDbTerm *res;

    *ret = am_true;

    if ((res = linkout_object_tree(tb, object)) != NULL) {
	free_term(res);
    }
    return DB_ERROR_NONE;
}


int db_slot_tree(Process *p, DbTableTree *tb, 
		 Eterm slot_term, Eterm *ret)
{
    sint32 slot;
    TreeDbTerm *st;
    Eterm *hp;
    Eterm copy;

    /*
     * The notion of a "slot" is not natural in a tree, but we try to
     * simulate it by giving the n'th node in the tree instead.
     * Traversing a tree in this way is not very convenient, but by
     * using the saved stack we at least sometimes will get acceptable 
     * performance.
     */

    if (is_not_small(slot_term) ||
	((slot = signed_val(slot_term)) < 0) ||
	(slot > tb->nitems))
	return DB_ERROR_BADPARAM;

    if (slot == tb->nitems) {
	*ret = am_EOT;
	return DB_ERROR_NONE;
    }

    /* 
     * We use the slot position and search from there, slot positions 
     * are counted from 1 and up.
     */
    ++slot;
    st = slot_search(p, tb, slot); 
    if (st == NULL) {
	*ret = am_false;
	return DB_ERROR_UNSPEC;
    }
    hp = HAlloc(p, st->dbterm.size + 2);
    copy = copy_shallow(st->dbterm.v, 
			st->dbterm.size, 
			&hp, 
			&p->off_heap);
    *ret = CONS(hp, copy, NIL);
    return DB_ERROR_NONE;
}



static BIF_RETTYPE ets_select_reverse(Process *p, Eterm a1, Eterm a2, Eterm a3)
{
    Eterm list;
    Eterm result;
    Eterm* hp;
    Eterm* hend;

    int max_iter = CONTEXT_REDS * 10;

    if (is_nil(a1)) {
	hp = HAlloc(p, 3);
	BIF_RET(TUPLE2(hp,a2,a3));
    } else if (is_not_list(a1)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    
    list = a1;
    result = a2;
    hp = hend = NULL;
    while (is_list(list)) {
	Eterm* pair = list_val(list);
	if (--max_iter == 0) {
	    BUMP_ALL_REDS(p);
	    HRelease(p, hp);
	    BIF_TRAP3(&ets_select_reverse_exp, p, list, result, a3);
	}
	if (hp == hend) {
	    hp = HAlloc(p, 64);
	    hend = hp + 64;
	}
	result = CONS(hp, CAR(pair), result);
	hp += 2;
	list = CDR(pair);
    }
    if (is_not_nil(list))  {
	goto error;
    }
    HRelease(p, hp);
    BUMP_REDS(p,CONTEXT_REDS - max_iter / 10);
    hp = HAlloc(p,3);
    BIF_RET(TUPLE2(hp, result, a3));
}

static BIF_RETTYPE bif_trap1(Export *bif,
			     Process *p, 
			     Eterm p1) 
{
    BIF_TRAP1(bif, p, p1);
}
    
static BIF_RETTYPE bif_trap3(Export *bif,
			     Process *p, 
			     Eterm p1, 
			     Eterm p2,
			     Eterm p3) 
{
    BIF_TRAP3(bif, p, p1, p2, p3);
}
    
/*
** This is called either when the select bif traps or when ets:select/1 
** is called. It does mostly the same as db_select_tree and may in either case
** trap to itself again (via the ets:select/1 bif).
** Note that this is common for db_select_tree and db_select_chunk_tree.
*/
int db_select_tree_continue(Process *p, 
			    DbTableTree *tb,
			    Eterm continuation,
			    Eterm *ret)
{
    struct select_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey;
    Eterm end_condition; 
    Binary *mp;
    Eterm key;
    Eterm *tptr;
    Sint chunk_size;
    int reverse;


#define RET_TO_BIF(Term, State) do { *ret = (Term); return State; } while(0);

    /* Decode continuation. We know it's a tuple but not the arity or 
       anything else */

    tptr = tuple_val(continuation);

    if (arityval(*tptr) != 8)
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    if (!is_small(tptr[4]) || !is_binary(tptr[5]) || 
	!(is_list(tptr[6]) || tptr[6] == NIL) || !is_small(tptr[7]) ||
	!is_small(tptr[8]))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    
    lastkey = tptr[2];
    end_condition = tptr[3];
    mp = ((ProcBin *) binary_val(tptr[5]))->val;
    if (!(mp->flags & BIN_FLAG_MATCH_PROG))
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    chunk_size = signed_val(tptr[4]);

    sc.p = p;
    sc.accum = tptr[6];
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.lastobj = NULL;
    sc.max = 1000;
    sc.keypos = tb->keypos;
    sc.all_objects = mp->flags & BIN_FLAG_ALL_OBJECTS;
    sc.chunk_size = chunk_size;
    reverse = unsigned_val(tptr[7]);
    sc.got = signed_val(tptr[8]);


    if (chunk_size) {
	if (reverse) {
	    traverse_backwards(tb, lastkey, &doit_select_chunk, &sc); 
	} else {
	    traverse_forward(tb, lastkey, &doit_select_chunk, &sc); 
	}
    } else {
	if (reverse) {
	    traverse_forward(tb, lastkey, &doit_select, &sc);
	} else {
	    traverse_backwards(tb, lastkey, &doit_select, &sc);
	}
    }

    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	if (chunk_size) {
	    Eterm *hp; 
	    unsigned sz;

	    if (sc.got < chunk_size || sc.lastobj == NULL) { 
		/* end of table, sc.lastobj may be NULL as we may have been
		   at the very last object in the table when trapping. */
		if (!sc.got) {
		    RET_TO_BIF(am_EOT, DB_ERROR_NONE);
		} else {
		    RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
					 sc.accum, NIL, am_EOT), 
			       DB_ERROR_NONE);
		}
	    }

	    key = GETKEY(tb, sc.lastobj);

	    sz = size_object(key);
	    hp = HAlloc(p, 9 + sz);
	    key = copy_struct(key, sz, &hp, &(p->off_heap));
	    continuation = TUPLE8
		(hp,
		 tptr[1],
		 key,
		 tptr[3], 
		 tptr[4],
		 tptr[5],
		 NIL,
		 tptr[7],
		 make_small(0));
	    RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
				 sc.accum, NIL, continuation), 
		       DB_ERROR_NONE);
	} else {
	    RET_TO_BIF(sc.accum, DB_ERROR_NONE);
	}
    }	
    key = GETKEY(tb, sc.lastobj);
    if (chunk_size) {
	if (end_condition != NIL && 
	    ((!reverse && cmp_partly_bound(end_condition,key) < 0) ||
	     (reverse && cmp_partly_bound(end_condition,key) > 0))) { 
	    /* done anyway */
	    if (!sc.got) {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p, 
				     sc.accum, NIL, am_EOT), 
			   DB_ERROR_NONE);
	    }
	}
    } else {
	if (end_condition != NIL && 
	    ((!reverse && cmp_partly_bound(end_condition,key) > 0) ||
	     (reverse && cmp_partly_bound(end_condition,key) < 0))) { 
	    /* done anyway */
	    RET_TO_BIF(sc.accum,DB_ERROR_NONE);
	}
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));
    continuation = TUPLE8
	(hp,
	 tptr[1],
	 key,
	 tptr[3], 
	 tptr[4],
	 tptr[5],
	 sc.accum,
	 tptr[7],
	 make_small(sc.got));
    RET_TO_BIF(bif_trap1(bif_export[BIF_ets_select_1], p, continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}


int db_select_tree(Process *p, DbTableTree *tb, 
		   Eterm pattern, int reverse, Eterm *ret)
{
    struct select_context sc;
    struct mp_info mpi;
    Eterm lastkey = NIL;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;


#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);       	\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = NIL;
    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->keypos;
    sc.got = 0;
    sc.chunk_size = 0;

    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(NIL,DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    if (!mpi.got_partial && mpi.some_limitation && 
	cmp(mpi.least,mpi.most) == 0) {
	doit_select(mpi.save_term,&sc,0 /* direction doesn't matter */);
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    if (reverse) {
	if (mpi.some_limitation) {
	    if ((this = find_prev_from_pb_key(tb, mpi.least)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.most;
	}
	
	traverse_forward(tb, lastkey, &doit_select, &sc);
    } else {
	if (mpi.some_limitation) {
	    if ((this = find_next_from_pb_key(tb, mpi.most)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.least;
	}
	
	traverse_backwards(tb, lastkey, &doit_select, &sc);
    }
#ifdef HARDDEBUG
	erl_printf(CERR,"Least: ");
	display(mpi.least,CERR);
	erl_printf(CERR,"\r\nMost: ");
	display(mpi.most,CERR);
	erl_printf(CERR,"\r\n");
#endif
    BUMP_REDS(p, 1000 - sc.max);
    if (sc.max > 0) {
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));

    if (mpi.all_objects)
	(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	    
    continuation = TUPLE8
	(hp,
	 tb->id,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 make_small(0), /* Chunk size of zero means not chunked to the
			   continuation BIF */
	 db_make_mp_binary(p,mpi.mp),
	 sc.accum,
	 make_small(reverse),
	 make_small(sc.got));

    /* Don't free mpi.mp, so don't use macro */
    *ret = bif_trap1(bif_export[BIF_ets_select_1], p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

int db_select_chunk_tree(Process *p, DbTableTree *tb, 
			 Eterm pattern, Sint chunk_size,
			 int reverse,
			 Eterm *ret)
{
    struct select_context sc;
    struct mp_info mpi;
    Eterm lastkey = NIL;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;


#define RET_TO_BIF(Term,RetVal) do { 		\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);	\
	}					\
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = NIL;
    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->keypos;
    sc.got = 0;
    sc.chunk_size = chunk_size;

    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(NIL,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;
    sc.all_objects = mpi.all_objects;

    if (!mpi.got_partial && mpi.some_limitation && 
	cmp(mpi.least,mpi.most) == 0) {
	doit_select(mpi.save_term,&sc, 0 /* direction doesn't matter */);
	if (sc.accum != NIL) {
	    hp=HAlloc(p, 3);
	    RET_TO_BIF(TUPLE2(hp,sc.accum,am_EOT),DB_ERROR_NONE);
	} else {
	    RET_TO_BIF(am_EOT,DB_ERROR_NONE);
	}
    }

    if (reverse) {
	if (mpi.some_limitation) {
	    if ((this = find_next_from_pb_key(tb, mpi.most)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.least;
	}

	traverse_backwards(tb, lastkey, &doit_select_chunk, &sc);
    } else {
	if (mpi.some_limitation) {
	    if ((this = find_prev_from_pb_key(tb, mpi.least)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	    sc.end_condition = mpi.most;
	}

	traverse_forward(tb, lastkey, &doit_select_chunk, &sc);
    }

    BUMP_REDS(p, 1000 - sc.max);
    if (sc.max > 0) {
	Eterm *hp; 
	unsigned sz;

	if (sc.got < chunk_size ||
	    sc.lastobj == NULL) { 
	    /* We haven't got all and we haven't trapped 
	       which should mean we are at the end of the 
	       table, sc.lastobj may be NULL if the table was empty */
	    
	    if (!sc.got) {
		RET_TO_BIF(am_EOT, DB_ERROR_NONE);
	    } else {
		RET_TO_BIF(bif_trap3(&ets_select_reverse_exp, p,
				     sc.accum, NIL, am_EOT), 
			   DB_ERROR_NONE);
	    }
	}

	key = GETKEY(tb, sc.lastobj);
	sz = size_object(key);
	hp = HAlloc(p, 9 + sz);
	key = copy_struct(key, sz, &hp, &(p->off_heap));
	if (mpi.all_objects)
	    (mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	
	continuation = TUPLE8
	    (hp,
	     tb->id,
	     key,
	     sc.end_condition, /* From the match program, 
				  needn't be copied */
	     make_small(chunk_size),
	     db_make_mp_binary(p,mpi.mp),
	     NIL,
	     make_small(reverse),
	     make_small(0));
	/* Don't let RET_TO_BIF macro free mpi.mp*/
	*ret = bif_trap3(&ets_select_reverse_exp, p,
			 sc.accum, NIL, continuation);
	return DB_ERROR_NONE; 
    }

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);
    hp = HAlloc(p, 9 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));

    if (mpi.all_objects)
	(mpi.mp)->flags |= BIN_FLAG_ALL_OBJECTS;
	    
    continuation = TUPLE8
	(hp,
	 tb->id,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 make_small(chunk_size),
	 db_make_mp_binary(p,mpi.mp),
	 sc.accum,
	 make_small(reverse),
	 make_small(sc.got));
    /* Don't let RET_TO_BIF macro free mpi.mp*/
    *ret = bif_trap1(bif_export[BIF_ets_select_1], p, continuation);
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

/*
** This is called when select_delete traps
*/
int db_select_delete_continue_tree(Process *p, 
				   DbTableTree *tb,
				   Eterm continuation,
				   Eterm *ret)
{
    struct select_delete_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey;
    Eterm end_condition; 
    Binary *mp;
    Eterm key;
    Eterm *tptr;


#define RET_TO_BIF(Term, State) do { 		\
	if (sc.erase_lastterm) {		\
	    free_term(sc.lastterm);		\
	}					\
	*ret = (Term); 				\
	return State; 				\
    } while(0);

    /* Decode continuation. We know it's correct, this can only be called
       by trapping */

    tptr = tuple_val(continuation);

    lastkey = tptr[2];
    end_condition = tptr[3];

    sc.erase_lastterm = 0; /* Before first RET_TO_BIF */
    sc.lastterm = NULL;

    mp = ((ProcBin *) binary_val(tptr[4]))->val;
    sc.p = p;
    sc.tb = tb;
    if (is_big(tptr[5])) {
	sc.accum = big_to_uint32(tptr[5]);
    } else {
	sc.accum = unsigned_val(tptr[5]);
    }
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.max = 1000;
    sc.keypos = tb->keypos;

    traverse_backwards(tb, lastkey, &doit_select_delete, &sc);

    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	RET_TO_BIF(make_small_or_big(sc.accum, p), DB_ERROR_NONE);
    }	
    key = GETKEY(tb, (sc.lastterm)->dbterm.tpl);
    if (end_condition != NIL && 
	cmp_partly_bound(end_condition,key) > 0) { /* done anyway */
	RET_TO_BIF(make_small_or_big(sc.accum,p),DB_ERROR_NONE);
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    hp = HAlloc(p, 6 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));
    continuation = TUPLE5
	(hp,
	 tptr[1],
	 key,
	 tptr[3], 
	 tptr[4],
	 make_small_or_big(sc.accum,p));
    RET_TO_BIF(bif_trap1(&ets_select_delete_continue_exp, p, continuation), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF
}

int db_select_delete_tree(Process *p, DbTableTree *tb, 
			  Eterm pattern, Eterm *ret)
{
    struct select_delete_context sc;
    struct mp_info mpi;
    Eterm lastkey = NIL;
    Eterm key;
    Eterm continuation;
    unsigned sz;
    Eterm *hp; 
    TreeDbTerm *this;
    int errcode;


#define RET_TO_BIF(Term,RetVal) do { 	       	\
	if (mpi.mp != NULL) {			\
	    erts_match_set_free(mpi.mp);       	\
	}					\
	if (sc.erase_lastterm) {                \
	    free_term(sc.lastterm);             \
	}                                       \
	*ret = (Term); 				\
	return RetVal; 			        \
    } while(0)

    mpi.mp = NULL;

    sc.accum = 0;
    sc.erase_lastterm = 0;
    sc.lastterm = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->keypos;
    sc.tb = tb;
    
    if ((errcode = analyze_pattern(tb, pattern, &mpi)) != DB_ERROR_NONE) {
	RET_TO_BIF(0,errcode);
    }

    if (!mpi.something_can_match) {
	RET_TO_BIF(make_small(0),DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    sc.mp = mpi.mp;

    if (!mpi.got_partial && mpi.some_limitation && 
	cmp(mpi.least,mpi.most) == 0) {
	doit_select_delete(mpi.save_term,&sc, 0 /* direction doesn't 
						   matter */);
	RET_TO_BIF(make_small_or_big(sc.accum,p),DB_ERROR_NONE);
    }

    if (mpi.some_limitation) {
	if ((this = find_next_from_pb_key(tb, mpi.most)) != NULL) {
	    lastkey = GETKEY(tb, this->dbterm.tpl);
	}
	sc.end_condition = mpi.least;
    }

    traverse_backwards(tb, lastkey, &doit_select_delete, &sc);
    BUMP_REDS(p, 1000 - sc.max);

    if (sc.max > 0) {
	RET_TO_BIF(make_small_or_big(sc.accum,p), DB_ERROR_NONE);
    }

    key = GETKEY(tb, (sc.lastterm)->dbterm.tpl);
    sz = size_object(key);
    hp = HAlloc(p, 6 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));
	    
    continuation = TUPLE5
	(hp,
	 tb->id,
	 key,
	 sc.end_condition, /* From the match program, needn't be copied */
	 db_make_mp_binary(p,mpi.mp),
	 make_small_or_big(sc.accum, p));

    /* Don't free mpi.mp, so don't use macro */
    if (sc.erase_lastterm) {
	free_term(sc.lastterm);
    }
    *ret = bif_trap1(&ets_select_delete_continue_exp, p, continuation); 
    return DB_ERROR_NONE;

#undef RET_TO_BIF

}

/*
** Other interface routines (not directly coupled to one bif)
*/

/* Get the memory consumption for a tree table */
int db_info_memory_tree(Process *p, DbTableTree *tb,
			Eterm *ret, 
			int *reds)
{
    Uint sum;
    sum = sizeof (DbTableHash) / sizeof (Eterm);
    sum += do_dump_tree(COUT, 0, tb->root); /* Nothing is written on 
					       COUT anyway */
    *ret = make_small(sum);
    *reds = (tb->nitems+1)/20;
    return DB_ERROR_NONE;
}


/* Display hash table contents (for dump) */
void db_print_tree(CIO fd, 
		   int show,
		   DbTableTree *tb, 
		   int *sum)
{
#ifdef TREE_DEBUG
    if (show)
	erl_printf(fd, "\r\nTree data dump:\r\n"
		   "------------------------------------------------\r\n");
    *sum += do_dump_tree2(fd, show, tb->root, 0);
    if (show)
	erl_printf(fd, "\r\n"
		   "------------------------------------------------\r\n");
#else
    erl_printf(fd,"Ordered set (AVL tree), Elements: %d\n", tb->nitems);
    *sum += do_dump_tree(fd,show, tb->root);
#endif
}

/* release all memory occupied by a single table */
void free_tree_table(DbTableTree *tb)
{
    do_free_tree(tb->root);
    if (tb->stack)
	sys_free(tb->stack);
}

/*
** Functions for internal use
*/

static TreeDbTerm *linkout_tree(DbTableTree *tb, 
				Eterm key)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    int c;
    int dir;
    TreeDbTerm *q = NULL;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    tb->stack_pos = tb->slot_pos = 0;
    dstack[dpos++] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = cmp(key,GETKEY(tb,(*this)->dbterm.tpl))) < 0) { 
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the one to delete*/
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub(this);
	    }
	    --(tb->nitems);
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left(this);
	} else {
	    state = balance_right(this);
	}
    }
    return q;
}

static TreeDbTerm *linkout_object_tree(DbTableTree *tb, 
				       Eterm object)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    int c;
    int dir;
    TreeDbTerm *q = NULL;
    Eterm key;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    
    key = GETKEY(tb, tuple_val(object));

    tb->stack_pos = tb->slot_pos = 0;
    dstack[dpos++] = DIR_END;
    for (;;) {
	if (!*this) { /* Failure */
	    return NULL;
	} else if ((c = cmp(key,GETKEY(tb,(*this)->dbterm.tpl))) < 0) { 
	    dstack[dpos++] = DIR_LEFT;
	    tstack[tpos++] = this;
	    this = &((*this)->left);
	} else if (c > 0) { /* go right */
	    dstack[dpos++] = DIR_RIGHT;
	    tstack[tpos++] = this;
	    this = &((*this)->right);
	} else { /* Equal key, found the only possible matching object*/
	    if (!eq(object,make_tuple((*this)->dbterm.tpl))) {
		return NULL;
	    }
	    q = (*this);
	    if (q->right == NULL) {
		(*this) = q->left;
		state = 1;
	    } else if (q->left == NULL) {
		(*this) = q->right;
		state = 1;
	    } else {
		dstack[dpos++] = DIR_LEFT;
		tstack[tpos++] = this;
		state = delsub(this);
	    }
	    --(tb->nitems);
	    break;
	}
    }
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left(this);
	} else {
	    state = balance_right(this);
	}
    }
    return q;
}

/*
** For the select functions, analyzes the pattern and determines which
** part of the tree should be searched. Also compiles the match program
*/
static int analyze_pattern(DbTableTree *tb, Eterm pattern, 
			   struct mp_info *mpi)
{
    Eterm lst, tpl, ttpl;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    Eterm *ptpl;
    int i;
    int num_heads = 0;
    Eterm key;
    Eterm partly_bound;
    int res;
    Eterm least = 0;
    Eterm most = 0;
    TreeDbTerm *this;

    mpi->some_limitation = 1;
    mpi->got_partial = 0;
    mpi->something_can_match = 0;
    mpi->mp = NULL;
    mpi->all_objects = 1;
    mpi->save_term = NULL;

    for (lst = pattern; is_list(lst); lst = CDR(list_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	return DB_ERROR_BADPARAM;
    }
    if (num_heads > 10) {
	buff = safe_alloc(sizeof(Eterm) * num_heads * 3);
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
		sys_free(buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
	ptpl = tuple_val(ttpl);
	if (ptpl[0] != make_arityval(3U)) {
	    if (buff != sbuff) { 
		sys_free(buff);
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

	partly_bound = NIL;
	res = key_given(tb, tpl, &this, &partly_bound);
	if ( res >= 0 ) {   /* Can match something */
	    key = 0;
	    mpi->something_can_match = 1;
	    if (res > 0) {
		mpi->save_term = this;
		key = GETKEY(tb,tuple_val(tpl)); 
	    } else if (partly_bound != NIL) {
		mpi->got_partial = 1;
		key = partly_bound;
	    } else {
		mpi->some_limitation = 0;
	    }
	    if (key != 0) {
		if (least == 0 || 
		    partly_bound_can_match_lesser
		    (key,least)) {
		    least = key;
		}
		if (most == 0 || 
		    partly_bound_can_match_greater
		    (key,most)) {
		    most = key;
		}
	    }
	}
    }
    mpi->least = least;
    mpi->most = most;
    if (mpi->something_can_match) {
	if ((mpi->mp = db_match_compile(matches, guards, bodies,
					num_heads, DCOMP_TABLE, NULL)) 
	    == NULL) {
	    if (buff != sbuff) { 
		sys_free(buff);
	    }
	    return DB_ERROR_BADPARAM;
	}
    }
    if (buff != sbuff) { 
	sys_free(buff);
    }
    return DB_ERROR_NONE;
}

static int do_dump_tree(CIO fd, int show, TreeDbTerm *t)
{
    int sum = 0;
    if (t == NULL)
	return 0;
    sum += do_dump_tree(fd, show, t->left);
    if (show) {
	display(make_tuple(t->dbterm.tpl), fd);
	erl_printf(fd, "\n");
    }
    sum += sizeof(TreeDbTerm)/sizeof(Eterm) + t->dbterm.size - 1;
    sum += do_dump_tree(fd, show, t->right); 
    return sum;
}

static void free_term(TreeDbTerm* p)
{

    db_free_term_data(&(p->dbterm));
    sys_free(p);
}

static void do_free_tree(TreeDbTerm *root) 
{
    if (!root)
	return;
    do_free_tree(root->left);
    do_free_tree(root->right);
    free_term(root);
}

static TreeDbTerm* get_term(TreeDbTerm* old, 
			    Eterm obj) 
{
    TreeDbTerm* p = db_get_term((old != NULL) ? &(old->dbterm) : NULL, 
				((char *) &(old->dbterm)) - ((char *) old),
				obj);
    return p;
}

/*
 * Deletion helpers
 */
static int balance_left(TreeDbTerm **this) 
{
    TreeDbTerm *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case -1:
	p->balance = 0;
	break;
    case 0:
	p->balance = 1;
	h = 0;
	break;
    case 1:
	p1 = p->right;
	b1 = p1->balance;
	if (b1 >= 0) { /* Single RR rotation */
	    p->right = p1->left;
	    p1->left = p;
	    if (b1 == 0) {
		p->balance = 1;
		p1->balance = -1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double RL rotation */
	    p2 = p1->left;
	    b2 = p2->balance;
	    p1->left = p2->right;
	    p2->right = p1;
	    p->right = p2->left;
	    p2->left = p;
	    p->balance = (b2 == 1) ? -1 : 0;
	    p1->balance = (b2 == -1) ? 1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
	break;
    }
    return h;
}

static int balance_right(TreeDbTerm **this) 
{
    TreeDbTerm *p, *p1, *p2;
    int b1, b2, h = 1;
    
    p = *this;
    switch (p->balance) {
    case 1:
	p->balance = 0;
	break;
    case 0:
	p->balance = -1;
	h = 0;
	break;
    case -1:
	p1 = p->left;
	b1 = p1->balance;
	if (b1 <= 0) { /* Single LL rotation */
	    p->left = p1->right;
	    p1->right = p;
	    if (b1 == 0) {
		p->balance = -1;
		p1->balance = 1;
		h = 0;
	    } else {
		p->balance = p1->balance = 0;
	    }
	    (*this) = p1;
	} else { /* Double LR rotation */
	    p2 = p1->right;
	    b2 = p2->balance;
	    p1->right = p2->left;
	    p2->left = p1;
	    p->left = p2->right;
	    p2->right = p;
	    p->balance = (b2 == -1) ? 1 : 0;
	    p1->balance = (b2 == 1) ? -1 : 0;
	    p2->balance = 0;
	    (*this) = p2;
	}
    }
    return h;
}

static int delsub(TreeDbTerm **this) 
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    TreeDbTerm *q = (*this);
    TreeDbTerm **r = &(q->left);
    int h;

    /*
     * Walk down the tree to the right and search 
     * for a void right child, pick that child out
     * and return it to be put in the deleted 
     * object's place.
     */
    
    while ((*r)->right != NULL) {
	tstack[tpos++] = r;
	r = &((*r)->right);
    }
    *this = *r;
    *r = (*r)->left;
    (*this)->left = q->left;
    (*this)->right = q->right;
    (*this)->balance = q->balance;
    tstack[0] = &((*this)->left);
    h = 1;
    while (tpos && h) {
	r = tstack[--tpos];
	h = balance_right(r);
    }
    return h;
}

/*
 * Helper for db_slot
 */

static TreeDbTerm *slot_search(Process *p, DbTableTree *tb, sint32 slot)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;

    if (slot == 1) { /* Don't search from where we are if we are 
			looking for the first slot */
	tb->slot_pos = 0;
    }

    if (tb->slot_pos == 0) { /* clear stack if slot positions 
				are not recorded */
	tb->stack_pos = 0;
    }
    if (EMPTY_NODE(tb)) {
	this = tb->root;
	if (this == NULL)
	    return NULL;
	while (this->left != NULL){
	    PUSH_NODE(tb, this);
	    this = this->left;
	}
	PUSH_NODE(tb, this);
	tb->slot_pos = 1;
    }
    this = TOP_NODE(tb);
    while (tb->slot_pos != slot && this != NULL) {
	if (slot > tb->slot_pos) {
	    if (this->right != NULL) {
		this = this->right;
		while (this->left != NULL) {
		    PUSH_NODE(tb, this);
		    this = this->left;
		}
		PUSH_NODE(tb, this);
	    } else {
		for (;;) {
		    tmp = POP_NODE(tb);
		    this = TOP_NODE(tb);
		    if (this == NULL || this->left == tmp)
			break;
		}
	    }		
	    ++(tb->slot_pos);
	} else {
	    if (this->left != NULL) {
		this = this->left;
		while (this->right != NULL) {
		    PUSH_NODE(tb, this);
		    this = this->right;
		}
		PUSH_NODE(tb, this);
	    } else {
		for (;;) {
		    tmp = POP_NODE(tb);
		    this = TOP_NODE(tb);
		    if (this == NULL || this->right == tmp)
			break;
		}
	    }		
	    --(tb->slot_pos);
	}
    }
    return this;
}

/*
 * Find next and previous in sort order
 */

static TreeDbTerm *find_next(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    if(( this = TOP_NODE(tb)) != NULL) {
	if (!CMP_EQ(GETKEY(tb, this->dbterm.tpl),key)) {
	    /* Start from the beginning */
	    tb->stack_pos = tb->slot_pos = 0;
	}
    }
    if (EMPTY_NODE(tb)) { /* Have to rebuild the stack */
	if (( this = tb->root ) == NULL)
	    return NULL;
	for (;;) {
	    PUSH_NODE(tb, this);
	    if (( c = cmp(GETKEY(tb, this->dbterm.tpl),key) ) < 0) {
		if (this->right == NULL) /* We are at the previos 
					    and the element does
					    not exist */
		    break;
		else
		    this = this->right;
	    } else if (c > 0) {
		if (this->left == NULL) /* Done */
		    return this;
		else
		    this = this->left;
	    } else
		break;
	}
    }
    /* The next element from this... */
    if (this->right != NULL) {
	this = this->right;
	PUSH_NODE(tb,this);
	while (this->left != NULL) {
	    this = this->left;
	    PUSH_NODE(tb, this);
	}
	if (tb->slot_pos > 0) 
	    ++(tb->slot_pos);
    } else {
	do {
	    tmp = POP_NODE(tb);
	    if (( this = TOP_NODE(tb)) == NULL) {
		tb->slot_pos = 0;
		return NULL;
	    }
	} while (this->right == tmp);
	if (tb->slot_pos > 0) 
	    ++(tb->slot_pos);
    }
    return this;
}

static TreeDbTerm *find_prev(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    if(( this = TOP_NODE(tb)) != NULL) {
	if (!CMP_EQ(GETKEY(tb, this->dbterm.tpl),key)) {
	    /* Start from the beginning */
	    tb->stack_pos = tb->slot_pos = 0;
	}
    }
    if (EMPTY_NODE(tb)) { /* Have to rebuild the stack */
	if (( this = tb->root ) == NULL)
	    return NULL;
	for (;;) {
	    PUSH_NODE(tb, this);
	    if (( c = cmp(GETKEY(tb, this->dbterm.tpl),key) ) > 0) {
		if (this->left == NULL) /* We are at the next 
					   and the element does
					   not exist */
		    break;
		else
		    this = this->left;
	    } else if (c < 0) {
		if (this->right == NULL) /* Done */
		    return this;
		else
		    this = this->right;
	    } else
		break;
	}
    }
    /* The previous element from this... */
    if (this->left != NULL) {
	this = this->left;
	PUSH_NODE(tb,this);
	while (this->right != NULL) {
	    this = this->right;
	    PUSH_NODE(tb, this);
	}
	if (tb->slot_pos > 0) 
	    --(tb->slot_pos);
    } else {
	do {
	    tmp = POP_NODE(tb);
	    if (( this = TOP_NODE(tb)) == NULL) {
		tb->slot_pos = 0;
		return NULL;
	    }
	} while (this->left == tmp);
	if (tb->slot_pos > 0) 
	    --(tb->slot_pos);
    }
    return this;
}

static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    /* spool the stack, we have to "re-search" */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(tb, this);
	if (( c = cmp_partly_bound(key,GETKEY(tb, this->dbterm.tpl)) ) >= 0) {
	    if (this->right == NULL) {
		do {
		    tmp = POP_NODE(tb);
		    if (( this = TOP_NODE(tb)) == NULL) {
			return NULL;
		    }
		} while (this->right == tmp);
		return this;
	    } else
		this = this->right;
	} else /*if (c < 0)*/ {
	    if (this->left == NULL) /* Done */
		return this;
	    else
		this = this->left;
	} 
    }
}

static TreeDbTerm *find_prev_from_pb_key(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    /* spool the stack, we have to "re-search" */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL)
	return NULL;
    for (;;) {
	PUSH_NODE(tb, this);
	if (( c = cmp_partly_bound(key,GETKEY(tb, this->dbterm.tpl)) ) <= 0) {
	    if (this->left == NULL) {
		do {
		    tmp = POP_NODE(tb);
		    if (( this = TOP_NODE(tb)) == NULL) {
			return NULL;
		    }
		} while (this->left == tmp);
		return this;
	    } else
		this = this->left;
	} else /*if (c < 0)*/ {
	    if (this->right == NULL) /* Done */
		return this;
	    else
		this = this->right;
	} 
    }
}


/*
 * Just lookup a node
 */
static TreeDbTerm *find_node(DbTableTree *tb, Eterm key)
{
    TreeDbTerm *this;
    int res;

    if(!EMPTY_NODE(tb) && 
       CMP_EQ(GETKEY(tb, ( this = TOP_NODE(tb) )->dbterm.tpl), key))
	return this;
    this = tb->root;
    while (this != NULL && 
	   ( res = cmp(key, GETKEY(tb, this->dbterm.tpl)) ) != 0) {
	if (res < 0)
	    this = this->left;
	else
	    this = this->right;
    }
    return this;
}

/*
 * Lookup a node and return the address of the node pointer in the tree
 */
static TreeDbTerm **find_node2(DbTableTree *tb, Eterm key)
{
    TreeDbTerm **this;
    int res;

    this = &tb->root;
    while ((*this) != NULL && 
	   ( res = cmp(key, GETKEY(tb, (*this)->dbterm.tpl)) ) != 0) {
	if (res < 0)
	    this = &((*this)->left);
	else
	    this = &((*this)->right);
    }
    if (*this == NULL)
	return NULL;
    return this;
}

/*
 * Callback function for db_do_update_counter
 */
static int realloc_counter(TreeDbTerm** bp, Uint sz, 
			   Eterm new_counter, int counterpos)
{
    TreeDbTerm* b = *bp;
    return db_realloc_counter((void **) bp, &(b->dbterm),
			      ((char *) &(b->dbterm)) - ((char *) b),
			      sz, new_counter, counterpos);
}

/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_backwards(DbTableTree *tb,
			       Eterm lastkey,
			       int (*doit)(TreeDbTerm *, void *, int),
			       void *context) 
{
    TreeDbTerm *this, *next;

    if (lastkey == NIL) {
	tb->stack_pos = tb->slot_pos = 0;
	if (( this = tb->root ) == NULL) {
	    return;
	}
	while (this != NULL) {
	    PUSH_NODE(tb, this);
	    this = this->right;
	}
	this = TOP_NODE(tb);
	next = find_prev(tb, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(this, context, 0)))
	    return;
    } else {
	next = find_prev(tb, lastkey);
    }

    while ((this = next) != NULL) {
	next = find_prev(tb, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(this, context, 0)))
	    return;
    }
}

/*
 * Traverse the tree with a callback function, used by db_match_xxx
 */
static void traverse_forward(DbTableTree *tb,
			     Eterm lastkey,
			     int (*doit)(TreeDbTerm *, void *, int),
			     void *context) 
{
    TreeDbTerm *this, *next;

    if (lastkey == NIL) {
	tb->stack_pos = tb->slot_pos = 0;
	if (( this = tb->root ) == NULL) {
	    return;
	}
	while (this != NULL) {
	    PUSH_NODE(tb, this);
	    this = this->left;
	}
	this = TOP_NODE(tb);
	next = find_next(tb, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(this, context, 1)))
	    return;
    } else {
	next = find_next(tb, lastkey);
    }

    while ((this = next) != NULL) {
	next = find_next(tb, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(this, context, 1)))
	    return;
    }
}

/*
 * Returns 0 if not given 1 if given and -1 on no possible match
 * if key is given and ret != NULL, *ret is set to point 
 * to the object concerned;
 */
static int key_given(DbTableTree *tb, Eterm pattern, TreeDbTerm **ret, 
		     Eterm *partly_bound)
{
    TreeDbTerm *this;
    Eterm key;

    if (pattern == am_Underscore || db_is_variable(pattern) != -1)
	return 0;
    key = db_getkey(tb->keypos, pattern);
    if (is_non_value(key))
	return -1;  /* can't possibly match anything */
    if (!db_has_variable(key)) {   /* Bound key */
	if(( this = find_node(tb, key) ) == NULL)
	    return -1;
	if (ret != NULL) 
	    *ret = this; 
	return 1;
    } else if (partly_bound != NULL && key != am_Underscore && 
	       db_is_variable(key) < 0)
	*partly_bound = key;
	
    return 0;
}



static int do_cmp_partly_bound(Eterm a, Eterm b, int *done)
{
    Eterm* aa;
    Eterm* bb;
    Eterm a_hdr;
    Eterm b_hdr;
    int i;
    int j;

    /* A variable matches anything */
    if (is_atom(a) && (a == am_Underscore || (db_is_variable(a) >= 0))) {
	*done = 1;
	return 0;
    }
    if (a == b)
	return 0;
    
    switch (a & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	if (!is_list(b)) {
	    return cmp(a,b);
	}
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_cmp_partly_bound(*aa++, *bb++, done)) != 0 || *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_cmp_partly_bound(*aa, *bb, done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    case TAG_PRIMARY_BOXED:
	if ((b & _TAG_PRIMARY_MASK) != TAG_PRIMARY_BOXED) {
	    return cmp(a,b);
	}
	a_hdr = ((*boxed_val(a)) & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE;
	b_hdr = ((*boxed_val(a)) & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE;
	if (a_hdr != b_hdr) {
	    return cmp(a, b);
	}
	if (a_hdr == (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE)) {
	    aa = tuple_val(a);
	    bb = tuple_val(b);
	    /* compare the arities */
	    i = arityval(*aa);	/* get the arity*/
	    if (i < arityval(*bb)) return(-1);
	    if (i > arityval(*bb)) return(1);
	    while (i--) {
		if ((j = do_cmp_partly_bound(*++aa, *++bb, done)) != 0 
		    || *done) 
		    return j;
	    }
	    return 0;
	}
	/* Drop through */
      default:
	  return cmp(a, b);
    }
}

static int cmp_partly_bound(Eterm partly_bound_key, Eterm bound_key) 
{
    int done = 0;
    int ret = do_cmp_partly_bound(partly_bound_key, bound_key, &done);
#ifdef HARDDEBUG
    erl_printf(CERR,"\r\ncmp_partly_bound: ");
    display(partly_bound_key,CERR);
    if (ret < 0)
	erl_printf(CERR," < ");
    else if (ret > 0)
	erl_printf(CERR," > ");
    else
	erl_printf(CERR," == ");
    display(bound_key,CERR);
    erl_printf(CERR,"\r\n");
#endif
    return ret;
}

/*
** For partly_bound debugging....
**
BIF_RETTYPE ets_testnisse_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm r1 = make_small(partly_bound_can_match_lesser(BIF_ARG_1,
							BIF_ARG_2));
    Eterm r2 = make_small(partly_bound_can_match_greater(BIF_ARG_1,
							 BIF_ARG_2));
    Eterm *hp = HAlloc(BIF_P,3);
    Eterm ret;

    ret = TUPLE2(hp,r1,r2);
    BIF_RET(ret);
}
**
*/
static int partly_bound_can_match_lesser(Eterm partly_bound_1, 
					 Eterm partly_bound_2) 
{
    int done = 0;
    int ret = do_partly_bound_can_match_lesser(partly_bound_1, 
					       partly_bound_2, 
					       &done);
#ifdef HARDDEBUG
    erl_printf(CERR,"\r\npartly_bound_can_match_lesser: ");
    display(partly_bound_1,CERR);
    if (ret)
	erl_printf(CERR," can match lesser than ");
    else
	erl_printf(CERR," can not match lesser than ");
    display(partly_bound_2,CERR);
    erl_printf(CERR,"\r\n");
#endif
    return ret;
}

static int partly_bound_can_match_greater(Eterm partly_bound_1, 
					  Eterm partly_bound_2) 
{
    int done = 0;
    int ret = do_partly_bound_can_match_greater(partly_bound_1, 
						partly_bound_2, 
						&done);
#ifdef HARDDEBUG
    erl_printf(CERR,"\r\npartly_bound_can_match_greater: ");
    display(partly_bound_1,CERR);
    if (ret)
	erl_printf(CERR," can match greater than ");
    else
	erl_printf(CERR," can not match greater than ");
    display(partly_bound_2,CERR);
    erl_printf(CERR,"\r\n");
#endif
    return ret;
}

static int do_partly_bound_can_match_lesser(Eterm a, Eterm b, 
					    int *done)
{
    Eterm* aa;
    Eterm* bb;
    int i;
    int j;

    if (is_atom(a) && (a == am_Underscore || 
		       (db_is_variable(a) >= 0))) {
	*done = 1;
	if (is_atom(b) && (b == am_Underscore || 
			   (db_is_variable(b) >= 0))) {
	    return 0;
	} else {
	    return 1;
	}
    } else if (is_atom(b) && (b == am_Underscore || 
			      (db_is_variable(b) >= 0))) {
	*done = 1;
	return 0;
    }

    if (a == b)
	return 0;

    if (not_eq_tags(a,b)) {
	*done = 1;
	return (cmp(a, b) < 0) ? 1 : 0;
    }

    /* we now know that tags are the same */
    switch (tag_val_def(a)) {
    case TUPLE_DEF:
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return 1;
	if (arityval(*aa) > arityval(*bb)) return 0;
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = do_partly_bound_can_match_lesser(*++aa, *++bb, 
						      done)) != 0 
		|| *done) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_partly_bound_can_match_lesser(*aa++, *bb++, 
						      done)) != 0 
		|| *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_partly_bound_can_match_lesser(*aa, *bb, 
							done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    default:
	if((i = cmp(a, b)) != 0) {
	    *done = 1;
	}
	return (i < 0) ? 1 : 0;
    }
}

static int do_partly_bound_can_match_greater(Eterm a, Eterm b, 
					    int *done)
{
    Eterm* aa;
    Eterm* bb;
    int i;
    int j;

    if (is_atom(a) && (a == am_Underscore || 
		       (db_is_variable(a) >= 0))) {
	*done = 1;
	if (is_atom(b) && (b == am_Underscore || 
			   (db_is_variable(b) >= 0))) {
	    return 0;
	} else {
	    return 1;
	}
    } else if (is_atom(b) && (b == am_Underscore || 
			      (db_is_variable(b) >= 0))) {
	*done = 1;
	return 0;
    }

    if (a == b)
	return 0;

    if (not_eq_tags(a,b)) {
	*done = 1;
	return (cmp(a, b) > 0) ? 1 : 0;
    }

    /* we now know that tags are the same */
    switch (tag_val_def(a)) {
    case TUPLE_DEF:
	aa = tuple_val(a);
	bb = tuple_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return 0;
	if (arityval(*aa) > arityval(*bb)) return 1;
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = do_partly_bound_can_match_greater(*++aa, *++bb, 
						      done)) != 0 
		|| *done) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = list_val(a);
	bb = list_val(b);
	while (1) {
	    if ((j = do_partly_bound_can_match_greater(*aa++, *bb++, 
						      done)) != 0 
		|| *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_partly_bound_can_match_greater(*aa, *bb, 
							done);
	    aa = list_val(*aa);
	    bb = list_val(*bb);
	}
    default:
	if((i = cmp(a, b)) != 0) {
	    *done = 1;
	}
	return (i > 0) ? 1 : 0;
    }
}

/*
 * Callback functions for the different match functions
 */

static int doit_select(TreeDbTerm *this, void *ptr, int forward)
{
    struct select_context *sc = (struct select_context *) ptr;
    Eterm ret;
    Uint32 dummy;

    sc->lastobj = this->dbterm.tpl;
    
    if (sc->end_condition != NIL && 
	((forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, 
					   this->dbterm.tpl)) < 0) ||
	 (!forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, 
					   this->dbterm.tpl)) > 0))) {
	return 0;
    }
    ret = db_prog_match(sc->p, sc->mp,
			make_tuple(this->dbterm.tpl), 
			0, &dummy);
    if (is_value(ret)) {
	Uint sz;
	Eterm *hp;
	if (sc->all_objects) {
	    hp = HAlloc(sc->p, this->dbterm.size + 2);
	    ret = copy_shallow(this->dbterm.v,
				     this->dbterm.size,
				     &hp,
				     &(sc->p->off_heap));
	} else {
	    sz = size_object(ret);
	    hp = HAlloc(sc->p, sz + 2);
	    ret = copy_struct(ret, sz, 
			      &hp, &(sc->p->off_heap));
	}
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

static int doit_select_chunk(TreeDbTerm *this, void *ptr, int forward)
{
    struct select_context *sc = (struct select_context *) ptr;
    Eterm ret;
    Uint32 dummy;

    sc->lastobj = this->dbterm.tpl;
    
    if (sc->end_condition != NIL && 
	((forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, 
					   this->dbterm.tpl)) < 0) ||
	 (!forward && 
	  cmp_partly_bound(sc->end_condition, 
			   GETKEY_WITH_POS(sc->keypos, 
					   this->dbterm.tpl)) > 0))) {
	return 0;
    }

    ret = db_prog_match(sc->p, sc->mp,
			make_tuple(this->dbterm.tpl), 
			0, &dummy);
    if (is_value(ret)) {
	Uint sz;
	Eterm *hp;

	++(sc->got);
	if (sc->all_objects) {
	    hp = HAlloc(sc->p, this->dbterm.size + 2);
	    ret = copy_shallow(this->dbterm.v,
				     this->dbterm.size,
				     &hp,
				     &(sc->p->off_heap));
	} else {
	    sz = size_object(ret);
	    hp = HAlloc(sc->p, sz + 2);
	    ret = copy_struct(ret, sz, 
			      &hp, &(sc->p->off_heap));
	}
	sc->accum = CONS(hp, ret, sc->accum);
    }
    if (--(sc->max) <= 0 || sc->got == sc->chunk_size) {
	return 0;
    }
    return 1;
}


static int doit_select_delete(TreeDbTerm *this, void *ptr, int forward)
{
    struct select_delete_context *sc = (struct select_delete_context *) ptr;
    Eterm ret;
    Uint32 dummy,key;

    if (sc->erase_lastterm)
	free_term(sc->lastterm);
    sc->erase_lastterm = 0;
    sc->lastterm = this;
    
    if (sc->end_condition != NIL && 
	cmp_partly_bound(sc->end_condition, 
			 GETKEY_WITH_POS(sc->keypos, 
					 this->dbterm.tpl)) > 0)
	return 0;
    ret = db_prog_match(sc->p, sc->mp,
			make_tuple(this->dbterm.tpl), 
			0, &dummy);
    if (ret == am_true) {
	key = GETKEY(sc->tb, this->dbterm.tpl);
	linkout_tree(sc->tb, key);
	sc->erase_lastterm = 1;
	++sc->accum;
    }
    if (--(sc->max) <= 0) {
	return 0;
    }
    return 1;
}

#ifdef TREE_DEBUG
static int do_dump_tree2(CIO fd, int show, TreeDbTerm *t, int offset)
{
    int sum = 0;
    if (t == NULL)
	return 0;
    sum += do_dump_tree2(fd, show, t->right, offset + 4);
    if (show) {
	int i;
	for (i = 0; i < offset; ++i)
	    erl_printf(fd, " ");
	display(make_tuple(t->dbterm.tpl), fd);
	erl_printf(fd, "(addr = 0x%08X, bal = %d)\r\n", t, t->balance);
    }
    sum += sizeof(TreeDbTerm)/sizeof(Eterm) + t->dbterm.size - 1;
    sum += do_dump_tree2(fd, show, t->left, offset + 4); 
    return sum;
}

#endif

#ifdef HARDDEBUG
void db_check_table_tree(DbTableTree *tb)
{
    check_table_tree(tb->root);
    check_saved_stack(tb);
    check_slot_pos(tb);
}

static TreeDbTerm *traverse_until(TreeDbTerm *t, int *current, int to)
{
    TreeDbTerm *tmp;
    if (t == NULL) 
	return NULL;
    tmp = traverse_until(t->left, current, to);
    if (tmp != NULL)
	return tmp;
    ++(*current);
    if (*current == to)
	return t;
    return traverse_until(t->right, current, to);
}

static void check_slot_pos(DbTableTree *tb)
{
    int pos = 0;
    TreeDbTerm *t;
    if (tb->slot_pos == 0 || tb->stack_pos == 0)
	return;
    t = traverse_until(tb->root, &pos, tb->slot_pos);
    if (t != tb->stack[tb->stack_pos - 1]) {
	erl_printf(CERR, "Slot position does not correspont with stack, "
		   "element position %d is really 0x%08X, when stack says "
		   "it's 0x%08X\r\n", tb->slot_pos, t, 
		   tb->stack[tb->stack_pos - 1]);
	do_dump_tree2(CERR, 1, tb->root, 0);
    }
}
	

static void check_saved_stack(DbTableTree *tb)
{
     TreeDbTerm *t = tb->root;
     int n = 0;
     if (tb->stack_pos == 0)
	 return;
     if (t != tb->stack[n]) {
	 erl_printf(CERR,"tb->stack[0] is 0x%08X, should be 0x%08X\r\n",
		    tb->stack[0], t);
	 do_dump_tree2(CERR, 1, tb->root, 0);
	 return;
     }
     while (n < tb->stack_pos) {
	 if (t == NULL) {
	     erl_printf(CERR, "NULL pointer in tree when stack not empty,"
			" stack depth is %d\r\n", n);
	     do_dump_tree2(CERR, 1, tb->root, 0);
	     return;
	 }
	 n++;
	 if (n < tb->stack_pos) {
	     if (tb->stack[n] == t->left)
		 t = t->left;
	     else if (tb->stack[n] == t->right)
		 t = t->right;
	     else {
		 erl_printf(CERR, "tb->stack[%d] == 0x%08X does not "
			    "represent child pointer in tree!"
			    "(left == 0x%08X, right == 0x%08X\r\n", 
			    n, tb->stack[n], t->left, t->right);
		 do_dump_tree2(CERR, 1, tb->root, 0);
		 return;
	     }
	 }
     }
}

static int check_table_tree(TreeDbTerm *t)
{
    int lh, rh;
    if (t == NULL)
	return 0;
    lh = check_table_tree(t->left);
    rh = check_table_tree(t->right);
    if ((rh - lh) != t->balance) {
	erl_printf(CERR, "Invalid tree balance for this node:\r\n");
	erl_printf(CERR,"balance = %d, left = 0x%08X, right = 0x%08X\r\n"
		   "data =",
		   t->balance, t->left, t->right);
	display(make_tuple(t->dbterm.tpl),CERR);
	erl_printf(CERR,"\r\nDump:\r\n---------------------------------\r\n");
	do_dump_tree2(CERR, 1, t, 0);
	erl_printf(CERR,"\r\n---------------------------------\r\n");
    }
    return ((rh > lh) ? rh : lh) + 1;
}
	
#endif
