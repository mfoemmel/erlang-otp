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

struct select_context {
    Process *p;
    Eterm accum;
    Binary *mp;
    Eterm end_condition;
    Eterm *lastobj;
    sint32 max;
    int keypos;
    int all_objects;
};

struct match_context {
    Process *p;
    eTerm accum;
    eTerm pattern;
    DbBindings bs, sz_bs;
    eTerm partly_bound;
    int keypos;
};

struct match_object_context {
    Process *p;
    eTerm accum;
    eTerm pattern;
    eTerm *lastobj; /* save the last object for state */
    sint32 max;
    eTerm partly_bound;
    int keypos;
    DbBindings bs;
};

struct match_erase_context {
    Process *p;
    DbTableTree *tb;
    eTerm pattern;
    DbBindings bs;
    eTerm partly_bound;
    int keypos;
};


/*
** Forward declarations 
*/
static void do_free_tree(TreeDbTerm *root);
static TreeDbTerm* get_term(TreeDbTerm* old, 
			    eTerm obj);
static void free_term(TreeDbTerm* p);
static int balance_left(TreeDbTerm **this); 
static int balance_right(TreeDbTerm **this); 
static int delsub(TreeDbTerm **this); 
static TreeDbTerm *slot_search(Process *p, DbTableTree *tb, sint32 slot);
static int realloc_counter(TreeDbTerm** bp, uint32 sz, 
			   eTerm new_counter, int counterpos);
static TreeDbTerm *find_node(DbTableTree *tb, eTerm key);
static TreeDbTerm **find_node2(DbTableTree *tb, eTerm key);
static TreeDbTerm *find_next(DbTableTree *tb, eTerm key);
static TreeDbTerm *find_prev(DbTableTree *tb, eTerm key);
static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, eTerm key);
static void traverse_backwards(DbTableTree *tb,
			       eTerm lastkey,
			       int (*doit)(TreeDbTerm *, void *),
			       void *context); 
static int key_given(DbTableTree *tb, eTerm pattern, TreeDbTerm **ret,
		     eTerm *partly_bound_key);
static int cmp_partly_bound(eTerm partly_bound_key, eTerm bound_key);
static int do_cmp_partly_bound(eTerm a, eTerm b, int *done);

static int doit_match(TreeDbTerm *this, void *ptr);
static int doit_match_object(TreeDbTerm *this, void *ptr);
static int doit_match_erase(TreeDbTerm *this, void *ptr);
static int doit_select(TreeDbTerm *this, void *ptr);
static int do_dump_tree(CIO fd, int show, TreeDbTerm *t);

static int partly_bound_can_match_lesser(eTerm partly_bound_1, 
					 eTerm partly_bound_2);
static int partly_bound_can_match_greater(eTerm partly_bound_1, 
					  eTerm partly_bound_2); 
static int do_partly_bound_can_match_lesser(eTerm a, eTerm b, 
					    int *done);
static int do_partly_bound_can_match_greater(eTerm a, eTerm b, 
					     int *done);
static BIF_RETTYPE tree_select_continue(Process *p, Eterm accum, 
					Eterm tabinfo, 
					Eterm progbin);

/*
** Static variables
*/

Export tree_select_continue_exp;

/*
** External interface 
*/
void db_initialize_tree(void)
{
    extern Eterm* em_apply_bif;
    memset(&tree_select_continue_exp, 0, sizeof(Export));
    tree_select_continue_exp.address = 
	&tree_select_continue_exp.code[3];
    tree_select_continue_exp.code[0] = am_ets;
    tree_select_continue_exp.code[1] = am_select;
    tree_select_continue_exp.code[2] = 3;
    tree_select_continue_exp.code[3] =
	(Eterm) em_apply_bif;
    tree_select_continue_exp.code[4] = 
	(Eterm) &tree_select_continue;
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

int db_first_tree(Process *p, DbTableTree *tb, eTerm *ret)
{
    TreeDbTerm *this;
    eTerm e;
    eTerm *hp;
    uint32 sz;

    /* Walk down to the tree to the left */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL) {
	*ret = db_am_eot;
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
		 eTerm key, eTerm *ret)
{
    TreeDbTerm *this;
    eTerm e;
    eTerm *hp;
    uint32 sz;

    if (is_atom(key) && key == db_am_eot)
	return DB_ERROR_BADKEY;
    if (( this = find_next(tb, key) ) == NULL) {
	*ret = db_am_eot;
	return DB_ERROR_NONE;
    }
    
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_last_tree(Process *p, DbTableTree *tb, eTerm *ret)
{
    TreeDbTerm *this;
    eTerm e;
    eTerm *hp;
    uint32 sz;

    /* Walk down to the tree to the left */
    tb->stack_pos = tb->slot_pos = 0;
    if (( this = tb->root ) == NULL) {
	*ret = db_am_eot;
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
		 eTerm key, eTerm *ret)
{
    TreeDbTerm *this;
    eTerm e;
    eTerm *hp;
    uint32 sz;

    if (is_atom(key) && key == db_am_eot)
	return DB_ERROR_BADKEY;
    if (( this = find_prev(tb, key) ) == NULL) {
	*ret = db_am_eot;
	return DB_ERROR_NONE;
    }
    
    e = GETKEY(tb, this->dbterm.tpl);
    sz = size_object(e);

    hp = HAlloc(p, sz);

    *ret = copy_struct(e,sz,&hp,&p->off_heap);
    
    return DB_ERROR_NONE;
}

int db_update_counter_tree(Process *p, DbTableTree *tb, 
			   eTerm key, eTerm incr, int counterpos, eTerm *ret)
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
			       (int (*)(void *, uint32, eTerm, int))
			       &realloc_counter, incr, ret);
    if (*bp != b) /* May be reallocated in which case 
		     the saved stack is messed up, clear stck if so. */
	tb->stack_pos = tb->slot_pos = 0;
    return res;
}

int db_put_tree(Process *proc, DbTableTree *tb, 
		eTerm obj, eTerm *ret)
{
    /* Non recursive insertion in AVL tree, building our own stack */
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    int c;
    eTerm key;
    int dir;
    TreeDbTerm *p1, *p2, *p;

    key = GETKEY(tb, ptr_val(obj));

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
		eTerm key, eTerm *ret)
{
    eTerm copy;
    eTerm *hp;
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

int db_get_element_tree(Process *p, DbTableTree *tb, 
			eTerm key, int ndex, eTerm *ret)
{
    /*
     * Look the node up:
     */
    eTerm *hp;
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
	eTerm element;
	uint32 sz;
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
		  eTerm key, eTerm *ret)
{
    TreeDbTerm **tstack[STACK_NEED];
    int tpos = 0;
    int dstack[STACK_NEED+1];
    int dpos = 0;
    int state = 0;
    TreeDbTerm **this = &tb->root;
    int c;
    int dir;
    TreeDbTerm *q;

    /*
     * Somewhat complicated, deletion in an AVL tree,
     * The two helpers balance_left and balance_right are used to
     * keep the balance. As in insert, we do the stacking ourselves.
     */

    *ret = am_true;

    tb->stack_pos = tb->slot_pos = 0;
    dstack[dpos++] = DIR_END;
    for (;;)
	if (!*this) { /* Failure */
	    return DB_ERROR_NONE;
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
	    free_term(q);
	    --(tb->nitems);
	    break;
	}
    while (state && ( dir = dstack[--dpos] ) != DIR_END) {
	this = tstack[--tpos];
	if (dir == DIR_LEFT) {
	    state = balance_left(this);
	} else {
	    state = balance_right(this);
	}
    }
    return DB_ERROR_NONE;
}

int db_slot_tree(Process *p, DbTableTree *tb, 
		 eTerm slot_term, eTerm *ret)
{
    sint32 slot;
    TreeDbTerm *st;
    eTerm *hp;
    eTerm copy;

    /*
     * The notion of a "slot" is not natural in a tree, but we try to
     * simulate it by giving the n'th node in the tree instead.
     * Traversing a tree in this way is not very convenient, but by
     * using the saved stack we at least sometimes will get acceptable 
     * performance.
     */

    slot = signed_val(slot_term);
    
    if (is_not_small(slot_term) || (slot < 0) || (slot > tb->nitems))
	return DB_ERROR_BADPARAM;

    if (slot == tb->nitems) {
	*ret = db_am_eot;
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



int db_match_tree(Process *p, DbTableTree *tb, 
		  eTerm pattern, eTerm *ret)
{
    struct match_context mc;
    eTerm bnd[DB_MATCH_NBIND];
    uint32 size_bnd[DB_MATCH_NBIND];
    TreeDbTerm *this;
    int res;
    eTerm last = NIL;

    mc.bs.size = mc.sz_bs.size = DB_MATCH_NBIND;
    mc.bs.ptr = bnd;
    mc.sz_bs.ptr = size_bnd;
    mc.accum = NIL;
    mc.p = p;
    mc.pattern = pattern;
    mc.keypos = tb->keypos;
    mc.partly_bound = NIL;
    
    res = key_given(tb, pattern, &this, &(mc.partly_bound));
    if (res < 0) 
	goto done;
    if (res > 0) { /* Key given, we only need one call to doit_match */ 
	doit_match(this,&mc);
	goto done;
    } 
	
    /* Complete match */
    if (mc.partly_bound != NIL && 
	(this = find_next_from_pb_key(tb, mc.partly_bound)) != NULL) {
	    last = GETKEY(tb, this->dbterm.tpl);
    }
    traverse_backwards(tb, last, &doit_match, &mc);
done:
    if (mc.bs.size != DB_MATCH_NBIND) {
	sys_free(mc.bs.ptr);
	sys_free(mc.sz_bs.ptr);
    }
    *ret = mc.accum;
    return DB_ERROR_NONE;
}
/*
** This is a BIF, i.e. It is called directly from the 
** emulator loop, but cannot be called without a trap,
** so even though it's all erlang terms, the parameters
** need not be checked.
*/
static BIF_RETTYPE tree_select_continue(Process *p, Eterm accum, 
					Eterm tabinfo, 
					Eterm progbin)
{
    Eterm tabname = ptr_val(tabinfo)[2];
    struct select_context sc;
    unsigned sz;
    Eterm *hp; 
    Eterm lastkey = ptr_val(tabinfo)[4];
    Eterm end_condition = ptr_val(tabinfo)[1];
    Binary *mp = ((ProcBin *) ptr_val(progbin))->val;
    Eterm key;
    DbTable *ttb;
    DbTableTree *tb;

    if ((ttb = db_get_table(p, tabname, DB_READ)) == NULL) {
	BIF_RET(accum);
    }
    tb = &(ttb->tree);

    sc.p = p;
    sc.accum = accum;
    sc.mp = mp;
    sc.end_condition = NIL;
    sc.lastobj = NULL;
    sc.max = 1000;
    sc.keypos = tb->keypos;
    sc.all_objects = (int) unsigned_val(ptr_val(tabinfo)[3]);

    traverse_backwards(tb, lastkey, &doit_select, &sc);
    BUMP_ALL_REDS(p);
    if (sc.max > 0) {
	BIF_RET(sc.accum);
    }	
    key = GETKEY(tb, sc.lastobj);
    if (end_condition != NIL && 
	cmp_partly_bound(end_condition,key) < 0) { /* done anyway */
	BIF_RET(sc.accum);
    }
    /* Not done yet, let's trap. */
    sz = size_object(key);
    hp = HAlloc(p, 5 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));
    tabinfo = TUPLE4(hp, end_condition, tb->id, 
		     make_small(sc.all_objects), key); 
    BIF_TRAP3(&tree_select_continue_exp, p, 
			  sc.accum, tabinfo, progbin);
}

static BIF_RETTYPE bif_trap_3(Export *bif,
			      Process *p, 
			      Eterm p1, Eterm p2, Eterm p3) 
{
    BIF_TRAP3(bif, p, p1, p2, p3);
}
    
int db_select_tree(Process *p, DbTableTree *tb, 
		   eTerm pattern,
		   eTerm *ret)
{
    struct select_context sc;
    Eterm lastkey = NIL;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[30];
    Eterm *buff = sbuff;
    Binary *mp = NULL;
    Eterm tabinfo;
    unsigned sz;
    Eterm *hp; 
    ProcBin *pb;
    Eterm *ptpl;
    Eterm lst, tpl, ttpl;
    int num_heads = 0;
    int i;
    int all_objects = 1;
    Eterm key;
    Eterm partly_bound;
    Eterm least = 0;
    Eterm most = 0;
    int some_limitation = 1;
    int got_partial = 0;
    int res;
    TreeDbTerm *this, *save_term = NULL;
    int something_can_match = 0;

#define RET_TO_BIF(Term,RetVal) do { 			\
	if (mp != NULL) {			\
	    erts_match_set_free(mp);		\
	}					\
	if (buff != sbuff) {                    \
	    sys_free(buff);                     \
	}                                       \
	*ret = (Term); 				\
	return RetVal; 			\
    } while(0)

    sc.accum = NIL;
    sc.lastobj = NULL;
    sc.p = p;
    sc.max = 1000; 
    sc.end_condition = NIL;
    sc.keypos = tb->keypos;

    for (lst = pattern; is_list(lst); lst = CDR(ptr_val(lst)))
	++num_heads;

    if (lst != NIL) {/* proper list... */
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    }

    if (num_heads > 10) {
	buff = safe_alloc(sizeof(Eterm) * num_heads * 3);
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for(lst = pattern; is_list(lst); lst = CDR(ptr_val(lst))) {
	Eterm body;
	ttpl = CAR(ptr_val(lst));
	if (!is_tuple(ttpl)) {
	    RET_TO_BIF(NIL, DB_ERROR_BADPARAM);
	}
	ptpl = ptr_val(ttpl);
	if (ptpl[0] != make_arityval(3U)) {
	    RET_TO_BIF(NIL, DB_ERROR_BADPARAM);
	}
	matches[i] = tpl = ptpl[1];
	guards[i] = ptpl[2];
	bodies[i] = body = ptpl[3];
	if (!is_list(body) || CDR(ptr_val(body)) != NIL ||
	    CAR(ptr_val(body)) != am_DollarUnderscore) {
	    all_objects = 0;
	}
	++i;

	partly_bound = NIL;
	res = key_given(tb, tpl, &this, &partly_bound);
	if ( res >= 0 ) {   /* Can match something */
	    key = 0;
	    something_can_match = 1;
	    if (res > 0) {
		save_term = this;
		key = GETKEY(tb,ptr_val(tpl)); 
	    } else if (partly_bound != NIL) {
		got_partial = 1;
		key = partly_bound;
	    } else {
		some_limitation = 0;
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
    if (!something_can_match) {
	RET_TO_BIF(NIL,DB_ERROR_NONE);  
	/* can't possibly match anything */
    }

    if ((mp = db_match_compile(matches, guards, bodies,
			       num_heads, DCOMP_BODY_RETURN, NULL)) 
	== NULL) {
	RET_TO_BIF(NIL,DB_ERROR_BADPARAM);
    }
    
    sc.mp = mp;
    sc.all_objects = all_objects;

    if (!got_partial && some_limitation && cmp(least,most) == 0) {
	doit_select(save_term,&sc);
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }

    if (some_limitation) {
	if ((this = find_next_from_pb_key(tb, most)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	}
	sc.end_condition = least;
#ifdef HARDDEBUG
	erl_printf(CERR,"Least: ");
	display(least,CERR);
	erl_printf(CERR,"\r\nMost: ");
	display(most,CERR);
	erl_printf(CERR,"\r\n");
#endif
    }

    BUMP_ALL_REDS(p);
    traverse_backwards(tb, lastkey, &doit_select, &sc);
    if (sc.max > 0) {
	RET_TO_BIF(sc.accum,DB_ERROR_NONE);
    }
    /* Trap */
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = make_thing(PROC_BIN_SIZE-1, 
				REFC_BINARY_SUBTAG);
    pb->size = 0;
    pb->next = p->off_heap.mso;
    p->off_heap.mso = pb;
    pb->val = mp;
    pb->bytes = mp->orig_bytes;

    key = GETKEY(tb, sc.lastobj);
    sz = size_object(key);

    mp = NULL; /* Don't free it */

    hp = HAlloc(p, 5 + sz);
    key = copy_struct(key, sz, &hp, &(p->off_heap));
    tabinfo = TUPLE4(hp, sc.end_condition, tb->id, 
		     make_small(all_objects), key); 
    RET_TO_BIF(bif_trap_3(&tree_select_continue_exp, p, 
			  sc.accum, tabinfo, make_binary(pb)), 
	       DB_ERROR_NONE);

#undef RET_TO_BIF

}

int db_match_object_tree(Process *p, DbTableTree *tb, 
			 eTerm pattern, eTerm state, eTerm *ret)
{
    struct match_object_context mc;
    eTerm bnd[DB_MATCH_NBIND];
    TreeDbTerm *this;
    int res;
    eTerm lastkey = NIL;
    sint32 expected;
    eTerm partly_bound = NIL;

    mc.bs.size = DB_MATCH_NBIND;
    mc.bs.ptr = bnd;
    mc.accum = NIL;
    mc.lastobj = NULL;
    mc.p = p;
    mc.pattern = pattern;
    mc.max = 1; /*to make one match possible without looking at the state */
    mc.partly_bound = NIL;
    mc.keypos = tb->keypos;

    res = key_given(tb, pattern, &this, &partly_bound);
    if (res < 0) 
	goto done;
    if (res > 0) { /* Key given, we only need one call to doit_match_object */ 
	doit_match_object(this,&mc);
	goto done;
    }
    /* More or less complete match */
    if (is_small(state)) { /* a first call in a chain of calls */
	if ((expected = mc.max = signed_val(state)) <= 0)
	    return DB_ERROR_BADPARAM;
	if (partly_bound != NIL) {
	    /* First call -> give the responsibility of checking when we are
	       done to the callback function by setting mc.partly_bond */
	    mc.partly_bound = partly_bound;
	    partly_bound = NIL; /* to avoid us checking too */
	    if ((this = find_next_from_pb_key(tb, mc.partly_bound)) != NULL) {
		lastkey = GETKEY(tb, this->dbterm.tpl);
	    }
	} 
    } else if (is_tuple(state)) { /* another call in a chain */
	eTerm *tupleptr = ptr_val(state);
	if (arityval(*tupleptr) != 3)
	    return DB_ERROR_BADPARAM;
	mc.accum = tupleptr[1];
	if (is_not_list(mc.accum) && is_not_nil(mc.accum))
	    return DB_ERROR_BADPARAM;
	/* Second is the last key */
	lastkey = tupleptr[2];
	/* Third is the counter */
	if (!is_small(tupleptr[3]))
	    return DB_ERROR_BADPARAM;
	expected = mc.max = signed_val(tupleptr[3]);
    } else
	return DB_ERROR_BADPARAM;
	
    traverse_backwards(tb, lastkey, &doit_match_object, &mc);
    if (mc.max <= 0) { /* We are not done and we must have passed at
			  least one element. */
	eTerm lok = GETKEY(tb, mc.lastobj);
	if (!(partly_bound != NIL && 
	      cmp_partly_bound(partly_bound, lok) > 0)) {
	    uint32 sz =  size_object(lok);
	    eTerm *hp = HAlloc(p, 4 + sz);
	    lok = copy_struct(lok, sz, &hp, &(p->off_heap));
	    mc.accum = TUPLE3(hp, mc.accum, lok, 
			      make_small(expected));
	}
    }
    BUMP_ALL_REDS(p);
done:
    if (mc.bs.size != DB_MATCH_NBIND) {
	sys_free(mc.bs.ptr);
    }
    *ret = mc.accum;
    return DB_ERROR_NONE;
}

int db_match_erase_tree(Process *p, DbTableTree *tb, 
			eTerm pattern, eTerm *ret)
{
    struct match_erase_context mc;
    eTerm bnd[DB_MATCH_NBIND];
    TreeDbTerm *this;
    int res;
    eTerm last = NIL;

    mc.bs.size = DB_MATCH_NBIND;
    mc.bs.ptr = bnd;
    mc.p = p;
    mc.tb = tb;
    mc.pattern = pattern;
    mc.keypos = tb->keypos;
    mc.partly_bound = NIL;
    
    res = key_given(tb, pattern, &this, &(mc.partly_bound));
    if (res < 0) 
	goto done;
    if (res > 0) { /* Key given, we only need one call to doit_match_erase */ 
	doit_match_erase(this,&mc);
	goto done;
    }
    /* Complete match */
    if (mc.partly_bound != NIL && 
	(this = find_next_from_pb_key(tb, mc.partly_bound)) != NULL) {
	    last = GETKEY(tb, this->dbterm.tpl);
    }
    traverse_backwards(tb, last, &doit_match_erase, &mc);
done:
    if (mc.bs.size != DB_MATCH_NBIND) {
	sys_free(mc.bs.ptr);
    }
    *ret = am_true;
    return DB_ERROR_NONE;
}


/*
** Other interface routines (not directly coupled to one bif)
*/

/* Get the memory consumption for a tree table */
int db_info_memory_tree(Process *p, DbTableTree *tb,
			eTerm *ret, 
			int *reds)
{
    uint32 sum;
    sum = sizeof (DbTableHash) / sizeof (eTerm);
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
    sum += sizeof(TreeDbTerm)/sizeof(eTerm) + t->dbterm.size - 1;
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
			    eTerm obj) 
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

static TreeDbTerm *find_next(DbTableTree *tb, eTerm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    if(( this = TOP_NODE(tb)) != NULL) {
	if (!EQ(GETKEY(tb, this->dbterm.tpl),key)) {
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

static TreeDbTerm *find_prev(DbTableTree *tb, eTerm key)
{
    TreeDbTerm *this;
    TreeDbTerm *tmp;
    int c;

    if(( this = TOP_NODE(tb)) != NULL) {
	if (!EQ(GETKEY(tb, this->dbterm.tpl),key)) {
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

static TreeDbTerm *find_next_from_pb_key(DbTableTree *tb, eTerm key)
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


/*
 * Just lookup a node
 */
static TreeDbTerm *find_node(DbTableTree *tb, eTerm key)
{
    TreeDbTerm *this;
    int res;

    if(!EMPTY_NODE(tb) && 
       EQ(GETKEY(tb, ( this = TOP_NODE(tb) )->dbterm.tpl), key))
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
static TreeDbTerm **find_node2(DbTableTree *tb, eTerm key)
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
static int realloc_counter(TreeDbTerm** bp, uint32 sz, 
			   eTerm new_counter, int counterpos)
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
			       eTerm lastkey,
			       int (*doit)(TreeDbTerm *, void *),
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
	if (!((*doit)(this, context)))
	    return;
    } else {
	next = find_prev(tb, lastkey);
    }

    while ((this = next) != NULL) {
	next = find_prev(tb, GETKEY(tb, this->dbterm.tpl));
	if (!((*doit)(this, context)))
	    return;
    }
}

/*
 * Returns 0 if not given 1 if given and -1 on no possible match
 * if key is given and ret != NULL, *ret is set to point 
 * to the object concerned;
 */
static int key_given(DbTableTree *tb, eTerm pattern, TreeDbTerm **ret, 
		     eTerm *partly_bound)
{
    TreeDbTerm *this;
    eTerm key;

    if (pattern == am_Underscore || db_is_variable(pattern) != -1)
	return 0;
    if ((key = db_getkey(tb->keypos, pattern)) == 0)
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

static int cmp_partly_bound(eTerm partly_bound_key, eTerm bound_key) 
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

static int do_cmp_partly_bound(eTerm a, eTerm b, int *done)
{
    eTerm* aa;
    eTerm* bb;
    int i;
    int j;

    /* A variable matches anything */
    if (is_atom(a) && (a == am_Underscore || (db_is_variable(a) >= 0))) {
	*done = 1;
	return 0;
    }

    if (a == b)
	return 0;

    if (not_eq_tags(a,b)) {
	*done = 1;
	return cmp(a, b);
    }

    /* we now know that tags are the same */
    switch (tag_val_def(a)) {
    case TUPLE_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return(-1);
	if (arityval(*aa) > arityval(*bb)) return(1);
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = do_cmp_partly_bound(*++aa, *++bb, done)) != 0 || *done) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	while (1) {
	    if ((j = do_cmp_partly_bound(*aa++, *bb++, done)) != 0 || *done) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return do_cmp_partly_bound(*aa, *bb, done);
	    aa = ptr_val(*aa);
	    bb = ptr_val(*bb);
	}
    default:
	return cmp(a, b);
    }
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
static int partly_bound_can_match_lesser(eTerm partly_bound_1, 
					 eTerm partly_bound_2) 
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

static int partly_bound_can_match_greater(eTerm partly_bound_1, 
					  eTerm partly_bound_2) 
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

static int do_partly_bound_can_match_lesser(eTerm a, eTerm b, 
					    int *done)
{
    eTerm* aa;
    eTerm* bb;
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
	aa = ptr_val(a);
	bb = ptr_val(b);
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
	aa = ptr_val(a);
	bb = ptr_val(b);
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
	    aa = ptr_val(*aa);
	    bb = ptr_val(*bb);
	}
    default:
	if((i = cmp(a, b)) != 0) {
	    *done = 1;
	}
	return (i < 0) ? 1 : 0;
    }
}

static int do_partly_bound_can_match_greater(eTerm a, eTerm b, 
					    int *done)
{
    eTerm* aa;
    eTerm* bb;
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
	aa = ptr_val(a);
	bb = ptr_val(b);
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
	aa = ptr_val(a);
	bb = ptr_val(b);
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
	    aa = ptr_val(*aa);
	    bb = ptr_val(*bb);
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
static int doit_match(TreeDbTerm *this, void *ptr)
{
    struct match_context *mc = (struct match_context *) ptr;
    int i;

    ZEROB(mc->bs);

    if (mc->partly_bound != NIL && 
	cmp_partly_bound(mc->partly_bound, 
			 GETKEY_WITH_POS(mc->keypos, 
					 this->dbterm.tpl)) > 0)
	return 0;

    if (db_do_match(make_tuple(this->dbterm.tpl), 
		    mc->pattern, &(mc->bs), &(mc->sz_bs)) != 0) {
	uint32 sz = 2;
	eTerm binding_list;
	eTerm *hp;

	for (i = 0; i < mc->bs.size; i++) {
	    if (mc->bs.ptr[i] != 0) {
		mc->sz_bs.ptr[i] = size_object(mc->bs.ptr[i]);
		sz += mc->sz_bs.ptr[i] + 2;
	    }
	}
	hp = HAlloc(mc->p, sz);
	binding_list = NIL;

	for (i = mc->bs.size - 1; i >= 0; i--) {
	    if (mc->bs.ptr[i] != 0) {
		eTerm bound = copy_struct(mc->bs.ptr[i], 
					  mc->sz_bs.ptr[i],
					  &hp, &(mc->p->off_heap));
		binding_list = CONS(hp, bound, binding_list);
		hp += 2;
	    }
	}
	mc->accum = CONS(hp, binding_list, mc->accum);
	/*hp += 2;*/
    }
    return 1;
}

static int doit_match_object(TreeDbTerm *this, void *ptr)
{
    struct match_object_context *mc = (struct match_object_context *) ptr;

    ZEROB(mc->bs);
    mc->lastobj = this->dbterm.tpl;
    
    if (mc->partly_bound != NIL && 
	cmp_partly_bound(mc->partly_bound, 
			 GETKEY_WITH_POS(mc->keypos, 
					 this->dbterm.tpl)) > 0)
	return 0;
    if (db_do_match(make_tuple(this->dbterm.tpl), 
		    mc->pattern, &(mc->bs), NULL) != 0) {
	uint32 sz = 2 + this->dbterm.size;
	eTerm tmp;
	eTerm *hp;

	hp = HAlloc(mc->p, sz);
	tmp = copy_shallow(this->dbterm.v, 
			   this->dbterm.size, 
			   &hp, 
			   &(mc->p->off_heap));
	mc->accum = CONS(hp, tmp, mc->accum);
	/*hp += 2;*/
    }
    if (--(mc->max) <= 0)
	return 0;
    return 1;
}

static int doit_select(TreeDbTerm *this, void *ptr)
{
    struct select_context *sc = (struct select_context *) ptr;
    Eterm ret;
    Uint32 dummy;

    sc->lastobj = this->dbterm.tpl;
    
    if (sc->end_condition != NIL && 
	cmp_partly_bound(sc->end_condition, 
			 GETKEY_WITH_POS(sc->keypos, 
					 this->dbterm.tpl)) > 0)
	return 0;
    if ((ret = db_prog_match(sc->p, sc->mp, 
			     make_tuple(this->dbterm.tpl), 
			     0, &dummy)) != 0) {
	uint32 sz;
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

/*
 * This one could be made more efficient, but the deletion 
 * always destroys the saved position stack, so
 * calling db_erase_tree isn't so bad.
 */
static int doit_match_erase(TreeDbTerm *this, void *ptr)
{
    struct match_erase_context *mc = (struct match_erase_context *) ptr;
    eTerm dummy,key;

    ZEROB(mc->bs);

    if (mc->partly_bound != NIL && 
	cmp_partly_bound(mc->partly_bound, 
			 GETKEY_WITH_POS(mc->keypos, 
					 this->dbterm.tpl)) > 0)
	return 0;

    if (db_do_match(make_tuple(this->dbterm.tpl), 
		    mc->pattern, &(mc->bs), NULL) != 0) {
	key = GETKEY(mc->tb, this->dbterm.tpl);
	db_erase_tree(mc->p, mc->tb, key, &dummy);
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
    sum += sizeof(TreeDbTerm)/sizeof(eTerm) + t->dbterm.size - 1;
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
