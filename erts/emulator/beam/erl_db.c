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
 * This file contains the bif interface functions and
 * the handling of the hash "meta table" ie the table of 
 * db tables.
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

/*
** Utility macros
*/

/* Get a key from any table structure and a tagged object */
#define TERM_GETKEY(tb, obj) db_getkey((tb)->common.keypos, (obj)) 
/* 
** The id in a tab_entry slot is
** DB_NOTUSED if it's never been used
** DB_USED if it's been freed
** An occupied slot has an (atom|small) id equal to the table's id
** This is so that we shall be able to terminate a search when we
** reach a point in the table that is impossible to reach if the id
** is there, we have to consider that tables can be removed thogh, so if
** we come to a removed slot, we must continue the search
*/

#define ISFREE(i)	((db_tables[i].id == DB_USED) || ISNOTUSED(i))
#define ISNOTUSED(i)	(db_tables[i].id == DB_NOTUSED)

/* 
** Globals 
*/

/* This is a hashlist of all tables we have */

static struct tab_entry {
    DbTable *t;
    uint32 id;            /* Automatically initialized */
    uint32 name;          /* An atom */
} *db_tables;  /* Local variable db_tables */

int user_requested_db_max_tabs;
static int db_max_tabs;
static int last_slot;
static int no_tabs;		/* Number of active tables */

/* 
** Forward decls, static functions 
*/
static void free_table(DbTable *tb);
static void print_table(CIO fd, int show,  DbTable* tb);
static int next_prime(int n);

/*
** BIF's
*/

/*
** Disables/enables rehashing for a table (if it is a hash table).
*/
BIF_RETTYPE ets_fixtable_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    eTerm arg;
    int cret;

    /* This doesn't affect trees, but who cares... */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    arg = BIF_ARG_2;
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_fixtable_hash(BIF_P, &(tb->hash), arg);
    } else {
	cret = DB_ERROR_NONE;
    }
    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (arg == am_true) 
      tb->common.status |= DB_FIXED;
    else if (arg == am_false)
      tb->common.status &= ~DB_FIXED;
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

/* 
** Returns the first Key in a table 
*/
BIF_RETTYPE ets_first_1(BIF_ALIST_1)
BIF_ADECL_1
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_first_hash(BIF_P, &(tb->hash), &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_first_tree(BIF_P, &(tb->tree), &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }
    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** The next BIF, given a key, return the "next" key 
*/
BIF_RETTYPE ets_next_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_next_hash(BIF_P, &(tb->hash), BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_next_tree(BIF_P, &(tb->tree), BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }
    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** Returns the first Key in a table 
*/
BIF_RETTYPE ets_last_1(BIF_ALIST_1)
BIF_ADECL_1
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	/* Hash tables have no order, last === first */
	cret = db_first_hash(BIF_P, &(tb->hash), &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_last_tree(BIF_P, &(tb->tree), &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }
    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** The next BIF, given a key, return the "next" key 
*/
BIF_RETTYPE ets_prev_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	/* Hash tables have no order, prev === next */
	cret = db_next_hash(BIF_P, &(tb->hash), BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_prev_tree(BIF_P, &(tb->tree), BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }
    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** update_counter(Tab, Key, Increment) 
** Returns new value (integer)
*/

BIF_RETTYPE ets_update_counter_3(BIF_ALIST_3)
BIF_ADECL_3
{
    DbTable* tb;
    eTerm ret;
    int cret;
    eTerm increment = BIF_ARG_3;
    int position = 0;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) { /*TT*/
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_3)) { /* key position specified */
	eTerm *tpl = tuple_val(BIF_ARG_3);
	if (arityval(*tpl) != 2 || !is_small(tpl[1]) ||
	    !(is_small(tpl[2]) || is_big(tpl[2]))) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	position = signed_val(tpl[1]);
	increment = tpl[2];
	if (position == tb->common.keypos) {
	   BIF_ERROR(BIF_P, BADARG);
	}
    }
	
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_update_counter_hash(BIF_P, &(tb->hash), 
				      BIF_ARG_2, increment, position, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_update_counter_tree(BIF_P, &(tb->tree), 
				      BIF_ARG_2, increment, position, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** The put BIF 
*/
BIF_RETTYPE ets_insert_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_tuple(BIF_ARG_2) || 
	(arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_put_hash(BIF_P, &(tb->hash), 
			   BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_put_tree(BIF_P, &(tb->tree), 
			   BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/*
** Rename a (possibly) named table
*/

BIF_RETTYPE ets_rename_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int oldslot, newslot;


    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_atom(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    oldslot = tb->common.slot;

    if (is_not_atom(tb->common.id)) { /* Not a named table */
	tb->common.the_name = BIF_ARG_2;
	BIF_RET(tb->common.id);
    }

    /* Ok, a named table, find a new slot for it */
    newslot = atom_val(BIF_ARG_2) % db_max_tabs;
    while (1) {
	if (ISFREE(newslot))
	    break;
	if (db_tables[newslot].id == BIF_ARG_2) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	if (++newslot == db_max_tabs)
	    newslot=0; 
    }
    db_tables[newslot].id = BIF_ARG_2;
    db_tables[newslot].t = tb;
    tb->common.id = tb->common.the_name = BIF_ARG_2;
    tb->common.slot = newslot;
    db_tables[oldslot].id = DB_USED;
    db_tables[oldslot].t = NULL;
    BIF_RET(tb->common.id);
}


/* 
** The create table BIF     
** Args: (Name, Properties) 
*/

BIF_RETTYPE ets_new_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int slot;
    eTerm list;
    eTerm val;
    eTerm ret;
    uint32 status;
    int keypos;
    int is_named;
    int cret;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_nil(BIF_ARG_2) && is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (no_tabs >  (db_max_tabs * 4) / 5) {
	cerr_pos = 0;
	erl_printf(CBUF, "** Too many db tables **\n");
	send_error_to_logger(BIF_P->group_leader);
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }

    status = DB_NORMAL | DB_SET | DB_LHASH | DB_PROTECTED;
    keypos = 1;
    is_named = 0;

    list = BIF_ARG_2;
    while(is_list(list)) {
	val = CAR(list_val(list));
	if (val == am_bag) {
	    status |= DB_BAG;
	    status &= ~(DB_SET | DB_DUPLICATE_BAG | DB_ORDERED_SET);
	}
	else if (val == am_duplicate_bag) {
	    status |= DB_DUPLICATE_BAG;
	    status &= ~(DB_SET | DB_BAG | DB_ORDERED_SET);
	}
	else if (val == am_ordered_set) {
	    status |= DB_ORDERED_SET;
	    status &= ~(DB_SET | DB_BAG | DB_DUPLICATE_BAG);
	}
	/*TT*/
	else if (is_tuple(val)) {
	    eTerm *tp = tuple_val(val);

	    if ((arityval(tp[0]) == 2) && (tp[1] == am_keypos) &&
		is_small(tp[2]) && (signed_val(tp[2]) > 0)) {
		keypos = signed_val(tp[2]);
	    }
	    else {
		BIF_ERROR(BIF_P, BADARG);
	    }
	}
	else if (val == am_public) {
	    status |= DB_PUBLIC;
	    status &= ~DB_PROTECTED;
	}
	else if (val == am_private) {
	    status |= DB_PRIVATE;
	    status &= ~DB_PROTECTED;
	}
	else if (val == am_named_table) {
	    is_named = 1;
	}
	else if (val == am_set || val == am_protected)
	    ;
	else {
	    BIF_ERROR(BIF_P, BADARG);
	}
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) /* it must be a well formed list */
	    BIF_ERROR(BIF_P, BADARG);

    /* allocate the slot for the table */
    if (++last_slot == db_max_tabs) 
	last_slot = 0;
    if (is_named) {
	slot = atom_val(BIF_ARG_1) % db_max_tabs;
	while (1) {
	    if (ISFREE(slot)) {
		ret = BIF_ARG_1;
		break;
	    }
	    if (db_tables[slot].id == BIF_ARG_1) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	    if (++slot == db_max_tabs) {
		slot=0; 
	    }
	}
    }
    else {  /* Allocate number slot */
	slot = last_slot;
	while(1) {
	    if (ISFREE(slot)) {
		ret = make_small(slot);
		break;
	    }
	    if (++slot == db_max_tabs) {
		slot=0; 
	    }
	}
    }

    /* Now slot and ret are properly set */
    /* ret will be the id for the table as well */
    /* Creat a new table and insert in db_tables */

    tb = (DbTable*) fix_alloc_from(55,table_desc);
    tb->common.id = ret;
    tb->common.the_name = BIF_ARG_1;
    tb->common.status = status;
    tb->common.keypos = keypos;
    tb->common.owner = BIF_P->id;

    tb->common.nitems = 0;

    tb->common.slot = slot;           /* store slot for erase */

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_create_hash(BIF_P, &(tb->hash));
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_create_tree(BIF_P, &(tb->tree));
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    if (cret != DB_ERROR_NONE) {
	fix_free(table_desc, (uint32 *) tb);
	BIF_ERROR(BIF_P, BADARG);
    }

    db_tables[slot].id = ret;  /* Insert the table */
    db_tables[slot].t = tb;
    
    BIF_P->flags |= F_USING_DB;        /* So we can remove tb if p dies */

    no_tabs++;

    BIF_RET(ret);
}

/* 
** The lookup BIF 
*/
BIF_RETTYPE ets_lookup_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_get_hash(BIF_P, &(tb->hash), 
			   BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_get_tree(BIF_P, &(tb->tree), 
			   BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }

}

/* 
** Get an element from a term
** get_element_3(Tab, Key, Index)
** return the element or a list of elements if bag
*/
BIF_RETTYPE ets_lookup_element_3(BIF_ALIST_3)
BIF_ADECL_3
{
    DbTable* tb;
    int index;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_small(BIF_ARG_3) || ((index = signed_val(BIF_ARG_3)) < 1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_get_element_hash(BIF_P, &(tb->hash), 
				   BIF_ARG_2, index, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_get_element_tree(BIF_P, &(tb->tree), 
				   BIF_ARG_2, index, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** BIF to erase a whole table and release all memory it holds 
*/
BIF_RETTYPE ets_db_delete_1(BIF_ALIST_1)
BIF_ADECL_1
{
    DbTable* tb;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    db_tables[tb->common.slot].id = DB_USED;
    db_tables[tb->common.slot].t = NULL;

    no_tabs--;

    free_table(tb); /* Takes care of different table types */
    BIF_RET(am_true);
}

/* 
** Erase an object, or maybe several objects if we have a bag  
** Called as db_erase(Tab, Key), where Key is element 1 of the
** object(s) we want to erase                                  
*/
BIF_RETTYPE ets_delete_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_erase_hash(BIF_P, &(tb->hash), 
			     BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_erase_tree(BIF_P, &(tb->tree), 
			     BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


/* 
** Call this BIF as db_erase(Tab,Pattern)
** If The first element in Pattern is bound, we go directly to the 
** right slot, otherwise, we search the whole table                
*/
BIF_RETTYPE ets_match_delete_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_match_erase_hash(BIF_P, &(tb->hash), 
				   BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_match_erase_tree(BIF_P, &(tb->tree), 
				   BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** Return a list of tables on this node 
*/
BIF_RETTYPE ets_all_0(BIF_ALIST_0)
BIF_ADECL_0
{
    DbTable* tb;
    uint32 previous;
    int i, j;
    uint32* hp = HAlloc(BIF_P, 2*no_tabs);

    previous = NIL;
    j = 0;
    for(i = 0; (i < db_max_tabs && j < no_tabs); i++) {
	if (!ISFREE(i)) {
	    j++;
	    tb = db_tables[i].t;
	    previous = CONS(hp, tb->common.id, previous);
	    hp += 2;
	}
    }
    ASSERT(j == no_tabs);
    BIF_RET(previous);
}


/*
** db_slot(Db, Slot) -> [Items].
*/
BIF_RETTYPE ets_slot_2(BIF_ALIST_2) 
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    /* The slot number is checked in table specific code. */

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_slot_hash(BIF_P, &(tb->hash), 
			    BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_slot_tree(BIF_P, &(tb->tree), 
			    BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* 
** The match BIF,  called as db_match(Table, Pattern)
*/

BIF_RETTYPE ets_match_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_match_hash(BIF_P, &(tb->hash), 
			     BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_match_tree(BIF_P, &(tb->tree), 
			     BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


BIF_RETTYPE ets_select_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_tree(BIF_P, &(tb->tree), 
			      BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


/* 
** db_match_object(Table, Pattern, State) 
*/
BIF_RETTYPE ets_db_match_object_3(BIF_ALIST_3)
BIF_ADECL_3
{
    DbTable* tb;
    int cret;
    eTerm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_match_object_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, BIF_ARG_3, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_match_object_tree(BIF_P, &(tb->tree), 
				    BIF_ARG_2, BIF_ARG_3, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


/* 
** BIF to extract information about a particular table
** Only the "memory" parameter generates table specific calls.
*/ 

BIF_RETTYPE ets_db_info_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DbTable* tb;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == am_size) 
	BIF_RET(make_small(tb->common.nitems));
    if (BIF_ARG_2 == am_type) {
	if (tb->common.status & DB_SET)  {
	    BIF_RET(am_set);
	}
	else if (tb->common.status & DB_DUPLICATE_BAG) {
	    BIF_RET(am_duplicate_bag);
	}
	else if (tb->common.status & DB_ORDERED_SET) {
	    BIF_RET(am_ordered_set);
	}
	/*TT*/
	else {
	    BIF_RET(am_bag);
	}
    }
    if (BIF_ARG_2 == am_memory) {
	int cret;
	eTerm ret;
	int reds;

	if (IS_HASH_TABLE(tb->common.status)) {
	    cret = db_info_memory_hash(BIF_P, &(tb->hash), 
				       &ret, &reds);
	} else if (IS_TREE_TABLE(tb->common.status)) {
	    cret = db_info_memory_tree(BIF_P, &(tb->tree), 
				       &ret, &reds);
	/*TT*/
	} else {
	    cret = DB_ERROR_UNSPEC;
	}
	
	switch (cret) {
	case DB_ERROR_NONE:
	    BIF_RET2(ret, reds);
	case DB_ERROR_SYSRES:
	    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
	default:
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    if (BIF_ARG_2 == am_owner) 
	BIF_RET(tb->common.owner);
    if (BIF_ARG_2 == am_protection) {
	if (tb->common.status & DB_PRIVATE) 
	    BIF_RET(am_private);
	if (tb->common.status & DB_PROTECTED)
	    BIF_RET(am_protected);
	if (tb->common.status & DB_PUBLIC)
	    BIF_RET(am_public);
    }
    if (BIF_ARG_2 == am_name)
	BIF_RET(tb->common.the_name);
    if (BIF_ARG_2 == am_keypos) 
	BIF_RET(make_small(tb->common.keypos));
    /* For debugging purpouses */
    if (BIF_ARG_2 == am_data) { 
	print_table(COUT, 1, tb);
	BIF_RET(am_true);
    }
    if (BIF_ARG_2 == am_atom_put("fixed",5)) { 
	if (tb->common.status & DB_FIXED)
	    BIF_RET(am_true);
	else
	    BIF_RET(am_false);
    }
    BIF_ERROR(BIF_P, BADARG);
}


BIF_RETTYPE ets_match_spec_compile_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Binary *mp = db_match_set_compile(BIF_P, BIF_ARG_1, 
				      DCOMP_BODY_RETURN);
    ProcBin *pb;
    if (mp == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    mp->refc++;
    pb = (ProcBin *) HAlloc(BIF_P, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = BIF_P->off_heap.mso;
    BIF_P->off_heap.mso = pb;
    pb->val = mp;
    pb->bytes = mp->orig_bytes;
    BIF_RET(make_binary(pb));
}

BIF_RETTYPE ets_match_spec_run_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Binary *mp;
    Eterm *hp;
    Eterm *last_cdr = NULL;
    Eterm res;
    Eterm ret = NIL;
    Eterm lst;
    unsigned sz;
    Uint32 dummy;
    ProcBin *bp;


    if (!(is_list(BIF_ARG_1) || BIF_ARG_1 == NIL) || !is_binary(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    
    bp = (ProcBin*) binary_val(BIF_ARG_2);
    if (thing_subtag(bp->thing_word) != REFC_BINARY_SUBTAG) {
	goto error;
    }
    mp = bp->val;
    if (!(mp->flags & BIN_FLAG_MATCH_PROG)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_ARG_1 == NIL) {
	BIF_RET(NIL);
    }

    for (lst = BIF_ARG_1; is_list(lst); lst = CDR(list_val(lst))) {
	res = db_prog_match(BIF_P, mp, CAR(list_val(lst)), 0, &dummy);
	if (is_value(res)) {
	    sz = size_object(res);
	    hp = HAlloc(BIF_P, sz + 2);
	    res = copy_struct(res, sz, &hp, &(BIF_P->off_heap));
	    if (last_cdr == NULL) {
		/* First object in list */
		ret = CONS(hp, res, NIL);
	    } else {
		*last_cdr = CONS(hp, res, NIL);
	    }
	    last_cdr = hp + 1;
	} else if (BIF_ARG_3 != 0U) {
	    hp = HAlloc(BIF_P, 2);
	    if (last_cdr == NULL) {
		/* First object in list */
		ret = CONS(hp, BIF_ARG_3, NIL);
	    } else {
		*last_cdr = CONS(hp, BIF_ARG_3, NIL);
	    }
	    last_cdr = hp + 1;
	}
    }
    BIF_RET(ret);
}
	
BIF_RETTYPE ets_match_spec_run_2(BIF_ALIST_2)
BIF_ADECL_2
{
    return ets_match_spec_run_3(BIF_P, BIF_ARG_1, BIF_ARG_2, 0U);
}

/*
** External interface (NOT BIF's)
*/


/* Init the db */

void init_db(void)
{
    int i;
    int max_ets;

    last_slot = 0;
    db_initialize_util();
    if (( max_ets = (user_requested_db_max_tabs*5)/4 ) < DB_DEF_MAX_TABS)
	db_max_tabs = DB_DEF_MAX_TABS;
    else
	db_max_tabs = next_prime(max_ets);

    db_tables = safe_alloc(sizeof(struct tab_entry)*db_max_tabs);
    no_tabs = 0;
    for (i=0; i<db_max_tabs; i++) {
	db_tables[i].id = DB_NOTUSED;
	db_tables[i].t = NULL;
    }
    db_am_eot = am_magic_word("$end_of_table");
    db_initialize_hash();
    db_initialize_tree();
    /*TT*/
}

/* Called when  a process which has created any tables dies */
/* So that we can remove the tables ceated by the process   */

void db_proc_dead(eTerm pid)
{
    int i;

    for (i=0; i<db_max_tabs; i++) {
	DbTable* tb = db_tables[i].t;
	if ((tb != NULL)  && (tb->common.owner == pid)) {
	    free_table(tb);
	    no_tabs--;
	    db_tables[i].id = DB_USED;
	    db_tables[i].t = NULL;
	}
    }
}

/*
** Internal functions.
*/


static int next_prime(int n)
{
    int i;
    if (n % 2 == 0) /* No even numbers... */
	++n;
    for (;;) {
	for (i = 3; (i*i) <= n; i+=2) {
	    if (n % i == 0)
		break;
	}
	if ((i*i) > n)
	    return n;
	n += 2;
    }
}

DbTable* db_get_table(Process *p, eTerm id, int what)
{
    int i, j;

    if (is_small(id))
	j = unsigned_val(id);
    else if (is_atom(id))
	j = atom_val(id);
    else
	return NULL;

    i = j = j % db_max_tabs;
    while (1) {
	if (db_tables[i].id == id) {
	    DbTable* tb = db_tables[i].t;
	    if ((tb->common.status & what) != 0 || 
		p->id == tb->common.owner) {
		return tb;
	    }
	    return NULL;
	}
	if (ISNOTUSED(i++))
	    return NULL;
	if (i == db_max_tabs) 
	    i = 0; 
	if (i == j)
	    return NULL;
    }
    return NULL;
}

static void free_table(DbTable *tb) {
    if (IS_HASH_TABLE(tb->common.status))
	free_hash_table(&(tb->hash));
    else if (IS_TREE_TABLE(tb->common.status))
	free_tree_table(&(tb->tree));
    /*TT*/
    else
	erl_exit(1,"Panic: Unknown table type (status word = 0x%08x)!",
		 tb->common.status);
    
    fix_free(table_desc, (uint32*) tb);
}

static void print_table(CIO fd, int show,  DbTable* tb)
{
    int sum = 0;

    erl_printf(fd, "Table "); display(tb->common.id,fd);
    erl_printf(fd, "(with name)" );
    display(tb->common.the_name, fd);
    erl_printf(fd, "\n");

    erl_printf(fd, "Owner "); display(tb->common.owner,fd);
    erl_printf(fd, "\n");

    if (IS_HASH_TABLE(tb->common.status)) {
	db_print_hash(fd, show, &(tb->hash), &sum);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	db_print_tree(fd, show, &(tb->tree), &sum);
	/*TT*/
    } else {
	erl_printf(fd,"Table is of unknown type!\n");
    }
    erl_printf(fd,"Table's got %d objects\n", tb->common.nitems);
    erl_printf(fd,"Table's got %d words of active data\n", sum);
    erl_printf(fd,"\n");
}


void db_info(CIO fd, int show)    /* Called by break handler */
{
    int i;
    for (i=0; i < db_max_tabs; i++) 
	if (!ISFREE(i)) {
	    erl_printf(fd, "In slot %d\n", i);
	    print_table(fd, show, db_tables[i].t);
	}
}


#ifdef HARDDEBUG   /* Here comes some debug functions */

void db_check_tables(void)
{
    int i;

    for (i = 0; i < db_max_tabs; i++) {
	if (!ISFREE(i)) {
	    DbTable* tb = db_tables[i].t; 
	    if (IS_HASH_TABLE(tb->common.status)) {
		db_check_table_hash(&(tb->hash));
	    } else if (IS_TREE_TABLE(tb->common.status)) {
		db_check_table_tree(&(tb->tree));
		/*TT*/
	    } else {
		erl_exit(1,"Panic: Unknown table type "
			 "(status word = 0x%08x)!",
			 tb->common.status);
	    }
	}
    }
}

#endif /* HARDDEBUG */
