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
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "bif.h"
#include "big.h"


Uint erts_tot_ets_memory_size;

/*
** Utility macros
*/

/* Get a key from any table structure and a tagged object */
#define TERM_GETKEY(tb, obj) db_getkey((tb)->common.keypos, (obj)) 

/* Utility macros for determining need of auto fixtable */

#define ONLY_WRITER(P,T) (((T)->common.status & DB_PRIVATE) || \
(((T)->common.status & DB_PROTECTED) && (T)->common.owner == (P)->id))
#define ONLY_READER(P,T) (((T)->common.status & DB_PRIVATE) && \
(T)->common.owner == (P)->id)
#define DID_TRAP(P,Ret) (!is_value(Ret) && ((P)->freason == TRAP))
#define SOLE_LOCKER(P,Fixations) ((Fixations) != NULL && \
(Fixations)->next == NULL && (Fixations)->pid == (P)->id && \
(Fixations)->counter == 1)

/* 
** The id in a tab_entry slot is
** DB_NOTUSED if it's never been used(*)
** DB_USED if it's been freed(*)
** An occupied slot has an (atom|small) id equal to the table's id
** This is so that we shall be able to terminate a search when we
** reach a point in the table that is impossible to reach if the id
** is there, we have to consider that tables can be removed thogh, so if
** we come to a removed slot, we must continue the search
** (*) A slot that has been used can actually be marked as DB_NOTUSED, 
** that happens if the next slot is also DB_NOTUSED when freeing.
** Then the slot we're freeing need not be marked as 
** DB_USED as you needn't continue search in that case.
**
**
*/

#define ISFREE(i)	((db_tables[i].id == DB_USED) || ISNOTUSED(i))
#define ISNOTUSED(i)	(db_tables[i].id == DB_NOTUSED)

/* 
** Globals 
*/

/* This is a hashlist of all tables we have */

static struct tab_entry {
    DbTable *t;
    Uint id;            /* Automatically initialized */
    Eterm name;          /* An atom */
} *db_tables;  /* Local variable db_tables */

int user_requested_db_max_tabs;
int erts_ets_realloc_always_moves;
static int db_max_tabs;
static int last_slot;
static int no_tabs;		/* Number of active tables */
static DbTable *meta_pid_to_tab; /* Pid mapped to owned tables */
static DbTable *meta_pid_to_fixed_tab; /* Pid mapped to fixed tables */
static Eterm ms_delete_all;
static Eterm ms_delete_all_buff[8]; /* To compare with for deletion 
				       of all objects */
/* 
** Forward decls, static functions 
*/
static void meta_mark_free(int idx);
static void free_table(DbTable *tb);
static void free_fixations(DbTable *tb);
static Eterm free_table_cont(Process *p, DbTable *tb, int first);
static void print_table(CIO fd, int show,  DbTable* tb);
static int next_prime(int n);
static BIF_RETTYPE ets_select_delete_1(Process *p, Eterm a1);
static BIF_RETTYPE ets_select_count_1(Process *p, Eterm a1);
static BIF_RETTYPE ets_select_trap_1(Process *p, Eterm a1);

/* 
 * Exported global
 */
Export ets_select_delete_continue_exp;
Export ets_select_count_continue_exp;
Export ets_select_continue_exp;

/*
** BIF's
*/

/*
** Disables/enables rehashing for a table (if it is a hash table).
*/
BIF_RETTYPE ets_fixtable_2(BIF_ALIST_2)
{
    DbTable* tb;
    Eterm arg;
    int cret;

    /* This doesn't affect trees, but who cares... */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    arg = BIF_ARG_2;

    if (arg == am_true) {
	return ets_safe_fixtable_2(BIF_P, BIF_ARG_1, BIF_ARG_2);
    } else if (arg == am_false) {
	DbFixation *fix;

	for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
	    db_erase_bag_exact2(&(meta_pid_to_fixed_tab->hash),
				fix->pid,
				make_small(tb->common.slot));
	}
	while (tb->common.fixations != NULL) {
	    fix = tb->common.fixations;
	    tb->common.fixations = fix->next;
	    erts_db_free(ERTS_ALC_T_DB_FIXATION,
			 tb, (void *) fix, sizeof(DbFixation));
	}
	if (IS_HASH_TABLE(tb->common.status)) {
	    cret = db_fixtable_hash(&(tb->hash), am_false);
	}	
	tb->common.status &= ~DB_FIXED;
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE ets_safe_fixtable_2(BIF_ALIST_2)
{
    DbTable *tb;
    int cret;
    DbFixation *fix,**pp;
    Eterm dummy;
    Eterm meta_tuple[3];

#ifdef HARDDEBUG
    erl_printf(CERR,"ets:safe_fixtable(");
    display(BIF_ARG_1,CERR);
    erl_printf(CERR,",");
    display(BIF_ARG_2,CERR);
    erl_printf(CERR,"); Process: ");
    display(BIF_P->id,CERR);
    erl_printf(CERR,", initial: ");
    display(BIF_P->initial[0],CERR);
    erl_printf(CERR,":");
    display(BIF_P->initial[1],CERR);
    erl_printf(CERR,"/%d\r\n",BIF_P->initial[2]);
#endif
    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    

    cret = DB_ERROR_NONE;    
    if (BIF_ARG_2 == am_true) {
	if (!(tb->common.status & DB_FIXED)) {
	    if (IS_HASH_TABLE(tb->common.status)) {
		cret = db_fixtable_hash(&(tb->hash), am_true);
	    }
	}
	if (cret == DB_ERROR_NONE) {
	    if (!(tb->common.status & DB_FIXED)) { 
		tb->common.status |= DB_FIXED;
		get_now(&(tb->common.megasec),
			&(tb->common.sec), 
			&(tb->common.microsec));
	    }
	    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
		if (fix->pid == BIF_P->id) {
		    ++(fix->counter);
		    break;
		}
	    }
	    if (fix == NULL) {
		fix = (DbFixation *) erts_db_alloc(ERTS_ALC_T_DB_FIXATION,
						   tb, sizeof(DbFixation));
		fix->pid = BIF_P->id;
		fix->counter = 1;
		fix->next = tb->common.fixations;
		tb->common.fixations = fix;
		BIF_P->flags |= F_USING_DB;        /* So we can remove tb if p dies */
		if (db_put_hash(NULL,&(meta_pid_to_fixed_tab->hash),
				TUPLE2(meta_tuple, 
				       BIF_P->id, 
				       make_small(tb->common.slot)),
				&dummy)
		    != DB_ERROR_NONE) {
		    erl_exit(1,"Could not insert ets metadata"
			     " in safe_fixtable.");
		}	
	    }
	}
    } else if (BIF_ARG_2 == am_false) {
	if (!(tb->common.status & DB_FIXED)) {
	    goto done;
	}

	for (pp = &(tb->common.fixations); *pp != NULL;
	     pp = &((*pp)->next)) {
	    if ((*pp)->pid == BIF_P->id) {
		--((*pp)->counter);
		ASSERT((*pp)->counter >= 0);
		if ((*pp)->counter == 0) {
		    fix = *pp;
		    *pp = (*pp)->next;
		    db_erase_bag_exact2(&(meta_pid_to_fixed_tab->hash),
					BIF_P->id,
					make_small(tb->common.slot));
		    erts_db_free(ERTS_ALC_T_DB_FIXATION,
				 tb, (void *) fix, sizeof(DbFixation));
		}
		break;
	    }
	}
	if (tb->common.fixations == NULL) {
	    if (IS_HASH_TABLE(tb->common.status)) {
		cret = db_fixtable_hash(&(tb->hash), am_false);
	    }	
	    tb->common.status &= ~DB_FIXED;
	}
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
done:
    BIF_RET(am_true);
}

/* 
** Returns the first Key in a table 
*/
BIF_RETTYPE ets_first_1(BIF_ALIST_1)
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
{
    DbTable* tb;
    Eterm ret;
    int cret;
    Eterm increment = BIF_ARG_3;
    Sint position = 0;
    Eterm threshold = NIL;
    Eterm warp_to = NIL;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) { /*TT*/
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_3)) { /* key position specified */
	Eterm *tpl = tuple_val(BIF_ARG_3);
	switch (arityval(*tpl)) {
	case 4: /* threshold specified */
	    if (!(is_small(tpl[3]) || is_big(tpl[3])) ||
		!(is_small(tpl[4]) || is_big(tpl[4]))) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	    threshold = tpl[3];
	    warp_to = tpl[4];
	    /* Fall through */
	case 2:
	    if (!is_small(tpl[1]) ||
		!(is_small(tpl[2]) || is_big(tpl[2]))) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	    position = signed_val(tpl[1]);
	    increment = tpl[2];
	    if (position == tb->common.keypos) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	    break;
	default:
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
	
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_update_counter_hash(BIF_P, &(tb->hash), 
				      BIF_ARG_2, increment, 0, position, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_update_counter_tree(BIF_P, &(tb->tree), 
				      BIF_ARG_2, increment, 0, position, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    if (cret == DB_ERROR_NONE &&
	threshold != NIL) { /* Maybe warp it */
	if ((cmp(increment,make_small(0)) < 0) ? /* negative increment? */
	    (cmp(ret,threshold) < 0) :  /* if negative, check if below */
	    (cmp(ret,threshold) > 0)) { /* else check if above threshold */
	    if (IS_HASH_TABLE(tb->common.status)) {
		cret = 
		    db_update_counter_hash(BIF_P, &(tb->hash), 
					   BIF_ARG_2, warp_to, 1, 
					   position, &ret);
	    } else if (IS_TREE_TABLE(tb->common.status)) {
		cret = 
		    db_update_counter_tree(BIF_P, &(tb->tree), 
					   BIF_ARG_2, warp_to, 1, 
					   position, &ret);
		/*TT*/
	    } /* table type already checked */
	}
    }

    switch (cret) {
    case DB_ERROR_NONE:
	/* Check for threshold */
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
{
    DbTable* tb;
    int cret = DB_ERROR_NONE;
    Eterm ret = am_true;
    Eterm lst;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	BIF_RET(am_true);
    }
    if (is_list(BIF_ARG_2)) {
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    if (is_not_tuple(CAR(list_val(lst))) || 
		(arityval(*tuple_val(CAR(list_val(lst)))) < tb->common.keypos)) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	}
	if (lst != NIL) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	if (IS_HASH_TABLE(tb->common.status)) {
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_put_hash(BIF_P, &(tb->hash), 
					CAR(list_val(lst)), &ret)) != DB_ERROR_NONE)
		    break;
	    }
	} else if (IS_TREE_TABLE(tb->common.status)) {
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_put_tree(BIF_P, &(tb->tree), 
					CAR(list_val(lst)), &ret)) != DB_ERROR_NONE)
		    break;
	    }
	    /*TT*/
	} else {
	    cret = DB_ERROR_UNSPEC;
	}
    } else {
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
** The put-if-not-already-there BIF... 
*/
BIF_RETTYPE ets_insert_new_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret = DB_ERROR_NONE;
    Eterm ret = am_true;
    Eterm lst;
    Eterm lookup_ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	BIF_RET(am_true);
    }
    if (is_list(BIF_ARG_2)) {
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    if (is_not_tuple(CAR(list_val(lst))) || 
		(arityval(*tuple_val(CAR(list_val(lst)))) < tb->common.keypos)) {
		BIF_ERROR(BIF_P, BADARG);
	    }
	}
	if (lst != NIL) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	if (IS_HASH_TABLE(tb->common.status)) {
	    /* Loop through list first to verify that *all* keys are "free" */
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_member_hash(BIF_P, &(tb->hash), 
					TERM_GETKEY(tb,CAR(list_val(lst))), 
					&lookup_ret)) != DB_ERROR_NONE ||
		    (lookup_ret != am_false)) {
		    ret = am_false;
		    goto done;
		}
	    }
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_put_hash(BIF_P, &(tb->hash), 
					CAR(list_val(lst)), &ret)) != DB_ERROR_NONE)
		    break;
	    }
	} else if (IS_TREE_TABLE(tb->common.status)) {
	    /* Loop through list first to verify that *all* keys are "free" */
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_member_tree(BIF_P, &(tb->tree), 
					TERM_GETKEY(tb,CAR(list_val(lst))), 
					&lookup_ret)) != DB_ERROR_NONE ||
		    (lookup_ret != am_false)) {
		    ret = am_false;
		    goto done;
		}
	    }
	    for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
		if ((cret = db_put_tree(BIF_P, &(tb->tree), 
					CAR(list_val(lst)), &ret)) != DB_ERROR_NONE)
		    break;
	    }
	    /*TT*/
	} else {
	    cret = DB_ERROR_UNSPEC;
	}
    } else {
	if (is_not_tuple(BIF_ARG_2) || 
	    (arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	if (IS_HASH_TABLE(tb->common.status)) {
	    if ((cret = db_member_hash(BIF_P, &(tb->hash), 
				       TERM_GETKEY(tb,BIF_ARG_2), 
				       &lookup_ret)) != DB_ERROR_NONE ||
		(lookup_ret != am_false)) {
		ret = am_false;
	    } else {
		cret = db_put_hash(BIF_P, &(tb->hash), 
				   BIF_ARG_2, &ret);
	    }
	} else if (IS_TREE_TABLE(tb->common.status)) {
	    if ((cret = db_member_tree(BIF_P, &(tb->tree), 
				       TERM_GETKEY(tb,BIF_ARG_2), 
				       &lookup_ret)) != DB_ERROR_NONE ||
		(lookup_ret != am_false)) {
		ret = am_false;
	    } else {
		cret = db_put_tree(BIF_P, &(tb->tree), 
				   BIF_ARG_2, &ret);
	    }
	    /*TT*/
	} else {
	    cret = DB_ERROR_UNSPEC;
	}
    }
done:
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
{
    DbTable* tb;
    int oldslot, newslot;
    Eterm dummy;
    Eterm meta_tuple[3];
    DbFixation *fix;

#ifdef HARDDEBUG
    erl_printf(CERR,"ets:rename(");
    display(BIF_ARG_1,CERR);
    erl_printf(CERR,",");
    display(BIF_ARG_2,CERR);
    erl_printf(CERR,"); Process: ");
    display(BIF_P->id,CERR);
    erl_printf(CERR,", initial: ");
    display(BIF_P->initial[0],CERR);
    erl_printf(CERR,":");
    display(BIF_P->initial[1],CERR);
    erl_printf(CERR,"/%d\r\n",BIF_P->initial[2]);
#endif

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
    meta_mark_free(oldslot);
    db_tables[oldslot].t = NULL;

    if (db_put_hash(NULL,&(meta_pid_to_tab->hash),
		    TUPLE2(meta_tuple, 
			   tb->common.owner, 
			   make_small(newslot)),
		    &dummy)
	!= DB_ERROR_NONE) {
	erl_exit(1,"Could not insert ets metadata in rename.");
    }

    db_erase_bag_exact2(&(meta_pid_to_tab->hash),
			tb->common.owner,make_small(oldslot));

    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
	db_erase_bag_exact2(&(meta_pid_to_fixed_tab->hash),
			    fix->pid,
			    make_small(oldslot));
	if (db_put_hash(NULL,&(meta_pid_to_fixed_tab->hash),
			TUPLE2(meta_tuple, 
			       fix->pid, 
			       make_small(newslot)),
			&dummy)
	    != DB_ERROR_NONE) {
	    erl_exit(1,"Could not insert ets metadata"
		     " in rename.");
	}		
    }
    BIF_RET(tb->common.id);
}


/* 
** The create table BIF     
** Args: (Name, Properties) 
*/

BIF_RETTYPE ets_new_2(BIF_ALIST_2)
{
    DbTable* tb;
    int slot;
    Eterm list;
    Eterm val;
    Eterm ret;
    Uint32 status;
    Sint keypos;
    int is_named;
    int cret;
    Eterm dummy;
    Eterm meta_tuple[3];

#ifdef HARDDEBUG
    erl_printf(CERR,"ets:new(");
    display(BIF_ARG_1,CERR);
    erl_printf(CERR,",");
    display(BIF_ARG_2,CERR);
    erl_printf(CERR,"); Process: ");
    display(BIF_P->id,CERR);
    erl_printf(CERR,", initial: ");
    display(BIF_P->initial[0],CERR);
    erl_printf(CERR,":");
    display(BIF_P->initial[1],CERR);
    erl_printf(CERR,"/%d\r\n",BIF_P->initial[2]);
#endif

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
	    Eterm *tp = tuple_val(val);

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

    {
        DbTable init_tb;
	init_tb.common.memory_size = 0;
	tb = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
				      &init_tb,
				      sizeof(DbTable));
	tb->common.memory_size = init_tb.common.memory_size;
    }

    tb->common.id = ret;
    tb->common.the_name = BIF_ARG_1;
    tb->common.status = status;
    tb->common.keypos = keypos;
    tb->common.owner = BIF_P->id;

    tb->common.nitems = 0;
    tb->common.kept_items = 0;

    tb->common.slot = slot;           /* store slot for erase */

    tb->common.fixations = NULL;

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_create_hash(BIF_P, &(tb->hash));
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_create_tree(BIF_P, &(tb->tree));
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    if (cret != DB_ERROR_NONE) {
	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
	BIF_ERROR(BIF_P, BADARG);
    }

    db_tables[slot].id = ret;  /* Insert the table */
    db_tables[slot].t = tb;
    
    BIF_P->flags |= F_USING_DB;        /* So we can remove tb if p dies */

    no_tabs++;

    if (db_put_hash(NULL,&(meta_pid_to_tab->hash),TUPLE2(meta_tuple, BIF_P->id, 
							 make_small(slot)),&dummy)
	!= DB_ERROR_NONE) {
	erl_exit(1,"Could not update ets metadata.");
    }

    BIF_RET(ret);
}

/* 
** The lookup BIF 
*/
BIF_RETTYPE ets_lookup_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
** The lookup BIF 
*/
BIF_RETTYPE ets_member_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_member_hash(BIF_P, &(tb->hash), 
			      BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_member_tree(BIF_P, &(tb->tree), 
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
{
    DbTable* tb;
    Sint index;
    int cret;
    Eterm ret;

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
{
    DbTable* tb;

    if (is_CP(BIF_ARG_1)) {
	/*
	 * We have been called through a trap.
	 */
	return free_table_cont(BIF_P, (DbTable *) BIF_ARG_1, 0);
    }

#ifdef HARDDEBUG
    erl_printf(CERR,"ets:delete(");
    display(BIF_ARG_1,CERR);
    erl_printf(CERR,"); Process: ");
    display(BIF_P->id,CERR);
    erl_printf(CERR,", initial: ");
    display(BIF_P->initial[0],CERR);
    erl_printf(CERR,":");
    display(BIF_P->initial[1],CERR);
    erl_printf(CERR,"/%d\r\n",BIF_P->initial[2]);
#endif

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    /*
     * Clear all access bits to prevent any ets operation to access the
     * table while it is being deleted.
     */
    tb->common.status &= ~(DB_PROTECTED|DB_PUBLIC|DB_PRIVATE);
    if (tb->common.owner != BIF_P->id) {
	Eterm dummy;
	Eterm meta_tuple[3];

	/*
	 * The process is being deleted by a process other than its owner.
	 * To make sure that the table will be completely deleted if the
	 * current process will be killed (e.g. by an EXIT signal), we will
	 * now transfer the ownership to the current process.
	 */
	db_erase_bag_exact2(&(meta_pid_to_tab->hash), tb->common.owner,
			    make_small(tb->common.slot));
	BIF_P->flags |= F_USING_DB;
	tb->common.owner = BIF_P->id;
	db_put_hash(NULL,
		    &(meta_pid_to_tab->hash),
		    TUPLE2(meta_tuple,BIF_P->id,make_small(tb->common.slot)),
		    &dummy);
    }

    free_fixations(tb);
    return free_table_cont(BIF_P, tb, 1);
}

/* 
** BIF to erase a whole table and release all memory it holds 
*/
BIF_RETTYPE ets_delete_all_objects_1(BIF_ALIST_1)
{
    DbTable* tb;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	if (tb->common.status & DB_FIXED) {
	    db_mark_all_deleted_hash(&(tb->hash));
	} else {
	    free_hash_table(&(tb->hash));
	    db_create_hash(BIF_P, &(tb->hash));
	    tb->common.nitems = 0;
	}
    } else if (IS_TREE_TABLE(tb->common.status)) {
	free_tree_table(&(tb->tree));
	db_create_tree(BIF_P, &(tb->tree));
	tb->common.nitems = 0;
    /*TT*/
    } else {
	erl_exit(1,"Panic: Unknown table type (status word = 0x%08x)!",
		 tb->common.status);
    }

    BIF_RET(am_true);
}

/* 
** Erase an object with given key, or maybe several objects if we have a bag  
** Called as db_erase(Tab, Key), where Key is element 1 of the
** object(s) we want to erase                                  
*/
BIF_RETTYPE ets_delete_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
** Erase a specific object, or maybe several objects if we have a bag  
*/
BIF_RETTYPE ets_delete_object_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_tuple(BIF_ARG_2) || 
	(arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_erase_object_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_erase_object_tree(BIF_P, &(tb->tree), 
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
** This is for trapping, cannot be called directly.
*/
static BIF_RETTYPE ets_select_delete_1(Process *p, Eterm a1)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;
    
    CHECK_TABLES();
#ifdef DEBUG
    /*
     * Make sure that the table exists.
     */
    if (!is_tuple(a1)) {
	/* "Cannot" happen, this is not a real BIF, its trapped
	   to under controlled conditions */
	erl_exit(1,"Internal error in ets:select_delete");
    }
#endif
    tptr = tuple_val(a1);
#ifdef DEBUG
    if (arityval(*tptr) < 1) {
	erl_exit(1,"Internal error in ets:select_delete");
    }
#endif
    
    if ((tb = db_get_table(p, tptr[1], 
			   DB_WRITE)) == NULL) {
	/* This should be OK, the emulator handles errors in BIF's that aren't
	   exported nowdays... */
	BIF_ERROR(p,BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	DbFixation *fix = tb->common.fixations;
	if ((tb->common.status & DB_FIXED) && !ONLY_READER(p,tb) &&
	    SOLE_LOCKER(p,fix)) {
	    cret = db_select_delete_continue_hash(p, &(tb->hash), a1, &ret,1);
	} else {
	    cret = db_select_delete_continue_hash(p, &(tb->hash), a1, &ret,0);
	}
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_delete_continue_tree(p, &(tb->tree), a1, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !DID_TRAP(p,ret) &&
	   !ONLY_READER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_RET(ret);
    default:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !ONLY_READER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_ERROR(p, BADARG);
    }
}
    

BIF_RETTYPE ets_select_delete_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if(eq(BIF_ARG_2, ms_delete_all)){
	int nitems = tb->common.nitems;
	ret = ets_delete_all_objects_1(BIF_P,BIF_ARG_1);
	if (ret == am_true) {
	    BIF_RET(make_small_or_big(nitems,BIF_P));
	} else {
	    return ret;
	}
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_delete_hash(BIF_P, &(tb->hash), 
				     BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_delete_tree(BIF_P, &(tb->tree), 
				     BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_READER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
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
{
    DbTable* tb;
    Eterm previous;
    int i, j;
    Eterm* hp = HAlloc(BIF_P, 2*no_tabs);

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
{
    DbTable* tb;
    int cret;
    Eterm ret;

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
** The match BIF,  called as ets:match(Table, Pattern), ets:match(Continuation) or ets:match(Table,Pattern,ChunkSize).
*/

BIF_RETTYPE ets_match_1(BIF_ALIST_1)
{
    return ets_select_1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_match_2(BIF_ALIST_2)
{
    Eterm ms;
    Eterm buff[8];
    Eterm *hp = buff;
    /*hp = HAlloc(BIF_P, 8);*/
    ms = CONS(hp, am_DollarDollar, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    return ets_select_2(BIF_P, BIF_ARG_1, ms);
}

BIF_RETTYPE ets_match_3(BIF_ALIST_3)
{
    Eterm ms;
    Eterm buff[8];
    Eterm *hp = buff;
    /*hp = HAlloc(BIF_P, 8);*/
    ms = CONS(hp, am_DollarDollar, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    return ets_select_3(BIF_P, BIF_ARG_1, ms, BIF_ARG_3);
}


BIF_RETTYPE ets_select_3(BIF_ALIST_3)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Sint chunk_size;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Chunk size strictly greater than 0 */
    if (is_not_small(BIF_ARG_3) || (chunk_size = signed_val(BIF_ARG_3)) <= 0) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, chunk_size, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_chunk_tree(BIF_P, &(tb->tree), 
				    BIF_ARG_2, chunk_size, 0, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_trap_1(Process *p, Eterm a1)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1)
    if ((tb = db_get_table(p, tptr[1], DB_READ)) == NULL) {
	BIF_ERROR(p, BADARG);
    }
    
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash_continue(p, &(tb->hash), 
				       a1, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_tree_continue(p, &
				       (tb->tree), 
				       a1, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !DID_TRAP(p,ret) &&
	   !ONLY_WRITER(p,tb)) {
	    /* We did trap, but no more... */
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} /* Otherwise keep it fixed */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !ONLY_WRITER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_ERROR(p, SYSTEM_LIMIT);
    default:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !ONLY_WRITER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_ERROR(p, BADARG);
    }
}


BIF_RETTYPE ets_select_1(BIF_ALIST_1)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;

    CHECK_TABLES();

    /*
     * Make sure that the table exists.
     */

    if (!is_tuple(BIF_ARG_1)) {
	if (BIF_ARG_1 == am_EOT) {
	    BIF_RET(am_EOT);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    tptr = tuple_val(BIF_ARG_1);
    if (arityval(*tptr) < 1 || (tb = db_get_table(BIF_P, tptr[1], DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash_continue(BIF_P, &(tb->hash), 
				       BIF_ARG_1, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_tree_continue(BIF_P, &
				       (tb->tree), 
				       BIF_ARG_1, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE ets_select_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash(BIF_P, &(tb->hash), 
			      BIF_ARG_2, 0 /* chunk size */, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_tree(BIF_P, &(tb->tree), 
			      BIF_ARG_2, 0 /* not reversed */, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_count_1(Process *p, Eterm a1)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1)
    if ((tb = db_get_table(p, tptr[1], DB_READ)) == NULL) {
	BIF_ERROR(p, BADARG);
    }
    
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_count_hash_continue(p, &(tb->hash), 
					     a1, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_count_tree_continue(p, &(tb->tree), 
					     a1, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !DID_TRAP(p,ret) &&
	   !ONLY_WRITER(p,tb)) {
	    /* We did trap, but no more... */
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} /* Otherwise keep it fixed */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !ONLY_WRITER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_ERROR(p, SYSTEM_LIMIT);
    default:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !ONLY_WRITER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	BIF_ERROR(p, BADARG);
    }
}

BIF_RETTYPE ets_select_count_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_count_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_count_tree(BIF_P, &(tb->tree), 
				    BIF_ARG_2, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


BIF_RETTYPE ets_select_reverse_3(BIF_ALIST_3)
{
    DbTable* tb;
    int cret;
    Eterm ret;
    Sint chunk_size;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Chunk size strictly greater than 0 */
    if (is_not_small(BIF_ARG_3) || (chunk_size = signed_val(BIF_ARG_3)) <= 0) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash(BIF_P, &(tb->hash), 
				    BIF_ARG_2, chunk_size, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_chunk_tree(BIF_P, &(tb->tree), 
				    BIF_ARG_2, chunk_size, 1, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE ets_select_reverse_1(BIF_ALIST_1)
{
    return ets_select_1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_select_reverse_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (IS_HASH_TABLE(tb->common.status)) {
	cret = db_select_hash(BIF_P, &(tb->hash), 
			      BIF_ARG_2, 0 /* chunk size */, &ret);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	cret = db_select_tree(BIF_P, &(tb->tree), 
			      BIF_ARG_2, 1 /* reversed */, &ret);
	/*TT*/
    } else {
	cret = DB_ERROR_UNSPEC;
    }

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    ets_safe_fixtable_2(BIF_P, tb->common.id, am_true);
	} /* Otherwise keep it as is */
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}


/* 
** ets:match_object(Continuation), ets:match_object(Table, Pattern), ets:match_object(Table,Pattern,ChunkSize) 
*/
BIF_RETTYPE ets_match_object_1(BIF_ALIST_1)
{
    return ets_select_1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_match_object_2(BIF_ALIST_2)
{
    Eterm ms;
    Eterm buff[8];
    Eterm *hp = buff;
    /*hp = HAlloc(BIF_P, 8);*/
    ms = CONS(hp, am_DollarUnderscore, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    return ets_select_2(BIF_P, BIF_ARG_1, ms);
}

BIF_RETTYPE ets_match_object_3(BIF_ALIST_3)
{
    Eterm ms;
    Eterm buff[8];
    Eterm *hp = buff;
    /*hp = HAlloc(BIF_P, 8);*/
    ms = CONS(hp, am_DollarUnderscore, NIL);
    hp += 2;
    ms = TUPLE3(hp, BIF_ARG_2, NIL, ms); 
    hp += 4;
    ms = CONS(hp, ms, NIL);
    return ets_select_3(BIF_P, BIF_ARG_1, ms, BIF_ARG_3);
}

/* 
** BIF to extract information about a particular table
** Only the "memory" parameter generates table specific calls.
*/ 

BIF_RETTYPE ets_db_info_2(BIF_ALIST_2)
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
	Uint words = (tb->common.memory_size + sizeof(Uint) - 1)/sizeof(Uint);
	BIF_RET(make_small_or_big(words, BIF_P));
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
    if (BIF_ARG_2 == am_atom_put("kept_objects",12)) 
	BIF_RET(make_small(tb->common.kept_items));
    if (BIF_ARG_2 == am_atom_put("safe_fixed",10)) { 
	if (tb->common.fixations != NULL) {
	    Uint need;
	    Eterm *hp;
	    Eterm tpl, lst;
	    DbFixation *fix;
	    need = 7;
	    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
		need += 5;
	    }
	    hp = HAlloc(BIF_P,need);
	    lst = NIL;
	    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
		tpl = TUPLE2(hp,fix->pid,make_small(fix->counter));
		hp += 3;
		lst = CONS(hp,tpl,lst);
		hp += 2;
	    }
	    tpl = TUPLE3(hp,
			 make_small(tb->common.megasec),
			 make_small(tb->common.sec),
			 make_small(tb->common.microsec));
	    hp += 4;
	    BIF_RET(TUPLE2(hp, tpl, lst));
	} else {
	    BIF_RET(am_false);
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}


BIF_RETTYPE ets_is_compiled_ms_1(BIF_ALIST_1)
{
    if (erts_db_is_compiled_ms(BIF_ARG_1)) {
	BIF_RET(am_true);
    } else {
	BIF_RET(am_false);
    }
}

BIF_RETTYPE ets_match_spec_compile_1(BIF_ALIST_1)
{
    Binary *mp = db_match_set_compile(BIF_P, BIF_ARG_1, DCOMP_TABLE);
    ProcBin *pb;
    if (mp == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    mp->refc++;
    pb = (ProcBin *) HAlloc(BIF_P, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = MSO(BIF_P).mso;
    MSO(BIF_P).mso = pb;
    pb->val = mp;
    pb->bytes = mp->orig_bytes;
    BIF_RET(make_binary(pb));
}
BIF_RETTYPE ets_match_spec_run_r_3(BIF_ALIST_3)
{
    Eterm ret = BIF_ARG_3;
    int i = 0;
    Eterm *hp;
    Eterm lst;
    ProcBin *bp;
    Binary *mp;
    Eterm res;
    Uint32 dummy;
    Uint sz;

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
	goto error;
    }

    if (BIF_ARG_1 == NIL) {
	BIF_RET(BIF_ARG_3);
    }
    for (lst = BIF_ARG_1; is_list(lst); lst = CDR(list_val(lst))) {
	if (++i > CONTEXT_REDS) {
	    BUMP_ALL_REDS(BIF_P);
	    BIF_TRAP3(bif_export[BIF_ets_match_spec_run_r_3],
		      BIF_P,lst,BIF_ARG_2,ret);
	}
	res = db_prog_match(BIF_P, mp, CAR(list_val(lst)), 0, &dummy);
	if (is_value(res)) {
	    sz = size_object(res);
	    hp = HAlloc(BIF_P, sz + 2);
	    res = copy_struct(res, sz, &hp, &MSO(BIF_P));
	    ret = CONS(hp,res,ret);
	    /*hp += 2;*/
	} 
    }
    if (lst != NIL) {
	goto error;
    }
    BIF_RET2(ret,i);
}

#if 0
BIF_RETTYPE ets_match_spec_run_3(BIF_ALIST_3)
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
	    res = copy_struct(res, sz, &hp, &MSO(BIF_P));
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
{
    return ets_match_spec_run_3(BIF_P, BIF_ARG_1, BIF_ARG_2, 0U);
}
#endif
/*
** External interface (NOT BIF's)
*/


/* Init the db */

void init_db(void)
{
    DbTable init_tb;
    int i;
    int max_ets;
    extern Eterm* em_apply_bif;
    Eterm *hp;

    erts_tot_ets_memory_size = 0;
    last_slot = 0;
    db_initialize_util();
    if (( max_ets = (user_requested_db_max_tabs*5)/4 ) < DB_DEF_MAX_TABS)
	db_max_tabs = DB_DEF_MAX_TABS;
    else
	db_max_tabs = next_prime(max_ets);

    db_tables = erts_db_alloc_nt(ERTS_ALC_T_DB_TABLES,
				 sizeof(struct tab_entry)*db_max_tabs);

    no_tabs = 0;
    for (i=0; i<db_max_tabs; i++) {
	db_tables[i].id = DB_NOTUSED;
	db_tables[i].t = NULL;
    }
    db_initialize_hash();
    db_initialize_tree();
    /*TT*/
    /* Create meta table invertion. */
    init_tb.common.memory_size = 0;
    meta_pid_to_tab = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
					       &init_tb,
					       sizeof(DbTable));
    meta_pid_to_tab->common.memory_size = init_tb.common.memory_size;

    meta_pid_to_tab->common.id = NIL;
    meta_pid_to_tab->common.the_name = am_true;
    meta_pid_to_tab->common.status = (DB_NORMAL | DB_BAG | DB_LHASH | 
				      DB_PUBLIC);
    meta_pid_to_tab->common.keypos = 1;
    meta_pid_to_tab->common.owner = NIL;
    meta_pid_to_tab->common.nitems = 0;
    meta_pid_to_tab->common.slot = -1;
    if (db_create_hash(NULL, &(meta_pid_to_tab->hash)) != DB_ERROR_NONE) {
	erl_exit(1,"Unable to create ets metadata tables.");
    }

    init_tb.common.memory_size = 0;
    meta_pid_to_fixed_tab = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
						     &init_tb,
						     sizeof(DbTable));
    meta_pid_to_fixed_tab->common.memory_size = init_tb.common.memory_size;

    meta_pid_to_fixed_tab->common.id = NIL;
    meta_pid_to_fixed_tab->common.the_name = am_true;
    meta_pid_to_fixed_tab->common.status = (DB_NORMAL | DB_BAG | DB_LHASH | 
					    DB_PUBLIC);
    meta_pid_to_fixed_tab->common.keypos = 1;
    meta_pid_to_fixed_tab->common.owner = NIL;
    meta_pid_to_fixed_tab->common.nitems = 0;
    meta_pid_to_fixed_tab->common.slot = -1;
    if (db_create_hash(NULL, &(meta_pid_to_fixed_tab->hash)) 
	!= DB_ERROR_NONE) {
	erl_exit(1,"Unable to create ets metadata tables.");
    }
    /* Non visual BIF to trap to. */
    memset(&ets_select_delete_continue_exp, 0, sizeof(Export));
    ets_select_delete_continue_exp.address = 
	&ets_select_delete_continue_exp.code[3];
    ets_select_delete_continue_exp.code[0] = am_ets;
    ets_select_delete_continue_exp.code[1] = am_atom_put("delete_trap",11);
    ets_select_delete_continue_exp.code[2] = 1;
    ets_select_delete_continue_exp.code[3] =
	(Eterm) em_apply_bif;
    ets_select_delete_continue_exp.code[4] = 
	(Eterm) &ets_select_delete_1;
    /* Non visual BIF to trap to. */
    memset(&ets_select_count_continue_exp, 0, sizeof(Export));
    ets_select_count_continue_exp.address = 
	&ets_select_count_continue_exp.code[3];
    ets_select_count_continue_exp.code[0] = am_ets;
    ets_select_count_continue_exp.code[1] = am_atom_put("count_trap",11);
    ets_select_count_continue_exp.code[2] = 1;
    ets_select_count_continue_exp.code[3] =
	(Eterm) em_apply_bif;
    ets_select_count_continue_exp.code[4] = 
	(Eterm) &ets_select_count_1;
    /* Non visual BIF to trap to. */
    memset(&ets_select_continue_exp, 0, sizeof(Export));
    ets_select_continue_exp.address = 
	&ets_select_continue_exp.code[3];
    ets_select_continue_exp.code[0] = am_ets;
    ets_select_continue_exp.code[1] = am_atom_put("select_trap",11);
    ets_select_continue_exp.code[2] = 1;
    ets_select_continue_exp.code[3] =
	(Eterm) em_apply_bif;
    ets_select_continue_exp.code[4] = 
	(Eterm) &ets_select_trap_1;
    hp = ms_delete_all_buff;
    ms_delete_all = CONS(hp, am_true, NIL);
    hp += 2;
    ms_delete_all = TUPLE3(hp,am_Underscore,NIL,ms_delete_all);
    hp +=4;
    ms_delete_all = CONS(hp, ms_delete_all,NIL);
}

/* Called when  a process which has created any tables dies */
/* So that we can remove the tables ceated by the process   */

#define ARRAY_CHUNK 100
void db_proc_dead(Eterm pid)
{
    Eterm arr[ARRAY_CHUNK];
    int arr_siz;
    int ret;
    Eterm dummy;
    DbFixation *fix,**pp;
    int i;

#ifdef HARDDEBUG
    erl_printf(CERR,"db_proc_dead(");
    erl_printf(CERR,"); Process: ");
    display(pid,CERR);
    erl_printf(CERR,"\r\n");
#endif

    for (;;) {
	arr_siz = ARRAY_CHUNK;
	if ((ret = db_get_element_array(&(meta_pid_to_tab->hash), 
					pid, 2, arr, &arr_siz)) == 
	    DB_ERROR_BADKEY) {
	    /* done */
	    break;
	} else if (ret != DB_ERROR_NONE) {
	    erl_exit(1,"Inconsistent ets metadata");
	}
	for (i = 0; i < arr_siz; ++i) {
	    Sint ix = unsigned_val(arr[i]); /* slot */
	    DbTable* tb = db_tables[ix].t;
	    ASSERT(tb->common.owner == pid);
	    free_table(tb);
	    no_tabs--;
	    meta_mark_free(ix);
	    db_tables[ix].t = NULL;
	    if (arr_siz == ARRAY_CHUNK) {
		/* Need to erase each explicitly */
		db_erase_bag_exact2(&(meta_pid_to_tab->hash),pid,arr[i]);
	    }
	}
	if (arr_siz < ARRAY_CHUNK) {
	    db_erase_hash(NULL,&(meta_pid_to_tab->hash),pid,&dummy);
	} 
    }
    /* And now for the fixations... */
    for (;;) {
	arr_siz = ARRAY_CHUNK;
	if ((ret = db_get_element_array(&(meta_pid_to_fixed_tab->hash), 
					pid, 2, arr, &arr_siz)) == 
	    DB_ERROR_BADKEY) {
	    /* done */
	    return;
	} else if (ret != DB_ERROR_NONE) {
	    erl_exit(1,"Inconsistent ets metadata");
	}
	for (i = 0; i < arr_siz; ++i) {
	    Sint ix = unsigned_val(arr[i]); /* slot */
	    DbTable* tb = db_tables[ix].t;
	    if (tb == NULL) {
		/* Was owner */
		continue;
	    }

	    for (pp = &(tb->common.fixations); *pp != NULL;
		 pp = &((*pp)->next)) {
		if ((*pp)->pid == pid) {
		    fix = *pp;
		    *pp = (*pp)->next;
		    if (arr_siz == ARRAY_CHUNK) {
			db_erase_bag_exact2
			    (&(meta_pid_to_fixed_tab->hash),
			     pid,
			     make_small(tb->common.slot));
		    }
		    erts_db_free(ERTS_ALC_T_DB_FIXATION,
				 tb, (void *) fix, sizeof(DbFixation));
		    break;
		}
	    }
	    if (tb->common.fixations == NULL) {
		if (IS_HASH_TABLE(tb->common.status)) {
		    db_fixtable_hash(&(tb->hash), am_false);
		}	
		tb->common.status &= ~DB_FIXED;
	    }
	}
	if (arr_siz < ARRAY_CHUNK) {
	    db_erase_hash(NULL,&(meta_pid_to_fixed_tab->hash),pid,&dummy);
	} 
    }
}

/*
** Internal functions.
*/
static void meta_mark_free(int idx)
{
    int i;
    if (ISNOTUSED((idx + 1) % db_max_tabs)) {
	db_tables[idx].id = DB_NOTUSED;
	for (i = ((idx > 0) ? idx : db_max_tabs) - 1; 
	     i != idx && db_tables[i].id == DB_USED; 
	     i = ((i > 0) ? i : db_max_tabs) - 1) {
	    db_tables[i].id = DB_NOTUSED;
	}
    } else {
	db_tables[idx].id = DB_USED;
    }
}

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

DbTable* db_get_table(Process *p, Eterm id, int what)
{
    Sint i, j;

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

static void free_fixations(DbTable *tb)
{
    DbFixation *fix;
    DbFixation *next_fix;

    fix = tb->common.fixations;
    while (fix != NULL) {
	next_fix = fix->next;
	db_erase_bag_exact2(&(meta_pid_to_fixed_tab->hash),
			    fix->pid,
			    make_small(tb->common.slot));
	erts_db_free(ERTS_ALC_T_DB_FIXATION,
		     tb, (void *) fix, sizeof(DbFixation));
	fix = next_fix;
    }
    tb->common.fixations = NULL;
}

static void free_table(DbTable *tb)
{
    free_fixations(tb);

    if (IS_HASH_TABLE(tb->common.status))
	free_hash_table(&(tb->hash));
    else if (IS_TREE_TABLE(tb->common.status))
	free_tree_table(&(tb->tree));
    /*TT*/
    else
	erl_exit(1,"Panic: Unknown table type (status word = 0x%08x)!",
		 tb->common.status);

    erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
}

static BIF_RETTYPE free_table_cont(Process* p, DbTable *tb, int first)
{
    Eterm result;

    if (IS_HASH_TABLE(tb->common.status)) {
	result = erts_free_hash_table_cont(&(tb->hash), first);
    } else if (IS_TREE_TABLE(tb->common.status)) {
	result = erts_free_tree_table_cont(&(tb->tree), first);
    } else {
    /*TT*/
	erl_exit(1,"Panic: Unknown table type (status word = 0x%08x)!",
		 tb->common.status);
    }

    if (result == 0) {
	/* More work to be done. Let other processes work and call us again. */
	BUMP_ALL_REDS(p);
	BIF_TRAP1(bif_export[BIF_ets_db_delete_1], p, (Eterm) tb);
    } else {
	/* Completely done - we will not get called again. */
	meta_mark_free(tb->common.slot);
	db_tables[tb->common.slot].t = NULL;
	no_tabs--;
	db_erase_bag_exact2(&(meta_pid_to_tab->hash),tb->common.owner,
			    make_small(tb->common.slot));
	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
	BUMP_REDS(p, tb->common.nitems/15);
	return am_true;
    }
}

static void print_table(CIO fd, int show,  DbTable* tb)
{
    erl_printf(fd, "Table: "); display(tb->common.id, fd); erl_printf(fd, "\n");
    erl_printf(fd, "Name: " ); display(tb->common.the_name, fd); erl_printf(fd, "\n");

    if (IS_HASH_TABLE(tb->common.status)) {
	db_print_hash(fd, show, &(tb->hash));
    } else if (IS_TREE_TABLE(tb->common.status)) {
	db_print_tree(fd, show, &(tb->tree));
	/*TT*/
    } else {
	erl_printf(fd,"Table is of unknown type!\n");
    }
    erl_printf(fd, "Objects: %d\n", tb->common.nitems);
    erl_printf(fd, "Words: %d\n",
	       (tb->common.memory_size + sizeof(Uint) - 1)/sizeof(Uint));
}

void db_info(CIO fd, int show)    /* Called by break handler */
{
    int i;
    for (i=0; i < db_max_tabs; i++) 
	if (!ISFREE(i)) {
	    erl_printf(fd, "=ets:");
	    display(db_tables[i].t->common.owner, fd);
	    erl_printf(fd, "\n");
	    erl_printf(fd, "Slot: %d\n", i);
	    print_table(fd, show, db_tables[i].t);
	}
#ifdef DEBUG
    erl_printf(fd, "=internal_ets: Process to table index\n");
    print_table(fd, show, meta_pid_to_tab);
    erl_printf(fd, "=internal_ets: Process to fixation index\n");
    print_table(fd, show, meta_pid_to_fixed_tab);
#endif
}

void
erts_db_foreach_table(void (*func)(DbTable *, void *), void *arg)
{
    int i, j;
    j = 0;
    for(i = 0; (i < db_max_tabs && j < no_tabs); i++) {
	if (!ISFREE(i)) {
	    j++;
	    (*func)(db_tables[i].t, arg);
	}
    }
    ASSERT(j == no_tabs);
}

void
erts_db_foreach_offheap(DbTable *tab,
			void (*func)(ErlOffHeap *, void *),
			void *arg)
{
    if (IS_HASH_TABLE(tab->common.status)) {
	erts_db_hash_foreach_offheap(&(tab->hash), func, arg);
    }
    else if (IS_TREE_TABLE(tab->common.status)) {
	erts_db_tree_foreach_offheap(&(tab->tree), func, arg);
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
