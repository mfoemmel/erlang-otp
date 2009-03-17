/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

/*
 * This file contains the bif interface functions and
 * the handling of the "meta tables" ie the tables of 
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


erts_smp_atomic_t erts_ets_misc_mem_size;

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
** The main meta table, containing all ets tables.
*/
#ifdef ERTS_SMP
#  define META_MAIN_TAB_LOCK_CNT 16
static union {
    erts_smp_spinlock_t lck;
    byte _cache_line_alignment[64];
}meta_main_tab_locks[META_MAIN_TAB_LOCK_CNT];
#endif
static struct {
    union {
	DbTable *tb;     /* Only directly readable if slot is ALIVE */
	Uint next_free;  /* (index<<2)|1 if slot is FREE */
    }u;
} *meta_main_tab;

/* A slot in meta_main_tab can have three states:
 * FREE : Free to use for new table. Part of linked free-list.
 * ALIVE: Contains a table
 * DEAD : Contains a table that is being removed.
 */
#define IS_SLOT_FREE(i)	(meta_main_tab[(i)].u.next_free & 1)
#define IS_SLOT_DEAD(i) (meta_main_tab[(i)].u.next_free & 2)
#define IS_SLOT_ALIVE(i) (!(meta_main_tab[(i)].u.next_free & (1|2)))
#define GET_NEXT_FREE_SLOT(i) (meta_main_tab[(i)].u.next_free >> 2)
#define SET_NEXT_FREE_SLOT(i,next) (meta_main_tab[(i)].u.next_free = ((next)<<2)|1)
#define MARK_SLOT_DEAD(i) (meta_main_tab[(i)].u.next_free |= 2)
#define GET_ANY_SLOT_TAB(i) ((DbTable*)(meta_main_tab[(i)].u.next_free & ~(1|2))) /* dead or alive */

static ERTS_INLINE void meta_main_tab_lock(unsigned slot)
{
#ifdef ERTS_SMP
    erts_smp_spin_lock(&meta_main_tab_locks[slot % META_MAIN_TAB_LOCK_CNT].lck);
#endif
}

static ERTS_INLINE void meta_main_tab_unlock(unsigned slot)
{
#ifdef ERTS_SMP
    erts_smp_spin_unlock(&meta_main_tab_locks[slot % META_MAIN_TAB_LOCK_CNT].lck);
#endif
}

static erts_smp_spinlock_t meta_main_tab_main_lock;
static Uint meta_main_tab_first_free;   /* Index of first free slot */
static int meta_main_tab_cnt;		/* Number of active tables */
static Uint meta_main_tab_slot_mask;    /* The slot index part of an unnamed table id */
static Uint meta_main_tab_seq_incr;
static Uint meta_main_tab_seq_cnt = 0;  /* To give unique(-ish) table identifiers */



/* 
** The meta hash table of all NAMED ets tables
*/
#ifdef ERTS_SMP
#  define META_NAME_TAB_LOCK_CNT 16
union {
    erts_smp_rwmtx_t lck;
    byte _cache_line_alignment[64];
}meta_name_tab_rwlocks[META_NAME_TAB_LOCK_CNT];
#endif
static struct meta_name_tab_entry {
    union {
	Eterm name_atom;
	Eterm mcnt; /* Length of mvec in multiple tab entry */
    }u;
    union {
	DbTable *tb;
	struct meta_name_tab_entry* mvec;
    }pu;
} *meta_name_tab;

static unsigned meta_name_tab_mask;

static ERTS_INLINE
struct meta_name_tab_entry* meta_name_tab_bucket(Eterm name, 
						 erts_smp_rwmtx_t** lockp)
{
    unsigned bix = atom_val(name) & meta_name_tab_mask;
    struct meta_name_tab_entry* bucket = &meta_name_tab[bix];
#ifdef ERTS_SMP
    *lockp = &meta_name_tab_rwlocks[bix % META_NAME_TAB_LOCK_CNT].lck;
#endif
    return bucket;
}    


typedef enum { LCK_READ=1, LCK_WRITE=2 } db_lock_kind_t;

extern DbTableMethod db_hash;
extern DbTableMethod db_tree;

int user_requested_db_max_tabs;
int erts_ets_realloc_always_moves;
static int db_max_tabs;
static DbTable *meta_pid_to_tab; /* Pid mapped to owned tables */
static DbTable *meta_pid_to_fixed_tab; /* Pid mapped to fixed tables */
static Eterm ms_delete_all;
static Eterm ms_delete_all_buff[8]; /* To compare with for deletion 
				       of all objects */

/* 
** Forward decls, static functions 
*/

static void fix_table_locked(Process* p, DbTable* tb);
static void unfix_table_locked(Process* p,  DbTable* tb);
static void free_fixations_locked(DbTable *tb);

static int free_table_cont(Process *p,
			   DbTable *tb,
			   int first,
			   int clean_meta_tab);
static void print_table(int to, void *to_arg, int show,  DbTable* tb);
static BIF_RETTYPE ets_select_delete_1(Process *p, Eterm a1);
static BIF_RETTYPE ets_select_count_1(Process *p, Eterm a1);
static BIF_RETTYPE ets_select_trap_1(Process *p, Eterm a1);
static BIF_RETTYPE ets_delete_trap(Process *p, Eterm a1);
static Eterm table_info(Process* p, DbTable* tb, Eterm What);

/* 
 * Exported global
 */
Export ets_select_delete_continue_exp;
Export ets_select_count_continue_exp;
Export ets_select_continue_exp;

/*
 * Static traps
 */
static Export ets_delete_continue_exp;

static ERTS_INLINE DbTable* db_ref(DbTable* tb)
{
    if (tb != NULL)
	erts_refc_inc(&tb->common.ref, 2);
    return tb;
}

static ERTS_INLINE DbTable* db_unref(DbTable* tb)
{
    if (!erts_refc_dectest(&tb->common.ref, 0)) {
#ifdef HARDDEBUG
	if (erts_smp_atomic_read(&tb->common.memory_size) != sizeof(DbTable)) {
	    erts_fprintf(stderr, "ets: db_unref memory remain=%ld fix=%x\n",
			 erts_smp_atomic_read(&tb->common.memory_size)-sizeof(DbTable), 
			 tb->common.fixations);
	}
	erts_fprintf(stderr, "ets: db_unref(%T) deleted!!!\r\n", 
		     tb->common.id);

	erts_fprintf(stderr, "ets: db_unref: meta_pid_to_tab common.memory_size = %ld\n",
		     erts_smp_atomic_read(&meta_pid_to_tab->common.memory_size));
	print_table(ERTS_PRINT_STDOUT, NULL, 1, meta_pid_to_tab);


	erts_fprintf(stderr, "ets: db_unref: meta_pid_to_fixed_tab common.memory_size = %ld\n",
		     erts_smp_atomic_read(&meta_pid_to_fixed_tab->common.memory_size));
	print_table(ERTS_PRINT_STDOUT, NULL, 1, meta_pid_to_fixed_tab);
	
#endif
#ifdef ERTS_SMP
	erts_smp_rwmtx_destroy(&tb->common.rwlock);
#endif
	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(DbTable));
	return NULL;
    }
    return tb;
}

static ERTS_INLINE void db_init_lock(DbTable* tb, char *name)
{
    erts_refc_init(&tb->common.ref, 1);
#ifdef ERTS_SMP
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_smp_rwmtx_init_x(&tb->common.rwlock, name, tb->common.the_name);
#else
    erts_smp_rwmtx_init(&tb->common.rwlock, name);
#endif
#endif
}

static ERTS_INLINE void db_lock_take_over_ref(DbTable* tb, db_lock_kind_t kind)
{
#ifdef ERTS_SMP
    if (kind == LCK_WRITE)
	erts_smp_rwmtx_rwlock(&tb->common.rwlock);
    else
	erts_smp_rwmtx_rlock(&tb->common.rwlock);
#endif
}

static ERTS_INLINE void db_lock(DbTable* tb, db_lock_kind_t kind)
{
    (void) db_ref(tb);
#ifdef ERTS_SMP
    db_lock_take_over_ref(tb, kind);
#endif
}

static ERTS_INLINE void db_unlock(DbTable* tb, db_lock_kind_t kind)
{
#ifdef ERTS_SMP
    if (kind == LCK_WRITE)
	erts_smp_rwmtx_rwunlock(&tb->common.rwlock);
    else
	erts_smp_rwmtx_runlock(&tb->common.rwlock);
#endif
    (void) db_unref(tb); /* May delete table... */
}
    
static ERTS_INLINE
DbTable* db_get_table_aux(Process *p,
			  Eterm id,
			  int what,
			  db_lock_kind_t kind,
			  db_lock_kind_t (*get_kind)(DbTable *))
{
    DbTable *tb = NULL;

    if (is_small(id)) {
	Uint slot = unsigned_val(id) & meta_main_tab_slot_mask;
	meta_main_tab_lock(slot);
	if (slot < db_max_tabs && IS_SLOT_ALIVE(slot)) {
	    /* SMP: inc to prevent race, between unlock of meta_main_tab_lock
	     * and the table locking outside the meta_main_tab_lock
	     */
	    tb = db_ref(meta_main_tab[slot].u.tb);
	}
	meta_main_tab_unlock(slot);
    }
    else if (is_atom(id)) {
	erts_smp_rwmtx_t* rwlock;
	struct meta_name_tab_entry* bucket = meta_name_tab_bucket(id,&rwlock);
	erts_smp_rwmtx_rlock(rwlock);
	if (bucket->pu.tb != NULL) {
	    if (is_atom(bucket->u.name_atom)) { /* single */
		if (bucket->u.name_atom == id) {
		    tb = db_ref(bucket->pu.tb);
		}
	    }
	    else { /* multi */
		Uint cnt = unsigned_val(bucket->u.mcnt);
		Uint i;
		for (i=0; i<cnt; i++) {
		    if (bucket->pu.mvec[i].u.name_atom == id) {
			tb = db_ref(bucket->pu.mvec[i].pu.tb);
			break;
		    }
		}
	    }
	}
	erts_smp_rwmtx_runlock(rwlock);
    }
    if (tb) {
#ifdef ERTS_SMP
	    if (get_kind)
		kind = (*get_kind)(tb);
#endif
	    db_lock_take_over_ref(tb, kind);
	if (tb->common.id == id && ((tb->common.status & what) != 0 || 
				    p->id == tb->common.owner)) {
		return tb;
	    }
	    db_unlock(tb, kind);
    }
	    return NULL;
	}

static ERTS_INLINE DbTable* db_get_table(Process *p,
			     Eterm id,
			     int what,
			     db_lock_kind_t kind)
{
    return db_get_table_aux(p, id, what, kind, NULL);
}

static ERTS_INLINE DbTable* db_get_table2(Process *p,
			      Eterm id,
			      int what,
			      db_lock_kind_t (*get_kind)(DbTable *))
{
    return db_get_table_aux(p, id, what, LCK_WRITE, get_kind);
}

/* Requires meta_main_tab_locks[slot] locked.
*/
static ERTS_INLINE void free_slot(int slot)
{
    ASSERT(!IS_SLOT_FREE(slot));
    erts_smp_spin_lock(&meta_main_tab_main_lock);
    SET_NEXT_FREE_SLOT(slot,meta_main_tab_first_free);
    meta_main_tab_first_free = slot;
    meta_main_tab_cnt--;
    erts_smp_spin_unlock(&meta_main_tab_main_lock);
}

static int insert_named_tab(Eterm name_atom, DbTable* tb)
{
    int ret = 0;
    erts_smp_rwmtx_t* rwlock;
    struct meta_name_tab_entry* new_entry;
    struct meta_name_tab_entry* bucket = meta_name_tab_bucket(name_atom,
							      &rwlock);

    erts_smp_rwmtx_rwlock(rwlock);

    if (bucket->pu.tb == NULL) { /* empty */
	new_entry = bucket;
    }
    else {
	struct meta_name_tab_entry* entries;
	Uint cnt;
	if (is_atom(bucket->u.name_atom)) { /* single */
	    size_t size;
	    if (bucket->u.name_atom == name_atom) {
		goto done;
	    }
	    cnt = 2;
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    entries = erts_db_alloc_nt(ERTS_ALC_T_DB_NTAB_ENT, size);
	    ERTS_ETS_MISC_MEM_ADD(size);
	    new_entry = &entries[0];
	    entries[1] = *bucket;
	}
	else { /* multi */
	    size_t size, old_size;
	    Uint i;
	    cnt = unsigned_val(bucket->u.mcnt);
	    for (i=0; i<cnt; i++) {
		if (bucket->pu.mvec[i].u.name_atom == name_atom) {
		    goto done;
		}
	    }
	    old_size = sizeof(struct meta_name_tab_entry)*cnt;
	    size = sizeof(struct meta_name_tab_entry)*(cnt+1);
	    entries = erts_db_realloc_nt(ERTS_ALC_T_DB_NTAB_ENT,
					 bucket->pu.mvec,
					 old_size,
					 size);
	    ERTS_ETS_MISC_MEM_ADD(size-old_size);
	    new_entry = &entries[cnt];
	    cnt++;
	}
	bucket->pu.mvec = entries;
	bucket->u.mcnt = make_small(cnt);
    }
    new_entry->pu.tb = tb;
    new_entry->u.name_atom = name_atom;
    ret = 1; /* Ok */

done:
    erts_smp_rwmtx_rwunlock(rwlock);
    return ret;
}

static int remove_named_tab(Eterm name_atom)
{
    int ret = 0;
    erts_smp_rwmtx_t* rwlock;
    struct meta_name_tab_entry* bucket = meta_name_tab_bucket(name_atom,
							      &rwlock);
    erts_smp_rwmtx_rwlock(rwlock);
    if (bucket->pu.tb == NULL) {
	goto done;
    }
    else if (is_atom(bucket->u.name_atom)) { /* single */
	if (bucket->u.name_atom != name_atom) {
	    goto done;
	}
	bucket->pu.tb = NULL;
    }
    else { /* multi */
	Uint cnt = unsigned_val(bucket->u.mcnt);
	Uint i = 0;
	for (;;) {
	    if (bucket->pu.mvec[i].u.name_atom == name_atom) {
		break;
	    }
	    if (++i >= cnt) {
		goto done;
	    }
	}
	if (cnt == 2) { /* multi -> single */
	    size_t size;
	    struct meta_name_tab_entry* entries = bucket->pu.mvec;
	    *bucket = entries[1-i];
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    erts_db_free_nt(ERTS_ALC_T_DB_NTAB_ENT, entries, size);
	    ERTS_ETS_MISC_MEM_ADD(-size);
	    ASSERT(is_atom(bucket->u.name_atom));
	}
	else {
	    size_t size, old_size;
	    ASSERT(cnt > 2);
	    bucket->u.mcnt = make_small(--cnt);
	    if (i != cnt) {
		/* reposition last one before realloc destroys it */
		bucket->pu.mvec[i] = bucket->pu.mvec[cnt];
	    }
	    old_size = sizeof(struct meta_name_tab_entry)*(cnt+1);
	    size = sizeof(struct meta_name_tab_entry)*cnt;
	    bucket->pu.mvec = erts_db_realloc_nt(ERTS_ALC_T_DB_NTAB_ENT,
						 bucket->pu.mvec,
						 old_size,
						 size);
	    ERTS_ETS_MISC_MEM_ADD(size - old_size);
    
	}
    }
    ret = 1; /* Ok */

done:
    erts_smp_rwmtx_rwunlock(rwlock);
    return ret;
}


/*
 * BIFs.
 */

BIF_RETTYPE ets_safe_fixtable_2(BIF_ALIST_2)
{
    DbTable *tb;

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:safe_fixtable(%T,%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_ARG_2, BIF_P->id,
		BIF_P->initial[0], BIF_P->initial[1], BIF_P->initial[2]);
#endif
    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_ARG_2 == am_true) {
	fix_table_locked(BIF_P, tb);
    }
    else if (BIF_ARG_2 == am_false) {
	if (tb->common.status & DB_FIXED) {
	    unfix_table_locked(BIF_P, tb);
	}
    }
    else {
	db_unlock(tb, LCK_WRITE);
	BIF_ERROR(BIF_P, BADARG);
    }
    db_unlock(tb, LCK_WRITE);
    BIF_RET(am_true);
}

#ifdef ERTS_SMP
#define STEP_LOCK_TYPE(T) \
  (IS_TREE_TABLE((T)->common.type) ? LCK_WRITE : LCK_READ)
#else
#define STEP_LOCK_TYPE(T) LCK_WRITE
#endif

static db_lock_kind_t
step_lock_type(DbTable *tb)
{
    return STEP_LOCK_TYPE(tb);
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

    tb = db_get_table2(BIF_P, BIF_ARG_1, DB_READ, step_lock_type);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_first(BIF_P, tb, &ret);

    db_unlock(tb, STEP_LOCK_TYPE(tb));

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

    tb = db_get_table2(BIF_P, BIF_ARG_1, DB_READ, step_lock_type);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_next(BIF_P, tb, BIF_ARG_2, &ret);

    db_unlock(tb, STEP_LOCK_TYPE(tb));

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** Returns the last Key in a table 
*/
BIF_RETTYPE ets_last_1(BIF_ALIST_1)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table2(BIF_P, BIF_ARG_1, DB_READ, step_lock_type);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_last(BIF_P, tb, &ret);

    db_unlock(tb, STEP_LOCK_TYPE(tb));

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** The prev BIF, given a key, return the "previous" key 
*/
BIF_RETTYPE ets_prev_2(BIF_ALIST_2)
{
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();

    tb = db_get_table2(BIF_P, BIF_ARG_1, DB_READ, step_lock_type);

    if (!tb) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_prev(BIF_P,tb,BIF_ARG_2,&ret);

    db_unlock(tb, STEP_LOCK_TYPE(tb));

    if (cret != DB_ERROR_NONE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
}

/* 
** update_element(Tab, Key, {Pos, Value})
** update_element(Tab, Key, [{Pos, Value}])
*/
BIF_RETTYPE ets_update_element_3(BIF_ALIST_3)
{
    DbTable* tb;
    int cret = DB_ERROR_BADITEM;
    Eterm list;
    Eterm iter;
    Eterm cell[2];
    DbUpdateHandle handle;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) {
	goto bail_out;
    }
    if (is_tuple(BIF_ARG_3)) {
	list = CONS(cell, BIF_ARG_3, NIL);
    }
    else {
	list = BIF_ARG_3;
    }

    if (!tb->common.meth->db_lookup_dbterm(tb, BIF_ARG_2, &handle)) {
	cret = DB_ERROR_BADKEY;
	goto bail_out;
    }

    /* First verify that list is ok to avoid nasty rollback scenarios
    */
    for (iter=list ; is_not_nil(iter); iter = CDR(list_val(iter))) {
	Eterm pv;
	Eterm* pvp;
	Sint position;

	if (is_not_list(iter)) {
	    goto bail_out;
	}
	pv = CAR(list_val(iter));    /* {Pos,Value} */
	if (is_not_tuple(pv)) {
	    goto bail_out;
	}
	pvp = tuple_val(pv);
	if (arityval(*pvp) != 2 || !is_small(pvp[1])) {
	    goto bail_out;
	}
	position = signed_val(pvp[1]);
	if (position < 1 || position == tb->common.keypos || 
	    position > arityval(handle.dbterm->tpl[0])) {
	    goto bail_out;
	}	
    }
    /* The point of no return, no failures from here on.
    */
    cret = DB_ERROR_NONE;

    for (iter=list ; is_not_nil(iter); iter = CDR(list_val(iter))) {
	Eterm* pvp = tuple_val(CAR(list_val(iter)));    /* {Pos,Value} */
	db_do_update_element(&handle, signed_val(pvp[1]), pvp[2]);
    }

    if (handle.mustFinalize) {
	db_finalize_update_element(&handle);
    }

bail_out:
    db_unlock(tb, LCK_WRITE);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(am_true);
    case DB_ERROR_BADKEY:
	BIF_RET(am_false);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
	break;
    }
}

/* 
** update_counter(Tab, Key, Incr) 
** update_counter(Tab, Key, {Upop}) 
** update_counter(Tab, Key, [{Upop}]) 
** Upop = {Pos,Incr} | {Pos,Incr,Threshold,WarpTo}
** Returns new value(s) (integer or [integer])
*/
BIF_RETTYPE ets_update_counter_3(BIF_ALIST_3)
{
    DbTable* tb;
    int cret = DB_ERROR_BADITEM;
    Eterm upop_list;
    int list_size;
    Eterm ret;  /* int or [int] */
    Eterm* ret_list_currp = NULL;
    Eterm* ret_list_prevp = NULL;
    Eterm iter;
    Eterm cell[2];
    Eterm tuple[3];
    DbUpdateHandle handle;
    Uint halloc_size = 0; /* overestimated heap usage */
    Eterm* htop;          /* actual heap usage */
    Eterm* hstart;
    Eterm* hend;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (!(tb->common.status & (DB_SET | DB_ORDERED_SET))) {
	goto bail_out;
    }
    if (is_integer(BIF_ARG_3)) {  /* Incr */
	upop_list = CONS(cell, TUPLE2(tuple, make_small(tb->common.keypos+1),
				      BIF_ARG_3), NIL);
    }
    else if (is_tuple(BIF_ARG_3)) { /* {Upop} */
	upop_list = CONS(cell, BIF_ARG_3, NIL);
    }
    else { /* [{Upop}] (probably) */
	upop_list = BIF_ARG_3;
	ret_list_prevp = &ret;
    }

    if (!tb->common.meth->db_lookup_dbterm(tb, BIF_ARG_2, &handle)) {
	goto bail_out; /* key not found */
    }

    /* First verify that list is ok to avoid nasty rollback scenarios
    */
    list_size = 0;
    for (iter=upop_list ; is_not_nil(iter); iter = CDR(list_val(iter)),
	                                    list_size += 2) {
	Eterm upop;
	Eterm* tpl;
	Sint position;
	Eterm incr, warp, oldcnt;

	if (is_not_list(iter)) {
	    goto bail_out;
	}
	upop = CAR(list_val(iter));
	if (is_not_tuple(upop)) {
	    goto bail_out;
	}
	tpl = tuple_val(upop);
	switch (arityval(*tpl)) {
	case 4: /* threshold specified */
	    if (is_not_integer(tpl[3])) {
		goto bail_out;
	    }
	    warp = tpl[4];
	    if (is_big(warp)) {
		halloc_size += BIG_NEED_SIZE(big_arity(warp));
	    }
	    else if (is_not_small(warp)) {
		goto bail_out;
	    }
	    /* Fall through */
	case 2:
	    if (!is_small(tpl[1])) {
		goto bail_out;
	    }
	    incr = tpl[2];
	    if (is_big(incr)) {
		halloc_size += BIG_NEED_SIZE(big_arity(incr));
	    }
	    else if (is_not_small(incr)) {
		goto bail_out;
	    }
	    position = signed_val(tpl[1]);
	    if (position < 1 || position == tb->common.keypos ||
		position > arityval(handle.dbterm->tpl[0])) {
		goto bail_out;
	    }
	    oldcnt = handle.dbterm->tpl[position];
	    if (is_big(oldcnt)) {
		halloc_size += BIG_NEED_SIZE(big_arity(oldcnt));
	    }
	    else if (is_not_small(oldcnt)) {
		goto bail_out;
	    }
	    break;
	default:
	    goto bail_out;
	}
	halloc_size += 2;  /* worst growth case: small(0)+small(0)=big(2) */
    }

    /* The point of no return, no failures from here on.
    */
    cret = DB_ERROR_NONE;

    if (ret_list_prevp) { /* Prepare to return a list */
	ret = NIL;
	halloc_size += list_size;
	hstart = HAlloc(BIF_P, halloc_size);
	ret_list_currp = hstart;
	htop = hstart + list_size;
	hend = hstart + halloc_size;
    }
    else {
	hstart = htop = HAlloc(BIF_P, halloc_size);
    }
    hend = hstart + halloc_size;

    for (iter=upop_list ; is_not_nil(iter); iter = CDR(list_val(iter))) {

	Eterm* tpl = tuple_val(CAR(list_val(iter)));
	Sint position = signed_val(tpl[1]);
	Eterm incr = tpl[2];
	Eterm oldcnt = handle.dbterm->tpl[position];
	Eterm newcnt = db_add_counter(&htop, oldcnt, incr);

	if (newcnt == NIL) {
	    cret = DB_ERROR_SYSRES; /* Can only happen if BIG_ARITY_MAX */
	    ret = NIL;              /* is reached, ie should not happen */
	    htop = hstart;
	    break;
	}
	ASSERT(is_integer(newcnt));

	if (arityval(*tpl) == 4) { /* Maybe warp it */
	    Eterm threshold = tpl[3];
	    if ((cmp(incr,make_small(0)) < 0) ? /* negative increment? */
		(cmp(newcnt,threshold) < 0) :  /* if negative, check if below */
		(cmp(newcnt,threshold) > 0)) { /* else check if above threshold */

		newcnt = tpl[4];
	    }
	}

	db_do_update_element(&handle,position,newcnt);

	if (ret_list_prevp) {
	    *ret_list_prevp = CONS(ret_list_currp,newcnt,NIL);
	    ret_list_prevp = &CDR(ret_list_currp);
	    ret_list_currp += 2;
	}
	else {
	    ret = newcnt;
	    break;	    
	}
    }
    if (handle.mustFinalize) {
	db_finalize_update_element(&handle);
    }

    ASSERT(is_integer(ret) || is_nil(ret) || 
	   (is_list(ret) && (list_val(ret)+list_size)==ret_list_currp));
    ASSERT(htop <= hend);

    HRelease(BIF_P,hend,htop);

bail_out:
    db_unlock(tb, LCK_WRITE);

    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
	break;
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
    DbTableMethod* meth;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	db_unlock(tb, LCK_WRITE);
	BIF_RET(am_true);
    }
    meth = tb->common.meth;
    if (is_list(BIF_ARG_2)) {
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    if (is_not_tuple(CAR(list_val(lst))) || 
		(arityval(*tuple_val(CAR(list_val(lst)))) < tb->common.keypos)) {
		goto badarg;
	    }
	}
	if (lst != NIL) {
	    goto badarg;
	}
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    cret = meth->db_put(BIF_P, tb, CAR(list_val(lst)), &ret);
	    if (cret != DB_ERROR_NONE)
		break;
	}
    } else {
	if (is_not_tuple(BIF_ARG_2) || 
	    (arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	    goto badarg;
	}
	cret = meth->db_put(BIF_P, tb, BIF_ARG_2, &ret);
    }

    db_unlock(tb, LCK_WRITE);
    
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
 badarg:
    db_unlock(tb, LCK_WRITE);
    BIF_ERROR(BIF_P, BADARG);    
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
    DbTableMethod* meth;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (BIF_ARG_2 == NIL) {
	db_unlock(tb, LCK_WRITE);
	BIF_RET(am_true);
    }
    meth = tb->common.meth;
    if (is_list(BIF_ARG_2)) {
	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    if (is_not_tuple(CAR(list_val(lst))) || 
		(arityval(*tuple_val(CAR(list_val(lst)))) < tb->common.keypos)) {
		goto badarg;
	    }
	}
	if (lst != NIL) {
	    goto badarg;
	}

	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    cret = meth->db_member(BIF_P, tb,
				   TERM_GETKEY(tb,CAR(list_val(lst))),
				   &lookup_ret);
	    if ((cret != DB_ERROR_NONE) || (lookup_ret != am_false)) {
		ret = am_false;
		goto done;
	    }
	}

	for (lst = BIF_ARG_2; is_list(lst); lst = CDR(list_val(lst))) {
	    cret = meth->db_put(BIF_P, tb,CAR(list_val(lst)),&ret);
	    if (cret != DB_ERROR_NONE)
		break;
	}
    } else {
	if (is_not_tuple(BIF_ARG_2) || 
	    (arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	    goto badarg;
	}
	cret = meth->db_member(BIF_P, tb,TERM_GETKEY(tb,BIF_ARG_2),
			       &lookup_ret);
	if ((cret != DB_ERROR_NONE) || (lookup_ret != am_false)) 
	    ret = am_false;
	else {
	    cret = meth->db_put(BIF_P,tb,BIF_ARG_2, &ret);
	}
    }

done:
    db_unlock(tb, LCK_WRITE);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
 badarg:
    db_unlock(tb, LCK_WRITE);
    BIF_ERROR(BIF_P, BADARG);    
}

/*
** Rename a (possibly) named table
*/

BIF_RETTYPE ets_rename_2(BIF_ALIST_2)
{
    DbTable* tb;
    Eterm ret;

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:rename(%T,%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_ARG_2, BIF_P->id,
		BIF_P->initial[0], BIF_P->initial[1], BIF_P->initial[2]);
#endif

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_atom(BIF_ARG_2)) {
	goto badarg;
    }

    if (is_not_atom(tb->common.id)) { /* Not a named table */
	tb->common.the_name = BIF_ARG_2;
	goto done;
    }

    if (!insert_named_tab(BIF_ARG_2,tb)) {
	goto badarg;
    }
    if (!remove_named_tab(tb->common.id)) {
	erl_exit(1,"Could not find named tab %s", tb->common.id);
    }

    tb->common.id = tb->common.the_name = BIF_ARG_2;

 done:
    ret = tb->common.id;
    db_unlock(tb, LCK_WRITE);
    BIF_RET(ret);
 badarg:
    db_unlock(tb, LCK_WRITE);
    BIF_ERROR(BIF_P, BADARG);    
}


/* 
** The create table BIF     
** Args: (Name, Properties) 
*/

BIF_RETTYPE ets_new_2(BIF_ALIST_2)
{
    DbTable* tb = NULL;
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
    DbTableMethod* meth;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_nil(BIF_ARG_2) && is_not_list(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
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

    /* we create table outside any table lock
     * and take the unusal cost of destroy table if it
     * fails to find a slot 
     */
    {
        DbTable init_tb;

	erts_smp_atomic_init(&init_tb.common.memory_size, 0);
	tb = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
				      &init_tb,
				      sizeof(DbTable));
	ERTS_ETS_MISC_MEM_ADD(sizeof(DbTable));
	erts_smp_atomic_init(&tb->common.memory_size,
			     erts_smp_atomic_read(&init_tb.common.memory_size));
    }

    if (IS_HASH_TABLE(status))
	meth = &db_hash;
    else if (IS_TREE_TABLE(status))
	meth = &db_tree;
    else
	meth = NULL;

    tb->common.meth = meth;


    tb->common.the_name = BIF_ARG_1;
    tb->common.status = status;
    
    db_init_lock(tb, "db_tab");
#ifdef ERTS_SMP
    tb->common.type = status & ERTS_ETS_TABLE_TYPES;
    /* Note, 'type' is *read only* from now on... */
#endif
    tb->common.keypos = keypos;
    tb->common.owner = BIF_P->id;

    tb->common.nitems = 0;
    tb->common.kept_items = 0;

    tb->common.fixations = NULL;

    if (meth == NULL) 
	cret = DB_ERROR_UNSPEC;
    else
	cret = meth->db_create(BIF_P, tb);
    if (cret != DB_ERROR_NONE) {
	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(DbTable));
	BIF_ERROR(BIF_P, BADARG);
    }

    erts_smp_spin_lock(&meta_main_tab_main_lock);

    if (meta_main_tab_cnt >= db_max_tabs) {
	erts_smp_spin_unlock(&meta_main_tab_main_lock);
	erts_send_error_to_logger_str(BIF_P->group_leader,
				      "** Too many db tables **\n");
	tb->common.meth->db_free_table(tb);
	erts_db_free(ERTS_ALC_T_DB_TABLE, tb, (void *) tb, sizeof(DbTable));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(DbTable));
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }

    slot = meta_main_tab_first_free;
    ASSERT(slot>=0 && slot<db_max_tabs);
    meta_main_tab_first_free = GET_NEXT_FREE_SLOT(slot);
    meta_main_tab_cnt++;

    if (is_named) {
		ret = BIF_ARG_1;
    }
    else {
	ret = make_small(slot | meta_main_tab_seq_cnt);
	meta_main_tab_seq_cnt += meta_main_tab_seq_incr;
	ASSERT((unsigned_val(ret) & meta_main_tab_slot_mask) == slot);
    }
    erts_smp_spin_unlock(&meta_main_tab_main_lock);

    tb->common.id = ret;
    tb->common.slot = slot;           /* store slot for erase */

    meta_main_tab_lock(slot);
    meta_main_tab[slot].u.tb = tb;
    ASSERT(IS_SLOT_ALIVE(slot));
    meta_main_tab_unlock(slot);

    if (is_named && !insert_named_tab(BIF_ARG_1, tb)) {
	meta_main_tab_lock(slot);
	free_slot(slot);
	meta_main_tab_unlock(slot);

	db_lock_take_over_ref(tb,LCK_WRITE);
	tb->common.meth->db_free_table(tb);
	db_unlock(tb,LCK_WRITE);
	BIF_ERROR(BIF_P, BADARG);
    }
    
    BIF_P->flags |= F_USING_DB; /* So we can remove tb if p dies */

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:new(%T,%T)=%T; Process: %T, initial: %T:%T/%bpu\n",
		 BIF_ARG_1, BIF_ARG_2, ret, BIF_P->id,
		 BIF_P->initial[0], BIF_P->initial[1], BIF_P->initial[2]);
	erts_fprintf(stderr, "ets: new: meta_pid_to_tab common.memory_size = %ld\n",
		     erts_smp_atomic_read(&meta_pid_to_tab->common.memory_size));
	erts_fprintf(stderr, "ets: new: meta_pid_to_fixed_tab common.memory_size = %ld\n",
		     erts_smp_atomic_read(&meta_pid_to_fixed_tab->common.memory_size));
#endif

    db_lock(meta_pid_to_tab, LCK_WRITE);
    if (db_put_hash(NULL, meta_pid_to_tab,
		    TUPLE2(meta_tuple, BIF_P->id, 
			   make_small(slot)),&dummy) != DB_ERROR_NONE) {
	erl_exit(1,"Could not update ets metadata.");
    }
    db_unlock(meta_pid_to_tab, LCK_WRITE);

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

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_get(BIF_P, tb, BIF_ARG_2, &ret);

    db_unlock(tb, LCK_READ);

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

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_member(BIF_P, tb, BIF_ARG_2, &ret);

    db_unlock(tb, LCK_READ);

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

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_READ)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_small(BIF_ARG_3) || ((index = signed_val(BIF_ARG_3)) < 1)) {
	db_unlock(tb, LCK_READ);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_get_element(BIF_P, tb, 
					   BIF_ARG_2, index, &ret);
    db_unlock(tb, LCK_READ);
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
 * BIF to erase a whole table and release all memory it holds 
 */
BIF_RETTYPE ets_delete_1(BIF_ALIST_1)
{
    int trap;
    DbTable* tb;

#ifdef HARDDEBUG
    erts_fprintf(stderr,
		"ets:delete(%T); Process: %T, initial: %T:%T/%bpu\n",
		BIF_ARG_1, BIF_P->id,
		BIF_P->initial[0], BIF_P->initial[1], BIF_P->initial[2]);
#endif

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /*
     * Clear all access bits to prevent any ets operation to access the
     * table while it is being deleted.
     */
    tb->common.status &= ~(DB_PROTECTED|DB_PUBLIC|DB_PRIVATE);
    tb->common.status |= DB_DELETE;

    meta_main_tab_lock(tb->common.slot);
    /* We must keep the slot, to be found by db_proc_dead() if process dies */
    MARK_SLOT_DEAD(tb->common.slot);
    meta_main_tab_unlock(tb->common.slot);
    if (is_atom(tb->common.id)) {
	remove_named_tab(tb->common.id);
    }
    
    if (tb->common.owner != BIF_P->id) {
	Eterm dummy;
	Eterm meta_tuple[3];

	/*
	 * The process is being deleted by a process other than its owner.
	 * To make sure that the table will be completely deleted if the
	 * current process will be killed (e.g. by an EXIT signal), we will
	 * now transfer the ownership to the current process.
	 */
	db_lock(meta_pid_to_tab, LCK_WRITE);
	db_erase_bag_exact2(meta_pid_to_tab, tb->common.owner,
			    make_small(tb->common.slot));

	BIF_P->flags |= F_USING_DB;
	tb->common.owner = BIF_P->id;

	db_put_hash(NULL,
		    meta_pid_to_tab,
		    TUPLE2(meta_tuple,BIF_P->id,make_small(tb->common.slot)),
		    &dummy);
	db_unlock(meta_pid_to_tab, LCK_WRITE);
    }

    free_fixations_locked(tb);

    trap = free_table_cont(BIF_P, tb, 1, 1);
    db_unlock(tb, LCK_WRITE);
    if (trap) {
	/*
	 * Package the DbTable* pointer into a bignum so that it can be safely
	 * passed through a trap. We used to pass the DbTable* pointer directly
	 * (it looks like an continuation pointer), but that is will crash the
	 * emulator if this BIF is call traced.
	 */
	Eterm *hp = HAlloc(BIF_P, 2);
	hp[0] = make_pos_bignum_header(1);
	hp[1] = (Eterm) tb;
	BIF_TRAP1(&ets_delete_continue_exp, BIF_P, make_big(hp));
    }
    else {
	BIF_RET(am_true);
    }
}

/* 
** BIF to erase a whole table and release all memory it holds 
*/
BIF_RETTYPE ets_delete_all_objects_1(BIF_ALIST_1)
{
    DbTable* tb;

    CHECK_TABLES();

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    tb->common.meth->db_delete_all_objects(BIF_P, tb);

    db_unlock(tb, LCK_WRITE);

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

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_erase(BIF_P,tb,BIF_ARG_2,&ret);

    db_unlock(tb, LCK_WRITE);

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

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_tuple(BIF_ARG_2) || 
	(arityval(*tuple_val(BIF_ARG_2)) < tb->common.keypos)) {
	db_unlock(tb, LCK_WRITE);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_erase_object(BIF_P, tb,
					    BIF_ARG_2, &ret);
    db_unlock(tb, LCK_WRITE);

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
    BIF_RETTYPE result;
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
    
    if ((tb = db_get_table(p, tptr[1], DB_WRITE, LCK_WRITE)) == NULL) {
	/* This should be OK, the emulator handles errors in BIF's that aren't
	   exported nowdays... */
	BIF_ERROR(p,BADARG);
    }

    cret = tb->common.meth->db_select_delete_continue(p,tb,a1,&ret);

    db_unlock(tb, LCK_WRITE);
    /* SMP fixme table may be deleted !!! */

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && !DID_TRAP(p,ret) &&
	   !ONLY_READER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	ERTS_BIF_PREP_RET(result, ret);
	break;
    default:
	if(IS_HASH_TABLE(tb->common.status) && !ONLY_READER(p,tb)) {
	    ets_safe_fixtable_2(p, tb->common.id, am_false);
	} 
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    erts_match_set_release_result(p);

    return result;
}
    

BIF_RETTYPE ets_select_delete_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_WRITE, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if(eq(BIF_ARG_2, ms_delete_all)){
	int nitems = tb->common.nitems;
	tb->common.meth->db_delete_all_objects(BIF_P, tb);
	db_unlock(tb, LCK_WRITE);
	BIF_RET(erts_make_integer(nitems,BIF_P));
    }
    cret = tb->common.meth->db_select_delete(BIF_P, tb, BIF_ARG_2, &ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && DID_TRAP(BIF_P,ret) && 
	   !ONLY_READER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	}
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    erts_match_set_release_result(BIF_P);

    return result;
}

/* 
** Return a list of tables on this node 
*/
BIF_RETTYPE ets_all_0(BIF_ALIST_0)
{
    DbTable* tb;
    Eterm previous;
    int i, j;
    Eterm* hp;
    Eterm* hendp;
    int t_tabs_cnt;
    int t_max_tabs;

    erts_smp_spin_lock(&meta_main_tab_main_lock);
    t_tabs_cnt = meta_main_tab_cnt;
    t_max_tabs = db_max_tabs;
    erts_smp_spin_unlock(&meta_main_tab_main_lock);

    hp = HAlloc(BIF_P, 2*t_tabs_cnt);
    hendp = hp + 2*t_tabs_cnt;

    previous = NIL;
    j = 0;
    for(i = 0; (i < t_max_tabs && j < t_tabs_cnt); i++) {
	meta_main_tab_lock(i);
	if (IS_SLOT_ALIVE(i)) {
	    j++;
	    tb = meta_main_tab[i].u.tb;
	    previous = CONS(hp, tb->common.id, previous);
	    hp += 2;
	}
	meta_main_tab_unlock(i);
    }
    HRelease(BIF_P, hendp, hp);
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

    if ((tb = db_get_table2(BIF_P, BIF_ARG_1, DB_READ, 
			    step_lock_type)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    /* The slot number is checked in table specific code. */
    cret = tb->common.meth->db_slot(BIF_P, tb, BIF_ARG_2, &ret);
    db_unlock(tb, STEP_LOCK_TYPE(tb));
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
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Sint chunk_size;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Chunk size strictly greater than 0 */
    if (is_not_small(BIF_ARG_3) || (chunk_size = signed_val(BIF_ARG_3)) <= 0) {
	db_unlock(tb, LCK_WRITE);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_select_chunk(BIF_P, tb,
					    BIF_ARG_2, chunk_size, 
					    0 /* not reversed */, &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	db_unlock(tb, LCK_WRITE);
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    erts_match_set_release_result(BIF_P);

    return result;
}


/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_trap_1(Process *p, Eterm a1)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1)

    if ((tb = db_get_table(p, tptr[1], DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(p, BADARG);
    }

    cret = tb->common.meth->db_select_continue(p, tb,a1,&ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && !DID_TRAP(p,ret) &&
	   !ONLY_WRITER(p,tb)) {
	    /* We did trap, but no more... */
	    unfix_table_locked(p, tb);
	} /* Otherwise keep it fixed */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	if(IS_HASH_TABLE(tb->common.status) && !ONLY_WRITER(p,tb)) {
	    unfix_table_locked(p, tb);
	}
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	if(IS_HASH_TABLE(tb->common.status) && !ONLY_WRITER(p,tb)) {
	    unfix_table_locked(p, tb);
	}
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(p);

    return result;
}


BIF_RETTYPE ets_select_1(BIF_ALIST_1)
{
    BIF_RETTYPE result;
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
    if (arityval(*tptr) < 1 || 
	(tb = db_get_table(BIF_P, tptr[1], DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_select_continue(BIF_P,tb,
					       BIF_ARG_1, &ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&  DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(BIF_P);

    return result;
}

BIF_RETTYPE ets_select_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    cret = tb->common.meth->db_select(BIF_P, tb, BIF_ARG_2,
				      0, &ret);

    /* SMP fixme table may be deleted !!! */
    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(BIF_P);

    return result;
}

/* We get here instead of in the real BIF when trapping */
static BIF_RETTYPE ets_select_count_1(Process *p, Eterm a1)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Eterm *tptr;

    CHECK_TABLES();

    tptr = tuple_val(a1);
    ASSERT(arityval(*tptr) >= 1)
    if ((tb = db_get_table(p, tptr[1], DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(p, BADARG);
    }

    cret = tb->common.meth->db_select_count_continue(p, tb, a1, &ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   !DID_TRAP(p,ret) &&
	   !ONLY_WRITER(p,tb)) {
	    /* We did trap, but no more... */
	    unfix_table_locked(p, tb);
	} /* Otherwise keep it fixed */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	if(IS_HASH_TABLE(tb->common.status) && !ONLY_WRITER(p,tb)) {
	    unfix_table_locked(p, tb);
	}
	ERTS_BIF_PREP_ERROR(result, p, SYSTEM_LIMIT);
	break;
    default:
	if(IS_HASH_TABLE(tb->common.status) && !ONLY_WRITER(p,tb)) {
	    unfix_table_locked(p, tb);
	}
	ERTS_BIF_PREP_ERROR(result, p, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);
    erts_match_set_release_result(p);

    return result;
}

BIF_RETTYPE ets_select_count_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    cret = tb->common.meth->db_select_count(BIF_P,tb,BIF_ARG_2, &ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }
    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(BIF_P);

    return result;
}


BIF_RETTYPE ets_select_reverse_3(BIF_ALIST_3)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;
    Sint chunk_size;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* Chunk size strictly greater than 0 */
    if (is_not_small(BIF_ARG_3) || (chunk_size = signed_val(BIF_ARG_3)) <= 0) {
	db_unlock(tb, LCK_WRITE);
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_select_chunk(BIF_P,tb,
					    BIF_ARG_2, chunk_size, 
					    1 /* reversed */, &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) &&
	   DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(BIF_P);

    return result;
}

BIF_RETTYPE ets_select_reverse_1(BIF_ALIST_1)
{
    return ets_select_1(BIF_P, BIF_ARG_1);
}

BIF_RETTYPE ets_select_reverse_2(BIF_ALIST_2)
{
    BIF_RETTYPE result;
    DbTable* tb;
    int cret;
    Eterm ret;

    CHECK_TABLES();
    /*
     * Make sure that the table exists.
     */

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_READ, LCK_WRITE)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    cret = tb->common.meth->db_select(BIF_P,tb,BIF_ARG_2,
				      1 /*reversed*/, &ret);

    switch (cret) {
    case DB_ERROR_NONE:
	if(IS_HASH_TABLE(tb->common.status) && DID_TRAP(BIF_P,ret) &&
	   !ONLY_WRITER(BIF_P,tb)) {
	    /* We will trap and as we're here this call wasn't a trap... */
	    fix_table_locked(BIF_P, tb);
	} /* Otherwise keep it as is */
	ERTS_BIF_PREP_RET(result, ret);
	break;
    case DB_ERROR_SYSRES:
	ERTS_BIF_PREP_ERROR(result, BIF_P, SYSTEM_LIMIT);
	break;
    default:
	ERTS_BIF_PREP_ERROR(result, BIF_P, BADARG);
	break;
    }

    db_unlock(tb, LCK_WRITE);

    erts_match_set_release_result(BIF_P);

    return result;
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
 * BIF to extract information about a particular table.
 */ 

BIF_RETTYPE ets_info_1(BIF_ALIST_1)
{
    static Eterm fields[] = {am_protection, am_keypos, am_type, am_named_table,
				 am_node, am_size, am_name, am_owner, am_memory};
    Eterm results[sizeof(fields)/sizeof(Eterm)];
    DbTable* tb;
    Eterm res;
    int i;
    Eterm* hp;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL) {
	if (is_atom(BIF_ARG_1) || is_small(BIF_ARG_1)) {
	    BIF_RET(am_undefined);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    for (i = 0; i < sizeof(fields)/sizeof(Eterm); i++) {
	results[i] = table_info(BIF_P, tb, fields[i]);
	ASSERT(is_value(results[i]));
    }
    db_unlock(tb, LCK_READ);

    hp = HAlloc(BIF_P, 5*sizeof(fields)/sizeof(Eterm));
    res = NIL;
    for (i = 0; i < sizeof(fields)/sizeof(Eterm); i++) {
	Eterm tuple;
	tuple = TUPLE2(hp, fields[i], results[i]);
	hp += 3;
	res = CONS(hp, tuple, res);
	hp += 2;
    }
    BIF_RET(res);
}

/* 
 * BIF to extract information about a particular table.
 */ 

BIF_RETTYPE ets_info_2(BIF_ALIST_2)
{
    DbTable* tb;
    Eterm ret = THE_NON_VALUE;

    if ((tb = db_get_table(BIF_P, BIF_ARG_1, DB_INFO, LCK_READ)) == NULL) {
	if (is_atom(BIF_ARG_1) || is_small(BIF_ARG_1)) {
	    BIF_RET(am_undefined);
	}
	BIF_ERROR(BIF_P, BADARG);
    }
    ret = table_info(BIF_P, tb, BIF_ARG_2);
    db_unlock(tb, LCK_READ);
    if (is_non_value(ret)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(ret);
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
    Eterm *hp;
    if (mp == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, PROC_BIN_SIZE);

    BIF_RET(erts_mk_magic_binary_term(&hp, &MSO(BIF_P), mp));
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
    if (!IsMatchProgBinary(mp)) {
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


/*
** External interface (NOT BIF's)
*/


/* Init the db */

void init_db(void)
{
    DbTable init_tb;
    int i;
    extern Eterm* em_apply_bif;
    Eterm *hp;
    unsigned bits;
    size_t size;

#ifdef ERTS_SMP
    for (i=0; i<META_MAIN_TAB_LOCK_CNT; i++) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_smp_spinlock_init_x(&meta_main_tab_locks[i].lck, "meta_main_tab_slot", make_small(i));
#else
	erts_smp_spinlock_init(&meta_main_tab_locks[i].lck, "meta_main_tab_slot");
#endif
    }
    erts_smp_spinlock_init(&meta_main_tab_main_lock, "meta_main_tab_main");
    for (i=0; i<META_NAME_TAB_LOCK_CNT; i++) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_smp_rwmtx_init_x(&meta_name_tab_rwlocks[i].lck, "meta_name_tab", make_small(i));
#else
	erts_smp_rwmtx_init(&meta_name_tab_rwlocks[i].lck, "meta_name_tab");
#endif
    }
#endif

    erts_smp_atomic_init(&erts_ets_misc_mem_size, 0);
    db_initialize_util();

    if (user_requested_db_max_tabs < DB_DEF_MAX_TABS)
	db_max_tabs = DB_DEF_MAX_TABS;
    else
	db_max_tabs = user_requested_db_max_tabs;

    bits = erts_fit_in_bits(db_max_tabs-1);
    if (bits > SMALL_BITS) {
	erl_exit(1,"Max limit for ets tabled too high %u (max %u).",
		 db_max_tabs, 1L<<SMALL_BITS);
    }
    meta_main_tab_slot_mask = (1L<<bits) - 1;
    meta_main_tab_seq_incr = (1L<<bits);

    size = sizeof(*meta_main_tab)*db_max_tabs;
    meta_main_tab = erts_db_alloc_nt(ERTS_ALC_T_DB_TABLES, size);
    ERTS_ETS_MISC_MEM_ADD(size);

    meta_main_tab_cnt = 0;
    for (i=1; i<db_max_tabs; i++) {
	SET_NEXT_FREE_SLOT(i-1,i);
    }
    SET_NEXT_FREE_SLOT(db_max_tabs-1, (Uint)-1);
    meta_main_tab_first_free = 0;

    meta_name_tab_mask = (1L<<(bits-1)) - 1; /* At least half the size of main tab */
    size = sizeof(struct meta_name_tab_entry)*(meta_name_tab_mask+1);
    meta_name_tab = erts_db_alloc_nt(ERTS_ALC_T_DB_TABLES, size);
    ERTS_ETS_MISC_MEM_ADD(size);

    for (i=0; i<=meta_name_tab_mask; i++) {
	meta_name_tab[i].pu.tb = NULL;
	meta_name_tab[i].u.name_atom = NIL;
    }

    db_initialize_hash();
    db_initialize_tree();

    /*TT*/
    /* Create meta table invertion. */
    erts_smp_atomic_init(&init_tb.common.memory_size, 0);
    meta_pid_to_tab = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
					       &init_tb,
					       sizeof(DbTable));
    ERTS_ETS_MISC_MEM_ADD(sizeof(DbTable));
    erts_smp_atomic_init(&meta_pid_to_tab->common.memory_size,
			 erts_smp_atomic_read(&init_tb.common.memory_size));

    meta_pid_to_tab->common.id = NIL;
    meta_pid_to_tab->common.the_name = am_true;
    meta_pid_to_tab->common.status = (DB_NORMAL | DB_BAG | DB_LHASH | 
				      DB_PUBLIC);
#ifdef ERTS_SMP
    meta_pid_to_tab->common.type
	= meta_pid_to_tab->common.status & ERTS_ETS_TABLE_TYPES;
    /* Note, 'type' is *read only* from now on... */
#endif
    meta_pid_to_tab->common.keypos = 1;
    meta_pid_to_tab->common.owner  = NIL;
    meta_pid_to_tab->common.nitems = 0;
    meta_pid_to_tab->common.slot   = -1;
    meta_pid_to_tab->common.meth   = &db_hash;

    db_init_lock(meta_pid_to_tab, "meta_pid_to_tab");

    if (db_create_hash(NULL, meta_pid_to_tab) != DB_ERROR_NONE) {
	erl_exit(1,"Unable to create ets metadata tables.");
    }

    erts_smp_atomic_set(&init_tb.common.memory_size, 0);
    meta_pid_to_fixed_tab = (DbTable*) erts_db_alloc(ERTS_ALC_T_DB_TABLE,
						     &init_tb,
						     sizeof(DbTable));
    ERTS_ETS_MISC_MEM_ADD(sizeof(DbTable));
    erts_smp_atomic_init(&meta_pid_to_fixed_tab->common.memory_size,
			 erts_smp_atomic_read(&init_tb.common.memory_size));

    meta_pid_to_fixed_tab->common.id = NIL;
    meta_pid_to_fixed_tab->common.the_name = am_true;
    meta_pid_to_fixed_tab->common.status = (DB_NORMAL | DB_BAG | DB_LHASH | 
					    DB_PUBLIC);
#ifdef ERTS_SMP
    meta_pid_to_fixed_tab->common.type
	= meta_pid_to_fixed_tab->common.status & ERTS_ETS_TABLE_TYPES;
    /* Note, 'type' is *read only* from now on... */
#endif
    meta_pid_to_fixed_tab->common.keypos = 1;
    meta_pid_to_fixed_tab->common.owner  = NIL;
    meta_pid_to_fixed_tab->common.nitems = 0;
    meta_pid_to_fixed_tab->common.slot   = -1;
    meta_pid_to_fixed_tab->common.meth   = &db_hash;

    db_init_lock(meta_pid_to_fixed_tab, "meta_pid_to_fixed_tab");

    if (db_create_hash(NULL, meta_pid_to_fixed_tab) != DB_ERROR_NONE) {
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

    /* Non visual BIF to trap to. */
    memset(&ets_delete_continue_exp, 0, sizeof(Export));
    ets_delete_continue_exp.address = &ets_delete_continue_exp.code[3];
    ets_delete_continue_exp.code[0] = am_ets;
    ets_delete_continue_exp.code[1] = am_atom_put("delete_trap",11);
    ets_delete_continue_exp.code[2] = 1;
    ets_delete_continue_exp.code[3] = (Eterm) em_apply_bif;
    ets_delete_continue_exp.code[4] = (Eterm) &ets_delete_trap;

    hp = ms_delete_all_buff;
    ms_delete_all = CONS(hp, am_true, NIL);
    hp += 2;
    ms_delete_all = TUPLE3(hp,am_Underscore,NIL,ms_delete_all);
    hp +=4;
    ms_delete_all = CONS(hp, ms_delete_all,NIL);
}

#define ARRAY_CHUNK 100

typedef enum {
    ErtsDbProcCleanupProgressTables,
    ErtsDbProcCleanupProgressFixations,
    ErtsDbProcCleanupProgressDone,
} ErtsDbProcCleanupProgress;

typedef enum {
    ErtsDbProcCleanupOpGetTables,
    ErtsDbProcCleanupOpDeleteTables,
    ErtsDbProcCleanupOpGetFixations,
    ErtsDbProcCleanupOpDeleteFixations,
    ErtsDbProcCleanupOpDone
} ErtsDbProcCleanupOperation;

typedef struct {
    ErtsDbProcCleanupProgress progress;
    ErtsDbProcCleanupOperation op;
    struct {
	Eterm arr[ARRAY_CHUNK];
	int size;
	int ix;
	int clean_ix;
    } slots;
} ErtsDbProcCleanupState;


static void
proc_exit_cleanup_tables_meta_data(Eterm pid, ErtsDbProcCleanupState *state)
{
    ASSERT(state->slots.clean_ix <= state->slots.ix);
    if (state->slots.clean_ix < state->slots.ix) {
	db_lock(meta_pid_to_tab, LCK_WRITE);
	if (state->slots.size < ARRAY_CHUNK
	    && state->slots.ix == state->slots.size) {
	    Eterm dummy;
	    db_erase_hash(NULL,meta_pid_to_tab,pid,&dummy);
	}
	else {
	    int ix;
	    /* Need to erase each explicitly */
	    for (ix = state->slots.clean_ix; ix < state->slots.ix; ix++)
		db_erase_bag_exact2(meta_pid_to_tab,
				    pid,
				    state->slots.arr[ix]);
	}
	db_unlock(meta_pid_to_tab, LCK_WRITE);
	state->slots.clean_ix = state->slots.ix;
    }
}

static void
proc_exit_cleanup_fixations_meta_data(Eterm pid, ErtsDbProcCleanupState *state)
{
    ASSERT(state->slots.clean_ix <= state->slots.ix);
    if (state->slots.clean_ix < state->slots.ix) {
	db_lock(meta_pid_to_fixed_tab, LCK_WRITE);
	if (state->slots.size < ARRAY_CHUNK
	    && state->slots.ix == state->slots.size) {
	    Eterm dummy;
	    db_erase_hash(NULL,meta_pid_to_fixed_tab,pid,&dummy);
	}
	else {
	    int ix;
	    /* Need to erase each explicitly */
	    for (ix = state->slots.clean_ix; ix < state->slots.ix; ix++)
		db_erase_bag_exact2(meta_pid_to_fixed_tab,
				    pid,
				    state->slots.arr[ix]);
	}
	db_unlock(meta_pid_to_fixed_tab, LCK_WRITE);
	state->slots.clean_ix = state->slots.ix;
    }
}

/*
 * erts_db_process_exiting() is called when a process terminates.
 * It returns 0 when completely done, and !0 when it wants to
 * yield. c_p->u.exit_data can hold a pointer to a state while
 * yielding.
 */
#define ERTS_DB_INTERNAL_ERROR(LSTR) \
  erl_exit(ERTS_ABORT_EXIT, "%s:%d:erts_db_process_exiting(): " LSTR "\n", \
	   __FILE__, __LINE__)

int
erts_db_process_exiting(Process *c_p, ErtsProcLocks c_p_locks)
{
    ErtsDbProcCleanupState *state = (ErtsDbProcCleanupState *) c_p->u.exit_data;
    Eterm pid = c_p->id;
    ErtsDbProcCleanupState default_state;
    int ret;

    if (!state) {
	state = &default_state;
	state->progress = ErtsDbProcCleanupProgressTables;
	state->op = ErtsDbProcCleanupOpGetTables;
    }

    while (!0) {
	switch (state->op) {
	case ErtsDbProcCleanupOpGetTables:
	    state->slots.size = ARRAY_CHUNK;
	    db_lock(meta_pid_to_tab, LCK_READ);
	    ret = db_get_element_array(meta_pid_to_tab,
				       pid,
				       2,
				       state->slots.arr,
				       &state->slots.size);
	    db_unlock(meta_pid_to_tab, LCK_READ);
	    if (ret == DB_ERROR_BADKEY) {
		/* Done with tables; now fixations */
		state->progress = ErtsDbProcCleanupProgressFixations;
		state->op = ErtsDbProcCleanupOpGetFixations;
		break;
	    } else if (ret != DB_ERROR_NONE) {
		ERTS_DB_INTERNAL_ERROR("Inconsistent ets table metadata");
	    }

	    state->slots.ix = 0;
	    state->slots.clean_ix = 0;
	    state->op = ErtsDbProcCleanupOpDeleteTables;
	    /* Fall through */

	case ErtsDbProcCleanupOpDeleteTables:

	    while (state->slots.ix < state->slots.size) {
		DbTable *tb = NULL;
		Sint ix = unsigned_val(state->slots.arr[state->slots.ix]);
		meta_main_tab_lock(ix);
		if (!IS_SLOT_FREE(ix)) {
		    tb = db_ref(GET_ANY_SLOT_TAB(ix));
		    ASSERT(tb);
		}
		meta_main_tab_unlock(ix);
		if (tb) {
		    int do_yield;
		    db_lock_take_over_ref(tb, LCK_WRITE);
		    /* Ownership may have changed since
		       we looked up the table. */
		    if (tb->common.owner != pid)
			do_yield = 0;
		    else {
			int first_call;
#ifdef HARDDEBUG
			erts_fprintf(stderr,
				     "erts_db_process_exiting(); Table: %T, "
				     "Process: %T\n",
				     tb->common.id, pid);
#endif
			first_call = (tb->common.status & DB_DELETE) == 0;
			if (first_call) {
			    first_call = 1;
			    
			    /* Clear all access bits. */
			    tb->common.status &= ~(DB_PROTECTED
						   | DB_PUBLIC
						   | DB_PRIVATE);
			    tb->common.status |= DB_DELETE;

			    if (is_atom(tb->common.id))
				remove_named_tab(tb->common.id);

			    free_fixations_locked(tb);
			}

			do_yield = free_table_cont(c_p, tb, first_call, 0);
		    }
		    db_unlock(tb, LCK_WRITE);
		    if (do_yield)
			goto yield;
		}
		state->slots.ix++;
		if (ERTS_BIF_REDS_LEFT(c_p) <= 0)
		    goto yield;
	    }

	    proc_exit_cleanup_tables_meta_data(pid, state);
	    state->op = ErtsDbProcCleanupOpGetTables;
	    break;

	case ErtsDbProcCleanupOpGetFixations:
	    state->slots.size = ARRAY_CHUNK;
	    db_lock(meta_pid_to_fixed_tab, LCK_READ);
	    ret = db_get_element_array(meta_pid_to_fixed_tab, 
				       pid,
				       2,
				       state->slots.arr,
				       &state->slots.size);
	    db_unlock(meta_pid_to_fixed_tab, LCK_READ);

	    if (ret == DB_ERROR_BADKEY) {
		/* Done */
		state->progress = ErtsDbProcCleanupProgressDone;
		state->op = ErtsDbProcCleanupOpDone;
		break;
	    } else if (ret != DB_ERROR_NONE) {
		ERTS_DB_INTERNAL_ERROR("Inconsistent ets fix table metadata");
	    }

	    state->slots.ix = 0;
	    state->slots.clean_ix = 0;
	    state->op = ErtsDbProcCleanupOpDeleteFixations;
	    /* Fall through */

	case ErtsDbProcCleanupOpDeleteFixations:

	    while (state->slots.ix < state->slots.size) {
		DbTable *tb = NULL;
		Sint ix = unsigned_val(state->slots.arr[state->slots.ix]);
		meta_main_tab_lock(ix);
		if (IS_SLOT_ALIVE(ix)) {
		    tb = db_ref(meta_main_tab[ix].u.tb);
		    ASSERT(tb);
		}
		meta_main_tab_unlock(ix);
		if (tb) {
		    int reds;
		    DbFixation **pp;

		    db_lock_take_over_ref(tb, LCK_WRITE);
		    reds = 10;

		    for (pp = &(tb->common.fixations);
			 *pp;
			 pp = &((*pp)->next)) {
			if ((*pp)->pid == pid) {
			    DbFixation *fix = *pp;
			    *pp = (*pp)->next;
			    erts_db_free(ERTS_ALC_T_DB_FIXATION,
					 tb,
					 (void *) fix,
					 sizeof(DbFixation));
			    ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
			    break;
			}
		    }
		    if (tb->common.fixations == NULL) {
			if (IS_HASH_TABLE(tb->common.status)) {
			    db_unfix_table_hash(&(tb->hash));
			    reds += 40;
			}
			tb->common.status &= ~DB_FIXED;
		    }
		    db_unlock(tb, LCK_WRITE);
		    BUMP_REDS(c_p, reds);
		}
		state->slots.ix++;
		if (ERTS_BIF_REDS_LEFT(c_p) <= 0)
		    goto yield;
	    }

	    proc_exit_cleanup_fixations_meta_data(pid, state);
	    state->op = ErtsDbProcCleanupOpGetFixations;
	    break;

	case ErtsDbProcCleanupOpDone:

	    if (state != &default_state)
		erts_free(ERTS_ALC_T_DB_PROC_CLEANUP, state);
	    c_p->u.exit_data = NULL;
	    return 0;

	default:
	    ERTS_DB_INTERNAL_ERROR("Bad internal state");
	}
    }

 yield:

    switch (state->progress) {
    case ErtsDbProcCleanupProgressTables:
	proc_exit_cleanup_tables_meta_data(pid, state);
	break;
    case ErtsDbProcCleanupProgressFixations:
	proc_exit_cleanup_fixations_meta_data(pid, state);
	break;
    default:
	break;
    }

    ASSERT(c_p->u.exit_data == (void *) state
	   || state == &default_state);

    if (state == &default_state) {
	c_p->u.exit_data = erts_alloc(ERTS_ALC_T_DB_PROC_CLEANUP,
				      sizeof(ErtsDbProcCleanupState));
	sys_memcpy(c_p->u.exit_data,
		   (void*) state,
		   sizeof(ErtsDbProcCleanupState));
    }

    return !0;
}

/*  SMP note: table must be WRITE locked */
static void fix_table_locked(Process* p, DbTable* tb)
{
    DbFixation *fix;
    Eterm dummy;
    Eterm meta_tuple[3];

    if (!(tb->common.status & DB_FIXED)) { 
	tb->common.status |= DB_FIXED;
	get_now(&(tb->common.megasec),
		&(tb->common.sec), 
		&(tb->common.microsec));
    }
    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
	if (fix->pid == p->id) {
	    ++(fix->counter);
	    break;
	}
    }
    if (fix == NULL) {
	fix = (DbFixation *) erts_db_alloc(ERTS_ALC_T_DB_FIXATION,
					   tb, sizeof(DbFixation));
	ERTS_ETS_MISC_MEM_ADD(sizeof(DbFixation));
	fix->pid = p->id;
	fix->counter = 1;
	fix->next = tb->common.fixations;
	tb->common.fixations = fix;
	p->flags |= F_USING_DB;        
	db_lock(meta_pid_to_fixed_tab, LCK_WRITE);
	if (db_put_hash(NULL,meta_pid_to_fixed_tab,
			TUPLE2(meta_tuple, 
			       p->id, 
			       make_small(tb->common.slot)),
			&dummy)
	    != DB_ERROR_NONE) {
	    erl_exit(1,"Could not insert ets metadata"
		     " in safe_fixtable.");
	}	
	db_unlock(meta_pid_to_fixed_tab, LCK_WRITE);
    }
}

/*  SMP note: table must be WRITE locked */
static void unfix_table_locked(Process* p,  DbTable* tb)
{
    DbFixation **pp;
    DbFixation *fix;
    
    for (pp = &(tb->common.fixations); *pp != NULL; pp = &((*pp)->next)) {
	if ((*pp)->pid == p->id) {
	    --((*pp)->counter);
	    ASSERT((*pp)->counter >= 0);
	    if ((*pp)->counter == 0) {
		fix = *pp;
		*pp = (*pp)->next;
		db_lock(meta_pid_to_fixed_tab, LCK_WRITE);
		db_erase_bag_exact2(meta_pid_to_fixed_tab,
				    p->id,
				    make_small(tb->common.slot));
		db_unlock(meta_pid_to_fixed_tab, LCK_WRITE);
		erts_db_free(ERTS_ALC_T_DB_FIXATION,
			     tb, (void *) fix, sizeof(DbFixation));
		ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));
	    }
	    break;
	}
    }
    if (tb->common.fixations == NULL) {
	if (IS_HASH_TABLE(tb->common.status)) {
	    db_unfix_table_hash(&(tb->hash));
	}
	tb->common.status &= ~DB_FIXED;
    }
}

/* Assume that tb is locked (write) */
static void free_fixations_locked(DbTable *tb)
{
    DbFixation *fix;
    DbFixation *next_fix;

    fix = tb->common.fixations;
    while (fix != NULL) {
	next_fix = fix->next;
	db_lock(meta_pid_to_fixed_tab, LCK_WRITE);
	db_erase_bag_exact2(meta_pid_to_fixed_tab,
			    fix->pid,
			    make_small(tb->common.slot));
	db_unlock(meta_pid_to_fixed_tab, LCK_WRITE);
	erts_db_free(ERTS_ALC_T_DB_FIXATION,
		     tb, (void *) fix, sizeof(DbFixation));
	ERTS_ETS_MISC_MEM_ADD(-sizeof(DbFixation));

	fix = next_fix;
    }
    tb->common.fixations = NULL;
}


static BIF_RETTYPE ets_delete_trap(Process *p, Eterm cont)
{
    int trap;
    Eterm* ptr = big_val(cont);
    DbTable *tb = (DbTable *) ptr[1];

    ASSERT(*ptr == make_pos_bignum_header(1));

    db_lock(tb, LCK_WRITE);
    trap = free_table_cont(p, tb, 0, 1);
    db_unlock(tb, LCK_WRITE);

    if (trap) {
	BIF_TRAP1(&ets_delete_continue_exp, p, cont);
    }
    else {
	BIF_RET(am_true);
    }
}


/*
 * free_table_cont() returns 0 when done and !0 when more work is needed.
 */
static int free_table_cont(Process *p,
			   DbTable *tb,
			   int first,
			   int clean_meta_tab)
{
    Eterm result;

#ifdef HARDDEBUG
    if (!first) {
	erts_fprintf(stderr,"ets: free_table_cont %T (continue)\r\n",
		     tb->common.id);
    }
#endif

    result = tb->common.meth->db_free_table_continue(tb, first);

    if (result == 0) {
#ifdef HARDDEBUG
	erts_fprintf(stderr,"ets: free_table_cont %T (continue begin)\r\n",
		     tb->common.id);
#endif
	/* More work to be done. Let other processes work and call us again. */
	BUMP_ALL_REDS(p);
	return !0;
    }
    else {
#ifdef HARDDEBUG
	erts_fprintf(stderr,"ets: free_table_cont %T (continue end)\r\n",
		     tb->common.id);
#endif
	/* Completely done - we will not get called again. */
	meta_main_tab_lock(tb->common.slot);
	free_slot(tb->common.slot);
	meta_main_tab_unlock(tb->common.slot);

	if (clean_meta_tab) {
	    db_lock(meta_pid_to_tab, LCK_WRITE);
	    db_erase_bag_exact2(meta_pid_to_tab,tb->common.owner,
				make_small(tb->common.slot));
	    db_unlock(meta_pid_to_tab, LCK_WRITE);
	}
	db_unref(tb);
	BUMP_REDS(p, 100);
	return 0;
    }
}

static Eterm table_info(Process* p, DbTable* tb, Eterm What)
{
    Eterm ret = THE_NON_VALUE;

    if (What == am_size) {
	ret = make_small(tb->common.nitems);
    } else if (What == am_type) {
	if (tb->common.status & DB_SET)  {
	    ret = am_set;
	} else if (tb->common.status & DB_DUPLICATE_BAG) {
	    ret = am_duplicate_bag;
	} else if (tb->common.status & DB_ORDERED_SET) {
	    ret = am_ordered_set;
	} else { /*TT*/
	    ASSERT(tb->common.status & DB_BAG);
	    ret = am_bag;
	}
    } else if (What == am_memory) {
	Uint words = (Uint) ((erts_smp_atomic_read(&tb->common.memory_size)
			      + sizeof(Uint)
			      - 1)
			     / sizeof(Uint));
	ret = erts_make_integer(words, p);
    } else if (What == am_owner) {
	ret = tb->common.owner;
    } else if (What == am_protection) {
	if (tb->common.status & DB_PRIVATE) 
	    ret = am_private;
	else if (tb->common.status & DB_PROTECTED)
	    ret = am_protected;
	else if (tb->common.status & DB_PUBLIC)
	    ret = am_public;
    } else if (What == am_name) {
	ret = tb->common.the_name;
    } else if (What == am_keypos) {
	ret = make_small(tb->common.keypos);
    } else if (What == am_node) {
	ret = erts_this_dist_entry->sysname;
    } else if (What == am_named_table) {
	ret = is_atom(tb->common.id) ? am_true : am_false;
    /*
     * For debugging purposes
     */
    } else if (What == am_data) { 
	print_table(ERTS_PRINT_STDOUT, NULL, 1, tb);
	ret = am_true;
    } else if (What == am_atom_put("fixed",5)) { 
	if (tb->common.status & DB_FIXED)
	    ret = am_true;
	else
	    ret = am_false;
    } else if (What == am_atom_put("kept_objects",12)) {
	ret = make_small(tb->common.kept_items);
    } else if (What == am_atom_put("safe_fixed",10)) { 
	if (tb->common.fixations != NULL) {
	    Uint need;
	    Eterm *hp;
	    Eterm tpl, lst;
	    DbFixation *fix;
	    need = 7;
	    for (fix = tb->common.fixations; fix != NULL; fix = fix->next) {
		need += 5;
	    }
	    hp = HAlloc(p, need);
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
	    ret = TUPLE2(hp, tpl, lst);
	} else {
	    ret = am_false;
	}
    }
    return ret;
}

static void print_table(int to, void *to_arg, int show,  DbTable* tb)
{
    erts_print(to, to_arg, "Table: %T\n", tb->common.id);
    erts_print(to, to_arg, "Name: %T\n", tb->common.the_name);

    tb->common.meth->db_print(to, to_arg, show, tb);

    erts_print(to, to_arg, "Objects: %d\n", tb->common.nitems);
    erts_print(to, to_arg, "Words: %bpu\n",
	       (Uint) ((erts_smp_atomic_read(&tb->common.memory_size)
			+ sizeof(Uint)
			- 1)
		       / sizeof(Uint)));
}

void db_info(int to, void *to_arg, int show)    /* Called by break handler */
{
    int i;
    for (i=0; i < db_max_tabs; i++) 
	if (IS_SLOT_ALIVE(i)) {
	    erts_print(to, to_arg, "=ets:%T\n", meta_main_tab[i].u.tb->common.owner);
	    erts_print(to, to_arg, "Slot: %d\n", i);
	    print_table(to, to_arg, show, meta_main_tab[i].u.tb);
	}
#ifdef DEBUG
    erts_print(to, to_arg, "=internal_ets: Process to table index\n");
    print_table(to, to_arg, show, meta_pid_to_tab);
    erts_print(to, to_arg, "=internal_ets: Process to fixation index\n");
    print_table(to, to_arg, show, meta_pid_to_fixed_tab);
#endif
}

Uint
erts_get_ets_misc_mem_size(void)
{
    /* Memory not allocated in ets_alloc */
    return (Uint) erts_smp_atomic_read(&erts_ets_misc_mem_size);
}

/* SMP Note: May only be used when system is locked */
void
erts_db_foreach_table(void (*func)(DbTable *, void *), void *arg)
{
    int i, j;
    j = 0;
    for(i = 0; (i < db_max_tabs && j < meta_main_tab_cnt); i++) {
	if (IS_SLOT_ALIVE(i)) {
	    j++;
	    (*func)(meta_main_tab[i].u.tb, arg);
	}
    }
    ASSERT(j == meta_main_tab_cnt);
}

/* SMP Note: May only be used when system is locked */
void
erts_db_foreach_offheap(DbTable *tb,
			void (*func)(ErlOffHeap *, void *),
			void *arg)
{
    tb->common.meth->db_foreach_offheap(tb, func, arg);
}

/*
 * For testing of meta tables only.
 *
 * Given a name atom (as returned from ets:new/2), return a list of 'cnt'
 * number of other names that will hash to the same bucket in meta_name_tab.
 *
 * WARNING: Will bloat the atom table!
 */
Eterm
erts_ets_colliding_names(Process* p, Eterm name, Uint cnt)
{
    Eterm list = NIL;
    Eterm* hp = HAlloc(p,cnt*2);
    Uint index = atom_val(name) & meta_name_tab_mask;

    while (cnt) {
        if (index != atom_val(name)) {
            while (index >= atom_table_size()) {
                char tmp[20];
                erts_snprintf(tmp, sizeof(tmp), "am%x", atom_table_size());
                am_atom_put(tmp,strlen(tmp));
            }
            list = CONS(hp, make_atom(index), list);
            hp += 2;
            --cnt;
        }
        index += meta_name_tab_mask + 1;
    }
    return list;
}


#ifdef HARDDEBUG   /* Here comes some debug functions */

void db_check_tables(void)
{
#ifdef ERTS_SMP
    return;
#else
    int i;

    for (i = 0; i < db_max_tabs; i++) {
	if (IS_SLOT_ALIVE(i)) {
	    DbTable* tb = meta_main_tab[i].t; 
	    tb->common.meth->db_check_table(tb);
	}
    }
#endif
}

#endif /* HARDDEBUG */
