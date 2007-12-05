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
 * This file now contains only the definitions needed for the
 * meta table.
 *
 */

#ifndef __DB_H__
#define __DB_H__

#include "sys.h"
#include "bif.h"

#include "erl_db_util.h" /* Flags */
#include "erl_db_hash.h" /* DbTableHash */
#include "erl_db_tree.h" /* DbTableTree */
/*TT*/

Uint erts_ets_memory_size(void);

/*
 * So, the structure for a database table, NB this is only
 * interesting in db.c.
 */
union db_table {
    DbTableCommon common; /* Any type of db table */
    DbTableHash hash;     /* Linear hash array specific data */
    DbTableTree tree;     /* AVL tree specific data */
    /*TT*/
};

/* This should be a prime number. Find them with super:/usr/games/prime */
#define DB_DEF_MAX_TABS 2053 /* Superseeded by environment variable 
				"ERL_MAX_ETS_TABLES" */
#define ERL_MAX_ETS_TABLES_ENV "ERL_MAX_ETS_TABLES"

void init_db(void);
void db_proc_dead(Eterm pid);
void db_info(int, void *, int);
void erts_db_foreach_table(void (*)(DbTable *, void *), void *);
void erts_db_foreach_offheap(DbTable *,
			     void (*func)(ErlOffHeap *, void *),
			     void *);

extern int user_requested_db_max_tabs; /* set in erl_init */
extern int erts_ets_realloc_always_moves;  /* set in erl_init */
extern Export ets_select_delete_continue_exp;
extern Export ets_select_count_continue_exp;
extern Export ets_select_continue_exp;

Eterm erts_ets_slot_to_atom(Uint slot);

#endif

#if defined(ERTS_WANT_DB_INTERNAL__) && !defined(ERTS_HAVE_DB_INTERNAL__)
#define ERTS_HAVE_DB_INTERNAL__

#include "erl_alloc.h"

extern erts_smp_atomic_t erts_tot_ets_memory_size; /* NOTE: Memory size in bytes! */

/*
 * _fnf : Failure Not Fatal (same as for erts_alloc/erts_realloc/erts_free)
 * _nt  : No Table (i.e. memory not associated with a specific table)
 */

#define ERTS_DB_ALC_MEM_UPDATE_(TAB, FREE_SZ, ALLOC_SZ)			\
do {									\
    long sz__ = ((long) (ALLOC_SZ)) - ((long) (FREE_SZ));		\
    erts_smp_atomic_add(&erts_tot_ets_memory_size, sz__);		\
    ASSERT((TAB));							\
    erts_smp_atomic_add(&(TAB)->common.memory_size, sz__);		\
} while (0)

#define ERTS_DB_ALC_MEM_UPDATE_NT_(FREE_SZ, ALLOC_SZ)			\
do {									\
    long sz__ = ((long) (ALLOC_SZ)) - ((long) (FREE_SZ));		\
    erts_smp_atomic_add(&erts_tot_ets_memory_size, sz__);		\
} while (0)

ERTS_GLB_INLINE void *erts_db_alloc(ErtsAlcType_t type,
				    DbTable *tab,
				    Uint size);
ERTS_GLB_INLINE void *erts_db_alloc_fnf(ErtsAlcType_t type,
					DbTable *tab,
					Uint size);
ERTS_GLB_INLINE void *erts_db_alloc_nt(ErtsAlcType_t type, Uint size);
ERTS_GLB_INLINE void *erts_db_alloc_fnf_nt(ErtsAlcType_t type, Uint size);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_db_alloc(ErtsAlcType_t type, DbTable *tab, Uint size)
{
    void *res = erts_alloc(type, size);
    ERTS_DB_ALC_MEM_UPDATE_(tab, 0, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_alloc_fnf(ErtsAlcType_t type, DbTable *tab, Uint size)
{
    void *res = erts_alloc_fnf(type, size);
    if (!res)
	return NULL;
    ERTS_DB_ALC_MEM_UPDATE_(tab, 0, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_alloc_nt(ErtsAlcType_t type, Uint size)
{
    void *res = erts_alloc(type, size);
    ERTS_DB_ALC_MEM_UPDATE_NT_(0, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_alloc_fnf_nt(ErtsAlcType_t type, Uint size)
{
    void *res = erts_alloc_fnf(type, size);
    if (!res)
	return NULL;
    ERTS_DB_ALC_MEM_UPDATE_NT_(0, size);
    return res;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

ERTS_GLB_INLINE void *erts_db_realloc(ErtsAlcType_t type,
				      DbTable *tab,
				      void *ptr,
				      Uint old_size,
				      Uint size);
ERTS_GLB_INLINE void *erts_db_realloc_fnf(ErtsAlcType_t type,
					  DbTable *tab,
					  void *ptr,
					  Uint old_size,
					  Uint size);
ERTS_GLB_INLINE void *erts_db_realloc_nt(ErtsAlcType_t type,
					 void *ptr,
					 Uint old_size,
					 Uint size);
ERTS_GLB_INLINE void *erts_db_realloc_fnf_nt(ErtsAlcType_t type,
					     void *ptr,
					     Uint old_size,
					     Uint size);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void *
erts_db_realloc(ErtsAlcType_t type, DbTable *tab, void *ptr,
		Uint old_size, Uint size)
{
    void *res;
    ASSERT(!ptr || old_size == ERTS_ALC_DBG_BLK_SZ(ptr));
    res = erts_realloc(type, ptr, size);
    ERTS_DB_ALC_MEM_UPDATE_(tab, old_size, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_realloc_fnf(ErtsAlcType_t type, DbTable *tab, void *ptr,
		    Uint old_size, Uint size)
{
    void *res;
    ASSERT(!ptr || old_size == ERTS_ALC_DBG_BLK_SZ(ptr));
    res = erts_realloc_fnf(type, ptr, size);
    if (!res)
	return NULL;
    ERTS_DB_ALC_MEM_UPDATE_(tab, old_size, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_realloc_nt(ErtsAlcType_t type, void *ptr,
		   Uint old_size, Uint size)
{
    void *res;
    ASSERT(!ptr || old_size == ERTS_ALC_DBG_BLK_SZ(ptr));
    res = erts_realloc(type, ptr, size);
    ERTS_DB_ALC_MEM_UPDATE_NT_(old_size, size);
    return res;
}

ERTS_GLB_INLINE void *
erts_db_realloc_fnf_nt(ErtsAlcType_t type, void *ptr,
		       Uint old_size, Uint size)
{
    void *res;
    ASSERT(!ptr || old_size == ERTS_ALC_DBG_BLK_SZ(ptr));
    res = erts_realloc_fnf(type, ptr, size);
    if (!res)
	return NULL;
    ERTS_DB_ALC_MEM_UPDATE_NT_(old_size, size);
    return res;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

ERTS_GLB_INLINE void erts_db_free(ErtsAlcType_t type,
				  DbTable *tab,
				  void *ptr,
				  Uint size);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_db_free(ErtsAlcType_t type, DbTable *tab, void *ptr, Uint size)
{
    ASSERT(ptr != 0);
    ASSERT(size == ERTS_ALC_DBG_BLK_SZ(ptr));
    ERTS_DB_ALC_MEM_UPDATE_(tab, size, 0);

    ASSERT(((void *) tab) != ptr
	   || erts_smp_atomic_read(&tab->common.memory_size) == 0);

    erts_free(type, ptr);
}
#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#undef ERTS_DB_ALC_MEM_UPDATE_NT_
#undef ERTS_DB_ALC_MEM_UPDATE_

#endif /* #if defined(ERTS_WANT_DB_INTERNAL__) && !defined(ERTS_HAVE_DB_INTERNAL__) */

