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


#define ERTS_DB_MORE_MEM(Sz) 					\
  (erts_tot_ets_memory_words += (Sz))
#define ERTS_DB_LESS_MEM(Sz) 					\
  (ASSERT_EXPR(erts_tot_ets_memory_words >= (Sz)),		\
   erts_tot_ets_memory_words -= (Sz))

#define ERTS_DB_TAB_MORE_MEM(Tab, Sz)				\
  (ERTS_DB_MORE_MEM((Sz)),					\
   ((DbTableCommon *) (Tab))->memory += (Sz))
#define ERTS_DB_TAB_LESS_MEM(Tab, Sz)				\
  (ERTS_DB_LESS_MEM((Sz)),					\
   ASSERT_EXPR(((DbTableCommon *) (Tab))->memory >= (Sz)),	\
   ((DbTableCommon *) (Tab))->memory -= (Sz))

extern Uint erts_tot_ets_memory_words;

/*
 * So, the structure for a database table, NB this is only
 * interesting in db.c.
 */
typedef union db_table {
    DbTableCommon common; /* Any type of db table */
    DbTableHash hash;     /* Linear hash array specific data */
    DbTableTree tree;     /* AVL tree specific data */
    /*TT*/
} DbTable;

/* This should be a prime number. Find them with super:/usr/games/prime */
#define DB_DEF_MAX_TABS 2053 /* Superseeded by environment variable 
				"ERL_MAX_ETS_TABLES" */
#define ERL_MAX_ETS_TABLES_ENV "ERL_MAX_ETS_TABLES"

EXTERN_FUNCTION(void, init_db, (_VOID_));
void init_db(void);
void db_proc_dead(Eterm pid);
void db_info(CIO, int);
DbTable* db_get_table(Process *p, Eterm id, int what);
void erts_db_foreach_table(void (*)(DbTable *, void *), void *);
void erts_db_foreach_offheap(DbTable *,
			     void (*func)(ErlOffHeap *, void *),
			     void *);

extern int user_requested_db_max_tabs; /* set in erl_init */
extern Export ets_select_delete_continue_exp;
extern Export ets_select_count_continue_exp;
extern Export ets_select_continue_exp;
#endif

