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
void db_proc_dead(eTerm pid);
void db_info(CIO, int);
DbTable* db_get_table(Process *p, eTerm id, int what);

extern int user_requested_db_max_tabs; /* set in erl_init */

#endif

