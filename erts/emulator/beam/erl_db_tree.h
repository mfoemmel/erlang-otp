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
#ifndef _DB_TREE_H
#define _DB_TREE_H

#include "erl_db_util.h"

typedef struct tree_db_term {
    struct  tree_db_term *left, *right;  /* left and right child */
    int  balance;                        /* tree balancing value */
    DbTerm dbterm;                       /* The actual term */
} TreeDbTerm;

typedef struct db_table_tree {
    DbTableCommon common;

    /* Tree-specific fields */
    TreeDbTerm *root;         /* The tree root */
    TreeDbTerm **stack;       /* The first/next stack */
    Uint stack_pos;           /* Current position on stack */
    Uint slot_pos;            /* Current "slot" */
    Uint deletion;		/* Being deleted */
} DbTableTree;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_tree(void);

int db_create_tree(Process *p, DbTable *tbl);

#endif /* _DB_TREE_H */
