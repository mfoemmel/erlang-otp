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

#include "erl_db_util.h" /* DbTerm */

typedef struct tree_db_term {
    struct  tree_db_term *left, *right;  /* left and right child */
    int  balance;                        /* tree balancing value */
    DbTerm dbterm;                       /* The actual term */
} TreeDbTerm;

typedef struct db_table_tree {
    eTerm  owner;             /* Pid of the creator */
    eTerm  the_name;          /* an atom   */
    eTerm  id;                /* atom | integer */
    uint32 status;            /* bit masks defining type etc */
    int slot;                 /* slot in db_tables */
    int keypos;               /* defaults to 1 */
    int nitems;               /* Total number of items */

    TreeDbTerm *root;         /* The tree root */
    TreeDbTerm **stack;       /* The first/next stack */
    uint32 stack_pos;         /* Current position on stack */
    uint32 slot_pos;          /* Current "slot" */
} DbTableTree;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_tree(void);
int db_create_tree(Process *p, DbTableTree *tb /* [in out] */);
int db_first_tree(Process *p, DbTableTree *tb /* [in out] */, 
		  eTerm *ret /* [out] */);
int db_next_tree(Process *p, DbTableTree *tb /* [in out] */, 
		 eTerm key /* [in] */,
		 eTerm *ret /* [out] */);
int db_last_tree(Process *p, DbTableTree *tb /* [in out] */, 
		  eTerm *ret /* [out] */);
int db_prev_tree(Process *p, DbTableTree *tb /* [in out] */, 
		 eTerm key /* [in] */,
		 eTerm *ret /* [out] */);
int db_update_counter_tree(Process *p, DbTableTree *tb /* [in out] */, 
			   eTerm key /* [in] */,
			   eTerm incr, /* [in] */
			   int counterpos, /* [in] */
			   eTerm *ret /* [out] */);
int db_put_tree(Process *p, DbTableTree *tb /* [in out] */, 
		eTerm obj /* [in] */,
		eTerm *ret /* [out] */);
int db_get_tree(Process *p, DbTableTree *tb /* [in out] */, 
		eTerm key /* [in] */,
		eTerm *ret /* [out] */);
int db_get_element_tree(Process *p, DbTableTree *tb /* [in out] */, 
			eTerm key /* [in] */,
			int ndex, /* [in] */
			eTerm *ret /* [out] */);
int db_erase_tree(Process *p, DbTableTree *tb /* [in out] */, 
		  eTerm key /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_erase_tree(Process *p, DbTableTree *tb /* [in out] */, 
			eTerm pattern /* [in] */,
			eTerm *ret /* [out] */);
int db_slot_tree(Process *p, DbTableTree *tb /* [in out] */, 
		  eTerm slot_term /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_tree(Process *p, DbTableTree *tb /* [in out] */, 
		  eTerm pattern /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_object_tree(Process *p, DbTableTree *tb /* [in out] */, 
			 eTerm pattern /* [in] */,
			 eTerm state /* [in] */,
			 eTerm *ret /* [out] */);
int db_select_tree(Process *p, DbTableTree *tb /* [in out] */, 
		   eTerm pattern /* [in] */,
		   eTerm *ret /* [out] */);

int  db_info_memory_tree(Process *p, DbTableTree *tb /* [in out] */,
			 eTerm *ret /* [out] */, 
			 int *reds /* [out] */);
void db_print_tree(CIO fd /* [in] */, 
		   int show /* [in] */,
		   DbTableTree *tb /* [in] */, 
		   int *sum /* [out] */);
void free_tree_table(DbTableTree *tb /* [in out] */);
#ifdef HARDDEBUG
void db_check_table_tree(DbTableTree *tb);
#endif

#endif /* _DB_TREE_H */
