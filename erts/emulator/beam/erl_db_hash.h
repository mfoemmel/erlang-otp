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
#ifndef _DB_HASH_H
#define _DB_HASH_H

#include "erl_db_util.h" /* DbTerm */

typedef struct fixed_deletion {
    int slot;
    struct fixed_deletion *next;
} FixedDeletion;

typedef struct hash_db_term {
    struct  hash_db_term* next;  /* next bucket */
    HashValue  hvalue;        /* stored hash value */
    DbTerm dbterm;         /* The actual term */
} HashDbTerm;

typedef struct db_table_hash {
    Eterm  owner;             /* Pid of the creator */
    Eterm  the_name;          /* an atom   */
    Eterm  id;                /* atom | integer */
    Uint32 status;            /* bit masks defining type etc */
    int slot;                 /* slot in db_tables */
    int keypos;               /* defaults to 1 */
    int nitems;               /* Total number of items */
    Uint memory;               /* Total memory size */
    int kept_items;           /* Number of kept elements due to fixation */
    Uint megasec,sec,microsec; /* Last fixation time */
    DbFixation *fixations;     /* List of processes who have fixed 
				  the table */

    FixedDeletion *fixdel; /* List of slots where elements have
			      been deleted while table is fixed. */
    int szm;          /* current size mask */
    int nactive;      /* Number of "active" slots */
    int nslots;       /* Total number of slots */
    int p;            /* Split position */
    int nsegs;        /* Number of segments */
    HashDbTerm ***seg; /* The actual table */
} DbTableHash;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_hash(void);
int db_fixtable_hash(DbTableHash *tb /* [in out] */, Eterm arg);
int db_create_hash(Process *p, DbTableHash *tb /* [in out] */);
int db_first_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  Eterm *ret /* [out] */);
int db_next_hash(Process *p, DbTableHash *tb /* [in out] */, 
		 Eterm key /* [in] */,
		 Eterm *ret /* [out] */);
int db_update_counter_hash(Process *p, DbTableHash *tb /* [in out] */, 
			   Eterm key /* [in] */,
			   Eterm incr, /* [in] */
			   int warp, /* [in] */
			   int counterpos, /* [in] */
			   Eterm *ret /* [out] */);
int db_put_hash(Process *p, DbTableHash *tb /* [in out] */, 
		Eterm obj /* [in] */,
		Eterm *ret /* [out] */);
int db_get_hash(Process *p, DbTableHash *tb /* [in out] */, 
		Eterm key /* [in] */,
		Eterm *ret /* [out] */);
int db_member_hash(Process *p, DbTableHash *tb /* [in out] */, 
		   Eterm key /* [in] */,
		   Eterm *ret /* [out] */);
int db_get_element_array(DbTableHash *tb, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret); /* Interface for meta pid table */
int db_get_element_hash(Process *p, DbTableHash *tb /* [in out] */, 
			Eterm key /* [in] */,
			int ndex, /* [in] */
			Eterm *ret /* [out] */);
int db_erase_bag_exact2(DbTableHash *tb, 
			Eterm key,
			Eterm value); /* Internal interface for meta PID 
					 table */
int db_mark_all_deleted_hash(DbTableHash *tb /* [in out] */);
int db_erase_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  Eterm key /* [in] */,
		  Eterm *ret /* [out] */);
int db_erase_object_hash(Process *p, DbTableHash *tb /* [in out] */, 
			 Eterm object /* [in] */,
			 Eterm *ret /* [out] */);
int db_slot_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  Eterm slot_term /* [in] */,
		  Eterm *ret /* [out] */);
int db_select_hash(Process *p, DbTableHash *tb /* [in out] */, 
		   Eterm pattern /* [in] */,
		   Sint chunk_size /* [in] */,
		   Eterm *ret /* [out] */);
int db_select_count_hash(Process *p, DbTableHash *tb /* [in out] */, 
			 Eterm pattern /* [in] */,
			 Eterm *ret /* [out] */);
int db_select_delete_hash(Process *p, DbTableHash *tb /* [in out] */, 
			  Eterm pattern /* [in] */,
			  Eterm *ret /* [out] */);

int db_select_hash_continue(Process *p, DbTableHash *tb /* [in out] */, 
			    Eterm continuation /* [in] */,
			    Eterm *ret /* [out] */);

int db_select_count_hash_continue(Process *p, 
				  DbTableHash *tb /* [in out] */, 
				  Eterm continuation /* [in] */,
				  Eterm *ret /* [out] */);

int db_select_delete_continue_hash(Process *p ,/* [in out] */ 
				   DbTableHash *tb /* [in out] */,
				   Eterm continuation /* [in] */,
				   Eterm *ret /* [out] */,
				   int force_delete /* [in] */);
void db_print_hash(CIO fd /* [in] */, 
		   int show /* [in] */,
		   DbTableHash *tb /* [in] */);
void free_hash_table(DbTableHash *tb /* [in out] */);

void erts_db_hash_foreach_offheap(DbTableHash *,
				  void (*)(ErlOffHeap *, void *),
				  void *);

#ifdef HARDDEBUG
void db_check_table_hash(DbTableHash *tb);
#endif

#endif /* _DB_HASH_H */
