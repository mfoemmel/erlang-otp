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
    uint32  hvalue;        /* stored hash value */
    DbTerm dbterm;         /* The actual term */
} HashDbTerm;

typedef struct db_table_hash {
    eTerm  owner;             /* Pid of the creator */
    eTerm  the_name;          /* an atom   */
    eTerm  id;                /* atom | integer */
    uint32 status;            /* bit masks defining type etc */
    int slot;                 /* slot in db_tables */
    int keypos;               /* defaults to 1 */
    int nitems;               /* Total number of items */

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
int db_fixtable_hash(Process *p, DbTableHash *tb /* [in out] */, eTerm arg);
int db_create_hash(Process *p, DbTableHash *tb /* [in out] */);
int db_first_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  eTerm *ret /* [out] */);
int db_next_hash(Process *p, DbTableHash *tb /* [in out] */, 
		 eTerm key /* [in] */,
		 eTerm *ret /* [out] */);
int db_update_counter_hash(Process *p, DbTableHash *tb /* [in out] */, 
			   eTerm key /* [in] */,
			   eTerm incr, /* [in] */
			   int counterpos, /* [in] */
			   eTerm *ret /* [out] */);
int db_put_hash(Process *p, DbTableHash *tb /* [in out] */, 
		eTerm obj /* [in] */,
		eTerm *ret /* [out] */);
int db_get_hash(Process *p, DbTableHash *tb /* [in out] */, 
		eTerm key /* [in] */,
		eTerm *ret /* [out] */);
int db_get_element_array(DbTableHash *tb, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret); /* Interface for meta pid table */
int db_get_element_hash(Process *p, DbTableHash *tb /* [in out] */, 
			eTerm key /* [in] */,
			int ndex, /* [in] */
			eTerm *ret /* [out] */);
int db_erase_bag_exact2(DbTableHash *tb, 
			Eterm key,
			Eterm value); /* Internal interface for meta PID 
					 table */
int db_erase_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  eTerm key /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_erase_hash(Process *p, DbTableHash *tb /* [in out] */, 
			eTerm pattern /* [in] */,
			eTerm *ret /* [out] */);
int db_slot_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  eTerm slot_term /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_hash(Process *p, DbTableHash *tb /* [in out] */, 
		  eTerm pattern /* [in] */,
		  eTerm *ret /* [out] */);
int db_match_object_hash(Process *p, DbTableHash *tb /* [in out] */, 
			 eTerm pattern /* [in] */,
			 eTerm state /* [in] */,
			 eTerm *ret /* [out] */);
int db_select_hash(Process *p, DbTableHash *tb /* [in out] */, 
		   eTerm pattern /* [in] */,
		   eTerm *ret /* [out] */);


int  db_info_memory_hash(Process *p, DbTableHash *tb /* [in out] */,
			 eTerm *ret /* [out] */, 
			 int *reds /* [out] */);
void db_print_hash(CIO fd /* [in] */, 
		   int show /* [in] */,
		   DbTableHash *tb /* [in] */, 
		   int *sum /* [out] */);
void free_hash_table(DbTableHash *tb /* [in out] */);
#ifdef HARDDEBUG
void db_check_table_hash(DbTableHash *tb);
#endif

#endif /* _DB_HASH_H */
