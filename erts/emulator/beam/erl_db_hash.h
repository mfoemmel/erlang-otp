/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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

#ifndef _DB_HASH_H
#define _DB_HASH_H

#include "erl_db_util.h" /* DbTerm & DbTableCommon */

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
    DbTableCommon common;

    /* Hash-specific fields */
    FixedDeletion *fixdel;	/*
				 * List of slots where elements have
				 * been deleted while table is fixed.
				 */
    HashDbTerm ***seg;		/* The actual table */

    /* Hash-specific fields, 32-bit quantities */
    int szm;          /* current size mask */
    int nactive;      /* Number of "active" slots */
    int nslots;       /* Total number of slots */
    int p;            /* Split position */
    int nsegs;        /* Number of segments */
} DbTableHash;

/*
** Function prototypes, looks the same (except the suffix) for all 
** table types. The process is always an [in out] parameter.
*/
void db_initialize_hash(void);
void db_unfix_table_hash(DbTableHash *tb /* [in out] */);

/* Interface for meta pid table */
int db_create_hash(Process *p, 
		   DbTable *tbl /* [in out] */);

int db_put_hash(Process *p, DbTable *tbl, Eterm obj, Eterm *ret);

int db_get_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int db_erase_hash(Process *p, DbTable *tbl, Eterm key, Eterm *ret);

int db_get_element_array(DbTable *tbl, 
			 Eterm key,
			 int ndex, 
			 Eterm *ret,
			 int *num_ret); 

int db_erase_bag_exact2(DbTable *tbl, Eterm key, Eterm value);

/* not yet in method table */
int db_mark_all_deleted_hash(DbTable *tbl);


#endif /* _DB_HASH_H */
