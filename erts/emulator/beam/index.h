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
** General hash and index functions
** The idea behind this file was to capture the
** way Atom,Export and Module table was implemented
*/
#ifndef __INDEX_H__
#define __INDEX_H__

#ifndef __HASH_H__
#include "hash.h"
#endif

typedef struct index_slot 
{
    HashBucket bucket;
    int index;
} IndexSlot;


typedef struct index_table
{
    Hash htable;        /* Mapping obj -> index */
    ErtsAlcType_t type;
    int size;           /* Allocated size */
    int limit;          /* Max size */
    int rate;           /* Factor or increament */
    int sz;             /* Current size */
    int is_allocated;   /* 0 iff not alloacted */
    IndexSlot** table;  /* Mapping index -> obj */
} IndexTable;


IndexTable *index_new(ErtsAlcType_t,char*,int,int,int,HashFunctions);
IndexTable *index_init(ErtsAlcType_t,IndexTable*,char*,int,int,int,HashFunctions);
void index_delete(IndexTable*);
void index_info(CIO,IndexTable*);
int index_table_sz(IndexTable *);

int index_get(IndexTable*, void*);
int index_put(IndexTable*, void*);
int index_erase(IndexTable*, void*);

int index_iter(IndexTable*, int);
#endif

    
