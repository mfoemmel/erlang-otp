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
** General hash functions
**
*/
#ifndef __HASH_H__
#define __HASH_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

typedef unsigned long HashValue;

typedef FUNCTION(int,(*HCMP_FUN),(void*, void*));
typedef FUNCTION(HashValue,(*H_FUN),(void*));
typedef FUNCTION(void*,(*HALLOC_FUN), (void*));
typedef FUNCTION(void,(*HFREE_FUN), (void*));

/*
** This bucket must be placed in top of 
** every object that uses hashing!!!
** (Object*) == (Object*) &bucket
*/
typedef struct hash_bucket
{
    struct hash_bucket* next;	/* Next bucket */
    HashValue hvalue;           /* Store hash value for get, rehash */
} HashBucket;

typedef struct hash_functions
{
    H_FUN hash;
    HCMP_FUN cmp;
    HALLOC_FUN alloc;
    HFREE_FUN free;
} HashFunctions;


typedef struct hash
{
    HashFunctions fun;   /* Function block */
    int is_allocated;    /* 0 iff hash structure is on stack or is static */
    char* name;          /* Table name (static string, for debugging) */
    int size;		 /* Number of slots */
    int ix;              /* Size index in prime table */
    int used;		 /* Number of slots used */
    HashBucket** bucket; /* Vector of bucket pointers (objects) */
} Hash;

EXTERN_FUNCTION(Hash*, hash_new, (char*, int, HashFunctions));
EXTERN_FUNCTION(Hash*, hash_init, (Hash*, char*, int, HashFunctions));
EXTERN_FUNCTION(void,  hash_delete, (Hash*));
EXTERN_FUNCTION(void,  hash_info, (CIO, Hash*));
EXTERN_FUNCTION(int,   hash_table_sz, (Hash *));

EXTERN_FUNCTION(void*, hash_get, (Hash*, void*));
EXTERN_FUNCTION(void*, hash_put, (Hash*, void*));
EXTERN_FUNCTION(void*, hash_erase, (Hash*, void*));

#endif
