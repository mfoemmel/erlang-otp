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

typedef struct {
  char *name;
  int   size;
  int   used;
  int   objs;
  int   depth;
} HashInfo;

typedef struct hash
{
    HashFunctions fun;   /* Function block */
    int is_allocated;    /* 0 iff hash structure is on stack or is static */
    char* name;          /* Table name (static string, for debugging) */
    int size;		 /* Number of slots */
    int size20percent;   /* 20 percent of number of slots */
    int size80percent;   /* 80 percent of number of slots */
    int ix;              /* Size index in size table */
    int used;		 /* Number of slots used */
    HashBucket** bucket; /* Vector of bucket pointers (objects) */
#ifdef INSTRUMENT
    int from_buckets;
#endif
} Hash;

#ifdef INSTRUMENT
#define hash_new(N, S, F) hash_new_from(111, 110, (N), (S), (F))
#define hash_init(H, N, S, F) hash_init_from(110, (H), (N), (S), (F))
Hash* hash_new_from(int, int, char*, int, HashFunctions);
Hash* hash_init_from(int, Hash*, char*, int, HashFunctions);
#else
#define hash_new_from(FT, FB, N, S, F) hash_new((N), (S), (F))
#define hash_init_from(FB, H, N, S, F) hash_init((H), (N), (S), (F))
Hash* hash_new(char*, int, HashFunctions);
Hash* hash_init(Hash*, char*, int, HashFunctions);
#endif

void  hash_delete(Hash*);
void  hash_get_info(HashInfo*, Hash*);
void  hash_info(CIO, Hash*);
int   hash_table_sz(Hash *);

void* hash_get(Hash*, void*);
void* hash_put(Hash*, void*);
void* hash_erase(Hash*, void*);
void  hash_foreach(Hash*, void (*func)(void *, void *), void *);

#endif
