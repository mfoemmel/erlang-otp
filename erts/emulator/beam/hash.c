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
 *     $Id $
 */
/*
** General hash functions
**
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"

/*
** List of sizes (all are primes)
*/
static const int h_size_table[] = {
    2, 5, 11, 23, 47, 97, 197, 397, 797,  /* double upto here */
    1201,   1597,
    2411,   3203, 
    4813,   6421,
    9643,   12853, 
    19289,  25717,
    51437,
    102877,
    205759,
    411527,
    823117,
    1646237,
    3292489,
    6584983,
    13169977,
    26339969,
    52679969, 
    -1 
};

/*
** Get info about hash
**
*/

void hash_get_info(HashInfo *hi, Hash *h)
{
    int size = h->size;
    int i;
    int max_depth = 0;
    int objects = 0;

    for (i = 0; i < size; i++) {
	int depth = 0;
	HashBucket* b = h->bucket[i];
	
	while (b != (HashBucket*) 0) {
	    objects++;
	    depth++;
	    b = b->next;
	}
	if (depth > max_depth)
	    max_depth = depth;
    }

    hi->name  = h->name;
    hi->size  = h->size;
    hi->used  = h->used;
    hi->objs  = objects;
    hi->depth = max_depth;
    
}

/*
** Display info about hash
**
*/

void hash_info(CIO to, Hash* h)
{
    HashInfo hi;

    hash_get_info(&hi, h);

    erl_printf(to, "Hash Table(%s), ", hi.name);
    erl_printf(to, "size(%d), ",       hi.size);
    erl_printf(to, "used(%d), ",       hi.used);
    erl_printf(to, "objs(%d), ",       hi.objs);
    erl_printf(to, "depth(%d)\n",      hi.depth);
}


/*
 * Returns size of table in bytes. Stored objects not included.
 */
int 
hash_table_sz(Hash *h)
{
  int i;
  for(i=0;h->name[i];i++);
  i++;
  return sizeof(Hash) + h->size*sizeof(HashBucket*) + i;
}


/*
** init a pre allocated or static hash structure
** and allocate buckets.
*/
#ifdef INSTRUMENT
Hash* hash_init_from(int from_buckets, Hash* h, char* name, int size,
		     HashFunctions fun)
#else
Hash* hash_init(Hash* h, char* name, int size, HashFunctions fun)
#define from_buckets (-1)
#endif
{
    int sz;
    int ix = 0;

#ifdef INSTRUMENT
    h->from_buckets = from_buckets;
#endif

    while (h_size_table[ix] != -1 && h_size_table[ix] < size)
	ix++;
    if (h_size_table[ix] == -1)
	erl_exit(1, "panic: too large hash table size (%d)\n", size);

    size = h_size_table[ix];
    sz = size*sizeof(HashBucket*);

    if ((h->bucket = (HashBucket**) sys_alloc_from(from_buckets, sz)) == NULL)
	erl_exit(1, "can't allocate hash buckets (%d)\n", sz);

    sys_memzero(h->bucket, sz);
    h->is_allocated = 0;
    h->name = name;
    h->fun = fun;
    h->size = size;
    h->size20percent = h->size/5;
    h->size80percent = (4*h->size)/5;
    h->ix = ix;
    h->used = 0;
    return h;
#undef from_buckets
}

/*
** Create a new hash table
*/
#ifdef INSTRUMENT
Hash* hash_new_from(int from_table, int from_buckets, char* name, int size,
		    HashFunctions fun)
#else
Hash* hash_new(char* name, int size, HashFunctions fun)
#define from_table (-1)
#define from_buckets (-1)
#endif
{
    Hash* h;

    if ((h = (Hash*) sys_alloc_from(from_table, sizeof(Hash))) == (Hash*) 0)
	return (Hash*) 0;

    h = hash_init_from(from_buckets, h, name, size, fun);
    h->is_allocated =  1;
    return h;
#undef from_table
#undef from_buckets
}

/*
** Delete hash table and all objects
*/
void hash_delete(Hash* h)
{
    int old_size = h->size;
    int i;

    for (i = 0; i < old_size; i++) {
	HashBucket* b = h->bucket[i];
	while (b != (HashBucket*) 0) {
	    HashBucket* b_next = b->next;
	    
	    h->fun.free((void*) b);
	    b = b_next;
	}
    }
    sys_free(h->bucket);
    if (h->is_allocated)
	sys_free((void*) h);
}

/*
** Rehash all objects
*/
static void rehash(Hash* h, int grow)
{
    int sz;
    int old_size = h->size;
    HashBucket** new_bucket;
    int i;
#ifdef INSTRUMENT
    int from_buckets = h->from_buckets;
#else
#define from_buckets (-1)
#endif

    if (grow) {
	if ((h_size_table[h->ix+1]) == -1)
	    return;
	h->ix++;
    }
    else {
	if (h->ix == 0)
	    return;
	h->ix--;
    }
    h->size = h_size_table[h->ix];
    h->size20percent = h->size/5;
    h->size80percent = (4*h->size)/5;
    sz = h->size*sizeof(HashBucket*);

    if ((new_bucket = (HashBucket**) sys_alloc_from(from_buckets,sz)) == NULL)
	erl_exit(1, "can't allocate hash buckets (%d)\n", sz);
    sys_memzero(new_bucket, sz);

    h->used = 0;

    for (i = 0; i < old_size; i++) {
	HashBucket* b = h->bucket[i];
	while (b != (HashBucket*) 0) {
	    HashBucket* b_next = b->next;
	    int ix = b->hvalue % h->size;
	    if (new_bucket[ix] == NULL)
		h->used++;
	    b->next = new_bucket[ix];
	    new_bucket[ix] = b;
	    b = b_next;
	}
    }
    sys_free(h->bucket);
    h->bucket = new_bucket;
#undef from_buckets
}

/*
** Find an object in the hash table
**
*/
void* hash_get(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];
	
    while(b != (HashBucket*) 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0))
	    return (void*) b;
	b = b->next;
    }
    return (void*) 0;
}

/*
** Find or insert an object in the hash table
*/
void* hash_put(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];

    while(b != (HashBucket*) 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0))
	    return (void*) b;
	b = b->next;
    }
    b = (HashBucket*) h->fun.alloc(tmpl);

    if (h->bucket[ix] == NULL)
	h->used++;

    b->hvalue = hval;
    b->next = h->bucket[ix];
    h->bucket[ix] = b;

    if (h->used > h->size80percent)  /* rehash at 80% */
	rehash(h, 1);
    return (void*) b;
}
/*
** Erase hash entry return template if erased
** return 0 if not erased
*/
void* hash_erase(Hash* h, void* tmpl)
{
    HashValue hval = h->fun.hash(tmpl);
    int ix = hval % h->size;
    HashBucket* b = h->bucket[ix];
    HashBucket* prev = 0;
	
    while(b != 0) {
	if ((b->hvalue == hval) && (h->fun.cmp(tmpl, (void*)b) == 0)) {
	    if (prev != 0)
		prev->next = b->next;
	    else
		h->bucket[ix] = b->next;
	    h->fun.free((void*)b);
	    if (h->bucket[ix] == NULL)
		h->used--;
	    if (h->used < h->size20percent)  /* rehash at 20% */
		rehash(h, 0);
	    return tmpl;
	}
	prev = b;
	b = b->next;
    }
    return (void*)0;
}

void hash_foreach(Hash* h, void (*func)(void *, void *), void *func_arg2)
{
    int i;

    for (i = 0; i < h->size; i++) {
	HashBucket* b = h->bucket[i];
	while(b != (HashBucket*) 0) {
	    (*func)((void *) b, func_arg2);
	    b = b->next;
	}
    }
}

