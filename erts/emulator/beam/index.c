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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "index.h"


void index_info(int to, void *arg, IndexTable *t)
{
    hash_info(to, arg, &t->htable);
    erts_print(to, arg, "=index_table:%s\n", t->htable.name);
    erts_print(to, arg, "size: %d\n",	t->size);
    erts_print(to, arg, "limit: %d\n",	t->limit);
    erts_print(to, arg, "used: %d\n",	t->sz);
    erts_print(to, arg, "rate: %d\n",	t->rate);
}


/*
 * Returns size of table in bytes. Stored objects not included.
 */
int 
index_table_sz(IndexTable *t)
{
  return (sizeof(IndexTable)
          - sizeof(Hash)
          + t->size*sizeof(IndexSlot*)
          +  hash_table_sz(&(t->htable)));
}


/*
** init a pre allocated or static hash structure
** and allocate buckets.
*/
IndexTable *index_init(ErtsAlcType_t type, IndexTable* t, char* name,
		       int size, int limit, int rate, HashFunctions fun)
{
    int sz = size*sizeof(IndexSlot*);

    hash_init(type, &t->htable, name, 3*size/4, fun);

    t->size = size;
    t->limit = limit;
    t->rate = rate;
    t->sz = 0;
    t->is_allocated = 0;
    t->type = type;
    t->table = (IndexSlot**) erts_alloc(type, sz);
    sys_memzero(t->table, sz);
    return t;
}

/*
** Create a new hash table
*/
IndexTable *index_new(ErtsAlcType_t type, char* name, int size, int limit,
		      int rate, HashFunctions fun)
{
    IndexTable* t;
    t = erts_alloc(type, sizeof(IndexTable));
    index_init(type, t, name, size, limit, rate, fun);
    t->is_allocated = 1;
    return t;
}

void index_delete(IndexTable* t)
{
    hash_delete(&t->htable);

    erts_free(t->type, t->table);
    if (t->is_allocated)
	erts_free(t->type, t);
}


int
index_put(IndexTable* t, void* tmpl)
{
    int ix;
    IndexSlot* p = (IndexSlot*) hash_put(&t->htable, tmpl);

    if (p->index >= 0) {
	return p->index;
    }

    ix = t->sz;
    if (ix >= t->size) {
	int sz;
	int sz_inc;

	if (t->rate < 0)
	    sz_inc = t->size/-t->rate;
	else
	    sz_inc = t->rate;

	if (sz_inc == 0)
	    sz = t->size + 1;
	else
	    sz = t->size + sz_inc;

	if (ix >= sz)
	    sz = ix + 1;
	
	if (sz >= t->limit)
	    sz = t->limit;
	if (ix >= t->limit)
	    erl_exit(1, "no more index entries in %s (max=%d)\n",
		     t->htable.name, t->limit);
	t->table = (IndexSlot**) erts_realloc(t->type,
					      (void *) t->table, 
					      sz*sizeof(IndexSlot*));
	sys_memzero(t->table+t->size,
		    (sz - t->size)*sizeof(void*));
	t->size = sz;
    }

    p->index = ix;
    t->table[ix] = p;
    t->sz = ix+1;
    return ix;
}


int index_get(IndexTable* t, void* tmpl)
{
    IndexSlot* p = (IndexSlot*) hash_get(&t->htable, tmpl);

    if (p != NULL)
	return p->index;
    return -1;
}


int index_erase(IndexTable* t, void* tmpl)
{
    IndexSlot* p = (IndexSlot*) hash_erase(&t->htable, tmpl);

    if (p != NULL) {
	t->table[p->index] = NULL;
	return p->index;
    }
    return -1;
}

/* Iterates over the used indices in the table. 'prev' should be the
   previously used index, or -1 to start. Returns -1 when finished.
*/
int index_iter(IndexTable* t, int prev)
{
   int i = prev;

   while (++i < t->size)
   {
      if (t->table[i] != NULL)
	 return i;
   }
   return -1;
}
