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


void index_info(to, t)
CIO to; IndexTable* t;
{
    hash_info(to, &t->htable);
    erl_printf(to, "Index Table(%s), ", t->htable.name);
    erl_printf(to, "size(%d), ", t->size);
    erl_printf(to, "limit(%d), ", t->limit);
    erl_printf(to, "used(%d), ", t->sz);
    erl_printf(to, "rate(%d)\n", t->rate);
}


/*
** init a pre allocated or static hash structure
** and allocate buckets.
*/
IndexTable* index_init(t, name, size, limit, rate, fun)
IndexTable* t; char* name; int size; int limit; int rate; HashFunctions fun;
{
    int sz = size*sizeof(IndexSlot*);
    hash_init(&t->htable, name, 3*size/4, fun);

    t->size = size;
    t->limit = limit;
    t->rate = rate;
    t->sz = 0;
    t->is_allocated = 0;
    t->table = (IndexSlot**) sys_alloc_from(120,sz);
    if (t->table == NULL)
	erl_exit(1, "can't allocate index table (%d)\n", sz);
    sys_memzero(t->table, sz);
    return t;
}

/*
** Create a new hash table
*/
IndexTable* index_new(name, size, limit, rate, fun)
char* name; int size; int limit; int rate; HashFunctions fun;
{
    IndexTable* t;

    if ((t = (IndexTable*) sys_alloc_from(121,sizeof(IndexTable))) == (IndexTable*) 0)
	return (IndexTable*) 0;

    index_init(t, name, size, limit, rate, fun);
    t->is_allocated = 1;
    return t;
}

void index_delete(t)
IndexTable* t;
{
    hash_delete(&t->htable);

    sys_free(t->table);
    if (t->is_allocated)
	sys_free(t);
}


int index_put(t, tmpl)
IndexTable* t; void* tmpl;
{
    int ix;
    IndexSlot* p = (IndexSlot*) hash_put(&t->htable, tmpl);
    IndexSlot* q = (IndexSlot*) tmpl;

    if (p->index >= 0) {
	if (q->index >= 0 && q->index != p->index)
	    erl_exit(1, "index in %s already installed (%d != %d)\n",
		     t->htable.name, q->index, p->index);
	return p->index;
    }

    if (q->index >= 0)
	ix = q->index;
    else
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
	    erl_exit(1, "panic : no more index entries in %s (max=%d)\n",
		     t->htable.name, t->limit);
	t->table = (IndexSlot**) sys_realloc(t->table, 
					     sz*sizeof(IndexSlot*));
	if (t->table == NULL)
	    erl_exit(1, "can't allocate %s (%d)\n", t->htable.name, sz);
	sys_memzero(t->table+t->size,
		    (sz - t->size)*sizeof(void*));
	t->size = sz;
    }

    p->index = ix;
    t->table[ix] = p;
    t->sz = ix+1;
    return ix;
}


int index_get(t, tmpl)
IndexTable* t; void* tmpl;
{
    IndexSlot* p = (IndexSlot*) hash_get(&t->htable, tmpl);

    if (p != NULL)
	return p->index;
    return -1;
}


int index_erase(t, tmpl)
IndexTable* t; void* tmpl;
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
int index_iter(t, prev)
IndexTable* t;
int prev;
{
   int i = prev;

   while (++i < t->size)
   {
      if (t->table[i] != NULL)
	 return i;
   }
   return -1;
}
