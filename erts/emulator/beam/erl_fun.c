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
#include "erl_fun.h"
#include "hash.h"

Hash erts_fun_table;

static HashValue fun_hash(ErlFunEntry* obj);
static int fun_cmp(ErlFunEntry* obj1, ErlFunEntry* obj2);
static ErlFunEntry* fun_alloc(ErlFunEntry* template);
static void fun_free(ErlFunEntry* obj);

/*
 * The address field of every fun that has no loaded code will point
 * to unloaded_fun[]. The -1 in unloaded_fun[0] will be interpreted
 * as an illegal arity when attempting to call a fun.
 */
static Eterm unloaded_fun_code[3] = {NIL, -1, 0};
static Eterm* unloaded_fun = unloaded_fun_code + 2;

void
erts_init_fun_table(void)
{
    HashFunctions f;

    f.hash = (H_FUN) fun_hash;
    f.cmp  = (HCMP_FUN) fun_cmp;
    f.alloc = (HALLOC_FUN) fun_alloc;
    f.free = (HFREE_FUN) fun_free;

    hash_init(ERTS_ALC_T_FUN_TABLE, &erts_fun_table, "fun_table", 16, f);
}

void
erts_fun_info(CIO to)
{
    hash_info(to, &erts_fun_table);
}

ErlFunEntry*
erts_put_fun_entry(Eterm mod, int uniq, int index)
{
    ErlFunEntry template;
    ErlFunEntry* fe;

    ASSERT(is_atom(mod));
    template.old_uniq = uniq;
    template.old_index = index;
    template.module = mod;
    fe = (ErlFunEntry *) hash_put(&erts_fun_table, (void*) &template);
    fe->refc++;
    return fe;
}

ErlFunEntry*
erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
		    byte* uniq, int index, int arity)
{
    ErlFunEntry template;
    ErlFunEntry* fe;

    ASSERT(is_atom(mod));
    template.old_uniq = old_uniq;
    template.old_index = old_index;
    template.module = mod;
    fe = (ErlFunEntry *) hash_put(&erts_fun_table, (void*) &template);
    memcpy(fe->uniq, uniq, sizeof(fe->uniq));
    fe->index = index;
    fe->arity = arity;
    fe->refc++;
    return fe;
}

struct my_key {
    Eterm mod;
    byte* uniq;
    int index;
    ErlFunEntry* fe;
};

static void
search(void* b, void* data)
{
    struct my_key* key = (struct my_key *) data;
    ErlFunEntry* fe = (ErlFunEntry *) b;

    if (fe->module == key->mod &&
	fe->index == key->index &&
	memcmp(fe->uniq, key->uniq, 16) == 0) {
	key->fe = fe;
    }
}

ErlFunEntry*
erts_put_debug_fun_entry(Eterm mod, byte* uniq, int index)
{
    struct my_key key;

    key.mod = mod;
    key.uniq = uniq;
    key.index = index;
    key.fe = NULL;
    hash_foreach(&erts_fun_table, search, &key);
    if (key.fe != NULL) {
	key.fe->refc++;
    }
    return key.fe;
}

ErlFunEntry*
erts_get_fun_entry(Eterm mod, int uniq, int index)
{
    ErlFunEntry template;

    ASSERT(is_atom(mod));
    template.old_uniq = uniq;
    template.old_index = index;
    template.module = mod;
    return (ErlFunEntry *) hash_get(&erts_fun_table, (void*) &template);
}

void
erts_erase_fun_entry(ErlFunEntry* fe)
{
    hash_erase(&erts_fun_table, (void *) fe);
}

#ifndef SHARED_HEAP
void
erts_cleanup_funs(ErlFunThing* funp)
{
    while (funp) {
	ErlFunEntry* fe = funp->fe;
	if (--(fe->refc) == 0) {
	    erts_erase_fun_entry(fe);
	}
	funp = funp->next;
    }
}
#endif

void
erts_cleanup_funs_on_purge(Eterm* start, Eterm* end)
{
    int limit = erts_fun_table.size;
    HashBucket** bucket = erts_fun_table.bucket;
    ErlFunEntry* to_delete = NULL;
    int i;

    for (i = 0; i < limit; i++) {
	HashBucket* b = bucket[i];

	while (b) {
	    ErlFunEntry* fe = (ErlFunEntry *) b;
	    Eterm* addr = fe->address;

	    if (start <= addr && addr < end) {
		fe->address = unloaded_fun;
		fe->refc--;
		if (fe->refc == 0) {
		    fe->address = (void *) to_delete;
		    to_delete = fe;
		}
	    }
	    b = b->next;
	}
    }

    while (to_delete != NULL) {
	ErlFunEntry* next = (ErlFunEntry *) to_delete->address;
	erts_erase_fun_entry(to_delete);
	to_delete = next;
    }
}

void
erts_dump_fun_entries(CIO fd)
{
    int limit = erts_fun_table.size;
    HashBucket** bucket = erts_fun_table.bucket;
    int i;

    for (i = 0; i < limit; i++) {
	HashBucket* b = bucket[i];

	while (b) {
	    ErlFunEntry* fe = (ErlFunEntry *) b;
	    sys_printf(fd, "=fun\n");
	    sys_printf(fd, "Module: ");
	    print_atom(atom_val(fe->module), fd);
	    sys_printf(fd, "\n");
	    sys_printf(fd, "Uniq: %d\n", fe->old_uniq);
	    sys_printf(fd, "Index: %d\n",fe->old_index);
	    sys_printf(fd, "Address: %p\n", fe->address);
#ifdef HIPE
	    sys_printf(fd, "Native_address: %p\n", fe->native_address);
#endif
	    sys_printf(fd, "Refc: %d\n", fe->refc);
	    b = b->next;
	}
    }
}

static HashValue
fun_hash(ErlFunEntry* obj)
{
    return (HashValue) (obj->old_uniq ^ obj->old_index ^ atom_val(obj->module));
}

static int
fun_cmp(ErlFunEntry* obj1, ErlFunEntry* obj2)
{
    return !(obj1->module == obj2->module && 
	     obj1->old_uniq == obj2->old_uniq &&
	     obj1->old_index == obj2->old_index);
}

static ErlFunEntry*
fun_alloc(ErlFunEntry* template)
{
    ErlFunEntry* obj = (ErlFunEntry *) erts_alloc(ERTS_ALC_T_FUN_ENTRY,
						  sizeof(ErlFunEntry));

    obj->old_uniq = template->old_uniq;
    obj->old_index = template->old_index;
    obj->module = template->module;
    obj->refc = 0;
    obj->address = unloaded_fun;
#ifdef HIPE
    obj->native_address = NULL;
#endif
    return obj;
}

static void
fun_free(ErlFunEntry* obj)
{
    erts_free(ERTS_ALC_T_FUN_ENTRY, (void *) obj);
}
