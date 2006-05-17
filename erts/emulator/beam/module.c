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
#include "module.h"

#define MODULE_SIZE   50
#define MODULE_LIMIT  64*1024
#define MODULE_RATE   10

static IndexTable module_table;

#include "erl_smp.h"

static erts_smp_rwmtx_t module_table_lock;

#define module_read_lock()	erts_smp_rwmtx_rlock(&module_table_lock)
#define module_read_unlock()	erts_smp_rwmtx_runlock(&module_table_lock)
#define module_write_lock()	erts_smp_rwmtx_rwlock(&module_table_lock)
#define module_write_unlock()	erts_smp_rwmtx_rwunlock(&module_table_lock)
#define module_init_lock()	erts_smp_rwmtx_init(&module_table_lock, \
						    "module_tab")

void module_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	module_read_lock();
    index_info(to, to_arg, &module_table);
    if (lock)
	module_read_unlock();
}


static HashValue module_hash(Module* x)
{
    return (HashValue) x->module;
}


static int module_cmp(Module* tmpl, Module* obj)
{
    if (tmpl->module == obj->module)
	return 0;
    return 1;
}


static Module* module_alloc(Module* tmpl)
{
    Module* obj = (Module*) erts_alloc(ERTS_ALC_T_MODULE, sizeof(Module));

    obj->module = tmpl->module;
    obj->code = 0;
    obj->old_code = 0;
    obj->code_length = 0;
    obj->old_code_length = 0;
    obj->slot.index = -1;
    return obj;
}


static void module_free(Module* obj)
{
    erts_free(ERTS_ALC_T_MODULE, (void*) obj);
}


void init_module_table(void)
{
    HashFunctions f;

    module_init_lock();
    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = (HFREE_FUN) module_free;

    index_init(ERTS_ALC_T_MODULE_TABLE, &module_table, "module_code",
	       MODULE_SIZE, MODULE_LIMIT, MODULE_RATE, f);
}

Module*
erts_get_module(Eterm mod)
{
    Module e;
    int index;
    Module *ret;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    module_read_lock();
    index = index_get(&module_table, (void*) &e);
    if (index == -1) {
	ret = NULL;
    } else {
	ret = (Module*)module_table.table[index];
    }
    module_read_unlock();
    return ret;
}

Module*
erts_put_module(Eterm mod)
{
    Module e;
    int index;
    Module *ret;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    module_write_lock();
    index = index_put(&module_table, (void*) &e);
    ret = (Module*)module_table.table[index];
    module_write_unlock();
    return ret;
}

Module *module_code(int i)
{
    Module *ret;
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	module_read_lock();
    ret = (Module*)module_table.table[i];
    if (lock)
	module_read_unlock();
    return ret;
}

int module_code_size(void)
{
    int size;
    int lock = !ERTS_IS_CRASH_DUMPING;

    if (lock)
	module_read_lock();
    size = module_table.sz;
    if (lock)
	module_read_unlock();
    return size;
}

int module_table_sz(void)
{
    int sz;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	module_read_lock();
    sz = index_table_sz(&module_table);
    if (lock)
	module_read_unlock();
    return sz;
}

int module_iter(int i)
{
    int index;

    module_read_lock();
    index = index_iter(&module_table, i);
    module_read_unlock();
    return index;
}
