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

IndexTable module_table;

void module_info(to)
CIO to;
{
    index_info(to, &module_table);
}


static HashValue module_hash(x)
Module* x;
{
    return (HashValue) x->module;
}


static int module_cmp(tmpl, obj)
Module* tmpl; Module* obj;
{
    if (tmpl->module == obj->module)
	return 0;
    return 1;
}


static Module* module_alloc(tmpl)
Module* tmpl;
{
    Module* obj = (Module*) fix_alloc(module_desc);

    obj->module = tmpl->module;
    obj->code = 0;
    obj->old_code = 0;
    obj->code_length = 0;
    obj->old_code_length = 0;
    obj->slot.index = -1;
    return obj;
}


static void module_free(obj)
Module* obj;
{
    fix_free(module_desc, (void*) obj);
}


void init_module_table(void)
{
    HashFunctions f;

    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = (HFREE_FUN) module_free;

    index_init(&module_table, "module_code",
	       MODULE_SIZE, MODULE_LIMIT, MODULE_RATE, f);
}

Module*
erts_get_module(Eterm mod)
{
    Module e;
    int index;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    index = index_get(&module_table, (void*) &e);
    if (index == -1) {
	return NULL;
    } else {
	return module_code(index);
    }
}

Module*
erts_put_module(Eterm mod)
{
    Module e;
    int index;

    ASSERT(is_atom(mod));
    e.module = atom_val(mod);
    index = index_put(&module_table, (void*) &e);
    return module_code(index);
}

