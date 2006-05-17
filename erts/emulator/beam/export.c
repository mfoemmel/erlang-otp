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
#include "export.h"
#include "hash.h"

#define EXPORT_SIZE   500
#define EXPORT_LIMIT  (64*1024)
#define EXPORT_RATE   100

#define EXPORT_HASH(m,f,a) ((m)*(f)+(a))
static IndexTable export_table;

#include "erl_smp.h"

static erts_smp_rwmtx_t export_table_lock;

#define export_read_lock()	erts_smp_rwmtx_rlock(&export_table_lock)
#define export_read_unlock()	erts_smp_rwmtx_runlock(&export_table_lock)
#define export_write_lock()	erts_smp_rwmtx_rwlock(&export_table_lock)
#define export_write_unlock()	erts_smp_rwmtx_rwunlock(&export_table_lock)
#define export_init_lock()	erts_smp_rwmtx_init(&export_table_lock, \
						    "export_tab")


extern Eterm* em_call_error_handler;
extern Uint* em_call_traced_function;

void
export_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_read_lock();
    index_info(to, to_arg, &export_table);
    if (lock)
	export_read_unlock();
}


static HashValue
export_hash(Export* x)
{
    return EXPORT_HASH(x->code[0], x->code[1], x->code[2]);
}

static int
export_cmp(Export* tmpl, Export* obj)
{
    return !(tmpl->code[0] == obj->code[0] &&
	     tmpl->code[1] == obj->code[1] &&
	     tmpl->code[2] == obj->code[2]);
}


static Export*
export_alloc(Export* tmpl)
{
    Export* obj = (Export*) erts_alloc(ERTS_ALC_T_EXPORT, sizeof(Export));
    
    obj->fake_op_func_info_for_hipe[0] = 0;
    obj->fake_op_func_info_for_hipe[1] = 0;
    obj->code[0] = tmpl->code[0];
    obj->code[1] = tmpl->code[1];
    obj->code[2] = tmpl->code[2];
    obj->slot.index = -1;
    obj->address = obj->code+3;
    obj->code[3] = (Eterm) em_call_error_handler;
    obj->code[4] = 0;
    obj->match_prog_set = NULL;
    return obj;
}


static void 
export_free(Export* obj)
{
    erts_free(ERTS_ALC_T_EXPORT,  (void*) obj);
}


void
init_export_table(void)
{
    HashFunctions f;

    export_init_lock();
    f.hash = (H_FUN) export_hash;
    f.cmp  = (HCMP_FUN) export_cmp;
    f.alloc = (HALLOC_FUN) export_alloc;
    f.free = (HFREE_FUN) export_free;

    index_init(ERTS_ALC_T_EXPORT_TABLE, &export_table, "export_list",
	       EXPORT_SIZE, EXPORT_LIMIT, EXPORT_RATE, f);
}

/*
 * Return a pointer to the export entry for the given function,
 * or NULL otherwise.  Notes:
 *
 * 1) BIFs have export entries and can be called through
 *    a wrapper in the export entry.
 * 2) Functions referenced by a loaded module, but not yet loaded
 *    also have export entries.  The export entry contains
 *    a wrapper which invokes the error handler if a function is
 *    called through such an export entry.
 * 3) This function is suitable for the implementation of erlang:apply/3.
 */

Export*
erts_find_export_entry(Eterm m, Eterm f, unsigned int a)
{
    HashValue hval = EXPORT_HASH(m, f, a);
    int ix;
    HashBucket* b;

    export_read_lock();
    ix = hval % export_table.htable.size;
    b = export_table.htable.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */
	
    while (b != (HashBucket*) 0) {
	Export* ep = (Export *) b;
	if (ep->code[0] == m && ep->code[1] == f && ep->code[2] == a) {
	    break;
	}
	b = b->next;
    }
    export_read_unlock();
    return (Export*)b;
}


/*
 * Find the export entry for a loaded function.
 * Returns a NULL pointer if the given function is not loaded, or
 * a pointer to the export entry.
 *
 * Note: This function never returns export entries for BIFs
 * or functions which are not yet loaded.  This makes it suitable
 * for use by the erlang:function_exported/3 BIF or whenever you
 * cannot depend on the error_handler.
 */

Export*
erts_find_function(Eterm m, Eterm f, unsigned int a)
{
    Export e;
    Export* ep;

    e.code[0] = m;
    e.code[1] = f;
    e.code[2] = a;

    export_read_lock();
    ep = hash_get(&export_table.htable, (void*) &e);
    if (ep != NULL && ep->address == ep->code+3 &&
	ep->code[3] != (Uint) em_call_traced_function) {
	ep = NULL;
    }
    export_read_unlock();
    return ep;
}


/*
 * Returns a pointer to an existing export entry for a MFA,
 * or creates a new one and returns the pointer.
 */

Export*
erts_export_put(Eterm mod, Eterm func, unsigned int arity)
{
    Export e;
    int ix;
    Export *ep;
    
    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));
    
    e.fake_op_func_info_for_hipe[0] = 0;
    e.fake_op_func_info_for_hipe[1] = 0;
    e.code[0] = mod;
    e.code[1] = func;
    e.code[2] = arity;
    e.address = e.code+3;
    e.code[4] = 0;
    e.match_prog_set = NULL;
    export_write_lock();
    ix = index_put(&export_table, (void*) &e);
    ep = (Export*)export_table.table[ix];
    export_write_unlock();
    return ep;
}

Export *export_list(int i)
{
    Export *ep;

    export_read_lock();
    ep = (Export*)export_table.table[i];
    export_read_unlock();
    return ep;
}

int export_list_size(void)
{
    int size;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_read_lock();
    size = export_table.sz;
    if (lock)
	export_read_unlock();
    return size;
}

int export_table_sz(void)
{
    int sz;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	export_read_lock();
    sz = index_table_sz(&export_table);
    if (lock)
	export_read_unlock();
    return sz;
}

Export *export_get(Export *e)
{
    Export *ep;

    export_read_lock();
    ep = hash_get(&export_table.htable, e);
    export_read_unlock();
    return ep;
}
