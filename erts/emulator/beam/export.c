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
IndexTable export_table;

extern Eterm* em_call_error_handler;
extern Uint* em_call_traced_function;

void
export_info(CIO to)
{
    index_info(to, &export_table);
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
    Export* obj = (Export*) fix_alloc(export_desc);

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
    fix_free(export_desc, (void*) obj);
}


void
init_export_table(void)
{
    HashFunctions f;

    f.hash = (H_FUN) export_hash;
    f.cmp  = (HCMP_FUN) export_cmp;
    f.alloc = (HALLOC_FUN) export_alloc;
    f.free = (HFREE_FUN) export_free;

    index_init(&export_table, "export_list",
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
erts_find_export_entry(Eterm m, Eterm f, int a)
{
    HashValue hval = EXPORT_HASH(m, f, a);
    int ix = hval % export_table.htable.size;
    HashBucket* b = export_table.htable.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */
	
    while (b != (HashBucket*) 0) {
	Export* ep = (Export *) b;
	if (ep->code[0] == m && ep->code[1] == f && ep->code[2] == a) {
	    return ep;
	}
	b = b->next;
    }
    return NULL;
}


/*
 * Find the export entry for a loaded function.
 * Returns a NULL pointer if the given function is not loaded, or
 * a pointer to the export entry.
 *
 * Note: This function never return export entries for BIFs
 * or functions which are not yet loaded.  This make it suitable
 * for use by the erlang:function_exported/3 BIF or whenever you
 * cannot depend on the error_handler.
 */

Export*
erts_find_function(Eterm m, Eterm f, int a)
{
    Export e;
    Export* ep;

    e.code[0] = m;
    e.code[1] = f;
    e.code[2] = a;

    if ((ep = hash_get(&export_table.htable, (void*) &e)) == NULL) {
	return NULL;
    }
    if (ep->address == ep->code+3 && ep->code[3] != (Uint) em_call_traced_function) {
	return NULL;
    }
    return ep;
}


/*
 * Returns a pointer to an existing export entry for a MFA,
 * or creates a new one and returns the pointer.
 */

Export*
erts_export_put(Eterm mod, Eterm func, int arity)
{
    Export e;

    ASSERT(is_atom(mod));
    ASSERT(is_atom(func));

    e.code[0] = mod;
    e.code[1] = func;
    e.code[2] = arity;
    e.address = e.code+3;
    e.code[4] = 0;
    e.match_prog_set = NULL;
    return export_list(index_put(&export_table, (void*) &e));
}
