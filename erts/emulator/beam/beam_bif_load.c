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
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "beam_load.h"
#include "big.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_binary.h"

Eterm erts_preloaded(Process* p);

static Eterm check_process_code(Process* rp, Module* modp);
static void delete_code(Module* modp);
static void delete_export_references(Eterm module);
static int purge_module(int module);

int erts_match_spec_is_on = 0;
Binary* erts_default_match_spec = NULL;
int erts_match_local_functions = 0;

Eterm
load_module_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm   reason;
    Eterm*  hp;
    int      i;
    int      sz;
    byte*    code;
    
    if (is_not_atom(BIF_ARG_1) || is_not_binary(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    hp = HAlloc(BIF_P, 3);
    GET_BINARY_BYTES(BIF_ARG_2, code);
    sz = binary_size(BIF_ARG_2);
    if ((i = do_load(BIF_P->group_leader, BIF_ARG_1, code, sz)) < 0) { 
	switch (i) {
	case -1: reason = am_badfile; break; 
	case -2: reason = am_nofile; break;
	case -3: reason = am_not_purged; break;
	default: reason = am_badfile; break;
	}
	BIF_RET(TUPLE2(hp, am_error, reason));
    }
    if (erts_match_spec_is_on) {
	Eterm mfa[1] = {BIF_ARG_1};
	(void) erts_set_trace_pattern(mfa, 1, erts_default_match_spec, 1,
				      erts_match_local_functions);
    }
    BIF_RET(TUPLE2(hp, am_module, BIF_ARG_1));
}

BIF_RETTYPE purge_module_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (purge_module(atom_val(BIF_ARG_1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

Eterm
check_process_code_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Process* rp;
    Module* modp;

    if (is_not_atom(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_pid(BIF_ARG_1) || (pid_node(BIF_ARG_1) != THIS_NODE) ||
	(pid_number(BIF_ARG_1) >= max_process)) {
	goto error;
    }
    rp = process_tab[pid_number(BIF_ARG_1)];
    if (INVALID_PID(rp, BIF_ARG_1)) {
	BIF_RET(am_false);
    }
    modp = erts_get_module(BIF_ARG_2);
    return check_process_code(rp, modp);
}


BIF_RETTYPE delete_module_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Module* modp;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    modp = erts_get_module(BIF_ARG_1);
    if (modp == NULL) {
	return am_undefined;
    }
    if (modp->old_code != 0) {
	cerr_pos = 0;
	erl_printf(CBUF, "Module ");
	print_atom(atom_val(BIF_ARG_1), CBUF);
	erl_printf(CBUF, " must be purged before loading\n");
	send_error_to_logger(BIF_P->group_leader);
	goto error;
    }

    delete_export_references(BIF_ARG_1);
    delete_code(modp);
    return am_true;
}

BIF_RETTYPE module_loaded_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Module* modp;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((modp = erts_get_module(BIF_ARG_1)) == NULL || modp->code == NULL) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE pre_loaded_0(BIF_ALIST_0)
BIF_ADECL_0
{
    return erts_preloaded(BIF_P);
}

BIF_RETTYPE loaded_0(BIF_ALIST_0)
BIF_ADECL_0
{
    Eterm previous = NIL;
    Eterm* hp;
    int i;
    int j = 0;
    
    for (i = 0; i < module_code_size; i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    j++;
	}
    }
    if (j > 0) {
	hp = HAlloc(BIF_P, j*2);

	for (i = 0; i < module_code_size; i++) {
	    if (module_code(i) != NULL &&
		((module_code(i)->code_length != 0) ||
		 (module_code(i)->old_code_length != 0))) {
		previous = CONS(hp, make_atom(module_code(i)->module), 
				previous);
		hp += 2;
	    }
	}
    }
    BIF_RET(previous);
}

static int
check_process_funs(Process* rp, Module* modp)
{
    ErlFunThing* funp;
    int done_gc = 0;

 rescan:
    for (funp = rp->off_heap.funs; funp; funp = funp->next) {
	Eterm* code;

	if (funp->modp != modp) { /* Wrong module. */
	    continue;
	}

	/*
	 * Check if this fun is found in the current code.  If it is,
	 * the old code is not needed for this fun.
	 */

	code = modp->code;
	if (code != NULL && funp->index < code[MI_NUM_LAMBDAS]) {
	    Eterm* fun_entry = ((Eterm *) code[MI_LAMBDA_PTR]) + 2*funp->index;
	    if (fun_entry[0] == funp->uniq) {
		continue;
	    }
	}

	/*
	 * If the fun is found in the old code, the old code cannot be safely
	 * purged yet. Unless we have alreday done garbage collection, do
	 * one to possibly get the offending fun thrown out.
	 */

	code = modp->old_code;
	if (code != NULL && funp->index < code[MI_NUM_LAMBDAS]) {
	    Eterm* fun_entry = ((Eterm *) code[MI_LAMBDA_PTR]) + 2*funp->index;
	    if (fun_entry[0] == funp->uniq) {
		if (done_gc) {
		    return am_true;
		} else {
		    done_gc = 1;
		    rp->flags |= F_NEED_FULLSWEEP;
		    (void) erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
		    goto rescan;
		}
	    }
	}
    }
    return am_false;
}

static Eterm
check_process_code(Process* rp, Module* modp)
{
    Eterm* start;
    Eterm* end;
    Eterm* sp;

#define INSIDE(a) (start <= (a) && (a) < end)
    if (modp == NULL) {		/* Doesn't exist. */
	return am_false;
    } else if (modp->old_code == NULL) { /* No old code. */
	return am_false;
    }

    /*
     * Pick up limits for the module.
     */
    start = modp->old_code;
    end = (Eterm *)((char *)start + modp->old_code_length);

    /*
     * Check if current instruction or continuation pointer points into module.
     */
    if (INSIDE(rp->i) || INSIDE(rp->cp)) {
	return am_true;
    }

    /*
     * Check all continuation pointers stored on the stack.
     */
    for (sp = rp->stop; sp < rp->hend; sp++) {
	if (is_CP(*sp) && INSIDE(cp_val(*sp))) {
	    return am_true;
	}
    }

    /*
     * See if there are funs that refer to the old version of the module.
     */
    return check_process_funs(rp, modp);
#undef INSIDE
}


static int
purge_module(int module)
{
    Eterm* code;
    Module* modp;
    int i;

    /*
     * Correct module?
     */

    if ((modp = erts_get_module(make_atom(module))) == NULL) {
	return -2;
    }

    /*
     * Any code to purge?
     */
    if (modp->old_code == 0) {
	if (display_loads) {
	    erl_printf(COUT,"No code to purge for ");
	    print_atom(module, COUT);
	    erl_printf(COUT,"\n");
	}
	return -1;
    }

    if (display_loads) {
	erl_printf(COUT,"Purging code for ");
	print_atom(module, COUT);
	erl_printf(COUT,"\n");
    }

    /*
     * Remove the old code.
     */
    code = modp->old_code;
    beam_catches_delmod(modp->old_catches, code, modp->old_code_length);
    sys_free((char *) code);
    modp->old_code = NULL;
    modp->old_code_length = 0;
    modp->old_catches = BEAM_CATCHES_NIL;

    /*
     * Remove the code from the address table too.
     */
    for (i = 0; i < num_loaded_modules; i++) {
	if (modules[i].start == code) {
	    num_loaded_modules--;
	    while (i < num_loaded_modules) {
		modules[i] = modules[i+1];
		i++;
	    }
	    return 0;
	}
    }

    ASSERT(0);			/* Not found? */
    return 0;
}


/*
 * Move code from current to old.
 */

static void 
delete_code(Module* modp)
{
    /*
     * Clear breakpoints if any
     */
    if (modp->code != NULL && modp->code[MI_NUM_BREAKPOINTS] > 0) {
	erts_clear_module_break(modp);
	modp->code[MI_NUM_BREAKPOINTS] = 0;
    }
    modp->old_code = modp->code;
    modp->old_code_length = modp->code_length;
    modp->old_catches = modp->catches;
    modp->code = NULL;
    modp->code_length = 0;
    modp->catches = BEAM_CATCHES_NIL;
}


/* null all references on the export table for the module called with the
   atom index below */

static void
delete_export_references(uint32 module)
{
    int i;

    ASSERT(is_atom(module));

    for (i = 0; i < export_list_size; i++) {
        if (export_list(i) != NULL && (export_list(i)->code[0] == module)) {
	    if (export_list(i)->address == beam_debug_apply+5) {
		continue;
	    }
	    if (export_list(i)->address == export_list(i)->code+3 &&
		(export_list(i)->code[3] == (Eterm) em_apply_bif)) {
		continue;
	    }
	    export_list(i)->address = export_list(i)->code+3;
	    export_list(i)->code[3] = (Uint) em_call_error_handler;
	    export_list(i)->code[4] = 0;
	    MatchSetUnref(export_list(i)->match_prog_set);
	    export_list(i)->match_prog_set = NULL;
	}
    }
}

int
beam_make_current_old(Eterm module)
{
    Module* modp = erts_put_module(module);

    /*
     * Check if the previous code has been already deleted;
     * if not, delete old code; error if old code already exists.
     */

    if (modp->code != NULL && modp->old_code != NULL)  {
	return -3;
    } else if (modp->old_code == NULL) { /* Make the current version old. */
	if (display_loads) {
	    erl_printf(COUT, "saving old code\n");
	}
	delete_code(modp);
	delete_export_references(module);
    }
    return 0;
}

