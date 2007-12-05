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

static Eterm check_process_code(Process* rp, Module* modp);
static void delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp);
static void delete_export_references(Eterm module);
static int purge_module(int module);
static int is_native(Eterm* code);
static int any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);
static int any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size);

Eterm
load_module_2(BIF_ALIST_2)
{
    Eterm   reason;
    Eterm*  hp;
    int      i;
    int      sz;
    byte*    code;
    int trace_pattern_is_on;
    Binary *match_spec;
    Binary *meta_match_spec;
    struct trace_pattern_flags trace_pattern_flags;
    Eterm meta_tracer_pid;
    Eterm res;
    byte* temp_alloc = NULL;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((code = erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc)) == NULL) {
	goto error;
    }
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();

    hp = HAlloc(BIF_P, 3);
    sz = binary_size(BIF_ARG_2);
    if ((i = erts_load_module(BIF_P, 0,
			      BIF_P->group_leader, &BIF_ARG_1, code, sz)) < 0) { 
	switch (i) {
	case -1: reason = am_badfile; break; 
	case -2: reason = am_nofile; break;
	case -3: reason = am_not_purged; break;
	case -4:
	    reason = am_atom_put("native_code", sizeof("native_code")-1);
	    break;
	default: reason = am_badfile; break;
	}
	res = TUPLE2(hp, am_error, reason);
	goto done;
    }

    erts_get_default_trace_pattern(&trace_pattern_is_on,
				   &match_spec,
				   &meta_match_spec,
				   &trace_pattern_flags,
				   &meta_tracer_pid);
    if (trace_pattern_is_on) {
	Eterm mfa[1];
	mfa[0] = BIF_ARG_1;
	(void) erts_set_trace_pattern(mfa, 1, 
				      match_spec, 
				      meta_match_spec,
				      1, trace_pattern_flags, 
				      meta_tracer_pid);
    }

    res = TUPLE2(hp, am_module, BIF_ARG_1);

 done:
    erts_free_aligned_binary_bytes(temp_alloc);
    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    BIF_RET(res);
}

BIF_RETTYPE purge_module_1(BIF_ALIST_1)
{
    int purge_res;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();
    purge_res = purge_module(atom_val(BIF_ARG_1));

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    if (purge_res < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE code_is_module_native_1(BIF_ALIST_1)
{
    Module* modp;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((modp = erts_get_module(BIF_ARG_1)) == NULL) {
	return am_undefined;
    }
    return (is_native(modp->code) ||
	    (modp->old_code != 0 && is_native(modp->old_code))) ?
		am_true : am_false;
}

BIF_RETTYPE code_make_stub_module_3(BIF_ALIST_3)
{
    Eterm res;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    erts_export_consolidate();
    res = erts_make_stub_module(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    return res;
}

Eterm
check_process_code_2(BIF_ALIST_2)
{
    Process* rp;
    Module* modp;

    if (is_not_atom(BIF_ARG_2)) {
	goto error;
    }
    if (is_internal_pid(BIF_ARG_1)) {
	Eterm res;
	if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	    goto error;
	rp = erts_pid2proc_not_running(BIF_P, ERTS_PROC_LOCK_MAIN,
				       BIF_ARG_1, ERTS_PROC_LOCK_MAIN);
	if (!rp) {
	    ERTS_BIF_CHK_EXITED(BIF_P);
	    ERTS_SMP_BIF_CHK_RESCHEDULE(BIF_P);
	    BIF_RET(am_false);
	}
	modp = erts_get_module(BIF_ARG_2);
	res = check_process_code(rp, modp);
#ifdef ERTS_SMP
	if (BIF_P != rp)
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
#endif
	BIF_RET(res);
    }
    else if (is_external_pid(BIF_ARG_1)
	     && external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	BIF_RET(am_false);
    }

 error:
    BIF_ERROR(BIF_P, BADARG);
}


BIF_RETTYPE delete_module_1(BIF_ALIST_1)
{
    int res;

    if (is_not_atom(BIF_ARG_1))
	goto badarg;

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(0);

    {
	Module *modp = erts_get_module(BIF_ARG_1);
	if (!modp) {
	    res = am_undefined;
	}
	else if (modp->old_code != 0) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Module %T must be purged before loading\n",
			  BIF_ARG_1);
	    erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	    res = am_badarg;
	}
	else {
	    delete_export_references(BIF_ARG_1);
	    delete_code(BIF_P, 0, modp);
	    res = am_true;
	}
    }

    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    if (res == am_badarg) {
    badarg:
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

BIF_RETTYPE module_loaded_1(BIF_ALIST_1)
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
{
    return erts_preloaded(BIF_P);
}

BIF_RETTYPE loaded_0(BIF_ALIST_0)
{
    Eterm previous = NIL;
    Eterm* hp;
    int i;
    int j = 0;
    
    for (i = 0; i < module_code_size(); i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    j++;
	}
    }
    if (j > 0) {
	hp = HAlloc(BIF_P, j*2);

	for (i = 0; i < module_code_size(); i++) {
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

static Eterm
check_process_code(Process* rp, Module* modp)
{
    Eterm* start;
    char* mod_start;
    Uint mod_size;
    Eterm* end;
    Eterm* sp;
#ifndef HYBRID /* FIND ME! */
    ErlFunThing* funp;
    int done_gc = 0;
#endif

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
    mod_start = (char *) start;
    mod_size = modp->old_code_length;

    /*
     * Check if current instruction or continuation pointer points into module.
     */
    if (INSIDE(rp->i) || INSIDE(rp->cp)) {
	return am_true;
    }

    /*
     * Check all continuation pointers stored on the stack.
     */
    for (sp = rp->stop; sp < STACK_START(rp); sp++) {
	if (is_CP(*sp) && INSIDE(cp_val(*sp))) {
	    return am_true;
	}
    }

    /* 
     * Check all continuation pointers stored in stackdump
     * and clear exception stackdump if there is a pointer
     * to the module.
     */
    if (rp->ftrace != NIL) {
	struct StackTrace *s;
	ASSERT(is_list(rp->ftrace));
	s = (struct StackTrace *) big_val(CDR(list_val(rp->ftrace)));
	if ((s->pc && INSIDE(s->pc)) ||
	    (s->current && INSIDE(s->current))) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	} else {
	    int i;
	    for (i = 0;  i < s->depth;  i++) {
		if (INSIDE(s->trace[i])) {
		    rp->freason = EXC_NULL;
		    rp->fvalue = NIL;
		    rp->ftrace = NIL;
		    break;
		}
	    }
	}
    }

    /*
     * See if there are funs that refer to the old version of the module.
     */

#ifndef HYBRID /* FIND ME! */
 rescan:
    for (funp = MSO(rp).funs; funp; funp = funp->next) {
	Eterm* fun_code;

	fun_code = funp->fe->address;

	if (INSIDE((Eterm *) funp->fe->address)) {
	    if (done_gc) {
		return am_true;
	    } else {
		/*
		 * Try to get rid of this fun by garbage collecting.
		 * Clear both fvalue and ftrace to make sure they
		 * don't hold any funs.
		 */
		rp->freason = EXC_NULL;
		rp->fvalue = NIL;
		rp->ftrace = NIL;
		done_gc = 1;
                FLAGS(rp) |= F_NEED_FULLSWEEP;
		(void) erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
		goto rescan;
	    }
	}
    }
#endif

    /*
     * See if there are constants inside the module referenced by the process.
     */
    done_gc = 0;
    for (;;) {
	ErlMessage* mp;

	if (any_heap_ref_ptrs(&rp->fvalue, &rp->fvalue+1, mod_start, mod_size)) {
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	}
	if (any_heap_ref_ptrs(rp->stop, rp->hend, mod_start, mod_size)) {
	    goto need_gc;
	}
	if (any_heap_refs(rp->heap, rp->htop, mod_start, mod_size)) {
	    goto need_gc;
	}

	if (any_heap_refs(rp->old_heap, rp->old_htop, mod_start, mod_size)) {
	    goto need_gc;
	}

	if (rp->dictionary != NULL) {
	    Eterm* start = rp->dictionary->data;
	    Eterm* end = start + rp->dictionary->used;

	    if (any_heap_ref_ptrs(start, end, mod_start, mod_size)) {
		goto need_gc;
	    }
	}

	for (mp = rp->msg.first; mp != NULL; mp = mp->next) {
	    if (any_heap_ref_ptrs(mp->m, mp->m+2, mod_start, mod_size)) {
		goto need_gc;
	    }
	}
	break;

    need_gc:
	if (done_gc) {
	    return am_true;
	} else {
	    Eterm* literals;
	    Uint lit_size;

	    /*
	     * Try to get rid of constants by by garbage collecting.
	     * Clear both fvalue and ftrace.
	     */
	    rp->freason = EXC_NULL;
	    rp->fvalue = NIL;
	    rp->ftrace = NIL;
	    done_gc = 1;
	    FLAGS(rp) |= F_NEED_FULLSWEEP;
	    (void) erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
	    literals = (Eterm *) modp->old_code[MI_LITERALS_START];
	    lit_size = (Eterm *) modp->old_code[MI_LITERALS_END] - literals;
	    erts_garbage_collect_literals(rp, literals, lit_size);
	}
    }
    return am_false;
#undef INSIDE
}

#define in_area(ptr,start,nbytes) \
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

static int
any_heap_ref_ptrs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size)
{
    Eterm* p;
    Eterm val;

    for (p = start; p < end; p++) {
	val = *p;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	case TAG_PRIMARY_LIST:
	    if (in_area(val, mod_start, mod_size)) {
		return 1;
	    }
	    break;
	}
    }
    return 0;
}

static int
any_heap_refs(Eterm* start, Eterm* end, char* mod_start, Uint mod_size)
{
    Eterm* p;
    Eterm val;

    for (p = start; p < end; p++) {
	val = *p;
	switch (primary_tag(val)) {
	case TAG_PRIMARY_BOXED:
	case TAG_PRIMARY_LIST:
	    if (in_area(val, mod_start, mod_size)) {
		return 1;
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (!header_is_transparent(val)) {
		Eterm* new_p = p + thing_arityval(val);
		ASSERT(start <= new_p && new_p < end);
		p = new_p;
	    }
	}
    }
    return 0;
}

#undef in_area


static int
purge_module(int module)
{
    Eterm* code;
    Eterm* end;
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
	    erts_printf("No code to purge for %T\n", make_atom(module));
	}
	return -1;
    }


    /*
     * Remove the old code.
     */
    ASSERT(erts_total_code_size >= modp->old_code_length);
    erts_total_code_size -= modp->old_code_length;
    code = modp->old_code;
    end = (Eterm *)((char *)code + modp->old_code_length);
    erts_cleanup_funs_on_purge(code, end);
    beam_catches_delmod(modp->old_catches, code, modp->old_code_length);
    erts_free(ERTS_ALC_T_CODE, (void *) code);
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
	    mid_module = &modules[num_loaded_modules/2];
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
delete_code(Process *c_p, ErtsProcLocks c_p_locks, Module* modp)
{
#ifdef ERTS_ENABLE_LOCK_CHECK
#ifdef ERTS_SMP
    if (c_p && c_p_locks)
	erts_proc_lc_chk_only_proc_main(c_p);
    else
#endif
	erts_lc_check_exact(NULL, 0);
#endif

    /*
     * Clear breakpoints if any
     */
    if (modp->code != NULL && modp->code[MI_NUM_BREAKPOINTS] > 0) {
	if (c_p && c_p_locks)
	    erts_smp_proc_unlock(c_p, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);
	erts_clear_module_break(modp);
	modp->code[MI_NUM_BREAKPOINTS] = 0;
	erts_smp_release_system();
	if (c_p && c_p_locks)
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
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
delete_export_references(Eterm module)
{
    int i;

    ASSERT(is_atom(module));

    for (i = 0; i < export_list_size(); i++) {
	Export *ep = export_list(i);
        if (ep != NULL && (ep->code[0] == module)) {
	    if (ep->address == ep->code+3 &&
		(ep->code[3] == (Eterm) em_apply_bif)) {
		continue;
	    }
	    ep->address = ep->code+3;
	    ep->code[3] = (Uint) em_call_error_handler;
	    ep->code[4] = 0;
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	}
    }
}


int
beam_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module)
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
	    erts_printf("saving old code\n");
	}
	delete_code(c_p, c_p_locks, modp);
	delete_export_references(module);
    }
    return 0;
}

static int
is_native(Eterm* code)
{
    return ((Eterm *)code[MI_FUNCTIONS])[1] != 0;
}


