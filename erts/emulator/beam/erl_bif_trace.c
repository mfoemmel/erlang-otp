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
/*
 * Trace BIFs.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "beam_bp.h"

static void new_seq_trace_token(Process* p); /* help func for seq_trace_2*/
static Eterm check_tracee(Process* p, Eterm tracer, Eterm tracee);
static Eterm trace_info_pid(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_on_load(Process* p, Eterm key);

static int setup_func_trace(Export* ep, void* match_prog);
static int reset_func_trace(Export* ep);
static int trace_state(Export* ep);
static int reset_bif_trace(int bif_index);
static int setup_bif_trace(int bif_index, void* match_prog);

Eterm
suspend_process_1(Process* p, Eterm pid)
{
    Process* tracee;

    if (is_non_value(check_tracee(p, p->id, pid))) {
	BIF_ERROR(p, BADARG);
    }
    tracee = process_tab[pid_number(pid)];
    erl_suspend(tracee, NIL);
    BIF_RET(am_true);
}

Eterm
resume_process_1(Process* p, Eterm pid)
{
    Process* tracee;

    if (is_non_value(check_tracee(p, p->id, pid))) {
	BIF_ERROR(p, BADARG);
    }
    tracee = process_tab[pid_number(pid)];
    erl_resume(tracee);
    BIF_RET(am_true);
}

/*
 * Turn on/off call tracing for the given function(s).
 */
  
Eterm
trace_pattern_2(Process* p, Eterm MFA, Eterm Pattern)
{
    return trace_pattern_3(p,MFA,Pattern,NIL);
}

Eterm
trace_pattern_3(Process* p, Eterm MFA, Eterm Pattern, Eterm flaglist)
{
    Eterm* tp;
    Eterm mfa[3];
    int i;
    int matches = 0;
    int specified = 0;
    int on;
    Binary* match_prog_set;
    Eterm l;
    int is_local = 0;

    /*
     * Check and compile the match specification.
     */

    if (Pattern == am_false) {
	match_prog_set = NULL;
	on = 0;
    } else if (is_nil(Pattern) || Pattern == am_true) {
	match_prog_set = NULL;
	on = 1;
    } else if ((match_prog_set = erts_match_set_compile(p, Pattern)) != NULL) {
	MatchSetRef(match_prog_set);
	on = 1;
    } else{
	goto error;
    }

    for(l = flaglist; is_list(l); l = CDR(list_val(l))) {
	switch (CAR(list_val(l))) {
	case am_local:
	    is_local = 1;
	    break;
	case am_global:
	    break;
	default:
	    goto error;
	}
    }

    if (l != NIL) {
	goto error;
    }

    /*
     * Check the MFA specification.
     */

    if (MFA == am_on_load) {
	MatchSetUnref(erts_default_match_spec);
	erts_match_spec_is_on = on;
	erts_default_match_spec = match_prog_set;
	erts_match_local_functions = is_local;
	return make_small(0);
    } else if (is_tuple(MFA)) {
	tp = tuple_val(MFA);
	if (tp[0] != make_arityval(3)) {
	    goto error;
	}
	mfa[0] = tp[1];
	mfa[1] = tp[2];
	mfa[2] = tp[3];
	if (!is_atom(mfa[0]) || !is_atom(mfa[1]) ||
	    (!is_small(mfa[2]) && mfa[2] != am_Underscore)) {
	    goto error;
	}
	for (i = 0; i < 3 && mfa[i] != am_Underscore; i++, specified++) {
	    /* Empty loop body */
	}
	for (i = specified; i < 3; i++) {
	    if (mfa[i] != am_Underscore) {
		goto error;
	    }
	}
	if (is_small(mfa[2])) {
	    mfa[2] = signed_val(mfa[2]);
	}
    } else {
	goto error;
    }

    matches = erts_set_trace_pattern(mfa, specified, match_prog_set, on, is_local);
    MatchSetUnref(match_prog_set);
    return make_small(matches);

 error:
    MatchSetUnref(match_prog_set);
    BIF_ERROR(p, BADARG);
}       



Uint 
erts_trace_flag2bit(Eterm flag) 
{
    switch (flag) {
    case am_send: return F_TRACE_SEND;
    case am_receive: return F_TRACE_RECEIVE;
    case am_set_on_spawn: return F_TRACE_SOS;
    case am_procs: return F_TRACE_PROCS;
    case am_set_on_first_spawn: return F_TRACE_SOS1;
    case am_set_on_link: return F_TRACE_SOL;
    case am_set_on_first_link: return F_TRACE_SOL1;
    case am_timestamp: return F_TIMESTAMP;
    case am_running: return F_TRACE_SCHED;
    case am_garbage_collection: return F_TRACE_GC;
    case am_call: return  F_TRACE_CALLS;
    case am_arity: return F_TRACE_ARITY_ONLY;
    case am_return_to: return F_TRACE_RETURN_TO;
    case am_silent: return F_TRACE_SILENT;
    default: return 0;
    }
}

Eterm
trace_3(Process* p, Eterm pid_spec, Eterm how, Eterm list)
{
    Eterm item;
    int on;
    Eterm tracer = p->id;
    int matches = 0;
    Uint mask = 0;
    Uint res;

    switch (how) {
    case am_false: on = 0; break;
    case am_true: on = 1; break;
    default: goto error;
    }

    while (is_list(list)) {
	item = CAR(list_val(list));
	if (is_atom(item) && (res = erts_trace_flag2bit(item)) != 0) {
	    mask |= res;
	} else if (is_tuple(item)) {
	    Eterm* tp = tuple_val(item);

	    if (arityval(tp[0]) != 2 || tp[1] != am_tracer) {
		goto error;
	    }
	    tracer = tp[2];
	    if (!is_pid(tracer) && !is_port(tracer)) {
		goto error;
	    }
	} else {
	    goto error;
	}
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
	goto error;
    }

    /*
     * Set/reset the call trace flag for the given Pids.
     */

    if (is_pid(pid_spec)) {
	Process* tracee = process_tab[pid_number(pid_spec)];

	if (is_non_value(check_tracee(p, tracer, pid_spec))) {
	    goto error;
	}
	if (on) {
	    tracee->flags |= mask;
	} else {
	    tracee->flags &= ~mask;
	}
	if(!(tracee->flags & TRACE_FLAGS)) {
	    tracee->tracer_proc = NIL;
	} else {
	    tracee->tracer_proc = tracer;
	}
	matches = 1;
    } else {
	int ok = 0;

	if (pid_spec == am_all || pid_spec == am_existing) {
	    int i;

	    ok = 1;
	    for (i = 0; i < max_process; i++) {
		Process* proc = process_tab[i];

		if (proc == NULL || proc->status == P_EXITING ||
		    proc->id == tracer) {
		    continue;
		}
		if (proc->flags & TRACE_FLAGS && proc->tracer_proc != tracer) {
		    Process* tracer_p = process_tab[pid_number(proc->tracer_proc)];
		    if (!INVALID_PID(tracer_p, proc->tracer_proc)) {
			continue;
		    } else {
			proc->flags &= ~TRACE_FLAGS;
		    }
		}
		if (on) {
		    proc->flags |= mask;
		} else {
		    proc->flags &= ~mask;
		}
		if(!(proc->flags & TRACE_FLAGS)) {
		    proc->tracer_proc = NIL;
		} else {
		    proc->tracer_proc = tracer;
		}
		matches++;
	    }
	}
	if (pid_spec == am_all || pid_spec == am_new) {
	    ok = 1;
	    if (on) {
		erts_default_process_flags |= mask;
	    } else {
		erts_default_process_flags &= ~mask;
	    }
	    if(!(erts_default_process_flags & TRACE_FLAGS)) {
		erts_default_tracer = NIL;
	    } else {
		erts_default_tracer = tracer;
	    }
	}
	if (!ok) {
	    goto error;
	}
    }
    BIF_RET(make_small(matches));

 error:
    BIF_ERROR(p, BADARG);
}

/*
 * Check that a process to be traced conforms to the house's rules:
 * That it is not dead, is local, is not tracing, is not being traced
 * by another process.
 */

static Eterm
check_tracee(Process* p, Eterm tracer, Eterm tracee)
{
    Process* tracee_ptr;

    if (pid_node(tracee) != THIS_NODE) {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (pid_number(tracee) >= max_process) {
	goto error;
    }

    tracee_ptr = process_tab[pid_number(tracee)];
    if (INVALID_PID(tracee_ptr, tracee) || tracee_ptr->id == tracer) {
	goto error;
    }

    if (tracee_ptr->flags & TRACE_FLAGS 
	&& tracee_ptr->tracer_proc != tracer) {
	/* This tracee is already being traced, and not by the 
	 * tracer to be */
	if (is_port(tracee_ptr->tracer_proc)) {
	    Port *tracer_port = 
		&erts_port[port_index(tracee_ptr->tracer_proc)];
	    /* 
	     * XXX Complicated test for invalid trace port. 
	     * See erl_trace.c:send_to_port.
	     */
	    if (tracer_port == NULL 
		|| tracer_port->id != tracee_ptr->tracer_proc
		|| tracer_port->status == FREE
		|| (tracer_port->status
		    & (EXITING | CLOSING 
		       | PORT_BUSY | DISTRIBUTION)) != 0) {
		/* Invalid current tracer port -> discard and approve */
		tracee_ptr->flags &= ~TRACE_FLAGS;
		tracee_ptr->tracer_proc = NIL;
	    } else 
		goto already_traced;
	} else {
	    Process *tracer_p = 
		process_tab[pid_number(tracee_ptr->tracer_proc)];
	    
	    if (INVALID_PID(tracer_p, tracee_ptr->tracer_proc)) {
		/* Invalid current tracer process -> discard and approve */
		tracee_ptr->flags &= ~TRACE_FLAGS;
		tracee_ptr->tracer_proc = NIL;
	    } else
		goto already_traced;
	}
    }
    return tracee;

 already_traced:
    cerr_pos = 0;
    erl_printf(CBUF, "** can only have one tracer per process\n");
    send_error_to_logger(p->group_leader);
    goto error;
}

/*
 * Return information about a process or an external function being traced.
 */

Eterm
trace_info_2(Process* p, Eterm What, Eterm Key)
{
    if (What == am_on_load) {
	BIF_RET(trace_info_on_load(p, Key));
    } else if (is_atom(What) || is_pid(What)) {
	BIF_RET(trace_info_pid(p, What, Key));
    } else if (is_tuple(What)) {
	BIF_RET(trace_info_func(p, What, Key));
    } else {
	BIF_ERROR(p, BADARG);
    }
}

static Eterm
trace_info_pid(Process* p, Eterm pid_spec, Eterm key)
{
    Eterm* tracer;
    Uint* flagp;
    Process* tracer_proc_ptr;
    Eterm* hp;

    if (pid_spec == am_new) {
	tracer = &erts_default_tracer;
	flagp = &erts_default_process_flags;
    } else if (is_pid(pid_spec) && pid_node(pid_spec) == THIS_NODE &&
	       pid_number(pid_spec) < max_process) {
	Process* tracee = process_tab[pid_number(pid_spec)];
	if (INVALID_PID(tracee, pid_spec)) {
	    return am_undefined;
	} else {
	    tracer = &(tracee->tracer_proc);
	    flagp = &(tracee->flags);
	}
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (is_pid(*tracer)) {
	tracer_proc_ptr = process_tab[pid_number(*tracer)];
	if (INVALID_PID(tracer_proc_ptr, *tracer)) {
	    *flagp &= ~TRACE_FLAGS;
	    *tracer = NIL;
	}
    }

    if (key == am_flags) {
	int num_flags = 14;
	Uint flags = *flagp;
	Eterm flag_list = NIL;

#define FLAG0(flag_mask,flag) \
  if (flags & (flag_mask)) { flag_list = CONS(hp, flag, flag_list); hp += 2; } else {}

#if defined(DEBUG)
    /*
     * Check num_flags if this assertion fires.
     */
#  define FLAG ASSERT(num_flags-- > 0); FLAG0
#else
#  define FLAG FLAG0
#endif
	hp = HAlloc(p, 3+2*num_flags);
	FLAG(F_TRACE_SEND, am_send);
	FLAG(F_TRACE_RECEIVE, am_receive);
	FLAG(F_TRACE_SOS, am_set_on_spawn);
	FLAG(F_TRACE_CALLS, am_call);
	FLAG(F_TRACE_PROCS, am_procs);
	FLAG(F_TRACE_SOS1, am_set_on_first_spawn);
	FLAG(F_TRACE_SOL, am_set_on_link);
	FLAG(F_TRACE_SOL1, am_set_on_first_link);
	FLAG(F_TRACE_SCHED, am_running);
	FLAG(F_TRACE_GC, am_garbage_collection);
	FLAG(F_TIMESTAMP, am_timestamp);
	FLAG(F_TRACE_ARITY_ONLY, am_arity);
	FLAG(F_TRACE_RETURN_TO, am_return_to);
	FLAG(F_TRACE_SILENT, am_silent);
#undef FLAG0
#undef FLAG
	return TUPLE2(hp, key, flag_list);
    } else if (key == am_tracer) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, *tracer);
    } else {
	goto error;
    }
}

#define FUNC_TRACE_NOEXIST 0
#define FUNC_TRACE_UNTRACED 1
#define FUNC_TRACE_GLOBAL_TRACE 2
#define FUNC_TRACE_LOCAL_TRACE 3
static int function_is_traced(Eterm mfa[3], Binary **match_spec /* out */)
{
    Export e;
    Export* ep;
    int i;
    Uint *code;

    /* First look for an export entry */
    e.code[0] = mfa[0];
    e.code[1] = mfa[1];
    e.code[2] = mfa[2];
    if ((ep = hash_get(&export_table.htable, (void*) &e)) != NULL) {
	if (ExportIsBuiltIn(ep)) { /* A BIF */
	    if (trace_state(ep) == 1) {
		*match_spec = ep->match_prog_set;
		for (i = 0; i < BIF_SIZE; ++i) {
		    if (bif_export[i] == ep) {
			return (erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL) ?
			    FUNC_TRACE_LOCAL_TRACE : FUNC_TRACE_GLOBAL_TRACE;
		    }
		}
		erl_exit(1,"Impossible ghost bif encountered in trace_info.");
	    } else {
		return FUNC_TRACE_UNTRACED;
	    }
	} else { /* Not a BIF */	
	    if (trace_state(ep) == 1) {
		*match_spec = ep->match_prog_set;
		return FUNC_TRACE_GLOBAL_TRACE;
	    }
	}
    }

    /* OK, now look for breakpoint tracing */
    if ((code = erts_find_local_func(mfa)) != NULL) {
	return (erts_is_local_tracepoint(code, match_spec)) ? 
	    FUNC_TRACE_LOCAL_TRACE : FUNC_TRACE_UNTRACED;
    } 
    return FUNC_TRACE_NOEXIST;
}
	

static Eterm
trace_info_func(Process* p, Eterm func_spec, Eterm key)
{
    Eterm* tp;
    Eterm* hp;
    Eterm mfa[3];
    Binary *ms = NULL;
    Eterm traced = NIL;
    Eterm match_spec = am_undefined;

    if (!is_tuple(func_spec)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    tp = tuple_val(func_spec);
    if (tp[0] != make_arityval(3)) {
	goto error;
    }
    if (!is_atom(tp[1]) || !is_atom(tp[2]) || !is_small(tp[3])) {
	goto error;
    }
    mfa[0] = tp[1];
    mfa[1] = tp[2];
    mfa[2] = signed_val(tp[3]);

    switch(function_is_traced(mfa,&ms)) {
    case FUNC_TRACE_NOEXIST:
	traced = am_undefined;
	break;
    case FUNC_TRACE_UNTRACED:
	traced = am_false;
	match_spec = am_false;
	break;
    case FUNC_TRACE_GLOBAL_TRACE:
	traced = am_global;
	match_spec = NIL; /* Fix up later if it's asked for*/
	break;
    case FUNC_TRACE_LOCAL_TRACE:
	traced = am_local;
	match_spec = NIL; /* Fix up later if it's asked for*/
	break;
    }
    if (key == am_traced) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, traced);
    } else if (key == am_match_spec) {
	if (ms) {
	    int sz;
	    match_spec = MatchSetGetSource(ms);
	    sz = size_object(match_spec);
	    hp = HAlloc(p, sz + 3);
	    match_spec = copy_struct(match_spec, sz, &hp, &(p->off_heap));
	} else {
	    hp = HAlloc(p, 3);
	}
	return TUPLE2(hp, key, match_spec);
    } else {
	goto error;
    }
}

static Eterm
trace_info_on_load(Process* p, Eterm key)
{
    Eterm* hp;

    switch (key) {
    case am_traced:
	{
	    Eterm traced;

	    if (!erts_match_spec_is_on) {
		traced = am_false;
	    } else if (erts_match_local_functions) {
		traced = am_local;
	    } else {
		traced = am_global;
	    }
	    hp = HAlloc(p, 3);
	    return TUPLE2(hp, key, traced);
	}
    case am_match_spec:
	{
	    Eterm match_spec;

	    if (!erts_match_spec_is_on) {
		match_spec = am_false;
		hp = HAlloc(p, 3);
	    } else if (erts_default_match_spec) {
		int sz;
		match_spec = MatchSetGetSource(erts_default_match_spec);
		sz = size_object(match_spec);
		hp = HAlloc(p, sz + 3);
		match_spec = copy_struct(match_spec, sz, &hp, &(p->off_heap));
	    } else {
		match_spec = NIL;
		hp = HAlloc(p, 3);
	    }
	    hp = HAlloc(p, 3);
	    return TUPLE2(hp, key, match_spec);
	}
    default:
	BIF_ERROR(p, BADARG);
    }
}

#undef FUNC_TRACE_NOEXIST
#undef FUNC_TRACE_UNTRACED
#undef FUNC_TRACE_GLOBAL_TRACE
#undef FUNC_TRACE_LOCAL_TRACE

int
erts_set_trace_pattern(Eterm* mfa, int specified, Binary* match_prog_set,
		       int on, int is_local)
{
    int matches = 0;
    int i;
    
    /*
     * First work on normal functions (not real BIFs).
     */

    for (i = 0; i < export_list_size; i++) {
	Export* ep = export_list(i);
	int j;

	if (ExportIsBuiltIn(ep)) {
	    continue;
	}

	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j == specified) {
	    if (on) {
		if (!is_local)
		    matches += setup_func_trace(ep, match_prog_set);
		else
		    reset_func_trace(ep);
	    } else if (!is_local) {
		matches += reset_func_trace(ep);
	    }
	}
    }

    /*
    ** OK, now for the bif's
    */
    for (i = 0; i < BIF_SIZE; ++i) {
	Export *ep = bif_export[i];
	int j;

	if (!ExportIsBuiltIn(ep)) {
	    continue;
	}

	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j == specified) {
	    if (on) {
		matches += setup_bif_trace(i, match_prog_set);
		if (is_local)
		    erts_bif_trace_flags[i] |= BIF_TRACE_AS_LOCAL;
		else
		    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
	    } else {
		if (is_local == !!(erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL)) {
		    matches += reset_bif_trace(i);
		    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
		} else {
		    ++matches;
		}
	    }
	}
    }

    /*
    ** So, now for breakpoint tracing
    */
    if (is_local) {
	if (on) {
	    matches += erts_set_break(mfa, specified, match_prog_set);
	} else {
	    matches += erts_clear_break(mfa, specified);
	}
    } else {
	if (on) {
	   erts_clear_break(mfa, specified);
	}
    }
    return matches;
}

/*
 * Setup function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

static int
setup_func_trace(Export* ep, void* match_prog)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = match_prog;
	    MatchSetRef(ep->match_prog_set);
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }

    ep->code[3] = (Uint) em_call_traced_function;
    ep->code[4] = (Uint) ep->address;
    ep->address = ep->code+3;
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
    return 1;
}

static int
setup_bif_trace(int bif_index, void* match_prog)
{
    Export *ep = bif_export[bif_index];
    BifFunction func = (BifFunction) ep->code[4];

#ifdef HARDDEBUG
    erl_printf(CERR,"setup_bif_trace: "); display(ep->code[0],CERR); erl_printf(CERR,":"); display(ep->code[1],CERR);
    erl_printf(CERR,"/%d\r\n",ep->code[2]);
#endif
    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(func != NULL);
    if (func == bif_table[bif_index].f) {
	ep->code[4] = (Uint) bif_table[bif_index].traced;
	/* Can't have a match program if not traced. */
	ASSERT(ep->match_prog_set == NULL);
	ep->match_prog_set = match_prog;
	MatchSetRef(ep->match_prog_set);
    } else if (func == bif_table[bif_index].traced) { /* Change match program */
	MatchSetUnref(ep->match_prog_set);
	ep->match_prog_set = match_prog;
	MatchSetRef(ep->match_prog_set);
    }
    return 1;
}

/*
 * Reset function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

static int
reset_func_trace(Export* ep)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    ep->address = (Uint *) ep->code[4];
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }

    /*
     * Nothing to do, but the export entry matches.
     */

    return 1;
}

static int
reset_bif_trace(int bif_index)
{
    Export *ep = bif_export[bif_index];
    BifFunction func = (BifFunction) ep->code[4];

    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(func != NULL);
    if (bif_table[bif_index].traced == func) {
	ep->code[4] = (Uint) bif_table[bif_index].f;
	MatchSetUnref(ep->match_prog_set);
	ep->match_prog_set = NULL;
    }

    return 1;
}

/*
 * Test if the given export entry is traced.
 *
 * Return Value: 1 if the export entry is traced, 0 if not, -1 for undefined.
 */
  
static int
trace_state(Export* ep)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return -1;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    return 1;
	} else if (ep->code[3] == (Uint) em_apply_bif) {
	    int i;
	    BifFunction func = (BifFunction) ep->code[4];

	    ASSERT(func != NULL);
	    for (i = 0; i < BIF_SIZE; i++) {
		if (func == bif_table[i].f) {
		    return 0;
		} else if (func == bif_table[i].traced) {
		    return 1;
		}
	    }
	    ASSERT(0);		/* We shouldn't get here. */
	    return 0;
	}
    }
    return 0;
}

/*
 * Sequential tracing
 *
 * The sequential trace token is internally implemented as
 * a tuple
 *         {Flags, Label, Serial, Sender, LastSerial}
 * 
 * where 
 *       - Flags is an integer (using masks 1, 2, and 4, for send,
 *         receive and print, respectively), 
 *       - Label is any term, Serial (for now XXX) is an integer (it should
 *         be a list reflecting split traces), and 
 *       - Sender is the Pid of the sender (i.e. the current process, 
 *         except immediately after a message reception, in case it is
 *         the pid of the process that sent the message).
 *
 */

BIF_RETTYPE seq_trace_2(BIF_ALIST_2)    
BIF_ADECL_2
{
    Eterm res;
    res = erts_seq_trace(BIF_P, BIF_ARG_1, BIF_ARG_2, 1);
    if (is_non_value(res)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
}

Eterm erts_seq_trace(Process *p, Eterm arg1, Eterm arg2, 
			  int build_result)
{
    Eterm flags;
    Eterm old_value = am_true;
    Eterm* hp;
    int current_flag;

    if (!is_atom(arg1)) {
	return THE_NON_VALUE;
    }


    if (arg1 == am_send) {
	current_flag = SEQ_TRACE_SEND;
    } else if (arg1 == am_receive) {
	current_flag = SEQ_TRACE_RECEIVE; 
    } else if (arg1 == am_print) {
	current_flag = SEQ_TRACE_PRINT; 
    } else if (arg1 == am_timestamp) {
	current_flag = SEQ_TRACE_TIMESTAMP; 
    }
    else
	current_flag = 0;

    if (current_flag && ( (arg2 == am_true) || (arg2 == am_false)) ) {
	/* Flags */
        new_seq_trace_token(p);
        flags = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(p));
	if (build_result) {
	    old_value = flags & current_flag ? am_true : am_false;
	} 
	if (arg2 == am_true)
	    SEQ_TRACE_TOKEN_FLAGS(p) = make_small(flags|current_flag);
	else if (arg2 == am_false)
	    SEQ_TRACE_TOKEN_FLAGS(p) = make_small(flags&~current_flag);
	else { 
	    return THE_NON_VALUE;
	}
	return old_value;
    }
    else if (arg1 == am_label) {
	if (!(is_atom(arg2) || is_small(arg2))) {
	    return THE_NON_VALUE;
	}
        new_seq_trace_token(p);
	if (build_result) {
	    old_value = SEQ_TRACE_TOKEN_LABEL(p);
	}
	SEQ_TRACE_TOKEN_LABEL(p) = arg2;
    	return old_value;
    }
    else if (arg1 == am_serial) {
	uint32* tp;
	if (is_not_tuple(arg2)) {
	    return THE_NON_VALUE;
	}
	tp = tuple_val(arg2);
	if ((*tp != make_arityval(2)) || is_not_small(*(tp+1)) || is_not_small(*(tp+2))) {
	    return THE_NON_VALUE;
        }
        new_seq_trace_token(p);
	if (build_result) {
	    hp = HAlloc(p,3);
	    old_value = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(p),
			       SEQ_TRACE_TOKEN_SERIAL(p));
	}
	SEQ_TRACE_TOKEN_LASTCNT(p) = *(tp+1);
 	SEQ_TRACE_TOKEN_SERIAL(p) = *(tp+2);
	p->seq_trace_clock = unsigned_val(*(tp+2));
	p->seq_trace_lastcnt = unsigned_val(*(tp+1));
    	return old_value;
    }
    else if (arg1 == am_sequential_trace_token) {
	if (is_not_nil(arg2)) {
	    return THE_NON_VALUE;
        }
	if (build_result) {
	    old_value = SEQ_TRACE_TOKEN(p);
	}
        SEQ_TRACE_TOKEN(p) = NIL;
        return old_value;
    }
    else {
	return THE_NON_VALUE;
    }
}

void new_seq_trace_token(Process* p) {
    uint32* hp;
    if (SEQ_TRACE_TOKEN(p) == NIL) {
      hp = HAlloc(p, 6);
      SEQ_TRACE_TOKEN(p) = TUPLE5(hp, make_small(0), /*Flags*/ 
				      make_small(0), /*Label*/
                                      make_small(0), /*Serial*/
				      p->id,         /*From*/
	                              make_small(p->seq_trace_lastcnt));
    }
}

BIF_RETTYPE seq_trace_info_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 item, res;
    uint32* hp;
    uint32 current_flag;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    item = BIF_ARG_1;

    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	if ((item == am_send) || (item == am_receive) || 
	    (item == am_print) || (item == am_timestamp)) {
	    hp = HAlloc(BIF_P,3);
	    res = TUPLE2(hp, item, am_false);
	    BIF_RET(res);
	} 
	else if ((item == am_label) || (item == am_serial)) {
	    BIF_RET(NIL);
	}
	else {
	    BIF_ERROR(BIF_P, BADARG);
	}
    }

    if (BIF_ARG_1 == am_send) {
	current_flag = SEQ_TRACE_SEND;
    } else if (BIF_ARG_1 == am_receive) {
	current_flag = SEQ_TRACE_RECEIVE; 
    } else if (BIF_ARG_1 == am_print) {
	current_flag = SEQ_TRACE_PRINT; 
    } else if (BIF_ARG_1 == am_timestamp) {
	current_flag = SEQ_TRACE_TIMESTAMP; 
    }
    else
	current_flag = 0;


    if (current_flag) {
      res = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(BIF_P)) & current_flag ? 
	  am_true : am_false;
    }
    else if (item == am_label) {
      res = SEQ_TRACE_TOKEN_LABEL(BIF_P);
    }
    else if (item  == am_serial) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(BIF_P), SEQ_TRACE_TOKEN_SERIAL(BIF_P));
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, 3);
    res = TUPLE2(hp, item, res);
    BIF_RET(res);
}

/*
   seq_trace_print(Message) -> true | false
   This function passes Message to the system_tracer
   if the trace_token is not NIL.
   Returns true if Message is passed else false
   Note! That true is returned if the conditions to pass Message is
   fulfilled, but nothing is passed if system_seq_tracer is not set.
 */
BIF_RETTYPE seq_trace_print_1(BIF_ALIST_1)    
BIF_ADECL_1
{
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) 
	BIF_RET(am_false);
    seq_trace_update_send(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_1, 
		     SEQ_TRACE_PRINT, NIL);
    BIF_RET(am_true);
}

/*
   seq_trace_print(Label,Message) -> true | false
   This function passes Message to the system_tracer
   if the trace_token is not NIL and the trace_token label is equal to
   Label. Returns true if Message is passed else false
   Note! That true is returned if the conditions to pass Message is
   fulfilled, but nothing is passed if system_seq_tracer is not set.
 */
BIF_RETTYPE seq_trace_print_2(BIF_ALIST_2)    
BIF_ADECL_2
{
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) 
	BIF_RET(am_false);
    if (!(is_atom(BIF_ARG_1) || is_small(BIF_ARG_1))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (SEQ_TRACE_TOKEN_LABEL(BIF_P) != BIF_ARG_1)
	BIF_RET(am_false);
    seq_trace_update_send(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_2, 
		     SEQ_TRACE_PRINT, NIL);
    BIF_RET(am_true);
}
