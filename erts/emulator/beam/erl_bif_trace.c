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
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "beam_bp.h"

static void new_seq_trace_token(Process* p); /* help func for seq_trace_2*/
static int already_traced(Process *tracee_p, Eterm tracer);
static Eterm check_tracee(Process *p, Eterm tracer, Eterm tracee);
static Eterm trace_info_pid(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_on_load(Process* p, Eterm key);

static int setup_func_trace(Export* ep, void* match_prog);
static int reset_func_trace(Export* ep);
static void reset_bif_trace(int bif_index);
static void setup_bif_trace(int bif_index);
static void set_trace_bif(int bif_index, void* match_prog);
static void clear_trace_bif(int bif_index);

Eterm
suspend_process_1(Process* p, Eterm pid)
{
    Process* tracee;

    if (is_non_value(check_tracee(p, p->id, pid))) {
	BIF_ERROR(p, BADARG);
    }
    tracee = process_tab[internal_pid_index(pid)];
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
    tracee = process_tab[internal_pid_index(pid)];
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
    Eterm mfa[3];
    int i;
    int matches = 0;
    int specified = 0;
    enum erts_break_op on;
    Binary* match_prog_set;
    Eterm l;
    struct trace_pattern_flags flags = erts_trace_pattern_flags_off;
    int is_global;
    Process *meta_tracer_proc = p;
    Eterm meta_tracer_pid = p->id;
    
    /*
     * Check and compile the match specification.
     */
    
    if (Pattern == am_false) {
	match_prog_set = NULL;
	on = 0;
    } else if (is_nil(Pattern) || Pattern == am_true) {
	match_prog_set = NULL;
	on = 1;
    } else if (Pattern == am_restart) {
	match_prog_set = NULL;
	on = erts_break_reset;
    } else if (Pattern == am_pause) {
	match_prog_set = NULL;
	on = erts_break_stop;
    } else if ((match_prog_set = erts_match_set_compile(p, Pattern)) != NULL) {
	MatchSetRef(match_prog_set);
	on = 1;
    } else{
	goto error;
    }
    
    is_global = 0;
    for(l = flaglist; is_list(l); l = CDR(list_val(l))) {
	if (is_tuple(CAR(list_val(l)))) {
	    Eterm *tp = tuple_val(CAR(list_val(l)));
	    
	    if (arityval(tp[0]) != 2 || tp[1] != am_meta) {
		goto error;
	    }
	    meta_tracer_pid = tp[2];
	    if (is_internal_pid(meta_tracer_pid)) {
		if (internal_pid_index(meta_tracer_pid) >= erts_max_processes) {
		    goto error;
		}
		meta_tracer_proc = 
		    process_tab[internal_pid_index(meta_tracer_pid)];
		if (INVALID_PID(meta_tracer_proc, meta_tracer_pid)) {
		    goto error;
		}
	    } else if (is_internal_port(meta_tracer_pid)) {
		Port *meta_tracer_port;
		
		if (internal_port_index(meta_tracer_pid) >= erts_max_ports)
		    goto error;
		meta_tracer_port = 
		    &erts_port[internal_port_index(meta_tracer_pid)];
		if (INVALID_TRACER_PORT(meta_tracer_port, meta_tracer_pid)) {
		    goto error;
		}
		meta_tracer_proc = NULL;
	    } else {
		goto error;
	    }
	    if (is_global) {
		goto error;
	    }
	    flags.breakpoint = 1;
	    flags.meta       = 1;
	} else {
	    switch (CAR(list_val(l))) {
	    case am_local:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.local      = 1;
		break;
	    case am_meta:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.meta       = 1;
		break;
	    case am_global:
		if (flags.breakpoint) {
		    goto error;
		}
		is_global = !0;
		break;
	    case am_call_count:
		if (is_global) {
		    goto error;
		}
		flags.breakpoint = 1;
		flags.call_count = 1;
		break;
	    default:
		goto error;
	    }
	}
    }
    if (l != NIL) {
	goto error;
    }
    
    if (match_prog_set && !flags.local && !flags.meta && flags.call_count) {
	/* A match prog is not allowed with just call_count */
	goto error;
    }

    /*
     * Check the MFA specification.
     */

    if (MFA == am_on_load) {
	if (flags.local || (! flags.breakpoint)) {
	    MatchSetUnref(erts_default_match_spec);
	    erts_default_match_spec = match_prog_set;
	    MatchSetRef(erts_default_match_spec);
	}
	if (flags.meta) {
	    MatchSetUnref(erts_default_meta_match_spec);
	    erts_default_meta_match_spec = match_prog_set;
	    MatchSetRef(erts_default_meta_match_spec);
	    erts_default_meta_tracer_pid = meta_tracer_pid;
	    if (meta_tracer_proc) {
		meta_tracer_proc->flags |= F_TRACER;
	    }
	} else if (! flags.breakpoint) {
	    MatchSetUnref(erts_default_meta_match_spec);
	    erts_default_meta_match_spec = NULL;
	    erts_default_meta_tracer_pid = NIL;
	}
	MatchSetUnref(match_prog_set);
	if (erts_default_trace_pattern_flags.breakpoint &&
	    flags.breakpoint) { 
	    /* Breakpoint trace -> breakpoint trace */
	    ASSERT(erts_default_trace_pattern_is_on);
	    if (on) {
		erts_default_trace_pattern_flags.local
		    |= flags.local;
		erts_default_trace_pattern_flags.meta
		    |= flags.meta;
		erts_default_trace_pattern_flags.call_count
		    |= (on == 1) ? flags.call_count : 0;
	    } else {
		erts_default_trace_pattern_flags.local
		    &= ~flags.local;
		erts_default_trace_pattern_flags.meta
		    &= ~flags.meta;
		erts_default_trace_pattern_flags.call_count
		    &= ~flags.call_count;
		if (! (erts_default_trace_pattern_flags.breakpoint =
		       erts_default_trace_pattern_flags.local |
		       erts_default_trace_pattern_flags.meta |
		       erts_default_trace_pattern_flags.call_count)) {
		    erts_default_trace_pattern_is_on = !!on; /* i.e off */
		}
	    }
	} else if (! erts_default_trace_pattern_flags.breakpoint &&
		   ! flags.breakpoint) {
	    /* Global call trace -> global call trace */
	    erts_default_trace_pattern_is_on = !!on;
	} else if (erts_default_trace_pattern_flags.breakpoint &&
		   ! flags.breakpoint) {
	    /* Breakpoint trace -> global call trace */
	    if (on) {
		erts_default_trace_pattern_flags = flags; /* Struct copy */
		erts_default_trace_pattern_is_on = !!on;
	    }
	} else {
	    ASSERT(! erts_default_trace_pattern_flags.breakpoint &&
		   flags.breakpoint);
	    /* Global call trace -> breakpoint trace */
	    if (on) {
		if (on != 1) {
		    flags.call_count = 0;
		}
		flags.breakpoint = flags.local | flags.meta | flags.call_count;
		erts_default_trace_pattern_flags = flags; /* Struct copy */
		erts_default_trace_pattern_is_on = !!flags.breakpoint;
	    }
	}
	
	return make_small(0);
    } else if (is_tuple(MFA)) {
	Eterm *tp = tuple_val(MFA);
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
    
    if (meta_tracer_proc) {
	meta_tracer_proc->flags |= F_TRACER;
    }
    matches = erts_set_trace_pattern(mfa, specified, 
				     match_prog_set, match_prog_set,
				     on, flags, meta_tracer_pid);
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
    case am_all: return TRACE_FLAGS;
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
    Process *tracer_p = NULL;
    Port *tracer_port = NULL;
    Eterm tracer = NIL;
    int matches = 0;
    Uint mask = 0;
    Uint res;
#ifdef HAVE_ERTS_NOW_CPU
    int cpu_ts = 0;
#endif

    switch (how) {
    case am_false: 
	on = 0; 
	break;
    case am_true: 
	on = 1; 
	tracer_p = p;
	tracer = p->id;
	break;
    default: 
	goto error;
    }

    while (is_list(list)) {
	item = CAR(list_val(list));
	if (is_atom(item) && (res = erts_trace_flag2bit(item)) != 0) {
	    mask |= res;
#ifdef HAVE_ERTS_NOW_CPU
	} else if (item == am_cpu_timestamp) {
	    cpu_ts = !0;
#endif
	} else if (is_tuple(item)) {
	    Eterm* tp = tuple_val(item);
	    
	    if (arityval(tp[0]) != 2 || tp[1] != am_tracer)
		goto error;
	    tracer = tp[2];
	    if (is_internal_pid(tracer)) {
		if (internal_pid_index(tracer) >= erts_max_processes)
		    goto error;
		tracer_p = process_tab[internal_pid_index(tracer)];
		if (INVALID_PID(tracer_p, tracer))
		    goto error;
		tracer_port = NULL;
	    } else if (is_internal_port(tracer)) {
		if (internal_port_index(tracer) >= erts_max_ports)
		    goto error;
		tracer_port = &erts_port[internal_port_index(tracer)];
		if (INVALID_TRACER_PORT(tracer_port, tracer))
		    goto error;
		tracer_p = NULL;
	    } else
		goto error;
	} else
	    goto error;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list))
	goto error;

    /*
     * Set/reset the call trace flag for the given Pids.
     */

    if (is_pid(pid_spec)) {
	Process *tracee_p;

#ifdef HAVE_ERTS_NOW_CPU
	if (cpu_ts) {
	    goto error;
	}
#endif
	/* Check that the tracee is not dead, not tracing 
	 * and not about to be tracing.
	 */

	if (is_not_internal_pid(pid_spec)
	    || internal_pid_index(pid_spec) >= erts_max_processes)
	    goto error;
	tracee_p = process_tab[internal_pid_index(pid_spec)];
	if (INVALID_PID(tracee_p, pid_spec))
	    goto error;

	if (tracer != NIL) {
	    ASSERT((tracer_p != NULL && tracer_port == NULL)
		   || (tracer_p == NULL && tracer_port != NULL));
	    if (pid_spec == tracer)
		goto error;
	    if (already_traced(tracee_p, tracer))
		goto already_traced;
	}
	if (on) {
	    tracee_p->flags |= mask;
	} else {
	    tracee_p->flags &= ~mask;
	}
	if(!(tracee_p->flags & TRACE_FLAGS)) {
	    tracee_p->tracer_proc = NIL;
	} else if (tracer != NIL) {
	    tracee_p->tracer_proc = tracer;
	}
	if (tracer_p) {
	    tracer_p->flags |= F_TRACER;
	}
	matches = 1;
    } else {
	int ok = 0;

#ifdef HAVE_ERTS_NOW_CPU
	if (cpu_ts) {
	    if (pid_spec == am_all) {
		if (on) {
		    if (!erts_cpu_timestamp) {
			if (erts_start_now_cpu() < 0) {
			    goto error;
			}
			erts_cpu_timestamp = !0;
		    }
		}
	    } else {
		goto error;
	    }
	}
#endif
	
	if (pid_spec == am_all || pid_spec == am_existing) {
	    int i;

	    ok = 1;
	    for (i = 0; i < erts_max_processes; i++) {
		Process* tracee_p = process_tab[i];
		
		if (INVALID_PID(tracee_p, tracee_p->id))
		    continue;
		if (tracer != NIL) {
		    if (tracee_p->id == tracer)
			continue;
		    if (already_traced(tracee_p, tracer))
			continue;
		}
		if (on) {
		    tracee_p->flags |= mask;
		} else {
		    tracee_p->flags &= ~mask;
		}
		if(!(tracee_p->flags & TRACE_FLAGS)) {
		    tracee_p->tracer_proc = NIL;
		} else if (tracer != NIL) {
		    tracee_p->tracer_proc = tracer;
		}
		if (tracer_p) {
		    tracer_p->flags |= F_TRACER;
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
	    } else if (tracer != NIL) {
		if (tracer_p) {
		    tracer_p->flags |= F_TRACER;
		}
		erts_default_tracer = tracer;
	    }
#ifdef HAVE_ERTS_NOW_CPU
	    if (cpu_ts && !on) {
		/* cpu_ts => pid_spec == am_all */
		if (erts_cpu_timestamp) {
		    erts_stop_now_cpu();
		    erts_cpu_timestamp = 0;
		}
	    }
#endif
	}
	
	if (!ok)
	    goto error;
    }
    BIF_RET(make_small(matches));

 error:
    BIF_ERROR(p, BADARG);

 already_traced:
    cerr_pos = 0;
    erl_printf(CBUF, "** can only have one tracer per process\n");
    send_error_to_logger(p->group_leader);
    goto error;
}

/* Check that the process to be traced is not already traced
 * by a valid other tracer than the tracer to be.
 */
static int already_traced(Process *tracee_p, Eterm tracer) {
    if (tracee_p->flags & TRACE_FLAGS
	&& tracee_p->tracer_proc != tracer) {
	/* This tracee is already being traced, and not by the 
	 * tracer to be */
	if (is_internal_port(tracee_p->tracer_proc)) {
	    Port *tracer_port =
	      &erts_port[internal_port_index(tracee_p->tracer_proc)];
	    if (INVALID_TRACER_PORT(tracer_port, tracee_p->tracer_proc)) {
		/* Current trace port now invalid 
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else if(is_internal_pid(tracee_p->tracer_proc)) {
	    Process *tracer_p;
	    ASSERT(internal_pid_index(tracee_p->tracer_proc)
		   < erts_max_processes);
	    tracer_p = process_tab[internal_pid_index(tracee_p->tracer_proc)];
	    if (INVALID_PID(tracer_p, tracee_p->tracer_proc)) {
		/* Current trace process now invalid
		 * - discard it and approve the new. */
		goto remove_tracer;
	    } else
		return 1;
	}
	else {
	remove_tracer:
	    tracee_p->flags &= ~TRACE_FLAGS;
	    tracee_p->tracer_proc = NIL;
	}
    }
    return 0;
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

    if (is_not_internal_pid(tracee)) {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (internal_pid_index(tracee) >= erts_max_processes)
	goto error;

    tracee_ptr = process_tab[internal_pid_index(tracee)];    

    if (INVALID_PID(tracee_ptr, tracee)
	|| tracee_ptr->id == tracer) /* Local pids */
	goto error;

    if (already_traced(tracee_ptr, tracer))
	goto already_traced;

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
    } else if (is_internal_pid(pid_spec)
	       && internal_pid_index(pid_spec) < erts_max_processes) {
	Process* tracee = process_tab[internal_pid_index(pid_spec)];
	if (INVALID_PID(tracee, pid_spec)) {
	    return am_undefined;
	} else {
	    tracer = &(tracee->tracer_proc);
	    flagp = &(tracee->flags);
	}
    } else if (is_external_pid(pid_spec)
	       && external_pid_dist_entry(pid_spec) == erts_this_dist_entry) {
	    return am_undefined;
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (is_internal_pid(*tracer)) {
	ASSERT(internal_pid_index(*tracer) < erts_max_processes);
	tracer_proc_ptr = process_tab[internal_pid_index(*tracer)];
	if (INVALID_PID(tracer_proc_ptr, *tracer)) {
	    *flagp &= ~TRACE_FLAGS;
	    *tracer = NIL;
	}
    } else if (is_external_pid(*tracer)) {
	*flagp &= ~TRACE_FLAGS;
	*tracer = NIL;
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
	return TUPLE2(hp, key, *tracer); /* Local pid */
    } else {
	goto error;
    }
}

#define FUNC_TRACE_NOEXIST      0
#define FUNC_TRACE_UNTRACED     (1<<0)
#define FUNC_TRACE_GLOBAL_TRACE (1<<1)
#define FUNC_TRACE_LOCAL_TRACE  (1<<2)
#define FUNC_TRACE_META_TRACE   (1<<3)
#define FUNC_TRACE_COUNT_TRACE  (1<<4)
/*
 * Returns either FUNC_TRACE_NOEXIST, FUNC_TRACE_UNTRACED,
 * FUNC_TRACE_GLOBAL_TRACE, or,
 * an or'ed combination of at least one of FUNC_TRACE_LOCAL_TRACE,
 * FUNC_TRACE_META_TRACE, FUNC_TRACE_COUNT_TRACE.
 *
 * If the return value contains FUNC_TRACE_GLOBAL_TRACE 
 * or FUNC_TRACE_LOCAL_TRACE *ms is set.
 *
 * If the return value contains FUNC_TRACE_META_TRACE, 
 * *ms_meta or *tracer_pid_meta is set.
 *
 * If the return value contains FUNC_TRACE_COUNT_TRACE, *count is set.
 */
static int function_is_traced(Eterm mfa[3], 
			      Binary **ms, /* out */
			      Binary **ms_meta,  /* out */
			      Eterm   *tracer_pid_meta, /* out */
			      Sint    *count)    /* out */
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
	if (ep->address == ep->code+3 &&
	    ep->code[3] != (Uint) em_call_error_handler) {
	    if (ep->code[3] == (Uint) em_call_traced_function) {
		*ms = ep->match_prog_set;
		return FUNC_TRACE_GLOBAL_TRACE;
	    }
	    if (ep->code[3] == (Uint) em_apply_bif) {
		for (i = 0; i < BIF_SIZE; ++i) {
		    if (bif_export[i] == ep) {
			int r = 0;
			
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_GLOBAL) {
			    *ms = ep->match_prog_set;
			    return FUNC_TRACE_GLOBAL_TRACE;
			} else {
			    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL) {
				r |= FUNC_TRACE_LOCAL_TRACE;
				*ms = ep->match_prog_set;
			    }
			    if (erts_is_mtrace_bif(ep->code+3, ms_meta, 
						   tracer_pid_meta)) {
				r |= FUNC_TRACE_META_TRACE;
			    }
			}
			return r ? r : FUNC_TRACE_UNTRACED;
		    }
		}
		erl_exit(1,"Impossible ghost bif encountered in trace_info.");
	    }
	}
    }
    
    /* OK, now look for breakpoint tracing */
    if ((code = erts_find_local_func(mfa)) != NULL) {
	int r = 
	    (erts_is_trace_break(code, ms, NULL)
	     ? FUNC_TRACE_LOCAL_TRACE : 0) 
	    | (erts_is_mtrace_break(code, ms_meta, tracer_pid_meta)
	       ? FUNC_TRACE_META_TRACE : 0)
	    | (erts_is_count_break(code, count)
	       ? FUNC_TRACE_COUNT_TRACE : 0);
	
	return r ? r : FUNC_TRACE_UNTRACED;
    } 
    return FUNC_TRACE_NOEXIST;
}

static Eterm
trace_info_func(Process* p, Eterm func_spec, Eterm key)
{
    Eterm* tp;
    Eterm* hp;
    Eterm mfa[3];
    Binary *ms = NULL, *ms_meta = NULL;
    Sint count = 0;
    Eterm traced = am_false;
    Eterm match_spec = am_false;
    Eterm retval = am_false;
    Eterm meta = am_false;
    int r;

    if (!is_tuple(func_spec)) {
	goto error;
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

    r = function_is_traced(mfa, &ms, &ms_meta, &meta, &count);
    switch (r) {
    case FUNC_TRACE_NOEXIST:
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_undefined);
    case FUNC_TRACE_UNTRACED:
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_false);
    case FUNC_TRACE_GLOBAL_TRACE:
	traced = am_global;
	match_spec = NIL; /* Fix up later if it's asked for*/
	break;
    default:
	if (r & FUNC_TRACE_LOCAL_TRACE) {
	    traced = am_local;
	    match_spec = NIL; /* Fix up later if it's asked for*/
	}
	break;
    }

    switch (key) {
    case am_traced:
	retval = traced;
	break;
    case am_match_spec:
	if (ms) {
	    match_spec = MatchSetGetSource(ms);
	    match_spec = copy_object(match_spec, p);
	}
	retval = match_spec;
	break;
    case am_meta:
	retval = meta;
	break;
    case am_meta_match_spec:
	if (r & FUNC_TRACE_META_TRACE) {
	    if (ms_meta) {
		retval = MatchSetGetSource(ms_meta);
		retval = copy_object(retval, p);
	    } else {
		retval = NIL;
	    }
	}
	break;
    case am_call_count:
	if (r & FUNC_TRACE_COUNT_TRACE) {
	    retval = count < 0 ? 
		make_small_or_big(-count-1, p) : 
		make_small_or_big(count, p);
	}
	break;
    case am_all: {
	Eterm match_spec_meta = am_false, c = am_false, t;
	
	if (ms) {
	    match_spec = MatchSetGetSource(ms);
	    match_spec = copy_object(match_spec, p);
	}
	if (r & FUNC_TRACE_META_TRACE) {
	    if (ms_meta) {
		match_spec_meta = MatchSetGetSource(ms_meta);
		match_spec_meta = copy_object(match_spec_meta, p);
	    } else
		match_spec_meta = NIL;
	}
	if (r & FUNC_TRACE_COUNT_TRACE) {
	    c = count < 0 ? 
		make_small_or_big(-count-1, p) : 
		make_small_or_big(count, p);
	}
	hp = HAlloc(p, (3+2)*5);
	retval = NIL;
	t = TUPLE2(hp, am_call_count, c); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta_match_spec, match_spec_meta); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_meta, meta); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_match_spec, match_spec); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
	t = TUPLE2(hp, am_traced, traced); hp += 3;
	retval = CONS(hp, t, retval); hp += 2;
    }   break;
    default:
	goto error;
    }
    hp = HAlloc(p, 3);
    return TUPLE2(hp, key, retval);

 error:
    BIF_ERROR(p, BADARG);
}

static Eterm
trace_info_on_load(Process* p, Eterm key)
{
    Eterm* hp;
    
    if (! erts_default_trace_pattern_is_on) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, am_false);
    }
    switch (key) {
    case am_traced:
	{
	    Eterm traced = am_false;
	    
	    if (! erts_default_trace_pattern_flags.breakpoint) {
		traced = am_global;
	    } else if (erts_default_trace_pattern_flags.local) {
		traced = am_local;
	    }
	    hp = HAlloc(p, 3);
	    return TUPLE2(hp, key, traced);
	}
    case am_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if ((! erts_default_trace_pattern_flags.breakpoint) ||
		erts_default_trace_pattern_flags.local) {
		if (erts_default_match_spec) {
		    match_spec = MatchSetGetSource(erts_default_match_spec);
		    match_spec = copy_object(match_spec, p);
		    hp = HAlloc(p, 3);
		} else {
		    match_spec = NIL;
		    hp = HAlloc(p, 3);
		}
	    } else {
		hp = HAlloc(p, 3);
	    }
	    return TUPLE2(hp, key, match_spec);
	}
    case am_meta:
	hp = HAlloc(p, 3);
	if (erts_default_trace_pattern_flags.meta) {
	    return TUPLE2(hp, key, erts_default_meta_tracer_pid);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_meta_match_spec:
	{
	    Eterm match_spec = am_false;
	    
	    if (erts_default_trace_pattern_flags.meta) {
		if (erts_default_meta_match_spec) {
		    match_spec = 
			MatchSetGetSource(erts_default_meta_match_spec);
		    match_spec = copy_object(match_spec, p);
		    hp = HAlloc(p, 3);
		} else {
		    match_spec = NIL;
		    hp = HAlloc(p, 3);
		}
	    } else {
		hp = HAlloc(p, 3);
	    }
	    return TUPLE2(hp, key, match_spec);
	}
    case am_call_count:
	hp = HAlloc(p, 3);
	if (erts_default_trace_pattern_flags.call_count) {
	    return TUPLE2(hp, key, am_true);
	} else {
	    return TUPLE2(hp, key, am_false);
	}
    case am_all:
	{
	    Eterm match_spec = am_false, meta_match_spec = am_false, r = NIL, t;
	    
	    if (erts_default_trace_pattern_flags.local ||
		(! erts_default_trace_pattern_flags.breakpoint)) {
		match_spec = NIL;
	    }
	    if (erts_default_match_spec) {
		match_spec = MatchSetGetSource(erts_default_match_spec);
		match_spec = copy_object(match_spec, p);
	    }
	    if (erts_default_trace_pattern_flags.meta) {
		meta_match_spec = NIL;
	    }
	    if (erts_default_meta_match_spec) {
		meta_match_spec = 
		    MatchSetGetSource(erts_default_meta_match_spec);
		meta_match_spec = copy_object(meta_match_spec, p);
	    }
	    hp = HAlloc(p, (3+2)*5 + 3);
	    t = TUPLE2(hp, am_call_count, 
		       (erts_default_trace_pattern_flags.call_count
			? am_true : am_false)); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta_match_spec, meta_match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_meta, 
		       (erts_default_trace_pattern_flags.meta
			? erts_default_meta_tracer_pid : am_false)); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_match_spec, match_spec); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    t = TUPLE2(hp, am_traced,
		       (! erts_default_trace_pattern_flags.breakpoint ?
			am_global : (erts_default_trace_pattern_flags.local ?
				     am_local : am_false))); hp += 3;
	    r = CONS(hp, t, r); hp += 2;
	    return TUPLE2(hp, key, r);
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
erts_set_trace_pattern(Eterm* mfa, int specified, 
		       Binary* match_prog_set, Binary *meta_match_prog_set,
		       int on, struct trace_pattern_flags flags,
		       Eterm meta_tracer_pid)
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
		if (! flags.breakpoint)
		    matches += setup_func_trace(ep, match_prog_set);
		else
		    reset_func_trace(ep);
	    } else if (! flags.breakpoint) {
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
	
	if (bif_table[i].f == bif_table[i].traced) {
	    /* Trace wrapper same as regular function - untraceable */
	    continue;
	}
	
	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j == specified) {
	    if (! flags.breakpoint) { /* Export entry call trace */
		if (on) {
		    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_META) {
			ASSERT(ExportIsBuiltIn(bif_export[i]));
			erts_clear_mtrace_bif
			    ((Uint *)bif_export[i]->code + 3);
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_META;
		    }
		    set_trace_bif(i, match_prog_set);
		    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
		    erts_bif_trace_flags[i] |= BIF_TRACE_AS_GLOBAL;
		    setup_bif_trace(i);
		} else { /* off */
		    if (erts_bif_trace_flags[i] & BIF_TRACE_AS_GLOBAL) {
			clear_trace_bif(i);
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
		    }
		    if (! erts_bif_trace_flags[i]) {
			reset_bif_trace(i);
		    }
		}
		matches++;
	    } else { /* Breakpoint call trace */
		int m = 0;
		
		if (on) {
		    if (flags.local) {
			set_trace_bif(i, match_prog_set);
			erts_bif_trace_flags[i] |= BIF_TRACE_AS_LOCAL;
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
			m = 1;
		    }
		    if (flags.meta) {
			erts_set_mtrace_bif
			    ((Uint *)bif_export[i]->code + 3,
			     meta_match_prog_set, meta_tracer_pid);
			erts_bif_trace_flags[i] |= BIF_TRACE_AS_META;
			erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_GLOBAL;
			m = 1;
		    }
		    if (erts_bif_trace_flags[i]) {
			setup_bif_trace(i);
		    }
		} else { /* off */
		    if (flags.local) {
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_LOCAL) {
			    clear_trace_bif(i);
			    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_LOCAL;
			}
			m = 1;
		    }
		    if (flags.meta) {
			if (erts_bif_trace_flags[i] & BIF_TRACE_AS_META) {
			    erts_clear_mtrace_bif
				((Uint *)bif_export[i]->code + 3);
			    erts_bif_trace_flags[i] &= ~BIF_TRACE_AS_META;
			}
			m = 1;
		    }
		    if (! erts_bif_trace_flags[i]) {
			reset_bif_trace(i);
		    }
		}
		matches += m;
	    }
	}
    }

    /*
    ** So, now for breakpoint tracing
    */
    if (on) {
	if (! flags.breakpoint) {
	    erts_clear_trace_break(mfa, specified);
	    erts_clear_mtrace_break(mfa, specified);
	    erts_clear_count_break(mfa, specified);
	} else {
	    int m = 0;
	    if (flags.local) {
		m = erts_set_trace_break(mfa, specified, match_prog_set,
					 am_true);
	    }
	    if (flags.meta) {
		m = erts_set_mtrace_break(mfa, specified, meta_match_prog_set,
					  meta_tracer_pid);
	    }
	    if (flags.call_count) {
		m = erts_set_count_break(mfa, specified, on);
	    }
	    /* All assignments to 'm' above should give the same value,
	     * so just use the last */
	    matches += m;
	}
    } else {
	int m = 0;
	if (flags.local) {
	    m = erts_clear_trace_break(mfa, specified);
	}
	if (flags.meta) {
	    m = erts_clear_mtrace_break(mfa, specified);
	}
	if (flags.call_count) {
	    m = erts_clear_count_break(mfa, specified);
	}
	/* All assignments to 'm' above should give the same value,
	 * so just use the last */
	matches += m;
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
    
    /*
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(ep->address)) {
	return 0;
    }
    
    ep->code[3] = (Uint) em_call_traced_function;
    ep->code[4] = (Uint) ep->address;
    ep->address = ep->code+3;
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
    return 1;
}

static void setup_bif_trace(int bif_index) {
    Export *ep = bif_export[bif_index];
    
    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(ep->code[4]);
    ep->code[4] = (Uint) bif_table[bif_index].traced;
}

static void set_trace_bif(int bif_index, void* match_prog) {
    Export *ep = bif_export[bif_index];
    
#ifdef HARDDEBUG
    erl_printf(CERR,"set_trace_bif: "); display(ep->code[0],CERR); erl_printf(CERR,":"); display(ep->code[1],CERR);
    erl_printf(CERR,"/%d\r\n",ep->code[2]);
#endif
    ASSERT(ExportIsBuiltIn(ep));
    MatchSetUnref(ep->match_prog_set);
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
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
     * Currently no trace support for native code.
     */
    if (erts_is_native_break(ep->address)) {
	return 0;
    }
    
    /*
     * Nothing to do, but the export entry matches.
     */

    return 1;
}

static void reset_bif_trace(int bif_index) {
    Export *ep = bif_export[bif_index];
    
    ASSERT(ExportIsBuiltIn(ep));
    ASSERT(ep->code[4]);
    ASSERT(! ep->match_prog_set);
    ASSERT(! erts_is_mtrace_bif((Uint *)ep->code+3, NULL, NULL));
    ep->code[4] = (Uint) bif_table[bif_index].f;
}

static void clear_trace_bif(int bif_index) {
    Export *ep = bif_export[bif_index];
    
#ifdef HARDDEBUG
    erl_printf(CERR,"clear_trace_bif: "); display(ep->code[0],CERR); erl_printf(CERR,":"); display(ep->code[1],CERR);
    erl_printf(CERR,"/%d\r\n",ep->code[2]);
#endif
    ASSERT(ExportIsBuiltIn(ep));
    MatchSetUnref(ep->match_prog_set);
    ep->match_prog_set = NULL;
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
	if (! is_small(arg2)) {
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
	Eterm* tp;
	if (is_not_tuple(arg2)) {
	    return THE_NON_VALUE;
	}
	tp = tuple_val(arg2);
	if ((*tp != make_arityval(2)) || is_not_small(*(tp+1)) || is_not_small(*(tp+2))) {
	    return THE_NON_VALUE;
        }
        new_seq_trace_token(p);
	if (build_result) {
	    hp = ArithAlloc(p,3);
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

void
new_seq_trace_token(Process* p)
{
    Eterm* hp;

    if (SEQ_TRACE_TOKEN(p) == NIL) {
	hp = ArithAlloc(p, 6);
	SEQ_TRACE_TOKEN(p) = TUPLE5(hp, make_small(0),		/* Flags  */ 
				    make_small(0),		/* Label  */
				    make_small(0),		/* Serial */
				    p->id, /* Internal pid */	/* From   */
				    make_small(p->seq_trace_lastcnt));
    }
}

BIF_RETTYPE seq_trace_info_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm item;
    Eterm res;
    Eterm* hp;
    Uint current_flag;

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
	} else if ((item == am_label) || (item == am_serial)) {
	    BIF_RET(NIL);
	} else {
	    goto error;
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
    } else {
	current_flag = 0;
    }

    if (current_flag) {
	res = unsigned_val(SEQ_TRACE_TOKEN_FLAGS(BIF_P)) & current_flag ? 
	    am_true : am_false;
    } else if (item == am_label) {
	res = SEQ_TRACE_TOKEN_LABEL(BIF_P);
    } else if (item  == am_serial) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, SEQ_TRACE_TOKEN_LASTCNT(BIF_P), SEQ_TRACE_TOKEN_SERIAL(BIF_P));
    } else {
    error:
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
		     SEQ_TRACE_PRINT, NIL, BIF_P);
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
		     SEQ_TRACE_PRINT, NIL, BIF_P);
    BIF_RET(am_true);
}
