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

#ifdef SEQ_TRACE
static void new_seq_trace_token(Process* p); /* help func for seq_trace_2*/
#endif
static Eterm check_tracee(Process* p, Eterm tracer, Eterm tracee);
static Eterm trace_info_pid(Process* p, Eterm pid_spec, Eterm key);
static Eterm trace_info_func(Process* p, Eterm pid_spec, Eterm key);


Eterm
suspend_process_1(Process* p, Eterm pid)
{
    Process* tracee;

    if (check_tracee(p, p->id, pid) == 0) {
	BIF_ERROR(p, BADARG);
    }
    tracee = process_tab[get_number(pid)];
    erl_suspend(tracee, NIL);
    BIF_RET(am_true);
}

Eterm
resume_process_1(Process* p, Eterm pid)
{
    Process* tracee;

    if (check_tracee(p, p->id, pid) == 0) {
	BIF_ERROR(p, BADARG);
    }
    tracee = process_tab[get_number(pid)];
    erl_resume(tracee);
    BIF_RET(am_true);
}

/*
 * Turn on/off call tracing for the given function(s).
 */
  
Eterm
trace_pattern_2(Process* p, Eterm MFA, Eterm Pattern)
{
    Eterm* tp;
    Eterm mfa[3];
    int i;
    int matches = 0;
    int specified = 0;
    int on;
    Binary* match_prog_set;

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

    /*
     * Match and patch all export entries.
     */

    if (!is_tuple(MFA)) {
	goto error;
    }
    tp = ptr_val(MFA);
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
    for (i = 0; i < export_list_size; i++) {
	Export* ep = export_list(i);
	int j;

	for (j = 0; j < specified && mfa[j] == ep->code[j]; j++) {
	    /* Empty loop body */
	}
	if (j == specified) {
	    if (on) {
		matches += erts_setup_func_trace(ep, match_prog_set);
	    } else {
		matches += erts_reset_func_trace(ep);
	    }
	}
    }
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
    case am_old_call_trace: return F_TRACE_CALLS_OLD;
    case am_call: return  F_TRACE_CALLS;
    case am_arity: return F_TRACE_ARITY_ONLY;
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
	item = CAR(ptr_val(list));
	if (is_atom(item) && (res = erts_trace_flag2bit(item)) != 0) {
	    mask |= res;
	} else if (is_tuple(item)) {
	    Eterm* tp = ptr_val(item);

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
	list = CDR(ptr_val(list));
    }
    if (is_not_nil(list)) {
	goto error;
    }

    /*
     * Set/reset the call trace flag for the given Pids.
     */

    if (is_pid(pid_spec)) {
	Process* tracee = process_tab[get_number(pid_spec)];

	if (check_tracee(p, tracer, pid_spec) == 0) {
	    goto error;
	}
	if (on) {
	    tracee->flags |= mask;
	} else {
	    tracee->flags &= ~mask;
	}
	tracee->tracer_proc = tracer;
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
		    Process* tracer_p = process_tab[get_number(proc->tracer_proc)];
		    if (!INVALID_PID(tracer_p, proc->tracer_proc)) {
			continue;
		    } else {
			proc->flags &= ~TRACE_FLAGS;
		    }
		}
		if (on) {
		    proc->flags |= mask;
		    proc->tracer_proc = tracer;
		} else {
		    proc->flags &= ~mask;
		    proc->tracer_proc = NIL;
		}
		matches++;
	    }
	}
	if (pid_spec == am_all || pid_spec == am_new) {
	    ok = 1;
	    if (on) {
		erts_default_process_flags |= mask;
		erts_default_tracer = tracer;
	    } else {
		erts_default_process_flags &= ~mask;
		erts_default_tracer = NIL;
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

    if (get_node(tracee) != THIS_NODE) {
    error:
	BIF_ERROR(p, BADARG);
    }

    if (get_number(tracee) >= max_process) {
	goto error;
    }

    tracee_ptr = process_tab[get_number(tracee)];
    if (INVALID_PID(tracee_ptr, tracee) || tracee_ptr->id == tracer) {
	goto error;
    }

    if (tracee_ptr->flags & TRACE_FLAGS && tracee_ptr->tracer_proc != tracer) {
	Process *tracer_p = process_tab[get_number(tracee_ptr->tracer_proc)];

	if (INVALID_PID(tracer_p, tracee_ptr->tracer_proc)) {
	    tracee_ptr->flags &= ~TRACE_FLAGS;
	    tracee_ptr->tracer_proc = NIL;
	} else {
	    cerr_pos = 0;
	    erl_printf(CBUF, "** can only have one tracer process per process\n");
	    send_error_to_logger(p->group_leader);
	    goto error;
	}
    }
    return tracee;
}

/*
 * Return information about a process or an external function being traced.
 */

Eterm
trace_info_2(Process* p, Eterm What, Eterm Key)
{
    if (is_atom(What) || is_pid(What)) {
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
    } else if (is_pid(pid_spec) && get_node(pid_spec) == THIS_NODE &&
	       get_number(pid_spec) < max_process) {
	Process* tracee = process_tab[get_number(pid_spec)];
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
	tracer_proc_ptr = process_tab[get_number(*tracer)];
	if (INVALID_PID(tracer_proc_ptr, *tracer)) {
	    *flagp &= ~TRACE_FLAGS;
	    *tracer = NIL;
	}
    }

    if (key == am_flags) {
	int num_flags = 13;
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
	FLAG(F_TRACE_CALLS_OLD, am_call);
	FLAG(F_TRACE_CALLS, am_call);
	FLAG(F_TRACE_PROCS, am_procs);
	FLAG(F_TRACE_SOS1, am_set_on_first_spawn);
	FLAG(F_TRACE_SOL, am_set_on_link);
	FLAG(F_TRACE_SOL1, am_set_on_first_link);
	FLAG(F_TRACE_SCHED, am_running);
	FLAG(F_TRACE_GC, am_garbage_collection);
	FLAG(F_TIMESTAMP, am_timestamp);
	FLAG(F_TRACE_ARITY_ONLY, am_arity);
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

static Eterm
trace_info_func(Process* p, Eterm func_spec, Eterm key)
{
    Eterm* tp;
    Eterm* hp;
    Export e;
    Export* ep;
    Eterm traced = 0;

    if (!is_tuple(func_spec)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    tp = ptr_val(func_spec);
    if (tp[0] != make_arityval(3)) {
	goto error;
    }
    if (!is_atom(tp[1]) || !is_atom(tp[2]) || !is_small(tp[3])) {
	goto error;
    }
    e.code[0] = tp[1];
    e.code[1] = tp[2];
    e.code[2] = signed_val(tp[3]);
    if ((ep = hash_get(&export_table.htable, (void*) &e)) == NULL) {
	return am_undefined;
    }

    switch (erts_trace_state(ep)) {
    case -1: return am_undefined;
    case 0: traced = am_false; break;
    case 1: traced = am_true; break;
    }

    if (key == am_traced) {
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, traced);
    } else if (key == am_match_spec) {
	Eterm match_spec = NIL;
	if (ep->match_prog_set) {
	    match_spec = MatchSetGetSource(ep->match_prog_set);
	}
	hp = HAlloc(p, 3);
	return TUPLE2(hp, key, match_spec);
    } else {
	goto error;
    }
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
#ifndef SEQ_TRACE
    BIF_ERROR(BIF_P, BADARG);
#else
    Eterm res;
    if ((res = erts_seq_trace(BIF_P, BIF_ARG_1, BIF_ARG_2, 1)) == 
	(Eterm) 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(res);
#endif
}

#ifdef SEQ_TRACE
Eterm erts_seq_trace(Process *p, Eterm arg1, Eterm arg2, 
			  int build_result)
{
    Eterm flags;
    Eterm old_value = am_true;
    Eterm* hp;
    int current_flag;

    if (!is_atom(arg1)) {
	return (Eterm) 0;
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
	    return (Eterm) 0;
	}
	return old_value;
    }
    else if (arg1 == am_label) {
	if (!(is_atom(arg2) || is_small(arg2))) {
	    return (Eterm) 0;
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
	    return (Eterm) 0;
	}
	tp = ptr_val(arg2);
	if ((*tp != make_arityval(2)) || is_not_small(*(tp+1)) || is_not_small(*(tp+2))) {
	    return (Eterm) 0;
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
	    return (Eterm) 0;
        }
	if (build_result) {
	    old_value = SEQ_TRACE_TOKEN(p);
	}
        SEQ_TRACE_TOKEN(p) = NIL;
        return old_value;
    }
    else {
	return (Eterm) 0;
    }
}
#endif

#ifdef SEQ_TRACE
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
#endif

BIF_RETTYPE seq_trace_info_1(BIF_ALIST_1)
BIF_ADECL_1
{
#ifndef SEQ_TRACE
    BIF_ERROR(BIF_P, BADARG);
#else
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
#endif
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
#ifndef SEQ_TRACE
    BIF_ERROR(BIF_P, BADARG);
#else
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) 
	BIF_RET(am_false);
    if (!(is_atom(BIF_ARG_1) || is_small(BIF_ARG_1))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (SEQ_TRACE_TOKEN_LABEL(BIF_P) != BIF_ARG_1)
	BIF_RET(am_false);
    seq_trace_update_send(BIF_P);
    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_2, SEQ_TRACE_PRINT, NIL);
    BIF_RET(am_true);
#endif
}
