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
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

extern int fixed_deletion_desc;

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#if defined(HAVE_SOLARIS_SPARC_PERFMON)
# include <sys/ioccom.h>
# define PERFMON_SETPCR			_IOW('P', 1, unsigned long long)
# define PERFMON_GETPCR			_IOR('P', 2, unsigned long long)
#endif

static Eterm
make_bin_list(Process* p, ProcBin* pb)
{
    Eterm res = NIL;
    Eterm* hp;
    Eterm tuple;

    while (pb) {
	hp = HAlloc(p, 4+2);
	tuple = TUPLE3(hp, make_small_or_big((Uint)pb->val, p),
		       make_small_or_big(pb->val->orig_size, p),
		       make_small(pb->val->refc));
	hp += 4;
	res = CONS(hp, tuple, res);
	pb = pb->next;
    }
    return res;
}

Eterm
process_info_1(Process* p, Eterm pid)
{
    static Eterm keys[] = {
	am_current_function,
	am_initial_call,
	am_status,
	am_message_queue_len,
	am_messages,
	am_links,
	am_dictionary,
	am_trap_exit,
	am_error_handler,
	am_priority,
	am_group_leader,
	am_heap_size,
	am_stack_size,
	am_reductions,
	am_garbage_collection,
    };
    Eterm items[ASIZE(keys)];
    Eterm result = NIL;
    Eterm tmp;
    Eterm* hp;
    int i;

    /*
     * Collect all information about the process.
     */

    for (i = 0; i < ASIZE(keys); i++) {
	Eterm item;

	item = process_info_2(p, pid, keys[i]);
	if (is_non_value(item)) {
	    return THE_NON_VALUE;
	}
	if (item == am_undefined) {
	    return am_undefined;
	}
	items[i] = item;
    }

    /*
     * The dollar dictionary is special. We will only show it if its is non-empty.
     */

    tmp = process_info_2(p, pid, am_DollarDictionary);

    hp = HAlloc(p, 2*(ASIZE(keys)+2));

    if (is_tuple(tmp)) {
	Eterm* tp = tuple_val(tmp);
	if (tp[2] != NIL) {
	    result = CONS(hp, tmp, result);
	    hp += 2;
	}
    }

    /*
     * Build the resulting list.
     */

    for (i = ASIZE(keys) - 1; i >= 0; i--) {
	result = CONS(hp, items[i], result);
	hp += 2;
    }

    /*
     * Registered name is also special.
     */
    
    tmp = process_info_2(p, pid, am_registered_name);
    if (is_tuple(tmp)) {
	result = CONS(hp, tmp, result);
    }
    return result;
}

BIF_RETTYPE process_info_2(BIF_ALIST_2) 
BIF_ADECL_2
{
    uint32 item, term, list;
    Eterm res;
    Process *rp;
    Eterm* hp;
    int i, j;
    Eterm pid = BIF_ARG_1;

    if (is_not_pid(pid) || (pid_node(pid) != THIS_NODE) ||
	(pid_number(pid) >= max_process)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    i = pid_creation(pid);
    if ((i != this_creation) && (i != 0))
	BIF_RET(am_undefined);

    if (is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);

    item = BIF_ARG_2;
    rp = process_tab[pid_number(BIF_ARG_1)];

    /* if the process is not active return undefined */
    if (INVALID_PID(rp, BIF_ARG_1))
    {
	if (rp != NULL && rp->status == P_EXITING)
	    ;
	else
	    BIF_RET(am_undefined);
    }
    res = NIL;

    if (item == am_registered_name) {
	if (rp->reg != NULL) {
	    hp = HAlloc(BIF_P, 3);
	    res = make_atom(rp->reg->name);
	}
	else if (rp->reg_atom != -1 && rp->status == P_EXITING) {
	    hp = HAlloc(BIF_P, 3);
	    res = make_atom(rp->reg_atom);
	}
	else {
	    BIF_RET(NIL);
	}
    } else if (item == am_current_function) {
	if (rp->current == NULL) {
	    rp->current = find_function_from_pc(rp->i);
	}
	if (rp->current == NULL) {
	    hp = HAlloc(BIF_P, 3);
	    res = am_undefined;
	} else {
	    uint32* current;

	    if (rp->current[0] == am_erlang &&
		rp->current[1] == am_process_info &&
		(rp->current[2] == 1 || rp->current[2] == 2) &&
		(current = find_function_from_pc(rp->cp)) != NULL) {

		/*
		 * The current function is erlang:process_info/2,
		 * which is not the answer that the application want.
		 * We will use the function pointed into by rp->cp
		 * instead.
		 */

		rp->current = current;
	    }

	    hp = HAlloc(BIF_P, 3+4);
	    res = TUPLE3(hp, rp->current[0],
			 rp->current[1], make_small(rp->current[2]));
	    hp += 4;
	}
    } else if (item == am_initial_call) {
	hp = HAlloc(BIF_P, 3+4);
	res = TUPLE3(hp,
		     rp->initial[INITIAL_MOD],
		     rp->initial[INITIAL_FUN],
		     make_small(rp->initial[INITIAL_ARI]));
	hp += 4;
    } else if (item == am_status ) {
	hp = HAlloc(BIF_P, 3);
	switch (rp->status) {
	case P_RUNABLE:
	    res = am_runnable;
	    break;
	case P_WAITING:
	    res = am_waiting;
	    break;
	case P_RUNNING:
	    res = am_running;
	    break;
	case P_SUSPENDED:
	    res = am_suspended;
	    break;
	case P_EXITING:
	    res = am_exiting;
	    break;
	default:
	    res = am_undefined;
	}
    } else if (item == am_messages) {
	ErlMessage* mp;
	uint32* cons;
	int n = rp->msg.len;
	uint32 size;

	if (n == 0) {
	    hp = HAlloc(BIF_P, 3);
	    res = NIL;
	} else {
	    size = 0;
	    if (rp != BIF_P) {
		mp = rp->msg.first;
		while(mp != NULL) {
		    size += size_object(mp->mesg);
		    mp = mp->next;
		}
	    }
	    hp = HAlloc(BIF_P, 3 + size + 2*n);
	    hp += 2*n;		/* skip the list !!! */
	    cons = hp - 2;
	    res = make_list(cons); /* first cons cell */
	    /* Build with back-pointers (as cons whould have done) */
	    mp = rp->msg.first;
	    while(mp != NULL) {
		if (rp == BIF_P)
		    cons[0] = mp->mesg;	/* write head */
		else {
		    size = size_object(mp->mesg); /* XXX I know */
		    cons[0] = copy_struct(mp->mesg, size, &hp, &BIF_P->off_heap);
		}
		cons -= 2;	/* next cell */
		cons[3] = make_list(cons); /* write tail */
		mp = mp->next;
	    }
	    cons[3] = NIL; 
	}
    } else if (item == am_message_queue_len) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(rp->msg.len);
    } else if (item == am_links) {
	int n = 0;
	ErlLink* lnk;

	lnk = rp->links;
	while(lnk != NULL) {
	    if (lnk->type == LNK_LINK)
		n++;
	    lnk = lnk->next;
	}
	hp = HAlloc(BIF_P, 3 + 2*n);
	lnk = rp->links;
	while(lnk != NULL) {
	    if (lnk->type == LNK_LINK) {
		res = CONS(hp, lnk->item, res);
		hp += 2;
	    }
	    lnk = lnk->next;
	}
    } else if (item == am_monitors) {
	int n = 0;
	ErlLink* lnk;

	/* lnk->item is the monitor link origin end */
	lnk = rp->links;
	while(lnk != NULL) {
	    if ((lnk->type == LNK_LINK1)
		&& (rp->id == lnk->item) ) {
		n += 5; /* For a cons cell and a 2-tuple */
		if (is_small(lnk->data))
		    /* Monitor by name. Will need an additional 2-tuple. */
		    n += 3;
		else if (is_not_pid(lnk->data))
		    ASSERT(0);
	    }
	    lnk = lnk->next;
	}
	hp = HAlloc(BIF_P, 3 + n);
	lnk = rp->links;
	while(lnk != NULL) {
	    if ((lnk->type == LNK_LINK1)
		&& (rp->id == lnk->item) ) {
		if (is_small(lnk->data)) {
		    /* Monitor by name. 
		     * Build {process, {Name, Node}} and cons it. 
		     */
		    Eterm     t1, t2;
		    Sint      slot = unsigned_val(lnk->data);
		    ErlLink** dist_lnkp =
			find_link_by_ref(&dist_addrs[slot].links, &lnk->ref);
		    ASSERT(dist_lnkp != NULL);
		    t1 = TUPLE2(hp, (*dist_lnkp)->data, 
			       dist_addrs[slot].sysname );
		    hp += 3;
		    t2 = TUPLE2(hp, am_process, t1);
		    hp += 3;
		    res = CONS(hp, t2, res);
		    hp += 2;
		} else if (is_pid(lnk->data)) {
		    /* Monitor by pid. Build {process, Pid} and cons it. */
		    Eterm t;
		    t = TUPLE2(hp, am_process, lnk->data);
		    hp += 3;
		    res = CONS(hp, t, res);
		    hp += 2;
		} else
		    ASSERT(0);
	    }
	    lnk = lnk->next;
	}
    } else if (item == am_monitored_by) {
	int n = 0;
	ErlLink* lnk;

	/* lnk->item is the monitor link origin end */
	lnk = rp->links;
	while(lnk != NULL) {
	    if ((lnk->type == LNK_LINK1)
		&& (rp->id != lnk->item) )
		n++;
	    lnk = lnk->next;
	}
	hp = HAlloc(BIF_P, 3 + 2*n);
	lnk = rp->links;
	while(lnk != NULL) {
	    if ((lnk->type == LNK_LINK1)
		&& (rp->id != lnk->item) ) {
		res = CONS(hp, lnk->item, res);
		hp += 2;
	    }
	    lnk = lnk->next;
	}
    } else if (item == am_dictionary) {
	res = dictionary_copy(BIF_P, rp->dictionary);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_DollarDictionary) {
	res = dictionary_copy(BIF_P, rp->debug_dictionary);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_trap_exit) {
	hp = HAlloc(BIF_P, 3);
	if (rp->flags  & F_TRAPEXIT)
	    res = am_true;
	else
	    res = am_false;
    } else if (item == am_error_handler ) {
	hp = HAlloc(BIF_P, 3);
	res = rp->error_handler;
    } else if (item == am_heap_size) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(rp->heap_sz);
    } else if (item == am_stack_size) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(rp->hend - rp->stop);
    } else if (item == am_memory) { /* Memory consumed in bytes */
	Uint size = 0;
	ErlLink* lnk;

	lnk = rp->links;
	while (lnk != NULL) {
	    size += sizeof(ErlLink);
	    lnk = lnk->next;
	}
	size += (rp->heap_sz + rp->mbuf_sz) * sizeof(Eterm) + sizeof(Process);
	if (rp->old_heap != NULL && rp->old_hend != NULL) {
	    size += (rp->old_hend - rp->old_heap) * sizeof(Eterm);
	}
	if (rp->reg) {
	    size += sizeof(struct reg_proc);
	}
	size += rp->msg.len * sizeof(ErlMessage);
	hp = HAlloc(BIF_P, 3);
	res = make_small_or_big(size, BIF_P);
    } else if (item == am_garbage_collection){
	hp = HAlloc(BIF_P, 3+2+3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(rp->max_gen_gcs));
	hp += 3;
	res = CONS(hp, res, NIL);
	hp += 2;
    } else if (item == am_group_leader) {
	hp = HAlloc(BIF_P, 3);
	res = rp->group_leader;
    } else if (item == am_reductions) {
	uint32 reds;

	hp = HAlloc(BIF_P, 3);
	reds = rp->reds + erts_current_reductions(BIF_P, rp);
	res = make_small_or_big(reds, BIF_P);
    } else if (item == am_priority) {
	hp = HAlloc(BIF_P, 3);
	switch(rp->prio) {
	case PRIORITY_MAX:
	    res = am_max; break;
	case PRIORITY_HIGH:
	    res = am_high; break;
	case PRIORITY_NORMAL:
	    res = am_normal; break;
	case PRIORITY_LOW:
	    res = am_low; break;
	}
    } else if (item == am_trace) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(rp->flags & TRACE_FLAGS);
    } else if (item == am_binary) {
	res = make_bin_list(BIF_P, rp->off_heap.mso);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_sequential_trace_token) {
	uint32 size = size_object(rp->seq_trace_token);
	hp = HAlloc(BIF_P, size+3);
	res = copy_struct(rp->seq_trace_token, size, &hp, &BIF_P->off_heap);
    } else if (item == am_exit) {
	uint32 size;
	if (rp->status == P_EXITING) {
	    size = size_object(rp->fvalue);
	    hp = HAlloc(BIF_P, 3+size);
	    res = copy_struct(rp->fvalue, size, &hp, &BIF_P->off_heap);
	} else
	    BIF_RET(NIL);
    } else if (item == am_catchlevel) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(catchlevel(BIF_P));
    } else if (item == am_backtrace) {
	cerr_pos = 0;
	stack_dump2(rp, CBUF);
	res = new_binary(BIF_P, tmp_buf, cerr_pos);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_last_calls) {
	if (rp->ct == NULL) {
	    hp = HAlloc(BIF_P, 3);
	    res = am_false;
	} else {
	    hp = HAlloc(BIF_P, rp->ct->n*(2+4) + 3);
	    /* one cons cell and a 3-struct,
	       and the 2-tuple below */
	    list = NIL;
	    for (i = 0; i < rp->ct->n; i++) {
		j = rp->ct->cur - i - 1;
		if (j < 0)
		    j += rp->ct->len;
		if (rp->ct->ct[j] == &exp_send)
		    term = am_send;
		else if (rp->ct->ct[j] == &exp_receive)
		    term = am_receive;
		else if (rp->ct->ct[j] == &exp_timeout)
		    term = am_timeout;
		else {
		    term = TUPLE3(hp,
				  rp->ct->ct[j]->code[0],
				  rp->ct->ct[j]->code[1],
				  make_small(rp->ct->ct[j]->code[2]));
		    hp += 4;
		}
		list = CONS(hp, term, list);
		hp += 2;
	    }
	    res = list;
	}
    } else {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(TUPLE2(hp, item, res));
}

/*
 * This function takes care of calls to erlang:system_info/1 when the argument
 * is a tuple.
 */
static BIF_RETTYPE
info_1_tuple(Process* BIF_P,	/* Pointer to current process. */
	     uint32* tp,	/* Pointer to first element in tuple */
	     int arity)		/* Arity of tuple (untagged). */
{
    Eterm sel;

    sel = *tp++;

    if (0) {
	;
#ifdef INSTRUMENT
    } else if (sel == am_allocated) {
	int len;

	if (!is_string(*tp))
	    return THE_NON_VALUE;
       
	if ((len = intlist_to_buf(*tp, tmp_buf, TMP_BUF_SIZE-1)) < 0)
	    return THE_NON_VALUE;
	tmp_buf[len] = '\0';

	if (dump_memory_data(tmp_buf))
	    BIF_RET(am_true);
	else
	    return THE_NON_VALUE;	/* Return {error, Errno} instead? */
#endif
#ifdef PURIFY
    } else if (sel == am_purify) {
	if (*tp == am_memory) {
	    BIF_RET(make_small_or_big(purify_new_leaks(), BIF_P));
	} else if (*tp == am_fd) {
	    BIF_RET(make_small_or_big(purify_new_fds_inuse(), BIF_P));
	} else if (*tp == am_running) {
	    BIF_RET(purify_is_running() ? am_true : am_false);
	} else if (is_list(*tp)) {
	    int iopos = 0;
	    int r;

	    r = io_list_to_buf(*tp, (char*) tmp_buf, &iopos, TMP_BUF_SIZE - 5);
	    if (r == 0) {
		tmp_buf[iopos] = 0;
		purify_printf("%s\n", tmp_buf);
	    } else {
		return THE_NON_VALUE;
	    }
	    BIF_RET(am_true);
	}
#endif
#ifdef QUANTIFY
    } else if (sel == am_quantify) {
	if (*tp == am_clear) {
	    quantify_clear_data();
	    BIF_RET(am_true);
	} else if (*tp == am_start) {
	    quantify_start_recording_data();
	    BIF_RET(am_true);
	} else if (*tp == am_stop) {
	    quantify_stop_recording_data();
	    BIF_RET(am_true);
	} else if (*tp == am_running) {
	    BIF_RET(quantify_is_running() ? am_true : am_false);
	}
#endif
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (sel == am_ultrasparc_set_pcr) {
	unsigned long long tmp;
	int fd;
	int rc;

	if (arity != 2 || !is_small(*tp)) {
	    return THE_NON_VALUE;
	}
	tmp = signed_val(*tp);
	if ((fd = open("/dev/perfmon", O_RDONLY)) == -1) {
	    BIF_RET(am_false);
	}
	rc = ioctl(fd, PERFMON_SETPCR, &tmp);
	close(fd);
	if (rc < 0) {
	    BIF_RET(am_false);
	}
	BIF_RET(am_true);
#endif
    }
    return THE_NON_VALUE;
}

BIF_RETTYPE info_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return system_info_1(BIF_P, BIF_ARG_1);
}


BIF_RETTYPE system_info_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm res;
    Eterm* hp;
    Eterm val;
    unsigned count;
    int i;

    cerr_pos = 0;

    if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	uint32 arity = *tp++;
	res = info_1_tuple(BIF_P, tp, arityval(arity));
	if (is_non_value(res))
	    goto error;
	return res;
    } else if (BIF_ARG_1 == am_sequential_tracer) {
	if (is_pid(system_seq_tracer) || is_port(system_seq_tracer)) {
	    val = system_seq_tracer;
	} else {
	    val = am_false;
	}
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_sequential_tracer, val);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection){
	hp = HAlloc(BIF_P, 3+2);
	res = TUPLE2(hp, am_fullsweep_after, make_small(erts_max_gen_gcs));
	hp += 3;
	res = CONS(hp, res, NIL);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_fullsweep_after){
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(erts_max_gen_gcs));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_process_count) {
	count = 0;
	for (i = 0; i < max_process; i++) {
	    if (process_tab[i] != NULL) {
		count++;
	    }
	}
	BIF_RET(make_small(count));
    } else if (BIF_ARG_1 == am_process_limit) {
	BIF_RET(make_small(max_process));
    } else if (BIF_ARG_1 == am_info) {
	info(CBUF);
    } else if (BIF_ARG_1 == am_procs)
	process_info(CBUF);
    else if (BIF_ARG_1 == am_loaded)
	loaded(CBUF);
    else if (BIF_ARG_1 == am_dist)
	distribution_info(CBUF);
    else if (BIF_ARG_1 == am_system_version) {
	int n;

	sys_strcpy((char*)tmp_buf, "Erlang (");
	n = sys_strlen((char*)tmp_buf);
	sys_strcpy((char*)(tmp_buf+n), EMULATOR);
	n += sys_strlen(EMULATOR);
	sys_strcpy((char*)(tmp_buf+n), ") emulator version ");
	n += 19;
	sys_strcpy((char*)(tmp_buf+n), ERLANG_VERSION);
	n += sizeof(ERLANG_VERSION)-1;
	/* OTP_RELEASE is *only* for OTP binary releases */
#ifndef OTP_RELEASE
	sys_strcpy((char *)(tmp_buf+n), " [source]");
	n = strlen(tmp_buf);
#endif	
#ifdef ET_DEBUG
	sys_strcpy((char *)(tmp_buf+n), " [type-assertions]");
	n = strlen(tmp_buf);
#endif	
#ifdef DEBUG
	sys_strcpy((char *)(tmp_buf+n), " [debug-compiled]");
	n = strlen(tmp_buf);
#endif	
#ifdef INSTRUMENT
	sys_strcpy((char *)(tmp_buf+n), " [instrumented]");
	n = strlen(tmp_buf);
#endif	
#ifdef USE_THREADS
	sys_strcpy((char *)(tmp_buf+n), " [threads]");
	n = strlen(tmp_buf);
#endif	
	tmp_buf[n] = '\n';
	tmp_buf[n+1] = '\0';
	n++;
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
    }
#ifdef INSTRUMENT
    else if (BIF_ARG_1 == am_allocated) {
       uint32 val;

       val = collect_memory(BIF_P);
       BIF_RET(val);
    }
#endif
    else if (BIF_ARG_1 == am_allocated_areas) {
#undef DEBUG_AA_2TUPS
#ifdef DEBUG
#define DEBUG_AA_2TUPS 1
#else
#define DEBUG_AA_2TUPS 0
#endif
#undef  NO_AA_2TUPS
#define NO_AA_2TUPS  (7 + DEBUG_AA_2TUPS)
#undef  NO_AA_3TUPS
#define NO_AA_3TUPS  11
#undef  NO_AA_TUPS
#define NO_AA_TUPS   (NO_AA_2TUPS+NO_AA_3TUPS)
#undef  AA_WORDS
#define AA_WORDS     (2*NO_AA_TUPS+3*NO_AA_2TUPS+4*NO_AA_3TUPS)

      uint32 tuples[NO_AA_TUPS];
      int i;
      int code_sz = 0;
#ifdef DEBUG
      uint32 *endp;
#endif

      for (i = 0; i < module_code_size; i++) {
        if (module_code(i) != NULL &&
            ((module_code(i)->code_length != 0) ||
             (module_code(i)->old_code_length != 0))) {
          code_sz += module_code(i)->code_length;
          if (module_code(i)->old_code_length != 0) {
            code_sz += module_code(i)->old_code_length;
          }
        }
      }

      hp = HAlloc(BIF_P, AA_WORDS);

#ifdef DEBUG
      endp = hp + AA_WORDS;
#endif

      i = 0;
      tuples[i++] = TUPLE2(hp,
                           am_static,
                           make_small_or_big(MAXDIST * sizeof(DistEntry)
                                             /* Distribution table */

                                             + max_process * sizeof(Process*)
                                             /* Process table */

                                             + erl_max_ports * sizeof(Port)
                                             /* Port table */

                                             + TMP_BUF_SIZE * sizeof(byte)
                                             /* Tmp buffer */

                                             + 14 /* XXX */ * sizeof(uint32)
                                             /* Tmp buffer 
                                                (dmem in dist.c) */

                                             + 64+1 /* XXX */
                                             /* fix alloc descs
                                                (dbuf in erl_message.c) */ 

                                             /* XXX continue ... */,
                                             BIF_P));
      hp += 3;
      tuples[i++] = TUPLE3(hp,
                           am_atom_space,
                           make_small_or_big(reserved_atom_space, BIF_P),
                           make_small_or_big(atom_space, BIF_P));
      hp += 4;
      tuples[i++] = TUPLE2(hp,
                           am_binary,
                           make_small_or_big(tot_bin_allocated, BIF_P));
      hp += 3;
      tuples[i++] = TUPLE2(hp,
                           am_atom_table,
                           make_small_or_big(index_table_sz(&atom_table),
                                             BIF_P));
      hp += 3;
      tuples[i++] = TUPLE2(hp,
                           am_module_table,
                           make_small_or_big(index_table_sz(&module_table),
                                             BIF_P));
      hp += 3;
      tuples[i++] = TUPLE2(hp,
                           am_export_table,
                           make_small_or_big(index_table_sz(&export_table),
                                             BIF_P));
      hp += 3;
      tuples[i++] = TUPLE2(hp,
                           am_register_table,
                           make_small_or_big(hash_table_sz(&process_reg),
                                             BIF_P));
      hp += 3;
      tuples[i++] = TUPLE2(hp,
                           am_loaded_code,
                           make_small_or_big(code_sz, BIF_P));
      hp += 3;
      tuples[i++] = TUPLE3(hp,
                           am_process_desc,
                           make_small_or_big(fix_info(process_desc), BIF_P),
                           make_small_or_big(fix_used(process_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_table_desc,
                           make_small_or_big(fix_info(table_desc), BIF_P),
                           make_small_or_big(fix_used(table_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_link_desc,
                           make_small_or_big(fix_info(link_desc), BIF_P),
                           make_small_or_big(fix_used(link_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_atom_desc,
                           make_small_or_big(fix_info(atom_desc), BIF_P),
                           make_small_or_big(fix_used(atom_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_export_desc,
                           make_small_or_big(fix_info(export_desc), BIF_P),
                           make_small_or_big(fix_used(export_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_module_desc,
                           make_small_or_big(fix_info(module_desc), BIF_P),
                           make_small_or_big(fix_used(module_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_preg_desc,
                           make_small_or_big(fix_info(preg_desc), BIF_P),
                           make_small_or_big(fix_used(preg_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_mesg_desc,
                           make_small_or_big(fix_info(mesg_desc), BIF_P),
                           make_small_or_big(fix_used(mesg_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_plist_desc,
                           make_small_or_big(fix_info(plist_desc), BIF_P),
                           make_small_or_big(fix_used(plist_desc), BIF_P));
      hp += 4;
      tuples[i++] = TUPLE3(hp,
                           am_fixed_deletion_desc,
                           make_small_or_big(fix_info(fixed_deletion_desc),
                                             BIF_P),
                           make_small_or_big(fix_used(fixed_deletion_desc),
                                             BIF_P));
      hp += 4;
#ifdef DEBUG
      tuples[i++] = TUPLE2(hp,
                           am_SYSTEM,
                           make_small_or_big(tot_allocated, BIF_P));
      hp += 3;
#endif

      ASSERT(i == NO_AA_TUPS);

      res = NIL;
      for(i--; i >= 0; i--) {
        res = CONS(hp, tuples[i], res);
        hp += 2;
      }
#ifdef DEBUG
      ASSERT(endp == hp);
#endif
      BIF_RET(res);
#undef DEBUG_AA_2TUPS
#undef NO_AA_2TUPS
#undef NO_AA_3TUPS
#undef NO_AA_TUPS
#undef AA_WORDS
    }
    else if (BIF_ARG_1 == am_os_type) {
       uint32 type = am_atom_put(os_type, strlen(os_type));
       uint32 flav, tup;

       os_flavor(tmp_buf, TMP_BUF_SIZE);
       flav = am_atom_put(tmp_buf, strlen(tmp_buf));
       hp = HAlloc(BIF_P, 3);
       tup = TUPLE2(hp, type, flav);
       BIF_RET(tup);
    }
    else if (BIF_ARG_1 == am_thread_pool_size) {
	extern int erts_async_max_threads;
	int n;
	
#ifdef USE_THREADS
	n = erts_async_max_threads;
#else
	n = 0;
#endif
	BIF_RET(make_small(n));
    }
    else if (BIF_ARG_1 == am_os_version) {
       int major, minor, build;
       uint32 tup;

       os_version(&major, &minor, &build);
       hp = HAlloc(BIF_P, 4);
       tup = TUPLE3(hp,
		    make_small(major),
		    make_small(minor),
		    make_small(build));
       BIF_RET(tup);
    }
    else if (BIF_ARG_1 == am_version) {
	int n = strlen(ERLANG_VERSION);
	hp = HAlloc(BIF_P, ((sizeof ERLANG_VERSION)-1) * 2);
	BIF_RET(buf_to_intlist(&hp, (byte*)ERLANG_VERSION, n, NIL));
    }
    else if (BIF_ARG_1 == am_machine) {
	int n = strlen(EMULATOR);
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, (byte*)EMULATOR, n, NIL));
    }
    else if (BIF_ARG_1 == am_garbage_collection) {
	BIF_RET(am_generational);
    } else if (BIF_ARG_1 == am_instruction_counts) {
	int i;
	hp = HAlloc(BIF_P, num_instructions*5);
	res = NIL;
	for (i = num_instructions-1; i >= 0; i--) {
	    Eterm tuple;
	    Eterm atom = am_atom_put(opc[i].name, strlen(opc[i].name));
	    Eterm count = make_small_or_big(opc[i].count, BIF_P);

	    tuple = TUPLE2(hp, atom, count);
	    hp += 3;
	    res = CONS(hp, tuple, res);
	    hp += 2;
	}
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_heap_sizes) {
	return erts_heap_sizes(BIF_P);
#if defined(__GNUC__) && defined(HAVE_SOLARIS_SPARC_PERFMON)
    } else if (BIF_ARG_1 == am_ultrasparc_read_tick1) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_ultrasparc_read_tick2) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3410000;" /* rd %tick, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_ultrasparc_read_pic1) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	hp = HAlloc(BIF_P, 5);
	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_ultrasparc_read_pic2) {
	register unsigned high asm("%l0");
	register unsigned low asm("%l1");

	asm volatile (".word 0xa3444000;" /* rd %asr17, %l1 */
		      ".word 0xa1347020" /* srlx  %l1, 0x20, %l0 */
		      : "=r" (high), "=r" (low));
	hp = HAlloc(BIF_P, 5);
	res = TUPLE4(hp, make_small(high >> 16),
		     make_small(high & 0xFFFF),
		     make_small(low >> 16),
		     make_small(low & 0xFFFF));
	BIF_RET(res);
#endif
    } else {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    res = new_binary(BIF_P, tmp_buf, cerr_pos);
    BIF_RET(res);
}

Eterm
port_info_1(Process* p, Eterm pid)
{
    static Eterm keys[] = {
	am_name,
	am_links,
	am_id,
	am_connected,
	am_input,
	am_output,
    };
    Eterm items[ASIZE(keys)];
    Eterm result = NIL;
    Eterm* hp;
    int i;

    /*
     * Collect all information about the port.
     */

    for (i = 0; i < ASIZE(keys); i++) {
	Eterm item;

	item = port_info_2(p, pid, keys[i]);
	if (is_non_value(item)) {
	    return THE_NON_VALUE;
	}
	if (item == am_undefined) {
	    return am_undefined;
	}
	items[i] = item;
    }

    /*
     * Build the resulting list.
     */

    hp = HAlloc(p, 2*ASIZE(keys));
    for (i = ASIZE(keys) - 1; i >= 0; i--) {
	result = CONS(hp, items[i], result);
	hp += 2;
    }
    
    return result;
}


/**********************************************************************/ 
/* Return information on ports */
/* Info:
**    id          Port index
**    connected   (Pid)
**    links       List of pids
**    name        String
**    input       Number of bytes input from port program
**    output      Number of bytes output to the port program
*/

BIF_RETTYPE port_info_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32 portid = BIF_ARG_1;
    uint32 item = BIF_ARG_2;
    uint32 res;
    uint32* hp;
    int i;
    int count;
    int portix;

    if (is_not_port(portid) || (port_node(portid) != THIS_NODE) ||
	((portix = port_index(portid)) >= erl_max_ports)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    i = port_creation(portid);
    if (((i != this_creation) && (i != 0)) || (erts_port[portix].status == FREE)) {
	BIF_RET(am_undefined);
    }

    if (item == am_id) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(port_number(portid));
    }
    else if (item == am_links) {
	int n = 0;
	ErlLink* lnk;

	lnk = erts_port[portix].links;
	while(lnk != NULL) {
	    if (lnk->type == LNK_LINK)
		n++;
	    lnk = lnk->next;
	}
	hp = HAlloc(BIF_P, 3 + 2*n);

	lnk = erts_port[portix].links;
	res = NIL;
	while(lnk != NULL) {
	    if (lnk->type == LNK_LINK) {
		res = CONS(hp, lnk->item, res);
		hp += 2;
	    }
	    lnk = lnk->next;
	}
    }
    else if (item == am_name) {
	count = sys_strlen(erts_port[portix].name);

	hp = HAlloc(BIF_P, 3 + 2*count);
	res = buf_to_intlist(&hp,(byte*)erts_port[portix].name,count,NIL);
    }
    else if (item == am_connected) {
	hp = HAlloc(BIF_P, 3);
	res = erts_port[portix].connected;
    }
    else if (item == am_input) {
	hp = HAlloc(BIF_P, 3);
	res = make_small_or_big(erts_port[portix].bytes_in, BIF_P);
    }
    else if (item == am_output) {
	hp = HAlloc(BIF_P, 3);
	res = make_small_or_big(erts_port[portix].bytes_out, BIF_P);
    }
    else
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(TUPLE2(hp, item, res));
}


Eterm
fun_info_2(Process* p, Eterm fun, Eterm what)
{
    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);
	Eterm val;
	Eterm* hp;

	switch (what) {
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = funp->creator;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = make_atom(funp->modp->module);
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->index);
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = funp->uniq;
	    break;
	case am_env:
	    {
		Uint num_free = funp->num_free;
		int i;

		hp = HAlloc(p, 3 + 2*num_free);
		val = NIL;
		for (i = num_free-1; i >= 0; i--) {
		    val = CONS(hp, funp->env[i], val);
		    hp += 2;
		}
	    }
	    break;
	default:
	    goto error;
	}
	return TUPLE2(hp, what, val);
    }

 error:
    BIF_ERROR(p, BADARG);
}


/* this is a general call which return some possibly useful information */

BIF_RETTYPE statistics_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res;
    uint32* hp;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    if (BIF_ARG_1 == am_context_switches) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, make_small_or_big(context_switches, BIF_P), SMALL_ZERO);
	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_garbage_collection) {
	hp = HAlloc(BIF_P, 4);
	res = TUPLE3(hp, make_small_or_big(garbage_cols, BIF_P),
		     make_small_or_big(reclaimed, BIF_P),
		     SMALL_ZERO);
	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_reductions) {
	uint32 reds;
	uint32 b1, b2;

	reds = reductions + erts_current_reductions(BIF_P, BIF_P);
	b1 = make_small_or_big(reds, BIF_P);
	b2 = make_small_or_big(reds - last_reds, BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2); 
	last_reds  = reds;
	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_runtime) {
	unsigned long u1, u2, dummy;
	uint32 b1, b2;
	elapsed_time_both(&u1,&dummy,&u2,&dummy);
	b1 = make_small_or_big(u1,BIF_P);
	b2 = make_small_or_big(u2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    }
    else if (BIF_ARG_1 ==  am_run_queue) {
	res = sched_q_len();
	BIF_RET(make_small(res));
    }
    else if (BIF_ARG_1 == am_wall_clock) {
	uint32 w1, w2;
	uint32 b1, b2;
	wall_clock_elapsed_time_both(&w1, &w2);
	b1 = make_small_or_big(w1,BIF_P);
	b2 = make_small_or_big(w2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_io) {
	uint32 r1, r2;
	uint32 in, out;
	in = make_small_or_big(bytes_in,BIF_P);
	out = make_small_or_big(bytes_out,BIF_P); 
	hp = HAlloc(BIF_P, 9);
	r1 = TUPLE2(hp,  am_input, in);
	hp += 3;
	r2 = TUPLE2(hp, am_output, out);
	hp += 3;
	BIF_RET(TUPLE2(hp, r1, r2));
    }
    BIF_ERROR(BIF_P, BADARG);
}
