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
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "erl_version.h"
#include "erl_db_util.h"
#include "erl_message.h"
#include "erl_binary.h"
#include "erl_db.h"
#include "erl_instrument.h"
#ifdef ELIB_ALLOC_IS_CLIB
#include "elib_stat.h"
#endif
#ifdef HIPE
#include "hipe_arch.h"
#endif

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

#ifdef USE_THREADS
extern int erts_async_max_threads;
#endif
/* Keep erts_system_version as a global variable for easy access from a core */
static char erts_system_version[] = ("Erlang (" EMULATOR ")"
				     " emulator version " ERLANG_VERSION
#ifdef ARCH_64
				     " [64-bit]"
#endif
#ifndef OTP_RELEASE
				     " [source]"
#endif	
#ifdef HIPE
				     " [hipe]"
#endif	
#ifdef SHARED_HEAP
				     " [shared heap]"
#endif
#ifdef HYBRID
				     " [hybrid heap]"
#endif
#ifdef NOMOVE
				     " [non-moving GC]"
#endif
#ifdef INCREMENTAL_GC
				     " [incremental GC]"
#endif
#ifdef ET_DEBUG
#if ET_DEBUG
				     " [type-assertions]"
#endif
#endif	
#ifdef DEBUG
				     " [debug-compiled]"
#endif	
#ifdef USE_THREADS
				     " [threads:%d]"
#endif
#ifdef USE_KERNEL_POLL
				     " [kernel-poll]"
#endif	
#ifdef HEAP_FRAG_ELIM_TEST
				     " [heap-frag]"
#endif	

				     "\n");


#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#if defined(HAVE_SOLARIS_SPARC_PERFMON)
# include <sys/ioccom.h>
# define PERFMON_SETPCR			_IOW('P', 1, unsigned long long)
# define PERFMON_GETPCR			_IOR('P', 2, unsigned long long)
#endif

static Eterm
bld_bin_list(Uint **hpp, Uint *szp, ProcBin* pb)
{
    Eterm res = NIL;
    Eterm tuple;

    for (; pb; pb = pb->next) {
	Eterm val = erts_bld_uint(hpp, szp, (Uint) pb->val);
	Eterm orig_size = erts_bld_uint(hpp, szp, pb->val->orig_size);

	if (szp)
	    *szp += 4+2;
	if (hpp) {
	    tuple = TUPLE3(*hpp, val, orig_size, make_small(pb->val->refc));
	    res = CONS(*hpp + 4, tuple, res);
	    *hpp += 4+2;
	}
    }
    return res;
}


/*
  make_monitor_list:
  returns a list of records..
  -record(erl_monitor, {
            type, % MON_ORIGIN or MON_TARGET (1 or 3)
	    ref,
	    pid, % Process or nodename
	    name % registered name or []
          }).
*/

static void do_calc_mon_size(ErtsMonitor *mon, void *vpsz)
{
    Uint *psz = vpsz;
    *psz += IS_CONST(mon->ref) ? 0 : NC_HEAP_SIZE(mon->ref);
    *psz += IS_CONST(mon->pid) ? 0 : NC_HEAP_SIZE(mon->pid);
    *psz += 8; /* CONS + 5-tuple */ 
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} MonListContext;

static void do_make_one_mon_element(ErtsMonitor *mon, void * vpmlc)
{
    MonListContext *pmlc = vpmlc;
    Eterm tup;
    Eterm r = (IS_CONST(mon->ref)
	       ? mon->ref
	       : STORE_NC(&(pmlc->hp), &MSO(pmlc->p).externals, mon->ref));
    Eterm p = (IS_CONST(mon->pid)
	       ? mon->pid
	       : STORE_NC(&(pmlc->hp), &MSO(pmlc->p).externals, mon->pid));
    tup = TUPLE5(pmlc->hp, pmlc->tag, make_small(mon->type), r, p, mon->name);
    pmlc->hp += 6;
    pmlc->res = CONS(pmlc->hp, tup, pmlc->res);
    pmlc->hp += 2;
}

static Eterm 
make_monitor_list(Process *p, ErtsMonitor *root)
{
    DECL_AM(erl_monitor);
    Uint sz = 0;
    MonListContext mlc;

    erts_doforall_monitors(root, &do_calc_mon_size, &sz);
    if (sz == 0) {
	return NIL;
    }
    mlc.p = p;
    mlc.hp = HAlloc(p,sz);
    mlc.res = NIL;
    mlc.tag = AM_erl_monitor;
    erts_doforall_monitors(root, &do_make_one_mon_element, &mlc);
    return mlc.res;
}

/*
  make_link_list:
  returns a list of records..
  -record(erl_link, {
            type, % LINK_NODE or LINK_PID (1 or 3)
	    pid, % Process or nodename
	    targets % List of erl_link's or nil
          }).
*/

static void do_calc_lnk_size(ErtsLink *lnk, void *vpsz)
{
    Uint *psz = vpsz;
    *psz += IS_CONST(lnk->pid) ? 0 : NC_HEAP_SIZE(lnk->pid);
    if (lnk->type != LINK_NODE && lnk->root != NULL) { 
	/* Node links use this pointer as ref counter... */
	erts_doforall_links(lnk->root,&do_calc_lnk_size,vpsz);
    }
    *psz += 7; /* CONS + 4-tuple */ 
}

typedef struct {
    Process *p;
    Eterm *hp;
    Eterm res;
    Eterm tag;
} LnkListContext;

static void do_make_one_lnk_element(ErtsLink *lnk, void * vpllc)
{
    LnkListContext *pllc = vpllc;
    Eterm tup;
    Eterm old_res, targets = NIL;
    Eterm p = (IS_CONST(lnk->pid)
	       ? lnk->pid
	       : STORE_NC(&(pllc->hp), &MSO(pllc->p).externals, lnk->pid));
    if (lnk->type == LINK_NODE) {
	targets = make_small(ERTS_LINK_ROOT_AS_UINT(lnk));
    } else if (lnk->root != NULL) {
	old_res = pllc->res;
	pllc->res = NIL;
	erts_doforall_links(lnk->root,&do_make_one_lnk_element, vpllc);
	targets = pllc->res;
	pllc->res = old_res;
    }
    tup = TUPLE4(pllc->hp, pllc->tag, make_small(lnk->type), p, targets);
    pllc->hp += 5;
    pllc->res = CONS(pllc->hp, tup, pllc->res);
    pllc->hp += 2;
}

static Eterm 
make_link_list(Process *p, ErtsLink *root, Eterm tail)
{
    DECL_AM(erl_link);
    Uint sz = 0;
    LnkListContext llc;

    erts_doforall_links(root, &do_calc_lnk_size, &sz);
    if (sz == 0) {
	return tail;
    }
    llc.p = p;
    llc.hp = HAlloc(p,sz);
    llc.res = tail;
    llc.tag = AM_erl_link;
    erts_doforall_links(root, &do_make_one_lnk_element, &llc);
    return llc.res;
}

char*
erts_get_system_version(int *lenp)
{
#ifdef USE_THREADS
    static char sbuf[sizeof(erts_system_version) + 20];

    sprintf(sbuf, erts_system_version, erts_async_max_threads);
    if (lenp != NULL) {
	*lenp = sys_strlen(sbuf);
    }
    return sbuf;
#else
    if (lenp != NULL) {
	*lenp = sizeof(erts_system_version) - 1;
    }
    return erts_system_version;
#endif
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
    Eterm items[ASIZE(keys)+2];
    Eterm result = NIL;
    Eterm tmp;
    Eterm* hp;
    int i;
    int next = 0;

    /*
     * The dollar dictionary is special. We will only show it if its is non-empty.
     */

    tmp = process_info_2(p, pid, am_DollarDictionary);
    if (is_non_value(tmp)) {
	return THE_NON_VALUE;
    } else if (is_tuple(tmp)) {
	Eterm* tp = tuple_val(tmp);
	if (tp[2] != NIL) {
	    items[next++] = tmp;
	}
    }

    /*
     * Registered name is also special.
     */
    
    tmp = process_info_2(p, pid, am_registered_name);
    if (is_tuple(tmp)) {
	items[next++] = tmp;
    }

    /*
     * Collect all information about the process.
     */

    for (i = 0; i < ASIZE(keys); i++) {
	Eterm item;

	item = process_info_2(p, pid, keys[i]);
	if (item == am_undefined) {
	    return am_undefined;
	}
	items[next++] = item;
    }

    /*
     * Build the resulting list.
     */

    hp = HAlloc(p, 2*next);
    for (i = next - 1; i >= 0; i--) {
	result = CONS(hp, items[i], result);
	hp += 2;
    }

    return result;
}

typedef struct {
    Eterm entity;
    Eterm node;
} MonitorInfo;

typedef struct {
    MonitorInfo *mi;
    Uint mi_i;
    Uint mi_max;
    int sz;
} MonitorInfoCollection;

#define INIT_MONITOR_INFOS(MIC) do {		\
    (MIC).mi = NULL;				\
    (MIC).mi_i = (MIC).mi_max = 0;		\
    (MIC).sz = 0;                               \
} while(0)

#define MI_INC 50
#define EXTEND_MONITOR_INFOS(MICP)					\
do {									\
    if ((MICP)->mi_i >= (MICP)->mi_max) {				\
	(MICP)->mi = ((MICP)->mi ? erts_realloc(ERTS_ALC_T_TMP,		\
						(MICP)->mi,		\
						((MICP)->mi_max+MI_INC)	\
						* sizeof(MonitorInfo))	\
		      : erts_alloc(ERTS_ALC_T_TMP,			\
				   MI_INC*sizeof(MonitorInfo)));	\
	(MICP)->mi_max += MI_INC;					\
    }									\
 } while (0)
#define DESTROY_MONITOR_INFOS(MIC)			\
do {							\
    if ((MIC).mi != NULL) {				\
	erts_free(ERTS_ALC_T_TMP, (void *) (MIC).mi);	\
    }							\
 } while (0)

static void collect_one_link(ErtsLink *lnk, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
    EXTEND_MONITOR_INFOS(micp);
    if (!(lnk->type == LINK_PID)) {
	return;
    }
    micp->mi[micp->mi_i].entity = lnk->pid;
    micp->sz += 2 + NC_HEAP_SIZE(lnk->pid);
    micp->mi_i++;
} 

static void collect_one_origin_monitor(ErtsMonitor *mon, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
 
    if (mon->type != MON_ORIGIN) {
	return;
    }
    EXTEND_MONITOR_INFOS(micp);
    if (is_atom(mon->pid)) { /* external by name */
	micp->mi[micp->mi_i].entity = mon->name;
	micp->mi[micp->mi_i].node = mon->pid;
	micp->sz += 3; /* need one 2-tuple */
    } else if (is_external_pid(mon->pid)) { /* external by pid */
	micp->mi[micp->mi_i].entity = mon->pid;
	micp->mi[micp->mi_i].node = NIL;
	micp->sz += NC_HEAP_SIZE(mon->pid);
    } else if (!is_nil(mon->name)) { /* internal by name */
	micp->mi[micp->mi_i].entity = mon->name;
	micp->mi[micp->mi_i].node = erts_this_dist_entry->sysname;
	micp->sz += 3; /* need one 2-tuple */
    } else { /* internal by pid */
	micp->mi[micp->mi_i].entity = mon->pid;
	micp->mi[micp->mi_i].node = NIL;
	/* no additional heap space needed */
    }
    micp->mi_i++;
    micp->sz += 2 + 3; /* For a cons cell and a 2-tuple */
}

static void collect_one_target_monitor(ErtsMonitor *mon, void *vmicp)
{
    MonitorInfoCollection *micp = vmicp;
 
    if (mon->type != MON_TARGET) {
	return;
    }

    EXTEND_MONITOR_INFOS(micp);
  
    micp->mi[micp->mi_i].node = NIL;
    micp->mi[micp->mi_i].entity = mon->pid;
    micp->sz += (NC_HEAP_SIZE(mon->pid) + 2 /* cons */);
    micp->mi_i++;
}


static void one_link_size(ErtsLink *lnk, void *vpu)
{
    Uint *pu = vpu;
    *pu += ERTS_LINK_SIZE*sizeof(Uint);
    if(!IS_CONST(lnk->pid))
	*pu += NC_HEAP_SIZE(lnk->pid)*sizeof(Uint);
    if (lnk->type != LINK_NODE && lnk->root != NULL) {
	erts_doforall_links(lnk->root,&one_link_size,vpu);
    }
}
static void one_mon_size(ErtsMonitor *mon, void *vpu)
{
    Uint *pu = vpu;
    *pu += ERTS_MONITOR_SIZE*sizeof(Uint);
    if(!IS_CONST(mon->pid))
	*pu += NC_HEAP_SIZE(mon->pid)*sizeof(Uint);
    if(!IS_CONST(mon->ref))
	*pu += NC_HEAP_SIZE(mon->ref)*sizeof(Uint);
}

BIF_RETTYPE process_info_2(BIF_ALIST_2) 
{
    Eterm item, term, list;
    Eterm res;
    Process *rp;
    Eterm* hp;
    int i, j;
    Eterm pid = BIF_ARG_1;

    if (is_external_pid(pid)
	&& external_pid_dist_entry(pid) == erts_this_dist_entry)
	BIF_RET(am_undefined);
	
    if (is_not_internal_pid(pid)
	|| internal_pid_index(BIF_ARG_1) >= erts_max_processes) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);

    item = BIF_ARG_2;
    
    rp = process_tab[internal_pid_index(BIF_ARG_1)];

    /* if the process is not active return undefined */
    if (INVALID_PID(rp, BIF_ARG_1)) {
	BIF_RET(am_undefined);
    }
    res = NIL;

    if (item == am_registered_name) {
	if (rp->reg != NULL) {
	    hp = HAlloc(BIF_P, 3);
	    res = rp->reg->name;
	} else {
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
	    Eterm* current;

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
	Eterm* cons;
	int n = rp->msg.len;
	Uint size;

	if (n == 0) {
	    hp = HAlloc(BIF_P, 3);
	    res = NIL;
	} else {
	    size = 0;
	    if (rp != BIF_P) {
		mp = rp->msg.first;
		while(mp != NULL) {
		    size += size_object(ERL_MESSAGE_TERM(mp));
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
		if (rp == BIF_P) {
		    cons[0] = ERL_MESSAGE_TERM(mp);
		} else {
		    cons[0] = copy_object(ERL_MESSAGE_TERM(mp), BIF_P);
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
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_doforall_links(rp->nlinks,&collect_one_link,&mic);

	hp = HAlloc(BIF_P, 3 + mic.sz);
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    item = STORE_NC(&hp, &MSO(BIF_P).externals, mic.mi[i].entity); 
	    res = CONS(hp, item, res);
	    hp += 2;
	}
	DESTROY_MONITOR_INFOS(mic);

    } else if (item == am_monitors) {
	MonitorInfoCollection mic;
	int i;

	INIT_MONITOR_INFOS(mic);
	erts_doforall_monitors(rp->monitors,&collect_one_origin_monitor,&mic);
	hp = HAlloc(BIF_P, 3 + mic.sz);
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    if (is_atom(mic.mi[i].entity)) {
		/* Monitor by name. 
		 * Build {process, {Name, Node}} and cons it. 
		 */
		Eterm t1, t2;

		t1 = TUPLE2(hp, mic.mi[i].entity, mic.mi[i].node);
		hp += 3;
		t2 = TUPLE2(hp, am_process, t1);
		hp += 3;
		res = CONS(hp, t2, res);
		hp += 2;
	    }
	    else {
		/* Monitor by pid. Build {process, Pid} and cons it. */
		Eterm t;
		Eterm pid = STORE_NC(&hp,
				     &MSO(BIF_P).externals,
				     mic.mi[i].entity);
		t = TUPLE2(hp, am_process, pid);
		hp += 3;
		res = CONS(hp, t, res);
		hp += 2;
	    }
	}
	DESTROY_MONITOR_INFOS(mic);
    } else if (item == am_monitored_by) {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);
	erts_doforall_monitors(rp->monitors,&collect_one_target_monitor,&mic);
	hp = HAlloc(BIF_P, 3 + mic.sz);

	res = NIL;
	for (i = 0; i < mic.mi_i; ++i) {
	    item = STORE_NC(&hp, &MSO(BIF_P).externals, mic.mi[i].entity); 
	    res = CONS(hp, item, res);
	    hp += 2;
	}
	DESTROY_MONITOR_INFOS(mic);
    } else if (item == am_dictionary) {
	res = erts_dictionary_copy(BIF_P, rp->dictionary);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_DollarDictionary) {
	res = erts_dictionary_copy(BIF_P, rp->debug_dictionary);
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
	res = make_small(HEAP_SIZE(rp));
    } else if (item == am_stack_size) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(STACK_START(rp) - rp->stop);
    } else if (item == am_memory) { /* Memory consumed in bytes */
	Uint size = 0;
	Uint hsz = 3;
	size += sizeof(Process);

	erts_doforall_links(rp->nlinks, &one_link_size, &size);
	erts_doforall_monitors(rp->monitors, &one_mon_size, &size);
#ifdef SHARED_HEAP
	size += (rp->stack - rp->send) * sizeof(Eterm);
#else
	size += (rp->heap_sz + rp->mbuf_sz) * sizeof(Eterm);
	if (rp->old_hend && rp->old_heap)
	    size += (rp->old_hend - rp->old_heap) * sizeof(Eterm);
#endif

	size += rp->msg.len * sizeof(ErlMessage);

	if (rp->arg_reg != rp->def_arg_reg) {
	    size += rp->arity * sizeof(rp->arg_reg[0]);
	}
	
	if (rp->ct) {
	    size += (sizeof(*(rp->ct))
		     + (rp->ct->len-1) * sizeof(rp->ct->ct[0]));
	}

	size += erts_dicts_mem_size(rp);

	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
    } else if (item == am_garbage_collection){
	hp = HAlloc(BIF_P, 3+2+3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(MAX_GEN_GCS(rp)));
	hp += 3;
	res = CONS(hp, res, NIL);
	hp += 2;
    } else if (item == am_group_leader) {
	int sz = NC_HEAP_SIZE(rp->group_leader);
	hp = HAlloc(BIF_P, 3 + sz);
	/*
	 * XXX Multi-thread note: Reading from another process's heap.
	 */
	res = STORE_NC(&hp, &MSO(BIF_P).externals, rp->group_leader);
    } else if (item == am_reductions) {
	Uint reds;

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
	Uint sz = 3;
	(void) bld_bin_list(NULL, &sz, MSO(rp).mso);
	hp = HAlloc(BIF_P, sz);
	res = bld_bin_list(&hp, NULL, MSO(rp).mso);
    } else if (item == am_sequential_trace_token) {

	/*
	 * XXX Multi-thread note: Reading from another process's heap.
	 */
	res = copy_object(rp->seq_trace_token, BIF_P);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_catchlevel) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(catchlevel(BIF_P));
    } else if (item == am_backtrace) {
	cerr_pos = 0;
	erts_stack_dump(rp, CBUF);
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
#undef MI_INC

/*
 * This function takes care of calls to erlang:system_info/1 when the argument
 * is a tuple.
 */
static BIF_RETTYPE
info_1_tuple(Process* BIF_P,	/* Pointer to current process. */
	     Eterm* tp,		/* Pointer to first element in tuple */
	     int arity)		/* Arity of tuple (untagged). */
{
    Eterm sel;

    sel = *tp++;



    if (sel == am_memory) {
	if (arity != 2)
	    return am_badarg;
	return erts_memory(NULL, BIF_P, *tp);
    } else if (sel == am_allocated) {
	if (arity == 2) {
	    int len;

	    if (!is_string(*tp))
		return THE_NON_VALUE;
       
	    if ((len = intlist_to_buf(*tp, tmp_buf, TMP_BUF_SIZE-1)) < 0)
		return THE_NON_VALUE;
	    tmp_buf[len] = '\0';

	    if (erts_instr_dump_memory_map(tmp_buf))
		return am_true;
	    else
		return am_false;
	}
	else if (arity == 3 && tp[0] == am_status) {
	    if (is_atom(tp[1]))
		return erts_instr_get_stat(BIF_P, tp[1], 1);
	    else {
		int len;
		if (!is_string(tp[1]))
		    return THE_NON_VALUE;
       
		if ((len = intlist_to_buf(tp[1], tmp_buf, TMP_BUF_SIZE-1)) < 0)
		    return THE_NON_VALUE;
		tmp_buf[len] = '\0';

		if (erts_instr_dump_stat(tmp_buf, 1))
		    return am_true;
		else
		    return am_false;
	    }
	}
	else
	    return THE_NON_VALUE;
#ifdef PURIFY
    } else if (sel == am_purify) {
	if (*tp == am_memory) {
	    BIF_RET(make_small_or_big(purify_new_leaks(), BIF_P));
	} else if (*tp == am_fd) {
	    BIF_RET(make_small_or_big(purify_new_fds_inuse(), BIF_P));
	} else if (*tp == am_running) {
	    BIF_RET(purify_is_running() ? am_true : am_false);
	} else if (is_list(*tp)) {
	    int r;

	    r = io_list_to_buf(*tp, (char*) tmp_buf, TMP_BUF_SIZE - 1);
	    if (r >= 0) {
		tmp_buf[TMP_BUF_SIZE - 1 - r] = '\0';
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
    } else if (sel == am_link_list) {
	ErtsLink *links = NULL;
	if(is_internal_pid(*tp)) {
	    Process *p = pid2proc(*tp);
	    if(p)
		links = p->nlinks;
	    else
		return am_undefined;
	}
	else if(is_internal_port(*tp)) {
	    Port *p = id2port(*tp);
	    if(p)
		links = p->nlinks;
	    else
		return am_undefined;
	}
	else if(is_node_name_atom(*tp)) {
	    DistEntry *dep = erts_find_dist_entry(*tp);
	    if(dep) {
		Eterm subres;
		subres = make_link_list(BIF_P,dep->nlinks,NIL);
		return make_link_list(BIF_P, dep->node_links, subres);
	    } else {
		return am_undefined;
	    }
	}
	else
	    return THE_NON_VALUE;

	return make_link_list(BIF_P, links, NIL);
    } else if (sel == am_monitor_list) {
	if(is_internal_pid(*tp)) {
	    Process *p = pid2proc(*tp);
	    if(p) {
		return make_monitor_list(BIF_P, p->monitors);
	    } else {
		return am_undefined;
	    }
	} else if(is_node_name_atom(*tp)) {
	    DistEntry *dep = erts_find_dist_entry(*tp);
	    if(dep) {
		return make_monitor_list(BIF_P, dep->monitors);
	    } else {
		return am_undefined;
	    }
	}
	return THE_NON_VALUE;
    } else if (sel == am_allocator && arity == 2) {
	return erts_allocator_info_term(BIF_P, *tp);
    }

    return THE_NON_VALUE;
}

BIF_RETTYPE system_info_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    Eterm val;
    unsigned count;
    int i;
    DECL_AM(ets_realloc_moves);
    DECL_AM(dist_ctrl);

    cerr_pos = 0;

    if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	Uint arity = *tp++;
	res = info_1_tuple(BIF_P, tp, arityval(arity));
	if (is_non_value(res))
	    goto error;
	return res;
    } else if (BIF_ARG_1 == am_compat_rel) {
	ASSERT(erts_compat_rel > 0);
	BIF_RET(make_small(erts_compat_rel));
    } else if (BIF_ARG_1 == am_memory) {
	BIF_RET(erts_memory(NULL, BIF_P, THE_NON_VALUE));
    } else if (BIF_ARG_1 == am_allocated_areas) {
	BIF_RET(erts_allocated_areas(NULL, BIF_P));
    } else if (BIF_ARG_1 == am_allocated) {
	BIF_RET(erts_instr_get_memory_map(BIF_P));
    } else if (BIF_ARG_1 == am_hipe_architecture) {
#if defined(HIPE)
	BIF_RET(hipe_arch_name);
#else
	BIF_RET(am_undefined);
#endif
    } else if (BIF_ARG_1 == am_trace_control_word) {
	BIF_RET(db_get_trace_control_word_0(BIF_P));
    } else if (BIF_ARG_1 == AM_ets_realloc_moves) {
 	BIF_RET((erts_ets_realloc_always_moves) ? am_true : am_false);
    } else if (BIF_ARG_1 == am_sequential_tracer) {
	if (is_pid(system_seq_tracer) || is_port(system_seq_tracer)) {
	    val = STORE_NC_IN_PROC(BIF_P, system_seq_tracer);
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
	for (i = 0; i < erts_max_processes; i++) {
	    if (process_tab[i] != NULL && process_tab[i]->status != P_EXITING) {
		count++;
	    }
	}
	BIF_RET(make_small(count));
    } else if (BIF_ARG_1 == am_process_limit) {
	BIF_RET(make_small(erts_max_processes));
    } else if (BIF_ARG_1 == am_info) {
	info(CBUF);
    } else if (BIF_ARG_1 == am_procs)
	process_info(CBUF);
    else if (BIF_ARG_1 == am_loaded)
	loaded(CBUF);
    else if (BIF_ARG_1 == am_dist)
	distribution_info(CBUF);
    else if (BIF_ARG_1 == AM_dist_ctrl) {
	DistEntry *dep;
	i = 0;
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) 
	    ++i;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next)
	    ++i;
	hp = HAlloc(BIF_P,i*(3+2));
	res = NIL;
	for (dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	for (dep = erts_visible_dist_entries; dep; dep = dep->next) {
	    Eterm tpl;
	    ASSERT(is_immed(dep->cid));
	    tpl = TUPLE2(hp, dep->sysname, dep->cid);
	    hp +=3;
	    res = CONS(hp, tpl, res);
	    hp += 2;
	}
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_version) {
	char *sys_ver;
	int sys_ver_len;

	sys_ver = erts_get_system_version(&sys_ver_len);
	hp = HAlloc(BIF_P, sys_ver_len*2);
	BIF_RET(buf_to_intlist(&hp, sys_ver, sys_ver_len, NIL));
    } else if (BIF_ARG_1 == am_system_architecture) {
	int n;

	sys_strcpy((char*)tmp_buf, ERLANG_ARCHITECTURE);
	n = sys_strlen((char*)tmp_buf);
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
    } 
    else if (BIF_ARG_1 == am_memory_types) {
	return erts_instr_get_type_info(BIF_P);
    }
    else if (BIF_ARG_1 == am_os_type) {
       Eterm type = am_atom_put(os_type, strlen(os_type));
       Eterm flav, tup;

       os_flavor(tmp_buf, TMP_BUF_SIZE);
       flav = am_atom_put(tmp_buf, strlen(tmp_buf));
       hp = HAlloc(BIF_P, 3);
       tup = TUPLE2(hp, type, flav);
       BIF_RET(tup);
    }
    else if (BIF_ARG_1 == am_allocator) {
	BIF_RET(erts_allocator_options((void *) BIF_P));
    }
    else if (BIF_ARG_1 == am_thread_pool_size) {
#ifdef USE_THREADS
	extern int erts_async_max_threads;
#endif
	int n;
	
#ifdef USE_THREADS
	n = erts_async_max_threads;
#else
	n = 0;
#endif
	BIF_RET(make_small(n));
    }
    else if (BIF_ARG_1 == am_elib_malloc) {
#ifdef ELIB_ALLOC_IS_CLIB
	struct elib_stat stat;
	DECL_AM(heap_size);
	DECL_AM(max_alloced_size);
	DECL_AM(alloced_size);
	DECL_AM(free_size);
	DECL_AM(no_alloced_blocks);
	DECL_AM(no_free_blocks);
	DECL_AM(smallest_alloced_block);
	DECL_AM(largest_free_block);
	Eterm atoms[8];
	Eterm ints[8];
	Uint **hpp;
	Uint sz;
	Uint *szp;
	int length;
#ifdef DEBUG
	Uint *endp;
#endif

	elib_stat(&stat);

	/* First find out the heap size needed ... */
	hpp = NULL;
	szp = &sz;
	sz = 0;

    build_elib_malloc_term:
	length = 0;
	atoms[length] = AM_heap_size;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.mem_total*sizeof(Uint));
	atoms[length] = AM_max_alloced_size;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.mem_max_alloc*sizeof(Uint));
	atoms[length] = AM_alloced_size;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.mem_alloc*sizeof(Uint));
	atoms[length] = AM_free_size;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.mem_free*sizeof(Uint));
	atoms[length] = AM_no_alloced_blocks;
	ints[length++] = erts_bld_uint(hpp, szp, (Uint) stat.mem_blocks);
	atoms[length] = AM_no_free_blocks;
	ints[length++] = erts_bld_uint(hpp, szp, (Uint) stat.free_blocks);
	atoms[length] = AM_smallest_alloced_block;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.min_used*sizeof(Uint));
	atoms[length] = AM_largest_free_block;
	ints[length++] = erts_bld_uint(hpp, szp,
				       (Uint) stat.max_free*sizeof(Uint));



	ASSERT(length <= sizeof(atoms)/sizeof(Eterm));
	ASSERT(length <= sizeof(ints)/sizeof(Eterm));

	res = erts_bld_2tup_list(hpp, szp, length, atoms, ints);

	if (szp) {
	    /* ... and then build the term */
	    hp = HAlloc(BIF_P, sz);
#ifdef DEBUG
	    endp = hp + sz;
#endif

	    szp = NULL;
	    hpp = &hp;
	    goto build_elib_malloc_term;
	}

#ifdef DEBUG
	ASSERT(endp == hp);
#endif

#else /* #ifdef ELIB_ALLOC_IS_CLIB */
	res = am_false;
#endif /* #ifdef ELIB_ALLOC_IS_CLIB */

	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_os_version) {
       int major, minor, build;
       Eterm tup;

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
    } else if (BIF_ARG_1 == am_wordsize) {
	return make_small(sizeof(Eterm));
    } else if (BIF_ARG_1 == am_endian) {
#if defined(WORDS_BIGENDIAN)
	return am_big;
#else
	return am_little;
#endif
    } else if (BIF_ARG_1 == am_heap_sizes) {
	return erts_heap_sizes(BIF_P);
    } else if (BIF_ARG_1 == am_global_heaps_size) {
#ifdef SHARED_HEAP
	Uint hsz = 0;
	Uint sz = 0;

	sz += global_heap_sz;
	sz += global_mbuf_sz;
#ifdef NOMOVE
        /* The size of the old generation is a bit hard to define here...
         * The ammount of live data in the last collection perhaps..? */
#else
	if (global_old_hend && global_old_heap)
	    sz += global_old_hend - global_old_heap;
#endif

	sz *= sizeof(Eterm);

	(void) erts_bld_uint(NULL, &hsz, sz);
	hp = hsz ? HAlloc(BIF_P, hsz) : NULL;
	res = erts_bld_uint(&hp, NULL, sz);
#else
	res = make_small(0);
#endif
	return res;
    } else if (BIF_ARG_1 == am_heap_type) {
#if defined(SHARED_HEAP)
	return am_shared;
#elif defined(HYBRID)
        return am_hybrid;
#else
	return am_private;
#endif
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
    } else if (BIF_ARG_1 == am_threads) {
#ifdef USE_THREADS
	return am_true;
#else
	return am_false;
#endif
    } else if (BIF_ARG_1 == am_creation) {
	return make_small(erts_this_node->creation);
    } else if (BIF_ARG_1 == am_break_ignored) {
      extern int ignore_break;
      if (ignore_break) 
	return am_true; 
      else
	return am_false;
    } else {
	/* Arguments that are unusual... */
	DECL_AM(node_and_dist_references);
	DECL_AM(stop_memory_trace);

	if (BIF_ARG_1 == AM_node_and_dist_references) {
	    /*
	     * OBSERVE! Only supposed to be used for testing, and debugging.
	     */
	    BIF_RET(erts_get_node_and_dist_references(BIF_P));
	} else if (BIF_ARG_1 == AM_stop_memory_trace) {
	    erts_mtrace_stop();
	    BIF_RET(am_true);
	}
	    
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
    Eterm tmp;
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

    hp = HAlloc(p, 2*ASIZE(keys)+2);
    for (i = ASIZE(keys) - 1; i >= 0; i--) {
	result = CONS(hp, items[i], result);
	hp += 2;
    }
    
    /*
     * Registered name is special.
     */
    
    tmp = port_info_2(p, pid, am_registered_name);
    if (is_tuple(tmp)) {
	result = CONS(hp, tmp, result);
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
{
    Eterm portid = BIF_ARG_1;
    Eterm item = BIF_ARG_2;
    Eterm res;
    Eterm* hp;
    int count;
    int portix;
    Process *p;
    Port *pt;

    if (is_atom(portid)) {
	whereis_name(portid, &p, &pt);
	if (pt != NULL)
	    portid = pt->id;
	else
	    BIF_ERROR(BIF_P, BADARG);
    }

    if (is_external_port(portid)
	&& external_port_dist_entry(portid) == erts_this_dist_entry)
	BIF_RET(am_undefined);

    if (is_not_internal_port(portid)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (((portix = internal_port_index(portid)) >= erts_max_ports)
	|| INVALID_PORT(erts_port+portix, portid)) {
	BIF_RET(am_undefined);
    }

    if (item == am_id) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(internal_port_number(portid));
    }
    else if (item == am_links) {
	MonitorInfoCollection mic;
	int i;
	Eterm item;

	INIT_MONITOR_INFOS(mic);

	erts_doforall_links( erts_port[portix].nlinks,&collect_one_link,&mic);

	hp = HAlloc(BIF_P, 3 + mic.sz);
	res = NIL;
	for (i = 0; i < mic.mi_i; i++) {
	    item = STORE_NC(&hp, &MSO(BIF_P).externals, mic.mi[i].entity); 
	    res = CONS(hp, item, res);
	    hp += 2;
	}
	DESTROY_MONITOR_INFOS(mic);

    }
    else if (item == am_name) {
	count = sys_strlen(erts_port[portix].name);

	hp = HAlloc(BIF_P, 3 + 2*count);
	res = buf_to_intlist(&hp,(byte*)erts_port[portix].name,count,NIL);
    }
    else if (item == am_connected) {
	hp = HAlloc(BIF_P, 3);
	res = erts_port[portix].connected; /* internal pid */
    }
    else if (item == am_input) {
	hp = HAlloc(BIF_P, 3);
	res = make_small_or_big(erts_port[portix].bytes_in, BIF_P);
    }
    else if (item == am_output) {
	hp = HAlloc(BIF_P, 3);
	res = make_small_or_big(erts_port[portix].bytes_out, BIF_P);
    }
    else if (item == am_registered_name) {
	RegProc *reg;
	hp = HAlloc(BIF_P, 3);
	reg = erts_port[portix].reg;
	if (reg == NULL)
	    BIF_RET(NIL);
	else
	    res = reg->name;
    }
    else if (item == am_memory) {
	/* All memory consumed in bytes (the Port struct should not be
	   included though).
	 */
	Uint hsz = 3;
	Uint size = 0;
	ErlHeapFragment* bp;

	hp = HAlloc(BIF_P, 3);

	erts_doforall_links(erts_port[portix].nlinks, &one_link_size, &size);

	for (bp = erts_port[portix].bp; bp; bp = bp->next)
	    size += sizeof(ErlHeapFragment) + (bp->size - 1)*sizeof(Eterm);

	if (erts_port[portix].linebuf)
	    size += sizeof(LineBuf) + erts_port[portix].linebuf->ovsiz;

	/* ... */


	/* All memory allocated by the driver should be included, but it is
	   hard to retrieve... */
	
	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
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
	    val = funp->fe->module;
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->index);
	    break;
	case am_new_uniq:
	    val = new_binary(p, funp->fe->uniq, 16);
	    hp = HAlloc(p, 3);
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_index);
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->fe->old_uniq);
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
	case am_refc:
	    hp = HAlloc(p, 3);
	    val = make_small_or_big(funp->fe->refc, p);
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(funp->arity);
	    break;
	case am_name:
	    hp = HAlloc(p, 3);
	    val = funp->fe->address[-2];
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
{
    Eterm res;
    Eterm* hp;

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
	Uint reds;
	Eterm b1, b2;

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
	Eterm b1, b2;
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
	Uint w1, w2;
	Eterm b1, b2;
	wall_clock_elapsed_time_both(&w1, &w2);
	b1 = make_small_or_big(w1,BIF_P);
	b2 = make_small_or_big(w2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    }
    else if (BIF_ARG_1 == am_io) {
	Eterm r1, r2;
	Eterm in, out;
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

BIF_RETTYPE error_logger_warning_map_0(BIF_ALIST_0)
{
    BIF_RET(erts_error_logger_warnings);
}

void
erts_bif_info_init(void)
{

}
