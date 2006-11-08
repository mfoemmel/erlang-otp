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
#include "erl_nmgc.h"
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
#include "dist.h"
#ifdef ELIB_ALLOC_IS_CLIB
#include "elib_stat.h"
#endif
#ifdef HIPE
#include "hipe_arch.h"
#endif

#ifdef VALGRIND
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>
#endif

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)
#define INIT_AM(S) AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

#ifdef USE_THREADS
extern int erts_async_max_threads;
#endif
/* Keep erts_system_version as a global variable for easy access from a core */
static char erts_system_version[] = ("Erlang (" EMULATOR ")"
				     " emulator version " ERLANG_VERSION
#ifndef OTP_RELEASE
				     " [source]"
#endif	
#ifdef ARCH_64
				     " [64-bit]"
#endif
#ifdef ERTS_SMP
				     " [smp:%bpu]"
#endif
#ifdef USE_THREADS
				     " [async-threads:%d]"
#endif
#ifdef HIPE
				     " [hipe]"
#endif	
#ifdef HYBRID
				     " [hybrid heap]"
#endif
#ifdef INCREMENTAL
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
#ifdef ERTS_ENABLE_LOCK_CHECK
				     " [lock-checking]"
#endif
#ifdef ERTS_ENABLE_KERNEL_POLL
				     " [kernel-poll:%s]"
#endif	
#ifdef HEAP_FRAG_ELIM_TEST
				     " [no-frag]"
#endif
#ifdef PURIFY
				     " [purify-compiled]"
#endif	
#ifdef VALGRIND
				     " [valgrind-compiled]"
#endif
				     "\n");

#if defined(PURIFY) || defined(VALGRIND)
static Eterm AM_error_checker;
#ifdef VALGRIND
static Eterm AM_valgrind;
#endif
#endif

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
	    Uint refc = (Uint) erts_smp_atomic_read(&pb->val->refc);
	    tuple = TUPLE3(*hpp, val, orig_size, make_small(refc));
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

int
erts_print_system_version(int to, void *arg, Process *c_p)
{
    return erts_print(to, arg, erts_system_version
#ifdef ERTS_SMP
		      , erts_get_no_schedulers()
#endif
#ifdef USE_THREADS
		      , erts_async_max_threads
#endif
#ifdef ERTS_ENABLE_KERNEL_POLL
		      , erts_use_kernel_poll ? "true" : "false"
#endif
	);
}

BIF_RETTYPE
process_info_aux(Process *BIF_P, Process *rp, Eterm rpid, Eterm item);

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
    Process *rp;
    Eterm items[ASIZE(keys)+2];
    Eterm result = NIL;
    Eterm tmp;
    Eterm* hp;
    int i;
    int next = 0;

    if (is_external_pid(pid)
	&& external_pid_dist_entry(pid) == erts_this_dist_entry)
	return am_undefined;
	
    if (is_not_internal_pid(pid)
	|| internal_pid_index(pid) >= erts_max_processes) {
	BIF_ERROR(p, BADARG);
    }

    rp = erts_pid2proc(p, ERTS_PROC_LOCK_MAIN,
		       pid, ERTS_PROC_LOCKS_ALL);
    if (!rp) {
	return am_undefined;
    }

    /*
     * The dollar dictionary is special. We will only show it if its is non-empty.
     */

    tmp = process_info_aux(p, rp, rp->id, am_DollarDictionary);
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
    
    tmp = process_info_aux(p, rp, rp->id, am_registered_name);
    if (is_tuple(tmp)) {
	items[next++] = tmp;
    }

    /*
     * Collect all information about the process.
     */

    for (i = 0; i < ASIZE(keys); i++) {
	Eterm item;

	item = process_info_aux(p, rp, rp->id, keys[i]);
	if (item == am_undefined) {
	    result = am_undefined;
	    goto done;
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


 done:
#ifdef ERTS_SMP
    erts_smp_proc_unlock(rp, 
			 (rp != p
			  ? ERTS_PROC_LOCKS_ALL
			  : (ERTS_PROC_LOCKS_ALL & ~ERTS_PROC_LOCK_MAIN)));
#endif
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
    int flags;
    Eterm res;
    Process *rp;
    Eterm pid = BIF_ARG_1;
    Uint32 info_locks;

    if (is_external_pid(pid)
	&& external_pid_dist_entry(pid) == erts_this_dist_entry)
	BIF_RET(am_undefined);
	
    if (is_not_internal_pid(pid)
	|| internal_pid_index(BIF_ARG_1) >= erts_max_processes) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_atom(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);

    flags = 0;
    info_locks = ERTS_PROC_LOCK_MAIN; 

    switch (BIF_ARG_2) {
    case am_internal_status:
	flags = ERTS_P2P_FLG_ALLOW_OTHER_X;
#ifdef ERTS_SMP
    case am_status:
	info_locks = ERTS_PROC_LOCK_STATUS;
	break;
    case am_messages:
    case am_message_queue_len:
	info_locks |= ERTS_PROC_LOCK_MSGQ;
	break;
    case am_links:
    case am_monitors:
    case am_monitored_by:
	info_locks = ERTS_PROC_LOCK_LINK;
	break;
    case am_memory:
	info_locks |= ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCK_MSGQ;
    default:
#endif
	break;
    }

    rp = erts_pid2proc_opt(BIF_P, ERTS_PROC_LOCK_MAIN, pid, info_locks, flags);
    if (rp || BIF_ARG_2 == am_internal_status)
	res = process_info_aux(BIF_P, rp, pid, BIF_ARG_2);
    else {
	res = am_undefined; /* if the process is not active return undefined */
    }

#ifdef ERTS_SMP
    if (BIF_P == rp)
	info_locks &= ~ERTS_PROC_LOCK_MAIN;
    if (rp && info_locks)
	erts_smp_proc_unlock(rp, info_locks);
#endif

    BIF_RET(res);
}

BIF_RETTYPE
process_info_aux(Process *BIF_P, Process *rp, Eterm rpid, Eterm item)
{
    Eterm *hp;
    Eterm res = NIL;

    /* internal_status is a little special... */
    ASSERT(rp || item == am_internal_status);

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
    } else if (item == am_status || item == am_internal_status) {
	res = erts_process_status(BIF_P, ERTS_PROC_LOCK_MAIN, rp, rpid);
	if (res == am_undefined)
	    BIF_RET(res);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_messages) {
	ErlMessage* mp;
	int n;

	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);
	n = rp->msg.len;

	if (n == 0) {
	    hp = HAlloc(BIF_P, 3);
	} else {
	    Eterm* ma = (Eterm *) erts_alloc(ERTS_ALC_T_TMP, n*sizeof(Eterm));
	    int i;

	    i = 0;
	    for (mp = rp->msg.first; mp != NULL; mp = mp->next) {
#ifdef HYBRID
		/*
		 * Hybrid: Almost all messages already are in the message area.
		 */
		if (NO_COPY(ERL_MESSAGE_TERM(mp)) || rp == BIF_P) {
		    /* Constant, already in message area, or same process. */
		    ma[i] = ERL_MESSAGE_TERM(mp);
		} else {
		    ma[i] = copy_object(ERL_MESSAGE_TERM(mp), BIF_P);
		}
#else
		/*
		 * We must copy the message if it belongs to another process.
		 */
		if (rp == BIF_P) {
#if defined(ERTS_SMP) && !defined(HEAP_FRAG_ELIM_TEST)
		    if (mp->bp) {
			erts_move_msg_mbuf_to_proc_mbufs(BIF_P, mp);
		    }
#endif
		    ma[i] = ERL_MESSAGE_TERM(mp);
		} else {
		    ma[i] = copy_object(ERL_MESSAGE_TERM(mp), BIF_P);
		}
#endif
		i++;
	    }
	    hp = HAlloc(BIF_P, 3+2*n);
	    for (i = n-1; i >= 0; i--) {
		res = CONS(hp, ma[i], res);
		hp += 2;
	    }
	    erts_free(ERTS_ALC_T_TMP, (void *) ma);
	}
    } else if (item == am_message_queue_len) {
	hp = HAlloc(BIF_P, 3);
	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);
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
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, HEAP_SIZE(rp));
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, HEAP_SIZE(rp));
    } else if (item == am_stack_size) {
	Uint stack_size = STACK_START(rp) - rp->stop;
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, stack_size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, stack_size);
    } else if (item == am_memory) { /* Memory consumed in bytes */
	Uint size = 0;
	Uint hsz = 3;
	size += sizeof(Process);

	ERTS_SMP_MSGQ_MV_INQ2PRIVQ(rp);

	erts_doforall_links(rp->nlinks, &one_link_size, &size);
	erts_doforall_monitors(rp->monitors, &one_mon_size, &size);
	size += (rp->heap_sz + rp->mbuf_sz) * sizeof(Eterm);
	if (rp->old_hend && rp->old_heap)
	    size += (rp->old_hend - rp->old_heap) * sizeof(Eterm);

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
	Uint reds = rp->reds + erts_current_reductions(BIF_P, rp);
	Uint hsz = 3;
	(void) erts_bld_uint(NULL, &hsz, reds);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, reds);
    } else if (item == am_priority) {
	hp = HAlloc(BIF_P, 3);
	res = erts_get_process_priority(rp);
    } else if (item == am_trace) {
	hp = HAlloc(BIF_P, 3);
	res = make_small(rp->trace_flags & TRACEE_FLAGS);
    } else if (item == am_binary) {
	Uint sz = 3;
	(void) bld_bin_list(NULL, &sz, MSO(rp).mso);
	hp = HAlloc(BIF_P, sz);
	res = bld_bin_list(&hp, NULL, MSO(rp).mso);
#ifdef HYBRID
    } else if (item == am_message_binary) {
	Uint sz = 3;
	(void) bld_bin_list(NULL, &sz, erts_global_offheap.mso);
	hp = HAlloc(BIF_P, sz);
	res = bld_bin_list(&hp, NULL, erts_global_offheap.mso);
#endif
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
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_stack_dump(ERTS_PRINT_DSBUF, (void *) dsbufp, rp);
	res = new_binary(BIF_P, (byte *) dsbufp->str, (int) dsbufp->str_len);
	erts_destroy_tmp_dsbuf(dsbufp);
	hp = HAlloc(BIF_P, 3);
    } else if (item == am_last_calls) {
	if (rp->ct == NULL) {
	    hp = HAlloc(BIF_P, 3);
	    res = am_false;
	} else {
	    /*
	     * One cons cell and a 3-struct, and a 2-tuple.
	     * Might be less than that, if there are sends, receives or timeouts,
	     * so we must do a HRelease() to avoid creating holes.
	     */
	    Uint needed = rp->ct->n*(2+4) + 3;
	    Eterm* limit;
	    Eterm term, list;
	    int i, j;

	    hp = HAlloc(BIF_P, needed);
	    limit = hp + needed;
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
	    res = TUPLE2(hp, item, res);
	    hp += 3;
	    HRelease(BIF_P,limit,hp);
	    BIF_RET(res);
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
	Eterm res;
	if (arity != 2)
	    return am_badarg;
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	res = erts_memory(NULL, NULL, BIF_P, *tp);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	return res;
    } else if (sel == am_allocated) {
	if (arity == 2) {
	    Eterm res = THE_NON_VALUE;
	    char *buf;
	    int len = is_string(*tp);
	    if (len <= 0)
		return res;
	    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, len+1);
	    if (intlist_to_buf(*tp, buf, len) != len)
		erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
	    buf[len] = '\0';
	    res = erts_instr_dump_memory_map(buf) ? am_true : am_false;
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    return res;
	}
	else if (arity == 3 && tp[0] == am_status) {
	    if (is_atom(tp[1]))
		return erts_instr_get_stat(BIF_P, tp[1], 1);
	    else {
		Eterm res = THE_NON_VALUE;
		char *buf;
		int len = is_string(tp[1]);
		if (len <= 0)
		    return res;
		buf = (char *) erts_alloc(ERTS_ALC_T_TMP, len+1);
		if (intlist_to_buf(tp[1], buf, len) != len)
		    erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
		buf[len] = '\0';
		res = erts_instr_dump_stat(buf, 1) ? am_true : am_false;
		erts_free(ERTS_ALC_T_TMP, (void *) buf);
		return res;
	    }
	}
	else
	    return THE_NON_VALUE;
#if defined(PURIFY) || defined(VALGRIND)
    } else if (sel == AM_error_checker
#if defined(PURIFY)
	       || sel == am_purify
#elif defined(VALGRIND)
	       || sel == AM_valgrind
#endif
	) {
	if (*tp == am_memory) {
#if defined(PURIFY)
	    BIF_RET(erts_make_integer(purify_new_leaks(), BIF_P));
#elif defined(VALGRIND)
	    VALGRIND_DO_LEAK_CHECK;
	    BIF_RET(make_small(0));
#endif
	} else if (*tp == am_fd) {
#if defined(PURIFY)
	    BIF_RET(erts_make_integer(purify_new_fds_inuse(), BIF_P));
#elif defined(VALGRIND)
	    /* Not present in valgrind... */
	    BIF_RET(make_small(0));
#endif
	} else if (*tp == am_running) {
#if defined(PURIFY)
	    BIF_RET(purify_is_running() ? am_true : am_false);
#elif defined(VALGRIND)
	    BIF_RET(RUNNING_ON_VALGRIND ? am_true : am_false);
#endif
	} else if (is_list(*tp)) {
#if defined(PURIFY)
#define ERTS_ERROR_CHECKER_PRINTF purify_printf
#elif defined(VALGRIND)
#define ERTS_ERROR_CHECKER_PRINTF VALGRIND_PRINTF
#endif
	    int buf_size = 8*1024; /* Try with 8KB first */
	    char *buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
	    int r = io_list_to_buf(*tp, (char*) buf, buf_size - 1);
	    if (r < 0) {
		erts_free(ERTS_ALC_T_TMP, (void *) buf);
		buf_size = io_list_len(*tp);
		if (buf_size < 0)
		    return THE_NON_VALUE;
		buf_size++;
		buf = erts_alloc(ERTS_ALC_T_TMP, buf_size);
		r = io_list_to_buf(*tp, (char*) buf, buf_size - 1);
		ASSERT(r == buf_size - 1);
	    }
	    buf[buf_size - 1 - r] = '\0';
	    ERTS_ERROR_CHECKER_PRINTF("%s\n", buf);
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	    BIF_RET(am_true);
#undef ERTS_ERROR_CHECKER_PRINTF
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
    } else if (sel == am_allocator && arity == 2) {
	return erts_allocator_info_term(BIF_P, *tp);
    }
    return THE_NON_VALUE;
}

#define INFO_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_info_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp);

    if (need <= free_size)
	return dsbufp;
    size = need - free_size + INFO_DSBUF_INC_SZ;
    size = ((size + INFO_DSBUF_INC_SZ - 1)/INFO_DSBUF_INC_SZ)*INFO_DSBUF_INC_SZ;
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_INFO_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

static erts_dsprintf_buf_t *
erts_create_info_dsbuf(Uint size)
{
    Uint init_size = size ? size : INFO_DSBUF_INC_SZ;
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_info_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_INFO_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_INFO_DSBUF, init_size);
    dsbufp->str[0] = '\0';
    dsbufp->size = init_size;
    return dsbufp;
}

static void
erts_destroy_info_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    if (dsbufp->str)
	erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_INFO_DSBUF, (void *) dsbufp);
}


BIF_RETTYPE system_info_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;
    Eterm val;
    int i;
    DECL_AM(ets_realloc_moves);
    DECL_AM(dist_ctrl);

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
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	res = erts_memory(NULL, NULL, BIF_P, THE_NON_VALUE);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_allocated_areas) {
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	res = erts_allocated_areas(NULL, NULL, BIF_P);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	BIF_RET(res);
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
	val = erts_get_system_seq_tracer();
	ASSERT(is_internal_pid(val) || is_internal_port(val) || val==am_false)
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_sequential_tracer, val);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection){
	Uint val = (Uint) erts_smp_atomic_read(&erts_max_gen_gcs);
	hp = HAlloc(BIF_P, 3+2);
	res = TUPLE2(hp, am_fullsweep_after, make_small(val));
	hp += 3;
	res = CONS(hp, res, NIL);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_fullsweep_after){
	Uint val = (Uint) erts_smp_atomic_read(&erts_max_gen_gcs);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_fullsweep_after, make_small(val));
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_process_count) {
	BIF_RET(make_small(erts_process_count()));
    } else if (BIF_ARG_1 == am_process_limit) {
	BIF_RET(make_small(erts_max_processes));
    } else if (BIF_ARG_1 == am_info
	       || BIF_ARG_1 == am_procs
	       || BIF_ARG_1 == am_loaded
	       || BIF_ARG_1 == am_dist) {
	erts_dsprintf_buf_t *dsbufp = erts_create_info_dsbuf(0);

	/* Need to be the only thread running... */
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_smp_block_system(0);

	if (BIF_ARG_1 == am_info)
	    info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_procs)
	    process_info(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else if (BIF_ARG_1 == am_loaded)
	    loaded(ERTS_PRINT_DSBUF, (void *) dsbufp);
	else
	    distribution_info(ERTS_PRINT_DSBUF, (void *) dsbufp);

	erts_smp_release_system();
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	ASSERT(dsbufp && dsbufp->str);
	res = new_binary(BIF_P, (byte *) dsbufp->str, (int) dsbufp->str_len);
	erts_destroy_info_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (BIF_ARG_1 == AM_dist_ctrl) {
	DistEntry *dep;
	i = 0;
	ERTS_SMP_LOCK_NODE_TABLES_AND_ENTRIES;
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
	ERTS_SMP_UNLOCK_NODE_TABLES_AND_ENTRIES;
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_version) {
	erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(0);
	erts_print_system_version(ERTS_PRINT_DSBUF, (void *) dsbufp, BIF_P);
	hp = HAlloc(BIF_P, dsbufp->str_len*2);
	res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
	erts_destroy_tmp_dsbuf(dsbufp);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_system_architecture) {
	hp = HAlloc(BIF_P, 2*(sizeof(ERLANG_ARCHITECTURE)-1));
	BIF_RET(buf_to_intlist(&hp,
			       ERLANG_ARCHITECTURE,
			       sizeof(ERLANG_ARCHITECTURE)-1,
			       NIL));
    } 
    else if (BIF_ARG_1 == am_memory_types) {
	return erts_instr_get_type_info(BIF_P);
    }
    else if (BIF_ARG_1 == am_os_type) {
       Eterm type = am_atom_put(os_type, strlen(os_type));
       Eterm flav, tup;
       char *buf = erts_alloc(ERTS_ALC_T_TMP, 1024); /* More than enough */

       os_flavor(buf, 1024);
       flav = am_atom_put(buf, strlen(buf));
       hp = HAlloc(BIF_P, 3);
       tup = TUPLE2(hp, type, flav);
       erts_free(ERTS_ALC_T_TMP, (void *) buf);
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
	BIF_RET(buf_to_intlist(&hp, ERLANG_VERSION, n, NIL));
    }
    else if (BIF_ARG_1 == am_machine) {
	int n = strlen(EMULATOR);
	hp = HAlloc(BIF_P, n*2);
	BIF_RET(buf_to_intlist(&hp, EMULATOR, n, NIL));
    }
    else if (BIF_ARG_1 == am_garbage_collection) {
	BIF_RET(am_generational);
#ifndef ERTS_SMP /* Not supported with smp emulator */
    } else if (BIF_ARG_1 == am_instruction_counts) {
#ifdef DEBUG
	Eterm *endp;
#endif
	Eterm *hp, **hpp;
	Uint hsz, *hszp;
	int i;

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

    bld_instruction_counts:

	res = NIL;
	for (i = num_instructions-1; i >= 0; i--) {
	    res = erts_bld_cons(hpp, hszp,
				erts_bld_tuple(hpp, hszp, 2,
					       am_atom_put(opc[i].name,
							   strlen(opc[i].name)),
					       erts_bld_uint(hpp, hszp,
							     opc[i].count)),
				res);
	}

	if (!hpp) {
	    hp = HAlloc(BIF_P, hsz);
	    hpp = &hp;
#ifdef DEBUG
	    endp = hp + hsz;
#endif
	    hszp = NULL;
	    goto bld_instruction_counts;
	}

#ifdef DEBUG
	ASSERT(endp == hp);
#endif

	BIF_RET(res);
#endif /* #ifndef ERTS_SMP */
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
#ifdef HYBRID
	Uint hsz = 0;
	Uint sz = 0;

	sz += global_heap_sz;
#ifdef INCREMENTAL
        /* The size of the old generation is a bit hard to define here...
         * The amount of live data in the last collection perhaps..? */
        sz = 0;
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
#if defined(HYBRID)
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
	DECL_AM(constant_pool_support);
	DECL_AM(scheduler_id);
	DECL_AM(schedulers);
	DECL_AM(smp_support);
	DECL_AM(lock_checking);
	DECL_AM(kernel_poll);
	DECL_AM(check_io);
	DECL_AM(stop_memory_trace);

	if (BIF_ARG_1 == AM_smp_support) {
#ifdef ERTS_SMP
	    BIF_RET(am_true);
#else
	    BIF_RET(am_false);
#endif
	} else if (BIF_ARG_1 == AM_constant_pool_support) {
#if defined(HEAP_FRAG_ELIM_TEST)
	    BIF_RET(am_true);
#else
	    BIF_RET(am_false);
#endif
	} else if (BIF_ARG_1 == AM_scheduler_id) {
#ifdef ERTS_SMP
	    ASSERT(BIF_P->scheduler_data);
	    BIF_RET(erts_make_integer(BIF_P->scheduler_data->no, BIF_P));
#else
	    BIF_RET(make_small(1));
#endif
	} else if (BIF_ARG_1 == AM_schedulers) {
	    res = make_small(erts_get_no_schedulers());
	    BIF_RET(res);
	} else if (BIF_ARG_1 == AM_kernel_poll) {
#ifdef ERTS_ENABLE_KERNEL_POLL
	    BIF_RET(erts_use_kernel_poll ? am_true : am_false);
#else
	    BIF_RET(am_false);
#endif    
	} else if (BIF_ARG_1 == AM_lock_checking) {
#ifdef ERTS_ENABLE_LOCK_CHECK
	    BIF_RET(am_true);
#else
	    BIF_RET(am_false);
#endif
	} else if (BIF_ARG_1 == AM_check_io) {
	    BIF_RET(erts_check_io_info(BIF_P));
	} else if (BIF_ARG_1 == AM_stop_memory_trace) {
	    erts_mtrace_stop();
	    BIF_RET(am_true);
	}
	    
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
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
    Eterm reg_name;
    Eterm* hp;
    Uint need;
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
    reg_name = port_info_2(p, pid, am_registered_name);

    /*
     * Build the resulting list.
     */

    need = 2*ASIZE(keys);
    if (is_tuple(reg_name)) {
	need += 2;
    }
    hp = HAlloc(p, need);
    for (i = ASIZE(keys) - 1; i >= 0; i--) {
	result = CONS(hp, items[i], result);
	hp += 2;
    }
    if (is_tuple(reg_name)) {
	result = CONS(hp, reg_name, result);
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
    BIF_RETTYPE ret;
    Eterm portid = BIF_ARG_1;
    Port *prt;
    Eterm item = BIF_ARG_2;
    Eterm res;
    Eterm* hp;
    int count;

    if (is_internal_port(portid))
	prt = erts_id2port(portid, BIF_P, ERTS_PROC_LOCK_MAIN);
    else if (is_atom(portid))
	erts_whereis_name(BIF_P, ERTS_PROC_LOCK_MAIN, 0,
			  portid, NULL, 0, 0, &prt);
    else if (is_external_port(portid)
	     && external_port_dist_entry(portid) == erts_this_dist_entry)
	BIF_RET(am_undefined);
    else {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (!prt) {
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

	erts_doforall_links(prt->nlinks, &collect_one_link, &mic);

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
	count = sys_strlen(prt->name);

	hp = HAlloc(BIF_P, 3 + 2*count);
	res = buf_to_intlist(&hp, prt->name, count, NIL);
    }
    else if (item == am_connected) {
	hp = HAlloc(BIF_P, 3);
	res = prt->connected; /* internal pid */
    }
    else if (item == am_input) {
	Uint hsz = 3;
	Uint n = prt->bytes_in;
	(void) erts_bld_uint(NULL, &hsz, n);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, n);
    }
    else if (item == am_output) {
	Uint hsz = 3;
	Uint n = prt->bytes_out;
	(void) erts_bld_uint(NULL, &hsz, n);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, n);
    }
    else if (item == am_registered_name) {
	RegProc *reg;
	reg = prt->reg;
	if (reg == NULL) {
	    ERTS_BIF_PREP_RET(ret, NIL);
	    goto done;
	} else {
	    hp = HAlloc(BIF_P, 3);
	    res = reg->name;
	}
    }
    else if (item == am_memory) {
	/* All memory consumed in bytes (the Port struct should not be
	   included though).
	 */
	Uint hsz = 3;
	Uint size = 0;
	ErlHeapFragment* bp;

	hp = HAlloc(BIF_P, 3);

	erts_doforall_links(prt->nlinks, &one_link_size, &size);

	for (bp = prt->bp; bp; bp = bp->next)
	    size += sizeof(ErlHeapFragment) + (bp->size - 1)*sizeof(Eterm);

	if (prt->linebuf)
	    size += sizeof(LineBuf) + prt->linebuf->ovsiz;

	/* ... */


	/* All memory allocated by the driver should be included, but it is
	   hard to retrieve... */
	
	(void) erts_bld_uint(NULL, &hsz, size);
	hp = HAlloc(BIF_P, hsz);
	res = erts_bld_uint(&hp, NULL, size);
    }
    else {
	ERTS_BIF_PREP_ERROR(ret, BIF_P, BADARG);
	goto done;
    }

    ERTS_BIF_PREP_RET(ret, TUPLE2(hp, item, res));

 done:

    erts_smp_io_unlock();

    return ret;
}


Eterm
fun_info_2(Process* p, Eterm fun, Eterm what)
{
    Eterm* hp;
    Eterm val;

    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);

	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_local;
	    break;
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
	    val = erts_make_integer(erts_smp_atomic_read(&funp->fe->refc), p);
	    hp = HAlloc(p, 3);
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
    } else if (is_export(fun)) {
	Export* exp = (Export *) (export_val(fun))[1];
	switch (what) {
	case am_type:
	    hp = HAlloc(p, 3);
	    val = am_external;
	    break;
	case am_pid:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_module:
	    hp = HAlloc(p, 3);
	    val = exp->code[0];
	    break;
	case am_new_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_new_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_index:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_uniq:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_env:
	    hp = HAlloc(p, 3);
	    val = NIL;
	    break;
	case am_refc:
	    hp = HAlloc(p, 3);
	    val = am_undefined;
	    break;
	case am_arity:
	    hp = HAlloc(p, 3);
	    val = make_small(exp->code[2]);
	    break;
	case am_name:
	    hp = HAlloc(p, 3);
	    val = exp->code[1];
	    break;
	default:
	    goto error;
	}
    } else {
    error:
	BIF_ERROR(p, BADARG);
    }
    return TUPLE2(hp, what, val);
}


/* this is a general call which return some possibly useful information */

BIF_RETTYPE statistics_1(BIF_ALIST_1)
{
    Eterm res;
    Eterm* hp;

    if (BIF_ARG_1 == am_context_switches) {
	Eterm cs = erts_make_integer(erts_get_total_context_switches(), BIF_P);
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, cs, SMALL_ZERO);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_garbage_collection) {
	Uint hsz = 4;
	ErtsGCInfo gc_info;
	Eterm gcs;
	Eterm recl;
	erts_gc_info(&gc_info);
	(void) erts_bld_uint(NULL, &hsz, gc_info.garbage_collections);
	(void) erts_bld_uint(NULL, &hsz, gc_info.reclaimed);
	hp = HAlloc(BIF_P, hsz);
	gcs = erts_bld_uint(&hp, NULL, gc_info.garbage_collections);
	recl = erts_bld_uint(&hp, NULL, gc_info.reclaimed);
	res = TUPLE3(hp, gcs, recl, SMALL_ZERO);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_total_reductions(&reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_exact_reductions) {
	Uint reds;
	Uint diff;
	Uint hsz = 3;
	Eterm b1, b2;

	erts_get_exact_total_reductions(BIF_P, &reds, &diff);
	(void) erts_bld_uint(NULL, &hsz, reds);
	(void) erts_bld_uint(NULL, &hsz, diff);
	hp = HAlloc(BIF_P, hsz);
	b1 = erts_bld_uint(&hp, NULL, reds);
	b2 = erts_bld_uint(&hp, NULL, diff);
	res = TUPLE2(hp, b1, b2); 
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_runtime) {
	unsigned long u1, u2, dummy;
	Eterm b1, b2;
	elapsed_time_both(&u1,&dummy,&u2,&dummy);
	b1 = erts_make_integer(u1,BIF_P);
	b2 = erts_make_integer(u2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 ==  am_run_queue) {
	res = sched_q_len();
	BIF_RET(make_small(res));
    } else if (BIF_ARG_1 == am_wall_clock) {
	Uint w1, w2;
	Eterm b1, b2;
	wall_clock_elapsed_time_both(&w1, &w2);
	b1 = erts_make_integer(w1,BIF_P);
	b2 = erts_make_integer(w2,BIF_P);
	hp = HAlloc(BIF_P,3);
	res = TUPLE2(hp, b1, b2);
	BIF_RET(res);
    } else if (BIF_ARG_1 == am_io) {
	Eterm r1, r2;
	Eterm in, out;
	Uint hsz = 9;
	
	erts_smp_io_safe_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

	(void) erts_bld_uint(NULL, &hsz, bytes_in);
	(void) erts_bld_uint(NULL, &hsz, bytes_out);
	hp = HAlloc(BIF_P, hsz);
	in = erts_bld_uint(&hp, NULL, bytes_in);
	out = erts_bld_uint(&hp, NULL, bytes_out);

	erts_smp_io_unlock();

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

static erts_smp_atomic_t available_internal_state;

BIF_RETTYPE erts_debug_get_internal_state_1(BIF_ALIST_1)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */

    if (!erts_smp_atomic_read(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	DECL_AM(node_and_dist_references);
	DECL_AM(DbTable_words);
	DECL_AM(next_pid);
	DECL_AM(next_port);
	DECL_AM(check_io_debug);
	DECL_AM(available_internal_state);

	if (BIF_ARG_1 == AM_node_and_dist_references) {
	    /* Used by node_container_SUITE (emulator) */
	    Eterm res = erts_get_node_and_dist_references(BIF_P);
	    BIF_RET(res);
	}
	else if (BIF_ARG_1 == AM_next_pid || BIF_ARG_1 == AM_next_port) {
	    /* Used by node_container_SUITE (emulator) */
	    Eterm res;
	    if (BIF_ARG_1 == AM_next_pid)
		res = erts_test_next_pid(0, 0);
	    else {
		erts_smp_io_safe_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		res = erts_test_next_port(0, 0);
		erts_smp_io_unlock();
	    }
	    if (res < 0)
		BIF_RET(am_false);
	    BIF_RET(erts_make_integer(res, BIF_P));
	}
	else if (BIF_ARG_1 == AM_DbTable_words) {
	    /* Used by ets_SUITE (stdlib) */
	    size_t words = (sizeof(DbTable) + sizeof(Uint) - 1)/sizeof(Uint);
	    BIF_RET(make_small((Uint) words));
	}
	else if (BIF_ARG_1 == AM_check_io_debug) {
	    /* Used by (emulator) */
	    int res;
#ifdef HAVE_ERTS_CHECK_IO_DEBUG
	    res = erts_check_io_debug();
#else
	    res = 0;
#endif
	    ASSERT(res >= 0);
	    BIF_RET(erts_make_integer((Uint) res, BIF_P));
	}
	else if (BIF_ARG_1 == AM_available_internal_state) {
	    BIF_RET(am_true);
	}
    }
    else if (is_tuple(BIF_ARG_1)) {
	Eterm* tp = tuple_val(BIF_ARG_1);
	switch (arityval(tp[0])) {
	case 2: {
	    DECL_AM(link_list);
	    DECL_AM(monitor_list);
	    DECL_AM(channel_number);
	    DECL_AM(have_pending_exit);

	    if (tp[1] == AM_link_list) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
		    Eterm res;
		    Process *p;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_LINK);
		    if (!p) {
			ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
		    res = make_link_list(BIF_P, p->nlinks, NIL);
		    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
		    BIF_RET(res);
		}
		else if(is_internal_port(tp[2])) {
		    Eterm res;
		    Port *p = erts_id2port(tp[2], BIF_P, ERTS_PROC_LOCK_MAIN);
		    if(p)
			res = make_link_list(BIF_P, p->nlinks, NIL);
		    else
			res = am_undefined;
		    erts_smp_io_unlock();
		    BIF_RET(res);
		}
		else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm subres;
			erts_smp_dist_entry_lock(dep);
			subres = make_link_list(BIF_P, dep->nlinks, NIL);
			subres = make_link_list(BIF_P, dep->node_links, subres);
			erts_smp_dist_entry_unlock(dep);
			erts_deref_dist_entry(dep);
			BIF_RET(subres);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (tp[1] == AM_monitor_list) {
		/* Used by erl_link_SUITE (emulator) */
		if(is_internal_pid(tp[2])) {
		    Process *p;
		    Eterm res;

		    p = erts_pid2proc(BIF_P,
				      ERTS_PROC_LOCK_MAIN,
				      tp[2],
				      ERTS_PROC_LOCK_LINK);
		    if (!p) {
			ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
			BIF_RET(am_undefined);
		    }
		    res = make_monitor_list(BIF_P, p->monitors);
		    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_LINK);
		    BIF_RET(res);
		} else if(is_node_name_atom(tp[2])) {
		    DistEntry *dep = erts_find_dist_entry(tp[2]);
		    if(dep) {
			Eterm ml;
			erts_smp_dist_entry_lock(dep);
			ml = make_monitor_list(BIF_P, dep->monitors);
			erts_smp_dist_entry_unlock(dep);
			erts_deref_dist_entry(dep);
			BIF_RET(ml);
		    } else {
			BIF_RET(am_undefined);
		    }
		}
	    }
	    else if (tp[1] == AM_channel_number) {
		Eterm res;
		DistEntry *dep = erts_find_dist_entry(tp[2]);
		if (!dep)
		    res = am_undefined;
		else {
		    Uint cno = dist_entry_channel_no(dep);
		    res = make_small(cno);
		    erts_deref_dist_entry(dep);
		}
		BIF_RET(res);
	    }
	    else if (tp[1] == AM_have_pending_exit) {
		Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
					    tp[2], ERTS_PROC_LOCK_STATUS);
		if (!rp) {
		    BIF_RET(am_undefined);
		}
		else {
		    Eterm res = ERTS_PROC_PENDING_EXIT(rp) ? am_true : am_false;
		    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_STATUS);
		    BIF_RET(res);
		}
	    }

	    break;
	}
	default:
	    break;
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE erts_debug_set_internal_state_2(BIF_ALIST_2)
{
    DECL_AM(available_internal_state);
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */

    if (BIF_ARG_1 == AM_available_internal_state
	&& (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false)) {
	long on = (long) (BIF_ARG_2 == am_true);
	long prev_on = erts_smp_atomic_xchg(&available_internal_state, on);
	if (on) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Process %T ", BIF_P->id);
	    if (erts_is_alive())
		erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname);
	    erts_dsprintf(dsbufp,
			  "enabled access to the emulator internal state.\n");
	    erts_dsprintf(dsbufp,
			  "NOTE: This is an erts internal test feature and "
			  "should *only* be used by OTP test-suites.\n");
	    erts_send_warning_to_logger(BIF_P->group_leader, dsbufp);
	}
	BIF_RET(prev_on ? am_true : am_false);
    }

    if (!erts_smp_atomic_read(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	DECL_AM(next_pid);
	DECL_AM(next_port);
	DECL_AM(send_fake_exit_signal);
	
	if (BIF_ARG_1 == AM_next_pid || BIF_ARG_1 == AM_next_port) {
	    /* Used by node_container_SUITE (emulator) */
	    Uint next;

	    if (term_to_Uint(BIF_ARG_2, &next) != 0) {
		Eterm res;

		if (BIF_ARG_1 == AM_next_pid)
		    res = erts_test_next_pid(1, next);
		else {
		    erts_smp_io_safe_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
		    res = erts_test_next_port(1, next);
		    erts_smp_io_unlock();
		}
		if (res < 0)
		    BIF_RET(am_false);
		BIF_RET(erts_make_integer(res, BIF_P));
	    }
	}
	else if (BIF_ARG_1 == AM_send_fake_exit_signal) {
	    /* Used by signal_SUITE (emulator) */

	    /* Testcases depend on the exit being received via
	       a pending exit when the receiver is the same as
	       the caller.  */
	    if (is_tuple(BIF_ARG_2)) {
		Eterm* tp = tuple_val(BIF_ARG_2);
		if (arityval(tp[0]) == 3
		    && (is_pid(tp[1]) || is_port(tp[1]))
		    && is_internal_pid(tp[2])) {
		    int xres;
		    Uint32 rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
		    Process *rp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
						tp[2], rp_locks);
		    if (!rp) {
			DECL_AM(dead);
			BIF_RET(AM_dead);
		    }

#ifdef ERTS_SMP
		    if (BIF_P == rp)
			rp_locks |= ERTS_PROC_LOCK_MAIN;
#endif
		    xres = erts_send_exit_signal(NULL, /* NULL in order to
							  force a pending exit
							  when we send to our
							  selves. */
						 tp[1],
						 rp,
						 &rp_locks,
						 tp[3],
						 NIL,
						 NULL,
						 0);
#ifdef ERTS_SMP
		    if (BIF_P == rp)
			rp_locks &= ~ERTS_PROC_LOCK_MAIN;
#endif
		    erts_smp_proc_unlock(rp, rp_locks);
		    if (xres > 1) {
			DECL_AM(message);
			BIF_RET(AM_message);
		    }
		    else if (xres == 0) {
			DECL_AM(unaffected);
			BIF_RET(AM_unaffected);
		    }
		    else {
			DECL_AM(exit);
			BIF_RET(AM_exit);
		    }
		}
	    }
	}
    }

    BIF_ERROR(BIF_P, BADARG);
}

void
erts_bif_info_init(void)
{
#if defined(PURIFY) || defined(VALGRIND)
    INIT_AM(error_checker);
#ifdef VALGRIND
    INIT_AM(valgrind);
#endif
#endif
    erts_smp_atomic_init(&available_internal_state, 0);
}
