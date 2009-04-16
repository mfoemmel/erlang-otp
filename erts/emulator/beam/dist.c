/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */

/*
 * distribution of erlang messages to other nodes.
 */


/* define this to get a lot of debug output */
/* #define MESS_DEBUG */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "dist.h"
#include "bif.h"


#define PASS_THROUGH 'p'        /* This code should go */

/* distribution trap functions */
Export* dsend2_trap = NULL;
Export* dsend3_trap = NULL;
/*Export* dsend_nosuspend_trap = NULL;*/
Export* dlink_trap = NULL;
Export* dunlink_trap = NULL;
Export* dmonitor_node_trap = NULL;
Export* dgroup_leader_trap = NULL;
Export* dexit_trap = NULL;
Export* dmonitor_p_trap = NULL;

/* local variables */

static int node_is_alive;

/* forward declarations */

static int clear_dist_entry(DistEntry*);
static int pack_and_send(ErtsDistOpData *, Eterm, Eterm, int);
static void send_nodes_mon_msgs(Process *, Eterm, Eterm, Eterm, Eterm);
static void init_nodes_monitors(void);

static Uint no_caches;

static void
init_alive(void)
{
    node_is_alive = 0;
}

static void
set_alive(void)
{
    node_is_alive = 1;
}

static void
set_not_alive(void)
{
    node_is_alive = 0;
}

static ERTS_INLINE int
is_alive(void)
{
    return node_is_alive;
}

int erts_is_alive(void)
{
    return is_alive();
}

void clear_cache(DistEntry *dep)
{
    ErlCache* cp;

    if ((cp = dep->cache) != NULL) {
	int i;
	for(i = 0; i < sizeof(cp->in_arr)/sizeof(cp->in_arr[0]); ++i) {
	    cp->in_arr[i] = THE_NON_VALUE;
	    cp->out_arr[i] = THE_NON_VALUE;
	}
    }
}

void delete_cache(DistEntry *dep)
{
    ErlCache* cp;

    if ((cp = dep->cache) != NULL) {
	erts_free(ERTS_ALC_T_DCACHE, (void *) cp);
	ASSERT(no_caches > 0);
	no_caches--;
	dep->cache = NULL;
    }
}


void create_cache(DistEntry *dep)
{
    ErlCache* cp;

    if ((cp = dep->cache) == NULL) {
	dep->cache = (ErlCache*) erts_alloc(ERTS_ALC_T_DCACHE,sizeof(ErlCache));
	no_caches++;
	clear_cache(dep);
    }
}

Uint erts_dist_cache_size(void)
{
    return no_caches*sizeof(ErlCache);
}

/*
** A full node name constists of a "n@h"
**
** n must be a valid node name: string of ([a-z][A-Z][0-9]_-)+
** 
** h is not checked at all, we assume that we have a properly
** configured machine where the networking is ok for the OS
**
** We do check that there is not a second @ in the string, since
** many distributed operations are guaranteed not to work then.
*/


static int is_node_name(char *ptr, int len)
{
   int c = '\0';		/* suppress use-before-set warning */
   int pos = 0;

   while (pos < len) {
      c = ptr[pos++];
      if (! ((c == '-') || (c == '_') ||
	     ((c >= 'a') && (c <= 'z')) ||
	     ((c >= 'A') && (c <= 'Z')) ||
	     ((c >= '0') && (c <= '9'))))
	 break;
   }

   /* Scanned past the host name: now we want to see a '@', and there
      should be text both before and after it. */
   if (c != '@' || pos < 2 || pos == len)
      return 0;

   while (pos < len) {
      c = ptr[pos++];
      if (c == '@')
	 return 0;
   }

   return 1;
}

int is_node_name_atom(Eterm a)
{
    int i;
    if(is_not_atom(a))
	return 0;
    i = atom_val(a);
    ASSERT((i > 0) && (i < atom_table_size()) &&  (atom_tab(i) != NULL));
    return is_node_name((char*)atom_tab(i)->name, atom_tab(i)->len);
}

typedef struct {
    DistEntry *dep;
} NetExitsContext;

/* 
** This function is called when a distribution 
** port or process terminates
*/
static void doit_monitor_net_exits(ErtsMonitor *mon, void *vnecp)
{
    Process *rp;
    ErtsMonitor *rmon;
    DistEntry *dep = ((NetExitsContext *) vnecp)->dep;
    ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;

    rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);
    if (!rp) {
	ASSERT(0); /* ? */
	goto done;
    }

    if (mon->type == MON_ORIGIN) {
	/* local pid is beeing monitored */
	rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	/* ASSERT(rmon != NULL); nope, can happen during process exit */
	if (rmon != NULL) {
	    erts_destroy_monitor(rmon);
	}
    } else {
	Eterm lhp[3];
	Eterm watched;
	ASSERT(mon->type == MON_TARGET);
	rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	/* ASSERT(rmon != NULL); can happen during process exit */
	if (rmon != NULL) {
	    ASSERT(is_atom(rmon->name) || is_nil(rmon->name));
	    watched = (is_atom(rmon->name)
		       ? TUPLE2(lhp, rmon->name, dep->sysname)
		       : rmon->pid);
#ifdef ERTS_SMP
	    rp_locks |= ERTS_PROC_LOCKS_MSG_SEND;
	    erts_smp_proc_lock(rp, ERTS_PROC_LOCKS_MSG_SEND);
#endif
	    erts_queue_monitor_message(rp, &rp_locks, mon->ref, am_process, 
				       watched, am_noconnection);
	    erts_destroy_monitor(rmon);
	}
    }
    erts_smp_proc_unlock(rp, rp_locks);
 done:
    erts_destroy_monitor(mon);
}
	
typedef struct {
    NetExitsContext *necp;
    ErtsLink *lnk;
} LinkNetExitsContext;

/* 
** This is the function actually doing the job of sending exit messages
** for links in a dist entry upon net_exit (the node goes down), NB,
** only process links, not node monitors are handled here, 
** they reside in a separate tree....
*/
static void doit_link_net_exits_sub(ErtsLink *sublnk, void *vlnecp)
{
    ErtsLink *lnk = ((LinkNetExitsContext *) vlnecp)->lnk; /* the local pid */
    ErtsLink *rlnk;
    Process *rp;

    ASSERT(lnk->type == LINK_PID);
    if (is_internal_pid(lnk->pid)) {
	int xres;
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;

	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (!rp) {
	    goto done;
	}

	rlnk = erts_remove_link(&(rp->nlinks), sublnk->pid);
	xres = erts_send_exit_signal(NULL,
				     sublnk->pid,
				     rp,
				     &rp_locks,
				     am_noconnection,
				     NIL,
				     NULL,
				     0);

	if (rlnk) {
	    erts_destroy_link(rlnk);
	    if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
		/* We didn't exit the process and it is traced */
		trace_proc(NULL, rp, am_getting_unlinked, sublnk->pid);
	    }
	}
	erts_smp_proc_unlock(rp, rp_locks);
    }
 done:
    erts_destroy_link(sublnk);

}
    




/* 
** This function is called when a distribution 
** port or process terminates, once for each link on the high level, 
** it in turn traverses the link subtree for the specific link node...
*/
static void doit_link_net_exits(ErtsLink *lnk, void *vnecp)
{
    LinkNetExitsContext lnec = {(NetExitsContext *) vnecp, lnk};
    ASSERT(lnk->type == LINK_PID)
    erts_sweep_links(ERTS_LINK_ROOT(lnk), &doit_link_net_exits_sub, (void *) &lnec);
#ifdef DEBUG
    ERTS_LINK_ROOT(lnk) = NULL;
#endif
    erts_destroy_link(lnk);
}


static void doit_node_link_net_exits(ErtsLink *lnk, void *vnecp)
{
    DistEntry *dep = ((NetExitsContext *) vnecp)->dep;
    Eterm name = dep->sysname;
    Process *rp;
    ErtsLink *rlnk;
    Uint i,n;
    ASSERT(lnk->type == LINK_NODE)
    if (is_internal_pid(lnk->pid)) {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK;
	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (!rp) {
	    goto done;
	}
	rlnk = erts_remove_link(&(rp->nlinks), name);
	if (rlnk != NULL) {
	    ASSERT(is_atom(rlnk->pid) && (rlnk->type == LINK_NODE));
	    erts_destroy_link(rlnk);
	}
	n = ERTS_LINK_REFC(lnk);
	for (i = 0; i < n; ++i) {
	    ErlHeapFragment* bp;
	    ErlOffHeap *ohp;
	    Eterm tup;
	    Eterm *hp = erts_alloc_message_heap(3,&bp,&ohp,rp,&rp_locks);
	    tup = TUPLE2(hp, am_nodedown, name);
	    erts_queue_message(rp, &rp_locks, bp, tup, NIL);
	}
	erts_smp_proc_unlock(rp, rp_locks);
    }
 done:
    erts_destroy_link(lnk);
}

	
/*
 * proc is currently running or exiting process.
 */
int erts_do_net_exits(DistEntry *dep, Eterm reason)
{
    Eterm nodename;

    if (dep == erts_this_dist_entry) {  /* Net kernel has died (clean up!!) */
	Eterm nd_reason = (reason == am_no_network
			   ? am_no_network
			   : am_net_kernel_terminated);
	erts_smp_mtx_lock(&erts_dist_table_mtx);

	/* KILL all port controllers */
	while(erts_visible_dist_entries || erts_hidden_dist_entries) {
	    DistEntry *tdep;
	    Eterm prt_id;
	    Port *prt;
	    if(erts_hidden_dist_entries)
		tdep = erts_hidden_dist_entries;
	    else
		tdep = erts_visible_dist_entries;
	    prt_id = tdep->cid;
	    ASSERT(is_internal_port(prt_id));
	    erts_smp_mtx_unlock(&erts_dist_table_mtx);

	    prt = erts_id2port(prt_id, NULL, 0);
	    if (prt) {
		ASSERT(prt->status & ERTS_PORT_SFLG_DISTRIBUTION);
		ASSERT(prt->dist_entry);
		/* will call do_net_exists !!! */
		erts_do_exit_port(prt, prt_id, nd_reason);
		erts_port_release(prt);
	    }

	    erts_smp_mtx_lock(&erts_dist_table_mtx);
	}

	erts_smp_mtx_unlock(&erts_dist_table_mtx);

	nodename = erts_this_dist_entry->sysname;
	erts_smp_block_system(ERTS_BS_FLG_ALLOW_GC);
	erts_set_this_node(am_Noname, 0);
	set_not_alive();
	send_nodes_mon_msgs(NULL, am_nodedown, nodename, am_visible, nd_reason);
	erts_smp_release_system();

    }
    else { /* recursive call via erts_do_exit_port() will end up here */
	NetExitsContext nec = {dep};
	ErtsLink *nlinks;
	ErtsLink *node_links;
	ErtsMonitor *monitors;
	Uint32 flags;

	erts_smp_dist_entry_lock(dep);

	ERTS_SMP_LC_ASSERT(is_internal_port(dep->cid)
			   && erts_lc_is_port_locked(&erts_port[internal_port_index(dep->cid)]));

	/*
	 * NOTE: We are *not* allowed to release the port lock between
	 *       this point and the call to clear_dist_entry()
	 *       (setnode_3 depends on it).
	 */
	dep->status |= ERTS_DE_SFLG_EXITING;

	monitors	= dep->monitors;
        nlinks		= dep->nlinks;
	node_links	= dep->node_links;
	dep->monitors	= NULL;
        dep->nlinks	= NULL;
	dep->node_links	= NULL;

	nodename = dep->sysname;
	flags = dep->flags;

	erts_smp_dist_entry_unlock(dep);

	erts_sweep_monitors(monitors, &doit_monitor_net_exits, (void *) &nec);
	erts_sweep_links(nlinks, &doit_link_net_exits, (void *) &nec);
	erts_sweep_links(node_links, &doit_node_link_net_exits, (void *) &nec);
	clear_dist_entry(dep);

	send_nodes_mon_msgs(NULL,
			    am_nodedown,
			    nodename,
			    flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			    reason == am_normal ? am_connection_closed : reason);
    }
    return 1;
}

static Export*
trap_function(Eterm func, int arity)
{
    return erts_export_put(am_erlang, func, arity);
}

void init_dist(void)
{
    init_alive();
    init_nodes_monitors();

    no_caches = 0;

    /* Lookup/Install all references to trap functions */
    dsend2_trap = trap_function(am_dsend,2);
    dsend3_trap = trap_function(am_dsend,3);
    /*    dsend_nosuspend_trap = trap_function(am_dsend_nosuspend,2);*/
    dlink_trap = trap_function(am_dlink,1);
    dunlink_trap = trap_function(am_dunlink,1);
    dmonitor_node_trap = trap_function(am_dmonitor_node,3);
    dgroup_leader_trap = trap_function(am_dgroup_leader,2);
    dexit_trap = trap_function(am_dexit, 2);
    dmonitor_p_trap = trap_function(am_dmonitor_p, 2);
}

static int clear_dist_entry(DistEntry *dep)
{
    erts_smp_dist_entry_lock(dep);
    clear_cache(dep);
    erts_set_dist_entry_not_connected(dep);
    dep->nlinks = NULL;
    dep->node_links = NULL;
    dep->monitors = NULL;
    dep->status &= ERTS_DE_SFLG_INITIALIZING; /* Someone else might be
						 initializing so we are
						 not allowed to clear
						 the initializing flag */
    erts_smp_dist_entry_unlock(dep);
    return 0;
}

/*
 * SMP NOTE on dist_*() functions:
 *
 * Requirements for usage of dist_*() functions:
 *   Port lock on controlling port, and lock on dep has to be held,
 *   and if c_p != NULL, at least main lock has to be held on c_p.
 *
 * Also note that lock on dep will be released and reacquired,
 * and that lock(s) on c_p may be released and reacquired.
 *
 */

/*
** Send a DOP_LINK link message
*/
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_link(ErtsDistOpData *dodp, Eterm local, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_LINK), local, remote);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 0);
}


/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_unlink(ErtsDistOpData *dodp, Eterm local, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_UNLINK), local, remote);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 0);
}


/* A local process that's beeing monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason},
   which is rather sad as only the ref is needed, no pid's... */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_m_exit(ErtsDistOpData *dodp, Eterm watcher, Eterm watched, 
		     Eterm ref, Eterm reason)
{
    Eterm ctl;
    Eterm ctl_heap[6];

    ctl = TUPLE5(&ctl_heap[0], make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

    ASSERT(!erts_lookup_monitor(dodp->dep->monitors, ref));

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 1);
}
/* We want to monitor a process (named or unnamed) on another node, we send:
   {DOP_MONITOR_P, Local pid, Remote pid or name, Ref}, which is exactly what's
   needed on the other side... */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_monitor(ErtsDistOpData *dodp, Eterm watcher, Eterm watched,
		      Eterm ref)
{
    Eterm ctl;
    Eterm ctl_heap[5];

    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 0);
}

/* A local process monitoring a remote one wants to stop monitoring, either 
   because of a demonitor bif call or because the local process died. We send
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref}, which is once again
   rather redundant as only the ref will be needed on the other side... */

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_demonitor(ErtsDistOpData *dodp, Eterm watcher, Eterm watched, 
			Eterm ref, int force)
{
    Eterm ctl;
    Eterm ctl_heap[5];

    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, force);
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_send(ErtsDistOpData *dodp, Eterm remote, Eterm message)
{
    Eterm ctl;
    Eterm ctl_heap[5];
    Eterm token = NIL;
    Process *sender = dodp->proc;

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote, sender);
    }

    if (token != NIL)
	ctl = TUPLE4(&ctl_heap[0],
		     make_small(DOP_SEND_TT), am_Cookie, remote, token);
    else
	ctl = TUPLE3(&ctl_heap[0], make_small(DOP_SEND), am_Cookie, remote);
    return pack_and_send(dodp, ctl, message, 0);
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_reg_send(ErtsDistOpData *dodp, Eterm remote_name, Eterm message)
{
    Eterm ctl;
    Eterm ctl_heap[6];
    Eterm token = NIL;
    Process *sender = dodp->proc;

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote_name, sender);
    }

    if (token != NIL)
	ctl = TUPLE5(&ctl_heap[0], make_small(DOP_REG_SEND_TT),
		     sender->id, am_Cookie, remote_name, token);
    else
	ctl = TUPLE4(&ctl_heap[0], make_small(DOP_REG_SEND),
		     sender->id, am_Cookie, remote_name);
    return pack_and_send(dodp, ctl, message, 0);
}

/* local has died, deliver the exit signal to remote
** We must always send the exit message to the other node
** this implies that the driver must always be ready to queue
** data even if it has signaled that it is busy !!!
*/
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_exit_tt(ErtsDistOpData *dodp, Eterm local, Eterm remote, 
		      Eterm reason, Eterm token)
{
    Eterm ctl;
    Eterm ctl_heap[6];
    ErtsLink *lnk, *sublnk;
    DistEntry *dep = dodp->dep;

    if (token != NIL) {	
	/* token should be updated by caller */
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
	ctl = TUPLE5(&ctl_heap[0],
		     make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
	ctl = TUPLE4(&ctl_heap[0], make_small(DOP_EXIT), local, remote, reason);
    }
    
    lnk = erts_lookup_link(dep->nlinks, local);

    if (lnk != NULL) {
	sublnk = erts_remove_link(&(ERTS_LINK_ROOT(lnk)), remote);
	if (sublnk != NULL) {
	    erts_destroy_link(sublnk);
	}
	if (ERTS_LINK_ROOT(lnk) == NULL) {
	    erts_destroy_link(erts_remove_link(&(dep->nlinks), local));
	}
    }

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_exit(ErtsDistOpData *dodp,
		   Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl_heap[5];
    Eterm ctl = TUPLE4(&ctl_heap[0],
		       make_small(DOP_EXIT), local, remote, reason);
    ErtsLink *lnk, *sublnk;
    DistEntry *dep = dodp->dep;

    lnk = erts_lookup_link(dep->nlinks, local);

    if (lnk != NULL) {
	sublnk = erts_remove_link(&(ERTS_LINK_ROOT(lnk)), remote);
	if (sublnk != NULL) {
	    erts_destroy_link(sublnk);
	}
	if (ERTS_LINK_ROOT(lnk) == NULL) {
	    erts_destroy_link(erts_remove_link(&(dep->nlinks), local));
	}
    }

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* internal version of dist_exit2 that force send through busy port */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_exit2(ErtsDistOpData *dodp,
		    Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl_heap[5];
    Eterm ctl = TUPLE4(&ctl_heap[0],
		       make_small(DOP_EXIT2), local, remote, reason);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 0);
}


/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int erts_dist_group_leader(ErtsDistOpData *dodp, Eterm leader, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0],
		       make_small(DOP_GROUP_LEADER), leader, remote);

    return pack_and_send(dodp, ctl, THE_NON_VALUE, 0);
}

#if defined(PURIFY)
#  define PURIFY_MSG(msg) \
    purify_printf("%s, line %d: %s", __FILE__, __LINE__, msg)
#elif defined(VALGRIND)
#include <valgrind/valgrind.h>
#include <valgrind/memcheck.h>

#  define PURIFY_MSG(msg)                                                \
    do {								 \
	char buf__[1]; size_t bufsz__ = sizeof(buf__);			 \
	if (erts_sys_getenv("VALGRIND_LOG_XML", buf__, &bufsz__) >= 0) { \
	    VALGRIND_PRINTF("<erlang_error_log>"			 \
			    "%s, line %d: %s</erlang_error_log>\n",	 \
			    __FILE__, __LINE__, msg);			 \
	} else {							 \
	    VALGRIND_PRINTF("%s, line %d: %s", __FILE__, __LINE__, msg); \
	}								 \
    } while (0)
#else
#  define PURIFY_MSG(msg)
#endif

/*
** Input from distribution port.
**  Input follows the distribution protocol v4.5
**  
**   The protocol is a 4 byte header protocol
**   the DOP_DATA is stripped by driver_output
**
**   assert  hlen == 0 !!!
*/
int erts_net_message(Port *prt,
		     DistEntry *dep,
		     byte *hbuf,
		     int hlen,
		     byte *buf,
		     int len)
{
    byte *t;
    Sint ctl_len;
    Sint decoded_size;
    int orig_ctl_len;
    Eterm arg;
    Eterm from, to;
    Eterm watcher, watched;
    Eterm ref;
    Eterm *tuple;
    Eterm message;
    Eterm reason;
    Process* rp;
    Eterm ctl_default[64];
    Eterm* ctl = ctl_default;
    ErlOffHeap off_heap;
    Eterm* hp;
    Eterm* hp_end;
    Sint type;
    Eterm token;
    Eterm token_size;
    int orig_len = len;
    ErtsMonitor *mon;
    ErtsLink *lnk, *sublnk;
    int res;

    /* Thanks to Luke Gorrie */
    off_heap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    off_heap.funs = NULL;
#endif
    off_heap.overhead = 0;
    off_heap.externals = NULL;

    ERTS_SMP_CHK_NO_PROC_LOCKS;

    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(prt));

    if (!is_alive())
	return 0;
    if (hlen > 0)
	goto data_error;
    if (len == 0)  /* HANDLE TICK !!! */
	return 0;
    t = buf+1;     /* Skip PASS_THROUGH */


    if (len == 1) {
	PURIFY_MSG("data error");
	goto data_error;
    }
#ifdef MESS_DEBUG
    print_pass_through(dep, t, len-1);
#endif
    if ((ctl_len = erts_decoded_size(t, len-1, 0)) == -1) {
	PURIFY_MSG("data error");
	goto data_error;
    }
    orig_ctl_len = ctl_len;
    if (ctl_len > sizeof(ctl_default)/sizeof(ctl_default[0])) {
	ctl = erts_alloc(ERTS_ALC_T_DCTRL_BUF, ctl_len * sizeof(Eterm));
    }
    hp = ctl;

    erts_smp_dist_entry_lock(dep);
    arg = erts_from_external_format(dep, &hp, &t, &off_heap);
    erts_smp_dist_entry_unlock(dep);
    if (is_non_value(arg)) {
	PURIFY_MSG("data error");
	goto data_error;
    }
    ctl_len = t - buf;
    len -= ctl_len;

    if (is_not_tuple(arg) || 
	(tuple = tuple_val(arg), arityval(*tuple) < 1) ||
	is_not_small(tuple[1])) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Invalid distribution message: %.200T", arg);
	erts_send_error_to_logger_nogl(dsbufp);
	goto data_error;
    }

    token_size = 0;

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK:
	from = tuple[2];
	to   = tuple[3];  /* local proc to link to */

	if (is_not_pid(from) || is_not_pid(to)) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    PURIFY_MSG("data error");
	    erts_dsprintf(dsbufp,
			  "Invalid DOP_LINK distribution message: %.200T",
			  arg);
	    erts_send_error_to_logger_nogl(dsbufp);
	    goto data_error;
	}

	rp = erts_pid2proc_opt(NULL, 0,
			       to, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	erts_smp_dist_entry_lock(dep);
	if (!rp) {
	    /* This is tricky (we MUST force a distributed send) */
	    /* We may send it to net_kernel and let it do the job !!! */
	    ErtsDistOpData dod;
	    ERTS_DIST_OP_DATA_INIT(&dod, NULL, 0, dep, prt);
	    ASSERT(dep->cid == prt->id);
	    erts_dist_exit(&dod, to, from, am_noproc);
	    erts_smp_dist_entry_unlock(dep);
	    break;
	}

	res = erts_add_link(&(rp->nlinks), LINK_PID, from);

	if (res < 0) {
	    /* It was already there! Lets skip the rest... */
	    erts_smp_dist_entry_unlock(dep);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	    break;
	}
	lnk = erts_add_or_lookup_link(&(dep->nlinks), LINK_PID, rp->id);
	erts_add_link(&(ERTS_LINK_ROOT(lnk)), LINK_PID, from);
	erts_smp_dist_entry_unlock(dep);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(NULL, rp, am_getting_linked, from);

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	break;

    case DOP_UNLINK: {
	from = tuple[2];
	to = tuple[3];
	
	rp = erts_pid2proc_opt(NULL, 0,
			       to, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (!rp)
	    break;

	erts_smp_dist_entry_lock(dep);
	lnk = erts_remove_link(&(rp->nlinks), from);
	if (lnk != NULL) {
	    erts_destroy_link(lnk);
	}

	lnk = erts_lookup_link(dep->nlinks, rp->id); 
	if (lnk != NULL) {
	    sublnk = erts_remove_link(&(ERTS_LINK_ROOT(lnk)), from);
	    if (sublnk != NULL) {
		erts_destroy_link(sublnk);
	    }
	    if (ERTS_LINK_ROOT(lnk) == NULL) {
		erts_destroy_link(erts_remove_link(&(dep->nlinks), rp->id));
	    } 
	}

	erts_smp_dist_entry_unlock(dep);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && lnk != NULL) {
	    trace_proc(NULL, rp, am_getting_unlinked, from);
	}

	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	break;
    }
    
    case DOP_MONITOR_P: {
	/* A remote process wants to monitor us, we get:
	   {DOP_MONITOR_P, Remote pid, local pid or name, ref} */
	Eterm name;

	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

	if (is_atom(watched)) {
	    name = watched;
	    rp = erts_whereis_process(NULL, 0,
				      watched, ERTS_PROC_LOCK_LINK,
				      ERTS_P2P_FLG_ALLOW_OTHER_X);
	}
	else {
	    name = NIL;
	    rp = erts_pid2proc_opt(NULL, 0,
				   watched, ERTS_PROC_LOCK_LINK,
				   ERTS_P2P_FLG_ALLOW_OTHER_X);
	}

	erts_smp_dist_entry_lock(dep);
	
	if (!rp) {
	    ErtsDistOpData dod;
	    ERTS_DIST_OP_DATA_INIT(&dod, NULL, 0, dep, prt);
	    ASSERT(dep->cid == prt->id);
	    erts_dist_m_exit(&dod, watcher, watched, ref, am_noproc);
	}
	else {
	    if (is_atom(watched))
		watched = rp->id;
	    erts_add_monitor(&(dep->monitors), MON_ORIGIN, ref, watched, name);
	    erts_add_monitor(&(rp->monitors), MON_TARGET, ref, watcher, name);
	    erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	}

	erts_smp_dist_entry_unlock(dep);

	break;
    }

    case DOP_DEMONITOR_P:
	/* A remote node informs us that a local pid in no longer monitored
	   We get {DOP_DEMONITOR_P, Remote pid, Local pid or name, ref},
	   We need only the ref of course */

	/* watcher = tuple[2]; */
	/* watched = tuple[3]; May be an atom in case of monitor name */
	ref = tuple[4];

	erts_smp_dist_entry_lock(dep);
	mon = erts_remove_monitor(&(dep->monitors),ref);
	/* ASSERT(mon != NULL); can happen in case of broken dist message */
	if (mon == NULL) {
	    erts_smp_dist_entry_unlock(dep);
	    break;
	}
	watched = mon->pid;
	erts_destroy_monitor(mon);
	erts_smp_dist_entry_unlock(dep);
	rp = erts_pid2proc_opt(NULL, 0,
			       watched, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	if (!rp) {
	    break;
	}
	mon = erts_remove_monitor(&(rp->monitors),ref);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_LINK);
	ASSERT(mon != NULL);
	if (mon == NULL) {
	    break;
	}
	erts_destroy_monitor(mon);
	break;

    case DOP_NODE_LINK: /* XXX never sent ?? */
	break;

    case DOP_REG_SEND_TT:
	token_size = size_object(tuple[5]);
	/* Fall through ... */
    case DOP_REG_SEND:
	/* {DOP_REG_SEND, From, Cookie, ToName} -- Message */
	/* {DOP_REG_SEND_TT, From, Cookie, ToName, TraceToken} -- Message */

	/*
	 * There is intentionally no testing of the cookie (it is always '')
	 * from R9B and onwards.
	 */

	if ((decoded_size = erts_decoded_size(t, len, 0)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}
	from = tuple[2];
	to = tuple[4];
	rp = erts_whereis_process(NULL, 0, to, 0, ERTS_P2P_FLG_SMP_INC_REFC);
	if (!rp) {
	    ErlHeapFragment* msg;
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    msg = new_message_buffer(decoded_size);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
	    ErtsProcLocks locks = 0;
	    ErlHeapFragment *bp;
	    ErlOffHeap *ohp;
	    hp = erts_alloc_message_heap(decoded_size+token_size,
					 &bp,&ohp,rp,&locks);
	    hp_end = hp + decoded_size + token_size;
	    message = erts_from_external_format(dep, &hp, &t, ohp);
	    if (is_non_value(message)) {
		PURIFY_MSG("data error");
		goto data_error;
	    }
	    if (type == DOP_REG_SEND) {
		token = NIL;
	    } else {
		token = tuple[5];
		token = copy_struct(token, token_size, &hp, ohp);
	    }
	    if (!bp) {
		HRelease(rp,hp_end,hp);
	    }
	    else {
		Uint final_size = hp - &bp->mem[0];
		Eterm brefs[2] = {message, token};
		ASSERT(decoded_size + token_size - (hp_end - hp) == final_size);
		bp = erts_resize_message_buffer(bp, final_size, &brefs[0], 2);
		message = brefs[0];
		token = brefs[1];
	    }
	    erts_queue_message(rp, &locks, bp, message, token);
	    erts_smp_proc_unlock(rp, locks);
	    erts_smp_proc_dec_refc(rp);
	}
	break;

    case DOP_SEND_TT:
	token_size = size_object(tuple[4]);
	/* Fall through ... */
    case DOP_SEND:
	/*
	 * There is intentionally no testing of the cookie (it is always '')
	 * from R9B and onwards.
	 */

	if ((decoded_size = erts_decoded_size(t, len, 0)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}

	to = tuple[3];
	rp = erts_pid2proc_opt(NULL, 0, to, 0, ERTS_P2P_FLG_SMP_INC_REFC);
	if (!rp) {
	    ErlHeapFragment* msg;
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    msg = new_message_buffer(decoded_size);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
	    ErtsProcLocks locks = 0;
	    ErlOffHeap *ohp;
	    ErlHeapFragment *bp;
	    hp = erts_alloc_message_heap(decoded_size+token_size,&bp,&ohp,rp,&locks);

	    hp_end = hp + decoded_size + token_size;
	    message = erts_from_external_format(dep, &hp, &t, ohp);
	    if (is_non_value(message)) {
		PURIFY_MSG("data error");
		goto data_error;
	    }
	    if (type == DOP_SEND) {
		token = NIL;
	    } else {
		token = tuple[4];
		token = copy_struct(token, token_size, &hp, ohp);
	    }
	    if (!bp) {
		HRelease(rp,hp_end,hp);
	    }
	    else {
		Uint final_size = hp - &bp->mem[0];
		Eterm brefs[2] = {message, token};
		ASSERT(decoded_size + token_size - (hp_end - hp) == final_size);
		bp = erts_resize_message_buffer(bp, final_size, &brefs[0], 2);
		message = brefs[0];
		token = brefs[1];
	    }
	    erts_queue_message(rp, &locks, bp, message, token);
	    erts_smp_proc_unlock(rp, locks);
	    erts_smp_proc_dec_refc(rp);
	}
	break;

    case DOP_MONITOR_P_EXIT: {
	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */
	   

	Eterm lhp[3];
	Eterm sysname;
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_MSG_SEND|ERTS_PROC_LOCK_LINK;

	/* watched = tuple[2]; */  /* remote proc which died */
	/* watcher = tuple[3]; */
	ref     = tuple[4];
	reason  = tuple[5];

	erts_smp_dist_entry_lock(dep);
	sysname = dep->sysname;
	mon = erts_remove_monitor(&(dep->monitors), ref);
	/*
	 * If demonitor was performed at the same time as the
	 * monitored process exits, monitoring side will have
	 * removed info about monitor. In this case, do nothing
	 * and everything will be as it should.
	 */
	erts_smp_dist_entry_unlock(dep);
	if (mon == NULL) {
	    break;
	}
	rp = erts_pid2proc(NULL, 0, mon->pid, rp_locks);
	if (rp == NULL) {
	    break;
	}

	erts_destroy_monitor(mon);

	mon = erts_remove_monitor(&(rp->monitors),ref);

	if (mon == NULL) {
	    erts_smp_proc_unlock(rp, rp_locks);
	    break;
	}
	
	watched = (is_not_nil(mon->name)
		   ? TUPLE2(&lhp[0], mon->name, sysname)
		   : mon->pid);
	
	erts_queue_monitor_message(rp, &rp_locks,
				   ref, am_process, watched, reason);
	erts_smp_proc_unlock(rp, rp_locks);
	erts_destroy_monitor(mon);
	break;
    }

    case DOP_EXIT_TT:
    case DOP_EXIT: {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
	/* 'from', which 'to' is linked to, died */
	if (type == DOP_EXIT) {
	   from = tuple[2];
	   to = tuple[3];
	   reason = tuple[4];
	   token = NIL;
	} else {
	   from = tuple[2];
	   to = tuple[3];
	   token = tuple[4];
	   reason = tuple[5];
	}
	if (is_not_internal_pid(to)) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    PURIFY_MSG("data error");
	    erts_dsprintf(dsbufp,
			  "Invalid DOP_EXIT distribution message: %.200T",
			  arg);
	    erts_send_error_to_logger_nogl(dsbufp);
	    goto data_error;
	}

	erts_smp_dist_entry_lock(dep);
	lnk = erts_lookup_link(dep->nlinks, to); 
	if (lnk != NULL) {
	    sublnk = erts_remove_link(&(ERTS_LINK_ROOT(lnk)), from);
	    if (sublnk != NULL) {
		erts_destroy_link(sublnk);
	    }
	    if (ERTS_LINK_ROOT(lnk) == NULL) {
		erts_destroy_link(erts_remove_link(&(dep->nlinks), to));
	    } 
	}

	erts_smp_dist_entry_unlock(dep);

	rp = erts_pid2proc(NULL, 0, to, rp_locks);
	if (rp) {
	    lnk = erts_remove_link(&(rp->nlinks), from);

	    /* If lnk == NULL, we have unlinked on this side, i.e.
	     * ignore exit.
	     */
	    if (lnk) {
		int xres;
		erts_destroy_link(lnk);
#if 0
		/* Arndt: Maybe it should never be 'kill', but it can be,
		   namely when a linked process does exit(kill). Until we know
		   whether that is incorrect and what should happen instead,
		   we leave the assertion out. */
		ASSERT(reason != am_kill); /* should never be kill (killed) */
#endif
		xres = erts_send_exit_signal(NULL,
					     from,
					     rp,
					     &rp_locks, 
					     reason,
					     token,
					     NULL,
					     ERTS_XSIG_FLG_IGN_KILL);
		if (xres >= 0 && IS_TRACED_FL(rp, F_TRACE_PROCS)) {
		    /* We didn't exit the process and it is traced */
		    trace_proc(NULL, rp, am_getting_unlinked, from);
		}
	    }
	    erts_smp_proc_unlock(rp, rp_locks);
	}
	break;
    }
    case DOP_EXIT2_TT:
    case DOP_EXIT2: {
	ErtsProcLocks rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	/* 'from' is send an exit signal to 'to' */
	if (type == DOP_EXIT2) {
	   from = tuple[2];
	   to = tuple[3];
	   reason = tuple[4];
	   token = NIL;
	} else {
	   from = tuple[2];
	   to = tuple[3];
	   token = tuple[4];
	   reason = tuple[5];
	}
	rp = erts_pid2proc_opt(NULL, 0, to, rp_locks,
			       ERTS_P2P_FLG_SMP_INC_REFC);
	if (rp) {
	    (void) erts_send_exit_signal(NULL,
					 from,
					 rp,
					 &rp_locks,
					 reason,
					 token,
					 NULL,
					 0);
	    erts_smp_proc_unlock(rp, rp_locks);
	    erts_smp_proc_dec_refc(rp);
	}
	break;
    }
    case DOP_GROUP_LEADER:
	from = tuple[2];   /* Group leader  */
	to = tuple[3];     /* new member */
	if (is_not_pid(from))
	    break;

	rp = erts_pid2proc(NULL, 0, to, ERTS_PROC_LOCK_MAIN);
	if (!rp)
	    break;
	rp->group_leader = STORE_NC_IN_PROC(rp, from);
	erts_smp_proc_unlock(rp, ERTS_PROC_LOCK_MAIN);
	break;

    default: {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Illegal value in distribution dispatch switch: "
			  "%.200T",
			  arg);
	    erts_send_error_to_logger_nogl(dsbufp);
	    PURIFY_MSG("data error");
	    goto data_error;
	}
    }

    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (off_heap.externals) {
	erts_cleanup_externals(off_heap.externals);
    }
#ifndef HYBRID /* FIND ME! */
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
#endif
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return 0;

 data_error:
    {
	int i;

	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp,
		      "Got invalid data on distribution channel, "
		      "offending packet is: <<");
	for(i = 0; i < orig_len; ++i)
	    erts_dsprintf(dsbufp, i ? ",%b8u" : "%b8u", buf[i]);
	erts_dsprintf(dsbufp, ">>");
	erts_send_warning_to_logger_nogl(dsbufp);
    }
    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (off_heap.externals) {
	erts_cleanup_externals(off_heap.externals);
    }
#ifndef HYBRID /* FIND ME! */
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
#endif
    erts_do_exit_port(prt, dep->cid, am_killed);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    return -1;
}


/* 
 * Here is a version that fills the array with two distinct 
 * encodings of two different erlang terms. This is so
 * that the receiver can look at the first message, see that it
 * a send message , and then unpack the second term directly
 * on the receiving processes heap. This removes the need for an extra
 * copying of each message from the net.
 *   
 */

/* Return -1 on error   */
/*         0 on ok      */
/*         1 on resend  */

#define DEFAULT_TMP_DIST_BUF_SZ (8*1024)

static int pack_and_send(ErtsDistOpData *dodp,
			 Eterm ctl,
			 Eterm mess,
			 int force_busy)
{
    byte *bufp;
    Uint bufsz;
    byte *t;
    Port* p;
    DistEntry *dep = dodp->dep;
    Eterm cid = dep->cid;
#ifdef ERTS_SMP
    Process *c_p;
    ErtsProcLocks c_p_locks;
#endif

    /* Caller is *not* allowed to hold status proc lock */
    ERTS_SMP_LC_ASSERT(!(dodp->lcks & ERTS_PROC_LOCK_STATUS));

    if (!is_alive())
	return -1;
    if (cid == NIL)
	return 0;
    if (dep->status & ERTS_DE_SFLG_EXITING)
	return 0; /* Ignore it */

    p = dodp->dprt;

    ASSERT(is_internal_port(cid));
    ASSERT(!INVALID_PORT(p, cid));
    ERTS_SMP_LC_ASSERT(erts_lc_is_port_locked(p));

    if (p->status & ERTS_PORT_SFLG_EXITING)
	return 0;
    if (!force_busy && (p->status & ERTS_PORT_SFLG_PORT_BUSY))
	return 1;

#ifdef MESS_DEBUG
    if (is_value(mess))
	erts_printf(stderr, ">>ctl+mess>> %T && %T\n", ctl, mess);
    else
	erts_printf(stderr, ">> %T\n", ctl);
#endif
    bufp = (byte *) erts_alloc(ERTS_ALC_T_TMP_DIST_BUF, DEFAULT_TMP_DIST_BUF_SZ);
    bufsz = DEFAULT_TMP_DIST_BUF_SZ;
    t = bufp;
    *t++ = PASS_THROUGH;          /* not needed !!! */

    erts_to_external_format(dep, ctl, &t, &bufp, &bufsz);
    if (is_value(mess))
	erts_to_external_format(dep, mess, &t, &bufp, &bufsz);

#ifdef ERTS_SMP
    /*
     * When we call dist_port_command we should only hold the port lock.
     */
    erts_smp_dist_entry_unlock(dep);
    c_p = dodp->proc;
    c_p_locks = c_p ? dodp->lcks : 0;
    if (c_p_locks)
	erts_smp_proc_unlock(c_p, c_p_locks);

    ERTS_SMP_CHK_NO_PROC_LOCKS;
#endif

    dist_port_command(p, bufp, t-bufp);
    erts_free(ERTS_ALC_T_TMP_DIST_BUF, (void *) bufp);

#ifdef ERTS_SMP
    /* Restore locks as held when pack_and_send() was called...
     * Lock order of interest:
     * - port lock
     * - process locks
     * - dist entry
     */
    if (c_p_locks)
	erts_smp_proc_lock(c_p, c_p_locks);
    erts_smp_dist_entry_lock(dep);
#endif

    ASSERT(dodp->dprt);
    ASSERT(dodp->dprt->id != dep->cid || !INVALID_PORT(dodp->dprt, dep->cid));

    return 0;
}

struct print_to_data {
    int to;
    void *arg;
};

static void doit_print_monitor_info(ErtsMonitor *mon, void *vptdp)
{
    int to = ((struct print_to_data *) vptdp)->to;
    void *arg = ((struct print_to_data *) vptdp)->arg;
    Process *rp;
    ErtsMonitor *rmon;
    rp = erts_pid2proc_unlocked(mon->pid);
    if (!rp || (rmon = erts_lookup_monitor(rp->monitors, mon->ref)) == NULL) {
	erts_print(to, arg, "Warning, stray monitor for: %T\n", mon->pid);
    } else if (mon->type == MON_ORIGIN) {
	/* Local pid is being monitored */
	erts_print(to, arg, "Remotely monitored by: %T %T\n",
		   mon->pid, rmon->pid);
    } else {
	erts_print(to, arg, "Remote monitoring: %T ", mon->pid);
	if (is_not_atom(rmon->pid))
	    erts_print(to, arg, "%T\n", rmon->pid);
	else
	    erts_print(to, arg, "{%T, %T}\n",
		       rmon->name,
		       rmon->pid); /* which in this case is the 
				      remote system name... */
    }
}    

static void print_monitor_info(int to, void *arg, ErtsMonitor *mon)
{
    struct print_to_data ptd = {to, arg};
    erts_doforall_monitors(mon,&doit_print_monitor_info,&ptd);
}

typedef struct {
    struct print_to_data *ptdp;
    Eterm from;
} PrintLinkContext;

static void doit_print_link_info2(ErtsLink *lnk, void *vpplc)
{
    PrintLinkContext *pplc = (PrintLinkContext *) vpplc;
    erts_print(pplc->ptdp->to, pplc->ptdp->arg, "Remote link: %T %T\n",
	       pplc->from, lnk->pid);
}

static void doit_print_link_info(ErtsLink *lnk, void *vptdp)
{
    if (is_internal_pid(lnk->pid) && erts_pid2proc_unlocked(lnk->pid)) {
	PrintLinkContext plc = {(struct print_to_data *) vptdp, lnk->pid};
	erts_doforall_links(ERTS_LINK_ROOT(lnk), &doit_print_link_info2, &plc);
    } 
}

static void print_link_info(int to, void *arg, ErtsLink *lnk)
{
    struct print_to_data ptd = {to, arg};
    erts_doforall_links(lnk, &doit_print_link_info, (void *) &ptd);
}

typedef struct {
    struct print_to_data ptd;
    Eterm sysname;
} PrintNodeLinkContext;
    

static void doit_print_nodelink_info(ErtsLink *lnk, void *vpcontext)
{
    PrintNodeLinkContext *pcontext = vpcontext;

    if (is_internal_pid(lnk->pid) && erts_pid2proc_unlocked(lnk->pid))
	erts_print(pcontext->ptd.to, pcontext->ptd.arg,
		   "Remote monitoring: %T %T\n", lnk->pid, pcontext->sysname);
}

static void print_nodelink_info(int to, void *arg, ErtsLink *lnk, Eterm sysname)
{
    PrintNodeLinkContext context = {{to, arg}, sysname};
    erts_doforall_links(lnk, &doit_print_nodelink_info, &context);
}


static int
info_dist_entry(int to, void *arg, DistEntry *dep, int visible, int connected)
{

  if (visible && connected) {
      erts_print(to, arg, "=visible_node:");
  } else if (connected) {
      erts_print(to, arg, "=hidden_node:");
  } else {
      erts_print(to, arg, "=not_connected:");
  }
  erts_print(to, arg, "%d\n", dist_entry_channel_no(dep));

  if(connected && is_nil(dep->cid)) {
    erts_print(to, arg,
	       "Error: Not connected node still registered as connected:%T\n",
	       dep->sysname);
    return 0;
  }

  if(!connected && is_not_nil(dep->cid)) {
    erts_print(to, arg,
	       "Error: Connected node not registered as connected:%T\n",
	       dep->sysname);
    return 0;
  }

  erts_print(to, arg, "Name: %T", dep->sysname);
#ifdef DEBUG
  erts_print(to, arg, " (refc=%d)", erts_refc_read(&dep->refc, 1));
#endif
  erts_print(to, arg, "\n");
  if (!connected && is_nil(dep->cid)) {
    if (dep->nlinks) {
      erts_print(to, arg, "Error: Got links to not connected node:%T\n",
		 dep->sysname);
    }
    return 0;
  }

  erts_print(to, arg, "Controller: %T\n", dep->cid, to);

  erts_print_node_info(to, arg, dep->sysname, NULL, NULL);
  print_monitor_info(to, arg, dep->monitors);
  print_link_info(to, arg, dep->nlinks);
  print_nodelink_info(to, arg, dep->node_links, dep->sysname);

  return 0;
    
}
int distribution_info(int to, void *arg)	/* Called by break handler */
{
    DistEntry *dep;

    erts_print(to, arg, "=node:%T\n", erts_this_dist_entry->sysname);
 
    if (erts_this_node->sysname == am_Noname) {
	erts_print(to, arg, "=no_distribution\n");
	return(0);
    }

#if 0
    if (!erts_visible_dist_entries && !erts_hidden_dist_entries) 
      erts_print(to, arg, "Alive but not holding any connections \n");
#endif

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, arg, dep, 1, 1);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, arg, dep, 0, 1);
    }

    for (dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
	info_dist_entry(to, arg, dep, 0, 0);
    }

    return(0);
}


/* Turn this on to get printouts of all distribution messages
 * which go on the line
 */

#ifdef MESS_DEBUG


void upp(byte *buf, int sz)
{
    bin_write(ERTS_PRINT_STDERR,NULL,buf,sz);
}

void print_pass_through(DistEntry *dep, byte *t, int len)
{
    ErlHeapFragment* ctl = NULL;
    ErlHeapFragment* msg = NULL;
    Eterm* hp;
    byte *orig = t;
    Uint i;
    Sint decoded_size;

    if ((decoded_size = erts_decoded_size(t, len, 0)) == -1) {
	erts_printf(stderr, "Bailing out in decode_size control message\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size control message\n");
    }
    ctl = new_message_buffer(decoded_size);
    hp = ctl->mem;
    i = erts_from_external_format(dep, &hp, &t, &ctl->mso);
    if (is_non_value(i)) {
	erts_printf(stderr, "Bailing out in erts_from_external_format control "
		    "message\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in erts_from_external_format control "
		 "message\n");
    }
    erts_printf(stderr, "GOT: %T", i);
    if (t >= (orig + len)) {
	erts_printf(stderr, "\n");
	free_message_buffer(ctl);
	return;
    }
    if ((decoded_size = erts_decoded_size(t, len, 0)) == -1) {
	erts_printf(stderr, "Bailing out in decode_size second element\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size second element\n");
    }
    msg = new_message_buffer(decoded_size);
    hp = msg->mem;
    i = erts_from_external_format(dep, &hp, &t, &msg->mso);
    if (is_non_value(i)) {
	erts_printf(stderr, "Bailing out in erts_from_external_format second "
		    "element\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in erts_from_external_format second "
		 "element\n");
    }
    erts_printf(stderr, "%T\n", i);
    free_message_buffer(msg);
    return;
}
#endif



/****************************************************************************
  DISTRIBUTION BIFS:

            setnode/2     -- start distribution
            setnode/3     -- set node controller

            node/1        -- return objects node name
            node/0        -- return this node name
            nodes/0       -- return a list of all (non hidden) nodes
            is_alive      -- return true if distribution is running else false
	    monitor_node  -- turn on/off node monitoring

            node controller only:
            dist_exit/3       -- send exit signals from remote to local process
            dist_link/2       -- link a remote process to a local
            dist_unlink/2     -- unlink a remote from a local
****************************************************************************/



/**********************************************************************
 ** Set the node name of current node fail if node already is set.
 ** setnode(name@host, Creation)
 ** loads functions pointer to trap_functions from module erlang.
 **    erlang:dsend/2
 **    erlang:dlink/1
 **    erlang:dunlink/1
 **    erlang:dmonitor_node/3
 **    erlang:dgroup_leader/2
 **    erlang:dexit/2
 **  -- are these needed ?
 **    dexit/1
 ***********************************************************************/

BIF_RETTYPE setnode_2(BIF_ALIST_2)
{
    Process *net_kernel;
    Uint creation;

    /* valid creation ? */
    if(!term_to_Uint(BIF_ARG_2, &creation))
	goto error;
    if(creation > 3)
	goto error;

    /* valid node name ? */
    if (!is_node_name_atom(BIF_ARG_1))
	goto error;

    if (BIF_ARG_1 == am_Noname) /* cant use this name !! */
	goto error;
    if (is_alive())     /* must not be alive! */
	goto error;

    /* Check that all trap functions are defined !! */
    if (dsend2_trap->address == NULL ||
	dsend3_trap->address == NULL ||
	/*	dsend_nosuspend_trap->address == NULL ||*/
	dlink_trap->address == NULL ||
	dunlink_trap->address == NULL ||
	dmonitor_node_trap->address == NULL ||
	dgroup_leader_trap->address == NULL ||
	dmonitor_p_trap->address == NULL ||
	dexit_trap->address == NULL) {
	goto error;
    }

    net_kernel = erts_whereis_process(BIF_P, ERTS_PROC_LOCK_MAIN,
				      am_net_kernel, ERTS_PROC_LOCK_MAIN, 0);
    if (!net_kernel)
	goto error;

    /* By setting dist_entry==erts_this_dist_entry and DISTRIBUTION on
       net_kernel do_net_exist will be called when net_kernel
       is terminated !! */
    (void *) ERTS_PROC_SET_DIST_ENTRY(net_kernel,
				      ERTS_PROC_LOCK_MAIN,
				      erts_this_dist_entry);
    erts_refc_inc(&erts_this_dist_entry->refc, 2);
    net_kernel->flags |= F_DISTRIBUTION;

    if (net_kernel != BIF_P)
	erts_smp_proc_unlock(net_kernel, ERTS_PROC_LOCK_MAIN);

#ifdef DEBUG
    erts_smp_mtx_lock(&erts_dist_table_mtx);
    ASSERT(!erts_visible_dist_entries && !erts_hidden_dist_entries);
    erts_smp_mtx_unlock(&erts_dist_table_mtx);
#endif

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_block_system(ERTS_BS_FLG_ALLOW_GC);
    erts_set_this_node(BIF_ARG_1, (Uint32) creation);
    set_alive();
    send_nodes_mon_msgs(NULL, am_nodeup, BIF_ARG_1, am_visible, NIL);
    erts_smp_release_system();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************
 ** Allocate a dist entry, set node name install the connection handler
 ** setnode_3({name@host, Creation}, Cid, {Type, Version, Initial, IC, OC})
 ** Type = flag field, where the flags are specified in dist.h
 ** Version = distribution version, >= 1
 ** IC = in_cookie (ignored)
 ** OC = out_cookie (ignored)
 **
 ** Note that in distribution protocols above 1, the Initial parameter
 ** is always NIL and the cookies are always the atom '', cookies are not
 ** sent in the distribution messages but are only used in 
 ** the handshake.
 **
 ***********************************************************************/

BIF_RETTYPE setnode_3(BIF_ALIST_3)
{
    Uint flags;
    unsigned long version;
    Eterm ic, oc;
    Eterm *tp;
    DistEntry *dep = NULL;
    Port *pp = NULL;

    /*
     * Check and pick out arguments
     */

    if (!is_node_name_atom(BIF_ARG_1) ||
	is_not_internal_port(BIF_ARG_2) ||
	(erts_this_node->sysname == am_Noname)) {
	goto error;
    }

    if (!is_tuple(BIF_ARG_3))
	goto error;
    tp = tuple_val(BIF_ARG_3);
    if (*tp++ != make_arityval(4))
	goto error;
    if (!is_small(*tp))
	goto error;
    flags = unsigned_val(*tp++);
    if (!is_small(*tp) || (version = unsigned_val(*tp)) == 0)
	goto error;
    ic = *(++tp);
    oc = *(++tp);
    if (!is_atom(ic) || !is_atom(oc))
	goto error;

    /* DFLAG_EXTENDED_REFERENCES is compulsory from R9 and forward */
    if (!(DFLAG_EXTENDED_REFERENCES & flags)) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "%T", BIF_P->id);
	if (BIF_P->reg)
	    erts_dsprintf(dsbufp, " (%T)", BIF_P->reg->name);
	erts_dsprintf(dsbufp,
		      " attempted to enable connection to node %T "
		      "which is not able to handle extended references.\n",
		      BIF_ARG_1);
	erts_send_error_to_logger(BIF_P->group_leader, dsbufp);
	goto error;
    }

    /*
     * Arguments seem to be in order.
     */

    /* get dist_entry */
    dep = erts_find_or_insert_dist_entry(BIF_ARG_1);
    if (dep == erts_this_dist_entry)
	goto error;
    else if (!dep)
	goto system_limit; /* Should never happen!!! */

    erts_smp_dist_entry_lock(dep);

    if (dep->status & ERTS_DE_SFLG_INITIALIZING) {
	/* This should normally never happen. If it happen,
	   yield until not initializing and decide then
	   if we should succeed or fail. */
	erts_smp_dist_entry_unlock(dep);
	erts_deref_dist_entry(dep);
	ERTS_BIF_YIELD3(bif_export[BIF_setnode_3], BIF_P,
			BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    if (dep->cid == BIF_ARG_2)
	goto already_set;

    dep->status |= ERTS_DE_SFLG_INITIALIZING;

    if (dep->cid != NIL) {
#ifdef ERTS_SMP
	if (dep->status & ERTS_DE_SFLG_EXITING) {
	    /*
	     * Current controller port is currently exiting; wait for
	     * it to finish (the port has it's lock locked during the
	     * whole exiting phase).
	     */
	    Port *epp = erts_de2port(dep, BIF_P, ERTS_PROC_LOCK_MAIN);
	    if (epp)
		erts_smp_port_unlock(epp);
	    ASSERT(!(dep->status & ERTS_DE_SFLG_EXITING));
	    ASSERT(dep->cid == NIL);
	}
	else
#endif	
	    goto error_de;
    }

    erts_smp_dist_entry_unlock(dep);
    pp = erts_id2port(BIF_ARG_2, BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_dist_entry_lock(dep);

    if (!pp || (pp->status & ERTS_PORT_SFLG_EXITING) || pp->dist_entry) {
	if (pp)
	    erts_smp_port_unlock(pp);
	goto error_de;
    }

    erts_port_status_bor_set(pp, ERTS_PORT_SFLG_DISTRIBUTION);

    pp->dist_entry = dep;

    if (!(flags & DFLAG_ATOM_CACHE) ||
	(!(flags & DFLAG_PUBLISHED) && !(flags & DFLAG_HIDDEN_ATOM_CACHE))
	/* Nodes which cannot use atom cache on non-published connections
	   doesn't send the DFLAG_HIDDEN_ATOM_CACHE flag. */) {
	delete_cache(dep);
    } else {
	create_cache(dep);
    }

    dep->version = version;
    dep->creation = 0;

    erts_set_dist_entry_connected(dep, BIF_ARG_2, flags);

    dep->status &= ~ERTS_DE_SFLG_INITIALIZING;
    erts_smp_dist_entry_unlock(dep);
    send_nodes_mon_msgs(BIF_P,
			am_nodeup,
			BIF_ARG_1,
			flags & DFLAG_PUBLISHED ? am_visible : am_hidden,
			NIL);
    erts_smp_port_unlock(pp);

    BIF_RET(am_true);

 already_set:
    erts_smp_dist_entry_unlock(dep);
    erts_deref_dist_entry(dep);
    BIF_RET(am_true);

 error_de:
    dep->status &= ~ERTS_DE_SFLG_INITIALIZING;
    erts_smp_dist_entry_unlock(dep);
    erts_deref_dist_entry(dep);
 error:
    BIF_ERROR(BIF_P, BADARG);

 system_limit:
    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
}


/**********************************************************************/
/* dist_exit(Local, Term, Remote) -> Bool */

BIF_RETTYPE dist_exit_3(BIF_ALIST_3)
{
    Eterm local;
    Eterm remote;
    DistEntry *rdep;

    local = BIF_ARG_1;
    remote = BIF_ARG_3;

    /* Check that remote is a remote process */
    if (is_not_external_pid(remote))
	goto error;

    rdep = external_dist_entry(remote);
    
    if(rdep == erts_this_dist_entry)
	goto error;

    /* Check that local is local */
    if (is_internal_pid(local)) {
	Process *lp;
	ErtsProcLocks lp_locks;
	if (BIF_P->id == local) {
	    lp_locks = ERTS_PROC_LOCKS_ALL;
	    lp = BIF_P;
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
	}
	else {
	    lp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	    lp = erts_pid2proc_opt(BIF_P, ERTS_PROC_LOCK_MAIN,
				   local, lp_locks,
				   ERTS_P2P_FLG_SMP_INC_REFC);
	    if (!lp) {
		BIF_RET(am_true); /* ignore */
	    }
	}
	
	(void) erts_send_exit_signal(BIF_P,
				     remote,
				     lp,
				     &lp_locks,
				     BIF_ARG_2,
				     NIL,
				     NULL,
				     0);
#ifdef ERTS_SMP
	if (lp == BIF_P)
	    lp_locks &= ~ERTS_PROC_LOCK_MAIN;
#endif
	erts_smp_proc_unlock(lp, lp_locks);
	if (lp != BIF_P)
	    erts_smp_proc_dec_refc(lp);
	else {
	    /*
	     * We may have exited current process and may have to take action.
	     */
	    ERTS_BIF_CHK_EXITED(BIF_P);
	    ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, ERTS_PROC_LOCK_MAIN);
	}
    }
    else if (is_external_pid(local)
	     && external_dist_entry(local) == erts_this_dist_entry) {
	BIF_RET(am_true); /* ignore */
    }
    else
	goto error;
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* node(Object) -> Node */

BIF_RETTYPE node_1(BIF_ALIST_1)
{ 
    if (is_not_node_container(BIF_ARG_1))
      BIF_ERROR(BIF_P, BADARG);
    BIF_RET(node_container_node_name(BIF_ARG_1));
}

/**********************************************************************/
/* node() -> Node */

BIF_RETTYPE node_0(BIF_ALIST_0)
{
    BIF_RET(erts_this_dist_entry->sysname);
}


/**********************************************************************/
/* nodes() -> [ Node ] */

#if 0 /* Done in erlang.erl instead. */
BIF_RETTYPE nodes_0(BIF_ALIST_0)
{
  return nodes_1(BIF_P, am_visible);
}
#endif


BIF_RETTYPE nodes_1(BIF_ALIST_1)
{
    Eterm result;
    int length;
    Eterm* hp;
    int not_connected = 0;
    int visible = 0;
    int hidden = 0;
    int this = 0;
    Uint  buf[2]; /* For one cons-cell */
    DistEntry *dep;
    Eterm arg_list = BIF_ARG_1;
#ifdef DEBUG
    Eterm* endp;
#endif
    if (is_atom(BIF_ARG_1))
      arg_list = CONS(buf, BIF_ARG_1, NIL);

    while (is_list(arg_list)) {
      switch(CAR(list_val(arg_list))) {
      case am_visible:   visible = 1;                                 break;
      case am_hidden:    hidden = 1;                                  break;
      case am_known:     visible = hidden = not_connected = this = 1; break;
      case am_this:      this = 1;                                    break;
      case am_connected: visible = hidden = 1;                        break;
      default:           BIF_ERROR(BIF_P, BADARG);                    break;
      }
      arg_list = CDR(list_val(arg_list));
    }

    if (is_not_nil(arg_list))
      BIF_ERROR(BIF_P, BADARG);

    length = 0;

    erts_smp_mtx_lock(&erts_dist_table_mtx);

    ASSERT(erts_no_of_not_connected_dist_entries >= 0);
    ASSERT(erts_no_of_hidden_dist_entries >= 0);
    ASSERT(erts_no_of_visible_dist_entries >= 0);
    if(not_connected)
      length += erts_no_of_not_connected_dist_entries;
    if(hidden)
      length += erts_no_of_hidden_dist_entries;
    if(visible)
      length += erts_no_of_visible_dist_entries;
    if(this)
      length++;

    result = NIL;

    if (length == 0) {
	erts_smp_mtx_unlock(&erts_dist_table_mtx);
	BIF_RET(result);
    }

    hp = HAlloc(BIF_P, 2*length);

#ifdef DEBUG
    endp = hp + length*2;
#endif
    if(not_connected)
      for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
	result = CONS(hp, dep->sysname, result);
	hp += 2;
      }
    if(hidden)
      for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
	result = CONS(hp, dep->sysname, result);
	hp += 2;
      }
    if(visible)
      for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
	result = CONS(hp, dep->sysname, result);
	hp += 2;
      }
    if(this) {
	result = CONS(hp, erts_this_dist_entry->sysname, result);
	hp += 2;
    }
    ASSERT(endp == hp);
    erts_smp_mtx_unlock(&erts_dist_table_mtx);
    BIF_RET(result);
}

/**********************************************************************/
/* is_alive() -> Bool */

BIF_RETTYPE is_alive_0(BIF_ALIST_0)
{
    Eterm res = is_alive() ? am_true : am_false;
    BIF_RET(res);
}

/**********************************************************************/
/* erlang:monitor_node(Node, Bool, Options) -> Bool */

BIF_RETTYPE monitor_node_3(BIF_ALIST_3)
{
    DistEntry *dep;
    ErtsLink *lnk;
    Eterm l;

    for (l = BIF_ARG_3; l != NIL && is_list(l); l = CDR(list_val(l))) {
	Eterm t = CAR(list_val(l));
	/* allow_passive_connect the only available option right now */
	if (t != am_allow_passive_connect) {
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    if (l != NIL) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_not_atom(BIF_ARG_1) ||
	((BIF_ARG_2 != am_true) && (BIF_ARG_2 != am_false)) ||
	((erts_this_node->sysname == am_Noname)
	 && (BIF_ARG_1 != erts_this_node->sysname))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((dep = erts_sysname_to_connected_dist_entry(BIF_ARG_1)) == NULL) {
	BIF_TRAP3(dmonitor_node_trap, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }
    if (dep == erts_this_dist_entry)
	goto done;

    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_LINK);
    erts_smp_dist_entry_lock(dep);

    if (BIF_ARG_2 == am_true) {
	ASSERT(dep->cid != NIL);
	lnk = erts_add_or_lookup_link(&(dep->node_links), LINK_NODE, 
				      BIF_P->id);
	++ERTS_LINK_REFC(lnk);
	lnk = erts_add_or_lookup_link(&(BIF_P->nlinks), LINK_NODE, BIF_ARG_1);
	++ERTS_LINK_REFC(lnk);
    }
    else  {
	lnk = erts_lookup_link(dep->node_links, BIF_P->id);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_REFC(lnk)) == 0) {
		erts_destroy_link(erts_remove_link(&(dep->node_links), 
						   BIF_P->id));
	    }
	}
	lnk = erts_lookup_link(BIF_P->nlinks, BIF_ARG_1);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_REFC(lnk)) == 0) {
		erts_destroy_link(erts_remove_link(&(BIF_P->nlinks), 
						   BIF_ARG_1));
	    }
	}
    }

    erts_smp_dist_entry_unlock(dep);
    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_LINK);

 done:
    erts_deref_dist_entry(dep);
    BIF_RET(am_true);
}

/* monitor_node(Node, Bool) -> Bool */

BIF_RETTYPE monitor_node_2(BIF_ALIST_2)
{
    BIF_RET(monitor_node_3(BIF_P,BIF_ARG_1,BIF_ARG_2,NIL));
}

BIF_RETTYPE net_kernel_dflag_unicode_io_1(BIF_ALIST_1)
{
    DistEntry *de;
    Uint32 f;
    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    de = pid_dist_entry(BIF_ARG_1);
    ASSERT(de != NULL);
    if (de == erts_this_dist_entry) {
	BIF_RET(am_true);
    }
    erts_smp_dist_entry_lock(de);
    f = de->flags;
    erts_smp_dist_entry_unlock(de);
    BIF_RET(((f & DFLAG_UNICODE_IO) ? am_true : am_false));
}
    
/*
 * The major part of the implementation of net_kernel:monitor_nodes/[1,2]
 * follows.
 *
 * Currently net_kernel:monitor_nodes/[1,2] calls process_flag/2 which in
 * turn calls erts_monitor_nodes(). If the process_flag() call fails (with
 * badarg), the code in net_kernel determines what type of error to return.
 * This in order to simplify the task of being backward compatible.
 */

#define ERTS_NODES_MON_OPT_TYPE_VISIBLE		(((Uint16) 1) << 0)
#define ERTS_NODES_MON_OPT_TYPE_HIDDEN		(((Uint16) 1) << 1)
#define ERTS_NODES_MON_OPT_DOWN_REASON		(((Uint16) 1) << 2)

#define ERTS_NODES_MON_OPT_TYPES \
  (ERTS_NODES_MON_OPT_TYPE_VISIBLE|ERTS_NODES_MON_OPT_TYPE_HIDDEN)

typedef struct ErtsNodesMonitor_ ErtsNodesMonitor;
struct ErtsNodesMonitor_ {
    ErtsNodesMonitor *prev;
    ErtsNodesMonitor *next;
    Process *proc;
    Uint16 opts;
    Uint16 no;
};

static erts_smp_mtx_t nodes_monitors_mtx;
static ErtsNodesMonitor *nodes_monitors;
static ErtsNodesMonitor *nodes_monitors_end;

/*
 * Nodes monitors are stored in a double linked list. 'nodes_monitors'
 * points to the beginning of the list and 'nodes_monitors_end' points
 * to the end of the list.
 *
 * There might be more than one entry per process in the list. If so,
 * they are located in sequence. The 'nodes_monitors' field of the
 * process struct refers to the first element in the sequence
 * corresponding to the process in question.
 */

static void
init_nodes_monitors(void)
{
    erts_smp_mtx_init(&nodes_monitors_mtx, "nodes_monitors");
    nodes_monitors = NULL;
    nodes_monitors_end = NULL;
}

static ERTS_INLINE Uint
nodes_mon_msg_sz(ErtsNodesMonitor *nmp, Eterm what, Eterm reason)
{
    Uint sz;
    if (!nmp->opts) {
	sz = 3;
    }
    else {
	sz = 0;

	if (nmp->opts & ERTS_NODES_MON_OPT_TYPES)
	    sz += 2 + 3;

	if (what == am_nodedown
	    && (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
	    if (is_not_immed(reason))
		sz += size_object(reason);
	    sz += 2 + 3;
	}

	sz += 4;
    }
    return sz;
}

static ERTS_INLINE void
send_nodes_mon_msg(Process *rp,
		   ErtsProcLocks *rp_locksp,
		   ErtsNodesMonitor *nmp,
		   Eterm node,
		   Eterm what,
		   Eterm type,
		   Eterm reason,
		   Uint sz)
{
    Eterm msg;
    ErlHeapFragment* bp;
    ErlOffHeap *ohp;
    Eterm *hp = erts_alloc_message_heap(sz, &bp, &ohp, rp, rp_locksp);
#ifdef DEBUG
    Eterm *hend = hp + sz;
#endif

    if (!nmp->opts) {
	msg = TUPLE2(hp, what, node);
#ifdef DEBUG
	hp += 3;
#endif
    }
    else {
	Eterm tup;
	Eterm info = NIL;

	if (nmp->opts & (ERTS_NODES_MON_OPT_TYPE_VISIBLE
			 | ERTS_NODES_MON_OPT_TYPE_HIDDEN)) {

	    tup = TUPLE2(hp, am_node_type, type);
	    hp += 3;
	    info = CONS(hp, tup, info);
	    hp += 2;
	}

	if (what == am_nodedown
	    && (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)) {
	    Eterm rsn_cpy;
	    
	    if (is_immed(reason))
		rsn_cpy = reason;
	    else {
		Eterm rsn_sz = size_object(reason);
		rsn_cpy = copy_struct(reason, rsn_sz, &hp, ohp);
	    }

	    tup = TUPLE2(hp, am_nodedown_reason, rsn_cpy);
	    hp += 3;
	    info = CONS(hp, tup, info);
	    hp += 2;
	}

	msg = TUPLE3(hp, what, node, info);
#ifdef DEBUG
	hp += 4;
#endif
    }

    ASSERT(hend == hp);
    erts_queue_message(rp, rp_locksp, bp, msg, NIL);
}

static void
send_nodes_mon_msgs(Process *c_p, Eterm what, Eterm node, Eterm type, Eterm reason)
{
    ErtsNodesMonitor *nmp;
    ErtsProcLocks rp_locks = 0; /* Init to shut up false warning */
    Process *rp = NULL;

    ASSERT(is_immed(what));
    ASSERT(is_immed(node));
    ASSERT(is_immed(type));

    ERTS_SMP_LC_ASSERT(!c_p
		       || (erts_proc_lc_my_proc_locks(c_p)
			   == ERTS_PROC_LOCK_MAIN));
    erts_smp_mtx_lock(&nodes_monitors_mtx);

    for (nmp = nodes_monitors; nmp; nmp = nmp->next) {
	int i;
	Uint16 no;
	Uint sz;

	ASSERT(nmp->proc != NULL);

	if (!nmp->opts) {
	    if (type != am_visible)
		continue;
	}
	else {
	    switch (type) {
	    case am_hidden:
		if (!(nmp->opts & ERTS_NODES_MON_OPT_TYPE_HIDDEN))
		    continue;
		break;
	    case am_visible:
		if ((nmp->opts & ERTS_NODES_MON_OPT_TYPES)
		    && !(nmp->opts & ERTS_NODES_MON_OPT_TYPE_VISIBLE))
		    continue;
		break;
	    default:
		erl_exit(ERTS_ABORT_EXIT, "Bad node type found\n");
	    }
	}

	if (rp != nmp->proc) {
	    if (rp) {
		if (rp == c_p)
		    rp_locks &= ~ERTS_PROC_LOCK_MAIN;
		erts_smp_proc_unlock(rp, rp_locks);
	    }

	    rp = nmp->proc;
	    rp_locks = 0;
	    if (rp == c_p)
		rp_locks |= ERTS_PROC_LOCK_MAIN;
	}

	ASSERT(rp);

	sz = nodes_mon_msg_sz(nmp, what, reason);

	for (i = 0, no = nmp->no; i < no; i++)
	    send_nodes_mon_msg(rp,
			       &rp_locks,
			       nmp,
			       node,
			       what,
			       type,
			       reason,
			       sz);
    }

    if (rp) {
	if (rp == c_p)
	    rp_locks &= ~ERTS_PROC_LOCK_MAIN;
	erts_smp_proc_unlock(rp, rp_locks);
    }

    erts_smp_mtx_unlock(&nodes_monitors_mtx);
}

static Eterm
insert_nodes_monitor(Process *c_p, Uint32 opts)
{
    Uint16 no = 1;
    Eterm res = am_false;
    ErtsNodesMonitor *xnmp, *nmp;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&nodes_monitors_mtx));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) & ERTS_PROC_LOCK_MAIN);

    xnmp = c_p->nodes_monitors;
    if (xnmp) {
	ASSERT(!xnmp->prev || xnmp->prev->proc != c_p);

	while (1) {
	    ASSERT(xnmp->proc == c_p);
	    if (xnmp->opts == opts)
		break;
	    if (!xnmp->next || xnmp->next->proc != c_p)
		break;
	    xnmp = xnmp->next;
	}
	ASSERT(xnmp);
	ASSERT(xnmp->proc == c_p);
	ASSERT(xnmp->opts == opts
	       || !xnmp->next
	       || xnmp->next->proc != c_p);

	if (xnmp->opts != opts)
	    goto alloc_new;
	else {
	    res = am_true;
	    no = xnmp->no++;
	    if (!xnmp->no) {
		/*
		 * 'no' wrapped; transfer all prevous monitors to new
		 * element (which will be the next element in the list)
		 * and set this to one...
		 */
		xnmp->no = 1;
		goto alloc_new;
	    }
	}
    }
    else {
    alloc_new:
	nmp = erts_alloc(ERTS_ALC_T_NODES_MON, sizeof(ErtsNodesMonitor));
	nmp->proc = c_p;
	nmp->opts = opts;
	nmp->no = no;

	if (xnmp) {
	    ASSERT(nodes_monitors);
	    ASSERT(c_p->nodes_monitors);
	    nmp->next = xnmp->next;
	    nmp->prev = xnmp;
	    xnmp->next = nmp;
	    if (nmp->next) {
		ASSERT(nodes_monitors_end != xnmp);
		ASSERT(nmp->next->prev == xnmp);
		nmp->next->prev = nmp;
	    }
	    else {
		ASSERT(nodes_monitors_end == xnmp);
		nodes_monitors_end = nmp;
	    }
	}
	else {
	    ASSERT(!c_p->nodes_monitors);
	    c_p->nodes_monitors = nmp;
	    nmp->next = NULL;
	    nmp->prev = nodes_monitors_end;
	    if (nodes_monitors_end) {
		ASSERT(nodes_monitors);
		nodes_monitors_end->next = nmp;
	    }
	    else {
		ASSERT(!nodes_monitors);
		nodes_monitors = nmp;
	    }
	    nodes_monitors_end = nmp;
	}
    }
    return res;
}

static Eterm
remove_nodes_monitors(Process *c_p, Uint32 opts, int all)
{
    Eterm res = am_false;
    ErtsNodesMonitor *nmp;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&nodes_monitors_mtx));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) & ERTS_PROC_LOCK_MAIN);

    nmp = c_p->nodes_monitors;
    ASSERT(!nmp || !nmp->prev || nmp->prev->proc != c_p);

    while (nmp && nmp->proc == c_p) {
	if (!all && nmp->opts != opts)
	    nmp = nmp->next;
	else { /* if (all || nmp->opts == opts) */
	    ErtsNodesMonitor *free_nmp;
	    res = am_true;
	    if (nmp->prev) {
		ASSERT(nodes_monitors != nmp);
		nmp->prev->next = nmp->next;
	    }
	    else {
		ASSERT(nodes_monitors == nmp);
		nodes_monitors = nmp->next;
	    }
	    if (nmp->next) {
		ASSERT(nodes_monitors_end != nmp);
		nmp->next->prev = nmp->prev;
	    }
	    else {
		ASSERT(nodes_monitors_end == nmp);
		nodes_monitors_end = nmp->prev;
	    }
	    free_nmp = nmp;
	    nmp = nmp->next;
	    if (c_p->nodes_monitors == free_nmp)
		c_p->nodes_monitors = nmp && nmp->proc == c_p ? nmp : NULL;
	    erts_free(ERTS_ALC_T_NODES_MON, free_nmp);
	}
    }
    
    ASSERT(!all || !c_p->nodes_monitors);
    return res;
}

void
erts_delete_nodes_monitors(Process *c_p, ErtsProcLocks locks)
{
#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
    if (c_p) {
	ErtsProcLocks might_unlock = locks & ~ERTS_PROC_LOCK_MAIN;
	if (might_unlock)
	    erts_proc_lc_might_unlock(c_p, might_unlock);
    }
#endif
    if (erts_smp_mtx_trylock(&nodes_monitors_mtx) == EBUSY) {
	ErtsProcLocks unlock_locks = locks & ~ERTS_PROC_LOCK_MAIN;
	if (c_p && unlock_locks)
	    erts_smp_proc_unlock(c_p, unlock_locks);
	erts_smp_mtx_lock(&nodes_monitors_mtx);
	if (c_p && unlock_locks)
	    erts_smp_proc_lock(c_p, unlock_locks);
    }
    remove_nodes_monitors(c_p, 0, 1);
    erts_smp_mtx_unlock(&nodes_monitors_mtx);
}

Eterm
erts_monitor_nodes(Process *c_p, Eterm on, Eterm olist)
{
    Eterm res;
    Eterm opts_list = olist;
    Uint16 opts = (Uint16) 0;

    ASSERT(c_p);
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);

    if (on != am_true && on != am_false)
	return THE_NON_VALUE;

    if (is_not_nil(opts_list)) {
	int all = 0, visible = 0, hidden = 0;

	while (is_list(opts_list)) {
	    Eterm *cp = list_val(opts_list);
	    Eterm opt = CAR(cp);
	    opts_list = CDR(cp);
	    if (opt == am_nodedown_reason)
		opts |= ERTS_NODES_MON_OPT_DOWN_REASON;
	    else if (is_tuple(opt)) {
		Eterm* tp = tuple_val(opt);
		if (arityval(tp[0]) != 2)
		    return THE_NON_VALUE;
		switch (tp[1]) {
		case am_node_type:
		    switch (tp[2]) {
		    case am_visible:
			if (hidden || all)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPE_VISIBLE;
			visible = 1;
			break;
		    case am_hidden:
			if (visible || all)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPE_HIDDEN;
			hidden = 1;
			break;
		    case am_all:
			if (visible || hidden)
			    return THE_NON_VALUE;
			opts |= ERTS_NODES_MON_OPT_TYPES;
			all = 1;
			break;
		    default:
			return THE_NON_VALUE;
		    }
		    break;
		default:
		    return THE_NON_VALUE;
		}
	    }
	    else {
		return THE_NON_VALUE;
	    }
	}

	if (is_not_nil(opts_list))
	    return THE_NON_VALUE;
    }

    erts_smp_mtx_lock(&nodes_monitors_mtx);

    if (on == am_true)
	res = insert_nodes_monitor(c_p, opts);
    else
	res = remove_nodes_monitors(c_p, opts, 0);

    erts_smp_mtx_unlock(&nodes_monitors_mtx);

    return res;
}

/*
 * Note, this function is only used for debuging.
 */

Eterm
erts_processes_monitoring_nodes(Process *c_p)
{
    ErtsNodesMonitor *nmp;
    Eterm res;
    Eterm *hp;
    Eterm **hpp;
    Uint sz;
    Uint *szp;
#ifdef DEBUG
    Eterm *hend;
#endif

    ASSERT(c_p);
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCK_MAIN);
    erts_smp_mtx_lock(&nodes_monitors_mtx);

    sz = 0;
    szp = &sz;
    hpp = NULL;

 bld_result:
    res = NIL;

    for (nmp = nodes_monitors_end; nmp; nmp = nmp->prev) {
	Uint16 i;
	for (i = 0; i < nmp->no; i++) {
	    Eterm olist = NIL;
	    if (nmp->opts & ERTS_NODES_MON_OPT_TYPES) {
		Eterm type;
		switch (nmp->opts & ERTS_NODES_MON_OPT_TYPES) {
		case ERTS_NODES_MON_OPT_TYPES:        type = am_all;     break;
		case ERTS_NODES_MON_OPT_TYPE_VISIBLE: type = am_visible; break;
		case ERTS_NODES_MON_OPT_TYPE_HIDDEN:  type = am_hidden;  break;
		default: erl_exit(ERTS_ABORT_EXIT, "Bad node type found\n");
		}
		olist = erts_bld_cons(hpp, szp, 
				      erts_bld_tuple(hpp, szp, 2,
						     am_node_type,
						     type),
				      olist);
	    }
	    if (nmp->opts & ERTS_NODES_MON_OPT_DOWN_REASON)
		olist = erts_bld_cons(hpp, szp, am_nodedown_reason, olist);
	    res = erts_bld_cons(hpp, szp,
				erts_bld_tuple(hpp, szp, 2,
					       nmp->proc->id,
					       olist),
				res);
	}
    }

    if (!hpp) {
	hp = HAlloc(c_p, sz);
#ifdef DEBUG
	hend = hp + sz;
#endif
	hpp = &hp;
	szp = NULL;
	goto bld_result;
    }

    ASSERT(hp == hend);

    erts_smp_mtx_unlock(&nodes_monitors_mtx);

    return res;
}
