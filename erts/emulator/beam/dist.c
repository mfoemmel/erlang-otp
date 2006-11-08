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
 * distribution of erlang messages to other computers
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
static int pack_and_send(Process*, Uint32, DistEntry*, Eterm, Eterm, int);

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
    Uint32 rp_locks = ERTS_PROC_LOCK_LINK;

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
	Uint32 rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;

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
    erts_sweep_links(lnk->root, &doit_link_net_exits_sub, (void *) &lnec);
#ifdef DEBUG
    lnk->root = NULL;
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
	Uint32 rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_MSG_SEND;
	rp = erts_pid2proc(NULL, 0, lnk->pid, rp_locks);
	if (!rp) {
	    goto done;
	}
	rlnk = erts_remove_link(&(rp->nlinks), name);
	if (rlnk != NULL) {
	    ASSERT(is_atom(rlnk->pid) && (rlnk->type == LINK_NODE));
	    erts_destroy_link(rlnk);
	}
	n = ERTS_LINK_ROOT_AS_UINT(lnk);
#ifdef ERTS_SMP
	/* Drop messages if receiver has a pending exit ... */
	if (!ERTS_PROC_PENDING_EXIT(rp))
#endif
	{
	    for (i = 0; i < n; ++i) {
		ErlHeapFragment* bp;
		ErlOffHeap *ohp;
		Eterm tup;
		Eterm *hp = erts_alloc_message_heap(3,&bp,&ohp,rp,&rp_locks);
		tup = TUPLE2(hp, am_nodedown, name);
		erts_queue_message(rp, rp_locks, bp, tup, NIL);
	    }
	}
	erts_smp_proc_unlock(rp, rp_locks);
    }
 done:
    erts_destroy_link(lnk);
}

	
/*
 * proc is currently running or exiting process.
 */
int erts_do_net_exits(DistEntry *dep)
{
    if (dep == erts_this_dist_entry) {  /* Net kernel has died (clean up!!) */
	DistEntry *tdep;
	(void) erts_smp_io_lock();
	erts_smp_mtx_lock(&erts_dist_table_mtx);

	/* KILL all port controllers */
	while(erts_visible_dist_entries || erts_hidden_dist_entries) {
	    if(erts_hidden_dist_entries)
		tdep = erts_hidden_dist_entries;
	    else
		tdep = erts_visible_dist_entries;
	    ASSERT(is_internal_port(tdep->cid)
		   && (erts_port[internal_port_index(tdep->cid)].status
		       & DISTRIBUTION)
		   && erts_port[internal_port_index(tdep->cid)].dist_entry);
	    erts_smp_mtx_unlock(&erts_dist_table_mtx);
	    /* will call do_net_exists !!! */
	    erts_do_exit_port(tdep->cid, tdep->cid, am_killed);
	    erts_smp_mtx_lock(&erts_dist_table_mtx);
	}

	erts_smp_mtx_unlock(&erts_dist_table_mtx);

	erts_smp_io_unlock();
	erts_smp_block_system(ERTS_BS_FLG_ALLOW_GC);
	erts_set_this_node(am_Noname, 0);
	set_not_alive();
	erts_smp_release_system();

    }
    else { /* recursive call via erts_do_exit_port() will end up here */
	NetExitsContext nec = {dep};
	ErtsLink *nlinks;
	ErtsLink *node_links;
	ErtsMonitor *monitors;

	ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

	erts_smp_dist_entry_lock(dep);
	monitors	= dep->monitors;
        nlinks		= dep->nlinks;
	node_links	= dep->node_links;
	dep->monitors	= NULL;
        dep->nlinks	= NULL;
	dep->node_links	= NULL;
	erts_smp_dist_entry_unlock(dep);

	erts_sweep_monitors(monitors, &doit_monitor_net_exits, (void *) &nec);
	erts_sweep_links(nlinks, &doit_link_net_exits, (void *) &nec);
	erts_sweep_links(node_links, &doit_node_link_net_exits, (void *) &nec);

	clear_dist_entry(dep);
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
    clear_cache(dep);
    erts_set_dist_entry_not_connected(dep);
    erts_smp_dist_entry_lock(dep);
    dep->nlinks = NULL;
    dep->node_links = NULL;
    dep->monitors = NULL;
    dep->status = 0;
    erts_smp_dist_entry_unlock(dep);
    return 0;
}

/*
 * SMP NOTE on dist_*() functions:
 *
 * Requirements for usage of dist_*() functions:
 *   I/O lock, lock on dep has to be held, and if c_p != NULL, at least
 *   main lock has to be held on c_p.
 *
 * Also note that lock on dep will be released and reacquired,
 * and that lock(s) on c_p may be released and reacquired.
 *
 */

/*
** Send a DOP_LINK link message
*/
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_link(Process *c_p, Uint32 c_p_locks,
	      DistEntry *dep, Eterm local, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_LINK), local, remote);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 0);
}


/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_unlink(Process *c_p, Uint32 c_p_locks,
		DistEntry *dep, Eterm local, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0], make_small(DOP_UNLINK), local, remote);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 0);
}


/* A local process that's beeing monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason},
   which is rather sad as only the ref is needed, no pid's... */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_m_exit(Process *c_p, Uint32 c_p_locks,
		DistEntry *dep, Eterm watcher, Eterm watched, 
		Eterm ref, Eterm reason)
{
    Eterm ctl;
    Eterm ctl_heap[6];

    ctl = TUPLE5(&ctl_heap[0], make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

#ifdef DEBUG
    ASSERT(!erts_lookup_monitor(dep->monitors, ref));
#endif

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 1);
}
/* We want to monitor a process (named or unnamed) on another node, we send:
   {DOP_MONITOR_P, Local pid, Remote pid or name, Ref}, which is exactly what's
   needed on the other side... */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_monitor(Process *c_p, Uint32 c_p_locks,
		 DistEntry *dep, Eterm watcher, Eterm watched, Eterm ref)
{
    Eterm ctl;
    Eterm ctl_heap[5];

    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 0);
}

/* A local process monitoring a remote one wants to stop monitoring, either 
   because of a demonitor bif call or because the local process died. We send
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref}, which is once again
   rather redundant as only the ref will be needed on the other side... */

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_demonitor(Process *c_p, Uint32 c_p_locks,
		   DistEntry *dep, Eterm watcher, Eterm watched, 
		   Eterm ref, int force)
{
    Eterm ctl;
    Eterm ctl_heap[5];

    ctl = TUPLE4(&ctl_heap[0],
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, force);
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_send(Process* sender, Uint32 sender_locks,
	      DistEntry *dep, Eterm remote, Eterm message)
{
    Eterm ctl;
    Eterm ctl_heap[5];
    Eterm token = NIL;

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
    return pack_and_send(sender, sender_locks, dep, ctl, message, 0);
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_reg_send(Process* sender, Uint32 sender_locks,
		  DistEntry *dep, Eterm remote_name, Eterm message)
{
    Eterm ctl;
    Eterm ctl_heap[6];
    Eterm token = NIL;

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
    return pack_and_send(sender, sender_locks, dep, ctl, message, 0);
}

/* local has died, deliver the exit signal to remote
** We must always send the exit message to the other node
** this implies that the driver must always be ready to queue
** data even if it has signaled that it is busy !!!
*/
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_exit_tt(Process* c_p, Uint32 c_p_locks,
		 DistEntry *dep, Eterm local, Eterm remote, 
		 Eterm reason, Eterm token)
{
    Eterm ctl;
    Eterm ctl_heap[6];
    ErtsLink *lnk, *sublnk;

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
	sublnk = erts_remove_link(&(lnk->root), remote);
	if (sublnk != NULL) {
	    erts_destroy_link(sublnk);
	}
	if (lnk->root == NULL) {
	    erts_destroy_link(erts_remove_link(&(dep->nlinks), local));
	}
    }

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_exit(Process* c_p, Uint32 c_p_locks,
	      DistEntry *dep, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl_heap[5];
    Eterm ctl = TUPLE4(&ctl_heap[0],
		       make_small(DOP_EXIT), local, remote, reason);
    ErtsLink *lnk, *sublnk;

    lnk = erts_lookup_link(dep->nlinks, local);

    if (lnk != NULL) {
	sublnk = erts_remove_link(&(lnk->root), remote);
	if (sublnk != NULL) {
	    erts_destroy_link(sublnk);
	}
	if (lnk->root == NULL) {
	    erts_destroy_link(erts_remove_link(&(dep->nlinks), local));
	}
    }

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* internal version of dist_exit2 that force send through busy port */
/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_exit2(Process* c_p, Uint32 c_p_locks,
	       DistEntry *dep, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl_heap[5];
    Eterm ctl = TUPLE4(&ctl_heap[0],
		       make_small(DOP_EXIT2), local, remote, reason);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 0);
}


/* SMP NOTE: See "SMP NOTE on dist_*() functions" above */
int dist_group_leader(Process* c_p, Uint32 c_p_locks,
		      DistEntry *dep, Eterm leader, Eterm remote)
{
    Eterm ctl_heap[4];
    Eterm ctl = TUPLE3(&ctl_heap[0],
		       make_small(DOP_GROUP_LEADER), leader, remote);

    return pack_and_send(c_p, c_p_locks, dep, ctl, THE_NON_VALUE, 0);
}

/*
** Input from distribution port.
**  Input follows the distribution protocol v4.5
**  
**   The protocol is a 4 byte header protocol
**   the DOP_DATA is stripped by driver_output
**
**   assert  hlen == 0 !!!
*/
int erts_net_message(DistEntry *dep, byte *hbuf, int hlen, byte *buf, int len)
{
    byte *t;
    int i;
    int ctl_len;
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

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    if (!is_alive())
	return 0;
    if (hlen > 0)
	goto data_error;
    if (len == 0)  /* HANDLE TICK !!! */
	return 0;
    t = buf+1;     /* Skip PASS_THROUGH */

#if defined(PURIFY)
#  define PURIFY_MSG(msg) \
    purify_printf("%s, line %d: %s", __FILE__, __LINE__, msg)
#elif defined(VALGRIND)
#  define PURIFY_MSG(msg) \
    VALGRIND_PRINTF("%s, line %d: %s", __FILE__, __LINE__, msg)
#  define PURIFY_MSG
#else
#  define PURIFY_MSG(msg)
#endif

    if (len == 1) {
	PURIFY_MSG("data error");
	goto data_error;
    }
#ifdef MESS_DEBUG
    print_pass_through(dep, t, len-1);
#endif
    if ((ctl_len = decode_size(t, len-1)) == -1) {
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

	rp = erts_pid2proc_opt(NULL, 0,
			       to, ERTS_PROC_LOCK_LINK,
			       ERTS_P2P_FLG_ALLOW_OTHER_X);
	erts_smp_dist_entry_lock(dep);
	if (!rp) {
	    /* This is tricky (we MUST force a distributed send) */
	    /* We may send it to net_kernel and let it do the job !!! */
	    dist_exit(NULL, 0, dep, to, from, am_noproc);
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
	erts_add_link(&(lnk->root), LINK_PID, from);
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
	    sublnk = erts_remove_link(&(lnk->root), from);
	    if (sublnk != NULL) {
		erts_destroy_link(sublnk);
	    }
	    if (lnk->root == NULL) {
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
	    rp = erts_whereis_process(NULL, 0, watched, ERTS_PROC_LOCK_LINK, 1);
	}
	else {
	    name = NIL;
	    rp = erts_pid2proc_opt(NULL, 0,
				   watched, ERTS_PROC_LOCK_LINK,
				   ERTS_P2P_FLG_ALLOW_OTHER_X);
	}

	erts_smp_dist_entry_lock(dep);
	
	if (!rp)
	    dist_m_exit(NULL, 0, dep, watcher, watched, ref, am_noproc);
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

	if ((i = decode_size(t, len)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}
	from = tuple[2];
	to = tuple[4];
	rp = erts_whereis_process(NULL, 0, to, ERTS_PROC_LOCKS_MSG_SEND, 0);
	if (!rp || ERTS_PROC_PENDING_EXIT(rp)) {
	    ErlHeapFragment* msg;
#ifdef ERTS_SMP
	    if (rp)
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_MSG_SEND);
#endif
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    msg = new_message_buffer(i);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
	    Uint32 locks = ERTS_PROC_LOCKS_MSG_SEND;
	    ErlHeapFragment *bp;
	    ErlOffHeap *ohp;
	    hp = erts_alloc_message_heap(i+token_size,&bp,&ohp,rp,&locks);
	    hp_end = hp + i + token_size;
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
		ASSERT(i + token_size - (hp_end - hp) == final_size);
		bp = erts_resize_message_buffer(bp, final_size, &brefs[0], 2);
		message = brefs[0];
		token = brefs[1];
	    }
	    erts_queue_message(rp, locks, bp, message, token);
	    erts_smp_proc_unlock(rp, locks);
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

	if ((i = decode_size(t, len)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}

	to = tuple[3];
	rp = erts_pid2proc(NULL, 0, to, ERTS_PROC_LOCKS_MSG_SEND);
	if (!rp || ERTS_PROC_PENDING_EXIT(rp)) {
	    ErlHeapFragment* msg;
#ifdef ERTS_SMP
	    if (rp)
		erts_smp_proc_unlock(rp, ERTS_PROC_LOCKS_MSG_SEND);
#endif
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    msg = new_message_buffer(i);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
	    Uint32 locks = ERTS_PROC_LOCKS_MSG_SEND;
	    ErlOffHeap *ohp;
	    ErlHeapFragment *bp;
	    hp = erts_alloc_message_heap(i+token_size,&bp,&ohp,rp,&locks);

	    hp_end = hp + i + token_size;
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
		ASSERT(i + token_size - (hp_end - hp) == final_size);
		bp = erts_resize_message_buffer(bp, final_size, &brefs[0], 2);
		message = brefs[0];
		token = brefs[1];
	    }
	    erts_queue_message(rp, locks, bp, message, token);
	    erts_smp_proc_unlock(rp, locks);
	}
	break;

    case DOP_MONITOR_P_EXIT: {
	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */
	   

	Eterm lhp[3];
	Eterm sysname;
	Uint32 rp_locks = ERTS_PROC_LOCKS_MSG_SEND|ERTS_PROC_LOCK_LINK;

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
	Uint32 rp_locks = ERTS_PROC_LOCK_LINK|ERTS_PROC_LOCKS_XSIG_SEND;
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
	if (is_not_internal_pid(to) && is_not_internal_port(to))
	    break;

	erts_smp_dist_entry_lock(dep);
	lnk = erts_lookup_link(dep->nlinks, to); 
	if (lnk != NULL) {
	    sublnk = erts_remove_link(&(lnk->root), from);
	    if (sublnk != NULL) {
		erts_destroy_link(sublnk);
	    }
	    if (lnk->root == NULL) {
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
	else if (is_internal_port(to)) {
	    /* Internal port */
	    int ix = internal_port_index(to);
	    if (! INVALID_PORT(erts_port+ix, to)) {
		lnk = erts_remove_link(&(erts_port[ix].nlinks), from);
		if (lnk != NULL) {
		    erts_destroy_link(lnk);
		}
	    }
	    erts_do_exit_port(to, from, reason);
	}
	break;
    }
    case DOP_EXIT2_TT:
    case DOP_EXIT2: {
	Uint32 rp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
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
	rp = erts_pid2proc(NULL, 0, to, rp_locks);
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

 data_error: {
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
    erts_do_exit_port(dep->cid, dep->cid, am_killed);
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

static int pack_and_send(Process *c_p, Uint32 c_p_locks,
			 DistEntry *dep, Eterm ctl, Eterm mess, int force_busy)
{
    byte *bufp;
    Uint bufsz;
    byte *t;
    Port* p;
    Eterm cid = dep->cid;

    if (!is_alive())
	return -1;
    if (cid == NIL)
	return 0;
    if (dep->status & D_EXITING) /* ??? */
	return 0; /* Ignore it */

    ASSERT(is_internal_port(cid));
    p = &erts_port[internal_port_index(cid)];
    if (p->status & EXITING)
	return 0;
    if (!force_busy && (p->status & PORT_BUSY))
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
     * When we call dist_port_command we should only hold the io lock.
     */
    erts_smp_dist_entry_unlock(dep);
    if (!c_p)
	c_p_locks = 0;
    if (c_p_locks)
	erts_smp_proc_unlock(c_p, c_p_locks);

    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
#endif

    dist_port_command(p, bufp, t-bufp);
    erts_free(ERTS_ALC_T_TMP_DIST_BUF, (void *) bufp);

#ifdef ERTS_SMP
    /* Restore locks as held when pack_and_send() was called...
     * Lock order of interest:
     * - io lock
     * - process locks
     * - dist entry
     */
    if (c_p_locks)
	erts_smp_proc_lock(c_p, c_p_locks);
    erts_smp_dist_entry_lock(dep);
#endif

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
	erts_doforall_links(lnk->root, &doit_print_link_info2, &plc);
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

    if ((i = decode_size(t, len)) == -1) {
	erts_printf(stderr, "Bailing out in decode_size control message\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size control message\n");
    }
    ctl = new_message_buffer(i);
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
    if ((i = decode_size(t, len)) == -1) {
	erts_printf(stderr, "Bailing out in decode_size second element\n");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size second element\n");
    }
    msg = new_message_buffer(i);
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
    net_kernel->dist_entry = erts_this_dist_entry;
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
    BIF_RETTYPE res;
    Uint flags;
    unsigned long version;
    Eterm ic, oc;
    Eterm *tp;
    DistEntry *dep = NULL;
    int ix;

    /*
     * Check and pick out arguments
     */

    erts_smp_io_safe_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

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

    if (dep->cid == BIF_ARG_2) {
	erts_deref_dist_entry(dep);
	goto done;
    }
    /* We may have a sync problem here ?? */
    if (dep->cid != NIL)
	goto error;

    ix = internal_port_index(BIF_ARG_2);
    if ((INVALID_PORT(erts_port+ix, BIF_ARG_2)) 
	|| (erts_port[ix].status & EXITING)
	|| (erts_port[ix].dist_entry != NULL))
	goto error;

    erts_port[ix].status |= DISTRIBUTION;

    erts_port[ix].dist_entry = dep;

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

 done:
    ERTS_BIF_PREP_RET(res, am_true);

 done_error:

    erts_smp_io_unlock();

    return res;

    /* Errors ... */

 error:
    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
    if (dep)
	erts_deref_dist_entry(dep);
    goto done_error;

 system_limit:
    ERTS_BIF_PREP_ERROR(res, BIF_P, SYSTEM_LIMIT);
    goto done_error;
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

    /* Check that remote is a remote process or port */
    if (is_not_external_pid(remote) && is_not_external_port(remote))
	goto error;

    rdep = external_dist_entry(remote);
    
    if(rdep == erts_this_dist_entry)
	goto error;

    /* Also check that if the caller is a distribution controller then the
       object is from that node */
    if (((BIF_P->dist_entry != NULL) && (BIF_P->dist_entry != rdep)))
	goto error;

    /* Check that local is local */
    if (is_internal_pid(local)) {
	Process *lp;
	Uint32 lp_locks;
	if (BIF_P->id == local) {
	    lp_locks = ERTS_PROC_LOCKS_ALL;
	    lp = BIF_P;
	    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCKS_ALL_MINOR);
	}
	else {
	    lp_locks = ERTS_PROC_LOCKS_XSIG_SEND;
	    lp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN, local, lp_locks);
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
	if (lp == BIF_P) {
	     /*
	      * We may have exited current process and may have to take action.
	      */
#ifdef ERTS_SMP
	     ERTS_SMP_BIF_CHK_PENDING_EXIT(BIF_P, lp_locks);
	     lp_locks &= ~ERTS_PROC_LOCK_MAIN;
#else
	     ERTS_BIF_CHK_EXITED(BIF_P);
#endif
	 }
	 erts_smp_proc_unlock(lp, lp_locks);
    }
    else if (is_internal_port(local)) {
	erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	erts_do_exit_port(local, remote, BIF_ARG_2);
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
	ERTS_BIF_CHK_EXITED(BIF_P);
    }
    else if ((is_external_pid(local) || is_external_port(local))
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
/* link(Local, Remote) -> Bool */

BIF_RETTYPE dist_link_2(BIF_ALIST_2)
{
    Eterm local;
    Eterm remote;
    Process *lp;
    DistEntry *dep = BIF_P->dist_entry;
    ErtsLink *lnk;

    erts_smp_io_safe_lock(BIF_P, ERTS_PROC_LOCK_MAIN);

    /* Must be called from distribution process */
    if (!(BIF_P->flags & F_DISTRIBUTION) || (dep == NULL))
	goto error;

    local = BIF_ARG_1;
    remote = BIF_ARG_2;

    if (is_not_pid(remote) || (pid_dist_entry(remote) != dep))
	goto error;
    if (is_pid(local)) {
	lp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
			   local, ERTS_PROC_LOCK_LINK);
	erts_smp_dist_entry_lock(dep);
	if (!lp) {
	    ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
	    dist_exit(BIF_P, ERTS_PROC_LOCK_MAIN,
		      dep, local, remote,  am_noproc);
	    erts_smp_dist_entry_unlock(dep);
	    erts_smp_io_unlock();
	    BIF_RET(am_true);
	}
    }
    else /* no ports yet */
	goto error;

    erts_smp_io_unlock();

    if (erts_add_link(&(lp->nlinks), LINK_PID, remote) < 0) {
	erts_smp_dist_entry_unlock(dep);
	BIF_RET(am_true);
    }
    lnk = erts_add_or_lookup_link(&(dep->nlinks), LINK_PID, lp->id);

    erts_add_link(&(lnk->root), LINK_PID, remote);

    erts_smp_dist_entry_unlock(dep);

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_getting_linked, remote);

    erts_smp_proc_unlock(lp, ERTS_PROC_LOCK_LINK);
    BIF_RET(am_true);

 error:
    erts_smp_io_unlock();
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* unlink(Local, Remote) -> Bool */

BIF_RETTYPE dist_unlink_2(BIF_ALIST_2)
{
    Eterm local = BIF_ARG_1;
    Eterm remote = BIF_ARG_2;
    Process *lp;
    DistEntry *dep = BIF_P->dist_entry;
    ErtsLink *lnk, *sublnk;

    /* Must be called from distribution process */
    if (!(BIF_P->flags & F_DISTRIBUTION) || (dep == NULL))
	goto error;

    /* Remote must be a process */
    if (is_not_pid(remote) || (pid_dist_entry(remote) != dep))
	goto error;

    if (is_pid(local)) {
	lp = erts_pid2proc(BIF_P, ERTS_PROC_LOCK_MAIN,
			   local, ERTS_PROC_LOCK_LINK);
	if (!lp) {
	    ERTS_SMP_ASSERT_IS_NOT_EXITING(BIF_P);
	    BIF_RET(am_true);
	}
    }
    else /* no ports yet */
	goto error;

    erts_smp_dist_entry_lock(dep);

    /* unlink and ignore errors */
    lnk = erts_lookup_link(dep->nlinks, local);

    if (lnk != NULL) {
	sublnk = erts_remove_link(&(lnk->root), remote);
	if (sublnk != NULL) {
	    erts_destroy_link(sublnk);
	}
	if (lnk->root == NULL) {
	    erts_destroy_link(erts_remove_link(&(dep->nlinks), local));
	}
    }
    lnk = erts_remove_link(&(lp->nlinks), remote);
    if (lnk != NULL) {
	erts_destroy_link(lnk);
    }
    erts_smp_dist_entry_unlock(dep);

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_unlink, remote);
    erts_smp_proc_unlock(lp, ERTS_PROC_LOCK_LINK);
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
	++ERTS_LINK_ROOT_AS_UINT(lnk);
	lnk = erts_add_or_lookup_link(&(BIF_P->nlinks), LINK_NODE, BIF_ARG_1);
	++ERTS_LINK_ROOT_AS_UINT(lnk);
    }
    else  {
	lnk = erts_lookup_link(dep->node_links, BIF_P->id);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_ROOT_AS_UINT(lnk)) == 0) {
		erts_destroy_link(erts_remove_link(&(dep->node_links), 
						   BIF_P->id));
	    }
	}
	lnk = erts_lookup_link(BIF_P->nlinks, BIF_ARG_1);
	if (lnk != NULL) {
	    if ((--ERTS_LINK_ROOT_AS_UINT(lnk)) == 0) {
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

