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

byte *dist_buf;
int dist_buf_size;

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
static Process *net_kernel;    /* we don't want to look it up */
static Eterm* dmem;

#define DMEM_SIZE (14+2+REF_THING_SIZE) /* Enough to hold any control msg */

/* forward declarations */

static int clear_dist_entry(DistEntry*);
static int pack_and_send(DistEntry*, Eterm, Eterm, int);

static Uint no_caches;

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


int is_node_name(char *ptr, int len)
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
    ASSERT((i > 0) && (i < atom_table_size) &&  (atom_tab(i) != NULL));
    return is_node_name(atom_tab(i)->name, atom_tab(i)->len);
}

/* 
** This function is called when a distribution 
** port or process terminates
*/
static void doit_monitor_net_exits(ErtsMonitor *mon, void *vdep)
{
    Process *rp;
    ErtsMonitor *rmon;
    DistEntry *dep = vdep;
    ASSERT(pid2proc(mon->pid) != NULL);
    if (mon->type == MON_ORIGIN) {
	/* local pid is beeing monitored */
	if ((rp = pid2proc(mon->pid)) == NULL) {
	    goto done;
	}
	rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	/* ASSERT(rmon != NULL); nope, can happen during process exit */
	if (rmon != NULL) {
	    erts_destroy_monitor(rmon);
	}
    } else {
	Eterm lhp[3];
	Eterm watched;
	ASSERT(mon->type == MON_TARGET);
	
	if ((rp = pid2proc(mon->pid)) == NULL)
	    goto done;
	rmon = erts_remove_monitor(&(rp->monitors),mon->ref);
	/* ASSERT(rmon != NULL); can happen during process exit */
	if (rmon != NULL) {
	    ASSERT(is_atom(rmon->name) || is_nil(rmon->name));
	    watched = (is_atom(rmon->name)
		       ? TUPLE2(lhp, rmon->name, dep->sysname)
		       : rmon->pid);
	    queue_monitor_message(rp, mon->ref, am_process, 
				  watched, am_noconnection);
	    erts_destroy_monitor(rmon);
	}
    }
 done:
    erts_destroy_monitor(mon);
}
	
typedef struct {
    DistEntry *dep;
    ErtsLink *lnk;
} LinkNetExitsContext;

/* 
** This is the function actually doing the job of sending exit messages
** for links in a dist entry upon net_exit (the node goes down), NB,
** only process links, not node monitors are handled here, 
** they reside in a separate tree....
*/
static void doit_link_net_exits_sub(ErtsLink *sublnk, void * vpcontext)
{
    LinkNetExitsContext *pcontext = vpcontext;
    ErtsLink *lnk = pcontext->lnk; /* the local pid */
    ErtsLink *rlnk;
    Process *rp;
    ASSERT(lnk->type == LINK_PID)
    if (is_internal_pid(lnk->pid)) {
	if ((rp = pid2proc(lnk->pid)) == NULL)
	    goto done;
	if (rp->flags & F_TRAPEXIT) {
	    rlnk = erts_remove_link(&(rp->nlinks), sublnk->pid);
	    if (rlnk != NULL) {
		erts_destroy_link(rlnk);
	    }
	    deliver_exit_message(sublnk->pid, rp, am_noconnection);
	    if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlnk != NULL) {
		trace_proc(NULL, rp, 
			   am_getting_unlinked, sublnk->pid);
	    }
	}
	else
	    schedule_exit(rp, am_noconnection);
    }
 done:
    erts_destroy_link(sublnk);

}
    




/* 
** This function is called when a distribution 
** port or process terminates, once for each link on the high level, 
** it in turn traverses the link subtree for the specific link node...
*/
static void doit_link_net_exits(ErtsLink *lnk, void *vpdep)
{
    LinkNetExitsContext context = {(DistEntry *) vpdep, lnk};
    ASSERT(lnk->type == LINK_PID)
    erts_sweep_links(lnk->root,&doit_link_net_exits_sub,&context);
#ifdef DEBUG
    lnk->root = NULL;
#endif
    erts_destroy_link(lnk);
}


static void doit_node_link_net_exits(ErtsLink *lnk, void *vpdep)
{
    DistEntry *dep = vpdep;
    Eterm name = dep->sysname;
    Process *rp;
    ErtsLink *rlnk;
    Uint i,n;
    ASSERT(lnk->type == LINK_NODE)
    if (is_internal_pid(lnk->pid)) {
	ErlHeapFragment* bp;
	Eterm* hp;
	Eterm tup;
	if ((rp = pid2proc(lnk->pid)) == NULL)
	    goto done;
	rlnk = erts_remove_link(&(rp->nlinks), name);
	if (rlnk != NULL) {
	    ASSERT(is_atom(rlnk->pid) && (rlnk->type == LINK_NODE));
	    erts_destroy_link(rlnk);
	}
	n = ERTS_LINK_ROOT_AS_UINT(lnk);
	for (i = 0; i < n; ++i) {
	    bp = new_message_buffer(3);
	    hp = bp->mem;
	    tup = TUPLE2(hp, am_nodedown, name);
	    queue_message_tt(rp, bp, tup, NIL);
	}
#ifdef HEAP_FRAG_ELIM_TEST
	if (SAVED_HEAP_TOP(rp) == NULL) {
	    SAVED_HEAP_TOP(rp) = HEAP_TOP(rp);
	    HEAP_TOP(rp) = HEAP_LIMIT(rp);
	}
#endif
	MSO(rp).overhead = HEAP_SIZE(rp);
	BUMP_ALL_REDS(rp);
    }
 done:
    erts_destroy_link(lnk);
}

	

int do_net_exits(DistEntry *dep)
{
    ErtsLink* lnk;

    if (dep == erts_this_dist_entry) {  /* Net kernel has died (clean up!!) */
	DistEntry *tdep;

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
	    /* will call do_net_exists !!! */
	    do_exit_port(tdep->cid, tdep->cid, am_killed);
	}

	net_kernel = NULL;
	erts_set_this_node(am_Noname, 0);
    }
    else {
	erts_sweep_monitors(dep->monitors,&doit_monitor_net_exits,dep);
	dep->monitors = NULL;

	lnk = dep->nlinks;
	dep->nlinks = NULL;

	erts_sweep_links(lnk, &doit_link_net_exits, dep);


	lnk = dep->node_links;
	dep->node_links = NULL;
	
	erts_sweep_links(lnk, &doit_node_link_net_exits, dep);

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
    net_kernel = NULL;

    no_caches = 0;

    dist_buf = tmp_buf;   /* This is the buffer we encode into */
    dist_buf_size = TMP_BUF_SIZE - 20;

    dmem = (Eterm *) erts_alloc(ERTS_ALC_T_DMSG_BLD_BUF,
				DMEM_SIZE * sizeof(Eterm));

    /* Lookup/Install all references to trap functions */
    dsend2_trap = trap_function(am_dsend,2);
    dsend3_trap = trap_function(am_dsend,3);
    /*    dsend_nosuspend_trap = trap_function(am_dsend_nosuspend,2);*/
    dlink_trap = trap_function(am_dlink,1);
    dunlink_trap = trap_function(am_dunlink,1);
    dmonitor_node_trap = trap_function(am_dmonitor_node,2);
    dgroup_leader_trap = trap_function(am_dgroup_leader,2);
    dexit_trap = trap_function(am_dexit, 2);
    dmonitor_p_trap = trap_function(am_dmonitor_p, 2);
}

static int clear_dist_entry(DistEntry *dep)
{
    clear_cache(dep);
    erts_set_dist_entry_not_connected(dep);
    dep->nlinks = NULL;
    dep->node_links = NULL;
    dep->monitors = NULL;
    dep->status = 0;
    return 0;
}

/*
** Send a DOP_LINK link message
*/
int dist_link(DistEntry *dep, Eterm local, Eterm remote)
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_LINK), local, remote);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
}


int dist_unlink(DistEntry *dep, Eterm local, Eterm remote)
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_UNLINK), local, remote);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
}


/* A local process that's beeing monitored by a remote one exits. We send:
   {DOP_MONITOR_P_EXIT, Local pid or name, Remote pid, ref, reason},
   which is rather sad as only the ref is needed, no pid's... */
int dist_m_exit(DistEntry *dep, Eterm watcher, Eterm watched, 
		Eterm ref, Eterm reason)
{
    Eterm ctl;
    Eterm *hp = dmem;
#ifdef DEBUG
    ErtsMonitor *mon;
#endif

    ctl = TUPLE5(hp, make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

#ifdef DEBUG
    mon = erts_remove_monitor(&(dep->monitors), ref);
    ASSERT(mon == NULL);
#endif

    return pack_and_send(dep, ctl, THE_NON_VALUE, 1);
}
/* We want to monitor a process (named or unnamed) on another node, we send:
   {DOP_MONITOR_P, Local pid, Remote pid or name, Ref}, which is exactly what's
   needed on the other side... */
int dist_monitor(DistEntry *dep, Eterm watcher, Eterm watched, Eterm ref)
{
    Eterm ctl;
    Eterm *hp = dmem;

    ctl = TUPLE4(hp,
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
}

/* A local process monitoring a remote one wants to stop monitoring, either 
   because of a demonitor bif call or because the local process died. We send
   {DOP_DEMONITOR_P, Local pid, Remote pid or name, ref}, which is once again
   rather redundant as only the ref will be needed on the other side... */

int dist_demonitor(DistEntry *dep, Eterm watcher, Eterm watched, 
		   Eterm ref, int force)
{
    Eterm ctl;
    Eterm *hp = dmem;

    ctl = TUPLE4(hp,
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(dep, ctl, THE_NON_VALUE, force);
}

int dist_send(Process* sender, DistEntry *dep, Eterm remote, Eterm message)
{
    Eterm ctl;
    Eterm token = NIL;

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote, sender);
    }

    if (token != NIL)
	ctl = TUPLE4(dmem,make_small(DOP_SEND_TT), am_Cookie, remote, token);
    else
	ctl = TUPLE3(dmem,make_small(DOP_SEND), am_Cookie, remote);
    return pack_and_send(dep, ctl, message, 0);
}

int dist_reg_send(Process* sender, DistEntry *dep, Eterm remote_name, Eterm message)
{
    Eterm ctl;
    Eterm token = NIL;

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote_name, sender);
    }

    if (token != NIL)
	ctl = TUPLE5(dmem,make_small(DOP_REG_SEND_TT),
		     sender->id, am_Cookie, remote_name, token);
    else
	ctl = TUPLE4(dmem,make_small(DOP_REG_SEND),
		     sender->id, am_Cookie, remote_name);
    return pack_and_send(dep, ctl, message, 0);
}

/* local has died, deliver the exit signal to remote
** We must always send the exit message to the other node
** this implies that the driver must always be ready to queue
** data even if it has signaled that it is busy !!!
*/
int dist_exit_tt(DistEntry *dep, Eterm local, Eterm remote, 
		 Eterm reason, Eterm token)
{
    Eterm ctl;
    ErtsLink *lnk, *sublnk;

    if (token != NIL) {	
	/* token should be updated by caller */
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
	ctl = TUPLE5(dmem, make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
	ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
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

    return pack_and_send(dep, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

int dist_exit(DistEntry *dep, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
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


    return pack_and_send(dep, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* internal version of dist_exit2 that force send through busy port */
int dist_exit2(DistEntry *dep, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl = TUPLE4(dmem, make_small(DOP_EXIT2), local, remote, reason);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
}


int dist_group_leader(DistEntry *dep, Eterm leader, Eterm remote)
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_GROUP_LEADER), leader, remote);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
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
int net_mess2(DistEntry *dep, byte *hbuf, int hlen, byte *buf, int len)
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
#ifndef SHARED_HEAP
    Eterm ctl_default[64];
    Eterm* ctl = ctl_default;
#endif
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
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
    off_heap.funs = NULL;
#endif
#endif
    off_heap.overhead = 0;
    off_heap.externals = NULL;

    if (net_kernel == NULL)  /* XXX check if this may trig */
	return 0;
    if (hlen > 0)
	goto data_error;
    if (len == 0)  /* HANDLE TICK !!! */
	return 0;
    t = buf+1;     /* Skip PASS_THROUGH */

#if defined(PURIFY)
#  define PURIFY_MSG(msg) \
    purify_printf("%s, line %d: %s", __FILE__, __LINE__, msg)
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
#ifdef SHARED_HEAP
    hp = erts_global_alloc(ctl_len);
#else
    if (ctl_len > sizeof(ctl_default)/sizeof(ctl_default[0])) {
	ctl = erts_alloc(ERTS_ALC_T_DCTRL_BUF, ctl_len * sizeof(Eterm));
    }
    hp = ctl;
#endif

    arg = erts_from_external_format(dep, &hp, &t, &off_heap);
    if (is_non_value(arg)) {
	PURIFY_MSG("data error");
	goto data_error;
    }
    ctl_len = t - buf;
    len -= ctl_len;

    if (is_not_tuple(arg) || 
	(tuple = tuple_val(arg), arityval(*tuple) < 1) ||
	is_not_small(tuple[1]))
    {
	cerr_pos = 0;
	erl_printf(CBUF, "Invalid distribution message: ");
	ldisplay(arg, CBUF, 200);
	send_error_to_logger(NIL);
	goto data_error;
    }

    token_size = 0;

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK:
	from = tuple[2];
	to   = tuple[3];  /* local proc to link to */

	if ((rp = pid2proc(to)) == NULL) {
	    /* This is tricky (we MUST force a distributed send) */
	    /* We may send it to net_kernel and let it do the job !!! */
	    dist_exit(dep, to, from, am_noproc);
	    break;
	}

	res = erts_add_link(&(rp->nlinks), LINK_PID, from);

	if (res < 0) {
	    /* It was already there! Lets skip the rest... */
	    break;
	}
	lnk = erts_add_or_lookup_link(&(dep->nlinks), LINK_PID, rp->id);
	erts_add_link(&(lnk->root), LINK_PID, from);

	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(NULL, rp, am_getting_linked, from);
	break;

    case DOP_UNLINK: {
	from = tuple[2];
	to = tuple[3];
	
	if ((rp = pid2proc(to)) == NULL)
	    break;

	lnk = erts_remove_link(&(rp->nlinks), from);
	if (lnk != NULL) {
	    erts_destroy_link(lnk);
	}

	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && lnk != NULL) {
	    trace_proc(NULL, rp, am_getting_unlinked, from);
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

	break;
    }
    
    case DOP_MONITOR_P: {
	/* A remote process wants to monitor us, we get:
	   {DOP_MONITOR_P, Remote pid, local pid or name, ref} */
	Eterm name = NIL;

	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

	if (is_atom(watched)) {
	    name = watched;
	    rp = whereis_process(watched);
	    if ((rp == NULL) || (rp->status == P_EXITING)) {
		dist_m_exit(dep, watcher, watched, ref, am_noproc);
		break;
	    }
	    watched = rp->id;
	} else if ((rp = pid2proc(watched)) == NULL) {
	   dist_m_exit(dep, watcher, watched, ref, am_noproc);
	   break;
	} 
	    

	erts_add_monitor(&(dep->monitors), MON_ORIGIN, ref, watched, name);
	erts_add_monitor(&(rp->monitors), MON_TARGET, ref, watcher, name);
	break;
    }

    case DOP_DEMONITOR_P:
	/* A remote node informs us that a local pid in no longer monitored
	   We get {DOP_DEMONITOR_P, Remote pid, Local pid or name, ref},
	   We need only the ref of course */

	/* watcher = tuple[2]; */
	/* watched = tuple[3]; May be an atom in case of monitor name */
	ref = tuple[4];

	mon = erts_remove_monitor(&(dep->monitors),ref);
	/* ASSERT(mon != NULL); can happen in case of broken dist message */
	if (mon == NULL) {
	    break;
	}
	watched = mon->pid;
	erts_destroy_monitor(mon);
	rp = pid2proc(watched);
	ASSERT(rp != NULL);
	if (rp == NULL) {
	   break;
	}
	mon = erts_remove_monitor(&(rp->monitors),ref);
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
	if ((rp = whereis_process(to)) == NULL) {
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    ErlHeapFragment* msg = new_message_buffer(i);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
#ifdef SHARED_HEAP
            if (rp->htop == NULL)
            {
                ASSERT(global_htop != NULL);
                rp->htop = global_htop;
                rp->heap = global_heap;
                rp->hend = global_hend;
                rp->heap_sz = global_heap_sz;
		
	        hp = HAlloc(rp, i + token_size);

                global_htop = rp->htop;
                global_heap = rp->heap;
                global_hend = rp->hend;
                global_heap_sz = rp->heap_sz;
                rp->htop = NULL;
                rp->heap = NULL;
                rp->hend = NULL;
                rp->heap_sz = 0;
            }
	    else
	        hp = HAlloc(rp, i + token_size);
#else
	    hp = HAlloc(rp, i + token_size);
	    hp_end = hp + i + token_size;
#endif
	    message = erts_from_external_format(dep, &hp, &t, &MSO(rp));
	    if (is_non_value(message)) {
		PURIFY_MSG("data error");
		goto data_error;
	    }
	    if (type == DOP_REG_SEND) {
		token = NIL;
	    } else {
		token = tuple[5];
		token = copy_struct(token, token_size, &hp, &MSO(rp));
	    }
#ifndef SHARED_HEAP
	    HRelease(rp,hp_end,hp);
#endif
	    queue_message_tt(rp, NULL, message, token);
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
	if ((rp = pid2proc(to)) == NULL) {
	    /*
	     * The receiver is invalid, but we must decode the
	     * message anyway to keep the atom cache up-to-date.
	     */
	    ErlHeapFragment* msg = new_message_buffer(i);
	    hp = msg->mem;
	    (void) erts_from_external_format(dep, &hp, &t, &msg->off_heap);
	    free_message_buffer(msg);
	} else {
#ifdef SHARED_HEAP
            if (rp->htop == NULL)
            {
                ASSERT(global_htop != NULL);
                rp->htop = global_htop;
                rp->heap = global_heap;
                rp->hend = global_hend;
                rp->heap_sz = global_heap_sz;
		
	        hp = HAlloc(rp, i + token_size);

                global_htop = rp->htop;
                global_heap = rp->heap;
                global_hend = rp->hend;
                global_heap_sz = rp->heap_sz;
                rp->htop = NULL;
                rp->heap = NULL;
                rp->hend = NULL;
                rp->heap_sz = 0;
            }
	    else {
	        hp = HAlloc(rp, i + token_size);
	    }
#else
	    hp = HAlloc(rp, i + token_size);
	    hp_end = hp + i + token_size;
#endif
	    message = erts_from_external_format(dep, &hp, &t, &MSO(rp));
	    if (is_non_value(message)) {
		PURIFY_MSG("data error");
		goto data_error;
	    }
	    if (type == DOP_SEND) {
		token = NIL;
	    } else {
		token = tuple[4];
		token = copy_struct(token, token_size, &hp, &MSO(rp));
	    }
#ifndef SHARED_HEAP
	    HRelease(rp,hp_end,hp);
#endif
	    queue_message_tt(rp, NULL, message, token);
	}
	break;

    case DOP_MONITOR_P_EXIT: {
	/* We are monitoring a process on the remote node which dies, we get
	   {DOP_MONITOR_P_EXIT, Remote pid or name, Local pid, ref, reason} */
	   

	Eterm lhp[3];

	/* watched = tuple[2]; */  /* remote proc which died */
	/* watcher = tuple[3]; */
	ref     = tuple[4];
	reason  = tuple[5];

	mon = erts_remove_monitor(&(dep->monitors), ref);
	/* ASSERT(mon != NULL); can happen when broken message */ 
	if (mon == NULL) {
	    break;
	}
	rp = pid2proc(mon->pid);
	ASSERT(rp != NULL);
	if (rp == NULL) {
	    break;
	}
	
	erts_destroy_monitor(mon);

	mon = erts_remove_monitor(&(rp->monitors),ref);
	ASSERT(mon != NULL);
	if (mon == NULL) {
	    break;
	}
	
	watched = (is_not_nil(mon->name)
		   ? TUPLE2(&lhp[0], mon->name, dep->sysname)
		   : mon->pid);
	
	queue_monitor_message(rp, ref, am_process, watched, reason);
	
	erts_destroy_monitor(mon);
	break;
    }

    case DOP_EXIT_TT:
    case DOP_EXIT:
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


	if (is_internal_pid(to)) {
	    rp = internal_pid_index(to) < erts_max_processes ? 
		process_tab[internal_pid_index(to)] : NULL;
	    if (INVALID_PID(rp, to))
		break;

	    lnk = erts_remove_link(&(rp->nlinks), from);
	    if (lnk != NULL) {
		erts_destroy_link(lnk);
	    }
#if 0
	    /* Arndt: Maybe it should never be 'kill', but it can be,
	       namely when a linked process does exit(kill). Until we know
	       whether that is incorrect and what should happen instead,
	       we leave the assertion out. */
	    ASSERT(reason != am_kill); /* should never be kill (killed) */
#endif
	    if (rp->flags & F_TRAPEXIT) {
		/* token updated by remote node */
		deliver_exit_message_tt(from, rp, reason, token);
		if (IS_TRACED_FL(rp, F_TRACE_PROCS) && lnk != NULL) {
		    trace_proc(NULL, rp, am_getting_unlinked, from);
		}
	    } else if (reason == am_normal) {
		if (IS_TRACED_FL(rp, F_TRACE_PROCS) && lnk != NULL) {
		    trace_proc(NULL, rp, am_getting_unlinked, from);
		}
	    } else {
		schedule_exit(rp, reason);
	    }
	}
	else {
	    /* Internal port */
	    int ix = internal_port_index(to);
	    if (! INVALID_PORT(erts_port+ix, to)) {
		lnk = erts_remove_link(&(erts_port[ix].nlinks), from);
		if (lnk != NULL) {
		    erts_destroy_link(lnk);
		}
	    }
	    do_exit_port(to, from, reason);
	}
	break;

    case DOP_EXIT2_TT:
    case DOP_EXIT2:
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
	if (is_internal_pid(to)) {
	    rp = internal_pid_index(to) < erts_max_processes ?
		process_tab[internal_pid_index(to)] : NULL;
	    if (INVALID_PID(rp, to))
		break;
	    if (reason == am_kill)
		schedule_exit(rp, am_killed);
	    else if (rp->flags & F_TRAPEXIT)
		/* token updated by remote node */
		deliver_exit_message_tt(from, rp, reason, token);
	    else if (reason != am_normal)
		schedule_exit(rp, reason);
	}
	break;

    case DOP_GROUP_LEADER:
	from = tuple[2];   /* Group leader  */
	to = tuple[3];     /* new member */
	if (is_not_internal_pid(to))
	    break;
	rp = internal_pid_index(to) < erts_max_processes ?
	    process_tab[internal_pid_index(to)] : NULL;
	if (INVALID_PID(rp, to))
	    break;
	/*
	 * XXX Multi-thread note: Allocating on another process's heap.
	 */
	if (is_pid(from))
	    rp->group_leader = STORE_NC_IN_PROC(rp, from);
	break;

    default:
	cerr_pos = 0;
	erl_printf(CBUF, "Illegal value in distribution dispatch switch: ");
	ldisplay(arg, CBUF, 200);
	send_error_to_logger(NIL);
	PURIFY_MSG("data error");
	goto data_error;
    }

    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (off_heap.externals) {
	erts_cleanup_externals(off_heap.externals);
    }
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
#endif
#endif
    return 0;

 data_error:
    {
	char *initial = "Got invalid data on distribution channel, "
	    "offending packet is: <<";
	char *trailer =">>";
	int inilen = strlen(initial)+strlen(trailer);
	int toprint = orig_len;

	cerr_pos = 0;
	if (toprint*4 >= (TMP_BUF_SIZE - inilen))
	    toprint = (TMP_BUF_SIZE - 1 - inilen) / 4;
	erl_printf(CBUF,"%s",initial);
	for(i = 0; i < toprint; ++i) {
	    if(i < toprint - 1) {
		erl_printf(CBUF,"%d,",(int)buf[i]);
	    } else {
		erl_printf(CBUF,"%d",(int)buf[i]);
	    }
	}
	erl_printf(CBUF,trailer);
	erts_send_warning_to_logger(NIL,tmp_buf,cerr_pos);
    }
    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (off_heap.externals) {
	erts_cleanup_externals(off_heap.externals);
    }
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	erts_free(ERTS_ALC_T_DCTRL_BUF, (void *) ctl);
    }
#endif
#endif
    do_exit_port(dep->cid, dep->cid, am_killed);
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


static int pack_and_send(DistEntry *dep, Eterm ctl, Eterm mess, int force_busy)
{
    byte *t;
    Port* p;
    Eterm cid = dep->cid;

    if (erts_this_node->sysname == am_Noname)
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
    if (is_value(mess)) {
	erl_printf(CERR,">>ctl+mess>> ");
	display(ctl,CERR);
	erl_printf(CERR," && ");
	display(mess,CERR);
	erl_printf(CERR,"\n\r");
    }
    else {
	erl_printf(CERR,">> ");
	display(ctl,CERR);
	erl_printf(CERR, "\n\r");
    }
#endif
    t = dist_buf;
    *t++ = PASS_THROUGH;          /* not needed !!! */
    erts_to_external_format(dep, ctl, &t);
    if (is_value(mess))
	erts_to_external_format(dep, mess, &t);
    dist_port_command(p, dist_buf, t-dist_buf);
    return 0;
}

static void doit_print_monitor_info(ErtsMonitor *mon, void *vciop)
{
    CIO *ciop = vciop;
    Process *rp;
    ErtsMonitor *rmon;
    if ((rp = pid2proc(mon->pid)) == NULL || 
	(rmon = erts_lookup_monitor(rp->monitors, mon->ref)) == NULL) {
	erl_printf(*ciop, "Warning, stray monitor for: ");
	display(mon->pid,*ciop);
    } else if (mon->type == MON_ORIGIN) {
	/* Local pid is being monitored */
	erl_printf(*ciop, "Remotely monitored by: ");
	display(mon->pid,*ciop);
	erl_printf (*ciop, " ");
	display(rmon->pid,*ciop);
    } else {
	erl_printf (*ciop, "Remote monitoring: ");
	display(mon->pid,*ciop);
	erl_printf (*ciop, " ");
	if (is_not_atom(rmon->pid)) {
	    display(rmon->pid, *ciop);
	} else {
	    erl_printf (*ciop,"{");
	    display(rmon->name,*ciop);
	    erl_printf (*ciop,", ");
	    display(rmon->pid,*ciop); /* which in this case is the 
					 remote system name... */
	    erl_printf (*ciop,"}");
	}
    }
    erl_printf(*ciop," \n");
}    

static void print_monitor_info(CIO to, ErtsMonitor *mon)
{
    erts_doforall_monitors(mon,&doit_print_monitor_info,&to);
}

typedef struct {
    CIO *pto;
    Eterm from;
} PrintLinkContext;

static void doit_print_link_info2(ErtsLink *lnk, void *vpplc)
{
    PrintLinkContext *pplc = vpplc;
    erl_printf(*(pplc->pto), "Remote link: ");
    display(pplc->from,*(pplc->pto));
    erl_printf (*(pplc->pto), " ");
    display(lnk->pid,*(pplc->pto));
    erl_printf(*(pplc->pto)," \n");
}

static void doit_print_link_info(ErtsLink *lnk, void *vpto)
{
    if (is_internal_pid(lnk->pid)) {
	PrintLinkContext plc = {(CIO *) vpto, lnk->pid}; 
	if (pid2proc(lnk->pid) == NULL)
	    return;
	erts_doforall_links(lnk->root, &doit_print_link_info2, &plc);
    } 
}

static void print_link_info(CIO to, ErtsLink *lnk)
{
    erts_doforall_links(lnk, &doit_print_link_info, &to);
}

typedef struct {
    CIO *pto;
    Eterm sysname;
} PrintNodeLinkContext;
    

static void doit_print_nodelink_info(ErtsLink *lnk, void *vpcontext)
{
    PrintNodeLinkContext *pcontext = vpcontext;

    if (is_internal_pid(lnk->pid)) {
	if (pid2proc(lnk->pid) == NULL)
	    return;
	erl_printf(*(pcontext->pto), "Remote monitoring: ");
	display(lnk->pid,*(pcontext->pto));
	erl_printf (*(pcontext->pto), " ");
	display(pcontext->sysname,*(pcontext->pto));
    } 
}

static void print_nodelink_info(CIO to, ErtsLink *lnk, Eterm sysname)
{
    PrintNodeLinkContext context = {&to, sysname};
    erts_doforall_links(lnk, &doit_print_nodelink_info, &context);
}


static int
info_dist_entry(CIO to, DistEntry *dep, int visible, int connected)
{

  if (visible && connected) {
      erl_printf(to, "=visible_node:");
  } else if (connected) {
      erl_printf(to, "=hidden_node:");
  } else {
      erl_printf(to, "=not_connected:");
  }
  erl_printf(to, "%d\n", dist_entry_channel_no(dep));

  if(connected && is_nil(dep->cid)) {
    erl_printf(to,"Error: Not connected node still registered as connected:");
    display(dep->sysname, to);
    erl_printf(to, "\n");
    return 0;
  }

  if(!connected && is_not_nil(dep->cid)) {
    erl_printf(to,"Error: Connected node not registered as connected:");
    display(dep->sysname, to);
    erl_printf(to, "\n");
    return 0;
  }

  erl_printf(to, "Name: ");
  display(dep->sysname, to);
#ifdef DEBUG
  erl_printf(to," (refc=%d)", dep->refc);
#endif
  erl_printf(to, "\n");
  if (!connected && is_nil(dep->cid)) {
    if (dep->nlinks) {
      erl_printf(to,"Error: Got links to not connected node:");
      display(dep->sysname, to);
      erl_printf(to, "\n");
    }
    return 0;
  }

  erl_printf(to, "Controller: ");
  display(dep->cid, to);
  erl_printf(to, "\n");

  erts_print_node_info(to, dep->sysname, NULL, NULL);
  print_monitor_info(to, dep->monitors);
  print_link_info(to, dep->nlinks);
  print_nodelink_info(to,dep->node_links, dep->sysname);

  return 0;
    
}
int distribution_info(CIO to)		/* Called by break handler */
{
    DistEntry *dep;

    erl_printf(to, "=node:");
    display(erts_this_dist_entry->sysname, to);
    erl_printf(to, "\n");

    if (erts_this_node->sysname == am_Noname) {
	erl_printf(to, "=no_distribution\n");
	return(0);
    }

#if 0
    if (!erts_visible_dist_entries && !erts_hidden_dist_entries) 
      erl_printf(to,"Alive but not holding any connections \n");
#endif

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, dep, 1, 1);
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, dep, 0, 1);
    }

    for (dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
	info_dist_entry(to, dep, 0, 0);
    }

    return(0);
}


/* Turn this on to get printouts of all distribution messages
 * which go on the line
 */

#ifdef MESS_DEBUG


void upp(byte *buf, int sz)
{
    bin_write(CERR,buf,sz);
}

void print_pass_through(DistEntry *dep, byte *t, int len)
{
    ErlHeapFragment* ctl = NULL;
    ErlHeapFragment* msg = NULL;
    Eterm* hp;
    byte *orig = t;
    Uint i;

    if ((i = decode_size(t, len)) == -1) {
	erl_printf(CERR,"Bailing out in decode_size control message\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size control message\n");
    }
    ctl = new_message_buffer(i);
    hp = ctl->mem;
    i = erts_from_external_format(dep, &hp, &t, &ctl->mso);
    if (is_non_value(i)) {
	erl_printf(CERR,"Bailing out in erts_from_external_format control message\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in erts_from_external_format control message\n");
    }
    erl_printf(CERR,"GOT: ");
    display(i, CERR);
    if (t >= (orig + len)) {
	erl_printf(CERR,"\n\r");
	free_message_buffer(ctl);
	return;
    }
    if ((i = decode_size(t, len)) == -1) {
	erl_printf(CERR,"Bailing out in decode_size second element\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size second element\n");
    }
    msg = new_message_buffer(i);
    hp = msg->mem;
    i = erts_from_external_format(dep, &hp, &t, &msg->mso);
    if (is_non_value(i)) {
	erl_printf(CERR,"Bailing out in erts_from_external_format second element\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in erts_from_external_format second element\n");
    }
    display(i, CERR);
    erl_printf(CERR, "\n\r");
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
 ** loads functions pointer to trap_functions from module (erl_net?)
 **    erlang:dsend/2
 **    erlang:dlink/1
 **    erlang:dunlink/1
 **    erlang:dmonitor_node/2
 **    erlang:dgroup_leader/2
 **    erlang:dexit/2
 **  -- are these needed ?
 **    dexit/1
 ***********************************************************************/

BIF_RETTYPE setnode_2(BIF_ALIST_2)
{
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
    if (net_kernel != NULL)     /* net_kernel must be down */
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

    if ((net_kernel = whereis_process(am_net_kernel)) == NULL)
	goto error;
    /* By setting dist_entry==erts_this_dist_entry and DISTRIBUTION on
       net_kernel do_net_exist will be called when net_kernel
       is terminated !! */
    net_kernel->dist_entry = erts_this_dist_entry;
    erts_this_dist_entry->refc++;
    net_kernel->flags |= F_DISTRIBUTION;

    ASSERT(!erts_visible_dist_entries && !erts_hidden_dist_entries);

    erts_set_this_node(BIF_ARG_1, (Uint32) creation);

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
    DistEntry *dep;
    int ix;

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
	cerr_pos = 0;
	display(BIF_P->id, CBUF);
	if (BIF_P->reg) {
	    erl_printf(CBUF, " (");
	    display(BIF_P->reg->name, CBUF);
	    erl_printf(CBUF, ")");
	}
	erl_printf(CBUF, " attempted to enable connection to node ");
	display(BIF_ARG_1, CBUF);
	erl_printf(CBUF, " which is not able to handle extended references.\n");
	send_error_to_logger(NIL);
	goto error;
    }

    /*
     * Arguments seem to be in order.
     */

    /* get dist_entry */
    if ((dep = erts_find_or_insert_dist_entry(BIF_ARG_1))
	== erts_this_dist_entry) {
	goto error;
    } else if (!dep) {
	BIF_ERROR(BIF_P,SYSTEM_LIMIT); /* Should never happen!!! */
    }

    if (dep->cid == BIF_ARG_2)
	goto done;
    /* We may have a sync problem here ?? */
    if (dep->cid != NIL)
	goto error;

    ix = internal_port_index(BIF_ARG_2);
    if ((INVALID_PORT(erts_port+ix, BIF_ARG_2)) 
	|| (erts_port[ix].status & EXITING)
	|| (erts_port[ix].dist_entry != NULL))
      goto error;

    erts_port[ix].status |= DISTRIBUTION;

    if (erts_port[ix].dist_entry) {
	DEREF_DIST_ENTRY(erts_port[ix].dist_entry);
    }

    erts_port[ix].dist_entry = dep;
    dep->refc++;

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
    BIF_RET(am_true);

error:
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/
/* dist_exit(Local, Term, Remote) -> Bool */

BIF_RETTYPE dist_exit_3(BIF_ALIST_3)
{
    Eterm local;
    Eterm remote;
    DistEntry *rdep;
    Process *lp;
    Eterm exit_value = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;

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

	if ((lp = pid2proc(local)) == NULL)
	    BIF_RET(am_true); /* ignore */

	if ((lp->flags & F_TRAPEXIT) && (BIF_ARG_2 != am_kill))
	    deliver_exit_message(remote, lp, exit_value);
	else if (BIF_ARG_2 != am_normal)
	    schedule_exit(lp, exit_value);
	
	if (BIF_P->status != P_RUNNING) {
	    BIF_P->fvalue = exit_value;
	    KILL_CATCHES(BIF_P);
	    BIF_ERROR(BIF_P, EXC_EXIT);
	}
    }
    else if (is_internal_port(local)) {
	do_exit_port(local, remote, BIF_ARG_2);
	if (BIF_P->status != P_RUNNING) {
	    BIF_P->fvalue = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;
	    KILL_CATCHES(BIF_P);
	    BIF_ERROR(BIF_P, EXC_EXIT);         
	}
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


    /* Must be called from distribution process */
    if (!(BIF_P->flags & F_DISTRIBUTION) || (dep == NULL))
	goto error;

    local = BIF_ARG_1;
    remote = BIF_ARG_2;

    if (is_not_pid(remote) || (pid_dist_entry(remote) != dep))
	goto error;
    if (is_pid(local)) {
	if ((lp = pid2proc(local)) == NULL) {
	    dist_exit(dep, local, remote,  am_noproc);
	    BIF_RET(am_true);
	}
    }
    else /* no ports yet */
	goto error;

    if (erts_add_link(&(lp->nlinks), LINK_PID, remote) < 0) {
	BIF_RET(am_true);
    }
    lnk = erts_add_or_lookup_link(&(dep->nlinks), LINK_PID, lp->id);

    erts_add_link(&(lnk->root), LINK_PID, remote);

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_getting_linked, remote);
    BIF_RET(am_true);

 error:
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
	if ((lp = pid2proc(local)) == NULL)
	    BIF_RET(am_true);
    }
    else /* no ports yet */
	goto error;

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

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_unlink, remote);
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

    if (length == 0)
      BIF_RET(result);

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

    BIF_RET(result);
}

/**********************************************************************/
/* is_alive() -> Bool */

BIF_RETTYPE is_alive_0(BIF_ALIST_0)
{
    if (erts_this_node->sysname == am_Noname)
	BIF_RET(am_false);
    BIF_RET(am_true);
}

/**********************************************************************/
/* monitor_node(Node, Bool) -> Bool */

BIF_RETTYPE monitor_node_2(BIF_ALIST_2)
{
    DistEntry *dep;
    ErtsLink *lnk;

    if (is_not_atom(BIF_ARG_1) ||
	((BIF_ARG_2 != am_true) && (BIF_ARG_2 != am_false)) ||
	((erts_this_node->sysname == am_Noname)
	 && (BIF_ARG_1 != erts_this_node->sysname))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((dep = erts_sysname_to_connected_dist_entry(BIF_ARG_1)) == NULL) {
	BIF_TRAP2(dmonitor_node_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
    }
    if (dep == erts_this_dist_entry) 
	BIF_RET(am_true);
    if (BIF_ARG_2 == am_true) {
	ASSERT(dep->cid != NIL);
	lnk = erts_add_or_lookup_link(&(dep->node_links), LINK_NODE, 
				      BIF_P->id);
	++ERTS_LINK_ROOT_AS_UINT(lnk);
	lnk = erts_add_or_lookup_link(&(BIF_P->nlinks), LINK_NODE, BIF_ARG_1);
	++ERTS_LINK_ROOT_AS_UINT(lnk);

	BIF_RET(am_true);
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

	BIF_RET(am_true);
    }
}
