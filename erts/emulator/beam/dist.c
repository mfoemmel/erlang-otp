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
Export* dsend_trap = NULL;
Export* dsend_nosuspend_trap = NULL;
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
	sys_free(cp);
	dep->cache = NULL;
    }
}


void create_cache(DistEntry *dep)
{
    ErlCache* cp;

    if ((cp = dep->cache) == NULL) {
	dep->cache = (ErlCache*) safe_alloc_from(150,sizeof(ErlCache));
	clear_cache(dep);
    }
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

int do_net_exits(DistEntry *dep)
{
    Eterm tup;
    Eterm name;
    Eterm item;
    ErlLink* lnk;
    Process *rp;
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
	lnk = dep->links;
	dep->links = NULL;

	while (lnk != NULL) {
	    item = lnk->item;
	    switch(lnk->type) {
	    case LNK_LINK:
		if (is_internal_pid(item)) {
		    if ((rp = pid2proc(item)) == NULL)
			break;
		    if (rp->flags & F_TRAPEXIT) {
			ErlLink **rlinkpp = 
			    find_link(&rp->links, LNK_LINK, lnk->data, NIL);
			del_link(rlinkpp);
			deliver_exit_message(lnk->data, rp, am_noconnection);
			if (IS_TRACED_FL(rp, F_TRACE_PROCS) 
			    && rlinkpp != NULL) {
			    trace_proc(NULL, rp, 
				       am_getting_unlinked, lnk->data);
			}
		    }
		    else
			schedule_exit(rp, am_noconnection);
		}
		break;

	    case LNK_LINK1:
		if (is_external_pid(item)
		    && external_pid_dist_entry(item) != erts_this_dist_entry) {
		   /* We are being monitored */
		   if ((rp = pid2proc(lnk->data)) == NULL)
		      break;
		   del_link(find_link_by_ref(&rp->links, lnk->ref));
		} else {
		   Eterm lhp[3];
		   Eterm watched;
		   /* We are monitoring */
		   if ((rp = pid2proc(item)) == NULL)
		      break;
		   ASSERT(is_pid(lnk->data) || is_atom(lnk->data));
		   watched = (is_atom(lnk->data)
			      ? TUPLE2(&lhp[0], lnk->data, dep->sysname)
			      : lnk->data);
		   queue_monitor_message(rp, lnk->ref, am_process, 
					 watched, am_noconnection);
		   del_link(find_link_by_ref(&rp->links, lnk->ref));
		}
		break;

	    case LNK_NODE:
		name = dep->sysname;
		if (is_internal_pid(item)) {
		    ErlHeapFragment* bp;
		    Eterm* hp;
		    if ((rp = pid2proc(item)) == NULL)
			break;
		    del_link(find_link(&rp->links,LNK_NODE,name,NIL));
		    bp = new_message_buffer(3);
		    hp = bp->mem;
		    tup = TUPLE2(hp, am_nodedown, name);
		    queue_message_tt(rp, bp, tup, NIL);
		    if (SAVED_HEAP_TOP(rp) == NULL) {
			SAVED_HEAP_TOP(rp) = HEAP_TOP(rp);
			HEAP_TOP(rp) = HEAP_LIMIT(rp);
		    }
		    MSO(rp).overhead = HEAP_SIZE(rp);
		    BUMP_ALL_REDS(rp);
		}
		break;
		
	    case LNK_OMON:
	    case LNK_TMON:
	    default:
		erl_exit(1, "bad link type in dist links\n");
	    }
	    del_link(&lnk);
	}
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

    dist_buf = tmp_buf;   /* This is the buffer we encode into */
    dist_buf_size = TMP_BUF_SIZE - 20;

    dmem = (Eterm *) erts_definite_alloc(DMEM_SIZE * sizeof(Eterm));
    if (!dmem)
	dmem = (Eterm *) safe_alloc_from(151, DMEM_SIZE * sizeof(Eterm));

    /* Lookup/Install all references to trap functions */
    dsend_trap = trap_function(am_dsend,2);
    dsend_nosuspend_trap = trap_function(am_dsend_nosuspend,2);
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
    dep->links = NULL;
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

int dist_m_exit(DistEntry *dep, Eterm watcher, Eterm watched, 
		Eterm ref, Eterm reason)
{
    Eterm ctl;
    Eterm *hp = dmem;

    ctl = TUPLE5(hp, make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

    del_link(find_link_by_ref(&(dep->links), ref));

    return pack_and_send(dep, ctl, THE_NON_VALUE, 1);
}

int dist_monitor(DistEntry *dep, Eterm watcher, Eterm watched, Eterm ref)
{
    Eterm ctl;
    Eterm *hp = dmem;

    ctl = TUPLE4(hp,
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    return pack_and_send(dep, ctl, THE_NON_VALUE, 0);
}

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

    if (token != NIL) {	
	/* token should be updated by caller */
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
	ctl = TUPLE5(dmem, make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
	ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
    }
    del_link(find_link(&(dep->links), LNK_LINK, local, remote));

    return pack_and_send(dep, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

int dist_exit(DistEntry *dep, Eterm local, Eterm remote, Eterm reason)
{
    Eterm ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
    del_link(find_link(&(dep->links), LNK_LINK, local, remote));
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
/* #define TRACE_CORRUPTION 1 */
int net_mess2(DistEntry *dep, byte *hbuf, int hlen, byte *buf, int len)
{
    ErlLink **lnkp;
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
    int type;
    Eterm token;
    Eterm token_size;
#ifdef TRACE_CORRUPTION
    int orig_len = len;
#endif
    /* Thanks to Luke Gorrie */
    off_heap.mso = NULL;
#ifndef SHARED_HEAP
    off_heap.funs = NULL;
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
	ctl = safe_alloc_from(280, ctl_len * sizeof(Eterm));
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
	if (find_link(&rp->links,LNK_LINK, from, NIL) != NULL)
	    break;
	dep->links = new_link(dep->links,LNK_LINK, to, from);
	rp->links = new_link(rp->links, LNK_LINK, from, NIL);
	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(NULL, rp, am_getting_linked, from);
	break;

    case DOP_UNLINK: {
	ErlLink **rlinkpp;
	from = tuple[2];
	to = tuple[3];
	
	if ((rp = pid2proc(to)) == NULL)
	    break;
	rlinkpp = find_link(&rp->links, LNK_LINK, from, NIL);
	del_link(rlinkpp);
	del_link(find_link(&dep->links, LNK_LINK, to, from));
	
	if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
	    trace_proc(NULL, rp, am_getting_unlinked, from);
	}
	break;
    }
    
    case DOP_MONITOR_P: {
	Eterm watched_p;

	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

	watched_p = watched;

	if (is_atom(watched)) {
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
	dep->links = new_ref_link(dep->links, LNK_LINK1, watcher, watched, ref);
	rp->links = new_ref_link(rp->links, LNK_LINK1, watcher, watched_p, ref);
	break;
    }

    case DOP_DEMONITOR_P:
	/* watcher = tuple[2]; */
	/* watched = tuple[3]; May be an atom in case of monitor name */
	ref = tuple[4];

	lnkp = find_link_by_ref(&dep->links, ref);
	if (lnkp == NULL)
	    break;
	watched = (*lnkp)->data;
	if ((rp = pid2proc(watched)) == NULL)
	   break;
	del_link(lnkp);
	del_link(find_link_by_ref(&rp->links, ref));
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
	    hp = HAlloc(rp, i + token_size);
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
	    hp = HAlloc(rp, i + token_size);
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
	    queue_message_tt(rp, NULL, message, token);

	}
	break;

    case DOP_MONITOR_P_EXIT: {
	Eterm lhp[3];

	/* watched = tuple[2]; */  /* remote proc which died */
	watcher = tuple[3];
	ref     = tuple[4];
	reason  = tuple[5];

	if ((rp = pid2proc(watcher)) == NULL)
	   break;

	lnkp = find_link_by_ref(&rp->links, ref);
	if (lnkp == NULL)
	    break;
	
	del_link(lnkp);

	lnkp = find_link_by_ref(&dep->links, ref);
	
	ASSERT(is_pid((*lnkp)->data) || is_atom((*lnkp)->data));
	watched = (is_atom((*lnkp)->data)
		   ? TUPLE2(&lhp[0], (*lnkp)->data, dep->sysname)
		   : (*lnkp)->data);
	
	queue_monitor_message(rp, ref, am_process, watched, reason);
	del_link(lnkp);
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
	del_link(find_link(&dep->links, LNK_LINK, to, from));

	if (is_internal_pid(to)) {
	    ErlLink **rlinkpp;
	    rp = internal_pid_index(to) < erts_max_processes ? 
		process_tab[internal_pid_index(to)] : NULL;
	    if (INVALID_PID(rp, to))
		break;
	    rlinkpp = find_link(&rp->links, LNK_LINK, from, NIL);
	    del_link(rlinkpp);
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
		if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
		    trace_proc(NULL, rp, am_getting_unlinked, from);
		}
	    } else if (reason == am_normal) {
		if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
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
		del_link(find_link(&erts_port[ix].links,LNK_LINK,from,NIL));
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
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	sys_free(ctl);
    }
#endif
    return 0;

 data_error:
#ifdef TRACE_CORRUPTION
#warning TRACE_CORRUPTION is enabled, this is a custom build emulator
    if (dep != NULL) { /*Always?*/
	int z;
	cerr_pos = 0;
	erl_printf(CBUF, "Illegal data on distribution port: \n<<");
	for(z=0; z < orig_len && z < 1024; ++z) {
	    erl_printf(CBUF, "%d,", buf[z]);
	}
	erl_printf(CBUF, ">>\nnodename:[");
	display(dep->sysname,CBUF);
	erl_printf(CBUF,"]\nrefc %d, creation %d, cid ",
		   (int) dep->refc, (int) dep->creation);
	display(dep->cid,CBUF);
	erl_printf(CBUF,", status %d, flags %d,\ncache 0x%08x, version %d\n",
		   (int) dep->status, (int) dep->flags, (int) dep->cache,
		   (int) dep->version);
	send_error_to_logger(NIL);
    }
#endif
    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (off_heap.externals) {
	erts_cleanup_externals(off_heap.externals);
    }
#ifndef SHARED_HEAP
    if (off_heap.funs) {
	erts_cleanup_funs(off_heap.funs);
    }
    if (ctl != ctl_default) {
	sys_free(ctl);
    }
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

static int
info_dist_entry(CIO to, DistEntry *dep, int connected)
{
  ErlLink* lnk;

  if(connected && is_nil(dep->cid)) {
    erl_printf(to,"Error: Not connected node still registered as connected:");
    display(dep->sysname, to);
    return 0;
  }

  if(!connected && is_not_nil(dep->cid)) {
    erl_printf(to,"Error: Connected node not registered as connected:");
    display(dep->sysname, to);
    return 0;
  }

  if (!connected && is_nil(dep->cid)) {
    erl_printf(to, "%d : ", dist_entry_channel_no(dep));
    display(dep->sysname, to);
    erl_printf(to, "\n");
#ifdef DEBUG
  erl_printf(to,"\nReference count: %d\n", dep->refc);
#endif
    if (dep->links) {
      erl_printf(to,"Error: Got links to not connected node:");
      display(dep->sysname, to);
    }
    return 0;
  }
  erl_printf(to, "%d : Connection to:", dist_entry_channel_no(dep));
  display(dep->sysname, to);
  erl_printf(to, " Controller:");
  display(dep->cid, to);

#ifdef DEBUG
  erl_printf(to,"Reference count: %d\n", dep->refc);
#endif
  erl_printf(to, "\n");

  erts_print_node_info(to, dep->sysname, NULL, NULL);


  if ((lnk = dep->links)) {
    erl_printf(to,"\n");
    erl_printf(to,"Remote links and monitors to/from ");
    display(dep->sysname,to);
    erl_printf(to,":\n");
    while(lnk) {
      switch(lnk->type) {
      case LNK_LINK:
	if (is_internal_pid(lnk->item)) {
	  if (pid2proc(lnk->item) == NULL)
	    break;
	  display(lnk->item,to);
	  erl_printf (to," is linked to ");
	  display(lnk->data,to);
	}
	break;
      case LNK_LINK1:
	if (is_external_pid(lnk->item)
	    && external_pid_dist_entry(lnk->item) != erts_this_dist_entry) {
	  /* We are being monitored */
	  if (pid2proc(lnk->data) == NULL)
	    break;
	  display(lnk->data,to);
	  erl_printf (to," is being monitored by ");
	  display(lnk->item,to);
	} else {
	  /* We are monitoring */
	  if (pid2proc(lnk->item) == NULL)
	    break;
	  display(lnk->item,to);
	  erl_printf (to," is monitoring ");
	  if(is_atom(lnk->data)) {
	    erl_printf (to,"{");
	    display(lnk->data,to);
	    erl_printf (to,", ");
	    display(dep->sysname,to);
	    erl_printf (to,"}");
	  }
	  else
	    display(lnk->data,to);

	}
	break;
      case LNK_NODE:
	if (is_internal_pid(lnk->item)) {
	  if (pid2proc(lnk->item) == NULL)
	    break;
	  display(lnk->item,to);
	  erl_printf (to," is monitoring ");
	  display(dep->sysname,to);
	}
	break;
      case LNK_OMON:
      case LNK_TMON:
      default:
	erl_printf (to,"Bad remote link type (%d) found", lnk->type);
      }
      erl_printf(to," \n");
      lnk = lnk->next;
    }
  }
  else
      erl_printf(to,"\n");

  return 0;
    
}
int distribution_info(CIO to)		/* Called by break handler */
{
    DistEntry *dep;

    if (erts_this_node->sysname == am_Noname) {
	erl_printf(to,"Not alive\n"); 
	return(0);
    }
#if 0
    if (!erts_visible_dist_entries && !erts_hidden_dist_entries) 
      erl_printf(to,"Alive but not holding any connections \n");
#endif
    if (erts_visible_dist_entries) {
      erl_printf(to,"------------------------\n");
      erl_printf(to,"-- Visible nodes -------\n");
      erl_printf(to,"------------------------\n");
    }

    for(dep = erts_visible_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, dep, 1);
      if (dep->next)
	erl_printf(to,"------------------------\n");
    }

    if (erts_hidden_dist_entries) {
      erl_printf(to,"------------------------\n");
      erl_printf(to,"-- Hidden nodes --------\n");
      erl_printf(to,"------------------------\n");
    }

    for(dep = erts_hidden_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, dep, 1);
      if (dep->next)
	erl_printf(to,"------------------------\n");
    }

    if (erts_not_connected_dist_entries) {
      erl_printf(to,"------------------------\n");
      erl_printf(to,"-- Not connected -------\n");
      erl_printf(to,"------------------------\n");
    }
    else
      erl_printf(to,"------------------------\n");

    for(dep = erts_not_connected_dist_entries; dep; dep = dep->next) {
      info_dist_entry(to, dep, 0);
      erl_printf(to,"------------------------\n");
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
BIF_ADECL_2
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
    if (dsend_trap->address == NULL ||
	dsend_nosuspend_trap->address == NULL ||
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
BIF_ADECL_3
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
BIF_ADECL_3
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
	    BIF_ERROR(BIF_P, USER_EXIT);
	}
    }
    else if (is_internal_port(local)) {
	do_exit_port(local, remote, BIF_ARG_2);
	if (BIF_P->status != P_RUNNING) {
	    BIF_P->fvalue = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;
	    KILL_CATCHES(BIF_P);
	    BIF_ERROR(BIF_P, USER_EXIT);         
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
BIF_ADECL_2
{
    Eterm local;
    Eterm remote;
    Process *lp;
    DistEntry *dep = BIF_P->dist_entry;

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

    if (find_link(&lp->links, LNK_LINK, remote,NIL) != NULL)
        BIF_RET(am_true);

    dep->links = new_link(dep->links,LNK_LINK, local, remote);
    lp->links = new_link(lp->links,LNK_LINK,remote,NIL);

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_getting_linked, remote);
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* unlink(Local, Remote) -> Bool */

BIF_RETTYPE dist_unlink_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm local = BIF_ARG_1;
    Eterm remote = BIF_ARG_2;
    Process *lp;
    DistEntry *dep = BIF_P->dist_entry;

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
    del_link(find_link(&lp->links, LNK_LINK, remote, NIL));
    del_link(find_link(&dep->links, LNK_LINK, local, remote));
    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(BIF_P, lp, am_unlink, remote);
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/
/* node(Object) -> Node */

BIF_RETTYPE node_1(BIF_ALIST_1)
BIF_ADECL_1
{ 
    if (is_not_node_container(BIF_ARG_1))
      BIF_ERROR(BIF_P, BADARG);
    BIF_RET(node_container_node_name(BIF_ARG_1));
}

/**********************************************************************/
/* node() -> Node */

BIF_RETTYPE node_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RET(erts_this_dist_entry->sysname);
}


/**********************************************************************/
/* nodes() -> [ Node ] */

#if 0 /* Done in erlang.erl instead. */
BIF_RETTYPE nodes_0(BIF_ALIST_0)
BIF_ADECL_0
{
  return nodes_1(BIF_P, am_visible);
}
#endif


BIF_RETTYPE nodes_1(BIF_ALIST_1)
BIF_ADECL_1
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
BIF_ADECL_0
{
    if (erts_this_node->sysname == am_Noname)
	BIF_RET(am_false);
    BIF_RET(am_true);
}

/**********************************************************************/
/* monitor_node(Node, Bool) -> Bool */

BIF_RETTYPE monitor_node_2(BIF_ALIST_2)
BIF_ADECL_2
{
    DistEntry *dep;

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
	dep->links = new_link(dep->links, LNK_NODE, BIF_P->id, NIL);

	BIF_P->links = new_link(BIF_P->links, LNK_NODE, BIF_ARG_1, NIL);
	BIF_RET(am_true);
    }
    else  {
	del_link(find_link(&(dep->links), LNK_NODE, BIF_P->id,NIL));
	del_link(find_link(&(BIF_P->links), LNK_NODE, BIF_ARG_1, NIL));
	BIF_RET(am_true);
    }
}
