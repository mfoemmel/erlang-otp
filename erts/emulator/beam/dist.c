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
#define DEFAULT_COOKIE_SIZE 20

byte *dist_buf;
int dist_buf_size;

/* distribution trap functions */
Export* dsend_trap = NULL;
Export* dlink_trap = NULL;
Export* dunlink_trap = NULL;
Export* dmonitor_node_trap = NULL;
Export* dgroup_leader_trap = NULL;
Export* dexit_trap = NULL;
#ifdef MONITOR_ENHANCE
Export* dmonitor_node2_trap = NULL;
#endif
Export* dmonitor_p_trap = NULL;

DistEntry* dist_addrs;
int        this_creation;      /* set by setnode/2 */
uint32     this_node;          /* mysyst@myhost    */
int        MAXDIST;


/* local variables */
static Process *net_kernel;    /* we don't want to look it up */
static Eterm* dmem;

#define DMEM_SIZE	(14+2+REF_WORDS) /* Enough to hold any control msg */

/* forward declarations */

static FUNCTION(int, clear_dist_entry, (int));
static int pack_and_send(int, Eterm, Eterm, int);

void clear_cache(slot)
int slot;
{
    ErlCache* cp;

    if ((cp = dist_addrs[slot].cache) != NULL) {
	int i;
	for(i = 0; i < sizeof(cp->in_arr)/sizeof(cp->in_arr[0]); ++i) {
	    cp->in_arr[i] = THE_NON_VALUE;
	    cp->out_arr[i] = THE_NON_VALUE;
	}
    }
}

void delete_cache(slot)
int slot;
{
    ErlCache* cp;

    if ((cp = dist_addrs[slot].cache) != NULL) {
	sys_free(cp);
	dist_addrs[slot].cache = NULL;
    }
}


void create_cache(slot)
int slot;
{
    ErlCache* cp;

    if ((cp = dist_addrs[slot].cache) == NULL) {
	dist_addrs[slot].cache = (ErlCache*) safe_alloc_from(150,sizeof(ErlCache));
	clear_cache(slot);
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

int is_node_name(ptr, len)
char* ptr; int len;
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

/* 
** This function is called when a distribution 
** port or process terminates
*/

int do_net_exits(slot)
int slot;
{
    uint32 tup;
    uint32 name;
    uint32 item;
    int i;
    ErlLink* lnk;
    Process *rp;
    DistEntry* dp = &dist_addrs[slot];

    if (slot == 0) {  /* Net kernel has died (clean up!!) */
	/* KILL all port controllers */
	for (i = 1; i < MAXDIST; i++) {
	    if (is_port(dist_addrs[i].cid)) {
		/* will call do_net_exists !!! */
		do_exit_port(dist_addrs[i].cid,
			     dist_addrs[i].cid, am_killed);
	    }
	    else if (is_pid(dist_addrs[i].cid)) {
		Process* proc = pid2proc(dist_addrs[i].cid);
		if (proc != NULL) 
		    do_exit(proc, am_killed);
	    }
	    /* We don't set 'sysname' to NIL, so node(P) of a distributed
	       pid still works as expected. */
	    dist_addrs[i].cid = NIL;
	    dist_addrs[i].links = NULL;
	    dist_addrs[i].status = 0;
	    dist_addrs[i].in_cookie = NIL;
	    dist_addrs[i].out_cookie = NIL;
	}
	net_kernel = NULL;
	this_node = am_Noname;
	this_creation = 0;
	dist_addrs[THIS_NODE].status = D_RESERVED;
	dist_addrs[THIS_NODE].sysname = this_node;
	dist_addrs[THIS_NODE].in_cookie = NIL;
	dist_addrs[THIS_NODE].out_cookie = NIL;
    }
    else {
	lnk = dp->links;
	dp->links = NULL;

	while (lnk != NULL) {
	    item = lnk->item;
	    switch(lnk->type) {
	    case LNK_LINK:
		if (is_pid(item)) {
		    if ((rp = pid2proc(item)) == NULL)
			break;
		    if (rp->flags & F_TRAPEXIT) {
			del_link(find_link(&rp->links,LNK_LINK,
					   lnk->data,NIL));
			deliver_exit_message(lnk->data, rp,
					     am_noconnection);
		    }
		    else
			schedule_exit(rp, am_noconnection);
		}
		break;

	    case LNK_LINK1:
		if (pid_node(item) != THIS_NODE) {
		   /* We are being monitored */
		   if ((rp = pid2proc(lnk->data)) == NULL)
		      break;
		   del_link(find_link_by_ref(&rp->links, &lnk->ref));
		} else {
		   /* We are monitoring */
		   if ((rp = pid2proc(item)) == NULL)
		      break;
		   queue_monitor_message(rp, &lnk->ref, am_process, 
					 lnk->data, am_noconnection);
		   del_link(find_link_by_ref(&rp->links, &lnk->ref));
		}
		break;

	    case LNK_NODE:
		name = dp->sysname;
		if (is_pid(item)) {
		    ErlHeapFragment* bp;
		    uint32* hp;
		    if ((rp = pid2proc(item)) == NULL)
			break;
		    del_link(find_link(&rp->links,LNK_NODE,name,slot));
		    bp = new_message_buffer(3);
		    hp = bp->mem;
		    tup = TUPLE2(hp, am_nodedown, name);
		    queue_message(rp, bp, tup);
		}
		break;
		
#ifdef MONITOR_ENHANCE
	    case LNK_NODE1:
		{
		   uint32 ref;

		   ref = lnk->ref;
		   rp = pid2proc(item);
#if 0
		   /* Unnecessary test? */
		   if (rp == NULL)
		      break;
#endif
		   queue_monitor_message(rp, ref, am_node, dp->sysname, ...);
		   del_link(find_link_by_ref(&rp->links,ref));
		}
		break;
#endif
		
	    case LNK_OMON:
	    case LNK_TMON:
	    default:
		erl_exit(1, "bad link type in dist links\n");
	    }
	    del_link(&lnk);
	}
	clear_dist_entry(slot);
    }
    return 1;
}

static Export*
trap_function(Eterm func, int arity)
{
    return erts_export_put(am_erlang, func, arity);
}

void init_dist()
{
    int i;

    net_kernel = NULL;

    dist_buf = tmp_buf;   /* This is the buffer we encode into */
    dist_buf_size = TMP_BUF_SIZE - 20;

    i = sys_max_files() * 2;  
    if (i > MAX_NODE)
	MAXDIST = MAX_NODE;
    else 
	MAXDIST = i;

    dmem = (uint32*) safe_alloc_from(151, DMEM_SIZE * sizeof(uint32));
    dist_addrs = (DistEntry *) safe_alloc_from(152,
					       MAXDIST * sizeof(DistEntry));

    for (i = 0; i < MAXDIST; i++) {
	dist_addrs[i].sysname = NIL;
	dist_addrs[i].cid = NIL;
	dist_addrs[i].links = NULL;
	dist_addrs[i].status = 0;
	dist_addrs[i].in_cookie = NIL;
	dist_addrs[i].out_cookie = NIL;
	dist_addrs[i].cache = NULL;
    }
    this_node = am_Noname;
    dist_addrs[0].status = D_RESERVED;    /* make sure no one clear this */
    dist_addrs[0].sysname = this_node;    /* this node */

    /* Lookup/Install all references to trap functions */
    dsend_trap = trap_function(am_dsend,2);
    dlink_trap = trap_function(am_dlink,1);
    dunlink_trap = trap_function(am_dunlink,1);
    dmonitor_node_trap = trap_function(am_dmonitor_node,2);
    dgroup_leader_trap = trap_function(am_dgroup_leader,2);
    dexit_trap = trap_function(am_dexit, 2);
#ifdef MONITOR_ENHANCE
    dmonitor_node2_trap = trap_function(am_dmonitor_node2, 2);
#endif
    dmonitor_p_trap = trap_function(am_dmonitor_p, 2);
    this_creation = 0;
}

static int clear_dist_entry(slot)
int slot;
{
    clear_cache(slot);
    dist_addrs[slot].cid = NIL;
    dist_addrs[slot].links = NULL;
    dist_addrs[slot].status = 0;
    /* In 4.5 distribution we do not clear cookies.
     * In next version the cookies should be cleared.
     */
    dist_addrs[slot].out_cookie = dist_addrs[THIS_NODE].out_cookie; 
    dist_addrs[slot].in_cookie = dist_addrs[THIS_NODE].in_cookie;
     /*
     */
    return 0;
}

/*
** Send a DOP_LINK link message
*/
int dist_link(slot, local, remote)
int slot; uint32 remote; uint32 local;
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_LINK), local, remote);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 0);
}


int dist_unlink(slot, local, remote)
int slot; uint32 remote; uint32 local;
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_UNLINK), local, remote);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 0);
}

int dist_m_exit(slot, watcher, watched, ref0, reason)
int slot; uint32 watcher; uint32 watched; Ref *ref0; uint32 reason;
{
    uint32 ref, ref1;
    uint32 rsize;
    uint32 ctl;
    uint32 *hp = dmem;

    ref1 = make_ref(ref0);
    rsize = size_object(ref1);

    ref = copy_struct(ref1, rsize, &hp, NULL);
    ctl = TUPLE5(hp, make_small(DOP_MONITOR_P_EXIT),
		 watched, watcher, ref, reason);

    del_link(find_link_by_ref(&dist_addrs[slot].links, ref_ptr(ref)));

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 1);
}

int dist_monitor(slot, watcher, watched, ref0)
int slot; uint32 watcher; uint32 watched; Ref *ref0;
{
    uint32 ref, ref1;
    uint32 rsize;
    uint32 ctl;
    uint32 *hp = dmem;

    ref1 = make_ref(ref0);
    rsize = size_object(ref1);

    ref = copy_struct(ref1, rsize,
		      &hp, NULL);
    ctl = TUPLE4(hp,
		 make_small(DOP_MONITOR_P),
		 watcher, watched, ref);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 0);
}

int dist_demonitor(slot, watcher, watched, ref0, force)
int slot; uint32 watcher; uint32 watched; Ref *ref0; int force;
{
    uint32 ref, ref1;
    uint32 rsize;
    uint32 ctl;
    uint32 *hp = dmem;

    ref1 = make_ref(ref0);
    rsize = size_object(ref1);

    ref = copy_struct(ref1, rsize,
		      &hp, NULL);
    ctl = TUPLE4(hp,
		 make_small(DOP_DEMONITOR_P),
		 watcher, watched, ref);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, force);
}

int
dist_send(Process* sender, int slot, Eterm remote, Eterm message)
{
    uint32 ctl;
    uint32 cookie = dist_addrs[slot].out_cookie;
    unsigned long version = dist_addrs[slot].version;
    uint32 token = NIL;

    if (cookie == NIL) {
	cookie = dist_addrs[THIS_NODE].out_cookie;
    }

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote);
    }

    ASSERT(is_port(dist_addrs[slot].cid));
    if (version > 0 && token != NIL)
	ctl = TUPLE4(dmem,make_small(DOP_SEND_TT),cookie, remote,
		     token);
    else
	ctl = TUPLE3(dmem,make_small(DOP_SEND),cookie, remote);
    return pack_and_send(slot, ctl, message, 0);
}

int
dist_reg_send(Process* sender, int slot, Eterm remote_name, Eterm message)
{
    uint32 ctl;
    uint32 cookie = dist_addrs[slot].out_cookie;
    unsigned long version = dist_addrs[slot].version;
    uint32 token = NIL;

    if (cookie == NIL) {
	cookie = dist_addrs[THIS_NODE].out_cookie;
    }

    if (SEQ_TRACE_TOKEN(sender) != NIL) {
	seq_trace_update_send(sender);
	token = SEQ_TRACE_TOKEN(sender);
	seq_trace_output(token, message, SEQ_TRACE_SEND, remote_name);
    }

    ASSERT(is_port(dist_addrs[slot].cid));
    if (version > 0 && token != NIL)
	ctl = TUPLE5(dmem,make_small(DOP_REG_SEND_TT),
		     sender->id, cookie, remote_name, token);
    else
	ctl = TUPLE4(dmem,make_small(DOP_REG_SEND),
		     sender->id, cookie, remote_name);
    return pack_and_send(slot, ctl, message, 0);
}

/* local has died, deliver the exit signal to remote
** We must always send the exit message to the other node
** this implies that the driver must always be ready to queue
** data even if it has signaled that it is busy !!!
*/
int dist_exit_tt(slot, local, remote, reason, token)
int slot; uint32 local; uint32 remote; uint32 reason; uint32 token;
{
    Eterm ctl;

    if (dist_addrs[slot].version > 0 && token != NIL) {	
	/* token should be updated by caller */
	seq_trace_output_exit(token, reason, SEQ_TRACE_SEND, remote, local);
	ctl = TUPLE5(dmem, make_small(DOP_EXIT_TT), local, remote, token, reason);
    } else {
	ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
    }
    del_link(find_link(&dist_addrs[slot].links, LNK_LINK, local, remote));
    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

int dist_exit(slot, local, remote, reason)
int slot; uint32 local; uint32 remote; uint32 reason;
{
    uint32 ctl = TUPLE4(dmem, make_small(DOP_EXIT), local, remote, reason);
    del_link(find_link(&dist_addrs[slot].links, LNK_LINK, local, remote));
    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 1);  /* forced, i.e ignore busy */
}

/* internal version of dist_exit2 that force send through busy port */
int dist_exit2(slot, local, remote, reason)
int slot; uint32 local; uint32 remote; uint32 reason;
{
    Eterm ctl = TUPLE4(dmem, make_small(DOP_EXIT2), local, remote, reason);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 0);
}


int dist_group_leader(slot, leader, remote)
int slot; uint32 leader; uint32 remote;
{
    Eterm ctl = TUPLE3(dmem, make_small(DOP_GROUP_LEADER), leader, remote);

    ASSERT(is_port(dist_addrs[slot].cid));
    return pack_and_send(slot, ctl, THE_NON_VALUE, 0);
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
int net_mess2(slot, hbuf, hlen, buf, len)
int slot; byte* hbuf; int hlen; byte* buf; int len;
{
    ErlLink **lnkp;
    byte *t;
    int i;
    int ctl_len;
    uint32 arg;
    uint32 from, to;
    uint32 watcher, watched;
    uint32 ref;
    uint32 res;
    uint32 *tuple;
    uint32 message;
    uint32 reason;
    Process* rp;
    ErlHeapFragment* msg = NULL;
    Eterm ctl_default[64];
    Eterm* ctl = ctl_default;
    ErlOffHeap off_heap;
    Eterm* hp;
    int type;
    Eterm token;

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
    print_pass_through(slot, t, len-1);
#endif
    if ((ctl_len = decode_size(t, len-1)) == -1) {
	PURIFY_MSG("data error");
	goto data_error;
    }
    if (ctl_len > sizeof(ctl)/sizeof(ctl[0])) {
	ctl = safe_alloc(ctl_len * sizeof(Eterm));
    }
    hp = ctl;

    off_heap.mso = NULL;
    off_heap.funs = NULL;
    off_heap.overhead = 0;
    arg = from_external(slot, &hp, &t, &off_heap);
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

    switch (type = unsigned_val(tuple[1])) {
    case DOP_LINK:
	from = tuple[2];
	to   = tuple[3];  /* local proc to link to */

	if ((rp = pid2proc(to)) == NULL) {
	    /* This is tricky (we MUST force a distributed send) */
	    /* We may send it to net_kernel and let it do the job !!! */
	    dist_exit(slot, to, from, am_noproc);
	    break;
	}
	if (find_link(&rp->links,LNK_LINK, from, NIL) != NULL)
	    break;
	dist_addrs[slot].links = new_link(dist_addrs[slot].links,LNK_LINK,
					  to, from);
	rp->links = new_link(rp->links, LNK_LINK, from, NIL);
	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(rp, am_getting_linked, from);
	break;

    case DOP_UNLINK:
	from = tuple[2];
	to = tuple[3];

	if ((rp = pid2proc(to)) == NULL)
	    break;
	del_link(find_link(&rp->links, LNK_LINK, from, NIL));
	del_link(find_link(&dist_addrs[slot].links, LNK_LINK, to, from));

	if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	    trace_proc(rp, am_unlink, from);
	break;

    case DOP_MONITOR_P:
	watcher = tuple[2];
	watched = tuple[3];  /* local proc to monitor */
	ref     = tuple[4];

	if (is_atom(watched)) {
	    rp = whereis_process(atom_val(watched));
	    if ((rp == NULL) || (rp->status == P_EXITING)) {
		dist_m_exit(slot, watcher, watched, ref_ptr(ref), am_noproc);
		break;
	    } else
		watched = rp->id;
	} else if ((rp = pid2proc(watched)) == NULL) {
	   dist_m_exit(slot, watcher, watched, ref_ptr(ref), am_noproc);
	   break;
	}
	dist_addrs[slot].links = new_ref_link(dist_addrs[slot].links,
					      LNK_LINK1, watcher, watched,
					      ref);
	rp->links = new_ref_link(rp->links, LNK_LINK1, watcher, watched, ref);
	break;

    case DOP_DEMONITOR_P:
	/* watcher = tuple[2]; */
	/* watched = tuple[3]; May be an atom in case of monitor name */
	ref = tuple[4];

	lnkp = find_link_by_ref(&dist_addrs[slot].links, ref_ptr(ref));
	if (lnkp == NULL)
	    break;
	watched = (*lnkp)->data;
	if ((rp = pid2proc(watched)) == NULL)
	   break;
	del_link(find_link_by_ref(&rp->links, ref_ptr(ref)));
	break;

    case DOP_NODE_LINK: /* XXX never sent ?? */
	break;

    case DOP_REG_SEND_TT:
    case DOP_REG_SEND:
	/* {DOP_REG_SEND, From, Cookie, ToName} -- Message */
	/* {DOP_REG_SEND_TT, From, Cookie, ToName, TraceToken} -- Message */
	if ((i = decode_size(t, len)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}
	from = tuple[2];
	to = tuple[4];

	msg = new_message_buffer(i+ctl_len);
	hp = msg->mem;
	message = from_external(slot, &hp, &t, &msg->off_heap);
	if (is_non_value(message)) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}
	
	if (type == DOP_REG_SEND) {
	    token = NIL;
	} else {
	    token = tuple[5];
	    token = copy_struct(token, size_object(token), &hp, NULL);
	}

	if (tuple[3] != dist_addrs[slot].in_cookie) {
	    res = TUPLE4(hp, from, am_badcookie, to, message);
	    dist_addrs[slot].out_cookie = am_badcookie;
	    queue_message_tt(net_kernel, msg, res, token);
	} else if ((rp = whereis_process(atom_val(to))) == NULL) {
	    /* No receiver on this node -- silently ignore this message */
	    free_message_buffer(msg);
	} else {
	    queue_message_tt(rp, msg, message, token);
	}
	break;

    case DOP_SEND_TT:
    case DOP_SEND:
	if ((i = decode_size(t, len)) == -1) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}
	msg = new_message_buffer(i+ctl_len);
	hp = msg->mem;

	/*
	 * We must decode the message even if the receiver is
	 * invalid in order to keep the atom cache up to date.
	 */
 
	message = from_external(slot, &hp, &t, &msg->off_heap);
	if (is_non_value(message)) {
	    PURIFY_MSG("data error");
	    goto data_error;
	}

	if (type == DOP_SEND) {
	    token = NIL;
	} else {
	    token = tuple[4];
	    token = copy_struct(token, size_object(token), &hp, NULL);
	}

	to = tuple[3];
	if ((rp = pid2proc(to)) == NULL) {
	    free_message_buffer(msg);
	} else if (tuple[2] != dist_addrs[slot].in_cookie) {
	    res = TUPLE4(hp, dist_addrs[slot].sysname, am_badcookie, 
			 to, message);
	    dist_addrs[slot].out_cookie = am_badcookie;
	    queue_message_tt(net_kernel, msg, res, token);
	} else {
	    queue_message_tt(rp, msg, message, token);
	}
	break;

    case DOP_MONITOR_P_EXIT:
	watched = tuple[2];  /* remote proc which died */
	watcher = tuple[3];
	ref     = tuple[4];
	reason  = tuple[5];

	if ((rp = pid2proc(watcher)) == NULL)
	   break;

	lnkp = find_link_by_ref(&rp->links, ref_ptr(ref));
	if (lnkp == NULL)
	   break;
	
	del_link(lnkp);
	del_link(find_link_by_ref(&dist_addrs[slot].links, ref_ptr(ref)));

	queue_monitor_message(rp, ref_ptr(ref), am_process, watched, reason);
	break;

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
	i = pid_or_port_creation(to);
	if ((i != 0) && (i != this_creation))
	    break;
	del_link(find_link(&dist_addrs[slot].links, LNK_LINK, to, from));

	if (is_pid(to)) {
	    rp = process_tab[pid_number(to)];
	    if (INVALID_PID(rp, to))
		break;
	    del_link(find_link(&rp->links, LNK_LINK, from, NIL));
#if 0
	    /* Arndt: Maybe it should never be 'kill', but it can be,
	       namely when a linked process does exit(kill). Until we know
	       whether that is incorrect and what should happen instead,
	       we leave the assertion out. */
	    ASSERT(reason != am_kill); /* should never be kill (killed) */
#endif
	    if (rp->flags & F_TRAPEXIT)
		/* token updated by remote node */
		deliver_exit_message_tt(from, rp, reason, token); 
	    else if (reason != am_normal)
		schedule_exit(rp, reason);
	}
	else if (is_port(to)) {
	    int ix = port_index(to);
	    if (erts_port[ix].status != FREE) {
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
	i = pid_creation(to);
	if ((i != 0) && (i != this_creation))
	    break;
	if (is_pid(to)) {
	    rp = process_tab[pid_number(to)];
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
	rp = process_tab[pid_number(to)];
	if (INVALID_PID(rp, to))
	    break;
	rp->group_leader = from;
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
    if (ctl != ctl_default) {
	sys_free(ctl);
    }
    return 0;

 data_error:
    if (off_heap.mso) {
	erts_cleanup_mso(off_heap.mso);
    }
    if (ctl != ctl_default) {
	sys_free(ctl);
    }
    if (msg) {
	free_message_buffer(msg);
    }
    do_exit_port(dist_addrs[slot].cid, dist_addrs[slot].cid, am_killed);
    return -1;
}



/*
** Return a slot to a node name
** the slot entry must have a valid connetion entry
*/

int sysname_to_dist_slot(sysname) 
Eterm sysname;
{
    int start;
    int pos;

    if (sysname == this_node)
	return 0;  /* special case */
    start = pos = atom_tab(atom_val(sysname))->slot.bucket.hvalue % MAXDIST;

    while (1) {
	if ((dist_addrs[pos].sysname == sysname) &&
	    (dist_addrs[pos].cid != NIL))
	    return pos;
	pos = (pos + 1) % MAXDIST;
	if (pos == start)
	    return -1;  /* went all the way round */
    }
    return -1;
}

int find_or_insert_dist_slot(sysname)
Eterm sysname;
{
    int start;
    int pos;
    int found_pos = -1;

    if (sysname == this_node)
	return 0; /* ok */
    if (is_not_atom(sysname))
	return -1;
    start = pos = atom_tab(atom_val(sysname))->slot.bucket.hvalue % MAXDIST;

    while (1) {
	/* Keep sysnames on clear */
	if (dist_addrs[pos].sysname == sysname)
	    return pos;
	if (dist_addrs[pos].sysname == am_invalid) {
	    if (found_pos == -1)
		found_pos = pos;
	}
	pos = (pos + 1) % MAXDIST;
	if (pos == start) {  /* Gone all the way around */
	    if (found_pos != -1) {
		dist_addrs[found_pos].sysname = sysname;
		dist_addrs[found_pos].status |= D_RESERVED;
		return found_pos;
	    }
	    else {
		int i;
		int done = 0;

		for(i = 1; i < MAXDIST; i++) {
		    if ((dist_addrs[i].cid == NIL) &&
			!(dist_addrs[i].status & D_RESERVED)) {
			dist_addrs[i].sysname = am_invalid;
			dist_addrs[i].status = 0;
			done++;
		    }
		}
		if (done == 0) {
		    cerr_pos = 0;
		    erl_printf(CBUF, "Out of space in dist table");
		    send_error_to_logger(NIL);
		    return -1;
		}
		pos = (pos + 1) % MAXDIST;
	    }
	}
    }
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


static int 
pack_and_send(int slot, Eterm ctl, Eterm mess, int force_busy)
{
    byte *t;
    Port* p;
    Eterm cid = dist_addrs[slot].cid;

    if (this_node == am_Noname)
	return -1;
    if (cid == NIL)
	return 0;
    if (dist_addrs[slot].status & D_EXITING) /* ??? */
	return 0; /* Ignore it */
    p = &erts_port[port_index(cid)];
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
    to_external(slot, ctl, &t);
    if (is_value(mess))
	to_external(slot, mess, &t);
    dist_port_command(p, dist_buf, t-dist_buf);
    return 0;
}



int distribution_info(to)		/* Called by break handler */
CIO to;
{
    int slot;
    ErlLink* lnk;
    int done = 0;

    if (this_node == am_Noname) {
	erl_printf(to,"Not alive\n"); 
	return(0);
    }
    erl_printf(to,"------------------------\n");
    for (slot = 1; slot < MAXDIST; slot++) {
	if (dist_addrs[slot].cid == NIL) {
	    if (dist_addrs[slot].links)
		erl_printf(to,"error .. Got links on channel %d\n", slot);
	    continue;
	}
	done++;
	erl_printf(to, "%d : Connection to:", slot);
	display(dist_addrs[slot].sysname, to);
	erl_printf(to, " Controller:");
	display(dist_addrs[slot].cid, to);
	erl_printf(to, " out_cookie: ");
	display(dist_addrs[slot].out_cookie, to);
	erl_printf(to,"  in_cookie: ", slot);
	display(dist_addrs[slot].in_cookie, to);
	erl_printf(to, "\n");
	
	if ((lnk = dist_addrs[slot].links)) {
	    erl_printf(to,"\n");
	    erl_printf(to,"Processes holding remote links to ");
	    display(dist_addrs[slot].sysname,to);
	    erl_printf(to,":\n");
	    while(lnk) {
		display(lnk->item,to);
		erl_printf (to," linked to ");
		if (lnk->data == NIL)
		    display(dist_addrs[slot].sysname, to);
		else
		    display(lnk->data,to);
		erl_printf(to," \n");
		lnk = lnk->next;
	    }
	}
	erl_printf(to,"\n------------------------\n");
    }
    if (!done) 
	erl_printf(to,"Alive but not holding any connections \n");
    return(0);
}

/* Turn this on to get printouts of all distribution messages
 * which go on the line
 */

#ifdef MESS_DEBUG


void upp(buf,sz)
byte* buf;
int sz;
{
    bin_write(CERR,buf,sz);
}

void print_pass_through(slot, t, len)
int slot; byte *t; int len;
{
    ErlHeapFragment* ctl = NULL;
    ErlHeapFragment* msg = NULL;
    uint32* hp;
    byte *orig = t;
    uint32 i;

    if ((i = decode_size(t, len)) == -1) {
	erl_printf(CERR,"Bailing out in decode_size control message\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in decode_size control message\n");
    }
    ctl = new_message_buffer(i);
    hp = ctl->mem;
    i = from_external(slot, &hp, &t, &ctl->mso);
    if (is_non_value(i)) {
	erl_printf(CERR,"Bailing out in from_external control message\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in from_external control message\n");
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
    i = from_external(slot, &hp, &t, &msg->mso);
    if (is_non_value(i)) {
	erl_printf(CERR,"Bailing out in from_external second element\n\r");
	upp(orig, len);
	erl_exit(1, "Bailing out in from_external second element\n");
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
    int  hour,minute,secs;
    char buf[DEFAULT_COOKIE_SIZE];
    long seed;
    int n;
    int i;
    uint32 cookie;

    if (is_not_atom(BIF_ARG_1) || is_not_small(BIF_ARG_2))
	goto error;
    /* valid creation ? */
    if ((n = signed_val(BIF_ARG_2)) < 0)
	goto error;
    /* valid node name ? */
    if (!is_node_name(atom_tab(atom_val(BIF_ARG_1))->name,
		      atom_tab(atom_val(BIF_ARG_1))->len))
	goto error;

    if (BIF_ARG_1 == am_Noname) /* cant use this name !! */
	goto error;
    if (net_kernel != NULL)     /* net_kernel must be down */
	goto error;

    /* Check that all trap functions are defined !! */
    if (dsend_trap->address == NULL ||
	dlink_trap->address == NULL ||
	dunlink_trap->address == NULL ||
	dmonitor_node_trap->address == NULL ||
	dgroup_leader_trap->address == NULL ||
#ifdef MONITOR_ENHANCE
	dmonitor_node2_trap->address == NULL ||
#endif
	dmonitor_p_trap->address == NULL ||
	dexit_trap->address == NULL)
	goto error;

    if ((net_kernel = whereis_process(atom_val(am_net_kernel))) == NULL)
	goto error;
    /* By setting dslot==0 (this node slot) and DISTRIBUTION on
       net_kernel do_net_exist will be called when net_kernel
       is terminated !! */
    net_kernel->dslot = 0;
    net_kernel->flags |= F_DISTRIBUTION;
    get_time(&hour, &minute, &secs);
    if (secs == 0)
	secs=1;
    seed = 2*(hour + minute + secs) + 1;
    i = 0;
    while (i < DEFAULT_COOKIE_SIZE) {
	int c;
	seed = (seed *  1220703125) & 0x3fffffff;
	c = (seed >> 8) & 0x7f;
	if ((c > 64 && c < 91) || (c > 96 && c < 123))
	    buf[i++] = c;
    }
    cookie = am_atom_put(buf,DEFAULT_COOKIE_SIZE);
    dist_addrs[THIS_NODE].sysname = BIF_ARG_1;
    dist_addrs[THIS_NODE].out_cookie = cookie;
    dist_addrs[THIS_NODE].in_cookie = cookie;
    for (i = 1; i < MAXDIST; i++)
	dist_addrs[i].in_cookie = cookie;
    this_node = BIF_ARG_1;
    this_creation = n & 0xff;
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************
 ** Allocate a dist slot, set node name install the connection handler
 ** setnode_3(name@host, Cid, {Type, Version, Initial, IC, OC})
 ** Type = flag field, where the flags are specified in dist.h
 ** Version = distribution version, >= 1
 ** IC = in_cookie, ie the cookie expected on incoming messages, 
 ** OC = out_cookie, ie the cookie set in outgoing messages.
 **
 ** Note that in distribution protocols above 1, the Initial parameter
 ** is alwais NIL and the cookies are always the atom '', cookies are not
 ** sent in the distribution messages but are only used in 
 ** the handshake.
 **
 ***********************************************************************/

BIF_RETTYPE setnode_3(BIF_ALIST_3)
BIF_ADECL_3
{
    int slot;
    Uint flags;
    unsigned long version;
    Eterm ic, oc;
    Eterm *tp;


    /*
     * Check and pick out arguments
     */
    if (is_not_atom(BIF_ARG_1) ||
	is_not_port(BIF_ARG_2) ||
	(this_node == am_Noname)) {
	goto error;
    }

    if (!is_node_name(atom_tab(atom_val(BIF_ARG_1))->name,
		      atom_tab(atom_val(BIF_ARG_1))->len))
	goto error;
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

    /*
     * Arguments seem to be in order.
     */

    /* get slot (0 is this node name) */
    if ((slot = find_or_insert_dist_slot(BIF_ARG_1)) <= 0)
	goto error;

    if (dist_addrs[slot].cid == BIF_ARG_2)
	goto done;
    /* We may have a sync problem here ?? */
    if (dist_addrs[slot].cid != NIL)
	goto error;

    if (is_port(BIF_ARG_2)) {
	int n = port_node(BIF_ARG_2);
	int ix = port_index(BIF_ARG_2);
	if ((n != THIS_NODE) || (erts_port[ix].status == FREE) ||
	    (erts_port[ix].status & EXITING) || (erts_port[ix].dslot != -1))
	    goto error;
	erts_port[ix].status |= DISTRIBUTION;
	erts_port[ix].dslot = slot;
    }

    if (!(flags & DFLAG_ATOM_CACHE) ||
	(!(flags & DFLAG_PUBLISHED) && !(flags & DFLAG_HIDDEN_ATOM_CACHE))
	/* Nodes which cannot use atom cache on non-published connections
	   (pre r7b01_patched nodes) doesn't send the DFLAG_HIDDEN_ATOM_CACHE
	   flag. */) {
	delete_cache(slot);
    } else {
	create_cache(slot);
    }

    dist_addrs[slot].flags = flags;
    dist_addrs[slot].version = version;
    dist_addrs[slot].cid = BIF_ARG_2;
    dist_addrs[slot].in_cookie = ic;
    dist_addrs[slot].out_cookie = oc;


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
    uint32 local;
    uint32 remote;
    uint32 rslot;
    Process *lp;
    uint32 exit_value = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;

    local = BIF_ARG_1;
    remote = BIF_ARG_3;

    /* Check that it is a process or port */
    if (is_pid(remote))
	rslot = pid_node(remote);
    else if (is_port(remote))
	rslot = port_node(remote);
    else
	goto error;

    /* Check that remote is remote, also check that if
       the caller is a distribution controller then the
       object is from that node */
    if ((rslot == THIS_NODE) ||
	((BIF_P->dslot != -1) && (BIF_P->dslot != rslot)))
	goto error;

    /* Check that local is local */
    if (is_pid(local) && (pid_node(local) == THIS_NODE)) {
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
    else if (is_port(local)) {
	if (port_node(local) != THIS_NODE)
	    goto error;
	do_exit_port(local, remote, BIF_ARG_2);
	if (BIF_P->status != P_RUNNING) {
	    BIF_P->fvalue = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;
	    KILL_CATCHES(BIF_P);
	    BIF_ERROR(BIF_P, USER_EXIT);         
	}
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
    uint32 local;
    uint32 remote;
    Process *lp;
    int slot = BIF_P->dslot;

    /* Must be called from distribution process */
    if (!(BIF_P->flags & F_DISTRIBUTION) || (slot == -1))
	goto error;

    local = BIF_ARG_1;
    remote = BIF_ARG_2;

    if (is_not_pid(remote) || (pid_node(remote) != slot))
	goto error;
    if (is_pid(local)) {
	if ((lp = pid2proc(local)) == NULL) {
	    dist_exit(slot, local, remote,  am_noproc);
	    BIF_RET(am_true);
	}
    }
    else /* no ports yet */
	goto error;

    if (find_link(&lp->links, LNK_LINK, remote,NIL) != NULL)
        BIF_RET(am_true);

    dist_addrs[slot].links = new_link(dist_addrs[slot].links,LNK_LINK,
				      local, remote);
    lp->links = new_link(lp->links,LNK_LINK,remote,NIL);

    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(lp, am_getting_linked, remote);
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* unlink(Local, Remote) -> Bool */

BIF_RETTYPE dist_unlink_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32 local = BIF_ARG_1;
    uint32 remote = BIF_ARG_2;
    Process *lp;
    int slot = BIF_P->dslot;

    /* Must be called from distribution process */
    if (!(BIF_P->flags & F_DISTRIBUTION) || (slot == -1))
	goto error;

    /* Remote must be a process */
    if (is_not_pid(remote) || (pid_node(remote) != slot))
	goto error;

    if (is_pid(local)) {
	if ((lp = pid2proc(local)) == NULL)
	    BIF_RET(am_true);
    }
    else /* no ports yet */
	goto error;

    /* unlink and ignore errors */
    del_link(find_link(&lp->links, LNK_LINK, remote, NIL));
    del_link(find_link(&dist_addrs[slot].links, LNK_LINK,
		       local, remote));
    if (IS_TRACED_FL(lp, F_TRACE_PROCS))
        trace_proc(lp, am_unlink, remote);
    BIF_RET(am_true);

 error:
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/
/* node(Object) -> Node */

BIF_RETTYPE node_1(BIF_ALIST_1)
BIF_ADECL_1
{ 
    int val;
    if (is_pid(BIF_ARG_1))
	val = pid_node(BIF_ARG_1);
    else if ((is_port(BIF_ARG_1)))
	val = port_node(BIF_ARG_1);
    else if (is_ref(BIF_ARG_1))
	val = ref_node(BIF_ARG_1);
    else {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(dist_addrs[val].sysname);
}

/**********************************************************************/
/* node() -> Node */

BIF_RETTYPE node_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RET(this_node);
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
#undef  ADD_NODE
#define ADD_NODE                                                             \
  (((dist_addrs[i].cid != NIL) &&                                            \
    (connected                                                               \
     || (published && (dist_addrs[i].flags & DFLAG_PUBLISHED))               \
     || (hidden && !(dist_addrs[i].flags & DFLAG_PUBLISHED))))               \
   || (known && (dist_addrs[i].status & D_RESERVED)))

#undef  SET_NODE_TYPE
#define SET_NODE_TYPE(X)                                                     \
  do {                                                                       \
    switch((X)) {                                                            \
    case am_visible:   published = 1;                                 break; \
    case am_hidden:    hidden    = 1;                                 break; \
    case am_known:     known     = 1;                                 break; \
    case am_this:      this      = 1;                                 break; \
    case am_connected: connected = 1;                                 break; \
    default:           BIF_ERROR(BIF_P, BADARG);                      break; \
    }                                                                        \
  } while(0)

    int i;
    uint32 previous;
    int length;
    uint32* hp;
    int connected = 0;
    int published = 0;
    int hidden = 0;
    int known = 0;
    int this = 0;

    if (is_atom(BIF_ARG_1))
      SET_NODE_TYPE(BIF_ARG_1);
    else {
      Eterm list = BIF_ARG_1;

      while (is_list(list)) {
	SET_NODE_TYPE(CAR(list_val(list)));
	list = CDR(list_val(list));
      }

      if (is_not_nil(list))
	BIF_ERROR(BIF_P, BADARG);
    }

    length = this || known ? 1 : 0;
    if (this_node != am_Noname)
      for (i=1; i <MAXDIST ; i++)
	if (ADD_NODE)
	  length++;

    if (length == 0)
	BIF_RET(NIL);

    hp = HAlloc(BIF_P, 2*length);

    previous = NIL;

    if(this || known) {
      previous = CONS(hp, this_node, previous);
      hp += 2;
    }

    if (this_node != am_Noname)
      for (i = 1; i <MAXDIST; i++)
	if (ADD_NODE) {
	  previous = CONS(hp, dist_addrs[i].sysname, previous);
	  hp += 2;
	}

    BIF_RET(previous);
#undef  ADD_NODE
#undef  SET_NODE_TYPE
}

/**********************************************************************/
/* is_alive() -> Bool */

BIF_RETTYPE is_alive_0(BIF_ALIST_0)
BIF_ADECL_0
{
    if (this_node == am_Noname)
	BIF_RET(am_false);
    BIF_RET(am_true);
}

/**********************************************************************/
/* monitor_node(Node, Bool) -> Bool */

BIF_RETTYPE monitor_node_2(BIF_ALIST_2)
BIF_ADECL_2
{
    int slot;

    if (is_not_atom(BIF_ARG_1) ||
	((BIF_ARG_2 != am_true) && (BIF_ARG_2 != am_false)) ||
	((this_node == am_Noname) && (BIF_ARG_1 != this_node))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((slot = find_or_insert_dist_slot(BIF_ARG_1)) < 0 ) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (slot == 0) 
	BIF_RET(am_true);
    if (BIF_ARG_2 == am_true) {
	if (dist_addrs[slot].cid == NIL)
	    BIF_TRAP2(dmonitor_node_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	dist_addrs[slot].links = new_link(dist_addrs[slot].links,
					  LNK_NODE, BIF_P->id, NIL);
	/* slot is inserted in process side (easy removal) */
	BIF_P->links = new_link(BIF_P->links, LNK_NODE, BIF_ARG_1, slot);
	BIF_RET(am_true);
    }
    else  {
	del_link(find_link(&dist_addrs[slot].links, LNK_NODE, BIF_P->id,NIL));
	del_link(find_link(&BIF_P->links, LNK_NODE, BIF_ARG_1, NIL));
	BIF_RET(am_true);
    }
}
