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

extern DriverEntry fd_driver_entry;
extern DriverEntry vanilla_driver_entry;
extern DriverEntry spawn_driver_entry;

static uint32 make_ref(Process *);	/* forward */

/*
 * The BIF's now follow, see the Erlang Manual for a description of what
 * each individual BIF does.
 *
 * Guards BIFs must not build anything at all on the heap.
 * They must use the ArithAlloc() macro instead of HAlloc().
 */

BIF_RETTYPE spawn_3(BIF_ALIST_3)
BIF_ADECL_3
{
    ErlSpawnOpts so;
    uint32 pid;

    so.flags = 0;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (pid == 0) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

/**********************************************************************/

/* utility to add a new link between processes p and rp */

static void insert_link(p, rp)
Process* p; Process* rp;
{
    p->links = new_link(p->links, LNK_LINK, rp->id, NIL);
    rp->links = new_link(rp->links, LNK_LINK, p->id, NIL);

    if (IS_TRACED(p)) {
	if (p->flags & F_TRACE_SOL)  {
	    rp->flags |= (p->flags & TRACE_FLAGS);
	    rp->tracer_proc = p->tracer_proc;    /* maybe steal */
	}
	if (p->flags & F_TRACE_SOL1 )  { /* maybe override */
	    rp->flags |= (p->flags & TRACE_FLAGS);
	    rp->tracer_proc = p->tracer_proc;   
	    rp ->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
	    p->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
	}
    }
    if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	trace_proc(rp, am_getting_linked, p->id);
}


/* create a link to the process */
BIF_RETTYPE link_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp;
    int slot;
    int ix;
    int code;

    if (IS_TRACED(BIF_P)) {
	if (BIF_P->flags & F_TRACE_PROCS)
	    trace_proc(BIF_P, am_link, BIF_ARG_1);
    }
    /* check that the pid which is our argument is OK */

    if (is_not_pid(BIF_ARG_1)) {
	if (is_not_port(BIF_ARG_1))
	    BIF_ERROR(BIF_P, BADARG);
    
	/* we are linking to a port */
	if (find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL) != NULL)
	    BIF_RET(am_true); /* already linked */

	if (get_node_port(BIF_ARG_1) != THIS_NODE) {
	    /* Don't allow link to a remote port */
	    BIF_ERROR(BIF_P, BADARG);
	} else {
	    ix = get_port_index(BIF_ARG_1);
	    if (erts_port[ix].status == FREE) {
		if (BIF_P->flags & F_TRAPEXIT) {
		    deliver_exit_message(BIF_ARG_1, BIF_P, am_noproc);
		    BIF_RET(am_true);
		} else {
		    BIF_ERROR(BIF_P, NOPROC);
		}
	    }
	    erts_port[ix].links = new_link(erts_port[ix].links,LNK_LINK,BIF_P->id,NIL);
	}
	BIF_P->links = new_link(BIF_P->links,LNK_LINK,BIF_ARG_1,NIL);
	BIF_RET(am_true);
    }

    /* we are linking to another process */
    /* check that the PID is OK */
    
    if (BIF_ARG_1 == BIF_P->id)
	BIF_RET(am_true);  /* Some people try the silliest things... */

    if (find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1, NIL) != NULL)
	BIF_RET(am_true);	/*  already linked */	
    
    if ((slot = get_node(BIF_ARG_1)) != THIS_NODE) {  /* link to net */
	if (dist_addrs[slot].cid == NIL)
	    BIF_TRAP1(dlink_trap, BIF_P, BIF_ARG_1);

	if ((code = dist_link(slot, BIF_P->id, BIF_ARG_1)) == 1) {
	    ASSERT(is_port(dist_addrs[slot].cid)); 
	    erl_suspend(BIF_P, dist_addrs[slot].cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	else if (code < 0) { /* XXX is this the correct behaviour ??? */
	    BIF_ERROR(BIF_P, NOTALIVE);
	}
	/* insert the link in our own process */
	BIF_P->links = new_link(BIF_P->links,LNK_LINK,BIF_ARG_1,NIL);
	dist_addrs[slot].links = new_link(dist_addrs[slot].links,
					  LNK_LINK, BIF_P->id, BIF_ARG_1);
	BIF_RET(am_true);
    }

    if (get_number(BIF_ARG_1) >= max_process) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    /* get a pointer to the process struct of the linked process */
    rp = process_tab[get_number(BIF_ARG_1)];
    
    /* is the right process - ie not free and the pid corresponds?? */
    if (INVALID_PID(rp, BIF_ARG_1)) {
	/* Schedule an exit and insert the link in our own queue only */
	if (BIF_P->flags & F_TRAPEXIT) {
	    deliver_exit_message(BIF_ARG_1, BIF_P, am_noproc);
	    BIF_RET(am_true);
	}
	else
	    BIF_ERROR(BIF_P, NOPROC);
    }
    insert_link(BIF_P, rp);
    BIF_RET(am_true);
}


BIF_RETTYPE demonitor_1(BIF_ALIST_1)
BIF_ADECL_1
{
   ErlLink **lnkp = NULL;
   Process *rp;
   uint32 to, ref_p;
   int slot;

   if (is_not_refer(BIF_ARG_1))
      goto error;

   if ((slot = get_node_reference(BIF_ARG_1)) != THIS_NODE)
      goto error;

   ref_p = BIF_P->id;
   lnkp = find_link_by_ref(&BIF_P->links, ref_ptr(BIF_ARG_1));

   if (lnkp == NULL)
      return am_true;

   /* ref_p is the id of one of the processes of the link. We want it
      to be 'item'. */
   to = (*lnkp)->data;
   if (to == ref_p)
      goto error;

   if ((*lnkp)->type == LNK_LINK1) {
      if ((slot = get_node(to)) != THIS_NODE)
      {
	 dist_demonitor(slot, ref_p, to, ref_ptr(BIF_ARG_1));
	 /* We don't check the return code here. to be complete, we
	    should, and perhaps reschedule. */
      } else {
	 if ((rp = pid2proc(to)) != NULL)
	    del_link(find_link_by_ref(&rp->links, ref_ptr(BIF_ARG_1)));
      }
   }
#ifdef MONITOR_ENHANCE
   else {
      /* LNK_NODE1 */
      if ((slot = find_or_insert_dist_slot(to)) > 0) {
	  if (dist_addrs[slot].cid != NIL)
	      del_link(find_link_by_ref(&dist_addrs[slot].links,
					ref_ptr(BIF_ARG_1)));
      }
   }
#endif

   del_link(lnkp);
   BIF_RET(am_true);

 error:
   BIF_ERROR(BIF_P, BADARG);
}

/* reason may be composite; type and item must be atomic objects! */
void queue_monitor_message(Process *p,
			   Ref *ref0,
			   uint32 type,
			   uint32 item,
			   uint32 reason)
{
   uint32 tup;
   uint32 *hp;
   uint32 ref, ref1;
   ErlHeapFragment *bp;

   uint32 size, rsize;
   uint32 reason1;

   size = size_object(reason);
   ref1 = make_refer(ref0);
   rsize = size_object(ref1);
   bp = new_message_buffer(6+size+rsize);
   hp = bp->mem;
   reason1 = copy_struct(reason, size, &hp, &bp->off_heap);
   ref = copy_struct(ref1, rsize, &hp, &bp->off_heap);
   tup = TUPLE5(hp, am_DOWN, ref, type, item, reason1);
   queue_message(p, bp, tup);
}

BIF_RETTYPE monitor_2(BIF_ALIST_2)
BIF_ADECL_2
{
   int linktype;
   uint32 type, item;
   uint32 ref;
   Process *rp;
   int slot;
   int code;
   ErlLink **lnkp;
#ifdef MONITOR_ENHANCE
   int ix;
#endif

   type = BIF_ARG_1;
   item = BIF_ARG_2;
   if (type == am_process) {
      if (is_not_pid(item))
	 goto error;
      slot = get_node(item);
   }
#ifdef MONITOR_ENHANCE
   else if (type == am_port) {
      if (is_not_port(item))
	 goto error;
      slot = get_node_port(item);
   }
   else if (type == am_node) {
      if (is_not_atom(item) ||
	  ((this_node == am_noname) && (item != this_node)))
	 goto error;

      if ((slot = find_or_insert_dist_slot(item)) < 0 )
	 goto error;

      if (slot == 0)
	 BIF_RET(make_ref(BIF_P));

      if (dist_addrs[slot].cid == NIL)
	 BIF_TRAP2(dmonitor_node2_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
/*
Add this to erlang.erl:

-export([dmonitor_node2/2]).

dmonitor_node2(Type, Node) ->
    %% Type = node
    case net_kernel:connect(Node) of
	true ->
	    erlang:monitor(node, Node);
	false ->
	    Ref = make_ref(),
	    self() ! {'DOWN', Ref, node, Node},
	    Ref
    end.
*/

   }
#endif
   else
      goto error;

   ref = make_ref(BIF_P);

   /* Currently, we have a port or a pid or an atom at this point. */

   if (item == BIF_P->id)
      BIF_RET(ref);		/* Some people try the silliest things... */

   if (type == am_process) {
      if (slot != THIS_NODE) {
	 if (dist_addrs[slot].cid == NIL)
	    BIF_TRAP2(dmonitor_p_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);

	 if (!(dist_addrs[slot].flags & DFLAG_DIST_MONITOR)) 
	     /* 4 = distr. version for R6 */
	    goto error;

	 if ((code = dist_monitor(slot, BIF_P->id,
				  BIF_ARG_2, ref_ptr(ref))) == 1) {
	    ASSERT(is_port(dist_addrs[slot].cid)); 
	    erl_suspend(BIF_P, dist_addrs[slot].cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	 }
	 else if (code < 0) {	/* XXX is this the correct behaviour ??? */
	    /* When does this happen, and what do we do? */
	    BIF_ERROR(BIF_P, NOTALIVE);
	 }
	 BIF_P->links = new_ref_link(BIF_P->links, LNK_LINK1,
				     BIF_P->id, item, ref);
	 dist_addrs[slot].links = new_ref_link(dist_addrs[slot].links,
					       LNK_LINK1, BIF_P->id,
					       item, ref);
	 BIF_RET(ref);
      }
      rp = process_tab[get_number(item)];
      if (INVALID_PID(rp, item))
	 lnkp = NULL;
      else
	 lnkp = &rp->links;
      linktype = LNK_LINK1;
   }
#ifdef MONITOR_ENHANCE
   else if (type == am_port) {
      if (slot != THIS_NODE)
	 goto error;
      ix = get_port_index(item);
      if (port[ix].status == FREE)
	 lnkp = NULL;
      else
	 lnkp = &port[ix].links;
      linktype = LNK_LINK1;
   } else {
      /* node */
      lnkp = &dist_addrs[slot].links;
      linktype = LNK_NODE1;
   }
#endif

   if (lnkp == NULL)
   {
      queue_monitor_message(BIF_P, ref_ptr(ref), type, item, am_noproc);
   }
   else
   {
      BIF_P->links = new_ref_link(BIF_P->links, linktype,
				  BIF_P->id, item, ref);
      *lnkp = new_ref_link(*lnkp, linktype,
			   BIF_P->id, item, ref);
   }

   BIF_RET(ref);

 error:
   BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* this is a combination of the spawn and link BIFs */

BIF_RETTYPE spawn_link_3(BIF_ALIST_3)
BIF_ADECL_3
{
    ErlSpawnOpts so;
    uint32 pid;

    so.flags = SPO_LINK;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (pid == 0) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

/**********************************************************************/

BIF_RETTYPE spawn_opt_2(BIF_ALIST_2)
BIF_ADECL_2
{
    ErlSpawnOpts so;
    uint32 pid;
    uint32* tp;
    uint32 ap;
    uint32 arg;

    /*
     * Check that the first argument is a tuple of three elements.
     */
    if (is_not_tuple(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    tp = ptr_val(BIF_ARG_1);
    if (*tp != make_arityval(3))
	goto error;

    /*
     * Store default values for options.
     */
    so.flags = SPO_USE_ARGS;
    so.min_heap_size = H_MIN_SIZE;
    so.gc_switch = switch_gc_threshold;
    so.priority = PRIORITY_NORMAL;
    so.process_flags = 0;

    /*
     * Walk through the option list.
     */
    ap = BIF_ARG_2;
    while (is_list(ap)) {
	arg = CAR(ptr_val(ap));
	if (arg == am_link) {
	    so.flags |= SPO_LINK;
	} else if (is_tuple(arg)) {
	    uint32* tp2 = ptr_val(arg);
	    uint32 val;
	    if (*tp2 != make_arityval(2))
		goto error;
	    arg = tp2[1];
	    val = tp2[2];
	    if (arg == am_priority) {
		if (val == am_max)
		    so.priority = PRIORITY_MAX;
		else if (val == am_high)
		    so.priority = PRIORITY_HIGH;
		else if (val == am_normal)
		    so.priority = PRIORITY_NORMAL;
		else if (val == am_low)
		    so.priority = PRIORITY_LOW;
		else
		    goto error;
	    } else if (arg == am_gc_switch) {
		if (val == am_infinity) {
		    so.gc_switch = MAX_SMALL;
		} else if (is_small(val) && signed_val(val) >= 0) {
		    so.gc_switch = signed_val(val);
		} else {
		    goto error;
		}
	    } else if (arg == am_min_heap_size && is_small(val)) {
		so.min_heap_size = signed_val(val);
		if (so.min_heap_size < 0) {
		    goto error;
		} else if (so.min_heap_size < H_MIN_SIZE) {
		    so.min_heap_size = H_MIN_SIZE;
		} else {
		    so.min_heap_size = next_heap_size(so.min_heap_size, 0);
		}
	    } else {
		goto error;
	    }
	} else {
	    goto error;
	}
	ap = CDR(ptr_val(ap));
    }
    if (is_not_nil(ap))
	goto error;

    /*
     * Spawn the process.
     */
    pid = erl_create_process(BIF_P, tp[1], tp[2], tp[3], &so);
    if (pid == 0) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

  
/**********************************************************************/
/* remove a link from a process */
BIF_RETTYPE unlink_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp;
    int slot;
    int ix;

    if (IS_TRACED(BIF_P)) {
	if (BIF_P->flags & F_TRACE_PROCS) 
	    trace_proc(BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_not_pid(BIF_ARG_1)) {
	if (is_not_port(BIF_ARG_1))
	    BIF_ERROR(BIF_P, BADARG);

	if (get_node_port(BIF_ARG_1) != THIS_NODE)
	    BIF_ERROR(BIF_P, BADARG);

	del_link(find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL));
	ix = get_port_index(BIF_ARG_1);
	if (erts_port[ix].status != FREE)
	    del_link(find_link(&erts_port[ix].links,LNK_LINK,BIF_P->id,NIL));
	BIF_RET(am_true);
    }

    del_link(find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL));

    if ((slot = get_node(BIF_ARG_1)) != THIS_NODE) {
	if (dist_addrs[slot].cid == NIL)
	    BIF_TRAP1(dunlink_trap, BIF_P, BIF_ARG_1);

	if (dist_unlink(slot, BIF_P->id, BIF_ARG_1) == 1) {
	    ASSERT(is_port(dist_addrs[slot].cid)); 
	    erl_suspend(BIF_P, dist_addrs[slot].cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	del_link(find_link(&dist_addrs[slot].links, LNK_LINK,
			   BIF_P->id, BIF_ARG_1));
	BIF_RET(am_true);
    }

     /* process ok ? */
    if (get_number(BIF_ARG_1) >= max_process)
	BIF_ERROR(BIF_P, BADARG);
     
    /* get process struct */
    rp = process_tab[get_number(BIF_ARG_1)];

    /* and we mean this process */
    if (INVALID_PID(rp, BIF_ARG_1))
	BIF_RET(am_true);

    /* unlink and ignore errors */
    del_link(find_link(&rp->links, LNK_LINK, BIF_P->id, NIL));
     
     BIF_RET(am_true);
}

/**********************************************************************/
/*
 * Similar to exit/1, except that the EXIT term will look like
 * {Term,Where}, where Where describes the location of the failure.
 */

BIF_RETTYPE fault_1(Process* p, Eterm term)
{
    p->fvalue = term;
    BIF_ERROR(p, USER_ERROR);
}

BIF_RETTYPE fault_2(Process* p, Eterm value, Eterm args)
{
    Eterm* hp = HAlloc(p, 3);

    p->fvalue = TUPLE2(hp, value, args);
    BIF_ERROR(p, USER_ERROR2);
}

/**********************************************************************/
/* this is the same as throw/1 except that we set freason to USER_EXIT */

BIF_RETTYPE exit_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_P->fvalue = BIF_ARG_1;  /* exit value */
    BIF_ERROR(BIF_P, USER_EXIT);
}

/**********************************************************************/
/* send an exit message to another process (if trapping exits) or
   exit the other process */

BIF_RETTYPE exit_2(BIF_ALIST_2)
BIF_ADECL_2
{
     Process *rp;
     int slot;
     int code;
     uint32 exit_value = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;
     /*
      * If the first argument is not a pid, it must a be port
      * or it is an error.
      */

     if (is_not_pid(BIF_ARG_1)) {
	 if (is_not_port(BIF_ARG_1)) 
	     BIF_ERROR(BIF_P, BADARG);

	 if (get_node_port(BIF_ARG_1) != THIS_NODE) {
	     /* remote port */
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 do_exit_port(BIF_ARG_1, BIF_P->id, BIF_ARG_2);
	 if (BIF_P->status != P_RUNNING) {
	     BIF_P->fvalue = exit_value;
	     KILL_CATCHES(BIF_P);
	     BIF_ERROR(BIF_P, USER_EXIT);
	 }
	 BIF_RET(am_true);
     }
     
     /*
      * It is a pid.  If it is a remote pid, send a message to the remote
      * node.
      */

     if ((slot = get_node(BIF_ARG_1)) != THIS_NODE) {
	 if (dist_addrs[slot].cid == NIL)
	     BIF_TRAP2(dexit_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);

	 if ((code = dist_exit2(slot, BIF_P->id, BIF_ARG_1, BIF_ARG_2)) == 1) {
	     ASSERT(is_port(dist_addrs[slot].cid)); 
	     erl_suspend(BIF_P, dist_addrs[slot].cid);
	     BIF_ERROR(BIF_P, RESCHEDULE);
	 }
	 else if (code < 0) {
	     BIF_ERROR(BIF_P, NOTALIVE);
	 }
	 BIF_RET(am_true);
     }

     /*
      * The pid is local.  Verify that it refers to an existing process.
      */

     if (get_number(BIF_ARG_1) >= max_process)
	 BIF_ERROR(BIF_P, BADARG);
     rp = process_tab[get_number(BIF_ARG_1)];
     if (INVALID_PID(rp, BIF_ARG_1))
	 BIF_RET(am_true);

     /*
      * Send an 'EXIT' message or schedule an process exit.
      */

     if (rp->flags & F_TRAPEXIT && BIF_ARG_2 != am_kill)
	 deliver_exit_message(BIF_P->id, rp, BIF_ARG_2);
     else if (BIF_ARG_2 != am_normal || rp->id == BIF_P->id)
	 schedule_exit(rp, exit_value);

     /*
      * If the current process is not running (because schedule_exit()
      * was called for it above), make sure it exits.
      */

     if (BIF_P->status != P_RUNNING) {
	 BIF_P->fvalue = exit_value;
	 KILL_CATCHES(BIF_P);
	 BIF_ERROR(BIF_P, USER_EXIT);
     }

     BIF_RET(am_true);
}

/**********************************************************************/
/* this sets some process info- trapping exits or the error handler */


/* Not a bif. A help function for process_flag_2 and process_flag_3. */
static BIF_RETTYPE process_flag_aux(Process *BIF_P,
			     Process *rp,
			     Eterm flag,
			     Eterm val)
{
   uint32 old_value = NIL;	/* shut up warning about use before set */
   int i;
   struct saved_calls *ct;

   if (flag == am_error_handler) {
      if (is_not_atom(val)) {
	 BIF_ERROR(BIF_P, BADARG);
      }
      old_value = rp->error_handler;
      rp->error_handler = val;
      BIF_RET(old_value);
   }
   else if (flag == am_priority) {
      switch(rp->prio) {
       case PRIORITY_MAX:
	 old_value = am_max; break;
       case PRIORITY_HIGH:
	 old_value = am_high; break;
       case PRIORITY_NORMAL:
	 old_value = am_normal; break;
       case PRIORITY_LOW:
	 old_value = am_low; break;
      }
      if (val == am_max)
	 rp->prio = PRIORITY_MAX;
      else if (val == am_high)
	 rp->prio = PRIORITY_HIGH;
      else if (val == am_normal)
	 rp->prio = PRIORITY_NORMAL;
      else if (val == am_low)
	 rp->prio = PRIORITY_LOW;
      else {
	 BIF_ERROR(BIF_P, BADARG);
      }
      BIF_RET(old_value);
   }
   else if (flag == am_trap_exit) {
      if (rp->flags & F_TRAPEXIT) {
	 old_value = am_true;
      } else {
	 old_value = am_false;
      }
      if (val == am_true) {
	 rp->flags = rp->flags  | F_TRAPEXIT;
	 BIF_RET(old_value);
      }
      if (val == am_false) {
	 rp->flags = rp->flags & ~F_TRAPEXIT;
	 BIF_RET(old_value);
      } 
   }
   else if (flag == am_save_calls) {
      if (!is_small(val))
	 BIF_ERROR(BIF_P, BADARG);

      i = signed_val(val);
      if (i < 0 || i > 10000)
	 BIF_ERROR(BIF_P, BADARG);

      if (rp->ct == NULL)
	 old_value = make_small(0);
      else
	 old_value = make_small(rp->ct->len);
      if (i == 0) {
	 ct = NULL;
      } else {
	 ct = safe_alloc(sizeof(*ct) + (i-1) * sizeof(ct->ct[0]));
	 ct->len = i;
	 ct->cur = 0;
	 ct->n = 0;
      }

      if (rp->ct != NULL)
	 sys_free(rp->ct);
      rp->ct = ct;

      /* Make sure the process in question is rescheduled
	 immediately, if it's us, so the call saving takes effect. */
      if (rp == BIF_P)
	 BIF_RET2(old_value, CONTEXT_REDS);
      else
	 BIF_RET(old_value);
   }

   BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE process_flag_2(BIF_ALIST_2)
BIF_ADECL_2
{
   return process_flag_aux(BIF_P, BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE process_flag_3(BIF_ALIST_3)
BIF_ADECL_3
{
   Process *rp;

   if ((rp = pid2proc(BIF_ARG_1)) == NULL)
      BIF_ERROR(BIF_P, BADARG);

   if (BIF_ARG_2 != am_save_calls)
      BIF_ERROR(BIF_P, BADARG);

   return process_flag_aux(BIF_P, rp, BIF_ARG_2, BIF_ARG_3);
}

/**********************************************************************/

/* register(atom, Process) registers a global process (for this node) */

BIF_RETTYPE register_2(BIF_ALIST_2)   /* (Atom,Pid)   */
BIF_ADECL_2
{
     Process *rp;
     
     if ((is_not_atom(BIF_ARG_1)) || ((rp = pid2proc(BIF_ARG_2)) == NULL)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     /* Check that we don't register undefined */
     if (BIF_ARG_1 == am_undefined)
	 BIF_ERROR(BIF_P, BADARG);
     if (rp->reg != NULL) {
	 cerr_pos = 0;
	 display(rp->id, CBUF);
	 erl_printf(CBUF," already registered as ");
	 print_atom(rp->reg->name, CBUF);
	 erl_printf(CBUF,"\n");
	 send_error_to_logger(BIF_P->group_leader);
	 BIF_ERROR(BIF_P, BADARG);
     }
     if (register_process(unsigned_val(BIF_ARG_1), rp) != rp) {
	 cerr_pos = 0;
	 print_atom(unsigned_val(BIF_ARG_1), CBUF);
	 erl_printf(CBUF," already registered\n");
	 send_error_to_logger(BIF_P->group_leader);
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(am_true);
}


/**********************************************************************/

/* removes the registration of a process */

BIF_RETTYPE unregister_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((unregister_process(unsigned_val(BIF_ARG_1))) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

/**********************************************************************/

/* find out the pid of a registered process */
/* this is a rather unsafe BIF as it allows users to do nasty things. */

BIF_RETTYPE whereis_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp;
 
    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((rp = whereis_process(unsigned_val(BIF_ARG_1))) == NULL) {
	BIF_RET(am_undefined);
    }
    BIF_RET(rp->id);
}

/**********************************************************************/

/* return a list of the registered processes */

BIF_RETTYPE registered_0(BIF_ALIST_0)
BIF_ADECL_0
{
    int i;
    uint32 res;
    uint32 need;
    uint32* hp;
     
    /* work out how much heap we need & maybe garb, by scanning through
       the registered process table */
    need = 0;
    for (i = 0; i < max_process; i++) {
	if ((process_tab[i] != NULL) && (process_tab[i]->reg != NULL))
	    need += 2;
    }
    if (need == 0)
	BIF_RET(NIL);
    hp = HAlloc(BIF_P, need);
     
     /* scan through again and make the list */ 
    res = NIL;
    for (i = 0; i < max_process; i++) {
	if ((process_tab[i] != NULL) && (process_tab[i]->reg != NULL)) {
	    res = CONS(hp, make_atom(process_tab[i]->reg->name), res);
	    hp += 2;
	}
    }
    BIF_RET(res);
}

/**********************************************************************/

/*
 * Send a message to Process, Port or Registered Process.
 * Returns the message sent.
 */

BIF_RETTYPE send_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Process* rp;
    int slot;
    int creation;
    Eterm* tp;

    switch(tag_val_def(BIF_ARG_1)) {
    case PID_DEF:   /* Send to pid */
	if ((slot = get_node(BIF_ARG_1)) != THIS_NODE) { /* Remote Pid */
	    if (dist_addrs[slot].cid == NIL)
		BIF_TRAP2(dsend_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	    if (dist_send(BIF_P, slot, BIF_ARG_1, BIF_ARG_2) == 1) {
		/* Only ports can be busy */
		ASSERT(is_port(dist_addrs[slot].cid)); 
		erl_suspend(BIF_P, dist_addrs[slot].cid);
		BIF_ERROR(BIF_P, RESCHEDULE);
	    }
	    if (IS_TRACED(BIF_P))
		trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);  /* (p, to, msg )*/
	    if (BIF_P->ct != NULL)
	       save_calls(BIF_P, &exp_send);
	    BIF_RET2(BIF_ARG_2, 50);
	}

	if (IS_TRACED(BIF_P))
	    trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);  /* (p, to, msg )*/
	if (BIF_P->ct != NULL)
	   save_calls(BIF_P, &exp_send);
	if (get_number(BIF_ARG_1) >= MAX_PROCESS)
	    BIF_ERROR(BIF_P, BADARG);
	creation = get_creation(BIF_ARG_1);

	if ((creation != 0)  && (creation != this_creation)) {
	    cerr_pos = 0;
	    erl_printf(CBUF, "Discarding message (%d,%d)", creation, this_creation);
	    display(BIF_ARG_2,CBUF);
	    erl_printf(CBUF,"to a process in an old incarnation of this node\n");
	    send_error_to_logger(BIF_P->group_leader);
	    BIF_RET(BIF_ARG_2);
	}
	rp = process_tab[get_number(BIF_ARG_1)];
	if (INVALID_PID(rp, BIF_ARG_1)) {
	    BIF_RET(BIF_ARG_2);
	}
	break;

    case PORT_DEF:  /* Send to port */
	/* XXX let port_command handle the busy stuff !!! */
	if (erts_port[get_port_index(BIF_ARG_1)].status & PORT_BUSY) {
	    erl_suspend(BIF_P, BIF_ARG_1);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}

	if (IS_TRACED(BIF_P)) 	/* trace once only !! */
	    trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
	if (BIF_P->ct != NULL)
	   save_calls(BIF_P, &exp_send);

	if (SEQ_TRACE_TOKEN(BIF_P) != NIL) {
	    seq_trace_update_send(BIF_P);
	    seq_trace_output(SEQ_TRACE_TOKEN(BIF_P), BIF_ARG_2, SEQ_TRACE_SEND, BIF_ARG_1);
	}	    

	/* XXX NO GC in port command */
	port_command(BIF_ARG_1, BIF_ARG_2);

	if (BIF_P->status == P_EXITING) {
	    KILL_CATCHES(BIF_P); /* Must exit */
	    BIF_ERROR(BIF_P, USER_ERROR);
	}
	BIF_RET(BIF_ARG_2);

    case ATOM_DEF:  /* Send to registered process */
	if (IS_TRACED(BIF_P))
	    trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);  /* (p, to, msg )*/
	if (BIF_P->ct != NULL)
	   save_calls(BIF_P, &exp_send);

	if ((rp = whereis_process(unsigned_val(BIF_ARG_1))) == NULL) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	if (rp->status == P_EXITING) {
	    BIF_RET(BIF_ARG_2);
	}
	break;

    case TUPLE_DEF: /* Send remote */
	tp = ptr_val(BIF_ARG_1);
	if (*tp != make_arityval(2))
	    BIF_ERROR(BIF_P, BADARG);
	if (is_not_atom(tp[1]) || is_not_atom(tp[2]))
	    BIF_ERROR(BIF_P, BADARG);

	/* find_or_insert_dist_slot will complain */
	if ((slot = find_or_insert_dist_slot(tp[2])) < 0) {
	    if (IS_TRACED(BIF_P))
		trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
	    if (BIF_P->ct != NULL)
	       save_calls(BIF_P, &exp_send);
	    BIF_RET2(BIF_ARG_2, 50);
	}

	if (slot == THIS_NODE) {
	    if (IS_TRACED(BIF_P))
		trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
	    if (BIF_P->ct != NULL)
	       save_calls(BIF_P, &exp_send);
	    if ((rp = whereis_process(unsigned_val(tp[1]))) == NULL) {
		BIF_RET(BIF_ARG_2);
	    }
	    if (rp->status == P_EXITING) {
		BIF_RET(BIF_ARG_2);
	    }
	    break; /* jump to send_message */
	}

	if (dist_addrs[slot].cid == NIL) {
	    BIF_TRAP2(dsend_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	}
	if (dist_reg_send(BIF_P, slot, tp[1], BIF_ARG_2) == 1) {
	    ASSERT(is_port(dist_addrs[slot].cid));
	    erl_suspend(BIF_P, dist_addrs[slot].cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	if (IS_TRACED(BIF_P))
	    trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
	if (BIF_P->ct != NULL)
	   save_calls(BIF_P, &exp_send);
	BIF_RET(BIF_ARG_2);

    default:
	if (IS_TRACED(BIF_P)) /* XXX Is this really neccessary ??? */
	    trace_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
	if (BIF_P->ct != NULL)
	   save_calls(BIF_P, &exp_send);
	BIF_ERROR(BIF_P, BADARG);
    }
    send_message(BIF_P, rp, BIF_ARG_2);
    BIF_RET2(BIF_ARG_2, rp->msg.len*4);
}


/**********************************************************************/
/*
 * apply/3 is implemented as an instruction and as erlang code in the
 * erlang module.
 *
 * There is only one reason that apply/3 is included in the BIF table:
 * The error handling code in the beam emulator passes the pointer to
 * this function to the error handling code if the apply instruction
 * fails.  The error handling use the function pointer to lookup
 * erlang:apply/3 in the BIF table.
 *
 * This function will never be called.  (It could be if init did something
 * like this:  apply(erlang, apply, [M, F, A]). Not recommended.)
 */

BIF_RETTYPE apply_3(BIF_ALIST_3)
BIF_ADECL_3
{
    BIF_ERROR(BIF_P, BADARG);
}


/* maths abs function */
BIF_RETTYPE abs_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res;
    sint32 i0, i;
    uint32* hp;

    /* integer arguments */
    if (is_small(BIF_ARG_1)) {
	i0 = signed_val(BIF_ARG_1);
	i = abs(i0);
	if (i0 == MIN_SMALL)
	    BIF_RET(make_small_or_big(i, BIF_P));
	else {
	    BIF_RET(make_small(i));
	}
    }
    else if (is_big(BIF_ARG_1)) {
	if (!big_sign(BIF_ARG_1)) {
	    BIF_RET(BIF_ARG_1);
	} else {
	    int sz = big_arity(BIF_ARG_1) + 1;
	    uint32* x;

	    hp = ArithAlloc(BIF_P, sz);	/* See note at beginning of file */
	    sz--;
	    res = make_big(hp);
	    x = ptr_val(BIF_ARG_1);
	    *hp++ = make_thing(sz, POSITIVE_BIG_SUBTAG);
	    x++;                          /* skip thing */
	    while(sz--)
		*hp++ = *x++;
	    BIF_RET(res);
	}
    }
    else if (is_float(BIF_ARG_1)) {
	FloatDef f;

	GET_DOUBLE(BIF_ARG_1, f);
	if (f.fd < 0.0) {
	    hp = ArithAlloc(BIF_P, 3); 	/* See note at beginning of file */
	    f.fd = fabs(f.fd);
	    res = make_float(hp);
	    PUT_DOUBLE(f, hp);
	    BIF_RET(res);
	}
	else
	    BIF_RET(BIF_ARG_1);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* integer to float */
BIF_RETTYPE float_1(BIF_ALIST_1)
BIF_ADECL_1
{
     uint32 res;
     uint32* hp;
     sint32 i;
     FloatDef f;
     
     /* check args */
     if (is_not_integer(BIF_ARG_1)) {
	 if (is_float(BIF_ARG_1)) 
	     BIF_RET(BIF_ARG_1);
	 BIF_ERROR(BIF_P, BADARG);
     }
     if (is_small(BIF_ARG_1)) {
	 i = signed_val(BIF_ARG_1);
	 f.fd = i;		/* use "C"'s auto casting */
     }
     else {
	 if (!FP_PRE_CHECK_OK()) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 f.fd = big_to_double(BIF_ARG_1);
	 if (!FP_RESULT_OK(f.fd)) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
     }
     hp = ArithAlloc(BIF_P, 3);	/* See note at beginning of file */
     res = make_float(hp);
     PUT_DOUBLE(f, hp);
     BIF_RET(res);
}

/**********************************************************************/

/* truncate a float returning an integer */
BIF_RETTYPE trunc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res;
    FloatDef f;
     
    /* check arg */
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* truncate it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd >= 0.0) ? floor(f.fd) : ceil(f.fd));
    BIF_RET(res);
}

/**********************************************************************/

/* round a 'number' to an integer */

BIF_RETTYPE round_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 res;
    FloatDef f;
     
    /* check arg */ 
    if (is_not_float(BIF_ARG_1)) {
	if (is_integer(BIF_ARG_1)) 
	    BIF_RET(BIF_ARG_1);
	BIF_ERROR(BIF_P, BADARG);
    }
     
    /* get the float */
    GET_DOUBLE(BIF_ARG_1, f);

    /* round it and return the resultant integer */
    res = double_to_integer(BIF_P, (f.fd > 0.0) ? f.fd + 0.5 : f.fd - 0.5);
    BIF_RET(res);
}
/**********************************************************************/

/* 
 * Open a port. Most of the work is not done here but rather in
 * the file io.c.
 * Error returns: -1 or -2 returned from open_driver (-2 implies
 * 'errno' contains error code; -1 means we don't really know what happened),
 * -3 if argument parsing failed.
 */
static int open_port(pid, name, settings)
uint32 pid, name, settings;
{
    int i, port_num;
    uint32 option;
    uint32 arity;
    uint32 *tp;
    uint32 *nargs;
    DriverEntry* driver;
    char* name_buf;
    SysDriverOpts opts;
    int binary_io;
    int soft_eof;
    int linebuf;

    /* These are the defaults */
    opts.packet_bytes = 0;
    opts.use_stdio = 1;
    opts.redir_stderr = 0;
    opts.read_write = 0;
    opts.hide_window = 0;
    opts.wd = NULL;
    opts.envir = NULL;
    opts.exit_status = 0;
    binary_io = 0;
    soft_eof = 0;
    linebuf = 0;

    if (is_not_list(settings) && is_not_nil(settings))
	return -3;

    /*
     * Parse the settings.
     */

    if (is_not_nil(settings)) {
	nargs = ptr_val(settings);
	while (1) {
	    if (is_tuple(*nargs)) {
		tp = ptr_val(*nargs);
		arity = *tp++;
		if (arity != make_arityval(2))
		    return -3;
		option = *tp++;
		if (option == am_packet) {
		   if (is_not_small(*tp))
		      return -3;
		   opts.packet_bytes = signed_val(*tp);
		   switch (opts.packet_bytes) {
		    case 1:
		    case 2:
		    case 4:
		      break;
		    default:
		      return -3;
		   }
		} else if (option == am_line) {
		    if (is_not_small(*tp))
			return -3;
		    linebuf = signed_val(*tp);
		    if(linebuf <= 0)
			return -3;
		} else if (option == am_env) {
		   if (is_not_binary(*tp))
		      return -3;
		   opts.envir = (char *) ((ProcBin *) ptr_val(*tp))->bytes;
		} else if (option == am_cd) {
		   if (is_not_binary(*tp))
		      return -3;
		   opts.wd = (char *) ((ProcBin *) ptr_val(*tp))->bytes;
		} else
		   return -3;
	    } else if (*nargs == am_stream) {
		opts.packet_bytes = 0;
	    } else if (*nargs == am_use_stdio) {
		opts.use_stdio = 1;
	    } else if (*nargs == am_stderr_to_stdout) {
		opts.redir_stderr = 1;
	    } else if (*nargs == am_line) {
		linebuf = 512;
	    } else if (*nargs == am_nouse_stdio) {
		opts.use_stdio = 0;
	    } else if (*nargs == am_binary) {
		binary_io = 1;
	    } else if (*nargs == am_in) {
		opts.read_write |= DO_READ;
	    } else if (*nargs == am_out) {
		opts.read_write |= DO_WRITE;
	    } else if (*nargs == am_eof) {
		soft_eof = 1;
	    } else if (*nargs == am_hide) {
		opts.hide_window = 1;
	    } else if (*nargs == am_exit_status) {
		opts.exit_status = 1;
	    } else {
		return -3;
	    }
	    if (is_nil(*++nargs)) 
		break;
	    if (is_not_list(*nargs)) 
		return -3;
	    nargs = ptr_val(*nargs);
	}
    }
    if (opts.read_write == 0)	/* implement default */
	opts.read_write = DO_READ|DO_WRITE;

    /* Mutually exclusive arguments. */
    if((linebuf && opts.packet_bytes) || 
       (opts.redir_stderr && !opts.use_stdio))
	return -3; 

    /*
     * Parse the first argument and start the appropriate driver.
     */

    if (is_atom(name) || is_string(name)) {
	/* a vanilla port */
	if (is_atom(name)) {
	    if (atom_tab(unsigned_val(name))->len >= TMP_BUF_SIZE)
		return -3;
	    sys_memcpy(tmp_buf, atom_tab(unsigned_val(name))->name, 
		       atom_tab(unsigned_val(name))->len);
	    tmp_buf[atom_tab(unsigned_val(name))->len] = '\0';
	} else {
	    i = intlist_to_buf(name, tmp_buf, TMP_BUF_SIZE);
	    tmp_buf[i] = '\0';
	}
	name_buf = tmp_buf;
	driver = &vanilla_driver_entry;
    } else {   
	if (is_not_tuple(name))
	    return -3;		/* Not a process or fd port */
	tp = ptr_val(name);
	arity = *tp++;

	if (*tp == am_spawn) {	/* A process port */
	    if (arity != make_arityval(2)) {
		return -3;
	    }
	    name = tp[1];
	    if (is_atom(name)) {
		if (atom_tab(unsigned_val(name))->len >= TMP_BUF_SIZE)
		    return -3;
		sys_memcpy(tmp_buf, atom_tab(unsigned_val(name))->name,
			   atom_tab(unsigned_val(name))->len);
		tmp_buf[atom_tab(unsigned_val(name))->len] = '\0';
	    } else  if (is_string(name)) {
		 i = intlist_to_buf(name,tmp_buf, TMP_BUF_SIZE);
		 tmp_buf[i] = '\0';
	    } else
		return -3;
	    name_buf = tmp_buf;
	    driver = &spawn_driver_entry;
	} else if (*tp == am_fd) { /* An fd port */
	    int n;
	    char sbuf[16];
	    char* p;

	    opts.ifd = unsigned_val(tp[1]);
	    opts.ofd = unsigned_val(tp[2]);
	    if (arity != make_arityval(3)) {
		return -3;
	    }
	    if (is_not_small(tp[1]) || is_not_small(tp[2])) {
		return -3;
	    }

	    /* Syntesize name from input and output descriptor. */
	    name_buf = tmp_buf;
	    p = int_to_buf(opts.ifd, sbuf);
	    n = sys_strlen(p);
	    sys_strncpy(name_buf, p, n);
	    name_buf[n] = '/';
	    p = int_to_buf(opts.ofd, sbuf);
	    sys_strcpy(name_buf+n+1, p);

	    driver = &fd_driver_entry;
	} else
	    return -3;
    }

    if (driver != &spawn_driver_entry && opts.exit_status)
       return -3;

    if ((port_num = open_driver(driver, pid, name_buf, &opts)) < 0) {
	DEBUGF(("open_driver returned %d\n", port_num));
	return port_num;
    }

    if (binary_io) {
	erts_port[port_num].status |= BINARY_IO;
    }
    if (soft_eof) {
	erts_port[port_num].status |= SOFT_EOF;
    }
    if (linebuf && erts_port[port_num].linebuf == NULL){
	erts_port[port_num].linebuf = allocate_linebuf(linebuf); 
	erts_port[port_num].status |= LINEBUF_IO;
    }

    return port_num;
}

BIF_RETTYPE open_port_prim_2(BIF_ALIST_2)
BIF_ADECL_2
{
    int port_num;
    Eterm port_val;
    int am;
    char *str;

    if ((port_num = open_port(BIF_P->id, BIF_ARG_1, BIF_ARG_2)) < 0) {
       if (port_num == -3) {
	  BIF_ERROR(BIF_P, BADARG);
       }

       if (port_num == -2)
	  str = erl_errno_id(errno);
       else
	  str = "einval";

       am = atom_put(str, strlen(str));

       BIF_P->fvalue = make_atom(am);
       BIF_ERROR(BIF_P, USER_ERROR);
    }

    port_val = erts_port[port_num].id;
    erts_port[port_num].links = new_link(erts_port[port_num].links,LNK_LINK,
					 BIF_P->id, NIL);
    BIF_P->links = new_link(BIF_P->links, LNK_LINK, port_val, NIL);
    BIF_RET(port_val);
}


/**********************************************************************/

/* return the length of a list */

BIF_RETTYPE length_1(BIF_ALIST_1)
BIF_ADECL_1
{
     uint32 list;
     uint32 i;
     
     if (is_nil(BIF_ARG_1)) 
	 BIF_RET(SMALL_ZERO);
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     list = BIF_ARG_1;
     i = 0;
     while (is_list(list)) {
	 i++;
	 list = CDR(ptr_val(list));
     }
     if (is_not_nil(list))  {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(make_small(i));
}

/**********************************************************************/

/* returns the head of a list - this function is unecessary
   and is only here to keep Robert happy (Even more, since it's OP as well) */
BIF_RETTYPE hd_1(BIF_ALIST_1)
BIF_ADECL_1
{
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(CAR(ptr_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the tails of a list - same comment as above */

BIF_RETTYPE tl_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(CDR(ptr_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the size of a tuple or a binary */

BIF_RETTYPE size_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if (is_tuple(BIF_ARG_1)) {
	Eterm* tupleptr = ptr_val(BIF_ARG_1);

	BIF_RET(make_small(arityval(*tupleptr)));
    } else if (is_binary(BIF_ARG_1)) {
	ProcBin* bptr = (ProcBin*) ptr_val(BIF_ARG_1);
	if (thing_subtag(bptr->thing_word) != FUN_SUBTAG) {
	    BIF_RET(make_small_or_big(bptr->size, BIF_P));
	}
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the N'th element of a tuple */

BIF_RETTYPE element_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if (is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_2)) {
	uint32 *tuple_ptr = ptr_val(BIF_ARG_2);
	sint32 ix = signed_val(BIF_ARG_1);

	if ((ix >= 1) && (ix <= arityval(*tuple_ptr)))
	    BIF_RET(tuple_ptr[ix]);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* we have discussed a destructive set elements - maybe someday */
/* set the n'th element in a tuple */

BIF_RETTYPE setelement_3(BIF_ALIST_3)
BIF_ADECL_3
{
    uint32* ptr;
    uint32* hp;
    uint32* resp;
    uint32  ix;
    uint32  size;

    if (is_not_small(BIF_ARG_1) || is_not_tuple(BIF_ARG_2))
	BIF_ERROR(BIF_P, BADARG);
    ptr = ptr_val(BIF_ARG_2);
    ix = signed_val(BIF_ARG_1);
    size = arityval(*ptr) + 1;   /* include arity */
    if ((ix < 1) || (ix >= size))
	BIF_ERROR(BIF_P, BADARG);

    hp = HAlloc(BIF_P, size);

    /* copy the tuple */
    resp = hp;
    while (size--)  /* XXX memcpy ? */
	*hp++ = *ptr++;
    resp[ix] = BIF_ARG_3;
    BIF_RET(make_tuple(resp));
}

/**********************************************************************/

BIF_RETTYPE make_tuple_2(BIF_ALIST_2)
BIF_ADECL_2
{
    int n;
    uint32* hp;
    uint32 res;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, n+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(n);
    while (n--) {
	*hp++ = BIF_ARG_2;
    }
    BIF_RET(res);
}


/**********************************************************************/

BIF_RETTYPE append_element_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32* ptr;
    uint32* hp;
    uint32 arity;
    uint32 res;

    if (is_not_tuple(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = ptr_val(BIF_ARG_1);
    arity = arityval(*ptr);
    hp = HAlloc(BIF_P, arity + 2);
    res = make_tuple(hp);
    *hp = make_arityval(arity+1);
    while (arity--) {
	*++hp = *++ptr;
    }
    *++hp = BIF_ARG_2;
    BIF_RET(res);
}

/**********************************************************************/

/* convert an atom to a list of ascii integer */

BIF_RETTYPE atom_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 need;
    uint32* hp;
    Atom* ap;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
     
    /* read data from atom table */
    ap = atom_tab(unsigned_val(BIF_ARG_1));
    if (ap->len == 0)
	BIF_RET(NIL);	/* the empty atom */
    need = ap->len*2;
    hp = HAlloc(BIF_P, need);
    BIF_RET(buf_to_intlist(&hp,ap->name,ap->len, NIL));
}

/**********************************************************************/

/* convert a list of ascii intgers to an atom */
 
BIF_RETTYPE list_to_atom_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int i;

    if ((i = intlist_to_buf(BIF_ARG_1,tmp_buf,TMP_BUF_SIZE)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    else if (i > MAX_ATOM_LENGTH) {
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    }
    else {
	BIF_RET(am_atom_put(tmp_buf, i));
    }
}


/**********************************************************************/

/* convert an integer to a list of ascii integers */

BIF_RETTYPE integer_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32* hp;
    uint32* hp_end;
    uint32 need;

    if (is_not_integer(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_1)) {
	byte *c;
	int n;
	char ibuf[12];

	c = (byte*) int_to_buf(signed_val(BIF_ARG_1), ibuf);
	n = sys_strlen((char*)c);
	need = 2*n;
	hp = HAlloc(BIF_P, need);
	BIF_RET(buf_to_intlist(&hp, c, n, NIL));
    }
    else {
	int n = big_decimal_estimate(BIF_ARG_1);
	uint32 res;

	need = 2*n;
	hp = HAlloc(BIF_P, need);
	hp_end = hp + need;
	res = big_to_list(BIF_ARG_1, &hp);
#ifdef DEBUG
	while (hp < hp_end) *hp++ = NIL;
#endif
	BIF_RET(res);
    }
}

/**********************************************************************/

/* convert a list of ascii ascii integer value to an integer */

BIF_RETTYPE list_to_integer_1(BIF_ALIST_1)
BIF_ADECL_1
{
     uint32 *pos;
     sint32 i = 0;
     int skip = 0;
     int neg = 0;
     int n = 0;
     int m;
     int lg2;
     uint32 res;
     uint32* hp;
     uint32* hp_end;

     /* must be a list */
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }

     /* if first char is a '-' then it is a negative integer */
     pos = ptr_val(BIF_ARG_1);
     if (*pos == make_small('-')) {
	  neg = 1;
	  skip = 1;
	  if (is_not_list(*(pos + 1))) {
	      BIF_ERROR(BIF_P, BADARG);
	  }
	  pos = ptr_val(*(pos + 1));
     } else {
	  if (*pos == make_small('+')) {
	       /* ignore plus */
	      skip = 1;
	      if (is_not_list(*(pos + 1))) {
		  BIF_ERROR(BIF_P, BADARG);
	      }
	      pos = ptr_val(*(pos + 1));
	  }
      }

     /* Calculate size and do type check */

     while(1) {
	 if (is_not_small(*pos)) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 if (unsigned_val(*pos) < '0' || unsigned_val(*pos) > '9') {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 i = i * 10;
	 i = i + unsigned_val(*pos) - '0';
	 n++;
	 if (*(pos + 1) == NIL) break;
	 if (is_not_list(*(pos + 1))) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 pos = ptr_val(*(pos + 1));
     }

      /* If n <= 8 then we know it's a small int 
      ** since 2^27 = 134217728. If n > 8 then we must
      ** construct a bignum and let that routine do the checking
      */

     if (n <= 8)  /* It must be small */
     {
	 if (neg) i = -i;
	 BIF_RET(make_small(i));
     }

     lg2 =  (n+1)*230/69+1;
     m  = (lg2+D_EXP-1)/D_EXP; /* number of digits */
     m  = ((m+1)>>1) + 1;      /* number of words + thing */

     hp = HAlloc(BIF_P, m);
     hp_end = hp + m;

     pos = ptr_val(BIF_ARG_1);
     if (skip)
	 pos = ptr_val(*(pos+1));

     /* load first digits (at least one digit) */
     if ((i = (n % 4)) == 0)
	 i = 4;
     n -= i;
     m = 0;
     while(i--) {
	 m = 10*m + (unsigned_val(*pos) - '0');
	 pos = ptr_val(*(pos + 1));
     }
     res = small_to_big(m, hp);  /* load first digits */

     while(n) {
	 i = 4;
	 n -= 4;
	 m = 0;
	 while(i--) {
	     m = 10*m + (unsigned_val(*pos) - '0');
	     pos = ptr_val(*(pos+1));
	 }
	 if (is_small(res))
	     res = small_to_big(signed_val(res), hp);
	 res = big_times_small(res, D_DECIMAL_BASE, hp);
	 if (is_small(res))
	     res = small_to_big(signed_val(res), hp);
	 res = big_plus_small(res, m, hp);
     }

     if (is_big(res))  /* check if small */
	 res = big_plus_small(res, 0, hp); /* includes convert to small */

     if (neg) {
	 if (is_small(res))
	     res = make_small(-signed_val(res));
	 else {
	     pos = ptr_val(res); /* point to thing */
	     *pos = set_subtag(*pos, NEGATIVE_BIG_SUBTAG);
	 }
     }


     /* XXX Why this code? */
     if (is_big(res))
	 hp += (big_arity(res)+1);
#ifdef DEBUG	
	while (hp < hp_end) *hp++ = NIL;
#endif     
     BIF_RET(res);
 }

/**********************************************************************/

/* convert a float to a list of ascii characters */

BIF_RETTYPE float_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
     int i;
     uint32 need;
     uint32* hp;
     FloatDef f;
     char fbuf[30];
     
     /* check the arguments */
     if (is_not_float(BIF_ARG_1))
	 BIF_ERROR(BIF_P, BADARG);
     GET_DOUBLE(BIF_ARG_1, f);
     if ((i = sys_double_to_chars(f.fd, fbuf)) <= 0)
	 BIF_ERROR(BIF_P, INTERNAL_ERROR);
     need = i*2;
     hp = HAlloc(BIF_P, need);
     BIF_RET(buf_to_intlist(&hp, fbuf, i, NIL));
 }

/**********************************************************************/

/* convert a list of ascii  integer values e's +'s and -'s to a float */

BIF_RETTYPE list_to_float_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int i;
    FloatDef f;
    uint32 res;
    uint32* hp;

    if ((i = intlist_to_buf(BIF_ARG_1,tmp_buf,TMP_BUF_SIZE-1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    tmp_buf[i] = '\0';		/* null terminal */

    if (!FP_PRE_CHECK_OK())
	BIF_ERROR(BIF_P, BADARG);

    if (sys_chars_to_double((char*)tmp_buf, &f.fd) != 0)
	BIF_ERROR(BIF_P, BADARG);

    if (FP_RESULT_OK(f.fd)) {
	hp = HAlloc(BIF_P, 3);

	res = make_float(hp);
	PUT_DOUBLE(f, hp);
	BIF_RET(res);
    }
    else
	BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/

/* convert a tuple to a list */

BIF_RETTYPE tuple_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 n;
    uint32 *tupleptr;
    uint32 list = NIL;
    uint32* hp;

    if (is_not_tuple(BIF_ARG_1))  {
	BIF_ERROR(BIF_P, BADARG);
    }

    tupleptr = ptr_val(BIF_ARG_1);
    n = arityval(*tupleptr);
    hp = HAlloc(BIF_P, 2 * n);
    tupleptr++;

    while(n--) {
	list = CONS(hp, tupleptr[n], list);
	hp += 2;
    }
    BIF_RET(list);
}

/**********************************************************************/

/* convert a list to a tuple */

BIF_RETTYPE list_to_tuple_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32  list = BIF_ARG_1;
    uint32* cons;
    uint32 res;
    uint32* hp;
    int len;

    if ((len = list_length(list)) < 0)
	BIF_ERROR(BIF_P, BADARG);

    hp = HAlloc(BIF_P, len+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(len);
    while(is_list(list)) {
	cons = ptr_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    BIF_RET(res);
}

/**********************************************************************/

/* return the pid of our own process, in most cases this has been replaced by
   a machine instruction */

BIF_RETTYPE self_0(BIF_ALIST_0)
BIF_ADECL_0
{
     BIF_RET(BIF_P->id);
     
}

/**********************************************************************/

/*
   New representation of refs in R6.

   A ref is now a memory object, with the layout

#define REF_WORDS	3

typedef struct {
    uint32 t;
    uint32 h;
    uint32 w[REF_WORDS];
} Ref;

where the actual object in memory may use less than REF_WORDS words in w.

t is a header word, containing size info (a 'thing' word).
h contains node and creation info.

w[0] is the least significant part (changes most rapidly when creating
new refs).

In the external format, a new tag NEW_REFERENCE_EXT is used, which has
this layout:

  1      2       N            1           N'
+-----+-----+-----------+-----------+-----------+
| 114 | Len |    Node   | Creation  |   ID  ... |
+-----+-----+-----------+-----------+-----------+

Node and Creation as in the old REFERENCE_EXT case.
Len is the length in bytes of Node+Creation+ID.
N' is a multiple of 4.
The words of w are laid out in ID, 4 bytes at a time.

In w[0], only the usual 18 bits are used. Ordinarily, in "long refs"
all words are used (in other words, practically never wrap around), but
for compatibility with older nodes, "short refs" exist. Short refs
come into being by being converted from the old external format for
refs (tag REFERENCE_EXT). Short refs are converted back to the old
external format.
When converting a long ref to the external format in the case of preparing
for sending to an older node, the ref is truncated by only using
the first word (with 18 significant bits), and using the old tag
REFERENCE_EXT.

When comparing refs or different size, only the parts up to the length
of the shorter operand are used. This has the desirable effect that a
long ref sent to an old node and back will be treated as equal to
the original, although some of the bits have been lost.

The hash value for a ref always considers only the first word, since
in the above scenario, the original and the copy should have the same
hash value.

*/

static uint32 reference = 0;   /* XXX fix proper init */
static uint32 reference1 = 0;
static uint32 reference2 = 0;

/* For internal use */
static uint32 make_ref(Process *p)
{
    uint32 *hp;
    uint32 t;

    reference++;
    if (reference >= MAX_REFERENCE) {
	reference = 0;
	reference1++;
	if (reference1 == 0) {
	    reference2++;
	}
    }

    hp = HAlloc(p, REF_WORDS + 2);
    hp[0] = make_thing(REF_WORDS + 1, REF_THING_SUBTAG);
    hp[1] = make_refer2(THIS_NODE,0);
    hp[2] = reference;
    hp[3] = reference1;
    hp[4] = reference2;
    t = make_refer(hp);
    return t;
}

BIF_RETTYPE make_ref_0(BIF_ALIST_0)
BIF_ADECL_0
{
    uint32 r;

    r = make_ref(BIF_P);
    BIF_RET(r);
}

/**********************************************************************/

/* return the time of day */

BIF_RETTYPE time_0(BIF_ALIST_0)
BIF_ADECL_0
{
     int hour, minute, second;
     uint32* hp;

     get_time(&hour, &minute, &second);
     hp = HAlloc(BIF_P, 4);	/* {hour, minute, second}  + arity */
     BIF_RET(TUPLE3(hp, make_small(hour), make_small(minute),
		    make_small(second)));
}
/**********************************************************************/

/* return the date */

BIF_RETTYPE date_0(BIF_ALIST_0)
BIF_ADECL_0
{
     int year, month, day;
     uint32* hp;
     
     get_date(&year, &month, &day);
     hp = HAlloc(BIF_P, 4);	/* {year, month, day}  + arity */
     BIF_RET(TUPLE3(hp, make_small(year), make_small(month), make_small(day)));
}

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_0(BIF_ALIST_0)
BIF_ADECL_0
{
     int year, month, day;
     int hour, minute, second;
     uint32 res1, res2;
     uint32* hp;

     /* read the clock */
     get_universaltime(&year, &month, &day, &hour, &minute, &second);

     hp = HAlloc(BIF_P, 4+4+3);

     /* and return the tuple */
     res1 = TUPLE3(hp,make_small(year),make_small(month),make_small(day));
     hp += 4;
     res2 = TUPLE3(hp,make_small(hour),make_small(minute),make_small(second));
     hp += 4;
     BIF_RET(TUPLE2(hp, res1, res2));
 }

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE localtime_0(BIF_ALIST_0)
BIF_ADECL_0
{
     int year, month, day;
     int hour, minute, second;
     uint32 res1, res2;
     uint32* hp;

     /* read the clock */
     get_localtime(&year, &month, &day, &hour, &minute, &second);

     hp = HAlloc(BIF_P, 4+4+3);

     /* and return the tuple */
     res1 = TUPLE3(hp,make_small(year),make_small(month),make_small(day));
     hp += 4;
     res2 = TUPLE3(hp,make_small(hour),make_small(minute),make_small(second));
     hp += 4;
     BIF_RET(TUPLE2(hp, res1, res2));
}
/**********************************************************************/

/* type check and extract components from a tuple on form: {{Y,M,D},{H,M,S}} */
static int time_to_parts(date, year, month, day, hour, minute, second)
uint32 date;
int* year; int* month; int* day;
int* hour;int* minute; int* second;
{
    uint32 *t1;
    uint32 *t2;

    if (is_not_tuple(date))
	return 0;
    t1 = ptr_val(date);
    if (arityval(t1[0]) !=2 || 
	is_not_tuple(t1[1]) || is_not_tuple(t1[2]))
	return 0;
    t2 = ptr_val(t1[1]);
    t1 = ptr_val(t1[2]);
    if (arityval(t2[0]) != 3 || 
	is_not_small(t2[1]) || is_not_small(t2[2]) || is_not_small(t2[3]))
	return 0;
    *year  = signed_val(t2[1]);
    *month = signed_val(t2[2]);
    *day   = signed_val(t2[3]);
    if (arityval(t1[0]) != 3 || 
	is_not_small(t1[1]) || is_not_small(t1[2]) || is_not_small(t1[3]))
	return 0;
    *hour   = signed_val(t1[1]);
    *minute = signed_val(t1[2]);
    *second = signed_val(t1[3]);
    return 1;
}


/* return the universal time */

BIF_RETTYPE localtime_to_universaltime_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int year, month, day;
    int hour, minute, second;
    uint32 res1, res2;
    uint32* hp;

    if (!time_to_parts(BIF_ARG_1, &year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    if (!local_to_univ(&year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    
    hp = HAlloc(BIF_P, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
 }
	 

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_to_localtime_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int year, month, day;
    int hour, minute, second;
    uint32 res1, res2;
    uint32* hp;

    if (!time_to_parts(BIF_ARG_1, &year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    if (!univ_to_local(&year, &month, &day, 
		       &hour, &minute, &second))
	BIF_ERROR(BIF_P, BADARG);
    
    hp = HAlloc(BIF_P, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
}

/**********************************************************************/


 /* return a timestamp */
BIF_RETTYPE now_0(BIF_ALIST_0)
BIF_ADECL_0
{
    uint32 megasec, sec, microsec;
    uint32* hp;

    get_now(&megasec, &sec, &microsec);
    hp = HAlloc(BIF_P, 4);
    BIF_RET(TUPLE3(hp,make_small(megasec),make_small(sec),
		   make_small(microsec)));
}

/**********************************************************************/

BIF_RETTYPE garbage_collect_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Process *rp;

    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    } else if (BIF_P->id == BIF_ARG_1) {
	FORCE_GC(BIF_P);
	BUMP_ALL_REDS(BIF_P);
    } else if ((rp = pid2proc(BIF_ARG_1)) == NULL) {
	BIF_RET(am_false);
    } else {
	erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
    }
    BIF_RET(am_true);
}

BIF_RETTYPE garbage_collect_0(BIF_ALIST_0)
BIF_ADECL_0
{
    FORCE_GC(BIF_P);
    BUMP_ALL_REDS(BIF_P);
    BIF_RET(am_true);
}

/**********************************************************************/

/* returns a list of the active processes in the system */
/* scans the whole of the process table */

BIF_RETTYPE processes_0(BIF_ALIST_0)
BIF_ADECL_0
{
    int i;
    int need = 0;
    uint32 res = NIL;
    uint32* hp;
    Process *p;
     
    /* first work out how many processes there are */
    for (i = 0; i < max_process; i++)
	if ((p = process_tab[i]) != NULL)
	    need += 2;
     
    hp = HAlloc(BIF_P, need);     /* we need two heap words for each pid */

    /* make the list by scanning again (bakward) */
    for (i = max_process-1; i >= 0; i--) {
	if ((p = process_tab[i]) != NULL) {
	    res = CONS(hp, process_tab[i]->id, res);
	    hp += 2;
	}
    }
    BIF_RET(res);
}

/**********************************************************************/

BIF_RETTYPE is_process_alive_1(BIF_ALIST_1) 
BIF_ADECL_1
{
   Process *rp;
   int i;
   uint32 pid = BIF_ARG_1;

   if (is_not_pid(pid) || (get_node(pid) != THIS_NODE) ||
       (get_number(pid) >= max_process)) {
      BIF_ERROR(BIF_P, BADARG);
   }

   i = get_creation(pid);
   if ((i != this_creation) && (i != 0))
      BIF_RET(am_false);

   rp = process_tab[get_number(BIF_ARG_1)];

   if (INVALID_PID(rp, BIF_ARG_1))
   {
      BIF_RET(am_false);
   }
   BIF_RET(am_true);
}

BIF_RETTYPE process_display_2(BIF_ALIST_2)
BIF_ADECL_2
{
   Process *rp;
   uint32 pid = BIF_ARG_1;

   if (is_not_pid(pid) || (get_node(pid) != THIS_NODE) ||
       (get_number(pid) >= max_process)) {
      BIF_ERROR(BIF_P, BADARG);
   }

   if (BIF_ARG_2 != am_backtrace)
      BIF_ERROR(BIF_P, BADARG);

   rp = process_tab[get_number(BIF_ARG_1)];
   if (rp == NULL) {
       BIF_ERROR(BIF_P, BADARG);
   }
   stack_dump2(rp, CERR);
   BIF_RET(am_true);
}

/**********************************************************************/
/* Return a list of active ports */

BIF_RETTYPE ports_0(BIF_ALIST_0)
BIF_ADECL_0
{
    int i;
    int need = 0;
    uint32 res = NIL;
    uint32* hp;
     
    /* first work out how many processes there are */
    for (i = 0; i < erl_max_ports; i++) {
	if (erts_port[i].status != FREE)
	    need += 2;
    }
    hp = HAlloc(BIF_P, need);

    for (i = erl_max_ports-1; i >= 0; i--) {
	if (erts_port[i].status != FREE) {
	    res = CONS(hp, erts_port[i].id, res);
	    hp += 2;
	}
    }
    BIF_RET(res);
}

/**********************************************************************/

BIF_RETTYPE throw_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, THROWN);
}

/**********************************************************************/


/* 
 * Non-standard, undocumented, dirty BIF, meant for debugging.
 *
 */
BIF_RETTYPE display_1(BIF_ALIST_1)
BIF_ADECL_1
{
    display(BIF_ARG_1, COUT);
    erl_putc('\r', COUT);
    erl_putc('\n', COUT);
    BIF_RET(am_true);
}

Eterm
display_string_1(Process* p, Eterm string)
{
    int len;

    if (!is_string(string)) {
	BIF_ERROR(p, BADARG);
    }
    if ((len = intlist_to_buf(string, tmp_buf, TMP_BUF_SIZE-1)) < 0) {
	BIF_ERROR(p, BADARG);
    }
    tmp_buf[len] = '\0';
    erl_printf(CERR, "%s", tmp_buf);
    BIF_RET(am_true);
}

Eterm
display_nl_0(Process* p)
{
    erl_putc('\r', CERR);
    erl_putc('\n', CERR);
    BIF_RET(am_true);
}

/**********************************************************************/

/* stop the system */
/* ARGSUSED */
BIF_RETTYPE halt_0(BIF_ALIST_0)
BIF_ADECL_0
{
    VERBOSE(erl_printf(COUT, "System halted by BIF halt/0\n"););
    erl_exit(0, "");
    return 0;  /* Pedantic (lint does not know about erl_exit) */
}

/**********************************************************************/

#define MSG_SIZE	200

/* stop the system with exit code */
/* ARGSUSED */
BIF_RETTYPE halt_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int code;
    static char msg[MSG_SIZE];
    int i;

    if (is_small(BIF_ARG_1) && (code = signed_val(BIF_ARG_1)) >= 0) {
       VERBOSE(erl_printf(COUT, "System halted by BIF halt(%d)\n", code););
       erl_exit(-code, "");
    }
    else if (is_string(BIF_ARG_1) || BIF_ARG_1 == NIL) {
       if ((i = intlist_to_buf(BIF_ARG_1, msg, MSG_SIZE-1)) < 0)
	  BIF_ERROR(BIF_P, BADARG);
       msg[i] = '\0';
       VERBOSE(erl_printf(COUT, "System halted by BIF halt(%s)\n", msg););
       erl_exit(127, "%s\n", msg);
    }
    else
       BIF_ERROR(BIF_P, BADARG);

    return 0;  /* Pedantic (lint does not know about erl_exit) */
}

BIF_RETTYPE function_exported_3(BIF_ALIST_3)
BIF_ADECL_3
{
    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) || 
	is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (erts_find_function(BIF_ARG_1, BIF_ARG_2, signed_val(BIF_ARG_3)) == NULL) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

/**********************************************************************/    

BIF_RETTYPE is_builtin_3(Process* p, Eterm Mod, Eterm Name, Eterm Arity)
{
    if (is_not_atom(Mod) || is_not_atom(Name) || is_not_small(Arity)) {
	BIF_ERROR(p, BADARG);
    }
    BIF_RET(erts_is_builtin(Mod, Name, signed_val(Arity)) ?
	    am_true : am_false);
}

/**********************************************************************/    

BIF_RETTYPE port_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
   int n;
   char* tmpp;
   uint32 obj = BIF_ARG_1;
   uint32* hp;

   tmpp = (char*) tmp_buf;

   if (is_port(obj)) {
      int i, ix, node;

      node = get_node_port(obj);
      ix = get_number_port(obj);
      i = get_creation(obj);
      sprintf(tmpp, "#Port<%d.%d>", node, ix);
   } else
      BIF_ERROR(BIF_P, BADARG);

   n = strlen(tmp_buf);
   hp = HAlloc(BIF_P, n*2);	/* we need length * 2 heap words */
   BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
}

BIF_RETTYPE ref_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
   int n;
   char* tmpp;
   uint32 obj = BIF_ARG_1;
   uint32* hp;
   int i;

   tmpp = (char*) tmp_buf;

   if (is_refer(obj)) {
      sprintf(tmpp, "#Ref<%ld",
	      get_node_reference(obj));
      tmpp = strchr(tmpp, '\0');
      for (i = refer_arity(obj)-2; i >= 0; i--) {
	  sprintf(tmpp, ".%lu",
		  ref_ptr(obj)->w[i]);
	  tmpp = strchr(tmpp, '\0');
      }
      sprintf(tmpp, ">");
   } else
      BIF_ERROR(BIF_P, BADARG);

   n = strlen(tmp_buf);
   hp = HAlloc(BIF_P, n*2);	/* we need length * 2 heap words */
   BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
}

Eterm
fun_to_list_1(Process* p, Eterm fun)
{
    Eterm mod;			/* Module that fun is defined in. */
    int index;			/* Index for fun. */
    int uniq;			/* Unique number for fun. */
    char* tmpp = (char *) tmp_buf;
    Atom* ap;
    Eterm* hp;
    Eterm* tp;
    int len;

    if (is_binary(fun) && thing_subtag(*ptr_val(fun)) == FUN_SUBTAG) {
	ErlFunThing* funp = (ErlFunThing *) ptr_val(fun);

	mod = make_atom(funp->modp->module);
	index = funp->index;
	uniq = unsigned_val(funp->uniq);

#if defined(ALLOW_FUN_TUPLES)
    build_list:
#endif
	strcpy(tmpp, "#Fun<");
	tmpp += strlen(tmpp);
	ap = atom_tab(unsigned_val(mod));
	sys_memcpy(tmpp, ap->name, ap->len);
	tmpp += ap->len;
	sprintf(tmpp, ".%d.%d>", index, uniq);
	len = strlen(tmp_buf);
	hp = HAlloc(p, 2*len);
	return buf_to_intlist(&hp, tmp_buf, len, NIL);
    }
#if defined(ALLOW_FUN_TUPLES)
    else if (is_tuple(fun) && ((tp = ptr_val(fun)), *tp = make_arityval(5)) &&
	     tp[1] == am_fun) {
	mod = tp[2];
	index = unsigned_val(tp[3]);
	uniq = unsigned_val(tp[4]);
	goto build_list;
    }
#endif
    BIF_ERROR(p, BADARG);
}

/**********************************************************************/    

/* convert a pid to an erlang list (for the linked cons cells) of the form
   %node.serial.number% to a PID
 */

BIF_RETTYPE pid_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int n;
    char* cp;
    char* tmpp;
    char ibuf[12];
    uint32 pid = BIF_ARG_1;
    uint32* hp;

    /* check arg */
    if (is_not_pid(pid)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    tmpp = (char*) tmp_buf;
    *tmpp++ = '<';
    
    /* node part */
    cp = int_to_buf(get_node(pid), ibuf);
    n = sys_strlen(cp);
    sys_strncpy(tmpp, cp, n);
    tmpp += n;
    *tmpp++ = '.';

    /* number part */
    cp = int_to_buf(get_number(pid), ibuf);
    n = sys_strlen(cp);
    sys_strncpy(tmpp, cp, n);
    tmpp += n;
    *tmpp++ = '.';

    /* serial part */
    cp = int_to_buf(get_serial(pid), ibuf);
    n = sys_strlen(cp);
    sys_strncpy(tmpp, cp, n);
    tmpp += n;
    *tmpp++ = '>';
    *tmpp = '\0';

    n = tmpp - (char*)tmp_buf;  /* total length */
    hp = HAlloc(BIF_P, n*2);    /* we need length * 2 heap words */
    BIF_RET(buf_to_intlist(&hp, tmp_buf, n, NIL));
}

/**********************************************************************/

/* convert a list of ascii characeters of the form
   <node.serial.number> to a PID
*/

BIF_RETTYPE list_to_pid_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 a = 0, b = 0, c = 0;
    char* cp;
    int i;

    /* walk down the list and create a C string */
    if ((i = intlist_to_buf(BIF_ARG_1,tmp_buf,TMP_BUF_SIZE-1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    tmp_buf[i] = '\0';		/* null terminal */

    cp = (char*) tmp_buf;
    if (*cp++ != '<') goto bad;
    
    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { a = 10*a + (*cp - '0'); cp++; }

    if (*cp++ != '.') goto bad;

    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { b = 10*b + (*cp - '0'); cp++; }

    if (*cp++ != '.') goto bad;

    if (*cp < '0' || *cp > '9') goto bad;
    while(*cp >= '0' && *cp <= '9') { c = 10*c + (*cp - '0'); cp++; }

    if (*cp++ != '>') goto bad;
    if (*cp != '\0') goto bad;

    /* <a.b.c> a = node, b = process number, c = serial */

    /* bounds check */
    /* NOT max_process here !!! */

    if ((b >= MAX_PROCESS) || (c >= MAX_SERIAL) || (a >= (1 << P_NODE)) ||
	( (a != THIS_NODE) && (dist_addrs[a].cid == NIL))) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_pid(c, a, b));

 bad:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* convert an object to an array of bytes */

BIF_RETTYPE term_to_binary_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int size;
    uint32 bin;
    ProcBin* pb;
    byte *c;

    size = encode_size_struct(BIF_ARG_1, TERM_TO_BINARY_DFLAGS);
    bin = new_binary(BIF_P, (byte *)NULL, size);
    pb = (ProcBin *) ptr_val(bin);
    c = pb->bytes;
    to_external(-1, BIF_ARG_1, &c);
    if (c > (pb->bytes + size))
	erl_exit(1, "Internal error in term_to_binary %d\n",(c-pb->bytes));
    /* adjust since encode_size_struct will return more than needed */
    pb->size = c - pb->bytes;
    BIF_RET(bin);
}

/**********************************************************************/

BIF_RETTYPE binary_to_term_1(BIF_ALIST_1)
BIF_ADECL_1
{
    ProcBin* bp;
    byte* c;
    int i;
    Eterm res;
    Eterm* hp;
    Eterm* endp;

    if (is_not_binary(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }

    bp = (ProcBin*) ptr_val(BIF_ARG_1);
    if (thing_subtag(bp->thing_word) == FUN_SUBTAG) {
	goto error;
    }

    if ((i = decode_size(bp->bytes, bp->size)) < 0) {
	goto error;
    }

    hp = HAlloc(BIF_P, i);
    endp = hp + i;
    c = bp->bytes;
    if ((res = from_external(-1, &hp, &c, &BIF_P->off_heap)) == 0) {
	goto error;
    }
    if (hp > endp) {
	erl_exit(1, "binary_to_term_1: buffer overrun by %d byte(s)\n", hp-endp);
    }
    return res;
}

/**********************************************************************/

/*
 * This function is obsolete. Retained for backward compatibility.
 */
  
BIF_RETTYPE old_binary_to_term_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return binary_to_term_1(BIF_P, BIF_ARG_1);
}


/**********************************************************************/

BIF_RETTYPE group_leader_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RET(BIF_P->group_leader);
}

/**********************************************************************/
/* arg1 == leader, arg2 == new member */

BIF_RETTYPE group_leader_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Process* new_member;
    int code;
    int slot;

    if (is_not_pid(BIF_ARG_1) || is_not_pid(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if ((slot = get_node(BIF_ARG_2)) != THIS_NODE) {
	if (dist_addrs[slot].cid == NIL)
	    BIF_TRAP2(dgroup_leader_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	if ((code = dist_group_leader(slot, BIF_ARG_1, BIF_ARG_2)) == 1) {
	    ASSERT(is_port(dist_addrs[slot].cid)); 
	    erl_suspend(BIF_P, dist_addrs[slot].cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	else if (code < 0) {  /* XXX is this the correct behaviour ??? */
	    BIF_ERROR(BIF_P, NOTALIVE);
	}
	BIF_RET(am_true);
    }
    if ((new_member = pid2proc(BIF_ARG_2)) == NULL) {
	BIF_ERROR(BIF_P, BADARG);
    }
    new_member->group_leader = BIF_ARG_1;
    BIF_RET(am_true);
}
    
BIF_RETTYPE system_flag_2(BIF_ALIST_2)    
BIF_ADECL_2
{
    int n;

    if (BIF_ARG_1 == am_keep_zombies) {
       int oval, nval;

       if (is_small(BIF_ARG_2)) {
	   nval = signed_val(BIF_ARG_2);
	   if (keep_zombies(-nval, &oval))
	       BIF_RET(make_small(-oval));
       }
   error:
       BIF_ERROR(BIF_P, BADARG);
    } else if (BIF_ARG_1 == am_min_heap_size) {
	int oval = H_MIN_SIZE;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	H_MIN_SIZE = next_heap_size(n, 0);
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_max_tables) {
	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_max_processes) {
	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_garbage_collection) {
	if (BIF_ARG_2 == am_generational) {
	    switch_gc_threshold = 0;
	} else if (BIF_ARG_2 == am_fullsweep) {
	    switch_gc_threshold = MAX_SMALL;
	} else if (is_small(BIF_ARG_2) && (n = signed_val(BIF_ARG_2)) > 0) {
	    switch_gc_threshold = n;
	}
	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_display_items) {
	int oval = display_items;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	display_items = n < 32 ? 32 : n;
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_debug_flags) {
	BIF_RET(am_true);
    } else if (BIF_ARG_1 == am_backtrace_depth) {
	int oval = erts_backtrace_depth;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	erts_backtrace_depth = n;
	BIF_RET(make_small(oval));
    }
    else if (BIF_ARG_1 == am_sequential_tracer) {
        Eterm old_value;

	if (is_pid(system_seq_tracer) || is_port(system_seq_tracer)) {
	    old_value = system_seq_tracer;
	} else {
	    old_value = am_false;
	}

	if (BIF_ARG_2 == am_false) {
	    system_seq_tracer = NIL;
	    BIF_RET(old_value);
	} else if (is_pid(BIF_ARG_2) && (get_node(BIF_ARG_2) == THIS_NODE)) {
	    /* must be a local PID */
	    Process* tracer = process_tab[get_number(BIF_ARG_2)];
	    if (!INVALID_PID(tracer, BIF_ARG_2)) {
		system_seq_tracer = BIF_ARG_2;
		BIF_RET(old_value);
	    }
	} else if (is_port(BIF_ARG_2)) {
	    system_seq_tracer = BIF_ARG_2;
	    BIF_RET(old_value);
	}
    }
    else if (BIF_ARG_1 == make_small(1)) {
	uint32 i;
	ErlMessage* mp;
	for (i = 0; i < max_process; i++) {
	    if (process_tab[i] != (Process*) 0) {
		Process* p = process_tab[i];
		p->seq_trace_token = NIL;
		p->seq_trace_clock = 0;
		p->seq_trace_lastcnt = 0;
		mp = p->msg.first;
		while(mp != NULL) {
		    mp->seq_trace_token = NIL;
		    mp = mp->next;
		}
	    }
	}
	BIF_RET(am_true);    
    }
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE system_info_1(BIF_ALIST_1)    
BIF_ADECL_1
{
    Eterm res;
    Eterm* hp;
    Eterm val;

    if (BIF_ARG_1 == am_sequential_tracer) {
	if (is_pid(system_seq_tracer) || is_port(system_seq_tracer)) {
	    val = system_seq_tracer;
	} else {
	    val = am_false;
	}
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_sequential_tracer, val);
	BIF_RET(res);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE hash_2(BIF_ALIST_2)
BIF_ADECL_2
{
    uint32 hash;
    sint32 range;

    if (is_not_small(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((range = signed_val(BIF_ARG_2)) <= 0) {  /* [1..MAX_SMALL] */
	BIF_ERROR(BIF_P, BADARG);
    }
    hash = make_hash(BIF_ARG_1, 0);
    BIF_RET(make_small(1 + (hash % range)));   /* [1..range] */
}

/**********************************************************************/

/* XXX   Rewrite this !!!! */
BIF_RETTYPE float_to_words_1(BIF_ALIST_1)
BIF_ADECL_1
{
    uint32 e1,e2;
    uint32* hp;
    FloatDef f;

    if (is_not_float(BIF_ARG_1)) 
	BIF_ERROR(BIF_P, BADARG);
    
    GET_DOUBLE(BIF_ARG_1, f);
    
    hp = HAlloc(BIF_P, 3+5+5);
    
    if (abs(f.fw[0]) > MAX_SMALL) {
	e1 = small_to_big(f.fw[0], hp);
	hp += big_arity(e1)+1;
    }
    else
	e1 = make_small(f.fw[0]);

    if (abs(f.fw[1]) > MAX_SMALL) {
	e2 = small_to_big(f.fw[1], hp);
	hp += big_arity(e2)+1;
    }
    else
	e2 = make_small(f.fw[1]);

    BIF_RET(TUPLE2(hp, e1, e2));
}

/****************************************************************************

  PORT BIFS:

           port_command/2   -- replace Port ! {..., {command, Data}}
               port_command(Port, Data) -> true
               when port(Port), io-list(Data)

           port_control/3   -- new port_control(Port, Ctl, Data) -> Reply
	      port_control(Port, Ctl, Data) -> Reply
              where integer(Ctl), io-list(Data), io-list(Reply)

           port_close/1     -- replace Port ! {..., close}
             port_close(Port) -> true
             when port(Port)

           port_connect/2   -- replace Port ! {..., {connect, Pid}}
              port_connect(Port, Pid) 
              when port(Port), pid(Pid)

 ***************************************************************************/

static Port* id2port(id)
uint32 id;
{
    int ix;

    if (is_not_port(id) || (get_node_port(id) != THIS_NODE))
	return NULL;
    ix = get_port_index(id);
    if ((erts_port[ix].status == FREE) || (erts_port[ix].status & CLOSING))
	return NULL;
    return &erts_port[ix];
}


BIF_RETTYPE port_command_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Port* p;

    if ((p = id2port(BIF_ARG_1)) == NULL)
	BIF_ERROR(BIF_P, BADARG);

    if (p->status & PORT_BUSY) {
	erl_suspend(BIF_P, BIF_ARG_1);
	BIF_ERROR(BIF_P, RESCHEDULE);
    }

    if (write_port(get_port_index(BIF_ARG_1), BIF_ARG_2) != 0)
    {
       BIF_ERROR(BIF_P, BADARG);
    }

    if (BIF_P->status == P_EXITING) {
       KILL_CATCHES(BIF_P);	/* Must exit */
       BIF_ERROR(BIF_P, USER_ERROR);
    }
    BIF_RET(am_true);
}


BIF_RETTYPE port_control_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Port* p;
    int op;
    uint32 res;

    if ((p = id2port(BIF_ARG_1)) == NULL) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_not_small(BIF_ARG_2))
	goto error;
    op = signed_val(BIF_ARG_2);
    
    if (port_control(BIF_P, p, op, BIF_ARG_3, &res) == 0)
	goto error;
    BIF_RET(res);
}

BIF_RETTYPE port_close_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Port* p;
    if ((p = id2port(BIF_ARG_1)) == NULL)
	BIF_ERROR(BIF_P, BADARG);
    do_exit_port(BIF_ARG_1, p->connected, am_normal);
    /* since we terminate port with reason normal 
       we SHOULD never get an exit signal out selfs !!!
       */
    BIF_RET(am_true);
}


BIF_RETTYPE port_connect_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Port* p;
    if (is_not_pid(BIF_ARG_2) || (p = id2port(BIF_ARG_1)) == NULL)
	BIF_ERROR(BIF_P, BADARG);
    p->connected = BIF_ARG_2;
    BIF_RET(am_true);
}

/****************************************************************************
** BIF Timer support
****************************************************************************/

typedef struct bif_timer_rec {
    struct bif_timer_rec* next;
    ErlTimer tm;
    ErlHeapFragment* bp;
    uint32 message;
    uint32 pid;
    Ref ref;
} BifTimerRec;

#define TIMER_HASH_VEC 3331

static BifTimerRec* bif_tm_vec[TIMER_HASH_VEC];  /* XXX fix proper init */

static BifTimerRec** find_timer(ref)
Ref *ref;
{
    uint32 wd = ref->w[0];
    int ix = wd % TIMER_HASH_VEC;
    BifTimerRec** tp = &bif_tm_vec[ix];

    while(*tp != NULL) {
	if (eqref(ref, &(*tp)->ref))
	    return tp;
	tp = &(*tp)->next;
    }
    return NULL;
}

static void bif_timeout_proc(btm)
BifTimerRec* btm;
{
    Process* rp;
    BifTimerRec** tp = find_timer(&btm->ref);
    int invalid_pid;

    ASSERT((tp != NULL) && (*tp == btm));
    *tp = btm->next;

    if (is_atom(btm->pid))
    {
       rp = whereis_process(unsigned_val(btm->pid));
       invalid_pid = (rp == NULL);
    }
    else
    {
       rp = process_tab[get_number(btm->pid)];
       invalid_pid = (INVALID_PID(rp, btm->pid));
    }

    if (invalid_pid)
       free_message_buffer(btm->bp);
    else
       queue_message(rp, btm->bp, btm->message);

    sys_free(btm);
}

/* tm arg contains the BifTimerRec */
static void bif_cancel_proc(btm)
BifTimerRec* btm;
{
    free_message_buffer(btm->bp);
    sys_free(btm);
}

static int term_to_unsigned(uint32 term, unsigned *u)
{
   int i;

   if (is_small(term))
   {
      i = signed_val(term);
      if (i < 0)
	 return 0;
      *u = i;
      return 1;
   }
   else if (is_big(term))
      return big_to_unsigned(term, u);
   else
      return 0;
}

static BifTimerRec *do_timer(pack, process, arg1, arg2, arg3)
int pack;
Process *process;
uint32 arg1, arg2, arg3;
{
    BifTimerRec* btm;
    ErlHeapFragment* bp;
    unsigned timeout;
    uint32 size;
    uint32 term, msg;
    uint32 ref;
    uint32* hp;
    int ix;

    if (!term_to_unsigned(arg1, &timeout))
       return NULL;
    if (is_not_pid(arg2) && is_not_atom(arg2))
       return NULL;
    if (is_pid(arg2) && get_node(arg2) != THIS_NODE)
       return NULL;

    ref = make_ref(process);
    msg = arg3;

    if (pack) {
       hp = HAlloc(process, 4);
       term = TUPLE3(hp, am_timeout, ref, msg);
    }
    else
       term = msg;

    size = size_object(term);
    btm = (BifTimerRec*) safe_alloc(sizeof(BifTimerRec));
    btm->bp = bp = new_message_buffer(size);
    sys_memcpy(&btm->ref, ptr_val(ref), sizeof(btm->ref));
    btm->pid = arg2;
    ix = get_number_reference(ref) % TIMER_HASH_VEC;
    btm->next = bif_tm_vec[ix];
    bif_tm_vec[ix] = btm;
    hp = bp->mem;
    btm->message = copy_struct(term, size, &hp, &bp->off_heap);
    btm->tm.active = 0; /* MUST be initalized */
    erl_set_timer(&btm->tm,
		  bif_timeout_proc,
		  bif_cancel_proc,
		  (void*)btm,
		  timeout);

    return btm;
}

static Eterm
copy_ref(Process *p, Ref *ref0)
{
    Eterm* hp;
    Eterm ref, ref1;
    uint32 size;

    ref1 = make_refer(ref0);
    size = size_object(ref1);

    hp = HAlloc(p, size);
    ref = copy_struct(ref1, size, &hp, &p->off_heap);
    return ref;
}

/* send_after(Time, Pid, Message) -> Ref */
BIF_RETTYPE send_after_3(BIF_ALIST_3)
BIF_ADECL_3
{
    BifTimerRec* btm;

    btm = do_timer(0, BIF_P,
		   BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (btm != NULL)
	BIF_RET(copy_ref(BIF_P, &btm->ref));
    else
	BIF_ERROR(BIF_P, BADARG);
}

/* start_timer(Time, Pid, Message) -> Ref */
BIF_RETTYPE start_timer_3(BIF_ALIST_3)
BIF_ADECL_3
{
    BifTimerRec* btm;

    btm = do_timer(1, BIF_P,
		   BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (btm != NULL)
       BIF_RET(copy_ref(BIF_P, &btm->ref));
    else
       BIF_ERROR(BIF_P, BADARG);
}

/* cancel_timer(Ref)) -> Bool */
BIF_RETTYPE cancel_timer_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BifTimerRec* btm;
    BifTimerRec** tp;
    uint32 left;

    if (is_not_refer(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if ((tp = find_timer(ref_ptr(BIF_ARG_1))) != NULL) {
	btm = *tp;
	*tp = btm->next;
	left = time_left(&btm->tm);
	erl_cancel_timer(&btm->tm);
	BIF_RET(make_small_or_big(left, BIF_P));
    }
    else 
	BIF_RET(am_false);
}

void print_timer_info(CIO to)
{
   int i;
   BifTimerRec *p;

   for (i = 0; i < TIMER_HASH_VEC; i++)
      if ((p = bif_tm_vec[i]) != NULL)
      {
	 erl_printf(to, "message=");
	 display(p->message,to);
	 erl_printf(to, ", pid=");
	 display(p->pid,to);
	 erl_printf(to, ", time left %d ms", time_left(&p->tm));
	 erl_printf(to, "\n");
      }
}
