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
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "erl_db_util.h"
#include "register.h"

static Eterm new_ref(Process *);	/* forward */


/*
 * The BIF's now follow, see the Erlang Manual for a description of what
 * each individual BIF does.
 *
 * Guards BIFs must not build anything at all on the heap.
 * They must use the ArithAlloc() macro instead of HAlloc().
 */

BIF_RETTYPE spawn_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = 0;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

/**********************************************************************/

/* Utility to add a new link between processes p and rp.
 * Process p must be the currently executing process.
 */

static void insert_link(Process* p, Process* rp)
{
    p->links = new_link(p->links, LNK_LINK, rp->id, NIL);
    rp->links = new_link(rp->links, LNK_LINK, p->id, NIL);

    ASSERT(is_nil(p->tracer_proc)
	   || is_internal_pid(p->tracer_proc)
	   || is_internal_port(p->tracer_proc));

    if (IS_TRACED(p)) {
	if (p->flags & F_TRACE_SOL)  {
	    rp->flags |= (p->flags & TRACE_FLAGS);
	    rp->tracer_proc = p->tracer_proc; /* maybe steal */
	}
	if (p->flags & F_TRACE_SOL1 )  { /* maybe override */
	    rp->flags |= (p->flags & TRACE_FLAGS);
	    rp->tracer_proc = p->tracer_proc;   
	    rp ->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
	    p->flags &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
	}
    }
    if (IS_TRACED_FL(rp, F_TRACE_PROCS))
	trace_proc(p, rp, am_getting_linked, p->id);
}


/* create a link to the process */
BIF_RETTYPE link_1(BIF_ALIST_1)
{
    Process *rp;
    DistEntry *dep;
    int ix;
    int code;

    if (IS_TRACED(BIF_P)) {
	if (BIF_P->flags & F_TRACE_PROCS)
	    trace_proc(BIF_P, BIF_P, am_link, BIF_ARG_1);
    }
    /* check that the pid or port which is our argument is OK */

    if (is_internal_port(BIF_ARG_1)) {
    
	/* we are linking to a port */
	if (find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL) != NULL)
	    BIF_RET(am_true); /* already linked */

	ix = internal_port_index(BIF_ARG_1);
	if (INVALID_PORT(erts_port+ix, BIF_ARG_1))
	    goto res_no_proc;
	erts_port[ix].links = new_link(erts_port[ix].links,LNK_LINK,BIF_P->id,NIL);

	BIF_P->links = new_link(BIF_P->links,LNK_LINK,BIF_ARG_1,NIL);
	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	goto res_no_proc;
	

    if (is_not_pid(BIF_ARG_1))
      BIF_ERROR(BIF_P, BADARG);

    /* we are linking to another process */
    /* check that the PID is OK */
    
    if (BIF_ARG_1 == BIF_P->id)
	BIF_RET(am_true);  /* Some people try the silliest things... */

    if (find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1, NIL) != NULL)
	BIF_RET(am_true);	/*  already linked */	
    
    if (is_external_pid(BIF_ARG_1)) {
	dep = external_pid_dist_entry(BIF_ARG_1);
	if (dep == erts_this_dist_entry)
	    goto res_no_proc;

	/* link to net */
	if (dep->cid == NIL)
	    BIF_TRAP1(dlink_trap, BIF_P, BIF_ARG_1);

	if ((code = dist_link(dep, BIF_P->id, BIF_ARG_1)) == 1) {
	    ASSERT(is_internal_port(dep->cid)); 
	    erl_suspend(BIF_P, dep->cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	else if (code < 0) { /* XXX is this the correct behaviour ??? */
	    BIF_ERROR(BIF_P, EXC_NOTALIVE);
	}
	if (dep->cid == NIL) {/* Might have been disconnected 
					     during dist_link */
	    BIF_TRAP1(dlink_trap, BIF_P, BIF_ARG_1);
	}
	
	/* insert the link in our own process */
	BIF_P->links = new_link(BIF_P->links,LNK_LINK,BIF_ARG_1,NIL);
	dep->links = new_link(dep->links, LNK_LINK, BIF_P->id, BIF_ARG_1);
	BIF_RET(am_true);
    }

    /* Internal pid... */

    if (internal_pid_index(BIF_ARG_1) >= erts_max_processes) {
	BIF_ERROR(BIF_P, BADARG);
    }

    /* get a pointer to the process struct of the linked process */
    rp = process_tab[internal_pid_index(BIF_ARG_1)];

    /* is the right process - ie not free and the pid corresponds?? */
    if (INVALID_PID(rp, BIF_ARG_1))
	goto res_no_proc;

    insert_link(BIF_P, rp);
    BIF_RET(am_true);

 res_no_proc:
    if (BIF_P->flags & F_TRAPEXIT) {
	deliver_exit_message(BIF_ARG_1, BIF_P, am_noproc);
	BIF_RET(am_true);
    }
    else
	BIF_ERROR(BIF_P, EXC_NOPROC);
}


BIF_RETTYPE demonitor_1(BIF_ALIST_1)
{
   ErlLink **lnkp;  /* The monitor link list entry to delete */
   Process  *rp;    /* Local target process */
   Eterm     ref;   /* BIF_ARG_1 */
   Eterm     to;    /* Monitor link traget */
   Eterm     ref_p; /* Pid of this end */
   DistEntry *dep;  /* Target's distribution entry */

   ref = BIF_ARG_1;
   if (is_not_internal_ref(ref))
      goto error; /* Cannot be this monitor's ref */
   ref_p = BIF_P->id;
   lnkp = find_link_by_ref(&BIF_P->links, ref);
   if (lnkp == NULL)
      return am_true;
   if (!EQ(ref_p, (*lnkp)->item))
      goto error; /* Trying to demonitor from wrong end */
   to = (*lnkp)->data;

   if ((*lnkp)->type == LNK_LINK1) {
      if (is_atom(to)) {
	  /* Monitoring a name at node to */
	  ASSERT(is_node_name_atom(to));
	  dep = erts_sysname_to_connected_dist_entry(to);
	  if(!dep) {
	      /* XXX Is this possible? Shouldn't this link
		 previously have been removed if the node
		 had previously been disconnected. */
	      ASSERT(0);
	      cerr_pos = 0;
	      erl_printf(CBUF, "Stale process monitor ");
	      display(ref, CBUF);
	      erl_printf(CBUF, " to ");
	      display(to, CBUF);
	      erl_printf(CBUF, " found\n");
	      send_error_to_logger(BIF_P->group_leader);
	      del_link(lnkp);
	      BIF_RET(am_true);
	  }
      }
      else {
	 ASSERT(is_pid(to));
	 dep = pid_dist_entry(to);
      }
      if (dep != erts_this_dist_entry) {
	 ErlLink** dist_lnkp;
	 Sint      code;
	 dist_lnkp = find_link_by_ref(&dep->links, ref);

	 if (!dist_lnkp) {
	     /* XXX How is this possible? Shouldn't this link
		previously have been removed when the distributed
		end was removed. */
	     ASSERT(0);
	     cerr_pos = 0;
	     erl_printf(CBUF, "Stale process monitor ");
	     display(ref, CBUF);
	     erl_printf(CBUF, " to ");
	     display(to, CBUF);
	     erl_printf(CBUF, " found\n");
	     send_error_to_logger(BIF_P->group_leader);
	     del_link(lnkp);
	     BIF_RET(am_true);
	 }
	 /* Soft (no force) send, use ->data in dist slot 
	  * monitor list since in case of monitor name 
	  * the atom is stored there. Reschedule if necessary.
	  */
	 code = dist_demonitor(dep, ref_p, (*dist_lnkp)->data, ref, 0);
	 if (code == 1) {
	    ASSERT(is_internal_port(dep->cid));
	    erl_suspend(BIF_P, dep->cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	 } else if (code < 0) { /* XXX is this the correct behaviour ??? */
	     /* Should rarely happen since this node must be 
	      * distributed for us to start the monitor.
	      */
	    BIF_ERROR(BIF_P, EXC_NOTALIVE);
	 }
	 /* We need to look up the link again, since the other node may have
	    gone down, the dist_demonitor call detected that, and therefore
	    what dist_lnkp pointed at is no longer there. */
	 dist_lnkp = find_link_by_ref(&dep->links, ref);
	 /* And then, if the dist entry was deleted, the process does no 
	    longer have the link either... Look that up again... */ 
	 lnkp = find_link_by_ref(&BIF_P->links, ref);
	 del_link(dist_lnkp);
      } else { /* Local monitor */
	 if ((rp = pid2proc(to)) != NULL)
	    del_link(find_link_by_ref(&rp->links, ref));
      }
   } else {
      ASSERT(0);
   }

   del_link(lnkp);
   BIF_RET(am_true);

 error:
   BIF_ERROR(BIF_P, BADARG);
}

/* Type must be atomic object! */
void
queue_monitor_message(Process *p, Eterm ref, Eterm type,
		      Eterm item, Eterm reason)
{
    Eterm tup;
    Eterm* hp;
    Eterm reason_copy, ref_copy, item_copy;
    Uint reason_size, ref_size, item_size;
    
    reason_size = IS_CONST(reason) ? 0 : size_object(reason);
    item_size   = IS_CONST(item) ? 0 : size_object(item);
    ref_size    = size_object(ref);

    hp = HAlloc(p, 6+reason_size+ref_size+item_size);

    reason_copy = (IS_CONST(reason)
		   ? reason
		   : copy_struct(reason, reason_size, &hp, &MSO(p)));
    item_copy   = (IS_CONST(item)
		   ? item
		   : copy_struct(item, item_size, &hp, &MSO(p)));
    ref_copy    = copy_struct(ref, ref_size, &hp, &MSO(p));

    tup = TUPLE5(hp, am_DOWN, ref_copy, type, item_copy, reason_copy);
    queue_message_tt(p, NULL, tup, NIL);
}

BIF_RETTYPE monitor_2(BIF_ALIST_2)
{
   Eterm       type; /* BIF_ARG_1 */
   Eterm       item; /* Item to put in link list *lnkp */
   Eterm       p_item; /* Item to put in this process's link list */
   Eterm       ref;
   DistEntry  *dep; /* Distribution entry */
   ErlLink   **lnkp = NULL;
   int broken_connection = 0;

   type = BIF_ARG_1;
   p_item = item = BIF_ARG_2;

   if (type == am_process) {
      Process *rp = NULL; /* Suppress use before set warning */

      if (is_tuple(item)) { /* (process, {Name, Node}) */
	 Eterm *tp = tuple_val(item);
	 Eterm nodename; /* Node */

	 if (arityval(*tp) != 2) 
	    goto error;
	 item = tp[1];
	 nodename    = tp[2];
	 if (is_not_atom(item) || is_not_atom(nodename))
	    goto error;
	 if ((erts_this_node->sysname == am_Noname)
	     && (nodename != erts_this_node->sysname))
	    goto error; /* Remote monitor from (this) undistributed node */
	 if ((dep = erts_sysname_to_connected_dist_entry(nodename)) == NULL) {
	    BIF_TRAP2(dmonitor_p_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	 }

	 if (dep == erts_this_dist_entry) {
	 locally_registered:
	    if ((rp = whereis_process(item)) != NULL) {
	       if (rp == BIF_P) /* Monitor of own process */
		  BIF_RET(new_ref(BIF_P));
	    }
	 }
      } else if (is_atom(item)) { /* (process, Name) */
	 dep = erts_this_dist_entry;
	 goto locally_registered;
      } else if (is_internal_pid(item)) { /* (process, Pid) */
	 if (item == BIF_P->id) /* Monitor of own process */
	    BIF_RET(new_ref(BIF_P));
         dep = internal_pid_dist_entry(item);
	 if (internal_pid_index(item) < erts_max_processes) {
	     rp = process_tab[internal_pid_index(item)];
	     if (INVALID_PID(rp, item))
		 rp = NULL;
	 }
	 else
	     rp = NULL;
      } else if (is_external_pid(item)) { /* (process, Pid) */
         rp = NULL;
	 dep = external_pid_dist_entry(item);
      } else
	  goto error;

      /* So far we know:
       * dep = dist entry pointer
       * item = pid() | atom() (registered name)
       */

      ref = new_ref(BIF_P);

      if (dep == erts_this_dist_entry) { /* Local process monitor */
	  if(is_atom(item) && rp) {
	      /* "item" is the registered name which will
		 be stored in monitored process. */
	      p_item = rp->id;
	      lnkp = &rp->links;
	  }
	  else if (!INVALID_PID(rp, item)) {
	      lnkp = &rp->links;
	  }
	  else
	      lnkp = NULL;
      } else { /* Remote process monitor */
	 Sint code;
         if (is_nil(dep->cid))
            BIF_TRAP2(dmonitor_p_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	 if (!(dep->flags & DFLAG_DIST_MONITOR)) 
	     /* 4 = distr. version for R6 */
	    goto error;
	 if (is_not_external_pid(item)
	     && !(dep->flags & DFLAG_DIST_MONITOR_NAME) )
	     /* Other node only supports monitor by pid */
	     goto error;
	 if ((code = dist_monitor(dep, BIF_P->id, item, ref)) == 1) {
	    ASSERT(is_internal_port(dep->cid)); 
	    erl_suspend(BIF_P, dep->cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	 } else if (code < 0) {	/* XXX is this the correct behaviour ??? */
	    /* Should not happen since this is taken care of in the 
	     * argument check above; node is not distributed yet,
	     * and then is BADARG returned (goto error). 
	     * EXC_NOTALIVE is really more informative, but
	     * does not seem to be the common practice today.
	     */
	    BIF_ERROR(BIF_P, EXC_NOTALIVE);
	 } 
	 if (dep->cid == NIL) {
	     lnkp = NULL;
	     broken_connection = 1; /* Node went down when we sent the 
				       distribution message, handle like 
				       it went down after... */
	 } else {
	     lnkp = &dep->links;
	 }
	 if (! is_external_pid(item)) {
	     /* "item" is the registered name. Registered name will
		be stored in dist_entry and nodename will be stored
		in process */
	     p_item = dep->sysname;
	 }
      }
   } else {/* BIF_ARG_1, i.e type, is invalid */
       goto error;
   }

   /* Set up the links */
   if (lnkp == NULL) {
       if (broken_connection) { /* Connection breakdown when sending 
				   monitor message over distribution */
	   queue_monitor_message(BIF_P, ref, type, BIF_ARG_2, am_noconnection);
       } else {
	   Eterm lhp[3];
	   item = (is_atom(BIF_ARG_2)
		   ? TUPLE2(&lhp[0], BIF_ARG_2, erts_this_dist_entry->sysname)
		   : BIF_ARG_2);
	   queue_monitor_message(BIF_P, ref, type, item, am_noproc);
       }
   } else { 
       /* Note! perhaps not same data put in both lists;
	* p_item (may be)!= item
	*/
      BIF_P->links = new_ref_link(BIF_P->links, LNK_LINK1,
				  BIF_P->id, p_item, ref);
      *lnkp = new_ref_link(*lnkp, LNK_LINK1,
			   BIF_P->id, item, ref);
   }
   BIF_RET(ref);

 error:
   BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/
/* this is a combination of the spawn and link BIFs */

BIF_RETTYPE spawn_link_3(BIF_ALIST_3)
{
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = SPO_LINK;
    pid = erl_create_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

/**********************************************************************/

BIF_RETTYPE spawn_opt_1(BIF_ALIST_1)
{
    ErlSpawnOpts so;
    Eterm pid;
    Eterm* tp;
    Eterm ap;
    Eterm arg;

    /*
     * Check that the first argument is a tuple of four elements.
     */
    if (is_not_tuple(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    tp = tuple_val(BIF_ARG_1);
    if (*tp != make_arityval(4))
	goto error;

    /*
     * Store default values for options.
     */
    so.flags = SPO_USE_ARGS;
    so.min_heap_size = H_MIN_SIZE;
    so.priority = PRIORITY_NORMAL;
    so.max_gen_gcs = erts_max_gen_gcs;

    /*
     * Walk through the option list.
     */
    ap = tp[4];
    while (is_list(ap)) {
	arg = CAR(list_val(ap));
	if (arg == am_link) {
	    so.flags |= SPO_LINK;
	} else if (is_tuple(arg)) {
	    Eterm* tp2 = tuple_val(arg);
	    Eterm val;
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
	    } else if (arg == am_min_heap_size && is_small(val)) {
		int min_heap_size = signed_val(val);
		if (min_heap_size < 0) {
		    goto error;
		} else if (min_heap_size < H_MIN_SIZE) {
		    so.min_heap_size = H_MIN_SIZE;
		} else {
		    so.min_heap_size = erts_next_heap_size(min_heap_size, 0);
		}
	    } else if (arg == am_fullsweep_after && is_small(val)) {
		int max_gen_gcs = signed_val(val);
		if (max_gen_gcs < 0) {
		    goto error;
		} else {
		    so.max_gen_gcs = max_gen_gcs;
		}
	    } else {
		goto error;
	    }
	} else {
	    goto error;
	}
	ap = CDR(list_val(ap));
    }
    if (is_not_nil(ap)) {
	goto error;
    }

    /*
     * Spawn the process.
     */
    pid = erl_create_process(BIF_P, tp[1], tp[2], tp[3], &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
}

  
/**********************************************************************/
/* remove a link from a process */
BIF_RETTYPE unlink_1(BIF_ALIST_1)
{
    Process *rp;
    DistEntry *dep;
    int ix;
    ErlLink **rlinkpp;

    if (IS_TRACED(BIF_P)) {
	if (BIF_P->flags & F_TRACE_PROCS) 
	    trace_proc(BIF_P, BIF_P, am_unlink, BIF_ARG_1);
    }

    if (is_internal_port(BIF_ARG_1)) {
	del_link(find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL));
	ix = internal_port_index(BIF_ARG_1);
	if (! INVALID_PORT(erts_port+ix, BIF_ARG_1))
	    del_link(find_link(&erts_port[ix].links,LNK_LINK,BIF_P->id,NIL));
	BIF_RET(am_true);
    }
    else if (is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry) {
	BIF_RET(am_true);
    }

    if (is_not_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);

    del_link(find_link(&BIF_P->links,LNK_LINK,BIF_ARG_1,NIL));

    if (is_external_pid(BIF_ARG_1)) {
	dep = external_pid_dist_entry(BIF_ARG_1);
	if (dep == erts_this_dist_entry)
	    BIF_RET(am_true);

	if (is_nil(dep->cid))
	    BIF_TRAP1(dunlink_trap, BIF_P, BIF_ARG_1);

	if (dist_unlink(dep, BIF_P->id, BIF_ARG_1) == 1) {
	    ASSERT(is_internal_port(dep->cid)); 
	    erl_suspend(BIF_P, dep->cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	del_link(find_link(&dep->links, LNK_LINK, BIF_P->id, BIF_ARG_1));
	BIF_RET(am_true);
    }

    /* Internal pid... */

     /* process ok ? */
    if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	BIF_ERROR(BIF_P, BADARG);

    /* get process struct */
    rp = process_tab[internal_pid_index(BIF_ARG_1)];

    /* and we mean this process */
    if (INVALID_PID(rp, BIF_ARG_1))
	BIF_RET(am_true);

    /* unlink and ignore errors */
    rlinkpp = find_link(&rp->links, LNK_LINK, BIF_P->id, NIL);
    del_link(rlinkpp);
    if (IS_TRACED_FL(rp, F_TRACE_PROCS) && rlinkpp != NULL) {
	trace_proc(BIF_P, rp, am_getting_unlinked, BIF_P->id);
    }
     
    BIF_RET(am_true);
}

BIF_RETTYPE hibernate_3(BIF_ALIST_3)
{
    /*
     * hibernate/3 is implemented as an instruction; therefore
     * this function will never be called.
     */
    BIF_ERROR(BIF_P, BADARG);
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
{
    BIF_P->fvalue = BIF_ARG_1;  /* exit value */
    BIF_ERROR(BIF_P, USER_EXIT);
}

/**********************************************************************/
/* send an exit message to another process (if trapping exits) or
   exit the other process */

BIF_RETTYPE exit_2(BIF_ALIST_2)
{
     Process *rp;
     DistEntry *dep;
     int code;
     Eterm exit_value = (BIF_ARG_2 == am_kill) ? am_killed : BIF_ARG_2;

     /*
      * If the first argument is not a pid, or a local port it is an error.
      */

     if (is_internal_port(BIF_ARG_1)) {
	 do_exit_port(BIF_ARG_1, BIF_P->id, BIF_ARG_2);
	 if (BIF_P->status != P_RUNNING) {
	     BIF_P->fvalue = exit_value;
	     KILL_CATCHES(BIF_P);
	     BIF_ERROR(BIF_P, USER_EXIT);
	 }
	 BIF_RET(am_true);
     }
     else if(is_external_port(BIF_ARG_1)
	     && external_port_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	 BIF_RET(am_true);
     
     /*
      * If it is a remote pid, send a message to the remote node.
      */

     if (is_external_pid(BIF_ARG_1)) {
	 dep = external_pid_dist_entry(BIF_ARG_1);
	 if(dep == erts_this_dist_entry)
	     BIF_RET(am_true);
	     
	 if (dep->cid == NIL)
	     BIF_TRAP2(dexit_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);

	 if ((code = dist_exit2(dep, BIF_P->id, BIF_ARG_1, BIF_ARG_2)) == 1) {
	     ASSERT(is_internal_port(dep->cid)); 
	     erl_suspend(BIF_P, dep->cid);
	     BIF_ERROR(BIF_P, RESCHEDULE);
	 }
	 else if (code < 0) {
	     BIF_ERROR(BIF_P, EXC_NOTALIVE);
	 }
	 BIF_RET(am_true);
     }
     else if (is_not_internal_pid(BIF_ARG_1)) 
       BIF_ERROR(BIF_P, BADARG);


     /*
      * The pid is internal.  Verify that it refers to an existing process.
      */


     if (internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	 BIF_ERROR(BIF_P, BADARG);
     rp = process_tab[internal_pid_index(BIF_ARG_1)];
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
   Eterm old_value = NIL;	/* shut up warning about use before set */
   int i;
   struct saved_calls *ct;

   if (flag == am_error_handler) {
      if (is_not_atom(val)) {
	 goto error;
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
	  goto error;
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
      if (!is_small(val)) {
	  goto error;
      }
      i = signed_val(val);
      if (i < 0 || i > 10000) {
	  goto error;
      }

      if (rp->ct == NULL)
	 old_value = make_small(0);
      else
	 old_value = make_small(rp->ct->len);
      if (i == 0) {
	 ct = NULL;
      } else {
	 Uint sz = sizeof(*ct) + (i-1) * sizeof(ct->ct[0]);
	 ct = erts_alloc(ERTS_ALC_T_CALLS_BUF, sz);
	 ERTS_PROC_MORE_MEM(sz);
	 ct->len = i;
	 ct->cur = 0;
	 ct->n = 0;
      }

      if (rp->ct != NULL) {
	 ERTS_PROC_LESS_MEM(sizeof(*(rp->ct))
			    + (rp->ct->len-1) * sizeof(rp->ct->ct[0]));
	 erts_free(ERTS_ALC_T_CALLS_BUF, (void *) rp->ct);
      }
      rp->ct = ct;

      /* Make sure the process in question is rescheduled
	 immediately, if it's us, so the call saving takes effect. */
      if (rp == BIF_P)
	 BIF_RET2(old_value, CONTEXT_REDS);
      else
	 BIF_RET(old_value);
   } else if (flag == am_min_heap_size) {
       if (!is_small(val)) {
	   goto error;
       }
       i = signed_val(val);
       if (i < 0) {
	   goto error;
       }
       old_value = make_small(BIF_P->min_heap_size);
       if (i < H_MIN_SIZE) {
	   BIF_P->min_heap_size = H_MIN_SIZE;
       } else {
	   BIF_P->min_heap_size = erts_next_heap_size(i, 0);
       }
       BIF_RET(old_value);
   }

 error:
   BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE process_flag_2(BIF_ALIST_2)
{
   return process_flag_aux(BIF_P, BIF_P, BIF_ARG_1, BIF_ARG_2);
}

BIF_RETTYPE process_flag_3(BIF_ALIST_3)
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
{
     Process *rp;
     Port *rport;
     
     if (is_not_atom(BIF_ARG_1))
	 BIF_ERROR(BIF_P, BADARG);
     /* Check that we don't register undefined */
     if (BIF_ARG_1 == am_undefined)
	 BIF_ERROR(BIF_P, BADARG);

     if ((rp = pid2proc(BIF_ARG_2)) != NULL) {
	 if (rp->reg != NULL) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 if (register_process(BIF_P, BIF_ARG_1, rp) != rp) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
     } else if ((rport = id2port(BIF_ARG_2)) != NULL) {
	 if (rport->reg != NULL) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
	 if (register_port(BIF_ARG_1, rport) != rport) {
	     BIF_ERROR(BIF_P, BADARG);
	 }
     }

     BIF_RET(am_true);
}


/**********************************************************************/

/* removes the registration of a process or port */

BIF_RETTYPE unregister_1(BIF_ALIST_1)
{
    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((unregister_name(BIF_P, BIF_ARG_1)) == 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(am_true);
}

/**********************************************************************/

/* find out the pid of a registered process */
/* this is a rather unsafe BIF as it allows users to do nasty things. */

BIF_RETTYPE whereis_1(BIF_ALIST_1)
{
    Process *rp;
    Port *pt;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    whereis_name(BIF_ARG_1, &rp, &pt);
    if (rp != NULL)
	BIF_RET(rp->id); /* Internal pid */
    else if (pt != NULL)
	BIF_RET(pt->id); /* Internal port */
    else
	BIF_RET(am_undefined);
}

/**********************************************************************/

/* return a list of the registered processes */

BIF_RETTYPE registered_0(BIF_ALIST_0)
{
    int i;
    Eterm res;
    Uint need;
    Eterm* hp;
    HashBucket **bucket = process_reg.bucket;

    /* work out how much heap we need & maybe garb, by scanning through
       the registered process table */
    need = 0;
    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    need += 2;
	    b = b->next;
	}
    }

    if (need == 0)
	BIF_RET(NIL);
    hp = HAlloc(BIF_P, need);
     
     /* scan through again and make the list */ 
    res = NIL;

    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    RegProc *reg = (RegProc *) b;

	    res = CONS(hp, reg->name, res);
	    hp += 2;
	    b = b->next;
	}
    }

    BIF_RET(res);
}

/**********************************************************************/

/*
 * erlang:'!'/2
 */

Eterm
ebif_bang_2(Process* p, Eterm To, Eterm Message)
{
    return send_2(p, To, Message);
}


/*
 * Send a message to Process, Port or Registered Process.
 * Returns non-negative reduction bump or negative result code.
 */
#define SEND_TRAP       (-1)
#define SEND_RESCHEDULE (-2)
#define SEND_BADARG     (-3)
#define SEND_USER_ERROR (-4)

Sint do_send(Process *p, Eterm to, Eterm msg, int suspend);

Sint
do_send(Process *p, Eterm to, Eterm msg, int suspend) {
    Eterm portid;
    Port *pt;
    Process* rp;
    DistEntry *dep;
    Eterm* tp;
    
    if (is_internal_pid(to)) {
	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (p->ct != NULL)
	   save_calls(p, &exp_send);
	
	if (internal_pid_index(to) >= erts_max_processes)
	    return SEND_BADARG;
	rp = process_tab[internal_pid_index(to)];
	
	if (INVALID_PID(rp, to)) {
	    return 0;
	}
    } else if (is_external_pid(to)) {
	dep = external_pid_dist_entry(to);
	if(dep == erts_this_dist_entry) {
	    cerr_pos = 0;
	    erl_printf(CBUF,
		       "Discarding message (%d,%d)",
		       external_pid_creation(to),
		       erts_this_node->creation);
	    display(msg,CBUF);
	    erl_printf(CBUF,
		       "to a process in an old incarnation of this node\n");
	    send_error_to_logger(p->group_leader);
	    return 0;
	}
	
	/* Send to remote process */
	if (is_nil(dep->cid))
	    return SEND_TRAP;
	if (dist_send(p, dep, to, msg) == 1) {
	    /* Only ports can be busy */
	    ASSERT(is_internal_port(dep->cid));
	    if (suspend) {
		erl_suspend(p, dep->cid);
		if (erts_system_monitor_flags.busy_dist_port) {
		    monitor_generic(p, am_busy_dist_port, dep->cid);
		}
	    }
	    return SEND_RESCHEDULE;
	}
	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (p->ct != NULL)
	    save_calls(p, &exp_send);
	return 50;
    } else if (is_atom(to)) {
	whereis_name(to, &rp, &pt);
	if (pt != NULL) {
	    portid = pt->id;
	    goto port_common;
	}
	
	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (p->ct != NULL)
	   save_calls(p, &exp_send);
	
	if (rp == NULL) {
	    return SEND_BADARG;
	}
	if (rp->status == P_EXITING) {
	    return 0;
	}
    } else if (is_external_port(to)
	       && (external_port_dist_entry(to)
		   == erts_this_dist_entry)) {
	cerr_pos = 0;
	erl_printf(CBUF,
		   "Discarding message (%d,%d)",
		   external_port_creation(to),
		   erts_this_node->creation);
	display(msg,CBUF);
	erl_printf(CBUF,
		   "to a port in an old incarnation of this node\n");
	send_error_to_logger(p->group_leader);
	return 0;
    } else if (is_internal_port(to)) {
	portid = to;
	
      port_common:
	/* XXX let port_command handle the busy stuff !!! */
	if (! INVALID_PORT(erts_port+internal_port_index(portid), portid)
	    && (erts_port[internal_port_index(portid)].status & PORT_BUSY)) {
	    if (suspend) {
		erl_suspend(p, portid);
		if (erts_system_monitor_flags.busy_port) {
		    monitor_generic(p, am_busy_port, portid);
		}
	    }
	    return SEND_RESCHEDULE;
	}
	
	if (IS_TRACED(p)) 	/* trace once only !! */
	    trace_send(p, portid, msg);
	if (p->ct != NULL)
	   save_calls(p, &exp_send);
	
	if (SEQ_TRACE_TOKEN(p) != NIL) {
	    seq_trace_update_send(p);
	    seq_trace_output(SEQ_TRACE_TOKEN(p), msg, 
			     SEQ_TRACE_SEND, portid, p);
	}	    
	
	/* XXX NO GC in port command */
	port_command(p->id, portid, msg);
	
	if (p->status == P_EXITING) {
	    KILL_CATCHES(p); /* Must exit */
	    return SEND_USER_ERROR;
	}
	return 0;
    } else if (is_tuple(to)) { /* Remote send */
	tp = tuple_val(to);
	if (*tp != make_arityval(2))
	    return SEND_BADARG;
	if (is_not_atom(tp[1]) || is_not_atom(tp[2]))
	    return SEND_BADARG;
	
	/* sysname_to_connected_dist_entry will return NULL if there
	   is no dist_entry or the dist_entry has no port*/
	if ((dep = erts_sysname_to_connected_dist_entry(tp[2])) == NULL) {
	    return SEND_TRAP;
	}
	
	if (dep == erts_this_dist_entry) {
	    if (IS_TRACED(p))
		trace_send(p, to, msg);
	    if (p->ct != NULL)
	       save_calls(p, &exp_send);
	    
	    whereis_name(tp[1], &rp, &pt);
	    if (pt != NULL) {
		portid = pt->id;
		goto port_common;
	    }
	    if (rp == NULL) {
		return 0;
	    }
	    if (rp->status == P_EXITING) {
		return 0;
	    }
	    goto send_message;
	}
	
	ASSERT(dep->cid != NIL);
	if (dist_reg_send(p, dep, tp[1], msg) == 1) {
	    ASSERT(is_port(dep->cid));
	    if (suspend) {
		erl_suspend(p, dep->cid);
		if (erts_system_monitor_flags.busy_dist_port) {
		    monitor_generic(p, am_busy_dist_port, dep->cid);
		}
	    }
	    return SEND_RESCHEDULE;
	}
	if (IS_TRACED(p))
	    trace_send(p, to, msg);
	if (p->ct != NULL)
	   save_calls(p, &exp_send);
	return 0;
    } else {
	if (IS_TRACED(p)) /* XXX Is this really neccessary ??? */
	    trace_send(p, to, msg);
	if (p->ct != NULL)
	   save_calls(p, &exp_send);
	return SEND_BADARG;
    }
    
 send_message:
    send_message(p, rp, msg);
    return rp->msg.len*4;
}


Eterm
send_3(Process *p, Eterm to, Eterm msg, Eterm opts) {
    int connect = !0;
    int suspend = !0;
    Eterm l = opts;
    Sint result;
    
    while (is_list(l)) {
	if (CAR(list_val(l)) == am_noconnect) {
	    connect = 0;
	} else if (CAR(list_val(l)) == am_nosuspend) {
	    suspend = 0;
	} else {
	    BIF_ERROR(p, BADARG);
	}
	l = CDR(list_val(l));
    }
    if(!is_nil(l)) {
	BIF_ERROR(p, BADARG);
    }
    
    result = do_send(p, to, msg, suspend);
    if (result > 0) {
	BUMP_REDS(p, result);
	BIF_RET(am_ok);
    } else switch (result) {
    case 0:
	BIF_RET(am_ok); 
	break;
    case SEND_TRAP:
	if (connect) {
	    BIF_TRAP3(dsend3_trap, p, to, msg, opts); 
	} else {
	    BIF_RET(am_noconnect);
	}
	break;
    case SEND_RESCHEDULE:
	if (suspend) {
	    BIF_ERROR(p, RESCHEDULE);
	} else {
	    BIF_RET(am_nosuspend);
	}
	break;
    case SEND_BADARG:
	BIF_ERROR(p, BADARG); 
	break;
    case SEND_USER_ERROR:
	BIF_ERROR(p, USER_ERROR); 
	break;
    default:
	ASSERT(! "Illegal send result"); 
	break;
    }
    ASSERT(! "Can not arrive here");
    BIF_ERROR(p, BADARG);
}

Eterm
send_2(Process *p, Eterm to, Eterm msg) {
    Sint result = do_send(p, to, msg, !0);
    
    if (result > 0) {
	BUMP_REDS(p, result);
	BIF_RET(msg);
    } else switch (result) {
    case 0:
	BIF_RET(msg); 
	break;
    case SEND_TRAP:
	BIF_TRAP2(dsend2_trap, p, to, msg); 
	break;
    case SEND_RESCHEDULE:
	BIF_ERROR(p, RESCHEDULE); 
	break;
    case SEND_BADARG:
	BIF_ERROR(p, BADARG); 
	break;
    case SEND_USER_ERROR:
	BIF_ERROR(p, USER_ERROR); 
	break;
    default:
	ASSERT(! "Illegal send result"); 
	break;
    }
    ASSERT(! "Can not arrive here");
    BIF_ERROR(p, BADARG);
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
{
    BIF_ERROR(BIF_P, BADARG);
}


/* maths abs function */
BIF_RETTYPE abs_1(BIF_ALIST_1)
{
    Eterm res;
    Sint i0, i;
    Eterm* hp;

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
	    Uint* x;

	    hp = ArithAlloc(BIF_P, sz);	/* See note at beginning of file */
	    sz--;
	    res = make_big(hp);
	    x = big_val(BIF_ARG_1);
	    *hp++ = make_pos_bignum_header(sz);
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
{
    Eterm res;
    Eterm* hp;
    FloatDef f;
     
    /* check args */
    if (is_not_integer(BIF_ARG_1)) {
	if (is_float(BIF_ARG_1))  {
	    BIF_RET(BIF_ARG_1);
	} else {
	badarg:
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    if (is_small(BIF_ARG_1)) {
	Sint i = signed_val(BIF_ARG_1);
	f.fd = i;		/* use "C"'s auto casting */
    } else if (big_to_double(BIF_ARG_1, &f.fd) < 0) {
	goto badarg;
    }
    hp = ArithAlloc(BIF_P, 3);	/* See note at beginning of file */
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

/**********************************************************************/

/* truncate a float returning an integer */
BIF_RETTYPE trunc_1(BIF_ALIST_1)
{
    Eterm res;
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
{
    Eterm res;
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

/* return the length of a list */

BIF_RETTYPE length_1(BIF_ALIST_1)
{
    Eterm list;
    Uint i;
     
    if (is_nil(BIF_ARG_1)) 
	BIF_RET(SMALL_ZERO);
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    list = BIF_ARG_1;
    i = 0;
    while (is_list(list)) {
	i++;
	list = CDR(list_val(list));
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
{
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(CAR(list_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the tails of a list - same comment as above */

BIF_RETTYPE tl_1(BIF_ALIST_1)
{
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(CDR(list_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the size of a tuple or a binary */

BIF_RETTYPE size_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	Eterm* tupleptr = tuple_val(BIF_ARG_1);

	BIF_RET(make_small(arityval(*tupleptr)));
    } else if (is_binary(BIF_ARG_1)) {
	BIF_RET(make_small_or_big(binary_size(BIF_ARG_1), BIF_P));
    } else if (is_vector(BIF_ARG_1)) {
	return vector_val(BIF_ARG_1)[1];
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the N'th element of a tuple */

BIF_RETTYPE element_2(BIF_ALIST_2)
{
    if (is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_2)) {
	Eterm* tuple_ptr = tuple_val(BIF_ARG_2);
	Sint ix = signed_val(BIF_ARG_1);

	if ((ix >= 1) && (ix <= arityval(*tuple_ptr)))
	    BIF_RET(tuple_ptr[ix]);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* set the n'th element in a tuple */

BIF_RETTYPE setelement_3(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Eterm* resp;
    Uint ix;
    Uint size;

    if (is_not_small(BIF_ARG_1) || is_not_tuple(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = tuple_val(BIF_ARG_2);
    ix = signed_val(BIF_ARG_1);
    size = arityval(*ptr) + 1;   /* include arity */
    if ((ix < 1) || (ix >= size)) {
	goto error;
    }

    hp = HAlloc(BIF_P, size);

    /* copy the tuple */
    resp = hp;
    while (size--) {		/* XXX use memcpy? */
	*hp++ = *ptr++;
    }
    resp[ix] = BIF_ARG_3;
    BIF_RET(make_tuple(resp));
}

/**********************************************************************/

BIF_RETTYPE make_tuple_2(BIF_ALIST_2)
{
    int n;
    Eterm* hp;
    Eterm res;

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
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;

    if (is_not_tuple(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = tuple_val(BIF_ARG_1);
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
{
    Uint need;
    Eterm* hp;
    Atom* ap;

    if (is_not_atom(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
     
    /* read data from atom table */
    ap = atom_tab(atom_val(BIF_ARG_1));
    if (ap->len == 0)
	BIF_RET(NIL);	/* the empty atom */
    need = ap->len*2;
    hp = HAlloc(BIF_P, need);
    BIF_RET(buf_to_intlist(&hp,ap->name,ap->len, NIL));
}

/**********************************************************************/

/* convert a list of ascii intgers to an atom */
 
BIF_RETTYPE list_to_atom_1(BIF_ALIST_1)
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
{
    Eterm* hp;
    Uint need;

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
	Eterm res;

	need = 2*n;
	hp = HAlloc(BIF_P, need);
	res = big_to_list(BIF_ARG_1, &hp);
	BIF_RET(res);
    }
}

/**********************************************************************/

/* convert a list of ascii ascii integer value to an integer */

BIF_RETTYPE list_to_integer_1(BIF_ALIST_1)
{
     Eterm lst;
     Sint32 i = 0;
     int skip = 0;
     int neg = 0;
     int n = 0;
     int m;
     int lg2;
     Eterm res;
     Eterm* hp;

     /* must be a list */
     if (is_not_list(BIF_ARG_1)) {
     error:
	 BIF_ERROR(BIF_P, BADARG);
     }
     lst = BIF_ARG_1;

     /* if first char is a '-' then it is a negative integer */
     if (CAR(list_val(lst)) == make_small('-')) {
	  neg = 1;
	  skip = 1;
	  lst = CDR(list_val(lst));
	  if (is_not_list(lst)) {
	      goto error;
	  }
     } else if (CAR(list_val(lst)) == make_small('+')) {
	 /* ignore plus */
	 skip = 1;
	 lst = CDR(list_val(lst));
	 if (is_not_list(lst)) {
	     goto error;
	 }
     }

     /* Calculate size and do type check */

     while(1) {
	 if (is_not_small(CAR(list_val(lst)))) {
	     goto error;
	 }
	 if (unsigned_val(CAR(list_val(lst))) < '0' ||
	     unsigned_val(CAR(list_val(lst))) > '9') {
	     goto error;
	 }
	 i = i * 10;
	 i = i + unsigned_val(CAR(list_val(lst))) - '0';
	 n++;
	 lst = CDR(list_val(lst));
	 if (is_nil(lst))
	     break;
	 if (is_not_list(lst)) {
	     goto error;
	 }
     }

      /* If n <= 8 then we know it's a small int 
      ** since 2^27 = 134217728. If n > 8 then we must
      ** construct a bignum and let that routine do the checking
      */

     if (n <= SMALL_DIGITS) {  /* It must be small */
	 if (neg) i = -i;
	 BIF_RET(make_small(i));
     }

     lg2 =  (n+1)*230/69+1;
     m  = (lg2+D_EXP-1)/D_EXP; /* number of digits */
     m  = ((m+1)>>1) + 1;      /* number of words + thing */

     hp = HAlloc(BIF_P, m);

     lst = BIF_ARG_1;
     if (skip)
	 lst = CDR(list_val(lst));

     /* load first digits (at least one digit) */
     if ((i = (n % 4)) == 0)
	 i = 4;
     n -= i;
     m = 0;
     while(i--) {
	 m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
	 lst = CDR(list_val(lst));
     }
     res = small_to_big(m, hp);  /* load first digits */

     while(n) {
	 i = 4;
	 n -= 4;
	 m = 0;
	 while(i--) {
	     m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
	     lst = CDR(list_val(lst));
	 }
	 if (is_small(res))
	     res = small_to_big(signed_val(res), hp);
	 res = big_times_small(res, D_DECIMAL_BASE, hp);
	 if (is_small(res))
	     res = small_to_big(signed_val(res), hp);
	 res = big_plus_small(res, m, hp);
     }

     if (is_big(res))  /* check if small */
	 res = big_plus_small(res, 0, hp); /* includes conversion to small */

     if (neg) {
	 if (is_small(res))
	     res = make_small(-signed_val(res));
	 else {
	     Uint *big = big_val(res); /* point to thing */
	     *big = bignum_header_neg(*big);
	 }
     }

     /* XXX Why this code? */
     if (is_big(res)) {
	 hp += (big_arity(res)+1);
     }
     BIF_RET(res);
 }

/**********************************************************************/

/* convert a float to a list of ascii characters */

BIF_RETTYPE float_to_list_1(BIF_ALIST_1)
{
     int i;
     Uint need;
     Eterm* hp;
     FloatDef f;
     char fbuf[30];
     
     /* check the arguments */
     if (is_not_float(BIF_ARG_1))
	 BIF_ERROR(BIF_P, BADARG);
     GET_DOUBLE(BIF_ARG_1, f);
     if ((i = sys_double_to_chars(f.fd, fbuf)) <= 0)
	 BIF_ERROR(BIF_P, EXC_INTERNAL_ERROR);
     need = i*2;
     hp = HAlloc(BIF_P, need);
     BIF_RET(buf_to_intlist(&hp, fbuf, i, NIL));
 }

/**********************************************************************/

/* convert a list of ascii  integer values e's +'s and -'s to a float */

BIF_RETTYPE list_to_float_1(BIF_ALIST_1)
{
    int i;
    FloatDef f;
    Eterm res;
    Eterm* hp;

    if ((i = intlist_to_buf(BIF_ARG_1, tmp_buf, TMP_BUF_SIZE-1)) < 0) {
    badarg:
	BIF_ERROR(BIF_P, BADARG);
    }
    tmp_buf[i] = '\0';		/* null terminal */

    ERTS_FP_CHECK_INIT();
    if (sys_chars_to_double((char*)tmp_buf, &f.fd) != 0) {
	goto badarg;
    }
    hp = HAlloc(BIF_P, 3);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}


/**********************************************************************/

/* convert a tuple to a list */

BIF_RETTYPE tuple_to_list_1(BIF_ALIST_1)
{
    Uint n;
    Eterm *tupleptr;
    Eterm list = NIL;
    Eterm* hp;

    if (is_not_tuple(BIF_ARG_1))  {
	BIF_ERROR(BIF_P, BADARG);
    }

    tupleptr = tuple_val(BIF_ARG_1);
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
{
    Eterm list = BIF_ARG_1;
    Eterm* cons;
    Eterm res;
    Eterm* hp;
    int len;

    if ((len = list_length(list)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, len+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(len);
    while(is_list(list)) {
	cons = list_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    BIF_RET(res);
}

/**********************************************************************/

/* return the pid of our own process, in most cases this has been replaced by
   a machine instruction */

BIF_RETTYPE self_0(BIF_ALIST_0)
{
     BIF_RET(BIF_P->id);
}

/**********************************************************************/

/*
   New representation of refs in R9, see erl_term.h

   In the first data word, only the usual 18 bits are used. Ordinarily,
   in "long refs" all words are used (in other words, practically never
   wrap around), but for compatibility with older nodes, "short refs"
   exist. Short refs come into being by being converted from the old
   external format for refs (tag REFERENCE_EXT). Short refs are
   converted back to the old external format.

   When converting a long ref to the external format in the case of
   preparing for sending to an older node, the ref is truncated by only
   using the first word (with 18 significant bits), and using the old tag
   REFERENCE_EXT.

   When comparing refs or different size, only the parts up to the length
   of the shorter operand are used. This has the desirable effect that a
   long ref sent to an old node and back will be treated as equal to
   the original, although some of the bits have been lost.

   The hash value for a ref always considers only the first word, since
   in the above scenario, the original and the copy should have the same
   hash value.
*/

static Uint32 reference0; /* Initialized in erts_init_bif */
static Uint32 reference1;
static Uint32 reference2;

/* For internal use */
static Eterm new_ref(Process *p)
{
    Eterm* hp;

    reference0++;
    if (reference0 >= MAX_REFERENCE) {
	reference0 = 0;
	reference1++;
	if (reference1 == 0) {
	    reference2++;
	}
    }

    hp = HAlloc(p, REF_THING_SIZE);
    write_ref_thing(hp, reference0, reference1, reference2);
    return make_internal_ref(hp);
}

BIF_RETTYPE make_ref_0(BIF_ALIST_0)
{
    return new_ref(BIF_P);
}

/**********************************************************************/

/* return the time of day */

BIF_RETTYPE time_0(BIF_ALIST_0)
{
     int hour, minute, second;
     Eterm* hp;

     get_time(&hour, &minute, &second);
     hp = HAlloc(BIF_P, 4);	/* {hour, minute, second}  + arity */
     BIF_RET(TUPLE3(hp, make_small(hour), make_small(minute),
		    make_small(second)));
}
/**********************************************************************/

/* return the date */

BIF_RETTYPE date_0(BIF_ALIST_0)
{
     int year, month, day;
     Eterm* hp;
     
     get_date(&year, &month, &day);
     hp = HAlloc(BIF_P, 4);	/* {year, month, day}  + arity */
     BIF_RET(TUPLE3(hp, make_small(year), make_small(month), make_small(day)));
}

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_0(BIF_ALIST_0)
{
     int year, month, day;
     int hour, minute, second;
     Eterm res1, res2;
     Eterm* hp;

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
{
     int year, month, day;
     int hour, minute, second;
     Eterm res1, res2;
     Eterm* hp;

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
static int 
time_to_parts(Eterm date, int* year, int* month, int* day,
	      int* hour, int* minute, int* second)
{
    Eterm* t1;
    Eterm* t2;

    if (is_not_tuple(date)) {
	return 0;
    }
    t1 = tuple_val(date);
    if (arityval(t1[0]) !=2 || 
	is_not_tuple(t1[1]) || is_not_tuple(t1[2]))
	return 0;
    t2 = tuple_val(t1[1]);
    t1 = tuple_val(t1[2]);
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

BIF_RETTYPE 
localtime_to_universaltime_2(Process *p, Eterm localtime, Eterm dst)
{
    int year, month, day;
    int hour, minute, second;
    int isdst;
    Eterm res1, res2;
    Eterm* hp;
    
    if (dst == am_true) isdst = 1;
    else if (dst == am_false) isdst = 0;
    else if (dst == am_undefined) isdst = -1;
    else goto error;
    
    if (!time_to_parts(localtime, &year, &month, &day, 
		       &hour, &minute, &second)) goto error;
    if (!local_to_univ(&year, &month, &day, 
		       &hour, &minute, &second, isdst)) goto error;
    
    hp = HAlloc(p, 4+4+3);
    res1 = TUPLE3(hp,make_small(year),make_small(month),
		  make_small(day));
    hp += 4;
    res2 = TUPLE3(hp,make_small(hour),make_small(minute),
		  make_small(second));
    hp += 4;
    BIF_RET(TUPLE2(hp, res1, res2));
 error:
    BIF_ERROR(p, BADARG);
 }
	 

/**********************************************************************/

/* return the universal time */

BIF_RETTYPE universaltime_to_localtime_1(BIF_ALIST_1)
{
    int year, month, day;
    int hour, minute, second;
    Eterm res1, res2;
    Eterm* hp;

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
{
    Uint megasec, sec, microsec;
    Eterm* hp;

    get_now(&megasec, &sec, &microsec);
    hp = HAlloc(BIF_P, 4);
    BIF_RET(TUPLE3(hp, make_small(megasec), make_small(sec),
		   make_small(microsec)));
}

/**********************************************************************/

BIF_RETTYPE garbage_collect_1(BIF_ALIST_1)
{
    Process *rp;

    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    } else if ((rp = pid2proc(BIF_ARG_1)) == NULL) {
	BIF_RET(am_false);
    } else {
	/* The GC cost is taken for the process executing this BIF. */
	int reds;

#ifdef SHARED_HEAP
	/* We need heap pointers in the process struct of current
	   process. (Both procs share the same heaps). */
	rp = BIF_P;
#endif

        FLAGS(rp) |= F_NEED_FULLSWEEP;
	reds = erts_garbage_collect(rp, 0, rp->arg_reg, rp->arity);
	BIF_RET2(am_true, reds);
    }
}

BIF_RETTYPE garbage_collect_0(BIF_ALIST_0)
{
    int reds;

    FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
    reds = erts_garbage_collect(BIF_P, 0, BIF_P->arg_reg, BIF_P->arity);
    BIF_RET2(am_true, reds);
}

/**********************************************************************/

/* returns a list of the active processes in the system */
/* scans the whole of the process table */

BIF_RETTYPE processes_0(BIF_ALIST_0)
{
    int i;
    int need = 0;
    Eterm res = NIL;
    Eterm* hp;
    Process *p;
     
    /* first work out how many processes there are */
    for (i = 0; i < erts_max_processes; i++)
	if ((p = process_tab[i]) != NULL)
	    need += 2;
     
    hp = HAlloc(BIF_P, need);     /* we need two heap words for each pid */

    /* make the list by scanning again (bakward) */
    for (i = erts_max_processes-1; i >= 0; i--) {
	if ((p = process_tab[i]) != NULL) {
	    res = CONS(hp, process_tab[i]->id, res);
	    hp += 2;
	}
    }
    BIF_RET(res);
}

/**********************************************************************/

BIF_RETTYPE is_process_alive_1(BIF_ALIST_1) 
{
   if(is_internal_pid(BIF_ARG_1)) {
       Process *rp;

       if(internal_pid_index(BIF_ARG_1) >= erts_max_processes)
	   BIF_ERROR(BIF_P, BADARG);

       rp = process_tab[internal_pid_index(BIF_ARG_1)];

       if (INVALID_PID(rp, BIF_ARG_1)) {
	   BIF_RET(am_false);
       }
       BIF_RET(am_true);
   }
   else if(is_external_pid(BIF_ARG_1)) {
       if(external_pid_dist_entry(BIF_ARG_1) == erts_this_dist_entry)
	   BIF_RET(am_false); /* A pid from an old incarnation of this node */
       else
	   BIF_ERROR(BIF_P, BADARG);
   }
   else {
      BIF_ERROR(BIF_P, BADARG);
   }
}

BIF_RETTYPE process_display_2(BIF_ALIST_2)
{
   Process *rp;


   rp = pid2proc(BIF_ARG_1);
   if(!rp)
       BIF_ERROR(BIF_P, BADARG);

   if (BIF_ARG_2 != am_backtrace)
       BIF_ERROR(BIF_P, BADARG);

   erts_stack_dump(rp, CERR);
   BIF_RET(am_true);
}

/**********************************************************************/
/* Return a list of active ports */

BIF_RETTYPE ports_0(BIF_ALIST_0)
{
    int i;
    int need = 0;
    Eterm res = NIL;
    Eterm* hp;
     
    /* first work out how many ports there are */
    for (i = 0; i < erts_max_ports; i++) {
	if (erts_port[i].status != FREE)
	    need += 2;
    }
    hp = HAlloc(BIF_P, need);

    for (i = erts_max_ports-1; i >= 0; i--) {
	if (erts_port[i].status != FREE) {
	    res = CONS(hp, erts_port[i].id, res);
	    hp += 2;
	}
    }
    BIF_RET(res);
}

/**********************************************************************/

BIF_RETTYPE throw_1(BIF_ALIST_1)
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
{
    VERBOSE(erl_printf(COUT, "System halted by BIF halt/0\n"););
    erl_exit(0, "");
    return NIL;  /* Pedantic (lint does not know about erl_exit) */
}

/**********************************************************************/

#define MSG_SIZE	200

/* stop the system with exit code */
/* ARGSUSED */
BIF_RETTYPE halt_1(BIF_ALIST_1)
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

    return NIL;  /* Pedantic (lint does not know about erl_exit) */
}

BIF_RETTYPE function_exported_3(BIF_ALIST_3)
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

BIF_RETTYPE ref_to_list_1(BIF_ALIST_1)
{
   int n;
   char* tmpp;
   Eterm obj = BIF_ARG_1;
   Eterm* hp;
   int i;

   tmpp = (char*) tmp_buf;

   if (is_ref(obj)) {
      Uint32 *ref_num;
      sprintf(tmpp, "#Ref<%ld", ref_channel_no(obj));
      tmpp = strchr(tmpp, '\0');
      ref_num = ref_numbers(obj);
      for (i = ref_no_of_numbers(obj)-1; i >= 0; i--) {
	  sprintf(tmpp, ".%lu", (Uint) ref_num[i]);
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
    int len;

    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);

	mod = funp->fe->module;
	index = funp->fe->old_index;
	uniq = funp->fe->old_uniq;
	strcpy(tmpp, "#Fun<");
	tmpp += strlen(tmpp);
	ap = atom_tab(atom_val(mod));
	sys_memcpy(tmpp, ap->name, ap->len);
	tmpp += ap->len;
	sprintf(tmpp, ".%d.%d>", index, uniq);
	len = strlen(tmp_buf);
	hp = HAlloc(p, 2*len);
	return buf_to_intlist(&hp, tmp_buf, len, NIL);
    }
    BIF_ERROR(p, BADARG);
}

/**********************************************************************/    

/* convert a pid to an erlang list (for the linked cons cells) of the form
   %node.serial.number% to a PID
 */

BIF_RETTYPE pid_to_list_1(BIF_ALIST_1)
{
    int n;
    char* cp;
    char* tmpp;
    char ibuf[12];
    Eterm pid = BIF_ARG_1;
    Eterm* hp;

    /* check arg */
    if (is_not_pid(pid)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    tmpp = (char*) tmp_buf;
    *tmpp++ = '<';
    
    /* node part */
    cp = int_to_buf(pid_channel_no(pid), ibuf);
    n = sys_strlen(cp);
    sys_strncpy(tmpp, cp, n);
    tmpp += n;
    *tmpp++ = '.';

    /* number part */
    cp = int_to_buf(pid_number(pid), ibuf);
    n = sys_strlen(cp);
    sys_strncpy(tmpp, cp, n);
    tmpp += n;
    *tmpp++ = '.';

    /* serial part */
    cp = int_to_buf(pid_serial(pid), ibuf);
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
{
    Uint a = 0, b = 0, c = 0;
    char* cp;
    int i;
    DistEntry *dep;

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

    dep = erts_channel_no_to_dist_entry(a);

    if (!dep)
	goto bad;


    if (c > ERTS_MAX_PID_SERIAL || b > ERTS_MAX_PID_NUMBER)
	goto bad;

    if(dep == erts_this_dist_entry) {
	BIF_RET(make_internal_pid(make_pid_data(c, b)));
    }
    else {
      ExternalThing *etp;
      ErlNode *enp;

      if (is_nil(dep->cid))
	  goto bad;
      
      enp = erts_find_or_insert_node(dep->sysname, dep->creation);

      etp = (ExternalThing *) HAlloc(BIF_P, EXTERNAL_THING_HEAD_SIZE + 1);
      etp->header = make_external_pid_header(1);
      etp->next = MSO(BIF_P).externals;
      etp->node = enp;
      etp->data[0] = make_pid_data(c, b);

      MSO(BIF_P).externals = etp;
      enp->refc++;
      BIF_RET(make_external_pid(etp));
    }

 bad:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/*
 * This function is obsolete. Retained for backward compatibility.
 */
  
BIF_RETTYPE old_binary_to_term_1(BIF_ALIST_1)
{
    return binary_to_term_1(BIF_P, BIF_ARG_1);
}


/**********************************************************************/

BIF_RETTYPE group_leader_0(BIF_ALIST_0)
{
    BIF_RET(BIF_P->group_leader);
}

/**********************************************************************/
/* arg1 == leader, arg2 == new member */

BIF_RETTYPE group_leader_2(BIF_ALIST_2)
{
    Process* new_member;
    int code;
    DistEntry *dep;

    if (is_not_pid(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_external_pid(BIF_ARG_2)) {
	dep = external_pid_dist_entry(BIF_ARG_2);
	if(dep == erts_this_dist_entry)
	    BIF_ERROR(BIF_P, BADARG);
	if (is_nil(dep->cid))
	    BIF_TRAP2(dgroup_leader_trap, BIF_P, BIF_ARG_1, BIF_ARG_2);
	if ((code = dist_group_leader(dep, BIF_ARG_1, BIF_ARG_2)) == 1) {
	    ASSERT(is_internal_port(dep->cid)); 
	    erl_suspend(BIF_P, dep->cid);
	    BIF_ERROR(BIF_P, RESCHEDULE);
	}
	else if (code < 0) {  /* XXX is this the correct behaviour ??? */
	    BIF_ERROR(BIF_P, EXC_NOTALIVE);
	}
	BIF_RET(am_true);
    }
    else if (is_internal_pid(BIF_ARG_2)) {
	if ((new_member = pid2proc(BIF_ARG_2)) == NULL) {
	    BIF_ERROR(BIF_P, BADARG);
	}
    }
    else {
	BIF_ERROR(BIF_P, BADARG);
    }

    /*
     * XXX Multi-thread note: Allocating on another process's heap.
     */
    new_member->group_leader = STORE_NC_IN_PROC(new_member, BIF_ARG_1);
    BIF_RET(am_true);
}
    
BIF_RETTYPE system_flag_2(BIF_ALIST_2)    
{
    int n;

    if (BIF_ARG_1 == am_fullsweep_after) {
	int oval = erts_max_gen_gcs;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	erts_max_gen_gcs = n;
	if ((Uint) n > (Uint16) -1) {
	    erts_max_gen_gcs = (Uint16) -1;
	}
	BIF_RET(make_small(oval));
    } else if (BIF_ARG_1 == am_min_heap_size) {
	int oval = H_MIN_SIZE;
	if (!is_small(BIF_ARG_2) || (n = signed_val(BIF_ARG_2)) < 0) {
	    goto error;
	}
	H_MIN_SIZE = erts_next_heap_size(n, 0);
	BIF_RET(make_small(oval));
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
    } else if (BIF_ARG_1 == am_trace_control_word) {
	BIF_RET(db_set_trace_control_word_1(BIF_P, BIF_ARG_2));
    } else if (BIF_ARG_1 == am_sequential_tracer) {
        Eterm old_value;

	if (is_internal_pid(system_seq_tracer)
	    || is_internal_port(system_seq_tracer)) {
	    old_value = system_seq_tracer;
	} else {
	    old_value = am_false;
	}

	if (BIF_ARG_2 == am_false) {
	    system_seq_tracer = NIL;
	    BIF_RET(old_value);
	} else if (is_internal_pid(BIF_ARG_2)) {
	    if (internal_pid_index(BIF_ARG_2) < erts_max_processes) {
		Process* tracer = process_tab[internal_pid_index(BIF_ARG_2)];

		if (!INVALID_PID(tracer, BIF_ARG_2)) {
		    system_seq_tracer = BIF_ARG_2;
		    BIF_RET(old_value);
		}
	    }
	} else if (is_internal_port(BIF_ARG_2)) {
	    if (! INVALID_TRACER_PORT(erts_port+internal_port_index(BIF_ARG_2), 
				      BIF_ARG_2)) {
		system_seq_tracer = BIF_ARG_2;
		BIF_RET(old_value);
	    }
	}
    } else if (BIF_ARG_1 == make_small(1)) {
	Uint i;
	ErlMessage* mp;
	for (i = 0; i < erts_max_processes; i++) {
	    if (process_tab[i] != (Process*) 0) {
		Process* p = process_tab[i];
		p->seq_trace_token = NIL;
		p->seq_trace_clock = 0;
		p->seq_trace_lastcnt = 0;
		mp = p->msg.first;
		while(mp != NULL) {
		    ERL_MESSAGE_TOKEN(mp) = NIL;
		    mp = mp->next;
		}
	    }
	}
	BIF_RET(am_true);    
    }
    error:
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

BIF_RETTYPE hash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Sint32 range;

    if (is_not_small(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((range = signed_val(BIF_ARG_2)) <= 0) {  /* [1..MAX_SMALL] */
	BIF_ERROR(BIF_P, BADARG);
    }
    hash = make_broken_hash(BIF_ARG_1, 0);
    BIF_RET(make_small(1 + (hash % range)));   /* [1..range] */
}

BIF_RETTYPE phash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;
    Sint32 tmp;

    /* Assumes that small's are max 2^32 */
    ASSERT(MAX_SMALL == ((1 << (27))-1));
    if (is_big(BIF_ARG_2)) {
	if (big_fits_in_uint32(BIF_ARG_2)) {
	    range = big_to_uint32(BIF_ARG_2);
	} else {
	    /* Check for special case 2^32 */
	    Eterm *bp = big_val(BIF_ARG_2);
	    if (BIG_SIZE(bp) == 3 && !BIG_DIGIT(bp,0) && !BIG_DIGIT(bp,1) &&
		BIG_DIGIT(bp,2) == 1) {
		range = 0;
	    } else {
	    error:
		BIF_ERROR(BIF_P, BADARG);  
	    }
	} 
    } else if(is_small(BIF_ARG_2)) {
	if ((tmp = signed_val(BIF_ARG_2)) <= 0) {
	    goto error;
	}
	range = (Uint32) tmp;
    } else {
	goto error;
    }
    hash = make_hash(BIF_ARG_1, 0);
    if (range) {
	final_hash = 1 + (hash % range); /* [1..range] */
    } else if ((final_hash = hash + 1) == 0) {
	/*
	 * XXX In this case, there will still be a ArithAlloc() in erts_mixed_plus().
	 */
	BIF_RET(erts_mixed_plus(BIF_P,
				make_small_or_big(hash, BIF_P),make_small(1)));
    }

    /*
     * Return either a small or a big. Use the heap for bigs if there is room.
     */
    if (IS_USMALL(0, final_hash)) {
	BIF_RET(make_small(final_hash));
    } else {
	Eterm* hp = HAlloc(BIF_P, BIG_NEED_SIZE(2));
	BIF_RET(uint_to_big(final_hash, hp));
    }
}

BIF_RETTYPE phash2_1(BIF_ALIST_1)
{
    Uint32 hash;

    hash = make_hash2(BIF_ARG_1);
    BIF_RET(make_small(hash & MAX_SMALL));
}

BIF_RETTYPE phash2_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;
    Sint32 tmp;

    /* Assumes that small's are max 2^32 */
    ASSERT(sizeof(Uint) == 4);
    if (is_big(BIF_ARG_2)) {
	if (big_fits_in_uint32(BIF_ARG_2)) {
	    range = big_to_uint32(BIF_ARG_2);
	} else {
	    /* Check for special case 2^32 */
	    Eterm *bp = big_val(BIF_ARG_2);
	    if (BIG_SIZE(bp) == 3 && !BIG_DIGIT(bp,0) && !BIG_DIGIT(bp,1) &&
		BIG_DIGIT(bp,2) == 1) {
		range = 0;
	    } else {
	    error:
		BIF_ERROR(BIF_P, BADARG);  
	    }
	} 
    } else if(is_small(BIF_ARG_2)) {
	if ((tmp = signed_val(BIF_ARG_2)) <= 0) {
	    goto error;
	}
	range = (Uint32) tmp;
    } else {
	goto error;
    }
    hash = make_hash2(BIF_ARG_1);
    if (range) {
	final_hash = hash % range; /* [0..range-1] */
    } else {
	final_hash = hash;
    }
    /*
     * Return either a small or a big. Use the heap for bigs if there is room.
     */
    if (IS_USMALL(0, final_hash)) {
	BIF_RET(make_small(final_hash));
    } else {
	Eterm* hp = HAlloc(BIF_P, BIG_NEED_SIZE(2));
	BIF_RET(uint_to_big(final_hash, hp));
    }
}

BIF_RETTYPE bump_reductions_1(BIF_ALIST_1)
{
    int reds;
	
    if (is_not_small(BIF_ARG_1) || ((reds = signed_val(BIF_ARG_1)) < 0)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (reds > CONTEXT_REDS) {
	reds = CONTEXT_REDS;
    }
    BIF_RET2(am_true, reds);
}


/****************************************************************************
** BIF Timer support
****************************************************************************/

#define BTR_FLG_SL_TIMER (1 << 0)

typedef struct bif_timer_rec {
    struct bif_timer_rec* next;
    ErlTimer tm;
    ErlHeapFragment* bp;
    Uint32 flags;
    Eterm message;
    Eterm pid;
    Eterm ref;
    Uint  ref_heap[REF_THING_SIZE];
} BifTimerRec;

#define TIMER_HASH_VEC 3331

static BifTimerRec* bif_tm_vec[TIMER_HASH_VEC];  
static Uint no_bif_timers;

Uint
bif_timer_memory_size(void)
{
    return (sizeof(BifTimerRec *)*TIMER_HASH_VEC
	    + no_bif_timers*sizeof(BifTimerRec));
}

static BifTimerRec** find_timer(Eterm ref)
{
    Eterm wd = ref_numbers(ref)[0];
    int ix = wd % TIMER_HASH_VEC;
    BifTimerRec** tp = &bif_tm_vec[ix];

    while(*tp != NULL) {
	if (eq(ref, (*tp)->ref))
	    return tp;
	tp = &(*tp)->next;
    }
    return NULL;
}

static void
bif_timeout_proc(BifTimerRec* btm)
{
    Process* rp;
    BifTimerRec** tp = find_timer(btm->ref);
    int invalid_pid;

    ASSERT((tp != NULL) && (*tp == btm));
    *tp = btm->next;

    if (is_atom(btm->pid)) {
	rp = whereis_process(btm->pid);
	invalid_pid = (rp == NULL);
    } else {
	rp = internal_pid_index(btm->pid) < erts_max_processes ? 
	    process_tab[internal_pid_index(btm->pid)] : NULL;
	invalid_pid = (INVALID_PID(rp, btm->pid));
    }

    if (invalid_pid) {
	free_message_buffer(btm->bp);
    } else {
	queue_message_tt(rp, btm->bp, btm->message, NIL);
    }
    ERTS_PROC_LESS_MEM(sizeof(BifTimerRec));
    ASSERT(no_bif_timers > 0);
    no_bif_timers--;
    if (btm->flags & BTR_FLG_SL_TIMER)
	erts_free(ERTS_ALC_T_SL_BIF_TIMER, (void *) btm);
    else
	erts_free(ERTS_ALC_T_LL_BIF_TIMER, (void *) btm);
}

/* tm arg contains the BifTimerRec */
static void bif_cancel_proc(BifTimerRec* btm)
{
    free_message_buffer(btm->bp);
    ERTS_PROC_LESS_MEM(sizeof(BifTimerRec));
    ASSERT(no_bif_timers > 0);
    no_bif_timers--;
    if (btm->flags & BTR_FLG_SL_TIMER)
	erts_free(ERTS_ALC_T_SL_BIF_TIMER, (void *) btm);
    else
	erts_free(ERTS_ALC_T_LL_BIF_TIMER, (void *) btm);
}

static BifTimerRec* 
do_timer(int pack, Process *process, Eterm arg1, Eterm arg2, Eterm arg3)
{
    BifTimerRec* btm;
    ErlHeapFragment* bp;
    Uint timeout;
    Uint size;
    Eterm term, msg;
    Eterm ref;
    Eterm* hp;
    int ix;
    
    if (!term_to_Uint(arg1, &timeout))
	return NULL;
    if (is_not_internal_pid(arg2) && is_not_atom(arg2))
	return NULL;
    
    ref = new_ref(process);
    msg = arg3;
    
    if (pack) {
	hp = HAlloc(process, 4);
	term = TUPLE3(hp, am_timeout, ref, msg);
    } else {
	term = msg;
    }
    
    size = size_object(term);
    ERTS_PROC_MORE_MEM(sizeof(BifTimerRec));
    no_bif_timers++;
    if (timeout < ERTS_ALC_MIN_LONG_LIVED_TIME) {
	btm = (BifTimerRec *) erts_alloc(ERTS_ALC_T_SL_BIF_TIMER,
					 sizeof(BifTimerRec));
	btm->flags = BTR_FLG_SL_TIMER;
    }
    else {
	btm = (BifTimerRec *) erts_alloc(ERTS_ALC_T_LL_BIF_TIMER,
					 sizeof(BifTimerRec));
	btm->flags = 0;
    }

    btm->bp = bp = new_message_buffer(size);
    sys_memcpy((void *) btm->ref_heap,
	       (void *) ref_thing_ptr(ref),
	       sizeof(RefThing));
    btm->ref = make_internal_ref(btm->ref_heap);
    btm->pid = arg2;
    ix = ref_numbers(ref)[0] % TIMER_HASH_VEC;
    btm->next = bif_tm_vec[ix];
    bif_tm_vec[ix] = btm;
    hp = bp->mem;
    btm->message = copy_struct(term, size, &hp, &bp->off_heap);
    btm->tm.active = 0; /* MUST be initalized */
    erl_set_timer(&btm->tm,
		  (ErlTimeoutProc) bif_timeout_proc,
		  (ErlCancelProc) bif_cancel_proc,
		  (void*)btm,
		  timeout);
    
    return btm;
}

/* send_after(Time, Pid, Message) -> Ref */
BIF_RETTYPE send_after_3(BIF_ALIST_3)
{
    BifTimerRec* btm;

    btm = do_timer(0, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (btm != NULL)
	BIF_RET(STORE_NC_IN_PROC(BIF_P, btm->ref));
    else
	BIF_ERROR(BIF_P, BADARG);
}

/* start_timer(Time, Pid, Message) -> Ref */
BIF_RETTYPE start_timer_3(BIF_ALIST_3)
{
    BifTimerRec* btm;

    btm = do_timer(1, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);

    if (btm != NULL)
       BIF_RET(STORE_NC_IN_PROC(BIF_P, btm->ref));
    else
       BIF_ERROR(BIF_P, BADARG);
}

/* cancel_timer(Ref)) -> Bool */
BIF_RETTYPE cancel_timer_1(BIF_ALIST_1)
{
    BifTimerRec* btm;
    BifTimerRec** tp;
    Uint left;

    if (is_not_ref(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    if ((tp = find_timer(BIF_ARG_1)) != NULL) {
	btm = *tp;
	*tp = btm->next;
	left = time_left(&btm->tm);
	erl_cancel_timer(&btm->tm);
	BIF_RET(make_small_or_big(left, BIF_P));
    }
    else 
	BIF_RET(am_false);
}

/* read_timer(Ref) -> false | RemainingTime */
BIF_RETTYPE read_timer_1(BIF_ALIST_1)
{
    BifTimerRec** tp;

    if (is_not_ref(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((tp = find_timer(BIF_ARG_1)) == NULL) {
	BIF_RET(am_false);
    } else {
	Uint left = time_left(&(*tp)->tm);
	BIF_RET(make_small_or_big(left, BIF_P));
    }
}

void print_timer_info(CIO to)
{
   int i;
   BifTimerRec *p;

   for (i = 0; i < TIMER_HASH_VEC; i++) {
       if ((p = bif_tm_vec[i]) != NULL) {
	   erl_printf(to, "=timer:");
	   display(p->pid, to);
	   erl_printf(to, "\n");
	   erl_printf(to, "Message: ");
	   display(p->message, to);
	   erl_printf(to, "\n");
	   erl_printf(to, "Time left: %d ms\n", time_left(&p->tm));
       }
   }
}

void erts_init_bif(void)
{
    int i;

    reference0 = 0;
    reference1 = 0;
    reference2 = 0;

    no_bif_timers = 0;
    for (i = 0; i < TIMER_HASH_VEC; ++i) {
	bif_tm_vec[i] = NULL;
    }
}

BIF_RETTYPE blocking_read_file_1(BIF_ALIST_1)
{
    Eterm bin;
    Eterm* hp;
    byte *buff;
    int i, buff_size;
    FILE *file;
    struct stat file_info;
 
    /* turn the argument into a C string */
    if ((i = intlist_to_buf(BIF_ARG_1,tmp_buf,TMP_BUF_SIZE-1)) < 0) {
	BIF_ERROR(BIF_P, BADARG);
    }
    tmp_buf[i] = '\0';		/* null terminal */
 
    /* the file name is in tmp_buf */
    hp = HAlloc(BIF_P, 3);
 
    file = fopen(tmp_buf, "r");
    if(file == NULL){
	BIF_RET(TUPLE2(hp, am_error, am_nofile));
    }
 
    stat(tmp_buf, &file_info);  
    buff_size = file_info.st_size;
    buff = (byte *) erts_alloc_fnf(ERTS_ALC_T_TMP, buff_size);
    if (!buff) {
	fclose(file);
	BIF_RET(TUPLE2(hp, am_error, am_allocator));
    }
    fread(buff, 1, buff_size, file);
    fclose(file);
    bin = new_binary(BIF_P, buff, buff_size);
    erts_free(ERTS_ALC_T_TMP, (void *) buff);
 
    BIF_RET(TUPLE2(hp, am_ok, bin));
}
#ifdef HARDDEBUG
/*
You'll need this line in bif.tab to be able to use this debug bif

bif erlang:send_to_logger/2

*/
BIF_RETTYPE send_to_logger_2(BIF_ALIST_2)
{
    int len;
    if (!is_atom(BIF_ARG_1) || !(is_list(BIF_ARG_2) ||
				 is_nil(BIF_ARG_1))) {
	BIF_ERROR(BIF_P,BADARG);
    }
    cerr_pos = 0;
    if ((len = io_list_to_buf(BIF_ARG_2, tmp_buf, TMP_BUF_SIZE)) < 0) {
	BIF_ERROR(BIF_P,BADARG);
    }
    cerr_pos = TMP_BUF_SIZE - len;
    switch (BIF_ARG_1) {
    case am_info:
	erts_send_info_to_logger(BIF_P->group_leader,tmp_buf,cerr_pos);
	break;
    case am_warning:
	erts_send_warning_to_logger(BIF_P->group_leader,tmp_buf,cerr_pos);
	break;
    case am_error:
	erts_send_error_to_logger(BIF_P->group_leader,tmp_buf,cerr_pos);
	break;
    case am_false:
	send_error_to_logger(BIF_P->group_leader);
	break;
    default:
	{
	    BIF_ERROR(BIF_P,BADARG);
	}
    }
    BIF_RET(am_true);
}
#endif /* HARDDEBUG */
