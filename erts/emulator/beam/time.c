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
 * TIMING WHEEL
 * 
 * Timeouts kept in an wheel. A timeout is measured relative to the
 * current slot (tiw_pos) in the wheel, and inserted at slot 
 * (tiw_pos + timeout) % TIW_SIZE. Each timeout also has a count
 * equal to timeout/TIW_SIZE, which is needed since the time axis
 * is wrapped arount the wheel. 
 *
 * Several slots may be processed in one operation. If the number of
 * slots is greater that the wheel size, the wheel is only traversed
 * once,
 *
 * The following example shows a time axis where there is one timeout
 * at each "tick", and where 1, 2, 3 ... wheel slots are released in
 * one operation. The notation "<x" means "release all items with
 * counts less than x". 
 *
 * Size of wheel: 4
 * 
 *   --|----|----|----|----|----|----|----|----|----|----|----|----|----
 *    0.0  0.1  0.2  0.3  1.0  1.1  1.2  1.3  2.0  2.1  2.2  2.3  3.0
 * 
 * 1   [    )
 *     <1  0.1  0.2  0.3  0.0  1.1  1.2  1.3  1.0  2.1  2.2  2.3  2.0
 * 
 * 2   [         )
 *     <1   <1  0.2  0.3  0.0  0.1  1.2  1.3  1.0  1.1  2.2  2.3  2.0
 * 
 * 3   [              )
 *     <1   <1   <1  0.3  0.0  0.1  0.2  1.3  1.0  1.1  1.2  2.3  2.0
 * 
 * 4   [                   )
 *     <1   <1   <1   <1  0.0  0.1  0.2  0.3  1.0  1.1  1.2  1.3  2.0
 * 
 * 5   [                        )
 *     <2   <1   <1   <1.      0.1  0.2  0.3  0.0  1.1  1.2  1.3  1.0
 * 
 * 6   [                             )
 *     <2   <2   <1   <1.           0.2  0.3  0.0  0.1  1.2  1.3  1.0
 * 
 * 7   [                                  )
 *     <2   <2   <2   <1.                0.3  0.0  0.1  0.2  1.3  1.0
 * 
 * 8   [                                       )   
 *     <2   <2   <2   <2.                     0.0  0.1  0.2  0.3  1.0
 * 
 * 9   [                                            )
 *     <3   <2   <2   <2.                          0.1  0.2  0.3  0.0
 * 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"

#define TIW_SIZE 8192   /* timing wheel size (should be a power of 2) */
static ErlTimer** tiw;  /* the timing wheel, allocated in init_time() */
static uint32 tiw_pos;  /* current position in wheel */
static uint32 tiw_nto;  /* number of timeouts in wheel */
static ErlTimer* tm_list; /* new timers created while bumping */

/* Actual interval time chosen by sys_init_time() */
static int itime;
static int bump_lock;  /* set while bumping */


void increment_time(ticks)
int ticks;
{
    do_time += ticks;
}

static void insert_timer(p, t)
ErlTimer* p; uint32 t;
{
    uint32 tm;
    uint32 ticks;

    /* The current slot (tiw_pos) in timing wheel is the next slot to be
     * be processed. Hence no extra time tick is needed.
     *
     * (x + y - 1)/y is precisely the "number of bins" formula.
     */
    ticks = (t + itime - 1) / itime;
    ticks += do_time;		/* Add backlog of unprocessed time */
    
    /* calculate slot */
    tm = (ticks + tiw_pos) % TIW_SIZE;
    p->slot = (uint32)tm;
    p->count = (uint32)(ticks / TIW_SIZE);
  
    /* insert at head of list at slot */
    p->next = tiw[tm];
    tiw[tm] = p;
    tiw_nto++;
}

void bump_timer()
{
    uint32 keep_pos, count;
    ErlTimer* p;
    ErlTimer** prev;
    uint32 dtime = do_time;  /* local copy */

    do_time = 0;
    /* no need to bump the position if there aren't any timeouts */
    if (tiw_nto == 0)
	return;
    bump_lock = 1; /* avoid feedback loops in timeout proc */
    /* if do_time > TIW_SIZE we want to go around just once */
    count = (uint32)(dtime / TIW_SIZE) + 1;
    keep_pos = (tiw_pos + dtime) % TIW_SIZE;
    if (dtime > TIW_SIZE) dtime = TIW_SIZE;
  
    while (dtime > 0) {
	/* this is to decrease the counters with the right amount */
	/* when dtime >= TIW_SIZE */
	if (tiw_pos == keep_pos) count--;
	prev = &tiw[tiw_pos];
	while ((p = *prev) != NULL) {
	    if (p->count < count) {     /* we have a timeout */
		*prev = p->next;	/* Remove from list */
		tiw_nto--;
		p->next = NULL;
		p->slot = 0;
		p->active = 0;
		(*p->timeout)(p->arg);  /* call timeout callback */
	    }
	    else {
		/* no timeout, just decrease counter */
		p->count -= count;
		prev = &p->next;
	    }
	}
	tiw_pos = (tiw_pos + 1) % TIW_SIZE;
	dtime--;
    }
    bump_lock = 0;
    tiw_pos = keep_pos;

    /* do_time should be 0 during timeout/set_timer feedback 
     ** tiw_pos is also updated before inserting new timers 
     */
    if ((p = tm_list) != NULL) {
	tm_list = NULL;
	while (p != NULL) {
	    ErlTimer* p_next = p->next;
	    insert_timer(p, p->count);
	    p = p_next;
	}
    }
}

/* this routine links the time cells into a free list at the start
   and sets the time queue as empty */
void init_time()
{
  int i;

  if ((tiw = (ErlTimer**)sys_alloc_from(140,TIW_SIZE * sizeof(ErlTimer*))) == NULL)
      erl_exit(1, "can't allocate timing wheel\n");
  for(i = 0; i < TIW_SIZE; i++)
      tiw[i] = NULL;
  do_time = tiw_pos = tiw_nto = 0;
  tm_list = NULL;
  bump_lock = 0;

  /* system dependant init */
  itime = erts_init_time_sup();
}

/*
** Insert a process into the time queue, with a timeout 't'
*/
void erl_set_timer(p, timeout, cancel, arg, t)
ErlTimer* p;
ErlTimeoutProc timeout;
ErlCancelProc cancel;
void* arg;
uint32 t;
{
    if (p->active)  /* XXX assert ? */
	return;
    p->timeout = timeout;
    p->cancel = cancel;
    p->arg = arg;
    p->active = 1;
    if (bump_lock) {
	p->next = tm_list;
	tm_list = p;
	p->count = t;  /* time is saved here used by bump */
    }
    else
	insert_timer(p, t);
}

void erl_cancel_timer(p)
ErlTimer* p;
{
    ErlTimer *tp;
    ErlTimer **prev;

    if (!p->active)  /* allow repeated cancel (drivers) */
	return;
    /* find p in linked list at slot p->slot and remove it */
    prev = &tiw[p->slot];
    while ((tp = *prev) != NULL) {
	if (tp == p) {
	    *prev = p->next;	/* Remove from list */
	    tiw_nto--;
	    p->next = NULL;
	    p->slot = p->count = 0;
	    p->active = 0;
	    if (p->cancel != NULL)
		(*p->cancel)(p->arg);
	    break;
	}
	prev = &tp->next;
    }
}

/* get the time (in units of itime) to the next timeout,
   or -1 if there are no timeouts                     */
 
int next_time()
{
    int i, tm, nto;
    unsigned int min;
    ErlTimer* p;
  
    if (tiw_nto == 0) return -1;        /* no timeouts in wheel */
  
    /* start going through wheel to find next timeout */
    tm = nto = 0;
    min = (unsigned int) -1;	/* max unsigned int */
    i = tiw_pos;
    do {
	p = tiw[i];
	while (p != NULL) {
	    nto++;
	    if (p->count == 0) {
		/* found next timeout */
		return ((tm >= do_time) ? (tm - do_time) : 0);
	    } else {
		/* keep shortest time in 'min' */
		if (tm + p->count*TIW_SIZE < min)
		    min = tm + p->count*TIW_SIZE;
	    }
	    p = p->next;
	}
	/* when we have found all timeouts the shortest time will be in min */
	if (nto == tiw_nto) break;
	tm++;
	i = (i + 1) % TIW_SIZE;
    } while (i != tiw_pos);
    return ((min >= do_time) ? (min - do_time) : 0);
}

/*
  Returns the amount of time left in ms until the timer 'p' is triggered.
  0 is returned if 'p' isn't active.
  0 is returned also if the timer is overdue (i.e., would have triggered
  immediately if it hadn't been cancelled).
*/
uint32 time_left(p)
ErlTimer *p;
{
    uint32 left;

    if (!p->active)
	return 0;

    if (p->slot < tiw_pos)
	left = (p->count + 1) * TIW_SIZE + p->slot - tiw_pos;
    else
	left = p->count * TIW_SIZE + p->slot - tiw_pos;
    if (left < do_time)
	left = 0;
    else
	left -= do_time;

    return left * itime;
}

#ifdef DEBUG

void p_slpq()
{
    int i;
    ErlTimer* p;
  
    /* print the whole wheel, starting at the current position */
    erl_printf(COUT, "\ntiw_pos = %d tiw_nto %d\n\r", tiw_pos, tiw_nto);
    i = tiw_pos;
    if (tiw[i] != NULL) {
	erl_printf(COUT, "%d:\n\r", i);
	for(p = tiw[i]; p != NULL; p = p->next) {
	    erl_printf(COUT, " (count %d, slot %d)\n\r",
		       p->count, p->slot);
	}
    }
    for(i = (i+1)%TIW_SIZE; i != tiw_pos; i = (i+1)%TIW_SIZE) {
	if (tiw[i] != NULL) {
	    erl_printf(COUT, "%d:\n\r", i);
	    for(p = tiw[i]; p != NULL; p = p->next) {
		erl_printf(COUT, " (count %d, slot %d)\n\r",
			   p->count, p->slot);
	    }
	}
    }
}

#endif /* DEBUG */
