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
 * Purpose: driver_select() and friends.
 */

/***********************************************************************
  Roadmap to driver_select() & friends.

  The driver_select() function is basically a call to
  WaitForMultipleObjects().  But there are two problems
  with the WFMO() function: It can only handle 64 objects,
  and it will only point out the first element selected.

  To work around these problems, this module uses one or
  more threads (called "waiters"), each of which waits for 63 objects.
  One object (the first one in the array) is use to force
  the thread into a standby mode, when objects need to be
  inspected, added or removed.

  Each threads keeps two parallell arrays, one with events
  and the other the port number and other needed information.
  These arrays are organized as follows:

     +-------------------+ \                \
 0   | Enter standby     |  |		     |
     +-------------------+  |		     |
 1   | Object 1   	 |  |		     |
     +-------------------+  |		     |
 2   | Object 2   	 |   > active_events |
     +-------------------+  |		     |
           .                |		     |
	   .                |		     |
	   .                |		      > total_events
     +-------------------+  |		     |
N-1  | Object N-1   	 | /		     |
     +-------------------+  		     |
 N   | Object N          |		     |
     +-------------------+		     |
           .              		     |
	   .               		     |
	   .				     |
     +-------------------+		     |
N+M  | Object N+M        |		    /
     +-------------------+

  The first N objects are active, i.e. part of the array passed
  to the WFMO().  The rest are objects already signaled.
  When the thread discovers a new signaled object, it will swap it
  with another object and decrement active_events.

  The main emulator thread periodically orders all waiter threads
  to enter standby mode.  Then the main thread call the callback
  functions for all signaled objects in the upper end of each
  array.  Finally, it sets active_events = total_events and
  order all threads to start.
***********************************************************************/

#include "sys.h"
#include "erl_driver.h"

/*
 * The following values are non-zero, constant, odd, large, and atypical.
 *      Non-zero values help find bugs assuming zero filled data.
 *      Constant values are good so that memory filling is deterministic
 *          (to help make bugs reproducable).  Of course it is bad if
 *          the constant filling of weird values masks a bug.
 *      Mathematically odd numbers are good for finding bugs assuming a cleared
 *          lower bit, as well as useful for trapping on the Mac.
 *      Large numbers (byte values at least) are less typical, and are good
 *          at finding bad addresses.
 *      Atypical values (i.e. not too often) are good since they typically
 *          cause early detection in code.
 *      For the case of no-man's land and free blocks, if you store to any
 *          of these locations, the memory integrity checker will detect it.
 */

#define NoMansLandFill 0xFD	/* fill no-man's land with this */
#define DeadLandFill   0xDD	/* fill free objects with this */
#define CleanLandFill  0xCD	/* fill new objects with this */

#define START_WAITER(w) \
  if (in_win_check_io) \
    ; \
  else { \
    SetEvent((w)->go_ahead); \
  }

#define STOP_WAITER(w) \
  if (in_win_check_io) \
    ; \
  else { \
    setup_standby_wait(1); \
    SetEvent((w)->events[0]); \
    wait_standby(); \
  }

#define START_WAITERS() \
  if (in_win_check_io) \
    ; \
  else { \
    int i; \
    for (i = 0; i < num_waiters; i++) { \
	SetEvent(waiter[i]->go_ahead); \
    } \
 }

#define STOP_WAITERS() \
  if (in_win_check_io) \
    ; \
  else { \
    int i; \
    setup_standby_wait(num_waiters); \
    for (i = 0; i < num_waiters; i++) { \
	SetEvent(waiter[i]->events[0]); \
    } \
    wait_standby(); \
 } while (0)


typedef void (*IoHandler)(int, int);
typedef struct _EventData EventData;

/*
 * Private functions.
 */

static int set_driver_select(int port, HANDLE event, IoHandler handler);
static int cancel_driver_select(HANDLE event);
static void new_waiter(void);
static void* checked_alloc(unsigned);
static void* checked_realloc(void *,unsigned);
static void my_do_break(int, int);
static DWORD WINAPI threaded_waiter(LPVOID param);
#ifdef DEBUG
static void consistency_check(struct _Waiter* w);
#endif

BOOL WINAPI ctrl_handler(DWORD);

/*
 * External functions.
 */

EXTERN_FUNCTION(void, input_ready, (int, int));
EXTERN_FUNCTION(void, output_ready, (int, int));
EXTERN_FUNCTION(void, erl_exit, (int n, char*, _DOTS_));
EXTERN_FUNCTION(void, do_break, (void));

/*
 * External variables.
 */

extern int nohup;
extern HANDLE erts_service_event;

typedef struct _EventData {
    HANDLE event;		/* For convenience. */
    int port;			/* Port that the event belongs to, or -1 if no
				 * no port. */
    
    IoHandler handler;		/* The function be called when selected. */
    int to_be_deleted;		/* Non-zero if a delayed cancellation is active. */
    EventData* next;		/* Next in free or delete lists. */
};

typedef struct _Waiter {
    /*
     * In the events and evdata arrays, the 0th entry is used for getting
     * the thread out of the WaitForMultipleObjects() call (auto-reset).
     */

    HANDLE events[MAXIMUM_WAIT_OBJECTS]; /* The events. */
    EventData* evdata[MAXIMUM_WAIT_OBJECTS]; /* Pointers to associated data. */


    int active_events;		/* Number of events to take part in
				 * WaitForMultipleObjects().
				 */
    int total_events;		/* Total number of events in the arrays. */

    /*
     * This is an heap of EventData objects.  This is so that the evdata
     * array can contain only pointers (for speed of exchanging events).
     */

    EventData evdata_heap[MAXIMUM_WAIT_OBJECTS];
    EventData* first_free_evdata; /* Index of first free EventData object. */

    /*
     * The following event is set by the main emulator thread.
     */

    HANDLE go_ahead;		/* The waiter may continue. (Auto-reset) */

} Waiter;

static Waiter** waiter;
static EventData* to_delete = NULL; /* List of delayed cancellations. */

static int allocated_waiters;  /* Size ow waiter array */ 
static int num_waiters;		/* Number of waiter threads. */

volatile int sys_io_ready;		/* Some I/O is ready. */
HANDLE event_io_ready;	/* Same meaning as the variable. (Manual reset) */

static volatile int in_win_check_io = FALSE;
static void (*break_func)();
static void (*quit_func)();
static HANDLE break_event;

static volatile int standby_wait_counter;
static CRITICAL_SECTION standby_crit;
static HANDLE standby_wait_event;


void
init_sys_select(void)
{
    num_waiters = 0;
    allocated_waiters = 64;
    waiter = checked_alloc(sizeof(Waiter *)*allocated_waiters);
    InitializeCriticalSection(&standby_crit);
    standby_wait_counter = 0;
    event_io_ready = CreateManualEvent(FALSE);
    standby_wait_event = CreateManualEvent(FALSE);
    break_event = CreateAutoEvent(FALSE);
    set_driver_select(0, break_event, my_do_break);
}

static void setup_standby_wait(int num_threads)
{
    EnterCriticalSection(&standby_crit);
    standby_wait_counter = num_threads;
    ResetEvent(standby_wait_event);
    LeaveCriticalSection(&standby_crit);
}

static void signal_standby(void) 
{
    EnterCriticalSection(&standby_crit);
    --standby_wait_counter;
    if (standby_wait_counter < 0) {
	LeaveCriticalSection(&standby_crit);
	erl_exit(1,"Standby signalled by more threads than expected");
    }
    if (!standby_wait_counter) {
	SetEvent(standby_wait_event);
    }
    LeaveCriticalSection(&standby_crit);
}

static void wait_standby(void)
{
    WaitForSingleObject(standby_wait_event,INFINITE);
}

/* ----------------------------------------------------------------------
 * driver_select --
 * 	Orders the emulator to inform the driver owning
 *	the filedescriptor when there is input available or output is possible.
 *
 * Results:
 *	None.
 * ----------------------------------------------------------------------
 */
int
driver_select(port, event, mode, on)
    int port;			/* The port number. */
    HANDLE event;		/* The event to wait for. */
    int mode;			/* Read or write mode. */
    int on;			/* TRUE for enabling, FALSE for disabling. */
{
    DEBUGF(("driver_select(%d, 0x%x, %d, %d)\n", port, event, mode, on));

    ASSERT(event != INVALID_HANDLE_VALUE);
    if (on) {
	IoHandler func = 0;
	if (mode & DO_READ) {
	    func = input_ready;
	} else if (mode & DO_WRITE) {
	    func = output_ready;
	}
	ASSERT(func != 0);
	if (func) {
	    return set_driver_select(port, event, func);
	}
	return 0;
    } else {
	int result;

	STOP_WAITERS();
	result = cancel_driver_select(event);
	START_WAITERS();
	return result;
    }
}

int
driver_event(port, event, event_data)
     int port;
     HANDLE event;
     ErlDrvEventData event_data;
{
    return -1;
}


static int
set_driver_select(port, event, handler)
    int port;			/* The port number. */
    HANDLE event;		/* The event to wait for. */
    IoHandler handler;		/* Handler function to be called
				 * when selected.
				 */
{
    int i;
    int best_waiter = -1;	/* The waiter with lowest number of events. */
    int lowest = MAXIMUM_WAIT_OBJECTS; /* Lowest number of events
					* in any waiter.
					*/
    EventData* ev;
    Waiter* w;

    /*
     * Find the waiter which is least busy.
     */

    for (i = 0; i < num_waiters; i++) {
	if (waiter[i]->total_events < lowest) {
	    lowest = waiter[i]->total_events;
	    best_waiter = i;
	}
    }

    /*
     * Stop the selected waiter, or start a new waiter if all were busy.
     */

    if (best_waiter >= 0) {
	w = waiter[best_waiter];
	STOP_WAITER(w);
    } else {
	new_waiter();
	w = waiter[num_waiters-1];
    }

#ifdef DEBUG
    consistency_check(w);
#endif

    /*
     * Allocate and initialize an EventData structure.
     */

    ev = w->first_free_evdata;
    w->first_free_evdata = ev->next;
    ev->event = event;
    ev->port = port;
    ev->handler = handler;
    ev->to_be_deleted = FALSE;
    ev->next = NULL;

    /*
     * At this point, the selected waiter (newly-created or not) is
     * standing by.  Put the new event into the active part of the array.
     */

    if (w->active_events < w->total_events) {
	/*
	 * Move the first event beyond the active part of the array to
	 * the very end to make place for the new event.
	 */

	w->events[w->total_events] = w->events[w->active_events];
	w->evdata[w->total_events] = w->evdata[w->active_events];
    }
    w->events[w->active_events] = event;
    w->evdata[w->active_events] = ev;
    w->active_events++;
    w->total_events++;

#ifdef DEBUG
    consistency_check(w);
#endif
    START_WAITER(w);
    return 0;
}


static int
cancel_driver_select(event)
    HANDLE event;		/* The event to stop waiting for. */
{
    int i;
    int result = -1;

    ASSERT(event != INVALID_HANDLE_VALUE);
    for (i = 0; i < num_waiters; i++) {
	Waiter* w = waiter[i];
	int j;

#ifdef DEBUG
	consistency_check(w);
#endif
	for (j = 0; j < w->total_events; j++) {
	    if (w->events[j] == event) {

		/*
		 * If win_check_io() is active, we must delay the deletion.
		 */

		if (in_win_check_io) {

		    /*
		     * Note that an event might be cancelled several times.
		     * In that case, it is essential to not put the event
		     * several times on the list of delayed cancellations.
		     */

		    if (w->evdata[j]->to_be_deleted == FALSE) {
			w->evdata[j]->next = to_delete;
			to_delete = w->evdata[j];
			w->evdata[j]->to_be_deleted = TRUE;
		    }
		    return 0;
		}

		/*
		 * Free the event's EventData structure.
		 */

		w->evdata[j]->event = INVALID_HANDLE_VALUE;
		w->evdata[j]->port = -1;
		w->evdata[j]->handler = 0;
		w->evdata[j]->to_be_deleted = FALSE;
		w->evdata[j]->next = w->first_free_evdata;
		w->first_free_evdata = w->evdata[j];

		/*
		 * If the event is active, we will overwrite it
		 * with the last active event and make the hole
		 * the first non-active event.
		 */

		if (j < w->active_events) {
		    w->active_events--;
		    w->events[j] = w->events[w->active_events];
		    w->evdata[j] = w->evdata[w->active_events];
		    j = w->active_events;
		}

		/*
		 * Overwrite the event (or the hole created above)
		 * with the last event in the array.
		 */

		w->total_events--;
		w->events[j] = w->events[w->total_events];
		w->evdata[j] = w->evdata[w->total_events];

#ifdef DEBUG
		w->events[w->total_events] = (HANDLE) CleanLandFill;
		w->evdata[w->total_events] = (EventData *) CleanLandFill;
		consistency_check(w);
#endif
		result = 0;
		break;
	    }
	}
    }
    return result;
}


void
win_check_io(wait)
int wait;
{
    register int i;
    int n;
    EventData* ev;

    /*
     * If there is no I/O events ready and if we were asked to wait,
     * wait for the event until the next timer expires.
     */

    if (wait && !sys_io_ready) {
	SysTimeval tv;
	DWORD timeout;
	HANDLE harr[2] = {event_io_ready};
	int num_h = 1;

	erts_time_remaining(&tv);
	timeout = tv.tv_sec * 1000 + tv.tv_usec / 1000;

	if (erts_service_event != NULL) {
	    harr[num_h++] = erts_service_event;
	}

	WaitForMultipleObjects(num_h, harr, FALSE, timeout);
    }

    /* Poll the service event to see if we're to exit. */
    if (erts_service_event != NULL && 
	WaitForSingleObject(erts_service_event, 0) == WAIT_OBJECT_0) {
	    erl_exit(0,"");
    }

    /*
     * Deliver the current time and get out of here if there are no
     * I/O events ready (meaning that a timer has expired).
     */

    if (!sys_io_ready) {
	erts_deliver_time(NULL);
	return;
    }

    /*
     * Since we are still here, we know that there one or more events
     * are ready.  Find out which they are.  Since stopping the waiters
     * involves context switches, we don't want to do sys_deliver_time()
     * until waiters are stopped.
     */

    STOP_WAITERS();
    sys_io_ready = 0;
    ResetEvent(event_io_ready);

    n = num_waiters;		/* Essential; so that we won't iterate over
				 * a newly inserted waiter.
				 */

    erts_deliver_time(NULL);
    in_win_check_io = TRUE;
    for (i = 0; i < n; i++) {
	Waiter* w = waiter[i];
	int j;
	int first;
	int last;

#ifdef DEBUG
	consistency_check(w);
#endif

	/*
	 * The following code is essential to make sure that insertions
	 * caused by calls to driver_select() from any of the rotines below
	 * will work.  Picking up "first" and "last" makes sure that we
	 * won't iterate over any new entries.  Setting w->active_events
	 * makes sure set_driver_select() will insert the new entries
	 * at the end without moving old entries.
	 */

	first = w->active_events;
	last = w->total_events;
	w->active_events = w->total_events;

	/*
	 * Now call the routines for all selected events.
	 *
	 * Deletions and the macros STOP and START macros for waiters will
	 * be disabled while the callbacks are active.
	 */

	for (j = first; j < last; j++) {
	    ASSERT(w->evdata[j]->port != -1);
	    ASSERT(w->events[j] != INVALID_HANDLE_VALUE);
	    (*(w->evdata[j]->handler))(w->evdata[j]->port, (int) w->events[j]);
	}
	w->active_events = w->total_events; /* In case something was inserted. */

#ifdef DEBUG
	consistency_check(w);
#endif
    }
    in_win_check_io = FALSE;

    /*
     * If any of the routines above called driver_select() to cancel
     * a selection, these cancellations will be done now.
     */
    
    while ((ev = to_delete) != NULL) {
	to_delete = ev->next; /* Essential to get the pointer before
			       * the cancellation, before the struct
			       * will be put into the free list.
			       */
	DEBUGF(("Delayed cancellation of 0x%x\n", ev->event));
	cancel_driver_select(ev->event);
    }

    START_WAITERS();
}


static void
my_do_break(int dummy1, int dummy2)
{
    do_break();
}

BOOL WINAPI
ctrl_handler(DWORD dwCtrlType)
{
    switch (dwCtrlType) {
    case CTRL_C_EVENT:
    case CTRL_BREAK_EVENT:
	SetEvent(break_event);
	break;
    case CTRL_LOGOFF_EVENT:
	if (nohup)
	    return TRUE;
	/* else pour through... */
    case CTRL_CLOSE_EVENT:
    case CTRL_SHUTDOWN_EVENT:
	erl_exit(0, "");
	break;
    }
    return TRUE;
}

void init_break_handler()
{
    ConSetCtrlHandler(ctrl_handler);
    SetConsoleCtrlHandler(ctrl_handler, TRUE);
}

static void*
checked_alloc(size)
     unsigned int size;		/* Amount to allocate. */
{
    void* p;

    if ((p = sys_alloc(size)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", size);
#ifdef DEBUG
    memset(p, CleanLandFill, size);
#endif
    return p;
}

static void*
checked_realloc(ptr,size)
     void *ptr;
     unsigned int size;		/* Amount to allocate. */
{
    void* p;

    if ((p = sys_realloc(ptr,size)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", size);
#ifdef DEBUG
    memset(p, CleanLandFill, size);
#endif
    return p;
}


static void
new_waiter(void)
{
    register Waiter* w;
    DWORD tid;			/* Id for thread. */
    HANDLE thread;
    int i;

    if (num_waiters == allocated_waiters) {
	allocated_waiters += 64;
	waiter = checked_realloc(waiter,sizeof(Waiter *)*allocated_waiters);
    }
	
    /*
    if (num_waiters == MAXIMUM_WAIT_OBJECTS) {
	erl_exit(1, "Can't wait for more than %d events",
		 MAXIMUM_WAIT_OBJECTS * MAXIMUM_WAIT_OBJECTS);
    }
    */
    w = (Waiter *) checked_alloc(sizeof(Waiter));
    waiter[num_waiters] = w;

    w->events[0] = CreateAutoEvent(FALSE);
    w->evdata[0] = NULL;	/* Should never be used. */
    w->active_events = 1;
    w->total_events = 1;

    /*
     * Form the free list of EventData objects.
     */

    w->evdata_heap[0].port = -1;
    w->evdata_heap[0].next = 0;	/* Last in free list. */
    for (i = 1; i < MAXIMUM_WAIT_OBJECTS; i++) {
	w->evdata_heap[i].port = -1;
	w->evdata_heap[i].next = w->evdata_heap+i-1;
    }
    w->first_free_evdata = w->evdata_heap+MAXIMUM_WAIT_OBJECTS-1;

    /*
     * Create the other events.
     */

    w->go_ahead = CreateAutoEvent(FALSE);

    /*
     * Create the thread.
     */

    thread = (HANDLE) _beginthreadex(NULL, 0, threaded_waiter, w, 0, &tid);
    if (thread == (HANDLE) -1) {
	erl_exit(1, "Can't create more threads in driver_select()");
    }
    CloseHandle(thread);

    /*
     * Finally, done.
     */

    num_waiters++;
}

#ifdef DEBUG
static void
consistency_check(Waiter* w)
{
    int i;
    
    ASSERT(w->active_events <= w->total_events);
    ASSERT(w->evdata[0] == NULL);

    for (i = 1; i < w->total_events; i++) {
	ASSERT(w->events[i] == w->evdata[i]->event);
	ASSERT(w->evdata[i]->port != -1);
    }
}

#endif



static DWORD WINAPI
threaded_waiter(LPVOID param)
{
    register Waiter* w = (Waiter *) param;

 again:
    WaitForSingleObject(w->go_ahead, INFINITE);
    if (w->active_events == 0) {
	return 0;
    }
    ASSERT(w->evdata[0] == NULL);

    for (;;) {
	int i;
	i = WaitForMultipleObjects(w->active_events, w->events, FALSE, INFINITE);
	switch (i) {
	case WAIT_FAILED:
	    DEBUGF(("Wait failed: %s\n", last_error()));
	    for (i = 0; i < w->active_events; i++) {
		if (WaitForSingleObject(w->events[i], 0) == WAIT_FAILED) {
		    DEBUGF(("Invalid handle: i = %d, handle = 0x%0x\n",
			    i, w->events[i]));
		    erl_exit(1, "Invalid handle %d in WaitForMultipleObjects",
			     w->events[i]);
		}
	    }
	    ASSERT(0);		/* Can't happen. */
	    break;
	case WAIT_OBJECT_0:
	    signal_standby();
	    goto again;
#ifdef DEBUG
	case WAIT_TIMEOUT:
	    ASSERT(0);
#endif
	default:
#ifdef DEBUG
	    consistency_check(w);
#endif
	    ASSERT(WAIT_OBJECT_0 < i && i < WAIT_OBJECT_0+w->active_events);

	    if (w->active_events == w->total_events) {
		/*
		 * To avoid race conditions in win_check_io(),
		 * sys_io_ready should be set before setting the event.
		 */
		sys_io_ready = 1;
		SetEvent(event_io_ready);
	    }
	    ASSERT(i >= WAIT_OBJECT_0+1);
	    i -= WAIT_OBJECT_0;
	    ASSERT(i >= 1);
	    w->active_events--;
	    if (i < w->active_events) {
		HANDLE te = w->events[i];
		EventData* tp = w->evdata[i];
		w->events[i] = w->events[w->active_events];
		w->evdata[i] = w->evdata[w->active_events];
		w->events[w->active_events] = te;
		w->evdata[w->active_events] = tp;
	    }
#ifdef DEBUG
	    consistency_check(w);
#endif
	    break;
	}
    }
}


