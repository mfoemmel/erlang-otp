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
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define ERTS_WANT_BREAK_HANDLING
#define WANT_NONBLOCKING 
#include "sys.h"
#include "global.h"
#include "erl_driver.h"
#include "erl_poll.h"
#include "erl_check_io.h"
#include "erl_alloc.h"

#define ERTS_EV_FLG_IGNORE		(((short) 1) << 0)

#define ERTS_EV_TYPE_NONE		((short) 0)
#define ERTS_EV_TYPE_DRV_SEL		((short) 1)
#define ERTS_EV_TYPE_DRV_EV		((short) 2)

#define ERTS_DRV_EV_STATE_EXTRA_SIZE 128

#if defined(ERTS_KERNEL_POLL_VERSION)
#  define ERTS_CIO_EXPORT(FUNC) FUNC ## _kp
#elif defined(ERTS_NO_KERNEL_POLL_VERSION)
#  define ERTS_CIO_EXPORT(FUNC) FUNC ## _nkp
#else
#  define ERTS_CIO_EXPORT(FUNC) FUNC
#endif

#define ERTS_CIO_HAVE_DRV_EVENT \
  (ERTS_POLL_USE_POLL && !ERTS_POLL_USE_KERNEL_POLL)

#define ERTS_CIO_POLL_CTL	ERTS_POLL_EXPORT(erts_poll_control)
#define ERTS_CIO_POLL_WAIT	ERTS_POLL_EXPORT(erts_poll_wait)
#define ERTS_CIO_POLL_INTR 	ERTS_POLL_EXPORT(erts_poll_interrupt)
#define ERTS_CIO_NEW_POLLSET 	ERTS_POLL_EXPORT(erts_poll_create_pollset)
#define ERTS_CIO_FREE_POLLSET	ERTS_POLL_EXPORT(erts_poll_destroy_pollset)
#define ERTS_CIO_POLL_MAX_FDS	ERTS_POLL_EXPORT(erts_poll_max_fds)
#define ERTS_CIO_POLL_INIT	ERTS_POLL_EXPORT(erts_poll_init)
#define ERTS_CIO_POLL_INFO	ERTS_POLL_EXPORT(erts_poll_info)

static ErtsPollSet pollset;

/*
 * ErtsDrvEventDataState is used by driver_event() which is almost never
 * used. We allocate ErtsDrvEventDataState separate since we dont wan't
 * the size of ErtsDrvEventState to increase due to driver_event()
 * information.
 */
typedef struct {
    int port;
    ErlDrvEventData data;
    ErtsPollEvents removed_events;
} ErtsDrvEventDataState;

typedef struct {
    union {
	ErtsDrvEventDataState *event;
	struct {
	    int inport;
	    int outport;
	} select;
    } driver;
    ErtsPollEvents events;
    short flags;
    short type;
} ErtsDrvEventState;

struct erts_fd_list {
    struct erts_fd_list *next;
    int fd;
};

static int max_fds = -1;
static erts_smp_mtx_t drv_ev_state_mtx;
static int drv_ev_state_len;
static ErtsDrvEventState *drv_ev_state;

static erts_smp_atomic_t in_poll_wait;

struct erts_fd_list *ignored_list;

static void select_large_fd_error(ErlDrvPort, int, int, int);
#if ERTS_CIO_HAVE_DRV_EVENT
static void event_large_fd_error(ErlDrvPort, int, ErlDrvEventData);
#endif


ERTS_QUALLOC_IMPL(fd_list, struct erts_fd_list, 64, ERTS_ALC_T_FD_LIST)
#if ERTS_CIO_HAVE_DRV_EVENT
ERTS_QUALLOC_IMPL(drv_ev_data, ErtsDrvEventDataState, 16,
		  ERTS_ALC_T_DRV_EV_D_STATE)
#endif

static ERTS_INLINE void
check_ignore(int fd, ErtsPollEvents new_evs, ErtsPollEvents old_evs)
{

    if (!new_evs
	&& old_evs
	&& !(drv_ev_state[fd].flags & ERTS_EV_FLG_IGNORE)
	&& erts_smp_atomic_read(&in_poll_wait)) {
	struct erts_fd_list *fdlp = fd_list_alloc();
	fdlp->fd = fd;
	fdlp->next = ignored_list;
	ignored_list = fdlp;
	drv_ev_state[fd].flags |= ERTS_EV_FLG_IGNORE;
    }

}

static ERTS_INLINE void
reset_ignores(void)
{
    struct erts_fd_list *fdlp = ignored_list;

    while (fdlp) {
	struct erts_fd_list *ffdlp = fdlp;
	drv_ev_state[fdlp->fd].flags &= ~ERTS_EV_FLG_IGNORE;
	fdlp = fdlp->next;
	fd_list_free(ffdlp);
    }
    ignored_list = NULL;
}

static void
grow_drv_ev_state(int min_ix)
{
    int i;
    int new_len = min_ix + 1 + ERTS_DRV_EV_STATE_EXTRA_SIZE;
    if (new_len > max_fds)
	new_len = max_fds;
    drv_ev_state = (drv_ev_state_len
		     ? erts_realloc(ERTS_ALC_T_DRV_EV_STATE,
				    drv_ev_state,
				    sizeof(ErtsDrvEventState)*new_len)
		     : erts_alloc(ERTS_ALC_T_DRV_EV_STATE,
				  sizeof(ErtsDrvEventState)*new_len));
    for (i = drv_ev_state_len; i < new_len; i++) {
	/* driver_select() depends on inport/outport being < 0
	   when not selected... */
	drv_ev_state[i].driver.select.inport = -1;
	drv_ev_state[i].driver.select.outport = -1;
	drv_ev_state[i].events = 0;
	drv_ev_state[i].flags = 0;
	drv_ev_state[i].type = ERTS_EV_TYPE_NONE;
    }
    drv_ev_state_len = new_len;
}

static void
deselect(int fd)
{
    ErtsPollEvents old_events = drv_ev_state[fd].events;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&drv_ev_state_mtx));
    ASSERT(drv_ev_state[fd].events);
    drv_ev_state[fd].events
	= ERTS_CIO_POLL_CTL(pollset, fd, drv_ev_state[fd].events, 0);
    ASSERT(drv_ev_state[fd].events == 0);
#if ERTS_CIO_HAVE_DRV_EVENT
    if (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_EV)
	drv_ev_data_free(drv_ev_state[fd].driver.event);
#endif
    drv_ev_state[fd].type = ERTS_EV_TYPE_NONE;
    drv_ev_state[fd].flags = 0;
    /* driver_select() depends on inport/outport being < 0
       when not selected... */
    drv_ev_state[fd].driver.select.inport = -1;
    drv_ev_state[fd].driver.select.outport = -1;
    check_ignore(fd, (ErtsPollEvents) 0, old_events);
}

int
ERTS_CIO_EXPORT(driver_select)(ErlDrvPort ix,
			       ErlDrvEvent e,
			       int mode,
			       int on)
{
    int fd = (int) e;
    ErtsPollEvents ctl_events = (ErtsPollEvents) 0;
    ErtsPollEvents new_events, old_events;

    if (fd < 0)
	return -1;

    if (fd >= max_fds) {
	select_large_fd_error(ix, fd, mode, on);
	return -1;
    }

    erts_smp_mtx_lock(&drv_ev_state_mtx);

    if (fd >= drv_ev_state_len)
	grow_drv_ev_state(fd);

#if ERTS_CIO_HAVE_DRV_EVENT
    if (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_EV)
	deselect(fd);
#endif

    if (mode & DO_READ)
	ctl_events |= ERTS_POLL_EV_IN;
    if (mode & DO_WRITE)
	ctl_events |= ERTS_POLL_EV_OUT;

    ASSERT(drv_ev_state[fd].events
	   ? (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_SEL)
	   : (drv_ev_state[fd].type == ERTS_EV_TYPE_NONE));

    new_events = ERTS_CIO_POLL_CTL(pollset, fd, ctl_events, on);
    if (new_events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL)) {
	erts_smp_mtx_unlock(&drv_ev_state_mtx);
	return -1;
    }

    old_events = drv_ev_state[fd].events;

    ASSERT(on
	   ? (new_events == (drv_ev_state[fd].events | ctl_events))
	   : (new_events == (drv_ev_state[fd].events & ~ctl_events)));

    drv_ev_state[fd].events = new_events;
    if (on) {
	drv_ev_state[fd].type = ERTS_EV_TYPE_DRV_SEL;
	if (ctl_events & ERTS_POLL_EV_IN)
	    drv_ev_state[fd].driver.select.inport = (int) ix;
	if (ctl_events & ERTS_POLL_EV_OUT)
	    drv_ev_state[fd].driver.select.outport = (int) ix;
    }
    else {
	if (ctl_events & ERTS_POLL_EV_IN)
	    drv_ev_state[fd].driver.select.inport = -1;
	if (ctl_events & ERTS_POLL_EV_OUT)
	    drv_ev_state[fd].driver.select.outport = -1;
	if (new_events == 0) {
	    drv_ev_state[fd].flags = 0;
	    drv_ev_state[fd].type = ERTS_EV_TYPE_NONE;
	}
    }

    check_ignore(fd, new_events, old_events);
    erts_smp_mtx_unlock(&drv_ev_state_mtx);

    return 0;
}

int
ERTS_CIO_EXPORT(driver_event)(ErlDrvPort ix,
			      ErlDrvEvent e,
			      ErlDrvEventData event_data)
{
#if !ERTS_CIO_HAVE_DRV_EVENT
    return -1;
#else
    int fd = (int) e;
    ErtsPollEvents events;
    ErtsPollEvents add_events;
    ErtsPollEvents remove_events;
 
   if (fd < 0)
	return -1;

    if (fd >= max_fds) {
	event_large_fd_error(ix, fd, event_data);
	return -1;
    }

    erts_smp_mtx_lock(&drv_ev_state_mtx);

    if (fd >= drv_ev_state_len)
	grow_drv_ev_state(fd);

    if (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_SEL)
	deselect(fd);

    events = drv_ev_state[fd].events;

    if (!event_data) {
	remove_events = events;
	add_events = 0;
    }
    else {
	remove_events = ~event_data->events & events;
	add_events = ~events & event_data->events;

    }

    if (add_events) {
	events = ERTS_CIO_POLL_CTL(pollset, fd, add_events, 1);
	if (events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL))
	    goto error;
    }
    if (remove_events) {
	events = ERTS_CIO_POLL_CTL(pollset, fd, remove_events, 0);
	if (events & (ERTS_POLL_EV_ERR|ERTS_POLL_EV_NVAL))
	    goto error;
    }
    if (event_data) {
	if (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_EV) {
	    drv_ev_state[fd].driver.event->removed_events &= ~add_events;
	    drv_ev_state[fd].driver.event->removed_events |= remove_events;
	}
	else {
	    drv_ev_state[fd].driver.event = drv_ev_data_alloc();
	    drv_ev_state[fd].driver.event->port = ix;
	    drv_ev_state[fd].driver.event->removed_events = (ErtsPollEvents) 0;
	    drv_ev_state[fd].type = ERTS_EV_TYPE_DRV_EV;
	}
	drv_ev_state[fd].driver.event->data = event_data;
    }
    else {
	if (drv_ev_state[fd].type == ERTS_EV_TYPE_DRV_EV)
	    drv_ev_data_free(drv_ev_state[fd].driver.event);
	/* driver_select() depends on inport/outport being < 0
	   when not selected... */
	drv_ev_state[fd].driver.select.inport = -1;
	drv_ev_state[fd].driver.select.outport = -1;
	drv_ev_state[fd].flags = 0;
	drv_ev_state[fd].type = ERTS_EV_TYPE_NONE;
    }
    check_ignore(fd, events, drv_ev_state[fd].events);
    drv_ev_state[fd].events = events;
    ASSERT(event_data ? events == event_data->events : events == 0); 
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    return 0;
 error:
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    return -1;
#endif
}


static void
large_fd_error_common(erts_dsprintf_buf_t *dsbufp)
{
    erts_dsprintf(dsbufp,
		  "fd=%d is larger than the largest allowed fd=%d\n",
		  max_fds - 1);
}

static void
select_error_common(erts_dsprintf_buf_t *dsbufp,
		    ErlDrvPort ix, int fd, int mode, int on)
{
    erts_dsprintf(dsbufp,
		  "driver_select(%p, %d, %s%s%s, %d) "
		  "by driver %s port=%T failed: ",
		  ix,
		  fd,
		  mode & DO_READ ? "DO_READ" : "",
		  (mode & (DO_READ|DO_WRITE)) == (DO_READ|DO_WRITE) ? "|" : "",
		  mode & DO_WRITE ? "DO_WRITE" : "",
		  on,
		  erts_port[(int) ix].drv_ptr->driver_name,
		  erts_port[(int) ix].id);
}

static void
select_large_fd_error(ErlDrvPort ix, int fd, int mode, int on)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
    select_error_common(dsbufp, ix, fd, mode, on);
    large_fd_error_common(dsbufp);
    erts_send_error_to_logger_nogl(dsbufp);
}

#if ERTS_CIO_HAVE_DRV_EVENT

static void
event_error_common(erts_dsprintf_buf_t *dsbufp,
		   ErlDrvPort ix, int fd, ErlDrvEventData event_data)
{
    erts_dsprintf(dsbufp, "driver_event(%p, %d, ", ix, fd);
    if (!event_data)
	erts_dsprintf(dsbufp, "NULL");
    else
	erts_dsprintf(dsbufp, "{0x%x, 0x%x}",
		      (unsigned int) event_data->events,
		      (unsigned int) event_data->revents);
    erts_dsprintf(dsbufp,
		  ") by driver %s port=%T failed: ",
		  erts_port[(int) ix].drv_ptr->driver_name,
		  erts_port[(int) ix].id);
}

static void
event_large_fd_error(ErlDrvPort ix, int fd, ErlDrvEventData event_data)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
    event_error_common(dsbufp, ix, fd, event_data);
    large_fd_error_common(dsbufp);
    erts_send_error_to_logger_nogl(dsbufp);
}

#endif

#ifdef ERTS_SMP
static ERTS_INLINE void
bump_timers(void)
{
#ifndef ERTS_TIMER_THREAD
    long dt = do_time_read_and_reset();
    if (dt) {
	erts_smp_io_unlock();
	bump_timer(dt);
	erts_smp_io_lock();
    }
#endif
}
#endif

static ERTS_INLINE void
iready(int ix, int fd)
{
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    input_ready(ix, fd);
    erts_smp_mtx_lock(&drv_ev_state_mtx);
}

static ERTS_INLINE void
oready(int ix, int fd)
{
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    output_ready(ix, fd);
    erts_smp_mtx_lock(&drv_ev_state_mtx);
}

#if ERTS_CIO_HAVE_DRV_EVENT

static ERTS_INLINE void
eready(int ix, int fd, ErlDrvEventData event_data)
{
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    event_ready(ix, fd, event_data);
    erts_smp_mtx_lock(&drv_ev_state_mtx);
}

#endif

static void bad_fd_in_pollset(int, int, int, short);

void
ERTS_CIO_EXPORT(erts_check_io)(int do_wait)
{
    ErtsPollResFd pollres[256];
    int pollres_len;
    SysTimeval wait_time;
    int poll_ret, i;

 restart:

#ifdef ERTS_SMP
    ERTS_CIO_POLL_INTR(pollset, 0);
#endif

    /* Figure out timeout value */
    if (do_wait) {
	erts_time_remaining(&wait_time);
    } else {			/* poll only */
	wait_time.tv_sec = 0;
	wait_time.tv_usec = 0;
    }


    erts_smp_io_unlock();

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif
    erts_smp_activity_change(ERTS_ACTIVITY_IO,
			     ERTS_ACTIVITY_WAIT,
			     NULL,
			     NULL,
			     NULL);

    pollres_len = sizeof(pollres)/sizeof(ErtsPollResFd);
    erts_smp_atomic_set(&in_poll_wait, 1);
    poll_ret = ERTS_CIO_POLL_WAIT(pollset, pollres, &pollres_len, &wait_time);

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_check_exact(NULL, 0); /* No locks should be locked */
#endif
    erts_smp_activity_change(ERTS_ACTIVITY_WAIT,
			     ERTS_ACTIVITY_IO,
			     NULL,
			     NULL,
			     NULL);
    erts_smp_io_lock();

    erts_deliver_time(); /* sync the machine's idea of time */

#ifdef ERTS_SMP
    bump_timers();
#endif

    if (erts_break_requested)
	erts_do_break_handling();

    if (poll_ret != 0) {
	erts_smp_atomic_set(&in_poll_wait, 0);

	if (poll_ret == EAGAIN)
	    goto restart;

	if (poll_ret != ETIMEDOUT
	    && poll_ret != EINTR
	    && poll_ret != ERRNO_BLOCK) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "erts_poll_wait() failed: %s (%d)\n",
			  erl_errno_id(poll_ret), poll_ret);
	    erts_send_error_to_logger_nogl(dsbufp);
	}

	return;
    }

    erts_smp_mtx_lock(&drv_ev_state_mtx);
    erts_smp_atomic_set(&in_poll_wait, 0);

    for (i = 0; i < pollres_len; i++) {
	int fd = pollres[i].fd;

	if (drv_ev_state[fd].flags & ERTS_EV_FLG_IGNORE)
	    continue;

	switch (drv_ev_state[fd].type) {
	case ERTS_EV_TYPE_DRV_SEL: { /* Requested via driver_select()... */
	    ErtsPollEvents revents;
	    ErtsPollEvents revent_mask;

	    revent_mask = ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT);
	    revent_mask |= drv_ev_state[fd].events;
	    revents = pollres[i].events & revent_mask;

	    if (revents & ERTS_POLL_EV_ERR) {
		/*
		 * Let the driver handle the error condition. Only input,
		 * only output, or nothing might have been selected.
		 * We *do not* want to call a callback that corresponds
		 * to an event not selected. revents might give us a clue
		 * on which one to call.
		 */ 
		if ((revents & ERTS_POLL_EV_IN)
		    || (!(revents & ERTS_POLL_EV_OUT)
			&& drv_ev_state[fd].events & ERTS_POLL_EV_IN))
		    iready(drv_ev_state[fd].driver.select.inport, fd);
		else if (drv_ev_state[fd].events & ERTS_POLL_EV_OUT)
		    oready(drv_ev_state[fd].driver.select.outport, fd);
	    }
	    else if (revents & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
		if (revents & ERTS_POLL_EV_OUT)
		    oready(drv_ev_state[fd].driver.select.outport, fd);
		/* Someone might have deselected input since revents
		   was read (true also on the non-smp emulator since
		   oready() may have been called); therefore, update
		   revents... */
		revents &= ~(~drv_ev_state[fd].events & ERTS_POLL_EV_IN);
		if (revents & ERTS_POLL_EV_IN)
		    iready(drv_ev_state[fd].driver.select.inport, fd);
	    }
	    else if (revents & ERTS_POLL_EV_NVAL) {
		bad_fd_in_pollset(fd,
				  drv_ev_state[fd].driver.select.inport,
				  drv_ev_state[fd].driver.select.outport,
				  drv_ev_state[fd].events);
	    }
	    break;
	}

#if ERTS_CIO_HAVE_DRV_EVENT
	case ERTS_EV_TYPE_DRV_EV: { /* Requested via driver_event()... */
	    ErlDrvEventData event_data = drv_ev_state[fd].driver.event->data;
	    ErtsPollEvents revents;
	    ASSERT(drv_ev_state[fd].driver.event);
	    ASSERT(drv_ev_state[fd].driver.event->data);
	    revents = pollres[i].events;
	    revents &= ~drv_ev_state[fd].driver.event->removed_events;

	    if (revents) {
		event_data->events = drv_ev_state[fd].events;
		event_data->revents = revents;

		eready(drv_ev_state[fd].driver.event->port, fd, event_data);
	    }
	    break;
	}
#endif

	case ERTS_EV_TYPE_NONE: /* Deselected ... */
	    break;

	default: { /* Error */
	    erts_dsprintf_buf_t *dsbufp;
	    dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp,
			  "Invalid event request type for fd in erts_poll()! "
			  "fd=%d, event request type=%sd\n", fd,
			  (int) drv_ev_state[fd].type);
	    ASSERT(0);
	    deselect(fd);
	    break;
	}
	}

    }

    reset_ignores();

    erts_smp_mtx_unlock(&drv_ev_state_mtx);
}

static void
bad_fd_in_pollset(int fd, int inport, int outport, short events)
{
    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();

    if (events & (ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) {
	char *io_str;
	int port;
	int both = 0;
	if ((events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT)) {
	    io_str = "input/output";
	    port = -1;
	    both = 1;
	}
	if (events & ERTS_POLL_EV_IN) {
	    io_str = "input";
	    port = inport;
	}
	else {
	    io_str = "output";
	    port = outport;
	}
	erts_dsprintf(dsbufp,
		      "Bad %s fd in erts_poll()! fd=%d, ",
		      io_str, fd);
	if (port < 0)
	    erts_dsprintf(dsbufp, "ports=%T/%T",
			  inport >= 0 ? erts_port[inport].id : am_undefined,
			  outport >= 0 ? erts_port[outport].id : am_undefined);
	else
	    erts_dsprintf(dsbufp, "port=%T",
			  port >= 0 ? erts_port[port].id : am_undefined);
	erts_dsprintf(dsbufp, ", driver=%s, name=%s\n",
		      erts_port[port].drv_ptr->driver_name,
		      erts_port[port].name);
    }
    else {
	erts_dsprintf(dsbufp, "Bad fd in erts_poll()! fd=%d\n", fd);
    }
    erts_send_error_to_logger_nogl(dsbufp);

    /* unmap entry */
    deselect(fd);
}

void
ERTS_CIO_EXPORT(erts_init_check_io)(void)
{
    init_fd_list_alloc();
    ignored_list = NULL;
    erts_smp_atomic_init(&in_poll_wait, 0);
    ERTS_CIO_POLL_INIT();
    max_fds = ERTS_CIO_POLL_MAX_FDS();
    pollset = ERTS_CIO_NEW_POLLSET();
    erts_smp_mtx_init(&drv_ev_state_mtx, "drv_ev_state");
    drv_ev_state_len = 0;
    drv_ev_state = NULL;
#if ERTS_CIO_HAVE_DRV_EVENT
    init_drv_ev_data_alloc();
#endif
}

int
ERTS_CIO_EXPORT(erts_check_io_max_files)(void)
{
    return max_fds;
}

Uint
ERTS_CIO_EXPORT(erts_check_io_size)(void)
{
    Uint res;
    ErtsPollInfo pi;
    erts_smp_mtx_lock(&drv_ev_state_mtx);
    ERTS_CIO_POLL_INFO(pollset, &pi);
    res = pi.memory_size;
    res += sizeof(ErtsDrvEventState)*drv_ev_state_len;
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    return res;
}

Eterm
ERTS_CIO_EXPORT(erts_check_io_info)(void *proc)
{
    Process *p = (Process *) proc;
    Eterm tags[12], values[12], res;
    Uint sz, *szp, *hp, **hpp, memory_size;
    Sint i;
    ErtsPollInfo pi;

    erts_smp_mtx_lock(&drv_ev_state_mtx);
    ERTS_CIO_POLL_INFO(pollset, &pi);
    memory_size = pi.memory_size;
    memory_size += sizeof(ErtsDrvEventState)*drv_ev_state_len;
    erts_smp_mtx_unlock(&drv_ev_state_mtx);

    hpp = NULL;
    szp = &sz;
    sz = 0;

 bld_it:
    i = 0;

    tags[i] = erts_bld_atom(hpp, szp, "name");
    values[i++] = erts_bld_atom(hpp, szp, "erts_poll");

    tags[i] = erts_bld_atom(hpp, szp, "primary");
    values[i++] = erts_bld_atom(hpp, szp, pi.primary);

    tags[i] = erts_bld_atom(hpp, szp, "fallback");
    values[i++] = erts_bld_atom(hpp, szp, pi.fallback ? pi.fallback : "false");

    tags[i] = erts_bld_atom(hpp, szp, "kernel_poll");
    values[i++] = erts_bld_atom(hpp, szp,
				pi.kernel_poll ? pi.kernel_poll : "false");

    tags[i] = erts_bld_atom(hpp, szp, "memory_size");
    values[i++] = erts_bld_uint(hpp, szp, memory_size);

    tags[i] = erts_bld_atom(hpp, szp, "total_poll_set_size");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.poll_set_size);

    if (pi.fallback) {
	tags[i] = erts_bld_atom(hpp, szp, "fallback_poll_set_size");
	values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.fallback_poll_set_size);
    }

    tags[i] = erts_bld_atom(hpp, szp, "lazy_updates");
    values[i++] = pi.lazy_updates ? am_true : am_false;

    if (pi.lazy_updates) {
	tags[i] = erts_bld_atom(hpp, szp, "pending_updates");
	values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.pending_updates);
    }

    tags[i] = erts_bld_atom(hpp, szp, "batch_updates");
    values[i++] = pi.batch_updates ? am_true : am_false;

    tags[i] = erts_bld_atom(hpp, szp, "concurrent_updates");
    values[i++] = pi.concurrent_updates ? am_true : am_false;

    tags[i] = erts_bld_atom(hpp, szp, "max_fds");
    values[i++] = erts_bld_uint(hpp, szp, (Uint) pi.max_fds);

    res = erts_bld_2tup_list(hpp, szp, i, tags, values);

    if (!hpp) {
	hp = HAlloc(p, sz);
	hpp = &hp;
	szp = NULL;
	goto bld_it;
    }

    return res;
}

static ERTS_INLINE ErtsPollEvents
print_events(ErtsPollEvents ev)
{
    int first = 1;
    if(ev & ERTS_POLL_EV_IN) {
	ev &= ~ERTS_POLL_EV_IN;
	erts_printf("%s%s", first ? "" : "|", "IN");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_OUT) {
	ev &= ~ERTS_POLL_EV_OUT;
	erts_printf("%s%s", first ? "" : "|", "OUT");
	first = 0;
    }
    /* The following should not appear... */
    if(ev & ERTS_POLL_EV_NVAL) {
	erts_printf("%s%s", first ? "" : "|", "NVAL");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_ERR) {
	erts_printf("%s%s", first ? "" : "|", "ERR");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_1SHOT) {
	erts_printf("%s%s", first ? "" : "|", "1SHOT");
	first = 0;
    }
    if(ev & ERTS_POLL_EV_FULL_1SHOT) {
	erts_printf("%s%s", first ? "" : "|", "FULL_1SHOT");
	first = 0;
    }
    if (ev)
	erts_printf("%s0x%b32x", first ? "" : "|", (Uint32) ev);
    return ev;
}

int
ERTS_CIO_EXPORT(erts_check_io_debug)(void)
{
    int res = 0;
    int fd;
    int used_fds = 0;
    int internal_fds = 0;
    ErtsPollEvents *epep;
    ErtsDrvEventState null_des;

    null_des.driver.select.inport = -1;
    null_des.driver.select.outport = -1;
    null_des.events = 0;
    null_des.flags = 0;
    null_des.type = ERTS_EV_TYPE_NONE;

    erts_smp_mtx_lock(&drv_ev_state_mtx);

    erts_printf("--- fds in pollset --------------------------------------\n");

    epep = erts_alloc(ERTS_ALC_T_TMP, sizeof(ErtsPollEvents)*max_fds);

    ERTS_POLL_EXPORT(erts_poll_get_selected_events)(pollset, epep, max_fds);

    for (fd = 0; fd < max_fds; fd++) {
	int err = 0;
	ErtsDrvEventState *desp = ((fd < drv_ev_state_len)
				   ? &drv_ev_state[fd]
				   : &null_des);
	ErtsPollEvents cio_events = desp->events;
	ErtsPollEvents ep_events = epep[fd];
	int internal = 0;
#ifdef HAVE_FSTAT
	struct stat stat_buf;
#endif

	if (desp->events || ep_events) {
	    if (ep_events & ERTS_POLL_EV_NVAL) {
		ep_events &= ~ERTS_POLL_EV_NVAL;
		internal = 1;
		internal_fds++;
	    }
	    else
		used_fds++;

	    erts_printf("fd=%d ", fd);

#ifdef HAVE_FSTAT
	    if (fstat(fd, &stat_buf) < 0)
		erts_printf("type=unknown ");
	    else {
		erts_printf("type=");
#ifdef S_ISSOCK
		if (S_ISSOCK(stat_buf.st_mode))
		    erts_printf("sock ");
		else
#endif
#ifdef S_ISFIFO
		if (S_ISFIFO(stat_buf.st_mode))
		    erts_printf("fifo ");
		else
#endif
#ifdef S_ISCHR
		if (S_ISCHR(stat_buf.st_mode))
		    erts_printf("chr ");
		else
#endif
#ifdef S_ISDIR
		if (S_ISDIR(stat_buf.st_mode))
		    erts_printf("dir ");
		else
#endif
#ifdef S_ISBLK
		if (S_ISBLK(stat_buf.st_mode))
		    erts_printf("blk ");
		else
#endif
#ifdef S_ISREG
		if (S_ISREG(stat_buf.st_mode))
		    erts_printf("reg ");
		else
#endif
#ifdef S_ISLNK
		if (S_ISLNK(stat_buf.st_mode))
		    erts_printf("lnk ");
		else
#endif
#ifdef S_ISDOOR
		if (S_ISDOOR(stat_buf.st_mode))
		    erts_printf("door ");
		else
#endif
#ifdef S_ISWHT
		if (S_ISWHT(stat_buf.st_mode))
		    erts_printf("wht ");
		else
#endif
#ifdef S_ISXATTR
		if (S_ISXATTR(stat_buf.st_mode))
		    erts_printf("xattr ");
		else
#endif
		    erts_printf("unknown ");
	    }
#endif

	    if (desp->type == ERTS_EV_TYPE_DRV_SEL) {
		erts_printf("driver_select ");
		
		if (internal) {
		    erts_printf("internal ");
		    err = 1;		    
		}

		if (cio_events == ep_events) {
		    erts_printf("ev=");
		    if (print_events(cio_events) != 0)
			err = 1;
		}
		else {
		    err = 1;
		    erts_printf("cio_ev=");
		    print_events(cio_events);
		    erts_printf(" ep_ev=");
		    print_events(ep_events);
		}
		erts_printf(" ");
		if (cio_events & ERTS_POLL_EV_IN) {
		    int ix = desp->driver.select.inport;
		    if (ix < 0) {
			erts_printf("inport=none indrv=none ");
			err = 1;
		    }
		    else {
			erts_printf("inport=%T indrv=%s ",
				   erts_port[ix].id,
				   erts_port[ix].drv_ptr->driver_name);
		    }
		}
		if (cio_events & ERTS_POLL_EV_OUT) {
		    int ix = desp->driver.select.outport;
		    if (ix < 0) {
			erts_printf("outport=none outdrv=none ");
			err = 1;
		    }
		    else {
			erts_printf("outport=%T outdrv=%s ",
				   erts_port[ix].id,
				   erts_port[ix].drv_ptr->driver_name);
		    }
		}
	    }
	    else if (desp->type == ERTS_EV_TYPE_DRV_EV) {
		int ix;
		erts_printf("driver_event ");
		if (internal) {
		    erts_printf("internal ");
		    err = 1;		    
		}
		if (cio_events == ep_events) {
		    erts_printf("ev=0x%b32x", (Uint32) cio_events);
		}
		else {
		    err = 1;
		    erts_printf("cio_ev=0x%b32x", (Uint32) cio_events);
		    erts_printf(" ep_ev=0x%b32x", (Uint32) ep_events);
		}
		ix = desp->driver.event->port;
		if (ix < 0) {
		    erts_printf(" port=none drv=none ");
		    err = 1;
		}
		else {
		    erts_printf(" port=%T drv=%s ",
			       erts_port[ix].id,
			       erts_port[ix].drv_ptr->driver_name);
		}
	    }
	    else if (internal) {
		erts_printf("internal ");
		if (cio_events) {
		    err = 1;
		    erts_printf("cio_ev=");
		    print_events(cio_events);
		}
		if (ep_events) {
		    erts_printf("ep_ev=");
		    print_events(ep_events);
		}
	    }
	    else {
		err = 1;
		erts_printf("control_type=%d ", desp->type);
		if (cio_events == ep_events) {
		    erts_printf("ev=0x%b32x", (Uint32) cio_events);
		}
		else {
		    erts_printf("cio_ev=0x%b32x", (Uint32) cio_events);
		    erts_printf(" ep_ev=0x%b32x", (Uint32) ep_events);
		}
	    }

	    if (err) {
		res++;
		erts_printf(" ERROR");
	    }
	    erts_printf("\n");
	}
    }
    erts_printf("\n");
    erts_printf("used fds=%d\n", used_fds);
    erts_printf("internal fds=%d\n", internal_fds);
    erts_printf("---------------------------------------------------------\n");
    fflush(stdout);
    erts_free(ERTS_ALC_T_TMP, (void *) epep);
    erts_smp_mtx_unlock(&drv_ev_state_mtx);
    return res;
}


#ifdef ERTS_SMP

void
ERTS_CIO_EXPORT(erts_wake_io_thread)(void)
{
    ERTS_CIO_POLL_INTR(pollset, 1);
}

#endif /* #ifdef ERTS_SMP */
