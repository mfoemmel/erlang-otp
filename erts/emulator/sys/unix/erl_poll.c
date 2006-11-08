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
 *
 */

/*
 * Description:	Poll interface suitable for ERTS with or without
 *              SMP support.
 *
 *		The interface is currently implemented using:
 *		- select
 *		- poll
 *              - /dev/poll
 *              - epoll with poll or select as fallback
 *              - kqueue with poll or select as fallback
 *
 *		Some time in the future it will also be
 *		implemented using Solaris ports.
 *
 *
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


#define WANT_NONBLOCKING

#include "erl_poll.h"
#if ERTS_POLL_USE_KQUEUE
#  include <sys/types.h>
#  include <sys/event.h>
#  include <sys/time.h>
#endif
#if ERTS_POLL_USE_SELECT
#  ifdef SYS_SELECT_H
#    include <sys/select.h>
#  endif
#endif
#ifdef NO_SYSCONF
#  if ERTS_POLL_USE_SELECT
#    include <sys/param.h>
#  else
#    include <limits.h>
#  endif
#endif
#include "erl_driver.h"
#include "erl_alloc.h"

#if !defined(ERTS_POLL_USE_EPOLL) \
    && !defined(ERTS_POLL_USE_DEVPOLL)  \
    && !defined(ERTS_POLL_USE_POLL) \
    && !defined(ERTS_POLL_USE_SELECT)
#error "Missing implementation of erts_poll()"
#endif

#if defined(ERTS_KERNEL_POLL_VERSION) && !ERTS_POLL_USE_KERNEL_POLL
#error "Missing kernel poll implementation of erts_poll()"
#endif

#if defined(ERTS_NO_KERNEL_POLL_VERSION) && ERTS_POLL_USE_KERNEL_POLL
#error "Kernel poll used when it shouldn't be used"
#endif

#if 0
#define ERTS_POLL_DEBUG_PRINT
#endif

#if defined(DEBUG) && 0
#define HARD_DEBUG
#endif

#define ERTS_POLL_USE_BATCH_UPDATE_POLLSET (ERTS_POLL_USE_DEVPOLL \
					    || ERTS_POLL_USE_KQUEUE)
#define ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE \
   (defined(ERTS_SMP) || ERTS_POLL_USE_KERNEL_POLL || ERTS_POLL_USE_POLL)

#define ERTS_POLL_USE_CONCURRENT_UPDATE \
   (defined(ERTS_SMP) && ERTS_POLL_USE_EPOLL)

#define FDS_STATUS_EXTRA_FREE_SIZE 128
#define POLL_FDS_EXTRA_FREE_SIZE 128

#ifdef ERTS_SMP

#define ERTS_POLLSET_LOCK(PS) \
  erts_smp_mtx_lock(&(PS)->mtx)
#define ERTS_POLLSET_UNLOCK(PS) \
  erts_smp_mtx_unlock(&(PS)->mtx)
#define ERTS_POLLSET_SET_POLLED_CHK(PS) \
  ((int) erts_smp_atomic_xchg(&(PS)->polled, (long) 1))
#define ERTS_POLLSET_SET_POLLED(PS) \
  erts_smp_atomic_set(&(PS)->polled, (long) 1)
#define ERTS_POLLSET_UNSET_POLLED(PS) \
  erts_smp_atomic_set(&(PS)->polled, (long) 0)
#define ERTS_POLLSET_IS_POLLED(PS) \
  ((int) erts_smp_atomic_read(&(PS)->polled))
#define ERTS_POLLSET_SET_POLLER_WOKEN_CHK(PS) \
  ((int) erts_smp_atomic_xchg(&(PS)->woken, (long) 1))
#define ERTS_POLLSET_SET_POLLER_WOKEN(PS) \
  erts_smp_atomic_set(&(PS)->woken, (long) 1)
#define ERTS_POLLSET_UNSET_POLLER_WOKEN(PS) \
  erts_smp_atomic_set(&(PS)->woken, (long) 0)
#define ERTS_POLLSET_IS_POLLER_WOKEN(PS) \
  ((int) erts_smp_atomic_read(&(PS)->woken))

#else

#define ERTS_POLLSET_LOCK(PS)
#define ERTS_POLLSET_UNLOCK(PS)
#define ERTS_POLLSET_SET_POLLED_CHK(PS) 0
#define ERTS_POLLSET_UNSET_POLLED(PS)
#define ERTS_POLLSET_IS_POLLED(PS) 0
#define ERTS_POLLSET_SET_POLLER_WOKEN_CHK(PS) 1
#define ERTS_POLLSET_SET_POLLER_WOKEN(PS)
#define ERTS_POLLSET_UNSET_POLLER_WOKEN(PS)
#define ERTS_POLLSET_IS_POLLER_WOKEN(PS) 1

#endif

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS) \
  erts_smp_atomic_set(&(PS)->have_update_requests, (long) 1)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS) \
  erts_smp_atomic_set(&(PS)->have_update_requests, (long) 0)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) \
  ((int) erts_smp_atomic_read(&(PS)->have_update_requests))
#else
#define ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(PS)
#define ERTS_POLLSET_HAVE_UPDATE_REQUESTS(PS) 0
#endif

#define ERTS_POLLSET_IS_INTERRUPTED(PS) \
  ((int) erts_smp_atomic_read(&(PS)->interrupt))
#define ERTS_POLLSET_UNSET_INTERRUPTED_CHK(PS) \
  ((int) erts_smp_atomic_xchg(&(PS)->interrupt, (long) 0))
#define ERTS_POLLSET_UNSET_INTERRUPTED(PS) \
  erts_smp_atomic_set(&(PS)->interrupt, (long) 0)
#define ERTS_POLLSET_SET_INTERRUPTED(PS) \
  erts_smp_atomic_xchg(&(PS)->interrupt, (long) 1)

#if ERTS_POLL_USE_FALLBACK
#  if ERTS_POLL_USE_POLL
#    define ERTS_POLL_NEED_FALLBACK(PS) ((PS)->no_poll_fds > 1)
#  elif ERTS_POLL_USE_SELECT
#    define ERTS_POLL_NEED_FALLBACK(PS) ((PS)->no_select_fds > 1)
#  endif
#endif
/*
 * --- Data types ------------------------------------------------------------
 */

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#define ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE 128

typedef struct ErtsPollSetUpdateRequestsBlock_ ErtsPollSetUpdateRequestsBlock;
struct ErtsPollSetUpdateRequestsBlock_ {
    ErtsPollSetUpdateRequestsBlock *next;
    int len;
    int fds[ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE];
};

#endif


#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
#  define ERTS_POLL_FD_FLG_INURQ	(((unsigned short) 1) << 0)
#endif
#if ERTS_POLL_USE_FALLBACK
#  define ERTS_POLL_FD_FLG_INFLBCK	(((unsigned short) 1) << 1)
#  define ERTS_POLL_FD_FLG_USEFLBCK	(((unsigned short) 1) << 2)
#endif
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
#  define ERTS_POLL_FD_FLG_RST		(((unsigned short) 1) << 3)
#endif
typedef struct {
#if ERTS_POLL_USE_POLL
    int pix;
#endif
    ErtsPollEvents used_events;
    ErtsPollEvents events;
#if ERTS_POLL_USE_KQUEUE
    unsigned short res_ev_ix;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE || ERTS_POLL_USE_FALLBACK
    unsigned short flags;
#endif

} ErtsFdStatus;

#if ERTS_POLL_USE_KQUEUE
/* res_ev_ix max value */
#define ERTS_POLL_MAX_RES ((1 << sizeof(unsigned short)*8) - 1)

#define ERTS_POLL_KQ_OP_HANDLED			1
#define ERTS_POLL_KQ_OP_DEL_R			2
#define ERTS_POLL_KQ_OP_DEL_W			3
#define ERTS_POLL_KQ_OP_ADD_R			4
#define ERTS_POLL_KQ_OP_ADD_W			5
#define ERTS_POLL_KQ_OP_ADD2_R			6
#define ERTS_POLL_KQ_OP_ADD2_W			7

#endif

struct ErtsPollSet_ {
    ErtsPollSet next;
    int internal_fd_limit;
    ErtsFdStatus *fds_status;
    int no_of_user_fds;
    int fds_status_len;
#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
    int res_events_len;
#if ERTS_POLL_USE_EPOLL
    struct epoll_event *res_events;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *res_events;
#elif ERTS_POLL_USE_DEVPOLL
    struct pollfd *res_events;
#endif
#endif /* ERTS_POLL_USE_KERNEL_POLL */
#if ERTS_POLL_USE_POLL
    int next_poll_fds_ix;
    int no_poll_fds;
    int poll_fds_len;
    struct pollfd*poll_fds;
#elif ERTS_POLL_USE_SELECT
    int next_sel_fd;
    int max_fd;
#if ERTS_POLL_USE_FALLBACK
    int no_select_fds;
#endif
    fd_set input_fds;
    fd_set res_input_fds;
    fd_set output_fds;
    fd_set res_output_fds;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    ErtsPollSetUpdateRequestsBlock update_requests;
    ErtsPollSetUpdateRequestsBlock *curr_upd_req_block;
    erts_smp_atomic_t have_update_requests;
#endif
#ifdef ERTS_SMP
    erts_smp_atomic_t polled;
    erts_smp_atomic_t woken;
    erts_smp_mtx_t mtx;
    int wake_fds[2];
#endif
#if ERTS_POLL_USE_FALLBACK
    int fallback_used;
#endif
    erts_smp_atomic_t interrupt;
};

static void fatal_error(char *format, ...);

static int max_fds = -1;
static ErtsPollSet pollsets;
static erts_smp_spinlock_t pollsets_lock;

#if ERTS_POLL_USE_POLL

static ERTS_INLINE short
ev2pollev(ErtsPollEvents ev)
{
#if !ERTS_POLL_USE_FALLBACK || ERTS_POLL_USE_KQUEUE
    return ERTS_POLL_EV_E2N(ev);
#else /* Note, we only map events we are interested in */
    short res_ev = (short) 0;
    if (ev & ERTS_POLL_EV_IN)
	res_ev |= ERTS_POLL_EV_NKP_IN;
    if (ev & ERTS_POLL_EV_OUT)
	res_ev |= ERTS_POLL_EV_NKP_OUT;
    return res_ev;
#endif
}

static ERTS_INLINE ErtsPollEvents
pollev2ev(short ev)
{
#if !ERTS_POLL_USE_FALLBACK || ERTS_POLL_USE_KQUEUE
    return ERTS_POLL_EV_N2E(ev);
#else /* Note, we only map events we are interested in */
    ErtsPollEvents res_ev = (ErtsPollEvents) 0;
    if (ev & ERTS_POLL_EV_NKP_IN)
	res_ev |= ERTS_POLL_EV_IN;
    if (ev & ERTS_POLL_EV_NKP_OUT)
	res_ev |= ERTS_POLL_EV_OUT;
    if (ev & ERTS_POLL_EV_NKP_ERR)
	res_ev |= ERTS_POLL_EV_ERR;
    if (ev & ERTS_POLL_EV_NKP_NVAL)
	res_ev |= ERTS_POLL_EV_NVAL;
    return res_ev;
#endif
}

#endif

#if ERTS_POLL_USE_DEVPOLL && defined(HARD_DEBUG)
static void check_poll_status(ErtsPollSet ps);
#endif
#ifdef ERTS_POLL_DEBUG_PRINT
static void print_misc_debug_info(void);
#endif

/*
 * --- Wakeup pipe -----------------------------------------------------------
 */

#ifdef ERTS_SMP

static ERTS_INLINE void
wake_poller(ErtsPollSet ps)
{
    if (!ERTS_POLLSET_SET_POLLER_WOKEN_CHK(ps)) {
	ssize_t res;
	if (ps->wake_fds[1] < 0)
	    return; /* Not initialized yet */
	do {	
	    res = write(ps->wake_fds[1], "!", 1);
	} while (res < 0 && errno == EINTR);
	if (res <= 0 && errno != ERRNO_BLOCK) {
	    fatal_error("%s:%d:wake_poller(): "
			"Failed to write on wakeup pipe fd=%d: "
			"%s (%d)\n",
			__FILE__, __LINE__,
			ps->wake_fds[1],
			erl_errno_id(errno), errno);
	}
    }
}

static ERTS_INLINE void
cleanup_wakeup_pipe(ErtsPollSet ps)
{
    int fd = ps->wake_fds[0];
    int res;
    do {
	char *buf[32];
	res = read(fd, (void *) buf, 32);
    } while (res > 0 || (res < 0 && errno == EINTR));
    if (res < 0 && errno != ERRNO_BLOCK) {
	fatal_error("%s:%d:cleanup_wakeup_pipe(): "
		    "Failed to read on wakeup pipe fd=%d: "
		    "%s (%d)\n",
		    __FILE__, __LINE__,
		    fd,
		    erl_errno_id(errno), errno);
    }
}

static void
create_wakeup_pipe(ErtsPollSet ps)
{
    int wake_fds[2];
    ps->wake_fds[0] = -1;
    ps->wake_fds[1] = -1;
    if (pipe(wake_fds) < 0) {
	fatal_error("%s:%d:create_wakeup_pipe(): "
		    "Failed to create pipe: %s (%d)\n",
		    __FILE__,
		    __LINE__,
		    erl_errno_id(errno),
		    errno);
    }
    SET_NONBLOCKING(wake_fds[0]);
    SET_NONBLOCKING(wake_fds[1]);

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("wakeup fds = {%d, %d}\n", wake_fds[0], wake_fds[1]);
#endif

    ERTS_POLL_EXPORT(erts_poll_control)(ps,
					wake_fds[0],
					ERTS_POLL_EV_IN,
					1);
#if ERTS_POLL_USE_FALLBACK
    /* We depend on the wakeup pipe being handled by kernel poll */
    if (ps->fds_status[wake_fds[0]].flags & ERTS_POLL_FD_FLG_INFLBCK)
	fatal_error("%s:%d:create_wakeup_pipe(): Internal error\n",
		    __FILE__, __LINE__);
#endif
    if (ps->internal_fd_limit <= wake_fds[1])
	ps->internal_fd_limit = wake_fds[1] + 1;
    if (ps->internal_fd_limit <= wake_fds[0])
	ps->internal_fd_limit = wake_fds[0] + 1;
    ps->wake_fds[0] = wake_fds[0];
    ps->wake_fds[1] = wake_fds[1];
}

#endif /* ERTS_SMP */

/*
 * --- Poll set update requests ----------------------------------------------
 */
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE

static ERTS_INLINE void
enqueue_update_request(ErtsPollSet ps, int fd)
{
    ErtsPollSetUpdateRequestsBlock *urqbp;

    ASSERT(fd < ps->fds_status_len);

    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INURQ)
	return;

    if (ps->update_requests.len == 0)
	ERTS_POLLSET_SET_HAVE_UPDATE_REQUESTS(ps);

    urqbp = ps->curr_upd_req_block;

    if (urqbp->len == ERTS_POLLSET_UPDATE_REQ_BLOCK_SIZE) {
	ASSERT(!urqbp->next);
	urqbp = erts_alloc(ERTS_ALC_T_POLLSET_UPDREQ,
			   sizeof(ErtsPollSetUpdateRequestsBlock));
	ps->curr_upd_req_block->next = urqbp;
	ps->curr_upd_req_block = urqbp;
	urqbp->next = NULL;
	urqbp->len = 0;
    }

    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INURQ;
    urqbp->fds[urqbp->len++] = fd;
}

static ERTS_INLINE void
free_update_requests_block(ErtsPollSet ps,
			   ErtsPollSetUpdateRequestsBlock *urqbp)
{
    if (urqbp != &ps->update_requests)
	erts_free(ERTS_ALC_T_POLLSET_UPDREQ, (void *) urqbp);
    else {
	urqbp->next = NULL;
	urqbp->len = 0;
    }
}

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

/*
 * --- Growing poll set structures -------------------------------------------
 */

#if ERTS_POLL_USE_KERNEL_POLL
static void
grow_res_events(ErtsPollSet ps, int new_len)
{
    size_t new_size = sizeof(
#if ERTS_POLL_USE_EPOLL
	struct epoll_event
#elif ERTS_POLL_USE_DEVPOLL
	struct pollfd
#elif ERTS_POLL_USE_KQUEUE
	struct kevent
#endif
	)*new_len;
    /* We do not need to save previously stored data */
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, ps->res_events);
    ps->res_events = erts_alloc(ERTS_ALC_T_POLL_RES_EVS, new_size);
    ps->res_events_len = new_len;
}
#endif /* ERTS_POLL_USE_KERNEL_POLL */

#if ERTS_POLL_USE_POLL
static void
grow_poll_fds(ErtsPollSet ps, int min_ix)
{
    int i;
    int new_len = min_ix + 1 + POLL_FDS_EXTRA_FREE_SIZE;
    if (new_len > max_fds)
	new_len = max_fds;
    ps->poll_fds = (ps->poll_fds_len
		    ? erts_realloc(ERTS_ALC_T_POLL_FDS,
				   ps->poll_fds,
				   sizeof(struct pollfd)*new_len)
		    : erts_alloc(ERTS_ALC_T_POLL_FDS,
				 sizeof(struct pollfd)*new_len));
    for (i = ps->poll_fds_len; i < new_len; i++) {
	ps->poll_fds[i].fd = -1;
	ps->poll_fds[i].events = (short) 0;
	ps->poll_fds[i].revents = (short) 0;
    }
    ps->poll_fds_len = new_len;
}
#endif

static void
grow_fds_status(ErtsPollSet ps, int min_fd)
{
    int i;
    int new_len = min_fd + 1 + FDS_STATUS_EXTRA_FREE_SIZE;
    ASSERT(min_fd < max_fds);
    if (new_len > max_fds)
	new_len = max_fds;
    ps->fds_status = (ps->fds_status_len
		      ? erts_realloc(ERTS_ALC_T_FD_STATUS,
				     ps->fds_status,
				     sizeof(ErtsFdStatus)*new_len)
		      : erts_alloc(ERTS_ALC_T_FD_STATUS,
				   sizeof(ErtsFdStatus)*new_len));
    for (i = ps->fds_status_len; i < new_len; i++) {
#if ERTS_POLL_USE_POLL
	ps->fds_status[i].pix = -1;
#endif
	ps->fds_status[i].used_events = (ErtsPollEvents) 0;
	ps->fds_status[i].events = (ErtsPollEvents) 0;
#if ERTS_POLL_USE_KQUEUE
	ps->fds_status[i].res_ev_ix = (unsigned short) ERTS_POLL_MAX_RES;
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE || ERTS_POLL_USE_FALLBACK
	ps->fds_status[i].flags = (unsigned short) 0;
#endif
    }
    ps->fds_status_len = new_len;
}

/*
 * --- Selecting fd to poll on -----------------------------------------------
 */

#if ERTS_POLL_USE_FALLBACK
static int update_fallback_pollset(ErtsPollSet ps, int fd);
#endif

static ERTS_INLINE int
need_update(ErtsPollSet ps, int fd)
{
#if ERTS_POLL_USE_KERNEL_POLL
    int reset;
#endif

    ASSERT(fd < ps->fds_status_len);

#if ERTS_POLL_USE_KERNEL_POLL
    reset = (int) (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST);
    if (reset && !ps->fds_status[fd].used_events) {
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
	reset = 0;
    }
#elif defined(ERTS_SMP)
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
#endif

    if (ps->fds_status[fd].used_events != ps->fds_status[fd].events)
	return 1;

#if ERTS_POLL_USE_KERNEL_POLL
    return reset;
#else
    return 0;
#endif
}

#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET

#if ERTS_POLL_USE_KQUEUE
#define ERTS_POLL_MIN_BATCH_BUF_SIZE 128
#else
#define ERTS_POLL_MIN_BATCH_BUF_SIZE 64
#endif

typedef struct {
    int len;
    int size;
#if ERTS_POLL_USE_DEVPOLL
    struct pollfd *buf;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *buf;
    struct kevent *ebuf;
#endif
} ErtsPollBatchBuf;


static ERTS_INLINE void
setup_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    bbp->len = 0;
#if ERTS_POLL_USE_DEVPOLL
    bbp->size = ps->res_events_len;
    bbp->buf = ps->res_events;
#elif ERTS_POLL_USE_KQUEUE
    bbp->size = ps->res_events_len/2;
    bbp->buf = ps->res_events;
    bbp->ebuf = bbp->buf + bbp->size;
#endif
}


#if ERTS_POLL_USE_DEVPOLL

static void
write_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    ssize_t wres;
    char *buf = (char *) bbp->buf;
    size_t buf_size = sizeof(struct pollfd)*bbp->len;
    
    while (1) {
	wres = write(ps->kp_fd, (void *) buf, buf_size);
	if (wres < 0) {
	    if (errno == EINTR)
		continue;
	    fatal_error("%s:%d:write_batch_buf(): "
			"Failed to write to /dev/poll: "
			"%s (%d)\n",
			__FILE__, __LINE__,
			erl_errno_id(errno), errno);
	}
	buf_size -= wres;
	if (buf_size <= 0)
	    break;
	buf += wres;
    }

    if (buf_size < 0) {
	fatal_error("%s:%d:write_devpoll_buf(): Internal error\n",
		    __FILE__, __LINE__);
    }
    bbp->len = 0;
}

#elif ERTS_POLL_USE_KQUEUE

static void
write_batch_buf(ErtsPollSet ps, ErtsPollBatchBuf *bbp)
{
    int res;
    int len = bbp->len;
    struct kevent *buf = bbp->buf;
    struct timespec ts = {0, 0};

    do {
	res = kevent(ps->kp_fd, buf, len, NULL, 0, &ts);
    } while (res < 0 && errno == EINTR);
    if (res < 0) {
	int i;
	struct kevent *ebuf = bbp->ebuf;
	do {
	    res = kevent(ps->kp_fd, buf, len, ebuf, len, &ts);
	} while (res < 0 && errno == EINTR);
	if (res < 0) {
	    fatal_error("%s:%d: kevent() failed: %s (%d)\n",
			__FILE__, __LINE__, erl_errno_id(errno), errno);
	}
	for (i = 0; i < res; i++) {
	    if (ebuf[i].flags & EV_ERROR) {
		short filter;
		int fd = (int) ebuf[i].ident;

		switch ((int) ebuf[i].udata) {

		    /*
		     * Since we use a lazy update approach EV_DELETE will
		     * frequently fail. This since kqueue automatically
		     * removes a file descriptor that is closed from the
		     * poll set.
		     */
		case ERTS_POLL_KQ_OP_DEL_R:
		case ERTS_POLL_KQ_OP_DEL_W:
		case ERTS_POLL_KQ_OP_HANDLED:
		    break;

		    /*
		     * According to the kqueue man page EVFILT_READ support
		     * does not imply EVFILT_WRITE support; therefore,
		     * if an EV_ADD fail, we may have to remove other
		     * events on this fd in the kqueue pollset before
		     * adding fd to the fallback pollset.
		     */
		case ERTS_POLL_KQ_OP_ADD_W:
		    if (ps->fds_status[fd].used_events & ERTS_POLL_EV_IN) {
			filter = EVFILT_READ;
			goto rm_add_fb;
		    }
		    goto add_fb;
		case ERTS_POLL_KQ_OP_ADD_R:
		    if (ps->fds_status[fd].used_events & ERTS_POLL_EV_OUT) {
			filter = EVFILT_WRITE;
			goto rm_add_fb;
		    }
		    goto add_fb;
		case ERTS_POLL_KQ_OP_ADD2_W:
		case ERTS_POLL_KQ_OP_ADD2_R: {
		    int j;
		    for (j = i+1; j < res; j++) {
			if (fd == (int) ebuf[j].ident) {
			    ebuf[j].udata = (void *) ERTS_POLL_KQ_OP_HANDLED;
			    if (!(ebuf[j].flags & EV_ERROR)) {
				switch ((int) ebuf[j].udata) {
				case ERTS_POLL_KQ_OP_ADD2_W:
				    filter = EVFILT_WRITE;
				    goto rm_add_fb;
				case ERTS_POLL_KQ_OP_ADD2_R:
				    filter = EVFILT_READ;
				    goto rm_add_fb;
				default:
				    fatal_error("%s:%d:write_batch_buf(): "
						"Internal error",
						__FILE__, __LINE__);
				    break;
				}
			    }
			    goto add_fb;
			}
		    }
		    /* The other add succeded... */
		    filter = (((int) ebuf[i].udata == ERTS_POLL_KQ_OP_ADD2_W)
			      ? EVFILT_READ
			      : EVFILT_WRITE);
		rm_add_fb:
		    { 
			struct kevent kev;
			struct timespec ts = {0, 0};
			EV_SET(&kev, fd, filter, EV_DELETE, 0, 0, 0);
			(void) kevent(ps->kp_fd, &kev, 1, NULL, 0, &ts);
		    }

		add_fb:
		    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_USEFLBCK;
		    ASSERT(ps->fds_status[fd].used_events);
		    ps->fds_status[fd].used_events = 0;
		    ps->no_of_user_fds--;
		    update_fallback_pollset(ps, fd);
		    ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
		    break;
		}
		default:
		    fatal_error("%s:%d:write_batch_buf(): Internal error",
				__FILE__, __LINE__);
		    break;
		}
	    }
	}
    }
    bbp->len = 0;
}

#endif /* ERTS_POLL_USE_KQUEUE */

static ERTS_INLINE void
batch_update_pollset(ErtsPollSet ps, int fd, ErtsPollBatchBuf *bbp)
{
    int buf_len;
#if ERTS_POLL_USE_DEVPOLL
    short events;
    struct pollfd *buf;
#elif ERTS_POLL_USE_KQUEUE
    struct kevent *buf;
#endif

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Doing lazy update on fd=%d\n", fd);
#endif

    if (!need_update(ps, fd))
	return;

    /* Make sure we have room for at least maximum no of entries
       per fd */
    if (bbp->size - bbp->len < 2)
	write_batch_buf(ps, bbp);

    buf_len = bbp->len;
    buf = bbp->buf;

    ASSERT(fd < ps->fds_status_len);

#if ERTS_POLL_USE_DEVPOLL
    events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
    if (!events) {
	buf[buf_len].events = POLLREMOVE;
	ps->no_of_user_fds--;
    }
    else if (!ps->fds_status[fd].used_events) {
	buf[buf_len].events = events;
	ps->no_of_user_fds++;
    }
    else {
	if ((ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)
	    || (ps->fds_status[fd].used_events & ~events)) {
	    /* Reset or removed events... */
	    buf[buf_len].fd = fd;
	    buf[buf_len].events = POLLREMOVE;
	    buf[buf_len++].revents = 0;
	}
	buf[buf_len].events = events;
    }
    buf[buf_len].fd = fd;
    buf[buf_len++].revents = 0;

#elif ERTS_POLL_USE_KQUEUE

    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK) {
	if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK)
	    update_fallback_pollset(ps, fd);
	else { /* Remove from fallback and try kqueue */
	    ErtsPollEvents events = ps->fds_status[fd].events;
	    ps->fds_status[fd].events = (ErtsPollEvents) 0;
	    update_fallback_pollset(ps, fd);
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    if (events) {
		ps->fds_status[fd].events = events;
		goto try_kqueue;
	    }
	}
    }
    else {
	ErtsPollEvents events, used_events;
	int mod_w, mod_r;
    try_kqueue:
	events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
	used_events = ERTS_POLL_EV_E2N(ps->fds_status[fd].used_events);
	if (!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)) {
	    if (!used_events &&
		(events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT))
		goto do_add_rw;
	    mod_r = ((events & ERTS_POLL_EV_IN)
		     != (used_events & ERTS_POLL_EV_IN));
	    mod_w = ((events & ERTS_POLL_EV_OUT)
		     != (used_events & ERTS_POLL_EV_OUT));
	    goto do_mod;
	}
	else { /* Reset */
	    if ((events & ERTS_POLL_EV_IN) && (events & ERTS_POLL_EV_OUT)) {
	    do_add_rw:
		EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_ADD,
		       0, 0, (void *) ERTS_POLL_KQ_OP_ADD2_R);
		buf_len++;
		EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_ADD,
		       0, 0, (void *) ERTS_POLL_KQ_OP_ADD2_W);
		buf_len++;

	    }
	    else {
		mod_r = 1;
		mod_w = 1;
	    do_mod:
		if (mod_r) {
		    if (events & ERTS_POLL_EV_IN) {
			EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_ADD,
			       0, 0, (void *) ERTS_POLL_KQ_OP_ADD_R);
			buf_len++;
		    }
		    else if (used_events & ERTS_POLL_EV_IN) {
			EV_SET(&buf[buf_len], fd, EVFILT_READ, EV_DELETE,
			       0, 0, (void *) ERTS_POLL_KQ_OP_DEL_R);
			buf_len++;
		    }
		}
		if (mod_w) {
		    if (events & ERTS_POLL_EV_OUT) {
			EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_ADD,
			       0, 0, (void *) ERTS_POLL_KQ_OP_ADD_W);
			buf_len++;
		    }
		    else if (used_events & ERTS_POLL_EV_OUT) {
			EV_SET(&buf[buf_len], fd, EVFILT_WRITE, EV_DELETE,
			       0, 0, (void *) ERTS_POLL_KQ_OP_DEL_W);
			buf_len++;
		    }
		}
	    }
	}
	if (used_events) {
	    if (!events) {
		ps->no_of_user_fds--;
	    }
	}
	else {
	    if (events)
		ps->no_of_user_fds++;
	}
	ASSERT((events & ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) == 0);
	ASSERT((used_events & ~(ERTS_POLL_EV_IN|ERTS_POLL_EV_OUT)) == 0);
    }	    

#endif

    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
    ps->fds_status[fd].used_events = ps->fds_status[fd].events;

    bbp->len = buf_len;
}

#else /* !ERTS_POLL_USE_BATCH_UPDATE_POLLSET */

#if ERTS_POLL_USE_EPOLL
static int
#if ERTS_POLL_USE_CONCURRENT_UPDATE
conc_update_pollset(ErtsPollSet ps, int fd, int *update_fallback)
#else
update_pollset(ErtsPollSet ps, int fd)
#endif
{
    int res;
    int op;
    struct epoll_event epe;

    ASSERT(fd < ps->fds_status_len);

    if (!need_update(ps, fd))
	return 0;

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Doing update on fd=%d\n", fd);
#endif
    if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK) {
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	if (!*update_fallback) {
	    *update_fallback = 1;
	    return 0;
	}
#endif
	if (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK) {
	    return update_fallback_pollset(ps, fd);
	}
	else { /* Remove from fallback and try epoll */
	    ErtsPollEvents events = ps->fds_status[fd].events;
	    ps->fds_status[fd].events = (ErtsPollEvents) 0;
	    res = update_fallback_pollset(ps, fd);
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    if (!events)
		return res;
	    ps->fds_status[fd].events = events;
	}
    }

    epe.events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
    epe.data.fd = fd;

    if (epe.events && ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST) {
	do {
	    res = epoll_ctl(ps->kp_fd, EPOLL_CTL_DEL, fd, &epe);
	} while (res != 0 && errno == EINTR);
	ps->no_of_user_fds--;
	ps->fds_status[fd].used_events = 0;
	epe.events = ERTS_POLL_EV_E2N(ps->fds_status[fd].events);
	epe.data.fd = fd;
    }

    if (!epe.events) {
	/* A note on EPOLL_CTL_DEL: linux kernel versions before 2.6.9
	   need a non-NULL event pointer even though it is ignored... */
	op = EPOLL_CTL_DEL;
	ps->no_of_user_fds--;
    }
    else if (!ps->fds_status[fd].used_events) {
	op = EPOLL_CTL_ADD;
	ps->no_of_user_fds++;
    }
    else {
	op = EPOLL_CTL_MOD;
    }

    do {
	res = epoll_ctl(ps->kp_fd, op, fd, &epe);
    } while (res != 0 && errno == EINTR);

#if defined(ERTS_POLL_DEBUG_PRINT) && 1
    {
	int saved_errno = errno;
	erts_printf("%s = epoll_ctl(%d, %s, %d, {Ox%x, %d})\n",
		     res == 0 ? "0" : erl_errno_id(errno),
		     ps->kp_fd,
		     (op == EPOLL_CTL_ADD
		      ? "EPOLL_CTL_ADD"
		      : (op == EPOLL_CTL_MOD
			 ? "EPOLL_CTL_MOD"
			 : (op == EPOLL_CTL_DEL
			    ? "EPOLL_CTL_DEL"
			    : "UNKNOWN"))),
		     fd,
		     epe.events,
		     fd);
	errno = saved_errno;
    }
#endif
    if (res == 0)
	ps->fds_status[fd].used_events = ps->fds_status[fd].events;
    else {
	switch (op) {
	case EPOLL_CTL_MOD:
	    epe.events = 0;
	    do {
		res = epoll_ctl(ps->kp_fd, EPOLL_CTL_DEL, fd, &epe);
	    } while (res != 0 && errno == EINTR);
	    ps->fds_status[fd].used_events = 0;
	/* Fall through ... */
	case EPOLL_CTL_ADD: {
	    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_USEFLBCK;
	    ps->no_of_user_fds--;
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	    if (!*update_fallback) {
		*update_fallback = 1;
		return 0;
	    }
#endif
	    res = update_fallback_pollset(ps, fd);
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
	    break;
	}
	case EPOLL_CTL_DEL: {
	    /*
	     * Since we use a lazy update approach EPOLL_CTL_DEL will
	     * frequently fail. This since epoll automatically removes
	     * a filedescriptor that is closed from the poll set.
	     */
	    ps->fds_status[fd].used_events = 0;
	    res = 0;
	    break;
	}
	default:
	    fatal_error("%s:%d:update_pollset(): Internal error\n",
			__FILE__, __LINE__);
	    break;
	}
    }
    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_RST;
    return res;
}

#if ERTS_POLL_USE_CONCURRENT_UPDATE
static int
update_pollset(ErtsPollSet ps, int fd)
{
    int update_fallback = 1;
    return conc_update_pollset(ps, fd, &update_fallback);
}
#endif

#endif /* ERTS_POLL_USE_EPOLL */

#endif /* ERTS_POLL_USE_BATCH_UPDATE_POLLSET */

#if ERTS_POLL_USE_POLL || ERTS_POLL_USE_SELECT || ERTS_POLL_USE_FALLBACK

#if ERTS_POLL_USE_FALLBACK
static int update_fallback_pollset(ErtsPollSet ps, int fd)
#else
static int update_pollset(ErtsPollSet ps, int fd)
#endif
{
#ifdef ERTS_POLL_DEBUG_PRINT
#if ERTS_POLL_USE_FALLBACK
    erts_printf("Doing fallback update on fd=%d\n", fd);
#else
    erts_printf("Doing update on fd=%d\n", fd);
#endif
#endif

    ASSERT(fd < ps->fds_status_len);
#if ERTS_POLL_USE_FALLBACK
    ASSERT(ps->fds_status[fd].used_events
	   ? (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK)
	   : (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_USEFLBCK));
#endif

    if (!need_update(ps, fd))
	return 0;

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
    if (!ps->fds_status[fd].events) {
	int pix = ps->fds_status[fd].pix;
	int last_pix;
	if (pix < 0) {
#if ERTS_POLL_USE_FALLBACK
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK));
#endif
	    return -1;
	}
#if ERTS_POLL_USE_FALLBACK
	ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
#endif
	ps->no_of_user_fds--;
	last_pix = --ps->no_poll_fds;
	if (pix != last_pix) {
	/* Move last pix to this pix */
	    ps->poll_fds[pix].fd = ps->poll_fds[last_pix].fd;
	    ps->poll_fds[pix].events = ps->poll_fds[last_pix].events;
	    ps->poll_fds[pix].revents = ps->poll_fds[last_pix].revents;
	    ps->fds_status[ps->poll_fds[pix].fd].pix = pix;
	}
	/* Clear last pix */
	ps->poll_fds[last_pix].fd = -1;
	ps->poll_fds[last_pix].events = (short) 0;
	ps->poll_fds[last_pix].revents = (short) 0;
	/* Clear this fd status */
	ps->fds_status[fd].pix = -1;
	ps->fds_status[fd].used_events = (ErtsPollEvents) 0;
#if ERTS_POLL_USE_FALLBACK
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INFLBCK;
#endif
    }
    else {
	int pix = ps->fds_status[fd].pix;
	if (pix < 0) {
#if ERTS_POLL_USE_FALLBACK
	    ASSERT(!(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK)
		    || fd == ps->kp_fd);
#endif
	    ps->no_of_user_fds++;
	    ps->fds_status[fd].pix = pix = ps->no_poll_fds++;
	    if (pix >= ps->poll_fds_len)
		grow_poll_fds(ps, pix);
	    ps->poll_fds[pix].fd = fd;
	    ps->fds_status[fd].pix = pix;
#if ERTS_POLL_USE_FALLBACK
	    ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INFLBCK;
#endif
	}

#if ERTS_POLL_USE_FALLBACK
	ASSERT(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_INFLBCK);
#endif

	/* Events to be used in next poll */
	ps->poll_fds[pix].events = ev2pollev(ps->fds_status[fd].events);
	if (ps->poll_fds[pix].revents) {
	    /* Remove result events that we should not poll for anymore */
	    ps->poll_fds[pix].revents
		&= ev2pollev(~(~ps->fds_status[fd].used_events
			       & ps->fds_status[fd].events));
	}
	/* Save events to be used in next poll */
	ps->fds_status[fd].used_events = ps->fds_status[fd].events;
    }
    return 0;
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
    ErtsPollEvents events = ps->fds_status[fd].events;
    if ((ERTS_POLL_EV_IN & events)
	!= (ERTS_POLL_EV_IN & ps->fds_status[fd].used_events)) {
	if (ERTS_POLL_EV_IN & events) {
	    FD_SET(fd, &ps->input_fds);
	}
	else {
	    FD_CLR(fd, &ps->input_fds);
	}
    }
    if ((ERTS_POLL_EV_OUT & events)
	!= (ERTS_POLL_EV_OUT & ps->fds_status[fd].used_events)) {
	if (ERTS_POLL_EV_OUT & events) {
	    FD_SET(fd, &ps->output_fds);
	}
	else {
	    FD_CLR(fd, &ps->output_fds);
	}
    }

    if (!ps->fds_status[fd].used_events) {
	ASSERT(events);
	ps->no_of_user_fds++;
#if ERTS_POLL_USE_FALLBACK
	ps->no_select_fds++,
	ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_INFLBCK;
#endif
    }
    else if (!events) {
	ASSERT(ps->fds_status[fd].used_events);
	ps->no_of_user_fds--;
	ps->fds_status[fd].events = events;
#if ERTS_POLL_USE_FALLBACK
	ps->no_select_fds--,
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INFLBCK;
#endif
    }

    ps->fds_status[fd].used_events = events;

    if (events && fd > ps->max_fd)
	ps->max_fd = fd;
    else if (!events && fd == ps->max_fd) {
	int max = ps->max_fd;
	for (max = ps->max_fd; max >= 0; max--)
	    if (ps->fds_status[max].used_events)
		break;
	ps->max_fd = max;
    }

    return 0;
#endif
}

#endif /* ERTS_POLL_USE_POLL || ERTS_POLL_USE_SELECT || ERTS_POLL_USE_FALLBACK */

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE

static void
handle_update_requests(ErtsPollSet ps)
{
    ErtsPollSetUpdateRequestsBlock *urqbp = &ps->update_requests;
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    ErtsPollBatchBuf bb;
    setup_batch_buf(ps, &bb);
#endif

    while (urqbp) {
	ErtsPollSetUpdateRequestsBlock *free_urqbp = urqbp;
	int i;
	int len = urqbp->len;
	for (i = 0; i < len; i++) {
	    int fd = urqbp->fds[i];
	    ASSERT(fd < ps->fds_status_len);
	    ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_INURQ;
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
	    batch_update_pollset(ps, fd, &bb);
#else
	    update_pollset(ps, fd);
#endif
	}

	free_urqbp = urqbp;
	urqbp = urqbp->next;

	free_update_requests_block(ps, free_urqbp);

    }

#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    if (bb.len)
	write_batch_buf(ps, &bb);
#endif

    ps->curr_upd_req_block = &ps->update_requests;

#if ERTS_POLL_USE_DEVPOLL && defined(HARD_DEBUG)
    check_poll_status(ps);
#endif

    ERTS_POLLSET_UNSET_HAVE_UPDATE_REQUESTS(ps);
}

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

static ERTS_INLINE ErtsPollEvents
poll_control(ErtsPollSet ps, int fd, ErtsPollEvents events, int on,
	     int *have_set_have_update_requests, int *do_wake)
{
    ErtsPollEvents new_events;

    if (fd < ps->internal_fd_limit || fd >= max_fds) {
	if (fd < 0) {
	    new_events = ERTS_POLL_EV_ERR;
	    goto done;
	}
#if ERTS_POLL_USE_KERNEL_POLL
	if (fd == ps->kp_fd) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
#ifdef ERTS_SMP
	if (fd == ps->wake_fds[0] || fd == ps->wake_fds[1]) {
	    new_events = ERTS_POLL_EV_NVAL;
	    goto done;
	}
#endif
    }

    if (fd >= ps->fds_status_len)
	grow_fds_status(ps, fd);

    ASSERT(fd < ps->fds_status_len);

    new_events = ps->fds_status[fd].events;

    if (events == 0)
	goto done;

    if (on)
	new_events |= events;
    else
	new_events &= ~events;

    if ((new_events & ~ERTS_POLL_EV_FULL_1SHOT) == 0) {
        new_events = (ErtsPollEvents) 0;
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
	ps->fds_status[fd].flags |= ERTS_POLL_FD_FLG_RST;
#endif
#if ERTS_POLL_USE_FALLBACK
	ps->fds_status[fd].flags &= ~ERTS_POLL_FD_FLG_USEFLBCK;
#endif
    }

    ps->fds_status[fd].events = new_events;

    if (new_events == ps->fds_status[fd].used_events
#if ERTS_POLL_USE_KERNEL_POLL || defined(ERTS_SMP)
	&& !(ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST)
#endif
	) {
	goto done;
    }

#if !ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    if (update_pollset(ps, fd) != 0)
	new_events = ERTS_POLL_EV_ERR;
#else /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

#if ERTS_POLL_USE_CONCURRENT_UPDATE
    if (ERTS_POLLSET_IS_POLLED(ps)) {
	int update_fallback = 0;
	conc_update_pollset(ps, fd, &update_fallback);
	if (!update_fallback)
	    goto done;
    }
#endif

    enqueue_update_request(ps, fd);
	
#ifdef ERTS_SMP
    /*
     * If new events have been added, we need to wake up the
     * polling thread, but if events have been removed we don't.
     */
    if ((new_events && (ps->fds_status[fd].flags & ERTS_POLL_FD_FLG_RST))
	|| (~ps->fds_status[fd].used_events & new_events))
	*do_wake = 1;
#endif /* ERTS_SMP */

#endif /* ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE */

 done:
#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("0x%x = poll_control(ps, %d, 0x%x, %s)\n",
		 (int) new_events, fd, (int) events, on ? "on" : "off");
#endif
    return new_events;
}

void
ERTS_POLL_EXPORT(erts_poll_controlv)(ErtsPollSet ps,
				     ErtsPollControlEntry pcev[],
				     int len)
{
    int i;
    int hshur = 0;
    int do_wake = 0;

    ERTS_POLLSET_LOCK(ps);

    for (i = 0; i < len; i++) {
	pcev[i].events = poll_control(ps,
				      pcev[i].fd,
				      pcev[i].events,
				      pcev[i].on,
				      &hshur,
				      &do_wake);
    }

#ifdef ERTS_SMP
    if (do_wake)
	wake_poller(ps);
#endif /* ERTS_SMP */

    ERTS_POLLSET_UNLOCK(ps);
}

ErtsPollEvents
ERTS_POLL_EXPORT(erts_poll_control)(ErtsPollSet ps,
				    int fd,
				    ErtsPollEvents events, int on)
{
    int hshur = 0;
    int do_wake = 0;
    ErtsPollEvents res;

    ERTS_POLLSET_LOCK(ps);

    res = poll_control(ps, fd, events, on, &hshur, &do_wake);

#ifdef ERTS_SMP
    if (do_wake)
	wake_poller(ps);
#endif /* ERTS_SMP */

    ERTS_POLLSET_UNLOCK(ps);
    return res;
}

/*
 * --- Wait on poll set ------------------------------------------------------
 */

static ERTS_INLINE void
handle_1shot(ErtsPollSet ps, int fd, ErtsPollEvents revents)
{
    ErtsPollEvents events;

    ASSERT(fd < ps->fds_status_len);

    events = ps->fds_status[fd].events;
    if (events & ERTS_POLL_EV_1SHOT) {
	if (events & ERTS_POLL_EV_FULL_1SHOT)
	    events = (ErtsPollEvents) 0;
	else {
	    if (revents & ERTS_POLL_EV_IN) {
		if (revents & ERTS_POLL_EV_OUT)
		    events = (ErtsPollEvents) 0;
		else
		    events &= ~ERTS_POLL_EV_IN;
	    }
	    else if (revents & ERTS_POLL_EV_OUT)
		events &= ~ERTS_POLL_EV_OUT;
	}
	if (ps->fds_status[fd].used_events != events) {
	    ps->fds_status[fd].events = events;
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
	    enqueue_update_request(ps, fd);
#else
	    update_pollset(ps, fd);
#endif
	}
    }
}

#if ERTS_POLL_USE_KERNEL_POLL

static ERTS_INLINE int
save_kp_result(ErtsPollSet ps, ErtsPollResFd pr[], int max_res, int chk_fds_res)
{
    int res = 0;
    int i;
    int n = chk_fds_res < max_res ? chk_fds_res : max_res;
#ifdef ERTS_SMP
    int wake_fd = ps->wake_fds[0];
#endif

    for (i = 0; i < n; i++) {

#if ERTS_POLL_USE_EPOLL		/* --- epoll ------------------------------- */

	if (ps->res_events[i].events) {
	    int fd = ps->res_events[i].data.fd;
	    ErtsPollEvents revents;
#ifdef ERTS_SMP
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
	    revents = ERTS_POLL_EV_N2E(ps->res_events[i].events);
	    pr[res].fd = fd;
	    pr[res].events = revents;
	    handle_1shot(ps, fd, revents);
	    res++;
	}

#elif ERTS_POLL_USE_KQUEUE	/* --- kqueue ------------------------------ */

	struct kevent *ev;
	int fd;
	int ix;

	ev = &ps->res_events[i];
	fd = (int) ev->ident;
	ASSERT(fd < ps->fds_status_len);
	ix = (int) ps->fds_status[fd].res_ev_ix;

	ASSERT(ix >= 0);
	if (ix >= res || pr[ix].fd != fd) {
	    ix = res;
	    pr[ix].fd = (int) ev->ident;
	    pr[ix].events = (ErtsPollEvents) 0;
	}

	if (ev->filter == EVFILT_READ) {
#ifdef ERTS_SMP
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
	    pr[ix].events |= ERTS_POLL_EV_IN;
	}
	else if (ev->filter == EVFILT_WRITE)
	    pr[ix].events |= ERTS_POLL_EV_OUT;
	if (ev->flags & (EV_ERROR|EV_EOF)) {
	    if ((ev->flags & EV_ERROR) && (((int) ev->data) == EBADF))
		pr[ix].events |= ERTS_POLL_EV_NVAL;
	    else
		pr[ix].events |= ERTS_POLL_EV_ERR;
	}
	if (pr[ix].events) {
	    handle_1shot(ps, fd, pr[ix].events);
	    if (res == ix) {
		ps->fds_status[fd].res_ev_ix = (unsigned short) ix;
		res++;
	    }
	}

#elif ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */

	if (ps->res_events[i].revents) {
	    int fd = ps->res_events[i].fd;
	    ErtsPollEvents revents;
#ifdef ERTS_SMP
	    if (fd == wake_fd) {
		cleanup_wakeup_pipe(ps);
		continue;
	    }
#endif
	    revents = ERTS_POLL_EV_N2E(ps->res_events[i].events);
	    pr[res].fd = fd;
	    pr[res].events = revents;
	    handle_1shot(ps, fd, revents);
	    res++;
	}

#endif

    }

    return res;
}

#endif /* ERTS_POLL_USE_KERNEL_POLL */

#if ERTS_POLL_USE_FALLBACK

static int
get_kp_results(ErtsPollSet ps, ErtsPollResFd pr[], int max_res)
{
    int res;
#if ERTS_POLL_USE_KQUEUE
    struct timespec ts = {0, 0};
#endif

    if (max_res > ps->res_events_len)
	grow_res_events(ps, max_res);

    do {
#if ERTS_POLL_USE_EPOLL
	res = epoll_wait(ps->kp_fd, ps->res_events, max_res, 0);
#elif ERTS_POLL_USE_KQUEUE
	res = kevent(ps->kp_fd, NULL, 0, ps->res_events, max_res, &ts);
#endif
    } while (res < 0 && errno == EINTR);

    if (res < 0) {
	fatal_error("%s:%d: %s() failed: %s (%d)\n",
		    __FILE__, __LINE__,
#if ERTS_POLL_USE_EPOLL
		    "epoll_wait",
#elif ERTS_POLL_USE_KQUEUE
		    "kevent",
#endif
		    erl_errno_id(errno), errno);
    }

    return save_kp_result(ps, pr, max_res, res);
}

#endif /* ERTS_POLL_USE_FALLBACK */



static ERTS_INLINE int
save_poll_result(ErtsPollSet ps, ErtsPollResFd pr[], int max_res,
		 int chk_fds_res, int ebadf)
{
#if ERTS_POLL_USE_DEVPOLL
    return save_kp_result(ps, pr, max_res, chk_fds_res);
#elif ERTS_POLL_USE_FALLBACK
    if (!ps->fallback_used)
	return save_kp_result(ps, pr, max_res, chk_fds_res);
    else
#endif /* ERTS_POLL_USE_FALLBACK */
    {

#if ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
	int res = 0;
#if defined(ERTS_SMP) && !ERTS_POLL_USE_FALLBACK 
	int wake_fd = ps->wake_fds[0];
#endif
	int i, first_ix, end_ix;

	/*
	 * In order to be somewhat fair, we continue on the poll_fds
	 * index where we stopped last time.
	 */
	first_ix = i = ((ps->next_poll_fds_ix < ps->no_poll_fds)
			? ps->next_poll_fds_ix
			: 0);
	end_ix = ps->no_poll_fds;

	while (1) {
	    while (i < end_ix && res < max_res) {
		if (ps->poll_fds[i].revents != (short) 0) {
		    int fd = ps->poll_fds[i].fd;
		    ErtsPollEvents revents;
#if ERTS_POLL_USE_FALLBACK
		    if (fd == ps->kp_fd) {
			res += get_kp_results(ps, &pr[res], max_res-res);
			i++;
			continue;
		    }
#elif defined(ERTS_SMP)
		    if (fd == wake_fd) {
			cleanup_wakeup_pipe(ps);
			i++;
			continue;
		    }
#endif
		    revents = pollev2ev(ps->poll_fds[i].revents);
		    pr[res].fd = fd;
		    pr[res].events = revents;
		    handle_1shot(ps, fd, revents);
		    res++;
		}
		i++;
	    }
	    if (res == max_res || i == first_ix)
		break;
	    ASSERT(i == ps->no_poll_fds);
	    i = 0;
	    end_ix = first_ix;
	}

	ps->next_poll_fds_ix = i;
	return res;

#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
	int res = 0;
#if defined(ERTS_SMP) && !ERTS_POLL_USE_FALLBACK 
	int wake_fd = ps->wake_fds[0];
#endif
	int fd, first_fd, end_fd;

	/*
	 * In order to be fair, we continue on the fd where we stopped
	 * last time.
	 */
	first_fd = fd = ps->next_sel_fd <= ps->max_fd ? ps->next_sel_fd : 0;
	end_fd = ps->max_fd + 1;

	if (!ebadf) {
	    while (1) {
		while (fd < end_fd && res < max_res) {

		    pr[res].events = (ErtsPollEvents) 0;
		    if (FD_ISSET(fd, &ps->res_input_fds)) {
#if ERTS_POLL_USE_FALLBACK
			if (fd == ps->kp_fd) {
			    res += get_kp_results(ps, &pr[res], max_res-res);
			    fd++;
			    continue;
			}
#elif defined(ERTS_SMP)
			if (fd == wake_fd) {
			    cleanup_wakeup_pipe(ps);
			    fd++;
			    continue;
			}
#endif
			pr[res].events |= ERTS_POLL_EV_IN;
		    }
		    if (FD_ISSET(fd, &ps->res_output_fds))
			pr[res].events |= ERTS_POLL_EV_OUT;
		    if (pr[res].events) {
			pr[res].fd = fd;
			handle_1shot(ps, fd, pr[res].events);
			res++;
		    }
		    fd++;
		}
		if (res == max_res || fd == first_fd)
		    break;
		ASSERT(fd == ps->max_fd + 1);
		fd = 0;
		end_fd = first_fd;
	    }
	}
	else {
	    /*
	     * Bad file descriptors in poll set.
	     *
	     * This only happens when running poorly written
	     * drivers. This code could be optimized, but we
	     * don't bother since it should never happen...
	     */
	    while (1) {
		while (fd < end_fd && res < max_res) {
		    if (ps->fds_status[fd].events) {
			int sres;
			fd_set *iset = NULL;
			fd_set *oset = NULL;
			if (ps->fds_status[fd].events & ERTS_POLL_EV_IN) {
			    iset = &ps->res_input_fds;
			    FD_ZERO(iset);
			    FD_SET(fd, iset);
			}
			if (ps->fds_status[fd].events & ERTS_POLL_EV_OUT) {
			    oset = &ps->res_output_fds;
			    FD_ZERO(oset);
			    FD_SET(fd, oset);
			
			}
			do {
			    /* Initiate 'tv' each time;
			       select() may modify it */
			    SysTimeval tv = {0, 0};
			    sres = select(ps->max_fd+1, iset, oset, NULL, &tv);
			} while (sres < 0 && errno == EINTR);
			if (sres < 0) {
#if ERTS_POLL_USE_FALLBACK
			    if (fd == ps->kp_fd) {
				res += get_kp_results(ps,
						      &pr[res],
						      max_res-res);
				fd++;
				continue;
			    }
#elif  defined(ERTS_SMP)
			    if (fd == wake_fd) {
				cleanup_wakeup_pipe(ps);
				fd++;
				continue;
			    }
#endif
			    pr[res].fd = fd;
			    pr[res].events = ERTS_POLL_EV_NVAL;
			    handle_1shot(ps, fd, pr[res].events);
			    res++;
			}
			else if (sres > 0) {
			    pr[res].fd = fd;
			    if (iset && FD_ISSET(fd, iset)) {
#if ERTS_POLL_USE_FALLBACK
				if (fd == ps->kp_fd) {
				    res += get_kp_results(ps,
							  &pr[res],
							  max_res-res);
				    fd++;
				    continue;
				}
#elif  defined(ERTS_SMP)
				if (fd == wake_fd) {
				    cleanup_wakeup_pipe(ps);
				    fd++;
				    continue;
				}
#endif
				pr[res].events |= ERTS_POLL_EV_IN;
			    }
			    if (oset && FD_ISSET(fd, oset)) {
				pr[res].events |= ERTS_POLL_EV_OUT;
			    }
			    ASSERT(pr[res].events);
			    handle_1shot(ps, fd, pr[res].events);
			    res++;
			}
		    }
		    fd++;
		}
		if (res == max_res || fd == first_fd)
		    break;
		ASSERT(fd == ps->max_fd + 1);
		fd = 0;
		end_fd = first_fd;
	    }
	}
	ps->next_sel_fd = fd;
	return res;
#endif
    }
}

static ERTS_INLINE int
check_fd_events(ErtsPollSet ps, SysTimeval *tv, int max_res, int *ps_locked)
{
    ASSERT(!*ps_locked);
    if (ps->no_of_user_fds == 0 && tv->tv_usec == 0 && tv->tv_sec == 0) {
	/* Nothing to poll and zero timeout; done... */
	return 0;
    }
    else {

#if ERTS_POLL_USE_FALLBACK
	if (!(ps->fallback_used = ERTS_POLL_NEED_FALLBACK(ps))) {

#if ERTS_POLL_USE_EPOLL		/* --- epoll ------------------------------- */
	    int timeout = tv->tv_sec*1000 + tv->tv_usec/1000;
	    if (max_res > ps->res_events_len)
		grow_res_events(ps, max_res);
	    return epoll_wait(ps->kp_fd, ps->res_events, max_res, timeout);
#elif ERTS_POLL_USE_KQUEUE	/* --- kqueue ------------------------------ */
	    struct timespec ts;
	    ts.tv_sec = tv->tv_sec;
	    ts.tv_nsec = tv->tv_usec*1000;
	    if (max_res > ps->res_events_len)
		grow_res_events(ps, max_res);
	    return kevent(ps->kp_fd, NULL, 0, ps->res_events, max_res, &ts);
#endif				/* ----------------------------------------- */

	}
	else /* use fallback (i.e. poll() or select()) */
#endif /* ERTS_POLL_USE_FALLBACK */
	{

#if ERTS_POLL_USE_DEVPOLL	/* --- devpoll ----------------------------- */
	    struct dvpoll poll_res;
	    if (max_res > ps->res_events_len)
		grow_res_events(ps, max_res);
	    poll_res.dp_fds = ps->res_events;
	    poll_res.dp_nfds = max_res;
	    poll_res.dp_timeout = tv->tv_sec*1000 + tv->tv_usec/1000;
	    return ioctl(ps->kp_fd, DP_POLL, &poll_res);
#elif ERTS_POLL_USE_POLL	/* --- poll -------------------------------- */
	    int timeout = tv->tv_sec*1000 + tv->tv_usec/1000;
	    return poll(ps->poll_fds, ps->no_poll_fds, timeout);
#elif ERTS_POLL_USE_SELECT	/* --- select ------------------------------ */
	    int res;
	    ps->res_input_fds = ps->input_fds;
	    ps->res_output_fds = ps->output_fds;
	    res = select(ps->max_fd + 1,
			 &ps->res_input_fds,
			 &ps->res_output_fds,
			 NULL,
			 tv);
#ifdef ERTS_SMP
	    if (res < 0
		&& errno == EBADF
		&& ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
		/*
		 * This may have happened because another thread deselected
		 * a fd in our poll set and then closed it, i.e. the driver
		 * behaved correctly. We wan't to avoid looking for a bad
		 * fd, that may even not exist anymore. Therefore, handle
		 * update requests and try again.
		 *
		 * We don't know how much of the timeout is left; therfore,
		 * we use a zero timeout. If no error occur and no events
		 * have triggered, we fake an EAGAIN error and let the caller
		 * restart us.
		 */
		SysTimeval zero_tv = {0, 0};
		*ps_locked = 1;
		ERTS_POLLSET_LOCK(ps);
		handle_update_requests(ps);
		res = select(ps->max_fd + 1,
			     &ps->res_input_fds,
			     &ps->res_output_fds,
			     NULL,
			     &zero_tv);
		if (res == 0) {
		    errno = EAGAIN;
		    res = -1;
		}
	    }
#endif /* ERTS_SMP */
	    return res;
#endif				/* ----------------------------------------- */
	}
    }
}

int
ERTS_POLL_EXPORT(erts_poll_wait)(ErtsPollSet ps,
				 ErtsPollResFd pr[],
				 int *len,
				 SysTimeval *tv)
{
    int res, no_fds;
    int ebadf = 0;
    int ps_locked;

    no_fds = *len;
#ifdef ERTS_POLL_MAX_RES
    if (no_fds >= ERTS_POLL_MAX_RES)
	no_fds = ERTS_POLL_MAX_RES;
#endif

    *len = 0;

#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Entering erts_poll_wait(), timeout=%d\n",
		 (int) tv->tv_sec*1000 + tv->tv_usec/1000);
#endif

    ERTS_POLLSET_UNSET_POLLER_WOKEN(ps);
    if (ERTS_POLLSET_SET_POLLED_CHK(ps)) {
	res = EINVAL; /* Another thread is in erts_poll_wait()
			 on this pollset... */
	goto done;
    }
    if (ERTS_POLLSET_UNSET_INTERRUPTED_CHK(ps)) {
#ifdef ERTS_SMP
	ERTS_POLLSET_LOCK(ps);
	cleanup_wakeup_pipe(ps);
	ERTS_POLLSET_UNSET_POLLED(ps);
	ERTS_POLLSET_UNLOCK(ps);
#endif
	res = EINTR;
	goto done;
    }
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    if (ERTS_POLLSET_HAVE_UPDATE_REQUESTS(ps)) {
	ERTS_POLLSET_LOCK(ps);
	handle_update_requests(ps);
	ERTS_POLLSET_UNLOCK(ps);
    }
#endif

    ps_locked = 0;
    res = check_fd_events(ps, tv, no_fds, &ps_locked);

    ERTS_POLLSET_SET_POLLER_WOKEN(ps);

    if (res == 0) {
	res = ETIMEDOUT;
    }
    else if (res < 0) {
#if ERTS_POLL_USE_SELECT
	if (errno == EBADF) {
	    ebadf = 1;
	    goto save_results;
	}
#endif
	res = errno;
    }
    else {
#if ERTS_POLL_USE_SELECT
    save_results:
#endif

#ifdef ERTS_SMP
	if (!ps_locked) {
	    ps_locked = 1;
	    ERTS_POLLSET_LOCK(ps);
	}
#endif

	no_fds = save_poll_result(ps, pr, no_fds, res, ebadf);

	res = (no_fds == 0
	       ? (ERTS_POLLSET_UNSET_INTERRUPTED_CHK(ps) ? EINTR : EAGAIN)
	       : 0);
	*len = no_fds;
    }

#ifdef ERTS_SMP
    if (ps_locked)
	ERTS_POLLSET_UNLOCK(ps);
    ERTS_POLLSET_UNSET_POLLED(ps);
#endif

 done:
#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("Leaving %s = erts_poll_wait()\n",
		 res == 0 ? "0" : erl_errno_id(res));
#endif

    return res;
}

/*
 * --- Interrupt a thread doing erts_poll_wait() -----------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_interrupt)(ErtsPollSet ps, int set)
{
#ifdef ERTS_POLL_DEBUG_PRINT
    erts_printf("erts_poll_interrupt(%p, %d)\n", (void *) ps, set);
#endif
    if (set) {
	ERTS_POLLSET_SET_INTERRUPTED(ps);
#ifdef ERTS_SMP
	wake_poller(ps);
#endif
    }
    else {
	ERTS_POLLSET_UNSET_INTERRUPTED(ps);
    }
}

int
ERTS_POLL_EXPORT(erts_poll_max_fds)(void)
{
    return max_fds;
}
/*
 * --- Initialization --------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_init)(void)
{
    erts_smp_spinlock_init(&pollsets_lock, "pollsets_lock");
    pollsets = NULL;

    errno = 0;

#if !defined(NO_SYSCONF)
    max_fds = sysconf(_SC_OPEN_MAX);
#elif ERTS_POLL_USE_SELECT
    max_fds = NOFILE;
#else
    max_fds = OPEN_MAX;
#endif

#if ERTS_POLL_USE_SELECT && defined(FD_SETSIZE)
    if (max_fds > FD_SETSIZE)
	max_fds = FD_SETSIZE;
#endif

    if (max_fds < 0)
	fatal_error("erts_poll_init(): Failed to get max number of files: %s\n",
		    erl_errno_id(errno));

#ifdef ERTS_POLL_DEBUG_PRINT
    print_misc_debug_info();
#endif
}

ErtsPollSet
ERTS_POLL_EXPORT(erts_poll_create_pollset)(void)
{
#if ERTS_POLL_USE_KERNEL_POLL
    int kp_fd;
#endif
    ErtsPollSet ps = erts_alloc(ERTS_ALC_T_POLLSET,
				sizeof(struct ErtsPollSet_));
    ps->internal_fd_limit = 0;
    ps->fds_status = NULL;
    ps->fds_status_len = 0;
    ps->no_of_user_fds = 0;
#if ERTS_POLL_USE_KERNEL_POLL
    ps->kp_fd = -1;
#if ERTS_POLL_USE_EPOLL
    kp_fd = epoll_create(256);
    ps->res_events_len = 0;
    ps->res_events = NULL;
#elif ERTS_POLL_USE_DEVPOLL
    kp_fd = open("/dev/poll", O_RDWR);
    ps->res_events_len = 0;
    ps->res_events = NULL;
#elif ERTS_POLL_USE_KQUEUE
    kp_fd = kqueue();
    ps->res_events_len = 0;
    ps->res_events = NULL;
#endif
    if (kp_fd < 0)
	fatal_error("erts_poll_create_pollset(): Failed to "
#if ERTS_POLL_USE_EPOLL
		    "create epoll set"
#elif ERTS_POLL_USE_DEVPOLL
		    "to open /dev/poll"
#elif ERTS_POLL_USE_KQUEUE
		    "create kqueue"
#endif
		    ": %s (%d)\n",
		    erl_errno_id(errno), errno);
#endif /* ERTS_POLL_USE_KERNEL_POLL */
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
    /* res_events is also used as write buffer */
    grow_res_events(ps, ERTS_POLL_MIN_BATCH_BUF_SIZE);
#endif
#if ERTS_POLL_USE_POLL
    ps->next_poll_fds_ix = 0;
    ps->no_poll_fds = 0;
    ps->poll_fds_len = 0;
    ps->poll_fds = NULL;
#elif ERTS_POLL_USE_SELECT
    ps->next_sel_fd = 0;
    ps->max_fd = -1;
#if ERTS_POLL_USE_FALLBACK
    ps->no_select_fds = 0;
#endif
    FD_ZERO(&ps->input_fds);
    FD_ZERO(&ps->res_input_fds);
    FD_ZERO(&ps->output_fds);
    FD_ZERO(&ps->res_output_fds);
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    ps->update_requests.next = NULL;
    ps->update_requests.len = 0;
    ps->curr_upd_req_block = &ps->update_requests;
    erts_smp_atomic_init(&ps->have_update_requests, 0);
#endif
#ifdef ERTS_SMP
    erts_smp_atomic_init(&ps->polled, 0);
    erts_smp_atomic_init(&ps->woken, 0);
    erts_smp_mtx_init(&ps->mtx, "pollset");
    create_wakeup_pipe(ps);
#endif
#if ERTS_POLL_USE_FALLBACK
    grow_fds_status(ps, kp_fd);
    /* Force kernel poll fd into fallback (poll/select) set */
    ps->fds_status[kp_fd].flags
	|= ERTS_POLL_FD_FLG_INFLBCK|ERTS_POLL_FD_FLG_USEFLBCK;
    ERTS_POLL_EXPORT(erts_poll_control)(ps, kp_fd, ERTS_POLL_EV_IN, 1);
#endif
#if ERTS_POLL_USE_KERNEL_POLL
    if (ps->internal_fd_limit <= kp_fd)
	ps->internal_fd_limit = kp_fd + 1;
    ps->kp_fd = kp_fd;
#endif
    erts_smp_atomic_init(&ps->interrupt, 0);
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    handle_update_requests(ps);
#endif
#if ERTS_POLL_USE_FALLBACK
    ps->fallback_used = 0;
#endif
    ps->no_of_user_fds = 0; /* Don't count wakeup pipe and fallback fd */

    erts_smp_spin_lock(&pollsets_lock);
    ps->next = pollsets;
    pollsets = ps;
    erts_smp_spin_unlock(&pollsets_lock);

    return ps;
}

void
ERTS_POLL_EXPORT(erts_poll_destroy_pollset)(ErtsPollSet ps)
{

    if (ps->fds_status)
	erts_free(ERTS_ALC_T_FD_STATUS, (void *) ps->fds_status);

#if ERTS_POLL_USE_EPOLL
    if (ps->kp_fd >= 0)
	close(ps->kp_fd);
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, (void *) ps->res_events);
#elif ERTS_POLL_USE_DEVPOLL
    if (ps->kp_fd >= 0)
	close(ps->kp_fd);
    if (ps->res_events)
	erts_free(ERTS_ALC_T_POLL_RES_EVS, (void *) ps->res_events);
#elif ERTS_POLL_USE_POLL
    if (ps->poll_fds)
	erts_free(ERTS_ALC_T_POLL_FDS, (void *) ps->poll_fds);
#elif ERTS_POLL_USE_SELECT
#endif
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    {
	ErtsPollSetUpdateRequestsBlock *urqbp = ps->update_requests.next;
	while (urqbp) {
	    ErtsPollSetUpdateRequestsBlock *free_urqbp = urqbp;
	    urqbp = urqbp->next;
	    free_update_requests_block(ps, free_urqbp);
	}
    }
#endif
#ifdef ERTS_SMP
    erts_smp_mtx_destroy(&ps->mtx);
    if (ps->wake_fds[0] >= 0)
	close(ps->wake_fds[0]);
    if (ps->wake_fds[1] >= 0)
	close(ps->wake_fds[1]);
#endif

    erts_smp_spin_lock(&pollsets_lock);
    if (ps == pollsets)
	pollsets = pollsets->next;
    else {
	ErtsPollSet prev_ps;
	for (prev_ps = pollsets; ps != prev_ps->next; prev_ps = prev_ps->next);
	ASSERT(ps == prev_ps->next);
	prev_ps->next = ps->next;
    }
    erts_smp_spin_unlock(&pollsets_lock);

    erts_free(ERTS_ALC_T_POLLSET, (void *) ps);
}

/*
 * --- Info ------------------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_info)(ErtsPollSet ps, ErtsPollInfo *pip)
{
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    int pending_updates;
#endif
    Uint size = 0;

    ERTS_POLLSET_LOCK(ps);

    size += sizeof(struct ErtsPollSet_);
    size += ps->fds_status_len*sizeof(ErtsFdStatus);

#if ERTS_POLL_USE_EPOLL
    size += ps->res_events_len*sizeof(struct epoll_event);
#elif ERTS_POLL_USE_DEVPOLL
    size += ps->res_events_len*sizeof(struct pollfd);
#elif ERTS_POLL_USE_KQUEUE
    size += ps->res_events_len*sizeof(struct kevent);
#endif

#if ERTS_POLL_USE_POLL
    size += ps->poll_fds_len*sizeof(struct pollfd);
#elif ERTS_POLL_USE_SELECT
#endif

#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
    {
	ErtsPollSetUpdateRequestsBlock *urqbp = ps->update_requests.next;
	pending_updates = ps->update_requests.len;
	while (urqbp) {
	    size += sizeof(ErtsPollSetUpdateRequestsBlock);
	    pending_updates += urqbp->len;
	}
    }
#endif

    pip->primary = 
#if ERTS_POLL_USE_KQUEUE
	"kqueue"
#elif ERTS_POLL_USE_EPOLL
	"epoll"
#elif ERTS_POLL_USE_DEVPOLL
	"/dev/poll"
#elif ERTS_POLL_USE_POLL
	"poll"
#elif ERTS_POLL_USE_SELECT
	"select"
#endif
	;

    pip->fallback = 
#if !ERTS_POLL_USE_FALLBACK
	NULL
#elif ERTS_POLL_USE_POLL
	"poll"
#elif ERTS_POLL_USE_SELECT
	"select"
#endif
	;

    pip->kernel_poll = 
#if !ERTS_POLL_USE_KERNEL_POLL
	NULL
#elif ERTS_POLL_USE_KQUEUE
	"kqueue"
#elif ERTS_POLL_USE_EPOLL
	"epoll"
#elif ERTS_POLL_USE_DEVPOLL
	"/dev/poll"
#endif
	;

    pip->memory_size = size;

    pip->poll_set_size = ps->no_of_user_fds;
#ifdef ERTS_SMP
    pip->poll_set_size++; /* Wakeup pipe */
#endif

    pip->fallback_poll_set_size =
#if !ERTS_POLL_USE_FALLBACK
	0
#elif ERTS_POLL_USE_POLL
	ps->no_poll_fds
#elif ERTS_POLL_USE_SELECT
	ps->no_select_fds
#endif
	;

#if ERTS_POLL_USE_FALLBACK
    /* If only kp_fd is in fallback poll set we don't use fallback... */
    if (pip->fallback_poll_set_size == 1)
	pip->fallback_poll_set_size = 0;
    else
	pip->poll_set_size++; /* kp_fd */
#endif

    pip->lazy_updates =
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
	1
#else
	0
#endif
	;

    pip->pending_updates = 
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
	pending_updates
#else
	0
#endif
	;

    pip->batch_updates = 
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
	1
#else
	0
#endif
	;

    pip->concurrent_updates =
#if ERTS_POLL_USE_CONCURRENT_UPDATE
	1
#else
	0
#endif
	;

    pip->max_fds = max_fds;

    ERTS_POLLSET_UNLOCK(ps);

}

/*
 * Fatal error...
 */

static void
fatal_error(char *format, ...)
{
    va_list ap;

    if (ERTS_IS_CRASH_DUMPING || erts_got_sigusr1) {
	/*
	 * Crash dump writing and reception of sigusr1 (which will
	 * result in a crash dump) closes all file descriptors. This
	 * typically results in a fatal error for erts_poll() (wakeup
	 * pipes and kernel poll fds are closed).
	 *
	 * We ignore the error and let the crash dump writing continue...
	 */
	return;
    }
    va_start(ap, format);
    erts_vfprintf(stderr, format, ap);
    va_end(ap);
    abort();
}
 
/*
 * --- Debug -----------------------------------------------------------------
 */

void
ERTS_POLL_EXPORT(erts_poll_get_selected_events)(ErtsPollSet ps,
						ErtsPollEvents ev[],
						int len)
{
    int fd;
    ERTS_POLLSET_LOCK(ps);
    for (fd = 0; fd < len; fd++) {
	if (fd >= ps->fds_status_len)
	    ev[fd] = 0;
	else {
	    ev[fd] = ps->fds_status[fd].events;
#ifdef ERTS_SMP
	    if (fd == ps->wake_fds[0] || fd == ps->wake_fds[1])
		ev[fd] |= ERTS_POLL_EV_NVAL;
#endif
#if ERTS_POLL_USE_KERNEL_POLL
	    if (fd == ps->kp_fd)
		ev[fd] |= ERTS_POLL_EV_NVAL;
#endif
	}
    }
    ERTS_POLLSET_UNLOCK(ps);

}

#if ERTS_POLL_USE_DEVPOLL && defined(HARD_DEBUG)

static void
check_poll_status(ErtsPollSet ps)
{
    int i;
    for (i = 0; i < ps->fds_status_len; i++) {
	int ires;
	struct pollfd dp_fd;
	short events = ERTS_POLL_EV_E2N(ps->fds_status[i].events);
	
	dp_fd.fd = i;
	dp_fd.events = (short) 0;
	dp_fd.revents = (short) 0;

	ires = ioctl(ps->kp_fd, DP_ISPOLLED, &dp_fd);

	if (ires == 0) {
	    ASSERT(!events);
	}
	else if (ires == 1) {
	    ASSERT(events);
	    ASSERT(events == dp_fd.revents);
	}
	else {
	    ASSERT(0);
	}
	ASSERT(dp_fd.fd == i);
	ASSERT(ps->fds_status[i].events == ps->fds_status[i].used_events);
    }
}

#endif

#ifdef ERTS_POLL_DEBUG_PRINT
static void
print_misc_debug_info(void)
{
    erts_printf("erts_poll using: %s lazy_updates:%s batch_updates:%s\n",
#if ERTS_POLL_USE_KQUEUE
		"kqueue"
#elif ERTS_POLL_USE_EPOLL
		"epoll"
#elif ERTS_POLL_USE_DEVPOLL
		"/dev/poll"
#endif
#if ERTS_POLL_USE_FALLBACK
		"-"
#endif
#if ERTS_POLL_USE_POLL
		"poll"
#elif ERTS_POLL_USE_SELECT
		"select"
#endif
		,
#if ERTS_POLL_USE_UPDATE_REQUESTS_QUEUE
		"true"
#else
		"false"
#endif
		,
#if ERTS_POLL_USE_BATCH_UPDATE_POLLSET
		"true"
#else
		"false"
#endif
		);

    erts_printf("ERTS_POLL_EV_IN=0x%x\n"
		"ERTS_POLL_EV_OUT=0x%x\n"
		"ERTS_POLL_EV_NVAL=0x%x\n"
		"ERTS_POLL_EV_ERR=0x%x\n",
		ERTS_POLL_EV_IN,
		ERTS_POLL_EV_OUT,
		ERTS_POLL_EV_NVAL,
		ERTS_POLL_EV_ERR);

#ifdef FD_SETSIZE
    erts_printf("FD_SETSIZE=%d\n", FD_SETSIZE);
#endif
}
    
#endif
