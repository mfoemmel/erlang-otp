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

#ifdef ISC32
#define _POSIX_SOURCE
#define _XOPEN_SOURCE
#endif

#include <sys/times.h>		/* ! */
#include <signal.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <termios.h>
#include <ctype.h>
#include <sys/utsname.h>

#if !defined(USE_SELECT)

#  ifdef HAVE_POLL_H
#    include <poll.h>
#  endif
#  ifdef HAVE_SYS_STROPTS_H
#    include <sys/stropts.h>	/* some keep INFTIM here */
#  endif

#  ifdef USE_KERNEL_POLL
#    ifdef HAVE_SYS_EVENT_H
#      include <sys/event.h>
#      define USE_KQUEUE
#    endif
#    ifdef HAVE_SYS_DEVPOLL_H
#      define USE_DEVPOLL
#      include <sys/devpoll.h>
#    endif
#    ifdef HAVE_LINUX_KPOLL_H
#      define USE_DEVPOLL
#      include <asm/page.h>
#      include <sys/mman.h>
#      include <sys/ioctl.h>
#      ifndef POLLREMOVE
#        define POLLREMOVE 0x1000 /* some day it will make it to bits/poll.h ;-) */
#      endif
#      include <linux/kpoll.h>
#    endif
#    ifdef USE_DEVPOLL /* can only use one of them ... */
#      ifdef USE_KQUEUE
#        undef USE_KQUEUE
#      endif
#    endif
#  endif /* USE_KERNEL_POLL */
#endif /* !USE_SELECT */

#ifdef ISC32
#include <sys/bsdtypes.h>
#endif

#define NEED_CHILD_SETUP_DEFINES
#define WANT_NONBLOCKING    /* must define this to pull in defs from sys.h */
#include "sys.h"

#ifdef SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef NO_SYSCONF
#  ifdef USE_SELECT
#    include <sys/param.h>
#    define MAX_FILES()		NOFILE
#  else
#    include <limits.h>
#    define MAX_FILES()		OPEN_MAX
#  endif
#  define TICKS_PER_SEC()	HZ
#else
#define MAX_FILES()	sysconf(_SC_OPEN_MAX)
#define TICKS_PER_SEC()	sysconf(_SC_CLK_TCK)
#endif

#ifdef HAVE_GETHRVTIME
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/signal.h>
#  include <sys/fault.h>
#  include <sys/syscall.h>
#  include <sys/procfs.h>
#  include <fcntl.h>
#endif

#ifdef USE_THREADS
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

extern char **environ;

#define MAX_VSIZE 16		/* Max number of entries allowed in an I/O
				 * vector sock_sendv().
				 */

/*
 * Don't need global.h, but bif_table.h (included by bif.h),
 * won't compile otherwise
 */
#include "global.h" 
#include "bif.h"

#include "erl_sys_driver.h"

#ifndef DISABLE_VFORK
#define DISABLE_VFORK 0
#endif

#ifdef USE_THREADS
#  ifdef ENABLE_CHILD_WAITER_THREAD
#    define CHLDWTHR ENABLE_CHILD_WAITER_THREAD
#  else
#    define CHLDWTHR 0
#  endif
#else
#  define CHLDWTHR 0
#endif
/*
 * [OTP-3906]
 * Solaris signal management gets confused when threads are used and a
 * lot of child processes dies. The confusion results in that SIGCHLD
 * signals aren't delivered to the emulator which in turn results in
 * a lot of defunct processes in the system.
 *
 * The problem seems to appear when a signal is frequently
 * blocked/unblocked at the same time as the signal is frequently
 * propagated. The child waiter thread is a workaround for this problem.
 * The SIGCHLD signal is always blocked (in all threads), and the child
 * waiter thread fetches the signal by a call to sigwait(). See
 * child_waiter().
 */

EXTERN_FUNCTION(void, input_ready, (int, int));
EXTERN_FUNCTION(void, output_ready, (int, int));
EXTERN_FUNCTION(int, check_async_ready, (_VOID_));
EXTERN_FUNCTION(int, driver_interrupt, (int, int));
EXTERN_FUNCTION(void, increment_time, (int));
EXTERN_FUNCTION(int, next_time, (_VOID_));
int send_error_to_logger(Eterm);
EXTERN_FUNCTION(void, do_break, (_VOID_));

EXTERN_FUNCTION(void, erl_sys_args, (int*, char**));

/* The following two defs should probably be moved somewhere else */

extern void erts_sys_init_float(void);

extern void erl_crash_dump(char* file, int line, char* fmt, ...);

#ifdef USE_SELECT

#define NULLFDS ((fd_set *) 0)

static fd_set input_fds;
static fd_set output_fds;
static fd_set read_fds;  /* Gotta be global */
static fd_set write_fds;

#else

struct readyfd {
    int    iport;
    int    oport;
    struct pollfd pfd;
    ErlDrvEventData event_data;
};

static struct pollfd*  poll_fds;      /* Allocated at startup */
static struct readyfd* ready_fds;     /* Collect after poll */
static int             nof_ready_fds; /* Number of fds after poll */

#ifdef USE_KERNEL_POLL
static int use_kernel_poll = 0;
#endif

#ifdef USE_DEVPOLL

static int             dev_poll_fd;   /* fd for /dev/poll */
#ifdef HAVE_LINUX_KPOLL_H
static char *          dev_poll_map;  /* mmap'ed area from kernel /dev/kpoll */
static struct k_poll   dev_poll;      /* control block for /dev/kpoll */
static int max_poll_idx;              /* highest non /dev/kpoll fd */

static void kpoll_enable();
#else
static struct dvpoll   dev_poll;      /* control block for /dev/poll */
#endif /* !HAVE_LINUX_KPOLL_H */
static struct pollfd*  dev_poll_rfds = NULL; /* Allocated at startup */

static void devpoll_init(void);
static void devpoll_update_pix(int pix);
#ifdef HAVE_SYS_DEVPOLL_H
static void devpoll_clear_pix(int pix);
#endif /* HAVE_SYS_DEVPOLL_H */

#endif /* !USE_DEVPOLL */

#ifdef USE_KQUEUE

static int              kqueue_fd;
static struct kevent    *kqueue_res;
static int              max_poll_idx;   /* highest non kqueue fd */

static void kqueue_init();
static void kqueue_enable();
static void kqueue_update_pix(int pix, int old_events);

#define ERL_POLL_READY_ENTRIES  2

#endif

#endif

#ifndef ERL_POLL_READY_ENTRIES
#define ERL_POLL_READY_ENTRIES  1
#endif

static int max_fd;

#define DIR_SEPARATOR_CHAR    '/'

#if defined(DEBUG)
#define ERL_BUILD_TYPE_MARKER ".debug"
#elif defined(PURIFY)
#define ERL_BUILD_TYPE_MARKER ".purify"
#elif defined(QUANTIFY)
#define ERL_BUILD_TYPE_MARKER ".quantify"
#else /* opt */
#define ERL_BUILD_TYPE_MARKER
#endif

#define CHILD_SETUP_PROG_NAME	"child_setup" ERL_BUILD_TYPE_MARKER
#if !DISABLE_VFORK
static char *child_setup_prog;
#endif

#ifdef DEBUG
static int debug_log = 0;
#endif

#if CHLDWTHR
static ethr_tid child_waiter_tid;
#else
static volatile int children_died;
#endif


static struct fd_data {
    int   inport;
    int   outport;
#if !defined(USE_SELECT)
    ErlDrvEventData event_data;
    int   pix;       /* index in poll_fds array */
#endif
    char  pbuf[4];   /* hold partial packet bytes */
    int   psz;       /* size of pbuf */
    char  *buf;
    char  *cpos;
    int   sz;
    int   remain;  /* for input on fd */
} *fd_data;			/* indexed by fd */

/* static FUNCTION(int, write_fill, (int, char*, int)); unused? */
static FUNCTION(void, check_io, (int));
static FUNCTION(void, note_child_death, (int, int));

#if CHLDWTHR
static FUNCTION(void *, child_waiter, (void *));
#endif

/********************* General functions ****************************/

/* This is used by both the drivers and general I/O, must be set early */
static int max_files;

/* 
 * a few variables used by the break handler 
 */
static volatile int break_requested = 0;
/* set early so the break handler has access to initial mode */
static struct termios initial_tty_mode;
/* assume yes initially, ttsl_init will clear it */
int using_oldshell = 1; 

/*
 * reset the terminal to the original settings on exit
 */
void sys_tty_reset(void)
{
  if (using_oldshell) {
    SET_BLOCKING(0);
  }
  else if (isatty(0)) {
    tcsetattr(0,TCSANOW,&initial_tty_mode);
  }
}

#if defined(USE_THREADS) && defined(ETHR_HAVE_ETHR_SIG_FUNCS)
/*
 * Child thread inherits parents signal mask at creation. In order to
 * guarantee that the main thread will receive all SIGINT, SIGCHLD, and
 * SIGUSR1 signals sent to the process, we block these signals in the
 * parent thread when creating a new thread.
 */

static sigset_t thr_create_sigmask;

/*
 * thr_create_prepare() is called in parent thread before thread creation.
 * Returned value is passed as argument to thr_create_cleanup().
 */
static void *
thr_create_prepare(void)
{
    sigset_t *saved_sigmask;
    saved_sigmask = (sigset_t *) erts_alloc(ERTS_ALC_T_TMP, sizeof(sigset_t));
    erts_thr_sigmask(SIG_BLOCK, &thr_create_sigmask, saved_sigmask);
    return (void *) saved_sigmask;
}


/* thr_create_cleanup() is called in parent thread after thread creation. */
static void
thr_create_cleanup(void *saved_sigmask)
{
    /* Restore signalmask... */
    erts_thr_sigmask(SIG_SETMASK, (sigset_t *) saved_sigmask, NULL);
    erts_free(ERTS_ALC_T_TMP, saved_sigmask);
}

#endif

void
erts_sys_pre_init(void)
{
#ifdef USE_THREADS
    ethr_init_data *eidp = NULL;
#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
    ethr_init_data eid = {thr_create_prepare,	/* Before creation in parent */
			  thr_create_cleanup,	/* After creation in parent */
			  NULL			/* After creation in child */};

    eidp = &eid;

    sigemptyset(&thr_create_sigmask);
    sigaddset(&thr_create_sigmask, SIGINT);   /* block interrupt */
    sigaddset(&thr_create_sigmask, SIGCHLD);  /* block child signals */
    sigaddset(&thr_create_sigmask, SIGUSR1);  /* block user defined signal */
#endif

    erts_thr_init(eidp);
#endif
}

void
erl_sys_init(void)
{
#if !DISABLE_VFORK
    char *bindir;
    Uint csp_path_sz;

    bindir = getenv("BINDIR");
    if (!bindir)
        erl_exit(-1, "Environment variable BINDIR is not set\n");
    if (bindir[0] != DIR_SEPARATOR_CHAR)
	erl_exit(-1,
		 "Environment variable BINDIR does not contain an"
		 " absolute path\n");
    csp_path_sz = (strlen(bindir)
		   + 1 /* DIR_SEPARATOR_CHAR */
		   + sizeof(CHILD_SETUP_PROG_NAME)
		   + 1);
    child_setup_prog = erts_alloc(ERTS_ALC_T_CS_PROG_PATH, csp_path_sz);
    erts_sys_misc_mem_sz += csp_path_sz;
    sprintf(child_setup_prog,
            "%s%c%s",
            bindir,
            DIR_SEPARATOR_CHAR,
            CHILD_SETUP_PROG_NAME);
#endif

#ifdef USE_SETLINEBUF
    setlinebuf(stdout);
#else
    setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);
#endif

    if ((max_files = MAX_FILES()) < 0)
	erl_exit(1, "Can't get no. of available file descriptors\n");

    /* Note: this is if MAX_FILES() differ from FD_SETSIZE which it */
    /* does by deafult on e.g. BSDI  */
#if defined(USE_SELECT) && defined(FD_SETSIZE)
    if (max_files > FD_SETSIZE) 
	max_files = FD_SETSIZE;
#endif

    erts_sys_init_float();

    /* we save this so the break handler can set and reset it properly */
    /* also so that we can reset on exit (break handler or not) */
    if (isatty(0)) {
	tcgetattr(0,&initial_tty_mode);
    }
}

/* signal handling */

#ifdef SIG_SIGSET		/* Old SysV */
RETSIGTYPE (*sys_sigset(sig, func))()
int sig;
RETSIGTYPE (*func)();
{
    return(sigset(sig, func));
}
void sys_sigblock(int sig)
{
    sighold(sig);
}
void sys_sigrelease(int sig)
{
    sigrelse(sig);
}
#else /* !SIG_SIGSET */
#ifdef SIG_SIGNAL		/* Old BSD */
RETSIGTYPE (*sys_sigset(sig, func))(int, int)
int sig;
RETSIGTYPE (*func)();
{
    return(signal(sig, func));
}
sys_sigblock(int sig)
{
    sigblock(sig);
}
sys_sigrelease(int sig)
{
    sigsetmask(sigblock(0) & ~sigmask(sig));
}
#else /* !SIG_SIGNAL */	/* The True Way - POSIX!:-) */
RETSIGTYPE (*sys_sigset(int sig, RETSIGTYPE (*func)(int)))(int)
{
    struct sigaction act, oact;

    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    act.sa_handler = func;
    sigaction(sig, &act, &oact);
    return(oact.sa_handler);
}

#ifdef USE_THREADS
#undef  sigprocmask
#define sigprocmask erts_thr_sigmask
#endif

void sys_sigblock(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_BLOCK, &mask, (sigset_t *)NULL);
}

void sys_sigrelease(int sig)
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_UNBLOCK, &mask, (sigset_t *)NULL);
}
#endif /* !SIG_SIGNAL */
#endif /* !SIG_SIGSET */

#if (0) /* not used? -- gordon */
static void (*break_func)();
static RETSIGTYPE break_handler(int sig)
{
#ifdef QNX
    /* Turn off SIGCHLD during break processing */
    sys_sigblock(SIGCHLD);
#endif
    (*break_func)();
#ifdef QNX
    sys_sigrelease(SIGCHLD);
#endif
}
#endif /* 0 */

/* set up signal handlers for break and quit */
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE request_break(void)
#else
static RETSIGTYPE request_break(int signum)
#endif
{
  /* just set a flag - checked for and handled 
   * in main thread (not signal handler).
   * see check_io() 
   */
#ifdef DEBUG
  fprintf(stderr,"break!\n");
#endif
  if (break_requested > 0)
     erl_exit(0, "");

  break_requested = 1;

}

#ifdef ETHR_UNUSABLE_SIGUSRX
#warning "Unusable SIGUSR1 & SIGUSR2. Disabling use of these signals"
#endif

#ifndef ETHR_UNUSABLE_SIGUSRX

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal1(void)
#else
static RETSIGTYPE user_signal1(int signum)
#endif
{
   /* We do this at interrupt level, since the main reason for
      wanting to generate a crash dump in this way is that the emulator
      is hung somewhere, so it won't be able to poll any flag we set here.
      */

   erl_exit(1, "Received SIGUSR1\n");
}

#ifdef QUANTIFY
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE user_signal2(void)
#else
static RETSIGTYPE user_signal2(int signum)
#endif
{
   quantify_save_data();
}
#endif

#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */

#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE do_quit(void)
#else
static RETSIGTYPE do_quit(int signum)
#endif
{
    halt_0(0);
}

void erts_set_ignore_break(void) {
    sys_sigset(SIGINT,  SIG_IGN);
    sys_sigset(SIGQUIT, SIG_IGN);
    sys_sigset(SIGTSTP, SIG_IGN);
}

void init_break_handler(void)
{
   sys_sigset(SIGINT, request_break);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigset(SIGUSR1, user_signal1);
#ifdef QUANTIFY
   sys_sigset(SIGUSR2, user_signal2);
#endif
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
   sys_sigset(SIGQUIT, do_quit);
}

int sys_max_files(void)
{
   return(max_files);
}

static void block_signals(void)
{
#if !CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif
   sys_sigblock(SIGINT);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigblock(SIGUSR1);
#endif
}

static void unblock_signals(void)
{
    /* Update erl_child_setup.c if changed */
#if !CHLDWTHR
   sys_sigrelease(SIGCHLD);
#endif
   sys_sigrelease(SIGINT);
#ifndef ETHR_UNUSABLE_SIGUSRX
   sys_sigrelease(SIGUSR1);
#endif /* #ifndef ETHR_UNUSABLE_SIGUSRX */
}

/******************* Routines for time measurement *********************/

int erts_ticks_per_sec = 0; /* Will be SYS_CLK_TCK in erl_unix_sys.h */
int erts_ticks_per_sec_wrap = 0; /* Will be SYS_CLK_TCK_WRAP */
static int ticks_bsr = 0; /* Shift wrapped tick value this much to the right */

/* 
 * init timers, chose a tick length, and return it.
 * Unix is priviliged when it comes to time, as erl_time_sup.c 
 * does almost everything. Other platforms have to
 * emulate Unix in this sense.
 */
int sys_init_time(void)
{
    /* 
     * This (erts_ticks_per_sec) is only for times() (CLK_TCK), 
     * the resolution is always one millisecond..
     */
    if ((erts_ticks_per_sec = TICKS_PER_SEC()) < 0)
	erl_exit(1, "Can't get clock ticks/sec\n");
    if (erts_ticks_per_sec >= 1000) {
	/* Workaround for beta linux kernels, need to be done in runtime
	   to make erlang run on both 2.4 and 2.5 kernels. In the future, 
	   the kernel ticks might as 
	   well be used as a high res timer instead, but that's for when the 
	   majority uses kernels with HZ == 1024 */
	ticks_bsr = 3;
    } else {
	ticks_bsr = 0;
    }
    erts_ticks_per_sec_wrap = (erts_ticks_per_sec >> ticks_bsr);
    return SYS_CLOCK_RESOLUTION;
}

clock_t sys_times_wrap(void)
{
    SysTimes dummy;
    clock_t result = (sys_times(&dummy) >> ticks_bsr);
    return result;
}

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "unix";

static int
get_number(char **str_ptr)
{
    char* s = *str_ptr;		/* Pointer to beginning of string. */
    char* dot;			/* Pointer to dot in string or NULL. */

    if (!isdigit((int) *s))
	return 0;
    if ((dot = strchr(s, '.')) == NULL) {
	*str_ptr = s+strlen(s);
	return atoi(s);
    } else {
	*dot = '\0';
	*str_ptr = dot+1;
	return atoi(s);
    }
}

void
os_flavor(char* namebuf, 	/* Where to return the name. */
	  unsigned size) 	/* Size of name buffer. */
{
    static int called = 0;
    static struct utsname uts;	/* Information about the system. */

    if (!called) {
	char* s;

	(void) uname(&uts);
	called = 1;
	for (s = uts.sysname; *s; s++) {
	    if (isupper((int) *s)) {
		*s = tolower((int) *s);
	    }
	}
    }
    strcpy(namebuf, uts.sysname);
}

void
os_version(pMajor, pMinor, pBuild)
int* pMajor;			/* Pointer to major version. */
int* pMinor;			/* Pointer to minor version. */
int* pBuild;			/* Pointer to build number. */
{
    struct utsname uts;		/* Information about the system. */
    char* release;		/* Pointer to the release string:
				 * X.Y or X.Y.Z.
				 */

    (void) uname(&uts);
    release = uts.release;
    *pMajor = get_number(&release);
    *pMinor = get_number(&release);
    *pBuild = get_number(&release);
}

void init_getenv_state(GETENV_STATE *state)
{
   *state = NULL;
}

char *getenv_string(GETENV_STATE *state0)
{
   char **state = (char **) *state0;
   char *cp;

   if (state == NULL)
      state = environ;

   cp = *state++;
   *state0 = (GETENV_STATE) state;

   return cp;
}

/************************** Port I/O *******************************/



/* I. Common stuff */

#define TMP_BUF_MAX (tmp_buf_size - 1024)
static byte *tmp_buf;
static Uint tmp_buf_size;
int cerr_pos;

/* II. The spawn/fd/vanilla drivers */

/* This data is shared by these drivers - initialized by spawn_init() */
static struct driver_data {
    int port_num, ofd, packet_bytes;
    int report_exit;
    int pid;
    int alive;
    int status;
} *driver_data;			/* indexed by fd */

#if CHLDWTHR
/* chld_stat_mtx is used to protect against concurrent accesses
   of the driver_data fields pid, alive, and status. */
ethr_mutex chld_stat_mtx = ETHR_MUTEX_INITER;
#define CHLD_STAT_LOCK		erts_mtx_lock(&chld_stat_mtx)
#define CHLD_STAT_UNLOCK	erts_mtx_unlock(&chld_stat_mtx)
#else
#define CHLD_STAT_LOCK
#define CHLD_STAT_UNLOCK
#endif

/* Driver interfaces */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static ErlDrvData vanilla_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static void fd_stop(ErlDrvData);
static void stop(ErlDrvData);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void ready_output(ErlDrvData, ErlDrvEvent);
static void output(ErlDrvData, char*, int);
static void outputv(ErlDrvData, ErlIOVec*);


const struct erl_drv_entry spawn_driver_entry = {
    spawn_init,
    spawn_start,
    stop,
    output,
    ready_input,
    ready_output,
    "spawn",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};
const struct erl_drv_entry fd_driver_entry = {
    NULL,
    fd_start,
    fd_stop,
    output,
    ready_input,
    ready_output, 
    "fd",
    NULL,
    NULL,
    NULL,
    NULL,
    outputv,
    NULL
};
const struct erl_drv_entry vanilla_driver_entry = {
    NULL,
    vanilla_start,
    stop,
    output,
    ready_input,
    ready_output,
    "vanilla",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#ifdef USE_THREADS

static int  async_drv_init(void);
static ErlDrvData async_drv_start(ErlDrvPort, char*, SysDriverOpts*);
static void async_drv_stop(ErlDrvData);
static void async_drv_input(ErlDrvData, ErlDrvEvent);

/* INTERNAL use only */

struct erl_drv_entry async_driver_entry = {
    async_drv_init,
    async_drv_start,
    async_drv_stop,
    NULL,
    async_drv_input,
    NULL,
    "async",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

#endif

/* Handle SIGCHLD signals. */
#if (defined(SIG_SIGSET) || defined(SIG_SIGNAL))
static RETSIGTYPE onchld(void)
#else
static RETSIGTYPE onchld(int signum)
#endif
{
#if CHLDWTHR
    ASSERT(0); /* We should *never* catch a SIGCHLD signal */
#else
    children_died = 1;
#endif
}

static int set_driver_data(int port_num,
			   int ifd,
			   int ofd,
			   int packet_bytes,
			   int read_write,
			   int exit_status,
			   int pid)
{
    if (read_write & DO_READ) {
	driver_data[ifd].packet_bytes = packet_bytes;
	driver_data[ifd].port_num = port_num;
	driver_data[ifd].report_exit = exit_status;
	driver_data[ifd].pid = pid;
	driver_data[ifd].alive = 1;
	driver_data[ifd].status = 0;
	if (read_write & DO_WRITE) {
	    driver_data[ifd].ofd = ofd;
	    if (ifd != ofd)
		driver_data[ofd] = driver_data[ifd];  /* structure copy */
	} else {		/* DO_READ only */
	    driver_data[ifd].ofd = -1;
	}
	(void) driver_select(port_num, ifd, DO_READ, 1);
	return(ifd);
    } else {			/* DO_WRITE only */
	driver_data[ofd].packet_bytes = packet_bytes;
	driver_data[ofd].port_num = port_num;
	driver_data[ofd].report_exit = exit_status;
	driver_data[ofd].ofd = ofd;
	driver_data[ofd].pid = pid;
	driver_data[ofd].alive = 1;
	driver_data[ofd].status = 0;
	return(ofd);
    }
}

static int spawn_init()
{
   int i;

   sys_sigset(SIGPIPE, SIG_IGN); /* Ignore - we'll handle the write failure */
   driver_data = (struct driver_data *)
       erts_alloc(ERTS_ALC_T_DRV_TAB, max_files * sizeof(struct driver_data));
   erts_sys_misc_mem_sz += max_files * sizeof(struct driver_data);

   for (i = 0; i < max_files; i++)
      driver_data[i].pid = -1;

#if CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif

   sys_sigset(SIGCHLD, onchld); /* Reap children */

#if CHLDWTHR
   erts_thr_create(&child_waiter_tid, child_waiter, NULL, 0);
#endif

   return 1;
}

static void close_pipes(int ifd[2], int ofd[2], int read_write)
{
    if (read_write & DO_READ) {
	(void) close(ifd[0]);
	(void) close(ifd[1]);
    }
    if (read_write & DO_WRITE) {
	(void) close(ofd[0]);
	(void) close(ofd[1]);
    }
}

static void init_fd_data(int fd, int prt)
{
    fd_data[fd].buf = NULL;
    fd_data[fd].cpos = NULL;
    fd_data[fd].remain = 0;
    fd_data[fd].sz = 0;
    fd_data[fd].psz = 0;
}

static char **build_unix_environment(char *block)
{
    int i;
    int j;
    int len;
    char *cp;
    char **cpp;
    char** old_env;
    
    cp = block;
    len = 0;
    while (*cp != '\0') {
	cp += strlen(cp) + 1;
	len++;
    }
    old_env = environ;
    while (*old_env++ != NULL) {
	len++;
    }
    
    cpp = (char **) erts_alloc_fnf(ERTS_ALC_T_ENVIRONMENT,
				   sizeof(char *) * (len+1));
    if (cpp == NULL) {
	return NULL;
    }

    cp = block;
    len = 0;
    while (*cp != '\0') {
	cpp[len] = cp;
	cp += strlen(cp) + 1;
	len++;
    }
    
    i = len;
    for (old_env = environ; *old_env; old_env++) {
	char* old = *old_env;

	for (j = 0; j < len; j++) {
	    char *s, *t;

	    s = cpp[j];
	    t = old;
	    while (*s == *t && *s != '=') {
		s++, t++;
	    }
	    if (*s == '=' && *t == '=') {
		break;
	    }
	}

	if (j == len) {		/* New version not found */
	    cpp[len++] = old;
	}
    }

    for (j = 0; j < i; j++) {
	if (cpp[j][strlen(cpp[j])-1] == '=') {
	    cpp[j] = cpp[--len];
	}
    }

    cpp[len] = NULL;
    return cpp;
}

/*
  [arndt] In most Unix systems, including Solaris 2.5, 'fork' allocates memory
  in swap space for the child of a 'fork', whereas 'vfork' does not do this.
  The natural call to use here is therefore 'vfork'. Due to a bug in
  'vfork' in Solaris 2.5 (apparently fixed in 2.6), using 'vfork'
  can be dangerous in what seems to be these circumstances:
      If the child code under a vfork sets the signal action to SIG_DFL
      (or SIG_IGN)
      for any signal which was previously set to a signal handler, the
      state of the parent is clobbered, so that the later arrival of
      such a signal yields a sigsegv in the parent. If the signal was
      not set to a signal handler, but ignored, all seems to work.
  If you change the forking code below, beware of this.
 */

static ErlDrvData spawn_start(ErlDrvPort port_num, char* name, SysDriverOpts* opts)
{
    int ifd[2], ofd[2], len, pid, i;
    char *p1, *p2;
    char **volatile new_environ; /* volatile since a vfork() then cannot
				    cause 'new_environ' to be clobbered
				    in the parent process. */
    int saved_errno;
    long res;

    switch (opts->read_write) {
    case DO_READ:
	if (pipe(ifd) < 0)
	    return ERL_DRV_ERROR_ERRNO;
	if (ifd[0] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	ofd[1] = -1;		/* keep purify happy */
	break;
    case DO_WRITE:
	if (pipe(ofd) < 0) return ERL_DRV_ERROR_ERRNO;
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	ifd[0] = -1;		/* keep purify happy */
	break;
    case DO_READ|DO_WRITE:
	if (pipe(ifd) < 0) return ERL_DRV_ERROR_ERRNO;
	errno = EMFILE;		/* default for next two conditions */
	if (ifd[0] >= max_files || pipe(ofd) < 0) {
	    close_pipes(ifd, ofd, DO_READ);
	    return ERL_DRV_ERROR_ERRNO;
	}
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return ERL_DRV_ERROR_ERRNO;
	}
	break;
    default:
	ASSERT(0);
	return ERL_DRV_ERROR_GENERAL;
    }

    /* make the string suitable for giving to "sh" (5 = strlen("exec")) */
    len = strlen(name);
    if (len + 5 >= tmp_buf_size) {
	close_pipes(ifd, ofd, opts->read_write);
	errno = ENAMETOOLONG;
	return ERL_DRV_ERROR_ERRNO;
    }
    /* name == tmp_buf needs overlapping-safe move - just do it always */
    /* should use memmove() but it isn't always available */
    p1 = (char *)&tmp_buf[len + 5];
    p2 = name+len;
    while (p2 >= name)
	*p1-- = *p2--;
    memcpy(tmp_buf, "exec ", 5);

    if (opts->envir == NULL) {
	new_environ = environ;
    } else if ((new_environ = build_unix_environment(opts->envir)) == NULL) {
	errno = ENOMEM;
	return ERL_DRV_ERROR_ERRNO;
    } 

#ifndef QNX

    /* Block child from SIGINT and SIGUSR1. Must be before fork()
       to be safe. */
    block_signals();

    CHLD_STAT_LOCK;

#if !DISABLE_VFORK
    /* See fork/vfork discussion before this function. */
    if (getenv("ERL_NO_VFORK") != NULL) {
#endif

	DEBUGF(("Using fork\n"));
	pid = fork();

	if (pid == 0) {
	    /* The child! Setup child... */

	    /* OBSERVE!
	     * Keep child setup after vfork() (implemented below and in
	     * erl_child_setup.c) up to date if changes are made here.
	     */

	    if (opts->use_stdio) {
		if (opts->read_write & DO_READ) {
		    /* stdout for process */
		    if (dup2(ifd[1], 1) < 0)
			goto child_error;
		    if(opts->redir_stderr)
			/* stderr for process */
			if (dup2(ifd[1], 2) < 0)
			    goto child_error;
		}
		if (opts->read_write & DO_WRITE)
		    /* stdin for process */
		    if (dup2(ofd[0], 0) < 0)
			goto child_error;
	    }
	    else {	/* XXX will fail if ofd[0] == 4 (unlikely..) */
		if (opts->read_write & DO_READ)
		    if (dup2(ifd[1], 4) < 0)
			goto child_error;
		if (opts->read_write & DO_WRITE)
		    if (dup2(ofd[0], 3) < 0)
			goto child_error;
	    }

	    for (i = opts->use_stdio ? 3 : 5; i < max_files; i++)
		(void) close(i);
	    
	    if (opts->wd && chdir(opts->wd) < 0)
		goto child_error;

#if defined(USE_SETPGRP_NOARGS)		/* SysV */
	    (void) setpgrp();
#elif defined(USE_SETPGRP)		/* BSD */
	    (void) setpgrp(0, getpid());
#else					/* POSIX */
	    (void) setsid();
#endif
	    
	    unblock_signals();

	    execle("/bin/sh", "sh", "-c", tmp_buf, (char *) NULL, new_environ);

	child_error:
	    _exit(1);
	}
#if !DISABLE_VFORK
    }
    else { /* Use vfork() */
	char *cs_argv[CS_ARGV_NO_OF_ARGS + 1];
	char fd_close_range[44];                  /* 44 bytes are enough to  */
	char dup2_op[CS_ARGV_NO_OF_DUP2_OPS][44]; /* hold any "%d:%d" string */
                                                  /* on a 64-bit machine.    */

	/* Setup argv[] for the child setup program (implemented in
	   erl_child_setup.c) */
	i = 0;
	if (opts->use_stdio) {
	    if (opts->read_write & DO_READ){
		/* stdout for process */
		sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 1);
		if(opts->redir_stderr)
		    /* stderr for process */
		    sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 2);
	    }
	    if (opts->read_write & DO_WRITE)
		/* stdin for process */
		sprintf(&dup2_op[i++][0], "%d:%d", ofd[0], 0);
	} else {	/* XXX will fail if ofd[0] == 4 (unlikely..) */
	    if (opts->read_write & DO_READ)
		sprintf(&dup2_op[i++][0], "%d:%d", ifd[1], 4);
	    if (opts->read_write & DO_WRITE)
		sprintf(&dup2_op[i++][0], "%d:%d", ofd[0], 3);
	}
	for (; i < CS_ARGV_NO_OF_DUP2_OPS; i++)
	    strcpy(&dup2_op[i][0], "-");
	sprintf(fd_close_range, "%d:%d", opts->use_stdio ? 3 : 5, max_files-1);

	cs_argv[CS_ARGV_PROGNAME_IX] = child_setup_prog;
	cs_argv[CS_ARGV_WD_IX] = opts->wd ? opts->wd : ".";
	cs_argv[CS_ARGV_CMD_IX] = tmp_buf; /* Command */
	cs_argv[CS_ARGV_FD_CR_IX] = fd_close_range;
	for (i = 0; i < CS_ARGV_NO_OF_DUP2_OPS; i++)
	    cs_argv[CS_ARGV_DUP2_OP_IX(i)] = &dup2_op[i][0];
	cs_argv[CS_ARGV_NO_OF_ARGS] = NULL;

	DEBUGF(("Using vfork\n"));
	pid = vfork();

	if (pid == 0) {
	    /* The child! */

	    /* Observe!
	     * OTP-4389: The child setup program (implemented in
	     * erl_child_setup.c) will perform the necessary setup of the
	     * child before it execs to the user program. This because
	     * vfork() only allow an *immediate* execve() or _exit() in the
	     * child.
	     */
	    execve(child_setup_prog, cs_argv, new_environ);
	    _exit(1);
	}
    }
#endif

    if (pid == -1) {
        saved_errno = errno;
        unblock_signals();
        close_pipes(ifd, ofd, opts->read_write);
	errno = saved_errno;
	return ERL_DRV_ERROR_ERRNO;
    }
#else /* QNX */
    if (opts->use_stdio) {
	if (opts->read_write & DO_READ)
	    qnx_spawn_options.iov[1] = ifd[1];  /* stdout for process */
	if (opts->read_write & DO_WRITE)
	    qnx_spawn_options.iov[0] = ofd[0];  /* stdin for process */
	} 
    else {
	if (opts->read_write & DO_READ)
	    qnx_spawn_options.iov[4] = ifd[1];
	if (opts->read_write & DO_WRITE)
	    qnx_spawn_options.iov[3] = ofd[0];
    }
    /* Close fds on exec */
    for (i = 3; i < max_files; i++)
	fcntl(i, F_SETFD, 1);

    qnx_spawn_options.flags = _SPAWN_SETSID;
    if ((pid = spawnl(P_NOWAIT, "/bin/sh", "/bin/sh", "-c", tmp_buf, 
                      (char *) 0)) < 0) {
        reset_qnx_spawn();
	close_pipes(ifd, ofd, opts->read_write);
	return ERL_DRV_ERROR_GENERAL;
    }
    reset_qnx_spawn();
#endif /* QNX */

    if (new_environ != environ)
	erts_free(ERTS_ALC_T_ENVIRONMENT, (void *) new_environ);

    if (opts->read_write & DO_READ) 
	(void) close(ifd[1]);
    if (opts->read_write & DO_WRITE)
	(void) close(ofd[0]);
	
    if (opts->read_write & DO_READ) {
	SET_NONBLOCKING(ifd[0]);
	init_fd_data(ifd[0], port_num);
    }
    if (opts->read_write & DO_WRITE) {
	SET_NONBLOCKING(ofd[1]);
        init_fd_data(ofd[1], port_num);
    }

    res = set_driver_data(port_num, ifd[0], ofd[1], opts->packet_bytes,
			  opts->read_write, opts->exit_status, pid);
    /* Don't unblock SIGCHLD until now, since the call above must
       first complete putting away the info about our new subprocess. */
    unblock_signals();

    /* Don't unlock chld_stat_mtx until now of the same reason as above */
    CHLD_STAT_UNLOCK;

    return (ErlDrvData)res;
}

#ifdef QNX
static reset_qnx_spawn()
{
    int i;

    /* Reset qnx_spawn_options */
    qnx_spawn_options.flags = 0; 
    qnx_spawn_options.iov[0] = 0xff;
    qnx_spawn_options.iov[1] = 0xff;
    qnx_spawn_options.iov[2] = 0xff;
    qnx_spawn_options.iov[3] = 0xff;
}
#endif

static ErlDrvData fd_start(ErlDrvPort port_num, char* name,
			   SysDriverOpts* opts)
{
    ErlDrvData res;

    if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
	((opts->read_write & DO_WRITE) && opts->ofd >= max_files))
	return ERL_DRV_ERROR_GENERAL;

    /*
     * Historical:
     *
     * "Note about nonblocking I/O.
     *
     * At least on Solaris, setting the write end of a TTY to nonblocking,
     * will set the input end to nonblocking as well (and vice-versa).
     * If erl is run in a pipeline like this:  cat | erl
     * the input end of the TTY will be the standard input of cat.
     * And cat is not prepared to handle nonblocking I/O."
     *
     * Actually, the reason for this is not that the tty itself gets set
     * in non-blocking mode, but that the "input end" (cat's stdin) and
     * the "output end" (erlang's stdout) are typically the "same" file
     * descriptor, dup()'ed from a single fd by one of this process'
     * ancestors.
     *
     * The workaround for this problem used to be a rather bad kludge,
     * interposing an extra process ("internal cat") between erlang's
     * stdout and the original stdout, allowing erlang to set its stdout
     * in non-blocking mode without affecting the stdin of the preceding
     * process in the pipeline - and being a kludge, it caused all kinds
     * of weird problems.
     *
     * So, this is the current logic:
     *
     * The only reason to set non-blocking mode on the output fd at all is
     * if it's something that can cause a write() to block, of course,
     * i.e. primarily if it points to a tty, socket, pipe, or fifo. 
     *
     * If we don't set non-blocking mode when we "should" have, and output
     * becomes blocked, the entire runtime system will be suspended - this
     * is normally bad of course, and can happen fairly "easily" - e.g. user
     * hits ^S on tty - but doesn't necessarily happen.
     * 
     * If we do set non-blocking mode when we "shouldn't" have, the runtime
     * system will end up seeing EOF on the input fd (due to the preceding
     * process dying), which typically will cause the entire runtime system
     * to terminate immediately (due to whatever erlang process is seeing
     * the EOF taking it as a signal to halt the system). This is *very* bad.
     * 
     * I.e. we should take a conservative approach, and only set non-
     * blocking mode when we a) need to, and b) are reasonably certain
     * that it won't be a problem. And as in the example above, the problem
     * occurs when input fd and output fd point to different "things".
     *
     * However, determining that they are not just the same "type" of
     * "thing", but actually the same instance of that type of thing, is
     * unreasonably complex in many/most cases.
     *
     * Also, with pipes, sockets, and fifos it's far from obvious that the
     * user *wants* non-blocking output: If you're running erlang inside
     * some complex pipeline, you're probably not running a real-time system
     * that must never stop, but rather *want* it to suspend if the output
     * channel is "full".
     *
     * So, the bottom line: We will only set the output fd non-blocking if
     * it points to a tty, and either a) the input fd also points to a tty,
     * or b) we can make sure that setting the output fd non-blocking
     * doesn't interfere with someone else's input, via a somewhat milder
     * kludge than the above.
     *
     * Also keep in mind that while this code is almost exclusively run as
     * a result of an erlang open_port({fd,0,1}, ...), that isn't the only
     * case - it can be called with any old pre-existing file descriptors,
     * the relations between which (if they're even two) we can only guess
     * at - still, we try our best...
     */

    if (opts->read_write & DO_READ) {
	init_fd_data(opts->ifd, port_num);
    }
    if (opts->read_write & DO_WRITE) {
	init_fd_data(opts->ofd, port_num);

	/* If we don't have a read end, all bets are off - no non-blocking. */
	if (opts->read_write & DO_READ) {

	    if (isatty(opts->ofd)) { /* output fd is a tty:-) */

		if (isatty(opts->ifd)) { /* input fd is also a tty */

		    /* To really do this "right", we should also check that
		       input and output fd point to the *same* tty - but
		       this seems like overkill; ttyname() isn't for free,
		       and this is a very common case - and it's hard to
		       imagine a scenario where setting non-blocking mode
		       here would cause problems - go ahead and do it. */

		    SET_NONBLOCKING(opts->ofd);

		} else {	/* output fd is a tty, input fd isn't */

		    /* This is a "problem case", but also common (see the
		       example above) - i.e. it makes sense to try a bit
		       harder before giving up on non-blocking mode: Try to
		       re-open the tty that the output fd points to, and if
		       successful replace the original one with the "new" fd
		       obtained this way, and set *that* one in non-blocking
		       mode. (Yes, this is a kludge.)

		       However, re-opening the tty may fail in a couple of
		       (unusual) cases:

		       1) The name of the tty (or an equivalent one, i.e.
			  same major/minor number) can't be found, because
			  it actually lives somewhere other than /dev (or
			  wherever ttyname() looks for it), and isn't
			  equivalent to any of those that do live in the
			  "standard" place - this should be *very* unusual.

		       2) Permissions on the tty don't allow us to open it -
			  it's perfectly possible to have an fd open to an
			  object whose permissions wouldn't allow us to open
			  it. This is not as unusual as it sounds, one case
			  is if the user has su'ed to someone else (not
			  root) - we have a read/write fd open to the tty
			  (because it has been inherited all the way down
			  here), but we have neither read nor write
			  permission for the tty.

		       In these cases, we finally give up, and don't set the
		       output fd in non-blocking mode. */

		    char *tty;
		    int nfd;

		    if ((tty = ttyname(opts->ofd)) != NULL &&
			(nfd = open(tty, O_WRONLY)) != -1) {
			dup2(nfd, opts->ofd);
			close(nfd);
			SET_NONBLOCKING(opts->ofd);
		    }
		}
	    }
	}
    }
    CHLD_STAT_LOCK;
    res = (ErlDrvData)(long)set_driver_data(port_num, opts->ifd, opts->ofd,
				      opts->packet_bytes,
				      opts->read_write, 0, -1);
    CHLD_STAT_UNLOCK;
    return res;
}

static void clear_fd_data(int fd) 
{
    if (fd_data[fd].sz > 0) {
	erts_free(ERTS_ALC_T_FD_ENTRY_BUF, (void *) fd_data[fd].buf);
	ASSERT(erts_sys_misc_mem_sz >= fd_data[fd].sz);
	erts_sys_misc_mem_sz -= fd_data[fd].sz;
    }
    fd_data[fd].buf = NULL;
    fd_data[fd].sz = 0;
    fd_data[fd].remain = 0;
    fd_data[fd].cpos = NULL;
    fd_data[fd].psz = 0;
}

static void nbio_stop_fd(int prt, int fd)
{
    driver_select(prt,fd,DO_READ|DO_WRITE,0);
    clear_fd_data(fd);
    SET_BLOCKING(fd);
}

static void fd_stop(ErlDrvData fd)  /* Does not close the fds */
{
    int ofd;
    
    nbio_stop_fd(driver_data[(int)(long)fd].port_num, (int)(long)fd);
    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && ofd != -1) 
	nbio_stop_fd(driver_data[(int)(long)fd].port_num, (int)(long)ofd);
}

static ErlDrvData vanilla_start(ErlDrvPort port_num, char* name,
				SysDriverOpts* opts)
{
    int flags, fd;
    ErlDrvData res;

    flags = (opts->read_write == DO_READ ? O_RDONLY :
	     opts->read_write == DO_WRITE ? O_WRONLY|O_CREAT|O_TRUNC :
	     O_RDWR|O_CREAT);
    if ((fd = open(name, flags, 0666)) < 0)
	return ERL_DRV_ERROR_GENERAL;
    if (fd >= max_files) {
	close(fd);
	return ERL_DRV_ERROR_GENERAL;
    }
    SET_NONBLOCKING(fd);
    init_fd_data(fd, port_num);

    CHLD_STAT_LOCK;
    res = (ErlDrvData)(long)set_driver_data(port_num, fd, fd,
				      opts->packet_bytes,
				      opts->read_write, 0, -1);
    CHLD_STAT_UNLOCK;
    return res;
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static void stop(ErlDrvData fd)
{
    int prt, ofd;

    prt = driver_data[(int)(long)fd].port_num;
    nbio_stop_fd(prt, (int)(long)fd);
    close((int)(long)fd);

    ofd = driver_data[(int)(long)fd].ofd;
    if (ofd != (int)(long)fd && (int)(long)ofd != -1) {
	nbio_stop_fd(prt, ofd);
	(void) close(ofd);
    }

    CHLD_STAT_LOCK;

    /* Mark as unused. Maybe resetting the 'port_num' slot is better? */
    driver_data[(int)(long)fd].pid = -1;

    CHLD_STAT_UNLOCK;
}

static void outputv(ErlDrvData e, ErlIOVec* ev)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    int ofd = driver_data[fd].ofd;
    int n;
    int sz;
    char lb[4];
    char* lbp;
    int len = ev->size;

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    ev->iov[0].iov_base = lbp;
    ev->iov[0].iov_len = pb;
    ev->size += pb;
    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enqv(ix, ev, 0);
	if (sz + ev->size >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	int vsize = ev->vsize > MAX_VSIZE ? MAX_VSIZE : ev->vsize;

	n = writev(ofd, (const void *) (ev->iov), vsize);
	if (n == ev->size)
	    return; /* 0;*/
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1;*/
	    }
	    n = 0;
	}
	driver_enqv(ix, ev, n);  /* n is the skip value */
	driver_select(ix, ofd, DO_WRITE, 1);
    }
    /* return 0;*/
}


static void output(ErlDrvData e, char* buf, int len)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int pb = driver_data[fd].packet_bytes;
    int ofd = driver_data[fd].ofd;
    int n;
    int sz;
    char lb[4];
    char* lbp;
    struct iovec iv[2];

    /* (len > ((unsigned long)-1 >> (4-pb)*8)) */
    if (((pb == 2) && (len > 0xffff)) || (pb == 1 && len > 0xff)) {
	driver_failure_posix(ix, EINVAL);
	return; /* -1; */
    }
    put_int32(len, lb);
    lbp = lb + (4-pb);

    if ((sz = driver_sizeq(ix)) > 0) {
	driver_enq(ix, lbp, pb);
	driver_enq(ix, buf, len);
	if (sz + len + pb >= (1 << 13))
	    set_busy_port(ix, 1);
    }
    else {
	iv[0].iov_base = lbp;
	iv[0].iov_len = pb;  /* should work for pb=0 */
	iv[1].iov_base = buf;
	iv[1].iov_len = len;
	n = writev(ofd, iv, 2);
	if (n == pb+len)
	    return; /* 0; */
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return; /* -1; */
	    }
	    n = 0;
	}
	if (n < pb) {
	    driver_enq(ix, lbp+n, pb-n);
	    driver_enq(ix, buf, len);
	}
	else {
	    n -= pb;
	    driver_enq(ix, buf+n, len-n);
	}
	driver_select(ix, ofd, DO_WRITE, 1);
    }
    return; /* 0; */
}

static int port_inp_failure(int port_num, int ready_fd, int res)
				/* Result: 0 (eof) or -1 (error) */
{
    int err = errno;
    int status;
    int alive;
    
    CHLD_STAT_LOCK;

    alive = driver_data[ready_fd].alive;
    status = driver_data[ready_fd].status;

    CHLD_STAT_UNLOCK;

    ASSERT(res <= 0);
    (void) driver_select(port_num, ready_fd, DO_READ|DO_WRITE, 0); 
    clear_fd_data(ready_fd);
    if (res == 0) {
       if (!alive && driver_data[ready_fd].report_exit) {
	  /* We need not be prepared for stopped/continued processes. */
	  if (WIFSIGNALED(status))
	     status = 128 + WTERMSIG(status);
	  else
	     status = WEXITSTATUS(status);

	  driver_report_exit(driver_data[ready_fd].port_num,
			     status);
       }
       else if (driver_data[ready_fd].report_exit) {
	   /* We have eof and want to report exit status, but the process
	      hasn't exited yet. Select the fd again, so we come back
	      here. */
	   (void) driver_select(port_num, ready_fd, DO_READ|DO_WRITE, 1);
	   return 0;
       }
       driver_failure_eof(port_num);
    } else {
	driver_failure_posix(port_num, err);
    }
    return 0;
}

/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_input(ErlDrvData e, ErlDrvEvent ready_fd)
{
    int fd = (int)(long)e;
    int port_num;
    int packet_bytes;
    int res;
    Uint h;
    char *buf;

    port_num = driver_data[fd].port_num;
    packet_bytes = driver_data[fd].packet_bytes;

    if (packet_bytes == 0) {
	res = read(ready_fd, tmp_buf, tmp_buf_size);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0)
	    port_inp_failure(port_num, ready_fd, res);
	else 
	    driver_output(port_num, (char*)tmp_buf, res);
    }
    else if (fd_data[ready_fd].remain > 0) { /* We try to read the remainder */
	/* space is allocated in buf */
	res = read(ready_fd, fd_data[ready_fd].cpos, 
		   fd_data[ready_fd].remain);
	if (res < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {
	    port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == fd_data[ready_fd].remain) { /* we're done  */
	    driver_output(port_num, fd_data[ready_fd].buf, 
			  fd_data[ready_fd].sz);
	    clear_fd_data(ready_fd);
	}
	else { /*  if (res < fd_data[ready_fd].remain) */
	    fd_data[ready_fd].cpos += res;
	    fd_data[ready_fd].remain -= res;
	}
    }
    else if (fd_data[ready_fd].remain == 0) { /* clean fd */
	/* We make one read attempt and see what happens */
	res = read(ready_fd, tmp_buf, tmp_buf_size);
	if (res < 0) {  
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK))
		port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {     	/* eof */
	    port_inp_failure(port_num, ready_fd, res);
	} 
	else if (res < packet_bytes - fd_data[ready_fd].psz) { 
	    memcpy(fd_data[ready_fd].pbuf+fd_data[ready_fd].psz,
		   tmp_buf, res);
	    fd_data[ready_fd].psz += res;
	}
	else  { /* if (res >= packet_bytes) */
	    unsigned char* cpos = tmp_buf;
	    int bytes_left = res;

	    while (1) {
		int psz = fd_data[ready_fd].psz;
		char* pbp = fd_data[ready_fd].pbuf + psz;

		while(bytes_left && (psz < packet_bytes)) {
		    *pbp++ = *cpos++;
		    bytes_left--;
		    psz++;
		}
		
		if (psz < packet_bytes) {
		    fd_data[ready_fd].psz = psz;
		    break;
		}
		fd_data[ready_fd].psz = 0;

		switch (packet_bytes) {
		case 1: h = get_int8(fd_data[ready_fd].pbuf);  break;
		case 2: h = get_int16(fd_data[ready_fd].pbuf); break;
		case 4: h = get_int32(fd_data[ready_fd].pbuf); break;
		default: ASSERT(0); return; /* -1; */
		}

		if (h <= (bytes_left)) {
		    driver_output(port_num, (char*) cpos, h);
		    cpos += h;
		    bytes_left -= h;
		    continue;
		}
		else {		/* The last message we got was split */
		    buf = erts_alloc_fnf(ERTS_ALC_T_FD_ENTRY_BUF, h);
		    if (!buf) {
			errno = ENOMEM;
			port_inp_failure(port_num, ready_fd, -1);
		    }
		    else {
			erts_sys_misc_mem_sz += h;
			sys_memcpy(buf, cpos, bytes_left);
			fd_data[ready_fd].buf = buf;
			fd_data[ready_fd].sz = h;
			fd_data[ready_fd].remain = h - bytes_left;
			fd_data[ready_fd].cpos = buf + bytes_left;
		    }
		    break;
		}
	    }
	}
    }
}


/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static void ready_output(ErlDrvData e, ErlDrvEvent ready_fd)
{
    int fd = (int)(long)e;
    int ix = driver_data[fd].port_num;
    int n;
    struct iovec* iv;
    int vsize;
    

    if ((iv = (struct iovec*) driver_peekq(ix, &vsize)) == NULL) {
	driver_select(ix, ready_fd, DO_WRITE, 0);
	return; /* 0; */
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
    if ((n = writev(ready_fd, iv, vsize)) > 0) {
	if (driver_deq(ix, n) == 0)
	    set_busy_port(ix, 0);
    }
    else if (n < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return; /* 0; */
	else {
	    int res = errno;
	    driver_select(ix, ready_fd, DO_WRITE, 0);
	    driver_failure_posix(ix, res);
	    return; /* -1; */
	}
    }
    return; /* 0; */
}

#if !defined(USE_SELECT)

/* At least on FreeBSD, we need POLLRDNORM for normal files, not POLLIN. */
/* Whether this is a bug in FreeBSD, I don't know. */
#ifdef POLLRDNORM
#define POLL_INPUT	(POLLIN | POLLRDNORM)
#else
#define POLL_INPUT	POLLIN
#endif

#ifdef USE_KERNEL_POLL
#define DRIVER_SELECT_P static int driver_select_p
#else
#define DRIVER_SELECT_P int driver_select
#endif

/* poll version of driver_select() */

DRIVER_SELECT_P(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{

    int fd = (int)e;
    if ((fd < 0) || (fd >= max_files))
	return -1;

    if (on) {
	if (mode & (DO_READ|DO_WRITE)) {
	    int pix = fd_data[fd].pix;  /* index to poll_fds */

	    if (pix < 0) {  /* add new slot */
		max_fd++;   /* FIXME: panic if max_fds >= max_files */

		pix = max_fd;
		fd_data[fd].pix = pix;
		fd_data[fd].event_data = NULL;
	    }
	    poll_fds[pix].fd = fd;
	    if (mode & DO_READ) {
		fd_data[fd].inport = ix;
		poll_fds[pix].events |= POLL_INPUT;
	    }
	    if (mode & DO_WRITE) {
		fd_data[fd].outport = ix;
		poll_fds[pix].events |= POLLOUT;
	    }
	}
    }
    else {
	int pix = fd_data[fd].pix;  /* index to poll_fds */
	int rix;                    /* index to ready_fds */

	if (pix < 0) 	           /* driver deselected already */
	    return -1;
	/* Find index of this ready fd */
	for (rix = 0; rix < nof_ready_fds; rix++)
	    if (ready_fds[rix].pfd.fd == fd)
		break;
	if (mode & DO_READ) {
	    poll_fds[pix].events &= ~POLL_INPUT;
	    /* Erase POLL_INPUT event from poll result being processed */
	    if ((rix < nof_ready_fds)
		&& (ready_fds[rix].pfd.events & POLL_INPUT)) 
		ready_fds[rix].pfd.revents &= ~POLL_INPUT;
	}
	if (mode & DO_WRITE) {
	    poll_fds[pix].events &= ~POLLOUT;
	    /* Erase POLLOUT event from poll result being processed */
	    if ((rix < nof_ready_fds)
		&& (ready_fds[rix].pfd.events & POLLOUT)) 
		ready_fds[rix].pfd.revents &= ~POLLOUT;
	}
	if (poll_fds[pix].events == 0) {
	    /* Erase all events from poll result being processed */
	    if (rix < nof_ready_fds)
		ready_fds[rix].pfd.revents = 0;

	    /* delete entry by moving the last entry to pix position */
	    /* FIXME: panic if max_fd == -1 */

	    fd_data[fd].pix  = -1;

	    if (pix != max_fd) {
		int rfd = poll_fds[max_fd].fd;
		poll_fds[pix] = poll_fds[max_fd];
		fd_data[rfd].pix = pix;
	    }

	    /* Need to clear at least .events since it is or'ed when
	     * this slot is reused - might as well clear everything.
	     */
	    poll_fds[max_fd].fd      = -1;
	    poll_fds[max_fd].events  = 0;
	    poll_fds[max_fd].revents = 0;
	    
	    max_fd--;
	}
    }
    return 0;
}


#ifdef USE_KERNEL_POLL

/* kernel poll version of driver_select() */

static int driver_select_kp(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{
    int fd = (int)e;
    if ((fd < 0) || (fd >= max_files))
	return -1;

    if (on) {
	if (mode & (DO_READ|DO_WRITE)) {
	    int pix = fd_data[fd].pix;  /* index to poll_fds */
#if defined(USE_DEVPOLL) || defined(USE_KQUEUE)
	    int old_events;
#endif

	    if (pix < 0) {  /* add new slot */
		max_fd++;   /* FIXME: panic if max_fds >= max_files */
		pix = max_fd;
		fd_data[fd].pix = pix;
		/* init the pfd structure */
		poll_fds[pix].fd = fd;
		poll_fds[pix].events = 0;
	    }

#if defined(USE_DEVPOLL) || defined(USE_KQUEUE)
	    old_events = poll_fds[pix].events;
#endif
	    poll_fds[pix].fd = fd;
	    if (mode & DO_READ) {
		fd_data[fd].inport = ix;
		poll_fds[pix].events |= POLL_INPUT;
	    }
	    if (mode & DO_WRITE) {
		fd_data[fd].outport = ix;
		poll_fds[pix].events |= POLLOUT;
	    }

#ifdef USE_DEVPOLL
	    if (poll_fds[pix].events != old_events) 
                devpoll_update_pix(pix);
#endif
#ifdef USE_KQUEUE
	    if (poll_fds[pix].events != old_events) 
                kqueue_update_pix(pix,old_events);
#endif
	}
    }
    else {
	int pix = fd_data[fd].pix;  /* index to poll_fds */
#if defined(USE_DEVPOLL) || defined(USE_KQUEUE)
	int old_events ;
#endif

	int rix = 0;
	int srix[ERL_POLL_READY_ENTRIES];      /* index(es) to ready_fds */
	int srix_idx;

	if (pix < 0) 	           /* driver deselected already */
	    return -1;

	for(srix_idx=0;srix_idx<(sizeof(srix)/sizeof(srix[0]));srix_idx++)
	  srix[srix_idx]=-1;
	srix_idx=0;

#if defined(USE_DEVPOLL) || defined(USE_KQUEUE)
	old_events = poll_fds[pix].events;
#endif
	if (mode & DO_READ) {
	  poll_fds[pix].events &= ~POLL_INPUT;
          fd_data[fd].inport = -1;
        }
	if (mode & DO_WRITE) {
	  poll_fds[pix].events &= ~POLLOUT;
          fd_data[fd].outport = -1;
        }

	/* Find index(es) of this fd in ready_fds */
	for (rix = 0; rix < nof_ready_fds; rix++) {
	  if (ready_fds[rix].pfd.fd == fd) {

	    if (mode & DO_READ) 
	      ready_fds[rix].pfd.revents &= ~POLL_INPUT;

	    if (mode & DO_WRITE) 
	      ready_fds[rix].pfd.revents &= ~POLLOUT;

	    srix[srix_idx] = rix;
	    if (++srix_idx == (sizeof(srix)/sizeof(srix[0]))) break;
	  }
	}
	if (poll_fds[pix].events == 0) {
#ifdef USE_DEVPOLL
	    if ( old_events && (dev_poll_fd != -1) ) {
	       /* Tell /dev/[k]poll that we are not interested any more ... */
	       poll_fds[pix].events = POLLREMOVE;
	       devpoll_update_pix(pix);
	       /* devpoll_update_pix may change the pix */
	       pix = fd_data[fd].pix;
	       poll_fds[pix].events = 0;
	    }
#endif
#ifdef USE_KQUEUE
	    if ( old_events && (kqueue_fd != -1) ) {
                /* Tell kqueue that we are not interested any more ... */
                kqueue_update_pix(pix,old_events);
                /*       devpoll_update_pix may change the pix */
                pix = fd_data[fd].pix;
                poll_fds[pix].events = 0;
	    }
#endif

	    /* Erase all events from poll result being processed */
	    {
                int i;
                for(i=0;i<srix_idx;i++) {
                    if ((srix[i] != -1))
                        ready_fds[srix[i]].pfd.revents = 0;
                }
	    }

	    /* delete entry by moving the last entry to pix position */
	    /* FIXME: panic if max_fd == -1 */

	    fd_data[fd].pix  = -1;

	    if (pix != max_fd) {
		int rfd = poll_fds[max_fd].fd;
		poll_fds[pix] = poll_fds[max_fd];
		fd_data[rfd].pix = pix;
	    }
            
	    /* Need to clear at least .events since it is or'ed when
	     * this slot is reused - might as well clear everything.
             */
	    poll_fds[max_fd].fd      = -1;
	    poll_fds[max_fd].events  = 0;
	    poll_fds[max_fd].revents = 0;
	    
	    max_fd--;
	}
#ifdef USE_DEVPOLL
	else if (mode) {

	    /* If we have removed an event request but not all of them
	       then we have to remove all requests and redo the
	       request. This is because on Solaris the event mask is
	       OR'ed in, i.e. if we earlier requested both POLLIN and
	       POLLOUT and remove POLLOUT nothing will be changed */

#ifdef HAVE_SYS_DEVPOLL_H
	    devpoll_clear_pix(pix);
#endif /* HAVE_SYS_DEVPOLL_H */
	    devpoll_update_pix(pix);
	}
#endif
#ifdef USE_KQUEUE
	else {
	    kqueue_update_pix(pix,old_events);
	}
#endif

    }
    return 0;
}

static int (*driver_select_func)(ErlDrvPort,
				 ErlDrvEvent,
				 int,
				 int) = driver_select_p;

int driver_select(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{
    return (*driver_select_func)(ix, e, mode, on);
}

#endif /* #ifdef USE_KERNEL_POLL */

int driver_event(ErlDrvPort ix, ErlDrvEvent e, ErlDrvEventData event_data) {
    int fd = (int)e;
    if ((fd < 0) || (fd >= max_files))
	return -1;

    if (event_data && event_data->events) {
	int pix = fd_data[fd].pix;  /* index to poll_fds */
	
	if (pix < 0) {  /* add new slot */
	    max_fd++;   /* FIXME: panic if max_fds >= max_files */
	    
	    pix = max_fd;
	    poll_fds[pix].fd = fd;
	    fd_data[fd].pix = pix;
	    fd_data[fd].inport = ix;
	}
	fd_data[fd].event_data = event_data;
	poll_fds[pix].events = event_data->events;
    } else {
	int pix = fd_data[fd].pix;  /* index to poll_fds */

	if (pix < 0) 	           /* driver deselected already */
	    return -1;

	/* delete entry by moving the last entry to pix position */
	/* FIXME: panic if max_fd == -1 */
	
	fd_data[fd].pix  = -1;
	
	if (pix != max_fd) {
	    int rfd = poll_fds[max_fd].fd;
	    poll_fds[pix] = poll_fds[max_fd];
	    fd_data[rfd].pix = pix;
	}
	
	/* Need to clear at least .events since it is or'ed when
	 * this slot is reused - might as well clear everything.
	 */
	poll_fds[max_fd].fd      = -1;
	poll_fds[max_fd].events  = 0;
	poll_fds[max_fd].revents = 0;
	
	max_fd--;
    }
    return 0;
}

#else  /* if defined(USE_SELECT) */

/* Interface function available to driver writers */
int driver_select(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
 {
   int this_port = (int)ix;
   int fd = (int)e;
#if defined(FD_SETSIZE)
    if (fd >= FD_SETSIZE) {
	erl_exit(1,"driver_select called with too large file descriptor %d"
		 "(FD_SETSIZE == %d)",
		 fd,FD_SETSIZE);
    }
#endif
    if (fd >= 0 && fd < max_files) {
	if (on) {
	    if (mode & DO_READ) {
		fd_data[fd].inport = this_port; 
		FD_SET(fd, &input_fds);
	    }
	    if (mode & DO_WRITE) {
		fd_data[fd].outport = this_port; 
		FD_SET(fd, &output_fds);
	    }
	    if ((mode & (DO_READ|DO_WRITE)) && max_fd < fd) max_fd = fd;
	} else {
	    if (mode & DO_READ) {
		FD_CLR(fd, &input_fds);
	    }
	    if (mode & DO_WRITE) {
		FD_CLR(fd, &output_fds);
	    }
	    if ((mode & (DO_READ|DO_WRITE)) && max_fd == fd) {
		while (max_fd >= 0 &&
		       !FD_ISSET(max_fd, &input_fds) &&
		       !FD_ISSET(max_fd, &output_fds))
		    max_fd--;
	    }
	}
	return(0);
    } else
	return(-1);
}

int driver_event(ErlDrvPort ix, ErlDrvEvent e, ErlDrvEventData event_data) {
    return -1;
}

#endif /*  !defined(USE_SELECT) */

/*
** Async opertation support
*/
#ifdef USE_THREADS

static int async_fd[2];

/* called from threads !! */
void sys_async_ready(int fd)
{
    int r;
    r = write(fd, "0", 1);  /* signal main thread fd MUST be async_fd[1] */
    DEBUGF(("sys_async_ready: r = %d\r\n", r));
}

static int async_drv_init(void)
{
    async_fd[0] = -1;
    async_fd[1] = -1;
    return 0;
}

static ErlDrvData async_drv_start(ErlDrvPort port_num,
				  char* name, SysDriverOpts* opts)
{
    if (async_fd[0] != -1)
	return ERL_DRV_ERROR_GENERAL;
    if (pipe(async_fd) < 0)
	return ERL_DRV_ERROR_GENERAL;

    DEBUGF(("async_drv_start: %d\r\n", port_num));

    driver_select(port_num, async_fd[0], DO_READ, 1);

    if (init_async(async_fd[1]) < 0)
	return ERL_DRV_ERROR_GENERAL;
    return (ErlDrvData)port_num;
}

static void async_drv_stop(ErlDrvData e)
{
    int port_num = (int)(long)e;

    DEBUGF(("async_drv_stop: %d\r\n", port_num));

    exit_async();

    driver_select(port_num, async_fd[0], DO_READ, 0);

    close(async_fd[0]);
    close(async_fd[1]);
    async_fd[0] = async_fd[1] = -1;
}


static void async_drv_input(ErlDrvData e, ErlDrvEvent fd)
{
    char buf[1];

    DEBUGF(("async_drv_input\r\n"));
    read((int)fd, buf, 1);     /* fd MUST be async_fd[0] */
    check_async_ready();  /* invoke all async_ready */
}

#endif


/* Lifted out from check_io() */
static void do_break_handling(void)
{
    struct termios temp_mode;
    int saved = 0;
    
    /* during break we revert to initial settings */
    /* this is done differently for oldshell */
    if (using_oldshell) {
      SET_BLOCKING(1);
    }
    else if (isatty(0)) {
      tcgetattr(0,&temp_mode);
      tcsetattr(0,TCSANOW,&initial_tty_mode);
      saved = 1;
    }
    
    /* call the break handling function, reset the flag */
    do_break();
    break_requested = 0;
    fflush(stdout);
    
    /* after break we go back to saved settings */
    if (using_oldshell) {
      SET_NONBLOCKING(1);
    }
    else if (saved) {
      tcsetattr(0,TCSANOW,&temp_mode);
    }
}



/* See if there is any i/o pending. If do_wait is 1 wait for i/o.
   Both are done using "select". NULLTV (ie 0) causes select to wait.
   &poll (ie a pointer to a zero timeval) causes select to poll.
   If select returns unexpectedly we probably have an interrupt.
   In this case we just return. This routine also does the time
   measurement, using time_remaining() and deliver_time().
   
   Note: it is extremley important that this function always calls
   deliver_time() before returning.
*/
#ifdef USE_SELECT
static void check_io(do_wait)
int do_wait;
{
    SysTimeval wait_time;
    SysTimeval *sel_time;
    SysTimeval poll;

    int local_max_fd = max_fd;
    int i;
    fd_set err_set;

    /* choose timeout value for select() */
    if (do_wait) {
	erts_time_remaining(&wait_time);
	sel_time = &wait_time;
    } else if (max_fd == -1) {  /* No need to poll. 
				 * (QNX's select crashes;-) */
	erts_deliver_time(NULL); /* sync the machine's idea of time */
        return;
    } else {			/* poll only */
        poll.tv_sec  = 0;	/* zero time - used for polling */
	poll.tv_usec = 0;	/* Initiate each time, Linux select() may */
	sel_time = &poll;	/* modify it */
    }

    read_fds = input_fds; 
    write_fds = output_fds;
    i = select(max_fd + 1, &read_fds, &write_fds, NULLFDS, sel_time);
    erts_deliver_time(NULL); /* sync the machine's idea of time */

    /* break handling moved here, signal handler just sets flag */
    if (break_requested) 
	do_break_handling();

    if (i <= 0) {
	/* probably an interrupt or poll with no input */

	if ((i == -1) && (errno == EBADF)) {
	    /* check which fd that is bad */
	    for (i = 0; i <= local_max_fd; i++) {
		if (FD_ISSET(i, &input_fds)) {
		    poll.tv_sec  = 0;	/* zero time - used for polling */
		    poll.tv_usec = 0;	/* Initiate each time, 
					 * Linux select() may */
		    sel_time = &poll;	/* modify it */
		    FD_ZERO(&err_set);
		    FD_SET(i, &err_set);
		    if (select(max_fd + 1, &err_set, NULL, NULL, sel_time) == -1){
			/* bad read FD found */
			erl_printf(CBUF, "Bad input_fd in select! "
				   "fd,port,driver,name: %d,%d,%s,%s\n", 
				   i, fd_data[i].inport, 
				   erts_port[fd_data[i].inport].drv_ptr
				   ->driver_name,
				   erts_port[fd_data[i].inport].name);
			send_error_to_logger(NIL);
			/* remove the bad fd */
			FD_CLR(i, &input_fds);
		    }
		}
		if (FD_ISSET(i, &output_fds)) {
		    poll.tv_sec  = 0;	/* zero time - used for polling */
		    poll.tv_usec = 0;	/* Initiate each time, 
					 * Linux select() may */
		    sel_time = &poll;	/* modify it */
		    FD_ZERO(&err_set);
		    FD_SET(i, &err_set);
		    if (select(max_fd + 1, NULL, &err_set, NULL, sel_time) 
			== -1) {
			/* bad write FD found */
			erl_printf(CBUF, "Bad output_fd in select! "
				   "fd,port,driver,name: %d,%d,%s,%s\n", 
				   i, fd_data[i].outport, 
				   erts_port[fd_data[i].outport].drv_ptr->
				   driver_name,
				   erts_port[fd_data[i].outport].name);
			send_error_to_logger(NIL);
			/* remove the bad fd */
			FD_CLR(i, &output_fds);
		    }
		}
	    }
	}
	return;
    }
    /* do the write's first */
    for(i =0; i <= local_max_fd; i++) {
	if (FD_ISSET(i, &write_fds)) /* No need to check output_fds here
				      * since output_ready can deselect
				      * it's input fd brother, but not the
				      * other way around. This because
				      * output_ready is run before
				      * input_ready.
				      */
	    output_ready(fd_data[i].outport, i);
    }
    for (i = 0; i <= local_max_fd; i++) {
	if (FD_ISSET(i, &read_fds) && FD_ISSET(i, &input_fds))
	    input_ready(fd_data[i].inport, i);
    }
}

#else /* poll() implementation of check_io() */


#ifdef USE_KERNEL_POLL
#define CHECK_IO_P static void check_io_p
#else
#define CHECK_IO_P static void check_io
#endif

CHECK_IO_P(int do_wait)
{
    SysTimeval wait_time;
    int r, i;
    int max_fd_plus_one = max_fd + 1;
    int timeout;		/* In milliseconds */
    struct readyfd* rp;
    struct readyfd* qp;

    /* Figure out timeout value */
    if (do_wait) {
	erts_time_remaining(&wait_time);
	timeout = wait_time.tv_sec * 1000 + wait_time.tv_usec / 1000;
    } else if (max_fd == -1) {  /* No need to poll. */
	erts_deliver_time(NULL); /* sync the machine's idea of time */
        return;
    } else {			/* poll only */
	timeout = 0;
    }

    if ((r = poll(poll_fds, max_fd_plus_one, timeout)) > 0) {
	int rr = r;
	/* collect ready fds into the ready_fds stucture,
	 * this makes the calls to input ready/output ready
	 * independant of the poll_fds array 
	 ** (accessed via call to driver_select etc)
	 */
	rp = ready_fds;
	for (i = 0; rr && (i < max_fd_plus_one); i++) {
	    short revents = poll_fds[i].revents;
	    
	    if (revents != 0) {
		int fd = poll_fds[i].fd;
		rp->pfd = poll_fds[i]; /* COPY! */
		rp->iport = fd_data[fd].inport;
		rp->oport = fd_data[fd].outport;
		rp->event_data = fd_data[fd].event_data;
		rp++;
		rr--;
	    }
	}
	nof_ready_fds = r;
    } else {
	nof_ready_fds = 0;
	rp = NULL; /* avoid 'uninitialized' warning */
    }

    erts_deliver_time(NULL); /* sync the machine's idea of time */

    if (break_requested) 
	do_break_handling();

    if (r == 0)     /* timeout */
	return;

    if (r < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return;
	erl_printf(CBUF, "poll() error %d (%s)\n", errno, erl_errno_id(errno));
	send_error_to_logger(NIL);
	return;
    }

    ASSERT(rp);
    ASSERT(nof_ready_fds > 0);

    /* Note: this is *not* the same behaviour as in the select
     *       implementation, where *all* write ready file descriptors
     *       are done first.
     */

    for (qp = ready_fds; qp < rp; qp++) {
	int   fd      = qp->pfd.fd;
	short revents = qp->pfd.revents;

	if (revents && qp->event_data) {
	    qp->event_data->revents = revents;
	    event_ready(qp->iport, fd, qp->event_data);
	}
	else if (revents & (POLL_INPUT|POLLOUT)) {
	    if (revents & POLLOUT)
		output_ready(qp->oport, fd);
	    if (revents & (POLL_INPUT|POLLHUP))
		input_ready(qp->iport, fd);
	}
	else if (revents & (POLLERR|POLLHUP)) {
	    /* let the driver handle the error condition */
	    if (qp->pfd.events & POLL_INPUT)
		input_ready(qp->iport, fd);
	    else
		output_ready(qp->oport, fd);
	}
	else if (revents & POLLNVAL) {
	    if (qp->pfd.events & POLL_INPUT) {
		erl_printf(CBUF, "Bad input fd in poll()! fd,port,driver,name:"
			   " %d,%d,%s,%s\n", fd,
			   qp->iport, 
			   erts_port[qp->iport].drv_ptr->driver_name,
			   erts_port[qp->iport].name);
	    } 
	    else if (qp->pfd.events & POLLOUT) {
		erl_printf(CBUF,
			   "Bad output fd in poll()! fd,port,driver,name:"
			   " %d,%d,%s,%s\n", fd,
			   qp->oport,
			   erts_port[qp->oport].drv_ptr->driver_name,
			   erts_port[qp->oport].name);
	    } 
	    else {
		erl_printf(CBUF, "Bad fd in poll(), %d!\n", fd);
	    }
	    send_error_to_logger(NIL);

	    /* unmap entry */
	    if (qp->pfd.events & POLL_INPUT)
		driver_select(qp->iport, fd, DO_READ|DO_WRITE, 0);
	    if (qp->pfd.events & POLLOUT) {
		if (!(qp->pfd.events & POLL_INPUT) || (qp->iport != qp->oport))
		    driver_select(qp->oport, fd, DO_READ|DO_WRITE, 0);
	    }
	}
    }
}


#ifdef USE_KERNEL_POLL
/* kernel poll implementation of check_io() */

static void check_io_kp(int do_wait)
{
    SysTimeval wait_time;
    int r, i;
#ifndef USE_KQUEUE
    int max_fd_plus_one = max_fd + 1;
#endif
    int timeout;		/* In milliseconds */
    struct readyfd* rp;
    struct readyfd* qp;

    /* Figure out timeout value */
    if (do_wait) {
	erts_time_remaining(&wait_time);
	timeout = wait_time.tv_sec * 1000 + wait_time.tv_usec / 1000;
    } else if (max_fd == -1) {  /* No need to poll. */
	erts_deliver_time(NULL); /* sync the machine's idea of time */
        return;
    } else {			/* poll only */
	timeout = 0;
    }

#ifdef USE_DEVPOLL
    if ( dev_poll_fd != -1 ) {
#ifdef HAVE_LINUX_KPOLL_H
      int do_event_poll;

      do_event_poll = 0;
      if ((r = poll(poll_fds, (max_poll_idx+1), timeout)) > 0 ) {
#else
      dev_poll.dp_timeout = timeout;
      dev_poll.dp_nfds    = max_fd_plus_one;
      dev_poll.dp_fds     = dev_poll_rfds;
      if ((r = ioctl(dev_poll_fd, DP_POLL, &dev_poll)) > 0 ) {
#endif
	/* collect ready fds into the ready_fds stucture,
	 * this makes the calls to input ready/output ready
	 * independant of the poll_fds array 
	 ** (accessed via call to driver_select etc)
	 */
	int rr;
	int vr = 0; /* valid */
#ifdef HAVE_LINUX_KPOLL_H
	dev_poll_rfds = poll_fds;
#endif
	rp = ready_fds;
	rr = r;
#ifdef HAVE_LINUX_KPOLL_H
	r = max_poll_idx+1;
#endif
	for (i = 0; rr && (i < r); i++) {
	    short revents = dev_poll_rfds[i].revents;

	    if (revents != 0) {
#if HAVE_LINUX_KPOLL_H
	        if (dev_poll_rfds[i].fd != dev_poll_fd) {
#endif
		    int fd = dev_poll_rfds[i].fd;

		    rp->pfd = dev_poll_rfds[i]; /* COPY! */
		    rp->iport = fd_data[fd].inport;
		    rp->oport = fd_data[fd].outport;
		    rp++;
		    rr--;
		    vr++;
#if HAVE_LINUX_KPOLL_H
	        } else {
	            do_event_poll = 1;
		}
#endif
	    }

	}
	nof_ready_fds = vr;

#if HAVE_LINUX_KPOLL_H
	if ( do_event_poll ) {
	  /* Now do the fast poll */
	  dev_poll.kp_timeout = 0;
	  dev_poll.kp_resoff  = 0;
	  if ((r = ioctl(dev_poll_fd, KP_POLL, &dev_poll)) > 0 ) {
	    dev_poll_rfds = (struct pollfd *)(dev_poll_map + dev_poll.kp_resoff);

	    for (i = 0; (i < r); i++) {
	      short revents = dev_poll_rfds[i].revents;

	      if (revents != 0) {
	        int fd = dev_poll_rfds[i].fd;
		rp->pfd = dev_poll_rfds[i]; /* COPY! */
		rp->iport = fd_data[fd].inport;
		rp->oport = fd_data[fd].outport;
		rp++;
	      } 

	    }
	    nof_ready_fds += r;
	  }
	}
#endif

      } else {
	nof_ready_fds = 0;
	rp = NULL; /* avoid 'uninitialized' warning */
      }
    } else {
#endif
#ifdef USE_KQUEUE
    if ((r = poll(poll_fds, max_poll_idx+1, timeout)) > 0) {
#else
    if ((r = poll(poll_fds, max_fd_plus_one, timeout)) > 0) {
#endif
	int rr = r;
	int vr = 0;
#ifdef USE_KQUEUE
	int do_kevent = 0;
#endif
	/* collect ready fds into the ready_fds stucture,
	 * this makes the calls to input ready/output ready
	 * independant of the poll_fds array 
	 ** (accessed via call to driver_select etc)
	 */
	rp = ready_fds;
#ifdef USE_KQUEUE
	for (i = 0; rr && (i < (max_poll_idx+1)); i++) {
#else
	for (i = 0; rr && (i < max_fd_plus_one); i++) {
#endif
	    short revents = poll_fds[i].revents;
	    
	    if (revents != 0) {

#ifdef USE_KQUEUE
            if ((kqueue_fd != -1) && (poll_fds[i].fd == kqueue_fd)) {
                do_kevent=1;
            } else {
#endif
		int fd = poll_fds[i].fd;
		rp->pfd = poll_fds[i]; /* COPY! */
		rp->iport = fd_data[fd].inport;
		rp->oport = fd_data[fd].outport;
		rp++;
		rr--;
		vr++;
#ifdef USE_KQUEUE
	    }
#endif
	    }
	}
#ifdef USE_KQUEUE
	if ( do_kevent ) {
            int res;
            struct timespec ts;

            memset(&ts,(char)0,sizeof(ts));

            if ( (res=kevent(kqueue_fd,NULL,0,kqueue_res,2*max_files,&ts)) < 0 ) {
                erl_exit(1, "%s:%d kevent call failed. errno = %d\n",__FILE__,__LINE__,errno);
            } else {
                int i;
                for(i=0;i<res;i++) {
                    int pix;
                    int fd;
                    struct kevent *kep = &kqueue_res[i];
                    fd = kep->ident;
                    pix = fd_data[fd].pix;
                    
                    /* Now     we should copy this into the result fd array               */
                    /* Please note t  he kqueue version may generate two entries per fd */
                    /* This is handled in dr  iver_select for cleanup                   */
                    
                    rp->pfd = poll_fds[pix]; /* COPY! */
                    if ( kep->filter == EVFILT_READ ) {
                        rp->pfd.revents |= POLL_INPUT;
                    } else if ( kep->filter == EVFILT_WRITE ) {
                        rp->pfd.revents |= POLLOUT;
                        if ( kep->flags & EV_EOF )
                            rp->pfd.revents |= POLLHUP;
                        if ( kep->flags & EV_ERROR )
                            rp->pfd.revents |= POLLERR;
                    } 
                    if ( kep->flags & EV_EOF )
                        rp->pfd.revents |= POLLHUP;
                    if ( kep->flags & EV_ERROR )
                        rp->pfd.revents |= POLLERR;
                    
                    rp->iport = fd_data[fd].inport;
                    rp->oport = fd_data[fd].outport;
                    rp++;
                    vr++;
                }
            }
	}
#endif
	nof_ready_fds = vr;
    } else {
	nof_ready_fds = 0;
	rp = NULL; /* avoid 'uninitialized' warning */
    }
#ifdef USE_DEVPOLL
    } 
#endif

    erts_deliver_time(NULL); /* sync the machine's idea of time */

    if (break_requested) 
	do_break_handling();

    if (r == 0)     /* timeout */
	return;

    if (r < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return;
	erl_printf(CBUF, "poll() error %d (%s)\n", errno, erl_errno_id(errno));
	send_error_to_logger(NIL);
	return;
    }

    ASSERT(rp);
    ASSERT(nof_ready_fds > 0);

    /* Note: this is *not* the same behaviour as in the select
     *       implementation, where *all* write ready file descriptors
     *       are done first.
     */

    for (qp = ready_fds; qp < rp; qp++) {
	int   fd      = qp->pfd.fd;
	short revents = qp->pfd.revents;

#ifdef __linux__
	/* Linux sets pollhup on fresh sockets. Mask it out if not POLL_INPUT */
	if (!(poll_fds[fd_data[fd].pix].events & POLL_INPUT)) {
	  revents &= ~POLLHUP;
	}
#endif

	if (revents & (POLL_INPUT|POLLOUT)) {
	    if (revents & POLLOUT) 
		output_ready(qp->oport, fd);
	    /* check if output_ready affected selected events */
	    if (revents & (POLL_INPUT|POLLHUP)) {
	        if (poll_fds[fd_data[fd].pix].events & POLL_INPUT) 
                    if ( qp->iport != -1 ) 
                        input_ready(qp->iport, fd);
	    }
	}
	else if (revents & (POLLERR|POLLHUP)) {
	    /* let the driver handle the error condition */
	    if (qp->pfd.events & POLL_INPUT) {
                Port* p = &erts_port[qp->iport];
                if ( p->status == FREE ) {
                    erl_printf(CBUF, "Driver input on free port! fd,port,driver,name:"
                               " %d,%d,%s,%s\n", fd,
                               qp->iport, 
                               erts_port[qp->iport].drv_ptr->driver_name,
                               erts_port[qp->iport].name);
                }
                if ( qp->iport != -1 )
                    input_ready(qp->iport, fd);
            }
	    else
		output_ready(qp->oport, fd);
	}
	else if (revents & POLLNVAL) {
	    if (qp->pfd.events & POLL_INPUT) {
		erl_printf(CBUF, "Bad input fd in poll()! fd,port,driver,name:"
			   " %d,%d,%s,%s\n", fd,
			   qp->iport, 
			   erts_port[qp->iport].drv_ptr->driver_name,
			   erts_port[qp->iport].name);
	    } 
	    else if (qp->pfd.events & POLLOUT) {
		erl_printf(CBUF,
			   "Bad output fd in poll()! fd,port,driver,name:"
			   " %d,%d,%s,%s\n", fd,
			   qp->oport,
			   erts_port[qp->oport].drv_ptr->driver_name,
			   erts_port[qp->oport].name);
	    } 
	    else {
		erl_printf(CBUF, "Bad fd in poll(), %d!\n", fd);
	    }
	    send_error_to_logger(NIL);

	    /* unmap entry */
	    if (qp->pfd.events & POLL_INPUT)
		driver_select(qp->iport, fd, DO_READ|DO_WRITE, 0);
	    if (qp->pfd.events & POLLOUT) {
		if (!(qp->pfd.events & POLL_INPUT) || (qp->iport != qp->oport))
		    driver_select(qp->oport, fd, DO_READ|DO_WRITE, 0);
	    }
	}
    }
}

static void (*check_io_func)(int) = check_io_p;

static void check_io(int do_wait)
{
    (*check_io_func)(do_wait);
}

#endif /* #ifdef USE_KERNEL_POLL */

#endif

/* Fills in the systems representation of the jam/beam process identifier.
** The Pid is put in STRING representation in the supplied buffer,
** no interpretatione of this should be done by the rest of the
** emulator. The buffer should be at least 21 bytes long.
*/
void sys_get_pid(char *buffer){
    pid_t p = getpid();
    /* Assume the pid is scalar and can rest in an unsigned long... */
    sprintf(buffer,"%lu",(unsigned long) p);
}

int sys_putenv(char *buffer){
    Uint sz = strlen(buffer)+1;
    char *env = erts_alloc(ERTS_ALC_T_PUTENV_STR, sz);
    erts_sys_misc_mem_sz += sz;
    strcpy(env,buffer);
    return(putenv(env));
}

void
sys_init_io(byte *buf, Uint size)
{
    tmp_buf = buf;
    tmp_buf_size = size;
    cerr_pos = 0;

    fd_data = (struct fd_data *)
	erts_alloc(ERTS_ALC_T_FD_TAB, max_files * sizeof(struct fd_data));
    erts_sys_misc_mem_sz += max_files * sizeof(struct fd_data);

    max_fd = -1;

#ifdef USE_SELECT
    FD_ZERO(&input_fds);
    FD_ZERO(&output_fds);

#else
    poll_fds =  (struct pollfd *)
	erts_alloc(ERTS_ALC_T_POLL_FDS, max_files * sizeof(struct pollfd));
    erts_sys_misc_mem_sz += max_files * sizeof(struct pollfd);
    ready_fds =  (struct readyfd *)
	erts_alloc(ERTS_ALC_T_READY_FDS, (ERL_POLL_READY_ENTRIES
					  * max_files
					  * sizeof(struct readyfd)));
    erts_sys_misc_mem_sz += (ERL_POLL_READY_ENTRIES
			     * max_files
			     * sizeof(struct readyfd));

    nof_ready_fds = 0;
    {
	int i;

	for (i=0; i < max_files; i++) {
	    poll_fds[i].fd      = -1;
	    poll_fds[i].events  = 0;
	    poll_fds[i].revents = 0;

	    fd_data[i].pix = -1;
	}
    }


#ifdef USE_KERNEL_POLL

    if (use_kernel_poll) {
	driver_select_func = driver_select_kp;
	check_io_func = check_io_kp;

#ifdef USE_DEVPOLL
	devpoll_init();
#if HAVE_LINUX_KPOLL_H
	max_poll_idx = -1;
	kpoll_enable();
#endif
#else
#ifdef USE_KQUEUE
	kqueue_init();
	max_poll_idx = -1;
	kqueue_enable();
#endif
#endif /* ! USE_DEVPOLL */

    }
    else {
	driver_select_func = driver_select_p;
	check_io_func = check_io_p;
    }

#endif /* #ifdef USE_KERNEL_POLL */

#endif /* !USE_SELECT */

#ifdef USE_THREADS
    {
	/* This is speical stuff, starting a driver from the 
	 * system routines, but is a nice way of handling stuff
	 * the erlang way
	 */
	SysDriverOpts dopts;
	int ret;

	sys_memset((void*)&dopts, 0, sizeof(SysDriverOpts));
	add_driver_entry(&async_driver_entry);
	/* FIXME: 7 == NIL */
	ret = open_driver(&async_driver_entry, 7, "async", &dopts);
	DEBUGF(("open_driver = %d\n", ret));
    }
#endif
}

#if (0) /* unused? */
static int write_fill(fd, buf, len)
int fd, len;
char *buf;
{
    int i, done = 0;
    
    do {
	if ((i = write(fd, buf+done, len-done)) < 0) {
	    if (errno != EINTR)
		return (i);
	    i = 0;
	}
	done += i;
    } while (done < len);
    return (len);
}
#endif

extern const char pre_loaded_code[];
extern char* const pre_loaded[];

void erts_sys_alloc_init(void)
{
    elib_ensure_initialized();
}

void *erts_sys_alloc(ErtsAlcType_t t, void *x, Uint sz)
{
    void *res = malloc((size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return malloc((size_t) sz);
    }
#endif
    return res;
}

void *erts_sys_realloc(ErtsAlcType_t t, void *x, void *p, Uint sz)
{
    void *res = realloc(p, (size_t) sz);
#if HAVE_ERTS_MSEG
    if (!res) {
	erts_mseg_clear_cache();
	return realloc(p, (size_t) sz);
    }
#endif
    return res;
}

void erts_sys_free(ErtsAlcType_t t, void *x, void *p)
{
    free(p);
}

/* What happens when we write to a buffer? Do we want the extra \r?
   Now we do this also when not in raw mode. */
static char *fix_nl(char *f, CIO where)
{
   char c;
   static char buf[100];
   char *from, *to;

   if (where != CERR)
      return f;

   from = f;
   to = buf;

   while ((c = *from++) != '\0')
   {
      if (to >= buf+95)
	 return f;
      if (c == '\n')
	 *to++ = '\r';
      *to++ = c;
   }
   *to = '\0';
   return buf;
}

/* XXX It isn't possible to do this safely without "*nsprintf"
   (i.e. something that puts a limit on the number of chars printed)
   - the below is probably the best we can do...    */
    
/*VARARGS*/
void sys_printf(CIO where, char* format, ...)
{
    va_list va;
    va_start(va,format);

    format = fix_nl(format, where);

    if (where == CBUF) {
	if (cerr_pos < TMP_BUF_MAX) {
	    vsprintf((char*)&tmp_buf[cerr_pos],format,va);
	    cerr_pos += sys_strlen((char*)&tmp_buf[cerr_pos]);
	    if (cerr_pos >= tmp_buf_size)
		erl_exit(1, "Internal buffer overflow in erl_printf\n");
	    if (cerr_pos >= TMP_BUF_MAX) {
		strcpy((char*)&tmp_buf[TMP_BUF_MAX - 3], "...");
		cerr_pos = TMP_BUF_MAX;
	    }
	}
    }
    else if (where == CERR)
	erl_error(format, va);
    else if (where == COUT)  {
        vfprintf(stdout, format, va);
    } else {
        /* where indicates which fd to write to */
        vsprintf((char*)tmp_buf,format,va);
	write(where,tmp_buf,sys_strlen((char*)tmp_buf));
    }
      
    va_end(va);
}

void sys_putc(ch, where)
int ch; CIO where;
{
    if (ch == '\n' && where == CERR)
       sys_putc('\r', where);

    if (where == CBUF) {
	if (cerr_pos < TMP_BUF_MAX) {
	    tmp_buf[cerr_pos++] = ch;
	    if (cerr_pos == TMP_BUF_MAX) {
		strcpy((char*)&tmp_buf[TMP_BUF_MAX - 3], "...");
		cerr_pos = TMP_BUF_MAX;
	    }
	}
	else if (cerr_pos >= tmp_buf_size)
	    erl_exit(1, "Internal buffer overflow in erl_printf\n");
    }
    else if (where == COUT) {
	fputc(ch, stdout);
    } else
	sys_printf(where, "%c", ch);
}

/* Return a pointer to a vector of names of preloaded modules */

Preload*
sys_preloaded(void)
{
    return (Preload *) pre_loaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char*
sys_preload_begin(Preload* p)
{
    return p->code;
}

/* Clean up if allocated */
void sys_preload_end(Preload* p)
{
    /* Nothing */
}

/* Read a key from console (?) */

int sys_get_key(fd)
int fd;
{
    int c;
    unsigned char rbuf[64];

    fflush(stdout);		/* Flush query ??? */

    if ((c = read(fd,rbuf,64)) <= 0) {
      return c; 
    }

    return rbuf[0]; 
}


#ifdef DEBUG

#if 0
/* handy debug function that prints the current state of poll_fds[] */
static void print_poll_fds()
{
    int i;
    fprintf(stderr, "print_poll_fds() max_fd=%d\n\r", max_fd);
    for (i=0; i<max_fd+1; i++) {
	int fd=poll_fds[i].fd;
	fprintf(stderr, "poll_fds[%d].fd=%d events=0x%x revents=0x%x "
		"fd_data[%d].pix=%d inport=%d outport=%d\n\r",
		i, fd, poll_fds[i].events, poll_fds[i].revents,
		fd, fd_data[fd].pix, fd_data[fd].inport, fd_data[fd].outport);
    }
}
#endif

#if (0) /* unused? */
static void  sdbg()
{
    int i;
    Pend *p;
    fprintf(stderr, "maxfd = %d\n", max_fd);
    for (i = 0; i <max_files; i++) {
	if (FD_ISSET(i, &input_fds))
	    fprintf(stderr, "fd %d set on input \n", i);
	if (fd_data[i].remain > 0)
	    fprintf(stderr,"   Pending input %d bytes on %d\n", 
		    fd_data[i].remain,i);

	if (FD_ISSET(i, &output_fds))
	    fprintf(stderr, "fd %d set on output \n", i);
    }
}
#endif /* (0) */

extern int erts_initialized;
void
erl_assert_error(char* expr, char* file, int line)
{   
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
    if (erts_initialized)
	erl_crash_dump(file, line, "Assertion failed: %s\n", expr);
    abort();
}

void
erl_debug(char* fmt, ...)
{
    char sbuf[1024];		/* Temporary buffer. */
    va_list va;
    
    if (debug_log) {
	va_start(va, fmt);
	vsprintf(sbuf, fmt, va);
	va_end(va);
	fprintf(stderr, "%s", sbuf);
    }
}

#endif /* DEBUG */

static void note_child_death(int pid, int status)
{
  int i;

  for (i = 0; i < max_files; i++)
    if (driver_data[i].pid == pid) {
      driver_data[i].alive = 0;
      driver_data[i].status = status;
      break;
    }
}

#if CHLDWTHR

static void *
child_waiter(void *unused)
{
  sigset_t chldsigset;
  int sig;
  int pid;
  int status;

  sigemptyset(&chldsigset);
  sigaddset(&chldsigset, SIGCHLD);

  while(1) {
    do {
      pid = waitpid(-1, &status, 0);
      if(pid < 0 && errno == ECHILD) {
	/* Based on that all threads block SIGCHLD all the time! */
	erts_thr_sigwait(&chldsigset, &sig);
	ASSERT(sig == SIGCHLD);
      }
    } while(pid < 0);

    CHLD_STAT_LOCK;
    note_child_death(pid, status);
    CHLD_STAT_UNLOCK;
  }

  return NULL;
}

#define check_children()

#else

static void check_children(void)
{
    int pid;
    int status;

    if (children_died) {
      sys_sigblock(SIGCHLD);
      while ((pid = waitpid(-1, &status, WNOHANG)) > 0)
	note_child_death(pid, status);
      children_died = 0;
      sys_sigrelease(SIGCHLD);
    }

}

#endif

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void
erl_sys_schedule(int runnable)
{
    if (runnable) {
	check_io(0);		/* Poll for I/O */
	check_async_ready();	/* Check async completions */
    } else {
	check_io(check_async_ready() ? 0 : 1);
    }
    check_children();
}


#ifdef USE_KERNEL_POLL /* get_value() is currently only used when
			  kernel-poll is used */

/* Get arg marks argument as handled by
   putting NULL in argv */
static char *
get_value(char* rest, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    argv[*ip] = NULL;
    if (*rest == '\0') {
	char *next = argv[*ip + 1];
	if (next[0] == '-'
	    && next[1] == '-'
	    &&  next[2] == '\0') {
	    erl_printf(CERR, "bad \"%s\" value: \n", param);
	    erts_usage();
	}
	(*ip)++;
	argv[*ip] = NULL;
	return next;
    }
    return rest;
}

#endif /* #ifdef USE_KERNEL_POLL */

void
erl_sys_args(int* argc, char** argv)
{
    int i, j;

    i = 1;

    ASSERT(argc && argv);

    while (i < *argc) {
	if(argv[i][0] == '-') {
	    switch (argv[i][1]) {
#ifdef USE_KERNEL_POLL
	    case 'K': {
		char *arg = get_value(argv[i] + 2, argv, &i);
		if (strcmp("true", arg) == 0) {
		    use_kernel_poll = 1;
		}
		else if (strcmp("false", arg) == 0) {
		    use_kernel_poll = 0;
		}
		else {
		    erl_printf(CERR, "bad \"K\" value: %s\n", arg);
		    erts_usage();
		}
		break;
	    }
#endif
	    case '-':
		goto done_parsing;
	    default:
		break;
	    }
	}
	i++;
    }

 done_parsing:

    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;
}

#ifdef HAVE_GETHRVTIME_PROCFS_IOCTL

int sys_start_hrvtime(void)
{
    long msacct = PR_MSACCT;
    int fd;

    if ( (fd = open("/proc/self", O_WRONLY)) == -1) {
	return -1;
    }
    if (ioctl(fd, PIOCSET, &msacct) < 0) {
	close(fd);
	return -2;
    }
    close(fd);
    return 0;
}

int sys_stop_hrvtime(void)
{
    long msacct = PR_MSACCT;
    int fd;

    if ( (fd = open("/proc/self", O_WRONLY)) == -1) {
	return -1;
    }
    if (ioctl(fd, PIOCRESET, &msacct) < 0) {
	close(fd);
	return -2;
    }
    close(fd);
    return 0;
}

#endif /* HAVE_GETHRVTIME_PROCFS_IOCTL */

#ifdef USE_KERNEL_POLL /* kernel poll support */

#ifdef USE_KQUEUE

static void kqueue_init()
{
    if ( getenv("ERL_NO_KERNEL_POLL") != NULL ) {
        DEBUGF(("Use of kqueue disabled\n"));
        kqueue_fd=-1;
        return;
    } 
    
    if ( (kqueue_fd=kqueue()) < 0 ) {
        DEBUGF(("Will use poll()\n"));
        kqueue_fd = -1;
    } else {
	kqueue_res = 
	    (struct kevent *) erts_alloc(ERTS_ALC_T_POLL_FDS,
					 (ERL_POLL_READY_ENTRIES
					  * sizeof(struct kevent)
					  * max_files));
	erts_sys_misc_mem_sz += (ERL_POLL_READY_ENTRIES
				 *sizeof(struct kevent)
				 *max_files);
    }
}

static void kqueue_enable()
{
    int pix;
    
    /*     Add kqueue fd to pollable descriptors */
    if ( kqueue_fd == -1 ) return;
    
    max_fd++;
    max_poll_idx++;
    pix = max_fd;
    fd_data[kqueue_fd].pix = pix;
    poll_fds[pix].fd = kqueue_fd;
    poll_fds[pix].events = POLL_INPUT;
    poll_fds[pix].revents = 0;
}

static void kqueue_set_event(int fd, int filter, int flags)
{
    int res;
    struct timespec ts;
    struct kevent ke,*kep=&ke;
    
    memset(&ts,(char)0,sizeof(ts));
    memset(kep,(char)0,sizeof(struct kevent));
    
    kep->ident = fd;
    kep->filter = filter;
    kep->flags = flags;
    
    if ( (res=kevent(kqueue_fd,kep,1,NULL,0,&ts)) == -1 ) {
        fprintf(stderr,"WARNING: %s:%d kevent call failed errno = %d fd = %d filter = %d flags = %d (ignored)\r\n",
                __FILE__,__LINE__,errno,fd,filter,flags);
        if ( (errno == ENOENT) ) return; /* fd is not valid (closed?) */
        erl_exit(1, "%s:%d kevent call failed. fd = %d filter = %d flags = %d errno = %d\n",
                 __FILE__,__LINE__,fd,filter,flags,errno);
    }
}

static void kqueue_update_pix(int pix, int old_events)
{
    struct stat sb;
    
    if ( fstat(poll_fds[pix].fd,&sb) == -1 ) {
        erl_exit(1,"Can't fstat file %d. errno = %d\n",poll_fds[pix].fd,errno);
    }
    
    if ( (kqueue_fd != -1) && ( S_ISFIFO(sb.st_mode) || S_ISSOCK(sb.st_mode) ) ) {
        
        /* Input */
        if ( (poll_fds[pix].events & POLL_INPUT) == POLL_INPUT ) 
            kqueue_set_event(poll_fds[pix].fd,EVFILT_READ,(EV_ADD|EV_ENABLE));
        else 
            if ( ( old_events & POLL_INPUT ) == POLL_INPUT ) 
                kqueue_set_event(poll_fds[pix].fd,EVFILT_READ,EV_DELETE);
        
        /* Output */
        if ( (poll_fds[pix].events & POLLOUT) == POLLOUT ) 
            kqueue_set_event(poll_fds[pix].fd,EVFILT_WRITE,(EV_ADD|EV_ENABLE));
        else 
            if ( ( old_events & POLLOUT ) == POLLOUT ) 
                kqueue_set_event(poll_fds[pix].fd,EVFILT_WRITE,EV_DELETE);
	
    } else {
        if ( poll_fds[pix].events == 0 ) {
            if ( pix != max_poll_idx ) {
                struct pollfd tmp_pfd;
                /* swap this slot with max_poll_idx and decrement */
                tmp_pfd = poll_fds[pix];
                poll_fds[pix] = poll_fds[max_poll_idx];
                fd_data[poll_fds[pix].fd].pix = pix;
                poll_fds[max_poll_idx] = tmp_pfd;
                fd_data[poll_fds[max_poll_idx].fd].pix = max_poll_idx;
            }
            max_poll_idx--;
            /* the normal processing in driver_select will kick it out completely */
        } else {
            if ( pix > max_poll_idx ) {
                max_poll_idx++;
                if ( max_poll_idx != max_fd ) {
                    struct pollfd tmp_pfd;
                    tmp_pfd = poll_fds[max_poll_idx];
                    poll_fds[max_poll_idx] = poll_fds[pix];
                    fd_data[poll_fds[max_poll_idx].fd].pix = max_poll_idx;
                    poll_fds[pix] = tmp_pfd;
                    fd_data[poll_fds[pix].fd].pix = pix;
                }
            }
        }
    }
}
  
#endif

/* /dev/kpoll support */

#ifdef USE_DEVPOLL

#if HAVE_LINUX_KPOLL_H
static void kpoll_enable()
{
    int pix;
    
    /* Add /dev/epoll fd to pollable descriptors */
    if ( dev_poll_fd == -1 ) return;
    
    max_fd++;
    max_poll_idx++;
    pix = max_fd;
    fd_data[dev_poll_fd].pix = pix;
    poll_fds[pix].fd = dev_poll_fd;
    poll_fds[pix].events = POLL_INPUT;
    poll_fds[pix].revents = 0;
}

static void kpoll_init()
{
    if ( (dev_poll_fd=open("/dev/kpoll",O_RDWR)) < 0 ) {
        /* This will happen if the module is not inserted yet as well... */
        DEBUGF(("Will use poll()\n"));
        dev_poll_fd = -1; /* We will not use /dev/kpoll */
    } else {
      DEBUGF(("Will use /dev/kpoll\n"));
      if ( ioctl(dev_poll_fd,KP_ALLOC,max_files) == -1 ) {
	perror("ioctl(KP_ALLOC)");
	erl_exit(1, "Can't allocate %d /dev/kpoll file descriptors\n",
		 max_files);
      }
      if ( (dev_poll_map = (char *)mmap(NULL,KP_MAP_SIZE(max_files),
					PROT_READ|PROT_WRITE, 
					MAP_PRIVATE, dev_poll_fd, 0)) == NULL ) {
	erl_exit(1, "Can't mmap /dev/kpoll result area.\n");
      }
      dev_poll_rfds =  NULL;
    }
}

#endif /* HAVE_LINUX_KPOLL_H */

#ifdef HAVE_SYS_DEVPOLL_H

static void solaris_devpoll_init(void)
{
    if ( (dev_poll_fd=open("/dev/poll",O_RDWR)) < 0 ) {
        DEBUGF(("Will use poll()\n"));
        dev_poll_fd = -1; /* We will not use /dev/poll */
    } else {
        DEBUGF(("Will use /dev/poll\n"));
        dev_poll_rfds =
	    (struct pollfd *) erts_alloc(ERTS_ALC_T_POLL_FDS,
					 max_files * sizeof(struct pollfd));
	erts_sys_misc_mem_sz += max_files * sizeof(struct pollfd);
    }
}

#endif

static void devpoll_init(void) 
{
    if ( getenv("ERL_NO_KERNEL_POLL") != NULL ) {
        DEBUGF(("Use of kernel poll disabled.\n"));
        dev_poll_fd=-1;
    } else {
        /* Determine use of poll vs. /dev/poll at runtime */
#ifdef HAVE_LINUX_KPOLL_H
        kpoll_init();
#else
#ifdef HAVE_SYS_DEVPOLL_H
        solaris_devpoll_init();
#endif
#endif
    }
}

static int devpoll_write(int fd, void *buf, size_t count)
{
    int res;
    int left = count;

    do {
        if ( (res=write(fd,buf,left)) < 0 ) {
            if ( errno == EINTR ) continue;
            return res;
        } else {
            buf += res;
            left -= res;
        }
    } while (left);
    return count;
}

static void devpoll_update_pix(int pix)
{
    int res;

#if HAVE_LINUX_KPOLL_H
    struct stat sb;

    if ( fstat(poll_fds[pix].fd,&sb) == -1 ) {
        erl_exit(1,"Can't fstat file %d. errno = %d\n",poll_fds[pix].fd,errno);
    }

    if ( (dev_poll_fd != -1 ) && (S_ISFIFO(sb.st_mode) || S_ISSOCK(sb.st_mode)) ) {

#endif
    if ( dev_poll_fd != -1 ) {
        if ( (res=devpoll_write(dev_poll_fd,&poll_fds[pix],sizeof(struct pollfd))) != 
             (sizeof(struct pollfd)) ) {
            erl_exit(1,"Can't write to /dev/poll\n");
        }
    }
#if HAVE_LINUX_KPOLL_H
    } else {
        if ( poll_fds[pix].events & POLLREMOVE ) {
            if ( pix != max_poll_idx ) {
                struct pollfd tmp_pfd;
                /* swap this slot with max_poll_idx and decrement */
                tmp_pfd = poll_fds[pix];
                poll_fds[pix] = poll_fds[max_poll_idx];
                fd_data[poll_fds[pix].fd].pix = pix;
                poll_fds[max_poll_idx] = tmp_pfd;
                fd_data[poll_fds[max_poll_idx].fd].pix = max_poll_idx;
            }
            max_poll_idx--;
            /* the normal processing in driver_select will kick it out completely */
        } else {
            if ( pix > max_poll_idx ) {
                max_poll_idx++;
                if ( max_poll_idx != max_fd ) {
                    struct pollfd tmp_pfd;
                    tmp_pfd = poll_fds[max_poll_idx];
                    poll_fds[max_poll_idx] = poll_fds[pix];
                    fd_data[poll_fds[max_poll_idx].fd].pix = max_poll_idx;
                    poll_fds[pix] = tmp_pfd;
                    fd_data[poll_fds[pix].fd].pix = pix;
                }
            }
        }
    }
#endif /* HAVE_LINUX_KPOLL_H */
}


#ifdef HAVE_SYS_DEVPOLL_H

static void devpoll_clear_pix(int pix)
{
    struct pollfd tmp = poll_fds[pix];
    tmp.events = POLLREMOVE;

    if ( dev_poll_fd != -1 ) {
        if ( (devpoll_write(dev_poll_fd,&tmp,sizeof(struct pollfd))) != 
             (sizeof(struct pollfd)) ) {
            erl_exit(1,"Can't write to /dev/poll\n");
        }
    }
}
#endif /* HAVE_SYS_DEVPOLL_H */

#endif /* USE_DEVPOLL */

#endif /* USE_KERNEL_POLL */
