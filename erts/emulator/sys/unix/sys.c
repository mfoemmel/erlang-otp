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
#endif /* !USE_SELECT */

#ifdef ISC32
#include <sys/bsdtypes.h>
#endif

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

#include "driver.h"

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
#ifdef USE_THREADS
EXTERN_FUNCTION(void, async_ready, (int, int));
EXTERN_FUNCTION(int, init_async, (int));
EXTERN_FUNCTION(int, exit_async, (_VOID_));
EXTERN_FUNCTION(int, erts_thread_sigmask, (int, const sigset_t*, sigset_t*));
EXTERN_FUNCTION(int, erts_thread_sigwait, (const sigset_t*, int*));
#endif
EXTERN_FUNCTION(int, check_async_ready, (_VOID_));
EXTERN_FUNCTION(int, driver_output, (int, char*, int));
EXTERN_FUNCTION(int, driver_failure, (int, int));
EXTERN_FUNCTION(int, driver_interrupt, (int, int));
EXTERN_FUNCTION(void, increment_time, (int));
EXTERN_FUNCTION(int, next_time, (_VOID_));
EXTERN_FUNCTION(int, send_error_to_logger, (uint32));
EXTERN_FUNCTION(int, schedule, (_VOID_));
EXTERN_FUNCTION(void, set_busy_port, (int, int));
EXTERN_FUNCTION(void, do_break, (_VOID_));

EXTERN_FUNCTION(void, erl_sys_args, (int*, char**));

#if CHLDWTHR
EXTERN_FUNCTION(erl_mutex_t, erts_mutex_sys, (int));
#endif


void erl_crash_dump(char* fmt, va_list args);

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
};

static struct pollfd*  poll_fds;      /* Allocated at startup */
static struct readyfd* ready_fds;     /* Collect after poll */
static int             nof_ready_fds; /* Number of fds after poll */

#endif

static int max_fd;

#ifdef DEBUG
static int debug_log = 0;
#endif

#if CHLDWTHR
static erl_thread_t child_waiter_tid;
#else
static volatile int children_died;
#endif

/* We maintain a linked fifo queue of these structs in order */
/* to manage unfinnished reads/and writes on differenet fd's */

typedef struct pend {
    char *cpos;
    int fd;
    int remain;
    struct pend *next;
    char buf[1];   /* this is a trick to be able to malloc one chunk */
} Pend;

static struct fd_data {
    int   inport;
    int   outport;
#if !defined(USE_SELECT)
    int   pix;       /* index in poll_fds array */
#endif
    char  *buf;
    char  *cpos;
    int   sz;
    int   remain;  /* for input on fd */
    Pend* pending;   /* pending outputs */
} *fd_data;			/* indexed by fd */
    


#ifndef NO_FPE_SIGNALS
/* global variable for floating point checks, (see sys.h) */
/* Note! This is part of the interface Machine <---> sys.c */
int erl_fp_exception = 0;
static FUNCTION(void, init_fpe_handler, (void));
#  ifdef HAVE_IEEE_HANDLER
FUNCTION(void, fpe_sig_handler, (int, int, struct sigcontext*, char*));
#  else
/* FUNCTION(RETSIGTYPE, fpe_sig_handler, (int, int, struct sigcontext*)); */
FUNCTION(RETSIGTYPE, fpe_sig_handler, (int, int, void*));
#  endif
#else
#  ifdef USE_MATHERR
int erl_fp_exception = 0;
#  endif
#endif

/* forward declarations */
static FUNCTION(int, read_fill, (int, char*, int));
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

void
erl_sys_init(void)
{
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

#ifdef NO_FPE_SIGNALS
#ifdef SIGFPE
    sys_sigset(SIGFPE, SIG_IGN); /* Ignore so we can test for NaN and Inf */
#endif
#else
    init_fpe_handler();
#endif

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
void sys_sigblock(sig)
int sig;
{
    sighold(sig);
}
void sys_sigrelease(sig)
int sig;
{
    sigrelse(sig);
}
#else /* !SIG_SIGSET */
#ifdef SIG_SIGNAL		/* Old BSD */
RETSIGTYPE (*sys_sigset(sig, func))()
int sig;
RETSIGTYPE (*func)();
{
    return(signal(sig, func));
}
sys_sigblock(sig)
int sig;
{
    sigblock(sig);
}
sys_sigrelease(sig)
int sig;
{
    sigsetmask(sigblock(0) & ~sigmask(sig));
}
#else /* !SIG_SIGNAL */	/* The True Way - POSIX!:-) */
RETSIGTYPE (*sys_sigset(sig, func))()
int sig;
RETSIGTYPE (*func)();
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
#define sigprocmask erts_thread_sigmask
#endif

void sys_sigblock(sig)
int sig;
{
    sigset_t mask;

    sigemptyset(&mask);
    sigaddset(&mask, sig);
    sigprocmask(SIG_BLOCK, &mask, (sigset_t *)NULL);
}

void sys_sigrelease(sig)
int sig;
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
static RETSIGTYPE break_handler(sig)
int sig;
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
static void request_break()
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

static void user_signal1()
{
   /* We do this at interrupt level, since the main reason for
      wanting to generate a crash dump in this way is that the emulator
      is hung somewhere, so it won't be able to poll any flag we set here.
      */

   erl_exit(1, "Received SIGUSR1\n");
}

#ifdef QUANTIFY
static void user_signal2()
{
   quantify_save_data();
}
#endif

static void do_quit()
{
    halt_0(0);
}

void init_break_handler()
{
   sys_sigset(SIGINT, request_break);
   sys_sigset(SIGUSR1, user_signal1);
#ifdef QUANTIFY
   sys_sigset(SIGUSR2, user_signal2);
#endif
   sys_sigset(SIGQUIT, do_quit);
}

int sys_max_files() 
{
   return(max_files);
}

static void block_signals()
{
#if !CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif
   sys_sigblock(SIGINT);
   sys_sigblock(SIGUSR1);
}

static void unblock_signals()
{
#if !CHLDWTHR
   sys_sigrelease(SIGCHLD);
#endif
   sys_sigrelease(SIGINT);
   sys_sigrelease(SIGUSR1);
}

/******************* Routines for time measurement *********************/

int erts_ticks_per_sec = 0; /* Will be SYS_CLK_TCK in erl_unix_sys.h */

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
    return SYS_CLOCK_RESOLUTION;
}

/************************** OS info *******************************/

/* Used by erlang:info/1. */
/* (This code was formerly in drv.XXX/XXX_os_drv.c) */

char os_type[] = "unix";

static int
get_number(str_ptr)
char** str_ptr;
{
    char* s = *str_ptr;		/* Pointer to beginning of string. */
    char* dot;			/* Pointer to dot in string or NULL. */

    if (!isdigit(*s))
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
os_flavor(namebuf, size)
char* namebuf;			/* Where to return the name. */
unsigned size;			/* Size of name buffer. */
{
    struct utsname uts;		/* Information about the system. */

    (void) uname(&uts);
    strcpy(namebuf, uts.sysname);
    for ( ; *namebuf; namebuf++)
	if (isupper(*namebuf))
	    *namebuf = tolower(*namebuf);
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

void init_getenv_state(state)
GETENV_STATE *state;
{
   *state = NULL;
}

char *getenv_string(state0)
GETENV_STATE *state0;
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
static uint32 tmp_buf_size;
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
/* dead_child_status_lock is used to protect against concurrent accesses
   of the driver_data fields pid, alive, and status. */
erl_mutex_t dead_child_status_lock;
#endif

/* Driver interfaces */
static long spawn_start();
static long fd_start();
static long vanilla_start();
static int spawn_init();
static int fd_stop();
static int stop();
static int ready_input();
static int ready_output();
static int output();
static int outputv();


const struct driver_entry spawn_driver_entry = {
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
const struct driver_entry fd_driver_entry = {
    null_func,
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
const struct driver_entry vanilla_driver_entry = {
    null_func,
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

static int  async_drv_init();
static long async_drv_start();
static int  async_drv_stop();
static int  async_drv_input();

/* INTERNAL use only */

struct driver_entry async_driver_entry = {
    async_drv_init,
    async_drv_start,
    async_drv_stop,
    null_func,
    async_drv_input,
    null_func,
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
static RETSIGTYPE onchld()
{
#if CHLDWTHR
    ASSERT(0); /* We should *never* catch a SIGCHLD signal */
#else
    children_died = 1;
#endif
}

static int set_driver_data(port_num,
			   ifd,
			   ofd,
			   packet_bytes,
			   read_write,
			   exit_status,
			   pid)
int port_num, ifd, ofd, packet_bytes, read_write, exit_status, pid;
{
    if (read_write & DO_READ) {
	driver_data[ifd].packet_bytes = packet_bytes;
	driver_data[ifd].port_num = port_num;
	driver_data[ifd].report_exit = exit_status;
	driver_data[ifd].pid = pid;
	driver_data[ifd].alive = 1;
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
	return(ofd);
    }
}

static int spawn_init()
{
   int i;

   sys_sigset(SIGPIPE, SIG_IGN); /* Ignore - we'll handle the write failure */
   if ((driver_data = (struct driver_data *)
	sys_alloc_from(181,max_files * sizeof(struct driver_data))) == NULL)
      erl_exit(1, "Can't allocate %d bytes of memory\n",
	       max_files * sizeof(struct driver_data));

   for (i = 0; i < max_files; i++)
      driver_data[i].pid = -1;

#if CHLDWTHR
   sys_sigblock(SIGCHLD);
#endif

   sys_sigset(SIGCHLD, onchld); /* Reap children */

#if CHLDWTHR
   dead_child_status_lock = erts_mutex_sys(2);
   if(erts_thread_create(&child_waiter_tid, child_waiter, NULL, 0) != 0)
     erl_exit(1, "Failed to start child waiter thread");
#endif

   return 1;
}

static void close_pipes(ifd, ofd, read_write)
int ifd[2], ofd[2];
int read_write;
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

static void init_fd_data(fd, prt)
int fd, prt;
{
    fd_data[fd].pending = NULL;
    fd_data[fd].buf = fd_data[fd].cpos = NULL;
    fd_data[fd].remain = fd_data[fd].sz = 0;
}

static char **build_unix_environment(block)
char *block;
{
   int len;
   char *cp;
   char **cpp;

   cp = block;
   len = 0;
   while (*cp != '\0')
   {
      cp += strlen(cp) + 1;
      len++;
   }

   cpp = (char **) sys_alloc (sizeof(char *) * (len+1));
   if (cpp == NULL)
      return NULL;
   
   cp = block;
   len = 0;
   while (*cp != '\0')
   {
      cpp[len] = cp;
      cp += strlen(cp) + 1;
      len++;
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

static long spawn_start(port_num, name, opts)
    int port_num;
    char* name;
    SysDriverOpts* opts;
{
    int ifd[2], ofd[2], len, pid, i;
    char *p1, *p2;
    char **new_environ;
    char **tmp;
    int saved_errno;
    long res;

    switch (opts->read_write) {
    case DO_READ:
	if (pipe(ifd) < 0)
	    return(-2);
	if (ifd[0] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return(-2);
	}
	ofd[1] = -1;		/* keep purify happy */
	break;
    case DO_WRITE:
	if (pipe(ofd) < 0) return(-2);
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return(-2);
	}
	ifd[0] = -1;		/* keep purify happy */
	break;
    case DO_READ|DO_WRITE:
	if (pipe(ifd) < 0) return(-2);
	errno = EMFILE;		/* default for next two conditions */
	if (ifd[0] >= max_files || pipe(ofd) < 0) {
	    close_pipes(ifd, ofd, DO_READ);
	    return(-2);
	}
	if (ofd[1] >= max_files) {
	    close_pipes(ifd, ofd, opts->read_write);
	    errno = EMFILE;
	    return(-2);
	}
	break;
    default:
	ASSERT(0);
	return(-1);
    }

    /* make the string suitable for giving to "sh" (5 = strlen("exec")) */
    len = strlen(name);
    if (len + 5 >= tmp_buf_size) {
	close_pipes(ifd, ofd, opts->read_write);
	errno = ENAMETOOLONG;
	return(-2);
    }
    /* name == tmp_buf needs overlapping-safe move - just do it always */
    /* should use memmove() but it isn't always available */
    p1 = (char *)&tmp_buf[len + 5];
    p2 = name+len;
    while (p2 >= name)
	*p1-- = *p2--;
    memcpy(tmp_buf, "exec ", 5);

    if (opts->envir != NULL)
    {
       if ((tmp = build_unix_environment(opts->envir)) == NULL)
       {
	  errno = ENOMEM;
	  return -2;
       }
    }
    else
       tmp = NULL;

    new_environ = tmp;		/* Using 'tmp' has only one purpose: */
				/* to suppress a warning from the compiler */
				/* about 'new_environ' being clobbered */
				/* by 'vfork'. */
				/* Why this suppresses the warning is */
				/* a mystery. */

#ifndef QNX
    /* Block child from SIGINT and SIGUSR1. Must be before fork()
       to be safe. */
    block_signals();

#if CHLDWTHR
    erts_mutex_lock(dead_child_status_lock);
#endif

    /* See fork/vfork discussion before this function. */
    if (getenv("ERL_NO_VFORK") != NULL) {
      DEBUGF(("Using fork\n"));
      pid = fork();
    } else {
      DEBUGF(("Using vfork\n"));
      pid = vfork();
    }

    if (pid == 0) {
	/* child */
	if (opts->use_stdio) {
	    if (opts->read_write & DO_READ){
		dup2(ifd[1], 1); /* stdout for process */
		if(opts->redir_stderr)
		    dup2(ifd[1], 2); /* stderr for process */
	    }
	    if (opts->read_write & DO_WRITE)
		dup2(ofd[0], 0); /* stdin for process */
	} else {		/* XXX will fail if ofd[0] == 4 (unlikely..) */
	    if (opts->read_write & DO_READ)
		dup2(ifd[1], 4);
	    if (opts->read_write & DO_WRITE)
		dup2(ofd[0], 3);
	}
	for (i = opts->use_stdio ? 3 : 5; i < max_files; i++)
	    (void) close(i);
#ifdef USE_SETPGRP_NOARGS	/* SysV */
	(void) setpgrp();
#else
#ifdef USE_SETPGRP		/* BSD */
	pid = getpid();
	(void) setpgrp(0,pid);
#else				/* POSIX */
	(void) setsid();
#endif
	unblock_signals();
#endif
	if (opts->wd != NULL)
	{
	   if (chdir(opts->wd) < 0)
	      _exit(1);
	}

	execle("/bin/sh", "sh", "-c",
	       tmp_buf,
	       (char *) 0,
	       new_environ != NULL ? new_environ : environ);
	_exit(1);
    } else if (pid == -1) {
        saved_errno = errno;
        unblock_signals();
        close_pipes(ifd, ofd, opts->read_write);
	errno = saved_errno;
	return(-2);
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
	return(-1);
    }
    reset_qnx_spawn();
#endif /* QNX */

    if (new_environ != NULL)
       sys_free(new_environ);

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

#if CHLDWTHR
    /* Don't unlock dead_child_status_lock until now of the same reason
       as above */
    erts_mutex_unlock(dead_child_status_lock);
#endif
    return res;
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

static long fd_start(port_num, name, opts)
    int port_num;
    char* name;			/* Ignored. */
    SysDriverOpts* opts;
{
    long res;

    if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
	((opts->read_write & DO_WRITE) && opts->ofd >= max_files))
	return(-1);

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
#if CHLDWTHR
    erts_mutex_lock(dead_child_status_lock);
#endif
    res = set_driver_data(port_num, opts->ifd, opts->ofd,
			  opts->packet_bytes, opts->read_write, 0, -1);
#if CHLDWTHR
    erts_mutex_unlock(dead_child_status_lock);
#endif
    return res;
    
}

static void clear_fd_data(fd) 
int fd;
{
    
    if (fd_data[fd].sz > 0) 
	sys_free(fd_data[fd].buf);
    fd_data[fd].buf = NULL;
    fd_data[fd].sz = 0;
    fd_data[fd].remain = 0;
    fd_data[fd].cpos = NULL;
    fd_data[fd].pending = NULL;
}

static void nbio_stop_fd(prt, fd)
int prt, fd;
{
    Pend *p, *p1;
    
    driver_select(prt,fd,DO_READ|DO_WRITE,0);
    clear_fd_data(fd);
    p = fd_data[fd].pending;
    SET_BLOCKING(fd);
    while (p) {
	p1 = p->next;
	sys_free(p);
	p = p1;
    }
    fd_data[fd].pending = NULL;
}

static int fd_stop(fd)  /* Does not close the fds */
long fd;
{
    int ofd;
    
    nbio_stop_fd(driver_data[fd].port_num, (int)fd);
    ofd = driver_data[fd].ofd;
    if (ofd != fd && ofd != -1) 
	nbio_stop_fd(driver_data[fd].port_num, (int)ofd);
    return 1;
}

static long vanilla_start(port_num, name, opts)
    int port_num;
    char *name;
    SysDriverOpts* opts;
{
    int flags, fd;
    long res;

    flags = (opts->read_write == DO_READ ? O_RDONLY :
	     opts->read_write == DO_WRITE ? O_WRONLY|O_CREAT|O_TRUNC :
	     O_RDWR|O_CREAT);
    if ((fd = open(name, flags, 0666)) < 0)
	return(-1);
    if (fd >= max_files) {
	close(fd);
	return(-1);
    }
    SET_NONBLOCKING(fd);
    init_fd_data(fd, port_num);
#if CHLDWTHR
    erts_mutex_lock(dead_child_status_lock);
#endif
    res = set_driver_data(port_num, fd, fd,
			  opts->packet_bytes, opts->read_write, 0, -1);
#if CHLDWTHR
    erts_mutex_unlock(dead_child_status_lock);
#endif
    return res;
}

/* Note that driver_data[fd].ifd == fd if the port was opened for reading, */
/* otherwise (i.e. write only) driver_data[fd].ofd = fd.  */

static int stop(fd)
long fd;
{
    int prt, ofd;

    prt = driver_data[fd].port_num;
    nbio_stop_fd(prt, fd);
    close(fd);

    ofd = driver_data[fd].ofd;
    if (ofd != fd && ofd != -1) {
	nbio_stop_fd(prt, ofd);
	(void) close(ofd);
    }

#if CHLDWTHR
    erts_mutex_lock(dead_child_status_lock);
#endif

    /* Mark as unused. Maybe resetting the 'port_num' slot is better? */
    driver_data[fd].pid = -1;

#if CHLDWTHR
    erts_mutex_unlock(dead_child_status_lock);
#endif

    return 1;
}

static int outputv(fd, ev)
int fd; 
ErlIOVec* ev;
{
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
	return -1;
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
	    return 0;
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return -1;
	    }
	    n = 0;
	}
	driver_enqv(ix, ev, n);  /* n is the skip value */
	driver_select(ix, ofd, DO_WRITE, 1);
    }
    return 0;
}


static int output(fd, buf, len)
long fd;
char *buf;
int len;
{
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
	return -1;
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
	    return 0;
	if (n < 0) {
	    if ((errno != EINTR) && (errno != ERRNO_BLOCK)) {
		driver_failure_posix(ix, errno);
		return -1;
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
    return 0;
}

static int ensure_header(fd, buf,  packet_size, sofar)
int fd;
char *buf;
int packet_size, sofar;
{
    int res;
    int remaining = packet_size - sofar;
    SET_BLOCKING(fd);  /* FIXME !!!!!!!! */
    if (read_fill(fd, buf+sofar, remaining) != remaining)
	return -1;
    switch (packet_size) {
    case 1: res = get_int8(buf); break;
    case 2: res = get_int16(buf); break;
    case 4: res = get_int32(buf); break;
    default:
	return -1;
    }
    SET_NONBLOCKING(fd);
    return(res);
}

static int port_inp_failure(port_num, ready_fd, res)
int port_num;
int ready_fd;
int res;			/* Result: 0 (eof) or -1 (error) */
{
    int err = errno;
    int status;
    int alive;
    
#if CHLDWTHR
    erts_mutex_lock(dead_child_status_lock);
#endif

    alive = driver_data[ready_fd].alive;
    status = driver_data[ready_fd].status;

#if CHLDWTHR
    erts_mutex_unlock(dead_child_status_lock);
#endif

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

static int ready_input(fd, ready_fd)
long fd;
int ready_fd;
{
    int port_num, packet_bytes, res;
    uint32 h;
    char *buf;

    port_num = driver_data[fd].port_num;
    packet_bytes = driver_data[fd].packet_bytes;

    if (packet_bytes == 0) {
	if ((res = read(ready_fd, tmp_buf, tmp_buf_size)) > 0) {
	    driver_output(port_num, (char*)tmp_buf, res);
	    return 0;
	}
	if ((res < 0) && ((errno == EINTR) || (errno == ERRNO_BLOCK)))
	    return 0;
	return port_inp_failure(port_num, ready_fd, res);
    }

    if (fd_data[ready_fd].remain > 0) { /* We try to read the remainder */
	/* space is allocated in buf */
	res = read(ready_fd, fd_data[ready_fd].cpos, 
		   fd_data[ready_fd].remain);
	if (res < 0) {
	    if ((errno == EINTR) || (errno == ERRNO_BLOCK))
		return 0;
	    else
		return port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0) {
	    return port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == fd_data[ready_fd].remain) { /* we're done  */
	    driver_output(port_num, fd_data[ready_fd].buf, 
			  fd_data[ready_fd].sz);
	    clear_fd_data(ready_fd);
	    return 0;
	}
	else { /*  if (res < fd_data[ready_fd].remain) */
	    fd_data[ready_fd].cpos += res;
	    fd_data[ready_fd].remain -= res;
	    return 0;
	}
    }


    if (fd_data[ready_fd].remain == 0) { /* clean fd */
	/* We make one read attempt and see what happens */
	res = read(ready_fd, tmp_buf, tmp_buf_size);
	if (res < 0) {  
	    if ((errno == EINTR) || (errno == ERRNO_BLOCK))
		return 0;
	    return port_inp_failure(port_num, ready_fd, res);
	}
	else if (res == 0)		/* eof */
	    return port_inp_failure(port_num, ready_fd, res);
	else if (res < packet_bytes) { /* Ugly case... get at least */
	    /*
	     * XXX Fix this!!!
	     */
	    if ((h = ensure_header(ready_fd, tmp_buf, packet_bytes, res))==-1)
		return port_inp_failure(port_num, ready_fd, -1);
	    if ((buf = sys_alloc(h)) == NULL) {
		errno = ENOMEM;
		return port_inp_failure(port_num, ready_fd, -1);
	    }
	    fd_data[ready_fd].buf = buf;
	    fd_data[ready_fd].sz = h;
	    fd_data[ready_fd].remain = h;
	    fd_data[ready_fd].cpos = buf;
	    return 0;
	}
	else  { /* if (res >= packet_bytes) */
	    unsigned char* cpos = tmp_buf;
	    int bytes_left = res;
	    while (1) {		/* driver_output as many as possible */
		if (bytes_left == 0) {
		    clear_fd_data(ready_fd);
		    return 0;
		}
		if (bytes_left < packet_bytes) { /* Yet an ugly case */
		    if((h=ensure_header(ready_fd, cpos, 
					packet_bytes, bytes_left))==-1)
			return port_inp_failure(port_num, ready_fd, -1);
		    if ((buf = sys_alloc(h)) == NULL) {
			errno = ENOMEM;
			return port_inp_failure(port_num, ready_fd, -1);
		    }
		    fd_data[ready_fd].buf = buf;
		    fd_data[ready_fd].sz = h;
		    fd_data[ready_fd].remain = h;
		    fd_data[ready_fd].cpos = buf;
		    return 0;
		}
		switch (packet_bytes) {
		case 1: h = get_int8(cpos); cpos += 1; break;
		case 2: h = get_int16(cpos); cpos += 2; break;
		case 4: h = get_int32(cpos); cpos += 4; break;
		default:
		    return -1;
		}
		bytes_left -= packet_bytes;
		/* we've got the header, now check if we've got the data */
		if (h <= (bytes_left)) {
		    driver_output(port_num, (char*) cpos, h);
		    cpos += h;
		    bytes_left -= h;
		    continue;
		}
		else {		/* The last message we got was split */
		    if ((buf = sys_alloc(h)) == NULL) {
			errno = ENOMEM;
			return port_inp_failure(port_num, ready_fd, -1);
		    }
		    sys_memcpy(buf, cpos, bytes_left);
		    fd_data[ready_fd].buf = buf;
		    fd_data[ready_fd].sz = h;
		    fd_data[ready_fd].remain = h - bytes_left;
		    fd_data[ready_fd].cpos = buf + bytes_left;
		    return 0;
		}
	    }
	    return 0;
	}
    }
    ASSERT(0);
    return -1;
}


/* fd is the drv_data that is returned from the */
/* initial start routine                        */
/* ready_fd is the descriptor that is ready to read */

static int ready_output(fd, ready_fd)
long fd;
int ready_fd;
{
    int ix = driver_data[fd].port_num;
    int n;
    struct iovec* iv;
    int vsize;
    

    if ((iv = (struct iovec*) driver_peekq(ix, &vsize)) == NULL) {
	driver_select(ix, ready_fd, DO_WRITE, 0);
	return 0;
    }
    vsize = vsize > MAX_VSIZE ? MAX_VSIZE : vsize;
    if ((n = writev(ready_fd, iv, vsize)) > 0) {
	if (driver_deq(ix, n) == 0)
	    set_busy_port(ix, 0);
    }
    else if (n < 0) {
	if (errno == ERRNO_BLOCK || errno == EINTR)
	    return 0;
	else {
	    int res = errno;
	    driver_select(ix, ready_fd, DO_WRITE, 0);
	    driver_failure_posix(ix, res);
	    return -1;
	}
    }
    return 0;
}

/*
**
*/
#if !defined(USE_SELECT)

int driver_select(int ix, int fd, int mode, int on)
{
    if ((fd < 0) || (fd >= max_files))
	return -1;

    if (on) {
	if (mode & (DO_READ|DO_WRITE)) {
	    int pix = fd_data[fd].pix;  /* index to poll_fds */

	    if (pix < 0) {  /* add new slot */
		max_fd++;   /* FIXME: panic if max_fds >= max_files */

		pix = max_fd;
		fd_data[fd].pix = pix;
	    }
	    poll_fds[pix].fd = fd;
	    if (mode & DO_READ) {
		fd_data[fd].inport = ix;
		poll_fds[pix].events |= POLLIN;
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
	    poll_fds[pix].events &= ~POLLIN;
	    /* Erase POLLIN event from poll result being processed */
	    if ((rix < nof_ready_fds)
		&& (ready_fds[rix].pfd.events & POLLIN)) 
		ready_fds[rix].pfd.revents &= ~POLLIN;
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

#else

/* Interface function available to driver writers */
int driver_select(this_port, fd, mode, on)
int this_port, fd, mode, on;
{
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

#endif

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

static int async_drv_init()
{
    async_fd[0] = -1;
    async_fd[1] = -1;
    return 0;
}

static long async_drv_start(int port_num, char* name, SysDriverOpts* opts)
{
    if (async_fd[0] != -1)
	return -1;
    if (pipe(async_fd) < 0)
	return -1;

    DEBUGF(("async_drv_start: %d\r\n", port_num));

    driver_select(port_num, async_fd[0], DO_READ, 1);

    if (init_async(async_fd[1]) < 0)
	return -1;
    return port_num;
}


static int async_drv_stop(int port_num)
{
    DEBUGF(("async_drv_stop: %d\r\n", port_num));

    exit_async();

    driver_select(port_num, async_fd[0], DO_READ, 0);

    close(async_fd[0]);
    close(async_fd[1]);
    async_fd[0] = async_fd[1] = -1;
    return 0;
}


static int async_drv_input(int port_num, int fd)  
{
    char buf[1];

    DEBUGF(("async_drv_input\r\n"));
    read(fd, buf, 1);     /* fd MUST be async_fd[0] */
    check_async_ready();  /* invoke all async_ready */
    return 0;
}

#endif


/* Lifted out from check_io() */
static void do_break_handling()
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

static void check_io(do_wait)
int do_wait;
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

	if (revents & (POLLIN|POLLOUT)) {
	    if (revents & POLLOUT)
		output_ready(qp->oport, fd);
	    if (revents & (POLLIN|POLLHUP))
		input_ready(qp->iport, fd);
	}
	else if (revents & (POLLERR|POLLHUP)) {
	    /* let the driver handle the error condition */
	    if (qp->pfd.events & POLLIN)
		input_ready(qp->iport, fd);
	    else
		output_ready(qp->oport, fd);
	}
	else if (revents & POLLNVAL) {
	    if (qp->pfd.events & POLLIN) {
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
	    if (qp->pfd.events & POLLIN)
		driver_select(qp->iport, fd, DO_READ|DO_WRITE, 0);
	    if (qp->pfd.events & POLLOUT) {
		if (!(qp->pfd.events & POLLIN) || (qp->iport != qp->oport))
		    driver_select(qp->oport, fd, DO_READ|DO_WRITE, 0);
	    }
	}
    }
}

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
    char *env = safe_alloc(strlen(buffer)+1);
    strcpy(env,buffer);
    return(putenv(env));
}

void sys_init_io(buf, size)
byte *buf;
uint32 size;
{
    tmp_buf = buf;
    tmp_buf_size = size;
    cerr_pos = 0;

    if ((fd_data = (struct fd_data *)
	 sys_alloc_from(180,max_files * sizeof(struct fd_data))) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n",
		 max_files * sizeof(struct fd_data));

#ifdef USE_SELECT
    FD_ZERO(&input_fds);
    FD_ZERO(&output_fds);

#else
    poll_fds =  (struct pollfd *)
	sys_alloc_from(182, max_files * sizeof(struct pollfd));
    ready_fds =  (struct readyfd *)
	sys_alloc_from(182, max_files * sizeof(struct readyfd));

    if (poll_fds == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n",
		 max_files * sizeof(struct pollfd));

    if (ready_fds == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n",
		 max_files * sizeof(struct readyfd));

    {
	int i;

	for (i=0; i < max_files; i++) {
	    poll_fds[i].fd      = -1;
	    poll_fds[i].events  = 0;
	    poll_fds[i].revents = 0;

	    fd_data[i].pix = -1;
	}
    }
#endif

    max_fd = -1;

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


/* Fill buffer, return buffer length, 0 for EOF, < 0 for error except EINTR. */
static int read_fill(fd, buf, len)
int fd, len;
char *buf;
{
    int i, got = 0;

    do {
	if ((i = read(fd, buf+got, len-got)) <= 0) {
	    if (i == 0 || errno != EINTR)
		return (i);
	    i = 0;
	}
	got += i;
    } while (got < len);
    return (len);
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

/* If we are not using elib then we may want to defined
** the functions 
** sys_alloc
** sys_realloc
** sys_free
*/
#ifndef USE_ELIB

/* We do not define the functions if they are
** macros
*/
#ifndef sys_alloc2

#ifdef DEBUG
int tot_allocated = 0;
#endif

/* 
 * !!!!!!!  Allocated blocks MUST be aligned correctly !!!!!!!!!
 */
#define MEM_BEFORE 0xABCDEF97

#define MEM_AFTER1  0xBA
#define MEM_AFTER2  0xDC
#define MEM_AFTER3  0xFE
#define MEM_AFTER4  0x77

#define SIZEOF_AFTER 4

#define SET_AFTER(p) \
  (p)[0] = MEM_AFTER1; (p)[1] = MEM_AFTER2; \
  (p)[2] = MEM_AFTER3; (p)[3] = MEM_AFTER4

#define CLEAR_AFTER(p) \
  (p)[0] = 0; (p)[1] = 0; \
  (p)[2] = 0; (p)[3] = 0

#define CHECK_AFTER(p) \
  ((p)[0] == MEM_AFTER1 && (p)[1] == MEM_AFTER2 && \
  (p)[2] == MEM_AFTER3 && (p)[3] == MEM_AFTER4)
  
typedef union most_strict {
    double x;
    long y;
} Most_strict;

typedef struct memory_guard {
    unsigned pattern;		/* Fence pattern. */
    unsigned size;		/* Size of allocated memory block. */
    Most_strict align;		/* Ensure proper alignment. */
} Memory_guard;

void* sys_alloc2(size)
unsigned int size;
{
    register unsigned char* p;

#ifdef DEBUG
    size += sizeof(Memory_guard) + SIZEOF_AFTER;
#endif

    p = (unsigned char *) malloc(size);

#ifdef DEBUG
    if (p != NULL) {
	Memory_guard* before = (Memory_guard *) p;
	unsigned char* after;

	p += sizeof(Memory_guard);
	before->pattern = MEM_BEFORE;
	before->size = size - sizeof(Memory_guard) - SIZEOF_AFTER;
	after = p + before->size;
	SET_AFTER(after);
	tot_allocated += before->size;
    }
#endif
    return p;
}

void* sys_realloc2(ptr, size)
void* ptr; unsigned int size;
{
    register unsigned char* p;

#ifdef DEBUG
    unsigned old_size;
    Memory_guard* before;
    unsigned char* after;

    p = (unsigned char *) ptr;
    before = (Memory_guard *) (p-sizeof(Memory_guard));
    if (before->pattern != MEM_BEFORE) {
	erl_exit(1, "realloc: Fence before memory at 0x%p clobbered\n",
		 ptr);
    }
    old_size = before->size;
    after = p+old_size;
    if (!CHECK_AFTER(after)) {
	erl_exit(1, "realloc: Fence after memory at 0x%p (size %d) clobbered\n",
		 ptr, size);
    }
    ptr = ((unsigned char*) ptr) - sizeof(Memory_guard);
    size += sizeof(Memory_guard) + SIZEOF_AFTER;
#endif
    p = realloc(ptr, size);
#ifdef DEBUG
    if (p != NULL) {
	before = (Memory_guard *) p;
	before->size = size-sizeof(Memory_guard)-SIZEOF_AFTER;
	p += sizeof(Memory_guard);
	after = p + before->size;
	SET_AFTER(after);
	tot_allocated += (before->size - old_size);
    }
#endif

    return p;
}

void sys_free2(ptr)
void* ptr;
{
#ifdef DEBUG
    unsigned size;
    register unsigned char* p;
    Memory_guard* before;
    unsigned char* after;

    if (ptr) {
      p = (unsigned char *) ptr;
      before = (Memory_guard *) (p-sizeof(Memory_guard));
    
      if (before->pattern != MEM_BEFORE) {
	erl_exit(1, "free: Fence before %p clobbered\n", ptr);
      }
      size = before->size;
      after = p + size;
      if (!CHECK_AFTER(after)) {
	erl_exit(1, "free: Fence after block 0x%p of size %d clobbered\n",
		 ptr, size);
      }
      CLEAR_AFTER(after);
      before->pattern = 0;
      before->size = 0;
      tot_allocated -= size;
      ptr = ((unsigned char*) ptr) - sizeof(Memory_guard);
    }
#endif
    free(ptr);
}
#endif

#endif

/* Float conversion */

int sys_chars_to_double(buf, fp)
char* buf; double* fp;
{
    char *s = buf;

    /* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

    /* Robert says that something like this is what he really wanted:
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9].%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */

    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-')
      s++;
	    
    if (!ISDIGIT(*s))		/* Leading digits. */
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s++ != '.')		/* Decimal part. */
      return -1;
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-')
	  s++;
	if (!ISDIGIT(*s))
	  return -1;
	while (ISDIGIT(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;

#if defined(HP9000) || defined(ISC32)
    errno = 0;
#endif
    *fp = atof(buf);
#ifdef DEBUG
    if (errno == ERANGE)
	fprintf(stderr, "errno = ERANGE in list_to_float\n\r");
#endif

#if defined(HP9000) || defined(ISC32)
    if (errno == ERANGE)
	return -1;
/*
**  Replaces following code:
**   if (errno == ERANGE) {
**       *fp = 1.2e300;		
**       *fp = *fp / 1.5e-100;	
**   }				
*/

#endif
    return 0;
}


/* 
** Convert a double to ascii format 0.dddde[+|-]ddd
** return number of characters converted
*/

int sys_double_to_chars(fp, buf)
double fp; char* buf;
{
    (void) sprintf(buf, "%.20e", fp);
    return strlen(buf);
}

#ifdef USE_MATHERR
int matherr(exc)
struct exception *exc;
{
    erl_fp_exception++;
    DEBUGF(("FP exception (matherr) (0x%x) (%d)\r\n", exc->type, erl_fp_exception));
    return 1;
}
#endif

#ifndef NO_FPE_SIGNALS

#ifdef HAVE_IEEE_HANDLER

#include <floatingpoint.h>

static void init_fpe_handler()
{
    erl_fp_exception=0;
    ieee_handler("set", "division",  (sigfpe_handler_type)fpe_sig_handler);
    ieee_handler("set", "underflow",  (sigfpe_handler_type)fpe_sig_handler);
    ieee_handler("set", "overflow",  (sigfpe_handler_type)fpe_sig_handler);
    ieee_handler("set", "invalid",  (sigfpe_handler_type)fpe_sig_handler);
}

void fpe_sig_handler(sig, code, scp, addr)
int sig, code;
struct sigcontext *scp;
char *addr;
{
    erl_fp_exception++;
    DEBUGF(("FP exception (0x%x) (%d)\n\r", code, erl_fp_exception));
}

#endif /* HAVE_IEEE_HANDLER */

#endif /* NO_FPE_SIGNALS */

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

#ifndef __STDC__

void sys_printf(va_alist)
    va_dcl
{
    va_list va;
    CIO where;
    char   *format;
    va_start(va);
    where = va_arg(va, CIO);
    format = va_arg(va, char *);
#else
void sys_printf(CIO where, char* format, ...)
{
    va_list va;
    va_start(va,format);
#endif

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
	p = fd_data[i].pending;
	while(p) {
	    fprintf(stderr, "  Pending output %d bytes on %d\n", p->remain,i);
	    p = p->next;
	}
    }
}
#endif /* (0) */


void
erl_assert_error(char* expr, char* file, int line)
{   
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
    erl_crash_dump(NULL, NULL);
    abort();
}

#ifdef __STDC__
void
erl_debug(char* fmt, ...)
#else
void
erl_debug(fmt, va_alist)
char* fmt;
va_dcl
#endif /* __STDC__ */
{
    char sbuf[1024];		/* Temporary buffer. */
    va_list va;
    
    if (debug_log) {
	VA_START(va, fmt);
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
	if(erts_thread_sigwait(&chldsigset, &sig) < 0) {
	  ASSERT(errno == EINTR);
	}
	else {
	  ASSERT(sig == SIGCHLD);
	}
      }
    } while(pid < 0);
    erts_mutex_lock(dead_child_status_lock);
    note_child_death(pid, status);
    erts_mutex_unlock(dead_child_status_lock);
  }

  return NULL;
}

#define check_children()

#else

static void check_children()
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

void
erl_sys_schedule_loop(void)
{
    for (;;) {
	while (schedule()) {
	    check_io(0);	/* Poll for I/O. */
	    check_async_ready(); /* Check async compleations */
	    check_children();
	}
	if (check_async_ready())
	    check_io(0);
	else
	    check_io(1);	/* Wait for I/O or a timeout. */
	check_children();
    }
}

void
erl_sys_args(argc, argv)
int* argc;
char** argv;
{
    ;
}
