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


#include "global.h" 
#include "bif.h"
#include "elib_stat.h"

#include "ose.h"
#include "malloc.h"
#include "outfmt.h"
#include "efs.h"
#include "unistd.h"
#include "fm.sig"
#include "prh.sig"

#include "erl_sys_driver.h"
#include "erl_port_signals.sig"

#include <termios.h>

/* Solaris headers, only to be used with SFK */
#ifdef _OSE_SFK_
#include <ctype.h>
#include <string.h>
#endif

#ifndef RETSIGTYPE
#define RETSIGTYPE void
#endif

#define WANT_NONBLOCKING
#include "sys.h"

#define MAX_FILES() 256		/* how to find in OSE!? */

EXTERN_FUNCTION(void, erl_exit, (int n, char*, _DOTS_));
EXTERN_FUNCTION(void, erl_error, (char*, va_list));
EXTERN_FUNCTION(void, input_ready, (int, int));
EXTERN_FUNCTION(void, output_ready, (int, int));
EXTERN_FUNCTION(int, driver_interrupt, (int, int));
EXTERN_FUNCTION(void, increment_time, (int));
EXTERN_FUNCTION(int, next_time, (_VOID_));
EXTERN_FUNCTION(int, erl_dbg_fputc, (char, FILE*));
EXTERN_FUNCTION(int, erl_dbg_vfprintf, (FILE*, char*, va_list));
EXTERN_FUNCTION(int, erl_dbg_fprintf, (FILE*, char*, ...));

extern Uint erts_max_ports;
extern PROCESS erl_block;

#define NULLTV  ((struct timeval *) 0)
#define NULLFDS ((struct fd_set *) 0)

/* forward declarations */
static FUNCTION(void, check_io, (int));
static FUNCTION(void, initialize_allocation, (void));
static FUNCTION(void, start_pgm_server, (void));
FUNCTION(int,  get_pgm_data, (char*, void**, void**, int*));
 
#define DEFAULT_PORT_STACK_SIZE  4095
static int port_stack_size;
static int max_files = 50;	/* default configAll.h */

static PROCESS stdin_pid_;

/* used by the break handler (set by signal handler on ctrl-c) */
static volatile int break_requested = 0;
static int ignore_break = 0;

typedef struct port_entry_ {
  HashBucket hash;
  PROCESS pid_;
  int port;
  int state;
} port_entry;

#define PID2PORT_INIT_SZ 16

HashFunctions pt_funs;
static Hash pt;

/* callbacks for hash.c */
static HashValue pt_hash(void *);
static int pt_cmp(void *, void *);
static void *pt_alloc(void *);
static void pt_free(void *);

#ifdef DEBUG
static int debug_log = 1;
#endif

int using_oldshell = 1; 

#ifdef TRACE_OSE_SIG_ALLOC
static Uint ose_sig_allocated;
static Uint ose_sig_allocated_bytes;
static Uint ose_sig_freed;
#endif

/********************* General functions ****************************/

void erts_sys_alloc_init(void) {
  /* initialize heap memory handler */
  initialize_allocation();
}

Eterm erts_check_io_info(void *proc)
{
    return NIL;
}

Uint
erts_sys_misc_mem_sz(void)
{
    return (Uint) 0 /* FIXME */;
}

void
erts_sys_pre_init(void)
{

}

void erl_sys_init(void)
{
  break_requested = 0;

  if ((max_files = MAX_FILES()) < 0)
    erl_exit(1, "Can't get no. of available file descriptors\n");

#ifdef TRACE_OSE_SIG_ALLOC
  ose_sig_allocated = 0;
  ose_sig_allocated_bytes = 0;
  ose_sig_freed = 0;
#endif

  /* no stdio buffering */
  setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);

  pt_funs.hash = (H_FUN) pt_hash;
  pt_funs.cmp = (HCMP_FUN) pt_cmp;
  pt_funs.alloc = (HALLOC_FUN) pt_alloc;
  pt_funs.free = (HFREE_FUN) pt_free;

  hash_init(ERTS_ALC_T_PRT_TAB, &pt, "pid2port", PID2PORT_INIT_SZ, pt_funs);
}

void erl_sys_init_final(void)
{
  /* server for registering (user) port programs and drivers */
  start_pgm_server();
}

/*
 * Called from schedule() when it runs out of runnable processes,
 * or when Erlang code has performed INPUT_REDUCTIONS reduction
 * steps. runnable == 0 iff there are no runnable Erlang processes.
 */
void erl_sys_schedule(int runnable)
{
    if (runnable) {
	check_io(0);		/* Poll for I/O */
    } else {
	check_io(1);
    }
}

int sys_max_files(void) 
{
  return(max_files);
}


/************************* Break handling ******************************/

void erts_set_ignore_break(void) {
  ignore_break = 1;
}

/* remote break request, makes it possible to signal a break from 
   outside the emulator */
void remote_request_break(void) {
#ifdef DEBUG
  fprintf(stderr,"remote break!\n");
#endif
  break_requested++;
}

/* signal break */
static void request_break(void)
{
  /* just set a flag - checked for and handled in main process,
     see check_io() */
#ifdef DEBUG
  fprintf(stderr,"break!\n");
#endif
  if (break_requested > 0)
     erl_exit(0, "");

  break_requested = 1;

}

static void break_handling() {
  extern void do_break();

  if (break_requested > 1)
    erl_exit(0, "");
  break_requested = 0;
  /* call the break handling function, reset flag */
  do_break();
  /* the stdin process has suspended itself, resume execution now */
  start(stdin_pid_);
}


/******************* Routines for time measurement *********************/

static unsigned long wrapped;
static unsigned long last_ticks;
#define MAX_TICKS 0xFFFFFFFFUL	    /* 2e(sizeof(OSTICK)) */
static unsigned long tick_interval; /* value of SYSTEM_TIMER, but in nanosecs */
int sys_clk_tck;		    /* value of SYS_CLK_TCK (ticks/sec) */

int sys_init_time(void) 
{
  sys_clk_tck = 1000000UL / system_tick();
  return SYS_CLOCK_RESOLUTION;
}

int sys_init_hrtime(void)
{
  wrapped = 0;
  tick_interval = system_tick() * 1000;
  last_ticks = get_ticks();
  return 0;
}

void sys_gettimeofday(SysTimeval *tvp)
{
  struct timezone z; 

  gettimeofday(tvp, &z);
}

SysHrTime sys_gethrtime(void) 
{
  unsigned long ticks;

  ticks = get_ticks();

  if (ticks < last_ticks)
    ticks += (MAX_TICKS - last_ticks);

  return (SysHrTime)ticks * (SysHrTime)tick_interval;
}
    
clock_t sys_times(SysTimes *t)
{
    t->tms_stime = t->tms_cutime = t->tms_cstime = 0;
    t->tms_utime = 0;		/* what's user time!? */
    return system_tick(); 
}


/************************** OS info *******************************/

#define MAX_VER_STR 9           /* Number of characters to
                                   consider in version string */  
#define MAX_SYS_STR 50		/* Max no of characters in system info string */

static FUNCTION(int, get_number, (char** str_ptr));

char os_type[] = "ose";

/* use Solaris lib for now 
   static int isdigit(char *s) {
   int i;
  #define ISDIGIT(d) ((d) >= '0' && (d) <= '9')
  for(i=0; i<strlen(s); i++)
  if(!ISDIGIT(s[i])) return 0;
    return 1;
  }
*/

static int
get_number(char **str_ptr)
{
    char* s = *str_ptr;		/* Pointer to beginning of string. */
    char* dot;			/* Pointer to dot in string or NULL. */

    if (!isdigit(*s))		/* gcc: warning: subscript has type `char' */
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

static int
split_os_info(char *os_info, char *sysname, char *version)
{
  char cur;
  int i = 0, n = 0;

  while((cur = os_info[i]) != '/')
    sysname[i++] = cur;
  sysname[i] = '\0';
  if(os_info[++i] == 'R') i++;
  while((os_info[i] != '/') && (os_info[i] != '\0'))
    version[n++] = os_info[i++];
  version[n] = '\0';
  return 0;
}

/* namebuf;     Where to return the name. */
/* size;	Size of name buffer. */
void
os_flavor(char *namebuf, unsigned size)
{
    char *os_info;		/* Pointer to: <OSE_product>/<revision>/<other_info> */
    char release[MAX_VER_STR+3]; /* dummy */

    os_info = get_cpu(current_process());
    split_os_info(os_info, namebuf, release);
    free_buf((union SIGNAL **)&os_info);
}

/* int* pMajor;			Pointer to major version. */
/* int* pMinor;			Pointer to minor version. */
/* int* pBuild;			Pointer to build number. */
void
os_version(int *pMajor, int *pMinor, int *pBuild)
{
    char *os_info;		/* Pointer to: <OSE_product>/<revision>/<other_info> */
    char *release;		/* Pointer to the release string: X.Y or X.Y.Z. */
    char sysname[MAX_SYS_STR+1]; /* dummy */

    release = erts_alloc(ERTS_ALC_T_TMP, MAX_VER_STR+3);

    os_info = get_cpu(current_process());
    split_os_info(os_info, sysname, release);

    *pMajor = get_number(&release);
    *pMinor = get_number(&release);
    *pBuild = get_number(&release);

    erts_free(ERTS_ALC_T_TMP, release);
    free_buf((union SIGNAL **)&os_info);
}

typedef struct getenv_state {int c; char* p; char** e; char* prev} GetEnvState;

void init_getenv_state(GETENV_STATE *state) {
  char *envstr;
  int len, c=0;
  int getenv_sz = 0;
  char **environment = NULL;
  GetEnvState *s;
  
  s = (GetEnvState *) erts_alloc(ERTS_ALC_T_GETENV_STATE,
				 sizeof(GetEnvState));
  envstr = get_env_list(get_bid(current_process()), NULL); /* get 1st string of vars */
  while((len = strlen(envstr)) > 0) {
    if (c >= getenv_sz) {
	getenv_sz += 10;
	environment = (char **) erts_realloc(ERTS_ALC_T_GETENV_STATE,
					     (void *) environment,
					     getenv_sz*sizeof(char*));
    }
    environment[c] = erts_alloc(ERTS_ALC_T_GETENV_STR, len+1);
    strcpy(environment[c], envstr);
    free_buf((union SIGNAL **)&envstr);
    while((environment[c][len] != 32) && len > 0) len--; /* find last var in string */
    if(len > 0) len += 1;
    envstr = get_env_list(get_bid(current_process()), /* get next string */
			  (environment[c])+len); 
    c++;
	
  }
  free_buf((union SIGNAL **)&envstr);
  environment[c] = NULL;
  s->c = 0; s->p = environment[0]; s->e = environment; s->prev = NULL;
  *state = (GETENV_STATE)s; 
}

/* Before building and returning a new "var=value" string, we free
   the string from the previous call. We assume this function will
   always be called for all variables (i.e. called until it returns 0. */
char *getenv_string(GETENV_STATE *state0) {
  char *next, *value;
  char* env = NULL;
  GetEnvState *s = (GetEnvState *)*state0;

  if(s->prev != NULL) erts_free(ERTS_ALC_T_GETENV_STR, s->prev);

  while(1) {
    if((s->e)[s->c] == NULL) {	
      /* last string processed, clean up */
      erts_free(ERTS_ALC_T_GETENV_STATE, s->e);
      erts_free(ERTS_ALC_T_GETENV_STATE, s);
      return NULL;
    }
    /* if next==NULL, we're at the last var in string */
    if((next = strchr(s->p, 32)) != NULL) 
      *next = '\0';
    if((value = get_env(get_bid(current_process()), s->p)) != NULL) {
      env = erts_alloc(ERTS_ALC_T_GETENV_STR, strlen(s->p)+strlen(value)+2);
      strcpy(env, s->p);
      strcat(env, "=");
      strcat(env, value);
      free_buf((union SIGNAL **)&value);
    }
    if(next == NULL) {	/* done with this str? if so, try next */
      erts_free(ERTS_ALC_T_GETENV_STR, (s->e)[s->c]);
      (s->c)++; s->p = (s->e)[s->c];
    } else {
      s->p = (char*)((unsigned int)next + 1);
    }
    if(env != NULL) {
      s->prev = env;
      return env;
    }
  }
}
    
void fini_getenv_state(GETENV_STATE *state)
{
   *state = NULL;
}
      
/************************** Port I/O *******************************/

#define DRV_RECV      0
#define DRV_NOT_RECV -1
#define DRV_STARTED  -2
#define DRV_STOPPED  -3

#define TMP_BUF_MAX (tmp_buf_size - 1024)

static int process_counter = 0;

typedef struct sig_entry {
  ErlDrvPort port;
  void *sig;
  struct sig_entry *next_sig;
} SigEntry;

static SigEntry *sigQ_1st = NULL;  /* local queue of signals to be processed */
static SigEntry *sigQ_last = NULL;
static SigEntry *sig_buf_1st = NULL; /* buffer of pending signals */
static SigEntry *sig_buf_last = NULL;

static byte *tmp_buf;
static Uint tmp_buf_size;


/*-------------------------- Drivers -----------------------------*/

struct driver_data_t {
  int port_num;
  int packet_bytes;
  int report_exit;
  PROCESS pid_;
  PROCESS sec_pid_;
  int status;
  int exitcode; 
  int exit_reported;
};

/* common driver interface */
static void stop_driver(ErlDrvData);

/* fd driver interface */
static ErlDrvData fd_start(ErlDrvPort, char*, SysDriverOpts*);
static void fd_ready_input(ErlDrvData, ErlDrvEvent);
static void fd_output(ErlDrvData, char*, int);

struct erl_drv_entry fd_driver_entry = {
    NULL,
    fd_start,
    stop_driver,
    fd_output,
    fd_ready_input,
    NULL,
    "fd",
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
};

/* spawn driver interface */
static ErlDrvData spawn_start(ErlDrvPort, char*, SysDriverOpts*);
static int spawn_init(void);
static void ready_input(ErlDrvData, ErlDrvEvent);
static void output(ErlDrvData, char*, int);

struct erl_drv_entry spawn_driver_entry = {
  spawn_init,			/* init */
  spawn_start,			/* port opened */
  stop_driver,			/* port closed */
  output,			/* data from erlang */
  ready_input,			/* data in message queue */
  NULL,				/* ready to send, not used */
  "spawn",
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL
};


union portSig {
  SIGSELECT sig_no;
  struct PortData port_data;
};

static struct driver_data_t* set_driver_data(port_num,
					     packet_bytes,
					     exit_status,
					     pid_, sec_pid_)
int port_num, packet_bytes, exit_status; 
PROCESS pid_, sec_pid_;
{
  struct driver_data_t* driver_data;

  driver_data = erts_alloc_fnf(ERTS_ALC_T_DRIVER_DATA,
			       sizeof(struct driver_data_t));
  if(!driver_data) {
    fprintf(stderr, "Error: Not enough memory for driver! Port=%d\n", port_num);
    return NULL;
  }

  driver_data->packet_bytes = packet_bytes;
  driver_data->port_num = port_num;
  driver_data->report_exit = exit_status;
  driver_data->pid_ = pid_;
  driver_data->sec_pid_ = sec_pid_;

  driver_select(port_num, pid_, DO_START, 1);
  driver_select(port_num, pid_, DO_READ, 1);

  if(sec_pid_ != 0) {
    driver_select(port_num, sec_pid_, DO_START, 1);
    driver_select(port_num, sec_pid_, DO_READ, 1);
  }
  return driver_data;
}

static int port_inp_failure(struct driver_data_t *dd, int res)
{
  /* res: 0 (eof) or -1 (error) */

  driver_select(dd->port_num, dd->pid_, DO_READ, 0); 
  if (res == 0) {
    if (dd->report_exit)
      driver_report_exit(dd->port_num, 0);
    driver_failure_eof(dd->port_num);
  } else {
    driver_failure_atom(dd->port_num, "port_exit");
  }
  return 0;
}

/*** common driver interface ***/

/* To stop the port process, send an attach signal to it. A properly
   written port process should detect this and shut down. We shouldn't
   use kill_proc, since the process might be in a sensitive state. */

static void stop_driver(ErlDrvData driver_data)
{
  struct driver_data_t* dd = (struct driver_data_t *)driver_data;
  union AttachSig *sig;

  sig = (union AttachSig *)alloc(sizeof(struct PortData), OS_ATTACH_SIG);
  send((union SIGNAL **)&sig, dd->pid_);
  driver_select(dd->port_num, dd->pid_, DO_STOP, 1);
  process_counter--;

  if(dd->sec_pid_ != 0) {
    sig = (union AttachSig *)alloc(sizeof(struct PortData), OS_ATTACH_SIG);
    send((union SIGNAL **)&sig, dd->pid_);
    driver_select(dd->port_num, dd->sec_pid_, DO_STOP, 1);
    process_counter--;
  }
  erts_free(ERTS_ALC_T_DRIVER_DATA, dd);		
}

/*** fd driver implementation ***/

#define FD_SIG_NO  1001
#define ACK_SIG_NO 1002

struct fdStruct {
  SIGSELECT sig_no;
  int fdout;
  int fdin;
  PROCESS in_pid_;
};

struct ackStruct {
  SIGSELECT sig_no;
};

union fdSig {
  SIGSELECT sig_no;
  struct fdStruct fds;
};

union ackSig {
  SIGSELECT sig_no;
};

void send_ack(PROCESS pid_) {
  union ackSig *sig;
  sig = (union ackSig *)ose_sig_alloc(sizeof(struct ackStruct), ACK_SIG_NO);
  send((union SIGNAL **)&sig, pid_);
}

void recv_ack() {
  union ackSig *sig;
  static const SIGSELECT recv_ack_sig[2] = {1, ACK_SIG_NO};
  sig = (union ackSig *)receive((SIGSELECT*)recv_ack_sig);
  ose_sig_free_buf((union SIGNAL **)&sig);
}

/* This process will handle general fd output, print received buffer
   to fdout. */
OS_PROCESS(fdout_process) {
  union fdSig *fd_sig;
  union portSig *port_sig;
  static const SIGSELECT recv_fd_sig[2] = {1, FD_SIG_NO};
  /* static const SIGSELECT recv_port_sig[2] = {1, PORT_DATA}; */
  static const SIGSELECT recv_any_sig[]   = {0};
  int fdout;
  char *buf;
  int len, pos, res;
  PROCESS erts_;
  PROCESS in_pid_;

#ifdef DEBUG
  printf("fdout process started (%li)\n\n", (unsigned long)current_process());
#endif

  /* get descriptor */
  fd_sig = (union fdSig *)receive((SIGSELECT*)recv_fd_sig);
  fdout = fd_sig->fds.fdout;
  in_pid_ = fd_sig->fds.in_pid_;
  erts_ = sender((union SIGNAL **)&fd_sig);
  ose_sig_free_buf((union SIGNAL **)&fd_sig);

  /* Attach to erts so that an OS_ATTACH_SIG is received when
     erlang is shut down. When this happens, also kill the input 
     process which is stuck in read. */
  attach(NULL, erts_);

  /* wait for something to print out */
  while(1) {
#ifdef DEBUG
    printf("Waiting to print to %d...\n\n", fdout);
#endif

    port_sig = (union portSig *)receive((SIGSELECT*)recv_any_sig);

    if(port_sig->sig_no == OS_ATTACH_SIG) {
#ifdef DEBUG
      printf("ERTS has terminated, cleaning up... ");
#endif
      ose_sig_free_buf((union SIGNAL **)&port_sig);
      kill_proc(in_pid_);
      kill_proc(current_process());
#ifdef DEBUG
      printf("done !\n");
#endif      
    }

    len = port_sig->port_data.len;
    buf = port_sig->port_data.buf;

#ifdef DEBUG
    printf("Will print \"%s\" (%d bytes) to %d!\n\n", buf, len, fdout);
#endif

    /* If we print to stdout, we should use fprintf rather than write. The reason 
       is that the printouts may be directed to the serial port and the device
       driver (ose_serdd) cannot write buffers larger than 1024 bytes. (fprintf
       prints line-by-line, 80 bytes at a time). */

    if((fdout != fileno(stdout)) && (fdout != fileno(stderr))) {
      pos = 0;
      while(pos < len) {
	if((res = write(fdout, (void *)(buf+pos), len-pos)) != -1) {
	  pos+=res;
	} else {
	  if((errno != EINTR) && (errno != EAGAIN)) { /* fatal */
	    fprintf(stderr, "IO error! Cannot write %d bytes to %d. Errno: %d\n", 
		    len-pos, fdout, errno);
	    pos = len;
	  }
	}
      }
    } else {			/* printing to stdout or stderr */
      FILE *stream;
      if(fdout == fileno(stdout)) stream = stdout; else stream = stderr;
      if(fprintf(stream, "%s", buf) == EOF) 
	fprintf(stderr, "IO error! Cannot write %d bytes to %d. Errno: %d\n", 
		len, fdout, errno);
      fflush(stream);
    }
    
    /* this must not be called until operation is finished with buf */
    ose_sig_free_buf((union SIGNAL **)&port_sig);
    /* driver (erts_) waits for print operation to finish */
    send_ack(erts_);
  }
}
   
static Boolean 
set_term_flags(int fd, EfsBitmask flags) 
{ 
  return efs_config_term(fd, FM_TERM_FLAGS, flags, 0) == EFS_SUCCESS; 
} 

/* This process will wait for incoming bytes on fdin. The data is
   passed on to erts as a signal. */
OS_PROCESS(fdin_process) {
  union fdSig *fd_sig;
  union portSig *port_sig;
  static const SIGSELECT recv_fd_sig[2] = {1, FD_SIG_NO};
  static const SIGSELECT recv_port_sig[2] = {1, PORT_DATA};
  int fdin, result, max_sig_buf_size;
  PROCESS erts_;
  unsigned char keys[FM_TCC_SIZE];
  unsigned char intr;
  EfsBitmask old_flags;
  char *env;

#ifdef DEBUG
  printf("fdin process started (%li)\n\n", (unsigned long)current_process());
#endif

  /* get descriptor */
  fd_sig = (union fdSig *)receive((SIGSELECT*)recv_fd_sig);
  fdin = fd_sig->fds.fdin;
  erts_ = sender((union SIGNAL **)&fd_sig);
  ose_sig_free_buf((union SIGNAL **)&fd_sig);

  /* check the start mode and suspend the process if noshell is set */
  /* note: to start interactive mode later, simply activate this process again */
  if((env = getenv("ERL_START_MODE")) != NULL) { /* if not def, assume interactive */
    if(strcmp(env, "noshell") == 0) {
#ifdef DEBUG
      printf("noshell mode, suspending fd_in\n");
#endif
      stop(current_process());
    }
    free_buf((union SIGNAL **) &env);
  }

  max_sig_buf_size = sizeof(struct PortData)+tmp_buf_size;

  while(1) {
    int cursor=0, done=0;
    char ch;

    port_sig = (union portSig *)ose_sig_alloc(max_sig_buf_size, PORT_DATA);

    /* If we're reading from stdin, we need to set the terminal mode to raw
       and handle each character individually to detect a control character.
       Note that since ^C is reserved by ose_shell, we use ^X instead to
       signal an emulator break. */
    if(fileno(stdin) == fdin) {	/* stdin */
      memset(port_sig->port_data.buf, '\0', sizeof(char));
      /* get terminal flags */
      if(efs_examine_term(fdin, FM_TERM_FLAGS, &old_flags, 0) == EFS_SUCCESS) {
	/* switch off line edit and ctrl character echo */
	EfsBitmask new_flags =  ~FM_TIFL_LINE_EDIT & ~FM_TIFL_ECHO_CTRL & (old_flags | FM_TIFL_ECHO);
	if(efs_config_term(fdin, FM_TERM_FLAGS, new_flags, 0) == EFS_SUCCESS) {
	  /* disable ^C for OSE */
	  if(efs_examine_term(fdin, FM_TERM_CTRLCHARS, keys, 0) ==  EFS_SUCCESS) {
	    intr = keys[FM_TCC_INTR];
	    keys[FM_TCC_INTR] = FM_TCC_DISABLED; 
	    if(efs_config_term(fdin, FM_TERM_CTRLCHARS, keys, 0) != EFS_SUCCESS)
	      fprintf(stderr, "efs_config_term failed\n");
	  }
	  else 
	    fprintf(stderr, "efs_examine_term failed\n");

	  /* read characters from stdin (until NL or ctrl character) */
	  while((cursor < max_sig_buf_size) && !done) {
	    if((result = read(fdin, &ch, 1)) == 1) {
	      switch(ch) {
	      case 1:		/* ^A (beginning of line) */
	      case 2:		/* ^B (char back) */
	      case 5:		/* ^E (end of line) */
	      case 6:		/* ^F (char fwd)  */
	      case 0x7F:	/* del */
		break;
	      case 7:		/* ^G (new shell) */
		fprintf(stdout, " <abort>\n");
		port_sig->port_data.buf[0] = 7;
		cursor = 1; done = 1;
		break;
	      case 8:		/* backspace */
		if(cursor > 0) {
		  port_sig->port_data.buf[--cursor] = '\0';
		}
		break;
	      case 0xD:
		fprintf(stdout, "\n"); /* '\n' not echoed by terminal */
		port_sig->port_data.buf[cursor++] = ch;
		done = 1;
		break;
	      case 0x3:		/* ^C (break) */
		if(!ignore_break) {
		  fprintf(stdout, " <break>\n"); /* echo ctrl character */
		  cursor = 0; done = 1;
		} else {
		  strcpy(port_sig->port_data.buf, "break_ignored.\n");
		  cursor = 15; done = 1;
		}
		break;
	      case 0x12:	/*  ^R (new prompt) */
		fprintf(stdout, " <ignore>\n");
		strcpy(port_sig->port_data.buf, "ok.\n");
		cursor = 4; done = 1;
		break;
	      case 0x18:	/* ^X (break) */
		if(!ignore_break) {
		  fprintf(stdout, " <break>\n"); /* echo ctrl character */
		  cursor = 0; done = 1;
		} else {
		  strcpy(port_sig->port_data.buf, "break_ignored.\n");
		  cursor = 15; done = 1;
		}
		break;
	      default:
		port_sig->port_data.buf[cursor++] = ch;
	      }
	    } else {
	      fprintf(stderr, "Failed to read char! Errno = %d\n", errno);
	      cursor = -1; done = 1;
	    } 
	  }
	  /* reset terminal settings */
	  efs_config_term(fdin, FM_TERM_FLAGS, old_flags, 0);
	  keys[FM_TCC_INTR] = intr;
	  efs_config_term(fdin, FM_TERM_CTRLCHARS, keys, 0);
	} else {
	  fprintf(stderr, "Can't set term flags!\n");
	  cursor = -1;
	}    
      } else {
	fprintf(stderr, "Can't get term flags!\n");
	cursor = -1;
      }

    } else {			/* other fd than stdin */
      if((result = read(fdin, port_sig->port_data.buf, tmp_buf_size)) > 0)
	cursor = result;
      else {
	fprintf(stderr, "Failed to read! Errno = %d\n", errno);
	cursor = -1;
      }
    }

    if(cursor > 0) {
      port_sig->port_data.buf[cursor] = '\0';
#ifdef DEBUG
      printf("Read \"%s\" from %d (%d bytes)\n\n", port_sig->port_data.buf, fdin, cursor);
#endif
      port_sig->port_data.len = cursor;
      send((union SIGNAL **)&port_sig, erts_);
      /* wait until erts is ready for more */
      recv_ack();
    } else {
      ose_sig_free_buf((union SIGNAL **)&port_sig);
      if(cursor == 0) {		/* break */
	request_break();
	/* suspend process, will resume after break has been handled */
	stop(current_process());
      } else {			/* error */
	fprintf(stderr, "Fatal error, fdin (%d) process terminating!\n", fdin);
	kill_proc(current_process());
      }
    }
  }
}


/* This starts the fd driver which spawns 2 process, one each for output
   and input. The reason for this is to avoid blocking output while
   waiting for input or polling the input descriptor. */
static ErlDrvData 
fd_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts)
{
  int packet_bytes = opts->packet_bytes;
  PROCESS in_pid_, out_pid_;
  OSENTRYPOINT *entrypoint_out = &fdout_process;
  OSENTRYPOINT *entrypoint_in = &fdin_process;
  union fdSig *sig;
  struct driver_data_t* res;

#ifdef DEBUG
  erl_dbg_fprintf(stdout, "FD driver starting...\n");
#endif

  if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
      ((opts->read_write & DO_WRITE) && opts->ofd >= max_files)) {
    return (ErlDrvData) -1;
  }
  
  if (process_counter == erts_max_ports-1) 
    return (ErlDrvData)ERL_DRV_ERROR_GENERAL;
  else
    process_counter+=2;
  
  out_pid_ = create_process (OS_BG_PROC, name, entrypoint_out, 
			     (OSADDRESS)port_stack_size, 20, 
			     0, erl_block, NULL, 0, 0);
  efs_clone(out_pid_);
  start(out_pid_);

  in_pid_ = create_process (OS_BG_PROC, name, entrypoint_in, 
			    (OSADDRESS)port_stack_size, 20, 
			    0, erl_block, NULL, 0, 0);
  efs_clone(in_pid_);
  start(in_pid_);
  
#ifdef DEBUG
  printf("Spawned fd processes %li(in) %li(out)\n", 
	 (unsigned long)in_pid_, (unsigned long)out_pid_); 
#endif

  sig = (union fdSig *)ose_sig_alloc(sizeof(struct fdStruct), FD_SIG_NO);
  sig->fds.fdout = 0;
  sig->fds.fdin =  opts->ifd;
  sig->fds.in_pid_ = 0;  
  send((union SIGNAL **)&sig, in_pid_);

  sig = (union fdSig *)ose_sig_alloc(sizeof(struct fdStruct), FD_SIG_NO);
  sig->fds.fdout = opts->ofd;
  sig->fds.fdin =  0;
  sig->fds.in_pid_ = in_pid_;
  send((union SIGNAL **)&sig, out_pid_);

  /* check if the file descriptor for input is stdin, if so record the 
     in_pid_, needed for break handling */
  if(opts->ifd == fileno(stdin)) {
#ifdef DEBUG
    printf("Pid for stdin: %li\n", (unsigned long)in_pid_);
#endif
    stdin_pid_ = in_pid_;
  }

  /* make sure input process is set as main process, we will not
     receive anything from the output process */
  if((res = set_driver_data(port_num, packet_bytes,
			    opts->exit_status, in_pid_, out_pid_)) == NULL)
    return ERL_DRV_ERROR_GENERAL;
  return (ErlDrvData)res;
}

/* Erlang writes to fd */
static void fd_output(ErlDrvData drv_data, char *buf, int len)
{
  struct driver_data_t* dd = (struct driver_data_t *)drv_data;
  PROCESS out_pid_;
  union portSig *sig;

  out_pid_ = dd->sec_pid_;

  if(len > 65536)
    erl_dbg_fprintf(stderr, "\r\nWARNING! fd_out signal = %d bytes!\r\n", len);

  sig = (union portSig *)ose_sig_alloc(sizeof(struct PortData)+len, PORT_DATA);
  memcpy(sig->port_data.buf, buf, len);
  sig->port_data.buf[len] = '\0'; 
  sig->port_data.len = len;
  
#ifdef DEBUG
  printf("Sending \"%s\" (%d bytes) to %li\n", sig->port_data.buf, len, 
	 (unsigned long)out_pid_);
#endif

  send((union SIGNAL **)&sig, out_pid_);

  /* wait until buffer has been written before accepting new data */
  recv_ack();
}

/* data from input process */
static void fd_ready_input(ErlDrvData drv_data, ErlDrvEvent sig)
{
  struct driver_data_t *dd = (struct driver_data_t *)drv_data;
  union portSig *signal = (union portSig *)sig;
  int port_num, len;
  char *buf;
  PROCESS pid_;

  pid_ = dd->pid_;
  port_num = dd->port_num;
  len = signal->port_data.len;
  buf = signal->port_data.buf;
  
#ifdef DEBUG
  printf("Receiving %d bytes (\"%s\") from %li, port %d\n", len, buf, 
	 (unsigned long)pid_, port_num);
#endif

  /* this should not be necessary...
     memcpy(tmp_buf, buf, len);	*/

  if (len > 0)
    driver_output(port_num, buf, len);
  else
    port_inp_failure(dd, -1);
  
  /* tell input process it's ok to read more*/
  send_ack(pid_);

  ose_sig_free_buf((union SIGNAL **)&signal);
}


/*** spawn driver implementation ***/

static int spawn_init(void)
{
  char *stackenv;
  int size;
  
  if ((stackenv = getenv("ERLPORTSTACKSIZE")) != NULL &&
      (size = atoi(stackenv)) > 0)
    port_stack_size = size;
  else
    port_stack_size = DEFAULT_PORT_STACK_SIZE;
  
  if(stackenv != NULL) free_buf((union SIGNAL **) &stackenv);

  return 0;
}

static ErlDrvData
spawn_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts)
{
    int packet_bytes = opts->packet_bytes;
    PROCESS port_pid_;
    OSENTRYPOINT *entrypoint = NULL;
    union portSig *sig;
    void *func = NULL;
    struct driver_data_t* res;

#ifdef DEBUG
    printf("Spawn driver starting: %s, port %d\n", name, (int)port_num);
#endif

    if (process_counter == erts_max_ports) 
      return (ErlDrvData)ERL_DRV_ERROR_GENERAL;
    else
      process_counter++;

    /* create and start OSE port process
       first lookup the process entrypoint from the pgm server */
    if(!get_pgm_data(name, &func, NULL, NULL)){
      fprintf(stderr, "Port process entrypoint for \"%s\" not registered!", name);
      return (ErlDrvData)ERL_DRV_ERROR_GENERAL;
    }
    entrypoint = func;
    port_pid_ = create_process(opts->process_type, name, entrypoint, 
			       (OSADDRESS)port_stack_size, opts->priority, 
			       0, erl_block, NULL, 0, 0);
#ifdef DEBUG
    erl_dbg_fprintf(stdout, "Creating process %s, port: %d, type: %d, prio: %d, pid: %li\n", 
		    name, (int)port_num, opts->process_type, opts->priority, 
		    (unsigned long)port_pid_);
#endif
    
    efs_clone(port_pid_);
    start(port_pid_);
    attach(NULL, port_pid_);
    
#ifdef DEBUG
    erl_dbg_fprintf(stdout, "Spawned %s as process %li\n", name, (unsigned long)port_pid_);
#endif      

    if((res = set_driver_data(port_num, packet_bytes,
			      opts->exit_status, port_pid_, 0)) == NULL)
      return ERL_DRV_ERROR_GENERAL;
    return (ErlDrvData)res;
}

/* Erlang sends message to port */
static void output(ErlDrvData drv_data, char* buf, int len)
{
  struct driver_data_t* dd = (struct driver_data_t *)drv_data;
  int pb;
  PROCESS pid_;
  union portSig *sig;
  
  pid_ = dd->pid_;
  pb = dd->packet_bytes;	

  /* Only stream mode makes sense to use for OSE port communication. However,
     FOR TESTING PURPOSES we need to support packet mode as well. In packet
     mode an initial OSE signal will be sent only containing the header bytes
     (the length indicator of 1,2 or 4 bytes) in buf. This signal is followed 
     by the signal containing the actual message. Note that the len element in
     the "header signal" will be set to 0. */

  if(pb > 0) {
    int i, n;
    sig = (union portSig *)ose_sig_alloc(sizeof(struct PortData)+pb, PORT_DATA);
    n = len;
    for(i = pb; i > 0; i--) {
	sig->port_data.buf[i-1] = (char) n;	/* store least significant byte. */
	n = n >> 8;
    }
    sig->port_data.len = 0;
    send((union SIGNAL **)&sig, pid_);
  }

  sig = (union portSig *)ose_sig_alloc(sizeof(struct PortData)+len, PORT_DATA);
  memcpy(sig->port_data.buf, buf, len);
  sig->port_data.buf[len] = '\0';
  sig->port_data.len = len;
  send((union SIGNAL **)&sig, pid_);

  /* we don't need driver_select here since we'll never wait for 'ready' before sending */
}

/* message from port process pointed to by sig */
static void ready_input(ErlDrvData drv_data, ErlDrvEvent sig)
{
  struct driver_data_t *dd = (struct driver_data_t *)drv_data;
  union portSig *signal = (union portSig *)sig;
  int port_num, len;
  int result;

  port_num = dd->port_num;

  /* if the port process has terminated, this should result in an exit
     unless the report_exit flag is set */
  if(signal->sig_no == OS_ATTACH_SIG) {
#ifdef DEBUG
    erl_dbg_fprintf(stderr, "Port process has terminated, closing port %d!\n", (int)port_num);
#endif
    port_inp_failure(dd, 0);
  } else {
    len = signal->port_data.len;
    if (len >= 0) {
      if((result = driver_output(port_num, signal->port_data.buf, len)) < 0) {
#ifdef DEBUG
	erl_dbg_fprintf(stderr, "Invalid port (%d) or bad data, %d bytes ignored!\n",
			(int)port_num, len);
#endif
      }
    } else { 
      fprintf(stderr, "Data from port process %d has undefined size!\n", (int)port_num);
      port_inp_failure(dd, -1);
    }
  }
  ose_sig_free_buf((union SIGNAL **)&signal);
}

/*----------------------- port I/O ctrl --------------------------*/

/* some forward declarations */
static void driver_receiving(int port, PROCESS pid_);
static void driver_not_receiving(int port, PROCESS pid_);
static int pid2port(PROCESS pid_, int *port);  
static int buffer_sig(ErlDrvPort ix, void *sig, SigEntry *q0, SigEntry *q1);
static void driver_started(int port, PROCESS pid_);
static void driver_stopped(int port, PROCESS pid_);

int driver_select(ErlDrvPort ix, ErlDrvEvent e, int mode, int on)
{
  PROCESS pid_ = (PROCESS)e;
  int port_ix = (int)ix;

  if (mode & DO_READ)
    on ? driver_receiving(port_ix, pid_) : driver_not_receiving(port_ix, pid_);
  else if (mode & DO_WRITE) {
    if(on) output_ready(port_ix, 0);
  }
  else if (mode & DO_START)
    driver_started(port_ix, pid_);
  else if (mode & DO_STOP)
    driver_stopped(port_ix, pid_);

  return 0;
}

extern PROCESS erlang_starter_;

static void check_input_ready(void *sig)
{
  PROCESS pid_;
  int port;

  /* any type of signal may be received here and passed on to
     the driver which defines the signal types */
  
  /* sig -> pid -> port */
  pid_ = sender((union SIGNAL **)&sig);  

  switch(pid2port(pid_, &port)) {
    
  case DRV_RECV:		/* driver is known and receiving */
    input_ready(port, (int)sig);
    return;

  case DRV_NOT_RECV:		/* driver is known but not receiving */
    buffer_sig(port, sig, sig_buf_1st, sig_buf_last);
    return;

  case DRV_STOPPED:		/* driver is not known (stopped) */
    /* driver stopped or erlang terminated from shell */
    if(pid_ == erlang_starter_)
      erl_exit(0, "Shut down detected, halting node!\n");
    else
      /* Normal case: Driver is stopped and port process is terminated.
	 When OS_ATTACH_SIG is received, the driver is already unregistered. */
#ifdef DEBUG
      printf("Signal from process: %li. Driver stopped!?\n", (unsigned long)pid_);
#endif
    ose_sig_free_buf((union SIGNAL **)&sig);    
  }
}

static void check_io(int wait)
{
  struct timeval wait_time;
  OSTIME timeout;
  void *sig;
  SigEntry *next_sig;
  static const SIGSELECT any_sig[] = { 0 };
#ifdef DEBUG
  PROCESS from_;
  struct OS_pcb *pcbBuff;
#endif

  /* break handling, stdin process sets flag */
  if (break_requested) {
    break_handling();
  }

  if (wait) {
    erts_time_remaining(&wait_time);
    timeout = (OSTIME)(wait_time.tv_sec*1000) + (OSTIME)(wait_time.tv_usec/1000);
  } else 
    timeout = 0;

  /* by calling driver_sig_pending() a driver may enqueue previously
     (by the driver) buffered signals that will be processed here before 
     new signals are received */
  while(sigQ_1st) {
    check_input_ready(sigQ_1st->sig);
    next_sig = sigQ_1st->next_sig;
    sys_free(sigQ_1st);		/* check_input_ready() frees sig */
    sigQ_1st = next_sig;
  }
  sigQ_last = NULL;

  /* go on and receive new signals, won't wait if Q is empty */
  if ( !(((sig = receive_w_tmo(0, (SIGSELECT*)any_sig)) == NULL) && (timeout == 0)) ) {
    if (sig != NULL) {		/* first signal found */
#ifdef xDEBUG
      from_ = sender((union SIGNAL **)&sig);
      pcbBuff = get_pcb(from_);
      erl_dbg_fprintf(stdout, "Signal %d from %li (%s) received!\n", ((union SIGNAL *)sig)->sig_no, 
		      (unsigned long)from_, &pcbBuff->strings[pcbBuff->name]);
      free_buf((union SIGNAL **) &pcbBuff);
#endif
      check_input_ready(sig);
      timeout = 0;		/* don't wait for second signal */
    }

    /* process all signals in Q, wait for first if not already found */
    while((sig = receive_w_tmo(timeout, (SIGSELECT*)any_sig)) != NULL) {
#ifdef xDEBUG
      from_ = sender((union SIGNAL **)&sig);
      pcbBuff = get_pcb(from_);
      printf("Signal %d from %li (%s) received!\n", ((union SIGNAL *)sig)->sig_no, 
	     (unsigned long)from_, &pcbBuff->strings[pcbBuff->name]);
      free_buf((union SIGNAL **) &pcbBuff);
#endif
      check_input_ready(sig);
      timeout = 0;		
    }
  }
  erts_deliver_time();		/* sync the machine's idea of time */
}

/* add signal to be processed by check_io */
int erl_driver_sig_pending(ErlDrvPort ix, void *sig) {
  return buffer_sig(ix, sig, sigQ_1st, sigQ_last);
}

/* add new signal to list */
static int 
buffer_sig(ErlDrvPort ix, void *sig, SigEntry *q0, SigEntry *q1) {
  SigEntry *new_sig;

  new_sig = (SigEntry *) erts_alloc(ERTS_ALC_T_SIG_ENTRY, sizeof(SigEntry));
  new_sig->next_sig = NULL;
  new_sig->port = ix;
  new_sig->sig = sig;
  /* enqueue the signal */  
  q1->next_sig = new_sig;
  q1 = new_sig;
  return 0;
}

#define MOVE   0
#define DELETE 1

/* action == MOVE:   move all signals with pid_ as sender from src to target list
   action == DELETE: delete all signals that have pid_ as sender */
static void 
process_sigs(int action, PROCESS pid_, SigEntry *src0, SigEntry *src1,
	     SigEntry *target0, SigEntry *target1) {
  SigEntry *prev, *curr;

  curr = prev = src0;
  while(curr) {
    if (sender((union SIGNAL **)(&(curr->sig))) == pid_) {
      if (action == MOVE)
	buffer_sig((ErlDrvPort)curr->port, curr->sig, target0, target1);
      if (curr != prev) {
	prev->next_sig = curr->next_sig;
	erts_free(ERTS_ALC_T_SIG_ENTRY, curr);
	if (action == DELETE) ose_sig_free_buf((union SIGNAL **)(&(curr->sig)));
	curr = prev->next_sig;
      }
      else {			/* curr is first elem in list */
	if (src1 == src0)	/* only one elem in list*/
	  src1 = src0 = curr->next_sig;
	else
	  src0 = curr->next_sig;
	erts_free(ERTS_ALC_T_SIG_ENTRY, curr);
	if (action == DELETE) ose_sig_free_buf((union SIGNAL **)(&(curr->sig)));
	curr = prev = src0;
      }
    }
    else 			/* stays put, move on */
      curr = curr->next_sig;
  }
}

/* create initial pid -> port mapping 
   this function should be called initially (for each pid)
   before receiving is switched on or off */
static void driver_started(int port, PROCESS pid_) {  
  port_entry tmpl;

  tmpl.pid_ = pid_;
  tmpl.port = port;
  tmpl.state = DRV_NOT_RECV;	/* default state */
  hash_put(&pt, &tmpl);		/* insert entry with new state */
}

/* set port state to "receiving", also check if there are buffered signals
   for this port and if so, process them */
static void driver_receiving(int port, PROCESS pid_) {
  port_entry tmpl;

  tmpl.pid_ = pid_;
  hash_erase(&pt, &tmpl);	/* remove previous entry */

  tmpl.port = port;
  tmpl.state = DRV_RECV;
  hash_put(&pt, &tmpl);		/* insert entry with new state */

  /* move buffered signals to pending queue */
  process_sigs(MOVE, pid_, sig_buf_1st, sig_buf_last, sigQ_1st, sigQ_last);
}

/* set status for port to "not receiving" (incoming signals will be buffered) */
static void driver_not_receiving(int port, PROCESS pid_) {
  port_entry tmpl;

  tmpl.pid_ = pid_;
  hash_erase(&pt, &tmpl);	/* remove previous entry */

  tmpl.port = port;
  tmpl.state = DRV_NOT_RECV;
  hash_put(&pt, &tmpl);		/* insert entry with new state */
  
  /* buffer signals in pending queue */
  process_sigs(MOVE, pid_, sigQ_1st, sigQ_last, sig_buf_1st, sig_buf_last);
} 

/* Remove pid -> port mapping, also check if there are buffered signals
   from process pid_ and if so, delete them. Note that driver_stopped() must 
   be called for each pid that's been registered for a port! */
static void driver_stopped(int port, PROCESS pid_) {
  port_entry tmpl;

  tmpl.pid_ = pid_;
  hash_erase(&pt, &tmpl);

  /* delete signals associated with port, both buffered and pending */
  process_sigs(DELETE, pid_, sig_buf_1st, sig_buf_last, NULL, NULL);
  process_sigs(DELETE, pid_, sigQ_1st, sigQ_last, NULL, NULL);
}

/* lookup port given pid
   returns: DRV_RECV | DRV_NOT_RECV | DRV_STOPPED */
static int pid2port(PROCESS pid_, int *port) {
  port_entry tmpl;
  port_entry *result;
  
  tmpl.pid_ = pid_;
  if ((result = hash_get(&pt, &tmpl)) != NULL) {
    *port = result->port;
    return result->state;
  }
  *port = -1;
  return DRV_STOPPED;
}

/*********** hash callbacks **********/
static HashValue pt_hash(void *bucket) {
  return ((port_entry *) bucket)->pid_;
}

static int pt_cmp(void *src, void *test) {
  int pid_src =  ((port_entry *) src)->pid_;
  int pid_test = ((port_entry *) test)->pid_;
  if (pid_src == pid_test) return 0;
  return 1;
}

static void* pt_alloc(void *bucket) {
  port_entry *entry = erts_alloc(ERTS_ALC_T_PRT_ENTRY, sizeof(port_entry));
  entry->port = ((port_entry *) bucket)->port;
  entry->pid_ = ((port_entry *) bucket)->pid_;
  entry->state = ((port_entry *) bucket)->state;
  return (void *)entry;
}

static void pt_free(void *bucket) {
  erts_free(ERTS_ALC_T_PRT_ENTRY, bucket);
}


/********** Port prog and driver registration server ***********/

#define MAX_ENTRIES erts_max_ports

/* the PGM server should use a separate heap (i.e. call OSE 
   malloc instead of erts_alloc) */
#define PGM_SERVER_ALLOC(size) malloc(size)
/* #define PGM_SERVER_ALLOC(size) erts_alloc(ERTS_ALC_T_PGM_TAB, size) */
#define PGM_SERVER_FREE(p) free(p)
/* #define PGM_SERVER_FREE(p) erts_free(ERTS_ALC_T_PGM_ENTRY, p) */

extern void reg_erl_user_pgms(void);

union pgmSig {
  SIGSELECT sigNo;
  struct pgm_entry pgm;
};

OS_PROCESS(erl_sys_pgm_server) {
  union pgmSig *sig;
  static const SIGSELECT select[] = {0};
  pgmEntry **pgms;
  int pgmcount = 0;
  int i = 0;
  PROCESS erts_, client_;

#ifdef DEBUG
  erl_dbg_fprintf(stdout, "erl_sys_pgm_server %li started\n", 
		  (unsigned long)current_process());
#endif

  hunt(ERTS_OSE_PROC_NAME, 0, &erts_, NULL);
  attach(NULL, erts_);

  pgms = PGM_SERVER_ALLOC((MAX_ENTRIES * sizeof(pgmEntry*)));

  while(1) {
    sig = (union pgmSig *)receive((SIGSELECT *)select);
    switch(sig->sigNo) {
    case REG_PGM:
#ifdef DEBUG
      printf("Received reg_pgm: {%s,%X,%d}\n", 
	     sig->pgm.name, sig->pgm.entrypoint, sig->pgm.is_static);
#endif
      pgms[pgmcount] = PGM_SERVER_ALLOC((sizeof(pgmEntry) + strlen(sig->pgm.name)));
      strcpy(pgms[pgmcount]->name, sig->pgm.name);
      pgms[pgmcount]->entrypoint = sig->pgm.entrypoint;
      pgms[pgmcount]->hnd = sig->pgm.hnd;
      pgms[pgmcount]->is_static = sig->pgm.is_static;
      free_buf((union SIGNAL **)&sig);
      pgmcount++;
      break;  
    case DEL_PGM:
      /* find entry given name,
	 move all preceeding entries one step back */
      i = 0;
      while((strcmp(pgms[i]->name, sig->pgm.name) != 0) && (i < pgmcount)) i++;
      if(i == pgmcount) {	/* not found */
	fprintf(stderr, "Cannot delete program %s, not registered!\n", sig->pgm.name);	 
	free_buf((union SIGNAL **)&sig);
	break;
      }
#ifdef DEBUG
      printf("Will delete entry %d(%d), %s, %X\n", i, pgmcount, pgms[i]->name, pgms[i]->entrypoint);
#endif
      PGM_SERVER_FREE(pgms[i]);
      while(i < (pgmcount-1)) { pgms[i] = pgms[i+1]; i++; }
      --pgmcount;
      client_ = sender((union SIGNAL **)&sig);
      send((union SIGNAL **)&sig, client_);      
      break;    
    case OS_ATTACH_SIG:
      free_buf((union SIGNAL **)&sig);
      kill_proc(current_process());
      break;
    case GET_PGM:		/* lookup pgm data */
    case ADD_HND:		/* add program handle */
    default:			
      i = 0;
      while(i < pgmcount) {
	if( (strcmp(pgms[i]->name, sig->pgm.name) == 0) ||
	    ( (sig->pgm.entrypoint != NULL) && 
	      (pgms[i]->entrypoint == sig->pgm.entrypoint) ) ) {
	  if(sig->sigNo == ADD_HND)
	    pgms[i]->hnd = sig->pgm.hnd;
	  else {		/* pgm data */
	    strcpy(sig->pgm.name, pgms[i]->name);
	    sig->pgm.hnd = pgms[i]->hnd;
	    sig->pgm.is_static = pgms[i]->is_static;
	  }
	  sig->pgm.entrypoint = pgms[i]->entrypoint;
	  break;
	}
	i++;
      }
      if(i == pgmcount) sig->pgm.entrypoint = NULL; /* not found */
      client_ = sender((union SIGNAL **)&sig);
      send((union SIGNAL **)&sig, client_);
    }
  }
}

static void start_pgm_server(void) {
  PROCESS server_;

  server_ = create_process(OS_BG_PROC, PGM_SERVER,
			   erl_sys_pgm_server,
			   65535, 20, 0,
			   erl_block, NULL, 0, 0);
  efs_clone(server_);
  start(server_);

  /* call function to register all static port progs and drivers */
  reg_erl_user_pgms();
}

/* 
   search for data given name or entrypoint

   name == NULL:    search on entrypoint, ignore name value
   name[0] == '\0': search on entrypoint, fill name with found program name
   name == <pgm>  : search on name, possibly set entrypoint to found value

   no search on hnd or is_static
*/
int get_pgm_data(char *name, void **entrypoint, void **hnd, int *is_static) {
  union pgmSig *sig;
  static const SIGSELECT select[] = {1, GET_PGM};
  PROCESS server_;

  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_get_pgm_data: %s not running!\n", PGM_SERVER);
    return 0;
  }

  if((name != NULL) && (name[0] != '\0')) { /* name is key */
    sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), GET_PGM);  
    strcpy(sig->pgm.name, name);   
  } else {
    sig = (union pgmSig *)alloc(sizeof(pgmEntry) + 256, GET_PGM);
    sig->pgm.name[0] = '\0';
  }

  if(entrypoint != NULL) 
    sig->pgm.entrypoint = *entrypoint;
  else
    sig->pgm.entrypoint = NULL;

  send((union SIGNAL **)&sig, server_);
  sig = (union pgmSig *)receive((SIGSELECT *) select);

  if(sig->pgm.entrypoint == NULL) { /* not found */
    free_buf((union SIGNAL **)&sig);
    return 0;
  }
  if(name != NULL) strcpy(name, sig->pgm.name);
  if(entrypoint != NULL) *entrypoint = sig->pgm.entrypoint;
  if(hnd != NULL)        *hnd =        sig->pgm.hnd;
  if(is_static != NULL)  *is_static =  sig->pgm.is_static;
  free_buf((union SIGNAL **)&sig);
  return 1;
}

static int reg_pgm(char *name, void *entry, PROCESS from_) {
  union pgmSig *sig;
  PROCESS erts_, server_;

  hunt(ERTS_OSE_PROC_NAME, 0, &erts_, NULL);

  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_reg_pgm(%s): %s not running!\n", name, PGM_SERVER);
    kill_proc(current_process());
  }
  sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), REG_PGM);
  strcpy(sig->pgm.name, name);
  sig->pgm.hnd = NULL;
  sig->pgm.entrypoint = entry;
  if(from_ == erts_)		/* static programs are reg. by erts_ */
    sig->pgm.is_static = 1;    
  else 
    sig->pgm.is_static = 0;
  send((union SIGNAL **)&sig, server_);
  return 0;
}

void del_pgm(char *name) {
  union pgmSig *sig;
  static const SIGSELECT select[] = {1, DEL_PGM};
  PROCESS server_;
  
  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_del_pgm(%s): %s not running!\n", name, PGM_SERVER);
    kill_proc(current_process());
  }
  sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), DEL_PGM);
  strcpy(sig->pgm.name, name);
  send((union SIGNAL **)&sig, server_);
  sig = (union pgmSig *)receive((SIGSELECT *) select);
  free_buf((union SIGNAL **)&sig);
}

int unreg_pgm(char *name) {
  int is_static;

  if(get_pgm_data(name, NULL, NULL, &is_static)) {
    if(is_static) {
      /* fprintf(stderr, "%s is static and cannot be unregistered\n", name); */
      return -1;
    } else {
      del_pgm(name);
      return 0;
    }
  }
  fprintf(stderr, "%s is not a registered program\n", name);
  return -1;
}

/* exported functions for registering static user drivers and port programs
   (dynamic programs use a lib) */
int erl_reg_port_prog(char *name, OSENTRYPOINT *entrypoint, PROCESS from_) {
  reg_pgm(name, (void*)entrypoint, from_);
}
int erl_unreg_port_prog(char *name) {
  unreg_pgm(name);
}
int erl_reg_driver(char *name, void *drv_init_func, PROCESS from_) {
  reg_pgm(name, drv_init_func, from_);
}
int erl_unreg_driver(char *name) {
  unreg_pgm(name);
}

/* used by ose_ddll_drv to save the program handle with the driver info */
int add_pgm_handle(char *name, void *hnd) {
  union pgmSig *sig;
  static const SIGSELECT select[] = {1, ADD_HND};
  PROCESS server_;
  int result;

  if(!hunt(PGM_SERVER, 0, &server_, NULL)) {
    fprintf(stderr, "sys_add_hnd(%s): %s not running!\n", name, PGM_SERVER);
    return 0;
  }
  sig = (union pgmSig *)alloc(sizeof(pgmEntry) + strlen(name), ADD_HND);
  strcpy(sig->pgm.name, name);
  sig->pgm.hnd = hnd;
  sig->pgm.entrypoint = NULL;
  send((union SIGNAL **)&sig, server_);
  sig = (union pgmSig *)receive((SIGSELECT *) select);
  result = (sig->pgm.entrypoint == NULL ? 0 : 1);
  free_buf((union SIGNAL **)&sig);
  return result;
}

/************************** Misc *******************************/

void sys_get_pid(char *buffer) {
  sprintf(buffer, "%li", (unsigned long)current_process());
}

/* set env variabel, data is on form: "VAR=VALUE" */
int sys_putenv(char *buffer) {
  char *tmp, *var, *value;
  int result;

  tmp = strtok(buffer, "=");
  var = erts_alloc(ERTS_ALC_T_PUTENV_STR, strlen(tmp)+1);
  strcpy(var, tmp);
  tmp =  strtok(NULL, "=");
  value = erts_alloc(ERTS_ALC_T_PUTENV_STR, strlen(tmp)+1);
  strcpy(value, tmp);
  strtok(NULL, " ");
  result = set_env (get_bid(current_process()), var, value);
  return result;
}

void sys_init_io(void) 
{
  tmp_buf = (byte *) erts_alloc(ERTS_ALC_T_SYS_TMP_BUF, SYS_TMP_BUF_SIZE);
  tmp_buf_size = SYS_TMP_BUF_SIZE;
  printf("buf size set to %d\n", tmp_buf_size);
}

extern const char pre_loaded_code[];
extern char* const pre_loaded[];

/* Return a pointer to a vector of names of preloaded modules */

Preload* sys_preloaded(void)
{
    return (Preload *) pre_loaded;
}

/* Return a pointer to preloaded code for module "module" */
unsigned char* sys_preload_begin(Preload *pp)
{
    return pp->code;
}

/* Clean up if allocated */
void sys_preload_end(Preload *pp)
{
    /* Nothing */
}

/* Read a key from console */
int sys_get_key(int fd)
{
  int c;
  unsigned char rbuf[64];

  fflush(stdout);		/* flush query */
  if ((c = read(fd, rbuf, 64)) <= 0) {
    return c; 
  }
  return rbuf[0];
}

#ifdef DEBUG

void
erl_assert_error(char* expr, char* file, int line)
{   
#ifndef USE_ERL_DEBUG_PRINTF
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
#else
    erl_dbg_fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
		    expr, file, line);
#endif
    erl_crash_dump(NULL, NULL);
    abort();
}

void
erl_debug(char* fmt, ...)
{
    char sbuf[1024];		/* Temporary buffer. */
    va_list va;
    
    va_start(va, fmt);
    vsprintf(sbuf, fmt, va);
    va_end(va);
#ifndef USE_ERL_DEBUG_PRINTF
    fprintf(stderr, "%s\n", sbuf);
#else
    erl_dbg_fprintf(stderr, "%s\n", sbuf);
#endif
}

#endif /* DEBUG */

void
erl_sys_args(int* argc, char** argv)
{
    /* Dummy */
}


/*-------------------- Memory handling --------------------*/

extern void *elib_malloc(size_t);
extern void *elib_realloc(void *, size_t);
extern void elib_free(void *);
extern void elib_init(void *, int);
extern void elib_force_init(void *, int);
extern size_t elib_sizeof(void *);

#define USING_ELIB_MALLOC         1   /* We are using the elib_malloc */
#define WARN_MALLOC_MIX           2   /* Warn if plain malloc or save_malloc
					 is mixed with sys_free2 or 
					 sys_realloc2 */
#define REALLOC_MOVES             4   /* Always move on realloc 
					 (less fragmentation) */
#define USER_POOL                 8   /* The user supplied the memory
					 pool, it was not save_alloced. */
#define RECLAIM_USER_POOL        16   /* Use the reclaim mechanism in the
					 user pool. */
#define NEW_USER_POOL            32   /* The user pool is newly suppllied,
					 any old pool should be discarded */

static int alloc_flags = 0;
static int alloc_pool_size = 0;
static void *alloc_pool_ptr = NULL;

/*
 * Initialize, sets the values of pointers based on
 * either nothing (the default) or what's set previously by the
 * erl_set_memory_block function.
 */
static void initialize_allocation(void){
#ifdef ENABLE_ELIB_MALLOC
  elib_force_init(alloc_pool_ptr, alloc_pool_size);
  alloc_flags |= USING_ELIB_MALLOC;
  alloc_flags &= ~(NEW_USER_POOL); /* It's never new after initialization*/
#endif
}

void *sys_calloc2(Uint nelem, Uint elsize) {
  void *ptr = erts_alloc_fnf(ERTS_ALC_T_UNDEF, nelem*elsize);
  if(ptr)
    memset(ptr, 0, nelem*elsize);
  return ptr;
}

/* static unsigned long allocated = 0; */

/*
 * The malloc wrapper
 */
void *erts_sys_alloc(ErtsAlcType_t t, void *x, Uint size)
{
#ifdef ENABLE_ELIB_MALLOC
    return elib_malloc((size_t)size);
#else
    return sys_ose_malloc(size);
#endif

}

/*
 * The realloc wrapper
 */
void *
erts_sys_realloc(ErtsAlcType_t type, void *extra, void *ptr, Uint size)
{
#ifdef ENABLE_ELIB_MALLOC
    if(alloc_flags & REALLOC_MOVES) {
      byte *p;
      size_t osz = elib_sizeof(ptr);
      p = (byte*)elib_malloc((size_t) size);
      if(p != NULL){
	memcpy(p, ptr,(((size_t)size) < osz) ? ((size_t)size) : osz);
	elib_free(ptr);
      }
      return p;
    } else {
	return elib_realloc(ptr,(size_t)size);
    }
#else
    return sys_ose_realloc(ptr, size);
#endif
}

/*
 * The free wrapper
 */
void
erts_sys_free(ErtsAlcType_t type, void *extra, void *ptr)
{
#ifdef ENABLE_ELIB_MALLOC
    elib_free(ptr);
#else
    sys_ose_free(ptr);
#endif
}

/* 
 * External interface to be called before erlang is started 
 * Parameters:
 * isize: The size of the memory block where erlang should malloc().
 * iptr: (optional) A pointer to a user supplied memory block of 
 *       size isize.
 * warn_save: Instructs sys_free2 and sys_realloc2 to warn if
 *            memory allocation/reallocation/freeing is mixed between
 *            pure malloc/save_malloc/sys_alloc2 routines (only
 *            warns if elib is actually used in the sys_alloc2 routines).
 * realloc_moves: Always allocate a fresh memory block on reallocation 
 *                (less fragmentation).
 * reclaim_in_supplied: Use memory reclaim mechanisms inside the user
 *                      supplied area, this makes one area reusable between
 *                      starts of erlang and might be nice for drivers etc.
 */

int erl_set_memory_block(int isize, int iptr, int warn_save, 
			 int realloc_moves, int reclaim_in_supplied) {
    if(isize < 8 * 1024 *1024)
	erts_fprintf(stderr,
		     "Warning, the memory pool of %dMb may be to small to "
		     "run erlang in!\n", isize / (1024 * 1024));
    alloc_pool_size = (size_t) isize;
    alloc_pool_ptr = (void *) iptr;
    alloc_flags = 0;
    /* USING_ELIB_MALLOC gets set by the initialization routine */
    if(iptr != NULL)
	alloc_flags |= (USER_POOL | NEW_USER_POOL);
    if(realloc_moves)
	alloc_flags |= REALLOC_MOVES;
    if(warn_save)
	alloc_flags |= WARN_MALLOC_MIX;
    if(iptr != NULL && reclaim_in_supplied)
	alloc_flags |= RECLAIM_USER_POOL;
    return 0;
}

/* External statistics interface */
int erl_memory_show() {
    struct elib_stat statistics;
    erts_printf("Allocation settings:\n");
    erts_printf("Using elib_malloc with memory pool size of %lu bytes.\n",
	       (unsigned long) alloc_pool_size);
    erts_printf("Realloc-always-moves is %s\n", 
	       (alloc_flags & REALLOC_MOVES) ? "on" : "off");
    erts_printf("Warnings about mixed malloc/free's are %s\n", 
	       (alloc_flags & WARN_MALLOC_MIX) ? "on" : "off");
    if(alloc_flags & USER_POOL){
	erts_printf("The memory block used by elib is user supplied "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
	if(alloc_flags & RECLAIM_USER_POOL)
	    erts_printf("Allocated memory within the user supplied pool\n"
		       "  will be automatically reclaimed at task exit.\n");
    } else {
	erts_printf("The memory block used by elib is save_malloc'ed "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
    }
#ifdef NO_FIX_ALLOC
    erts_printf("Fix_alloc is disabled in this build\n");
#endif
    erts_printf("Statistics from elib_malloc:\n");
    elib_stat(&statistics);
    erts_printf("Type          Size (bytes) Number of blocks\n");
    erts_printf("============= ============ ================\n");
    erts_printf("Total:        %12lu %16lu\n",
	       (unsigned long) statistics.mem_total*4,
	       (unsigned long) statistics.mem_blocks);
    erts_printf("Allocated:    %12lu %16lu\n",
	       (unsigned long) statistics.mem_alloc*4,
	       (unsigned long) statistics.mem_blocks-statistics.free_blocks);
    erts_printf("Free:         %12lu %16lu\n",
	       (unsigned long) statistics.mem_free*4,
	       (unsigned long) statistics.free_blocks);
    erts_printf("Largest free: %12lu                -\n\n",
	       (unsigned long) statistics.max_free*4);
    return 0;
}

/* More programmer friendly (as opposed to user friendly ;-) interface
   to the memory statistics. */

/*! uses VxWorks type, needs rewrite for OSE
int erl_mem_info_get(MEM_PART_STATS *stats) {
    struct elib_stat statistics;
    if(!(alloc_flags & USING_ELIB_MALLOC))
	return -1;
    elib_stat(&statistics);
    stats->numBytesFree = statistics.mem_free*4;
    stats->numBlocksFree = statistics.free_blocks;
    stats->maxBlockSizeFree = statistics.max_free*4;
    stats->numBytesAlloc = statistics.mem_alloc*4;
    stats->numBlocksAlloc = statistics.mem_blocks-statistics.free_blocks;
    return 0;
}
*/

#ifdef _OSE_SFK_
void *memcpy(void *s1, const void *s2, size_t n) {
  int i;

  for(i=0; i<n; i++) {
    ((char*)s1)[i] = ((char*)s2)[i];
  }
  return s1;
}
#endif

/******************** Floating point ********************/

/* Float conversion */

int sys_chars_to_double(char *buf, double *fp)
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
  
  if (sscanf(buf, "%lf", fp) != 1)
    return -1;

  return 0;
}

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted
 */

int sys_double_to_chars(double fp, char *buf)
{
  (void) sprintf(buf, "%e", fp);
  return strlen(buf);
}


/**************** Trace & Debug *******************/
#ifdef TRACE_OSE_SIG_ALLOC

union SIGNAL *ose_sig_alloc(OSBUFSIZE size, SIGSELECT signo) {
    ++ose_sig_allocated;
    ose_sig_allocated_bytes += (Uint)size;
    return alloc(size, signo);
}

void ose_sig_free_buf(union SIGNAL **sig) {
    ++ose_sig_freed;
    free_buf(sig);
}

int ose_sig_trace_info(int argc, char **argv) {
  erl_dbg_fprintf(stdout, "ALLOC: N = %u, Bytes = %u\n", 
		  (Uint)ose_sig_allocated,(Uint)ose_sig_allocated_bytes);
  erl_dbg_fprintf(stdout, "FREE: N = %u\n", 
		  (Uint)ose_sig_freed);
  return 0;
}
  

#endif /* TRACE_OSE_SIG_ALLOC */

/******************** dummies ********************/

void init_break_handler() {
}

int driver_event(ErlDrvPort ix, ErlDrvEvent e, ErlDrvEventData event_data) {
  return 0;
}

/* vanilla driver - dummy for now */
struct erl_drv_entry vanilla_driver_entry;
  
