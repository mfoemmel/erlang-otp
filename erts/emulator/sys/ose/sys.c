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

#include "erl_sys_driver.h"
#include "port_signals.sig"

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

#define MAX_FILES() 256		/* how to find for OSE!? */

EXTERN_FUNCTION(void, erl_exit, (int n, char*, _DOTS_));
EXTERN_FUNCTION(void, erl_error, (char*, va_list));
EXTERN_FUNCTION(void, input_ready, (int, int));
EXTERN_FUNCTION(void, output_ready, (int, int));
EXTERN_FUNCTION(int, driver_interrupt, (int, int));
EXTERN_FUNCTION(void, increment_time, (int));
EXTERN_FUNCTION(int, next_time, (_VOID_));
EXTERN_FUNCTION(int, send_error_to_logger, (uint32));

extern Uint erts_max_ports;
extern PROCESS erl_block;

#define NULLTV  ((struct timeval *) 0)
#define NULLFDS ((struct fd_set *) 0)

/* forward declarations */
static FUNCTION(void, check_io, (int));
static FUNCTION(void, initialize_allocation, (void));

#define DEFAULT_PORT_STACK_SIZE  4095
static int port_stack_size;

static int max_files = 50;	/* default configAll.h */

/* 
 * used by the break handler (set by signal handler on ctrl-c)
 */
static volatile int break_requested = 0;

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

union SIGNAL {
  SIGSELECT sig_no;
};

static PROCESS stdin_pid_;

/********************* General functions ****************************/

void
erl_sys_init(void)
{
  break_requested = 0;

  if ((max_files = MAX_FILES()) < 0)
    erl_exit(1, "Can't get no. of available file descriptors\n");

  /* initialize heap memory handler */
  initialize_allocation();

  /* no stdio buffering */
  setvbuf(stdout, (char *)NULL, _IOLBF, BUFSIZ);

  pt_funs.hash = (H_FUN) pt_hash;
  pt_funs.cmp = (HCMP_FUN) pt_cmp;
  pt_funs.alloc = (HALLOC_FUN) pt_alloc;
  pt_funs.free = (HFREE_FUN) pt_free;

  hash_init(&pt, "pid2port", PID2PORT_INIT_SZ, pt_funs);

}

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
    } else {
	check_io(1);
    }
}

int sys_max_files(void) 
{
  return(max_files);
}


/************************* Break handling ******************************/

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

    release = sys_alloc(MAX_VER_STR+3);

    os_info = get_cpu(current_process());
    split_os_info(os_info, sysname, release);

    *pMajor = get_number(&release);
    *pMinor = get_number(&release);
    *pBuild = get_number(&release);

    sys_free(release);
    free_buf((union SIGNAL **)&os_info);
}

typedef struct getenv_state {int c; char* p; char** e;} GetEnvState;

void init_getenv_state(GETENV_STATE *state) {
  char *envstr;
  int len, c=0;
  char **environment;
  GetEnvState *s;
  
  environment = (char**)sys_alloc(10*sizeof(char*));
  s = (GetEnvState *)sys_alloc(sizeof(GetEnvState));
  envstr = get_env_list(get_bid(current_process()), NULL); /* get 1st string of vars */
  while((len = strlen(envstr)) > 0) {
    environment[c] = sys_alloc(len+1);
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
  s->c = 0; s->p = environment[0]; s->e = environment;
  *state = (GETENV_STATE)s; 
}

char *getenv_string(GETENV_STATE *state0) {
  char *next, *value;
  char* env = NULL;
  GetEnvState *s = (GetEnvState *)*state0;

  while(1) {
    if((s->e)[s->c] == NULL) {	
      /* last string processed, clean up */
      sys_free(s->e);
      sys_free(s);
      return NULL;
    }
    /* if next==NULL, we're at the last var in string */
    if((next = strchr(s->p, 32)) != NULL) 
      *next = '\0';
    if((value = get_env(get_bid(current_process()), s->p)) != NULL) {
      env = safe_alloc(strlen(s->p)+strlen(value)+2);
      strcpy(env, s->p);
      strcat(env, "=");
      strcat(env, value);
      free_buf((union SIGNAL **)&value);
    }
    if(next == NULL) {	/* done with this str? if so, try next */
      sys_free((s->e)[s->c]);
      (s->c)++; s->p = (s->e)[s->c];
    } else {
      s->p = (char*)((unsigned int)next + 1);
    }
    if(env != NULL)
      return env;
  }
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
static Uint32 tmp_buf_size;
int cerr_pos;


/*-------------------------- Drivers -----------------------------*/

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
/*
  Various port programs need to ported as well! See c_src for each
  application + etc in erts! 
*/
/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/


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

const struct erl_drv_entry fd_driver_entry = {
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

const struct erl_drv_entry spawn_driver_entry = {
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
  struct ErlPid   erl_pid;
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

  driver_data = (struct driver_data_t*)sys_alloc(sizeof(struct driver_data_t));

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
  (void) driver_select(dd->port_num, dd->pid_, DO_READ, 0); 
  if (res == 0) {
    if (dd->report_exit) {
      int tmpexit = 0;
      int reported;
      if ((reported = dd->exit_reported))
	tmpexit = dd->exitcode;
      if (reported) {
	erl_printf(CERR, "Exitcode %d reported\r\n", tmpexit);
	driver_report_exit(dd->port_num, tmpexit);
      }
    }
  }
  driver_failure(dd->port_num, res);
  return 0;
}

/*** common driver interface ***/

static void stop_driver(ErlDrvData driver_data)
{
  struct driver_data_t* dd = (struct driver_data_t *)driver_data;

  kill_proc(dd->pid_);		/* stop and delete port process */
  driver_select(dd->port_num, dd->pid_, DO_STOP, 1);
  process_counter--;

  if(dd->sec_pid_ != 0) {
    kill_proc(dd->pid_);		/* stop and delete port process */
    driver_select(dd->port_num, dd->sec_pid_, DO_STOP, 1);
    process_counter--;
  }
  sys_free(dd);		
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
  sig = (union ackSig *)alloc(sizeof(struct ackStruct), ACK_SIG_NO);
  send((union SIGNAL **)&sig, pid_);
}

void recv_ack() {
  union ackSig *sig;
  static const SIGSELECT recv_ack_sig[2] = {1, ACK_SIG_NO};
  sig = (union ackSig *)receive((SIGSELECT*)recv_ack_sig);
  free_buf((union SIGNAL **)&sig);
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
  free_buf((union SIGNAL **)&fd_sig);

  /* Attach to erts so that an OS_ATTACH_SIG is received when
     erlang is shut down. When this happens, also kill the input 
     process which is stuck in read. */
  attach(NULL, erts_);

  /* wait for something to print out */
  while(1) {
#ifdef xDEBUG
    printf("Waiting to print to %d...\n\n", fdout);
#endif

    port_sig = (union portSig *)receive((SIGSELECT*)recv_any_sig);

    if(port_sig->sig_no == OS_ATTACH_SIG) {
#ifdef DEBUG
      printf("ERTS has terminated, cleaning up... ");
#endif
      free_buf((union SIGNAL **)&port_sig);
      kill_proc(in_pid_);
      kill_proc(current_process());
#ifdef DEBUG
      printf("done !\n");
#endif      
    }

    len = port_sig->port_data.len;
    buf = port_sig->port_data.buf;

#ifdef xDEBUG
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
    free_buf((union SIGNAL **)&port_sig);
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
  free_buf((union SIGNAL **)&fd_sig);

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

    port_sig = (union portSig *)alloc(max_sig_buf_size, PORT_DATA);

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
		fprintf(stdout, " <break>\n"); /* echo ctrl character */
		cursor = 0; done = 1;
		break;
	      case 0x12:	/*  ^R (new prompt) */
		fprintf(stdout, " <ignore>\n");
		strcpy(port_sig->port_data.buf, "ok.\n");
		cursor = 4; done = 1;
		break;
	      case 0x18:	/* ^X (break) */
		fprintf(stdout, " <break>\n"); /* echo ctrl character */
		cursor = 0; done = 1;
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
#ifdef xDEBUG
      printf("Read \"%s\" from %d (%d bytes)\n\n", port_sig->port_data.buf, fdin, cursor);
#endif
      port_sig->port_data.len = cursor;
      send((union SIGNAL **)&port_sig, erts_);
      /* wait until erts is ready for more */
      recv_ack();
    } else {
      free_buf((union SIGNAL **)&port_sig);
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

  if (((opts->read_write & DO_READ) && opts->ifd >= max_files) ||
      ((opts->read_write & DO_WRITE) && opts->ofd >= max_files)) {
    return (ErlDrvData) -1;
  }
  
  if (process_counter == erts_max_ports-1) 
    return (ErlDrvData)ERL_DRV_ERROR_GENERAL;
  else
    process_counter+=2;
  
  out_pid_ = create_process (OS_PRI_PROC, name, entrypoint_out, 
			     (OSADDRESS)port_stack_size, 20, 
			     0, erl_block, NULL, 0, 0);
  efs_clone(out_pid_);
  start(out_pid_);

  in_pid_ = create_process (OS_PRI_PROC, name, entrypoint_in, 
			    (OSADDRESS)port_stack_size, 20, 
			    0, erl_block, NULL, 0, 0);
  efs_clone(in_pid_);
  start(in_pid_);
  
#ifdef xDEBUG
  printf("Spawned fd processes %li(in) %li(out)\n", 
	 (unsigned long)in_pid_, (unsigned long)out_pid_); 
#endif

  sig = (union fdSig *)alloc(sizeof(struct fdStruct), FD_SIG_NO);
  sig->fds.fdout = 0;
  sig->fds.fdin =  opts->ifd;
  sig->fds.in_pid_ = 0;  
  send((union SIGNAL **)&sig, in_pid_);

  sig = (union fdSig *)alloc(sizeof(struct fdStruct), FD_SIG_NO);
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
  return (ErlDrvData)(set_driver_data(port_num, packet_bytes,
				      opts->exit_status, in_pid_, out_pid_));
}

/* Erlang writes to fd */
static void fd_output(ErlDrvData drv_data, char *buf, int len)
{
  struct driver_data_t* dd = (struct driver_data_t *)drv_data;
  PROCESS out_pid_;
  union portSig *sig;

  out_pid_ = dd->sec_pid_;

  sig = (union portSig *)alloc(sizeof(struct PortData)+len, PORT_DATA);
  memcpy(sig->port_data.buf, buf, len);
  sig->port_data.buf[len] = '\0'; 
  sig->port_data.len = len;

#ifdef xDEBUG
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
  
#ifdef xDEBUG
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

  free_buf((union SIGNAL **)&signal);
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

/* extern OSENTRYPOINT erl_port_init0; */
extern int user_port_map(char*, void**);

static int sys_port_map(char *name, void **func) {
  /*
  if(strcmp(name, "sys_port1") == 0) {
    *func = (void)&sys_port1;
    return 1;
  }
  */
  return 0;
}

static ErlDrvData
spawn_start(ErlDrvPort port_num, char *name, SysDriverOpts* opts)
{
    int packet_bytes = opts->packet_bytes;
    PROCESS port_pid_;
    /*    OSENTRYPOINT *entrypoint = &erl_port_init0;  */
    OSENTRYPOINT *entrypoint = NULL;
    union portSig *sig;
    int result;
    void *func = NULL;

#ifdef DEBUG
    printf("Spawn driver starting: %s, port %d\n", name, (int)port_num);
#endif

    if (process_counter == erts_max_ports) 
      return (ErlDrvData)ERL_DRV_ERROR_GENERAL;
    else
      process_counter++;

    /* create and start OSE port process */
    /* later create separate block for port processes so they don't
       interfere with erts */
    
    if(!(result = sys_port_map(name, &func)))
      result = user_port_map(name, &func);

    if(result) {
      entrypoint = func;
#ifdef DEBUG
      printf("Creating process, type: %d, prio: %d, entry: %x\n", 
	     opts->process_type, opts->priority, *entrypoint);
#endif
      
      port_pid_ = create_process(opts->process_type, name, entrypoint, 
				 (OSADDRESS)port_stack_size, opts->priority, 
				 0, erl_block, NULL, 0, 0);
      efs_clone(port_pid_);
      start(port_pid_);
      attach(NULL, port_pid_);
      
      /* send id of erts process to port process */
      sig = (union portSig *)alloc(sizeof(struct ErlPid), ERL_PID);
      sig->erl_pid.pid_ = current_process();
      send((union SIGNAL **)&sig, port_pid_);

#ifdef DEBUG
      printf("Spawned %s as process %li\n", name, (unsigned long)port_pid_);
#endif      
      return (ErlDrvData)(set_driver_data(port_num, packet_bytes,
					  opts->exit_status, port_pid_, 0));
    } else {
      fprintf(stderr, "Invalid port process name \"%s\", not started!", name);
      return (ErlDrvData) -1;
    }
}


/* Erlang sends message to port */
static void output(ErlDrvData drv_data, char* buf, int len)
{
  struct driver_data_t* dd = (struct driver_data_t *)drv_data;
  int pb;
  PROCESS pid_;
  union portSig *sig;
  
  pid_ = dd->pid_;
  pb = dd->packet_bytes;	/* used if not stream mode!? */

  sig = (union portSig *)alloc(sizeof(struct PortData)+len, PORT_DATA);
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

  port_num = dd->port_num;

  if(signal->sig_no == OS_ATTACH_SIG) {
    fprintf(stderr, "Port process has terminated, closing port %d!\n", (int)port_num);
    driver_exit(port_num, 0);
  } else {
    len = signal->port_data.len;
    if (len > 0)
      driver_output(port_num, signal->port_data.buf, len);
    else
      port_inp_failure(dd, -1);
  }
  free_buf((union SIGNAL **)&signal);
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
    free_buf((union SIGNAL **)&sig);    
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
      printf("Signal %d from %li (%s) received!\n", ((union SIGNAL *)sig)->sig_no, 
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
  erts_deliver_time(NULL);		/* sync the machine's idea of time */
}

/* add signal to be processed by check_io */
int driver_sig_pending(ErlDrvPort ix, void *sig) {
  return buffer_sig(ix, sig, sigQ_1st, sigQ_last);
}

/* add new signal to list */
static int 
buffer_sig(ErlDrvPort ix, void *sig, SigEntry *q0, SigEntry *q1) {
  SigEntry *new_sig;

  new_sig = (SigEntry *)sys_alloc(sizeof(SigEntry));
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
	sys_free(curr);
	if (action == DELETE) free_buf((union SIGNAL **)(&(curr->sig)));
	curr = prev->next_sig;
      }
      else {			/* curr is first elem in list */
	if (src1 == src0)	/* only one elem in list*/
	  src1 = src0 = curr->next_sig;
	else
	  src0 = curr->next_sig;
	sys_free(curr);
	if (action == DELETE) free_buf((union SIGNAL **)(&(curr->sig)));
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
  port_entry *entry = sys_alloc(sizeof(port_entry));
  entry->port = ((port_entry *) bucket)->port;
  entry->pid_ = ((port_entry *) bucket)->pid_;
  entry->state = ((port_entry *) bucket)->state;
  return (void *)entry;
}

static void pt_free(void *bucket) {
  sys_free(bucket);
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
  var = safe_alloc(strlen(tmp)+1);
  strcpy(var, tmp);
  tmp =  strtok(NULL, "=");
  value = safe_alloc(strlen(tmp)+1);
  strcpy(value, tmp);
  strtok(NULL, " ");
  result = set_env (get_bid(current_process()), var, value);
  return result;
}

void sys_init_io(byte *buf, Uint32 size) 
{
  tmp_buf = buf;
  tmp_buf_size = size;
  printf("buf size set to %d\n", size);
}

extern const char pre_loaded_code[];
extern char* const pre_loaded[];


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
  
  if (where == CERR) {
    erl_error(format, va);
  }
  else if (where == COUT) {
    vfprintf(stdout, format, va);
  }
  else if (where == CBUF) {
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
  } else {
    /* Seems only to be used in crash_dumping... */
    vsprintf(tmp_buf, format, va);
    if(sys_strlen(tmp_buf) >= TMP_BUF_MAX)
      erl_exit(1, "Internal buffer overflow in erl_printf\n");
    write((int) where, tmp_buf, strlen(tmp_buf));
  }
  va_end(va);
}

    
void sys_putc(int ch, CIO where)
{
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
  else if (where == COUT)
    fputc(ch, stdout);
  else
    sys_printf(where, "%c", ch);
}

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
    fflush(stdout);
    fprintf(stderr, "Assertion failed: %s in %s, line %d\n",
	    expr, file, line);
    fflush(stderr);
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
    fprintf(stderr, "%s\n", sbuf);
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

#ifdef INSTRUMENT
/* When instrumented sys_alloc and friends are implemented in utils.c */
#define SYS_ALLOC_ELSEWHERE
#endif

/* A = alloc |realloc | free */
#ifdef SYS_ALLOC_ELSEWHERE

/* sys_A implemented elsewhere (calls sys_A2) ... */ 
#ifndef sys_alloc2
/* ... and sys_A2 makros not used; use this implementation as sys_A2 */
#define SYS_ALLOC   sys_alloc2
#define SYS_REALLOC sys_realloc2
#define SYS_FREE    sys_free2
#else  /* ifndef sys_alloc2 */
/* ... and sys_A2 makros used; don't use this implementation as sys_A2 */
#endif /* ifndef sys_alloc2 */

#else /* #ifdef SYS_ALLOC_ELSEWHERE */

/* sys_A not implemented elsewhere ... */
#ifndef sys_alloc2
/* ... and sys_A2 makros not used; skip sys_A2 and use
   this implementation as sys_A */
#define SYS_ALLOC   sys_alloc
#define SYS_REALLOC sys_realloc
#define SYS_FREE    sys_free

#else /* ifndef sys_alloc2 */

/* ... and sys_A2 makros used; need sys_A (symbols) */
void* sys_alloc(Uint size)              { return sys_alloc2(size);        }
void* sys_realloc(void* ptr, Uint size) { return sys_realloc2(ptr, size); }
void* sys_free(void* ptr)               { return sys_free2(ptr);          }
#endif /* ifndef sys_alloc2 */

#endif /* #ifdef SYS_ALLOC_ELSEWHERE */

#ifdef SYS_ALLOC 

/* 
 * !!!!!!!  Allocated blocks MUST be aligned correctly !!!!!!!!!
 */
#define MEM_BEFORE  ((Uint) 0xABCDEF97)

#define MEM_AFTER1  ((byte) 0xBA)
#define MEM_AFTER2  ((byte) 0xDC)
#define MEM_AFTER3  ((byte) 0xFE)
#define MEM_AFTER4  ((byte) 0x77)

#define SL_MALLOC_MEM_BEFORE ((Uint) ~MEM_BEFORE)

#define SL_MALLOC_MEM_AFTER1 ((byte) ~MEM_AFTER1)
#define SL_MALLOC_MEM_AFTER2 ((byte) ~MEM_AFTER2)
#define SL_MALLOC_MEM_AFTER3 ((byte) ~MEM_AFTER3)
#define SL_MALLOC_MEM_AFTER4 ((byte) ~MEM_AFTER4)

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
    Uint pattern;			/* Fence pattern. */
    Uint size;				/* Size of allocated memory block. */
    Most_strict block[1];		/* Ensure proper alignment. */
} Memory_guard;

#define MEM_GUARD_SZ (sizeof(Memory_guard) - sizeof(Most_strict))

void *sys_calloc2(Uint nelem, Uint elsize) {
  void *ptr = sys_ose_calloc(nelem, elsize);
  return ptr;
}

/* static unsigned long allocated = 0; */

/*
 * The malloc wrapper
 */
void *SYS_ALLOC(Uint size) {
    register byte *p;

#ifdef DEBUG
    size += MEM_GUARD_SZ + SIZEOF_AFTER;
#endif

    /* printf("Allocating %d bytes (%li allocated)\n", size, allocated);
       allocated += size; */

#ifdef ENABLE_ELIB_MALLOC
    p = (byte *)elib_malloc((size_t)size);
#else
    p = (byte *)sys_ose_malloc(size);
#endif

#ifdef DEBUG
    if (p != NULL) {
	Memory_guard* before = (Memory_guard *) p;
	byte* after;

	p += MEM_GUARD_SZ;
	before->pattern = MEM_BEFORE;
	before->size = size - MEM_GUARD_SZ - SIZEOF_AFTER;
	after = p + before->size;
	SET_AFTER(after);
    }
#endif    
    return (void*)p;
}

/*
 * The realloc wrapper
 */
void *SYS_REALLOC(void *ptr, Uint size) {
    register byte *p;

#ifdef DEBUG
    {
      Uint old_size;
      Memory_guard* before;
      byte* after;
      
      /* Note: realloc(NULL, size) is supposed to work, even in DEBUG mode. */
      if (ptr) {
	p = (byte *) ptr;
	before = (Memory_guard *) (p-MEM_GUARD_SZ);
	if (before->pattern != MEM_BEFORE) {
	  if(before->pattern == SL_MALLOC_MEM_BEFORE
	     && ((byte *)p+before->size)[0] == SL_MALLOC_MEM_AFTER1
	     && ((byte *)p+before->size)[1] == SL_MALLOC_MEM_AFTER2
	     && ((byte *)p+before->size)[2] == SL_MALLOC_MEM_AFTER3
	     && ((byte *)p+before->size)[3] == SL_MALLOC_MEM_AFTER4)
	    erl_exit(1,
		     "sys_realloc on memory allocated by sl_malloc/sl_realloc at "
		     "0x%p (size %d)\n",
		     ptr,
		     before->size);
	  
	  erl_exit(1, "realloc: Fence before memory at 0x%p clobbered\n",
		   ptr);
	}
	old_size = before->size;
	after = p+old_size;
	if (!CHECK_AFTER(after)) {
	  erl_exit(1, "realloc: Fence after memory at 0x%p (size %d) clobbered\n",
		   ptr, size);
	}
	ptr = ((byte*) ptr) - MEM_GUARD_SZ;
	size += MEM_GUARD_SZ + SIZEOF_AFTER;
      }
    }
#endif
    
#ifdef ENABLE_ELIB_MALLOC
    if(alloc_flags & REALLOC_MOVES) {
      size_t osz = elib_sizeof(ptr);
      p = (byte*)elib_malloc((size_t) size);
      if(p != NULL){
	memcpy(p, ptr,(((size_t)size) < osz) ? ((size_t)size) : osz);
	elib_free(ptr);
      }
    } else {
      p = (byte*)elib_realloc(ptr,(size_t)size);
    }
#else
    p = (byte*)sys_ose_realloc(ptr, size);
#endif

#ifdef DEBUG
    if (p != NULL) {
	before = (Memory_guard *) p;
	before->size = size-MEM_GUARD_SZ-SIZEOF_AFTER;
	p += MEM_GUARD_SZ;
	after = p + before->size;
	SET_AFTER(after);
    }
#endif
    return (void*)p;
}

/*
 * The free wrapper
 */
void SYS_FREE(void *ptr) {
#ifdef DEBUG
    Uint size;
    register byte* p;
    Memory_guard* before;
    byte* after;

    if (ptr) {
      p = (byte *) ptr;
      before = (Memory_guard *) (p-MEM_GUARD_SZ);
    
      if (before->pattern != MEM_BEFORE) {
	if(before->pattern == SL_MALLOC_MEM_BEFORE
	   && ((byte *)p+before->size)[0] == SL_MALLOC_MEM_AFTER1
	   && ((byte *)p+before->size)[1] == SL_MALLOC_MEM_AFTER2
	   && ((byte *)p+before->size)[2] == SL_MALLOC_MEM_AFTER3
	   && ((byte *)p+before->size)[3] == SL_MALLOC_MEM_AFTER4)
	  erl_exit(1,
		   "sys_free on memory allocated by sl_malloc/sl_realloc at "
		   "0x%p (size %d)\n",
		   ptr,
		   before->size);
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
      ptr = ((byte*) ptr) - MEM_GUARD_SZ;
    }
#endif

#ifdef ENABLE_ELIB_MALLOC
    elib_free(ptr);
#else
    sys_ose_free(ptr);
#endif
}

#endif /* #ifdef SYS_ALLOC */


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
	erl_printf(CERR,"Warning, the memory pool of %dMb may be to small to "
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
    erl_printf(COUT,"Allocation settings:\n");
    erl_printf(COUT,"Using elib_malloc with memory pool size of %lu bytes.\n",
	       (unsigned long) alloc_pool_size);
    erl_printf(COUT,"Realloc-always-moves is %s\n", 
	       (alloc_flags & REALLOC_MOVES) ? "on" : "off");
    erl_printf(COUT,"Warnings about mixed malloc/free's are %s\n", 
	       (alloc_flags & WARN_MALLOC_MIX) ? "on" : "off");
    if(alloc_flags & USER_POOL){
	erl_printf(COUT,"The memory block used by elib is user supplied "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
	if(alloc_flags & RECLAIM_USER_POOL)
	    erl_printf(COUT,"Allocated memory within the user supplied pool\n"
		       "  will be automatically reclaimed at task exit.\n");
    } else {
	erl_printf(COUT,"The memory block used by elib is save_malloc'ed "
		   "at 0x%08x.\n", (unsigned int) alloc_pool_ptr);
    }
#ifdef NO_FIX_ALLOC
    erl_printf(COUT,"Fix_alloc is disabled in this build\n");
#endif
    erl_printf(COUT,"Statistics from elib_malloc:\n");
    elib_stat(&statistics);
    erl_printf(COUT,"Type          Size (bytes) Number of blocks\n");
    erl_printf(COUT,"============= ============ ================\n");
    erl_printf(COUT,"Total:        %12lu %16lu\n",
	       (unsigned long) statistics.mem_total*4,
	       (unsigned long) statistics.mem_blocks);
    erl_printf(COUT,"Allocated:    %12lu %16lu\n",
	       (unsigned long) statistics.mem_alloc*4,
	       (unsigned long) statistics.mem_blocks-statistics.free_blocks);
    erl_printf(COUT,"Free:         %12lu %16lu\n",
	       (unsigned long) statistics.mem_free*4,
	       (unsigned long) statistics.free_blocks);
    erl_printf(COUT,"Largest free: %12lu                -\n\n",
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


/******************** dummies ********************/

void init_break_handler() {
}

void sys_tty_reset() {
}

char *ddll_error() {
  return (char*)NULL;
}

void *ddll_open(char *full_name) {
  return (void*)NULL;
}

void *ddll_sym(void *handle, char *func_name) {
  return (void*)NULL;
}

int ddll_close(void *handle) {
  return 0;
}

int driver_event(ErlDrvPort ix, ErlDrvEvent e, ErlDrvEventData event_data) {
  return 0;
}

/* vanilla driver - dummy for now */
const struct erl_drv_entry vanilla_driver_entry;
  

/*! *** Qs ***
 - Problem with port program: How to "carry" the create_process args?
    Possibilites: 1. Don't allow port programs, only drivers (handle spawning themselves)
                  2. Restrict port program to only use defaults
                  3. Extend open_port -> driver_start i/f
		  4. "Work around" existing i/f
 */
