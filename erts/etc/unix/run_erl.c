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
 * Module: run_erl.c
 * 
 * This module implements a reader/writer process that opens two specified 
 * FIFOs, one for reading and one for writing; reads from the read FIFO
 * and writes to stdout and the write FIFO.
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <dirent.h>
#include <termios.h>

#define noDEBUG

#define DEFAULT_LOG_GENERATIONS 5
#define LOG_MAX_GENERATIONS     1000      /* No more than 1000 log files */
#define LOG_MIN_GENERATIONS     2         /* At least two to switch between */
#define DEFAULT_LOG_MAXSIZE     100000
#define LOG_MIN_MAXSIZE         1000      /* Smallast value for changing log file */
#define LOG_STUBNAME            "erlang.log."
#define LOG_PERM                0664
#define LOG_ACTIVITY_MINUTES    5
#define LOG_ALIVE_MINUTES       15

#define PERM            0600
#define STATUSFILENAME  "/run_erl.log"
#define PIPE_STUBNAME   "erlang.pipe"
#define PIPE_STUBLEN    strlen(PIPE_STUBNAME)

#ifndef FILENAME_MAX
#define FILENAME_MAX 250
#endif

#ifndef O_SYNC
#define O_SYNC 0
#define USE_FSYNC 1
#endif

#define MAX(x,y)  ((x) > (y) ? (x) : (y))

/* prototypes */
static void usage(char *);
static int create_fifo(char *name, int perm);
static int open_pty_master(char **name);
static int open_pty_slave(char *name);
static void pass_on(pid_t, int);
static void exec_shell(char **);
static void status(const char *format,...);
static void error(const char *format,...);
static void catch_sigpipe(int);
static int next_log(int log_num);
static int prev_log(int log_num);
static int find_next_log_num();
static int open_log(int log_num, int flags);
static void write_to_log(int* lfd, int* log_num, char* buf, int len);
#ifdef DEBUG
static void show_terminal_settings(struct termios *t);
#endif

/* static data */
static char FIFO1[FILENAME_MAX], FIFO2[FILENAME_MAX];
static char STATUSFILE[FILENAME_MAX];
static char log_dir[FILENAME_MAX];
static char pipename[FILENAME_MAX];
static FILE *stdstatus = NULL;
static struct sigaction sig_act;
static int fifowrite = 0;
static int log_generations = DEFAULT_LOG_GENERATIONS;
static int log_maxsize     = DEFAULT_LOG_MAXSIZE;

static void usage(char *pname)
{
  fprintf(stderr, "Usage: %s (pipe_name|pipe_dir/) log_dir \"command [parameters ...]\"\n", pname);
  fprintf(stderr, "\nYou may also set the environment variables RUN_ERL_LOG_GENERATIONS\n");
  fprintf(stderr, "and RUN_ERL_LOG_MAXSIZE to the number of log files to use and the\n");
  fprintf(stderr, "size of the log file when to switch to the next log file\n");
}

int main(int argc, char **argv)
{
  int childpid;
  int mfd, sfd;
  int fd;
  char *p, *ptyslave;

  if(argc<4) {
    usage(argv[0]);
    exit(99);
  }
  strncpy(pipename, argv[1], sizeof(pipename));

  strncpy(log_dir, argv[2], sizeof(log_dir));
  strncpy(STATUSFILE, log_dir, sizeof(STATUSFILE));
  strncat(STATUSFILE, STATUSFILENAME, sizeof(STATUSFILE));

#ifdef DEBUG
  status("%s: pid is : %d\n", argv[0], getpid());
#endif

  /* Get values for LOG file handling from the environment */

  if ((p = getenv("RUN_ERL_LOG_GENERATIONS"))) {
    log_generations = atoi(p);
    if (log_generations < LOG_MIN_GENERATIONS)
      error("Minumum RUN_ERL_LOG_GENERATIONS is %d\n", LOG_MIN_GENERATIONS);
    if (log_generations > LOG_MAX_GENERATIONS)
      error("Maxumum RUN_ERL_LOG_GENERATIONS is %d\n", LOG_MAX_GENERATIONS);
  }

  if ((p = getenv("RUN_ERL_LOG_MAXSIZE"))) {
    log_maxsize = atoi(p);
    if (log_maxsize < LOG_MIN_MAXSIZE)
      error("Minumum RUN_ERL_LOG_MAXSIZE is %d\n", LOG_MIN_MAXSIZE);
  }

  /*
   * Create FIFOs and open them 
   */

  if(*pipename && pipename[strlen(pipename)-1] == '/') {
    /* The user wishes us to find a unique pipe name in the specified */
    /* directory */
    int highest_pipe_num = 0;
    DIR *dirp;
    struct dirent *direntp;

    dirp = opendir(pipename);
    if(!dirp) {
      error("Can't access pipe directory %s.\n", pipename);
      exit(1);
    }

    /* Check the directory for existing pipes */
    
    while((direntp=readdir(dirp)) != NULL) {
      if(strncmp(direntp->d_name,PIPE_STUBNAME,PIPE_STUBLEN)==0) {
	int num = atoi(direntp->d_name+PIPE_STUBLEN+1);
	if(num > highest_pipe_num)
	  highest_pipe_num = num;
      }
    }	
    closedir(dirp);
    sprintf(pipename+strlen(pipename),
	    "%s.%d",PIPE_STUBNAME,highest_pipe_num+1);
  } /* if */

  /* write FIFO - is read FIFO for `to_erl' program */
  strncpy(FIFO1, pipename, FILENAME_MAX);
  strncat(FIFO1, ".r", FILENAME_MAX - strlen(FIFO1));
  if (create_fifo(FIFO1, PERM) < 0) {
    error("Cannot create FIFO %s for writing.\n", FIFO1);
    exit(1);
  }

  /* read FIFO - is write FIFO for `to_erl' program */
  strncpy(FIFO2, pipename, FILENAME_MAX);
  strncat(FIFO2, ".w", FILENAME_MAX - strlen(FIFO2));
  /* Check that nobody is running run_erl already */
  if ((fd = open (FIFO2, O_WRONLY|O_NDELAY, 0)) >= 0) {
    /* Open as client succeeded -- run_erl is already running! */
    fprintf(stderr, "Erlang already running on pipe %s.\n", pipename);
    close(fd);
    exit(1);
  }
  if (create_fifo(FIFO2, PERM) < 0) { 
    error("Cannot create FIFO %s for reading.\n", FIFO2);
    exit(1);
  }

  /*
   * Open master pseudo-terminal
   */

  if ((mfd = open_pty_master(&ptyslave)) < 0) {
    error("Could not open pty master\n");
    exit(1);
  }

  /* 
   * Now create a child process
   */

  if ((childpid = fork()) < 0) {
    error("Cannot fork\n");
    exit(1);
  }
  if (childpid == 0) {
    /* Child */
    close(mfd);
    /* disassociate from control terminal */
#ifdef USE_SETPGRP_NOARGS       /* SysV */
    setpgrp();
#else
#ifdef USE_SETPGRP              /* BSD */
    setpgrp(0,getpid());
#else                           /* POSIX */
    setsid();
#endif
#endif
    /* Open the slave pty */
    if ((sfd = open_pty_slave(ptyslave)) < 0) {
      error("Could not open pty slave %s\n", ptyslave);
      exit(1);
    }
    /* Close stdio */
    close(0);
    close(1);
    close(2);
    if (dup(sfd) != 0 || dup(sfd) != 1 || dup(sfd) != 2) {
      status("Cannot dup\n");
    }
    close(sfd);
    exec_shell(argv+1); /* exec_shell expects argv[2] to be */
                        /* the command name, so we have to */
                        /* adjust. */
  } else {
    /* Parent */
    /* Catch the SIGPIPE signal */
    sigemptyset(&sig_act.sa_mask);
    sig_act.sa_flags = 0;
    sig_act.sa_handler = catch_sigpipe;
    sigaction(SIGPIPE, &sig_act, (struct sigaction *)NULL);

    /*
     * read and write: enter the workloop
     */

    pass_on(childpid, mfd);
  }
  return 0;
} /* main() */

/* pass_on()
 * Is the work loop of the logger. Selects on the pipe to the to_erl
 * program erlang. If input arrives from to_erl it is passed on to
 * erlang.
 */
static void pass_on(pid_t childpid, int mfd)
{
  int len;
  fd_set readfds;
  struct timeval timeout;
  time_t last_activity;
  char buf[BUFSIZ];
  int lognum;
  int rfd, wfd=0, lfd=0;
  int maxfd;
  int ready;

  /* Open the to_erl pipe for reading.
   * We can't open the writing side because nobody is reading and 
   * we'd either hang or get an error.
   */
  if ((rfd = open(FIFO2, O_RDONLY|O_NDELAY, 0)) < 0) {
    error("Could not open FIFO %s for reading.\n", FIFO2);
    exit(1);
  }

#ifdef DEBUG
  status("run_erl: %s opened for reading\n", FIFO2);
#endif

  /* Open the log file */

  lognum = find_next_log_num();
  lfd = open_log(lognum, O_RDWR|O_APPEND|O_CREAT|O_SYNC);

  /* Enter the work loop */

  while (1) {
    maxfd = MAX(rfd, mfd);
    FD_ZERO(&readfds);
    FD_SET(rfd, &readfds);
    FD_SET(mfd, &readfds);
    time(&last_activity);
    timeout.tv_sec  = LOG_ALIVE_MINUTES*60; /* don't assume old BSD bug */
    timeout.tv_usec = 0;
    ready = select(maxfd + 1, &readfds, NULL, NULL, &timeout);
    if (ready < 0) {
      /* Some error occured */
      error("Error in select.");
      exit(1);
    } else {
      /* Check how long time we've been inactive */
      time_t now;
      time(&now);
      if(!ready || now - last_activity > LOG_ACTIVITY_MINUTES*60) {
	/* Either a time out: 15 minutes without action, */
	/* or something is coming in right now, but it's a long time */
	/* since last time, so let's write a time stamp this message */
	sprintf(buf, "\n===== %s%s", ready?"":"ALIVE ", ctime(&now));
	write_to_log(&lfd, &lognum, buf, strlen(buf));
      }
    }
  
    /*
     * Read master pty write to FIFO
     */
    if (FD_ISSET(mfd, &readfds)) {
#ifdef DEBUG
      status("Pty master read; ");
#endif
      if ((len = read(mfd, buf, BUFSIZ)) <= 0) {
	close(rfd);
	if(wfd) close(wfd);
	close(mfd);
	unlink(FIFO1);
	unlink(FIFO2);
	if (len < 0) {
	  if(errno == EIO)
	    error("Erlang closed the connection.\n");
	  else
	    error("Error in reading from terminal: errno=%d\n",errno);
	  exit(1);
	}
	exit(0);
      }

      write_to_log(&lfd, &lognum, buf, len);

      /* Write to to_erl operator, if any */

      if (fifowrite) {
#ifdef DEBUG
	status("FIFO write; ");
#endif
	/* Ignore write errors - typically there is no one
	 * reading at the other end of the FIFO.
	 */
	if(wfd) 
	  if(write(wfd, buf, len) < 0) {
	    close(wfd);
	    wfd = 0;
	  }
      }
#ifdef DEBUG
      status("OK\n");
#endif
    }

    /*
     * Read from FIFO, write to master pty
     */
    if (FD_ISSET(rfd, &readfds)) {
#ifdef DEBUG
      status("FIFO read; ");
#endif
      fifowrite = 1;
      if ((len = read(rfd, buf, BUFSIZ)) < 0) {
	close(rfd);
	if(wfd) close(wfd);
	close(mfd);
	unlink(FIFO1);
	unlink(FIFO2);
	error("Error in reading from FIFO.\n");
	exit(1);
      }

      /* Try to open the write pipe to to_erl. Now that we got some data
      * from to_erl, to_erl should already be reading this pipe - open
      * should succeed. But in case of error, we just ignore it.
      */

      if(!len) {
	close(rfd);
	rfd = open(FIFO2, O_RDONLY|O_NDELAY, 0);
	if (rfd < 0) {
	  error("Could not open FIFO %s for reading.\n", FIFO2);
	  exit(1);
	}
      } else {
	if(!wfd) {
	  if ((wfd = open(FIFO1, O_WRONLY|O_NDELAY, 0)) < 0) {
	    status("Client expected on FIFO %s, but can't open (len=%d)\n",
		   FIFO1, len);
	    close(rfd);
	    rfd = open(FIFO2, O_RDONLY|O_NDELAY, 0);
	    if (rfd < 0) {
	      error("Could not open FIFO %s for reading.\n", FIFO2);
	      exit(1);
	    }
	    wfd = 0;
	  } else {
#ifdef DEBUG
	    status("run_erl: %s opened for writing\n", FIFO1);
#endif
	  }
	}
	
	/* Write the message */
#ifdef DEBUG
	status("Pty master write; ");
#endif
	if(len==1 && buf[0] == '\003') {
	  kill(childpid,SIGINT);
	} else if(write(mfd, buf, len) != len) {
	  error("Error in writing to terminal.\n");
	  close(rfd);
	  if(wfd) close(wfd);
	  close(mfd);
	  exit(1);
	}
      }
#ifdef DEBUG
      status("OK\n");
#endif
    }
  }
} /* pass_on() */

/*
 * catch_sigpipe()
 * Called if there is an exception on a pipe.
 * This is normally because the to_erl program is no longer connected
 * to the pipe. We just set a flag that indicates that no writing to
 * the pipe is to be done.
 */
static void catch_sigpipe(int sig)
{
  switch(sig) {
  case SIGPIPE:
    fifowrite = 0;
  default:
    ;
  }
}

/*
 * next_log:
 * Returns the index number that follows the given index number.
 * (Wrapping after log_generations)
 */
static int next_log(int log_num) {
  return log_num>=log_generations?1:log_num+1;
}

/*
 * prev_log:
 * Returns the index number that precedes the given index number.
 * (Wrapping after log_generations)
 */
static int prev_log(int log_num) {
  return log_num<=1?log_generations:log_num-1;
}

/*
 * find_next_log_num()
 * Searches through the log directory to check which logs that already
 * exist. It finds the "hole" in the sequence, and returns the index
 * number for the last log in the log sequence. If there is no hole, index
 * 1 is returned.
 */
static int find_next_log_num() {
  int i, next_gen, log_gen;
  DIR *dirp;
  struct dirent *direntp;
  int log_exists[LOG_MAX_GENERATIONS+1];
  int stub_len = strlen(LOG_STUBNAME);

  /* Initialize exiting log table */

  for(i=log_generations; i>=0; i--)
    log_exists[i] = 0;
  dirp = opendir(log_dir);
  if(!dirp) {
    error("Can't access log directory %s.\n", log_dir);
    exit(1);
  }

  /* Check the directory for existing logs */

  while((direntp=readdir(dirp)) != NULL) {
    if(strncmp(direntp->d_name,LOG_STUBNAME,stub_len)==0) {
      int num = atoi(direntp->d_name+stub_len);
      if(num < 1 || num > log_generations)
	continue;
      log_exists[num] = 1;
    }
  }	
  closedir(dirp);

  /* Find out the next available log file number */

  next_gen = 0;
  for(i=log_generations; i>=0; i--) {
    if(log_exists[i])
      if(next_gen)
	break;
      else 
	;
    else
      next_gen = i;
  }

  /* Find out the current log file number */

  if(next_gen)
    log_gen = prev_log(next_gen);
  else
    log_gen = 1;

  return log_gen;
} /* find_next_log_num() */

/* open_log()
 * Opens a log file (with given index) for writing. Writing may be
 * at the end or a trucnating write, according to flags.
 * A LOGGING STARTED and time stamp message is inserted into the log file
 */
static int open_log(int log_num, int flags) {
  char buf[FILENAME_MAX];
  time_t now;
  int lfd;

  /* Remove the next log (to keep a "hole" in the log sequence) */
  sprintf(buf, "%s/%s%d", log_dir, LOG_STUBNAME, next_log(log_num));
  unlink(buf);

  /* Create or continue on the current log file */
  sprintf(buf, "%s/%s%d", log_dir, LOG_STUBNAME, log_num);
  if((lfd = open(buf, flags, LOG_PERM))<0){
    error("Can't open log file %s.", buf);
    exit(1);
  }

  /* Write a LOGGING STARTED and time stamp into the log file */
  time(&now);
  sprintf(buf, "\n=====\n===== LOGGING STARTED %s=====\n", ctime(&now));
  if(write(lfd, buf, strlen(buf)) != strlen(buf))
    status("Error in writing to log.\n");

#if USE_FSYNC
  fsync(lfd);
#endif

  return lfd;
}

/* write_to_log()
 * Writes a message to a log file. If the current log file is full,
 * a new log file is opened.
 */
static void write_to_log(int* lfd, int* log_num, char* buf, int len) {
  int size;

  /* Decide if new logfile needed, and open if so */
  
  size = lseek(*lfd,0,SEEK_END);
  if(size+len > log_maxsize) {
    close(*lfd);
    *log_num = next_log(*log_num);
    *lfd = open_log(*log_num, O_RDWR|O_CREAT|O_TRUNC|O_SYNC); 
  }

  /* Write to log file */

  if(write(*lfd, buf, len) != len) {
    status("Error in writing to log.\n");
  }

#if USE_FSYNC
  fsync(lfd);
#endif
}

/* create_fifo()
 * Creates a new fifo with the given name and permission.
 */
static int create_fifo(char *name, int perm)
{
  if ((mknod(name, S_IFIFO | perm, 0) < 0) && (errno != EEXIST))
    return -1;
  return 0;
}


/* open_pty_master()
 * Find a master device, open and return fd and slave device name
 */

static int open_pty_master(char **ptyslave)
{
  int mfd;
  char *major, *minor;

  static char majorchars[] = "pqrstuvwxyzabcdePQRSTUVWXYZABCDE";
  static char minorchars[] = "0123456789abcdefghijklmnopqrstuv"
			     "wxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_+";

  /* In the old time the names where /dex/ptyXY where */
  /* X is in "pqrs" and Y in "0123456789abcdef" but FreeBSD */
  /* and some Linux version has extended this. */

  /* This code could probebly be improved alot. For example look at */
  /* http://www.xcf.berkeley.edu/~ali/K0D/UNIX/PTY/code/pty.c.html */
  /* http://www.xcf.berkeley.edu/~ali/K0D/UNIX/PTY/code/upty.h.html */

  {
    /* New style devpts or devfs /dev/pty/{m,s}{0,1....} */

    static char ptyname[] = "/dev/pty/mX";

    for (minor = minorchars; *minor; minor++) {
      ptyname[10] = *minor;
      if ((mfd = open(ptyname, O_RDWR, 0)) >= 0) {
	ptyname[9] = 's';
	*ptyslave = ptyname;
	return mfd;
      }
    }
  }

  {
    /* Unix98 style /dev/ptym/ptyXY and /dev/pty/ttyXY */

    static char ptyname[] = "/dev/ptym/ptyXY";
    static char ttyname[] = "/dev/pty/ttyXY";

    for (major = majorchars; *major; major++) {
      ptyname[13] = *major;
      for (minor = minorchars; *minor; minor++) {
	ptyname[14] = *minor;
	if ((mfd = open(ptyname, O_RDWR, 0)) >= 0) {
	  ttyname[12] = *major;
	  ttyname[13] = *minor;
	  *ptyslave = ttyname;
	  return mfd;
	}
      }
    }
  }

  {
    /* Old style /dev/ptyXY */

    static char ptyname[] = "/dev/ptyXY";

    for (major = majorchars; *major; major++) {
      ptyname[8] = *major;
      for (minor = minorchars; *minor; minor++) {
	ptyname[9] = *minor;
	if ((mfd = open(ptyname, O_RDWR, 0)) >= 0) {
	  ptyname[5] = 't';
	  *ptyslave = ptyname;
	  return mfd;
	}
      }
    }
  }

  return -1;
}

static int open_pty_slave(char *name)
{
  int sfd;
#ifdef DEBUG
  struct termios tty_rmode;
#endif

  if ((sfd = open(name, O_RDWR, 0)) < 0) {
    return -1;
  }

#ifdef DEBUG
  if (tcgetattr(sfd, &tty_rmode) , 0) {
    fprintf(stderr, "Cannot get terminals current mode\n");
    exit(-1);
  }
  show_terminal_settings(&tty_rmode);
#endif

  return sfd;
}

/* exec_shell()
 * Executes the named command (in argv format) in a /bin/sh. IO redirection
 * should already have been taken care of, and this process should be the
 * child of a fork.
 */
static void exec_shell(char **argv)
{
  char *sh, **vp;
  int i;

  sh = "/bin/sh";
  if ((argv[0] = strrchr(sh, '/')) != NULL)
    argv[0]++;
  else
    argv[0] = sh;
  argv[1] = "-c";
  status("Args before exec of shell:\n");
  for (vp = argv, i = 0; *vp; vp++, i++)
    status("argv[%d] = %s\n", i, *vp);
  execv(sh, argv);
  error("Could not execve\n");
}

/* status()
 * Prints the arguments to a status file
 * Works like printf (see vfrpintf)
 */
static void status(const char *format,...)
{
  va_list args;
  time_t now;

  if (stdstatus == NULL)
    stdstatus = fopen(STATUSFILE, "w");
  if (stdstatus == NULL)
    return;
  now = time(NULL);
  fprintf(stdstatus, "run_erl [%d] %s", (int)getpid(), ctime(&now));
  va_start(args, format);
  vfprintf(stdstatus, format, args);
  va_end(args);
  fflush(stdstatus);
}

/* error()
 * Prints the arguments to stderr
 * Works like printf (see vfrpintf)
 */
static void error(const char *format,...)
{
  va_list args;
  time_t now;

  now = time(NULL);
  fprintf(stderr, "run_erl [%d] %s", (int)getpid(), ctime(&now));
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
}

#ifdef DEBUG

#define S(x)  ((x) > 0 ? 1 : 0)

static void show_terminal_settings(struct termios *t)
{
  printf("c_iflag:\n");
  printf("Signal interrupt on break:   BRKINT  %d\n", S(t->c_iflag & BRKINT));
  printf("Map CR to NL on input:       ICRNL   %d\n", S(t->c_iflag & ICRNL));
  printf("Ignore break condition:      IGNBRK  %d\n", S(t->c_iflag & IGNBRK));
  printf("Ignore CR:                   IGNCR   %d\n", S(t->c_iflag & IGNCR));
  printf("Ignore char with par. err's: IGNPAR  %d\n", S(t->c_iflag & IGNPAR));
  printf("Map NL to CR on input:       INLCR   %d\n", S(t->c_iflag & INLCR));
  printf("Enable input parity check:   INPCK   %d\n", S(t->c_iflag & INPCK));
  printf("Strip character              ISTRIP  %d\n", S(t->c_iflag & ISTRIP));
  printf("Enable start/stop input ctrl IXOFF   %d\n", S(t->c_iflag & IXOFF));
  printf("ditto output ctrl            IXON    %d\n", S(t->c_iflag & IXON));
  printf("Mark parity errors           PARMRK  %d\n", S(t->c_iflag & PARMRK));
  printf("\n");
  printf("c_oflag:\n");
  printf("Perform output processing    OPOST   %d\n", S(t->c_oflag & OPOST));
  printf("\n");
  printf("c_cflag:\n");
  printf("Ignore modem status lines    CLOCAL  %d\n", S(t->c_cflag & CLOCAL));
  printf("\n");
  printf("c_local:\n");
  printf("Enable echo                  ECHO    %d\n", S(t->c_lflag & ECHO));
  printf("\n");
  printf("c_cc:\n");
  printf("c_cc[VEOF]                           %d\n", t->c_cc[VEOF]);
}

#endif /* DEBUG */

/* END */
