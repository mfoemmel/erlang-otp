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
 * Module: to_erl.c
 * 
 * This module implements a process that opens two specified FIFOs, one
 * for reading and one for writing; reads from its stdin, and writes what
 * ithas read to the write FIF0; reads from the read FIFO, and writes to
 * its stdout.
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
#include <string.h>
#include <termios.h>
#include <dirent.h>
#include <signal.h>
#include <stropts.h>

#define noDEBUG

#define PIPE_DIR        "/tmp/"
#define PIPE_STUBNAME   "erlang.pipe"
#define PIPE_STUBLEN    strlen(PIPE_STUBNAME)

#ifdef DEBUG
#define STATUS(s)  { fprintf(stderr, (s)); fflush(stderr); }
#else
#define STATUS(s)
#endif

#ifndef FILENAME_MAX
#define FILENAME_MAX 250
#endif

static struct termios tty_smode, tty_rmode;
static int tty_eof = 0;
static int ctrlc = 0;

#ifdef DEBUG
static void show_terminal_settings(struct termios *);
#endif

static void handle_ctrlc(int sig) {
  /* Reinstall the handler, and signal break flag */
  signal(SIGINT,handle_ctrlc);
  ctrlc = 1;
}  

static void usage(char *pname)
{
  fprintf(stderr, "Usage: %s [pipe_name|pipe_dir/]\n", pname);
}

int main(int argc, char **argv)
{
  char  FIFO1[FILENAME_MAX], FIFO2[FILENAME_MAX];
  int i, len, wfd, rfd, result = 0;
  fd_set readfds;
  char buf[BUFSIZ];
  char pipename[FILENAME_MAX];

  if (argc < 1) {
    usage(argv[0]);
    exit(1);
  }

#ifdef DEBUG
  fprintf(stderr, "%s: pid is : %d\n", argv[0], getpid());
#endif

  if(argv[1])
    strcpy(pipename,argv[1]);
  else
    strcpy(pipename,PIPE_DIR);

  if(*pipename && pipename[strlen(pipename)-1] == '/') {
    /* The user wishes us to find a unique pipe name in the specified */
    /* directory */
    int highest_pipe_num = 0;
    DIR *dirp;
    struct dirent *direntp;

    dirp = opendir(pipename);
    if(!dirp) {
      fprintf(stderr, "Can't access pipe directory %s.\n", pipename);
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
	    (highest_pipe_num?"%s.%d":"%s"),PIPE_STUBNAME,highest_pipe_num);
  } /* if */

  /* read FIFO */
  strncpy(FIFO1, pipename, FILENAME_MAX);
  strncat(FIFO1, ".r", FILENAME_MAX - strlen(FIFO1));
  /* write FIFO */
  strncpy(FIFO2, pipename, FILENAME_MAX);
  strncat(FIFO2, ".w", FILENAME_MAX - strlen(FIFO2));

  if ((rfd = open (FIFO1, O_RDONLY|O_NDELAY, 0)) < 0) {
#ifdef DEBUG
    fprintf(stderr, "Could not open FIFO %s for reading.\n", FIFO1);
#endif
    fprintf(stderr, "No running Erlang on pipe %s.\n", pipename);
    exit(1);
  }
#ifdef DEBUG
  fprintf(stderr, "to_erl: %s opened for reading\n", FIFO1);
#endif

  if ((wfd = open (FIFO2, O_WRONLY|O_NDELAY, 0)) < 0) {
#ifdef DEBUG
    fprintf(stderr, "Could not open FIFO %s for writing.\n", FIFO2);
#endif
    fprintf(stderr, "No running Erlang on pipe %s.\n", pipename);
    close(rfd);
    exit(1);
  }
#ifdef DEBUG
  fprintf(stderr, "to_erl: %s opened for writing\n", FIFO2);
#endif

  fprintf(stderr, "Attaching to %s (^D to exit)\n\n", pipename);

  /* Set break handler to our handler */
  signal(SIGINT,handle_ctrlc);

  /* 
   * Save the current state of the terminal, and set raw mode.
   */
  if (tcgetattr(0, &tty_rmode) , 0) {
    fprintf(stderr, "Cannot get terminals current mode\n");
    exit(-1);
  }
  tty_smode = tty_rmode;
  tty_eof = '\004'; /* Ctrl+D to exit */
#ifdef DEBUG
  show_terminal_settings(&tty_rmode);
#endif
  tty_smode.c_iflag =
    0*IGNBRK |/*Ignore break condition.*/
    1*BRKINT |/*Signal interrupt on break.*/
    1*IGNPAR |/*Ignore characters with parity errors.*/
    0*PARMRK |/*Mark parity errors.*/
    0*INPCK  |/*Enable input parity check.*/
    1*ISTRIP |/*Strip character.*/
    0*INLCR  |/*Map NL to CR on input.*/
    0*IGNCR  |/*Ignore CR.*/
    0*ICRNL  |/*Map CR to NL on input.*/
    0*IUCLC  |/*Map upper-case to lower-case on input.*/
    0*IXON   |/*Enable start/stop output control.*/
    0*IXANY  |/*Enable any character to restart output.*/
    0*IXOFF  |/*Enable start/stop input control.*/
    0*IMAXBEL;/*Echo BEL on input line too long.*/

  tty_smode.c_oflag =
    1*OPOST  |/*Post-process output.*/
    0*OLCUC  |/*Map lower case to upper on output.*/
    1*ONLCR  |/*Map NL to CR-NL on output.*/
    0*OCRNL  |/*Map CR to NL on output.*/
    0*ONOCR  |/*No CR output at column 0.*/
    0*ONLRET |/*NL performs CR function.*/
    0*OFILL  |/*Use fill characters for delay.*/
    0*OFDEL  |/*Fill is DEL, else NULL.*/
              /*Select newline delays:*/
    1*NL0    |
    0*NL1    |
              /*Select carriage-return delays:*/
    1*CR0    |
    0*CR1    |
    0*CR2    |
    0*CR3    |
              /*Select horizontal tab delays:*/
    1*TAB0   |/*or tab expansion:*/
    0*TAB1   |
    0*TAB2   |
    0*TAB3   |/*Expand tabs to spaces.*/
    1*XTABS  |/*Expand tabs to spaces.*/
              /*Select backspace delays:*/
    1*BS0    |
    0*BS1    |
              /*Select vertical tab delays:*/
    1*VT0    |
    0*VT1    |
              /*Select form feed delays:*/
    1*FF0    |
    0*FF1    ;

  /* JALI: removed setting the tty_smode.c_cflag flags, since this is not */
  /* advisable if this is a *real* terminal, such as the console. In fact */
  /* this may hang the entire machine, deep, deep down (signalling break */
  /* or toggling the abort switch doesn't help) */

  tty_smode.c_lflag =
    0*ISIG   |/*Enable signals.*/
    0*ICANON |/*Canonical input (erase and kill processing).*/
    0*XCASE  |/*Canonical upper/lower presentation.*/
    0*ECHO   |/*Enable echo.*/
    0*ECHOE  |/*Echo erase character as BS-SP-BS.*/
    0*ECHOK  |/*Echo NL after kill character.*/
    0*ECHONL |/*Echo NL.*/
    0*NOFLSH |/*Disable flush after interrupt or quit.*/
    0*TOSTOP |/*Send SIGTTOU for background output.*/
    0*ECHOCTL|/*Echo control characters as ^char, delete as ^?.*/
    0*ECHOPRT|/*Echo erase character as character erased.*/
    0*ECHOKE |/*BS-SP-BS erase entire line on line kill.*/
    0*FLUSHO |/*Output is being flushed.*/
    0*PENDIN |/*Retype pending input at next read or input character.*/
    0*IEXTEN ;/*Enable extended (implementation-defined) functions.*/

   tty_smode.c_cc[VMIN]      =0;/* Note that VMIN is the same as VEOF! */
   tty_smode.c_cc[VTIME]     =0;/* Note that VTIME is the same as VEOL! */
   tty_smode.c_cc[VINTR]     =3;

   tcsetattr(0, TCSANOW, &tty_smode);

#ifdef DEBUG
   show_terminal_settings(&tty_smode);
#endif
  /*
   * Write a ^R to the FIFO which causes the other end to redisplay
   * the input line.
   */
  write(wfd, "\022", 1);
  /*
   * read and write
   */
  while (1) {
    FD_ZERO(&readfds);
    FD_SET(0, &readfds);
    FD_SET(rfd, &readfds);
    if (select(rfd + 1, &readfds, NULL, NULL, NULL) < 0) {
      if(ctrlc) {
	FD_ZERO(&readfds);
      } else {
	fprintf(stderr, "Error in select.\n");
	result = -1;
	break;
      }
    }
    /*
     * Read from terminal, write to FIFO
     */
    if (ctrlc || FD_ISSET(0, &readfds)) {
      STATUS("Terminal read; ");
      if(ctrlc) {
	ctrlc = 0;
	fprintf(stderr, "[Break]\n\r");
	buf[0] = '\003';
	len = 1;
      } else if ((len = read(0, buf, BUFSIZ)) <= 0) {
	close(rfd);
	close(wfd);
	if (len < 0) {
	  fprintf(stderr, "Error in reading from stdin.\n");
	  result = -1;
	} else {
	  fprintf(stderr, "[EOF]\n\r");
	}
	break;
      }
      /* check if there is an eof character in input */
      for (i = 0; i < len && buf[i] != tty_eof; i++);
      if (buf[i] == tty_eof) {
	fprintf(stderr, "[Quit]\n\r");
	break;
      }
      STATUS("FIFO write; \"");

#ifdef DEBUG
      write(1, buf, len);
#endif
      if (write(wfd, buf, len) != len) {
	fprintf(stderr, "Error in writing to FIFO.\n");
	close(rfd);
	close(wfd);
	result = -1;
	break;
      }
      STATUS("\" OK\r\n");
    }
    /*
     * Read from FIFO, write to terminal 
     */
    if (FD_ISSET(rfd, &readfds)) {
      STATUS("FIFO read: ");
      if ((len = read(rfd, buf, BUFSIZ)) <= 0) {
	close(rfd);
	close(wfd);
	if (len < 0) {
	  fprintf(stderr, "Error in reading from FIFO.\n");
	  result = -1;
	} else
	  fprintf(stderr, "[End]\n\r");
	break;
      }
      STATUS("Terminal write: \"");
      if (write(1, buf, len) != len) {
	fprintf(stderr, "Error in writing to terminal.\n");
	close(rfd);
	close(wfd);
	result = -1;
	break;
      }
      STATUS("\" OK\r\n");
    }
  }
  /* 
   * Reset terminal characterstics 
   * XXX
   */
  tcsetattr(0, TCSANOW, &tty_rmode);
  return 0;
}

#ifdef DEBUG
#define S(x)  ((x) > 0 ? 1 : 0)

static void show_terminal_settings(struct termios *t)
{
  fprintf(stderr,"c_iflag:\n");
  fprintf(stderr,"Signal interrupt on break:   BRKINT  %d\n", S(t->c_iflag & BRKINT));
  fprintf(stderr,"Map CR to NL on input:       ICRNL   %d\n", S(t->c_iflag & ICRNL));
  fprintf(stderr,"Ignore break condition:      IGNBRK  %d\n", S(t->c_iflag & IGNBRK));
  fprintf(stderr,"Ignore CR:                   IGNCR   %d\n", S(t->c_iflag & IGNCR));
  fprintf(stderr,"Ignore char with par. err's: IGNPAR  %d\n", S(t->c_iflag & IGNPAR));
  fprintf(stderr,"Map NL to CR on input:       INLCR   %d\n", S(t->c_iflag & INLCR));
  fprintf(stderr,"Enable input parity check:   INPCK   %d\n", S(t->c_iflag & INPCK));
  fprintf(stderr,"Strip character              ISTRIP  %d\n", S(t->c_iflag & ISTRIP));
  fprintf(stderr,"Enable start/stop input ctrl IXOFF   %d\n", S(t->c_iflag & IXOFF));
  fprintf(stderr,"ditto output ctrl            IXON    %d\n", S(t->c_iflag & IXON));
  fprintf(stderr,"Mark parity errors           PARMRK  %d\n", S(t->c_iflag & PARMRK));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_oflag:\n");
  fprintf(stderr,"Perform output processing    OPOST   %d\n", S(t->c_oflag & OPOST));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_cflag:\n");
  fprintf(stderr,"Ignore modem status lines    CLOCAL  %d\n", S(t->c_cflag & CLOCAL));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_local:\n");
  fprintf(stderr,"Enable echo                  ECHO    %d\n", S(t->c_lflag & ECHO));
  fprintf(stderr,"\n");
  fprintf(stderr,"c_cc:\n");
  fprintf(stderr,"c_cc[VEOF]                           %d\n", t->c_cc[VEOF]);
}
#endif
