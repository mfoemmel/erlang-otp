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
 *  File:      wd_keeper.c
 *  Purpose:   Hardware watchdog handler for Solaris on FORCE 5TE
 *
 * DESCRIPTION
 *
 *   This program enables the hardware watchdog of the CPU board,
 *   and then keeps the little puppy happy as long as it receives
 *   periodical signals from the outside. If there is too long a
 *   gap between these signals, the wd_keeper stops resetting the
 *   watchdog, which resets the system in a few seconds.
 *
 *   Once successfully started, SIGUSR1 signals are expected from
 *   the outside within the interval specified at startup. The hardware
 *   watchdog itself wannot be disabled once enabled, but by sending
 *   wd_keeper a SIGFREEZE, it keeps the dog happy even if no SIGUSR1
 *   signals arrive. Normal operation is resumed upon reception of a
 *   SIGTHAW signal.
 *
 * SYNOPSIS
 *
 *   wd_keeper [timeout [nice_value]]
 *
 *       timeout is the maximum time in seconds between consecutive
 *            SIGUSR1 signals. Defaults to 60 seconds.
 *       nice_value is passed to nice(2) to lower or (sooner) raise
 *            the priority of the wd_keeper process. 
 *
 *   When the program has started, it prints an 'A' on stdout to
 *   indicate the successful start of the hardware watchdog, and
 *   otherwise an 'X' to indicate a failure.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>

/* #include <sys/vme.h> */
#include "sys_vme.h"		/* This file is really the sys/vme.h file if the
				   FORCEvme package is installed on the system */

#define WD_RESET_INTERVAL 400	/* 400 msec (the HW watchdog requires a pat at least
				   every 830 ms, and we want some margins) */

extern int usleep(unsigned int useconds);

int main(int argc, char **argv);
void pet_dog(int vme);
void arm_wd(int sig);
void disarm_wd(int sig);
void pet_wd_wd(int sig);

/* Make these two volatile sig_atomic_t, thereby avoiding aggressive
   optimizations, as they will be modified from within the interrupt
   handler. It might be even better to have a special variable
   indicating that wd_wd_pet has been called, and only change wd_count
   from within the main loop. */
volatile sig_atomic_t wd_disarmed = 0; /* Armed */
volatile sig_atomic_t wd_count;
int wd_count_startval;

int
main(int argc, char **argv)
{
  int vme, res;
  unsigned int timeout = 0;

  if (argc >= 2) {
    timeout = (unsigned int)atoi(argv[1]);
  }
  if (!timeout) {
    timeout = 60;		/* Default */
  }

  wd_count_startval = (int)((1000 * timeout) / WD_RESET_INTERVAL);
  wd_count = wd_count_startval;

  if (sigset(SIGFREEZE, disarm_wd) != 0) {
    fprintf(stderr, "Error installing interrupt handler for SIGFREEZE: %d\n", errno);
    printf("X");
    fflush(stdout);
    return 1;
  }
  
  if (sigset(SIGTHAW, arm_wd) != 0) {
    fprintf(stderr, "Error installing interrupt handler for SIGTHAW: %d\n", errno);
    printf("X");
    fflush(stdout);
    return 1;
  }

  if (sigset(SIGUSR1, pet_wd_wd) != 0) {
    fprintf(stderr, "Error installing interrupt handler for SIGUSR1: %d\n", errno);
    printf("X");
    fflush(stdout);
    return 1;
  }
  
  if (argc >= 3) {
    int niceval = atoi(argv[2]);
    if (niceval) {
      if ((res = nice(niceval)) == -1)
	{
	  perror("Error setting process priority");
	  printf("X");
	  fflush(stdout);
	  return 1;
	}
    }
  }
  
  if ((vme=open("/dev/vme32d32", O_RDWR)) < 0) {
    perror("Error opening /dev/vme32d32");
    printf("X");
    fflush(stdout);
    return 1;
  }

  /* fprintf(stderr, "HW watchdog started (about to send ACK)\n"); */
  printf("A");			/* Indicate everything is OK */
  fflush(stdout);

  /* Watchdog is armed by petting it once */
  
  while ((wd_count > 0) || wd_disarmed) {
    pet_dog(vme);
    usleep(WD_RESET_INTERVAL*1000);
    if (!wd_disarmed)
      wd_count--;
  }
  
  /* When we get here, reboot is just a few seconds away */
  fprintf(stderr, "Watchdog timeout.\r\nSystem is about to be rebooted\r\n");
  
  return 1;
}
    
void
pet_dog(int vme)
{
  struct vme_ioctl vmeioctl;
  
  vmeioctl.reg_no = VME_A32MAP_REG;
  ioctl(vme, VME_GET_REG, &vmeioctl);
  /*
   * now (re-)enable watchdog timer (can not be disabled)
   * must be retriggered in a 600ms intervall or it will cause a NMI
   * at least then it must be retriggered or it will cause
   * a RESET after ... ms
   */
  vmeioctl.reg_data |= VME_WATCHDOG_ENA;
  ioctl(vme, VME_SET_REG, &vmeioctl);
}

/* NOTE: We'll have to manage w/o the fprintf()s, as I/O isn't allowed in
   signal handlers. Actually, it would probably work, especially of we used
   write(2) instead, but... */

void
arm_wd(int sig)
{
  /* fprintf(stderr, "Hardware watchdog armed.\r\n"); */
  wd_disarmed = 0;
  wd_count = wd_count_startval;
}

void
disarm_wd(int sig)
{
  /*  fprintf(stderr, "Hardware watchdog disarmed.\r\n"); */
  wd_disarmed = 1;
}

void
pet_wd_wd(int sig)
{
  wd_count = wd_count_startval;
}
