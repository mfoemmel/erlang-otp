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

#include "ose.h"
#include "malloc.h"
#include "errno.h"
#include "sys/stat.h"
#include "unistd.h"
#include "shell.h"
#include "fm.sig"

#include "inetlink.h"
#include "inet.h"
#include "inetutil.h"
#include "inet.sig"

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#include "time.h"
#include "rtc.h"

#define HOST "134.138.177.125"
#define PORT 5555

void set_clock(void);

static int receive_stuff(int s, char *buf, int nbyte) {
  int result;
  int received = 0;

  while(1) {
    if((result = recv(s, (void*)(buf+received), nbyte-received, 0)) < 0) {
      fprintf(stderr, "Read fails (%d), errno %d\n", result, errno);
      close(s);
      return -1;
    }
    received += result;
    if((result == 0) || (received == nbyte)) { /* done */
      buf[received] = '\0';
      return received;
    }      
  }
}

void set_clock() {
  struct sockaddr_in addr;
  int s, result, n, i=0;
  PROCESS inet_, startefs_;
  struct timeval tv;
  struct TimePair tvp0, tvp1;
  unsigned long secs; 
  time_t t;
  char buf[20];

  while(!hunt("ose_inet", 0, &inet_, NULL)) {
    fprintf(stderr, "waiting for ose_inet...\n");
    delay(2000);
  }

  memset(&addr, 0, sizeof (addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr(HOST);
  addr.sin_port = htons(PORT);

  /* open socket */
  if((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fprintf(stderr, "Open fails: %d)\n", errno);
    return;
  }

  rtc_get_time(&tvp0);  

  /* connect to server */
  result = connect(s, (struct sockaddr *)&addr, sizeof(struct sockaddr_in));
  if((result < 0) && (errno != EINPROGRESS)) {
    fprintf(stderr, "Unable to read time from host (%s:%d), errno %d\n", HOST, PORT, errno);
    close(s);
    return;
  }
  
  if((n = receive_stuff(s, buf, 19)) < 0) {
    fprintf(stderr, "Nothing received! errno %d\n", errno);
    return;
  }
  rtc_get_time(&tvp1);
  close(s);
  secs = tvp1.seconds - tvp0.seconds;
  t = (time_t)atol(buf);

  tvp0.seconds = t + secs/2;	/* compensate for TCP delay */
  tvp0.micros  = 0;
  /* temporary fix: need to add 2 hours (probably a UTC->localtime problem, 
     fix it properly later!) */
  tvp0.seconds += 7200;
  rtc_set_time(&tvp0);
  /* check that time was set properly and print it */
  rtc_get_time(&tvp1);
  t = tvp1.seconds;
  printf("\nClock set: %s\n", asctime(gmtime(&t)));
}

