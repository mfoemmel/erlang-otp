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
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif  VXWORKS
#include <vxWorks.h>
#include <ifLib.h>
#include <sockLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif

#ifdef DEBUG_DIST
#include <stdio.h>
extern int ei_trace_distribution;
#endif

#include <stdlib.h>
#include <string.h>

#include "erl_interface.h"
#include "erl_config.h"
#include "putget.h"

#ifndef SMALLBUF
#define SMALLBUF 512
#endif

/* stop the specified node */
int erl_unpublish(const char *alive)
{
  unsigned char buf[SMALLBUF];
  char *s = buf;
  int len = 1 + strlen(alive);
  int fd;

  put16be(s,len);
  put8(s,ERL_EPMD_STOP_REQ);
  strcpy(buf+3, alive);

  if ((fd = erl_epmd_connect(NULL)) < 0) return fd;

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr,"-> STOP %s\n",alive);
#endif
  
  if (readsocket(fd, buf, 7) != 7) {
    closesocket(fd);
    erl_errno = EIO;
    return -1; 
  }
  closesocket(fd);
  buf[7]=(char)0; /* terminate the string */
  
  if (!strcmp("STOPPED",buf)) {
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr,"<- STOPPED (success)\n");
#endif
    return 0;
  }
  else if (!strcmp("NOEXIST",buf)) {
#ifdef DEBUG_DIST
      if (ei_trace_distribution > 2) fprintf(stderr,"<- NOEXIST (failure)\n");
#endif
      erl_errno = EIO;
    return -1;
  }
  else {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) fprintf(stderr,"<- unknown (failure)\n");
#endif
    erl_errno = EIO;
    return -1; /* this shouldn't happen */
  }
}


