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
#include "ei_connect.h"

#ifndef SMALLBUF
#define SMALLBUF 512
#endif




/* publish our listen port and alive name */
/* return the (useless) creation number */
static int 
ei_epmd_r3_publish (int port, const char *alive)
{
  char buf[SMALLBUF];
  char *s = buf;
  int fd;
  int len = strlen(alive) + 3;
  int res,creation;

  s = buf;
  put16be(s,len);
  put8(s,ERL_EPMD_ALIVE_REQ);
  put16be(s,port); 
  strcpy(s, alive);

  if ((fd = erl_epmd_connect(NULL)) < 0) return fd;

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr,"-> ALIVE_REQ alive=%s port=%d\n",alive,port);
#endif

  if (readsocket(fd, buf, 3) != 3) {
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

  s = buf;
  if ((res=get8(s)) != ERL_EPMD_ALIVE_OK_RESP) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) fprintf(stderr,"<- ALIVE_NOK result=%d (failure)\n",res);
#endif
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

  creation = get16be(s);
  
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr,"<- ALIVE_OK creation=%d\n",creation);
#endif

  /* Don't close fd here! It keeps us registered with epmd */

  /* probably should save fd so we can close it later... */
  /* epmd_saveconn(OPEN,fd,alive); */

  /* return the creation number, for no good reason */
  /* return creation; */

  /* no! return the descriptor */
  return fd;
}

/* publish our listen port and alive name */
/* return the (useless) creation number */
/* this protocol is a lot more complex than the old one */
static int 
ei_epmd_r4_publish (int port, const char *alive)
{
  char buf[SMALLBUF];
  char *s = buf;
  int fd;
  int elen = 0;
  int nlen = strlen(alive);
  int len = elen + nlen + 13; /* hard coded: be careful! */
  int n;
  int res, creation;
  
  s = buf;
  put16be(s,len);

  put8(s,ERL_EPMD_ALIVE2_REQ);
  put16be(s,port); /* port number */
  put8(s,'h');            /* h = r4 hidden node */
  put8(s, ERL_MYPROTO);      /* protocol 0 ?? */
  put16be(s,ERL_DIST_HIGH);   /* highest understood version: 1 = R4 */
  put16be(s,ERL_DIST_LOW);    /* lowest:  0 = R3 */
  put16be(s,nlen);        /* length of alivename */
  strcpy(s, alive);
  s += nlen;
  put16be(s,elen);        /* length of extra string = 0 */
                          /* no extra string */

  if ((fd = erl_epmd_connect(NULL)) < 0) return fd;

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2)
    fprintf(stderr,"-> ALIVE2_REQ alive=%s port=%d ntype=%d "
	    "proto=%d dist-high=%d dist-low=%d\n",
	    alive,port,'H',ERL_MYPROTO,ERL_DIST_HIGH,ERL_DIST_LOW);
#endif
  
  if ((n = readsocket(fd, buf, 4)) != 4) {
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) fprintf(stderr,"<- CLOSE\n");
#endif
    closesocket(fd);
    return -2;			/* version mismatch */
  }
  /* Don't close fd here! It keeps us registered with epmd */
  s = buf;
  if (((res=get8(s)) != ERL_EPMD_ALIVE2_RESP)) {  /* response */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) {
      fprintf(stderr,"<- unknown (%d)\n",res);
      fprintf(stderr,"-> CLOSE\n");
    }
#endif
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr,"<- ALIVE2_RESP");
#endif

  if (((res=get8(s)) != 0)) {           /* 0 == success */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 2) fprintf(stderr," result=%d (fail)\n",res);
#endif
    closesocket(fd);
    erl_errno = EIO;
    return -1;
  }

  creation = get16be(s);
  
#ifdef DEBUG_DIST
  if (ei_trace_distribution > 2) fprintf(stderr," result=%d (ok) creation=%d\n",res,creation);
#endif

  /* probably should save fd so we can close it later... */
  /* epmd_saveconn(OPEN,fd,alive); */

  /* return the creation number, for no good reason */
  /* return creation;*/

  /* no - return the descriptor */
  return fd;
}

extern int 
erl_epmd_publish (int port, const char *alive)
{
  int i;

  /* try the new one first, then the old one */
  i = ei_epmd_r4_publish(port,alive);

  /* -2: new protocol not understood */
  if (i == -2) i = ei_epmd_r3_publish(port,alive);

  return i;
}


/* 
 * Publish a name for our C-node. 
 * a file descriptor is returned - close it to unpublish.
 * 
 */
int ei_publish(ei_cnode* ec, int port)
{
  return erl_epmd_publish(port, ei_thisalivename(ec));
}
