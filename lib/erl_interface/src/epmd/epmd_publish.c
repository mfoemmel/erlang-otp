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

#include <stdlib.h>
#include <string.h>

#include "eidef.h"
#include "ei_internal.h"
#include "putget.h"
#include "erl_interface.h"
#include "ei_epmd.h"


/* publish our listen port and alive name */
/* return the (useless) creation number */
static int ei_epmd_r3_publish (int port, const char *alive)
{
  char buf[EPMDBUF];
  char *s = buf;
  int fd;
  int len = strlen(alive) + 3;
  int res,creation;

  s = buf;
  put16be(s,len);
  put8(s,EI_EPMD_ALIVE_REQ);
  put16be(s,port); 
  strcpy(s, alive);

  if ((fd = ei_epmd_connect(NULL)) < 0) return fd;

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    return -1;
  }

  EI_TRACE_CONN2("ei_epmd_r3_publish",
		 "-> ALIVE_REQ alive=%s port=%d",alive,port);

  if (readsocket(fd, buf, 3) != 3) {
    closesocket(fd);
    return -1;
  }

  s = buf;
  if ((res=get8(s)) != EI_EPMD_ALIVE_OK_RESP) {
      EI_TRACE_ERR1("ei_epmd_r3_publish",
		    "<- ALIVE_NOK result=%d (failure)",res);
    closesocket(fd);
    return -1;
  }

  creation = get16be(s);

  EI_TRACE_CONN1("ei_epmd_r3_publish","<- ALIVE_OK creation=%d",creation);

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
static int ei_epmd_r4_publish (int port, const char *alive)
{
  char buf[EPMDBUF];
  char *s = buf;
  int fd;
  int elen = 0;
  int nlen = strlen(alive);
  int len = elen + nlen + 13; /* hard coded: be careful! */
  int n;
  int res, creation;
  
  s = buf;
  put16be(s,len);

  put8(s,EI_EPMD_ALIVE2_REQ);
  put16be(s,port); /* port number */
  put8(s,'h');            /* h = r4 hidden node */
  put8(s, EI_MYPROTO);      /* protocol 0 ?? */
  put16be(s,EI_DIST_HIGH);   /* highest understood version: 1 = R4 */
  put16be(s,EI_DIST_LOW);    /* lowest:  0 = R3 */
  put16be(s,nlen);        /* length of alivename */
  strcpy(s, alive);
  s += nlen;
  put16be(s,elen);        /* length of extra string = 0 */
                          /* no extra string */

  if ((fd = ei_epmd_connect(NULL)) < 0) return fd;

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    return -1;
  }

  EI_TRACE_CONN6("ei_epmd_r4_publish",
		 "-> ALIVE2_REQ alive=%s port=%d ntype=%d "
		 "proto=%d dist-high=%d dist-low=%d",
		 alive,port,'H',EI_MYPROTO,EI_DIST_HIGH,EI_DIST_LOW);
  
  if ((n = readsocket(fd, buf, 4)) != 4) {
    EI_TRACE_ERR0("ei_epmd_r4_publish","<- CLOSE");
    closesocket(fd);
    return -2;			/* version mismatch */
  }
  /* Don't close fd here! It keeps us registered with epmd */
  s = buf;
  if (((res=get8(s)) != EI_EPMD_ALIVE2_RESP)) {  /* response */
    EI_TRACE_ERR1("ei_epmd_r4_publish","<- unknown (%d)",res);
    EI_TRACE_ERR0("ei_epmd_r4_publish","-> CLOSE");
    closesocket(fd);
    return -1;
  }

  EI_TRACE_CONN0("ei_epmd_r4_publish","<- ALIVE2_RESP");

  if (((res=get8(s)) != 0)) {           /* 0 == success */
      EI_TRACE_ERR1("ei_epmd_r4_publish"," result=%d (fail)",res);
    closesocket(fd);
    return -1;
  }

  creation = get16be(s);

  EI_TRACE_CONN2("ei_epmd_r4_publish",
		 " result=%d (ok) creation=%d",res,creation);

  /* probably should save fd so we can close it later... */
  /* epmd_saveconn(OPEN,fd,alive); */

  /* return the creation number, for no good reason */
  /* return creation;*/

  /* no - return the descriptor */
  return fd;
}



/* 
 * Publish a name for our C-node. 
 * a file descriptor is returned - close it to unpublish.
 * 
 */
int ei_publish(ei_cnode* ec, int port)
{
  const char *alive = ei_thisalivename(ec);

  /* try the new one first, then the old one */
  int res = ei_epmd_r4_publish(port,alive);

  /* -2: new protocol not understood */
  if (res == -2)
    res = ei_epmd_r3_publish(port,alive);

  return res;
}
