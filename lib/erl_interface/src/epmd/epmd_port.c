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

#include "ei_internal.h"
#include "ei_epmd.h"
#include "putget.h"


/* connect to epmd on given host (use NULL for localhost) */
/* 
 * FIXME: Expects IPv4 addresses (excludes IPv6, Appletalk, IRDA and
 * whatever) */
int 
ei_epmd_connect (struct in_addr *inaddr)
{
  struct sockaddr_in saddr;
  int sd;

  memset(&saddr, 0, sizeof(saddr)); 
  saddr.sin_port = htons(EPMD_PORT);   
  saddr.sin_family = AF_INET;

  if (!inaddr) saddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  else memmove(&saddr.sin_addr,inaddr,sizeof(saddr.sin_addr));

  if (((sd = socket(PF_INET, SOCK_STREAM, 0)) < 0)) 
    return -1;

  if (connect(sd,(struct sockaddr *)&saddr,sizeof(saddr)) < 0) 
  {
    closesocket(sd);
    return -1;
  }

  return sd;
}

/* get the given node's listen port using old epmd protocol */
static int ei_epmd_r3_port (struct in_addr *addr, const char *alive)
{
  char buf[EPMDBUF];
  char *s = buf;
  int len = strlen(alive) + 1;
  int fd;
  int port;
#if defined(VXWORKS)
  char ntoabuf[32];
#endif
  
  put16be(s,len);
  put8(s,EI_EPMD_PORT_REQ);
  strcpy(s,alive);

  /* connect to epmd */
  if ((fd = ei_epmd_connect(addr)) < 0) 
  {
      return -1;
  }

  if (writesocket(fd, buf, len+2) != len+2) 
  {
    closesocket(fd);
    return -1;
  }

#ifdef VXWORKS
  /* FIXME use union/macro for level. Correct level? */
  if (ei_tracelevel > 2) {
    inet_ntoa_b(*addr,ntoabuf);
    EI_TRACE_CONN2("ei_epmd_r3_port",
		   "-> PORT_REQ alive=%s ip=%s",alive,ntoabuf);
  }
#else
  EI_TRACE_CONN2("ei_epmd_r3_port",
		 "-> PORT_REQ alive=%s ip=%s",alive,inet_ntoa(*addr));
#endif
  
  if (readsocket(fd, buf, 2) != 2) {
    EI_TRACE_ERR0("ei_epmd_r3_port","<- CLOSE");
    closesocket(fd);
    return -1;
  }
  closesocket(fd);
  s = buf;
  port = get16be(s);

  EI_TRACE_CONN1("ei_epmd_r3_port","<- PORT_RESP port=%d",port);

  return port;
}

static int ei_epmd_r4_port (struct in_addr *addr, const char *alive, int *dist)
{
  char buf[EPMDBUF];
  char *s = buf;
  int len = strlen(alive) + 1;
  int fd;
  int ntype;
  int port;
  int dist_high, dist_low, proto;
  int res;
#if defined(VXWORKS)
  char ntoabuf[32];
#endif
  
  put16be(s,len);
  put8(s,EI_EPMD_PORT2_REQ);
  strcpy(s,alive);
  
  /* connect to epmd */
  if ((fd = ei_epmd_connect(addr)) < 0)
  {
      return -1;
  }

  if (writesocket(fd, buf, len+2) != len+2) {
    closesocket(fd);
    return -1;
  }

#ifdef VXWORKS
  /* FIXME use union/macro for level. Correct level? */
  if (ei_tracelevel > 2) {
    inet_ntoa_b(*addr,ntoabuf);
    EI_TRACE_CONN2("ei_epmd_r4_port",
		   "-> PORT2_REQ alive=%s ip=%s",alive,ntoabuf);
  }
#else
  EI_TRACE_CONN2("ei_epmd_r4_port",
		 "-> PORT2_REQ alive=%s ip=%s",alive,inet_ntoa(*addr));
#endif

  /* read first two bytes (response type, response) */
  if (readsocket(fd, buf, 2) != 2) {
    EI_TRACE_ERR0("ei_epmd_r4_port","<- CLOSE");
    closesocket(fd);
    return -2;			/* version mismatch */
  }

  s = buf;
  res = get8(s);
  
  if (res != EI_EPMD_PORT2_RESP) { /* response type */
    EI_TRACE_ERR1("ei_epmd_r4_port","<- unknown (%d)",res);
    EI_TRACE_ERR0("ei_epmd_r4_port","-> CLOSE");
    closesocket(fd);
    return -1;
  }

  if ((res = get8(s))) {
    /* got negative response */
    EI_TRACE_ERR1("ei_epmd_r4_port","<- PORT2_RESP result=%d (error)",res);
    closesocket(fd);
    return -1;
  }

  EI_TRACE_CONN1("ei_epmd_r4_port","<- PORT2_RESP result=%d (ok)",res);

  /* expecting remaining 8 bytes */
  if (readsocket(fd,buf,8) != 8) {
    EI_TRACE_ERR0("ei_epmd_r4_port","<- CLOSE");
    closesocket(fd);
    return -1;
  }
  
  closesocket(fd);
  s = buf;

  port = get16be(s);
  ntype = get8(s); 
  proto = get8(s);
  dist_high = get16be(s);
  dist_low = get16be(s);
  
  EI_TRACE_CONN5("ei_epmd_r4_port",
		"   port=%d ntype=%d proto=%d dist-high=%d dist-low=%d",
		port,ntype,proto,dist_high,dist_low);

  /* right network protocol? */
  if (EI_MYPROTO != proto)
    return -1;

  /* is there overlap in our distribution versions? */
  if ((EI_DIST_HIGH < dist_low) || (EI_DIST_LOW > dist_high)) 
    return -1;

  /* choose the highest common version */
  /* i.e. min(his-max, my-max) */
  *dist = (dist_high > EI_DIST_HIGH ? EI_DIST_HIGH : dist_high);
    
  /* ignore the remaining fields */
  return port;
}

/* lookup the port number of the given node. 'dist' is an out-argument
 * which, if the lookup is successful, will be initialized to contain
 * the highest distribution version that is common to the calling node
 * and the node looked up. The function will attempt to contact epmd
 * version 4 before trying version 3. R3 (and earlier) nodes have
 * dist=0.
 */
int ei_epmd_port (struct in_addr *addr, const char *alive, int *dist)
{
  int i;

  /* try the new one first, then the old one */
  i = ei_epmd_r4_port(addr,alive,dist);

  /* -2: new protocol not understood */
  if (i == -2) {
    *dist = 0; 
    i = ei_epmd_r3_port(addr,alive);
  }

  return i;
}

