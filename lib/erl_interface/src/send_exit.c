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

#elif VXWORKS
#include <unistd.h>

#else /* unix */
#include <unistd.h>

#endif

#ifdef DEBUG_DIST
#include <stdio.h>
extern int ei_show_sendmsg(FILE *dest,char *header,char *msg);
extern int ei_trace_distribution;
#endif

#include <string.h>
#include <stdlib.h>
#include "ei.h"
#include "erl_connect.h"
#include "putget.h"

#ifdef __WIN32__
#define writesocket(fd,buf,n) send(fd,buf,n,0)
#else 
#define writesocket(fd,buf,n)  write(fd,buf,n)
#endif

extern erlang_trace *ei_trace(int query, erlang_trace *token);

#ifndef SMALLBUF
#define SMALLBUF 2048
#endif

/* use this to break a link */
extern int ei_send_exit(int fd, const erlang_pid *from, const erlang_pid *to, const char *reason)
{
  char sbuf[SMALLBUF];
  erlang_trace *token = NULL;
  char *dbuf = NULL;
  char *msgbuf;
  char *s;
  int index = 0;
  int len = strlen(reason) + 1080; /* see below */

  if (len > SMALLBUF)
    if (!(dbuf = malloc(index)))
      return -1;
  msgbuf = (dbuf ? dbuf : sbuf);


  /* are we tracing? */
  /* check that he can receive trace tokens first */
  if (erl_distversion(fd) > 0) token = ei_trace(0,NULL);

  index = 5;                                     /* max sizes: */
  ei_encode_version(msgbuf,&index);                     /*   1 */
  if (token) {
    ei_encode_tuple_header(msgbuf,&index,5);            /*   2 */
    ei_encode_long(msgbuf,&index,ERL_EXIT_TT);          /*   2 */
  }
  else {
    ei_encode_tuple_header(msgbuf,&index,4);
    ei_encode_long(msgbuf,&index,ERL_EXIT);
  }
  ei_encode_pid(msgbuf,&index,from);                    /* 268 */
  ei_encode_pid(msgbuf,&index,to);                      /* 268 */

  if (token) ei_encode_trace(msgbuf,&index,token);      /* 534 */

  /* Reason */
  ei_encode_string(msgbuf,&index,reason);               /* len */

  /* 5 byte header missing */
  s = msgbuf;
  put32be(s, index - 4);                                /*   4 */
  put8(s, ERL_PASS_THROUGH);                                /*   1 */
                                          /*** sum: len + 1080 */

#ifdef DEBUG_DIST
  if (ei_trace_distribution > 1) ei_show_sendmsg(stderr,msgbuf,NULL);
#endif

  writesocket(fd,msgbuf,index); 

  if (dbuf) free(dbuf);
  return 0;
}

