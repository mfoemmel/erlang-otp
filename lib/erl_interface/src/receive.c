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

/* common */
#include <string.h>
#include <stdlib.h>

#include "ei.h"
#include "putget.h"

#ifdef DEBUG_DIST
#include <stdio.h>
#include <errno.h>
extern int ei_show_recmsg(FILE *dest,erlang_msg *msg,char *buf);
extern int ei_trace_distribution;
#endif

#ifdef __WIN32__
#define readsocket(fd,buf,n) recv(fd,buf,n,0)
#define writesocket(fd,buf,n) send(fd,buf,n,0)
#else 
#define readsocket(fd,buf,n)  read(fd,buf,n)
#define writesocket(fd,buf,n)  write(fd,buf,n)
#endif

extern erlang_trace *ei_trace(int query, erlang_trace *token);

static int read_fill(int fd, char *buf, int len)
{
  int got = 0;
  int i;
  
  while (got < len) {
    i = readsocket(fd,buf+got,len-got);
    if (i<=0) return i;
    got+=i;
  }
  return got;
}
		     
#ifndef SMALLBUF 
#define SMALLBUF 2048
#endif 


/* length (4), PASS_THOUGH (1), header, message */
int ei_recv_internal(int fd, char **mbufp, int *bufsz, erlang_msg *msg, int *msglenp, int staticbufp)
{
  char header[SMALLBUF]; /* largest possible header is approx 1300 bytes */
  char *s=header;
  char *mbuf=*mbufp;
  int len = 0;
  int msglen = 0;
  int bytesread = 0;
  int remain;
  int arity;
  int version;
  int index = 0;
  int i = 0;
#ifdef DEBUG_DIST
  int show_this_msg = 0;
#endif

  /* get length field */
  if (read_fill(fd, header, 4) != 4) return -1;
  len = get32be(s);

  /* got tick - respond and return */
  if (!len) {
    unsigned char tock[] = {0,0,0,0};
    writesocket(fd, tock, sizeof(tock));
    *msglenp = 0;
    return 0;
  }
  
  /* turn off tracing on each receive. it will be turned back on if
   * we receive a trace token.
   */
  ei_trace(-1,NULL);
  
  /* read enough to get at least entire header */
  bytesread = (len > SMALLBUF ? SMALLBUF : len); 
  if ((i = read_fill(fd,header,bytesread)) != bytesread) return -1;

  /* now decode header */
  /* pass-through, version, control tuple header, control message type */
  s = header;
  index = 1;
  if ((get8(s) != ERL_PASS_THROUGH)
      || ei_decode_version(header,&index,&version)
      || (version != ERL_VERSION_MAGIC) 
      || ei_decode_tuple_header(header,&index,&arity) 
      || ei_decode_long(header,&index,&msg->msgtype)) return -1;

  switch (msg->msgtype) {
  case ERL_SEND:          /* { SEND, Cookie, ToPid } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 0) show_this_msg = 1;
#endif
    if (ei_decode_atom(header,&index,msg->cookie) 
	|| ei_decode_pid(header,&index,&msg->to)) return -1;
    break;

  case ERL_REG_SEND:     /* { REG_SEND, From, Cookie, ToName } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 0) show_this_msg = 1;
#endif
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_atom(header,&index,msg->cookie) 
	|| ei_decode_atom(header,&index,msg->toname)) return -1;
    /* actual message is remaining part of headerbuf, plus any unread bytes */
    break;

  case ERL_LINK:         /* { LINK, From, To } */
  case ERL_UNLINK:       /* { UNLINK, From, To } */
  case ERL_GROUP_LEADER: /* { GROUP_LEADER, From, To } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 1) show_this_msg = 1;
#endif
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to)) return -1;
    break;
    
  case ERL_EXIT:         /* { EXIT, From, To, Reason } */
  case ERL_EXIT2:        /* { EXIT2, From, To, Reason } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 1) show_this_msg = 1;
#endif
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to)) return -1;
    break;
    
  case ERL_SEND_TT:      /* { SEND_TT, Cookie, ToPid, TraceToken } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 0) show_this_msg = 1;
#endif
    if (ei_decode_atom(header,&index,msg->cookie) 
	|| ei_decode_pid(header,&index,&msg->to)
	|| ei_decode_trace(header,&index,&msg->token)) return -1;
    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  case ERL_REG_SEND_TT:  /* { REG_SEND_TT, From, Cookie, ToName, TraceToken } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 0) show_this_msg = 1;
#endif
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_atom(header,&index,msg->cookie) 
	|| ei_decode_atom(header,&index,msg->toname)
	|| ei_decode_trace(header,&index,&msg->token)) return -1;
    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  case ERL_EXIT_TT:     /* { EXIT_TT, From, To, TraceToken, Reason } */
  case ERL_EXIT2_TT:    /* { EXIT2_TT, From, To, TraceToken, Reason } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 1) show_this_msg = 1;
#endif
    if (ei_decode_pid(header,&index,&msg->from) 
	|| ei_decode_pid(header,&index,&msg->to)
	|| ei_decode_trace(header,&index,&msg->token)) return -1;
    ei_trace(1,&msg->token); /* turn on tracing */
    break;

  case ERL_NODE_LINK:   /* { NODE_LINK } */
#ifdef DEBUG_DIST
    if (ei_trace_distribution > 1) show_this_msg = 1;
#endif
    break;

  default:
    /* unknown type, just put any remaining bytes into buffer */
    break;
  }

  /* actual message is remaining part of headerbuf, plus any unread bytes */
  msglen = len - index;     /* message size (payload) */
  remain = len - bytesread; /* bytes left to read */

  /* if callers buffer is too small, we flush in the rest of the
   * message and discard it, unless we know that we can reallocate
   * the buffer in which case we do that and read the message.
   */
  if (msglen > *bufsz) {
    if (staticbufp) {
      int sz = SMALLBUF;
      /* flush in rest of packet */
      while (remain > 0) {
	if (remain < sz) sz = remain;
	if ((i=read_fill(fd,header,sz)) <= 0) break;
	remain -= i;
      }
      return -1;
    }
    else {
#ifdef DEBUG
      fprintf(stderr,"callers buffer too small (%d < %d) - allocating new\n",*bufsz,msglen);
#endif
      /* dynamic buffer */
      if (!(mbuf = malloc(msglen))) {
	return -1; /* couldn't allocate */
      }

      free(*mbufp);
      *mbufp = mbuf;
      *bufsz = msglen;
    }  
  }
  
  /* move remaining bytes to callers buffer */
  memmove(mbuf,header+index,bytesread-index);

  /* let the caller know how big the message is in his buffer */
  *msglenp = msglen;

  /* read the rest of the message into callers buffer */
  if (remain > 0) {
    if ((i = read_fill(fd,mbuf+bytesread-index,remain)) != remain) {
      *msglenp = bytesread-index+1; /* actual bytes in users buffer */
      return -1;
    }
  }

#ifdef DEBUG_DIST
  if (show_this_msg) ei_show_recmsg(stderr,msg,mbuf);
#endif

  /* the caller only sees "untraced" message types */
  /* the trace token is buried in the message struct */
  if (msg->msgtype > 10) msg->msgtype -= 10;
  
  return msg->msgtype;
}

int ei_receive_encoded(int fd, char **mbufp, int *bufsz, erlang_msg *msg, int *msglen)
{
  return ei_recv_internal(fd, mbufp, bufsz, msg, msglen, 0);
}
