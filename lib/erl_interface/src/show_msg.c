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
#include <stdio.h>
#include <stdlib.h>

#include "ei.h"
#include "putget.h"

/* the new TT stuff has been added, but when these messages are shown
 * they will look just like the non-tt ones for now.
 */
   
   
/* this should be long enough for longest atoms (256) but short enough for
 * fprintf to handle all at once (a few kb probably). */
#ifndef SMALLBUF
#define SMALLBUF 512
#endif


/*
 * this help function does the actual decoding of the
 * terms and is used by both ei_efprint and ei_sprintt.
 *
 * termbuf contains the undecoded term.
 * idx is the current position in termbuf.
 * pr is print function, one of fprintf or sprintf.
 * dest is print destination, e.g. a FILE* or char*, depending
 * on value of pr. Other combinations may be possible.
 */
static int show_pid(int (*pr)(), void *dest, const erlang_pid *pid)
{
  return pr(dest,"#Pid<%s.%u.%u.%u>",
	    pid->node,pid->num,pid->serial,pid->creation);
}

static int show_trace(int (*pr)(), void *dest, const erlang_trace *t)
{
  int n;

  n = pr(dest,"Trace: Label: %ld, Flags: 0x%lx serial: %ld, prev: %ld From: ",
	 t->label,t->flags,t->serial,t->prev);
  n += show_pid(pr,dest,&t->from);

  return n;
}


/* this function doesn't do anything but skip over the number in the buffer */
/* it doesn't really belong here either... */
static int ei_decode_bignum(const char *buf, int *index, void *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  long n;
  
  switch (get8(s)) {
  case ERL_LARGE_BIG_EXT:
    n = get32be(s);
    s += n+1;
    break;
    
  default:
    return -1;
  }

  *index += s-s0;
  
  return 0;
}

/* we only need to initialize some of these (after 32 everything printable) */
static non_printable[256] = {
  /*                  1                   2                   3   */
  /*  2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 */
  1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  /*             \b\t\n\v\f\r                                     */
};

static int printable_list_p(const unsigned char *buf, int buflen)
{
  int i;
  
  for (i=0; i<buflen; i++) if (non_printable[buf[i]]) return 0;

  /* is printable */
  return 1;
}

static int show_term(const char *termbuf, int *index, int (*pr)(), void *dest)
{
  int type;
  char smallbuf[SMALLBUF];
  int version;
  long num;
  double fnum;
  erlang_pid pid;
  erlang_port port;
  erlang_ref ref;
  int i, len;
  int n = 0;
  char *s;

  ei_get_type(termbuf,index,&type,&len);
  
  switch (type) {
  case ERL_VERSION_MAGIC:
    /* just skip past this */
    ei_decode_version(termbuf,index,&version);
    n += show_term(termbuf,index,pr,dest);
    break;
      
  case ERL_ATOM_EXT:
    ei_decode_atom(termbuf,index,smallbuf);
    n += pr(dest,"%s",smallbuf);
    break;

  case ERL_STRING_EXT:
    /* strings can be much longer than SMALLBUF */
    if (len < SMALLBUF) s = smallbuf;
    else if (!(s = malloc(len+1))) break;

    ei_decode_string(termbuf,index,s);

    if (printable_list_p(s,len)) {
      /* just show it as it is */
      n += pr(dest,"\"%s\"",s);
    }
    else {
      /* show it as a list instead */
      n += pr(dest,"[");
      for (i=0; i<len; i++) {
	if (i > 0) n += pr(dest,", ");
	n += pr(dest,"%d",s[i]);
      }
      n += pr(dest,"]");
    }

    /* did we allocate anything? */
    if (s && (s != smallbuf)) free(s);

    break;
    
  case ERL_SMALL_BIG_EXT:
  case ERL_SMALL_INTEGER_EXT:
  case ERL_INTEGER_EXT:
    ei_decode_long(termbuf,index,&num);
    n += pr(dest,"%ld",num);
    break;

  case ERL_FLOAT_EXT:
    ei_decode_double(termbuf,index,&fnum);
    n += pr(dest,"%f",fnum);
    break;

  case ERL_PID_EXT:
    ei_decode_pid(termbuf,index,&pid);
    n += show_pid(pr,dest,&pid);
    break;
    
  case ERL_SMALL_TUPLE_EXT:
  case ERL_LARGE_TUPLE_EXT:
    ei_decode_tuple_header(termbuf,index,&len);
    n += pr(dest,"{");
    for (i=0; i<len; i++) {
      if (i > 0) n += pr(dest,", ");
      n += show_term(termbuf,index,pr,dest);
    }
    n += pr(dest,"}");
    break;
    
  case ERL_LIST_EXT:
    ei_decode_list_header(termbuf,index,&len);
    n += pr(dest,"[");
    for (i=0; i<len; i++) {
      if (i > 0) n += pr(dest,", ");
      n += show_term(termbuf,index,pr,dest);
    }
    /* get the empty list at the end */
    ei_decode_list_header(termbuf,index,&len);
    n += pr(dest,"]");
    break;

  case ERL_NIL_EXT:
    ei_decode_list_header(termbuf,index,&len);
    n += pr(dest,"[]");
    break;
    
  case ERL_REFERENCE_EXT:
  case ERL_NEW_REFERENCE_EXT:
    ei_decode_ref(termbuf,index,&ref);
    n += pr(dest,"#Ref<%s",ref.node);
    for (i = 0; i < ref.len; i++) {
	n += pr(dest,".%u",ref.n[i]);
    }
    n += pr(dest,".%u>",ref.creation);
    break;

  case ERL_PORT_EXT:
    ei_decode_port(termbuf,index,&port);
    n += pr(dest,"#Port<%s.%u.%u>",port.node,port.id,port.creation);
    break;
      
  case ERL_BINARY_EXT:
    ei_decode_binary(termbuf,index,NULL,&num);
    n += pr(dest,"#Bin<%d>",num);
    break;
    
  case ERL_LARGE_BIG_EXT:
    /* doesn't actually decode - just skip over it */
    ei_decode_bignum(termbuf,index,NULL);
    n += pr(dest,"#Bignum");
    break;
    
  default:
    n += pr(dest,"#Unknown<%d.%d>",type,len);
    /* unfortunately we don't know how to skip over this type in
     * the buffer if we don't even know what it is, so we return.
     */
    return n;
    break;
  }

  return n;
}

/* print term to stream with fprintf */
static int ei_efprint(FILE *stream, const char *termbuf)
{
  int index = 0;
  return show_term(termbuf,&index,(int (*)())fprintf,stream);
}

#if (0)
/* print term to buffer with sprintf */
static int ei_esprint(char *buf, const char *termbuf)
{
  int index = 0;
  return show_term(termbuf,&index,(int (*)())sprintf,buf);
}

/* print term to buffer with sprintf */
static int ei_esnprint(char *buf, int n, const char *termbuf)
{
  int index = 0;
  return show_term(termbuf,&index,(int (*)())snprintf,buf,n);
}
#endif /* 0 */

static int show_msg(FILE *dest, int direction, const erlang_msg *msg, const char *buf)
{
  if (direction) fprintf(dest,"-> ");
  else fprintf(dest,"<- ");
  
  switch (msg->msgtype) {
  case ERL_LINK:
    fprintf(dest,"LINK From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    break;
      
  case ERL_SEND:
    fprintf(dest,"SEND To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    fprintf(dest,"\n   ");
    /* show the message */
    ei_efprint(dest,buf);
    break;
      
  case ERL_EXIT:
    fprintf(dest,"EXIT From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    /* show the reason */
    fprintf(dest,"\n   Reason: ");
    ei_efprint(dest,buf);
    break;
      
  case ERL_UNLINK:
    fprintf(dest,"UNLINK From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    break;
      
  case ERL_NODE_LINK:
    fprintf(dest,"NODE_LINK");
    break;
      
  case ERL_REG_SEND:
    fprintf(dest,"REG_SEND From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: %s\n   ",msg->toname);
    /* show the message */
    ei_efprint(dest,buf);
    break;
      
  case ERL_GROUP_LEADER:
    fprintf(dest,"GROUP_LEADER From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    break;
      
  case ERL_EXIT2:
    fprintf(dest,"EXIT2 From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    /* show the reason */
    fprintf(dest,"\n   Reason: ");
    ei_efprint(dest,buf);
    break;

    /* the new TT stuff below */

  case ERL_EXIT_TT:
    fprintf(dest,"EXIT_TT From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    fprintf(dest,"\n   ");
    show_trace((int (*)())fprintf,dest,&msg->token);
    /* show the reason */
    fprintf(dest,"\n   Reason: ");
    ei_efprint(dest,buf);
    break;
    
  case ERL_EXIT2_TT:
    fprintf(dest,"EXIT2_TT From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    fprintf(dest,"\n   ");
    show_trace((int (*)())fprintf,dest,&msg->token);
    /* show the reason */
    fprintf(dest,"\n   Reason: ");
    ei_efprint(dest,buf);
    break;
    
  case ERL_SEND_TT:
    fprintf(dest,"SEND_TT To: ");
    show_pid((int (*)())fprintf,dest,&msg->to);
    fprintf(dest,"\n   ");
    show_trace((int (*)())fprintf,dest,&msg->token);
    fprintf(dest,"\n   ");
    /* show the message */
    ei_efprint(dest,buf);
    break;

  case ERL_REG_SEND_TT:
    fprintf(dest,"REG_SEND_TT From: ");
    show_pid((int (*)())fprintf,dest,&msg->from);
    fprintf(dest," To: %s\n   ",msg->toname);
    show_trace((int (*)())fprintf,dest,&msg->token);
    fprintf(dest,"\n   ");
    /* show the message */
    ei_efprint(dest,buf);
    break;

  default:
    fprintf(dest,"Unknown message type: %ld",msg->msgtype);
  }
  fprintf(dest,"\n");

  return 0;
}

extern int ei_show_recmsg(FILE *dest, erlang_msg *msg, char *buf)
{
  return show_msg(dest, 0, msg, buf);
}

/* decode the buffer again before showing it */
extern int ei_show_sendmsg(FILE *dest, const char *header, const char *msgbuf)
{
  erlang_msg msg;
  const char *mbuf = NULL;
  int index = 0;
  int arity = 0;
  int version = 0;

  /* skip five bytes */
  index = 5;
  ei_decode_version(header,&index,&version);
  ei_decode_tuple_header(header,&index,&arity);
  ei_decode_long(header,&index,&msg.msgtype);

  switch (msg.msgtype) {
  case ERL_SEND:
    if (ei_decode_atom(header,&index,msg.cookie) 
	|| ei_decode_pid(header,&index,&msg.to)) return -1;
    mbuf = msgbuf;
    break;

  case ERL_SEND_TT:
    if (ei_decode_atom(header,&index,msg.cookie) 
	|| ei_decode_pid(header,&index,&msg.to)
	|| ei_decode_trace(header,&index,&msg.token)) return -1;
    mbuf = msgbuf;
    break;
    
  case ERL_REG_SEND:
    if (ei_decode_pid(header,&index,&msg.from) 
	|| ei_decode_atom(header,&index,msg.cookie) 
	|| ei_decode_atom(header,&index,msg.toname)) return -1;
    mbuf = msgbuf;
    break;
    
  case ERL_REG_SEND_TT:
    if (ei_decode_pid(header,&index,&msg.from) 
	|| ei_decode_atom(header,&index,msg.cookie) 
	|| ei_decode_atom(header,&index,msg.toname)
	|| ei_decode_trace(header,&index,&msg.token)) return -1;
    mbuf = msgbuf;
    break;

  case ERL_EXIT:
  case ERL_EXIT2:
    if (ei_decode_pid(header,&index,&msg.from) 
	|| ei_decode_pid(header,&index,&msg.to)) return -1;
    mbuf = header+index;

  case ERL_EXIT_TT:
  case ERL_EXIT2_TT:
    if (ei_decode_pid(header,&index,&msg.from) 
	|| ei_decode_pid(header,&index,&msg.to)
	|| ei_decode_trace(header,&index,&msg.token)) return -1;
    mbuf = header+index;
    break;
    
  case ERL_LINK:
  case ERL_UNLINK:
  case ERL_GROUP_LEADER:
    if (ei_decode_pid(header,&index,&msg.from) 
	|| ei_decode_pid(header,&index,&msg.to)) return -1;
    mbuf = header;
    break;
    
  case ERL_NODE_LINK:
    /* nothing to do */
    mbuf = header;
    break;

  default:
    break;
  }

  return show_msg(dest, 1, &msg, mbuf);
}


			   


