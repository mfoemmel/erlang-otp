/*
 * Created: 27 Feb 1996 by tobbe@erix.ericsson.se
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/param.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/times.h>
#include <signal.h>
#include "erl_interface.h"
#include "erl_connect.h"
#include "erl_format.h"
#include "erl_error.h"

#ifdef DP_TARGET
#define MAIN edate
#define THISHOSTNAME "besk"
#define THISALIVENAME "dpkortet"
#define THISNODENAME "dpkortet@besk.uab.ericsson.se"
#define THISCOOKIE "abc"
#define THISIPADDRESS dp_get_ip()
#define OTHERALIVENAME "madonna"
#define CHIVASIPADDRESS "134.138.199.62"
#else
#define MAIN main
#define THISHOSTNAME "chivas"
#define THISALIVENAME "lolita"
#define THISNODENAME "lolita@chivas.uab.ericsson.se"
#define THISCOOKIE "abc"
#define THISIPADDRESS my_get_ip()
#define OTHERALIVENAME "madonna"
#define CHIVASIPADDRESS "134.138.199.62"

/* forward */
unsigned long inet_addr(); 

unsigned long my_get_ip(void)
{
  struct hostent *hp;
  unsigned long ul;

  if ((hp = gethostbyname(THISHOSTNAME)) == 0) {
    exit(-1);
  }
  memcpy(&ul, *hp->h_addr_list, sizeof(struct in_addr)); /* Hm... */
  return ul;
}

#endif

/* MAXHOSTNAMELEN is defined in <sys/param.h> */
#define MAXHOSTLEN 255
#define MAXALIVELEN 63

#ifdef VRTX
#define MAXLINE 4096

static void putline(const char*, ...); 
static void local_print_term(ETERM*);

#define PRINT(t) \
{ \
  local_print_term(t); \
  putline("\n\r"); \
}
#else
#define PRINT(t) \
{ \
  print_term(stderr,t); \
  fprintf(stderr,"\n"); \
}
#endif


int MAIN() 
{
  int fd;
  ETERM *reply;
  unsigned long thisipaddress,otheripaddress;
  struct in_addr ip_addr;

  thisipaddress = THISIPADDRESS;
  if (!erl_connect_xinit(THISHOSTNAME,
			 THISALIVENAME,
			 THISNODENAME,
			 (IpAddr) &thisipaddress,
			 THISCOOKIE))
    erl_err_quit("<ERROR> in erl_lib_init !");

  if ((otheripaddress = inet_addr(CHIVASIPADDRESS)) != -1) {
    memcpy(&ip_addr.s_addr,(char *) &otheripaddress,sizeof(otheripaddress));
    /* 
     * Try to connect.
     */
    if ((fd = erl_xconnect(&ip_addr, OTHERALIVENAME)) < 0)
      erl_err_quit("erl_connect failed");
    
    if ((reply = erl_rpc(fd, "erlang", "date", erl_format("[]"))) == (ETERM *) ERL_ERROR)
      erl_err_msg("<ERROR> from rpc !\n");
    
    PRINT(reply);
    close(fd);
  }

  exit(0);

}

#ifdef VRTX

static void local_print_term(ETERM *tptr)
{
  int j,i, doquote;
  byte type;

  j = i = doquote = 0;
  type = tptr->type & ~HIGHBIT;
  switch(type) {
  case ATOM:
    if (! islower(tptr->uval.aval.val[0])) {
      doquote = 1;
      sc_putc('\'');
    }
    while (i < tptr->uval.aval.len) 
      sc_putc(tptr->uval.aval.val[i++]);
    if (doquote) sc_putc('\'');
    break;
  case VARIABLE:
    if (! isupper(tptr->uval.vval.name[0])) {
      doquote = 1;
      sc_putc('\'');
    }
    while (i < tptr->uval.vval.len) 
      sc_putc(tptr->uval.vval.name[i++]);
    if (doquote) sc_putc('\'');
    break;
  case PID:
    putline("<%s,%d,%d>",tptr->uval.pval->node,tptr->uval.pval->number, 
	    tptr->uval.pval->serial);
    break;
  case PORT:
    putline("#Port");
    break;
  case REF:
    putline("#Ref");
    break;
  case LIST:
    sc_putc('[');
    i = tptr->uval.lval.size;
    j = 0;
    while(j != i) {
      local_print_term(tptr->uval.lval.elems[j++] );
      if (j != i) sc_putc(',');
    }
    if ((erl_get_type(tptr->uval.lval.elems[j])) == NIL) {
      sc_putc(']'); 
      break;
    }
    /* Not well formed list */
    sc_putc('|');
    local_print_term(tptr->uval.tval.elems[j]);
    sc_putc(']'); 
    break;
  case TUPLE:
    sc_putc('{');
    i = tptr->uval.tval.size;
    j = 0;
    while(j != i) {
      local_print_term(tptr->uval.tval.elems[j++] );
      if (j != i) sc_putc(',');
    }
    sc_putc('}');
    break;
  case BINARY:
    putline("#Bin");
    break;
  case INTEGER:
  case SMALL_BIG:
    putline("%d",tptr->uval.ival);
    break;
  case U_SMALL_BIG:
    putline("%d",tptr->uval.uival);
    break;
  case NIL:
    putline("[]");
    break;
  case STRING:
    putline("\"%s\"",tptr->uval.stringval);
    break;
  case FLOAT:
    putline("%f", tptr->uval.fval);
    break;
  default:
    erl_err_msg("Bad type of term in local_print_term \n\r");
  }
} /* local_print_term */


static void putbuffer(char *buffer)
{
  int i;
  
  for(i=0; buffer[i]!=NULL; i++)
    sc_putc(buffer[i]);
  return;
} /* putbuffer */

static void doit(const char *fmt, va_list ap)
{
  char buf[MAXLINE];

  vsprintf(buf, fmt, ap);
  strcat(buf,"\000");
  putbuffer(buf); 
  return;
} /* doit */

static void putline(const char *fmt, ... )
{
  va_list ap;

  va_start(ap, fmt);
  doit( fmt, ap);
  va_end(ap);
  return;
} /* putline */

#endif
