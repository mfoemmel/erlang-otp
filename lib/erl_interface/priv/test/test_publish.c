/*
 * Created: 20 May 1996 by tobbe@erix.ericsson.se
 * 
 * Function: ./test_publish
 *   Then send a message: {hej,c99@<HostName>} ! 'An atom'.
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/param.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <sys/times.h>
#include <signal.h>
#include "proto.h"
#include "erl_interface.h"

/* forward */
unsigned long inet_addr(); 

#define MY_PORT 7172

#define PRINT(t) \
{ \
  erl_print_term(stderr,t); \
  fprintf(stderr,"\n"); \
}

#define MEM_DUMP() memory_dump()

static int mk_listen(short);
static char *get_cookie(void);
void do_memory_dump(const char*);
void memory_dump(int);

int main() 
{
  int fd,lfd,rc;
  fd_set readmask,orig_readmask;
  ErlConnect con;
  char *cookie;
  char buffer[BUFSIZ];
  ErlMessage emsg;
  int creation;
  struct timeval tv;
  ETERM *reply;
  ETERM *rpc_args;

  cookie = get_cookie();
  creation = (time(NULL) % 3) + 1; /* "random" in range 1-3 */
  erl_init(NULL,0);
  erl_connect_init(99, cookie, creation);
  rpc_args = erl_format("[]");

  if ((lfd = mk_listen((short) MY_PORT)) < 0)
    erl_err_sys("<ERROR> from mk_listen");

  if (!(creation = erl_publish(MY_PORT)))
    erl_err_quit("<ERROR> from erl_publish");

  erl_set_thiscreation(creation);
  fd = 0;
  FD_ZERO( &readmask );
  FD_ZERO( &orig_readmask );
  FD_SET( lfd , &orig_readmask );

  while (1) {
    tv.tv_sec = 1; tv.tv_usec = 0; /* don't assume BSD bug in select() */
    readmask = orig_readmask;
    if ((rc = select(FD_SETSIZE,&readmask, (fd_set *) NULL,
		     (fd_set *) NULL, 
		     (struct timeval *) &tv )) < 0 ) 
      erl_err_sys("From select (in erl_rpc_from) !");

    /* A connection attempt !! */

    if (FD_ISSET(lfd, &readmask)) {
      if (!fd) {
	struct in_addr in;

	MEM_DUMP();
	if ((fd = erl_accept(lfd , &con)) < 0)
	  erl_err_quit("<ERROR> from erl_accept");
	FD_SET( fd , &orig_readmask );
	memcpy(&in, con.ipadr, 4);
	fprintf(stderr,"Connected to: %s , at: %s\n",
		con.nodename, inet_ntoa(in));
	MEM_DUMP();
      }
      else {
	close(erl_accept(lfd, NULL));
	fprintf(stderr,"<WARNING> accept rejected\n");
      }
    }
    
    /* An incoming message !! */

    if (fd && FD_ISSET(fd, &readmask)) {
      MEM_DUMP();
      if ((rc = erl_receive_msg( fd , (unsigned char *) buffer , &emsg)) == ERL_TICK) {
	fprintf(stderr,"Got a tick !\n");
	MEM_DUMP();
	goto got_a_tick;
      }
      else if (rc == ERL_ERROR)
	goto got_an_error;
      else if (rc == ERL_MSG) {
	MEM_DUMP();
	fprintf(stderr,"Got msg: ");
	erl_print_term(stderr, emsg.msg);
	if (emsg.type == ERL_REG_SEND) 
	  fprintf(stderr," from: ");
	else
	  fprintf(stderr," to: ");
	PRINT(emsg.pid);
	fprintf(stderr,"\n");
	erl_free_term(emsg.msg);
	erl_free_term(emsg.pid);
	MEM_DUMP();
      }
      else
	fprintf(stderr,"<MEGA_ERROR> from erl_receive_msg\n");
    }

    /* Timeout !! */

    if (rc == 0 && fd) {
      if ((reply = erl_rpc(fd, "erlang", "time", rpc_args)) == (ETERM *) NULL)
	erl_err_quit("<ERROR> from rpc !\n");
      fprintf(stderr,"Erlang time: ");
      PRINT(reply);
      erl_free_term(reply);
    }

  got_a_tick:
    continue;
  } /* while */

got_an_error:
  close(fd);
  close(lfd);
  exit(-1);
}

static int mk_listen(short port) /* Port number */
{
  int listen_fd,one=1;
  struct sockaddr_in addr;

  if ( (listen_fd = socket( AF_INET , SOCK_STREAM , 0 )) < 0) 
    return -1;

  memset((void *) &addr , 0 , (size_t) sizeof(addr));
  addr.sin_family = AF_INET;              
  addr.sin_port   = htons( port );   
  addr.sin_addr.s_addr = htonl( INADDR_ANY ); 

  if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, (char *) &one, sizeof(one)) < 0)
    return -1;

  if (bind(listen_fd, (struct sockaddr *) &addr, sizeof( addr )) < 0)
    return -1;
  
  listen(listen_fd, 5);
  return listen_fd;

}


#define COOKIE_FILE "/.erlang.cookie"

static char *get_cookie(void)
{
  char fname[256],*home,*cookie;
  int fd,len;
  char buf[BUFSIZ];

  home = getenv("HOME");
  strcpy(fname, home);
  strcat(fname, COOKIE_FILE);
  if ((fd = open(fname, O_RDONLY)) < 0)
    erl_err_sys("<ERROR> open cookie file");

  if ((len = read(fd, buf, BUFSIZ)) < 0)
    erl_err_sys("<ERROR> reading cookie file (1)");
  else if (len == BUFSIZ)
    erl_err_sys("<ERROR> reading cookie file (2)");
  
  cookie = (char *) malloc(len+1);
  memcpy(cookie, buf, len);
  cookie[len] = '\0';
  /*
   * Remove trailing newline
   */
  if (cookie[len-1] == '\n')
    cookie[len-1] = '\0';
  return cookie;

} /* get_cookie */



/* 
 * Print out the content of the allocated blocks
 */
void memory_dump(void)
{
  int ab,fb;

  fprintf(stderr, "Content in allocated blocks: <<"); 
  erl_fix_statistics(&ab, &fb, do_memory_dump); 
  fprintf(stderr, ">>\n"); 
  fprintf(stderr, "Allocated blocks: %d , Free blocks: %d\n", ab, fb);
}

/* 
 * The actual print routine called from within erl_fix_statistics.
 */
void do_memory_dump(const char *mp)
{
  fprintf(stderr, "(");
  erl_print_term(stderr, (ETERM *) mp);
  fprintf(stderr, " , %d) ", ERL_COUNT(mp));
}

