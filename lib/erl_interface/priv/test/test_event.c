#include <stdio.h>
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
#include "erl_interface.h"

#ifdef SUNOS4
extern int fprintf();
extern int sscanf();
extern void bzero();
extern int gettimeofday();
extern int gethostname();
extern int select();
extern fflush();
#endif


typedef struct {   /* Linked Pids */
  ETERM *other;
  ETERM *self;
} the_pids;

static byte buffer[25*BUFSIZ];
static int creation=1;

#define SELF(fd) erl_mk_pid(erl_thisnodename(), fd, 0, creation)

/*
 * Get host entry (by address or name)
 */
struct hostent* get_hostent(host)
     char* host;
{
  struct hostent* hp;
  
  if (isdigit(*host)) {
    struct in_addr ip_addr;
    int b1, b2, b3, b4;
    long addr;
    
    if (sscanf(host, "%d.%d.%d.%d", 
	       &b1, &b2, &b3, &b4) != 4)
      return 0;
    addr = (b1 << 24) | (b2 << 16) | (b3 << 8) | b4;
    ip_addr.s_addr = htonl(addr);
    
    if ((hp = gethostbyaddr((char*) &ip_addr,
			    sizeof(struct in_addr), AF_INET)) == 0)
      return 0;
  }
  else if ((hp = gethostbyname(host)) == 0)
    return 0;
  return hp;
} /* get_hostent */


static char *get_cookie(void)
{
  char fname[256],*home,*cookie;
  int fd,len;

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

  if (verbosep) {
    fprintf(stderr,"Got cookie=<");
    write(2,cookie,len);
    fprintf(stderr,">\n");
  }

  return cookie;

} /* get_cookie */

static void hello_world(int sock, int type, ETERM* pat, ERL_USER_DATA eud)
{
  ETERM *v[2];
  static ETERM *msg=NULL;

  erl_err_msg("Enter hello_world !");

  if (msg == NULL) 
    /* Create it only the first time */
    msg  = erl_format("{~w,~a}", SELF(sock), erl_mk_atom("Hello World"));


  erl_reg_send(sock, "lolita", msg);

} /* hello_world */

static void display(int sock, int type, ETERM *pat, ERL_USER_DATA eud)
{
  static ETERM *args=NULL;

  erl_err_msg("Enter display !");

  if (args == NULL) 
    /* Create it only the first time */
    args = erl_cons(erl_mk_string("hello"), NULL);

  erl_reg_send(sock, "rex", msg);

} /* display */

extern int get_tick(void);

#define MY_TIMEOUT 20


static void my_callback(int fd, int type, ETERM *pat, ERL_USER_DATA eud)
{
  ErlMessage emsg;
  int rc;

  erl_err_msg("Enter my_callback !");

  if ((rc = erl_receive_msg(fd, buffer, &emsg)) != ERL_ERROR) {
    if (msg != ERL_TICK) {
      ETERM *goodbye;
      
      fprintf(stderr, "Got a message: ");
      erl_print_term(stderr, emsg.msg);
      fprintf(stderr, "\n");
      
      /* Now also test erl_send/3 */
      goodbye = erl_mk_atom("goodbye");
      erl_send(fd, emsg.pid, goodbye);
      
      erl_free_term(emsg.msg);
      erl_free_term(emsg.pid);
      erl_free_term(goodbye);
    }
    else
      fprintf(stderr, "Got a TICK !\n");
  }
  else
    erl_err_quit("my_callback: Got error from erl_receive_msg");

} /* my_callback */


/*--- M A I N ---*/

int main(int argc, char *argv[])
{
    int sock;
    struct hostent* hp;
    char host_name[MAXHOSTNAMELEN];
    char nodename[MAXHOSTLEN+1+MAXALIVELEN+1];
    char *host, *node, *cookie, *p;

    progname = argv[0];
    if (argc < 2)
      erl_err_quit("<ERROR> %s: Not enough arguments !", progname);

    
    creation = (time(NULL) % 3) + 1; /* "random" in range 1-3 */
    cookie   = get_cookie();

    erl_init((Heap *) NULL, 0);
    if (!erl_connect_init(17, cookie, creation))
      erl_err_quit("<ERROR> from erl_connect_init !");

    node = argv[1];
    if ((p = strchr((const char *) argv[1], (int) '@')) == 0) {
      if (gethostname(host_name, MAXHOSTNAMELEN) == -1)
	erl_err_quit("gethostname");
      host = host_name;
    }
    else {
      *p = 0;       /* path 0 */
      host = p+1;
    }

    /* Expand name to a real name (may be ip-address) */
    if ((hp = get_hostent(host)) == 0)
	erl_err_quit("get_host_ent");
    strcpy(host_name, hp->h_name);

    sprintf(nodename, "%s@%s", node, host_name);

    /* Try to connect to it */

    if ((sock = erl_connect(nodename)) < 0)
      erl_err_quit("<ERROR> can't connect");	

    erl_err_msg("We are now connected to %s (%d)\n", nodename, sock);

    erl_add_callback(sock, my_callback, ERL_READ, (ETERM *) NULL , (ERL_USER_DATA) NULL);
    erl_add_callback(sock, hello_world, ERL_CONN_TIMEOUT , (ETERM *) NULL, (ERL_USER_DATA) NULL);
    /*    erl_add_callback(sock, display, ERL_CONN_TIMEOUT , (ETERM *) NULL, (ERL_USER_DATA) NULL); */

    erl_main_loop(MY_TIMEOUT);

    exit(0);
}

