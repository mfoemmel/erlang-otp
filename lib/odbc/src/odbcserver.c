#ifdef __WIN32__
#include <windows.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>

#ifdef __WIN32__
#include <winsock.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <netdb.h>
#endif

#include "odbclli__s.h"


/* Used functions */
static void init(int argc, char** argv, CORBA_Environment** env,
		 erlang_pid* connected);
void terminate(CORBA_Environment* env, int reason);
static int server_loop(CORBA_Environment* env, erlang_pid* ErlpConnected);
int string_to_pid(char* str, erlang_pid* pid);


/* Command line argument flags */
#define NODENAMEARG "-node"  /* Partial node name argument (Node in Node@Host) */
#define HOSTARG "-host"      /* Partial node name argument (Host in Node@Host) */
#define BUFARG "-bufsz"      /* Buffer size argument */
#define COOKIEARG "-cookie"  /* Cookie argument */
#define ERLPIDARG "-erlpid"  /* Connected Erlang node */


/* Buffer sizes */
#define NODENAMESZ 512
#define HOSTNAMESZ 256
#define ATOMSZ 275



int
main(int argc, char **argv)
{
  int ret = 0;              /* Return value, 0 = OK */
  CORBA_Environment* env;   /* CORBA environment (com. buffers, socket desc.) */
  erlang_pid connected;     /* Pid of connected process (through argv) */

#ifdef __WIN32__
    WORD wVersionRequested;
    WSADATA wsaData;
    int err;

    wVersionRequested = MAKEWORD(1, 1);

    err = WSAStartup(wVersionRequested, &wsaData);
    if (err != 0)
	exit(1);

    if (LOBYTE(wsaData.wVersion) != 1 || HIBYTE(wsaData.wVersion ) != 1) {
        WSACleanup();
	exit(1);
    }
#endif

  /* Make stdout unbuffered. */
  setbuf(stdout, NULL);

  /* create file descriptors and buffers */
  init(argc, argv, &env, &connected);

  /* Tell erlang we're running. stdout redirected to port */
  fprintf(stdout, "running\n");

  /* start server loop */
  ret = server_loop(env, &connected);

  /* close file descriptors and free buffers */
  terminate(env, ret);

#ifdef __WIN32__
  WSACleanup();
#endif

  return ret;
}



/* Main loop
 *
 * Returns 0 for normal termination, >0 for exceptional termination.
 */
static int
server_loop(CORBA_Environment* env, /* CORBA environment (com. buffer, sock. desc.)*/
	    erlang_pid* connected)  /* Connected erlang process */
{
  erlang_msg msg;         /* Received message (on socket) */
  int msgtype=100;        /* Type of msg (100 == non-existing type > 0) */
  int ret = 0;

  /* Loop */
  /* Read a msg, handle it, loop while communication is OK (msgtype >= 0) */
  while (ret == 0)
  {

    /* write message to buffer */
    msgtype = ei_receive_encoded(env->_fd,
				 &(env->_inbuf),
				 &(env->_inbufsz),
				 &msg,
				 &(env->_iin));
    switch (msgtype)
    {
    /* got a message */
    case ERL_SEND:
    case ERL_REG_SEND:
      /* do transaction with sd */
      odbclli__switch(NULL, env);

      switch(env->_major)
      {
      /* Success */
      case CORBA_NO_EXCEPTION:
	/* send outdata */
	if (env->_iout > 0)
	  ei_send_encoded(env->_fd, &(env->_caller), env->_outbuf, env->_iout);

/* 	fprintf(stderr, "Successfull call !\n"); */
	break;

      /* System exception, terminate loop */
      case CORBA_SYSTEM_EXCEPTION:
	fprintf(stdout,
		"ODBC c-node: CORBA exception: %s\n",
		(char *) CORBA_exception_value(env));
	CORBA_exception_free(env);
	ret = 1;
	break;

      /* Should not come here, terminate loop */
      default:
	fprintf(stdout,	"ODBC c-node: Request failure.\n");
	ret = 2;
	break;
      }

      break;

    /* got an exit message */
    case ERL_EXIT:

      /* exit from connected process => terminate */
      if ((strcmp(msg.from.node, connected->node) == 0)  &&
	  msg.from.num == connected->num                 &&
	  msg.from.serial == connected->serial           &&
	  msg.from.creation == connected->creation)
      {
/* 	fprintf(stdout, "ODBC c-node: exit from controlling process.\n"); */
	ret = 3;
      }
      break;

    /* ignore other known messages */
    case ERL_LINK:
    case ERL_UNLINK:
    case ERL_NODE_LINK:
    case ERL_GROUP_LEADER:
    case ERL_EXIT2:
    case ERL_TICK:
    case ERL_SEND_TT:
    case ERL_EXIT_TT:
    case ERL_REG_SEND_TT:
    case ERL_EXIT2_TT:
/*       fprintf(stderr, "odbc-server, loop: ignore msg.\n"); */
      break;

    /* msgtype < 0 (== error => termination), or unknown type > 0 (ignored) */
    default:
      if (msgtype < 0)
      {
	fprintf(stdout, "ODBC c-node: Communication failure.\n");
	ret = 4;
      }
      break;
    }
  }

  return ret;
}



/* Initialisation
 *
 * Exits on error.
 */
static void
init(int argc,                /* argc from main */
     char** argv,             /* argv from main */
     CORBA_Environment** env, /* CORBA environment (com. buffer, socket desc.) */
     erlang_pid* connected)   /* Pid of connected process */
{
  char node[HOSTNAMESZ];       /* Partial node name (Node in Node@Host) */
  char host[HOSTNAMESZ];       /* Partial node name (Host in Node@Host) */
  char servernode[NODENAMESZ]; /* Complete node name */
  char* cookie = NULL;         /* Cookie (through argv) */
  long bufsz = 0;              /* Size of buffer for communication with */
                               /* connected process (through argv) */
  int sd;                      /* Socket descriptor */
  struct hostent *h;           /* Hostinfo, in particular the IP address */
  erlang_pid self;             /* Pid of this process */
  int i;                       /* Counter */


  /* read argv */
  for (i=1; i < argc; i+=2)
  {
    if (strcmp(NODENAMEARG, argv[i]) == 0)
      strcpy(node, argv[i+1]);

    else if (strcmp(HOSTARG, argv[i]) == 0)
      strcpy(host, argv[i+1]);

    else if (strcmp(BUFARG, argv[i]) == 0)
      bufsz = strtol(argv[i+1], NULL, 10);

    else if (strcmp(COOKIEARG, argv[i]) == 0)
      cookie = argv[i+1];
    
    else if (strcmp(ERLPIDARG, argv[i]) == 0)
      string_to_pid(argv[i+1], connected);
  }

  /* identify host */
  if (!(h = erl_gethostbyname(host)))
  {
    fprintf(stdout, "ODBC c-node: Can't find own ip address.\n");
    exit(5);
  }

  /* make the nodename server@host */
  sprintf(servernode, "%s@%s", node, host);

  /* initiate */
  erl_init(NULL, 0);

  /* Init connection to erlang node */
  /* host, alive, alive@host, addr, cookie, creation */
  if (!erl_connect_xinit(host,
			 node,
			 servernode,
			 (Erl_IpAddr)(h->h_addr_list[0]),
			 cookie,
			 0))
  {
    fprintf(stdout, "ODBC c-node: erl_connect_xinit failed\n");
    exit(6);
  }

  /* connect to erlang node */
  if ((sd = erl_connect(connected->node)) < 0)
  {
    fprintf(stdout, "ODBC c-node: Connection to erlang node failed\n");
    exit(7);
  }

  /* link to erlang process */
  strcpy(self.node, erl_thisnodename());
  self.num = self.serial = self.creation = 0;
  if (ei_send_link(sd, &self, connected) < 0)
  {
    erl_close_connection(sd);
    fprintf(stdout, "ODBC c-node: Couldn't link to erlang process.\n");
    exit(8);
  }

  /* init env */
  if ((*env = CORBA_Environment_alloc(bufsz, bufsz)) == NULL)
  {
    erl_close_connection(sd);
    fprintf(stdout, "ODBC c-node: Memory allocation failure.\n");
    exit(9);
  }

  (*env)->_fd = sd;
  (*env)->_inbufsz = bufsz;
  (*env)->_outbufsz = bufsz;
  (*env)->_iin = 0;
  (*env)->_iout = 0;

}



/* Termination
 *
 * Free all resources.
 * Write exceptional termination to stdout.
 * No return
 */
void
terminate(CORBA_Environment* env,   /* CORBA environment */
	  int reason)               /* Reason for termination */
{
  /* free buffers */
  CORBA_free(env->_inbuf);
  CORBA_free(env->_outbuf);

  /* free environment */
  CORBA_free(env);

  if (reason > 0)
    exit(reason);

  /* return socket */
  erl_close_connection(env->_fd);
}



/* Converts a string to an erlang_pid
 *
 * Returns 0 for OK.
 */
int
string_to_pid(char* str, erlang_pid* pid)
{
  unsigned char tmp[512];
  char *s;
  int i = 0;
  int idx;

  /* skip the opening bracket, parse at commas  */
  s = strtok(str+1,",");
  while (s)
  {
    tmp[i] = (char)(atoi(s));
    i++;
    s = strtok(NULL,","); /* next token */

    if (i >= sizeof(tmp)) return -1; /* string too long */
  }

  /* skip version, if it's there */
  /* may or may not succeed, doesn't matter */
  idx = 0;
  ei_decode_version((char*) tmp, &idx, NULL); 

  /* now get the pid */
  return ei_decode_pid((char*) tmp, &idx, pid);

}
