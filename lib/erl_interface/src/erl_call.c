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
/*
 * Function: Makes it possible to send and receive Erlang
 *    messages from the (Unix) command line. 
 *    Note: We don't free any memory at all since we only
 *    live for a short while.   
 *
 */

#ifdef __WIN32__
#include <winsock2.h>
#include <direct.h>
#include <windows.h>
#include <winbase.h>
#define MAXHOSTNAMELEN 260

#elif VXWORKS
#include <stdio.h>
#include <string.h>
#include <vxWorks.h>
#include <hostLib.h>
#include <selectLib.h>
#include <ifLib.h>
#include <sockLib.h>
#include <taskLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h> 
#include <time.h>
/*
#include <symLib.h>
#include <sysSymTbl.h>
#include <sysLib.h>
#include <tickLib.h>
#include <a_out.h>
*/
/* #include "netdb.h" */
#include "erl_malloc.h"

#else
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/time.h>
#include <unistd.h>
#include <sys/param.h> 
#include <netdb.h>
#include <sys/times.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif

#include <stdio.h>
#include <stdlib.h>

#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <signal.h>

#include "erl_interface.h"

#if !defined (__WIN32__) && !defined (VXWORKS)
extern int gethostname();
#endif

#ifdef __WIN32__
static void initWinSock(void);
#endif

#ifdef DEBUG
#define D(A) A
#else
#define D(A)
#endif

#define PRINT(t) \
{ \
  print_term(stderr,t); \
  fprintf(stderr,"\n"); \
}

#ifndef HP
#define RSH "rsh"
#endif

extern char *optarg;
extern int optind,optopt;

/*
 * Some nice global variables
 * (I don't think "nice" is the right word actually... -gordon)
 */
static int startp=0,cookiep=0,modp=0,evalp=0,randomp=0;
static int use_long_name=0; /* indicates if -name was used, else -sname or -n */
static int debugp=0,verbosep=0,modsize=0,haltp=0;
static char* progname = NULL;
static char *cookie=NULL,*host=NULL,*node=NULL;
static char *hidden=NULL;
static char *apply=NULL,*erl_script=NULL;
static char *module=NULL,*modname=NULL;

#if (0) /* ifdef USE_LOCAL_HEAP */
#define MEGA_SIZE 1024*16
static unsigned int megabuf[MEGA_SIZE];
#endif /* USE_LOCAL_HEAP */

static unsigned int *erl_heap=NULL;
static long erl_heap_size=0;

#if defined(VXWORKS)
static int unique_id(void){
    return taskIdSelf();
}
#endif

static void usage_noexit() {
  fprintf(stderr,"\nUsage: %s [-[demqrsv]] [-c Cookie] [-h HiddenName] \n", progname);
  fprintf(stderr,"            [-x ErlScript] [-a [Mod [Fun [Args]]]]\n");
  fprintf(stderr,"            (-n Node | -sname Node | -name Node)\n\n");
#ifdef __WIN32__
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a \"erlang length [[a,b,c]]\"\n");
#else
  fprintf(stderr,"  where: -a  apply(Mod,Fun,Args) (e.g -a 'erlang length [[a,b,c]]'\n");
#endif
  fprintf(stderr,"         -c  cookie string; by default read from ~/.erlang.cookie\n");
  fprintf(stderr,"         -d  direct Erlang output to ~/.erl_call.out.<Nodename>\n");
  fprintf(stderr,"         -e  evaluate contents of standard input (e.g echo \"X=1,Y=2,{X,Y}.\"|erl_call -e ...)\n");
  fprintf(stderr,"         -h  specify a name for the erl_call client node\n");
  fprintf(stderr,"         -m  read and compile Erlang module from stdin\n");
  fprintf(stderr,"         -n  name of Erlang node, same as -name\n");
  fprintf(stderr,"         -name  name of Erlang node, expanded to a fully qualified\n");
  fprintf(stderr,"         -sname name of Erlang node, short form will be used\n");
  fprintf(stderr,"         -q  halt the Erlang node (overrides the -s switch)\n");
  fprintf(stderr,"         -r  use a random name for the erl_call client node\n");
  fprintf(stderr,"         -s  start a new Erlang node if necessary\n");
  fprintf(stderr,"         -v  verbose mode, i.e print some information on stderr\n");
  fprintf(stderr,"         -x  use specified erl start script, default is erl\n");
}

static void usage_arg(const char *switchname) {
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  usage_noexit();
  exit(1);
}

static void usage_error(const char *switchname) {
  fprintf(stderr, "Illegal argument \'%s\'.\n", switchname);
  usage_noexit();
  exit(1);
}

static void usage() {
  usage_noexit();
  exit(0);
}


/*
 * Get host entry (by address or name)
 */
/* XXX: will fail on names like '2fun4you'.  */
static struct hostent* get_hostent(char *host)
{
  if (isdigit(*host)) {
    struct in_addr ip_addr;
    int b1, b2, b3, b4;
    long addr;
      
    /* XXX: Use inet_aton() (or inet_pton() and get v6 for free). */
    if (sscanf(host, "%d.%d.%d.%d", &b1, &b2, &b3, &b4) != 4) return NULL;
    addr = inet_addr(host);
    ip_addr.s_addr = htonl(addr);
      
    return erl_gethostbyaddr((char *)&ip_addr,sizeof(struct in_addr), AF_INET);
  }

  return erl_gethostbyname(host);
} /* get_hostent */




/* 
 * This function does only return on success.
 */
static int do_connect(char *nodename, int use_long_name, char *cookie)
{
  int fd;
  int flags;
  int r;

  flags = ERL_START_ENODE |
    (use_long_name? ERL_START_LONG : 0) |
    (verbosep? ERL_START_VERBOSE : 0) |
    (debugp? ERL_START_DEBUG : 0);

  if ((fd = erl_connect(nodename)) >= 0) {
    /* success */
    if (verbosep) 
      fprintf(stderr,"do_connect: now connected to node: <%s> !\n",nodename);
  }
  else {
    char alive[MAXALIVELEN];
    char *hostname;
    struct hostent *h;
    char *cookieargs[] = { "-setcookie",cookie,NULL};
    char **args = (cookie) ? cookieargs : NULL;

    if (!(hostname = strrchr(nodename,'@'))) return ERL_BADARG;
    strncpy(alive,nodename,hostname-nodename);
    alive[hostname-nodename] = 0x0;
    hostname++;

    h = erl_gethostbyname(hostname);


    if ((r=erl_start_sys(alive,(Erl_IpAddr)(h->h_addr_list[0]),flags,erl_script,args)) < 0) {
      erl_err_quit("<ERROR> Unable to start node, error = %d\n",r);
    }

    if ((fd=erl_connect(nodename)) >= 0) {
      /* success */
      if (verbosep) 
	fprintf(stderr,"do_connect: now connected to node: <%s> !\n",nodename);
    }
    else {
      /* (failure) */
      switch (fd) {
      case ERL_NO_DAEMON:
	erl_err_quit("<ERROR> No epmd running!");
	break;
      case ERL_CONNECT_FAIL:
	erl_err_quit("<ERROR> Connect failed!");
	break;
      case ERL_NO_PORT:
	erl_err_quit("<ERROR> Node is not running!");
	break;
      case ERL_TIMEOUT:
	erl_err_quit("<ERROR> Connect timed out!");
	break;
      default:
	erl_err_quit("<ERROR> Error during connect! (%d)",fd);
	break;
      }
    }
  }

  return fd;
} /* do_connect */

#define SKIP_SPACE(s) while(isspace(*(s))) (s)++
#define EAT(s) while (!isspace(*(s)) && (*(s) != '\0')) (s)++

static void split_apply_string(char *str, 
			       char **mod, 
			       char **fun, 
			       char **args)
{
  char *begin=str;
  char *start="start";
  char *empty_list="[]";
  int len;

  SKIP_SPACE(str);
  if (*str == '\0') 
    erl_err_quit("<ERROR> Wrong format of apply string (1) !");
  EAT(str);
  len = str-begin;
  *mod = (char *) calloc(len + 1, sizeof(char));
  memcpy(*mod, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *fun = (char *) calloc(strlen(start)+1, sizeof(char));
    strcpy(*fun, start);
    *args = (char *) calloc(strlen(empty_list)+1, sizeof(char));
    strcpy(*args, empty_list);
    return;
  }
  begin = str;
  EAT(str);
  len = str-begin;
  *fun = (char *) calloc(len + 1, sizeof(char));
  memcpy(*fun, begin, len);

  SKIP_SPACE(str);
  if (*str == '\0') {
    *args = (char *) calloc(strlen(empty_list)+1, sizeof(char));
    strcpy(*args, empty_list);
    return;
  }

  *args = (char *) calloc(strlen(str) + 1, sizeof(char));
  strcpy(*args, str);
  
  return;

} /* split_apply_string */


/* 
 * Read from stdin until EOF is reached.
 * Allocate the buffer needed.
 */
static int read_stdin(char **buf)
{
  char *tmp;
  int bsize=BUFSIZ,len=0,i;

  tmp = (char *) malloc(bsize);
  len = 0;
  while (1) {
    if ((i = read(0, &tmp[len], bsize-len)) < 0)
      erl_err_sys("<ERROR> when reading stdin");
    else if (i == 0) 
      break;
    else {
      len += i;
      if ((len+50) > bsize) {
	bsize = len * 2;
	tmp = (char *) realloc(tmp, bsize);
      }
      else
	continue;
    }
  } /* while */

  *buf = tmp;
  return len;

} /* read_stdin */

/*
 * Get the module from stdin.
 */
static int get_module(char **mbuf, char **mname)
{
  char *tmp;
  int len,i;

  len = read_stdin(mbuf);
  /*
   * Now, get the module name.
   */
  if ((tmp = strstr(*mbuf, "-module(")) != NULL) {
    char *start;
    tmp += strlen("-module(");
    while ((*tmp) == ' ') tmp++; /* eat space */
    start = tmp;
    while (1) {
      if (isalnum(*tmp) || (*tmp == '_')) {
	tmp++;
	continue;
      }
      else 
	break;
    } /* while */
    i = tmp - start;
    *mname = (char *) calloc(i+1, sizeof(char));
    memcpy(*mname, start, i);
  }

  return len;

} /* get_module */

/*
 * --- M A I N ---
 */
#if !defined(VXWORKS)
int main(int argc, char *argv[])
#else
int erl_call(int argc, char **argv)
#endif
{
    int i,fd,creation;
    struct hostent *hp;
    char host_name[MAXHOSTNAMELEN];
    char nodename[MAXNODELEN];
    char *p=NULL;
    char *ct=NULL; /* temporary used when truncating nodename */
    ETERM *reply;

#if (0) /* ifdef USE_LOCAL_HEAP */
    erl_heap = megabuf;
    erl_heap_size = MEGA_SIZE;
#endif /* USE_LOCAL_HEAP */

    progname = argv[0];

    /* Get the command line options */
    i=1;
    while (i < argc) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
	    case 's':
	      if (strcmp(argv[i], "-sname") == 0) { /* -sname NAME */
		if (i+1 >= argc)
		  usage_arg("-sname ");

		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 0;
	      }
	      else if (strcmp(argv[i],"-s") == 0) {
		startp = 1;
	      }
	      else
		usage_error(argv[i]);
	      break;

	    case 'q':
	      if (strlen(argv[i]) == 2) {
		haltp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'v':
	      if (strlen(argv[i]) == 2) {
	      verbosep = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'd':
	      if (strlen(argv[i]) == 2) {
	      debugp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'r':
	      if (strlen(argv[i]) == 2) {
	      randomp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'e':
	      if (strlen(argv[i]) == 2) {
	      evalp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'm':
	      if (strlen(argv[i]) == 2) {
	      modp = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'c':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-c ");
		cookiep = 1;
		cookie = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(cookie, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;
	      
	    case 'n':
	      if (strcmp(argv[i], "-name") == 0) {  /* -name NAME */
		if (i+1 >= argc)
		  usage_arg("-name ");

		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 1;
	      }
	      else if (strcmp(argv[i],"-n") == 0) { /* -n NAME */
		if (i+1 >= argc)
		  usage_arg("-n ");
		
		node = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(node, argv[i+1]);
		i++;
		use_long_name = 1;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'h':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-h ");
		hidden = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(hidden, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'x':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-x ");
		erl_script = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(erl_script, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case 'a':
	      if (strlen(argv[i]) == 2) {
		if (i+1 >= argc)
		  usage_arg("-a ");
		apply = (char *) malloc(strlen(argv[i+1]) + 1);
		strcpy(apply, argv[i+1]);
		i++;
	      }
	      else 
		usage_error(argv[i]);
	      break;

	    case '?':
	    default:
	      usage();
	    }
	}
	else
	  usage();
	i++;

    } /* while */
    
	
    /*
     * Can't have them both !
     */
    if (modp && evalp)
      usage();

    /*
     * Read an Erlang module from stdin.
     */
    if (modp) 
      modsize = get_module(&module, &modname);

    if (verbosep || debugp)
      fprintf(stderr,"Node = %s\nCookie = %s\n"
	      "Flags = %s %s %s\n"
	      "Module: name = %s , size = %d\n"
	      "Apply = %s\n",
	      node,
	      (cookie ? cookie : ""),
	      (startp? "startp" : ""),
	      (verbosep? "verbosep" : ""),
	      (debugp? "debugp" : ""),
	      (modname ? modname : ""), modsize,
	      (apply ? apply : "" ));

    /* 
     * What we, at least, requires !
     */
    if (node == NULL)
      usage();

    if (!cookiep)
      cookie = NULL;

    creation = (time(NULL) % 3) + 1; /* "random" in range 1-3 */

    /* Initiate the erl_interface stuff */
    erl_init((Erl_Heap *) erl_heap, erl_heap_size);

    if (hidden == NULL) {
      /* As default we are c17@gethostname */
      i = randomp ? (time(NULL) % 997) : 17;
      hidden = (char *) malloc(3 + 2 ); /* c17 or cXYZ */
#if defined(VXWORKS)
      sprintf(hidden, "c%d",
	  i < 0 ?  (int) unique_id() : i);
#else
      sprintf(hidden, "c%d",
	  i < 0 ?  (int) getpid() : i);
#endif
    }
    {
      /* A name for our hidden node was specified */
      char h_hostname[MAXHOSTNAMELEN];
      char h_nodename[MAXNODELEN];
      char *h_alivename=hidden;
      struct in_addr h_ipadr;
      char* ct;

#ifdef __WIN32__
      /*
       * XXX Extremly ugly, but needed to get erl_gethostbyname() below
       * to work.
       */
      initWinSock();
#endif

      gethostname(h_hostname, MAXHOSTLEN);
      if ((hp = erl_gethostbyname(h_hostname)) == 0) {
	  erl_err_quit("<ERROR> erl_call: Can't gethostbyname()");
      }
      if (use_long_name == 0) /* shortnames */
	if ((ct = strchr(hp->h_name, '.')) != NULL)
	  *ct = '\0';  
      strcpy(h_hostname, hp->h_name);
      memcpy(&h_ipadr.s_addr, *hp->h_addr_list, sizeof(struct in_addr));
      sprintf(h_nodename, "%s@%s", h_alivename, h_hostname);
      
      if (!erl_connect_xinit(h_hostname, h_alivename, h_nodename,
			     (Erl_IpAddr) &h_ipadr, cookie, creation))
	erl_err_quit("<ERROR> when trying to xinit connect !");
    }
    if ((p = strchr((const char *) node, (int) '@')) == 0) {
      strcpy(host_name, erl_thishostname());
      host = host_name;
    }
    else {
      *p = 0;
      host = p+1;
    }

    /* 
     * Expand name to a real name (may be ip-address) 
     */
    if ((hp = get_hostent(host)) == 0)
      erl_err_quit("<ERROR> get_host_ent");
    if (use_long_name == 0) /* shortnames */
      if ((ct = strchr(hp->h_name, '.')) != NULL)
	*ct = '\0';  
    strcpy(host_name, hp->h_name);
    sprintf(nodename, "%s@%s", node, host_name);

    /* 
     * Try to connect. Start an Erlang system if the
     * start option is on and no system is running.
     */
    if (startp && !haltp) {
      fd = do_connect(nodename, use_long_name,cookie);
    }
    else if ((fd = erl_connect(nodename)) < 0) {
      /* We failed to connect ourself */
      if (haltp)
	  exit(0);
      else 
	  erl_err_quit("<ERROR> erl_connect failed");
    }

    /* If we are connected and the halt switch is set */
    if (fd && haltp) {
      erl_rpc(fd, "erlang", "halt", erl_format("[]"));
      exit(0);
    }

    if (verbosep)
      fprintf(stderr,"We are now connected to node: <%s> !\n",nodename);

    /*
     * Compile the module read from stdin.
     */
    if (modp && (modname != NULL)) {
      char fname[256];

      strcpy(fname, modname);
      strcat(fname, ".erl");
      
      if (!(reply = erl_rpc(fd, "file", "write_file", 
			   erl_format("[~s,~w]", fname, 
				      erl_mk_binary(module, modsize)))))
	erl_err_msg("<ERROR> when writing source file: %s !\n", fname);

      if (!(reply = erl_rpc(fd, "c", "c", erl_format("[~a,[]]", modname))))
	erl_err_msg("<ERROR> when compiling file: %s !\n", fname);

      if (!erl_match(erl_format("{ok,_}"), reply))
	erl_err_msg("<ERROR> compiler errors !\n");
    }
    /*
     * Eval the Erlang functions read from stdin/
     */
    if (evalp) {
      char *evalbuf;
      int len;

      len = read_stdin(&evalbuf);
      if (!(reply = erl_rpc(fd, "lib", "eval_str", 
			    erl_format("[~w]", erl_mk_binary(evalbuf,len)))))
	erl_err_msg("<ERROR> when evaluating input: %s !\n", evalbuf);
      else
	erl_print_term(stdout,reply);
    }
    /*
     * Any Erlang call to be made ?
     */
    if (apply != NULL) {
      char *mod,*fun,*args;
      ETERM *e;

      split_apply_string(apply, &mod, &fun, &args);
      if (verbosep)
	fprintf(stderr,"Mod = %s, Fun = %s, Args = %s\n", mod, fun, args);

      if (!(e = erl_format(args))) 
	exit(-1);

      if (!(reply = erl_rpc(fd, mod, fun, e)))
	exit(-1);
      else
	erl_print_term(stdout,reply);
    }

    return(0);
}

#ifdef __WIN32__
/*
 * XXX This should not be here.  This is a quick fix to make erl_call
 * work at all on Windows NT.
 */
static void
initWinSock(void)
{
    WORD wVersionRequested;  
    WSADATA wsaData; 
    int err; 
    static int initialized;

    wVersionRequested = MAKEWORD(1, 1); 
    if (!initialized) {
	initialized = 1;
	err = WSAStartup(wVersionRequested, &wsaData); 
 
	if (err != 0) {
	    erl_err_msg("<ERROR> erl_connect_init: Can't initialize windows sockets: %d",
			err);
	}
  
	if ( LOBYTE( wsaData.wVersion ) != 1 || 
	    HIBYTE( wsaData.wVersion ) != 1 ) { 
	    erl_err_msg("<ERROR> erl_connect_init: This version of windows sockets "
			"not supported");
	    WSACleanup(); 
	}
    }
}
#endif
