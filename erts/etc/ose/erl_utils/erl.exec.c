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


#include "ose.h"
#include "dbgprintf.h"
#include "malloc.h"
#include "errno.h"
#include "sys/stat.h"
#include "unistd.h"
#include "shell.h"
#include "efs.h"
#include "efs_err.h"
#include "fm.sig"
#include "heapapi.h"
#include "outfmt.h"

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#ifndef MAXPATHLEN
#define MAXPATHLEN 2048
#endif

#define NO 0
#define YES 1

#define PATHSEP ":"
#define DIRSEP "/"
#define NULL_DEVICE "/dev/null"
#define BINARY_EXT ""
#define QUOTE(s) s


void usage(const char *switchname);
void ose_error(char* str, int n);

/*
 * Local functions.
 */

static void mergeargs(int *argc, char ***argv, char **addargs);
static char **build_args_from_env(void);
static void get_parameters(int argc, char** argv);
static void add_arg(char *new_arg);
static void add_Eargs(char *new_arg);
static int dirq(const char *dirpath);
static void *emalloc(size_t size);
static void *erealloc(void *p, size_t size);
static void efree(void *p);
static char* strsave(char* string);
static void get_home(void);
static int start_erl_ose(void);
static int ose_execvp(char *cmd, char **args);
static void print_args(char **args);
static void set_oldshell(int *argc, char ***argv);
static void reset_drivers(void);

void start_epmd(char *epmd);
PROCESS get_erl_bid(void);

/*
 * Variables.
 */
int nohup = 0;
int keep_window = 0;

static char* Eargsp[100];	/* Emulator arguments (to appear first). */
static int EargsCnt = 0;	/* Number of emulator arguments. */
static char *argsp[100];	/* Common arguments. */
static int argsCnt = 0;		/* Number of common arguments */
static char tmpStr[10240];	/* Temporary string buffer. */
static int verbose = 0;		/* If non-zero, print some extra information. */
static int start_detached = 0;	/* If non-zero, the emulator should be
				 * started detached (in the background). */

/* these variables are allocated memory for by get_env() during startup, the erts
   process will free them before terminating */
static char* bindir;		/* Location of executables. */
static char* rootdir;		/* Root location of Erlang installation. */
static char* emu;		/* Emulator to run. */
static char* progname;		/* Name of this program. */
static char* home;		/* Path of user's home directory. */

static char **erl_argv;
static int erl_argc = 0;

PROCESS erl_block;
PROCESS erlang_starter_;

extern PROCESS erl_tmp_;
extern int config_dns(void);
extern int erl_main(int, char **);
extern int erl_set_memory_block(int, int, int, int, int);
extern int erl_memory_show(void);

PROCESS get_erl_bid() {
  return get_bid(erl_tmp_);
}

static int restart = 0;

OS_PROCESS(erts) {
  int haltAfterwards = 0;	/* If true, put 's erlang halt' at the end
				 * of the arguments. */
  int isdistributed = 0;
  int no_epmd = 0;
  int i;
  char* s;
  char *epmd_prog = NULL;
  int process_args = 1;
  int argc = 0;
  char **argv;

  start_detached = 0;

  if(!restart) {
    /* allocate max heap memory that erts needs */
    if((s = get_env(erl_block, "OSE_EXT_HEAP_SIZE")) != NULL) {
      char* addr_str;
      if((addr_str = get_env(erl_block, "OSE_EXT_HEAP_ADDR")) != NULL) {
	size_t ext_heap_size;
	unsigned int ext_heap_addr;
	ext_heap_size = (size_t)atol(s);
	ext_heap_addr = (unsigned int)atol(addr_str);
	erl_dbg_fprintf(stdout, "Will extend heap %u bytes from address %X\n", 
			ext_heap_size, ext_heap_addr);
	heap_extend_heap((void*)ext_heap_addr, ext_heap_size);
	free_buf((union SIGNAL **) &addr_str);
	free_buf((union SIGNAL **) &s);
      } else {
	erl_dbg_fprintf(stderr, "Warning: Variable OSE_EXT_HEAP_ADDR not defined!\n");
	free_buf((union SIGNAL **) &s);
      }
    } else {
      if((s = get_env(erl_block, "OSE_EXT_HEAP_ADDR")) != NULL) {
	erl_dbg_fprintf(stderr, "Warning: Variable OSE_EXT_HEAP_SIZE not defined!\n");
	free_buf((union SIGNAL **) &s);
      }
    }
  } else {
    erl_dbg_fprintf(stdout, "erts restarting...\n");
    reset_drivers();
  }
  
  /* set restart indication so that extend heap is never called twice */
  restart = 1;

  /* assign memory block for erlang */
  if((s = get_env(erl_block, "ERL_HEAP_SIZE")) != NULL) {
    char* addr_str;
    if((addr_str = get_env(erl_block, "ERL_HEAP_ADDR")) != NULL) {
      int erl_heap_size;
      int erl_heap_addr;
      erl_heap_size = atoi(s);
      erl_heap_addr = atoi(addr_str);
      erl_dbg_fprintf(stdout, "Will reserve %d bytes from address %X for Erlang\n", 
		      erl_heap_size, erl_heap_addr);
      erl_set_memory_block((int)erl_heap_size, erl_heap_addr, 1, 1, 0);
      free_buf((union SIGNAL **) &addr_str);
      free_buf((union SIGNAL **) &s);
    } else {
      free_buf((union SIGNAL **) &s);
      erl_dbg_fprintf(stderr, "Error: ERL_HEAP_ADDR not defined!\n");
      kill_proc(current_process());
    }
  } else {
    erl_dbg_fprintf(stderr, "Warning: ERL_HEAP_SIZE and/or ERL_HEAP_ADDR not defined! ");
    erl_dbg_fprintf(stderr, "ELIB_MALLOC must not be used!\n");
  }

  /* copy the argv structure from command shell process */
  argv = (char **)malloc((erl_argc+1) * sizeof(char*));
  while(argc < erl_argc) {
    argv[argc] = (char *)malloc(strlen(erl_argv[argc])+1);
    strcpy(argv[argc], erl_argv[argc]);
    argc++;
  }
  argv[argc] = NULL;

  EargsCnt = 0;
  argsCnt = 0;

  set_oldshell(&argc, &argv);  

  mergeargs(&argc, &argv, build_args_from_env());

  get_parameters(argc, argv);
  add_Eargs(emu);		/* Will be argv[0] -- necessary! */

  /* add the bindir to the path (unless it is there already) */
  sprintf(tmpStr, "%s" DIRSEP "bin", rootdir);
  bindir = strsave(tmpStr);
  if((s = get_env(erl_block, "PATH")) != NULL) {
    if(strstr(s, bindir) == NULL)
      sprintf(tmpStr, "%s" PATHSEP "%s", bindir, s);
    else 
      sprintf(tmpStr, "%s", s);
    free_buf((union SIGNAL **) &s);
  }
  set_env(erl_block, "PATH", strsave(tmpStr));

  get_home();
  add_arg("-home");
  add_arg(home);
  i = 0;
  while (i < argc) {
    if (process_args) {
      switch (argv[i][0]) {
      case '-':
	switch (argv[i][1]) {
	  
	case 'c':
	  if (strcmp(argv[i], "-compile") == 0) {
	    /*
	     * Note that the shell script erl.exec does an recursive call
	     * on itself here.  We'll avoid doing that.
	     */
	    add_arg("-noshell");
	    add_arg("-noinput");
	    add_arg("-s");
	    add_arg("c");
	    add_arg("lc_batch");
	    add_Eargs("-B");
	    haltAfterwards = 0;
	  }
	  else {
	    add_arg(argv[i]);
	  }
	  break;
	  
	case 'd':
	  if (strcmp(argv[i], "-detached") != 0) {
	    add_arg(argv[i]);
	  } else {
	    start_detached = 1;
	    add_arg("-noshell");
	    add_arg("-noinput");
	  }
	  break;
	  
	case 'i':
	  if (strcmp(argv[i], "-instr") == 0)
	    ;	/* already handled above */
	  else
	    add_arg(argv[i]);
	  break;
	  
	case 'e':
	  if (strcmp(argv[i], "-extra") == 0) {
	    process_args = 0;
	    add_arg(argv[i]);
	  } else if (strcmp(argv[i], "-emu_args") == 0) { /* -emu_args */
	    verbose = 1;
	  } else if (strcmp(argv[i], "-env") == 0) { /* -env VARNAME VARVALUE */
	    if (i+2 >= argc)
	      usage("-env");
	    sprintf(tmpStr, "%s=%s", argv[i+1], argv[i+2]);
	    putenv(strsave(tmpStr));
	    i += 2;
	  } else if (strcmp(argv[i], "-epmd") == 0) { 
	    if (i+1 >= argc)
	      usage("-epmd");
	    epmd_prog = argv[i+1];
	    ++i;
	  } else {
	    add_arg(argv[i]);
	  }
	  break;
	case 'k':
	  if (strcmp(argv[i], "-keep_window") == 0) {
	    keep_window = 1;
	  } else
	    add_arg(argv[i]);
	  break;
	  
	case 'm':
	  /*
	   * Note that the shell script erl.exec does an recursive call
	   * on itself here.  We'll avoid doing that.
	   */
	  if (strcmp(argv[i], "-make") == 0) {
	    add_arg("-noshell");
	    add_arg("-noinput");
	    add_arg("-s"); 
	    add_arg("make"); 
	    add_arg("all");
	    add_Eargs("-B");
	    haltAfterwards = 1;
	    i = argc; /* Skip rest of command line */
	  } else if (strcmp(argv[i], "-man") == 0) {    
	    argv[i] = "man";
	    sprintf(tmpStr, "%s/man", rootdir);
	    set_env(erl_block, "MANPATH", strsave(tmpStr));
	    if(ose_execvp("man", argv+i) < 0)
	      ose_error("Could not execute the 'man' command.", 0);      
	    else
	      return;	      
	  } else
	    add_arg(argv[i]);
	  break;
	  
	case 'n':
	  if (strcmp(argv[i], "-name") == 0) { /* -name NAME */
	    if (i+1 >= argc)
	      usage("-name");
	    
	    /*
	     * Note: Cannot use add_args() here, due to non-defined
	     * evaluation order.
	     */
	    
	    add_arg(argv[i]);
	    add_arg(argv[i+1]);
	    isdistributed = 1;
	    i++;
	  } else if (strcmp(argv[i], "-noshell") == 0) {
	    add_arg("-noshell");
	    add_arg("-noinp_shell");
	    start_detached = 1;
	  } else if (strcmp(argv[i], "-noinput") == 0) {
	    add_arg("-noshell");
	    add_arg("-noinput");
	    start_detached = 1;
	  } else if (strcmp(argv[i], "-nohup") == 0) {
	    add_arg("-nohup");
	    nohup = 1;
	  } else if (strcmp(argv[i], "-no_epmd") == 0) {
	    add_arg("-no_epmd");
	    no_epmd = 1;
	  } else {
	    add_arg(argv[i]);
	  }
	  break;
	  
	case 's':	/* -sname NAME */
	  if (strcmp(argv[i], "-sname") == 0) {
	    if (i+1 >= argc)
	      usage("-sname");
	    add_arg(argv[i]);
	    add_arg(argv[i+1]);
	    isdistributed = 1;
	    i++;
	  }	      
	  else
	    add_arg(argv[i]);
	  
	  break;
	  
	case 'v':	/* -version */
	  if (strcmp(argv[i], "-version") == 0)
	    add_Eargs("-V");
	  else
	    add_arg(argv[i]);
	  break;
	  
	case 'x':
	  if (argv[i][2] != '\0') {
	    add_arg(argv[i]);
	  } else {	    
	    sprintf(tmpStr, "%s/lib/xerl", rootdir);
	    if (dirq(tmpStr)) {
	      sprintf(tmpStr, "%s/lib/pxw", rootdir);
	      if (dirq(tmpStr)) {
		add_arg("-x");
		add_arg("-noshell");
		break;
	      }
	    }
	    ose_error("You need the 'xerl' and 'pxw' bundles to run 'erl -x'", 0);
	  }
	  break;
	  
	default:
	  add_arg(argv[i]);
	  break;
	} /* switch(argv[i][1] */
	break;
	
      case '+':
	switch (argv[i][1]) {
	case 'i':
	case 'b':
	case 's':
	case 'h':
	case '#':
	case 'P':
	  {
	    char xx[3];
	    xx[0] = '-';
	    xx[1] = argv[i][1];
	    xx[2] = '\0';
	    if (i+1 >= argc) {
	      xx[0] = '+';
	      usage(xx);
	    }
	    add_Eargs(xx);
	    add_Eargs(argv[i+1]);
	    i++;
	    break;
	  }
	default:
	  argv[i][0] = '-'; /* Change +option to -option. */
	  add_Eargs(argv[i]);
	}
	break;
	
      default:
	if(strcmp(argv[i], "start_erl") != 0)
	  add_arg(argv[i]);
      } /* switch(argv[i][0] */
      i++;
    } else {
      add_arg(argv[i]);
      i++;
    }
  }	      
  /* Doesn't conflict with -extra, since -make skips all the rest of
     the arguments. */
  if (haltAfterwards) {
    add_arg("-s");
    add_arg("erlang");
    add_arg("halt");
  }
  
  /* start_epmd */
  if (isdistributed && !no_epmd)
    start_epmd(epmd_prog);
  
  add_Eargs("--");
  add_Eargs("-root");
  add_Eargs(rootdir);
  add_Eargs("-progname");
  add_Eargs(progname);
  add_Eargs("--");
  for (i = 0; i < argsCnt; i++)
    Eargsp[EargsCnt++] = argsp[i];
  Eargsp[EargsCnt] = NULL;
  
  if (verbose) {
    printf("Executing: %s", emu);
    for (i = 1; i < EargsCnt; i++)
      printf(" %s", Eargsp[i]);
    printf("\n\n");
  }

  start_erl_ose();
}

static void print_args(char **args) {
  int i=0;

  while(args[i] != NULL)
    printf("%s ", args[i++]);

  printf("\n");
  fflush(stdout);
}

void testva(char* format, ...) {
  char buf[100];
  va_list va;
  va_start(va, format);
  vsprintf(buf,format,va);
  printf("%s", buf);
}

#ifndef HAVE_GETHRTIME
#define HAVE_GETHRTIME
#endif

static int start_erl_ose(void) {
  char *start_mode;

  /* signal fsem to sync with shell cmd process (so that shell command 
     function may return if erts is started detached */ 
  signal_fsem(erlang_starter_);

  /* use block variable to indicate for sys which mode we're in */
  if(!start_detached) {
    start_mode = strsave("shell");
    set_env(erl_block, "ERL_START_MODE", start_mode);
    /* also attach to shell command process if erts is in interactive mode */
    attach(NULL, erlang_starter_);
  } else {
    start_mode = strsave("noshell");
    set_env(erl_block, "ERL_START_MODE", start_mode);
  }

  /* make sure data on stdio is never buffered */
  setvbuf(stdin, NULL,  _IONBF, 0);
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stderr, NULL, _IONBF, 0);  
  
  erl_dbg_fprintf(stdout, "beam starting on process: erts (%li)\n", current_process());

  /* erl_main starts the emulator (returns when erlang terminates) */
  erl_main(EargsCnt, Eargsp);

  erl_dbg_fprintf(stdout, "erl_main finished!\n");

  /* clean up environment variables */
  free_buf((union SIGNAL **) &bindir);
  free_buf((union SIGNAL **) &rootdir);
  free_buf((union SIGNAL **) &emu);
  free_buf((union SIGNAL **) &progname);
  free_buf((union SIGNAL **) &home);  
  free_buf((union SIGNAL **) &start_mode);  
}
  
int start_erl(int argc, char **argv) {
  union SIGNAL *sig;
  PROCESS erts_;
  OSENTRYPOINT erts;

  if(hunt("erts", 0, NULL, NULL)) {
    erl_dbg_fprintf(stderr, "ERROR: erts already running\n");
    return 0;
  }

  erl_block = get_bid(erl_tmp_);
  erl_dbg_fprintf(stdout, "Erlang block id: %li\n", (unsigned long)erl_block);
  fflush(stderr);

  /* initialise DNS service, needed for distributed Erlang */
  config_dns();

  erl_argc = argc;
  erl_argv = argv;		/* so that erts may copy the struct */

  /* used by erts to detect shutdown of ose shell (interactive mode only) */
  erlang_starter_ = current_process(); 

  /* create and start the erlang emulator as ose process "erts" */
  erts_ = create_process(OS_BG_PROC, /* processtype */
			 "erts", /* name        */
			 erts,	 /* entrypoint  */
			 524288, /* stacksize   */
			 20,	 /* priority (if OS_PRI_PROC) */
			 0,	 /* timeslice */
			 erl_block, /* block */
			 NULL,0,0); /* not used    */

  set_fsem((OSFSEMVAL) 0, current_process());

  efs_clone(erts_);
  start(erts_);

  /* sync with erts, this function must not return until erts has copied argv */
  wait_fsem((OSFSEMVAL) 1);

  /* in interactive mode the ose shell will "hang", waiting for erts to terminate */
  if(!start_detached) {	
    static const SIGSELECT recv_attach_sig[2] = {1, OS_ATTACH_SIG};
    erl_dbg_fprintf(stdout, "erts started in interactive mode\n");
    attach(NULL, erts_);
    sig = receive((SIGSELECT*)recv_attach_sig);
    erl_dbg_fprintf(stderr, "\n*** Erlang finished ***\n");
    free_buf(&sig);
  }
  return 0;

}

static int ose_execvp(char *cmd, char **args) {
  fprintf(stderr, "Not yet supported\n");
  return 0;
}

void usage(const char *switchname)
{
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  fprintf(stderr,
	  "Usage: erl [-version] [-sname NAME | -name NAME] "
	  "[-noshell] [-noinput] [-env VAR VALUE] [-compile file ...] "
	  "[-make] [-man [manopts] MANPAGE] [-x] [-emu_args] "
	  "[+m SIZE_IN_KB] [+M MAX_NO_OF_MMAPS] [+t SIZE_IN_KB] "
	  "[+T SIZE_IN_KB] [+i BOOT_MODULE] "
	  "[+b BOOT_FUN] [+s STACK_SIZE] [+h HEAP_SIZE] [+# ITEMS] "
	  "[+P MAX_PROCS] [args ...]\n");
}

static int dirq(const char *dirpath)
{
  struct stat statbuf;
  
  if (stat(dirpath, &statbuf) == -1)
    return 0;
  
  return (statbuf.st_mode & S_IFDIR);
}

static void str2args(char *cmd, int *argc, char ***argv) {
  int i=0, j=0, c=0;
  char flag[20];
  char **tmp = *argv;
  int len = strlen(cmd);

  for(i = 0; i <= len; i++) {      
    if(((i == len) || (cmd[i] == ' ')) && (j > 0)) {
      flag[j] = '\0';
      tmp[c] = malloc(strlen(flag)+1);
      strcpy(tmp[c++], flag);
      j = 0;
    } else {
      flag[j++] = cmd[i];
    }
  }
  tmp[(*argc = c)] = NULL;
}


static void add_arg(char *new_arg)
{
    argsp[argsCnt++] = QUOTE(new_arg);
}

static void add_Eargs(char *new_arg)
{
    Eargsp[EargsCnt++] = QUOTE(new_arg);
}

void ose_error(char *str, int n) {
  fprintf(stderr, str);
  fflush(stderr);
  error(n);
}

static void *emalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
	ose_error("Insufficient memory", 0);
    return p;
}

static void *erealloc(void *p, size_t size)
{
    void *res = realloc(p, size);
    if (res == NULL)
	ose_error("Insufficient memory", 0);
    return res;
}

static void efree(void *p) 
{
    free(p);
}


char* strsave(char* string)
{
    char* p = emalloc(strlen(string)+1);
    strcpy(p, string);
    return p;
}

static void get_parameters(int argc, char** argv)
{
  progname = get_env(erl_block, "PROGNAME");
  rootdir = get_env(erl_block, "ROOTDIR");
  emu = get_env(erl_block, "EMU");
  if (!progname || !rootdir || !emu ) {
    ose_error("PROGNAME, ROOTDIR and EMU must be set", 0);
  }
}

static void get_home(void)
{
    home = get_env(erl_block, "HOME");
    if (home == NULL)
	ose_error("HOME must be set", 0);
}

static char **build_args_from_env(void)
{
    int argc = 0;
    char **argv = NULL;
    int alloced = 0;
    char **cur_s = NULL;	/* Initialized to avoid warning. */
    int s_alloced = 0;
    int s_pos = 0;
    char *p;
    enum {Start, Build, Build0, BuildSQuoted, BuildDQuoted, AcceptNext} state;

#define ENSURE()					\
    if (s_pos >= s_alloced) {			        \
	if (!*cur_s) {					\
	    *cur_s = emalloc(s_alloced = 20);		\
	} else {					\
	    *cur_s = erealloc(*cur_s, s_alloced += 20);	\
	}						\
    }

    if (!(p = get_env(erl_block, "ERL_FLAGS")))
	return NULL;
    argv = emalloc(sizeof(char *) * (alloced = 10));
    state = Start;
    for(;;) {
	switch (state) {
	case Start:
	    if (!*p) 
		goto done;
	    if (argc >= alloced - 1) { /* Make room for extra NULL */
		argv = erealloc(argv, (alloced += 10) * sizeof(char *));
	    }
	    cur_s = argc + argv;
	    *cur_s = NULL;
	    s_pos = 0;
	    s_alloced = 0;
	    state = Build0;
	    break;
	case Build0:
	    switch (*p) {
	    case ' ':
		++p;
		break;
	    case '\0':
		state = Start;
		break;
	    default:
		state = Build;
		break;
	    }
	    break;
	case Build:
	    switch (*p) {
	    case ' ':
	    case '\0':
		ENSURE();
		(*cur_s)[s_pos] = '\0';
		++argc;
		state = Start;
		break;
	    case '"':
		++p;
		state = BuildDQuoted;
		break;
	    case '\'':
		++p;
		state = BuildSQuoted;
		break;
	    case '\\':
		++p;
		state = AcceptNext;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case BuildDQuoted:
	    switch (*p) {
	    case '"':
		++p;
		/* fall through */
	    case '\0':
		state = Build;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case BuildSQuoted:
	    switch (*p) {
	    case '\'':
		++p;
		/* fall through */
	    case '\0':
		state = Build;
		break;
	    default:
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
		break;
	    }
	    break;
	case AcceptNext:
	    if (!*p) {
		state = Build;
	    } else {
		ENSURE();
		(*cur_s)[s_pos++] = *p++;
	    }
	    state = Build;
	    break;
	}
    }
 done:
    if(p != NULL) free_buf((union SIGNAL **)&p);
    argv[argc] = NULL; /* Sure to be large enough */
    if (!argc) {
	efree(argv);
	return NULL;
    }
    return argv;
#undef ENSURE
}
		
void mergeargs(int *argc, char ***argv, char **addargs)
{
    int add = 0;
    char **tmp = addargs;
    char **res;
    char **rtmp;

    if (!addargs) {
	return;
    }

    while(*tmp++) {
	++add;
    }
    *argc +=add;
    res = emalloc((*argc + 1) * sizeof(char *));
    rtmp = res;
    for (tmp = *argv; (*rtmp = *tmp) != NULL; ++tmp, ++rtmp)
	;
    for (tmp = addargs; (*rtmp = *tmp) != NULL; ++tmp, ++rtmp)
	;
    *argv = res;
}

static void set_oldshell(int *argc, char ***argv) {
  int i;
  char *sh_flag = "-oldshell";
  char **tmp;

  tmp = *argv;

  for(i = 0; i < *argc; i++) {
    /* printf("%d: %s \n", i, tmp[i]); */
    if(strcmp(tmp[i], sh_flag) == 0)
      return;
  }

  tmp[*argc] = (char *)malloc(strlen(sh_flag));
  strcpy(tmp[*argc], sh_flag);
  tmp[*argc+1] = NULL;
  (*argc)++;

  /* print_args(*argv); */

}

static void reset_drivers() {
  extern void reset_ddll_drv(void);

  reset_ddll_drv();
}

/******************************* EPMD *******************************/
extern int start_ose_epmd(int, char**);

void start_epmd(char *epmd) {
  int i = 1;
  char epmd_flags[100];
  int argc;
  char **argv;
  int result;
  
  if(epmd != NULL) 
    strcpy(epmd_flags, epmd);
  else
    strcpy(epmd_flags, "epmd");

#ifdef DEBUG
  strcat(epmd_flags, " -d -d");
  i = 3;
  /* #else
  strcat(epmd_flags, " -d");
  i = 2; */
#endif

  argv = (char **)malloc((i+1)*sizeof(char*));

  str2args(epmd_flags, &argc, &argv);
  
  result = start_ose_epmd(argc, argv);
  
  for(i=0; i<argc; i++)
    free(argv[i]);
  free(argv);
}

/*************************** FOR DEBUGGING ***************************/
/* use this function to print both to standard out and to 
   debug printout driver (see Makefile for com port config) */

char dbg_print_buf[4096];

int erl_dbg_fputc(char ch, FILE* stream) {
  dbgprintf("%c", ch);
  return fputc(ch, stdout);
}

int erl_dbg_vfprintf(FILE* stream, char* format, va_list args) {
  int r;
  r = vsnprintf(dbg_print_buf, sizeof(dbg_print_buf), format, args);
  dbgprintf(dbg_print_buf);	/* print to driver */
  dbgprintf("\r");
  fprintf(stream, dbg_print_buf); /* print to stream */
  return r;
}

int erl_dbg_fprintf(FILE* stream, char* format, ...) { 
  int r;
  va_list va;
  va_start(va, format);
  r = erl_dbg_vfprintf(stream, format, va);
  va_end(va);
  return r;
}

/* dummy start function */
int erl_lm_init(int argc, char **argv) {
  static const SIGSELECT recv_any[] = {0};
  stop(current_process());
}

/************************* USEFUL SHELL CMDS *************************/
/* $ ext_heap <size> <startaddr> */
static int ext_heap(int argc, char **argv) {
  unsigned int ext_heap_addr;
  size_t ext_heap_size;

  if(argc < 3) {
    fprintf(stderr, "Missing arguments! Usage: ext_heap <size> <startaddr>\n");
    return 0;
  }

  ext_heap_size = (size_t)atol(argv[1]);
  ext_heap_addr = (unsigned int)atol(argv[2]);

  fprintf(stdout, "Will extend heap with %u bytes from address %X\n", 
	  ext_heap_size, ext_heap_addr);

  heap_extend_heap((void*)ext_heap_addr, ext_heap_size);
  return 0;
}

static int mem_show(int argc, char **argv) {
  erl_memory_show();
  return 0;
}

/************************* ERLANG START HOOKS **********************************/

void erlHooks(void) {
  extern int start_ose_epmd(int argc, char **argv);
  extern int get_conf(int argc, char **argv);
  extern int gethostname(int argc, char **argv);
  extern int dns_config(int argc, char **argv);
  extern int nslookup(int argc, char **argv);

  /* register shell commands */

  shell_add_cmd("start_erl", "start_erl [flags]", "start erlang emu", start_erl);

  shell_add_cmd("start_epmd", "start_epmd [flags]", "start erlang port mapper deamon", 
		start_ose_epmd);

  shell_add_cmd("getconf", "getconf < num > 2 >", "list interfaces", get_conf);

  shell_add_cmd("gethostname", "gethostname", "get hostname", gethostname);

  shell_add_cmd("dns_config", "dns_config", "configure DNS resolver", dns_config);

  shell_add_cmd("nslookup", "nslookup", "lookup host name or address", nslookup);

  shell_add_cmd("ext_heap", "ext_heap <size>", "extend heap <size> bytes", ext_heap);

  shell_add_cmd("erl_mem_show", "erl_mem_show", "show erts heap info", mem_show);
}

OS_PROCESS(erl_tmp) {
  stop(current_process());
}









