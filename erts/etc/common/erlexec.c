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
 * This is a C version of the erl.exec Bourne shell script, including
 * additions required for Windows NT.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_driver.h"
#include <stdlib.h>
#include <stdarg.h>

#ifdef __WIN32__
#  include "erl_version.h"
#  include "init_file.h"
#endif

#define NO 0
#define YES 1

#ifdef __WIN32__
#define INI_FILENAME "erl.ini"
#define INI_SECTION "erlang"
#define DIRSEP "\\"
#define PATHSEP ";"
#define NULL_DEVICE "nul"
#define BINARY_EXT ".exe"
#else
#define PATHSEP ":"
#define DIRSEP "/"
#define NULL_DEVICE "/dev/null"
#define BINARY_EXT ""
#endif
#define QUOTE(s) s

/* +M alloc_util allocators */
static const char plusM_au_allocs[]= {
    'B',	/* binary_alloc		*/
    'D',	/* std_alloc		*/
    'E',	/* ets_alloc		*/
    'H',	/* eheap_alloc		*/
    'L',	/* ll_alloc		*/
    'S',	/* sl_alloc		*/
    'T',	/* temp_alloc		*/
    '\0'
};

/* +M alloc_util allocator specific arguments */
static char *plusM_au_alloc_switches[] = {
    "as",
    "asbcst",
    "e",
    "lmbcs",
    "mbcgs",
    "mbsd",
    "mmbcs",
    "mmmbc",
    "mmsbc",
    "msbclt",
    "rsbcmt",
    "rsbcst",
    "sbct",
    "smbcs",
    NULL
};

/* +M other arguments */
static char *plusM_other_switches[] = {
    "ea",
    "ummc",
    "uycs",
    "im",
    "is",
    "it",
    "Mamcbf",
    "Mrmcbf",
    "Mmcs",
    "Mcci",
    "Fe",
    "Ye",
    "Ym",
    "Ytp",
    "Ytt",
    NULL
};


/*
 * Define sleep(seconds) in terms of Sleep() on Windows.
 */

#ifdef __WIN32__
#define sleep(seconds) Sleep(seconds*1000)
#endif

#define SHARED_SUFFIX	  ".shared"
#define HYBRID_SUFFIX	  ".hybrid"

/* The length of the longest memory architecture suffix. */
#define MA_SUFFIX_LENGTH  strlen(SHARED_SUFFIX)

/*
 * Define flags for different memory architectures.
 */
#define MA_SHARED    0x0001
#define MA_HYBRID    0x0002

void usage(const char *switchname);
void start_epmd(char *epmd);
void error(char* format, ...);

/*
 * Local functions.
 */

static void mergeargs(int *argc, char ***argv, char **addargs);
static char **build_args_from_env(char *env_var);
static void get_parameters(int argc, char** argv);
static void add_arg(char *new_arg);
static void add_args(char *first_arg, ...);
static void ensure_EargsSz(int sz);
static void add_Eargs(char *new_arg);
static void *emalloc(size_t size);
static void *erealloc(void *p, size_t size);
static void efree(void *p);
static char* strsave(char* string);
static int is_one_of_strings(char *str, char *strs[]);
static char *write_str(char *to, char *from);
static void get_home(void);
#ifdef __WIN32__
static void get_start_erl_data(char *);
static char* get_value(HKEY key, char* value_name, BOOL mustExit);
static char* possibly_quote(char* arg);

/* 
 * Functions from win_erlexec.c
 */
int start_win_emulator(char* emu, char** argv, int start_detached);
int start_emulator(char* emu, char** argv, int start_detached);
#endif



/*
 * Variables.
 */
int nohup = 0;
int keep_window = 0;

static char **Eargsp = NULL;	/* Emulator arguments (to appear first). */
static int EargsSz = 0;		/* Size of Eargsp */
static int EargsCnt = 0;	/* Number of emulator arguments. */
static char **argsp = NULL;	/* Common arguments. */
static int argsCnt = 0;		/* Number of common arguments */
static int argsSz = 0;		/* Size of argsp */
static char tmpStr[10240];	/* Temporary string buffer. */
static int verbose = 0;		/* If non-zero, print some extra information. */
static int start_detached = 0;	/* If non-zero, the emulator should be
				 * started detached (in the background).
				 */
static int mem_arch = 0;	/* If non-zero, start beam.ARCH or beam.ARCH.exe
				 * instead of beam or beam.exe, where ARCH is defined by flags. */

#ifdef __WIN32__
static char* key_val_name = ERLANG_VERSION; /* Used by the registry
					   * access functions.
					   */
static char* boot_script = NULL; /* used by option -start_erl and -boot */
static char* config_script = NULL; /* used by option -start_erl and -config */

static HANDLE this_module_handle;
static int run_werl;

#endif

/*
 * Needed parameters to be fetched from the environment (Unix)
 * or the ini file (Win32).
 */

static char* bindir;		/* Location of executables. */
static char* rootdir;		/* Root location of Erlang installation. */
static char* emu;		/* Emulator to run. */
static char* progname;		/* Name of this program. */
static char* home;		/* Path of user's home directory. */

/*
 * Add the arcitecture suffix to the program name if needed,
 * except on Windows, where we insert it just before ".EXE".
 */
static char*
add_extra_suffixes(char *prog, int memarch)
{
   char *res;
   char *p;
   int len;
#ifdef __WIN32__
   char *exe_p;
   int exe = 0;
#endif

   if (!memarch) {
       return prog;
   }

   len = strlen(prog);

   /* Worst-case allocation */
   p = emalloc(len +
	       MA_SUFFIX_LENGTH +
	       + 1);
   res = p;
   p = write_str(p, prog);

#ifdef __WIN32__
   exe_p = res + len - 4;
   if (exe_p >= res) {
      if (exe_p[0] == '.' &&
	  (exe_p[1] == 'e' || exe_p[1] == 'E') &&
	  (exe_p[2] == 'x' || exe_p[2] == 'X') &&
	  (exe_p[3] == 'e' || exe_p[3] == 'E')) {
	  p = exe_p;
	  exe = 1;
      }
   }
#endif

   if (memarch == MA_SHARED) {
       p = write_str(p, SHARED_SUFFIX);
   }
   else if (memarch == MA_HYBRID) {
       p = write_str(p, HYBRID_SUFFIX);
   }
#ifdef __WIN32__
   if (exe) {
       p = write_str(p, ".exe");
   }
#endif

   return res;
}

#ifdef __WIN32__
__declspec(dllexport) int win_erlexec(int argc, char **argv, HANDLE module, int windowed)
#else
int main(int argc, char **argv)
#endif
{
    int haltAfterwards = 0;	/* If true, put 's erlang halt' at the end
				 * of the arguments. */
    int isdistributed = 0;
    int no_epmd = 0;
    int i;
    char* s;
    char *epmd_prog = NULL;
    char *malloc_lib;
    int process_args = 1;
    int print_args_exit = 0;

#ifdef __WIN32__
    this_module_handle = module;
    run_werl = windowed;
    /* if we started this erl just to get a detached emulator, 
     * the arguments are already prepared for beam, so we skip
     * directly to start_emulator */
    s = getenv("ERL_CONSOLE_MODE");
    if (s != NULL && strcmp(s, "detached")==0) {
	ensure_EargsSz(argc + 1);
	memcpy((void *) Eargsp, (void *) argv, argc * sizeof(char *));
	Eargsp[argc] = NULL;
	goto skip_arg_massage;
    }   
#endif
    mergeargs(&argc, &argv, build_args_from_env("ERL_FLAGS"));
    mergeargs(&argc, &argv,
	      build_args_from_env("ERL_" OTP_SYSTEM_VERSION "_FLAGS"));

    i = 1;
#ifdef __WIN32__
    if ((argc > 2) && (strcmp(argv[i], "-regkey") == 0)) {
	key_val_name = strsave(argv[i+1]);
	i = 3;
    }
#endif		

    get_parameters(argc, argv);
    
    /*
     * Construct the path of the executable.
     */

    /* We need to do this before the ordinary processing. */
    malloc_lib = getenv("ERL_MALLOC_LIB");
    while (i < argc) {
	if (argv[i][0] == '+') {
	    if (argv[i][1] == 'M' && argv[i][2] == 'Y' && argv[i][3] == 'm') {
		if (argv[i][4] == '\0') {
		    if (++i < argc)
			malloc_lib = argv[i];
		    else
			usage("+MYm");
		}
		else
		    malloc_lib = &argv[i][4];
	    }
	}
	else if (argv[i][0] == '-') {
	    if (strcmp(argv[i], "-shared") == 0) {
		mem_arch = MA_SHARED;
	    }
	    else if (strcmp(argv[i], "-hybrid") == 0) {
		mem_arch = MA_HYBRID;
	    }
	}
	i++;
    }

    if (malloc_lib) {
	if (strcmp(malloc_lib, "libc") != 0)
	    usage("+MYm");
    }
#if !defined(__WIN32__)
    emu = add_extra_suffixes(emu, mem_arch);
    sprintf(tmpStr, "%s" DIRSEP "%s" BINARY_EXT, bindir, emu);
    emu = strsave(tmpStr);
#endif

    add_Eargs(emu);		/* Will be argv[0] -- necessary! */

    /*
     * Add the bindir to the path (unless it is there already).
     */

    if ((s = getenv("PATH")) == NULL) {
	sprintf(tmpStr, "PATH=%s" PATHSEP "%s" DIRSEP "bin", bindir, rootdir);
    } else if (strstr(s, bindir) == NULL) {
	sprintf(tmpStr, "PATH=%s" PATHSEP "%s" DIRSEP "bin" PATHSEP "%s", bindir, 
		rootdir, s);
    } else {
	sprintf(tmpStr, "PATH=%s", s);
    }
    putenv(strsave(tmpStr));

    i = 1;

    get_home();
    add_args("-home", home, NULL);
    while (i < argc) {
	if (process_args) {
	    switch (argv[i][0]) {
	      case '-':
		switch (argv[i][1]) {
#ifdef __WIN32__
		case 'b':
		    if (strcmp(argv[i], "-boot") == 0) {
			if (boot_script)
			    error("Conflicting -start_erl and -boot options");
			if (i+1 >= argc)
			    usage("-boot");
			boot_script = strsave(argv[i+1]);
			i++;
		    }
		    else {
			add_arg(argv[i]);
		    }
		    break;
#endif
		case 'c':
		    if (strcmp(argv[i], "-compile") == 0) {
			/*
			 * Note that the shell script erl.exec does an recursive call
			 * on itself here.  We'll avoid doing that.
			 */
			add_args("-noshell", "-noinput", "-s", "c", "lc_batch",
				 NULL);
			add_Eargs("-B");
			haltAfterwards = 0;
		    }
#ifdef __WIN32__
		    else if (strcmp(argv[i], "-config") == 0){
			if (config_script)
			    error("Conflicting -start_erl and -config options");
			if (i+1 >= argc)
			    usage("-config");
			config_script = strsave(argv[i+1]);
			i++;
		    }
#endif
		    else {
			add_arg(argv[i]);
		    }
		    break;

		  case 'd':
		    if (strcmp(argv[i], "-detached") != 0) {
			add_arg(argv[i]);
		    } else {
			start_detached = 1;
			add_args("-noshell", "-noinput", NULL);
		    }
		    break;

		  case 'i':
		    if (strcmp(argv[i], "-instr") == 0) {
			add_Eargs("-Mim");
			add_Eargs("true");
		    }
		    else
			add_arg(argv[i]);
		    break;

		  case 'e':
		    if (strcmp(argv[i], "-extra") == 0) {
			process_args = 0;
			add_arg(argv[i]);
		    } else if (strcmp(argv[i], "-emu_args") == 0) { /* -emu_args */
			verbose = 1;
		    } else if (strcmp(argv[i], "-emu_args_exit") == 0) {
			print_args_exit = 1;
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
			add_args("-noshell", "-noinput", "-s", "make", "all", NULL);
			add_Eargs("-B");
			haltAfterwards = 1;
			i = argc; /* Skip rest of command line */
		    } else if (strcmp(argv[i], "-man") == 0) {
#if defined(__WIN32__)
			error("-man not supported on Windows");
#else
			argv[i] = "man";
			sprintf(tmpStr, "MANPATH=%s/man", rootdir);
			putenv(strsave(tmpStr));
			execvp("man", argv+i);
			error("Could not execute the 'man' command.");
#endif
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
		    } else if (strcmp(argv[i], "-noinput") == 0) {
			add_args("-noshell", "-noinput", NULL);
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
#ifdef __WIN32__
		    else if (strcmp(argv[i], "-service_event")) {
			add_arg(argv[i]);
			add_arg(argv[i+1]);
			i++;
		    }		    
		    else if (strcmp(argv[i], "-start_erl") == 0) {
			if (i+1 < argc && argv[i+1][0] != '-') {
			    get_start_erl_data(argv[i+1]);
			    i++;
			} else
			    get_start_erl_data((char *) NULL);
		    }
#endif
		    else
			add_arg(argv[i]);
		
		    break;
	
		  case 'v':	/* -version */
		    if (strcmp(argv[i], "-version") == 0)
			add_Eargs("-V");
		    else
			add_arg(argv[i]);
		    break;

		  default:
		    add_arg(argv[i]);
		    break;
		} /* switch(argv[i][1] */
		break;

	      case '+':
		switch (argv[i][1]) {
		  case '#':
		  case 'A':
		  case 'b':
		  case 'h':
		  case 'i':
		  case 'P':
		  case 'R':
		  case 'W':
		  case 'K':
		      if (argv[i][2] != '\0')
			  goto the_default;
		      if (i+1 >= argc)
			  usage(argv[i]);
		      argv[i][0] = '-';
		      add_Eargs(argv[i]);
		      add_Eargs(argv[i+1]);
		      i++;
		      break;
		  case 'B':
		      argv[i][0] = '-';
		      if (argv[i][2] != '\0') {
			  if ((argv[i][2] != 'i') &&
			      (argv[i][2] != 'c') &&
			      (argv[i][2] != 'd')) { 
			  usage(argv[i]);
			} else {
			  add_Eargs(argv[i]);
			  break;
			}
		      }
		      if (i+1 < argc) {
			if ((argv[i+1][0] != '-') &&
			    (argv[i+1][0] != '+')) {
			  if (argv[i+1][0] == 'i') {
			    add_Eargs(argv[i]);
			    add_Eargs(argv[i+1]);
			    i++;
			    break;
			  } else {
			    usage(argv[i]);
			  }
			}
		      }
		      add_Eargs(argv[i]);
		      break;
		  case 'M': {
		      int x;
		      for (x = 0; plusM_au_allocs[x]; x++)
			  if (plusM_au_allocs[x] == argv[i][2])
			      break;
		      if ((plusM_au_allocs[x]
			   && is_one_of_strings(&argv[i][3],
						plusM_au_alloc_switches))
			  || is_one_of_strings(&argv[i][2],
					       plusM_other_switches)) {
			  if (i+1 >= argc
			      || argv[i+1][0] == '-'
			      || argv[i+1][0] == '+')
			      usage(argv[i]);
			  argv[i][0] = '-';
			  add_Eargs(argv[i]);
			  add_Eargs(argv[i+1]);
			  i++;
		      }
		      else
			  goto the_default;
		      break;
		  }
		  default:
		  the_default:
		    argv[i][0] = '-'; /* Change +option to -option. */
		    add_Eargs(argv[i]);
		}
		break;
      
	      default:
		add_arg(argv[i]);
	    } /* switch(argv[i][0] */
	    i++;
	} else {
	    add_arg(argv[i]);
	    i++;
	}
    }

#ifdef __WIN32__
    if (boot_script)
	add_args("-boot", boot_script, NULL);
    if (config_script)
	add_args("-config", config_script, NULL);
#endif

    /* Doesn't conflict with -extra, since -make skips all the rest of
       the arguments. */
    if (haltAfterwards) {
	add_args("-s", "erlang", "halt", NULL);
    }
    
    if (isdistributed && !no_epmd)
	start_epmd(epmd_prog);

#if (! defined(__WIN32__)) && defined(DEBUG)
    if (start_detached) {
	/* Start the emulator within an xterm.
	 * Move up all arguments and insert
	 * "xterm -e " first.
	 * The path must be searched for this 
	 * to work, i.e execvp() must be used. 
	 */
	ensure_EargsSz(EargsCnt+2);
	for (i = EargsCnt; i > 0; i--)
	    Eargsp[i+1] = Eargsp[i-1]; /* Two args to insert */
	EargsCnt += 2; /* Two args to insert */
	Eargsp[0] = emu = "xterm";
	Eargsp[1] = "-e";
    }    
#endif
    
    add_Eargs("--");
    add_Eargs("-root");
    add_Eargs(rootdir);
    add_Eargs("-progname");
    add_Eargs(progname);
    add_Eargs("--");
    ensure_EargsSz(EargsCnt + argsCnt + 1);
    for (i = 0; i < argsCnt; i++)
	Eargsp[EargsCnt++] = argsp[i];
    Eargsp[EargsCnt] = NULL;
    
    if (print_args_exit) {
	for (i = 1; i < EargsCnt; i++)
	    printf("%s ", Eargsp[i]);
	printf("\n");
	exit(0);
    }

    if (verbose) {
	printf("Executing: %s", emu);
	for (i = 0; i < EargsCnt; i++)
	    printf(" %s", Eargsp[i]);
	printf("\n\n");
    }


#ifdef __WIN32__

    if (EargsSz != EargsCnt + 1)
	Eargsp = (char **) erealloc((void *) Eargsp, (EargsCnt + 1) * 
				    sizeof(char *));
    efree((void *) argsp);

 skip_arg_massage:

    if (run_werl) {
      return start_win_emulator(emu, Eargsp, start_detached);
    } else {
      return start_emulator(emu, Eargsp, start_detached);
    }

#else
    if (start_detached) {
	int status = fork();
	if (status != 0)	/* Parent */
	    return 0;
	status = fork();
	if (status != 0)	/* Parent */
	    return 0;

	/*
	 * Grandchild.
	 */
	close(0);
	open("/dev/null", O_RDONLY);
	close(1);
	open("/dev/null", O_WRONLY);
	close(2);
	open("/dev/null", O_WRONLY);
#ifdef DEBUG
	execvp(emu, Eargsp); /* "xterm ..." needs to search the path */
#endif
    } 
#ifdef DEBUG
    else
#endif
	execv(emu, Eargsp);
    error("Error %d executing \'%s\'.", errno, emu);
    return 1;
#endif
}

void
usage(const char *switchname)
{
  fprintf(stderr, "Missing argument(s) for \'%s\'.\n", switchname);
  fprintf(stderr,
	  "Usage: erl [-version] [-sname NAME | -name NAME] "
	  "[-noshell] [-noinput] [-env VAR VALUE] [-compile file ...] "
#ifdef __WIN32__
	  "[-start_erl [datafile]] "
#endif
	  "[-make] [-man [manopts] MANPAGE] [-x] [-emu_args] "
	  "[+A THREADS] [+B[c|d|i]] [+c] [+h HEAP_SIZE] [+K BOOLEAN] "
	  "[+l] [+M<SUBSWITCH> <ARGUMENT>] [+P MAX_PROCS] [+R COMPAT_REL] "
	  "[+r] [+V] [+v] [+W<i|w>] [args ...]\n");
  exit(1);
}

void
start_epmd(char *epmd)
{
    char  epmd_cmd[MAXPATHLEN+100];
#ifdef __WIN32__
    char* arg1 = NULL;
#endif
    int   result;

    if (!epmd) {
	epmd = epmd_cmd;
#ifdef __WIN32__
	sprintf(epmd_cmd, "%s" DIRSEP "epmd", bindir);
	arg1 = "-daemon";
#else
	sprintf(epmd_cmd, "%s" DIRSEP "epmd -daemon", bindir);
#endif
    } 
#ifdef __WIN32__
    if (arg1 != NULL) {
	strcat(epmd, " ");
	strcat(epmd, arg1);
    }
    {
	STARTUPINFO start;
	PROCESS_INFORMATION pi;
	memset(&start, 0, sizeof (start));
	start.cb = sizeof (start);
	if (!CreateProcess(NULL, epmd, NULL, NULL, FALSE, 
			       CREATE_DEFAULT_ERROR_MODE | DETACHED_PROCESS,
			       NULL, NULL, &start, &pi))
	    result = -1;
	else
	    result = 0;
    }
#else
    result = system(epmd);
#endif
    if (result == -1) {
      fprintf(stderr, "Error spawning %s (error %d)\n", epmd_cmd,errno);
      exit(1);
    }
}

static void
add_arg(char *new_arg)
{
    if (argsCnt >= argsSz)
	argsp = (char **) erealloc((void *) argsp,
				   sizeof(char *) * (argsSz += 20));
    argsp[argsCnt++] = QUOTE(new_arg);
}

static void
add_args(char *first_arg, ...)
{
    va_list ap;
    char* arg;
    
    add_arg(first_arg);
    va_start(ap, first_arg);
    while ((arg = va_arg(ap, char *)) != NULL) {
	add_arg(arg);
    }
    va_end(ap);
}

static void
ensure_EargsSz(int sz)
{
    if (EargsSz < sz)
	Eargsp = (char **) erealloc((void *) Eargsp,
				    sizeof(char *) * (EargsSz = sz));
}

static void
add_Eargs(char *new_arg)
{
    if (EargsCnt >= EargsSz)
	Eargsp = (char **) erealloc((void *) Eargsp,
				    sizeof(char *) * (EargsSz += 20));
    Eargsp[EargsCnt++] = QUOTE(new_arg);
}

#if !defined(__WIN32__)
void error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;

    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);
    fprintf(stderr, "erlexec: %s\n", sbuf);
    exit(1);
}
#endif

static void *
emalloc(size_t size)
{
    void *p = malloc(size);
    if (p == NULL)
	error("Insufficient memory");
    return p;
}

static void *
erealloc(void *p, size_t size)
{
    void *res = realloc(p, size);
    if (res == NULL)
	error("Insufficient memory");
    return res;
}

static void
efree(void *p) 
{
    free(p);
}

static int
is_one_of_strings(char *str, char *strs[])
{
    int i, j;
    for (i = 0; strs[i]; i++) {
	for (j = 0; str[j] && strs[i][j] && str[j] == strs[i][j]; j++);
	if (!str[j] && !strs[i][j])
	    return 1;
    }
    return 0;
}

static char *write_str(char *to, char *from)
{
    while (*from)
	*(to++) = *(from++);
    *to = '\0';
    return to;
}

char*
strsave(char* string)
{
    char* p = emalloc(strlen(string)+1);
    strcpy(p, string);
    return p;
}


#if defined(__WIN32__)

static void get_start_erl_data(char *file)
{
    int fp;
    char tmpbuffer[512];
    char start_erl_data[512];
    int bytesread;
    char* reldir;
    char* otpstring;
    char* tprogname;
    if (boot_script) 
	error("Conflicting -start_erl and -boot options");
    if (config_script)
	error("Conflicting -start_erl and -config options");
    reldir = getenv("RELDIR");
    if (reldir)
	reldir = strsave(reldir);
    else {
	sprintf(tmpbuffer, "%s/releases", rootdir);
	reldir = strsave(tmpbuffer);
    }
    if (file == NULL)
       sprintf(start_erl_data, "%s/start_erl.data", reldir);
    else
       sprintf(start_erl_data, "%s", file);
    fp = _open(start_erl_data, _O_RDONLY );
    if( fp == -1 )
	error( "open failed on %s",start_erl_data );
    else {
	if( ( bytesread = _read( fp, tmpbuffer, 512 ) ) <= 0 )
	    error( "Problem reading file %s", start_erl_data );
	else {
	    tmpbuffer[bytesread]='\0';
	    if ((otpstring = strchr(tmpbuffer,' ')) != NULL) {
		*otpstring = '\0';
		otpstring++;
		
/*
 *   otpstring is the otpversion
 *   tmpbuffer is the emuversion
*/
	    }
	}
    }
    tprogname = otpstring;
    while (*tprogname) {
	if (*tprogname <= ' ') {
	    *tprogname='\0';
	    break;
	}
	tprogname++;
    }
	
    bindir = emalloc(512);
    sprintf(bindir,"%s/erts-%s/bin",rootdir,tmpbuffer);
    /* BINDIR=$ROOTDIR/erts-$ERTS_VSN/bin */
    tprogname = progname;
    progname = emalloc(strlen(tprogname) + 20);
    sprintf(progname,"%s -start_erl",tprogname);

    boot_script = emalloc(512);
    config_script = emalloc(512);
    sprintf(boot_script, "%s/%s/start", reldir, otpstring);
    sprintf(config_script, "%s/%s/sys", reldir, otpstring);
       
}


static char *replace_filename(char *path, char *new_base) 
{
    int plen = strlen(path);
    char *res = malloc((plen+strlen(new_base)+1)*sizeof(char));
    char *p;

    strcpy(res,path);
    for (p = res+plen-1 ;p >= res && *p != '\\'; --p)
        ;
    *(p+1) ='\0';
    strcat(res,new_base);
    return res;
}

static char *path_massage(char *long_path)
{
     char *p;

     p = malloc(MAX_PATH+1);
     strcpy(p, long_path);
     GetShortPathName(p, p, MAX_PATH);
     return p;
}
    
static char *do_lookup_in_section(InitSection *inis, char *name, 
				  char *section, char *filename, int is_path)
{
    char *p = lookup_init_entry(inis, name);

    if (p == NULL) {
	error("Could not find key %s in section %s of file %s",
	      name,section,filename);
    }

    if (is_path) {
	return path_massage(p);
    } else {
	return strsave(p);
    }
}


static void get_parameters(int argc, char** argv)
{
    char buffer[MAX_PATH];
    char *ini_filename;
    HANDLE module = GetModuleHandle(NULL); /* This might look strange, but we want the erl.ini 
					      that resides in the same dir as erl.exe, not 
					      an erl.ini in our directory */
    InitFile *inif;
    InitSection *inis;

    if (module == NULL) {
        error("Cannot GetModuleHandle()");
    }

    if (GetModuleFileName(module,buffer,MAX_PATH) == 0) {
        error("Could not GetModuleFileName");
    }

    ini_filename = replace_filename(buffer,INI_FILENAME);

    if ((inif = load_init_file(ini_filename)) == NULL) {
	error("Could not load init file %s",ini_filename);
    }

    if ((inis = lookup_init_section(inif,INI_SECTION)) == NULL) {
	error("Could not find section %s in init file %s",
	      INI_SECTION, ini_filename);
    }

    progname = do_lookup_in_section(inis, "Progname", INI_SECTION, 
				    ini_filename,0);
    bindir = do_lookup_in_section(inis, "Bindir", INI_SECTION, ini_filename,1);
    rootdir = do_lookup_in_section(inis, "Rootdir", INI_SECTION, 
				   ini_filename,1);
    emu = argv[0];

    free_init_file(inif);
    free(ini_filename);
}

static void
get_home(void)
{
    int len;
    char tmpstr[MAX_PATH+1];
    char* homedrive;
    char* homepath;

    homedrive = getenv("HOMEDRIVE");
    homepath = getenv("HOMEPATH");
    if (!homedrive || !homepath) {
	if (len = GetWindowsDirectory(tmpstr,MAX_PATH)) {
	    home = emalloc(len+1);
	    strcpy(home,tmpstr);
	} else
	    error("HOMEDRIVE or HOMEPATH is not set and GetWindowsDir failed");
    } else {
	home = emalloc(strlen(homedrive)+strlen(homepath)+1);
	strcpy(home, homedrive);
	strcat(home, homepath);
    }
}

#else

static void
get_parameters(int argc, char** argv)
{
    progname = getenv("PROGNAME");
    bindir = getenv("BINDIR");
    rootdir = getenv("ROOTDIR");
    emu = getenv("EMU");
    if (!progname || !bindir || !rootdir || !emu ) {
	error("BINDIR, ROOTDIR, EMU and PROGNAME  must be set");
    }
}

static void
get_home(void)
{
    home = getenv("HOME");
    if (home == NULL)
	error("HOME must be set");
}

#endif


static char **build_args_from_env(char *env_var)
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


    if (!(p = getenv(env_var)))
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
    argv[argc] = NULL; /* Sure to be large enough */
    if (!argc) {
	efree(argv);
	return NULL;
    }
    return argv;
#undef ENSURE
}
		
void		
mergeargs(int *argc, char ***argv, char **addargs)
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

#ifdef __WIN32__
static char*
possibly_quote(char* arg)
{
    int mustQuote = NO;
    int n = 0;
    char* s;
    char* narg;

    /*
     * Scan the string to find out if it needs quoting and return
     * the original argument if not.
     */

    for (s = arg; *s; s++, n++) {
	if (*s == ' ' || *s == '"') {
	    mustQuote = YES;
	    n++;
	}
    }
    if (!mustQuote) {
	return arg;
    }

    /*
     * Insert the quotes and put a backslash in front of every quote
     * inside the string.
     */

    s = narg = emalloc(n+2+1);
    for (*s++ = '"'; *arg; arg++, s++) {
	if (*s == '"') {
	    *s++ = '\\';
	}
	*s = *arg;
    }
    *s++ = '"';
    *s = '\0';
    return narg;
}

#endif





