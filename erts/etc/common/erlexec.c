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
#include "driver.h"
#include <stdlib.h>

#ifdef __WIN32__
#  include "erl_version.h"
#  include <winreg.h>
#endif

#define NO 0
#define YES 1

#ifdef __WIN32__
#define REGISTRY_PATH "Software\\Ericsson\\Erlang\\"
#define DIRSEP "\\"
#define PATHSEP ";"
#define NULL_DEVICE "nul"
#define BINARY_EXT ".exe"
#define QUOTE(s) possibly_quote(s)
#else
#define PATHSEP ":"
#define DIRSEP "/"
#define NULL_DEVICE "/dev/null"
#define BINARY_EXT ""
#define QUOTE(s) s
#endif

/*
 * Define sleep(seconds) in terms of Sleep() on Windows.
 */

#ifdef __WIN32__
#define sleep(seconds) Sleep(seconds*1000)
#endif


#if defined(__STDC__) || defined(_MSC_VER)
#define var_start(x, y) va_start(x, y)
#define USE_STDARG
#else
#define var_start(x, y) va_start(x)
#endif

#define INSTR_SUFFIX	".instr"

void usage(const char *switchname);
void start_epmd(char *epmd);
void error(char* format, ...);

/*
 * Local functions.
 */

static void mergeargs(int *argc, char ***argv, char **addargs);
static char **build_args_from_env(void);
static void get_parameters(void);
static void add_arg(char *new_arg);
static void add_args(char *first_arg, ...);
static void add_Eargs(char *new_arg);
static int dirq(const char *dirpath);
static void *emalloc(size_t size);
static void *erealloc(void *p, size_t size);
static void efree(void *p);
static char* strsave(char* string);
static FUNCTION(void, get_home, ());
#ifdef __WIN32__
static void get_start_erl_data(char *);
static char* get_value(HKEY key, char* value_name, BOOL mustExit);
static char* possibly_quote(char* arg);
/* 
 * Functions from win_erlexec.c
*/
#ifdef WIN32_WERL
int
start_win_emulator(char* emu, char** argv, int start_detached);
#else
int
start_emulator(char* emu, char** argv, int start_detached);
#endif
#endif



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
				 * started detached (in the background).
				 */
static int instrumented = 0;	/* If non-zero, start jam47.instr.exe (or
				 * whatever) instead of jam47.exe */

#ifdef __WIN32__
static char* key_val_name = ERLANG_VERSION; /* Used by the registry
					   * access functions.
					   */
static char* boot_script = NULL; /* used by option -start_erl and -boot */
static char* config_script = NULL; /* used by option -start_erl and -config */
#endif

/*
 * Needed parameters to be fetched from the environment (Unix)
 * or the registry (Win32).
 */

static char* bindir;		/* Location of executables. */
static char* rootdir;		/* Root location of Erlang installation. */
static char* emu;		/* Emulator to run. */
static char* progname;		/* Name of this program. */
static char* home;		/* Path of user's home directory. */

/*
   Add the instrumentation suffix to the program name, except on Windows,
   where we insert it just before ".EXE".
*/
static char *add_instrument_suffix(char *prog)
{
   char *p;
   int len;
#ifdef __WIN32__
   char *exe_p;
#endif

   len = strlen(prog);

   p = emalloc(len + strlen(INSTR_SUFFIX) + 1);
   strcpy(p, prog);

#ifdef __WIN32__
   exe_p = p + len - 4;
   if (exe_p >= p)
   {
      if (exe_p[0] == '.' &&
	  (exe_p[1] == 'e' || exe_p[1] == 'E') &&
	  (exe_p[2] == 'x' || exe_p[2] == 'X') &&
	  (exe_p[3] == 'e' || exe_p[3] == 'E'))
      {
	 sprintf(exe_p, "%s.exe", INSTR_SUFFIX);
	 return p;
      }
   }
#endif

   strcat(p, INSTR_SUFFIX);
   return p;
}

#ifdef WIN32_WERL
int
WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
		    PSTR szCmdLine, int iCmdShow)
{
    int argc = __argc;
    char **argv = __argv;
#else
int
main(int argc, char **argv)
{
#endif
    int haltAfterwards = 0;	/* If true, put 's erlang halt' at the end
				 * of the arguments. */
    int isdistributed = 0;
    int no_epmd = 0;
    int i;
    char* s;
    char *epmd_prog = NULL;

#ifdef __WIN32__

    /* if we started this erl just to get a detached emulator, 
     * the arguments are already prepared for beam, so we skip
     * directly to start_emulator */
    s = getenv("ERL_CONSOLE_MODE");
    if (s != NULL && strcmp(s, "detached")==0) {
	memcpy(Eargsp, argv, argc * sizeof(argv[1]));
	goto skip_arg_massage;
    }   
#endif
    mergeargs(&argc, &argv, build_args_from_env());

    i = 1;
#ifdef __WIN32__
    if ((argc > 2) && (strcmp(argv[i], "-regkey") == 0)) {
	key_val_name = strsave(argv[i+1]);
	i = 3;
    }
#endif		

    
    get_parameters();
    
    /*
     * Construct the path of the executable.
     */

    /* We need to do this before the ordinary processing. */
    while (i < argc) {
       if (strcmp(argv[i], "-instr") == 0)
	  instrumented = 1;
       i++;
    }
#ifdef __WIN32__
    emu = argv[0];		/* we relaunch ourserlves when -detached given */
#else
    if (instrumented) {
	emu = add_instrument_suffix(emu);
    }
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

    while (i < argc) {
	switch (argv[i][0]) {
	case '-':
	    switch (argv[i][1]) {
#if defined(__WIN32__) && defined(DEBUG)
	    case 'b':
		if (strcmp(argv[i], "-bc") == 0) {
		    static char bc[] = "bcpro.exe";
		    int i;

		    /*
		     * Run the emulator under control of BoundsChecker, by
		     * moving up all arguments and putting into arg0.
		     * Note that we find the path to bcpro, because the code
		     * which starts the emulator uses execv/spawnv, which don't
		     * search the path.
		     */

		    for (i = EargsCnt; i > 0; i--)
			Eargsp[i] = Eargsp[i-1];
		    EargsCnt++;
		    _searchenv(bc, "PATH", tmpStr);
		    if (tmpStr[0] == '\0')
			error("Can't find %s in PATH", bc);
		    Eargsp[0] = emu = strsave(tmpStr);
		}
		else if (strcmp(argv[i], "-boot") == 0) {
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
		if (strcmp(argv[i], "-instr") == 0)
		   ;		/* already handled above */
		else
		   add_arg(argv[i]);
		break;

	    case 'e':
		if (strcmp(argv[i], "-emu_args") == 0) { /* -emu_args */
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
		    add_args("-noshell", "-noinput", "-s", "make", "all", NULL);
		    add_Eargs("-B");
		    haltAfterwards = 1;
		    i = argc;	/* Skip rest of command line */
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
		} else if (strcmp(argv[i], "-noshell") == 0) {
		    add_args("-noshell", "-noinp_shell", NULL);
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

	    case 's':		/* -sname NAME */
		if (strcmp(argv[i], "-sname") == 0) {
		    if (i+1 >= argc)
			usage("-sname");
		    add_arg(argv[i]);
		    add_arg(argv[i+1]);
		    isdistributed = 1;
		    i++;
		}
#ifdef __WIN32__
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
	
	    case 'v':		/* -version */
		if (strcmp(argv[i], "-version") == 0)
		    add_Eargs("-V");
		else
		    add_arg(argv[i]);
		break;

	    case 'x':
		if (argv[i][2] != '\0') {
		    add_arg(argv[i]);
		} else {
#ifdef __WIN32__
		    error("'-x' supported on Unix only.");
#else
		    sprintf(tmpStr, "%s/lib/xerl", rootdir);
		    if (dirq(tmpStr)) {
			sprintf(tmpStr, "%s/lib/pxw", rootdir);
			if (dirq(tmpStr)) {
			    add_args("-x", "-noshell", NULL);
			    break;
			}
		    }
		    error("You need the 'xerl' and 'pxw' bundles to run 'erl -x'");
#endif
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
	    add_arg(argv[i]);
	} /* switch(argv[i][0] */
	i++;
    }

    get_home();
    add_args("-home", home, NULL);
#ifdef __WIN32__
    if (boot_script)
	add_args("-boot", boot_script, NULL);
    if (config_script)
	add_args("-config", config_script, NULL);
#endif
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
    for (i = 0; i < argsCnt; i++)
	Eargsp[EargsCnt++] = argsp[i];
    Eargsp[EargsCnt] = NULL;
    
    if (verbose) {
	printf("Executing: %s", emu);
	for (i = 0; i < EargsCnt; i++)
	    printf(" %s", Eargsp[i]);
	printf("\n\n");
    }

    
#ifdef __WIN32__

skip_arg_massage: ;
#ifdef WIN32_WERL
    return start_win_emulator(emu, Eargsp, start_detached);
#else
    return start_emulator(emu, Eargsp, start_detached);
#endif

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
	  "[-make] [-man [manopts] MANPAGE] [-x] [-emu_args] [+i BOOT_MODULE] "
	  "[+b BOOT_FUN] [+s STACK_SIZE] [+h HEAP_SIZE] [+# ITEMS] "
	  "[+P MAX_PROCS] [args ...]\n");
  exit(1);
}

static int
dirq(const char *dirpath)
{
    struct stat statbuf;

    if (stat(dirpath, &statbuf) == -1)
	return 0;

    return (statbuf.st_mode & S_IFDIR);
}

void
start_epmd(char *epmd)
{
    char  epmd_cmd[MAXPATHLEN+100];
    int   result;

    if (!epmd) {
#ifdef __WIN32__
	sprintf(epmd_cmd, "%s" DIRSEP "epmd", bindir);
	result = spawnlp(_P_DETACH, epmd_cmd, epmd_cmd, "-daemon", NULL);
#else
	sprintf(epmd_cmd, "%s" DIRSEP "epmd -daemon", bindir);
	result = system(epmd_cmd);
#endif
    } else {
#ifdef __WIN32__
	result = spawnlp(_P_DETACH, epmd, epmd, NULL);
#else
	result = system(epmd);
#endif
    }	
    if (result == -1) {
      fprintf(stderr, "Error spawning %s (error %d)\n", epmd_cmd,errno);
      exit(1);
    }
}

static void
add_arg(char *new_arg)
{
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
add_Eargs(char *new_arg)
{
    Eargsp[EargsCnt++] = QUOTE(new_arg);
}

#if !defined(__WIN32__)

#if defined(USE_STDARG)
void error(char* format, ...)
#else
void error(format, va_alist)
     char* format;
     va_dcl
#endif
{
    char sbuf[1024];
    va_list ap;

    var_start(ap, format);
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

static HKEY
open_key(char* key_name)
{
    HKEY key;
    char actual_key[512];
    
    sprintf(actual_key, REGISTRY_PATH "%s", key_name);
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, actual_key, 0, KEY_READ, &key)
	!= ERROR_SUCCESS)
	error("No %s key found in registry", actual_key);
    
    return key;
}

static char*
get_value(HKEY key, char* value_name, BOOL mustExist)
{
    DWORD size;			/* Size required for value. */

    if (RegQueryValueEx(key, value_name, NULL, NULL, NULL, &size)
	!= ERROR_SUCCESS) {
	if (mustExist) {
	    error("No value provided for %s%s in registry",
		  REGISTRY_PATH, value_name);
	}
    } else {
	char* value = emalloc(size);
	value[0] = '\0';
	RegQueryValueEx(key, value_name, NULL, NULL, value, &size);
	if (verbose) {
	    printf("%s%s = %s\n", REGISTRY_PATH, value_name, value);
	}
	return value;
    }
    return NULL;
}

static void
get_parameters(void)
{
    HKEY key;			/* Handle to open key. */
    char* tcl_library;
    char* tk_library;
    char* wish;
    char sbuf[2048];

    key = open_key(key_val_name);
    progname = get_value(key, "Progname", TRUE);
    bindir = get_value(key, "Bindir", TRUE);
    rootdir = get_value(key, "Rootdir", TRUE);
    emu = get_value(key, "Emulator", TRUE);
    /*
     * Get values for GS and store in environment.
     */

    tcl_library = get_value(key, "TclLibrary", FALSE);
    tk_library = get_value(key, "TkLibrary", FALSE);
    wish = get_value(key, "Wish", FALSE);
    if (tcl_library != NULL && tk_library != NULL) {
	sprintf(sbuf, "TCL_LIBRARY=%s", tcl_library);
	putenv(strsave(sbuf));
	sprintf(sbuf, "TK_LIBRARY=%s", tk_library);
	putenv(strsave(sbuf));
    }
    if (wish != NULL) {
	sprintf(sbuf, "_GS_WISH_EXECUTABLE_=%s", wish);
	putenv(strsave(sbuf));
    }

    RegCloseKey(key);
}

static void
get_home()
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
get_parameters()
{
  progname = getenv("PROGNAME");
  bindir = getenv("BINDIR");
  rootdir = getenv("ROOTDIR");
  emu = getenv("EMU");
  if (!progname || !bindir || !rootdir || !emu )
    error("BINDIR, ROOTDIR, EMU and PROGNAME  must be set");
}

static void
get_home()
{
  home = getenv("HOME");
  if (home == NULL)
    error("HOME must be set");
}


#endif


static char **build_args_from_env(void)
{
    int argc = 0;
    char **argv = NULL;
    int alloced = 0;
    char **cur_s;
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


    if (!(p = getenv("ERL_FLAGS")))
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





