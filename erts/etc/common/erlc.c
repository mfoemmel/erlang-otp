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
 * Purpose: Common compiler front-end.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#ifdef __WIN32__
#include <winbase.h>
/* FIXE ME config_win32.h? */
#define HAVE_STRERROR 1
#endif

#include <ctype.h>

#define NO 0
#define YES 1

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

#if defined(__STDC__) || defined(_MSC_VER)
#  define var_start(x, y) va_start(x, y)
#  define USE_STDARG
#else
#  define var_start(x, y) va_start(x)
#endif

static int debug = 0;		/* Bit flags for debug printouts. */

static char** eargv_base;	/* Base of vector. */
static char** eargv;		/* First argument for erl. */

static int eargc;		/* Number of arguments in eargv. */

#ifdef __WIN32__
#  define QUOTE(s) possibly_quote(s)
#  define IS_DIRSEP(c) ((c) == '/' || (c) == '\\')
#  define ERL_NAME "erl.exe"
#else
#  define QUOTE(s) s
#  define IS_DIRSEP(c) ((c) == '/')
#  define ERL_NAME "erl"
#endif

#define UNSHIFT(s) eargc++, eargv--; eargv[0] = QUOTE(s)
#define PUSH(s) eargv[eargc++] = QUOTE(s)
#define PUSH2(s, t) PUSH(s); PUSH(t)
#define PUSH3(s, t, u) PUSH2(s, t); PUSH(u)

static char* output_type = NULL; /* Type of output file. */
#ifdef __WIN32__
static int pause_after_execution = 0;
#endif

/*
 * Local functions.
 */

static char* process_opt(int* pArgc, char*** pArgv, int offset);
static void error(char* format, ...);
static void usage(void);
static char* emalloc(size_t size);
static char* strsave(char* string);
static void push_words(char* src);
static int run_erlang(char* name, char** argv);
static char* get_default_emulator(char* progname);
#ifdef __WIN32__
static char* possibly_quote(char* arg);
#endif

/*
 * Supply a strerror() function if libc doesn't.
 */
#ifndef HAVE_STRERROR
extern int sys_nerr;
#ifndef SYS_ERRLIST_DECLARED
extern const char * const sys_errlist[];
#endif

char *strerror(int errnum)
{
  static char *emsg[1024];

  if (errnum != 0) {
    if (errnum > 0 && errnum < sys_nerr) 
      sprintf((char *) &emsg[0], "(%s)", sys_errlist[errnum]);
    else 
      sprintf((char *) &emsg[0], "errnum = %d ", errnum);
  }
  else {
    emsg[0] = '\0';
  }
  return (char *) &emsg[0];
}
#endif /* !HAVE_STRERROR */

int
main(argc, argv)
int argc;
char** argv;
{
    char cwd[MAXPATHLEN];	/* Current working directory. */
    char** rpc_eargv;		/* Pointer to the beginning of arguments
				 * if calling a running Erlang system
				 * via erl_rpc().
				 */
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;

    emulator = getenv("ERLC_EMULATOR");
    if (emulator == NULL) {
	emulator = get_default_emulator(argv[0]);
    }

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy adding of emulator options (like -pa)
     * before '-s erlcompile compile_cmdline...'.
     *
     * Oh, by the way, we will push the compiler command in the
     * base of the eargv vector, and move it up later.
     */

    eargv_size = argc*4+100;
    eargv_base = (char **) emalloc(eargv_size*sizeof(char*));
    eargv = eargv_base;
    eargc = 0;
    push_words(emulator);
    eargc_base = eargc;
    eargv = eargv + eargv_size/2;
    eargc = 0;

    /*
     * Push initial arguments.
     */

    PUSH("-noinput");
    PUSH3("-s", "erl_compile", "compile_cmdline");
    rpc_eargv = eargv+eargc;

    /*
     * Push standard arguments to Erlang.
     */

    if (getcwd(cwd, sizeof(cwd)) == NULL)
	error("Failed to get current working directory: %s", strerror(errno));
#ifdef __WIN32__
    (void) GetShortPathName(cwd, cwd, sizeof(cwd));
#endif    
    PUSH2("@cwd", cwd);

    /*
     * Parse all command line switches.
     */

    while (argc > 1 && (argv[1][0] == '-' || argv[1][0] == '+')) {

	/*
	 * Options starting with '+' are passed on to Erlang.
	 */

	if (argv[1][0] == '+') {
	    PUSH2("@option", argv[1]+1);
	} else {
	    /*
	     * Interpret options starting with '-'.
	     */
	    
	    switch (argv[1][1]) {
	    case 'b':
		output_type = process_opt(&argc, &argv, 0);
		PUSH2("@output_type", output_type);
		break;
	    case 'c':		/* Allowed for compatibility with 'erl'. */
		if (strcmp(argv[1], "-compile") != 0)
		    goto error;
		break;
	    case 'd':
		debug = 1;
		break;
	    case 'D':
		{
		    char* def = process_opt(&argc, &argv, 0);
		    char* equals;
		    
		    if ((equals = strchr(def, '=')) == NULL) {
			PUSH2("@d", def);
		    } else {
			*equals = '\0';
			equals++;
			PUSH3("@dv", def, equals);
		    }
		}
		break;
	    case 'h':
		usage();
		break;
	    case 'i':
		if (strcmp(argv[1], "-ilroot") != 0)
		    goto error;
		argv[1] = "-i";
		PUSH2("@ilroot", process_opt(&argc, &argv, 0));
		break;
	    case 'I':
		PUSH2("@i", process_opt(&argc, &argv, 0));
		break;
	    case 'o':
		PUSH2("@outdir", process_opt(&argc, &argv, 0));
		break;
	    case 'O':
		PUSH("@optimize");
		if (argv[1][2] == '\0')
		    PUSH("1");
		else
		    PUSH(argv[1]+2);
		break;
	    case 'p':
		{
		    int c = argv[1][2];
		    
		    if (c != 'a' && c != 'z') {
			goto error;
#ifdef __WIN32__
		    } else if (strcmp(argv[1], "-pause") == 0) {
			pause_after_execution = 1;
#endif
		    } else {
			char option[4];

			UNSHIFT(process_opt(&argc, &argv, 1));
			option[0] = '-';
			option[1] = 'p';
			option[2] = c;
			option[3] = '\0';
			UNSHIFT(strsave(option));
		    }
		}
		break;
	    case 'r':
		if (strcmp(argv[1], "-rpc") != 0)
		    goto error;
		break;
	    case 's':
		if (argv[1][2] == '\0') {
		    ;
		} else if (strcmp(argv[1], "-stop") == 0) {
		    /*
		     * Not exiting here allows us to write:
		     *    erlc -stop jam file.erl
		     * to restart the compiler node before compiling.
		     */
		} else
		    goto error;
		break;
	    case 'v':		/* Verbose. */
		PUSH2("@verbose", "true");
		break;
	    case 'V':
		/** XXX Version perhaps, but of what? **/
		break;
	    case 'W':		/* Enable warnings. */
		PUSH2("@warn", "1");
		break;
	    case 'E':
	    case 'S':
	    case 'P':
		{
		    char* buf;

		    /* 
		     * From the given upper-case letter, construct
		     * a quoted atom.  This is a convenience for the
		     * Erlang compiler, to avoid fighting with the shell's
		     * quoting.
		     */

		    buf = emalloc(4);
		    buf[0] = '\'';
		    buf[1] = argv[1][1];
		    buf[2] = '\'';
		    buf[3] = '\0';

		    PUSH2("@option", buf);
		}
		break;

	    case '-':
		goto no_more_options;

	    default:
	    error:
		usage();
		break;
	    }
	}
	argc--, argv++;
    }

 no_more_options:

    if (argc <= 1) {
	/*
	 * To avoid starting an Erlang system unless absolutely needed
	 * exit if no files were specified on the command line.
	 */
	exit(0);
    }

    /*
     * The rest of the command line must be filenames.  Simply push them.
     */

    PUSH("@files");
    while (argc > 1) {
	PUSH(argv[1]);
	argc--, argv++;
    }

    /*
     * Move up the commands for invoking the emulator and adjust eargv
     * accordingly.
     */

    while (--eargc_base >= 0) {
	UNSHIFT(eargv_base[eargc_base]);
    }
    
    /*
     * Invoke Erlang with the collected options.
     */

    PUSH(NULL);
    return run_erlang(eargv[0], eargv);
}

static char*
process_opt(pArgc, pArgv, offset)
int* pArgc;
char*** pArgv;
int offset;
{
    int argc = *pArgc;
    char** argv = *pArgv;
    int c = argv[1][1];
    
    if (argv[1][2+offset] != '\0') {
	/*
	 * The option was given as -x<value>.
	 */
	return argv[1]+2+offset;
    }

    /*
     * Look at the next argument.
     */

    argc--, argv++;
    if (argc < 2 || argv[1][0] == '-')
	error("No value given to -%c option", c);
    *pArgc = argc;
    *pArgv = argv;
    return argv[1];
}

static void
push_words(src)
char* src;
{
    char sbuf[1024];
    char* dst;

    dst = sbuf;
    while ((*dst++ = *src++) != '\0') {
	if (isspace(*src)) {
	    *dst = '\0';
	    PUSH(strsave(sbuf));
	    dst = sbuf;
	    do {
		src++;
	    } while (isspace(*src));
	}
    }
    if (sbuf[0])
	PUSH(strsave(sbuf));
}

static int
run_erlang(progname, argv)
char* progname;
char** argv;
{
#ifdef __WIN32__
    int status;
#endif

    if (debug) {
	int i = 0;
	while (argv[i] != NULL)
	    printf(" %s", argv[i++]);
	printf("\n");
    }

#ifdef __WIN32__
    /*
     * Alas, we must wait here for the program to finish.
     * Otherwise, the shell from which we was executed will think
     * we are finished and print a prompt and read keyboard input.
     */

    status = spawnvp(_P_WAIT, progname, argv);
    if (status == -1) {
	fprintf(stderr, "erlc: Error executing '%s': %d", progname, errno);
    }
    if (pause_after_execution) {
	fprintf(stderr, "Press ENTER to continue . . .\n");
	while (getchar() != '\n')
	    ;
    }
    return status;
#else
    execvp(progname, argv);
    error("Error %d executing \'%s\'.", errno, progname);
    return 2;
#endif
}

static void
usage(void)
{
    static struct {
	char* name;
	char* desc;
    } options[] = {
	{"-b type", "type of output file (e.g. jam or beam)"},
	{"-d", "turn on debugging of erlc itself"},
	{"-Dname", "define name"},
	{"-Dname=value", "define name to have value"},
	{"-help", "shows this help text"},
	{"-ilroot", "root for include library"},
	{"-I path", "where to search for include files"},
	{"-o name", "name output directory or file"},
	{"-pa path", "add path to Erlang's code path"},
	{"-rpc", "use a running Erlang node for compilation"},
	{"-stop", "stop the compiler Erlang node (implies -rpc)"},
	{"-v", "verbose compiler output"},
	{"-W", "enable warnings"},
	{"-E", "generate listing of expanded code (Erlang compiler)"},
	{"-S", "generate assembly listing (Erlang compiler)"},
	{"-P", "generate listing of preprocessed code (Erlang compiler)"},
	{"+term", "pass the Erlang term unchanged to the compiler"},
    };
    int i;

    fprintf(stderr, "Usage:\terlc [options] file.ext ...\n");
    fprintf(stderr, "Options:\n");
    for (i = 0; i < sizeof(options)/sizeof(options[0]); i++) {
	fprintf(stderr, "%-14s %s\n", options[i].name, options[i].desc);
    }
    exit(1);
}

static void
#if defined(USE_STDARG)
error(char* format, ...)
#else
error(format, va_alist)
     char* format;
     va_dcl
#endif
{
    char sbuf[1024];
    va_list ap;
    
    var_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);
    fprintf(stderr, "erlc: %s\n", sbuf);
    exit(1);
}

static char*
emalloc(size_t size)
{
  char *p = malloc(size);
  if (p == NULL)
    error("Insufficient memory");
  return p;
}

static char*
strsave(char* string)
{
  char* p = emalloc(strlen(string)+1);
  strcpy(p, string);
  return p;
}

static char*
get_default_emulator(char* progname)
{
    char sbuf[MAXPATHLEN];
    char* s;

    strcpy(sbuf, progname);
    for (s = sbuf+strlen(sbuf); s >= sbuf; s--) {
	if (IS_DIRSEP(*s)) {
	    strcpy(s+1, ERL_NAME);
	    if (access(sbuf, 1) != -1) {
		return strsave(sbuf);
	    }
	    break;
	}
    }
    return ERL_NAME;
}

#ifdef __WIN32__
static char*
possibly_quote(char* arg)
{
    int mustQuote = NO;
    int n = 0;
    char* s;
    char* narg;

    if (arg == NULL) {
	return arg;
    }

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
