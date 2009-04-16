/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2007-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 */
/*
 * Purpose: escript front-end.
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#ifdef __WIN32__
#include <winbase.h>
#endif

#include <ctype.h>

#define NO 0
#define YES 1

#define ASIZE(a) (sizeof(a)/sizeof(a[0]))

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
#define UNSHIFT3(s, t, u) UNSHIFT(u); UNSHIFT(t); UNSHIFT(s)
#define PUSH(s) eargv[eargc++] = QUOTE(s)
#define PUSH2(s, t) PUSH(s); PUSH(t)
#define PUSH3(s, t, u) PUSH2(s, t); PUSH(u)
#define LINEBUFSZ 1024

/*
 * Local functions.
 */

static void error(char* format, ...);
static char* emalloc(size_t size);
static void efree(void *p);
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
#endif /* !SYS_ERRLIST_DECLARED */

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

static void
set_env(char *key, char *value)
{
#ifdef __WIN32__
    if (!SetEnvironmentVariable((LPCTSTR) key, (LPCTSTR) value))
	error("SetEnvironmentVariable(\"%s\", \"%s\") failed!", key, value);
#else
    size_t size = strlen(key) + 1 + strlen(value) + 1;
    char *str = emalloc(size);
    sprintf(str, "%s=%s", key, value);
    if (putenv(str) != 0)
	error("putenv(\"%s\") failed!", str);
#ifdef HAVE_COPYING_PUTENV
    efree(str);
#endif
#endif
}

static char *
get_env(char *key)
{
#ifdef __WIN32__
    DWORD size = 32;
    char *value = NULL;
    while (1) {
	DWORD nsz;
	if (value)
	    efree(value);
	value = emalloc(size);
	SetLastError(0);
	nsz = GetEnvironmentVariable((LPCTSTR) key, (LPTSTR) value, size);
	if (nsz == 0 && GetLastError() == ERROR_ENVVAR_NOT_FOUND) {
	    efree(value);
	    return NULL;
	}
	if (nsz <= size)
	    return value;
	size = nsz;
    }
#else
    return getenv(key);
#endif
}

static void
free_env_val(char *value)
{
#ifdef __WIN32__
    if (value)
	efree(value);
#endif
}

static void
append_shebang_args_to_erl_flags(char* argprefix, int argc, char** argv)
{
    if (argc > 1) {
	/* Open script file */
	FILE* fd = fopen (argv[1],"r");

	if (fd != NULL)	{
	    /* Read first line in script file */
	    char linebuf[LINEBUFSZ];
	    char* ptr = fgets(linebuf, LINEBUFSZ, fd);

	    if (ptr != NULL && linebuf[0] == '#' && linebuf[1] == '!') {
		/* Try to find args on second or third line */
		ptr = fgets(linebuf, LINEBUFSZ, fd);
		if (ptr != NULL && linebuf[0] == '%' && linebuf[1] == '%' && linebuf[2] == '!') {
		    /* Use second line */
		} else {
		    /* Try third line */
		    ptr = fgets(linebuf, LINEBUFSZ, fd);
		    if (ptr != NULL && linebuf[0] == '%' && linebuf[1] == '%' && linebuf[2] == '!') {
			/* Use third line */
		    } else {
			/* Do not use any line */
			ptr = NULL;
		    }
		}
	  
		if (ptr != NULL) {
		    /* Use entire line but the leading chars */
		    char* emulator_flags = linebuf;
		    emulator_flags++;
		    emulator_flags++;
	    
		    /* Get rid of trailing newline */
		    {
			char* linenl = strstr(emulator_flags, "\n");
			if (linenl) {
			    linenl[0] = (char)NULL;
			}
		    }
	    
		    /* Skip leading spaces */
		    while (emulator_flags && emulator_flags[0] == ' ') {
			emulator_flags++;
		    }
	    
		    if (emulator_flags) {
			/* Read old env setting */
			char* varname = "ERL_FLAGS";
			char* oldenv = getenv(varname);
			if (oldenv) {
			    /* Copy flags from script */
			    char* newenv = emalloc(strlen(oldenv)+LINEBUFSZ);
			    char* tmpenv = newenv;
			    strcpy(tmpenv, emulator_flags);
			    tmpenv += strlen(emulator_flags);
			    /* Append flags from old env setting */
			    strcpy(tmpenv, " ");
			    tmpenv++;
			    strcpy(tmpenv, oldenv);
			    set_env(varname, newenv);
			    efree(newenv);
			} else {
			    /* Set new env */
			    set_env(varname, emulator_flags);
			}
		    }
		} 
	    }
	    fclose(fd);
	} else {
	    error("Failed to open file: %s", argv[1]);
	}
    } else {
	error("Missing filename\n");
    }
}

int
main(int argc, char** argv)
{
    int eargv_size;
    int eargc_base;		/* How many arguments in the base of eargv. */
    char* emulator;
    char* env;
    char* invoker = argv[0];

    emulator = env = get_env("ESCRIPT_EMULATOR");
    if (emulator == NULL) {
	emulator = get_default_emulator(argv[0]);
    }

    /*
     * Allocate the argv vector to be used for arguments to Erlang.
     * Arrange for starting to pushing information in the middle of
     * the array, to allow easy addition of commands in the beginning.
     */

    eargv_size = argc*4+100;
    eargv_base = (char **) emalloc(eargv_size*sizeof(char*));
    eargv = eargv_base;
    eargc = 0;
    push_words(emulator);
    eargc_base = eargc;
    eargv = eargv + eargv_size/2;
    eargc = 0;

    free_env_val(env);

    /*
     * Push initial arguments.
     */

    PUSH("+B");
    PUSH2("-boot", "start_clean");
    PUSH("-noshell");
    PUSH3("-run", "escript", "start");

    /*
     * Push all options (without the hyphen) before the script name.
     */

    while (argc > 1 && argv[1][0] == '-') {
	PUSH(argv[1]+1);
      	argc--, argv++;
    }

    /*
     * Read options from the first row in the script and append them
     * to ERL_FLAGS
     */

    append_shebang_args_to_erl_flags(invoker, argc, argv);

    /*
     * Push the script name and everything following it as extra arguments.
     */

    PUSH("-extra");
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

static void
push_words(char* src)
{
    char sbuf[1024];
    char* dst;

    dst = sbuf;
    while ((*dst++ = *src++) != '\0') {
	if (isspace((int)*src)) {
	    *dst = '\0';
	    PUSH(strsave(sbuf));
	    dst = sbuf;
	    do {
		src++;
	    } while (isspace((int)*src));
	}
    }
    if (sbuf[0])
	PUSH(strsave(sbuf));
}
#ifdef __WIN32__
char *make_commandline(char **argv)
{
    static char *buff = NULL;
    static int siz = 0;
    int num = 0;
    char **arg, *p;

    if (*argv == NULL) { 
	return "";
    }
    for (arg = argv; *arg != NULL; ++arg) {
	num += strlen(*arg)+1;
    }
    if (!siz) {
	siz = num;
	buff = emalloc(siz*sizeof(char));
    } else if (siz < num) {
	siz = num;
	buff = realloc(buff,siz*sizeof(char));
    }
    p = buff;
    for (arg = argv; *arg != NULL; ++arg) {
	strcpy(p,*arg);
	p+=strlen(*arg);
	*p++=' ';
    }
    *(--p) = '\0';

    if (debug) {
	printf("Processed commandline:%s\n",buff);
    }
    return buff;
}

int my_spawnvp(char **argv)
{
    STARTUPINFO siStartInfo;
    PROCESS_INFORMATION piProcInfo;
    DWORD ec;

    memset(&siStartInfo,0,sizeof(STARTUPINFO));
    siStartInfo.cb = sizeof(STARTUPINFO); 
    siStartInfo.dwFlags = STARTF_USESTDHANDLES;
    siStartInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    siStartInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    siStartInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);

    if (!CreateProcess(NULL, 
		       make_commandline(argv),
		       NULL, 
		       NULL, 
		       TRUE, 
		       0,
		       NULL, 
		       NULL, 
		       &siStartInfo, 
		       &piProcInfo)) {
	return -1;
    }
    CloseHandle(piProcInfo.hThread);

    WaitForSingleObject(piProcInfo.hProcess,INFINITE);
    if (!GetExitCodeProcess(piProcInfo.hProcess,&ec)) {
	return 0;
    }
    return (int) ec;
}    
#endif /* __WIN32__ */


static int
run_erlang(char* progname, char** argv)
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

    status = my_spawnvp(argv)/*_spawnvp(_P_WAIT,progname,argv)*/;
    if (status == -1) {
	fprintf(stderr, "escript: Error executing '%s': %d", progname, 
		GetLastError());
    }
    return status;
#else
    execvp(progname, argv);
    error("Error %d executing \'%s\'.", errno, progname);
    return 2;
#endif
}

static void
error(char* format, ...)
{
    char sbuf[1024];
    va_list ap;
    
    va_start(ap, format);
    vsprintf(sbuf, format, ap);
    va_end(ap);
    fprintf(stderr, "escript: %s\n", sbuf);
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

static void
efree(void *p) 
{
    free(p);
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
#ifdef __WIN32__
	    if (_access(sbuf, 0) != -1) {
		return strsave(sbuf);
	    }
#else
	    if (access(sbuf, 1) != -1) {
		return strsave(sbuf);
	    }
#endif
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
	switch(*s) {
	case ' ':
	    mustQuote = YES;
	    continue;
	case '"':
	    mustQuote = YES;
	    n++;
	    continue;
	case '\\':
	    if(s[1] == '"')
		n++;
	    continue;
	default:
	    continue;
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
	if (*arg == '"' || (*arg == '\\' && arg[1] == '"')) {
	    *s++ = '\\';
	}
	*s = *arg;
    }
    if (s[-1] == '\\') {
	*s++ ='\\';
    }
    *s++ = '"';
    *s = '\0';
    return narg;
}
#endif /* __WIN32__ */
