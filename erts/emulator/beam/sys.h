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
#ifndef __SYS_H__
#define __SYS_H__

/* xxxP __VXWORKS__ */

#if defined (__WIN32__)
#  include "erl_win_sys.h"
#elif defined (VXWORKS) 
#  include "erl_vxworks_sys.h"
#else 
#  include "erl_unix_sys.h"
#ifndef UNIX
#  define UNIX 1
#endif
#endif

#ifdef DEBUG
#  define ASSERT(e) \
  if (e) { \
     ; \
  } else { \
     erl_assert_error(#e, __FILE__, __LINE__); \
  }
#  define ASSERT_EXPR(e) \
    ((void) ((e) ? 1 : (erl_assert_error(#e, __FILE__, __LINE__), 0)))
void erl_assert_error(char* expr, char* file, int line);
#else
#  define ASSERT(e)
#  define ASSERT_EXPR(e) ((void) 1)
#endif

/*
 * Microsoft C/C++: We certainly want to use stdarg.h and prototypes.
 * But MSC doesn't define __STDC__, unless we compile with the -Za
 * flag (strict ANSI C, no Microsoft extension).  Compiling with -Za
 * doesn't work: some Microsoft headers fail to compile...
 *
 * Solution: Test if __STDC__ or _MSC_VER is defined.
 *
 * Note: Simply defining __STDC__ doesn't work, as some Microsoft
 * headers will fail to compile!
 */

#if defined(__STDC__) || defined(_MSC_VER)
#  include <stdarg.h>
#  define VA_START(x, y) va_start(x, y)
#else
#  include <varargs.h>
#  define VA_START(x, y) va_start(x)
#endif

#if defined(__STDC__) || defined(_MSC_VER)
#  define EXTERN_FUNCTION(t, f, x)  extern t f x
#  define FUNCTION(t, f, x) t f x
#  define _DOTS_ ...
#  define _VOID_      void
#elif defined(__cplusplus)
#  define EXTERN_FUNCTION(f, x) extern "C" { f x }
#  define FUNCTION(t, f, x) t f x
#  define _DOTS_ ...
#  define _VOID_    void
#else
#  define EXTERN_FUNCTION(t, f, x) extern t f (/*x*/)
#  define FUNCTION(t, f, x) t f (/*x*/)
#  define _DOTS_
#  define _VOID_
#endif

/* This isn't sys-dependent, but putting it here benefits sys.c and drivers
   - allow use of 'const' regardless of compiler */

#if !defined(__STDC__) && !defined(_MSC_VER)
#  define const
#endif

#ifdef VXWORKS
/* Replace VxWorks' printf with a real one that does fprintf(stdout, ...) */
EXTERN_FUNCTION(int, real_printf, (const char *fmt, ...));
#  define printf real_printf
#endif

#if __GNUC__
#  define __noreturn __attribute__((noreturn))
#else
#  define __noreturn
#endif

/*
** The uint32, sint32 etc datatypes are deprecated and will be removed in 
** favour of the following datatypes:
** Eterm: A tagged erlang term (possibly 64 bits)
** UInt:  An unsigned integer exactly as large as an Eterm.
** SInt:  A signed integer exactly as large as an eterm and therefor large
**        enough to hold the return value of the signed_val() macro.
** Uint32: An unsigned integer of 32 bits exactly
** Sint32: A signed integer of 32 bits exactly
** Uint16: An unsigned integer of 16 bits exactly
** Sint16: A signed integer of 16 bits exactly.
*/

#if defined(SIZEOF_LONG) && (SIZEOF_LONG == 8)
#define ARCH_64
#endif

#ifdef ARCH_64

/* The new and preferred datatypes */
typedef unsigned long    Eterm;
typedef unsigned long    Uint;   
typedef long             Sint;
typedef unsigned int     Uint32;
typedef int              Sint32;
typedef unsigned short   Uint16;
typedef short            Sint16;

typedef unsigned char	 byte;

#else

/* The new and preferred datatypes */
typedef unsigned long   Eterm;  
typedef unsigned long   Uint;   
typedef long            Sint;
typedef unsigned long   Uint32;
typedef long            Sint32;
typedef unsigned short  Uint16;
typedef short           Sint16;

/* The old and deprecated types */
typedef long		sint32;
typedef unsigned long	uint32;
typedef short		sint16;
typedef unsigned short	uint16;
typedef unsigned char	byte;

#endif


/* Deal with memcpy() vs bcopy() etc. We want to use the mem*() functions,
   but be able to fall back on bcopy() etc on systems that don't have
   mem*(), but this doesn't work to well with memset()/bzero() - thus the
   memzero() macro.
*/

/* xxxP */
#if defined(USE_BCOPY)
#  define memcpy(a, b, c) bcopy((b), (a), (c))
#  define memcmp(a, b, c) bcmp((a), (b), (c))
#  define memzero(buf, len) bzero((buf), (len))
#else
#  define memzero(buf, len) memset((buf), '\0', (len))
#endif

/* Stuff that is useful for port programs, drivers, etc */

#ifdef ISC32			/* Too much for the Makefile... */
#  define signal	sigset
#  define lgamma	undef_math_func_1
#  define asinh	undef_math_func_1
#  define acosh	undef_math_func_1
#  define atanh	undef_math_func_1
#  define NO_FTRUNCATE
#  define SIG_SIGHOLD
#  define _POSIX_SOURCE 
#  define _XOPEN_SOURCE
#endif

#ifdef QNX			/* Too much for the Makefile... */
#  define SYS_SELECT_H
#  define erf	undef_math_func_1
#  define erfc	undef_math_func_1
#  define lgamma	undef_math_func_1
/* This definition doesn't take NaN into account, but matherr() gets those */
#  define finite(x) (fabs(x) != HUGE_VAL)
#  define USE_MATHERR
#  define HAVE_FINITE
#endif


#ifdef WANT_NONBLOCKING	    /* must define this to pull in fcntl.h/ioctl.h */

/* This is really a mess... We used to use fcntl O_NDELAY, but that seems
   to only work on SunOS 4 - in particular, on SysV-based systems
   (including Solaris 2), it does set non-blocking mode, but causes
   read() to return 0!!  fcntl O_NONBLOCK is specified by POSIX, and
   seems to work on most systems, with the notable exception of AIX,
   where the old ioctl FIONBIO is the *only* one that will set a *socket*
   in non-blocking mode - and ioctl FIONBIO on AIX *doesn't* work for
   pipes or ttys (O_NONBLOCK does)!!! For now, we'll use FIONBIO for AIX. */

#  ifdef __WIN32__

static unsigned long zero_value = 0, one_value = 1;
#    define SET_BLOCKING(fd)	{ if (ioctlsocket((fd), FIONBIO, &zero_value) != 0) fprintf(stderr, "Error setting socket to non-blocking: %d\n", WSAGetLastError()); }
#    define SET_NONBLOCKING(fd)	ioctlsocket((fd), FIONBIO, &one_value)

#  else
#    ifdef VXWORKS
#      include <fcntl.h> /* xxxP added for O_WRONLY etc ... macro:s ... */
#      include <ioLib.h>
static const int zero_value = 0, one_value = 1;
#      define SET_BLOCKING(fd)	ioctl((fd), FIONBIO, (int)&zero_value)
#      define SET_NONBLOCKING(fd)	ioctl((fd), FIONBIO, (int)&one_value)
#      define ERRNO_BLOCK EWOULDBLOCK

#    else
#      ifdef NB_FIONBIO		/* Old BSD */
#        include <sys/ioctl.h>
  static const int zero_value = 0, one_value = 1;
#        define SET_BLOCKING(fd)	ioctl((fd), FIONBIO, &zero_value)
#        define SET_NONBLOCKING(fd)	ioctl((fd), FIONBIO, &one_value)
#        define ERRNO_BLOCK EWOULDBLOCK
#      else /* !NB_FIONBIO */
#        include <fcntl.h>
#        ifdef NB_O_NDELAY		/* Nothing needs this? */
#          define NB_FLAG O_NDELAY
#          ifndef ERRNO_BLOCK		/* allow override (e.g. EAGAIN) via Makefile */
#            define ERRNO_BLOCK EWOULDBLOCK
#          endif
#        else  /* !NB_O_NDELAY */	/* The True Way - POSIX!:-) */
#          define NB_FLAG O_NONBLOCK
#          define ERRNO_BLOCK EAGAIN
#        endif /* !NB_O_NDELAY */
#        define SET_BLOCKING(fd)	fcntl((fd), F_SETFL, \
	  			      fcntl((fd), F_GETFL, 0) & ~NB_FLAG)
#        define SET_NONBLOCKING(fd)	fcntl((fd), F_SETFL, \
				      fcntl((fd), F_GETFL, 0) | NB_FLAG)
#      endif /* !NB_FIONBIO */
#    endif /* _WXWORKS_ */
#  endif /* !__WIN32__ */
#endif /* WANT_NONBLOCKING */
     
EXTERN_FUNCTION(int, check_async_ready, (_VOID_));

#ifdef USE_THREADS

EXTERN_FUNCTION(void, sys_async_ready, (int hndl));

#endif

/* Io constants to sys_printf and sys_putc */

typedef enum {
    CBUF = 0,
    COUT = 1,
    CERR = 2
} CIO;

typedef struct preload {
    char *name;			/* Name of module */
    int  size;			/* Size of code */
    unsigned char* code;	/* Code pointer */
} Preload;


/*
 * This structure contains options to all built in drivers.
 * None of the drivers use all of the fields.
 */

typedef struct _SysDriverOpts {
    int ifd;			/* Input file descriptor (fd driver). */
    int ofd;			/* Outputfile descriptor (fd driver). */
    int packet_bytes;		/* Number of bytes in packet header. */
    int read_write;		/* Read and write bits. */
    int use_stdio;		/* Use standard I/O: TRUE or FALSE. */
    int redir_stderr;           /* Redirect stderr to stdout: TRUE/FALSE. */
    int hide_window;		/* Hide this windows (Windows). */
    int exit_status;		/* Report exit status of subprocess. */
    char *envir;		/* Environment of the port process, */
				/* in Windows format. */
    char *wd;			/* Working directory. */
} SysDriverOpts;


extern int cerr_pos;

extern char os_type[];

extern int sys_init_time(void);
extern void erts_deliver_time(SysTimeval *);
extern void erts_time_remaining(SysTimeval *);
extern int erts_init_time_sup(void);

/*
 * System interfaces for startup/sae code (functions found in respective sys.c)
 */
extern void erl_sys_init(void);
extern void erl_sys_args(int *argc, char **argv);
extern void erl_sys_schedule_loop(void);
void sys_tty_reset(void);

EXTERN_FUNCTION(int, sys_max_files, (_VOID_));
void sys_init_io(byte*, Uint);
Preload* sys_preloaded(void);
EXTERN_FUNCTION(unsigned char*, sys_preload_begin, (Preload*));
EXTERN_FUNCTION(void, sys_preload_end, (Preload*));
EXTERN_FUNCTION(int, sys_get_key, (int));
EXTERN_FUNCTION(void, elapsed_time_both, (unsigned long*, unsigned long*, unsigned long*, unsigned long*));
EXTERN_FUNCTION(void, wall_clock_elapsed_time_both, (unsigned long*, unsigned long*));
EXTERN_FUNCTION(void, get_time, (int*, int*, int*));
EXTERN_FUNCTION(void, get_date, (int*, int*, int*));
EXTERN_FUNCTION(void, get_localtime, (int*, int*, int*, int*, int*, int*));
EXTERN_FUNCTION(void, get_universaltime, (int*, int*, int*, int*, int*, int*));
EXTERN_FUNCTION(int, univ_to_local, (int*, int*, int*, int*, int*, int*));
EXTERN_FUNCTION(int, local_to_univ, (int*, int*, int*, int*, int*, int*));
void get_now(Uint32*, Uint32*, Uint32*);
EXTERN_FUNCTION(void, set_break_quit, (void (*)(), void (*)()));



typedef void *GETENV_STATE;

EXTERN_FUNCTION(void, os_flavor, (char*, unsigned));
EXTERN_FUNCTION(void, os_version, (int*, int*, int*));
EXTERN_FUNCTION(void, init_getenv_state, (GETENV_STATE *));
EXTERN_FUNCTION(char *, getenv_string, (GETENV_STATE *));

/* xxxP */
EXTERN_FUNCTION(void, init_sys_float, (void));
EXTERN_FUNCTION(int, sys_chars_to_double, (char*, double*));
EXTERN_FUNCTION(int, sys_double_to_chars, (double, char*));
EXTERN_FUNCTION(void, sys_printf, (CIO, char*, _DOTS_));
EXTERN_FUNCTION(void, sys_putc, (int, CIO));
EXTERN_FUNCTION(void, sys_get_pid, (char *));
EXTERN_FUNCTION(int, sys_putenv, (char *));

/* Defined in sys.c (util.c when instrumented) */
EXTERN_FUNCTION(void*, sys_alloc, (Uint));
EXTERN_FUNCTION(void*, sys_realloc, (void*,Uint));
EXTERN_FUNCTION(void,  sys_free, (void*));

#ifdef INSTRUMENT
/* Defined in sys.c */
#ifndef sys_alloc2 /* Declare if not macros */
EXTERN_FUNCTION(void*, sys_alloc2, (Uint));
EXTERN_FUNCTION(void*, sys_realloc2, (void*, Uint));
EXTERN_FUNCTION(void,  sys_free2, (void*));
#endif /* !sys_alloc2 */


EXTERN_FUNCTION(void *, instr_alloc, (int, void *(*)(Uint), Uint));
EXTERN_FUNCTION(void *, instr_realloc,
		(int, void *(*)(void *, Uint, Uint), void *, Uint, Uint));
EXTERN_FUNCTION(void, instr_free, (void (*free_func)(void *), void *));

EXTERN_FUNCTION(void*, sys_realloc3, (void*, Uint, Uint));

#define sys_alloc_from(Where, Size) \
  instr_alloc((Where), sys_alloc2, (Size))
#define sys_realloc_from(Where, Ptr, Size) \
  instr_realloc((Where), sys_realloc3, (Ptr), 0, (Size))

#else /* #ifdef INSTRUMENT */

#define sys_alloc_from(Where, Size) sys_alloc(Size)
#define sys_realloc_from(Where,Ptr, Size) sys_realloc((Ptr),(Size))

#define fix_alloc_from(Where, Desc) fix_alloc((Desc))
#define safe_alloc_from(Where, Size) safe_alloc((Size))
#define safe_realloc_from(Where, Ptr, Size) safe_realloc((Ptr), (Size))

#endif /* #ifdef INSTRUMENT */

/* Options to sys_alloc_opt */
#define SYS_ALLOC_OPT_TRIM_THRESHOLD 0
#define SYS_ALLOC_OPT_TOP_PAD        1
#define SYS_ALLOC_OPT_MMAP_THRESHOLD 2
#define SYS_ALLOC_OPT_MMAP_MAX       3

/* Default values to sys_alloc_opt options */
#define ERTS_DEFAULT_TRIM_THRESHOLD  (128 * 1024)
#define ERTS_DEFAULT_TOP_PAD         0
#define ERTS_DEFAULT_MMAP_THRESHOLD  (128 * 1024)
#define ERTS_DEFAULT_MMAP_MAX        64

EXTERN_FUNCTION(int, sys_alloc_opt, (int, int));

typedef struct {
  Sint trim_threshold;
  Sint top_pad;
  Sint mmap_threshold;
  Sint mmap_max;
#ifdef INSTRUMENT
  Uint total;
  Uint maximum;
#endif
} SysAllocStat;

EXTERN_FUNCTION(void, sys_alloc_stat, (SysAllocStat *));

#ifdef VXWORKS
/* NOTE! sys_calloc2 does not exist on other 
   platforms than VxWorks */
EXTERN_FUNCTION(void*, sys_calloc2, (Uint, Uint));
#endif /* VXWORKS */


#define sys_memcpy(s1,s2,n)  memcpy(s1,s2,n)
#define sys_memmove(s1,s2,n) memmove(s1,s2,n)
#define sys_memcmp(s1,s2,n)  memcmp(s1,s2,n)
#define sys_memset(s,c,n)    memset(s,c,n)
#define sys_memzero(s, n)    memset(s,'\0',n)
#define sys_strcmp(s1,s2)    strcmp(s1,s2)
#define sys_strncmp(s1,s2,n) strncmp(s1,s2,n)
#define sys_strcpy(s1,s2)    strcpy(s1,s2)
#define sys_strncpy(s1,s2,n) strncpy(s1,s2,n)
#define sys_strlen(s)        strlen(s)

/* define function symbols (needed in sys_drv_api) */
#define sys_fp_alloc     sys_alloc
#define sys_fp_realloc   sys_realloc
#define sys_fp_free      sys_free
#define sys_fp_memcpy    memcpy
#define sys_fp_memmove   memmove
#define sys_fp_memcmp    memcmp
#define sys_fp_memset    memset
/* #define sys_fp_memzero    elib_memzero */
#define sys_fp_strcmp    strcmp
#define sys_fp_strncmp   strncmp
#define sys_fp_strcpy    strcpy
#define sys_fp_strncpy   strncpy
#define sys_fp_strlen    strlen


/* Return codes from the nb_read and nb_write functions */
#define FD_READY 1
#define FD_CONTINUE 2
#define FD_ERROR 3



/* Standard set of integer macros  .. */

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(i, s) {((char*)(s))[0] = (char)((i) >> 24) & 0xff; \
                        ((char*)(s))[1] = (char)((i) >> 16) & 0xff; \
                        ((char*)(s))[2] = (char)((i) >> 8)  & 0xff; \
                        ((char*)(s))[3] = (char)((i)        & 0xff);}

#define get_int16(s) ((((unsigned char*)  (s))[0] << 8) | \
                      (((unsigned char*)  (s))[1]))


#define put_int16(i, s) {((unsigned char*)(s))[0] = ((i) >> 8) & 0xff; \
                        ((unsigned char*)(s))[1] = (i)         & 0xff;}

#define get_int8(s) ((((unsigned char*)  (s))[0] ))


#define put_int8(i, s) { ((unsigned char*)(s))[0] = (i)         & 0xff;}

/*
 * Use DEBUGF as you would use printf, but use double parentheses:
 *
 *   DEBUGF(("Error: %s\n", error));
 *
 * The output will appear in a special console.
 */

#ifdef DEBUG
EXTERN_FUNCTION(void, erl_debug, (char* format, ...));
EXTERN_FUNCTION(void, erl_bin_write, (unsigned char *, int, int));

#  define DEBUGF(x) erl_debug x
#else
#  define DEBUGF(x)
#endif


#ifdef VXWORKS
/* This includes redefines of malloc etc 
   this should be done after sys_alloc, etc, above */
#  include "reclaim.h"
/*********************Malloc and friends************************
 * There is a problem with the naming of malloc and friends, 
 * malloc is used throughout sys.c and the resolver to mean save_alloc,
 * but it should actually mean either sys_alloc or sys_alloc2,
 * so the definitions from reclaim_master.h are not any
 * good, i redefine the malloc family here, although it's quite 
 * ugly, actually it would be preferrable to use the
 * names sys_alloc and so on throughout the offending code, but
 * that will be saved as an later exercise...
 * I also add an own calloc, to make the BSD resolver source happy.
 ***************************************************************/
/* Undefine malloc and friends */
#  ifdef malloc
#    undef malloc
#  endif
#  ifdef calloc
#    undef calloc
#  endif
#  ifdef realloc
#    undef realloc
#  endif
#  ifdef free
#    undef free
#  endif
/* Redefine malloc and friends */
#  define malloc sys_alloc
#  define calloc  sys_calloc
#  define realloc  sys_realloc
#  define free sys_free

#endif


#ifdef __WIN32__
void call_break_handler(void);
char* last_error(void);
char* win32_errorstr(int);


#endif


#endif

