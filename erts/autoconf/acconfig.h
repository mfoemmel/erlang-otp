
/*
 *  Erlang emulator options.
 */

/* Define if you don't want the fix allocator in Erlang */
#undef NO_FIX_ALLOC

/* Define if you wish to redefine FD_SETSIZE to be able to select on more fd */
#undef REDEFINE_FD_SETSIZE

/* Define if you do not have a high-res. timer & want to use times() instead */
#undef CORRECT_USING_TIMES


/*
 * elib_malloc options.
 */

/* Define to enable use of elib_malloc (a malloc() replacement). */
#undef ENABLE_ELIB_MALLOC

/* */
#undef ELIB_HEAP_SBRK

/* Define to enable the use of sorted blocks when using elib_malloc. */
#undef ELIB_SORTED_BLOCKS

/* Define to enable use of dlmalloc (a malloc() replacement). */
#undef ENABLE_DLMALLOC


/*
 *  Misc. system calls and include files.
 */

/* Define if you have the <sys/uio.h> header file. */
#undef HAVE_UIO_H

/* */
#undef NO_PRAGMA_WEAK

/* */
#undef NO_SA_LEN

/* */
#undef SOCKOPT_CONNECT_STAT

/* define if the variable sys_errlist is declared in a system header file */
#undef SYS_ERRLIST_DECLARED

/* define if you have the Solaris/ultrasparc /dev/perfmon interface */
#undef HAVE_SOLARIS_SPARC_PERFMON

/*
 *  Networking.
 */

/* Define if you have the <net/errno.h> header file. */
#undef HAVE_NET_ERRNO_H

/* Possible gethostbyname_r() function interfaces (sigh...) */
/* Note: don't use the number 1, to protect against out-of-sync configure */
#define GHBN_R_SOLARIS	2
#define GHBN_R_AIX	3
#define GHBN_R_GLIBC	4
/* Define to one of the above if you have the gethostbyname_r() function. */
#undef HAVE_GETHOSTBYNAME_R

/* define if h_errno is declared (in some way) in a system header file */
#undef H_ERRNO_DECLARED

/* Define if you need to include rpc/types.h to get INADDR_LOOPBACK defined */
#undef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H

/* Uncomment if you don't have a definition of INADDR_LOOPBACK */
#undef HAVE_NO_INADDR_LOOPBACK

/* Define if you have IP version 6 support. */
#undef HAVE_IN6

/* Early linux used in_addr6 instead of in6_addr, define if you have this */
#undef HAVE_IN_ADDR6_STRUCT

/* Define if setsockopt() accepts multicast options */
#undef HAVE_MULTICAST_SUPPORT

/* Define if you have <sys/sockio.h header file. */
#undef HAVE_SOCKIO_H

/* Define if you require <sys/socketio.h> instead of <sys/sockio.h> */
#undef HAVE_SOCKETIO_H

/* Define if you have SO_BSDCOMPAT flag on sockets. */
#undef HAVE_SO_BSDCOMPAT

/* Define if you have a decl of fprintf that conflicts with int fprintf() */
#undef HAVE_CONFLICTING_FPRINTF_DECLARATION
       
/* Define if you have a decl of fread that conflicts with int fread() */
#undef HAVE_CONFLICTING_FREAD_DECLARATION

/* Define if you have a decl of sbrk that conflicts with "void *sbrk()" */
#undef HAVE_VOID_PTR_SBRK_CONFLICT

/*
 *  Thread support.
 */

/* Define if you have the <pthread.h> header file. */
#undef HAVE_PTHREAD_H

/* Define if you have the <thread.h> header file. */
#undef HAVE_THREAD_H

/* Define the pthread.h header file is in pthread/mit directory. */
#undef HAVE_MIT_PTHREAD_H

/* Define if you want to enable child waiter thread */
#undef ENABLE_CHILD_WAITER_THREAD

/*
 *  Math definitions.
 */

/* Define if you have the finite() function. */
#undef HAVE_FINITE

/* Define if you have the fpsetmask() function. */
#undef HAVE_FPSETMASK

/* Define if you have matherr() function and struct exception type. */
#undef USE_MATHERR

/* Define if you have the ieee_handler() function. */
#undef HAVE_IEEE_HANDLER


/*
 * The lines below this marker is copied into the bottom of config.h.in
 */
@BOTTOM@

/* Redefine in6_addr. XXX this should be moved to the files where it's used? */
#ifdef HAVE_IN_ADDR6_STRUCT
#define in6_addr in_addr6
#endif

/* Define a reasonable default for INADDR_LOOPBACK */
/* XXX this should be moved to the files where it's used? */
#ifdef HAVE_NO_INADDR_LOOPBACK
#define INADDR_LOOPBACK (u_long)0x7F000001
#endif

#ifndef PURIFY /* Don't use elib_malloc or dlmalloc as clib
		  when purify is used */

/* If elib_malloc is used then elib_alloc_is_clib */
#ifdef ENABLE_ELIB_MALLOC
#define ELIB_ALLOC_IS_CLIB
#endif

/* If dlmalloc is used then dlmalloc is clib */
#ifdef ENABLE_DLMALLOC
#define DLMALLOC_IS_CLIB
#endif

#if defined(ELIB_ALLOC_IS_CLIB) && defined(DLMALLOC_IS_CLIB)
#error "Both elib_malloc and dlmalloc used as clib"
#endif

#endif

#ifdef REDEFINE_FD_SETSIZE
#define FD_SETSIZE 1024
#endif

#if !defined(HAVE_POLL) || !defined(HAVE_POLL_H)
#define USE_SELECT
#endif
