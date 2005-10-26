/* $Id$
 * hipe_x86_signal.c
 *
 * Erlang code compiled to x86 native code uses the x86 %esp as its
 * stack pointer. This improves performance in several ways:
 * - It permits the use of the x86 call and ret instructions, which
 *   reduces code volume and improves branch prediction.
 * - It avoids stealing a gp register to act as a stack pointer.
 *
 * Unix signal handlers are by default delivered onto the current
 * stack, i.e. %esp. This is a problem since our native-code stacks
 * are small and may not have room for the Unix signal handler.
 *
 * There is a way to redirect signal handlers to an "alternate" signal
 * stack by using the SA_ONSTACK flag with the sigaction() library call.
 * Unfortunately, this has to be specified explicitly for each signal,
 * and it is difficult to enforce given the presence of libraries.
 *
 * Our solution is to override the C library's signal handler setup
 * procedure with our own which enforces the SA_ONSTACK flag.
 *
 * XXX: This code only supports Linux with glibc-2.1 or above,
 * and Solaris 8.
 */
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include "hipe_signal.h"

#if __GLIBC__ == 2 && __GLIBC_MINOR__ == 3
/* See comment below for glibc 2.2. */
#ifndef __USE_GNU
#define __USE_GNU		/* to un-hide RTLD_NEXT */
#endif
#include <dlfcn.h>
static int (*__next_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__next_sigaction != 0)
extern int __sigaction(int, const struct sigaction*, struct sigaction*);
#define __SIGACTION __sigaction
static void do_init(void)
{
    __next_sigaction = dlsym(RTLD_NEXT, "__sigaction");
    if( __next_sigaction != 0 )
	return;
    perror("dlsym");
    abort();
}
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#endif	/* glibc 2.3 */

#if __GLIBC__ == 2 && (__GLIBC_MINOR__ == 2 /*|| __GLIBC_MINOR__ == 3*/)
/*
 * __libc_sigaction() is the core routine.
 * Without libpthread, sigaction() and __sigaction() are both aliases
 * for __libc_sigaction().
 * libpthread redefines __sigaction() as a non-trivial wrapper around
 * __libc_sigaction(), and makes sigaction() an alias for __sigaction().
 * glibc has internal calls to both sigaction() and __sigaction().
 *
 * Overriding __libc_sigaction() would be ideal, but doing so breaks
 * libpthread (threads hang).
 *
 * Overriding __sigaction(), using dlsym RTLD_NEXT to find glibc's
 * version of __sigaction(), works with glibc-2.2.4 and 2.2.5.
 * Unfortunately, this solution doesn't work with earlier versions,
 * including glibc-2.2.2 and glibc-2.1.92 (2.2 despite its name):
 * 2.2.2 SIGSEGVs in dlsym RTLD_NEXT (known glibc bug), and 2.1.92
 * SIGSEGVs inexplicably in two test cases in the HiPE test suite.
 *
 * Instead we only override sigaction() and call __sigaction()
 * directly. This should work for HiPE/x86 as long as only the Posix
 * signal interface is used, i.e. there are no calls to simulated
 * old BSD or SysV interfaces.
 * glibc's internal calls to __sigaction() appear to be mostly safe.
 * hipe_signal_init() fixes some unsafe ones, e.g. the SIGPROF handler.
 *
 * Tested with glibc-2.1.92 on RedHat 7.0, glibc-2.2.2 on RedHat 7.1,
 * glibc-2.2.4 on RedHat 7.2, and glibc-2.2.5 on RedHat 7.3.
 */
#if 0
/* works with 2.2.5 and 2.2.4, but not 2.2.2 or 2.1.92 */
#define __USE_GNU		/* to un-hide RTLD_NEXT */
#include <dlfcn.h>
static int (*__next_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__next_sigaction != 0)
#define __SIGACTION __sigaction
static void do_init(void)
{
    __next_sigaction = dlsym(RTLD_NEXT, "__sigaction");
    if( __next_sigaction != 0 )
	return;
    perror("dlsym");
    abort();
}
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#else
/* semi-works with all 2.2 versions so far */
extern int __sigaction(int, const struct sigaction*, struct sigaction*);
#define __next_sigaction __sigaction	/* pthreads-aware version */
#undef __SIGACTION	/* we can't override __sigaction() */
#define INIT()		do{}while(0)
#endif
#endif	/* glibc 2.2 */

#if __GLIBC__ == 2 && __GLIBC_MINOR__ == 1
/*
 * __sigaction() is the core routine.
 * Without libpthread, sigaction() is an alias for __sigaction().
 * libpthread redefines sigaction() as a non-trivial wrapper around
 * __sigaction().
 * glibc has internal calls to both sigaction() and __sigaction().
 *
 * Overriding __sigaction() would be ideal, but doing so breaks
 * libpthread (threads hang). Instead we override sigaction() and
 * use dlsym RTLD_NEXT to find glibc's version of sigaction().
 * glibc's internal calls to __sigaction() appear to be mostly safe.
 * hipe_signal_init() fixes some unsafe ones, e.g. the SIGPROF handler.
 *
 * Tested with glibc-2.1.3 on RedHat 6.2.
 */
#include <dlfcn.h>
static int (*__next_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__next_sigaction != 0)
#undef __SIGACTION
static void do_init(void)
{
    __next_sigaction = dlsym(RTLD_NEXT, "sigaction");
    if( __next_sigaction != 0 )
	return;
    perror("dlsym");
    abort();
}
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#endif	/* glibc 2.1 */

#if !defined(__GLIBC__)
/*
 * Assume Solaris/x86 2.8.
 * There is a number of sigaction() procedures in libc:
 * * sigaction(): weak reference to _sigaction().
 * * _sigaction(): apparently a simple wrapper around __sigaction().
 * * __sigaction(): apparently the procedure doing the actual system call.
 * * _libc_sigaction(): apparently some thread-related wrapper, which ends
 *   up calling __sigaction().
 * The threads library redefines sigaction() and _sigaction() to its
 * own wrapper, which checks for and restricts access to threads-related
 * signals. The wrapper appears to eventually call libc's __sigaction().
 *
 * We catch and override _sigaction() since overriding __sigaction()
 * causes fatal errors in some cases.
 *
 * When linked with thread support, there are calls to sigaction() before
 * our init routine has had a chance to find _sigaction()'s address.
 * This forces us to initialise at the first call.
 */
#include <dlfcn.h>
static int (*__next_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__next_sigaction != 0)
#define __SIGACTION _sigaction
static void do_init(void)
{
    __next_sigaction = dlsym(RTLD_NEXT, "_sigaction");
    if( __next_sigaction != 0 )
	return;
    perror("dlsym");
    abort();
}
#define _NSIG NSIG
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#endif	/* not glibc */

/*
 * This is our wrapper for sigaction(). sigaction() can be called before
 * hipe_signal_init() has been executed, especially when threads support
 * has been linked with the executable. Therefore, we must initialise
 * __next_sigaction() dynamically, the first time it's needed.
 */
static int my_sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    struct sigaction newact;

    INIT();

    if( act &&
	act->sa_handler != SIG_DFL &&
	act->sa_handler != SIG_IGN &&
	!(act->sa_flags & SA_ONSTACK) ) {
	newact = *act;
	newact.sa_flags |= SA_ONSTACK;
	act = &newact;
    }
    return __next_sigaction(signum, act, oldact);
}

/*
 * This overrides the C library's core sigaction() procedure, catching
 * all its internal calls.
 */
#ifdef __SIGACTION
int __SIGACTION(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    return my_sigaction(signum, act, oldact);
}
#endif

/*
 * This catches the application's own sigaction() calls.
 */
int sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    return my_sigaction(signum, act, oldact);
}

/*
 * 1. Set up alternate signal stack.
 * 2. Add SA_ONSTACK to existing user-defined signal handlers.
 */
void hipe_signal_init(void)
{
    static unsigned long my_sigstack[SIGSTKSZ/sizeof(long)];
    struct sigaltstack ss;
    struct sigaction sa;
    int i;

    INIT();

    ss.ss_sp = &my_sigstack[0];
    ss.ss_flags = SS_ONSTACK;
    ss.ss_size = sizeof my_sigstack;
    if( sigaltstack(&ss, NULL) < 0 ) {
	/* might be a broken pre-2.4 Linux kernel, try harder */
	ss.ss_flags = 0;
	if( sigaltstack(&ss, NULL) < 0 ) {
	    perror("sigaltstack");
	    abort();
	}
    }

    for(i = 1; i < _NSIG; ++i) {
	if( sigaction(i, NULL, &sa) ) {
	    /* This will fail with EINVAL on Solaris if 'i' is one of the
	       thread library's private signals. We DO catch the initial
	       setup of these signals, so things MAY be OK anyway. */
	    continue;
	}
	if( sa.sa_handler == SIG_DFL ||
	    sa.sa_handler == SIG_IGN ||
	    (sa.sa_flags & SA_ONSTACK) )
	    continue;
	sa.sa_flags |= SA_ONSTACK;
	if( sigaction(i, &sa, NULL) ) {
	    perror("sigaction");
	    abort();
	}
    }
}
