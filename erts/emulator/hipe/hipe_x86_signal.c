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
 * XXX: The current code only supports Linux with glibc 2.2 or 2.1,
 * and Solaris 8.
 */
#include <signal.h>
#include <stdio.h>

#if __GLIBC__ == 2 && __GLIBC_MINOR__ == 2
/*
 * In glibc 2.2 the signal setup functions call __sigaction(), which in
 * turn calls __libc_sigaction() [or perhaps there's a weak ref, I dunno].
 * So we just call __libc_sigaction() directly by name.
 * Note: The RTLD_NEXT trick does not work.
 */
extern int __libc_sigaction(int, const struct sigaction*, struct sigaction*);
#define INIT()		do{}while(0)
#endif

#if __GLIBC__ == 2 && __GLIBC_MINOR__ == 1
/*
 * In glibc 2.1 the signal setup functions call __sigaction() but there
 * is no further function behind it. So we use dlsym(RTLD_NEXT) to obtain
 * __sigaction()'s address, and call it indirectly.
 * XXX: May also work with glibc 2.0, but I can't test this.
 *
 * When linked with thread support, there are calls to sigaction() before
 * our init routine has had a chance to find __sigaction()'s address.
 * This forces us to initialise at the first call.
 */
#include <dlfcn.h>
static int (*__libc_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__libc_sigaction != 0)
static void do_init(void)
{
    if( (__libc_sigaction = dlsym(RTLD_NEXT, "__sigaction")) != 0 )
	return;
    perror("dlsym");
    abort();
}
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#endif

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
static int (*__libc_sigaction)(int, const struct sigaction*, struct sigaction*);
#define init_done()	(__libc_sigaction != 0)
static void do_init(void)
{
    if( (__libc_sigaction = dlsym(RTLD_NEXT, "_sigaction")) != 0 )
	return;
    perror("dlsym");
    abort();
}
#define __sigaction _sigaction
#define _NSIG NSIG
#define INIT()	do { if( !init_done() ) do_init(); } while(0)
#endif

/*
 * This is our wrapper for sigaction(). sigaction() can be called before
 * hipe_signal_init() has been executed, especially when threads support
 * has been linked with the executable. Therefore, we must initialise
 * __libc_sigaction() dynamically, the first time it's needed.
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
    return __libc_sigaction(signum, act, oldact);
}

/*
 * This overrides the C library's core sigaction() procedure, catching
 * all its internal calls.
 */
int __sigaction(int signum, const struct sigaction *act, struct sigaction *oldact)
{
    return my_sigaction(signum, act, oldact);
}

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
