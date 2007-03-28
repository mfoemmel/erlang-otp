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
 * Description: Thread library for use in the ERTS and other OTP
 *              applications.
 * Author: Rickard Green
 */

#ifndef ETHREAD_H__
#define ETHREAD_H__

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#  include "ethread_header_config.h"
#endif

#include <stdlib.h>
#include <errno.h>

typedef struct {
    long tv_sec;
    long tv_nsec;
} ethr_timeval;

#if defined(DEBUG)
#  undef ETHR_XCHK
#  define  ETHR_XCHK 1
#else
#  ifndef ETHR_XCHK
#    define ETHR_XCHK 0
#  endif
#endif

#undef ETHR_INLINE
#if defined(__GNUC__)
#  define ETHR_INLINE __inline__
#elif defined(__WIN32__)
#  define ETHR_INLINE __forceinline
#endif
#if defined(DEBUG) || !defined(ETHR_INLINE) || ETHR_XCHK
#  undef ETHR_TRY_INLINE_FUNCS
#endif

#define ETHR_RWMUTEX_INITIALIZED 	0x99999999
#define ETHR_MUTEX_INITIALIZED		0x77777777
#define ETHR_COND_INITIALIZED		0x55555555

#if defined(ETHR_PTHREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The pthread implementation                                                *
\*                                                                           */

#if defined(__linux__) && !defined(_GNU_SOURCE)
#error "_GNU_SOURCE not defined. Please, compile all files with -D_GNU_SOURCE."
#endif

#if defined(ETHR_HAVE_MIT_PTHREAD_H)
#include <pthread/mit/pthread.h>
#elif defined(ETHR_HAVE_PTHREAD_H)
#include <pthread.h>
#endif

#define ETHR_HAVE_ETHR_MUTEX_TRYLOCK 1

/* Types */

typedef pthread_t ethr_tid;

typedef struct ethr_mutex_ ethr_mutex;
struct ethr_mutex_ {
    pthread_mutex_t pt_mtx;
    int is_rec_mtx;
    ethr_mutex *prev;
    ethr_mutex *next;
#if ETHR_XCHK
    int initialized;
#endif
};

typedef struct ethr_cond_ ethr_cond;
struct ethr_cond_ {
    pthread_cond_t pt_cnd;
#if ETHR_XCHK
    int initialized;
#endif
};

#ifdef ETHR_EXTENDED_LIB

typedef struct ethr_rwmutex_ ethr_rwmutex;
struct ethr_rwmutex_ {
    pthread_rwlock_t pt_rwlock;
#if ETHR_XCHK
    int initialized;
#endif
};

#endif

/* Static initializers */
#if ETHR_XCHK
#define ETHR_MUTEX_XCHK_INITER	, ETHR_MUTEX_INITIALIZED
#define ETHR_COND_XCHK_INITER	, ETHR_COND_INITIALIZED
#else
#define ETHR_MUTEX_XCHK_INITER
#define ETHR_COND_XCHK_INITER
#endif

#define ETHR_MUTEX_INITER {PTHREAD_MUTEX_INITIALIZER, 0, NULL, NULL ETHR_MUTEX_XCHK_INITER}
#define ETHR_COND_INITER {PTHREAD_COND_INITIALIZER ETHR_COND_XCHK_INITER}

#if defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETTYPE) \
    || defined(ETHR_HAVE_PTHREAD_MUTEXATTR_SETKIND_NP)
#  define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1
#  ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#    define ETHR_REC_MUTEX_INITER \
            {PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP, 1, NULL, NULL ETHR_MUTEX_XCHK_INITER}
#  endif
#else
#  undef ETHR_HAVE_ETHR_REC_MUTEX_INIT
#endif

#ifndef ETHR_HAVE_PTHREAD_ATFORK
#  define ETHR_NO_FORKSAFETY 1
#endif

typedef pthread_key_t ethr_tsd_key;

#define ETHR_HAVE_ETHR_SIG_FUNCS 1

#ifdef ETHR_EXTENDED_LIB

#if (!defined(__builtin_expect) \
     || !defined(__GNUC__) \
     || (__GNUC__ < 3 && __GNUC_MINOR__ < 96))
#define __builtin_expect(X, Y) (X)
#endif

/* For CPU-optimised atomics, spinlocks, and rwlocks. */
#if defined(__GNUC__)
#if defined(__i386__)
#include "i386/ethread.h"
#elif defined(__x86_64__)
#include "x86_64/ethread.h"
#elif (defined(__powerpc__) || defined(__ppc__)) && !defined(__powerpc64__)
#include "ppc32/ethread.h"
#elif defined(__sparc__)
#include "sparc32/ethread.h"
#endif
#endif /* __GNUC__ */

#if defined(PURIFY) || defined(VALGRIND) || defined(ETHR_DISABLE_NATIVE_OPS)
#undef ETHR_HAVE_NATIVE_ATOMICS
#undef ETHR_HAVE_NATIVE_LOCKS
#endif

#ifdef ETHR_HAVE_NATIVE_ATOMICS
/*
 * Map ethread native atomics to ethread API atomics.
 */
typedef ethr_native_atomic_t ethr_atomic_t;

static ETHR_INLINE int
ethr_atomic_init(ethr_atomic_t *var, long i)
{
    ethr_native_atomic_init(var, i);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_set(ethr_atomic_t *var, long i)
{
    ethr_native_atomic_set(var, i);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_read(ethr_atomic_t *var, long *i)
{
    *i = ethr_native_atomic_read(var);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_add(ethr_atomic_t *var, long incr)
{
    ethr_native_atomic_add(var, incr);
    return 0;
}   
    
static ETHR_INLINE int
ethr_atomic_addtest(ethr_atomic_t *var, long i, long *testp)
{
    *testp = ethr_native_atomic_add_return(var, i);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_inc(ethr_atomic_t *var)
{
    ethr_native_atomic_inc(var);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_dec(ethr_atomic_t *var)
{
    ethr_native_atomic_dec(var);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_inctest(ethr_atomic_t *var, long *testp)
{
    *testp = ethr_native_atomic_inc_return(var);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_dectest(ethr_atomic_t *var, long *testp)
{
    *testp = ethr_native_atomic_dec_return(var);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_and_old(ethr_atomic_t *var, long mask, long *old)
{
    *old = ethr_native_atomic_and_retold(var, mask);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_or_old(ethr_atomic_t *var, long mask, long *old)
{
    *old = ethr_native_atomic_or_retold(var, mask);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_xchg(ethr_atomic_t *var, long new, long *old)
{
    *old = ethr_native_atomic_xchg(var, new);
    return 0;
}   

#define ETHR_HAVE_OPTIMIZED_ATOMIC_OPS 1
#endif /* ETHR_HAVE_NATIVE_ATOMICS */

#ifdef ETHR_HAVE_NATIVE_LOCKS
/*
 * Map ethread native spinlocks to ethread API spinlocks.
 */
typedef ethr_native_spinlock_t ethr_spinlock_t;

static ETHR_INLINE int
ethr_spinlock_init(ethr_spinlock_t *lock)
{
    ethr_native_spinlock_init(lock);
    return 0;
}

static ETHR_INLINE int
ethr_spinlock_destroy(ethr_spinlock_t *lock)
{
    return 0;
}

static ETHR_INLINE int
ethr_spin_unlock(ethr_spinlock_t *lock)
{
    ethr_native_spin_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_spin_lock(ethr_spinlock_t *lock)
{
    ethr_native_spin_lock(lock);
    return 0;
}

/*
 * Map ethread native rwlocks to ethread API rwlocks.
 */
typedef ethr_native_rwlock_t ethr_rwlock_t;

static ETHR_INLINE int
ethr_rwlock_init(ethr_rwlock_t *lock)
{
    ethr_native_rwlock_init(lock);
    return 0;
}

static ETHR_INLINE int
ethr_rwlock_destroy(ethr_rwlock_t *lock)
{
    return 0;
}

static ETHR_INLINE int
ethr_read_unlock(ethr_rwlock_t *lock)
{
    ethr_native_read_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_read_lock(ethr_rwlock_t *lock)
{
    ethr_native_read_lock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_write_unlock(ethr_rwlock_t *lock)
{
    ethr_native_write_unlock(lock);
    return 0;
}

static ETHR_INLINE int
ethr_write_lock(ethr_rwlock_t *lock)
{
    ethr_native_write_lock(lock);
    return 0;
}

#define ETHR_HAVE_OPTIMIZED_LOCKS 1
#endif /* ETHR_HAVE_NATIVE_LOCKS */

#endif /* #ifdef ETHR_EXTENDED_LIB */

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS

#define ETHR_ATOMIC_ADDR_BITS 4
#define ETHR_ATOMIC_ADDR_SHIFT 3

#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK

extern pthread_spinlock_t ethr_atomic_spinlock[1 << ETHR_ATOMIC_ADDR_BITS];

#define ETHR_ATOMIC_PTR2LCK__(PTR) \
(&ethr_atomic_spinlock[((((unsigned long) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
			& ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))])


#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    pthread_spinlock_t *slp__ = ETHR_ATOMIC_PTR2LCK__((AP));		\
    int res__ = pthread_spin_lock(slp__);				\
    if (res__ != 0)							\
	return res__;							\
    { EXPS; }								\
    return pthread_spin_unlock(slp__);					\
} while (0)

#else

extern pthread_mutex_t ethr_atomic_mutex[1 << ETHR_ATOMIC_ADDR_BITS];

#define ETHR_ATOMIC_PTR2LCK__(PTR) \
(&ethr_atomic_mutex[((((unsigned long) (PTR)) >> ETHR_ATOMIC_ADDR_SHIFT) \
		     & ((1 << ETHR_ATOMIC_ADDR_BITS) - 1))])

#define ETHR_ATOMIC_OP_FALLBACK_IMPL__(AP, EXPS)			\
do {									\
    pthread_mutex_t *mtxp__ = ETHR_ATOMIC_PTR2LCK__((AP));		\
    int res__ = pthread_mutex_lock(mtxp__);				\
    if (res__ != 0)							\
	return res__;							\
    { EXPS; }								\
    return pthread_mutex_unlock(mtxp__);				\
} while (0)

#endif

typedef long ethr_atomic_t;

#endif

#ifndef ETHR_HAVE_OPTIMIZED_LOCKS

#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
typedef struct {
    pthread_spinlock_t spnlck;
} ethr_spinlock_t;
typedef struct {
    pthread_spinlock_t spnlck;
    unsigned counter;
} ethr_rwlock_t;
#define ETHR_RWLOCK_WRITERS (((unsigned) 1) << 31)

#else

typedef struct {
    pthread_mutex_t mtx;
} ethr_spinlock_t;

typedef struct {
    pthread_rwlock_t rwlck;
} ethr_rwlock_t;

#endif

#endif

#ifdef ETHR_TRY_INLINE_FUNCS

static ETHR_INLINE int
ethr_mutex_trylock(ethr_mutex *mtx)
{
    return pthread_mutex_trylock(&mtx->pt_mtx);
}

static ETHR_INLINE int
ethr_mutex_lock(ethr_mutex *mtx)
{
    return pthread_mutex_lock(&mtx->pt_mtx);
}

static ETHR_INLINE int
ethr_mutex_unlock(ethr_mutex *mtx)
{
    return pthread_mutex_unlock(&mtx->pt_mtx);
}

#ifdef ETHR_EXTENDED_LIB

static ETHR_INLINE int
ethr_rwmutex_tryrlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_tryrdlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ethr_rwmutex_rlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_rdlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ethr_rwmutex_runlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_unlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ethr_rwmutex_tryrwlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_trywrlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ethr_rwmutex_rwlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_wrlock(&rwmtx->pt_rwlock);
}

static ETHR_INLINE int
ethr_rwmutex_rwunlock(ethr_rwmutex *rwmtx)
{
    return pthread_rwlock_unlock(&rwmtx->pt_rwlock);
}

#endif

#ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS

static ETHR_INLINE int
ethr_atomic_init(ethr_atomic_t *var, long i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = (ethr_atomic_t) i);
}

static ETHR_INLINE int
ethr_atomic_set(ethr_atomic_t *var, long i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var = (ethr_atomic_t) i);
}

static ETHR_INLINE int
ethr_atomic_read(ethr_atomic_t *var, long *i)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *i = (long) *var);
}

static ETHR_INLINE int
ethr_atomic_inctest(ethr_atomic_t *incp, long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, *testp = (long) ++(*incp));
}

static ETHR_INLINE int
ethr_atomic_dectest(ethr_atomic_t *decp, long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(decp, *testp = (long) --(*decp));
}

static ETHR_INLINE int
ethr_atomic_add(ethr_atomic_t *var, long incr)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *var += incr);
}   
    
static ETHR_INLINE int
ethr_atomic_addtest(ethr_atomic_t *incp, long i, long *testp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, *incp += i; *testp = *incp);
}

static ETHR_INLINE int
ethr_atomic_inc(ethr_atomic_t *incp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(incp, ++(*incp));
}

static ETHR_INLINE int
ethr_atomic_dec(ethr_atomic_t *decp)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(decp, --(*decp));
}

static ETHR_INLINE int
ethr_atomic_and_old(ethr_atomic_t *var, long mask, long *old)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var &= mask);
}

static ETHR_INLINE int
ethr_atomic_or_old(ethr_atomic_t *var, long mask, long *old)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var |= mask);
}

static ETHR_INLINE int
ethr_atomic_xchg(ethr_atomic_t *var, long new, long *old)
{
    ETHR_ATOMIC_OP_FALLBACK_IMPL__(var, *old = *var; *var = new);
}   

#endif /* #ifndef ETHR_HAVE_OPTIMIZED_ATOMIC_OPS */

#ifndef ETHR_HAVE_OPTIMIZED_LOCKS

static ETHR_INLINE int
ethr_spinlock_init(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    return pthread_spin_init(&lock->spnlck, 0);
#else
    return pthread_mutex_init(&lock->mtx, NULL);
#endif
}

static ETHR_INLINE int
ethr_spinlock_destroy(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    return pthread_spin_destroy(&lock->spnlck);
#else
    return pthread_mutex_destroy(&lock->mtx);
#endif
}


static ETHR_INLINE int
ethr_spin_unlock(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    return pthread_spin_unlock(&lock->spnlck);
#else
    return pthread_mutex_unlock(&lock->mtx);
#endif
}

static ETHR_INLINE int
ethr_spin_lock(ethr_spinlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    return pthread_spin_lock(&lock->spnlck);
#else
    return pthread_mutex_lock(&lock->mtx);
#endif
}

static ETHR_INLINE int
ethr_rwlock_init(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    lock->counter = 0;
    return pthread_spin_init(&lock->spnlck, 0);
#else
    return pthread_rwlock_init(&lock->rwlck, NULL);
#endif
}

static ETHR_INLINE int
ethr_rwlock_destroy(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    return pthread_spin_destroy(&lock->spnlck);
#else
    return pthread_rwlock_destroy(&lock->rwlck);
#endif
}

static ETHR_INLINE int
ethr_read_unlock(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    int res = pthread_spin_lock(&lock->spnlck);
    if (res != 0)
	return res;
    lock->counter--;
    return pthread_spin_unlock(&lock->spnlck);
#else
    return pthread_rwlock_unlock(&lock->rwlck);
#endif
}

static ETHR_INLINE int
ethr_read_lock(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    int locked = 0;
    do {
	int res = pthread_spin_lock(&lock->spnlck);
	if (res != 0)
	    return res;
	if ((lock->counter & ETHR_RWLOCK_WRITERS) == 0) {
	    lock->counter++;
	    locked = 1;
	}
	res = pthread_spin_unlock(&lock->spnlck);
	if (res != 0)
	    return res;
    } while (!locked);
    return 0;
#else
    return pthread_rwlock_rdlock(&lock->rwlck);
#endif
}

static ETHR_INLINE int
ethr_write_unlock(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    lock->counter = 0;
    return pthread_spin_unlock(&lock->spnlck);
#else
    return pthread_rwlock_unlock(&lock->rwlck);
#endif
}

static ETHR_INLINE int
ethr_write_lock(ethr_rwlock_t *lock)
{
#ifdef ETHR_HAVE_PTHREAD_SPIN_LOCK
    while (1) {
	int res = pthread_spin_lock(&lock->spnlck);
	if (res != 0)
	    return res;
	lock->counter |= ETHR_RWLOCK_WRITERS;
	if (lock->counter == ETHR_RWLOCK_WRITERS)
	    return 0;
	res = pthread_spin_unlock(&lock->spnlck);
	if (res != 0)
	    return res;
    }
    return 0;
#else
    return pthread_rwlock_wrlock(&lock->rwlck);
#endif
}

#endif /* ETHR_HAVE_OPTIMIZED_LOCKS */

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */

#elif defined(ETHR_WIN32_THREADS)
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * The native win32 threads implementation                                   *
\*                                                                           */

#ifdef WIN32_LEAN_AND_MEAN
#  define ETHR_WIN32_LEAN_AND_MEAN_ALREADY_DEFINED
#else
#  define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#ifndef ETHR_WIN32_LEAN_AND_MEAN_ALREADY_DEFINED
#  undef WIN32_LEAN_AND_MEAN
#endif

#ifdef ETHR_HAVE_TRYENTERCRITICALSECTION
#  define ETHR_HAVE_ETHR_MUTEX_TRYLOCK ETHR_HAVE_TRYENTERCRITICALSECTION
#endif

#ifdef ETHR_EXTENDED_LIB
#error "extended functionallity not implemented yet..."
#endif

#ifndef EWOULDBLOCK
#  define EWOULDBLOCK (10035) /* WSAEWOULDBLOCK */
#endif
#ifndef ETIMEDOUT
#  define ETIMEDOUT (10060) /* WSAETIMEDOUT */
#endif

/* Types */
typedef long ethr_tid; /* thread id type */
typedef struct {
    int initialized;
    CRITICAL_SECTION cs;
#if ETHR_XCHK
    int is_rec_mtx;
#endif
} ethr_mutex;

typedef struct cnd_wait_event__ cnd_wait_event_;

typedef struct {
    int initialized;
    CRITICAL_SECTION cs;
    cnd_wait_event_ *queue;
    cnd_wait_event_ *queue_end;
} ethr_cond;

/* Static initializers */

#define ETHR_MUTEX_INITER {0}
#define ETHR_COND_INITER {0}

#define ETHR_REC_MUTEX_INITER ETHR_MUTEX_INITER

#define ETHR_HAVE_ETHR_REC_MUTEX_INIT 1

typedef DWORD ethr_tsd_key;

#undef ETHR_HAVE_ETHR_SIG_FUNCS

typedef LONG ethr_atomic_t;

#ifdef ETHR_TRY_INLINE_FUNCS
int ethr_fake_static_mutex_init(ethr_mutex *mtx);

#ifdef ETHR_HAVE_ETHR_MUTEX_TRYLOCK

static ETHR_INLINE int
ethr_mutex_trylock(ethr_mutex *mtx)
{
    if (!mtx->initialized) {
	int res = ethr_fake_static_mutex_init(mtx);
	if (res != 0)
	    return res;
    }
    if (TryEnterCriticalSection(&mtx->cs))
	return 0;
    else
	return EBUSY;
}

#endif

static ETHR_INLINE int
ethr_mutex_lock(ethr_mutex *mtx)
{
    if (!mtx->initialized) {
	int res = ethr_fake_static_mutex_init(mtx);
	if (res != 0)
	    return res;
    }
    EnterCriticalSection(&mtx->cs);
    return 0;
}

static ETHR_INLINE int
ethr_mutex_unlock(ethr_mutex *mtx)
{
    LeaveCriticalSection(&mtx->cs);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_inctest(ethr_atomic_t *incp, long *testp)
{
    *testp = (long) InterlockedIncrement(incp);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_dectest(ethr_atomic_t *decp, long *testp)
{
    *testp = (long) InterlockedDecrement(decp);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_inc(ethr_atomic_t *incp)
{
    (void) InterlockedIncrement(incp);
    return 0;
}

static ETHR_INLINE int
ethr_atomic_dec(ethr_atomic_t *decp)
{
    (void) InterlockedDecrement(decp);
    return 0;
}

#endif /* #ifdef ETHR_TRY_INLINE_FUNCS */


#else /* No supportet thread lib found */

#ifdef ETHR_NO_SUPP_THR_LIB_NOT_FATAL
#define ETHR_NO_THREAD_LIB
#else
#error "No supported thread lib found"
#endif

#endif

#ifndef EWOULDBLOCK
#  define EWOULDBLOCK EAGAIN
#endif
#ifndef ETIMEDOUT
#  define ETIMEDOUT EAGAIN
#endif
/* ENOTSUP: same as in sys.h */
#ifndef ENOTSUP
#  ifdef EOPNOTSUPP
#    define ENOTSUP EOPNOTSUPP
#  else
#    define ENOTSUP -1738659
#  endif
#endif

typedef struct {
    void *(*alloc)(size_t);
    void *(*realloc)(void *, size_t);
    void (*free)(void *);
    void *(*thread_create_prepare_func)(void);
    void (*thread_create_parent_func)(void *);
    void (*thread_create_child_func)(void *);
} ethr_init_data;

#define ETHR_INIT_DATA_DEFAULT_INITER {malloc, realloc, free, NULL, NULL, NULL}

int ethr_init(ethr_init_data *);
int ethr_install_exit_handler(void (*funcp)(void));
int ethr_thr_create(ethr_tid *, void * (*)(void *), void *, int);
int ethr_thr_join(ethr_tid, void **);
int ethr_thr_detach(ethr_tid);
void ethr_thr_exit(void *);
ethr_tid ethr_self(void);
int ethr_equal_tids(ethr_tid, ethr_tid);
int ethr_mutex_init(ethr_mutex *);
#ifdef ETHR_HAVE_ETHR_REC_MUTEX_INIT
int ethr_rec_mutex_init(ethr_mutex *);
#endif
int ethr_mutex_destroy(ethr_mutex *);
int ethr_mutex_set_forksafe(ethr_mutex *);
int ethr_mutex_unset_forksafe(ethr_mutex *);
#ifndef ETHR_TRY_INLINE_FUNCS
#ifdef ETHR_HAVE_ETHR_MUTEX_TRYLOCK
int ethr_mutex_trylock(ethr_mutex *);
#endif
int ethr_mutex_lock(ethr_mutex *);
int ethr_mutex_unlock(ethr_mutex *);
#endif
int ethr_cond_init(ethr_cond *);
int ethr_cond_destroy(ethr_cond *);
int ethr_cond_signal(ethr_cond *);
int ethr_cond_broadcast(ethr_cond *);
int ethr_cond_wait(ethr_cond *, ethr_mutex *);
int ethr_cond_timedwait(ethr_cond *, ethr_mutex *, ethr_timeval *);

#ifdef ETHR_EXTENDED_LIB
int ethr_rwmutex_init(ethr_rwmutex *);
int ethr_rwmutex_destroy(ethr_rwmutex *);
#ifndef ETHR_TRY_INLINE_FUNCS
int ethr_rwmutex_tryrlock(ethr_rwmutex *);
int ethr_rwmutex_rlock(ethr_rwmutex *);
int ethr_rwmutex_runlock(ethr_rwmutex *);
int ethr_rwmutex_tryrwlock(ethr_rwmutex *);
int ethr_rwmutex_rwlock(ethr_rwmutex *);
int ethr_rwmutex_rwunlock(ethr_rwmutex *);
#endif
#endif

#ifndef ETHR_TRY_INLINE_FUNCS
int ethr_atomic_init(ethr_atomic_t *var, long i);
int ethr_atomic_set(ethr_atomic_t *var, long i);
int ethr_atomic_read(ethr_atomic_t *var, long *i);
int ethr_atomic_inctest(ethr_atomic_t *, long *);
int ethr_atomic_dectest(ethr_atomic_t *, long *);
int ethr_atomic_inc(ethr_atomic_t *);
int ethr_atomic_dec(ethr_atomic_t *);
int ethr_atomic_addtest(ethr_atomic_t *, long, long *);
int ethr_atomic_add(ethr_atomic_t *, long);
int ethr_atomic_and_old(ethr_atomic_t *, long, long *);
int ethr_atomic_or_old(ethr_atomic_t *, long, long *);
int ethr_atomic_xchg(ethr_atomic_t *, long, long *);

#if defined(ETHR_PTHREADS)
int ethr_spinlock_init(ethr_spinlock_t *);
int ethr_spinlock_destroy(ethr_spinlock_t *);
int ethr_spin_unlock(ethr_spinlock_t *);
int ethr_spin_lock(ethr_spinlock_t *);

int ethr_rwlock_init(ethr_rwlock_t *);
int ethr_rwlock_destroy(ethr_rwlock_t *);
int ethr_read_unlock(ethr_rwlock_t *);
int ethr_read_lock(ethr_rwlock_t *);
int ethr_write_unlock(ethr_rwlock_t *);
int ethr_write_lock(ethr_rwlock_t *);
#endif
#endif

int ethr_time_now(ethr_timeval *);
int ethr_tsd_key_create(ethr_tsd_key *);
int ethr_tsd_key_delete(ethr_tsd_key);
int ethr_tsd_set(ethr_tsd_key, void *);
void *ethr_tsd_get(ethr_tsd_key);

#ifdef ETHR_HAVE_ETHR_SIG_FUNCS
#include <signal.h>
int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset);
int ethr_sigwait(const sigset_t *set, int *sig);
#endif

#ifdef ETHR_XCHK
int ethr_xchk_have_locked_mutexes(void);
#endif

#endif /* #ifndef ETHREAD_H__ */
