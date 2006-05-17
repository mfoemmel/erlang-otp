/*
 * Native ethread atomics on x86/x86-64.
 * Author: Mikael Pettersson.
 *
 * This code requires a 486 or newer processor.
 */
#ifndef ETHREAD_I386_ATOMIC_H
#define ETHREAD_I386_ATOMIC_H

/* An atomic is an aligned long accessed via locked operations.
 */
typedef struct {
    volatile long counter;
} ethr_native_atomic_t;

#ifdef __x86_64__
#define LONG_SUFFIX "q"
#else
#define LONG_SUFFIX "l"
#endif

static ETHR_INLINE void
ethr_native_atomic_init(ethr_native_atomic_t *var, long i)
{
    var->counter = i;
}
#define ethr_native_atomic_set(v, i)	ethr_native_atomic_init((v), (i))

static ETHR_INLINE long
ethr_native_atomic_read(ethr_native_atomic_t *var)
{
    return var->counter;
}

static ETHR_INLINE void
ethr_native_atomic_add(ethr_native_atomic_t *var, long incr)
{
    __asm__ __volatile__(
       "lock; add" LONG_SUFFIX " %1, %0"
       : "=m"(var->counter)
       : "ir"(incr), "m"(var->counter));
}      
       
static ETHR_INLINE void
ethr_native_atomic_inc(ethr_native_atomic_t *var)
{
    __asm__ __volatile__(
	"lock; inc" LONG_SUFFIX " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE void
ethr_native_atomic_dec(ethr_native_atomic_t *var)
{
    __asm__ __volatile__(
	"lock; dec" LONG_SUFFIX " %0"
	: "=m"(var->counter)
	: "m"(var->counter));
}

static ETHR_INLINE long
ethr_native_atomic_add_return(ethr_native_atomic_t *var, long incr)
{
    long tmp;

    tmp = incr;
    __asm__ __volatile__(
	"lock; xadd" LONG_SUFFIX " %0, %1" /* xadd didn't exist prior to the 486 */
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */
    return tmp + incr;
}

#define ethr_native_atomic_inc_return(var) ethr_native_atomic_add_return((var), 1)
#define ethr_native_atomic_dec_return(var) ethr_native_atomic_add_return((var), -1)

static ETHR_INLINE long
ethr_native_atomic_and_retold(ethr_native_atomic_t *var, long mask)
{
    long tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
	__asm__ __volatile__(
	    "lock; cmpxchg" LONG_SUFFIX " %2, %3"
	    : "=a"(tmp), "=m"(var->counter)
	    : "r"(old & mask), "m"(var->counter), "0"(old));
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE long
ethr_native_atomic_or_retold(ethr_native_atomic_t *var, long mask)
{
    long tmp, old;

    tmp = var->counter;
    do {
	old = tmp;
	__asm__ __volatile__(
	    "lock; cmpxchg" LONG_SUFFIX " %2, %3"
	    : "=a"(tmp), "=m"(var->counter)
	    : "r"(old | mask), "m"(var->counter), "0"(old));
    } while (__builtin_expect(tmp != old, 0));
    /* now tmp is the atomic's previous value */
    return tmp;
}

static ETHR_INLINE long
ethr_native_atomic_xchg(ethr_native_atomic_t *var, long val)
{   
    long tmp = val;
    __asm__ __volatile__(
	"xchg" LONG_SUFFIX " %0, %1"
	: "=r"(tmp)
	: "m"(var->counter), "0"(tmp));
    /* now tmp is the atomic's previous value */ 
    return tmp;
} 

#undef LONG_SUFFIX

#endif /* ETHREAD_I386_ATOMIC_H */
