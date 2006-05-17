/*
 * Native ethread rwlocks on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_SPARC32_RWLOCK_H
#define ETHREAD_SPARC32_RWLOCK_H

/* Unlocked if zero, read-locked if positive, write-locked if -1. */
typedef struct {
    volatile int counter;
} ethr_native_rwlock_t;

static ETHR_INLINE void
ethr_native_rwlock_init(ethr_native_rwlock_t *lock)
{
    lock->counter = 0;
}

static ETHR_INLINE void
ethr_native_read_unlock(ethr_native_rwlock_t *lock)
{
    unsigned int old, new;

    __asm__ __volatile__("membar #LoadLoad|#StoreLoad");
    do {
	old = lock->counter;
	new = old-1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->counter), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
}

static ETHR_INLINE int
ethr_native_read_trylock(ethr_native_rwlock_t *lock)
{
    int old, new;

    do {
	old = lock->counter;
	if (__builtin_expect(old < 0, 0))
	    return 0;
	new = old+1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->counter), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
    __asm__ __volatile__("membar #StoreLoad|#StoreStore");
    return 1;
}

static ETHR_INLINE int
ethr_native_read_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->counter < 0;
}

static ETHR_INLINE void
ethr_native_read_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_read_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("membar #LoadLoad");
	} while (ethr_native_read_is_locked(lock));
   }
}

static ETHR_INLINE void
ethr_native_write_unlock(ethr_native_rwlock_t *lock)
{
    __asm__ __volatile__("membar #LoadStore|#StoreStore");
    lock->counter = 0;
}

static ETHR_INLINE int
ethr_native_write_trylock(ethr_native_rwlock_t *lock)
{
    unsigned int old, new;

    do {
	old = lock->counter;
	if (__builtin_expect(old != 0, 0))
	    return 0;
	new = -1;
	__asm__ __volatile__(
	    "cas [%2], %1, %0"
	    : "=&r"(new)
	    : "r"(old), "r"(&lock->counter), "0"(new)
	    : "memory");
    } while (__builtin_expect(old != new, 0));
    __asm__ __volatile__("membar #StoreLoad|#StoreStore");
    return 1;
}

static ETHR_INLINE int
ethr_native_write_is_locked(ethr_native_rwlock_t *lock)
{
    return lock->counter != 0;
}

static ETHR_INLINE void
ethr_native_write_lock(ethr_native_rwlock_t *lock)
{
    for(;;) {
	if (__builtin_expect(ethr_native_write_trylock(lock) != 0, 1))
	    break;
	do {
	    __asm__ __volatile__("membar #LoadLoad");
	} while (ethr_native_write_is_locked(lock));
   }
}

#endif /* ETHREAD_SPARC32_RWLOCK_H */
