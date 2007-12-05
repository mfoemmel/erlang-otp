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
 * The Initial Developer of the Original Code is Ericsson AB.
 * Portions created by Ericsson are Copyright 2006, Ericsson AB.
 * All Rights Reserved.''
 * 
 *     $Id$
 */

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

#ifdef ETHR_TRY_INLINE_FUNCS

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

#endif /* ETHR_TRY_INLINE_FUNCS */

#endif /* ETHREAD_SPARC32_RWLOCK_H */
