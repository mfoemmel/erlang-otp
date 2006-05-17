/*
 * Low-level ethread support on x86/x86-64.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_I386_ETHREAD_H
#define ETHREAD_I386_ETHREAD_H

#include "atomic.h"
#include "spinlock.h"
#include "rwlock.h"

#define ETHR_HAVE_NATIVE_ATOMICS 1
#define ETHR_HAVE_NATIVE_LOCKS 1

#endif /* ETHREAD_I386_ETHREAD_H */
