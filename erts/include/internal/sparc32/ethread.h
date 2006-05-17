/*
 * Low-level ethread support on SPARC V9.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_SPARC32_ETHREAD_H
#define ETHREAD_SPARC32_ETHREAD_H

#include "atomic.h"
#include "spinlock.h"
#include "rwlock.h"

#define ETHR_HAVE_NATIVE_ATOMICS 1
#define ETHR_HAVE_NATIVE_LOCKS 1

#endif /* ETHREAD_SPARC32_ETHREAD_H */
