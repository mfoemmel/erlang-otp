/*
 * Low-level ethread support on PowerPC.
 * Author: Mikael Pettersson.
 */
#ifndef ETHREAD_PPC32_ETHREAD_H
#define ETHREAD_PPC32_ETHREAD_H

#include "atomic.h"
#include "spinlock.h"
#include "rwlock.h"

#define ETHR_HAVE_NATIVE_ATOMICS 1
#define ETHR_HAVE_NATIVE_LOCKS 1

#endif /* ETHREAD_PPC32_ETHREAD_H */
