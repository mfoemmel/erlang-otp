/* $Id$
 */
#ifndef HIPE_GC_H
#define HIPE_GC_H

#if defined(__sparc__)
#include "hipe_sparc_gc.h"
#endif
#if defined(__i386__)
#include "hipe_x86_gc.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64_gc.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_gc.h"
#endif

#endif /* HIPE_GC_H */
