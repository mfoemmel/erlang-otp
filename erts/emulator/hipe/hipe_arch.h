/* $Id$
 */
#ifndef HIPE_ARCH_H
#define HIPE_ARCH_H

extern const void *hipe_arch_primop_address(Eterm key);

/* used by beam_load.c:patch(). patchtype == am_load_fe, Value is an ErlFunEntry* */
extern void hipe_patch_address(Uint *address, Eterm patchtype, Uint value);
extern void hipe_patch_load_fe(Uint *address, Uint value);
extern int hipe_patch_insn(void *address, Uint value, Eterm type);
extern int hipe_patch_call(void *callAddress, void *destAddress, void *trampoline);

extern void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity);

#if defined(__sparc__)
#include "hipe_sparc.h"
#endif
#if defined(__i386__)
#include "hipe_x86.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc.h"
#endif

#if !defined(AEXTERN)
#define AEXTERN(RET,NAME,PROTO)	extern RET NAME PROTO
#endif

#endif /* HIPE_ARCH_H */
