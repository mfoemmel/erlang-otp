/* $Id$
 */
#ifndef HIPE_AMD64_H
#define HIPE_AMD64_H

#include "hipe_x86.h"
#undef hipe_arch_name

/* for hipe_bifs_{read,write}_{s,u}64 */
static __inline__ int hipe_word64_address_ok(void *address)
{
    return 1;
}

#define hipe_arch_name	am_amd64

extern const Uint sse2_fnegate_mask[];

#endif /* HIPE_AMD64_H */
