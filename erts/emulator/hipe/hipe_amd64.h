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

/* for hipe_bifs_enter_code_2 */
extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
#define HIPE_ALLOC_CODE(n,c,t,p) hipe_alloc_code((n),(c),(t),(p))

#endif /* HIPE_AMD64_H */
