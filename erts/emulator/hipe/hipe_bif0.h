/* $Id$
 * hipe_bif0.h
 *
 * Compiler and linker support.
 */
#ifndef HIPE_BIF0_H
#define HIPE_BIF0_H

extern int hipe_patch_address(Uint *address, Eterm patchtype, Uint value);

extern Uint *hipe_bifs_find_pc_from_mfa(Eterm mfa);

/* shared with ggc.c -- NOT an official API */
extern Eterm *hipe_constants_start;
extern Eterm *hipe_constants_next;

#include "hash.h"
struct sdesc {	/* XXX: we depend on hipe_${ARCH}_loader.erl here */
    HashBucket bucket;	/* word 0: hash link, word 1: return address (key) */
    unsigned long altra;
    unsigned long exnra;
    unsigned int fsize;
    unsigned int arity;
    unsigned int nskip;
    unsigned int skip[1];	/* actually `nskip' words */
};
extern void hipe_init_sdesc_table(struct sdesc*);
extern struct sdesc *hipe_find_sdesc(unsigned long ra);

#endif /* HIPE_BIF0_H */
