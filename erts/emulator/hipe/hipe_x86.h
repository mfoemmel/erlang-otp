/* $Id$
 */
#ifndef HIPE_X86_H
#define HIPE_X86_H

static __inline__ void hipe_flush_icache_word(void *address)
{
    /* Do nothing. This works as long as compiled code is
       executed by a single CPU thread. */
}

static __inline__ void
hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    /* Do nothing. This works as long as compiled code is
       executed by a single CPU thread. */
}

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	0	/* all bits are significant */

/* for hipe_bifs_{read,write}_{s,u}32 */
static __inline__ int hipe_word32_address_ok(void *address)
{
    return 1;
}

/* Used when a BIF can trigger a stack walk. */
static __inline__ void hipe_set_narity(Process *p, unsigned int arity)
{
    p->hipe.narity = arity;
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#define hipe_arch_name	am_x86

#endif /* HIPE_X86_H */
