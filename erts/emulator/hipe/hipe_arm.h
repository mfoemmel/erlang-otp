/* $Id$
 */
#ifndef HIPE_ARM_H
#define HIPE_ARM_H

extern void hipe_flush_icache_word(void *address);
extern void hipe_flush_icache_range(void *address, unsigned int nbytes);

/* for stack descriptor hash lookup */
#define HIPE_RA_LSR_COUNT	2	/* low 2 bits are always zero */

/* for hipe_bifs_{read,write}_{s,u}32 */
static __inline__ int hipe_word32_address_ok(void *address)
{
    return ((unsigned long)address & 0x3) == 0;
}

/* Used when a BIF can trigger a stack walk. */
static __inline__ void hipe_set_narity(Process *p, unsigned int arity)
{
    /* XXX: for now; later we should have sufficient # of args in regs */
    p->hipe.narity = arity;
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#define hipe_arch_name	am_arm

/* for hipe_bifs_enter_code_2 */
extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
#define HIPE_ALLOC_CODE(n,c,t,p) hipe_alloc_code((n),(c),(t),(p))

extern void hipe_arm_inc_stack(void);

#endif /* HIPE_ARM_H */
