/* $Id$
 */
#ifndef HIPE_SPARC_H
#define HIPE_SPARC_H

static __inline__ void hipe_flush_icache_word(void *address)
{
    asm volatile("flush %0"
		 : /* no outputs */
		 : "r"(address)
		 : "memory");
}

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
    /* Do nothing. The 'stack arity' is always zero since no
       BIF has arity greater than HIPE_SPARC_ARGS_IN_REGS. */
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_UP

#define hipe_arch_name	am_ultrasparc

extern void nbif_inc_stack_0args(void);
extern void nbif_inc_stack_1args(void);
extern void nbif_inc_stack_2args(void);
extern void nbif_inc_stack_3args(void);
extern void nbif_inc_stack_4args(void);
extern void nbif_inc_stack_5args(void);
extern void nbif_inc_stack_6args(void);

/* for hipe_bifs_enter_code_2 */
extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
#define HIPE_ALLOC_CODE(n,c,t,p) hipe_alloc_code((n),(c),(t),(p))

#endif /* HIPE_SPARC_H */
