/* $Id$
 */
#ifndef HIPE_PPC_H
#define HIPE_PPC_H

static __inline__ void hipe_flush_icache_word(void *address)
{
    asm volatile("dcbst 0,%0\n"
		 "\tsync\n"
		 "\ticbi 0,%0\n"
		 "\tsync\n"
		 "\tisync"
		 :
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
    /* XXX: for now; later we should have sufficient # of args in regs on ppc */
    p->hipe.narity = arity;
}

/* Native stack growth direction. */
#define HIPE_NSTACK_GROWS_DOWN

#if defined(__powerpc64__)
#define hipe_arch_name	am_ppc64
#define AEXTERN(RET,NAME,PROTO)	extern const int NAME
AEXTERN(void,hipe_ppc_inc_stack,(void));
#else
#define hipe_arch_name	am_powerpc
extern void hipe_ppc_inc_stack(void); /* we don't have the AEXTERN() fallback :-( */
#endif

/* for hipe_bifs_enter_code_2 */
extern void *hipe_alloc_code(Uint nrbytes, Eterm callees, Eterm *trampolines, Process *p);
#define HIPE_ALLOC_CODE(n,c,t,p) hipe_alloc_code((n),(c),(t),(p))

#if !defined(__powerpc64__)
extern const unsigned int fconv_constant[];
#endif

#endif /* HIPE_PPC_H */
