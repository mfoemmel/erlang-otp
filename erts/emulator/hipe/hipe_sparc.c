/* $Id$
 */
#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */

extern void nbif_inc_stack_0args(void);
extern void nbif_inc_stack_1args(void);
extern void nbif_inc_stack_2args(void);
extern void nbif_inc_stack_3args(void);
extern void nbif_inc_stack_4args(void);
extern void nbif_inc_stack_5args(void);
extern void nbif_inc_stack_6args(void);
extern void nbif_inc_stack_7args(void);
extern void nbif_inc_stack_8args(void);
extern void nbif_inc_stack_9args(void);
extern void nbif_inc_stack_10args(void);
extern void nbif_inc_stack_11args(void);
extern void nbif_inc_stack_12args(void);
extern void nbif_inc_stack_13args(void);
extern void nbif_inc_stack_14args(void);
extern void nbif_inc_stack_15args(void);
extern void nbif_inc_stack_16args(void);

/* Flush dcache and invalidate icache for a range of addresses. */
void hipe_flush_icache_range(void *address, unsigned int nbytes)
{
    char *a = (char*)address;
    int n = nbytes;

    while( n > 0 ) {
	hipe_flush_icache_word(a);
	a += 4;
	n -= 4;
    }
}

/* called from hipe_bif0.c:hipe_bifs_primop_address_1() */
void *hipe_arch_primop_address(Eterm key)
{
    switch( key ) {
      case am_inc_stack_0args_0: return nbif_inc_stack_0args;
      case am_inc_stack_1args_0: return nbif_inc_stack_1args;
      case am_inc_stack_2args_0: return nbif_inc_stack_2args;
      case am_inc_stack_3args_0: return nbif_inc_stack_3args;
      case am_inc_stack_4args_0: return nbif_inc_stack_4args;
      case am_inc_stack_5args_0: return nbif_inc_stack_5args;
      case am_inc_stack_6args_0: return nbif_inc_stack_6args;
      case am_inc_stack_7args_0: return nbif_inc_stack_7args;
      case am_inc_stack_8args_0: return nbif_inc_stack_8args;
      case am_inc_stack_9args_0: return nbif_inc_stack_9args;
      case am_inc_stack_10args_0: return nbif_inc_stack_10args;
      case am_inc_stack_11args_0: return nbif_inc_stack_11args;
      case am_inc_stack_12args_0: return nbif_inc_stack_12args;
      case am_inc_stack_13args_0: return nbif_inc_stack_13args;
      case am_inc_stack_14args_0: return nbif_inc_stack_14args;
      case am_inc_stack_15args_0: return nbif_inc_stack_15args;
      case am_inc_stack_16args_0: return nbif_inc_stack_16args;
      default: return NULL;
    }
}

static void patch_sethi(Uint32 *address, unsigned int imm22)
{
    unsigned int insn = *address;
    *address = (insn & 0xFFC00000) | (imm22 & 0x003FFFFF);
    hipe_flush_icache_word(address);
}

static void patch_ori(Uint32 *address, unsigned int imm10)
{
    /* address points to an OR reg,imm,reg insn */
    unsigned int insn = *address;
    *address = (insn & 0xFFFFE000) | (imm10 & 0x3FF);
    hipe_flush_icache_word(address);
}

void hipe_patch_load_fe(Uint32 *address, Uint32 value)
{
    patch_sethi(address, value >> 10);
    patch_ori(address+1, value);
}

/* called from hipe_bif0.c:hipe_bifs_make_native_stub_2()
   and hipe_bif0.c:hipe_make_stub() */
void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity)
{
    unsigned int *code;
    unsigned int callEmuOffset;
    int i;
    
    code = erts_alloc(ERTS_ALC_T_HIPE, 5*sizeof(int));

    /* sethi %hi(Address), %g1 */
    code[0] = 0x03000000 | (((unsigned int)beamAddress >> 10) & 0x3FFFFF);
    /* or %g0, %o7, %l6 ! mov %o7, %l6 */
    code[1] = 0xAC10000F;
    /* or %g1, %lo(Address), %g1 */
    code[2] = 0x82106000 | ((unsigned int)beamAddress & 0x3FF);
    /* call callemu */
    callEmuOffset = (char*)nbif_callemu - (char*)&code[3];
    code[3] = (1 << 30) | ((callEmuOffset >> 2) & 0x3FFFFFFF);
    /* or %g0, Arity, %l7 ! mov Arity, %l7 */
    code[4] = 0xAE102000 | (beamArity & 0x0FFF);

    /* flush I-cache as if by write_u32() */
    for(i = 0; i < 5; ++i)
	hipe_flush_icache_word(&code[i]);

    return code;
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%08x |            |\r\n", offsetof(struct hipe_process_state,x), n, (unsigned)p->x)
    U("nra        ", nra);
    U("ncra       ", ncra);
#undef U
}
