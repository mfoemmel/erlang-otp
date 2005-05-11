/* $Id$
 */
#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"

#include "hipe_arch.h"
#include "hipe_native_bif.h"	/* nbif_callemu() */

#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"

extern void nbif_inc_stack_0(void);
extern void nbif_handle_fp_exception(void);

/* called from hipe_bif0.c:hipe_bifs_primop_address_1() */
const void *hipe_arch_primop_address(Eterm key)
{
    switch( key ) {
      case am_inc_stack_0: return nbif_inc_stack_0;
      case am_handle_fp_exception: return nbif_handle_fp_exception;
      default: return NULL;
    }
}

void hipe_patch_load_fe(Uint32 *address, Uint32 value)
{
    /* address points to a disp32 or imm32 operand */
    *address = value;
}

/* called from hipe_bif0.c:hipe_bifs_make_native_stub_2()
   and hipe_bif0.c:hipe_make_stub() */
void *hipe_make_native_stub(void *beamAddress, unsigned int beamArity)
{
    /*
     * This creates a native code stub with the following contents:
     *
     * movl $Address, P_BEAM_IP(%ebp)
     * movb $Arity, P_ARITY(%ebp)
     * jmp callemu
     *
     * The stub has variable size, depending on whether the P_BEAM_IP
     * and P_ARITY offsets fit in 8-bit signed displacements or not.
     * The rel32 offset in the final jmp depends on its actual location,
     * which also depends on the size of the previous instructions.
     * Arity is stored with a movb because (a) Björn tells me arities
     * are <= 255, and (b) a movb is smaller and faster than a movl.
     */
    unsigned int codeSize;
    unsigned char *code, *codep;
    unsigned int callEmuOffset;

    codeSize =	/* 16, 19, or 22 bytes */
	16 +	/* 16 when both offsets are 8-bit */
	(P_BEAM_IP >= 128 ? 3 : 0) +
	(P_ARITY >= 128 ? 3 : 0);
    codep = code = erts_alloc(ERTS_ALC_T_HIPE, codeSize);

    /* movl $beamAddress, P_BEAM_IP(%ebp); 3 or 6 bytes, plus 4 */
    codep[0] = 0xc7;
#if P_BEAM_IP >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] = P_BEAM_IP & 0xFF;
    codep[3] = (P_BEAM_IP >> 8) & 0xFF;
    codep[4] = (P_BEAM_IP >> 16) & 0xFF;
    codep[5] = (P_BEAM_IP >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] = P_BEAM_IP;
    codep += 3;
#endif
    codep[0] = ((unsigned int)beamAddress) & 0xFF;
    codep[1] = ((unsigned int)beamAddress >> 8) & 0xFF;
    codep[2] = ((unsigned int)beamAddress >> 16) & 0xFF;
    codep[3] = ((unsigned int)beamAddress >> 24) & 0xFF;
    codep += 4;

    /* movb $beamArity, P_ARITY(%ebp); 3 or 6 bytes */
    codep[0] = 0xc6;
#if P_ARITY >= 128
    codep[1] = 0x85;	/* disp32[EBP] */
    codep[2] = P_ARITY & 0xFF;
    codep[3] = (P_ARITY >> 8) & 0xFF;
    codep[4] = (P_ARITY >> 16) & 0xFF;
    codep[5] = (P_ARITY >> 24) & 0xFF;
    codep += 6;
#else
    codep[1] = 0x45;	/* disp8[EBP] */
    codep[2] = P_ARITY;
    codep += 3;
#endif
    codep[0] = beamArity;
    codep += 1;

    /* jmp callemu; 5 bytes */
    callEmuOffset = (unsigned char*)nbif_callemu - (code + codeSize);
    codep[0] = 0xe9;
    codep[1] = callEmuOffset & 0xFF;
    codep[2] = (callEmuOffset >> 8) & 0xFF;
    codep[3] = (callEmuOffset >> 16) & 0xFF;
    codep[4] = (callEmuOffset >> 24) & 0xFF;
    codep += 5;
    ASSERT(codep == code + codeSize);

    /* I-cache flush? */

    return code;
}

void hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%08x |            |\r\n", offsetof(struct hipe_process_state,x), n, (unsigned)p->x)
    U("ncsp       ", ncsp);
    U("narity     ", narity);
#undef U
}
