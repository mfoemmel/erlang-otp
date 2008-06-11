/* $Id$
 */
#ifndef HIPE_PPC_GLUE_H
#define HIPE_PPC_GLUE_H

#include "hipe_ppc_asm.h"		/* for NR_ARG_REGS, PPC_LEAF_WORDS */
#define NR_LEAF_WORDS			PPC_LEAF_WORDS
#define HIPE_ARCH_CALL_TO_NATIVE	hipe_ppc_call_to_native
#define HIPE_ARCH_RETURN_TO_NATIVE	hipe_ppc_return_to_native
#define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_ppc_tailcall_to_native
#define HIPE_ARCH_THROW_TO_NATIVE	hipe_ppc_throw_to_native
#include "hipe_risc_glue.h"

#endif /* HIPE_PPC_GLUE_H */
