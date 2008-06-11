/* $Id$
 */
#ifndef HIPE_ARM_GLUE_H
#define HIPE_ARM_GLUE_H

#include "hipe_arm_asm.h"		/* for NR_ARG_REGS, ARM_LEAF_WORDS */
#define NR_LEAF_WORDS			ARM_LEAF_WORDS
#define HIPE_ARCH_CALL_TO_NATIVE	hipe_arm_call_to_native
#define HIPE_ARCH_RETURN_TO_NATIVE	hipe_arm_return_to_native
#define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_arm_tailcall_to_native
#define HIPE_ARCH_THROW_TO_NATIVE	hipe_arm_throw_to_native
#include "hipe_risc_glue.h"

#endif /* HIPE_ARM_GLUE_H */
