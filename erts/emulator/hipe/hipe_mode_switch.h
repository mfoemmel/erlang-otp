/* $Id$
 * hipe_mode_switch.h
 */
#ifndef HIPE_MODE_SWITCH_H
#define HIPE_MODE_SWITCH_H

/* command codes for beam_emu -> hipe_mode_switch() call */
#define HIPE_MODE_SWITCH_CMD_CALL		0
#define HIPE_MODE_SWITCH_CMD_RETURN		1
#define HIPE_MODE_SWITCH_CMD_THROW		2
#define HIPE_MODE_SWITCH_CMD_RESUME		3
#define HIPE_MODE_SWITCH_CMD_RESCHEDULE		4

/* result codes for beam_emu <- hipe_mode_switch() return */
#define HIPE_MODE_SWITCH_RES_RETURN		5
#define HIPE_MODE_SWITCH_RES_CALL		6
#define HIPE_MODE_SWITCH_RES_THROW		7

/* additional result codes for hipe_mode_switch() <- native return */
#define HIPE_MODE_SWITCH_RES_SUSPEND		8
#define HIPE_MODE_SWITCH_RES_WAIT		9
#define HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT	10
#define HIPE_MODE_SWITCH_RES_RESCHEDULE		11
#define HIPE_MODE_SWITCH_RES_TRAP		12

#define HIPE_MODE_SWITCH_CMD_CALL_CLOSURE	13 /* BEAM -> mode_switch */
#define HIPE_MODE_SWITCH_RES_CALL_CLOSURE	14 /* mode_switch <- native */

#define HIPE_MODE_SWITCH_RES_APPLY		15 /* mode_switch <- native */

#ifndef ASM

#include "error.h"

int hipe_modeswitch_debug;

void hipe_mode_switch_init(void);
void hipe_set_call_trap(Uint *bfun, void *nfun, int is_closure);
Process *hipe_mode_switch(Process*, unsigned, Eterm*);
void hipe_inc_nstack(Process *p);
void hipe_set_closure_stub(ErlFunEntry *fe, unsigned num_free);
Eterm hipe_build_stacktrace(Process *p, struct StackTrace *s);

extern Uint hipe_beam_pc_return[];
extern Uint hipe_beam_pc_throw[];
extern Uint hipe_beam_pc_resume[];

#endif	/* ASM */

#endif /* HIPE_MODE_SWITCH_H */
