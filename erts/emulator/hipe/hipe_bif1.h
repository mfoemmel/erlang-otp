/* $Id$
 * hipe_bif1.h
 *
 * Performance analysis support.
 */
#ifndef HIPE_BIF1_H
#define HIPE_BIF1_H

struct hipe_call_count {
    unsigned count;
    Uint opcode;
};

extern unsigned int hipe_trap_count;

#endif /* HIPE_BIF1_H */
