/* $Id$
 * hipe_debug.h
 */
#ifndef HIPE_DEBUG_H
#define HIPE_DEBUG_H

extern void hipe_print_estack(Process *p);
extern void hipe_print_nstack(Process *p);
extern void hipe_print_heap(Process *p);
extern void hipe_print_pcb(Process *p);
extern void hipe_check_heap(Process *p);

#endif /* HIPE_DEBUG_H */
