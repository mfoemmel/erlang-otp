/* $Id$
 * hipe_signal.h
 *
 * Architecture-specific initialisation of Unix signals.
 */
#ifndef HIPE_SIGNAL_H
#define HIPE_SIGNAL_H

#ifdef __i386__
extern void hipe_signal_init(void);
#else
static __inline__ void hipe_signal_init(void) { }
#endif

#endif /* HIPE_SIGNAL_H */
