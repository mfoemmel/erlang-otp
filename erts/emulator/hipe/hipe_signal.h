/* $Id$
 * hipe_signal.h
 *
 * Architecture-specific initialisation of Unix signals.
 */
#ifndef HIPE_SIGNAL_H
#define HIPE_SIGNAL_H

#if defined(__i386__) || defined(__x86_64__)
extern void hipe_signal_init(void);
#else
static __inline__ void hipe_signal_init(void) { }
#endif

#endif /* HIPE_SIGNAL_H */
