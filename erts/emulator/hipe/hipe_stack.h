/* $Id$
 */
#ifndef HIPE_STACK_H
#define HIPE_STACK_H

extern void hipe_find_handler(Process*);

#ifdef __i386__
extern void hipe_clean_nstack(Process*);
#else
#define hipe_clean_nstack(p)	do{}while(0)
#endif

#endif /* HIPE_STACK_H */
