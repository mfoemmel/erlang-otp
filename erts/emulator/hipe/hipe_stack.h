/* $Id$
 */
#ifndef HIPE_STACK_H
#define HIPE_STACK_H

/* XXX: Shouldn't the following also start with hipe_ ??? */
extern void nbif_stack_trap_ra(void);

extern void hipe_print_nstack(Process*);
extern void hipe_find_handler(Process*);
extern void (*hipe_handle_stack_trap(Process*))(void);
extern void hipe_update_stack_trap(Process*, const struct sdesc*);

#ifdef __i386__
extern Eterm hipe_x86_check_process_code_2(Process*, Eterm, Eterm);
extern Eterm hipe_x86_garbage_collect_1(Process*, Eterm);
#endif

#endif /* HIPE_STACK_H */
