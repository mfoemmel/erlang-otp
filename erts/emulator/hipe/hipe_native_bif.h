/* $Id$
 * hipe_native_bif.h
 */
 
#ifndef HIPE_NATIVE_BIF_H
#define HIPE_NATIVE_BIF_H

/*
 * Prototypes for entry points used by native code.
 */
uint32 nbif_callemu(void);
uint32 nbif_suspend_0(void);
uint32 nbif_suspend_msg(void);
uint32 nbif_suspend_msg_timeout(void);

Eterm nbif_set_timeout(Process*, Eterm);
Eterm nbif_clear_timeout(Process*);
int hipe_mbox_empty(Process*);
Eterm hipe_get_msg(Process*);
void hipe_next_msg(Process*);
void hipe_select_msg(Process*);

Eterm nbif_inc_stack_0(void);
Eterm nbif_gc_1(void);

Eterm nbif_add_2(void);
Eterm nbif_sub_2(void);
Eterm nbif_mul_2(void);
Eterm nbif_div_2(void);
Eterm nbif_intdiv_2(void);
Eterm nbif_rem_2(void);
Eterm nbif_bsl_2(void);
Eterm nbif_bsr_2(void);
Eterm nbif_band_2(void);
Eterm nbif_bor_2(void);
Eterm nbif_bxor_2(void);
Eterm nbif_bnot_1(void);

Eterm nbif_test(void);

void nbif_bs_put_string(void);
void nbif_bs_init(void);
int nbif_bs_start_match(void);
int nbif_bs_put_binary_all(void);
int nbif_bs_put_binary(void);
int nbif_bs_put_float(void);
int nbif_bs_put_integer(void);
int nbif_bs_skip_bits_all(void);
int nbif_bs_skip_bits(void);
Eterm nbif_bs_get_integer(void);
Eterm nbif_bs_get_float(void);
Eterm nbif_bs_get_binary(void);
Eterm nbif_bs_get_binary_all(void);
int nbif_bs_test_tail(void);
void nbif_bs_restore(void);
void nbif_bs_save(void);
Eterm nbif_bs_final(void);

#if defined(__sparc__)
#define nbif_mbox_empty	hipe_mbox_empty
#define nbif_get_msg	hipe_get_msg
#define nbif_next_msg	hipe_next_msg
#define nbif_select_msg	hipe_select_msg
#define nbif_cmp_2	cmp
#define nbif_eq_2	eq
#elif defined(__i386__)
int nbif_mbox_empty(Process*);
Eterm nbif_get_msg(Process*);
void nbif_next_msg(Process*);
void nbif_select_msg(Process*);
Eterm nbif_cmp_2(void);
Eterm nbif_eq_2(void);
#endif

void hipe_handle_exception(Process*);

#define BIF_LIST(M,F,A,C,I)	Eterm nbif_##C(void);
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */
