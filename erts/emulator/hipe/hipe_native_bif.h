/* $Id$
 * hipe_native_bif.h
 */
 
#ifndef HIPE_NATIVE_BIF_H
#define HIPE_NATIVE_BIF_H

/*
 * Prototypes for entry points used by native code.
 */
Eterm nbif_callemu(void);
int nbif_suspend_0(void);	/* caller ignores retval */
int nbif_suspend_msg(void);
int nbif_suspend_msg_timeout(void);

Eterm nbif_set_timeout(Process*, Eterm);
Eterm nbif_clear_timeout(Process*);
int hipe_mbox_empty(Process*);
Eterm hipe_get_msg(Process*);
void hipe_next_msg(Process*);
void hipe_select_msg(Process*);

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

Eterm nbif_conv_big_to_float(void);
void nbif_handle_fp_exception(void);

void nbif_bs_put_string(void);
void nbif_bs_init(void);
int nbif_bs_start_match(void);
int nbif_bs_put_binary_all(void);
int nbif_bs_put_binary(void);
int nbif_bs_put_float(void);
int nbif_bs_put_integer(void);
int nbif_bs_put_big_integer(void);
int nbif_bs_put_small_float(void);
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
void *nbif_bs_get_matchbuffer(void);
char *nbif_bs_allocate(void);

#if defined(__sparc__)
/* #define nbif_mbox_empty	hipe_mbox_empty
 #define nbif_get_msg	hipe_get_msg
 #define nbif_next_msg	hipe_next_msg
 #define nbif_select_msg	hipe_select_msg */
int nbif_mbox_empty(Process*);
Eterm nbif_get_msg(Process*);
void nbif_next_msg(Process*);
void nbif_select_msg(Process*);
Eterm nbif_cmp_2(void);
Eterm nbif_eq_2(void);
/* #define nbif_cmp_2	cmp
   #define nbif_eq_2	eq  */
void nbif_inc_stack_0args(void);
void nbif_inc_stack_1args(void);
void nbif_inc_stack_2args(void);
void nbif_inc_stack_3args(void);
void nbif_inc_stack_4args(void);
void nbif_inc_stack_5args(void);
void nbif_inc_stack_6args(void);
void nbif_inc_stack_7args(void);
void nbif_inc_stack_8args(void);
void nbif_inc_stack_9args(void);
void nbif_inc_stack_10args(void);
void nbif_inc_stack_11args(void);
void nbif_inc_stack_12args(void);
void nbif_inc_stack_13args(void);
void nbif_inc_stack_14args(void);
void nbif_inc_stack_15args(void);
void nbif_inc_stack_16args(void);
#elif defined(__i386__)
int nbif_mbox_empty(Process*);
Eterm nbif_get_msg(Process*);
void nbif_next_msg(Process*);
void nbif_select_msg(Process*);
Eterm nbif_cmp_2(void);
Eterm nbif_eq_2(void);
Eterm nbif_inc_stack_0(void);
#endif

void hipe_gc(Process*, Eterm);
Eterm hipe_set_timeout(Process*, Eterm);
Eterm hipe_clear_timeout(Process*);
void hipe_handle_exception(Process*);
void *hipe_bs_get_matchbuffer(void);
char *hipe_bs_allocate(int);
int hipe_bs_put_big_integer(Eterm, Uint, byte*, unsigned, unsigned);
int hipe_bs_put_small_float(Eterm, Uint, byte*, unsigned, unsigned);
Eterm hipe_conv_big_to_float(Process*, Eterm);

#define BIF_LIST(M,F,A,C,I)	Eterm nbif_##C(void);
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */

