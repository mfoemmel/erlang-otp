/* $Id$
 * hipe_native_bif.h
 */
 
#ifndef HIPE_NATIVE_BIF_H
#define HIPE_NATIVE_BIF_H

/*
 * Prototypes for entry points used by native code.
 */
extern Eterm nbif_callemu(void);
extern int nbif_suspend_0(void);	/* caller ignores retval */
extern int nbif_suspend_msg(void);
extern int nbif_suspend_msg_timeout(void);

extern Eterm nbif_rethrow(Process*, Eterm, Eterm);
extern Eterm nbif_set_timeout(Process*, Eterm);
extern void hipe_select_msg(Process*);

extern Eterm nbif_gc_1(void);

extern Eterm nbif_apply(void);
extern Eterm nbif_find_na_or_make_stub(void);

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
char *nbif_bs_allocate(void);

void nbif_select_msg(Process*);
Eterm nbif_cmp_2(void);
Eterm nbif_eq_2(void);

void hipe_gc(Process*, Eterm);
Eterm hipe_set_timeout(Process*, Eterm);
void hipe_handle_exception(Process*);
Eterm hipe_rethrow(Process *c_p, Eterm exc, Eterm value);
char *hipe_bs_allocate(int);
int hipe_bs_put_big_integer(Eterm, Uint, byte*, unsigned, unsigned);
int hipe_bs_put_small_float(Eterm, Uint, byte*, unsigned, unsigned);
Eterm hipe_conv_big_to_float(Process*, Eterm);

#define BIF_LIST(M,F,A,C,I)	Eterm nbif_##C(void);
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */

