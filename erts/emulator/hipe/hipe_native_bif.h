/* $Id$
 * hipe_native_bif.h
 */
 
#ifndef HIPE_NATIVE_BIF_H
#define HIPE_NATIVE_BIF_H

#include "hipe_arch.h"

/*
 * Prototypes for entry points used by native code.
 */
AEXTERN(Eterm,nbif_callemu,(void));
AEXTERN(int,nbif_suspend_0,(void));	/* caller ignores retval */
AEXTERN(int,nbif_suspend_msg,(void));
AEXTERN(int,nbif_suspend_msg_timeout,(void));

AEXTERN(Eterm,nbif_rethrow,(Process*, Eterm, Eterm));
AEXTERN(Eterm,nbif_set_timeout,(Process*, Eterm));

AEXTERN(Eterm,nbif_gc_1,(void));

AEXTERN(Eterm,nbif_apply,(void));
AEXTERN(Eterm,nbif_find_na_or_make_stub,(void));

AEXTERN(Eterm,nbif_add_2,(void));
AEXTERN(Eterm,nbif_sub_2,(void));
AEXTERN(Eterm,nbif_mul_2,(void));

AEXTERN(Eterm,nbif_conv_big_to_float,(void));

AEXTERN(void,nbif_bs_put_string,(void));
AEXTERN(void,nbif_bs_init,(void));
AEXTERN(int,nbif_bs_start_match,(void));
AEXTERN(int,nbif_bs_put_binary_all,(void));
AEXTERN(int,nbif_bs_put_binary,(void));
AEXTERN(int,nbif_bs_put_float,(void));
AEXTERN(int,nbif_bs_put_integer,(void));
AEXTERN(int,nbif_bs_put_big_integer,(void));
AEXTERN(int,nbif_bs_put_small_float,(void));
AEXTERN(int,nbif_bs_skip_bits_all,(void));
AEXTERN(int,nbif_bs_skip_bits,(void));
AEXTERN(Eterm,nbif_bs_get_integer,(void));
AEXTERN(Eterm,nbif_bs_get_float,(void));
AEXTERN(Eterm,nbif_bs_get_binary,(void));
AEXTERN(Eterm,nbif_bs_get_integer_2,(void));
AEXTERN(Eterm,nbif_bs_get_float_2,(void));
AEXTERN(Eterm,nbif_bs_get_binary_2,(void));
AEXTERN(Eterm,nbif_bs_get_binary_all,(void));
AEXTERN(int,nbif_bs_test_tail,(void));
AEXTERN(void,nbif_bs_restore,(void));
AEXTERN(void,nbif_bs_save,(void));
AEXTERN(Eterm,nbif_bs_final,(void));
AEXTERN(char*,nbif_bs_allocate,(void));

AEXTERN(void,nbif_select_msg,(Process*));
AEXTERN(Eterm,nbif_cmp_2,(void));
AEXTERN(Eterm,nbif_eq_2,(void));

Eterm hipe_conv_big_to_float(Process*, Eterm);
void hipe_select_msg(Process*);
void hipe_gc(Process*, Eterm);
Eterm hipe_set_timeout(Process*, Eterm);
void hipe_handle_exception(Process*);
Eterm hipe_rethrow(Process *c_p, Eterm exc, Eterm value);
char *hipe_bs_allocate(int);
int hipe_bs_put_small_float(Process*, Eterm, Uint, byte*, unsigned, unsigned);

/*
 * Stuff that is different in SMP and non-SMP.
 */
#ifdef ERTS_SMP
int hipe_bs_put_big_integer(Process*, Eterm, Uint, byte*, unsigned, unsigned);
#else
int hipe_bs_put_big_integer(Eterm, Uint, byte*, unsigned, unsigned);
#endif

/*
 * SMP-specific stuff
 */
#ifdef ERTS_SMP
AEXTERN(void,nbif_atomic_inc,(void));
AEXTERN(void,nbif_clear_timeout,(Process*));
AEXTERN(Eterm,nbif_check_get_msg,(Process*));
AEXTERN(void,nbif_next_msg,(Process*));
void hipe_atomic_inc(int*);
void hipe_clear_timeout(Process*);
Eterm hipe_check_get_msg(Process*);
void hipe_next_msg(Process*);
int hipe_bs_start_match(Process *p, Eterm Bin);
int hipe_bs_skip_bits(Process *p, Uint num_bits);
int hipe_bs_skip_bits_all(Process *p);
int hipe_bs_test_tail(Process *p, Uint num_bits);
void hipe_bs_save(Process *p, int index);
void hipe_bs_restore(Process *p, int index);
void hipe_bs_init(Process *p);
int hipe_bs_put_integer(Process *p, Eterm Integer, Uint num_bits, unsigned flags);
int hipe_bs_put_binary(Process *p, Eterm Bin, Uint num_bits);
int hipe_bs_put_binary_all(Process *p, Eterm Bin);
void hipe_bs_put_string(Process *p, byte* iptr, Uint num_bytes);
#endif

#define BIF_LIST(M,F,A,C,I)	AEXTERN(Eterm,nbif_##C,(void));
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */
