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
AEXTERN(Eterm,nbif_bs_get_binary_all,(void));
AEXTERN(int,nbif_bs_test_tail,(void));
AEXTERN(void,nbif_bs_restore,(void));
AEXTERN(void,nbif_bs_save,(void));
AEXTERN(Eterm,nbif_bs_final,(void));
AEXTERN(char*,nbif_bs_allocate,(void));

AEXTERN(void,nbif_select_msg,(Process*));
AEXTERN(Eterm,nbif_cmp_2,(void));
AEXTERN(Eterm,nbif_eq_2,(void));

void hipe_select_msg(Process*);
void hipe_gc(Process*, Eterm);
Eterm hipe_set_timeout(Process*, Eterm);
void hipe_handle_exception(Process*);
Eterm hipe_rethrow(Process *c_p, Eterm exc, Eterm value);
char *hipe_bs_allocate(int);
int hipe_bs_put_big_integer(Eterm, Uint, byte*, unsigned, unsigned);
int hipe_bs_put_small_float(Eterm, Uint, byte*, unsigned, unsigned);
Eterm hipe_conv_big_to_float(Process*, Eterm);

#define BIF_LIST(M,F,A,C,I)	AEXTERN(Eterm,nbif_##C,(void));
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */
