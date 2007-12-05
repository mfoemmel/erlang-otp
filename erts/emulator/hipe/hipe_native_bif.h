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
AEXTERN(Eterm,nbif_nonclosure_address,(void));

AEXTERN(Eterm,nbif_add_2,(void));
AEXTERN(Eterm,nbif_sub_2,(void));
AEXTERN(Eterm,nbif_mul_2,(void));

AEXTERN(Eterm,nbif_conv_big_to_float,(void));


AEXTERN(int,nbif_bs_put_big_integer,(void));
AEXTERN(int,nbif_bs_put_small_float,(void));
AEXTERN(void,nbif_bs_put_bits,(void));
AEXTERN(Eterm,nbif_bs_get_integer_2,(void));
AEXTERN(Eterm,nbif_bs_get_float_2,(void));
AEXTERN(Eterm,nbif_bs_get_binary_2,(void));
AEXTERN(char*,nbif_bs_allocate,(void));
AEXTERN(Binary*,nbif_bs_reallocate,(void));

AEXTERN(void,nbif_select_msg,(Process*));
AEXTERN(Eterm,nbif_cmp_2,(void));
AEXTERN(Eterm,nbif_eq_2,(void));

Eterm hipe_nonclosure_address(Process*, Eterm, Uint);
Eterm hipe_conv_big_to_float(Process*, Eterm);
void hipe_select_msg(Process*);
void hipe_gc(Process*, Eterm);
Eterm hipe_set_timeout(Process*, Eterm);
void hipe_handle_exception(Process*);
Eterm hipe_rethrow(Process *c_p, Eterm exc, Eterm value);
char *hipe_bs_allocate(int);
Binary *hipe_bs_reallocate(Binary*, int);
int hipe_bs_put_small_float(Process*, Eterm, Uint, byte*, unsigned, unsigned);
void hipe_bs_put_bits(Eterm, Uint, byte*, unsigned, unsigned);

/*
 * Stuff that is different in SMP and non-SMP.
 */
#ifdef ERTS_SMP
int hipe_bs_put_big_integer(Process*, Eterm, Uint, byte*, unsigned, unsigned);
#else
int hipe_bs_put_big_integer(Eterm, Uint, byte*, unsigned, unsigned);
#endif

AEXTERN(Eterm,nbif_check_get_msg,(Process*));
Eterm hipe_check_get_msg(Process*);

/*
 * SMP-specific stuff
 */
#ifdef ERTS_SMP
AEXTERN(void,nbif_atomic_inc,(void));
AEXTERN(void,nbif_clear_timeout,(Process*));
void hipe_atomic_inc(int*);
void hipe_clear_timeout(Process*);
#endif

#define BIF_LIST(M,F,A,C,I)	AEXTERN(Eterm,nbif_##C,(void));
#include "erl_bif_list.h"
#undef BIF_LIST

#endif	/* HIPE_NATIVE_BIF_H */
