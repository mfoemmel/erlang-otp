/* $Id$
 */
#ifndef HIPE_PRIMOPS_H
#define HIPE_PRIMOPS_H

PRIMOP_LIST(am_erl_fp_exception, (int*)&erl_fp_exception) /* ignore volatile */
#if !defined(ERTS_SMP)
PRIMOP_LIST(am_erts_mb, &erts_mb)
#endif
#ifdef SHARED_HEAP
PRIMOP_LIST(am_erts_global_mso, &erts_global_offheap)
#endif
PRIMOP_LIST(am_suspend_msg, &nbif_suspend_msg)
PRIMOP_LIST(am_suspend_msg_timeout, &nbif_suspend_msg_timeout)
PRIMOP_LIST(am_suspend_0, &nbif_suspend_0)

PRIMOP_LIST(am_Plus, &nbif_add_2)
PRIMOP_LIST(am_Minus, &nbif_sub_2)
PRIMOP_LIST(am_Times, &nbif_mul_2)
PRIMOP_LIST(am_Div, &nbif_div_2)
PRIMOP_LIST(am_div, &nbif_intdiv_2)
PRIMOP_LIST(am_rem, &nbif_rem_2)
PRIMOP_LIST(am_bsl, &nbif_bsl_2)
PRIMOP_LIST(am_bsr, &nbif_bsr_2)
PRIMOP_LIST(am_band, &nbif_band_2)
PRIMOP_LIST(am_bor, &nbif_bor_2)
PRIMOP_LIST(am_bxor, &nbif_bxor_2)
PRIMOP_LIST(am_bnot, &nbif_bnot_1)

PRIMOP_LIST(am_gc_1, &nbif_gc_1)
#ifdef ERTS_SMP
PRIMOP_LIST(am_atomic_inc, &nbif_atomic_inc)
PRIMOP_LIST(am_clear_timeout, &nbif_clear_timeout)
PRIMOP_LIST(am_check_get_msg, &nbif_check_get_msg)
PRIMOP_LIST(am_next_msg, &nbif_next_msg)
#endif
PRIMOP_LIST(am_select_msg, &nbif_select_msg)
PRIMOP_LIST(am_set_timeout, &nbif_set_timeout)
PRIMOP_LIST(am_rethrow, &nbif_rethrow)

PRIMOP_LIST(am_bs_init, &nbif_bs_init)
PRIMOP_LIST(am_bs_final, &nbif_bs_final)
PRIMOP_LIST(am_bs_start_match, &nbif_bs_start_match)
PRIMOP_LIST(am_bs_get_integer, &nbif_bs_get_integer)
PRIMOP_LIST(am_bs_get_float, &nbif_bs_get_float)
PRIMOP_LIST(am_bs_get_binary, &nbif_bs_get_binary)
PRIMOP_LIST(am_bs_get_binary_all, &nbif_bs_get_binary_all)
PRIMOP_LIST(am_bs_skip_bits, &nbif_bs_skip_bits)
PRIMOP_LIST(am_bs_skip_bits_all, &nbif_bs_skip_bits_all)
PRIMOP_LIST(am_bs_test_tail, &nbif_bs_test_tail)
PRIMOP_LIST(am_bs_save, &nbif_bs_save)
PRIMOP_LIST(am_bs_restore, &nbif_bs_restore)
PRIMOP_LIST(am_bs_put_integer, &nbif_bs_put_integer)
PRIMOP_LIST(am_bs_put_binary, &nbif_bs_put_binary)
PRIMOP_LIST(am_bs_put_binary_all, &nbif_bs_put_binary_all)
PRIMOP_LIST(am_bs_put_float, &nbif_bs_put_float)
PRIMOP_LIST(am_bs_put_string, &nbif_bs_put_string)
PRIMOP_LIST(am_bs_allocate, &nbif_bs_allocate)
PRIMOP_LIST(am_bs_put_big_integer, &nbif_bs_put_big_integer)
PRIMOP_LIST(am_bs_put_small_float, &nbif_bs_put_small_float)

PRIMOP_LIST(am_cmp_2, &nbif_cmp_2)
PRIMOP_LIST(am_op_exact_eqeq_2, &nbif_eq_2)

PRIMOP_LIST(am_hipe_apply, &nbif_apply)
PRIMOP_LIST(am_find_na_or_make_stub, &nbif_find_na_or_make_stub)

PRIMOP_LIST(am_conv_big_to_float, &nbif_conv_big_to_float)

#if defined(__sparc__)
#include "hipe_sparc_primops.h"
#endif
#if defined(__i386__)
#include "hipe_x86_primops.h"
#endif
#if defined(__x86_64__)
#include "hipe_amd64_primops.h"
#endif
#if defined(__powerpc__) || defined(__ppc__) || defined(__powerpc64__)
#include "hipe_ppc_primops.h"
#endif
#if defined(__arm__)
#include "hipe_arm_primops.h"
#endif

#endif /* HIPE_PRIMOPS_H */
