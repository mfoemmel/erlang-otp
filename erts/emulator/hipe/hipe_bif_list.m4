/* $Id$
 *
 * List all non architecture-specific BIFs and primops, and
 * classify each as belonging to one of the classes below.
 * This list is included in hipe_${ARCH}_bifs.m4, which is
 * responsible for translating these classifications to the
 * best possible native code wrappers.
 *
 * XXX: We should have a more detailed BIF classification
 * with a number of orthogonal properties (e.g., UPDATES_HP,
 * NEEDS_NSP, CAN_FAIL, CAN_GC, etc), from which we should
 * generate appropriate interfaces.
 */

/****************************************************************
 *			BIF CLASS DESCRIPTIONS			*
 ****************************************************************/

/*
 * standard_bif_interface_0(nbif_name, cbif_name)
 * standard_bif_interface_1(nbif_name, cbif_name)
 * standard_bif_interface_2(nbif_name, cbif_name)
 * standard_bif_interface_3(nbif_name, cbif_name)
 *
 * A BIF with implicit P parameter, 0-3 ordinary parameters,
 * which may fail but not with RESCHEDULE.
 * Native P state (RA/NSP/HP/etc) may be accessed and updated.
 * [XXX: most will CONS but not access P->hipe]
 * GC may not occur (NSP/HP limits unchanged).
 * [XXX: Some actually do GC. SPARC could be broken.]
 */

/*
 * expensive_bif_interface_1(nbif_name, cbif_name)
 * expensive_bif_interface_2(nbif_name, cbif_name)
 *
 * A BIF which may fail with RESCHEDULE, otherwise
 * identical to standard_bif_interface_N.
 */

/*
 * nofail_primop_interface_0(nbif_name, cbif_name)
 * nofail_primop_interface_1(nbif_name, cbif_name)
 * nofail_primop_interface_2(nbif_name, cbif_name)
 * nofail_primop_interface_3(nbif_name, cbif_name)
 *
 * A primop or guard BIF with no failure mode, otherwise
 * identical to standard_bif_interface_N.
 */

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 1 ordinary parameter,
 * and no failure mode.
 * Native P state (RA/NSP/HP/etc) may be accessed and updated.
 * GC may occur (NSP/HP limits changed).
 */

/*
 * nocons_nofail_primop_interface_0(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_1(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_2(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_3(nbif_name, cbif_name)
 * nocons_nofail_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * Native P state will not be accessed or updated.
 * GC may not occur (NSP/HP limits unchanged).
 */

/*
 * noproc_primop_interface_0(nbif_name, cbif_name)
 * noproc_primop_interface_1(nbif_name, cbif_name)
 * noproc_primop_interface_2(nbif_name, cbif_name)
 * noproc_primop_interface_3(nbif_name, cbif_name)
 * noproc_primop_interface_5(nbif_name, cbif_name)
 *
 * A primop with no P parameter, 0-3 or 5 ordinary parameters,
 * and no failure mode.
 * Native P state will not be accessed or updated.
 * GC may not occur (NSP/HP limits unchanged).
 */

/****************************************************************
 *			BIF CLASSIFICATION			*
 ****************************************************************/

/*
 * BIFs with expensive failure modes.
 */
expensive_bif_interface_1(nbif_demonitor_1, demonitor_1)
expensive_bif_interface_2(nbif_exit_2, exit_2)
expensive_bif_interface_2(nbif_group_leader_2, group_leader_2)
expensive_bif_interface_1(nbif_link_1, link_1)
expensive_bif_interface_2(nbif_monitor_2, monitor_2)
expensive_bif_interface_2(nbif_port_command_2, port_command_2)
expensive_bif_interface_2(nbif_send_2, send_2)
expensive_bif_interface_1(nbif_unlink_1, unlink_1)
expensive_bif_interface_1(nbif_hipe_bifs_test_reschedule_1, hipe_bifs_test_reschedule_1)

/*
 * BIFs that may trigger a native stack walk with p->hipe.narity != 0.
 * Relevant when NR_ARG_REGS < the arity of the BIF.
 */
standard_bif_interface_2(nbif_check_process_code_2, hipe_check_process_code_2)
standard_bif_interface_1(nbif_garbage_collect_1, hipe_garbage_collect_1)
standard_bif_interface_1(nbif_hipe_bifs_show_nstack_1, hipe_show_nstack_1)

/*
 * Arithmetic operators called indirectly by the HiPE compiler.
 */
standard_bif_interface_2(nbif_add_2, erts_mixed_plus)
standard_bif_interface_2(nbif_sub_2, erts_mixed_minus)
standard_bif_interface_2(nbif_mul_2, erts_mixed_times)
standard_bif_interface_2(nbif_div_2, erts_mixed_div)
standard_bif_interface_2(nbif_intdiv_2, intdiv_2)
standard_bif_interface_2(nbif_rem_2, rem_2)
standard_bif_interface_2(nbif_bsl_2, bsl_2)
standard_bif_interface_2(nbif_bsr_2, bsr_2)
standard_bif_interface_2(nbif_band_2, band_2)
standard_bif_interface_2(nbif_bor_2, bor_2)
standard_bif_interface_2(nbif_bxor_2, bxor_2)
standard_bif_interface_1(nbif_bnot_1, bnot_1)

/*
 * Miscellaneous primops.
 */
gc_nofail_primop_interface_1(nbif_gc_1, hipe_gc)
standard_bif_interface_1(nbif_set_timeout, hipe_set_timeout)
standard_bif_interface_1(nbif_conv_big_to_float, hipe_conv_big_to_float)
standard_bif_interface_2(nbif_rethrow, hipe_rethrow)
standard_bif_interface_3(nbif_find_na_or_make_stub, hipe_find_na_or_make_stub)

/*
 * Mbox primops with implicit P parameter.
 */
nocons_nofail_primop_interface_0(nbif_select_msg, hipe_select_msg)

/*
 * Primops without any P parameter.
 * These cannot CONS or gc.
 */
noproc_primop_interface_2(nbif_cmp_2, cmp)
noproc_primop_interface_2(nbif_eq_2, eq)

/*
 * Bit-syntax primops with implicit P parameter.
 * XXX: only get_integer conses on the ordinary heap
 */
nofail_primop_interface_2(nbif_bs_get_integer, erts_bs_get_integer)
nofail_primop_interface_2(nbif_bs_get_binary, erts_bs_get_binary)
nofail_primop_interface_2(nbif_bs_get_float, erts_bs_get_float)
nofail_primop_interface_0(nbif_bs_get_binary_all, erts_bs_get_binary_all)
nofail_primop_interface_0(nbif_bs_final, erts_bs_final)

/*
 * Bit-syntax primops without any P parameter.
 * These cannot CONS or gc.
 */
noproc_primop_interface_1(nbif_bs_allocate, hipe_bs_allocate)

/*
 * Bit-syntax primops. The ERTS_SMP runtime system requires P,
 * hence the use of nocons_nofail_primop_interface_N().
 * When ERTS_SMP is disabled, noproc_primop_interface_N()
 * should be used instead.
 */
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_1(nbif_bs_start_match, hipe_bs_start_match)
nocons_nofail_primop_interface_1(nbif_bs_skip_bits, hipe_bs_skip_bits)
nocons_nofail_primop_interface_0(nbif_bs_skip_bits_all, hipe_bs_skip_bits_all)
nocons_nofail_primop_interface_1(nbif_bs_test_tail, hipe_bs_test_tail)
nocons_nofail_primop_interface_1(nbif_bs_save, hipe_bs_save)
nocons_nofail_primop_interface_1(nbif_bs_restore, hipe_bs_restore)
nocons_nofail_primop_interface_0(nbif_bs_init, hipe_bs_init)
nocons_nofail_primop_interface_3(nbif_bs_put_integer, hipe_bs_put_integer)
nocons_nofail_primop_interface_2(nbif_bs_put_binary, hipe_bs_put_binary)
nocons_nofail_primop_interface_1(nbif_bs_put_binary_all, hipe_bs_put_binary_all)
nocons_nofail_primop_interface_3(nbif_bs_put_float, hipe_bs_put_float)
nocons_nofail_primop_interface_2(nbif_bs_put_string, hipe_bs_put_string)
nocons_nofail_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
nocons_nofail_primop_interface_5(nbif_bs_put_small_float, hipe_bs_put_small_float)
',`
noproc_primop_interface_1(nbif_bs_start_match, erts_bs_start_match)
noproc_primop_interface_1(nbif_bs_skip_bits, erts_bs_skip_bits)
noproc_primop_interface_0(nbif_bs_skip_bits_all, erts_bs_skip_bits_all)
noproc_primop_interface_1(nbif_bs_test_tail, erts_bs_test_tail)
noproc_primop_interface_1(nbif_bs_save, erts_bs_save)
noproc_primop_interface_1(nbif_bs_restore, erts_bs_restore)
noproc_primop_interface_0(nbif_bs_init, erts_bs_init)
noproc_primop_interface_3(nbif_bs_put_integer, erts_bs_put_integer)
noproc_primop_interface_2(nbif_bs_put_binary, erts_bs_put_binary)
noproc_primop_interface_1(nbif_bs_put_binary_all, erts_bs_put_binary_all)
noproc_primop_interface_3(nbif_bs_put_float, erts_bs_put_float)
noproc_primop_interface_2(nbif_bs_put_string, erts_bs_put_string)
noproc_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
noproc_primop_interface_5(nbif_bs_put_small_float, hipe_bs_put_small_float)
')dnl

/*
 * SMP-specific stuff
 */
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_0(nbif_clear_timeout, hipe_clear_timeout)
nocons_nofail_primop_interface_0(nbif_check_get_msg, hipe_check_get_msg)
nocons_nofail_primop_interface_0(nbif_next_msg, hipe_next_msg)
noproc_primop_interface_1(nbif_atomic_inc, hipe_atomic_inc)
',)dnl

/*
 * Standard BIFs.
 * BIF_LIST(ModuleAtom,FunctionAtom,Arity,CFun,Index)
 */
define(BIF_LIST,`standard_bif_interface_$3(nbif_$4, $4)')
include(TARGET/`erl_bif_list.h')

/*
 * Guard BIFs.
 * GBIF_LIST(FunctionAtom,Arity,CFun)
 */
define(GBIF_LIST,`nofail_primop_interface_$2(gbif_$3, $3)')
include(`hipe/hipe_gbif_list.h')
