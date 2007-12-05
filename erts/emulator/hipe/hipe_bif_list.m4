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
 *
 * The classification is expressed in terms of the resources
 * and BIF failure modes described below.
 *
 * Resources:
 * - NSP: native stack pointer
 *   NSP is read by GC BIFs and primops, and hipe_handle_exception().
 *   NSP is updated at compiler-inserted calls to hipe_inc_nstack().
 *   No other BIF or primop may access NSP.
 * - NSP_LIMIT: native stack limit
 *   NSP_LIMIT is only updated at compiler-inserted calls to inc_stack.
 *   Everywhere else, the cached value equals the value stored in P.
 * - NRA: native return address
 *   NRA is read by GC BIFs and primops, and hipe_handle_exception().
 *   No BIF or primop may update NRA.
 * - HP: heap pointer
 *   All BIFs can read and update HP.
 *   Primops with access to P that do not access HP are called "nocons".
 * - HP_LIMIT: heap limit
 *   HP_LIMIT is only updated by GC BIFs and primops.
 *   Everywhere else, the cached value equals the value stored in P.
 * - FCALLS: reduction counter
 *   All BIFs can read and update FCALLS (because BEAM abuses FCALLS
 *   to trigger GCs). XXX: can we avoid that overhead?
 *   All nocons primops do not access FCALLS.
 *   All other primops with access to P can read and update FCALLS.
 * - P: pointer to the state record for the process
 *
 * BIF failure modes:
 * - none: zero-arity BIFs may not signal exceptions
 *   The BIF wrapper needs no checks before returning.
 * - standard: may signal any exception except RESCHEDULE
 *   The BIF wrapper must check for an exception before returning.
 * - expensive: may signal any exception including RESCHEDULE
 *   The BIF wrapper must preserve the actual parameters before
 *   calling the C code. After the C code returns, the BIF wrapper
 *   must check for an exception and be prepared to supply the
 *   actual parameters and its own start address to the handler.
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
 * HP and FCALLS may be read and updated.
 * HP_LIMIT, NSP, NSP_LIMIT, and NRA may not be accessed.
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
 * gc_bif_interface_0(nbif_name, cbif_name)
 * gc_bif_interface_1(nbif_name, cbif_name)
 * gc_bif_interface_2(nbif_name, cbif_name)
 *
 * A BIF which may do a GC or walk the native stack.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
 * Otherwise identical to standard_bif_interface_N.
 */

/*
 * expensive_gc_bif_interface_1(nbif_name, cbif_name)
 * expensive_gc_bif_interface_2(nbif_name, cbif_name)
 *
 * A BIF which may fail with RESCHEDULE, otherwise
 * identical to gc_bif_interface_N.
 */

/*
 * gc_nofail_primop_interface_1(nbif_name, cbif_name)
 *
 * A primop with implicit P parameter, 1 ordinary parameter,
 * and no failure mode.
 * May read NSP, NSP_LIMIT, NRA, HP, HP_LIMIT, and FCALLS.
 * May update HP, HP_LIMIT, and FCALLS.
 * May not update NSP, NSP_LIMIT, or NRA.
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
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
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
 * HP, HP_LIMIT, FCALLS, NSP, NSP_LIMIT, and NRA may not be accessed.
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
expensive_bif_interface_2(nbif_process_info_1, process_info_1)
expensive_bif_interface_2(nbif_process_info_2, process_info_2)
expensive_bif_interface_2(nbif_send_2, send_2)
expensive_bif_interface_2(nbif_send_3, send_3)
expensive_bif_interface_2(nbif_setnode_3, setnode_3)
expensive_bif_interface_2(nbif_suspend_process_2, suspend_process_2)
expensive_bif_interface_2(nbif_system_flag_2, system_flag_2)
expensive_bif_interface_1(nbif_unlink_1, unlink_1)
expensive_bif_interface_2(nbif_erts_debug_set_internal_state_2, erts_debug_set_internal_state_2)

/*
 * BIFs and primops that may do a GC (change heap limit and walk the native stack).
 */
expensive_gc_bif_interface_2(nbif_check_process_code_2, hipe_check_process_code_2)
gc_bif_interface_0(nbif_garbage_collect_0, garbage_collect_0)
expensive_gc_bif_interface_1(nbif_garbage_collect_1, hipe_garbage_collect_1)
gc_nofail_primop_interface_1(nbif_gc_1, hipe_gc)

/*
 * Debug BIFs that need read access to the full state.
 * hipe_bifs:nstack_used_size/0 only needs read access to NSP.
 * They are classified as GC BIFs for simplicity.
 */
gc_bif_interface_1(nbif_hipe_bifs_show_nstack_1, hipe_show_nstack_1)
gc_bif_interface_1(nbif_hipe_bifs_show_pcb_1, hipe_bifs_show_pcb_1)
gc_bif_interface_0(nbif_hipe_bifs_nstack_used_size_0, hipe_bifs_nstack_used_size_0)

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
standard_bif_interface_1(nbif_set_timeout, hipe_set_timeout)
standard_bif_interface_1(nbif_conv_big_to_float, hipe_conv_big_to_float)
standard_bif_interface_2(nbif_rethrow, hipe_rethrow)
standard_bif_interface_3(nbif_find_na_or_make_stub, hipe_find_na_or_make_stub)
standard_bif_interface_2(nbif_nonclosure_address, hipe_nonclosure_address)

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
 * XXX: all of the _2 versions cons on the ordinary heap
 * XXX: all of them can cons and thus update FCALLS
 */
nofail_primop_interface_3(nbif_bs_get_integer_2, erts_bs_get_integer_2)
nofail_primop_interface_3(nbif_bs_get_binary_2, erts_bs_get_binary_2)
nofail_primop_interface_3(nbif_bs_get_float_2, erts_bs_get_float_2)

/*
 * Bit-syntax primops without any P parameter.
 * These cannot CONS or gc.
 */
noproc_primop_interface_1(nbif_bs_allocate, hipe_bs_allocate)
noproc_primop_interface_2(nbif_bs_reallocate, hipe_bs_reallocate)

/*
 * Bit-syntax primops. The ERTS_SMP runtime system requires P,
 * hence the use of nocons_nofail_primop_interface_N().
 * When ERTS_SMP is disabled, noproc_primop_interface_N()
 * should be used instead.
 */
nocons_nofail_primop_interface_5(nbif_bs_put_small_float, hipe_bs_put_small_float)
noproc_primop_interface_5(nbif_bs_put_bits, hipe_bs_put_bits)
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
',`
noproc_primop_interface_5(nbif_bs_put_big_integer, hipe_bs_put_big_integer)
')dnl

gc_bif_interface_0(nbif_check_get_msg, hipe_check_get_msg)

/*
 * SMP-specific stuff
 */
ifelse(ERTS_SMP,1,`
nocons_nofail_primop_interface_0(nbif_clear_timeout, hipe_clear_timeout)
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
