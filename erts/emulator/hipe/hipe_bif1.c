/* $Id$
 * hipe_bif1.c
 *
 * Performance analysis support.
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "global.h"
#include "bif.h"
#include "big.h"
#include "error.h"
#include "beam_load.h"
#include "hipe_bif0.h"
#include "hipe_bif1.h"

#define BeamOpCode(Op)	((Uint)BeamOp(Op))

BIF_RETTYPE hipe_bifs_call_count_on_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if( pc[0] == BeamOpCode(op_hipe_trap_call) )
	BIF_ERROR(BIF_P, BADARG);
    if( pc[0] == BeamOpCode(op_hipe_call_count) )
	BIF_RET(NIL);
    hcc = erts_alloc(ERTS_ALC_T_HIPE, sizeof(*hcc));
    hcc->count = 0;
    hcc->opcode = pc[0];
    pc[-4] = (Eterm)hcc;
    pc[0] = BeamOpCode(op_hipe_call_count);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_call_count_off_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if( pc[0] != BeamOpCode(op_hipe_call_count) )
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    count = hcc->count;
    pc[0] = hcc->opcode;
    pc[-4] = (Eterm)NULL;
    erts_free(ERTS_ALC_T_HIPE, hcc);
    BIF_RET(make_small(count));
}

BIF_RETTYPE hipe_bifs_call_count_get_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if( pc[0] != BeamOpCode(op_hipe_call_count) )
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    BIF_RET(make_small(hcc->count));
}

BIF_RETTYPE hipe_bifs_call_count_clear_1(BIF_ALIST_1)
{
    Eterm *pc;
    struct hipe_call_count *hcc;
    unsigned count;

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    ASSERT(pc[-5] == BeamOpCode(op_i_func_info_IaaI));
    if( pc[0] != BeamOpCode(op_hipe_call_count) )
	BIF_RET(am_false);
    hcc = (struct hipe_call_count*)pc[-4];
    count = hcc->count;
    hcc->count = 0;
    BIF_RET(make_small(count));
}

/*****************************************************************************
 * BIFs for benchmarking. These only do usefull things if __BENCHMARK__
 * is defined in beam/benchmark.h
 * If benchmarking is not enabled all BIFs will return false. If the required
 * benchmark feature is not enabled, the counter will remain zero.
 *
 * process_info/0 -> { Number of live processes,
 *                     Processes spawned }
 *
 * process_info_clear/0 -> true
 *
 * message_info/0 -> { Messages sent,
 *                     Messages copied,
 *                     Ego messages (sender = receiver),
 *                     Words sent,
 *                     Words copied }
 *
 * message_info_clear/0 -> true
 *
 * message_sizes/0 -> true
 *                    This BIF displays a bar diagram with message sizes
 *
 * gc_info/0 -> { Minor collections,
 *                Major collections,
 *                Used heap,
 *                Allocated heap,
 *                Max used heap,
 *                Max allocated heap }
 *
 * shared_gc_info/0 -> { Minor collections of the shared heap,
 *                       Major collections of the shared heap,
 *                       Used shared heap,
 *                       Allocated shared heap,
 *                       Max used shared heap,
 *                       Max allocated shared heap }
 *
 * gc_info_clear/0 -> true
 *
 *
 * All timers returns tuples of the kind: { Minutes, Seconds, Milliseconds }
 * except for the max times in garbage collection where times are normally
 * small. The tuple is therefor: { Seconds, Milliseconds, Microseconds }
 *
 * system_timer/0 -> Mutator time
 *
 * system_timer_clear/0 -> true
 *
 * send_timer/0 -> { Send time,
 *                   Copy time,
 *                   Size time }
 *
 * send_timer_clear/0 -> true
 *
 * gc_timer/0 -> { Time in minor collection,          Time spent in GC
 *                 Time in major collection,
 *                 Max time in minor collection (us),
 *                 Max time in major collection (us) }
 *
 * shared_gc_timer/0 -> { Time in minor collection,   Time spent in GC
 *                        Time in major collection,   of the shared heap
 *                        Max time in minor collection (us),
 *                        Max time in major collection (us) }
 *
 * gc_timer_clear/0 -> true
 *
 */

BIF_RETTYPE hipe_bifs_process_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifndef BM_COUNTERS
    Uint processes_busy    = 0;
    Uint processes_spawned = 0;
#endif
    Eterm *hp;

    hp = HAlloc(BIF_P, 3);
    BIF_RET(TUPLE2(hp,make_small(processes_busy),
                      make_small(processes_spawned)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_process_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifdef BM_COUNTERS
    processes_spawned = 0;
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
#ifndef BM_COUNTERS
    unsigned long messages_sent   = 0;
    unsigned long messages_copied = 0;
    unsigned long messages_ego    = 0;
#endif
#ifndef BM_MESSAGE_SIZES
    unsigned long words_sent   = 0;
    unsigned long words_copied = 0;
#endif

    hp = HAlloc(BIF_P, 6);
    BIF_RET(TUPLE5(hp,make_small(messages_sent),
                      make_small(messages_copied),
                      make_small(messages_ego),
                      make_small(words_sent),
                      make_small(words_copied)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifdef BM_COUNTERS
    messages_sent   = 0;
    messages_copied = 0;
    messages_ego    = 0;
#endif
#ifdef BM_MESSAGE_SIZES
    words_sent   = 0;
    words_copied = 0;
    {
        int i;
        for (i = 0; i < 1000; i++)
            message_sizes[i] = 0;
    }
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_message_sizes_0(BIF_ALIST_0)
{
#ifdef BM_MESSAGE_SIZES
    int i, j, max = 0;
    int tmp[12] = {0,0,0,0,0,0,0,0,0,0,0,0};

    for (i = 0; i < 65; i++)
    {
        tmp[0] += message_sizes[i];
        if (tmp[0] > max)
            max = tmp[0];
    }
    for (i = 65; i < 999; i++)
    {
        tmp[i / 100 + 1] += message_sizes[i];
        if (tmp[i / 100 + 1] > max)
            max = tmp[i / 100 + 1];
    }
    tmp[11] = message_sizes[999];
    if (tmp[11] > max)
        max = tmp[11];
    for (i = -1; i < 11; i++)
    {
        int num = (tmp[i + 1] * 50) / max;
        if (i == -1)
            printf("\n\r  0 -  64: (%6d) |",tmp[0]);
        else if (i == 0)
            printf("\n\r 65 -  99: (%6d) |",tmp[1]);
        else if (i == 10)
            printf("\n\r  >= 1000: (%6d) |",tmp[11]);
        else
            printf("\n\r%3d - %3d: (%6d) |",i * 100,i * 100 + 99,
                   tmp[i + 1]);

        for (j = 0; j < num; j++)
            printf(".");
    }
    printf("\n\r");

    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifndef BM_COUNTERS
    Uint minor_garbage_cols = 0;
    Uint major_garbage_cols = 0;
#endif
#ifndef BM_HEAP_SIZES
    Uint max_used_heap      = 0;
    Uint max_allocated_heap = 0;
#endif
    Eterm *hp;

    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,make_small((uint)minor_garbage_cols),
                      make_small((uint)major_garbage_cols),
                      make_small((Uint)((BIF_P->htop - BIF_P->heap) +
                                        (OLD_HTOP(BIF_P) - OLD_HEAP(BIF_P)) +
                                        MBUF_SIZE(BIF_P))),
                      make_small((Uint)((BIF_P->hend - BIF_P->heap) +
                                        (OLD_HEND(BIF_P) - OLD_HEAP(BIF_P)) +
                                        MBUF_SIZE(BIF_P))),
                      make_small(max_used_heap),
                      make_small(max_allocated_heap)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_shared_gc_info_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifndef BM_COUNTERS
    Uint minor_global_garbage_cols = 0;
    Uint major_global_garbage_cols = 0;
#endif
#ifndef BM_HEAP_SIZES
    Uint max_used_global_heap      = 0;
    Uint max_allocated_global_heap = 0;
#endif
    Eterm *hp;

    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,make_small((uint)minor_global_garbage_cols),
                      make_small((uint)major_global_garbage_cols),
                      make_small((Uint)((BIF_P->htop - BIF_P->heap) +
                                        (OLD_HTOP(BIF_P) - OLD_HEAP(BIF_P)) +
                                        MBUF_SIZE(BIF_P))),
                      make_small((Uint)((BIF_P->hend - BIF_P->heap) +
                                        (OLD_HEND(BIF_P) - OLD_HEAP(BIF_P)) +
                                        MBUF_SIZE(BIF_P))),
                      make_small(max_used_global_heap),
                      make_small(max_allocated_global_heap)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_info_clear_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
#ifdef BM_COUNTERS
    minor_garbage_cols        = 0;
    major_garbage_cols        = 0;
    minor_global_garbage_cols = 0;
    major_global_garbage_cols = 0;
#endif
#ifdef BM_HEAP_SIZES
    max_used_heap             = 0;
    max_allocated_heap        = 0;
    max_used_global_heap      = 0;
    max_allocated_global_heap = 0;
#endif
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

#ifdef BM_TIMERS
#if defined(__i386__) && USE_PERFCTR
#define MAKE_TIME(_timer_) {                          \
    BM_TIMER_T tmp = _timer_;                         \
    milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    tmp /= 1000;                                      \
    sec = (uint)(tmp - ((int)(tmp / 60)) * 60);       \
    min = (uint)tmp / 60;                             }

#define MAKE_MICRO_TIME(_timer_) {                    \
    BM_TIMER_T tmp = _timer_ * 1000;                  \
    micro = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    tmp /= 1000;                                      \
    milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    sec = (uint)tmp / 1000;                           }

#else
#define MAKE_TIME(_timer_) {          \
    BM_TIMER_T tmp = _timer_/1000000; \
    milli = tmp % 1000;               \
    tmp /= 1000;                      \
    sec = tmp % 60;                   \
    min = tmp / 60;                   }

#define MAKE_MICRO_TIME(_timer_) {    \
    BM_TIMER_T tmp = _timer_/1000;    \
    micro = tmp % 1000;               \
    tmp /= 1000;                      \
    milli = tmp % 1000;               \
    sec = tmp / 1000;                 }

#endif
#else
#define MAKE_TIME(_timer_)
#define MAKE_MICRO_TIME(_timer_)
#endif

BIF_RETTYPE hipe_bifs_system_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    Eterm *hp;

    hp = HAlloc(BIF_P, 4);
    MAKE_TIME(system_time);
    BIF_RET(TUPLE3(hp,make_small(min),
                      make_small(sec),
                      make_small(milli)));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_system_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    system_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_send_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    uint min   = 0;
    uint sec   = 0;
    uint milli = 0;
    Eterm *hp;
    Eterm sendtime, copytime, sizetime;

    hp = HAlloc(BIF_P, 4 * 4);

    MAKE_TIME(send_time);
    sendtime = TUPLE3(hp,make_small(min),
                         make_small(sec),
                         make_small(milli));
    hp += 4;

    MAKE_TIME(copy_time);
    copytime = TUPLE3(hp,make_small(min),
                         make_small(sec),
                         make_small(milli));
    hp += 4;

    MAKE_TIME(size_time);
    sizetime = TUPLE3(hp,make_small(min),
                         make_small(sec),
                         make_small(milli));
    hp += 4;
    BIF_RET(TUPLE3(hp,sendtime,copytime,sizetime));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_send_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    send_time = 0;
    copy_time = 0;
    size_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    uint micro = 0;
    Eterm minor, major, max_min, max_maj;

    hp = HAlloc(BIF_P, 4 * 4 + 5);

    MAKE_TIME(minor_gc_time);
    minor = TUPLE3(hp,make_small(min),
                      make_small(sec),
                      make_small(milli));
    hp += 4;

    MAKE_TIME(major_gc_time);
    major = TUPLE3(hp,make_small(min),
                      make_small(sec),
                      make_small(milli));
    hp += 4;

    MAKE_MICRO_TIME(max_minor_time);
    max_min = TUPLE3(hp,make_small(sec),
                        make_small(milli),
                        make_small(micro));
    hp += 4;

    MAKE_MICRO_TIME(max_major_time);
    max_maj = TUPLE3(hp,make_small(sec),
                        make_small(milli),
                        make_small(micro));
    hp += 4;

    BIF_RET(TUPLE4(hp,minor,major,max_min,max_maj));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_shared_gc_timer_0(BIF_ALIST_0)
{
#ifdef __BENCHMARK__
    Eterm *hp;
    uint min = 0;
    uint sec = 0;
    uint milli = 0;
    uint micro = 0;
    Eterm minor, major, max_min, max_maj;

    hp = HAlloc(BIF_P, 4 * 4 + 5);

    MAKE_TIME(minor_global_gc_time);
    minor = TUPLE3(hp,make_small(min),
                      make_small(sec),
                      make_small(milli));
    hp += 4;

    MAKE_TIME(major_global_gc_time);
    major = TUPLE3(hp,make_small(min),
                      make_small(sec),
                      make_small(milli));
    hp += 4;

    MAKE_MICRO_TIME(max_global_minor_time);
    max_min = TUPLE3(hp,make_small(sec),
                        make_small(milli),
                        make_small(micro));
    hp += 4;

    MAKE_MICRO_TIME(max_global_major_time);
    max_maj = TUPLE3(hp,make_small(sec),
                        make_small(milli),
                        make_small(micro));
    hp += 4;

    BIF_RET(TUPLE4(hp,minor,major,max_min,max_maj));
#else
    BIF_RET(am_false);
#endif
}

BIF_RETTYPE hipe_bifs_gc_timer_clear_0(BIF_ALIST_0)
{
#ifdef BM_TIMERS
    minor_gc_time         = 0;
    major_gc_time         = 0;
    max_minor_time        = 0;
    max_major_time        = 0;
    minor_global_gc_time  = 0;
    major_global_gc_time  = 0;
    max_global_minor_time = 0;
    max_global_major_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
}

#undef MAKE_TIME
#undef MAKE_MICRO_TIME


/*
 * HiPE hrvtime().
 * These implementations are currently available:
 * + On Linux/x86 with the perfctr driver we can use the process'
 *   virtualised time-stamp counter. To enable this mode you must
 *   pass `--with-perfctr=/path/to/perfctr' when configuring.
 * + The fallback, which is the same as {X,_} = runtime(statistics).
 */

#if defined(__i386__) && USE_PERFCTR

/*
 * This uses the Linux/x86 perfctr driver to virtualise the
 * x86 time-stamp counter.
 */
#include "libperfctr.h"
static struct vperfctr *vperfctr;
static double cpu_khz;
#define hrvtime_is_started()	(vperfctr != NULL)

static void start_hrvtime(void)
{
    struct perfctr_info info;
    struct vperfctr_control control;

    if( vperfctr != NULL )
	return;
    vperfctr = vperfctr_open();
    if( vperfctr == NULL )
	return;
    if( vperfctr_info(vperfctr, &info) >= 0 ) {
	cpu_khz = (double)info.cpu_khz;
	if( info.cpu_features & PERFCTR_FEATURE_RDTSC ) {
	    memset(&control, 0, sizeof control);
	    control.cpu_control.tsc_on = 1;
	    if( vperfctr_control(vperfctr, &control) >= 0 )
		return;
	}
    }
    vperfctr_close(vperfctr);
    vperfctr = NULL;
}

static double get_hrvtime(void)
{
    unsigned long long ticks;
    double milli_seconds;

    ticks = vperfctr_read_tsc(vperfctr);
    milli_seconds = (double)ticks / cpu_khz;
    return milli_seconds;
}

static void stop_hrvtime(void)
{
    if( vperfctr ) {
	vperfctr_stop(vperfctr);
	vperfctr_close(vperfctr);
	vperfctr = NULL;
    }
}

#else

/*
 * Fallback, if nothing better exists.
 * This is the same as {X,_} = statistics(runtime), which uses
 * times(2) on Unix systems.
 */

#define hrvtime_is_started()	1
#define start_hrvtime()		do{}while(0)
#define stop_hrvtime()		do{}while(0)

static double get_hrvtime(void)
{
    unsigned long ms_user;
    elapsed_time_both(&ms_user, NULL, NULL, NULL);
    return (double)ms_user;
}

#endif	/* hrvtime support */

BIF_RETTYPE hipe_bifs_get_hrvtime_0(BIF_ALIST_0)
{
    Eterm *hp;
    Eterm res;
    FloatDef f;

    if( !hrvtime_is_started() ) {
	start_hrvtime();
	if( !hrvtime_is_started() )
	    BIF_ERROR(BIF_P, BADARG);
    }
    f.fd = get_hrvtime();
    hp = HAlloc(BIF_P, 3);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

BIF_RETTYPE hipe_bifs_stop_hrvtime_0(BIF_ALIST_0)
{
    stop_hrvtime();
    BIF_RET(am_true);
}
