/*
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

#define BeamOpCode(Op)	((uint32)BeamOp(Op))

BIF_RETTYPE hipe_bifs_call_count_on_1(BIF_ALIST_1)
BIF_ADECL_1
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
    hcc = safe_alloc(sizeof(*hcc));
    hcc->count = 0;
    hcc->opcode = pc[0];
    pc[-4] = (Eterm)hcc;
    pc[0] = BeamOpCode(op_hipe_call_count);
    BIF_RET(am_true);
}

BIF_RETTYPE hipe_bifs_call_count_off_1(BIF_ALIST_1)
BIF_ADECL_1
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
    sys_free(hcc);
    BIF_RET(make_small(count));
}

BIF_RETTYPE hipe_bifs_call_count_get_1(BIF_ALIST_1)
BIF_ADECL_1
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
BIF_ADECL_1
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

BIF_RETTYPE hipe_bifs_system_timer_get_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    uint min;
    uint sec;
    uint milli;
    uint micro;
    hrtime_t tmp;
    Eterm *hp;

    hp = HAlloc(BIF_P, 5);
    tmp = (sys_gethrtime() - sys_time)/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    BIF_RET(TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro)));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_system_timer_clear_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    sys_time = sys_gethrtime();
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_gc_timer_get_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    uint min;
    uint sec;
    uint milli;
    uint micro;
    hrtime_t tmp;
    Eterm *hp;
    Eterm major, minor, max_maj, max_min;

    hp = HAlloc(BIF_P, 5*5);
    tmp = major_gc_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    major = TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro));
    hp += 5;
    tmp = minor_gc_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    minor = TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro));
    hp += 5;
    tmp = max_major_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    max_maj = TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro));
    hp += 5;
    tmp = max_minor_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    max_min = TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro));
    hp += 5;
    BIF_RET(TUPLE4(hp,major,minor,max_maj,max_min));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_gc_timer_clear_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    major_gc_time = 0;
    minor_gc_time = 0;
    max_major_time = 0;
    max_minor_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_send_timer_get_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    uint min;
    uint sec;
    uint milli;
    uint micro;
    hrtime_t tmp;
    Eterm *hp;

    hp = HAlloc(BIF_P, 5);
    tmp = send_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    BIF_RET(TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro)));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_send_timer_clear_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    send_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_copy_timer_get_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    uint min;
    uint sec;
    uint milli;
    uint micro;
    hrtime_t tmp;
    Eterm *hp;

    hp = HAlloc(BIF_P, 5);
    tmp = copy_time/1000;
    micro = tmp % 1000;
    tmp /= 1000;
    milli = tmp % 1000;
    tmp /= 1000;
    sec = tmp % 60;
    min = tmp / 60;
    BIF_RET(TUPLE4(hp,make_small(min),
                      make_small(sec),
                      make_small(milli),
                      make_small(micro)));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_copy_timer_clear_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    copy_time = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_gc_info_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    Eterm *hp;

    hp = HAlloc(BIF_P, 7);
    BIF_RET(TUPLE6(hp,make_small((uint)major_garbage_cols),
                      make_small((uint)minor_garbage_cols),
                      make_small((uint)live_major_sum),
                      make_small((uint)live_minor_sum),
                      make_small((uint)ptrs_to_old),
                      make_small((uint)ptrs_to_young)));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_clear_gc_info_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    major_garbage_cols = 0;
    minor_garbage_cols = 0;
    live_major_sum = 0;
    live_minor_sum = 0;
    ptrs_to_old = 0;
    ptrs_to_young = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_bench_info_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    Eterm *hp;
    uint32 p_index, number = 0;

    for(p_index = 0; p_index < max_process; p_index++)
    {
        Process * p = process_tab[p_index];
        if(p == NULL)
            continue;
        number++;
    }
    hp = HAlloc(BIF_P, 5);
    BIF_RET(TUPLE4(hp,make_small(number),
                      make_small((uint)messages_sent),
                      make_small(biggest_heap_size_ever),
                      make_small(max_allocated_heap)));
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_clear_bench_info_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    int i;

    messages_sent = 0;
    biggest_heap_size_ever = 0;
    max_allocated_heap = 0;
    for (i=0; i<1000; i++)
        message_sizes[i] = 0;
    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }

BIF_RETTYPE hipe_bifs_message_info_0(BIF_ALIST_0)
BIF_ADECL_0
{
#ifdef BENCH_STAT
    int i,j, max = 0;
    int tmp[12] = {0,0,0,0,0,0,0,0,0,0,0,0};
    for (i=0; i<65; i++)
    {
        tmp[0] += message_sizes[i];
        if (tmp[0] > max) max = tmp[0];
    }
    for (i=65; i<999; i++)
    {
        tmp[i/100 + 1] += message_sizes[i];
        if (tmp[i/100 + 1] > max) max = tmp[i/100 + 1];
    }
    tmp[11] = message_sizes[999];
    if (tmp[11] > max) max = tmp[11];
    for (i=-1; i<11; i++)
    {
        int num = (tmp[i+1] * 50) / max;
        if (i == -1)
            printf("\n\r  0 -  64: (%6d) |",tmp[0]);
        else if (i == 0)
            printf("\n\r 65 -  99: (%6d) |",tmp[1]);
        else if (i == 10)
            printf("\n\r  >= 1000: (%6d) |",tmp[11]);
        else
            printf("\n\r%3d - %3d: (%6d) |",i*100,i*100+99,tmp[i+1]);
        for (j=0; j<num; j++) printf(".");
    }
    printf("\n\r");

    BIF_RET(am_true);
#else
    BIF_RET(am_false);
#endif
    }
