/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "benchmark.h"

#ifdef BM_COUNTERS
unsigned long processes_busy;
unsigned long processes_spawned;
unsigned long messages_sent;
unsigned long messages_copied;
unsigned long messages_ego;
unsigned long minor_garbage_cols;
unsigned long major_garbage_cols;
unsigned long minor_global_garbage_cols;
unsigned long major_global_garbage_cols;
unsigned long gc_in_copy;
#endif /* BM_COUNTERS */

#ifdef BM_TIMERS

#if defined(__i386__) && USE_PERFCTR

#include "libperfctr.h"
struct vperfctr *system_clock;
double cpu_khz;

static double get_hrvtime(void)
{
#if defined(__i386__) && USE_PERFCTR
    unsigned long long ticks;
    double milli_seconds;

    ticks = vperfctr_read_tsc(system_clock);
    milli_seconds = (double)ticks / cpu_khz;
    return milli_seconds;
#endif
}

static void stop_hrvtime(void)
{
#if defined(__i386__) && USE_PERFCTR
    if(system_clock)
    {
	vperfctr_stop(system_clock);
	vperfctr_close(system_clock);
	system_clock = NULL;
    }
#endif
}

#else /* not perfctr, asuming Solaris */
#include <time.h>
BM_TIMER_T system_clock;
#endif
#endif /* BM_TIMERS */

BM_NEW_TIMER(timer);
BM_NEW_TIMER(system);
BM_NEW_TIMER(minor_gc);
BM_NEW_TIMER(major_gc);
BM_NEW_TIMER(minor_global_gc);
BM_NEW_TIMER(major_global_gc);
BM_NEW_TIMER(send);
BM_NEW_TIMER(copy);
BM_NEW_TIMER(size);
BM_NEW_TIMER(max_minor);
BM_NEW_TIMER(max_major);
BM_NEW_TIMER(max_global_minor);
BM_NEW_TIMER(max_global_major);
BM_NEW_TIMER(local_in_global);

#ifdef BM_HEAP_SIZES
unsigned long max_used_heap;
unsigned long max_allocated_heap;
unsigned long max_used_global_heap;
unsigned long max_allocated_global_heap;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
unsigned long words_sent;
unsigned long words_copied;
unsigned long message_sizes[1000];
#endif /* BM_MESSAGE_SIZES */

/*****
 * The folowing functoins has to be defined, but they only have contents
 * if sertain keywords are defined.
 */

void init_benchmarking()
{
#ifdef BM_TIMERS
#if defined(__i386__) && USE_PERFCTR
    /* pass `--with-perfctr=/path/to/perfctr' when configuring */
    struct perfctr_info info;
    struct vperfctr_control control;

    system_clock = vperfctr_open();
    if (system_clock != NULL)
    {
        if (vperfctr_info(system_clock,&info) >= 0)
        {
            cpu_khz = (double)info.cpu_khz;
            if (info.cpu_features & PERFCTR_FEATURE_RDTSC)
            {
                memset(&control,0,sizeof control);
                control.cpu_control.tsc_on = 1;
            }
        }
        if (vperfctr_control(system_clock,&control) < 0)
        {
            vperfctr_close(system_clock);
            system_clock = NULL;
        }
    }
    timer_time = 0;
#else
    int i;
    for (i = 0; i < 1000; i++)
    {
        BM_START_TIMER(system);
        BM_STOP_TIMER(system);
    }
    timer_time = system_time / 1000;
#endif
#endif /* BM_TIMERS */

#ifdef BM_COUNTERS
    minor_garbage_cols        = 0;
    major_garbage_cols        = 0;
    minor_global_garbage_cols = 0;
    major_global_garbage_cols = 0;
    gc_in_copy         = 0;
    processes_busy     = 0;
    processes_spawned  = 0;
    messages_sent      = 0;
    messages_copied    = 0;
    messages_ego       = 0;
#endif /* BM_COUNTERS */

#ifdef BM_HEAP_SIZES
    max_used_heap             = 0;
    max_allocated_heap        = 0;
    max_used_global_heap      = 0;
    max_allocated_global_heap = 0;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
    words_sent   = 0;
    words_copied = 0;
    {
        int i;
        for (i = 0; i < 1000; i++)
            message_sizes[i] = 0;
    }
#endif /* BM_MESSAGE_SIZES */
}

void save_statistics()
{
#ifdef BM_STATISTICS
    FILE *file = fopen(BM_STATISTICS_FILE,"a");
    int i = 0;

    if (file)
    {
        fprintf(file,"-------------------------------------------------------------------------\n");
        fprintf(file,"The counters are reset at system start and are sums over the entire node.\n");
        fprintf(file,"You may reset them manually using the BIFs in the module hipe_bifs.\n");
        fprintf(file,"All times are given in milliseconds.\n");
        fprintf(file,"-------------------------------------------------------------------------\n");

        if (!tmp_buf)
        {
            fprintf(file,"Boot problems?\n");
        }
        else
        {
            display(erts_this_node->sysname,CBUF);
            while (tmp_buf[++i] != '\'' && tmp_buf[i] != '\0');
            if (tmp_buf[i] == '\'')
                tmp_buf[i+1] = '\0';
            fprintf(file,"Node: %s\n",tmp_buf);
        }

#ifdef BM_COUNTERS
        fprintf(file,"Number of processes spawned: %ld\n",processes_spawned);
        fprintf(file,"Number of local minor GCs: %ld\n",minor_garbage_cols);
        fprintf(file,"Number of local major GCs: %ld\n",major_garbage_cols);
        fprintf(file,"Number of global minor GCs: %ld\n",minor_global_garbage_cols);
        fprintf(file,"Number of global major GCs: %ld\n",major_global_garbage_cols);
        fprintf(file,"Number of messages sent: %ld\n",messages_sent);
        fprintf(file,"Number of messages copied: %ld\n",messages_copied);
        fprintf(file,"Number of messages sent to self: %ld\n",messages_ego);
#endif /* BM_COUNTERS */

#ifdef BM_MESSAGE_SIZES
        fprintf(file,"Number of words sent: %ld\n",words_sent);
        fprintf(file,"Number of words copied: %ld\n",words_copied);
#endif /* BM_MESSAGE_SIZES */

#ifdef BM_HEAP_SIZES
        fprintf(file,"Biggest local heap used (in words): %ld\n",max_used_heap);
        fprintf(file,"Biggest local heap allocated (in words): %ld\n",max_allocated_heap);
        fprintf(file,"Biggest global heap used (in words): %ld\n",max_used_global_heap);
        fprintf(file,"Biggest global heap allocated (in words): %ld\n",max_allocated_global_heap);
#endif /* BM_HEAP_SIZES */

#ifdef BM_TIMERS
        fprintf(file,"--- The total active system time is the sum of all times below ---\n");
        BM_TIME_PRINTER("Mutator time: %d:%02d.%03d\n",system_time);
        BM_TIME_PRINTER("Time spent in send (excluding size & copy): %d:%02d.%03d\n",send_time);
        BM_TIME_PRINTER("Time spent in size: %d:%02d.%03d\n",size_time);
        BM_TIME_PRINTER("Time spent in copy: %d:%02d.%03d\n",copy_time);
        BM_TIME_PRINTER("Time spent in local minor GC: %d:%02d.%03d\n",minor_gc_time);
        BM_TIME_PRINTER("Time spent in local major GC: %d:%02d.%03d\n",major_gc_time);
        BM_TIME_PRINTER("Time spent in global minor GC: %d:%02d.%03d\n",minor_global_gc_time);
        BM_TIME_PRINTER("Time spent in global major GC: %d:%02d.%03d\n",major_global_gc_time);
        fprintf(file,"---\n");
        BM_TIME_PRINTER("Maximum time spent in one separate local minor GC: %d:%02d.%03d\n",max_minor_time);
        BM_TIME_PRINTER("Maximum time spent in one separate local major GC: %d:%02d.%03d\n",max_major_time);
        BM_TIME_PRINTER("Maximum time spent in one separate global minor GC: %d:%02d.%03d\n",max_global_minor_time);
        BM_TIME_PRINTER("Maximum time spent in one separate global major GC: %d:%02d.%03d\n",max_global_major_time);
#endif /* BM_TIMERS */
        fclose(file);
    }
    else
        fprintf(stderr,"Sorry... Can not write to %s!\n\r",BM_STATISTICS_FILE);
#endif /* BM_STATISTICS */
}
