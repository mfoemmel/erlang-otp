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

#ifdef __BENCHMARK__
#include <time.h>

Uint   processes_spawned;
double messages_sent;
double major_garbage_cols;
double minor_garbage_cols;
Uint   biggest_heap_size_ever;
Uint   max_allocated_heap;
double live_major_sum;
double live_minor_sum;
double ptrs_to_old;
double ptrs_to_young;

hrtime_t timer_time;
hrtime_t system_time;
hrtime_t system_start;
hrtime_t minor_gc_time;
hrtime_t major_gc_time;
hrtime_t send_time;
hrtime_t copy_time;
hrtime_t size_time;
hrtime_t max_major_time;
hrtime_t max_minor_time;

#ifdef CALCULATE_MESSAGE_SIZES
Uint message_sizes[1000];
#endif

#endif /* __BENCHMARK__ */

/*****
 * The folowing functoins has to be defined, but they only have contents
 * if sertain keywords are defined.
 */

void init_benchmarking()
{
#ifdef __BENCHMARK__
    int i;
    hrtime_t timer_start;

    timer_time = 0;
    system_time = 0;
    minor_gc_time = 0;
    major_gc_time = 0;
    send_time = 0;
    copy_time = 0;
    size_time = 0;
    max_major_time = 0;
    max_minor_time = 0;
    major_garbage_cols = 0;
    minor_garbage_cols = 0;
    biggest_heap_size_ever = 0;
    max_allocated_heap = 0;
    live_major_sum = 0;
    live_minor_sum = 0;
    ptrs_to_old = 0;
    ptrs_to_young = 0;
    processes_spawned = 0;
    messages_sent = 0;

    for (i = 0; i < 1000; i++)
    {
        timer_start = sys_gethrtime();
        timer_time += (sys_gethrtime() - timer_start);
    }
    timer_time /= 1000;

# ifdef CALCULATE_MESSAGE_SIZES
    { int i;
      for (i=0; i<1000; i++) message_sizes[i] = 0;
    }
# endif
#endif
}

void save_statistics()
{
#ifdef SAVE_STATISTICS
    FILE *file = fopen(STATISTICS_FILE,"a");
    int i = 0;

    if (file)
    {
        hrtime_t sys_time = system_time;
        ldisplay(this_node,CBUF,50);
        while (tmp_buf[++i] != '\'' && tmp_buf[i] != '\0');
        if (tmp_buf[i] == '\'') tmp_buf[i+1] = '\0';
        fprintf(file,tmp_buf);
        fprintf(file,"-------------------------------------------------------------------------\n");
        fprintf(file,"The counters are reset at system start and are sums over the entire node.\n");
        fprintf(file,"You may reset them manually using the clear BIFs in the module hipe_bifs.\n");
        fprintf(file,"All times are given in milliseconds.\n");
        fprintf(file,"-------------------------------------------------------------------------\n");
        fprintf(file,"Number of processes spawned: %ld\n",processes_spawned);
        fprintf(file,"Number of messages sent: %.0f\n",messages_sent);
        fprintf(file,"Biggest heap used (in words): %ld\n",biggest_heap_size_ever);
        fprintf(file,"Biggest heap allocated (in words): %ld\n",max_allocated_heap);
        fprintf(file,"Number of minor garbage collections: %.0f\n",minor_garbage_cols);
        fprintf(file,"Number of major garbage collections: %.0f\n",major_garbage_cols);
        fprintf(file,"Live data recovered in minor collections (in words): %.0f\n",live_minor_sum);
        fprintf(file,"Live data recovered in major collections (in words): %.0f\n",live_major_sum);
        fprintf(file,"Number of pointers to young generation: %.0f\n",ptrs_to_young);
        fprintf(file,"Number of pointers to old generation: %.0f\n",ptrs_to_old);
        TIME_PRINTER("Mutator time: %d:%02d.%03d\n",sys_time);
        TIME_PRINTER("Time spent in send (excluding size & copy): %d:%02d.%03d\n",send_time);
        TIME_PRINTER("Time spent in size: %d:%02d.%03d\n",size_time);
        TIME_PRINTER("Time spent in copy: %d:%02d.%03d\n",copy_time);
        TIME_PRINTER("Time spent in minor garbage collection: %d:%02d.%03d\n",minor_gc_time);
        TIME_PRINTER("Time spent in major garbage collection: %d:%02d.%03d\n",major_gc_time);
        TIME_PRINTER("Maximum time spent in one separate minor garbage collection: %d:%02d.%03d\n",max_minor_time);
        TIME_PRINTER("Maximum time spent in one separate major garbage collection: %d:%02d.%03d\n",max_major_time);
        fclose(file);
    }
    else
        printf("Sorry... Can not write to %s!\n\r",STATISTICS_FILE);
#endif /* SAVE_STATISTICS */
}
