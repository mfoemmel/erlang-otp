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

#ifndef __BENCHMARK_H__
#define __BENCHMARK_H__

/* The define __BENCHMARK__ is the master switch to turn on and off
 * benchmarking. This will enable the BIFs in hipe_bif1.c.
 */
/* #define __BENCHMARK__ */

#ifdef __BENCHMARK__

/* The define CALCULATE_MESSAGE_SIZES makes the system keep a log over
 * the size of all messages sent in the system.
 * This define will enable hipe_bifs:message_info/0.
 */
/* #define CALCULATE_MESSAGE_SIZES */

/* The define CALCULATE_HEAP_SIZES makes the system go through all
 * processes at garbage collection time to sum their allocated and
 * used heap sizes.
 * If this is not defined the allocated and used heap size will be
 * reported as 0 in hipe_bifs:bench_info/0.
 */
#define CALCULATE_HEAP_SIZES

/* The define SAVE_STATISTICS will make the function save_statistics()
 * save an entry in the file STATISTICS_FILE. save_statistics() is
 * called for each erlang node at exit time. save_statistics() is
 * defined below.
 */
/* #define SAVE_STATISTICS */

#ifdef SAVE_STATISTICS
#  define STATISTICS_FILE "/tmp/erlang_statistics.log"
#endif

/* Global counters for benchmarks and statistics */
extern int    processes_busy;
extern Uint   processes_spawned;
extern double messages_sent;
extern double major_garbage_cols;
extern double minor_garbage_cols;
extern Uint   biggest_heap_size_ever;
extern Uint   max_allocated_heap;
extern double live_major_sum;
extern double live_minor_sum;
extern double ptrs_to_old;
extern double ptrs_to_young;

extern hrtime_t timer_time;
extern hrtime_t system_time;
extern hrtime_t system_start;
extern hrtime_t minor_gc_time;
extern hrtime_t major_gc_time;
extern hrtime_t send_time;
extern hrtime_t copy_time;
extern hrtime_t size_time;
extern hrtime_t max_major_time;
extern hrtime_t max_minor_time;

#ifdef CALCULATE_MESSAGE_SIZES
extern Uint message_sizes[1000];
#endif

#define NEW_TIMER(t) hrtime_t t##_start
#define START_TIMER(t) t##_start = sys_gethrtime()
#define STOP_TIMER(t) t##_time += (sys_gethrtime() - t##_start - timer_time)
#define SWAP_TIMER(t1,t2) t1##_time += (sys_gethrtime() - t1##_start -  \
                                        timer_time);                    \
                          t2##_start = sys_gethrtime();

#define TIME_PRINTER(str,time) {     \
    int min,sec,milli;               \
    hrtime_t tmp;                    \
    tmp = (time) / 1000000;          \
    milli = tmp % 1000;              \
    tmp /= 1000;                     \
    sec = tmp % 60;                  \
    min = tmp / 60;                  \
    fprintf(file,str,min,sec,milli); \
}

#else /* ! __BENCHMARK__ */

#define NEW_TIMER(t)
#define START_TIMER(t)
#define STOP_TIMER(t)
#define SWAP_TIMER(t1,t2)

#endif /* __BENCHMARK__ */

void init_benchmarking();
void save_statistics();

#endif
