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
/*
 * The defines below enables different parts of the benchmaring.
 * Counters and timers that are disabled are reported as 0 from the BIFs.
 */

/* BM_TIMERS keeps track of the time spent in diferent parts of the
 * system. It only measures accual active time, not time spent in idle
 * mode. These timers requires hardware support.
 * For Linux: Use the package perfctr from www.csd.uu.se/~mikpe/linux/perfctr
 * If this package is not specified when configuring the system,
 * the Solaris hrtime_t will be used.
 */
/* #define BM_TIMERS */

/* BM_COUNTERS count all kinds of events that occurs in the system.
 * Among other things it counts the number of messages, then number of
 * garbage collections, the number of processes spawned etc.
 */
#define BM_COUNTERS

/* BM_MESSAGE_SIZES keeps a log of the size of all messages sent in
 * the system.
 */
#define BM_MESSAGE_SIZES

/* BM_HEAP_SIZES goes through all processes at garbage collection time
 * to sum their allocated and used heap sizes.
 */
#define BM_HEAP_SIZES

/* BM_STATISTICS saves an entry in the file BM_STATISTICS_FILE. This
 * is done for each erlang node at exit time.
 */
/* #define BM_STATISTICS */

#endif /* __BENCHMARK__ */


#ifdef BM_STATISTICS
#  define BM_STATISTICS_FILE "/tmp/erlang_statistics.log"
#endif /* BM_STATISTICS */

#ifdef BM_COUNTERS
extern unsigned long processes_busy;
extern unsigned long processes_spawned;
extern unsigned long messages_sent;
extern unsigned long messages_copied;
extern unsigned long messages_ego;
extern unsigned long minor_garbage_cols;
extern unsigned long major_garbage_cols;
extern unsigned long minor_global_garbage_cols;
extern unsigned long major_global_garbage_cols;
extern unsigned long gc_in_copy;

#define BM_COUNT(var) var++;
#define BM_EGO_COUNT(send,rec) {     \
    if (send == rec)                 \
        BM_COUNT(messages_ego);      }

#define BM_LAZY_COPY_START int gcs = minor_global_garbage_cols + major_global_garbage_cols;
#define BM_LAZY_COPY_STOP gcs -= minor_global_garbage_cols + major_global_garbage_cols; \
    if (gcs > gc_in_copy) gc_in_copy = gcs;

#else /* not BM_COUNTERS */
#  define BM_COUNT(var)
#  define BM_EGO_COUNT(send,rec)
#endif /* BM_COUNTERS */

#ifdef BM_TIMERS
#if defined(__i386__) && USE_PERFCTR
#include "libperfctr.h"
extern struct vperfctr *system_clock;
extern double cpu_khz;
#define BM_TIMER_T double
#define BM_START_TIMER(t) timer_time =   \
                          (double)vperfctr_read_tsc(system_clock) / cpu_khz;
#define BM_STOP_TIMER(t) t##_time +=  \
      ((double)vperfctr_read_tsc(system_clock) / cpu_khz) - timer_time;

#define BM_TIME_PRINTER(str,time) {                   \
    int min,sec,milli;                                \
    BM_TIMER_T tmp = time;                            \
    milli = (uint)(tmp - ((int)(tmp / 1000)) * 1000); \
    tmp /= 1000;                                      \
    sec = (uint)(tmp - ((int)(tmp / 60)) * 60);       \
    min = (uint)tmp / 60;                             \
    fprintf(file,str,min,sec,milli);                  }

#else /* not USE_PERFCTR */

#define BM_TIMER_T hrtime_t
#define BM_START_TIMER(t) system_clock = sys_gethrtime()
#define BM_STOP_TIMER(t) {                                         \
    hrtime_t t1 = (sys_gethrtime() - system_clock) - timer_time;   \
    t##_time += (t1 > 0 ? t1 : 0);                                 }

#define BM_TIME_PRINTER(str,time) {  \
    int min,sec,milli;               \
    BM_TIMER_T tmp;                  \
    tmp = (time) / 1000000;          \
    milli = tmp % 1000;              \
    tmp /= 1000;                     \
    sec = tmp % 60;                  \
    min = tmp / 60;                  \
    fprintf(file,str,min,sec,milli); }

extern BM_TIMER_T system_clock;
#endif

extern BM_TIMER_T timer_time;
extern BM_TIMER_T system_time;
extern BM_TIMER_T minor_gc_time;
extern BM_TIMER_T major_gc_time;
extern BM_TIMER_T minor_global_gc_time;
extern BM_TIMER_T major_global_gc_time;
extern BM_TIMER_T local_in_global_time;
extern BM_TIMER_T send_time;
extern BM_TIMER_T copy_time;
extern BM_TIMER_T size_time;
extern BM_TIMER_T max_minor_time;
extern BM_TIMER_T max_major_time;
extern BM_TIMER_T max_global_minor_time;
extern BM_TIMER_T max_global_major_time;

#define BM_NEW_TIMER(t) BM_TIMER_T t##_time = 0;
#define BM_SWAP_TIMER(t1,t2) { BM_STOP_TIMER(t1); BM_START_TIMER(t2); }
#else /* not BM_TIMERS */
#  define BM_NEW_TIMER(t)
#  define BM_START_TIMER(t)
#  define BM_STOP_TIMER(t)
#  define BM_SWAP_TIMER(t1,t2)
#  define BM_TIME_PRINTER(str,time)
#endif /* BM_TIMERS */

#ifdef BM_HEAP_SIZES
extern unsigned long max_used_heap;
extern unsigned long max_allocated_heap;
extern unsigned long max_used_global_heap;
extern unsigned long max_allocated_global_heap;
#endif /* BM_HEAP_SIZES */

#ifdef BM_MESSAGE_SIZES
extern unsigned long words_sent;
extern unsigned long words_copied;
extern unsigned long message_sizes[1000];

#define BM_MESSAGE_COPIED(size) { \
    words_copied += size;         \
    BM_COUNT(messages_copied);    }

#define BM_MESSAGE(mess,send,rec) {  \
    Uint msize = size_object(mess);  \
    words_sent += msize;             \
    if (msize < 1000)                \
        message_sizes[msize]++;      \
    else                             \
        message_sizes[999]++;        \
    BM_EGO_COUNT(send,rec);          \
    BM_COUNT(messages_sent);         }

#else /* not BM_MESSAGE_SIZES */

#define BM_MESSAGE_COPIED(size) BM_COUNT(messages_copied);
#define BM_MESSAGE(mess,send,rec) {  \
    BM_EGO_COUNT(send,rec);          \
    BM_COUNT(messages_sent);         }

#endif /* BM_MESSAGE_SIZES */

void init_benchmarking(void);
void save_statistics(void);

#endif /* _BENCHMARK_H_ */
