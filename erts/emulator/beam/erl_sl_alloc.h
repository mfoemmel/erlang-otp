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

/*
 * Description:	A memory allocator for Short Lived memory blocks.
 * Author: 	Rickard Green <rickard.green@uab.ericsson.se>;
 *         	Erlang/OTP; Ericsson Utvecklings AB; February 2002
 */

#ifndef ERL_SL_ALLOC_H__
#define ERL_SL_ALLOC_H__

#define ERTS_SL_ALLOC_RELEASE			"2"
#define ERTS_SL_ALLOC_VERSION			"1.9.3"

#define ERTS_OLD_SL_ALLOC_RELEASE		"1"
#define ERTS_OLD_SL_ALLOC_VERSION		"1.0"

/* Currently only the main thread in the emulator use sl_alloc. As long
 * as there is only one thread using sl_alloc, there is no need to do
 * any special thread safety enforcement in sl_alloc (it will only give
 * a performance penalty).
 *
 * If more than one thread use sl_alloc, make sure that 
 * MULTIPLE_THREADS_USE_SL_ALLOC is defined to an integer != 0.
 */
#ifndef MULTIPLE_THREADS_USE_SL_ALLOC
#define MULTIPLE_THREADS_USE_SL_ALLOC		0
#endif

#ifndef HAVE_MMAP
#  define HAVE_MMAP 0
#endif
#ifndef HAVE_MREMAP
#  define HAVE_MREMAP 0
#endif

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Release 2                                                                 *
\*                                                                           */

/* Default settings */
#define ERTS_SL_ALLOC_DEFAULT_ENABLED 			(HAVE_MMAP)
#define ERTS_SL_ALLOC_DEFAULT_OLD_ENABLED 		(1)
#define ERTS_SL_ALLOC_DEFAULT_CARRIER_ORDER_SEARCH 	(0)
#define ERTS_SL_ALLOC_DEFAULT_MAX_MMAP_CARRIERS 	(64)
#define ERTS_SL_ALLOC_DEFAULT_SBC_THRESHOLD		(128*1024)
#define ERTS_SL_ALLOC_DEFAULT_SBC_MOVE_THRESHOLD	(80)
#define ERTS_SL_ALLOC_DEFAULT_MAIN_CARRIER_SIZE 	(1280*1024)
#define ERTS_SL_ALLOC_DEFAULT_SMALLEST_CARRIER_SIZE	(1280*1024)
#define ERTS_SL_ALLOC_DEFAULT_LARGEST_CARRIER_SIZE	(50*1024*1024)
#define ERTS_SL_ALLOC_DEFAULT_CARRIER_GROWTH_RATIO	(25)
#define ERTS_SL_ALLOC_DEFAULT_MAX_BLOCK_SEARCH_DEPTH	(3)

typedef struct {
    Uint32 giga_calls;
    Uint32 calls;
} ErtsSlAllocCallCounter;

typedef struct {
    Uint no_carriers;
    Uint no_blocks;
    Uint carriers_size;
    Uint blocks_size;
    Uint adm_size;
} ErtsSlAllocCarriersStatBase;

typedef struct {
    Uint max_blocks;
    Uint max_blocks_size;
    ErtsSlAllocCarriersStatBase mmap;
    ErtsSlAllocCarriersStatBase malloc;
} ErtsSlAllocCarriersStat;

typedef struct {
    Uint sl_alloc_enabled;
    Uint old_sl_alloc_enabled;
    Uint singleblock_carrier_threshold;
    Uint singleblock_carrier_move_threshold;
    Uint max_mmap_carriers;
    Uint main_carrier_size;
    Uint carrier_order_search;
    Uint smallest_multiblock_carrier_size;
    Uint largest_multiblock_carrier_size;
    Uint multiblock_carrier_growth_ratio;
    Uint max_block_search_depth;
    ErtsSlAllocCarriersStat singleblock;
    ErtsSlAllocCarriersStat multiblock;
    ErtsSlAllocCallCounter sl_alloc_calls;
    ErtsSlAllocCallCounter sl_free_calls;
    ErtsSlAllocCallCounter sl_realloc_calls;
    ErtsSlAllocCallCounter mmap_calls;
    ErtsSlAllocCallCounter munmap_calls;
    ErtsSlAllocCallCounter mremap_calls;
    ErtsSlAllocCallCounter malloc_calls;
    ErtsSlAllocCallCounter free_calls;
    ErtsSlAllocCallCounter realloc_calls;
} ErtsSlAllocStat;

typedef struct {
    int esla;
    int eosla;
    int mcs;
    int sbct;
    int sbcmt;
    int mmc;
    int cos;
    int scs;
    int lcs;
    int cgr;
    int mbsd;
} ErtsSlAllocInit;

#ifdef INSTRUMENT

void *erts_sl_alloc2(Uint);
void *erts_sl_realloc2(void *, Uint, Uint);
void  erts_sl_free2(void *);

#define erts_sl_alloc_from(Where, Size) \
  instr_alloc((Where), erts_sl_alloc2, (Size))
#define erts_sl_realloc_from(Where, Ptr, SaveSize, Size) \
  instr_realloc((Where), erts_sl_realloc2, (Ptr), (SaveSize), (Size))

void *erts_safe_sl_alloc_from(int, Uint);
void *erts_safe_sl_realloc_from(int, void *, Uint, Uint);

#else /* #ifdef INSTRUMENT */

#define erts_sl_alloc_from(Where, Size) erts_sl_alloc((Size))
#define erts_sl_realloc_from(Where, Ptr, SaveSize, Size) \
 erts_sl_realloc((Ptr), (SaveSize), (Size))

#define erts_safe_sl_alloc_from(Where, Size) erts_safe_sl_alloc((Size))
#define erts_safe_sl_realloc_from(Where, Ptr, SaveSize, Size) \
  erts_safe_sl_realloc((Ptr), (SaveSize), (Size))

#endif /* #ifdef INSTRUMENT */

void *erts_sl_alloc(Uint);
void *erts_sl_realloc(void *, Uint, Uint);
void  erts_sl_free(void *);
void  erts_sl_alloc_init(ErtsSlAllocInit *);
void  erts_sl_alloc_info(CIO);
Eterm erts_sl_alloc_stat_eterm(Process *);
void  erts_sl_alloc_stat(ErtsSlAllocStat *);
void *erts_safe_sl_alloc(Uint);
void *erts_safe_sl_realloc(void *, Uint, Uint);

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Release 1                                                                 *
\*                                                                           */

/* erts_old_sl_alloc_opt() parameters */
#define ERTS_SL_ALLOC_OPT_SBC_THRESHOLD		SYS_ALLOC_OPT_MMAP_THRESHOLD
#define ERTS_SL_ALLOC_OPT_MAX_MMAP_CARRIERS	SYS_ALLOC_OPT_MMAP_MAX
#define ERTS_SL_ALLOC_OPT_USE_MMAP_TABLE	4


typedef struct {
  Sint sl_alloc_enabled;
  Sint mmap_threshold;
  Sint mmap_max;
  Sint mmapped_chunks;
  Uint mmapped_chunks_size;
  Uint mmapped_blocks_size;
  struct {
    Sint in_use;
    Sint size;
    Sint used;
    Sint objs;
    Sint depth;
  } mmap_table;
} ErtsOldSlAllocStat;

void *erts_old_sl_alloc(Uint);
void *erts_old_sl_realloc(void *, Uint, Uint);
void  erts_old_sl_free(void *);
void  erts_old_sl_alloc_init(int);
int   erts_old_sl_alloc_opt(int, int);
void  erts_old_sl_alloc_info(CIO);
void  erts_old_sl_alloc_stat(ErtsOldSlAllocStat *);

#endif /* #ifndef ERL_SL_ALLOC_H__ */

