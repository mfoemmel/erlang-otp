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


/*
 * See the sl_alloc(3) man page for a comprehensive description
 * of sl_alloc and its use in the ERTS.
 *
 * This is a first version with multi block carriers (mbc). It has
 * proven to be useful, but it can (and will) be improved in a lot
 * of ways.
 *
 * Here is the beginning of the "will be done in the future list":
 * 1. Remove the "carrier order search" (cos) feature which proved
 *    not to be very useful (and cluttered the implementation).
 * 2. Remove the carrier field in block headers (which is only needed
 *    by the cos feature) and remove the bucket fields (etc) in
 *    the mbc header (which only is used by the cos feature). This
 *    will both make block headers shrink and the minimum blocks size
 *    shrink. 
 * 3. Optimize.
 * 4. Cleanup the code.
 * ...
 *
 * Here are parts of the "will probably be done in the future" list
 * (unordered):
 * * Implement special treatment of blocks smaller than the minimum
 *   block size (currently no blocks like this are allocated trough
 *   sl_alloc in the ERTS).
 * ...
 *
 */

/*
 * sl_alloc will enforce 8 byte alignment if malloc() (and mmap()) at
 * least enforces 8 byte alignment. If malloc() only enforces 4 byte
 * alignment sl_alloc will do so too. 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "erl_sl_alloc.h"
#include "big.h"
#include <float.h>

#ifdef DEBUG
#  ifndef HARD_DEBUG
#    define HARD_DEBUG 0
#  endif
#  if HARD_DEBUG
#    warning "*** *** *** *** ***"
#    warning "*** HARD_DEBUG  ***"
#    warning "*** *** *** *** ***"
#  endif
#else
#  undef HARD_DEBUG
#  define HARD_DEBUG 0
#endif

#ifdef INSTRUMENT
#define INSTR_EXPORT(BASE)	erts_sl_ ## BASE ## 2
#define INSTR_IMPORT(BASE)	sys_ ## BASE ## 2
#else
#define INSTR_EXPORT(BASE)	erts_sl_ ## BASE
#define INSTR_IMPORT(BASE)	sys_ ## BASE
#endif

#define EXPORT(BASE)		erts_sl_ ## BASE

#define SL_ALLOC		INSTR_EXPORT(alloc)
#define SL_REALLOC		INSTR_EXPORT(realloc)
#define SL_FREE			INSTR_EXPORT(free)

#define SL_ALLOC_INIT		EXPORT(alloc_init)
#define SL_ALLOC_INFO		EXPORT(alloc_info)
#define SL_ALLOC_STAT		EXPORT(alloc_stat)
#define SL_ALLOC_STAT_ETERM	EXPORT(alloc_stat_eterm)
#define SL_ALLOC_OPT		EXPORT(alloc_opt)

#define MEMCPY			sys_memcpy

#define MALLOC_FUNC		INSTR_IMPORT(alloc)
#define REALLOC_FUNC		INSTR_IMPORT(realloc)
#define FREE_FUNC		INSTR_IMPORT(free)

#undef  INIT_AM
#define INIT_AM(S) AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

#ifdef DEBUG
static int initialized = 0;
#else
#define initialized 1
#endif

#define USE_MMAP HAVE_MMAP
#if USE_MMAP && HAVE_MREMAP
#  define USE_MREMAP HAVE_MREMAP
#else
#  define USE_MREMAP 0
#endif

#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 0
#endif

#if USE_MMAP
#  ifdef _SC_PAGESIZE
#    define GET_PAGE_SIZE sysconf(_SC_PAGESIZE)
#  elif HAVE_GETPAGESIZE
#    define GET_PAGE_SIZE getpagesize()
#  else
#    error "Page size unknown"
     /* Implement some other way to get the real page size if needed! */
#  endif
#endif

#undef ASSERT
#ifdef DEBUG
#define ASSERT(B) \
 ((void) ((B) ? 1 : (fprintf(stderr, "%s:%d: Assertion failed: %s\n", \
			     __FILE__, __LINE__, #B), abort(), 0)))
#else
#define ASSERT(B) ((void) 1)
#endif

#if 0
/* Can be useful for debugging */
#define MBC_REALLOC_ALWAYS_MOVES
#endif

/* Threads ... */

#undef THREAD_SAFE_SL_ALLOC
#if defined(USE_THREADS) && MULTIPLE_THREADS_USE_SL_ALLOC
#define THREAD_SAFE_SL_ALLOC
#endif

#ifdef THREAD_SAFE_SL_ALLOC

#include "erl_threads.h"

#define SL_ALLOC_MUTEX_NO 4

#ifdef DEBUG
#define LOCK   ASSERT(0 == erts_mutex_lock(sl_alloc_lock))
#define UNLOCK ASSERT(0 == erts_mutex_unlock(sl_alloc_lock))
#else
#define LOCK   ((void) erts_mutex_lock(sl_alloc_lock))
#define UNLOCK ((void) erts_mutex_unlock(sl_alloc_lock))
#endif

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void));
erts_mutex_t erts_mutex_sys(int mno);
static erts_mutex_t sl_alloc_lock;

#ifndef INIT_MUTEX_IN_CHILD_AT_FORK
#define INIT_MUTEX_IN_CHILD_AT_FORK 0
#endif

static void lock_sl_alloc_lock(void)   { LOCK;   }
static void unlock_sl_alloc_lock(void) { UNLOCK; }
#if INIT_MUTEX_IN_CHILD_AT_FORK
static void init_sl_alloc_lock(void)
{
#ifdef DEBUG
  ASSERT((sl_alloc_lock = erts_mutex_sys(SL_ALLOC_MUTEX_NO)));
#else
  sl_alloc_lock = erts_mutex_sys(SL_ALLOC_MUTEX_NO);
#endif
}

#define CHILD_ATFORK   init_sl_alloc_lock
#else /* #if INIT_MUTEX_IN_CHILD_AT_FORK */
#define CHILD_ATFORK   unlock_sl_alloc_lock
#endif /* #if INIT_MUTEX_IN_CHILD_AT_FORK */
#define PARENT_ATFORK  unlock_sl_alloc_lock
#define PREPARE_ATFORK lock_sl_alloc_lock

#ifndef HAVE_PTHREAD_ATFORK
#define HAVE_PTHREAD_ATFORK 0
#endif

static void thread_safe_init(void)
{
#if defined(DEBUG) && HAVE_PTHREAD_ATFORK
    int atfork_res;
#endif

    sl_alloc_lock = erts_mutex_sys(SL_ALLOC_MUTEX_NO);
    if(!sl_alloc_lock) {
	/* erl_exit(-1, ...) because we don't want to generate an
	   erl_crash.dump (which will fail at this stage). */
	erl_exit(-1, "Failed to initialize sl_alloc lock\n");
    }

#if defined(DEBUG) && HAVE_PTHREAD_ATFORK
    atfork_res =
#endif

	erts_atfork_sys(PREPARE_ATFORK, PARENT_ATFORK, CHILD_ATFORK);

#if defined(DEBUG) && HAVE_PTHREAD_ATFORK
    ASSERT(atfork_res == 0);
#endif


}

#else /* #ifdef THREAD_SAFE_SL_ALLOC */

#define LOCK   
#define UNLOCK 

#endif /* #ifdef THREAD_SAFE_SL_ALLOC */

#if SIZEOF_LONG == 8
typedef unsigned long BucketMask_t;
#elif SIZEOF_UNSIGNED_LONG_LONG == 8
typedef unsigned long long BucketMask_t;
#else
typedef unsigned long BucketMask_t;
#endif

#define NO_OF_BKTS (sizeof(BucketMask_t)*8)

typedef union {char c[8]; long l; double d;} Unit_t;

typedef Uint Carrier_t;

typedef struct {
    Uint misc;
    Carrier_t *carrier; /* Will be removed in next version */
} Block_t;

typedef struct FreeBlock_t_ {
    Block_t block_head;
    struct FreeBlock_t_ *prev;
    struct FreeBlock_t_ *next;
} FreeBlock_t;

typedef struct {
    Block_t block_head;
} AllocBlock_t;


typedef struct {
    Carrier_t head;
} SBCarrier_t;

typedef struct MBCarrier_t_ {
    Carrier_t head;
    struct MBCarrier_t_ *prev;
    struct MBCarrier_t_ *next;
    BucketMask_t non_empty_buckets;
    FreeBlock_t *buckets[NO_OF_BKTS];
} MBCarrier_t;

#define MAX_PRE_CALCED_MBC_GROWTH_FACT	30

static double mbc_growth_fact[MAX_PRE_CALCED_MBC_GROWTH_FACT];
static int max_mbcgf_calced;

static BucketMask_t non_empty_buckets;
static FreeBlock_t *common_buckets[NO_OF_BKTS];
static int blocks_in_buckets[NO_OF_BKTS];
static Uint max_blk_search;

static int sl_alloc_disabled;
static int use_old_sl_alloc;

static Uint main_carrier_units;

/* Double linked list of all multi block carriers */
static MBCarrier_t *first_mb_carrier;
static MBCarrier_t *last_mb_carrier;
static MBCarrier_t *last_aux_mb_carrier;

/* Some statistics ... */
static ErtsSlAllocCallCounter sl_alloc_calls;
static ErtsSlAllocCallCounter sl_free_calls;
static ErtsSlAllocCallCounter sl_realloc_calls;
static ErtsSlAllocCallCounter mmap_calls;
static ErtsSlAllocCallCounter munmap_calls;
static ErtsSlAllocCallCounter mremap_calls;
static ErtsSlAllocCallCounter malloc_calls;
static ErtsSlAllocCallCounter free_calls;
static ErtsSlAllocCallCounter realloc_calls;

static Uint mmap_sb_carriers;
static Uint malloc_sb_carriers;
static Uint mmap_mb_carriers;
static Uint malloc_mb_carriers;
static Uint tot_mmap_sb_carrier_units;
static Uint tot_malloc_sb_carrier_units;
static Uint tot_mmap_mb_carrier_units;
static Uint tot_malloc_mb_carrier_units;
static Uint mmap_mbc_blks;
static Uint malloc_mbc_blks;
static Uint tot_mmap_sbc_blk_units;
static Uint tot_malloc_sbc_blk_units;
static Uint tot_mmap_mbc_blk_units;
static Uint tot_malloc_mbc_blk_units;
static Uint max_mbc_blk_units;
static Uint max_mbc_blks;
static Uint max_sbc_blk_units;
static Uint max_sbc_blks;

/* Single block carrier threshold
 * (blocks >= sbc_threshold will be allocated
 * in a single block carrier).
 */
static Uint sbc_threshold;
static Uint sbc_move_threshold;
/* Max number of mmap carriers */
static Uint max_mmap_carriers;
/* Search each carrier in order for a free block that fits? */
static int use_carrier_order_search;

static Uint mbc_growth_ratio;
static Uint smallest_mbc_units;
static Uint largest_mbc_units;

/* Used by bucket_index() */
static Uint bkt_max_d;
static Uint bkt_intrvl_d;

#if USE_MMAP
static Uint page_units;
#endif

#ifndef NO_SL_ALLOC_STAT_ETERM

/* Atoms used by erts_sl_alloc_stat_eterm() ... */
static Eterm AM_singleblock_carrier_threshold;
static Eterm AM_singleblock_carrier_move_threshold;
static Eterm AM_max_mmap_carriers;
static Eterm AM_singleblock_carriers;
static Eterm AM_multiblock_carriers;
static Eterm AM_carrier_order_search;
static Eterm AM_main_carrier_size;
static Eterm AM_smallest_multiblock_carrier_size;
static Eterm AM_largest_multiblock_carrier_size;
static Eterm AM_multiblock_carrier_growth_ratio;
static Eterm AM_max_block_search_depth;
static Eterm AM_malloc;
static Eterm AM_mmap;
static Eterm AM_carriers;
static Eterm AM_blocks;
static Eterm AM_carriers_size;
static Eterm AM_blocks_size;
static Eterm AM_adm_size;
static Eterm AM_max_blocks;
static Eterm AM_max_blocks_size;
static Eterm AM_calls;
static Eterm AM_sl_alloc;
static Eterm AM_sl_free;
static Eterm AM_sl_realloc;
static Eterm AM_malloc;
static Eterm AM_free;
static Eterm AM_realloc;
static Eterm AM_mmap;
static Eterm AM_munmap;
static Eterm AM_mremap;
static Eterm AM_unknown;

#endif

static int atoms_need_init; /* Need to initialize atoms? */

#if USE_MMAP && !defined(MAP_ANON)
/* File descriptor used by mmap() (opened on /dev/zero) */
static int mmap_fd;
#endif

#undef MIN
#undef MAX
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define FLOOR(X, I) (((X)/(I))*(I))
#define CEIL(X, I)  ((((X) - 1)/(I) + 1)*(I))

#define B2U(B) ((Uint) ((B)-1)/sizeof(Unit_t)+1)
#define U2B(U) ((Uint) (U)*sizeof(Unit_t))

#define ONE_GIGA (1000000000)

#define ZERO_CC(CC) ((CC).calls = 0, (CC).giga_calls = 0)

#define INC_CC(CC) ((CC).calls == ONE_GIGA - 1				\
		    ? ((CC).giga_calls++, (CC).calls = 0)		\
		    : (CC).calls++)

#define DEC_CC(CC) ((CC).calls == 0					\
		    ? ((CC).giga_calls--, (CC).calls = ONE_GIGA - 1)	\
		    : (CC).calls--)

#define PRINT_CC(TO, CC)						\
do {									\
    if ((CC).giga_calls == 0)						\
	erl_printf((TO), "%u", (CC).calls);				\
    else								\
	erl_printf((TO), "%u%09u", (CC).giga_calls, (CC).calls);	\
} while (0)

/* Mmap ... */

#define DO_MMAP(S, FL, FD) \
  (INC_CC(mmap_calls), \
   mmap((void *) 0, (S), PROT_READ|PROT_WRITE, (FL), (FD), 0))

#ifdef MAP_ANON

#define MMAP(S) DO_MMAP((S), MAP_ANON|MAP_PRIVATE, -1)

#else /* #ifdef MAP_ANON */

#define GET_MMAP_FD open("/dev/zero", O_RDWR)

#define MMAP(S)								\
  (mmap_fd < 0								\
   ? ((mmap_fd = GET_MMAP_FD) < 0					\
      ? MAP_FAILED							\
      : DO_MMAP((S), MAP_PRIVATE, mmap_fd))				\
   : DO_MMAP((S), MAP_PRIVATE, mmap_fd))

#endif /* #ifdef MAP_ANON */

#define DO_MUNMAP(C) \
  (INC_CC(munmap_calls), munmap((void *) (C), (size_t) U2B(CARRIER_UNITS((C)))))

#ifdef DEBUG
#define MUNMAP(C) ASSERT(0 == DO_MUNMAP((C)))
#else
#define MUNMAP(C) ((void) DO_MUNMAP((C)))
#endif

#if HAVE_MREMAP
#ifndef MREMAP_MAYMOVE
#define MREMAP_MAYMOVE 1
#endif

#define MREMAP(C, S) \
  (INC_CC(mremap_calls), \
   mremap((void *) (C), (size_t) U2B(CARRIER_UNITS((C))), (S), MREMAP_MAYMOVE))

#endif /* #if HAVE_MREMAP */

#undef  MALLOC
#undef  REALLOC
#undef  FREE

#define MALLOC(S)	(INC_CC(malloc_calls),	MALLOC_FUNC((S)))
#define REALLOC(P, S)	(INC_CC(realloc_calls),	REALLOC_FUNC((P), (S)))
#define FREE(P)		(INC_CC(free_calls),	FREE_FUNC((P)))

/* ... */

#define UNITS_SHIFT		3
#define UNITS_MASK		(~(~0 << (sizeof(Uint)*8-3)))
#define FLAG_MASK		0x7

/* Blocks ... */

#define THIS_BLK_FREE_FLAG 	(1 << 0)
#define PREV_BLK_FREE_FLAG 	(1 << 1)
#define SINGLE_BLK_FLAG 	(1 << 2)

#define MIN_BLK_UNITS  (B2U(CEIL(sizeof(FreeBlock_t) + sizeof(Uint), \
				 sizeof(Unit_t))))
#define ABLK_HDR_UNITS (B2U(CEIL(sizeof(AllocBlock_t), sizeof(Unit_t))))

#define MEM2BLK(P) ((Block_t *) (((Unit_t *) (P)) - ABLK_HDR_UNITS))
#define BLK2MEM(P) ((void *)    (((Unit_t *) (P)) + ABLK_HDR_UNITS))

#define SET_FREE_BLK_UNITS(B, U)			\
  do {							\
    SET_BLK_UNITS((B), (U));				\
    if (!IS_LAST_BLK(B))				\
	*((Uint *) (((Unit_t *) (B)) + (U) - 1)) = (U);	\
  } while (0)
#define SET_BLK_UNITS(B, U) \
  (((Block_t *) (B))->misc = \
   ((((Block_t *) (B))->misc & FLAG_MASK) | ((U) << UNITS_SHIFT)))
#define SET_BLK_CARRIER(B, C) \
  (((Block_t *) (B))->carrier = (Carrier_t *) (C))
#define SET_BLK_FREE(B) \
  (((Block_t *) (B))->misc |= THIS_BLK_FREE_FLAG)
#define SET_BLK_ALLOCED(B) \
  (((Block_t *) (B))->misc &= ~THIS_BLK_FREE_FLAG)
#define SET_PREV_BLK_FREE(B) \
  (((Block_t *) (B))->misc |= PREV_BLK_FREE_FLAG)
#define SET_PREV_BLK_ALLOCED(B) \
  (((Block_t *) (B))->misc &= ~PREV_BLK_FREE_FLAG)
#define SET_SINGLE_BLK(B) \
  (((Block_t *) (B))->misc |= SINGLE_BLK_FLAG)
#define SET_MULTI_BLK(B) \
  (((Block_t *) (B))->misc &= ~SINGLE_BLK_FLAG)

#define MEM_UNITS(B) \
  (BLK_UNITS(B) - (ABLK_HDR_UNITS))
#define BLK_UNITS(B) \
  ((((Block_t *) (B))->misc >> UNITS_SHIFT) & UNITS_MASK)
#define BLK_CARRIER(B) \
  (((Block_t *) (B))->carrier)
#define IS_PREV_BLK_FREE(B) \
  (((Block_t*)(B))->misc & PREV_BLK_FREE_FLAG)
#define IS_PREV_BLK_ALLOCED(B) \
  (!IS_PREV_BLK_FREE((B)))
#define IS_FREE_BLK(B) \
  (((Block_t*)(B))->misc & THIS_BLK_FREE_FLAG)
#define IS_ALLOCED_BLK(B) \
  (!IS_FREE_BLK((B)))  
#define IS_SBC_BLK(B) \
  (((Block_t*)(B))->misc & SINGLE_BLK_FLAG)
#define IS_MBC_BLK(B) \
  (!IS_SBC_BLK((B)))

#define IS_LAST_BLK(B) \
 (((Unit_t *) BLK_CARRIER((B))) + CARRIER_UNITS(BLK_CARRIER((B))) \
    == ((Unit_t *) (B)) + BLK_UNITS((B)))
#define IS_FIRST_BLK(B) \
  (MBC2MEM(BLK_CARRIER((B))) == ((void *) (B)))

#define NXT_BLK(B) \
  ((Block_t *) (((Unit_t *) (B)) + BLK_UNITS((B))))
#define PREV_BLK(B) \
  ((Block_t *) (((Unit_t *) (B)) - *((Uint *) (((Unit_t *) (B)) - 1))))

/* Carriers ... */

#define MIN_MBC_FIRST_FREE_UNITS	B2U(4*1024)
#define MIN_MBC_UNITS			B2U(16*1024)

#define MMAP_CARRIER_FLAG		(1 << 0)
#define SINGLE_BLK_CARRIER_FLAG		(1 << 1)
#define MAIN_CARRIER_FLAG		(1 << 2)

#define SBC_HDR_UNITS B2U(CEIL(sizeof(SBCarrier_t), sizeof(Unit_t)))
#define MBC_HDR_UNITS B2U(CEIL(sizeof(MBCarrier_t), sizeof(Unit_t)))

#define MBC2MEM(P) ((void *) (((Unit_t *) (P))+MBC_HDR_UNITS))

#define SBC2BLK(P) ((AllocBlock_t *) ((Unit_t *) (P))+SBC_HDR_UNITS)
#define SBC2MEM(P) ((void *) (((Unit_t *) (P))+SBC_HDR_UNITS+ABLK_HDR_UNITS))

#define IS_MMAP_CARRIER(C) \
  (*((Carrier_t*)(C)) & MMAP_CARRIER_FLAG)
#define IS_MALLOC_CARRIER(C) \
  (!IS_MMAP_CARRIER((C)))
#define IS_SB_CARRIER(C) \
  (*((Carrier_t*)(C)) & SINGLE_BLK_CARRIER_FLAG)
#define IS_MB_CARRIER(C) \
  (!IS_SB_CARRIER((C)))
#define IS_MAIN_CARRIER(C) \
  (*((Carrier_t*)(C)) & MAIN_CARRIER_FLAG)
#define IS_AUX_CARRIER(C) \
  (!IS_MAIN_CARRIER((C)))

#define CARRIER_UNITS(C) \
  ((*((Carrier_t*)(C)) >> UNITS_SHIFT) & UNITS_MASK)

#define SET_MMAP_CARRIER(C) \
  (*((Carrier_t*)(C)) |= MMAP_CARRIER_FLAG)
#define SET_MALLOC_CARRIER(C) \
  (*((Carrier_t*)(C)) &= ~MMAP_CARRIER_FLAG)
#define SET_SB_CARRIER(C) \
  (*((Carrier_t*)(C)) |= SINGLE_BLK_CARRIER_FLAG)
#define SET_MB_CARRIER(C) \
  (*((Carrier_t*)(C)) &= ~SINGLE_BLK_CARRIER_FLAG)
#define SET_MAIN_CARRIER(C) \
  (*((Carrier_t*)(C)) |= MAIN_CARRIER_FLAG)
#define SET_AUX_CARRIER(C) \
  (*((Carrier_t*)(C)) &= ~MAIN_CARRIER_FLAG)

#define SET_CARRIER_UNITS(C, U) \
  (*((Carrier_t*)(C)) = (*((Carrier_t*)(C)) & FLAG_MASK) | ((U) << UNITS_SHIFT))

#define CA_TYPE_SINGLEBLOCK			0
#define CA_TYPE_MULTIBLOCK			1

#define CA_FLAG_FORCE_MALLOC			(1 << 0)
#define CA_FLAG_MAIN_CARRIER			(1 << 1)
#define CA_FLAG_KEEP_MALLOC_CARRIER	       	(1 << 2)

#if HARD_DEBUG
static void check_sb_carrier(SBCarrier_t *);
static void check_mb_carrier(MBCarrier_t *);
#endif


/* Statistics updating ... */

#define STAT_SBC_BLK_ALLOC					\
    if (max_sbc_blks < (mmap_sb_carriers			\
			+ malloc_sb_carriers))			\
	max_sbc_blks = (mmap_sb_carriers			\
			+ malloc_sb_carriers);			\
    if (max_sbc_blk_units < (tot_mmap_sbc_blk_units		\
			     + tot_malloc_sbc_blk_units))	\
	max_sbc_blk_units = (tot_mmap_sbc_blk_units		\
			     + tot_malloc_sbc_blk_units)

#define STAT_MMAP_SB_CARRIER_ALLOC(CU, BU)			\
do {								\
    mmap_sb_carriers++;						\
    tot_mmap_sb_carrier_units += (CU);				\
    tot_mmap_sbc_blk_units += (BU);				\
    STAT_SBC_BLK_ALLOC;						\
} while (0)

#define STAT_MMAP_SB_CARRIER_FREE(CU, BU)			\
do {								\
    ASSERT(mmap_sb_carriers > 0);				\
    mmap_sb_carriers--;						\
    ASSERT(tot_mmap_sb_carrier_units >= (CU));			\
    tot_mmap_sb_carrier_units -= (CU);				\
    ASSERT(tot_mmap_sbc_blk_units >= (BU));			\
    tot_mmap_sbc_blk_units -= (BU);				\
} while (0)

#define STAT_MALLOC_SB_CARRIER_ALLOC(CU, BU)			\
do {								\
    malloc_sb_carriers++;					\
    tot_malloc_sb_carrier_units += (CU);			\
    tot_malloc_sbc_blk_units += (BU);				\
    STAT_SBC_BLK_ALLOC;						\
} while (0)

#define STAT_MALLOC_SB_CARRIER_FREE(CU, BU)			\
do {								\
    ASSERT(malloc_sb_carriers > 0);				\
    malloc_sb_carriers--;					\
    ASSERT(tot_malloc_sb_carrier_units >= (CU));		\
    tot_malloc_sb_carrier_units -= (CU);			\
    ASSERT(tot_malloc_sbc_blk_units >= (BU));			\
    tot_malloc_sbc_blk_units -= (BU);				\
} while (0)

#define STAT_MMAP_MB_CARRIER_ALLOC(U)				\
do {								\
    mmap_mb_carriers++;						\
    tot_mmap_mb_carrier_units += (U);				\
} while (0)

#define STAT_MMAP_MB_CARRIER_FREE(U)				\
do {								\
    ASSERT(mmap_mb_carriers > 0);				\
    mmap_mb_carriers--;						\
    ASSERT(tot_mmap_mb_carrier_units >= (U));			\
    tot_mmap_mb_carrier_units -= (U);				\
} while (0)

#define STAT_MALLOC_MB_CARRIER_ALLOC(U)				\
do {								\
    malloc_mb_carriers++;					\
    tot_malloc_mb_carrier_units += (U);				\
} while (0)

#define STAT_MALLOC_MB_CARRIER_FREE(U)				\
do {								\
    ASSERT(malloc_mb_carriers > 0);				\
    malloc_mb_carriers--;					\
    ASSERT(tot_malloc_mb_carrier_units >= (U));			\
    tot_malloc_mb_carrier_units -= (U);				\
} while (0)

#define STAT_MBC_BLK_ALLOC					\
    if (max_mbc_blks < mmap_mbc_blks + malloc_mbc_blks)		\
	max_mbc_blks = mmap_mbc_blks + malloc_mbc_blks;		\
    if (max_mbc_blk_units < (tot_mmap_mbc_blk_units		\
			     + tot_malloc_mbc_blk_units))	\
	max_mbc_blk_units = (tot_mmap_mbc_blk_units		\
			     + tot_malloc_mbc_blk_units)

#define STAT_MMAP_MBC_BLK_ALLOC(U)				\
do {								\
    mmap_mbc_blks++;						\
    tot_mmap_mbc_blk_units += (U);				\
    STAT_MBC_BLK_ALLOC;						\
} while (0)

#define STAT_MMAP_MBC_BLK_FREE(U)				\
do {								\
    ASSERT(mmap_mbc_blks > 0);					\
    mmap_mbc_blks--;						\
    ASSERT(tot_mmap_mbc_blk_units >= (U));			\
    tot_mmap_mbc_blk_units -= (U);				\
} while (0)

#define STAT_MALLOC_MBC_BLK_ALLOC(U)				\
do {								\
    malloc_mbc_blks++;						\
    tot_malloc_mbc_blk_units += (U);				\
    STAT_MBC_BLK_ALLOC;						\
} while (0)

#define STAT_MALLOC_MBC_BLK_FREE(U)				\
do {								\
    ASSERT(malloc_mbc_blks > 0);				\
    malloc_mbc_blks--;						\
    ASSERT(tot_malloc_mbc_blk_units >= (U));			\
    tot_malloc_mbc_blk_units -= (U);				\
} while (0)

/* Buckets ... */

#define BKT_INTRVL_A		1
#define BKT_INTRVL_B		16
#define BKT_INTRVL_C		96
#define BKT_INTRVL_D		bkt_intrvl_d

#define BKT_MIN_A		MIN_BLK_UNITS
#define BKT_MIN_B		CEIL(BKT_MAX_A+BKT_INTRVL_A, BKT_INTRVL_B)
#define BKT_MIN_C		CEIL(BKT_MAX_B+BKT_INTRVL_B, BKT_INTRVL_C)
#define BKT_MIN_D		CEIL(BKT_MAX_C+BKT_INTRVL_C, BKT_INTRVL_D)

#define BKT_MAX_A		(BKT_MIN_A + (NO_OF_BKTS/4-1)*BKT_INTRVL_A)
#define BKT_MAX_B		(BKT_MIN_B + (NO_OF_BKTS/4-1)*BKT_INTRVL_B)
#define BKT_MAX_C		(BKT_MIN_C + (NO_OF_BKTS/4-1)*BKT_INTRVL_C)
#define BKT_MAX_D		bkt_max_d

#define PBKT_RES(U, INT, MIN, BASE) (((U)+(INT)-(MIN))/(INT)+(BASE)-1)

#define PBKT_RES_A(U) PBKT_RES((U), BKT_INTRVL_A, BKT_MIN_A, 0)
#define PBKT_RES_B(U) PBKT_RES((U), BKT_INTRVL_B, BKT_MIN_B, NO_OF_BKTS/4)
#define PBKT_RES_C(U) PBKT_RES((U), BKT_INTRVL_C, BKT_MIN_C, NO_OF_BKTS/2)
#define PBKT_RES_D(U) PBKT_RES((U), BKT_INTRVL_D, BKT_MIN_D, NO_OF_BKTS*3/4)

#define GBKT_RES(U, INT, MIN, BASE) (((U)+2*(INT)-(MIN)-1)/(INT)+(BASE)-1)
#define GBKT_RES_A(U) PBKT_RES_A((U))
#define GBKT_RES_B(U) GBKT_RES((U), BKT_INTRVL_B, BKT_MIN_B, NO_OF_BKTS/4)
#define GBKT_RES_C(U) GBKT_RES((U), BKT_INTRVL_C, BKT_MIN_C, NO_OF_BKTS/2)
#define GBKT_RES_D(U) GBKT_RES((U), BKT_INTRVL_D, BKT_MIN_D, NO_OF_BKTS*3/4)

static int
bucket_index(Uint units)
{
    ASSERT(units >= MIN_BLK_UNITS);

    if (units <= BKT_MAX_A)
	return PBKT_RES_A(units);
    if (units <= BKT_MAX_B)
	return PBKT_RES_B(units);
    if (units <= BKT_MAX_C)
	return PBKT_RES_C(units);
    if (units <= BKT_MAX_D)
	return MIN(NO_OF_BKTS - 2, PBKT_RES_D(units));
    
    return NO_OF_BKTS - 1;
}

static int
find_bucket(BucketMask_t bucket_mask, Uint min_bucket)
{
    int min, mid, max;

    min = min_bucket;

    if(bucket_mask & (((BucketMask_t) 1) << min))
	return min;

    if((bucket_mask & (~((BucketMask_t) 0) << min)) == 0)
	return -1;

    /* There exists a non empty bucket; find it... */

    max = NO_OF_BKTS - 1;

    while(max != min) {
	mid = (max - min)/2 + min;
	if(bucket_mask
	   & (~(~((BucketMask_t) 0) << (mid + 1)))
	   & (~((BucketMask_t) 0) << min))
	    max = mid;
	else
	    min = mid + 1;
    }

    ASSERT(bucket_mask & (((BucketMask_t) 1) << min));
    ASSERT(!(bucket_mask
	     & (~(~((BucketMask_t) 0) << min))
	     & (~((BucketMask_t) 0) << min_bucket)));

    return min;
}

static FreeBlock_t *
search_free_blocks(FreeBlock_t *blk_list, Uint units)
{
    int i;
    Uint blk_units;
    Uint cand_units = 0;
    FreeBlock_t *blk = NULL;
    FreeBlock_t *cand_blk = NULL;
    MBCarrier_t *cand_carrier = NULL;

    for (blk = blk_list, i = 0;
	 blk && i < max_blk_search;
	 blk = blk->next, i++) {
	blk_units = BLK_UNITS(blk);

	if (blk_units == units
	    && (MBCarrier_t *) BLK_CARRIER(blk) != last_aux_mb_carrier)
	    return blk;

	if (blk_units >= units
	    && (!cand_blk
		|| ((MBCarrier_t *) BLK_CARRIER(blk) != last_aux_mb_carrier
		    && (cand_carrier == last_aux_mb_carrier
			|| blk_units < cand_units))
		|| ((MBCarrier_t *) BLK_CARRIER(blk) == last_aux_mb_carrier
		    && cand_carrier == last_aux_mb_carrier
		    && blk_units < cand_units))) {
	    cand_units = blk_units;
	    cand_blk = blk;
	    cand_carrier = (MBCarrier_t *) BLK_CARRIER(blk);
	}

    }
    return cand_blk;
}

static FreeBlock_t *
find_free_block(Uint units)
{
    int bi, unsafe_bi, min_bi;
    FreeBlock_t *blk;
    MBCarrier_t *c;

    unsafe_bi = bucket_index(units);
    
    min_bi = find_bucket(non_empty_buckets, unsafe_bi);
    if (min_bi < 0)
	return NULL;

    if (use_carrier_order_search) {
	for (c = first_mb_carrier; c; c = c->next) {
	    bi = find_bucket(c->non_empty_buckets, min_bi);
	    if (bi < 0)
		continue;
	    if (bi == unsafe_bi) {
		blk = search_free_blocks(c->buckets[bi], units);
		if (blk)
		    return blk;
		if (bi < NO_OF_BKTS - 1) {
		    bi = find_bucket(c->non_empty_buckets, bi + 1);
		    if (bi < 0)
			continue;
		}
		else
		    continue;
	    }
	    else {
		ASSERT(bi > unsafe_bi);
	    }
	    ASSERT(c->buckets[bi]);
	    /* We search a safe bucket */
	    blk = search_free_blocks(c->buckets[bi], units);
	    ASSERT(blk);
	    return blk;
	}
	
    }
    else {
	if (min_bi == unsafe_bi) {
	    blk = search_free_blocks(common_buckets[min_bi], units);
	    if (blk)
		return blk;
	    if (min_bi < NO_OF_BKTS - 1) {
		min_bi = find_bucket(non_empty_buckets, min_bi + 1);
		if (min_bi < 0)
		    return NULL;
	    }
	    else
		return NULL;
	}
	else {
	    ASSERT(min_bi > unsafe_bi);
	}
	ASSERT(common_buckets[min_bi]);
	/* We search a safe bucket */
	blk = search_free_blocks(common_buckets[min_bi], units);
	ASSERT(blk);
	return blk;
    }

    return NULL;
}

static void
link_free_block(FreeBlock_t *blk)
{
    int i;
    FreeBlock_t **buckets;

    i = bucket_index(BLK_UNITS(blk));


    if (use_carrier_order_search) {
	MBCarrier_t *c = (MBCarrier_t *) BLK_CARRIER(blk);
	buckets = c->buckets;
	c->non_empty_buckets |= ((BucketMask_t) 1) << i;
    }
    else {
	buckets = common_buckets;
    }

    blocks_in_buckets[i]++;
    non_empty_buckets |= ((BucketMask_t) 1) << i;
    blk->prev = NULL;
    blk->next = buckets[i];
    if (blk->next) {
	ASSERT(!blk->next->prev);
	blk->next->prev = blk;
    }
    buckets[i] = blk;
}

static void
unlink_free_block(FreeBlock_t *blk)
{
    int i;
    BucketMask_t *non_mt_bkts;
    FreeBlock_t **buckets;

    i = bucket_index(BLK_UNITS(blk));

    if (use_carrier_order_search) {
	MBCarrier_t *c = (MBCarrier_t *) BLK_CARRIER(blk);
	buckets = c->buckets;
	non_mt_bkts = &c->non_empty_buckets;
    }
    else {
	buckets = common_buckets;
	non_mt_bkts = &non_empty_buckets;
    }

    if (!blk->prev) {
	ASSERT(buckets[i] == blk);
	buckets[i] = blk->next;
    }
    else
	blk->prev->next = blk->next;
    if (blk->next)
	blk->next->prev = blk->prev;

    ASSERT(blocks_in_buckets[i] > 0);
    blocks_in_buckets[i]--;

    if (!buckets[i])
	*non_mt_bkts &= ~(((BucketMask_t) 1) << i);
    if (use_carrier_order_search && blocks_in_buckets[i] == 0)
	non_empty_buckets &= ~(((BucketMask_t) 1) << i);
}

static double
calc_mbc_growth_fact(int cs)
{
    double ratio = ((double)(100 + mbc_growth_ratio))/((double) 100);

    ASSERT(cs >= 0);
    ASSERT(cs > max_mbcgf_calced);

    if (cs >= MAX_PRE_CALCED_MBC_GROWTH_FACT) {
	double gf = pow(ratio, (double) cs);
	if (gf == HUGE_VAL)
	    return -1.0;
	return gf;
    }

    while (max_mbcgf_calced < cs) {
	max_mbcgf_calced++;

	if (DBL_MAX/ratio < mbc_growth_fact[max_mbcgf_calced-1])
	    mbc_growth_fact[max_mbcgf_calced] = -1.0;
	else
	    mbc_growth_fact[max_mbcgf_calced] =
		mbc_growth_fact[max_mbcgf_calced-1] * ratio;
    }

    return mbc_growth_fact[cs];
}


static Uint
next_mbc_units(void)
{
    double gf;
    Uint units;
    int cs;

    cs = mmap_mb_carriers + malloc_mb_carriers;
    ASSERT(cs >= 0);

    gf = cs > max_mbcgf_calced ? calc_mbc_growth_fact(cs) : mbc_growth_fact[cs];

    if (gf < 0 || DBL_MAX/gf < smallest_mbc_units)
	units = largest_mbc_units;
    else
	units = (Uint) smallest_mbc_units*gf;

    if (units < MIN_MBC_UNITS)
	units = MIN_MBC_UNITS;

    if (units > largest_mbc_units)
	units = largest_mbc_units;

    return units - MBC_HDR_UNITS;
}

static Carrier_t *carrier_alloc(Uint, Uint, Uint);
static void carrier_free(Carrier_t *);

/* Multi block carrier alloc/realloc/free ... */


/* Observe! mbc_alloc() may in case of memory shortage place the requested
 * block in a sbc.
 */
static void *
mbc_alloc(size_t size)
{
    Uint units;
    Block_t *blk;
    Uint nxt_units;
    MBCarrier_t *carrier;
    Block_t *nxt_blk;

    ASSERT(size);
    ASSERT(size < sbc_threshold);

    units = B2U(size) + ABLK_HDR_UNITS;

    if (units < MIN_BLK_UNITS)
	units = MIN_BLK_UNITS;

    blk = (Block_t *) find_free_block(units);

    if (!blk) {
	Uint c_units = next_mbc_units();
	if (c_units - MIN_MBC_FIRST_FREE_UNITS < units)
	    c_units = units + MIN_MBC_FIRST_FREE_UNITS;

	carrier = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK,
						c_units,
						0);
	if (!carrier) {
	    /*
	     * Emergency! We couldn't allocate the carrier as we wanted.
	     * If it's a relatively small request, we try one more time
	     * with a small mbc; otherwise, we place it in a sbc.
	     */
	    if (MIN_MBC_UNITS - MIN_MBC_FIRST_FREE_UNITS >= units)
		carrier = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK,
							MIN_MBC_UNITS,
							0);

	    if (!carrier) {
		/* The only thing left to do is to try to place it in a sbc. */
		SBCarrier_t *sbc;
		sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
						    B2U(size),
						    CA_FLAG_FORCE_MALLOC);
		return sbc ? SBC2MEM(sbc) : NULL;
	    }
	}
	
	blk = (Block_t *) MBC2MEM(carrier);

	ASSERT(BLK_UNITS(blk) >= units);
#if HARD_DEBUG
	link_free_block((FreeBlock_t *) blk);
	check_mb_carrier(carrier);
	unlink_free_block((FreeBlock_t *) blk);
#endif

    }
    else {
	carrier = (MBCarrier_t *) BLK_CARRIER(blk);

	ASSERT(IS_FREE_BLK(blk));
#if HARD_DEBUG
	check_mb_carrier(carrier);
#endif

	unlink_free_block((FreeBlock_t *) blk);
    }


    ASSERT(blk);
    ASSERT(IS_MBC_BLK(blk));

    SET_BLK_ALLOCED(blk);


    if (BLK_UNITS(blk) - MIN_BLK_UNITS >= units) {
	/* Shrink block... */
	nxt_units = BLK_UNITS(blk) - units;
	SET_BLK_UNITS(blk, units);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_FREE(nxt_blk);
	SET_PREV_BLK_ALLOCED(nxt_blk);
	SET_MULTI_BLK(nxt_blk);
	SET_BLK_CARRIER(nxt_blk, BLK_CARRIER(blk));
	SET_FREE_BLK_UNITS(nxt_blk, nxt_units);
	link_free_block((FreeBlock_t *) nxt_blk);

    }
    else {
	if (!IS_LAST_BLK(blk)) {
	    nxt_blk = NXT_BLK(blk);
	    SET_PREV_BLK_ALLOCED(nxt_blk);
	}
	units = BLK_UNITS(blk);
    }

    if (IS_MMAP_CARRIER(carrier)) 
	STAT_MMAP_MBC_BLK_ALLOC(units);
    else
	STAT_MALLOC_MBC_BLK_ALLOC(units);

#if HARD_DEBUG
    check_mb_carrier(carrier);
#endif

    ASSERT(BLK_UNITS(blk) >= MIN_BLK_UNITS);

    return BLK2MEM(blk);
}

static void
mbc_free(void *p)
{
    Uint units;
    MBCarrier_t *carrier;
    Block_t *blk;
    Block_t *nxt_blk;

    ASSERT(p);

    blk = MEM2BLK(p);
    units = BLK_UNITS(blk);
    carrier = (MBCarrier_t *) BLK_CARRIER(blk);
    ASSERT(IS_MBC_BLK(blk));
    ASSERT(units >= MIN_BLK_UNITS);

#if HARD_DEBUG
    check_mb_carrier(carrier);
#endif

    if (IS_MMAP_CARRIER(carrier)) 
	STAT_MMAP_MBC_BLK_FREE(units);
    else
	STAT_MALLOC_MBC_BLK_FREE(units);

    if (!IS_FIRST_BLK(blk) && IS_PREV_BLK_FREE(blk)) {
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	unlink_free_block((FreeBlock_t *) blk);

	units += BLK_UNITS(blk);
	ASSERT(carrier == (MBCarrier_t *) BLK_CARRIER(blk));
    }
    else {
	SET_BLK_FREE(blk);
    }

    SET_FREE_BLK_UNITS(blk, units);

    ASSERT(IS_MBC_BLK(blk));

    if (!IS_LAST_BLK(blk)) {
	nxt_blk = NXT_BLK(blk);
	if (IS_FREE_BLK(nxt_blk)) {
	    /* Coalesce with next block... */
	    unlink_free_block((FreeBlock_t *) nxt_blk);
	    units += BLK_UNITS(nxt_blk);
	    SET_FREE_BLK_UNITS(blk, units);
	}
	else
	    SET_PREV_BLK_FREE(nxt_blk);
    }


    if (IS_AUX_CARRIER(carrier) && IS_FIRST_BLK(blk) && IS_LAST_BLK(blk)) {
#if HARD_DEBUG
	link_free_block((FreeBlock_t *) blk);
	check_mb_carrier(carrier);
	unlink_free_block((FreeBlock_t *) blk);
#endif
	carrier_free((Carrier_t *) carrier);
    }
    else {
	link_free_block((FreeBlock_t *) blk);
#if HARD_DEBUG
	check_mb_carrier(carrier);
#endif
    }
}

static void *
mbc_realloc(void *p, size_t size)
{
    void *new_p;
    Uint old_units;
    Block_t *blk;
#ifndef MBC_REALLOC_ALWAYS_MOVES
    Uint units;
    Uint nxt_units;
    Block_t *nxt_blk;
    MBCarrier_t *carrier;
#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

    ASSERT(p);
    ASSERT(size);
    ASSERT(size < sbc_threshold);

    blk = (Block_t *) MEM2BLK(p);
    old_units = BLK_UNITS(blk);

    ASSERT(old_units >= MIN_BLK_UNITS);

#ifndef MBC_REALLOC_ALWAYS_MOVES


    units = B2U(size) + ABLK_HDR_UNITS;
    if (units < MIN_BLK_UNITS)
	units = MIN_BLK_UNITS;

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(IS_MBC_BLK(blk));

    if (old_units == units)
	return p;

    if ((IS_LAST_BLK(blk) || IS_ALLOCED_BLK(NXT_BLK(blk)))
	&& (old_units - MIN_BLK_UNITS < units && units < old_units))
	return p;

    if (units < old_units) {
	/* Shrink block... */
	carrier = (MBCarrier_t *) BLK_CARRIER(blk);

#if HARD_DEBUG
	check_mb_carrier(carrier);
#endif

	nxt_units = old_units - units;
	SET_BLK_UNITS(blk, units);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_FREE(nxt_blk);
	SET_PREV_BLK_ALLOCED(nxt_blk);
	SET_MULTI_BLK(nxt_blk);
	SET_BLK_CARRIER(nxt_blk, carrier);
	SET_FREE_BLK_UNITS(nxt_blk, nxt_units);

	if (IS_MMAP_CARRIER(carrier)) {
	    STAT_MMAP_MBC_BLK_FREE(old_units);
	    STAT_MMAP_MBC_BLK_ALLOC(units);
	}
	else {
	    STAT_MALLOC_MBC_BLK_FREE(old_units);
	    STAT_MALLOC_MBC_BLK_ALLOC(units);
	}

	ASSERT(BLK_UNITS(blk) >= MIN_BLK_UNITS);

	blk = nxt_blk;
	units = nxt_units;

	if (!IS_LAST_BLK(blk)) {
	    nxt_blk = NXT_BLK(blk);
	    if (IS_FREE_BLK(nxt_blk)) {
		/* Coalesce with next free block... */
		units += BLK_UNITS(nxt_blk);
		unlink_free_block((FreeBlock_t *) nxt_blk);
		SET_FREE_BLK_UNITS(blk, units);
		ASSERT(!IS_LAST_BLK(blk) ? IS_PREV_BLK_FREE(NXT_BLK(blk)) : 1);
	    }
	    else
		SET_PREV_BLK_FREE(nxt_blk);
	}

	link_free_block((FreeBlock_t *) blk);

#if HARD_DEBUG
	check_mb_carrier(carrier);
#endif

	return p;
    }

    /* Need larger block... */

    if (!IS_LAST_BLK(blk)) {
	nxt_blk = NXT_BLK(blk);
	if (IS_FREE_BLK(nxt_blk)
	    && units <= old_units + BLK_UNITS(nxt_blk)) {
	    /* Grow into next block... */
	    carrier = (MBCarrier_t *) BLK_CARRIER(blk);
#if HARD_DEBUG
	    check_mb_carrier(carrier);
#endif

	    unlink_free_block((FreeBlock_t *) nxt_blk);
	    nxt_units = BLK_UNITS(nxt_blk) - (units - old_units);

	    if (nxt_units < MIN_BLK_UNITS) {
		units += nxt_units;

		SET_BLK_UNITS(blk, units);

		if (!IS_LAST_BLK(blk)) {
		    nxt_blk = NXT_BLK(blk);
		    SET_PREV_BLK_ALLOCED(nxt_blk);
		}
	    }
	    else {
		SET_BLK_UNITS(blk, units);

		nxt_blk = NXT_BLK(blk);
		SET_BLK_FREE(nxt_blk);
		SET_PREV_BLK_ALLOCED(nxt_blk);
		SET_MULTI_BLK(nxt_blk);
		SET_BLK_CARRIER(nxt_blk, carrier);
		SET_FREE_BLK_UNITS(nxt_blk, nxt_units);
		link_free_block((FreeBlock_t *) nxt_blk);

		ASSERT(!IS_LAST_BLK(nxt_blk)
		       ? IS_PREV_BLK_FREE(NXT_BLK(nxt_blk))
		       : 1);
	    }

	    if (IS_MMAP_CARRIER(carrier)) {
		STAT_MMAP_MBC_BLK_FREE(old_units);
		STAT_MMAP_MBC_BLK_ALLOC(units);
	    }
	    else {
		STAT_MALLOC_MBC_BLK_FREE(old_units);
		STAT_MALLOC_MBC_BLK_ALLOC(units);
	    }

	    ASSERT(BLK_UNITS(blk) >= MIN_BLK_UNITS);
#if HARD_DEBUG
	    check_mb_carrier(carrier);
#endif
	    return p;
	}
    }

    /* Failed to grow; move into a new one... */

#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

    new_p = mbc_alloc(size);
    if (!new_p)
	return NULL;
    MEMCPY(new_p, p, MIN(size, U2B(old_units - ABLK_HDR_UNITS)));
    mbc_free(p);
    return new_p;
}

static Carrier_t *
carrier_alloc(Uint type, Uint data_units, Uint flags)
{
    Carrier_t *carrier;
#if USE_MMAP
    Uint mmap_carrier_units;
    int mmapped = 0;
#endif
    Uint block_units;
    Uint carrier_units;

    if (type == CA_TYPE_SINGLEBLOCK) {
	block_units = data_units + ABLK_HDR_UNITS;
	carrier_units = block_units + SBC_HDR_UNITS;
    }
    else {
	ASSERT(type == CA_TYPE_MULTIBLOCK);
	block_units = data_units;
	carrier_units = MBC_HDR_UNITS + data_units;
    }

#if USE_MMAP
    if (mmap_mb_carriers + mmap_sb_carriers >= max_mmap_carriers
	|| flags & CA_FLAG_FORCE_MALLOC)
	goto malloc_carrier;
    mmap_carrier_units = CEIL(carrier_units, page_units);

    carrier = (Carrier_t *) MMAP(U2B(mmap_carrier_units));
    if (carrier != (Carrier_t *) MAP_FAILED) {
	carrier_units = mmap_carrier_units;
	SET_MMAP_CARRIER(carrier);
	mmapped++;
    }
    else {
    malloc_carrier:
#endif
	carrier = (Carrier_t *) MALLOC(U2B(carrier_units));
	if (!carrier)
	    return NULL;
	SET_MALLOC_CARRIER(carrier);
#if USE_MMAP
    }
#endif

    if (flags & CA_FLAG_MAIN_CARRIER)
	SET_MAIN_CARRIER(carrier);
    else
	SET_AUX_CARRIER(carrier);

    SET_CARRIER_UNITS(carrier, carrier_units);

    if (type == CA_TYPE_SINGLEBLOCK) {
	SBCarrier_t *sb_carrier = (SBCarrier_t *) carrier;

	SET_SB_CARRIER(sb_carrier);
	SET_BLK_ALLOCED(SBC2BLK(sb_carrier));
	SET_PREV_BLK_ALLOCED(SBC2BLK(sb_carrier));
	SET_SINGLE_BLK(SBC2BLK(sb_carrier));
	SET_BLK_CARRIER(SBC2BLK(sb_carrier), sb_carrier);
	SET_BLK_UNITS(SBC2BLK(sb_carrier), block_units);

#if USE_MMAP
	if (mmapped)
	    STAT_MMAP_SB_CARRIER_ALLOC(carrier_units, block_units);
	else
#endif
	    STAT_MALLOC_SB_CARRIER_ALLOC(carrier_units, block_units);

#if HARD_DEBUG
	check_sb_carrier(sb_carrier);
#endif
    }
    else {
	int i;
	FreeBlock_t *blk;
	MBCarrier_t *mb_carrier = (MBCarrier_t *) carrier;

	SET_MB_CARRIER(carrier);

	mb_carrier->next = NULL;
	if (!last_mb_carrier) {
	    ASSERT(!first_mb_carrier);
	    first_mb_carrier = last_mb_carrier = mb_carrier;
	    mb_carrier->prev = NULL;
	}
	else {
	    ASSERT(first_mb_carrier);
	    ASSERT(!last_mb_carrier->next);
	    mb_carrier->prev = last_mb_carrier;
	    last_mb_carrier->next = mb_carrier;
	    last_mb_carrier = mb_carrier;
	}

	last_aux_mb_carrier = (IS_AUX_CARRIER(last_mb_carrier)
			       ? last_mb_carrier
			       : NULL);

	mb_carrier->non_empty_buckets = 0;

	for (i = 0; i < NO_OF_BKTS; i++)
	    mb_carrier->buckets[i] = NULL;

	blk = (FreeBlock_t *) MBC2MEM(mb_carrier);
	SET_BLK_FREE(blk);
	SET_PREV_BLK_ALLOCED(blk);
	SET_MULTI_BLK(blk);
	SET_BLK_CARRIER(blk, mb_carrier);
	SET_FREE_BLK_UNITS(blk, carrier_units - MBC_HDR_UNITS);

#if USE_MMAP
	if (mmapped)
	    STAT_MMAP_MB_CARRIER_ALLOC(carrier_units);
	else
#endif
	    STAT_MALLOC_MB_CARRIER_ALLOC(carrier_units);

    }
    return carrier;
}

static SBCarrier_t *
sb_carrier_resize(SBCarrier_t *carrier, Uint new_data_units, Uint flags)
{
#if USE_MMAP
    Uint new_mmap_carrier_units;
#endif
    SBCarrier_t *new_carrier;
    Uint old_carrier_units;
    Uint old_block_units;
    Uint new_block_units;
    Uint new_carrier_units;

#if HARD_DEBUG
    check_sb_carrier(carrier);
#endif

    ASSERT(IS_SB_CARRIER(carrier));
    ASSERT(IS_SBC_BLK(SBC2BLK(carrier)));

    new_block_units = new_data_units + ABLK_HDR_UNITS;
    new_carrier_units = new_block_units + SBC_HDR_UNITS;

    old_carrier_units = CARRIER_UNITS(carrier);
    old_block_units = BLK_UNITS(SBC2BLK(carrier));

#if USE_MMAP

    if (IS_MMAP_CARRIER(carrier)) {

	if (mmap_mb_carriers + mmap_sb_carriers > max_mmap_carriers
	    || flags & CA_FLAG_FORCE_MALLOC)
	    goto force_malloc;

	STAT_MMAP_SB_CARRIER_FREE(old_carrier_units, old_block_units);
	
	new_mmap_carrier_units = CEIL(new_carrier_units, page_units);
	if (new_mmap_carrier_units == old_carrier_units) {
#if !USE_MREMAP
	no_carrier_change:
#endif
	    SET_BLK_UNITS(SBC2BLK(carrier), new_block_units);
	    STAT_MMAP_SB_CARRIER_ALLOC(old_carrier_units, new_block_units);
	    return carrier;
	}

#if USE_MREMAP
	new_carrier = (SBCarrier_t*) MREMAP(carrier,
					    U2B(new_mmap_carrier_units));
	if (new_carrier != (SBCarrier_t *) MAP_FAILED) {
	    new_carrier_units = new_mmap_carrier_units;
	    STAT_MMAP_SB_CARRIER_ALLOC(new_carrier_units, new_block_units);
	    goto final_touch;
	} /* else goto force_malloc; */
#else

	if ((new_mmap_carrier_units < old_carrier_units)
	    && ((100*((float) (old_carrier_units - new_mmap_carrier_units))
		 /((float) old_carrier_units))
		< (float) sbc_move_threshold)) {
	    /* Less unused than move threshold; reuse old carrier... */
	    goto no_carrier_change;
	}

	new_carrier = (SBCarrier_t *) MMAP(U2B(new_mmap_carrier_units));
	if (new_carrier != (SBCarrier_t *) MAP_FAILED) {
	    new_carrier_units = new_mmap_carrier_units;
	    STAT_MMAP_SB_CARRIER_ALLOC(new_carrier_units, new_block_units);
	}
#endif
	else {
	force_malloc:
	    new_carrier = (SBCarrier_t *) MALLOC(U2B(new_carrier_units));
	    if (!new_carrier)
		return NULL;
	    SET_MALLOC_CARRIER(carrier); /* Will be copied into new_carrier */
	    STAT_MALLOC_SB_CARRIER_ALLOC(new_carrier_units, new_block_units);
	}
	MEMCPY((void *) new_carrier,
	       (void *) carrier,
	       U2B(SBC_HDR_UNITS + MIN(new_block_units, old_block_units)));
	MUNMAP(carrier);

    }
    else {
	if (mmap_mb_carriers + mmap_sb_carriers < max_mmap_carriers
	    && !(flags & CA_FLAG_KEEP_MALLOC_CARRIER)) {
	    new_mmap_carrier_units = CEIL(new_carrier_units, page_units);
	    new_carrier = (SBCarrier_t *) MMAP(U2B(new_mmap_carrier_units));
	    if (new_carrier == (SBCarrier_t *) MAP_FAILED)
		goto try_realloc;

	    new_carrier_units = new_mmap_carrier_units;
	    MEMCPY((void *) new_carrier,
		   (void *) carrier,
		   U2B(SBC_HDR_UNITS + MIN(new_block_units, old_block_units)));
	    SET_MMAP_CARRIER(new_carrier);
	    FREE((void *) carrier);

	    STAT_MALLOC_SB_CARRIER_FREE(old_carrier_units, old_block_units);
	    STAT_MMAP_SB_CARRIER_ALLOC(new_carrier_units, new_block_units);
	}
	else {
	try_realloc:
#endif
	    new_carrier = (SBCarrier_t *) REALLOC((void *) carrier,
						  U2B(new_carrier_units));
	    if (!new_carrier)
		return NULL;

	    STAT_MALLOC_SB_CARRIER_FREE(old_carrier_units, old_block_units);
	    STAT_MALLOC_SB_CARRIER_ALLOC(new_carrier_units, new_block_units);
#if USE_MMAP
	}
    }
#endif

#if USE_MREMAP
 final_touch:
#endif
    SET_BLK_UNITS(SBC2BLK(new_carrier), new_block_units);
    SET_CARRIER_UNITS(new_carrier, new_carrier_units);
    SET_BLK_CARRIER(SBC2BLK(new_carrier), new_carrier);


    ASSERT(IS_SB_CARRIER(new_carrier));
    ASSERT(IS_SBC_BLK(SBC2BLK(new_carrier)));

#if HARD_DEBUG
    check_sb_carrier(new_carrier);
#endif

    return new_carrier;

}

static void
carrier_free(Carrier_t *carrier)
{
#if USE_MMAP
    Uint mmapped = 0;
#endif
    Uint carrier_units = CARRIER_UNITS(carrier);

    if (IS_SB_CARRIER(carrier)) {
	SBCarrier_t *sbc = (SBCarrier_t *) carrier;
	Uint block_units = BLK_UNITS(SBC2BLK(sbc));
#if HARD_DEBUG
	check_sb_carrier(sbc);
#endif
#if USE_MMAP
	if (IS_MMAP_CARRIER(carrier)) {
	    mmapped++;
	    STAT_MMAP_SB_CARRIER_FREE(carrier_units, block_units);
	}
	else
#endif
	    STAT_MALLOC_SB_CARRIER_FREE(carrier_units, block_units);
    }
    else {
	MBCarrier_t *mbc = (MBCarrier_t *) carrier;

	ASSERT(IS_FIRST_BLK(MBC2MEM(mbc)) && IS_LAST_BLK(MBC2MEM(mbc)));

#ifdef DEBUG
	{
	    int i;
	    if (use_carrier_order_search) {
		ASSERT(!mbc->non_empty_buckets);
		for (i = 0; i < NO_OF_BKTS; i++) {
		    ASSERT(!mbc->buckets[i]);
		}
	    }
	}
#endif

	if (first_mb_carrier == mbc) {
	    first_mb_carrier = mbc->next;
	}
	else {
	    ASSERT(mbc->prev);
	    mbc->prev->next = mbc->next;
	}

	if (last_mb_carrier == mbc)
	    last_mb_carrier = mbc->prev;
	else {
	    ASSERT(mbc->next);
	    mbc->next->prev = mbc->prev;
	}

	
	last_aux_mb_carrier = (IS_AUX_CARRIER(last_mb_carrier)
			       ? last_mb_carrier
			       : NULL);

#if USE_MMAP
	if (IS_MMAP_CARRIER(mbc)) {
	    mmapped++;
	    STAT_MMAP_MB_CARRIER_FREE(carrier_units);
	}
	else
#endif
	    STAT_MALLOC_MB_CARRIER_FREE(carrier_units);
    }

#if USE_MMAP
    if (mmapped)
	MUNMAP(carrier);
    else
#endif
	FREE(carrier);
}

static int
set_sbc_threshold(Uint sbcl)
{
    sbc_threshold = sbcl;
    bkt_max_d = B2U(sbc_threshold);
    if (bkt_max_d < BKT_MAX_C + BKT_INTRVL_C*(NO_OF_BKTS/4))
	bkt_max_d = BKT_MAX_C + BKT_INTRVL_C*(NO_OF_BKTS/4);
    bkt_intrvl_d = (BKT_MAX_D - BKT_MAX_C)/(NO_OF_BKTS/4);
    return 1;
}

#ifndef NO_SL_ALLOC_STAT_ETERM

static void
init_atoms(void)
{
    LOCK;

    if (!atoms_need_init) {
	UNLOCK;
	return;
    }

    INIT_AM(singleblock_carrier_threshold);
    INIT_AM(singleblock_carrier_move_threshold);
    INIT_AM(max_mmap_carriers);
    INIT_AM(singleblock_carriers);
    INIT_AM(multiblock_carriers);
    INIT_AM(carrier_order_search);
    INIT_AM(main_carrier_size);
    INIT_AM(smallest_multiblock_carrier_size);
    INIT_AM(largest_multiblock_carrier_size);
    INIT_AM(multiblock_carrier_growth_ratio);
    INIT_AM(max_block_search_depth);
    INIT_AM(malloc);
    INIT_AM(mmap);
    INIT_AM(carriers);
    INIT_AM(blocks);
    INIT_AM(carriers_size);
    INIT_AM(blocks_size);
    INIT_AM(adm_size);
    INIT_AM(max_blocks);
    INIT_AM(max_blocks_size);
    INIT_AM(calls);
    INIT_AM(sl_alloc);
    INIT_AM(sl_free);
    INIT_AM(sl_realloc);
    INIT_AM(malloc);
    INIT_AM(free);
    INIT_AM(realloc);
    INIT_AM(mmap);
    INIT_AM(munmap);
    INIT_AM(mremap);
    INIT_AM(unknown);
    
    atoms_need_init = 0;

    UNLOCK;

}

#endif /* #ifndef NO_SL_ALLOC_STAT_ETERM */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Exported functions                                                        *
\*                                                                           */

/* -- erts_sl_alloc() ------------------------------------------------------ */

void *
SL_ALLOC(Uint size)
{
    void *res;

    ASSERT(initialized);

    if (sl_alloc_disabled)
	return MALLOC(size);

    if (use_old_sl_alloc)
	return erts_old_sl_alloc(size);

    if (!size)
	return NULL;

    LOCK;

    INC_CC(sl_alloc_calls);

    if (size >= sbc_threshold) {
	SBCarrier_t *sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
							 B2U(size),
							 0);
	res = sbc ? SBC2MEM(sbc) : NULL;
    }
    else
	res = mbc_alloc(size);

    UNLOCK;

    return res;
}


/* -- erts_sl_realloc() ---------------------------------------------------- */

void *
SL_REALLOC(void *p, Uint save_size, Uint size)
{
    Block_t *blk;
    void *res;

    ASSERT(initialized);

    if (sl_alloc_disabled)
	return REALLOC(p, size);

    if (use_old_sl_alloc)
	return erts_old_sl_realloc(p, save_size, size);

    if (!p) {
	if (size) {
	    res = SL_ALLOC(size);
	    LOCK;
	    INC_CC(sl_realloc_calls);
	    DEC_CC(sl_alloc_calls);
	    UNLOCK;
	    return res;
	}
	return NULL;
    }

    if (!size) {
	ASSERT(p);
	SL_FREE(p);
	LOCK;
	INC_CC(sl_realloc_calls);
	DEC_CC(sl_free_calls);
	UNLOCK;
	return NULL;
    }

    LOCK;

    INC_CC(sl_realloc_calls);
    
    blk = MEM2BLK(p);

    if (size < sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(p, size);
	else {
	    Uint units = B2U(size);
#if USE_MREMAP
	    if (100*((float) (page_units-SBC_HDR_UNITS-ABLK_HDR_UNITS-units))
		/((float) page_units) < (float) sbc_move_threshold)
		/* Data won't be copied into a new carrier... */
		goto do_carrier_resize;	    
#else
	    Uint carrier_units = CARRIER_UNITS(BLK_CARRIER(blk));


	    if (100*((float) (carrier_units-SBC_HDR_UNITS-ABLK_HDR_UNITS-units))
		/((float) carrier_units) < (float) sbc_move_threshold) {
		/* Data won't be moved into a new carrier... */
		ASSERT(carrier_units - SBC_HDR_UNITS >= units);
		goto do_carrier_resize;
	    }
#endif

	    res = mbc_alloc(size);
	    if (res) {
		MEMCPY((void*) res,
		       (void*) p,
		       MIN(U2B(BLK_UNITS(blk) - ABLK_HDR_UNITS), size));
		carrier_free(BLK_CARRIER(blk));
	    }
	}
    }
    else {
	SBCarrier_t *sbc;
	if(IS_SBC_BLK(blk)) {
	do_carrier_resize:
	    sbc = sb_carrier_resize((SBCarrier_t *) BLK_CARRIER(blk),
				    B2U(size),
				    0);
	    res = sbc ? SBC2MEM(sbc) : NULL;
	}
	else {
	    sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
						B2U(size),
						0);
	    if (sbc) {
		res = SBC2MEM(sbc);
		MEMCPY((void*) res,
		       (void*) p,
		       MIN(U2B(BLK_UNITS(blk) - ABLK_HDR_UNITS), size));
		mbc_free(p);
	    }
	    else
		res = NULL;
	}
    }

    UNLOCK;

    return res;
}

/* -- erts_sl_free() ------------------------------------------------------- */

void
SL_FREE(void *p)
{
    ASSERT(initialized);

    if (sl_alloc_disabled) {
	FREE(p);
	return;
    }

    if (use_old_sl_alloc) {
	erts_old_sl_free(p);
	return;
    }

    if (p) {
	Block_t *blk;

	LOCK;
	
	INC_CC(sl_free_calls);

	blk = MEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    carrier_free(BLK_CARRIER(blk));
	else
	    mbc_free(p);

	UNLOCK;
    }
}

/* -- erts_sl_alloc_info() ------------------------------------------------- */

void
SL_ALLOC_INFO(CIO to)
{
    ErtsSlAllocStat esas;
    Uint carriers_size;
    Uint blocks_size;
    Uint adm_size;
    Uint blocks;
    Uint carriers;
    Uint mmaps;

    ASSERT(initialized);

    if(sl_alloc_disabled) {
	erl_printf(to, "sl_alloc: disabled\n");
	return;
    }

    if(use_old_sl_alloc) {
	erts_old_sl_alloc_info(to);
	return;
    }

    SL_ALLOC_STAT(&esas);

    carriers_size = (esas.singleblock.mmap.carriers_size
		     + esas.singleblock.malloc.carriers_size
		     + esas.multiblock.mmap.carriers_size
		     + esas.multiblock.malloc.carriers_size);
    blocks_size = (esas.singleblock.mmap.blocks_size
		   + esas.singleblock.malloc.blocks_size
		   + esas.multiblock.mmap.blocks_size
		   + esas.multiblock.malloc.blocks_size);
    adm_size = (esas.singleblock.mmap.adm_size
		+ esas.singleblock.malloc.adm_size
		+ esas.multiblock.mmap.adm_size
		+ esas.multiblock.malloc.adm_size);
    blocks = (esas.singleblock.mmap.no_blocks
	      + esas.singleblock.malloc.no_blocks
	      + esas.multiblock.mmap.no_blocks
	      + esas.multiblock.malloc.no_blocks);
    carriers = (esas.singleblock.mmap.no_carriers
		+ esas.singleblock.malloc.no_carriers
		+ esas.multiblock.mmap.no_carriers
		+ esas.multiblock.malloc.no_carriers);
    mmaps = (esas.singleblock.mmap.no_carriers
	     + esas.multiblock.mmap.no_carriers);
    
    erl_printf(to, "sl_alloc: carriers size(%u), blocks size(%u), "
	       "adm size(%u), free size(%u), blocks(%u), carriers(%u), "
	       "mmaps(%u)\n", carriers_size, blocks_size, adm_size,
	       carriers_size - blocks_size - adm_size, blocks, carriers, mmaps);
}

/* -- erts_sl_alloc_stat() ------------------------------------------------- */

#ifndef NO_SL_ALLOC_STAT_ETERM

Eterm
SL_ALLOC_STAT_ETERM(Process *p)
{
#undef  ADD_2TUP
#undef  ADD_3TUP
#undef  MAX_HP_WORDS
#define ADD_2TUP(L, F, S)    L = CONS(hp+3, TUPLE2(hp, F, S), L);    hp += 3+2
#define ADD_3TUP(L, F, S, T) L = CONS(hp+4, TUPLE3(hp, F, S, T), L); hp += 4+2
#define MAX_HEAP_WORDS ((4+2 /* 3-tup + cons*/)*9 \
			+ (3+2 /* 2-tup + cons*/)*41 \
			+ (MAX(sizeof(ERTS_SL_ALLOC_VERSION), \
			       sizeof(ERTS_OLD_SL_ALLOC_VERSION))-1)*2 /*cons*/)
    Eterm *hp;
    Eterm list;
    ErtsSlAllocStat esas;
    Eterm res;
#ifdef DEBUG
    Eterm *hp_end;
#endif

    SL_ALLOC_STAT(&esas);

    if (!esas.sl_alloc_enabled) {
	Eterm INIT_AM(disabled);
	return AM_disabled;
    }

    if (atoms_need_init)
	init_atoms();

    hp = HAlloc(p, MAX_HEAP_WORDS);

#ifdef DEBUG
    hp_end = hp + MAX_HEAP_WORDS;
#endif

    res = NIL;

    /* Calls ... */

    if (!esas.old_sl_alloc_enabled) {
#undef  ADD_CALLS
#define ADD_CALLS(A, CC)						\
	list = ADD_3TUP(list,						\
			A,						\
			make_small_or_big(CC.giga_calls, p),		\
			make_small_or_big(CC.calls, p))

	list = NIL;
	ADD_CALLS(AM_realloc,    esas.realloc_calls);
	ADD_CALLS(AM_free,       esas.free_calls);
	ADD_CALLS(AM_malloc,     esas.malloc_calls);
#if USE_MMAP
#if USE_MREMAP
	ADD_CALLS(AM_mremap,     esas.mremap_calls);
#endif
	ADD_CALLS(AM_munmap,     esas.munmap_calls);
	ADD_CALLS(AM_mmap,       esas.mmap_calls);
#endif
	ADD_CALLS(AM_sl_realloc, esas.sl_realloc_calls);
	ADD_CALLS(AM_sl_free,    esas.sl_free_calls);
	ADD_CALLS(AM_sl_alloc,   esas.sl_alloc_calls);

#undef  ADD_CALLS

	ADD_2TUP(res, AM_calls, list);
    }

    /* Carriers state ... */

#define ADD_CARRIERS_STAT_BASE(BTYPE, ATYPE)				\
    if (esas.BTYPE.ATYPE.no_carriers) {					\
	Eterm tmp_ = NIL;						\
	ADD_2TUP(tmp_,							\
		 AM_adm_size,						\
		 esas.BTYPE.ATYPE.adm_size == 0				\
		 ? AM_unknown						\
		 : make_small_or_big(esas.BTYPE.ATYPE.adm_size, p));	\
	ADD_2TUP(tmp_,							\
		 AM_blocks_size,					\
		 make_small_or_big(esas.BTYPE.ATYPE.blocks_size, p));	\
	ADD_2TUP(tmp_,							\
		 AM_carriers_size,					\
		 make_small_or_big(esas.BTYPE.ATYPE.carriers_size, p));	\
	ADD_2TUP(tmp_,							\
		 AM_blocks,						\
		 make_small_or_big(esas.BTYPE.ATYPE.no_blocks, p));	\
	ADD_2TUP(tmp_,							\
		 AM_carriers,						\
		 make_small_or_big(esas.BTYPE.ATYPE.no_carriers, p));	\
	ADD_2TUP(list, AM_ ## ATYPE, tmp_);				\
    }

#define ADD_CARRIERS_STAT(BTYPE)					\
    list = NIL;								\
    ADD_CARRIERS_STAT_BASE(BTYPE, malloc);				\
    ADD_CARRIERS_STAT_BASE(BTYPE, mmap);				\
    if (esas.BTYPE.max_blocks) {					\
	ADD_2TUP(list,							\
		 AM_max_blocks_size,					\
		 make_small_or_big(esas.BTYPE.max_blocks_size, p));	\
	ADD_2TUP(list,							\
		 AM_max_blocks,						\
		 make_small_or_big(esas.BTYPE.max_blocks, p));		\
    }									\
    ADD_2TUP(res,  AM_ ## BTYPE ## _carriers, list)

    ADD_CARRIERS_STAT(singleblock);    /* Adds a maximum of 15 2tup+cons */
    if (!esas.old_sl_alloc_enabled) {
	ADD_CARRIERS_STAT(multiblock); /* Adds a maximum of 15 2tup+cons */
    }

#undef  ADD_CARRIERS_STAT
#undef  ADD_CARRIERS_STAT_BASE

    if (!esas.old_sl_alloc_enabled) {
	ADD_2TUP(res,
		 AM_carrier_order_search,
		 esas.carrier_order_search ? am_true : am_false);
	ADD_2TUP(res,
		 AM_main_carrier_size,
		 make_small_or_big(esas.main_carrier_size, p));
	ADD_2TUP(res,
		 AM_smallest_multiblock_carrier_size,
		 make_small_or_big(esas.smallest_multiblock_carrier_size,
				   p));
	ADD_2TUP(res,
		 AM_largest_multiblock_carrier_size,
		 make_small_or_big(esas.largest_multiblock_carrier_size, p));
	ADD_2TUP(res,
		 AM_multiblock_carrier_growth_ratio,
		 make_small_or_big(esas.multiblock_carrier_growth_ratio, p));
	ADD_2TUP(res,
		 AM_max_block_search_depth,
		 make_small_or_big(esas.max_block_search_depth, p));
    }
    ADD_2TUP(res,
	     AM_max_mmap_carriers,
	     make_small_or_big(esas.max_mmap_carriers, p));
    ADD_2TUP(res,
	     AM_singleblock_carrier_move_threshold,
	     make_small_or_big(esas.singleblock_carrier_move_threshold, p));
    ADD_2TUP(res,
	     AM_singleblock_carrier_threshold,
	     make_small_or_big(esas.singleblock_carrier_threshold, p));

    if (esas.old_sl_alloc_enabled)
	list = buf_to_intlist(&hp,
			      (byte*) ERTS_OLD_SL_ALLOC_VERSION,
			      sizeof(ERTS_OLD_SL_ALLOC_VERSION) - 1,
			      NIL);
    else
	list = buf_to_intlist(&hp,
			      (byte*) ERTS_SL_ALLOC_VERSION,
			      sizeof(ERTS_SL_ALLOC_VERSION) - 1,
			      NIL);
    ADD_2TUP(res, am_version, list);

    ASSERT(hp <= hp_end);

    HRelease(p, hp);

    return res;

#undef ADD_2TUP
#undef ADD_3TUP
}

#endif /* #ifndef NO_SL_ALLOC_STAT_ETERM */

/* -- erts_sl_alloc_stat() ------------------------------------------------- */

void
SL_ALLOC_STAT(ErtsSlAllocStat *p)
{
    ASSERT(initialized);

    
    if (use_old_sl_alloc) {
	ErtsOldSlAllocStat eosas;
	erts_old_sl_alloc_stat(&eosas);
	p->sl_alloc_enabled = eosas.sl_alloc_enabled;
	p->old_sl_alloc_enabled = 1;
	p->singleblock_carrier_threshold = eosas.mmap_threshold;
	p->singleblock_carrier_move_threshold = 80;
	p->max_mmap_carriers = eosas.mmap_max;
	p->smallest_multiblock_carrier_size = 0;
	p->largest_multiblock_carrier_size = 0;
	p->multiblock_carrier_growth_ratio = 0;
	p->carrier_order_search = 0;
	p->main_carrier_size = 0;
	p->singleblock.max_blocks = 0;
	p->singleblock.max_blocks_size = 0;
	p->singleblock.mmap.no_carriers = eosas.mmapped_chunks;
	p->singleblock.mmap.no_blocks = eosas.mmapped_chunks;
	p->singleblock.mmap.carriers_size = eosas.mmapped_chunks_size;
	p->singleblock.mmap.blocks_size = eosas.mmapped_blocks_size;
	p->singleblock.mmap.adm_size = 0;
	p->singleblock.malloc.no_carriers = 0;
	p->singleblock.malloc.no_blocks = 0;
	p->singleblock.malloc.carriers_size = 0;
	p->singleblock.malloc.blocks_size = 0;
	p->singleblock.malloc.adm_size = 0;
	p->multiblock.max_blocks = 0;
	p->multiblock.max_blocks_size = 0;
	p->multiblock.mmap.no_carriers = 0;
	p->multiblock.mmap.no_blocks = 0;
	p->multiblock.mmap.carriers_size = 0;
	p->multiblock.mmap.blocks_size = 0;
	p->multiblock.mmap.adm_size = 0;
	p->multiblock.malloc.no_carriers = 0;
	p->multiblock.malloc.no_blocks = 0;
	p->multiblock.malloc.carriers_size = 0;
	p->multiblock.malloc.blocks_size = 0;
	p->multiblock.malloc.adm_size = 0;

	ZERO_CC(p->mmap_calls);
	ZERO_CC(p->munmap_calls);
	ZERO_CC(p->mremap_calls);
	ZERO_CC(p->malloc_calls);
	ZERO_CC(p->realloc_calls);
	ZERO_CC(p->free_calls);
	ZERO_CC(p->sl_alloc_calls);
	ZERO_CC(p->sl_realloc_calls);
	ZERO_CC(p->sl_free_calls);

	return;
    }

    p->sl_alloc_enabled = !sl_alloc_disabled;
    p->old_sl_alloc_enabled = 0;

    p->singleblock_carrier_threshold = sbc_threshold;
    p->singleblock_carrier_move_threshold = sbc_move_threshold;
    p->max_mmap_carriers = max_mmap_carriers;
    p->carrier_order_search = use_carrier_order_search;
    p->main_carrier_size = U2B(main_carrier_units);
    p->smallest_multiblock_carrier_size = U2B(smallest_mbc_units);
    p->largest_multiblock_carrier_size = U2B(largest_mbc_units);
    p->multiblock_carrier_growth_ratio = mbc_growth_ratio;
    p->max_block_search_depth = max_blk_search;

    p->singleblock.max_blocks = max_sbc_blks;
    p->singleblock.max_blocks_size = U2B(max_sbc_blk_units);

    p->singleblock.mmap.no_carriers = mmap_sb_carriers;
    p->singleblock.mmap.no_blocks = mmap_sb_carriers;
    p->singleblock.mmap.carriers_size = U2B(tot_mmap_sb_carrier_units);
    p->singleblock.mmap.blocks_size = (U2B(tot_mmap_sbc_blk_units
					    - (mmap_sb_carriers
					       *ABLK_HDR_UNITS)));
    p->singleblock.mmap.adm_size = U2B(mmap_sb_carriers
					*SBC_HDR_UNITS);

    p->singleblock.malloc.no_carriers = malloc_sb_carriers;
    p->singleblock.malloc.no_blocks = malloc_sb_carriers;
    p->singleblock.malloc.carriers_size = U2B(tot_malloc_sb_carrier_units);
    p->singleblock.malloc.blocks_size = (U2B(tot_malloc_sbc_blk_units
					      - (malloc_sb_carriers
						 *ABLK_HDR_UNITS)));
    p->singleblock.malloc.adm_size = U2B(malloc_sb_carriers
					  *SBC_HDR_UNITS);

    p->multiblock.max_blocks = max_mbc_blks;
    p->multiblock.max_blocks_size = U2B(max_mbc_blk_units);

    p->multiblock.mmap.no_carriers = mmap_mb_carriers;
    p->multiblock.mmap.no_blocks = mmap_mbc_blks;
    p->multiblock.mmap.carriers_size = U2B(tot_mmap_mb_carrier_units);
    p->multiblock.mmap.blocks_size = (U2B(tot_mmap_mbc_blk_units
					    - (mmap_mbc_blks
					       *ABLK_HDR_UNITS)));
    p->multiblock.mmap.adm_size = U2B(mmap_mb_carriers*MBC_HDR_UNITS
				       + mmap_mbc_blks*ABLK_HDR_UNITS);

    p->multiblock.malloc.no_carriers = malloc_mb_carriers;
    p->multiblock.malloc.no_blocks = malloc_mbc_blks;
    p->multiblock.malloc.carriers_size = U2B(tot_malloc_mb_carrier_units);
    p->multiblock.malloc.blocks_size = (U2B(tot_malloc_mbc_blk_units
					    - (malloc_mbc_blks
					       *ABLK_HDR_UNITS)));
    p->multiblock.malloc.adm_size = U2B(malloc_mb_carriers*MBC_HDR_UNITS
					 + malloc_mbc_blks*ABLK_HDR_UNITS);
    
    p->mmap_calls = mmap_calls;
    p->munmap_calls = munmap_calls;
    p->mremap_calls = mremap_calls;
    p->malloc_calls = malloc_calls;
    p->realloc_calls = realloc_calls;
    p->free_calls = free_calls;
    p->sl_alloc_calls = sl_alloc_calls;
    p->sl_realloc_calls = sl_realloc_calls;
    p->sl_free_calls = sl_free_calls;

}

/* -- erts_sl_alloc_init() ------------------------------------------------- */

void
SL_ALLOC_INIT(ErtsSlAllocInit *arg)
{
    int i;
    Uint page_size;

    atoms_need_init = 1;

    /* Set defaults for arguments that not have been specified ... */
    if (arg->esla < 0)
	arg->esla = ERTS_SL_ALLOC_DEFAULT_ENABLED;
    if (arg->eosla < 0)
	arg->eosla = ERTS_SL_ALLOC_DEFAULT_OLD_ENABLED;
    if (arg->mcs < 0)
	arg->mcs = ERTS_SL_ALLOC_DEFAULT_MAIN_CARRIER_SIZE;
    if (arg->sbcmt < 0)
	arg->sbcmt = ERTS_SL_ALLOC_DEFAULT_SBC_MOVE_THRESHOLD;
    if (arg->sbct < 0)
	arg->sbct = ERTS_SL_ALLOC_DEFAULT_SBC_THRESHOLD;
    if (arg->mmc < 0)
	arg->mmc = ERTS_SL_ALLOC_DEFAULT_MAX_MMAP_CARRIERS;
    if (arg->cos < 0)
	arg->cos = ERTS_SL_ALLOC_DEFAULT_CARRIER_ORDER_SEARCH;
    if (arg->scs < 0)
	arg->scs = ERTS_SL_ALLOC_DEFAULT_SMALLEST_CARRIER_SIZE;
    if (arg->lcs < 0)
	arg->lcs = ERTS_SL_ALLOC_DEFAULT_LARGEST_CARRIER_SIZE;
    if (arg->cgr < 0)
	arg->cgr = ERTS_SL_ALLOC_DEFAULT_CARRIER_GROWTH_RATIO;
    if (arg->mbsd < 0)
	arg->mbsd = ERTS_SL_ALLOC_DEFAULT_MAX_BLOCK_SEARCH_DEPTH;

#ifdef THREAD_SAFE_SL_ALLOC
    thread_safe_init();
#endif

    use_old_sl_alloc = arg->eosla;
    sl_alloc_disabled = !arg->esla;

    if (use_old_sl_alloc) {
	erts_old_sl_alloc_init(arg->esla);
	erts_old_sl_alloc_opt(ERTS_SL_ALLOC_OPT_SBC_THRESHOLD, arg->sbct);
	erts_old_sl_alloc_opt(ERTS_SL_ALLOC_OPT_MAX_MMAP_CARRIERS, arg->mmc);
#ifdef DEBUG
	initialized = 1;
#endif
	return;
    }

    if (sl_alloc_disabled) {
#ifdef DEBUG
	initialized = 1;
#endif
	return;
    }

    main_carrier_units = arg->mcs ? B2U(arg->mcs) : 0;
    max_blk_search = arg->mbsd = MAX(1, arg->mbsd);
    mbc_growth_ratio = arg->cgr;
    smallest_mbc_units = B2U(arg->scs);
    largest_mbc_units = B2U(arg->lcs);
    use_carrier_order_search = arg->cos;
    max_mmap_carriers = arg->mmc;
    set_sbc_threshold(arg->sbct);
    sbc_move_threshold = arg->sbcmt;

    mbc_growth_fact[0] = 1.0;
    max_mbcgf_calced = 0;

    non_empty_buckets		= (BucketMask_t) 0;
    for (i = 0; i < NO_OF_BKTS; i++) {
 	common_buckets[i]	= NULL;
	blocks_in_buckets[i]	= 0;
    }

    first_mb_carrier		= NULL;
    last_mb_carrier		= NULL;
    last_aux_mb_carrier		= NULL;

    mmap_sb_carriers		= 0;
    malloc_sb_carriers		= 0;
    mmap_mb_carriers		= 0;
    malloc_mb_carriers		= 0;

    tot_mmap_sb_carrier_units	= 0;
    tot_malloc_sb_carrier_units	= 0;
    tot_mmap_mb_carrier_units	= 0;
    tot_malloc_mb_carrier_units	= 0;

    mmap_mbc_blks		= 0;
    malloc_mbc_blks		= 0;
    max_mbc_blks                = 0;

    tot_mmap_sbc_blk_units	= 0;
    tot_malloc_sbc_blk_units	= 0;
    tot_mmap_mbc_blk_units	= 0;
    tot_malloc_mbc_blk_units	= 0;
    max_mbc_blk_units		= 0;

    ZERO_CC(mmap_calls);
    ZERO_CC(munmap_calls);
    ZERO_CC(mremap_calls);
    ZERO_CC(malloc_calls);
    ZERO_CC(free_calls);
    ZERO_CC(realloc_calls);
    ZERO_CC(sl_alloc_calls);
    ZERO_CC(sl_free_calls);
    ZERO_CC(sl_realloc_calls);

#if USE_MMAP
    page_size = GET_PAGE_SIZE;

    if (page_size % sizeof(Unit_t)) /* A little paranoid... */
	erl_exit(-1, "Page size (%d) not evenly divideble by internal unit "
		 "size of sl_alloc (%d)\n", page_size, sizeof(Unit_t));

    page_units = B2U(page_size);

#ifndef MAP_ANON
    mmap_fd = GET_MMAP_FD;
#endif
#endif /* #if USE_MMAP */

    if (main_carrier_units) {
	MBCarrier_t *mbc;
	mbc = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK,
					    main_carrier_units,
					    CA_FLAG_FORCE_MALLOC
					    | CA_FLAG_MAIN_CARRIER);
	if (!mbc)
	    erl_exit(-1,
		     "Failed to allocate sl_alloc main carrier (size=%d)\n",
		     U2B(main_carrier_units));
	
	link_free_block((FreeBlock_t *) MBC2MEM(mbc));

	ASSERT(IS_MAIN_CARRIER(mbc));
#if HARD_DEBUG
	check_mb_carrier(mbc);
#endif
    }


#ifdef DEBUG
    initialized = 1;
#endif    
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */

#if HARD_DEBUG

static void
check_sb_carrier(SBCarrier_t *c)
{
    AllocBlock_t *blk = SBC2BLK(c);

    ASSERT(IS_SB_CARRIER(c));
    ASSERT(IS_AUX_CARRIER(c));
    ASSERT(IS_SBC_BLK(blk));
    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(CARRIER_UNITS(c) - SBC_HDR_UNITS >= BLK_UNITS(blk));
    if (IS_MMAP_CARRIER(c)) {
	ASSERT(CARRIER_UNITS(c) % page_units == 0);
    }
}

static void
check_mb_carrier(MBCarrier_t *c)
{
    Uint units;
    int i;
    int bi;
    int found;
    int first_blk = 1;
    Block_t *prev_blk;
    Block_t *blk;
    FreeBlock_t *fblk;
    FreeBlock_t **buckets;
    BucketMask_t non_mt_bkts;

    ASSERT(IS_MB_CARRIER(c));
    if (IS_MMAP_CARRIER(c)) {
	ASSERT(CARRIER_UNITS(c) % page_units == 0);
    }

    blk = (Block_t *) MBC2MEM(c);

    if (use_carrier_order_search) {
	buckets = c->buckets;
	non_mt_bkts = c->non_empty_buckets;
    }
    else {
	buckets = common_buckets;
	non_mt_bkts = non_empty_buckets;
    }

    ASSERT(IS_FIRST_BLK(blk));

    do {
	ASSERT(c == (MBCarrier_t *) BLK_CARRIER(blk));
	ASSERT(IS_MBC_BLK(blk));
	units = BLK_UNITS(blk);

	if(IS_FREE_BLK(blk)) {
	    if (((Unit_t *) blk) + units < ((Unit_t *) c) + CARRIER_UNITS(c)) {
		ASSERT(*((Uint *) (((Unit_t *) blk) + units - 1)) == units);
	    }

	    bi = bucket_index(units);

	    ASSERT(non_mt_bkts & ((BucketMask_t) 1) << bi);

	    found = 0;
	    for (fblk = buckets[bi]; fblk; fblk = fblk->next)
		if (blk == (Block_t *) fblk)
		    found++;
	    ASSERT(found == 1);
	}
	else
	    bi = -1;

	found = 0;
	for (i = 0; i < NO_OF_BKTS; i++) {
	    if (i == bi)
		continue; /* Already checked */
	    for (fblk = buckets[i]; fblk; fblk = fblk->next)
		if (blk == (Block_t *) fblk)
		    found++;
	}

	ASSERT(found == 0);

	if (first_blk)
	    first_blk = 0;
	else {
	    ASSERT(NXT_BLK(prev_blk) == blk);
	    if (IS_FREE_BLK(prev_blk)) {
		ASSERT(IS_PREV_BLK_FREE(blk));
		ASSERT(prev_blk == PREV_BLK(blk));
	    }
	    else {
		ASSERT(IS_PREV_BLK_ALLOCED(blk));
	    }
	}
	prev_blk = blk;
	blk = NXT_BLK(blk);
    } while (((Unit_t *) blk) < ((Unit_t *) c) + CARRIER_UNITS(c));

    ASSERT(IS_LAST_BLK(prev_blk));

    for(bi = 0; bi < NO_OF_BKTS; bi++) {
	if (use_carrier_order_search)
	    ASSERT(c->non_empty_buckets & (((BucketMask_t) 1) << bi)
		   ? non_empty_buckets & (((BucketMask_t) 1) << bi)
		   : 1);
	ASSERT(non_mt_bkts & (((BucketMask_t) 1) << bi)
	       ? buckets[bi] != NULL
	       : buckets[bi] == NULL);
    }


}

#endif
