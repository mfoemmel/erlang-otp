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
 * 1. Optimize.
 * 2. Cleanup the code.
 * ...
 *
 * Here are parts of the "will probably be done in the future" list
 * (unordered):
 * * Other special treatment of sbcs than malloc of sbcs when high
 *   sbc load.
 * * Implement special treatment of blocks smaller than the minimum
 *   block size.
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
#define MEMZERO			sys_memzero

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

#define NO_OF_BKT_IX_BITS (8)
#ifdef ARCH_64
#  define SUB_MASK_IX_SHIFT (6)
#else
#  define SUB_MASK_IX_SHIFT (5)
#endif
#define NO_OF_BKTS (((Word_t)1) << NO_OF_BKT_IX_BITS)
#define NO_OF_SUB_MASKS (NO_OF_BKTS/(((Word_t)1) << SUB_MASK_IX_SHIFT))
#define MAX_SUB_MASK_IX \
  ((((Word_t)1) << (NO_OF_BKT_IX_BITS - SUB_MASK_IX_SHIFT)) - 1)
#define MAX_SUB_BKT_IX ((((Word_t)1) << SUB_MASK_IX_SHIFT) - 1)
#define MAX_BKT_IX (NO_OF_BKTS - 1)

#define IX2SBIX(IX) ((IX) & (~(~((Word_t)0) << SUB_MASK_IX_SHIFT)))
#define IX2SMIX(IX) ((IX) >> SUB_MASK_IX_SHIFT)
#define MAKE_BKT_IX(SMIX, SBIX)	\
  ((((Word_t)(SMIX)) << SUB_MASK_IX_SHIFT) | ((Word_t)(SBIX)))

#define SET_BKT_MASK_IX(IX)						\
do {									\
    int sub_mask_ix__ = IX2SMIX((IX));					\
    bucket_masks.main |= (((Word_t)1) << sub_mask_ix__);		\
    bucket_masks.sub[sub_mask_ix__] |= (((Word_t)1) << IX2SBIX((IX)));	\
} while (0)

#define UNSET_BKT_MASK_IX(IX)						\
do {									\
    int sub_mask_ix__ = IX2SMIX((IX));					\
    bucket_masks.sub[sub_mask_ix__] &= ~(((Word_t)1) << IX2SBIX((IX)));	\
    if (!bucket_masks.sub[sub_mask_ix__])				\
	bucket_masks.main &= ~(((Word_t)1) << sub_mask_ix__);		\
} while (0)
	

typedef union {char c[8]; long l; double d;} Unit_t;
typedef Uint Word_t;

typedef struct {
    Word_t misc;
} Carrier_t;

typedef struct {
    Word_t misc;
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
} MBCarrier_t;

typedef struct {
    Word_t main;
    Word_t sub[NO_OF_SUB_MASKS];
} BucketMask_t; 

static BucketMask_t bucket_masks;
static FreeBlock_t *buckets[NO_OF_BKTS];
static Uint max_blk_search;

static int sl_alloc_disabled;
static int use_old_sl_alloc;

static Uint main_carrier_size;
static Carrier_t *main_carrier;

/* Double linked list of all multi block carriers */
static MBCarrier_t *first_mb_carrier;
static MBCarrier_t *last_mb_carrier;
static char *last_aux_mb_carrier_start;
static char *last_aux_mb_carrier_end;


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

static Uint no_of_mmap_sbcs;
static Uint no_of_malloc_sbcs;
static Uint max_no_of_sbcs;
static Uint max_no_of_sbcs_ever;
static Uint mmap_sbcs_total_size;
static Uint malloc_sbcs_total_size;
static Uint max_sbcs_total_size;
static Uint max_sbcs_total_size_ever;

static Uint sbc_blocks_total_size;
static Uint max_sbc_blocks_total_size;
static Uint max_sbc_blocks_total_size_ever;

static Uint no_of_mmap_mbcs;
static Uint no_of_malloc_mbcs;
static Uint max_no_of_mbcs;
static Uint max_no_of_mbcs_ever;
static Uint mmap_mbcs_total_size;
static Uint malloc_mbcs_total_size;
static Uint max_mbcs_total_size;
static Uint max_mbcs_total_size_ever;

static Uint no_of_mbc_blocks;
static Uint max_no_of_mbc_blocks;
static Uint max_no_of_mbc_blocks_ever;
static Uint mbc_blocks_total_size;
static Uint max_mbc_blocks_total_size;
static Uint max_mbc_blocks_total_size_ever;

/* Single block carrier threshold
 * (blocks >= sbc_threshold will be allocated
 * in a single block carrier).
 */
static Uint sbc_threshold;
static Uint sbc_shrink_threshold;
static double sbc_move_threshold;
/* Max number of mmap carriers */
static Uint max_mmap_carriers;

static Uint mbc_growth_stages;
static Uint smallest_mbc_size;
static Uint largest_mbc_size;

/* Used by BKT_IX() */
static Uint bkt_max_size_d;
static Uint bkt_intrvl_d;

#if USE_MMAP
static Uint page_size;
#endif

#ifndef NO_SL_ALLOC_STAT_ETERM

/* Atoms used by erts_sl_alloc_stat_eterm() ... */
static Eterm AM_singleblock_carrier_threshold;
static Eterm AM_singleblock_carrier_shrink_threshold;
static Eterm AM_singleblock_carrier_move_threshold;
static Eterm AM_mmap_singleblock_carrier_load_threshold;
static Eterm AM_max_mmap_carriers;
static Eterm AM_singleblock_carriers;
static Eterm AM_multiblock_carriers;
static Eterm AM_main_carrier_size;
static Eterm AM_smallest_multiblock_carrier_size;
static Eterm AM_largest_multiblock_carrier_size;
static Eterm AM_multiblock_carrier_growth_stages;
static Eterm AM_max_block_search_depth;
static Eterm AM_malloc;
static Eterm AM_mmap;
static Eterm AM_carriers;
static Eterm AM_blocks;
static Eterm AM_carriers_size;
static Eterm AM_blocks_size;
static Eterm AM_adm_size;
static Eterm AM_max_carriers;
static Eterm AM_max_blocks;
static Eterm AM_max_carriers_size;
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
static Eterm AM_release;
static Eterm AM_settings;

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
#define CEILING(X, I)  ((((X) - 1)/(I) + 1)*(I))

#undef  WORD_MASK
#define INV_WORD_MASK	((Word_t) (sizeof(Word_t) - 1))
#define WORD_MASK	(~INV_WORD_MASK)
#define WORD_FLOOR(X)	((X) & WORD_MASK)
#define WORD_CEILING(X)	WORD_FLOOR((X) + INV_WORD_MASK)

#undef  UNIT_MASK
#define INV_UNIT_MASK	((Word_t) (sizeof(Unit_t) - 1))
#define UNIT_MASK	(~INV_UNIT_MASK)
#define UNIT_FLOOR(X)	((X) & UNIT_MASK)
#define UNIT_CEILING(X)	UNIT_FLOOR((X) + INV_UNIT_MASK)

#undef  PAGE_MASK
#define INV_PAGE_MASK	((Word_t) (page_size - 1))
#define PAGE_MASK	(~INV_PAGE_MASK)
#define PAGE_FLOOR(X)	((X) & PAGE_MASK)
#define PAGE_CEILING(X)	PAGE_FLOOR((X) + INV_PAGE_MASK)

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

#define DO_MUNMAP(C, SZ) \
  (INC_CC(munmap_calls), munmap((void *) (C), (size_t) (SZ)))

#ifdef DEBUG
#define MUNMAP(C, SZ) ASSERT(0 == DO_MUNMAP((C), (SZ)))
#else
#define MUNMAP(C, SZ) ((void) DO_MUNMAP((C), (SZ)))
#endif

#if HAVE_MREMAP
#ifndef MREMAP_MAYMOVE
#define MREMAP_MAYMOVE 1
#endif

#define MREMAP(C, S) \
  (INC_CC(mremap_calls), \
   mremap((void *) (C), (size_t) CARRIER_SZ((C)), (S), MREMAP_MAYMOVE))

#endif /* #if HAVE_MREMAP */

#undef  MALLOC
#undef  REALLOC
#undef  FREE

#define MALLOC(S)	(INC_CC(malloc_calls),	MALLOC_FUNC((S)))
#define REALLOC(P, S)	(INC_CC(realloc_calls),	REALLOC_FUNC((P), (S)))
#define FREE(P)		(INC_CC(free_calls),	FREE_FUNC((P)))

/* ... */

#define SZ_MASK			(~((Word_t) 0) << 3)
#define FLG_MASK		(~(SZ_MASK))

/* Blocks ... */

#define SBC_BLK_FTR_FLG		(((Word_t) 1) << 0)
#define UNUSED1_BLK_FTR_FLG	(((Word_t) 1) << 1)
#define UNUSED2_BLK_FTR_FLG	(((Word_t) 1) << 2)

#define MIN_BLK_SZ  UNIT_CEILING(sizeof(FreeBlock_t) + sizeof(Word_t))
#define ABLK_HDR_SZ (sizeof(AllocBlock_t))
#define FBLK_FTR_SZ (sizeof(Word_t))

#define UMEMSZ2BLKSZ(SZ)						\
  (ABLK_HDR_SZ + (SZ) <= MIN_BLK_SZ					\
   ? MIN_BLK_SZ								\
   : UNIT_CEILING(ABLK_HDR_SZ + (SZ)))

#define UMEM2BLK(P) ((Block_t *) (((char *) (P)) - ABLK_HDR_SZ))
#define BLK2UMEM(P) ((void *)    (((char *) (P)) + ABLK_HDR_SZ))

#define PREV_BLK_SZ(B) \
  ((Uint) (*(((Word_t *) (B)) - 1) & SZ_MASK))

#define SET_BLK_SZ_FTR(B, SZ) \
  (*((Word_t *) (((char *) (B)) + (SZ) - sizeof(Word_t))) = (SZ))

#define THIS_FREE_BLK_HDR_FLG 	(((Word_t) 1) << 0)
#define PREV_FREE_BLK_HDR_FLG 	(((Word_t) 1) << 1)
#define LAST_BLK_HDR_FLG 	(((Word_t) 1) << 2)

#define SET_BLK_SZ(B, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (((Block_t *) (B))->misc = ((((Block_t *) (B))->misc & FLG_MASK) | (SZ))))
#define SET_BLK_FREE(B) \
  (((Block_t *) (B))->misc |= THIS_FREE_BLK_HDR_FLG)
#define SET_BLK_ALLOCED(B) \
  (((Block_t *) (B))->misc &= ~THIS_FREE_BLK_HDR_FLG)
#define SET_PREV_BLK_FREE(B) \
  (((Block_t *) (B))->misc |= PREV_FREE_BLK_HDR_FLG)
#define SET_PREV_BLK_ALLOCED(B) \
  (((Block_t *) (B))->misc &= ~PREV_FREE_BLK_HDR_FLG)
#define SET_LAST_BLK(B) \
  (((Block_t *) (B))->misc |= LAST_BLK_HDR_FLG)
#define SET_NOT_LAST_BLK(B) \
  (((Block_t *) (B))->misc &= ~LAST_BLK_HDR_FLG)

#define SBH_THIS_FREE		THIS_FREE_BLK_HDR_FLG
#define SBH_THIS_ALLOCED	((Word_t) 0)
#define SBH_PREV_FREE		PREV_FREE_BLK_HDR_FLG
#define SBH_PREV_ALLOCED	((Word_t) 0)
#define SBH_LAST_BLK		LAST_BLK_HDR_FLG
#define SBH_NOT_LAST_BLK	((Word_t) 0)

#define SET_BLK_HDR(B, Sz, F) \
  (ASSERT(((Sz) & FLG_MASK) == 0), ((Block_t *) (B))->misc = ((Sz) | (F)))

#define BLK_UMEM_SZ(B) \
  (BLK_SZ(B) - (ABLK_HDR_SZ))
#define BLK_SZ(B) \
  (((Block_t *) (B))->misc & SZ_MASK)
#define IS_PREV_BLK_FREE(B) \
  (((Block_t*)(B))->misc & PREV_FREE_BLK_HDR_FLG)
#define IS_PREV_BLK_ALLOCED(B) \
  (!IS_PREV_BLK_FREE((B)))
#define IS_FREE_BLK(B) \
  (((Block_t*)(B))->misc & THIS_FREE_BLK_HDR_FLG)
#define IS_ALLOCED_BLK(B) \
  (!IS_FREE_BLK((B)))  
#define IS_LAST_BLK(B) \
  (((Block_t*)(B))->misc & LAST_BLK_HDR_FLG)
#define IS_NOT_LAST_BLK(B) \
  (!IS_LAST_BLK((B)))

#define IS_FIRST_BLK(B) \
  (IS_PREV_BLK_FREE((B)) && (PREV_BLK_SZ((B)) == 0))
#define IS_NOT_FIRST_BLK(B) \
  (!IS_FIRST_BLK((B)))

#define SET_SBC_BLK_FTR(FTR) \
  ((FTR) = (0 | SBC_BLK_FTR_FLG))
#define SET_MBC_BLK_FTR(FTR) \
  ((FTR) = 0)

#define IS_SBC_BLK(B) \
  (IS_PREV_BLK_FREE((B)) && (((Word_t *) (B))[-1] & SBC_BLK_FTR_FLG))
#define IS_MBC_BLK(B) \
  (!IS_SBC_BLK((B)))

#define NXT_BLK(B) \
  ((Block_t *) (((char *) (B)) + BLK_SZ((B))))
#define PREV_BLK(B) \
  ((Block_t *) (((char *) (B)) - PREV_BLK_SZ((B))))

/* Carriers ... */

#define MIN_MBC_FIRST_FREE_SZ		(4*1024)
#define MIN_MBC_SZ			(16*1024)

#define MMAP_CARRIER_HDR_FLAG		(((Word_t) 1) << 0)
#define SBC_CARRIER_HDR_FLAG		(((Word_t) 1) << 1)

#define SBC_HDR_SZ \
  (UNIT_CEILING(sizeof(SBCarrier_t) + FBLK_FTR_SZ + ABLK_HDR_SZ) - ABLK_HDR_SZ)
#define MBC_HDR_SZ \
  (UNIT_CEILING(sizeof(MBCarrier_t) + FBLK_FTR_SZ + ABLK_HDR_SZ) - ABLK_HDR_SZ)

#define BLK2SBC(B) \
  ((SBCarrier_t *) (((char *) (B)) - SBC_HDR_SZ))
#define FBLK2MBC(B) \
  ((MBCarrier_t *) (((char *) (B)) - MBC_HDR_SZ))

#define MBC2FBLK(P) \
  ((Block_t *) (((char *) (P)) + MBC_HDR_SZ))
#define SBC2BLK(P) \
  ((Block_t *) (((char *) (P)) + SBC_HDR_SZ))
#define SBC2UMEM(P) \
  ((void *) (((char *) (P)) + (SBC_HDR_SZ + ABLK_HDR_SZ)))

#define IS_MMAP_CARRIER(C) \
  (((Carrier_t *) (C))->misc & MMAP_CARRIER_HDR_FLAG)
#define IS_MALLOC_CARRIER(C) \
  (!IS_MMAP_CARRIER((C)))
#define IS_SB_CARRIER(C) \
  (((Carrier_t *) (C))->misc & SBC_CARRIER_HDR_FLAG)
#define IS_MB_CARRIER(C) \
  (!IS_SB_CARRIER((C)))
#define IS_MAIN_CARRIER(C) \
  (main_carrier && ((Carrier_t *) (C)) == main_carrier)
#define IS_AUX_CARRIER(C) \
  (!IS_MAIN_CARRIER((C)))

#define CARRIER_SZ(C) \
  (((Carrier_t *) (C))->misc & SZ_MASK)

#define SET_MMAP_CARRIER(C) \
  (((Carrier_t *) (C))->misc |= MMAP_CARRIER_HDR_FLAG)
#define SET_MALLOC_CARRIER(C) \
  (((Carrier_t *) (C))->misc &= ~MMAP_CARRIER_HDR_FLAG)
#define SET_SB_CARRIER(C) \
  (((Carrier_t *) (C))->misc |= SBC_CARRIER_HDR_FLAG)
#define SET_MB_CARRIER(C) \
  (((Carrier_t *) (C))->misc &= ~SBC_CARRIER_HDR_FLAG)
#define SET_MAIN_CARRIER(C) \
  (main_carrier = (Carrier_t *) (C))
#define SET_AUX_CARRIER(C) \
  (IS_MAIN_CARRIER((C)) ? SET_MAIN_CARRIER(NULL) : ((Carrier_t *) NULL))

#define SET_CARRIER_SZ(C, SZ) \
  (ASSERT(((SZ) & FLG_MASK) == 0), \
   (((Carrier_t *) (C))->misc = (((Carrier_t *) (C))->misc & FLG_MASK) | (SZ)))

#define IS_BLK_ON_LAST_AUX_MBC(B) \
  (((char *) (B)) < last_aux_mb_carrier_end \
   && last_aux_mb_carrier_start <= ((char *) (B)))

#define CA_TYPE_SINGLEBLOCK			0
#define CA_TYPE_MULTIBLOCK			1

#define CA_FLAG_FORCE_MALLOC			(1 << 0)
#define CA_FLAG_MAIN_CARRIER			(1 << 1)
#define CA_FLAG_KEEP_MALLOC_CARRIER	       	(1 << 2)

#if HARD_DEBUG
static void check_blk_carrier(Block_t *);
#endif


/* Statistics updating ... */

#ifdef DEBUG
#define DEBUG_CHECK_CARRIER_NO_SZ                               \
    ASSERT((no_of_mmap_sbcs && mmap_sbcs_total_size)		\
	   || (!no_of_mmap_sbcs && !mmap_sbcs_total_size));	\
    ASSERT((no_of_malloc_sbcs && malloc_sbcs_total_size)	\
	   || (!no_of_malloc_sbcs && !malloc_sbcs_total_size));	\
    ASSERT((no_of_mmap_mbcs && mmap_mbcs_total_size)		\
	   || (!no_of_mmap_mbcs && !mmap_mbcs_total_size));	\
    ASSERT((no_of_malloc_mbcs && malloc_mbcs_total_size)	\
	   || (!no_of_malloc_mbcs && !malloc_mbcs_total_size));	\
    
#else
#define DEBUG_CHECK_CARRIER_NO_SZ
#endif

#define STAT_SBC_ALLOC(BSZ)					\
    sbc_blocks_total_size += (BSZ);				\
    if (max_sbc_blocks_total_size < sbc_blocks_total_size)	\
	max_sbc_blocks_total_size = sbc_blocks_total_size;	\
    if (max_no_of_sbcs < no_of_mmap_sbcs + no_of_malloc_sbcs)	\
	max_no_of_sbcs = no_of_mmap_sbcs + no_of_malloc_sbcs;	\
    if (max_sbcs_total_size < (mmap_sbcs_total_size		\
			       + malloc_sbcs_total_size))	\
	max_sbcs_total_size = (mmap_sbcs_total_size		\
			       + malloc_sbcs_total_size)


#define STAT_MMAP_SBC_ALLOC(CSZ, BSZ)				\
do {								\
    no_of_mmap_sbcs++;						\
    mmap_sbcs_total_size += (CSZ);				\
    STAT_SBC_ALLOC((BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MALLOC_SBC_ALLOC(CSZ, BSZ)				\
do {								\
    no_of_malloc_sbcs++;					\
    malloc_sbcs_total_size += (CSZ);				\
    STAT_SBC_ALLOC((BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)


#define STAT_SBC_FREE(BSZ)					\
    ASSERT(sbc_blocks_total_size >= (BSZ));			\
    sbc_blocks_total_size -= (BSZ)

#define STAT_MMAP_SBC_FREE(CSZ, BSZ)				\
do {								\
    ASSERT(no_of_mmap_sbcs > 0);				\
    no_of_mmap_sbcs--;						\
    ASSERT(mmap_sbcs_total_size >= (CSZ));			\
    mmap_sbcs_total_size -= (CSZ);				\
    STAT_SBC_FREE((BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MALLOC_SBC_FREE(CSZ, BSZ)				\
do {								\
    ASSERT(no_of_malloc_sbcs > 0);				\
    no_of_malloc_sbcs--;					\
    ASSERT(malloc_sbcs_total_size >= (CSZ));			\
    malloc_sbcs_total_size -= (CSZ);				\
    STAT_SBC_FREE((BSZ));					\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MBC_ALLOC						\
    if (max_no_of_mbcs < no_of_mmap_mbcs + no_of_malloc_mbcs)	\
	max_no_of_mbcs = no_of_mmap_mbcs + no_of_malloc_mbcs;	\
    if (max_mbcs_total_size < (mmap_mbcs_total_size		\
			       + malloc_mbcs_total_size))	\
	max_mbcs_total_size = (mmap_mbcs_total_size		\
			       + malloc_mbcs_total_size)


#define STAT_MMAP_MBC_ALLOC(CSZ)				\
do {								\
    no_of_mmap_mbcs++;						\
    mmap_mbcs_total_size += (CSZ);				\
    STAT_MBC_ALLOC;						\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MALLOC_MBC_ALLOC(CSZ)				\
do {								\
    no_of_malloc_mbcs++;					\
    malloc_mbcs_total_size += (CSZ);				\
    STAT_MBC_ALLOC;						\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MMAP_MBC_FREE(CSZ)					\
do {								\
    ASSERT(no_of_mmap_mbcs > 0);				\
    no_of_mmap_mbcs--;						\
    ASSERT(mmap_mbcs_total_size >= (CSZ));			\
    mmap_mbcs_total_size -= (CSZ);				\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MALLOC_MBC_FREE(CSZ)				\
do {								\
    ASSERT(no_of_malloc_mbcs > 0);				\
    no_of_malloc_mbcs--;					\
    ASSERT(malloc_mbcs_total_size >= (CSZ));			\
    malloc_mbcs_total_size -= (CSZ);				\
    DEBUG_CHECK_CARRIER_NO_SZ;					\
} while (0)

#define STAT_MBC_BLK_ALLOC(BSZ)					\
do {								\
    no_of_mbc_blocks++;						\
    if (max_no_of_mbc_blocks < no_of_mbc_blocks)		\
	max_no_of_mbc_blocks = no_of_mbc_blocks;		\
    mbc_blocks_total_size += (BSZ);				\
    if (max_mbc_blocks_total_size < mbc_blocks_total_size)	\
	max_mbc_blocks_total_size = mbc_blocks_total_size;	\
} while (0)

#define STAT_MBC_BLK_FREE(BSZ)					\
do {								\
    ASSERT(no_of_mbc_blocks > 0);				\
    no_of_mbc_blocks--;						\
    ASSERT(mbc_blocks_total_size >= (BSZ));			\
    mbc_blocks_total_size -= (BSZ);				\
} while (0)

#define SBC_LOAD_CHANGE_INTERVAL 1000
#define SBC_REQ_VEC_SZ 10
static double mmap_sbc_load_threshold;
static double sbc_requests_acc;
static Uint requests;
static Uint sbc_requests;
static Uint sbc_request_vec_ix;
static Uint sbc_request_vec[SBC_REQ_VEC_SZ];
static int sbc_mmap_allowed;

#define INIT_SBC_LOAD_CHECK()						\
do {									\
    Uint start_val = (Uint) ((mmap_sbc_load_threshold - 1)		\
			     * SBC_LOAD_CHANGE_INTERVAL / 100 / 2);	\
    for (sbc_request_vec_ix = SBC_REQ_VEC_SZ - 1;			\
	 sbc_request_vec_ix;						\
	 sbc_request_vec_ix--)						\
	sbc_request_vec[sbc_request_vec_ix] = start_val;		\
    sbc_requests_acc = SBC_REQ_VEC_SZ * start_val;			\
    sbc_mmap_allowed = 1;						\
    requests = 0;							\
    sbc_requests = 0;							\
} while (0)

#define SBC_LOAD \
  (100*((sbc_requests_acc + sbc_requests) \
	/ (requests + (SBC_LOAD_CHANGE_INTERVAL * SBC_REQ_VEC_SZ))))
#define IS_SBC_MMAP_ALLOWED() \
  (sbc_mmap_allowed \
   ? ((SBC_LOAD > mmap_sbc_load_threshold) ? (sbc_mmap_allowed = 0) : 1) \
   : ((SBC_LOAD > mmap_sbc_load_threshold - 1) ? 0 : (sbc_mmap_allowed = 1)))

#define NOTICE_SBC_REQUEST() (sbc_requests++)

#define NOTICE_REQUEST()						\
do {									\
    requests++;								\
    if (requests >= SBC_LOAD_CHANGE_INTERVAL) {				\
	sbc_requests_acc -= sbc_request_vec[sbc_request_vec_ix];	\
	sbc_request_vec[sbc_request_vec_ix] = sbc_requests;		\
	sbc_requests_acc += sbc_requests;				\
	sbc_request_vec_ix++;						\
	if(sbc_request_vec_ix >= SBC_REQ_VEC_SZ)			\
	    sbc_request_vec_ix = 0;					\
	sbc_requests = 0;						\
	requests = 0;							\
    }									\
} while (0)

/* Debug stuff... */
#ifdef DEBUG
static Uint carrier_alignment;
#define DEBUG_SAVE_ALIGNMENT(C)						\
do {									\
    Uint algnmnt__ = sizeof(Unit_t) - (((Uint) (C)) % sizeof(Unit_t));	\
    carrier_alignment = MIN(carrier_alignment, algnmnt__);		\
    ASSERT(((Uint) (C)) % sizeof(Word_t) == 0);				\
} while (0)
#define DEBUG_CHECK_ALIGNMENT(P)					\
do {									\
    ASSERT(sizeof(Unit_t) - (((Uint) (P)) % sizeof(Unit_t))		\
	   >= carrier_alignment);					\
    ASSERT(((Uint) (P)) % sizeof(Word_t) == 0);				\
} while (0)

#else
#define DEBUG_SAVE_ALIGNMENT(C)
#define DEBUG_CHECK_ALIGNMENT(P)
#endif


/* Buckets ... */

#define BKT_INTRVL_A		(1*sizeof(Unit_t))
#define BKT_INTRVL_B		(16*sizeof(Unit_t))
#define BKT_INTRVL_C		(96*sizeof(Unit_t))
#define BKT_INTRVL_D		bkt_intrvl_d

#define BKT_MIN_SIZE_A		MIN_BLK_SZ
#define BKT_MIN_SIZE_B		(BKT_MAX_SIZE_A + 1)
#define BKT_MIN_SIZE_C		(BKT_MAX_SIZE_B + 1)
#define BKT_MIN_SIZE_D		(BKT_MAX_SIZE_C + 1)

#define BKT_MAX_SIZE_A		((NO_OF_BKTS/4)*BKT_INTRVL_A+BKT_MIN_SIZE_A-1)
#define BKT_MAX_SIZE_B		((NO_OF_BKTS/4)*BKT_INTRVL_B+BKT_MIN_SIZE_B-1)
#define BKT_MAX_SIZE_C		((NO_OF_BKTS/4)*BKT_INTRVL_C+BKT_MIN_SIZE_C-1)
#define BKT_MAX_SIZE_D		bkt_max_size_d


#define BKT_MAX_IX_A		((NO_OF_BKTS*1)/4 - 1)
#define BKT_MAX_IX_B		((NO_OF_BKTS*2)/4 - 1)
#define BKT_MAX_IX_C		((NO_OF_BKTS*3)/4 - 1)
#define BKT_MAX_IX_D		((NO_OF_BKTS*4)/4 - 1)

#define BKT_MIN_IX_A		(0)
#define BKT_MIN_IX_B		(BKT_MAX_IX_A + 1)
#define BKT_MIN_IX_C		(BKT_MAX_IX_B + 1)
#define BKT_MIN_IX_D		(BKT_MAX_IX_C + 1)


static Uint bkt_max_size_d;
static Uint bkt_intrvl_d;

static void
init_bucket_index(Uint sbct)
{
    bkt_intrvl_d = 0;
    if (sbct > BKT_MIN_SIZE_D-1)
	bkt_intrvl_d =
	    UNIT_CEILING((3*(sbct-BKT_MIN_SIZE_D-1)/(NO_OF_BKTS/4-1)+1)/2);
    if (bkt_intrvl_d < BKT_INTRVL_C)
	bkt_intrvl_d = BKT_INTRVL_C;
    bkt_max_size_d = (NO_OF_BKTS/4)*bkt_intrvl_d + BKT_MIN_SIZE_D - 1;
}

#define BKT_IX_(SZ)							\
  ((SZ) <= BKT_MAX_SIZE_A						\
   ? (((SZ) - BKT_MIN_SIZE_A)/BKT_INTRVL_A + BKT_MIN_IX_A)		\
   : ((SZ) <= BKT_MAX_SIZE_B						\
      ? (((SZ) - BKT_MIN_SIZE_B)/BKT_INTRVL_B + BKT_MIN_IX_B)		\
      : ((SZ) <= BKT_MAX_SIZE_C						\
	 ? (((SZ) - BKT_MIN_SIZE_C)/BKT_INTRVL_C + BKT_MIN_IX_C)	\
	 : ((SZ) <= BKT_MAX_SIZE_D					\
	    ? (((SZ) - BKT_MIN_SIZE_D)/BKT_INTRVL_D + BKT_MIN_IX_D)	\
	    : (NO_OF_BKTS - 1)))))

#define BKT_MIN_SZ_(IX)							\
  ((IX) <= BKT_MAX_IX_A							\
   ? (((IX) - BKT_MIN_IX_A)*BKT_INTRVL_A + BKT_MIN_SIZE_A)		\
   : ((IX) <= BKT_MAX_IX_B						\
      ? (((IX) - BKT_MIN_IX_B)*BKT_INTRVL_B + BKT_MIN_SIZE_B)		\
      : ((IX) <= BKT_MAX_IX_C						\
	 ? (((IX) - BKT_MIN_IX_C)*BKT_INTRVL_C + BKT_MIN_SIZE_C)	\
	 : (((IX) - BKT_MIN_IX_D)*BKT_INTRVL_D + BKT_MIN_SIZE_D))))

#ifdef DEBUG

static int
BKT_IX(Uint size)
{
    int ix;
    ASSERT(size >= MIN_BLK_SZ);

    ix = BKT_IX_(size);

    ASSERT(0 <= ix && ix <= BKT_MAX_IX_D);

    return ix;
}

static Uint
BKT_MIN_SZ(int ix)
{
    Uint size;
    ASSERT(0 <= ix && ix <= BKT_MAX_IX_D);

    size = BKT_MIN_SZ_(ix);

#if HARD_DEBUG
    ASSERT(ix == BKT_IX(size));
    ASSERT(size == MIN_BLK_SZ || ix - 1 == BKT_IX(size - 1));
#endif

    return size;
}

#else

#define BKT_IX BKT_IX_
#define BKT_MIN_SZ BKT_MIN_SZ_

#endif

static int
find_bucket(int min_index)
{
    int min, mid, max;
    int sub_mask_ix, sub_bkt_ix;
    int ix = -1;

#undef  GET_MIN_BIT
#define GET_MIN_BIT(MinBit, BitMask, Min, Max)		\
    min = (Min);					\
    max = (Max);					\
    while(max != min) {					\
	mid = ((max - min) >> 1) + min;			\
	if((BitMask)					\
	   & (~(~((Word_t) 0) << (mid + 1)))		\
	   & (~((Word_t) 0) << min))			\
	    max = mid;					\
	else						\
	    min = mid + 1;				\
    }							\
    (MinBit) = min


    ASSERT(bucket_masks.main < (((Word_t) 1) << (MAX_SUB_MASK_IX+1)));

    sub_mask_ix = IX2SMIX(min_index);

    if ((bucket_masks.main & (~((Word_t) 0) << sub_mask_ix)) == 0)
	return -1;

    /* There exists a non empty bucket; find it... */

    if (bucket_masks.main & (((Word_t) 1) << sub_mask_ix)) {
	sub_bkt_ix = IX2SBIX(min_index);
	if ((bucket_masks.sub[sub_mask_ix]
	     & (~((Word_t) 0) << sub_bkt_ix)) == 0) {
	    sub_mask_ix++;
	    sub_bkt_ix = 0;
	    if ((bucket_masks.main & (~((Word_t) 0)<< sub_mask_ix)) == 0)
		return -1;
	}
	else
	    goto find_sub_bkt_ix;
    }
    else {
	sub_mask_ix++;
	sub_bkt_ix = 0;
    }

    ASSERT(sub_mask_ix <= MAX_SUB_MASK_IX);
    /* Has to be a bit > sub_mask_ix */
    ASSERT(bucket_masks.main & (~((Word_t) 0) << (sub_mask_ix)));
    GET_MIN_BIT(sub_mask_ix, bucket_masks.main, sub_mask_ix, MAX_SUB_MASK_IX);

 find_sub_bkt_ix:
    ASSERT(sub_mask_ix <= MAX_SUB_MASK_IX);
    ASSERT(sub_bkt_ix <= MAX_SUB_BKT_IX);

    if ((bucket_masks.sub[sub_mask_ix] & (((Word_t) 1) << sub_bkt_ix)) == 0) {
	ASSERT(sub_mask_ix + 1 <= MAX_SUB_BKT_IX);
	/* Has to be a bit > sub_bkt_ix */
	ASSERT(bucket_masks.sub[sub_mask_ix] & (~((Word_t) 0) << sub_bkt_ix));

	GET_MIN_BIT(sub_bkt_ix,
		    bucket_masks.sub[sub_mask_ix],
		    sub_bkt_ix+1,
		    MAX_SUB_BKT_IX);

	ASSERT(sub_bkt_ix <= MAX_SUB_BKT_IX);
    }

    ix = MAKE_BKT_IX(sub_mask_ix, sub_bkt_ix);

    ASSERT(0 <= ix && ix < NO_OF_BKTS); 

    return ix;

#undef  GET_MIN_BIT

}

static FreeBlock_t *
search_bucket(int ix, Uint size)
{
    int i;
    Uint min_sz;
    Uint blk_sz;
    Uint cand_sz = 0;
    FreeBlock_t *blk;
    FreeBlock_t *cand = NULL;
    int blk_on_lambc;
    int cand_on_lambc = 0;

    ASSERT(0 <= ix && ix <= NO_OF_BKTS - 1);

    if (!buckets[ix])
	return NULL;

    min_sz = BKT_MIN_SZ(ix);
    if (min_sz < size)
	min_sz = size;

    for (blk = buckets[ix], i = 0;
	 blk && i < max_blk_search;
	 blk = blk->next, i++) {

	blk_sz = BLK_SZ(blk);
	blk_on_lambc = IS_BLK_ON_LAST_AUX_MBC(blk);

	if (blk_sz == min_sz && !blk_on_lambc)
	    return blk;

	if (blk_sz >= min_sz
	    && (!cand
		|| (!blk_on_lambc && (cand_on_lambc || blk_sz < cand_sz))
		|| (blk_on_lambc && cand_on_lambc && blk_sz < cand_sz))) {
	    cand_sz = blk_sz;
	    cand = blk;
	    cand_on_lambc = blk_on_lambc;
	}

    }
    return cand;
}

static FreeBlock_t *
find_free_block(Uint size)
{
    int unsafe_bi, min_bi;
    FreeBlock_t *blk;

    unsafe_bi = BKT_IX(size);
    
    min_bi = find_bucket(unsafe_bi);
    if (min_bi < 0)
	return NULL;

    if (min_bi == unsafe_bi) {
	blk = search_bucket(min_bi, size);
	if (blk)
	    return blk;
	if (min_bi < NO_OF_BKTS - 1) {
	    min_bi = find_bucket(min_bi + 1);
	    if (min_bi < 0)
		return NULL;
	}
	else
	    return NULL;
    }
    else {
	ASSERT(min_bi > unsafe_bi);
    }

    /* We are guaranteed to find a block that fits in this bucket */
    blk = search_bucket(min_bi, size);
    ASSERT(blk);
    return blk;
}

static void
link_free_block(FreeBlock_t *blk)
{
    Uint sz;
    int i;

    sz = BLK_SZ(blk);

    ASSERT(sz >= MIN_BLK_SZ);

    i = BKT_IX(sz);

    SET_BKT_MASK_IX(i);

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
    Uint sz;

    sz = BLK_SZ(blk);
    i = BKT_IX(sz);

    if (!blk->prev) {
	ASSERT(buckets[i] == blk);
	buckets[i] = blk->next;
    }
    else
	blk->prev->next = blk->next;
    if (blk->next)
	blk->next->prev = blk->prev;

    if (!buckets[i])
	UNSET_BKT_MASK_IX(i);
}

static Uint
next_mbc_size(void)
{
    Uint size;
    int cs = no_of_mmap_mbcs + no_of_malloc_mbcs - (main_carrier ? 1 : 0);

    ASSERT(cs >= 0);
    ASSERT(largest_mbc_size >= smallest_mbc_size);

    if (cs >= mbc_growth_stages)
	size = largest_mbc_size;
    else
	size = ((Uint) (cs*(((double) (largest_mbc_size-smallest_mbc_size))
			    /((double) mbc_growth_stages))
			+ smallest_mbc_size));

    if (size < MIN_MBC_SZ)
	size = MIN_MBC_SZ;

    return size - MBC_HDR_SZ;
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
    Uint is_last_blk;
    Uint blk_sz;
    Block_t *blk;
    Uint nxt_blk_sz;
    MBCarrier_t *carrier;
    Block_t *nxt_blk;

    ASSERT(size);
    ASSERT(size < sbc_threshold);

    blk_sz = UMEMSZ2BLKSZ(size);

    blk = (Block_t *) find_free_block(blk_sz);

    if (!blk) {
	Uint c_sz = next_mbc_size();
	if (c_sz - MIN_MBC_FIRST_FREE_SZ < blk_sz)
	    c_sz = blk_sz + MIN_MBC_FIRST_FREE_SZ;

	carrier = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK, c_sz, 0);
	if (!carrier) {
	    /*
	     * Emergency! We couldn't allocate the carrier as we wanted.
	     * If it's a relatively small request, we try one more time
	     * with a small mbc; otherwise, we place it in a sbc.
	     */
	    if (MIN_MBC_SZ - MIN_MBC_FIRST_FREE_SZ >= blk_sz)
		carrier = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK,
							MIN_MBC_SZ,
							0);

	    if (!carrier) {
		/* The only thing left to do is to try to place it in a sbc. */
		SBCarrier_t *sbc;
		sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
						    size,
						    CA_FLAG_FORCE_MALLOC);
		return sbc ? SBC2UMEM(sbc) : NULL;
	    }
	}
	
	blk = (Block_t *) MBC2FBLK(carrier);

	ASSERT(BLK_SZ(blk) >= blk_sz);
	ASSERT(IS_MBC_BLK(blk));
#if HARD_DEBUG
	link_free_block((FreeBlock_t *) blk);
	check_blk_carrier(blk);
	unlink_free_block((FreeBlock_t *) blk);
#endif

    }
    else {
	ASSERT(IS_FREE_BLK(blk));
	ASSERT(IS_MBC_BLK(blk));
#if HARD_DEBUG
	check_blk_carrier(blk);
#endif

	unlink_free_block((FreeBlock_t *) blk);
    }


    ASSERT(blk);

    SET_BLK_ALLOCED(blk);

#ifdef DEBUG
    nxt_blk = NULL;
#endif

    is_last_blk = IS_LAST_BLK(blk);

    if (BLK_SZ(blk) - MIN_BLK_SZ >= blk_sz) {
	/* Shrink block... */
	nxt_blk_sz = BLK_SZ(blk) - blk_sz;
	SET_BLK_SZ(blk, blk_sz);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_HDR(nxt_blk,
		    nxt_blk_sz,
		    SBH_THIS_FREE|SBH_PREV_ALLOCED|SBH_NOT_LAST_BLK);

	if (is_last_blk) {
	    SET_NOT_LAST_BLK(blk);
	    SET_LAST_BLK(nxt_blk);
	}
	else
	    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);

	link_free_block((FreeBlock_t *) nxt_blk);

	ASSERT(IS_NOT_LAST_BLK(blk));
	ASSERT(IS_FREE_BLK(nxt_blk));
	ASSERT(is_last_blk ? IS_LAST_BLK(nxt_blk) : IS_NOT_LAST_BLK(nxt_blk));
	ASSERT(is_last_blk || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	ASSERT(is_last_blk || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));
	ASSERT(nxt_blk_sz == BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= MIN_BLK_SZ);
    }
    else {
	if (!is_last_blk) {
	    nxt_blk = NXT_BLK(blk);
	    SET_PREV_BLK_ALLOCED(nxt_blk);
	}
	blk_sz = BLK_SZ(blk);

	ASSERT(is_last_blk ? IS_LAST_BLK(blk) : IS_NOT_LAST_BLK(blk));
    }

    STAT_MBC_BLK_ALLOC(blk_sz);

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(blk_sz == BLK_SZ(blk));
    ASSERT(blk_sz % sizeof(Unit_t) == 0);
    ASSERT(blk_sz >= MIN_BLK_SZ);
    ASSERT(blk_sz >= size + ABLK_HDR_SZ);
    ASSERT(IS_MBC_BLK(blk));

    ASSERT(!nxt_blk || IS_PREV_BLK_ALLOCED(nxt_blk));
    ASSERT(!nxt_blk || IS_MBC_BLK(nxt_blk));

#if HARD_DEBUG
    check_blk_carrier(blk);
#endif

    return BLK2UMEM(blk);
}

static void
mbc_free(void *p)
{
    int is_first_blk;
    int is_last_blk;
    Uint blk_sz;
    Block_t *blk;
    Block_t *nxt_blk;

    ASSERT(p);

    blk = UMEM2BLK(p);
    blk_sz = BLK_SZ(blk);

    ASSERT(IS_MBC_BLK(blk));
    ASSERT(blk_sz >= MIN_BLK_SZ);

#if HARD_DEBUG
    check_blk_carrier(blk);
#endif

    STAT_MBC_BLK_FREE(blk_sz);

    is_first_blk = IS_FIRST_BLK(blk);
    is_last_blk = IS_LAST_BLK(blk);

    if (!is_first_blk && IS_PREV_BLK_FREE(blk)) {
	/* Coalesce with previous block... */
	blk = PREV_BLK(blk);
	unlink_free_block((FreeBlock_t *) blk);

	blk_sz += BLK_SZ(blk);
	is_first_blk = IS_FIRST_BLK(blk);
	SET_BLK_SZ(blk, blk_sz);
    }
    else {
	SET_BLK_FREE(blk);
    }

    if (is_last_blk)
	SET_LAST_BLK(blk);
    else {
	nxt_blk = NXT_BLK(blk);
	if (IS_FREE_BLK(nxt_blk)) {
	    /* Coalesce with next block... */
	    unlink_free_block((FreeBlock_t *) nxt_blk);
	    blk_sz += BLK_SZ(nxt_blk);
	    SET_BLK_SZ(blk, blk_sz);

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (is_last_blk) 
		SET_LAST_BLK(blk);
	    else {
		SET_NOT_LAST_BLK(blk);
		SET_BLK_SZ_FTR(blk, blk_sz);
	    }
	}
	else {
	    SET_PREV_BLK_FREE(nxt_blk);
	    SET_NOT_LAST_BLK(blk);
	    SET_BLK_SZ_FTR(blk, blk_sz);
	}

    }

    ASSERT(is_last_blk  ? IS_LAST_BLK(blk)  : IS_NOT_LAST_BLK(blk));
    ASSERT(is_first_blk ? IS_FIRST_BLK(blk) : IS_NOT_FIRST_BLK(blk));
    ASSERT(IS_FREE_BLK(blk));
    ASSERT(is_first_blk || IS_PREV_BLK_ALLOCED(blk));
    ASSERT(is_last_blk  || IS_PREV_BLK_FREE(NXT_BLK(blk)));
    ASSERT(blk_sz == BLK_SZ(blk));
    ASSERT(is_last_blk || blk == PREV_BLK(NXT_BLK(blk)));
    ASSERT(blk_sz % sizeof(Unit_t) == 0);
    ASSERT(IS_MBC_BLK(blk));

    if (is_first_blk
	&& is_last_blk
	&& IS_AUX_CARRIER(FBLK2MBC(blk))) {
#if HARD_DEBUG
	link_free_block((FreeBlock_t *) blk);
	check_blk_carrier(blk);
	unlink_free_block((FreeBlock_t *) blk);
#endif
	carrier_free((Carrier_t *) FBLK2MBC(blk));
    }
    else {
	link_free_block((FreeBlock_t *) blk);
#if HARD_DEBUG
	check_blk_carrier(blk);
#endif
    }
}

static void *
mbc_realloc(void *p, size_t size)
{
    void *new_p;
    Uint old_blk_sz;
    Block_t *blk;
#ifndef MBC_REALLOC_ALWAYS_MOVES
    Uint blk_sz;
    Block_t *nxt_blk;
    Uint nxt_blk_sz;
    int is_last_blk;
#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

    ASSERT(p);
    ASSERT(size);
    ASSERT(size < sbc_threshold);

    blk = (Block_t *) UMEM2BLK(p);
    old_blk_sz = BLK_SZ(blk);

    ASSERT(old_blk_sz >= MIN_BLK_SZ);

#ifndef MBC_REALLOC_ALWAYS_MOVES

    blk_sz = UMEMSZ2BLKSZ(size);

    ASSERT(IS_ALLOCED_BLK(blk));
    ASSERT(IS_MBC_BLK(blk));

    if (old_blk_sz == blk_sz)
	return p;

    is_last_blk = IS_LAST_BLK(blk);

    if ((is_last_blk || IS_ALLOCED_BLK(NXT_BLK(blk)))
	&& (old_blk_sz - MIN_BLK_SZ < blk_sz && blk_sz < old_blk_sz))
	return p;

    if (blk_sz < old_blk_sz) {
	/* Shrink block... */
	Block_t *nxt_nxt_blk;

#if HARD_DEBUG
	check_blk_carrier(blk);
#endif

	nxt_blk_sz = old_blk_sz - blk_sz;
	SET_BLK_SZ(blk, blk_sz);
	SET_NOT_LAST_BLK(blk);

	nxt_blk = NXT_BLK(blk);
	SET_BLK_HDR(nxt_blk,
		    nxt_blk_sz,
		    SBH_THIS_FREE|SBH_PREV_ALLOCED|SBH_NOT_LAST_BLK);

	STAT_MBC_BLK_FREE(old_blk_sz);
	STAT_MBC_BLK_ALLOC(blk_sz);

	ASSERT(BLK_SZ(blk) >= MIN_BLK_SZ);

	if (is_last_blk)
	    SET_LAST_BLK(nxt_blk);
	else {
	    nxt_nxt_blk = NXT_BLK(nxt_blk);
	    if (IS_FREE_BLK(nxt_nxt_blk)) {
		/* Coalesce with next free block... */
		nxt_blk_sz += BLK_SZ(nxt_nxt_blk);
		unlink_free_block((FreeBlock_t *) nxt_nxt_blk);
		SET_BLK_SZ(nxt_blk, nxt_blk_sz);

		is_last_blk = IS_LAST_BLK(nxt_nxt_blk);
		if (is_last_blk)
		    SET_LAST_BLK(nxt_blk);
		else
		    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
	    }
	    else {
		SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);
		SET_PREV_BLK_FREE(nxt_nxt_blk);
	    }
	}

	link_free_block((FreeBlock_t *) nxt_blk);


	ASSERT(IS_ALLOCED_BLK(blk));
	ASSERT(blk_sz == BLK_SZ(blk));
	ASSERT(blk_sz % sizeof(Unit_t) == 0);
	ASSERT(blk_sz >= MIN_BLK_SZ);
	ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	ASSERT(IS_MBC_BLK(blk));
    
	ASSERT(IS_FREE_BLK(nxt_blk));
	ASSERT(IS_PREV_BLK_ALLOCED(nxt_blk));
	ASSERT(nxt_blk_sz == BLK_SZ(nxt_blk));
	ASSERT(nxt_blk_sz % sizeof(Unit_t) == 0);
	ASSERT(nxt_blk_sz >= MIN_BLK_SZ);
	ASSERT(IS_MBC_BLK(nxt_blk));
	ASSERT(is_last_blk ? IS_LAST_BLK(nxt_blk) : IS_NOT_LAST_BLK(nxt_blk));
	ASSERT(is_last_blk || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	ASSERT(is_last_blk || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));

#if HARD_DEBUG
	check_blk_carrier(blk);
#endif

	return p;
    }

    /* Need larger block... */

    if (!is_last_blk) {
	nxt_blk = NXT_BLK(blk);
	if (IS_FREE_BLK(nxt_blk) && blk_sz <= old_blk_sz + BLK_SZ(nxt_blk)) {
	    /* Grow into next block... */

#if HARD_DEBUG
	    check_blk_carrier(blk);
#endif

	    unlink_free_block((FreeBlock_t *) nxt_blk);
	    nxt_blk_sz = BLK_SZ(nxt_blk) - (blk_sz - old_blk_sz);

	    is_last_blk = IS_LAST_BLK(nxt_blk);
	    if (nxt_blk_sz < MIN_BLK_SZ) {
		blk_sz += nxt_blk_sz;

		SET_BLK_SZ(blk, blk_sz);

		if (is_last_blk) {
		    SET_LAST_BLK(blk);
#ifdef DEBUG
		    nxt_blk = NULL;
#endif
		}
		else {
		    nxt_blk = NXT_BLK(blk);
		    SET_PREV_BLK_ALLOCED(nxt_blk);
#ifdef DEBUG
		    nxt_blk_sz = BLK_SZ(nxt_blk);
#endif
		}
	    }
	    else {
		SET_BLK_SZ(blk, blk_sz);

		nxt_blk = NXT_BLK(blk);
		SET_BLK_HDR(nxt_blk,
			    nxt_blk_sz,
			    SBH_THIS_FREE|SBH_PREV_ALLOCED|SBH_NOT_LAST_BLK);

		if (is_last_blk)
		    SET_LAST_BLK(nxt_blk);
		else
		    SET_BLK_SZ_FTR(nxt_blk, nxt_blk_sz);

		link_free_block((FreeBlock_t *) nxt_blk);

		ASSERT(IS_FREE_BLK(nxt_blk));
	    }

	    STAT_MBC_BLK_FREE(old_blk_sz);
	    STAT_MBC_BLK_ALLOC(blk_sz);


	    ASSERT(IS_ALLOCED_BLK(blk));
	    ASSERT(blk_sz == BLK_SZ(blk));
	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= MIN_BLK_SZ);
	    ASSERT(blk_sz >= size + ABLK_HDR_SZ);
	    ASSERT(IS_MBC_BLK(blk));

	    ASSERT(!nxt_blk || IS_PREV_BLK_ALLOCED(nxt_blk));
	    ASSERT(!nxt_blk || nxt_blk_sz == BLK_SZ(nxt_blk));
	    ASSERT(!nxt_blk || nxt_blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(!nxt_blk || nxt_blk_sz >= MIN_BLK_SZ);
	    ASSERT(!nxt_blk || IS_MBC_BLK(nxt_blk));
	    ASSERT(!nxt_blk || (is_last_blk
				? IS_LAST_BLK(nxt_blk)
				: IS_NOT_LAST_BLK(nxt_blk)));
	    ASSERT(!nxt_blk || is_last_blk
		   || IS_ALLOCED_BLK(nxt_blk)
		   || nxt_blk == PREV_BLK(NXT_BLK(nxt_blk)));
	    ASSERT(!nxt_blk || is_last_blk
		   || IS_ALLOCED_BLK(nxt_blk)
		   || IS_PREV_BLK_FREE(NXT_BLK(nxt_blk)));
#if HARD_DEBUG
	    check_blk_carrier(blk);
#endif

	    return p;
	}
    }

    /* Failed to grow; move into a new one... */

#endif /* #ifndef MBC_REALLOC_ALWAYS_MOVES */

    new_p = mbc_alloc(size);
    if (!new_p)
	return NULL;
    MEMCPY(new_p, p, MIN(size, old_blk_sz - ABLK_HDR_SZ));
    mbc_free(p);
    return new_p;
}


static Carrier_t *
carrier_alloc(Uint type, Uint data_size, Uint flags)
{
    Carrier_t *carrier;
#if USE_MMAP
    Uint mmap_carrier_size;
    int mmapped = 0;
#endif
    Uint block_size;
    Uint carrier_size;

    block_size = UMEMSZ2BLKSZ(data_size);

    if (type == CA_TYPE_SINGLEBLOCK) {
	carrier_size = block_size + SBC_HDR_SZ;
    }
    else {
	ASSERT(type == CA_TYPE_MULTIBLOCK);
	carrier_size = block_size + MBC_HDR_SZ;
#if HARD_DEBUG
	if (sizeof(Unit_t) == sizeof(Word_t))
	    carrier_size += sizeof(Word_t);
#endif
    }
    carrier_size = UNIT_CEILING(carrier_size);

#if USE_MMAP
    if (no_of_mmap_mbcs + no_of_mmap_sbcs >= max_mmap_carriers
	|| flags & CA_FLAG_FORCE_MALLOC)
	goto malloc_carrier;
    mmap_carrier_size = PAGE_CEILING(carrier_size);

    carrier = (Carrier_t *) MMAP(mmap_carrier_size);
    if (carrier != (Carrier_t *) MAP_FAILED) {
	carrier_size = mmap_carrier_size;
	SET_MMAP_CARRIER(carrier);
	mmapped++;
    }
    else {
    malloc_carrier:
#endif
	carrier = (Carrier_t *) MALLOC(carrier_size);
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

    SET_CARRIER_SZ(carrier, carrier_size);

    if (type == CA_TYPE_SINGLEBLOCK) {
	SBCarrier_t *sb_carrier = (SBCarrier_t *) carrier;
	Block_t *blk = SBC2BLK(sb_carrier);

	SET_SB_CARRIER(sb_carrier);

	SET_SBC_BLK_FTR(((Word_t *)blk)[-1]);
	SET_BLK_HDR(blk,
		    block_size,
		    SBH_THIS_ALLOCED|SBH_PREV_FREE|SBH_LAST_BLK);


#if USE_MMAP
	if (mmapped) {
	    STAT_MMAP_SBC_ALLOC(carrier_size, block_size);
	    ASSERT(carrier_size % page_size == 0);
	}
	else
#endif
	    STAT_MALLOC_SBC_ALLOC(carrier_size, block_size);


	ASSERT(IS_SBC_BLK(blk));
	ASSERT(IS_FIRST_BLK(blk));
	ASSERT(IS_LAST_BLK(blk));
	ASSERT(block_size == BLK_SZ(blk));
	ASSERT(block_size % sizeof(Unit_t) == 0);

#if HARD_DEBUG
	check_blk_carrier(blk);
#endif
    }
    else {
	FreeBlock_t *blk;
	MBCarrier_t *mb_carrier = (MBCarrier_t *) carrier;

#if HARD_DEBUG
	if (sizeof(Unit_t) == sizeof(Word_t))
	    carrier_size -= sizeof(Word_t);
#endif

	block_size = UNIT_FLOOR(carrier_size - MBC_HDR_SZ);

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

	if (last_aux_mb_carrier_start != (char *) last_mb_carrier
	    && last_mb_carrier
	    && IS_AUX_CARRIER(last_mb_carrier)) {
	    last_aux_mb_carrier_start = (char *) last_mb_carrier;
	    last_aux_mb_carrier_end = (((char *) last_mb_carrier)
				       + CARRIER_SZ(last_mb_carrier));
	}
	else {
	    last_aux_mb_carrier_start = NULL;
	    last_aux_mb_carrier_end = NULL;
	}

	blk = (FreeBlock_t *) MBC2FBLK(mb_carrier);

	SET_MBC_BLK_FTR(((Word_t *)blk)[-1]);
	SET_BLK_HDR(blk,
		    block_size,
		    SBH_THIS_FREE|SBH_PREV_FREE|SBH_LAST_BLK);

	ASSERT(IS_MBC_BLK(blk));
	ASSERT(IS_FIRST_BLK(blk));
	ASSERT(IS_LAST_BLK(blk));
	ASSERT(block_size == BLK_SZ(blk));
	ASSERT(block_size % sizeof(Unit_t) == 0);

#if USE_MMAP
	if (mmapped)
	    STAT_MMAP_MBC_ALLOC(carrier_size);
	else
#endif
	    STAT_MALLOC_MBC_ALLOC(carrier_size);

#if HARD_DEBUG
	*((MBCarrier_t **) NXT_BLK(blk)) = mb_carrier;
#endif

    }

    DEBUG_SAVE_ALIGNMENT(carrier);

    return carrier;
}

static SBCarrier_t *
sb_carrier_resize(SBCarrier_t *carrier, Uint new_data_size, Uint flags)
{
#if USE_MMAP
    Uint new_mmap_carrier_size;
#endif
    Block_t *blk;
    SBCarrier_t *new_carrier;
    Uint old_carrier_size;
    Uint old_block_size;
    Uint new_block_size;
    Uint new_carrier_size;
#ifdef DEBUG
    int is_mmapped;
#endif

    blk = SBC2BLK(carrier);

#if HARD_DEBUG
    check_blk_carrier(blk);
#endif

    ASSERT(IS_SB_CARRIER(carrier));
    ASSERT(IS_SBC_BLK(blk));

    new_block_size = UMEMSZ2BLKSZ(new_data_size);
    new_carrier_size = UNIT_CEILING(new_block_size + SBC_HDR_SZ);

    old_carrier_size = CARRIER_SZ(carrier);
    old_block_size = BLK_SZ(blk);

#if USE_MMAP

    if (IS_MMAP_CARRIER(carrier)) {
#ifdef DEBUG
	    is_mmapped = 1;
#endif
	STAT_MMAP_SBC_FREE(old_carrier_size, old_block_size);
	
	if (flags & CA_FLAG_FORCE_MALLOC)
	    goto force_malloc;

	new_mmap_carrier_size = PAGE_CEILING(new_carrier_size);
	if (old_carrier_size >= new_mmap_carrier_size
	    && ((((double) (old_carrier_size - new_mmap_carrier_size))
		 /((double) old_carrier_size))
		< ((double) sbc_shrink_threshold))) {
	    SET_BLK_SZ(SBC2BLK(carrier), new_block_size);
	    STAT_MMAP_SBC_ALLOC(old_carrier_size, new_block_size);
	    return carrier;
	}

#if USE_MREMAP
	new_carrier = (SBCarrier_t*) MREMAP(carrier, new_mmap_carrier_size);
	if (new_carrier != (SBCarrier_t *) MAP_FAILED) {
	    new_carrier_size = new_mmap_carrier_size;
	    STAT_MMAP_SBC_ALLOC(new_carrier_size, new_block_size);
	    goto final_touch;
	} /* else goto force_malloc; */
#else

	if (new_mmap_carrier_size < old_carrier_size) {
	    /* Shrink carrier ... */
	    MUNMAP((void *) (((char *) carrier) + new_mmap_carrier_size),
		   old_carrier_size - new_mmap_carrier_size);
	    STAT_MMAP_SBC_ALLOC(new_mmap_carrier_size, new_block_size);
	    new_carrier_size = new_mmap_carrier_size;
	    new_carrier = carrier;
	    goto final_touch;
	}

	new_carrier = (SBCarrier_t *) MMAP(new_mmap_carrier_size);
	if (new_carrier != (SBCarrier_t *) MAP_FAILED) {
	    new_carrier_size = new_mmap_carrier_size;
	    STAT_MMAP_SBC_ALLOC(new_carrier_size, new_block_size);
	}
#endif
	else {
	force_malloc:
#ifdef DEBUG
	    is_mmapped = 0;
#endif
	    new_carrier = (SBCarrier_t *) MALLOC(new_carrier_size);
	    if (!new_carrier)
		return NULL;
	    SET_MALLOC_CARRIER(carrier); /* Will be copied into new_carrier */
	    STAT_MALLOC_SBC_ALLOC(new_carrier_size, new_block_size);
	}
	MEMCPY((void *) new_carrier,
	       (void *) carrier,
	       SBC_HDR_SZ + MIN(new_block_size, old_block_size));
	MUNMAP(carrier, old_carrier_size);

    }
    else {
	if (!(flags & CA_FLAG_KEEP_MALLOC_CARRIER)
	    && !(flags & CA_FLAG_FORCE_MALLOC)
	    && no_of_mmap_mbcs + no_of_mmap_sbcs < max_mmap_carriers) {
	    new_mmap_carrier_size = PAGE_CEILING(new_carrier_size);
	    new_carrier = (SBCarrier_t *) MMAP(new_mmap_carrier_size);
	    if (new_carrier == (SBCarrier_t *) MAP_FAILED)
		goto try_realloc;

	    new_carrier_size = new_mmap_carrier_size;
	    MEMCPY((void *) new_carrier,
		   (void *) carrier,
		   SBC_HDR_SZ + MIN(new_block_size, old_block_size));
	    SET_MMAP_CARRIER(new_carrier);
	    FREE((void *) carrier);

	    STAT_MALLOC_SBC_FREE(old_carrier_size, old_block_size);
	    STAT_MMAP_SBC_ALLOC(new_carrier_size, new_block_size);
#ifdef DEBUG
	    is_mmapped = 1;
#endif
	}
	else {
	try_realloc:
#endif
#ifdef DEBUG
	    is_mmapped = 0;
#endif
	    new_carrier = (SBCarrier_t *) REALLOC((void *) carrier,
						  new_carrier_size);
	    if (!new_carrier)
		return NULL;

	    STAT_MALLOC_SBC_FREE(old_carrier_size, old_block_size);
	    STAT_MALLOC_SBC_ALLOC(new_carrier_size, new_block_size);
#if USE_MMAP
	}
    }

 final_touch:
#endif

    blk = SBC2BLK(new_carrier);
    SET_BLK_SZ(blk, new_block_size);
    SET_CARRIER_SZ(new_carrier, new_carrier_size);

    ASSERT(IS_SB_CARRIER(new_carrier));
    ASSERT(IS_SBC_BLK(SBC2BLK(new_carrier)));
    ASSERT(new_carrier_size == CARRIER_SZ(new_carrier));
#if USE_MMAP
    ASSERT((is_mmapped && IS_MMAP_CARRIER(new_carrier))
	   || (!is_mmapped && IS_MALLOC_CARRIER(new_carrier)));
    ASSERT(!is_mmapped || new_carrier_size % page_size == 0);
#endif

#if HARD_DEBUG
    check_blk_carrier(blk);
#endif

    DEBUG_SAVE_ALIGNMENT(new_carrier);

    return new_carrier;

}


static void
carrier_free(Carrier_t *carrier)
{
#if USE_MMAP
    Uint mmapped = 0;
#endif
    Uint carrier_size = CARRIER_SZ(carrier);

    if (IS_SB_CARRIER(carrier)) {
	SBCarrier_t *sbc = (SBCarrier_t *) carrier;
	Block_t *blk = SBC2BLK(sbc);
	Uint block_size = BLK_SZ(blk);
#if HARD_DEBUG
	check_blk_carrier(blk);
#endif
#if USE_MMAP
	if (IS_MMAP_CARRIER(carrier)) {
	    mmapped++;
	    STAT_MMAP_SBC_FREE(carrier_size, block_size);
	}
	else
#endif
	    STAT_MALLOC_SBC_FREE(carrier_size, block_size);
    }
    else {
	MBCarrier_t *mbc = (MBCarrier_t *) carrier;

	ASSERT(IS_FIRST_BLK(MBC2FBLK(mbc)) && IS_LAST_BLK(MBC2FBLK(mbc)));

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
	
	if (last_aux_mb_carrier_start != (char *) last_mb_carrier
	    && last_mb_carrier
	    && IS_AUX_CARRIER(last_mb_carrier)) {
	    last_aux_mb_carrier_start = (char *) last_mb_carrier;
	    last_aux_mb_carrier_end = (((char *) last_mb_carrier)
				       + CARRIER_SZ(last_mb_carrier));
	}
	else {
	    last_aux_mb_carrier_start = NULL;
	    last_aux_mb_carrier_end = NULL;
	}

#if USE_MMAP
	if (IS_MMAP_CARRIER(mbc)) {
	    mmapped++;
	    STAT_MMAP_MBC_FREE(carrier_size);
	}
	else
#endif
	    STAT_MALLOC_MBC_FREE(carrier_size);
    }

#if USE_MMAP
    if (mmapped)
	MUNMAP(carrier, carrier_size);
    else
#endif
	FREE(carrier);
}

#ifndef NO_SL_ALLOC_STAT_ETERM

/*
 * sl_alloc_stat_eterm() help functions
 */

static void
init_atoms(void)
{
    LOCK;

    if (!atoms_need_init) {
	UNLOCK;
	return;
    }


    INIT_AM(singleblock_carrier_threshold);
    INIT_AM(singleblock_carrier_shrink_threshold);
    INIT_AM(singleblock_carrier_move_threshold);
    INIT_AM(mmap_singleblock_carrier_load_threshold);
    INIT_AM(max_mmap_carriers);
    INIT_AM(singleblock_carriers);
    INIT_AM(multiblock_carriers);
    INIT_AM(main_carrier_size);
    INIT_AM(smallest_multiblock_carrier_size);
    INIT_AM(largest_multiblock_carrier_size);
    INIT_AM(multiblock_carrier_growth_stages);
    INIT_AM(max_block_search_depth);
    INIT_AM(malloc);
    INIT_AM(mmap);
    INIT_AM(carriers);
    INIT_AM(blocks);
    INIT_AM(carriers_size);
    INIT_AM(blocks_size);
    INIT_AM(adm_size);
    INIT_AM(max_carriers);
    INIT_AM(max_blocks);
    INIT_AM(max_carriers_size);
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
    INIT_AM(release);
    INIT_AM(settings);
    
    atoms_need_init = 0;

    UNLOCK;

}

#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple
#define bld_string	erts_bld_string

static void
add_2tup(Uint **hpp, Uint *szp, Eterm *addlp, Eterm el1, Eterm el2)
{
    *addlp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *addlp);
}

static void
add_3tup(Uint **hpp, Uint *szp, Eterm *addlp, Eterm el1, Eterm el2, Eterm el3)
{
    *addlp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 3, el1, el2, el3), *addlp);
}

static void
add_max_val_term(Uint **hpp, Uint *szp, Eterm *addlp, Eterm name,
		 ErtsSlAllocMaxVal *mvp)
{
    add_3tup(hpp,
	     szp,
	     addlp,
	     name,
	     bld_uint(hpp, szp, mvp->last),
	     bld_uint(hpp, szp, mvp->ever));
}

static void
add_carrier_stat_base_term(Uint **hpp, Uint *szp, Eterm *addlp, Eterm type,
			   ErtsSlAllocCarriersStatBase *csbp)
{
    if (csbp->carriers) {
	Eterm list = NIL;
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_carriers_size,
		 bld_uint(hpp, szp, csbp->carriers_size));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_carriers,
		 bld_uint(hpp, szp, csbp->carriers));
	add_2tup(hpp, szp, addlp, type, list);
    }
}

static void
add_carrier_stat_term(Uint **hpp, Uint *szp, Eterm *addlp, Eterm type,
		      ErtsSlAllocCarriersStat *csp, int add_max_vals)
{
    Eterm list = NIL;

    add_carrier_stat_base_term(hpp, szp, &list, AM_malloc, &csp->malloc);
    add_carrier_stat_base_term(hpp, szp, &list, AM_mmap, &csp->mmap);

    if (add_max_vals) {
	add_max_val_term(hpp,
			 szp,
			 &list,
			 AM_max_carriers_size,
			 &csp->max_carriers_size);
	add_max_val_term(hpp,
			 szp,
			 &list,
			 AM_max_carriers,
			 &csp->max_carriers);
	add_max_val_term(hpp,
			 szp,
			 &list,
			 AM_max_blocks_size,
			 &csp->max_blocks_size);
	add_max_val_term(hpp,
			 szp,
			 &list,
			 AM_max_blocks,
			 &csp->max_blocks);
    }

    add_2tup(hpp,
	     szp,
	     &list,
	     AM_carriers_size,
	     bld_uint(hpp,
		      szp,
		      csp->malloc.carriers_size + csp->mmap.carriers_size));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_carriers,
	     bld_uint(hpp,
		      szp,
		      csp->malloc.carriers + csp->mmap.carriers));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_adm_size,
	     (csp->adm_size == 0 && csp->blocks_size > 0
	      ? AM_unknown
	      : bld_uint(hpp, szp, csp->adm_size)));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_blocks_size,
	     bld_uint(hpp, szp, csp->blocks_size));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_blocks,
	     bld_uint(hpp, szp, csp->blocks));

    add_2tup(hpp, szp, addlp, type, list);
}

static void
add_calls_term(Uint **hpp, Uint *szp, Eterm *addlp, Eterm called_func,
	       ErtsSlAllocCallCounter *cc)
{
    add_3tup(hpp,
	     szp,
	     addlp,
	     called_func,
	     bld_uint(hpp, szp, cc->giga_calls),
	     bld_uint(hpp, szp, cc->calls));
}

static Eterm
stat_term(Uint **hpp, Uint *szp, ErtsSlAllocStat *sp)
{
    Eterm string;
    Eterm list;
    Eterm res_list = NIL;

    ASSERT(hpp || szp);

    if (szp)
	*szp = 0;

    /* Calls ... */
    if (!sp->old_sl_alloc_enabled) {
	list = NIL;
	add_calls_term(hpp, szp, &list, AM_realloc, &sp->realloc_calls);
	add_calls_term(hpp, szp, &list, AM_free, &sp->free_calls);
	add_calls_term(hpp, szp, &list, AM_malloc, &sp->malloc_calls);
#if USE_MMAP
#if USE_MREMAP
	add_calls_term(hpp, szp, &list, AM_mremap, &sp->mremap_calls);
#endif
	add_calls_term(hpp, szp, &list, AM_munmap, &sp->munmap_calls);
	add_calls_term(hpp, szp, &list, AM_mmap, &sp->mmap_calls);
#endif
	add_calls_term(hpp, szp, &list, AM_sl_alloc, &sp->sl_realloc_calls);
	add_calls_term(hpp, szp, &list, AM_sl_free, &sp->sl_free_calls);
	add_calls_term(hpp, szp, &list, AM_sl_alloc, &sp->sl_alloc_calls);

	add_2tup(hpp, szp, &res_list, AM_calls, list);
    }

    /* Carriers state ... */
    add_carrier_stat_term(hpp,
			  szp,
			  &res_list,
			  AM_singleblock_carriers,
			  &sp->singleblock,
			  !sp->old_sl_alloc_enabled);
    if (!sp->old_sl_alloc_enabled)
	add_carrier_stat_term(hpp,
			      szp,
			      &res_list,
			      AM_multiblock_carriers,
			      &sp->multiblock,
			      1);

    /* Settings ... */

    list = NIL;
    if (!sp->old_sl_alloc_enabled) {
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_main_carrier_size,
		 bld_uint(hpp, szp, sp->main_carrier_size));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_smallest_multiblock_carrier_size,
		 bld_uint(hpp,
			  szp,
			  sp->smallest_multiblock_carrier_size));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_largest_multiblock_carrier_size,
		 bld_uint(hpp,
			  szp,
			  sp->largest_multiblock_carrier_size));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_multiblock_carrier_growth_stages,
		 bld_uint(hpp,
			  szp,
			  sp->multiblock_carrier_growth_stages));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_max_block_search_depth,
		 bld_uint(hpp, szp, sp->max_block_search_depth));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_mmap_singleblock_carrier_load_threshold,
		 bld_uint(hpp,
			  szp,
			  sp->mmap_singleblock_carrier_load_threshold));
	add_2tup(hpp,
		 szp,
		 &list,
		 AM_singleblock_carrier_shrink_threshold,
		 bld_uint(hpp,
			  szp,
			  sp->singleblock_carrier_shrink_threshold));
    }
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_max_mmap_carriers,
	     bld_uint(hpp, szp, sp->max_mmap_carriers));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_singleblock_carrier_move_threshold,
	     bld_uint(hpp,
		      szp,
		      sp->singleblock_carrier_move_threshold));
    add_2tup(hpp,
	     szp,
	     &list,
	     AM_singleblock_carrier_threshold,
	     bld_uint(hpp,
		      szp,
		      sp->singleblock_carrier_threshold));

    if (!sp->old_sl_alloc_enabled)
	string = bld_string(hpp, szp, ERTS_SL_ALLOC_RELEASE);
    else
	string = bld_string(hpp, szp, ERTS_OLD_SL_ALLOC_RELEASE);

    add_2tup(hpp, szp, &list, AM_release, string);

    add_2tup(hpp, szp, &res_list, AM_settings, list);

    if (!sp->old_sl_alloc_enabled)
	string = bld_string(hpp, szp, ERTS_SL_ALLOC_VERSION);
    else
	string = bld_string(hpp, szp, ERTS_OLD_SL_ALLOC_VERSION);

    add_2tup(hpp, szp, &res_list, am_version, string);

    return res_list;
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
	SBCarrier_t *sbc;
	sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
					    size,
					    (IS_SBC_MMAP_ALLOWED()
					     ? 0
					     : CA_FLAG_FORCE_MALLOC));
	res = sbc ? SBC2UMEM(sbc) : NULL;
	NOTICE_SBC_REQUEST();
    }
    else
	res = mbc_alloc(size);

    NOTICE_REQUEST();
    UNLOCK;

    DEBUG_CHECK_ALIGNMENT(res);

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
    
    blk = UMEM2BLK(p);

    if (size < sbc_threshold) {
	if (IS_MBC_BLK(blk))
	    res = mbc_realloc(p, size);
	else {
#if USE_MMAP
	    if (IS_MALLOC_CARRIER(BLK2SBC(blk))
		|| (100 * (((double) (page_size
				     - (SBC_HDR_SZ + ABLK_HDR_SZ)
				     - size)) / ((double) page_size))
		    < sbc_move_threshold))
		/* Data won't be copied into a new carrier... */
		goto do_carrier_resize;

	    res = mbc_alloc(size);
	    if (res) {
		MEMCPY((void*) res,
		       (void*) p,
		       MIN(BLK_SZ(blk) - ABLK_HDR_SZ, size));
		carrier_free((Carrier_t *) BLK2SBC(blk));
	    }
#else
	    goto do_carrier_resize;
#endif
	}
    }
    else {
	SBCarrier_t *sbc;
	if(IS_SBC_BLK(blk)) {
	    NOTICE_SBC_REQUEST();
	do_carrier_resize:
	    sbc = sb_carrier_resize((SBCarrier_t *) BLK2SBC(blk),
				    size, CA_FLAG_KEEP_MALLOC_CARRIER);
	    res = sbc ? SBC2UMEM(sbc) : NULL;
	}
	else {
	    sbc = (SBCarrier_t *) carrier_alloc(CA_TYPE_SINGLEBLOCK,
						size,
						(IS_SBC_MMAP_ALLOWED()
						 ? 0
						 : CA_FLAG_FORCE_MALLOC));
	    if (sbc) {
		res = SBC2UMEM(sbc);
		MEMCPY((void*) res,
		       (void*) p,
		       MIN(BLK_SZ(blk) - ABLK_HDR_SZ, size));
		mbc_free(p);
	    }
	    else
		res = NULL;
	    NOTICE_SBC_REQUEST();
	}
    }

    NOTICE_REQUEST();
    UNLOCK;

    DEBUG_CHECK_ALIGNMENT(res);

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

	blk = UMEM2BLK(p);
	if (IS_SBC_BLK(blk))
	    carrier_free((Carrier_t *) BLK2SBC(blk));
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

    ASSERT(initialized);

    SL_ALLOC_STAT(&esas, 0);

    if(!esas.sl_alloc_enabled) {
	erl_printf(to, "sl_alloc: disabled\n");
	return;
    }

    if (esas.old_sl_alloc_enabled) {
	erl_printf(to,
		   "sl_alloc: ver(%s)\n",
		   ERTS_OLD_SL_ALLOC_VERSION);
	erl_printf(to,
		   "          sbc: cno(%u:%u), csz(%u:%u), \n",
		   esas.singleblock.malloc.carriers
		   + esas.singleblock.mmap.carriers,
		   esas.singleblock.mmap.carriers,
		   esas.singleblock.malloc.carriers_size
		   + esas.singleblock.mmap.carriers_size,
		   esas.singleblock.mmap.carriers_size);
	erl_printf(to,
		   "               bno(%u), bsz(%u), asz(unknown)\n",
		   esas.singleblock.blocks,
		   esas.singleblock.blocks_size);
	return;
    }
    
    erl_printf(to,
	       "sl_alloc: ver(%s)\n",
	       ERTS_SL_ALLOC_VERSION);
    erl_printf(to,
	       "          sbc: cno(%u:%u), csz(%u:%u), \n",
	       esas.singleblock.malloc.carriers
	       + esas.singleblock.mmap.carriers,
	       esas.singleblock.mmap.carriers,
	       esas.singleblock.malloc.carriers_size
	       + esas.singleblock.mmap.carriers_size,
	       esas.singleblock.mmap.carriers_size);
    erl_printf(to,
	       "               mcno(%u), mcsz(%u),\n",
	       esas.singleblock.max_carriers.ever,
	       esas.singleblock.max_carriers_size.ever);
    erl_printf(to,
	       "               bno(%u), bsz(%u), asz(%u),\n",
	       esas.singleblock.blocks,
	       esas.singleblock.blocks_size,
	       esas.singleblock.adm_size);
    erl_printf(to,
	       "               mbno(%u), mbsz(%u)\n",
	       esas.singleblock.max_blocks.ever,
	       esas.singleblock.max_blocks_size.ever);
    erl_printf(to,
	       "          mbc: cno(%u:%u), csz(%u:%u), \n",
	       esas.multiblock.malloc.carriers
	       + esas.multiblock.mmap.carriers,
	       esas.multiblock.mmap.carriers,
	       esas.multiblock.malloc.carriers_size
	       + esas.multiblock.mmap.carriers_size,
	       esas.multiblock.mmap.carriers_size);
    erl_printf(to,
	       "               mcno(%u), mcsz(%u), \n",
	       esas.multiblock.max_carriers.ever,
	       esas.multiblock.max_carriers_size.ever);
    erl_printf(to,
	       "               bno(%u), bsz(%u), asz(%u),\n",
	       esas.multiblock.blocks,
	       esas.multiblock.blocks_size,
	       esas.multiblock.adm_size);
    erl_printf(to,
	       "               mbno(%u), mbsz(%u)\n",
	       esas.multiblock.max_blocks.ever,
	       esas.multiblock.max_blocks_size.ever);
	       
}

/* -- erts_sl_alloc_stat_eterm() ------------------------------------------- */

#ifndef NO_SL_ALLOC_STAT_ETERM

Eterm
SL_ALLOC_STAT_ETERM(Process *p, int begin_max_period)
{
    Uint sz;
    Eterm *hp;
    ErtsSlAllocStat esas;
    Eterm res;
#ifdef DEBUG
    Eterm *hp_end;
#endif

    SL_ALLOC_STAT(&esas, begin_max_period);


    if (!esas.sl_alloc_enabled) {
	Eterm INIT_AM(disabled);
	return AM_disabled;
    }

    if (atoms_need_init)
	init_atoms();

    /* Calculate heap need for stat term ... */
    (void) stat_term(NULL, &sz, &esas);

    ASSERT(sz > 0);

    hp = HAlloc(p, sz);

#ifdef DEBUG
    hp_end = hp + sz;
#endif

    /* Write stat term ... */
    res = stat_term(&hp, NULL, &esas);

    ASSERT(hp == hp_end);

    return res;
}

#endif /* #ifndef NO_SL_ALLOC_STAT_ETERM */

/* -- erts_sl_alloc_stat() ------------------------------------------------- */

void
SL_ALLOC_STAT(ErtsSlAllocStat *p, int begin_max_period)
{
    ASSERT(initialized);

    if (use_old_sl_alloc) {
	ErtsOldSlAllocStat eosas;
	erts_old_sl_alloc_stat(&eosas);

	MEMZERO((void *) p, sizeof(ErtsSlAllocStat));

	p->sl_alloc_enabled = eosas.sl_alloc_enabled;
	p->old_sl_alloc_enabled = 1; 
	if (eosas.mmap_max >= 0) {
	    p->singleblock_carrier_threshold = (Uint) eosas.mmap_threshold;
	    p->singleblock_carrier_move_threshold = 80;
	    p->max_mmap_carriers = (Uint) eosas.mmap_max;

	    p->singleblock.blocks = (Uint) eosas.mmapped_chunks;
	    p->singleblock.blocks_size = eosas.mmapped_blocks_size;
	    p->singleblock.mmap.carriers = eosas.mmapped_chunks;
	    p->singleblock.mmap.carriers_size = eosas.mmapped_chunks_size;
	}

	return;
    }

    if (max_no_of_sbcs_ever < max_no_of_sbcs)
	max_no_of_sbcs_ever = max_no_of_sbcs;
    if (max_sbcs_total_size_ever < max_sbcs_total_size)
	max_sbcs_total_size_ever = max_sbcs_total_size;
    if (max_sbc_blocks_total_size_ever < max_sbc_blocks_total_size)
	max_sbc_blocks_total_size_ever = max_sbc_blocks_total_size;


    if (max_no_of_mbcs_ever < max_no_of_mbcs)
	max_no_of_mbcs_ever = max_no_of_mbcs;
    if (max_mbcs_total_size_ever < max_mbcs_total_size)
	max_mbcs_total_size_ever = max_mbcs_total_size;
    if (max_no_of_mbc_blocks_ever < max_no_of_mbc_blocks)
	max_no_of_mbc_blocks_ever = max_no_of_mbc_blocks;
    if (max_mbc_blocks_total_size_ever < max_mbc_blocks_total_size)
	max_mbc_blocks_total_size_ever = max_mbc_blocks_total_size;

    p->sl_alloc_enabled = !sl_alloc_disabled;
    p->old_sl_alloc_enabled = 0;

    p->singleblock_carrier_threshold = sbc_threshold;
    p->singleblock_carrier_shrink_threshold = sbc_shrink_threshold;
    p->singleblock_carrier_move_threshold = sbc_move_threshold;
    p->mmap_singleblock_carrier_load_threshold = mmap_sbc_load_threshold;
    p->max_mmap_carriers = max_mmap_carriers;
    p->main_carrier_size = main_carrier_size;
    p->smallest_multiblock_carrier_size = smallest_mbc_size;
    p->largest_multiblock_carrier_size = largest_mbc_size;
    p->multiblock_carrier_growth_stages = mbc_growth_stages;
    p->max_block_search_depth = max_blk_search;

    p->singleblock.blocks = no_of_mmap_sbcs + no_of_malloc_sbcs;
    p->singleblock.blocks_size = sbc_blocks_total_size;
    p->singleblock.adm_size = ((no_of_mmap_sbcs + no_of_malloc_sbcs)
			       *(SBC_HDR_SZ + ABLK_HDR_SZ));
    p->singleblock.max_blocks.ever = max_no_of_sbcs_ever;
    p->singleblock.max_blocks.last = max_no_of_sbcs;
    p->singleblock.max_blocks_size.ever = max_sbc_blocks_total_size_ever;
    p->singleblock.max_blocks_size.last = max_sbc_blocks_total_size;
    p->singleblock.max_carriers.ever = max_no_of_sbcs_ever;
    p->singleblock.max_carriers.last = max_no_of_sbcs;
    p->singleblock.max_carriers_size.ever = max_sbcs_total_size_ever;
    p->singleblock.max_carriers_size.last = max_sbcs_total_size;
    p->singleblock.mmap.carriers = no_of_mmap_sbcs;
    p->singleblock.mmap.carriers_size = mmap_sbcs_total_size;
    p->singleblock.malloc.carriers = no_of_malloc_sbcs;
    p->singleblock.malloc.carriers_size = malloc_sbcs_total_size;

    p->multiblock.blocks = no_of_mbc_blocks;
    p->multiblock.blocks_size = mbc_blocks_total_size;
    p->multiblock.adm_size = (no_of_mbc_blocks*ABLK_HDR_SZ
			      + ((no_of_mmap_mbcs+no_of_malloc_mbcs)
				 *MBC_HDR_SZ));
    p->multiblock.max_blocks.ever = max_no_of_mbc_blocks_ever;
    p->multiblock.max_blocks.last = max_no_of_mbc_blocks;
    p->multiblock.max_blocks_size.ever = max_mbc_blocks_total_size_ever;
    p->multiblock.max_blocks_size.last = max_mbc_blocks_total_size;
    p->multiblock.max_carriers.ever = max_no_of_mbcs_ever;
    p->multiblock.max_carriers.last = max_no_of_mbcs;
    p->multiblock.max_carriers_size.ever = max_mbcs_total_size_ever;
    p->multiblock.max_carriers_size.last = max_mbcs_total_size;
    p->multiblock.mmap.carriers = no_of_mmap_mbcs;
    p->multiblock.mmap.carriers_size = mmap_mbcs_total_size;
    p->multiblock.malloc.carriers = no_of_malloc_mbcs;
    p->multiblock.malloc.carriers_size = malloc_mbcs_total_size;

    p->mmap_calls = mmap_calls;
    p->munmap_calls = munmap_calls;
    p->mremap_calls = mremap_calls;
    p->malloc_calls = malloc_calls;
    p->realloc_calls = realloc_calls;
    p->free_calls = free_calls;
    p->sl_alloc_calls = sl_alloc_calls;
    p->sl_realloc_calls = sl_realloc_calls;
    p->sl_free_calls = sl_free_calls;

    if (begin_max_period) {
	max_no_of_sbcs = no_of_mmap_sbcs + no_of_malloc_sbcs;
	max_sbcs_total_size = mmap_sbcs_total_size + malloc_sbcs_total_size;
	max_sbc_blocks_total_size = sbc_blocks_total_size;

	max_no_of_mbcs = no_of_mmap_mbcs + no_of_malloc_mbcs;
	max_mbcs_total_size = mmap_mbcs_total_size + malloc_mbcs_total_size;
	max_no_of_mbc_blocks = no_of_mbc_blocks;
	max_mbc_blocks_total_size = mbc_blocks_total_size;
    }
}

/* -- erts_sl_alloc_init() ------------------------------------------------- */

void
SL_ALLOC_INIT(ErtsSlAllocInit *arg)
{
    int i;

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
    if (arg->sbcst < 0)
	arg->sbcst = ERTS_SL_ALLOC_DEFAULT_SBC_SHRINK_THRESHOLD;
    if (arg->msbclt < 0)
	arg->msbclt = ERTS_SL_ALLOC_DEFAULT_MMAP_SBC_LOAD_THRESHOLD;
    if (arg->mmc < 0)
	arg->mmc = ERTS_SL_ALLOC_DEFAULT_MAX_MMAP_CARRIERS;
    if (arg->scs < 0)
	arg->scs = ERTS_SL_ALLOC_DEFAULT_SMALLEST_CARRIER_SIZE;
    if (arg->lcs < 0)
	arg->lcs = ERTS_SL_ALLOC_DEFAULT_LARGEST_CARRIER_SIZE;
    if (arg->mbcgs < 0)
	arg->mbcgs = ERTS_SL_ALLOC_DEFAULT_CARRIER_GROWTH_STAGES;
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

    main_carrier_size = arg->mcs ? arg->mcs : 0;
    max_blk_search = arg->mbsd = MAX(1, arg->mbsd);
    if (arg->mbcgs < 1)
	mbc_growth_stages = arg->mbcgs = 1;
    else
	mbc_growth_stages = arg->mbcgs;
    smallest_mbc_size = arg->scs;
    if (arg->lcs < smallest_mbc_size)
	largest_mbc_size = arg->lcs = smallest_mbc_size;
    else
	largest_mbc_size = arg->lcs;
#if USE_MMAP
    max_mmap_carriers = arg->mmc;
#else
    max_mmap_carriers = arg->mmc = 0;
#endif
    sbc_threshold = arg->sbct;
    sbc_shrink_threshold = arg->sbcst;
    sbc_move_threshold = arg->sbcmt;
    mmap_sbc_load_threshold = arg->msbclt;

    init_bucket_index(sbc_threshold);

    bucket_masks.main = 0;
    for (i = 0; i < NO_OF_SUB_MASKS; i++)
	bucket_masks.sub[i] = 0;

    for (i = 0; i < NO_OF_BKTS; i++)
 	buckets[i] = NULL;

    first_mb_carrier = NULL;
    last_mb_carrier = NULL;
    last_aux_mb_carrier_start = NULL;
    last_aux_mb_carrier_end = NULL;

    no_of_mmap_sbcs = 0;
    no_of_malloc_sbcs = 0;
    max_no_of_sbcs = 0;
    max_no_of_sbcs_ever = 0;
    mmap_sbcs_total_size = 0;
    malloc_sbcs_total_size = 0;
    max_sbcs_total_size = 0;
    max_sbcs_total_size_ever = 0;

    sbc_blocks_total_size = 0;
    max_sbc_blocks_total_size = 0;
    max_sbc_blocks_total_size_ever = 0;

    no_of_mmap_mbcs = 0;
    no_of_malloc_mbcs = 0;
    max_no_of_mbcs = 0;
    max_no_of_mbcs_ever = 0;
    mmap_mbcs_total_size = 0;
    malloc_mbcs_total_size = 0;
    max_mbcs_total_size = 0;
    max_mbcs_total_size_ever = 0;

    no_of_mbc_blocks = 0;
    max_no_of_mbc_blocks = 0;
    max_no_of_mbc_blocks_ever = 0;
    mbc_blocks_total_size = 0;
    max_mbc_blocks_total_size = 0;
    max_mbc_blocks_total_size_ever = 0;

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

#ifndef MAP_ANON
    mmap_fd = GET_MMAP_FD;
#endif
#endif /* #if USE_MMAP */

#ifdef DEBUG
    carrier_alignment = sizeof(Unit_t);
#endif

    main_carrier = NULL;
    if (main_carrier_size) {
	MBCarrier_t *mbc;
	mbc = (MBCarrier_t *) carrier_alloc(CA_TYPE_MULTIBLOCK,
					    main_carrier_size,
					    CA_FLAG_FORCE_MALLOC
					    | CA_FLAG_MAIN_CARRIER);
	if (!mbc)
	    erl_exit(-1,
		     "Failed to allocate sl_alloc main carrier (size=%d)\n",
		     main_carrier_size);

	link_free_block((FreeBlock_t *) MBC2FBLK(mbc));

	ASSERT(IS_MAIN_CARRIER(mbc));
#if HARD_DEBUG
	check_blk_carrier((Block_t *) MBC2FBLK(mbc));
#endif
    }

    INIT_SBC_LOAD_CHECK();

#ifdef DEBUG
    initialized = 1;
#endif    
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug functions                                                           *
\*                                                                           */

#if HARD_DEBUG

static void
check_blk_carrier(Block_t *iblk)
{
    if (IS_SBC_BLK(iblk)) {
	SBCarrier_t *sbc = (SBCarrier_t *) BLK2SBC(iblk);

	ASSERT(SBC2BLK(sbc) == iblk);
	ASSERT(IS_ALLOCED_BLK(iblk));
	ASSERT(IS_FIRST_BLK(iblk));
	ASSERT(IS_LAST_BLK(iblk));
	ASSERT(CARRIER_SZ(sbc) - SBC_HDR_SZ >= BLK_SZ(iblk));
#if USE_MMAP
	if (IS_MMAP_CARRIER(sbc)) {
	    ASSERT(CARRIER_SZ(sbc) % page_size == 0);
	}
#endif
    }
    else {
	MBCarrier_t *mbc = NULL;
	Block_t *prev_blk = NULL;
	Block_t *blk;
	FreeBlock_t *fblk;
	char *carrier_end;
	Uint tot_blk_sz;
	Uint blk_sz;
	int i;
	int bi;
	int found;

	blk = iblk;
	tot_blk_sz = 0;

	while (1) {

	    if (prev_blk) {
		ASSERT(NXT_BLK(prev_blk) == blk);
		if (IS_FREE_BLK(prev_blk)) {
		    ASSERT(IS_PREV_BLK_FREE(blk));
		    ASSERT(prev_blk == PREV_BLK(blk));
		}
		else {
		    ASSERT(IS_PREV_BLK_ALLOCED(blk));
		}
	    }

	    if (mbc) {
		if (blk == iblk)
		    break;
		ASSERT(((Block_t *) mbc) < blk && blk < iblk);
	    }
	    else
		ASSERT(blk >= iblk);


	    ASSERT(IS_MBC_BLK(blk));

	    blk_sz = BLK_SZ(blk);

	    ASSERT(blk_sz % sizeof(Unit_t) == 0);
	    ASSERT(blk_sz >= MIN_BLK_SZ);

	    tot_blk_sz += blk_sz;

	    if(IS_FREE_BLK(blk)) {
		if (IS_NOT_LAST_BLK(blk))
		    ASSERT(*((Word_t *) (((char *) blk)+blk_sz-sizeof(Word_t)))
			   == blk_sz);

		bi = BKT_IX(blk_sz);

		ASSERT(bucket_masks.main & (((Word_t) 1) << IX2SMIX(bi)));
		ASSERT(bucket_masks.sub[IX2SMIX(bi)]
		       & (((Word_t) 1) << IX2SBIX(bi)));
		
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

	    if (IS_LAST_BLK(blk)) {
		carrier_end = ((char *) NXT_BLK(blk)) + sizeof(Word_t);
		mbc = *((MBCarrier_t **) NXT_BLK(blk));
		prev_blk = NULL;
		blk = MBC2FBLK(mbc);
		ASSERT(IS_FIRST_BLK(blk));
	    }
	    else {
		prev_blk = blk;
		blk = NXT_BLK(blk);
	    }
	}

	ASSERT(IS_MB_CARRIER(mbc));
	ASSERT(((char *) mbc) + MBC_HDR_SZ + tot_blk_sz  + sizeof(Word_t)
	       == carrier_end);
	ASSERT(((char *) mbc) + CARRIER_SZ(mbc) == carrier_end);

	for(bi = 0; bi < NO_OF_BKTS; bi++) {
	    if ((bucket_masks.main & (((Word_t) 1) << IX2SMIX(bi)))
		&& (bucket_masks.sub[IX2SMIX(bi)]
		    & (((Word_t) 1) << IX2SBIX(bi)))) {
		ASSERT(buckets[bi] != NULL);
	    }
	    else {
		ASSERT(buckets[bi] == NULL);
	    }
	}

#if USE_MMAP
	if (IS_MMAP_CARRIER(mbc)) {
	    ASSERT(CARRIER_SZ(mbc) % page_size == 0);
	}
#endif
    }

}

#endif
