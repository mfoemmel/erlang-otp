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

/* Some functions we use and export */
#undef  MALLOC
#undef  REALLOC
#undef  FREE
#undef  SL_MALLOC
#undef  SL_REALLOC
#undef  SL_FREE

#ifdef INSTRUMENT

#define MALLOC     sys_alloc2
#define REALLOC    sys_realloc2
#define FREE       sys_free2
#define SL_MALLOC  sys_sl_alloc2
#define SL_REALLOC sys_sl_realloc2
#define SL_FREE    sys_sl_free2

#else

#define MALLOC     sys_alloc
#define REALLOC    sys_realloc
#define FREE       sys_free
#define SL_MALLOC  sys_sl_alloc
#define SL_REALLOC sys_sl_realloc
#define SL_FREE    sys_sl_free

#endif

#ifdef DEBUG
static int initialized = 0;
#else
#define initialized 1
#endif

#ifndef HAVE_MMAP
#define HAVE_MMAP 0
#endif
#ifndef HAVE_MREMAP
#define HAVE_MREMAP 0
#endif



#if defined(USE_THREADS) && HAVE_MMAP
/* Thread synchronization currently only needed when we have mmap */
#define THREAD_SAFE_SL_ALLOC
#endif

#ifdef THREAD_SAFE_SL_ALLOC
/* Thread synchronization */

#include "driver.h"

#define SL_MALLOC_MUTEX_NO 3

#ifdef DEBUG
#define LOCK   ASSERT(0 == erts_mutex_lock(sl_malloc_lock))
#define UNLOCK ASSERT(0 == erts_mutex_unlock(sl_malloc_lock))
#else
#define LOCK   ((void) erts_mutex_lock(sl_malloc_lock))
#define UNLOCK ((void) erts_mutex_unlock(sl_malloc_lock))
#endif

int erts_atfork_sys(void (*prepare)(void),
		    void (*parent)(void),
		    void (*child)(void));
erl_mutex_t erts_mutex_sys(int mno);
erl_mutex_t sl_malloc_lock;

#ifndef INIT_MUTEX_IN_CHILD_AT_FORK
#define INIT_MUTEX_IN_CHILD_AT_FORK 0
#endif

static void lock_sl_malloc_lock(void)   { LOCK;   }
static void unlock_sl_malloc_lock(void) { UNLOCK; }
#if INIT_MUTEX_IN_CHILD_AT_FORK
static void init_sl_malloc_lock(void)
{
#ifdef DEBUG
  ASSERT((sl_malloc_lock = erts_mutex_sys(SL_MALLOC_MUTEX_NO)));
#else
  sl_malloc_lock = erts_mutex_sys(SL_MALLOC_MUTEX_NO);
#endif
}

#define CHILD_ATFORK   init_sl_malloc_lock
#else /* #if INIT_MUTEX_IN_CHILD_AT_FORK */
#define CHILD_ATFORK   unlock_sl_malloc_lock
#endif /* #if INIT_MUTEX_IN_CHILD_AT_FORK */
#define PARENT_ATFORK  unlock_sl_malloc_lock
#define PREPARE_ATFORK lock_sl_malloc_lock

#else /* #ifdef THREAD_SAFE_SL_ALLOC */

#define LOCK   
#define UNLOCK 

#endif /* #ifdef THREAD_SAFE_SL_ALLOC */

/* Use sl_alloc? */
static int sl_alloc_disabled;

/* Ensure correct alignment */
typedef union { char c; short s; int i; long l; float f; double d; } align_t;

#ifdef DEBUG

/* Exiting? (to aviod recursive erl_exit) */
static int exiting;

/* -- Fence around allocated blocks when debug compiled ------- */

/* Fence used by sys_alloc */
#define SYS_ALLOC_BEFORE_PATTERN ((unsigned long) (0xABCDEF97))
#define SYS_ALLOC_AFTER_PATTERN0 ((unsigned char) 0xBA)
#define SYS_ALLOC_AFTER_PATTERN1 ((unsigned char) 0xDC)
#define SYS_ALLOC_AFTER_PATTERN2 ((unsigned char) 0xFE)
#define SYS_ALLOC_AFTER_PATTERN3 ((unsigned char) 0x77)

/* Fence used by sys_sl_alloc */
#define BEFORE_PATTERN           ((unsigned long) ~SYS_ALLOC_BEFORE_PATTERN)
#define AFTER_PATTERN0           ((unsigned char) ~SYS_ALLOC_AFTER_PATTERN0)
#define AFTER_PATTERN1           ((unsigned char) ~SYS_ALLOC_AFTER_PATTERN1)
#define AFTER_PATTERN2           ((unsigned char) ~SYS_ALLOC_AFTER_PATTERN2)
#define AFTER_PATTERN3           ((unsigned char) ~SYS_ALLOC_AFTER_PATTERN3)

/* Beginning of block. */
typedef struct {
  unsigned long pattern;
  unsigned long block_size;
  align_t block[1];
} mem_guard;

#define GUARD_SZ (sizeof(mem_guard) - sizeof(align_t) + 4)
#define FBLK2GUARD(P) ((mem_guard *) (((char *)(P)) - (sizeof(mem_guard)    \
						       - sizeof(align_t))))

#define SET_GRD_GET_FBLK(P, S)                                              \
  set_guard_and_get_fake_block((P), (S))
#define CHK_GRD_GET_RBLK(P, OP)                                             \
  check_guard_and_get_real_block((P), __FILE__, __LINE__, (OP))


void *
set_guard_and_get_fake_block(void *p, size_t s)
{
  mem_guard *mg = (mem_guard *) p;
  unsigned char *ep = ((char *) (mg->block)) + s;

  mg->block_size = s;
  mg->pattern = BEFORE_PATTERN;
  ep[0] = AFTER_PATTERN0;
  ep[1] = AFTER_PATTERN1;
  ep[2] = AFTER_PATTERN2;
  ep[3] = AFTER_PATTERN3;
  return (void *) mg->block;
}

void *
check_guard_and_get_real_block(void *p, char *file, int line, char *func)
{
  mem_guard *mg = FBLK2GUARD(p);
  unsigned char *ep = ((unsigned char *) (mg->block)) + mg->block_size;

  if(exiting)
    return (void *) mg;

  if(mg->pattern != BEFORE_PATTERN) {
    exiting = 1;
    UNLOCK;
    if(mg->pattern == SYS_ALLOC_BEFORE_PATTERN
       && ep[0] == SYS_ALLOC_AFTER_PATTERN0
       && ep[1] == SYS_ALLOC_AFTER_PATTERN1
       && ep[2] == SYS_ALLOC_AFTER_PATTERN2
       && ep[3] == SYS_ALLOC_AFTER_PATTERN3)
      erl_exit(1,
	       "%s:%d: %s on memory allocated by sys_alloc/sys_realloc "
	       "0x%p (size %d)\n",
	       file,
	       line,
	       func,
	       p,
	       mg->block_size);
      
    erl_exit(1,
	     "%s:%d: (%s) Fence before memory at 0x%p clobbered\n",
	     file,
	     line,
	     func,
	     p);
  }

  if(ep[0] != AFTER_PATTERN0
     || ep[1] != AFTER_PATTERN1
     || ep[2] != AFTER_PATTERN2
     || ep[3] != AFTER_PATTERN3) {
    exiting = 1;
    UNLOCK;
    erl_exit(1,
	     "%s:%d: (%s) Fence after memory at 0x%p (size %d) clobbered\n",
	     file,
	     line,
	     func,
	     p,
	     mg->block_size);
  }

  return (void *) mg;
}

#endif /* #ifdef DEBUG */

#if HAVE_MMAP
#include "hash.h"

#define MEMCPY sys_memcpy

#if defined(PURIFY) && !defined(PURIFY_WITH_REAL_MMAP)
#define FAKE_MMAP
#else
#undef  FAKE_MMAP
#endif

/* Header stored in all mmapped chunks */
typedef struct mmap_chunk_ {
  /* ------------------------------ Header start --------------------------- */
  union {
    HashBucket hash;             /* Hash bucket used when mmap_table is used */
    struct {
      struct mmap_chunk_ *next;  /* Double linked list of mmap_chunks        */
      struct mmap_chunk_ *prev;  /* used when regions are used               */
    } dlist;                    
  } u;
  unsigned long block_size;      /* Size of block (memory returned to user)  */
  unsigned long chunk_size;      /* Size of chunk (memory mapped by mmap())  */
  /* ------------------------------ Header end ----------------------------- */
  align_t block[1];              /* Beginning of block                       */
} mmap_chunk;

/* Mmap chunk header size */
#define MMAP_CHUNK_HEAD_SZ (sizeof(mmap_chunk) - sizeof(align_t))

/* Maximum number of blocks to mmap() */
static int mmap_max;
/* Minimum size of blocks to mmap() */
static int mmap_threshold;
#if !defined(MAP_ANON) && !defined(FAKE_MMAP)
/* File descriptor used by mmap() (opened on /dev/zero) */
static int mmap_fd;
#endif
/* Number of mmapped chunks */
static int mmapped_chunks;
/* Number of mmapped chunks */
static unsigned long mmapped_chunks_size;
/* Number of mmapped chunks */
static unsigned long mmapped_blocks_size;
/* Lists of all mmapped chunks when mmap/malloc regions are used to
   distinguish between mmapped and malloced memory */
static mmap_chunk *mmap_list;
/* Mmap region mmap_min_addr -> 0xffffffff (on 32 bit arch). mmap_min_addr
   is the lowest address returned by mmap() */
static unsigned long mmap_min_addr;
/* Malloc region 0 -> malloc_max_addr. malloc_max_addr is the highest
   address returned by mmap() */
static unsigned long malloc_max_addr;
/* Hash table used to distingush between mmapped and malloced memory. */ 
static Hash mmap_table;
/* Mmap/malloc regions used? */
static int use_regions;

#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 0
#endif

#undef PAGE_SIZE

#if HAVE_GETPAGESIZE
#define PAGE_SIZE (pagesize_)
/* Cached page size */
static size_t pagesize_;
#else

#if 1
#error "No getpagesize()"
/* Implement some other way to get the real pagesize if needed! */
#else
/* Guess 4 Kb page size */
#define PAGE_SIZE (4096)
#endif

#endif

/* Min start size of mmap table */
#define MIN_MMAP_TABLE_START 11

/* Move mmapped memory to malloced memory if the request is lower than
   20 % of mmap threshold */
#define MMAP2MALLOC_THRESHOLD (mmap_threshold*1/5)

/* If we need more memory than we have or if unused pages exceeds 80%
   of the chunk we move an mmapped block to another mmapped block */
#define MOVE_MMAP2MMAP(NEEDED, PREVIOUS)                                      \
  ((NEEDED) > (PREVIOUS)                                                    \
   ? 1                                                                      \
   : (((PREVIOUS) - (NEEDED)) > ((PREVIOUS)*4/5) ? 1 : 0))

#undef  MAX
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#undef  MIN
#define MIN(A, B) ((A) < (B) ? (A) : (B))

#undef  PAGE_MASK
#define PAGE_MASK (PAGE_SIZE - 1)

/* Mmap chunks are requested in pages. MMAP_CHUNK_SIZE() returns needed
   chunk size given block size */
#define MMAP_CHUNK_SIZE(BSZ) ((MMAP_CHUNK_HEAD_SZ+(BSZ)+PAGE_MASK)          \
			      & ~PAGE_MASK)

#define MMAP_BLOCK2CHUNK(B) ((mmap_chunk *) (((char *)(B))                  \
					     - MMAP_CHUNK_HEAD_SZ))

#define IS_MMAPPED_CHUNK(P)                                                 \
  (use_regions                                                              \
   ? mmap_min_addr <= ((unsigned long) (P))                                 \
   : (((void *) 0) != hash_get(&mmap_table,                                 \
			       (void *) ((mmap_chunk *) (P))->block)))

#define IS_MMAPPED_BLOCK(P)                                                 \
  (use_regions                                                              \
   ? mmap_min_addr <= ((unsigned long) (P))                                 \
   : (((void *) 0) != hash_get(&mmap_table, (void *)(P))))


/* While all malloced memory is at lower addresses than all (explicitly by
   this module) mmapped memory we use malloc_max_addr and mmap_min_addr in
   order to determine if memory is mmapped or not. If malloc allocates
   memory at an address higher than the lowest mmapped address (malloc may
   use mmap() itself), we shift to usage of the mmap_table instead. */
#define CHECK_REGIONS if(malloc_max_addr >= mmap_min_addr) setup_mmap_table()

#define UPDATE_MALLOC_REGION(P)                                             \
do {                                                                        \
  if(use_regions) {                                                         \
    if(((unsigned long) (P)) > malloc_max_addr)                             \
      malloc_max_addr = ((unsigned long) (P));                              \
    CHECK_REGIONS;                                                          \
  }                                                                         \
} while (0)

#define UPDATE_MMAP_REGION(P)                                               \
do {                                                                        \
  if(use_regions) {                                                         \
    if(((unsigned long) (P)) < mmap_min_addr)                               \
      mmap_min_addr = ((unsigned long) (P));                                \
    CHECK_REGIONS;                                                          \
  }                                                                         \
} while (0)

#ifndef MMAP_FAILED
#define MMAP_FAILED ((void *)-1)
#endif

#ifdef FAKE_MMAP
static void *
fake_mmap(size_t s)
{
  void *p = MALLOC(s);
  if(!p)
    return MMAP_FAILED;
  return p;
}

#define MMAP(SZ)     (mmapped_chunks++, fake_mmap((SZ)))
#define MUNMAP(P)    (mmapped_chunks--, FREE((P)))

#if HAVE_MREMAP

static void *
fake_mremap(void *p, size_t s)
{
  void *np = REALLOC(p, s);
  if(!np)
    return MMAP_FAILED;
  return np;
}

#define MREMAP(P, S) fake_mremap((P), (S))

#endif /* #if HAVE_MREMAP */

#else /* #ifdef FAKE_MMAP */

#ifdef MAP_ANON

#define MMAP(SZ) (mmapped_chunks++,                                         \
		  mmap((void *) 0,                                          \
		       (SZ),                                                \
		       PROT_READ|PROT_WRITE,                                \
		       MAP_ANON|MAP_PRIVATE,                                \
		       -1,                                                  \
		       0))

#else /* #ifdef MAP_ANON */

#define GET_MMAP_FD open("/dev/zero", O_RDWR)

#define DO_MMAP(SZ) (mmapped_chunks++,                                      \
		     mmap((void *) 0,                                       \
			  (SZ),                                             \
			  PROT_READ|PROT_WRITE,                             \
			  MAP_PRIVATE,                                      \
			  mmap_fd,                                          \
			  0))
#define MMAP(SZ)                                                            \
  (mmap_fd < 0                                                              \
   ? ((mmap_fd = GET_MMAP_FD) < 0 ? MMAP_FAILED : DO_MMAP(SZ))              \
   : DO_MMAP(SZ))

#endif /* #ifdef MAP_ANON */

#ifdef DEBUG
#define MUNMAP(P)                                                           \
  ASSERT(0 == (mmapped_chunks--,                                            \
	       munmap((void *) (P),                                         \
		      (size_t) ((mmap_chunk *) (P))->chunk_size)))
#else
#define MUNMAP(P)                                                           \
  ((void) (mmapped_chunks--,                                                \
	   munmap((void *) (P), (size_t) ((mmap_chunk *) (P))->chunk_size)))
#endif

#if HAVE_MREMAP
#ifndef MREMAP_MAYMOVE
#define MREMAP_MAYMOVE 1
#endif
#define MREMAP(P, S) mremap((void *) (P),                                   \
			    (size_t) ((mmap_chunk *) (P))->chunk_size,      \
			    (S),                                            \
			    MREMAP_MAYMOVE)


#endif /* #if HAVE_MREMAP */

#endif /* #ifdef FAKE_MMAP */

/* -- Mmap table ---------------------------------------------------------- */

/* Some primes */
#define PRIME0 ((HashValue) 268438039)
#define PRIME1 ((HashValue) 268440479)
#define PRIME2 ((HashValue) 268439161)
#define PRIME3 ((HashValue) 268437017)


static HashValue
mmap_table_hash(void *p)
{
  unsigned char *c = (unsigned char *) &p;

  if(sizeof(void *) == 4) {
    return ((((PRIME0 + c[0])
	      *PRIME1 + c[1])
	     *PRIME2 + c[2])
	    *PRIME3 + c[3]);
  }
  else if(sizeof(void *) == 8) {
    return ((((((((PRIME0 + c[0])
		  *PRIME1 + c[1])
		 *PRIME2 + c[2])
		*PRIME3 + c[3])
	       *PRIME0 + c[4])
	      *PRIME1 + c[5])
	     *PRIME2 + c[6])
	    *PRIME3 + c[7]);
  }
  else {
    int i;
    int prime;
    HashValue h = 0;
    for(i = 0; i < sizeof(void *); i++) {
      switch(i % 4) {
      case 0: prime = PRIME0; break;
      case 1: prime = PRIME1; break;
      case 2: prime = PRIME2; break;
      case 3: prime = PRIME3; break;
      }
      h = h*prime + c[i];
    }
    return h;
  }
}

static int
mmap_table_cmp(void *block, void *bucket) {
  return ((align_t *) block == ((mmap_chunk *) bucket)->block) ? 0 : 1;
}

static void*
mmap_table_alloc(void *block)
{
  return (void *) MMAP_BLOCK2CHUNK(block);
}

static void
mmap_table_free(void *block)
{
}


static void
setup_mmap_table(void)
{
  mmap_chunk *mc;
  HashFunctions f;
#ifdef DEBUG
  int stored_chunks = 0;
#endif
  
  f.hash = (H_FUN) mmap_table_hash;
  f.cmp  = (HCMP_FUN) mmap_table_cmp;
  f.alloc = (HALLOC_FUN) mmap_table_alloc;
  f.free = (HFREE_FUN) mmap_table_free;

  hash_init(&mmap_table,
	    "mmap_table",
	    MAX(mmapped_chunks, MIN_MMAP_TABLE_START),
	    f);

  /* Move all previously mmapped chunks from the list to the table */

  while(mmap_list) {
    mc = mmap_list;
    mmap_list = mmap_list->u.dlist.next;

#ifdef DEBUG
    stored_chunks++;
    ASSERT(mc == (mmap_chunk *) hash_put(&mmap_table, (void *) mc->block));
#else
    (void) hash_put(&mmap_table, (void *) mc->block);
#endif
  }

#ifdef DEBUG
  ASSERT(stored_chunks == mmapped_chunks);
#endif

  use_regions = 0;
}

#ifdef DEBUG

#define HASH_PUT(P)                                                         \
  ASSERT((P) == (mmap_chunk *) hash_put(&mmap_table,                        \
				      (void *)((mmap_chunk *) (P))->block))

#define HASH_ERASE(P)                                                       \
  ASSERT(((void *) ((mmap_chunk *) (P))->block)                             \
	 == hash_erase(&mmap_table, (void *) ((mmap_chunk *) (P))->block))

#else /* #ifdef DEBUG */

#define HASH_PUT(P)                                                         \
  ((void) hash_put(&mmap_table, (void *)((mmap_chunk *) (P))->block))

#define HASH_ERASE(P)                                                       \
  ((void) hash_erase(&mmap_table, (void *)((mmap_chunk *) (P))->block))

#endif  /* #ifdef DEBUG */

/* -- Mmap chunk linking -------------------------------------------------- */

#define INLINE_LINK_MMAP_CHUNK_OPERATIONS
#ifdef  INLINE_LINK_MMAP_CHUNK_OPERATIONS

#define LINK_MMAP_CHUNK(P)                                                  \
do {                                                                        \
  mmapped_chunks_size += ((mmap_chunk *) (P))->chunk_size;                  \
  mmapped_blocks_size += ((mmap_chunk *) (P))->block_size;                  \
  if(use_regions) {                                                         \
    if(mmap_list)                                                           \
      mmap_list->u.dlist.prev = ((mmap_chunk *) (P));                       \
    ((mmap_chunk *) (P))->u.dlist.prev = NULL;                              \
    ((mmap_chunk *) (P))->u.dlist.next = mmap_list;                         \
    mmap_list = ((mmap_chunk *) (P));                                       \
  }                                                                         \
  else                                                                      \
    HASH_PUT((P));                                                          \
} while (0)

#define UNLINK_MMAP_CHUNK(P)                                                \
do {                                                                        \
  ASSERT(mmapped_chunks_size >= ((mmap_chunk *) (P))->chunk_size);          \
  ASSERT(mmapped_blocks_size >= ((mmap_chunk *) (P))->block_size);          \
  mmapped_chunks_size -= ((mmap_chunk *) (P))->chunk_size;                  \
  mmapped_blocks_size -= ((mmap_chunk *) (P))->block_size;                  \
  if(use_regions) {                                                         \
    if(((mmap_chunk *) (P))->u.dlist.prev)                                  \
      ((mmap_chunk *) (P))->u.dlist.prev->u.dlist.next                      \
	= ((mmap_chunk *) (P))->u.dlist.next;                               \
    else                                                                    \
      mmap_list = ((mmap_chunk *) (P))->u.dlist.next;                       \
    if(((mmap_chunk *) (P))->u.dlist.next)                                  \
      ((mmap_chunk *) (P))->u.dlist.next->u.dlist.prev                      \
	= ((mmap_chunk *) (P))->u.dlist.prev;                               \
  }                                                                         \
  else                                                                      \
    HASH_ERASE((P));                                                        \
} while (0)

#else

#define LINK_MMAP_CHUNK(P) link_mmap_chunk((P))
#define UNLINK_MMAP_CHUNK(P) unlink_mmap_chunk((P))

void
link_mmap_chunk(void *p)
{
  mmapped_chunks_size += ((mmap_chunk *) p)->chunk_size;
  mmapped_blocks_size += ((mmap_chunk *) p)->block_size;
  if(use_regions) {
    if(mmap_list)
      mmap_list->u.dlist.prev = ((mmap_chunk *) p);
    ((mmap_chunk *) p)->u.dlist.prev = NULL;
    ((mmap_chunk *) p)->u.dlist.next = mmap_list;
    mmap_list = ((mmap_chunk *) p);
  }
  else
    HASH_PUT(p);
}

void
unlink_mmap_chunk(void *p)
{
  ASSERT(mmapped_chunks_size >= ((mmap_chunk *) p)->chunk_size);
  ASSERT(mmapped_blocks_size >= ((mmap_chunk *) p)->block_size);
  mmapped_chunks_size -= ((mmap_chunk *) p)->chunk_size;
  mmapped_blocks_size -= ((mmap_chunk *) p)->block_size;
  if(use_regions) {
    if(((mmap_chunk *) p)->u.dlist.prev)
      ((mmap_chunk *) p)->u.dlist.prev->u.dlist.next
	= ((mmap_chunk *) p)->u.dlist.next;
    else
      mmap_list = ((mmap_chunk *) p)->u.dlist.next;
    if(((mmap_chunk *) p)->u.dlist.next)
      ((mmap_chunk *) p)->u.dlist.next->u.dlist.prev
	= ((mmap_chunk *) p)->u.dlist.prev;
  }
  else
    HASH_ERASE(p);
}
#endif

#endif /* #if HAVE_MMAP */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Exported functions                                                        *
\*                                                                           */

/* -- sys_sl_alloc() ------------------------------------------------------ */
void *
SL_MALLOC(unsigned int size)
{
  size_t sz = (size_t) size;
  void *p;

  ASSERT(initialized);

  if(sl_alloc_disabled)
    return MALLOC(sz);

  LOCK;

#ifdef DEBUG
  if(!size) {
    UNLOCK;
    return NULL;
  }
  sz += GUARD_SZ;
#endif

#if HAVE_MMAP

  ASSERT(mmapped_chunks <= mmap_max);
  if(sz >= mmap_threshold && mmapped_chunks < mmap_max) {
    size_t chunk_sz = MMAP_CHUNK_SIZE(sz);

    p = MMAP(chunk_sz);
    if(p != MAP_FAILED) {
      ((mmap_chunk *) p)->block_size = sz; 
      ((mmap_chunk *) p)->chunk_size = chunk_sz;
      LINK_MMAP_CHUNK(p);
      UPDATE_MMAP_REGION(p);
      UNLOCK;
#ifdef DEBUG
      return SET_GRD_GET_FBLK((void *) ((mmap_chunk *) p)->block,
			      sz - GUARD_SZ);
#else
      return (void *) ((mmap_chunk *) p)->block;
#endif
    }
    /* Failed to mmap, try malloc */
  }

#endif

  /* For now small requests are just malloced */
  p = MALLOC(sz);

#if HAVE_MMAP
  UPDATE_MALLOC_REGION(p);
#endif

  UNLOCK;

#ifdef DEBUG
  return SET_GRD_GET_FBLK(p, sz - GUARD_SZ);
#else
  return p;
#endif
}

/* -- sys_sl_free() ------------------------------------------------------- */

void
SL_FREE(void *ptr)
{
  ASSERT(initialized);

  if(sl_alloc_disabled) {
    FREE(ptr);
    return;
  }

  if(!ptr)
    return;

  LOCK;

#ifdef DEBUG
  ptr = CHK_GRD_GET_RBLK(ptr, "sys_sl_free");
#endif

#if HAVE_MMAP
  ASSERT(mmapped_chunks <= mmap_max);
  if(IS_MMAPPED_BLOCK(ptr)) {
    ptr = MMAP_BLOCK2CHUNK(ptr);
    UNLINK_MMAP_CHUNK(ptr);
    MUNMAP(ptr);
    UNLOCK;
    return;
  }
#endif

  FREE(ptr);

  UNLOCK;
}

/* -- sys_sl_realloc() ---------------------------------------------------- */

void *
SL_REALLOC(void *ptr, unsigned int save_size, unsigned int size)
{
  size_t sz = (size_t) size;
  size_t save_sz = (size_t) save_size;
  void *p;

  ASSERT(initialized);

  if(sl_alloc_disabled)
    return REALLOC(ptr, sz);

  if(!size) {
    SL_FREE(ptr);
    return NULL;
  }

  LOCK;

#ifdef DEBUG
  if(ptr) {
    size_t bs = FBLK2GUARD(ptr)->block_size;
    ptr = CHK_GRD_GET_RBLK(ptr, "sys_sl_realloc");
    if(save_sz > bs && !exiting) {
      exiting = 1;
      UNLOCK;
      erl_exit(1,
	       "%s:%d: sl_realloc called on block 0x%p (size %d) with too "
	       "large save_size (%d)\n",
	       __FILE__, __LINE__, ptr, bs, save_sz);
    }
    save_sz += GUARD_SZ;
    sz += GUARD_SZ;
  }
#endif

#if HAVE_MMAP
  {
    void *np;
    ASSERT(mmapped_chunks <= mmap_max);

    if(ptr && IS_MMAPPED_BLOCK(ptr)) {
      size_t chunk_sz = MMAP_CHUNK_SIZE(sz);
      p = MMAP_BLOCK2CHUNK(ptr);

      if(chunk_sz == ((mmap_chunk *) p)->chunk_size) {
	ASSERT(mmapped_blocks_size >= ((mmap_chunk *) p)->block_size);
	mmapped_blocks_size -= ((mmap_chunk *) p)->block_size;
	mmapped_blocks_size += sz;
	((mmap_chunk *) p)->block_size = sz;
	UNLOCK;
#ifdef DEBUG
	return SET_GRD_GET_FBLK(ptr, sz - GUARD_SZ);
#else
	return ptr;
#endif
      }

#if HAVE_MREMAP
      if(sz > MMAP2MALLOC_THRESHOLD) {
	/* mmapped -> mmapped */

	UNLINK_MMAP_CHUNK(p);
      
	np = MREMAP(p, chunk_sz);
	if(np != MAP_FAILED) {
	  ((mmap_chunk *) np)->block_size = sz;
	  ((mmap_chunk *) np)->chunk_size = chunk_sz;	
	  LINK_MMAP_CHUNK(np);
	  UPDATE_MMAP_REGION(np);
	  UNLOCK;

#ifdef DEBUG
	  return SET_GRD_GET_FBLK((void *) ((mmap_chunk *) np)->block,
				  sz - GUARD_SZ);
#else
	  return (void *) ((mmap_chunk *) np)->block;
#endif
	}

	LINK_MMAP_CHUNK(p);
      }
#endif /* #if HAVE_MREMAP */

      if(chunk_sz < ((mmap_chunk *) p)->chunk_size
	 && sz > MMAP2MALLOC_THRESHOLD
	 && !MOVE_MMAP2MMAP(chunk_sz, ((mmap_chunk *) p)->chunk_size)) {
	/* unchanged mmapping if not to much overhead */
	ASSERT(mmapped_blocks_size >= ((mmap_chunk *) p)->block_size);
	mmapped_blocks_size -=  ((mmap_chunk *) p)->block_size;
	mmapped_blocks_size += sz;
	((mmap_chunk *) p)->block_size = sz;
	UNLOCK;
#ifdef DEBUG
	return SET_GRD_GET_FBLK(ptr, sz - GUARD_SZ);
#else
	return ptr;
#endif
      }

      if(sz >= mmap_threshold /* && mmapped_chunks <= mmap_max */) {
	/* mmapped -> mmapped */
	np = MMAP(chunk_sz);
	if(np != MAP_FAILED) {
	  ((mmap_chunk *) np)->block_size = sz; 
	  ((mmap_chunk *) np)->chunk_size = chunk_sz;
	  LINK_MMAP_CHUNK(np);
	  UPDATE_MMAP_REGION(np);
	  np = ((mmap_chunk *) np)->block;
	}
	else
	  goto do_malloc; /* mmap failed; try with malloc... */
      }
      else {
	/* mmapped -> malloced */
      do_malloc:
	np = MALLOC(sz);
	UPDATE_MALLOC_REGION(np);
	if(!np) {
	  UNLOCK;
	  return NULL;
	}
      }

      MEMCPY(np, ptr, MIN(save_sz, sz));
#ifdef DEBUG
      np = SET_GRD_GET_FBLK(np, sz - GUARD_SZ);
#endif

      UNLINK_MMAP_CHUNK(p);
      MUNMAP(p);
      UNLOCK;
      return np;
    }

    if(sz >= mmap_threshold && mmapped_chunks < mmap_max) {
      /* malloced -> mmapped */
      size_t chunk_sz = MMAP_CHUNK_SIZE(sz);

      np = MMAP(chunk_sz);
      if(np != MAP_FAILED) {

	((mmap_chunk *) np)->block_size = sz; 
	((mmap_chunk *) np)->chunk_size = chunk_sz;

	LINK_MMAP_CHUNK(np);
	UPDATE_MMAP_REGION(np);
	np = ((mmap_chunk *) np)->block;

	if(ptr) {
	  MEMCPY(np, ptr, MIN(save_sz, sz));
	  FREE(ptr);
	}
#ifdef DEBUG
	np = SET_GRD_GET_FBLK(np, sz - GUARD_SZ);
#endif
	UNLOCK;
	return np;
      } /* else: mmap failed; try with realloc... */

    }

  }
#endif /* #if HAVE_MMAP */

  /* malloced -> malloced */
  p = REALLOC(ptr, sz);

#if HAVE_MMAP
  UPDATE_MALLOC_REGION(p);
#endif

  UNLOCK;

#ifdef DEBUG
  return SET_GRD_GET_FBLK(p, sz - GUARD_SZ);
#else
  return p;
#endif
}

/* ------------------------------------------------------------------------ */

int
sys_sl_alloc_opt(int param_number, int value)
{
  int res;

  ASSERT(initialized);

  if(sl_alloc_disabled)
    return 0;

  LOCK;

  if(value < 0)
    res = 0;
  else
    switch(param_number) {
    case SYS_SL_ALLOC_OPT_MMAP_THRESHOLD:
#if HAVE_MMAP
      mmap_threshold = value;
#endif
      res = 1;
      break;
    case SYS_SL_ALLOC_OPT_MMAP_MAX:
#if HAVE_MMAP
      mmap_max = value;
      res = 1;
      break;
#else
      res = (value == 0) ? 1 : 0;
#endif
      break;
    case SYS_SL_ALLOC_OPT_USE_MMAP_TABLE:
#if HAVE_MMAP
      if(!use_regions && value == 0)
	res = 0;
      else if(use_regions)
	setup_mmap_table();
      res = 1;
      break;
#else
      res = 0;
      break;
#endif
    default:
      res = 0;
      break;
    }

  UNLOCK;

  return res;
}

/* ------------------------------------------------------------------------ */

/*
  sys_sl_alloc_init() has to be called before any other sl_alloc functions
  are called and before any threads other than the initial thread have been
  started.
*/

void
sys_sl_alloc_init(int enable_sl_alloc)
{
#ifdef DEBUG
  int atfork_res;
  exiting = 0;
#endif
  sl_alloc_disabled = !enable_sl_alloc;

  if(sl_alloc_disabled) {
#ifdef DEBUG
    initialized = 1;
#endif
    return;
  }

#if HAVE_MMAP
#if HAVE_GETPAGESIZE
  pagesize_ = getpagesize();
#endif
  /* Default maximum number of blocks to mmap() */
  mmap_max = ERTS_DEFAULT_MMAP_MAX;
  /* Default minimum size of blocks to mmap() */
  mmap_threshold  = ERTS_DEFAULT_MMAP_THRESHOLD;
#if !defined(MAP_ANON) && !defined(FAKE_MMAP)
  /* File descriptor used by mmap() */
  mmap_fd = GET_MMAP_FD;
#endif
  /* Number of mmapped chunks */
  mmapped_chunks = 0;
  /* Size of mmapped chunks */
  mmapped_chunks_size = 0;
  /* Size of mmapped blocks */
  mmapped_blocks_size = 0;
  /* Lists of all mmapped chunks when mmap/malloc regions are used */
  mmap_list = NULL;
  /* Mmap region mmap_min_addr -> 0xffffffff (on 32 bit arch). mmap_min_addr
     is the lowest address returned by mmap() */
  mmap_min_addr = ~((unsigned long) 0);
  /* Malloc region 0 -> malloc_max_addr. malloc_max_addr is the highest
     address returned by mmap() */
  malloc_max_addr = 0;
  /* Use Mmap/malloc regions */
  use_regions = 1;
#endif /* #if HAVE_MMAP */

#ifdef THREAD_SAFE_SL_ALLOC
  sl_malloc_lock = erts_mutex_sys(SL_MALLOC_MUTEX_NO);

  if(!sl_malloc_lock) {
    /* erl_exit(-1, ...) because we don't want to generate an
       erl_crash.dump (which will fail at this stage). */
    erl_exit(-1, "Failed to initialize sl_alloc\n");
  }

#ifdef DEBUG
  atfork_res = 
#endif
    erts_atfork_sys(PREPARE_ATFORK, PARENT_ATFORK, CHILD_ATFORK);
#ifdef DEBUG
  
#ifndef HAVE_PTHREAD_ATFORK
#define HAVE_PTHREAD_ATFORK 0
#endif

#if HAVE_PTHREAD_ATFORK
  ASSERT(atfork_res == 0);
#endif

#endif /* #ifdef DEBUG */

#endif /* #ifdef THREAD_SAFE_SL_ALLOC */

#ifdef DEBUG
  initialized = 1;
#endif

}


/* ------------------------------------------------------------------------ */

void
sys_sl_alloc_info(CIO to)
{
  ASSERT(initialized);

  if(sl_alloc_disabled)
    return;

  LOCK;
#if HAVE_MMAP
  if(!use_regions)
    hash_info(to, &mmap_table);
  erl_printf(to, "Mmap chunks %d\n", mmapped_chunks);
  erl_printf(to, "Mmap size %u/%u\n",mmapped_blocks_size,mmapped_chunks_size);
#endif

  UNLOCK;
}

/* ------------------------------------------------------------------------ */

void
sys_sl_alloc_stat(SysSlAllocStat *ssasp)
{
  ASSERT(initialized);

  if(sl_alloc_disabled) {
    ssasp->sl_alloc_enabled    = 0;
    ssasp->mmap_threshold      = -1;
    ssasp->mmap_max            = -1;
    ssasp->mmapped_chunks      = 0;
    ssasp->mmapped_chunks_size = 0;
    ssasp->mmapped_blocks_size = 0;
    ssasp->mmap_table.in_use   = 0;
    ssasp->mmap_table.size     = 0;
    ssasp->mmap_table.used     = 0;
    ssasp->mmap_table.objs     = 0;
    ssasp->mmap_table.depth    = 0;
    return;
  }

  ssasp->sl_alloc_enabled    = 1;

#if HAVE_MMAP
  ssasp->mmap_threshold      = mmap_threshold;
  ssasp->mmap_max            = mmap_max;
  ssasp->mmapped_chunks      = mmapped_chunks;
  ssasp->mmapped_chunks_size = mmapped_chunks_size;
  ssasp->mmapped_blocks_size = mmapped_blocks_size;

  if(use_regions) {
    ssasp->mmap_table.in_use = 0;
    ssasp->mmap_table.size   = 0;
    ssasp->mmap_table.used   = 0;
    ssasp->mmap_table.objs   = 0;
    ssasp->mmap_table.depth  = 0;
  }
  else {
      HashInfo hi;
      hash_get_info(&hi, &mmap_table);
      ssasp->mmap_table.in_use = 1;
      ssasp->mmap_table.size   = hi.size;
      ssasp->mmap_table.used   = hi.used;
      ssasp->mmap_table.objs   = hi.objs;
      ssasp->mmap_table.depth  = hi.depth;
  }

#else
  ssasp->mmap_threshold      = -1;
  ssasp->mmap_max            = -1;
  ssasp->mmapped_chunks      = 0;
  ssasp->mmapped_chunks_size = 0;
  ssasp->mmapped_blocks_size = 0;
  ssasp->mmap_table.in_use   = 0;
  ssasp->mmap_table.size     = 0;
  ssasp->mmap_table.used     = 0;
  ssasp->mmap_table.objs     = 0;
  ssasp->mmap_table.depth    = 0;
#endif

}

/* ------------------------------------------------------------------------ */


