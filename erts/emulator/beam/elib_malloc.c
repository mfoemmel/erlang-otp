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
** Description: Faster malloc().
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifdef ENABLE_ELIB_MALLOC

#include <stdio.h>
#include "sys.h"
#include "driver.h"
#include "elib_stat.h"

extern erl_mutex_t erl_mutex_sys _ANSI_ARGS_((int mno));

/* To avoid clobbering of names becaure of reclaim on VxWorks,
   we undefine all possible malloc, calloc etc. */
#undef malloc
#undef calloc
#undef free
#undef realloc

#define ELIB_INLINE         /* inline all possible functions */
/* #define ELIB_SORTED_BLOCKS */  /* Keep blocks sorted in memory order */

#ifndef ELIB_ALIGN
#define ELIB_ALIGN             sizeof(double)
#endif

#ifndef ELIB_HEAP_SIZE
#define ELIB_HEAP_SIZE         (64*1024)  /* Default 64K */
#endif

#ifndef ELIB_HEAP_INCREAMENT
#define ELIB_HEAP_INCREAMENT   (32*1024)  /* Default 32K */
#endif

typedef unsigned long EWord;       /* Assume 32-bit in this implementation */
typedef unsigned short EHalfWord;  /* Assume 16-bit in this implementation */
typedef unsigned char EByte;       /* Assume 8-bit byte */


#define elib_printf fprintf
#define elib_putc   fputc


#if defined(__STDC__) || defined(__WIN32__)
#define CONCAT(x,y) x##y
#else
#define CONCAT(x,y) x/**/y
#endif


#ifdef ELIB_DEBUG
#define ELIB_PREFIX(fun, args) CONCAT(elib__,fun) args
#else
#define ELIB_PREFIX(fun, args) CONCAT(elib_,fun) args
#endif

#if defined(__STDC__)
void *ELIB_PREFIX(malloc, (size_t));
void *ELIB_PREFIX(calloc, (size_t, size_t));
void ELIB_PREFIX(free, (EWord *));
void *ELIB_PREFIX(realloc, (EWord *, size_t));
void* ELIB_PREFIX(memresize, (EWord *, int));
void* ELIB_PREFIX(memalign, (int, int));
int ELIB_PREFIX(memsize, (EWord *));
/* Extern interfaces used by VxWorks */
size_t elib_sizeof(void *);
void elib_init(EWord *, EWord);
void elib_force_init(EWord *, EWord);
#endif

/* bytes to pages */
#define PAGES(x)      (((x)+page_size-1) / page_size)
#define PAGE_ALIGN(p) ((char*)((((EWord)(p))+page_size-1)&~(page_size-1)))

/* bytes to words */
#define WORDS(x)      (((x)+sizeof(EWord)-1) / sizeof(EWord))

/* Align an address */
#define ALIGN(p)     ((EWord*)((((EWord)(p)+ELIB_ALIGN-1)&~(ELIB_ALIGN-1))))

/* Calculate the size needed to keep alignment */

#define ALIGN_BSZ(nb)  ((nb+sizeof(EWord)+ELIB_ALIGN-1) & ~(ELIB_ALIGN-1))

#define ALIGN_WSZ(nb)  WORDS(ALIGN_BSZ(nb))

#define ALIGN_SIZE(nb) (ALIGN_WSZ(nb) - 1)


/* PARAMETERS */

#if defined(ELIB_HEAP_SBRK)

/* Get the system page size (NEED MORE DEFINES HERE) */
#ifdef _SC_PAGESIZE
#define PAGE_SIZE   sysconf(_SC_PAGESIZE)
#elif defined(_MSC_VER)
#  ifdef _M_ALPHA
#    define PAGE_SIZE      0x2000
#  else
#    define PAGE_SIZE      0x1000
#  endif
#else
#define PAGE_SIZE   getpagesize()
#endif

#define ELIB_EXPAND(need)  expand_sbrk(need)
static FUNCTION(int, expand_sbrk, (EWord));

#elif defined(ELIB_HEAP_FIXED)

#define PAGE_SIZE 1024
#define ELIB_EXPAND(need) -1
static EWord fix_heap[WORDS(ELIB_HEAP_SIZE)];

#elif defined(ELIB_HEAP_USER)

#define PAGE_SIZE 1024
#define ELIB_EXPAND(need) -1

#else

#error "ELIB HEAP TYPE NOT SET"

#endif

static EWord* eheap;        /* Align heap start */
static EWord* eheap_top;    /* Point to end of heap */
EWord page_size = 0;        /* Set by elib_init */

#ifndef ELIB_FAILURE
#define ELIB_FAILURE    abort()
#endif

#if defined(ELIB_DEBUG) || defined(DEBUG)
#define ELIB_ALIGN_CHECK(p) do { \
       if ((EWord)(p) & (ELIB_ALIGN-1)) { \
	elib_printf(stderr, "RUNTIME ERROR: bad alignment\n"); \
        ELIB_FAILURE; \
     } \
  } while(0)
#else
#define ELIB_ALIGN_CHECK(p)
#endif


/* number of slots with a fixed size objects */
#define FIXED_BASE  5
#define FIXED   (1 << FIXED_BASE)         /* 2^5 */
#define DYNAMIC (32-FIXED_BASE)           /* 32 - 5 */

/*
** Free block layout
**   1 1          30
**  +--------------------------+
**  |F|P|        Size          |
**  +--------------------------+
**
** Where F is the free bit
**       P is the free above bit
**       Size is messured in words and does not include the hdr word
**
** If block is on the free list the size is also stored last in the
** block (in v[sz-3] for FreeBlock)
** 
*/
typedef struct _free_block {
    EWord hdr;
    struct _free_block* next;
    struct _free_block** prev;
    EWord v[1];
} FreeBlock;

typedef struct _allocated_block {
    EWord hdr;
    EWord v[3];
} AllocatedBlock;


#define FREE_BIT       0x80000000
#define FREE_ABOVE_BIT 0x40000000
#define SIZE_MASK      0x3fffffff     /* 2^30 words = 2^32 bytes */

/* Work on both FreeBlock and AllocatedBlock */
#define SIZEOF(p)         ((p)->hdr & SIZE_MASK)
#define IS_FREE(p)        (((p)->hdr & FREE_BIT) != 0)
#define IS_FREE_ABOVE(p)  (((p)->hdr & FREE_ABOVE_BIT) != 0)

/* Given that we have a free block above find it's size */
#define SIZEOF_ABOVE(p)    *(((EWord*) (p)) - 1)

#define MIN_BLOCK_SIZE      (sizeof(FreeBlock)/sizeof(EWord))
#define MIN_WORD_SIZE       (MIN_BLOCK_SIZE-1)
#define MIN_BYTE_SIZE       (sizeof(FreeBlock)-sizeof(EWord))

#define MIN_ALIGN_SIZE      ALIGN_SIZE(MIN_BYTE_SIZE)


static AllocatedBlock* heap_head = 0;
static AllocatedBlock* heap_tail = 0;
static EWord eheap_size = 0;

/* we must have room for head,tail,next,prev */

erl_mutex_t heap_lock;
static int heap_locked;

/* Fixed block pointer chains:
** Small blocks are stored for direct indexing with size (3..2^FIXED_BASE)
** index 0, 1 and 2 are not used
*/
static FreeBlock* h_fixed[FIXED];
 
/* Variable block pointer chains:
** store blocks of lengths < 2^i
** h_dynamic[i] contains block of sizes S where
** 2^i+FIXED_BASE <= S < 2^(i+FIXED_BASE+1)
*/
static FreeBlock* h_dynamic[DYNAMIC];

static int elib_need_init = 1;

static FUNCTION(void, deallocate, (AllocatedBlock*, int));

/*
** Unlink a free block
*/

#ifdef ELIB_INLINE

#define unlink_block(p) do { \
   if ((*(p)->prev = (p)->next) != 0) \
      (p)->next->prev = (p)->prev; \
   } while(0)


#define mark_allocated(p, szp) do { \
      (p)->hdr = ((p)->hdr & FREE_ABOVE_BIT) | (szp); \
      (p)->v[szp] &= ~FREE_ABOVE_BIT; \
   } while(0)


#define mark_free(p, szp) do { \
      (p)->hdr = FREE_BIT | (szp); \
      (p)->v[(szp)-3] = szp; \
   } while(0)

/* Help macros to log2 */
#define LOG_1(x)  (((x) > 1) ? 1 : 0)
#define LOG_2(x)  (((x) > 3) ? 2+LOG_1((x) >> 2) : LOG_1(x))
#define LOG_4(x)  (((x) > 15) ? 4+LOG_2((x) >> 4) : LOG_2(x))
#define LOG_8(x)  (((x) > 255) ? 8+LOG_4((x)>>8) : LOG_4(x))
#define LOG_16(x) (((x) > 65535) ? 16+LOG_8((x)>>16) : LOG_8(x))

#define log2(x)   LOG_16(x)

#else

static void unlink_block(p, sz)
FreeBlock* p; EWord sz;
{
    if ((*p->prev = p->next) != 0)
	p->next->prev = p->prev;
}

/*
** Mark block p as allocated (includes changing the free bit)
*/
static void mark_allocated(p,szp)
AllocatedBlock* p; EWord szp;
{
    p->hdr = (p->hdr & FREE_ABOVE_BIT) | szp;
    p->v[szp] &= ~FREE_ABOVE_BIT;  /* OBJECT BELOW P !!! */
}

static void mark_free(p, szp)
FreeBlock* p; EWord szp;
{
    p->hdr = FREE_BIT | szp;
    p->v[szp-3] = szp;
}

static int log2(n)
EWord n;
{
    int lg = 0;

    while (n >>= 1)
	lg++;
    return lg;
}

#endif

/*
** Link a free block into a free list
*/
static void link_block(p, sz)
FreeBlock* p; EWord sz;
{
    FreeBlock** hp;

    if (sz < FIXED)
	hp = &h_fixed[sz];
    else {
	int ix = log2(sz >> FIXED_BASE);
	hp = &h_dynamic[ix];
    }
#ifdef ELIB_SORTED_BLOCKS
    while(*hp > p)
	hp = &((*hp)->next);
#endif
    if ((p->next = *hp) != 0)
	p->next->prev = &p->next;
    p->prev = hp;
    *hp = p;
}

/*
** Split a block to be allocated.
** Mark block as ALLOCATED and clear
** FREE_ABOVE_BIT on next block
**
** nw is SIZE aligned and szp is SIZE aligned + 1
*/
static void split_block(p, nw, szp)
FreeBlock* p; EWord nw; EWord szp;
{
    EWord szq;
    FreeBlock* q;

    szq = szp - nw;
    /* Preserve FREE_ABOVE bit in p->hdr !!! */

    if (szq >= MIN_ALIGN_SIZE+1) {
	szq--;
	p->hdr = (p->hdr & FREE_ABOVE_BIT) | nw;

	q = (FreeBlock*) (((EWord*) p) + nw + 1);
	q->hdr = FREE_BIT | szq;
	q->v[szq-3] = szq;
	link_block(q, szq);

	q = (FreeBlock*) (((EWord*) q) + szq + 1);
	q->hdr |= FREE_ABOVE_BIT;
    }
    else {
	mark_allocated((AllocatedBlock*)p, szp);
    }
}

/*
** Find a free block
*/
static FreeBlock* alloc_block(nw)
EWord nw;
{
    FreeBlock* p;
    EWord szp;

    if (nw < FIXED) {
	if ((p = h_fixed[nw]) != 0)
	    return p;
	szp = nw + 1;
	while(szp < FIXED) {
	    if ((p = h_fixed[szp]) != 0)
		return p;
	    szp++;
	}
    }

    while(1) {
	int ix = log2(nw >> FIXED_BASE);

	/* scan for first fit */
	if ((p = h_dynamic[ix]) != 0) {
	    int limit = 256;	/* Max number of blocks to look at. */

	    /*
	     * We don't want to go on searching this list forever,
	     * since it might turn out that all blocks are too small.
	     */
	    while (p != 0 && limit-- > 0) {
		if (SIZEOF(p) >= nw) {
		    return p;
		}
		p = p->next;
		limit--;
	    }
	}

	ix++;
	/* scan for first fit (they must all be greater than nw) */
	while(ix < DYNAMIC && ((p = h_dynamic[ix]) == 0))
	    ix++;
	if (p != 0)
	    return p;
	if (ELIB_EXPAND(nw+MIN_WORD_SIZE))
	    return 0;
    }
}


size_t elib_sizeof(p)
void *p;
{
    AllocatedBlock* pp;

    if (p != 0) {
	pp = (AllocatedBlock*) (p-1);
	return SIZEOF(pp);
    }
    return 0;
}

/*
** Initialize the elib
** The addr and sz is only used when compiled with EXPAND_ADDR 
*/
/* Not static, this is used by VxWorks */
void elib_init(addr, sz)
EWord* addr; EWord sz;
{
    int i;
    FreeBlock* freep;
    EWord tmp_sz;
#ifdef ELIB_HEAP_SBRK
    char* top;
    EWord n;
#endif
    if (!elib_need_init)
	return;

    heap_lock = erl_mutex_sys(0);

    erl_mutex_lock(heap_lock);


    for (i = 0; i < FIXED; i++)
	h_fixed[i] = 0;
    
    for (i = 0; i < DYNAMIC; i++)
	h_dynamic[i] = 0;

    /* Get the page size (may involve system call!!!) */
    page_size = PAGE_SIZE;

#if defined(ELIB_HEAP_SBRK)
    sz = PAGES(ELIB_HEAP_SIZE)*page_size;

    if ((top = (char*) sbrk(0)) == (char*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(0)");
	ELIB_FAILURE;
    }
    n = PAGE_ALIGN(top) - top;
    if ((top = (char*) sbrk(n)) == (char*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(n)");
	ELIB_FAILURE;
    }
    if ((eheap = (EWord*) sbrk(sz)) == (EWord*)-1) {
	elib_printf(stderr, "could not initialize elib, sbrk(SIZE)");
	ELIB_FAILURE;
    }
    sz = WORDS(ELIB_HEAP_SIZE);
#elif defined(ELIB_HEAP_FIXED)
    eheap = fix_heap;
    sz = WORDS(ELIB_HEAP_SIZE);
#elif defined(ELIB_HEAP_USER)
    eheap = addr;
    sz = WORDS(sz);
#else
    return -1;
#endif
    eheap_size = 0;

    /* Make sure that the first word of the heap_head is aligned */
    addr = ALIGN(eheap+1);
    sz -= ((addr - 1) - eheap);      /* Subtract unusable size */
    eheap_top = eheap = addr - 1;    /* Set new aligned heap start */

    eheap_top[sz-1] = 0;	     /* Heap stop mark */

    addr = eheap;
    heap_head = (AllocatedBlock*) addr;
    heap_head->hdr = MIN_ALIGN_SIZE;
    for (i = 0; i < MIN_ALIGN_SIZE; i++)
	heap_head->v[i] = 0;

    addr += (MIN_ALIGN_SIZE+1);
    freep = (FreeBlock*) addr;
    tmp_sz = sz - (((MIN_ALIGN_SIZE+1) + MIN_BLOCK_SIZE) + 1 + 1);
    freep->hdr = FREE_BIT | tmp_sz;
    freep->v[tmp_sz-3] = tmp_sz;

    link_block(freep, tmp_sz);

    /* No need to align heap tail */
    heap_tail = (AllocatedBlock*) &eheap_top[sz-5];
    heap_tail->hdr = FREE_ABOVE_BIT | MIN_WORD_SIZE;
    heap_tail->v[0] = 0;
    heap_tail->v[1] = 0;
    heap_tail->v[2] = 0;

    eheap_top += sz;
    eheap_size += sz;

    elib_need_init = 0;
    erl_mutex_unlock(heap_lock);
    heap_locked = 0;
}

#ifdef ELIB_HEAP_USER
void elib_force_init(addr, sz)
EWord* addr; EWord sz;
{
    elib_need_init = 1;
    elib_init(addr,sz);
}
#endif

#ifdef ELIB_HEAP_SBRK

/*
** need in number of words (should include head and tail words)
*/
static int expand_sbrk(sz)
EWord sz;
{
    EWord* p;
    EWord  bytes = sz * sizeof(EWord);
    EWord  size;
    AllocatedBlock* tail;

    if (bytes < ELIB_HEAP_SIZE)
	size = PAGES(ELIB_HEAP_INCREAMENT)*page_size;
    else
	size = PAGES(bytes)*page_size;

    if ((p = (EWord*) sbrk(size)) == ((EWord*) -1))
	return -1;

    if (p != eheap_top) {
	elib_printf(stderr, "panic: sbrk moved\n");
	ELIB_FAILURE;
    }

    sz = WORDS(size);

    /* Set new endof heap marker and a new heap tail */
    eheap_top[sz-1] = 0;

    tail = (AllocatedBlock*) &eheap_top[sz-5];
    tail->hdr = FREE_ABOVE_BIT | MIN_WORD_SIZE;
    tail->v[0] = 0;
    tail->v[1] = 0;
    tail->v[2] = 0;

    /* Patch old tail with new appended size */
    heap_tail->hdr = (heap_tail->hdr & FREE_ABOVE_BIT) |
	(MIN_WORD_SIZE+1+(sz-5));
    deallocate(heap_tail, 0);

    heap_tail = tail;

    eheap_size += sz;
    eheap_top += sz;

    return 0;
}

#endif /* ELIB_HEAP_SBRK */


/*
** Scan heap and check for corrupted heap
*/
int elib_check_heap()
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    if (heap_locked) {
	elib_printf(stderr, "heap is locked no info avaiable\n");
	return 0;
    }

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    if (p->v[sz-1] != sz) {
		elib_printf(stderr, "panic: heap corrupted\r\n"); 
		ELIB_FAILURE;	
	    }
	    p = (AllocatedBlock*) (p->v + sz);
	    if (!IS_FREE_ABOVE(p)) {
		elib_printf(stderr, "panic: heap corrupted\r\n"); 
		ELIB_FAILURE;	
	    }
	}
	else
	    p = (AllocatedBlock*) (p->v + sz);
    }
    return 1;
}

/*
** Load the byte vector pointed to by v of length vsz
** with a heap image
** The scale is defined by vsz and the current heap size
** free = 0, full = 255
** 
** 
*/
int elib_heap_map(v, vsz)
EByte* v; int vsz;
{
    AllocatedBlock* p = heap_head;
    EWord sz;
    int gsz = eheap_size / vsz;  /* The granuality used */
    int fsz = 0;
    int usz = 0;

    if (gsz == 0)
	return -1;  /* too good reolution */

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    fsz += sz;
	    if ((fsz + usz) > gsz) {
		*v++ = (255*usz)/gsz;
		fsz -= (gsz - usz);
		usz = 0;
		while(fsz >= gsz) {
		    *v++ = 0;
		    fsz -= gsz;
		}
	    }
	}
	else {
	    usz += sz;
	    if ((fsz + usz) > gsz) {
		*v++ = 255 - (255*fsz)/gsz;
		usz -= (gsz - fsz);
		fsz = 0;
		while(usz >= gsz) {
		    *v++ = 255;
		    usz -= gsz;
		}
	    }
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Generate a histogram of free/allocated blocks
** Count granuality of 10 gives
** (0-10],(10-100],(100-1000],(1000-10000] ...
** (0-2], (2-4], (4-8], (8-16], ....
*/
static int i_logb(size, base)
EWord size; int base;
{
    int lg = 0;
    while(size >= base) {
	size /= base;
	lg++;
    }
    return lg;
}

int elib_histo(vf, va, vsz, base)
EWord* vf; EWord* va; int vsz; int base;
{
    AllocatedBlock* p = heap_head;
    EWord sz;
    int i;
    int linear;

    if ((vsz <= 1) || (vf == 0 && va == 0))
	return -1;

    if (base < 0) {
	linear = 1;
	base = -base;
    }
    else
	linear = 0;

    if (base <= 1)
	return -1;

    if (vf != 0) {
	for (i = 0; i < vsz; i++)
	    vf[i] = 0;
    }
    if (va != 0) {
	for (i = 0; i < vsz; i++)
	    va[i] = 0;
    }

    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    if (vf != 0) {
		int val;
		if (linear)
		    val = sz / base;
		else
		    val = i_logb(sz, base);
		if (val >= vsz)
		    vf[vsz-1]++;
		else
		    vf[val]++;
	    }
	}
	else {
	    if (va != 0) {
		int val;
		if (linear)
		    val = sz / base;
		else
		    val = i_logb(sz, base);
		if (val >= vsz)
		    va[vsz-1]++;
		else
		    va[val]++;
	    }
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Fill the info structure with actual values
** Total
** Allocated
** Free
** maxMaxFree     
*/
void elib_stat(info)
struct elib_stat* info;
{
    EWord blks = 0;
    EWord sz_free = 0;
    EWord sz_alloc = 0;
    EWord sz_max_free = 0;
    EWord sz_min_used = 0x7fffffff;
    EWord sz;
    EWord num_free = 0;
    AllocatedBlock* p = heap_head;

    info->mem_total = eheap_size;

    while((sz = SIZEOF(p)) != 0) {
	blks++;
	if (IS_FREE(p)) {
	    if (sz > sz_max_free)
		sz_max_free = sz;
	    sz_free += sz;
	    ++num_free;
	}
	else {
	    if (sz < sz_min_used)
		sz_min_used = sz;
	    sz_alloc += sz;
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
    info->mem_blocks = blks;
    info->free_blocks = num_free;
    info->mem_alloc = sz_alloc;
    info->mem_free = sz_free;
    info->min_used = sz_min_used;
    info->max_free = sz_max_free;
}

/*
** Dump the heap
*/
void elib_heap_dump(label)
char* label;
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    elib_printf(stderr, "HEAP DUMP (%s)\n", label);
    if (!elib_check_heap())
	return;
    
    while((sz = SIZEOF(p)) != 0) {
	if (IS_FREE(p)) {
	    elib_printf(stderr, "%p: FREE, size = %d\n", p, (int) sz);
	}
	else {
	    elib_printf(stderr, "%p: USED, size = %d %s\n", p, (int) sz,
		       IS_FREE_ABOVE(p)?"(FREE ABOVE)":"");
	}
	p = (AllocatedBlock*) (p->v + sz);
    }
}

/*
**  Scan heaps and count:
**  free_size, allocated_size, max_free_block
*/
void elib_statistics(to)
void* to;
{
    struct elib_stat info;
    EWord frag;

#ifdef DEBUG
    int i;
    EWord sz_fixed[FIXED];
    EWord sz_dynamic[DYNAMIC];
    FreeBlock* p;
    EByte map[80];
#endif

    if (!elib_check_heap())
	return;

    elib_stat(&info);

    frag = 1000 - ((1000 * info.max_free) / info.mem_free);

    elib_printf(to, "Heap Statistics: total(%d), blocks(%d), frag(%d.%d%%)\n", 
	       info.mem_total, info.mem_blocks,
	       (int) frag/10, (int) frag % 10);

    elib_printf(to, "                 allocated(%d), free(%d), "
		"free_blocks(%d)\n",
	       info.mem_alloc, info.mem_free,info.free_blocks);
    elib_printf(to, "                 max_free(%d),  min_used(%d)\n",
	       info.max_free, info.min_used);


#ifdef DEBUG
    for (i = 0; i < FIXED; i++) {
	EWord  sz = 0;

	p = h_fixed[i];
	while (p != 0) {
	    sz += SIZEOF(p);
	    p = p->next;
	}
	sz_fixed[i] = sz;
    }

    for (i = 0; i < DYNAMIC; i++) {
	EWord sz = 0;

	p = h_dynamic[i];
	while (p != 0) {
	    sz += SIZEOF(p);
	    p = p->next;
	}
	sz_dynamic[i] = sz;
    }

    elib_printf(to, "Fixed free lists:\n");
    for (i = 0; i < FIXED; i++)
	elib_printf(to, "size[%d] = %d\n", i, sz_fixed[i]);

    elib_printf(to, "Dynamic free lists\n");
    for (i = 0; i < DYNAMIC; i++)
	elib_printf(to, "size < 2^%d = %d\n", i+5, sz_dynamic[i]);

    elib_heap_map(map, 80);
    elib_printf(to, "Simple heap display:\n");
    for (i = 0; i < 80; i++) {
	if (map[i] < 70)
	    elib_putc(' ', to);
	else if (map[i] < 150)
	    elib_putc('+', to);
	else if (map[i] < 200)
	    elib_putc('*', to);
	else
	    elib_putc('#', to);
    }
    elib_putc('\n', to);
#endif /* DEBUG */
}

/*
** Allocate a least nb bytes
** Algorithm:
**    1) Try locate a block which match exacly among the by direct index.
**    2) Try using a fix block of greater size
**    3) Try locate a block by searching in lists where block sizes
**       X may vary between 2^i < X <= 2^(i+1)
**
** Reset memory to zero if clear is true
*/
static AllocatedBlock* allocate(nb, clear)
EWord nb; int clear;
{
    FreeBlock* p;
    EWord nw;

    if (nb < MIN_BYTE_SIZE)
	nw = MIN_ALIGN_SIZE;
    else
	nw = ALIGN_SIZE(nb);

    erl_mutex_lock(heap_lock);

    if ((p = alloc_block(nw)) == 0) {
	erl_mutex_unlock(heap_lock);	
	return 0;
    }

    unlink_block(p);
    split_block(p, nw, SIZEOF(p));
    
    if (clear) {
	EWord* pp = ((AllocatedBlock*)p)->v;

	while(nw--)
	    *pp++ = 0;
    }

    erl_mutex_unlock(heap_lock);
    return (AllocatedBlock*) p;
}


/*
** Deallocate memory pointed to by p
** 1. Merge with block above if this block is free
** 2. Merge with block below if this block is free
** Link the block to the correct free list
**
** p points to the block header!
**
*/
static void deallocate(p, need_lock)
AllocatedBlock* p; int need_lock;
{
    FreeBlock* q;
    EWord szq;
    EWord szp;

    if (need_lock)
	erl_mutex_lock(heap_lock);

    szp = SIZEOF(p);

    if (IS_FREE_ABOVE(p)) {
	szq = SIZEOF_ABOVE(p);
	q = (FreeBlock*) ( ((EWord*) p) - szq - 1);
	unlink_block(q);

	p = (AllocatedBlock*) q;
	szp += (szq + 1);
    }
    q = (FreeBlock*) (p->v + szp);
    if (IS_FREE(q)) {
	szq = SIZEOF(q);
	unlink_block(q);
	szp += (szq + 1);
    }
    else
	q->hdr |= FREE_ABOVE_BIT;

    /* The block above p can NEVER be free !!! */
    p->hdr = FREE_BIT | szp;
    p->v[szp-1] = szp;

    link_block((FreeBlock*) p, szp);

    if (need_lock)
	erl_mutex_unlock(heap_lock);
}

/*
** Reallocate memory
** If preserve is true then data is moved if neccesary
*/
static AllocatedBlock* reallocate(p, nb, preserve)
AllocatedBlock* p; EWord nb; int preserve;
{
    EWord szp;
    EWord szq;
    EWord sz;
    EWord nw;
    FreeBlock* q;

    if (nb < MIN_BYTE_SIZE)
	nw = MIN_ALIGN_SIZE;
    else
	nw = ALIGN_SIZE(nb);

    erl_mutex_lock(heap_lock);

    sz = szp = SIZEOF(p);

    /* Merge with block below */
    q = (FreeBlock*) (p->v + szp);
    if (IS_FREE(q)) {
	szq = SIZEOF(q);
	unlink_block(q);
	szp += (szq + 1);
    }

    if (nw <= szp) {
	split_block(p, nw, szp);
	erl_mutex_unlock(heap_lock);
	return p;
    }
    else {
	EWord* dp = p->v;
	AllocatedBlock* npp;

	if (IS_FREE_ABOVE(p)) {
	    szq = SIZEOF_ABOVE(p);
	    if (szq + szp + 1 >= nw) {
		q = (FreeBlock*) (((EWord*) p) - szq - 1);
		unlink_block(q);
		szp += (szq + 1);
		p = (AllocatedBlock*) q;

		if (preserve) {
		    EWord* pp = p->v;
		    while(sz--)
			*pp++ = *dp++;
		}
		split_block(p, nw, szp);
		erl_mutex_unlock(heap_lock);
		return p;
	    }
	}

	/*
	 * Update p so that allocate() and deallocate() works.
	 * (Note that allocate() may call expand_sbrk(), which in
	 * in turn calls deallocate().)
	 */

	p->hdr = (p->hdr & FREE_ABOVE_BIT) | szp;
	p->v[szp] &= ~FREE_ABOVE_BIT;

	erl_mutex_unlock(heap_lock);

	npp = allocate(nb, 0);
	if(npp == NULL)
	    return NULL;
	if (preserve) {
	    EWord* pp = npp->v;
	    while(sz--)
		*pp++ = *dp++;
	}
	deallocate(p, 1);
	return npp;
    }
}

/*
** What malloc() and friends should do (and return) when the heap is
** exhausted.  [sverkerw]
*/
static void* heap_exhausted()
{
    /* Choose behaviour */
#if 1
    /* Crash-and-burn --- leave a usable corpse (hopefully) */
    abort();
#endif    
    /* The usual ANSI-compliant behaviour */
    return 0;
}

/*
** Allocate size bytes of memory
*/
void* ELIB_PREFIX(malloc, (nb))
size_t nb;
{
    AllocatedBlock* p;

    if (elib_need_init)
	elib_init(NULL,(EWord)0);
    if (nb == 0)
	return 0;
    if ((p = allocate(nb, 0)) != 0) {
	ELIB_ALIGN_CHECK(p->v);
	return p->v;
    }
    return heap_exhausted();
}


void* ELIB_PREFIX(calloc, (nelem, size))
size_t nelem; size_t size;
{
    int nb;
    AllocatedBlock* p;
    
    if (elib_need_init)
	elib_init(NULL,(EWord)0);

    if ((nb = nelem * size) == 0)
	return 0;
    if ((p = allocate(nb, 1)) != 0) {
	ELIB_ALIGN_CHECK(p->v);
	return p->v;
    }
    return heap_exhausted();
}

/*
** Free memory allocated by malloc
*/

void ELIB_PREFIX(free, (p))
EWord* p;
{
    if (p != 0)
	deallocate((AllocatedBlock*)(p-1), 1);
}

/*
** Realloc the memory allocated in p to nb number of bytes
**
*/

void* ELIB_PREFIX(realloc, (p, nb))
EWord* p; size_t nb;
{
    AllocatedBlock* pp;

    if (p != 0) {
	pp = (AllocatedBlock*) (p-1);
	if (nb > 0) {
	    if ((pp = reallocate(pp, nb, 1)) != 0) {
		ELIB_ALIGN_CHECK(pp->v);
		return pp->v;
	    }
	}
	else
	    deallocate(pp, 1);
    }
    else if (nb > 0) {
	if (elib_need_init)
	  elib_init(NULL,(EWord)0);
	if ((pp = allocate(nb, 0)) != 0) {
	    ELIB_ALIGN_CHECK(pp->v);
	    return pp->v;
	}
	else
	    return heap_exhausted();
    }
    return 0;
}

/*
** Resize the memory area pointed to by p with nb number of bytes
*/
void* ELIB_PREFIX(memresize, (p, nb))
EWord* p; int nb;
{
    AllocatedBlock* pp;

    if (p != 0) {
	pp = (AllocatedBlock*) (p-1);
	if (nb > 0) {
	    if ((pp = reallocate(pp, nb, 0)) != 0) {
		ELIB_ALIGN_CHECK(pp->v);
		return pp->v;
	    }
	}
	else
	    deallocate(pp, 1);
    }
    else if (nb > 0) {
	if (elib_need_init)
	  elib_init(NULL,(EWord)0);
	if ((pp = allocate(nb, 0)) != 0) {
	    ELIB_ALIGN_CHECK(pp->v);
	    return pp->v;
	}
	else
	    return heap_exhausted();
    }
    return 0;
}


/* Create aligned memory a must be a power of 2 !!! */

void* ELIB_PREFIX(memalign, (a, nb))
int a; int nb;
{
    EWord nw;

    if ((nb == 0) || (a <= 0))
	return 0;
    if (a < ELIB_ALIGN)
	a = ELIB_ALIGN;

    /* Calculate the amount to allocated to guarantee alignment */
    if ((nw = WORDS(nb+a-1)) < MIN_WORD_SIZE)
	nw = MIN_WORD_SIZE;

    if (elib_need_init)
      elib_init(NULL,(EWord)0);

    return 0;  /* NOT YET */
}

/* Return memory size for pointer p in bytes */

int ELIB_PREFIX(memsize, (p))
EWord* p;
{
    return SIZEOF((AllocatedBlock*)(p-1))*4;
}


/*
** --------------------------------------------------------------------------
**   DEBUG LIBRARY
** --------------------------------------------------------------------------
*/

#ifdef ELIB_DEBUG

#define IN_HEAP(p)  (((p) >= (char*) eheap) && (p) < (char*) eheap_top)
/*
** ptr_to_block: return the pointer to heap block pointed into by ptr
** Returns 0 if not pointing into a block
*/

static EWord* ptr_to_block(ptr)
char* ptr;
{
    AllocatedBlock* p = heap_head;
    EWord sz;

    while((sz = SIZEOF(p)) != 0) {
	if ((ptr >= (char*) p->v) && (ptr < (char*)(p->v+sz)))
	    return p->v;
	p = (AllocatedBlock*) (p->v + sz);
    }
    return 0;
}

/*
** Validate a pointer
** returns:
**      0  - if points to start of a block
**      1  - if points outsize heap
**     -1  - if points inside block
**
*/
static int check_pointer(ptr)
char* ptr;
{
    if (IN_HEAP(ptr)) {
	if (ptr_to_block(ptr) == 0)
	    return 1;
	return 0;
    }
    return -1;
}

/*
** Validate a memory area
** returns:
**      0 - if area is included in a block
**     -1 - if area overlap a heap block
**      1 - if area is outside heap
*/
static int check_area(ptr, n)
char* ptr; int n;
{
    if (IN_HEAP(ptr)) {
	if (IN_HEAP(ptr+n-1)) {
	    EWord* p1 = ptr_to_block(ptr);
	    EWord* p2 = ptr_to_block(ptr+n-1);

	    if (p1 == p2)
		return (p1 == 0) ? -1 : 0;
	    return -1;
	}
    }
    else if (IN_HEAP(ptr+n-1))
	return -1;
    return 1;
}

/*
** Check if a block write will overwrite heap block
*/
static void check_write(ptr, n, file, line, fun)
char* ptr; int n;
char* file; int line; char* fun;
{
    if (check_area(ptr, n) == -1) {
	elib_printf(stderr, "RUNTIME ERROR: %s heap overwrite\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }
}

/*
** Check if a pointer is an allocated object
*/
static void check_allocated_block(ptr, file, line, fun)
char* ptr; char* file; int line; char* fun;
{
    EWord* q;

    if (!IN_HEAP(ptr) || ((q=ptr_to_block(ptr)) == 0) || (ptr != (char*) q)) {
	elib_printf(stderr, "RUNTIME ERROR: %s non heap pointer\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }

    if (IS_FREE((AllocatedBlock*)(q-1))) {
	elib_printf(stderr, "RUNTIME ERROR: %s free pointer\n", fun);
	elib_printf(stderr, "File: %s Line: %d\n", file, line);
	ELIB_FAILURE;
    }

}

/*
** --------------------------------------------------------------------------
**  DEBUG VERSIONS (COMPILED WITH THE ELIB.H)
** --------------------------------------------------------------------------
*/

void* elib_dbg_malloc(n, file, line)
int n; char* file; int line;
{
    return elib__malloc(n);
}

void* elib_dbg_calloc(n, s, file, line)
int n; int s; char* file; int line;
{
    return elib__calloc(n, s);
}

void* elib_dbg_realloc(p, n, file, line)
EWord* p; int n;
char* file; int line;
{
    if (p == 0)
	return elib__malloc(n);
    check_allocated_block(p, file, line, "elib_realloc");
    return elib__realloc(p, n);
}

void elib_dbg_free(p, file, line)
EWord* p;
char* file; int line;
{
    if (p == 0)
	return;
    check_allocated_block(p, file, line, "elib_free");
    elib__free(p);
}

void* elib_dbg_memalign(a, n, file, line)
int a; int n; char* file; int line;
{
    return elib__memalign(a, n);
}

void* elib_dbg_memresize(p, n, file, line)
EWord* p; int n;
char* file; int line;
{
    if (p == 0)
	return elib__malloc(n);
    check_allocated_block(p, file, line, "elib_memresize");
    return elib__memresize(p, n);
}

int elib_dbg_memsize(p, file, line)
void* p; char* file; int line;
{
    check_allocated_block(p, file, line, "elib_memsize");
    return elib__memsize(p);
}

/*
** --------------------------------------------------------------------------
**  LINK TIME FUNCTIONS (NOT COMPILED CALLS)
** --------------------------------------------------------------------------
*/

void* elib_malloc(n)
int n;
{
    return elib_dbg_malloc(n, "", -1);
}

void* elib_calloc(n, s)
int n, s;
{
    return elib_dbg_calloc(n, s, "", -1);
}

void* elib_realloc(p, n)     
EWord* p; int n;
{
    return elib_dbg_realloc(p, n, "", -1);
}

void elib_free(p)
EWord* p;
{
    elib_dbg_free(p, "", -1);
}

void* elib_memalign(a, n)
int a; int n;
{
    return elib_dbg_memalign(a, n, "", -1);
}

void* elib_memresize(p, n)
EWord* p; int n;
{
    return elib_dbg_memresize(p, n, "", -1);
}


int elib_memsize(p)
EWord* p;
{
    return elib_dbg_memsize(p, "", -1);
}

#endif /* ELIB_DEBUG */

/*
** --------------------------------------------------------------------------
** Map c library functions to elib
** --------------------------------------------------------------------------
*/

#if defined(ELIB_ALLOC_IS_CLIB)

void* malloc(nb)
size_t nb;
{
    return elib_malloc(nb);
}

void* calloc(nelem, size)
size_t nelem;
size_t size;
{
    return elib_calloc(nelem, size);
}

void free(p)
void *p;
{
    elib_free(p);
}


void* realloc(p, nb)
void* p;
size_t nb;
{
    return elib_realloc(p, nb);
}


void* memalign(a, s)
size_t a;
size_t s;
{
    return elib_memalign(a, s);
}

void* memresize(p, nb)
void* p; int nb;
{
    return elib_memresize(p, nb);
}

int memsize(p)
void* p;
{
    return elib_memsize(p);
}

#endif /* ELIB_ALLOC_IS_CLIB */

#endif /* ENABLE_ELIB_MALLOC */
