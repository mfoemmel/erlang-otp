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
 * Description:	A memory segment allocator. Segments that are deallocated
 *              are kept for a while in a segment "cache" before they are
 *              destroyed. When segments are allocated, cached segments
 *              are used if possible instead of creating new segments.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_mseg.h"
#include "global.h"

#if HAVE_ERTS_MSEG

#ifndef HAVE_GETPAGESIZE
#define HAVE_GETPAGESIZE 0
#endif

#ifdef _SC_PAGESIZE
#  define GET_PAGE_SIZE sysconf(_SC_PAGESIZE)
#elif HAVE_GETPAGESIZE
#  define GET_PAGE_SIZE getpagesize()
#else
#  error "Page size unknown"
     /* Implement some other way to get the real page size if needed! */
#endif

#define MAX_CACHE_SIZE 30

#undef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#undef MAX
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#undef  PAGE_MASK
#define INV_PAGE_MASK	((Uint) (page_size - 1))
#define PAGE_MASK	(~INV_PAGE_MASK)
#define PAGE_FLOOR(X)	((X) & PAGE_MASK)
#define PAGE_CEILING(X)	PAGE_FLOOR((X) + INV_PAGE_MASK)
#define PAGES(X)	((X) >> page_shift)

static int atoms_initialized;

static Uint cache_check_interval;

static void check_cache(void *unused);
static int is_cache_check_scheduled;

#if HAVE_MMAP
/* Mmap ... */

#define MMAP_PROT		(PROT_READ|PROT_WRITE)
#ifdef MAP_ANON
#  define MMAP_FLAGS		(MAP_ANON|MAP_PRIVATE)
#  define MMAP_FD		(-1)
#else
#  define MMAP_FLAGS		(MAP_PRIVATE)
#  define MMAP_FD		mmap_fd
static int mmap_fd;
#endif

#if HAVE_MREMAP
#  define HAVE_MSEG_RECREATE 1
#else
#  define HAVE_MSEG_RECREATE 0
#endif

#define CAN_PARTLY_DESTROY 1
#else  /* #if HAVE_MMAP */
#define CAN_PARTLY_DESTROY 0
#error "Not supported"
#endif /* #if HAVE_MMAP */

#if defined(ERTS_MSEG_FAKE_SEGMENTS)
#undef CAN_PARTLY_DESTROY
#define CAN_PARTLY_DESTROY 0
#endif

static const ErtsMsegOpt_t default_opt = ERTS_MSEG_DEFAULT_OPT_INITIALIZER;

typedef struct cache_desc_t_ {
    void *seg;
    Uint size;
    struct cache_desc_t_ *next;
    struct cache_desc_t_ *prev;
} cache_desc_t;

typedef struct {
    Uint32 giga_no;
    Uint32 no;
} CallCounter;

static int is_init_done;
static Uint page_size;
static Uint page_shift;

static struct {
    CallCounter alloc;
    CallCounter dealloc;
    CallCounter realloc;
    CallCounter create;
    CallCounter destroy;
#if HAVE_MSEG_RECREATE
    CallCounter recreate;
#endif
    CallCounter clear_cache;
    CallCounter check_cache;
} calls;

static cache_desc_t cache_descs[MAX_CACHE_SIZE];
static cache_desc_t *free_cache_descs;
static cache_desc_t *cache;
static cache_desc_t *cache_end;
static Uint cache_hits;
static Uint cache_size;
static Uint min_cached_seg_size;
static Uint max_cached_seg_size;

static Uint max_cache_size;
static Uint abs_max_cache_bad_fit;
static Uint rel_max_cache_bad_fit;

#if CAN_PARTLY_DESTROY
static Uint min_seg_size;
#endif

static Sint no_of_segments;
static Sint no_of_segments_watermark;

#define ONE_GIGA (1000000000)

#define ZERO_CC(CC) (calls.CC.no = 0, calls.CC.giga_no = 0)

#define INC_CC(CC) (calls.CC.no == ONE_GIGA - 1				\
		    ? (calls.CC.giga_no++, calls.CC.no = 0)		\
		    : calls.CC.no++)

#define DEC_CC(CC) (calls.CC.no == 0					\
		    ? (calls.CC.giga_no--,				\
		       calls.CC.no = ONE_GIGA - 1)			\
		    : calls.CC.no--)

#ifdef USE_THREADS

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Multi-threaded case                                                     *
\*                                                                         */

#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"

#undef LOCK
#undef UNLOCK
#undef WAIT
#undef TIMED_WAIT
#undef SIGNAL

#define MSEG_MUTEX_NO 5

#if defined(DEBUG) && defined(POSIX_THREADS)
#define LOCK(M) ASSERT(0 == erts_mutex_lock((M)))
#define UNLOCK(M) ASSERT(0 == erts_mutex_unlock((M)))
#define WAIT(C, M) ASSERT(0 == erts_cond_wait((C),(M)))
#define TIMED_WAIT(C, M, T)						\
do {									\
    int res__ = erts_cond_timedwait((C),(M),(T));			\
    ASSERT(res__ == 0 || res__ == ETIMEDOUT);				\
} while (0)
#define SIGNAL(C) ASSERT(0 == erts_cond_signal((C)))
#else
#define LOCK(M) ((void) erts_mutex_lock((M)))
#define UNLOCK(M) ((void) erts_mutex_unlock((M)))
#define WAIT(C, M) ((void) erts_cond_wait((C),(M)))
#define TIMED_WAIT(C, M, T) ((void) erts_cond_timedwait((C),(M),(T)))
#define SIGNAL(C) ((void) erts_cond_signal((C)))
#endif

static erts_mutex_t mseg_mutex;
static erts_cond_t mseg_cond;

static void thread_safe_init(void)
{
    mseg_mutex = erts_mutex_sys(ERTS_MUTEX_SYS_MSEG);
    if(!mseg_mutex || erts_mutex_set_default_atfork(mseg_mutex))
	erl_exit(1, "Failed to initialize mseg mutex\n");

}

static SysTimeval check_time;
static erts_thread_t mseg_cc_tid;

static void *
mseg_cache_cleaner(void *unused)
{
    LOCK(mseg_mutex);

    while (1) {

	while (!is_cache_check_scheduled)
	    WAIT(mseg_cond, mseg_mutex);

	while (1) {
	    SysTimeval time;
	    long wait_time;
	    sys_gettimeofday(&time);

	    if (time.tv_sec == check_time.tv_sec
		&& time.tv_usec >= check_time.tv_usec)
		break;

	    if (time.tv_sec > check_time.tv_sec)
		break;

	    wait_time = (check_time.tv_sec - time.tv_sec)*1000;
	    wait_time += (check_time.tv_usec - time.tv_usec)/1000;
	    wait_time++;

	    TIMED_WAIT(mseg_cond, mseg_mutex, wait_time);
	}

	check_cache(NULL);
    }

    UNLOCK(mseg_mutex); /* Actually not needed */
    return NULL;
}

static ERTS_INLINE void
schedule_cache_check(void)
{
    if (!is_cache_check_scheduled && is_init_done) {
	sys_gettimeofday(&check_time);
	check_time.tv_sec += cache_check_interval / 1000;
	check_time.tv_usec += (cache_check_interval % 1000)*1000;
	if (check_time.tv_usec >= 1000000) {
	    check_time.tv_sec++;
	    check_time.tv_usec -= 1000000;
	}
	is_cache_check_scheduled = 1;
	SIGNAL(mseg_cond);
    }
}

static void
mseg_late_init(void)
{
    int res;
    mseg_cond = erts_cond_create();
    if (!mseg_cond)
	erl_exit(1, "Failed to create mseg_cond!\n");

    res = erts_thread_create(&mseg_cc_tid, mseg_cache_cleaner, NULL, 1);
    if (res < 0)
	erl_exit(1, "Failed to create mseg_cache_cleaner thread!\n");
}

#else  /* #ifdef USE_THREADS */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Single-threaded case                                                    *
\*                                                                         */

static ErlTimer cache_check_timer;

static ERTS_INLINE void
schedule_cache_check(void)
{
    if (!is_cache_check_scheduled && is_init_done) {
	cache_check_timer.active = 0;
	erl_set_timer(&cache_check_timer,
		      check_cache,
		      NULL,
		      NULL,
		      cache_check_interval);
	is_cache_check_scheduled = 1;
    }
}

static void
mseg_late_init(void)
{
}

#undef LOCK
#undef UNLOCK
#define LOCK(M)
#define UNLOCK(M)

#endif  /* #ifdef USE_THREADS */

static ERTS_INLINE void *
mseg_create(Uint size)
{
    void *seg;

    ASSERT(size % page_size == 0);

#if defined(ERTS_MSEG_FAKE_SEGMENTS)
    seg = erts_sys_alloc(ERTS_ALC_N_INVALID, NULL, size);
#elif HAVE_MMAP
    seg = (void *) mmap((void *) 0, (size_t) size,
			MMAP_PROT, MMAP_FLAGS, MMAP_FD, 0);
    if (seg == (void *) MAP_FAILED)
	seg = NULL;
#else
#error "Missing mseg_create() implementation"
#endif

    INC_CC(create);

    return seg;
}

static ERTS_INLINE void
mseg_destroy(void *seg, Uint size)
{
#if defined(ERTS_MSEG_FAKE_SEGMENTS)
    erts_sys_free(ERTS_ALC_N_INVALID, NULL, seg);
#elif HAVE_MMAP

#ifdef DEBUG
    int res =
#endif

	munmap((void *) seg, size);

    ASSERT(size % page_size == 0);
    ASSERT(res == 0);
#else
#error "Missing mseg_destroy() implementation"
#endif

    INC_CC(destroy);

}

#if HAVE_MSEG_RECREATE

static ERTS_INLINE void *
mseg_recreate(void *old_seg, Uint old_size, Uint new_size)
{
    void *new_seg;

    ASSERT(old_size % page_size == 0);
    ASSERT(new_size % page_size == 0);

#if defined(ERTS_MSEG_FAKE_SEGMENTS)
    new_seg = erts_sys_realloc(ERTS_ALC_N_INVALID, NULL, old_seg, new_size);
#elif HAVE_MREMAP
    new_seg = (void *) mremap((void *) old_seg,
			      (size_t) old_size,
			      (size_t) new_size,
			      MREMAP_MAYMOVE);
    if (new_seg == (void *) MAP_FAILED)
	new_seg = NULL;
#else
#error "Missing mseg_recreate() implementation"
#endif

    INC_CC(recreate);

    return new_seg;
}

#endif /* #if HAVE_MSEG_RECREATE */


static ERTS_INLINE cache_desc_t * 
alloc_cd(void)
{
    cache_desc_t *cd = free_cache_descs;
    if (cd)
	free_cache_descs = cd->next;
    return cd;
}

static ERTS_INLINE void
free_cd(cache_desc_t *cd)
{
    cd->next = free_cache_descs;
    free_cache_descs = cd;
}


static ERTS_INLINE void
link_cd(cache_desc_t *cd)
{
    if (cache)
	cache->prev = cd;
    cd->next = cache;
    cd->prev = NULL;
    cache = cd;

    if (!cache_end) {
	ASSERT(!cd->next);
	cache_end = cd;
    }

    cache_size++;
}

static ERTS_INLINE void
end_link_cd(cache_desc_t *cd)
{
    if (cache_end)
	cache_end->next = cd;
    cd->next = NULL;
    cd->prev = cache_end;
    cache_end = cd;

    if (!cache) {
	ASSERT(!cd->prev);
	cache = cd;
    }

    cache_size++;
}

static ERTS_INLINE void
unlink_cd(cache_desc_t *cd)
{

    if (cd->next)
	cd->next->prev = cd->prev;
    else
	cache_end = cd->prev;
    
    if (cd->prev)
	cd->prev->next = cd->next;
    else
	cache = cd->next;
    ASSERT(cache_size > 0);
    cache_size--;
}

static ERTS_INLINE void
check_cache_limits(void)
{
    cache_desc_t *cd;
    max_cached_seg_size = 0;
    min_cached_seg_size = ~((Uint) 0);
    for (cd = cache; cd; cd = cd->next) {
	if (cd->size < min_cached_seg_size)
	    min_cached_seg_size = cd->size;
	if (cd->size > max_cached_seg_size)
	    max_cached_seg_size = cd->size;
    }

}

static ERTS_INLINE void
adjust_cache_size(int force_check_limits)
{
    cache_desc_t *cd;
    int check_limits = force_check_limits;
    Sint max_cached = no_of_segments_watermark - no_of_segments;

    while (cache_size > max_cached && cache_size > 0) {
	ASSERT(cache_end);
	cd = cache_end;
	if (!check_limits &&
	    !(min_cached_seg_size < cd->size
	      && cd->size < max_cached_seg_size)) {
	    check_limits = 1;
	}
	mseg_destroy(cd->seg, cd->size);
	unlink_cd(cd);
	free_cd(cd);
    }

    if (check_limits)
	check_cache_limits();

}

static void
check_cache(void *unused)
{
    is_cache_check_scheduled = 0;

    if (no_of_segments_watermark > no_of_segments)
	no_of_segments_watermark--;
    adjust_cache_size(0);

    if (cache_size)
	schedule_cache_check();

    INC_CC(check_cache);
}

static void
mseg_clear_cache(void)
{
    no_of_segments_watermark = 0;

    adjust_cache_size(1);

    ASSERT(!cache);
    ASSERT(!cache_end);
    ASSERT(!cache_size);

    no_of_segments_watermark = no_of_segments;

    INC_CC(clear_cache);
}

static void *
mseg_alloc(Uint *size_p, const ErtsMsegOpt_t *opt)
{

    Uint max, min, diff_size, size;
    cache_desc_t *cd, *cand_cd;
    void *seg;

    INC_CC(alloc);

    size = PAGE_CEILING(*size_p);

    no_of_segments++;
    if (no_of_segments_watermark < no_of_segments)
	no_of_segments_watermark = no_of_segments;

#if CAN_PARTLY_DESTROY
    if (size < min_seg_size)	
	min_seg_size = size;
#endif

    if (!opt->cache) {
    create_seg:
	adjust_cache_size(0);
	seg = mseg_create(size);
	if (!seg) {
	    mseg_clear_cache();
	    seg = mseg_create(size);
	    if (!seg)
		size = 0;
	}

	*size_p = size;
	return seg;
    }

    if (size > max_cached_seg_size)
	goto create_seg;

    if (size < min_cached_seg_size) {

	diff_size = min_cached_seg_size - size;

	if (diff_size > abs_max_cache_bad_fit)
	    goto create_seg;

	if (100*PAGES(diff_size) > rel_max_cache_bad_fit*PAGES(size))
	    goto create_seg;

    }

    max = 0;
    min = ~((Uint) 0);
    cand_cd = NULL;

    for (cd = cache; cd; cd = cd->next) {
	if (cd->size >= size) {
	    if (!cand_cd) {
		cand_cd = cd;
		continue;
	    }
	    else if (cd->size < cand_cd->size) {
		if (max < cand_cd->size)
		    max = cand_cd->size;
		if (min > cand_cd->size)
		    min = cand_cd->size;
		cand_cd = cd;
		continue;
	    }
	}
	if (max < cd->size)
	    max = cd->size;
	if (min > cd->size)
	    min = cd->size;
    }

    min_cached_seg_size = min;
    max_cached_seg_size = max;

    if (!cand_cd)
	goto create_seg;

    diff_size = cand_cd->size - size;

    if (diff_size > abs_max_cache_bad_fit
	|| 100*PAGES(diff_size) > rel_max_cache_bad_fit*PAGES(size)) {
	if (max_cached_seg_size < cand_cd->size)	
	    max_cached_seg_size = cand_cd->size;
	if (min_cached_seg_size > cand_cd->size)
	    min_cached_seg_size = cand_cd->size;
	goto create_seg;
    }

    cache_hits++;

    size = cand_cd->size;
    seg = cand_cd->seg;

    unlink_cd(cand_cd);
    free_cd(cand_cd);

    *size_p = size;

    return seg;
}


static void
mseg_dealloc(void *seg, Uint size, const ErtsMsegOpt_t *opt)
{
    cache_desc_t *cd;

    no_of_segments--;


    if (!opt->cache || max_cache_size == 0) {
	mseg_destroy(seg, size);
    }
    else {
	int check_limits = 0;
	
	if (size < min_cached_seg_size)
	    min_cached_seg_size = size;
	if (size > max_cached_seg_size)
	    max_cached_seg_size = size;

	if (!free_cache_descs) {
	    cd = cache_end;
	    if (!(min_cached_seg_size < cd->size
		  && cd->size < max_cached_seg_size)) {
		check_limits = 1;
	    }
	    mseg_destroy(cd->seg, cd->size);
	    unlink_cd(cd);
	    free_cd(cd);
	}

	cd = alloc_cd();
	ASSERT(cd);
	cd->seg = seg;
	cd->size = size;
	link_cd(cd);

	/* ASSERT(no_of_segments_watermark >= no_of_segments + cache_size); */

	if (check_limits)
	    check_cache_limits();

	schedule_cache_check();

    }

    INC_CC(dealloc);
}

static void *
mseg_realloc(void *seg, Uint old_size, Uint *new_size_p,
	     const ErtsMsegOpt_t *opt)
{
    void *new_seg;
    Uint new_size;

    if (!seg || !old_size) {
	new_seg = mseg_alloc(new_size_p, opt);
	DEC_CC(alloc);
	return new_seg;
    }

    if (!(*new_size_p)) {
	mseg_dealloc(seg, old_size, opt);
	DEC_CC(dealloc);
	return NULL;
    }

    new_seg = seg;
    new_size = PAGE_CEILING(*new_size_p);

    if (new_size == old_size)
	;
    else if (new_size < old_size) {
	Uint shrink_sz = old_size - new_size;

#if CAN_PARTLY_DESTROY
	if (new_size < min_seg_size)	
	    min_seg_size = new_size;
#endif

	if (shrink_sz < opt->abs_shrink_th
	    && 100*PAGES(shrink_sz) < opt->rel_shrink_th*PAGES(old_size)) {
	    new_size = old_size;
	}
	else {

#if CAN_PARTLY_DESTROY

	    if (shrink_sz > min_seg_size
		&& free_cache_descs
		&& opt->cache) {
		cache_desc_t *cd;

		cd = alloc_cd();
		ASSERT(cd);
		cd->seg = ((char *) seg) + new_size;
		cd->size = shrink_sz;
		end_link_cd(cd);
		schedule_cache_check();
	    }
	    else
		mseg_destroy(((char *) seg) + new_size, shrink_sz);

#elif HAVE_MSEG_RECREATE

	    goto do_recreate;

#else

	    new_seg = mseg_alloc(&new_size, opt);
	    if (!new_seg)
		new_size = old_size;
	    else {
		sys_memcpy(((char *) new_seg),
			   ((char *) seg),
			   MIN(new_size, old_size));
		mseg_dealloc(seg, old_size, opt);
	    }

#endif

	}
    }
    else {

	if (!opt->preserv) {
	    mseg_dealloc(seg, old_size, opt);
	    new_seg = mseg_alloc(&new_size, opt);
	}
	else {
#if HAVE_MSEG_RECREATE
#if !CAN_PARTLY_DESTROY
	do_recreate:
#endif
	    new_seg = mseg_recreate((void *) seg, old_size, new_size);
	    if (!new_seg)
		new_size = old_size;
#else
	    new_seg = mseg_alloc(&new_size, opt);
	    if (!new_seg)
		new_size = old_size;
	    else {
		sys_memcpy(((char *) new_seg),
			   ((char *) seg),
			   MIN(new_size, old_size));
		mseg_dealloc(seg, old_size, opt);
	    }
#endif
	}
    }

    INC_CC(realloc);

    *new_size_p = new_size;

    return new_seg;
}

/* --- Info stuff ---------------------------------------------------------- */

static struct {
    Eterm version;

    Eterm options;
    Eterm amcbf;
    Eterm rmcbf;
    Eterm mcs;
    Eterm cci;

    Eterm status;
    Eterm cached_segments;
    Eterm cache_hits;
    Eterm segments;
    Eterm segments_watermark;


    Eterm calls;
    Eterm mseg_alloc;
    Eterm mseg_dealloc;
    Eterm mseg_realloc;
    Eterm mseg_create;
    Eterm mseg_destroy;
#if HAVE_MSEG_RECREATE
    Eterm mseg_recreate;
#endif
    Eterm mseg_clear_cache;
    Eterm mseg_check_cache;

#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
#ifdef DEBUG
    Eterm *atom;
    for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	*atom = THE_NON_VALUE;
    }
#endif

    AM_INIT(version);

    AM_INIT(options);
    AM_INIT(amcbf);
    AM_INIT(rmcbf);
    AM_INIT(mcs);
    AM_INIT(cci);

    AM_INIT(status);
    AM_INIT(cached_segments);
    AM_INIT(cache_hits);
    AM_INIT(segments);
    AM_INIT(segments_watermark);

    AM_INIT(calls);
    AM_INIT(mseg_alloc);
    AM_INIT(mseg_dealloc);
    AM_INIT(mseg_realloc);
    AM_INIT(mseg_create);
    AM_INIT(mseg_destroy);
#if HAVE_MSEG_RECREATE
    AM_INIT(mseg_recreate);
#endif
    AM_INIT(mseg_clear_cache);
    AM_INIT(mseg_check_cache);

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	ASSERT(*atom != THE_NON_VALUE);
    }
#endif

    atoms_initialized = 1;
}


#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple
#define bld_string	erts_bld_string
#define bld_2tup_list	erts_bld_2tup_list

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static ERTS_INLINE void
add_3tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2, Eterm el3)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 3, el1, el2, el3), *lp);
}

static Eterm
info_options(char *prefix,
	     CIO *ciop,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (ciop) {
	CIO to = *ciop;
	erl_printf(to, "%samcbf: %lu\n", prefix, abs_max_cache_bad_fit);
	erl_printf(to, "%srmcbf: %lu\n", prefix, rel_max_cache_bad_fit);
	erl_printf(to, "%smcs: %lu\n", prefix, max_cache_size);
	erl_printf(to, "%scci: %lu\n", prefix, cache_check_interval);
    }

    if (hpp || szp) {

	if (!atoms_initialized)
	    init_atoms();

	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.cci,
		 bld_uint(hpp, szp, cache_check_interval));
	add_2tup(hpp, szp, &res,
		 am.mcs,
		 bld_uint(hpp, szp, max_cache_size));
	add_2tup(hpp, szp, &res,
		 am.rmcbf,
		 bld_uint(hpp, szp, rel_max_cache_bad_fit));
	add_2tup(hpp, szp, &res,
		 am.amcbf,
		 bld_uint(hpp, szp, abs_max_cache_bad_fit));

    }

    return res;
}

static Eterm
info_calls(CIO *ciop, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (ciop) {

#define PRINT_CC(TO, CC)						\
    if (calls.CC.giga_no == 0)						\
	erl_printf(TO, "mseg_%s calls: %lu\n", #CC, calls.CC.no);	\
    else								\
	erl_printf(TO, "mseg_%s calls: %lu%09lu\n", #CC,		\
		   calls.CC.giga_no, calls.CC.no)

	CIO to = *ciop;

	PRINT_CC(to, alloc);
	PRINT_CC(to, dealloc);
	PRINT_CC(to, realloc);
	PRINT_CC(to, create);
	PRINT_CC(to, destroy);
#if HAVE_MSEG_RECREATE
	PRINT_CC(to, recreate);
#endif
	PRINT_CC(to, clear_cache);
	PRINT_CC(to, check_cache);

#undef PRINT_CC

    }

    if (hpp || szp) {

	res = NIL;

	add_3tup(hpp, szp, &res,
		 am.mseg_check_cache,
		 bld_uint(hpp, szp, calls.check_cache.giga_no),
		 bld_uint(hpp, szp, calls.check_cache.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_clear_cache,
		 bld_uint(hpp, szp, calls.clear_cache.giga_no),
		 bld_uint(hpp, szp, calls.clear_cache.no));

#if HAVE_MSEG_RECREATE
	add_3tup(hpp, szp, &res,
		 am.mseg_recreate,
		 bld_uint(hpp, szp, calls.recreate.giga_no),
		 bld_uint(hpp, szp, calls.recreate.no));
#endif
	add_3tup(hpp, szp, &res,
		 am.mseg_destroy,
		 bld_uint(hpp, szp, calls.destroy.giga_no),
		 bld_uint(hpp, szp, calls.destroy.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_create,
		 bld_uint(hpp, szp, calls.create.giga_no),
		 bld_uint(hpp, szp, calls.create.no));


	add_3tup(hpp, szp, &res,
		 am.mseg_realloc,
		 bld_uint(hpp, szp, calls.realloc.giga_no),
		 bld_uint(hpp, szp, calls.realloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_dealloc,
		 bld_uint(hpp, szp, calls.dealloc.giga_no),
		 bld_uint(hpp, szp, calls.dealloc.no));
	add_3tup(hpp, szp, &res,
		 am.mseg_alloc,
		 bld_uint(hpp, szp, calls.alloc.giga_no),
		 bld_uint(hpp, szp, calls.alloc.no));
    }

    return res;
}

static Eterm
info_status(CIO *ciop, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    
    if (ciop) {
	CIO to = *ciop;

	erl_printf(to, "cached_segments: %lu\n", cache_size);
	erl_printf(to, "cache_hits: %lu\n", cache_hits);
	erl_printf(to, "segments: %lu\n", no_of_segments);
	erl_printf(to, "segments_watermark: %lu\n", no_of_segments_watermark);
    }

    if (hpp || szp) {
	res = NIL;
	add_2tup(hpp, szp, &res,
		 am.segments_watermark,
		 bld_uint(hpp, szp, no_of_segments_watermark));
	add_2tup(hpp, szp, &res,
		 am.segments,
		 bld_uint(hpp, szp, no_of_segments));
	add_2tup(hpp, szp, &res,
		 am.cache_hits,
		 bld_uint(hpp, szp, cache_hits));
	add_2tup(hpp, szp, &res,
		 am.cached_segments,
		 bld_uint(hpp, szp, cache_size));

    }

    return res;
}

static Eterm
info_version(CIO *ciop, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (ciop) {
	erl_printf(*ciop, "version: %s\n", ERTS_MSEG_VSN_STR);
    }

    if (hpp || szp) {
	res = bld_string(hpp, szp, ERTS_MSEG_VSN_STR);
    }

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Exported functions                                                        *
\*                                                                           */

Eterm
erts_mseg_info_options(CIO *ciop, Uint **hpp, Uint *szp)
{
    Eterm res;

    LOCK(mseg_mutex);

    res = info_options("option ", ciop, hpp, szp);

    UNLOCK(mseg_mutex);

    return res;
}

Eterm
erts_mseg_info(CIO *ciop, Uint **hpp, Uint *szp)
{
    Eterm res = THE_NON_VALUE;
    Eterm atoms[4];
    Eterm values[4];

    LOCK(mseg_mutex);

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    init_atoms();

	atoms[0] = am.version;
	atoms[1] = am.options;
	atoms[2] = am.status;
	atoms[3] = am.calls;
    }

    values[0] = info_version(ciop, hpp, szp);
    values[1] = info_options("option ", ciop, hpp, szp);
    values[2] = info_status(ciop, hpp, szp);
    values[3] = info_calls(ciop, hpp, szp);

    if (hpp || szp)
	res = bld_2tup_list(hpp, szp, 4, atoms, values);

    UNLOCK(mseg_mutex);

    return res;
}

void *
erts_mseg_alloc_opt(Uint *size_p, const ErtsMsegOpt_t *opt)
{
    void *seg;
    LOCK(mseg_mutex);
    seg = mseg_alloc(size_p, opt);
    UNLOCK(mseg_mutex);
    return seg;
}

void *
erts_mseg_alloc(Uint *size_p)
{
    return erts_mseg_alloc_opt(size_p, &default_opt);
}

void
erts_mseg_dealloc_opt(void *seg, Uint size, const ErtsMsegOpt_t *opt)
{
    LOCK(mseg_mutex);
    mseg_dealloc(seg, size, opt);
    UNLOCK(mseg_mutex);
}

void
erts_mseg_dealloc(void *seg, Uint size)
{
    erts_mseg_dealloc_opt(seg, size, &default_opt);
}

void *
erts_mseg_realloc_opt(void *seg, Uint old_size, Uint *new_size_p,
		      const ErtsMsegOpt_t *opt)
{
    void *new_seg;
    LOCK(mseg_mutex);
    new_seg = mseg_realloc(seg, old_size, new_size_p, opt);
    UNLOCK(mseg_mutex);
    return new_seg;
}

void *
erts_mseg_realloc(void *seg, Uint old_size, Uint *new_size_p)
{
    return erts_mseg_realloc_opt(seg, old_size, new_size_p, &default_opt);
}

void
erts_mseg_clear_cache(void)
{
    LOCK(mseg_mutex);
    mseg_clear_cache();
    UNLOCK(mseg_mutex);
}

Uint
erts_mseg_no(void)
{
    Uint n;
    LOCK(mseg_mutex);
    n = no_of_segments;
    UNLOCK(mseg_mutex);
    return n;
}

Uint
erts_mseg_unit_size(void)
{
    return page_size;
}

void
erts_mseg_init(ErtsMsegInit_t *init)
{
    int i;

    atoms_initialized = 0;
    is_init_done = 0;

    /* Options ... */

    abs_max_cache_bad_fit	= init->amcbf;
    rel_max_cache_bad_fit	= init->rmcbf;
    max_cache_size		= init->mcs;
    cache_check_interval	= init->cci;

    /* */

#ifdef USE_THREADS
    thread_safe_init();
#endif

#if HAVE_MMAP && !defined(MAP_ANON)
    mmap_fd = open("/dev/zero", O_RDWR);
    if (mmap_fd < 0)
	erl_exit(-1, "erts_mseg: unable to open /dev/zero\n");
#endif

    page_size = GET_PAGE_SIZE;

    page_shift = 1;
    while ((page_size >> page_shift) != 1) {
	if ((page_size & (1 << (page_shift - 1))) != 0)
	    erl_exit(1, "erts_mseg: Unexpected page_size %lu\n", page_size);
	page_shift++;
    }

    sys_memzero((void *) &calls, sizeof(calls));

#if CAN_PARTLY_DESTROY
    min_seg_size = ~((Uint) 0);
#endif

    cache = NULL;
    cache_end = NULL;
    cache_hits = 0;
    max_cached_seg_size = 0;
    min_cached_seg_size = ~((Uint) 0);
    cache_size = 0;

    if (max_cache_size > MAX_CACHE_SIZE)
	max_cache_size = MAX_CACHE_SIZE;

    if (max_cache_size > 0) {
	for (i = 0; i < max_cache_size - 1; i++)
	    cache_descs[i].next = &cache_descs[i + 1];
	cache_descs[max_cache_size - 1].next = NULL;
	free_cache_descs = &cache_descs[0];
    }
    else
	free_cache_descs = NULL;

    no_of_segments = 0;
    no_of_segments_watermark = 0;
}


/*
 * erts_mseg_late_init() have to be called after all allocators,
 * threads and timers have been initialized.
 */
void
erts_mseg_late_init(void)
{
    LOCK(mseg_mutex);
    mseg_late_init();
    is_init_done = 1;
    if (cache_size)
	schedule_cache_check();
    UNLOCK(mseg_mutex);
}

#endif /* #if HAVE_ERTS_MSEG */
