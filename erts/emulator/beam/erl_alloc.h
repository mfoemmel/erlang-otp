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

#ifndef ERL_ALLOC_H__
#define ERL_ALLOC_H__

#include "erl_alloc_types.h"
#include "erl_alloc_util.h"
#ifdef ERTS_ALC_THR_SPEC_ALLOCS
#ifdef USE_THREADS
#include "erl_threads.h"
#endif
#endif

#ifdef DEBUG
#  undef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 0
#endif

#ifndef ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_WANT_INLINE 1
#endif

#if ERTS_CAN_INLINE && ERTS_ALC_WANT_INLINE
#  define ERTS_ALC_DO_INLINE 1
#  define ERTS_ALC_INLINE static ERTS_INLINE
#else
#  define ERTS_ALC_DO_INLINE 0
#  define ERTS_ALC_INLINE
#endif

#define ERTS_FIX_CORE_ALLOCATOR ERTS_ALC_A_LONG_LIVED
extern ErtsAlcType_t erts_fix_core_allocator_ix;

typedef struct {
    Uint total;
    Uint used;
} ErtsFixInfo;

void erts_sys_alloc_init(void);
void *erts_sys_alloc(ErtsAlcType_t, void *, Uint);
void *erts_sys_realloc(ErtsAlcType_t, void *, void *, Uint);
void erts_sys_free(ErtsAlcType_t, void *, void *);


void erts_init_fix_alloc(Uint, void *(*)(Uint));
Uint erts_get_fix_size(ErtsAlcType_t);
void erts_set_fix_size(ErtsAlcType_t, Uint);
void erts_fix_info(ErtsAlcType_t, ErtsFixInfo *);
void *erts_fix_alloc(ErtsAlcType_t, void *, Uint);
void *erts_fix_realloc(ErtsAlcType_t, void *, void*, Uint);
void erts_fix_free(ErtsAlcType_t, void *, void*);


Eterm erts_memory(int *, void *, void *, Eterm);
Eterm erts_allocated_areas(int *, void *, void *);

void erts_allocator_info(int, void *);
Eterm erts_allocator_info_term(void *proc, Eterm which_alloc);
Eterm erts_allocator_options(void *proc);

void erts_alloc_init(int *argc, char **argv);

#if defined(GET_ERTS_ALC_TEST) || defined(ERTS_ALC_INTERNAL__)
/* Only for testing */
unsigned long erts_alc_test(unsigned long,
			    unsigned long,
			    unsigned long,
			    unsigned long);
#endif

#define ERTS_ALC_O_ALLOC		0
#define ERTS_ALC_O_REALLOC		1
#define ERTS_ALC_O_FREE			2

#define ERTS_ALC_E_NOTSUP		0
#define ERTS_ALC_E_NOMEM		1
#define ERTS_ALC_E_NOALLCTR		2

#define ERTS_ALC_MIN_LONG_LIVED_TIME	(10*60*1000)

typedef struct {
    int alloc_util;
    int enabled;
    void *extra;
} ErtsAllocatorInfo_t;

typedef struct {
    void *	(*alloc)	(ErtsAlcType_t, void *, Uint);
    void *	(*realloc)	(ErtsAlcType_t, void *, void *, Uint);
    void	(*free)		(ErtsAlcType_t, void *, void *);
    void *extra;
} ErtsAllocatorFunctions_t;

extern ErtsAllocatorFunctions_t erts_allctrs[ERTS_ALC_A_MAX+1];
extern ErtsAllocatorInfo_t erts_allctrs_info[ERTS_ALC_A_MAX+1];

#ifdef ERTS_ALC_THR_SPEC_ALLOCS
#ifdef USE_THREADS

typedef struct {
    erts_tsd_key_t key;
    void * (*start)(void);
} ErtsAllocatorThrSpec_t;

extern ErtsAllocatorThrSpec_t erts_allctr_thr_spec[ERTS_ALC_A_MAX+1];

#endif
#endif

void erts_alloc_enomem(ErtsAlcType_t,Uint)		__noreturn;
void erts_alloc_n_enomem(ErtsAlcType_t,Uint)		__noreturn;
void erts_realloc_enomem(ErtsAlcType_t,void*,Uint)	__noreturn;
void erts_realloc_n_enomem(ErtsAlcType_t,void*,Uint)	__noreturn;
void erts_alc_fatal_error(int,int,ErtsAlcType_t,...)	__noreturn;

/* --- DO *NOT* USE THESE DEPRECATED FUNCTIONS ---    Instead use:       */
void *safe_alloc(Uint)               __deprecated; /* erts_alloc()       */
void *safe_realloc(void *, Uint)     __deprecated; /* erts_realloc()     */
void  sys_free(void *)               __deprecated; /* erts_free()        */
void *sys_alloc(Uint )               __deprecated; /* erts_alloc_fnf()   */
void *sys_realloc(void *, Uint)      __deprecated; /* erts_realloc_fnf() */

/*
 * erts_alloc[_fnf](), erts_realloc[_fnf](), erts_free() works as
 * malloc(), realloc(), and free() with the following exceptions:
 *
 * * They take an extra type argument as first argument which is
 *   the memory type to operate on. Memory types are generated
 *   (as ERTS_ALC_T_[SOMETHING] defines) from the erl_alloc.types
 *   configuration file.
 * * The erts_alloc() and erts_realloc() functions terminate the
 *   emulator if memory cannot be obtained. The _fnf (Failure Not
 *   Fatal) suffixed versions return NULL if memory cannot be
 *   obtained.
 * * They may be static functions so function pointers to "the same"
 *   function may differ.
 *
 * IMPORTANT: Memory allocated or reallocated as type X, can only
 *            be reallocated or deallocated as type X.
 */

#if !ERTS_ALC_DO_INLINE

void *erts_alloc(ErtsAlcType_t type, Uint size);
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size);
void erts_free(ErtsAlcType_t type, void *ptr);
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size);
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size);

#endif /* #if !ERTS_ALC_DO_INLINE */

#if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__)

ERTS_ALC_INLINE
void *erts_alloc(ErtsAlcType_t type, Uint size)
{
    void *res;
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	size);
    if (!res)
	erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
    return res;
}

ERTS_ALC_INLINE
void *erts_realloc(ErtsAlcType_t type, void *ptr, Uint size)
{
    void *res;
    res = (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
    if (!res)
	erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
    return res;
}

ERTS_ALC_INLINE
void erts_free(ErtsAlcType_t type, void *ptr)
{
    (*erts_allctrs[ERTS_ALC_T2A(type)].free)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr);
}


ERTS_ALC_INLINE
void *erts_alloc_fnf(ErtsAlcType_t type, Uint size)
{
    return (*erts_allctrs[ERTS_ALC_T2A(type)].alloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	size);
}


ERTS_ALC_INLINE
void *erts_realloc_fnf(ErtsAlcType_t type, void *ptr, Uint size)
{
    return (*erts_allctrs[ERTS_ALC_T2A(type)].realloc)(
	ERTS_ALC_T2N(type),
	erts_allctrs[ERTS_ALC_T2A(type)].extra,
	ptr,
	size);
}

#endif /* #if ERTS_ALC_DO_INLINE || defined(ERTS_ALC_INTERNAL__) */

#define ERTS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      (void) 0, (void) 0, (void) 0)

#define ERTS_SMP_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
static erts_smp_spinlock_t NAME##_lck;					\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      erts_smp_spinlock_init(&NAME##_lck, #NAME "_alloc_lock"),\
		      erts_smp_spin_lock(&NAME##_lck),			\
		      erts_smp_spin_unlock(&NAME##_lck))

#ifdef ERTS_SMP

#define ERTS_TS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
ERTS_SMP_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)

#else /* !ERTS_SMP */

#define ERTS_TS_QUALLOC_IMPL(NAME, TYPE, PASZ, ALCT)			\
static erts_mtx_t NAME##_lck;						\
ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT,				\
		      erts_mtx_init(NAME##_lck, #NAME "_alloc_lock"),	\
		      erts_mtx_lock(&NAME##_lck),			\
		      erts_mtx_unlock(&NAME##_lck))


#endif

#define ERTS_PALLOC_IMPL(NAME, TYPE, PASZ)				\
ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, (void) 0, (void) 0, (void) 0)

#define ERTS_SMP_PALLOC_IMPL(NAME, TYPE, PASZ)				\
static erts_smp_spinlock_t NAME##_lck;					\
ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ,					\
		    erts_smp_spinlock_init(&NAME##_lck, #NAME "_alloc_lock"),\
		    erts_smp_spin_lock(&NAME##_lck),			\
		    erts_smp_spin_unlock(&NAME##_lck))

#ifdef ERTS_SMP

#define ERTS_TS_PALLOC_IMPL(NAME, TYPE, PASZ)				\
ERTS_SMP_PALLOC_IMPL(NAME, TYPE, PASZ)

#else /* !ERTS_SMP */

#define ERTS_TS_PALLOC_IMPL(NAME, TYPE, PASZ)				\
static erts_mtx_t NAME##_lck;						\
ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ,					\
		    erts_mtx_init(NAME##_lck, #NAME "_alloc_lock"),	\
		    erts_mtx_lock(&NAME##_lck),				\
		    erts_mtx_unlock(&NAME##_lck))

#endif

#define ERTS_QUICK_ALLOC_IMPL(NAME, TYPE, PASZ, ALCT, ILCK, LCK, ULCK)	\
ERTS_PRE_ALLOC_IMPL(NAME##_pre, TYPE, PASZ, ILCK, LCK, ULCK)		\
static void								\
init_##NAME##_alloc(void)						\
{									\
    init_##NAME##_pre_alloc();						\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res = NAME##_pre_alloc();					\
    if (!res)								\
	res = erts_alloc(ALCT, sizeof(TYPE));				\
    return res;								\
}									\
static ERTS_INLINE void							\
NAME##_free(TYPE *p)							\
{									\
    if (!NAME##_pre_free(p))						\
	erts_free(ALCT, (void *) p);					\
}

#define ERTS_PRE_ALLOC_IMPL(NAME, TYPE, PASZ, ILCK, LCK, ULCK)		\
union erts_qa_##NAME##__ {						\
    TYPE type;								\
    union erts_qa_##NAME##__ *next;					\
};									\
static union erts_qa_##NAME##__ qa_prealcd_##NAME[(PASZ)];		\
static union erts_qa_##NAME##__ *qa_freelist_##NAME;			\
static void								\
init_##NAME##_alloc(void)						\
{									\
    int i;								\
    qa_freelist_##NAME = &qa_prealcd_##NAME[0];				\
    for (i = 1; i < (PASZ); i++)					\
	qa_prealcd_##NAME[i-1].next = &qa_prealcd_##NAME[i];		\
    qa_prealcd_##NAME[(PASZ)-1].next = NULL;				\
    ILCK;								\
}									\
static ERTS_INLINE TYPE *						\
NAME##_alloc(void)							\
{									\
    TYPE *res;								\
    LCK;								\
    if (!qa_freelist_##NAME)						\
	res = NULL;							\
    else {								\
	res = &qa_freelist_##NAME->type;				\
	qa_freelist_##NAME = qa_freelist_##NAME->next;			\
    }									\
    ULCK;								\
    return res;								\
}									\
static ERTS_INLINE int							\
NAME##_free(TYPE *p)							\
{									\
    union erts_qa_##NAME##__ * up;					\
    up = ((union erts_qa_##NAME##__ *)					\
	  (((char *) p)							\
	   - ((char *) &((union erts_qa_##NAME##__ *) 0)->type)));	\
    if (up < &qa_prealcd_##NAME[0] || up > &qa_prealcd_##NAME[(PASZ)-1])\
	return 0;							\
    else {								\
	LCK;								\
	up->next = qa_freelist_##NAME;					\
	qa_freelist_##NAME = up;					\
	ULCK;								\
	return 1;							\
    }									\
}

#ifdef DEBUG
#define ERTS_ALC_DBG_BLK_SZ(PTR) (*(((Uint *) (PTR)) - 2))
#endif /* #ifdef DEBUG */

#undef ERTS_ALC_INLINE
#undef ERTS_ALC_ATTRIBUTES

#endif /* #ifndef ERL_ALLOC_H__ */


