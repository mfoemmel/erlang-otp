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

#ifndef ERL_ALLOC_UTIL__
#define ERL_ALLOC_UTIL__

#define ERTS_ALCU_VSN_STR "2.1"

typedef struct Allctr_t_ Allctr_t;

typedef struct {
    Uint ycs;
    Uint mmc;
} AlcUInit_t;

typedef struct {
    char *name_prefix;
    int ts;
    Uint sbct;
    Uint asbcst;
    Uint rsbcst;
    Uint rsbcmt;
    Uint mmbcs;
    Uint mmsbc;
    Uint mmmbc;
    Uint lmbcs;
    Uint smbcs;
    Uint mbcgs;
    int smn;
} AllctrInit_t;

#ifndef SMALL_MEMORY

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    1024*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    1024      		/* (amount) mmc:    max mseg carriers            */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    1,			/* (bool)   ts:     thread safe                  */\
    512*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    1024*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    10,			/* (amount) mmmbc:  max mseg mbcs                */\
    5*1024*1024,	/* (bytes)  lmbcs:  largest mbc size             */\
    1024*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10,			/* (amount) mbcgs:  mbc growth stages            */\
    -1			/* sys mutex no                                  */\
}

#else /* if SMALL_MEMORY */

#define ERTS_DEFAULT_ALCU_INIT {                                           \
    128*1024,		/* (bytes)  ycs:    sys_alloc carrier size       */\
    1024      		/* (amount) mmc:    max mseg carriers            */\
}

#define ERTS_DEFAULT_ALLCTR_INIT {                                         \
    NULL,                                                                  \
    1,			/* (bool)   ts:     thread safe                  */\
    64*1024,		/* (bytes)  sbct:   sbc threshold                */\
    2*1024*2024,	/* (amount) asbcst: abs sbc shrink threshold     */\
    20,			/* (%)      rsbcst: rel sbc shrink threshold     */\
    80,			/* (%)      rsbcmt: rel sbc move threshold       */\
    128*1024,		/* (bytes)  mmbcs:  main multiblock carrier size */\
    256,		/* (amount) mmsbc:  max mseg sbcs                */\
    10,			/* (amount) mmmbc:  max mseg mbcs                */\
    1024*1024,		/* (bytes)  lmbcs:  largest mbc size             */\
    128*1024,		/* (bytes)  smbcs:  smallest mbc size            */\
    10			/* (amount) mbcgs:  mbc growth stages            */\
}

#endif

#define ERTS_ALCU_SYS_MUTEXES (ERTS_ALC_A_MAX - ERTS_ALC_A_MIN + 1)

void *	erts_alcu_alloc(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free(ErtsAlcType_t, void *, void *);
void *	erts_alcu_alloc_ts(ErtsAlcType_t, void *, Uint);
void *	erts_alcu_realloc_ts(ErtsAlcType_t, void *, void *, Uint);
void	erts_alcu_free_ts(ErtsAlcType_t, void *, void *);
Eterm	erts_alcu_au_info_options(CIO *, Uint **, Uint *);
Eterm	erts_alcu_info_options(Allctr_t *, CIO *, Uint **, Uint *);
Eterm	erts_alcu_info(Allctr_t *, int, CIO *, Uint **, Uint *);
void	erts_alcu_init(AlcUInit_t *);

#endif

#if defined(GET_ERL_ALLOC_UTIL_IMPL) && !defined(ERL_ALLOC_UTIL_IMPL__)
#define ERL_ALLOC_UTIL_IMPL__

#ifdef USE_THREADS
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#endif

#include "erl_mseg.h"

#undef ERTS_ALLOC_UTIL_HARD_DEBUG
#ifdef DEBUG
#  if 0
#    define ERTS_ALLOC_UTIL_HARD_DEBUG
#  endif
#endif

#undef MIN
#undef MAX
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define FLOOR(X, I) (((X)/(I))*(I))
#define CEILING(X, I)  ((((X) - 1)/(I) + 1)*(I))

#undef  WORD_MASK
#define INV_WORD_MASK	((Uint) (sizeof(Uint) - 1))
#define WORD_MASK	(~INV_WORD_MASK)
#define WORD_FLOOR(X)	((X) & WORD_MASK)
#define WORD_CEILING(X)	WORD_FLOOR((X) + INV_WORD_MASK)

#undef  UNIT_MASK
#define INV_UNIT_MASK	((Uint) (sizeof(Unit_t) - 1))
#define UNIT_MASK	(~INV_UNIT_MASK)
#define UNIT_FLOOR(X)	((X) & UNIT_MASK)
#define UNIT_CEILING(X)	UNIT_FLOOR((X) + INV_UNIT_MASK)


#define SZ_MASK			(~((Uint) 0) << 3)
#define FLG_MASK		(~(SZ_MASK))


#define BLK_SZ(B) \
  (*((Block_t *) (B)) & SZ_MASK)

#define CARRIER_SZ(C) \
  ((C)->chdr & SZ_MASK)

typedef union {char c[8]; long l; double d;} Unit_t;

typedef struct Carrier_t_ Carrier_t;
struct Carrier_t_ {
    Uint chdr;
    Carrier_t *next;
    Carrier_t *prev;
};

typedef struct {
    Carrier_t *first;
    Carrier_t *last;
} CarrierList_t;

typedef Uint Block_t;
typedef Uint FreeBlkFtr_t;

typedef struct {
    Uint giga_no;
    Uint no;
} CallCounter_t;

typedef struct {
    Uint		no;
    Uint		size;
} StatValues_t;

typedef struct {
    StatValues_t	curr_mseg;
    StatValues_t	curr_sys_alloc;
    StatValues_t	max;
    StatValues_t	max_ever;
    struct {
	StatValues_t	curr;
	StatValues_t	max;
	StatValues_t	max_ever;
    } blocks;
} CarriersStats_t;

struct Allctr_t_ {

    /* Allocator name prefix */
    char *		name_prefix;

    /* Alloc, realloc and free names as atoms */
    struct {
	Eterm		alloc;
	Eterm		realloc;
	Eterm		free;
    } name;

    /* Version string */
    char *		vsn_str;

    /* Options */
    Uint		sbc_threshold;
    Uint		sbc_move_threshold;
    Uint		main_carrier_size;
    Uint		max_mseg_sbcs;
    Uint		max_mseg_mbcs;
    Uint		largest_mbc_size;
    Uint		smallest_mbc_size;
    Uint		mbc_growth_stages;
#if HAVE_ERTS_MSEG
    ErtsMsegOpt_t	mseg_opt;
#endif

    /* */
    Uint		mbc_header_size;
    Uint		min_mbc_size;
    Uint		min_mbc_first_free_size;
    Uint		min_block_size;

    /* Carriers */
    CarrierList_t	mbc_list;
    CarrierList_t	sbc_list;

    /* Main carrier (if there is one) */
    Carrier_t *		main_carrier;

    /* Callback functions (first 4 are mandatory) */
    Block_t *		(*get_free_block)	(Allctr_t *, Uint);
    void		(*link_free_block)	(Allctr_t *, Block_t *);
    void		(*unlink_free_block)	(Allctr_t *, Block_t *);
    Eterm		(*info_options)		(Allctr_t *, char *, CIO *,
						 Uint **, Uint *);

    Uint		(*get_next_mbc_size)	(Allctr_t *);
    void		(*creating_mbc)		(Allctr_t *, Carrier_t *);
    void		(*destroying_mbc)	(Allctr_t *, Carrier_t *);

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    void		(*check_block)		(Allctr_t *, Block_t *,  int);
    void		(*check_mbc)		(Allctr_t *, Carrier_t *);
#endif

#ifdef USE_THREADS
    /* Mutex for this allocator */
    erts_mutex_t	mutex;
    int			sys_mutex_no;
    int			thread_safe;
    struct {
	Allctr_t	*prev;
	Allctr_t	*next;
    } ts_list;
#endif

    int			stopped;

    /* Some statistics ... */
    struct {
	CallCounter_t	this_alloc;
	CallCounter_t	this_free;
	CallCounter_t	this_realloc;
	CallCounter_t	mseg_alloc;
	CallCounter_t	mseg_dealloc;
	CallCounter_t	mseg_realloc;
	CallCounter_t	sys_alloc;
	CallCounter_t	sys_free;
	CallCounter_t	sys_realloc;
    } calls;

    CarriersStats_t	sbcs;
    CarriersStats_t	mbcs;

};

int	erts_alcu_start(Allctr_t *, AllctrInit_t *);
void	erts_alcu_stop(Allctr_t *);

unsigned long	erts_alcu_test(unsigned long, unsigned long, unsigned long);



#endif /* #if defined(GET_ERL_ALLOC_UTIL_IMPL)
	      && !defined(ERL_ALLOC_UTIL_IMPL__) */

