
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
 * Description:	Management of memory allocators.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define ERTS_ALC_INTERNAL__
#include "sys.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#include "global.h"
#include "erl_db.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_instrument.h"
#include "erl_mseg.h"
#ifdef ELIB_ALLOC_IS_CLIB
#include "erl_version.h"
#endif

#define GET_ERL_GF_ALLOC_IMPL
#include "erl_goodfit_alloc.h"
#define GET_ERL_BF_ALLOC_IMPL
#include "erl_bestfit_alloc.h"
#define GET_ERL_AF_ALLOC_IMPL
#include "erl_afit_alloc.h"

#if defined(SMALL_MEMORY) || defined(PURIFY)
#define AU_ALLOC_DEFAULT_ENABLE(X)	0
#else
#define AU_ALLOC_DEFAULT_ENABLE(X)	(X)
#endif

#ifdef DEBUG
static Uint install_debug_functions(void);
#endif
extern void elib_ensure_initialized(void);

ErtsAllocatorFunctions_t erts_allctrs[ERTS_ALC_A_MAX+1];
ErtsAllocatorInfo_t erts_allctrs_info[ERTS_ALC_A_MAX+1];

typedef union {
    GFAllctr_t gfa;
    BFAllctr_t bfa;
    AFAllctr_t afa;
} allocator_state;

static allocator_state sl_alloc_state;
static allocator_state std_alloc_state;
static allocator_state ll_alloc_state;
static allocator_state temp_alloc_state;
static allocator_state eheap_alloc_state;
static allocator_state binary_alloc_state;
static allocator_state ets_alloc_state;


#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE
static void *(*fix_core_allocator)(ErtsAlcType_t, void *, Uint);
static void *fix_core_extra;
static void *fix_core_alloc(Uint size)
{
    return (*fix_core_allocator)(ERTS_ALC_T_UNDEF, fix_core_extra, size);
}
#endif

enum allctr_type {
    GOODFIT,
    BESTFIT,
    AFIT
};

struct au_init {
    int enable;
    enum allctr_type	atype;
    struct {
	AllctrInit_t	util;
	GFAllctrInit_t	gf;
	BFAllctrInit_t	bf;
	AFAllctrInit_t	af;
    } init;
};

#define DEFAULT_ALLCTR_INIT {		\
    ERTS_DEFAULT_ALLCTR_INIT,		\
    ERTS_DEFAULT_GF_ALLCTR_INIT,	\
    ERTS_DEFAULT_BF_ALLCTR_INIT,	\
    ERTS_DEFAULT_AF_ALLCTR_INIT		\
}

typedef struct {
#if HAVE_ERTS_MSEG
    ErtsMsegInit_t mseg;
#endif
    int trim_threshold;
    int top_pad;
    AlcUInit_t alloc_util;
    struct {
	int stat;
	int map;
	char *mtrace;
    } instr;
    struct au_init sl_alloc;
    struct au_init std_alloc;
    struct au_init ll_alloc;
    struct au_init temp_alloc;
    struct au_init eheap_alloc;
    struct au_init binary_alloc;
    struct au_init ets_alloc;
} init_t;

#define SET_DEFAULT_ALLOC_OPTS(IP)					\
do {									\
    struct au_init aui__ = {0, GOODFIT, DEFAULT_ALLCTR_INIT};		\
    sys_memcpy((void *) (IP), (void *) &aui__, sizeof(struct au_init));	\
} while (0)

static void
set_default_sl_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(0);
    ip->atype			= GOODFIT;
    ip->init.util.name_prefix	= "sl_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_SHORT_LIVED;
    ip->init.util.rsbcst	= 80;
    ip->init.util.smn		= ERTS_MUTEX_SYS_SL_ALLOC;
}

static void
set_default_std_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(0);
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "def_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_STANDARD;
    ip->init.util.smn		= ERTS_MUTEX_SYS_STD_ALLOC;
}

static void
set_default_ll_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->atype			= BESTFIT;
    ip->init.bf.ao		= 1;
    ip->init.util.mmsbc		= 0;
    ip->init.util.mmmbc		= 0;
    ip->init.util.sbct		= ~((Uint) 0);
    ip->init.util.name_prefix	= "ll_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 2*1024*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 1*1024*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_LONG_LIVED;
    ip->init.util.asbcst	= 0;
    ip->init.util.rsbcst	= 0;
    ip->init.util.rsbcmt	= 0;
    ip->init.util.smn		= ERTS_MUTEX_SYS_LL_ALLOC;
}

static void
set_default_temp_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->atype			= AFIT;
    ip->init.util.name_prefix	= "temp_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 64*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_TEMPORARY;
    ip->init.util.rsbcst	= 90;
    ip->init.util.smn		= ERTS_MUTEX_SYS_TEMP_ALLOC;
}

static void
set_default_eheap_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(1);
    ip->atype			= GOODFIT;
    ip->init.util.name_prefix	= "eheap_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 512*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 256*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_EHEAP;
    ip->init.util.rsbcst	= 50;
    ip->init.util.smn		= ERTS_MUTEX_SYS_EHEAP_ALLOC;
}

static void
set_default_binary_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(0);
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "binary_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_BINARY;
    ip->init.util.smn		= ERTS_MUTEX_SYS_BINARY_ALLOC;
}

static void
set_default_ets_alloc_opts(struct au_init *ip)
{
    SET_DEFAULT_ALLOC_OPTS(ip);
    ip->enable			= AU_ALLOC_DEFAULT_ENABLE(0);
    ip->atype			= BESTFIT;
    ip->init.util.name_prefix	= "ets_";
#ifndef SMALL_MEMORY
    ip->init.util.mmbcs 	= 128*1024; /* Main carrier size */
#else
    ip->init.util.mmbcs 	= 32*1024; /* Main carrier size */
#endif
    ip->init.util.ts 		= ERTS_ALC_MTA_ETS;
    ip->init.util.smn		= ERTS_MUTEX_SYS_ETS_ALLOC;
}

static void handle_args(int *, char **, init_t *);

static void
set_au_allocator(ErtsAllocatorFunctions_t *af,
		 ErtsAllocatorInfo_t *ai,
		 allocator_state *state,
		 struct au_init *init,
		 ErtsAllocatorFunctions_t *disable_af);
void
erts_alloc_init(int *argc, char **argv)
{
    Uint extra_block_size = 0;
    int i;
    init_t init = {
#if HAVE_ERTS_MSEG
	ERTS_MSEG_INIT_DEFAULT_INITIALIZER,
#endif
	ERTS_DEFAULT_TRIM_THRESHOLD,
	ERTS_DEFAULT_TOP_PAD,
	ERTS_DEFAULT_ALCU_INIT
    };

    erts_sys_alloc_init();
    erts_init_utils_mem();

    set_default_sl_alloc_opts(&init.sl_alloc);
    set_default_std_alloc_opts(&init.std_alloc);
    set_default_ll_alloc_opts(&init.ll_alloc);
    set_default_temp_alloc_opts(&init.temp_alloc);
    set_default_eheap_alloc_opts(&init.eheap_alloc);
    set_default_binary_alloc_opts(&init.binary_alloc);
    set_default_ets_alloc_opts(&init.ets_alloc);

    if (argc && argv)
	handle_args(argc, argv, &init);

#if HAVE_ERTS_MSEG
    erts_mseg_init(&init.mseg);
#endif
    erts_alcu_init(&init.alloc_util);
    erts_afalc_init();
    erts_bfalc_init();
    erts_gfalc_init();

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	erts_allctrs[i].alloc		= NULL;
	erts_allctrs[i].realloc		= NULL;
	erts_allctrs[i].free		= NULL;
	erts_allctrs[i].extra		= NULL;
	erts_allctrs_info[i].alloc_util	= 0;
	erts_allctrs_info[i].enabled	= 0;
	erts_allctrs_info[i].extra	= NULL;
    }

#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE
#ifndef PURIFY
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].alloc		= erts_fix_alloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].realloc		= erts_fix_realloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].free		= erts_fix_free;
    erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].enabled	= 1;
#else
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].alloc		= erts_sys_alloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].realloc		= erts_sys_realloc;
    erts_allctrs[ERTS_ALC_A_FIXED_SIZE].free		= erts_sys_free;
    erts_allctrs_info[ERTS_ALC_A_FIXED_SIZE].enabled	= 0;
#endif
#endif

    erts_allctrs[ERTS_ALC_A_SYSTEM].alloc		= erts_sys_alloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].realloc		= erts_sys_realloc;
    erts_allctrs[ERTS_ALC_A_SYSTEM].free		= erts_sys_free;
    erts_allctrs_info[ERTS_ALC_A_SYSTEM].enabled	= 1;

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_TEMPORARY],
		     &erts_allctrs_info[ERTS_ALC_A_TEMPORARY],
		     &temp_alloc_state,
		     &init.temp_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_SHORT_LIVED],
		     &erts_allctrs_info[ERTS_ALC_A_SHORT_LIVED],
		     &sl_alloc_state,
		     &init.sl_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_STANDARD],
		     &erts_allctrs_info[ERTS_ALC_A_STANDARD],
		     &std_alloc_state,
		     &init.std_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_LONG_LIVED],
		     &erts_allctrs_info[ERTS_ALC_A_LONG_LIVED],
		     &ll_alloc_state,
		     &init.ll_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_EHEAP],
		     &erts_allctrs_info[ERTS_ALC_A_EHEAP],
		     &eheap_alloc_state,
		     &init.eheap_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_BINARY],
		     &erts_allctrs_info[ERTS_ALC_A_BINARY],
		     &binary_alloc_state,
		     &init.binary_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    set_au_allocator(&erts_allctrs[ERTS_ALC_A_ETS],
		     &erts_allctrs_info[ERTS_ALC_A_ETS],
		     &ets_alloc_state,
		     &init.ets_alloc,
		     &erts_allctrs[ERTS_ALC_A_SYSTEM]);

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (!erts_allctrs[i].alloc)
	    erl_exit(1, "Missing alloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].realloc)
	    erl_exit(1, "Missing realloc function for %s\n", ERTS_ALC_A2AD(i));
	if (!erts_allctrs[i].free)
	    erl_exit(1, "Missing free function for %s\n", ERTS_ALC_A2AD(i));
    }

    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, init.trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, init.top_pad);

    /* Permanently disable use of mmap for sys_alloc (malloc). */
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, 0);

    fix_core_allocator	= erts_allctrs[ERTS_ALC_A_LONG_LIVED].alloc;
    fix_core_extra	= erts_allctrs[ERTS_ALC_A_LONG_LIVED].extra;

    erts_mtrace_init(init.instr.mtrace);
    extra_block_size += erts_instr_init(init.instr.stat, init.instr.map);

#ifdef DEBUG
    extra_block_size += install_debug_functions();
#endif

#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE

    erts_init_fix_alloc(extra_block_size, fix_core_alloc);

#ifndef PURIFY
    erts_set_fix_size(ERTS_ALC_T_PROC,		sizeof(Process));
    erts_set_fix_size(ERTS_ALC_T_DB_TABLE,	sizeof(DbTable));
    erts_set_fix_size(ERTS_ALC_T_ATOM,		sizeof(Atom));
    erts_set_fix_size(ERTS_ALC_T_EXPORT,	sizeof(Export));
    erts_set_fix_size(ERTS_ALC_T_MODULE,	sizeof(Module));
    erts_set_fix_size(ERTS_ALC_T_REG_PROC,	sizeof(RegProc));
    erts_set_fix_size(ERTS_ALC_T_LINK,		ERL_LINK_SIZE*sizeof(Uint));
    erts_set_fix_size(ERTS_ALC_T_LINK_SH,	ERL_LINK_SH_SIZE*sizeof(Uint));
    erts_set_fix_size(ERTS_ALC_T_PROC_LIST,	sizeof(ProcessList));
    erts_set_fix_size(ERTS_ALC_T_FUN_ENTRY,	sizeof(ErlFunEntry));

#endif
#endif

}

static void
set_au_allocator(ErtsAllocatorFunctions_t *af,
		 ErtsAllocatorInfo_t *ai,
		 allocator_state *state,
		 struct au_init *init,
		 ErtsAllocatorFunctions_t *disable_af)
{

    if (!init->enable) {
	sys_memcpy((void *) af,
		   (void *) disable_af,
		   sizeof(ErtsAllocatorFunctions_t));
	ai->alloc_util	= 0;
	ai->enabled	= 0;
	ai->extra	= NULL;
	return;
    }

#ifdef USE_THREADS
    if (init->init.util.ts) {
	af->alloc	= erts_alcu_alloc_ts;
	af->realloc	= erts_alcu_realloc_ts;
	af->free	= erts_alcu_free_ts;
    }
    else {
#endif
	af->alloc	= erts_alcu_alloc;
	af->realloc	= erts_alcu_realloc;
	af->free	= erts_alcu_free;
#ifdef USE_THREADS
    }
#endif
    af->extra		= NULL;
    ai->alloc_util	= 1;
    ai->enabled		= 1;

    switch (init->atype) {
    case GOODFIT:
	af->extra = (void *) erts_gfalc_start((GFAllctr_t *) state,
					      &init->init.gf,
					      &init->init.util);
	break;
    case BESTFIT:
	af->extra = (void *) erts_bfalc_start((BFAllctr_t *) state,
					      &init->init.bf,
					      &init->init.util);
	break;
    case AFIT:
	af->extra = (void *) erts_afalc_start((AFAllctr_t *) state,
					      &init->init.af,
					      &init->init.util);
	break;
    default:
	ASSERT(0);
    }

    if (!af->extra)
	erl_exit(1, "Failed to start %salloc\n", init->init.util.name_prefix);

    ai->extra = af->extra;
}


static void bad_param(char *param_start, char *param_end)
{
    size_t len = param_end - param_start;
    char param[100];
    if (len > 99)
	len = 99;
    sys_memcpy((void *) param, (void *) param_start, len);
    param[len] = '\0';
    erl_printf(CERR, "bad \"%s\" parameter\n", param);
    erts_usage();
}

static void bad_value(char *param_start, char *param_end, char *value)
{
    size_t len = param_end - param_start;
    char param[100];
    if (len > 99)
	len = 99;
    sys_memcpy((void *) param, (void *) param_start, len);
    param[len] = '\0';
    erl_printf(CERR, "bad \"%s\" value: %s\n", param, value);
    erts_usage();
}

/* Get arg marks argument as handled by
   putting NULL in argv */
static char *
get_value(char* rest, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    argv[*ip] = NULL;
    if (*rest == '\0') {
	char *next = argv[*ip + 1];
	if (next[0] == '-'
	    && next[1] == '-'
	    &&  next[2] == '\0') {
	    bad_value(param, rest, "");
	}
	(*ip)++;
	argv[*ip] = NULL;
	return next;
    }
    return rest;
}

static ERTS_INLINE int
has_prefix(const char *prefix, const char *string)
{
    int i;
    for (i = 0; prefix[i]; i++)
	if (prefix[i] != string[i])
	    return 0;
    return 1;
}

static int
get_bool_value(char *param_end, char** argv, int* ip)
{
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    if (strcmp(value, "true") == 0)
	return 1;
    else if (strcmp(value, "false") == 0)
	return 0;
    else
	bad_value(param, param_end, value);
    return -1;
}

static Uint
get_kb_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) strtol(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0 || tmp*1024 < tmp)
	bad_value(param, param_end, value);
    return (Uint) tmp*1024;
}

static Uint
get_amount_value(char *param_end, char** argv, int* ip)
{
    Sint tmp;
    char *rest;
    char *param = argv[*ip]+1;
    char *value = get_value(param_end, argv, ip);
    errno = 0;
    tmp = (Sint) strtol(value, &rest, 10);
    if (errno != 0 || rest == value || tmp < 0)
	bad_value(param, param_end, value);
    return (Uint) tmp;
}

static void
handle_au_arg(struct au_init *auip,
	      char* sub_param,
	      char** argv,
	      int* ip)
{
    char *param = argv[*ip]+1;

    switch (sub_param[0]) {
    case 'a':
	if(has_prefix("asbcst", sub_param)) {
	    auip->init.util.asbcst = get_kb_value(sub_param + 6, argv, ip);
	}
	else if(has_prefix("as", sub_param)) {
	    char *alg = get_value(sub_param + 2, argv, ip);
	    if (strcmp("bf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 0;
	    }
	    else if (strcmp("aobf", alg) == 0) {
		auip->atype = BESTFIT;
		auip->init.bf.ao = 1;
	    }
	    else if (strcmp("gf", alg) == 0) {
		auip->atype = GOODFIT;
	    }
	    else if (strcmp("af", alg) == 0) {
		auip->atype = AFIT;
	    }
	    else {
		bad_value(param, sub_param + 1, alg);
	    }
	}
	else
	    goto bad_switch;
	break;
    case 'e':
	auip->enable = get_bool_value(sub_param+1, argv, ip);
	break;
    case 'l':
	if (has_prefix("lmbcs", sub_param)) {
	    auip->init.util.lmbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 'm':
	if (has_prefix("mbcgs", sub_param)) {
	    auip->init.util.mbcgs = get_amount_value(sub_param + 5, argv, ip);

	}
	else if (has_prefix("mbsd", sub_param)) {
	    auip->init.gf.mbsd = get_amount_value(sub_param + 4, argv, ip);
	    if (auip->init.gf.mbsd < 1)
		auip->init.gf.mbsd = 1;
	}
	else if (has_prefix("mmbcs", sub_param)) {
	    auip->init.util.mmbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else if (has_prefix("mmmbc", sub_param)) {
	    auip->init.util.mmmbc = get_amount_value(sub_param + 5, argv, ip);
	}
	else if (has_prefix("mmsbc", sub_param)) {
	    auip->init.util.mmsbc = get_amount_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    case 'r':
	if(has_prefix("rsbcmt", sub_param)) {
	    auip->init.util.rsbcmt = get_amount_value(sub_param + 6, argv, ip);
	    if (auip->init.util.rsbcmt > 100)
		auip->init.util.rsbcmt = 100;
	}
	else if(has_prefix("rsbcst", sub_param)) {
	    auip->init.util.rsbcst = get_amount_value(sub_param + 6, argv, ip);
	    if (auip->init.util.rsbcst > 100)
		auip->init.util.rsbcst = 100;
	}
	else
	    goto bad_switch;
	break;
    case 's':
	if(has_prefix("sbct", sub_param)) {
	    auip->init.util.sbct = get_kb_value(sub_param + 4, argv, ip);
	}
	else if (has_prefix("smbcs", sub_param)) {
	    auip->init.util.smbcs = get_kb_value(sub_param + 5, argv, ip);
	}
	else
	    goto bad_switch;
	break;
    default:
    bad_switch:
	bad_param(param, sub_param);
    }
}

static void
handle_args(int *argc, char **argv, init_t *init)
{
    char *arg;
    char *rest;
    int i, j;

    i = 1;

    ASSERT(argc && argv && init);

    while (i < *argc) {
	if(argv[i][0] == '-') {
	    char *param = argv[i]+1;
	    switch (argv[i][1]) {
	    case 'M':
		switch (argv[i][2]) {
		case 'B':
		    handle_au_arg(&init->binary_alloc, &argv[i][3], argv, &i);
		    break;
		case 'D':
		    handle_au_arg(&init->std_alloc, &argv[i][3], argv, &i);
		    break;
		case 'E':
		    handle_au_arg(&init->ets_alloc, &argv[i][3], argv, &i);
		    break;
		case 'F': /* fix_alloc */
		    if (has_prefix("e", param+2)) {
			arg = get_value(param+3, argv, &i);
			if (strcmp("true", arg) != 0)
			    bad_value(param, param+3, arg);
		    }
		    else
			bad_param(param, param+2);
		    break;
		case 'H':
		    handle_au_arg(&init->eheap_alloc, &argv[i][3], argv, &i);
		    break;
		case 'L':
		    handle_au_arg(&init->ll_alloc, &argv[i][3], argv, &i);
		    break;
		case 'M':
		    if (has_prefix("amcbf", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.amcbf =
#endif
			    get_kb_value(argv[i]+8, argv, &i);
		    }
		    else if (has_prefix("rmcbf", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.rmcbf =
#endif
			    get_amount_value(argv[i]+8, argv, &i);
		    }
		    else if (has_prefix("mcs", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.mcs =
#endif
			    get_amount_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("cci", argv[i]+3)) {
#if HAVE_ERTS_MSEG
			init->mseg.cci =
#endif
			    get_amount_value(argv[i]+6, argv, &i);
		    }
		    else {
			bad_param(param, param+2);
		    }
		    break;
		case 'S':
		    handle_au_arg(&init->sl_alloc, &argv[i][3], argv, &i);
		    break;
		case 'T':
		    handle_au_arg(&init->temp_alloc, &argv[i][3], argv, &i);
		    break;
		case 'Y': { /* sys_alloc */
		    if (has_prefix("tt", param+2)) {
			/* set trim threshold */
			arg = get_value(param+4, argv, &i);
			errno = 0;
			init->trim_threshold = (int) strtol(arg, &rest, 10);
			if (errno != 0
			    || rest == arg
			    || init->trim_threshold < 0
			    || (INT_MAX/1024) < init->trim_threshold) {
			    bad_value(param, param+4, arg);
			}
			VERBOSE(erl_printf(COUT, "using trim threshold: %d\n",
					   init->trim_threshold););
			init->trim_threshold *= 1024;
		    }
		    else if (has_prefix("tp", param+2)) {
			/* set top pad */
			arg = get_value(param+4, argv, &i);
			errno = 0;
			init->top_pad = (int) strtol(arg, &rest, 10);
			if (errno != 0
			    || rest == arg
			    || init->top_pad < 0
			    || (INT_MAX/1024) < init->top_pad) {
			    bad_value(param, param+4, arg);
			}
			VERBOSE(erl_printf(COUT, "using top pad: %d\n",
					   init->top_pad););
			init->top_pad *= 1024;
		    }
		    else if (has_prefix("m", param+2)) {
			/* Has been handled by erlexec */
			(void) get_value(param+3, argv, &i);
		    }
		    else if (has_prefix("e", param+2)) {
			arg = get_value(param+3, argv, &i);
			if (strcmp("true", arg) != 0)
			    bad_value(param, param+3, arg);
		    }
		    else
			bad_param(param, param+2);
		    break;
		}
		case 'e':
		    switch (argv[i][3]) {
		    case 'a':
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("min", arg) == 0) {
			    init->sl_alloc.enable		= 0;
			    init->std_alloc.enable		= 0;
			    init->ll_alloc.enable		= 0;
			    init->temp_alloc.enable		= 0;
			    init->eheap_alloc.enable		= 0;
			    init->binary_alloc.enable		= 0;
			    init->ets_alloc.enable		= 0;
			}
			else if (strcmp("max", arg) == 0) {
			    init->sl_alloc.enable		= 1;
			    init->std_alloc.enable		= 1;
			    init->ll_alloc.enable		= 1;
			    init->temp_alloc.enable		= 1;
			    init->eheap_alloc.enable		= 1;
			    init->binary_alloc.enable		= 1;
			    init->ets_alloc.enable		= 1;
			}
			else if (strcmp("r10b", arg) == 0) {
			    init->sl_alloc.enable		= 1;
			    init->std_alloc.enable		= 1;
			    init->ll_alloc.enable		= 1;
			    init->temp_alloc.enable		= 1;
			    init->eheap_alloc.enable		= 1;
			    init->binary_alloc.enable		= 1;
			    init->ets_alloc.enable		= 1;
			}
			else {
			    bad_param(param, param+3);
			}
			break;
		    default:
			bad_param(param, param+1);
		    }
		    break;
		case 'i':
		    switch (argv[i][3]) {
		    case 's':
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("true", arg) == 0)
			    init->instr.stat = 1;
			else if (strcmp("false", arg) == 0)
			    init->instr.stat = 0;
			else
			    bad_value(param, param+3, arg);
			break;
		    case 'm':
			arg = get_value(argv[i]+4, argv, &i);
			if (strcmp("true", arg) == 0)
			    init->instr.map = 1;
			else if (strcmp("false", arg) == 0)
			    init->instr.map = 0;
			else
			    bad_value(param, param+3, arg);
			break;
		    case 't':
			init->instr.mtrace = get_value(argv[i]+4, argv, &i);
			break;
		    default:
			bad_param(param, param+2);
		    }
		    break;
		case 'u':
		    if (has_prefix("ycs", argv[i]+3)) {
			init->alloc_util.ycs
			    = get_kb_value(argv[i]+6, argv, &i);
		    }
		    else if (has_prefix("mmc", argv[i]+3)) {
			init->alloc_util.mmc
			    = get_amount_value(argv[i]+6, argv, &i);
		    }
		    else {
			bad_param(param, param+2);
		    }
		    break;
		default:
		    bad_param(param, param+1);
		}
		break;
	    case '-':
		if (argv[i][2] == '\0')
		    goto args_parsed;
		break;
	    default:
		break;
	    }
	}
	i++;
    }

 args_parsed:
    /* Handled arguments have been marked with NULL. Slide arguments
       not handled towards the beginning of argv. */
    for (i = 0, j = 0; i < *argc; i++) {
	if (argv[i])
	    argv[j++] = argv[i];
    }
    *argc = j;
    
}

static char *type_no_str(ErtsAlcType_t n)
{

#if ERTS_ALC_N_MIN != 0
    if (n < ERTS_ALC_N_MIN)
	return NULL;
#endif
    if (n > ERTS_ALC_N_MAX)
	return NULL;
    return (char *) ERTS_ALC_N2TD(n);
}

#define type_str(T) type_no_str(ERTS_ALC_T2N((T)))

void
erts_alc_fatal_error(int error, int func, ErtsAlcType_t n, ...)
{
    char buf[10];
    char *t_str;
    char *allctr_str;

    ASSERT(n >= ERTS_ALC_N_MIN);
    ASSERT(n <= ERTS_ALC_N_MAX);


    if (n < ERTS_ALC_N_MIN || ERTS_ALC_N_MAX < n)
	allctr_str = "UNKNOWN";
    else {
	ErtsAlcType_t a = ERTS_ALC_T2A(ERTS_ALC_N2T(n));
	if (erts_allctrs_info[a].enabled)
	    allctr_str = (char *) ERTS_ALC_A2AD(a);
	else
	    allctr_str = (char *) ERTS_ALC_A2AD(ERTS_ALC_A_SYSTEM);
    }

    t_str = type_no_str(n);
    if (!t_str) {
	sprintf(buf, "%d", (int) n);
	t_str = buf;
    }

    switch (error) {
    case ERTS_ALC_E_NOTSUP: {
	char *op_str;
	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "alloc";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "realloc";	break;
	case ERTS_ALC_O_FREE:		op_str = "free";	break;
	default:			op_str = "UNKNOWN";	break;
	}
	erl_exit(1,
		 "%s: %s operation not supported (memory type: \"%s\")\n",
		 allctr_str, op_str, t_str);
	break;
    }
    case ERTS_ALC_E_NOMEM: {
	Uint size;
	va_list argp;
	char *op = func == ERTS_ALC_O_REALLOC ? "reallocate" : "allocate";
	

	va_start(argp, n);
	size = va_arg(argp, Uint);
	va_end(argp);
	erl_exit(1,
		 "%s: Cannot %s %lu bytes of memory (of type \"%s\").\n",
		 allctr_str, op, size, t_str);
	break;
    }
    case ERTS_ALC_E_NOALLCTR:
	erl_exit(1,
		 "erts_alloc: Unknown allocator type: %d\n",
		 ERTS_ALC_T2A(ERTS_ALC_N2T(n)));
	break;
    default:
	erl_exit(1,"erts_alloc: Unknown error: %d\n", error);
	break;
    }
}

void
erts_alloc_enomem(ErtsAlcType_t type, Uint size)
{
    erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
}

void
erts_alloc_n_enomem(ErtsAlcType_t n, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_ALLOC, n, size);
}

void
erts_realloc_enomem(ErtsAlcType_t type, void *ptr, Uint size)
{
    erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
}

void
erts_realloc_n_enomem(ErtsAlcType_t n, void *ptr, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_REALLOC, n, size);
}

static ERTS_INLINE int
is_atom_text(Eterm atom, const char *str)
{
    int i;
    char *atxt = atom_tab(atom_val(atom))->name;
    int alen = atom_tab(atom_val(atom))->len;

    for (i = 0; i < alen && str[i]; i++)
	if (atxt[i] != str[i])
	    return 0;
    if (alen == i && !str[i])
	return 1;
    return 0;
}



/* from dist.c */
extern byte *dist_buf;
extern int dist_buf_size;

Eterm
erts_memory(CIO *ciop, void *proc, Eterm earg)
{
#define MEM_NEED_PARTS (!erts_instr_stat && want_tot_or_sys)
    ErtsFixInfo efi;
    struct {
	int total;
	int processes;
	int processes_used;
	int system;
	int atom;
	int atom_used;
	int binary;
	int code;
	int ets;
	int maximum;
    } want = {0};
    struct {
	Uint total;
	Uint processes;
	Uint processes_used;
	Uint system;
	Uint atom;
	Uint atom_used;
	Uint binary;
	Uint code;
	Uint ets;
	Uint maximum;
    } size = {0};
    Eterm atoms[sizeof(size)/sizeof(Uint)];
    Uint *uintps[sizeof(size)/sizeof(Uint)];
    Eterm euints[sizeof(size)/sizeof(Uint)];
    int need_atom;
    int want_tot_or_sys;
    int length;
    Eterm res = THE_NON_VALUE;

    /* Figure out whats wanted... */

    length = 0;
    if (earg == THE_NON_VALUE) { /* i.e. wants all */
	want.total = 1;
	atoms[length] = am_total;
	uintps[length++] = &size.total;

	want.processes = 1;
	atoms[length] = am_processes;
	uintps[length++] = &size.processes;

	want.processes_used = 1;
	atoms[length] = am_processes_used;
	uintps[length++] = &size.processes_used;

	want.system = 1;
	atoms[length] = am_system;
	uintps[length++] = &size.system;

	want.atom = 1;
	atoms[length] = am_atom;
	uintps[length++] = &size.atom;

	want.atom_used = 1;
	atoms[length] = am_atom_used;
	uintps[length++] = &size.atom_used;

	want.binary = 1;
	atoms[length] = am_binary;
	uintps[length++] = &size.binary;

	want.code = 1;
	atoms[length] = am_code;
	uintps[length++] = &size.code;

	want.ets = 1;
	atoms[length] = am_ets;
	uintps[length++] = &size.ets;

	want.maximum = erts_instr_stat;
	if (want.maximum) {
	    atoms[length] = am_maximum;
	    uintps[length++] = &size.maximum;
	}

    }
    else {
	Eterm wanted_list;
	if (is_nil(earg))
	    return NIL;
	wanted_list = earg;
	while (is_list(wanted_list)) {
	    switch (CAR(list_val(wanted_list))) {
	    case am_total:
		if (!want.total) {
		    want.total = 1;
		    atoms[length] = am_total;
		    uintps[length++] = &size.total;
		}
		break;
	    case am_processes:
		if (!want.processes) {
		    want.processes = 1;
		    atoms[length] = am_processes;
		    uintps[length++] = &size.processes;
		}
		break;
	    case am_processes_used:
		if (!want.processes_used) {
		    want.processes_used = 1;
		    atoms[length] = am_processes_used;
		    uintps[length++] = &size.processes_used;
		}
		break;
	    case am_system:
		if (!want.system) {
		    want.system = 1;
		    atoms[length] = am_system;
		    uintps[length++] = &size.system;
		}
		break;
	    case am_atom:
		if (!want.atom) {
		    want.atom = 1;
		    atoms[length] = am_atom;
		    uintps[length++] = &size.atom;
		}
		break;
	    case am_atom_used:
		if (!want.atom_used) {
		    want.atom_used = 1;
		    atoms[length] = am_atom_used;
		    uintps[length++] = &size.atom_used;
		}
		break;
	    case am_binary:
		if (!want.binary) {
		    want.binary = 1;
		    atoms[length] = am_binary;
		    uintps[length++] = &size.binary;
		}
		break;
	    case am_code:
		if (!want.code) {
		    want.code = 1;
		    atoms[length] = am_code;
		    uintps[length++] = &size.code;
		}
		break;
	    case am_ets:
		if (!want.ets) {
		    want.ets = 1;
		    atoms[length] = am_ets;
		    uintps[length++] = &size.ets;
		}
		break;
	    case am_maximum:
		if (erts_instr_stat) {
		    if (!want.maximum) {
			want.maximum = 1;
			atoms[length] = am_maximum;
			uintps[length++] = &size.maximum;
		    }
		}
		else
		    return am_badarg;
		break;
	    default:
		return am_badarg;
	    }
	    wanted_list = CDR(list_val(wanted_list));
	}
	if (is_not_nil(wanted_list))
	    return am_badarg;
    }

    ASSERT(length <= sizeof(atoms)/sizeof(Eterm));
    ASSERT(length <= sizeof(euints)/sizeof(Eterm));
    ASSERT(length <= sizeof(uintps)/sizeof(Uint));

    /* Calculate values needed... */

    want_tot_or_sys = want.total || want.system;
    need_atom = MEM_NEED_PARTS || want.atom;

    size.processes = size.processes_used = erts_tot_proc_mem;

    if (MEM_NEED_PARTS || want.processes) {
	erts_fix_info(ERTS_ALC_T_LINK, &efi);
	size.processes += efi.total - efi.used;
	erts_fix_info(ERTS_ALC_T_LINK_SH, &efi);
	size.processes += efi.total - efi.used;
	erts_fix_info(ERTS_ALC_T_PROC, &efi);
	size.processes += efi.total - efi.used;
	erts_fix_info(ERTS_ALC_T_REG_PROC, &efi);
	size.processes += efi.total - efi.used;
    }

    if (need_atom || want.atom_used) {
	size.atom = size.atom_used = index_table_sz(&atom_table);
	erts_fix_info(ERTS_ALC_T_ATOM, &efi);

	if (need_atom) {
	    size.atom += reserved_atom_space;
	    size.atom += efi.total;
	}

	if (want.atom_used) {
	    size.atom_used += atom_space;
	    size.atom_used += efi.used;
	}
    }

    size.binary = erts_allocated_binaries;

    if (MEM_NEED_PARTS || want.code) {
	size.code = index_table_sz(&module_table);
	erts_fix_info(ERTS_ALC_T_MODULE, &efi);
	size.code += efi.used;
	size.code += index_table_sz(&export_table);
	erts_fix_info(ERTS_ALC_T_EXPORT, &efi);
	size.code += efi.used;
	size.code += hash_table_sz(&erts_fun_table);
	erts_fix_info(ERTS_ALC_T_FUN_ENTRY, &efi);
	size.code += efi.used;
	size.code += allocated_modules*sizeof(Range);
	size.code += erts_total_code_size;
    }

    size.ets = erts_tot_ets_memory_words*sizeof(Uint);

    if (erts_instr_stat && (want_tot_or_sys || want.maximum)) {
	size.total = erts_instr_get_total();
	size.system = size.total - size.processes;
	size.maximum = erts_instr_get_max_total();
    }
    else if (want_tot_or_sys) {

	/* Static stuff ... */
	size.system = erts_max_ports*sizeof(Port);
	size.system += erts_timer_wheel_memory_size();
	size.system += TMP_BUF_SIZE*sizeof(byte);
	size.system += 14*sizeof(Eterm); /* XXX dmem in dist.c */
	size.system += 64+1; /* XXX dbuf in erl_message.c */

	/* Misc stuff ... */
	size.system += erts_sys_misc_mem_sz;
	size.system += erts_dist_table_size();
	size.system += erts_node_table_size();
	size.system += erts_bits_bufs_size();
	size.system += hash_table_sz(&process_reg);
	erts_fix_info(ERTS_ALC_T_REG_PROC, &efi);
	size.system += efi.total;
	erts_fix_info(ERTS_ALC_T_PROC_LIST, &efi);
	size.system += efi.total;
	if (dist_buf != tmp_buf)
	    size.system += dist_buf_size + 20;
        
	/* Atom, binary, code, and ets */
	size.system += size.atom;
	size.system += size.binary;
	size.system += size.code;
	size.system += size.ets;

	size.total = size.system + size.processes;
    }

    if (ciop) {
	int i;
	CIO to = *ciop;
	/* Print result... */
	erl_printf(to, "=memory\n");
	for (i = 0; i < length; i++) {
	    display(atoms[i], to);
	    erl_printf(to, ": %lu\n", *uintps[i]);
	}
    }

    if (proc) {
	/* Build erlang term result... */
	Uint *hp;
	Uint **hpp;
	Uint hsz;
	Uint *hszp;

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

	while (1) {
	    int i;
	    for (i = 0; i < length; i++)
		euints[i] = erts_bld_uint(hpp, hszp, *uintps[i]);
	    res = erts_bld_2tup_list(hpp, hszp, length, atoms, euints);
	    if (hpp)
		break;
	    hp = HAlloc((Process *) proc, hsz);
	    hpp = &hp;
	    hszp = NULL;
	}
    }

    return res;

#undef MEM_NEED_PARTS
}

struct aa_values {
    Uint arity;
    const char *name;
    Uint ui[2];
};

Eterm
erts_allocated_areas(CIO *ciop, void *proc)
{
#define MAX_AA_VALUES \
  (21 + (ERTS_ALC_N_MAX_A_FIXED_SIZE - ERTS_ALC_N_MIN_A_FIXED_SIZE + 1))

    struct aa_values values[MAX_AA_VALUES];
    Eterm res = THE_NON_VALUE;
    int i, length;
    ErtsFixInfo efi;

    i = 0;

    if (erts_instr_stat) {
	values[i].arity = 2;
	values[i].name = "total";
	values[i].ui[0] = erts_instr_get_total();
	i++;

	values[i].arity = 2;
	values[i].name = "maximum";
	values[i].ui[0] = erts_instr_get_max_total();
	i++;
    }

    values[i].arity = 3;
    values[i].name = "processes";
    values[i].ui[0] = erts_tot_proc_mem;

    values[i].ui[1] = erts_tot_proc_mem;
    erts_fix_info(ERTS_ALC_T_LINK, &efi);
    values[i].ui[1] += efi.total - efi.used;
    erts_fix_info(ERTS_ALC_T_LINK_SH, &efi);
    values[i].ui[1] += efi.total - efi.used;
    erts_fix_info(ERTS_ALC_T_PROC, &efi);
    values[i].ui[1] += efi.total - efi.used;
    erts_fix_info(ERTS_ALC_T_REG_PROC, &efi);
    values[i].ui[1] += efi.total - efi.used;
    i++;

    values[i].arity = 2;
    values[i].name = "ets";
    values[i].ui[0] = erts_tot_ets_memory_words*sizeof(Uint);
    i++;

    values[i].arity = 2;
    values[i].name = "sys_misc";
    values[i].ui[0] = erts_sys_misc_mem_sz;
    i++;

    values[i].arity = 2;
    values[i].name = "static";
    values[i].ui[0] = 
	erts_max_ports*sizeof(Port)		/* Port table */
	+ erts_timer_wheel_memory_size()	/* Timer wheel */
	+ TMP_BUF_SIZE*sizeof(byte)		/* Tmp buffer */
	+ 14*sizeof(Eterm)			/* XXX dmem in dist.c */
	+ 64+1				/* XXX dbuf in erl_message.c */;
    i++;

    values[i].arity = 3;
    values[i].name = "atom_space";
    values[i].ui[0] = reserved_atom_space;
    values[i].ui[1] = atom_space;
    i++;

    values[i].arity = 2;
    values[i].name = "binary";
    values[i].ui[0] = erts_allocated_binaries;
    i++;

    values[i].arity = 2;
    values[i].name = "atom_table";
    values[i].ui[0] = index_table_sz(&atom_table);
    i++;

    values[i].arity = 2;
    values[i].name = "module_table";
    values[i].ui[0] = index_table_sz(&module_table);
    i++;

    values[i].arity = 2;
    values[i].name = "export_table";
    values[i].ui[0] = index_table_sz(&export_table);
    i++;

    values[i].arity = 2;
    values[i].name = "register_table";
    values[i].ui[0] = hash_table_sz(&process_reg);
    i++;

    values[i].arity = 2;
    values[i].name = "fun_table";
    values[i].ui[0] = hash_table_sz(&erts_fun_table);
    i++;

    values[i].arity = 2;
    values[i].name = "module_refs";
    values[i].ui[0] = allocated_modules*sizeof(Range);
    i++;

    values[i].arity = 2;
    values[i].name = "loaded_code";
    values[i].ui[0] = erts_total_code_size;
    i++;

    values[i].arity = 2;
    values[i].name = "dist_table";
    values[i].ui[0] = erts_dist_table_size();
    i++;

    values[i].arity = 2;
    values[i].name = "node_table";
    values[i].ui[0] = erts_node_table_size();
    i++;

    values[i].arity = 2;
    values[i].name = "bits_bufs_size";
    values[i].ui[0] = erts_bits_bufs_size();
    i++;

    values[i].arity = 2;
    values[i].name = "bif_timer";
    values[i].ui[0] = bif_timer_memory_size();
    i++;

    values[i].arity = 2;
    values[i].name = "link_lh";
    values[i].ui[0] = erts_tot_link_lh_size;
    i++;

    values[i].arity = 2;
    values[i].name = "dist_buf";
    values[i].ui[0] = (dist_buf != tmp_buf
		       ? (dist_buf_size + 20)
		       : 0);
    i++;

    {
	Uint n;

	for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
	     n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
	     n++) {
	    erts_fix_info(ERTS_ALC_N2T(n), &efi);

	    values[i].arity = 3;
	    values[i].name = ERTS_ALC_N2TD(n);
	    values[i].ui[0] = efi.total;
	    values[i].ui[1] = efi.used;
	    i++;
	}    

    }

    length = i;
    ASSERT(length <= MAX_AA_VALUES);

    if (ciop) {
	/* Print result... */
	CIO to = *ciop;

	erl_printf(to, "=allocated_areas\n");
	for (i = 0; i < length; i++) {
	    switch (values[i].arity) {
	    case 2:
		erl_printf(to, "%s: %lu\n", values[i].name, values[i].ui[0]);
		break;
	    case 3:
		erl_printf(to, "%s: %lu %lu\n",
			   values[i].name, values[i].ui[0], values[i].ui[1]);
		break;
	    default:
		erl_printf(to, "ERROR: internal_error\n");
		ASSERT(0);
		return am_internal_error;
	    }
	}
    }

    if (proc) {
	/* Build erlang term result... */
	Eterm tuples[MAX_AA_VALUES];
	Uint *hp;
	Uint **hpp;
	Uint hsz;
	Uint *hszp;

	hpp = NULL;
	hsz = 0;
	hszp = &hsz;

	while (1) {
	    int i;
	    for (i = 0; i < length; i++) {
		Eterm atom;
		if (hpp)
		    atom = am_atom_put((byte *) values[i].name,
				       (int) strlen(values[i].name));
		else
		    atom = am_true;

		switch (values[i].arity) {
		case 2:
		    tuples[i] = erts_bld_tuple(hpp, hszp, 2,
					       atom,
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[0]));
		    break;
		case 3:
		    tuples[i] = erts_bld_tuple(hpp, hszp, 3,
					       atom,
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[0]),
					       erts_bld_uint(hpp, hszp,
							     values[i].ui[1]));
		    break;
		default:
		    ASSERT(0);
		    return am_internal_error;
		}
	    }
	    res = erts_bld_list(hpp, hszp, length, tuples);
	    if (hpp)
		break;
	    hp = HAlloc((Process *) proc, hsz);
	    hpp = &hp;
	    hszp = NULL;
	}
    }

    return res;
#undef MAX_AA_VALUES
}

Eterm
erts_allocator_info_term(void *proc, Eterm which_alloc)
{
    ErtsAlcType_t i;
    Uint sz = 0;
    Uint *hp = NULL;

    if (is_not_atom(which_alloc))
	return am_undefined;

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	if (is_atom_text(which_alloc, ERTS_ALC_A2AD(i))) {
	    if (!erts_allctrs_info[i].enabled)
		return am_false;
	    else {
		if (erts_allctrs_info[i].alloc_util) {
		    Allctr_t *allctr = (Allctr_t *) erts_allctrs_info[i].extra;
		    erts_alcu_info(allctr, 0, NULL, NULL, &sz);
		    if (sz)
			hp = HAlloc((Process *) proc, sz);
		    return erts_alcu_info(allctr, 1, NULL, &hp, NULL);
		}
		else {
		    Eterm *szp, **hpp, res;

		    switch (i) {
		    case ERTS_ALC_A_SYSTEM: {
			SysAllocStat sas;
			Eterm opts_am;
			Eterm opts;
			Eterm as[4];
			Eterm ts[4];
			int l;

			sys_alloc_stat(&sas);
			opts_am = am_atom_put("options", 7);

			szp = &sz;
			hpp = NULL;

		    restart_sys_alloc:
			l = 0;
			as[l] = am_atom_put("e", 1);
			ts[l++] = am_true;
#ifdef ELIB_ALLOC_IS_CLIB
			as[l] = am_atom_put("m", 1);
			ts[l++] = am_atom_put("elib", 4);
#else
			as[l] = am_atom_put("m", 1);
			ts[l++] = am_atom_put("libc", 4);
#endif
			if(sas.trim_threshold >= 0) {
			    as[l] = am_atom_put("tt", 2);
			    ts[l++] = erts_bld_uint(hpp, szp,
						    (Uint) sas.trim_threshold);
			}
			if(sas.top_pad >= 0) {
			    as[l] = am_atom_put("tp", 2);
			    ts[l++] = erts_bld_uint(hpp, szp, (Uint) sas.top_pad);
			}

			opts = erts_bld_2tup_list(hpp, szp, l, as, ts);
			res = erts_bld_2tup_list(hpp, szp, 1, &opts_am, &opts);
			
			if (szp) {
			    hp = HAlloc((Process *) proc, sz);
			    szp = NULL;
			    hpp = &hp;
			    goto restart_sys_alloc;
			}
			return res;
		    }
		    case ERTS_ALC_A_FIXED_SIZE: {
			ErtsAlcType_t n;
			Eterm as[2], vs[2];

			as[0] = am_atom_put("options", 7);
			as[1] = am_atom_put("pools", 5);

			szp = &sz;
			hpp = NULL;

		    restart_fix_alloc:

			vs[0] = erts_bld_cons(hpp, szp,
					      erts_bld_tuple(hpp, szp, 2, 
							     am_atom_put("e",
									 1),
							     am_true),
					      NIL);

			vs[1] = NIL;
			for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
			     n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
			     n++) {
			    ErtsFixInfo efi;
			    erts_fix_info(ERTS_ALC_N2T(n), &efi);

			    vs[1] = erts_bld_cons(
				hpp, szp,
				erts_bld_tuple(
				    hpp, szp, 3,
				    am_atom_put((char *) ERTS_ALC_N2TD(n),
						strlen(ERTS_ALC_N2TD(n))),
				    erts_bld_uint(hpp, szp, efi.total),
				    erts_bld_uint(hpp, szp, efi.used)),
				vs[1]);

			}
			
			res = erts_bld_2tup_list(hpp, szp, 2, as, vs);
			if (szp) {
			    hp = HAlloc((Process *) proc, sz);
			    szp = NULL;
			    hpp = &hp;
			    goto restart_fix_alloc;
			}
			return res;
		    }
		    default:
			ASSERT(0);
			return am_undefined;
		    }
		}
	    }
	}
    }

    if (is_atom_text(which_alloc, "mseg_alloc")) {

#if HAVE_ERTS_MSEG
	erts_mseg_info(NULL, NULL, &sz);
	if (sz)
	    hp = HAlloc((Process *) proc, sz);
	return erts_mseg_info(NULL, &hp, NULL);
#else
	return am_false;
#endif

    }
    else if (is_atom_text(which_alloc, "alloc_util")) {
	erts_alcu_au_info_options(NULL, NULL, &sz);
	if (sz)
	    hp = HAlloc((Process *) proc, sz);
	return erts_alcu_au_info_options(NULL, &hp, NULL);
    }

    return am_undefined;
}

void
erts_allocator_info(CIO to)
{
    ErtsAlcType_t a;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	erl_printf(to, "=allocator:%s\n", ERTS_ALC_A2AD(a));
	if (!erts_allctrs_info[a].enabled)
	    erl_printf(to, "option e: false\n");
	else {
	    if (erts_allctrs_info[a].alloc_util)
		erts_alcu_info(erts_allctrs_info[a].extra,
			       0, &to, NULL, NULL);
	    else {
		switch (a) {
		case ERTS_ALC_A_SYSTEM: {
		    SysAllocStat sas;
		    erl_printf(to, "option e: true\n");
#ifdef ELIB_ALLOC_IS_CLIB
		    erl_printf(to, "option m: elib\n");
#else
		    erl_printf(to, "option m: libc\n");
#endif
		    sys_alloc_stat(&sas);
		    if(sas.trim_threshold >= 0)
			erl_printf(to, "option tt: %d\n", sas.trim_threshold);
		    if(sas.top_pad >= 0)
			erl_printf(to, "option tp: %d\n", sas.top_pad);
		    break;
		}
		case ERTS_ALC_A_FIXED_SIZE: {
		    ErtsAlcType_t n;
		    erl_printf(to, "option e: true\n");

		    for (n = ERTS_ALC_N_MIN_A_FIXED_SIZE;
			 n <= ERTS_ALC_N_MAX_A_FIXED_SIZE;
			 n++) {
			ErtsFixInfo efi;
			erts_fix_info(ERTS_ALC_N2T(n), &efi);
			erl_printf(to, "%s: %lu %lu\n",
				   ERTS_ALC_N2TD(n),
				   efi.total,
				   efi.used);
		    }
		    break;
		}
		default:
		    ASSERT(0);
		    break;
		}
	    }
	}
    }

#if HAVE_ERTS_MSEG
    erl_printf(to, "=allocator:mseg_alloc\n");
    erts_mseg_info(&to, NULL, NULL);
#endif

    erl_printf(to, "=allocator:alloc_util\n");
    erts_alcu_au_info_options(&to, NULL, NULL);

    erl_printf(to, "=allocator:instr\n");
    erl_printf(to, "option m: %s\n", erts_instr_memory_map ? "true" : "false");
    erl_printf(to, "option s: %s\n", erts_instr_stat ? "true" : "false");
    erl_printf(to, "option t: %s\n", erts_mtrace_enabled ? "true" : "false");

}

Eterm
erts_allocator_options(void *proc)
{
#if HAVE_ERTS_MSEG
    int use_mseg = 0;
#endif
    Uint sz, *szp, *hp, **hpp;
    Eterm res, features, settings;
    Eterm atoms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+5];
    Uint terms[ERTS_ALC_A_MAX-ERTS_ALC_A_MIN+5];
    int a, length;
    SysAllocStat sas;
#ifdef DEBUG
    Uint *endp;
#endif

    sys_alloc_stat(&sas);

    /* First find out the heap size needed ... */
    hpp = NULL;
    szp = &sz;
    sz = 0;

 bld_term:

    length = 0;
    features = NIL;
    settings = NIL;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	Eterm tmp = NIL;
	atoms[length] = am_atom_put((char *) ERTS_ALC_A2AD(a),
				    strlen(ERTS_ALC_A2AD(a)));
	if (erts_allctrs_info[a].enabled) {
	    if (erts_allctrs_info[a].alloc_util) {
#if HAVE_ERTS_MSEG
		use_mseg++;
#endif
		tmp = erts_alcu_info_options(erts_allctrs_info[a].extra,
					     NULL, hpp, szp);
	    }
	    else {
		int l = 0;
		Eterm as[4];
		Eterm ts[4];

		as[l] = am_atom_put("e", 1);
		ts[l++] = am_true;

		switch (a) {
		case ERTS_ALC_A_SYSTEM:
#ifdef ELIB_ALLOC_IS_CLIB
		    as[l] = am_atom_put("m", 1);
		    ts[l++] = am_atom_put("elib", 4);
#else
		    as[l] = am_atom_put("m", 1);
		    ts[l++] = am_atom_put("libc", 4);
#endif
		    if(sas.trim_threshold >= 0) {
			as[l] = am_atom_put("tt", 2);
			ts[l++] = erts_bld_uint(hpp, szp,
						(Uint) sas.trim_threshold);
		    }
		    if(sas.top_pad >= 0) {
			as[l] = am_atom_put("tp", 2);
			ts[l++] = erts_bld_uint(hpp, szp, (Uint) sas.top_pad);
		    }
		    break;
		default:
		    break;
		}

		tmp = erts_bld_2tup_list(hpp, szp, l, as, ts);

	    }

	}
	else {
	    Eterm atom = am_atom_put("e", 1);
	    Eterm term = am_false;
	    tmp = erts_bld_2tup_list(hpp, szp, 1, &atom, &term);
	}

	terms[length++] = tmp;

    }

#if HAVE_ERTS_MSEG
    if (use_mseg) {
	atoms[length] = am_atom_put("mseg_alloc", 10);
	terms[length++] = erts_mseg_info_options(NULL, hpp, szp);
    }
#endif

    atoms[length] = am_atom_put("alloc_util", 10); 
    terms[length++] = erts_alcu_au_info_options(NULL, hpp, szp);

    {
	Eterm o[3], v[3];
	o[0] = am_atom_put("m", 1);
	v[0] = erts_instr_memory_map ? am_true : am_false;
	o[1] = am_atom_put("s", 1);
	v[1] = erts_instr_stat ? am_true : am_false;
	o[2] = am_atom_put("t", 1);
	v[2] = erts_mtrace_enabled ? am_true : am_false;

	atoms[length] = am_atom_put("instr", 5); 
	terms[length++] = erts_bld_2tup_list(hpp, szp, 3, o, v);
    }

    settings = erts_bld_2tup_list(hpp, szp, length, atoms, terms);

    length = 0;

    for (a = ERTS_ALC_A_MIN; a <= ERTS_ALC_A_MAX; a++) {
	if (erts_allctrs_info[a].enabled) {
	    terms[length++] = am_atom_put((char *) ERTS_ALC_A2AD(a),
					  strlen(ERTS_ALC_A2AD(a)));
	}
    }

#if HAVE_ERTS_MSEG
    if (use_mseg)
	terms[length++] = am_atom_put("mseg_alloc", 10);
#endif

    features = length ? erts_bld_list(hpp, szp, length, terms) : NIL;

#if defined(ELIB_ALLOC_IS_CLIB)
    {
	Eterm version;
	int i;
	int ver[5];
	i = sscanf(ERLANG_VERSION,
		   "%d.%d.%d.%d.%d",
		   &ver[0], &ver[1], &ver[2], &ver[3], &ver[4]);

	version = NIL;
	for(i--; i >= 0; i--)
	  version = erts_bld_cons(hpp, szp, make_small(ver[i]), version);

	res = erts_bld_tuple(hpp, szp, 4,
			     am_elib_malloc, version, features, settings);
    }
#elif defined(__GLIBC__)
    {
	Eterm AM_glibc = am_atom_put("glibc", 5);
	Eterm version;

	version = erts_bld_cons(hpp,
				szp,
				make_small(__GLIBC__),
#ifdef __GLIBC_MINOR__
				erts_bld_cons(hpp,
					      szp,
					      make_small(__GLIBC_MINOR__),
					      NIL)
#else
				NIL
#endif
	    );

	res = erts_bld_tuple(hpp, szp, 4,
			     AM_glibc, version, features, settings);
    }

#else /* unknown allocator */

    res = erts_bld_tuple(hpp, szp, 4,
			 am_undefined, NIL, features, settings);

#endif

    if (szp) {
	/* ... and then build the term */
	hp = HAlloc((Process *) proc, sz);
#ifdef DEBUG
	endp = hp + sz;
#endif
	hpp = &hp;
	szp = NULL;
	goto bld_term;
    }

#ifdef DEBUG
    ASSERT(endp == hp);
#endif

    return res;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Deprecated functions                                                    *
 *                                                                         *
 * These functions are still defined since "non-OTP linked in drivers" may *
 * contain (illegal) calls to them.                                        *
\*                                                                         */

/* --- DO *NOT* USE THESE FUNCTIONS --- */

void *sys_alloc(Uint sz)
{ return erts_alloc_fnf(ERTS_ALC_T_UNDEF, sz); }
void *sys_realloc(void *ptr, Uint sz)
{ return erts_realloc_fnf(ERTS_ALC_T_UNDEF, ptr, sz); }
void sys_free(void *ptr)
{ erts_free(ERTS_ALC_T_UNDEF, ptr); }
void *safe_alloc(Uint sz)
{ return erts_alloc(ERTS_ALC_T_UNDEF, sz); }
void *safe_realloc(void *ptr, Uint sz)
{ return erts_realloc(ERTS_ALC_T_UNDEF, ptr, sz); }


/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE: erts_alc_test() is only supposed to be used for testing.            *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_alc_test()                                                        *
\*                                                                           */

unsigned long erts_alc_test(unsigned long op,
			    unsigned long a1,
			    unsigned long a2,
			    unsigned long a3)
{
    switch (op >> 8) {
    case 0x0:	return erts_alcu_test(op,  a1, a2);
    case 0x1:	return erts_gfalc_test(op, a1, a2);
    case 0x2:	return erts_bfalc_test(op, a1, a2);
    case 0x3:	return erts_afalc_test(op, a1, a2);
    case 0xf:
	switch (op) {
	case 0xf00:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		return (unsigned long) erts_alcu_alloc_ts(ERTS_ALC_T_UNDEF,
							  (void *) a1,
							  (Uint) a2);
	    else
#endif
		return (unsigned long) erts_alcu_alloc(ERTS_ALC_T_UNDEF,
						       (void *) a1,
						       (Uint) a2);
	case 0xf01:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		return (unsigned long) erts_alcu_realloc_ts(ERTS_ALC_T_UNDEF,
							    (void *) a1,
							    (void *) a2,
							    (Uint) a3);
	    else
#endif
		return (unsigned long) erts_alcu_realloc(ERTS_ALC_T_UNDEF,
							 (void *) a1,
							 (void *) a2,
							 (Uint) a3);
	case 0xf02:
#ifdef USE_THREADS
	    if (((Allctr_t *) a1)->thread_safe)
		erts_alcu_free_ts(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    else
#endif
		erts_alcu_free(ERTS_ALC_T_UNDEF, (void *) a1, (void *) a2);
	    return 0;
	case 0xf03: {
	    Allctr_t *allctr;
	    struct au_init init;

	    SET_DEFAULT_ALLOC_OPTS(&init);
	    init.enable = 1;
	    init.atype = GOODFIT;
	    init.init.util.name_prefix = (char *) a1;
	    init.init.util.ts = a2 ? 1 : 0;

	    if ((char **) a3) {
		char **argv = (char **) a3;
		int i = 0;
		while (argv[i]) {
		    if (argv[i][0] == '-' && argv[i][1] == 't')
			handle_au_arg(&init, &argv[i][2], argv, &i);
		    else
			return (unsigned long) NULL;
		    i++;
		}
	    }

	    switch (init.atype) {
	    case GOODFIT:
		allctr = erts_gfalc_start((GFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
						     sizeof(GFAllctr_t)),
					  &init.init.gf,
					  &init.init.util);
		break;
	    case BESTFIT:
		allctr = erts_bfalc_start((BFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
						     sizeof(BFAllctr_t)),
					  &init.init.bf,
					  &init.init.util);
		break;
	    case AFIT:
		allctr = erts_afalc_start((AFAllctr_t *)
					  erts_alloc(ERTS_ALC_T_UNDEF,
							    sizeof(AFAllctr_t)),
					  &init.init.af,
					  &init.init.util);
		break;
	    default:
		ASSERT(0);
		allctr = NULL;
		break;
	    }

	    return (unsigned long) allctr;
	}
	case 0xf04:
	    erts_alcu_stop((Allctr_t *) a1);
	    erts_free(ERTS_ALC_T_UNDEF, (void *) a1);
	    return (unsigned long) 0;
#ifdef USE_THREADS
	case 0xf05: return (unsigned long) 1;
	case 0xf06: return (unsigned long) ((Allctr_t *) a1)->thread_safe;
#ifndef HAVE_PTHREAD_ATFORK
#define HAVE_PTHREAD_ATFORK 0
#endif
#if defined(POSIX_THREADS) && !HAVE_PTHREAD_ATFORK
	case 0xf07: return (unsigned long) 0;
#else
	case 0xf07: return (unsigned long) ((Allctr_t *) a1)->thread_safe;
#endif
#else /* #ifdef USE_THREADS */
	case 0xf05: return (unsigned long) 0;
	case 0xf06: return (unsigned long) 0;
	case 0xf07: return (unsigned long) 0;
#endif /* #ifdef USE_THREADS */
	case 0xf08: return (unsigned long) erts_mutex_create();
	case 0xf09: return (unsigned long) erts_mutex_destroy((erts_mutex_t) a1);
	case 0xf0a: return (unsigned long) erts_mutex_lock((erts_mutex_t) a1);
	case 0xf0b: return (unsigned long) erts_mutex_unlock((erts_mutex_t) a1);
	case 0xf0c: return (unsigned long) erts_cond_create();
	case 0xf0d: return (unsigned long) erts_cond_destroy((erts_cond_t) a1);
	case 0xf0e: return (unsigned long) erts_cond_broadcast((erts_cond_t) a1);
	case 0xf0f: return (unsigned long) erts_cond_wait((erts_cond_t) a1,
							  (erts_mutex_t) a2);
	case 0xf10: return (unsigned long)
		       erts_thread_create((erts_thread_t *) a1,
					  (void * (*)(void *)) a2,
					  (void *) a3,
					  0);
	case 0xf11: return (unsigned long) erts_thread_join((erts_thread_t) a1,
							    (void **) a2);
	case 0xf12: erts_thread_exit((erts_thread_t) a1); return 0;
	default:
	    break;
	}
    default:
	break;
    }

    ASSERT(0);
    return ~((unsigned long) 0);
}

#ifdef DEBUG
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * Debug stuff                                                               *
\*                                                                           */

#if 0
#define PRINT_OPS
#else
#undef PRINT_OPS
#endif


#define FENCE_SZ		(3*sizeof(Uint))

#ifdef ARCH_64
#define FENCE_PATTERN 0xABCDEF97ABCDEF97
#else
#define FENCE_PATTERN 0xABCDEF97
#endif

#define TYPE_PATTERN_MASK  ERTS_ALC_N_MASK
#define TYPE_PATTERN_SHIFT 16

#define FIXED_FENCE_PATTERN_MASK \
  (~((Uint) (TYPE_PATTERN_MASK << TYPE_PATTERN_SHIFT)))
#define FIXED_FENCE_PATTERN \
  (FENCE_PATTERN & FIXED_FENCE_PATTERN_MASK)

#define MK_PATTERN(T) \
  (FIXED_FENCE_PATTERN | (((T) & TYPE_PATTERN_MASK) << TYPE_PATTERN_SHIFT))

#define GET_TYPE_OF_PATTERN(P) \
  (((P) >> TYPE_PATTERN_SHIFT) & TYPE_PATTERN_MASK)


static void *
set_memory_fence(void *ptr, Uint sz, ErtsAlcType_t n)
{
    Uint *ui_ptr;
    Uint pattern;

    if (!ptr)
	return NULL;

    ui_ptr = (Uint *) ptr;
    pattern = MK_PATTERN(n);
    
    *(ui_ptr++) = sz;
    *(ui_ptr++) = pattern;
    memcpy((void *) (((char *) ui_ptr)+sz), (void *) &pattern, sizeof(Uint));

    return (void *) ui_ptr;
}

static void *
check_memory_fence(void *ptr, Uint *size, ErtsAlcType_t n, int func)
{
    Uint sz;
    Uint found_type;
    Uint pre_pattern;
    Uint post_pattern;
    Uint *ui_ptr;

    if (!ptr)
	return NULL;

    ui_ptr = (Uint *) ptr;
    pre_pattern = *(--ui_ptr);
    *size = sz = *(--ui_ptr);

    found_type = GET_TYPE_OF_PATTERN(pre_pattern);
    if (pre_pattern != MK_PATTERN(n)) {
	if ((FIXED_FENCE_PATTERN_MASK & pre_pattern) != FIXED_FENCE_PATTERN)
	    erl_exit(1,
		     "ERROR: Fence at beginning of memory block (p=0x%u) "
		     "clobbered.\n",
		     (unsigned long) ptr);
    }

    memcpy((void *) &post_pattern, (void *) (((char *)ptr)+sz), sizeof(Uint));

    if (post_pattern != MK_PATTERN(n)
	|| pre_pattern != post_pattern) {
	char fbuf[10];
	char obuf[10];
	char *ftype;
	char *otype;
	char *op_str;

	if ((FIXED_FENCE_PATTERN_MASK & post_pattern) != FIXED_FENCE_PATTERN)
	    erl_exit(1,
		     "ERROR: Fence at end of memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (unsigned long) ptr, (unsigned long) sz);
	if (found_type != GET_TYPE_OF_PATTERN(post_pattern))
	    erl_exit(1,
		     "ERROR: Fence around memory block (p=0x%u, sz=%u) "
		     "clobbered.\n",
		     (unsigned long) ptr, (unsigned long) sz);

	ftype = type_no_str(found_type);
	if (!ftype) {
	    sprintf(fbuf, "%d", (int) found_type);
	    ftype = fbuf;
	}
	otype = type_str(n);
	if (!otype) {
	    sprintf(obuf, "%d", (int) n);
	    otype = obuf;
	}

	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "allocated";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "reallocated";	break;
	case ERTS_ALC_O_FREE:		op_str = "freed";	break;
	default:			op_str = "???";		break;
	}

	erl_exit(1,
		 "ERROR: Memory block (p=0x%u, sz=%u) allocated as type \"%s\","
		 " but %s as type \"%s\".\n",
		 (unsigned long) ptr, (unsigned long) sz, ftype, op_str, otype);
    }

    return (void *) ui_ptr;
}

static ErtsAllocatorFunctions_t real_allctrs[ERTS_ALC_A_MAX+1];

static void *
debug_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint dsize;
    void *res;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);
    dsize = size + FENCE_SZ;
    res = (*real_af->alloc)(n, real_af->extra, dsize);

    res = set_memory_fence(res, size, n);

#ifdef PRINT_OPS
    fprintf(stderr, "0x%lx = alloc(%s, %lu)\r\n",
	    (Uint) res, ERTS_ALC_N2TD(n), size);
#endif

    return res;
}


static void *
debug_realloc(ErtsAlcType_t n, void *extra, void *ptr, Uint size)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    Uint dsize;
    Uint old_size;
    void *dptr;
    void *res;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);

    dsize = size + FENCE_SZ;
    dptr = check_memory_fence(ptr, &old_size, n, ERTS_ALC_O_REALLOC);

    if (old_size > size)
	sys_memset((void *) (((char *) ptr) + size),
		   0xf,
		   sizeof(Uint) + old_size - size);

    res = (*real_af->realloc)(n, real_af->extra, dptr, dsize);
    
    res = set_memory_fence(res, size, n);

#ifdef PRINT_OPS
    fprintf(stderr, "0x%lx = realloc(%s, 0x%lx, %lu)\r\n",
	    (Uint) res, ERTS_ALC_N2TD(n), (Uint) ptr, size);
#endif

    return res;
}

static void
debug_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    ErtsAllocatorFunctions_t *real_af = (ErtsAllocatorFunctions_t *) extra;
    void *dptr;
    Uint size;

    ASSERT(ERTS_ALC_N_MIN <= n && n <= ERTS_ALC_N_MAX);

    dptr = check_memory_fence(ptr, &size, n, ERTS_ALC_O_FREE);

    sys_memset((void *) dptr, n, size + FENCE_SZ);

    (*real_af->free)(n, real_af->extra, dptr);

#ifdef PRINT_OPS
    fprintf(stderr, "free(%s, 0x%lx)\r\n", ERTS_ALC_N2TD(n), (Uint) ptr);
#endif

}

static Uint
install_debug_functions(void)
{
    int i;
    ASSERT(sizeof(erts_allctrs) == sizeof(real_allctrs));

    sys_memcpy((void *)real_allctrs,(void *)erts_allctrs,sizeof(erts_allctrs));

    for (i = ERTS_ALC_A_MIN; i <= ERTS_ALC_A_MAX; i++) {
	erts_allctrs[i].alloc	= debug_alloc;
	erts_allctrs[i].realloc	= debug_realloc;
	erts_allctrs[i].free	= debug_free;
	erts_allctrs[i].extra	= (void *) &real_allctrs[i];
    }
    return FENCE_SZ;
}



#endif /* #ifdef DEBUG */

