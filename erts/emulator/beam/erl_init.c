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
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "erl_version.h"
#include "erl_db.h"
#include "beam_bp.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "dist.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_mode_switch_init() */
extern void hipe_signal_init(void);
#endif

extern void erl_crash_dump();
#ifdef __WIN32__
extern void ConWaitForExit(void);
#endif

int erts_initialized = 0;

/*
 * Configurable parameters.
 */

Uint display_items = 200;	/* no of items to display in traces etc */
Uint display_loads = 0;		/* print info about loaded modules */
int H_MIN_SIZE = H_DEFAULT_SIZE; /* The minimum heap grain */

Uint32 erts_debug_flags;	/* Debug flags. */
int heap_series = HS_FIBONACCI_SLOW; /* Series to use for heap size. */
int count_instructions;
int erts_backtrace_depth = 8;	/* How many functions to show in a backtrace
				 * in error codes.
				 */

int erts_async_max_threads = 0;  /* number of threads for async support */
Uint16 erts_max_gen_gcs = (Uint16) -1;

#ifdef DEBUG
Uint verbose;			/* noisy mode = 1 */
#endif

/*
 * Other global variables.
 */

#ifdef SHARED_HEAP
Eterm *global_heap;
Eterm *global_hend;
Eterm *global_htop;
Eterm *global_saved_htop;
ErlOffHeap erts_global_mso;
Uint   global_heap_sz;
Uint   global_heap_min_sz;

Eterm *global_high_water;
Eterm *global_old_hend;
Eterm *global_old_htop;
Eterm *global_old_heap;
Uint16 global_gen_gcs;
Uint16 global_max_gen_gcs;

Uint   global_gc_flags;
ErlHeapFragment* global_mbuf;
ErlHeapFragment* global_halloc_mbuf;
Uint global_mbuf_sz;
Eterm* erts_global_arith_heap;
Uint erts_global_arith_avail;
Eterm* erts_global_arith_lowest_htop;
#ifdef DEBUG
Eterm* erts_global_arith_check_me;
#endif
#endif

byte* tmp_buf;
Uint tot_bin_allocated = 0;
Uint do_time;			/* set at clock interupt */
Uint garbage_cols;		/* no of garbage collections */
Uint reclaimed;			/* no of words reclaimed in GCs */

#ifdef INSTRUMENT
Uint instr_send_sizes[INSTR_SEND_SIZES_MAX];
#endif

Eterm system_seq_tracer;

void init_emulator(void);

/*
 * Common error printout function, all error messages
 * that don't go to the error logger go through here.
 */

void erl_error(fmt, args)
char *fmt;
va_list args;
{
    vfprintf(stderr, fmt, args);
}

static int
has_prefix(const char *prefix, const char *string)
{
    int i;
    for (i = 0; prefix[i]; i++)
	if (prefix[i] != string[i])
	    return 0;
    return 1;
}

void
erts_short_init(void)
{
    ErtsSlAllocInit sla_init = ERTS_SL_ALLOC_INIT_DEFAULT_INITIALIZER;

    erts_sl_alloc_binaries = ERTS_SL_ALLOC_BINARIES_DEFAULT;

    erts_sl_alloc_init(&sla_init);
    erts_init_definite_alloc(DEFAULT_DEFINITE_ALLOC_BLOCK_SIZE);
    erts_init_utils();

    /* Permanently disable use of mmap for sys_alloc (malloc). */
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, 0);
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, ERTS_DEFAULT_TRIM_THRESHOLD);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, ERTS_DEFAULT_TOP_PAD);

    erl_sys_init();
    erl_init();
}

void
erl_init(void)
{
    garbage_cols = 0;
    reclaimed = 0;
    init_benchmarking();

    ASSERT(TMP_BUF_SIZE >= 16384);
    tmp_buf = (byte *) erts_definite_alloc(TMP_BUF_SIZE);
    if(!tmp_buf)
	tmp_buf = (byte *)safe_alloc_from(130, TMP_BUF_SIZE);
    init_alloc();
    erts_init_gc();

    H_MIN_SIZE = erts_next_heap_size(H_MIN_SIZE, 0);

#ifdef SHARED_HEAP
    global_heap_sz = erts_next_heap_size(H_MIN_SIZE,0);
    global_heap = (Eterm *)
	erts_safe_sl_alloc_from(4711, sizeof(Eterm) * global_heap_sz);
    global_hend = global_heap + global_heap_sz;
    global_htop = global_heap;

    global_old_hend = global_old_htop = global_old_heap = NULL;
    global_high_water = global_heap;
    global_gen_gcs = 0;
    global_max_gen_gcs = erts_max_gen_gcs;

    global_gc_flags = erts_default_process_flags;
    global_mbuf = NULL;
    global_halloc_mbuf = NULL;
    global_mbuf_sz = 0;

    erts_global_arith_heap = NULL;
    erts_global_arith_avail = 0;
    erts_global_arith_lowest_htop = NULL;
#ifdef DEBUG
    erts_global_arith_check_me = NULL;
#endif
#endif

    erts_init_bits();
    erts_init_fun_table();
    init_atom_table();
    init_export_table();
    init_module_table();
    init_register_table();
    init_message();
    init_emulator();
    erts_bp_init();
    init_db(); /* Must be after init_emulator */
    init_scheduler();
    init_time();
    erts_init_node_tables();
    init_dist();
    init_io();
    init_copy();
    init_load();
    erts_init_bif();
    erts_init_trace();
#ifdef HIPE
    hipe_mode_switch_init(); /* Must be after init_load/beam_catches/init */
#endif

#ifdef INSTRUMENT
    {int j;
     for (j=0;j<INSTR_SEND_SIZES_MAX;j++) 
	 instr_send_sizes[j]=0;
 }
#endif
}

/*
 * Create the very first process.
 */

void
erts_first_process(Eterm modname, void* code, unsigned size, int argc, char** argv)
{
    int i;
    Eterm args;
    Eterm pid;
    Eterm* hp;
    Process parent;
    Process* p;
    ErlSpawnOpts so;
    
    if (erts_find_function(modname, am_start, 1) == NULL) {
	char sbuf[256];
	Atom* ap;

	ap = atom_tab(atom_val(modname));
	memcpy(sbuf, ap->name, ap->len);
	sbuf[ap->len] = '\0';
	erl_exit(5, "No function %s:start/1\n", sbuf);
    }

    /*
     * We need a dummy parent process to be able to call erl_create_process().
     */

    erts_init_empty_process(&parent);
#ifdef SHARED_HEAP
    parent.heap_sz = global_heap_sz;
    parent.heap = global_heap;
    parent.hend = global_hend;
    parent.htop = global_htop;
#endif
    hp = HAlloc(&parent, argc*2 + 4);
    args = NIL;
    for (i = argc-1; i >= 0; i--) {
	int len = sys_strlen(argv[i]);
	args = CONS(hp, new_binary(&parent, argv[i], len), args);
	hp += 2;
    }
    args = CONS(hp, new_binary(&parent, code, size), args);
    hp += 2;
    args = CONS(hp, args, NIL);

    so.flags = 0;
    pid = erl_create_process(&parent, modname, am_start, args, &so);
    p = process_tab[internal_pid_index(pid)];
    p->group_leader = pid;
#ifdef SHARED_HEAP
    global_heap = parent.heap;
    global_htop = parent.htop;
    global_hend = parent.hend;
    global_heap_sz = parent.heap_sz;
#endif
    erts_cleanup_empty_process(&parent);
}

/*
 * XXX Old way of starting. Hopefully soon obsolete.
 */

static void
erl_first_process_otp(char* modname, void* code, unsigned size, int argc, char** argv)
{
    int i;
    Eterm start_mod;
    Eterm args;
    Eterm pid;
    Eterm* hp;
    Process parent;
    Process* p;
    ErlSpawnOpts so;
    Eterm env;
    
    start_mod = am_atom_put(modname, sys_strlen(modname));
    if (erts_find_function(start_mod, am_start, 2) == NULL) {
	erl_exit(5, "No function %s:start/2\n", modname);
    }

    /*
     * We need a dummy parent process to be able to call erl_create_process().
     */

    erts_init_empty_process(&parent);
#ifdef SHARED_HEAP
    parent.heap_sz = global_heap_sz;
    parent.heap = global_heap;
    parent.hend = global_hend;
    parent.htop = global_htop;
#endif
    hp = HAlloc(&parent, argc*2 + 4);
    args = NIL;
    for (i = argc-1; i >= 0; i--) {
	int len = sys_strlen(argv[i]);
	args = CONS(hp, new_binary(&parent, argv[i], len), args);
	hp += 2;
    }
    env = new_binary(&parent, code, size);
    args = CONS(hp, args, NIL);
    hp += 2;
    args = CONS(hp, env, args);

    so.flags = 0;
    pid = erl_create_process(&parent, start_mod, am_start, args, &so);
    ASSERT(internal_pid_index(pid) < erts_max_processes);
    p = process_tab[internal_pid_index(pid)];
    p->group_leader = pid; /* internal pid */
#ifdef SHARED_HEAP
    global_heap = parent.heap;
    global_htop = parent.htop;
    global_hend = parent.hend;
    global_heap_sz = parent.heap_sz;
#endif
    erts_cleanup_empty_process(&parent);
}

Eterm
erts_preloaded(Process* p)
{
    Eterm previous;
    int j;
    int need;
    Eterm mod;
    Eterm* hp;
    char* name;
    const Preload *preload = sys_preloaded();

    j = 0;
    while (preload[j].name != NULL) {
	j++;
    }
    previous = NIL;
    need = 2*j;
    hp = HAlloc(p, need);
    j = 0;
    while ((name = preload[j].name) != NULL)  {
	mod = am_atom_put(name, sys_strlen(name));
	previous = CONS(hp, mod, previous);
	hp += 2;
	j++;
    }
    return previous;
}


static void usage(void);


/* static variables that must not change (use same values at restart) */
static char* program;
static char* init = "init";
static char* boot = "boot";
static int    boot_argc;
static char** boot_argv;

static char* get_arg(rest, next, ip)
char* rest; char* next; int* ip;
{
    if (*rest == '\0') {
	if (next == NULL) {
	    erl_printf(CERR, "too few arguments\n");
	    usage();
	}
	(*ip)++;
	return next;
    }
    return rest;
}

static void 
load_preloaded(void)
{
    int i;
    int res;
    Preload* preload_p;
    Eterm module_name;
    byte* code;
    char* name;
    int length;

    if ((preload_p = sys_preloaded()) == NULL) {
	return;
    }
    i = 0;
    while ((name = preload_p[i].name) != NULL) {
	length = preload_p[i].size;
	module_name = am_atom_put(name, sys_strlen(name));
	if ((code = sys_preload_begin(&preload_p[i])) == 0)
	    erl_exit(1, "Failed to find preloaded code for module %s\n", 
		     name);
	res = erts_load_module(NIL, &module_name, code, length);
	sys_preload_end(&preload_p[i]);
	if (res < 0)
	    erl_exit(1,"Failed loading preloaded module %s\n", name);
	i++;
    }
}

/* be helpful (or maybe downright rude:-) */
static void usage()
{
    erl_printf(CERR, "usage: %s [flags] [ -- [init_args] ]\n", program);
    erl_printf(CERR, "The flags are:\n\n");
    erl_printf(CERR, "-v         turn on chatty mode, GCs will be reported etc\n");
    erl_printf(CERR, "-l         turn on auto load tracing\n");
    erl_printf(CERR, "-i module  set the boot module (default init)\n");
    erl_printf(CERR, "-b fun     set the boot function (default boot)\n");
    erl_printf(CERR, "-h number  set minimum heap size in words (default %d)\n",
	       H_DEFAULT_SIZE);
    erl_printf(CERR, "-# number  set the number of items to be used in traces etc\n");
    erl_printf(CERR, "-B         turn break handler off\n");
    erl_printf(CERR, "-P number  set maximum number of processes on this node\n");
    erl_printf(CERR, "           valid range is [%d-%d]\n",
	       ERTS_MIN_PROCESSES, ERTS_MAX_PROCESSES);
    erl_printf(CERR, "-A number  set number of threads in async thread pool\n");
    erl_printf(CERR, "           valid range is [0-256]\n");
    erl_printf(CERR, "-t number  set trim threshold (Kb)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-T number  set top pad\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-Se bool   enable sl_alloc\n");
    erl_printf(CERR, "           valid values are true | false\n");
    erl_printf(CERR, "-Sr number enable a specific sl_alloc release\n");
    erl_printf(CERR, "           valid releases are %s | %s\n",
	       ERTS_OLD_SL_ALLOC_RELEASE, ERTS_SL_ALLOC_RELEASE);
    erl_printf(CERR, "-Ssbct num set singleblock carrier threshold "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-Smmc numb set max mmap carriers (sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX);
    erl_printf(CERR, "-Ssbcmt nu set singleblock carrier move threshold "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-100]\n");
    erl_printf(CERR, "-Smsbclt n set mmap singleblock carrier load threshold "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-100]\n");
    erl_printf(CERR, "-Smcs numb set main carrier size (sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-Sscs numb set smallest (multiblock) carrier size "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-Slcs numb set largest (multiblock) carrier size "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-Smbcgs nu set (multiblock) carrier growth stages "
	       "(sl_alloc)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX);
    erl_printf(CERR, "-Smbsd num set max block search depth (sl_alloc)\n");
    erl_printf(CERR, "           valid range is [1-%d]\n", INT_MAX);
    erl_printf(CERR, "-Sb bool   sl_alloc:ate binaries\n");
    erl_printf(CERR, "           valid values are true | false\n");
    erl_printf(CERR, "\n\n");
    erl_exit(-1, "");
}

extern void elib_ensure_initialized(void);

void
erl_start(int argc, char **argv)
{
    int i = 1;
    char* arg=NULL;
    int have_break_handler = 1;
    char* tmpenvbuf;
    int trim_threshold = ERTS_DEFAULT_TRIM_THRESHOLD;
    int top_pad = ERTS_DEFAULT_TOP_PAD;
    Uint definite_block_size = DEFAULT_DEFINITE_ALLOC_BLOCK_SIZE;
    ErtsSlAllocInit sl_alloc_init = ERTS_SL_ALLOC_INIT_DEFAULT_INITIALIZER;

    elib_ensure_initialized();

    erts_initialized = 0;
    program = argv[0];

    /* First find out how to initialize sl_alloc. */

    erts_sl_alloc_binaries = ERTS_SL_ALLOC_BINARIES_DEFAULT;

    while (i < argc) {
	if (strcmp(argv[i], "--") == 0)
	    break;
	if(argv[i][0] == '-' && argv[i][1] == 'S') {
	    char *rest;
	    int tmp;

	    if (has_prefix("r", argv[i] + 2)) {
		/* Set release to use */
		arg = get_arg(argv[i]+3, argv[i+1], &i);
		if (strcmp(arg, ERTS_OLD_SL_ALLOC_VERSION) == 0
		    || strcmp(arg, ERTS_OLD_SL_ALLOC_RELEASE) == 0)
		    sl_alloc_init.eosla = 1;
		else if (strcmp(arg, ERTS_SL_ALLOC_VERSION) == 0
			 || strcmp(arg, ERTS_SL_ALLOC_RELEASE) == 0)
		    sl_alloc_init.eosla = 0;
		else {
		    erl_printf(CERR, "bad sl_alloc release: %s\n", arg);
		    usage();
		}
	    }
	    else if (has_prefix("e", argv[i] + 2)) {
		arg = get_arg(argv[i]+3, argv[i+1], &i);
		if (strcmp(arg, "true") == 0)
		    sl_alloc_init.esla = 1;
		else if (strcmp(arg, "false") == 0)
		    sl_alloc_init.esla = 0;
		else {
		    erl_printf(CERR, "bad sl_alloc enable: %s\n", arg);
		    usage();
		}
	    }
	    else if (has_prefix("b", argv[i] + 2)) {
		arg = get_arg(argv[i]+3, argv[i+1], &i);
		if (strcmp(arg, "true") == 0)
		    erts_sl_alloc_binaries = 1;
		else if (strcmp(arg, "false") == 0)
		    erts_sl_alloc_binaries = 0;
		else {
		    erl_printf(CERR, "bad sl_alloc binaries: %s\n", arg);
		    usage();
		}
	    }
	    else if (has_prefix("mmc", argv[i] + 2)) {
		/* Set max mmap carriers */
		arg = get_arg(argv[i]+5, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 || (INT_MAX) < tmp) {
		    erl_printf(CERR, "bad max mmap carriers: %s\n", arg);
		    usage();
		}
		sl_alloc_init.mmc = tmp;
	    }
	    else if (has_prefix("mcs", argv[i] + 2)) {
		/* Set main carrier size */
		arg = get_arg(argv[i]+5, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 ||
		    (INT_MAX/1024) < tmp) {
		    erl_printf(CERR, "bad main carrier size: %s\n", arg);
		    usage();
		}
		sl_alloc_init.mcs = tmp * 1024;
	    }
	    else if (has_prefix("scs", argv[i] + 2)) {
		/* Set smallest carrier size */
		arg = get_arg(argv[i]+5, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 ||
		    (INT_MAX/1024) < tmp) {
		    erl_printf(CERR, "bad smallest carrier size: %s\n", arg);
		    usage();
		}
		sl_alloc_init.scs = tmp * 1024;
	    }
	    else if (has_prefix("lcs", argv[i] + 2)) {
		/* Set largest carrier size */
		arg = get_arg(argv[i]+5, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 ||
		    (INT_MAX/1024) < tmp) {
		    erl_printf(CERR, "bad largest carrier size: %s\n", arg);
		    usage();
		}
		sl_alloc_init.lcs = tmp * 1024;
	    }
	    else if (has_prefix("mbcgs", argv[i] + 2)) {
		/* Set carrier growth stages */
		arg = get_arg(argv[i]+7, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0) {
		    erl_printf(CERR, "bad carrier growth stages: %s\n", arg);
		    usage();
		}
		sl_alloc_init.mbcgs = tmp;
	    }
	    else if(has_prefix("sbct", argv[i] + 2)) {
		/* Set singleblock carrier threshold */
		arg = get_arg(argv[i]+6, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 ||
		    (INT_MAX/1024) < tmp) {
		    erl_printf(CERR, "bad singleblock carrier threshold: %s\n",
			       arg);
		    usage();
		}
		sl_alloc_init.sbct = tmp * 1024;
	    }
	    else if (has_prefix("mbsd", argv[i] + 2)) {
		/* Set max block search depth */
		arg = get_arg(argv[i]+6, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 1 ||
		    (INT_MAX) < tmp) {
		    erl_printf(CERR, "bad max block search depth: %s\n", arg);
		    usage();
		}
		sl_alloc_init.mbsd = tmp;
	    }
	    else if(has_prefix("sbcmt", argv[i] + 2)) {
		/* Set singleblock carrier move threshold */
		arg = get_arg(argv[i]+7, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 || 100 < tmp) {
		    erl_printf(CERR, "bad singleblock carrier move "
			       "threshold: %s\n", arg);
		    usage();
		}
		sl_alloc_init.sbcmt = tmp;
	    }
	    else if(has_prefix("msbclt", argv[i] + 2)) {
		/* Set singleblock carrier move threshold */
		arg = get_arg(argv[i]+8, argv[i+1], &i);
		errno = 0;
		tmp = (int) strtol(arg, &rest, 10);
		if (errno != 0 || rest == arg || tmp < 0 || 100 < tmp) {
		    erl_printf(CERR, "bad mmap singleblock carrier load "
			       "threshold: %s\n", arg);
		    usage();
		}
		sl_alloc_init.msbclt = tmp;
	    }
	    else if (argv[i][2] == '\0') {
		erl_printf(CERR,
			      "\"-S\" is deprecated and will soon be removed; "
			   "use \"-Se false\" instead\n");
		sl_alloc_init.esla = 0;
	    }
	    else {
		erl_printf(CERR, "bad sl_alloc parameter: %s\n", argv[i]);
		usage();
	    }
	}
	i++;
    }

    /* Observe that erts_sl_alloc_init() has to be called
       before any other erts_sl_alloc*() functions are called and
       before any threads other than the initial thread
       have been created. */
    erts_sl_alloc_init(&sl_alloc_init);
    i = 1;

    erts_init_utils();
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, top_pad);
    /* Permanently disable use of mmap for sys_alloc (malloc). */
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, 0);

#if defined(HIPE) && defined(__i386__)
    hipe_signal_init();	/* must be done very early */
#endif
    erl_sys_init();
    erl_sys_args(&argc, argv);

    tmpenvbuf = getenv(ERL_MAX_ETS_TABLES_ENV);
    if (tmpenvbuf != NULL) 
	user_requested_db_max_tabs = atoi(tmpenvbuf);
    else
	user_requested_db_max_tabs = 0;

    tmpenvbuf = getenv("ERL_FULLSWEEP_AFTER");
    if (tmpenvbuf != NULL) {
	erts_max_gen_gcs = atoi(tmpenvbuf);
    }

    tmpenvbuf = getenv("ERL_THREAD_POOL_SIZE");
    if (tmpenvbuf != NULL) {
	erts_async_max_threads = atoi(tmpenvbuf);
    }
    

#ifdef DEBUG
    verbose = 0;
#endif

    system_seq_tracer = NIL;

    while (i < argc) {
	if (argv[i][0] != '-') {
	    usage();
	}
	if (strcmp(argv[i], "--") == 0) { /* end of emulator options */
	    i++;
	    break;
	}
	switch (argv[i][1]) {
	case '#' :
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((display_items = atoi(arg)) == 0) {
		erl_printf(CERR, "bad display items%s\n", arg);
		usage();
	    }
	    VERBOSE(erl_printf(COUT,"using display items %d\n",
			       display_items););
	    break;

	case 'l':
	    display_loads++;
	    break;
	    
	case 'v':
#ifdef DEBUG
	    verbose++;
#else
	    erl_printf(CERR, "warning: -v (only in debug compiled code)\n");
#endif
	    break;
	case 'V' :
	    {
		char tmp[256];

		tmp[0] = tmp[1] = '\0';
#ifdef DEBUG
		strcat(tmp, ",DEBUG");
#endif
#ifdef INSTRUMENT
		strcat(tmp, ",INSTRUMENT");
#endif
#ifdef USE_THREADS
		strcat(tmp, ",THREADS");
#endif
#ifdef HIPE
		strcat(tmp, ",HIPE");
#endif
#ifdef SHARED_HEAP
                strcat(tmp, ",SHARED_HEAP");
#endif
		erl_printf(CERR, "Erlang ");
		if (tmp[1]) {
		    erl_printf(CERR, "(%s) ", tmp+1);
		}
		erl_printf(CERR, "(" EMULATOR ") emulator version "
			   ERLANG_VERSION "\n");
		erl_exit(0, "");
	    }
	    break;

	case 'H':
	    if (argv[i][2] == 'f') {
		heap_series = HS_FIBONACCI;
	    } else if (argv[i][2] == 'F') {
		heap_series = HS_FIBONACCI_SLOW;
	    } else if (argv[i][2] == 'T') {
		heap_series = HS_POWER_TWO;
	    } else if (argv[i][2] == 't') {
		heap_series = HS_POWER_TWO_MINUS_ONE;
	    } else {
		erl_printf(CERR, "bad heap series %s (use 'f', 'T', or 't')\n", arg);
		usage();
	    }
	    break;

	case 'h':
	    /* set default heap size */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((H_MIN_SIZE = atoi(arg)) <= 0) {
		erl_printf(CERR, "bad heap size %s\n", arg);
		usage();
	    }
	    VERBOSE(erl_printf(COUT, "using minimum heap size %d\n",
			       H_MIN_SIZE););
	    break;

	case 'e':
	    /* set maximum number of ets tables */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (( user_requested_db_max_tabs = atoi(arg) ) < 0) {
		erl_printf(CERR, "bad maximum number of ets tables %s\n", arg);
		usage();
	    }
	    VERBOSE(erl_printf(COUT, "using maximum number of ets tables %d\n",
			       user_requested_db_max_tabs););
	    break;

	case 'i':
	    /* define name of module for initial function */
	    init = get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 'b':
	    /* define name of initial function */
	    boot = get_arg(argv[i]+2, argv[i+1], &i);
	    break;

	case 'B':
	    have_break_handler = 0;
	    break;

	case 'P':
	    /* set maximum number of processes */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (((erts_max_processes = atoi(arg)) < ERTS_MIN_PROCESSES)
		|| erts_max_processes > ERTS_MAX_PROCESSES) {
		erl_printf(CERR, "bad number of processes %s\n", arg);
		usage();
	    }
	    break;

	case 'd': {
	    char *rest;
	    int tmp;
	    /* set definite alloc block size */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    errno = 0;
	    tmp = (int) strtol(arg, &rest, 10);
	    if (errno != 0 || rest == arg || tmp < 0 || (INT_MAX/1024) < tmp) {
		erl_printf(CERR, "bad definite block size: %s\n", arg);
		usage();
	    }
	    definite_block_size = (Uint) tmp;
	    definite_block_size *= 1024;
	    break;
	}

	case 'A':
	    /* set number of threads in thread pool */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (((erts_async_max_threads = atoi(arg)) < -1) ||
		(erts_async_max_threads > 256)) {
		erl_printf(CERR, "bad number of threads %s\n", arg);
		usage();
	    }
	    break;

	case 't': {
	  char *rest;
	  /* set trim threshold */
	  arg = get_arg(argv[i]+2, argv[i+1], &i);
	  errno = 0;
	  trim_threshold = (int) strtol(arg, &rest, 10);
	  if (errno != 0
	      || rest == arg
	      || trim_threshold < 0
	      || (INT_MAX/1024) < trim_threshold) {
	    trim_threshold = ERTS_DEFAULT_TRIM_THRESHOLD/1024;
	    erl_printf(CERR,
		       "bad trim threshold: %s; using default: %d\n",
		       arg,
		       trim_threshold);
	  }
	  VERBOSE(erl_printf(COUT, "using trim threshold: %d\n",
			     trim_threshold););
	  trim_threshold *= 1024;
	  break;
        }

	case 'T': {
	  char *rest;
	  /* set top pad */
	  arg = get_arg(argv[i]+2, argv[i+1], &i);
	  errno = 0;
	  top_pad = (int) strtol(arg, &rest, 10);
	  if (errno != 0
	      || rest == arg
	      || top_pad < 0
	      || (INT_MAX/1024) < top_pad) {
	    top_pad = ERTS_DEFAULT_TOP_PAD/1024;
	    erl_printf(CERR,
		       "bad top pad: %s; using default: %d\n",
		       arg,
		       top_pad);
	  }
	  VERBOSE(erl_printf(COUT, "using top pad: %d\n",
			     top_pad););
	  top_pad *= 1024;
	  break;
        }

	case 'm':
	    erl_printf(CERR, "\"m\" is no longer a valid parameter\n");
	    usage();
	  break;

	case 'M':
	    erl_printf(CERR, "\"M\" is no longer a valid parameter\n");
	    usage();
	  break;

	case 'S':
	    /* Already handled (sl_alloc options). */
	    if (argv[i+1][0] != '-')
		i++;
	    break;

	case 'n':   /* XXX obsolete */
	    break;
	case 'c':
	    if (argv[i][2] == 'i') {
		count_instructions = 1;
	    }
	    break;
	default:
	    erl_printf(CERR, "%s unknown flag %s\n", argv[0], argv[i]);
	    usage();
	}
	i++;
    }

   /* Restart will not reinstall the break handler */
    if (have_break_handler) {
	init_break_handler();
    }

    boot_argc = argc - i;  /* Number of arguments to init */
    boot_argv = &argv[i];

    erts_init_definite_alloc(definite_block_size);
    erl_init();
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, top_pad);

    load_preloaded();
    
    erts_initialized = 1;

    erl_first_process_otp("otp_ring0", NULL, 0, boot_argc, boot_argv);
    process_main();
}


/*
 * Common exit function, all exits from the system go through here.
 * n <= 0 -> normal exit with status n;
 * n = 127 -> Erlang crash dump produced, exit with status 1;
 * other positive n -> Erlang crash dump and core dump produced.
 */

void erl_exit0(char *file, int line, int n, char *fmt,...)
{
    va_list args;

    va_start(args, fmt);

    save_statistics();

#ifdef SHARED_HEAP
    if (global_heap) erts_sl_free((void*)global_heap);
    global_heap = NULL;

    {
      ErlHeapFragment *tmp;
      while (global_mbuf != NULL) {
        tmp = global_mbuf->next;
        free_message_buffer(global_mbuf);
        global_mbuf = tmp;
      }
    }
#endif

    /* Produce an Erlang core dump if error */
    if(n > 0) erl_crash_dump(file,line,fmt,args); 

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif
    if (n == 127)
        exit(1);
    else if (n > 0)
        abort();
    else
        exit(abs(n));
}

void erl_exit(int n, char *fmt,...)
{
    va_list args;

    va_start(args, fmt);

    save_statistics();

#ifdef SHARED_HEAP
    if (global_heap) erts_sl_free((void*)global_heap);
    global_heap = NULL;

    {
      ErlHeapFragment *tmp;
      while (global_mbuf != NULL) {
        tmp = global_mbuf->next;
        free_message_buffer(global_mbuf);
        global_mbuf = tmp;
      }
    }
#endif

    /* Produce an Erlang core dump if error */
    if(n > 0) erl_crash_dump((char*) NULL,0,fmt,args); 

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif
    if (n == 127)
        exit(1);
    else if (n > 0)
        abort();
    else
        exit(abs(n));
}
