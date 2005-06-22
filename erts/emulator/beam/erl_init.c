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
#include <ctype.h>
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_version.h"
#include "erl_db.h"
#include "beam_bp.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "dist.h"
#include "erl_mseg.h"
#include "erl_nmgc.h"
#include "erl_threads.h"

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_mode_switch_init() */
#include "hipe_signal.h"	/* for hipe_signal_init() */
#endif

/*
 * Note about VxWorks: All variables must be initialized by executable code,
 * not by an initializer. Otherwise a new instance of the emulator will
 * inherit previous values.
 */

extern void erl_crash_dump_v(char *, int, char *, va_list);
#ifdef __WIN32__
extern void ConNormalExit(void);
extern void ConWaitForExit(void);
#endif

#define ERTS_MIN_COMPAT_REL 7

volatile int erts_writing_erl_crash_dump = 0;
int erts_initialized = 0;

static ethr_tid main_thread;

/*
 * Configurable parameters.
 */

Uint display_items;	    /* no of items to display in traces etc */
Uint display_loads;		/* print info about loaded modules */
int H_MIN_SIZE;			/* The minimum heap grain */

Uint32 erts_debug_flags;	/* Debug flags. */
int count_instructions;
int erts_backtrace_depth;	/* How many functions to show in a backtrace
				 * in error codes.
				 */

int erts_async_max_threads;  /* number of threads for async support */
Uint16 erts_max_gen_gcs = (Uint16) -1;

Eterm erts_error_logger_warnings; /* What to map warning logs to, am_error, 
				     am_info or am_warning, am_error is 
				     the default for BC */

int erts_compat_rel;

#ifdef DEBUG
verbose_level verbose;     /* See erl_debug.h for information about verbose */
#endif

int erts_disable_tolerant_timeofday; /* Time correction can be disabled it is
				      * not and/or it is too slow.
				      */


/*
 * Other global variables.
 */

int erts_use_r9_pids_ports;

#if defined(SHARED_HEAP) || defined(HYBRID)
Eterm *global_heap;
Eterm *global_hend;
Eterm *global_htop;
Eterm *global_saved_htop;
ErlOffHeap erts_global_offheap;
Uint   global_heap_sz = SH_DEFAULT_SIZE;

#ifndef INCREMENTAL_GC
Eterm *global_high_water;
#endif

#ifndef NOMOVE
Eterm *global_old_hend;
Eterm *global_old_htop;
Eterm *global_old_heap;
#endif

Uint16 global_gen_gcs;
Uint16 global_max_gen_gcs;
Uint   global_gc_flags;
#endif

#ifdef SHARED_HEAP
ErlHeapFragment *global_mbuf;
ErlHeapFragment *global_halloc_mbuf;
Uint   global_mbuf_sz;
Eterm *erts_global_arith_heap;
Uint   erts_global_arith_avail;
Eterm *erts_global_arith_lowest_htop;
#ifdef DEBUG
Eterm *erts_global_arith_check_me;
#endif
#endif

#ifdef HYBRID
Uint   global_heap_min_sz = SH_DEFAULT_SIZE;
#endif

byte* tmp_buf;
Uint do_time;			/* set at clock interupt */
Uint garbage_cols;		/* no of garbage collections */
Uint reclaimed;			/* no of words reclaimed in GCs */

Eterm system_seq_tracer;

int ignore_break;
int replace_intr;

static char*
progname(char *fullname) 
{
    int i;
    
    i = strlen(fullname);
    while (i >= 0) {
	if ((fullname[i] != '/') && (fullname[i] != '\\')) 
	    i--;
	else 
	    break;
    }
    return fullname+i+1;
}

static int
this_rel_num(void)
{
    static int this_rel = -1;

    if (this_rel < 1) {
	int i;
	char this_rel_str[] = ERLANG_OTP_RELEASE;
	    
	i = 0;
	while (this_rel_str[i] && !isdigit((int) this_rel_str[i]))
	    i++;
	this_rel = atoi(&this_rel_str[i]); 
	if (this_rel < 1)
	    erl_exit(-1, "Unexpected ERLANG_OTP_RELEASE format\n");
    }
    return this_rel;
}

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

void
erts_short_init(void)
{
    erts_initialized = 0;
    erts_writing_erl_crash_dump = 0;

    erts_compat_rel = this_rel_num();

    erts_use_r9_pids_ports = 0;

    erts_sys_pre_init();
    main_thread = erts_thr_self();
    erts_alloc_init(NULL, NULL);
    erts_init_utils();
    erl_sys_init();
    erl_init();
    erts_initialized = 1;
}

void
erl_init(void)
{
    init_benchmarking();

    ASSERT(TMP_BUF_SIZE >= 16384);
    tmp_buf = (byte *) erts_alloc(ERTS_ALC_T_TMP_BUF, TMP_BUF_SIZE);

    erts_init_gc();
    init_scheduler();

    H_MIN_SIZE = erts_next_heap_size(H_MIN_SIZE, 0);

    erts_bif_info_init();
    erts_init_binary();
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
    init_time();
    erts_init_node_tables();
    init_dist();
    init_io();
    init_copy();
    init_load();
    erts_init_bif();
    erts_init_trace();
    erts_init_obsolete();
#if HAVE_ERTS_MSEG
    erts_mseg_late_init(); /* Must be after timer (init_time()) and thread
			      initializations */
#endif
#ifdef HIPE
    hipe_mode_switch_init(); /* Must be after init_load/beam_catches/init */
#endif
#ifdef _OSE_
    erl_sys_init_final();
#endif
}

static void
init_shared_memory(int argc, char **argv)
{
#if defined(SHARED_HEAP) || defined(HYBRID)
    int arg_size = 0;

    global_heap_sz = erts_next_heap_size(global_heap_sz,0);

    /* Make sure arguments will fit on the heap, no one else will check! */
    while (argc--)
        arg_size += 2 + strlen(argv[argc]);
    if (global_heap_sz < arg_size)
        global_heap_sz = erts_next_heap_size(arg_size,1);

    global_heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
					    sizeof(Eterm) * global_heap_sz);
    global_hend = global_heap + global_heap_sz;
    global_htop = global_heap;

#ifndef INCREMENTAL_GC
    global_high_water = global_heap;
#endif
    global_gen_gcs = 0;
    global_max_gen_gcs = erts_max_gen_gcs;
    global_gc_flags = erts_default_process_flags;

#ifndef NOMOVE
    global_old_hend = global_old_htop = global_old_heap = NULL;
#endif
#endif

#ifdef SHARED_HEAP
    global_mbuf = NULL;
    global_mbuf_sz = 0;
    global_halloc_mbuf = NULL;
    erts_global_arith_heap = NULL;
    erts_global_arith_avail = 0;
    erts_global_arith_lowest_htop = NULL;
#ifdef DEBUG
    erts_global_arith_check_me = NULL;
#endif
#endif

#ifdef HYBRID
    erts_global_offheap.mso = NULL;
#ifndef HYBRID /* FIND ME! */
    erts_global_offheap.funs = NULL;
#endif
    erts_global_offheap.overhead = 0;
#endif

#ifdef NOMOVE
    erts_init_nmgc();
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
#if defined(SHARED_HEAP)
    parent.heap_sz = global_heap_sz;
    parent.heap = global_heap;
    parent.hend = global_hend;
    parent.htop = global_htop;
    parent.stop = global_hend;
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
#if defined(SHARED_HEAP)
    global_heap = parent.heap;
    global_htop = parent.htop;
    global_hend = parent.hend;
    global_heap_sz = parent.heap_sz;
#endif

    so.flags = 0;
    pid = erl_create_process(&parent, modname, am_start, args, &so);
    p = process_tab[internal_pid_index(pid)];
    p->group_leader = pid;

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
#if defined(SHARED_HEAP)
    parent.heap_sz = global_heap_sz;
    parent.heap = global_heap;
    parent.hend = global_hend;
    parent.htop = global_htop;
    parent.stop = global_hend;
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
#if defined(SHARED_HEAP)
    global_heap = parent.heap;
    global_htop = parent.htop;
    global_hend = parent.hend;
    global_heap_sz = parent.heap_sz;
#endif

    so.flags = 0;
    pid = erl_create_process(&parent, start_mod, am_start, args, &so);
    ASSERT(internal_pid_index(pid) < erts_max_processes);
    p = process_tab[internal_pid_index(pid)];
    p->group_leader = pid; /* internal pid */
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


/* static variables that must not change (use same values at restart) */
static char* program;
static char* init = "init";
static char* boot = "boot";
static int    boot_argc;
static char** boot_argv;

static char *
get_arg(char* rest, char* next, int* ip)
{
    if (*rest == '\0') {
	if (next == NULL) {
	    erl_printf(CERR, "too few arguments\n");
	    erts_usage();
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
void erts_usage(void)
{
    erl_printf(CERR, "Usage: %s [flags] [ -- [init_args] ]\n", progname(program));
    erl_printf(CERR, "The flags are:\n\n");

    /*    erl_printf(CERR, "-# number  set the number of items to be used in traces etc\n"); */

    erl_printf(CERR, "-A number  set number of threads in async thread pool,\n");
    erl_printf(CERR, "           valid range is [0-256]\n");

    erl_printf(CERR, "-B[c|d|i]  c to have Ctrl-c interrupt the Erlang shell,\n");
    erl_printf(CERR, "           d (or no extra option) to disable the break\n");
    erl_printf(CERR, "           handler, i to ignore break signals\n");

    /*    erl_printf(CERR, "-b func    set the boot function (default boot)\n"); */

    erl_printf(CERR, "-c         disable continuous date/time correction with\n");
    erl_printf(CERR, "           respect to uptime\n");
#ifdef SHARED_HEAP
    erl_printf(CERR, "-h number  set minimum heap size in words (default %d)\n",
	       SH_DEFAULT_SIZE);
#else
    erl_printf(CERR, "-h number  set minimum heap size in words (default %d)\n",
	       H_DEFAULT_SIZE);
#endif

    /*    erl_printf(CERR, "-i module  set the boot module (default init)\n"); */

    erl_printf(CERR, "-K boolean enable or disable kernel poll\n");

    erl_printf(CERR, "-l         turn on auto load tracing\n");

    erl_printf(CERR, "-M<X> <Y>  memory allocator switches,\n");
    erl_printf(CERR, "           see the erts_alloc(3) man page for more info.\n");

    erl_printf(CERR, "-P number  set maximum number of processes on this node,\n");
    erl_printf(CERR, "           valid range is [%d-%d]\n",
	       ERTS_MIN_PROCESSES, ERTS_MAX_PROCESSES);
    erl_printf(CERR, "-R number  set compatibility release number,\n");
    erl_printf(CERR, "           valid range [%d-%d]\n",
	       ERTS_MIN_COMPAT_REL, this_rel_num());

    erl_printf(CERR, "-r         force ets memory block to be moved on realloc\n");

    erl_printf(CERR, "-V         print Erlang version\n");

    erl_printf(CERR, "-v         turn on chatty mode (GCs will be reported etc)\n");

    erl_printf(CERR, "-W<i|w>    set error logger warnings mapping,\n");
    erl_printf(CERR, "           see error_logger documentation for details\n");

    erl_printf(CERR, "\n");
    erl_printf(CERR, "Note that if the emulator is started with erlexec (typically\n");
    erl_printf(CERR, "from the erl script), these flags should be specified with +.\n");
    erl_printf(CERR, "\n\n");
    erl_exit(-1, "");
}

void
erl_start(int argc, char **argv)
{
    int i = 1;
    char* arg=NULL;
    char* Parg = NULL;
    int have_break_handler = 1;
    char* tmpenvbuf;

    erts_disable_tolerant_timeofday = 0;
    display_items = 200;
    display_loads = 0;
    erts_backtrace_depth = DEFAULT_BACKTRACE_SIZE;
    erts_async_max_threads = 0;
    erts_max_gen_gcs = (Uint16) -1;
    H_MIN_SIZE = H_DEFAULT_SIZE;
    garbage_cols = 0;
    reclaimed = 0;

    erts_writing_erl_crash_dump = 0;
    erts_initialized = 0;
    ignore_break = 0;
    replace_intr = 0;
    program = argv[0];

    erts_compat_rel = this_rel_num();

    erts_use_r9_pids_ports = 0;

    erts_sys_pre_init();
    main_thread = erts_thr_self();

    erts_alloc_init(&argc, argv); /* Handles (and removes) -M flags */

    erts_init_utils();

#if defined(HIPE)
    hipe_signal_init();	/* must be done very early */
#endif
    erl_sys_init();
    erl_sys_args(&argc, argv);

    erts_ets_realloc_always_moves = 0;

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
    verbose = VERBOSE_SILENT;
#endif

    erts_error_logger_warnings = am_error;
    system_seq_tracer = NIL;

    while (i < argc) {
	if (argv[i][0] != '-') {
	    erts_usage();
	}
	if (strcmp(argv[i], "--") == 0) { /* end of emulator options */
	    i++;
	    break;
	}
	switch (argv[i][1]) {

	    /*
	     * NOTE: -M flags are handled (and removed from argv) by
	     * erts_alloc_init(). 
	     *
	     * The -d, -m, -S, -t, and -T flags was removed in
	     * Erlang 5.3/OTP R9C.
	     */

	case '#' :
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((display_items = atoi(arg)) == 0) {
		erl_printf(CERR, "bad display items%s\n", arg);
		erts_usage();
	    }
	    VERBOSE_MESSAGE((VERBOSE_CHATTY,"using display items %d\n",
			     display_items));
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
#ifdef USE_THREADS
		strcat(tmp, ",THREADS");
#endif
#ifdef HIPE
		strcat(tmp, ",HIPE");
#endif
#ifdef SHARED_HEAP
                strcat(tmp, ",SHARED_HEAP");
#endif
#ifdef HYBRID
                strcat(tmp, ",HYBRID");
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

	case 'H':		/* undocumented */
	    fprintf(stderr, "The undocumented +H option has been removed (R10B-6).\n\n");
	    break;

	case 'h':
	    /* set default heap size */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if ((H_MIN_SIZE = atoi(arg)) <= 0) {
		erl_printf(CERR, "bad heap size %s\n", arg);
		erts_usage();
	    }
#ifdef SHARED_HEAP
            global_heap_sz = H_MIN_SIZE;
#endif
	    VERBOSE_MESSAGE((VERBOSE_CHATTY, "using minimum heap size %d\n",
			     H_MIN_SIZE));
	    break;

	case 'e':
	    /* set maximum number of ets tables */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (( user_requested_db_max_tabs = atoi(arg) ) < 0) {
		erl_printf(CERR, "bad maximum number of ets tables %s\n", arg);
		erts_usage();
	    }
	    VERBOSE_MESSAGE((VERBOSE_CHATTY, "using maximum number of ets tables %d\n",
			     user_requested_db_max_tabs));
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
	  if (argv[i][2] == 'i')          /* +Bi */
	    ignore_break = 1;
	  else if (argv[i][2] == 'c')     /* +Bc */
	    replace_intr = 1;
	  else if (argv[i][2] == 'd')     /* +Bd */
	    have_break_handler = 0;
	  else if (argv[i+1][0] == 'i') { /* +B i */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    ignore_break = 1;
	  }
	  else if (argv[i+1][0] == 'c') { /* +B c */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    replace_intr = 1;
	  }
	  else if (argv[i+1][0] == 'd') { /* +B d */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    have_break_handler = 0;
	  }
	  else			          /* +B */
	    have_break_handler = 0;
	  break;

	case 'K':
	    /* If kernel poll support is present,
	       erl_sys_args() will remove the K parameter
	       and value */
	    get_arg(argv[i]+2, argv[i+1], &i);
	    erl_printf(CERR,
		       "kernel-poll not supported; \"K\" parameter ignored\n",
		       arg);
	    break;

	case 'P':
	    /* set maximum number of processes */
	    Parg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_max_processes = atoi(Parg);
	    /* Check of result is delayed until later. This because +p
	       may be given after +P. */
	    break;

	case 'R': {
	    /* set compatibility release */

	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    erts_compat_rel = atoi(arg);

	    if (erts_compat_rel < ERTS_MIN_COMPAT_REL
		|| erts_compat_rel > this_rel_num()) {
		erl_printf(CERR, "bad compatibility release number %s\n", arg);
		erts_usage();
	    }

	    ASSERT(ERTS_MIN_COMPAT_REL >= 7);
	    switch (erts_compat_rel) {
	    case 7:
	    case 8:
	    case 9:
		erts_use_r9_pids_ports = 1;
	    default:
		break;
	    }

	    break;
	}

	case 'A':
	    /* set number of threads in thread pool */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (((erts_async_max_threads = atoi(arg)) < -1) ||
		(erts_async_max_threads > 256)) {
		erl_printf(CERR, "bad number of threads %s\n", arg);
		erts_usage();
	    }
	    break;

 	case 'r':
	    erts_ets_realloc_always_moves = 1;
	    break;
	case 'n':   /* XXX obsolete */
	    break;
	case 'c':
	    if (argv[i][2] == 0) { /* -c: documented option */
		erts_disable_tolerant_timeofday = 1;
	    } else if (argv[i][2] == 'i') { /* -ci: undcoumented option */
		count_instructions = 1;
	    }
	    break;
	case 'W':
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    switch (arg[0]) {
	    case 'i':
		erts_error_logger_warnings = am_info;
		break;
	    case 'w':
		erts_error_logger_warnings = am_warning;
		break;
	    case 'e': /* The default */
		erts_error_logger_warnings = am_error;
	    default:
		erl_printf(CERR, "unrecognized warning_map option %s\n", arg);
		erts_usage();
	    }
	    break;

	default:
	    erl_printf(CERR, "%s unknown flag %s\n", progname(argv[0]), argv[i]);
	    erts_usage();
	}
	i++;
    }

    /* Delayed check of +P flag */
    if (erts_max_processes < ERTS_MIN_PROCESSES
	|| erts_max_processes > ERTS_MAX_PROCESSES
	|| (erts_use_r9_pids_ports
	    && erts_max_processes > ERTS_MAX_R9_PROCESSES)) {
	erl_printf(CERR, "bad number of processes %s\n", Parg);
	erts_usage();
    }

   /* Restart will not reinstall the break handler */
#ifdef __WIN32__
    if (ignore_break)
	erts_set_ignore_break();
    else if (replace_intr)
	erts_replace_intr();
    else
	init_break_handler();
#else
    if (ignore_break)
	erts_set_ignore_break();
    else if (have_break_handler)
	init_break_handler();
    if (replace_intr)
	erts_replace_intr();
#endif

    boot_argc = argc - i;  /* Number of arguments to init */
    boot_argv = &argv[i];

    erl_init();

    init_shared_memory(boot_argc, boot_argv);
    erts_initialized = 1;

    load_preloaded();

    erl_first_process_otp("otp_ring0", NULL, 0, boot_argc, boot_argv);
    process_main();
}


#ifdef USE_THREADS

void erts_thr_fatal_error(int err, char *what)
{
    char *errstr = err ? strerror(err) : NULL;
    erl_exit(1,
	     "Failed to %s: %s%s(%d)\n",
	     what,
	     errstr ? errstr : "",
	     errstr ? " " : "",
	     err);
}

#endif

static void
system_cleanup(int exit_code)
{

    /* No cleanup wanted if ...
     * 1. we are about to dump core,
     * 2. we haven't finished initializing, or
     * 3. another thread than the main thread is performing the exit.
     */
    if (exit_code > 0
	|| !erts_initialized
	|| !erts_equal_tids(main_thread, erts_thr_self()))
	return;

#ifdef HYBRID
    if (copy_src_stack) erts_free(ERTS_ALC_T_OBJECT_STACK,
                                  (void *)copy_src_stack);
    if (copy_dst_stack) erts_free(ERTS_ALC_T_OBJECT_STACK,
                                  (void *)copy_dst_stack);
    copy_src_stack = copy_dst_stack = NULL;
    erts_cleanup_offheap(&erts_global_offheap);
#endif

#ifdef SHARED_HEAP
    {
      ErlHeapFragment *tmp;
      while (global_mbuf != NULL) {
        tmp = global_mbuf->next;
        free_message_buffer(global_mbuf);
        global_mbuf = tmp;
      }
    }
#endif

#if defined(SHARED_HEAP) || defined(HYBRID)
    if (global_heap) {
	ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,
		       (void*) global_heap,
		       sizeof(Eterm) * global_heap_sz);
    }
    global_heap = NULL;
#endif

#ifdef NOMOVE
    erts_cleanup_nmgc();
#endif
#ifdef INCREMENTAL_GC
    erts_cleanup_incgc();
#endif

#ifdef USE_THREADS
    exit_async();
#endif
#if HAVE_ERTS_MSEG
    erts_mseg_exit();
#endif

    /*
     * A lot more cleaning could/should have been done...
     */

}

/*
 * Common exit function, all exits from the system go through here.
 * n <= 0 -> normal exit with status n;
 * n = 127 -> Erlang crash dump produced, exit with status 1;
 * other positive n -> Erlang crash dump and core dump produced.
 */

void erl_exit0(char *file, int line, int n, char *fmt,...)
{
    unsigned int an;
    va_list args;

    va_start(args, fmt);

    save_statistics();

    system_cleanup(n);

    /* Produce an Erlang core dump if error */
    if(n > 0 && erts_initialized) erl_crash_dump_v(file,line,fmt,args); 

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
    else ConNormalExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif

    an = abs(n);

    if (erts_mtrace_enabled)
	erts_mtrace_exit((Uint32) an);

    if (n == 127)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0)
        abort();
    exit(an);
}

void erl_exit(int n, char *fmt,...)
{
    unsigned int an;
    va_list args;

    va_start(args, fmt);

    save_statistics();

    system_cleanup(n);

    /* Produce an Erlang core dump if error */
    if(n > 0 && erts_initialized) erl_crash_dump_v((char*) NULL,0,fmt,args); 

    /* need to reinitialize va_args thing */
    va_end(args);
    va_start(args, fmt);

    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
    else ConNormalExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS) && !defined(_OSE_)
    sys_tty_reset();
#endif

    an = abs(n);

    if (erts_mtrace_enabled)
	erts_mtrace_exit((Uint32) an);

    if (n == 127)
	ERTS_EXIT_AFTER_DUMP(1);
    else if (n > 0)
        abort();
    exit(an);
}

