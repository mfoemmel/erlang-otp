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

extern void erl_crash_dump();
#ifdef __WIN32__
extern void ConWaitForExit(void);
#endif

/*
 * Configurable parameters.
 */

Uint32 max_process = MAX_PROCESS;
uint32 display_items = 100;	/* no of items to display in traces etc */
uint32 display_loads = 0;	/* print info about loaded modules */
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
uint32 verbose;		/* noisy mode = 1 */
#endif

/*
 * Other global variables.
 */

byte*      tmp_buf;
int        tot_bin_allocated = 0;
uint32     do_time;		/* set at clock interupt */
uint32     garbage_cols;	/* no of garbage collections */
uint32     reclaimed;		/* no of words reclaimed in GCs */

#ifdef INSTRUMENT
uint32 instr_send_sizes[INSTR_SEND_SIZES_MAX];
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

void
erl_init(void)
{
    garbage_cols = 0;
    reclaimed = 0;

    ASSERT(TMP_BUF_SIZE >= 16384);
    tmp_buf = (byte *)safe_alloc_from(130, TMP_BUF_SIZE);
    init_alloc();
    init_gc();

    H_MIN_SIZE = next_heap_size(H_MIN_SIZE, 0);

    erts_init_bits();
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
    init_dist();
    init_io();
    init_copy();
    init_load();
    erts_init_bif();

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
erl_first_process(char* modname, void* code, unsigned size,
		  int argc, char** argv)
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
	erl_exit(5, "No function %s:start/2\n", start_mod);
    }

    /*
     * We need a dummy parent process to be able to call erl_create_process().
     */

    erts_init_empty_process(&parent);
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
    p = process_tab[pid_number(pid)];
    p->group_leader = pid;
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
	res = do_load(NIL, module_name, code, length);
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
	       MIN_PROCESS, MAX_PROCESS);
    erl_printf(CERR, "-A number  set number or threads in async thread pool\n");
    erl_printf(CERR, "           valid range is [0-256]\n");
    erl_printf(CERR, "-m number  set mmap threshold (Kb)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-M number  set max number of mmappings\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX);
    erl_printf(CERR, "-t number  set trim threshold (Kb)\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "-T number  set top pad\n");
    erl_printf(CERR, "           valid range is [0-%d]\n", INT_MAX/1024);
    erl_printf(CERR, "\n\n");
    erl_exit(-1, "");
}


void
erl_start(int argc, char **argv)
{
    int i = 1;
    char* arg=NULL;
    int have_break_handler = 1;
    char* tmpenvbuf;
    int trim_threshold = ERTS_DEFAULT_TRIM_THRESHOLD;
    int top_pad = ERTS_DEFAULT_TOP_PAD;
    int mmap_max = ERTS_DEFAULT_MMAP_MAX;
    int mmap_threshold = ERTS_DEFAULT_MMAP_THRESHOLD;

    program = argv[0];

    erl_sys_init();
    erl_sys_args(&argc, argv);

    erts_init_utils();
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, top_pad);
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_THRESHOLD, mmap_threshold);
    /* Temporarily disable use of mmap during initialization. */
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, 0);

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
		char tmp[100];

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
	    if (((max_process = atoi(arg)) < MIN_PROCESS) ||
		(max_process > MAX_PROCESS)) {
		erl_printf(CERR, "bad number of processes %s\n", arg);
		usage();
	    }
	    break;

	case 'A':
	    /* set number of threads in thread pool */
	    arg = get_arg(argv[i]+2, argv[i+1], &i);
	    if (((erts_async_max_threads = atoi(arg)) < -1) ||
		(erts_async_max_threads > 256)) {
		erl_printf(CERR, "bad number of threads %s\n", arg);
		usage();
	    }
	    break;

	case 'm': {
	  char *rest;
	  /* set mmap threshold */
	  arg = get_arg(argv[i]+2, argv[i+1], &i);
	  errno = 0;
	  mmap_threshold = (int) strtol(arg, &rest, 10);
	  if (errno != 0
	      || rest == arg
	      || mmap_threshold < 0
	      || (INT_MAX/1024) < mmap_threshold) {
	    mmap_threshold = ERTS_DEFAULT_MMAP_THRESHOLD/1024;
	    erl_printf(CERR,
		       "bad mmap threshold: %s Kb; using default: %d Kb\n",
		       arg,
		       mmap_threshold);

	  }
	  VERBOSE(erl_printf(COUT, "using mmap threshold: %d Kb\n",
			     mmap_threshold););
	  mmap_threshold *= 1024;
	  break;
        }

	case 'M': {
	  char *rest;
	  /* set mmap max */
	  arg = get_arg(argv[i]+2, argv[i+1], &i);
	  errno = 0;
	  mmap_max = (int) strtol(arg, &rest, 10);
	  if (errno != 0 || rest == arg || mmap_max < 0) {
	    mmap_max = ERTS_DEFAULT_MMAP_MAX;
	    erl_printf(CERR,
		       "bad mmap max: %s; using default: %d\n",
		       arg,
		       mmap_max);
	  }
	  VERBOSE(erl_printf(COUT, "using mmap max: %d\n",
			     mmap_max););
	  break;
        }

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
    erl_init();
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, trim_threshold);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, top_pad);
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_THRESHOLD, mmap_threshold);
    if(!sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, mmap_max))
      sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, ERTS_DEFAULT_MMAP_MAX);
    load_preloaded();
    erl_first_process("otp_ring0", NULL, 0, boot_argc, boot_argv);
    erl_sys_schedule_loop();
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

    /* Produce an Erlang core dump if error */
    if(n > 0) erl_crash_dump(file,line,fmt,args); 
#if defined(VXWORKS)
    /* need to reinitialize va_args thing */
    va_start(args, fmt);
#endif
    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS)
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

    /* Produce an Erlang core dump if error */
    if(n > 0) erl_crash_dump((char*) NULL,0,fmt,args); 
#if defined(VXWORKS)
    /* need to reinitialize va_args thing */
    va_start(args, fmt);
#endif
    if (fmt != NULL && *fmt != '\0')
	  erl_error(fmt, args);	/* Print error message. */
    va_end(args);
#ifdef __WIN32__
    if(n > 0) ConWaitForExit();
#endif
#if !defined(__WIN32__) && !defined(VXWORKS)
    sys_tty_reset();
#endif
    if (n == 127)
        exit(1);
    else if (n > 0)
        abort();
    else
        exit(abs(n));
}
