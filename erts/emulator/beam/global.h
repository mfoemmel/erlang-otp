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

#ifndef __GLOBAL_H__
#define __GLOBAL_H__

/* #define BENCH_STAT */

#include "sys.h"
#include "erl_vm.h"
#include "hash.h"
#include "index.h"
#include "atom.h"
#include "export.h"
#include "module.h"
#include "register.h"
#include "erl_fun.h"

#include "erl_process.h"
#include "erl_sys_driver.h"

typedef enum {
    LNK_UNDEF = 0,
    LNK_LINK  = 1,  /* normal link */
    LNK_NODE  = 2,  /* node link */
    LNK_OMON  = 3,  /* originating object monitor */
    LNK_TMON  = 4,  /* terminating object monitor */
    LNK_LINK1 = 5,  /* uni-directional link, for monitor/2 */
#ifdef MONITOR_ENHANCE
    /* All code already written which does monitor/2 on something other
       than a local process is wrapped in this ifdef MONITOR_ENHANCE */
    LNK_NODE1 = 6,  /* node link, for monitor/2 */
#endif
} ErlLinkType;

typedef struct erl_link {
    struct erl_link* next;
    ErlLinkType type;		/* type of link */
    Eterm item;			/* the linked item */
    Eterm data;			/* data depending on use */
    Ref ref;			/* for LINK1 and NODE1 links */
} ErlLink;

ErlLink* new_link(ErlLink*, ErlLinkType, Eterm, Eterm);
ErlLink* new_ref_link(ErlLink*, ErlLinkType, Eterm, Eterm, Eterm);
void del_link(ErlLink**);
ErlLink** find_link(ErlLink**, ErlLinkType, Eterm, Eterm);
ErlLink** find_link_by_ref(ErlLink**, Ref*);

/*
 * The 9-bit node part of a pid is an index in a table pointing
 * to a struct of dist_entry.
 * Slot 0 is reserved for ourselves although we don't hash ourselves to 0.
 */

#define THIS_NODE 0     /* replace the global variable "node" */

#define D_EXITING   1   /* Status field vals  for dist_enntry's */
#define D_REAL_BUSY 4
#define D_RESERVED  8   /* Set-cookie called thing */


#define MAXINDX 255

typedef struct cache {
    Eterm in_arr[MAXINDX];
    Eterm out_arr[MAXINDX];
} ErlCache;

typedef struct dist_entry {
    Eterm sysname;		/* name@host atom for efficiency */
    Eterm cid;			/* connection handler (pid or port), NIL == free */
    ErlLink* links;		/* external link/monitors/node links */
    Uint32 status;		/* Slot status, like exiting reserved etc */
    Uint32 flags;		/* Distribution flags, like hidden, 
				   atom cache etc. */
    Eterm in_cookie;		/* cookie expected as input */
    Eterm out_cookie;		/* cookie sent with output */
    ErlCache* cache;		/* The atom cache */
    unsigned long version;	/* Protocol version */
} DistEntry;

typedef struct _process_list {
    Eterm pid;			/* Waiting process. */
    struct _process_list* next;	/* Next waiting process. */
} ProcessList;

#define SMALL_IO_QUEUE 5   /* Number of fixed elements */

typedef struct {
    int size;       /* total size in bytes */

    SysIOVec* v_start;
    SysIOVec* v_end;
    SysIOVec* v_head;
    SysIOVec* v_tail;
    SysIOVec  v_small[SMALL_IO_QUEUE];

    ErlDrvBinary** b_start;
    ErlDrvBinary** b_end;
    ErlDrvBinary** b_head;
    ErlDrvBinary** b_tail;
    ErlDrvBinary*  b_small[SMALL_IO_QUEUE];
} ErlIOQueue;

typedef struct line_buf {  /* Buffer used in line oriented I/O */
    int bufsiz;              /* Size of character buffer */
    int ovlen;               /* Length of overflow data */
    int ovsiz;               /* Actual size of overflow buffer */
    char data[1];            /* Starting point of buffer data,
			      data[0] is a flag indicating an unprocess CR,
			      The rest is the overflow buffer. */
} LineBuf;

typedef struct port {
    Eterm id;                   /* The Port id of this port */
    Eterm connected;            /* A connected process */
    Eterm caller;		/* Current caller. */
    Eterm data;			/* Data associated with port. */
    ErlHeapFragment* bp;	/* Heap fragment holding data (NULL if imm data). */
    Uint status;		/* Status and type flags */
    ErlLink* links;		/* List of links */
    Uint bytes_in;		/* Number of bytes read */
    Uint bytes_out;		/* Number of bytes written */
    ErlTimer tm;                 /* Timer entry */
    ErlIOQueue ioq;              /* driver accessible i/o queue */
    int    dslot;                /* Dist slot used in DISTRIBUTION */
    char *name;		         /* String used in the open */
    ErlDrvEntry* drv_ptr;
    long drv_data;
    ProcessList *suspended;	 /* List of suspended processes. */
    LineBuf *linebuf;            /* Buffer to hold data not ready for
				    process to get (line oriented I/O)*/
    int control_flags;		 /* Flags for port_control()  */
    struct reg_proc *reg;
} Port;

/* Driver handle (wrapper for old plain handle) */
#define ERL_DE_OK      0
#define ERL_DE_UNLOAD  1
#define ERL_DE_RELOAD  2

typedef struct {
    void* handle;           /* Handle for DLL or SO (for dynamic drivers). */
    int   ref_count;
    int   status;
    int   (*cb)();
    void** ca[4];           /* max 4 args now */
} DE_Handle;

/*
 * This structure represents a link to the next driver.
 */

typedef struct de_list {
    ErlDrvEntry *drv;		/* Pointer to entry for driver. */
    DE_Handle* de_hndl;	     /* Handle for DLL or SO (for dynamic drivers). */
    struct de_list *next;	/* Pointer to next. */
} DE_List;

extern DE_List *driver_list;

/*
 * Max no. of drivers (linked in and dynamically loaded). Each table
 * entry uses 4 bytes.
 */
#define DRIVER_TAB_SIZE 32

#define port_index(p)	((port_number(p)) % (erl_max_ports))

/*
** Just like the driver binary but with initial flags
** Note that the two structures Binary and ErlDrvBinary HAVE to
** be equal except for extra fields in the beginning of the struct.
** ErlDrvBinary is defined in erl_driver.h.
** When driver_alloc_binary is called, a Binary is allocated, but 
** the pointer returned is to the address of the first element that
** also occurs in the ErlDrvBinary struct (driver.*binary takes care if this).
** The driver need never know about additions to the internal Binary of the
** emulator. One should however NEVER be sloppy when mixing ErlDrvBinary
** and Binary, the macros below can convert one type to the other, as they both
** in reality are equal.
*/
typedef struct binary {
    unsigned flags;
    /* Add fields BEFORE this, otherwise the drivers crash */
    int orig_size;
    int refc;
    char orig_bytes[1]; /* to be continued */
} Binary;

#define Binary2ErlDrvBinary(B) ((ErlDrvBinary *) (&((B)->orig_size)))
#define ErlDrvBinary2Binary(D) ((Binary *) \
				(((char *) (D)) - \
				 ((char *) &(((Binary *) 0)->orig_size))))

/* A "magic" binary flag */
#define BIN_FLAG_MATCH_PROG 1
#define BIN_FLAG_USR1       2 /* Reserved for use by different modules too mark */
#define BIN_FLAG_USR2       4 /*  certain binaries as special (used by ets) */

/*
 * This structure represents one type of a binary in a process.
 */

typedef struct proc_bin {
    Eterm thing_word;		/* Subtag REFC_BINARY_SUBTAG. */
    Uint size;			/* Binary size in bytes. */
    struct proc_bin *next;	/* Pointer to next ProcBin. */
    Binary *val;		/* Pointer to Binary structure. */
    byte *bytes;		/* Pointer to the actual data bytes. */
} ProcBin;

/*
 * ProcBin size in Eterm words.
 */
#define PROC_BIN_SIZE (sizeof(ProcBin)/sizeof(Eterm))


/* arrays that get malloced at startup */
extern Port* erts_port;
extern DistEntry* dist_addrs;
extern byte*      tmp_buf;

extern Eterm node_cookie;
extern Uint tot_bin_allocated;
extern int this_creation;
extern Eterm this_node;         /* mysyst@myhost    */
extern Uint context_switches;	/* no of context switches */
extern Uint bytes_out;		/* no bytes written out */
extern Uint bytes_in;		/* no bytes sent into the system */
extern Uint display_items;	/* no of items to display in traces etc */
extern Uint reductions;		/* total number of reductions */
extern Uint garbage_cols;	/* no of garbage collections */
extern Uint reclaimed;		/* no of words reclaimed in GC's */
extern Uint display_loads;	/* print info about loaded modules */
extern Uint do_time;		/* set at clock interupt */
extern Uint last_reds;		/* to calculate no of reds since last call */
extern int    MAXDIST;

#define HS_FIBONACCI 0		/* Fibonacci */
#define HS_FIBONACCI_SLOW 1	/* Fibonacci with slowdown for big heaps */
#define HS_POWER_TWO 2		/* Powers of two */
#define HS_POWER_TWO_MINUS_ONE 3 /* Powers of two minus one word */

extern int heap_series;		/* Series to use for heap size. */
extern int erts_backtrace_depth;
extern Uint16 erts_max_gen_gcs;

#ifdef UNIFIED_HEAP
/* Global heap pointers */
extern Eterm *global_heap;    /* Heap start */
extern Eterm *global_hend;    /* Heap end */
extern Eterm *global_htop;    /* Heap top (heap pointer) */
extern Uint global_heap_sz;   /* Heap size, in words */
extern Uint global_heap_min_sz;

/* Global stuff for garbage collection */
extern Eterm *global_high_water;
extern Eterm *global_old_hend;
extern Eterm *global_old_htop;
extern Eterm *global_old_heap;
extern Uint16 global_gen_gcs;
extern Uint16 global_max_gen_gcs;
extern Uint   global_gc_flags;

extern ErlHeapFragment * global_mbuf;
extern Uint global_mbuf_sz;
#endif

#ifdef BENCH_STAT
/* Global stuff for benchmarks and statistics */
extern double messages_sent;
extern Uint message_sizes[1000];
extern double major_garbage_cols;
extern double minor_garbage_cols;
extern Uint biggest_heap_size_ever;
extern Uint max_allocated_heap;
extern double live_major_sum;
extern double live_minor_sum;
extern double ptrs_to_old;
extern double ptrs_to_young;

extern hrtime_t sys_time;
extern hrtime_t minor_gc_time;
extern hrtime_t major_gc_time;
extern hrtime_t send_time;
extern hrtime_t copy_time;
extern hrtime_t max_major_time;
extern hrtime_t max_minor_time;
#endif

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

/* For the fix allocator */
extern int process_desc;
extern int table_desc;
extern int atom_desc;
extern int export_desc;
extern int module_desc;
extern int preg_desc;
extern int link_desc;
extern int plist_desc;
extern int mesg_desc;
extern int erts_fun_desc;

#ifdef DEBUG
extern Uint verbose;		/* noisy mode = 1 */
#endif

extern Eterm system_seq_tracer;
#ifdef INSTRUMENT
#  define INSTR_SEND_SIZES_MAX 65535
extern Uint instr_send_sizes[INSTR_SEND_SIZES_MAX];
#endif

/*
 * Here is an implementation of a lightweiht stack.
 *
 * Use it like this:
 *
 * DECLARE_ESTACK(Stack)	(At the start of a block)
 * ...
 * ESTACK_PUSH(Stack, Term)
 * ...
 * if (ESTACK_ISEMPTY(Stack)) {
 *    Stack is empty
 * } else {
 *    Term = ESTACK_POP(Stack);
 *    Process popped Term here
 * }
 * ...
 * DESTROY_ESTACK(Stack)
 */
 
Eterm* erl_grow_stack(Eterm* ptr, size_t new_size);
#define ESTK_CONCAT(a,b) a##b
#define ESTK_SUBSCRIPT(s,i) *((Eterm *)((byte *)ESTK_CONCAT(s,_start) + (i)))
#define DEF_ESTACK_SIZE (16*sizeof(Eterm))

#define DECLARE_ESTACK(s)						\
    Eterm ESTK_CONCAT(s,_default_stack)[DEF_ESTACK_SIZE/sizeof(Eterm)];	\
    Eterm* ESTK_CONCAT(s,_start) = ESTK_CONCAT(s,_default_stack);	\
    size_t ESTK_CONCAT(s,_sp) = 0;					\
    size_t ESTK_CONCAT(s,_size) = DEF_ESTACK_SIZE

#define DESTROY_ESTACK(s)						\
do {									\
    if (ESTK_CONCAT(s,_start) != ESTK_CONCAT(s,_default_stack)) {	\
	sys_free(ESTK_CONCAT(s,_start));				\
    }									\
} while(0)

#define ESTACK_PUSH(s, x)							\
do {										\
    if (ESTK_CONCAT(s,_sp) == ESTK_CONCAT(s,_size)) {				\
	ESTK_CONCAT(s,_size) *= 2;						\
	ESTK_CONCAT(s,_start) =							\
	    erl_grow_stack(ESTK_CONCAT(s,_start), ESTK_CONCAT(s,_size));	\
    }										\
    ESTK_SUBSCRIPT(s,ESTK_CONCAT(s,_sp)) = (x);					\
    ESTK_CONCAT(s,_sp) += sizeof(Eterm);					\
} while(0)

#define ESTACK_ISEMPTY(s) (ESTK_CONCAT(s,_sp) == 0)
#define ESTACK_POP(s)								\
((ESTK_CONCAT(s,_sp) -= sizeof(Eterm)), ESTK_SUBSCRIPT(s,ESTK_CONCAT(s,_sp)))

/* flags  for the port status info*/

#define FREE          0
#define CONNECTED     (1 << 0)
#define EXITING       (1 << 1)
#define DISTRIBUTION  (1 << 2)
#define BINARY_IO     (1 << 3)
#define SOFT_EOF      (1 << 4)
#define PORT_BUSY     (1 << 5)  /* Flow control on ports */
#define CLOSING       (1 << 6)  /* Port is closing (no i/o accepted ) */
#define SEND_CLOSED   (1 << 7)  /* Send a closed message when terminating */
#define LINEBUF_IO    (1 << 8)  /* Line orinted io on port */

/* binary.c */

Eterm new_binary(Process*, byte*, int);
Eterm new_binary_arith(Process*, byte*, int);
Eterm erts_realloc_binary(Eterm bin, size_t size);
void erts_cleanup_mso(ProcBin* pb);

/* bif.c */
void queue_monitor_message(Process *, Ref *, Eterm, Eterm, Eterm);
void print_timer_info(CIO);
void erts_init_bif(void);

/* erl_bif_port.c */

Port* id2port(Eterm id);

/* beam_load.c */
int bin_load(Eterm, Eterm, byte*, int);
void init_load(void);
Eterm* find_function_from_pc(Eterm* pc);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);

/* break.c */
void init_break_handler(void);
void process_info(CIO);
void print_process_info(Process*, CIO);
void info(CIO);
void loaded(CIO);

/* config.c */

void __noreturn erl_exit(int n, char*, ...);
void __noreturn erl_exit0(char *, int, int n, char*, ...);
void erl_error(char*, va_list);

#define ERL_EXIT0(n,f)		erl_exit0(__FILE__, __LINE__, n, f)
#define ERL_EXIT1(n,f,a)	erl_exit0(__FILE__, __LINE__, n, f, a)
#define ERL_EXIT2(n,f,a,b)	erl_exit0(__FILE__, __LINE__, n, f, a, b)
#define ERL_EXIT3(n,f,a,b,c)	erl_exit0(__FILE__, __LINE__, n, f, a, b, c)

/* copy.c */
void init_copy(void);
void copy_object(Eterm, Process*, Uint, Eterm*, Process*);
Uint size_object(Eterm);
Eterm copy_struct(Eterm, Uint, Eterm**, ErlOffHeap*);
Eterm copy_shallow(Eterm*, Uint, Eterm**, ErlOffHeap*);

/* dist.c */
/* More in dist.h */
/* Atom cache */
extern void clear_cache(int);
extern void create_cache(int);
extern void delete_cache(int);

/* Utilities */
extern int do_net_exits(int);
extern int remove_from_link_list(int, Eterm, Eterm);
extern int add_to_link_list(int, Eterm, Eterm);
extern int sysname_to_dist_slot(Eterm);
extern int find_or_insert_dist_slot(Eterm);
extern int distribution_info(CIO);
extern int is_node_name(char*, int);

extern int net_mess2(int, byte*, int, byte*, int);

extern void init_dist(void);
extern int stop_dist(void);

void erl_progressf(char* format, ...);

#ifdef MESS_DEBUG
void print_pass_through(int, byte*, int);
#endif

/* beam_emu.c */
int catchlevel(Process*);
void init_emulator(_VOID_);
int process_main(Process*, int);

/* erl_init.c */
void erl_start(int, char**);

/* erl_md5.c */

typedef struct {
    Uint32 state[4];		/* state (ABCD) */
    Uint32 count[2];		/* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];	/* input buffer */
} MD5_CTX;

void MD5Init(MD5_CTX *);
void MD5Update(MD5_CTX *, unsigned char *, unsigned int);
void MD5Final(unsigned char [16], MD5_CTX *);

/* external.c */
int to_external(int, Eterm, byte**);
Eterm from_external(int, Eterm**, byte**, ErlOffHeap*);
Eterm encode_size_struct(Eterm, unsigned);
int decode_size(byte*, int);

/* fix_alloc.c */
void init_alloc(_VOID_);
int fix_info(int);
int fix_used(int);
int new_fix_size(int);
void fix_free(int, Eterm*);
Eterm* fix_alloc(int);
#ifdef INSTRUMENT
Eterm* fix_alloc_from(int, int);
#endif

/* ggc.c */
void init_gc(void);
int erts_garbage_collect(Process*, int, Eterm*, int);
int next_heap_size(int, int);
void stack_dump2(Process *, CIO);
Eterm erts_heap_sizes(Process* p);

/* io.c */

void wake_process_later(Eterm, Process*);
int open_driver(ErlDrvEntry*, Eterm, char*, SysDriverOpts*);
void close_port(Eterm);
void init_io(void);
void cleanup_io(void);
void do_exit_port(Eterm, Eterm, Eterm);
void port_command(Eterm, Eterm, Eterm);
int port_control(Process*, Port*, Uint, Eterm, Eterm*);
int write_port(Eterm caller_id, int p, Eterm list);
void print_port_info(int, CIO);
void dist_port_command(Port*, byte*, int);
void input_ready(int, int);
void output_ready(int, int);
void driver_report_exit(int, int);
LineBuf* allocate_linebuf(int);

/* time.c */
void increment_time(int);
void bump_timer(void);
void init_time(void);
void erl_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, Uint);
void erl_cancel_timer(ErlTimer*);
Uint time_left(ErlTimer *);
int next_time(_VOID_);

#ifdef DEBUG
void p_slpq(_VOID_);
#endif

/* utils.c */
#define erl_printf sys_printf
#define erl_putc   sys_putc

void erts_cleanup_offheap(ErlOffHeap *offheap);

void erl_suspend(Process*, Eterm);
void erl_resume(Process*);

int list_length(Eterm);
Export* erts_find_function(Eterm, Eterm, int);
int erts_is_builtin(Eterm, Eterm, int);
Eterm double_to_integer(Process*, double);
Uint32 make_broken_hash(Eterm, Uint32);
Uint32 make_phash(Eterm, Uint32);
Uint32 make_hash(Eterm, Uint32);

int send_error_to_logger(Eterm);
#ifdef INSTRUMENT
void* safe_alloc_from(int, Uint);
void* safe_realloc_from(int, void*, Uint);
void* safe_sl_alloc_from(int, Uint);
void* safe_sl_realloc_from(int, void*, Uint, Uint);
#endif
void* safe_alloc(Uint);
void* safe_realloc(void*, Uint);
void* safe_sl_alloc(Uint);
void* safe_sl_realloc(void*, Uint, Uint);
Process* pid2proc(Eterm);
void display(Eterm, CIO);
void ldisplay(Eterm, CIO, int);
void print_atom(int, CIO);
void erts_init_utils(void);

int eq(Eterm, Eterm);
#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

int cmp(Eterm, Eterm);
#define cmp_lt(a,b)	(cmp((a),(b)) < 0)
#define cmp_le(a,b)	(cmp((a),(b)) <= 0)
#define cmp_eq(a,b)	(cmp((a),(b)) == 0)
#define cmp_ne(a,b)	(cmp((a),(b)) != 0)
#define cmp_ge(a,b)	(cmp((a),(b)) >= 0)
#define cmp_gt(a,b)	(cmp((a),(b)) > 0)

#define CMP_LT(a,b)	((a) != (b) && cmp_lt((a),(b)))
#define CMP_GE(a,b)	((a) == (b) || cmp_ge((a),(b)))
#define CMP_EQ(a,b)	((a) == (b) || cmp_eq((a),(b)))
#define CMP_NE(a,b)	((a) != (b) && cmp_ne((a),(b)))

int term_to_Uint(Eterm term, Uint *up);

void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm);
Uint32 erts_call_trace(Process *p, Eterm mfa[], Binary *match_spec, Eterm* args, int local);
void erts_trace_return(Process* p, Eterm* fi, Eterm retval);
void erts_trace_return_to(Process *p, Uint *pc);
void trace_sched(Process*, Eterm);
void trace_proc(Process*, Process*, Eterm, Eterm);
void trace_proc_spawn(Process*, Eterm pid, Eterm mod, Eterm func, Eterm args);
void save_calls(Process *p, Export *);
void trace_gc(Process *p, Eterm what);
Uint erts_trace_flag2bit(Eterm flag);
Eterm erts_bif_trace(int bif_index, Process* p, 
		     Eterm arg1, Eterm arg2, Eterm arg3, Uint *I);

int member(Eterm, Eterm);
void bin_write(CIO, byte*, int);
int intlist_to_buf(Eterm, byte*, int);
char* int_to_buf(int, char*);
Eterm buf_to_intlist(Eterm**, byte*, int, Eterm);
int io_list_to_buf(Eterm, char*, int);
int io_list_len(Eterm);
int is_string(Eterm);
int do_load(Eterm, Eterm, byte*, int);
void erl_at_exit(FUNCTION(void,(*),(void*)), void*);
Eterm collect_memory(Process *);
void dump_memory_to_fd(int);
int dump_memory_data(const char *);

Eterm erts_mixed_plus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_minus(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_times(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_mixed_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_div(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_int_rem(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_band(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bor(Process* p, Eterm arg1, Eterm arg2);
Eterm erts_bxor(Process* p, Eterm arg1, Eterm arg2);

Uint erts_current_reductions(Process* current, Process *p);

#ifdef DEBUG
void upp(byte*, int);
void pat(Eterm);
void pinfo(void);
void pp(Process*);
void ppi(Eterm);
void pba(Process*, int);
void td(Eterm);
void ps(Process*, Eterm*);
#endif

/*
 * Interface to erl_init
 */
void erl_init(void);
void erl_first_process(char* modname, void* code, unsigned size,
		       int argc, char** argv);



#define seq_trace_output(token, msg, type, receiver, process) \
seq_trace_output_generic((token), (msg), (type), (receiver), (process), NIL)
#define seq_trace_output_exit(token, msg, type, receiver, exitfrom) \
seq_trace_output_generic((token), (msg), (type), (receiver), NULL, (exitfrom))
void seq_trace_output_generic(Eterm token, Eterm msg, Uint type, 
			      Eterm receiver, Process *process, Eterm exitfrom);

int seq_trace_update_send(Process *process);

Eterm erts_seq_trace(Process *process, 
		     Eterm atom_type, Eterm atom_true_or_false, 
		     int build_result);

int erts_set_trace_pattern(Eterm* mfa, int specified, Binary* match_prog_set,
			   int on, int is_local);

extern int erts_match_spec_is_on;
extern Binary* erts_default_match_spec;
extern int erts_match_local_functions;

/*
** Call_trace uses this API for the parameter matching functions
*/
    struct erl_heap_fragment* saved_program_buf;

#define MatchSetRef(MPSP) 			\
do {						\
    if ((MPSP) != NULL) {			\
	++((MPSP)->refc);			\
    }						\
} while (0)

#define MatchSetUnref(MPSP)					\
do {								\
    if (((MPSP) != NULL) && (--((MPSP)->refc) <= 0)) {	\
	erts_match_set_free(MPSP);				\
    }								\
} while(0)

#define MatchSetGetSource(MPSP) erts_match_set_get_source(MPSP)

extern Binary *erts_match_set_compile(Process *p, Eterm matchexpr);
Eterm erts_match_set_lint(Process *p, Eterm matchexpr); 
extern Eterm erts_match_set_run(Process *p, Binary *mpsp, 
				Eterm *args, int num_args,
				Uint32 *return_flags);
extern void erts_match_set_free(Binary *mpsp);
extern Eterm erts_match_set_get_source(Binary *mpsp);

#define MATCH_SET_RETURN_TRACE 0x1 /* return trace requested */
#define MATCH_SET_RETURN_TO_TRACE 0x2 /* Misleading name, it is not actually
					 set by the match program, but by the
					 breakpoint functions */

/*
 * Flag values when tracing bif
 */
#define BIF_TRACE_AS_LOCAL 0x1

#endif
