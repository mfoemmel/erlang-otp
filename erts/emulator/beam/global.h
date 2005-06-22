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

#include "sys.h"
#include "erl_alloc.h"
#include "erl_vm.h"
#include "erl_node_container_utils.h"
#include "hash.h"
#include "index.h"
#include "atom.h"
#include "export.h"
#include "module.h"
#include "register.h"
#include "erl_fun.h"
#include "erl_node_tables.h"
#include "benchmark.h"
#include "erl_process.h"
#include "erl_sys_driver.h"
#include "erl_debug.h"

#define D_EXITING           1   /* Status field vals  for dist_enntry's */
#define D_REAL_BUSY         4


#define MAXINDX 255

typedef struct cache {
    Eterm in_arr[MAXINDX];
    Eterm out_arr[MAXINDX];
} ErlCache;

typedef struct _process_list {
    Eterm pid;			/* Waiting process. (internal pid) */
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
    ErtsLink *nlinks;
    Uint bytes_in;		/* Number of bytes read */
    Uint bytes_out;		/* Number of bytes written */
    ErlTimer tm;                 /* Timer entry */
    ErlIOQueue ioq;              /* driver accessible i/o queue */
    DistEntry *dist_entry;       /* Dist entry used in DISTRIBUTION */
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
    int   (*cb)(void*, void*, void*, void*);
    void* ca[4];           /* max 4 args now */
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
    Uint flags;
    /* Add fields BEFORE this, otherwise the drivers crash */
    long orig_size;
    long refc;
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
#define BIN_FLAG_DRV        8

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
extern byte*      tmp_buf;

extern Uint erts_max_ports;
extern Uint erts_port_tab_index_mask;
/* controls warning mapping in error_logger */

extern Eterm node_cookie;
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

extern int erts_backtrace_depth;
extern Uint16 erts_max_gen_gcs;

extern int erts_disable_tolerant_timeofday;

/* Defines to ease the change of memory architecture */
#ifdef SHARED_HEAP
#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop
#  define HEAP_LIMIT(p)     (p)->hend
#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
#  define SAVED_HEAP_TOP(p) global_saved_htop
#  define STACK_START(p)    (p)->stack
#  define STACK_TOP(p)      (p)->stop
#  define STACK_END(p)      (p)->send
#  define HIGH_WATER(p)     global_high_water
#  define OLD_HEND(p)       global_old_hend
#  define OLD_HTOP(p)       global_old_htop
#  define OLD_HEAP(p)       global_old_heap
#  define GEN_GCS(p)        global_gen_gcs
#  define MAX_GEN_GCS(p)    global_max_gen_gcs
#  define FLAGS(p)          global_gc_flags
#  define MBUF(p)           global_mbuf
#  define HALLOC_MBUF(p)    global_halloc_mbuf
#  define MBUF_SIZE(p)      global_mbuf_sz
#  define MSO(p)            erts_global_offheap
#  define MIN_HEAP_SIZE(p)  H_MIN_SIZE
#else
#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop
#  define HEAP_LIMIT(p)     (p)->stop
#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
#  define SAVED_HEAP_TOP(p) (p)->saved_htop
#  define STACK_START(p)    (p)->hend
#  define STACK_TOP(p)      (p)->stop
#  define STACK_END(p)      (p)->htop
#  define HIGH_WATER(p)     (p)->high_water
#  define OLD_HEND(p)       (p)->old_hend
#  define OLD_HTOP(p)       (p)->old_htop
#  define OLD_HEAP(p)       (p)->old_heap
#  define GEN_GCS(p)        (p)->gen_gcs
#  define MAX_GEN_GCS(p)    (p)->max_gen_gcs
#  define FLAGS(p)          (p)->flags
#  define MBUF(p)           (p)->mbuf
#  define HALLOC_MBUF(p)    (p)->halloc_mbuf
#  define MBUF_SIZE(p)      (p)->mbuf_sz
#  define MSO(p)            (p)->off_heap
#  define MIN_HEAP_SIZE(p)  (p)->min_heap_size
#endif

#if (defined(NOMOVE) && defined(SHARED_HEAP))
#  undef OLD_HEND
#  undef OLD_HTOP
#  undef OLD_HEAP
#endif

#if defined(SHARED_HEAP) || defined(HYBRID)

/* Global heap pointers */
extern Eterm *global_heap;             /* Heap start */
extern Eterm *global_hend;             /* Heap end */
extern Eterm *global_htop;             /* Heap top (heap pointer) */
extern Eterm *global_saved_htop;       /* Saved heap top (heap pointer) */
extern Uint global_heap_sz;            /* Heap size, in words */
extern ErlOffHeap erts_global_offheap; /* Global MSO (OffHeap) list */

#ifdef NOMOVE
extern Eterm *nm_heap;
extern Eterm *nm_hend;
#define OLD_M_DATA_START nm_heap
#define OLD_M_DATA_END nm_hend
#else
extern Eterm *global_old_hend;
extern Eterm *global_old_htop;
extern Eterm *global_old_heap;
#define OLD_M_DATA_START global_old_heap
#define OLD_M_DATA_END global_old_htop
#endif

#ifdef INCREMENTAL_GC
#  define ACTIVATE(p)
#  define DEACTIVATE(p)
#  define IS_ACTIVE(p) 1
/* We still use the active-flag, but not in the same places */
/*
#  define INC_ACTIVATE(p)   (p)->active = 1;
#  define INC_DEACTIVATE(p) (p)->active = 0;
#  define INC_IS_ACTIVE(p)  ((p)->active != 0)
*/

#define INC_ACTIVATE(p) do {                                           \
    if ((p)->active) {                                                 \
        if ((p)->active_next != NULL) {                                \
            (p)->active_next->active_prev = (p)->active_prev;          \
            if ((p)->active_prev) {                                    \
                (p)->active_prev->active_next = (p)->active_next;      \
            } else {                                                   \
                inc_active_proc = (p)->active_next;                    \
            }                                                          \
            inc_active_last->active_next = (p);                        \
            (p)->active_next = NULL;                                   \
            (p)->active_prev = inc_active_last;                        \
            inc_active_last = (p);                                     \
        }                                                              \
    } else {                                                           \
        (p)->active_next = NULL;                                       \
        (p)->active_prev = inc_active_last;                            \
        if (inc_active_last) {                                         \
            inc_active_last->active_next = (p);                        \
        } else {                                                       \
            inc_active_proc = (p);                                     \
        }                                                              \
        inc_active_last = (p);                                         \
        (p)->active = 1;                                               \
    }                                                                  \
} while(0);

#define INC_DEACTIVATE(p) do {                                         \
    ASSERT((p)->active == 1);                                          \
    if ((p)->active_next == NULL) {                                    \
        inc_active_last = (p)->active_prev;                            \
    } else {                                                           \
        (p)->active_next->active_prev = (p)->active_prev;              \
    }                                                                  \
    if ((p)->active_prev == NULL) {                                    \
        inc_active_proc = (p)->active_next;                            \
    } else {                                                           \
        (p)->active_prev->active_next = (p)->active_next;              \
    }                                                                  \
    (p)->active = 0;                                                   \
} while(0);

#define INC_IS_ACTIVE(p)  ((p)->active != 0)

#else
#  define ACTIVATE(p)   (p)->active = 1;
#  define DEACTIVATE(p) (p)->active = 0;
#  define IS_ACTIVE(p)  ((p)->active != 0)
#  define INC_ACTIVATE(p)
#  define INC_IS_ACTIVE(p) 1
extern Eterm *global_high_water;
#endif

extern Uint16 global_gen_gcs;
extern Uint16 global_max_gen_gcs;
extern Uint   global_gc_flags;

#else
#  define ACTIVATE(p)
#  define DEACTIVATE(p)
#  define IS_ACTIVE(p) 1
#  define INC_ACTIVATE(p)
#endif /* SHARED_HEAP || HYBRID */

#ifdef SHARED_HEAP
extern ErlHeapFragment *global_mbuf;
extern ErlHeapFragment *global_halloc_mbuf;
extern Uint global_mbuf_sz;
#endif

#ifdef HYBRID
extern Uint global_heap_min_sz;
#endif

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

extern Eterm system_seq_tracer;

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
	erts_free(ERTS_ALC_T_ESTACK, ESTK_CONCAT(s,_start));		\
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

Eterm erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap);
Eterm new_binary(Process*, byte*, int);
Eterm new_binary_arith(Process*, byte*, int);
Eterm erts_realloc_binary(Eterm bin, size_t size);
void erts_cleanup_mso(ProcBin* pb);

/* erl_bif_info.c */

void erts_bif_info_init(void);

/* bif.c */
Uint bif_timer_memory_size(void);
void queue_monitor_message(Process *, Eterm, Eterm, Eterm, Eterm);
void print_timer_info(CIO);
void erts_init_bif(void);

/* erl_bif_port.c */

Port* id2port(Eterm id);

/* erl_bif_trace.c */
void erts_system_monitor_clear(void);

/* beam_load.c */
int erts_load_module(Eterm group_leader, Eterm* mod, byte* code, int size);
void init_load(void);
Eterm* find_function_from_pc(Eterm* pc);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);

/* break.c */
void init_break_handler(void);
void erts_set_ignore_break(void);
void erts_replace_intr(void);
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
Eterm copy_object(Eterm, Process*);
Uint size_object(Eterm);
Eterm copy_struct(Eterm, Uint, Eterm**, ErlOffHeap*);
Eterm copy_shallow(Eterm*, Uint, Eterm**, ErlOffHeap*);

#ifdef HYBRID
extern Eterm *copy_src_stack;
extern Uint copy_src_top;
extern Uint copy_src_size;
extern Eterm *copy_dst_stack;
extern Uint copy_dst_top;
extern Uint copy_dst_size;

extern Eterm *copy_offset_stack;
extern Uint copy_offset_top;
extern Uint copy_offset_size;

#define RRMA_DEFAULT_SIZE 256
#define MA_ROOT_PUSH(p,ptr,src) do {                                    \
  ASSERT((p)->rrma != NULL);                                            \
  ASSERT((p)->rrsrc != NULL);                                           \
  (p)->rrma[(p)->nrr] = (ptr);                                          \
  (p)->rrsrc[(p)->nrr++] = (src);                                       \
  if ((p)->nrr == (p)->rrsz)                                            \
  {                                                                     \
      ERTS_PROC_LESS_MEM(sizeof(Eterm) * (p)->rrsz * 2);                \
      (p)->rrsz *= 2;                                                   \
      ERTS_PROC_MORE_MEM(sizeof(Eterm) * (p)->rrsz * 2);                \
      (p)->rrma = (Eterm *) erts_realloc(ERTS_ALC_T_ROOTSET,            \
                                         (void*)(p)->rrma,              \
                                         sizeof(Eterm) * (p)->rrsz);    \
      (p)->rrsrc = (Eterm **) erts_realloc(ERTS_ALC_T_ROOTSET,          \
                                           (void*)(p)->rrsrc,           \
                                            sizeof(Eterm) * (p)->rrsz); \
  }                                                                     \
} while(0)

#define ROOT_PUSH(_s_,ptr) do {                                         \
  copy_##_s_##_stack[copy_##_s_##_top++] = (ptr);                       \
  if (copy_##_s_##_top == copy_##_s_##_size)                            \
  {                                                                     \
      ERTS_PROC_LESS_MEM(sizeof(Eterm) * copy_##_s_##_size);            \
      copy_##_s_##_size *= 2;                                           \
      ERTS_PROC_MORE_MEM(sizeof(Eterm) * copy_##_s_##_size);            \
      copy_##_s_##_stack =                                              \
        (Eterm *) erts_realloc(ERTS_ALC_T_OBJECT_STACK,                 \
                               (void*)copy_##_s_##_stack,               \
                               sizeof(Eterm) * copy_##_s_##_size);      \
  }                                                                     \
} while(0)

#define ROOT_POP(_s_) (copy_##_s_##_stack[--copy_##_s_##_top])
#define ROOT_TOP(_s_) (copy_##_s_##_stack[copy_##_s_##_top - 1])
#define ROOT_UPDATE(_s_,offset,value)  \
  *(ptr_val(copy_##_s_##_stack[copy_##_s_##_top - 1]) + (offset)) = (value)

#ifdef INCREMENTAL_GC
#define NO_COPY(obj) (IS_CONST(obj) ||                        \
                      (((ptr_val(obj) >= global_heap) &&      \
                        (ptr_val(obj) < global_htop)) ||      \
                       ((ptr_val(obj) >= inc_n2) &&           \
                        (ptr_val(obj) < inc_n2_end)) ||       \
                       ((ptr_val(obj) >= OLD_M_DATA_START) && \
                        (ptr_val(obj) < OLD_M_DATA_END))))
#else
#define NO_COPY(obj) (IS_CONST(obj) ||                        \
                      (((ptr_val(obj) >= global_heap) &&      \
                        (ptr_val(obj) < global_htop)) ||      \
                       ((ptr_val(obj) >= OLD_M_DATA_START) && \
                        (ptr_val(obj) < OLD_M_DATA_END))))
#endif

#define LAZY_COPY(from,obj) do {                     \
  if (!NO_COPY(obj)) {                               \
      BM_LAZY_COPY_START;                            \
      BM_COUNT(messages_copied);                     \
      obj = copy_struct_lazy(from,obj,0);            \
      BM_LAZY_COPY_STOP;                             \
  }                                                  \
} while(0)

Eterm copy_struct_lazy(Process*, Eterm, Uint);

#endif /* HYBRID */

/* dist.c */
/* More in dist.h */
/* Atom cache */
extern void clear_cache(DistEntry*);
extern void create_cache(DistEntry*);
extern void delete_cache(DistEntry*);

/* Utilities */
extern int do_net_exits(DistEntry*);
extern int distribution_info(CIO);
extern int is_node_name(char*, int);
extern int is_node_name_atom(Eterm a);

extern int net_mess2(DistEntry*, byte*, int, byte*, int);

extern void init_dist(void);
extern int stop_dist(void);

void erl_progressf(char* format, ...);

#ifdef MESS_DEBUG
void print_pass_through(int, byte*, int);
#endif

/* beam_emu.c */
int catchlevel(Process*);
void init_emulator(_VOID_);
void process_main(void);
Eterm build_stacktrace(Process* c_p, Eterm exc);
Eterm expand_error_value(Process* c_p, Uint freason, Eterm Value);

/* erl_init.c */

extern volatile int erts_writing_erl_crash_dump;
extern Eterm erts_error_logger_warnings;
extern int erts_initialized;
extern int erts_compat_rel;
void erts_short_init(void);
void erl_start(int, char**);
void erts_usage(void);
Eterm erts_preloaded(Process* p);
/* erl_md5.c */

typedef struct {
    Uint32 state[4];		/* state (ABCD) */
    Uint32 count[2];		/* number of bits, modulo 2^64 (lsb first) */
    unsigned char buffer[64];	/* input buffer */
} MD5_CTX;

void MD5Init(MD5_CTX *);
void MD5Update(MD5_CTX *, unsigned char *, unsigned int);
void MD5Final(unsigned char [16], MD5_CTX *);

/* erl_sl_alloc.c */
Eterm erts_sl_alloc_stat_eterm(Process *, int);

/* external.c */
int erts_to_external_format(DistEntry*, Eterm, byte**);
Eterm erts_from_external_format(DistEntry*, Eterm**, byte**, ErlOffHeap*);
Eterm encode_size_struct(Eterm, unsigned);
int decode_size(byte*, int);

/* ggc.c */
void erts_init_gc(void);
int erts_garbage_collect(Process*, int, Eterm*, int);
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(Process* p);
void erts_shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj);

#ifdef HYBRID
int erts_global_garbage_collect(Process*, int, Eterm*, int);
#endif

#ifdef NOMOVE
void erts_nm_init(void);
void erts_nm_cleanup(void);
#endif

#ifdef INCREMENTAL_GC
Eterm *erts_alloc_with_gc(Process*, int);
#endif

/* io.c */

void wake_process_later(Eterm, Process*);
int open_driver(ErlDrvEntry*, Eterm, char*, SysDriverOpts*);
void close_port(Eterm);
void init_io(void);
void cleanup_io(void);
void do_exit_port(Eterm, Eterm, Eterm);
void port_command(Eterm, Eterm, Eterm);
Eterm erts_port_control(Process*, Port*, Uint, Eterm);
int write_port(Eterm caller_id, int p, Eterm list);
void print_port_info(int, CIO);
void dist_port_command(Port*, byte*, int);
void input_ready(int, int);
void output_ready(int, int);
void event_ready(int, int, ErlDrvEventData);
void driver_report_exit(int, int);
LineBuf* allocate_linebuf(int);
int async_ready(int ix, void* data);
Sint erts_test_next_port(int, Uint);

/* erl_obsolete.c */
void erts_init_obsolete(void);

/* time.c */
void increment_time(int);
void bump_timer(void);
void init_time(void);
void erl_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, Uint);
void erl_cancel_timer(ErlTimer*);
Uint time_left(ErlTimer *);
int next_time(_VOID_);

Uint erts_timer_wheel_memory_size(void);

#if (defined(HAVE_GETHRVTIME) || defined(HAVE_CLOCK_GETTIME))
#  ifndef HAVE_ERTS_NOW_CPU
#    define HAVE_ERTS_NOW_CPU
#    ifdef HAVE_GETHRVTIME
#      define erts_start_now_cpu() sys_start_hrvtime()
#      define erts_stop_now_cpu()  sys_stop_hrvtime()
#    endif
#  endif
void erts_get_now_cpu(Uint* megasec, Uint* sec, Uint* microsec);
#endif

long erts_get_time(void);

#ifdef DEBUG
void p_slpq(_VOID_);
#endif

/* utils.c */

#define erl_printf sys_printf
#define erl_putc   sys_putc

void erts_cleanup_offheap(ErlOffHeap *offheap);
void erts_cleanup_externals(ExternalThing *);

void erl_suspend(Process*, Eterm);
void erl_resume(Process*);

Uint erts_fit_in_bits(Uint);
int list_length(Eterm);
Export* erts_find_function(Eterm, Eterm, unsigned int);
int erts_is_builtin(Eterm, Eterm, int);
Eterm double_to_integer(Process*, double);
Uint32 make_broken_hash(Eterm, Uint32);
Uint32 block_hash(byte *, unsigned, Uint32);
Uint32 make_hash2(Eterm);
Uint32 make_hash(Eterm, Uint32);

Eterm erts_bld_uint(Uint **hpp, Uint *szp, Uint ui);
Eterm erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr);
Eterm erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...);
Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[]);
Eterm erts_bld_string(Uint **hpp, Uint *szp, char *str);
Eterm erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[]);
Eterm erts_bld_2tup_list(Uint **hpp, Uint *szp,
			 Sint length, Eterm terms1[], Uint terms2[]);

Eterm store_external_or_ref_in_proc_(Process *, Eterm);
Eterm store_external_or_ref_(Uint **, ExternalThing **, Eterm);

#define NC_HEAP_SIZE(NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? 0 : (thing_arityval(*boxed_val((NC))) + 1))
#define STORE_NC(Hpp, ETpp, NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_((Hpp), (ETpp), (NC)))
#define STORE_NC_IN_PROC(Pp, NC) \
 (ASSERT_EXPR(is_node_container((NC))), \
  IS_CONST((NC)) ? (NC) : store_external_or_ref_in_proc_((Pp), (NC)))

int send_error_to_logger(Eterm); /* XXX:Obsolete */
int erts_send_info_to_logger(Eterm gleader, char *buf, int len); 
int erts_send_warning_to_logger(Eterm gleader, char *buf, int len);
int erts_send_error_to_logger(Eterm gleader, char *buf, int len);
Process* pid2proc(Eterm);
void display(Eterm, CIO);
void ldisplay(Eterm, CIO, int);
void print_atom(int, CIO);
void erts_init_utils(void);
void erts_init_utils_mem(void);

int eq(Eterm, Eterm);
#define EQ(x,y) (((x) == (y)) || (is_not_both_immed((x),(y)) && eq((x),(y))))

Sint cmp(Eterm, Eterm);
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

#ifdef HAVE_ERTS_NOW_CPU
extern int erts_cpu_timestamp;
#endif

/* erl_trace.c */
void erts_init_trace(void);
void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm);
Uint32 erts_call_trace(Process *p, Eterm mfa[], Binary *match_spec, Eterm* args,
		       int local, Eterm *tracer_pid);
void erts_trace_return(Process* p, Eterm* fi, Eterm retval, Eterm *tracer_pid);
void erts_trace_return_to(Process *p, Uint *pc);
void trace_sched(Process*, Eterm);
void trace_proc(Process*, Process*, Eterm, Eterm);
void trace_proc_spawn(Process*, Eterm pid, Eterm mod, Eterm func, Eterm args);
void save_calls(Process *p, Export *);
void trace_gc(Process *p, Eterm what);
void monitor_long_gc(Process *p, Uint time);
void monitor_large_heap(Process *p);
void monitor_generic(Process *p, Eterm type, Eterm spec);
Uint erts_trace_flag2bit(Eterm flag);
Eterm erts_bif_trace(int bif_index, Process* p, 
		     Eterm arg1, Eterm arg2, Eterm arg3, Uint *I);

int member(Eterm, Eterm);
void bin_write(CIO, byte*, int);
int intlist_to_buf(Eterm, byte*, int);

struct Sint_buf {
#ifdef ARCH_64
    char s[22];
#else
    char s[12];
#endif
};	
char* Sint_to_buf(Sint, struct Sint_buf*);

Eterm buf_to_intlist(Eterm**, byte*, int, Eterm);
int io_list_to_buf(Eterm, char*, int);
int io_list_len(Eterm);
int is_string(Eterm);
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

char* erts_get_system_version(int *len);

/*
 * Interface to erl_init
 */
void erl_init(void);
void erts_first_process(Eterm modname, void* code, unsigned size, int argc, char** argv);

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

struct trace_pattern_flags {
    unsigned int breakpoint : 1; /* Set if any other is set */
    unsigned int local      : 1; /* Local call trace breakpoint */
    unsigned int meta       : 1; /* Metadata trace breakpoint */
    unsigned int call_count : 1; /* Fast call count breakpoint */
};
extern const struct trace_pattern_flags erts_trace_pattern_flags_off;
int erts_set_trace_pattern(Eterm* mfa, int specified, 
			   Binary* match_prog_set, Binary *meta_match_prog_set,
			   int on, struct trace_pattern_flags,
			   Eterm meta_tracer_pid);

extern int                         erts_default_trace_pattern_is_on;
extern Binary                     *erts_default_match_spec;
extern Binary                     *erts_default_meta_match_spec;
extern struct trace_pattern_flags  erts_default_trace_pattern_flags;
extern Eterm                       erts_default_meta_tracer_pid;

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
extern void erts_match_prog_foreach_offheap(Binary *b,
					    void (*)(ErlOffHeap *, void *),
					    void *);

#define MATCH_SET_RETURN_TRACE 0x1 /* return trace requested */
#define MATCH_SET_RETURN_TO_TRACE 0x2 /* Misleading name, it is not actually
					 set by the match program, but by the
					 breakpoint functions */

/*
 * Flag values when tracing bif
 */
#define BIF_TRACE_AS_LOCAL  0x1
#define BIF_TRACE_AS_GLOBAL 0x2
#define BIF_TRACE_AS_META   0x4

#endif
