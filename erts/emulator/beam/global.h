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

#define ERTS_MAX_NO_OF_ASYNC_THREADS 1024

#define MAXINDX 255

typedef struct cache {
    Eterm in_arr[MAXINDX];
    Eterm out_arr[MAXINDX];
} ErlCache;

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
#ifdef ERTS_SMP
    ErtsSmpPTimer *ptimer;
#else
    ErlTimer tm;                 /* Timer entry */
#endif
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
#define ERL_DE_FORCE_UNLOAD 2 
#define ERL_DE_RELOAD  3
#define ERL_DE_PERMANENT 4

#define ERL_DE_PROC_LOADED 0
#define ERL_DE_PROC_AWAIT_UNLOAD 1
#define ERL_DE_PROC_AWAIT_LOAD 2

/* Flags for drivers, put locking policy here /PaN */
#define ERL_DE_FL_KILL_PORTS 1

#define ERL_FL_CONSISTENT_MASK ( ERL_DE_FL_KILL_PORTS )

/* System specific load errors are returned as positive values */
#define ERL_DE_NO_ERROR 0
#define ERL_DE_LOAD_ERROR_NO_INIT -1
#define ERL_DE_LOAD_ERROR_FAILED_INIT -2
#define ERL_DE_LOAD_ERROR_BAD_NAME -3
#define ERL_DE_LOAD_ERROR_NAME_TO_LONG -4
#define ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY -5
#define ERL_DE_ERROR_UNSPECIFIED -6
#define ERL_DE_LOOKUP_ERROR_NOT_FOUND -7
#define ERL_DE_DYNAMIC_ERROR_OFFSET -10

typedef struct de_proc_entry {
    Process *proc;                   /* The process... */
    Uint    awaiting_status;         /* PROC_LOADED == Have loaded the driver
			                PROC_AWAIT_UNLOAD == Wants to be notified 
			                when we have unloaded the driver (was locked)
			                PROC_AWAIT_LOAD == Wants to be notified when we
			                reloaded the driver (old was locked) */
    Eterm   heap[REF_THING_SIZE];    /* "ref heap" */
    struct  de_proc_entry *next;
} DE_ProcEntry;

typedef struct {
    void         *handle;             /* Handle for DLL or SO (for dyn. drivers). */
    DE_ProcEntry *procs;              /* List of pids that have loaded this driver,
				         or that wait for it to change state */
    int          port_count;          /* Number of ports using the driver */
    Uint         flags;               /* ERL_DE_FL_KILL_PORTS */
    int          status;              /* ERL_DE_xxx */
    char         *full_path;          /* Full path of the driver */
    char         *reload_full_path;   /* If status == ERL_DE_RELOAD, this contains
				         full name of driver (path) */
    char         *reload_driver_name; /* ... and this contains the driver name */
    Uint         reload_flags;        /* flags for reloaded driver */
} DE_Handle;

/*
 * This structure represents a link to the next driver.
 */

typedef struct de_list {
    ErlDrvEntry    *drv;	/* Pointer to entry for driver. */
    DE_Handle      *de_hndl;	/* Handle for DLL or SO (for dynamic drivers). */
    struct de_list *next;	/* Pointer to next. */
} DE_List;

extern DE_List *driver_list;

extern void erts_ddll_init(void);
extern void erts_ddll_lock_driver(DE_Handle *dh, char *name);
extern void erts_ddll_increment_port_count(DE_Handle *dh);
extern void erts_ddll_decrement_port_count(DE_Handle *dh);
extern char *erts_ddll_error(int code);
extern void erts_ddll_proc_dead(Process *p, Uint32 plocks);
extern int erts_ddll_driver_ok(DE_Handle *dh);
extern void erts_ddll_remove_monitor(Process *p, Eterm ref, Uint32 plocks);
extern Eterm erts_ddll_monitor_driver(Process *p, Eterm description, Uint32 plocks);
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
    erts_refc_t refc;
#ifdef ARCH_32
    Uint32 align__; /* *DO NOT USE* only for alignment. */
#endif
    /* Add fields BEFORE this, otherwise the drivers crash */
    long orig_size;
    char orig_bytes[1]; /* to be continued */
} Binary;

/*
 * 'Binary' alignment:
 *   Address of orig_bytes[0] of a Binary should always be 8-byte aligned.
 * It is assumed that the flags, refc, and orig_size fields are 4 bytes on
 * 32-bits architectures and 8 bytes on 64-bits architectures.
 */


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

extern Uint erts_max_ports;
extern Uint erts_port_tab_index_mask;
/* controls warning mapping in error_logger */

extern Eterm node_cookie;
extern Uint bytes_out;		/* no bytes written out */
extern Uint bytes_in;		/* no bytes sent into the system */
extern Uint display_items;	/* no of items to display in traces etc */
extern Uint display_loads;	/* print info about loaded modules */

extern int erts_backtrace_depth;
extern erts_smp_atomic_t erts_max_gen_gcs;

extern int erts_disable_tolerant_timeofday;

/* Defines to ease the change of memory architecture */
#  define HEAP_START(p)     (p)->heap
#  define HEAP_TOP(p)       (p)->htop
#  define HEAP_LIMIT(p)     (p)->stop
#  define HEAP_END(p)       (p)->hend
#  define HEAP_SIZE(p)      (p)->heap_sz
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

#ifdef HYBRID

/* Message Area heap pointers */
extern Eterm *global_heap;             /* Heap start */
extern Eterm *global_hend;             /* Heap end */
extern Eterm *global_htop;             /* Heap top (heap pointer) */
extern Eterm *global_saved_htop;       /* Saved heap top (heap pointer) */
extern Uint   global_heap_sz;          /* Heap size, in words */
extern Eterm *global_old_heap;         /* Old generation */
extern Eterm *global_old_hend;
extern ErlOffHeap erts_global_offheap; /* Global MSO (OffHeap) list */

extern Uint16 global_gen_gcs;
extern Uint16 global_max_gen_gcs;
extern Uint   global_gc_flags;

#ifdef INCREMENTAL
#define ACTIVATE(p)
#define DEACTIVATE(p)
#define IS_ACTIVE(p) 1

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
extern Eterm *global_old_htop;
extern Eterm *global_high_water;
#define ACTIVATE(p)   (p)->active = 1;
#define DEACTIVATE(p) (p)->active = 0;
#define IS_ACTIVE(p)  ((p)->active != 0)
#define INC_ACTIVATE(p)
#define INC_IS_ACTIVE(p) 1
#endif /* INCREMENTAL */

#else
#  define ACTIVATE(p)
#  define DEACTIVATE(p)
#  define IS_ACTIVE(p) 1
#  define INC_ACTIVATE(p)
#endif /* HYBRID */

#ifdef HYBRID
extern Uint global_heap_min_sz;
#endif

extern int bif_reductions;      /* reductions + fcalls (when doing call_bif) */
extern int stackdump_on_exit;

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
#define ERTS_IMMORTAL_PORT (1 << 9)

/* binary.c */

Eterm erts_new_heap_binary(Process *p, byte *buf, int len, byte** datap);
Eterm new_binary(Process*, byte*, int);
#if !defined(HEAP_FRAG_ELIM_TEST)
Eterm new_binary_arith(Process*, byte*, int);
#endif
Eterm erts_realloc_binary(Eterm bin, size_t size);
void erts_cleanup_mso(ProcBin* pb);

/* erl_bif_info.c */

void erts_bif_info_init(void);

/* bif.c */
Eterm erts_make_ref(Process *);
void erts_queue_monitor_message(Process *, Uint32*, Eterm, Eterm, Eterm, Eterm);
void erts_init_bif(void);

/* erl_bif_port.c */

/* erl_bif_trace.c */
void erts_system_monitor_clear(Process *c_p);
void erts_do_pending_suspend(Process *c_p, Uint32 enter_locks);

/* beam_load.c */
int erts_load_module(Process *c_p, Uint32 c_p_locks,
		     Eterm group_leader, Eterm* mod, byte* code, int size);
void init_load(void);
Eterm* find_function_from_pc(Eterm* pc);
Eterm erts_module_info_0(Process* p, Eterm module);
Eterm erts_module_info_1(Process* p, Eterm module, Eterm what);
Eterm erts_make_stub_module(Process* p, Eterm Mod, Eterm Beam, Eterm Info);

/* break.c */
void init_break_handler(void);
void erts_set_ignore_break(void);
void erts_replace_intr(void);
void process_info(int, void *);
void print_process_info(int, void *, Process*);
void info(int, void *);
void loaded(int, void *);

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
#define RRMA_DEFAULT_SIZE 256
#define RRMA_STORE(p,ptr,src) do {                                      \
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

/* Note that RRMA_REMOVE decreases the given index after deletion. 
 * This is done so that a loop with an increasing index can call
 * remove without having to decrease the index to see the element
 * placed in the hole after the deleted element.
 */
#define RRMA_REMOVE(p,index) do {                                 \
        p->rrsrc[index] = p->rrsrc[--p->nrr];                     \
        p->rrma[index--] = p->rrma[p->nrr];                       \
    } while(0);


/* The MessageArea STACKs are used while copying messages to the
 * message area.
 */
#define MA_STACK_EXTERNAL_DECLARE(type,_s_)     \
    typedef type ma_##_s_##_type;               \
    extern ma_##_s_##_type *ma_##_s_##_stack;   \
    extern Uint ma_##_s_##_top;                 \
    extern Uint ma_##_s_##_size;

#define MA_STACK_DECLARE(_s_)                                           \
    ma_##_s_##_type *ma_##_s_##_stack; Uint ma_##_s_##_top; Uint ma_##_s_##_size;

#define MA_STACK_ALLOC(_s_) do {                                        \
    ma_##_s_##_top = 0;                                                 \
    ma_##_s_##_size = 512;                                              \
    ma_##_s_##_stack = (ma_##_s_##_type*)erts_alloc(ERTS_ALC_T_OBJECT_STACK, \
                       sizeof(ma_##_s_##_type) * ma_##_s_##_size);      \
    ERTS_PROC_MORE_MEM(sizeof(ma_##_s_##_type) * ma_##_s_##_size);      \
} while(0)


#define MA_STACK_PUSH(_s_,val) do {                                     \
    ma_##_s_##_stack[ma_##_s_##_top++] = (val);                         \
    if (ma_##_s_##_top == ma_##_s_##_size)                              \
    {                                                                   \
        ERTS_PROC_LESS_MEM(sizeof(ma_##_s_##_type) * ma_##_s_##_size);  \
        ma_##_s_##_size *= 2;                                           \
        ERTS_PROC_MORE_MEM(sizeof(ma_##_s_##_type) * ma_##_s_##_size);  \
        ma_##_s_##_stack =                                              \
            (ma_##_s_##_type*) erts_realloc(ERTS_ALC_T_OBJECT_STACK,    \
                                           (void*)ma_##_s_##_stack,     \
                            sizeof(ma_##_s_##_type) * ma_##_s_##_size); \
    }                                                                   \
} while(0)

#define MA_STACK_POP(_s_) (ma_##_s_##_top != 0 ? ma_##_s_##_stack[--ma_##_s_##_top] : 0)
#define MA_STACK_TOP(_s_) (ma_##_s_##_stack[ma_##_s_##_top - 1])
#define MA_STACK_UPDATE(_s_,offset,value)                               \
  *(ma_##_s_##_stack[ma_##_s_##_top - 1] + (offset)) = (value)
#define MA_STACK_SIZE(_s_) (ma_##_s_##_top)
#define MA_STACK_ELM(_s_,i) ma_##_s_##_stack[i]

MA_STACK_EXTERNAL_DECLARE(Eterm,src);
MA_STACK_EXTERNAL_DECLARE(Eterm*,dst);
MA_STACK_EXTERNAL_DECLARE(Uint,offset);


#ifdef INCREMENTAL
extern Eterm *ma_pending_stack;
extern Uint ma_pending_top;
extern Uint ma_pending_size;

#define NO_COPY(obj) (IS_CONST(obj) ||                         \
                      (((ptr_val(obj) >= global_heap) &&       \
                        (ptr_val(obj) < global_htop)) ||       \
                       ((ptr_val(obj) >= inc_fromspc) &&       \
                        (ptr_val(obj) < inc_fromend)) ||       \
                       ((ptr_val(obj) >= global_old_heap) &&   \
                        (ptr_val(obj) < global_old_hend))))

#else

#define NO_COPY(obj) (IS_CONST(obj) ||                        \
                      (((ptr_val(obj) >= global_heap) &&      \
                        (ptr_val(obj) < global_htop)) ||      \
                       ((ptr_val(obj) >= global_old_heap) &&  \
                        (ptr_val(obj) < global_old_hend))))

#endif /* INCREMENTAL */

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
extern int erts_do_net_exits(DistEntry*);
extern int distribution_info(int, void *);
extern int is_node_name_atom(Eterm a);

extern int erts_net_message(DistEntry*, byte*, int, byte*, int);

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

/* external.c */
int erts_to_external_format(DistEntry*, Eterm, byte**, byte**, Uint *);
Eterm erts_from_external_format(DistEntry*, Eterm**, byte**, ErlOffHeap*);
Eterm encode_size_struct(Eterm, unsigned);
int decode_size(byte*, int);

/* ggc.c */


typedef struct {
    Uint garbage_collections;
    Uint reclaimed;
} ErtsGCInfo;

void erts_gc_info(ErtsGCInfo *gcip);
void erts_init_gc(void);
int erts_garbage_collect(Process*, int, Eterm*, int);
#if defined(HEAP_FRAG_ELIM_TEST)
Eterm erts_gc_after_bif_call(Process* p, Eterm result);
#endif
Uint erts_next_heap_size(Uint, Uint);
Eterm erts_heap_sizes(Process* p);
void erts_shrink_new_heap(Process *p, Uint new_sz, Eterm *objv, int nobj);

void erts_offset_off_heap(ErlOffHeap *, Sint, Eterm*, Eterm*);
void erts_offset_heap_ptr(Eterm*, Uint, Sint, Eterm*, Eterm*);
void erts_offset_heap(Eterm*, Uint, Sint, Eterm*, Eterm*);

#ifdef HYBRID
int erts_global_garbage_collect(Process*, int, Eterm*, int);
#endif

/* io.c */

void wake_process_later(Eterm, Process*);
int open_driver(ErlDrvEntry*, Eterm, char*, SysDriverOpts*);
void close_port(Eterm);
void init_io(void);
void cleanup_io(void);
void erts_do_exit_port(Eterm, Eterm, Eterm);
void erts_port_command(Process *, Eterm, Port *, Eterm);
Eterm erts_port_control(Process*, Port*, Uint, Eterm);
int write_port(Eterm caller_id, int p, Eterm list);
void print_port_info(int, void *, int);
void dist_port_command(Port*, byte*, int);
void input_ready(int, int);
void output_ready(int, int);
void event_ready(int, int, ErlDrvEventData);
void driver_report_exit(int, int);
LineBuf* allocate_linebuf(int);
int async_ready(int ix, void* data);
Sint erts_test_next_port(int, Uint);
int erts_driver_attach_port(ErlDrvPort ix);
int erts_driver_detach_port(ErlDrvPort ix);

/*
 * erts_smp_io_lock(), and erts_smp_io_unlock() are declared in sys.h
 * since they are needed by sys files.
 */
#ifdef ERTS_SMP
int erts_io_trylock(void);
int erts_io_safe_lock(Process *, Uint32);
int erts_lc_io_is_locked(void);
#endif

ERTS_GLB_INLINE int erts_smp_io_trylock(void);
ERTS_GLB_INLINE int erts_smp_io_safe_lock(Process *p, Uint32 plocks);
ERTS_GLB_INLINE int erts_smp_lc_io_is_locked(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE int
erts_smp_io_trylock(void)
{
#ifdef ERTS_SMP
    return erts_io_trylock();
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_smp_io_safe_lock(Process *p, Uint32 plocks)
{
#ifdef ERTS_SMP
    return erts_io_safe_lock(p, plocks);
#else
    return 0;
#endif
}

ERTS_GLB_INLINE int
erts_smp_lc_io_is_locked(void)
{
#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    return erts_lc_io_is_locked();
#else
    return 0;
#endif
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* port lookup */

#define INVALID_PORT(port, port_id) \
((port) == NULL || \
 (port)->status == FREE || \
 (port)->id != (port_id) || \
 ((port)->status & CLOSING))

/* Invalidate trace port if anything suspicious, for instance
 * that the port is a distribution port or it is busy.
 */
#define INVALID_TRACER_PORT(port, port_id) \
((port) == NULL || \
 (port)->id != (port_id) || \
 (port)->status == FREE || \
 ((port)->status & (EXITING|CLOSING|PORT_BUSY|DISTRIBUTION)))

ERTS_GLB_INLINE Port*erts_id2port(Eterm id, Process *c_p, Uint32 c_p_locks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Port*
erts_id2port(Eterm id, Process *c_p, Uint32 c_p_locks)
{
    int ix;

    if (is_not_internal_port(id))
	return NULL;

#ifdef ERTS_SMP
    if (!c_p || !c_p_locks)
	erts_smp_io_lock();
    else {
	if (erts_smp_io_trylock() == EBUSY) {
	    /* Unlock process locks, and acquire locks in lock order... */
	    erts_smp_proc_unlock(c_p, c_p_locks);
	    erts_smp_io_lock();
	    erts_smp_proc_lock(c_p, c_p_locks);
	    if (ERTS_PROC_IS_EXITING(c_p))
		goto error;
	}
    }
#endif

    ix = internal_port_index(id);
    if (!INVALID_PORT(&erts_port[ix], id))
	return &erts_port[ix];
#ifdef ERTS_SMP
 error:
    erts_smp_io_unlock();
#endif
    return NULL;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

/* erl_obsolete.c */
void erts_init_obsolete(void);

/* time.c */

ERTS_GLB_INLINE long do_time_read_and_reset(void);
#ifdef ERTS_TIMER_THREAD
ERTS_GLB_INLINE int next_time(void);
ERTS_GLB_INLINE void bump_timer(long);
#else
int next_time(void);
void bump_timer(long);
extern erts_smp_atomic_t do_time;	/* set at clock interrupt */
ERTS_GLB_INLINE void do_time_add(long);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

#ifdef ERTS_TIMER_THREAD
ERTS_GLB_INLINE long do_time_read_and_reset(void) { return 0; }
ERTS_GLB_INLINE int next_time(void) { return -1; }
ERTS_GLB_INLINE void bump_timer(long ignore) { }
#else
ERTS_GLB_INLINE long do_time_read_and_reset(void)
{
    return erts_smp_atomic_xchg(&do_time, 0L);
}
ERTS_GLB_INLINE void do_time_add(long elapsed)
{
    erts_smp_atomic_add(&do_time, elapsed);
}
#endif

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

void init_time(void);
void erl_set_timer(ErlTimer*, ErlTimeoutProc, ErlCancelProc, void*, Uint);
void erl_cancel_timer(ErlTimer*);
Uint time_left(ErlTimer *);

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

void erts_cleanup_offheap(ErlOffHeap *offheap);
void erts_cleanup_externals(ExternalThing *);

Uint erts_fit_in_bits(Uint);
int list_length(Eterm);
Export* erts_find_function(Eterm, Eterm, unsigned int);
int erts_is_builtin(Eterm, Eterm, int);
Uint32 make_broken_hash(Eterm, Uint32);
Uint32 block_hash(byte *, unsigned, Uint32);
Uint32 make_hash2(Eterm);
Uint32 make_hash(Eterm, Uint32);


Eterm erts_bld_atom(Uint **hpp, Uint *szp, char *str);
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

void erts_init_utils(void);
void erts_init_utils_mem(void);

erts_dsprintf_buf_t *erts_create_tmp_dsbuf(Uint);
void erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *);

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
void erts_trace_check_exiting(Eterm exiting);
Eterm erts_set_system_seq_tracer(Process *c_p, Uint32 c_p_locks, Eterm new);
Eterm erts_get_system_seq_tracer(void);
void erts_change_default_tracing(int setflags, Uint *flagsp, Eterm *tracerp);
void erts_get_default_tracing(Uint *flagsp, Eterm *tracerp);
void erts_set_system_monitor(Eterm monitor);
Eterm erts_get_system_monitor(void);

#ifdef ERTS_SMP
void erts_check_my_tracer_proc(Process *);
void erts_block_sys_msg_dispatcher(void);
void erts_release_sys_msg_dispatcher(void);
void erts_foreach_sys_msg_in_q(void (*func)(Eterm,
					    Eterm,
					    Eterm,
					    ErlHeapFragment *));
void erts_queue_error_logger_message(Eterm, Eterm, ErlHeapFragment *);
#endif

void erts_send_sys_msg_proc(Eterm, Eterm, Eterm, ErlHeapFragment *);
void trace_send(Process*, Eterm, Eterm);
void trace_receive(Process*, Eterm);
Uint32 erts_call_trace(Process *p, Eterm mfa[], Binary *match_spec, Eterm* args,
		       int local, Eterm *tracer_pid);
void erts_trace_return(Process* p, Eterm* fi, Eterm retval, Eterm *tracer_pid);
void erts_trace_exception(Process* p, Eterm mfa[], Eterm class, Eterm value, 
			  Eterm *tracer);
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
int erts_trace_flags(Eterm List, 
		 Uint *pMask, Eterm *pTracer, int *pCpuTimestamp);
Eterm erts_bif_trace(int bif_index, Process* p, 
		     Eterm arg1, Eterm arg2, Eterm arg3, Uint *I);

#ifdef ERTS_SMP
void erts_send_pending_trace_msgs(ErtsSchedulerData *esdp);
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)				\
do {									\
    if ((ESDP)->pending_trace_msgs)					\
	erts_send_pending_trace_msgs((ESDP));				\
} while (0)
#else
#define ERTS_SMP_CHK_PEND_TRACE_MSGS(ESDP)
#endif

int member(Eterm, Eterm);
void bin_write(int, void*, byte*, int);
int intlist_to_buf(Eterm, char*, int); /* most callers pass plain char*'s */

struct Sint_buf {
#ifdef ARCH_64
    char s[22];
#else
    char s[12];
#endif
};	
char* Sint_to_buf(Sint, struct Sint_buf*);

Eterm buf_to_intlist(Eterm**, char*, int, Eterm); /* most callers pass plain char*'s */
int io_list_to_buf(Eterm, char*, int);
int io_list_to_buf2(Eterm, char*, int);
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
Eterm erts_bnot(Process* p, Eterm arg);

#if defined(HEAP_FRAG_ELIM_TEST)
Eterm erts_gc_mixed_plus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_minus(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_times(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_mixed_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_div(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_int_rem(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_band(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bxor(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_bnot(Process* p, Eterm* reg, Uint live);

Eterm erts_gc_length_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_size_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_abs_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_float_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_round_1(Process* p, Eterm* reg, Uint live);
Eterm erts_gc_trunc_1(Process* p, Eterm* reg, Uint live);
#endif

Uint erts_current_reductions(Process* current, Process *p);

int erts_print_system_version(int to, void *arg, Process *c_p);

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
void
erts_get_default_trace_pattern(int *trace_pattern_is_on,
			       Binary **match_spec,
			       Binary **meta_match_spec,
			       struct trace_pattern_flags *trace_pattern_flags,
			       Eterm *meta_tracer_pid);
void erts_bif_trace_init(void);

/*
** Call_trace uses this API for the parameter matching functions
*/
    struct erl_heap_fragment* saved_program_buf;

#define MatchSetRef(MPSP) 			\
do {						\
    if ((MPSP) != NULL) {			\
	erts_refc_inc(&(MPSP)->refc, 1);	\
    }						\
} while (0)

#define MatchSetUnref(MPSP)					\
do {								\
    if (((MPSP) != NULL) && erts_refc_dectest(&(MPSP)->refc, 0) <= 0) { \
	erts_match_set_free(MPSP);				\
    }								\
} while(0)

#define MatchSetGetSource(MPSP) erts_match_set_get_source(MPSP)

extern Binary *erts_match_set_compile(Process *p, Eterm matchexpr);
Eterm erts_match_set_lint(Process *p, Eterm matchexpr); 
extern void erts_match_set_release_result(Process* p);
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
#define MATCH_SET_EXCEPTION_TRACE 0x4 /* exception trace requested */
#define MATCH_SET_RX_TRACE (MATCH_SET_RETURN_TRACE|MATCH_SET_EXCEPTION_TRACE)
/*
 * Flag values when tracing bif
 */
#define BIF_TRACE_AS_LOCAL  0x1
#define BIF_TRACE_AS_GLOBAL 0x2
#define BIF_TRACE_AS_META   0x4

/* Should maybe be placed in erl_message.h, but then we get an include mess. */

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			Uint32 *receiver_locks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Eterm *
erts_alloc_message_heap(Uint size,
			ErlHeapFragment **bpp,
			ErlOffHeap **ohpp,
			Process *receiver,
			Uint32 *receiver_locks)
{
    Eterm *hp;
#if defined(HEAP_FRAG_ELIM_TEST)
    if (HEAP_LIMIT(receiver) - HEAP_TOP(receiver) >= size) {
	hp = HEAP_TOP(receiver);
	HEAP_TOP(receiver) = hp + size;
	*bpp = NULL;
	*ohpp = &MSO(receiver);
    } else {
	ErlHeapFragment* bp = new_message_buffer(size);
	hp = bp->mem;
	*bpp = bp;
	*ohpp = &bp->off_heap;
    }
#else
#ifdef ERTS_SMP
    if (*receiver_locks & ERTS_PROC_LOCK_MAIN) {
    allocate_on_heap:
#endif
#if defined(ERTS_SMP) && defined(HEAP_FRAG_ELIM_TEST)
	if (HEAP_LIMIT(receiver) - HEAP_TOP(receiver) <= size)
	    goto allocate_in_mbuf;
	hp = HEAP_TOP(receiver);
	HEAP_TOP(receiver) = hp + size;
#else
	hp = HAlloc(receiver, size);
#endif
	*bpp = NULL;
	*ohpp = &MSO(receiver);
#ifdef ERTS_SMP
    }
    else if (erts_proc_trylock(receiver, ERTS_PROC_LOCK_MAIN) == 0) {
	*receiver_locks |= ERTS_PROC_LOCK_MAIN;
	goto allocate_on_heap;
    }
    else {
	ErlHeapFragment *bp;
#ifdef HEAP_FRAG_ELIM_TEST
    allocate_in_mbuf:
#endif
	bp = new_message_buffer(size);
	hp = bp->mem;
	*bpp = bp;
	*ohpp = &bp->off_heap;
    }
#endif
#endif
    return hp;
}

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif
