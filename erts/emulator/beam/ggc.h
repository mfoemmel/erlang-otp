#ifndef __GGC_H__
#define __GGC_H__

/*
 * GC macros, shared between ggc.c and hipe_${ARCH}_stack.c.
 */

#ifdef DEBUG
#define HARDDEBUG  1
/* See erl_debug.h for information about verbose levels */
/* GC_HEAP_TRACE is deprecated! Use different verbose levels instead */
/* GC_SWITCH_TRACE is deprecated! It was unused... */
/* OLD_HEAP_CREATION_TRACE is deprecated! Use different verbose levels instead */
#endif

#define MY_IS_MOVED(x)	(!is_header((x)))

#define MOVE_CONS(PTR,CAR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
									\
    HTOP[0] = CAR;		/* copy car */				\
    HTOP[1] = PTR[1];		/* copy cdr */				\
    gval = make_list(HTOP);	/* new location */			\
    *ORIG = gval;		/* redirect original reference */	\
    PTR[0] = THE_NON_VALUE;	/* store forwarding indicator */	\
    PTR[1] = gval;		/* store forwarding address */		\
    HTOP += 2;			/* update tospace htop */		\
} while(0)

#define MOVE_BOXED(PTR,HDR,HTOP,ORIG)					\
do {									\
    Eterm gval;								\
    Sint nelts;								\
									\
    ASSERT(is_header(HDR));						\
    gval = make_boxed(HTOP);						\
    *ORIG = gval;							\
    *HTOP++ = HDR;							\
    *PTR++ = gval;							\
    nelts = header_arity(HDR);						\
    switch ((HDR) & _HEADER_SUBTAG_MASK) {				\
    case SUB_BINARY_SUBTAG: nelts++; break;				\
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR-1))->num_free+1; break;	\
    }									\
    while (nelts--)							\
	*HTOP++ = *PTR++;						\
} while(0)

#define ptr_within(ptr, low, high) ((ptr) < (high) && (ptr) >= (low))

/* efficient range check */
#define in_area(ptr,start,nbytes) ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

#ifdef HARDDEBUG
extern int within(Eterm*, Process*);
#else
# define within(x,y) 1
#endif /* HARDDEBUG */

/*
 * This structure describes the rootset for the GC.
 */
typedef struct {
    Eterm* v[11];		/* Pointers to vectors with terms to GC
				 * (e.g. the stack).
				 */
    Uint sz[11];		/* Size of each vector. */
    Eterm* v_msg;		/* Pointer to messages to GC. */
    Eterm def_msg[32];		/* Default storage for messages (to avoid malloc). */
#ifdef HYBRID
    Uint n;
#if defined(HIPE)
    Process *p;			/* For scanning the nstack. */
#endif
#endif
} Rootset;


#ifdef HYBRID
extern char ma_gc_flags;

/* Tell the GC to look at all processes. This flag i s set in the
 * beginning of a major collection, and cleared at the end of minor
 * collections. This makes the GC look at all processes in the major
 * collection and the following minor, which is needed since there is
 * no old generation after a major collection.
 */
#define GC_INCLUDE_ALL  0x01

/* To let functions shared by local and global GC know this is a global GC */
#define GC_GLOBAL       0x02

/* In IncNMGC, to indicate that a GC-cycle is in progress */
#define GC_CYCLE        0x04

/* In IncNMGC, to indicate that the next GC cycle will be a major one */
#define GC_NEED_MAJOR   0x08

/* In IncNMGC, to indicate that the current GC cycle is a major one */
#define GC_MAJOR        0x10

/* In IncNMGC, to indicate that the last call to the collector
 * initiated a new collection cycle.
 * NOTE!!
 * This flag is only for communication with copy_stuct_lazy. copy_stuct_lazy
 * will clear this flag after a copy is done.
 */
#define GC_CYCLE_START  0x20
#endif


/* These functions are used by nmgc too. */
int setup_rootset(Process *p, Eterm *objv, int nobj, Rootset *rootset);
Uint collect_roots(Process* current, Eterm *objv, int nobj, Rootset rootset[]);
void restore_rootset(Process *p, Rootset *rootset);
#ifdef INCREMENTAL
void restore_one_rootset(Process *p, Rootset *rootset);
#endif

#endif /* __GGC_H__ */
