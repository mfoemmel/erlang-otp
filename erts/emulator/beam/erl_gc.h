#ifndef __ERL_GC_H__
#define __ERL_GC_H__

/* GC declarations shared by beam/erl_gc.c and hipe/hipe_gc.c */

#ifdef DEBUG
#  define HARDDEBUG 1
#endif

#define IS_MOVED(x)	(!is_header((x)))

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

#define in_area(ptr,start,nbytes) \
 ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))

extern Uint erts_test_long_gc_sleep;

#if defined(DEBUG) || defined(ERTS_OFFHEAP_DEBUG)
int within(Eterm *ptr, Process *p);
#endif

#endif /* __ERL_GC_H__ */
