#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#include "ggc.h"
#include "erl_binary.h"
#include "erl_nmgc.h"
#include "erl_debug.h"
#if HIPE
#include "hipe_bif0.h" /* for hipe_constants_{start,next} */
#endif

#ifdef INCREMENTAL_GC
/***************************************************************************
 *                                                                         *
 *          Incremental Garbage Collector for the Young Generation         *
 *                                                                         *
 ***************************************************************************/

#define DECREASE_WORK(n) inc_words_to_go -= (n);

#define INC_MOVE_CONS(PTR,HTOP,ORIG)	        	        	\
do {									\
    HTOP[0] = PTR[0];		/* copy car */				\
    HTOP[1] = PTR[1];		/* copy cdr */				\
    *ORIG = make_list(HTOP);	/* redirect original reference */	\
    HTOP += 2;			/* update tospace htop */		\
    DECREASE_WORK(2);                                                   \
} while(0)

#define INC_MOVE_BOXED(PTR,HTOP,ORIG)		        		\
do {									\
    Eterm gval;								\
    Sint nelts;								\
    Eterm hdr = *(PTR);                                                 \
									\
    ASSERT(is_header(hdr));						\
    gval = make_boxed(HTOP);						\
    *ORIG = gval;							\
    *HTOP++ = *PTR++;							\
    nelts = header_arity(hdr);						\
    DECREASE_WORK(nelts);                                               \
    switch ((hdr) & _HEADER_SUBTAG_MASK) {				\
    case SUB_BINARY_SUBTAG: nelts++; break;				\
    case FUN_SUBTAG: nelts+=((ErlFunThing*)(PTR-1))->num_free+1; break;	\
    }									\
    while (nelts--)							\
	*HTOP++ = *PTR++;						\
} while(0)

/*
 * The standard global_heap will act as nursery one.
 */
Eterm *inc_n2;
Eterm *inc_n2_end;
Eterm *inc_n1_scn_ptr;
Eterm **fwdptrs;
Eterm *inc_alloc_limit;
Process *inc_active_proc;
Process *inc_active_last;

static Eterm *inc_last_nursery;
static long   inc_n2_size = 0;  /* Will only contain accual size when growing n2 */
static int    inc_words_to_go;

/*
void print_one_rootset(Rootset root)
{
    int rts = root.n;
    while (rts--) {
        int sz = root.sz[rts];
        Eterm *ptr = root.v[rts];
        while (sz--) {
            printf("Root: 0x%08x\r\n",*ptr++);
        }
    }
}

void print_rootset(Rootset *root, int n)
{
    while (n--) {
        print_one_rootset(root[n]);
    }
}
*/

#ifdef INC_TIME_BASED
#if USE_PERFCTR

/*
 * This uses the Linux perfctr extension to virtualise the
 * time-stamp counter.
 */
#include "libperfctr.h"
static struct vperfctr *vperfctr;
static double cpu_khz;
static double tsc_to_cpu_mult;

static void inc_start_hrvtime(void)
{
    struct perfctr_info info;
    struct vperfctr_control control;

    if( vperfctr != NULL )
	return;
    vperfctr = vperfctr_open();
    if( vperfctr == NULL )
	return;
    if( vperfctr_info(vperfctr, &info) >= 0 ) {
	cpu_khz = (double)info.cpu_khz;
	tsc_to_cpu_mult = (double)(info.tsc_to_cpu_mult ? : 1);
	if( info.cpu_features & PERFCTR_FEATURE_RDTSC ) {
	    memset(&control, 0, sizeof control);
	    control.cpu_control.tsc_on = 1;
	    if( vperfctr_control(vperfctr, &control) >= 0 )
		return;
	}
    }
    vperfctr_close(vperfctr);
    vperfctr = NULL;
}

#define inc_get_hrvtime() (((double)vperfctr_read_tsc(vperfctr) * tsc_to_cpu_mult) / cpu_khz)

#endif /* USE_PERFCTR */
#endif /* INC_TIME_BASED */

#ifdef INC_TIME_BASED
#  define timeslice 1 /* milli seconds */
#  define WORK_MORE (inc_get_hrvtime() < start_time + timeslice)
#else
#  define inc_min_work 1000 /* words */
#  define WORK_MORE (inc_words_to_go > 0)
#endif

void erts_init_incgc(void)
{
    inc_last_nursery =
        (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP,
                                  global_heap_sz * sizeof(Eterm));
    inc_n2 = inc_n2_end = NULL;
    fwdptrs = erts_alloc(ERTS_ALC_T_ROOTSET,global_heap_sz * sizeof(Eterm*));
    ERTS_PROC_MORE_MEM(sizeof(Eterm*) * global_heap_sz);
    inc_active_proc = NULL;
    inc_active_last = NULL;
    inc_alloc_limit = global_hend;
#ifdef INC_TIME_BASED
    inc_start_hrvtime();
#endif
}

void erts_cleanup_incgc(void)
{
    if (inc_n2 != NULL)
        inc_last_nursery = inc_n2;

    ERTS_HEAP_FREE(ERTS_ALC_T_HEAP,(void*)inc_last_nursery,
                   global_heap_sz * sizeof(Eterm));
    erts_free(ERTS_ALC_T_ROOTSET,(void*)fwdptrs);
    ERTS_PROC_LESS_MEM(global_heap_sz * sizeof(Eterm*));
}

static Eterm *inc_rescueHeap(Process *p,Eterm *start, Eterm *end, Eterm *objv, int nobj)
{
    Eterm *hp = start;

    CHECK_MEMORY(start,end);
    while (WORK_MORE && hp != end) {
        //while (hp != end) {
        Eterm gval = *hp;
        switch (primary_tag(gval)) {

        case TAG_PRIMARY_LIST: {
	    Eterm *ptr = list_val(gval);
            if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                if (fwdptrs[ptr - inc_n2] != 0)
                    *hp++ = make_list(NM_FORWARD_VALUE(ptr));
                else {
#ifdef NOMOVE
                    Eterm *old_htop =
                        erts_nm_alloc(p,2,objv,nobj);
                    /* NM_STORE(build,old_htop,2); */
#endif
                    NM_MARK_FORWARD(ptr,old_htop);
                    INC_MOVE_CONS(ptr,old_htop,hp++);
                }
            }
            else
                ++hp;
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *ptr = boxed_val(gval);
            if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                if (fwdptrs[ptr - inc_n2] != 0) {
                    *hp++ = make_boxed(NM_FORWARD_VALUE(ptr));
                }
                else {
#ifdef NOMOVE
                    Eterm *old_htop =
                        erts_nm_alloc(p,BOXED_NEED(ptr,*ptr),objv,nobj);
                    /* NM_STORE(build,old_htop,BOXED_NEED(ptr,val)); */
                    NM_MARK_FORWARD(ptr,old_htop);
#endif
                    INC_MOVE_BOXED(ptr,old_htop,hp++);
                }
            }
            else
                ++hp;
            continue;
        }

        case TAG_PRIMARY_HEADER: {
            if (header_is_thing(gval))
                hp += (thing_arityval(gval)+1);
            else
                hp++;
            continue;
        }
            
        default:
	    hp++;
	    continue;
        }
    }

    return hp;
}

static Eterm *inc_rescueHeapNoBreak(Process *p,Eterm *start, Eterm *end, Eterm *objv, int nobj);
static Eterm *inc_rescueHeapNoBreak(Process *p,Eterm *start, Eterm *end, Eterm *objv, int nobj)
{
    Eterm *hp = start;

    CHECK_MEMORY(start,end);
    while (hp != end) {
        Eterm gval = *hp;
        switch (primary_tag(gval)) {

        case TAG_PRIMARY_LIST: {
	    Eterm *ptr = list_val(gval);
            if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                if (fwdptrs[ptr - inc_n2] != 0)
                    *hp++ = make_list(NM_FORWARD_VALUE(ptr));
                else {
#ifdef NOMOVE
                    Eterm *old_htop =
                        erts_nm_alloc(p,2,objv,nobj);
                    /* NM_STORE(build,old_htop,2); */
#endif
                    NM_MARK_FORWARD(ptr,old_htop);
                    INC_MOVE_CONS(ptr,old_htop,hp++);
                }
            }
            else
                ++hp;
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *ptr = boxed_val(gval);
            if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                if (fwdptrs[ptr - inc_n2] != 0) {
                    *hp++ = make_boxed(NM_FORWARD_VALUE(ptr));
                }
                else {
#ifdef NOMOVE
                    Eterm *old_htop =
                        erts_nm_alloc(p,BOXED_NEED(ptr,*ptr),objv,nobj);
                    /* NM_STORE(build,old_htop,BOXED_NEED(ptr,val)); */
                    NM_MARK_FORWARD(ptr,old_htop);
#endif
                    INC_MOVE_BOXED(ptr,old_htop,hp++);
                }
            }
            else
                ++hp;
            continue;
        }

        case TAG_PRIMARY_HEADER: {
            if (header_is_thing(gval))
                hp += (thing_arityval(gval)+1);
            else
                hp++;
            continue;
        }
            
        default:
	    hp++;
	    continue;
        }
    }
}

void erts_inc_gc(Process* p, int need, Eterm* objv_in, int nobj_in)
{
    int nobj = nobj_in + p->arity;
    Eterm* objv;
#ifdef INC_TIME_BASED
    double start_time = inc_get_hrvtime();
    int work_left_before = inc_words_to_go;
#endif

    BM_STOP_TIMER(copy);
    //BM_MMU_READ();
    BM_COUNT(minor_global_garbage_cols);
    BM_RESET_TIMER(gc);
    BM_START_TIMER(gc);

    objv = erts_alloc(ERTS_ALC_T_ROOTSET,nobj * sizeof(Eterm));
    ERTS_PROC_MORE_MEM(sizeof(Eterm) * nobj);

    {
        int i;
        for(i = 0; i < nobj_in; i++)
            objv[i] = objv_in[i];
        for(; i < nobj; i++)
            objv[i] = p->arg_reg[i - nobj_in];
    }

    VERBOSE_MESSAGE((VERBOSE_NOISY,
                     "Incremental GC START  Caused by: %s  Need: %d\n",
                     print_pid(p),need));

    ma_gc_flags |= GC_GLOBAL;
#ifndef INC_TIME_BASED
    /* Decide how much work to do this GC-slice */
    inc_words_to_go = (need > inc_min_work) ? need : inc_min_work;
#endif

 inc_collection:

    //BM_SWAP_TIMER(gc,misc0);
    if (inc_n2 == NULL) {
        VERBOSE_MESSAGE((VERBOSE_NOISY,"New collection cycle\n"));
        BM_COUNT(gc_cycles);
        inc_n2 = global_heap;
        inc_n2_end = global_htop;
        global_heap = global_htop = inc_last_nursery;
        global_hend = global_heap + global_heap_sz;
        inc_n1_scn_ptr = global_heap;
#ifdef INC_TIME_BASED
        work_left_before = inc_words_to_go = global_heap_sz;
#endif
#ifdef DEBUG
        inc_last_nursery = NULL;
#endif
        memset(fwdptrs,0,global_heap_sz * sizeof(Eterm));

        //printf("Fromspace: 0x%08x - 0x%08x\r\n",(int)inc_n2,(int)inc_n2_end);
        //print_tagged_memory(inc_n2,inc_n2_end);
        //print_nm_heap();

        {
            int i;
            for (i = 0; i < erts_num_active_procs; i++) {
                Process *cp = erts_active_procs[i];
                INC_ACTIVATE(cp);
            }
        }
    }

    /* Scan rootset for new pointers to N2 */
    while (WORK_MORE && inc_active_proc) {
        Rootset rootset;
        Process *cp = inc_active_proc;

        ASSERT(INC_IS_ACTIVE(cp));

        if (cp == p)
            rootset.n = setup_rootset(cp, objv, nobj, &rootset);
        else
            rootset.n = setup_rootset(cp, cp->arg_reg, cp->arity, &rootset);

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Scan %s\n",print_pid(cp)));
        //MA_GENSWEEP_NSTACK(cp, old_htop, n_htop, objv, nobj);

        while (WORK_MORE && rootset.n--) {
            Eterm *g_ptr = rootset.v[rootset.n];
            Uint g_sz = rootset.sz[rootset.n];

            while (WORK_MORE && g_sz--) {
                Eterm gval = *g_ptr;
                switch (primary_tag(gval)) {
                case TAG_PRIMARY_LIST: {
                    Eterm *ptr = list_val(gval);
                    if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                        if (NM_IS_FORWARDED(ptr)) {
                            *g_ptr++ = make_list(NM_FORWARD_VALUE(ptr));
                        }
                        else {
#ifdef NOMOVE
                            Eterm *old_htop =
                                erts_nm_alloc(p,2,objv,nobj);
                            /* NM_STORE(build,old_htop,2); */
#endif
                            NM_MARK_FORWARD(ptr,old_htop);
                            INC_MOVE_CONS(ptr,old_htop,g_ptr++);
                        }
                    }
                    else
                        ++g_ptr;
                    continue;
                }

                case TAG_PRIMARY_BOXED: {
                    Eterm *ptr = boxed_val(gval);
                    if (ptr_within(ptr,inc_n2,inc_n2_end)) {
                        if (NM_IS_FORWARDED(ptr)) {
                            *g_ptr++ = make_boxed(NM_FORWARD_VALUE(ptr));
                        }
                        else {
#ifdef NOMOVE
                            Eterm *old_htop =
                                erts_nm_alloc(p,BOXED_NEED(ptr,*ptr),
                                              objv,nobj);
                            /* NM_STORE(build,old_htop,BOXED_NEED(ptr,val)); */
                            NM_MARK_FORWARD(ptr,old_htop);
#endif
                            INC_MOVE_BOXED(ptr,old_htop,g_ptr++);
                        }
                    }
                    else
                        ++g_ptr;
                    continue;
                }

                default:
                    g_ptr++;
                    continue;
                }
            }
        }

        /* The young generation of this processes is also part of the
         * rootset.
         */
        if (WORK_MORE) {
            ErlHeapFragment* bp = MBUF(cp);

            if (cp->scan_top != HEAP_TOP(cp)) {
                //printf("Local heap before:\r\n");
                //print_untagged_memory(HIGH_WATER(cp),HEAP_TOP(cp));
                //inc_rescueHeap(p,HIGH_WATER(cp),HEAP_TOP(cp),objv,nobj);
                //cp->scan_top = inc_rescueHeap(p,cp->scan_top,HEAP_TOP(cp),objv,nobj);
                inc_rescueHeapNoBreak(p,cp->scan_top,HEAP_TOP(cp),objv,nobj);
            }

            while (WORK_MORE && bp) {
                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    //printf("Arith heap before:\r\n");
                    //print_untagged_memory(bp->mem,ARITH_HEAP(cp));
                    //Eterm *end = inc_rescueHeap(p,bp->mem,ARITH_HEAP(cp),objv,nobj);
                    inc_rescueHeapNoBreak(p,bp->mem,ARITH_HEAP(cp),objv,nobj);
                    //if (end != ARITH_HEAP(cp))
                    //    printf("O, no!!\r\n");
                } else {
                    //printf("Arith heap before:\r\n");
                    //print_untagged_memory(bp->mem,bp->mem + bp->size);
                    inc_rescueHeapNoBreak(p,bp->mem,bp->mem + bp->size,objv,nobj);
                    //Eterm *end = inc_rescueHeap(p,bp->mem,bp->mem + bp->size,objv,nobj);
                    //if (end != bp->mem + bp->size)
                    //    printf("O, no!!\r\n");
                }
                bp = bp->next;
            }

            if (WORK_MORE) {
                //printf("Rootset after:\r\n");
                //print_one_rootset(&rootset);
                restore_one_rootset(cp, &rootset);
                INC_DEACTIVATE(cp);
            }
        }
    }

    /* Update new pointers in N1 to point to new copies in old generation. */
    VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Update N1\n"));
    while (WORK_MORE && inc_n1_scn_ptr < global_htop) {
        Eterm val = *inc_n1_scn_ptr;
        switch (primary_tag(val))
            {
            case TAG_PRIMARY_LIST:
                if (ptr_within(ptr_val(val),inc_n2,inc_n2_end)) {
                    Eterm *vptr = list_val(val);
                    if (NM_IS_FORWARDED(vptr)) {
                        *inc_n1_scn_ptr = make_list(NM_FORWARD_VALUE(vptr));
                    }
                    else {
#ifdef NOMOVE
                        Eterm *old_htop =
                            erts_nm_alloc(p,2,objv,nobj);
                        /* NM_STORE(build,old_htop,2); */
#endif
                        NM_MARK_FORWARD(vptr,old_htop);
                        INC_MOVE_CONS(vptr,old_htop,inc_n1_scn_ptr);
                    }
                }
                inc_n1_scn_ptr++;
                continue;
            case TAG_PRIMARY_BOXED:
                if (ptr_within(ptr_val(val),inc_n2,inc_n2_end)) {
                    Eterm *vptr = boxed_val(val);
                    if (NM_IS_FORWARDED(vptr)) {
                        *inc_n1_scn_ptr = make_boxed(NM_FORWARD_VALUE(vptr));
                    }
                    else {
#ifdef NOMOVE
                        Eterm *old_htop =
                            erts_nm_alloc(p,BOXED_NEED(vptr,*vptr),
                                          objv,nobj);
                        /* NM_STORE(build,old_htop,2); */
#endif
                        NM_MARK_FORWARD(vptr,old_htop);
                        INC_MOVE_BOXED(vptr,old_htop,inc_n1_scn_ptr);
                    }
                }
                inc_n1_scn_ptr++;
                continue;
            case TAG_PRIMARY_HEADER:
                if (header_is_thing(val))
                    inc_n1_scn_ptr += thing_arityval(val) + 1;
                else
                    inc_n1_scn_ptr++;
                continue;
            default:
                inc_n1_scn_ptr++;
                continue;
            }
    }

    /*
     * All objects copied to the old generation above have been added
     * to the blue list (by nm_alloc). Now let's scan all objects in
     * the blue list for poiters to N2
     */
    VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Process the blue list\n"));
    while (WORK_MORE && !NM_STORAGE_EMPTY(blue)) {
        NM_Object *obj = NM_STORAGE_GET(blue);
        inc_rescueHeapNoBreak(p,obj->this,obj->this + obj->size,objv,nobj);
        //NM_Object *obj = NM_STORAGE_TOP(blue);
        //Eterm *end = inc_rescueHeap(p,obj->this,obj->this + obj->size,objv,nobj);
        //if (end == obj->this + obj->size) {
        //    NM_STORAGE_POP(blue);
        //} else { /* This didn't work at all... */
        //    obj->size -= end - obj->this;
        //    obj->this = end;
        //}
    }
    //BM_SWAP_TIMER(misc0,gc);

    if (WORK_MORE && NM_STORAGE_EMPTY(blue)) {
        {
            ExternalThing** prev = &erts_global_offheap.externals;
            ExternalThing* ptr = erts_global_offheap.externals;

            VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc externals\r\n"));
            while (ptr) {
                Eterm* ppt = (Eterm *) ptr;

                if (ptr_within(ppt, OLD_M_DATA_START, OLD_M_DATA_END)) {
                    prev = &ptr->next;
                    ptr = ptr->next;
                } else if (ptr_within(ppt, inc_n2, inc_n2_end) &&
                           NM_IS_FORWARDED(ppt)) {
                    ExternalThing* ro = (ExternalThing*)NM_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    DEREF_ERL_NODE(ptr->node);
                    *prev = ptr = ptr->next;
                }
            }
            ASSERT(*prev == NULL);
        }

        {
            ProcBin** prev = &erts_global_offheap.mso;
            ProcBin*  ptr  = erts_global_offheap.mso;

            VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc bins\r\n"));
            while (ptr) {
                Eterm *ppt = (Eterm*)ptr;

                if (ptr_within(ppt, OLD_M_DATA_START, OLD_M_DATA_END)) {
                    prev = &ptr->next;
                    ptr = ptr->next;
                } else if (ptr_within(ppt, inc_n2, inc_n2_end) &&
                           NM_IS_FORWARDED(ppt)) {
                    ProcBin* ro = (ProcBin*)NM_FORWARD_VALUE(ppt);
                    *prev = ro;         /* Patch to moved pos */
                    prev = &ro->next;
                    ptr = ro->next;
                } else {
                    Binary* bptr;
                    *prev = ptr->next;
                    bptr = ptr->val;
                    bptr->refc--;
                    if (bptr->refc == 0) {
                        if (bptr->flags & BIN_FLAG_MATCH_PROG) {
                            erts_match_set_free(bptr);
                        } else {
                            erts_bin_free(bptr);
                        }
                    }
                    ptr = *prev;
                }
            }
            ASSERT(*prev == NULL);
        }

        inc_alloc_limit = global_hend;
        inc_last_nursery = inc_n2;

#ifdef DEBUG
        /* Fill the heap with pointers to the cons of death */
        memset(inc_last_nursery,1,(inc_n2_end - inc_n2) * sizeof(Eterm));
#endif
        inc_n2 = inc_n2_end = NULL;

        if (inc_n2_size != 0) {
            /*
             * The previous collection cycle caused the nursery to
             * grow, now we have to grow N2 as well.
             */
            printf("Yes.. We do this!\r\n");
            inc_last_nursery =
                (Eterm*) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
					   (void*)inc_last_nursery,
					   sizeof(Eterm) * inc_n2_size,
					   sizeof(Eterm) * global_heap_sz);
            inc_n2_size = 0;
        }

        VERBOSE_MESSAGE((VERBOSE_NOISY,"Incremental GC cycle ENDING\n"));
        //print_memory_info(NULL);
        //printf("GC #%d\r\n",minor_global_garbage_cols);
        //printf("Last nursery (now dead): 0x%08x - 0x%08x\r\n",(int)inc_last_nursery,(int)(inc_last_nursery + global_heap_sz));
        //printf("Current nursery: 0x%08x - 0x%08x - 0x%08x\r\n",(int)global_heap,(int)global_htop,(int)(global_hend));
        //print_untagged_memory(global_heap,global_htop);
        //print_tagged_memory(global_heap,global_htop);
        //print_nm_heap();
    /*
    {
        NM_STORAGE_ITERATOR(blue);
        NM_Object *obj = NM_STORAGE_STEP(blue);
        while (obj) {
            printf("Blue object: 0x%08x\r\n",(int)obj->this);
            obj = NM_STORAGE_STEP(blue);
        }
    }
    */
    /*
    {
        int i;
        for (i = 0; i < erts_num_active_procs; i++) {
            Process *cp = erts_active_procs[i];
            ErlHeapFragment* bp = MBUF(cp);
 
            printf("New process: %s\r\n",print_pid(cp));
            //if (gc_cycles == 4)
            //    print_tagged_memory(cp->heap,cp->htop);
            //CHECK_HEAP(cp);
            //printf("Heap OK.\r\n");
            //printf("HighWater: 0x%08x\r\n",HIGH_WATER(cp));
            //printf("Saved pointers:\r\n");
            //print_untagged_memory(cp->rrma,cp->rrma + cp->nrr);
            //print_untagged_memory(cp->rrsrc,cp->rrsrc + cp->nrr);
            //printf("Local Heap after:\r\n");
            //print_untagged_memory(HIGH_WATER(cp),cp->htop);
            / *
            while (bp) {
                if ((ARITH_HEAP(cp) >= bp->mem) &&
                    (ARITH_HEAP(cp) < bp->mem + bp->size)) {
                    printf("Arith heap after:\r\n");
                    print_untagged_memory(bp->mem,ARITH_HEAP(cp));
                } else {
                    printf("Arith heap after:\r\n");
                    print_untagged_memory(bp->mem,bp->mem + bp->size);
                }
                bp = bp->next;
            }
            * /
        }
    }
    */

        VERBOSE_MESSAGE((VERBOSE_NOISY,"Collection cycle ends\n"));

        if (global_hend - global_htop <= need) {
            /*
             * Initiate a new GC cycle immediately and, if necessary,
             * enlarge the nursery.
             */
            if (global_heap_sz < need) {
                VERBOSE_MESSAGE((VERBOSE_NOISY,"Allocating a larger nursery\n"));
                inc_n2_size = global_heap_sz;
                global_heap_sz = erts_next_heap_size(need * 1.5,0);
                inc_last_nursery =
                    (Eterm*) ERTS_HEAP_REALLOC(ERTS_ALC_T_HEAP,
                                               (void*)inc_last_nursery,
                                               sizeof(Eterm) * inc_n2_size,
                                               sizeof(Eterm) * global_heap_sz);
                fwdptrs = erts_realloc(ERTS_ALC_T_ROOTSET,fwdptrs,
                                       global_heap_sz * sizeof(Eterm*));
                ERTS_PROC_LESS_MEM(sizeof(Eterm*) * inc_n2_size);
                ERTS_PROC_MORE_MEM(sizeof(Eterm*) * global_heap_sz);
            }
            goto inc_collection;
        }
    }
 
    {
        int i;
        for(i = 0; i < nobj_in; i++)
            objv_in[i] = objv[i];
        for(; i < nobj; i++)
            p->arg_reg[i - nobj_in] = objv[i];
    }
    ERTS_PROC_LESS_MEM(sizeof(Eterm) * nobj);
    erts_free(ERTS_ALC_T_ROOTSET,(void*)objv);

    if (inc_alloc_limit != global_hend) {
#ifdef INC_TIME_BASED
        if ((work_left_before - inc_words_to_go) == 0) {
            inc_alloc_limit = global_htop + need;
        } else {
            inc_alloc_limit = (global_hend - global_htop) /
                (inc_words_to_go / (work_left_before - inc_words_to_go)) +
                global_htop;
            if (inc_alloc_limit > global_hend)
                inc_alloc_limit = global_hend;
        }
#else
        if (need > inc_min_work)
            inc_alloc_limit = global_htop + need;
        else {
            inc_alloc_limit = global_htop + inc_min_work;
            if (inc_alloc_limit > global_hend)
                inc_alloc_limit = global_hend;
        }
#endif

        /* INC_TIME_BASED: If this fails we have to increase the timeslice! */
        ASSERT(inc_alloc_limit - global_htop > need);
    }

    ma_gc_flags &= ~GC_GLOBAL;

    ASSERT(global_hend - global_htop > need);

    BM_STOP_TIMER(gc);
    BM_COUNT(minor_global_garbage_cols);
#ifdef BM_TIMERS
    minor_global_gc_time += gc_time;
    if (gc_time > max_global_minor_time)
        max_global_minor_time = gc_time;

    pause_times[(((gc_time * 1000) < MAX_PAUSE_TIME) ?
                 (int)(gc_time * 1000) :
                 MAX_PAUSE_TIME - 1)]++;
#endif
    //BM_MMU_INIT();
    BM_START_TIMER(copy);
    VERBOSE_MESSAGE((VERBOSE_NOISY,"Incremental GC END\n"));
}
#undef WORK_MORE
#endif /* INCREMENTAL_GC */

#ifdef NOMOVE
/***************************************************************************
 *                                                                         *
 *         Non-Moving Garbage Collector for the Old Generation             *
 *                                                                         *
 ***************************************************************************/

Eterm *nm_heap;
Eterm *nm_hend;
char *blackmap = NULL;
NM_Page *nm_used_mem = NULL;

#ifndef INCREMENTAL_GC
char *fwdptrs  = NULL;
#endif

static int nm_pages = NM_NoPAGES;
static Eterm* root_save[NM_ROOTSAVE];
static int roots_saved = 0;
static NM_Page *nm_bibop = NULL;
static NM_MemBlock *nm_free_list = NULL;
//static NM_MemBlock *nm_last_free = NULL;

NM_STORAGE_DECLARATION(,gray);
NM_STORAGE_DECLARATION(,blue);
NM_STORAGE_DECLARATION(,root);
NM_STORAGE_DECLARATION(,build);

void erts_init_nmgc(void)
{
    int i;
    int size = nm_pages * NM_FULLPAGE;
    nm_heap = (Eterm *)ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,
                                       size * sizeof(Eterm));
    nm_hend = nm_heap + size;

    blackmap = (char*)erts_alloc(ERTS_ALC_T_ROOTSET, NM_FULLPAGE * nm_pages);

    for (i = 0; i < nm_pages; i++)
    {
        NM_Page *this = (NM_Page*)(nm_heap + i * NM_FULLPAGE);
        this->prev = (NM_Page*)((Eterm*)this - NM_FULLPAGE);
        this->next = (NM_Page*)((Eterm*)this + NM_FULLPAGE);
    }
    nm_bibop = (NM_Page*)nm_heap;
    nm_bibop->prev = NULL;
    ((NM_Page*)(nm_heap + (nm_pages - 1) * NM_FULLPAGE))->next = NULL;

    nm_used_mem = nm_bibop;
    nm_bibop = nm_bibop->next;
    if (nm_bibop)
        nm_bibop->prev = NULL;
    nm_used_mem->next = nm_used_mem->prev = NULL;

    nm_free_list = (NM_MemBlock*)nm_used_mem->start;
    nm_free_list->size = NM_PAGESIZE;
    nm_free_list->prev = NULL;
    nm_free_list->next = NULL;

    NM_STORAGE_INIT(gray);
    NM_STORAGE_INIT(blue);
    NM_STORAGE_INIT(root);
    NM_STORAGE_INIT(build);

#ifdef INCREMENTAL_GC
    erts_init_incgc();
#endif
}

void erts_cleanup_nmgc(void)
{
    if (nm_heap)
        erts_free(ERTS_ALC_T_OLD_HEAP,(void*)nm_heap);
    if (blackmap)
        erts_free(ERTS_ALC_T_ROOTSET,(void*)blackmap);
}

static void add_to_roots(Eterm *ptr)
{
  //printf("Save root #%d: 0x%08x\r\n",roots_saved,(int)ptr);
  //  root_save[roots_saved++] = ptr;
  //  if (roots_saved == NM_ROOTSAVE)
  //    printf("Too Many ROOTS!!\r\n");
}

/* XXX: Fix to not copy things:
 * if (ptr_within(min_nm_address,max_nm_adress))
 *   while (page)
 *     if (in this one)
 */

#ifdef HYBRID
/*
 * Scan a root-area for pointers to the nm_heap. (Stack, PCB etc.)
 */
static void root_area_scan(Eterm *ptr, int size)
{
    while (size--)
    {
        Eterm val = *ptr;

        switch (primary_tag(val))
        {
            case TAG_PRIMARY_LIST:
            {
                if (ptr_within(ptr_val(val),nm_heap,nm_hend))
                {
                    NM_STORE(gray,ptr_val(val),2);
                }
                break;
            }

            case TAG_PRIMARY_BOXED:
            {
                if (ptr_within(ptr_val(val),nm_heap,nm_hend))
                {
                    NM_STORE(gray,ptr_val(val),BOXED_NEED(ptr_val(val),
                                                          *ptr_val(val)));
                }
                break;
            }
        }
        ptr++;
    }
}

/*
 * Scan a heap area for pointers to the nm_heap. (p->heap, p->old_heap)
 */
static void private_heap_scan(Eterm *ptr, int size)
{
    //printf("HeapAreaScan 0x%08x of size %d\r\n",(int)ptr,size);
    while (size--)
    {
        Eterm val = *ptr;
        //printf("%2d: 0x%08x ",size,(int)val);
        switch (primary_tag(val))
        {
          case TAG_PRIMARY_LIST:
              //printf("list\r\n");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),2);
              }
              break;

          case TAG_PRIMARY_BOXED:
              //printf("boxed\r\n");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),BOXED_NEED(ptr_val(val),*ptr_val(val)));
              }
              break;

          case TAG_PRIMARY_IMMED1:
              //printf("immediate\r\n");
              break;

          case TAG_PRIMARY_HEADER:
          {
              //printf("header ");
              switch (val & _TAG_HEADER_MASK)
              {
                case ARITYVAL_SUBTAG:
                    //printf("tuple\r\n");
                    break;

                default:
                    //printf("other arity: %d\r\n",(int)thing_arityval(val));
                    ptr += thing_arityval(val);
                    size -= thing_arityval(val);
                    break;
              }
              break;
          }

          default: printf("You see, this can't really happen...\r\n");
        }
        ptr++;
    }
}

/*
 * Scan the young generation of the message area for pointers to the nm_heap.
 */
#ifdef INCREMENTAL_GC
static void message_area_scan(Eterm *ptr, int size)
{
    Eterm *stop = ptr + size;
    //printf("MessageAreaScan 0x%08x of size %d\r\n",(int)ptr,size);
    //print_untagged_memory(ptr,stop);
    while (ptr < stop)
    {
        Eterm val = *ptr;
        //printf("0x%08x: 0x%08x \r\n",(int)ptr,(int)val);
        switch (primary_tag(val))
        {
          case TAG_PRIMARY_LIST:
              //printf("list");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),2);
              }
              ptr++;
              continue;

          case TAG_PRIMARY_BOXED:
              //printf("boxed ");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend)) {
                  NM_STORE(gray,ptr_val(val),BOXED_NEED(ptr_val(val),*ptr_val(val)));
              }
              ptr++;
              continue;

          case TAG_PRIMARY_IMMED1:
              //printf("immediate");
              ptr++;
              continue;

          case TAG_PRIMARY_HEADER:
          {
              //printf("header ");
              switch (val & _TAG_HEADER_MASK) {
              case ARITYVAL_SUBTAG:
                  //printf("tuple");
                  break;

              default:
                  //printf("other arity: %d",(int)thing_arityval(val));
                  ptr += thing_arityval(val);
                  break;
              }
              ptr++;
              break;
          }

        default: printf("You see, this can't really happen...\r\n");
        }
        //printf("\r\n");
    }
}
#else
static void message_area_scan(Eterm *ptr, int size)
{
    Eterm *stop = ptr + size;
    //printf("MessageAreaScan 0x%08x of size %d\r\n",(int)ptr,size);
    //print_untagged_memory(ptr,stop);
    while (ptr < stop)
    {
        Eterm val = *ptr;
        //printf("0x%08x: 0x%08x \r\n",(int)ptr,(int)val);
        switch (primary_tag(val))
        {
          case TAG_PRIMARY_LIST:
              //printf("list");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),2);
              }
              ptr++;
              break;

          case TAG_PRIMARY_BOXED:
              //printf("boxed ");
              if (fwdptrs[ptr - global_heap] != 0) {
                  if (ptr_within(ptr_val(val),nm_heap,nm_hend)) {
                      //printf("Forwarded (a,*a): (0x%08x, 0x%08x)",ptr_val(val),*ptr_val(val));
                      ptr += BOXED_NEED(ptr_val(val),*ptr_val(val));
                  }
                  else if (is_thing(*ptr_val(val))) {
                      //printf("Forwarded thing arity: %d",thing_arityval(*ptr_val(val)));
                      ptr += thing_arityval(*ptr_val(val));
                  }
                  else {
                      //printf("Something forwarded...\r\n");
                      ptr++;
                  }
              }
              else if (ptr_within(ptr_val(val),nm_heap,nm_hend)) {
                  //printf("saving");
                  NM_STORE(gray,ptr_val(val),BOXED_NEED(ptr_val(val),*ptr_val(val)));
                  ptr++;
              }
              else
                  ptr++;
              break;

          case TAG_PRIMARY_IMMED1:
              //printf("immediate");
              ptr++;
              break;

          case TAG_PRIMARY_HEADER:
          {
              if (is_non_value(val))
              {
                  //printf("NON_VALUE (forwarded cons)");
                  ptr += 2;
              }
              else
              {
                  //printf("header ");
                  switch (val & _TAG_HEADER_MASK)
                  {
                      case ARITYVAL_SUBTAG:
                          //printf("tuple");
                          break;

                      default:
                          //printf("other arity: %d",(int)thing_arityval(val));
                          ptr += thing_arityval(val);
                          break;
                  }
                  ptr++;
              }
              break;
          }

          default: printf("You see, this can't really happen...\r\n");
        }
        //printf("\r\n");
    }
}
#endif /* INCREMENTAL_GC */
#endif /* HYBRID */

/*
 * Scans an area within the nm_heap for pointers to the nm_heap
 */
static void area_scan(Eterm *area, int size)
{
  //printf("Scan 0x%08x of size %d\r\n",(int)area,size);
    while (size--)
    {
        Eterm val = *area;
        //printf("%2d: 0x%08x ",size,(int)val);
        switch (primary_tag(val))
        {
          case TAG_PRIMARY_LIST:
            //printf("list\r\n");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),2);
              }
              break;

          case TAG_PRIMARY_BOXED:
            //printf("boxed\r\n");
              if (ptr_within(ptr_val(val),nm_heap,nm_hend))
              {
                  NM_STORE(gray,ptr_val(val),BOXED_NEED(ptr_val(val),*ptr_val(val)));
              }
              break;

          case TAG_PRIMARY_IMMED1:
            //printf("immediate\r\n");
              break;

          case TAG_PRIMARY_HEADER:
          {
            //printf("header ");
              switch (val & _TAG_HEADER_MASK)
              {
                case ARITYVAL_SUBTAG:
                  //printf("tuple\r\n");
                    break;
              /*
                Vectors may cause a problem some day...
                case VECTOR_SUBTAG:
                  printf("*************** vector ***************\r\n");
                    break;
               */
                default:
                  //printf("other arity: %d\r\n",(int)thing_arityval(val));
                    area += thing_arityval(val);
                    size -= thing_arityval(val);
                    /* There is a bug somewhere... */
                    //if (size < 0) size = 0;
                    break;
              }
              break;
          }

          default: printf("You are dreaming!\r\n");
        }
        area++;
    }
}

#ifdef SHARED_HEAP
static NM_MemBlock *erts_nm_collect(Process *p, int need, Eterm* objv, int nobj)
{
  //Rootset *rootset = erts_safe_sl_alloc(sizeof(Rootset)*erts_max_processes);
    Eterm *free_start = NULL;
    NM_MemBlock *found = NULL;
    NM_Page *page = NULL;
    BM_NEW_TIMER(old_gc);

    BM_SWAP_TIMER(gc,old_gc);
    BM_COUNT(major_global_garbage_cols);

    VERBOSE_MESSAGE((VERBOSE_NOISY,
                     "NM GC P:0x%08x START (NM Area: 0x%08x - 0x%08x)\r\n",
                     (int)p,(int)nm_heap,(int)nm_hend));
    //print_nm_heap();
    //print_heap(HEAP_START(p),HEAP_END(p));

    //printf("Mark roots\r\n");
    /*
    roots_saved = 0;
    {
        int n = collect_roots(p, objv, nobj, rootset);
        while (n--)
        {
            while (rootset[n].n--)
            {
                Eterm* ptr = rootset[n].v[rootset[n].n];
                Uint  sz = rootset[n].sz[rootset[n].n];
                root_area_scan(ptr,sz);
            }
        }
    }
    */
    //printf("Mark infants\r\n");
    //root_area_scan(HEAP_START(p),HEAP_TOP(p) - HEAP_START(p));  
    //BM_SWAP_TIMER(old_gc,misc0);
    {
        NM_STORAGE_ITERATOR(root);
        NM_Object *obj = NM_STORAGE_STEP(root);
        //printf("Mark root list\r\n");
        while (obj)
        {
            if (blackmap[obj->this - nm_heap] == 0)
            {
                blackmap[obj->this - nm_heap] = obj->size;
                //printf("Rescue #%d (0x%08x) size: %d\r\n",(int)(obj->this - nm_heap),(int)obj->this,obj->size);
                area_scan(obj->this,obj->size);
            }
            //else
            //BM_COUNT(messages_ego);
            //printf("Already scanned #%d\r\n",(int)(ptr_val(obj->this) - nm_heap));
            obj = NM_STORAGE_STEP(root);
        }
    }

    //printf("Mark gray list\r\n");
    {
        NM_Object *obj = NM_STORAGE_GET(gray);
        while (obj)
        {
            if (blackmap[obj->this - nm_heap] == 0)
            {
                blackmap[obj->this - nm_heap] = obj->size;
                //printf("Rescue #%d (0x%08x) size: %d\r\n",(int)(obj->this - nm_heap),(int)obj->this,obj->size);
                //BM_SWAP_TIMER(old_gc,misc2);
                area_scan(obj->this,obj->size);
                //BM_SWAP_TIMER(misc2,old_gc);
            }
            //else
            //BM_COUNT(messages_ego);
            //printf("Already scanned #%d\r\n",(int)(ptr_val(obj->this) - nm_heap));
            obj = NM_STORAGE_GET(gray);
        }
    }

    /* XXX: If we remember old free list, can we gain in any way..? */
    nm_free_list = NULL;
    page = nm_used_mem;

    //printf("Sweep phase\r\n");
    live = 0;
    while (page)
    {
        int scavenging = 0;
        Eterm *ptr = page->start;
        Eterm *page_end = ptr + NM_PAGESIZE;
        int n = page->start - nm_heap;

        while (ptr < page_end)
        {
            if (blackmap[n] != 0)
            {
                if (scavenging)
                {
                    scavenging = 0;
                    if ((ptr - free_start) * sizeof(Eterm) >=
                        sizeof(NM_MemBlock))
                    {
                        NM_MemBlock *new = (NM_MemBlock*)(free_start);
                        new->size = ptr - free_start;
                        new->prev = NULL;
                        new->next = nm_free_list;
                        if (nm_free_list)
                            nm_free_list->prev = new;
                        nm_free_list = new;
                        //printf("Created free block (1): 0x%08x of size %d\r\n",(int)new,new->size);
                        if ((new->size >= need) &&
                            (!found || (found->size > new->size)))
                        {
                            found = new;
                            //printf("We've got one!\r\n");
                        }
                    }
                }
                //printf("Found a live one #%d @ 0x%08x!\r\n",n,(int)ptr);
                ptr += blackmap[n];
                n += blackmap[n];
            }
            else if (!scavenging)
            {
              //printf("Free block starts @ 0x%08x\r\n",(int)ptr);
                free_start = ptr;
                scavenging = 1;
                n++; ptr++;
            }
            else
            {
                n++; ptr++;
            }
        }

        if (scavenging)
        {
            if (free_start == page->start)
            {
                NM_Page *next = page->next;
                //printf("Returning page 0x%08x to BIBOP\r\n",(int)page);
                if (page->next)
                    page->next->prev = page->prev;

                if (page->prev)
                    page->prev->next = page->next;
                else
                    nm_used_mem = page->next;

                page->next = nm_bibop;
                nm_bibop = page;
                page = next;
                //print_page_list(nm_bibop);
                //print_page_list(nm_used_mem);
                continue;
            }
            else if ((ptr - free_start) * sizeof(Eterm) >
                     sizeof(NM_MemBlock))
            {
                NM_MemBlock *new = (NM_MemBlock*)(free_start);
                new->size =  ptr - free_start;
                new->prev = NULL;
                new->next = nm_free_list;
                if (nm_free_list)
                    nm_free_list->prev = new;
                nm_free_list = new;
                //printf("Created free block (2): 0x%08x of size %d\r\n",(int)new,new->size);
                if ((new->size >= need) &&
                    (!found || (found->size > new->size)))
                {
                    found = new;
                    //printf("We've got one!\r\n");
                }
            }
        }
        page = page->next;
    }
    //print_free_list();
    //BM_SWAP_TIMER(misc2,old_gc);

    if (!found && !nm_bibop)
    {
        int i;
        int new_pages = nm_pages * 2;
        int size = sizeof(Eterm) * new_pages * NM_FULLPAGE;
        Eterm *new_heap = ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,size);
        Eterm *new_hend = new_heap + size;
        Eterm *new_htop;
        Eterm *last_page_end;
        NM_Page *new_used_mem;

        printf("We need to copy things!\r\n");

        /* Create new, bigger bag of pages */
        for (i = 0; i < new_pages; i++)
        {
            NM_Page *this =
              (NM_Page*)(new_heap + i * NM_FULLPAGE);
            this->prev = (NM_Page*)((Eterm*)this - NM_FULLPAGE);
            this->next = (NM_Page*)((Eterm*)this + NM_FULLPAGE);
        }
        nm_bibop = (NM_Page*)new_heap;
        nm_bibop->prev = NULL;
        ((NM_Page*)(new_heap + (new_pages - 1) *
                           NM_FULLPAGE))->next = NULL;

        new_used_mem = nm_bibop;
        nm_bibop = nm_bibop->next;
        nm_bibop->prev = NULL;
        new_used_mem->next = new_used_mem->prev = NULL;

        //print_page_list(nm_bibop);
        //print_page_list(new_used_mem);

        /* Move stuff from old bag to new */
        nm_free_list = NULL;
        new_htop = new_used_mem->start;
        last_page_end = new_htop + NM_PAGESIZE;
        page = nm_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + NM_PAGESIZE;
            int n = offsetof(NM_Page,start) / sizeof(void*) +
              ((Eterm*)page - nm_heap);
            while (ptr < page_end)
            {
                if (blackmap[n] > 0)
                {
                  //printf("Found a live one #%d @ 0x%08x! Left: %d\r\n",n,(int)ptr,last_page_end-new_htop);
                    if (last_page_end - new_htop < blackmap[n])
                    {
                        NM_Page *new_page = nm_bibop;

                        //printf("Need to fetch a new page!\r\n");
                        nm_bibop = nm_bibop->next;
                        if (nm_bibop)
                            nm_bibop->prev = NULL;

                        new_page->next = new_used_mem;
                        new_used_mem->prev = new_page;
                        new_used_mem = new_page;
                        //printf("Snatched a new page @ 0x%08x\r\n",(int)new_page);
                        new_htop = new_page->start;
                        last_page_end = new_htop + NM_PAGESIZE;
                    }

                    memcpy(new_htop,ptr,blackmap[n] * sizeof(Eterm));
                    for (i = 0; i < blackmap[n]; i++)
                      {
                        //printf("cp 0x%08x = 0x%08x",(int)ptr,(int)new_htop);
                        *ptr++ = (Eterm)new_htop++;
                        //printf("    done: 0x%08x\r\n",(int)*ptr++); OBS OBS!!
                      }
                  //new_htop += blackmap[n];
                  //ptr += blackmap[n];
                    n += blackmap[n];
                }
                else
                {
                    n++; ptr++;
                }
            }
            page = page->next;
        }

        page = nm_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + NM_PAGESIZE;

            /* XXX: If nm_used_mem is sorted in address order, this
             * pass can be done at the same time as copying. */
            while (ptr < page_end)
            {
                //printf("? 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
                if (ptr_within(ptr_val(*ptr),nm_heap,nm_hend))
                {
                    //printf("- 0x%08x: 0x%08x\r\n",(int)ptr_val(*ptr),(int)*((Eterm*)ptr_val(*ptr)));
                    *ptr = *((Eterm*)ptr_val(*ptr));
                    //printf("! 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
                }
                ptr++;
            }
            page = page->next;
        }

        printf("Restore rootset after heap move. Roots: %d\r\n",roots_saved);
        while (roots_saved--)
        {
            Eterm *ptr = root_save[roots_saved];
            //printf("? 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
            //printf("- 0x%08x: 0x%08x\r\n",(int)ptr_val(*ptr),(int)*((Eterm*)ptr_val(*ptr)));
            *ptr = *((Eterm*)ptr_val(*ptr));
            //printf("! 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
        }

        erts_free(ERTS_ALC_T_OLD_HEAP,(void*)nm_heap);

        nm_heap = new_heap;
        nm_hend = new_hend;
        nm_used_mem = new_used_mem;
        nm_pages = new_pages;

        if ((last_page_end - new_htop) * sizeof(Eterm) >=
            sizeof(NM_MemBlock))
        {
            nm_free_list = (NM_MemBlock*)(new_htop);
            nm_free_list->size = last_page_end - new_htop;
            nm_free_list->prev = NULL;
            nm_free_list->next = NULL;
            //printf("Created free block (3): 0x%08x of size %d\r\n",(int)nm_free_list,nm_free_list->size);
            if (nm_free_list->size >= need)
                found = nm_free_list;
        }
        //print_nm_heap();
        //print_free_list();
    }

    if (!found)
    {
        NM_Page *new_page = nm_bibop;
        NM_MemBlock *new_free = (NM_MemBlock*)new_page->start;

        nm_bibop = nm_bibop->next;

        new_page->next = nm_used_mem;
        if (nm_used_mem)
            nm_used_mem->prev = new_page;
        nm_used_mem = new_page;

        //BM_SWAP_TIMER(gc,misc1);
        memset(blackmap + ((void*)new_page - (void*)nm_heap) / sizeof(void*),
               0,NM_FULLPAGE);
        //BM_SWAP_TIMER(misc1,gc);

        new_free->prev = NULL;
        new_free->next = nm_free_list;
        new_free->size = NM_PAGESIZE;
        if (nm_free_list)
            nm_free_list->prev = new_free;
        nm_free_list = new_free;
        //printf("Snatched a new page @ 0x%08x\r\n",(int)new_page);
        //print_free_list();
        found = new_free;
    }

    //restore_rootset(NULL, rootset);
    //erts_sl_free((void *)rootset);
    //print_nm_heap();
    //print_page_list(nm_used_mem);
    //print_free_list();
    VERBOSE_MESSAGE((VERBOSE_NOISY,"NM GC P:0x%08x END\r\n",(int)p));

    BM_STOP_TIMER(old_gc);
#ifdef BM_TIMER
    major_global_gc_time += old_gc_time;
    if (old_gc_time > max_global_major_time)
      max_global_major_time = old_gc_time;
#endif
    BM_START_TIMER(gc);

    return found;
}

#else /* !SHARED_HEAP */

static NM_MemBlock *erts_nm_collect(Process *p, int need,
                                    Eterm* objv, int nobj)
{
    Rootset *rootset = erts_alloc(ERTS_ALC_T_ROOTSET,
                                  sizeof(Rootset)*erts_max_processes);
    Eterm *free_start = NULL;
    NM_MemBlock *found = NULL;
    NM_Page *page = NULL;
    Uint live = 0;
    Uint old_gen_sz = 0;
    BM_NEW_TIMER(old_gc);

    BM_SWAP_TIMER(gc,old_gc);
    //BM_SWAP_TIMER(misc0,old_gc);

    BM_COUNT(major_global_garbage_cols);

    VERBOSE_MESSAGE((VERBOSE_NOISY,"Non-Moving GC START  Proc: %s\n",
                     print_pid(p)));
    //print_proc_bins(p);
    //print_nm_heap();
    //print_heap(HEAP_START(p),HEAP_END(p));

    ma_gc_flags |= GC_INCLUDE_ALL;
    roots_saved = 0;
    {
        int n = collect_roots(p, objv, nobj, rootset);

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Mark roots\n"));
        while (n--)
        {
            while (rootset[n].n--)
            {
                Eterm* ptr = rootset[n].v[rootset[n].n];
                Uint  sz = rootset[n].sz[rootset[n].n];
                root_area_scan(ptr,sz);
            }
        }
    }

    {
        Process* p;
        Uint i;
        Uint n = erts_num_active_procs;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Mark process heaps\n"));
        for (i = 0; i < n; i++) {
            p = erts_active_procs[i];
            private_heap_scan(p->high_water,p->htop - p->high_water);
        }
    }

    VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Mark global heap\n"));
    message_area_scan(global_heap,global_htop - global_heap);

#ifdef INCREMENTAL_GC
    if (inc_n2 != NULL) {
        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Mark old nursery\n"));
        message_area_scan(inc_n2,inc_n2_end - inc_n2);
    }
#endif

    //BM_SWAP_TIMER(old_gc,misc0);
    /*
    {
        NM_STORAGE_ITERATOR(root);
        NM_Object *obj = NM_STORAGE_STEP(root);
        printf("Mark root list\r\n");
        while (obj)
        {
            if (blackmap[obj->this - nm_heap] == 0)
            {
                blackmap[obj->this - nm_heap] = obj->size;
                //printf("Rescue #%d (0x%08x) size: %d\r\n",(int)(obj->this - nm_heap),(int)obj->this,obj->size);
                area_scan(obj->this,obj->size);
            }
            //else
            //BM_COUNT(messages_ego);
            //printf("Already scanned #%d\r\n",(int)(ptr_val(obj->this) - nm_heap));
            obj = NM_STORAGE_STEP(root);
        }
    }
    */

    {
        NM_Object *obj = NM_STORAGE_GET(gray);

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Mark gray list\n"));
        while (obj)
        {
            if (blackmap[obj->this - nm_heap] == 0)
            {
                blackmap[obj->this - nm_heap] = obj->size;
                //printf("Rescue #%d (0x%08x) size: %d\r\n",(int)(obj->this - nm_heap),(int)obj->this,obj->size);
                //BM_SWAP_TIMER(old_gc,misc2);
                area_scan(obj->this,obj->size);
                //BM_SWAP_TIMER(misc2,old_gc);
            }
            //else
            //BM_COUNT(messages_ego);
            //printf("Already scanned #%d\r\n",(int)(ptr_val(obj->this) - nm_heap));
            obj = NM_STORAGE_GET(gray);
        }
    }

#ifdef INCREMENTAL_GC
    {
        ExternalThing** prev = &erts_global_offheap.externals;
        ExternalThing* ptr = erts_global_offheap.externals;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc externals\n"));
        while (ptr) {
            Eterm* ppt = (Eterm *) ptr;

            if ((ptr_within(ppt, nm_heap, nm_hend) &&
                 blackmap[ppt - nm_heap] == 0))
            {
                DEREF_ERL_NODE(ptr->node);
                *prev = ptr = ptr->next;
            } else {
                prev = &ptr->next;
                ptr = ptr->next;
            }
        }
        ASSERT(*prev == NULL);
    }

    {
        ProcBin** prev = &erts_global_offheap.mso;
        ProcBin*  ptr  = erts_global_offheap.mso;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc bins\n"));
        while (ptr) {
            Eterm *ppt = (Eterm*)ptr;

            if (ptr_within(ppt, nm_heap, nm_hend) &&
                blackmap[ppt - nm_heap] == 0)
            {
                Binary* bptr;
                *prev = ptr->next;
                bptr = ptr->val;
                bptr->refc--;
                if (bptr->refc == 0) {
                    if (bptr->flags & BIN_FLAG_MATCH_PROG) {
                        erts_match_set_free(bptr);
                    } else {
                        erts_bin_free(bptr);
                    }
                }
                ptr = *prev;
            } else {
                prev = &ptr->next;
                ptr = ptr->next;
            }
        }
        ASSERT(*prev == NULL);
    }
#else
    {
        ExternalThing** prev = &erts_global_offheap.externals;
        ExternalThing* ptr = erts_global_offheap.externals;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc externals\n"));
        while (ptr) {
            Eterm* ppt = (Eterm *) ptr;

            if (MY_IS_MOVED(*ppt)) {
                ExternalThing* ro = external_thing_ptr(*ppt);
                *prev = ro;         /* Patch to moved pos */
                prev = &ro->next;
                ptr = ro->next;
            } else if ((ptr_within(ppt, nm_heap, nm_hend) &&
                        blackmap[ppt - nm_heap] != 0) ||
                       ptr_within(ppt, global_heap, global_hend)) {
                prev = &ptr->next;
                ptr = ptr->next;
            } else {
                DEREF_ERL_NODE(ptr->node);
                *prev = ptr = ptr->next;
            }
        }
        ASSERT(*prev == NULL);
    }

    {
        ProcBin** prev = &erts_global_offheap.mso;
        ProcBin*  ptr  = erts_global_offheap.mso;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep proc bins\n"));
        while (ptr) {
            Eterm *ppt = (Eterm*)ptr;

            if (MY_IS_MOVED(*ppt)) {
                ProcBin* ro = (ProcBin*) binary_val(*ppt);
                *prev = ro;         /* Patch to moved pos */
                prev = &ro->next;
                ptr = ro->next;
            } else if ((ptr_within(ppt, nm_heap, nm_hend) &&
                        blackmap[ppt - nm_heap] != 0) ||
                       ptr_within(ppt, global_heap, global_hend)) {
                prev = &ptr->next;
                ptr = ptr->next;
            } else {
                Binary* bptr;
                *prev = ptr->next;
                bptr = ptr->val;
                bptr->refc--;
                if (bptr->refc == 0) {
                    if (bptr->flags & BIN_FLAG_MATCH_PROG) {
                        erts_match_set_free(bptr);
                    } else {
                        erts_bin_free(bptr);
                    }
                }
                ptr = *prev;
            }
        }
        ASSERT(*prev == NULL);
    }
#endif /* INCREMENTAL_GC */

    /* XXX: If we remember old free list, can we gain in any way..? */
    nm_free_list = NULL;
    page = nm_used_mem;

    VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Sweep phase\n"));
    while (page)
    {
        int scavenging = 0;
        int n = page->start - nm_heap;
        int stop = n + NM_PAGESIZE;

        old_gen_sz += NM_PAGESIZE;
        while (n < stop)
        {
            if (blackmap[n] != 0)
            {
                if (scavenging)
                {
                    Eterm *ptr = nm_heap + n;
                    scavenging = 0;
                    if ((ptr - free_start) * sizeof(Eterm) >=
                        sizeof(NM_MemBlock))
                    {
                        NM_MemBlock *new = (NM_MemBlock*)(free_start);
                        new->size = ptr - free_start;
                        new->prev = NULL;
                        new->next = nm_free_list;
                        if (nm_free_list)
                            nm_free_list->prev = new;
                        nm_free_list = new;
                        //printf("Created free block (1): 0x%08x of size %d\r\n",(int)new,new->size);
                        if ((new->size >= need) &&
                            (!found || (found->size > new->size)))
                        {
                            found = new;
                            //printf("We've got one!\r\n");
                        }
                    }
                }
                //printf("Found a live one #%d @ 0x%08x!\r\n",n,(int)ptr);
                live += blackmap[n];
                n += blackmap[n];
            }
            else if (!scavenging)
            {
              //printf("Free block starts @ 0x%08x\r\n",(int)ptr);
                free_start = nm_heap + n;
                scavenging = 1;
                n++;
            }
            else
            {
                n++;
            }
        }

        if (scavenging)
        {
            if (free_start == page->start)
            {
                NM_Page *next = page->next;
                //printf("Returning page 0x%08x to BIBOP\r\n",(int)page);
                if (page->next)
                    page->next->prev = page->prev;

                if (page->prev)
                    page->prev->next = page->next;
                else
                    nm_used_mem = page->next;

                page->next = nm_bibop;
                nm_bibop = page;
                page = next;
                //print_page_list(nm_bibop);
                //print_page_list(nm_used_mem);
                continue;
            }
            else if ((nm_heap + n - free_start) * sizeof(Eterm) >
                     sizeof(NM_MemBlock))
            {
                NM_MemBlock *new = (NM_MemBlock*)(free_start);
                new->size =  nm_heap + n - free_start;
                new->prev = NULL;
                new->next = nm_free_list;
                if (nm_free_list)
                    nm_free_list->prev = new;
                nm_free_list = new;
                //printf("Created free block (2): 0x%08x of size %d\r\n",(int)new,new->size);
                if ((new->size >= need) &&
                    (!found || (found->size > new->size)))
                {
                    found = new;
                    //printf("We've got one!\r\n");
                }
            }
        }
        page = page->next;
    }
    //print_free_list();

    if (!found && !nm_bibop)
    {
        int i;
        int new_pages = nm_pages * 2;
        int size = sizeof(Eterm) * new_pages * NM_FULLPAGE;
        Eterm *new_heap = ERTS_HEAP_ALLOC(ERTS_ALC_T_OLD_HEAP,size);
        Eterm *new_hend = new_heap + size;
        Eterm *new_htop;
        Eterm *last_page_end;
        NM_Page *new_used_mem;

        printf("We need to copy things!\r\n");

        /* Create new, bigger bag of pages */
        for (i = 0; i < new_pages; i++)
        {
            NM_Page *this =
              (NM_Page*)(new_heap + i * NM_FULLPAGE);
            this->prev = (NM_Page*)((Eterm*)this - NM_FULLPAGE);
            this->next = (NM_Page*)((Eterm*)this + NM_FULLPAGE);
        }
        nm_bibop = (NM_Page*)new_heap;
        nm_bibop->prev = NULL;
        ((NM_Page*)(new_heap + (new_pages - 1) *
                           NM_FULLPAGE))->next = NULL;

        new_used_mem = nm_bibop;
        nm_bibop = nm_bibop->next;
        nm_bibop->prev = NULL;
        new_used_mem->next = new_used_mem->prev = NULL;

        //print_page_list(nm_bibop);
        //print_page_list(new_used_mem);

        /* Move stuff from old bag to new */
        nm_free_list = NULL;
        new_htop = new_used_mem->start;
        last_page_end = new_htop + NM_PAGESIZE;
        page = nm_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + NM_PAGESIZE;
            int n = offsetof(NM_Page,start) / sizeof(void*) +
              ((Eterm*)page - nm_heap);
            while (ptr < page_end)
            {
                if (blackmap[n] > 0)
                {
                  //printf("Found a live one #%d @ 0x%08x! Left: %d\r\n",n,(int)ptr,last_page_end-new_htop);
                    if (last_page_end - new_htop < blackmap[n])
                    {
                        NM_Page *new_page = nm_bibop;

                        //printf("Need to fetch a new page!\r\n");
                        nm_bibop = nm_bibop->next;
                        if (nm_bibop)
                            nm_bibop->prev = NULL;

                        new_page->next = new_used_mem;
                        new_used_mem->prev = new_page;
                        new_used_mem = new_page;
                        //printf("Snatched a new page @ 0x%08x\r\n",(int)new_page);
                        new_htop = new_page->start;
                        last_page_end = new_htop + NM_PAGESIZE;
                    }

                    memcpy(new_htop,ptr,blackmap[n] * sizeof(Eterm));
                    for (i = 0; i < blackmap[n]; i++)
                      {
                        //printf("cp 0x%08x = 0x%08x",(int)ptr,(int)new_htop);
                        *ptr++ = (Eterm)new_htop++;
                        //printf("    done: 0x%08x\r\n",(int)*ptr++); OBS OBS!!
                      }
                  //new_htop += blackmap[n];
                  //ptr += blackmap[n];
                    n += blackmap[n];
                }
                else
                {
                    n++; ptr++;
                }
            }
            page = page->next;
        }

        page = nm_used_mem;
        while (page)
        {
            Eterm *ptr = page->start;
            Eterm *page_end = ptr + NM_PAGESIZE;

            /* XXX: If nm_used_mem is sorted in address order, this
             * pass can be done at the same time as copying. */
            while (ptr < page_end)
            {
                //printf("? 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
                if (ptr_within(ptr_val(*ptr),nm_heap,nm_hend))
                {
                    //printf("- 0x%08x: 0x%08x\r\n",(int)ptr_val(*ptr),(int)*((Eterm*)ptr_val(*ptr)));
                    *ptr = *((Eterm*)ptr_val(*ptr));
                    //printf("! 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
                }
                ptr++;
            }
            page = page->next;
        }

        printf("Restore rootset after heap move. Roots: %d\r\n",roots_saved);
        while (roots_saved--)
        {
            Eterm *ptr = root_save[roots_saved];
            //printf("? 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
            //printf("- 0x%08x: 0x%08x\r\n",(int)ptr_val(*ptr),(int)*((Eterm*)ptr_val(*ptr)));
            *ptr = *((Eterm*)ptr_val(*ptr));
            //printf("! 0x%08x: 0x%08x\r\n",(int)ptr,(int)*ptr);
        }

        erts_free(ERTS_ALC_T_OLD_HEAP,(void*)nm_heap);

        nm_heap = new_heap;
        nm_hend = new_hend;
        nm_used_mem = new_used_mem;
        nm_pages = new_pages;

        if ((last_page_end - new_htop) * sizeof(Eterm) >=
            sizeof(NM_MemBlock))
        {
            nm_free_list = (NM_MemBlock*)(new_htop);
            nm_free_list->size = last_page_end - new_htop;
            nm_free_list->prev = NULL;
            nm_free_list->next = NULL;
            //printf("Created free block (3): 0x%08x of size %d\r\n",(int)nm_free_list,nm_free_list->size);
            if (nm_free_list->size >= need)
                found = nm_free_list;
        }
        //print_nm_heap();
        //print_free_list();
    }

    if (!found)
    {
        NM_Page *new_page = nm_bibop;
        NM_MemBlock *new_free =
          (NM_MemBlock*)new_page->start;

        VERBOSE_MESSAGE((VERBOSE_MORE_MEM,"Fetching new page\n"));
        nm_bibop = nm_bibop->next;

        new_page->next = nm_used_mem;
        if (nm_used_mem)
            nm_used_mem->prev = new_page;
        nm_used_mem = new_page;

        // kolla detta med normal sidstorlek! old_gen_sz += NM_PAGESIZE;
        //BM_SWAP_TIMER(gc,misc1);
        memset(blackmap + ((void*)new_page - (void*)nm_heap) / sizeof(void*),
               0,NM_FULLPAGE);
        //BM_SWAP_TIMER(misc1,gc);

        new_free->prev = NULL;
        new_free->next = nm_free_list;
        new_free->size = NM_PAGESIZE;
        if (nm_free_list)
            nm_free_list->prev = new_free;
        nm_free_list = new_free;
        //printf("Snatched a new page @ 0x%08x\r\n",(int)new_page);
        //print_free_list();
        found = new_free;
    }

    restore_rootset(NULL, rootset);
    erts_free(ERTS_ALC_T_ROOTSET,(void *)rootset);
    ma_gc_flags &= ~GC_INCLUDE_ALL;

    VERBOSE_MESSAGE((VERBOSE_NOISY,"Non-Moving GC END\r\n"));
    //print_nm_heap();
    //print_page_list(nm_used_mem);
    //print_free_list();
    //print_proc_bins(p);

    BM_STOP_TIMER(old_gc);
#ifdef BM_TIMER
    major_global_gc_time += old_gc_time;
    if (old_gc_time > max_global_major_time)
      max_global_major_time = old_gc_time;

    if ((old_gc_time * 1000) < MAX_PAUSE_TIME)
        pause_times_old[(int)(old_gc_time * 1000)]++;
    else
        pause_times_old[MAX_PAUSE_TIME - 1]++;
#endif
    BM_START_TIMER(gc);

    return found;
}
#endif /* SHARED_HEAP */

#ifndef INCREMENTAL_GC
void erts_nm_copymark(Process *p, Eterm* objv, int nobj)
{
#if HIPE
    char *const_start = (char*)hipe_constants_start;
    unsigned long const_size = (char*)hipe_constants_next - const_start;
#endif
    NM_Object *obj = NM_STORAGE_GET(blue);
    while (obj)
    {
        Eterm *this = obj->this;
        int sz = obj->size;
        //printf("Blue object: 0x%08x of size: %d\r\n",(int)this,obj->size);
        while(sz--)
        {
          //printf("%d: 0x%08x  ",obj->size,(int)*this);
            switch (primary_tag(*this))
            {
              case TAG_PRIMARY_BOXED:
              {
                  Eterm *ptr = boxed_val(*this);
                  Eterm val = *ptr;
                  //printf("(boxed)\r\n");
                  if (ptr_within(ptr, nm_heap, nm_hend))
                  {
                      this++;
                  }
#if HIPE
                  else if( in_area(ptr, const_start, const_size) )
                  {
                      this++;
                  }
#endif
                  else if (MY_IS_MOVED(val))
                  {
                      ASSERT(is_boxed(val));
                      *this++ = val;
                  }
                  else
                  {
                    //printf("ptr: 0x%08x  val: 0x%08x  BN: %d\r\n",(int)ptr,(int)val,BOXED_NEED(ptr,val));
                    //printf("fun-free: %d\r\n",((ErlFunThing*)(ptr - 1))->num_free);
                      Eterm *old_htop = erts_nm_alloc(p,BOXED_NEED(ptr,val),objv,nobj);
                      NM_MARK_FORWARD(ptr);
                      MOVE_BOXED(ptr,val,old_htop,this++);
                  }
                  continue;
              }

              case TAG_PRIMARY_LIST:
              {
                  Eterm *ptr = list_val(*this);
                  Eterm val = *ptr;
                  //printf("(list)\r\n");
                  if (ptr_within(ptr, nm_heap, nm_hend))
                  {
                      this++;
                  }
#if HIPE
                  else if( in_area(ptr, const_start, const_size) )
                  {
                      this++;
                  }
#endif
                  else if (is_non_value(val))
                  {
                      *this++ = ptr[1];
                  }
                  else
                  {
                      Eterm *old_htop = erts_nm_alloc(p,2,objv,nobj);
                      MOVE_CONS(ptr,val,old_htop,this++);
                  }
                  continue;
              }

              case TAG_PRIMARY_HEADER:
              {
                //printf("(header");
                  if (header_is_thing(*this))
                  {
                    //printf("arity: %d)\r\n",thing_arityval(*this));
                      sz -= thing_arityval(*this);
                      this += (thing_arityval(*this) + 1);
                  }
                  else
                  {
                    //printf(")\r\n");
                      this++;
                  }
                  continue;
              }

              default:
              {
                //printf("(other)\r\n");
                  this++;
                  continue;
              }
            }
        }
        obj = NM_STORAGE_GET(blue);
    }
}
#endif /* !INCREMENTAL_GC */

Eterm *erts_nm_alloc(Process *p, int need, Eterm* objv, int nobj)
{
    NM_MemBlock *this = nm_free_list;

    //printf("Allocate %d\r\n",need);
    if (!this)
    {
        this = erts_nm_collect(p,need,objv,nobj);
        if (!this)
          printf("We may be low on memory...\r\n");
    }

    if (need > NM_PAGESIZE)
        printf("nm_alloc: ERROR! Your need is larger than the page size!\r\n");

    while (this->size < need)
    {
      //printf("Too small: %d\r\n",this->size);
        this = this->next;
        if (!this)
        {
            this = erts_nm_collect(p,need,objv,nobj);
        }
    }

    if ((this->size - need) * sizeof(Eterm) >= sizeof(NM_MemBlock))
    {
        NM_MemBlock *rest = (NM_MemBlock*)((Eterm*)this + need);
        //if (this->size > NM_PAGESIZE) print_nm_heap();
        //printf("NF: need: %d  rest: 0x%08x  this: 0x%08x\r\n",need,(int)rest,(int)this);
        /* The order here IS important! */
        rest->next = this->next;
        //printf("this  sz: %d  prev: 0x%08x  next: 0x%08x\r\n",this->size,(int)this->prev,(int)this->next);
        //printf("rest  sz: %d  prev: 0x%08x  next: 0x%08x\r\n",rest->size,(int)rest->prev,(int)rest->next);
        if (rest->next)
            rest->next->prev = rest;

        rest->prev = this->prev;

        if (rest->prev)
            rest->prev->next = rest;
        else
            nm_free_list = rest;

        rest->size = this->size - need;
        //printf("New free @ 0x%08x with size %d\r\n",(int)rest,rest->size);
    }
    else
    {
      //printf("Blocksize left: %d, min size: %d\r\n",this->size - need,sizeof(NM_MemBlock) / sizeof(void*));
        if (this->prev)
            this->prev->next = this->next;
        else
            nm_free_list = this->next;

        if (this->next)
          this->next->prev = this->prev;
        //print_free_list();
    }

    //printf("Allocated %d @ 0x%08x\r\n",need,(int)this);
    //print_nm_heap();

    /* We do not add everything we allocate to the build list, since
     * it would only incur a lot of duplets in the gray list. If all
     * the allocated objects are in the build list they are part of
     * the root set, but they will also be found by the mark scan and
     * added to the gray list.
    */
    NM_STORE(blue,(Eterm*)this,need);

    if (need > 255)
      printf("Implement 32-bit support in blackmap!!\r\n");

    blackmap[((Eterm*)this) - nm_heap] = need;

    return (Eterm*)this;
}

#ifdef DEBUG
void print_nm_heap(void)
{
    NM_Page *this = nm_used_mem;
    while (this)
    {
        Eterm *ptr = (Eterm*)this->start;
        Eterm *end = (Eterm*)this->start + NM_PAGESIZE;

        printf("Page: 0x%08x   Prev: 0x%08x  Next: 0x%08x\r\n",(int)this,(int)this->prev,(int)this->next);
        printf("----------------------------------------------------\r\n");
        print_untagged_memory(ptr,end);
        this = this->next;
    }
}

void print_nm_free_list(void)
{
    NM_MemBlock *this = nm_free_list;

    printf("-- Free list ----------------------------------------------------\r\n");
    while (this)
    {
        printf("Block @ 0x%08x sz: %8d prev: 0x%08x next: 0x%08x\r\n",(int)this,this->size,(int)this->prev,(int)this->next);
        this = this->next;

    }
    printf("-----------------------------------------------------------------\r\n");
}

static void print_page_list(NM_Page *list)
{
    printf("-- Page list -----------------------------------\r\n");
    while (list)
    {
        printf("Page @ 0x%08x prev: 0x%08x next: 0x%08x\r\n",(int)list,(int)list->prev,(int)list->next);
        list = list->next;
    }
    printf("------------------------------------------------\r\n");
}
#endif /* DEBUG */
#endif /* NOMOVE */
