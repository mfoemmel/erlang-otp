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
#include "big.h"
#include "bif.h"
#include "beam_catches.h"
#include "erl_debug.h"
#include "ggc.h"

#ifdef DEBUG

#define WITHIN(ptr, x, y) ((x) <= (ptr) && (ptr) < (y))

#if defined(HYBRID)
#if defined(NOMOVE)
/* Hybrid + NonMoving */
#define IN_HEAP(p, ptr)                                 \
   (WITHIN((ptr), p->heap, p->hend) || (OLD_HEAP(p) &&  \
    WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))) ||         \
    WITHIN((ptr), global_heap, global_hend) ||          \
    WITHIN((ptr), nm_heap, nm_hend))
#else
/* Hybrid */
#define IN_HEAP(p, ptr)                                 \
   (WITHIN((ptr), p->heap, p->hend) || (OLD_HEAP(p) &&  \
    WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))) ||         \
    WITHIN((ptr), global_heap, global_hend) ||          \
    (global_old_heap && WITHIN((ptr),global_old_heap,global_old_hend)))
#endif
#elif (defined(SHARED_HEAP) && defined(NOMOVE))
/* Shared + NonMoving */
#define IN_HEAP(p, ptr)                                 \
   (WITHIN((ptr), p->heap, p->hend) ||                  \
       WITHIN((ptr), nm_heap, nm_hend))
#else
/* Private & Shared */
#define IN_HEAP(p, ptr)                                 \
   (WITHIN((ptr), p->heap, p->hend) || (OLD_HEAP(p) &&  \
       WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))))
#endif

/*
 * This file defines functions for use within a debugger like gdb
 * and the declarations below is just to make gcc quiet.
 */

void pps(Process*, Eterm*);
void ptd(Process*, Eterm);
void paranoid_display(Process*, Eterm, CIO);

static int dcount;

static int pdisplay1(Process* p, Eterm obj, CIO fd);

void ptd(Process* p, Eterm x) 
{
    pdisplay1(p, x, CERR);
    erl_putc('\n', CERR);
}

/*
 * Paranoid version of display which doesn't crasch as easily if there
 * are errors in the data structures.
 */

void
paranoid_display(Process* p, Eterm obj, CIO fd)
{
    dcount = 100000;
    pdisplay1(p, obj, fd);
}

static int
pdisplay1(Process* p, Eterm obj, CIO fd)
{
    int i, k;
    Eterm* nobj;

    if (dcount-- <= 0)
	return(1);

    if (is_CP(obj)) {
	erl_printf(fd, "<cp/header:%08X", obj);
	return 0;
    }

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	erl_printf(fd, "[]");
	break;
    case ATOM_DEF:
	print_atom((int)atom_val(obj), fd);
	break;
    case SMALL_DEF:
	erl_printf(fd, "%ld", signed_val(obj));
	break;

    case BIG_DEF:
	nobj = big_val(obj);
	if (!IN_HEAP(p, nobj)) {
	    erl_printf(fd, "#<bad big %X>#", obj);
	    return 1;
	}

	i = BIG_SIZE(nobj);
	if (BIG_SIGN(nobj))
	    erl_printf(fd, "-#integer(%d) = {", i);
	else
	    erl_printf(fd, "#integer(%d) = {", i);
	erl_printf(fd, "%d", BIG_DIGIT(nobj, 0));
	for (k = 1; k < i; k++)
	    erl_printf(fd, ",%d", BIG_DIGIT(nobj, k));
	erl_putc('}', fd);
	break;
    case REF_DEF:
    case EXTERNAL_REF_DEF: {
	Uint32 *ref_num;
	erl_printf(fd, "#Ref<%lu", ref_channel_no(obj));
	ref_num = ref_numbers(obj);
	for (i = ref_no_of_numbers(obj)-1; i >= 0; i--)
	    erl_printf(fd, ",%lu", ref_num[i]);
	erl_printf(fd, ">");
	break;
    }
    case PID_DEF:
    case EXTERNAL_PID_DEF:
	erl_printf(fd, "<%lu.%lu.%lu>",
		   pid_channel_no(obj),
		   pid_number(obj),
		   pid_serial(obj));
	break;
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:
	erl_printf(fd, "#Port<%lu.%lu>",
		   port_channel_no(obj),
		   port_number(obj));
	break;
    case LIST_DEF:
	erl_putc('[', fd);
	nobj = list_val(obj);
	while (1) {
	    if (!IN_HEAP(p, nobj)) {
		erl_printf(fd, "#<bad list %X>", obj);
		return 1;
	    }
	    if (pdisplay1(p, *nobj++, fd) != 0)
		return(1);
	    if (is_not_list(*nobj))
		break;
	    erl_putc(',', fd);
	    nobj = list_val(*nobj);
	}
	if (is_not_nil(*nobj)) {
	    erl_putc('|', fd);
	    if (pdisplay1(p, *nobj, fd) != 0)
		return(1);
	}
	erl_putc(']', fd);
	break;
    case TUPLE_DEF:
	nobj = tuple_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	erl_putc('{', fd);
	while (i--) {
	    if (pdisplay1(p, *++nobj,fd) != 0) return(1);
	    if (i >= 1) erl_putc(',',fd);
	}
	erl_putc('}',fd);
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
#ifdef _OSE_
	    erl_printf(fd, "%e", ff.fd);
#else
	    erl_printf(fd, "%.20e", ff.fd);
#endif
	}
	break;
    case BINARY_DEF:
	erl_printf(fd, "#Bin");
	break;
    default:
	erl_printf(fd, "unknown object %x", obj);
    }
    return(0);
}

void
pps(Process* p, Eterm* stop)
{
    Eterm* sp = STACK_START(p) - 1;

    if (stop <= STACK_END(p)) {
        stop = STACK_END(p) + 1;
    }

    while(sp >= stop) {
	erl_printf(COUT,"%08lx: ", (Eterm) sp);
	if (is_catch(*sp)) {
	    erl_printf(COUT, "catch %d", (Uint)catch_pc(*sp));
	} else {
	    paranoid_display(p, *sp, COUT);
	}
	erl_putc('\r', COUT);
	erl_putc('\n', COUT);
	sp--;
    }
}

#endif /* DEBUG */

/*
 * check_heap and check_memory will run through the heap silently if
 * everything is ok.  If there are strange (untagged) data in the heap
 * this check will (most likely) fail and segfault (which is the
 * intended result..). I know it could be done in a nicer way with
 * assertions etc, but right now I only want this to find holes in the
 * heap.
 */
void check_heap(Process *p)
{
    check_memory(HEAP_START(p),HEAP_TOP(p));
#ifndef NOMOVE
    if (OLD_HEAP(p) != NULL)
        check_memory(OLD_HEAP(p),OLD_HTOP(p));
#endif
}

void check_memory(Eterm *start, Eterm *end)
{
    Eterm *pos = start;
    volatile Sint check;

    /* printf("Scan 0x%08x - 0x%08x\r\n",start,end); */
    while (pos < end) {
        Eterm hval = *pos++;

#ifdef DEBUG
#ifdef SHARED_HEAP
        if (hval == ARITH_MARKER || hval == 0x01010101) {
            continue;
        }
#else
        if (hval == ARITH_MARKER) {
            erl_exit(1, "erl_debug, check_memory: ARITH_MARKER found in heap fragment @ 0x%08x!\n",
		     (Sint)(pos-1));
            break;
        }
        else if (hval == 0x01010101) {
            print_untagged_memory(start, end);
            erl_exit(1, "erl_debug, check_memory: Uninitialized HAlloc'ed memory found @ 0x%08x!\n",
		     (Sint)(pos-1));
            break;
        }
#endif
#endif
        switch (primary_tag(hval)) {

        case TAG_PRIMARY_LIST: {
            Eterm *ptr = list_val(hval);
            check = (Sint)*ptr;
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *ptr = boxed_val(hval);
            check = (Sint)*ptr;
            continue;
        }

        case TAG_PRIMARY_HEADER: {
            if (header_is_thing(hval))
                pos += (thing_arityval(hval));
            continue;
        }

        default:
            /* Immediate value */
            continue;
        }
    }
}

/*
 * print_untagged_memory will print the contents of given memory area.
 */
void print_untagged_memory(Eterm *pos, Eterm *end)
{
    int i = 0;
    printf(" | H E A P   D U M P                                        |\r\n");
    printf(" | From: 0x%08lx  to  0x%08lx                         |\r\n",
	   (Uint)pos,(Uint)(end - 1));
    printf(" | Address    | Contents                                    |\r\n");
    printf(" |------------|---------------------------------------------|\r\n");
    while( pos < end ) {
        if (i == 0)
            printf(" | 0x%08lx | ", (Uint)pos);
        printf("0x%08lx ", (Uint)*pos);
        pos++; i++;
        if (i == 4) {
            printf("|\r\n");
            i = 0;
        }
    }
    while (i && i < 4) {
        printf("           ");
        i++;
    }
    if (i != 0)
        printf("|\r\n");
    printf(" |------------|---------------------------------------------|\r\n");
}

/*
 * print_tagged_memory will print contents of given memory area and
 * display it as if it was tagged Erlang terms (which it hopefully
 * is).  This function knows about forwarding pointers to be able to
 * print a heap during garbage collection. ldisplay do not know about
 * forwarding pointers though, so it will still crash if they are
 * encoutered...
 */
void print_tagged_memory(Eterm *pos, Eterm *end)
{
    printf("From: 0x%08lx  to  0x%08lx\n\r",(Uint)pos,(Uint)(end - 1));
    printf(" |         H E A P         |\r\n");
    printf(" | Address    | Contents   |\r\n");
    printf(" |------------|------------|\r\n");
    while( pos < end ) {
	Eterm val = pos[0];
	printf(" | 0x%08lx | 0x%08lx | ", (Uint)pos, (Uint)val);
	++pos;
        if( is_arity_value(val) ) {
	    printf("Arity(%lu)", arityval(val));
	} else if( is_thing(val) ) {
	    unsigned int ari = thing_arityval(val);
	    printf("Thing Arity(%u) Tag(%lu)", ari, thing_subtag(val));
	    while( ari ) {
		printf("\r\n | 0x%08lx | 0x%08lx | THING",
		       (Uint)pos, *pos);
		++pos;
		--ari;
	    }
	} else {
            switch (primary_tag(val)) {
            case TAG_PRIMARY_BOXED:
                if (!is_header(*boxed_val(val))) {
                    printf("Moved -> 0x%08x\r\n",
                           (unsigned int)*boxed_val(val));
                    continue;
                }
                break;

            case TAG_PRIMARY_LIST:
                if (is_non_value(*list_val(val))) {
                    printf("Moved -> 0x%08x\r\n",
                           (unsigned int)*(list_val(val) + 1));
                    continue;
                }
                break;
            }
            ldisplay(val, COUT, 30);
        }
	printf("\r\n");
    }
    printf(" |------------|------------|\r\n");
}

#ifdef HYBRID
void print_ma_info(void)
{
    printf("Message Area (start - top - end): 0x%08lx - 0x%08lx - 0x%08lx\r\n",
           (unsigned long)global_heap,(unsigned long)global_htop,(unsigned long)global_hend);
#ifndef NOMOVE
    printf("  High water: 0x%08lx   Old gen: 0x%08lx - 0x%08lx - 0x%08lx\r\n",
           (unsigned long)global_high_water,
           (unsigned long)global_old_heap,(unsigned long)global_old_htop,(unsigned long)global_old_hend);
#endif
}

void print_message_area(void)
{
    Eterm *pos = global_heap;
    Eterm *end = global_htop;

    printf("From: 0x%08lx  to  0x%08lx\n\r",(unsigned long)pos,(unsigned long)end);
    printf("(Old generation: 0x%08lx  to 0x%08lx\n\r",(unsigned long)OLD_M_DATA_START,(unsigned long)OLD_M_DATA_END);
    printf(" |         H E A P         |\r\n");
    printf(" | Address    | Contents   |\r\n");
    printf(" |------------|------------|\r\n");
    while( pos < end ) {
	Eterm val = pos[0];
	printf(" | 0x%08lx | 0x%08x | ", (unsigned long)pos, (unsigned int)val);
	++pos;
	if( is_arity_value(val) ) {
	    printf("Arity(%lu)", arityval(val));
	} else if( is_thing(val) ) {
	    unsigned int ari = thing_arityval(val);
	    printf("Thing Arity(%u) Tag(%lu)", ari, thing_subtag(val));
	    while( ari ) {
		printf("\r\n | 0x%08lx | 0x%08lx | THING",
		       (unsigned long)pos, *pos);
		++pos;
		--ari;
	    }
	} else
	    ldisplay(val, COUT, 30);
	printf("\r\n");
    }
    printf(" |------------|------------|\r\n");
}

void check_message_area()
{
    Eterm *pos = global_heap;
    Eterm *end = global_htop;

    while( pos < end ) {
	Eterm val = *pos++;
	if(is_header(val))
	    pos += thing_arityval(val);
	else if(!is_immed(val))
	    if ((ptr_val(val) < global_heap || ptr_val(val) >= global_htop) &&
                (ptr_val(val) < OLD_M_DATA_START || ptr_val(val) >= OLD_M_DATA_END))
            {
		printf("check_message_area: Stray pointer found\r\n");
                print_message_area();
                printf("Crashing to make it look real...\r\n");
                pos = 0;
            }
    }
}
#endif /* HYBRID */

void print_memory_info(Process *p)
{
    printf("===============================================================\r\n");
    if (p != NULL) {
        printf("| Memory info for %12s                                |\r\n",print_pid(p));
        printf("|- local heap ------------------------------------------------|\r\n");
        printf("| Young | 0x%08lx - (0x%08lx) - 0x%08lx - 0x%08lx |\r\n",(unsigned long)HEAP_START(p),(unsigned long)HIGH_WATER(p),(unsigned long)HEAP_TOP(p),(unsigned long)HEAP_END(p));
        if (OLD_HEAP(p) != NULL)
            printf("| Old   | 0x%08lx - 0x%08lx - 0x%08lx                |\r\n",(unsigned long)OLD_HEAP(p),(unsigned long)OLD_HTOP(p),(unsigned long)OLD_HEND(p));
    } else {
        printf("| Memory info (Global memory only)                            |\r\n");
#ifdef SHARED_HEAP
        printf("|- shared heap -----------------------------------------------|\r\n");
        printf("| Young | 0x%08lx - (0x%08lx) - 0x%08lx - 0x%08lx |\r\n",(unsigned long)HEAP_START(p),(unsigned long)HIGH_WATER(p),(unsigned long)HEAP_TOP(p),(unsigned long)HEAP_END(p));
        if (OLD_HEAP(p) != NULL)
            printf("| Old   | 0x%08lx - 0x%08lx - 0x%08lx                |\r\n",(unsigned long)OLD_HEAP(p),(unsigned long)OLD_HTOP(p),(unsigned long)OLD_HEND(p));
#endif
    }
#ifdef HYBRID
    printf("|- message area ----------------------------------------------|\r\n");
    printf("| Young | 0x%08lx - 0x%08lx - 0x%08lx                |\r\n",(unsigned long)global_heap,(unsigned long)global_htop,(unsigned long)global_hend);
    printf("| Old   | 0x%08lx - 0x%08lx                             |\r\n",(unsigned long)OLD_M_DATA_START,(unsigned long)OLD_M_DATA_END);
#endif
#ifdef INCREMENTAL_GC
    if (inc_n2 != NULL)
        printf("| N2   | 0x%08lx - 0x%08lx                              |\r\n",(unsigned long)inc_n2,(unsigned long)inc_n2_end);
#endif
    printf("===============================================================\r\n");
}

#ifdef INCREMENTAL_GC
void print_active_procs(void)
{
    Process *p = inc_active_proc;

    printf("Active processes:\r\n");
    while(p) {
        printf("%s\r\n",print_pid(p));
        p = p->active_next;
    }
}
#endif /* INCREMENTAL_GC */
