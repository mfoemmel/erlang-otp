/* $Id$
 * hipe_debug.c
 *
 * TODO:
 * - detect mode-switch native return addresses (ARCH-specific)
 * - map user-code native return addresses to symbolic names
 */
#include <stddef.h>	/* offsetof() */
#include <stdio.h>
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "beam_catches.h"
#include "beam_load.h"
#include "hipe_mode_switch.h"
#include "hipe_debug.h"

extern Uint beam_apply[];

static void print_beam_pc(Uint *pc)
{
    if( pc == hipe_beam_pc_return ) {
	printf("return-to-native");
    } else if( pc == hipe_beam_pc_throw ) {
	printf("throw-to-native");
    } else if( pc == &beam_apply[1] ) {
	printf("normal-process-exit");
    } else {
	Eterm *mfa = find_function_from_pc(pc);
	if( mfa ) {
	    display(mfa[0], COUT);
	    printf(":");
	    display(mfa[1], COUT);
	    printf("/%ld", mfa[2]);
	    printf(" + 0x%x", pc - &mfa[3]);
	} else {
	    printf("?");
	}
    }
}

static void print_native_pc(void *pc)
{
    printf("?");
}

#if 0
static void raw_slot(Eterm *pos, Eterm val, const char *type)
{
    printf(" | 0x%08x | 0x%08x | %s\r\n",
	   (unsigned int)pos,
	   (unsigned int)val,
	   type);
}
#endif

static void catch_slot(Eterm *pos, Eterm val, int is_native)
{
    Uint *pc = catch_pc(val);
    printf(" | 0x%08x | 0x%08x | CATCH 0x%08x (%s ",
	   (unsigned int)pos,
	   (unsigned int)val,
	   (unsigned int)pc,
	   is_native ? "NATIVE" : "BEAM");
    if( is_native )
	print_native_pc(pc);
    else
	print_beam_pc(pc);
    printf(")\r\n");
}

static void print_native_cp(Eterm *pos, Eterm val)
{
    printf(" | 0x%08x | 0x%08x | NATIVE PC ",
	   (unsigned int)pos,
	   (unsigned int)val);
    print_native_pc(cp_val(val));
    printf("\r\n");
}

static void print_beam_cp(Eterm *pos, Eterm val)
{
    printf(" |------------|------------| BEAM ACTIVATION RECORD\r\n");
    printf(" | 0x%08x | 0x%08x | BEAM PC ",
	   (unsigned int)pos,
	   (unsigned int)val);
    print_beam_pc(cp_val(val));
    printf("\r\n");
}

static void print_catch(Eterm *pos, Eterm val, int is_native)
{
    printf(" |............|............| %s CATCH FRAME\r\n",
	   is_native ? "NATIVE" : "BEAM");
    catch_slot(pos, val, is_native);
    printf(" |************|************|\r\n");
}

static void print_stack(Eterm *sp, Eterm *end, int is_native)
{
    printf(" | Address    | Contents   |\r\n");
    while( sp < end ) {
	Eterm val = sp[0];
	if( is_CP(val) ) {
	    if( is_native ) {
		print_native_cp(sp, val);
		sp += 1;
	    } else {
		print_beam_cp(sp, val);
		sp += 1;
	    }
	} else if( is_catch(val) ) {
	    print_catch(sp, val, is_native);
	    sp += 1;
	} else {
	    printf(" | 0x%08x | 0x%08lx | ", (unsigned int)sp, val);
	    ldisplay(val, COUT, 30);
	    printf("\r\n");
	    sp += 1;
	}
    }
    printf(" |------------|------------|\r\n");
}

void hipe_print_estack(Process *p)
{
    printf(" |       BEAM  STACK       |\r\n");
    print_stack(p->stop, STACK_START(p), 0);
}

void hipe_print_heap(Process *p)
{
    Eterm *pos = p->heap;
    Eterm *end = p->htop;

    printf("From: 0x%08x  to  0x%08x\n\r",(unsigned int)pos,(unsigned int)end);
    printf(" |         H E A P         |\r\n");
    printf(" | Address    | Contents   |\r\n");
    printf(" |------------|------------|\r\n");
    while( pos < end ) {
	Eterm val = pos[0];
	printf(" | 0x%08x | 0x%08x | ", (unsigned int)pos, (unsigned int)val);
	++pos;
	if( is_arity_value(val) ) {
	    printf("Arity(%lu)", arityval(val));
	} else if( is_thing(val) ) {
	    val = thing_arityval(val);
	    printf("Thing Arity(%lu) Tag(%lu)", val, thing_subtag(val));
	    while( val ) {
		printf("\r\n | 0x%08x | 0x%08lx | THING",
		       (unsigned int)pos, *pos);
		++pos;
		--val;
	    }
	} else
	    ldisplay(val, COUT, 30);
	printf("\r\n");
    }
    printf(" |------------|------------|\r\n");
}

void hipe_check_heap(Process *p)
{
    Eterm *pos = p->heap;
    Eterm *end = p->htop;

    printf("Check heap from: 0x%08x  to:  0x%08x\n\r",(unsigned int)pos,
                                                      (unsigned int)end);
    while( pos < end ) {
	Eterm val = pos[0];
	++pos;
	if( is_thing(val) ) {
	    val = thing_arityval(val);
	    while( val ) {
		(void) *(volatile Eterm*)pos;
		++pos;
		--val;
	    }
	}
    }
    printf("Heap OK.\r\n");
}

void hipe_print_pcb(Process *p)
{
    printf("P: 0x%08lx\r\n", (unsigned long)p);
    printf("-----------------------------------------------\r\n");
    printf("Offset| Name        | Value      | *Value     |\r\n");
#define U(n,x) \
    printf(" % 4d | %s | 0x%08x |            |\r\n", offsetof(Process,x), n, (unsigned)p->x)
#define P(n,x) \
    printf(" % 4d | %s | 0x%08x | 0x%08x |\r\n", offsetof(Process,x), n, (unsigned)p->x, p->x ? (unsigned)*(p->x) : -1U)
    
    U("htop       ", htop);
    U("hend       ", hend);
    U("heap       ", heap);
    U("heap_sz    ", heap_sz);
    U("stop       ", stop);
#ifdef SHARED_HEAP
    U("stack      ", stack);
    U("send       ", send);
#else
    U("gen_gcs    ", gen_gcs);
    U("max_gen_gcs", max_gen_gcs);
    U("high_water ", high_water);
    U("old_hend   ", old_hend);
    U("old_htop   ", old_htop);
    U("old_head   ", old_heap);
#endif
    U("min_heap_..", min_heap_size);
    U("status     ", status);
    U("rstatus    ", rstatus);
    U("rcount     ", rcount);
    U("id         ", id);
    U("prio       ", prio);
    U("reds       ", reds);
    U("error_han..", error_handler);
    U("tracer_pr..", tracer_proc);
    U("group_lea..", group_leader);
    U("flags      ", flags);
    U("fvalue     ", fvalue);
    U("freason    ", freason);
    U("fcalls     ", fcalls);
    /*XXX: ErlTimer tm; */
    U("next       ", next);
    /*XXX: ErlOffHeap off_heap; */
    U("reg        ", reg);
    U("links      ", links);
#ifndef SHARED_HEAP
    /*XXX: ErlMessageQueue msg; */
    U("mbuf       ", mbuf);
    U("mbuf_sz    ", mbuf_sz);
#endif
    U("dictionary ", dictionary);
    U("debug_dic..", debug_dictionary);
    U("ct         ", ct);
    U("seq..clock ", seq_trace_clock);
    U("seq..astcnt", seq_trace_lastcnt);
    U("seq..token ", seq_trace_token);
    U("intial[0]  ", initial[0]);
    U("intial[1]  ", initial[1]);
    U("intial[2]  ", initial[2]);
    P("current    ", current);
    P("cp         ", cp);
    P("i          ", i);
    U("catches    ", catches);
#ifndef SHARED_HEAP
    U("arith_heap ", arith_heap);
    U("arith_avail", arith_avail);
#endif
#ifdef DEBUG
    U("arith_file ", arith_file);
    U("arith_line ", arith_line);
    P("arith_che..", arith_check_me);
#endif
    U("arity      ", arity);
    P("arg_reg    ", arg_reg);
    U("max_arg_reg", max_arg_reg);
    U("def..reg[0]", def_arg_reg[0]);
    U("def..reg[1]", def_arg_reg[1]);
    U("def..reg[2]", def_arg_reg[2]);
    U("def..reg[3]", def_arg_reg[3]);
    U("def..reg[4]", def_arg_reg[4]);
    U("def..reg[5]", def_arg_reg[5]);
#ifdef HIPE
    U("nsp        ", hipe.nsp);
    U("nstack     ", hipe.nstack);
    U("nstend     ", hipe.nstend);
    U("ncallee    ", hipe.ncallee);
#if defined(__sparc__)
    U("nra        ", hipe.nra);
    U("ncra       ", hipe.ncra);
#endif
#if defined(__i386__)
    U("ncsp       ", hipe.ncsp);
    U("narity     ", hipe.narity);
#endif
#endif	/* HIPE */
#undef U
#undef P
    printf("-----------------------------------------------\r\n");
}
