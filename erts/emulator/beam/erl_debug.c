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

#define WITHIN(ptr, x, y) ((x) <= (ptr) && (ptr) < (y))

#define IN_HEAP(p, ptr) \
   (WITHIN((ptr), p->heap, p->hend) || (OLD_HEAP(p) && \
       WITHIN((ptr), OLD_HEAP(p), OLD_HEND(p))))

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
	erl_printf(fd, "%d", signed_val(obj));
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
