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

#define WITHIN(ptr, x, y) ((x) <= (ptr) && (ptr) < (y))

#define IN_HEAP(p, ptr) \
   (WITHIN((ptr), (p)->heap, (p)->hend) || (p->old_heap && \
       WITHIN((ptr), (p)->old_heap, (p)->old_hend)))

/*
 * This file defines functions for use within a debugger like gdb
 * and the declarations below is just to make gcc quit.
 */

void pps(Process*, uint32*);
void ptd(Process*, uint32);
void paranoid_display(Process*, uint32, CIO);

static int dcount;

static int pdisplay1(Process* p, uint32 obj, CIO fd);

void ptd(Process* p, uint32 x) 
{
    pdisplay1(p, x, CERR);
    erl_putc('\n', CERR);
}

/*
 * Paranoid version of display which doesn't crasch as easily if there
 * are errors in the data structures.
 */

void
paranoid_display(Process* p, uint32 obj, CIO fd)
{
    dcount = 100000;
    pdisplay1(p, obj, fd);
}

static int
pdisplay1(Process* p, uint32 obj, CIO fd)
{
    int i, k;
    uint32 *nobj;

    if (dcount-- <= 0)
	return(1);

    switch (tag_val_def(obj)) {
    case ATOM_DEF:
        if(is_nil(obj)) {
            erl_printf(fd, "[]");
            break;
        }
	print_atom((int)unsigned_val(obj), fd);
	break;
    case SMALL_DEF:
	erl_printf(fd, "%d", signed_val(obj));
	break;

    case BIG_DEF:
	nobj = ptr_val(obj);
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
    case REFER_DEF:
	erl_printf(fd, "<<%d",
		   get_node_reference(obj));
	for (i = refer_arity(obj)-2; i >= 0; i--)
	    erl_printf(fd, ",%lu",
		       ref_ptr(obj)->w[i]);
	erl_printf(fd, ">>");
	break;
    case PID_DEF:
	erl_printf(fd, "<%d.%d.%d>",
		get_node(obj),get_number(obj),get_serial(obj));
	break;
    case PORT_DEF:
	erl_printf(fd, "<%d,%d>", get_node_port(obj),
		get_number_port(obj));
	break;
    case LIST_DEF:
	erl_putc('[', fd);
	nobj = ptr_val(obj);
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
	    nobj = ptr_val(*nobj);
	}
	if (is_not_nil(*nobj)) {
	    erl_putc('|', fd);
	    if (pdisplay1(p, *nobj, fd) != 0)
		return(1);
	}
	erl_putc(']', fd);
	break;
    case TUPLE_DEF:
	nobj = ptr_val(obj);	/* pointer to arity */
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
	    erl_printf(fd, "%.20e", ff.fd);
	}
	break;
    case BINARY_DEF:
	erl_printf(fd, "#Bin");
	break;
    case CP0:
#ifndef NOT_ALIGNED
    case CP4:
    case CP8:
    case CP12:
#endif
        erl_printf(fd, "cp %d", obj);
        break;  
    case BLANK:
        erl_printf(fd, "blank");
        break;  
    default:
	erl_printf(fd, "unknown object %x", obj);
    }
    return(0);
}

void
pps(p, stop)
Process* p; uint32* stop;
{
    uint32* sp = p->hend-1;

    if (stop <= p->htop) {
	stop = p->htop + 1;
    }

    while(sp >= stop) {
	erl_printf(COUT,"%08lx: ", (uint32) sp);
	if (is_catch(*sp)) {
	    erl_printf(COUT, "catch %d", ptr_val(*sp));
	} else {
	    paranoid_display(p, *sp, COUT);
	}
	erl_putc('\r', COUT);
	erl_putc('\n', COUT);
	sp--;
    }
}
