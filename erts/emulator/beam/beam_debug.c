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
/*
 * Purpose: Basic debugging support.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "external.h"
#include "beam_load.h"

void dbg_bt(Process* p, Eterm* sp);
void dbg_where(Eterm* addr, Eterm x0, Eterm* reg);

static void print_big(CIO fd, Eterm* addr);
static int print_op(CIO to, int op, int size, Eterm* addr);
static Eterm* find_code(Eterm* addr);

extern Eterm beam_debug_apply[];
extern int beam_debug_apply_size;

Eterm
erts_debug_make_fun_1(Process* p, Eterm tuple)
{
    Eterm* tp;
    Eterm creator;
    Eterm module;
    Eterm index;
    Eterm uniq;
    Eterm env;
    int num_free;
    unsigned needed;
    ErlFunThing* funp;
    int i;

    /*
     * Retrieve and validate input tuple.
     */

    if (!is_tuple(tuple)) {
    error:
	BIF_ERROR(p, BADARG);
    }
    tp = ptr_val(tuple);
    if (tp[0] != make_arityval(5)) {
	goto error;
    }
    if (!is_pid(creator = tp[1])) {
	goto error;
    }
    if (!is_atom(module = tp[2])) {
	goto error;
    }
    if (!is_small(index = tp[3])) {
	goto error;
    }
    if (!is_small(uniq = tp[4])) {
	goto error;
    }
    env = tp[5];
    if ((num_free = list_length(env)) < 0) {
	goto error;
    }

    /*
     * Create the fun struct and fill it in.
     */

    needed = ERL_FUN_SIZE + num_free - 1;
    funp = (ErlFunThing *) HAlloc(p, needed);
    funp->thing_word = make_thing(ERL_FUN_SIZE-2, FUN_SUBTAG);
    funp->next = p->off_heap.funs;
    p->off_heap.funs = funp;
    funp->modp = erts_put_module(module);
    funp->index = unsigned_val(index);
    funp->uniq = uniq;
    funp->num_free = num_free;
    funp->creator = creator;
    for (i = 0; i < num_free; i++) {
	Eterm* ep = ptr_val(env);
	funp->env[i] = CAR(ep);
	env = CDR(ep);
    }
    return make_binary(funp);
}

Eterm
erts_debug_disassemble_1(Process* p, Eterm addr)
{
    Eterm* hp;
    Eterm* tp;
    Eterm bin;
    Eterm mfa;
    Eterm* funcinfo;
    Uint* code_base;
    Uint* code_ptr;
    Uint instr;
    unsigned uaddr;
    int i;

    if (is_small(addr) && signed_val(addr) > 0) {
	code_ptr = (Uint *) unsigned_val(addr);
	
	if ((funcinfo = find_code(code_ptr)) == NULL) {
	    BIF_RET(am_false);
	}
    } else if (is_big(addr) && big_to_unsigned(addr, &uaddr)) {
	code_ptr = (Uint *) uaddr;
	if ((funcinfo = find_code(code_ptr)) == NULL) {
	    BIF_RET(am_false);
	}
    } else if (is_tuple(addr)) {
	Module* modp;
	Eterm mod;
	Eterm name;
	Export* ep;
	int arity;
	int n;

	tp = ptr_val(addr);
	if (tp[0] != make_arityval(3)) {
	error:
	    BIF_ERROR(p, BADARG);
	}
	mod = tp[1];
	name = tp[2];
	if (!is_atom(mod) || !is_atom(name) || !is_small(tp[3])) {
	    goto error;
	}
	arity = signed_val(tp[3]);
	modp = erts_get_module(mod);

	/*
	 * Try the export entry first to allow disassembly of special functions
	 * such as erts_debug:apply/4.  Then search for it in the module.
	 */

	if ((ep = erts_find_function(mod, name, arity)) != NULL) {
	    code_ptr = ((Eterm *) ep->address) - 4;
	    funcinfo = code_ptr+1;
	} else if (modp == NULL || (code_base = modp->code) == NULL) {
	    BIF_RET(am_undef);
	} else {
	    n = code_base[MI_NUM_FUNCTIONS];
	    for (i = 0; i < n; i++) {
		code_ptr = (Uint *) code_base[MI_FUNCTIONS+i];
		if (code_ptr[2] == name && code_ptr[3] == arity) {
		    funcinfo = code_ptr+1;
		    break;
		}
	    }
	    if (i == n) {
		BIF_RET(am_undef);
	    }
	}
    } else {
	goto error;
    }

    cerr_pos = 0;
    erl_printf(CBUF, "%08X: ", code_ptr);
    instr = (Uint) code_ptr[0];
    for (i = 0; i < NUM_SPECIFIC_OPS; i++) {
	if (instr == (Uint) beam_ops[i] && opc[i].name[0] != '\0') {
	    code_ptr += print_op(CBUF, i, opc[i].sz-1, code_ptr+1) + 1;
	    break;
	}
    }
    if (i >= NUM_SPECIFIC_OPS) {
	erl_printf(CBUF, "unknown %d\n", instr);
	code_ptr++;
    }
    addr = make_small_or_big((Uint) code_ptr, p);
    bin = new_binary(p, tmp_buf, cerr_pos);
    hp = HAlloc(p, 4+4);
    ASSERT(is_atom(funcinfo[0]));
    ASSERT(is_atom(funcinfo[1]));
    mfa = TUPLE3(hp, funcinfo[0], funcinfo[1], make_small(funcinfo[2]));
    hp += 4;
    return TUPLE3(hp, addr, bin, mfa);
}

static Eterm*
find_code(Eterm* addr)
{
    if (beam_debug_apply <= addr && addr < beam_debug_apply+beam_debug_apply_size) {
	return beam_debug_apply+1;
    }
    return find_function_from_pc(addr);
}

void
dbg_bt(Process* p, Eterm* sp)
{
    Eterm* stack = p->hend;

    while (sp < stack) {
	if (is_CP(*sp)) {
	    Eterm* addr = find_function_from_pc(cp_ptr_val(*sp));
	    if (addr) {
		sys_printf(CERR, "%08X: ", addr);
		display(addr[0], CERR);
		sys_printf(CERR, ":");
		display(addr[1], CERR);
		sys_printf(CERR, "/%d", addr[2]);
		sys_printf(CERR, "\n");
	    }
	}
	sp++;
    }
}

void
dbg_where(Eterm* addr, Eterm x0, Eterm* reg)
{
    Eterm* f = find_function_from_pc(addr);

    if (f == NULL) {
	sys_printf(CERR, "???\n");
    } else {
	int arity;
	char* sep = "";
	int i;

	addr = f;
	arity = addr[2];
	sys_printf(CERR, "%08X: ", addr);
	display(addr[0], CERR);
	sys_printf(CERR, ":");
	display(addr[1], CERR);
	sys_printf(CERR, "(");
	for (i = 0; i < arity; i++) {
	    sys_printf(CERR, "%s", sep);
	    sep = ", ";
	    if (i == 0) {
		display(x0, CERR);
	    } else {
		display(reg[i], CERR);
	    }
	}
	sys_printf(CERR, ")\n");
    }
}

static int
print_op(CIO to, int op, int size, Eterm* addr)
{
    int i;
    Uint tag;
    char* sign;
    char* start_prog;		/* Start of program for packer. */
    char* prog;			/* Current position in packer program. */
    Uint stack[8];		/* Stack for packer. */
    Uint* sp = stack;		/* Points to next free position. */
    Uint packed = 0;		/* Accumulator for packed operations. */
    Uint args[8];		/* Arguments for this instruction. */
    Uint* ap;			/* Pointer to arguments. */

    /*
     * Copy all arguments to a local buffer.
     */

    ASSERT(size <= sizeof(args)/sizeof(args[0]));
    ap = args;
    for (i = 0; i < size; i++) {
	*ap++ = addr[i];
    }

    /*
     * Undo any packing done by the loader.  This is easily done by running
     * the packing program backwards and in reverse.
     */

    start_prog = opc[op].pack;
    prog = start_prog + strlen(start_prog);
    while (start_prog < prog) {
	prog--;
	switch (*prog) {
	case 'g':
	    *ap++ = *--sp;
	    break;
	case 'i':	/* Initialize packing accumulator. */
	    *ap++ = packed;
	    break;
	case 's':
	    *ap++ = packed & 0x3ff;
	    packed >>= 10;
	    break;
	case '0':	/* Shift 10 steps */
	    *ap++ = packed & 0x3ff;
	    packed >>= 10;
	    break;
	case '2':	/* Shift 12 steps */
	    *ap++ = packed & 0x3ff;
	    packed >>= 12;
	    break;
	case '6':	/* Shift 16 steps */
	    *ap++ = packed & 0x3ff;
	    packed >>= 16;
	    break;
	case 'p':
	    *sp++ = *--ap;
	    break;
	case 'P':
	    packed = *--sp;
	    break;
	default:
	    ASSERT(0);
	}
    }
    
    /*
     * Print the name and all operands of the instructions.
     */
	
    erl_printf(to, "%s ", opc[op].name);
    ap = args;
    sign = opc[op].sign;
    while (*sign) {
	switch (*sign) {
	case 'r':		/* x(0) */
	    erl_printf(to, "x(0)");
	    break;
	case 'x':		/* x(N) */
	    erl_printf(to, "x(%d)", ap[0]/4);
	    ap++;
	    break;
	case 'y':		/* y(N) */
	    erl_printf(to, "y(%d)", ap[0]/4-1);
	    ap++;
	    break;
	case 'n':		/* Nil */
	    erl_printf(to, "[]");
	    break;
	case 's':		/* Any source (tagged constant or register) */
	    tag = beam_reg_tag(*ap);
	    if (tag == X_REG_DEF) {
		erl_printf(to, "x(%d)", x_reg_number(*ap) >> 2);
		ap++;
		break;
	    } else if (tag == Y_REG_DEF) {
		erl_printf(to, "y(%d)", (y_reg_number(*ap) >> 2) - 1);
		ap++;
		break;
	    } else if (tag == R_REG_DEF) {
		erl_printf(to, "x(0)");
		ap++;
		break;
	    }
	case 'a':		/* Tagged constant */
	case 'i':		/* Tagged integer */
	case 'c':		/* Tagged constant */
	    display(*ap, to);
	    ap++;
	    break;
	case 'A':
	    erl_printf(to, "%d", unsigned_val(ap[0]));
	    ap++;
	    break;
	case 'd':		/* Destination (x(0), x(N), y(N)) */
	    switch (beam_reg_tag(*ap)) {
	    case X_REG_DEF:
		erl_printf(to, "x(%d)", (x_reg_number(*ap) >> 2));
		break;
	    case Y_REG_DEF:
		erl_printf(to, "y(%d)", (y_reg_number(*ap) >> 2) - 1);
		break;
	    case R_REG_DEF:
		erl_printf(to, "x(0)");
		break;
	    }
	    ap++;
	    break;
	case 'I':		/* Untagged integer. */
	case 't':
	    erl_printf(to, "%d", *ap);
	    ap++;
	    break;
	case 'f':		/* Destination label */
	    erl_printf(to, "f(%X)", *ap);
	    ap++;
	    break;
	case 'p':		/* Pointer (to label) */
	    {
		Eterm* f = find_function_from_pc((Eterm *)*ap);

		if (f+3 != (Eterm *) *ap) {
		    erl_printf(to, "p(%X)", *ap);
		} else {
		    display(f[0], to);
		    erl_printf(to, ":");
		    display(f[1], to);
		    erl_printf(to, "/%d", f[2]);
		}
		ap++;
	    }
	    break;
	case 'j':		/* Pointer (to label) */
	    erl_printf(to, "j(%X)", *ap);
	    ap++;
	    break;
	case 'e':		/* Export entry */
	    {
		Export* ex = (Export *) *ap;
		display(ex->code[0], to);
		erl_printf(to, ":");
		display(ex->code[1], to);
		erl_printf(to, "/%d", ex->code[2]);
		ap++;
	    }
	    break;
	case 'F':		/* Function definition */
	    break;
	case 'b':
	    for (i = 0; i < BIF_SIZE; i++) {
		BifFunction bif = (BifFunction) *ap;
		if (bif == bif_table[i].f) {
		    break;
		}
	    }
	    if (i == BIF_SIZE) {
		erl_printf(to, "b(%d)", (Uint) *ap);
	    } else {
		Eterm name = bif_table[i].name;
		unsigned arity = bif_table[i].arity;
		display(name, to);
		erl_printf(to, "/%d", arity);
	    }
	    ap++;
	    break;
	case 'P':		/* Untagged integer. */
	    erl_printf(to, "%d", (*ap-1)/4);
	    ap++;
	    break;
	case 'w':
	    {
		int arity = thing_arityval(ap[0]);
		print_big(to, ap);
		ap += arity + 1;
		size += arity;
	    }
	    break;
	default:
	    erl_printf(to, "???");
	    ap++;
	    break;
	}
	erl_printf(to, " ");
	sign++;
    }

    /*
     * Print more information about certain instructions.
     */

    ap = addr + size;
    switch (op) {
    case op_i_select_val_sfI:
	{
	    int n = ap[-1];

	    while (n > 0) {
		display(ap[0], to);
		erl_printf(to, " f(%X) ", ap[1]);
		ap += 2;
		size += 2;
		n--;
	    }
	}
	break;
    case op_i_jump_on_val_sfII:
	{
	    int n;
	    for (n = ap[-2]; n > 0; n--) {
		erl_printf(to, "f(%X) ", ap[0]);
		ap++;
		size++;
	    }
	}
	break;
    case op_i_select_big_sf:
	while (ap[0]) {
	    int arity = thing_arityval(ap[0]);
	    print_big(to, ap);
	    size += arity+1;
	    ap += arity+1;
	    erl_printf(to, " f(%X) ", ap[0]);
	    ap++;
	    size++;
	}
	ap++;
	size++;
	break;
    }
    erl_printf(to, "\n");

    return size;
}

static void
print_big(CIO fd, Eterm* addr)
{
    int i;
    int k;

    i = BIG_SIZE(addr);
    if (BIG_SIGN(addr))
	erl_printf(fd, "-#integer(%d) = {", i);
    else
	erl_printf(fd, "#integer(%d) = {", i);
    erl_printf(fd, "%d", BIG_DIGIT(addr, 0));
    for (k = 1; k < i; k++)
	erl_printf(fd, ",%d", BIG_DIGIT(addr, k));
    erl_putc('}', fd);
}
