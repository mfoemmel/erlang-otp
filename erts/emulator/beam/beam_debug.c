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
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "external.h"
#include "beam_load.h"
#include "beam_bp.h"
#include "erl_binary.h"

#ifdef ARCH_64
# define HEXF "%016lX"
#else
# define HEXF "%08X"
#endif

void dbg_bt(Process* p, Eterm* sp);
void dbg_where(Eterm* addr, Eterm x0, Eterm* reg);

static void print_big(CIO fd, Eterm* addr);
static int print_op(CIO to, int op, int size, Eterm* addr);
static Eterm* find_code(Eterm* addr);

extern Eterm beam_debug_apply[];
extern int beam_debug_apply_size;

Eterm
erts_debug_same_2(Process* p, Eterm term1, Eterm term2)
{
    return (term1 == term2) ? am_true : am_false;
}

Eterm
erts_debug_flat_size_1(Process* p, Eterm term)
{
    return make_small_or_big(size_object(term), p);
}

Eterm
erts_debug_breakpoint_2(Process* p, Eterm MFA, Eterm bool)
{
    Eterm* tp;
    Eterm mfa[3];
    int i;
    int specified = 0;

    if (is_not_tuple(MFA)) {
	goto error;
    }
    tp = tuple_val(MFA);
    if (*tp != make_arityval(3)) {
	goto error;
    }
    mfa[0] = tp[1];
    mfa[1] = tp[2];
    mfa[2] = tp[3];
    if (!is_atom(mfa[0]) || !is_atom(mfa[1]) ||
	(!is_small(mfa[2]) && mfa[2] != am_Underscore)) {
	goto error;
    }
    for (i = 0; i < 3 && mfa[i] != am_Underscore; i++, specified++) {
	/* Empty loop body */
    }
    for (i = specified; i < 3; i++) {
	if (mfa[i] != am_Underscore) {
	    goto error;
	}
    }
    if (is_small(mfa[2])) {
	mfa[2] = signed_val(mfa[2]);
    }
    if (bool == am_true) {
	return make_small(erts_set_debug_break(mfa, specified));
    } else if (bool == am_false) {
	return make_small(erts_clear_debug_break(mfa, specified));
    }

 error:
    BIF_ERROR(p, BADARG);
}

Eterm
erts_debug_disassemble_1(Process* p, Eterm addr)
{
    Eterm* hp;
    Eterm* tp;
    Eterm bin;
    Eterm mfa;
    Eterm* funcinfo = NULL;	/* Initialized to eliminate warning. */
    Uint* code_base;
    Uint* code_ptr = NULL;	/* Initialized to eliminate warning. */
    Uint instr;
    Uint uaddr;
    int i;

    if (term_to_Uint(addr, &uaddr)) {
	code_ptr = (Uint *) uaddr;
	if ((funcinfo = find_code(code_ptr)) == NULL) {
	    BIF_RET(am_false);
	}
    } else if (is_tuple(addr)) {
	Module* modp;
	Eterm mod;
	Eterm name;
	Export* ep;
	Sint arity;
	int n;

	tp = tuple_val(addr);
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
	    /* XXX: add "&& ep->address != ep->code+3" condition?
	     * Consider a traced function.
	     * Its ep will have ep->address == ep->code+3.
	     * erts_find_function() will return the non-NULL ep.
	     * Below we'll try to derive a code_ptr from ep->address.
	     * But this code_ptr will point to the start of the Export,
	     * not the function's func_info instruction. BOOM !?
	     */
	    code_ptr = ((Eterm *) ep->address) - 5;
	    funcinfo = code_ptr+2;
	} else if (modp == NULL || (code_base = modp->code) == NULL) {
	    BIF_RET(am_undef);
	} else {
	    n = code_base[MI_NUM_FUNCTIONS];
	    for (i = 0; i < n; i++) {
		code_ptr = (Uint *) code_base[MI_FUNCTIONS+i];
		if (code_ptr[3] == name && code_ptr[4] == arity) {
		    funcinfo = code_ptr+2;
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
    erl_printf(CBUF, HEXF ": ", code_ptr);
    instr = (Uint) code_ptr[0];
    for (i = 0; i < NUM_SPECIFIC_OPS; i++) {
	if (instr == (Uint) BeamOp(i) && opc[i].name[0] != '\0') {
	    code_ptr += print_op(CBUF, i, opc[i].sz-1, code_ptr+1) + 1;
	    break;
	}
    }
    if (i >= NUM_SPECIFIC_OPS) {
	erl_printf(CBUF, "unknown " HEXF "\n", instr);
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
	return beam_debug_apply+2;
    }
    return find_function_from_pc(addr);
}

void
dbg_bt(Process* p, Eterm* sp)
{
    Eterm* stack = STACK_START(p);

    while (sp < stack) {
	if (is_CP(*sp)) {
	    Eterm* addr = find_function_from_pc(cp_val(*sp));
	    if (addr) {
		sys_printf(CERR, HEXF ": ", addr);
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
	sys_printf(CERR, HEXF ": ", addr);
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
	case '0':	/* Tight shift */
	    *ap++ = packed & (BEAM_TIGHT_MASK / sizeof(Eterm));
	    packed >>= BEAM_TIGHT_SHIFT;
	    break;
	case '6':	/* Shift 16 steps */
	    *ap++ = packed & 0xffff;
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
	    erl_printf(to, "x(%d)", reg_index(ap[0]));
	    ap++;
	    break;
	case 'y':		/* y(N) */
	    erl_printf(to, "y(%d)", reg_index(ap[0]) - CP_SIZE);
	    ap++;
	    break;
	case 'n':		/* Nil */
	    erl_printf(to, "[]");
	    break;
	case 's':		/* Any source (tagged constant or register) */
	    tag = beam_reg_tag(*ap);
	    if (tag == X_REG_DEF) {
		erl_printf(to, "x(%d)", reg_index(*ap));
		ap++;
		break;
	    } else if (tag == Y_REG_DEF) {
		erl_printf(to, "y(%d)", reg_index(*ap) - CP_SIZE);
		ap++;
		break;
	    } else if (tag == R_REG_DEF) {
		erl_printf(to, "x(0)");
		ap++;
		break;
	    }
	    /*FALLTHROUGH*/
	case 'a':		/* Tagged atom */
	case 'i':		/* Tagged integer */
	case 'c':		/* Tagged constant */
	    display(*ap, to);
	    ap++;
	    break;
	case 'A':
	    erl_printf(to, "%d", arityval(ap[0]));
	    ap++;
	    break;
	case 'd':		/* Destination (x(0), x(N), y(N)) */
	    switch (beam_reg_tag(*ap)) {
	    case X_REG_DEF:
		erl_printf(to, "x(%d)", reg_index(*ap));
		break;
	    case Y_REG_DEF:
		erl_printf(to, "y(%d)", reg_index(*ap) - CP_SIZE);
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
	case 'P':	/* Byte offset into tuple (see beam_load.c) */
	    erl_printf(to, "%d", (*ap / sizeof(Eterm*)) - 1);
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
	case 'o':
	    {
		FloatDef f;
#ifdef ARCH_64
		ap++;
		f.fdw = *ap++;
#else
		ap++;
		f.fw[0] = *ap++;
		f.fw[1] = *ap++;
#endif
		erl_printf(to, "%g", f.fd);
		size++;
	    }
	    break;
	case 'l':		/* fr(N) */
	    erl_printf(to, "fr(%d)", reg_index(ap[0]));
	    ap++;
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
