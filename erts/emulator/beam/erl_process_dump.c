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
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_binary.h"

#define WORD_FMT "%X"
#define ADDR_FMT "%X"

#define OUR_NIL	_make_header(0,_TAG_HEADER_FLOAT)

static void dump_process_info(Process *p, CIO to);
static void dump_element(Eterm x, CIO fd);
static void dump_element_nl(Eterm x, CIO fd);
static int stack_element_dump(Process* p, Eterm* sp, int yreg, CIO fd);
static void print_function_from_pc(Eterm* x, CIO fd);
static void heap_dump(Eterm x, CIO fd);
static void dump_binaries(Binary* root, CIO fd);
static void dump_externally(Eterm term, CIO fd);

static Binary* all_binaries;

extern Eterm beam_apply[];
extern Eterm beam_exit[];


void
erts_deep_process_dump(CIO to)
{
    int i;

    all_binaries = NULL;
    
    for (i = 0; i < erts_max_processes; i++) {
	if ((process_tab[i] != NULL) && (process_tab[i]->i != ENULL)) {
	   if (process_tab[i]->status != P_EXITING) {
	       Process* p = process_tab[i];

	       if (p->status != P_GARBING) {
		   dump_process_info(p, to);
	       }
	   }
       }
    }

    dump_binaries(all_binaries, to);
}

static void
dump_process_info(Process *p, CIO to)
{
    Eterm* sp;
    ErlMessage* mp;
    int yreg = -1;

    if (p->msg.first) {
	erl_printf(to, "=proc_messages:");
	display(p->id, to);
	erl_printf(to, "\n");
	for (mp = p->msg.first; mp != NULL; mp = mp->next) {
	    Eterm mesg = ERL_MESSAGE_TERM(mp);
	    dump_element(mesg, to);
	    mesg = ERL_MESSAGE_TOKEN(mp);
	    erl_printf(to, ":");
	    dump_element(mesg, to);
	    erl_printf(to, "\n");
	}
    }

    if (p->dictionary) {
	erl_printf(to, "=proc_dictionary:");
	display(p->id, to);
	erl_printf(to, "\n");
	erts_deep_dictionary_dump(p->dictionary, dump_element_nl, to);
    }

    if (p->debug_dictionary) {
	erl_printf(to, "=debug_proc_dictionary:");
	display(p->id, to);
	erl_printf(to, "\n");
	erts_deep_dictionary_dump(p->debug_dictionary, dump_element_nl, to);
    }

    erl_printf(to, "=proc_stack:");
    display(p->id, to);
    erl_printf(to, "\n");

    for (sp = p->stop; sp < STACK_START(p); sp++) {
        yreg = stack_element_dump(p, sp, yreg, to);
    }

    erl_printf(to, "=proc_heap:");
    display(p->id, to);
    erl_printf(to, "\n");

    for (sp = p->stop; sp < STACK_START(p); sp++) {
	Eterm term = *sp;

	if (!is_catch(term) && !is_CP(term)) {
	    heap_dump(term, to);
	}
    }
    
    for (mp = p->msg.first; mp != NULL; mp = mp->next) {
	Eterm mesg = ERL_MESSAGE_TERM(mp);
	heap_dump(mesg, to);
	mesg = ERL_MESSAGE_TOKEN(mp);
	heap_dump(mesg, to);
    }

    if (p->dictionary) {
	erts_deep_dictionary_dump(p->dictionary, heap_dump, to);
    }

    if (p->debug_dictionary) {
	erts_deep_dictionary_dump(p->debug_dictionary, heap_dump, to);
    }
}

static void
dump_element(Eterm x, CIO fd)
{
    if (is_list(x)) {
	sys_printf(fd, "H" WORD_FMT, list_val(x));
    } else if (is_boxed(x)) {
	sys_printf(fd, "H" WORD_FMT, boxed_val(x));
    } else if (is_immed(x)) {
	if (is_atom(x)) {
	    char* s = atom_tab(atom_val(x))->name;
	    int len = atom_tab(atom_val(x))->len;
	    int i;

	    sys_printf(fd, "A%X:", atom_tab(atom_val(x))->len);
	    for (i = 0; i < len; i++) {
		erl_putc(*s++, fd);
	    }
	} else if (is_small(x)) {
	    erl_putc('I', fd);
	    display(x, fd);
	} else if (is_pid(x)) {
	    erl_putc('P', fd);
	    display(x, fd);
	} else if (is_port(x)) {
	    erl_printf(fd, "p<%lu.%lu>",
		       (unsigned long) port_channel_no(x),
		       (unsigned long) port_number(x));
	} else if (is_nil(x)) {
	    erl_putc('N', fd);
	}
    }
}

static void
dump_element_nl(Eterm x, CIO fd)
{
    dump_element(x, fd);
    erl_putc('\n', fd);
}


static int
stack_element_dump(Process* p, Eterm* sp, int yreg, CIO fd)
{
    Eterm x = *sp;

    if (yreg < 0 || is_CP(x)) {
        erl_printf(fd, "%p:", sp);
    } else {
        sys_printf(fd, "y%d:", yreg);
        yreg++;
    }

    if (is_CP(x)) {
        sys_printf(fd, "SReturn addr 0x%X (", (Eterm *) x);
        print_function_from_pc(cp_val(x), fd);
        sys_printf(fd, ")\n");
        yreg = 0;
    } else if is_catch(x) {
        sys_printf(fd, "SCatch 0x%X (", catch_pc(x));
        print_function_from_pc(catch_pc(x), fd);
        sys_printf(fd, ")\n");
    } else {
	dump_element(x, fd);
	erl_putc('\n', fd);
    }
    return yreg;
}

static void
print_function_from_pc(Eterm* x, CIO fd)
{
    Eterm* addr = find_function_from_pc(x);
    if (addr == NULL) {
        if (x == beam_exit) {
            sys_printf(fd, "<terminate process>");
        } else if (x == beam_apply+1) {
            sys_printf(fd, "<terminate process normally>");
        } else {
            sys_printf(fd, "unknown function");
        }
    } else {
        display(addr[0], fd);
        sys_printf(fd, ":");
        display(addr[1], fd);
        sys_printf(fd, "/%d", addr[2]);
        sys_printf(fd, " + %d", ((x-addr)-2) * sizeof(Eterm));
    }
}

static void
heap_dump(Eterm x, CIO fd)
{
    Eterm* ptr;
    Eterm last = OUR_NIL;
    Eterm* next = &last;

    if (is_immed(x) || is_CP(x)) {
	return;
    }

 again:
    if (x == OUR_NIL) {	/* We are done. */
	return;
    } if (is_CP(x)) {
	next = (Eterm *) x;
    } else if (is_list(x)) {
	ptr = list_val(x);
	if (ptr[0] != OUR_NIL) {
	    sys_printf(fd, ADDR_FMT ":l", ptr);
	    dump_element(ptr[0], fd);
	    erl_putc('|', fd);
	    dump_element(ptr[1], fd);
	    erl_putc('\n', fd);
	    if (is_immed(ptr[1])) {
		ptr[1] = make_small(0);
	    }
	    x = ptr[0];
	    ptr[0] = (Eterm) next;
	    next = ptr + 1;
	    goto again;
	}
    } else if (is_boxed(x)) {
	Eterm hdr;
	
	ptr = boxed_val(x);
	hdr = *ptr;
	if (hdr != OUR_NIL) {	/* If not visited */
	    sys_printf(fd, ADDR_FMT ":", ptr);
	    if (is_arity_value(hdr)) {
		Uint i;
		Uint arity = arityval(hdr);

		sys_printf(fd, "t" WORD_FMT ":", arity);
		for (i = 1; i <= arity; i++) {
		    dump_element(ptr[i], fd);
		    if (is_immed(ptr[i])) {
			ptr[i] = make_small(0);
		    }
		    if (i < arity) {
			erl_putc(',', fd);
		    }
		}
		erl_putc('\n', fd);
		if (arity == 0) {
		    ptr[0] = OUR_NIL;
		} else {
		    x = ptr[arity];
		    ptr[0] = (Eterm) next;
		    next = ptr + arity - 1;
		    goto again;
		}
	    } else if (hdr == HEADER_FLONUM) {
		FloatDef f;
		char sbuf[31];
		int i;

		GET_DOUBLE_DATA((ptr+1), f);
		i = sys_double_to_chars(f.fd, (char*) sbuf);
		sys_memset(sbuf+i, 0, 31-i);
		sys_printf(fd, "F%X:%s\n", i, sbuf);
		*ptr = OUR_NIL;
	    } else if (_is_bignum_header(hdr)) {
		erl_putc('B', fd);
		display(x, fd);
		erl_putc('\n', fd);
		*ptr = OUR_NIL;
	    } else if (is_binary_header(hdr)) {
		Uint tag = thing_subtag(hdr);
		Uint size = binary_size(x);
		Uint i;

		if (tag == HEAP_BINARY_SUBTAG) {
		    byte* p;

		    sys_printf(fd, "Yh%X:", size);
		    GET_BINARY_BYTES(x, p);
		    for (i = 0; i < size; i++) {
			sys_printf(fd, "%02X", p[i]);
		    }
		} else if (tag == REFC_BINARY_SUBTAG) {
		    ProcBin* pb = (ProcBin *) binary_val(x);
		    Binary* val = pb->val;

		    if (val->refc != 0) {
			val->flags = (Uint) all_binaries;
			val->refc = 0;
			all_binaries = val;
		    }
		    sys_printf(fd, "Yc%X:%X:%X", val,
			       pb->bytes - (byte *)val->orig_bytes,
			       size);
		} else if (tag == SUB_BINARY_SUBTAG) {
		    ErlSubBin* Sb = (ErlSubBin *) binary_val(x);
		    Eterm* real_bin = binary_val(Sb->orig);
		    void* val;

		    if (thing_subtag(*real_bin) == REFC_BINARY_SUBTAG) {
			ProcBin* pb = (ProcBin *) real_bin;
			val = pb->val;
		    } else {	/* Heap binary */
			val = real_bin;
		    }
		    sys_printf(fd, "Ys%X:%X:%X", val, Sb->offs, size);
		}
		erl_putc('\n', fd);
		*ptr = OUR_NIL;
	    } else if (is_external_pid_header(hdr)) {
		erl_putc('P', fd);
		display(x, fd);
		erl_putc('\n', fd);
		*ptr = OUR_NIL;
	    } else if (is_external_port_header(hdr)) {
		erl_printf(fd, "p<%lu.%lu>\n",
			   (unsigned long) port_channel_no(x),
			   (unsigned long) port_number(x));
		*ptr = OUR_NIL;
	    } else {
		/*
		 * All other we dump in the external term format.
		 */
		dump_externally(x, fd);
		erl_putc('\n', fd);
		*ptr = OUR_NIL;
	    }
	}
    }

    x = *next;
    *next = OUR_NIL;
    next--;
    goto again;
}

static void
dump_binaries(Binary* current, CIO fd)
{
    while (current) {
	long i;
	long size = current->orig_size;
	byte* bytes = current->orig_bytes;

	sys_printf(fd, "=binary:%X\n", current);
	sys_printf(fd, "%X:", size);
	for (i = 0; i < size; i++) {
	    sys_printf(fd, "%02X", bytes[i]);
	}
	erl_putc('\n', fd);
	current = (Binary *) current->flags;
    }
}

static void
dump_externally(Eterm term, CIO fd)
{
    byte sbuf[1024];
    byte* s; 
    byte* p;

    s = p = sbuf;
    erts_to_external_format(0, term, &p);
    sys_printf(fd, "E%X:", p-s);
    while (s < p) {
	sys_printf(fd, "%02X", *s++);
    }
}
