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
/* This File contains functions which are called if a user hits ^C */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "version.h"
#include "error.h"
#include "version.h"
#include "erl_db.h"
#include "bif.h"
#include "erl_version.h"
#include "hash.h"
#include "atom.h"
#include "beam_load.h"
#include "erl_instrument.h"

#ifdef _OSE_
#include "time.h"
#endif

/* Forward declarations -- should really appear somewhere else */
static void process_killer(void);
void do_break(void);
extern void erl_crash_dump(char *, int, char *, va_list);
#ifdef DEBUG
static void bin_check(void);
#endif

static void print_garb_info(Process* p, CIO to);
#ifdef OPPROF
static void dump_frequencies(void);
#endif

static void dump_attributes(CIO to, byte* ptr, int size);

extern char* erts_system_version[];

static void
port_info(CIO to)
{
    int i;
    for (i = 0; i < erts_max_ports; i++)
	print_port_info(i,to);
}

void
process_info(CIO to)
{
    int i;
    for (i = 0; i < erts_max_processes; i++) {
	if ((process_tab[i] != NULL) && (process_tab[i]->i != ENULL)) {
	   if (process_tab[i]->status != P_EXITING)
	       print_process_info(process_tab[i], to);
	}
    }

    port_info(to);
}

static void
process_killer(void)
{
    int i, j;
    Process* rp;

    erl_printf(COUT,"\n\nProcess Information\n\n");
    erl_printf(COUT,"--------------------------------------------------\n");
    for (i = erts_max_processes-1; i >= 0; i--) {
	if (((rp = process_tab[i]) != NULL) && rp->i != ENULL) {
	    int br;
	    print_process_info(rp,COUT);
	    erl_printf(COUT,"(k)ill (n)ext (r)eturn:\n");
	    while(1) {
		if ((j = sys_get_key(0)) <= 0)
		    halt_0(0);
		switch(j) {
		case 'k':
		    if (rp->status == P_WAITING) 
			schedule_exit(rp, am_killed);
		    else
			erl_printf(COUT,"Can only kill WAITING processes this way\n");

		case 'n': br = 1; break;
		case 'r': return;
		default: return;
		}
		if (br == 1) break;
	    }
	}
    }
}
			       
/* Display info about an individual Erlang process */
void
print_process_info(Process *p, CIO to)
{
    int garbing = 0;
    int running = 0;

    /* display the PID */
    erl_printf(to, "=proc:");
    display(p->id, to);
    erl_printf(to, "\n");

    /* Display the state */
    erl_printf(to, "State: ");
    switch (p->status) {
    case P_FREE:
	erl_printf(to,"Non Existing"); /* Should never happen */
	break;
    case P_RUNABLE:
	erl_printf(to,"Scheduled");
	break;
    case P_WAITING:
	erl_printf(to,"Waiting");
	break;
    case P_SUSPENDED:
	erl_printf(to,"Suspended");
	break;
    case P_RUNNING:
	erl_printf(to,"Running");
	running = 1;
	break;
    case P_EXITING:
	erl_printf(to,"Exiting");
	break;
    case P_GARBING:
	erl_printf(to,"Garbing");
	garbing = 1;
	running = 1;
	break;
    }
    erl_printf(to, "\n");

    /*
     * If the process is registered as a global process, display the
     * registered name
     */
    if (p->reg != NULL) {
	erl_printf(to,"Name: ");
	print_atom(atom_val(p->reg->name), to);
	erl_printf(to, "\n");
    }

    /*
     * Display the initial function name
     */
    erl_printf(to,"Spawned as: ");
    display(p->initial[INITIAL_MOD], to);
    erl_printf(to,":");
    display(p->initial[INITIAL_FUN], to);
    erl_printf(to,"/");
    erl_printf(to,"%d\n", (int)p->initial[INITIAL_ARI]);

    if (p->current != NULL) {
	if (running) {
	    erl_printf(to, "Last scheduled in for: ");
	} else {
	    erl_printf(to, "Current call: ");
	}
	display(p->current[0], to);
	erl_printf(to, ":");
	display(p->current[1], to);
	erl_printf(to,"/%d\n", p->current[2]);
    }

    erl_printf(to, "Spawned by: ");
    display(p->parent, to);
    erl_printf(to, "\n");

    erl_printf(to, "Started: %s", ctime((time_t*)&p->started));
    erl_printf(to, "Message queue length: %d\n", p->msg.len);

    /* display the message queue only if there is anything in it */
    if (!erts_writing_erl_crash_dump && p->msg.first != NULL && !garbing) {
	ErlMessage* mp = p->msg.first;

	erl_printf(to, "Message queue: [");
	mp = p->msg.first;
	while (mp != NULL) {
	    display(ERL_MESSAGE_TERM(mp), to);
	    if ((mp = mp->next) != NULL)
		erl_printf(to, ",");
	}
	erl_printf(to, "]\n");
    }

#ifndef SHARED_HEAP
    {
       long s = 0;
       int frags = 0;
       ErlHeapFragment *m = p->mbuf;
       while (m != NULL) {
	   frags++;
	   s += m->size;
	   m = m->next;
       }
       erl_printf(to, "Number of heap fragments: %d\n", frags);
    }
    erl_printf(to, "Heap fragment data: %u\n", (unsigned)(MBUF_SIZE(p)));
#endif

    if (p->ct != NULL) {
       int i, j;

       erl_printf(to, "Last calls:");
       for (i = 0; i < p->ct->n; i++) {
	     erl_printf(to, " ");
	     j = p->ct->cur - i - 1;
	     if (j < 0)
		j += p->ct->len;
	     if (p->ct->ct[j] == &exp_send)
		erl_printf(to, "send");
	     else if (p->ct->ct[j] == &exp_receive)
		erl_printf(to, "'receive'");
	     else if (p->ct->ct[j] == &exp_timeout)
		   erl_printf(to, "timeout");
	     else {
		display(p->ct->ct[j]->code[0], to);
		erl_printf(to, ":");
		display(p->ct->ct[j]->code[1], to);
		erl_printf(to, "/%d", p->ct->ct[j]->code[2]);
	     }
	  }
       erl_printf(to, "\n");
    }

    /* display the links only if there are any*/
    if (p->links != NULL) {
	ErlLink* lnk = p->links;
	erl_printf(to,"Link list: [");
	while(lnk != NULL) {
	    if (lnk->type == LNK_LINK1) {
	       erl_printf(to,"{");
	       if (lnk->item == p->id) {
		  ErlLink **lnkp;
		  erl_printf(to,"to,");

		  if (is_atom(lnk->data)) {
		      DistEntry *dep;

		      ASSERT(is_node_name_atom(lnk->data));
		      dep = erts_sysname_to_connected_dist_entry(lnk->data);
		      lnkp = (dep
			      ? find_link_by_ref(&dep->links, lnk->ref)
			      : NULL);
			  
		      erl_printf(to,"{");
		      if (lnkp)
			  display((*lnkp)->data, to);
		      else
			  erl_printf(to, "undefined"); /* An error */
		      erl_printf(to,",");
		      display(lnk->data, to);
		      erl_printf(to,"}");
		  }
		  else
		      display(lnk->data, to);
	       } else {
		  erl_printf(to,"from,");
		  display(lnk->item, to);
	       }
	       erl_printf(to,",");
	       display(lnk->ref, to);
	       erl_printf(to,"}");
	    } else {
	       display(lnk->item, to);
	    }
	    if ((lnk = lnk->next) != NULL)
	       erl_printf(to, ",");
	}
	erl_printf(to,"]\n");
    }


    if (!erts_writing_erl_crash_dump) {

	/* and the dictionary */
	if (p->dictionary != NULL && !garbing) {
	    erl_printf(to, "Dictionary: ");
	    erts_dictionary_dump(p->dictionary, to);
	    erl_printf(to, "\n");
	}
    
	/* as well as the debug dictionary */
	if (p->debug_dictionary != NULL && !garbing) {
	    erl_printf(to, "$Dictionary: ");
	    erts_dictionary_dump(p->debug_dictionary, to);
	    erl_printf(to, "\n");
	}
    }
    
    /* print the number of reductions etc */
    erl_printf(to, "Reductions: %u\n", (unsigned)p->reds);

#ifdef SHARED_HEAP
    erl_printf(to, "Stack: %u\n", (unsigned)(p->stack - p->send));
#else
    erl_printf(to, "Stack+heap: %u\n", (unsigned)p->heap_sz);
    erl_printf(to, "OldHeap: %u\n",
               (OLD_HEAP(p) == NULL) ? 0 :
               (unsigned)(OLD_HEND(p) - OLD_HEAP(p)) );
    erl_printf(to, "Heap unused: %u\n", (unsigned)(p->hend - p->htop));
    erl_printf(to, "OldHeap unused: %u\n",
	       (OLD_HEAP(p) == NULL) ? 0 : 
	       (unsigned)(OLD_HEND(p) - OLD_HEAP(p)) );
#endif

    if (garbing) {
	print_garb_info(p, to);
    }
    
    if (erts_writing_erl_crash_dump) {
	erts_program_counter_info(p, to);
    } else {
	erl_printf(to, "Stack dump:\n");
	erts_stack_dump(p, to);
    }
}

static void
print_garb_info(Process* p, CIO to)
{
    erl_printf(to, "New heap start: %X\n", p->heap);
    erl_printf(to, "New heap top: %X\n", p->htop);
    erl_printf(to, "Stack top: %X\n", p->stop);
    erl_printf(to, "Stack end: %X\n", p->hend);
    erl_printf(to, "Old heap start: %X\n", OLD_HEAP(p));
    erl_printf(to, "Old heap top: %X\n", OLD_HTOP(p));
    erl_printf(to, "Old heap end: %X\n", OLD_HEND(p));
}

void
info(CIO to)
{
    erts_memory(&to, NULL, THE_NON_VALUE);
    atom_info(to);
    module_info(to);
    export_info(to);
    register_info(to);
    erts_fun_info(to);
    erts_node_table_info(to);
    erts_dist_table_info(to);
    erts_allocated_areas(&to, NULL);
    erts_allocator_info(to);

}

void
loaded(CIO to)
{
    int i;
    int old = 0;
    int cur = 0;
    Eterm* code;

    /*
     * Calculate and print totals.
     */
    for (i = 0; i < module_code_size; i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    cur += module_code(i)->code_length;
	    if (module_code(i)->old_code_length != 0) {
		old += module_code(i)->old_code_length;
	    }
	}
    }
    erl_printf(to, "Current code: %d\n", cur);
    erl_printf(to, "Old code: %d\n", old);
    
    /*
     * Print one line per module.
     */
    for (i = 0; i < module_code_size; i++) {
	if (module_code(i) != NULL &&
	    ((module_code(i)->code_length != 0) ||
	     (module_code(i)->old_code_length != 0))) {
	    erl_printf(to, "=mod:");
	    print_atom(module_code(i)->module, to);
	    erl_printf(to,"\n");
	    erl_printf(to, "Current size: %d\n", module_code(i)->code_length);
	    code = module_code(i)->code;
	    if (code != NULL && code[MI_ATTR_PTR]) {
		erl_printf(to, "Current attributes: ");
		dump_attributes(to, (byte *) code[MI_ATTR_PTR], code[MI_ATTR_SIZE]);
	    }
	    if (code != NULL && code[MI_COMPILE_PTR]) {
		erl_printf(to, "Current compilation info: ");
		dump_attributes(to, (byte *) code[MI_COMPILE_PTR],
				code[MI_COMPILE_SIZE]);
	    }

	    if (module_code(i)->old_code_length != 0) {
		erl_printf(to, "Old size: %d\n", module_code(i)->old_code_length);
		code = module_code(i)->old_code;
		if (code[MI_ATTR_PTR]) {
		    erl_printf(to, "Old attributes: ");
		    dump_attributes(to, (byte *) code[MI_ATTR_PTR],
				    code[MI_ATTR_SIZE]);
		}
		if (code[MI_COMPILE_PTR]) {
		    erl_printf(to, "Old compilation info: ");
		    dump_attributes(to, (byte *) code[MI_COMPILE_PTR],
				    code[MI_COMPILE_SIZE]);
		}
	    }
	}
    }
}


static void
dump_attributes(CIO to, byte* ptr, int size)
{
    while (size-- > 0) {
	erl_printf(to, "%02X", *ptr++);
    }
    erl_printf(to, "\n");
}


void
do_break(void)
{
    int i;
    erl_printf(COUT, "\nBREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded\n");
    erl_printf(COUT, "       (v)ersion (k)ill (D)b-tables (d)istribution\n");
    while (1) {
	if ((i = sys_get_key(0)) <= 0)
	    halt_0(0);
	switch (i) {
	case 'q':
	case 'a': 
	case '*': /* 
		   * The asterisk is an read error on windows, 
		   * where sys_get_key isn't that great in console mode.
		   * The usual reason for a read error is Ctrl-C. Treat this as
		   * 'a' to avoid infinite loop.
		   */
	    halt_0(0);
	case 'A':		/* Halt generating crash dump */
	    erl_exit(1, "Crash dump requested by user");
	case 'c':
	    return;
	case 'p':
	    process_info(COUT);
	    return;
	case 'm':
	    return;
	case 'o':
	    port_info(COUT);
	    return;
	case 'i':
	    info(COUT);
	    return;
	case 'l':
	    loaded(COUT);
	    return;
	case 'v':
	    erl_printf(COUT, "Erlang (%s) emulator version "
		       ERLANG_VERSION "\n",
		       EMULATOR);
	    erl_printf(COUT, "Compiled on " ERLANG_COMPILE_DATE "\n");
	    return;
	case 'd':
	    distribution_info(COUT);
	    return;
	case 'D':
	    db_info(CERR, 1);
	    return; 
	case 'k':
	    process_killer();
	    return;
#ifdef OPPROF
	case 'X':
	    dump_frequencies();
	    return;
	case 'x':
	    {
		int i;
		for (i = 0; i <= HIGHEST_OP; i++) {
		    if (opc[i].name != NULL) {
			erl_printf(COUT, "%-16s %8d\n", opc[i].name, opc[i].count);
		    }
		}
	    }
	    return;
	case 'z':
	    {
		int i;
		for (i = 0; i <= HIGHEST_OP; i++)
		    opc[i].count = 0;
	    }
	    return;
#endif
#ifdef DEBUG
	case 't':
	    p_slpq();
	    return;
	case 'b':
	    bin_check();
	    return;
	case 'C':
	    abort();
#endif
	case '\n':
	    continue;
	default:
	    erl_printf(COUT, "Eh?\n\n");
	}
    }
}


#ifdef OPPROF
static void
dump_frequencies(void)
{
    int i;
    FILE* fp;
    time_t now;
    static char name[] = "op_freq.dump";

    fp = fopen(name, "w");
    if (fp == NULL) {
	fprintf(stderr, "Failed to open %s for writing\n", name);
	return;
    }

    time(&now);
    fprintf(fp, "# Generated %s\n", ctime(&now));

    for (i = 0; i <= HIGHEST_OP; i++) {
	if (opc[i].name != NULL) {
	    fprintf(fp, "%s %d\n", opc[i].name, opc[i].count);
	}
    }
    fclose(fp);
    erl_printf(COUT, "Frequencies dumped to %s\n", name);
}
#endif


#ifdef DEBUG

static void 
bin_check(void)
{
    Process  *rp;
    ProcBin *bp;
    int i, printed;

    for (i=0; i < erts_max_processes; i++) {
	if ((rp = process_tab[i]) == NULL)
	    continue;
#ifndef SHARED_HEAP
	if (!(bp = rp->off_heap.mso))
	    continue;
#endif
	printed = 0;
	while (bp) {
	    if (printed == 0) {
		erl_printf(COUT,"Process "); 
		display(rp->id, COUT);
		erl_printf(COUT," holding binary data \n");
		printed = 1;
	    }
	    erl_printf(COUT,"0x%08lx orig_size: %ld, norefs = %ld\n",
		       (unsigned long)bp->val, 
		       (long)bp->val->orig_size, 
		       (long)bp->val->refc);

	    bp = bp->next;
	}
	if (printed == 1)
	    erl_printf(COUT,"--------------------------------------\n");
    }
    /* db_bin_check() has to be rewritten for the AVL trees... */
    /*db_bin_check();*/ 
}

#endif

/* XXX THIS SHOULD BE IN SYSTEM !!!! */
void
erl_crash_dump(char *file, int line, char* fmt, va_list args)
{
    int fd;
    time_t now;
    char* dumpname;
    char buf[512];
#if !defined(VXWORKS) && !defined(__WIN32__)
    int i;
#endif

    if (erts_writing_erl_crash_dump)
	return;

    erts_writing_erl_crash_dump = 1; /* Allow us to pass certain places
					without locking... */

    dumpname = getenv("ERL_CRASH_DUMP");
    if (!dumpname) {
	dumpname = "erl_crash.dump";
    }
#ifndef VXWORKS
    close(3);			/* Make sure we have a free descriptor */
#ifndef __WIN32__
    for (i = 4; i < 1024; i++) {
	close(i);
    }
#endif
#endif
    fd = open(dumpname,O_WRONLY | O_CREAT | O_TRUNC,0640);
    if (fd < 0) 
	return; /* Can't create the crash dump, skip it */
    
    time(&now);
    erl_printf(fd, "=erl_crash_dump:0.1\n%s", ctime(&now));

    if (file != NULL)
       erl_printf(fd,"The error occurred in file %s, line %d\n", file, line);

    if (fmt != NULL && *fmt != '\0') {
	vsprintf(buf, fmt, args);
	erl_printf(fd, "Slogan: %s",buf);
    }
    erl_printf(fd, "System version: %s", erts_get_system_version(NULL));
    erl_printf(fd, "Compiled: " ERLANG_COMPILE_DATE "\n");
    erl_printf(fd, "Atoms: %d\n", atom_table.sz);
    info(fd); /* General system info */
    if (process_tab != NULL)  /* XXX true at init */
	process_info(fd); /* Info about each process and port */
    db_info(fd, 0);
    print_timer_info(fd);
    distribution_info(fd);
    erl_printf(fd, "=loaded_modules\n");
    loaded(fd);
    erts_dump_fun_entries(fd);
    erts_deep_process_dump(fd);
    erl_printf(fd,"=atoms\n");
    dump_atoms(fd);

    /* Keep the instrumentation data at the end of the dump */
    if (erts_instr_memory_map || erts_instr_stat) {
	erl_printf(fd, "=instr_data\n");

	if (erts_instr_stat) {
	    erl_printf(fd,"=memory_status\n");
	    erts_instr_dump_stat_to_fd(fd, 0);
	}
	if (erts_instr_memory_map) {
	    erl_printf(fd,"=memory_map\n");
	    erts_instr_dump_memory_map_to_fd(fd);
	}
    }

    erl_printf(fd,"=end\n");
    close(fd);
    sprintf(buf, "\r\nCrash dump was written to: %s\r\n", dumpname);
    write(2, buf, strlen(buf));
}
