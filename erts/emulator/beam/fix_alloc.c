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
/* General purpose Memory allocator for fixed block size objects         */
/* This allocater is at least an order of magnitude faster than malloc() */

#define NOPERBLOCK 20
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_db.h"

typedef struct fix_alloc_block {
    struct fix_alloc_block *next;
    Eterm mem[1];
} FixAllocBlock;

typedef struct fix_alloc {
    int item_size;
    Eterm *freelist;
    FixAllocBlock *blocks;
} FixAlloc;

static int init_fix_alloc(int);

static FixAlloc *fa;
static int max_sizes;

int process_desc;
int table_desc;
int export_desc;
int atom_desc;
int module_desc;
int preg_desc;
int link_desc;
int plist_desc;
int mesg_desc;
int erts_fun_desc;

void
init_alloc(void)
{
    init_fix_alloc(13);
    process_desc = new_fix_size(sizeof(Process));
    table_desc = new_fix_size(sizeof(DbTable));
    atom_desc = new_fix_size(sizeof(Atom));
    export_desc = new_fix_size(sizeof(Export));
    module_desc = new_fix_size(sizeof(Module));

    preg_desc = new_fix_size(sizeof(RegProc));
    link_desc = new_fix_size(sizeof(ErlLink));
    plist_desc = new_fix_size(sizeof(ProcessList));
    mesg_desc  = new_fix_size(sizeof(ErlMessage));
    erts_fun_desc = new_fix_size(sizeof(ErlFunEntry));
}


static int
init_fix_alloc(int max)
{
    max_sizes = max;
    fa = (FixAlloc*) erts_definite_alloc(max * sizeof(FixAlloc));
    if (!fa) {
	fa = (FixAlloc*) sys_alloc_from(31,max * sizeof(FixAlloc));
	if (!fa)
	    return 0;
    }
    sys_memzero(fa, max * sizeof(FixAlloc));
    return 1;
}

/* Calculate number of bytes allocated by 'desc' */
int
fix_info(int desc)
{
    FixAlloc* f = &fa[desc];
    FixAllocBlock* b = f->blocks;
    int n = 0;

    while (b) {
	n++;
	b = b->next;
    }
    return n*NOPERBLOCK*f->item_size;
}

/* Calculate number of used bytes allocated by 'desc' */
int
fix_used(int desc)
{
    FixAlloc* f = &fa[desc];
    FixAllocBlock* b = f->blocks;
    Eterm* fp;
    int n = 0;
    int allocated;
    int used;

    while (b) {
        n++;
        b = b->next;
    }
    allocated = n*NOPERBLOCK*f->item_size;

    n = 0;
    fp = f->freelist;
    while(fp) {
      n++;
      fp = (Eterm *) *fp;
    }
    used = allocated - n*f->item_size;
    ASSERT(used >= 0);
    return used;
}


/* Returns a small integer which must be used in all subsequent */
/* calls to fix_alloc() and fix_free() of this size             */

int
new_fix_size(int size)
{
    int i;

    while (size % sizeof(char*) != 0)     /* Alignment */
	size++;
    for (i=0; i<max_sizes; i++) {
	if (fa[i].item_size == 0 && size >= (sizeof(Eterm*))) {
	    fa[i].item_size = size;
	    fa[i].blocks = NULL;
	    fa[i].freelist = NULL;
	    return(i);
	}
    }
    erl_exit(1,"Fix allocator out of bounds \n");
    return(-1); /* Pedantic (lint does not know about erl_exit) */
}

void
fix_free(int desc, Eterm* ptr)
{
#if defined(NO_FIX_ALLOC)
    sys_free(ptr);
#elif defined(PURIFY)
    free(ptr);
#else
    FixAlloc *f = &fa[desc];

#ifdef DEBUG
    sys_memset(ptr, 0xff, f->item_size);    /* Clobber */
#endif
    *ptr = (Eterm) f->freelist;
    f->freelist = ptr;
#endif
}



#ifdef INSTRUMENT
Eterm *fix_alloc(int desc)
{
  return fix_alloc_from(-1, desc);
}

Eterm *fix_alloc_from(int from, int desc)
#else /* #ifdef INSTRUMENT */
#define from (-1)
Eterm *fix_alloc(int desc)
#endif /* #ifdef INSTRUMENT */
{
    Eterm* ret;
    FixAlloc *f = &fa[desc];

#if defined(NO_FIX_ALLOC)
    ret = sys_alloc_from(from == -1 ? 32 : from, f->item_size);
#elif defined(PURIFY)
    ret = (Eterm* ) malloc(f->item_size);
#else
    if (f->freelist == NULL) {  /* Gotta alloc some more mem */
	char *ptr;
	FixAllocBlock *bl;
	Uint n = f->item_size*(NOPERBLOCK)+sizeof(FixAllocBlock)-sizeof(Eterm);

	if((bl = (FixAllocBlock*) erts_definite_alloc(n)) == NULL) {
	    if ((bl = (FixAllocBlock*) sys_alloc_from(from == -1 ? 32 : from,
						      n)) == NULL)
		return(NULL);
	}

	bl->next = f->blocks;  /* link in first */
	f->blocks = bl;

	n = NOPERBLOCK;
	ptr = (char*) &f->blocks->mem[0];
	while(n--) {
	    *((Eterm*)ptr) = (Eterm)f->freelist;
	    f->freelist = (Eterm*) ptr;
	    ptr += f->item_size;
	}
    }

    ret = f->freelist;
    f->freelist = (Eterm*) *f->freelist;
#endif
    return ret;
}
