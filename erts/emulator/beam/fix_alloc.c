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
    uint32 mem[1];
} FixAllocBlock;

typedef struct fix_alloc {
    int item_size;
    uint32 *freelist;
    FixAllocBlock *blocks;
} FixAlloc;


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

void init_alloc()
{
    init_fix_alloc(20);
    process_desc = new_fix_size(sizeof(Process));
    table_desc = new_fix_size(sizeof(DbTable));
    atom_desc = new_fix_size(sizeof(Atom));
    export_desc = new_fix_size(sizeof(Export));
    module_desc = new_fix_size(sizeof(Module));
    preg_desc = new_fix_size(sizeof(RegProc));
    link_desc = new_fix_size(sizeof(ErlLink));
    plist_desc = new_fix_size(sizeof(ProcessList));
    mesg_desc  = new_fix_size(sizeof(ErlMessage));
}


int init_fix_alloc(max)
int max;
{
    max_sizes = max;
    if ((fa = (FixAlloc*) sys_alloc_from(31,max * sizeof(FixAlloc))) == NULL)
	return(0);
    sys_memzero(fa, max * sizeof(FixAlloc));
    return(1);
}

/* Calculate number of bytes allocated by 'desc' */
int fix_info(desc)
int desc;
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


/* Returns a small integer which must be used in all subsequent */
/* calls to fix_alloc() and fix_free() of this size             */

int new_fix_size(size)
int size;
{
    int i;

    while (size % sizeof(char*) != 0)     /* Alignment */
	size++;
    for (i=0; i<max_sizes; i++) {
	if (fa[i].item_size == 0 && size >= (sizeof(uint32*))) {
	    fa[i].item_size = size;
	    fa[i].blocks = NULL;
	    fa[i].freelist = NULL;
	    return(i);
	}
    }
    erl_exit(1,"Fix allocator out of bounds \n");
    return(-1); /* Pedantic (lint does not know about erl_exit) */
}

#if !defined(NO_FIX_ALLOC)
/* Release all memory allocated to desc back to std malloc()
** Redefined (tony): release memory only NOT the item type
*/

void fix_release(desc)
int desc;
{
    FixAllocBlock *bl;
    FixAlloc *f = &fa[desc];

    bl = f->blocks;
    while (bl) {
	FixAllocBlock* bl_next = bl->next;
	sys_free(bl);
	bl = bl_next;
    }
    f->freelist = NULL;
    f->blocks = NULL;

#ifdef PURIFY
    purify_map_pool(desc, free);
#endif    
}
#endif

void fix_free(desc, ptr)
int desc; uint32 *ptr;
{
#if defined(NO_FIX_ALLOC)
    sys_free(ptr);
#elif defined(PURIFY)
#ifdef __WIN32__
    purify_clear_pool_id(desc, ptr);
#endif
    free(ptr);
#else
    FixAlloc *f = &fa[desc];

#ifdef DEBUG
    sys_memset(ptr, 0xff, f->item_size);    /* Clobber */
#endif
    *ptr = (uint32) f->freelist;
    f->freelist = ptr;
#endif
}

uint32 *fix_alloc(desc)
int desc;
{
#ifdef INSTRUMENT
    extern int alloc_who;
#endif
    uint32 *ret;
    FixAlloc *f = &fa[desc];

#if defined(NO_FIX_ALLOC)
    ret = sys_alloc(f->item_size);
#elif defined(PURIFY)
    ret = (uint32* ) malloc(f->item_size);
    purify_set_pool_id(ret, desc);
#else
    if (f->freelist == NULL) {  /* Gotta alloc some more mem */
	char *ptr;
	FixAllocBlock *bl;
	int n = f->item_size*(NOPERBLOCK) + sizeof(FixAllocBlock) -
	    sizeof(uint32);

	if ((bl = (FixAllocBlock*) sys_alloc_from(32,n)) == NULL)
	    return(NULL);

	bl->next = f->blocks;  /* link in first */
	f->blocks = bl;

	n = NOPERBLOCK;
	ptr = (char*) &f->blocks->mem[0];
	while(n--) {
	    *((uint32*)ptr) = (uint32)f->freelist;
	    f->freelist = (uint32*) ptr;
	    ptr += f->item_size;
	}
    }
#ifdef INSTRUMENT
    alloc_who = -1;
#endif
    ret = f->freelist;
    f->freelist = (uint32*) *f->freelist;
#endif
    return (ret);
}
