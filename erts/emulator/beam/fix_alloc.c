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

#ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE

#if ERTS_ALC_MTA_FIXED_SIZE
#error Implement thread safety for fix_alloc() ...
#endif

typedef union {double d; long l;} align_t;

typedef struct fix_alloc_block {
    struct fix_alloc_block *next;
    align_t mem[1];
} FixAllocBlock;

typedef struct fix_alloc {
    Uint item_size;
    void *freelist;
    Uint no_free;
    Uint no_blocks;
    FixAllocBlock *blocks;
} FixAlloc;

static void *(*core_alloc)(Uint); 
static Uint xblk_sz;

static FixAlloc **fa;
#define FA_SZ (1 + ERTS_ALC_N_MAX_A_FIXED_SIZE - ERTS_ALC_N_MIN_A_FIXED_SIZE)

#define FIX_IX(N) ((N) - ERTS_ALC_N_MIN_A_FIXED_SIZE)

#define FIX_POOL_SZ(I_SZ) \
  ((I_SZ)*NOPERBLOCK + sizeof(FixAllocBlock) - sizeof(align_t))

#ifdef DEBUG
static int first_time;
#endif

void erts_init_fix_alloc(Uint extra_block_size,
			 void *(*alloc)(Uint))
{
    int i;

    xblk_sz = extra_block_size;
    core_alloc = alloc;

    fa = (FixAlloc **) (*core_alloc)(FA_SZ * sizeof(FixAlloc *));
    if (!fa)
	erts_alloc_enomem(ERTS_ALC_T_UNDEF, FA_SZ * sizeof(FixAlloc *));

    for (i = 0; i < FA_SZ; i++)
	fa[i] = NULL;
#ifdef DEBUG
    first_time = 1;
#endif
}

Uint
erts_get_fix_size(ErtsAlcType_t type)
{
    Uint i = FIX_IX(ERTS_ALC_T2N(type));
    return i < FA_SZ && fa[i] ? fa[i]->item_size : 0;
}

void
erts_set_fix_size(ErtsAlcType_t type, Uint size)
{
    Uint sz;
    Uint i;
    FixAlloc *fs;
    ErtsAlcType_t t_no = ERTS_ALC_T2N(type);
    sz = xblk_sz + size;

#ifdef DEBUG
    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= t_no);
    ASSERT(t_no <= ERTS_ALC_N_MAX_A_FIXED_SIZE);
#endif

    while (sz % sizeof(align_t) != 0)     /* Alignment */
	sz++;

    i = FIX_IX(t_no);
    fs = (FixAlloc *) (*core_alloc)(sizeof(FixAlloc));
    if (!fs)
	erts_alloc_n_enomem(t_no, sizeof(FixAlloc));
    
    fs->item_size = sz;
    fs->no_blocks = 0;
    fs->no_free = 0;
    fs->blocks = NULL;
    fs->freelist = NULL;
    if (fa[i])
	erl_exit(-1, "Attempt to overwrite existing fix size (%d)", i);
    fa[i] = fs;
}

void
erts_fix_info(ErtsAlcType_t type, ErtsFixInfo *efip)
{
    Uint i;
    FixAlloc *f;
#ifdef DEBUG
    FixAllocBlock *b;
    void *fp;
#endif
    Uint real_item_size;
    ErtsAlcType_t t_no = ERTS_ALC_T2N(type);

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= t_no);
    ASSERT(t_no <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    i = FIX_IX(t_no);
    f = fa[i];

    efip->total	= sizeof(FixAlloc *);
    efip->used	= 0;
    if (!f)
	return;

    real_item_size = f->item_size - xblk_sz;

    efip->total += sizeof(FixAlloc);
    efip->total += f->no_blocks*FIX_POOL_SZ(real_item_size);
    efip->used = efip->total - f->no_free*real_item_size;

#ifdef DEBUG
    ASSERT(efip->total >= efip->used);
    for(i = 0, b = f->blocks; b; i++, b = b->next);
    ASSERT(f->no_blocks == i);
    for (i = 0, fp = f->freelist; fp; i++, fp = *((void **) fp));
    ASSERT(f->no_free == i);
#endif

}

void
erts_fix_free(ErtsAlcType_t t_no, void *extra, void* ptr)
{
    Uint i;
    FixAlloc *f;

    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= t_no);
    ASSERT(t_no <= ERTS_ALC_N_MAX_A_FIXED_SIZE);

    i = FIX_IX(t_no);
    f = fa[i];

    *((void **) ptr) = f->freelist;
    f->freelist = ptr;
    f->no_free++;
}


void *erts_fix_realloc(ErtsAlcType_t t_no, void *extra, void* ptr, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOTSUP, ERTS_ALC_O_REALLOC, t_no);
    return NULL;
}

void *erts_fix_alloc(ErtsAlcType_t t_no, void *extra, Uint size)
{
    void *ret;
    int i;
    FixAlloc *f;

#ifdef DEBUG
    ASSERT(ERTS_ALC_N_MIN_A_FIXED_SIZE <= t_no);
    ASSERT(t_no <= ERTS_ALC_N_MAX_A_FIXED_SIZE);
    if (first_time) { /* Check that all sizes have been initialized */
	int i;
	for (i = 0; i < FA_SZ; i++)
	    ASSERT(fa[i]);
	first_time = 0;
    }
#endif


    i = FIX_IX(t_no);
    f = fa[i];

    ASSERT(f);
    ASSERT(f->item_size >= size);

    if (f->freelist == NULL) {  /* Gotta alloc some more mem */
	char *ptr;
	FixAllocBlock *bl;
	Uint n;

	bl = (*core_alloc)(FIX_POOL_SZ(f->item_size));
	if (!bl)
	    return NULL;

	bl->next = f->blocks;  /* link in first */
	f->blocks = bl;

	n = NOPERBLOCK;
	ptr = (char *) &f->blocks->mem[0];
	while(n--) {
	    *((void **) ptr) = f->freelist;
	    f->freelist = (void *) ptr;
	    ptr += f->item_size;
	}
	ASSERT(f->no_free == 0);
	f->no_free = NOPERBLOCK;
	f->no_blocks++;
    }

    ret = f->freelist;
    f->freelist = *((void **) f->freelist);
    ASSERT(f->no_free > 0);
    f->no_free--;

    return ret;
}

#endif /* #ifdef ERTS_ALC_N_MIN_A_FIXED_SIZE */
