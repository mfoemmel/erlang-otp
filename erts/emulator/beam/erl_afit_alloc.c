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
 * Description:	A fast allocator intended for temporary allocation.
 *              When allocating, only the first block in the free list
 *              is inspected, if this block doesn't fit a new carrier
 *              is created. NOTE: this allocator can behave really bad
 *              if misused.
 *              
 *              This module is a callback-module for erl_alloc_util.c
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "global.h"
#define GET_ERL_AF_ALLOC_IMPL
#include "erl_afit_alloc.h"


#define MIN_MBC_SZ		(16*1024)
#define MIN_MBC_FIRST_FREE_SZ	(4*1024)

/* Prototypes of callback functions */
static Block_t *	get_free_block		(Allctr_t *, Uint);
static void		link_free_block		(Allctr_t *, Block_t *);
static void		unlink_free_block	(Allctr_t *, Block_t *);


static Eterm		info_options		(Allctr_t *, char *, CIO *,
						 Uint **, Uint *);

static int atoms_initialized = 0;

void
erts_afalc_init(void)
{
    atoms_initialized = 0;
}

Allctr_t *
erts_afalc_start(AFAllctr_t *afallctr,
		 AFAllctrInit_t *afinit,
		 AllctrInit_t *init)
{
    AFAllctr_t nulled_state = {{0}};
    /* {{0}} is used instead of {0}, in order to avoid (an incorrect) gcc
       warning. gcc warns if {0} is used as initializer of a struct when
       the first member is a struct (not if, for example, the third member
       is a struct). */
    Allctr_t *allctr = (Allctr_t *) afallctr;

    sys_memcpy((void *) afallctr, (void *) &nulled_state, sizeof(AFAllctr_t));

    allctr->mbc_header_size		= sizeof(Carrier_t);
    allctr->min_mbc_size		= MIN_MBC_SZ;
    allctr->min_mbc_first_free_size	= MIN_MBC_FIRST_FREE_SZ;
    allctr->min_block_size		= sizeof(AFFreeBlock_t);
    allctr->vsn_str			= ERTS_ALC_AF_ALLOC_VSN_STR;

    /* Callback functions */
    allctr->get_free_block		= get_free_block;
    allctr->link_free_block		= link_free_block;
    allctr->unlink_free_block		= unlink_free_block;
    allctr->info_options		= info_options;

    allctr->get_next_mbc_size		= NULL;
    allctr->creating_mbc		= NULL;
    allctr->destroying_mbc		= NULL;

#ifdef ERTS_ALLOC_UTIL_HARD_DEBUG
    allctr->check_block			= NULL;
    allctr->check_mbc			= NULL;
#endif

    if (!erts_alcu_start(allctr, init))
	return NULL;

    return allctr;
}

static Block_t *
get_free_block(Allctr_t *allctr, Uint size)
{
    AFFreeBlock_t *res;
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    if (afallctr->free_list && BLK_SZ(afallctr->free_list) >= size) {
	res = afallctr->free_list;	
	afallctr->free_list = res->next;
	if (res->next)
	    res->next->prev = NULL;
    }
    else
	res = NULL;
    return (Block_t *) res;
}

static void
link_free_block(Allctr_t *allctr, Block_t *block)
{
    AFFreeBlock_t *blk = (AFFreeBlock_t *) block;
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    if (afallctr->free_list && BLK_SZ(afallctr->free_list) > BLK_SZ(blk)) {
	blk->next = afallctr->free_list->next;
	blk->prev = afallctr->free_list;
	afallctr->free_list->next = blk;
    }
    else {
	blk->next = afallctr->free_list;
	blk->prev = NULL;
	afallctr->free_list = blk;
    }

    if (blk->next)
	blk->next->prev = blk;
}

static void
unlink_free_block(Allctr_t *allctr, Block_t *block)
{
    AFFreeBlock_t *blk = (AFFreeBlock_t *) block;
    AFAllctr_t *afallctr = (AFAllctr_t *) allctr;

    if (blk->prev)
	blk->prev->next = blk->next;
    else
	afallctr->free_list = blk->next;
    if (blk->next)
	blk->next->prev = blk->prev;
}


static struct {
    Eterm as;
    Eterm af;
#ifdef DEBUG
    Eterm end_of_atoms;
#endif
} am;

static void ERTS_INLINE atom_init(Eterm *atom, char *name)
{
    *atom = am_atom_put(name, strlen(name));
}
#define AM_INIT(AM) atom_init(&am.AM, #AM)

static void
init_atoms(void)
{
#ifdef DEBUG
    Eterm *atom;
    for (atom = (Eterm *) &am; atom <= &am.end_of_atoms; atom++) {
	*atom = THE_NON_VALUE;
    }
#endif
    AM_INIT(as);
    AM_INIT(af);

#ifdef DEBUG
    for (atom = (Eterm *) &am; atom < &am.end_of_atoms; atom++) {
	ASSERT(*atom != THE_NON_VALUE);
    }
#endif

    atoms_initialized = 1;
}


#define bld_uint	erts_bld_uint
#define bld_cons	erts_bld_cons
#define bld_tuple	erts_bld_tuple

static ERTS_INLINE void
add_2tup(Uint **hpp, Uint *szp, Eterm *lp, Eterm el1, Eterm el2)
{
    *lp = bld_cons(hpp, szp, bld_tuple(hpp, szp, 2, el1, el2), *lp);
}

static Eterm
info_options(Allctr_t *allctr,
	     char *prefix,
	     CIO *ciop,
	     Uint **hpp,
	     Uint *szp)
{
    Eterm res = THE_NON_VALUE;

    if (ciop) {
	erl_printf(*ciop, "%sas: af\n", prefix);
    }

    if (hpp || szp) {
	
	if (!atoms_initialized)
	    init_atoms();

	res = NIL;
	add_2tup(hpp, szp, &res, am.as, am.af);
    }

    return res;
}



/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 * NOTE:  erts_afalc_test() is only supposed to be used for testing.         *
 *                                                                           *
 * Keep alloc_SUITE_data/allocator_test.h updated if changes are made        *
 * to erts_afalc_test()                                                      *
\*                                                                           */

unsigned long
erts_afalc_test(unsigned long op, unsigned long a1, unsigned long a2)
{
    switch (op) {
    default:	ASSERT(0); return ~((unsigned long) 0);
    }
}
