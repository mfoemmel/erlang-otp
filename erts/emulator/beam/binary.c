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

/*
 * Create a brand new binary from scratch.
 */

Eterm
new_binary(Process *p, byte *buf, int len)
{
    ProcBin* pb;
    Binary* bptr;

    /*
     * XXX Later, we should put small binaries directly on the heap.
     */

    /*
     * Allocate the binary struct itself.
     */
    bptr = (Binary *) safe_alloc_from(61, len+sizeof(Binary));
    bptr->flags = 0;
    bptr->orig_size = len;
    bptr->refc = 1;
    if (buf != NULL) {
	sys_memcpy(bptr->orig_bytes, buf, len);
    }

    /*
     * Now allocate the ProcBin on the heap.
     */
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = make_thing(PROC_BIN_SIZE-1, REFC_BINARY_SUBTAG);
    pb->size = len;
    pb->next = p->off_heap.mso;
    p->off_heap.mso = pb;
    pb->val = bptr;
    pb->bytes = bptr->orig_bytes;

    /*
     * Miscellanous updates. Return the tagged binary.
     */
    tot_bin_allocated += len;
    p->off_heap.overhead += pb->size / BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
    return make_binary(pb);
}

BIF_RETTYPE binary_to_list_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int i;
    Eterm previous;
    ProcBin* pb;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }

    pb = (ProcBin*) ptr_val(BIF_ARG_1);
    switch (thing_subtag(pb->thing_word)) {
    case REFC_BINARY_SUBTAG:
	i = pb->size;
	hp = HAlloc(BIF_P, i * 2);
	previous = NIL;
	while (i--) {
	    previous = CONS(hp, make_small(pb->bytes[i]), previous);
	    hp += 2;
	}
	BIF_RET(previous);
    case HEAP_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case SUB_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case FUN_SUBTAG:
	goto error;
    default:
	erl_exit(1, "%s, line %d: Bad subtag: %d",
		 __FILE__, __LINE__, thing_subtag(pb->thing_word));
    }

 error:
    BIF_ERROR(BIF_P, BADARG);
}

BIF_RETTYPE binary_to_list_3(BIF_ALIST_3)
BIF_ADECL_3
{
    int i;
    Eterm previous;
    ProcBin *pb;
    int start;
    int stop;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1)) {
	goto error;
    }
    if (is_not_small(BIF_ARG_3) || is_not_small(BIF_ARG_2)) {
	goto error;
    }
    pb = (ProcBin*) ptr_val(BIF_ARG_1);
    switch (thing_subtag(pb->thing_word)) {
    case REFC_BINARY_SUBTAG:
	i = pb->size;
	start = signed_val(BIF_ARG_2);
	stop = signed_val(BIF_ARG_3);
	if (start < 1 || start > i || stop < 1 || stop > i || stop < start) {
	    BIF_ERROR(BIF_P, BADARG);
	}

	hp = HAlloc(BIF_P, (stop-start+1) * 2);
	previous = NIL;
	for (i = stop - 1; i >= start -1 ; i--) {
	    previous = CONS(hp, make_small(pb->bytes[i]), previous);
	    hp += 2;
	}
	BIF_RET(previous);
    case HEAP_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case SUB_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case FUN_SUBTAG:
	goto error;
    default:
	erl_exit(1, "%s, line %d: Bad subtag: %d",
		 __FILE__, __LINE__, thing_subtag(pb->thing_word));
    }

 error:
    BIF_ERROR(BIF_P, BADARG);
}


/* Turn a possibly deep list of ints (and binaries) into */
/* One large binary object                               */

BIF_RETTYPE list_to_binary_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm bin;
    int j, i;
    ProcBin *bp;

    if (is_nil(BIF_ARG_1)) {
	BIF_RET(new_binary(BIF_P,(byte*)"",0));
    }
    if (is_not_list(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((i = io_list_len(BIF_ARG_1)) < 0) {
	goto error;
    }
    j = 0;
    bin = new_binary(BIF_P, (byte *)NULL, i);
    bp = (ProcBin *) ptr_val(bin);
    if (io_list_to_buf(BIF_ARG_1, (char*) bp->bytes, &j, i+1) != 0) {
	goto error;
    }
    BIF_RET(bin);
}


BIF_RETTYPE concat_binary_1(BIF_ALIST_1)
BIF_ADECL_1
{
    return list_to_binary_1(BIF_ALIST_1);  /* Not meaningful any longer */
}


BIF_RETTYPE split_binary_2(BIF_ALIST_2)
BIF_ADECL_2
{
    int pos;
    ProcBin* oldpb;
    ProcBin* newpb1;
    ProcBin* newpb2;
    Eterm* hp;

    if (is_not_binary(BIF_ARG_1) || is_not_small(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((pos = signed_val(BIF_ARG_2)) < 0) {
	goto error;
    }
    oldpb = (ProcBin*) ptr_val(BIF_ARG_1);
    if (thing_subtag(oldpb->thing_word) == FUN_SUBTAG) {
	goto error;
    }
    if (oldpb->size < pos) {
	goto error;
    }
    ASSERT(thing_subtag(oldpb->thing_word) == REFC_BINARY_SUBTAG);
    hp = HAlloc(BIF_P, 2*PROC_BIN_SIZE+3);

    newpb1 = (ProcBin *) hp;
    newpb1->thing_word = make_thing(PROC_BIN_SIZE-1, REFC_BINARY_SUBTAG);
    newpb1->size = pos;
    newpb1->next = BIF_P->off_heap.mso;
    newpb1->val = oldpb->val;
    newpb1->bytes = oldpb->bytes;
    hp += PROC_BIN_SIZE;

    newpb2 = (ProcBin *) hp;
    newpb2->thing_word = make_thing(PROC_BIN_SIZE-1, REFC_BINARY_SUBTAG);
    newpb2->size = oldpb->size - pos;
    newpb2->next = newpb1;
    newpb2->val = oldpb->val;
    newpb2->bytes = oldpb->bytes + pos;
    hp += PROC_BIN_SIZE;

    BIF_P->off_heap.mso = newpb2;
    oldpb->val->refc += 2;

    return TUPLE2(hp, make_binary(newpb1), make_binary(newpb2));
}

void
erts_cleanup_mso(ProcBin* pb)
{
    while (pb != NULL) {
	ProcBin* next = pb->next;
	ASSERT(pb->val->refc > 0);
	pb->val->refc--;
	if (pb->val->refc == 0) {
	    if (pb->val->flags & BIN_FLAG_MATCH_PROG) {
		erts_match_set_free(pb->val);
	    } else {
		tot_bin_allocated -= pb->val->orig_size;
		sys_free((char*)pb->val);
	    }
	}
	pb = next;
    }
}
