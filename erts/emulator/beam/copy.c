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
** Copy struct & size struct
**
*/

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "erl_binary.h"

void
init_copy(void)
{
}

/*
 *  Copy object "obj" to process p.
 */
void
copy_object(Eterm obj, Process* to, Uint extra, Eterm* res, Process* from)
{
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size+extra);

    *res = copy_struct(obj, size, &hp, &to->off_heap);
#ifdef DEBUG
    if (eq(obj, *res) == 0) {
	erl_exit(1, "copy not equal to source\n");
    }
#endif
}

/*
 * Return the "flat" size of the object.
 */

Uint
size_object(Eterm obj)
{
    Uint sum = 0;
    Eterm* ptr;
    int arity;

    DECLARE_ESTACK(s);
    for (;;) {
	switch (primary_tag(obj)) {
	case TAG_PRIMARY_LIST:
	    sum += 2;
	    ptr = list_val(obj);
	    obj = *ptr++;
	    if (!IS_CONST(obj)) {
		ESTACK_PUSH(s, obj);
	    }
	    obj = *ptr;
	    break;
	case TAG_PRIMARY_BOXED:
	    {
		Eterm hdr = *boxed_val(obj);
		ASSERT(is_header(hdr));
		switch (hdr & _TAG_HEADER_MASK) {
		case ARITYVAL_SUBTAG:
		    ptr = tuple_val(obj);
		    arity = header_arity(hdr);
		    sum += arity + 1;
		    if (arity == 0) { /* Empty tuple -- unusual. */
			goto size_common;
		    }
		    while (arity-- > 1) {
			obj = *++ptr;
			if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			}
		    }
		    obj = *++ptr;
		    break;
		case VECTOR_SUBTAG:
		    /*
		     * XXX This does not handle circular references properly.
		     */
		    ptr = vector_val(obj);
		    arity = vector_arity(hdr);
		    sum += arity + 1;
		    if (!IS_CONST(ptr[3])) {
			ESTACK_PUSH(s, ptr[3]);
		    }
		    obj = ptr[4];
		    break;
		case FUN_SUBTAG:
		    {
			Eterm* bptr = fun_val(obj);
			ErlFunThing* funp = (ErlFunThing *) bptr;
			unsigned num_free = funp->num_free;
			unsigned sz = thing_arityval(hdr);

			sum += sz + 1 + num_free;
			if (num_free == 0) {
			    goto size_common;
			} else {
			    bptr += sz + 1;
			    while (num_free-- > 1) {
				obj = *bptr++;
				if (!IS_CONST(obj)) {
				    ESTACK_PUSH(s, obj);
				}
			    }
			    obj = *bptr;
			}
			break;
		    }
		case SUB_BINARY_SUBTAG:
		    {
			Eterm real_bin;
			Uint offset; /* Not used. */
			Eterm hdr;
			GET_REAL_BIN(obj, real_bin, offset);
			hdr = *binary_val(real_bin);

			if (thing_subtag(hdr) == REFC_BINARY_SUBTAG) {
			    sum += PROC_BIN_SIZE;
			} else {
			    sum += heap_bin_size(binary_size(obj));
			}
			goto size_common;
		    }
		    break;
		default:
		    sum += thing_arityval(hdr) + 1;
		    /* Fall through */
		size_common:
		    if (ESTACK_ISEMPTY(s)) {
			DESTROY_ESTACK(s);
			return sum;
		    }
		    obj = ESTACK_POP(s);
		    break;
		}
	    }
	    break;
	case TAG_PRIMARY_IMMED1:
	    if (ESTACK_ISEMPTY(s)) {
		DESTROY_ESTACK(s);
		return sum;
	    }
	    obj = ESTACK_POP(s);
	    break;
	default:
	    erl_exit(1, "size_object: bad tag for %#x\n", obj);
	}
    }
}

/*
**  Copy a structure to the heap of p
*/
Eterm
copy_struct(Eterm obj, uint32 sz, uint32** hpp, ErlOffHeap* off_heap)
{
#define COPIED(x) (ptr_val(x) >= hstart && ptr_val(x) < hend)
    uint32* hstart;
    uint32* hend;
    uint32* htop;
    uint32* hbot;
    uint32* hp;
    uint32* objp;
    uint32* tp;
    uint32  res;
    uint32  elem;
    uint32* tailp;
    uint32* argp;
    uint32* const_tuple;
    Eterm hdr;
    int i;

    if (IS_CONST(obj))
	return obj;

    hstart = *hpp;
    hend   = hstart + sz;
    htop   = hstart;
    hbot   = hend;
    hp     = hstart;
    const_tuple = 0;

    /* Copy the object onto the heap */
    switch (primary_tag(obj)) {
    case TAG_PRIMARY_LIST: argp = &res; goto L_copy_list;
    case TAG_PRIMARY_BOXED: argp = &res; goto L_copy_boxed;
    default:
	erl_exit(1, "%s, line %d: Internal error in copy_struct: 0x%08x\n",
		 __FILE__, __LINE__,obj);
    }

 L_copy:
    while (hp != htop) {
	obj = *hp;

	switch (primary_tag(obj)) {
	case TAG_PRIMARY_IMMED1:
	    hp++;
	    break;
	case TAG_PRIMARY_LIST:
	    objp = list_val(obj);
	    if (objp >= hstart && objp < hend) {
		hp++;
		break;
	    }
	    argp = hp++;
	    /* Fall through */

	L_copy_list:
	    tailp = argp;
	    while (is_list(obj)) {
		objp = list_val(obj);
		tp = tailp;
		elem = *objp;
		if (IS_CONST(elem)) {
		    *(hbot-2) = elem;
		    tailp = hbot-1;
		    hbot -= 2;
		}
		else {
		    *htop = elem;
		    tailp = htop+1;
		    htop += 2;
		}
		*tp = make_list(tailp - 1);
		obj = *(objp+1);
	    }
	    switch (primary_tag(obj)) {
	    case TAG_PRIMARY_IMMED1: *tailp = obj; goto L_copy;
	    case TAG_PRIMARY_BOXED: argp = tailp; goto L_copy_boxed;
	    default:
		erl_exit(1, "%s, line %d: Internal error in copy_struct: 0x%08x\n",
			 __FILE__, __LINE__,obj);
	    }
	    
	case TAG_PRIMARY_BOXED:
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++;

	L_copy_boxed:
	    objp = boxed_val(obj);
	    hdr = *objp;
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    int const_flag = 1; /* assume constant tuple */
		    i = arityval(hdr);
		    *argp = make_tuple(htop);
		    tp = htop;	/* tp is pointer to new arity value */
		    *htop++ = *objp++; /* copy arity value */
		    while (i--) {
			elem = *objp++;
			if (!IS_CONST(elem)) {
			    const_flag = 0;
			}
			*htop++ = elem;
		    }
		    if (const_flag) {
			const_tuple = tp; /* this is the latest const_tuple */
		    }
		}
		break;
	    case VECTOR_SUBTAG:
		{
		    /*
		     * XXX This does not handle circular references properly.
		     */
		    i = vector_arity(hdr);
		    *argp = make_vector(htop);
		    *htop++ = *objp++; /* copy arity value */
		    while (i--) {
			*htop++ = *objp++;
		    }
		}
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb;

		    i = thing_arityval(*objp) + 1;
		    hbot -= i;
		    tp = hbot;
		    while (i--)  {
			*tp++ = *objp++;
		    }
		    *argp = make_binary(hbot);
		    pb = (ProcBin*) hbot;
		    pb->val->refc++;
		    pb->next = off_heap->mso;
		    off_heap->mso = pb;
		    off_heap->overhead += pb->size /
			BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
		}
		break;
	    case SUB_BINARY_SUBTAG:
		{
		    ErlSubBin* sb = (ErlSubBin *) objp;
		    Eterm real_bin = sb->orig;
		    Uint offset = sb->offs;
		    size_t size = sb->size;

		    objp = binary_val(real_bin);
		    if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
			ErlHeapBin* from = (ErlHeapBin *) objp;
			ErlHeapBin* to;
			i = heap_bin_size(size);
			hbot -= i;
			to = (ErlHeapBin *) hbot;
			to->thing_word = header_heap_bin(size);
			to->size = size;
			sys_memcpy(to->data, ((byte *)from->data)+offset, size);
		    } else {
			ProcBin* from = (ProcBin *) objp;
			ProcBin* to;
			
			ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
			hbot -= PROC_BIN_SIZE;
			to = (ProcBin *) hbot;
			to->thing_word = HEADER_PROC_BIN;
			to->size = size;
			to->val = from->val;
			to->val->refc++;
			to->bytes = from->bytes + offset;
			to->next = off_heap->mso;
			off_heap->mso = to;
			off_heap->overhead += to->size /
			    BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
		    }
		    *argp = make_binary(hbot);
		    break;
		}
		break;
	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) objp;

		    i =  thing_arityval(hdr) + 1 + funp->num_free;
		    tp = htop;
		    while (i--)  {
			*htop++ = *objp++;
		    }
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		    *argp = make_fun(tp);
		}
		break;
	    default:
		i = thing_arityval(hdr)+1;
		hbot -= i;
		tp = hbot;
		*argp = make_boxed(hbot);
		while (i--) {
		    *tp++ = *objp++;
		}
	    }
	    break;
	case TAG_PRIMARY_HEADER:
	    if (header_is_thing(obj) || hp == const_tuple) {
		hp += header_arity(obj) + 1;
	    } else {
		hp++;
	    }
	    break;
	}
    }

    if (htop > hbot) {
	erl_exit(1, "Internal error: copy_struct: htop, hbot overrun\n");
    }
    ASSERT(htop == hbot);
    *hpp = hend;
    return res;
#undef COPIED
}

/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 */
Eterm
copy_shallow(Eterm* ptr, uint32 sz, Eterm** hpp, ErlOffHeap* off_heap)
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    sint32 offs = hp - tp;

    while (sz--) {
	Eterm val = *tp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    *hp++ = offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
	    case VECTOR_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
		{
		    ProcBin* pb = (ProcBin *) (hp-1);
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    pb->val->refc++;
		    pb->next = off_heap->mso;
		    off_heap->mso = pb;
		}
		break;
	    case FUN_SUBTAG:
		{
		    ErlFunThing* funp = (ErlFunThing *) (hp-1);
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		}
		break;
	    default:
		{
		    int tari = header_arity(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		}
		break;
	    }
	    break;
	}
    }
    *hpp = hp;
    return make_tuple(ptr + offs); 
}
