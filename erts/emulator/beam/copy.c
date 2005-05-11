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
#include "big.h"
#include "erl_binary.h"


#ifdef HYBRID
Eterm *copy_src_stack;
Uint   copy_src_top;
Uint   copy_src_size;
Eterm *copy_dst_stack;
Uint   copy_dst_top;
Uint   copy_dst_size;

Eterm *copy_offset_stack;
Uint   copy_offset_top;
Uint   copy_offset_size;
#endif

void
init_copy(void)
{
#ifdef HYBRID
    copy_src_top = 0;
    copy_src_size = 512;
    copy_src_stack = (Eterm *)erts_alloc(ERTS_ALC_T_OBJECT_STACK,
                                         sizeof(Eterm) * copy_src_size);
    ERTS_PROC_MORE_MEM(sizeof(Eterm) * copy_src_size);
    copy_dst_top = 0;
    copy_dst_size = 512;
    copy_dst_stack = (Eterm *)erts_alloc(ERTS_ALC_T_OBJECT_STACK,
                                         sizeof(Eterm) * copy_dst_size);
    ERTS_PROC_MORE_MEM(sizeof(Eterm) * copy_dst_size);

    copy_offset_top = 0;
    copy_offset_size = 512;
    copy_offset_stack = (Eterm *)erts_alloc(ERTS_ALC_T_OBJECT_STACK,
                                            sizeof(Eterm) * copy_offset_size);
    ERTS_PROC_MORE_MEM(sizeof(Eterm) * copy_offset_size);
#endif
}

/*
 *  Copy object "obj" to process p.
 */
Eterm
copy_object(Eterm obj, Process* to)
{
#ifdef SHARED_HEAP
    return obj;
#else
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size);
    Eterm res;

    res = copy_struct(obj, size, &hp, &to->off_heap);
#ifdef DEBUG
    if (eq(obj, res) == 0) {
	erl_exit(1, "copy not equal to source\n");
    }
#endif
    return res;
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
		case FUN_SUBTAG:
		    {
			Eterm* bptr = fun_val(obj);
			ErlFunThing* funp = (ErlFunThing *) bptr;
			unsigned eterms = 1 /* creator */ + funp->num_free;
			unsigned sz = thing_arityval(hdr);

			sum += 1 /* header */ + sz + eterms;
			bptr += 1 /* header */ + sz;
			while (eterms-- > 1) {
			  obj = *bptr++;
			  if (!IS_CONST(obj)) {
			    ESTACK_PUSH(s, obj);
			  }
			}
			obj = *bptr;
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
 *  Copy a structure to a heap.
 */
Eterm
copy_struct(Eterm obj, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
{
#define in_area(ptr,start,nbytes) \
    ((unsigned long)((char*)(ptr) - (char*)(start)) < (nbytes))
    char* hstart;
    Uint hsize;
    Eterm* htop;
    Eterm* hbot;
    Eterm* hp;
    Eterm* objp;
    Eterm* tp;
    Eterm  res;
    Eterm  elem;
    Eterm* tailp;
    Eterm* argp;
    Eterm* const_tuple;
    Eterm hdr;
    int i;

    if (IS_CONST(obj))
	return obj;

    hp = htop = *hpp;
    hbot   = htop + sz;
    hstart = (char *)htop;
    hsize = (char*) hbot - hstart;
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
	    if (in_area(objp,hstart,hsize)) {
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
	    if (in_area(boxed_val(obj),hstart,hsize)) {
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

		    i =  thing_arityval(hdr) + 2 + funp->num_free;
		    tp = htop;
		    while (i--)  {
			*htop++ = *objp++;
		    }
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
		    funp = (ErlFunThing *) tp;
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		    funp->fe->refc++;
#endif
#endif
		    *argp = make_fun(tp);
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		  ExternalThing *etp = (ExternalThing *) htop;

		  i =  thing_arityval(hdr) + 1;
		  tp = htop;

		  while (i--)  {
		    *htop++ = *objp++;
		  }

		  etp->next = off_heap->externals;
		  off_heap->externals = etp;
		  etp->node->refc++;

		  *argp = make_external(tp);
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
    *hpp = (Eterm *) (hstart+hsize);
    return res;
#undef in_area
}

#ifdef HYBRID
#ifdef BM_MESSAGE_SIZES
#define BM_ADD(var,val) (var) += (val);
#else
#define BM_ADD(var,val)
#endif

#ifdef INCREMENTAL_GC
#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    BM_ADD(words_copied,n);                                             \
    BM_SWAP_TIMER(copy,system);                                         \
    (hp) = IncAlloc((p),n,NULL,0);                                      \
    BM_SWAP_TIMER(system,copy);                                         \
    if ((hp) == global_heap && obj != orig) {                           \
        global_htop = global_heap;                                      \
        copy_src_top = 0;                                               \
        copy_dst_top = 0;                                               \
        copy_offset_top = 0;                                            \
        goto copy_start;                                                \
    }                                                                   \
} while(0)

#else /* no INCREMELNTAL_GC */

#define GlobalAlloc(p, need, hp)                                        \
do {                                                                    \
    Uint n = (need);                                                    \
    total_need += n;                                                    \
    BM_ADD(words_copied,n);                                             \
    if (global_hend - n < global_htop) {                                \
        BM_SWAP_TIMER(copy,system);                                     \
        erts_global_garbage_collect((p),total_need,NULL,0);             \
        BM_SWAP_TIMER(system,copy);                                     \
        copy_src_top = 0;                                               \
        copy_dst_top = 0;                                               \
        copy_offset_top = 0;                                            \
        goto copy_start;                                                \
    }                                                                   \
    (hp) = global_htop;                                                 \
    global_htop += n;                                                   \
} while(0)
#endif /* INCREMENTAL_GC */


Eterm copy_struct_lazy(Process *from, Eterm orig, Uint offs)
{
    Eterm obj = orig;
    Eterm dest;
    int total_need = 0;

#ifdef DEBUG_COPY
#ifndef BM_COUNTERS
    static int messages_sent = 0;
    messages_sent++;
#endif
    printf("COPY START %d; %s is sending a message\n\r", messages_sent,
           (is_internal_pid(from->id) ? print_pid(from) : "NO PID" ));
    display(orig,COUT);
    printf("\n\r");
#endif

 copy_start:

    ROOT_PUSH(src,orig);
    ROOT_PUSH(dst,(Eterm)&dest);
    ROOT_PUSH(offset,offs);

    while (copy_src_top > 0) {

        obj = ROOT_POP(src);

        if (NO_COPY(obj)) {
            ROOT_UPDATE(dst,ROOT_POP(offset),obj);
            ROOT_POP(dst);
            continue;
        }

        switch (primary_tag(obj)) {
        case TAG_PRIMARY_LIST: {
            Eterm *hp;
            Eterm *objp = list_val(obj);

            GlobalAlloc(from,2,hp);
            ROOT_UPDATE(dst,ROOT_POP(offset),make_list(hp));
            ROOT_POP(dst);

            if (NO_COPY(*objp))
                hp[0] = *objp;
            else {
                ROOT_PUSH(src,*objp);
                ROOT_PUSH(dst,make_list(hp));
                ROOT_PUSH(offset,0);
            }

            objp++;
            if (NO_COPY(*objp))
                hp[1] = *objp;
            else {
                ROOT_PUSH(src,*objp);
                ROOT_PUSH(dst,make_list(hp));
                ROOT_PUSH(offset,1);
            }
            continue;
        }

        case TAG_PRIMARY_BOXED: {
            Eterm *objp = boxed_val(obj);
            Eterm hdr = *objp;

            switch (hdr & _TAG_HEADER_MASK) {
            case ARITYVAL_SUBTAG: {
                Uint ari = arityval(hdr);
                Uint i;
                Eterm *hp;
                GlobalAlloc(from,ari + 1,hp);
                ROOT_UPDATE(dst,ROOT_POP(offset),make_tuple(hp));
                ROOT_POP(dst);
                *hp = *objp++;
                for (i = 1; i <= ari; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp))
                            hp[i] = *objp++;
                        else {
                            ROOT_PUSH(src,*objp++);
                            ROOT_PUSH(dst,make_tuple(hp));
                            ROOT_PUSH(offset,i);
                        }
                        break;
                    default:
                        hp[i] = *objp++;
                    }
                }
                continue;
            }

            case REFC_BINARY_SUBTAG: {
                ProcBin *pb;
                Uint i = thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                ROOT_UPDATE(dst,ROOT_POP(offset),make_binary(hp));
                ROOT_POP(dst);
                pb = (ProcBin*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
                pb->val->refc++;
                pb->next = erts_global_offheap.mso;
                erts_global_offheap.mso = pb;
                erts_global_offheap.overhead += pb->size /
                    BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
                continue;
            }

            case FUN_SUBTAG: {
                ErlFunThing *funp = (ErlFunThing*) objp;
                Uint i = thing_arityval(*objp) + 2;
                Uint j = i + funp->num_free;
                Uint k = i;
                Eterm *hp, *hp_start;
                GlobalAlloc(from,j,hp);
                hp_start = hp;
                ROOT_UPDATE(dst,ROOT_POP(offset),make_fun(hp));
                ROOT_POP(dst);
                funp = (ErlFunThing*) hp;
                while (i--) {
                    *hp++ = *objp++;
                }
#ifndef HYBRID // FIND ME!
                funp->next = erts_global_offheap.funs;
                erts_global_offheap.funs = funp;
                funp->fe->refc++;
#endif
                for (i = k; i < j; i++) {
                    switch (primary_tag(*objp)) {
                    case TAG_PRIMARY_LIST:
                    case TAG_PRIMARY_BOXED:
                        if (NO_COPY(*objp))
                            *hp++ = *objp++;
                        else {
                            ROOT_PUSH(src,*objp++);
                            ROOT_PUSH(dst,make_fun(hp_start));
                            ROOT_PUSH(offset,i);
                            hp++;
                        }
                        break;
                    default:
                        *hp++ = *objp++;
                    }
                }
                continue;
            }

            case EXTERNAL_PID_SUBTAG:
            case EXTERNAL_PORT_SUBTAG:
            case EXTERNAL_REF_SUBTAG: {
                ExternalThing *etp;
                Uint i =  thing_arityval(*objp) + 1;
                Eterm *hp;
                GlobalAlloc(from,i,hp);
                ROOT_UPDATE(dst,ROOT_POP(offset),make_external(hp));
                ROOT_POP(dst);
                etp = (ExternalThing*) hp;
                while (i--)  {
                    *hp++ = *objp++;
                }

                etp->next = erts_global_offheap.externals;
                erts_global_offheap.externals = etp;
                etp->node->refc++;
                continue;
            }

            case SUB_BINARY_SUBTAG: {
                ErlSubBin *sb = (ErlSubBin *) objp;
                Eterm real_bin = sb->orig;
                Uint sub_offset = sb->offs;
                size_t size = sb->size;

                objp = binary_val(real_bin);
                if (thing_subtag(*objp) == HEAP_BINARY_SUBTAG) {
                    ErlHeapBin *from_bin;
                    ErlHeapBin *to_bin;
                    Uint i = heap_bin_size(size);
                    Eterm *hp;
                    GlobalAlloc(from,i,hp);
                    ROOT_UPDATE(dst,ROOT_POP(offset),make_binary(hp));
                    ROOT_POP(dst);
                    from_bin = (ErlHeapBin *) objp;
                    to_bin = (ErlHeapBin *) hp;
                    to_bin->thing_word = header_heap_bin(size);
                    to_bin->size = size;
                    sys_memcpy(to_bin->data, ((byte *)from_bin->data) +
                               sub_offset, size);
                } else {
                    ProcBin *from_bin;
                    ProcBin *to_bin;
                    Eterm *hp;

                    ASSERT(thing_subtag(*objp) == REFC_BINARY_SUBTAG);
                    GlobalAlloc(from,PROC_BIN_SIZE,hp);
                    ROOT_UPDATE(dst,ROOT_POP(offset),make_binary(hp));
                    ROOT_POP(dst);
                    from_bin = (ProcBin *) objp;
                    to_bin = (ProcBin *) hp;
                    to_bin->thing_word = HEADER_PROC_BIN;
                    to_bin->size = size;
                    to_bin->val = from_bin->val;
                    to_bin->val->refc++;
                    to_bin->bytes = from_bin->bytes + sub_offset;
                    to_bin->next = erts_global_offheap.mso;
                    erts_global_offheap.mso = to_bin;
                    erts_global_offheap.overhead += to_bin->size /
                        BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
                }
                continue;
            }

            default: {
                Uint size = thing_arityval(hdr) + 1;
                Eterm *hp;
                GlobalAlloc(from,size,hp);
                ROOT_UPDATE(dst,ROOT_POP(offset),make_boxed(hp));
                ROOT_POP(dst);
                while (size--) {
                    *hp++ = *objp++;
                }
                continue;
            }
            }
            continue;
        }

        case TAG_PRIMARY_HEADER:
        ASSERT((obj & _TAG_HEADER_MASK) == ARITYVAL_SUBTAG);
        {
            Eterm *objp = &obj;
            Uint ari = arityval(obj);
            Uint i;
            Eterm *hp;
            GlobalAlloc(from,ari + 1,hp);
            ROOT_UPDATE(dst,ROOT_POP(offset),make_tuple(hp));
            ROOT_POP(dst);
            *hp = *objp++;
            for (i = 1; i <= ari; i++) {
                switch (primary_tag(*objp)) {
                case TAG_PRIMARY_LIST:
                case TAG_PRIMARY_BOXED:
                    if (NO_COPY(*objp))
                        hp[i] = *objp++;
                    else {
                        ROOT_PUSH(src,*objp++);
                        ROOT_PUSH(dst,make_tuple(hp));
                        ROOT_PUSH(offset,i);
                    }
                    break;
                default:
                    hp[i] = *objp++;
                }
            }
            continue;
        }

        default:
            erl_exit(1, "%s, line %d: Internal error in copy_struct_lazy: 0x%08x\n",
                     __FILE__, __LINE__,obj);
        }
    }

#ifdef DEBUG_COPY
    printf("After: ");
    display(dest,COUT);
    printf("\n\r");

    //print_tagged_memory(global_heap,global_htop);
#endif

    ASSERT(copy_src_top == 0);
    ASSERT(copy_dst_top == 0);
    ASSERT(copy_offset_top == 0);

    return dest;
}

#undef NO_COPY
#endif /* HYBRID */

/*
 * Copy a term that is guaranteed to be contained in a single
 * heap block. The heap block is copied word by word, and any
 * pointers are offsetted to point correctly in the new location.
 *
 * Typically used to copy a term from an ets table.
 */
Eterm
copy_shallow(Eterm* ptr, Uint sz, Eterm** hpp, ErlOffHeap* off_heap)
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    Sint offs = hp - tp;

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
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
		    ErlFunThing* funp = (ErlFunThing *) (hp-1);
#endif
#endif
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
#ifndef HYBRID /* FIND ME! */
#ifndef SHARED_HEAP
		    funp->next = off_heap->funs;
		    off_heap->funs = funp;
		    funp->fe->refc++;
#endif
#endif
		}
		break;
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		{
		    ExternalThing* etp = (ExternalThing *) (hp-1);
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		    etp->next = off_heap->externals;
		    off_heap->externals = etp;
		    etp->node->refc++;
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
