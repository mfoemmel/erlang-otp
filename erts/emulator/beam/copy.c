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

static void cp_binary(Eterm obj, Eterm** hpp, Eterm** hbotp,
		      Eterm* res, ErlOffHeap* off_heap);

void
init_copy(void)
{
}

/*
 *  Copy object "obj" to process p.
 */
int
copy_object(Eterm obj, Process* to, uint32 extra, Eterm* res, Process* from)
{
    Uint size = size_object(obj);
    Eterm* hp = HAlloc(to, size+extra);

    *res = copy_struct(obj, size, &hp, &to->off_heap);
#ifdef DEBUG
    if (eq(obj, *res) == 0) {
	erl_exit(1, "copy not equal to source\n");
    }
#endif
    return 0;
}

/*
**  Copy n objects objs
**
*/
int copy_objects(obj, n, to, extra, res, from)
uint32* obj; int n; Process* to; uint32 extra;
uint32* res; Process* from;
{
    int i;
    int size;
    uint32* hp;

    /* Use "res" to store temporary sizes */
    size = 0;
    for (i = 0; i < n; i++) {
	res[i] = size_object(obj[i]);
	size += res[i];
    }

    hp = HAlloc(to, size+extra);
    for (i = 0; i < n; i++)
	res[i] = copy_struct(obj[i], res[i], &hp, &to->off_heap);
    return 0;
}

/*
 * Return the "flat" size of the object.
 */

Eterm
size_object(Eterm obj)
{
    ErlStack s;
    uint32 sum = 0;
    Eterm* ptr;
    int arity;

    INIT_ESTACK(s);
    for (;;) {
	switch (tag_val_def(obj)) {
	case REFER_DEF:
	    sum += refer_arity(obj)+1;
	    goto size_common;
	case BIG_DEF:
	    sum += big_arity(obj)+1;
	    /* Fall through */
	case SMALL_DEF:
	case ATOM_DEF:
	case PORT_DEF:
	case PID_DEF:
	size_common:
	    if ((obj = ESTACK_POP(s)) == 0) {
		DESTROY_ESTACK(s);
		return sum;
	    }
	    break;
	case BINARY_DEF:
	    {
		Eterm* bptr = ptr_val(obj);
		Uint sz = thing_arityval(*bptr);

		ASSERT(is_thing(*bptr));
		switch (thing_subtag(*bptr)) {
		case REFC_BINARY_SUBTAG:
		    ASSERT(sz == PROC_BIN_SIZE-1);
		    sum += sz + 1;
		    goto size_common;
		case FUN_SUBTAG:
		    {
			ErlFunThing* funp = (ErlFunThing *) bptr;
			unsigned num_free = funp->num_free;
			
			ASSERT(sz + 2 == ERL_FUN_SIZE);
			sum += sz + 1 + num_free;
			if (num_free == 0) {
			    goto size_common;
			} else {
			    bptr += sz + 1;
			    while (num_free-- > 1) {
				obj = *bptr++;
				if (!IS_ONE_CELL(obj)) {
				    ESTACK_PUSH(s, obj);
				}
			    }
			    obj = *bptr;
			}
			break;
		    }
		default:
		    erl_exit(1, "%s, line %d: Bad subtag: %d\n",
			     __FILE__, __LINE__, thing_subtag(*bptr));
		}
	    }
	    break;
	case FLOAT_DEF:
	    sum += 3;
	    if ((obj = ESTACK_POP(s)) == 0) {
		DESTROY_ESTACK(s);
		return sum;
	    }
	    break;
	case TUPLE_DEF:
	    ptr = ptr_val(obj);
	    arity = arityval(*ptr);
	    sum += arity + 1;
	    if (arity == 0) {
		if ((obj = ESTACK_POP(s)) == 0) {
		    DESTROY_ESTACK(s);
		    return sum;
		}
	    } else {
		while (arity-- > 1) {
		    obj = *++ptr;
		    if (!IS_ONE_CELL(obj)) {
			ESTACK_PUSH(s, obj);
		    }
		}
		obj = *++ptr;
	    }
	    break;
	case LIST_DEF:
	    sum += 2;
	    ptr = ptr_val(obj);
	    obj = *ptr++;
	    if (!IS_ONE_CELL(obj)) {
		ESTACK_PUSH(s, obj);
	    }
	    obj = *ptr;
	    break;
	case ARITYVAL_DEF:
	case THING_DEF:
	case CP0:
	case CP4:
	case CP8:
	case CP12:
	    erl_exit(1, "size_object: bad tag %d for %d\n", tag_val_def(obj), obj);
	    break;
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
    int const_flag;
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
    switch (tag_val_def(obj)) {
    case FLOAT_DEF:  argp = &res; goto L_copy_float;
    case BIG_DEF:    argp = &res; goto L_copy_big;
    case REFER_DEF:  argp = &res; goto L_copy_refer;
    case TUPLE_DEF:  argp = &res; goto L_copy_tuple;
    case LIST_DEF:   argp = &res; goto L_copy_list;
    case BINARY_DEF: argp = &res; goto L_copy_binary;
    default:
	erl_exit(1, "%s, line %d: Internal error in copy_struct: 0x%08x\n",
		 __FILE__, __LINE__,obj);
    }

 L_copy:
    while (hp != htop) {
	obj = *hp;
	switch(tag_val_def(obj)) {
	case FLOAT_DEF:  
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_float;
	case REFER_DEF:
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_refer;
	case BIG_DEF:
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_big;
	case TUPLE_DEF: 
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_tuple;
	case LIST_DEF:
	    objp = ptr_val(obj);
	    if (objp >= hstart && objp < hend) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_list;
	case BINARY_DEF:
	    if (COPIED(obj)) {
		hp++;
		break;
	    }
	    argp = hp++; goto L_copy_binary;
	    break;
	case SMALL_DEF:
	case ATOM_DEF:
	case PID_DEF:
	case PORT_DEF:
	    hp++;
	    break;
	case ARITYVAL_DEF:
	    if (hp == const_tuple)
		hp += (arityval(obj)+1);
	    else
		hp++;
	    break;
	case THING_DEF:  /* binaries */
	    hp += (thing_arityval(obj)+1);
	    break;
	default:
	    erl_exit(1, "Internal error: in copy_struct (2) 0x%08x\n", *hp);
	}
    }

    if (htop > hbot) {
	erl_exit(1, "Internal error: in copy_struct (3)\n");
    }
    ASSERT(htop == hbot);
    *hpp = hend;
    return res;


/* -----------------------------------------------
   Copy list:
   Arguments are argp: pointer to store the result
                  obj: the list to copy
   Cells with constant car value is copied at the
   bottom of the heap otherwise cell is copied at
   the top of the stack
   ----------------------------------------------- */

 L_copy_list:
    tailp = argp;
    while(is_list(obj)) {
	objp = ptr_val(obj);
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
    if (IS_CONST(obj)) {
	*tailp = obj;
	goto L_copy;
    }
    else {
	switch(tag_val_def(obj)) {
	case FLOAT_DEF:  argp = tailp; goto L_copy_float;
	case REFER_DEF:  argp = tailp; goto L_copy_refer;
	case BIG_DEF:    argp = tailp; goto L_copy_big;
	case TUPLE_DEF:  argp = tailp; goto L_copy_tuple;
	case BINARY_DEF: argp = tailp; goto L_copy_binary;
	default:
	    erl_exit(1, "Internal error: copy_struct (cdr)\n");
	}
    }
    
/* -----------------------------------------------
   Copy float:
   Arguments are argp: pointer to store the result
                  obj: the object to copy
   Thing is placed at the bottom of heap (hbot)
   ----------------------------------------------- */

 L_copy_float:
    objp = ptr_val(obj);
    hbot -= 3;
    tp = hbot;
    *argp = make_float(hbot);
    *tp++ = *objp++;
    *tp++ = *objp++;
    *tp   = *objp++;
    goto L_copy;

/* -----------------------------------------------
   Copy refer:
   Arguments are argp: pointer to store the result
                  obj: the object to copy
   Thing is placed at the bottom of heap (hbot)
   ----------------------------------------------- */

 L_copy_refer:
    objp = ptr_val(obj);
    i = thing_arityval(*objp)+1;
    hbot -= i;
    tp = hbot;
    *argp = make_refer(hbot);
    while(i--)
	*tp++ = *objp++;
    goto L_copy;

/* -----------------------------------------------
   Copy big:
   Arguments are argp: pointer to store the result
                  obj: the object to copy
   Thing is placed at the bottom of heap (hbot)
   ----------------------------------------------- */

 L_copy_big:
    objp = ptr_val(obj);
    i = thing_arityval(*objp)+1;
    hbot -= i;
    tp = hbot;
    *argp = make_big(hbot);
    while(i--)
	*tp++ = *objp++;
    goto L_copy;

 L_copy_binary:
    cp_binary(obj, &htop, &hbot, argp, off_heap);
    goto L_copy;

/* -----------------------------------------------
   Copy tuple:
   Arguments are argp: pointer to store the result
                  obj: the object to copy
   Thing is placed at the top of heap (htop)
   ----------------------------------------------- */

 L_copy_tuple:
    const_flag = 1;             /* assume constant tuple */
    objp = ptr_val(obj);
    i = arityval(*objp);
    *argp = make_tuple(htop);
    tp = htop;               /* tp is pointer to new arity value */
    *htop++ = *objp++;       /* copy arity value */
    while(i--) {
	elem = *objp++;
	if (!IS_CONST(elem))
	    const_flag = 0;
	*htop++ = elem;
    }
    if (const_flag)
	const_tuple = tp;     /* this is the latest const_tuple */
    goto L_copy;
#undef COPIED
}

/*
 * Copy the cells in a tuple, adjusting the pointers to its subterms but
 * not copying them.
 */

Eterm
copy_shallow(Eterm* ptr, uint32 sz, Eterm** hpp, ErlOffHeap* off_heap)
{
    Eterm* tp = ptr;
    Eterm* hp = *hpp;
    sint32 offs = hp - tp;

    while (sz--) {
	Eterm val = *tp++;
	switch(tag_val_def(val)) {
	case BIG_DEF:
	case REFER_DEF:
	case FLOAT_DEF:
	case LIST_DEF:
	case TUPLE_DEF:
	case BINARY_DEF:
	    *hp++ = offset_ptr(val, offs);
	    break;
	case THING_DEF: 
	    *hp++ = val;	/* Copy thing word */
	    switch (thing_subtag(val)) {
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
		{  /* bigs, floats, references */
		    int tari = thing_arityval(val);

		    sz -= tari;
		    while (tari--) {
			*hp++ = *tp++;
		    }
		}
		break;
	    }
	    break;
	case SMALL_DEF:
	case ATOM_DEF:
	case PID_DEF:
	case PORT_DEF:
	case ARITYVAL_DEF:
	    *hp++ = val;
	    break;
	case MOVED_DEF:
	    erl_exit(1, "Internal error: in copy_shallow 0x%08x\n", val);
	}
    }
    *hpp = hp;
    return make_tuple(ptr + offs); 
}

static void
cp_binary(Eterm obj, Eterm** hpp, Eterm** hbotp, Eterm* res, ErlOffHeap* off_heap)
{
    Eterm *objp, *hp, *tp;
    ProcBin *pb;
    Eterm* hbot = *hbotp;

    hp = *hpp;
    objp = ptr_val(obj);
    ASSERT(is_thing(*objp));
    switch (thing_subtag(*objp)) {
    case REFC_BINARY_SUBTAG:
	{
	    int i;

	    i = thing_arityval(*objp) + 1;
	    hbot -= i;
	    tp = hbot;
	    while (i--)  {
		*tp++ = *objp++;
	    }
	    *res = make_binary(hbot);
	    pb = (ProcBin*) hbot;
	    pb->val->refc++;
	    pb->next = off_heap->mso;
	    off_heap->mso = pb;
	    off_heap->overhead += pb->size / BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
	    *hbotp = hbot;
	}
	break;
    case FUN_SUBTAG:
	{
	    int i;
	    Eterm* htop = *hpp;
	    ErlFunThing* funp = (ErlFunThing *) objp;

	    i = thing_arityval(*objp) + 1 + funp->num_free;
	    tp = htop;
	    while (i--)  {
		*htop++ = *objp++;
	    }
	    funp = (ErlFunThing *) tp;
	    funp->next = off_heap->funs;
	    off_heap->funs = funp;
	    *res = make_binary(tp);
	    *hpp = htop;
	}
	break;
    case HEAP_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Heap binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    case SUB_BINARY_SUBTAG:
	erl_exit(1, "%s, line %d: Sub binaries not implemented yet",
		 __FILE__, __LINE__);
	break;
    default:
	erl_exit(1, "%s, line %d: Bad subtag: %d",
		 __FILE__, __LINE__, thing_subtag(*objp));

    }
}
