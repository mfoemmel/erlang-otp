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
#include "bif.h"

/* Imported from drv/gzio.c. Why not in any header file? */
EXTERN_FUNCTION(DriverBinary*, gzinflate_buffer, (char*, int));

/* Imported from sys.c */
EXTERN_FUNCTION(void*, sys_alloc2, (unsigned int));
EXTERN_FUNCTION(void*, sys_realloc2, (void*,unsigned int));
EXTERN_FUNCTION(void, sys_free2, (void*));


/* Forward */
static int is_printable_string(uint32);

#define UNSAFE_MASK  0xc0000000	/* Mask for bits that must be constant
				 * in pointers.
				 */

/*
 * The following macro checks that the start and end addresses of
 * a memory block allocated by sys_alloc() are inside the allowable
 * ranges (to allow four tag bits).
 */

#if EXTRA_POINTER_BITS == 0
/*
 * In this case, memory is allocated from 0 and upwards.
 * If the end pointer is okay, the start pointer must be okay too.
 */
#define CHECK_MEMORY(ptr, size) \
  if (((((unsigned long) ptr)+size) & UNSAFE_MASK) != 0) {\
    erl_exit(1, "Got unusable memory block 0x%x, size %u\n", ptr, size); \
  }
#else
/*
 * The following test assumes that the start pointer can never be
 * below the allowed range.  If that could be the case we would have
 * to test both start and end pointers.
 */
#define CHECK_MEMORY(ptr, size) \
  if (((((unsigned long) ptr)+size) & UNSAFE_MASK) != EXTRA_POINTER_BITS) {\
    erl_exit(1, "Got unusable memory block 0x%x, size %u\n", ptr, size); \
  }

#endif

/* allocate buffer memory and attach it to heap */
Eterm* 
halloc(Process* p, uint32 sz)
{
    ErlHeapFragment* bp = new_message_buffer(sz);

    bp->next = p->mbuf;
    p->mbuf = bp;

    /*
     * Update the size of message buffers associated with the process.
     * Test if time to do GC; if so bump the reduction count to force
     * a context switch.
     */

    p->mbuf_sz += sz;
    p->off_heap.overhead += (sizeof(ErlHeapFragment)/sizeof(uint32) - 1); 
    if (((p->mbuf_sz + p->off_heap.overhead)*MBUF_GC_FACTOR) >= p->heap_sz) {
	BUMP_ALL_REDS(p);
    }
    return bp->mem;
}

/*
 * Helper function for the ESTACK macros defined in global.h.
 */

void
erl_grow_stack(ErlStack* s)
{
    int sz;

    if (s->start == s->default_stack) {
	sz = sizeof(s->default_stack) / sizeof(s->default_stack[0]);
	s->start = (uint32*) sys_alloc_from(4, 2 * sz * sizeof(uint32));
	sys_memcpy(s->start, s->default_stack, sizeof(uint32)*sz);
    } else {
	sz = s->end - s->start;
	s->start = (uint32*) sys_realloc ((char*)s->start,
					  2*sz*sizeof(uint32));
    }
    s->end = s->start + 2*sz;
    s->sp = s->start + sz;
}


/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/*
** Generate the integer part from a double
*/
uint32 double_to_integer(p, x)
Process* p; double x;
{
    int sgn;
    int ds;
    digit_t* xp;
    int i;
    uint32 res;
    uint32 sz;
    uint32* hp;

    if ((x <= (float) MAX_SMALL) && (x >= (float) MIN_SMALL)) {
	sint32 xi = x;
	return make_small(xi);
    }

    if (x < 0) {
	sgn = 1;
	x = -x;
    }
    else
	sgn = 0;

    /* Unscale & (calculate exponent) */
    ds = 0;
    while(x >= 1.0) {
	x /= D_BASE;         /* "shift" right */
	ds++;
    }
    sz = ((ds+1) >> 1);          /* number of words */

    /*
     * Beam note: This function is called from guard bifs (round/1 and trunc/1),
     * which are not allowed to build anything at all on the heap.
     * Therefore it is essential to use the ArithAlloc() macro instead of HAlloc()
     * (on Jam, ArithAlloc() is just an alias for HAlloc()).
     */
    hp = ArithAlloc(p, sz+1);
    res = make_big(hp);
    xp = (digit_t*) (hp + 1);

    for (i = ds-1; i >= 0; i--) {
	digit_t d;

	x *= D_BASE;      /* "shift" left */
	d = x;            /* trunc */
	xp[i] = d;        /* store digit */
	x -= d;           /* remove integer part */
    }
    if (ds & 1)  /* odd ds need to zero high word */
	xp[ds] = 0;

    if (sgn)
	*hp = make_thing(sz, NEGATIVE_BIG_SUBTAG);
    else
	*hp = make_thing(sz, POSITIVE_BIG_SUBTAG);
    hp += (sz + 1);
    return res;
}

/*
** Create a new link with ref
*/
ErlLink* new_ref_link(next, type, item, data, ref)
ErlLink* next; ErlLinkType type; uint32 item; uint32 data; uint32 ref;
{
    ErlLink* lnk = (ErlLink*) fix_alloc(link_desc);

    lnk->next = next;
    lnk->type = type;
    lnk->item = item;
    lnk->data = data;
    if (ref == NIL) {
	lnk->ref.t = NIL;
    } else {
	sys_memcpy(&lnk->ref, ref_ptr(ref), sizeof(Ref));
    }
    return lnk;
}

/*
** Create a new link
*/
ErlLink* new_link(next, type, item, data)
ErlLink* next; ErlLinkType type; uint32 item; uint32 data;
{
   return new_ref_link(next, type, item, data, NIL);
}

/*
** Delete an old link (and relink)
*/
void del_link(lnk)
ErlLink** lnk;
{
    ErlLink* tlink;

    if (lnk != NULL) {
	tlink = *lnk;
	*lnk = tlink->next;
	fix_free(link_desc, (uint32*)tlink);
    }
}

/*
** Find a link, given the value of the ref field
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink** find_link_by_ref(first, ref)
ErlLink** first; Ref *ref;
{
    ErlLink* lnk = *first;
    ErlLink* prev = NULL;

    while (lnk != NULL) {
	if (!is_nil(lnk->ref.t) && eqref(&lnk->ref,ref)) {
	    return (prev == NULL) ? first : &prev->next;
	}
	prev = lnk;
	lnk = lnk->next;
    }
    return NULL;
}

/*
** Find a link.
** Result is NULL if not found 
** otherwise a pointer to a pointer to it is returned (fit with del_link)
*/
ErlLink** find_link(first, type, item, data)
ErlLink** first; ErlLinkType type; uint32 item; uint32 data;
{
    ErlLink* lnk = *first;
    ErlLink* prev = NULL;

    while(lnk != NULL) {
	if ((lnk->type == type) && (lnk->item == item)) {
	    if ((data == NIL) || (lnk->data == data))
		return (prev == NULL) ? first : &prev->next;
	}
	prev = lnk;
	lnk = lnk->next;
    }
    return NULL;
}

/*
** Calculate length of a list 
** -1 if not a proper list i.e not terminated with NIL
*/
int list_length(list)
uint32 list;
{
    int i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(ptr_val(list));
    }
    if (is_not_nil(list))
	return -1;
    return i;
}


/* make a hash index from an erlang term */

/* some prime numbers just above 2 ^ 28 */

#define FUNNY_NUMBER1  268440163
#define FUNNY_NUMBER2  268439161
#define FUNNY_NUMBER3  268435459
#define FUNNY_NUMBER4  268436141
#define FUNNY_NUMBER5  268438633
#define FUNNY_NUMBER6  268437017
#define FUNNY_NUMBER7  268438039
#define FUNNY_NUMBER8  268437511
#define FUNNY_NUMBER9  268439627
#define FUNNY_NUMBER10 268440479

Uint
make_hash(Eterm term, Uint hash)
{
    switch tag_val_def(term) {
    case ATOM_DEF:
	if (is_nil(term)) {
	    return hash*FUNNY_NUMBER3 + 1;
	}
	return hash*FUNNY_NUMBER1 + 
	    (atom_tab(unsigned_val(term))->slot.bucket.hvalue);
    case SMALL_DEF:
	return hash*FUNNY_NUMBER2 + unsigned_val(term);
    case BINARY_DEF:
	{
	    if (thing_subtag(*ptr_val(term)) != FUN_SUBTAG) {
		byte* ptr = ((ProcBin*) ptr_val(term))->bytes;
		unsigned sz = ((ProcBin*) ptr_val(term))->size;
		int i = (sz < 15) ? sz : 15;

		while (i--) {
		    hash = hash*FUNNY_NUMBER1 + *ptr++;
		}
		return hash*FUNNY_NUMBER4 + sz;
	    } else {
		ErlFunThing* funp = (ErlFunThing *) ptr_val(term);
		Uint num_free = funp->num_free;
		Uint i;

		hash = hash * FUNNY_NUMBER10 + num_free;
		hash = hash*FUNNY_NUMBER1 + 
		    (atom_tab(funp->modp->module)->slot.bucket.hvalue);
		hash = hash*FUNNY_NUMBER2 + funp->index;
		hash = hash*FUNNY_NUMBER2 + unsigned_val(funp->uniq);
		for (i = 0; i < num_free; i++) {
		    hash = make_hash(funp->env[i], hash);
		}
		return hash;
	    }
	}

    case PID_DEF:
	return hash*FUNNY_NUMBER5 + get_number(term);

    case PORT_DEF:
	return hash*FUNNY_NUMBER9 + get_number_port(term);

    case REFER_DEF:
	return hash*FUNNY_NUMBER9 + get_number_reference(term);

    case FLOAT_DEF: 
	{
	    FloatDef ff;
	    GET_DOUBLE(term, ff);
	    return hash*FUNNY_NUMBER6 + (ff.fw[0] ^ ff.fw[1]);
	}
	break;

    case LIST_DEF:
	{
	    uint32* list = ptr_val(term);

	    while(1) {
		hash = make_hash(*list, hash);
		if (is_not_list(CDR(list)))
		    return make_hash(CDR(list),hash)*FUNNY_NUMBER8;
		list = ptr_val(CDR(list));
	    }
	}
	break;

    case BIG_DEF:
      {
	uint32* ptr  = ptr_val(term);
	uint32 arity = thing_arityval(*ptr);
	int subtype = thing_subtag(*ptr);
	int i = arity;
	
	ptr++;
	while (i--) {
	    hash = hash*FUNNY_NUMBER2 + *ptr++;
	}
	
	if (subtype == NEGATIVE_BIG_SUBTAG)
	    return hash*FUNNY_NUMBER3 + arity;
	else
	    return hash*FUNNY_NUMBER2 + arity;
      }
      break;

    case TUPLE_DEF: 
	{
	    uint32* ptr = ptr_val(term);
	    uint32 arity = arityval(*ptr);
	    int i = arity;

	    ptr++;
	    while(i--)
		hash = make_hash(*ptr++, hash);
	    return hash*FUNNY_NUMBER9 + arity;
	}
	break;

    default:
	erl_exit(1, "Invalid tag in make_hash\n");
	return 0;
    }
}

int send_error_to_logger(gleader)
uint32 gleader;
{
    Process* p;
    ErlHeapFragment* bp;
    uint32* hp;
    uint32 name;
    uint32 res;
    uint32 gl;
    uint32 list;
    int i;
    
    if (gleader == 0)
	gl = am_noproc;
    else
	gl = gleader;
    if ((i = cerr_pos) == 0)
	return 0;
    name = am_error_logger;
    if ((p = whereis_process(unsigned_val(name))) == NULL)  {
	erl_printf(CERR,"%s",tmp_buf);
	return(0);
    }
    /* !!!!!!! Uhhh  */
    if (p->status == P_EXITING || p->status == P_RUNNING) {
	erl_printf(CERR,"%s",tmp_buf);
	return(0);
    }
    bp = new_message_buffer(i*2 + 4);
    hp = bp->mem;
    list = buf_to_intlist(&hp, tmp_buf, i, NIL);
    res = TUPLE3(hp, am_emulator, gl, list);
    queue_message(p, bp, res);
    return 1;
}


/* Ok it's not very clever to rename this function, but i do it any way :-) */
/* safe_malloc() -> safe_alloc() */

void *safe_alloc(len)
uint32 len;
{
    char *buf;

    if ((buf = sys_alloc(len)) == NULL)
	erl_exit(1, "Can't allocate %d bytes of memory\n", len);
    CHECK_MEMORY(buf, len);
    return(buf);
}


void *safe_realloc(ptr, len)
char* ptr; uint32 len;
{
    char *buf;

    if ((buf = sys_realloc(ptr, len)) == NULL)
	erl_exit(1, "Can't reallocate %d bytes of memory\n", len);
    CHECK_MEMORY(buf, len);
    return(buf);
}


/* eq and cmp are written as separate functions a eq is a little faster */

/*
 * Test for equality of two terms.
 * Returns 0 if not equal, or a non-zero value otherwise.
 */

int
eq(Eterm a, Eterm b)
{
    int i;
    Eterm* aa;
    Eterm* bb;

    if (a == b) 
	return 1;

    if (not_eq_tags(a, b))
	return 0;

    switch (tag_val_def(a)) {
    case BINARY_DEF:
	{
	    ProcBin* b1 = (ProcBin*) ptr_val(a);
	    ProcBin* b2 = (ProcBin*) ptr_val(b);
	    Uint st1 = thing_subtag(b1->thing_word);
	    Uint st2 = thing_subtag(b2->thing_word);
	    
	    if (st1 == REFC_BINARY_SUBTAG && st2 == REFC_BINARY_SUBTAG) {
		return b1->size == b2->size &&
		    sys_memcmp(b1->bytes, b2->bytes, b1->size) == 0;
	    } else if (st1 == FUN_SUBTAG && st2 == FUN_SUBTAG) {
		ErlFunThing* f1 = (ErlFunThing *) b1;
		ErlFunThing* f2 = (ErlFunThing *) b2;
		int num_free;

		if (f1->modp != f2->modp || f1->index != f2->index ||
		    f1->uniq != f2->uniq || f1->num_free != f2->num_free) {
		    return 0;
		}
		num_free = f1->num_free;
		for (i = 0; i < num_free; i++) {
		    if (!eq(f1->env[i], f2->env[i])) {
			return 0;
		    }
		}
		return 1;
	    } else {
		return 0;
	    }
	}
    case LIST_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	while (1) {
	    if (!eq(*aa++, *bb++)) return(0);
	    if (*aa == *bb) return(1);
	    if (is_not_list(*aa) || is_not_list(*bb)) return(eq(*aa, *bb));
	    aa = ptr_val(*aa);
	    bb = ptr_val(*bb);
	}
    case TUPLE_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	if (*aa != *bb) return(0); /* different arities */
	i = arityval(*aa);	   /* get the arity*/
	while (i--) {
	    if (eq(*++aa, *++bb) == 0) return(0);
	}
	return(1);
    case REFER_DEF:
        {
	    int alen = refer_arity(a);
	    int blen = refer_arity(b);
	    int len;

	    if (ref_ptr(a)->h != ref_ptr(b)->h)
		return 0;

	    len = alen;
	    if (len > blen)
		len = blen;

	    for (i = len-2; i >= 0; i--) {
		if (ref_ptr(a)->w[i] != ref_ptr(b)->w[i])
		    return 0;
	    }
	    return 1;
	}
    case BIG_DEF:
	aa = ptr_val(a);  /* get pointer to thing */
	bb = ptr_val(b);
	if (*aa != *bb) return(0);
        i = BIG_ARITY(aa);
	while(i--) {
	    if (*++aa != *++bb) return(0);
	}
	return(1);
    case FLOAT_DEF:
	{
	    FloatDef af;
	    FloatDef bf;

	    GET_DOUBLE(a, af);
	    GET_DOUBLE(b, bf);
	    if (af.fd == bf.fd) return(1);
	    return(0);
	}

    default:
	return(0);
    }
}

/* 
 * Lexically compare two strings of bytes (string s1 length l1 and s2 l2).
 *
 *	s1 < s2	return -1
 *	s1 = s2	return  0
 *	s1 > s2 return +1
 */
static int cmpbytes(s1,l1,s2,l2)
byte *s1,*s2;
int l1,l2;
{
    int i;
    i = 0;
    while((i < l1) && (i < l2)) {
	if (s1[i] < s2[i]) return(-1);
	if (s1[i] > s2[i]) return(1);
	i++;
    }
    if (l1 < l2) return(-1);
    if (l1 > l2) return(1);
    return(0);
}


/*
 * Compare objects.
 * Returns 0 if equal, a negative value if a < b, or a positive number a > b.
 *
 * According to the Erlang Standard, types are orderered as follows:
 *   numbers < (characters) < atoms < refs < funs < ports < pids <
 *   tuples < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

static Eterm big_buf[2];

int
cmp(Eterm a, Eterm b)
{
    Eterm* aa;
    Eterm* bb;
    int i;
    int j;

    if (a == b) {		/* Equal values or pointers. */
	return 0;
    }

    /*
     * NIL is a special case because it is an atom. Only conses and
     * binaries are greater than NIL.
     */

    if (is_nil(a)) {
	if (tag_val_def(b) == LIST ||
	    (tag_val_def(b) == BINARY &&
	     thing_subtag(*ptr_val(b)) != FUN_SUBTAG)) {
	    return -1;
	}
	return 1;
    } else if (is_nil(b)) {
	if (tag_val_def(a) == LIST ||
	    (tag_val_def(a) == BINARY &&
	     thing_subtag(*ptr_val(a)) != FUN_SUBTAG)) {
	    return 1;
	}
	return -1;
    }

    /*
     * Take care of the case that the tags are different.
     */

    if (not_eq_tags(a, b)) {
	FloatDef f1, f2;
	Eterm big;

	switch(NUMBER_CODE(a, b)) {
	case SMALL_BIG:
	    big = small_to_big(signed_val(a), big_buf);
	    return big_comp(big, b);
	case SMALL_FLOAT:
	    f1.fd = signed_val(a);
	    GET_DOUBLE(b, f2);
	    return float_comp(f1.fd, f2.fd);
	case BIG_SMALL:
	    big = small_to_big(signed_val(b), big_buf);
	    return big_comp(a, big);
	case BIG_FLOAT:
	    f1.fd = big_to_double(a);
	    GET_DOUBLE(b, f2);
	    if (!FP_RESULT_OK(f1.fd)) {
		return big_sign(a) ? -1 : 1;
	    }
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_SMALL:
	    GET_DOUBLE(a, f1);
	    f2.fd = signed_val(b);
	    return float_comp(f1.fd, f2.fd);
	case FLOAT_BIG:
	    GET_DOUBLE(a, f1);
	    f2.fd = big_to_double(b);
	    if (!FP_RESULT_OK(f2.fd)) {
	       return big_sign(b) ? 1 : -1;
	    }
	    return float_comp(f1.fd, f2.fd);
	default:
	    {
		int atag = tag_val_def(a);
		int btag = tag_val_def(b);

		/*
		 * The tags are different. Binaries must be handled specially.
		 */
		    
		if (atag == BINARY) {
		    if (thing_subtag(*ptr_val(a)) != FUN_SUBTAG) {
			return 1;
		    } else if (btag > PORT) {
			return 1;
		    } else {
			return -1;
		    }
		} else if (btag == BINARY) {
		    if (thing_subtag(*ptr_val(b)) != FUN_SUBTAG) {
			return -1;
		    } else if (atag >= REFER) {
			return -1;
		    } else {
			return 1;
		    }
		} else {
		    /*
		     * The greater tag, the lesser term (when binaries and nils
		     * have been handled). This is of course highly dependent
		     * on the tag order.
		     */
		    return btag - atag;
		}
	    }
	}
    }

    /*
     * The tags are the same.
     */

    switch (tag_val_def(a)) {
    case ATOM_DEF:
	return(cmpbytes(atom_tab(unsigned_val(a))->name, atom_tab(unsigned_val(a))->len,
			atom_tab(unsigned_val(b))->name, atom_tab(unsigned_val(b))->len));
    case TUPLE_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	/* compare the arities */
	if (arityval(*aa) < arityval(*bb)) return(-1);
	if (arityval(*aa) > arityval(*bb)) return(1);
	i = arityval(*aa);	/* get the arity*/
	while (i--) {
	    if ((j = cmp(*++aa, *++bb)) != 0) 
		return j;
	}
	return 0;
    case LIST_DEF:
	aa = ptr_val(a);
	bb = ptr_val(b);
	while (1) {
	    if ((j = cmp(*aa++, *bb++)) != 0) 
		return j;
	    if (*aa==*bb)
		return 0;
	    if (is_not_list(*aa) || is_not_list(*bb))
		return cmp(*aa, *bb);
	    aa = ptr_val(*aa);
	    bb = ptr_val(*bb);
	}
    case SMALL_DEF:
	return signed_val(a) - signed_val(b);
    case FLOAT_DEF:
	{
	    FloatDef af;
	    FloatDef bf; 

	    GET_DOUBLE(a, af);
	    GET_DOUBLE(b, bf);
	    return float_comp(af.fd, bf.fd);
	}

    case REFER_DEF:
        {
	    int alen = refer_arity(a);
	    int blen = refer_arity(b);
	    int len;

	    if (ref_ptr(a)->h < ref_ptr(b)->h)
		return -1;
	    else if (ref_ptr(a)->h > ref_ptr(b)->h)
		return 1;

	    len = alen;
	    if (len > blen)
		len = blen;

	    for (i = len-2; i >= 0; i--) {
		if (ref_ptr(a)->w[i] < ref_ptr(b)->w[i])
		    return -1;
		else if (ref_ptr(a)->w[i] > ref_ptr(b)->w[i])
		    return 1;
	    }
	    return 0;
	}
    case BIG_DEF:
	return big_comp(a, b);

    case BINARY_DEF:
	{
	    ProcBin* b1 = (ProcBin*) ptr_val(a);
	    ProcBin* b2 = (ProcBin*) ptr_val(b);
	    Uint st1 = thing_subtag(b1->thing_word);
	    Uint st2 = thing_subtag(b2->thing_word);

	    if (st1 == REFC_BINARY_SUBTAG && st2 == REFC_BINARY_SUBTAG) {
		int diff = b1->size - b2->size;
		if (diff != 0) {
		    return diff;
		} else {
		    return sys_memcmp(b1->bytes, b2->bytes, b1->size);
		}
	    } else if (st1 == FUN_SUBTAG && st2 == FUN_SUBTAG) {
		ErlFunThing* f1 = (ErlFunThing *) b1;
		ErlFunThing* f2 = (ErlFunThing *) b2;
		int num_free;
		int diff;

		diff = cmpbytes(atom_tab(f1->modp->module)->name,
				atom_tab(f1->modp->module)->len,
				atom_tab(f2->modp->module)->name,
				atom_tab(f2->modp->module)->len);
		if (diff != 0) {
		    return diff;
		}
		diff = f1->index - f2->index;
		if (diff != 0) {
		    return diff;
		}
		diff = f1->uniq - f2->uniq;
		if (diff != 0) {
		    return diff;
		}
		diff = f1->num_free - f2->num_free;
		if (diff != 0) {
		    return diff;
		}
		num_free = f1->num_free;
		for (i = 0; i < num_free; i++) {
		    if ((diff = cmp(f1->env[i], f2->env[i])) != 0) {
			return diff;
		    }
		}
		return 0;
	    } else {
		return st2 - st1;
	    }
	}
	break;
    case PID_DEF:
	if ((! (get_node(a) == get_node(b))) && /*  different nodes */
	    (get_number(a) == get_number(b)) &&
	    (get_serial(a) == get_serial(b))) { /* equal numbers */
	    
	    uint32 atoma, atomb;
	    i = get_node(a); /* index in atom table */
	    j = get_node(b);
	    atoma = dist_addrs[i].sysname;
	    atomb = dist_addrs[j].sysname;
	    return(cmpbytes(atom_tab(unsigned_val(atoma))->name,
			    atom_tab(unsigned_val(atoma))->len,
			    atom_tab(unsigned_val(atomb))->name,
			    atom_tab(unsigned_val(atomb))->len));
	    

	}
    default:
	return (a < b) ? -1 : 1;
    }
}

Process* pid2proc(pid)
uint32 pid;
{
    Process *rp;
    int i;
    int pix = get_number(pid);

    if (is_not_pid(pid) || 
	(get_node(pid) != THIS_NODE) || 
	(pix >= max_process))
	return NULL;
    i = get_creation(pid);
    if ((i != this_creation) && (i != 0))
	return NULL;

    rp = process_tab[pix];
    if (INVALID_PID(rp, pid))
	return NULL;
    return rp;
}


static int dcount;

/* 
 * Display a term.
 */

static int 
display1(Eterm obj, CIO fd)
{
    int i, k;
    Eterm* nobj;

    if (dcount-- <= 0) return(1);

    switch (tag_val_def(obj)) {
    case ATOM_DEF:
        if(is_nil(obj)) {
            erl_printf(fd, "[]");
            break;
        }
	print_atom((int)unsigned_val(obj),fd);
	break;
    case SMALL_DEF:
	erl_printf(fd, "%d", signed_val(obj));
	break;
    case BIG_DEF:
	nobj = ptr_val(obj);
	i = BIG_SIZE(nobj);
	if (BIG_SIGN(nobj))
	    erl_printf(fd, "-#integer(%d) = {", i);
	else
	    erl_printf(fd, "#integer(%d) = {", i);
	erl_printf(fd, "%d", BIG_DIGIT(nobj, 0));
	for (k = 1; k < i; k++)
	    erl_printf(fd, ",%d", BIG_DIGIT(nobj, k));
	erl_putc('}', fd);
	break;
    case REFER_DEF:
	erl_printf(fd, "<<%d",
		   get_node_reference(obj));
	for (i = refer_arity(obj)-2; i >= 0; i--)
	    erl_printf(fd, ",%lu",
		       ref_ptr(obj)->w[i]);
	erl_printf(fd, ">>");
	break;
    case PID_DEF:
	erl_printf(fd, "<%d.%d.%d>",
		get_node(obj),get_number(obj),get_serial(obj));
	break;
    case PORT_DEF:
	erl_printf(fd, "<%d,%d>", get_node_port(obj),
		get_number_port(obj));
	break;
    case LIST_DEF:
	if (is_printable_string(obj)) {
	   int c;
	   erl_putc('"', fd);
	   nobj = ptr_val(obj);
	   while (1) {
	      if (dcount-- <= 0) return(1);
	      c = signed_val(*nobj++);
	      if (c == '\n') {
		 erl_putc('\\', fd);
		 erl_putc('n', fd);
	      } else {
		 if (c == '"')
		    erl_putc('\\', fd);
		 erl_putc(c, fd);
	      }
	      if (is_not_list(*nobj)) break;
	      nobj = ptr_val(*nobj);
	   }
	   erl_putc('"', fd);
	} else {
	   erl_putc('[', fd);
	   nobj = ptr_val(obj);
	   while (1) {
	      if (display1(*nobj++, fd) != 0) return(1);
	      if (is_not_list(*nobj)) break;
	      erl_putc(',',fd);
	      nobj = ptr_val(*nobj);
	   }
	   if (is_not_nil(*nobj)) {
	      erl_putc('|', fd);
	      if (display1(*nobj, fd) != 0) return(1);
	   }
	   erl_putc(']', fd);
	}
	break;
    case TUPLE_DEF:
	nobj = ptr_val(obj);	/* pointer to arity */
	i = arityval(*nobj);	/* arity */
	erl_putc('{', fd);
	while (i--) {
	    if (display1(*++nobj,fd) != 0) return(1);
	    if (i >= 1) erl_putc(',',fd);
	}
	erl_putc('}',fd);
	break;
    case FLOAT_DEF: {
	    FloatDef ff;
	    GET_DOUBLE(obj, ff);
	    erl_printf(fd, "%.20e", ff.fd);
	}
	break;
    case BINARY_DEF:
	{
	    ProcBin* pb = (ProcBin *) ptr_val(obj);

	    if (thing_subtag(pb->thing_word) != FUN_SUBTAG) {
		erl_printf(fd, "#Bin<%d>", pb->size);
	    } else {
		ErlFunThing* funp = (ErlFunThing *) pb;
		Atom* ap;

		erl_printf(fd, "#Fun<");
		ap = atom_tab(funp->modp->module);
		for (i = 0; i < ap->len; i++) {
		    erl_putc(ap->name[i], fd);
		}
		erl_printf(fd, ".%d.%d>", funp->index, unsigned_val(funp->uniq));
	    }
	}
	break;
    case CP0:
    case CP4:
    case CP8:
    case CP12:
        erl_printf(fd, "<cp:%x>", obj);
        break;  
    case BLANK:
        erl_printf(fd, "blank");
        break;  
    default:
	erl_printf(fd, "<unknown:%x>", obj);
    }
    return(0);
}


/*
 * Display a term on file fd.
 * Only used by debugging rountines as Erlang formatting is 
 * done in the io module.
 */

void
display(Eterm obj, CIO fd)
{
    dcount = 100000;
    display1(obj, fd);
}


/* as above, but limit the number of items printed */
void ldisplay(obj, fd, count)
uint32 obj; CIO fd; int count;
{
    dcount = count;
    display1(obj, fd);
    if (dcount <= 0) erl_printf(fd, "... "); /* Show that more items exit */
}


/* print a name doing what quoting is necessary */
static void print_name(s, n, fd)
byte *s; int n; CIO fd;
{
    
    int need_quote;
    int pos;
    byte *cpos;
    int c;

    if (n == 0) {
	erl_printf(fd, "''");
	return;
    }

    need_quote = 0;
    cpos = s;
    pos = n - 1;

    c = *cpos++;
    if (!IS_LOWER(c))
	need_quote++;
    else {
	while (pos--) {
	    c = *cpos++;
	    if (!IS_ALNUM(c) && (c != '_')) {
		need_quote++;
		break;
	    }
	}
    }
    cpos = s;
    pos = n;
    if (need_quote)
	erl_putc('\'',fd);
    while(pos--) {
	c = *cpos++;
	switch(c) {
	case '\'': erl_printf(fd, "\\'"); break;
	case '\\': erl_printf(fd, "\\\\"); break;
	case '\n': erl_printf(fd, "\\n"); break;
	case '\f': erl_printf(fd, "\\f"); break;
	case '\t': erl_printf(fd, "\\t"); break;
	case '\r': erl_printf(fd, "\\r"); break;
	case '\b': erl_printf(fd, "\\b"); break;
	case '\v': erl_printf(fd, "\\v"); break;
	default:
	    if (IS_CNTRL(c))
		erl_printf(fd, "\\%03o", c);
	    else
		erl_putc(c, fd);
	    break;
	}
    }
    if (need_quote) 
	erl_putc('\'',fd);
}

/* print the text of an atom with number i on open file descriptor fd */
void print_atom(i, fd)
int i; CIO fd;
{
    if ((i < 0) || (i >= atom_table_size) ||  (atom_tab(i) == NULL))
	erl_printf(fd, "???");
    print_name(atom_tab(i)->name, atom_tab(i)->len, fd);
    dcount -= atom_tab(i)->len;
}

/* 
 *  member(X,Y)
 *  returns 0 if X is a member of list Y
 *  returns 1 if X is not a member of list Y
 *  returns 2 if Y is not a list or is a badly formed list
 */

int member(x,y)
uint32 x,y;
{
    uint32 *z;
    if (is_nil(y)) return(1); /* empty list */
    if (is_not_list(y)) return(2); /* bad argument */
    z = ptr_val(y);
    for (;;) {
	if (eq(*z, x)) return(0);
	if (is_nil(*(z + 1))) return(1); /* end of list */
	if (is_not_list(*(z + 1))) return(2); /* badly formed list */
	z = ptr_val(*(z + 1));
    }
}


/* routines for converting no aligned bytes arrays to floats (double)
   and vice versa */
double bytes_to_float(b)
byte *b;
{
    union {
	double fd;
	byte fb[sizeof(double)];
    } f;
    int i;
    for (i = 0; i < sizeof(double); i++) f.fb[i] = *b++;
    return(f.fd);
}


void float_to_bytes(b,fl)
byte *b; double fl;
{
    union {
	double fd;
	byte fb[sizeof(double)];
    } f;
    int i;

    f.fd = fl;
    for (i = 0; i < sizeof(double); i++)
	*b++ = f.fb[i];
}


void bin_write(fp,buf,sz)
CIO fp; byte* buf;
int sz;
{
    int i;

    for (i=0;i<sz;i++) {
	if (IS_DIGIT(buf[i]))
	    erl_printf(fp, "%d,", buf[i]);
	else if (IS_PRINT(buf[i])) {
	    erl_putc(buf[i],fp);
	    erl_putc(',',fp);
	}
	else
	    erl_printf(fp,"%d,", buf[i]);
    }
    erl_putc('\n',fp);
}

/* Fill buf with the contents of bytelist list 
   return number of chars in list or -1 for error */

int intlist_to_buf(list,buf,len)
uint32 list;
byte *buf;
int len;
{
    uint32 *listptr;
    int sz = 0;

    if (is_nil(list)) 
	return 0;
    if (is_not_list(list))
	return -1;
    listptr = ptr_val(list);

    while (sz < len) {
	if (!is_byte(*listptr)) 
	    return -1;
	buf[sz++] = unsigned_val(*listptr);
	if (is_nil(*(listptr + 1)))
	    return(sz);
	if (is_not_list(*(listptr + 1))) 
	    return -1;
	listptr = ptr_val(*(listptr + 1));
    }
    return -1;			/* not enough space */
}

/*
** Convert an integer to a byte list buf must have at least 12 bytes avaiable
** return pointer to converted stuff (need not to be at start of buf!)
*/
char* int_to_buf(n, buf)
int n; char* buf;
{
    char* p = buf+11;
    int sign = 0;

    *p-- = '\0'; /* null terminate */
    if (n == 0)
	*p-- = '0';
    else if (n < 0) {
	sign = 1;
	n = -n;
    }

    while (n != 0) {
	*p-- = (n % 10) + '0';
	n /= 10;
    }
    if (sign)
	*p-- = '-';
    return p+1;
}

/* Build a list of integers in some safe memory area
** Memory must be pre allocated prio call 2*len in size
** hp is a pointer to the "heap" pointer on return
** this pointer is updated to point after the list
*/

uint32 buf_to_intlist(hpp, buf, len, tail)
uint32** hpp; byte *buf; int len; uint32 tail;
{
    uint32* hp = *hpp;

    buf += (len-1);
    while(len > 0) {
	tail = CONS(hp, make_small((byte)*buf), tail);
	hp += 2;
	buf--;
	len--;
    }
    *hpp = hp;
    return tail;
}

/*
** write io list in to a buffer.
**
** A iolist is defined as:
**
** iohead ::= Binary
**        |   Byte (i.e integer in range [0..255]
**        |   iolist
**        ;
**
** iotail ::= []
**        |   Binary  (added by tony)
**        |   iolist
**        ;
**
** iolist ::= []
**        |   Binary
**        |   [ iohead | iotail]
**        ;
** 
** return (char*) 0 on overflow, 
**        (char*) 1 on error
**        pointer continue buffer otherwise
** 
*/
#define IOL_OVERFLOW ((char*) 0)
#define IOL_ERROR    ((char*) 1)

static char*
iol_to_buf(Eterm list, char* ptr, char* maxptr)
{
    int i;

    while (is_list(list)) {
	Eterm* cons = ptr_val(list);
	Eterm obj = CAR(cons);

	list = CDR(cons);
	if (is_byte(obj)) {
	    if (ptr >= maxptr)
		return IOL_OVERFLOW;
	    *ptr++ = unsigned_val(obj);
	} else if (is_list(obj)) {
	    if ((ptr = iol_to_buf(obj, ptr, maxptr)) <= IOL_ERROR)
		return ptr;
	} else if (is_binary(obj)) {
	    ProcBin *pb = (ProcBin*) ptr_val(obj);
	    if (thing_subtag(pb->thing_word) == FUN_SUBTAG) {
		return IOL_ERROR;
	    } else {
		i = pb->size;
		if (ptr + i >= maxptr)
		    return IOL_OVERFLOW;
		sys_memcpy(ptr, pb->bytes, i);
		ptr += i;
	    }
	} else if (!is_nil(obj))
	    return IOL_ERROR;
    }

    if (is_nil(list)) {
	return ptr;
    } else if (is_binary(list)) {
	ProcBin *pb = (ProcBin*) ptr_val(list);
	if (thing_subtag(pb->thing_word) == FUN_SUBTAG) {
	    return IOL_ERROR;
	} else {
	    i = pb->size;
	    if (ptr + i >= maxptr)
		return IOL_OVERFLOW;
	    sys_memcpy(ptr, pb->bytes, i);
	    return ptr + i;
	}
    }
    return IOL_ERROR;
}


/* Fills a deep list of chars and binaries int buf */
/* Used by the port write routines                 */
/* Return 0 on success,                            */
/*        -1 on overflow                           */
/*        -2 on type error                         */

int
io_list_to_buf(Eterm list, char* buf, int* cpos, int max)
{
    char* ptr = buf + *cpos;  /* start position */
    char* end = buf + max;    /* end position */
    char* cur;
    
    if ((cur = iol_to_buf(list, ptr, end)) > IOL_ERROR) {
	*cpos += (cur - ptr);
	return 0;
    }
    else if (cur == IOL_OVERFLOW)
	return -1;
    else
	return -2;
}


int
io_list_len(Eterm list)
{
    int len = 0;
    int i;

    while (is_list(list)) {
	Eterm* cons = ptr_val(list);
	Eterm obj = CAR(cons);

	list = CDR(cons);
	if (is_byte(obj)) {
	    len++;
	} else if (is_list(obj)) {
	    if ((i = io_list_len(obj)) < 0)
		return i;
	    len += i;
	} else if (is_binary(obj)) {
	    ProcBin* pb = (ProcBin *) ptr_val(obj);
	    if (thing_subtag(pb->thing_word) == FUN_SUBTAG) {
		return -1;
	    } else {
		len += pb->size;
	    }
	} else if (!is_nil(obj)) {
	    return -1;
	}
    }
    if (is_nil(list))
	return len;
    else if (is_binary(list)) {
	ProcBin* pb = (ProcBin *) ptr_val(list);
	if (thing_subtag(pb->thing_word) == FUN_SUBTAG) {
	    return -1;
	} else {
	    return len + pb->size;
	}
    } else
	return -1;
}

/* return 0 if item is not a non-empty flat list of bytes */

int is_string(list)
uint32 list;
{
    int len = 0;

    while(is_list(list)) {
	uint32* consp = ptr_val(list);
	uint32  hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

/* return 0 if item is not a non-empty flat list of printable characters */

static int is_printable_string(uint32 list)
{
    int len = 0;
    int c;

    while(is_list(list)) {
	uint32* consp = ptr_val(list);
	uint32  hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	c = signed_val(hd);
	/* IS_PRINT || IS_SPACE would be another way to put it */
	if (IS_CNTRL(c) && !IS_SPACE(c))
	   return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

int
do_load(group_leader, mod, code, size)
    uint32 group_leader;	/* Group leader or 0 if none. */
    uint32 mod;			/* Module name as an atom. */
    byte* code;			/* Points to the code to load */
    int size;			/* Size of code to load. */
{
    DriverBinary* bin;
    int result;

    if ((bin = (DriverBinary *) gzinflate_buffer(code, size)) == NULL) {
	return -1;
    }
    result = bin_load(group_leader, mod, bin->orig_bytes, bin->orig_size);
    driver_free_binary(bin);
    return result;
}

#ifdef DEBUG
/* debug routine which checks for a valid object */
int 
check_struct(Eterm obj)
{
/*
 * XXX This routine seems to be buggy and unreliable.
 */
    return 0;
#if 0
    uint32 i, arity, *nobj;
    switch (tag_val_def(obj)) {
    case ATOM_DEF:
	if (unsigned_val(obj) >= atom_table_size)
	    return 0;
	return 1;
    case SMALL_DEF:
    case PID_DEF:
    case REFER_DEF:
    case PORT_DEF:
	return 0;
    case BINARY_DEF:
	if (((ProcBin*) ptr_val(obj))->mark == 1)
	    return 1;
	if (((ProcBin*) ptr_val(obj))->size > 0xfffffff)
	    return 1;
	if (((ProcBin*) ptr_val(obj))->bytes == 0)
	    return 1;
	return 0;
    case BIG_DEF:
	nobj = ptr_val(obj);
	if (is_not_thing(*nobj)) return(1);
	/* A bignum on the heap may never fit into a small !!! */
	if ((arity = BIG_ARITY(nobj)) == 1) {
	    uint32 d =  BIG_DIGIT(nobj,0)+D_BASE*BIG_DIGIT(nobj,1);

	    if (IS_USMALL(BIG_SIGN(nobj), d))
		return(1);
	}
	return(0);

    case FLOAT_DEF:
	if (thing_subtag(*ptr_val(obj)) != FLOAT_THING_SUBTAG) {
	    return(1);
	}
	return(0); /* no way to check binary data */
    case LIST_DEF:
	nobj = ptr_val(obj);
	while (1) {
	    if (check_struct(*nobj++) != 0) return(1);
	    if (is_not_list(*nobj)) return(check_struct(*nobj));
	    nobj = ptr_val(*nobj);
	}
    case TUPLE_DEF:
	if (is_not_arity_value(*(ptr_val(obj)))) return(1);
	arity = arityval(*(ptr_val(obj)));
	for (i = 0; i < arity; i++)
	    if (check_struct(*(ptr_val(obj) + i + 1)) != 0) return(1);
	return(0);
    default:
	return(1);
    }
#endif
}
#endif /* DEBUG */

#ifdef INSTRUMENT
typedef union most_strict {
    double x;
    long y;
} Most_strict;

/* Note: sizeof(mem_link) is used as a constant in
   tools/src/instrument.erl; keep it updated if this struct changes. */

typedef struct mem_link
{
   struct mem_link *prev, *next;
   unsigned long size;
   int type;
   uint32 p;			/* which process allocated */
   Most_strict align;
} mem_link;

int alloc_who = -1;

static mem_link *mem_anchor = NULL;	/* better to set this in erl_init */

extern uint32 current_process;

static void link_in(l, size)
mem_link *l;
unsigned size;
{
   l->next = mem_anchor;
   if (mem_anchor != NULL)
      mem_anchor->prev = l;
   l->prev = NULL;
   l->size = size;
   if (l->type == -1)
      l->type = alloc_who;
   alloc_who = -1;
   mem_anchor = l;

   l->p = current_process;
}

static void link_out(l)
mem_link *l;
{
   mem_link *prev, *next;

   prev = l->prev;
   next = l->next;
   if (prev != NULL)
      prev->next = next;
   else
      mem_anchor = next;

   if (next != NULL)
      next->prev = prev;
}

void* sys_alloc(size)
unsigned int size;
{
   char *p;
   mem_link *l;

   p = sys_alloc2(size + sizeof(mem_link));
   if (p == NULL)
      return NULL;

   l = (mem_link *) p;
   l->type = -1;
   link_in(l, size);

   return (void *) (p + sizeof(mem_link));
}

void* sys_realloc(ptr, size)
void* ptr; unsigned int size;
{
   char *p, *new_p;
   mem_link *l;
   unsigned old_size;

   p = ((char *) ptr) - sizeof(mem_link);

   l = (mem_link *) p;
   link_out(l);
   old_size = l->size;

   new_p = sys_realloc2(p, size + sizeof(mem_link));
   if (new_p == NULL)
      return NULL;

   l = (mem_link *) new_p;
   link_in(l, size);

   return (void *) (new_p + sizeof(mem_link));
}

void sys_free(ptr)
void* ptr;
{
   mem_link *l;
   char *p;

   p = ((char *) ptr) - sizeof(mem_link);

   l = (mem_link *) p;
   link_out(l);

   sys_free2(p);
}

static void dump_memory_to_stream(FILE *f)
{
   mem_link *l;

   l = mem_anchor;

   while (l != NULL)
   {
      if (l->p == 0)
	 fprintf(f, "{%d, %lu, %lu, undefined}.\n",
		 l->type,
		 ((unsigned long) l) + sizeof(mem_link),
		 l->size);
      else
	 fprintf(f, "{%d, %lu, %lu, {%ld,%ld,%ld}}.\n",
		 l->type,
		 ((unsigned long) l) + sizeof(mem_link),
		 l->size,
		 get_node(l->p),get_number(l->p),get_serial(l->p));
      l = l->next;
   }
}

void dump_memory_to_fd(int fd)
{
   char buf[BUFSIZ];
   FILE *f;

   f = fdopen(fd, "w");
   if (f == NULL)
      return;

   /* Avoid allocating memory; we may have run out of it at this point. */
   setbuf(f, buf);

   dump_memory_to_stream(f);
   fflush(f);
}

int dump_memory_data(name)
const char *name;
{
   FILE *f;

   f = fopen(name, "w");
   if (f == NULL)
      return 0;

   dump_memory_to_stream(f);

   fclose(f);
   return 1;
}

uint32 collect_memory(process)
Process *process;
{
   uint32 list, tup;
   uint32 *hp, *end_hp;
   mem_link *l;
   uint32 need;
   uint32 pid;

   list = NIL;

   need = 0;
   l = mem_anchor;
   while (l != NULL)
   {
      need += 4+2;
      l = l->next;
   }

   /* The "alloc" operation itself is likely to add to the list,
      so add a little. */
   need += 20;

   hp = HAlloc(process, need);
   end_hp = hp + need;

   l = mem_anchor;
   while (l != NULL)
   {
      /* If it should turn out we need more than we allocated, jump
	 out, and continue allocating on the heap instead. */
      if (hp >= end_hp - (4+5+2))
	 break;

      if (l->p == 0)
	 pid = am_undefined;
      else
      {
	 pid = TUPLE3(hp,
		      make_small(get_node(l->p)),
		      make_small(get_number(l->p)),
		      make_small(get_serial(l->p)));
	 hp += 4;
      }

      tup = TUPLE4(hp,
		   make_small(l->type),
		   make_small(((int) l) + sizeof(mem_link)),
		   make_small(l->size),
		   pid);
      hp += 5;
      list = CONS(hp, tup, list);
      hp += 2;

      l = l->next;
   }

   while (l != NULL)
   {
      if (l->p == 0)
	 pid = am_undefined;
      else
      {
	 hp = HAlloc(process, 4);
	 pid = TUPLE3(hp,
		      make_small(get_node(l->p)),
		      make_small(get_number(l->p)),
		      make_small(get_serial(l->p)));
      }

      hp = HAlloc(process, 5);
      tup = TUPLE4(hp,
		   make_small(l->type),
		   make_small(((int) l) + sizeof(mem_link)),
		   make_small(l->size),
		   pid);
      hp = HAlloc(process, 2);
      list = CONS(hp, tup, list);

      l = l->next;
   }

   return list;
}

void alloc_from(from)
int from;
{
   if (alloc_who == -1)
      alloc_who = from;
}

#else

void* sys_alloc(size)
unsigned int size;
{
   return sys_alloc2(size);
}

void* sys_realloc(ptr, size)
void* ptr; unsigned int size;
{
   return sys_realloc2(ptr, size);
}

void sys_free(ptr)
void* ptr;
{
   sys_free2(ptr);
}
#endif

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(buf,sz)
byte* buf;
int sz;
{
    bin_write(CERR,buf,sz);
}

/* Print an atom as an uint32 or just as an index */     
void pat(a)
uint32 a;
{
    upp(atom_tab(unsigned_val(a))->name,
	atom_tab(unsigned_val(a))->len);
}


void pinfo()
{
    process_info(COUT);
}


void pp(p)
Process *p;
{
    print_process_info(p,CERR);
}
    
void ppi(p)
uint32 p;
{
    pp(process_tab[get_number(p)]);
}

void td(x) 
uint32 x;
{
    display(x, CERR);
    erl_putc('\n', CERR);
}

void ps(p, stop)
Process* p; uint32* stop;
{
    uint32* sp = p->hend-1;

    if (stop <= p->htop) {
	stop = p->htop + 1;
    }

    while(sp >= stop) {
	erl_printf(COUT,"%08lx: ", (uint32) sp);
	ldisplay(*sp, COUT, 75);
	erl_putc('\r', COUT);
	erl_putc('\n', COUT);
	sp--;
    }
}
#endif
