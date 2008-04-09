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
/*  Implementation of the erlang external format 
 *
 *  And a nice cache mechanism which is used just to send a
 *  index indicating a specific atom to a remote node instead of the
 *  entire atom.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "external.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "zlib.h"

#ifdef HIPE
#include "hipe_mode_switch.h"
#endif
#define in_area(ptr,start,nbytes) ((Uint)((char*)(ptr) - (char*)(start)) < (nbytes))

#define MAX_STRING_LEN 0xffff
#define dec_set_creation(nodename,creat)				\
  (((nodename) == erts_this_node->sysname && (creat) == ORIG_CREATION)	\
   ? erts_this_node->creation						\
   : (creat))

/*
 * For backward compatibility reasons, only encode integers that
 * fit in 28 bits (signed) using INTEGER_EXT.
 */
#define IS_SSMALL28(x) (((Uint) (((x) >> (28-1)) + 1)) < 2)

/*
 *   Valid creations for nodes are 1, 2, or 3. 0 can also be sent
 *   as creation, though. When 0 is used as creation, the real creation
 *   is unknown. Creation 0 on data will be changed to current
 *   creation of the node which it belongs to when it enters
 *   that node.
 *       This typically happens when a remote pid is created with
 *   list_to_pid/1 and then sent to the remote node. This behavior 
 *   has the undesirable effect that a pid can be passed between nodes,
 *   and as a result of that not being equal to itself (the pid that
 *   comes back isn't equal to the original pid).
 *
 */

static byte* enc_term(DistEntry*, Eterm, byte*, Uint32);
static byte* enc_atom(DistEntry*, Eterm, byte*);
static byte* enc_pid(DistEntry*, Eterm, byte*);
static byte* dec_term(DistEntry*, Eterm**, byte*, ErlOffHeap*, Eterm*);
static byte* dec_atom(DistEntry*, byte*, Eterm*);
static byte* dec_pid(DistEntry*, Eterm**, byte*, ErlOffHeap*, Eterm*);
static byte* dec_hashed_atom(DistEntry*, byte*, Eterm*);
static byte* dec_and_insert_hashed_atom(DistEntry*, byte*, Eterm*);
static Eterm term_to_binary(Process* p, Eterm Term, int level, Uint flags);
static Sint decoded_size(byte *ep, byte* endp, int only_heap_bins);


static Uint encode_size_struct2(Eterm, unsigned);


/**********************************************************************/

Eterm
term_to_binary_1(Process* p, Eterm Term)
{
    return term_to_binary(p, Term, 0, TERM_TO_BINARY_DFLAGS);
}

Eterm
term_to_binary_2(Process* p, Eterm Term, Eterm Flags)
{
    int level = 0;
    Uint flags = TERM_TO_BINARY_DFLAGS;

    while (is_list(Flags)) {
	Eterm arg = CAR(list_val(Flags));
	Eterm* tp;
	if (arg == am_compressed) {
	    level = Z_DEFAULT_COMPRESSION;
	} else if (is_tuple(arg) && *(tp = tuple_val(arg)) == make_arityval(2)) {
	    if (tp[1] == am_minor_version && is_small(tp[2])) {
		switch (signed_val(tp[2])) {
		case 0:
		    flags = TERM_TO_BINARY_DFLAGS;
		    break;
		case 1:
		    flags = TERM_TO_BINARY_DFLAGS|DFLAG_NEW_FLOATS;
		    break;
		default:
		    goto error;
		}
	    } else if (tp[1] == am_compressed && is_small(tp[2])) {
		Uint level_term = tp[2];
		if (is_not_small(level_term)) {
		    goto error;
		}
		level = unsigned_val(level_term);
		if (!(0 <= level && level < 10)) {
		    goto error;
		}
	    } else {
		goto error;
	    }
	} else {
	error:
	    BIF_ERROR(p, BADARG);
	}
	Flags = CDR(list_val(Flags));
    }
    if (is_not_nil(Flags)) {
	goto error;
    }

    return term_to_binary(p, Term, level, flags);
}

static ERTS_INLINE Sint
binary2term_prepare(ErtsBinary2TermState *state, byte *data, Sint data_size)
{
    Sint res;
    byte *bytes = data;
    Sint size = data_size;

    state->exttmp = 0;

    if (size < 1 || *bytes != VERSION_MAGIC) {
    error:
	if (state->exttmp)
	    erts_free(ERTS_ALC_T_TMP, state->extp);
	state->extp = NULL;
	state->exttmp = 0;
	return -1;
    }
    bytes++;
    size--;
    if (size < 5 || *bytes != COMPRESSED) {
	state->extp = bytes;
    }
    else  {
	uLongf dest_len = get_int32(bytes+1);
	state->extp = erts_alloc(ERTS_ALC_T_TMP, dest_len);
	state->exttmp = 1;
	if (uncompress(state->extp, &dest_len, bytes+5, size-5) != Z_OK)
	    goto error;
	size = (Sint) dest_len;
    }
    res = decoded_size(state->extp, state->extp + size, 0);
    if (res < 0)
	goto error;
    return res;
}

static ERTS_INLINE void
binary2term_abort(ErtsBinary2TermState *state)
{
    if (state->exttmp) {
	state->exttmp = 0;
	erts_free(ERTS_ALC_T_TMP, state->extp);
    }
}

static ERTS_INLINE Eterm
binary2term_create(ErtsBinary2TermState *state, Eterm **hpp, ErlOffHeap *ohp)
{
    Eterm res;
    if (!dec_term(NULL, hpp, state->extp, ohp, &res))
	res = THE_NON_VALUE;
    if (state->exttmp) {
	state->exttmp = 0;
	erts_free(ERTS_ALC_T_TMP, state->extp);
    }
    return res;
}

Sint
erts_binary2term_prepare(ErtsBinary2TermState *state, byte *data, Sint data_size)
{
    return binary2term_prepare(state, data, data_size);
}

void
erts_binary2term_abort(ErtsBinary2TermState *state)
{
    binary2term_abort(state);
}

Eterm
erts_binary2term_create(ErtsBinary2TermState *state, Eterm **hpp, ErlOffHeap *ohp)
{
    return binary2term_create(state, hpp, ohp);
}

BIF_RETTYPE binary_to_term_1(BIF_ALIST_1)
{
    Sint heap_size;
    Eterm res;
    Eterm* hp;
    Eterm* endp;
    Sint size;
    byte* bytes;
    byte* temp_alloc = NULL;
    ErtsBinary2TermState b2ts;

    if ((bytes = erts_get_aligned_binary_bytes(BIF_ARG_1, &temp_alloc)) == NULL) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    size = binary_size(BIF_ARG_1);

    heap_size = binary2term_prepare(&b2ts, bytes, size);
    if (heap_size < 0)
	goto error;

    hp = HAlloc(BIF_P, heap_size);
    endp = hp + heap_size;

    res = binary2term_create(&b2ts, &hp, &MSO(BIF_P));

    erts_free_aligned_binary_bytes(temp_alloc);

    if (hp > endp) {
	erl_exit(1, ":%s, line %d: heap overrun by %d words(s)\n",
		 __FILE__, __LINE__, hp-endp);
    }

    HRelease(BIF_P, endp, hp);

    if (res == THE_NON_VALUE)
	goto error;

    return res;
}

Eterm
external_size_1(Process* p, Eterm Term)
{
    Uint size = encode_size_struct(Term, TERM_TO_BINARY_DFLAGS);
    if (IS_USMALL(0, size)) {
	BIF_RET(make_small(size));
    } else {
	Eterm* hp = HAlloc(p, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(size, hp));
    }
}

static Eterm
term_to_binary(Process* p, Eterm Term, int level, Uint flags)
{
    int size;
    Eterm bin;
    size_t real_size;
    byte* endp;

    size = encode_size_struct(Term, flags);

    if (level != 0) {
	byte buf[256];
	byte* bytes = buf;
	byte* out_bytes;
	uLongf dest_len;

	if (sizeof(buf) < size) {
	    bytes = erts_alloc(ERTS_ALC_T_TMP, size);
	}

	if ((endp = enc_term(NULL, Term, bytes, flags))
	    == NULL) {
	    erl_exit(1, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, real_size - size);
	}

	/*
	 * We don't want to compress if compression actually increases the size.
	 * Therefore, don't give zlib more out buffer than the size of the
	 * uncompressed external format (minus the 5 bytes needed for the
	 * COMPRESSED tag). If zlib returns any error, we'll revert to using
	 * the original uncompressed external term format.
	 */

	if (real_size < 5) {
	    dest_len = 0;
	} else {
	    dest_len = real_size - 5;
	}
	bin = new_binary(p, NULL, real_size+1);
	out_bytes = binary_bytes(bin);
	out_bytes[0] = VERSION_MAGIC;
	if (compress2(out_bytes+6, &dest_len, bytes, real_size, level) != Z_OK) {
	    sys_memcpy(out_bytes+1, bytes, real_size);
	    bin = erts_realloc_binary(bin, real_size+1);
	} else {
	    out_bytes[1] = COMPRESSED;
	    put_int32(real_size, out_bytes+2);
	    bin = erts_realloc_binary(bin, dest_len+6);
	}
	if (bytes != buf) {
	    erts_free(ERTS_ALC_T_TMP, bytes);
	}
	return bin;
    } else {
	byte* bytes;

	bin = new_binary(p, (byte *)NULL, size);
	bytes = binary_bytes(bin);
	bytes[0] = VERSION_MAGIC;
	if ((endp = enc_term(NULL, Term, bytes+1, flags))	
	    == NULL) {
	    erl_exit(1, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, endp - (bytes + size));
	}
	return erts_realloc_binary(bin, real_size);
    }
}

/*
 * This function fills ext with the external format of atom.
 * If it's an old atom we just supply an index, otherwise
 * we insert the index _and_ the entire atom. This way the receiving side
 * does not have to perform an hash on the etom to locate it, and
 * we save a lot of space on the wire.
 */

static byte*
enc_atom(DistEntry *dep, Eterm atom, byte *ep)
{
    Eterm* ce;
    Uint ix;
    int i, j;

    ASSERT(is_atom(atom));

    /*
     * term_to_binary/1,2 and the initial distribution message
     * don't use the cache.
     */
    if (!dep || (dep->cache == NULL)) { 
	i = atom_val(atom);
	j = atom_tab(i)->len;
	*ep++ = ATOM_EXT;
	put_int16(j, ep);  /* XXX reduce this to 8 bit in the future */
	ep += 2;
	sys_memcpy((char *) ep, (char*)atom_tab(i)->name, (int) j);
	ep += j;
	return ep;
    }

    ix = atom_val(atom) % MAXINDX;
    ce = &dep->cache->out_arr[ix];
    
    if (*ce == atom) {		/* The atom is present in the cache. */
	*ep++ = CACHED_ATOM;
	*ep++ = ix;
	return ep;
    }

    /*
     * Empty slot or another atom - overwrite.
     */

    *ce = atom;
    i = atom_val(atom);
    j = atom_tab(i)->len;
    *ep++ = NEW_CACHE;
    *ep++ = ix;
    put_int16(j, ep);
    ep += 2;
    sys_memcpy((char *) ep, (char*) atom_tab(i)->name, (int) j);
    return ep + j;
}

static byte*
enc_pid(DistEntry *dep, Eterm pid, byte* ep)
{
    Uint on, os;

    *ep++ = PID_EXT;
    /* insert  atom here containing host and sysname  */
    ep = enc_atom(dep, pid_node_name(pid), ep);

    /* two bytes for each number and serial */

    on = pid_number(pid);
    os = pid_serial(pid);

    put_int32(on, ep);
    ep += 4;
    put_int32(os, ep);
    ep += 4;
    *ep++ = pid_creation(pid);
    return ep;
}

static byte*
dec_hashed_atom(DistEntry *dep, byte *ep, Eterm* objp)
{
    if (!dep)
      *objp = am_Underscore; /* This is for the first distribution message */
    else
      *objp = dep->cache->in_arr[get_int8(ep)];
    return ep+1;
}


static byte* 
dec_and_insert_hashed_atom(DistEntry *dep, byte *ep, Eterm* objp)
{
    Uint ix;
    Uint len;

    ix = get_int8(ep);
    ep++;
    len = get_int16(ep);
    ep += 2;
    if (!dep) /* This is for the first distribution message */
	*objp = am_atom_put((char*)ep,len);
    else
	*objp = dep->cache->in_arr[ix] = 
	    am_atom_put((char*)ep,len);
    return ep + len;
}

/* Expect an atom (cached or new) */
static byte*
dec_atom(DistEntry *dep, byte* ep, Eterm* objp)
{
    Uint len;

    switch (*ep++) {
    case ATOM_EXT:
	len = get_int16(ep),
	ep += 2;
	*objp = am_atom_put((char*)ep, len);
	return ep + len;

    case NEW_CACHE:
	if (dep != NULL && !(dep->flags & DFLAG_ATOM_CACHE)) {
	    goto error;
	}
	return dec_and_insert_hashed_atom(dep,ep,objp); 

    case CACHED_ATOM:
	if (dep != NULL && !(dep->flags & DFLAG_ATOM_CACHE)) {
	    goto error;
	}
	return dec_hashed_atom(dep,ep,objp);

    default:
    error:
	*objp = NIL;	/* Don't leave a hole in the heap */
	return NULL;
    }
}

static byte*
dec_pid(DistEntry *dep, Eterm** hpp, byte* ep, ErlOffHeap* off_heap, Eterm* objp)
{
    Eterm sysname;
    Uint data;
    Uint num;
    Uint ser;
    Uint cre;
    ErlNode *node;

    *objp = NIL;		/* In case we fail, don't leave a hole in the heap */

    /* eat first atom */
    if ((ep = dec_atom(dep, ep, &sysname)) == NULL)
	return NULL;
    num = get_int32(ep);
    ep += 4;
    if (num > ERTS_MAX_PID_NUMBER)
	return NULL;
    ser = get_int32(ep);
    ep += 4;
    if (ser > ERTS_MAX_PID_SERIAL)
	return NULL;
    if ((cre = get_int8(ep)) >= MAX_CREATION)
	return NULL;
    ep += 1;

    /*
     * We are careful to create the node entry only after all
     * validity tests are done.
     */
    cre = dec_set_creation(sysname,cre);
    node = erts_find_or_insert_node(sysname,cre);

    data = make_pid_data(ser, num);
    if(node == erts_this_node) {
	*objp = make_internal_pid(data);
    } else {
	ExternalThing *etp = (ExternalThing *) *hpp;
	*hpp += EXTERNAL_THING_HEAD_SIZE + 1;

	etp->header = make_external_pid_header(1);
	etp->next = off_heap->externals;
	etp->node = node;
	etp->data[0] = data;

	off_heap->externals = etp;
	*objp = make_external_pid(etp);
    }
    return ep;
}


static byte*
enc_term(DistEntry *dep, Eterm obj, byte* ep, Uint32 dflags)
{
    Uint n;
    Uint i;
    Uint j;
    Uint* ptr;
    FloatDef f;
    byte* back;

    switch(tag_val_def(obj)) {
    case NIL_DEF:
	*ep++ = NIL_EXT;   /* soley empty lists */
	return ep;

    case ATOM_DEF:
	return enc_atom(dep,obj,ep);

    case SMALL_DEF:
      {
	  Sint val = signed_val(obj);

	  if ((Uint)val < 256) {
	      *ep++ = SMALL_INTEGER_EXT;
	      put_int8(val, ep);
	      return ep + 1;
	  } else if (sizeof(Sint) == 4 || IS_SSMALL28(val)) {
	      *ep++ = INTEGER_EXT;
	      put_int32(val, ep);
	      return ep + 4;
	  } else {
	      Eterm tmp_big[2];
	      Eterm big = small_to_big(val, tmp_big);
	      *ep++ = SMALL_BIG_EXT;
	      n = big_bytes(big);
	      ASSERT(n < 256);
	      put_int8(n, ep);
	      ep += 1;
	      *ep++ = big_sign(big);
	      return big_to_bytes(big, ep);
	  }
      }

    case BIG_DEF:
	if ((n = big_bytes(obj)) < 256) {
	    *ep++ = SMALL_BIG_EXT;
	    put_int8(n, ep);
	    ep += 1;
	}
	else {
	    *ep++ = LARGE_BIG_EXT;
	    put_int32(n, ep);
	    ep += 4;
	}
	*ep++ = big_sign(obj);
	return big_to_bytes(obj, ep);

    case PID_DEF:
    case EXTERNAL_PID_DEF:
	return enc_pid(dep, obj, ep);

    case REF_DEF:
    case EXTERNAL_REF_DEF: {
	Uint32 *ref_num;

	ASSERT(dflags & DFLAG_EXTENDED_REFERENCES);
	*ep++ = NEW_REFERENCE_EXT;
	i = ref_no_of_numbers(obj);
	put_int16(i, ep);
	ep += 2;
	ep = enc_atom(dep,ref_node_name(obj),ep);
	*ep++ = ref_creation(obj);
	ref_num = ref_numbers(obj);
	for (j = 0; j < i; j++) {
	    put_int32(ref_num[j], ep);
	    ep += 4;
	}
	return ep;
    }
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:

	*ep++ = PORT_EXT;
	ep = enc_atom(dep,port_node_name(obj),ep);
	j = port_number(obj);
	put_int32(j, ep);
	ep += 4;
	*ep++ = port_creation(obj);
	return ep;

    case LIST_DEF:
	if ((i = is_string(obj)) && (i < MAX_STRING_LEN)) {
	    *ep++ = STRING_EXT;
	    put_int16(i, ep);
	    ep += 2;
	    while (is_list(obj)) {
		Eterm* cons = list_val(obj);
		*ep++ = unsigned_val(CAR(cons));
		obj = CDR(cons);
	    }
	    return ep;
	}
	*ep++ = LIST_EXT;
	back = ep;  /* leave length space */
	ep += 4;
	i = 0;
	while (is_list(obj)) {
	    Eterm* cons = list_val(obj);
	    i++;  /* count number of cons cells */
	    if ((ep = enc_term(dep, CAR(cons), ep, dflags)) == NULL)
		return NULL;
	    obj = CDR(cons);
	}
	if ((ep = enc_term(dep, obj, ep, dflags)) == NULL)
	    return NULL;
	put_int32(i, back);
	return ep;

    case TUPLE_DEF:
	ptr = tuple_val(obj);
	i = arityval(*ptr);
	ptr++;
	if (i <= 0xff) {
	    *ep++ = SMALL_TUPLE_EXT;
	    put_int8(i, ep);
	    ep += 1;
	}
	else  {
	    *ep++ = LARGE_TUPLE_EXT;
	    put_int32(i, ep);
	    ep += 4;
	}
	while(i--) {
	    if ((ep = enc_term(dep, *ptr++, ep, dflags)) == NULL)
		return NULL;
	}
	return ep;

    case FLOAT_DEF:
	GET_DOUBLE(obj, f);
	if (dflags & DFLAG_NEW_FLOATS) {
	    *ep++ = NEW_FLOAT_EXT;
#ifdef WORDS_BIGENDIAN
	    put_int32(f.fw[0], ep);
	    ep += 4;
	    put_int32(f.fw[1], ep);
#else
	    put_int32(f.fw[1], ep);
	    ep += 4;
	    put_int32(f.fw[0], ep);
#endif		
	    return ep+4;
	} else {
	    *ep++ = FLOAT_EXT;

	    /* now the sprintf which does the work */
	    i = sys_double_to_chars(f.fd, (char*) ep);

	    /* Don't leave garbage after the float!  (Bad practice in general,
	     * and Purify complains.)
	     */
	    sys_memset(ep+i, 0, 31-i);
	    return ep + 31;
	}

    case BINARY_DEF:
	{
	    Uint bitoffs;
	    Uint bitsize;
	    byte* bytes;

	    ERTS_GET_BINARY_BYTES(obj, bytes, bitoffs, bitsize);
	    if (bitsize == 0) {
		/* Plain old byte-sized binary. */
		*ep++ = BINARY_EXT;
		j = binary_size(obj);
		put_int32(j, ep);
		ep += 4;
		copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j);
		return ep + j;
	    } else if (dflags & DFLAG_BIT_BINARIES) {
		/* Bit-level binary. */
		*ep++ = BIT_BINARY_EXT;
		j = binary_size(obj);
		put_int32((j+1), ep);
		ep += 4;
		*ep++ = bitsize;
		ep[j] = 0;	/* Zero unused bits at end of binary */
		copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j+bitsize);
		return ep + j + 1;
	    } else {
		/*
		 * Bit-level binary, but the receiver doesn't support it.
		 * Build a tuple instead.
		 */
		*ep++ = SMALL_TUPLE_EXT;
		*ep++ = 2;
		*ep++ = BINARY_EXT;
		j = binary_size(obj);
		put_int32((j+1), ep);
		ep += 4;
		ep[j] = 0;	/* Zero unused bits at end of binary */
		copy_binary_to_buffer(ep, 0, bytes, bitoffs, 8*j+bitsize);
		ep += j+1;
		*ep++ = SMALL_INTEGER_EXT;
		*ep++ = bitsize;
		return ep;
	    }
	}
    case EXPORT_DEF:
	{
	    Export* exp = (Export *) (export_val(obj))[1];
	    if ((dflags & DFLAG_EXPORT_PTR_TAG) != 0) {
		*ep++ = EXPORT_EXT;
		ep = enc_atom(dep, exp->code[0], ep);
		ep = enc_atom(dep, exp->code[1], ep);
		return enc_term(dep, make_small(exp->code[2]), ep, dflags);
	    } else {
		/* Tag, arity */
		*ep++ = SMALL_TUPLE_EXT;
		put_int8(2, ep);
		ep += 1;

		/* Module name */
		ep = enc_atom(dep, exp->code[0], ep);

		/* Function name */
		return enc_atom(dep, exp->code[1], ep);
	    }
	}
	break;
    case FUN_DEF:
	if ((dflags & DFLAG_NEW_FUN_TAGS) != 0) {
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    Uint num_free = funp->num_free;
	    byte* size_p;

	    *ep++ = NEW_FUN_EXT;
	    size_p = ep;
	    ep += 4;
	    *ep = funp->arity;
	    ep += 1;
	    sys_memcpy(ep, funp->fe->uniq, 16);
	    ep += 16;
	    put_int32(funp->fe->index, ep);
	    ep += 4;
	    put_int32(num_free, ep);
	    ep += 4;
	    ep = enc_atom(dep, funp->fe->module, ep);
	    ep = enc_term(dep, make_small(funp->fe->old_index), ep, dflags);
	    ep = enc_term(dep, make_small(funp->fe->old_uniq), ep, dflags);
	    ep = enc_pid(dep, funp->creator, ep);
	    ptr = funp->env;
	    while (num_free-- > 0) {
		if ((ep = enc_term(dep, *ptr++, ep, dflags)) == NULL) {
		    return NULL;
		}
	    }
	    put_int32(ep-size_p, size_p);
	} else if ((dflags & DFLAG_FUN_TAGS) != 0) {
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    Uint num_free = funp->num_free;

	    *ep++ = FUN_EXT;
	    put_int32(num_free, ep);
	    ep += 4;
	    ep = enc_pid(dep, funp->creator, ep);
	    ep = enc_atom(dep, funp->fe->module, ep);
	    ep = enc_term(dep, make_small(funp->fe->old_index), ep, dflags);
	    ep = enc_term(dep, make_small(funp->fe->old_uniq), ep, dflags);
	    ptr = funp->env;
	    while (num_free-- > 0) {
		if ((ep = enc_term(dep, *ptr++, ep, dflags)) == NULL) {
		    return NULL;
		}
	    }
	} else {
	    /*
	     * Communicating with an obsolete erl_interface or jinterface node.
	     * Convert the fun to a tuple to avoid crasching.
	     */
		
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
		
	    /* Tag, arity */
	    *ep++ = SMALL_TUPLE_EXT;
	    put_int8(5, ep);
	    ep += 1;
		
	    /* 'fun' */
	    ep = enc_atom(dep, am_fun, ep);
		
	    /* Module name */
	    ep = enc_atom(dep, funp->fe->module, ep);
		
	    /* Index, Uniq */
	    *ep++ = INTEGER_EXT;
	    put_int32(funp->fe->old_index, ep);
	    ep += 4;
	    *ep++ = INTEGER_EXT;
	    put_int32(unsigned_val(funp->fe->old_uniq), ep);
	    ep += 4;
		
	    /* Environment sub-tuple arity */
	    if (funp->num_free <= 0xff) {
		*ep++ = SMALL_TUPLE_EXT;
		put_int8(funp->num_free, ep);
		ep += 1;
	    } else {
		*ep++ = LARGE_TUPLE_EXT;
		put_int32(funp->num_free, ep);
		ep += 4;
	    }
		
	    /* Environment tuple */
	    for (i = 0; i < funp->num_free; i++) {
		if ((ep = enc_term(dep, funp->env[i], ep, dflags)) == NULL) {
		    return NULL;
		}
	    }
	} 	    
	return ep;
    }
    return NULL;
}

/* This function is written in this strange way because in dist.c
   we call it twice for some messages, First we insert a control message 
   in the buffer , and then we insert the actual message in the buffer
   immediataly following the control message. If the real (2) message
   makes the buffer owerflow, we must not destroy the work we have already
   done, i.e we must not forget to copy the encoding of the 
   control message as well, (If there is one)
   
   If the caller of erts_to_external_format() passes a pointer to a
   DistEntry, it also has to pass pointers to the buffer pointer and
   the buffer size of the buffer to encode in. The buffer has to be
   allocated with the ERTS_ALC_T_TMP_DIST_BUF type. The caller is
   responsible for deallocating the buffer. If the buffer is to small,
   erts_to_external_format() will reallocate it. Ugly, but it works...

   We don't have the energy to describe the external format in words here.
   The code explains itself :-). However, All external encodings
   are predeeded with the special character VERSION_MAGIC, which makes
   it possible do distinguish encodings from incompatible erlang systems.
   Every time the external format is changed, This VERSION_MAGIC shall be 
   incremented.

*/

int
erts_to_external_format(DistEntry *dep, Eterm obj, byte **ext,
			byte **bufpp, Uint *bufszp)
{
    byte* ptr;
    unsigned dflags;

    ASSERT(ext);
    ptr = *ext;
    ASSERT(ptr);

    if (!dep)
	dflags = TERM_TO_BINARY_DFLAGS;
    else {
	Uint free_size;
	Uint size;

	dflags = dep->flags;

	ASSERT(bufpp && bufszp);
	ASSERT(*bufpp && *bufszp && *bufpp <= ptr && ptr < *bufpp + *bufszp);

	free_size = *bufszp - (ptr - *bufpp); 
	size = 50 /* ??? */ + encode_size_struct(obj, dflags);

	if (size > free_size) {
	    byte *bufp;

	    *bufszp += size - free_size;
	    bufp = erts_realloc(ERTS_ALC_T_TMP_DIST_BUF, *bufpp, *bufszp);

	    if (bufp != *bufpp) {
		ptr = *ext = bufp + (ptr - *bufpp);
		*bufpp = bufp;
	    }
	}
    }

    *ptr++ = VERSION_MAGIC;
    if ((ptr = enc_term(dep, obj, ptr, dflags)) == NULL)
	erl_exit(1, "Internal data structure error "
		 "(in erts_to_external_format())\n");
    *ext = ptr;
    return 0;
}

/*
** hpp is set to either a &p->htop or
** a pointer to a memory pointer (form message buffers)
** on return hpp is updated to point after allocated data
*/
Eterm
erts_from_external_format(DistEntry *dep,
			  Eterm** hpp,
			  byte **ext,
			  ErlOffHeap* off_heap)
{
    Eterm obj;
    byte* ep = *ext;

    if (*ep++ != VERSION_MAGIC) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	if (dep)
	    erts_dsprintf(dsbufp,
			  "** Got message from noncompatible erlang on "
			  "channel %d\n",
			  dist_entry_channel_no(dep));
	else 
	    erts_dsprintf(dsbufp,
			  "** Attempt to convert old non compatible "
			  "binary %d\n",
			  *ep);
	erts_send_error_to_logger_nogl(dsbufp);
	return THE_NON_VALUE;
    }
    if ((ep = dec_term(dep, hpp, ep, off_heap, &obj)) == NULL) {
	if (dep) {	/* Don't print this for binary_to_term */
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Got corrupted message on channel %d\n",
			  dist_entry_channel_no(dep));
	    erts_send_error_to_logger_nogl(dsbufp);
	}
#ifdef DEBUG
	bin_write(ERTS_PRINT_STDERR,NULL,*ext,500);
#endif
	return THE_NON_VALUE;
    }
    *ext = ep;
    return obj;
}

static byte*
dec_term(DistEntry *dep, Eterm** hpp, byte* ep, ErlOffHeap* off_heap, Eterm* objp)
{
    int n;
    register Eterm* hp = *hpp;	/* Please don't take the address of hp */
    Eterm* next = objp;

    *next = (Eterm) NULL;

    while (next != NULL) {
	objp = next;
	next = (Eterm *) (*objp);

	switch (*ep++) {
	case INTEGER_EXT:
	    {
		Sint sn = get_int32(ep);

		ep += 4;
		if (MY_IS_SSMALL(sn)) {
		    *objp = make_small(sn);
		} else {
		    *objp = small_to_big(sn, hp);
		    hp += BIG_UINT_HEAP_SIZE;
		}
		break;
	    }
	case SMALL_INTEGER_EXT:
	    n = get_int8(ep);
	    ep++;
	    *objp = make_small(n);
	    break;
	case SMALL_BIG_EXT:
	    n = get_int8(ep);
	    ep++;
	    goto big_loop;
	case LARGE_BIG_EXT:
	    n = get_int32(ep);
	    ep += 4;
	big_loop:
	    {
		Eterm big;
		byte* first;
		byte* last;
		Uint neg;

		neg = get_int8(ep); /* Sign bit */
		ep++;

		/*
		 * Strip away leading zeroes to avoid creating illegal bignums.
		 */
		first = ep;
		last = ep + n;
		ep += n;
		do {
		    --last;
		} while (first <= last && *last == 0);

		if ((n = last - first + 1) == 0) {
		    /* Zero width bignum defaults to zero */
		    big = make_small(0);
		} else {
		    big = bytes_to_big(first, n, neg, hp);
		    if (is_big(big)) {
			hp += big_arity(big) + 1;
		    }
		}
		*objp = big;
		break;
	    }
	case NEW_CACHE:
	    if (dep != NULL && !(dep->flags & DFLAG_ATOM_CACHE)) {
		goto error;
	    }
	    ep = dec_and_insert_hashed_atom(dep, ep, objp);
	    break;
	case CACHED_ATOM:
	    if (dep != NULL && !(dep->flags & DFLAG_ATOM_CACHE)) {
		goto error;
	    }
	    ep = dec_hashed_atom(dep, ep, objp);
	    break;
	case ATOM_EXT:
	    n = get_int16(ep);
	    ep += 2;
	    *objp = am_atom_put((char*)ep, n);
	    ep += n;
	    break;
	case LARGE_TUPLE_EXT:
	    n = get_int32(ep);
	    ep += 4;
	    goto tuple_loop;
	case SMALL_TUPLE_EXT:
	    n = get_int8(ep);
	    ep++;
	tuple_loop:
	    *objp = make_tuple(hp);
	    *hp++ = make_arityval(n);
	    hp += n;
	    objp = hp - 1;
	    while (n-- > 0) {
		objp[0] = (Eterm) next;
		next = objp;
		objp--;
	    }
	    break;
	case NIL_EXT:
	    *objp = NIL;
	    break;
	case LIST_EXT:
	    n = get_int32(ep);
	    ep += 4;
	    if (n == 0) {
		*objp = NIL;
		break;
	    }
	    *objp = make_list(hp);
	    hp += 2*n;
	    objp = hp - 2;
	    objp[0] = (Eterm) (objp+1);
	    objp[1] = (Eterm) next;
	    next = objp;
	    objp -= 2;
	    while (--n > 0) {
		objp[0] = (Eterm) next;
		objp[1] = make_list(objp + 2);
		next = objp;
		objp -= 2;
	    }
	    break;
	case STRING_EXT:
	    n = get_int16(ep);
	    ep += 2;
	    if (n == 0) {
		*objp = NIL;
		break;
	    }
	    *objp = make_list(hp);
	    while (n-- > 0) {
		hp[0] = make_small(*ep++);
		hp[1] = make_list(hp+2);
		hp += 2;
	    }
	    hp[-1] = NIL;
	    break;
	case FLOAT_EXT:
	    {
		FloatDef ff;

		if (sys_chars_to_double((char*)ep, &ff.fd) != 0) {
		    goto error;
		}
		ep += 31;
		*objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case NEW_FLOAT_EXT:
	    {
		FloatDef ff;
		volatile int *fpexnp = erts_get_current_fp_exception();

#ifdef WORDS_BIGENDIAN
		ff.fw[0] = get_int32(ep);
		ep += 4;
		ff.fw[1] = get_int32(ep);
		ep += 4;
#else
		ff.fw[1] = get_int32(ep);
		ep += 4;
		ff.fw[0] = get_int32(ep);
		ep += 4;
#endif		
		__ERTS_FP_CHECK_INIT(fpexnp);
		__ERTS_FP_ERROR_THOROUGH(fpexnp, ff.fd, goto error);
		*objp = make_float(hp);
		PUT_DOUBLE(ff, hp);
		hp += FLOAT_SIZE_OBJECT;
		break;
	    }
	case PID_EXT:
	    *hpp = hp;
	    ep = dec_pid(dep, hpp, ep, off_heap, objp);
	    hp = *hpp;
	    if (ep == NULL) {
		return NULL;
	    }
	    break;
	case PORT_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		Uint num;
		Uint cre;

		if ((ep = dec_atom(dep, ep, &sysname)) == NULL) {
		    goto error;
		}
		if ((num = get_int32(ep)) > ERTS_MAX_PORT_NUMBER) {
		    goto error;
		}
		ep += 4;
		if ((cre = get_int8(ep)) >= MAX_CREATION) {
		    goto error;
		}
		ep++;
		cre = dec_set_creation(sysname,cre);
		node = erts_find_or_insert_node(sysname, cre);

		if(node == erts_this_node) {
		    *objp = make_internal_port(num);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
		    hp += EXTERNAL_THING_HEAD_SIZE + 1;
		    
		    etp->header = make_external_port_header(1);
		    etp->next = off_heap->externals;
		    etp->node = node;
		    etp->data[0] = num;

		    off_heap->externals = etp;
		    *objp = make_external_port(etp);
		}

		break;
	    }
	case REFERENCE_EXT:
	    {
		Eterm sysname;
		ErlNode *node;
		int i;
		Uint cre;
		Uint32 *ref_num;
		Uint32 r0;
		Uint ref_words;

		ref_words = 1;

		if ((ep = dec_atom(dep,ep, &sysname)) == NULL)
		    goto error;
		if ((r0 = get_int32(ep)) >= MAX_REFERENCE )
		    goto error;
		ep += 4;

		if ((cre = get_int8(ep)) >= MAX_CREATION)
		    goto error;
		ep += 1;
		goto ref_ext_common;

	    case NEW_REFERENCE_EXT:

		ref_words = get_int16(ep);
		ep += 2;

		if (ref_words > ERTS_MAX_REF_NUMBERS)
		    goto error;

		if ((ep = dec_atom(dep, ep, &sysname)) == NULL)
		    goto error;

		if ((cre = get_int8(ep)) >= MAX_CREATION)
		    goto error;
		ep += 1;

		r0 = get_int32(ep);
		ep += 4;
		if (r0 >= MAX_REFERENCE)
		    goto error;

	    ref_ext_common:

		cre = dec_set_creation(sysname, cre);
		node = erts_find_or_insert_node(sysname, cre);
		if(node == erts_this_node) {
		    RefThing *rtp = (RefThing *) hp;
		    hp += REF_THING_HEAD_SIZE;
#ifdef ARCH_64
		    rtp->header = make_ref_thing_header(ref_words/2 + 1);
#else
		    rtp->header = make_ref_thing_header(ref_words);
#endif
		    *objp = make_internal_ref(rtp);
		}
		else {
		    ExternalThing *etp = (ExternalThing *) hp;
		    hp += EXTERNAL_THING_HEAD_SIZE;
		    
#ifdef ARCH_64
		    etp->header = make_external_ref_header(ref_words/2 + 1);
#else
		    etp->header = make_external_ref_header(ref_words);
#endif
		    etp->next = off_heap->externals;
		    etp->node = node;

		    off_heap->externals = etp;
		    *objp = make_external_ref(etp);
		}

		ref_num = (Uint32 *) hp;
#ifdef ARCH_64
		*(ref_num++) = ref_words /* 32-bit arity */;
#endif
		ref_num[0] = r0;
		for(i = 1; i < ref_words; i++) {
		    ref_num[i] = get_int32(ep);
		    ep += 4;
		}
#ifdef ARCH_64
		if ((1 + ref_words) % 2)
		    ref_num[ref_words] = 0;
		hp += ref_words/2 + 1;
#else
		hp += ref_words;
#endif
		break;
	    }
	case BINARY_EXT:
	    {
		n = get_int32(ep);
		ep += 4;
	    
		if (n <= ERL_ONHEAP_BIN_LIMIT || off_heap == NULL) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    hp += heap_bin_size(n);
		    sys_memcpy(hb->data, ep, n);
		    *objp = make_binary(hb);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;
		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    sys_memcpy(dbin->orig_bytes, ep, n);
		    pb = (ProcBin *) hp;
		    hp += PROC_BIN_SIZE;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->mso;
		    off_heap->mso = pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
		    *objp = make_binary(pb);
		}
		ep += n;
		break;
	    }
	case BIT_BINARY_EXT:
	    {
		Eterm bin;
		ErlSubBin* sb;
		Uint bitsize;

		n = get_int32(ep);
		bitsize = ep[4];
		ep += 5;
		if (n <= ERL_ONHEAP_BIN_LIMIT || off_heap == NULL) {
		    ErlHeapBin* hb = (ErlHeapBin *) hp;

		    hb->thing_word = header_heap_bin(n);
		    hb->size = n;
		    sys_memcpy(hb->data, ep, n);
		    bin = make_binary(hb);
		    hp += heap_bin_size(n);
		} else {
		    Binary* dbin = erts_bin_nrml_alloc(n);
		    ProcBin* pb;
		    dbin->flags = 0;
		    dbin->orig_size = n;
		    erts_refc_init(&dbin->refc, 1);
		    sys_memcpy(dbin->orig_bytes, ep, n);
		    pb = (ProcBin *) hp;
		    pb->thing_word = HEADER_PROC_BIN;
		    pb->size = n;
		    pb->next = off_heap->mso;
		    off_heap->mso = pb;
		    pb->val = dbin;
		    pb->bytes = (byte*) dbin->orig_bytes;
		    pb->flags = 0;
		    bin = make_binary(pb);
		    hp += PROC_BIN_SIZE;
		}
		ep += n;
		if (bitsize == 0) {
		    *objp = bin;
		} else {
		    sb = (ErlSubBin *) hp;
		    sb->thing_word = HEADER_SUB_BIN;
		    sb->orig = bin;
		    sb->size = n - 1;
		    sb->bitsize = bitsize;
		    sb->bitoffs = 0;
		    sb->offs = 0;
		    sb->is_writable = 0;
		    *objp = make_binary(sb);
		    hp += ERL_SUB_BIN_SIZE;
		}
		break;
	    }
	case EXPORT_EXT:
	    {
		Eterm mod;
		Eterm name;
		Eterm temp;
		Sint arity;

		ep = dec_atom(dep, ep, &mod);
		ep = dec_atom(dep, ep, &name);
		*hpp = hp;
		ep = dec_term(dep, hpp, ep, off_heap, &temp);
		hp = *hpp;
		if (ep == NULL) {
		    return NULL;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		arity = signed_val(temp);
		if (arity < 0) {
		    goto error;
		}
		*objp = make_export(hp);
		*hp++ = HEADER_EXPORT;
		*hp++ = (Eterm) erts_export_get_or_make_stub(mod, name, arity);
		break;
	    }
	    break;
	case NEW_FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Uint arity;
		Eterm module;
		byte* uniq;
		int index;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm* temp_hp;
		Eterm** hpp = &temp_hp;
		Eterm temp;

		ep += 4;	/* Skip total size in bytes */
		arity = *ep++;
		uniq = ep;
		ep += 16;
		index = get_int32(ep);
		ep += 4;
		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		if (num_free > 0) {
		    /* Don't leave a hole in case we fail */
		    *hp = make_pos_bignum_header(num_free-1);
		}
		hp += num_free;
		*hpp = hp;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
		funp->creator = NIL; /* Don't leave a hole in case we fail */
		*objp = make_fun(funp);

		/* Module */
		if ((ep = dec_atom(dep, ep, &temp)) == NULL) {
		    goto error;
		}
		module = temp;

		/* Index */
		if ((ep = dec_term(dep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		/* Uniq */
		if ((ep = dec_term(dep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_uniq = unsigned_val(temp);

#ifndef HYBRID /* FIND ME! */
		/*
		 * It is safe to link the fun into the fun list only when
		 * no more validity tests can fail.
		 */
		funp->next = off_heap->funs;
		off_heap->funs = funp;
#endif

		funp->fe = erts_put_fun_entry2(module, old_uniq, old_index,
					       uniq, index, arity);
		funp->arity = arity;
#ifdef HIPE
		if (funp->fe->native_address == NULL) {
		    hipe_set_closure_stub(funp->fe, num_free);
		}
		funp->native_address = funp->fe->native_address;
#endif
		hp = *hpp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) next;
		    next = funp->env + i;
		}
		/* Creator */
		funp->creator = (Eterm) next;
		next = &(funp->creator);
		break;
	    }
	case FUN_EXT:
	    {
		ErlFunThing* funp = (ErlFunThing *) hp;
		Eterm module;
		Sint old_uniq;
		Sint old_index;
		unsigned num_free;
		int i;
		Eterm* temp_hp;
		Eterm** hpp = &temp_hp;
		Eterm temp;

		num_free = get_int32(ep);
		ep += 4;
		hp += ERL_FUN_SIZE;
		if (num_free > 0) {
		    /* Don't leave a hole in the heap in case we fail. */
		    *hp = make_pos_bignum_header(num_free-1);
		}
		hp += num_free;
		*hpp = hp;
		funp->thing_word = HEADER_FUN;
		funp->num_free = num_free;
		*objp = make_fun(funp);

		/* Creator pid */
		switch(*ep) {
		case PID_EXT:
		    ep = dec_pid(dep, hpp, ++ep, off_heap, &funp->creator);
		    if (ep == NULL) {
			funp->creator = NIL; /* Don't leave a hole in the heap */
			goto error;
		    }
		    break;
		default:
		    goto error;
		}

		/* Module */
		if ((ep = dec_atom(dep, ep, &temp)) == NULL) {
		    goto error;
		}
		module = temp;

		/* Index */
		if ((ep = dec_term(dep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		old_index = unsigned_val(temp);

		/* Uniq */
		if ((ep = dec_term(dep, hpp, ep, off_heap, &temp)) == NULL) {
		    goto error;
		}
		if (!is_small(temp)) {
		    goto error;
		}
		
#ifndef HYBRID /* FIND ME! */
		/*
		 * It is safe to link the fun into the fun list only when
		 * no more validity tests can fail.
		 */
		funp->next = off_heap->funs;
		off_heap->funs = funp;
#endif

		old_uniq = unsigned_val(temp);

		funp->fe = erts_put_fun_entry(module, old_uniq, old_index);
		funp->arity = funp->fe->address[-1] - num_free;
#ifdef HIPE
		funp->native_address = funp->fe->native_address;
#endif
		hp = *hpp;

		/* Environment */
		for (i = num_free-1; i >= 0; i--) {
		    funp->env[i] = (Eterm) next;
		    next = funp->env + i;
		}
		break;
	    }
	default:
	error:
	    /*
	     * Be careful to return the updated heap pointer, to avoid
	     * that the caller wipes out binaries or other off-heap objects
	     * that may have been linked into the process.
	     */
	    *hpp = hp;
	    return NULL;
	}
    }
    *hpp = hp;
    return ep;
}

/* returns the number of bytes needed to encode an object
   to a sequence of bytes
   N.B. That this must agree with to_external2() above!!!
   (except for cached atoms) */

Uint
encode_size_struct(Eterm obj, unsigned dflags)
{
    return (1 + encode_size_struct2(obj, dflags));
				/* 1 for the VERSION_MAGIC */
}

static Uint
encode_size_struct2(Eterm obj, unsigned dflags)
{
    Uint m,i, arity, *nobj, sum;

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	return 1;
    case ATOM_DEF:
	/* Make sure NEW_CACHE ix l1 l0 a1 a2 .. an fits */
	return (1 + 1 + 2 + atom_tab(atom_val(obj))->len);
    case SMALL_DEF:
      {
	  Sint val = signed_val(obj);

	  if ((Uint)val < 256)
	      return 1 + 1;		/* SMALL_INTEGER_EXT */
	  else if (sizeof(Sint) == 4 || IS_SSMALL28(val))
	      return 1 + 4;		/* INTEGER_EXT */
	  else {
	      Eterm tmp_big[2];
	      i = big_bytes(small_to_big(val, tmp_big));
	      return 1 + 1 + 1 + i;	/* SMALL_BIG_EXT */
	  }
      }
    case BIG_DEF:
	if ((i = big_bytes(obj)) < 256)
	    return 1 + 1 + 1 + i;  /* tag,size,sign,digits */
	else
	    return 1 + 4 + 1 + i;  /* tag,size,sign,digits */
    case PID_DEF:
    case EXTERNAL_PID_DEF:
	return (1 + encode_size_struct2(pid_node_name(obj), dflags)
		+ 4 + 4 + 1);
    case REF_DEF:
    case EXTERNAL_REF_DEF:
	ASSERT(dflags & DFLAG_EXTENDED_REFERENCES);
	i = ref_no_of_numbers(obj);
	return (1 + 2 + encode_size_struct2(ref_node_name(obj), dflags)
		+ 1 + 4*i);
    case PORT_DEF:
    case EXTERNAL_PORT_DEF:
	return (1 + encode_size_struct2(port_node_name(obj), dflags)
		+ 4 + 1);
    case LIST_DEF:
	if ((m = is_string(obj)) && (m < MAX_STRING_LEN))
	    return m + 2 + 1;
	nobj = list_val(obj);
	sum = 5;
	while (1) {
	    sum += encode_size_struct2(*nobj++, dflags);
	    if (is_not_list(*nobj)) {
		if (is_nil(*nobj))
		    return sum + 1;
		return(sum + encode_size_struct2(*nobj, dflags));
	    }
	    nobj = list_val(*nobj);
	}
    case TUPLE_DEF:
	arity = arityval(*(tuple_val(obj)));
	if (arity <= 0xff) 
	    sum = 1 + 1;
	else
	    sum = 1 + 4;
	for (i = 0; i < arity; i++)
	    sum += encode_size_struct2(*(tuple_val(obj) + i + 1), dflags);
	return sum;
    case FLOAT_DEF:
	if (dflags & DFLAG_NEW_FLOATS) {
	    return 9;
	} else {
	    return 32;   /* Yes, including the tag */
	}
    case BINARY_DEF:
	return 1 + 4 + binary_size(obj) +
	    5;			/* For unaligned binary */
    case FUN_DEF:
	if ((dflags & DFLAG_NEW_FUN_TAGS) != 0) {
	    sum = 20+1+1+4;	/* New ID + Tag */
	    goto fun_size;
	} else if ((dflags & DFLAG_FUN_TAGS) != 0) {
	    sum = 1;		/* Tag */
	    goto fun_size;
	} else {
	    /*
	     * Size when fun is mapped to a tuple (to avoid crashing).
	     */

	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);

	    sum = 1 + 1;	/* Tuple tag, arity */
	    sum += 1 + 1 + 2 + atom_tab(atom_val(am_fun))->len; /* 'fun' */
	    sum += 1 + 1 + 2 +
		atom_tab(atom_val(funp->fe->module))->len; /* Module name */
	    sum += 2 * (1 + 4); /* Index + Uniq */

	    /*
	     * The free variables are a encoded as a sub tuple.
	     */
	    sum += 1 + (funp->num_free < 0x100 ? 1 : 4);
	    for (i = 0; i < funp->num_free; i++) {
		sum += encode_size_struct2(funp->env[i], dflags);
	    }
	    return sum;
	}
    
    fun_size:
	{
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    sum += 4;		/* Length field (number of free variables */
	    sum += encode_size_struct2(funp->creator, dflags);
	    sum += encode_size_struct2(funp->fe->module, dflags);
	    sum += 2 * (1+4);	/* Index, Uniq */
	    for (i = 0; i < funp->num_free; i++) {
		sum += encode_size_struct2(funp->env[i], dflags);
	    }
	    return sum;
	} 

    case EXPORT_DEF:
	{
	    Export* ep = (Export *) (export_val(obj))[1];
	    sum = 1;
	    sum += encode_size_struct2(ep->code[0], dflags);
	    sum += encode_size_struct2(ep->code[1], dflags);
	    sum += encode_size_struct2(make_small(ep->code[2]), dflags);
	    return sum;
	}
	return 0;

    default:
	erl_exit(1,"Internal data structure error (in encode_size_struct2)%x\n",
		 obj);
    }
    return -1; /* Pedantic (lint does not know about erl_exit) */
}

static Sint
decoded_size(byte *ep, byte* endp, int no_refc_bins)
{
    int heap_size = 0;
    int terms = 1;
    int atom_extra_skip = 0;
    Uint n;
    DECLARE_ESTACK(s);

#define SKIP(sz)				\
    do {					\
	if ((sz) <= endp-ep) {			\
	    ep += (sz);				\
        } else {return -1; };			\
    } while (0)

#define SKIP2(sz1, sz2)				\
    do {					\
	Uint sz = (sz1) + (sz2);		\
	if (sz1 < sz && (sz) <= endp-ep) {	\
	    ep += (sz);				\
        } else {return -1; }			\
    } while (0)

#define CHKSIZE(sz)				\
    do {					\
	 if ((sz) > endp-ep) { return -1; }	\
    } while (0)

    for (;;) {
	while (terms-- > 0) {
	    int tag;

	    CHKSIZE(1);
	    tag = ep++[0];
	    switch (tag) {
	    case INTEGER_EXT:
		SKIP(4);
		heap_size += BIG_UINT_HEAP_SIZE;
		break;
	    case SMALL_INTEGER_EXT:
		SKIP(1);
		break;
	    case SMALL_BIG_EXT:
		CHKSIZE(1);
		n = ep[0];		/* number of bytes */
		SKIP2(n, 1+1);		/* skip size,sign,digits */
		heap_size += 1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
		break;
	    case LARGE_BIG_EXT:
		CHKSIZE(4);
		n = (ep[0] << 24) | (ep[1] << 16) | (ep[2] << 8) | ep[3];
		SKIP2(n,4+1);		/* skip, size,sign,digits */
		heap_size += 1+1+(n+sizeof(Eterm)-1)/sizeof(Eterm); /* XXX: 1 too much? */
		break;
	    case ATOM_EXT:
		CHKSIZE(2);
		n = (ep[0] << 8) | ep[1];
		if (n > MAX_ATOM_LENGTH) {
		    return -1;
		}
		SKIP(n+2+atom_extra_skip);
		atom_extra_skip = 0;
		break;
	    case NEW_CACHE:
		CHKSIZE(3);
		n = get_int16(ep+1);
		if (n > MAX_ATOM_LENGTH) {
		    return -1;
		}
		SKIP(n+3+atom_extra_skip);
		atom_extra_skip = 0;
		break;
	    case CACHED_ATOM:
		SKIP(1+atom_extra_skip);
		atom_extra_skip = 0;
		break;
	    case PID_EXT:
		atom_extra_skip = 9;
		 /* In case it is an external pid */
		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
		terms++;
		break;
	    case PORT_EXT:
		atom_extra_skip = 5;
		 /* In case it is an external port */
		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
		terms++;
		break;
	    case NEW_REFERENCE_EXT:
		{
		    int id_words;

		    CHKSIZE(2);
		    id_words = get_int16(ep);
		    
		    if (id_words > ERTS_MAX_REF_NUMBERS)
			return -1;

		    ep += 2;
		    atom_extra_skip = 1 + 4*id_words;
		    /* In case it is an external ref */
#ifdef ARCH_64
		    heap_size += EXTERNAL_THING_HEAD_SIZE + id_words/2 + 1;
#else
		    heap_size += EXTERNAL_THING_HEAD_SIZE + id_words;
#endif
		    terms++;
		    break;
		}
	    case REFERENCE_EXT:
		/* In case it is an external ref */
		heap_size += EXTERNAL_THING_HEAD_SIZE + 1;
		atom_extra_skip = 5;
		terms++;
		break;
	    case NIL_EXT:
		break;
	    case LIST_EXT:
		CHKSIZE(4);
		ESTACK_PUSH(s, terms);
		terms = (ep[0] << 24) | (ep[1] << 16) | (ep[2] << 8) | ep[3];
		terms++;
		ep += 4;
		heap_size += 2 * terms;
		break;
	    case SMALL_TUPLE_EXT:
		CHKSIZE(1);
		ESTACK_PUSH(s, terms);
		terms = *ep++;
		heap_size += terms + 1;
		break;
	    case LARGE_TUPLE_EXT:
		CHKSIZE(4);
		ESTACK_PUSH(s, terms);
		terms = (ep[0] << 24) | (ep[1] << 16) | (ep[2] << 8) | ep[3];
		ep += 4;
		heap_size += terms + 1;
		break;
	    case STRING_EXT:
		CHKSIZE(2);
		n = (ep[0] << 8) | ep[1];
		SKIP(n+2);
		heap_size += 2 * n;
		break;
	    case FLOAT_EXT:
		SKIP(31);
		heap_size += FLOAT_SIZE_OBJECT;
		break;
	    case NEW_FLOAT_EXT:
		SKIP(8);
		heap_size += FLOAT_SIZE_OBJECT;
		break;
	    case BINARY_EXT:
		CHKSIZE(4);
		n = (ep[0] << 24) | (ep[1] << 16) | (ep[2] << 8) | ep[3];
		SKIP2(n, 4);
		if (n <= ERL_ONHEAP_BIN_LIMIT || no_refc_bins) {
		    heap_size += heap_bin_size(n);
		} else {
		    heap_size += PROC_BIN_SIZE;
		}
		break;
	    case BIT_BINARY_EXT:
		{
		    CHKSIZE(5);
		    n = (ep[0] << 24) | (ep[1] << 16) | (ep[2] << 8) | ep[3];
		    SKIP2(n, 5);
		    if (n <= ERL_ONHEAP_BIN_LIMIT || no_refc_bins) {
			heap_size += heap_bin_size(n) + ERL_SUB_BIN_SIZE;
		    } else {
			heap_size += PROC_BIN_SIZE + ERL_SUB_BIN_SIZE;
		    }
		}
		break;
	    case EXPORT_EXT:
		ESTACK_PUSH(s, terms);
		terms = 3;
		heap_size += 2;
		break;
	    case NEW_FUN_EXT:
		{
		    int num_free;
		    Uint total_size;

		    CHKSIZE(4);
		    total_size = get_int32(ep);
		    CHKSIZE(total_size);
		    CHKSIZE(20+1+4);
		    ep += 20+1+4;
		    /*FALLTHROUGH*/

		case FUN_EXT:
		    CHKSIZE(4);
		    num_free = get_int32(ep);
		    ep += 4;
		    ESTACK_PUSH(s, terms);
		    terms = 4 + num_free;
		    heap_size += ERL_FUN_SIZE + num_free;
		    break;
		}
	    default:
		return -1;
	    }
	}
	if (ESTACK_ISEMPTY(s)) {
	    DESTROY_ESTACK(s);
	    return heap_size;
	}
	terms = ESTACK_POP(s);
    }
#undef SKIP
#undef SKIP2
#undef CHKSIZE
}

Sint
erts_decoded_size(byte* t, Uint size, int no_refc_bins)
{
    if (size > 0 && *t == VERSION_MAGIC) {
	t++;
    } else {
	return -1;
    }
    size--;
    return decoded_size(t, t+size, no_refc_bins);
}
