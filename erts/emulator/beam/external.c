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
#include "zlib.h"

#define MAX_STRING_LEN 0xffff
#define dec_set_creation(node,creat) \
(node == THIS_NODE && creat == this_creation) ? ORIG_CREATION : creat /* local pid */

/*
 * Setting the creation is tricky.  For foreign node names, we always
 * retain the creation.  If the nodename is the same as our own,
 * we must retain the creation of the pid if it has one (it must be
 * a previous incarnation of the current node, which another node has
 * passed to us).  Otherwise, if the creation is 0, we set the
 * creation to the current creation.
 */
#define pid_enc_set_creation(node,obj) \
(node == THIS_NODE && pid_creation(obj) == 0) ? this_creation : pid_creation(obj)

#define port_enc_set_creation(node,obj) \
(node == THIS_NODE && port_creation(obj) == 0) ? this_creation : port_creation(obj)

#define ref_enc_set_creation(node,obj) \
(node == THIS_NODE && ref_creation(obj) == 0) ? this_creation : ref_creation(obj)

extern int dist_buf_size;
extern byte *dist_buf;

static byte* enc_term(int, Eterm, byte*, unsigned);
static FUNCTION(byte*, enc_atom, (int, uint32, byte*));
static FUNCTION(byte*, enc_pid, (int, uint32, byte*));
static byte* dec_term(int, Eterm**, byte*, ErlOffHeap*, Eterm*);
static FUNCTION(byte*, dec_atom, (int, byte*, uint32*));
static FUNCTION(byte*, dec_pid, (int, byte*, uint32*));
static FUNCTION(byte*, dec_hashed_atom, (int, byte*, uint32*));
static FUNCTION(byte*, dec_and_insert_hashed_atom, (int,byte*,uint32*));
static Eterm term_to_binary(Process* p, Eterm Term, int compressed);

static FUNCTION(int, decode_size2, (byte**, byte*, int*));
static uint32 encode_size_struct2(Eterm, unsigned);

/**********************************************************************/

Eterm
term_to_binary_1(Process* p, Eterm Term)
{
    return term_to_binary(p, Term, 0);
}

Eterm
term_to_binary_2(Process* p, Eterm Term, Eterm Flags)
{
    int compressed = 0;

    while (is_list(Flags)) {
	Eterm arg = CAR(list_val(Flags));
	if (arg == am_compressed) {
	    compressed = 1;
	} else {
	error:
	    BIF_ERROR(p, BADARG);
	}
	Flags = CDR(list_val(Flags));
    }
    if (is_not_nil(Flags)) {
	goto error;
    }

    return term_to_binary(p, Term, compressed);
}

BIF_RETTYPE binary_to_term_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int heap_size;
    Eterm res;
    Eterm* hp;
    Eterm* endp;
    size_t size;
    byte* bytes;
    byte* tmp_ptr;
    byte* dest_ptr = NULL;
    byte* ep;
    int ok = 1;

    if (is_not_binary(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    size = binary_size(BIF_ARG_1);
    GET_BINARY_BYTES(BIF_ARG_1, bytes);
    if (size < 1 || bytes[0] != VERSION_MAGIC) {
	goto error;
    }
    bytes++;
    size--;
    if (size >= 5 && bytes[0] == COMPRESSED) {
	uLongf dest_len;

	dest_len = get_int32(bytes+1);
	dest_ptr = safe_alloc(dest_len);
	if (uncompress(dest_ptr, &dest_len, bytes+5, size-5) != Z_OK) {
	    sys_free(dest_ptr);
	    goto error;
	}
	bytes = dest_ptr;
	size = dest_len;
    }

    tmp_ptr = bytes;
    heap_size = decode_size2(&tmp_ptr, tmp_ptr+size, &ok);
    if (!ok) {
	goto error;
    }
    hp = HAlloc(BIF_P, heap_size);
    endp = hp + heap_size;
    ep = dec_term(-1, &hp, bytes, &BIF_P->off_heap, &res);
    if (dest_ptr != NULL) {
	sys_free(dest_ptr);
    }
    if (ep == NULL) {
	goto error;
    }
    if (hp > endp) {
	erl_exit(1, ":%s, line %d: heap overrun by %d words(s)\n",
		 __FILE__, __LINE__, hp-endp);
    }
    HRelease(BIF_P, hp);
    return res;
}


static Eterm
term_to_binary(Process* p, Eterm Term, int compressed)
{
    int size;
    Eterm bin;
    size_t real_size;
    byte* endp;

    size = encode_size_struct(Term, TERM_TO_BINARY_DFLAGS);

    if (compressed) {
	byte buf[256];
	byte* bytes = buf;
	byte* out_bytes;
	uLongf dest_len;

	if (sizeof(buf) < size) {
	    bytes = safe_alloc(size);
	}

	if ((endp = enc_term(-1, Term, bytes, TERM_TO_BINARY_DFLAGS)) == NULL) {
	    erl_exit(1, "%s, line %d: bad term: %x\n",
		     __FILE__, __LINE__, Term);
	}
	real_size = endp - bytes;
	if (real_size > size) {
	    erl_exit(1, "%s, line %d: buffer overflow: %d word(s)\n",
		     __FILE__, __LINE__, real_size - size);
	}
	dest_len = real_size + real_size / 1000 + 12;
	bin = new_binary(p, NULL, dest_len+6);
	GET_BINARY_BYTES(bin, out_bytes);
	out_bytes[0] = VERSION_MAGIC;
	out_bytes[1] = COMPRESSED;
	put_int32(real_size, out_bytes+2);
	if (compress(out_bytes+6, &dest_len, bytes, real_size) != Z_OK) {
	    BIF_ERROR(p, BADARG);
	}
	if (bytes != buf) {
	    sys_free(bytes);
	}
	return erts_realloc_binary(bin, dest_len+6);
    } else {
	byte* bytes;

	bin = new_binary(p, (byte *)NULL, size);
	GET_BINARY_BYTES(bin, bytes);
	
	bytes[0] = VERSION_MAGIC;
	if ((endp = enc_term(-1, Term, bytes+1, TERM_TO_BINARY_DFLAGS)) == NULL) {
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
enc_atom(int slot, Eterm atom, byte *ep)
{
    Eterm* ce;
    Uint ix;
    int i, j;

    ASSERT(is_atom(atom));

    /*
     * term_to_binary/1,2 and the initial distribution message
     * don't use the cache.
     */
    if ((slot < 0) || (dist_addrs[slot].cache == NULL)) { 
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
    ce = &dist_addrs[slot].cache->out_arr[ix];
    
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
enc_pid(int slot, Eterm pid, byte* ep)
{
    uint32 m;
    uint32 j;

    *ep++ = PID_EXT;
    m = pid_node(pid);
    /* insert  atom here containing host and sysname  */
    ep = enc_atom(slot, dist_addrs[m].sysname, ep);

    /* two bytes for each number and serial */
    j = pid_number(pid);
    put_int32(j, ep);
    ep += 4;
    j = pid_serial(pid);
    put_int32(j, ep);
    ep += 4;
    *ep++ = pid_enc_set_creation(m,pid);
    return ep;
}

static byte* dec_hashed_atom(slot,ep,objp)
int slot; byte *ep; uint32* objp;
{
    if (slot < 0)
	*objp = am_Underscore; /* This is for the first distribution message */
    else
	*objp = dist_addrs[slot].cache->in_arr[get_int8(ep)];
    return ep+1;
}


static byte* dec_and_insert_hashed_atom(slot,ep,objp) 
int slot; byte *ep; uint32* objp;
{
    uint32 ix;
    uint32 len;

    ix = get_int8(ep);
    ep++;
    len = get_int16(ep);
    ep += 2;
    if (slot < 0) /* This is for the first distribution message */
	*objp = am_atom_put((char*)ep,len);
    else
	*objp = dist_addrs[slot].cache->in_arr[ix] = 
	    am_atom_put((char*)ep,len);
    return ep + len;
}

/* Expect an atom (cached or new) */
static byte* dec_atom(slot, ep, objp)
int slot; byte* ep; uint32* objp;
{
    uint32 len;

    switch (*ep++) {
    case ATOM_EXT:
	len = get_int16(ep),
	ep += 2;
	*objp = am_atom_put((char*)ep, len);
	return ep + len;

    case NEW_CACHE:
	return dec_and_insert_hashed_atom(slot,ep,objp); 

    case CACHED_ATOM:
	return dec_hashed_atom(slot,ep,objp);

    default:
	return NULL;
    }
}

static byte* dec_pid(slot, ep, objp)
int slot; byte* ep; uint32* objp;
{
    uint32 i;
    uint32 j;
    uint32 k;
    uint32 n;
    int si;

    /* eat first atom */
    if ((ep = dec_atom(slot, ep, &i)) == NULL)
	return NULL;
    if ((si = find_or_insert_dist_slot(i)) < 0)
	return NULL;
    j = get_int32(ep);
    ep += 4;
    k = get_int32(ep);
    ep += 4;
    if ((n = get_int8(ep)) >= MAX_CREATION)
	return NULL;
    ep += 1;

    n = dec_set_creation(si,n);
    if (j >= MAX_PROCESS) /* CHECK max_process for si==0 ? */
	return NULL;
    if (k >= MAX_SERIAL)
	return NULL;
    *objp = make_pid3(k,si,j,n);
    return ep;
}


/* This function is written in this strange way because in dist.c
   we call it twice for some messages, First we insert a control message 
   in the buffer , and then we insert the actual message in the buffer
   immediataly following the control message. If the real (2) message
   makes the buffer owerflow, we must not destroy the work we have already
   done, i.e we must not forget to copy the encoding of the 
   control message as well, (If there is one)
   
   The buffer dist_buf initially points to tmp_buf, So if we write
   an application where the encoding of all distributed messages are <
   TMP_BUF_SIZE, all remote messages will be encoded into tmp_buf,
   and no additional buffer allocation is necessary.
   However, once a message does not fit into tmp_buf we 
   allocate a larger buffer and set dist_buf to point to that buffer.
   We never go back to tmp_buf situation again.

   We don't have the energy to describe the external format in words here.
   The code explains itself :-). However, All external encodings
   are predeeded with the special character VERSION_MAGIC, which makes
   it possible do distinguish encodings from incompatible erlang systems.
   Every time the external format is changed, This VERSION_MAGIC shall be 
   incremented.

*/



static byte*
enc_term(int slot, Eterm obj, byte* ep, unsigned dist_flags)
{
    int val;
    uint32 n;
    uint32 i;
    uint32 j;
    uint32 m;
    uint32* ptr;
    FloatDef f;
    byte* back;

    switch(tag_val_def(obj)) {
    case NIL_DEF:
	*ep++ = NIL_EXT;   /* soley empty lists */
	return ep;

    case ATOM_DEF:
	return enc_atom(slot,obj,ep);

    case SMALL_DEF:
	if (is_byte(obj)) {
	    val = unsigned_val(obj);
	    *ep++ = SMALL_INTEGER_EXT;
	    put_int8(val, ep);
	    return ep + 1;
	}
	else {
	    val = signed_val(obj);
	    *ep++ = INTEGER_EXT;
	    put_int32(val, ep);
	    return ep + 4;
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
	return enc_pid(slot, obj, ep);

    case REF_DEF:
	i = ref_arity(obj);
	if (i == 2 || (dist_flags & DFLAG_EXTENDED_REFERENCES) == 0) {
	    *ep++ = REFERENCE_EXT;
	    m = ref_node(obj);
	    ep = enc_atom(slot,dist_addrs[m].sysname,ep);
	    j = ref_number(obj);
	    put_int32(j, ep);
	    ep += 4;
	    *ep++ = ref_enc_set_creation(m,obj);
	    return ep;
	} else {
	    *ep++ = NEW_REFERENCE_EXT;
	    put_int16(i-1, ep);
	    ep += 2;
	    m = ref_node(obj);
	    ep = enc_atom(slot,dist_addrs[m].sysname,ep);
	    *ep++ = ref_enc_set_creation(m,obj);
	    for (j = 0; j < i-1; j++) {
		put_int32(ref_ptr(obj)->w[j], ep);
		ep += 4;
	    }
	    return ep;
	}

    case PORT_DEF:
	*ep++ = PORT_EXT;
	m = port_node(obj);
	ep = enc_atom(slot,dist_addrs[m].sysname,ep);
	j = port_number(obj);
	put_int32(j, ep);
	ep += 4;
	*ep++ = port_enc_set_creation(m,obj);
	return ep;

    case LIST_DEF:
	if ((i = is_string(obj)) && (i < MAX_STRING_LEN)) {
	    *ep++ = STRING_EXT;
	    put_int16(i, ep);
	    ep += 2;
	    while (is_list(obj)) {
		uint32* cons = list_val(obj);
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
	    uint32* cons = list_val(obj);
	    i++;  /* count number of cons cells */
	    if ((ep = enc_term(slot, CAR(cons), ep, dist_flags)) == NULL)
		return NULL;
	    obj = CDR(cons);
	}
	if ((ep = enc_term(slot, obj, ep, dist_flags)) == NULL)
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
	    if ((ep = enc_term(slot, *ptr++, ep, dist_flags)) == NULL)
		return NULL;
	}
	return ep;

    case FLOAT_DEF:
	*ep++ = FLOAT_EXT;
	GET_DOUBLE(obj, f);

	/* now the sprintf which does the work */
	i = sys_double_to_chars(f.fd, (char*) ep);

	/* Don't leave garbage after the float!  (Bad practice in general,
	 * and Purify complains.)
	 */
	sys_memset(ep+i, 0, 31-i);

	return ep + 31;

    case BINARY_DEF:
	{
	    byte* bytes;
	    GET_BINARY_BYTES(obj, bytes);
	    *ep++ = BINARY_EXT;
	    j = binary_size(obj);
	    put_int32(j, ep);
	    ep += 4;
	    sys_memcpy(ep, bytes, j);
	    return ep + j;
	}
    case FUN_DEF:
	if ((dist_flags & DFLAG_FUN_TAGS) != 0) {
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);
	    Uint num_free = funp->num_free;

	    *ep++ = FUN_EXT;
	    put_int32(num_free, ep);
	    ep += 4;
	    ep = enc_pid(slot, funp->creator, ep);
	    ep = enc_atom(slot, make_atom(funp->modp->module), ep);
	    ep = enc_term(slot, make_small(funp->index), ep, dist_flags);
	    ep = enc_term(slot, funp->uniq, ep, dist_flags);
	    ptr = funp->env;
	    while (num_free-- > 0) {
		if ((ep = enc_term(slot, *ptr++, ep, dist_flags)) == NULL) {
		    return NULL;
		}
	    }
	}
	return ep;
    }
    return NULL;
}

int
to_external(int slot, Eterm obj, byte **ext)
{
    byte* ptr = *ext;
    byte* dist_end = dist_buf + dist_buf_size;
    unsigned dist_flags;

    if (slot > 0) {
	dist_flags = dist_addrs[slot].flags;
    } else {
	dist_flags = DFLAG_EXTENDED_REFERENCES | DFLAG_FUN_TAGS;
    }

    /* check if we are using dist_buf !! */
    if ((slot > 0) && (ptr >= dist_buf) && (ptr < dist_end)) {
	int size = 50 + encode_size_struct(obj, dist_flags);

	/* check if distbuf must grow */
	if ((ptr + size) > dist_end) {
	    int len = ptr - dist_buf;
	    char* buf;
	    
	    size += (1000 + len);
	    buf = (byte*) safe_alloc(20+size);  /* REMOVE THIS SLOPPY !!! */
	    
	    /* We need to restore the old contetnts of dist_buf
	       before we can proceed */
	    sys_memcpy(buf,dist_buf,len);
	    if (dist_buf != tmp_buf)
		sys_free(dist_buf);
	    dist_buf_size = size;
	    dist_buf = buf;
	    ptr =  dist_buf + len;
	    *ext = ptr;
	}
    }
    *ptr++ = VERSION_MAGIC;
    if ((ptr = enc_term(slot, obj, ptr, dist_flags)) == NULL)
	erl_exit(1, "Internal data structure error (in to_external)\n");
    *ext = ptr;
    return 0;
}

/*
** hpp is set to either a &p->htop or
** a pointer to a memory pointer (form message buffers)
** on return hpp is updated to point after allocated data
*/
Eterm
from_external(int slot, Eterm** hpp, byte **ext, ErlOffHeap* off_heap)
{
    Eterm obj;
    byte* ep = *ext;

    if (*ep++ != VERSION_MAGIC) {
	cerr_pos = 0;
	if (slot >= 0)
	    erl_printf(CBUF,
		       "** Got message from noncompatible erlang on slot %d\n",
		       slot);
	else 
	    erl_printf(CBUF,
		       "** Attempt to convert old non compatible binary %d\n",
		       *ep);
	send_error_to_logger(NIL);
	return THE_NON_VALUE;
    }
    if ((ep = dec_term(slot, hpp, ep, off_heap, &obj)) == NULL) {
	if (slot >= 0) {	/* Don't print this for binary_to_term */
	    cerr_pos = 0;
	    erl_printf(CBUF,"Got corrupted message on slot %d\n", slot);
	    send_error_to_logger(NIL);
	}
#ifdef DEBUG
	bin_write(CERR,*ext,500);
#endif
	return THE_NON_VALUE;
    }
    *ext = ep;
    return obj;
}


static byte*
dec_term(int slot, Eterm** hpp, byte* ep, ErlOffHeap* off_heap, Eterm* objp)
{
    Eterm temp;
    uint32 j;
    uint32 k;
    uint32 n;
    int si;
    ProcBin* pb;

    switch (*ep++) {
    case INTEGER_EXT:
	{
	    Sint n = get_int32(ep);

	    ep += 4;
	    if (IS_SSMALL(n)) {
		*objp = make_small(n);
	    } else {
		Eterm* hp = *hpp;
		*objp = uint32_to_big(n, hp);
		*hpp = hp + 2;
	    }
	    return ep;
	}

    case SMALL_INTEGER_EXT:
	n = get_int8(ep);
	ep++;
	*objp = make_small(n);
	return ep;

    case SMALL_BIG_EXT:
	n = get_int8(ep);
	ep++;
	goto big_loop;

    case LARGE_BIG_EXT:
	n = get_int32(ep);
	ep += 4;

    big_loop:
	{
	    Eterm* hp = *hpp;
	    Eterm big;

	    k = get_int8(ep);
	    ep++;
	    big = bytes_to_big(ep, n, k, hp);
	    if (is_big(big)) {
		hp += (big_arity(big)+1);
	    }
	    *objp = big;
	    ep += n;
	    *hpp = hp;
	    return ep;
	}

    case ATOM_EXT:
	n = get_int16(ep);
	ep += 2;
	*objp = am_atom_put((char*)ep, n);
	return ep + n;

    case NEW_CACHE:
	return dec_and_insert_hashed_atom(slot,ep,objp);

    case CACHED_ATOM:
	return dec_hashed_atom(slot,ep,objp);

    case PID_EXT:
	return dec_pid(slot, ep, objp);

    case NEW_REFERENCE_EXT:
	{
	    Eterm* hp = *hpp;
	    int i;

	    k = get_int16(ep);
	    ep += 2;
	    
	    if ((ep = dec_atom(slot,ep, &temp)) == NULL)
		return NULL;
	    if ((si = find_or_insert_dist_slot(temp)) < 0)
		return 0;
	    if ((n = get_int8(ep)) >= MAX_CREATION)
		return NULL;
	    ep += 1;
	    n = dec_set_creation(si,n);
	    *objp = make_ref(hp);
	    hp[0] = make_ref_header(k + 1);
	    hp[1] = make_ref3(si,0,n);
	    for (i = 0; i < k; i++) {
		hp[i+2] = get_int32(ep);
		ep += 4;
	    }
	    hp += k+2;
	    *hpp = hp;
	    return ep;
	}

    case REFERENCE_EXT:
	{
	    Eterm* hp = *hpp;

	    if ((ep = dec_atom(slot,ep, &temp)) == NULL)
		return NULL;
	    if ((si = find_or_insert_dist_slot(temp)) < 0)
		return 0;
	    if ((j = get_int32(ep)) >= MAX_REFERENCE )
		return NULL;
	    ep += 4;
	    if ((n = get_int8(ep)) >= MAX_CREATION)
		return NULL;
	    ep += 1;
	    n = dec_set_creation(si,n);
	    *objp = make_ref(hp);
	    hp[0] = make_ref_header(2);
	    hp[1] = make_ref3(si,0,n);
	    hp[2] = j;
	    hp += 3;
	    *hpp = hp;
	    return ep;
	}
    case PORT_EXT:
	{
	    if ((ep = dec_atom(slot,ep, &temp)) == NULL)
		return NULL;
	    if ((si = find_or_insert_dist_slot(temp)) < 0)
		return 0;
	    if ((j = get_int32(ep)) >= MAX_PORT)
		return NULL;
	    ep += 4;
	    if ((n = get_int8(ep)) >= MAX_CREATION)
		return NULL;
	    ep += 1;
	    n = dec_set_creation(si,n);
	    *objp = make_port3(si,j,n);
	    return ep;
	}

    case NIL_EXT:
	*objp = NIL;
	return ep;

    case LIST_EXT:
	{
	    Eterm* hp;

	    n = get_int32(ep);
	    ep += 4;
	    if (n == 0) {
		*objp = NIL;
		return ep;
	    }
	    hp = *hpp;
	    *hpp = hp + 2 * n;
	    *objp = make_list(hp);
	    while (n-- > 0) {
		if ((ep = dec_term(slot, hpp, ep, off_heap, &hp[0])) == NULL) {
		    return NULL;
		}
		hp[1] = make_list(hp + 2);
		hp += 2;
	    }
	    if (*ep == NIL_EXT) {
		ep++;
		hp[-1] = NIL;
	    } else if ((ep = dec_term(slot, hpp, ep, off_heap, &hp[-1])) == NULL) {
		return NULL;
	    }
	    return ep;
	}

    case STRING_EXT:
	{
	    Eterm* hp;

	    n = get_int16(ep);
	    ep += 2;
	    if (n == 0) {
		*objp = NIL;
		return ep;
	    }
	    hp = *hpp;
	    *objp = make_list(hp);
	    while (n-- > 0) {
		hp[0] = make_small(*ep++);
		hp[1] = make_list(hp+2);
		hp += 2;
	    }
	    hp[-1] = NIL;
	    *hpp = hp;
	    return ep;
	}

    case SMALL_TUPLE_EXT:
	n = get_int8(ep);
	ep++;
	goto tuple_loop;

    case LARGE_TUPLE_EXT:
	n = get_int32(ep);
	ep += 4;

    tuple_loop:
	{
	    Eterm* hp = *hpp;

	    *objp = make_tuple(hp);
	    *hp++ = make_arityval(n);
	    *hpp = hp + n;
	    while (n-- > 0) {
		if ((ep = dec_term(slot, hpp, ep, off_heap, hp++)) == NULL) {
		    return NULL;
		}
	    }
	    return ep;
	}

    case FLOAT_EXT:
	{
	    Eterm* hp = *hpp;
	    FloatDef ff;

	    if (sys_chars_to_double((char*)ep, &ff.fd) != 0) {
		return NULL;
	    }
	    ep += 31;
	    *objp = make_float(hp);
	    PUT_DOUBLE(ff, hp);
	    hp += 3;
	    *hpp = hp;
	    return ep;
	}

    case BINARY_EXT:
	{
	    Eterm* hp = *hpp;
	    
	    n = get_int32(ep);
	    ep += 4;
	    
	    if (n <= ERL_ONHEAP_BIN_LIMIT) {
		ErlHeapBin* hb = (ErlHeapBin *) hp;

		hb->thing_word = header_heap_bin(n);
		hb->size = n;
		hp += heap_bin_size(n);
		sys_memcpy(hb->data, ep, n);
		*objp = make_binary(hb);
	    } else {
		Binary* dbin = (Binary *) safe_alloc(n+sizeof(Binary));
		dbin->flags = 0;
		dbin->orig_size = n;
		dbin->refc = 1;
		sys_memcpy(dbin->orig_bytes, ep, n);
		tot_bin_allocated += n;
		pb = (ProcBin *) hp;
		hp += PROC_BIN_SIZE;
		*hpp = hp;
		pb->thing_word = HEADER_PROC_BIN;
		pb->size = n;
		pb->next = off_heap->mso;
		off_heap->mso = pb;
		pb->val = dbin;
		pb->bytes = dbin->orig_bytes;
		*objp = make_binary(pb);
	    }
	    ep += n;
	    *hpp = hp;
	    return ep;
	}

    case FUN_EXT:
	{
	    Eterm* hp = *hpp;
	    ErlFunThing* funp = (ErlFunThing *) hp;
	    unsigned num_free;
	    int i;

	    num_free = get_int32(ep);
	    ep += 4;
	    hp += ERL_FUN_SIZE + num_free;
	    *hpp = hp;
	    funp->thing_word = HEADER_FUN;
	    funp->next = off_heap->funs;
	    off_heap->funs = funp;
	    funp->num_free = num_free;
	    *objp = make_fun(funp);

	    /* Creator pid */
	    if (*ep++ != PID_EXT) {
		return NULL;
	    }
	    if ((ep = dec_pid(slot, ep, &funp->creator)) == NULL) {
		return NULL;
	    }

	    /* Module */
	    if ((ep = dec_atom(slot, ep, &temp)) == NULL) {
		return NULL;
	    }
	    funp->modp = erts_put_module(temp);

	    /* Index */
	    if ((ep = dec_term(slot, hpp, ep, off_heap, &temp)) == NULL) {
		return NULL;
	    }
	    if (!is_small(temp)) {
		return NULL;
	    }
	    funp->index = unsigned_val(temp);

	    /* Uniq */
	    if ((ep = dec_term(slot, hpp, ep, off_heap, &temp)) == NULL) {
		return NULL;
	    }
	    if (!is_small(temp)) {
		return NULL;
	    }
	    funp->uniq = temp;

	    /* Environment */
	    for (i = 0; i < num_free; i++) {
		if ((ep = dec_term(slot, hpp, ep, off_heap, funp->env+i)) == NULL) {
		    return NULL;
		}
	    }
	    return ep;
	}
    default:
	return NULL;
    }
}


/* returns the number of bytes needed to encode an object
   to a sequence of bytes
   N.B. That this must agree with to_external2() above!!!
   (except for cached atoms) */

Eterm
encode_size_struct(Eterm obj, unsigned dist_flags)
{
    return (1 + encode_size_struct2(obj, dist_flags));
				/* 1 for the VERSION_MAGIC */
}

static Eterm
encode_size_struct2(Eterm obj, unsigned dist_flags)
{
    uint32 m,i, arity, *nobj, sum;

    switch (tag_val_def(obj)) {
    case NIL_DEF:
	return 1;
    case ATOM_DEF:
	/* Make sure NEW_CACHE ix l1 l0 a1 a2 .. an fits */
	return (1 + 1 + 2 + atom_tab(atom_val(obj))->len);
    case SMALL_DEF:
	if (unsigned_val(obj) < 256 ) 
	    return(1 + 1);
	else 
	    return(1 + 4);
    case BIG_DEF:
	if ((i = big_bytes(obj)) < 256)
	    return 1 + 1 + 1 + i;  /* tag,size,sign,digits */
	else
	    return 1 + 4 + 1 + i;  /* tag,size,sign,digits */
    case PID_DEF:
	m = pid_node(obj);
	return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		+ 4 + 4 + 1);
    case REF_DEF:
	i = ref_arity(obj);
	m = ref_node(obj);
	if (i == 2 || (dist_flags & DFLAG_EXTENDED_REFERENCES) == 0) {
	    return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		    + 4 + 1);
	} else {
	    return (1 + 2 + encode_size_struct2(dist_addrs[m].sysname,
						dist_flags)
		    + 4*(ref_arity(obj)-1) + 1);
	}
    case PORT_DEF:
	m = port_node(obj);
	return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		+ 4 + 1);
    case LIST_DEF:
	if ((m = is_string(obj)) && (m < MAX_STRING_LEN))
	    return m + 2 + 1;
	nobj = list_val(obj);
	sum = 5;
	while (1) {
	    sum += encode_size_struct2(*nobj++, dist_flags);
	    if (is_not_list(*nobj)) {
		if (is_nil(*nobj))
		    return sum + 1;
		return(sum + encode_size_struct2(*nobj, dist_flags));
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
	    sum += encode_size_struct2(*(tuple_val(obj) + i + 1), dist_flags);
	return sum;
    case FLOAT_DEF:
	return 32;   /* Yes, including the tag */
    case BINARY_DEF:
	return 1 + 4 + binary_size(obj);
    case FUN_DEF:
	if ((dist_flags & DFLAG_FUN_TAGS) != 0) {
	    ErlFunThing* funp = (ErlFunThing *) fun_val(obj);

	    sum = 1;		/* Tag */
	    sum += 4;		/* Length field (number of free variables */
	    sum += encode_size_struct2(funp->creator, dist_flags);
	    sum += encode_size_struct2(make_atom(funp->modp->module), dist_flags);
	    sum += 2 * (1+4);	/* Index, Uniq */
	    for (i = 0; i < funp->num_free; i++) {
		sum += encode_size_struct2(funp->env[i], dist_flags);
	    }
	    return sum;
	}
	/*FALLTHROUGH*/
    default:
	erl_exit(1,"Internal data structure error (in encode_size_struct2)%x\n",
		 obj);
    }
    return -1; /* Pedantic (lint does not know about erl_exit) */
}

static int
decode_size2(ext, endp, okp)
    byte **ext;
    byte* endp;
    int* okp;
{
    uint32 sum,j,k,i = 0;

#define SKIP(sz) \
    do { \
	*ext += (sz); \
	if ((*ext) > endp) { return *okp = 0; } \
    } while (0)

#define CHKSIZE(sz) \
    do { \
	 if ((*ext)+(sz) > endp) { return *okp = 0; } \
    } while (0)

    SKIP(1);
    switch ((*ext)[-1]) {
    case INTEGER_EXT:
	SKIP(4);
	return(3);
    case SMALL_INTEGER_EXT:
	SKIP(1);
	return(1);
    case SMALL_BIG_EXT:
	CHKSIZE(1);
	i = **ext;		/* number of bytes */
	SKIP(1+1+i);		/* skip size,sign,digits */
	return 1+1+(i+3)/4;
    case LARGE_BIG_EXT:
	CHKSIZE(4);
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	SKIP(4+1+i);		/* skip, size,sign,digits */
	return 1+1+(i+3)/4;
    case ATOM_EXT:
	CHKSIZE(2);
	i = (**ext << 8) | (*ext)[1];
	SKIP(i+2);
	return(1);
    case NEW_CACHE:
	CHKSIZE(3);
	i = get_int16(*ext+1);
	SKIP(i+3);
	return(1);
    case CACHED_ATOM:
	SKIP(1);
	return(1);
    case PID_EXT:
	if (decode_size2(ext, endp, okp) == 0) /* Eat first atom */
	    return 0;
	SKIP(9);
	return(1);
    case NEW_REFERENCE_EXT:
	CHKSIZE(2);
	i = get_int16(*ext);
	SKIP(2);
	/* eat first atom */
	if (decode_size2(ext, endp, okp) == 0)
	    return 0;
	SKIP(4*i+1);
	return(1+i+2);
    case REFERENCE_EXT:
	/* eat first atom */
	if (decode_size2(ext, endp, okp) == 0)
	    return 0;
	SKIP(5);		/* One int and the creation field */
	return(1+3);
    case PORT_EXT:
	/* eat first atom */
	if (decode_size2(ext, endp, okp) == 0)
	    return 0;
	SKIP(5);		/* One int and the creation field */
	return(1);
    case NIL_EXT:
	return(1);
    case LIST_EXT:
	i = j = sum = 0;
	CHKSIZE(4);
	j = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	SKIP(4);
	for(k=0; k<j; k++) {
	    if ((i = decode_size2(ext, endp, okp)) == 0)
		return(0);
	    sum += 1 + i;
	}
	CHKSIZE(1);
	if (**ext == NIL_EXT) {
	    SKIP(1);
	    return sum + 1;
	}
	if ((i = decode_size2(ext, endp, okp)) == 0)
	    return 0;
	return(sum + i);
    case STRING_EXT:
	CHKSIZE(2);
	i = **ext << 8 | (*ext)[1];
	SKIP(i+2);
	return (2 * i) + 1;
    case SMALL_TUPLE_EXT:
	CHKSIZE(1);
	i = *(*ext);
	SKIP(1);
	sum = 2;
	for (j = 0; j < i; j++) {
	    if ((k = decode_size2(ext, endp, okp)) == 0)
		return(0);
	    sum += k;
	}
	return(sum);
    case LARGE_TUPLE_EXT:
	CHKSIZE(4);
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	SKIP(4);
	sum = 2;
	for (j = 0; j < i; j++) {
	    if ((k = decode_size2(ext, endp, okp)) == 0)
		return(0);
	    sum += k;
	}
	return(sum);
    case FLOAT_EXT:
	SKIP(31);
	return(4);
    case BINARY_EXT:
	CHKSIZE(4);
	i = (**ext << 24) | ((*ext)[1] << 16) |((*ext)[2] << 8) | (*ext)[3];
	SKIP(4+i);
	if (i <= ERL_ONHEAP_BIN_LIMIT) {
	    return 1+heap_bin_size(i);
	} else {
	    return 1+PROC_BIN_SIZE;
	}
    case FUN_EXT:
	{
	    unsigned size = 0;
	    int sz;
	    int num_free;

	    /* Number of free variables */
	    SKIP(4);
	    num_free = get_int32(*ext-4);
	    size += num_free;

	    /* Creator pid */
	    if ((sz = decode_size2(ext, endp, okp)) == 0) {
		return 0;
	    }
	    size += sz;

	    /* Module name */
	    if ((sz = decode_size2(ext, endp, okp)) == 0) {
		return 0;
	    }
	    size += sz;

	    /* Index */
	    sz = decode_size2(ext, endp, okp);
	    if (sz == 0) {
		return 0;
	    }
	    size += sz;

	    /* Uniq */
	    sz = decode_size2(ext, endp, okp);
	    if (sz == 0) {
		return 0;
	    }
	    size += sz;

	    /* Env */
	    for (i = 0; i < num_free; i++) {
		sz = decode_size2(ext, endp, okp);
		if (sz == 0) {
		    return 0;
		}
		size += sz;
	    }
	    return 1 + 1 + 1 + 1 +size;
	}
    default:
	return *okp = 0;
    }
}

int
decode_size(byte* t, int size)
{
    int heap_size;
    int ok = 1;

    if (size > 0 && *t == VERSION_MAGIC)
	t++;
    else
	return -1;
    size--;
    heap_size = decode_size2(&t, t+size, &ok);
    return ok ? heap_size : -1;
}
