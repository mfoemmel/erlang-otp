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
#include "external.h"
#include "big.h"
#include "dist.h"

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
#define enc_set_creation(node,obj) \
(node == THIS_NODE && get_creation(obj) == 0) ? this_creation : get_creation(obj)

#define ref_enc_set_creation(node,obj) \
(node == THIS_NODE && get_creation_reference(obj) == 0) ? this_creation : get_creation_reference(obj)

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

static FUNCTION(int, decode_size2, (byte**, byte*, int*));
static uint32 encode_size_struct2(Eterm, unsigned);

/* This function fills ext with the external format of atom 
   if it's an old atom we just supply an index, otherewise
   we insert the index _and_ the entire atom , This way the receiving side
   does not have to perform an hash on the etom to locate it, and
   we save a lot of space on the wire */


static byte* enc_atom(slot,atom,ep)
int slot; uint32 atom; byte *ep;
{
    uint32* ce;
    uint32 ix;
    int i,j;

    ASSERT(is_atom(atom));

    /*  for term_to_words and initial setup msg */     
    if ((slot < 0) || (dist_addrs[slot].cache == NULL)) { 
	i = unsigned_val(atom);
	j = atom_tab(i)->len;
	*ep++ = ATOM_EXT;
	put_int16(j, ep);  /* XXX reduce this to 8 bit in the future */
	ep += 2;
	sys_memcpy((char *) ep, (char*)atom_tab(i)->name, (int) j);
	ep += j;
	return ep;
    }
    ix = unsigned_val(atom) % MAXINDX;
    ce = &dist_addrs[slot].cache->out_arr[ix];
    
    if (*ce == 0) {   /* FREE CACHE ENTRY */
	/* Ok insert the atom in the cache */
	i = unsigned_val(atom);
	j = atom_tab(i)->len;
	*ep++ = NEW_CACHE;
	*ep++ = ix;
	put_int16(j, ep);
	ep += 2;
	sys_memcpy((char *) ep, (char*) atom_tab(i)->name, (int) j);
	*ce = atom;
	return ep + j;
    }
    else  { /*   case CACHE_OCCUPIED: */
	if (*ce == atom) {
	    *ep++ =  CACHED_ATOM;
	    *ep++ = ix;
	    return ep;
	}
	/* we try to encode an atom that hashed into an already
	   occupied slot ... owerwrite */
	i = unsigned_val(atom);
	j = atom_tab(i)->len;
	*ep++ = NEW_CACHE;
	*ep++ = ix;
	put_int16(j, ep);
	ep += 2;
	sys_memcpy((char *) ep, (char*) atom_tab(i)->name, (int) j);
	*ce = atom;
	return ep + j;
    }
}

static byte* enc_pid(slot, pid, ep)
int slot; uint32 pid; byte* ep;
{
    uint32 m;
    uint32 j;

    *ep++ = PID_EXT;
    m = get_node(pid);
    /* insert  atom here containing host and sysname  */
    ep = enc_atom(slot, dist_addrs[m].sysname, ep);

    /* two bytes for each number and serial */
    j = get_number(pid);
    put_int32(j, ep);
    ep += 4;
    j = get_serial(pid);
    put_int32(j, ep);
    ep += 4;
    *ep++ = enc_set_creation(m,pid);
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
    if (slot < 0) /* This is for the first distrribution message */
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
    ProcBin *bp;
    FloatDef f;
    byte* back;

    switch(tag_val_def(obj)) {
    case ATOM_DEF:
	if (is_nil(obj)) {
            *ep++ = NIL_EXT;   /* soley empty lists */
	    return ep;
        }
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

    case REFER_DEF:
	i = refer_arity(obj);
	if (i == 2 || (dist_flags & DFLAG_EXTENDED_REFERENCES) == 0) {
	    *ep++ = REFERENCE_EXT;
	    m = get_node_reference(obj);
	    ep = enc_atom(slot,dist_addrs[m].sysname,ep);
	    j = get_number_reference(obj);
	    put_int32(j, ep);
	    ep += 4;
	    *ep++ = ref_enc_set_creation(m,obj);
	    return ep;
	} else {
	    *ep++ = NEW_REFERENCE_EXT;
	    put_int16(i-1, ep);
	    ep += 2;
	    m = get_node_reference(obj);
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
	m = get_node_port(obj);
	ep = enc_atom(slot,dist_addrs[m].sysname,ep);
	j = get_number_port(obj);
	put_int32(j, ep);
	ep += 4;
	*ep++ = enc_set_creation(m,obj);
	return ep;

    case LIST_DEF:
	if ((i = is_string(obj)) && (i < MAX_STRING_LEN)) {
	    *ep++ = STRING_EXT;
	    put_int16(i, ep);
	    ep += 2;
	    while (is_list(obj)) {
		uint32* cons = ptr_val(obj);
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
	    uint32* cons = ptr_val(obj);
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
	ptr = ptr_val(obj);
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
	ptr = ptr_val(obj);
	GET_DOUBLE(obj, f);

	/* now the sprintf which does the work */
	i = sys_double_to_chars(f.fd, (char*) ep);

	/* Don't leave garbage after the float!  (Bad practice in general,
	 * and Purify complains.)
	 */
	sys_memset(ep+i, 0, 31-i);

	return ep + 31;

    case BINARY_DEF:
	bp = (ProcBin*) ptr_val(obj);
	switch (thing_subtag(bp->thing_word)) {
	case REFC_BINARY_SUBTAG:
	    *ep++ = BINARY_EXT;
	    j = bp->size;
	    put_int32(j, ep);
	    ep += 4;
	    sys_memcpy((char *) ep, (char*) bp->bytes, (int) j);
	    return ep + j;
	case FUN_SUBTAG:
	    if ((dist_flags & DFLAG_FUN_TAGS) != 0) {
		ErlFunThing* funp = (ErlFunThing *) bp;
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
		return ep;
	    }
#ifdef ALLOW_FUN_TUPLES
	    else {
		/*
		 * Map fun to a tuple.
		 */
		
		ErlFunThing* funp = (ErlFunThing *) bp;
		
		/* Tag, arity */
		*ep++ = SMALL_TUPLE_EXT;
		put_int8(5, ep);
		ep += 1;
		
		/* 'fun' */
		ep = enc_atom(slot, am_fun, ep);
		
		/* Module name */
		ep = enc_atom(slot, make_atom(funp->modp->module), ep);
		
		/* Index, Uniq */
		*ep++ = INTEGER_EXT;
		put_int32(funp->index, ep);
		ep += 4;
		*ep++ = INTEGER_EXT;
		put_int32(unsigned_val(funp->uniq), ep);
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
		    if ((ep = enc_term(slot, funp->env[i], ep, dist_flags)) == NULL) {
			return NULL;
		    }
		}
	    }
#endif
	    return ep;
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
		     __FILE__, __LINE__, thing_subtag(bp->thing_word));
	}
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
	send_error_to_logger(0);
	return 0;
    }
    if ((ep = dec_term(slot, hpp, ep, off_heap, &obj)) == NULL) {
	if (slot >= 0) {	/* Don't print this for binary_to_term */
	    cerr_pos = 0;
	    erl_printf(CBUF,"Got corrupted message on slot %d\n", slot);
	    send_error_to_logger(0);
	}
#ifdef DEBUG
	bin_write(CERR,*ext,500);
#endif
	return 0;
    }
#ifdef DEBUG
    check_struct(obj);
#endif
    *ext = ep;
    return obj;
}


static byte*
dec_term(int slot, Eterm** hpp, byte* ep, ErlOffHeap* off_heap, Eterm* objp)
{
    uint32 i;
    uint32 j;
    uint32 k;
    uint32 n;
    uint32* ptr = NULL;		/* suppress warnings about use before set */
    int si;
    FloatDef ff;
    ProcBin* pb;
    Eterm* hp = *hpp;

    switch (*ep++) {
    case INTEGER_EXT:
	n = get_int32(ep); 
	ep += 4;
	*objp = make_small(n);
	return ep;

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
	k = get_int8(ep);
	ep++;
	i = bytes_to_big(ep, n, k, hp);
	if (is_big(i))
	    hp += (big_arity(i)+1);
	*objp = i;
	ep += n;
	*hpp = hp;
	return ep;

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
	k = get_int16(ep);
	ep += 2;

	if ((ep = dec_atom(slot,ep, &i)) == NULL)
	    return NULL;
	if ((si = find_or_insert_dist_slot(i)) < 0)
	    return 0;
	if ((n = get_int8(ep)) >= MAX_CREATION)
	    return NULL;
	ep += 1;
	n = dec_set_creation(si,n);
	*objp = make_refer(hp);
	hp[0] = make_thing(k + 1, REF_THING_SUBTAG);
	hp[1] = make_refer3(si,0,n);
	for (i = 0; i < k; i++) {
	    hp[i+2] = get_int32(ep);
	    ep += 4;
	}
	hp += k+2;
	*hpp = hp;
	return ep;

    case REFERENCE_EXT:
	if ((ep = dec_atom(slot,ep, &i)) == NULL)
	    return NULL;
	if ((si = find_or_insert_dist_slot(i)) < 0)
	    return 0;
	if ((j = get_int32(ep)) >= MAX_REFERENCE )
	    return NULL;
	ep += 4;
	if ((n = get_int8(ep)) >= MAX_CREATION)
	    return NULL;
	ep += 1;
	n = dec_set_creation(si,n);
	*objp = make_refer(hp);
	hp[0] = make_thing(2, REF_THING_SUBTAG);
	hp[1] = make_refer3(si,0,n);
	hp[2] = j;
	hp += 3;
	*hpp = hp;
	return ep;

    case PORT_EXT:
	if ((ep = dec_atom(slot,ep, &i)) == NULL)
	    return NULL;
	if ((si = find_or_insert_dist_slot(i)) < 0)
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

    case NIL_EXT:
	*objp = NIL;
	return ep;

    case LIST_EXT:
	n = get_int32(ep);
	ep += 4;
	if (n == 0) {
	    *objp = NIL;
	    return ep;
	}
	*objp = make_list(hp);
	for (k = 0; k < n; k++) {
	    ptr = hp;
	    hp += 2;
	    if ((ep = dec_term(slot, &hp, ep, off_heap, &ptr[0])) == NULL) {
		return NULL;
	    }
	    ptr[1] = make_list(hp);
	}
	if (*ep == NIL_EXT) {
	    ep++;
	    ptr[1] = NIL;
	}
	else {
	    if ((ep = dec_term(slot, &hp, ep, off_heap, &ptr[1])) == NULL)
		return NULL;
	}
	*hpp = hp;
	return ep;

    case STRING_EXT:
	n = get_int16(ep);
	ep += 2;
	if (n == 0) {
	    *objp = NIL;
	    return ep;
	}
	*objp = make_list(hp);
	for (k = 0; k < n; k++) {
	    ptr = hp;
	    hp += 2;
	    ptr[0] = make_small(*ep);
	    ptr[1] = make_list(hp);
	    ep++;
	}
	ptr[1] = NIL;
	*hpp = hp;
	return ep;

    case SMALL_TUPLE_EXT:
	n = get_int8(ep);
	ep++;
	goto tuple_loop;

    case LARGE_TUPLE_EXT:
	n = get_int32(ep);
	ep += 4;

    tuple_loop:
	*objp = make_tuple(hp);
	ptr = hp;
	*ptr = make_arityval(n);
	hp += (n+1);
	for (j = 1; j <= n; j++) {
	    if ((ep = dec_term(slot, &hp, ep, off_heap, &ptr[j])) == NULL) {
		return NULL;
	    }
	}
	*hpp = hp;
	return ep;

    case FLOAT_EXT:
	if (sys_chars_to_double((char*)ep, &ff.fd) != 0)
	    return NULL;
	ep += 31;
	*objp = make_float(hp);
	PUT_DOUBLE(ff, hp);
	hp += 3;
	*hpp = hp;
	return ep;

    case BINARY_EXT:
	{
	    Binary* dbin;

	    n = get_int32(ep);
	    ep += 4;
	    dbin = DriverBinary2Binary(driver_alloc_binary(n));
	    sys_memcpy(dbin->orig_bytes, ep, n);
	    pb = (ProcBin *) hp;
	    hp += PROC_BIN_SIZE;
	    *hpp = hp;
	    pb->thing_word = make_thing(PROC_BIN_SIZE-1, REFC_BINARY_SUBTAG);
	    pb->size = n;
	    pb->next = off_heap->mso;
	    off_heap->mso = pb;
	    pb->val = dbin;
	    pb->bytes = dbin->orig_bytes;
	    ep += n;
	    *objp = make_binary(pb);
	    return ep;
	}

    case FUN_EXT:
	{
	    ErlFunThing* funp = (ErlFunThing *) hp;
	    Eterm mod;
	    Eterm temp;
	    unsigned num_free;

	    num_free = get_int32(ep);
	    ep += 4;
	    hp += ERL_FUN_SIZE - 1 + num_free;
	    funp->thing_word = make_thing(ERL_FUN_SIZE-2, FUN_SUBTAG);
	    funp->next = off_heap->funs;
	    off_heap->funs = funp;
	    funp->num_free = num_free;

	    /* Creator pid */
	    if (*ep++ != PID_EXT) {
		return NULL;
	    }
	    if ((ep = dec_pid(slot, ep, &funp->creator)) == NULL) {
		return NULL;
	    }

	    /* Module */
	    if ((ep = dec_atom(slot, ep, &mod)) == NULL) {
		return NULL;
	    }
	    funp->modp = erts_put_module(mod);

	    /* Index */
	    if ((ep = dec_term(slot, &hp, ep, off_heap, &temp)) == NULL) {
		return NULL;
	    }
	    if (!is_small(temp)) {
		return NULL;
	    }
	    funp->index = unsigned_val(temp);

	    /* Uniq */
	    if ((ep = dec_term(slot, &hp, ep, off_heap, &temp)) == NULL) {
		return NULL;
	    }
	    if (!is_small(temp)) {
		return NULL;
	    }
	    funp->uniq = temp;

	    /* Envioronment */
	    for (i = 0; i < num_free; i++) {
		if ((ep = dec_term(slot, &hp, ep, off_heap, funp->env+i)) == NULL) {
		    return NULL;
		}
	    }

	    *hpp = hp;
	    *objp = make_binary(funp);
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
    ProcBin *bp;

    switch (tag_val_def(obj)) {
    case ATOM_DEF:
	if (is_nil(obj)) 
	    return 1;
	/* Make sure NEW_CACHE ix l1 l0 a1 a2 .. an fits */
	return (1 + 1 + 2 + atom_tab(unsigned_val(obj))->len);
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
	m = get_node(obj);
	return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		+ 4 + 4 + 1);
    case REFER_DEF:
	i = refer_arity(obj);
	m = get_node_reference(obj);
	if (i == 2 || (dist_flags & DFLAG_EXTENDED_REFERENCES) == 0) {
	    return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		    + 4 + 1);
	} else {
	    return (1 + 2 + encode_size_struct2(dist_addrs[m].sysname,
						dist_flags)
		    + 4*(refer_arity(obj)-1) + 1);
	}
    case PORT_DEF:
	m = get_node_port(obj);
	return (1 + encode_size_struct2(dist_addrs[m].sysname, dist_flags)
		+ 4 + 1);
    case LIST_DEF:
	if ((m = is_string(obj)) && (m < MAX_STRING_LEN))
	    return m + 2 + 1;
	nobj = ptr_val(obj);
	sum = 5;
	while (1) {
	    sum += encode_size_struct2(*nobj++, dist_flags);
	    if (is_not_list(*nobj)) {
		if (is_nil(*nobj))
		    return sum + 1;
		return(sum + encode_size_struct2(*nobj, dist_flags));
	    }
	    nobj = ptr_val(*nobj);
	}
    case TUPLE_DEF:
	arity = arityval(*(ptr_val(obj)));
	if (arity <= 0xff) 
	    sum = 1 + 1;
	else
	    sum = 1 + 4;
	for (i = 0; i < arity; i++)
	    sum += encode_size_struct2(*(ptr_val(obj) + i + 1), dist_flags);
	return sum;
    case FLOAT_DEF:
	return 32;   /* Yes, including the tag */
    case BINARY_DEF:
	bp = (ProcBin*) ptr_val(obj);
	if (thing_subtag(bp->thing_word) != FUN_SUBTAG) {
	    return 1 + 4 + bp->size;
	} else if ((dist_flags & DFLAG_FUN_TAGS) != 0) {
	    ErlFunThing* funp = (ErlFunThing *) bp;

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
#ifdef ALLOW_FUN_TUPLES
	else {
	    /*
	     * Size when fun is mapped to a tuple.
	     */

	    ErlFunThing* funp = (ErlFunThing *) bp;

	    sum = 1 + 1;	/* Tuple tag, arity */
	    sum += 1 + 1 + 2 + atom_tab(unsigned_val(am_fun))->len; /* 'fun' */
	    sum += 1 + 1 + 2 + atom_tab(funp->modp->module)->len; /* Module name */
	    sum += 2 * (1 + 4); /* Index + Uniq */

	    /*
	     * The free variables are a encoded as a sub tuple.
	     */
	    sum += 1 + (funp->num_free < 0x100 ? 1 : 4);
	    for (i = 0; i < funp->num_free; i++) {
		sum += encode_size_struct2(funp->env[i], dist_flags);
	    }
	    return sum;
	}
#endif
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
	return(1);
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
	return(1+PROC_BIN_SIZE);
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
	    return 1 + 1 + 1 +size;
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
