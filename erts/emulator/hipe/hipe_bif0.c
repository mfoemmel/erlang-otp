/* $Id$
 * hipe_bif0.c
 *
 * Compiler and linker support.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "error.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_db.h"
#include "hash.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_bif0.h"
/* We need hipe_literals.h for HIPE_SYSTEM_CRC, but it redefines
   a few constants. #undef them here to avoid warnings. */
#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h"
#endif

#define BeamOpCode(Op)	((Uint)BeamOp(Op))

/* check if an address is unsafe for a 32-bit load or store */
#if defined(__i386__)
#define is_unsafe_32(address)	0
#else
#define is_unsafe_32(address)	((unsigned long)(address) & 3)
#endif

static int term_to_Sint(Eterm term, Sint *sp)
{
    if( is_small(term) ) {
	*sp = signed_val(term);
	return 1;
    } else if (is_big(term) && big_fits_in_sint32(term) ) {
	*sp = big_to_sint32(term);
	return 1;
    } else
	return 0;
}

static Eterm Sint32_to_term(Sint32 x, Process *p)
{
    if( MY_IS_SSMALL(x) ) {
	return make_small(x);
    } else {
	Eterm *hp = HAlloc(p, BIG_NEED_SIZE(2));
	return small_to_big(x, hp);
    }
}

static Eterm Uint_to_term(Uint x, Process *p)
{
    if( IS_USMALL(0, x) ) {
	return make_small(x);
    } else {
	Eterm *hp = HAlloc(p, BIG_NEED_SIZE(2));
	return uint_to_big(x, hp);
    }
}

static void *term_to_address(Eterm arg)
{
    Uint u;
    return term_to_Uint(arg, &u) ? (void*)u : NULL;
}

static Eterm address_to_term(void *address, Process *p)
{
    return Uint_to_term((Uint)address, p);
}

/*
 * BIFs for reading and writing memory. Used internally by HiPE.
 */
BIF_RETTYPE hipe_bifs_read_u8_1(BIF_ALIST_1)
BIF_ADECL_1
{
    unsigned char *address = term_to_address(BIF_ARG_1);
    if( !address )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(*address));
}

BIF_RETTYPE hipe_bifs_read_s32_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Sint32 *address = term_to_address(BIF_ARG_1);
    if( !address || is_unsafe_32(address) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Sint32_to_term(*address, BIF_P));
}

BIF_RETTYPE hipe_bifs_read_u32_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Uint *address = term_to_address(BIF_ARG_1);
    if( !address || is_unsafe_32(address) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(*address, BIF_P));
}

BIF_RETTYPE hipe_bifs_write_u8_2(BIF_ALIST_2)
BIF_ADECL_2
{
    unsigned char *address;

    address = term_to_address(BIF_ARG_1);
    if( !address || is_not_small(BIF_ARG_2) )
	BIF_ERROR(BIF_P, BADARG);
    *address = unsigned_val(BIF_ARG_2);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_write_s32_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Sint32 *address;
    Sint value;

    address = term_to_address(BIF_ARG_1);
    if( !address || is_unsafe_32(address) )
	BIF_ERROR(BIF_P, BADARG);
    if( !term_to_Sint(BIF_ARG_2, &value) )
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_write_u32_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Uint *address;
    Uint value;

    address = term_to_address(BIF_ARG_1);
    if( !address || is_unsafe_32(address) )
	BIF_ERROR(BIF_P, BADARG);
    if( !term_to_Uint(BIF_ARG_2, &value) )
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
#if defined(__sparc__)
    asm volatile("flush %0"
		 : /* no outputs */
		 : "r"(address)
		 : "memory");
#endif

    BIF_RET(NIL);
}

/*
 * BIFs for SML-like mutable arrays and reference cells.
 * For now, limited to containing immediate data.
 */
#if 1	/* use bignums as carriers, easier on the gc */
#define make_array_header(sz)	make_pos_bignum_header((sz))
#define array_header_arity(h)	header_arity((h))
#define make_array(hp)		make_big((hp))
#define is_not_array(x)		is_not_big((x))
#define array_val(x)		big_val((x))
#else	/* use tuples as carriers, easier debugging, harder on the gc */
#define make_array_header(sz)	make_arityval((sz))
#define array_header_arity(h)	arityval((h))
#define make_array(hp)		make_tuple((hp))
#define is_not_array(x)		is_not_tuple((x))
#define array_val(x)		tuple_val((x))
#endif
#define array_length(a)		array_header_arity(array_val((a))[0])

BIF_RETTYPE hipe_bifs_array_2(BIF_ALIST_2)
BIF_ADECL_2
{
    Eterm *hp;
    int nelts, i;

    if( is_not_small(BIF_ARG_1) ||
	(nelts = signed_val(BIF_ARG_1)) < 0 ||
	is_not_immed(BIF_ARG_2) )
	BIF_ERROR(BIF_P, BADARG);
    if( nelts == 0 )	/* bignums must not be empty */
	BIF_RET(NIL);
    hp = HAlloc(BIF_P, 1+nelts);
    hp[0] = make_array_header(nelts);
    for(i = 1; i <= nelts; ++i)
	hp[i] = BIF_ARG_2;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_array_length_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if( is_not_array(BIF_ARG_1) ) {
	if( is_nil(BIF_ARG_1) )	/* NIL represents empty arrays */
	    BIF_RET(make_small(0));
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(array_header_arity(array_val(BIF_ARG_1)[0])));
}

BIF_RETTYPE hipe_bifs_array_sub_2(BIF_ALIST_2)
BIF_ADECL_2
{
    unsigned i;

    if( is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[i+1]);
}

BIF_RETTYPE hipe_bifs_array_update_3(BIF_ALIST_3)
BIF_ADECL_3
{
    unsigned i;

    if( is_not_immed(BIF_ARG_3) ||
	is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[i+1] = BIF_ARG_3;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_ref_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm *hp;

    if( is_not_immed(BIF_ARG_1) )
	BIF_RET(BADARG);
    hp = HAlloc(BIF_P, 1+1);
    hp[0] = make_array_header(1);
    hp[1] = BIF_ARG_1;
    BIF_RET(make_array(hp));
}

BIF_RETTYPE hipe_bifs_ref_get_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if( is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[1]);
}

BIF_RETTYPE hipe_bifs_ref_set_2(BIF_ALIST_2)
BIF_ADECL_2
{
    if( is_not_immed(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1) )
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[1] = BIF_ARG_2;
    BIF_RET(NIL);
}

/*
 * Allocate memory for code.
 */
BIF_RETTYPE hipe_bifs_alloc_code_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm *block;

    if( is_not_small(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    block = (Eterm*) safe_alloc(unsigned_val(BIF_ARG_1));
    BIF_RET(address_to_term(block, BIF_P));
}

/*
 * hipe_bifs_alloc_constant_1 is like hipe_bifs_alloc_code_1, except it
 * returns memory suitable for storing constant Erlang values.
 *
 * These constants must not be forwarded by the gc.
 * Therefore, the gc needs to be able to distinguish between
 * collectible objects and constants. Unfortunately, an Erlang
 * process' collectible objects are scattered around in two
 * heaps and a list of message buffers, so testing "is X a
 * collectible object?" can be expensive.
 *
 * Instead, constants are placed in a single contiguous area,
 * which allows for an inexpensive "is X a constant?" test.
 *
 * XXX: Allow this area to be grown.
 */

/* not static, needed by garbage collector */
Eterm *hipe_constants_start = NULL;
Eterm *hipe_constants_next = NULL;
static unsigned constants_avail_words = 0;
#define CONSTANTS_BYTES	(1024*4096)

static Eterm *constants_alloc(unsigned nwords)
{
    Eterm *next;

    /* initialise at the first call */
    if( (next = hipe_constants_next) == NULL ) {
	next = (Eterm*)safe_alloc(CONSTANTS_BYTES);
	hipe_constants_start = next;
	hipe_constants_next = next;
	constants_avail_words = CONSTANTS_BYTES / sizeof(Eterm);
    }
    if( nwords > constants_avail_words )
	erl_exit(1, "out of constants pool memory\n");
    constants_avail_words -= nwords;
    hipe_constants_next = next + nwords;
    return next;
}

BIF_RETTYPE hipe_bifs_alloc_constant_1(BIF_ALIST_1)
BIF_ADECL_1
{
    unsigned nwords;
    Eterm *block;

    if( is_not_small(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    nwords = unsigned_val(BIF_ARG_1);
    block = constants_alloc(nwords);
    BIF_RET(address_to_term(block, BIF_P));
}

BIF_RETTYPE hipe_bifs_term_size_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RET(make_small(size_object(BIF_ARG_1)));
}

BIF_RETTYPE hipe_bifs_copy_term_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Eterm size, *hp, res;

    hp = term_to_address(BIF_ARG_2);
    if( !hp || is_not_small(BIF_ARG_3) || is_immed(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    size = unsigned_val(BIF_ARG_3);

    /* this only works as long as BIF_ARG_1 contains no binaries :-( */
    res = copy_struct(BIF_ARG_1, size, &hp, NULL);

    BIF_RET(address_to_term((void*)res, BIF_P));
}

/*
 * Convert {M,F,A} to pointer to first insn after initial func_info.
 */
Uint *hipe_bifs_find_pc_from_mfa(Eterm mfa)
{
    Eterm *tp;
    Module *modp;
    Eterm mod;
    Eterm name;
    int arity;
    Uint *code_base;
    int i, n;

    if( !is_tuple(mfa) )
	return NULL;
    tp = tuple_val(mfa);
    if( tp[0] != make_arityval(3) )
	return NULL;
    mod = tp[1];
    name = tp[2];
    if( !is_atom(mod) || !is_atom(name) || !is_small(tp[3]) )
	return NULL;
    arity = signed_val(tp[3]);
    modp = erts_get_module(mod);
    if( modp == NULL || (code_base = modp->code) == NULL )
	return NULL;
    n = code_base[MI_NUM_FUNCTIONS];
    for(i = 0; i < n; ++i) {
	Uint *code_ptr = (Uint*)code_base[MI_FUNCTIONS+i];
	ASSERT(code_ptr[0] == BeamOpCode(op_i_func_info_IaaI));
	if( code_ptr[3] == name && code_ptr[4] == arity )
	    return code_ptr+5;
    }
    return NULL;
}

BIF_RETTYPE hipe_bifs_fun_to_address_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm *pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(address_to_term(pc, BIF_P));
}

BIF_RETTYPE hipe_bifs_fun_to_address_3(BIF_ALIST_3)
BIF_ADECL_3
{
  Export* export_entry;

  if( is_not_atom(BIF_ARG_1) ||
      is_not_atom(BIF_ARG_2) ||
      is_not_small(BIF_ARG_3) ||
      signed_val(BIF_ARG_3) < 0 )
    BIF_ERROR(BIF_P, BADARG);

  export_entry = erts_find_export_entry(BIF_ARG_1, BIF_ARG_2,
					signed_val(BIF_ARG_3));
  if( !export_entry )
      BIF_RET(am_false);
  BIF_RET(address_to_term(export_entry->address, BIF_P));
}

BIF_RETTYPE hipe_bifs_set_native_address_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Eterm *pc;
    void *address;
    int is_closure;

    switch( BIF_ARG_3 ) {
      case am_false:
	is_closure = 0;
	break;
      case am_true:
	is_closure = 1;
	break;
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    address = term_to_address(BIF_ARG_2);
    if( !address )
	BIF_ERROR(BIF_P, BADARG);

    pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);

    if( pc ) {
#if HIPE
	hipe_set_call_trap(pc, address, is_closure);
	BIF_RET(am_true);
#endif
    }
    BIF_RET(am_false);
}

/*
 * hipe_bifs_address_to_fun(Address)
 *    - Address is the address of the start of a JAM function's code
 *    - returns {Module, Function, Arity}
 */
BIF_RETTYPE hipe_bifs_address_to_fun_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm *pc;
    Eterm *funcinfo;
    Eterm *hp;

    pc = term_to_address(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    funcinfo = find_function_from_pc(pc);
    if( !funcinfo )
	BIF_RET(am_false);
    hp = HAlloc(BIF_P, 4);
    hp[0] = make_arityval(3);
    hp[1] = funcinfo[0];
    hp[2] = funcinfo[1];
    hp[3] = make_small(funcinfo[2]);
    BIF_RET(make_tuple(hp));
}

/*
 * Native-code stack descriptor hash table.
 *
 * This uses a specialised version of BEAM's hash table code:
 * - hash table size is always a power of two
 *   permits replacing an expensive integer division operation
 *   with a cheap bitwise 'and' in the hash index calculation
 * - lookups assume the key is in the table
 *   permits removing NULL checks
 * - switched order of the hash bucket next and hvalue fields
 *   the hvalue field, which must always be checked, gets a zero
 *   structure offset, which is faster on some architectures;
 *   the next field is only referenced if hvalue didn't match
 * These changes yield a much more efficient lookup operation.
 */
struct hipe_sdesc_table hipe_sdesc_table;

static struct sdesc **alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct sdesc*);
    struct sdesc **bucket = sys_alloc_from(110, nbytes);
    if( !bucket )
	erl_exit(1, "failed to allocate hash buckets (%lu)\n", nbytes);
    sys_memzero(bucket, nbytes);
    return bucket;
}

static void hipe_grow_sdesc_table(void)
{
    unsigned int old_size, new_size, new_mask;
    struct sdesc **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << hipe_sdesc_table.log2size;
    hipe_sdesc_table.log2size += 1;
    new_size = 1 << hipe_sdesc_table.log2size;
    new_mask = new_size - 1;
    hipe_sdesc_table.mask = new_mask;
    old_bucket = hipe_sdesc_table.bucket;
    new_bucket = alloc_bucket(new_size);
    hipe_sdesc_table.bucket = new_bucket;
    for(i = 0; i < old_size; ++i) {
	struct sdesc *b = old_bucket[i];
	while( b != NULL ) {
	    struct sdesc *next = b->bucket.next;
	    unsigned int j = (b->bucket.hvalue >> HIPE_RA_LSR_COUNT) & new_mask;
	    b->bucket.next = new_bucket[j];
	    new_bucket[j] = b;
	    b = next;
	}
    }
    sys_free(old_bucket);
}

static struct sdesc *hipe_put_sdesc(struct sdesc *sdesc)
{
    unsigned long ra;
    unsigned int i;
    struct sdesc *chain;
    unsigned int size;

    ra = sdesc->bucket.hvalue;
    i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    chain = hipe_sdesc_table.bucket[i];

    for(; chain != NULL; chain = chain->bucket.next)
	if( chain->bucket.hvalue == ra )
	    return chain;	/* collision! (shouldn't happen) */

    sdesc->bucket.next = hipe_sdesc_table.bucket[i];
    hipe_sdesc_table.bucket[i] = sdesc;
    hipe_sdesc_table.used += 1;
    size = 1 << hipe_sdesc_table.log2size;
    if( hipe_sdesc_table.used > (4*size)/5 )	/* rehash at 80% */
	hipe_grow_sdesc_table();
    return sdesc;
}

void hipe_init_sdesc_table(struct sdesc *sdesc)
{
    unsigned int log2size, size;

    log2size = 10;
    size = 1 << log2size;
    hipe_sdesc_table.log2size = log2size;
    hipe_sdesc_table.mask = size - 1;
    hipe_sdesc_table.used = 0;
    hipe_sdesc_table.bucket = alloc_bucket(size);

    hipe_put_sdesc(sdesc);
}

/* XXX: remove later when sparc is more debugged 
#ifdef __sparc__ 
const struct sdesc *hipe_find_sdesc(unsigned long ra)
{
    unsigned int i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    const struct sdesc *sdesc = hipe_sdesc_table.bucket[i];
    for(; sdesc; sdesc = sdesc->bucket.next)
	if( sdesc->bucket.hvalue == ra )
	    return sdesc;
    fprintf(stderr, "%s: ra %#lx has no sdesc\r\n", __FUNCTION__, ra);
    abort();
}
  #endif */

/*
 * XXX: x86 and SPARC currently use the same stack descriptor
 * representation. If different representations are needed in
 * the future, this code has to be made target dependent.
 */
static struct sdesc *decode_sdesc(Eterm arg)
{
    Uint ra, exnra;
    Eterm *live;
    unsigned int fsize, arity, nlive, i, nslots, off;
    unsigned int livebitswords, sdescwords;
    void *p;
    struct sdesc *sdesc;

    if( is_not_tuple(arg) ||
	(tuple_val(arg))[0] != make_arityval(5) ||
	term_to_Uint((tuple_val(arg))[1], &ra) == 0 ||
	term_to_Uint((tuple_val(arg))[2], &exnra) == 0 ||
	is_not_small((tuple_val(arg))[3]) ||
	(fsize = unsigned_val((tuple_val(arg))[3])) > 65535 ||
	is_not_small((tuple_val(arg))[4]) ||
	(arity = unsigned_val((tuple_val(arg))[4])) > 255 ||
	is_not_tuple((tuple_val(arg))[5]) )
	return 0;
    /* Get tuple with live slots */
    live = tuple_val((tuple_val(arg))[5]) + 1;
    /* Get number of live slots */
    nlive = arityval(live[-1]);
    /* Calculate size of frame = locals + ra + arguments */
    nslots = fsize + 1 + arity;
    /* Check that only valid slots are given. */
    for(i = 0; i < nlive; ++i) {
	if( is_not_small(live[i]) ||
	    (off = unsigned_val(live[i]), off >= nslots) ||
	    off == fsize )
	    return 0;
    }

    /* Calculate number of words for the live bitmap. */
    livebitswords = (fsize + arity + 1 + 31) / 32;
    /* Calculate number of words for the stack descriptor. */
    sdescwords = 3 + livebitswords + (exnra ? 1 : 0);
    p = safe_alloc(sdescwords*4);
    /* If we have an exception handler use the
       special sdesc_with_exnra structure. */
    if( exnra ) {
	struct sdesc_with_exnra *sdesc_we = p;
	sdesc_we->exnra = exnra;
	sdesc = &(sdesc_we->sdesc);
    } else
	sdesc = p;

    /* Initialise head of sdesc. */
    sdesc->bucket.next = 0;
    sdesc->bucket.hvalue = ra;
    sdesc->summary = (fsize << 9) | (exnra ? (1<<8) : 0) | arity;
    /* Clear all live-bits */
    for(i = 0; i < livebitswords; ++i)
	sdesc->livebits[i] = 0;
    /* Set live-bits given by caller. */
    for(i = 0; i < nlive; ++i) {
	off = unsigned_val(live[i]);
	sdesc->livebits[off / 32] |= (1 << (off & 31));
    }
    return sdesc;
}

BIF_RETTYPE hipe_bifs_enter_sdesc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    struct sdesc *sdesc;

    sdesc = decode_sdesc(BIF_ARG_1);
    if( !sdesc ) {
	fprintf(stderr, "%s: bad sdesc!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    if( hipe_put_sdesc(sdesc) != sdesc ) {
	fprintf(stderr, "%s: duplicate entry!\r\n", __FUNCTION__);
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(NIL);
}

/*
 * Hash table mapping {M,F,A} to nbif address.
 */
struct nbif {
    HashBucket bucket;
    Eterm mod;
    Eterm fun;
    unsigned arity;
    void *address;
};

static struct nbif nbifs[BIF_SIZE] = {
#define BIF_LIST(MOD,FUN,ARY,CFUN,IX)	\
	{ {0,0}, MOD, FUN, ARY, nbif_##CFUN },
#include "erl_bif_list.h"
#undef BIF_LIST
};

#define NBIF_HASH(m,f,a)	((m)*(f)+(a))
static Hash nbif_table;

static HashValue nbif_hash(struct nbif *x)
{
    return NBIF_HASH(x->mod, x->fun, x->arity);
}

static int nbif_cmp(struct nbif *x, struct nbif *y)
{
    return !(x->mod == y->mod && x->fun == y->fun && x->arity == y->arity);
}

static struct nbif *nbif_alloc(struct nbif *x)
{
    return x;	/* pre-allocated */
}

static void init_nbif_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) nbif_hash;
    f.cmp = (HCMP_FUN) nbif_cmp;
    f.alloc = (HALLOC_FUN) nbif_alloc;
    f.free = NULL;

    hash_init(&nbif_table, "nbif_table", 500, f);

    for(i = 0; i < BIF_SIZE; ++i)
	hash_put(&nbif_table, &nbifs[i]);
}

static void *nbif_address(Eterm mod, Eterm fun, unsigned arity)
{
    struct nbif tmpl;
    struct nbif *nbif;

    tmpl.mod = mod;
    tmpl.fun = fun;
    tmpl.arity = arity;

    nbif = hash_get(&nbif_table, &tmpl);
    return nbif ? nbif->address : NULL;
}

/*
 * hipe_bifs_bif_address(M,F,A) -> address or false
 */
BIF_RETTYPE hipe_bifs_bif_address_3(BIF_ALIST_3)
BIF_ADECL_3
{
    void *address;
    static int init_done = 0;

    if( !init_done ) {
	init_nbif_table();
	init_done = 1;
    }

    if( is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) ||
	is_not_small(BIF_ARG_3) ||
	signed_val(BIF_ARG_3) < 0 )
        BIF_RET(am_false);

    address = nbif_address(BIF_ARG_1, BIF_ARG_2, unsigned_val(BIF_ARG_3));
    if( address )
	BIF_RET(address_to_term(address, BIF_P));
    BIF_RET(am_false);
}

/*
 * hipe_bifs_primop_address(Atom) -> address or false
 */
BIF_RETTYPE hipe_bifs_primop_address_1(BIF_ALIST_1)
BIF_ADECL_1
{
    void *res;

    switch( BIF_ARG_1 ) {
#define check_bif(Name,Address)	case Name: res = Address; break
	check_bif(am_callemu, nbif_callemu);
	check_bif(am_suspend_msg, nbif_suspend_msg);
	check_bif(am_suspend_msg_timeout, nbif_suspend_msg_timeout);
	check_bif(am_suspend_0, nbif_suspend_0);

	check_bif(am_Plus, nbif_add_2);
	check_bif(am_Minus, nbif_sub_2);
	check_bif(am_Times, nbif_mul_2);
	check_bif(am_Div, nbif_div_2);
	check_bif(am_div, nbif_intdiv_2);
	check_bif(am_rem, nbif_rem_2);
	check_bif(am_bsl, nbif_bsl_2);
	check_bif(am_bsr, nbif_bsr_2);
	check_bif(am_band, nbif_band_2);
	check_bif(am_bor, nbif_bor_2);
	check_bif(am_bxor, nbif_bxor_2);
	check_bif(am_bnot, nbif_bnot_1);

	check_bif(am_gc_1, nbif_gc_1);
	check_bif(am_get_msg, nbif_get_msg);
	check_bif(am_select_msg, nbif_select_msg);
	check_bif(am_mbox_empty, nbif_mbox_empty);
	check_bif(am_next_msg, nbif_next_msg);
	check_bif(am_set_timeout, nbif_set_timeout);
	check_bif(am_clear_timeout, nbif_clear_timeout);

	check_bif(am_bs_init, nbif_bs_init);
	check_bif(am_bs_final, nbif_bs_final);
	check_bif(am_bs_start_match, nbif_bs_start_match);
	check_bif(am_bs_get_integer, nbif_bs_get_integer);
	check_bif(am_bs_get_float, nbif_bs_get_float);
	check_bif(am_bs_get_binary, nbif_bs_get_binary);
	check_bif(am_bs_get_binary_all, nbif_bs_get_binary_all);
	check_bif(am_bs_skip_bits, nbif_bs_skip_bits);
	check_bif(am_bs_skip_bits_all, nbif_bs_skip_bits_all);
	check_bif(am_bs_test_tail, nbif_bs_test_tail);
	check_bif(am_bs_save, nbif_bs_save);
	check_bif(am_bs_restore, nbif_bs_restore);
	check_bif(am_bs_put_integer, nbif_bs_put_integer);
	check_bif(am_bs_put_binary, nbif_bs_put_binary);
	check_bif(am_bs_put_binary_all, nbif_bs_put_binary_all);
	check_bif(am_bs_put_float, nbif_bs_put_float);
	check_bif(am_bs_put_string, nbif_bs_put_string);
	check_bif(am_bs_get_matchbuffer, nbif_bs_get_matchbuffer);

	check_bif(am_cmp_2, nbif_cmp_2);
	check_bif(am_op_exact_eqeq_2, nbif_eq_2);

	check_bif(am_test, nbif_test);


	check_bif(am_clear_fp_exception, nbif_clear_fp_exception);
	check_bif(am_check_fp_exception, nbif_check_fp_exception);
	check_bif(am_conv_big_to_float, nbif_conv_big_to_float);

#ifdef __sparc__
	check_bif(am_inc_stack_0args_0, nbif_inc_stack_0args);
	check_bif(am_inc_stack_1args_0, nbif_inc_stack_1args);
	check_bif(am_inc_stack_2args_0, nbif_inc_stack_2args);
	check_bif(am_inc_stack_3args_0, nbif_inc_stack_3args);
	check_bif(am_inc_stack_4args_0, nbif_inc_stack_4args);
	check_bif(am_inc_stack_5args_0, nbif_inc_stack_5args);
	check_bif(am_inc_stack_6args_0, nbif_inc_stack_6args);
	check_bif(am_inc_stack_7args_0, nbif_inc_stack_7args);
	check_bif(am_inc_stack_8args_0, nbif_inc_stack_8args);
	check_bif(am_inc_stack_9args_0, nbif_inc_stack_9args);
	check_bif(am_inc_stack_10args_0, nbif_inc_stack_10args);
	check_bif(am_inc_stack_11args_0, nbif_inc_stack_11args);
	check_bif(am_inc_stack_12args_0, nbif_inc_stack_12args);
	check_bif(am_inc_stack_13args_0, nbif_inc_stack_13args);
	check_bif(am_inc_stack_14args_0, nbif_inc_stack_14args);
	check_bif(am_inc_stack_15args_0, nbif_inc_stack_15args);
	check_bif(am_inc_stack_16args_0, nbif_inc_stack_16args);
#endif
#ifdef __i386__
	check_bif(am_inc_stack_0, nbif_inc_stack_0);
#endif
#undef check_bif
      default:
	BIF_RET(am_false);
    }
    BIF_RET(address_to_term(res, BIF_P));
}

/*
 * hipe_bifs_gbif_address(F,A) -> address or false
 */
#define GBIF_LIST(ATOM,ARY,CFUN) extern Eterm gbif_##CFUN(void);
#include "hipe_gbif_list.h"
#undef GBIF_LIST

BIF_RETTYPE hipe_bifs_gbif_address_2(BIF_ALIST_2)
BIF_ADECL_2
{
    unsigned arity;
    void *address;

    if( is_not_atom(BIF_ARG_1) || is_not_small(BIF_ARG_2) )
	BIF_RET(am_false);	/* error or false, does it matter? */
    arity = signed_val(BIF_ARG_2);
    /* XXX: replace with a hash table later */
    do { /* trick to let us use 'break' instead of 'goto' */
#define GBIF_LIST(ATOM,ARY,CFUN) if(BIF_ARG_1 == ATOM && arity == ARY) { address = CFUN; break; }
#include "hipe_gbif_list.h"
#undef GBIF_LIST
	printf("\r\n%s: guard BIF ", __FUNCTION__);
	fflush(stdout);
	print_atom(atom_val(BIF_ARG_1), COUT);
	printf("/%u isn't listed in hipe_gbif_list.h\r\n", arity);
	BIF_RET(am_false);
    } while(0);
    BIF_RET(address_to_term(address, BIF_P));
}

BIF_RETTYPE hipe_bifs_atom_to_word_1(BIF_ALIST_1)
BIF_ADECL_1
{
    if( is_not_atom(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

BIF_RETTYPE hipe_bifs_term_to_word_1(BIF_ALIST_1)
BIF_ADECL_1
{
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

BIF_RETTYPE hipe_bifs_emu_stub_3(BIF_ALIST_3)
BIF_ADECL_3
{
  Export* export_entry;

  if( is_not_atom(BIF_ARG_1) ||
      is_not_atom(BIF_ARG_2) ||
      is_not_small(BIF_ARG_3) ||
      signed_val(BIF_ARG_3) < 0 )
    BIF_ERROR(BIF_P, BADARG);

  export_entry = erts_export_put(BIF_ARG_1, BIF_ARG_2, signed_val(BIF_ARG_3));
  BIF_RET(address_to_term(export_entry->address, BIF_P));
}

DbTable* code_tb = (DbTable*) NULL;
void init_code_table(void)
{
    Uint32 status;
    int keypos;
    int cret;

    status = DB_NORMAL | DB_SET | DB_LHASH | DB_PROTECTED;
    keypos = 1;

    code_tb = (DbTable*) fix_alloc_from(55, table_desc);
    code_tb->common.status = status;
    code_tb->common.keypos = keypos;
    code_tb->common.nitems = 0;
    cret = db_create_hash((Process *)NULL, &(code_tb->hash));

    if (cret != DB_ERROR_NONE) {
	printf("HiPE: Not enough mem for code table!\n");
	exit(1); /* TODO: Die gracefully */
    }
}

BIF_RETTYPE hipe_bifs_set_funinfo_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int cret;
    Eterm ret;

    if (code_tb == (DbTable*) NULL)
	init_code_table();

    if (is_not_tuple(BIF_ARG_1) ||
	(arityval(*tuple_val(BIF_ARG_1)) < code_tb->common.keypos)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    cret = db_put_hash(BIF_P, &(code_tb->hash), BIF_ARG_1, &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* XXX: this is really a primop, not a BIF */
BIF_RETTYPE hipe_conv_big_to_float(BIF_ALIST_1)
BIF_ADECL_1
{
    Eterm res;
    Eterm* hp;
    FloatDef f;

    if( is_not_big(BIF_ARG_1) ) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if( big_to_double(BIF_ARG_1, &f.fd) < 0 ) {
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, 3);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

BIF_RETTYPE hipe_bifs_get_funinfo_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int cret;
    Eterm ret;

    if (code_tb == (DbTable*) NULL)
	init_code_table();

    cret = db_get_hash(BIF_P, &(code_tb->hash), BIF_ARG_1, &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/*
  At least parts of this should be inlined in native code.
  The rest could be made a primop used by both the emulator and
  native code...
*/

BIF_RETTYPE hipe_bifs_make_fun_3(BIF_ALIST_3)
BIF_ADECL_3
{
  Eterm free_vars;
  Eterm mod;
  Eterm *tp;
  Uint index;
  Uint uniq;
  Uint num_free;
  Eterm tmp_var;
  Uint *tmp_ptr;
  unsigned needed;
  ErlFunThing *funp;
  Eterm *hp;
  int i;

  if (is_not_list(BIF_ARG_1) && is_not_nil(BIF_ARG_1)) {
    printf("Not a list\n");
    BIF_ERROR(BIF_P, BADARG);
  }

  free_vars = BIF_ARG_1;

  if (is_not_atom(BIF_ARG_2)) {
    printf("Not an atom\n");
    BIF_ERROR(BIF_P, BADARG);
  }
  mod = BIF_ARG_2;

  if (is_not_tuple(BIF_ARG_3) ||
      (arityval(*tuple_val(BIF_ARG_3)) != 3 )) {
    printf("Not an good tuple\n");
    BIF_ERROR(BIF_P, BADARG);
  }
  tp = tuple_val(BIF_ARG_3);

  if(term_to_Uint(tp[1], &index) == 0) {
    printf("Bad index\n");
    BIF_ERROR(BIF_P, BADARG);
  }
  if(term_to_Uint(tp[2], &uniq) == 0){
    printf("Bad unique\n");
    BIF_ERROR(BIF_P, BADARG);
  }
  if(term_to_Uint(tp[3], &num_free) == 0){
    printf("Bad num_free\n");
    BIF_ERROR(BIF_P, BADARG);
  }

  needed = ERL_FUN_SIZE + num_free;
  funp = (ErlFunThing *) HAlloc(BIF_P, needed);
  hp = funp->env;

  funp->thing_word = HEADER_FUN;

  /* Need a ErlFunEntry* fe
     fe->refc++;
     funp->fe = fe; */

  funp->num_free = num_free;
  funp->creator = BIF_P->id;
  for (i = 0; i < num_free; i++) {
    if (is_nil(free_vars)) {
      printf("to few free vars\n");
      BIF_ERROR(BIF_P, BADARG);
    }
    tmp_ptr = list_val(free_vars);
    tmp_var = CAR(tmp_ptr);
    free_vars = CDR(tmp_ptr);
    *hp++ = tmp_var;
  }
  if (is_not_nil(free_vars))  {
    printf("to many free vars\n");
    BIF_ERROR(BIF_P, BADARG);
  }

#ifndef SHARED_HEAP
  funp->next = MSO(BIF_P).funs;
  MSO(BIF_P).funs = funp;
#endif

  BIF_RET(make_fun(funp));
}

BIF_RETTYPE hipe_bifs_make_fe_3(BIF_ALIST_3)
BIF_ADECL_3
{
  /*
     args: Nativecodeaddress, Module, {Uniq, Index, BeamAddress}
   */

  Eterm mod;
  Uint index;
  Uint uniq;
  void *beam_address;
  ErlFunEntry* fe;
  Eterm *tp;
  void *native_address;

  native_address = term_to_address(BIF_ARG_1);
  if( !native_address )
    BIF_ERROR(BIF_P, BADARG);

  if (is_not_atom(BIF_ARG_2)) {
    BIF_ERROR(BIF_P, BADARG);
  }
  mod = BIF_ARG_2;

  if (is_not_tuple(BIF_ARG_3) ||
      (arityval(*tuple_val(BIF_ARG_3)) != 3 )) {
    BIF_ERROR(BIF_P, BADARG);
  }
  tp = tuple_val(BIF_ARG_3);
  if(term_to_Uint(tp[1], &uniq) == 0){
    printf("Bad unique\n");
    BIF_ERROR(BIF_P, BADARG);
  }
  if(term_to_Uint(tp[2], &index) == 0) {
    printf("Bad index\n");
    BIF_ERROR(BIF_P, BADARG);
  }

  beam_address = term_to_address(tp[3]);
  if( !beam_address )
    BIF_ERROR(BIF_P, BADARG);

  fe = erts_get_fun_entry(mod, uniq, index);
  if (fe == NULL) {
    int i = atom_val(mod);
    char atom_buf[256];

    atom_buf[0] = '\0';
    strncat(atom_buf, atom_tab(i)->name, atom_tab(i)->len);
    printf("no fun entry for %s %ld:%ld\n", atom_buf, uniq, index);
    BIF_ERROR(BIF_P, BADARG);
  }
  fe->native_address = native_address;
  BIF_RET(address_to_term((void *)fe, BIF_P));
}

int hipe_patch_address(Uint *address, Eterm patchtype, Uint value)
{
    switch( patchtype ) {
#if defined(__i386__)
      case am_address:
	{	/* address points to a disp32 or imm32 operand */
	    *address = value;
	    return 1;
	}
#endif
#if defined(__sparc__)
      case am_sethi:
	{	 /* address points to a SETHI insn */
	    unsigned int high22 = value >> 10;
	    unsigned int sethi_insn = *address;
	    *address = (sethi_insn & 0xFFC00000) | high22;
	    /* Flush the I-cache. */
	    asm volatile("flush %0"
			 : /* no outputs */
			 : "r"(address)
			 : "memory");
	    return 1;
	}
      case am_or:
	{	/* address points to an OR reg,imm,reg insn */
	    unsigned int low10 = value & 0x3FF;
	    unsigned int or_insn = *address;
	    *address = (or_insn & 0xFFFFE000) | low10;
	    /* Flush the I-cache. */
	    asm volatile("flush %0"
			 : /* no outputs */
			 : "r"(address)
			 : "memory");
	    return 1;
	}
#endif
      default:
	{
	    fprintf(stderr, "hipe_patch_address: unknown patchtype %#lx\r\n",
		    patchtype);
	    return 0;
	}
    }
}

BIF_RETTYPE hipe_bifs_check_crc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    Uint crc;

    term_to_Uint(BIF_ARG_1, &crc);
    if( !crc )
	BIF_ERROR(BIF_P, BADARG);
    if( crc == HIPE_SYSTEM_CRC )
	BIF_RET(am_true);
    BIF_RET(am_false);
}
