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
#include "beam_catches.h"
#include "erl_db.h"
#include "erl_bits.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_bif0.h"
#endif

#define BeamOpCode(Op)	((uint32)BeamOp(Op))

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

#ifdef UNIFIED_HEAP
    /* Ugly, but must prevent code loading in this case. */
    BIF_ERROR(BIF_P, SYSTEM_LIMIT);
#endif
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
 * Catch table accesses.
 */

BIF_RETTYPE hipe_bifs_catch_index_to_word_1(BIF_ALIST_1)
BIF_ADECL_1
{
    int i;

    if( is_not_small(BIF_ARG_1) ||
	(i = signed_val(BIF_ARG_1)) < 0 )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(make_catch(i), BIF_P));
}

BIF_RETTYPE hipe_bifs_catch_table_nil_0(BIF_ALIST_0)
BIF_ADECL_0
{
    BIF_RET(make_small(BEAM_CATCHES_NIL));
}

BIF_RETTYPE hipe_bifs_catch_table_insert_2(BIF_ALIST_2)
BIF_ADECL_2
{
    void *address;

    if( is_not_small(BIF_ARG_2) ||
	(address = term_to_address(BIF_ARG_1)) == NULL )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(beam_catches_cons(address, signed_val(BIF_ARG_2))));
}

BIF_RETTYPE hipe_bifs_catch_table_remove_3(BIF_ALIST_3)
BIF_ADECL_3
{
    int head;
    void *code;
    unsigned code_bytes;

    if( is_not_small(BIF_ARG_1) ||
	(head = signed_val(BIF_ARG_1)) < BEAM_CATCHES_NIL ||
	(code = term_to_address(BIF_ARG_2)) == NULL ||
	!is_small(BIF_ARG_3) ||
	(code_bytes = signed_val(BIF_ARG_3)) < 0 )
	BIF_ERROR(BIF_P, BADARG);
    beam_catches_delmod(head, code, code_bytes);
    BIF_RET(NIL);
}

/*
 * Native-code stack descriptor hash table.
 */
static Hash sdesc_table;

static HashValue sdesc_hash(struct sdesc *x)
{
    return x->bucket.hvalue;
}

static int sdesc_cmp(struct sdesc *x, struct sdesc *y)
{
    return 0;	/* only called if the hvalues already match */
}

static struct sdesc *sdesc_alloc(struct sdesc *x)
{
    return x;	/* pre-allocated */
}

void hipe_init_sdesc_table(struct sdesc *sdesc)
{
    HashFunctions f;

    f.hash = (H_FUN) sdesc_hash;
    f.cmp = (HCMP_FUN) sdesc_cmp;
    f.alloc = (HALLOC_FUN) sdesc_alloc;
    f.free = NULL;	/* XXX: needed if we ever start deallocating code */

    hash_init(&sdesc_table, "sdesc_table", 500, f);

    if( hash_put(&sdesc_table, sdesc) != sdesc ) {
	fprintf(stderr, "%s: initial hash_put() failed\r\n",__FUNCTION__);
	abort();
    }
}

struct sdesc *hipe_find_sdesc(unsigned long ra)
{
    /* inlined & specialised version of hash_get() for speed */
    HashBucket *b = sdesc_table.bucket[ra % sdesc_table.size];
    for(; b; b = b->next)
	if( b->hvalue == ra )
	    return (struct sdesc*)b;
    return 0;
}

BIF_RETTYPE hipe_bifs_enter_sdesc_1(BIF_ALIST_1)
BIF_ADECL_1
{
    struct sdesc *sdesc;

    sdesc = term_to_address(BIF_ARG_1);
    if( !sdesc )
	BIF_ERROR(BIF_P, BADARG);
    if( hash_put(&sdesc_table, sdesc) != sdesc ) {
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
	/* XXX: might return am_false instead, but the HiPE compiler
	   "knows" about this error exit :-( */
	BIF_ERROR(BIF_P, BADARG);

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
	check_bif(am_inc_stack_0, nbif_inc_stack_0);
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

	check_bif(am_cmp_2, nbif_cmp_2);
	check_bif(am_op_exact_eqeq_2, nbif_eq_2);

	check_bif(am_test, nbif_test);
#undef check_bif
      default:
	/* XXX: might return am_false instead, but the HiPE compiler
	   "knows" about this error exit :-( */
	BIF_ERROR(BIF_P, BADARG);
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
	printf("\r\n" "%s: guard BIF ", __FUNCTION__);
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
    uint32 status;
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

  funp->next = BIF_P->off_heap.funs;
  BIF_P->off_heap.funs = funp;

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
