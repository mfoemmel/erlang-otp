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
#include "erl_bits.h"
#include "erl_binary.h"
#ifdef HIPE
#include <stddef.h>	/* offsetof() */
#include "hipe_arch.h"
#include "hipe_stack.h"
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

int term_to_Sint32(Eterm term, Sint *sp)
{
    Sint val;

    if( !term_to_Sint(term, &val) )
	return 0;
    if( (Sint)(Sint32)val != val )
	return 0;
    *sp = val;
    return 1;
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

void *term_to_address(Eterm arg)
{
    Uint u;
    return term_to_Uint(arg, &u) ? (void*)u : NULL;
}

Eterm address_to_term(void *address, Process *p)
{
    return Uint_to_term((Uint)address, p);
}

/*
 * BIFs for reading and writing memory. Used internally by HiPE.
 */
#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_read_u8_1(BIF_ALIST_1)
{
    unsigned char *address = term_to_address(BIF_ARG_1);
    if( !address )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(make_small(*address));
}
#endif

BIF_RETTYPE hipe_bifs_read_u32_1(BIF_ALIST_1)
{
    Uint32 *address = term_to_address(BIF_ARG_1);
    if( !address || !hipe_word32_address_ok(address) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(*address, BIF_P));
}

BIF_RETTYPE hipe_bifs_write_u8_2(BIF_ALIST_2)
{
    unsigned char *address;

    address = term_to_address(BIF_ARG_1);
    if( !address || is_not_small(BIF_ARG_2) )
	BIF_ERROR(BIF_P, BADARG);
    *address = unsigned_val(BIF_ARG_2);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_write_s32_2(BIF_ALIST_2)
{
    Sint32 *address;
    Sint value;

    address = term_to_address(BIF_ARG_1);
    if( !address || !hipe_word32_address_ok(address) )
	BIF_ERROR(BIF_P, BADARG);
    if( !term_to_Sint32(BIF_ARG_2, &value) )
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_write_u32_2(BIF_ALIST_2)
{
    Uint32 *address;
    Uint value;

    address = term_to_address(BIF_ARG_1);
    if( !address || !hipe_word32_address_ok(address) )
	BIF_ERROR(BIF_P, BADARG);
    if( !term_to_Uint(BIF_ARG_2, &value) )
	BIF_ERROR(BIF_P, BADARG);
    if( (Uint)(Uint32)value != value )
	BIF_ERROR(BIF_P, BADARG);
    *address = value;
    hipe_flush_icache_word(address);
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
{
    Eterm *hp;
    Sint nelts, i;

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
{
    if( is_not_array(BIF_ARG_1) ) {
	if( is_nil(BIF_ARG_1) )	/* NIL represents empty arrays */
	    BIF_RET(make_small(0));
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(make_small(array_header_arity(array_val(BIF_ARG_1)[0])));
}

BIF_RETTYPE hipe_bifs_array_sub_2(BIF_ALIST_2)
{
    Uint i;

    if( is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[i+1]);
}

BIF_RETTYPE hipe_bifs_array_update_3(BIF_ALIST_3)
{
    Uint i;

    if( is_not_immed(BIF_ARG_3) ||
	is_not_small(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	(i = unsigned_val(BIF_ARG_2)) >= array_length(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[i+1] = BIF_ARG_3;
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_ref_1(BIF_ALIST_1)
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
{
    if( is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(array_val(BIF_ARG_1)[1]);
}

BIF_RETTYPE hipe_bifs_ref_set_2(BIF_ALIST_2)
{
    if( is_not_immed(BIF_ARG_2) ||
	is_not_array(BIF_ARG_1) ||
	array_val(BIF_ARG_1)[0] != make_array_header(1) )
	BIF_ERROR(BIF_P, BADARG);
    array_val(BIF_ARG_1)[1] = BIF_ARG_2;
    BIF_RET(NIL);
}

/*
 * Allocate memory and copy machine code to it.
 */
BIF_RETTYPE hipe_bifs_enter_code_2(BIF_ALIST_2)
{
    Uint nrbytes;
    void *bytes;
    void *address;
    Eterm trampolines;
    Eterm *hp;

    if( is_not_binary(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = binary_size(BIF_ARG_1);
    GET_BINARY_BYTES(BIF_ARG_1, bytes);
    trampolines = NIL;
#ifdef HIPE_ALLOC_CODE
    address = HIPE_ALLOC_CODE(nrbytes, BIF_ARG_2, &trampolines, BIF_P);
    if( !address )
	BIF_ERROR(BIF_P, BADARG);
#else
    if( is_not_nil(BIF_ARG_2) )
	BIF_ERROR(BIF_P, BADARG);
    address = erts_alloc(ERTS_ALC_T_HIPE, nrbytes);
#endif
    memcpy(address, bytes, nrbytes);
    hipe_flush_icache_range(address, nrbytes);
    hp = HAlloc(BIF_P, 3);
    hp[0] = make_arityval(2);
    hp[1] = address_to_term(address, BIF_P);
    hp[2] = trampolines;
    BIF_RET(make_tuple(hp));
}

/*
 * Allocate memory for arbitrary non-Erlang data.
 */
BIF_RETTYPE hipe_bifs_alloc_data_2(BIF_ALIST_2)
{
    Uint align, nrbytes;
    void *block;

    if( is_not_small(BIF_ARG_1) || is_not_small(BIF_ARG_2) ||
	(align = unsigned_val(BIF_ARG_1),
	 align != sizeof(long) && align != sizeof(double)) )
	BIF_ERROR(BIF_P, BADARG);
    nrbytes = unsigned_val(BIF_ARG_2);
    block = erts_alloc(ERTS_ALC_T_HIPE, nrbytes);
    if( (unsigned long)block & (align-1) )
	fprintf(stderr, "Yikes! erts_alloc() returned misaligned address %p\r\n", block);
    BIF_RET(address_to_term(block, BIF_P));
}

/*
 * Memory area for constant Erlang terms.
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
#define CONSTANTS_BYTES	(1536*1024*sizeof(Eterm))  /* 1.5 M words */

static Eterm *constants_alloc(unsigned nwords)
{
    Eterm *next;

    /* initialise at the first call */
    if( (next = hipe_constants_next) == NULL ) {
	next = (Eterm*)erts_alloc(ERTS_ALC_T_HIPE, CONSTANTS_BYTES);
	hipe_constants_start = next;
	hipe_constants_next = next;
	constants_avail_words = CONSTANTS_BYTES / sizeof(Eterm);
    }
    if( nwords > constants_avail_words ) {
	fprintf(stderr, "Native code constants pool depleted!\r\n");
	/* Must terminate immediately. erl_exit() seems to
	   continue running some code which then SIGSEGVs. */
	exit(1);
    }
    constants_avail_words -= nwords;
    hipe_constants_next = next + nwords;
    return next;
}

BIF_RETTYPE hipe_bifs_constants_size_0(BIF_ALIST_0)
{
    BIF_RET(make_small(hipe_constants_next - hipe_constants_start));
}

/*
 * Merging constant Erlang terms.
 * Uses the constants pool and a hash table of all top-level
 * terms merged so far. (Sub-terms are not merged.)
 */
struct const_term {
    HashBucket bucket;
    Eterm val;		/* tagged pointer to mem[0] */
    Eterm mem[1];	/* variable size */
};

static Hash const_term_table;
static ErlOffHeap const_term_table_off_heap;

static HashValue const_term_hash(void *tmpl)
{
    return make_hash2((Eterm)tmpl);
}

static int const_term_cmp(void *tmpl, void *bucket)
{
    return !eq((Eterm)tmpl, ((struct const_term*)bucket)->val);
}

static void *const_term_alloc(void *tmpl)
{
    Eterm obj;
    Uint size;
    Eterm *hp;
    struct const_term *p;

    obj = (Eterm)tmpl;
    ASSERT(is_not_immed(obj));
    size = size_object(obj);

    p = (struct const_term*)constants_alloc(size + (offsetof(struct const_term, mem)/sizeof(Eterm)));

    /* I have absolutely no idea if having a private 'off_heap'
       works or not. _Some_ off_heap object is required for
       REFC_BINARY and FUN values, but _where_ it should be is
       a complete mystery to me. */
    hp = &p->mem[0];
    p->val = copy_struct(obj, size, &hp, &const_term_table_off_heap);

    return &p->bucket;
}

static void init_const_term_table(void)
{
    HashFunctions f;
    f.hash = (H_FUN) const_term_hash;
    f.cmp = (HCMP_FUN) const_term_cmp;
    f.alloc = (HALLOC_FUN) const_term_alloc;
    f.free = (HFREE_FUN) NULL;
    hash_init(ERTS_ALC_T_HIPE, &const_term_table, "const_term_table", 97, f);
}

BIF_RETTYPE hipe_bifs_merge_term_1(BIF_ALIST_1)
{
    static int init_done = 0;
    struct const_term *p;
    Eterm val;

    val = BIF_ARG_1;
    if( is_not_immed(val) ) {
	if( !init_done ) {
	    init_const_term_table();
	    init_done = 1;
	}
	p = (struct const_term*)hash_put(&const_term_table, (void*)val);
	val = p->val;
    }
    BIF_RET(val);
}

struct mfa {
    Eterm mod;
    Eterm fun;
    Uint  ari;
};

static int term_to_mfa(Eterm term, struct mfa *mfa)
{
    Eterm mod, fun, a;
    Uint ari;

    if (is_not_tuple(term))
	return 0;
    if (tuple_val(term)[0] != make_arityval(3))
	return 0;
    mod = tuple_val(term)[1];
    if (is_not_atom(mod))
	return 0;
    mfa->mod = mod;
    fun = tuple_val(term)[2];
    if (is_not_atom(fun))
	return 0;
    mfa->fun = fun;
    a = tuple_val(term)[3];
    if (is_not_small(a))
	return 0;
    ari = unsigned_val(a);
    if (ari > 255)
	return 0;
    mfa->ari = ari;
    return 1;
}

#ifdef DEBUG_LINKER
static void print_mfa(Eterm mod, Eterm fun, unsigned int ari)
{
    display(mod, COUT);
    printf(":");
    display(fun, COUT);
    printf("/%u", ari);
}
#endif

/*
 * Convert {M,F,A} to pointer to first insn after initial func_info.
 */
static Uint *hipe_find_emu_address(Eterm mod, Eterm name, unsigned int arity)
{
    Module *modp;
    Uint *code_base;
    int i, n;

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

Uint *hipe_bifs_find_pc_from_mfa(Eterm term)
{
    struct mfa mfa;

    if (!term_to_mfa(term, &mfa))
	return NULL;
    return hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);
}

BIF_RETTYPE hipe_bifs_fun_to_address_1(BIF_ALIST_1)
{
    Eterm *pc = hipe_bifs_find_pc_from_mfa(BIF_ARG_1);
    if( !pc )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(address_to_term(pc, BIF_P));
}

static void *hipe_get_emu_address(Eterm m, Eterm f, unsigned int arity, int is_remote)
{
    void *address = NULL;
    if (!is_remote)
	address = hipe_find_emu_address(m, f, arity);
    if( !address ) {
	/* if not found, stub it via the export entry */
	Export *export_entry = erts_export_put(m, f, arity);
	address = export_entry->address;
    }
    return address;
}

#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_get_emu_address_1(BIF_ALIST_1)
{
    struct mfa mfa;
    void *address;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    address = hipe_get_emu_address(mfa.mod, mfa.fun, mfa.ari);
    BIF_RET(address_to_term(address, BIF_P));
}
#endif

BIF_RETTYPE hipe_bifs_set_native_address_3(BIF_ALIST_3)
{
    Eterm *pc;
    void *address;
    int is_closure;
    struct mfa mfa;

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

    /* The mfa is needed again later, otherwise we could
       simply have called hipe_bifs_find_pc_from_mfa(). */
    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    pc = hipe_find_emu_address(mfa.mod, mfa.fun, mfa.ari);

    if( pc ) {
	hipe_mfa_save_orig_beam_op(mfa.mod, mfa.fun, mfa.ari, pc);
#if HIPE
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mfa.mod, mfa.fun, mfa.ari);
	printf(": planting call trap to %p at BEAM pc %p\r\n", address, pc);
#endif
	hipe_set_call_trap(pc, address, is_closure);
	BIF_RET(am_true);
#endif
    }
#ifdef DEBUG_LINKER
    printf("%s: ", __FUNCTION__);
    print_mfa(mfa.mod, mfa.fun, mfa.ari);
    printf(": no BEAM pc found\r\n");
#endif
    BIF_RET(am_false);
}

#if 0 /* XXX: unused */
/*
 * hipe_bifs_address_to_fun(Address)
 *    - Address is the address of the start of a emu function's code
 *    - returns {Module, Function, Arity}
 */
BIF_RETTYPE hipe_bifs_address_to_fun_1(BIF_ALIST_1)
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
#endif

BIF_RETTYPE hipe_bifs_enter_sdesc_1(BIF_ALIST_1)
{
    struct sdesc *sdesc;

    sdesc = hipe_decode_sdesc(BIF_ARG_1);
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

    hash_init(ERTS_ALC_T_NBIF_TABLE, &nbif_table, "nbif_table", 500, f);

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
{
    void *res;

    switch( BIF_ARG_1 ) {
#define check_bif(Name,Address)	case Name: res = Address; break
        check_bif(am_erl_fp_exception, (int*)&erl_fp_exception); /* ignore volatile */
        check_bif(am_erts_mb, &erts_mb);
	check_bif(am_erts_save_mb, &erts_save_mb);
#ifdef SHARED_HEAP
	check_bif(am_erts_global_mso, &erts_global_offheap);
#endif
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
	check_bif(am_select_msg, nbif_select_msg);
	check_bif(am_set_timeout, nbif_set_timeout);
	check_bif(am_rethrow, nbif_rethrow);

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
	check_bif(am_bs_allocate, nbif_bs_allocate);
	check_bif(am_bs_put_big_integer, nbif_bs_put_big_integer);
	check_bif(am_bs_put_small_float, nbif_bs_put_small_float);

	check_bif(am_cmp_2, nbif_cmp_2);
	check_bif(am_op_exact_eqeq_2, nbif_eq_2);

	check_bif(am_hipe_apply, nbif_apply);
	check_bif(am_find_na_or_make_stub, nbif_find_na_or_make_stub);

	check_bif(am_conv_big_to_float, nbif_conv_big_to_float);

#undef check_bif
      default:
	res = hipe_arch_primop_address(BIF_ARG_1);
	if( res )
	    break;
	BIF_RET(am_false);
    }
    BIF_RET(address_to_term(res, BIF_P));
}

#if 0 /* XXX: unused */
/*
 * hipe_bifs_gbif_address(F,A) -> address or false
 */
#define GBIF_LIST(ATOM,ARY,CFUN) extern Eterm gbif_##CFUN(void);
#include "hipe_gbif_list.h"
#undef GBIF_LIST

BIF_RETTYPE hipe_bifs_gbif_address_2(BIF_ALIST_2)
{
    Uint arity;
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
	printf("/%lu isn't listed in hipe_gbif_list.h\r\n", arity);
	BIF_RET(am_false);
    } while(0);
    BIF_RET(address_to_term(address, BIF_P));
}
#endif

BIF_RETTYPE hipe_bifs_atom_to_word_1(BIF_ALIST_1)
{
    if( is_not_atom(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

BIF_RETTYPE hipe_bifs_term_to_word_1(BIF_ALIST_1)
{
    BIF_RET(Uint_to_term(BIF_ARG_1, BIF_P));
}

/*
 * The funinfo (code table) data structure records the following
 * about a given MFA:
 *
 * - the list of MFAs it calls [key: refers_to]
 * - the list of MFAs that calls it [key: referred_from]
 *
 * This is currently just a pile of terms in an ETS table.
 * XXX: Reimplement and provide higher-level interfaces.
 */

DbTable* code_tb = (DbTable*) NULL;
static void init_code_table(void)
{
    Uint32 status;
    int keypos;
    int cret;

    status = DB_NORMAL | DB_SET | DB_LHASH | DB_PROTECTED;
    keypos = 1;

    code_tb = (DbTable*) erts_alloc(ERTS_ALC_T_DB_TABLE, sizeof(DbTable));
    code_tb->common.status = status;
    code_tb->common.keypos = keypos;
    code_tb->common.nitems = 0;
    cret = db_create_hash((Process *)NULL, &(code_tb->hash));

    if (cret != DB_ERROR_NONE) {
	printf("HiPE: Not enough mem for code table!\n");
	exit(1); /* TODO: Die gracefully */
    }
}

/* could be a BIF, but we want control over the table contents */
static BIF_RETTYPE hipe_bifs_set_funinfo_3(BIF_ALIST_3)
{
    Eterm tuple[3+3];
    int cret;
    Eterm ret;

    if (code_tb == (DbTable*) NULL)
	init_code_table();

    tuple[0] = make_arityval(2);
    tuple[1] = BIF_ARG_1;	/* the MFA */
    tuple[2] = BIF_ARG_2;	/* the key */
    tuple[3] = make_arityval(2);
    tuple[4] = make_tuple(&tuple[0]);
    tuple[5] = BIF_ARG_3;	/* the data */

    cret = db_put_hash(BIF_P, &(code_tb->hash), make_tuple(&tuple[3]), &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

/* could be a BIF, but we want control over the table contents */
static BIF_RETTYPE hipe_bifs_get_funinfo_2(BIF_ALIST_2)
{
    Eterm tuple[3];
    int cret;
    Eterm ret;

    if (code_tb == (DbTable*) NULL)
	init_code_table();

    tuple[0] = make_arityval(2);
    tuple[1] = BIF_ARG_1;	/* the MFA */
    tuple[2] = BIF_ARG_2;	/* the key */

    cret = db_get_hash(BIF_P, &(code_tb->hash), make_tuple(&tuple[0]), &ret);
    switch (cret) {
    case DB_ERROR_NONE:
	BIF_RET(ret);
    case DB_ERROR_SYSRES:
	BIF_ERROR(BIF_P, SYSTEM_LIMIT);
    default:
	BIF_ERROR(BIF_P, BADARG);
    }
}

BIF_RETTYPE hipe_bifs_set_funinfo_refers_to_2(BIF_ALIST_2)
{
    return hipe_bifs_set_funinfo_3(BIF_P, BIF_ARG_1, am_refers_to, BIF_ARG_2);
}

BIF_RETTYPE hipe_bifs_get_funinfo_refers_to_1(BIF_ALIST_1)
{
    return hipe_bifs_get_funinfo_2(BIF_P, BIF_ARG_1, am_refers_to);
}

BIF_RETTYPE hipe_bifs_set_funinfo_referred_from_2(BIF_ALIST_2)
{
    return hipe_bifs_set_funinfo_3(BIF_P, BIF_ARG_1, am_referred_from, BIF_ARG_2);
}

BIF_RETTYPE hipe_bifs_get_funinfo_referred_from_1(BIF_ALIST_1)
{
    return hipe_bifs_get_funinfo_2(BIF_P, BIF_ARG_1, am_referred_from);
}

/* XXX: this is really a primop, not a BIF */
BIF_RETTYPE hipe_conv_big_to_float(BIF_ALIST_1)
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
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
    res = make_float(hp);
    PUT_DOUBLE(f, hp);
    BIF_RET(res);
}

#if 0 /* XXX: unused */
/*
  At least parts of this should be inlined in native code.
  The rest could be made a primop used by both the emulator and
  native code...
*/

BIF_RETTYPE hipe_bifs_make_fun_3(BIF_ALIST_3)
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
#ifndef HYBRID /* FIND ME! */
  funp->next = MSO(BIF_P).funs;
  MSO(BIF_P).funs = funp;
#endif
#endif

  BIF_RET(make_fun(funp));
}
#endif

BIF_RETTYPE hipe_bifs_make_fe_3(BIF_ALIST_3)
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

#if 0 /* XXX: unused */
BIF_RETTYPE hipe_bifs_make_native_stub_2(BIF_ALIST_2)
{
    void *beamAddress;
    Uint beamArity;
    void *stubAddress;

    if( (beamAddress = term_to_address(BIF_ARG_1)) == 0 ||
	is_not_small(BIF_ARG_2) ||
	(beamArity = unsigned_val(BIF_ARG_2)) >= 256 )
	BIF_ERROR(BIF_P, BADARG);
    stubAddress = hipe_make_native_stub(beamAddress, beamArity);
    BIF_RET(address_to_term(stubAddress, BIF_P));
}
#endif

/*
 * MFA info hash table:
 * - maps MFA to native code entry point
 * - maps MFA to most recent trampoline [if powerpc]
 *
 * XXX: migrate refers_to/referred_from from the
 * funinfo ETS table to this hash table
 */
struct hipe_mfa_info {
    struct {
	unsigned long hvalue;
	struct hipe_mfa_info *next;
    } bucket;
    Eterm m;	/* atom */
    Eterm f;	/* atom */
    unsigned int a;
    void *remote_address;
    void *local_address;
    Eterm *beam_code;
    Uint orig_beam_op;
#ifdef __powerpc__
    void *trampoline;
#endif
};

static struct {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    struct hipe_mfa_info **bucket;
} hipe_mfa_info_table;

#define HIPE_MFA_HASH(M,F,A)	((M) * (F) + (A))

static struct hipe_mfa_info **hipe_mfa_info_table_alloc_bucket(unsigned int size)
{
    unsigned long nbytes = size * sizeof(struct hipe_mfa_info*);
    struct hipe_mfa_info **bucket = erts_alloc(ERTS_ALC_T_HIPE, nbytes);
    sys_memzero(bucket, nbytes);
    return bucket;
}

static void hipe_mfa_info_table_grow(void)
{
    unsigned int old_size, new_size, new_mask;
    struct hipe_mfa_info **old_bucket, **new_bucket;
    unsigned int i;

    old_size = 1 << hipe_mfa_info_table.log2size;
    hipe_mfa_info_table.log2size += 1;
    new_size = 1 << hipe_mfa_info_table.log2size;
    new_mask = new_size - 1;
    hipe_mfa_info_table.mask = new_mask;
    old_bucket = hipe_mfa_info_table.bucket;
    new_bucket = hipe_mfa_info_table_alloc_bucket(new_size);
    hipe_mfa_info_table.bucket = new_bucket;
    for(i = 0; i < old_size; ++i) {
	struct hipe_mfa_info *b = old_bucket[i];
	while( b != NULL ) {
	    struct hipe_mfa_info *next = b->bucket.next;
	    unsigned int j = b->bucket.hvalue & new_mask;
	    b->bucket.next = new_bucket[j];
	    new_bucket[j] = b;
	    b = next;
	}
    }
    erts_free(ERTS_ALC_T_HIPE, old_bucket);
}

static struct hipe_mfa_info *hipe_mfa_info_table_alloc(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *res;

    res = (struct hipe_mfa_info*)erts_alloc(ERTS_ALC_T_HIPE, sizeof(*res));
    res->m = m;
    res->f = f;
    res->a = arity;
    res->remote_address = NULL;
    res->local_address = NULL;
    res->beam_code = NULL;
    res->orig_beam_op = 0;
#ifdef __powerpc__
    res->trampoline = NULL;
#endif

    return res;
}

void hipe_mfa_info_table_init(void)
{
    unsigned int log2size, size;

    log2size = 10;
    size = 1 << log2size;
    hipe_mfa_info_table.log2size = log2size;
    hipe_mfa_info_table.mask = size - 1;
    hipe_mfa_info_table.used = 0;
    hipe_mfa_info_table.bucket = hipe_mfa_info_table_alloc_bucket(size);
}

static inline struct hipe_mfa_info *hipe_mfa_info_table_get(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for(; p; p = p->bucket.next)
	/* XXX: do we want to compare p->bucket.hvalue as well? */
	if (p->m == m && p->f == f && p->a == arity)
	    return p;
    return NULL;
}

#if 0 /* XXX: unused */
void *hipe_mfa_find_na(Eterm m, Eterm f, unsigned int arity)
{
    const struct hipe_mfa_info *p;

    p = hipe_mfa_info_table_get(m, f, arity);
    return p ? p->address : NULL;
}
#endif

static struct hipe_mfa_info *hipe_mfa_info_table_put(Eterm m, Eterm f, unsigned int arity)
{
    unsigned long h;
    unsigned int i;
    struct hipe_mfa_info *p;
    unsigned int size;

    h = HIPE_MFA_HASH(m, f, arity);
    i = h & hipe_mfa_info_table.mask;
    p = hipe_mfa_info_table.bucket[i];
    for(; p; p = p->bucket.next)
	/* XXX: do we want to compare p->bucket.hvalue as well? */
	if( p->m == m && p->f == f && p->a == arity )
	    return p;
    p = hipe_mfa_info_table_alloc(m, f, arity);
    p->bucket.hvalue = h;
    p->bucket.next = hipe_mfa_info_table.bucket[i];
    hipe_mfa_info_table.bucket[i] = p;
    hipe_mfa_info_table.used += 1;
    size = 1 << hipe_mfa_info_table.log2size;
    if( hipe_mfa_info_table.used > (4*size/5) )		/* rehash at 80% */
	hipe_mfa_info_table_grow();
    return p;
}

static void hipe_mfa_set_na(Eterm m, Eterm f, unsigned int arity, void *address, int is_exported)
{
    struct hipe_mfa_info *p = hipe_mfa_info_table_put(m, f, arity);
#ifdef DEBUG_LINKER
    printf("%s: ", __FUNCTION__);
    print_mfa(m, f, arity);
    printf(": changing address from %p to %p\r\n", p->local_address, address);
#endif
    p->local_address = address;
    if (is_exported)
	p->remote_address = address;
}

#ifdef __powerpc__
void *hipe_mfa_get_trampoline(Eterm m, Eterm f, unsigned int arity)
{
    struct hipe_mfa_info *p = hipe_mfa_info_table_put(m, f, arity);
    return p->trampoline;
}

void hipe_mfa_set_trampoline(Eterm m, Eterm f, unsigned int arity, void *trampoline)
{
    struct hipe_mfa_info *p = hipe_mfa_info_table_put(m, f, arity);
    p->trampoline = trampoline;
}
#endif

BIF_RETTYPE hipe_bifs_set_funinfo_native_address_3(BIF_ALIST_3)
{
    struct mfa mfa;
    void *address;
    int is_exported;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    address = term_to_address(BIF_ARG_2);
    if( !address )
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_3 == am_true)
	is_exported = 1;
    else if (BIF_ARG_3 == am_false)
	is_exported = 0;
    else
	BIF_ERROR(BIF_P, BADARG);
    hipe_mfa_set_na(mfa.mod, mfa.fun, mfa.ari, address, is_exported);
    BIF_RET(NIL);
}

BIF_RETTYPE hipe_bifs_invalidate_funinfo_native_addresses_1(BIF_ALIST_1)
{
    Eterm lst;
    struct mfa mfa;
    struct hipe_mfa_info *p;

    lst = BIF_ARG_1;
    while (is_list(lst)) {
	if (!term_to_mfa(CAR(list_val(lst)), &mfa))
	    BIF_ERROR(BIF_P, BADARG);
	lst = CDR(list_val(lst));
	p = hipe_mfa_info_table_get(mfa.mod, mfa.fun, mfa.ari);
	if (p) {
	    p->remote_address = NULL;
	    p->local_address = NULL;
	    if (p->beam_code) {
#ifdef DEBUG_LINKER
		printf("%s: ", __FUNCTION__);
		print_mfa(mfa.mod, mfa.fun, mfa.ari);
		printf(": removing call trap from BEAM pc %p (new op %#lx)\r\n",
		       p->beam_code, p->orig_beam_op);
#endif
		p->beam_code[0] = p->orig_beam_op;
		p->beam_code = NULL;
		p->orig_beam_op = 0;
	    } else {
#ifdef DEBUG_LINKER
		printf("%s: ", __FUNCTION__);
		print_mfa(mfa.mod, mfa.fun, mfa.ari);
		printf(": no call trap to remove\r\n");
#endif
	    }
	}
    }
    if (is_not_nil(lst))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(NIL);
}

void hipe_mfa_save_orig_beam_op(Eterm mod, Eterm fun, unsigned int ari, Eterm *pc)
{
    Uint orig_beam_op;
    struct hipe_mfa_info *p;

    orig_beam_op = pc[0];
    if (orig_beam_op != BeamOpCode(op_hipe_trap_call_closure) &&
	orig_beam_op != BeamOpCode(op_hipe_trap_call)) {
	p = hipe_mfa_info_table_put(mod, fun, ari);
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mod, fun, ari);
	printf(": saving orig op %#lx from BEAM pc %p\r\n", orig_beam_op, pc);
#endif
	p->beam_code = pc;
	p->orig_beam_op = orig_beam_op;
    } else {
#ifdef DEBUG_LINKER
	printf("%s: ", __FUNCTION__);
	print_mfa(mod, fun, ari);
	printf(": orig op %#lx already saved\r\n", orig_beam_op);
#endif
    }
}

static void *hipe_make_stub(Eterm m, Eterm f, unsigned int arity, int is_remote)
{
    void *BEAMAddress;
    void *StubAddress;

#if 0
    if( is_not_atom(m) || is_not_atom(f) || arity > 255 )
	return NULL;
#endif
    BEAMAddress = hipe_get_emu_address(m, f, arity, is_remote);
    StubAddress = hipe_make_native_stub(BEAMAddress, arity);
#if 0
    hipe_mfa_set_na(m, f, arity, StubAddress);
#endif
    return StubAddress;
}

static void *hipe_get_na_nofail(Eterm m, Eterm f, unsigned int a, int is_remote)
{
    struct hipe_mfa_info *p;
    void *address;

    p = hipe_mfa_info_table_get(m, f, a);
    if (p) {
	/* find address, predicting for a runtime apply call */
	address = p->remote_address;
	if (!is_remote)
	    address = p->local_address;
	if (address)
	    return address;

	/* bummer, install stub, checking if one already existed */
	address = p->remote_address;
	if (address)
	    return address;
    } else {
	p = hipe_mfa_info_table_put(m, f, a);
    }
    address = hipe_make_stub(m, f, a, is_remote);
    /* XXX: how to tell if a BEAM MFA is exported or not? */
    p->remote_address = address;
    return address;
}

/* used for apply/3 in hipe_mode_switch */
void *hipe_get_remote_na(Eterm m, Eterm f, unsigned int a)
{
    if (is_not_atom(m) || is_not_atom(f) || a > 255)
	return NULL;
    return hipe_get_na_nofail(m, f, a, 1);
}

/* primop, but called like a BIF for error handling purposes */
BIF_RETTYPE hipe_find_na_or_make_stub(BIF_ALIST_3)
{
    Uint arity;
    void *address;

    if( is_not_atom(BIF_ARG_1) || is_not_atom(BIF_ARG_2) )
	BIF_ERROR(BIF_P, BADARG);
    arity = unsigned_val(BIF_ARG_3); /* no error check */
    address = hipe_get_na_nofail(BIF_ARG_1, BIF_ARG_2, arity, 1);
    BIF_RET((Eterm)address);	/* semi-Ok */
}

BIF_RETTYPE hipe_bifs_find_na_or_make_stub_2(BIF_ALIST_2)
{
    struct mfa mfa;
    void *address;
    int is_remote;

    if (!term_to_mfa(BIF_ARG_1, &mfa))
	BIF_ERROR(BIF_P, BADARG);
    if (BIF_ARG_2 == am_true)
	is_remote = 1;
    else if (BIF_ARG_2 == am_false)
	is_remote = 0;
    else
	BIF_ERROR(BIF_P, BADARG);
    address = hipe_get_na_nofail(mfa.mod, mfa.fun, mfa.ari, is_remote);
    BIF_RET(address_to_term(address, BIF_P));
}

BIF_RETTYPE hipe_bifs_check_crc_1(BIF_ALIST_1)
{
    Uint crc;

    term_to_Uint(BIF_ARG_1, &crc);
    if( !crc )
	BIF_ERROR(BIF_P, BADARG);
    if( crc == HIPE_SYSTEM_CRC )
	BIF_RET(am_true);
    BIF_RET(am_false);
}

BIF_RETTYPE hipe_bifs_system_crc_1(BIF_ALIST_1)
{
    Uint crc;

    term_to_Uint(BIF_ARG_1, &crc);
    if( !crc )
	BIF_ERROR(BIF_P, BADARG);
    crc ^= (HIPE_SYSTEM_CRC ^ HIPE_LITERALS_CRC);
    BIF_RET(Uint_to_term(crc, BIF_P));
}

BIF_RETTYPE hipe_bifs_get_rts_param_1(BIF_ALIST_1)
{
    unsigned int is_defined;
    unsigned long value;

    if( is_not_small(BIF_ARG_1) )
	BIF_ERROR(BIF_P, BADARG);
    is_defined = 1;
    value = 0;
    switch( unsigned_val(BIF_ARG_1) ) {
	RTS_PARAMS_CASES
      default:
	BIF_ERROR(BIF_P, BADARG);
    }
    if( !is_defined )
	BIF_RET(NIL);
    BIF_RET(Uint_to_term(value, BIF_P));
}

void hipe_patch_address(Uint *address, Eterm patchtype, Uint value)
{
    switch( patchtype ) {
      case am_load_fe:
	hipe_patch_load_fe(address, value);
	return;
      default:
	fprintf(stderr, "%s: unknown patchtype %#lx\r\n",
		__FUNCTION__, patchtype);
	return;
    }
}
