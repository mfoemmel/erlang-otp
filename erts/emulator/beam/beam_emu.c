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
#include "error.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode: lb_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define OpCode(OpCode)    ((uint32*)op_##OpCode)
#  define Goto(Rel) {Go = (int)(Rel); goto emulator_loop;}
#  define LabelAddr(Addr) &&##Addr
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *(Rel)
#  define LabelAddr(Addr) &&##Addr
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

/*
 * Define macros for deep checking of terms.
 */

#if defined(HARDDEBUG)

#  define CHECK_TERM(T) size_object(T)

#  define CHECK_ARGS(PC) \
do { \
  int i_; \
  int Arity_ = PC[-1]; \
  if (Arity_ > 0) { \
	CHECK_TERM(r(0)); \
  } \
  for (i_ = 1; i_ < Arity_; i_++) { \
	CHECK_TERM(x(i_)); \
  } \
} while (0)
    
#else
#  define CHECK_TERM(T) ASSERT(!is_CP(T))
#  define CHECK_ARGS(T)
#endif

#define is_both_small(X, Y) (((X) & (Y) & TAGMASK) == SMALL)

#define MY_IS_SSMALL(x) (((unsigned) (((x) >> (BODY-1)) + 1)) < 2)

#define MAX(x, y) (((x) > (y)) ? (x) : (y))

#define GET_BIF_ADDRESS(p) ((BifFunction) (((Export *) p)->code[4]))

/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only activly used when
 * the process is switched out.
 *
 * Reason for reuse: The temporary big numbers must be in memory
 * below 256Mb, not on the stack, to be able to have a tagged pointer to
 * them.  Using static variables would not be thread-safe.
 */
#define TMP_BIG1(p) ((p)->def_arg_reg)
#define TMP_BIG2(p) ((p)->def_arg_reg+2)
#define REDS_IN(p)  ((p)->def_arg_reg[5])

/*
 * Add a byte offset to a pointer to uint32.  This is useful when the
 * the loader has precalculated a byte offset.
 */
#define ADD_BYTE_OFFSET(ptr, offset) \
   ((uint32 *) (((unsigned char *)ptr) + (offset)))

/* We don't check the range if an ordinary switch is used */
#ifdef NO_JUMP_TABLE
#define VALID_INSTR(IP) (0 <= (sint32)(IP) && ((sint32)(IP) < (NUMBER_OF_OPCODES*2+10)))
#else
#define VALID_INSTR(IP) \
   ((sint32)LabelAddr(emulator_loop) <= (sint32)(IP) && \
    (sint32)(IP) < (sint32)LabelAddr(end_emulator_loop))
#endif /* NO_JUMP_TABLE */

#define SET_CP(p, ip) \
   ASSERT(VALID_INSTR(*(ip))); \
   (p)->cp = (ip)

#define SET_I(ip) \
   ASSERT(VALID_INSTR(* (uint32 *)(ip))); \
   I = (ip)

#define FetchArgs(S1, S2) tmp_arg1 = (S1); tmp_arg2 = (S2)

/*
 * Store a result into a register given a destination descriptor.
 */

#define StoreResult(Result, DestDesc) \
  do { \
    Eterm stb_reg; \
    stb_reg = (DestDesc); \
    CHECK_TERM(Result); \
    switch (beam_reg_tag(stb_reg)) { \
    case R_REG_DEF: \
      r(0) = (Result); break; \
    case X_REG_DEF: \
      xb(stb_reg) = (Result); break; \
    default: \
      yb(stb_reg-1) = (Result); break; \
    } \
  } while (0)

#define StoreSimpleDest(Src, Dest) Dest = (Src)

/*
 * Store a result into a register and execute the next instruction.
 * Dst points to the word with a destination descriptor, which MUST
 * be just before the next instruction.
 */
 
#define StoreBifResult(Dst, Result) \
  do { \
    uint32* stb_next; \
    Eterm stb_reg; \
    stb_reg = Arg(Dst); \
    I += (Dst) + 2; \
    stb_next = (uint32 *) *I; \
    CHECK_TERM(Result); \
    switch (beam_reg_tag(stb_reg)) { \
    case R_REG_DEF: \
      r(0) = (Result); Goto(stb_next); \
    case X_REG_DEF: \
      xb(stb_reg) = (Result); Goto(stb_next); \
    default: \
      yb(stb_reg-1) = (Result); Goto(stb_next); \
    } \
  } while (0)

#define ClauseFail() goto lb_jump_f

#define Badmatch(Term) { \
    c_p->fvalue = (Term); \
    goto badmatch; \
}

#define SAVE_CP(X)		*(X) = make_cp(c_p->cp)
#define RESTORE_CP(X)		SET_CP(c_p, cp_ptr_val(*(X)))

#define ISCATCHEND(instr) ((uint32 *) *(instr) == OpCode(catch_end_y))

/*
 * Special Beam instructions.
 */

Eterm beam_apply[2];
Eterm beam_exit[1];
Eterm beam_debug_apply[11];
int beam_debug_apply_size = sizeof(beam_debug_apply)/sizeof(beam_debug_apply[0]);

Eterm* em_call_error_handler;
Eterm* em_apply_bif;
Eterm* em_call_traced_function;

/*
 * All Beam instructions in numerical order.
 */

void** beam_ops;
extern int count_instructions;

#define SWAPIN \
    HTOP = c_p->htop; \
    E = c_p->stop

#define SWAPOUT \
    c_p->htop = HTOP; \
    c_p->stop = E

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(uint32 *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(uint32 *) (((unsigned char *)E) + (N)))
#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

/*
 * Makes sure that there are StackNeed + HeapNeed + 1 words available
 * on the combined heap/stack segment, then allocates StackNeed + 1
 * words on the stack and saves CP.
 *
 * M is number of live registers to preserve during garbage collection
 */

#define AH(StackNeed, HeapNeed, M) \
  do { \
     int needed; \
     ASSERT(c_p->htop <= E && E <= c_p->hend); \
     needed = (StackNeed) + CP_SIZE; \
     if (E - (needed + (HeapNeed)) < HTOP) { \
           SWAPOUT; \
           reg[0] = r(0); \
           FCALLS -= erts_garbage_collect(c_p, needed + (HeapNeed), reg, (M)); \
           r(0) = reg[0]; \
           SWAPIN; \
     } \
     E -= needed; \
     SAVE_CP(E); \
     ASSERT(c_p->htop <= E && E <= c_p->hend); \
  } while (0)


#define Allocate(Ns, Live) AH(Ns, 0, Live)

#define AllocateZero(Ns, Live) \
 do { Eterm* ptr; \
      int i = (Ns); \
      AH(i, 0, Live); \
      for (ptr = E + i; ptr > E; ptr--) { \
	 make_blank(*ptr); \
     } \
  } while (0)

#define AllocateHeap(Ns, Nh, Live) AH(Ns, Nh, Live)

#define PutString(Len, Ptr, Dst) \
  do { \
      int len = (Len); \
      unsigned char* s = (unsigned char *) (Ptr); \
      Eterm result = NIL; \
      for (s = (unsigned char *) Arg(1); len > 0; s--, len--) { \
	  PutList(make_small(*s), result, result, StoreSimpleDest); \
      } \
      StoreResult(result, Dst); \
  } while (0)

#define AllocateHeapZero(Ns, Nh, Live) \
 do { Eterm* ptr; \
      int i = (Ns); \
      AH(i, Nh, Live); \
      for (ptr = E + i; ptr > E; ptr--) { \
	 make_blank(*ptr); \
     } \
  } while (0)

#define AllocateInit(Ns, Live, Y) \
  do { AH(Ns, 0, Live); make_blank(Y); } while (0)

/*
 * Like the AH macro, but allocates no additional heap space.
 */

#define A(StackNeed, M) AH(StackNeed, 0, M)

#define D(N) \
     RESTORE_CP(E); \
     E += (N) + CP_SIZE; \
     ASSERT(c_p->htop <= E && E <= c_p->hend); \

/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 */

#define TestHeap(Nh, Live) \
  do { \
    unsigned need = (Nh); \
    ASSERT(c_p->htop <= E && E <= c_p->hend); \
    if (E < (HTOP + need)) { \
       SWAPOUT; \
       reg[0] = r(0); \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live)); \
       r(0) = reg[0]; \
       SWAPIN; \
    } \
  } while (0)

#define I(N) make_blank(yb(N))
#define Init(N) make_blank(yb(N))

#define Init2(Y1, Y2) do { make_blank(Y1); make_blank(Y2); } while (0)
#define Init3(Y1, Y2, Y3) \
   do { make_blank(Y1); make_blank(Y2); make_blank(Y3); } while (0)

#define MakeFun(Mod, Index, Uniq, NumFree) \
  do { \
     SWAPOUT; \
     reg[0] = r(0); \
     r(0) = make_fun(c_p, reg, Mod, Index, Uniq, NumFree); \
     HTOP = c_p->htop; \
  } while (0)


/*
 * Check that we haven't used the reductions and jump to function pointed to by
 * the I register.  If we are out of reductions, do a context switch.
 */

#define DispatchMacro() \
  do { \
     uint32* dis_next; \
     dis_next = (uint32 *) *I; \
     CHECK_ARGS(I); \
     if (FCALLS > 0) { \
        FCALLS--; \
        Goto(dis_next); \
     } else { \
	goto context_switch; \
     } \
 } while (1)

#define DispatchMacrox() \
  do { \
     if (FCALLS > 0) { \
        uint32* dis_next; \
        SET_I(((Export *) Arg(0))->address); \
        dis_next = (uint32 *) *I; \
        FCALLS--; \
        CHECK_ARGS(I); \
        Goto(dis_next); \
     } else if (c_p->ct != NULL && FCALLS > -o_reds) { \
        goto save_calls1; \
     } else { \
        SET_I(((Export *) Arg(0))->address); \
        CHECK_ARGS(I); \
	goto context_switch; \
     } \
 } while (1)

#ifdef DEBUG
/*
 * To simplify breakpoint setting, put the code in one place only and jump to it.
 */
#  define Dispatch() goto do_dispatch
#  define Dispatchx() goto do_dispatchx
#else
/*
 * Inline for speed.
 */
#  define Dispatch() DispatchMacro()
#  define Dispatchx() DispatchMacrox()
#endif

#define Self(R) R = c_p->id
#define Node(R) R = this_node

#define Arg(N)       I[(N)+1]
#define Next(N) \
    I += (N) + 1; \
    ASSERT(VALID_INSTR(*I)); \
    Goto(*I)

#define PreFetch(N, Dst) do { Dst = (uint32 *) *(I + N + 1); } while (0)
#define NextPF(N, Dst) \
    I += N + 1; \
    ASSERT(VALID_INSTR(Dst)); \
    Goto(Dst)

#define GetR(pos, tr) \
   do { \
     tr = Arg(pos); \
     switch (beam_reg_tag(tr)) { \
     case R_REG_DEF: tr = r(0); break; \
     case X_REG_DEF: tr = xb(x_reg_number(tr)); break; \
     case Y_REG_DEF: ASSERT(y_reg_number(tr) > 0); tr = yb(y_reg_number(tr)); break; \
     } \
     CHECK_TERM(tr); \
   } while (0)

#define GetArg1(N, Dst) GetR((N), Dst)

#define GetArg2(N, Dst1, Dst2) \
   do { \
     GetR(N, Dst1); \
     GetR((N)+1, Dst2); \
   } while (0)

#define PutList(H, T, Dst, Store) \
  do { \
   HTOP[0] = (H); HTOP[1] = (T); \
   Store(make_list(HTOP), Dst); \
   HTOP += 2; \
  } while (0)

#define Move(Src, Dst, Store) \
   do { \
       Eterm term = (Src); \
       Store(term, Dst); \
   } while (0)

#define Move2(src1, dst1, src2, dst2) dst1 = (src1); dst2 = (src2)

#define MoveFloat(Float1, Float2, Dest) \
    Dest = make_float(HTOP); \
    HTOP[0] = make_thing(2, FLOAT_THING_SUBTAG); \
    HTOP[1] = Float1; \
    HTOP[2] = Float2; \
    HTOP += 3;

#define MoveGenDest(src, dstp) \
   if ((dstp) == NULL) { r(0) = (src); } else { *(dstp) = src; }

#define MoveReturn(Src, Dest) \
    (Dest) = (Src); \
    I = c_p->cp; \
    ASSERT(VALID_INSTR(*c_p->cp)); \
    CHECK_TERM(r(0)); \
    Goto(*I)

#define DeallocateReturn(Deallocate) \
  do { \
    int words_to_pop = (Deallocate); \
    SET_I(cp_ptr_val(*E)); \
    E = ADD_BYTE_OFFSET(E, words_to_pop); \
    ASSERT(c_p->htop <= E && E <= c_p->hend); \
    CHECK_TERM(r(0)); \
    Goto(*I); \
  } while (0)

#define MoveDeallocateReturn(Src, Dest, Deallocate) \
    (Dest) = (Src); \
    DeallocateReturn(Deallocate)

#define MoveCall(Src, Dest, CallDest, Size) \
    (Dest) = (Src); \
    SET_CP(c_p, I+Size+1); \
    SET_I((uint32 *) CallDest); \
    Dispatch();

#define MoveCallLast(Src, Dest, CallDest, Deallocate) \
    (Dest) = (Src); \
    RESTORE_CP(E); \
    E = ADD_BYTE_OFFSET(E, (Deallocate)); \
    ASSERT(c_p->htop <= E && E <= c_p->hend); \
    SET_I((uint32 *) CallDest); \
    Dispatch();

#define GetList(Src, H, T) do { \
   uint32* tmp_ptr = ptr_val(Src); \
   H = CAR(tmp_ptr); \
   T = CDR(tmp_ptr); } while (0)

#define GetTupleElement(Src, Element, Dest) \
  do { \
    tmp_arg1 = (Eterm) (((unsigned char *) ptr_val(Src)) + (Element)); \
    (Dest) = (*(Eterm *)tmp_arg1); \
  } while (0)

#define ExtractNextElement(Dest) \
    tmp_arg1 += sizeof(Eterm); \
    (Dest) = (* (Eterm *) (((unsigned char *) tmp_arg1)))

#define ExtractNextElement2(Dest) \
  do { \
    Eterm* ene_dstp = &(Dest); \
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1]; \
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2]; \
    tmp_arg1 += sizeof(Eterm) + sizeof(Eterm); \
  } while (0)

#define ExtractNextElement3(Dest) \
  do { \
    Eterm* ene_dstp = &(Dest); \
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1]; \
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2]; \
    ene_dstp[2] = ((Eterm *) tmp_arg1)[3]; \
    tmp_arg1 += 3*sizeof(Eterm); \
  } while (0)

#define ExtractNextElement4(Dest) \
  do { \
    Eterm* ene_dstp = &(Dest); \
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1]; \
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2]; \
    ene_dstp[2] = ((Eterm *) tmp_arg1)[3]; \
    ene_dstp[3] = ((Eterm *) tmp_arg1)[4]; \
    tmp_arg1 += 4*sizeof(Eterm); \
  } while (0)

#define ExtractElement(Element, Dest) \
  do { \
     tmp_arg1 += (Element); \
     (Dest) = (* (Eterm *) tmp_arg1); \
  } while (0)

#define PutTuple(Arity, Src, Dest) \
     ASSERT(is_arity_value(Arity)); \
     Dest = make_tuple(HTOP); \
     HTOP[0] = (Arity); \
     HTOP[1] = (Src); \
     HTOP += 2

#define Put(Word) *HTOP++ = (Word)

#define Equal(X, Y, Action) if (X != Y) { Action; }

#define IsFloat(Src, Fail) if (is_not_float(Src)) { Fail; }

#define IsInteger(Src, Fail) if (is_not_integer(Src)) { Fail; }

#define IsNumber(X, Fail) if (is_not_integer(X) && is_not_float(X)) { Fail; }

#define IsConstant(X, Fail) if (is_list(X) || is_nil(X) || is_tuple(X)) { Fail; }

#define IsAtom(Src, Fail) if (is_not_atom(Src)) { Fail; }

#define IsIntegerAllocate(Src, Need, Alive, Fail) \
    if (is_not_integer(Src)) { Fail; } \
    A(Need, Alive)

#define IsNil(Src, Fail) if (is_not_nil(Src)) { Fail; }

#define IsList(Src, Fail) if (is_not_list(Src) && is_not_nil(Src)) { Fail; }

#define IsNonemptyList(Src, Fail) if (is_not_list(Src)) { Fail; }

#define IsNonemptyListAllocate(Src, Need, Alive, Fail) \
    if (is_not_list(Src)) { Fail; } \
    A(Need, Alive)

#define IsNonemptyListTestHeap(Src, Need, Alive, Fail) \
    if (is_not_list(Src)) { Fail; } \
    TestHeap(Need, Alive)

#define IsTuple(X, Action) if (is_not_tuple(X)) Action

#define IsArity(Pointer, Arity, Fail) \
    if (*(Eterm *)(tmp_arg1 = (Eterm)ptr_val(Pointer)) != (Arity)) { Fail; }

#if defined(ALLOW_FUN_TUPLES)
#  define IsFunction(X, Action) \
  do { \
     Eterm* _tp; \
     if ( !((is_binary(X) && thing_subtag(*ptr_val(X)) == FUN_SUBTAG) || \
	    (is_tuple(X) && (*(_tp = ptr_val(X)) == make_arityval(5)) && \
	     _tp[1] == am_fun)) ) { \
          Action; \
     } \
  } while (0)
#else
#  define IsFunction(X, Action) \
  do { \
     Eterm* _tp; \
     if ( !((is_binary(X) && thing_subtag(*ptr_val(X)) == FUN_SUBTAG)) ) { \
          Action; \
     } \
  } while (0)
#endif

#define IsTupleOfArity(Src, Arity, Fail) \
  do { \
    if (is_not_tuple(Src) || *(Eterm *)(tmp_arg1 = (Eterm) ptr_val(Src)) != Arity) { \
        Fail; \
    } \
  } while (0)
#define IsBinary(Src, Fail) \
 if (is_not_binary(Src) || thing_subtag(*ptr_val(Src)) == FUN_SUBTAG) { Fail; }

#define IsPort(Src, Fail) if (is_not_port(Src)) { Fail; }
#define IsPid(Src, Fail) if (is_not_pid(Src)) { Fail; }
#define IsRef(Src, Fail) if (is_not_refer(Src)) { Fail; }

static uint32* handle_error(Process* c_p, uint32* pc, Eterm* reg, BifFunction bf);
static uint32 call_error_handler(Process* p, uint32* ip, Eterm* reg);
static uint32 module_info_0(Process* p, uint32 module);
static uint32 module_info_1(Process* p, uint32 module, uint32 what);
static uint32* apply(Process* p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static uint32* call_fun(Process* p, int arity, Eterm* reg, Eterm args);
static uint32* apply_fun(Process* p, Eterm fun, Eterm args, uint32* reg);
static Eterm make_fun(Process* p, Eterm* reg, Eterm mod, unsigned index,
		      unsigned uniq, unsigned num_free);
static uint32 mixed_eq(uint32 arg1, uint32 arg2);
static uint32 mixed_ge(uint32 arg1, uint32 arg2);


void
init_emulator(void)
{
    /*
     * Test for assumptions about tags.
     */
    ASSERT(R_REG_DEF != 3);
    ASSERT(X_REG_DEF != 3);
    ASSERT(Y_REG_DEF != 3);
    ASSERT((SMALL & 3) == 3);
    ASSERT((ATOM & 3) == 3);
    ASSERT((NIL & 3) == 3);

    /*
     * The is_both_small() macro makes the following assumption.
     */
    ASSERT(SMALL == 15);

    process_main(NULL, -1);
}

/*
 * On certain platforms, make sure the main variables really are placed
 * in registers.
 */

#if defined(__GNUC__) && defined(sparc) && !defined(DEBUG)
#  define REG_x0 asm("%l0")
#  define REG_xregs asm("%l1")
#  define REG_htop asm("%l2")
#  define REG_stop asm("%l3")
#  define REG_I asm("%l4")
#  define REG_fcalls asm("%l5")
#  define REG_tmp_arg1 asm("%l6")
#  define REG_tmp_arg2 asm("%l7")
#else
#  define REG_x0
#  define REG_xregs
#  define REG_htop
#  define REG_stop
#  define REG_I
#  define REG_fcalls
#  define REG_tmp_arg1
#  define REG_tmp_arg2
#endif

int process_main(c_p, reds)
    Process* c_p;
    int reds;
{
    /*
     * X register zero; also called r(0)
     */
    register uint32 x0 REG_x0;

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register uint32* reg REG_xregs;

    /*
     * Top of heap (next free location); grows upwards.
     */
    register uint32* HTOP REG_htop;

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    register uint32* E REG_stop;

    /*
     * Pointer to next threaded instruction.
     */
    register uint32 *I REG_I;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register sint32 FCALLS REG_fcalls;

    /*
     * Temporaries used for picking up arguments for instructions.
     */
    register uint32 tmp_arg1 REG_tmp_arg1;
    register uint32 tmp_arg2 REG_tmp_arg2;

    static uint32 save_reg[MAX_REG];	
    /* X registers -- not used directly, but
     * through 'reg', because using it directly
     * needs two instructions on a SPARC,
     * while using it through reg needs only
     * one.
     */

    /* For keeping the old value of 'reds' when call saving is active.
     */
    int o_reds;

#ifndef NO_JUMP_TABLE
    static void* opcodes[] = { DEFINE_OPCODES };
    static void* counting_opcodes[] = { DEFINE_COUNTING_OPCODES };
#else
    int Go;
#endif

    /*
     * Note: In this function, we attempt to place rarely executed code towards
     * the end of the function, in the hope that the cache hit rate will be better.
     * The initialization code is only run once, so it is at the very end.
     *
     * Note: c_p->arity must be set to reflect the number of useful terms in
     * c_p->arg_reg before *returning* from this function.  *Inside* this function,
     * there is no need to set it.
     */

    if (reds < 0) {
	goto init_emulator;
    } else {
	uint32* argp = c_p->arg_reg;
	uint32* next;
	int i;

	reg = save_reg;
	for (i = c_p->arity - 1; i > 0; i--) {
	    reg[i] = argp[i];
	    CHECK_TERM(reg[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	SET_I(c_p->i);

	if (c_p->ct != NULL) {
	   o_reds = reds;
	   REDS_IN(c_p) = 0;
	} else {
	   o_reds = 0;
	   REDS_IN(c_p) = reds;
	}
	FCALLS = REDS_IN(c_p);

	next = (uint32 *) *I;
	r(0) = c_p->arg_reg[0];
#ifdef HARDDEBUG
	if (c_p->arity > 0) {
	    CHECK_TERM(r(0));
	}
#endif
	SWAPIN;
	ASSERT(VALID_INSTR(next));
	Goto(next);
    }

#if defined(DEBUG) || defined(NO_JUMP_TABLE)
 emulator_loop:
#endif

#ifdef NO_JUMP_TABLE
    switch (Go) {
#endif
#include "beam_hot.h"

 OpCase(i_plus_jd):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 int i = signed_val(tmp_arg1) + signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     StoreBifResult(1, result);
	 }
     }
     result = erts_mixed_plus(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_minus_jd):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 int i = signed_val(tmp_arg1) - signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     StoreBifResult(1, result);
	 }
     }
     result = erts_mixed_minus(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_is_lt_f):
    if (tmp_arg1 == tmp_arg2 || mixed_ge(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ge_f):
    if (tmp_arg1 != tmp_arg2 && !mixed_ge(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_f):
    if (tmp_arg1 != tmp_arg2 && !mixed_eq(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ne_f):
    if (tmp_arg1 == tmp_arg2 || mixed_eq(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_exact_f):
    if (tmp_arg1 != tmp_arg2 && !eq(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_call_only_f):
    SET_I((uint32 *) Arg(0));
    Dispatch();

 OpCase(i_call_last_fP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));
    ASSERT(c_p->htop <= E && E <= c_p->hend);
    SET_I((uint32 *) Arg(0));
    Dispatch();

 OpCase(i_call_f):
    SET_CP(c_p, I+2);
    SET_I((uint32 *) Arg(0));
    Dispatch();

 OpCase(i_call_ext_last_eP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));
    ASSERT(c_p->htop <= E && E <= c_p->hend);

    /*
     * Note: The pointer to the export entry is never NULL; if the module
     * is not loaded, it points to code which will invoke the error handler
     * (see lb_call_error_handler below).
     */
    Dispatchx();

 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
    Dispatchx();

 OpCase(i_call_ext_only_e):
    Dispatchx();

 OpCase(init_y): {
     uint32* next;

     PreFetch(1, next);
     make_blank(yb(Arg(0)));
     NextPF(1, next);
 }

 OpCase(return):
    SET_I(c_p->cp);
    CHECK_TERM(r(0));
    Goto(*I);

 OpCase(test_heap_1_put_list_Iy): {
     uint32* next;

     PreFetch(2, next);
     TestHeap(Arg(0), 1);
     PutList(yb(Arg(1)), r(0), r(0), StoreSimpleDest);
     CHECK_TERM(r(0));
     NextPF(2, next);
 }

 OpCase(put_string_IId):
    {
      unsigned char* s;
      int len;
      Eterm result;

      len = Arg(0);		/* Length. */
      result = NIL;
      for (s = (unsigned char *) Arg(1); len > 0; s--, len--) {
	  PutList(make_small(*s), result, result, StoreSimpleDest);
      }
      StoreBifResult(2, result);
    }

    /*
     * Send is almost a standard call-BIF with two arguments, except for:
     *    1) It cannot be traced.
     *	  2) There is no pointer to the send_2 function stored in
     *       the instruction.
     */
 OpCase(send): {
     Eterm* next;
     Eterm result;

     c_p->fcalls = FCALLS - 1;
     result = send_2(c_p, r(0), x(1));
     PreFetch(0, next);
     FCALLS = c_p->fcalls;
     if (result) {
	 r(0) = result;
	 CHECK_TERM(r(0));
	 NextPF(0, next);
     } else if (c_p->freason == RESCHEDULE) {
	 uint32* argp;

	 c_p->arity = 2;

	 /*
	  * Moving c_p->arg to a register is shorter than using c_p->arg_reg
	  * directly, since c_p->arg_reg is a pointer (not an array)
	  * and the compiler generates code to fetch the pointer every time.
	  */
	 argp = c_p->arg_reg;
	 argp[0] = r(0);
	 argp[1] = x(1);
	 SWAPOUT;
	 c_p->i = I;
	 c_p->current = NULL;
	 return REDS_IN(c_p) - FCALLS;
     } else if (c_p->freason == TRAP) {
	 SET_CP(c_p, I+1);
	 SET_I(((Export *)(c_p->fvalue))->address);
	 r(0) = c_p->def_arg_reg[0];
	 x(1) = c_p->def_arg_reg[1];
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(tl_jsd): {
     Eterm list;
     Eterm result;

     GetArg1(1, list);
     if (is_not_list(list)) {
	 goto badarg;
     }
     result = ptr_val(list)[1];
     StoreBifResult(2, result);
 }

 OpCase(hd_jsd): {
     Eterm list;
     Eterm result;

     GetArg1(1, list);
     if (is_not_list(list)) {
	 goto badarg;
     }
     result = ptr_val(list)[0];
     StoreBifResult(2, result);
 }

 OpCase(i_element_jssd): {
     Eterm index;
     Eterm tuple;

     /*
      * Inlined version of element/2 for speed.
      */
     GetArg2(1, index, tuple);
     if (is_small(index) && is_tuple(tuple)) {
	uint32* tp = ptr_val(tuple);

	if ((signed_val(index) >= 1) &&
	    (signed_val(index) <= arityval(*tp))) {
	    Eterm result = tp[signed_val(index)];
	    StoreBifResult(3, result);
	}
    }
 }
 /* Fall through */

 OpCase(badarg_j):
 badarg:
    c_p->freason = BADARG;
    goto lb_Cl_error;

 OpCase(i_fast_element_jIsd): {
     Eterm tuple;

     /*
      * Inlined version of element/2 for even more speed.
      * The first argument is an untagged integer >= 1.
      * The second argument is guaranted to be a register operand.
      */
     GetArg1(2, tuple);
     if (is_tuple(tuple)) {
	 uint32* tp = ptr_val(tuple);
	 tmp_arg2 = Arg(1);
	 if (tmp_arg2 <= arityval(*tp)) {
	     Eterm result = tp[tmp_arg2];
	     StoreBifResult(3, result);
	 }
     }
     goto badarg;
 }

 OpCase(catch_yf):
    c_p->catches++;
    yb(Arg(0)) = make_catch(Arg(1));
    Next(2);

 OpCase(catch_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     CHECK_TERM(r(0));
     Next(1);
 }

 /*
  * Skeleton for receive statement:
  *
  *      L1:          <-------------------+
  *                   <-----------+       |
  *     	     	       	  |   	  |
  *             loop_rec L2 ------+---+   |
  *             ...               |   |   |
  *             remove_message 	  |   |	  |
  *             jump L3           |   |   |
  *		...	          |   |   |
  *		loop_rec_end L1 --+   |   |
  *      L2:          <---------------+   |
  *	   	wait L1  -----------------+      or wait_timeout
  *		timeout
  *
  *	 L3:    Code after receive...
  *
  *
  */


    /*
     * Pick up the next message and place it in a x register (not r(0)).
     * If no message, jump to a wait or wait_timeout instruction.
     */
 OpCase(loop_rec_fx):
 {
     uint32* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((uint32 *) Arg(0));
	 Goto(*I);		/* Jump to a wait or wait_timeout instruction */
     }
     PreFetch(2, next);
     xb(Arg(1)) = msgp->mesg;
     CHECK_TERM(msgp->mesg);
     NextPF(2, next);
 }

    /*
     * Pick up the next message and place it in x(0).
     * If no message, jump to a wait or wait_timeout instruction.
     */
 OpCase(loop_rec_fr):
 {
     uint32* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((uint32 *) Arg(0));
	 Goto(*I);		/* Jump to a wait or wait_timeout instruction */
     }
     PreFetch(1, next);
     r(0) = msgp->mesg;
     CHECK_TERM(r(0));
     NextPF(1, next);
 }

 /*
  * Remove a (matched) message from the message queue.
  */
 OpCase(remove_message): {
     uint32* next;
     ErlMessage* msgp;

     PreFetch(0, next);
     msgp = PEEK_MESSAGE(c_p);
     if (c_p->ct != NULL) {
	 save_calls(c_p, &exp_receive);
     }
     SEQ_TRACE_TOKEN(c_p) = msgp->seq_trace_token;
     if (SEQ_TRACE_TOKEN(c_p) != NIL) {
	 Eterm msg;
	 ASSERT(is_tuple(SEQ_TRACE_TOKEN(c_p)));
	 ASSERT(SEQ_TRACE_TOKEN_ARITY(c_p) == 5);
	 ASSERT(is_small(SEQ_TRACE_TOKEN_SERIAL(c_p)));
	 ASSERT(is_small(SEQ_TRACE_TOKEN_LASTCNT(c_p)));
	 ASSERT(is_small(SEQ_TRACE_TOKEN_FLAGS(c_p)));
	 ASSERT(is_pid(SEQ_TRACE_TOKEN_SENDER(c_p)));
	 c_p->seq_trace_lastcnt = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
	 if (c_p->seq_trace_clock < unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p))) {
	     c_p->seq_trace_clock = unsigned_val(SEQ_TRACE_TOKEN_SERIAL(c_p));
	 }
	 msg = msgp->mesg;
	 seq_trace_output(SEQ_TRACE_TOKEN(c_p), msg, SEQ_TRACE_RECEIVE, c_p->id);
     }
     UNLINK_MESSAGE(c_p, msgp);
     JOIN_MESSAGE(c_p);
     CANCEL_TIMER(c_p);
     fix_free(mesg_desc, (Eterm *) msgp);
     NextPF(0, next);
 }

    /*
     * Advance the save pointer to the next message (the current
     * message didn't match), then jump to the loop_rec instruction.
     */
 OpCase(loop_rec_end_f):
    SET_I((uint32 *) Arg(0));
    SAVE_MESSAGE(c_p);
    Goto(*I);		/* To loop_rec */

    /*
     * Prepare to wait for a message or a timeout, whichever occurs first.
     */
 OpCase(wait_timeout_fs): {
     Eterm timeout_value;

     /*
      * If we have already set the timer, we must NOT set it again.  Therefore,
      * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
      */
     if (c_p->flags & (F_INSLPQUEUE | F_TIMO)) {
	 goto wait2;
     }
     GetArg1(1, timeout_value);
     if (timeout_value != make_small(0)) {
	 unsigned time_val;

	 if (is_small(timeout_value) && signed_val(timeout_value) > 0) {
	     /*
	      * The timer routiner will set c_p->i to the value in
	      * c_p->def_arg_reg[0].  Note that it is safe to use this
	      * location because there are no living x registers in
	      * a receive statement.
	      */
	     c_p->def_arg_reg[0] = (uint32) (I+3);
	     set_timer(c_p, unsigned_val(timeout_value));
	 } else if (timeout_value == am_infinity) {
	     c_p->flags |= F_TIMO;
	 } else if (is_big(timeout_value) && big_to_unsigned(timeout_value, &time_val)) {
	     c_p->def_arg_reg[0] = (uint32) (I+3);
	     set_timer(c_p, time_val);
	 } else {		/* Wrong time */
	     OpCase(i_wait_error):
	     c_p->freason = TIMEOUT_VALUE;
	     goto find_func_info;
	 }

	 /*
	  * Prepare to wait indefinitely for a new message to arrive
	  * (or the time set above if falling through from above).
	  *
	  * When a new message arrives, control will be transferred
	  * the loop_rec instruction (at label L1).  In case of
	  * of timeout, control will be transferred to the timeout
	  * instruction followinged the wait_timeout instruction.
	  */

	 OpCase(wait_f):
     wait2:
	 c_p->i = (uint32 *) Arg(0); /* L1 */
	 SWAPOUT;
	 c_p->arity = 0;
	 c_p->status = P_WAITING;
	 c_p->current = NULL;
	 return REDS_IN(c_p) - FCALLS;
     }
     Next(2);
 }

 OpCase(wait_timeout_fI):
 {
     /*
      * If we have already set the timer, we must NOT set it again.  Therefore,
      * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
      */
     if ((c_p->flags & (F_INSLPQUEUE | F_TIMO)) == 0) {
	 c_p->def_arg_reg[0] = (uint32) (I+3);
	 set_timer(c_p, Arg(1));
     }
     goto wait2;
 }

    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
 OpCase(timeout): {
     uint32* next;

     PreFetch(0, next);
     if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
	 trace_receive(c_p, am_timeout);
     }
     if (c_p->ct != NULL)
	save_calls(c_p, &exp_timeout);
     c_p->flags &= ~F_TIMO;
     SEQ_TRACE_TOKEN(c_p) = NIL;
     JOIN_MESSAGE(c_p);
     NextPF(0, next);
 }

 OpCase(i_select_val_sfI):
     GetArg1(0, tmp_arg1);

 do_binary_search:
 {
     struct Pairs {
	 int val;
	 uint32 *addr;
     };
     struct Pairs* low;
     struct Pairs* high;
     struct Pairs* mid;

     low = (struct Pairs *) &Arg(3);
     high = low + Arg(2);
     while (low < high) {
	 mid = low + (high-low) / 2;
	 if (tmp_arg1 < mid->val) {
	     high = mid;
	 } else if (tmp_arg1 > mid->val) {
	     low = mid + 1;
	 } else {
	     SET_I(mid->addr);
	     Goto(*I);
	 }
     }
     SET_I((uint32 *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_zero_sfI):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = signed_val(index);
	 if (index < Arg(2)) {
	     SET_I((uint32 *) (&Arg(3))[index]);
	     Goto(*I);
	 }
     }
     SET_I((uint32 *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_sfII):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = (unsigned) (signed_val(index) - Arg(3));
	 if (index < Arg(2)) {
	     SET_I((uint32 *) (&Arg(4))[index]);
	     Goto(*I);
	 }
     }
     SET_I((uint32 *) Arg(1));
     Goto(*I);
 }

    /*
     * All guards with zero arguments have special instructions:
     * 	self/0
     * 	node/0
     *
     * All other guard BIFs take one or two arguments.
     */

    /*
     * Guard BIF in head.  On failure, ignore the error and jump
     * to the code for the next clause.  We don't support tracing
     * of guard BIFs.
     */

 OpCase(bif1_fbsd):
    {
	BifFunction bf;
	Eterm arg;
	Eterm result;

	GetArg1(2, arg);
	bf = (BifFunction) Arg(1);
	result = (*bf)(c_p, arg);
	if (result) {
	    StoreBifResult(3, result);
	}
	SET_I((uint32 *) Arg(0));
	Goto(*I);
    }

    /*
     * Guard BIF in body.  It can fail like any BIF.  No trace support.
     */

 OpCase(bif1_body_bsd):
    {
	BifFunction bf;
	Eterm arg;
	Eterm result;

	GetArg1(1, arg);
	bf = (BifFunction) Arg(0);
	result = (*bf)(c_p, arg);
	if (result) {
	    StoreBifResult(2, result);
	}
	reg[0] = arg;
	SWAPOUT;
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 /*
  * Guards bifs and, or, xor in guards.  Currently not generated by any
  * any compiler.
  */
 OpCase(i_bif2_fbd):
    {
	BifFunction bf;
	Eterm result;

	bf = (BifFunction) Arg(1);
	result = (*bf)(c_p, tmp_arg1, tmp_arg2);
	if (result) {
	    StoreBifResult(2, result);
	}
	SET_I((uint32 *) Arg(0));
	Goto(*I);
    }

 /*
  * Guards bifs and, or, xor, relational operators in body.  Generated
  * by the new compiler.
  */
 OpCase(i_bif2_body_bd):
    {
	BifFunction bf;
	Eterm result;

	bf = (BifFunction) Arg(0);
	result = (*bf)(c_p, tmp_arg1, tmp_arg2);
	if (result) {
	    ASSERT(!is_CP(result));
	    StoreBifResult(1, result);
	}
	reg[0] = tmp_arg1;
	reg[1] = tmp_arg2;
	SWAPOUT;
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

    /*
     * The most general BIF call.  The BIF may build any amount of data
     * on the heap.  The result is always returned in r(0).
     * Must be followed by a TestHeap instruction (if anything need to
     * to be constructed on the heap).
     *
     * XXX The support for tracing should be removed from the instructions.
     * Tracing should be supported in some other way, with zero cost
     * if not used.
     */
 OpCase(call_bif0_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}

	/*
	 * A BIF with no arguments cannot fail (especially not with badarg).
	 */
	r(0) = (*bf)(c_p);
	FCALLS = c_p->fcalls;
	HTOP = c_p->htop;
	CHECK_TERM(r(0));
	Next(1);
    }

 OpCase(call_bif1_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	uint32* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	result = (*bf)(c_p, r(0));
	FCALLS = c_p->fcalls;
	HTOP = c_p->htop;
	if (result) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 1;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(((Export *)(c_p->fvalue))->address);
	    r(0) = c_p->def_arg_reg[0];
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	reg[0] = r(0);
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif2_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	uint32* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	CHECK_TERM(r(0));
	CHECK_TERM(x(1));
	result = (*bf)(c_p, r(0), x(1));
	FCALLS = c_p->fcalls;
	HTOP = c_p->htop;
	if (result) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 2;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(((Export *)(c_p->fvalue))->address);
	    r(0) = c_p->def_arg_reg[0];
	    x(1) = c_p->def_arg_reg[1];
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	reg[0] = r(0);
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif3_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	uint32* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	result = (*bf)(c_p, r(0), x(1), x(2));
	FCALLS = c_p->fcalls;
	HTOP = c_p->htop;
	if (result) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 3;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(((Export *)(c_p->fvalue))->address);
	    r(0) = c_p->def_arg_reg[0];
	    x(1) = c_p->def_arg_reg[1];
	    x(2) = c_p->def_arg_reg[2];
	    Dispatch();
	}


	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	reg[0] = r(0);
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 /*
  * Arithmetic operations.
  */

 OpCase(i_times_jd):
 {
     Eterm result;

     result = erts_mixed_times(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
 }
    /* Fall through into lb_Cl_error. */

 /*
  * An error occured in an arithmetic operation or test that could
  * appear either in a head or in a body.
  * In a head, execution should continue at failure address in Arg(0).
  * In a body, Arg(0) == 0 and an exception should be raised.
  */
 lb_Cl_error: {
     if (Arg(0) != 0) {
	 OpCase(jump_f): {
	     SET_I((uint32 *) Arg(0));
	     Goto(*I);
	 }
     }
     ASSERT(c_p->freason != BADMATCH || c_p->fvalue != 0);
     goto find_func_info;
 }

 OpCase(i_m_div_jd):
 {
     Eterm result;

     result = erts_mixed_div(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_int_div_jd):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 result = make_small(signed_val(tmp_arg1) / signed_val(tmp_arg2));
	 StoreBifResult(1, result);
     }
     result = erts_int_div(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_rem_jd):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 result = make_small(signed_val(tmp_arg1) % signed_val(tmp_arg2));
     } else {
	 result = erts_int_rem(c_p, tmp_arg1, tmp_arg2);
     }
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_band_jd):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG & TAG == TAG.
	  */
	 result = tmp_arg1 & tmp_arg2;
	 StoreBifResult(1, result);
     }
     result = erts_band(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_bor_jd):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG | TAG == TAG.
	  */
	 result = tmp_arg1 | tmp_arg2;
	 StoreBifResult(1, result);
     }
     result = erts_bor(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_bxor_jd):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * We could extract the tag from one argument, but a tag extraction
	  * could mean a shift.  Therefore, play it safe here.
	  */
	 result = make_small(signed_val(tmp_arg1) ^ signed_val(tmp_arg2));
	 StoreBifResult(1, result);
     }
     result = erts_bxor(c_p, tmp_arg1, tmp_arg2);
     if (result) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 {
     int i;
     int ires;
     uint32* bigp;

     OpCase(i_bsr_jd):
	 if (is_small(tmp_arg2)) {
	     i = -signed_val(tmp_arg2);
	     if (is_small(tmp_arg1)) {
		 goto small_shift;
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(1, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 }
     goto badarith;
     
     OpCase(i_bsl_jd):
	 if (is_small(tmp_arg2)) {
	     i = signed_val(tmp_arg2);

	     if (is_small(tmp_arg1)) {
	     small_shift:
		 ires = signed_val(tmp_arg1);
	     
		 if (i == 0 || ires == 0) {
		     StoreBifResult(1, tmp_arg1);
		 } else if (i < 0)  { /* Right shift */
		     i = -i;
		     if (i >= 27) {
			 tmp_arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		     } else {
			 tmp_arg1 = make_small(ires >> i);
		     }
		     StoreBifResult(1, tmp_arg1);
		 } else if (i < 27) { /* Left shift */
		     if ((ires > 0 && ((-1 << (27-i)) & ires) == 0) ||
			 ((-1 << (27-i)) & ~ires) == 0) {
			 tmp_arg1 = make_small(ires << i);
			 StoreBifResult(1, tmp_arg1);
		     }
		 }
		 tmp_arg1 = small_to_big(ires, TMP_BIG1(c_p));

	     big_shift:
		 if (i > 0) {	/* Left shift. */
		     ires = big_size(tmp_arg1) + (i / D_EXP);
		 } else {	/* Right shift. */
		     ires = big_size(tmp_arg1);
		     if (ires <= (-i / D_EXP))
			 ires = 3;
		     else
			 ires -= (-i / D_EXP);
		 }
		 bigp = ArithAlloc(c_p, BIG_NEED_SIZE(ires+1));

		 tmp_arg1 = big_lshift(tmp_arg1, i, bigp);
		 ArithCheck(c_p);
		 if (is_nil(tmp_arg1)) {
		 system_limit:
		     c_p->freason = SYSTEM_LIMIT;
		     goto lb_Cl_error;
		 }
		 StoreBifResult(1, tmp_arg1);
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(1, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 }
 }

 badarith:
    c_p->freason = BADARITH;
    goto lb_Cl_error;

 OpCase(i_apply): {
     uint32* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_last_P): {
     uint32* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_CP(c_p, (uint32 *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_only): {
     uint32* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     uint32* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = c_p->htop;
     if (next != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_last_P): {
     uint32 *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = c_p->htop;
     if (next != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_CP(c_p, (uint32 *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_only): {
     uint32* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = c_p->htop;
     if (next != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_I(next);
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_I): {
     uint32* next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, 0);
     HTOP = c_p->htop;
     if (next != NULL) {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatch();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_last_IP): {
     uint32* next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, 0);
     HTOP = c_p->htop;
     if (next != NULL) {
	r(0) = reg[0];
	CHECK_TERM(r(0));
	SET_CP(c_p, (uint32 *) E[0]);
	E = ADD_BYTE_OFFSET(E, Arg(1));
	SET_I(next);
	Dispatch();
     }
     goto find_func_info;
 }

#ifdef DEBUG
    /*
     * Set a breakpoint here to get control just after a call instruction.
     * I points to the first instruction in the called function.
     *
     * In gdb, use 'call dis(I-4, 1)' to show the name of the function.
     */
 do_dispatch:
     DispatchMacro();

 do_dispatchx:
     DispatchMacrox();
#endif

    /*
     * Jumped to from the Dispatch() macro when the reductions are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can (normally) get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch:
 {
     uint32 *argp;
     int reds_used;
     int i;

     c_p->current = I-3;	/* FuncBegin + 1: points to Mod, Func, Arity */
     c_p->arity = c_p->current[2];

     /*
      * Make sure that there is enough room for the argument registers to be saved.
      */
     if (c_p->arity > c_p->max_arg_reg) {
	 /*
	  * Yes, this is an expensive operation, but you only pay it the first
	  * time you call a function with more than 6 arguments which is
	  * scheduled out.  This is better than paying for 26 words of wasted
	  * space for most processes which never call functions with more than
	  * 6 arguments.
	  */
	 c_p->max_arg_reg = c_p->arity;
	 if (c_p->arg_reg != c_p->def_arg_reg) {
	     c_p->arg_reg = (uint32 *) safe_realloc((char *) c_p->arg_reg,
						    c_p->arity *
						    sizeof(c_p->arg_reg[0]));
	 } else {
	     c_p->arg_reg = (uint32 *) safe_alloc(c_p->arity * sizeof(c_p->arg_reg[0]));
	 }
     }

     /*
      * Since REDS_IN(c_p) is stored in the save area (c_p->arg_reg) we must read it
      * now before saving registers.
      *
      * The '+ 1' compensates for the last increment which was not done
      * (beacuse the code for the Dispatch() macro becomes shorter that way).
      */

     reds_used = REDS_IN(c_p) - FCALLS + 1;
     
     /*
      * Save the argument registers and everything else.
      */

     argp = c_p->arg_reg;
     for (i = c_p->arity - 1; i > 0; i--) {
	 argp[i] = reg[i];
     }
     c_p->arg_reg[0] = r(0);
     SWAPOUT;
     c_p->i = I;
     add_to_schedule_q(c_p);
     return reds_used;
 }

 OpCase(i_put_float_od):
 {
     uint32* hp = arith_alloc(c_p, 3);
     uint32 f = make_float(hp);

     hp[0] = make_thing(2, FLOAT_THING_SUBTAG);
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     StoreBifResult(2, f);
 }

 OpCase(i_fetch_float1_o):
 {
     uint32* hp = arith_alloc(c_p, 3);
     tmp_arg1 = make_float(hp);

     hp[0] = make_thing(2, FLOAT_THING_SUBTAG);
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     Next(2);
 }

 OpCase(i_fetch_float2_o):
 {
     uint32* hp = arith_alloc(c_p, 3);
     tmp_arg2 = make_float(hp);

     hp[0] = make_thing(2, FLOAT_THING_SUBTAG);
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     Next(2);
 }

 OpCase(i_select_tuple_arity_sfI):
 {
     GetArg1(0, tmp_arg1);

     if (is_tuple(tmp_arg1)) {
	 tmp_arg1 = *ptr_val(tmp_arg1);
	 goto do_binary_search;
     }
     SET_I((uint32 *) Arg(1));
     Goto(*I);
 }     

 /*
  * Arg(0): N = Thing word (tag, sign, number of words)
  * Arg(1..N): Value
  * Arg(N+1): Destination register
  */
  
 OpCase(i_put_big_wd):
 {
     uint32 thing = Arg(0);
     uint32 size = thing_arityval(thing);
     uint32* hp = arith_alloc(c_p, size+1);
     uint32 big = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(uint32));
     StoreBifResult(size+1, big);
 }

 OpCase(i_fetch_big1_w):
 {
     uint32 thing = Arg(0);
     uint32 size = thing_arityval(thing);
     uint32* hp = arith_alloc(c_p, size+1);
     tmp_arg1 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(uint32));
     Next(size+1);
 }

 OpCase(i_fetch_big2_w):
 {
     uint32 thing = Arg(0);
     uint32 size = thing_arityval(thing);
     uint32* hp = arith_alloc(c_p, size+1);
     tmp_arg2 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(uint32));
     Next(size+1);
 }

 OpCase(i_select_big_sf):
    {
	uint32* bigp;
	uint32 arity;
	uint32* given;
	uint32 given_arity;
	uint32 given_size;

	GetArg1(0, tmp_arg1);
	if (is_big(tmp_arg1)) {

	    /*
	     * The loader has sorted the bignumbers in descending order
	     * on the arity word.  Therefore, we know that the search
	     * has failed as soon as we encounter an arity word less than
	     * the arity word of the given number.  There is a zero word
	     * (less than any valid arity word) stored after the last bignumber.
	     */

 	    given = ptr_val(tmp_arg1);
	    given_arity = given[0];
	    given_size = thing_arityval(given_arity);
	    bigp = &Arg(2);
	    while ((arity = bigp[0]) >= given_arity) {
		if (arity == given_arity &&
		    memcmp(bigp+1, given+1, sizeof(uint32)*given_size) == 0) {
		    SET_I((uint32 *) bigp[given_size+1]);
		    Goto(*I);
		}
		bigp += thing_arityval(arity) + 2;
	    }
	}

	/*
	 * Failed.
	 */

	SET_I((uint32 *) Arg(1));
	Goto(*I);
    }

 OpCase(i_select_float_sfI):
 {
     uint32 fpart1;
     uint32 fpart2;
     int n;
     struct ValLabel {
	 uint32 fpart1;
	 uint32 fpart2;
	 uint32 *addr;
     };
     struct ValLabel* ptr;

     GetArg1(0, tmp_arg1);
     ASSERT(is_float(tmp_arg1));
     fpart1 = ptr_val(tmp_arg1)[1];
     fpart2 = ptr_val(tmp_arg1)[2];

     n = Arg(2);
     ptr = (struct ValLabel *) &Arg(3);
     while (n-- > 0) {
	 if (ptr->fpart1 == fpart1 && ptr->fpart2 == fpart2) {
	     SET_I(ptr->addr);
	     Goto(*I);
	 }
	 ptr++;
     }
     SET_I((uint32 *) Arg(1));
     Goto(*I);
 }

 OpCase(set_tuple_element_sdP): {
     Eterm element;
     Eterm tuple;
     uint32* next;
     uint32* p;
     
     PreFetch(3, next);
     GetArg2(0, element, tuple);
     ASSERT(is_tuple(tuple));
     p = (uint32 *) ((unsigned char *) ptr_val(tuple) + Arg(2));
     *p = element;
     NextPF(3, next);
 }

 OpCase(int_bnot_jsd):
    GetArg1(1, tmp_arg1);
    if (is_small(tmp_arg1)) {
	tmp_arg1 = make_small(~signed_val(tmp_arg1));
    } else if (is_big(tmp_arg1)) {
	uint32* bigp = ArithAlloc(c_p, BIG_NEED_SIZE(big_size(tmp_arg1)+1));
	tmp_arg1 = big_bnot(tmp_arg1, bigp);
	ArithCheck(c_p);
	if (is_nil(tmp_arg1)) {
	    goto system_limit;
	}
    } else {
	goto badarith;
    }
    StoreBifResult(2, tmp_arg1);

 OpCase(i_is_ne_exact_f):
    if (eq(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = NORMAL;
     c_p->arity = 0;		/* In case this process will ever be garbed again. */
     do_exit(c_p, am_normal);
     return REDS_IN(c_p) - FCALLS;
 }

    /*
     * Suspend BIF and prepare BIF to be rescheduled.
     */
 suspend_bif: {
     uint32* argp = c_p->arg_reg;
     argp[0] = r(0);
     argp[1] = x(1);
     argp[2] = x(2);
     SWAPOUT;
     c_p->i = I;
     c_p->current = NULL;
     return REDS_IN(c_p) - FCALLS;
 }

 OpCase(badmatch_s): {
     GetArg1(0, tmp_arg1);
     c_p->fvalue = tmp_arg1;
 }

 badmatch: {
     c_p->freason = BADMATCH;
 }

 find_func_info: {
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, I, reg, NULL);
     goto post_error_handling;
 }

 OpCase(call_error_handler):
    /*
     * At this point, I points to the code[3] in the export entry for
     * a function which is not loaded.
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&call_error_handler
     * code[4]: Not used
     */
    SWAPOUT;
    reg[0] = r(0);
    tmp_arg1 = call_error_handler(c_p, I-3, reg);
    r(0) = reg[0];
    HTOP = c_p->htop;
    if (tmp_arg1) {
	SET_I(c_p->i);
	Dispatch();
    }

 /* Fall through */
 OpCase(error_action_code): {
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, NULL, reg, NULL);
 post_error_handling:
     if (I == 0) {
	 return REDS_IN(c_p) - FCALLS;
     } else {
	 r(0) = reg[0];
	 CHECK_TERM(r(0));
	 SWAPIN;
	 Goto(*I);
     }
 }

 OpCase(apply_bif):
    /*
     * At this point, I points to the code[3] in the export entry for
     * the BIF:
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&apply_bif
     * code[4]: Function pointer to BIF function
     */
     {
	BifFunction bf;

	c_p->current = I-3;	/* In case we apply process_info. */
	c_p->i = I;		/* In case we apply check_process_code. */
	c_p->arity = 0;		/* To allow garbage collection on ourselves
				 * (check_process_code/2).
				 */
				   
	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	bf = (BifFunction) Arg(0);
	tmp_arg1 = (*bf)(c_p, r(0), x(1), x(2));
	FCALLS = c_p->fcalls;
	SWAPIN;			/* There might have been a garbage collection. */
	if (tmp_arg1) {
	    r(0) = tmp_arg1;
	    CHECK_TERM(r(0));
	    SET_I(c_p->cp);
	    Goto(*I);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = I[-1];
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    SET_I(((Export *)(c_p->fvalue))->address);
	    r(0) = c_p->def_arg_reg[0];
	    x(1) = c_p->def_arg_reg[1];
	    x(2) = c_p->def_arg_reg[2];
	    Dispatch();
	}
	reg[0] = r(0);
	I = handle_error(c_p, c_p->cp, reg, bf);
	goto post_error_handling;
    }

 OpCase(i_put_tuple_only_Ad): {
     tmp_arg1 = make_tuple(HTOP);
     *HTOP++ = Arg(0);
     StoreBifResult(1, tmp_arg1);
 }

    /*
     * Pick up the next message and place it in the selected y register,
     * ready for pattern matching.
     * If no message, jump to a wait or wait_timeout instruction.
     */
 OpCase(loop_rec_fy):
 {
     uint32* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((uint32 *) Arg(0));
	 Goto(*I);		/* Jump to a wait or wait_timeout instruction */
     }
     PreFetch(2, next);
     yb(Arg(1)) = msgp->mesg;
     CHECK_TERM(msgp->mesg);
     NextPF(2, next);
 }

 OpCase(case_end_s):
    GetArg1(0, tmp_arg1);
    c_p->fvalue = tmp_arg1;
    c_p->freason = CASE_CLAUSE;
    goto find_func_info;

 OpCase(if_end):
    c_p->freason = IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_aaI): {
     c_p->freason = FUNCTION_CLAUSE;
     c_p->current = I + 1;
     goto lb_error_action_code;
 }

#include "beam_cold.h"

 OpCase(is_eq_exact_body): {
     uint32* next;

     PreFetch(0, next);
     if (tmp_arg1 == tmp_arg2 || eq(tmp_arg1, tmp_arg2)) {
	 NextPF(0, next);
     }
     Badmatch(tmp_arg1);
 }

    /*
     * Trace and debugging support.
     */

 OpCase(deallocate_I): {	/* Only used when tracing. */
     uint32* next;

     PreFetch(1, next);
     D(Arg(0));
     NextPF(1, next);
 }

 OpCase(i_trace_info):
    if (IS_TRACED_FL(c_p, F_TRACE_CALLS_OLD)) {
	reg[0] = r(0);
	erts_trace_call_or_ret(c_p, I[-3], I[-2], I[-1], reg, am_call);
    }
    Next(0);

 OpCase(i_trace_return):
    SET_I(c_p->cp);
    if (IS_TRACED_FL(c_p, F_TRACE_CALLS_OLD|F_TIMESTAMP)) {
	if ((c_p->current = find_function_from_pc(I)) == NULL) {
	/*
	 * We will assume that this is the initial function
	 * (e.g. spawn_link(erlang, abs, [1])).
	 */
	    c_p->current = c_p->initial;
	}
	reg[0] = r(0);
	erts_trace_call_or_ret(c_p, c_p->current[0], c_p->current[1], c_p->current[2],
			       reg, am_return_to);
    }
    Goto(*I);

    /*
     * At this point, I points to the code[3] in the export entry for
     * a trace-enabled function.
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&call_traced_function
     * code[4]: Address of function.
     */
 OpCase(call_traced_function): {
     if (IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 unsigned offset = (unsigned) (((Export *) 0)->code+3);
	 Export* ep = (Export *) (((char *)I)-offset);
	 Uint32 flags;

	 SWAPOUT;
	 reg[0] = r(0);
	 flags = erts_call_trace(c_p, ep, reg);
	 SWAPIN;
	 
	 if (flags & MATCH_SET_RETURN_TRACE) {
	     static void* return_trace[1] = {OpCode(return_trace)};

	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 2 < HTOP) {
		 /* SWAPOUT, SWAPIN was done and r(0) was saved above */
		 FCALLS -= erts_garbage_collect(c_p, 2, reg, ep->code[2]);
		 r(0) = reg[0];
		 SWAPIN;
	     }
	     E -= 2;
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     ASSERT(is_CP((Eterm)ep));
	     E[1] = make_cp(c_p->cp);
	     E[0] = make_cp(ep);
	     c_p->cp = (Eterm *) make_cp(return_trace);
	 }
     }
     SET_I((Uint *) Arg(0));
     Dispatch();
 }

 OpCase(return_trace): {
     Export* ep = (Export *) E[0];

     erts_trace_return(c_p, ep->code, r(0));
     SET_I((Eterm *) E[1]);
     E += 2;
     Goto(*I);
 }

 /*
  * Instructions for module_info/0,1.
  */

 OpCase(i_module_info_0_a): {
     SWAPOUT;
     r(0) = module_info_0(c_p, Arg(0));
     HTOP = c_p->htop;
     SET_I(c_p->cp);
     Goto(*I);
 }

 OpCase(i_module_info_1_a): {
     uint32 res;

     SWAPOUT;
     res = module_info_1(c_p, Arg(0), r(0));
     HTOP = c_p->htop;
     if (res != 0) {
	 r(0) = res;
	 SET_I(c_p->cp);
	 Goto(*I);
     }
     c_p->freason = FUNCTION_CLAUSE;
     c_p->current = I-3;
     goto lb_error_action_code;
 }

    DEFINE_COUNTING_LABELS;
#ifndef NO_JUMP_TABLE
#ifdef DEBUG
 end_emulator_loop:
#endif
#endif

 OpCase(int_code_end):
 OpCase(label_L):
    erl_exit(1, "meta op\n");

    /*
     * One-time initialization of Beam emulator.
     */

 init_emulator:
 {
     int i;
     Export* ep;

#ifndef NO_JUMP_TABLE
     /* Are tables correctly generated by beam_makeops? */
     ASSERT(sizeof(counting_opcodes) == sizeof(opcodes));
     
     if (!count_instructions) {
	 beam_ops = opcodes;
     } else {
#ifdef DEBUG
	 counting_opcodes[op_catch_end_y] = LabelAddr(lb_catch_end_y);
#endif
	 counting_opcodes[op_i_func_info_aaI] = LabelAddr(lb_i_func_info_aaI);
	 beam_ops = counting_opcodes;
     }
#endif /* NO_JUMP_TABLE */
     
     em_call_error_handler = OpCode(call_error_handler);
     em_call_traced_function = OpCode(call_traced_function);
     em_apply_bif = OpCode(apply_bif);
     beam_apply[0] = (Eterm) OpCode(i_apply);
     beam_apply[1] = (Eterm) OpCode(normal_exit);
     beam_exit[0] = (Eterm) OpCode(error_action_code);

     /*
      * Enter all BIFs into the export table.
      */
     for (i = 0; i < BIF_SIZE; i++) {
	 ep = erts_export_put(bif_table[i].module,
			      bif_table[i].name,
			      bif_table[i].arity);
	 bif_export[i] = ep;
	 ep->code[3] = (uint32) OpCode(apply_bif);
	 ep->code[4] = (uint32) bif_table[i].f;
     }

     /*
      * erts_debug:apply(Mod, Func, Args, {M,F,A})
      *  Applies Mod:Func with Args in the same way as apply/3.
      *  If a BIF is applied and it fails, the normal EXIT code
      *  to look as if the error occurred in function {M,F,A}.
      *  Also, the normal stack backtrace will suppressed.
      */

     ep = erts_export_put(am_erts_debug, am_apply, 4);
     ep->address = (Eterm *) (beam_debug_apply+4);

     /* func_info erts_debug apply 4 */
     beam_debug_apply[0] = (Eterm) OpCode(i_func_info_aaI);
     beam_debug_apply[1] = am_erts_debug;
     beam_debug_apply[2] = am_apply;
     beam_debug_apply[3] = 4;

     /* allocate 1 4 */
     beam_debug_apply[4] = (Eterm) OpCode(allocate_tt);
     beam_debug_apply[5] = (4 << 16) | 1;

     /* move {x,3} {y,0} */
     beam_debug_apply[6] = (Eterm) OpCode(move_xy);
     beam_debug_apply[7] = (1 << 18) | (3 << 2);

     /* i_apply */
     beam_debug_apply[8] = (Eterm) OpCode(i_apply);

     /* deallocate_return 1 */
     beam_debug_apply[9] = (Eterm) OpCode(deallocate_return_P);
     beam_debug_apply[10] = (1+1)*4;

     return 0;
 }
#ifdef NO_JUMP_TABLE
 default:
    erl_exit(1, "unexpected op code %d\n",Go);
  }
#endif
    return 0;			/* Never executed */

  save_calls1:
    {
       uint32* dis_next;

       save_calls(c_p, (Export *) Arg(0));

       SET_I(((Export *) Arg(0))->address);

       dis_next = (uint32 *) *I;
       FCALLS--;
       Goto(dis_next);
    }
}

static uint32*
handle_error(Process* c_p, uint32* pc, uint32* reg, BifFunction bf)
{
    uint32* hp;

#if defined(DEBUG)
#  define KILL_HP(hp)  ((hp) = NULL)
#else
#  define KILL_HP(hp)
#endif

    /*
     * For most exceptions, the error reason will look like {Value,Where},
     * where Where is a list.
     * Where will not be used for exit/1 and throw/1.
     */

    Eterm Value;
    Eterm Where = NIL;
    Eterm* next_p = &Where;	/* Where to store the next element of Where. */
    Uint* prev = NULL;		/* Pointer to func_info for previous function
				 * put in Where.
				 */

    /*
     * Mapping from error codes to atoms.
     */
    static Eterm error_atom[NUMBER_EXIT_CODES] = {
	am_normal,		/* 0 */
	am_badmatch,		/* 1 */
	am_case_clause,		/* 2 */
	am_if_clause,		/* 3 */
	am_undef,		/* 4 */
	am_badarg,		/* 5 */
	am_badarith,		/* 6 */
	am_function_clause,	/* 7 */
	am_badsig,		/* 8 */
	am_timeout_value,	/* 9 */
	am_nocatch,		/* 10 */
	am_noproc,		/* 11 */
	am_notalive,		/* 12 */
	am_system_limit,	/* 13 */
	am_badfun,		/* 14 */
	am_internal_error,	/* 15 */
	am_internal_error,	/* 16 */
	am_internal_error,	/* 17 */
	am_internal_error,	/* 18 */
	am_internal_error,	/* 19 */
	am_badarity,		/* 20 */
    };

    /*
     * First, make sure that we know the {M,F,A} of the current function.
     */

    if (pc != NULL) {
	if ((c_p->current = find_function_from_pc(pc)) == NULL) {
	    if (beam_debug_apply <= pc && pc < beam_debug_apply+beam_debug_apply_size) {
		c_p->current = c_p->stop;
		if (c_p->freason == USER_ERROR2) {
		    c_p->freason = USER_ERROR;
		    bf = NULL;
		}
	    } else {
		/*
		 * We will assume that this is the initial function
		 * (e.g. spawn_link(erlang, abs, [1])).
		 */
		c_p->current = c_p->initial;
	    }
	}
    }

    /*
     * Retrieve the atom to use in Value.
     */

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */
    ASSERT(c_p->freason != RESCHEDULE); /* Should have been handled earlier. */
    ASSERT(0 <= c_p->freason && c_p->freason < NUMBER_EXIT_CODES); /* Out of range. */

    if (0 <= c_p->freason && c_p->freason < NUMBER_EXIT_CODES) {
	Value = error_atom[c_p->freason];
    } else {
	Value = am_internal_error;
	c_p->freason = INTERNAL_ERROR;
    }

    /*
     * Some errors are a little bit more complicated.
     */

    switch (c_p->freason) {
    case BADMATCH:
    case CASE_CLAUSE:
    case BADFUN:
	ASSERT(c_p->fvalue != 0);
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, Value, c_p->fvalue);
	KILL_HP(hp);
	break;
    case USER_EXIT:
    case USER_ERROR:
	ASSERT(c_p->fvalue != 0);
	Value = c_p->fvalue;
	break;
    case USER_ERROR2:
	{
	    Eterm mfa;
	    Eterm* tp;

	    ASSERT(is_tuple(c_p->fvalue));
	    prev = c_p->current;
	    ASSERT(prev != NULL);
	    hp = HAlloc(c_p, 6);
	    tp = ptr_val(c_p->fvalue);
	    Value = tp[1];
	    mfa = TUPLE3(hp, prev[0], prev[1], tp[2]);
	    hp += 4;
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = hp + 1;
	    bf = NULL;
	    KILL_HP(hp);
	    break;
	}
    case BADARITY:
	ASSERT(c_p->fvalue != 0);
	hp = HAlloc(c_p, 2);
	ASSERT(*next_p == NIL);
	*next_p = CONS(hp, c_p->fvalue, NIL);
	next_p = hp + 1;
	KILL_HP(hp);
	break;
    case THROWN:
	if (c_p->catches != 0) {
	    ASSERT(c_p->fvalue != 0);
	    Value = c_p->fvalue;
	} else {
	    hp = HAlloc(c_p, 3);
	    Value = TUPLE2(hp, am_nocatch, c_p->fvalue);
	    c_p->freason = UNDEF; /* Force stack backtrace. */
	    KILL_HP(hp);
	}
	break;
    case INTERNAL_ERROR:
	c_p->catches = 0;
	break;
    }

#ifdef DEBUG
    c_p->fvalue = 0;
    ASSERT(Value != am_internal_error);
#endif

    /*
     * Build the Where part of the reason (except exit/1, throw/1, and normal
     * process termination).
     */

    if (c_p->freason == THROWN || c_p->freason == NORMAL || c_p->freason == USER_EXIT) {
	reg[0] = Value;
    } else {
	Eterm mfa;
	int max_depth = erts_backtrace_depth;
	Uint* fi;
	Uint* ptr;

	/*
	 * If the failure was in a BIF, this is the first element in Where.
	 * If the failure was caused by erlang:fault/1, we won't show it.
	 */

	if (bf != NULL && bf != fault_1) {
	    int i;

	    for (i = 0; i < BIF_SIZE; i++) {
		if (bf == bif_table[i].f || bf == bif_table[i].traced) {
		    int arity = bif_table[i].arity;
		    Eterm args;

		    hp = HAlloc(c_p, 6+2*arity);
		    args = NIL;
		    while (arity > 0) {
			args = CONS(hp, reg[arity-1], args);
			hp += 2;
			arity--;
		    }
		    mfa = TUPLE3(hp, bif_table[i].module, bif_table[i].name, args);
		    hp += 4;
		    ASSERT(*next_p == NIL);
		    *next_p = CONS(hp, mfa, NIL);
		    next_p = hp + 1;
		    KILL_HP(hp);
		    break;
		}
	    }
	    if (i >= BIF_SIZE) {
		erl_exit(1, "Bif not found");
	    }
	}

	/*
	 * Add the {M,F,A} for the current function, where A is arity or arguments.
	 */

	if (c_p->current != prev) {
	    Eterm a;		/* Arguments or arity. */
	    prev = c_p->current;
	    if (c_p->current == c_p->stop) {
		mfa = c_p->stop[1];
		max_depth = 0;
	    } else {
		if (c_p->freason != FUNCTION_CLAUSE) {
		    a = make_small(prev[2]);
		} else {
		    int i;

		    hp = HAlloc(c_p, 2*prev[2]);
		    a = NIL;
		    for (i = prev[2]-1; i >= 0; i--) {
			a = CONS(hp, reg[i], a);
			hp += 2;
		    }
		    KILL_HP(hp);
		}
		hp = HAlloc(c_p, 4);
		mfa = TUPLE3(hp, prev[0], prev[1], a);
		hp += 4;
	    }
	    hp = HAlloc(c_p, 2);
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = hp + 1;
	    KILL_HP(hp);
	}

	/*
	 * The continuation pointer (c_p->cp) in most cases points to
	 * a function which we have called and already returned from,
	 * because the deallocate_return instruction doesn't update c_p->cp.
	 * Therefore, we will ignore c_p->cp, except for function_clause
	 * when it *is* accurate.
	 */

	fi = find_function_from_pc(c_p->cp);
	if (c_p->freason == FUNCTION_CLAUSE && fi != NULL &&
	    fi != prev && max_depth > 0) {
	    prev = fi;
	    hp = HAlloc(c_p, 6);
	    mfa = TUPLE3(hp, fi[0], fi[1], make_small(fi[2]));
	    hp += 4;
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = hp + 1;
	    max_depth--;
	    KILL_HP(hp);
	}


	/*
	 * Traverse the stack backwards and add all unique functions
	 * to Where.
	 */

	if (max_depth != 0) {
	    hp = HAlloc(c_p, max_depth*6);
	    for (ptr = c_p->stop; ptr < c_p->hend; ptr++) {
		if (is_CP(*ptr)) {
		    fi = find_function_from_pc(cp_ptr_val(*ptr));
		    if (fi != NULL && fi != prev) {
			if (max_depth-- <= 1) {
			    ASSERT(*next_p == NIL);
			    *next_p = am_more;
			    break;
			}
			prev = fi;
			mfa = TUPLE3(hp, fi[0], fi[1], make_small(fi[2]));
			hp += 4;
			ASSERT(*next_p == NIL);
			*next_p = CONS(hp, mfa, NIL);
			hp += 2;
			next_p = hp - 1;
		    }
		}
	    }
	    KILL_HP(hp);
	}

	/*
	 * Build the final error term: {Value,Where}.
	 */

	hp = HAlloc(c_p, 3);
	reg[0] = TUPLE2(hp, Value, Where);
	KILL_HP(hp);
    }

    if (c_p->catches == 0) {
	/*
	 * No catch active -- terminate the process.
	 */
	if (c_p->freason != NORMAL && c_p->freason != USER_EXIT) {
	    cerr_pos = 0;
	    erl_printf(CBUF, "Error in process ");
	    display(c_p->id, CBUF);
	    if (this_node != am_Noname) {
		erl_printf(CBUF, " on node ");
		print_atom(unsigned_val(this_node), CBUF);
	    }
	    erl_printf(CBUF, " with exit value: ");
	    ldisplay(reg[0], CBUF, display_items);
	    erl_printf(CBUF, "\n");
	    send_error_to_logger(c_p->group_leader);
	}

	/*
	 * If zombies are kept, the process will be garbage-collected.
	 * Must zero c_p->arity to indicate that there are no live registers.
	 */
	c_p->arity = 0;
	do_exit(c_p, reg[0]);
    } else {
	Eterm* ptr;

	/*
	 * Search for the first catch.
	 */

	for (ptr = c_p->stop + 1; ptr < c_p->hend; ptr++) {
	    if (is_catch(*ptr)) {
		pc = (uint32 *)(ptr_val(*ptr));
		while (is_not_CP(*ptr)) {
		    ptr--;
		    ASSERT(c_p->stop <= ptr);
		}
		c_p->stop = ptr;
		
		if (c_p->freason != THROWN) {
		    hp = HAlloc(c_p, 3);
		    reg[0] = TUPLE2(hp, am_EXIT, reg[0]);
		}
		return pc;
	    }
	}
	erl_exit(1, "Catch not found");
    }
    return 0;
}


static uint32
call_error_handler(Process* p, uint32* fi, uint32* reg)
{
    uint32* hp;
    Export* ep;
    int arity;
    uint32 args;
    int i;

    /*
     * Search for the error_handler module.
     */
    ep = erts_find_function(p->error_handler, am_undefined_function, 3);
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = UNDEF;
	return 0;
    }
    p->i = ep->address;

    /*
     * Create a list with all arguments in the x registers.
     */

    arity = fi[2];
    hp = HAlloc(p, arity*2);
    args = NIL;
    for (i = arity-1; i >= 0; i--) {
	args = CONS(hp, reg[i], args);
	hp += 2;
    }

    /*
     * Set up registers for call to error_handler:undefined_function/3.
     */
    reg[0] = fi[0];
    reg[1] = fi[1];
    reg[2] = args;
    return 1;
}


static uint32
module_info_0(Process* p, uint32 module)
{
    uint32* hp;
    uint32 list = NIL;
    uint32 tup;

#define BUILD_INFO(What) \
    tup = module_info_1(p, module, What); \
    hp = HAlloc(p, 5); \
    tup = TUPLE2(hp, What, tup); \
    hp += 3; \
    list = CONS(hp, tup, list)

    BUILD_INFO(am_compile);
    BUILD_INFO(am_attributes);
    BUILD_INFO(am_imports);
    BUILD_INFO(am_exports);

#undef BUILD_INFO

    return list;
}

static Eterm
module_info_1(Process* p, Eterm module, Eterm what)
{
    if (what == am_module) {
	return module;
    } else if (what == am_imports) {
	return NIL;
    } else if (what == am_exports) {
	return exported_from_module(p, module);
    } else if (what == am_functions) {
	return functions_in_module(p, module);
    } else if (what == am_attributes) {
	return attributes_for_module(p, module);
    } else if (what == am_compile) {
	return compilation_info_for_module(p, module);
    }
    return 0;
}

static Uint*
apply(Process* p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Export* ep;
    Eterm tmp;

    /*
     * Check the arguments which should be of the form apply(Module,
     * Function, Arguments) where Module and Function are atoms and
     * Arguments is an arity long list of terms.
     */
    if (is_not_atom(module) || is_not_atom(function)) {
	/*
	 * No need to test args here -- done below.
	 */
    error:
	p->freason = BADARG;

    error2:
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
	return 0;
    }

    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]).
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < MAX_REG) {
	    reg[arity++] = CAR(ptr_val(tmp));
	    tmp = CDR(ptr_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    goto error2;
	}
    }
    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	goto error;
    }

    /*
     * Get the index into the export table, or failing that the export
     * entry for the error handler module.  Only give up if no error
     * handler module.
     *
     * Note: All BIFs have export entries; thus, no special case is needed.
     */

    if ((ep = erts_find_export_entry(module, function, arity)) == NULL) {
	if ((ep = erts_find_export_entry(p->error_handler,
					 am_undefined_function, 3)) == NULL) {
	    goto error;
	} else {
	    reg[0] = module;
	    reg[1] = function;
	    reg[2] = args;
	}
    } else if (p->ct != NULL) {
	save_calls(p, ep);
    }

    return ep->address;
}

static Uint*
call_fun(Process* p,		/* Current process. */
	 int arity,		/* Number of arguments for Fun. */
	 Eterm* reg,		/* Contents of registers. */
	 Eterm args)		/* 0 or pre-built list of arguments. */
{
    Eterm fun = reg[arity];
    int i;
    Eterm function;
    Eterm* code;
    Eterm* hp;

    if (is_binary(fun)) {
	ErlFunThing* funp = (ErlFunThing *) ptr_val(fun);
	Eterm* current_code;
	Export* ep;
	Module* modp;
	Uint index;
	Uint uniq;
	Eterm* var_ptr;
	unsigned num_free;

	if (thing_subtag(funp->thing_word) != FUN_SUBTAG) {
	    goto badfun;
	}
	modp = funp->modp;
	index = funp->index;
	uniq = funp->uniq;
	var_ptr = funp->env;
	num_free = funp->num_free;
	    
	/*
	 * Search for and invoke the Fun in the new version of the code.
	 */

	current_code = code = modp->code;
	if (code != NULL && index < code[MI_NUM_LAMBDAS]) {
	    Uint* fun_entry = ((Uint *) code[MI_LAMBDA_PTR]) + 2*index;
	    Uint* code_ptr =  (Uint *) fun_entry[1];

	    if (fun_entry[0] == uniq) {
		if (code_ptr[-1] != arity+num_free) {
		badarity:
		    if (args == 0) {
			args = NIL;
			hp = HAlloc(p, arity*2);
			for (i = arity-1; i >= 0; i--) {
			    args = CONS(hp, reg[i], args);
			    hp += 2;
			}
		    }

		    hp = HAlloc(p, 3);
		    p->freason = BADARITY;
		    p->fvalue = TUPLE2(hp, fun, args);
		    return NULL;
		}
		reg += arity;
		for (i = 0; i < num_free; i++) {
		    reg[i] = var_ptr[i];
		}
		return code_ptr;
	    }
	}

	/*
	 * Not found in new code.  Try old code.
	 */

	code = modp->old_code;
	if (code != NULL && index < code[MI_NUM_LAMBDAS]) {
	    Uint* fun_entry = ((Uint *) code[MI_LAMBDA_PTR]) + 2*index;
	    Uint* code_ptr =  (Uint *) fun_entry[1];

	    if (fun_entry[0] == uniq) {
		if (code_ptr[-1] != arity+num_free) {
		    goto badarity;
		}
		reg += arity;
		for (i = 0; i < num_free; i++) {
		    reg[i] = var_ptr[i];
		}
		return code_ptr;
	    }
	}

	if (current_code == NULL) {
	    /*
	     * This is important to be able to debug funs in
	     * a module which has ever been loaded.
	     */
	    ep = erts_find_export_entry(p->error_handler, am_undefined_lambda, 3);
	    if (ep == NULL) {
		p->freason = UNDEF;
		return 0;
	    }
	    
	    if (args == 0) {
		hp = HAlloc(p, 2*arity);
		args = NIL;
		while (arity-- > 0) {
		    args = CONS(hp, reg[arity], args);
		    hp += 2;
		}
	    }

	    reg[0] = make_atom(modp->module);
	    reg[1] = fun;
	    reg[2] = args;
	    return ep->address;
	}

	/*
	 * No such lambda.  Fall into the badfun case.
	 */
    } else if (is_tuple(fun)) {
	Eterm* tp = ptr_val(fun);

#if defined(ALLOW_FUN_TUPLES)	
	if (*tp == make_arityval(5) && tp[1] == am_fun) {
	    Eterm module = tp[2];
	    Module* modp;
	    unsigned index;
	    Eterm* var_ptr;
	    unsigned num_free;
	    Eterm* current_code;
	    Export* ep;

	    if (!is_tuple(tp[5])) {
		goto badfun;
	    }
	    var_ptr = ptr_val(tp[5]);
	    num_free = arityval(*var_ptr);
	    
	    /*
	     * If the module isn't loaded, we must invoke
	     * error_handler:undefined_lambda/3.
	     */

	    modp = erts_get_module(module);
	    if (modp == NULL) {
	    not_loaded:
		ep = erts_find_export_entry(p->error_handler, am_undefined_lambda, 3);
		if (ep == NULL) {
		    p->freason = UNDEF;
		    return 0;
		}
	    
		if (args == 0) {
		    hp = HAlloc(p, 2*arity);
		    args = NIL;
		    while (arity-- > 0) {
			args = CONS(hp, reg[arity], args);
			hp += 2;
		    }
		}
		
		reg[0] = module;
		reg[1] = fun;
		reg[2] = args;
		return ep->address;
	    }
	    
	    index = unsigned_val(tp[3]);
	    
	    /*
	     * Search for and invoke the Fun in the new version of the code.
	     */
	    
	    current_code = code = modp->code;
	    if (code != NULL && index < code[MI_NUM_LAMBDAS]) {
		Uint* fun_entry = ((Uint *) code[MI_LAMBDA_PTR]) + 2*index;
		Uint* code_ptr =  (Uint *) fun_entry[1];

		if (fun_entry[0] == tp[4]) {
		    if (code_ptr[-1] != arity+num_free) {
			goto badarity;
		    }
		    reg += arity;
		    var_ptr++;
		    for (i = 0; i < num_free; i++) {
			reg[i] = var_ptr[i];
		    }
		    return code_ptr;
		}
	    }
	    
	    /*
	     * Not found in new code.  Try old code.
	     */
	    
	    code = modp->old_code;
	    if (code != NULL && index < code[MI_NUM_LAMBDAS]) {
		Uint* fun_entry = ((Uint *) code[MI_LAMBDA_PTR]) + 2*index;
		Uint* code_ptr =  (Uint *) fun_entry[1];

		if (fun_entry[0] == tp[4]) {
		    if (code_ptr[-1] != arity+num_free) {
			goto badarity;
		    }
		    reg += arity;
		    var_ptr++;
		    for (i = 0; i < num_free; i++) {
			reg[i] = var_ptr[i];
		    }
		    return code_ptr;
		}
	    }
	    
	    if (current_code == NULL) {
		/*
		 * This is important to be able to debug funs in
		 * a module which has ever been loaded.
		 */
		goto not_loaded;
	    }
	    
	    /*
	     * No such lambda.  Fall into the badfun case.
	     */
	} else
#endif
	    if (*tp == make_arityval(2)) {
		Export* ep;
		Eterm module;

		module = tp[1];
		function = tp[2];
		if (!is_atom(module) || !is_atom(function)) {
		    goto badfun;
		}
		if ((ep = erts_find_export_entry(module, function, arity)) == NULL) {
		    ep = erts_find_export_entry(p->error_handler, am_undefined_function, 3);
		    if (ep == NULL) {
			p->freason = UNDEF;
			return 0;
		    }
		    if (args == 0) {
			hp = HAlloc(p, 2*arity);
			args = NIL;
			while (arity-- > 0) {
			    args = CONS(hp, reg[arity], args);
			    hp += 2;
			}
		    }

		    reg[0] = module;
		    reg[1] = function;
		    reg[2] = args;
		}
		return ep->address;
	    }
    }

    /*
     * Default error reason if the Fun argument is bad.
     */
 badfun:
    p->freason = BADFUN;
    p->fvalue = fun;
    return NULL;
}

static uint32*
apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;

    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]).
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < MAX_REG) {
	    reg[arity++] = CAR(ptr_val(tmp));
	    tmp = CDR(ptr_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    return NULL;
	}
    }

    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	p->freason = UNDEF;
	return NULL;
    }
    reg[arity] = fun;
    return call_fun(p, arity, reg, args);
}


static Eterm
make_fun(Process* p, Eterm* reg, Eterm mod, unsigned index,
	 unsigned uniq, unsigned num_free)
{
#if 1
    unsigned needed = ERL_FUN_SIZE + num_free - 1;
    ErlFunThing* funp = (ErlFunThing *) HAlloc(p, needed);
    Eterm* hp = funp->env;
    int i;

    funp->thing_word = make_thing(ERL_FUN_SIZE-2, FUN_SUBTAG);
    funp->next = p->off_heap.funs;
    p->off_heap.funs = funp;
    funp->modp = erts_put_module(mod);
    funp->index = index;
    funp->uniq = make_small(uniq);
    funp->num_free = num_free;
    funp->creator = p->id;
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }
    return make_binary(funp);
#else
    unsigned needed = 7 + num_free;
    Eterm* hp = HAlloc(p, needed);
    Eterm free_vars = make_tuple(hp);
    int i;

    *hp++ = make_arityval(num_free);
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }

    return TUPLE5(hp, am_fun, mod, make_small(index), make_small(uniq), free_vars);
#endif
}



uint32*
arith_alloc(Process* p, uint32 need)
{
    ErlHeapFragment* bp;
    uint32 n;
    uint32* hp;
#ifdef DEBUG
    uint32 i;
#endif

    n = (need < 128) ? 128 : need;
    bp = new_message_buffer(n+1);
    bp->next = p->mbuf;
    p->mbuf = bp;
    p->mbuf_sz += n+1;
    p->arith_avail = n - need;
    hp = bp->mem;
#ifdef DEBUG
    for (i = 0; i <= n; i++) {
	hp[i] = ARITH_MARKER;
    }
#endif
    p->arith_heap = hp + need;
#ifdef DEBUG
    p->arith_check_me = p->arith_heap;
#endif
    return hp;
}

static uint32
mixed_eq(uint32 arg1, uint32 arg2)
{
    FloatDef farg1, farg2;

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:		/* Has to be false */
    case BIG_SMALL:
	return 0;
    case SMALL_FLOAT:
	GET_DOUBLE(arg2, farg2);
	return signed_val(arg1) == farg2.fd;
    case BIG_FLOAT:
	GET_DOUBLE(arg2, farg2);
	return big_to_double(arg1) == farg2.fd;
    case BIG_BIG:
	return big_comp(arg1, arg2) == 0;
    case FLOAT_SMALL:
	GET_DOUBLE(arg1, farg1);
	return farg1.fd == signed_val(arg2);
    case FLOAT_BIG:
	GET_DOUBLE(arg1, farg1);
	return farg1.fd == big_to_double(arg2);
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, farg1);
	GET_DOUBLE(arg2, farg2);
	return farg1.fd == farg2.fd;
    default:
	return cmp(arg1, arg2) == 0;
    }
}

/*
 * XXX This function should probably be removed and cmp() be called directly.
 */

static uint32
mixed_ge(uint32 arg1, uint32 arg2)
{
    FloatDef farg1, farg2;
    double tmp;

    if (arg1 == arg2)
	return 1;		/* Identity or equal SMALL */

    /*
     * Note: Nil ([]) is a special case of BIG.  Nil is greater than any number.
     */

    switch (NUMBER_CODE(arg1, arg2)) {
    case SMALL_BIG:
	return big_sign(arg2) != 0; /* Greater if negative big. */
    case BIG_SMALL:
	return big_sign(arg1) == 0; /* Greater if positive big. */
    case SMALL_FLOAT:
	GET_DOUBLE(arg2, farg2);
	return signed_val(arg1) >= farg2.fd;
    case BIG_FLOAT:
	GET_DOUBLE(arg2, farg2);
	tmp = big_to_double(arg1);
	if (!FP_RESULT_OK(tmp))
	   return big_sign(arg1) == 0;
	return tmp >= farg2.fd;
    case BIG_BIG:
	return big_comp(arg1, arg2) >= 0;
    case FLOAT_SMALL:
	GET_DOUBLE(arg1, farg1);
	return farg1.fd >= signed_val(arg2);
    case FLOAT_BIG:
	GET_DOUBLE(arg1, farg1);
	tmp = big_to_double(arg2);
	if (!FP_RESULT_OK(tmp))
	   return big_sign(arg2) != 0;
	return farg1.fd >= tmp;
    case FLOAT_FLOAT:
	GET_DOUBLE(arg1, farg1);
	GET_DOUBLE(arg2, farg2);
	return farg1.fd >= farg2.fd;
    default:
	return cmp(arg1, arg2) >= 0;
    }
}


int catchlevel(Process *p)
{
   return p->catches;
}

/*
 * Check if the given function is built-in (i.e. a BIF implemented in C).
 *
 * Returns 0 if not built-in, and a non-zero value if built-in.
 */

int
erts_is_builtin(Eterm Mod, Eterm Name, int arity)
{
    Export e;
    Export* ep;

    e.code[0] = Mod;
    e.code[1] = Name;
    e.code[2] = arity;

    if ((ep = hash_get(&export_table.htable, (void*) &e)) == NULL) {
	return 0;
    }
    return ep->address == ep->code+3 && (ep->code[3] == (Uint) em_apply_bif);
}

/*
 * Setup function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

int
erts_setup_func_trace(Export* ep, void* match_prog)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = match_prog;
	    MatchSetRef(ep->match_prog_set);
	    return 1;
	} else if (ep->code[3] == (Uint) em_apply_bif) {
	    int i;
	    BifFunction func = (BifFunction) ep->code[4];

	    ASSERT(func != NULL);
	    for (i = 0; i < BIF_SIZE; i++) {
		if (func == bif_table[i].f) {
		    ep->code[4] = (Uint) bif_table[i].traced;
		    /* Can't have a match program if not traced. */
		    ASSERT(ep->match_prog_set == NULL);
		    ep->match_prog_set = match_prog;
		    MatchSetRef(ep->match_prog_set);
		    break;
		} else if (func == bif_table[i].traced) { /* Change match program */
		    MatchSetUnref(ep->match_prog_set);
		    ep->match_prog_set = match_prog;
		    MatchSetRef(ep->match_prog_set);
		    break;
		}
	    }
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }

    ep->code[3] = (Uint) em_call_traced_function;
    ep->code[4] = (Uint) ep->address;
    ep->address = ep->code+3;
    ep->match_prog_set = match_prog;
    MatchSetRef(ep->match_prog_set);
    return 1;
}

/*
 * Reset function tracing for the given exported function.
 *
 * Return Value: 1 if entry refers to a BIF or loaded function,
 * 0 if the entry refers to a function not loaded.
 */

int
erts_reset_func_trace(Export* ep)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return 0;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    ep->address = (Uint *) ep->code[4];
	    MatchSetUnref(ep->match_prog_set);
	    ep->match_prog_set = NULL;
	    return 1;
	} else if (ep->code[3] == (Uint) em_apply_bif) {
	    int i;
	    BifFunction func = (BifFunction) ep->code[4];

	    ASSERT(func != NULL);
	    for (i = 0; i < BIF_SIZE; i++) {
		if (bif_table[i].traced == func) {
		    ep->code[4] = (Uint) bif_table[i].f;
		    MatchSetUnref(ep->match_prog_set);
		    ep->match_prog_set = NULL;
		}
	    }
	    return 1;
	} else {
	    /*
	     * We ignore apply/3 and anything else.
	     */
	    return 0;
	}
    }

    /*
     * Nothing to do, but the export entry matches.
     */

    return 1;
}

/*
 * Test if the given export entry is traced.
 *
 * Return Value: 1 if the export entry is traced, 0 if not, -1 for undefined.
 */
  
int
erts_trace_state(Export* ep)
{
    if (ep->address == ep->code+3) {
	if (ep->code[3] == (Uint) em_call_error_handler) {
	    return -1;
	} else if (ep->code[3] == (Uint) em_call_traced_function) {
	    return 1;
	} else if (ep->code[3] == (Uint) em_apply_bif) {
	    int i;
	    BifFunction func = (BifFunction) ep->code[4];

	    ASSERT(func != NULL);
	    for (i = 0; i < BIF_SIZE; i++) {
		if (func == bif_table[i].f) {
		    return 0;
		} else if (func == bif_table[i].traced) {
		    return 1;
		}
	    }
	    ASSERT(0);		/* We shouldn't get here. */
	    return 0;
	}
    }
    return 0;
}

/*
 * Return the current number of reductions for the given process.
 * To get the total number of reductions, p->reds must be added.
 */

Uint
erts_current_reductions(Process *current, Process *p)
{
    if (current != p) {
	return 0;
    } else if (current->fcalls < 0 && current->ct != NULL) {
	return -current->fcalls;
    } else {
	return REDS_IN(current) - current->fcalls;
    }
}
