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
#include "erl_binary.h"
#include "erl_bits.h"
#include "beam_bp.h"
#include "beam_catches.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif1.h"
#endif

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode: lb_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define OpCode(OpCode)    ((Uint*)op_##OpCode)
#  define Goto(Rel) {Go = (int)(Rel); goto emulator_loop;}
#  ifdef __GNUC__
#    define LabelAddr(Addr) &&Addr
#  else
#    define LabelAddr(Addr) &&##Addr
#  endif
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *(Rel)
#  ifdef __GNUC__
#    define LabelAddr(Addr) &&Addr
#  else
#    define LabelAddr(Addr) &&##Addr
#  endif
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

#ifdef UNIFIED_HEAP
  #define STACK_BEGIN c_p->stack
  #define STACK_END   c_p->send
  #define HEAP_START  global_heap
  #define HEAP_TOP    global_htop
  #define HEAP_END    global_hend
#else
  #define STACK_BEGIN c_p->hend
  #define STACK_END   c_p->htop
  #define HEAP_START  c_p->heap
  #define HEAP_TOP    c_p->htop
  #define HEAP_END    c_p->hend
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

#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#define GET_BIF_ADDRESS(p) ((BifFunction) (((Export *) p)->code[4]))

/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only activly used when
 * the process is switched out.
 */
#define REDS_IN(p)  ((p)->def_arg_reg[5])

/*
 * Add a byte offset to a pointer to Eterm.  This is useful when the
 * the loader has precalculated a byte offset.
 */
#define ADD_BYTE_OFFSET(ptr, offset) \
   ((Eterm *) (((unsigned char *)ptr) + (offset)))

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
   ASSERT(VALID_INSTR(* (Eterm *)(ip))); \
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
      xb(x_reg_offset(stb_reg)) = (Result); break; \
    default: \
      yb(y_reg_offset(stb_reg)) = (Result); break; \
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
    Eterm* stb_next; \
    Eterm stb_reg; \
    stb_reg = Arg(Dst); \
    I += (Dst) + 2; \
    stb_next = (Eterm *) *I; \
    CHECK_TERM(Result); \
    switch (beam_reg_tag(stb_reg)) { \
    case R_REG_DEF: \
      r(0) = (Result); Goto(stb_next); \
    case X_REG_DEF: \
      xb(x_reg_offset(stb_reg)) = (Result); Goto(stb_next); \
    default: \
      yb(y_reg_offset(stb_reg)) = (Result); Goto(stb_next); \
    } \
  } while (0)

#define ClauseFail() goto lb_jump_f

#define Badmatch(Term) { \
    c_p->fvalue = (Term); \
    goto badmatch; \
}

#define SAVE_CP(X)		*(X) = make_cp(c_p->cp)
#define RESTORE_CP(X)		SET_CP(c_p, cp_val(*(X)))

#define ISCATCHEND(instr) ((Eterm *) *(instr) == OpCode(catch_end_y))

/*
 * Special Beam instructions.
 */

Eterm beam_apply[2];
Eterm beam_exit[1];
Eterm beam_debug_apply[12];
int beam_debug_apply_size = sizeof(beam_debug_apply)/sizeof(beam_debug_apply[0]);

Eterm* em_call_error_handler;
Eterm* em_apply_bif;
Eterm* em_call_traced_function;

/*
 * All Beam instructions in numerical order.
 */

#ifndef NO_JUMP_TABLE
void** beam_ops;
#endif

extern int count_instructions;

#define SWAPIN \
    HTOP = HEAP_TOP; \
    E = c_p->stop

#define SWAPOUT \
    HEAP_TOP = HTOP; \
    c_p->stop = E

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(Eterm *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(Eterm *) (((unsigned char *)E) + (N)))
#define fb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

#ifdef UNIFIED_HEAP

#ifdef DEBUG
#define DEBUG_MEMSET sys_memset(c_p->send, 0xff, c_p->stack_sz*sizeof(Eterm))
#else
#define DEBUG_MEMSET
#endif

#define AllocateStack(StackNeed)                                         \
  do {                                                                   \
    ASSERT(c_p->send <= E && E <= c_p->stack);                           \
    if (E - (StackNeed) < c_p->send) {                                   \
      int used_stack = c_p->stack - E;                                   \
      int new_sz = next_heap_size(c_p->stack_sz + (StackNeed), 0);       \
      Eterm *new_stack =                                                 \
        (Eterm*)safe_alloc_from(901, sizeof(Eterm) * new_sz);            \
      sys_memmove((new_stack + new_sz) - used_stack, E,                  \
                  used_stack * sizeof(Eterm));                           \
      DEBUG_MEMSET;                                                      \
      sys_free((void *)c_p->send);                                       \
      c_p->stack_sz = new_sz;                                            \
      c_p->send = new_stack;                                             \
      c_p->stop = new_stack + new_sz - used_stack;                       \
      c_p->stack = new_stack + new_sz;                                   \
      E = c_p->stop;                                                     \
      r(0) = reg[0];                                                     \
    }                                                                    \
  } while(0)

/*
 * Makes sure that there are StackNeed and HeapNeed + 1 words available
 * on the stack and heap respectively, then allocates StackNeed + 1
 * words on the stack and saves CP.
 *
 * M is number of live registers to preserve during garbage collection
 */

#define AH(StackNeed, HeapNeed, M)                                       \
  do {                                                                   \
     int needed;                                                         \
     int new_sz = 0;                                                     \
     int used_stack = c_p->stack - E;                                    \
     Eterm *new_stack = NULL;                                            \
     ASSERT(c_p->send <= E && E <= c_p->stack);                          \
     ASSERT(HEAP_START <= HTOP && HTOP <= HEAP_END);                     \
     needed = (StackNeed) + CP_SIZE;                                     \
     if(E - needed < c_p->send) {                                        \
       new_sz = next_heap_size(c_p->stack_sz + needed, 0);               \
       new_stack = (Eterm *) safe_alloc_from(901, sizeof(Eterm)*new_sz); \
       sys_memmove((new_stack + new_sz) - used_stack,                    \
                   E,                                                    \
                   used_stack * sizeof(Eterm));                          \
       DEBUG_MEMSET;                                                     \
       sys_free((void *)c_p->send);                                      \
       c_p->stack_sz = new_sz;                                           \
       c_p->send = new_stack;                                            \
       c_p->stop = new_stack + new_sz - used_stack;                      \
       c_p->stack = new_stack + new_sz;                                  \
       E = c_p->stop;                                                    \
     }                                                                   \
     if (HTOP + (HeapNeed) > HEAP_END) {                                 \
           SWAPOUT;                                                      \
           reg[0] = r(0);                                                \
           FCALLS -= erts_garbage_collect(c_p, (HeapNeed), reg, (M));    \
           r(0) = reg[0];                                                \
           SWAPIN;                                                       \
     }                                                                   \
     E -= needed;                                                        \
     SAVE_CP(E);                                                         \
     ASSERT(c_p->send <= E && E <= c_p->stack);                          \
     ASSERT(HEAP_START <= HTOP && HTOP <= HEAP_END);                     \
  } while (0)
#else
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
     if (E - HTOP < (needed + (HeapNeed))) { \
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
#endif


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
     ASSERT(STACK_END <= E && E <= STACK_BEGIN); \

/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 */

#ifdef UNIFIED_HEAP
#define TestHeap(Nh, Live) \
  do { \
    unsigned need = (Nh); \
    ASSERT(HEAP_START <= HTOP && HTOP <= HEAP_END); \
    if (HEAP_END < (HTOP + need)) { \
       SWAPOUT; \
       reg[0] = r(0); \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live)); \
       r(0) = reg[0]; \
       SWAPIN; \
    } \
  } while (0)
#else
#define TestHeap(Nh, Live) \
  do { \
    unsigned need = (Nh); \
    ASSERT(c_p->htop <= E && E <= c_p->hend); \
    if (E - HTOP < need) { \
       SWAPOUT; \
       reg[0] = r(0); \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live)); \
       r(0) = reg[0]; \
       SWAPIN; \
    } \
  } while (0)
#endif

#define Init(N) make_blank(yb(N))

#define Init2(Y1, Y2) do { make_blank(Y1); make_blank(Y2); } while (0)
#define Init3(Y1, Y2, Y3) \
   do { make_blank(Y1); make_blank(Y2); make_blank(Y3); } while (0)

#define MakeFun(FunP, NumFree) \
  do { \
     SWAPOUT; \
     reg[0] = r(0); \
     r(0) = new_fun(c_p, reg, (ErlFunEntry *) FunP, NumFree); \
     HTOP = HEAP_TOP; \
  } while (0)


/*
 * Check that we haven't used the reductions and jump to function pointed to by
 * the I register.  If we are out of reductions, do a context switch.
 */

#define DispatchMacro()				\
  do {						\
     Eterm* dis_next;				\
     dis_next = (Eterm *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || (c_p->ct != NULL && FCALLS > -o_reds)) { \
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch;			\
     }						\
 } while (0)

#define DispatchMacroFun()			\
  do {						\
     Eterm* dis_next;				\
     dis_next = (Eterm *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || (c_p->ct != NULL && FCALLS > -o_reds)) { \
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch_fun;		\
     }						\
 } while (0)

#define DispatchMacrox()				\
  do {							\
     if (FCALLS > 0) {					\
        Eterm* dis_next;				\
        SET_I(((Export *) Arg(0))->address);		\
        dis_next = (Eterm *) *I;			\
        FCALLS--;					\
        CHECK_ARGS(I);					\
        Goto(dis_next);					\
     } else if (c_p->ct != NULL && FCALLS > -o_reds) {	\
        goto save_calls1;				\
     } else {						\
        SET_I(((Export *) Arg(0))->address);		\
        CHECK_ARGS(I);					\
	goto context_switch;				\
     }							\
 } while (0)

#ifdef DEBUG
/*
 * To simplify breakpoint setting, put the code in one place only and jump to it.
 */
#  define Dispatch() goto do_dispatch
#  define Dispatchx() goto do_dispatchx
#  define Dispatchfun() goto do_dispatchfun
#else
/*
 * Inline for speed.
 */
#  define Dispatch() DispatchMacro()
#  define Dispatchx() DispatchMacrox()
#  define Dispatchfun() DispatchMacroFun()
#endif

#define Self(R) R = c_p->id
#define Node(R) R = this_node

#define Arg(N)       I[(N)+1]
#define Next(N) \
    I += (N) + 1; \
    ASSERT(VALID_INSTR(*I)); \
    Goto(*I)

#define PreFetch(N, Dst) do { Dst = (Eterm *) *(I + N + 1); } while (0)
#define NextPF(N, Dst) \
    I += N + 1; \
    ASSERT(VALID_INSTR(Dst)); \
    Goto(Dst)

#define GetR(pos, tr) \
   do { \
     tr = Arg(pos); \
     switch (beam_reg_tag(tr)) { \
     case R_REG_DEF: tr = r(0); break; \
     case X_REG_DEF: tr = xb(x_reg_offset(tr)); break; \
     case Y_REG_DEF: ASSERT(y_reg_offset(tr) >= CP_SIZE); tr = yb(y_reg_offset(tr)); break; \
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
    HTOP[0] = HEADER_FLONUM; \
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
    SET_I(cp_val(*E)); \
    E = ADD_BYTE_OFFSET(E, words_to_pop); \
    ASSERT(STACK_END <= E && E <= STACK_BEGIN); \
    CHECK_TERM(r(0)); \
    Goto(*I); \
  } while (0)

#define MoveDeallocateReturn(Src, Dest, Deallocate) \
    (Dest) = (Src); \
    DeallocateReturn(Deallocate)

#define MoveCall(Src, Dest, CallDest, Size)	\
    (Dest) = (Src);				\
    SET_CP(c_p, I+Size+1);			\
    SET_I((Eterm *) CallDest);			\
    Dispatch();

#define MoveCallLast(Src, Dest, CallDest, Deallocate)	\
    (Dest) = (Src);					\
    RESTORE_CP(E);					\
    E = ADD_BYTE_OFFSET(E, (Deallocate));		\
    ASSERT(STACK_END <= E && E <= STACK_BEGIN);		\
    SET_I((Eterm *) CallDest);				\
    Dispatch();

#define GetList(Src, H, T) do {			\
   Eterm* tmp_ptr = list_val(Src);		\
   H = CAR(tmp_ptr);				\
   T = CDR(tmp_ptr); } while (0)

#define GetTupleElement(Src, Element, Dest)					\
  do {										\
    tmp_arg1 = (Eterm) (((unsigned char *) tuple_val(Src)) + (Element));	\
    (Dest) = (*(Eterm *)tmp_arg1);						\
  } while (0)

#define ExtractNextElement(Dest)				\
    tmp_arg1 += sizeof(Eterm);					\
    (Dest) = (* (Eterm *) (((unsigned char *) tmp_arg1)))

#define ExtractNextElement2(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1];	\
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2];	\
    tmp_arg1 += sizeof(Eterm) + sizeof(Eterm);	\
  } while (0)

#define ExtractNextElement3(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1];	\
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2];	\
    ene_dstp[2] = ((Eterm *) tmp_arg1)[3];	\
    tmp_arg1 += 3*sizeof(Eterm);		\
  } while (0)

#define ExtractNextElement4(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) tmp_arg1)[1];	\
    ene_dstp[1] = ((Eterm *) tmp_arg1)[2];	\
    ene_dstp[2] = ((Eterm *) tmp_arg1)[3];	\
    ene_dstp[3] = ((Eterm *) tmp_arg1)[4];	\
    tmp_arg1 += 4*sizeof(Eterm);		\
  } while (0)

#define ExtractElement(Element, Dest)		\
  do {						\
     tmp_arg1 += (Element);			\
     (Dest) = (* (Eterm *) tmp_arg1);		\
  } while (0)

#define PutTuple(Arity, Src, Dest)		\
     ASSERT(is_arity_value(Arity));		\
     Dest = make_tuple(HTOP);			\
     HTOP[0] = (Arity);				\
     HTOP[1] = (Src);				\
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
    if (*(Eterm *)(tmp_arg1 = (Eterm)tuple_val(Pointer)) != (Arity)) { Fail; }

#define IsFunction(X, Action) \
  do { \
     if ( !(is_fun(X)) ) { \
          Action; \
     } \
  } while (0)

#define IsTupleOfArity(Src, Arity, Fail) \
  do { \
    if (is_not_tuple(Src) || *(Eterm *)(tmp_arg1 = (Eterm) tuple_val(Src)) != Arity) { \
        Fail; \
    } \
  } while (0)

#define IsBinary(Src, Fail) \
 if (is_not_binary(Src)) { Fail; }

#define BsStartMatch(Src, Fail) erts_InitMatchBuf(Src, Fail)

#define BsGetInteger8(Dst, Store, Fail)				\
 do {								\
    Eterm _result;						\
    if (erts_mb.size - erts_mb.offset < 8) { Fail; }		\
    _result = make_small(erts_mb.base[erts_mb.offset/8]);	\
    erts_mb.offset += 8;					\
    Store(_result, Dst);					\
 } while (0)

#define BsGetInteger16(Dst, Store, Fail)				\
 do {									\
    Eterm _result;							\
    if (erts_mb.size - erts_mb.offset < 16) { Fail; }			\
    _result = make_small(get_int16(erts_mb.base + erts_mb.offset/8));	\
    erts_mb.offset += 16;						\
    Store(_result, Dst);						\
 } while (0)

#define BsGetInteger32(Dst, Store, Fail)			\
 do {								\
    Uint32 _integer;						\
    Eterm _result;						\
    if (erts_mb.size - erts_mb.offset < 32) { Fail; }	\
    _integer = get_int32(erts_mb.base + erts_mb.offset/8);	\
    erts_mb.offset += 32;					\
    if (IS_USMALL(0, _integer)) {				\
	_result = make_small(_integer);				\
    } else {							\
	Eterm* _hp = ArithAlloc(c_p, BIG_NEED_SIZE(2));		\
	_result = uint_to_big((Uint) _integer, _hp);			\
    }								\
    Store(_result, Dst);					\
 } while (0)

#define BsGetIntegerImm(Sz, Flags, Dst, Store, Fail)	\
 do {							\
    Eterm _result;					\
    SWAPOUT;						\
    _result = erts_bs_get_integer(c_p, (Sz), (Flags));	\
    HTOP = HEAP_TOP;					\
    if (is_non_value(_result)) { Fail; }		\
    else { Store(_result, Dst); }			\
 } while (0)

#define BsGetInteger(Sz, Flags, Dst, Store, Fail)			\
 do {									\
    Eterm _result; int _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_integer(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP;							\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetFloat(Sz, Flags, Dst, Store, Fail)				\
 do {									\
    Eterm _result; int _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_float(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP;							\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetBinaryImm(Sz, Flags, Dst, Store, Fail)	\
 do {							\
    Eterm _result;					\
    SWAPOUT;						\
    _result = erts_bs_get_binary(c_p, (Sz), (Flags));	\
    HTOP = HEAP_TOP;					\
    if (is_non_value(_result)) { Fail; }		\
    else { Store(_result, Dst); }			\
 } while (0)

#define BsGetBinary(Sz, Flags, Dst, Store, Fail)			\
 do {									\
    Eterm _result; int _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_binary(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP;							\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetBinaryAll(Dst, Store, Fail)	\
 do {						\
    Eterm _result;				\
    SWAPOUT;					\
    _result = erts_bs_get_binary_all(c_p);	\
    HTOP = HEAP_TOP;				\
    if (is_non_value(_result)) { Fail; }	\
    else { Store(_result, Dst); }		\
 } while (0)

#define BsSkipBits(Bits, Unit, Fail)					\
 do {									\
    size_t new_offset; int _size;					\
    if (!is_small(Bits) || (_size = signed_val(Bits)) < 0) { Fail; }	\
    new_offset = erts_mb.offset + _size * (Unit);			\
    if (new_offset <= erts_mb.size) { erts_mb.offset = new_offset; }	\
    else { Fail; }							\
 } while (0)

#define BsSkipBitsAll(Fail)			\
 do {						\
    if (erts_mb.offset % 8 != 0) { Fail; }	\
    erts_mb.offset = erts_mb.size;		\
 } while (0)

#define BsSkipBitsAllAligned()			\
 do {						\
    erts_mb.offset = erts_mb.size;		\
 } while (0)

#define BsSkipBitsImm(Bits, Fail)					\
 do {									\
    size_t new_offset = erts_mb.offset + (Bits);			\
    if (new_offset <= erts_mb.size) { erts_mb.offset = new_offset; }	\
    else { Fail; }							\
 } while (0)


#define BsPutIntegerImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_bs_put_integer((Src), (Sz), (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutInteger(Sz, Flags, Src)					\
 do {									\
    int _size;							\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_integer((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutFloatImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_bs_put_float((Src), (Sz), (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutFloat(Sz, Flags, Src)					\
 do {									\
    int _size;							\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_float((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutBinary(Sz, Flags, Src)					\
 do {									\
    int _size;							\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_binary((Src), _size)) { goto badarg; }		\
 } while (0)

#define BsPutBinaryImm(Sz, Src)				\
 do {							\
    if (!erts_bs_put_binary((Src), (Sz))) { goto badarg; }	\
 } while (0)

#define BsPutBinaryAll(Src)				\
 do {							\
    if (!erts_bs_put_binary_all((Src))) { goto badarg; }	\
 } while (0)


#define IsPort(Src, Fail) if (is_not_port(Src)) { Fail; }
#define IsPid(Src, Fail) if (is_not_pid(Src)) { Fail; }
#define IsRef(Src, Fail) if (is_not_ref(Src)) { Fail; }

static Eterm* handle_error(Process* c_p, Eterm* pc, Eterm* reg, BifFunction bf);
static Eterm call_error_handler(Process* p, Eterm* ip, Eterm* reg);
static Eterm call_breakpoint_handler(Process* p, Eterm* fi, Eterm* reg);
static Eterm* apply(Process* p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static Eterm* call_fun(Process* p, int arity, Eterm* reg, Eterm args);
static Eterm* apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg);
static Eterm new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free);


void
init_emulator(void)
{
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
    register Eterm x0 REG_x0;

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register Eterm* reg REG_xregs;

    /*
     * Top of heap (next free location); grows upwards.
     */
    register Eterm* HTOP REG_htop;

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    register Eterm* E REG_stop;

    /*
     * Pointer to next threaded instruction.
     */
    register Eterm *I REG_I;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register sint32 FCALLS REG_fcalls;

    /*
     * Temporaries used for picking up arguments for instructions.
     */
    register Eterm tmp_arg1 REG_tmp_arg1;
    register Eterm tmp_arg2 REG_tmp_arg2;
    Eterm tmp_big[2];		/* Temporary buffer for small bignums. */

    static Eterm save_reg[MAX_REG];	
    /* X registers -- not used directly, but
     * through 'reg', because using it directly
     * needs two instructions on a SPARC,
     * while using it through reg needs only
     * one.
     */

    /*
     * Floating point registers.
     */
    static FloatDef freg[MAX_REG];

    /*
     * For keeping the old value of 'reds' when call saving is active.
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
	Eterm* argp = c_p->arg_reg;
	Eterm* next;
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

	next = (Eterm *) *I;
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
     if (is_value(result)) {
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
     if (is_value(result)) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 OpCase(i_is_lt_f):
    if (CMP_GE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ge_f):
    if (CMP_LT(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_f):
    if (CMP_NE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ne_f):
    if (CMP_EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_exact_f):
    if (!EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_call_only_f): {
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_call_last_fP): {
     RESTORE_CP(E);
     E = ADD_BYTE_OFFSET(E, Arg(1));
     ASSERT(STACK_END <= E && E <= STACK_BEGIN);
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_call_f): {
     SET_CP(c_p, I+2);
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_call_ext_last_eP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));
    ASSERT(STACK_END <= E && E <= STACK_BEGIN);

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
     Eterm* next;

     PreFetch(1, next);
     make_blank(yb(Arg(0)));
     NextPF(1, next);
 }

 OpCase(return):
    SET_I(c_p->cp);
    CHECK_TERM(r(0));
    Goto(*I);

 OpCase(test_heap_1_put_list_Iy): {
     Eterm* next;

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

     SWAPOUT;
     c_p->fcalls = FCALLS - 1;
     result = send_2(c_p, r(0), x(1));
     PreFetch(0, next);
     FCALLS = c_p->fcalls;
     HTOP = HEAP_TOP;
     if (is_value(result)) {
	 r(0) = result;
	 CHECK_TERM(r(0));
	 NextPF(0, next);
     } else if (c_p->freason == RESCHEDULE) {
	 Eterm* argp;

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

 OpCase(i_element_jssd): {
     Eterm index;
     Eterm tuple;

     /*
      * Inlined version of element/2 for speed.
      */
     GetArg2(1, index, tuple);
     if (is_small(index) && is_tuple(tuple)) {
	 Eterm* tp = tuple_val(tuple);

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
	 Eterm* tp = tuple_val(tuple);
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
    yb(Arg(0)) = Arg(1);
    Next(2);

 OpCase(catch_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 if (x(1) == am_THROW) {
	     r(0) = x(2);
	 } else {
	     Eterm* hp = ArithAlloc(c_p, 3);
	     r(0) = TUPLE2(hp, am_EXIT, x(2));
	 }
     }
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
     Eterm* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((Eterm *) Arg(0));
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
     Eterm* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((Eterm *) Arg(0));
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
     Eterm* next;
     ErlMessage* msgp;

     PreFetch(0, next);
     msgp = PEEK_MESSAGE(c_p);
     if (c_p->ct != NULL) {
	 save_calls(c_p, &exp_receive);
     }
     if (msgp->seq_trace_token == NIL) {
	 SEQ_TRACE_TOKEN(c_p) = msgp->seq_trace_token;
     } else if (msgp->seq_trace_token != am_undefined) {
	 Eterm msg;
	 SEQ_TRACE_TOKEN(c_p) = msgp->seq_trace_token;
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
	 seq_trace_output(SEQ_TRACE_TOKEN(c_p), msg, SEQ_TRACE_RECEIVE, 
			  c_p->id, c_p);
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
 OpCase(loop_rec_end_f): {
     SET_I((Eterm *) Arg(0));
     SAVE_MESSAGE(c_p);
     Goto(*I);		/* To loop_rec */
 }
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
	 Uint time_val;

	 if (is_small(timeout_value) && signed_val(timeout_value) > 0) {
	     /*
	      * The timer routiner will set c_p->i to the value in
	      * c_p->def_arg_reg[0].  Note that it is safe to use this
	      * location because there are no living x registers in
	      * a receive statement.
	      */
	     c_p->def_arg_reg[0] = (Eterm) (I+3);
	     set_timer(c_p, unsigned_val(timeout_value));
	 } else if (timeout_value == am_infinity) {
	     c_p->flags |= F_TIMO;
	 } else if (term_to_Uint(timeout_value, &time_val)) {
	     c_p->def_arg_reg[0] = (Eterm) (I+3);
	     set_timer(c_p, time_val);
	 } else {		/* Wrong time */
	     OpCase(i_wait_error):
	     c_p->freason = EXC_TIMEOUT_VALUE;
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

	 wait2: {
	     c_p->i = (Eterm *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;
	     c_p->status = P_WAITING;
	     c_p->current = NULL;
	     return REDS_IN(c_p) - FCALLS;
	 }
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
	 c_p->def_arg_reg[0] = (Eterm) (I+3);
	 set_timer(c_p, Arg(1));
     }
     goto wait2;
 }

    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
 OpCase(timeout): {
     Eterm* next;

     PreFetch(0, next);
     if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
	 trace_receive(c_p, am_timeout);
     }
     if (c_p->ct != NULL)
	save_calls(c_p, &exp_timeout);
     c_p->flags &= ~F_TIMO;
     JOIN_MESSAGE(c_p);
     NextPF(0, next);
 }

 OpCase(i_select_val_sfI):
     GetArg1(0, tmp_arg1);

 do_binary_search:
 {
     struct Pairs {
	 int val;
	 Eterm* addr;
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
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_zero_sfI):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = signed_val(index);
	 if (index < Arg(2)) {
	     SET_I((Eterm *) (&Arg(3))[index]);
	     Goto(*I);
	 }
     }
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }

 OpCase(i_jump_on_val_sfII):
 {
     Eterm index;

     GetArg1(0, index);
     if (is_small(index)) {
	 index = (unsigned) (signed_val(index) - Arg(3));
	 if (index < Arg(2)) {
	     SET_I((Eterm *) (&Arg(4))[index]);
	     Goto(*I);
	 }
     }
     SET_I((Eterm *) Arg(1));
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
	if (is_value(result)) {
	    StoreBifResult(3, result);
	}
	SET_I((Eterm *) Arg(0));
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
	if (is_value(result)) {
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
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	SET_I((Eterm *) Arg(0));
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
	if (is_value(result)) {
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
	r(0) = (*bf)(c_p, I);
	FCALLS = c_p->fcalls;
	HTOP = HEAP_TOP;
	CHECK_TERM(r(0));
	Next(1);
    }

 OpCase(call_bif1_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	Eterm* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	result = (*bf)(c_p, r(0), I);
	FCALLS = c_p->fcalls;
	HTOP = HEAP_TOP;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 1;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    goto call_bif_trap3;
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif2_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	Eterm* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	CHECK_TERM(r(0));
	CHECK_TERM(x(1));
	result = (*bf)(c_p, r(0), x(1), I);
	FCALLS = c_p->fcalls;
	HTOP = HEAP_TOP;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 2;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	    goto call_bif_trap3;
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	reg[0] = r(0);
	c_p->cp = NULL;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(call_bif3_e):
    {
	BifFunction bf = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	Eterm* next;

	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	if (FCALLS <= 0) {
	   save_calls(c_p, (Export *) Arg(0));
	}
	PreFetch(1, next);
	result = (*bf)(c_p, r(0), x(1), x(2), I);
	FCALLS = c_p->fcalls;
	HTOP = HEAP_TOP;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == RESCHEDULE) {
	    c_p->arity = 3;
	    goto suspend_bif;
	} else if (c_p->freason == TRAP) {
	call_bif_trap3:
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
	ASSERT(c_p->stop == E);
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
     if (is_value(result)) {
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
	     SET_I((Eterm *) Arg(0));
	     Goto(*I);
	 }
     }
     ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
     goto find_func_info;
 }

 OpCase(i_m_div_jd):
 {
     Eterm result;

     result = erts_mixed_div(c_p, tmp_arg1, tmp_arg2);
     if (is_value(result)) {
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
     if (is_value(result)) {
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
     if (is_value(result)) {
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
     if (is_value(result)) {
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
     if (is_value(result)) {
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
     if (is_value(result)) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 {
     int i;
     int ires;
     Eterm* bigp;

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
		     if (i >= SMALL_BITS-1) {
			 tmp_arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		     } else {
			 tmp_arg1 = make_small(ires >> i);
		     }
		     StoreBifResult(1, tmp_arg1);
		 } else if (i < SMALL_BITS-1) { /* Left shift */
		     if ((ires > 0 && ((-1 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			 ((-1 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			 tmp_arg1 = make_small(ires << i);
			 StoreBifResult(1, tmp_arg1);
		     }
		 }
		 tmp_arg1 = small_to_big(ires, tmp_big);

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
     Eterm* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_last_P): {
     Eterm* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (Eterm *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_only): {
     Eterm* next;

     if ((next = apply(c_p, r(0), x(1), x(2), reg)) != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatch();
     }
     SWAPOUT;
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     Eterm* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = HEAP_TOP;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_last_P): {
     Eterm* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = HEAP_TOP;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (Eterm *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_only): {
     Eterm* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = HEAP_TOP;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_I): {
     Eterm* next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     HTOP = HEAP_TOP;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_last_IP): {
     Eterm* next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     HTOP = HEAP_TOP;
     if (next != NULL) {
	r(0) = reg[0];
	SET_CP(c_p, (Eterm *) E[0]);
	E = ADD_BYTE_OFFSET(E, Arg(1));
	SET_I(next);
	Dispatchfun();
     }
     goto find_func_info;
 }

#ifdef DEBUG
    /*
     * Set a breakpoint here to get control just after a call instruction.
     * I points to the first instruction in the called function.
     *
     * In gdb, use 'call dis(I-5, 1)' to show the name of the function.
     */
 do_dispatch:
     DispatchMacro();

 do_dispatchx:
     DispatchMacrox();

 do_dispatchfun:
     DispatchMacroFun();

#endif

    /*
     * Jumped to from the Dispatch() macro when the reductions are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch_fun:
    c_p->arity = I[-1] + 1;
    goto context_switch2;

 context_switch:
    c_p->arity = I[-1];

 context_switch2:		/* Entry for fun calls. */
    c_p->current = I-3;		/* Pointer to Mod, Func, Arity */

 {
     Eterm* argp;
     int reds_used;
     int i;

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
	     c_p->arg_reg = (Eterm *) safe_realloc((char *) c_p->arg_reg,
						    c_p->arity *
						    sizeof(c_p->arg_reg[0]));
	 } else {
	     c_p->arg_reg = (Eterm *)
		 safe_alloc_from(310, c_p->arity * sizeof(c_p->arg_reg[0]));
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
     Eterm* hp = ArithAlloc(c_p, 3);
     Eterm f = make_float(hp);

     hp[0] = HEADER_FLONUM;
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     StoreBifResult(2, f);
 }

 OpCase(i_fetch_float1_o):
 {
     Eterm* hp = ArithAlloc(c_p, 3);
     tmp_arg1 = make_float(hp);

     hp[0] = HEADER_FLONUM;
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     Next(2);
 }

 OpCase(i_fetch_float2_o):
 {
     Eterm* hp = ArithAlloc(c_p, 3);
     tmp_arg2 = make_float(hp);

     hp[0] = HEADER_FLONUM;
     hp[1] = Arg(0);
     hp[2] = Arg(1);
     Next(2);
 }

 OpCase(i_select_tuple_arity_sfI):
 {
     GetArg1(0, tmp_arg1);

     if (is_tuple(tmp_arg1)) {
	 tmp_arg1 = *tuple_val(tmp_arg1);
	 goto do_binary_search;
     }
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }     

 /*
  * Arg(0): N = Thing word (tag, sign, number of words)
  * Arg(1..N): Value
  * Arg(N+1): Destination register
  */
  
 OpCase(i_put_big_wd):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = ArithAlloc(c_p, size+1);
     Eterm big = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     StoreBifResult(size+1, big);
 }

 OpCase(i_fetch_big1_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = ArithAlloc(c_p, size+1);
     tmp_arg1 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     Next(size+1);
 }

 OpCase(i_fetch_big2_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = ArithAlloc(c_p, size+1);
     tmp_arg2 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     Next(size+1);
 }

 OpCase(i_select_big_sf):
    {
	Eterm* bigp;
	Uint arity;
	Eterm* given;
	Uint given_arity;
	Uint given_size;

	GetArg1(0, tmp_arg1);
	if (is_big(tmp_arg1)) {

	    /*
	     * The loader has sorted the bignumbers in descending order
	     * on the arity word.  Therefore, we know that the search
	     * has failed as soon as we encounter an arity word less than
	     * the arity word of the given number.  There is a zero word
	     * (less than any valid arity word) stored after the last bignumber.
	     */

 	    given = big_val(tmp_arg1);
	    given_arity = given[0];
	    given_size = thing_arityval(given_arity);
	    bigp = &Arg(2);
	    while ((arity = bigp[0]) >= given_arity) {
		if (arity == given_arity &&
		    memcmp(bigp+1, given+1, sizeof(Eterm)*given_size) == 0) {
		    SET_I((Eterm *) bigp[given_size+1]);
		    Goto(*I);
		}
		bigp += thing_arityval(arity) + 2;
	    }
	}

	/*
	 * Failed.
	 */

	SET_I((Eterm *) Arg(1));
	Goto(*I);
    }

 OpCase(i_select_float_sfI):
 {
     Uint fpart1;
     Uint fpart2;
     int n;
     struct ValLabel {
	 Uint fpart1;
	 Uint fpart2;
	 Eterm* addr;
     };
     struct ValLabel* ptr;

     GetArg1(0, tmp_arg1);
     ASSERT(is_float(tmp_arg1));
     fpart1 = float_val(tmp_arg1)[1];
     fpart2 = float_val(tmp_arg1)[2];

     n = Arg(2);
     ptr = (struct ValLabel *) &Arg(3);
     while (n-- > 0) {
	 if (ptr->fpart1 == fpart1 && ptr->fpart2 == fpart2) {
	     SET_I(ptr->addr);
	     Goto(*I);
	 }
	 ptr++;
     }
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }

 OpCase(set_tuple_element_sdP): {
     Eterm element;
     Eterm tuple;
     Eterm* next;
     Eterm* p;
     
     PreFetch(3, next);
     GetArg2(0, element, tuple);
     ASSERT(is_tuple(tuple));
     p = (Eterm *) ((unsigned char *) tuple_val(tuple) + Arg(2));
     *p = element;
     NextPF(3, next);
 }

 OpCase(int_bnot_jsd):
    GetArg1(1, tmp_arg1);
    if (is_small(tmp_arg1)) {
	tmp_arg1 = make_small(~signed_val(tmp_arg1));
    } else if (is_big(tmp_arg1)) {
	Eterm* bigp = ArithAlloc(c_p, BIG_NEED_SIZE(big_size(tmp_arg1)+1));
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
    if (EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0;		/* In case this process will ever be garbed again. */
     do_exit(c_p, am_normal);
     return REDS_IN(c_p) - FCALLS;
 }

    /*
     * Suspend BIF and prepare BIF to be rescheduled.
     */
 suspend_bif: {
     Eterm* argp = c_p->arg_reg;
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
    HTOP = HEAP_TOP;
    if (tmp_arg1) {
	SET_I(c_p->i);
	Dispatch();
    }

 /* Fall through */
 OpCase(error_action_code): {
 no_error_handler:
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, NULL, reg, NULL);
 post_error_handling:
     if (I == 0) {
	 return REDS_IN(c_p) - FCALLS;
     } else {
	 r(0) = reg[0];
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

	c_p->current = I-3;	/* In case we apply process_info/1,2. */
	c_p->i = I;		/* In case we apply check_process_code/2. */
	c_p->arity = 0;		/* To allow garbage collection on ourselves
				 * (check_process_code/2).
				 */
				   
	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	bf = (BifFunction) Arg(0);
	ASSERT(I[-1] <= 3);
	switch (I[-1]) {
	case 3:
	    tmp_arg1 = (*bf)(c_p, r(0), x(1), x(2), I);
	    break;
	case 2:
	    tmp_arg1 = (*bf)(c_p, r(0), x(1), I);
	    break;
	case 1:
	    tmp_arg1 = (*bf)(c_p, r(0), I);
	    break;
	case 0:
	    tmp_arg1 = (*bf)(c_p, I);
	    break;
	}
	FCALLS = c_p->fcalls;
	SWAPIN;			/* There might have been a garbage collection. */
	if (is_value(tmp_arg1)) {
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
     Eterm* next;
     ErlMessage* msgp = PEEK_MESSAGE(c_p);

     if (msgp == NULL) {
	 SET_I((Eterm *) Arg(0));
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
    c_p->freason = EXC_CASE_CLAUSE;
    goto find_func_info;

 OpCase(if_end):
    c_p->freason = EXC_IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_IaaI): {
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = I + 2;
     goto lb_error_action_code;
 }

 /*
  * Construction of binaries.
  */

 OpCase(i_bs_init): {
     Eterm *next;
     PreFetch(0, next);
     erts_bin_offset = 0;
     NextPF(0, next);
 }

 OpCase(i_bs_final_jd): {
     Eterm *next;
     Eterm b;

     if (erts_bin_offset % 8 != 0) {
	 goto badarg;
     }
     PreFetch(2, next);
     b = new_binary_arith(c_p, erts_bin_buf, erts_bin_offset / 8);
     StoreResult(b, Arg(1));
     NextPF(2, next);
 }

 OpCase(i_bs_final_heap_d): {
     Eterm *next;
     ErlHeapBin* hb;
     Eterm b;
     unsigned len;

     PreFetch(1, next);
     len = erts_bin_offset / 8;
     hb = (ErlHeapBin *) ArithAlloc(c_p, heap_bin_size(len));
     hb->thing_word = header_heap_bin(len);
     hb->size = len;
     sys_memcpy(hb->data, erts_bin_buf, len);
     b = make_binary(hb);
     StoreResult(b, Arg(0));
     NextPF(1, next);
 }

 OpCase(bs_put_string_II):
    {
	erts_bs_put_string((byte *) Arg(1), Arg(0));
	Next(2);
    }

 /*
  * Matching of binaries.
  */

 OpCase(bs_test_zero_tail_f): {
     Eterm* next;

     PreFetch(1, next);
     if (erts_mb.size != erts_mb.offset) {
	 ClauseFail();
     }
     NextPF(1, next);
 }

 OpCase(bs_test_tail_imm_fI): {
     Eterm* next;

     PreFetch(2, next);
     if (erts_mb.size - erts_mb.offset != Arg(1)) {
	 ClauseFail();
     }
     NextPF(2, next);
 }

 OpCase(bs_save_I): {
     Eterm* next;

     PreFetch(1, next);
     erts_save_mb[Arg(0)] = erts_mb;
     NextPF(1, next);
 }

 OpCase(bs_restore_I): {
     Eterm* next;

     PreFetch(1, next);
     erts_mb = erts_save_mb[Arg(0)];
     NextPF(1, next);
 }

#include "beam_cold.h"

 OpCase(is_eq_exact_body): {
     Eterm* next;

     PreFetch(0, next);
     if (EQ(tmp_arg1, tmp_arg2)) {
	 NextPF(0, next);
     }
     Badmatch(tmp_arg1);
 }

 /*
  * This instruction is probably never used (because it is combined with a
  * a return). However, a future compiler might for some reason emit a
  * deallocate not followed by a return, and that should work.
  */
 OpCase(deallocate_I): {
     Eterm* next;

     PreFetch(1, next);
     D(Arg(0));
     NextPF(1, next);
 }

    /*
     * Trace and debugging support.
     */

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
	 flags = erts_call_trace(c_p, ep->code, ep->match_prog_set, reg, 0);
	 SWAPIN;
	 
	 if (flags & MATCH_SET_RETURN_TRACE) {
	     static void* return_trace[1] = {OpCode(return_trace)};

#ifdef UNIFIED_HEAP
             AllocateStack(2);
             E -= 2;
             ASSERT(c_p->send <= E && E <= c_p->stack);
#else
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 2 < HTOP) {
		 /* SWAPOUT, SWAPIN was done and r(0) was saved above */
		 FCALLS -= erts_garbage_collect(c_p, 2, reg, ep->code[2]);
		 r(0) = reg[0];
		 SWAPIN;
	     }
	     E -= 2;
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
#endif
	     ASSERT(is_CP((Eterm)(ep->code)));
	     E[1] = make_cp(c_p->cp);
	     E[0] = make_cp(ep->code);
	     c_p->cp = (Eterm *) make_cp((Uint*)return_trace);
	 }
     }
     SET_I((Uint *) Arg(0));
     Dispatch();
 }

 OpCase(return_trace): {
     Uint* code = (Uint *) E[0];

     if (IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 erts_trace_return(c_p, code, r(0));
     }
     SET_I((Eterm *) E[1]);
     E += 2;
     Goto(*I);
 }

 OpCase(i_trace_breakpoint): {
     Uint real_I;
     Uint32 flags;
     if (IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 Uint *cpp;
	 flags = 0;
	 SWAPOUT;
	 reg[0] = r(0);

	 if (*cp_val((Eterm)c_p->cp) 
	     == (Uint) OpCode(return_trace)) {
	     cpp = &((Uint) E[1]);
	 } else if (*cp_val((Eterm)c_p->cp) 
		    == (Uint) OpCode(i_return_to_trace)) {
	     cpp = &((Uint) E[0]);
	 } else {
	     cpp = NULL;
	 }
	 if (cpp) {
	     Eterm *cp_save = c_p->cp;
	     for (;;) {
		 ASSERT(is_CP(*cpp));
		 if (*cp_val(*cpp) == (Uint) OpCode(return_trace)) {
		     cpp += 2;
		 } else if (*cp_val(*cpp) == (Uint) OpCode(i_return_to_trace)) {
		     cpp += 1;
		 } else
		     break;
	     }
	     c_p->cp = (Eterm *) *cpp;
	     ASSERT(is_CP((Eterm)c_p->cp));
	     real_I = erts_process_break(c_p, I, I - 3, reg, &flags);
	     c_p->cp = cp_save;
	 } else {
	     real_I = erts_process_break(c_p, I, I - 3, reg, &flags);
	 }

	 if ((flags & MATCH_SET_RETURN_TO_TRACE)) {
	     static void* return_to_trace[1] = {OpCode(i_return_to_trace)};
	     if ((Uint) c_p->cp != make_cp((Uint*)return_to_trace)) {
		 /* Look down the stack for other return_to frames */
		 int do_insert = 1;
		 if (*cp_val((Eterm)c_p->cp) == (Uint) OpCode(return_trace)) {
		     cpp = &((Uint) E[1]);
		     for(;;) {
			 ASSERT(is_CP(*cpp));
			 if (*cp_val(*cpp) == 
			     (Uint) OpCode(return_trace)) {
			     cpp += 2;
			 } else {
			     break;
			 }
		     }
		     if (*cp_val(*cpp) == 
			 (Uint) OpCode(i_return_to_trace)) {
			 do_insert = 0;
		     }
		 } 
		 if (do_insert) {
#ifdef UNIFIED_HEAP
                     AllocateStack(1);
                     E -= 1;
                     ASSERT(c_p->send <= E && E <= c_p->stack);
#else
		     ASSERT(c_p->htop <= E && E <= c_p->hend);
		     if (E - 1 < HTOP) {
			 /* SWAPOUT was done and r(0) was saved above */
			 FCALLS -= erts_garbage_collect(c_p, 1, reg, I[-1]);
			 r(0) = reg[0];
			 SWAPIN;
		     }
		     E -= 1;
		     ASSERT(c_p->htop <= E && E <= c_p->hend);
#endif
		     E[0] = make_cp(c_p->cp);
		     c_p->cp = (Eterm *) make_cp((Uint*)return_to_trace);
		 }
	     }  
	 }
	 if (flags & MATCH_SET_RETURN_TRACE) {
	     static void* return_trace[1] = {OpCode(return_trace)};

#ifdef UNIFIED_HEAP
             SWAPOUT;
             AllocateStack(2);
             E -= 2;
             ASSERT(c_p->send <= E && E <= c_p->stack);
#else
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 2 < HTOP) {
		 /* Stack pointer may have been changed by 
		    return_to trace above */
		 SWAPOUT; 
		 FCALLS -= erts_garbage_collect(c_p, 2, reg, I[-1]);
		 r(0) = reg[0];
		 SWAPIN;
	     }
	     E -= 2;
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
#endif
	     ASSERT(is_CP((Eterm) (I - 3)));
	     E[1] = make_cp(c_p->cp);
	     E[0] = make_cp(I - 3); /* We ARE at the beginning of an 
				       instruction,
				       the funcinfo is above i. */
	     c_p->cp = (Eterm *) make_cp((Uint*)return_trace);
	 }
     } else {
	 TraceBpLookupInstr(I,real_I);
     }
     Goto(real_I);
 }
 
 OpCase(i_return_to_trace): {
     Uint *cpp = &((Uint) E[0]);
     for(;;) {
	 ASSERT(is_CP(*cpp));
	 if (*cp_val(*cpp) == (Uint) OpCode(return_trace)) {
	     cpp += 2;
	 } else {
	     break;
	 }
     }
     if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
	 erts_trace_return_to(c_p, cp_val(*cpp));
     }
     SET_I((Eterm *) E[0]);
     E += 1;
     Goto(*I);
 }

 /*
  * Instructions for module_info/0,1.
  */

 OpCase(i_module_info_0): {
     SWAPOUT;
     r(0) = erts_module_info_0(c_p, I[-3]);
     HTOP = HEAP_TOP;
     SET_I(c_p->cp);
     Goto(*I);
 }

 OpCase(i_module_info_1): {
     Eterm res;

     SWAPOUT;
     res = erts_module_info_1(c_p, I[-3], r(0));
     HTOP = HEAP_TOP;
     if (is_value(res)) {
	 r(0) = res;
	 SET_I(c_p->cp);
	 Goto(*I);
     }
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = I-3;
     goto lb_error_action_code;
 }

 /*
  * New floating point instructions.
  */

 OpCase(fmove_ol): {
     Eterm fr = Arg(2);
     Eterm* next;

     PreFetch(3, next);
     *ADD_BYTE_OFFSET(freg[0].fw, fr) = Arg(0);
     *ADD_BYTE_OFFSET(freg[0].fw, fr+sizeof(Eterm)) = Arg(1);
     NextPF(3, next);
 }
 OpCase(fmove_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     Eterm* next;

     PreFetch(2, next);
     GetR(0, targ1);
     *ADD_BYTE_OFFSET(freg[0].fw, fr) = *(float_val(targ1)+1);
     *ADD_BYTE_OFFSET(freg[0].fw, fr+sizeof(Eterm)) = *(float_val(targ1)+2);
     NextPF(2, next);
 }
 OpCase(fmove_ld): {
     Eterm* hp = ArithAlloc(c_p, 3);
     Eterm dest = make_float(hp);
     Eterm fr = Arg(0);
     Eterm* next;

     ArithCheck(c_p);
     PreFetch(2, next);
     hp[0] = HEADER_FLONUM;
     hp[1] = *ADD_BYTE_OFFSET(freg[0].fw, fr);
     hp[2] = *ADD_BYTE_OFFSET(freg[0].fw, fr+sizeof(Eterm));
     StoreResult(dest, Arg(1));
     NextPF(2, next);
 }

 OpCase(fconv_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     Eterm* next;

     GetR(0, targ1);
     PreFetch(2, next);
     if (is_small(targ1)) {
	 fb(fr) = (double) signed_val(targ1);
     } else if (is_big(targ1)) {
	 if (big_to_double(targ1, &fb(fr)) < 0) {
	     goto fbadarith;
	 }
     } else if (is_float(targ1)) {
	 *ADD_BYTE_OFFSET(freg[0].fw, fr) = *(float_val(targ1)+1);
	 *ADD_BYTE_OFFSET(freg[0].fw, fr+sizeof(Eterm)) = *(float_val(targ1)+2);
     } else {
	 goto fbadarith;
     }
     NextPF(2, next);
 }

#ifdef NO_FPE_SIGNALS
     OpCase(fclearerror):
     OpCase(i_fcheckerror):
	 erl_exit(1, "fclearerror/i_fcheckerror without fpe signals (beam_emu)");
#else
     OpCase(fclearerror): {
	 Eterm* next;

	 PreFetch(0, next);
	 ERTS_FP_CHECK_INIT();
	 NextPF(0, next);
     }

     OpCase(i_fcheckerror): {
	 Eterm* next;

	 PreFetch(0, next);
	 ERTS_FP_ERROR(freg[0].fd, goto fbadarith);
	 NextPF(0, next);
     }
#  undef ERTS_FP_CHECK_INIT
#  undef ERTS_FP_ERROR
#  define ERTS_FP_CHECK_INIT()
#  define ERTS_FP_ERROR(a, b)
#endif


 OpCase(i_fadd_lll): {
     Eterm* next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT();
     fb(Arg(2)) = fb(Arg(0)) + fb(Arg(1));
     ERTS_FP_ERROR(fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fsub_lll): {
     Eterm* next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT();
     fb(Arg(2)) = fb(Arg(0)) - fb(Arg(1));
     ERTS_FP_ERROR(fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fmul_lll): {
     Eterm* next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT();
     fb(Arg(2)) = fb(Arg(0)) * fb(Arg(1));
     ERTS_FP_ERROR(fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fdiv_lll): {
     Eterm* next;

     PreFetch(3, next);
     ERTS_FP_CHECK_INIT();
     fb(Arg(2)) = fb(Arg(0)) / fb(Arg(1));
     ERTS_FP_ERROR(fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fnegate_ll): {
     Eterm* next;

     PreFetch(2, next);
     ERTS_FP_CHECK_INIT();
     fb(Arg(1)) = -fb(Arg(0));
     ERTS_FP_ERROR(fb(Arg(1)), goto fbadarith);
     NextPF(2, next);

 fbadarith:
     c_p->freason = BADARITH;
     goto find_func_info;
 }

#ifdef HIPE
 {
     unsigned cmd;
     unsigned result;

     OpCase(hipe_trap_call): {
	 /*
	  * I[-5]: &&lb_i_func_info_IaaI
	  * I[-4]: Native code callee (inserted by HiPE)
	  * I[-3]: Module (tagged atom)
	  * I[-2]: Function (tagged atom)
	  * I[-1]: Arity (untagged integer)
	  * I[ 0]: &&lb_hipe_trap_call
	  * ... remainder of original BEAM code
	  */
	 ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
	 c_p->hipe.ncallee = (void(*)(void)) I[-4];
	 cmd = HIPE_MODE_SWITCH_CMD_CALL | (I[-1] << 8);
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_call_closure): {
       ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.ncallee = (void(*)(void)) I[-4];
       cmd = HIPE_MODE_SWITCH_CMD_CALL_CLOSURE | (I[-1] << 8);
       goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_return): {
	 cmd = HIPE_MODE_SWITCH_CMD_RETURN;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_throw): {
	 cmd = HIPE_MODE_SWITCH_CMD_THROW;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_resume): {
	 cmd = HIPE_MODE_SWITCH_CMD_RESUME;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_reschedule): {
	 cmd = HIPE_MODE_SWITCH_CMD_RESCHEDULE;
	 goto L_hipe_mode_switch;
     }
 L_hipe_mode_switch:
     SWAPOUT;
     c_p->fcalls = FCALLS;
     reg[0] = r(0);
     result = hipe_mode_switch(c_p, cmd, reg);
     FCALLS = c_p->fcalls;
     SWAPIN;
     switch( result ) {
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 MoveReturn(reg[0], r(0));
       case HIPE_MODE_SWITCH_RES_SUSPEND:
	 return REDS_IN(c_p) - FCALLS;
       case HIPE_MODE_SWITCH_RES_CALL:
	 SET_I(c_p->i);
	 r(0) = reg[0];
	 Dispatch();
       case HIPE_MODE_SWITCH_RES_THROW:
	 if( c_p->freason == EXC_USER_ERROR ) {
	     c_p->freason = EXC_ERROR; /* don't build backtrace */
	 }
	 c_p->cp = NULL;
	 I = handle_error(c_p, I, reg, NULL);
	 goto post_error_handling;
       default:
	 erl_exit(1, "hipe_mode_switch: result %u\n", result);
     }
 }
 OpCase(hipe_call_count): {
     /*
      * I[-5]: &&lb_i_func_info_IaaI
      * I[-4]: pointer to struct hipe_call_count (inserted by HiPE)
      * I[-3]: Module (tagged atom)
      * I[-2]: Function (tagged atom)
      * I[-1]: Arity (untagged integer)
      * I[ 0]: &&lb_hipe_call_count
      * ... remainder of original BEAM code
      */
     struct hipe_call_count *hcc = (struct hipe_call_count*)I[-4];
     ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
     ASSERT(hcc != NULL);
     ASSERT(VALID_INSTR(hcc->opcode));
     ++(hcc->count);
     Goto(hcc->opcode);
 }
#endif /* HIPE */

 OpCase(i_yield):
 {
     /* This is safe as long as REDS_IN(c_p) is never stored 
      * in c_p->arg_reg[0]. It is currently stored in c_p->def_arg_reg[5],
      * which may be c_p->arg_reg[5], which is close, but no banana.
      */
     c_p->arg_reg[0] = am_true;
     c_p->arity = 1; /* One living register (the 'true' return value) */
     SWAPOUT;
     c_p->i = I + 1; /* Next instruction */
     add_to_schedule_q(c_p);
     return REDS_IN(c_p) - FCALLS;
 }

 OpCase(i_debug_breakpoint): {
     SWAPOUT;
     reg[0] = r(0);
     tmp_arg1 = call_breakpoint_handler(c_p, I-3, reg);
     r(0) = reg[0];
     HTOP = HEAP_TOP;
     if (tmp_arg1) {
	 SET_I(c_p->i);
	 Dispatch();
     }
     goto no_error_handler;
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
	 counting_opcodes[op_i_func_info_IaaI] = LabelAddr(lb_i_func_info_IaaI);
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
	 ep->code[3] = (Eterm) OpCode(apply_bif);
	 ep->code[4] = (Eterm) bif_table[i].f;
     }

     /*
      * erts_debug:apply(Mod, Func, Args, {M,F,A})
      *  Applies Mod:Func with Args in the same way as apply/3.
      *  If a BIF is applied and it fails, the normal EXIT code
      *  to look as if the error occurred in function {M,F,A}.
      *  Also, the normal stack backtrace will suppressed.
      */

     ep = erts_export_put(am_erts_debug, am_apply, 4);
     ep->address = (Eterm *) (beam_debug_apply+5);

     /* func_info 0 erts_debug apply 4 */
     beam_debug_apply[0] = (Eterm) OpCode(i_func_info_IaaI);
     beam_debug_apply[1] = 0;
     beam_debug_apply[2] = am_erts_debug;
     beam_debug_apply[3] = am_apply;
     beam_debug_apply[4] = 4;

     /* allocate 1 4 */
     beam_debug_apply[5] = (Eterm) OpCode(allocate_tt);
     beam_debug_apply[6] = (4 << 16) | 1;

     /* move {x,3} {y,0} */
     beam_debug_apply[7] = (Eterm) OpCode(move_xy);
     beam_debug_apply[8] = (1 << 18) | (3 << 2);

     /* i_apply */
     beam_debug_apply[9] = (Eterm) OpCode(i_apply);

     /* deallocate_return 1 */
     beam_debug_apply[10] = (Eterm) OpCode(deallocate_return_P);
     beam_debug_apply[11] = (1+1)*4;

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
	Eterm* dis_next;

	save_calls(c_p, (Export *) Arg(0));

	SET_I(((Export *) Arg(0))->address);

	dis_next = (Eterm *) *I;
	FCALLS--;
	Goto(dis_next);
    }
}

/*
 * Mapping from error codes to atoms.
 */
Eterm error_atom[NUMBER_EXIT_CODES] = {
  am_internal_error,	/* 0 */
  am_normal,		/* 1 */
  am_internal_error,	/* 2 */
  am_badarg,		/* 3 */
  am_badarith,		/* 4 */
  am_badmatch,		/* 5 */
  am_function_clause,	/* 6 */
  am_case_clause,	/* 7 */
  am_if_clause,		/* 8 */
  am_undef,		/* 9 */
  am_badfun,		/* 10 */
  am_badarity,		/* 11 */
  am_timeout_value,	/* 12 */
  am_noproc,		/* 13 */
  am_notalive,		/* 14 */
  am_system_limit,	/* 15 */
};

static Eterm*
handle_error(Process* c_p, Eterm* pc, Eterm* reg, BifFunction bf)
{
    Eterm* hp;

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
    Eterm* next_p = &Where;	
                              /* Where to store the next element of Where. */
    Eterm *save_current = c_p->current; 
                              /* Needed when bif traps throws exceptions */
    c_p->i = pc;	      /* In case we call erl_exit(). */

    /*
     * First, make sure that we know the {M,F,A} of the current function.
     */

    if (pc != NULL) {
	if ((c_p->current = find_function_from_pc(pc)) == NULL) {
	    if (beam_debug_apply <= pc && pc < beam_debug_apply+beam_debug_apply_size) {
		c_p->current = c_p->stop;
		if (c_p->freason & EXF_ARGLIST) {
		    c_p->freason -= EXF_ARGLIST;
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
    { Uint r = c_p->freason & EXF_INDEXBITS;
      ASSERT(r < NUMBER_EXIT_CODES); /* range check */
      if (r < NUMBER_EXIT_CODES) {
          Value = error_atom[r];
      } else {
	  Value = am_internal_error;
	  c_p->freason = EXC_INTERNAL_ERROR;
      }
    }

    /*
     * Throws that are not caught are turned into 'nocatch' errors.
     */

    if ((c_p->freason & EXF_THROWN) && (c_p->catches <= 0) ) {
        hp = HAlloc(c_p, 3);
        c_p->fvalue = TUPLE2(hp, am_nocatch, c_p->fvalue);
        c_p->freason = EXC_USER_ERROR;   /* force stack trace and log */
        KILL_HP(hp);
    }

    /*
     * Make sure we form the correct error value
     */

    switch (c_p->freason & EXF_INDEXBITS) {
    case (EXC_EXIT & EXF_INDEXBITS):
        /* Primary exceptions use fvalue directly */
        ASSERT(is_value(c_p->fvalue));
        Value = c_p->fvalue;
        break;
    case (EXC_BADMATCH & EXF_INDEXBITS):
    case (EXC_CASE_CLAUSE & EXF_INDEXBITS):
    case (EXC_BADFUN & EXF_INDEXBITS):
	ASSERT(is_value(c_p->fvalue));
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, Value, c_p->fvalue);
	KILL_HP(hp);
	break;
    case (EXC_BADARITY & EXF_INDEXBITS):
	ASSERT(is_value(c_p->fvalue));
	hp = HAlloc(c_p, 2);
	ASSERT(*next_p == NIL);
	*next_p = CONS(hp, c_p->fvalue, NIL);
	next_p = hp + 1;
	KILL_HP(hp);
	break;
    }

#ifdef DEBUG
    c_p->fvalue = THE_NON_VALUE;
    ASSERT(Value != am_internal_error);
#endif

    /*
     * Build the Where part of the reason (the backtrace), if the
     * EXF_TRACE flag is set.
     */

    if (c_p->freason & EXF_TRACE) {
        Eterm mfa;
	int max_depth = erts_backtrace_depth;
	Uint* fi;
	Uint* ptr;
	Uint* prev = NULL;	/* Pointer to func_info for previous function
				 * put in Where.
				 */

	/*
	 * Check if we have an arglist term to use instead of the arity
         * for the top level call. (If so, this is encoded in Value.)
	 */

	if (c_p->freason & EXF_ARGLIST) {
	    Eterm* tp;

	    ASSERT(is_tuple(Value));
	    prev = c_p->current;
	    ASSERT(prev != NULL);
	    hp = HAlloc(c_p, 6);
	    tp = tuple_val(Value);
	    Value = tp[1];
	    mfa = TUPLE3(hp, prev[0], prev[1], tp[2]);
	    hp += 4;
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = hp + 1;
	    bf = NULL;
	    KILL_HP(hp);
	}

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
		/* 
		 * The Bif does not really exist (no BIF entry). 
		 * It is a TRAP and trap's are called through
		 * apply_bif, which also sets c_p->current (luckily).
		 * We save c_p->current at the beginning of this function
		 * so that we can dig out {M,F,Args} from that. 
		 */

		int arity = save_current[2];
		Eterm args;
		ASSERT(is_atom(save_current[0]) && is_atom(save_current[1]) &&
		       save_current[2] <= 3);
		

		hp = HAlloc(c_p, 6+2*arity);
		args = NIL;
		while (arity > 0) {
		    args = CONS(hp, reg[arity-1], args);
		    hp += 2;
		    arity--;
		}
		mfa = TUPLE3(hp, save_current[0], save_current[1], args);
		hp += 4;
		ASSERT(*next_p == NIL);
		*next_p = CONS(hp, mfa, NIL);
		next_p = hp + 1;
		KILL_HP(hp);
	    }
	}

	/*
	 * Add the {M,F,A} for the current function,
	 * where A is arity or arguments.
	 */

	if (c_p->current != prev) {
	    Eterm a;		/* Arguments or arity. */
	    prev = c_p->current;
	    if (c_p->current == c_p->stop) {
		mfa = c_p->stop[1];
		max_depth = 0;
	    } else {
		if ( (c_p->freason & EXF_INDEXBITS) !=
		     (EXC_FUNCTION_CLAUSE & EXF_INDEXBITS) ) {
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
	if ( (c_p->freason & EXF_INDEXBITS) ==
	     (EXC_FUNCTION_CLAUSE & EXF_INDEXBITS) &&
	     fi != NULL && fi != prev && max_depth > 0) {
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
	    for (ptr = c_p->stop; ptr < STACK_BEGIN; ptr++) {
		if (is_CP(*ptr)) {
		    fi = find_function_from_pc(cp_val(*ptr));
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
	Value = TUPLE2(hp, Value, Where);
	KILL_HP(hp);
    }

    if ((c_p->catches <= 0) || (c_p->freason & EXF_PANIC)) {
	/*
	 * No catch active -- terminate the process.
	 */
	if (c_p->freason & EXF_LOG) {
	    cerr_pos = 0;
	    erl_printf(CBUF, "Error in process ");
	    display(c_p->id, CBUF);
	    if (this_node != am_Noname) {
		erl_printf(CBUF, " on node ");
		print_atom(atom_val(this_node), CBUF);
	    }
	    erl_printf(CBUF, " with exit value: ");
	    ldisplay(Value, CBUF, display_items);
	    erl_printf(CBUF, "\n");
	    send_error_to_logger(c_p->group_leader);
	}

	/*
	 * If zombies are kept, the process will be garbage-collected.
	 * Must zero c_p->arity to indicate that there are no live registers.
	 */
	c_p->arity = 0;
	do_exit(c_p, Value);
    } else {
	Eterm* ptr;

	/*
	 * Make sure the exception is stable, if anybody looks at
	 * freason/fvalue again after this point, by storing the final
	 * Value and keeping only the primary flags.
	 */

	c_p->freason &= EXF_PRIMARY;   /* index becomes zero */
	c_p->fvalue = Value;
	
	/*
	 * Search for the first catch.
	 */

	for (ptr = c_p->stop + CP_SIZE; ptr < STACK_BEGIN; ptr++) {
	    if (is_catch(*ptr)) {
		pc = catch_pc(*ptr);
		while (is_not_CP(*ptr)) {
		    ptr--;
		    ASSERT(c_p->stop <= ptr);
		}
		c_p->stop = ptr;
		
		reg[0] = THE_NON_VALUE;
		if ((c_p->freason & EXF_THROWN) != 0) {
		    reg[1] = am_THROW;
		} else if ((c_p->freason & EXF_EXIT) != 0) {
		    reg[1] = am_EXIT;
		} else {
		    reg[1] = am_ERROR;
		}
		reg[2] = Value;
		return pc;
	    }
	}
	erl_exit(1, "Catch not found");
    }
    return 0;
}


static Eterm
call_error_handler(Process* p, Eterm* fi, Eterm* reg)
{
    Eterm* hp;
    Export* ep;
    int arity;
    Eterm args;
    int i;

    /*
     * Search for the error_handler module.
     */
    ep = erts_find_function(p->error_handler, am_undefined_function, 3);
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = EXC_UNDEF;
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

static Eterm
call_breakpoint_handler(Process* p, Eterm* fi, Eterm* reg)
{
    Eterm* hp;
    Export* ep;
    int arity;
    Eterm args;
    int i;

    /*
     * Search for error handler module.
     */
    ep = erts_find_function(p->error_handler, am_breakpoint, 3);
    if (ep == NULL) {		/* No error handler */
	p->current = fi;
	p->freason = EXC_UNDEF;
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
     * Set up registers for call to error_handler:breakpoint/3.
     */
    reg[0] = fi[0];
    reg[1] = fi[1];
    reg[2] = args;
    return 1;
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
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
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
	 Eterm args)		/* THE_NON_VALUE or pre-built list of arguments. */
{
    Eterm fun = reg[arity];
    int i;
    Eterm function;
    Eterm* hp;

    if (is_fun(fun)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);
	ErlFunEntry* fe;
	Eterm* code_ptr;
	Eterm* var_ptr;
	int actual_arity;
	unsigned num_free;

	fe = funp->fe;
	num_free = funp->num_free;
	code_ptr = fe->address;
	actual_arity = (int) code_ptr[-1];
	var_ptr = funp->env;

	if (actual_arity == arity+num_free) {
	    reg += arity;
	    for (i = 0; i < num_free; i++) {
		reg[i] = var_ptr[i];
	    }
	    reg[i] = fun;
	    return code_ptr;
	} else {
	    /*
	     * Something wrong here. First build a list of the arguments.
	     */  

	    if (is_non_value(args)) {
		args = NIL;
		hp = HAlloc(p, arity*2);
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    if (actual_arity >= 0) {
		/*
		 * There is a fun defined, but the call has the wrong arity.
		 */
		hp = HAlloc(p, 3);
		p->freason = EXC_BADARITY;
		p->fvalue = TUPLE2(hp, fun, args);
		return NULL;
	    } else {
		Export* ep;
		Module* modp;
		Eterm module;

		/*
		 * No arity. There is no module loaded that defines the fun,
		 * either because the fun is newly created from the external
		 * representation (the module has never been loaded),
		 * or the module defining the fun has been unloaded.
		 */

		module = fe->module;
		if ((modp = erts_get_module(module)) != NULL && modp->code != NULL) {
		    /*
		     * There is a module loaded, but obviously the fun is not
		     * defined in it. We must not call the error_handler
		     * (or we will get into an infinite loop).
		     */
		    goto badfun;
		}
		
		/*
		 * No current code for this module. Call the error_handler module
		 * to attempt loading the module.
		 */

		ep = erts_find_function(p->error_handler, am_undefined_lambda, 3);
		if (ep == NULL) {	/* No error handler */
		    p->current = NULL;
		    p->freason = EXC_UNDEF;
		    return NULL;
		}
		reg[0] = module;
		reg[1] = fun;
		reg[2] = args;
		return ep->address;
	    }
	}
    } else if (is_tuple(fun)) {
	Eterm* tp = tuple_val(fun);

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
		    p->freason = EXC_UNDEF;
		    return 0;
		}
		if (is_non_value(args)) {
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
    p->current = NULL;
    p->freason = EXC_BADFUN;
    p->fvalue = fun;
    return NULL;
}

static Eterm*
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
	if (arity < MAX_REG-1) {
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    return NULL;
	}
    }

    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	p->freason = EXC_UNDEF;
	return NULL;
    }
    reg[arity] = fun;
    return call_fun(p, arity, reg, args);
}


static Eterm
new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free)
{
    unsigned needed = ERL_FUN_SIZE + num_free;
    ErlFunThing* funp = (ErlFunThing *) HAlloc(p, needed);
    Eterm* hp = funp->env;
    int i;

    fe->refc++;
    funp->thing_word = HEADER_FUN;
    funp->next = p->off_heap.funs;
    p->off_heap.funs = funp;
    funp->fe = fe;
    funp->num_free = num_free;
    funp->creator = p->id;
#ifdef HIPE
    funp->native_address = fe->native_address;
#endif
    funp->arity = (int)fe->address[-1] - num_free;
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }
    return make_fun(funp);
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
