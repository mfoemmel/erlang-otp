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

#include <stddef.h> /* offsetof() */
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
#  define LabelAddr(Addr) &&##Addr
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *(Rel)
#  define LabelAddr(Label) &&Label
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

/*
 * Allocate memory on secondary arithmetic heap.
 * We need our special version because the heap top is in the
 * HTOP variable and not in the process structure.
 */
#if defined(DEBUG)
#  define BeamArithAlloc(p, need)                               \
   (ASSERT_EXPR((need) >= 0),                                   \
    ((ARITH_AVAIL(p) < (need)) ?                                \
     erts_arith_alloc((p), HTOP, (need)) :                      \
     ((ARITH_HEAP(p) += (need)), (ARITH_AVAIL(p) -= (need)),    \
      (ARITH_CHECK_ME(p) = ARITH_HEAP(p)),                      \
      (ARITH_HEAP(p) - (need)))))
#else
#  define BeamArithAlloc(p, need)                               \
    ((ARITH_AVAIL(p) < (need)) ?                                \
     erts_arith_alloc((p), HTOP, (need)) :                      \
     ((ARITH_HEAP(p) += (need)),                                \
      (ARITH_AVAIL(p) -= (need)),                               \
      (ARITH_HEAP(p) - (need))))
#endif

/*
 * Define macros for deep checking of terms.
 */

#if defined(HARDDEBUG)

#  define CHECK_TERM(T) size_object(T)

#  define CHECK_ARGS(PC)                 \
do {                                     \
  int i_;                                \
  int Arity_ = PC[-1];                   \
  if (Arity_ > 0) {                      \
	CHECK_TERM(r(0));                \
  }                                      \
  for (i_ = 1; i_ < Arity_; i_++) {      \
	CHECK_TERM(x(i_));               \
  }                                      \
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
#define VALID_INSTR(IP) (0 <= (int)(IP) && ((int)(IP) < (NUMBER_OF_OPCODES*2+10)))
#else
#define VALID_INSTR(IP) \
   ((Sint)LabelAddr(emulator_loop) <= (Sint)(IP) && \
    (Sint)(IP) < (Sint)LabelAddr(end_emulator_loop))
#endif /* NO_JUMP_TABLE */

#define SET_CP(p, ip)           \
   ASSERT(VALID_INSTR(*(ip)));  \
   (p)->cp = (ip)

#define SET_I(ip) \
   ASSERT(VALID_INSTR(* (Eterm *)(ip))); \
   I = (ip)

#define FetchArgs(S1, S2) tmp_arg1 = (S1); tmp_arg2 = (S2)

/*
 * Store a result into a register given a destination descriptor.
 */

#define StoreResult(Result, DestDesc)               \
  do {                                              \
    Eterm stb_reg;                                  \
    stb_reg = (DestDesc);                           \
    CHECK_TERM(Result);                             \
    switch (beam_reg_tag(stb_reg)) {                \
    case R_REG_DEF:                                 \
      r(0) = (Result); break;                       \
    case X_REG_DEF:                                 \
      xb(x_reg_offset(stb_reg)) = (Result); break;  \
    default:                                        \
      yb(y_reg_offset(stb_reg)) = (Result); break;  \
    }                                               \
  } while (0)

#define StoreSimpleDest(Src, Dest) Dest = (Src)

/*
 * Store a result into a register and execute the next instruction.
 * Dst points to the word with a destination descriptor, which MUST
 * be just before the next instruction.
 */
 
#define StoreBifResult(Dst, Result)                          \
  do {                                                       \
    Eterm* stb_next;                                         \
    Eterm stb_reg;                                           \
    stb_reg = Arg(Dst);                                      \
    I += (Dst) + 2;                                          \
    stb_next = (Eterm *) *I;                                 \
    CHECK_TERM(Result);                                      \
    switch (beam_reg_tag(stb_reg)) {                         \
    case R_REG_DEF:                                          \
      r(0) = (Result); Goto(stb_next);                       \
    case X_REG_DEF:                                          \
      xb(x_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    default:                                                 \
      yb(y_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    }                                                        \
  } while (0)

#define ClauseFail() goto lb_jump_f

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

#if defined(SHARED_HEAP)
#define SWAPIN                    \
    HTOP = HEAP_TOP(c_p);         \
    HeapLimit = HEAP_END(c_p);    \
    StackLimit = STACK_END(c_p);  \
    E = c_p->stop

#define SWAPOUT                   \
    HEAP_TOP(c_p) = HTOP;         \
    HEAP_END(c_p) = HeapLimit;    \
    STACK_END(c_p) = StackLimit;  \
    c_p->stop = E

#elif defined(HYBRID)
#define SWAPIN             \
    g_htop = global_htop;  \
    g_hend = global_hend;  \
    HTOP = HEAP_TOP(c_p);  \
    E = c_p->stop

#define SWAPOUT            \
    global_htop = g_htop;  \
    global_hend = g_hend;  \
    HEAP_TOP(c_p) = HTOP;  \
    c_p->stop = E

#else
#define SWAPIN             \
    HTOP = HEAP_TOP(c_p);  \
    E = c_p->stop

#define SWAPOUT            \
    HEAP_TOP(c_p) = HTOP;  \
    c_p->stop = E
#endif

#define SAVE_HTOP HEAP_TOP(c_p) = HTOP

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(Eterm *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(Eterm *) (((unsigned char *)E) + (N)))
#define fb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

#ifdef SHARED_HEAP

#ifdef DEBUG
#define DEBUG_MEMSET sys_memset(c_p->send, 0xff, c_p->stack_sz*sizeof(Eterm))
#else
#define DEBUG_MEMSET
#endif

#define AllocateStack(StackNeed)					\
  do {									\
    ASSERT(c_p->send <= E && E <= c_p->stack);				\
    if (E - c_p->send < (StackNeed)) {					\
      int used_stack = c_p->stack - E;					\
      int new_sz = erts_next_heap_size(c_p->stack_sz + (StackNeed), 0);	\
      Eterm *new_stack =						\
        (Eterm*) ERTS_STACK_ALLOC(sizeof(Eterm) * new_sz);		\
      sys_memmove((new_stack + new_sz) - used_stack, E,			\
                  used_stack * sizeof(Eterm));				\
      DEBUG_MEMSET;							\
      ERTS_STACK_FREE((void *) c_p->send, c_p->stack_sz*sizeof(Eterm));	\
      c_p->stack_sz = new_sz;						\
      c_p->send = new_stack;						\
      c_p->stop = new_stack + new_sz - used_stack;			\
      c_p->stack = new_stack + new_sz;					\
      E = c_p->stop;							\
    }									\
    E -= (StackNeed);							\
    ASSERT(c_p->send <= E && E <= c_p->stack);				\
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
     ASSERT(StackLimit <= E && E <= c_p->stack);                         \
     ASSERT(HEAP_START(c_p) <= HTOP && HTOP <= HeapLimit);               \
     needed = (StackNeed) + CP_SIZE;                                     \
     if (E - StackLimit < needed) {                                      \
       int used_stack = c_p->stack - E;                                  \
       int new_sz = erts_next_heap_size(c_p->stack_sz + needed, 0);      \
       Eterm *new_stack = (Eterm *)                                      \
	   ERTS_STACK_ALLOC(sizeof(Eterm)*new_sz);                       \
       sys_memcpy((new_stack + new_sz) - used_stack,                     \
                   E,                                                    \
                   used_stack * sizeof(Eterm));                          \
       DEBUG_MEMSET;                                                     \
       ERTS_STACK_FREE((void *) StackLimit, c_p->stack_sz*sizeof(Eterm));\
       c_p->stack_sz = new_sz;                                           \
       StackLimit = c_p->send = new_stack;                               \
       c_p->stop = new_stack + new_sz - used_stack;                      \
       c_p->stack = new_stack + new_sz;                                  \
       E = c_p->stop;                                                    \
     }                                                                   \
     if ((HeapNeed) > HeapLimit - HTOP) {                                \
           SWAPOUT;                                                      \
           reg[0] = r(0);                                                \
           FCALLS -= erts_garbage_collect(c_p, (HeapNeed), reg, (M));    \
           r(0) = reg[0];                                                \
           SWAPIN;                                                       \
     }                                                                   \
     E -= needed;                                                        \
     SAVE_CP(E);                                                         \
     ASSERT(c_p->send <= E && E <= c_p->stack);                          \
     ASSERT(HEAP_START(c_p) <= HTOP && HTOP <= HeapLimit);               \
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
  } while (0)
#endif

#define Allocate(Ns, Live) AH(Ns, 0, Live)

#define AllocateZero(Ns, Live)             \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, 0, Live);                      \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateHeap(Ns, Nh, Live) AH(Ns, Nh, Live)

#define PutString(Len, Ptr, Dst)                                     \
  do {                                                               \
      int len = (Len);                                               \
      unsigned char* s = (unsigned char *) (Ptr);                    \
      Eterm result = NIL;                                            \
      for (s = (unsigned char *) Arg(1); len > 0; s--, len--) {      \
	  PutList(make_small(*s), result, result, StoreSimpleDest);  \
      }                                                              \
      StoreResult(result, Dst);                                      \
  } while (0)

#define AllocateHeapZero(Ns, Nh, Live)     \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, Nh, Live);                     \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateInit(Ns, Live, Y) \
  do { AH(Ns, 0, Live); make_blank(Y); } while (0)

/*
 * Like the AH macro, but allocates no additional heap space.
 */

#define A(StackNeed, M) AH(StackNeed, 0, M)

#define D(N)             \
     RESTORE_CP(E);      \
     E += (N) + CP_SIZE;


/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 */

#ifdef SHARED_HEAP
#define TestHeap(Nh, Live)                                      \
  do {                                                          \
    unsigned need = (Nh);                                       \
    ASSERT(HEAP_START(c_p) <= HTOP && HTOP <= HeapLimit);       \
    if (HeapLimit - HTOP < need) {                              \
       SWAPOUT;                                                 \
       reg[0] = r(0);                                           \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  \
       ASSERT(IS_ACTIVE(c_p));                                  \
       r(0) = reg[0];                                           \
       SWAPIN;                                                  \
    }                                                           \
  } while (0)
#else
#define TestHeap(Nh, Live)                                      \
  do {                                                          \
    unsigned need = (Nh);                                       \
    if (E - HTOP < need) {                                      \
       SWAPOUT;                                                 \
       reg[0] = r(0);                                           \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  \
       r(0) = reg[0];                                           \
       SWAPIN;                                                  \
    }                                                           \
  } while (0)
#endif /* SHARED_HEAP */

#ifdef HYBRID
#ifdef INCREMENTAL_GC
#define TestGlobalHeap(Nh, Live, hp)                                    \
  do {                                                                  \
    unsigned need = (Nh);                                               \
    ASSERT(global_heap <= g_htop && g_htop <= global_hend);             \
    SWAPOUT;                                                            \
    reg[0] = r(0);                                                      \
    FCALLS -= need;                                                     \
    (hp) = IncAlloc(c_p,need,reg,(Live));                               \
    r(0) = reg[0];                                                      \
    SWAPIN;                                                             \
  } while (0)
#else
#define TestGlobalHeap(Nh, Live, hp)                                    \
  do {                                                                  \
    unsigned need = (Nh);                                               \
    ASSERT(global_heap <= g_htop && g_htop <= global_hend);             \
    if (g_hend - g_htop < need) {                                       \
       SWAPOUT;                                                         \
       reg[0] = r(0);                                                   \
       FCALLS -= erts_global_garbage_collect(c_p, need, reg, (Live));   \
       r(0) = reg[0];                                                   \
       SWAPIN;                                                          \
    }                                                                   \
    (hp) = global_htop;                                                 \
  } while (0)
#endif
#endif /* HYBRID */

#define Init(N) make_blank(yb(N))

#define Init2(Y1, Y2) do { make_blank(Y1); make_blank(Y2); } while (0)
#define Init3(Y1, Y2, Y3) \
   do { make_blank(Y1); make_blank(Y2); make_blank(Y3); } while (0)

#define MakeFun(FunP, NumFree)					\
  do {								\
     SWAPOUT;							\
     reg[0] = r(0);						\
     r(0) = new_fun(c_p, reg, (ErlFunEntry *) FunP, NumFree);	\
     SWAPIN;							\
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
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
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
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch_fun;		\
     }						\
 } while (0)

#define DispatchMacrox()					\
  do {								\
     if (FCALLS > 0) {						\
        Eterm* dis_next;					\
        SET_I(((Export *) Arg(0))->address);			\
        dis_next = (Eterm *) *I;				\
        FCALLS--;						\
        CHECK_ARGS(I);						\
        Goto(dis_next);						\
     } else if (c_p->ct != NULL && FCALLS > neg_o_reds) {	\
        goto save_calls1;					\
     } else {							\
        SET_I(((Export *) Arg(0))->address);			\
        CHECK_ARGS(I);						\
	goto context_switch;					\
     }								\
 } while (0)

#ifdef SHARED_HEAP
#define MAYBE_SHRINK(p,hp,res,alloc) ;
#else
#define MAYBE_SHRINK(p,hp,res,alloc)                                \
  do {                                                              \
      Uint actual;                                                  \
      if (is_small(res)) {                                          \
          erts_arith_shrink(p, hp);                                 \
      } else if ((actual = bignum_header_arity(*hp)+1) < alloc) {   \
          erts_arith_shrink(p, hp+actual);                          \
      }                                                             \
  } while (0)
#endif

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
#define Node(R) R = erts_this_node->sysname

#define Arg(N)       I[(N)+1]
#define Next(N)                \
    I += (N) + 1;              \
    ASSERT(VALID_INSTR(*I));   \
    Goto(*I)

#define PreFetch(N, Dst) do { Dst = (Eterm *) *(I + N + 1); } while (0)
#define NextPF(N, Dst)         \
    I += N + 1;                \
    ASSERT(VALID_INSTR(Dst));  \
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

#define GetArg2(N, Dst1, Dst2)     \
   do {                            \
     GetR(N, Dst1);                \
     GetR((N)+1, Dst2);            \
   } while (0)

#define PutList(H, T, Dst, Store)  \
  do {                             \
   HTOP[0] = (H); HTOP[1] = (T);   \
   Store(make_list(HTOP), Dst);    \
   HTOP += 2;                      \
  } while (0)

#define Move(Src, Dst, Store)      \
   do {                            \
       Eterm term = (Src);         \
       Store(term, Dst);           \
   } while (0)

#define Move2(src1, dst1, src2, dst2) dst1 = (src1); dst2 = (src2)

#define MoveGenDest(src, dstp) \
   if ((dstp) == NULL) { r(0) = (src); } else { *(dstp) = src; }

#define MoveReturn(Src, Dest)       \
    (Dest) = (Src);                 \
    I = c_p->cp;                    \
    ASSERT(VALID_INSTR(*c_p->cp));  \
    CHECK_TERM(r(0));               \
    Goto(*I)

#define DeallocateReturn(Deallocate)       \
  do {                                     \
    int words_to_pop = (Deallocate);       \
    SET_I(cp_val(*E));                     \
    E = ADD_BYTE_OFFSET(E, words_to_pop);  \
    CHECK_TERM(r(0));                      \
    Goto(*I);                              \
  } while (0)

#define MoveDeallocateReturn(Src, Dest, Deallocate)  \
    (Dest) = (Src);                                  \
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
    SET_I((Eterm *) CallDest);				\
    Dispatch();

#define MoveCallOnly(Src, Dest, CallDest)	\
    (Dest) = (Src);				\
    SET_I((Eterm *) CallDest);			\
    Dispatch();

#define GetList(Src, H, T) do {			\
   Eterm* tmp_ptr = list_val(Src);		\
   H = CAR(tmp_ptr);				\
   T = CDR(tmp_ptr); } while (0)

#define GetTupleElement(Src, Element, Dest)				  \
  do {									  \
    tmp_arg1 = (Eterm) (((unsigned char *) tuple_val(Src)) + (Element));  \
    (Dest) = (*(Eterm *)tmp_arg1);					  \
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

#define IsIntegerAllocate(Src, Need, Alive, Fail)  \
    if (is_not_integer(Src)) { Fail; }             \
    A(Need, Alive)

#define IsNil(Src, Fail) if (is_not_nil(Src)) { Fail; }

#define IsList(Src, Fail) if (is_not_list(Src) && is_not_nil(Src)) { Fail; }

#define IsNonemptyList(Src, Fail) if (is_not_list(Src)) { Fail; }

#define IsNonemptyListAllocate(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    A(Need, Alive)

#define IsNonemptyListTestHeap(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    TestHeap(Need, Alive)

#define IsTuple(X, Action) if (is_not_tuple(X)) Action

#define IsArity(Pointer, Arity, Fail) \
    if (*(Eterm *)(tmp_arg1 = (Eterm)tuple_val(Pointer)) != (Arity)) { Fail; }

#define IsFunction(X, Action)			\
  do {						\
     if ( !(is_any_fun(X)) ) {			\
          Action;				\
     }						\
  } while (0)

#define IsFunction2(F, A, Action)		\
  do {						\
     if (is_function_2(c_p, F, A) != am_true ) {\
          Action;				\
     }						\
  } while (0)

#define IsTupleOfArity(Src, Arity, Fail) \
  do { \
    if (is_not_tuple(Src) || *(Eterm *)(tmp_arg1 = (Eterm) tuple_val(Src)) != Arity) { \
        Fail; \
    } \
  } while (0)

#define IsBoolean(X, Fail) if ((X) != am_true && (X) != am_false) { Fail; }

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
    if (erts_mb.size - erts_mb.offset < 32) { Fail; }	        \
    _integer = get_int32(erts_mb.base + erts_mb.offset/8);	\
    erts_mb.offset += 32;					\
    if (IS_USMALL(0, _integer)) {				\
	_result = make_small(_integer);				\
    } else {							\
	Eterm* _hp = BeamArithAlloc(c_p, BIG_UINT_HEAP_SIZE);	\
	_result = uint_to_big((Uint) _integer, _hp);		\
    }								\
    Store(_result, Dst);					\
 } while (0)

#define BsGetIntegerImm(Sz, Flags, Dst, Store, Fail)	\
 do {							\
    Eterm _result;					\
    SWAPOUT;						\
    _result = erts_bs_get_integer(c_p, (Sz), (Flags));	\
    HTOP = HEAP_TOP(c_p);				\
    if (is_non_value(_result)) { Fail; }		\
    else { Store(_result, Dst); }			\
 } while (0)

#define BsGetInteger(Sz, Flags, Dst, Store, Fail)			\
 do {									\
    Eterm _result; Sint _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_integer(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP(c_p);						\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetFloat(Sz, Flags, Dst, Store, Fail)				\
 do {									\
    Eterm _result; Sint _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_float(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP(c_p);					      	\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetBinaryImm(Sz, Flags, Dst, Store, Fail)	\
 do {							\
    Eterm _result;					\
    SWAPOUT;						\
    _result = erts_bs_get_binary(c_p, (Sz), (Flags));	\
    HTOP = HEAP_TOP(c_p);				\
    if (is_non_value(_result)) { Fail; }		\
    else { Store(_result, Dst); }			\
 } while (0)

#define BsGetBinary(Sz, Flags, Dst, Store, Fail)			\
 do {									\
    Eterm _result; Sint _size;						\
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { Fail; }	\
    _size *= ((Flags) >> 3);						\
    SWAPOUT;								\
    _result = erts_bs_get_binary(c_p, _size, (Flags));			\
    HTOP = HEAP_TOP(c_p);						\
    if (is_non_value(_result)) { Fail; }				\
    else { Store(_result, Dst); }					\
 } while (0)

#define BsGetBinaryAll(Dst, Store, Fail)	\
 do {						\
    Eterm _result;				\
    SWAPOUT;					\
    _result = erts_bs_get_binary_all(c_p);	\
    HTOP = HEAP_TOP(c_p);			\
    if (is_non_value(_result)) { Fail; }	\
    else { Store(_result, Dst); }		\
 } while (0)

#define BsSkipBits(Bits, Unit, Fail)					\
 do {									\
    size_t new_offset; Sint _size;					\
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

#define NewBsPutIntegerImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_new_bs_put_integer((Src), (Sz), (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutInteger(Sz, Flags, Src)						\
 do {										\
    Sint _size = signed_val(Sz) * ((Flags) >> 3);				\
    if (!erts_new_bs_put_integer((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutFloatImm(Sz, Flags, Src)					\
 do {									\
    if (!erts_new_bs_put_float((Src), (Sz), (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutFloat(Sz, Flags, Src)					\
 do {									\
    Sint _size = signed_val(Sz) * ((Flags) >> 3);			\
    if (!erts_new_bs_put_float((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define NewBsPutBinary(Sz, Flags, Src)					\
 do {									\
    Sint _size = signed_val(Sz) * ((Flags) >> 3);			\
    if (!erts_new_bs_put_binary((Src), _size)) { goto badarg; }		\
 } while (0)

#define NewBsPutBinaryImm(Sz, Src)				        \
 do {							        \
    if (!erts_new_bs_put_binary((Src), (Sz))) { goto badarg; }	\
 } while (0)

#define NewBsPutBinaryAll(Src)				        \
 do {							        \
    if (!erts_new_bs_put_binary_all((Src))) { goto badarg; }	\
 } while (0)

/*
 * Macros for old instruction set for constructing binaries.
 */

#define BsPutInteger(Sz, Flags, Src)					\
 do {									\
    Sint _size;							        \
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_integer((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutFloat(Sz, Flags, Src)					\
 do {									\
    Sint _size;							        \
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_float((Src), _size, (Flags))) { goto badarg; }	\
 } while (0)

#define BsPutBinary(Sz, Flags, Src)					\
 do {									\
    Sint _size;							        \
    if (!is_small(Sz) || (_size = signed_val(Sz)) < 0) { goto badarg; }	\
    _size *= ((Flags) >> 3);						\
    if (!erts_bs_put_binary((Src), _size)) { goto badarg; }		\
 } while (0)

#define BsPutBinaryAll(Src)				        \
 do {							        \
    if (!erts_bs_put_binary_all((Src))) { goto badarg; }	\
 } while (0)


#define IsPort(Src, Fail) if (is_not_port(Src)) { Fail; }
#define IsPid(Src, Fail) if (is_not_pid(Src)) { Fail; }
#define IsRef(Src, Fail) if (is_not_ref(Src)) { Fail; }

static Eterm* handle_error(Process* c_p, Eterm* pc, Eterm* reg, BifFunction bf);
static Eterm* next_catch(Process* c_p);
static void terminate_proc(Process* c_p, Eterm Value);
static Eterm add_stacktrace(Process* c_p, Eterm Value, Eterm exc);
static void save_stacktrace(Process* c_p, Eterm* pc, Eterm* reg,
			     BifFunction bf, Eterm args);
static struct StackTrace * get_trace_from_exc(Eterm exc);
static Eterm make_arglist(Process* c_p, Eterm* reg, int a);
static Eterm call_error_handler(Process* p, Eterm* ip, Eterm* reg);
static Eterm call_breakpoint_handler(Process* p, Eterm* fi, Eterm* reg);
static Uint* fixed_apply(Process* p, Eterm* reg, Uint arity);
static Eterm* apply(Process* p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static int hibernate(Process* c_p, Eterm module, Eterm function,
		     Eterm args, Eterm* reg);
static Eterm* call_fun(Process* p, int arity, Eterm* reg, Eterm args);
static Eterm* apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg);
static Eterm new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free);

#if defined(_OSE_) || defined(VXWORKS)
static int init_done;
#endif

void
init_emulator(void)
{
#if defined(_OSE_) || defined(VXWORKS)
    init_done = 0;
#endif
    process_main();
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

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(void)
{
#if !defined(_OSE_) && !defined(VXWORKS)
    static int init_done = 0;
#endif
    Process* c_p = NULL;
    int reds_used;

    /*
     * X register zero; also called r(0)
     */
    register Eterm x0 REG_x0 = NIL;

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register Eterm* reg REG_xregs = NULL;

    /*
     * Top of heap (next free location); grows upwards.
     */
    register Eterm* HTOP REG_htop = NULL;

#ifdef SHARED_HEAP
    /* Heap limit and Stack limit. If possible, the c compiler might
     * choose to place this variable in to a register.  In any case a
     * local variable is faster than accessing the PCB.
     */
     Eterm *HeapLimit;
     Eterm *StackLimit;
#endif

#ifdef HYBRID
     Eterm *g_htop;
     Eterm *g_hend;
#endif

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    register Eterm* E REG_stop = NULL;

    /*
     * Pointer to next threaded instruction.
     */
    register Eterm *I REG_I = NULL;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register Sint FCALLS REG_fcalls = 0;

    /*
     * Temporaries used for picking up arguments for instructions.
     */
    register Eterm tmp_arg1 REG_tmp_arg1 = NIL;
    register Eterm tmp_arg2 REG_tmp_arg2 = NIL;
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
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

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
     * c_p->arg_reg before calling the scheduler.
     */

    if (!init_done) {
	init_done = 1;
	goto init_emulator;
    }
    reg = save_reg;	/* XXX: probably wastes a register on x86 */
    c_p = NULL;
    reds_used = 0;
    goto do_schedule1;

 do_schedule:
    reds_used = REDS_IN(c_p) - FCALLS;
 do_schedule1:
    c_p = schedule(c_p, reds_used);
#ifdef HEAP_FRAG_ELIM_TEST
    ASSERT(SAVED_HEAP_TOP(c_p) == NULL);
#endif
    {
	int reds;
	Eterm* argp;
	Eterm* next;
	int i;

	argp = c_p->arg_reg;
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

	reds = c_p->fcalls;
	if (c_p->ct != NULL) {
	    neg_o_reds = -reds;
	    FCALLS = REDS_IN(c_p) = 0;
	} else {
	    neg_o_reds = 0;
	    FCALLS = REDS_IN(c_p) = reds;
	}

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
	 Sint i = signed_val(tmp_arg1) + signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     StoreBifResult(1, result);
	 }
     
     }
     SAVE_HTOP;
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
	 Sint i = signed_val(tmp_arg1) - signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     StoreBifResult(1, result);
	 }
     }
     SAVE_HTOP;
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

 OpCase(i_move_call_only_fcr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_only_f): {
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_move_call_last_fPcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_last_fP): {
     RESTORE_CP(E);
     E = ADD_BYTE_OFFSET(E, Arg(1));
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_move_call_crf): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_f): {
     SET_CP(c_p, I+2);
     SET_I((Eterm *) Arg(0));
     Dispatch();
 }

 OpCase(i_move_call_ext_last_ePcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_last_eP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));

    /*
     * Note: The pointer to the export entry is never NULL; if the module
     * is not loaded, it points to code which will invoke the error handler
     * (see lb_call_error_handler below).
     */
    Dispatchx();

 OpCase(i_move_call_ext_cre): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
    Dispatchx();

 OpCase(i_move_call_ext_only_ecr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
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
#ifdef HYBRID
     SWAPIN;
#else
     HTOP = HEAP_TOP(c_p);
#endif
     if (is_value(result)) {
	 r(0) = result;
	 CHECK_TERM(r(0));
#ifdef HEAP_FRAG_ELIM_TEST
	 if (MBUF(c_p) != NULL) {
	     reg[0] = r(0);
	     FCALLS -= erts_garbage_collect(c_p, 0, reg, 1);
	     SWAPIN;
	     r(0) = reg[0];
	 }
	 CHECK_TERM(r(0));
#endif
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
#ifdef HEAP_FRAG_ELIM_TEST
	 if (MBUF(c_p) != NULL) {
	     FCALLS -= erts_garbage_collect(c_p, 0, argp, 2);
	 }
#endif
	 goto do_schedule;
     } else if (c_p->freason == TRAP) {
	 SET_CP(c_p, I+1);
	 SET_I(((Export *)(c_p->def_arg_reg[0]))->address);
#ifdef HEAP_FRAG_ELIM_TEST
	 if (MBUF(c_p) != NULL) {
	     FCALLS -= erts_garbage_collect(c_p, 0, c_p->def_arg_reg+1, 2);
	 }
#endif
	 SWAPIN;
	 r(0) = c_p->def_arg_reg[1];
	 x(1) = c_p->def_arg_reg[2];
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
      * The second argument is guaranteed to be a register operand.
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
	 if (x(1) == am_throw) {
	     r(0) = x(2);
	 } else {
	     if (x(1) == am_error) {
	         SWAPOUT;
		 x(2) = add_stacktrace(c_p, x(2), x(3));
		 SWAPIN;
	     }
	     /* only x(2) is included in the rootset here */
#ifdef SHARED_HEAP
	     if (HeapLimit - HTOP < 3) {
		 SWAPOUT;
		 FCALLS -= erts_garbage_collect(c_p, 3, reg+2, 1);
		 SWAPIN;
	     }
#else
	     if (E - HTOP < 3) {
		 SWAPOUT;
		 FCALLS -= erts_garbage_collect(c_p, 3, reg+2, 1);
		 SWAPIN;
	     }
#endif
	     r(0) = TUPLE2(HTOP, am_EXIT, x(2));
	     HTOP += 3;
	 }
     }
     CHECK_TERM(r(0));
     Next(1);
 }

 OpCase(try_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 r(0) = x(1);
	 x(1) = x(2);
	 x(2) = x(3);
     }
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
     xb(Arg(1)) = ERL_MESSAGE_TERM(msgp);
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
     r(0) = ERL_MESSAGE_TERM(msgp);
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
     if (ERL_MESSAGE_TOKEN(msgp) == NIL) {
	 SEQ_TRACE_TOKEN(c_p) = NIL;
     } else if (ERL_MESSAGE_TOKEN(msgp) != am_undefined) {
	 Eterm msg;
	 SEQ_TRACE_TOKEN(c_p) = ERL_MESSAGE_TOKEN(msgp);
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
	 msg = ERL_MESSAGE_TERM(msgp);
	 seq_trace_output(SEQ_TRACE_TOKEN(c_p), msg, SEQ_TRACE_RECEIVE, 
			  c_p->id, c_p);
     }
     UNLINK_MESSAGE(c_p, msgp);
     JOIN_MESSAGE(c_p);
     CANCEL_TIMER(c_p);
     free_message(msgp);
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
     *
     * Note: In order to keep the compatibility between 32 and 64 bits
     * emulators, only timeout values that can be represented in 32 bits
     * (unsigned) or less are allowed.
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
#if !defined(ARCH_64)
	 Uint time_val;
#endif

	 if (is_small(timeout_value) && signed_val(timeout_value) > 0 &&
#if defined(ARCH_64)
	     ((unsigned_val(timeout_value) >> 32) == 0)
#else
	     1
#endif
	     ) {
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
#if !defined(ARCH_64)
	 } else if (term_to_Uint(timeout_value, &time_val)) {
	     c_p->def_arg_reg[0] = (Eterm) (I+3);
	     set_timer(c_p, time_val);
#endif
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
	  * instruction following the wait_timeout instruction.
	  */

	 OpCase(wait_f):

	 wait2: {
	     c_p->i = (Eterm *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;
	     c_p->status = P_WAITING;
	     c_p->current = NULL;
	     goto do_schedule;
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
	 Eterm val;
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
	 index = (Uint) (signed_val(index) - Arg(3));
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
	Eterm (*bf)(Process*, Eterm);
	Eterm arg;
	Eterm result;

	GetArg1(2, arg);
	bf = (BifFunction) Arg(1);
	SAVE_HTOP;
	c_p->fcalls = FCALLS;
	result = (*bf)(c_p, arg);
	FCALLS = c_p->fcalls;
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
	Eterm (*bf)(Process*, Eterm);

	Eterm arg;
	Eterm result;

	GetArg1(1, arg);
	bf = (BifFunction) Arg(0);
	SAVE_HTOP;
	c_p->fcalls = FCALLS;
	result = (*bf)(c_p, arg);
	FCALLS = c_p->fcalls;
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
  * Guards bifs and, or, xor in guards.
  */
 OpCase(i_bif2_fbd):
    {
	Eterm (*bf)(Process*, Eterm, Eterm);
	Eterm result;

	bf = (BifFunction) Arg(1);
	SAVE_HTOP;
	c_p->fcalls = FCALLS;
	result = (*bf)(c_p, tmp_arg1, tmp_arg2);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	SET_I((Eterm *) Arg(0));
	Goto(*I);
    }

 /*
  * Guards bifs and, or, xor, relational operators in body.
  */
 OpCase(i_bif2_body_bd):
    {
	Eterm (*bf)(Process*, Eterm, Eterm);
	Eterm result;

	bf = (BifFunction) Arg(0);
	SAVE_HTOP;
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
     */
 OpCase(call_bif0_e):
    {
	Eterm (*bf)(Process*, Uint*) = GET_BIF_ADDRESS(Arg(0));

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
#ifdef HYBRID
        SWAPIN;
#else
	HTOP = HEAP_TOP(c_p);
#endif
	CHECK_TERM(r(0));
	Next(1);
    }

 OpCase(call_bif1_e):
    {
	Eterm (*bf)(Process*, Eterm, Uint*) = GET_BIF_ADDRESS(Arg(0));
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
#ifdef HYBRID
        SWAPIN;
#else
	HTOP = HEAP_TOP(c_p);
#endif
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
	Eterm (*bf)(Process*, Eterm, Eterm, Uint*) = GET_BIF_ADDRESS(Arg(0));
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
#ifdef HYBRID
        SWAPIN;
#else
	HTOP = HEAP_TOP(c_p);
#endif
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
	Eterm (*bf)(Process*, Eterm, Eterm, Eterm, Uint*) = GET_BIF_ADDRESS(Arg(0));
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
#ifdef HYBRID
        SWAPIN;
#else
	HTOP = HEAP_TOP(c_p);
#endif
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
	    SET_I(((Export *)(c_p->def_arg_reg[0]))->address);
	    SWAPIN;
	    r(0) = c_p->def_arg_reg[1];
	    x(1) = c_p->def_arg_reg[2];
	    x(2) = c_p->def_arg_reg[3];
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

     SAVE_HTOP;
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

     SAVE_HTOP;
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
     SAVE_HTOP;
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
	 SAVE_HTOP;
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
     SAVE_HTOP;
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
     SAVE_HTOP;
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
     SAVE_HTOP;
     result = erts_bxor(c_p, tmp_arg1, tmp_arg2);
     if (is_value(result)) {
	 StoreBifResult(1, result);
     }
     goto lb_Cl_error;
 }

 {
     Sint i;
     Sint ires;
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
		     if ((ires > 0 && ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			 ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
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
			 ires = 3; /* ??? */
		     else
			 ires -= (-i / D_EXP);
		 }
		 bigp = BeamArithAlloc(c_p, BIG_NEED_SIZE(ires+1));

		 tmp_arg1 = big_lshift(tmp_arg1, i, bigp);
		 if (is_nil(tmp_arg1)) {
		 system_limit:
		     c_p->freason = SYSTEM_LIMIT;
		     goto lb_Cl_error;
		 }
                 MAYBE_SHRINK(c_p,bigp,tmp_arg1,BIG_NEED_SIZE(ires+1));
		 ArithCheck(c_p);
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

 OpCase(apply_I): {
     Eterm* next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     HTOP = HEAP_TOP(c_p);
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(apply_last_IP): {
     Eterm* next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     HTOP = HEAP_TOP(c_p);
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (Eterm *) E[0]);
	 E = ADD_BYTE_OFFSET(E, Arg(1));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     Eterm* next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     HTOP = HEAP_TOP(c_p);
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
     HTOP = HEAP_TOP(c_p);
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
     HTOP = HEAP_TOP(c_p);
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
     HTOP = HEAP_TOP(c_p);
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
     HTOP = HEAP_TOP(c_p);
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
	 Uint size = c_p->arity * sizeof(c_p->arg_reg[0]);
	 if (c_p->arg_reg != c_p->def_arg_reg) {
	     ERTS_PROC_LESS_MEM(c_p->max_arg_reg * sizeof(c_p->arg_reg[0]));
	     ERTS_PROC_MORE_MEM(size);
	     c_p->arg_reg = (Eterm *) erts_realloc(ERTS_ALC_T_ARG_REG,
						   (void *) c_p->arg_reg,
						   size);
	 } else {
	     ERTS_PROC_MORE_MEM(size);
	     c_p->arg_reg = (Eterm *) erts_alloc(ERTS_ALC_T_ARG_REG, size);
	 }
	 c_p->max_arg_reg = c_p->arity;
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
     goto do_schedule1;
 }

#ifdef HEAP_FRAG_ELIM_TEST
 OpCase(i_put_float_od):
 {
     Eterm f = make_float(&Arg(0));
     StoreBifResult(3, f);
 }

 OpCase(i_fetch_float1_o):
 {
     tmp_arg1 = make_float(&Arg(0));
     Next(3);
 }

 OpCase(i_fetch_float2_o):
 {
     tmp_arg2 = make_float(&Arg(0));
     Next(3);
 }
#else
 OpCase(i_put_float_od):
 {
     Eterm* hp = BeamArithAlloc(c_p, FLOAT_SIZE_OBJECT);
     Eterm f = make_float(hp);

     PUT_DOUBLE(*(FloatDef*)&Arg(1), hp);
     StoreBifResult(3, f);
 }

 OpCase(i_fetch_float1_o):
 {
     Eterm* hp = BeamArithAlloc(c_p, FLOAT_SIZE_OBJECT);
     tmp_arg1 = make_float(hp);

     PUT_DOUBLE(*(FloatDef*)&Arg(1), hp);
     Next(3);
 }

 OpCase(i_fetch_float2_o):
 {
     Eterm* hp = BeamArithAlloc(c_p, FLOAT_SIZE_OBJECT);
     tmp_arg2 = make_float(hp);

     PUT_DOUBLE(*(FloatDef*)&Arg(1), hp);
     Next(3);
 }
#endif

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

#ifdef HEAP_FRAG_ELIM_TEST
 /*
  * Arg(0): N = Thing word (tag, sign, number of words)
  * Arg(1..N): Value
  * Arg(N+1): Destination register
  */
  
 OpCase(i_put_big_wd):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm big = make_big(&Arg(0));
     StoreBifResult(size+1, big);
 }

 OpCase(i_fetch_big1_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     tmp_arg1 = make_big(&Arg(0));
     Next(size+1);
 }

 OpCase(i_fetch_big2_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     tmp_arg2 = make_big(&Arg(0));
     Next(size+1);
 }
#else
 /*
  * Arg(0): N = Thing word (tag, sign, number of words)
  * Arg(1..N): Value
  * Arg(N+1): Destination register
  */
  
 OpCase(i_put_big_wd):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = BeamArithAlloc(c_p, size+1);
     Eterm big = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     StoreBifResult(size+1, big);
 }

 OpCase(i_fetch_big1_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = BeamArithAlloc(c_p, size+1);
     tmp_arg1 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     Next(size+1);
 }

 OpCase(i_fetch_big2_w):
 {
     Eterm thing = Arg(0);
     Uint size = thing_arityval(thing);
     Eterm* hp = BeamArithAlloc(c_p, size+1);
     tmp_arg2 = make_big(hp);

     hp[0] = thing;
     memcpy(hp+1, &Arg(1), size*sizeof(Eterm));
     Next(size+1);
 }
#endif

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
	    while ((arity = bigp[0]) > given_arity) {
		bigp += thing_arityval(arity) + 2;
	    }
	    while (bigp[0] == given_arity) {
		if (memcmp(bigp+1, given+1, sizeof(Eterm)*given_size) == 0) {
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

#ifdef ARCH_64
 OpCase(i_select_float_sfI):
 {
     Uint f;
     int n;
     struct ValLabel {
	 Uint f;
	 Eterm* addr;
     };
     struct ValLabel* ptr;

     GetArg1(0, tmp_arg1);
     ASSERT(is_float(tmp_arg1));
     f = float_val(tmp_arg1)[1];
     n = Arg(2);
     ptr = (struct ValLabel *) &Arg(3);
     while (n-- > 0) {
	 if (ptr->f == f) {
	     SET_I(ptr->addr);
	     Goto(*I);
	 }
	 ptr++;
     }
     SET_I((Eterm *) Arg(1));
     Goto(*I);
 }
#else
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
#endif

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
	Eterm* bigp = BeamArithAlloc(c_p, BIG_NEED_SIZE(big_size(tmp_arg1)+1));
	tmp_arg1 = big_bnot(tmp_arg1, bigp);
        MAYBE_SHRINK(c_p,bigp,tmp_arg1,BIG_NEED_SIZE(big_size(tmp_arg1)+1));
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
     goto do_schedule;
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
     goto do_schedule;
 }

 OpCase(raise_ss): {
     /* This was not done very well in R10-0; then, we passed the tag in
	the first argument and hoped that the existing c_p->ftrace was
	still correct. But the ftrace-object already includes the tag
	(or rather, the freason). Now, we pass the original ftrace in
	the first argument. We also handle atom tags in the first
	argument for backwards compatibility.
     */
     GetArg2(0, tmp_arg1, tmp_arg2);
     c_p->fvalue = tmp_arg2;
     if (c_p->freason == EXC_NULL) {
       /* a safety check for the R10-0 case; should not happen */
       c_p->ftrace = NIL;
       c_p->freason = EXC_ERROR;
     }
     /* for R10-0 code, keep existing c_p->ftrace and hope it's correct */
     switch (tmp_arg1) {
     case am_throw:
       c_p->freason = EXC_THROWN & ~EXF_SAVETRACE;
       break;
     case am_error:
       c_p->freason = EXC_ERROR & ~EXF_SAVETRACE;
       break;
     case am_exit:
       c_p->freason = EXC_EXIT & ~EXF_SAVETRACE;
       break;
     default:
       {/* R10-1 and later
	   XXX note: should do sanity check on given trace if it can be
	   passed from a user! Currently only expecting generated calls.
	*/
	 struct StackTrace *s;
	 c_p->ftrace = tmp_arg1;
	 s = get_trace_from_exc(tmp_arg1);
	 if (s == NULL) {
	   c_p->freason = EXC_ERROR;
	 } else {
	   c_p->freason = PRIMARY_EXCEPTION(s->freason);
	 }
       }
     }
     goto find_func_info;
 }

 OpCase(badmatch_s): {
     GetArg1(0, tmp_arg1);
     c_p->fvalue = tmp_arg1;
     c_p->freason = BADMATCH;
 }
 /* Fall through here */

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
    HTOP = HEAP_TOP(c_p);
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
	 goto do_schedule;
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
	BifFunction vbf;

	c_p->current = I-3;	/* In case we apply process_info/1,2. */
	c_p->i = I;		/* In case we apply check_process_code/2. */
	c_p->arity = 0;		/* To allow garbage collection on ourselves
				 * (check_process_code/2).
				 */
				   
	SWAPOUT;
	c_p->fcalls = FCALLS - 1;
	vbf = (BifFunction) Arg(0);
	ASSERT(I[-1] <= 3);
	switch (I[-1]) {
	case 3:
	    {
		Eterm (*bf)(Process*, Eterm, Eterm, Eterm, Uint*) = vbf;
		tmp_arg1 = (*bf)(c_p, r(0), x(1), x(2), I);
	    }
	    break;
	case 2:
	    {
		Eterm (*bf)(Process*, Eterm, Eterm, Uint*) = vbf;
		tmp_arg1 = (*bf)(c_p, r(0), x(1), I);
	    }
	    break;
	case 1:
	    {
		Eterm (*bf)(Process*, Eterm, Uint*) = vbf;
		tmp_arg1 = (*bf)(c_p, r(0), I);
	    }
	    break;
	case 0:
	    {
		Eterm (*bf)(Process*, Uint*) = vbf;
		tmp_arg1 = (*bf)(c_p, I);
		break;
	    }
	}
	FCALLS = c_p->fcalls;
	SWAPIN;			/* There might have been a garbage collection. */
	if (is_value(tmp_arg1)) {
	    r(0) = tmp_arg1;
#ifdef HEAP_FRAG_ELIM_TEST
	    TestHeap(1, 1);
#endif
	    CHECK_TERM(r(0));
	    SET_I(c_p->cp);
	    Goto(*I);
	} else if (c_p->freason == RESCHEDULE) {
	    Eterm* argp = c_p->arg_reg;

	    c_p->arity = I[-1];
	    argp[0] = r(0);
	    argp[1] = x(1);
	    argp[2] = x(2);
	    SWAPOUT;
	    c_p->i = I;
	    c_p->current = NULL;
#ifdef HEAP_FRAG_ELIM_TEST
	    if (MBUF(c_p) != NULL) {
		erts_garbage_collect(c_p, 0, argp, c_p->arity);
	    }
#endif
	    goto do_schedule;
	} else if (c_p->freason == TRAP) {
	    SET_I(((Export *)(c_p->def_arg_reg[0]))->address);
#ifdef HEAP_FRAG_ELIM_TEST
	    if (MBUF(c_p) != NULL) {
		/* SWAPOUT done above */
		FCALLS -= erts_garbage_collect(c_p, 0, c_p->def_arg_reg+1, I[-1]);
		SWAPIN;
	    }
#endif
	    r(0) = c_p->def_arg_reg[1];
	    x(1) = c_p->def_arg_reg[2];
	    x(2) = c_p->def_arg_reg[3];
	    Dispatch();
	}
	reg[0] = r(0);
	I = handle_error(c_p, c_p->cp, reg, vbf);
	goto post_error_handling;
    }

 OpCase(i_get_sd):
    {
	Eterm arg;
	Eterm result;

	GetArg1(0, arg);
	result = erts_pd_hash_get(c_p, arg);
	StoreBifResult(1, result);
    }

 OpCase(i_put_tuple_only_Ad): {
     tmp_arg1 = make_tuple(HTOP);
     *HTOP++ = Arg(0);
     StoreBifResult(1, tmp_arg1);
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

 OpCase(try_case_end_s):
    GetArg1(0, tmp_arg1);
    c_p->fvalue = tmp_arg1;
    c_p->freason = EXC_TRY_CLAUSE;
    goto find_func_info;

 /*
  * Construction of binaries using new instructions.
  */

 {
     OpCase(i_bs_init_fail_heap_IjId): {
	 /* tmp_arg1 was fetched by an i_fetch instruction */
	 tmp_arg2 = Arg(0);
	 I++;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_rjId): {
	 tmp_arg1 = r(0);
	 tmp_arg2 = 0;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_yjId): {
	 tmp_arg1 = yb(Arg(0));
	 tmp_arg2 = 0;
	 I++;
	 goto do_bs_init;
     }

     OpCase(i_bs_init_fail_xjId): {
	 tmp_arg1 = xb(Arg(0));
	 tmp_arg2 = 0;
	 I++;
     }
	 /* FALL THROUGH */
     do_bs_init:
	 if (is_not_small(tmp_arg1)) {
	     goto badarg;
	 } else {
	     Sint size = signed_val(tmp_arg1);

	     if (size < 0) {
		 goto badarg;
	     }
	     tmp_arg1 = (Eterm) size;
	 }

	 if (tmp_arg1 <= ERL_ONHEAP_BIN_LIMIT) {
	     goto do_heap_bin_alloc;
	 } else {
	     goto do_proc_bin_alloc;
	 }


     OpCase(i_bs_init_heap_IIId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = Arg(1);
	 I++;
	 goto do_proc_bin_alloc;
     }

     OpCase(i_bs_init_IId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = 0;
     }
     /* FALL THROUGH */
     do_proc_bin_alloc: {
	 Binary* bptr;
	 ProcBin* pb;

	 erts_bin_offset = 0;
	 TestHeap(tmp_arg2 + PROC_BIN_SIZE, Arg(1));

	 /*
	  * Allocate the binary struct itself.
	  */
	 bptr = erts_bin_nrml_alloc(tmp_arg1);
	 bptr->flags = 0;
	 bptr->orig_size = tmp_arg1;
	 bptr->refc = 1;
	 erts_current_bin = (byte *) bptr->orig_bytes;

	 /*
	  * Now allocate the ProcBin on the heap.
	  */
	 pb = (ProcBin *) HTOP;
	 HTOP += PROC_BIN_SIZE;
	 pb->thing_word = HEADER_PROC_BIN;
	 pb->size = tmp_arg1;
	 pb->next = MSO(c_p).mso;
	 MSO(c_p).mso = pb;
	 pb->val = bptr;
	 pb->bytes = bptr->orig_bytes;

	 MSO(c_p).overhead += pb->size / BINARY_OVERHEAD_FACTOR / sizeof(Eterm);
	 StoreBifResult(2, make_binary(pb));
     }

     OpCase(i_bs_init_heap_bin_heap_IIId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = Arg(1);
	 I++;
	 goto do_heap_bin_alloc;
     }

     OpCase(i_bs_init_heap_bin_IId): {
	 tmp_arg1 = Arg(0);
	 tmp_arg2 = 0;
     }
     /* Fall through */
     do_heap_bin_alloc:
	 {
	     ErlHeapBin* hb;
	     Uint bin_need;

	     bin_need = heap_bin_size(tmp_arg1);
	     erts_bin_offset = 0;
	     TestHeap(bin_need+tmp_arg2, Arg(1));
	     hb = (ErlHeapBin *) HTOP;
	     HTOP += bin_need;
	     hb->thing_word = header_heap_bin(tmp_arg1);
	     hb->size = tmp_arg1;
	     erts_current_bin = (byte *) hb->data;
	     tmp_arg1 = make_binary(hb);
	     StoreBifResult(2, tmp_arg1);
	 }
 }

 OpCase(i_bs_bits_to_bytes_rjd): {
     tmp_arg1 = r(0);
     goto do_bits_to_bytes;
 }

 OpCase(i_bs_bits_to_bytes_yjd): {
     tmp_arg1 = yb(Arg(0));
     I++;
     goto do_bits_to_bytes;

 OpCase(i_bs_bits_to_bytes_xjd): {
     tmp_arg1 = xb(Arg(0));
     I++;
 }

 do_bits_to_bytes:
     {
	 if (is_not_valid_bit_size(tmp_arg1)) {
	     goto lb_Cl_error;
	 }
	 tmp_arg1 = make_small(unsigned_val(tmp_arg1) >> 3);
	 StoreBifResult(1, tmp_arg1);
     }
 }

 OpCase(i_bs_add_jId): {
     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Uint Unit = Arg(1);
	 Sint Arg1 = signed_val(tmp_arg1);
	 Sint Arg2 = signed_val(tmp_arg2);

	 if (Arg1 >= 0 && Arg2 >= 0) {
	     Sint res = Arg1 + Unit*Arg2;

	     if (MY_IS_SSMALL(res)) {
		 Eterm result = make_small(res);
		 StoreBifResult(2, result);
	     }
	 }
     }

     /*
      * Fall through to here if there were any errors in the arguments.
      */
     goto badarg;
 }

 OpCase(i_new_bs_put_string_II):
    {
	Eterm* next;
	PreFetch(2, next);
	erts_new_bs_put_string((byte *) Arg(1), Arg(0));
	NextPF(2, next);
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
	 flags = erts_call_trace(c_p, ep->code, ep->match_prog_set, reg,
				 0, &c_p->tracer_proc);
	 SWAPIN;
	 
	 if (flags & MATCH_SET_RETURN_TRACE) {
	     static void* return_trace[1] = {OpCode(return_trace)};

#ifdef SHARED_HEAP
             AllocateStack(3);
#else
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
	     if (E - 3 < HTOP) {
		 /* SWAPOUT, SWAPIN was done and r(0) was saved above */
		 FCALLS -= erts_garbage_collect(c_p, 3, reg, ep->code[2]);
		 r(0) = reg[0];
		 SWAPIN;
	     }
	     E -= 3;
	     ASSERT(c_p->htop <= E && E <= c_p->hend);
#endif
	     ASSERT(is_CP((Eterm)(ep->code)));
	     ASSERT(is_internal_pid(c_p->tracer_proc) || 
		    is_internal_port(c_p->tracer_proc));
	     E[2] = make_cp(c_p->cp);
	     E[1] = am_true; /* Process tracer */
	     E[0] = make_cp(ep->code);
	     c_p->cp = (Eterm *) make_cp((Uint*)return_trace);
	 }
     }
     SET_I((Uint *) Arg(0));
     Dispatch();
 }

 OpCase(return_trace): {
     Uint* code = (Uint *) E[0];
     Eterm tracer_pid = E[1];
     
     SWAPOUT;		/* Needed for shared heap */
     erts_trace_return(c_p, code, r(0), &tracer_pid);
     SWAPIN;
     SET_I((Eterm *) E[2]);
     E += 3;
     Goto(*I);
 }

 OpCase(i_count_breakpoint): {
     Uint real_I;
     
     ErtsCountBreak((Uint *) I, &real_I);
     ASSERT(VALID_INSTR(real_I));
     Goto(real_I);
 }

 OpCase(i_trace_breakpoint):
     if (! IS_TRACED_FL(c_p, F_TRACE_CALLS)) {
	 Uint real_I;
	 
	 ErtsBreakSkip((Uint *) I, &real_I);
	 Goto(real_I);
     }
 /* Fall through to next case */
 OpCase(i_mtrace_breakpoint): {
     Uint real_I;
     Uint32 flags;
     Eterm tracer_pid;
     Uint *cpp;
     flags = 0;
     SWAPOUT;
     reg[0] = r(0);

     if (*cp_val((Eterm)c_p->cp) 
	 == (Uint) OpCode(return_trace)) {
	 cpp = (Uint*)&E[2];
     } else if (*cp_val((Eterm)c_p->cp) 
		== (Uint) OpCode(i_return_to_trace)) {
	 cpp = (Uint*)&E[0];
     } else {
	 cpp = NULL;
     }
     if (cpp) {
	 Eterm *cp_save = c_p->cp;
	 for (;;) {
	     ASSERT(is_CP(*cpp));
	     if (*cp_val(*cpp) == (Uint) OpCode(return_trace)) {
		 cpp += 3;
	     } else if (*cp_val(*cpp) == (Uint) OpCode(i_return_to_trace)) {
		 cpp += 1;
	     } else
		 break;
	 }
	 c_p->cp = (Eterm *) *cpp;
	 ASSERT(is_CP((Eterm)c_p->cp));
	 real_I = erts_trace_break(c_p, I, reg, &flags, &tracer_pid);
	 SWAPIN;		/* Needed by shared heap. */
	 c_p->cp = cp_save;
     } else {
	 real_I = erts_trace_break(c_p, I, reg, &flags, &tracer_pid);
	 SWAPIN;		/* Needed by shared heap. */
     }

     if ((flags & MATCH_SET_RETURN_TO_TRACE)) {
	 static void* return_to_trace[1] = {OpCode(i_return_to_trace)};
	 if ((Uint) c_p->cp != make_cp((Uint*)return_to_trace)) {
	     /* Look down the stack for other return_to frames */
	     int do_insert = 1;
	     if (*cp_val((Eterm)c_p->cp) == (Uint) OpCode(return_trace)) {
		 cpp = (Uint*)&E[2];
		 for(;;) {
		     ASSERT(is_CP(*cpp));
		     if (*cp_val(*cpp) == 
			 (Uint) OpCode(return_trace)) {
			 cpp += 3;
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
#ifdef SHARED_HEAP
		 AllocateStack(1);
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

#ifdef SHARED_HEAP
	 AllocateStack(3);
#else
	 ASSERT(c_p->htop <= E && E <= c_p->hend);
	 if (E - 3 < HTOP) {
	     /* Stack pointer may have been changed by 
		return_to trace above */
	     SWAPOUT; 
	     FCALLS -= erts_garbage_collect(c_p, 3, reg, I[-1]);
	     r(0) = reg[0];
	     SWAPIN;
	 }
	 E -= 3;
	 ASSERT(c_p->htop <= E && E <= c_p->hend);
#endif
	 ASSERT(is_CP((Eterm) (I - 3)));
	 ASSERT(am_true == tracer_pid || 
		is_internal_pid(tracer_pid) || is_internal_port(tracer_pid));
	 E[2] = make_cp(c_p->cp);
	 E[1] = tracer_pid;
	 E[0] = make_cp(I - 3); /* We ARE at the beginning of an 
				   instruction,
				   the funcinfo is above i. */
	 c_p->cp = (Eterm *) make_cp((Uint*)return_trace);
     }
     Goto(real_I);
 }
 
 OpCase(i_return_to_trace): {
     Uint *cpp = (Uint*)&E[0];
     for(;;) {
	 ASSERT(is_CP(*cpp));
	 if (*cp_val(*cpp) == (Uint) OpCode(return_trace)) {
	     cpp += 3;
	 } else {
	     break;
	 }
     }
     if (IS_TRACED_FL(c_p, F_TRACE_RETURN_TO)) {
	 SWAPOUT;		/* Needed for shared heap */
	 erts_trace_return_to(c_p, cp_val(*cpp));
	 SWAPIN;
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
     HTOP = HEAP_TOP(c_p);
     SET_I(c_p->cp);
     Goto(*I);
 }

 OpCase(i_module_info_1): {
     Eterm res;

     SWAPOUT;
     res = erts_module_info_1(c_p, I[-3], r(0));
     HTOP = HEAP_TOP(c_p);
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
  * Instructions for allocating on the message area.
  */

 OpCase(i_global_cons):
 {
     Eterm *next;
#ifdef HYBRID
     Eterm *hp;

     PreFetch(0,next);
     TestGlobalHeap(2,2,hp);
     hp[0] = r(0);
     hp[1] = x(1);
     r(0) = make_list(hp);
#ifndef INCREMENTAL_GC
     global_htop += 2;
#endif
     NextPF(0,next);
#else
     PreFetch(0,next);
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
 }

 OpCase(i_global_tuple):
 {
     Eterm *next;
     int len;
#ifdef HYBRID
     Eterm list;
     Eterm *hp;
#endif

     if ((len = list_length(r(0))) < 0) {
         goto badarg;
     }

     PreFetch(0,next);
#ifdef HYBRID
     TestGlobalHeap(len + 1,1,hp);
     list = r(0);
     r(0) = make_tuple(hp);
     *hp++ = make_arityval(len);
     while(is_list(list))
     {
         Eterm* cons = list_val(list);
         *hp++ = CAR(cons);
         list = CDR(cons);
     }
#ifndef INCREMENTAL_GC
     global_htop += len + 1;
#endif
     NextPF(0,next);
#else
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
 }

 OpCase(i_global_copy):
 {
     Eterm *next;
     PreFetch(0,next);
#ifdef HYBRID
     if (!IS_CONST(r(0)))
     {
         BM_SWAP_TIMER(system,copy);
         SWAPOUT;
         reg[0] = r(0);
         reg[1] = NIL;
         r(0) = copy_struct_lazy(c_p,r(0),0);
         ASSERT(copy_src_top == 0);
         ASSERT(copy_dst_top == 0);
         ASSERT(copy_offset_top == 0);
         SWAPIN;
         BM_SWAP_TIMER(copy,system);
     }
     NextPF(0,next);
#else
     c_p->freason = EXC_INTERNAL_ERROR;
     goto find_func_info;
#endif
 }

 /*
  * New floating point instructions.
  */

 OpCase(fmove_ol): {
     Eterm fr = Arg(3);
     Eterm* next;

     PreFetch(4, next);
     GET_DOUBLE_DATA(&Arg(1), *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(4, next);
 }
 OpCase(fmove_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     Eterm* next;

     PreFetch(2, next);
     GetR(0, targ1);
     /* Arg(0) == HEADER_FLONUM */
     GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(2, next);
 }

 OpCase(fmove_new_ld): {
     Eterm fr = Arg(0);
     Eterm dest = make_float(HTOP);

     PUT_DOUBLE(*(FloatDef*)ADD_BYTE_OFFSET(freg, fr), HTOP);
     HTOP += FLOAT_SIZE_OBJECT;
     StoreBifResult(1, dest);
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
	 GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     } else {
	 goto fbadarith;
     }
     NextPF(2, next);
 }

 /*
  * Old allocating fmove.
  */

 OpCase(fmove_old_ld): {
     Eterm* hp = BeamArithAlloc(c_p, FLOAT_SIZE_OBJECT);
     Eterm dest = make_float(hp);
     Eterm fr = Arg(0);
     Eterm* next;

     ArithCheck(c_p);
     PreFetch(2, next);
     PUT_DOUBLE(*(FloatDef*)ADD_BYTE_OFFSET(freg, fr), hp);
     StoreResult(dest, Arg(1));
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
	 ++hipe_trap_count;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_call_closure): {
       ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.ncallee = (void(*)(void)) I[-4];
       cmd = HIPE_MODE_SWITCH_CMD_CALL_CLOSURE | (I[-1] << 8);
       ++hipe_trap_count;
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
     /* XXX: this abuse of def_arg_reg[] is horrid! */
     SWAPOUT;
     c_p->fcalls = FCALLS;
     c_p->def_arg_reg[4] = -neg_o_reds;
     reg[0] = r(0);
     c_p = hipe_mode_switch(c_p, cmd, reg);
     neg_o_reds = -c_p->def_arg_reg[4];
     FCALLS = c_p->fcalls;
     SWAPIN;
     switch( c_p->def_arg_reg[3] ) {
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 MoveReturn(reg[0], r(0));
       case HIPE_MODE_SWITCH_RES_CALL:
	 SET_I(c_p->i);
	 r(0) = reg[0];
	 Dispatch();
       case HIPE_MODE_SWITCH_RES_THROW:
	 c_p->cp = NULL;
	 I = handle_error(c_p, I, reg, NULL);
	 goto post_error_handling;
       default:
	 erl_exit(1, "hipe_mode_switch: result %u\n", c_p->def_arg_reg[3]);
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
     goto do_schedule;
 }

 OpCase(i_hibernate): {
     SWAPOUT;
     if (hibernate(c_p, r(0), x(1), x(2), reg)) {
	 goto do_schedule;
     } else {
	 I = handle_error(c_p, I, reg, hibernate_3);
	 goto post_error_handling;
     }
 }

 OpCase(i_debug_breakpoint): {
     SWAPOUT;
     reg[0] = r(0);
     tmp_arg1 = call_breakpoint_handler(c_p, I-3, reg);
     r(0) = reg[0];
     HTOP = HEAP_TOP(c_p);
     if (tmp_arg1) {
	 SET_I(c_p->i);
	 Dispatch();
     }
     goto no_error_handler;
 }

 /*
  * Construction of binaries using old instructions.
  */

 OpCase(i_bs_init_old): {
     Eterm *next;
     PreFetch(0, next);
     erts_bin_offset = 0;
     NextPF(0, next);
 }

 OpCase(i_bs_final_jd): {
     Eterm *next;
     Eterm b;

     PreFetch(2, next);
     SAVE_HTOP;
     c_p->fcalls = FCALLS;
     b = erts_bs_final(c_p);
     FCALLS = c_p->fcalls;
     if (is_non_value(b)) {
	 goto badarg;
     }
     StoreResult(b, Arg(1));
     NextPF(2, next);
 }

 OpCase(i_bs_put_string_II):
    {
	Eterm* next;
	PreFetch(2, next);
	erts_bs_put_string((byte *) Arg(1), Arg(0));
	NextPF(2, next);
    }


    DEFINE_COUNTING_LABELS;
#ifndef NO_JUMP_TABLE
#ifdef DEBUG
 end_emulator_loop:
#endif
#endif

 OpCase(int_code_end):
 OpCase(label_L):
 OpCase(too_old_compiler):
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
      *  If a BIF is applied and it fails, change the normal EXIT code
      *  to look as if the error occurred in function {M,F,A}.
      *  Also, the normal stack backtrace will be suppressed.
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
     beam_debug_apply[6] = (4 << BEAM_LOOSE_SHIFT) | 1;

     /* move {x,3} {y,0} */
     beam_debug_apply[7] = (Eterm) OpCode(move_xy);
     beam_debug_apply[8] = ((1 << BEAM_LOOSE_SHIFT) * sizeof(Eterm)) |
	 (3 * sizeof(Eterm));

     /* i_apply */
     beam_debug_apply[9] = (Eterm) OpCode(i_apply);

     /* deallocate_return 1 */
     beam_debug_apply[10] = (Eterm) OpCode(deallocate_return_P);
     beam_debug_apply[11] = (1+1)*sizeof(Eterm);

     return;
 }
#ifdef NO_JUMP_TABLE
 default:
    erl_exit(1, "unexpected op code %d\n",Go);
  }
#endif
    return;			/* Never executed */

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
 * Mapping from the error code 'class tag' to atoms.
 */
Eterm exception_tag[NUMBER_EXC_TAGS] = {
  am_error,	/* 0 */
  am_exit,	/* 1 */
  am_throw,	/* 2 */
};

/*
 * Mapping from error code 'index' to atoms.
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
  am_try_clause,	/* 16 */
};

/*
 * To fully understand the error handling, one must keep in mind that
 * when an exception is thrown, the search for a handler can jump back
 * and forth between Beam and native code. Upon each mode switch, a
 * dummy handler is inserted so that if an exception reaches that point,
 * the handler is invoked (like any handler) and transfers control so
 * that the search for a real handler is continued in the other mode.
 * Therefore, c_p->freason and c_p->fvalue must still hold the exception
 * info when the handler is executed, but normalized so that creation of
 * error terms and saving of the stack trace is only done once, even if
 * we pass through the error handling code several times.
 *
 * When a new exception is raised, the current stack trace information
 * is quick-saved in a small structure allocated on the heap. Depending
 * on how the exception is eventually caught (perhaps by causing the
 * current process to terminate), the saved information may be used to
 * create a symbolic (human-readable) representation of the stack trace
 * at the point of the original exception.
 */

static Eterm*
handle_error(Process* c_p, Eterm* pc, Eterm* reg, BifFunction bf)
{
    Eterm* hp;
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;
    c_p->i = pc;    /* In case we call erl_exit(). */

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */
    ASSERT(c_p->freason != RESCHEDULE); /* Should have been handled earlier. */

    /*
     * Check if we have an arglist for the top level call. If so, this
     * is encoded in Value, so we have to dig out the real Value as well
     * as the Arglist.
     */
    if (c_p->freason & EXF_ARGLIST) {
	  Eterm* tp;
	  ASSERT(is_tuple(Value));
	  tp = tuple_val(Value);
	  Value = tp[1];
	  Args = tp[2];
    }

    /*
     * Save the stack trace info if the EXF_SAVETRACE flag is set. The
     * main reason for doing this separately is to allow throws to later
     * become promoted to errors without losing the original stack
     * trace, even if they have passed through one or more catch and
     * rethrow. It also makes the creation of symbolic stack traces much
     * more modular.
     */
    if (c_p->freason & EXF_SAVETRACE) {
        save_stacktrace(c_p, pc, reg, bf, Args);
    }

    /*
     * Throws that are not caught are turned into 'nocatch' errors
     */
    if ((c_p->freason & EXF_THROWN) && (c_p->catches <= 0) ) {
	hp = HAlloc(c_p, 3);
        Value = TUPLE2(hp, am_nocatch, Value);
        c_p->freason = EXC_ERROR;
    }

    /* Get the fully expanded error term */
    Value = expand_error_value(c_p, c_p->freason, Value);

    /* Save final error term and stabilize the exception flags so no
       further expansion is done. */
    c_p->fvalue = Value;
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Find a handler or die */
    if ((c_p->catches <= 0) || (c_p->freason & EXF_PANIC)) {
	terminate_proc(c_p, Value);
    } else {
        /* The Beam handler code (catch_end or try_end) checks reg[0]
	   for THE_NON_VALUE to see if the previous code finished
	   abnormally. If so, reg[1], reg[2] and reg[3] should hold the
	   exception class, term and trace, respectively. (If the
	   handler is just a trap to native code, these registers will
	   be ignored.) */
	reg[0] = THE_NON_VALUE;
	reg[1] = exception_tag[GET_EXC_CLASS(c_p->freason)];
	reg[2] = Value;
	reg[3] = c_p->ftrace;
        return next_catch(c_p);
    }
    return 0;
}

/*
 * Find the nearest catch handler, assuming there is one
 */
static Eterm*
next_catch(Process* c_p) {
    Eterm* pc;
    Eterm* ptr;
    for (ptr = c_p->stop + CP_SIZE; ptr < STACK_START(c_p); ptr++) {
        if (is_catch(*ptr)) {
	    pc = catch_pc(*ptr);
	    while (is_not_CP(*ptr)) {
	        ptr--;
		ASSERT(c_p->stop <= ptr);
	    }
	    c_p->stop = ptr;
	    return pc;
	}
    }
    erl_exit(1, "Catch not found");
}

/*
 * Terminating the process when an exception is not caught
 */
void terminate_proc(Process* c_p, Eterm Value) {
    /* Add a stacktrace if this is an error. */
    if (GET_EXC_CLASS(c_p->freason) == EXTAG_ERROR) {
        Value = add_stacktrace(c_p, Value, c_p->ftrace);
    }
    /* EXF_LOG is a primary exception flag */
    if (c_p->freason & EXF_LOG) {
        cerr_pos = 0;
	erl_printf(CBUF, "Error in process ");
	display(c_p->id, CBUF);
	if (erts_this_node->sysname != am_Noname) {
	    erl_printf(CBUF, " on node ");
	    print_atom(atom_val(erts_this_node->sysname), CBUF);
	}
	erl_printf(CBUF, " with exit value: ");
	ldisplay(Value, CBUF, display_items);
	erl_printf(CBUF, "\n");
	send_error_to_logger(c_p->group_leader);
    }
    /*
     * If we use a shared heap, the process will be garbage-collected.
     * Must zero c_p->arity to indicate that there are no live registers.
     */
    c_p->arity = 0;
    do_exit(c_p, Value);
}

/*
 * Build and add a symbolic stack trace to the error value.
 */
static Eterm
add_stacktrace(Process* c_p, Eterm Value, Eterm exc) {
    Eterm Where = build_stacktrace(c_p, exc);
    Eterm* hp = HAlloc(c_p, 3);
    return TUPLE2(hp, Value, Where);
}

/*
 * Forming the correct error value from the internal error code.
 * This does not update c_p->fvalue or c_p->freason.
 */
Eterm
expand_error_value(Process* c_p, Uint freason, Eterm Value) {
    Eterm* hp;
    Uint r;

    r = GET_EXC_INDEX(freason);
    ASSERT(r < NUMBER_EXIT_CODES); /* range check */
    ASSERT(is_value(Value));

    switch (r) {
    case (GET_EXC_INDEX(EXC_PRIMARY)):
        /* Primary exceptions use fvalue as it is */
	break;
    case (GET_EXC_INDEX(EXC_BADMATCH)):
    case (GET_EXC_INDEX(EXC_CASE_CLAUSE)):
    case (GET_EXC_INDEX(EXC_TRY_CLAUSE)):
    case (GET_EXC_INDEX(EXC_BADFUN)):
    case (GET_EXC_INDEX(EXC_BADARITY)):
        /* Some common exceptions: value -> {atom, value} */
        ASSERT(is_value(Value));
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, error_atom[r], Value);
	break;
    default:
        /* Other exceptions just use an atom as descriptor */
        Value = error_atom[r];
	break;
    }
#ifdef DEBUG
    ASSERT(Value != am_internal_error);
#endif
    return Value;
}

/*
 * Quick-saving the stack trace in an internal form on the heap. Note
 * that c_p->ftrace will point to a cons cell which holds the given args
 * and the saved data (encoded as a bignum).
 *
 * (It would be much better to put the arglist - when it exists - in the
 * error value instead of in the actual trace; e.g. '{badarg, Args}'
 * instead of using 'badarg' with Args in the trace. The arglist may
 * contain very large values, and right now they will be kept alive as
 * long as the stack trace is live. Preferably, the stack trace should
 * always be small, so that it does not matter if it is long-lived.
 * However, it is probably not possible to ever change the format of
 * error terms.)  */

static void
save_stacktrace(Process* c_p, Eterm* pc, Eterm* reg, BifFunction bf,
		Eterm args) {
    Eterm *hp;
    struct StackTrace* s;
    int sz;
    int depth = erts_backtrace_depth;    /* max depth (never negative) */
    if (depth > 0) {
	/* There will always be a current function */
	depth --;
    }

    /* Create a container for the exception data */
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm)*depth
          + sizeof(Eterm) - 1) / sizeof(Eterm);
    hp = HAlloc(c_p, 2 + 1 + sz);
    s = (struct StackTrace *) (hp + 2);
    c_p->ftrace = CONS(hp, args, make_big((Eterm *) s));
    /* The following fields are inside the bignum */
    s->header = make_pos_bignum_header(sz);
    s->freason = c_p->freason;
    s->depth = 0;

    /*
     * If the failure was in a BIF other than 'error', 'exit' or
     * 'throw', find the bif-table index and save the argument
     * registers by consing up an arglist.
     */
    if (bf != NULL && bf != error_1 && bf != error_2
	&& bf != fault_1 && bf != fault_2
	&& bf != exit_1 && bf != throw_1) {
        int i;
	int a = 0;
	for (i = 0; i < BIF_SIZE; i++) {
	    if (bf == bif_table[i].f || bf == bif_table[i].traced) {
		Export *ep = bif_export[i];
		s->current = ep->code;
	        a = bif_table[i].arity;
		break;
	    }
	}
	if (i >= BIF_SIZE) {
	    /* 
	     * The Bif does not really exist (no BIF entry).  It is a
	     * TRAP and traps are called through apply_bif, which also
	     * sets c_p->current (luckily).
	     */
	    ASSERT(c_p->current);
	    s->current = c_p->current;
	    a = s->current[2];
	    ASSERT(s->current[2] <= 3);
	}
	/* Save first stack entry */
	ASSERT(pc);
	if (depth > 0) {
	    s->trace[s->depth++] = pc;
	    depth--;
	}
	s->pc = NULL;
	*hp = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
    } else {
	s->current = c_p->current;
        /* 
	 * For a function_clause error, the arguments are in the beam
	 * registers, c_p->cp is valid, and c_p->current is set.
	 */
	if ( (GET_EXC_INDEX(s->freason)) ==
	     (GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) ) {
	    int a;
	    ASSERT(s->current);
	    a = s->current[2];
	    *hp = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
	    /* Save first stack entry */
	    ASSERT(c_p->cp);
	    if (depth > 0) {
		s->trace[s->depth++] = c_p->cp;
		depth--;
	    }
	    s->pc = NULL; /* Ignore pc */
	} else {
	    s->pc = pc;
	}
    }

    /* Save the actual stack trace */
    if (depth > 0) {
	Eterm *ptr, *prev = s->depth ? s->trace[s->depth-1] : NULL;
	/*
	 * Traverse the stack backwards and add all unique continuation
	 * pointers to the buffer, up to the maximum stack trace size.
	 */
	for (ptr = c_p->stop;
	     (ptr < STACK_START(c_p)) && (depth > 0); 
	     ptr++) {
	    if (is_CP(*ptr)) {
		Eterm *cp = (Eterm *)(*ptr);
		if (cp != prev) {
		    prev = cp;
		    s->trace[s->depth++] = cp;
		    depth--;
		}
	    }
	}
    }
    ASSERT((Eterm *) (s->trace+s->depth) <= hp+2+1+sz);
}

/*
 * Getting the relevant fields from the term pointed to by ftrace
 */

static struct StackTrace *get_trace_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NULL;
    } else {
	ASSERT(is_list(exc));
	return (struct StackTrace *) big_val(CDR(list_val(exc)));
    }
}

static Eterm get_args_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NIL;
    } else {
	ASSERT(is_list(exc));
	return CAR(list_val(exc));
    }
}

static int is_raised_exc(Eterm exc) {
    if (exc == NIL) {
        return 0;
    } else {
        ASSERT(is_list(exc));
        return bignum_header_is_neg(*big_val(CDR(list_val(exc))));
    }
}

/*
 * Creating a list with the argument registers
 */
static Eterm
make_arglist(Process* c_p, Eterm* reg, int a) {
    Eterm args = NIL;
    Eterm* hp = HAlloc(c_p, 2*a);
    while (a > 0) {
        args = CONS(hp, reg[a-1], args);
	hp += 2;
	a--;
    }
    return args;
}

/*
 * Building a symbolic representation of a saved stack trace. Note that
 * the exception object 'exc', unless NIL, points to a cons cell which
 * holds the given args and the quick-saved data (encoded as a bignum).
 *
 * If the bignum is negative, the given args is a complete stacktrace.
 */
Eterm
build_stacktrace(Process* c_p, Eterm exc) {
    struct StackTrace* s;
    Eterm  args;
    int    depth;
    Eterm* current;
    Eterm  Where = NIL;
    Eterm* next_p = &Where;

    if (! (s = get_trace_from_exc(exc))) {
        return NIL;
    }
    if (s->freason & EXF_NATIVE) {
      /* Just return a null trace if the exception was in native code.
       */
      return NIL;
    }
    if (is_raised_exc(exc)) {
	return get_args_from_exc(exc);
    }

    /*
     * Find the current function. If the saved s->pc is null, then the
     * saved s->current should already contain the proper value.
     */
    if (s->pc != NULL) {
	current = find_function_from_pc(s->pc);
    } else {
	current = s->current;
    }
    /*
     * If current is still NULL, default to the initial function
     * (e.g. spawn_link(erlang, abs, [1])).
     */
    if (current == NULL) {
	current = c_p->initial;
	args = am_true; /* Just in case */
    } else {
	args = get_args_from_exc(exc);
    }
    
    /*
     * Add the {M,F,A} for the current function 
     * (where A is arity or [Argument]).
     */
    {
	Eterm mfa;
	Eterm* hp = HAlloc(c_p, 6);
	if (args != am_true) {
	    /* We have an arglist - use it */
	    mfa = TUPLE3(hp, current[0], current[1], args);
	} else {
	    Eterm arity = make_small(current[2]);
	    mfa = TUPLE3(hp, current[0], current[1], arity);
	}
	hp += 4;
	ASSERT(*next_p == NIL);
	*next_p = CONS(hp, mfa, NIL);
	next_p = &CDR(list_val(*next_p));
    }
    depth = s->depth;
    /* Finally, we go through the saved continuation pointers 
     */
    if (depth > 0) {
        int i;
        Eterm *hp, *hp_end;

        hp = HAlloc(c_p, 6 * depth);
	hp_end = hp + 6*depth;
	for (i = 0; i < depth; i++) {
	    Eterm mfa;
	    Eterm *fi = find_function_from_pc((Eterm *) s->trace[i]);
	    if (fi == NULL) continue;
	    mfa = TUPLE3(hp, fi[0], fi[1], make_small(fi[2]));
	    hp += 4;
	    ASSERT(*next_p == NIL);
	    *next_p = CONS(hp, mfa, NIL);
	    next_p = &CDR(list_val(*next_p));
	    hp += 2;
	}
        HRelease(c_p, hp_end, hp);
    }
    return Where;
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
    Eterm tmp, this;

    /*
     * Check the arguments which should be of the form apply(Module,
     * Function, Arguments) where Function is an atom and
     * Arguments is an arity long list of terms.
     */
    if (is_not_atom(function)) {
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

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    this = THE_NON_VALUE;
    if (is_not_atom(module)) {
	Eterm* tp;

        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        this = module;
        module = tp[1];
        if (is_not_atom(module)) goto error;
    }
    
    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]). If the module argument
     * was an abstract module, add 1 to the function arity and put the
     * module argument in the n+1st x register as a THIS reference.
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < (MAX_REG - 1)) {
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
    if (this != THE_NON_VALUE) {
        reg[arity++] = this;
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
fixed_apply(Process* p, Eterm* reg, Uint arity)
{
    Export* ep;
    Eterm module;
    Eterm function;

    module = reg[arity];    /* The THIS pointer already in place */
    function = reg[arity+1];

    if (is_not_atom(function)) {
    error:
	p->freason = BADARG;
	reg[0] = module;
	reg[1] = function;
	reg[2] = NIL;
	return 0;
    }

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    if (is_not_atom(module)) {
	Eterm* tp;
        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        module = tp[1];
        if (is_not_atom(module)) goto error;
        ++arity;
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
	    int i;
	    Eterm* hp = HAlloc(p, 2*arity);
	    Eterm args = NIL;
	    
	    for (i = arity-1; i >= 0; i--) {
		args = CONS(hp, reg[i], args);
		hp += 2;
	    }
	    reg[0] = module;
	    reg[1] = function;
	    reg[2] = args;
	}
    } else if (p->ct != NULL) {
	save_calls(p, ep);
    }

    return ep->address;
}

static int
hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;
    int new_sz;

    if (is_not_atom(module) || is_not_atom(function)) {
	/*
	 * No need to test args here -- done below.
	 */
    error:
	c_p->freason = BADARG;

    error2:
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
	return 0;
    }

    arity = 0;
    tmp = args;
    while (is_list(tmp)) {
	if (arity < MAX_REG) {
	    tmp = CDR(list_val(tmp));
	    arity++;
	} else {
	    c_p->freason = SYSTEM_LIMIT;
	    goto error2;
	}
    }
    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	goto error;
    }

    /*
     * At this point, arguments are known to be good.
     */

    if (c_p->arg_reg != c_p->def_arg_reg) {
	/* Save some memory */
	ERTS_PROC_LESS_MEM(c_p->max_arg_reg * sizeof(c_p->arg_reg[0]));
	erts_free(ERTS_ALC_T_ARG_REG, c_p->arg_reg);
	c_p->arg_reg = c_p->def_arg_reg;
	c_p->max_arg_reg = sizeof(c_p->def_arg_reg)/sizeof(c_p->def_arg_reg[0]);
    }

    /*
     * Arrange for the process to be resumed at the given MFA with
     * the stack cleared.
     */
    c_p->arity = 3;
    c_p->arg_reg[0] = module;
    c_p->arg_reg[1] = function;
    c_p->arg_reg[2] = args;
    c_p->stop = STACK_START(c_p);
    c_p->catches = 0;
    c_p->i = beam_apply;
    c_p->cp = (Eterm *) beam_apply+1;

    /*
     * If there are no waiting messages, garbage collect and
     * shrink the heap. 
     */
    if (c_p->msg.len > 0) {
	add_to_schedule_q(c_p);
    } else {
	FLAGS(c_p) |= F_NEED_FULLSWEEP;
	c_p->fvalue = NIL;
	erts_garbage_collect(c_p, 0, c_p->arg_reg, c_p->arity);
	new_sz = HEAP_TOP(c_p) - HEAP_START(c_p);
	erts_shrink_new_heap(c_p, new_sz, c_p->arg_reg, c_p->arity);
	c_p->status = P_WAITING;
    }
    c_p->current = bif_export[BIF_hibernate_3]->code;
    return 1;
}

static Uint*
call_fun(Process* p,		/* Current process. */
	 int arity,		/* Number of arguments for Fun. */
	 Eterm* reg,		/* Contents of registers. */
	 Eterm args)		/* THE_NON_VALUE or pre-built list of arguments. */
{
    Eterm fun = reg[arity];
    Eterm hdr;
    int i;
    Eterm function;
    Eterm* hp;

    if (!is_boxed(fun)) {
	goto badfun;
    }
    hdr = *boxed_val(fun);

    if (is_fun_header(hdr)) {
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

	if (actual_arity == arity+num_free) {
	    if (num_free == 0) {
		return code_ptr;
	    } else {
		var_ptr = funp->env;
		reg += arity;
		i = 0;
		do {
		    reg[i] = var_ptr[i];
		    i++;
		} while (i < num_free);
		reg[i] = fun;
		return code_ptr;
	    }
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
    } else if (is_export_header(hdr)) {
	Export* ep = (Export *) (export_val(fun))[1];
	int actual_arity = (int) ep->code[2];
	if (arity == actual_arity) {
	    return ep->address;
	} else {
	    /*
	     * Wrong arity. First build a list of the arguments.
	     */  

	    if (is_non_value(args)) {
		args = NIL;
		hp = HAlloc(p, arity*2);
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    hp = HAlloc(p, 3);
	    p->freason = EXC_BADARITY;
	    p->fvalue = TUPLE2(hp, fun, args);
	    return NULL;
	}
    } else if (hdr == make_arityval(2)) {
	Eterm* tp;
	Export* ep;
	Eterm module;

	tp = tuple_val(fun);
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
    } else {
    badfun:
	p->current = NULL;
	p->freason = EXC_BADFUN;
	p->fvalue = fun;
	return NULL;
    }
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
    ErlFunThing* funp;
    Eterm* hp;
    int i;

    if (HEAP_LIMIT(p) - HEAP_TOP(p) <= needed) {
	erts_garbage_collect(p, needed, reg, num_free);
    }
    hp = p->htop;
    p->htop = hp + needed;
    funp = (ErlFunThing *) hp;
    hp = funp->env;
    fe->refc++;
    funp->thing_word = HEADER_FUN;
#ifndef SHARED_HEAP
#ifndef HYBRID /* FIND ME! */
    funp->next = MSO(p).funs;
    MSO(p).funs = funp;
#endif
#endif
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
