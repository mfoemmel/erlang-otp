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
/*
 * Common utilities for the different types of db tables.
 * Mostly matching etc.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "erl_db.h"
#include "bif.h"
#include "big.h"

#include "erl_db_util.h"


/*
** Flags for the guard bif's
*/

/* These are offsets from the DCOMP_* value */
#define DBIF_GUARD 1
#define DBIF_BODY  0

/* These are the DBIF flag bits corresponding to the DCOMP_* value.
 * If a bit is set, the BIF is allowed in that context. */
#define DBIF_TABLE_GUARD (1 << (DCOMP_TABLE + DBIF_GUARD))
#define DBIF_TABLE_BODY  (1 << (DCOMP_TABLE + DBIF_BODY))
#define DBIF_TRACE_GUARD (1 << (DCOMP_TRACE + DBIF_GUARD))
#define DBIF_TRACE_BODY  (1 << (DCOMP_TRACE + DBIF_BODY))
#define DBIF_ALL \
DBIF_TABLE_GUARD | DBIF_TABLE_BODY | DBIF_TRACE_GUARD | DBIF_TRACE_BODY



/*
** Some convenience macros for stacks (DMC == db_match_compile)
*/

#define DMC_DEFAULT_SIZE 25

#define DMC_STACK_TYPE(Type) DMC_##Type##_stack

#define DMC_DECLARE_STACK_TYPE(Type)            \
typedef struct DMC_STACK_TYPE(Type) {		\
    int pos;					\
    int siz;					\
    Type def[DMC_DEFAULT_SIZE];		        \
    Type *data;					\
} DMC_STACK_TYPE(Type)
    
#define DMC_INIT_STACK(Name) \
     (Name).pos = 0; (Name).siz = DMC_DEFAULT_SIZE; (Name).data = (Name).def

#define DMC_STACK_DATA(Name) (Name).data

#define DMC_STACK_NUM(Name) (Name).pos

#define DMC_PUSH(On, What) 						  \
(((On).pos < (On).siz) ?						  \
 ((On).data[(On).pos++] = What) :					  \
 ((((On).data) = (((On).def == (On).data) ? 			  \
	memcpy(safe_alloc(((On).siz *= 2) *                              \
			  sizeof(*((On).data))),(On).def,                 \
	       DMC_DEFAULT_SIZE*sizeof(*((On).data))) :                   \
	safe_realloc((char *) (On).data,                                  \
		     ((On).siz *= 2) * sizeof(*((On).data)))))           \
  [(On).pos++] = What))

#define DMC_POP(From) (From).data[--(From).pos]

#define DMC_TOP(From) (From).data[(From).pos - 1]

#define DMC_EMPTY(Name) ((Name).pos == 0)

#define DMC_PEEK(On, At) (On).data[At]     

#define DMC_POKE(On, At, Value) ((On).data[At] = (Value))

#define DMC_CLEAR(Name) (Name).pos = 0

#define DMC_FREE(Name) if ((Name).def != (Name).data) sys_free((Name).data)


/* Type checking... */

#define BOXED_IS_TUPLE(Boxed) is_arity_value(*boxed_val((Boxed)))

/*
**
** Types and enum's (compiled matches)
**
*/

/*
** match VM instructions
*/
typedef enum {
    matchArray, /* Only when parameter is an array (DCOMP_TRACE) */
    matchArrayBind, /* ------------- " ------------ */
    matchTuple,
    matchPushT,
    matchPushL,
    matchPop,
    matchBind,
    matchCmp,
    matchEqBin,
    matchEqFloat,
    matchEqBig,
    matchEqRef,
    matchEq,
    matchList,
    matchSkip,
    matchPushC,
    matchConsA, /* Car is below Cdr */
    matchConsB, /* Cdr is below Car (unusual) */
    matchMkTuple,
    matchCall0,
    matchCall1,
    matchCall2,
    matchCall3,
    matchPushV,
    matchPushExpr, /* Push the whole expression we're matching ('$_') */
    matchPushArrayAsList, /* Only when parameter is an Array and 
			     not an erlang term  (DCOMP_TRACE) */
    matchPushArrayAsListU, /* As above but unknown size */
    matchTrue,
    matchOr,
    matchAnd,
    matchOrElse,
    matchAndThen,
    matchSelf,
    matchWaste,
    matchReturn,
    matchProcessDump,
    matchDisplay,
    matchIsSeqTrace,
    matchSetSeqToken,
    matchGetSeqToken,
    matchSetReturnTrace,
    matchCatch,
    matchEnableTrace,
    matchDisableTrace,
    matchEnableTrace2,
    matchDisableTrace2,
    matchTryMeElse,
    matchCaller,
    matchHalt,
    matchGetTraceControlWord,
    matchSetTraceControlWord,
    matchSilent
} MatchOps;

/*
** Guard bif's
*/

typedef struct dmc_guard_bif {
    Eterm name; /* atom */
    BIF_RETTYPE (*biff)();
    int arity;
    Uint32 flags;
} DMCGuardBif; 

/*
** Error information (for lint)
*/

/*
** Type declarations for stacks
*/
DMC_DECLARE_STACK_TYPE(Eterm);

DMC_DECLARE_STACK_TYPE(Uint);

DMC_DECLARE_STACK_TYPE(unsigned);

/*
** Data about the heap during compilation
*/

typedef struct DMCHeap {
    int size;
    unsigned def[DMC_DEFAULT_SIZE];
    unsigned *data;
    int used;
} DMCHeap;

/*
** Return values from sub compilation steps (guard compilation)
*/

typedef enum dmc_ret { 
    retOk, 
    retFail, 
    retRestart 
} DMCRet; 

/*
** Diverse context information
*/

typedef struct dmc_context {
    int stack_need;
    int stack_used;
    ErlHeapFragment *save;
    ErlHeapFragment *copy;
    Eterm *matchexpr;
    Eterm *guardexpr;
    Eterm *bodyexpr;
    int num_match;
    int current_match;
    int eheap_need;
    Uint32 cflags;
    DMC_STACK_TYPE(Uint) *labels;
    int is_guard; /* 1 if in guard, 0 if in body */
    int special; /* 1 if the head in the match was a single expression */ 
    DMCErrInfo *err_info;
} DMCContext;

/*
**
** Global variables 
**
*/

/*
** Internal
*/

/* 
** The pseudo process used by the VM (pam).
*/

static Process match_pseudo_process;

/* The trace control word. */

static Uint trace_control_word;



/*
** The table of callable bif's, i e guard bif's and 
** some special animals that can provide us with trace
** information. This array is sorted on init.
*/
static DMCGuardBif guard_tab[] =
{
    {
	am_is_atom,
	&is_atom_1,
	1,
	DBIF_ALL
    },
    {
	am_is_constant,
	&is_constant_1,
	1,
	DBIF_ALL
    },    
    {
	am_is_float,
	&is_float_1,
	1,
	DBIF_ALL
    },
    {
	am_is_integer,
	&is_integer_1,
	1,
	DBIF_ALL
    },
    {
	am_is_list,
	&is_list_1,
	1,
	DBIF_ALL
    },
    {
	am_is_number,
	&is_number_1,
	1,
	DBIF_ALL
    },
    {
	am_is_pid,
	&is_pid_1,
	1,
	DBIF_ALL
    },
    {
	am_is_port,
	&is_port_1,
	1,
	DBIF_ALL
    },
    {
	am_is_reference,
	&is_reference_1,
	1,
	DBIF_ALL
    },
    {
	am_is_tuple,
	&is_tuple_1,
	1,
	DBIF_ALL
    },
    {
	am_is_binary,
	&is_binary_1,
	1,
	DBIF_ALL
    },
    {
	am_is_function,
	&is_function_1,
	1,
	DBIF_ALL
    },
    {
	am_is_record,
	&is_record_3,
	3,
	DBIF_ALL
    },
    {
	am_abs,
	&abs_1,
	1,
	DBIF_ALL
    },
    {
	am_element,
	&element_2,
	2,
	DBIF_ALL
    },
    {
	am_hd,
	&hd_1,
	1,
	DBIF_ALL
    },
    {
	am_length,
	&length_1,
	1,
	DBIF_ALL
    },
    {
	am_node,
	&node_1,
	1,
	DBIF_ALL
    },
    {
	am_node,
	&node_0,
	0,
	DBIF_ALL
    },
    {
	am_round,
	&round_1,
	1,
	DBIF_ALL
    },
    {
	am_size,
	&size_1,
	1,
	DBIF_ALL
    },
    {
	am_tl,
	&tl_1,
	1,
	DBIF_ALL
    },
    {
	am_trunc,
	&trunc_1,
	1,
	DBIF_ALL
    },
    {
	am_Plus,
	&splus_2,
	2,
	DBIF_ALL
    },
    {
	am_Minus,
	&sminus_2,
	2,
	DBIF_ALL
    },
    {
	am_Times,
	&stimes_2,
	2,
	DBIF_ALL
    },
    {
	am_Div,
	&sdiv_2,
	2,
	DBIF_ALL
    },
    {
	am_div,
	&div_2,
	2,
	DBIF_ALL
    },
    {
	am_rem,
	&rem_2,
	2,
	DBIF_ALL
    },
    {
	am_band,
	&band_2,
	2,
	DBIF_ALL
    },
    {
	am_bor,
	&bor_2,
	2,
	DBIF_ALL
    },
    {
	am_bxor,
	&bxor_2,
	2,
	DBIF_ALL
    },
    {
	am_bnot,
	&bnot_1,
	1,
	DBIF_ALL
    },
    {
	am_bsl,
	&bsl_2,
	2,
	DBIF_ALL
    },
    {
	am_bsr,
	&bsr_2,
	2,
	DBIF_ALL
    },
    {
	am_Gt,
	&sgt_2,
	2,
	DBIF_ALL
    },
    {
	am_Ge,
	&sge_2,
	2,
	DBIF_ALL
    },
    {
	am_Lt,
	&slt_2,
	2,
	DBIF_ALL
    },
    {
	am_Le,
	&sle_2,
	2,
	DBIF_ALL
    },
    {
	am_Eq,
	&seq_2,
	2,
	DBIF_ALL
    },
    {
	am_Eqeq,
	&seqeq_2,
	2,
	DBIF_ALL
    },
    {
	am_Neq,
	&sneq_2,
	2,
	DBIF_ALL
    },
    {
	am_Neqeq,
	&sneqeq_2,
	2,
	DBIF_ALL
    },
    {
	am_not,
	&not_1,
	1,
	DBIF_ALL
    },
    {
	am_xor,
	&xor_2,
	2,
	DBIF_ALL
    },
    {
	am_get_tcw,
	&db_get_trace_control_word_0,
	0,
	DBIF_TRACE_GUARD | DBIF_TRACE_BODY
    },
    {
	am_set_tcw,
	&db_set_trace_control_word_1,
	1,
	DBIF_TRACE_BODY
    }
};

/*
** Exported
*/
Eterm db_am_eot;                /* Atom '$end_of_table' */

/*
** Forward decl's
*/


/*
** ... forwards for compiled matches
*/
/* Utility code */
static DMCGuardBif *dmc_lookup_bif(Eterm t, int arity);
static Eterm dmc_lookup_bif_reversed(void *f);
static int cmp_uint(void *a, void *b);
static int cmp_guard_bif(void *a, void *b);
static int match_compact(ErlHeapFragment *expr, DMCErrInfo *err_info);
static Uint my_size_object(Eterm t);
static Eterm my_copy_struct(Eterm t, Eterm **hp, ErlOffHeap* off_heap);
static Binary *allocate_magic_binary(size_t size);


/* Guard compilation */
static void do_emit_constant(DMCContext *context, DMC_STACK_TYPE(Uint) *text,
			     Eterm t);
static DMCRet dmc_list(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant);
static DMCRet dmc_tuple(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant);
static DMCRet dmc_variable(DMCContext *context,
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Uint) *text,
			   Eterm t,
			   int *constant);
static DMCRet dmc_fun(DMCContext *context,
		      DMCHeap *heap,
		      DMC_STACK_TYPE(Uint) *text,
		      Eterm t,
		      int *constant);
static DMCRet dmc_expr(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant);
static DMCRet compile_guard_expr(DMCContext *context,
				    DMCHeap *heap,
				    DMC_STACK_TYPE(Uint) *text,
				    Eterm t);
/* match expression subroutine */
static DMCRet dmc_one_term(DMCContext *context, 
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Eterm) *stack,
			   DMC_STACK_TYPE(Uint) *text,
			   Eterm c);


#ifdef DMC_DEBUG
static int test_disassemble_next = 0;
static void db_match_dis(Binary *prog);
#define TRACE erl_printf(CERR,"Trace: %s:%d\n",__FILE__,__LINE__)
#define FENCE_PATTERN_SIZE 1
#define FENCE_PATTERN 0xDEADBEEFUL
#else
#define TRACE /* Nothing */
#define FENCE_PATTERN_SIZE 0
#endif
static void add_dmc_err(DMCErrInfo *err_info, 
			   char *str,
			   int variable,
			   Eterm term,
			   DMCErrorSeverity severity);

static Eterm dpm_array_to_list(Process *psp, Eterm *arr, int arity);

static Eterm match_spec_test(Process *p, Eterm against, Eterm spec, int trace);

/*
** Interface routines.
*/

/*
** Pseudo BIF:s to be callable from the PAM VM.
*/

BIF_RETTYPE db_get_trace_control_word_0(Process *p) 
{
    BIF_RET(make_small_or_big(trace_control_word, p));
}

BIF_RETTYPE db_set_trace_control_word_1(Process *p, Eterm new) 
{
    Uint val;
    Eterm old;
    if (!term_to_Uint(new, &val))
	BIF_ERROR(p, BADARG);
    old = make_small_or_big(trace_control_word, p);
    trace_control_word = val;
    BIF_RET(old);
}

/*
** The API used by the tracer (declared in global.h):
*/

/*
** Matchexpr is a list of tuples containing match-code, i e:
**
** Matchexpr = [{Pattern, Guards, Body}, ...]
** Pattern = [ PatternExpr , ...]
** PatternExpr = Constant | PatternTuple | PatternList | Variable
** Constant = Any erlang term
** PatternTuple = { PatternExpr ... }
** PatternList = [ PatternExpr ]
** Variable = '$' ++ <number>
** Guards = [Guard ...]
** Guard = {GuardFunc, GuardExpr, ...}
** GuardExpr = BoundVariable | Guard | GuardList | GuardTuple | ConstExpr
** BoundVariable = Variable (existing in Pattern)  
** GuardList = [ GuardExpr , ... ]
** GuardTuple = {{ GuardExpr, ... }}
** ConstExpr = {const, Constant}
** GuardFunc = is_list | .... | element | ...
** Body = [ BodyExpr, ... ]
** BodyExpr = GuardExpr | { BodyFunc, GuardExpr, ... }
** BodyFunc = return_trace | seq_trace | trace | ...
** - or something like that...
*/


Eterm erts_match_set_get_source(Binary *mpsp)
{
    MatchProg *prog = Binary2MatchProg(mpsp);
    return prog->saved_program;
}

/* This one is for the tracing */
Binary *erts_match_set_compile(Process *p, Eterm matchexpr) {
    Binary *bin;
    Uint sz;
    Eterm *hp;
    
    bin = db_match_set_compile(p, matchexpr, DCOMP_TRACE);
    if (bin != NULL) {
	MatchProg *prog = Binary2MatchProg(bin);
	sz = size_object(matchexpr);
	prog->saved_program_buf = new_message_buffer(sz);
	hp = prog->saved_program_buf->mem;
	prog->saved_program = 
	    copy_struct(matchexpr, sz, &hp, 
			&(prog->saved_program_buf->off_heap));
    }
    return bin;
}

Binary *db_match_set_compile(Process *p, Eterm matchexpr, 
			     int flags) 
{
    Eterm l;
    Eterm t;
    Eterm l2;
    Eterm *tp;
    Eterm *hp;
    int n = 0;
    int num_heads;
    int i;
    Binary *mps = NULL;
    int compiled = 0;
    Eterm *matches,*guards, *bodies;
    Eterm *buff;
    Eterm sbuff[15];

    if (!is_list(matchexpr))
	return NULL;
    num_heads = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l)))
	++num_heads;

    if (l != NIL) /* proper list... */
	return NULL;

    if (num_heads > 5) {
	buff = safe_alloc(sizeof(Eterm) * num_heads * 3);
    } else {
	buff = sbuff;
    }

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l))) {
	t = CAR(list_val(l));
	if (!is_tuple(t) || arityval((tp = tuple_val(t))[0]) != 3) {
	    goto error;
	}
	if (!(flags == DCOMP_TRACE) || (!is_list(tp[1]) && 
					!is_nil(tp[1]))) {
	    t = tp[1];
	} else {
	    /* This is when tracing, the parameter is a list,
	       that I convert to a tuple and that is matched 
	       against an array (strange, but gives the semantics
	       of matching against a parameter list) */
	    n = 0;
	    for (l2 = tp[1]; is_list(l2); l2 = CDR(list_val(l2))) {
		++n;
	    }
	    if (l2 != NIL) {
		goto error;
	    }
	    hp = HAlloc(p, n + 1);
	    t = make_tuple(hp);
	    *hp++ = make_arityval((Uint) n);
	    l2 = tp[1];
	    while (n--) {
		*hp++ = CAR(list_val(l2));
		l2 = CDR(list_val(l2));
	    }
	}
	matches[i] = t;
	guards[i] = tp[2];
	bodies[i] = tp[3];
	++i;
    }
    if ((mps = db_match_compile(matches, guards, bodies,
				num_heads,
				flags,
				NULL)) == NULL) {
	goto error;
    }
    compiled = 1;
    if (buff != sbuff) {
	sys_free(buff);
    }
    return mps;

error:
    if (compiled) {
	erts_match_set_free(mps);
    }
    if (buff != sbuff) {
	sys_free(buff);
    }
    return NULL;
}

/* This is used when tracing */
Eterm erts_match_set_lint(Process *p, Eterm matchexpr) {
    return db_match_set_lint(p, matchexpr, DCOMP_TRACE);
}

Eterm db_match_set_lint(Process *p, Eterm matchexpr, int flags) 
{
    Eterm l;
    Eterm t;
    Eterm l2;
    Eterm *tp;
    Eterm *hp;
    DMCErrInfo *err_info = db_new_dmc_err_info();
    Eterm ret;
    int n = 0;
    int num_heads;
    Binary *mp;
    Eterm *matches,*guards, *bodies;
    Eterm sbuff[15];
    Eterm *buff = sbuff;
    int i;

    if (!is_list(matchexpr)) {
	add_dmc_err(err_info, "Match programs are not in a list.", 
		    -1, 0UL, dmcError);
	goto done;
    }
    num_heads = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l)))
	++num_heads;

    if (l != NIL)  { /* proper list... */
	add_dmc_err(err_info, "Match programs are not in a proper "
		    "list.", 
		    -1, 0UL, dmcError);
	goto done;
    }

    if (num_heads > 5) {
	buff = safe_alloc(sizeof(Eterm) * num_heads * 3);
    } 

    matches = buff;
    guards = buff + num_heads;
    bodies = buff + (num_heads * 2);

    i = 0;
    for (l = matchexpr; is_list(l); l = CDR(list_val(l))) {
	t = CAR(list_val(l));
	if (!is_tuple(t) || arityval((tp = tuple_val(t))[0]) != 3) {
	    add_dmc_err(err_info, 
			"Match program part is not a tuple of "
			"arity 3.", 
			-1, 0UL, dmcError);
	    goto done;
	}
	if (!(flags == DCOMP_TRACE) || (!is_list(tp[1]) && 
					!is_nil(tp[1]))) {
	    t = tp[1];
	} else {
	    n = 0;
	    for (l2 = tp[1]; is_list(l2); l2 = CDR(list_val(l2))) {
		++n;
	    }
	    if (l2 != NIL) {
		add_dmc_err(err_info, 
			    "Match expression part %s is not a "
			    "proper list.", 
			    -1, tp[1], dmcError);
		
		goto done;
	    }
	    hp = HAlloc(p, n + 1);
	    t = make_tuple(hp);
	    *hp++ = make_arityval((Uint) n);
	    l2 = tp[1];
	    while (n--) {
		*hp++ = CAR(list_val(l2));
		l2 = CDR(list_val(l2));
	    }
	}
	matches[i] = t;
	guards[i] = tp[2];
	bodies[i] = tp[3];
	++i;
    }
    mp = db_match_compile(matches, guards, bodies, num_heads,
			  flags, err_info); 
    if (mp != NULL) {
	erts_match_set_free(mp);
    }
done:
    ret = db_format_dmc_err_info(p, err_info);
    db_free_dmc_err_info(err_info);
    if (buff != sbuff) {
	sys_free(buff);
    }
    return ret;
}
    
Eterm erts_match_set_run(Process *p, Binary *mpsp, 
			 Eterm *args, int num_args, 
			 Uint32 *return_flags) 
{
    Eterm ret;

    ret = db_prog_match(p, mpsp,
			(Eterm) args, 
			num_args, return_flags);
    if (is_value(ret)) {
#if defined(HARDDEBUG)
	if (is_non_value(ret)) {
	    erl_printf(CERR,"Failed\r\n");
	} else {
	    erl_printf(CERR, "Returning :");
	    display(ret,CERR);
	    erl_printf(CERR, "\r\n");
	}
#endif
	if ((p->flags & F_TRACE_SILENT) 
	    || ret == am_false)
	    return THE_NON_VALUE;
	else
	    return ret;
    } 
    return ret;
}

/*
** API Used by other erl_db modules.
*/

void db_initialize_util(void){
    qsort(guard_tab, 
	  sizeof(guard_tab) / sizeof(DMCGuardBif), 
	  sizeof(DMCGuardBif), 
	  (int (*)(const void *, const void *)) &cmp_guard_bif);
    erts_init_empty_process(&match_pseudo_process);
    trace_control_word = 0;
}



Eterm db_getkey(int keypos, Eterm obj)
{
    if (is_tuple(obj)) {
	Eterm *tptr = tuple_val(obj);
	if (arityval(*tptr) >= keypos)
	    return *(tptr + keypos);
    }
    return THE_NON_VALUE;
}

/*
** Matching compiled (executed by "Pam" :-)
*/

/*
** The actual compiling of the match expression and the guards
*/
Binary *db_match_compile(Eterm *matchexpr, 
			 Eterm *guards, 
			 Eterm *body,
			 int num_progs,
			 Uint32 flags, 
			 DMCErrInfo *err_info)
{
    DMCHeap heap;
    DMC_STACK_TYPE(Eterm) stack;
    DMC_STACK_TYPE(Uint) text;
    DMC_STACK_TYPE(Uint) labels;
    DMCContext context;
    MatchProg *ret = NULL;
    Eterm t;
    Uint i;
    Uint num_iters;
    int structure_checked;
    DMCRet res;
    int current_try_label;
    Uint max_eheap_need;
    Binary *bp = NULL;
    unsigned clause_start;


    DMC_INIT_STACK(stack);
    DMC_INIT_STACK(text);
    DMC_INIT_STACK(labels);

    context.stack_need = context.stack_used = 0;
    context.save = context.copy = NULL;
    context.num_match = num_progs;
    context.matchexpr = matchexpr;
    context.guardexpr = guards;
    context.bodyexpr = body;
    context.eheap_need = 0;
    context.err_info = err_info;
    context.cflags = flags;
    context.labels = &labels;

    heap.size = DMC_DEFAULT_SIZE;
    heap.data = heap.def;

    /*
    ** Compile the match expression
    */
restart:
    heap.used = 0;
    max_eheap_need = 0;
    for (context.current_match = 0; 
	 context.current_match < num_progs; 
	 ++context.current_match) { /* This loop is long, 
				       too long */
	memset(heap.data, 0, heap.size * sizeof(*heap.data));
	t = context.matchexpr[context.current_match];
	context.stack_used = 0;
	context.eheap_need = 0;
	structure_checked = 0;
	if (context.current_match < num_progs - 1) {
	    DMC_PUSH(text,matchTryMeElse);
	    DMC_PUSH(text,current_try_label = 
		     DMC_STACK_NUM(*(context.labels)));
	    DMC_PUSH(*(context.labels), 0);
	} else {
	    current_try_label = -1;
	}
	clause_start = DMC_STACK_NUM(text); /* the "special" test needs it */
	DMC_PUSH(stack,NIL);
	for (;;) {
	    switch (t & _TAG_PRIMARY_MASK) {
	    case TAG_PRIMARY_BOXED:
		if (!BOXED_IS_TUPLE(t)) {
		    goto simple_term;
		}
		num_iters = arityval(*tuple_val(t));
		if (!structure_checked) { /* i.e. we did not 
					     pop it */
		    DMC_PUSH(text,matchTuple);
		    DMC_PUSH(text,num_iters);
		}
		structure_checked = 0;
		for (i = 1; i <= num_iters; ++i) {
		    if ((res = dmc_one_term(&context, 
					    &heap, 
					    &stack, 
					    &text, 
					    tuple_val(t)[i]))
			!= retOk) {
			if (res == retRestart) {
			    goto restart; /* restart the 
					     surrounding 
					     loop */
			} else goto error;
		    }	    
		}
		break;
	    case TAG_PRIMARY_LIST:
		if (!structure_checked) {
		    DMC_PUSH(text, matchList);
		}
		structure_checked = 0; /* Whatever it is, we did 
					  not pop it */
		if ((res = dmc_one_term(&context, &heap, &stack, 
					&text, CAR(list_val(t))))
		    != retOk) {
		    if (res == retRestart) {
			goto restart;
		    } else goto error;
		}	    
		t = CDR(list_val(t));
		continue;
	    default: /* Nil and non proper tail end's or 
			single terms as match 
			expressions */
	    simple_term:
		structure_checked = 0;
		if ((res = dmc_one_term(&context, &heap, &stack, 
					&text, t))
		    != retOk) {
		    if (res == retRestart) {
			goto restart;
		    } else goto error;
		}	    
		break;
	    }

	    /* The *program's* stack just *grows* while we are 
	       traversing one composite data structure, we can 
	       check the stack usage here */

	    if (context.stack_used > context.stack_need)
		context.stack_need = context.stack_used;

	    /* We are at the end of one composite data structure, 
	       pop sub structures and emit a matchPop instruction 
	       (or break) */
	    if ((t = DMC_POP(stack)) == NIL) {
		break;
	    } else {
		DMC_PUSH(text, matchPop);
		structure_checked = 1; /* 
					* Checked with matchPushT 
					* or matchPushL
					*/
		--(context.stack_used);
	    }
	}
    
	/* 
	** There is one single top variable in the match expression
	** iff the text is tho Uint's and the single instruction 
	** is 'matchBind' or it is only a skip.
	*/
	context.special = 
	    (DMC_STACK_NUM(text) == 2 + clause_start && 
	     DMC_PEEK(text,clause_start) == matchBind) || 
	    (DMC_STACK_NUM(text) == 1 + clause_start && 
	     DMC_PEEK(text, clause_start) == matchSkip);

	if (flags == DCOMP_TRACE) {
	    if (context.special) {
		if (DMC_PEEK(text, clause_start) == matchBind) {
		    DMC_POKE(text, clause_start, matchArrayBind);
		} 
	    } else {
		ASSERT(DMC_STACK_NUM(text) >= 1);
		if (DMC_PEEK(text, clause_start) != matchTuple) {
		    /* If it isn't "special" and the argument is 
		       not a tuple, the expression is not valid 
		       when matching an array*/
		    if (context.err_info) {
			add_dmc_err(context.err_info, 
				    "Match head is invalid in "
				    "this context.", 
				    -1, 0UL,
				    dmcError);
		    }
		    goto error;
		}
		DMC_POKE(text, clause_start, matchArray);
	    }
	}


	/*
	** ... and the guards
	*/
	context.is_guard = 1;
	if (compile_guard_expr
	    (&context,
	     &heap,
	     &text,
	     context.guardexpr[context.current_match]) != retOk) 
	    goto error;
	context.is_guard = 0;
	if ((context.cflags == DCOMP_TABLE) && 
	    !is_list(context.bodyexpr[context.current_match])) {
	    if (context.err_info) {
		add_dmc_err(context.err_info, 
			    "Body clause does not return "
			    "anything.", -1, 0UL,
			    dmcError);
	    }
	    goto error;
	}
	if (compile_guard_expr
	    (&context,
	     &heap,
	     &text,
	     context.bodyexpr[context.current_match]) != retOk) 
	    goto error;

	/*
	 * The compilation does not bail out when error information
	 * is requested, so we need to detect that here...
	 */
	if (context.err_info != NULL && 
	    (context.err_info)->error_added) {
	    goto error;
	}


	/* If the matchprogram comes here, the match is 
	   successfull */
	DMC_PUSH(text,matchHalt);
	/* Fill in try-me-else label if there is one. */ 
	if (current_try_label >= 0) {
	    DMC_POKE(*(context.labels), current_try_label, 
		     DMC_STACK_NUM(text));
	}
	/* So, how much eheap did this part of the match program need? */
	if (context.eheap_need > max_eheap_need) {
	    max_eheap_need = context.eheap_need;
	}
    } /* for (context.current_match = 0 ...) */


    /*
    ** Done compiling
    ** Allocate enough space for the program,
    ** heap size is in 'heap_used', stack size is in 'stack_need'
    ** and text size is simply DMC_STACK_NUM(text).
    ** The "program memory" is allocated like this:
    ** text ----> +-------------+
    **            |             |
    **              ..........
    ** labels --> +             + (labels are offset's from text)
    **              ..........
    ** heap ----> +             +
    **              ..........
    ** eheap ---> +             +
    **              ..........
    ** stack ---> +             +
    **              ..........
    **            +-------------+
    ** The stack is expected to grow towards *higher* adresses.
    ** A special case is when the match expression is a single binding 
    ** (i.e '$1'), then the field single_variable is set to 1.
    */
    bp = allocate_magic_binary
	((sizeof(MatchProg) - sizeof(Uint)) +
	 (DMC_STACK_NUM(text) * sizeof(Uint)) +
	 (heap.used * sizeof(Eterm)) +
	 (max_eheap_need * sizeof(Eterm)) +
	 (context.stack_need * sizeof(Eterm *)) +
	 (DMC_STACK_NUM(labels) * sizeof(Uint)) +
	 (3 * (FENCE_PATTERN_SIZE * sizeof(Eterm *))));
    ret = Binary2MatchProg(bp);
    ret->saved_program_buf = NULL;
    ret->saved_program = NIL;
    ret->term_save = context.save;
    ret->num_bindings = heap.used;
    ret->labels = (Uint *) (ret->text + DMC_STACK_NUM(text));
    ret->heap = (Eterm *) (ret->labels + DMC_STACK_NUM(labels));
    ret->single_variable = context.special;
    ret->eheap = (Eterm *) (ret->heap + (heap.used + FENCE_PATTERN_SIZE));
    ret->stack = (Eterm **) (ret->eheap + 
			     (max_eheap_need + FENCE_PATTERN_SIZE));
#ifdef DMC_DEBUG
    *((unsigned long *) (ret->heap + heap.used)) = FENCE_PATTERN;
    *((unsigned long *) (ret->eheap + max_eheap_need)) = FENCE_PATTERN;
    *((unsigned long *) (ret->stack + context.stack_need)) = FENCE_PATTERN;
    ret->stack_size = context.stack_need;
#endif
    sys_memcpy(ret->text, DMC_STACK_DATA(text), 
	       DMC_STACK_NUM(text) * sizeof(Uint));
    sys_memcpy(ret->labels, DMC_STACK_DATA(labels), 
	       DMC_STACK_NUM(labels) * sizeof(Uint));
    sys_memset(ret->heap, 0, heap.used * sizeof(Eterm));
    /* 
     * Fall through to cleanup code, but context.save should not be free'd
     */  
    context.save = NULL;
error: /* Here is were we land when compilation failed. */
    while (context.save != NULL) {
	ErlHeapFragment *ll = context.save->next;
	free_message_buffer(context.save);
	context.save = ll;
    }
    DMC_FREE(stack);
    DMC_FREE(text);
    DMC_FREE(labels);
    if (context.copy != NULL) 
	free_message_buffer(context.copy);
    if (heap.data != heap.def)
	sys_free(heap.data);
    return bp;
}

/*
** Free a match program (in a binary)
*/
void erts_match_set_free(Binary *bprog)
{
    MatchProg *prog;
    ErlHeapFragment *tmp, *ll;
    if (bprog == NULL)
	return;
    prog = Binary2MatchProg(bprog);
    tmp = prog->term_save; 
    while (tmp != NULL) {
	ll = tmp->next;
	free_message_buffer(tmp);
	tmp = ll;
    }
    if (prog->saved_program_buf != NULL)
	free_message_buffer(prog->saved_program_buf);
    sys_free(bprog);
}

/*
** This is not the most efficient way to do it, but it's a rare
** and not especially nice case when this is used.
*/
static Eterm dpm_array_to_list(Process *psp, Eterm *arr, int arity)
{
    Eterm *hp = HAlloc(psp, arity * 2);
    Eterm ret = NIL;
    while (--arity >= 0) {
	ret = CONS(hp, arr[arity], ret);
	hp += 2;
    }
    return ret;
}
/*
** Execution of the match program, this is Pam.
** May return THE_NON_VALUE, which is a bailout.
** the para meter 'arity' is only used if 'term' is actually an array,
** i.e. 'DCOMP_TRACE' was specified 
*/
Eterm db_prog_match(Process *p, Binary *bprog, Eterm term, 
		    int arity,
		    Uint32 *return_flags)
{
    MatchProg *prog = Binary2MatchProg(bprog);
    Eterm *ep;
    Eterm *tp;
    Eterm t;
    Eterm **sp;
    Eterm *esp;
    Eterm *hp;
    Uint *pc = prog->text;
    Eterm *ehp;
    Eterm ret;
    Uint n = 0; /* To avoid warning. */
    int i;
    unsigned do_catch;
    Process *psp = &match_pseudo_process;
    Process *tmpp;
    Eterm (*bif)();
    int fail_label;


#ifdef DMC_DEBUG
    unsigned long *heap_fence =  (unsigned long *) prog->eheap - 1;
    unsigned long *eheap_fence = (unsigned long *) ((Eterm *) prog->stack) - 1;
    unsigned long *stack_fence = (unsigned long *) ((Eterm *) prog->stack) + 
	prog->stack_size;
    Uint save_op = 0;
#endif /* DMC_DEBUG */

#ifdef HARDDEBUG
#define FAIL() {erl_printf(COUT,"Fail line %d\r\n",__LINE__); goto fail;}
#else
#define FAIL() goto fail
#endif
#define FAIL_TERM am_EXIT /* The term to set as return when bif fails and
			     do_catch != 0 */


    ASSERT(*heap_fence == FENCE_PATTERN);
    ASSERT(*eheap_fence == FENCE_PATTERN);
    ASSERT(*stack_fence == FENCE_PATTERN);


    /*
    ** Bif's that are called from this VM is expected NOT to allocate
    ** anything except using ArithAlloc, i clean up from last call here,
    ** so that data returned survive until a) the program is free'd, or
    ** b) this function is called again.
    */
#ifdef UNIFIED_HEAP
    if (psp->off_heap.mso || psp->off_heap.funs) {
#else
    if (psp->mbuf || psp->off_heap.mso || psp->off_heap.funs) {
	ErlHeapFragment *bp = psp->mbuf;
	ErlHeapFragment *sbp;
	while (bp != NULL) {
	    sbp = bp->next;
	    free_message_buffer(bp);
	    bp = sbp;
	}
	psp->mbuf = NULL;
#endif
	erts_cleanup_offheap(&psp->off_heap);
	psp->off_heap.mso = NULL;
	psp->off_heap.funs = NULL;
	psp->off_heap.overhead = 0;
	/* Never GC'ed, dont touch old_mso */
	psp->arith_avail = 0;
	psp->arith_heap = NULL;
#ifdef DEBUG
	p->arith_check_me = NULL;
#endif
    }

    *return_flags = 0U;

restart:
    ep = &term;
    sp = prog->stack;
    esp = (Eterm *) prog->stack;
    hp = prog->heap;
    ehp = prog->eheap;
    ret = am_true;
    do_catch = 0;
    fail_label = -1;

    for (;;) {
#ifdef DMC_DEBUG
	if (*heap_fence != FENCE_PATTERN) {
	    erl_exit(1, "Heap fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, *heap_fence);
	}
	if (*eheap_fence != FENCE_PATTERN) {
	    erl_exit(1, "Eheap fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, 
		     *eheap_fence);
	}
	if (*stack_fence != FENCE_PATTERN) {
	    erl_exit(1, "Stack fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, 
		     *stack_fence);
	}
	save_op = *pc;
#endif
	switch (*pc++) {
	case matchTryMeElse:
	    n = *pc++;
	    fail_label = prog->labels[n];
	    break;
	case matchArray: /* only when DCOMP_TRACE, is always first
			    instruction. */
	    n = *pc++;
	    if ((int) n != arity)
		FAIL();
	    ep = (Eterm *) *ep;
	    break;
	case matchArrayBind: /* When the array size is unknown. */
	    n = *pc++;
	    hp[n] = dpm_array_to_list(psp, (Eterm *) term, arity);
	    break;
	case matchTuple: /* *ep is a tuple of arity n */
	    if (!is_tuple(*ep))
		FAIL();
	    ep = tuple_val(*ep);
	    n = *pc++;
	    if (arityval(*ep) != n)
		FAIL();
	    ++ep;
	    break;
	case matchPushT: /* *ep is a tuple of arity n, 
			    push ptr to first element */
	    if (!is_tuple(*ep))
		FAIL();
	    tp = tuple_val(*ep);
	    n = *pc++;
	    if (arityval(*tp) != n)
		FAIL();
	    *sp++ = tp + 1;
	    ++ep;
	    break;
	case matchList:
	    if (!is_list(*ep))
		FAIL();
	    ep = list_val(*ep);
	    break;
	case matchPushL:
	    if (!is_list(*ep))
		FAIL();
	    *sp++ = list_val(*ep);
	    ++ep;
	    break;
	case matchPop:
	    ep = *(--sp);
	    break;
	case matchBind:
	    n = *pc++;
	    hp[n] = *ep++;
	    break;
	case matchCmp:
	    n = *pc++;
	    if (!eq(hp[n],*ep))
		FAIL();
	    ++ep;
	    break;
	case matchEqBin:
	    t = (Eterm) *pc++;
	    if (!eq(*ep,t))
		FAIL();
	    ++ep;
	    break;
	case matchEqFloat:
	    if (memcmp(float_val(*ep) + 1, pc, sizeof(double)))
		FAIL();
	    pc += 2;
	    ++ep;
	    break;
	case matchEqRef:
	    tp = ref_val(*ep);
	    if (!eqref(tp, pc))
		FAIL();
	    i = REF_ARITY(pc);
	    pc += i+1;
	    ++ep;
	    break;
	case matchEqBig:
	    tp = big_val(*ep);
	    if (*tp != *pc)
		FAIL();
	    i = BIG_ARITY(pc);
	    while(i--)
		if (*++tp != *++pc)
		    FAIL();
	    ++pc;
	    ++ep;
	    break;
	case matchEq:
	    t = (Eterm) *pc++; 
	    if (t != *ep++)
		FAIL();
	    break;
	case matchSkip:
	    ++ep;
	    break;
	/* 
	 * Here comes guard instructions 
	 */
	case matchPushC: /* Push constant */
	    *esp++ = *pc++;
	    break;
	case matchConsA:
	    ehp[1] = *--esp;
	    ehp[0] = esp[-1];
	    esp[-1] = make_list(ehp);
	    ehp += 2;
	    break;
	case matchConsB:
	    ehp[0] = *--esp;
	    ehp[1] = esp[-1];
	    esp[-1] = make_list(ehp);
	    ehp += 2;
	    break;
	case matchMkTuple:
	    n = *pc++;
	    t = make_tuple(ehp);
	    *ehp++ = make_arityval(n);
	    while (n--) {
		*ehp++ = *--esp;
	    }
	    *esp++ = t;
	    break;
	case matchCall0:
	    bif = (Eterm (*)()) *pc++;
	    t = (*bif)(psp);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    *esp++ = t;
	    break;
	case matchCall1:
	    bif = (Eterm (*)()) *pc++;
	    t = (*bif)(psp, esp[-1]);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    esp[-1] = t;
	    break;
	case matchCall2:
	    bif = (Eterm (*)()) *pc++;
	    t = (*bif)(psp, esp[-1], esp[-2]);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    --esp;
	    esp[-1] = t;
	    break;
	case matchCall3:
	    bif = (Eterm (*)()) *pc++;
	    t = (*bif)(psp, esp[-1], esp[-2], esp[-3]);
	    if (is_non_value(t)) {
		if (do_catch)
		    t = FAIL_TERM;
		else
		    FAIL();
	    }
	    esp -= 2;
	    esp[-1] = t;
	    break;
	case matchPushV:
	    *esp++ = hp[*pc++];
	    break;
	case matchPushExpr:
	    *esp++ = term;
	    break;
	case matchPushArrayAsList:
	    n = arity; /* Only happens when 'term' is an array */
	    tp = (Eterm *) term;
	    *esp++  = make_list(ehp);
	    while (n--) {
		*ehp++ = *tp++;
		*ehp = make_list(ehp + 1);
		ehp++; /* As pointed out by Mikael Pettersson the expression
			  (*ehp++ = make_list(ehp + 1)) that I previously
			  had written here has undefined behaviour. */
	    }
	    ehp[-1] = NIL;
	    break;
	case matchPushArrayAsListU:
	    /* This instruction is NOT efficient. */
	    *esp++  = dpm_array_to_list(psp, (Eterm *) term, arity); 
	    break;
	case matchTrue:
	    if (*--esp != am_true)
		FAIL();
	    break;
	case matchOr:
	    n = *pc++;
	    t = am_false;
	    while (n--) {
		if (*--esp == am_true) {
		    t = am_true;
		}
	    }
	    *esp++ = t;
	    break;
	case matchAnd:
	    n = *pc++;
	    t = am_true;
	    while (n--) {
		if (*--esp != am_true) {
		    t = am_false;
		}
	    }
	    *esp++ = t;
	    break;
	case matchOrElse:
	    n = *pc++;
	    if (*--esp == am_true) {
		++esp;
		pc = (prog->text) + prog->labels[n];
	    }
	    break;
	case matchAndThen:
	    n = *pc++;
	    if (*--esp != am_true) {
		*esp++ = am_false;
		pc = (prog->text) + prog->labels[n];
	    }
	    break;
	case matchSelf:
	    *esp++ = p->id;
	    break;
	case matchWaste:
	    --esp;
	    break;
	case matchReturn:
	    ret = *--esp;
	    break;
	case matchProcessDump:
	    cerr_pos = 0;
	    print_process_info(p, CBUF);
	    *esp++ = new_binary(psp, tmp_buf, cerr_pos);
	    break;
	case matchDisplay: /* Debugging, not for production! */
	    display(esp[-1], COUT);
	    erl_printf(COUT, "\r\n");
	    esp[-1] = am_true;
	    break;
	case matchSetReturnTrace:
	    *return_flags |= MATCH_SET_RETURN_TRACE;
	    *esp++ = am_true;
	    break;
	case matchIsSeqTrace:
	    if (SEQ_TRACE_TOKEN(p) != NIL)
		*esp++ = am_true;
	    else
		*esp++ = am_false;
	    break;
	case matchSetSeqToken:
	    t = erts_seq_trace(p, esp[-1], esp[-2], 0);
	    if (is_non_value(t)) {
		esp[-2] = FAIL_TERM;
	    } else {
		esp[-2] = t;
	    }
	    --esp;
	    break;
	case matchGetSeqToken:
	    if (SEQ_TRACE_TOKEN(p) == NIL) 
		*esp++ = NIL;
	    else {
 		*esp++ = make_tuple(ehp);
 		ehp[0] = make_arityval(5);
 		ehp[1] = SEQ_TRACE_TOKEN_FLAGS(p);
 		ehp[2] = SEQ_TRACE_TOKEN_LABEL(p);
 		ehp[3] = SEQ_TRACE_TOKEN_SERIAL(p);
 		ehp[4] = SEQ_TRACE_TOKEN_SENDER(p);
 		ehp[5] = SEQ_TRACE_TOKEN_LASTCNT(p);
		ASSERT(SEQ_TRACE_TOKEN_ARITY(p) == 5);
		ASSERT(is_immed(ehp[1]));
		ASSERT(is_immed(ehp[2]));
		ASSERT(is_immed(ehp[3]));
		ASSERT(is_immed(ehp[4]));
		ASSERT(is_immed(ehp[5]));
 		ehp += 6;
	    } 
	    break;
	case matchEnableTrace:
	    n = erts_trace_flag2bit(esp[-1]);
	    if (n != 0) {
		p->flags |= n;
		esp[-1] = am_true;
	    } else {
		esp[-1] = FAIL_TERM;
	    }
	    break;
	case matchEnableTrace2:
	    n = erts_trace_flag2bit(esp[-2]);
	    if (n != 0 && ((is_pid(esp[-1]) && 
			    pid_node(esp[-1]) == THIS_NODE &&
			    pid_number(esp[-1]) < max_process && 
			    (pid_creation(esp[-1]) == this_creation ||
			     pid_creation(esp[-1]) == 0) &&
			    (tmpp = process_tab[pid_number(esp[-1])])) ||
			   (is_atom(esp[-1]) &&
			    (tmpp = whereis_process(esp[-1]))))) {
		/* Always take over the tracer of the current process */
		tmpp->tracer_proc = p->tracer_proc;
		tmpp->flags |= n;
		esp[-2] = am_true;
	    } else {
		esp[-2] = FAIL_TERM;
	    }
	    --esp;
	    break;
	case matchDisableTrace:
	    n = erts_trace_flag2bit(esp[-1]);
	    if (n != 0) {
		p->flags &= ~n;
		if (!(p->flags & TRACE_FLAGS)) {
		    p->tracer_proc = NIL;
		}
		esp[-1] = am_true;
	    } else {
		esp[-1] = FAIL_TERM;
	    }
	    break;
	case matchDisableTrace2:
	    n = erts_trace_flag2bit(esp[-2]);
	    if (n != 0 && ((is_pid(esp[-1]) && 
			    pid_node(esp[-1]) == THIS_NODE &&
			    pid_number(esp[-1]) < max_process && 
			    (pid_creation(esp[-1]) == this_creation ||
			     pid_creation(esp[-1]) == 0) &&
			    (tmpp = process_tab[pid_number(esp[-1])])) ||
			   (is_atom(esp[-1]) &&
			    (tmpp = whereis_process(esp[-1]))))) {
		tmpp->flags &= ~n;
		if (!(p->flags & TRACE_FLAGS)) {
		    p->tracer_proc = NIL;
		}
		esp[-2] = am_true;
	    } else {
		esp[-2] = FAIL_TERM;
	    }
	    --esp;
	    break;
 	case matchCaller:
 	    if (!(p->cp) || !(hp = find_function_from_pc(p->cp))) {
 		*esp++ = am_undefined;
 	    } else {
 		*esp++ = make_tuple(ehp);
 		ehp[0] = make_arityval(3);
 		ehp[1] = hp[0];
 		ehp[2] = hp[1];
 		ehp[3] = make_small(hp[2]);
 		ehp += 4;
 	    }
 	    break;
	case matchSilent:
	    --esp;
	    if (*esp == am_true)
		p->flags |= F_TRACE_SILENT;
	    else if (*esp == am_false)
		p->flags &= ~F_TRACE_SILENT;
	    break;
	case matchCatch:
	    do_catch = 1;
	    break;
	case matchHalt:
	    goto success;
	default:
	    erl_exit(1, "Internal error: unexpected opcode in match program.");
	}
    }
fail:
    *return_flags = 0U;
    if (fail_label >= 0) { /* We failed during a "TryMeElse", 
			      lets restart, with the next match 
			      program */
	pc = (prog->text) + fail_label;
	goto restart;
    }
    ret = THE_NON_VALUE;
success:
#ifdef DMC_DEBUG
	if (*heap_fence != FENCE_PATTERN) {
	    erl_exit(1, "Heap fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, *heap_fence);
	}
	if (*eheap_fence != FENCE_PATTERN) {
	    erl_exit(1, "Eheap fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, 
		     *eheap_fence);
	}
	if (*stack_fence != FENCE_PATTERN) {
	    erl_exit(1, "Stack fence overwritten in db_prog_match after op "
		     "0x%08x, overwritten with 0x%08x.", save_op, 
		     *stack_fence);
	}
#endif
    return ret;
#undef FAIL
#undef FAIL_TERM
}

/*
 * Convert a match program to a "magic" binary to return up to erlang
 */
Eterm db_make_mp_binary(Process *p, Binary *mp) {
    ProcBin *pb;
    mp->refc++;
    pb = (ProcBin *) HAlloc(p, PROC_BIN_SIZE);
    pb->thing_word = HEADER_PROC_BIN;
    pb->size = 0;
    pb->next = p->off_heap.mso;
    p->off_heap.mso = pb;
    pb->val = mp;
    pb->bytes = mp->orig_bytes;
    return make_binary(pb);
}

DMCErrInfo *db_new_dmc_err_info(void) 
{
    DMCErrInfo *ret = safe_alloc(sizeof(DMCErrInfo));
    ret->var_trans = NULL;
    ret->num_trans = 0;
    ret->error_added = 0;
    ret->first = NULL;
    return ret;
}

Eterm db_format_dmc_err_info(Process *p, DMCErrInfo *ei)
{
    int ll,sl;
    int vnum;
    DMCError *tmp;
    Eterm *lhp, *shp;
    Eterm ret = NIL, tpl, sev;
    char buff[DMC_ERR_STR_LEN + 20 /* for the number */];

    ll = 0;
    for (tmp = ei->first; tmp != NULL; tmp = tmp->next)
	++ll;
    lhp = HAlloc(p, ll * (2 /*cons cell*/ + 3 /*tuple of arity 2*/));
    for (tmp = ei->first; tmp != NULL; tmp = tmp->next) {
	if (tmp->variable >= 0 && 
	    tmp->variable < ei->num_trans &&
	    ei->var_trans != NULL) {
	    vnum = (int) ei->var_trans[tmp->variable];
	} else {
	    vnum = tmp->variable;
	}
	if (vnum >= 0)
	    sprintf(buff,tmp->error_string, vnum);
	else
	    strcpy(buff,tmp->error_string);
	sl = strlen(buff);
	shp = HAlloc(p, sl * 2);
	sev = (tmp->severity == dmcWarning) ? 
	    am_atom_put("warning",7) :
	    am_error;
	tpl = TUPLE2(lhp, sev, buf_to_intlist(&shp, buff, sl, NIL));
	lhp += 3;
	ret = CONS(lhp, tpl, ret);
	lhp += 2;
    }
    return ret;
}

void db_free_dmc_err_info(DMCErrInfo *ei){
    while (ei->first != NULL) {
	DMCError *ll = ei->first->next;
	sys_free(ei->first);
	ei->first = ll;
    }
    sys_free(ei);
}

#define FIX_BIG_SIZE 16
#define MAX_NEED(x,y) (((x)>(y)) ? (x) : (y))

static Eterm  big_tmp[2];
Eterm  db_big_buf[FIX_BIG_SIZE];

Eterm add_counter(Eterm counter, Eterm incr)
{
    Eterm res;
    Sint ires;
    Eterm arg1;
    Eterm arg2;
    Uint sz1;
    Uint sz2;
    Uint need;
    Eterm *ptr;
    int i;

    if (is_small(counter) && is_small(incr)) {
	ires = signed_val(counter) + signed_val(incr);
	if (IS_SSMALL(ires))
	    return make_small(ires);
	else
	    return small_to_big(ires, db_big_buf);
    }
    else {
	switch(i = NUMBER_CODE(counter, incr)) {
	case SMALL_BIG:
	    arg1 = small_to_big(signed_val(counter), big_tmp);
	    arg2 = incr;
	    break;
	case BIG_SMALL:
	    arg1 = counter;
	    arg2 = small_to_big(signed_val(incr), big_tmp);
	    break;
	case BIG_BIG:
	    arg1 = incr;
	    arg2 = counter;
	    break;
	default:
	    return THE_NON_VALUE;
	}
	sz1 = big_size(arg1);
	sz2 = big_size(arg2);
	sz1 = MAX_NEED(sz1,sz2)+1;
	need = BIG_NEED_SIZE(sz1);
	if (need <= FIX_BIG_SIZE)
	    ptr = db_big_buf;
	else {
	    if ((ptr = sys_alloc_from(53,need*sizeof(Eterm))) == NULL)
		return NIL;  /* system limit */
	}
	res = big_plus(arg1, arg2, ptr);
	if (is_small(res) || is_nil(res)) {
	    if (ptr != db_big_buf)
		sys_free(ptr);
	}
	return res;
    }
}

/*
** The actual update of a counter, a lot of parameters are needed:
** p: The calling process (BIF_P), 
** bp: A pointer to the pointer to the object to be updated (extra 
** indirection for the reallocation), this pointer is only used to
** pass information to the realloc function.
** tpl: A pointer to the tuple in the DbTerm.
** keypos: The key position in the DbTerm.
** realloc_fun: A function that does the reallocation, it takes 
** bp, new size, new_value and keypos as parameter. 
** ret: pointer to where the result is put.
** Returns normal DB error code.
*/

int db_do_update_counter(Process *p, void *bp /* XDbTerm **bp */, 
			 Eterm *tpl, int counterpos,
			 int (*realloc_fun)(void *, Uint, Eterm, int),
			 Eterm incr,
			 Eterm *ret)
{
    Eterm counter;
    Eterm *counterp;
    Eterm res; /* In register? */


    /*if (make_arityval(keypos+1) != *tpl)
	return DB_ERROR_BADITEM;*/
    if (arityval(*tpl) < counterpos || !(is_small(tpl[counterpos]) ||
					 is_big(tpl[counterpos])))
	return DB_ERROR_BADITEM;

    counterp = tpl + counterpos;
    counter = *counterp;

    if ((res = add_counter(counter, incr)) == NIL) 
	return DB_ERROR_SYSRES;
    else if (is_non_value(res))
	return DB_ERROR_UNSPEC;

    if (is_small(res)) {
	if (is_small(counter)) {
	    *counterp = res;
	} else {
	    if ((*realloc_fun)(bp, 0, res, counterpos) < 0) 
		return DB_ERROR_SYSRES;
	}
	*ret = res;
	return DB_ERROR_NONE;
    } else {
	Eterm *ptr = big_val(res);
	Uint sz = BIG_ARITY(ptr) + 1;
	Eterm *hp;

	if ((*realloc_fun)(bp, sz, res, counterpos) < 0) 
	    return DB_ERROR_SYSRES;
	hp = HAlloc(p, sz);
	sys_memcpy(hp, ptr, sz*sizeof(Eterm));
	res = make_big(hp);
	hp += sz;
	if (ptr != db_big_buf)
	    sys_free(ptr);
	*ret = res;
	return DB_ERROR_NONE;
    }
}   

/*
** Copy the object into a possibly new DbTerm, 
** offset is the offset of the DbTerm from the start
** of the sysAllocaed structure, The possibly realloced and copied
** structure is returned. Make sure (((char *) old) - offset) is a 
** pointer to sys_alloced data.
*/
void* db_get_term(DbTerm* old, Uint offset, Eterm obj)
{
    int size = size_object(obj);
    void *structp = ((char*) old) - offset;
    DbTerm* p;
    Eterm copy;
    Eterm *top;

    if (old != 0) {
	erts_cleanup_offheap(&old->off_heap);
	if (size == old->size) {
	    p = old;
	} else {
	    structp = safe_realloc(structp,
				   offset + sizeof(DbTerm) + 
				   sizeof(Eterm)*(size-1));
	    p = (DbTerm*) ((void *)(((char *) structp) + offset));
	}
    }
    else {
	structp = safe_alloc_from(52, offset + sizeof(DbTerm) + 
				  sizeof(Eterm)*(size-1));
	p = (DbTerm*) ((void *)(((char *) structp) + offset));
    }
    p->size = size;
    p->off_heap.mso = 0;
    p->off_heap.funs = 0;
    p->off_heap.overhead = 0;

    top = p->v;
    copy = copy_struct(obj, size, &top, &p->off_heap);
    p->tpl = tuple_val(copy);	/* XXX: this _is_ always a tuple, right? */
    return structp;
}


void db_free_term_data(DbTerm* p)
{
    erts_cleanup_offheap(&p->off_heap);
}


/*
** Copy the new counter value into the DbTerm at ((char *) *bp) + offset,
** Allocate new structure of (needed size + offset) if that DbTerm
** is to small. When changing size, the old structure is 
** freed using sys_free, make sure this can be done (((char *) b) - offset
** is a pointer to a sys_alloc'ed area).
** bp is a pure out parameter, i e it does not have to
** point to (((char *) b) - offset) when calling.
*/
int db_realloc_counter(void** bp, DbTerm *b, Uint offset, Uint sz, 
		       Eterm new_counter, int counterpos)
{
    DbTerm* new;
    void *newbp;
    Eterm  old_counter;
    Uint  old_sz;
    Uint  new_sz;
    Uint  basic_sz;
    Eterm  copy;
    Eterm *top;
    Eterm *ptr;

    old_counter = b->tpl[counterpos];

    if (is_small(old_counter))
	old_sz = 0;
    else {
	top = big_val(old_counter);
	old_sz = BIG_ARITY(top) + 1;
	if (sz == old_sz) {  /* OK we fit in old space */
	    sys_memcpy(top, big_val(new_counter), sz*sizeof(Eterm));
	    return 0;
	}
    }

    basic_sz = b->size - old_sz;
    new_sz = basic_sz + sz;

    newbp = 
	safe_alloc_from(57, sizeof(DbTerm)+sizeof(Eterm)*(new_sz-1)+offset);

    if (newbp == NULL)
	return -1;

    new = (DbTerm*) ((void *)(((char *) newbp) + offset));
    memcpy(newbp, ((char *) b) - offset, offset); 
   
    new->size = new_sz;
    new->off_heap.mso = NULL;
    new->off_heap.funs = NULL;
    new->off_heap.overhead = 0;
    top = new->v;

    b->tpl[counterpos] = SMALL_ZERO;               /* zap, do not copy */

    /* copy term (except old counter) */
    copy = copy_struct(make_tuple(b->tpl), basic_sz, 
		       &top, &new->off_heap);
    new->tpl = tuple_val(copy);

    db_free_term_data(b);
    sys_free(((char *) b) - offset);  /* free old term */
    *bp = newbp;     /* patch new */

    /* copy new counter */
    if (sz == 0)
	new->tpl[counterpos] = new_counter;  /* must be small !!! */
    else {
	ptr = big_val(new_counter);
	sys_memcpy(top, ptr, sz*sizeof(Eterm));
	new->tpl[counterpos] = make_big(top);
    }
    return 0;
}

/*
** Check if object represents a "match" variable 
** i.e and atom $N where N is an integer 
**
*/

int db_is_variable(Eterm obj)
{
    byte *b;
    int n;
    int N;

    if (is_not_atom(obj))
        return -1;
    b = atom_tab(atom_val(obj))->name;
    if ((n = atom_tab(atom_val(obj))->len) < 2)
        return -1;
    if (*b++ != '$')
        return -1;
    n--;
    /* Handle first digit */
    if (*b == '0')
        return (n == 1) ? 0 : -1;
    if (*b >= '1' && *b <= '9')
        N = *b++ - '0';
    else
        return -1;
    n--;
    while(n--) {
        if (*b >= '0' && *b <= '9') {
            N = N*10 + (*b - '0');
            b++;
        }
        else
            return -1;
    }
    return N;
}


/* check if obj is (or contains) a variable */
/* return 1 if obj contains a variable or underscore */
/* return 0 if obj is fully ground                   */

int db_has_variable(Eterm obj)
{
    switch(obj & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST: {
	while (is_list(obj)) {
	    if (db_has_variable(CAR(list_val(obj))))
		return 1;
	    obj = CDR(list_val(obj));
	}
	return(db_has_variable(obj));  /* Non wellformed list or [] */
    }
    case TAG_PRIMARY_BOXED: 
	if (!BOXED_IS_TUPLE(obj)) {
	    return 0;
	} else {
	    Eterm *tuple = tuple_val(obj);
	    int arity = arityval(*tuple++);
	    while(arity--) {
		if (db_has_variable(*tuple))
		    return 1;
		tuple++;
	    }
	    return(0);
	}
    case TAG_PRIMARY_IMMED1:
	if (obj == am_Underscore || db_is_variable(obj) >= 0)
	    return 1;
    }
    return 0;
}

/* 
** Local (static) utilities.
*/

/*
***************************************************************************
** Compiled matches 
***************************************************************************
*/
/*
** Utility to add an error
*/

static void add_dmc_err(DMCErrInfo *err_info, 
			char *str,
			int variable,
			Eterm term,
			DMCErrorSeverity severity)
{
    /* Linked in in reverse order, to ease the formatting */
    DMCError *e = safe_alloc(sizeof(DMCError));
    if (term != 0UL) {
	cerr_pos = 0;
	display(term, CBUF);
	tmp_buf[cerr_pos] = '\0';
	if (strlen(str) + strlen(tmp_buf) + 1 <= DMC_ERR_STR_LEN) {
	    sprintf(e->error_string, str, tmp_buf);
	} else {
	    strncpy(e->error_string, str, DMC_ERR_STR_LEN);
	    e->error_string[DMC_ERR_STR_LEN] ='\0';
	}
	variable = -1; /* variable and term are mutually exclusive */
    } else {
	strncpy(e->error_string, str, DMC_ERR_STR_LEN);
	e->error_string[DMC_ERR_STR_LEN] ='\0';
    }
    e->variable = variable;
    e->severity = severity;
    e->next = err_info->first;
#ifdef HARDDEBUG
    erl_printf(CERR,"add_dmc_err: %s\n",e->error_string);
#endif
    err_info->first = e;
    if (severity >= dmcError)
	err_info->error_added = 1;
}
    
/*
** Handle one term in the match expression (not the guard) 
*/
static DMCRet dmc_one_term(DMCContext *context, 
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Eterm) *stack,
			   DMC_STACK_TYPE(Uint) *text,
			   Eterm c)
{
    Sint n;
    Eterm *hp;
    ErlHeapFragment *tmp_mb;
    Uint sz, sz2, sz3;
    Uint i, j;


    switch (c & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_IMMED1:
	if ((n = db_is_variable(c)) >= 0) { /* variable */
	    if (n >= heap->size) {
		/*
		** Ouch, big integer in match variable.
		*/
		Eterm *save_hp;
		ASSERT(heap->data == heap->def);
		sz = sz2 = sz3 = 0;
		for (j = 0; j < context->num_match; ++j) {
		    sz += size_object(context->matchexpr[j]);
		    sz2 += size_object(context->guardexpr[j]);
		    sz3 += size_object(context->bodyexpr[j]);
		}
		context->copy = 
		    new_message_buffer(sz + sz2 + sz3 +
				       context->num_match);
		save_hp = hp = context->copy->mem;
		hp += context->num_match;
		for (j = 0; j < context->num_match; ++j) {
		    context->matchexpr[j] = 
			copy_struct(context->matchexpr[j], 
				    size_object(context->matchexpr[j]), &hp, 
				    &(context->copy->off_heap));
		    context->guardexpr[j] = 
			copy_struct(context->guardexpr[j], 
				    size_object(context->guardexpr[j]), &hp, 
				    &(context->copy->off_heap));
		    context->bodyexpr[j] = 
			copy_struct(context->bodyexpr[j], 
				    size_object(context->bodyexpr[j]), &hp, 
				    &(context->copy->off_heap));
		}
		for (j = 0; j < context->num_match; ++j) {
		    /* the actual expressions can be 
		       atoms in their selves, place them first */
		    *save_hp++ = context->matchexpr[j]; 
		}
		heap->size = match_compact(context->copy, 
					   context->err_info);
		for (j = 0; j < context->num_match; ++j) {
		    /* restore the match terms, as they
		       may be atoms that changed */
		    context->matchexpr[j] = context->copy->mem[j];
		}
		heap->data = safe_alloc(heap->size * 
					sizeof(unsigned));
		sys_memset(heap->data, 0, 
			   heap->size * sizeof(unsigned));
		DMC_CLEAR(*stack);
		/*DMC_PUSH(*stack,NIL);*/
		DMC_CLEAR(*text);
		return retRestart;
	    }
	    if (heap->data[n]) { /* already bound ? */
		DMC_PUSH(*text,matchCmp);
		DMC_PUSH(*text,n);
	    } else { /* Not bound, bind! */
		if (n >= heap->used)
		    heap->used = n + 1;
		DMC_PUSH(*text,matchBind);
		DMC_PUSH(*text,n);
		heap->data[n] = 1;
	    }
	} else if (c == am_Underscore) {
	    DMC_PUSH(*text, matchSkip);
	} else { /* Any immediate value */
	    DMC_PUSH(*text, matchEq);
	    DMC_PUSH(*text, (Uint) c);
	}
	break;
    case TAG_PRIMARY_LIST:
	DMC_PUSH(*text, matchPushL);
	++(context->stack_used);
	DMC_PUSH(*stack, c); 
	break;
    case TAG_PRIMARY_BOXED: {
	Eterm hdr = *boxed_val(c);
	switch ((hdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE):    
	    n = arityval(*tuple_val(c));
	    DMC_PUSH(*text, matchPushT);
	    ++(context->stack_used);
	    DMC_PUSH(*text, n);
	    DMC_PUSH(*stack, c);
	    break;
	case (_TAG_HEADER_REF >> _TAG_PRIMARY_SIZE):
	    n = thing_arityval(*ref_val(c));
	    DMC_PUSH(*text, matchEqRef);
	    DMC_PUSH(*text, *ref_val(c));
	    for (i = 1; i <= n; ++i) {
		DMC_PUSH(*text, (Uint) ref_val(c)[i]);
	    }
	    break;
	case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
	    n = thing_arityval(*big_val(c));
	    DMC_PUSH(*text, matchEqBig);
	    DMC_PUSH(*text, *big_val(c));
	    for (i = 1; i <= n; ++i) {
		DMC_PUSH(*text, (Uint) big_val(c)[i]);
	    }
	    break;
	case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
	    DMC_PUSH(*text,matchEqFloat);
	    DMC_PUSH(*text, (Uint) float_val(c)[1]);
	    DMC_PUSH(*text, (Uint) float_val(c)[2]);
	    break;
	default: /* BINARY,FUN or VECTOR */
	    /*
	    ** Make a private copy...
	    */
	    n = size_object(c);
	    tmp_mb = new_message_buffer(n);
	    hp = tmp_mb->mem;
	    DMC_PUSH(*text, matchEqBin);
	    DMC_PUSH(*text, copy_struct(c, n, &hp, &(tmp_mb->off_heap)));
	    tmp_mb->next = context->save;
	    context->save = tmp_mb;
	    break;
	}
	break;
    }
    default:
	erl_exit(1, "db_match_compile: "
		 "Bad object on heap: 0x%08lx\n",
		 (unsigned long) c);
    }
    return retOk;
}

/*
** Match guard compilation
*/

static void do_emit_constant(DMCContext *context, DMC_STACK_TYPE(Uint) *text,
			     Eterm t) 
{
	int sz;
	ErlHeapFragment *emb;
	Eterm *hp;
	Eterm tmp;

	if (IS_CONST(t)) {
	    tmp = t;
	} else {
	    sz = my_size_object(t);
	    emb = new_message_buffer(sz);
	    hp = emb->mem;
	    tmp = my_copy_struct(t,&hp,&(emb->off_heap));
	    emb->next = context->save;
	    context->save = emb;
	}
	DMC_PUSH(*text,matchPushC);
	DMC_PUSH(*text,(Uint) tmp);
	if (++context->stack_used > context->stack_need)
	    context->stack_need = context->stack_used;
}

#define RETURN_ERROR_X(String, X, Y, ContextP, ConstantF)        \
do {                                                            \
if ((ContextP)->err_info != NULL) {				\
    (ConstantF) = 0;						\
    add_dmc_err((ContextP)->err_info, String, X, Y, dmcError);  \
    return retOk;						\
} else 								\
  return retFail;                                                \
} while(0)

#define RETURN_ERROR(String, ContextP, ConstantF) \
     RETURN_ERROR_X(String, -1, 0UL, ContextP, ConstantF)

#define RETURN_VAR_ERROR(String, N, ContextP, ConstantF) \
     RETURN_ERROR_X(String, N, 0UL, ContextP, ConstantF)

#define RETURN_TERM_ERROR(String, T, ContextP, ConstantF) \
     RETURN_ERROR_X(String, -1, T, ContextP, ConstantF)

#define WARNING(String, ContextP) \
add_dmc_err((ContextP)->err_info, String, -1, 0UL, dmcWarning)

#define VAR_WARNING(String, N, ContextP) \
add_dmc_err((ContextP)->err_info, String, N, 0UL, dmcWarning)

#define TERM_WARNING(String, T, ContextP) \
add_dmc_err((ContextP)->err_info, String, -1, T, dmcWarning)

static DMCRet dmc_list(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant)
{
    int c1;
    int c2;
    int ret;

    if ((ret = dmc_expr(context, heap, text, CAR(list_val(t)), &c1)) != retOk)
	return ret;

    if ((ret = dmc_expr(context, heap, text, CDR(list_val(t)), &c2)) != retOk)
	return ret;

    if (c1 && c2) {
	*constant = 1;
	return retOk;
    } 
    *constant = 0;
    if (!c1) {
	/* The CAR is not a constant, so if the CDR is, we just push it,
	   otherwise it is already pushed. */
	if (c2)
	    do_emit_constant(context, text, CDR(list_val(t)));
	DMC_PUSH(*text, matchConsA);
    } else { /* !c2 && c1 */
	do_emit_constant(context, text, CAR(list_val(t)));
	DMC_PUSH(*text, matchConsB);
    }
    --context->stack_used; /* Two objects on stack becomes one */
    context->eheap_need += 2;
    return retOk;
}

static DMCRet dmc_tuple(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant)
{
    DMC_STACK_TYPE(Uint) instr_save;
    int all_constant = 1;
    int textpos = DMC_STACK_NUM(*text);
    Eterm *p = tuple_val(t);
    Uint nelems = arityval(*p);
    Uint i;
    int c;
    DMCRet ret;
    Uint lblpos = DMC_STACK_NUM(*(context->labels));


    /*
    ** We remember where we started to layout code, 
    ** assume all is constant and back up and restart if not so.
    ** The tuple should be laid out with the last element first,
    ** so we can memcpy the tuple to the eheap;
    */
    for (i = nelems; i > 0; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (!c && all_constant) {
	    all_constant = 0;
	    if (i < nelems) {
		Uint j;
		Uint txtnum = DMC_STACK_NUM(*text);
		/* oups, we need to relayout the constantants */
		/* save the already laid out instructions */
		DMC_INIT_STACK(instr_save);
		while (DMC_STACK_NUM(*text) > textpos) 
		    DMC_PUSH(instr_save, DMC_POP(*text));
		for (j = nelems; j > i; --j)
		    do_emit_constant(context, text, p[j]);
		while(!DMC_EMPTY(instr_save))
		    DMC_PUSH(*text, DMC_POP(instr_save));
		if (DMC_STACK_NUM(*(context->labels)) != lblpos) {
		    /*
		    ** This is NOT funny... All labels have to be offset
		    ** By how many instructions we inserted during 
		    ** constant emission...
		    */
		    int diff = DMC_STACK_NUM(*text) - txtnum;
		    Uint nlblpos = DMC_STACK_NUM(*(context->labels));
		    for (j = lblpos; j < nlblpos; ++j) {
			DMC_POKE(*(context->labels), j, 
				 (Uint) DMC_PEEK(*(context->labels), j) + 
				 diff);
		    }
		}
		DMC_FREE(instr_save);
	    }
	} else if (c && !all_constant) {
	    /* push a constant */
	    do_emit_constant(context, text, p[i]);
	}
    }
    
    if (all_constant) {
	*constant = 1;
	return retOk;
    }
    DMC_PUSH(*text, matchMkTuple);
    DMC_PUSH(*text, nelems);
    context->stack_used -= (nelems - 1);
    context->eheap_need += (nelems + 1);
    *constant = 0;
    return retOk;
}

static DMCRet dmc_whole_expression(DMCContext *context,
				   DMCHeap *heap,
				   DMC_STACK_TYPE(Uint) *text,
				   Eterm t,
				   int *constant)
{
    if (context->cflags == DCOMP_TRACE) {
	/* Hmmm, convert array to list... */
	if (context->special) {
	   DMC_PUSH(*text, matchPushArrayAsListU);
	} else { 
	    ASSERT(is_tuple(context->matchexpr
			    [context->current_match]));
	    context->eheap_need += 
		arityval(*(tuple_val(context->matchexpr
				     [context->current_match]))) * 2;
	    DMC_PUSH(*text, matchPushArrayAsList);
	}
    } else {
	DMC_PUSH(*text, matchPushExpr);
    }
    ++context->stack_used;
    if (context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    *constant = 0;
    return retOk;
}

static DMCRet dmc_variable(DMCContext *context,
			   DMCHeap *heap,
			   DMC_STACK_TYPE(Uint) *text,
			   Eterm t,
			   int *constant)
{
    Uint n = db_is_variable(t);
    ASSERT(n >= 0);
    if (n >= heap->used) 
	RETURN_VAR_ERROR("Variable $%d is unbound.", n, context, *constant);
    if (heap->data[n] == 0U)
	RETURN_VAR_ERROR("Variable $%d is unbound.", n, context, *constant);
    DMC_PUSH(*text, matchPushV);
    DMC_PUSH(*text, n);
    ++context->stack_used;
    if (context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    *constant = 0;
    return retOk;
}

static DMCRet dmc_all_bindings(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(Uint) *text,
			       Eterm t,
			       int *constant)
{
    int i;
    int heap_used = 0;

    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, NIL);
    for (i = heap->used - 1; i >= 0; --i) { 
	if (heap->data[i]) {
	    DMC_PUSH(*text, matchPushV);
	    DMC_PUSH(*text, i);
	    DMC_PUSH(*text, matchConsB);
	    heap_used += 2;
	}
    }
    ++context->stack_used;
    if ((context->stack_used + 1) > context->stack_need)
	context->stack_need = (context->stack_used + 1);
    context->eheap_need += heap_used;
    *constant = 0;
    return retOk;
}

static DMCRet dmc_const(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'const' called with more than one "
			  "argument in %s.", t, context, *constant);
    }
    *constant = 1;
    return retOk;
}

static DMCRet dmc_and(DMCContext *context,
		      DMCHeap *heap,
		      DMC_STACK_TYPE(Uint) *text,
		      Eterm t,
		      int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'and' called without arguments "
			  "in %s.", t, context, *constant);
    }
    *constant = 0;
    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    DMC_PUSH(*text, matchAnd);
    DMC_PUSH(*text, (Uint) a - 1);
    context->stack_used -= (a - 2);
    return retOk;
}

static DMCRet dmc_or(DMCContext *context,
		     DMCHeap *heap,
		     DMC_STACK_TYPE(Uint) *text,
		     Eterm t,
		     int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'or' called without arguments "
			  "in %s.", t, context, *constant);
    }
    *constant = 0;
    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    DMC_PUSH(*text, matchOr);
    DMC_PUSH(*text, (Uint) a - 1);
    context->stack_used -= (a - 2);
    return retOk;
}


static DMCRet dmc_andthen(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(Uint) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    int lbl;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'andalso/andthen' called without"
			  " arguments "
			  "in %s.", t, context, *constant);
    }
    *constant = 0;
    lbl = DMC_STACK_NUM(*(context->labels));
    DMC_PUSH(*(context->labels), 0);
    for (i = 2; i <= a; ++i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
	DMC_PUSH(*text, matchAndThen);
	DMC_PUSH(*text, (Uint) lbl);
	--(context->stack_used);
    }
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    DMC_POKE(*(context->labels), lbl, DMC_STACK_NUM(*text));
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_orelse(DMCContext *context,
			 DMCHeap *heap,
			 DMC_STACK_TYPE(Uint) *text,
			 Eterm t,
			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int i;
    int c;
    int lbl;
    
    if (a < 2) {
	RETURN_TERM_ERROR("Special form 'orelse' called without arguments "
			  "in %s.", t, context, *constant);
    }
    *constant = 0;
    lbl = DMC_STACK_NUM(*(context->labels));
    DMC_PUSH(*(context->labels), 0);
    for (i = 2; i <= a; ++i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
	DMC_PUSH(*text, matchOrElse);
	DMC_PUSH(*text, (Uint) lbl);
	--(context->stack_used);
    }
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_false);
    DMC_POKE(*(context->labels), lbl, DMC_STACK_NUM(*text));
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_message(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(Uint) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'message' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'message' called in guard context.",
		     context, 
		     *constant);
    }

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'message' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchReturn);
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}

static DMCRet dmc_self(DMCContext *context,
		     DMCHeap *heap,
		     DMC_STACK_TYPE(Uint) *text,
		     Eterm t,
		     int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'self' called with arguments "
			  "in %s.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchSelf);
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_return_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(Uint) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'return_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'return_trace' called in "
		     "guard context.", context, *constant);
    }

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'return_trace' called with "
			  "arguments in %s.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchSetReturnTrace); /* Pushes 'true' on the stack */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}



static DMCRet dmc_is_seq_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(Uint) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'is_seq_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'is_seq_trace' called with "
			  "arguments in %s.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchIsSeqTrace); 
    /* Pushes 'true' or 'false' on the stack */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_set_seq_token(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(Uint) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'set_seq_token' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'set_seq_token' called in "
		     "guard context.", context, *constant);
    }

    if (a != 3) {
	RETURN_TERM_ERROR("Special form 'set_seq_token' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[3]);
    }
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchSetSeqToken);
    --context->stack_used; /* Remove two and add one */
    return retOk;
}

static DMCRet dmc_get_seq_token(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(Uint) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'get_seq_token' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'get_seq_token' called in "
		     "guard context.", context, *constant);
    }
    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'get_seq_token' called with "
			  "arguments in %s.", t, context, 
			  *constant);
    }

    *constant = 0;
    DMC_PUSH(*text, matchGetSeqToken);
    context->eheap_need += 6;     /* A 5-tuple is built */
    if (++context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}



static DMCRet dmc_display(DMCContext *context,
			  DMCHeap *heap,
			  DMC_STACK_TYPE(Uint) *text,
			  Eterm t,
			  int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'display' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'display' called in guard context.",
		     context, 
		     *constant);
    }

    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'display' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchDisplay);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}

static DMCRet dmc_process_dump(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(Uint) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    
    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'process_dump' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'process_dump' called in "
		     "guard context.", context, *constant);
    }

    if (a != 1) {
	RETURN_TERM_ERROR("Special form 'process_dump' called with "
			  "arguments in %s.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchProcessDump); /* Creates binary */
    if (++context->stack_used > context->stack_need)
	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_enable_trace(DMCContext *context,
			       DMCHeap *heap,
			       DMC_STACK_TYPE(Uint) *text,
			       Eterm t,
			       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'enable_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'enable_trace' called in guard context.",
		     context, 
		     *constant);
    }

    switch (a) {
    case 2:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchEnableTrace);
	/* Push as much as we remove, stack_need is untouched */
	break;
    case 3:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchEnableTrace2);
	--context->stack_used; /* Remove two and add one */
	break;
    default:
	RETURN_TERM_ERROR("Special form 'enable_trace' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    return retOk;
}

static DMCRet dmc_disable_trace(DMCContext *context,
				DMCHeap *heap,
				DMC_STACK_TYPE(Uint) *text,
				Eterm t,
				int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
    

    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'disable_trace' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
	RETURN_ERROR("Special form 'disable_trace' called in guard context.",
		     context, 
		     *constant);
    }

    switch (a) {
    case 2:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchDisableTrace);
	/* Push as much as we remove, stack_need is untouched */
	break;
    case 3:
	*constant = 0;
	if ((ret = dmc_expr(context, heap, text, p[3], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[3]);
	}
	if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	    return ret;
	}
	if (c) { 
	    do_emit_constant(context, text, p[2]);
	}
	DMC_PUSH(*text, matchDisableTrace2);
	--context->stack_used; /* Remove two and add one */
	break;
    default:
	RETURN_TERM_ERROR("Special form 'disable_trace' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    return retOk;
}



static DMCRet dmc_caller(DMCContext *context,
 			 DMCHeap *heap,
 			 DMC_STACK_TYPE(Uint) *text,
 			 Eterm t,
 			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
     
    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'caller' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
 	RETURN_ERROR("Special form 'caller' called in "
 		     "guard context.", context, *constant);
    }
  
    if (a != 1) {
 	RETURN_TERM_ERROR("Special form 'caller' called with "
 			  "arguments in %s.", t, context, *constant);
    }
    *constant = 0;
    DMC_PUSH(*text, matchCaller); /* Creates binary */
    context->eheap_need += 4;     /* A 3-tuple is built */
    if (++context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}


  
static DMCRet dmc_silent(DMCContext *context,
 			 DMCHeap *heap,
 			 DMC_STACK_TYPE(Uint) *text,
 			 Eterm t,
 			 int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    DMCRet ret;
    int c;
     
    if (context->cflags != DCOMP_TRACE) {
	RETURN_ERROR("Special form 'silent' used in wrong dialect.",
		     context, 
		     *constant);
    }
    if (context->is_guard) {
 	RETURN_ERROR("Special form 'silent' called in "
 		     "guard context.", context, *constant);
    }
  
    if (a != 2) {
	RETURN_TERM_ERROR("Special form 'silent' called with wrong "
			  "number of arguments in %s.", t, context, 
			  *constant);
    }
    *constant = 0;
    if ((ret = dmc_expr(context, heap, text, p[2], &c)) != retOk) {
	return ret;
    }
    if (c) { 
	do_emit_constant(context, text, p[2]);
    }
    DMC_PUSH(*text, matchSilent);
    DMC_PUSH(*text, matchPushC);
    DMC_PUSH(*text, am_true);
    /* Push as much as we remove, stack_need is untouched */
    return retOk;
}
  


static DMCRet dmc_fun(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant)
{
    Eterm *p = tuple_val(t);
    Uint a = arityval(*p);
    int c;
    int i;
    DMCRet ret;
    DMCGuardBif *b;
 
    /* Special forms. */
    switch (p[1]) {
    case am_const:
	return dmc_const(context, heap, text, t, constant);
    case am_and:
	return dmc_and(context, heap, text, t, constant);
    case am_or:
	return dmc_or(context, heap, text, t, constant);
    case am_andalso:
    case am_andthen:
	return dmc_andthen(context, heap, text, t, constant);
    case am_orelse:
	return dmc_orelse(context, heap, text, t, constant);
    case am_self:
	return dmc_self(context, heap, text, t, constant);
    case am_message:
	return dmc_message(context, heap, text, t, constant);
    case am_is_seq_trace:
	return dmc_is_seq_trace(context, heap, text, t, constant);
    case am_set_seq_token:
	return dmc_set_seq_token(context, heap, text, t, constant);
    case am_get_seq_token:
	return dmc_get_seq_token(context, heap, text, t, constant);
    case am_return_trace:
	return dmc_return_trace(context, heap, text, t, constant);
    case am_display:
	return dmc_display(context, heap, text, t, constant);
    case am_process_dump:
	return dmc_process_dump(context, heap, text, t, constant);
    case am_enable_trace:
	return dmc_enable_trace(context, heap, text, t, constant);
    case am_disable_trace:
	return dmc_disable_trace(context, heap, text, t, constant);
    case am_caller:
 	return dmc_caller(context, heap, text, t, constant);
    case am_silent:
 	return dmc_silent(context, heap, text, t, constant);
    }

    b = dmc_lookup_bif(p[1], ((int) a) - 1);

    if (b == NULL) {
	if (context->err_info != NULL) {
	    /* Ugly, should define a better RETURN_TERM_ERROR interface... */
	    char buff[100];
	    sprintf(buff, "Function %%s/%d does_not_exist.", (int)a - 1);
	    RETURN_TERM_ERROR(buff, p[1], context, *constant);
	} else {
	    return retFail;
	}
    } 
    ASSERT(b->arity == ((int) a) - 1);
    if (! (b->flags & 
	   (1 << (context->cflags + 
		  (context->is_guard ? DBIF_GUARD : DBIF_BODY))))) {
	/* Body clause used in wrong context. */
	if (context->err_info != NULL) {
	    /* Ugly, should define a better RETURN_TERM_ERROR interface... */
	    char buff[100];
	    sprintf(buff, 
		    "Function %%s/%d cannot be called in this context.",
		    (int)a - 1);
	    RETURN_TERM_ERROR(buff, p[1], context, *constant);
	} else {
	    return retFail;
	}
    }	

    *constant = 0;

    for (i = a; i > 1; --i) {
	if ((ret = dmc_expr(context, heap, text, p[i], &c)) != retOk)
	    return ret;
	if (c) 
	    do_emit_constant(context, text, p[i]);
    }
    switch (b->arity) {
    case 0:
	DMC_PUSH(*text, matchCall0);
	break;
    case 1:
	DMC_PUSH(*text, matchCall1);
	break;
    case 2:
	DMC_PUSH(*text, matchCall2);
	break;
    case 3:
	DMC_PUSH(*text, matchCall3);
	break;
    default:
	erl_exit(1,"ets:match() internal error, "
		 "guard with more than 3 arguments.");
    }
    DMC_PUSH(*text, (Uint) b->biff);
    context->stack_used -= (((int) a) - 2);
    if (context->stack_used > context->stack_need)
 	context->stack_need = context->stack_used;
    return retOk;
}

static DMCRet dmc_expr(DMCContext *context,
		       DMCHeap *heap,
		       DMC_STACK_TYPE(Uint) *text,
		       Eterm t,
		       int *constant)
{
    DMCRet ret;
    Eterm tmp;
    Eterm *p;


    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	if ((ret = dmc_list(context, heap, text, t, constant)) != retOk)
	    return ret;
	break;
    case TAG_PRIMARY_BOXED:
	if (!BOXED_IS_TUPLE(t)) {
	    goto simple_term;
	}
	p = tuple_val(t);
#ifdef HARDDEBUG
	erl_printf(CERR,"%d %d %d %d\r\n",arityval(*p),is_tuple(tmp = p[1]),
		   is_atom(p[1]),db_is_variable(p[1]));
#endif
	if (arityval(*p) == 1 && is_tuple(tmp = p[1])) {
	    if ((ret = dmc_tuple(context, heap, text, tmp, constant)) != retOk)
		return ret;
	} else if (arityval(*p) >= 1 && is_atom(p[1]) && 
		   !(db_is_variable(p[1]) >= 0)) {
	    if ((ret = dmc_fun(context, heap, text, t, constant)) != retOk)
		return ret;
	} else
	    RETURN_TERM_ERROR("%s is neither a function call, nor a tuple "
			      "(tuples are written {{ ... }}).", t,
			      context, *constant);
	break;
    case TAG_PRIMARY_IMMED1:
	if (db_is_variable(t) >= 0) {
	    if ((ret = dmc_variable(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	} else if (t == am_DollarUnderscore) {
	    if ((ret = dmc_whole_expression(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	} else if (t == am_DollarDollar) {
	    if ((ret = dmc_all_bindings(context, heap, text, t, constant)) 
		!= retOk)
		return ret;
	    break;
	}	    
	/* Fall through */
    default:
    simple_term:
	*constant = 1;
    }
    return retOk;
}

    
static DMCRet compile_guard_expr(DMCContext *context,
				 DMCHeap *heap,
				 DMC_STACK_TYPE(Uint) *text,
				 Eterm l)
{
    DMCRet ret;
    int constant;
    Eterm t;

    if (l != NIL) {
	if (!is_list(l))
	    RETURN_ERROR("Match expression is not a list.", 
			 context, constant);
	if (!(context->is_guard)) {
	    DMC_PUSH(*text, matchCatch);
	}
	while (is_list(l)) {
	    constant = 0;
	    t = CAR(list_val(l));
	    if ((ret = dmc_expr(context, heap, text, t, &constant)) !=
		retOk)
		return ret;
	    if (constant) {
		do_emit_constant(context, text, t);
	    }
	    l = CDR(list_val(l));
	    if (context->is_guard) {
		DMC_PUSH(*text,matchTrue);
	    } else {
		DMC_PUSH(*text,matchWaste);
	    }
	    --context->stack_used;
	}
	if (l != NIL) 
	    RETURN_ERROR("Match expression is not a proper list.",
			 context, constant);
	if (!(context->is_guard) && (context->cflags == DCOMP_TABLE)) {
	    ASSERT(matchWaste == DMC_TOP(*text));
	    (void) DMC_POP(*text);
	    DMC_PUSH(*text, matchReturn); /* Same impact on stack as 
					     matchWaste */
	}
    }
    return retOk;
}




/*
** Match compilation utility code
*/

/*
** Handling of bif's in match guard expressions
*/

static DMCGuardBif *dmc_lookup_bif(Eterm t, int arity)
{
    /*
    ** Place for optimization, bsearch is slower than inlining it...
    */
    DMCGuardBif node = {0,NULL,0};
    node.name = t;
    node.arity = arity;
    return bsearch(&node, 
		   guard_tab, 
		   sizeof(guard_tab) / sizeof(DMCGuardBif),
		   sizeof(DMCGuardBif), 
		   (int (*)(const void *, const void *)) &cmp_guard_bif); 
}

static Eterm dmc_lookup_bif_reversed(void *f)
{
    int i;
    for (i = 0; i < (sizeof(guard_tab) / sizeof(DMCGuardBif)); ++i)
	if (f == guard_tab[i].biff)
	    return guard_tab[i].name;
    return am_undefined;
}

/* For sorting. */
static int cmp_uint(void *a, void *b) 
{
    if (*((unsigned *)a) <  *((unsigned *)b))
	return -1;
    else
	return (*((unsigned *)a) >  *((unsigned *)b));
}

static int cmp_guard_bif(void *a, void *b)
{
    int ret;
    if (( ret = ((int) atom_val(((DMCGuardBif *) a)->name)) -
	 ((int) atom_val(((DMCGuardBif *) b)->name)) ) == 0) {
	ret = ((DMCGuardBif *) a)->arity - ((DMCGuardBif *) b)->arity;
    }
    return ret;
}

/*
** Compact the variables in a match expression i e make {$1, $100, $1000} 
** become {$0,$1,$2}.
*/
static int match_compact(ErlHeapFragment *expr, DMCErrInfo *err_info)
{
    int i, j, a, n, x;
    DMC_STACK_TYPE(unsigned) heap;
    Eterm *p;
    char buff[25] = "$"; /* large enough for 64 bit to */
    int ret;

    DMC_INIT_STACK(heap);

    p = expr->mem;
    i = expr->size;
    while (i--) {
	if (is_thing(*p)) {
	    a = thing_arityval(*p);
	    ASSERT(a <= i);
	    i -= a;
	    p += a;
	} else if (is_atom(*p) && (n = db_is_variable(*p)) >= 0) {
	    x = DMC_STACK_NUM(heap);
	    for (j = 0; j < x && DMC_PEEK(heap,j) != n; ++j) 
		;
	    
	    if (j == x)
		DMC_PUSH(heap,n);
	}
	++p;
    }
    qsort(DMC_STACK_DATA(heap), DMC_STACK_NUM(heap), sizeof(unsigned), 
	  (int (*)(const void *, const void *)) &cmp_uint);

    if (err_info != NULL) { /* lint needs a translation table */
	err_info->var_trans = safe_alloc(sizeof(unsigned) * 
					 DMC_STACK_NUM(heap));
	sys_memcpy(err_info->var_trans, DMC_STACK_DATA(heap),
		   DMC_STACK_NUM(heap) * sizeof(unsigned));
	err_info->num_trans = DMC_STACK_NUM(heap);
    }

    p = expr->mem;
    i = expr->size;
    while (i--) {
	if (is_thing(*p)) {
	    a = thing_arityval(*p);
	    i -= a;
	    p += a;
	} else if (is_atom(*p) && (n = db_is_variable(*p)) >= 0) {
	    x = DMC_STACK_NUM(heap);
#ifdef HARDDEBUG
	    display(*p, CERR);
#endif
	    for (j = 0; j < x && DMC_PEEK(heap,j) != n; ++j) 
		;
	    ASSERT(j < x);
	    sprintf(buff+1,"%u", (unsigned) j);
	    /* Yes, writing directly into terms, they ARE off heap */
	    *p = am_atom_put(buff, strlen(buff));
	}
	++p;
    }
    ret = DMC_STACK_NUM(heap);
    DMC_FREE(heap);
    return ret;
}

/*
** Simple size object that takes care of function calls and constant tuples
*/
static Uint my_size_object(Eterm t) 
{
    Uint sum = 0;
    Eterm tmp;
    Eterm *p;
    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	sum += 2 + my_size_object(CAR(list_val(t))) + 
	    my_size_object(CDR(list_val(t)));
	break;
    case TAG_PRIMARY_BOXED:
	if ((((*boxed_val(t)) & 
	      _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) !=
	    (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE)) {
	    goto simple_term;
	}

	if (arityval(*tuple_val(t)) == 1 && is_tuple(tmp = tuple_val(t)[1])) {
	    Uint i,n;
	    p = tuple_val(tmp);
	    n = arityval(p[0]);
	    sum += 1 + n;
	    for (i = 1; i <= n; ++i)
		sum += my_size_object(p[i]);
	} else if (arityval(*tuple_val(t)) == 2 &&
		   is_atom(tmp = tuple_val(t)[1]) &&
		   tmp == am_const) {
	    sum += size_object(tuple_val(t)[2]);
	} else {
	    erl_exit(1,"Internal error, sizing unrecognized object in "
		     "(d)ets:match compilation.");
	}
	break;
    default:
    simple_term:
	sum += size_object(t);
	break;
    }
    return sum;
}

static Eterm my_copy_struct(Eterm t, Eterm **hp, ErlOffHeap* off_heap)
{
    Eterm ret = NIL, a, b;
    Eterm *p;
    Uint sz;
    switch (t & _TAG_PRIMARY_MASK) {
    case TAG_PRIMARY_LIST:
	a = my_copy_struct(CAR(list_val(t)), hp, off_heap);
	b = my_copy_struct(CDR(list_val(t)), hp, off_heap);
	ret = CONS(*hp, a, b);
	*hp += 2;
	break;
    case TAG_PRIMARY_BOXED:
	if (BOXED_IS_TUPLE(t)) {
	    if (arityval(*tuple_val(t)) == 1 && 
		is_tuple(a = tuple_val(t)[1])) {
		Uint i,n;
		Eterm *savep = *hp;
		ret = make_tuple(savep);
		p = tuple_val(a);
		n = arityval(p[0]);
		*hp += n + 1;
		*savep++ = make_arityval(n);
		for(i = 1; i <= n; ++i) 
		    *savep++ = my_copy_struct(p[i], hp, off_heap);
	    } else if (arityval(*tuple_val(t)) == 2 && 
		       is_atom(a = tuple_val(t)[1]) &&
		       a == am_const) {
		/* A {const, XXX} expression */
		b = tuple_val(t)[2];
		sz = size_object(b);
		ret = copy_struct(b,sz,hp,off_heap);
	    } else {
		erl_exit(1, "Trying to constant-copy non constant expression "
			 "0x%08x in (d)ets:match compilation.", (unsigned long) t);
	    }
	} else {
	    sz = size_object(t);
	    ret = copy_struct(t,sz,hp,off_heap);
	}
	break;
    default:
	ret = t;
    }
    return ret;
}

static Binary *allocate_magic_binary(size_t size)
{
    Binary* bptr;
    bptr = (Binary *) safe_alloc_from(61, 
				      size + sizeof(Binary) - 1);
    bptr->flags = BIN_FLAG_MATCH_PROG;
    bptr->orig_size = size;
    bptr->refc = 0;
    return bptr;
}
    


/*
** Compiled match bif interface
*/
/*
** erlang:match_spec_test(MatchAgainst, MatchSpec, Type) -> 
**   {ok, Return, Flags, Errors} | {error, Errors}
** MatchAgainst -> if Type == trace: list() else tuple()
** MatchSpec -> MatchSpec with body corresponding to Type
** Type -> trace | table (only trace implemented in R5C)
** Return -> if Type == trace TraceReturn else {BodyReturn, VariableBindings}
** TraceReturn -> {true | false | term()} 
** BodyReturn -> term()
** VariableBindings -> [term(), ...] 
** Errors -> [OneError, ...]
** OneError -> {error, string()} | {warning, string()}
** Flags -> [Flag, ...]
** Flag -> return_trace (currently only flag)
*/
BIF_RETTYPE match_spec_test_3(BIF_ALIST_3)
BIF_ADECL_3
{
    Eterm res;
#ifdef DMC_DEBUG
    if (BIF_ARG_3 == am_atom_put("dis",3)) {
	test_disassemble_next = 1;
	BIF_RET(am_true);
    } else
#endif
    if (BIF_ARG_3 == am_trace) {
	res = match_spec_test(BIF_P, BIF_ARG_1, BIF_ARG_2, 1);
	if (is_value(res)) {
	    BIF_RET(res);
	}
    } else if (BIF_ARG_3 == am_table) {
	res = match_spec_test(BIF_P, BIF_ARG_1, BIF_ARG_2, 0);
	if (is_value(res)) {
	    BIF_RET(res);
	}
    } 
    BIF_ERROR(BIF_P, BADARG);
}

static Eterm match_spec_test(Process *p, Eterm against, Eterm spec, int trace)
{
    Eterm lint_res;
    Binary *mps;
    Eterm res;
    Eterm ret;
    Eterm flg;
    Eterm *hp;
    Eterm *arr;
    int n;
    Eterm l;
    Uint32 ret_flags;
    Uint sz;
    Eterm *save_cp;

    if (trace && !(is_list(against) || against == NIL)) {
	return THE_NON_VALUE;
    }
    if (trace) {
	/* I WANT to use the erts_... routines here, to test them properly */
	lint_res = erts_match_set_lint(p, spec); 
	mps = erts_match_set_compile(p, spec);
    } else {
	lint_res = db_match_set_lint(p, spec, DCOMP_TABLE);
	mps = db_match_set_compile(p, spec, DCOMP_TABLE);
    }
    
    if (mps == NULL) {
	hp = HAlloc(p,3);
	ret = TUPLE2(hp, am_error, lint_res);
    } else {
#ifdef DMC_DEBUG
	if (test_disassemble_next) {
	    test_disassemble_next = 0;
	    db_match_dis(mps);
	}
#endif /* DMC_DEBUG */
	l = against;
	n = 0;
	while (is_list(l)) {
	    ++n;
	    l = CDR(list_val(l));
	}
	if (trace) {
	    arr = sys_alloc(sizeof(Eterm) * n);
	    l = against;
	    n = 0;
	    while (is_list(l)) {
		arr[n] = CAR(list_val(l));
		++n;
		l = CDR(list_val(l));
	    }
	} else {
	    n = 0;
	    arr = (Eterm *) against;
	}
	
	/* We are in the context of a BIF, 
	   {caller} should return 'undefined' */
	save_cp = p->cp;
	p->cp = NULL;
	res = erts_match_set_run(p, mps, arr, n, &ret_flags);
	p->cp = save_cp;
	if (is_non_value(res)) {
	    res = am_false;
	}
	sz = size_object(res);
	hp = HAlloc(p, 5 + ((ret_flags) ? 2 : 0) + sz);
	res = copy_struct(res, sz, &hp, &p->off_heap);
	if (!ret_flags) {
	    flg = NIL;
	} else {
	    flg = CONS(hp, am_return_trace, NIL);
	    hp += 2;
	}
	if (trace) {
	    sys_free(arr);
	}
	ret = TUPLE4(hp, am_atom_put("ok",2), res, flg, lint_res);
    }
    return ret;
}

#ifdef DMC_DEBUG
/*
** Disassemble match program
*/
static void db_match_dis(Binary *bp)
{
    MatchProg *prog = Binary2MatchProg(bp);
    Uint *t = prog->text;
    Uint n;
    Eterm p;
    int first;
    ErlHeapFragment *tmp;

    while (t < prog->labels) {
	switch (*t) {
	case matchTryMeElse:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"TryMeElse\t%lu\r\n", n);
	    break;
	case matchArray:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"Array\t%lu\r\n", n);
	    break;
	case matchArrayBind:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"ArrayBind\t%lu\r\n", n);
	    break;
	case matchTuple:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"Tuple\t%lu\r\n", n);
	    break;
	case matchPushT:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"PushT\t%lu\r\n", n);
	    break;
	case matchPushL:
	    ++t;
	    erl_printf(COUT,"PushL\r\n");
	    break;
	case matchPop:
	    ++t;
	    erl_printf(COUT,"Pop\r\n");
	    break;
	case matchBind:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"Bind\t%lu\r\n", n);
	    break;
	case matchCmp:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"Cmp\t%lu\r\n", n);
	    break;
	case matchEqBin:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erl_printf(COUT,"EqBin\t0x%08x (", (unsigned long) t);
	    display(p,COUT);
	    erl_printf(COUT,")\r\n");
	    break;
	case matchEqRef:
	    ++t;
	    n = thing_arityval(*t);
	    ++t;
	    erl_printf(COUT,"EqRef\t(%d) {", (int) n);
	    first = 1;
	    while (n--) {
		if (first)
		    first = 0;
		else
		    erl_printf(COUT,", ");
		erl_printf(COUT,"0x%08x", (unsigned long) *t);
		++t;
	    }
	    erl_printf(COUT,"}\r\n");
	    break;
	case matchEqBig:
	    ++t;
	    n = thing_arityval(*t);
	    ++t;
	    erl_printf(COUT,"EqBig\t(%d) {", (int) n);
	    first = 1;
	    while (n--) {
		if (first)
		    first = 0;
		else
		    erl_printf(COUT,", ");
		erl_printf(COUT,"0x%08x", (unsigned long) *t);
		++t;
	    }
	    erl_printf(COUT,"}\r\n");
	    break;
	case matchEqFloat:
	    ++t;
	    {
		double num;
		memcpy(&num,t, 2 * sizeof(*t));
		t += 2;
		erl_printf(COUT,"EqFloat\t%f\r\n", num);
	    }
	    break;
	case matchEq:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erl_printf(COUT,"Eq  \t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchList:
	    ++t;
	    erl_printf(COUT,"List\r\n");
	    break;
	case matchHalt:
	    ++t;
	    erl_printf(COUT,"Halt\r\n");
	    break;
	case matchSkip:
	    ++t;
	    erl_printf(COUT,"Skip\r\n");
	    break;
	case matchPushC:
	    ++t;
	    p = (Eterm) *t;
	    ++t;
	    erl_printf(COUT, "PushC\t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchConsA:
	    ++t;
	    erl_printf(COUT,"ConsA\r\n");
	    break;
	case matchConsB:
	    ++t;
	    erl_printf(COUT,"ConsB\r\n");
	    break;
	case matchMkTuple:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"MkTuple\t%lu\r\n", n);
	    break;
	case matchOr:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"Or\t%lu\r\n", n);
	    break;
	case matchAnd:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"And\t%lu\r\n", n);
	    break;
	case matchOrElse:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"OrElse\t%lu\r\n", n);
	    break;
	case matchAndThen:
	    ++t;
	    n = *t;
	    ++t;
	    erl_printf(COUT,"AndThen\t%lu\r\n", n);
	    break;
	case matchCall0:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erl_printf(COUT, "Call0\t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchCall1:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erl_printf(COUT, "Call1\t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchCall2:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erl_printf(COUT, "Call2\t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchCall3:
	    ++t;
	    p = dmc_lookup_bif_reversed((void *) *t);
	    ++t;
	    erl_printf(COUT, "Call3\t");
	    display(p,COUT);
	    erl_printf(COUT,"\r\n");
	    break;
	case matchPushV:
	    ++t;
	    n = (Uint) *t;
	    ++t;
	    erl_printf(COUT, "PushV\t%lu\r\n", n);
	    break;
	case matchTrue:
	    ++t;
	    erl_printf(COUT,"True\r\n");
	    break;
	case matchPushExpr:
	    ++t;
	    erl_printf(COUT,"PushExpr\r\n");
	    break;
	case matchPushArrayAsList:
	    ++t;
	    erl_printf(COUT,"PushArrayAsList\r\n");
	    break;
	case matchPushArrayAsListU:
	    ++t;
	    erl_printf(COUT,"PushArrayAsListU\r\n");
	    break;
	case matchSelf:
	    ++t;
	    erl_printf(COUT,"Self\r\n");
	    break;
	case matchWaste:
	    ++t;
	    erl_printf(COUT,"Waste\r\n");
	    break;
	case matchReturn:
	    ++t;
	    erl_printf(COUT,"Return\r\n");
	    break;
	case matchProcessDump:
	    ++t;
	    erl_printf(COUT,"ProcessDump\r\n");
	    break;
	case matchDisplay:
	    ++t;
	    erl_printf(COUT,"Display\r\n");
	    break;
	case matchIsSeqTrace:
	    ++t;
	    erl_printf(COUT,"IsSeqTrace\r\n");
	    break;
	case matchSetSeqToken:
	    ++t;
	    erl_printf(COUT,"SetSeqToken\r\n");
	    break;
	case matchGetSeqToken:
	    ++t;
	    erl_printf(COUT,"GetSeqToken\r\n");
	    break;
	case matchSetReturnTrace:
	    ++t;
	    erl_printf(COUT,"SetReturnTrace\r\n");
	    break;
	case matchCatch:
	    ++t;
	    erl_printf(COUT,"Catch\r\n");
	    break;
	case matchEnableTrace:
	    ++t;
	    erl_printf(COUT,"EnableTrace\r\n");
	    break;
	case matchDisableTrace:
	    ++t;
	    erl_printf(COUT,"DisableTrace\r\n");
	    break;
	case matchEnableTrace2:
	    ++t;
	    erl_printf(COUT,"EnableTrace2\r\n");
	    break;
	case matchDisableTrace2:
	    ++t;
	    erl_printf(COUT,"DisableTrace2\r\n");
	    break;
 	case matchCaller:
 	    ++t;
 	    erl_printf(COUT,"Caller\r\n");
 	    break;
 	case matchGetTraceControlWord:
 	    ++t;
 	    erl_printf(COUT,"GetTraceControlWord\r\n");
 	    break;
 	case matchSetTraceControlWord:
 	    ++t;
 	    erl_printf(COUT,"SetTraceControlWord\r\n");
 	    break;
	default:
	    erl_printf(COUT,"??? (0x%08x)\r\n", *t);
	    ++t;
	    break;
	}
    }
    erl_printf(COUT,"\r\n\r\nterm_save: {");
    first = 1;
    for (tmp = prog->term_save; tmp; tmp = tmp->next) {
	if (first)
	    first = 0;
	else
	    erl_printf(COUT,", ");
	erl_printf(COUT,"0x%08x", (unsigned long) tmp);
    }
    erl_printf(COUT,"}\r\n");
    erl_printf(COUT,"num_bindings: %d\r\n", prog->num_bindings);
    erl_printf(COUT,"stack: 0x%08x\r\n", (unsigned long) prog->stack);
    erl_printf(COUT,"heap: 0x%08x\r\n", (unsigned long) prog->heap);
    erl_printf(COUT,"eheap: 0x%08x\r\n", (unsigned long) prog->eheap);
    erl_printf(COUT,"labels: 0x%08x\r\n", (unsigned long) prog->labels);
    t = prog->labels;
    for (n = 0; t < prog->heap; ++n) {
	erl_printf(COUT,"labels[%d] = %d\r\n", (int) n, (int) *t);
	++t;
    }
    erl_printf(COUT,"text: 0x%08x\r\n", (unsigned long) prog->text);
    erl_printf(COUT,"stack_size: %d (words)\r\n", prog->stack_size);
    
}

#endif /* DMC_DEBUG */


