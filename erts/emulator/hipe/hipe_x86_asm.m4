changecom(`/*', `*/')dnl
/*
 * $Id$
 */
`#ifndef HIPE_X86_ASM_H
#define HIPE_X86_ASM_H'

/*
 * Tunables.
 */
define(LEAF_WORDS,24)dnl number of stack words for leaf functions
define(NR_ARG_REGS,3)dnl admissible values are 0 to 5, inclusive
define(HP_IN_ESI,1)dnl change to 0 to not reserve a global register for HP
define(SIMULATE_NSP,0)dnl change to 1 to simulate call/ret insns

`#define X86_LEAF_WORDS	'LEAF_WORDS
`#define LEAF_WORDS	'LEAF_WORDS

/*
 * Reserved registers.
 */
`#define P	%ebp'

`#define X86_HP_IN_ESI	'HP_IN_ESI
`#if X86_HP_IN_ESI
#define SAVE_HP		movl %esi, P_HP(P)
#define RESTORE_HP	movl P_HP(P), %esi
#else
#define SAVE_HP		/*empty*/
#define RESTORE_HP	/*empty*/
#endif'

`#define NSP		%esp
#define SAVE_CSP	movl %esp, P_CSP(P)
#define RESTORE_CSP	movl P_CSP(P), %esp'

`#define X86_SIMULATE_NSP	'SIMULATE_NSP

/*
 * Context switching macros.
 */
`#define SWITCH_C_TO_ERLANG_QUICK	\
	SAVE_CSP; \
	movl P_NSP(P), NSP'

`#define SWITCH_ERLANG_TO_C_QUICK	\
	movl NSP, P_NSP(P); \
	RESTORE_CSP'

`#define SWITCH_C_TO_ERLANG	\
	RESTORE_HP;		\
	SWITCH_C_TO_ERLANG_QUICK'

`#define SWITCH_ERLANG_TO_C	\
	SAVE_HP;		\
	SWITCH_ERLANG_TO_C_QUICK'

/*
 * Argument (parameter) registers.
 */
`#define X86_NR_ARG_REGS	'NR_ARG_REGS
`#define NR_ARG_REGS	'NR_ARG_REGS

ifelse(eval(NR_ARG_REGS >= 1),0,,
``#define ARG0	%eax
'')dnl
ifelse(eval(NR_ARG_REGS >= 2),0,,
``#define ARG1	%edx
'')dnl
ifelse(eval(NR_ARG_REGS >= 3),0,,
``#define ARG2	%ecx
'')dnl
ifelse(eval(NR_ARG_REGS >= 4),0,,
``#define ARG3	%ebx
'')dnl
ifelse(eval(NR_ARG_REGS >= 5),0,,
``#define ARG4	%edi
'')dnl

/*
 * TEMP_RV:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 */
`#define TEMP_RV	%ebx'

/*
 * TEMP_NSP:
 *	Used in BIF wrappers to permit copying stacked parameter from
 *	the native stack to the C stack.
 *	Set up by NBIF_COPY_NSP(arity) and used by NBIF_ARG(arity,argno).
 *	TEMP_NSP may alias the last BIF argument register.
 *	NBIF_COPY_NSP and NBIF_ARG currently fail if ARITY > NR_ARG_REGS!
 */
`#define TEMP_NSP	%edi'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_x86_glue.S support			X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl LOAD_ARG_REGS
dnl
define(LAR_1,`movl P_ARG$1(P), ARG$1 ; ')dnl
define(LAR_N,`ifelse(eval($1 >= 0),0,,`LAR_N(eval($1-1))LAR_1($1)')')dnl
define(LOAD_ARG_REGS,`LAR_N(eval(NR_ARG_REGS-1))')dnl
`#define LOAD_ARG_REGS	'LOAD_ARG_REGS

dnl
dnl STORE_ARG_REGS
dnl
define(SAR_1,`movl ARG$1, P_ARG$1(P) ; ')dnl
define(SAR_N,`ifelse(eval($1 >= 0),0,,`SAR_N(eval($1-1))SAR_1($1)')')dnl
define(STORE_ARG_REGS,`SAR_N(eval(NR_ARG_REGS-1))')dnl
`#define STORE_ARG_REGS	'STORE_ARG_REGS

dnl
dnl NSP_CALL(FUN)
dnl Emit a CALL FUN instruction, or simulate it.
dnl FUN must not be an NSP-based memory operand.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_CALL(FUN)	call FUN'',
``#define NSP_CALL(FUN)	subl $4,NSP; movl $1f,(NSP); jmp FUN; 1:'')dnl

dnl
dnl NSP_RETN(NPOP)
dnl Emit a RET $NPOP instruction, or simulate it.
dnl NPOP should be non-zero.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RETN(NPOP)	ret $NPOP'',
``#define NSP_RETN(NPOP)	movl (NSP),TEMP_RV; addl $4+NPOP,NSP; jmp *TEMP_RV'')dnl

dnl
dnl NSP_RET0
dnl Emit a RET instruction, or simulate it.
dnl
ifelse(eval(SIMULATE_NSP),0,
``#define NSP_RET0	ret'',
``#define NSP_RET0	movl (NSP),TEMP_RV; addl $4,NSP; jmp *TEMP_RV'')dnl

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_x86_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_COPY_NSP(ARITY)
dnl if ARITY > NR_ARG_REGS then TEMP_NSP := %esp.
dnl Allows the stacked formals to be referenced via TEMP_NSP after the stack switch.
dnl
define(NBIF_COPY_NSP,`ifelse(eval($1 > NR_ARG_REGS),0,,`movl	%esp, TEMP_NSP')')dnl
`/* #define NBIF_COPY_NSP_0	'NBIF_COPY_NSP(0)` */'
`/* #define NBIF_COPY_NSP_1	'NBIF_COPY_NSP(1)` */'
`/* #define NBIF_COPY_NSP_2	'NBIF_COPY_NSP(2)` */'
`/* #define NBIF_COPY_NSP_3	'NBIF_COPY_NSP(3)` */'
`/* #define NBIF_COPY_NSP_5	'NBIF_COPY_NSP(5)` */'

dnl
dnl NBIF_ARG(ARITY,ARGNO)
dnl Generates an operand for this formal parameter.
dnl It will be a register operand when 0 <= ARGNO < NR_ARG_REGS.
dnl It will be a memory operand via TEMP_NSP when ARGNO >= NR_ARG_REGS.
dnl
define(NBIF_ARG,`ifelse(eval($2 >= NR_ARG_REGS),0,`ARG'$2,eval(($1-NR_ARG_REGS)*4-($2-NR_ARG_REGS)*4)`(TEMP_NSP)')')dnl
`/* #define NBIF_ARG_1_0	'NBIF_ARG(1,0)` */'
`/* #define NBIF_ARG_2_0	'NBIF_ARG(2,0)` */'
`/* #define NBIF_ARG_2_1	'NBIF_ARG(2,1)` */'
`/* #define NBIF_ARG_3_0	'NBIF_ARG(3,0)` */'
`/* #define NBIF_ARG_3_1	'NBIF_ARG(3,1)` */'
`/* #define NBIF_ARG_3_2	'NBIF_ARG(3,2)` */'

dnl
dnl NBIF_RET(ARITY)
dnl Generates a return from a native BIF, taking care to pop
dnl any stacked formal parameters.
dnl
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,0,eval(4*($1 - NR_ARG_REGS)))')dnl
define(NBIF_RET_N,`ifelse(eval($1),0,`NSP_RET0',`NSP_RETN($1)')')dnl
define(NBIF_RET,`NBIF_RET_N(eval(RET_POP($1)))')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'
`/* #define NBIF_RET_5	'NBIF_RET(5)` */'

dnl
dnl NBIF_SAVE_RESCHED_ARGS(ARITY)
dnl Used in the expensive_bif_interface_{1,2}() macros to copy
dnl caller-save registers to non-volatile locations.
dnl Currently, 1 <= ARITY <= 2, so this simply moves the argument
dnl registers to the argument slots in the PCB.
dnl
define(NBIF_MIN,`ifelse(eval($1 > $2),0,$1,$2)')dnl
define(NBIF_SVA_1,`ifelse(eval($1 < NR_ARG_REGS),0,,`movl ARG$1, P_ARG$1(P); ')')dnl
define(NBIF_SVA_N,`ifelse(eval($1 >= 0),0,,`NBIF_SVA_N(eval($1-1))NBIF_SVA_1($1)')')dnl
define(NBIF_SAVE_RESCHED_ARGS,`NBIF_SVA_N(eval(NBIF_MIN($1,NR_ARG_REGS)-1))')dnl
`/* #define NBIF_SAVE_RESCHED_ARGS_1 'NBIF_SAVE_RESCHED_ARGS(1)` */'
`/* #define NBIF_SAVE_RESCHED_ARGS_2 'NBIF_SAVE_RESCHED_ARGS(2)` */'

dnl
dnl NBIF_SAVE_CALLER_SAVE
dnl NBIF_RESTORE_CALLER_SAVE(N)
dnl Used in callee_save_primop_interface_0() macro to save and restore
dnl C caller-save argument registers around calls to inc_stack_0.
dnl The first 3 arguments registers are C caller-save, remaining ones
dnl are C callee-save. (Yes: calleE_save_primop_interface_0
dnl preserves the calleR-save registers.)
dnl
define(NR_CALLER_SAVE,NBIF_MIN(NR_ARG_REGS,3))dnl
define(NBIF_SCS_1,`pushl ARG$1 ; ')dnl
define(NBIF_SCS_N,`ifelse(eval($1 >= 0),0,,`NBIF_SCS_1($1)NBIF_SCS_N(eval($1-1))')')dnl
define(NBIF_SAVE_CALLER_SAVE,`NBIF_SCS_N(eval(NR_CALLER_SAVE-1))')dnl
define(NBIF_RCS_1,`movl eval(4*($1+$2))(%esp), ARG$1 ; ')dnl
define(NBIF_RCS_N,`ifelse(eval($1 >= 0),0,,
`NBIF_RCS_N(eval($1-1),$2)NBIF_RCS_1($1,$2)')')dnl
define(NBIF_RESTORE_CALLER_SAVE,
`NBIF_RCS_N(eval(NR_CALLER_SAVE-1),$1)addl `$'eval(4*($1+NR_CALLER_SAVE))`, %esp'')dnl
`#define NBIF_SAVE_CALLER_SAVE	'NBIF_SAVE_CALLER_SAVE
`#define NBIF_RESTORE_CALLER_SAVE_1	'NBIF_RESTORE_CALLER_SAVE(1)

`#endif /* HIPE_X86_ASM_H */'
