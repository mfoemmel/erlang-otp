changecom(`/*', `*/')dnl
/*
 * $Id$
 */

/*
 * Reserved registers.
 */
`#define P	%ebp'
`#define HP	%esi'

/*
 * Context switching macros.
 */
`#define SWITCH_C_TO_ERLANG_QUICK	\
	movl	%esp, P_CSP(P);	\
	movl	P_NSP(P), %esp'

`#define SWITCH_ERLANG_TO_C_QUICK	\
	movl	%esp, P_NSP(P);	\
	movl	P_CSP(P), %esp'

`#define SWITCH_C_TO_ERLANG	\
	movl	P_HP(P), HP;	\
	SWITCH_C_TO_ERLANG_QUICK'

`#define SWITCH_ERLANG_TO_C	\
	movl	HP, P_HP(P);	\
	SWITCH_ERLANG_TO_C_QUICK'

/*
 * Argument (parameter) registers.
 */
define(NR_ARG_REGS,3)dnl admissible values are 0 to 5, inclusive
`#define X86_NR_ARG_REGS	'NR_ARG_REGS

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
 * Temporary registers in runtime glue code.
 * 1. They are C callee-save.
 * 2. They may alias some general argument registers.
 * 3. TEMP1 DOES NOT alias any native BIF argument register.
 */
`#define TEMP0	%ebx'
`#define TEMP1	%edi'

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

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_x86_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_COPY_ESP(ARITY)
dnl if ARITY > NR_ARG_REGS then TEMP1 := %esp.
dnl Allows the stacked formals to be referenced via TEMP1 after the stack switch.
dnl
define(NBIF_COPY_ESP,`ifelse(eval($1 > NR_ARG_REGS),0,,`movl	%esp, TEMP1')')dnl
`/* #define NBIF_COPY_ESP_0	'NBIF_COPY_ESP(0)` */'
`/* #define NBIF_COPY_ESP_1	'NBIF_COPY_ESP(1)` */'
`/* #define NBIF_COPY_ESP_2	'NBIF_COPY_ESP(2)` */'
`/* #define NBIF_COPY_ESP_3	'NBIF_COPY_ESP(3)` */'

dnl
dnl NBIF_ARG(ARITY,ARGNO)
dnl Generates an operand for this formal parameter.
dnl It will be a register operand when 0 <= ARGNO < NR_ARG_REGS.
dnl It will be a memory operand when ARGNO >= NR_ARG_REGS.
dnl
define(NBIF_ARG,`ifelse(eval($2 >= NR_ARG_REGS),0,`ARG'$2,eval(($1-NR_ARG_REGS)*4-($2-NR_ARG_REGS)*4)`(TEMP1)')')dnl
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
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,,`$'eval(4*($1 - NR_ARG_REGS)))')dnl
dnl NBIF_RET(ARITY)
define(NBIF_RET,`ret	RET_POP($1)')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'

dnl
dnl NBIF_SAVE_RESCHED_ARGS(ARITY)
dnl Used in the expensive_bif_interface_{1,2}() macros to copy
dnl caller-save registers to non-volatile locations.
dnl Currently, 1 <= ARITY <= 2, so this simply moves the argument
dnl registers to the callee-save temps.
dnl
define(NBIF_MIN,`ifelse(eval($1 > $2),0,$1,$2)')dnl
define(NBIF_SVA_1,`ifelse(eval($1 < NR_ARG_REGS),0,,`movl ARG$1, TEMP$1 ; ')')dnl
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
`/* #define NBIF_SAVE_CALLER_SAVE	'NBIF_SAVE_CALLER_SAVE` */'
`/* #define NBIF_RESTORE_CALLER_SAVE_1	'NBIF_RESTORE_CALLER_SAVE(1)` */'
