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
 * There are three primary exception classes:
 *
 *	- exit			Process termination - not an error.
 *	- error			Error (will be logged).
 *	- thrown		Nonlocal return (turns into a 'nocatch'
 *				error if not caught by the process).
 *
 * On top of these, we define a couple of often used special cases.
 *
 *	- fault			Error; stack trace will be added.
 *	- normal		Exit with reason 'normal'.
 *
 * In addition, we define a number of exit codes as a convenient
 * short-hand: instead of building the error descriptor term at the time
 * the exception is raised, it is built as necessary when the exception
 * is handled. Examples are BADARG (EXC_BADARG), BADFUN, etc.
 */

/*
 * Bits 0-1 index the 'exception class tag' table.
 */
#define EXC_CLASSBITS 0x0003
#define GET_EXC_CLASS(x) ((x) & EXC_CLASSBITS)

/*
 * Exit code flags
 */
#define EXF_PANIC	(1<<2)	/* ignore catches */
#define EXF_THROWN	(1<<3)	/* nonlocal return */
#define EXF_LOG		(1<<4)	/* write to logger on termination */
#define EXF_TRACE	(1<<5)	/* build backtrace */
#define EXF_ARGLIST	(1<<6)	/* has arglist for top of trace */

#define EXC_FLAGBITS 0x007c

/*
 * The primary fields of an exception code
 */
#define EXF_PRIMARY	(EXF_PANIC | EXF_THROWN | EXF_LOG)
#define PRIMARY_EXCEPTION(x) ((x) & (EXF_PRIMARY | EXC_CLASSBITS))

/*
 * Exception class tags (indices into the 'exception_tag' array)
 */
#define EXTAG_EXIT	0
#define EXTAG_ERROR	1
#define EXTAG_THROWN	2

#define NUMBER_EXC_TAGS 3	/* The number of exception class tags */

/*
 * Bits 7-11 of the error code are used for indexing into
 * the short-hand error descriptor table.
 */
#define EXC_INDEXBITS 0x0f80
#define GET_EXC_INDEX(x) (((x) & EXC_INDEXBITS) >> 7)

/*
 * Exit codes. Note that indices are assigned low numbers starting at 0
 * to allow them to be used as array indices. The primary exceptions
 * share index 0.
 */
#define EXC_PRIMARY 0
#define EXC_EXIT   (EXTAG_EXIT)
					/* Generic exit (final exit
					 * term in p->fvalue) */
#define EXC_ERROR  (EXTAG_ERROR | EXF_LOG)
					/* Generic error (final exit 
					 * term in p->fvalue) */
#define EXC_THROWN (EXTAG_THROWN | EXF_THROWN)
					/* Generic nonlocal return
					 * (thrown term in p->fvalue) */

#define EXC_FAULT  (EXC_ERROR | EXF_TRACE)
					/* Fault = error + trace */
#define EXC_USER_ERROR EXC_FAULT	/* Alias for fault */
#define EXC_USER_ERROR2 (EXC_FAULT | EXF_ARGLIST)
					/* Fault with given arglist term
					 * (exit reason in p->fvalue) */

#define EXC_NORMAL		((1 << 7) | EXC_EXIT)
					/* Normal exit (reason 'normal') */
#define EXC_INTERNAL_ERROR	((2 << 7) | EXC_FAULT | EXF_PANIC)
					/* Things that shouldn't happen */
#define EXC_BADARG		((3 << 7) | EXC_FAULT)
					/* Bad argument to a BIF */
#define EXC_BADARITH		((4 << 7) | EXC_FAULT)
					/* Bad arithmetic */
#define EXC_BADMATCH		((5 << 7) | EXC_FAULT)
					/* Bad match in function body */
#define EXC_FUNCTION_CLAUSE	((6 << 7) | EXC_FAULT)
					 /* No matching function head */
#define EXC_CASE_CLAUSE		((7 << 7) | EXC_FAULT)
					/* No matching case clause */
#define EXC_IF_CLAUSE		((8 << 7) | EXC_FAULT)
					/* No matching if clause */
#define EXC_UNDEF		((9 << 7) | EXC_FAULT)
				 	/* No farity that matches */
#define EXC_BADFUN		((10 << 7) | EXC_FAULT)
					/* Not an existing fun */
#define EXC_BADARITY		((11 << 7) | EXC_FAULT)
					/* Attempt to call fun with
					 * wrong number of arguments. */
#define EXC_TIMEOUT_VALUE	((12 << 7) | EXC_FAULT)
					/* Bad time out value */
#define EXC_NOPROC		((13 << 7) | EXC_FAULT)
					/* No process or port */
#define EXC_NOTALIVE		((14 << 7) | EXC_FAULT)
					/* Not distributed */
#define EXC_SYSTEM_LIMIT	((15 << 7) | EXC_FAULT)
					/* Ran out of something */
#define EXC_TRY_CLAUSE		((16 << 7) | EXC_FAULT)
					/* No matching try clause */

#define NUMBER_EXIT_CODES 17	/* The number of exit code indices */

/*
 * Internal pseudo-error codes.
 */
#define TRAP		31	/* BIF Trap to erlang code */
#define RESCHEDULE	30	/* BIF must be rescheduled */

/*
 * Aliases for some common exit codes.
 */

#define USER_EXIT EXC_EXIT
#define USER_ERROR EXC_USER_ERROR
#define USER_ERROR2 EXC_USER_ERROR2
#define THROWN EXC_THROWN
#define BADARG EXC_BADARG
#define BADARITH EXC_BADARITH
#define BADMATCH EXC_BADMATCH
#define SYSTEM_LIMIT EXC_SYSTEM_LIMIT


/*
 * Pseudo error codes (these are never seen by the user).
 */

#define TLOAD_OK 0              /* The threaded code linking was successful */
#define TLOAD_MAGIC_NUMBER 1    /* Wrong kind of object file */
#define TLOAD_FORMAT 2          /* Format error while reading object code */
#define TLOAD_MODULE 3          /* Module name in object code does not match */
#define TLOAD_SIZE 4            /* Given size in object code differs from actual size */

/*
 * The table translating an exception code to an atom.
 */
Eterm error_atom[NUMBER_EXIT_CODES];

/*
 * The exception tag table.
 */
Eterm exception_tag[NUMBER_EXC_TAGS];
