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
 * Exit codes.  Note that all are assigned low number to allow them
 * to be used as array indexes.
 */

#define NORMAL 0		/* Normal exit */
#define BADMATCH 1		/* Bad match in function body */
#define CASE_CLAUSE 2		/* Mo matching case clasue */
#define IF_CLAUSE   3		/* No mathcing if clause */
#define UNDEF 4 		/* No farity which matches */
#define BADARG 5		/* Bad argument to a BIF */
#define BADARITH 6		/* Bad arithmetic */
#define FUNCTION_CLAUSE 7	/* No matching function head */
#define BADSIGNAL 8		/* Bad message sending to port */
#define TIMEOUT_VALUE 9		/* Bad time out value */
#define NOCATCH 10		/* No catch at a throw */
#define NOPROC  11		/* No process or port */
#define NOTALIVE 12		/* Not distributed */
#define SYSTEM_LIMIT 13         /* Run out of something */
#define BADFUN 14
#define INTERNAL_ERROR 15	/* Things that shouldn't happen */
#define THROWN 16		/* The user has called the bif throw
				 * (thrown term in p->fvalue).
				 */
#define USER_EXIT 17		/* The user has called the bif exit
				 * (actual exit code in p->fvalue). 
				 */
#define USER_ERROR 18		/* Error in BIF; use p->fvalue as
				 * exit reason.
				 */

#define USER_ERROR2 19		/* Error in BIF; use p->fvalue as
				 * exit reason.
				 */
#define BADARITY 20		/* Attempt to call fun with wrong number of arguments. */

/*
 * The number of exit codes.
 */
#define NUMBER_EXIT_CODES (BADARITY+1)

/*
 * Pseudo error codes (these are never seen by the user).
 */

#define TRAP      100		/* BIF Trap to erlang code */
#define RESCHEDULE 101		/* BIF must be rescheduled */

#define TLOAD_OK 0              /* The threaded code linking was successful */
#define TLOAD_MAGIC_NUMBER 1    /* Wrong kind of object file */
#define TLOAD_FORMAT 2          /* Format error while reading object code */
#define TLOAD_MODULE 3          /* Module name in object code does not match */
#define TLOAD_SIZE 4            /* Given size in object code differs from actual size */
