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
#include <stddef.h>
#include <stdlib.h>
#include "erl_eterm.h"
#include "erl_error.h"
#include "erl_malloc.h"
#include "erl_fix_alloc.h"
#include "erl_internal.h"

void erl_init_malloc(Erl_Heap *hp, long heap_size) 
{
  erl_init_eterm_alloc();
} /* erl_init_malloc */

ETERM *erl_alloc_eterm(unsigned char type)
{
  ETERM *e;

  /* Use fix size allocator */
  if (!(e = (ETERM *) erl_eterm_alloc())) 
    erl_err_sys("<ERROR> erl_alloc_eterm: Failed to allocate more memory\n");

  ERL_HEADER(e)->count = 0;
  ERL_HEADER(e)->type  = type;
  return e;

} /* erl_alloc_eterm */

#define EXTERNAL 1
#define INTERNAL 0
#define COMPOUND     1
#define NOT_COMPOUND 0
static void _erl_free_term(); /* forward */

/* 
 * Free a term, but don't deallocate it until
 * the reference counter triggers.
 */
void erl_free_term(ETERM *ep)
{
  _erl_free_term(ep, EXTERNAL, NOT_COMPOUND);
} /* erl_free_term */

/* 
 * Free a term regardless of its reference 
 * counter value. Use this when you have 
 * built compound terms such as lists or tuples.
 */

/*
 * Tearing down term structures no-matter-what is a horrible idea if
 * any term happens to be shared (with some other structure or even
 * with yourself).
 */
__SYM_WARNING(erl_free_compound, 
	      "the `erl_free_compound' function is dangerous "
	      "and should not be used.");

void 
erl_free_compound (ETERM *ep)
{
  _erl_free_term(ep, EXTERNAL, COMPOUND);
} /* erl_free_compound */


/*
** The actual free'ing is done here in _erl_free_term.
** It is by nature recursive, but does not recurse 
** on the CDR of a list, which makes it usable for large lists.
*/

/*
** Convenience macro, called for variables and lists,
** avoids deep recursions.
*/
#define RESTART(Eterm, External, Compound) 		\
do { 							\
    ETERM *sep;						\
    sep = (Eterm);					\
    external = (External);				\
    compound = (Compound);				\
    /* Clear header info */				\
    ERL_TYPE(ep)  = ERL_UNDEF;				\
    erl_eterm_free((unsigned int *) ep);		\
    ep = sep;						\
    goto restart;      	       			        \
} while(0)

#define FREE_AND_CLEAR(ptr)			\
do {						\
    erl_free(ptr);				\
    (ptr) = NULL;				\
} while (0)

static void 
_erl_free_term (ETERM *ep, int external, int compound)
{
restart:
    if (ep == NULL) 
	return;
    if (compound || ERL_NO_REF(ep)) {
	/* Yes, it's time to *really* free this one ! */
	switch(ERL_TYPE(ep)) 
	    {
	    case ERL_ATOM:
		FREE_AND_CLEAR(ERL_ATOM_PTR(ep));
		break;
	    case ERL_VARIABLE:
		FREE_AND_CLEAR(ERL_VAR_NAME(ep));
		/* Note: It may be unbound ! */
		if (ERL_VAR_VALUE(ep) != NULL) {
		    ERL_COUNT(ERL_VAR_VALUE(ep))--;
		    /* Cleanup and Restart with the actual value */
		    RESTART(ERL_VAR_VALUE(ep), INTERNAL, compound);
		}
		break;
	    case ERL_LIST: 
		ERL_COUNT(HEAD(ep))--; 
		_erl_free_term(HEAD(ep), INTERNAL, compound);
		ERL_COUNT(TAIL(ep))--;
		/* Clean up and walk on to CDR in list */
		RESTART(TAIL(ep), INTERNAL, compound);
		break;
	    case ERL_TUPLE: 
		{
		    int i;
		    for (i=0; i < ERL_TUPLE_SIZE(ep); i++) 
			if (ERL_TUPLE_ELEMENT(ep, i)) {
			    ERL_COUNT(ERL_TUPLE_ELEMENT(ep, i))--;
			    _erl_free_term(ERL_TUPLE_ELEMENT(ep, i), 
					   INTERNAL, compound);
			}
		    FREE_AND_CLEAR(ERL_TUPLE_ELEMS(ep));
		}
	    break;
	    case ERL_BINARY:
		FREE_AND_CLEAR(ERL_BIN_PTR(ep));
		break;
	    case ERL_PID:
		FREE_AND_CLEAR(ERL_PID_NODE(ep));
		break;
	    case ERL_PORT:
		FREE_AND_CLEAR(ERL_PORT_NODE(ep));
		break;
	    case ERL_REF:
		FREE_AND_CLEAR(ERL_REF_NODE(ep));
		break;
	    case ERL_EMPTY_LIST:
	    case ERL_INTEGER:
	    case ERL_SMALL_BIG:
	    case ERL_U_SMALL_BIG:
	    case ERL_FLOAT:
		break;
	    case ERL_FUNCTION: 
		{
		    int i;

		    _erl_free_term(ERL_FUN_INDEX(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_UNIQ(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_CREATOR(ep), INTERNAL, compound);
		    _erl_free_term(ERL_FUN_MODULE(ep), INTERNAL, compound);
		    if (ERL_CLOSURE(ep) != NULL) {
			for (i = 0;  i < ERL_CLOSURE_SIZE(ep); i++) 
			    _erl_free_term(ERL_CLOSURE_ELEMENT(ep,i),
					   INTERNAL, compound);
		    }
		}
		break;
	    } /* switch */

	/* Clear header info for those cases where we are done */
	ERL_TYPE(ep)  = ERL_UNDEF;
	erl_eterm_free(ep);
    } else if (external) {
	ERL_COUNT(ep)--;
	external = INTERNAL;
	goto restart;
    }
} /* _erl_free_term */
#undef RESTART
#undef FREE_AND_CLEAR

void erl_free_array(ETERM **arr, int size)
{
  int i;

  for (i=0; i<size; i++)
    erl_free_term(arr[i]);

} /* erl_free_array */


/*
 * Local Variables:
 * compile-command: "cd ..; ERL_TOP=/clearcase/otp/erts make -k"
 * End:
 */
