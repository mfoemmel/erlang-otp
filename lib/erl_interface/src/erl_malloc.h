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
#ifndef _ERL_MALLOC_H
#define _ERL_MALLOC_H

#include "erl_eterm.h"

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __STDC__
extern void erl_init_malloc(Erl_Heap*,long);
extern void *erl_malloc(long);
extern void erl_free(void*);
extern ETERM *erl_alloc_eterm(unsigned char);
extern void erl_free_term(ETERM*);
extern void erl_free_compound(ETERM*);
extern void erl_free_array(ETERM**,int);
extern int erl_current_fix_desc(void);
#else
extern void erl_init_malloc();
extern void *erl_malloc();
extern void erl_free();
extern ETERM *erl_alloc_eterm();
extern void erl_free_term();
extern void erl_free_compound();
extern void erl_free_array();
extern int erl_current_fix_desc();
#endif

#ifdef __cplusplus
}
#endif

#endif

