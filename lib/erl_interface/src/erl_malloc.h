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

#include "portability.h"
#include "erl_eterm.h"

__ERL_BEGIN_DECL

extern void erl_init_malloc __ERL_P((Erl_Heap*,long));
extern void *erl_malloc __ERL_P((long));
extern void *erl_realloc __ERL_P((void*, long));
extern void erl_free __ERL_P((void*));

extern ETERM *erl_alloc_eterm __ERL_P((unsigned char));
extern void erl_free_term __ERL_P((ETERM*));
extern void erl_free_compound __ERL_P((ETERM*));
extern void erl_free_array __ERL_P((ETERM**,int));
extern int erl_current_fix_desc __ERL_P((void));

__ERL_END_DECL

#endif

