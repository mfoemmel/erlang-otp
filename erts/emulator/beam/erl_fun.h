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

#ifndef __ERLFUNTABLE_H__
#define __ERLFUNTABLE_H__

/*
 * Fun entry.
 */

typedef struct erl_fun_entry {
    HashBucket bucket;		/* MUST BE LOCATED AT TOP OF STRUCT!!! */

    byte uniq[16];		/* MD5 for module. */
    int index;			/* New style index. */
    int old_uniq;		/* Unique number (old_style) */
    int old_index;		/* Old style index */
    Eterm* address;		/* Pointer to code for fun */

#ifdef HIPE
    Eterm* native_address;	/* Native entry code for fun. */
#endif

    Uint arity;			/* The arity of the fun. */
    Eterm module;		/* Tagged atom for module. */
    int refc;			/* Reference count: One for code + one for each
				   fun object in each process. */
} ErlFunEntry;

/*
 * This structure represents a 'fun' (lambda). It is stored on
 * process heaps. It has variable size depending on the size
 * of the environment.
 */

typedef struct erl_fun_thing {
    Eterm thing_word;		/* Subtag FUN_SUBTAG. */
    struct erl_fun_thing* next;	/* Next fun in mso list. */
    Eterm creator;		/* Pid of creator process (contains node). */
    ErlFunEntry* fe;		/* Pointer to fun entry. */
#ifdef HIPE
    Eterm* native_address;	/* Native code for the fun. */
#endif
    Uint arity;			/* The arity of the fun. */
    Uint num_free;		/* Number of free variables (in env). */
    Eterm env[1];		/* Environment (free variables). */
} ErlFunThing;

/* ERL_FUN_SIZE does _not_ include space for the environment */
#define ERL_FUN_SIZE ((sizeof(ErlFunThing)/sizeof(Eterm))-1)

void erts_init_fun_table(void);
void erts_fun_info(CIO to);

ErlFunEntry* erts_put_fun_entry(Eterm mod, int uniq, int index);
ErlFunEntry* erts_get_fun_entry(Eterm mod, int uniq, int index);

ErlFunEntry* erts_put_fun_entry2(Eterm mod, int old_uniq, int old_index,
				byte* uniq, int index, int arity);
ErlFunEntry* erts_get_fun_entry2(Eterm mod, int old_uniq, int old_index,
				byte* uniq, int index, int arity);

void erts_erase_fun_entry(ErlFunEntry* fe);
void erts_cleanup_funs(ErlFunThing* funp);
void erts_cleanup_funs_on_purge(Eterm* start, Eterm* end);
void erts_dump_fun_entries(CIO fd);

#endif
