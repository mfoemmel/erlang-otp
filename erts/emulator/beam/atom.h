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
#ifndef __ATOM_H__
#define __ATOM_H__

#ifndef __INDEX_H__
#include "index.h"
#endif

#include "erl_atom_table.h"

#define MAX_ATOM_LENGTH 255

/*
 * Atom entry.
 */
typedef struct atom {
    IndexSlot slot;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    int len;         /* length of atom name */
    int ord0;        /* ordinal value of first 3 bytes + 7 bits */
    byte* name;      /* name of atom */
} Atom;

Atom *atom_tab(int i);
int atom_table_size(void);	/* number of elements */
int atom_table_sz(void);	/* table size in bytes, excluding stored objects */

Eterm am_atom_put(const char*, int); /* most callers pass plain char*'s */
int atom_erase(byte*, int);
int atom_static_put(byte*, int);
void init_atom_table(void);
void atom_info(int, void *);
void dump_atoms(int, void *);
int erts_atom_get(byte* name, int len, Eterm* ap);
void erts_atom_get_text_space_sizes(Uint *reserved, Uint *used);
#endif

