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

#ifndef __EI_ENCODE_TERM_H__
#define __EI_ENCODE_TERM_H__

#include <string.h>

#include "ei.h"

typedef struct {
    char ei_type;
    int arity;
    int size;
    union {
	long i_val;
	double d_val;
	char atom_name[MAXATOMLEN];
	erlang_pid pid;
	erlang_port port;
	erlang_ref ref;
    } value;
} ei_term;

/* Returns 1 if term is decoded, 0 if term is OK, but not decoded here
   and -1 if something is wrong.
   ONLY changes index if term is decoded (return value 1)! */

int ei_decode_ei_term(const char* buf, int* index, ei_term* term);

#endif

