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
 * Function:
 * ei_print_term to print out a binary coded term
 */

#ifndef EI_PRINTTERM_H
#define EI_PRINTTERM_H

#ifndef EI_H
#include "ei.h"
#endif

int ei_print_term(FILE *fp, const char* buf, int* index);
int ei_s_print_term(char** s, const char* buf, int* index);
int ei_skip_term(const char* buf, int* index);

#endif

