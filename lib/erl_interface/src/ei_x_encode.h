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
* ei_x_encode to encode in a self-expanding buffer
*/

#ifndef EI_X_ENCODE_H
#define EI_X_ENCODE_H

#include "eicode.h"

/* a dynamic version of ei */
typedef struct ei_x_buff_TAG {
    char* buff;
    int buffsz;
    int index;
} ei_x_buff;

int ei_x_new(ei_x_buff* x);
int ei_x_new_with_version(ei_x_buff* x);
int ei_x_free(ei_x_buff* x);
int ei_x_append(ei_x_buff* x, const ei_x_buff* x2);
int ei_x_append_buf(ei_x_buff* x, const char* buf, int len);
int ei_x_encode_string(ei_x_buff* x, const char* s);
int ei_x_encode_string_len(ei_x_buff* x, const char* s, int len);
int ei_x_encode_binary(ei_x_buff* x, const void* s, int len);
int ei_x_encode_long(ei_x_buff* x, long n);
int ei_x_encode_ulong(ei_x_buff* x, unsigned long n);
int ei_x_encode_double(ei_x_buff* x, double dbl);
int ei_x_encode_list_header(ei_x_buff* x, long n);
int ei_x_encode_empty_list(ei_x_buff* x);
int ei_x_encode_version(ei_x_buff* x);
int ei_x_encode_tuple_header(ei_x_buff* x, long n);
int ei_x_encode_atom(ei_x_buff* x, const char* s);
int ei_x_encode_atom_len(ei_x_buff* x, const char* s, int len);
int ei_x_encode_pid(ei_x_buff* x, const erlang_pid* pid);

int ei_x_encode_term(ei_x_buff* x, void* t);
int ei_x_encode_fun(ei_x_buff* x, const erlang_fun* fun);

int ei_x_format(ei_x_buff* x, const char* fmt, ...);
int x_fix_buff(ei_x_buff* x, int szneeded);

#endif

