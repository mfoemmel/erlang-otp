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
#ifndef EIEXT_H
#define EIEXT_H

#ifdef __cplusplus
extern "C" {
#endif

/* these are the term type indicators used in
 * the external (distribution) format
 */

#define ERL_SMALL_INTEGER_EXT 'a'
#define ERL_INTEGER_EXT       'b'
#define ERL_FLOAT_EXT         'c'
#define ERL_ATOM_EXT          'd'
#define ERL_REFERENCE_EXT     'e'
#define ERL_NEW_REFERENCE_EXT 'r'
#define ERL_PORT_EXT          'f'
#define ERL_PID_EXT           'g'
#define ERL_SMALL_TUPLE_EXT   'h'
#define ERL_LARGE_TUPLE_EXT   'i'
#define ERL_NIL_EXT           'j'
#define ERL_STRING_EXT        'k'
#define ERL_LIST_EXT          'l'
#define ERL_BINARY_EXT        'm'
#define ERL_SMALL_BIG_EXT     'n'
#define ERL_LARGE_BIG_EXT     'o'
#define ERL_NEW_FUN_EXT	      'p'
#define ERL_FUN_EXT	      'u'
 
#define ERL_NEW_CACHE         'N' /* c nodes don't know these two */
#define ERL_CACHED_ATOM       'C'

#define ERL_VERSION_MAGIC 131   /* 130 in erlang 4.2 */

/* from erl_eterm.h */
#define ERL_MAX ((1 << 27)-1)
#define ERL_MIN -(1 << 27)

/* these are the control message types */
#define ERL_LINK          1
#define ERL_SEND          2
#define ERL_EXIT          3
#define ERL_UNLINK        4
#define ERL_NODE_LINK     5
#define ERL_REG_SEND      6
#define ERL_GROUP_LEADER  7
#define ERL_EXIT2         8
#define ERL_PASS_THROUGH      'p'

/* new ones for tracing, from Kenneth */
#define ERL_SEND_TT      12
#define ERL_EXIT_TT      13
#define ERL_REG_SEND_TT  16
#define ERL_EXIT2_TT     18

#ifdef __cplusplus
}
#endif

#endif
