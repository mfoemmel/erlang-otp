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
/* Same order as the ordering of terms in erlang */

/* Since there are 255 different External tag values to choose from
   There is no reason to not be extravagant.
   Hence, the different tags for large/small tuple e.t.c
*/


#define SMALL_INTEGER_EXT 'a'
#define INTEGER_EXT       'b'
#define FLOAT_EXT         'c'
#define ATOM_EXT          'd'
#define REFERENCE_EXT     'e'
#define NEW_REFERENCE_EXT 'r'
#define PORT_EXT          'f'
#define PID_EXT           'g'
#define SMALL_TUPLE_EXT   'h'
#define LARGE_TUPLE_EXT   'i'
#define NIL_EXT           'j'
#define STRING_EXT        'k'
#define LIST_EXT          'l'
#define BINARY_EXT        'm'
#define SMALL_BIG_EXT     'n'
#define LARGE_BIG_EXT     'o'
#define FUN_EXT           'u'

#define NEW_CACHE         'N'
#define CACHED_ATOM       'C'

#define VERSION_MAGIC 131   /* 130 in erlang 4.2 */
                /* Increment this when changing the external format. */
                /* ON the other hand, don't change the external format */
                /* since that breaks other peoples code! */
