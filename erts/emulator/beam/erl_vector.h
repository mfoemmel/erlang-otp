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

#define VECTOR_SIZE(Vec) (signed_val(((vector_val(Vec))[1])))
#define VECTOR_ARRAY_SIZE(Vec) \
 ((signed_val(((vector_val(Vec))[1])) < 5) ? 5 : (signed_val(((vector_val(Vec))[1]))))

Eterm erts_unchecked_vector_get(int index, Eterm Vec);
Eterm* erts_copy_vector(Eterm vec, Eterm* hp, Eterm* resp);
void erts_flatten_vector(Eterm* array, Eterm vec);
