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
#include "ei.h"
#include "putget.h"

/* report type identifier from the start of the buffer */
/* for types with meaningful length attributes, return the length too.
   In other cases, return length 0 */
int ei_get_type(const char *buf, const int *index, int *type, int *len)
{
  const char *s = buf + *index;

  *type = get8(s);
  
  switch (*type) {
  case ERL_SMALL_TUPLE_EXT:
    *len = get8(s);
    break;
    
  case ERL_ATOM_EXT:
  case ERL_STRING_EXT:
    *len = get16be(s);
    break;
    
  case ERL_LARGE_TUPLE_EXT:
  case ERL_LIST_EXT:
  case ERL_BINARY_EXT:
    *len = get32be(s);
    break;
  
  default:
    *len = 0;
    break;
  }

  /* leave index unchanged */
  return 0;
}
