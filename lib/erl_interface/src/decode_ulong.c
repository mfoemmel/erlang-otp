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

int ei_decode_ulong(const char *buf, int *index, unsigned long *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  long n;
  int arity;
  int sign;

  switch (get8(s)) {
  case ERL_SMALL_INTEGER_EXT:
    n = get8(s);
    break;
    
  case ERL_INTEGER_EXT:
    n = get32be(s);
    break;
    
  case ERL_SMALL_BIG_EXT:
    if ((arity = get8(s)) != 4) return -1;
    if ((sign = get8(s))) return -1;
    n = get32le(s);     /* OBS! Little Endian! */
    break;
    
  default:
    return -1;
  }

  if (p) *p = n;
  *index += s-s0;
  
  return 0; 
}
