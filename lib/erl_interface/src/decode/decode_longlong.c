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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

#ifdef EI_64BIT
int ei_decode_long(const char *buf, int *index, long *p)
{
    return ei_decode_longlong(buf, index, p);
}
#endif

int ei_decode_longlong(const char *buf, int *index, EI_LONGLONG *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  EI_LONGLONG n;
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
    if ((arity = get8(s)) > 8) return -1;
    sign = get8(s);
    /* Little Endian, and n always positive, except for LONG_MIN */
    {
	int pos, shift = 0;
	n = 0;
	for (pos = 0; pos < arity; pos++) {
	    n |= get8(s) << shift;
	    shift += 8;
	}
    }

    if (sign) {
      /* check for overflow */
      if ((n - 1) < 0) return -1;
      n = -n;
    } else {
      /* check for overflow */
      if (n < 0) return -1;
    }
    
    break;
    
  default:
    return -1;
  }

  if (p) *p = n;
  *index += s-s0;
  
  return 0; 
}
