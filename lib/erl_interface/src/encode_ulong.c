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

int ei_encode_ulong(char *buf, int *index, unsigned long p)
{
  char *s = buf + *index;
  char *s0 = s;

  if (p > ERL_MAX) {
    if (!buf) s += 7;
    else {
      put8(s,ERL_SMALL_BIG_EXT);
      put8(s,4);	             /* len = four bytes */
      put8(s, 0);                 /* save sign separately */
      put32le(s,p);               /* OBS: Little Endian, and p now positive */
    }
  }
  else if ((p < 256) && (p >= 0)) {
    if (!buf) s += 2;
    else {
      put8(s,ERL_SMALL_INTEGER_EXT);
      put8(s,(p & 0xff));
    }
  }
  else {
    if (!buf) s += 5;
    else {
      put8(s,ERL_INTEGER_EXT);
      put32be(s,p);
    }
  }

  *index += s-s0; 

  return 0; 
}

