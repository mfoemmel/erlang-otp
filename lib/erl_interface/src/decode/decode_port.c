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
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_decode_port(const char *buf, int *index, erlang_port *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  int len;
  
  if (get8(s) != ERL_PORT_EXT) return -1;

  /* first the nodename */
  if (get8(s) != ERL_ATOM_EXT) return -1;

  len = get16be(s);

  if (p) {
    memmove(p->node, s, len);
    p->node[len] = (char)0;
  }
  s += len;
  
  /* now the numbers: num (4), creation (1) */
  if (p) {
    p->id = get32be(s);
    p->creation = get8(s) & 0x03;
  }
  else s += 5;
  
  *index += s-s0;
  
  return 0;
}
