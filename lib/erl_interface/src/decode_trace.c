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

int ei_decode_trace(const char *buf, int *index, erlang_trace *p)
{
  int arity = 0;
  
  /* { Flags, Label, Serial, FromPid, Prev } */
  if (ei_decode_tuple_header(buf,index,&arity)
      || (arity != 5)
      || ei_decode_long(buf,index,&p->flags)
      || ei_decode_long(buf,index,&p->label)
      || ei_decode_long(buf,index,&p->serial)
      || ei_decode_pid(buf,index,&p->from)
      || ei_decode_long(buf,index,&p->prev)) return -1;

  /* index is updated by the functions we called */
  
  return 0;
}
