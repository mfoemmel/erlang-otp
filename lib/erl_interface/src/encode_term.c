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
#include "erl_interface.h"
#include "ei.h"
#include "putget.h"

/* the actual encoder */
extern int erl_encode_it(ETERM *ep, unsigned char **ext);
     
extern int ei_encode_term(char *buf, int *index, void *t)
{
  char *s = buf + *index;
  char *s0 = s;

  if (!buf) s += erl_term_len(t) -1; /* -1 for version */
  else {
    /* this encodes all but the version at the start */
    /* and it will move s forward the right number of bytes */
    if (erl_encode_it(t,(unsigned char **)&s)) return -1;
  }
  
  *index += s - s0;
  
  return 0;
}

