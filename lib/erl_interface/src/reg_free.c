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
#include <stdlib.h>
#include "eihash.h"
#include "eireg.h"

/* free a registry object (inode) on the freelist. The "value"
 * contained by the object is removed.
 */
extern void ei_reg_free(ei_reg *reg, ei_reg_obj *obj)
{
  /* free the value part */
  switch (ei_reg_typeof(obj)) {
    case EI_STR:
      free(obj->val.s);
      break;

    case EI_BIN:
      free(obj->val.p);
      break;
  }

  /* fprintf(stderr,"%s:%d: saving %p on freelist\n",__FILE__,__LINE__,obj);*/
  
  /* save the rest on the freelist */
  obj->next = reg->freelist;
  reg->freelist = obj;

  return;
}
