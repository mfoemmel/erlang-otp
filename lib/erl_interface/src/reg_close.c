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

/* really remove an object (help function to hash_freetab) */
static void obj_free(void *p)
{
  ei_reg_obj *obj = p;
  
  if (obj) {
    switch (ei_reg_typeof(obj)) {
    case EI_STR:
      free(obj->val.s);
      break;

    case EI_BIN:
      free(obj->val.p);
      break;
    }

    /* really remove the inode (don't use freelist here) */
    free(obj);
  }
  return;
}

/* close an open registry */
extern int ei_reg_close(ei_reg *reg)
{
  ei_reg_obj *obj, *next;
  
  if (!reg) return -1; /* return EI_BADARG; */
  
  /* remove hash_table */
  ei_hash_freetab(reg->tab,obj_free);

  /* remove freelist */
  obj = reg->freelist;
  while (obj) {
    next = obj->next;
    free(obj);
    obj = next;
  }

  /* remove registry */
  free(reg);

  return 0;
}
