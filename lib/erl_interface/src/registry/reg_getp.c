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
#include "reg.h"

const void *ei_reg_getpval(ei_reg *reg, const char *key, int *size)
{
  ei_hash *tab;
  ei_reg_obj *obj;

  if (!key || !reg) return NULL;
  tab = reg->tab;
  
  if ((!(obj=ei_hash_lookup(tab,key))) || /* return (const void *)EI_NOTFOUND; */
      (obj->attr & EI_DELET)  || /* return (const void *)EI_NOTFOUND; */
      (ei_reg_typeof(obj) != EI_STR)) /* return (const void *)EI_TYPE; */
    return NULL;

  if (size) *size=obj->size;
  return obj->val.p;
}
