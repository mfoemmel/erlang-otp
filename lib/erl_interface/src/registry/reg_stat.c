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
#include "reg.h"

/* Get object attributes.
 * This function returns a COPY of the ei_reg_obj 
 * struct for an object. 
 */
int ei_reg_stat(ei_reg *reg, const char *key, struct ei_reg_stat *obuf)
{
  ei_hash *tab;
  ei_reg_obj *obj;

  if (!key || !obuf || !reg) return -1; /* return EI_BADARG; */
  tab = reg->tab;

  if (!(obj=ei_hash_lookup(tab,key))) return -1; /* return EI_NOTFOUND; */

  obuf->attr = obj->attr;
  obuf->size = obj->size;
  
  return 0;
}
