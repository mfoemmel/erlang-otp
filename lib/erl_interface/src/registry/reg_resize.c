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

/* resize a registry - return the new size or -1 on error */
int ei_reg_resize(ei_reg *reg, int newsize)
{
  ei_hash *newtab=NULL;

  if (!reg) return -1;
  if (newsize <= 0) return -1;
  
  if ((newtab=ei_hash_resize(reg->tab,newsize))) {
    reg->tab = newtab;
  }

  return reg->tab->size;
}
