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
#include "eihash.h"
#include "eireg.h"

/* get table information */
extern int ei_reg_tabstat(ei_reg *reg, struct ei_reg_tabstat *obuf)
{
  ei_hash *tab;
  
  if (!reg || !obuf) return -1; /*  return EI_BADARG; */
  tab = reg->tab;
  
  obuf->npos = tab-> npos;
  obuf->size = tab->size;
  obuf->nelem = tab->nelem;
  obuf->collisions = tab->nelem - tab->npos;

  return 0;
}
