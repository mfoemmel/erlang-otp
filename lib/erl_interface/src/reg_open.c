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

/* open a registry */
extern ei_reg *ei_reg_open(int size)
{
  ei_reg *new;
  
  if (size <= 0) return NULL;

  if (!(new = malloc(sizeof(*new)))) return NULL;

  new->freelist = NULL;
  
  if (!(new->tab = ei_hash_newtab(size))) {
    free(new);
    return NULL;
  }

  return new;
}
