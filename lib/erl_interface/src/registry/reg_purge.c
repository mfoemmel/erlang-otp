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

static ei_bucket *do_purge(ei_reg *reg, int i)
{
  ei_hash *tab = reg->tab;
  ei_bucket *head = tab->tab[i];
  ei_bucket *this, *next;
  ei_reg_obj *obj;

  /* first position special case */
  while ((this=head)) {
    obj = (ei_reg_obj*)(this->value); /* cast to eliminate 'const' warning */
    if (obj->attr & EI_DELET) {
      head = this->next;
      ei_reg_free(reg,obj); /* free obj to freelist */
      ei_hash_bfree(tab,this); /* free bucket to freelist */
      tab->nelem--;
    }
    else break;
  }

  /* check remaining positions */
  this = head;
  while (this && this->next) {
    next = this->next;
    obj = (ei_reg_obj*)(next->value); /* cast to eliminate 'const' warning */
    if (obj->attr & EI_DELET) {
      this->next = next->next;
      ei_reg_free(reg,obj); /* free object to freelist */
      ei_hash_bfree(tab,next); /* free bucket to freelist */
      tab->nelem--;
    }
    else this = this->next; 
  }
  
  return head;
}

int ei_reg_purge(ei_reg *reg)
{
  ei_hash *tab;
  int i;

  if (!reg) return -1;
  tab = reg->tab;

  for (i=0;i<tab->size;i++) {
    if ((tab->tab[i])) {
      tab->tab[i] = do_purge(reg,i);
      if (!tab->tab[i]) tab->npos--;
    }
  }

  return 0;
}
