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

#include "ose.h"
#include "dbgprintf.h"
#include "stdio.h"

static int has_name(char *name, char *test) {
  if(strcmp(name, test) == 0)
    return 1;
  else 
    return 0;
}



extern OSENTRYPOINT port_test;

int user_port_map(char *name, void **func) {
  if(has_name(name, "port_test")) {
    *func = (void*)&port_test;
    return 1;
  }
  /*
  if(has_name(name, "something_else")) {
    entrypoint = &something_else;
    return 1;
  }
  */

  return 0;
}
  
