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

#include "dbgprintf.h"
#include "erl_driver.h"
#include "sys.h"

#ifdef _OSE_SFK_
#include <strings.h>
#endif

/*
extern void erl_port_init(char *name, PROCESS erts_);

OS_PROCESS(erl_port_init0)
{
  struct OS_pcb *pcb;
  char *name;
  PROCESS erts_pid_;

#ifdef DEBUG
  dbgprintf("\nPort process started!\n");
#endif
  pcb = get_pcb(current_process());
  name = (char*)malloc(strlen(&(pcb->strings[pcb->name]))+1);
  strcpy(name, &(pcb->strings[pcb->name]));
  free_buf((union SIGNAL **)&pcb);
  if (hunt(ERTS_OSE_PROC_NAME, 0, &erts_pid_, NULL))
    if(sys_port_map(name)
    erl_port_init(name, erts_pid_);
  else {
    fprintf(stderr, "ERTS process not found. Can't start port program \"%s\"!\n", name);
    error(0);
  }   
}
*/

