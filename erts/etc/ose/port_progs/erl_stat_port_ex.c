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
#include "stdio.h"  
#include "string.h"
#include "efs.h"

#include "ose_erl_port_prog.h"
#include "erl_port_signals.sig"

union SIGNAL {
   SIGSELECT sigNo;
   struct PortData portData;
   }; 

ERL_PORT_PROG(erl_stat_port_ex) {
  static const SIGSELECT recv[] = {2, OS_ATTACH_SIG, PORT_DATA};
  union SIGNAL *sig;
  PROCESS erts_;
  
  if(!hunt("erts", 0, &erts_, NULL)) {
    fprintf(stderr, "PORT PROCESS: Fail to read ERTS pid, closing!\n");
    kill_proc(current_process());
  }
  attach(NULL, erts_);

  while(1) {
    sig = receive((SIGSELECT*)recv);
    switch(sig->sigNo) {
    
    case OS_ATTACH_SIG:
      free_buf(&sig);
      fprintf(stderr, "PORT PROCESS: ERTS process terminated, closing!\n");
      kill_proc(current_process());
      break;

    case PORT_DATA:
      if(strcmp(sig->portData.buf, "stop") == 0) {
	printf("PORT PROCESS: stopped by user!\n");
	kill_proc(current_process());
      } else {
      /* echo the message */
      send(&sig, erts_);
      }
    }
  }
}

