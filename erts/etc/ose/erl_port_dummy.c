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
#include "malloc.h"

#include "port_signals.sig"

union ErlSig {
  SIGSELECT sigNo;
  struct ErlPid erlPid;
  struct PortData portData;
};

OS_PROCESS(port_test) {
  static const SIGSELECT recv_pid_sig[2] = {1, ERL_PID};
  static const SIGSELECT recv_any[] = {0};
  union ErlSig *sig, *echo;
  PROCESS erts_;
  int len;

  /* receive the pid of erts */
  sig = (union ErlSig *)receive((SIGSELECT*)recv_pid_sig);
  erts_ = sig->erlPid.pid_;
  free_buf((union SIGNAL **)&sig);

  attach(NULL, erts_);

  while(1) {
    sig = (union ErlSig *)receive((SIGSELECT*)recv_any);

    switch(sig->sigNo) {
    
    case OS_ATTACH_SIG:
      fprintf(stderr, "PORT_PROCESS: ERTS process terminated, closing!\n");
      kill_proc(current_process());
      break;

    case PORT_DATA:
      len = sig->portData.len;
      echo = (union ErlSig *)alloc(sizeof(struct PortData)+len, PORT_DATA);
      memcpy(echo->portData.buf, sig->portData.buf, len); /*!?*/

      echo->portData.buf[len] = '\0';
      echo->portData.len = len;
      send((union SIGNAL **)&echo, erts_); /*fails here*/

      if(strcmp(sig->portData.buf, "stop") == 0) {
	kill_proc(current_process());
      }
      break;
    }
    free_buf((union SIGNAL **)&sig);
  }
}
