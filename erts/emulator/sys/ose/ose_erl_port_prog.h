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

/*
 * Include file for erlang port program writers in OSE.
 */

#ifndef __OSE_ERL_PORT_PROG_H__
#define __OSE_ERL_PORT_PROG_H__

#include "ose.h"

#ifndef _REG_ERL_USER_PGMS
#define _REG_ERL_USER_PGMS
#define REG_ERL_USER_PGMS  void reg_erl_user_pgms(void)
#endif


/* ---------- Registration of port program. Example: -------

   OS_PROCESS(ose_lm_init_proc) 
   {
      ERL_PORT_PROG_DECL(port_prog_example);
      ERL_PORT_PROG_REG(port_prog_example);
      stop(current_process());
   }

   -------- Implementation of port program. Example: -------

   union SIGNAL {
   SIGSELECT sigNo;
   struct PortData portData;
   }; 

   ERL_PORT_PROG(port_prog_example)
   {
      static const SIGSELECT recv[] = {2, OS_ATTACH_SIG, PORT_DATA};
      union SIGNAL *sig;
      PROCESS erts_;

      if(!hunt("erts", 0, &erts_, NULL))
         kill_proc(current_process());
      attach(NULL, erts_);
    
      sig = receive((SIGSELECT*)recv);

      switch(sig->sigNo) {

      case OS_ATTACH_SIG:
         free_buf(&sig);
         kill_proc(current_process());
	 break;

      case PORT_DATA: 
         ...
         free_buf(&sig);
      }
   }

   ------------- Unregistration of port program -----------

   Since a dynamic port program is loaded, started, killed and 
   deleted outside the control of ERTS, a program must be
   explicitly unregistered when it's not to be used anymore.
   (Perhaps the load module is even to be deleted). 

   This is accomplished by using the macro:

           ERL_PORT_PROG_UNREG(NAME). 

   The macro could be used in the port program code to unregister 
   the program before termination. It could also be used in a shell 
   command, for example, to manually unregister the program.

   -------------------------------------------------------- */

/* functions called with macros below, don't use directly */
extern int erl_reg_port_prog(char *name, OSENTRYPOINT *entrypoint, PROCESS whoami_);
extern int erl_unreg_port_prog(char *name);


/* NAME is the name of the port prog, the OSE process and the entrypoint */

/* declare the port program entrypoint */
#define ERL_PORT_PROG_DECL(NAME) extern OSENTRYPOINT NAME

/* register the port program entrypoint */
#define ERL_PORT_PROG_REG(NAME) erl_reg_port_prog(#NAME, &NAME, current_process())

/* unregister the port program entrypoint */
#define ERL_PORT_PROG_UNREG(NAME) erl_unreg_port_prog(#NAME)

/* declare the port program start process */
#define ERL_PORT_PROG(NAME) OS_PROCESS(NAME)

#endif
