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


/* -------------------------------------------------------------------
   -------------------------------------------------------------------

                TO REGISTER PROGRAMS LINKED WITH ERTS

   This file exports a function for registering user port programs and
   drivers. The function is called by the Erlang Runtime System as it 
   starts up. Declare and register those programs that are statically 
   linked with ERTS within this function. The function should have the
   following form:

      REG_ERL_USER_PGMS { { <PORT_PGM_DECL> 
                            <PORT_PGM_REG> }
                          { <DRIVER_DECL>   
                            <DRIVER_REG>   } }

   Registering means supplying the name of a port program or driver, 
   which is translated into, and mapped to, the entrypoint of the program 
   (an OSENTRYPOINT in case of a port program and an init function pointer
   in case of a driver). This mapping enables ERTS to later start the
   program by name when erlang:open_port/2 is called. 

   Only static port programs and drivers must be registered by this
   function. Programs that are compiled and loaded separately from
   ERTS (as OSE PRH load modules) should register in the load module 
   initialization process instead. Note that the OSE_DRIVER_INIT macro 
   must be used to declare driver init functions (it corresponds to 
   DRIVER_INIT for other platforms).

   -------------------------------------------------------------------
   -------------------------------------------------------------------- */


#include "ose_erl_port_prog.h"
#include "ose_erl_driver.h"

REG_ERL_USER_PGMS {
  {
  /* -------------------------------------------------------------------  
                       REGISTER PORT PROGRAMS

     First declare the port programs by using the macro:

            ERL_PORT_PROG_DECL(<name>)

     name is the name of the port program to be used in erlang:open_port/2. 
     It is also the name of the OSE process that will run the program, 
     as well as the name of the OSENTRYPOINT.

     Then specify the port programs to be registered by means of the
     registration macro:

           ERL_PORT_PROG_REG(<name>)

     Example:

          {
            ERL_PORT_PROG_DECL(my_port_prog1);
	    ERL_PORT_PROG_DECL(my_port_prog2);

	    ERL_PORT_PROG_REG(my_port_prog1);
	    ERL_PORT_PROG_REG(my_port_prog2);
	  } 

     The main function of the port program (in the file that implements
     the program), should be declared as:

           ERL_PORT_PROG(<name>)

     -------------------------------------------------------------------- */

    ERL_PORT_PROG_DECL(erl_stat_port_ex); /* erl_stat_port_ex.c */

    ERL_PORT_PROG_REG(erl_stat_port_ex); /* erl_stat_port_ex.c */

  }{
  /* -------------------------------------------------------------------  
                         REGISTER DRIVERS

     First declare the drivers by using the macro:

            ERL_DRIVER_DECL(<name>)

     name is the name of the driver to be used in erlang:open_port/2. 

     Then specify the drivers to be registered by means of the
     registration macro:

           ERL_DRIVER_REG(<name>)

     Example:

          {
            ERL_DRIVER_DECL(my_driver1);
	    ERL_DRIVER_DECL(my_driver2);

	    ERL_DRIVER_REG(my_driver1);
	    ERL_DRIVER_REG(my_driver2);
	  } 

     The init function of the driver (in the file that implements
     the program), should be declared as:

           ERL_DRIVER_INIT(<name>, <erl_driver_if>)

     where erl_driver_if is the struct of pointers to the functions
     in the erlang driver interface (type ErlDrvIf*). ERL_DRIVER_INIT
     should return a pointer to a driver entry struct (type ErlDrvEntry*).

     -------------------------------------------------------------------- */


    ERL_DRIVER_DECL(erl_stat_portdrv_ex); /* erl_stat_portdrv_ex.c */

#ifdef USE_PPL_DRV
    ERL_DRIVER_DECL(erl_ppl_drv);         /* erl_ppl_drv.c */
    ERL_DRIVER_REG(erl_ppl_drv);         /* erl_ppl_drv.c */
#endif

    ERL_DRIVER_REG(erl_stat_portdrv_ex);  /* erl_stat_portdrv_ex.c */
  }
}
