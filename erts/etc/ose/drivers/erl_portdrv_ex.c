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
#include "errno.h"
#include "dbgprintf.h"

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#include "ose_erl_driver.h"


/* -------------------------------------------------------------------------
** Data types
**/

typedef struct _erl_drv_data EchoDrvData;



/* -------------------------------------------------------------------------
** Entry struct
**/

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command);
static void         echo_drv_stop(EchoDrvData *data_p);
static void         echo_drv_output(EchoDrvData *data_p, char *buf, int len);
static void         echo_drv_finish(void);
static int          echo_drv_control(EchoDrvData *data_p, unsigned int command,
				     char *buf, int len,
				     char **rbuf, int rlen);

static ErlDrvEntry echo_drv_entry = { 
    NULL, /* init */
    echo_drv_start,
    echo_drv_stop,
    echo_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "erl_portdrv_ex",
    echo_drv_finish,
    NULL, /* handle */
    echo_drv_control,
    NULL, /* timeout */
    NULL, /* outputv */
    NULL  /* ready_async */
};



/* -------------------------------------------------------------------------
** Entry functions
**/

ERL_DRIVER_INIT(erl_portdrv_ex)
{
  DRIVER_INTERFACE_INIT();
  return &echo_drv_entry;
}

static EchoDrvData *echo_drv_start(ErlDrvPort port, char *command) {
    void *void_ptr;
    
    return void_ptr = port;
}

static void echo_drv_stop(EchoDrvData *data_p) {
}

static void echo_drv_output(EchoDrvData *data_p, char *buf, int len) {
    void *void_ptr;
    ErlDrvPort port = void_ptr = data_p;
    
    driver_output(port, buf, len);
}

static void echo_drv_finish() {
}

static int echo_drv_control(EchoDrvData *data_p, unsigned int command,
			    char *buf, int len,
			    char **rbuf, int rlen) {
    return 0;
}


/* ------------- LM INIT PROCESSES ------------ */

/* register port program from static process */
OS_PROCESS(erl_portdrv_ex_reg) {
  ERL_DRIVER_DECL(erl_portdrv_ex);
  ERL_DRIVER_REG(erl_portdrv_ex);
  stop(current_process());
}

/* good for nothing LM init process */
OS_PROCESS(erl_portdrv_ex_main) {
  stop(current_process());
}

