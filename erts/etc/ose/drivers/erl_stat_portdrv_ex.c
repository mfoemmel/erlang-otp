#include <stdio.h>
#include "ose.h"
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
    "erl_stat_portdrv_ex",
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

ERL_DRIVER_INIT(erl_stat_portdrv_ex)
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
