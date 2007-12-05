/*<copyright>
 * <year>2004-2007</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
 * ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include "erl_driver.h"
#include "rxposix.h"

static ErlDrvData rx_driver_start(ErlDrvPort port, char *buff);
static void rx_driver_stop(ErlDrvData handle);
static void rx_driver_finish(void);
static int rx_driver_control(ErlDrvData handle, unsigned int command, 
			      char* buf, int count, char** res, int res_size);

/*
** The driver struct
*/
static ErlDrvEntry rx_driver_entry = {
  NULL,				/* F_PTR init, N/A */
  rx_driver_start,	 /* L_PTR start, called when port is opened */
  rx_driver_stop,	  /* F_PTR stop, called when port is closed */
  NULL,		       /* F_PTR output, called when erlang has sent */
  NULL,		   /* F_PTR ready_input, called when input descriptor 
		      ready */
  NULL,			    /* F_PTR ready_output, called when output 
			       descriptor ready */
  "erl_rx_driver",  /* char *driver_name, the argument to open_port */
  rx_driver_finish,	      /* F_PTR finish, called when unloaded */
  NULL,				/* void * that is not used (BC) */
  rx_driver_control,	    /* F_PTR control, port_control callback */
  NULL,				/* F_PTR timeout, reserved */
  NULL,				/* F_PTR outputv, reserved */
};

DRIVER_INIT(erl_rx_driver)
{
  return &rx_driver_entry;
}

static ErlDrvData rx_driver_start(ErlDrvPort port, char *buff)
{      
  set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
  return (ErlDrvData) 0;
}

static void
rx_driver_stop(ErlDrvData handle) 
{  
}

static void
rx_driver_finish(void) 
{
}

#define MAX_MATCHES 256

static int
rx_driver_control(ErlDrvData handle, unsigned op,
		   char* buf, int count, char** res, int res_size)
{
  char* pattern;
  int pattern_len = *((int *) buf);
  char* str;
  int str_len;
  regex_t compiled;
  regmatch_t* matches = NULL;
  int n;

  buf += sizeof(unsigned);
  count -= sizeof(unsigned);
  pattern = buf;
  str = buf + pattern_len;
  str_len = count - pattern_len;

  /*
   * Compile the pattern.
   */
  n = regncomp(&compiled, pattern, pattern_len, REG_EXTENDED);
  if (n != 0) {
    return 0;
  }

  /*
   * Match the pattern.
   */
  n = regnexec(&compiled, str, str_len, 0, &matches, REG_ALLOC_REGS);
  if (n != 0) {
    *res = 0;
  } else {
    ErlDrvBinary* bin;
    int* rp;
    int i;

    bin = driver_alloc_binary(2*sizeof(int)*compiled.re_nsub);
    *res = (void *) bin;
    rp = (int *) bin->orig_bytes;
    
    for (i = 0; i < compiled.re_nsub; i++) {
      *rp++ = matches[i].rm_so;
      *rp++ = matches[i].rm_eo;
    }
  }

  /*
   * Free the compiled regexp.
   */
#if 0
  regfree(&compiled);
#endif
  return 0;
}
