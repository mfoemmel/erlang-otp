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
 * Interface functions to the dynamic linker using dl* functions.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif


#include "sys.h"
#include "global.h"
#include "erl_ddll.h"

#include "ose.h"
#include "mms_err.h"
#include "errno.h"
#include "sys/stat.h"
#include "dbgprintf.h"
#include "efs.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#include "prh.sig"
#include "prhfuncs.h"

#define NO_DRIVER_MACROS
#include "ose_erl_driver.h"
#undef  NO_DRIVER_MACROS

#define MAX_NAME_LEN 255       /* XXX should we get the system path size? */
#define ATTACH_SIGNO (~0UL)
#define PLS_SERVER "ose_pls_elf/"

#define ERL_NO_PLS       -1
#define ERL_DRV_NOT_REG  -2
#define ERL_DRV_EXISTS   -3
#define ERL_NO_HND       -4

static int dlopen_error;	/* set if error occurs */

extern int get_pgm_data(char*, void**, void*, int*);
extern int add_pgm_handle(char*, PrhProgramHandle);
extern void del_pgm(char*);

union SIGNAL
{
  SIGSELECT sigNo;
  struct PrhLoadProgramRequest loadProgramRequest;
  struct PrhLoadProgramReply loadProgramReply;
  struct PrhStartProgramRequest startProgramRequest;
  struct PrhStartProgramReply startProgramReply;
  struct PrhKillProgramRequest killProgramRequest;
  struct PrhKillProgramReply killProgramReply;
  struct PrhRemoveLMRequest removeLMRequest;
  struct PrhRemoveLMReply removeLMReply;
};

static ErlDrvIf erl_drv_funcs = 
{
  driver_select,
  driver_event,
  driver_output,
  driver_output2,
  driver_output_binary,
  driver_outputv,
  driver_vec_to_buf,
  driver_set_timer,
  driver_cancel_timer,
  driver_read_timer,
  erl_errno_id,
  driver_failure_eof,
  driver_failure_atom,
  driver_failure_posix,
  driver_failure,
  driver_exit,
  set_busy_port,
  set_port_control_flags,
  get_port_flags,
  driver_alloc_binary,
  driver_realloc_binary,
  driver_free_binary,
  driver_alloc,
  driver_realloc,
  driver_free,
  driver_enq,
  driver_pushq,
  driver_deq,
  driver_sizeq,
  driver_enq_bin,
  driver_pushq_bin,
  driver_peekqv,
  driver_peekq,
  driver_enqv,
  driver_pushqv,
  add_driver_entry,
  remove_driver_entry,
  driver_mk_atom,
  driver_mk_port,
  driver_connected,
  driver_caller,
  driver_output_term,
  driver_send_term,
  driver_async,
  driver_async_cancel,
  driver_attach,
  driver_detach
};

static int pls_req(union SIGNAL **sig) { 
  PROCESS pls_;
  OSATTREF attref;
  static const SIGSELECT select[] = {5, PRH_LOAD_PROGRAM_REPLY,  
				        PRH_START_PROGRAM_REPLY, 
				        PRH_KILL_PROGRAM_REPLY, 
				        PRH_REMOVE_LM_REPLY, 
				        ATTACH_SIGNO};

  if(!hunt(PLS_SERVER, 0, &pls_, NULL)) {
    free_buf(sig);
    dlopen_error = ERL_NO_PLS;
    return -1;
  } 
  send(sig, pls_);
  *sig = alloc(sizeof(SIGSELECT), ATTACH_SIGNO);
  attref = attach(sig, pls_);
  *sig = receive((SIGSELECT *) select);
  if((*sig)->sigNo == ATTACH_SIGNO) {
    free_buf(sig);
    dlopen_error = ERL_NO_PLS;
    return -1;
  }
  detach(&attref);
  return 0;
}

/*
 * ddll_open(name)
 *
 * name is the identity of the load module containing a driver 
 * (one driver per LM since ddll_drv will only add one driver entry
 * per loading occasion). The identity of the LM must be the name of  * the driver. An elf extension should never be specified. If an LM 
 * "name" isn't found, "name.elf" will be searched for automatically.
 *
 * It is assumed that an AS has already been connected!
 *
 * Note: erl_ddll:load_driver/2 is also called for a static driver 
 * linked with erts (so that the driver entry may be added to 
 * driver_list, see io.c). For this reason we need to check here if 
 * the driver is registered as static and, if so, ignore loading a
 * module.
 *
 * Returns a pointer to the init function of the driver.
 */
void *ddll_open(char *fullname)
{
  struct PrhConnectAsfReply *conReply;
  struct PrhLoadProgramReply *loadReply;
  PrhProgramHandle programHandle;
  PrhStatus res;
  union SIGNAL *sig;
  int size, is_static;
  void *entrypoint;
  char *name;
  char *elfname;

  dlopen_error = 0;

  /* name does not contain a path, ignore the initial '/' */
  name = fullname + 1;

  /* If the driver is already registered, we'll skip the loading
     procedure and return the registered entrypoint (the user has to
     unload a dynaimc driver before loading a new one with the same 
     name). This is necessary for static drivers, but also to avoid
     data inconsistency if a driver has been previously loaded and 
     started from outside erlang. */

  if(get_pgm_data(name, &entrypoint, NULL, &is_static)) {
    DEBUGF(("static or already registered with %X\n", entrypoint));
    return entrypoint;
  }

 /* load module */  
  size = strlen(name) + 2;
  sig = alloc(sizeof(struct PrhLoadProgramRequest) + size,
	      PRH_LOAD_PROGRAM_REQUEST);
  sig->loadProgramRequest.programHandle = PRH_ALLOCATE_HANDLE;
  sig->loadProgramRequest.options = 0;
  prh_init_strings(&sig->loadProgramRequest.strings);
  prh_add_string(&sig->loadProgramRequest.strings, name);
  DEBUGF(("Loading %s...\n", name));
  if(pls_req(&sig) < 0)
    return NULL;
  loadReply = &sig->loadProgramReply;
  /* if LM wasn't found, try with an elf extension */
  if(loadReply->status == PRH_ELM_NOT_FOUND) {
    free_buf(&sig);
    elfname = driver_alloc(strlen(name) + 5);
    sprintf(elfname, "%s.elf", name);
    size = strlen(elfname) + 2;
    sig = alloc(sizeof(struct PrhLoadProgramRequest) + size,
		PRH_LOAD_PROGRAM_REQUEST);
    sig->loadProgramRequest.programHandle = PRH_ALLOCATE_HANDLE;
    sig->loadProgramRequest.options = 0;
    prh_init_strings(&sig->loadProgramRequest.strings);
    prh_add_string(&sig->loadProgramRequest.strings, elfname);
    DEBUGF(("Loading %s...\n", elfname));
    driver_free(elfname);
    if(pls_req(&sig) < 0)
      return NULL;
  }
  /* start program (which should register the driver) */
  if(loadReply->status == PRH_SUCCESS) {
    programHandle = loadReply->programHandle;
    free_buf(&sig);   
    sig = alloc(sizeof(struct PrhStartProgramRequest),
		PRH_START_PROGRAM_REQUEST);
    sig->startProgramRequest.programHandle = programHandle;
    DEBUGF(("Starting %s(%u)...\n", name, programHandle));
    if(pls_req(&sig) < 0)
      return NULL;
    if(sig->startProgramReply.status == PRH_SUCCESS) {
      free_buf(&sig);
      DEBUGF(("Mapping %s -> %u...\n", name, programHandle));
      /* save program handle with name and entrypoint */
      if(!add_pgm_handle(name, programHandle)) {
	dlopen_error = ERL_DRV_NOT_REG;
	return NULL;
      }
      get_pgm_data(name, &entrypoint, NULL, NULL);
      return entrypoint;
    }
    dlopen_error = sig->startProgramReply.status;
    free_buf(&sig);
    return NULL;
  }
  dlopen_error = loadReply->status;
  free_buf(&sig);
  return NULL;
}

/* We have "fooled" ddll_drv to call this function instead of the true driver
   init function. Here we know the address of the true driver init function 
   and can call it with a struct containing pointers to the functions in the 
   erlang driver interface. */
static ErlDrvEntry* ddll_init_caller(DE_Handle* dh)
{
  ErlDrvEntry* (*init_func)(ErlDrvIf*);

  init_func = dh->handle;
  return (*init_func)(&erl_drv_funcs);
}

/*
 * ddll_sym is normally used to find the address of an arbitrary symbol. Here, 
 * instead, we return the address of a function implemented in this file. This 
 * function will use the handle element in the DE_Handle struct (which it 
 * receives from ddll_drv) to retrieve the true driver init function address
 * and then call this init function with a struct containing pointers to the
 * functions in the erlang driver interface.
 * 
 * The init function of the driver should have been registered during load
 * module program start. (It's important that PLS does not send 
 * startProgramReply until the driver has finished registering itself with 
 * the erts pgm server process, i.e. the driver should register during the 
 * LM initialisation phase). 
 */
void *ddll_sym(void *handle, char *func_name)
{
  return &ddll_init_caller;
}

/* 
 * Close a driver, meaning stop the program and unregister the driver.
 */
int ddll_close(void *entrypoint)
{
   union SIGNAL *sig;
   static const SIGSELECT select[] = {3, PRH_KILL_PROGRAM_REPLY, 
				         PRH_REMOVE_LM_REPLY, 
				         ATTACH_SIGNO};
   PrhStatus status;
   char name[256];
   void *tmp;
   void *hnd;
   int is_static;

   /* lookup the LM handle */
   name[0] = '\0';
   if(get_pgm_data(name, &entrypoint, &hnd, &is_static)) {
     if(is_static) {
       /* don't unregister a static driver, ignore */
       return 0;
     } 
     else if(hnd == NULL) {	/* not loaded by erlang */
       del_pgm(name);
       dlopen_error = ERL_NO_HND;
       return -1;
     }
   } else {			/* not found */
     dlopen_error = ERL_DRV_NOT_REG;
     return -1;
   }

   DEBUGF(("Removing entry %s at %X (handle %u)\n", name, entrypoint, hnd));

   /** at this point we know it's a registered dynamic driver **/

   /* kill the program */
   sig = alloc(sizeof (struct PrhKillProgramRequest),
	       PRH_KILL_PROGRAM_REQUEST);
   sig->killProgramRequest.programHandle = (PrhProgramHandle)hnd;
   if(pls_req(&sig) < 0)
     return -1;
   if(sig->killProgramReply.status != PRH_SUCCESS) {
     dlopen_error = sig->killProgramReply.status;
     free_buf(&sig);
     return -1;
   }
   free_buf(&sig);
   /* delete the program data */
   del_pgm(name);

   if(get_pgm_data(NULL, &entrypoint, &hnd, &is_static)) {
     fprintf(stderr, "Entry %X not deleted!\n", entrypoint);
     return -1;
   }
  
   return 0;
}

/*
 * Return string that describes the (current) error
 */
char *ddll_error() 
{
  static char msg[128];

  if(dlopen_error != 0) {
    switch(dlopen_error) {
    case ERL_NO_PLS:
      strcpy(msg, "cannot contact pls server");
      break;
    case ERL_DRV_NOT_REG:
      strcpy(msg, "driver has not been properly registered");
      break;
    case ERL_DRV_EXISTS:
      strcpy(msg, "driver is already loaded");
      break;
    case ERL_NO_HND:
      strcpy(msg, "driver wasn't loaded with load_driver/2, can't unload");
      break;
    default:
      sprintf(msg, "prh load/unload or start/kill error, status = %d", dlopen_error); 
    }
    return msg;
  }
  return "unknown error";
}

