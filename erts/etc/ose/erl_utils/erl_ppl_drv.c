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

/* --------------------- PORT PROGRAM LOADER -----------------
 * This is a driver that can be used to load and start an OSE
 * program - an Erlang port program - given the load module 
 * name. Example, loading the module "drv_lm":
 *  
 *        P = erlang:open_port({spawn,erl_ppl_drv}, []).
 *        Result = port_call(P, 0, "drv_lm")
 *
 * Result = {ok,Handle} | {error,Reason}
 *
 * Note that the port program should take care of registering 
 * and unregistering itself!
 * ----------------------------------------------------------- */


#include <stdio.h>
#include "ose.h"

#include "ose_erl_driver.h"
#include "erl_error.h"
#include "ei.h"

#include "stdlib.h"
#include "stdio.h"
#include "string.h"

#include "prh.sig"
#include "prhfuncs.h"

#define LOAD   0
#define UNLOAD 1

#define NO_PLS      -1
#define PGM_NOT_REG -2

#define FAIL(v, r) { ((v)=(r)); return -1; } 
#define SUCCESS(v, r) { ((v)=(r)); return 0; }

#define ATTACH_SIGNO (~0UL)
#define PLS_SERVER "ose_pls_elf/"

/* -------------------------------------------------------------------------
** Data types
**/

typedef struct _erl_drv_data PplDrvData;

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

/* -------------------------------------------------------------------------
** Entry struct
**/

static PplDrvData *ppl_drv_start(ErlDrvPort port, char *command);
static void        ppl_drv_stop(PplDrvData *data_p);
static void        ppl_drv_output(PplDrvData *data_p, char *buf, int len);
static void        ppl_drv_finish(void);
static int         ppl_drv_call(PplDrvData *data_p, unsigned int command, 
				char *buf, int len, char **rbuf, int rlen, 
				unsigned *ret_flags); 

static ErlDrvEntry ppl_drv_entry = { 
    NULL, /* init */
    ppl_drv_start,
    ppl_drv_stop,
    ppl_drv_output,
    NULL, /* ready_input */
    NULL, /* ready_output */
    "erl_ppl_drv",
    ppl_drv_finish,
    NULL, /* handle */
    NULL, /* control */
    NULL, /* timeout */
    NULL, /* outputv */
    NULL, /* ready_async */
    NULL, /* flush */
    ppl_drv_call,
    NULL  /* event */
};



/* -------------------------------------------------------------------------
** Entry functions
**/

ERL_DRIVER_INIT(erl_ppl_drv)
{
  DRIVER_INTERFACE_INIT();
  return &ppl_drv_entry;
}

static PplDrvData *ppl_drv_start(ErlDrvPort port, char *command) {
    void *void_ptr;
    
    return void_ptr = port;
}

static void ppl_drv_stop(PplDrvData *data_p) {
}

static void ppl_drv_output(PplDrvData *data_p, char *buf, int len) {
}

static void ppl_drv_finish() {
}

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
    return -1;
  } 
  send(sig, pls_);
  *sig = alloc(sizeof(SIGSELECT), ATTACH_SIGNO);
  attref = attach(sig, pls_);
  *sig = receive((SIGSELECT *) select);
  if((*sig)->sigNo == ATTACH_SIGNO) {
    free_buf(sig);
    return -1;
  }
  detach(&attref);
  return 0;
}

extern int add_pgm_handle(char*, PrhProgramHandle);

static int load_pgm(char *name, PrhProgramHandle *result, int *reason) {
  struct PrhConnectAsfReply *conReply;
  struct PrhLoadProgramReply *loadReply;
  PrhProgramHandle hnd;
  PrhStatus res;
  union SIGNAL *sig;
  int size, status;
  char *elfname;

  size = strlen(name) + 2;
  sig = alloc(sizeof(struct PrhLoadProgramRequest) + size,
	      PRH_LOAD_PROGRAM_REQUEST);
  sig->loadProgramRequest.programHandle = PRH_ALLOCATE_HANDLE;
  sig->loadProgramRequest.options = 0;
  prh_init_strings(&sig->loadProgramRequest.strings);
  prh_add_string(&sig->loadProgramRequest.strings, name);
  printf("Loading %s...\n", name);
  if(pls_req(&sig) < 0) FAIL(*reason, NO_PLS)
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
    printf("Trying %s...\n", elfname);
    driver_free(elfname);
    if(pls_req(&sig) < 0) FAIL(*reason, NO_PLS)
    /* start program (which should register the port prog) */
    if(loadReply->status == PRH_SUCCESS) {
      hnd = loadReply->programHandle;
      free_buf(&sig);   
      sig = alloc(sizeof(struct PrhStartProgramRequest),
		  PRH_START_PROGRAM_REQUEST);
      sig->startProgramRequest.programHandle = hnd;
      printf("Starting %s (%u)...\n", name, hnd);
      if(pls_req(&sig) < 0) FAIL(*reason, NO_PLS)
      if(sig->startProgramReply.status == PRH_SUCCESS) {
	free_buf(&sig);
	printf("Mapping %s -> %u...\n", name, hnd);
	/* save program handle with name and entrypoint */
	if(!add_pgm_handle(name, hnd)) {
	  *result = hnd;
	  FAIL(*reason, PGM_NOT_REG)
	}
	SUCCESS(*result, hnd)
      }
      status = sig->startProgramReply.status;
      free_buf(&sig);
      FAIL(*reason, status)
    }
    status = loadReply->status;
    free_buf(&sig);
    FAIL(*reason, status)
  }
}

static int unload_pgm(PrhProgramHandle hnd, int *reason) {
  union SIGNAL *sig;
  static const SIGSELECT select[] = {3, PRH_KILL_PROGRAM_REPLY, 
				     PRH_REMOVE_LM_REPLY, 
				     ATTACH_SIGNO};
  PrhStatus status;
  
  /* kill the program */
  sig = alloc(sizeof (struct PrhKillProgramRequest),
	      PRH_KILL_PROGRAM_REQUEST);
  sig->killProgramRequest.programHandle = (PrhProgramHandle)hnd;
  printf("Unloading pgm %d...\n", hnd);
  if(pls_req(&sig) < 0) FAIL(*reason, NO_PLS)
  if(sig->killProgramReply.status != PRH_SUCCESS) {
    status = sig->killProgramReply.status;
    free_buf(&sig);
    FAIL(*reason, status)
  }
  free_buf(&sig);
  SUCCESS(*reason, 0)
}
 
static int encode_ok(unsigned long result, char **rbuf, int rlen) {
  int nlen;
  ei_x_buff x;

  ei_x_new(&x);
  ei_x_format(&x, "{~a,~u}", "ok", result);
  nlen = x.index;
  if (nlen > rlen) {
    *rbuf = driver_alloc(nlen);
  }
  memcpy(*rbuf,x.buff,nlen);
  ei_x_free(&x);
  return nlen;
}
  
static int encode_error(int reason, char **rbuf, int rlen) {
  int nlen;
  ei_x_buff x;

  ei_x_new(&x);
  ei_x_format(&x, "{~a,~i}", "error", reason);
  nlen = x.index;
  if (nlen > rlen) {
    *rbuf = driver_alloc(nlen);
  }
  memcpy(*rbuf,x.buff,nlen);
  ei_x_free(&x);
  return nlen;
}
  
static int ppl_drv_call(PplDrvData *data_p, unsigned int command, char *buf, 
			int len, char **rbuf, int rlen, unsigned *ret_flags) 
{
  char name[256];
  PrhProgramHandle hnd; 
  int reason, ver, type, sz, index = 0, dummy;
  ei_x_buff x;
  
  ei_decode_version(buf, &index, &ver);

  switch(command) {
  case LOAD:
    ei_decode_string(buf, &index, name);
    if(load_pgm(name, &hnd, &reason) < 0) {
      if(reason == PGM_NOT_REG) {
	unload_pgm(hnd, &dummy);
      }
      return encode_error(reason, rbuf, rlen);
    }
    return encode_ok((unsigned long)hnd, rbuf, rlen);
    
  case UNLOAD:
    ei_decode_ulong(buf, &index, (unsigned long *)&hnd);
    if(unload_pgm(hnd, &reason) < 0)
      return encode_error(reason, rbuf, rlen);
    return encode_ok(0, rbuf, rlen);
    
  default:
    return -1;
  }
}
