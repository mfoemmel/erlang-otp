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
 * Include file for erlang driver writers in OSE.
 *
 */

#ifndef __OSE_ERL_DRIVER_H__
#define __OSE_ERL_DRIVER_H__

#include "ose.h"
#include "erl_driver.h"

#ifndef _REG_ERL_USER_PGMS
#define _REG_ERL_USER_PGMS
#define REG_ERL_USER_PGMS  void reg_erl_user_pgms(void)
#endif

extern int erl_reg_driver(char *name, void *drv_init_func, PROCESS whoami_);
extern int erl_driver_sig_pending(ErlDrvPort port, void *sig);

/* 
   ---- struct with all functions in the erlang driver interface ----
*/
typedef struct erl_drv_if {
  int (*driver_select)(ErlDrvPort port, ErlDrvEvent event, int mode, int on);
  int (*driver_event)(ErlDrvPort port, ErlDrvEvent event, 
		      ErlDrvEventData event_data);
  int (*driver_output)(ErlDrvPort port, char *buf, int len);
  int (*driver_output2)(ErlDrvPort port, char *hbuf, int hlen, 
			char *buf, int len);
  int (*driver_output_binary)(ErlDrvPort port, char *hbuf, int hlen,
			      ErlDrvBinary* bin, int offset, int len);
  int (*driver_outputv)(ErlDrvPort port, char* hbuf, int hlen, ErlIOVec *ev,
			int skip);
  int (*driver_vec_to_buf)(ErlIOVec *ev, char *buf, int len);
  int (*driver_set_timer)(ErlDrvPort port, unsigned long time);
  int (*driver_cancel_timer)(ErlDrvPort port);
  int (*driver_read_timer)(ErlDrvPort port, unsigned long *time_left); 
  char* (*erl_errno_id)(int error); 
  int (*driver_failure_eof)(ErlDrvPort port);
  int (*driver_failure_atom)(ErlDrvPort port, char *string);
  int (*driver_failure_posix)(ErlDrvPort port, int error);
  int (*driver_failure)(ErlDrvPort port, int error);
  int (*driver_exit )(ErlDrvPort port, int err);
  void (*set_busy_port)(ErlDrvPort port, int on);
  void (*set_port_control_flags)(ErlDrvPort port, int flags);
  int (* get_port_flags)(ErlDrvPort port);
  ErlDrvBinary* (*driver_alloc_binary)(int size);
  ErlDrvBinary* (*driver_realloc_binary)(ErlDrvBinary *bin, int size);
  void (*driver_free_binary)(ErlDrvBinary *bin);
  void* (*driver_alloc)(size_t size);
  void* (*driver_realloc)(void *ptr, size_t size);
  void (*driver_free)(void *ptr);
  int (*driver_enq)(ErlDrvPort port, char* buf, int len);
  int (*driver_pushq)(ErlDrvPort port, char* buf, int len);
  int (*driver_deq)(ErlDrvPort port, int size);
  int (*driver_sizeq)(ErlDrvPort port);
  int (*driver_enq_bin)(ErlDrvPort port, ErlDrvBinary *bin, int offset, 
			int len);
  int (*driver_pushq_bin)(ErlDrvPort port, ErlDrvBinary *bin, int offset,
			  int len);
  int (*driver_peekqv)(ErlDrvPort port, ErlIOVec *ev);
  SysIOVec* (*driver_peekq)(ErlDrvPort port, int *vlen);
  int (*driver_enqv)(ErlDrvPort port, ErlIOVec *ev, int skip);
  int (*driver_pushqv)(ErlDrvPort port, ErlIOVec *ev, int skip);
  void (*add_driver_entry)(ErlDrvEntry *de);
  int (*remove_driver_entry)(ErlDrvEntry *de);
  ErlDrvTermData (*driver_mk_atom)(char*);
  ErlDrvTermData (*driver_mk_port)(ErlDrvPort);
  ErlDrvTermData (*driver_connected)(ErlDrvPort);
  ErlDrvTermData (*driver_caller)(ErlDrvPort);
  int (*driver_output_term)(ErlDrvPort ix, ErlDrvTermData* data, int len);
  int (*driver_send_term)(ErlDrvPort ix, ErlDrvTermData to,
			  ErlDrvTermData* data, int len);
  long (*driver_async)(ErlDrvPort ix,
		       unsigned int* key,
		       void (*async_invoke)(void*), 
		       void* async_data,
		       void (*async_free)(void*));
  int (*driver_async_cancel)(unsigned int key);
  int (*driver_lock_driver)(ErlDrvPort ix);
} ErlDrvIf;


/* ---------- Registration of dynamic driver. Example: -------

   OS_PROCESS(ose_lm_init_proc) 
   {
      ERL_DRIVER_DECL(driver_example);
      ERL_DRIVER_REG(driver_example);
      stop(current_process());
   }

   (See erl_user_pgm.c for registering a static driver).

   --------- Driver implementation. Example: --------

   ERL_DRIVER_INIT(driver_example)
   {
       DRIVER_INTERFACE_INIT();
       return &driver_example_entry;
   }

   static ErlDrvData start(ErlDrvPort port, char *buf, SysDriverOpts* opts)
   {
       driver_set_timer(port, 1000);
       return (ErlDrvData)port;
   }

   ...

   ----------------------------------------------------------- */


/* NAME is the name of the driver as well as the load module id */

/* declare the driver entrypoint */
#define ERL_DRIVER_DECL(NAME) extern ErlDrvEntry* NAME##_init(ErlDrvIf*)

/* register the driver entrypoint */
#define ERL_DRIVER_REG(NAME) erl_reg_driver(#NAME, &(NAME##_init), \
                                            current_process())

/* Note! There is no macro for unregistering since this is
   handled by ERTS when erl_ddll:unload_driver/1 is called. */

/* declare this driver init function */
#define ERL_DRIVER_INIT(NAME)             \
        static ErlDrvIf* erl_drv_funcs_g; \
        ErlDrvEntry* NAME##_init(ErlDrvIf* erl_drv_funcs)

/* initialise global pointer to driver i/f struct */
#define DRIVER_INTERFACE_INIT() (erl_drv_funcs_g = erl_drv_funcs)

/* call function in erlang driver i/f: DRIVER_APPLY(Func, Args...)
   --- this only works with the gcc preprocessor --- */
/* #define DRIVER_APPLY(FUNC, ...) ((*erl_drv_funcs_g->FUNC)(__VA_ARGS__)) */

/* a macro for each function in the erlang driver interface */
#ifndef NO_DRIVER_MACROS
#define driver_select (*erl_drv_funcs_g->driver_select)
#define driver_event (*erl_drv_funcs_g->driver_event)
#define driver_output (*erl_drv_funcs_g->driver_output)
#define driver_output2 (*erl_drv_funcs_g->driver_output2)
#define driver_output_binary (*erl_drv_funcs_g->driver_output_binary)
#define driver_outputv (*erl_drv_funcs_g->driver_outputv)
#define driver_vec_to_buf (*erl_drv_funcs_g->driver_vec_to_buf)
#define driver_set_timer (*erl_drv_funcs_g->driver_set_timer)
#define driver_cancel_timer (*erl_drv_funcs_g->driver_cancel_timer)
#define driver_read_timer (*erl_drv_funcs_g->driver_read_timer)
#define erl_errno_id (*erl_drv_funcs_g->erl_errno_id)
#define driver_failure_eof (*erl_drv_funcs_g->driver_failure_eof)
#define driver_failure_atom (*erl_drv_funcs_g->driver_failure_atom)
#define driver_failure_posix (*erl_drv_funcs_g->driver_failure_posix)
#define driver_failure (*erl_drv_funcs_g->driver_failure)
#define driver_exit (*erl_drv_funcs_g->driver_exit)
#define set_busy_port (*erl_drv_funcs_g->set_busy_port)
#define set_port_control_flags (*erl_drv_funcs_g->set_port_control_flags)
#define get_port_flags (*erl_drv_funcs_g->get_port_flags)
#define driver_alloc_binary (*erl_drv_funcs_g->driver_alloc_binary)
#define driver_realloc_binary (*erl_drv_funcs_g->driver_realloc_binary)
#define driver_free_binary (*erl_drv_funcs_g->driver_free_binary)
#define driver_alloc (*erl_drv_funcs_g->driver_alloc)
#define driver_realloc (*erl_drv_funcs_g->driver_realloc)
#define driver_free (*erl_drv_funcs_g->driver_free)
#define driver_enq (*erl_drv_funcs_g->driver_enq)
#define driver_pushq (*erl_drv_funcs_g->driver_pushq)
#define driver_deq (*erl_drv_funcs_g->driver_deq)
#define driver_sizeq (*erl_drv_funcs_g->driver_sizeq)
#define driver_enq_bin (*erl_drv_funcs_g->driver_enq_bin)
#define driver_pushq_bin (*erl_drv_funcs_g->driver_pushq_bin)
#define driver_peekqv (*erl_drv_funcs_g->driver_peekqv)
#define driver_peekq (*erl_drv_funcs_g->driver_peekq)
#define driver_enqv (*erl_drv_funcs_g->driver_enqv)
#define driver_pushqv (*erl_drv_funcs_g->driver_pushqv)
#define add_driver_entry (*erl_drv_funcs_g->add_driver_entry)
#define remove_driver_entry (*erl_drv_funcs_g->remove_driver_entry)
#define driver_mk_atom (*erl_drv_funcs_g->driver_mk_atom)
#define driver_mk_port (*erl_drv_funcs_g->driver_mk_port)
#define driver_connected (*erl_drv_funcs_g->driver_connected)
#define driver_caller (*erl_drv_funcs_g->driver_caller)
#define driver_output_term (*erl_drv_funcs_g->driver_output_term)
#define driver_send_term (*erl_drv_funcs_g->driver_send_term)
#define driver_async (*erl_drv_funcs_g->driver_async)
#define driver_async_cancel (*erl_drv_funcs_g->driver_async_cancel)
#define driver_lock_driver (*erl_drv_funcs_g->driver_lock_driver)
#endif

#endif
