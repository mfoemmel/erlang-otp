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
 * Include file for erlang driver writers using dynamic drivers on windows.
 */

/* Maybe this should be auto generated, but I'll leave that for now... */

#ifndef _ERL_WIN_DYN_DRIVER_H 
#define _ERL_WIN_DYN_DRIVER_H 

#define WDD_FTYPE(FunctionName) TWinDynDriver##FunctionName

#define WDD_TYPEDEF(RetType, FunctionName, Params) \
  typedef RetType WDD_FTYPE(FunctionName)##Params 

WDD_TYPEDEF(int, null_func,(void));
WDD_TYPEDEF(int, driver_failure_atom,(ErlDrvPort, char *));
WDD_TYPEDEF(int, driver_failure_posix,(ErlDrvPort, int));
WDD_TYPEDEF(int, driver_failure,(ErlDrvPort, int));
WDD_TYPEDEF(int, driver_exit, (ErlDrvPort, int));
WDD_TYPEDEF(int, driver_failure_eof, (ErlDrvPort));
WDD_TYPEDEF(int, driver_select, (ErlDrvPort, ErlDrvEvent, int, int));
WDD_TYPEDEF(int, driver_event, (ErlDrvPort, ErlDrvEvent,ErlDrvEventData));
WDD_TYPEDEF(int, driver_output, (ErlDrvPort, char *, int));
WDD_TYPEDEF(int, driver_output2, (ErlDrvPort, char *, int,char *, int));
WDD_TYPEDEF(int, driver_output_binary, (ErlDrvPort, char *, int,ErlDrvBinary*, int, int));
WDD_TYPEDEF(int, driver_outputv, (ErlDrvPort, char*, int, ErlIOVec *,int));
WDD_TYPEDEF(int, driver_vec_to_buf, (ErlIOVec *, char *, int));
WDD_TYPEDEF(int, driver_set_timer, (ErlDrvPort, unsigned long));
WDD_TYPEDEF(int, driver_cancel_timer, (ErlDrvPort));
WDD_TYPEDEF(int, driver_read_timer, (ErlDrvPort, unsigned long *));
WDD_TYPEDEF(char *, erl_errno_id, (int));
WDD_TYPEDEF(void, set_busy_port, (ErlDrvPort, int));
WDD_TYPEDEF(void, set_port_control_flags, (ErlDrvPort, int));
WDD_TYPEDEF(int, get_port_flags, (ErlDrvPort));
WDD_TYPEDEF(ErlDrvBinary *, driver_alloc_binary, (int));
WDD_TYPEDEF(ErlDrvBinary *, driver_realloc_binary, (ErlDrvBinary *, int));
WDD_TYPEDEF(void, driver_free_binary, (ErlDrvBinary *));
WDD_TYPEDEF(void *, driver_alloc, (size_t));
WDD_TYPEDEF(void *, driver_realloc, (void *, size_t));
WDD_TYPEDEF(void, driver_free, (void *));
WDD_TYPEDEF(int, driver_enq, (ErlDrvPort, char*, int));
WDD_TYPEDEF(int, driver_pushq, (ErlDrvPort, char*, int));
WDD_TYPEDEF(int, driver_deq, (ErlDrvPort, int));
WDD_TYPEDEF(int, driver_sizeq, (ErlDrvPort));
WDD_TYPEDEF(int, driver_enq_bin, (ErlDrvPort, ErlDrvBinary *, int,int));
WDD_TYPEDEF(int, driver_pushq_bin, (ErlDrvPort, ErlDrvBinary *, int,int));
WDD_TYPEDEF(int, driver_peekqv, (ErlDrvPort, ErlIOVec *));
WDD_TYPEDEF(SysIOVec *, driver_peekq, (ErlDrvPort, int *));
WDD_TYPEDEF(int, driver_enqv, (ErlDrvPort, ErlIOVec *, int));
WDD_TYPEDEF(int, driver_pushqv, (ErlDrvPort, ErlIOVec *, int));
WDD_TYPEDEF(void, add_driver_entry, (ErlDrvEntry *));
WDD_TYPEDEF(int, remove_driver_entry, (ErlDrvEntry *));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_atom, (char*));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_port,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_connected,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_caller,(ErlDrvPort));
WDD_TYPEDEF(ErlDrvTermData, driver_mk_term_nil,(void));
WDD_TYPEDEF(int, driver_output_term, (ErlDrvPort, ErlDrvTermData*, int));
WDD_TYPEDEF(int, driver_send_term, (ErlDrvPort, ErlDrvTermData, ErlDrvTermData*, int));
WDD_TYPEDEF(long, driver_async, (ErlDrvPort,unsigned int*,void (*)(void*),void*,void (*)(void*)));
WDD_TYPEDEF(int, driver_async_cancel, (unsigned int));
WDD_TYPEDEF(int, driver_attach, (ErlDrvPort));
WDD_TYPEDEF(int, driver_detach, (ErlDrvPort));
WDD_TYPEDEF(void *, driver_dl_open, (char *));
WDD_TYPEDEF(void *, driver_dl_sym, (void *, char *));
WDD_TYPEDEF(int, driver_dl_close, (void *));
WDD_TYPEDEF(char *, driver_dl_error, (void));
WDD_TYPEDEF(unsigned long, erts_alc_test, (unsigned long,
					   unsigned long,
					   unsigned long,
					   unsigned long));



typedef struct {
    WDD_FTYPE(null_func) *null_func;
    WDD_FTYPE(driver_failure_atom) *driver_failure_atom;
    WDD_FTYPE(driver_failure_posix) *driver_failure_posix;
    WDD_FTYPE(driver_failure) *driver_failure;
    WDD_FTYPE(driver_exit) *driver_exit;
    WDD_FTYPE(driver_failure_eof) *driver_failure_eof;
    WDD_FTYPE(driver_select) *driver_select;
    WDD_FTYPE(driver_event) *driver_event;
    WDD_FTYPE(driver_output) *driver_output;
    WDD_FTYPE(driver_output2) *driver_output2;
    WDD_FTYPE(driver_output_binary) *driver_output_binary;
    WDD_FTYPE(driver_outputv) *driver_outputv;
    WDD_FTYPE(driver_vec_to_buf) *driver_vec_to_buf;
    WDD_FTYPE(driver_set_timer) *driver_set_timer;
    WDD_FTYPE(driver_cancel_timer) *driver_cancel_timer;
    WDD_FTYPE(driver_read_timer) *driver_read_timer;
    WDD_FTYPE(erl_errno_id) *erl_errno_id;
    WDD_FTYPE(set_busy_port)* set_busy_port;
    WDD_FTYPE(set_port_control_flags) *set_port_control_flags;
    WDD_FTYPE(get_port_flags) *get_port_flags;
    WDD_FTYPE(driver_alloc_binary) *driver_alloc_binary;
    WDD_FTYPE(driver_realloc_binary) *driver_realloc_binary;
    WDD_FTYPE(driver_free_binary) *driver_free_binary;
    WDD_FTYPE(driver_alloc) *driver_alloc;
    WDD_FTYPE(driver_realloc) *driver_realloc;
    WDD_FTYPE(driver_free) *driver_free;
    WDD_FTYPE(driver_enq) *driver_enq;
    WDD_FTYPE(driver_pushq) *driver_pushq;
    WDD_FTYPE(driver_deq) *driver_deq;
    WDD_FTYPE(driver_sizeq) *driver_sizeq;
    WDD_FTYPE(driver_enq_bin)* driver_enq_bin;
    WDD_FTYPE(driver_pushq_bin) *driver_pushq_bin;
    WDD_FTYPE(driver_peekqv) *driver_peekqv;
    WDD_FTYPE(driver_peekq) *driver_peekq;
    WDD_FTYPE(driver_enqv) *driver_enqv;
    WDD_FTYPE(driver_pushqv) *driver_pushqv;
    WDD_FTYPE(add_driver_entry) *add_driver_entry;
    WDD_FTYPE(remove_driver_entry) *remove_driver_entry;
    WDD_FTYPE(driver_mk_atom) *driver_mk_atom;
    WDD_FTYPE(driver_mk_port) *driver_mk_port;
    WDD_FTYPE(driver_connected) *driver_connected;
    WDD_FTYPE(driver_caller) *driver_caller;
    WDD_FTYPE(driver_mk_term_nil) *driver_mk_term_nil;
    WDD_FTYPE(driver_output_term) *driver_output_term;
    WDD_FTYPE(driver_send_term) *driver_send_term;
    WDD_FTYPE(driver_async) *driver_async;
    WDD_FTYPE(driver_async_cancel) *driver_async_cancel;
    WDD_FTYPE(driver_attach) *driver_attach;
    WDD_FTYPE(driver_detach) *driver_detach;
    WDD_FTYPE(driver_dl_open) *driver_dl_open;
    WDD_FTYPE(driver_dl_sym) *driver_dl_sym;
    WDD_FTYPE(driver_dl_close) *driver_dl_close;
    WDD_FTYPE(driver_dl_error) *driver_dl_error;
    WDD_FTYPE(erts_alc_test) *erts_alc_test;
  /* Add new calls here */
} TWinDynDriverCallbacks;   

/* This header is included explicitly by the ddll static driver, it musn't define things then */ 
#ifndef STATIC_ERLANG_DRIVER

extern TWinDynDriverCallbacks WinDynDriverCallbacks;

#define null_func (WinDynDriverCallbacks.null_func)
#define driver_failure_atom (WinDynDriverCallbacks.driver_failure_atom)
#define driver_failure_posix (WinDynDriverCallbacks.driver_failure_posix)
#define driver_failure (WinDynDriverCallbacks.driver_failure)
#define driver_exit (WinDynDriverCallbacks.driver_exit)
#define driver_failure_eof (WinDynDriverCallbacks.driver_failure_eof)
#define driver_select (WinDynDriverCallbacks.driver_select)
#define driver_event (WinDynDriverCallbacks.driver_event)
#define driver_output (WinDynDriverCallbacks.driver_output)
#define driver_output2 (WinDynDriverCallbacks.driver_output2)
#define driver_output_binary (WinDynDriverCallbacks.driver_output_binary)
#define driver_outputv (WinDynDriverCallbacks.driver_outputv)
#define driver_vec_to_buf (WinDynDriverCallbacks.driver_vec_to_buf)
#define driver_set_timer (WinDynDriverCallbacks.driver_set_timer)
#define driver_cancel_timer (WinDynDriverCallbacks.driver_cancel_timer)
#define driver_read_timer (WinDynDriverCallbacks.driver_read_timer)
#define erl_errno_id (WinDynDriverCallbacks.erl_errno_id)
#define set_busy_port (WinDynDriverCallbacks.set_busy_port)
#define set_port_control_flags (WinDynDriverCallbacks.set_port_control_flags)
#define get_port_flags (WinDynDriverCallbacks.get_port_flags)
#define driver_alloc_binary (WinDynDriverCallbacks.driver_alloc_binary)
#define driver_realloc_binary (WinDynDriverCallbacks.driver_realloc_binary)
#define driver_free_binary (WinDynDriverCallbacks.driver_free_binary)
#define driver_alloc (WinDynDriverCallbacks.driver_alloc)
#define driver_realloc (WinDynDriverCallbacks.driver_realloc)
#define driver_free (WinDynDriverCallbacks.driver_free)
#define driver_enq (WinDynDriverCallbacks.driver_enq)
#define driver_pushq (WinDynDriverCallbacks.driver_pushq)
#define driver_deq (WinDynDriverCallbacks.driver_deq)
#define driver_sizeq (WinDynDriverCallbacks.driver_sizeq)
#define driver_enq_bin (WinDynDriverCallbacks.driver_enq_bin)
#define driver_pushq_bin (WinDynDriverCallbacks.driver_pushq_bin)
#define driver_peekqv (WinDynDriverCallbacks.driver_peekqv)
#define driver_peekq (WinDynDriverCallbacks.driver_peekq)
#define driver_enqv (WinDynDriverCallbacks.driver_enqv)
#define driver_pushqv (WinDynDriverCallbacks.driver_pushqv)
#define add_driver_entry (WinDynDriverCallbacks.add_driver_entry)
#define remove_driver_entry (WinDynDriverCallbacks.remove_driver_entry)
#define driver_mk_atom (WinDynDriverCallbacks.driver_mk_atom)
#define driver_mk_port (WinDynDriverCallbacks.driver_mk_port)
#define driver_connected (WinDynDriverCallbacks.driver_connected)
#define driver_caller (WinDynDriverCallbacks.driver_caller)
#define driver_mk_term_nil (WinDynDriverCallbacks.driver_mk_term_nil)
#define driver_output_term (WinDynDriverCallbacks.driver_output_term)
#define driver_send_term (WinDynDriverCallbacks.driver_send_term)
#define driver_async (WinDynDriverCallbacks.driver_async)
#define driver_async_cancel (WinDynDriverCallbacks.driver_async_cancel)
#define driver_attach (WinDynDriverCallbacks.driver_attach)
#define driver_detach (WinDynDriverCallbacks.driver_detach)
#define driver_dl_open (WinDynDriverCallbacks.driver_dl_open)
#define driver_dl_sym (WinDynDriverCallbacks.driver_dl_sym)
#define driver_dl_close (WinDynDriverCallbacks.driver_dl_close)
#define driver_dl_error (WinDynDriverCallbacks.driver_dl_error)
#define erts_alc_test (WinDynDriverCallbacks.erts_alc_test)

/* The only variable in the interface... */
#define driver_term_nil (driver_mk_term_nil())

#include <stdio.h>
#include <stdlib.h> 

#define DRIVER_INIT(DriverName)									\
ErlDrvEntry *erl_dyndriver_real_driver_init(void);									\
TWinDynDriverCallbacks WinDynDriverCallbacks;							\
__declspec(dllexport) ErlDrvEntry *driver_init(TWinDynDriverCallbacks *callbacks)	        \
{												\
    memcpy(&WinDynDriverCallbacks,callbacks,sizeof(TWinDynDriverCallbacks));			\
    return erl_dyndriver_real_driver_init();									\
}												\
ErlDrvEntry *erl_dyndriver_real_driver_init(void)

/* This is to make erl_driver.h avoid changing what's done here */
#define ERL_DRIVER_TYPES_ONLY

#else /* defined(STATIC_ERLANG_DRIVER) */
/* This is for the ddll driver */

#define ERL_INIT_CALLBACK_STRUCTURE(W)			\
do {				                        \
((W).null_func) = null_func;				\
((W).driver_failure_atom) = driver_failure_atom;	\
((W).driver_failure_posix) = driver_failure_posix;	\
((W).driver_failure) = driver_failure;			\
((W).driver_exit) = driver_exit;			\
((W).driver_failure_eof) = driver_failure_eof;		\
((W).driver_select) = driver_select;			\
((W).driver_event) = driver_event;			\
((W).driver_output) = driver_output;			\
((W).driver_output2) = driver_output2;			\
((W).driver_output_binary) = driver_output_binary;	\
((W).driver_outputv) = driver_outputv;			\
((W).driver_vec_to_buf) = driver_vec_to_buf;		\
((W).driver_set_timer) = driver_set_timer;		\
((W).driver_cancel_timer) = driver_cancel_timer;	\
((W).driver_read_timer) = driver_read_timer;		\
((W).erl_errno_id) = erl_errno_id;			\
((W).set_busy_port) = set_busy_port;			\
((W).set_port_control_flags) = set_port_control_flags;	\
((W).get_port_flags) = get_port_flags;			\
((W).driver_alloc_binary) = driver_alloc_binary;	\
((W).driver_realloc_binary) = driver_realloc_binary;	\
((W).driver_free_binary) = driver_free_binary;		\
((W).driver_alloc) = driver_alloc;			\
((W).driver_realloc) = driver_realloc;			\
((W).driver_free) = driver_free;			\
((W).driver_enq) = driver_enq;				\
((W).driver_pushq) = driver_pushq;			\
((W).driver_deq) = driver_deq;				\
((W).driver_sizeq) = driver_sizeq;			\
((W).driver_enq_bin) = driver_enq_bin;			\
((W).driver_pushq_bin) = driver_pushq_bin;		\
((W).driver_peekqv) = driver_peekqv;			\
((W).driver_peekq) = driver_peekq;			\
((W).driver_enqv) = driver_enqv;			\
((W).driver_pushqv) = driver_pushqv;			\
((W).add_driver_entry) = add_driver_entry;		\
((W).remove_driver_entry) = remove_driver_entry;	\
((W).driver_mk_atom) = driver_mk_atom;			\
((W).driver_mk_port) = driver_mk_port;			\
((W).driver_connected) = driver_connected;		\
((W).driver_caller) = driver_caller;			\
((W).driver_mk_term_nil) = driver_mk_term_nil;		\
((W).driver_output_term) = driver_output_term;		\
((W).driver_send_term) = driver_send_term;		\
((W).driver_async) = driver_async;			\
((W).driver_async_cancel) = driver_async_cancel;	\
((W).driver_attach) = driver_attach;			\
((W).driver_detach) = driver_detach;                    \
((W).driver_dl_open) =  driver_dl_open;			\
((W).driver_dl_sym) =  driver_dl_sym;			\
((W).driver_dl_close) =  driver_dl_close;		\
((W).driver_dl_error) =  driver_dl_error;		\
((W).erts_alc_test) = erts_alc_test;			\
} while (0)



#endif /* STATIC_ERLANG_DRIVER */
#endif /* _ERL_WIN_DYN_DRIVER_H */
