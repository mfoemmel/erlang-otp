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
 * Description:	Check I/O
 *
 * Author: 	Rickard Green
 */

#ifndef ERL_CHECK_IO_H__
#define ERL_CHECK_IO_H__

#ifdef ERTS_ENABLE_KERNEL_POLL

int driver_select_kp(ErlDrvPort, ErlDrvEvent, int, int);
int driver_select_nkp(ErlDrvPort, ErlDrvEvent, int, int);
int driver_event_kp(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
int driver_event_nkp(ErlDrvPort, ErlDrvEvent, ErlDrvEventData);
Uint erts_check_io_size_kp(void);
Uint erts_check_io_size_nkp(void);
Eterm erts_check_io_info_kp(void *);
Eterm erts_check_io_info_nkp(void *);
int erts_check_io_max_files_kp(void);
int erts_check_io_max_files_nkp(void);
void erts_check_io_kp(int);
void erts_check_io_nkp(int);
void erts_init_check_io_kp(void);
void erts_init_check_io_nkp(void);
int erts_check_io_debug_kp(void);
int erts_check_io_debug_nkp(void);

#ifdef ERTS_SMP
void erts_wake_io_thread_kp(void);
void erts_wake_io_thread_nkp(void);
#endif

#else /* !ERTS_ENABLE_KERNEL_POLL */

Uint erts_check_io_size(void);
Eterm erts_check_io_info(void *);
int erts_check_io_max_files(void);
void erts_check_io(int);
void erts_init_check_io(void);

#endif

#endif /*  ERL_CHECK_IO_H__ */

