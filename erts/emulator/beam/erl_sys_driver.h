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
 * Include file for erlang driver writers.
 */

#ifndef __ERL_SYS_DRIVER_H__
#define __ERL_SYS_DRIVER_H__

#ifdef __ERL_DRIVER_H__
#error erl_sys_driver.h cannot be included after erl_driver.h
#endif

#define ERL_SYS_DRV

typedef long ErlDrvEvent; /* An event to be selected on. */
typedef long ErlDrvPort; /* A port descriptor. */

/* typedef struct _SysDriverOpts SysDriverOpts; defined in sys.h */

#include "erl_driver.h"

#endif




