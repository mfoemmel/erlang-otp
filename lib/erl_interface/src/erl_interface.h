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
#ifndef _ERL_INTERFACE_H
#define _ERL_INTERFACE_H
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Purpose:  Erlang types in C
 */
#include "erl_eterm.h"
#include "erl_format.h"
#include "erl_marshal.h"
#include "erl_fix_alloc.h"
#include "erl_malloc.h"
#include "erl_rport.h"
#include "erl_connect.h"
#include "erl_start.h"
#include "erl_error.h"
#include "erl_resolve.h"
#include "erl_locking.h"
#include "erl_global.h"
#include "erl_epmd.h"

#ifdef __WIN32__
#define MAXPATHLEN 256
#define writesocket(sock,buf,nbyte) send(sock,buf,nbyte,0)
#define  readsocket(sock,buf,nbyte) recv(sock,buf,nbyte,0)
#else /* not __WIN32__ */
#define writesocket write
#define readsocket  read
#define closesocket close
#define ioctlsocket ioctl
#endif

#ifdef __cplusplus
}
#endif

#endif

