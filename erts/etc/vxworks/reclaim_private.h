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
#ifndef _RECLAIM_PRIVATE_H
#define _RECLAIM_PRIVATE_H
/*
 * Private header for the reclaim facility, also included in the emulator.
 */

#include "reclaim.h"

/* Typedefs for ANSI memory allocation function pointers */
typedef void *(*MallocFunction)(size_t);
typedef void *(*ReallocFunction)(void *, size_t);
typedef void *(*CallocFunction)(size_t, size_t);
typedef void (*FreeFunction)(void *);
typedef STATUS (*CfreeFunction)(char *);

/* Functions for internal use and use by the emulator */
extern int reclaim_max_files(void);
extern void set_reclaim_free_function(FreeFunction f);
extern void save_delete_hook(FUNCPTR func, caddr_t parm);
extern void *save_malloc2(size_t size, MallocFunction mf);
extern void *save_calloc2(size_t nelem, size_t elsize, CallocFunction cf);
extern void *save_realloc2(void *optr, size_t size, ReallocFunction rf);
extern void save_free2(void *ptr, FreeFunction ff);
extern void save_cfree2(void *ptr, CfreeFunction ff);

#endif /* _RECLAIM_PRIVATE_H */
