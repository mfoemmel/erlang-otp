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
** Registerd processes
*/

#ifndef __REGPROC_H__
#define __REGPROC_H__

#ifndef __PROCESS_H__
#include "erl_process.h"
#endif

typedef struct reg_proc
{
    HashBucket bucket;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Process *p;         /* The process registerd */
    int name;           /* Atom index to name */
} RegProc;

extern Hash process_reg;

EXTERN_FUNCTION(void, init_register_table, (_VOID_));
EXTERN_FUNCTION(void, register_info, (CIO));
EXTERN_FUNCTION(Process*, register_process, (int, Process*));
EXTERN_FUNCTION(Process*, whereis_process, (int));
EXTERN_FUNCTION(Process*, unregister_process, (int));


#endif
