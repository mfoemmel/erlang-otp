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
** Registered processes
*/

#ifndef __REGPROC_H__
#define __REGPROC_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __HASH_H__
#include "hash.h"
#endif

#ifndef __PROCESS_H__
#include "erl_process.h"
#endif

struct port;

typedef struct reg_proc
{
    HashBucket bucket;  /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Process *p;         /* The process registerd (only one of this and
			   'pt' is non-NULL */
    struct port *pt;    /* The port registered */
    Eterm name;         /* Atom name */
} RegProc;

extern Hash process_reg;

void init_register_table(void);
void register_info(CIO to);
Process *register_process(Process *c_p, Eterm name, Process *p);
struct port *register_port(Eterm name, struct port *pt);
Process *whereis_process(Eterm name);
void whereis_name(Eterm name, Process **p, struct port **pt);
int unregister_name(Process *c_p, Eterm name);


#endif
