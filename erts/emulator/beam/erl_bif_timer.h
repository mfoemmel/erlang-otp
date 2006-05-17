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


#ifndef ERL_BIF_TIMER_H__
#define ERL_BIF_TIMER_H__

#include "sys.h"
#include "erl_message.h"

struct process;

typedef struct ErtsBifTimer_ ErtsBifTimer;

Uint erts_bif_timer_memory_size(void);
void erts_print_bif_timer_info(int to, void *to_arg);
void erts_cancel_bif_timers(struct process *p, Uint32 plocks);
void erts_bif_timer_init(void);
void erts_bif_timer_foreach(void (*func)(Eterm,Eterm,ErlHeapFragment *,void *),
			    void *arg);
#endif
