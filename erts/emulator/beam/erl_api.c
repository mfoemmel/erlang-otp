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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"

void
ErlInit(void)
{
    erl_sys_init();
    erl_init();
}

int
ErlLoadModule(char* modname, void* code, unsigned size)
{
    Eterm module_name = am_atom_put(modname, sys_strlen(modname));
    return do_load(0, module_name, code, size);
}

void
ErlCreateInitialProcess(char* modname, void* code, unsigned size,
			int argc, char** argv)
{
    erl_first_process(modname, code, size, argc, argv);
}

void
ErlScheduleLoop(void)
{
    erl_sys_schedule_loop();
}
