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
#include "erl_driver.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_api.h"

APIEXTERN 
void
ErlInit(void)
{
    ErtsSlAllocInit sla_init;
    /* Value < 0 == default will be used */
    sla_init.esla  = -1;
    sla_init.eosla = -1;
    sla_init.mcs   = -1;
    sla_init.sbct  = -1;
    sla_init.sbcmt = -1;
    sla_init.mmc   = -1;
    sla_init.cos   = -1;
    sla_init.scs   = -1;
    sla_init.lcs   = -1;
    sla_init.cgr   = -1;
    sla_init.mbsd  = -1;

    erts_sl_alloc_init(&sla_init);
    erts_init_definite_alloc(DEFAULT_DEFINITE_ALLOC_BLOCK_SIZE);
    erts_init_utils();
    /* Permanently disable use of mmap for sys_alloc (malloc). */
    sys_alloc_opt(SYS_ALLOC_OPT_MMAP_MAX, 0);
    sys_alloc_opt(SYS_ALLOC_OPT_TRIM_THRESHOLD, ERTS_DEFAULT_TRIM_THRESHOLD);
    sys_alloc_opt(SYS_ALLOC_OPT_TOP_PAD, ERTS_DEFAULT_TOP_PAD);

    erl_sys_init();
    erl_init();
}

APIEXTERN 
void
ErlOtpStart(int argc, char** argv)
{
    erl_start(argc, argv);
}


APIEXTERN 
int
ErlLoadModule(char* modname, void* code, unsigned size)
{
    Eterm module_name = am_atom_put(modname, sys_strlen(modname));
    return do_load(NIL, module_name, code, size);
}

APIEXTERN 
void
ErlCreateInitialProcess(char* modname, void* code, unsigned size,
			int argc, char** argv)
{
    erl_first_process(modname, code, size, argc, argv);
}

APIEXTERN 
void
ErlScheduleLoop(void)
{
    erl_sys_schedule_loop();
}

APIEXTERN
int
ErlGetConsoleKey(void)
{
    return sys_get_key(0);
}
