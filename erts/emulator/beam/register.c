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
** Manage Registered processes
*/
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"
#include "atom.h"
#include "register.h"

Hash process_reg;

#define PREG_HASH_SIZE 10

void register_info(CIO to) {
    hash_info(to, &process_reg);
}


static HashValue reg_hash(RegProc *obj) {
    return (HashValue) atom_val(obj->name);
}

static int reg_cmp(RegProc *tmpl, RegProc *obj) {
    return (tmpl->name == obj->name) ? 0 : 1;
}

static RegProc* reg_alloc(RegProc *tmpl) {
    RegProc* obj = (RegProc*) fix_alloc(preg_desc);
    
    obj->name = tmpl->name;
    obj->p = tmpl->p;
    obj->pt = tmpl->pt;
    return obj;
}

static void reg_free(RegProc *obj) {
    fix_free(preg_desc, (void*) obj);
}

void init_register_table(void)
{
    HashFunctions f;

    f.hash = (H_FUN) reg_hash;
    f.cmp  = (HCMP_FUN) reg_cmp;
    f.alloc = (HALLOC_FUN) reg_alloc;
    f.free = (HFREE_FUN) reg_free;

    hash_init(&process_reg, "process_reg", PREG_HASH_SIZE, f);
}

/*
** Register a port (cant be registerd twice)
** Returns 0 if port already registered
** Returns rp the portes registered (does not have to be p)
*/
Port* register_port(Eterm name, Port *pt) {
    RegProc r, *rp;

    if (pt->reg != (RegProc*) 0)
	return (Port*) 0;

    r.name = name;
    r.pt = pt;
    r.p = NULL;
    
    rp = (RegProc*) hash_put(&process_reg, (void*) &r);
    if (rp->pt == pt)
	pt->reg = rp;
    return rp->pt;
}

/*
** Register a process (cant be registered twice)
** Returns 0 if process already registered
** Returns rp the processes registered (does not have to be p)
*/
Process* register_process(Process *c_p, Eterm name, Process *p) {
    RegProc r, *rp;

    if (p->reg != NULL)
	return NULL;

    r.name = name;
    r.p = p;
    r.pt = NULL;
    
    rp = (RegProc*) hash_put(&process_reg, (void*) &r);
    if (rp->p == p) {
	if (IS_TRACED_FL(p, F_TRACE_PROCS)) {
	    trace_proc(c_p, p, am_register, name);
	}
	p->reg = rp;
    }
    return rp->p;
}

/*
** Find registered process (whereis)
*/
Process* whereis_process(Eterm name) {
    RegProc r, *rp;

    r.name = name;
    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL)
	return rp->p;
    return (Process*) 0;
}

void whereis_name(Eterm name, Process **p, Port **pt)
{
    RegProc r, *rp;

    r.name = name;
    *p = NULL;
    *pt = NULL;
    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL) {
	*p = rp->p;
	*pt = rp->pt;
    }
}

/*
** Unregister a name
** Return 0 if not registered
** Otherwise returns 1
*/
int unregister_name(Process *c_p, Eterm name) {
    RegProc r, *rp;
    
    r.name = name;
    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL) {
	Process* p = rp->p;
	Port* pt = rp->pt;
	if (pt != NULL) {
	    pt->reg = NULL;
	} else if (p != NULL) {
	    if (p->status == P_EXITING)
		p->reg_atom = name;
	    p->reg = NULL;
	    if (IS_TRACED_FL(p, F_TRACE_PROCS)) {
		trace_proc(c_p, p, am_unregister, name);
	    }
	}
	hash_erase(&process_reg, (void*) &r);
	return 1;
    }
    return 0;
}


