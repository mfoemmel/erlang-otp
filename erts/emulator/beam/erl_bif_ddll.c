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
 * BIFs belonging to the 'erl_ddll' module together with utilityn functions
 * for dynameic loading. The actual loading is done in erl_sys_ddll.c in 
 * respective system dependent directory.
 * Th drivers are kept record of in two ways. The driver handle is kept 
 * and accessed in the actual driver structure, while a separate structure
 * keeps record of the processes accessing it

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

#ifdef ERTS_SMP
#define DDLL_SMP 1
#else
#define DDLL_SMP 0
#endif

/*
 * Local types
 */

typedef struct {
    Eterm pid;
    Process *proc;
    Uint status;
    Uint count;
} ProcEntryInfo;

/*
 * Forward
 */
static char *pick_list_or_atom(Eterm name_term);
static DE_List *lookup_driver(char *name);
static Eterm mkatom(char *str);
static void add_proc_loaded(DE_Handle *dh, Process *proc); 
static void set_driver_reloading(DE_Handle *dh, Process *proc, char *path, char *name, Uint flags);
static int load_driver_entry(DE_Handle **dhp, char *path, char *name);
static int do_unload_driver_entry(DE_Handle *dh, Eterm *save_name);
static int do_load_driver_entry(DE_Handle *dh, char *path, char *name);
static void unload_driver_entry(DE_Handle *dh);
static int reload_driver_entry(DE_Handle *dh);
static int build_proc_info(DE_Handle *dh, ProcEntryInfo **out_pei, Uint filter);
static DE_ProcEntry *find_proc_entry(DE_Handle *dh, Process *proc, Uint status);
static void remove_proc_entry(DE_Handle *dh, DE_ProcEntry *pe);
static int num_procs(DE_Handle *dh, Uint status);
/*static int num_entries(DE_Handle *dh, Process *proc, Uint status);*/
static void notify_proc(Process *proc, Eterm ref, Eterm driver_name, 
			Eterm type, Eterm tag, int errcode);
static void notify_all(DE_Handle *dh, char *name, Uint awaiting, Eterm type, Eterm tag);
static int load_error_need(int code);
static Eterm build_load_error_hp(Eterm *hp, int code);
static Eterm build_load_error(Process *p, int code);
static int errdesc_to_code(Eterm errdesc, int *code /* out */);
static Eterm add_monitor(Process *p, DE_Handle *dh, Uint status);
static Eterm notify_when_loaded(Process *p, Eterm name_term, char *name, Uint32 plocks);
static Eterm notify_when_unloaded(Process *p, Eterm name_term, char *name, Uint32 plocks);
/*
 * Try to load. If the driver is OK, add as LOADED.  If the driver is
 * UNLOAD, change to reload and add as LOADED, there should be no other
 * LOADED tagged pid's.  If the driver is RELOAD then add/increment as
 * LOADED (should be some LOADED pid).  If the driver is not present,
 * really load and add as LOADED {ok,loaded} {ok,pending_driver}
 * {error, permanent} {error,load_error()}
 */
BIF_RETTYPE erl_ddll_try_load_3(Process *p, Eterm path_term, 
				Eterm name_term, Eterm options)
{
    char *path = NULL;
    int path_len;
    char *name = NULL;
#if DDLL_SMP
    int have_proc_lock = 1;
#endif
    DE_Handle *dh;
    DE_List *de;
    int res;
    Eterm soft_error_term = NIL;
    Eterm ok_term = NIL;
    Eterm *hp;
    Eterm t;
    int monitor = 0;
    Eterm l;
    Uint flags = 0;

    for(l = options; is_list(l); l =  CDR(list_val(l))) {
	Eterm opt = CAR(list_val(l));
	Eterm *tp;
	if (is_not_tuple(opt)) {
	    goto error;
	}
	tp = tuple_val(opt);
	if (*tp != make_arityval(2) || is_not_atom(tp[1])) {
	    goto error;
	}
	switch (tp[1]) {
	case am_driver_options:
	    {
		Eterm ll;
		for(ll = tp[2]; is_list(ll); ll = CDR(list_val(ll))) {
		    Eterm dopt = CAR(list_val(ll));
		    if (dopt == am_kill_ports) {
			flags |= ERL_DE_FL_KILL_PORTS;
		    } else {
			goto error;
		    }
		}
		if (is_not_nil(ll)) {
		    goto error;
		}
	    }
	    break;
	case am_monitor:
	    if (tp[2] != am_pending_driver && tp[2] != am_pending ) { 
		goto error;
	    }
	    monitor = 1;
	    break;
	default:
	    goto error;
	}
    }
    if (is_not_nil(l)) {
	goto error;
    }


    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

    path_len = io_list_len(path_term);

    if (path_len <= 0) {
	goto error;
    }
    path = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, path_len + 1 /* might need path separator */ + sys_strlen(name) + 1);
    if (io_list_to_buf(path_term, path, path_len) != 0) {
	goto error;
    }
    while (path_len > 0 && (path[path_len-1] == '\\' || path[path_len-1] == '/')) {
	--path_len;
    }
    path[path_len++] = '/';
    /*path[path_len] = '\0';*/
    sys_strcpy(path+path_len,name);

#if DDLL_SMP
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    have_proc_lock = 0;
    erts_smp_io_lock();
#endif    

    if ((de = lookup_driver(name)) != NULL) {
	if (de->de_hndl == NULL) {
	    /* static_driver */
	    soft_error_term = am_linked_in_driver;
	    goto soft_error;
	} else {
	    dh = de->de_hndl;
	    if (dh->status == ERL_DE_OK) {
		/*already loaded and healthy (might be by me) */
		if (sys_strcmp(dh->full_path, path) || 
		    (dh->flags & ERL_FL_CONSISTENT_MASK) != (flags &  ERL_FL_CONSISTENT_MASK)) {
		    soft_error_term = am_inconsistent;
		    goto soft_error;
		}
		add_proc_loaded(dh,p);
		monitor = 0;
		ok_term = mkatom("already_loaded");
	    } else if (dh->status == ERL_DE_UNLOAD || dh->status == ERL_DE_FORCE_UNLOAD) {
		/* Unload requested and granted (no more processes) */
		set_driver_reloading(dh, p, path, name, flags);
		ok_term = am_pending_driver;
	    } else if (dh->status == ERL_DE_RELOAD) {
		if (sys_strcmp(dh->reload_full_path, path) || 
		    (dh->reload_flags & ERL_FL_CONSISTENT_MASK) != 
		        (flags &  ERL_FL_CONSISTENT_MASK)) {
		    soft_error_term = am_inconsistent;
		    goto soft_error;
		}
		/* Load of granted unload... */
		add_proc_loaded(dh,p);
		ok_term = am_pending_driver;
	    } else { /* ERL_DE_PERMANENT */
		soft_error_term = am_permanent;
		goto soft_error;
	    }
	}
    } else {
	if ((res = load_driver_entry(&dh, path, name)) !=  ERL_DE_NO_ERROR) {
#if DDLL_SMP
	    /* Need process lock for build_load_error */
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	    have_proc_lock = 1;
#endif
	    soft_error_term = build_load_error(p, res);
	    goto soft_error;
	} else {
	    dh->flags = flags;
	    add_proc_loaded(dh,p);
	    monitor = 0;
	    ok_term = mkatom("loaded");
	}
    }
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
#if DDLL_SMP
    if (!have_proc_lock) {
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif
    p->flags |= F_USING_DDLL;
    if (monitor) {
	Eterm mref = add_monitor(p, dh, ERL_DE_PROC_AWAIT_LOAD);
	hp = HAlloc(p,4);
	t = TUPLE3(hp, am_ok, ok_term, mref);
    } else {
	hp = HAlloc(p,3);
	t = TUPLE2(hp, am_ok, ok_term);
    }
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    BIF_RET(t);
 soft_error:
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
#if DDLL_SMP
    erts_smp_io_unlock();
    if (!have_proc_lock) {
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
#endif
    hp = HAlloc(p,3);
    t = TUPLE2(hp, am_error, soft_error_term);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    BIF_RET(t);
 error:
    ERTS_SMP_LC_ASSERT(!erts_smp_lc_io_is_locked());
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    if (path != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) path);
    }
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    BIF_ERROR(p,BADARG);
}


/* 
   You have to have loaded the driver and the pid state 
   is LOADED or AWAIT_LOAD. You will be removed from the list
   regardless of driver state.
   If the driver is loaded by someone else to, return is
   {ok, pending_process}
   If the driver is loaded but locked by a port, return is
   {ok, pending_driver}
   If the driver is loaded and free to unload (you're the last holding it)
   {ok, unloaded}
   If it's not loaded or not loaded by you
   {error, not_loaded} or {error, not_loaded_by_you}

   Internally, if its in state UNLOADING, just return {ok, pending_driver} and
   remove/decrement this pid (which should be an LOADED tagged one).
   If the state is RELOADING, this pid should be in list as LOADED tagged, 
   only AWAIT_LOAD would be possible but not allowed for unloading, remove it 
   and, if the last LOADED tagged, change from RELOAD to UNLOAD and notify
   any AWAIT_LOAD-waiters with {'DOWN', ref(), driver, name(), load_cancelled}
   If the driver made itself permanent, {'UP', ref(), driver, name(), permanent}
*/
Eterm erl_ddll_try_unload_2(Process *p, Eterm name_term, Eterm options)
{
    char *name = NULL;
    Eterm ok_term = NIL;
    Eterm soft_error_term = NIL;
    DE_List *de;
    DE_Handle *dh;
    DE_ProcEntry *pe;
    Eterm *hp;
    Eterm t;
    int monitor = 0;
    Eterm l;
    int kill_ports = 0;

    for(l = options; is_list(l); l =  CDR(list_val(l))) {
	Eterm opt = CAR(list_val(l));
	Eterm *tp;
	if (is_not_tuple(opt)) {
	    if (opt == am_kill_ports) {
		kill_ports = 1;
		continue;
	    } else {
		goto error;
	    }
	}
	tp = tuple_val(opt);
	if (*tp != make_arityval(2) || tp[1] != am_monitor) {
	    goto error;
	}
	if (tp[2] == am_pending_driver) { 
	    monitor = 1;
	} else if (tp[2] == am_pending) {
	    monitor = 2;
	} else {
	    goto error;
	}
    }
    if (is_not_nil(l)) {
	goto error;
    }

    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

#if DDLL_SMP
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
    erts_smp_io_lock();
#endif    

    if ((de = lookup_driver(name)) == NULL) {
	soft_error_term = am_not_loaded;
	goto soft_error;
    }

    if (de->de_hndl == NULL) {
	soft_error_term = am_linked_in_driver;
	goto soft_error;
    } else if (de->de_hndl->status == ERL_DE_PERMANENT) {
	soft_error_term = am_permanent;
	goto soft_error;
    }	
    dh = de->de_hndl;
    if (dh->flags & ERL_DE_FL_KILL_PORTS) {
	kill_ports = 1;
    }
    if ((pe = find_proc_entry(dh, p, ERL_DE_PROC_LOADED)) == NULL) {
	if (num_procs(dh, ERL_DE_PROC_LOADED) > 0) {
	    soft_error_term = am_not_loaded_by_this_process;
	    goto soft_error;
	}
    } else {
	remove_proc_entry(dh, pe);
	erts_free(ERTS_ALC_T_DDLL_PROCESS, pe);
    }
    if (num_procs(dh, ERL_DE_PROC_LOADED) > 0) {
	ok_term = mkatom("pending_process");
	--monitor;
	goto done;
    }
    if (dh->port_count > 0) {
	if (dh->status == ERL_DE_RELOAD) {
	    notify_all(dh, de->drv->driver_name, 
		       ERL_DE_PROC_AWAIT_LOAD, am_DOWN, am_load_cancelled);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_full_path);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_driver_name);
	    dh->reload_full_path = dh->reload_driver_name = NULL; 
	    dh->reload_flags = 0;
	} 
	++kill_ports;
	dh->status = ERL_DE_UNLOAD;
	ok_term = am_pending_driver;
	goto done;
    }
    /* Really unload the driver... */
    ASSERT((dh->status == ERL_DE_OK));
    /* Might be to myself, but no problem since I will take the process lock then */
    notify_all(dh, de->drv->driver_name, ERL_DE_PROC_AWAIT_UNLOAD, am_DOWN, am_unloaded);
    ASSERT((dh->procs == NULL));
    ASSERT((dh->reload_full_path == NULL));
    ASSERT((dh->reload_driver_name == NULL));
    unload_driver_entry(dh);
    monitor = 0;
    ok_term = mkatom("unloaded");
done:
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
#if DDLL_SMP
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
#endif
    p->flags |= F_USING_DDLL;
    if (monitor > 0) {
	Eterm mref = add_monitor(p, dh, ERL_DE_PROC_AWAIT_UNLOAD);
	hp = HAlloc(p,4);
	t = TUPLE3(hp, am_ok, ok_term, mref);
    } else {
	hp = HAlloc(p,3);
	t = TUPLE2(hp, am_ok, ok_term);
    }
    if (kill_ports > 1) {
	int j;
	int c = dh->port_count;
#if DDLL_SMP
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
#endif
	dh->status = ERL_DE_FORCE_UNLOAD;
	for (j = 0; j < erts_max_ports && c > 0; j++) {
	    if (erts_port[j].status != FREE &&
		erts_port[j].drv_ptr->handle == dh) {
		/* Keep track of ports, driver might be recursively closed */
		--c;
		driver_failure_atom(j, "driver_unloaded");
	    }
	}
#if DDLL_SMP
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
#endif
    } 
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    if (kill_ports > 1) {
	ERTS_BIF_CHK_EXITED(p); /* May be exited by port killing */
    }
    BIF_RET(t);
 
soft_error:
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
#if DDLL_SMP
    erts_smp_io_unlock();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    hp = HAlloc(p,3);
    t = TUPLE2(hp, am_error, soft_error_term);
    BIF_RET(t);
 
 error: /* No lock fiddling before going here */
    ERTS_SMP_LC_ASSERT(!erts_smp_lc_io_is_locked());
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    BIF_ERROR(p,BADARG);
}


/* 
 * A shadow of the "real" demonitor BIF
 */
BIF_RETTYPE erl_ddll_demonitor_1(Process *p, Eterm ref)
{
   if (is_not_internal_ref(ref)) {
       BIF_ERROR(p, BADARG);
   }
   if (p->flags & F_USING_DDLL) {
       erts_ddll_remove_monitor(p, ref, ERTS_PROC_LOCK_MAIN);
   }
   BIF_RET(am_true);
}

/* 
 * A shadow of the "real" monitor BIF
 */
BIF_RETTYPE erl_ddll_monitor_2(Process *p, Eterm dr, Eterm what)
{
    if (dr != am_driver) {
	BIF_ERROR(p,BADARG);
    }
    return erts_ddll_monitor_driver(p, what, ERTS_PROC_LOCK_MAIN);
}

/* 
 * Return list of loaded drivers {ok,[string()]} 
 */
Eterm erl_ddll_loaded_drivers_0(Process *p)
{
    Eterm *hp;
    int need = 3;
    Eterm res = NIL;
    DE_List *de;
#if DDLL_SMP
    if (erts_smp_io_trylock() == EBUSY) {
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_io_lock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }	
#endif
    for (de = driver_list; de != NULL; de = de->next) {
	need += sys_strlen(de->drv->driver_name)*2+2;
    }
    hp = HAlloc(p,need);
    for (de = driver_list; de != NULL; de = de->next) {
	Eterm l;
	l = buf_to_intlist(&hp, de->drv->driver_name, sys_strlen(de->drv->driver_name), NIL);
	res = CONS(hp,l,res);
	hp += 2;
    }
    res = TUPLE2(hp,am_ok,res);
    /* hp += 3 */
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    BIF_RET(res);
}

/* 
 * More detailed info about loaded drivers: 
 * item is processes, driver_options, port_count, linked_in_driver, 
 * permanent, awaiting_load, awaiting_unload 
 */
Eterm erl_ddll_info_2(Process *p, Eterm name_term, Eterm item) 
{
    char *name = NULL;
    Eterm res = NIL;
    DE_List *de;
    ProcEntryInfo *pei = NULL;
    int num_pei;
    Eterm *hp;
    int i;
    Uint filter;
#if DDLL_SMP
    int have_io_lock = 0;
#endif

    if ((name = pick_list_or_atom(name_term)) == NULL) {
	goto error;
    }

    if (!is_atom(item)) {
	goto error;
    }

#if DDLL_SMP 
    if (erts_smp_io_trylock() == EBUSY) {
	/* Unlock process locks, and acquire locks in lock order... */
	erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	erts_smp_io_lock();
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    }
    have_io_lock = 1;
#endif
    if ((de = lookup_driver(name)) == NULL) {
	goto error;
    }
    
    switch (item) {
    case am_processes:
	filter = ERL_DE_PROC_LOADED;
	break;
    case am_driver_options:
	if (de->de_hndl == NULL) {
	    res = am_linked_in_driver;
	} else {
	    Uint start_flags = de->de_hndl->flags & ERL_FL_CONSISTENT_MASK;
	    /* Cheating, only one flag for now... */
	    if (start_flags & ERL_DE_FL_KILL_PORTS) {
		Eterm *myhp;
		myhp = HAlloc(p,2);
		res = CONS(myhp,am_kill_ports,NIL);
	    } else {
		res = NIL;
	    }
	}
	goto done;
    case am_port_count:
	if (de->de_hndl == NULL) {
	    res = am_linked_in_driver;
	} else if (de->de_hndl->status == ERL_DE_PERMANENT) {
	    res = am_permanent;
	} else {
	    res = make_small(de->de_hndl->port_count);
	}
	goto done;
    case am_linked_in_driver:
	if (de->de_hndl == NULL){
	    res = am_true;
	} else {
	    res = am_false;
	}
	goto done;
    case am_permanent:
	if (de->de_hndl != NULL && de->de_hndl->status == ERL_DE_PERMANENT) {
	    res = am_true;
	} else {
	    res = am_false;
	}
	goto done;
    case am_awaiting_load:
	filter = ERL_DE_PROC_AWAIT_LOAD;
	break;
    case am_awaiting_unload:
	filter = ERL_DE_PROC_AWAIT_UNLOAD;
	break;
    default:
	goto error;
    }

    if (de->de_hndl == NULL) {
	res = am_linked_in_driver;
	goto done;
    } else if (de->de_hndl->status == ERL_DE_PERMANENT) {
	res = am_permanent;
	goto done;
    }
    num_pei = build_proc_info(de->de_hndl, &pei, filter);
    if (!num_pei) {
	goto done;
    }
    hp = HAlloc(p,num_pei * (2+3));
    for (i = 0; i < num_pei; ++ i) {
	Eterm tpl = TUPLE2(hp,pei[i].pid,make_small(pei[i].count));
	hp += 3;
	res = CONS(hp,tpl,res);
	hp += 2;
    }    
 done:    
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    BIF_RET(res);
 error:
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
#if DDLL_SMP
    if (have_io_lock) {
	erts_smp_io_unlock();
    }
#endif
    BIF_ERROR(p,BADARG);
}

/*
 * Backend for erl_ddll:format_error, handles all "soft" errors returned by builtins,
 * possibly by calling the system specific error handler
 */
Eterm erl_ddll_format_error_int_1(Process *p, Eterm code_term)
{
    char *errstring = NULL;
    int errint;
    int len;
    Eterm ret = NIL;
    Eterm *hp;

    /* These errors can only appear in the erlang interface, not in the interface provided
       to drivers... */
    switch (code_term) {
    case am_inconsistent:
	errstring = "Driver name and/or driver options are inconsistent with "
	    "currently loaded driver";
	break;
    case am_linked_in_driver:
	errstring = "Driver is statically linked and "
	    "cannot be loaded/unloaded";
	break;
    case am_permanent:
	errstring = "DDLL driver is permanent an can not be unloaded/loaded";
	break;
    case am_not_loaded:
	errstring = "DDLL driver is not loaded";
	break;
    case am_not_loaded_by_this_process:
	errstring = "DDLL driver was not loaded by this process";
	break;
    case am_not_pending:
	errstring = "DDLL load not pending for this driver name";
	break;
    case am_already_loaded:
	errstring = "DDLL driver is already loaded successfully";
	break;
    case am_unloading:
	errstring = "Driver is unloading";
	break;
    default:
	/* A "real" error, we translate the atom to a code and translate the code 
	   to a string in the same manner as in the interface provided to drivers... */
	if (errdesc_to_code(code_term,&errint) != 0) {
	    goto error;
	}
#if DDLL_SMP 
	if (erts_smp_io_trylock() == EBUSY) {
	    /* Unlock process locks, and acquire locks in lock order... */
	    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
	    erts_smp_io_lock();
	    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
	}
#endif
	errstring = erts_ddll_error(errint);
#if DDLL_SMP
	erts_smp_io_unlock();
#endif
	break;
    }
    if (errstring == NULL) {
	goto error;
    }
    len = sys_strlen(errstring);
    hp = HAlloc(p, 2 * len);
    ret = buf_to_intlist(&hp, errstring, len, NIL);
    BIF_RET(ret);
 error:
    BIF_ERROR(p,BADARG);
} 

void erts_ddll_init(void)
{
    erl_sys_ddll_init();
}

/* Return value as a bif, called by erlang:monitor */
Eterm erts_ddll_monitor_driver(Process *p, Eterm description, Uint32 plocks) 
{
    Eterm *tp;
    Eterm ret;
    char *name;

    if (is_not_tuple(description)) {
	BIF_ERROR(p,BADARG);
    }
    tp = tuple_val(description);
    if (*tp != make_arityval(2)) {
	BIF_ERROR(p,BADARG);
    }
    if ((name = pick_list_or_atom(tp[1])) == NULL) {
	BIF_ERROR(p,BADARG);
    }
    switch (tp[2]) {
    case am_loaded:
	ERTS_BIF_PREP_RET(ret, notify_when_loaded(p,tp[1],name,plocks));
	break;
    case am_unloaded:
	ERTS_BIF_PREP_RET(ret, notify_when_unloaded(p,tp[1],name,plocks));
	break;
    default:
	ERTS_BIF_PREP_ERROR(ret,p,BADARG);
	break;
    }

    erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    return ret;
}

void erts_ddll_remove_monitor(Process *p, Eterm ref, Uint32 plocks)
{ 
    DE_List *de;
    erts_smp_proc_unlock(p, plocks);
    erts_smp_io_lock();
    de = driver_list;
    while (de != NULL) {
	if (de->de_hndl != NULL && de->de_hndl->status != ERL_DE_PERMANENT) {
	    DE_ProcEntry **pe = &(de->de_hndl->procs);
	    while ((*pe) != NULL) {
		if ((*pe)->proc == p && 
		    ((*pe)->awaiting_status == ERL_DE_PROC_AWAIT_LOAD ||
		     (*pe)->awaiting_status == ERL_DE_PROC_AWAIT_UNLOAD) &&
		    eq(make_internal_ref(&((*pe)->heap)),ref)) {
		    DE_ProcEntry *r = *pe;
		    *pe = r->next;
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) r);
		    goto done;
		} 
		pe = &((*pe)->next);
	    }
	}
	de = de->next;
    }
 done:
    erts_smp_io_unlock();
    erts_smp_proc_lock(p, plocks);
}

/* 
 * Called from erl_process.c, Need to take the io_lock.
 */
void erts_ddll_proc_dead(Process *p, Uint32 plocks) 
{
    DE_List **de;
    erts_smp_proc_unlock(p, plocks);
    erts_smp_io_lock();
    de = &driver_list;
    while (*de != NULL) {
	if ((*de)->de_hndl != NULL && (*de)->de_hndl->status != ERL_DE_PERMANENT) {
	    DE_ProcEntry **pe = &((*de)->de_hndl->procs);
	    int kill_ports = ((*de)->de_hndl->flags & ERL_DE_FL_KILL_PORTS);
	    int left = 0;
	    while ((*pe) != NULL) {
		if ((*pe)->proc == p) {
		    DE_ProcEntry *r = *pe;
		    *pe = r->next;
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) r);
		} else {
		    if ((*pe)->awaiting_status == ERL_DE_PROC_LOADED) {
			++left;
		    }
		    pe = &((*pe)->next);
		}
	    }
	    /* Do we have anyone holding the driver? */
	    if (!left && ((*de)->de_hndl->port_count == 0)) {
		/* Remove driver! */
		DE_List *q;
		DE_Handle *dh = (*de)->de_hndl;
		if (dh->reload_full_path != NULL) {
		    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_full_path);
		}
		if (dh->reload_driver_name != NULL) {
		    erts_free(ERTS_ALC_T_DDLL_HANDLE,dh->reload_driver_name);
		}
		dh->reload_full_path = dh->reload_driver_name = NULL; 		
		dh->reload_flags = 0;
		dh->status = ERL_DE_UNLOAD;
		notify_all(dh, (*de)->drv->driver_name, 
			   ERL_DE_PROC_AWAIT_LOAD, am_DOWN, am_load_cancelled);
		notify_all(dh, (*de)->drv->driver_name, 
			   ERL_DE_PROC_AWAIT_UNLOAD, am_DOWN, am_unloaded);
		ASSERT(dh->procs == NULL);
		q = *de;
		*de = (*de)->next;    
		if (q->drv->finish) {
		    (*(q->drv->finish))();
		}
		erts_sys_ddll_close(dh->handle);
		erts_free(ERTS_ALC_T_DRV_ENTRY_LIST, (void *) q);
		ASSERT(dh->full_path != NULL);
		erts_free(ERTS_ALC_T_DDLL_HANDLE, dh->full_path);
		erts_free(ERTS_ALC_T_DDLL_HANDLE, dh);
	    } else {
		if (!left && ((*de)->de_hndl->port_count > 0)) {
		    if (kill_ports) {
			int j;
			int c = (*de)->de_hndl->port_count;
			(*de)->de_hndl->status = ERL_DE_FORCE_UNLOAD;
			for (j = 0; j < erts_max_ports && c > 0; j++) {
			    if (erts_port[j].status != FREE &&
				erts_port[j].drv_ptr->handle == (*de)->de_hndl) {
				/* This might close the driver and hence (*de)
				   is reset, that's why we need to keep track 
				   of the port count */
				--c;
				driver_failure_atom(j, "driver_unloaded");
			    }
			}
		    } else {
			(*de)->de_hndl->status = ERL_DE_UNLOAD;
		    }
		}
		de = &((*de)->next);
	    }
	} else {
	    de = &((*de)->next);
	}
    }
    erts_smp_io_unlock();
    erts_smp_proc_lock(p, plocks);
}
void erts_ddll_lock_driver(DE_Handle *dh, char *name)
{
    DE_ProcEntry *p,*q;
    notify_all(dh, name, 
	       ERL_DE_PROC_AWAIT_LOAD, am_UP, am_permanent);
    notify_all(dh, name, 
	       ERL_DE_PROC_AWAIT_UNLOAD, am_UP, am_permanent);
    
    p = dh->procs; 
    while(p != NULL) {
	q = p;
	p = p->next;
	erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
    }
    dh->status = ERL_DE_PERMANENT;
}

/* The pid list -
   ERL_DE_PROC_AWAIT_UNLOAD -> Awaiting actual unloading, can be anyone 
   doing that
   ERL_DE_PROC_LOADED -> Has requested load and got pending,driver answer
   ERL_DE_AWAIT_LOAD -> Has managed to request loading notification, which 
   must have happened when having a ERL_DE_PROC_LOADED entry, but that might 
   have been removed */

void erts_ddll_increment_port_count(DE_Handle *dh)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    dh->port_count++;
}

int erts_ddll_driver_ok(DE_Handle *dh) {
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    return ((dh == NULL) || dh->status != ERL_DE_FORCE_UNLOAD);
}

void erts_ddll_decrement_port_count(DE_Handle *dh)
{
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    dh->port_count--;
    if (dh->port_count == 0 && dh->status != ERL_DE_OK) {
	DE_ProcEntry **p = &(dh->procs);
	Eterm save_driver_name;
	do_unload_driver_entry(dh,&save_driver_name);
	while (*p != NULL) {
	    DE_ProcEntry *q;
	    if ((*p)->awaiting_status == ERL_DE_PROC_AWAIT_UNLOAD) {
		notify_proc((*p)->proc, 
			    make_internal_ref(&((*p)->heap)),
			    save_driver_name,am_DOWN,am_unloaded, 0);
		q = *p;
		*p = q->next;
		erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
	    } else {
		ASSERT(dh->status == ERL_DE_RELOAD);
		p = &((*p)->next);
	    }
	}

	if (dh->status == ERL_DE_UNLOAD || dh->status == ERL_DE_FORCE_UNLOAD) {
	    ASSERT(dh->full_path != NULL);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
	    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	} else { /* ERL_DE_RELOAD */
	    int reload_res =
		reload_driver_entry(dh);
	    p = &(dh->procs);
	    while (*p != NULL) {
		DE_ProcEntry *q;
		if ((*p)->awaiting_status == ERL_DE_PROC_AWAIT_LOAD) {
		    if (reload_res == 0) {
			notify_proc((*p)->proc, 
				    make_internal_ref(&((*p)->heap)),
				    save_driver_name, am_UP, am_loaded, 0);
		    } else {
			notify_proc((*p)->proc, 
				    make_internal_ref(&((*p)->heap)),
				    save_driver_name, am_DOWN, am_load_failure, reload_res);
		    }
		    q = *p;
		    *p = q->next;
		    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
		} else {
		    if (reload_res != 0) {
			DE_ProcEntry *q = *p;
			*p = q->next;
			erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) q);
		    } else {
			p = &((*p)->next);
		    }
		}
	    }
	    if (reload_res != 0) {
		ASSERT(dh->full_path == NULL);
		erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	    }
	}
    }	    
}    


char *erts_ddll_error(int code) {
    switch (code) {
    case ERL_DE_NO_ERROR:
	return "No error";
    case ERL_DE_LOAD_ERROR_NO_INIT:
	return "No driver init in dynamic library";
    case  ERL_DE_LOAD_ERROR_FAILED_INIT:
	return "Driver init failed";
    case ERL_DE_LOAD_ERROR_BAD_NAME:
	return "Bad driver name";
    case ERL_DE_LOAD_ERROR_NAME_TO_LONG:
	return "Driver name to long";
    case ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY:
	return "DDLL functionality not available on this platform";
    case ERL_DE_ERROR_UNSPECIFIED:
	return "Unspecified dynamic library error";
    case ERL_DE_LOOKUP_ERROR_NOT_FOUND:
	return "Symbol not found in dynamic library";
    default:
	return erts_sys_ddll_error(code);
    }
}

/*
 * Utilities
 */
static Eterm notify_when_loaded(Process *p, Eterm name_term, char *name, Uint32 plocks)
{ 
    Eterm r = NIL;
    Eterm immediate_tag = NIL;
    Eterm immediate_type = NIL;
    DE_List *de;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & plocks);
#if DDLL_SMP 
    if (erts_smp_io_trylock() == EBUSY) {
	/* Unlock process locks, and acquire locks in lock order... */
	erts_smp_proc_unlock(p, plocks);
	erts_smp_io_lock();
	erts_smp_proc_lock(p, plocks);
    }
#endif
    if ((de = lookup_driver(name)) == NULL) {
	immediate_tag = am_unloaded;
	immediate_type = am_DOWN;
	goto immediate;
    }
    if (de->de_hndl == NULL || de->de_hndl->status == ERL_DE_PERMANENT) {
	immediate_tag = am_permanent;
	immediate_type = am_UP;
	goto immediate;
    }	

    switch (de->de_hndl->status) {
    case ERL_DE_OK:
	immediate_tag = am_loaded;
	immediate_type = am_UP;
	goto immediate;
    case ERL_DE_UNLOAD:
    case ERL_DE_FORCE_UNLOAD:
	immediate_tag = am_load_cancelled;
	immediate_type = am_DOWN;
	goto immediate;
    case ERL_DE_RELOAD:
	break;
    default:
	erl_exit(1,"Internal error, unknown state %u in dynamic driver.", de->de_hndl->status);
    }
    p->flags |= F_USING_DDLL;
    r = add_monitor(p, de->de_hndl, ERL_DE_PROC_AWAIT_LOAD);
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    BIF_RET(r);
 immediate:
    r =  erts_make_ref(p);
#if DDLL_SMP 
    erts_smp_proc_unlock(p, plocks);
#endif
    notify_proc(p, r, name_term, immediate_type, immediate_tag, 0);
#if DDLL_SMP
    erts_smp_io_unlock();
    erts_smp_proc_lock(p, plocks);
#endif
    BIF_RET(r);
}

static Eterm notify_when_unloaded(Process *p, Eterm name_term, char *name, Uint32 plocks)
{ 
    Eterm r = NIL;
    Eterm immediate_tag = NIL;
    Eterm immediate_type = NIL;
    DE_List *de;

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & plocks);
#if DDLL_SMP 
    if (erts_smp_io_trylock() == EBUSY) {
	/* Unlock process locks, and acquire locks in lock order... */
	erts_smp_proc_unlock(p, plocks);
	erts_smp_io_lock();
	erts_smp_proc_lock(p, plocks);
    }
#endif
    if ((de = lookup_driver(name)) == NULL) {
	immediate_tag = am_unloaded;
	immediate_type = am_DOWN;
	goto immediate;
    }
    if (de->de_hndl == NULL || de->de_hndl->status == ERL_DE_PERMANENT) {
	immediate_tag = am_permanent;
	immediate_type = am_UP;
	goto immediate;
    }	

    p->flags |= F_USING_DDLL;
    r = add_monitor(p, de->de_hndl, ERL_DE_PROC_AWAIT_UNLOAD);
#if DDLL_SMP
    erts_smp_io_unlock();
#endif
    BIF_RET(r);
 immediate:
    r =  erts_make_ref(p);
#if DDLL_SMP 
    erts_smp_proc_unlock(p, plocks);
#endif
    notify_proc(p, r, name_term, immediate_type, immediate_tag, 0);
#if DDLL_SMP
    erts_smp_io_unlock();
    erts_smp_proc_lock(p, plocks);
#endif
    BIF_RET(r);
}

static DE_ProcEntry *find_proc_entry(DE_Handle *dh, Process *proc, Uint status)
{
    DE_ProcEntry *p = dh->procs;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    while (p != NULL) {
	if (p->proc == proc && p->awaiting_status == status) {
	    return p;
	}
	p = p->next;
    }
    return NULL;
}

static void remove_proc_entry(DE_Handle *dh, DE_ProcEntry *pe)
{
    DE_ProcEntry **p = &(dh->procs);

    while (*p != NULL && *p != pe) {
	p = &((*p)->next);
    }
    if ((*p) != NULL) {
	*p = (*p)->next;
    }
}

static int num_procs(DE_Handle *dh, Uint status) {
    DE_ProcEntry *p = dh->procs;
    int i = 0;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    while (p != NULL) {
	if (p->awaiting_status == status) {
	    ++i;
	}
	p = p->next;
    }
    return i;
}
/*
static int num_entries(DE_Handle *dh, Process *proc, Uint status) {    
    DE_ProcEntry *p = dh->procs;
    int i = 0;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    while (p != NULL) {
	if (p->awaiting_status == status && p->proc == proc) {
	    ++i;
	}
	p = p->next;
    }
    return i;
}
*/
static void add_proc_loaded(DE_Handle *dh, Process *proc) 
{
    DE_ProcEntry *p;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->awaiting_status = ERL_DE_PROC_LOADED;
    p->next = dh->procs;
    dh->procs = p;
}

static Eterm copy_ref(Eterm ref, Eterm *hp)
{
    RefThing *ptr = ref_thing_ptr(ref);
    memcpy(hp, ptr, sizeof(RefThing));
    return (make_internal_ref(hp));
}

static void add_proc_waiting(DE_Handle *dh, Process *proc, 
			     Uint status, Eterm ref) 
{
    DE_ProcEntry *p;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->awaiting_status = status;
    copy_ref(ref, p->heap);
    p->next = dh->procs;
    dh->procs = p;
}

static Eterm add_monitor(Process *p, DE_Handle *dh, Uint status)
{
    Eterm r;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    r = erts_make_ref(p);
    add_proc_waiting(dh, p, status, r);
    return r;
}
    

static void set_driver_reloading(DE_Handle *dh, Process *proc, char *path, char *name, Uint flags)
{
    DE_ProcEntry *p;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    p = erts_alloc(ERTS_ALC_T_DDLL_PROCESS, sizeof(DE_ProcEntry));
    p->proc = proc;
    p->awaiting_status = ERL_DE_OK;
    p->next = dh->procs;
    dh->procs = p;
    dh->status = ERL_DE_RELOAD;
    dh->reload_full_path = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(path) + 1);
    strcpy(dh->reload_full_path,path);
    dh->reload_driver_name = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(name) + 1);
    strcpy(dh->reload_driver_name,name);
    dh->reload_flags = flags;
}

static int do_load_driver_entry(DE_Handle *dh, char *path, char *name)
{
    void *init_handle;
    int res;
    ErlDrvEntry *dp;
 
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    if ((res =  erts_sys_ddll_open(path, &(dh->handle))) != ERL_DE_NO_ERROR) {
	return res;
    }
    
    if ((res = erts_sys_ddll_load_driver_init(dh->handle, 
					      &init_handle)) != ERL_DE_NO_ERROR) {
	erts_sys_ddll_close(dh->handle);
	return ERL_DE_LOAD_ERROR_NO_INIT;
    }
    
    dp = erts_sys_ddll_call_init(init_handle);
    if (dp == NULL) {
	erts_sys_ddll_close(dh->handle);
	return ERL_DE_LOAD_ERROR_FAILED_INIT;
    }
    if (strcmp(name, dp->driver_name) != 0) {
	erts_sys_ddll_close(dh->handle);
	return ERL_DE_LOAD_ERROR_BAD_NAME;
    }
    dh->port_count = 0;
    dh->full_path = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sys_strlen(path) + 1);
    sys_strcpy(dh->full_path, path);
    dh->flags = 0;
    dh->status = ERL_DE_OK;
    dp->handle = dh;

    add_driver_entry(dp); /* io.c */

    return ERL_DE_NO_ERROR;
}

static int do_unload_driver_entry(DE_Handle *dh, Eterm *save_name)
{
    DE_List **p = &driver_list;
    DE_List *q;
    int i;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    /* XXX:PaN only in DEBUG code? */
    for (i = 0; i < erts_max_ports; i++) {
	if (erts_port[i].status != FREE &&
	    !(erts_port[i].status & EXITING) &&
	    erts_port[i].drv_ptr->handle == dh) {
	    erl_exit(1,"Internal error: Port %d holding dynamic "
		     "driver without reference count",i);
	}
    }

    while (*p != NULL) {
	if ((*p)->de_hndl == dh) {
		
	    q = *p;
	    *p = (*p)->next;
	    if (save_name != NULL) {
		*save_name = mkatom(q->drv->driver_name);
	    } 
	    /* XXX:PaN Future locking problems? */
	    if (q->drv->finish) {
		(*(q->drv->finish))();
	    }
	    erts_sys_ddll_close(dh->handle);
	    erts_free(ERTS_ALC_T_DRV_ENTRY_LIST, (void *) q);
	    return 1;
	}
	p = &(*p)->next;
    }
    return 0;
}

static int load_driver_entry(DE_Handle **dhp, char *path, char *name)
{
    int res;
    DE_Handle *dh = erts_alloc(ERTS_ALC_T_DDLL_HANDLE, sizeof(DE_Handle));
    dh->handle = NULL;
    dh->procs = NULL;
    dh->port_count = 0;
    dh->status = -1;
    dh->reload_full_path = NULL;
    dh->reload_driver_name = NULL;
    dh->reload_flags = 0;
    dh->full_path = NULL;
    dh->flags = 0;

    if ((res = do_load_driver_entry(dh, path, name)) != ERL_DE_NO_ERROR) {
	erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
	dh = NULL;
    }
    *dhp = dh;
    return res;
}

static void unload_driver_entry(DE_Handle *dh)
{
    do_unload_driver_entry(dh, NULL);
    if (dh->full_path != NULL) {
	erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
    }
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh);
}

static int reload_driver_entry(DE_Handle *dh)
{
    char *path = dh->reload_full_path;
    char *name = dh->reload_driver_name;
    int loadres;
    Uint flags = dh->reload_flags;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    dh->reload_full_path = NULL;
    dh->reload_driver_name = NULL;

    ASSERT(dh->port_count == 0);
    ASSERT(dh->full_path != NULL);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) dh->full_path);
    dh->full_path = NULL;

    loadres = do_load_driver_entry(dh, path, name);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) path);
    erts_free(ERTS_ALC_T_DDLL_HANDLE, (void *) name);
    if (loadres == ERL_DE_NO_ERROR) {
	dh->status = ERL_DE_OK;
	dh->flags = flags;
    }
    return loadres;
}
 
/*
 * Notification {tag = atom(), ref = ref(), driver_name = atom()} or
 *              {'$DDLL_load_failure', ref = ref(), driver_name = atom(), 
 *               error_term = atom() | {system_error, int()}}
 */
   
static void notify_proc(Process *proc, Eterm ref, Eterm driver_name, Eterm type, 
			Eterm tag, int errcode)
{
    Eterm mess;
    Eterm r;
    Eterm *hp;
    ErlHeapFragment *bp;
    ErlOffHeap *ohp;
    Uint32 rp_locks = ERTS_PROC_LOCKS_MSG_SEND;
    ERTS_SMP_CHK_NO_PROC_LOCKS;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    erts_smp_proc_lock(proc, rp_locks);
    if (errcode != 0) {
	int need = load_error_need(errcode);
	Eterm e;
	hp = erts_alloc_message_heap(6 /* tuple */ + 3 /* Error tuple */ + 
				     REF_THING_SIZE + need, &bp, &ohp, 
				     proc, &rp_locks);
	r = copy_ref(ref,hp);
	hp += REF_THING_SIZE;
	e = build_load_error_hp(hp, errcode);
	hp += need;
	mess = TUPLE2(hp,tag,e);
	hp += 3;
	mess = TUPLE5(hp,type,r,am_driver,driver_name,mess);
    } else {	
	hp = erts_alloc_message_heap(6 /* tuple */ + REF_THING_SIZE, &bp, &ohp, proc, &rp_locks);
	r = copy_ref(ref,hp);
	hp += REF_THING_SIZE;
	mess = TUPLE5(hp,type,r,am_driver,driver_name,tag);
    }
    erts_queue_message(proc, rp_locks, bp, mess, am_undefined);
    erts_smp_proc_unlock(proc, rp_locks);
    ERTS_SMP_CHK_NO_PROC_LOCKS;
}

static void notify_all(DE_Handle *dh, char *name, Uint awaiting, Eterm type, Eterm tag)
{
    DE_ProcEntry **p;

    p = &(dh->procs);
    while (*p != NULL) {
	if ((*p)->awaiting_status == awaiting) {
	    DE_ProcEntry *pe;
	    pe = *p;
	    *p = pe->next;
	    notify_proc(pe->proc, make_internal_ref(&(pe->heap)), mkatom(name), type, tag, 0);
	    erts_free(ERTS_ALC_T_DDLL_PROCESS, (void *) pe);
	} else {
	    p = &((*p)->next);
	}
    }
}



typedef struct errcode_entry {
    char *atm;
    int code;
} ErrcodeEntry;

static ErrcodeEntry errcode_tab[] = {
    {"no_error", ERL_DE_NO_ERROR},
    {"no_driver_init", ERL_DE_LOAD_ERROR_NO_INIT},
    {"driver_init_failed", ERL_DE_LOAD_ERROR_FAILED_INIT},
    {"bad_driver_name", ERL_DE_LOAD_ERROR_BAD_NAME},
    {"driver_name_to_long", ERL_DE_LOAD_ERROR_NAME_TO_LONG},
    {"no_ddll_available",  ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY},
    {"unspecified_error", ERL_DE_ERROR_UNSPECIFIED},
    {"symbol_not_found", ERL_DE_LOOKUP_ERROR_NOT_FOUND},
    {NULL,0}
};

static int errdesc_to_code(Eterm errdesc, int *code /* out */)
{
    int i;
    if (is_atom(errdesc)) {
	Atom *ap = atom_tab(atom_val(errdesc)); 
	for (i = 0; errcode_tab[i].atm != NULL; ++i) {
	    int len = sys_strlen(errcode_tab[i].atm);
	    if (len == ap->len && 
		!sys_strncmp(errcode_tab[i].atm,(char *) ap->name,len)) {
		*code = errcode_tab[i].code;
		return 0;
	    }
	}
	return -1;
    } else if (is_tuple(errdesc)) {
	Eterm *tp = tuple_val(errdesc);
	if (*tp != make_arityval(2) || tp[1] != am_open_error || is_not_small(tp[2])) {
	    return -1;
	}
	*code = signed_val(tp[2]);
	return 0;
    }
    return -1;
}

static Eterm build_load_error(Process *p, int code)
{
    int need = load_error_need(code);
    Eterm *hp = NULL;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    if (need) {
	hp = HAlloc(p,need);
    }
    return build_load_error_hp(hp,code);
}
    
static int load_error_need(int code)
{
    ErrcodeEntry *ee = errcode_tab;
    while (ee->atm != NULL) {
	if (ee->code == code) {
	    return 0;
	}
	++ee;
    }
    return 3;
}

static Eterm build_load_error_hp(Eterm *hp, int code)
{
    ErrcodeEntry *ee = errcode_tab;
    while (ee->atm != NULL) {
	if (ee->code == code) {
	    return mkatom(ee->atm);
	}
	++ee;
    }
    return TUPLE2(hp,am_open_error, make_small(code));
}
    


static Eterm mkatom(char *str)
{
    return am_atom_put(str, sys_strlen(str));
}

static char *pick_list_or_atom(Eterm name_term)
{ 
    char *name = NULL;
    int name_len;
    if (is_atom(name_term)) {
	Atom *ap = atom_tab(atom_val(name_term));
	if (ap->len == 0) {
	    /* If io_lists with zero length is not allowed, 
	       then the empty atom shouldn't */
	    goto error;
	}
	name = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, ap->len + 1);
	memcpy(name,ap->name,ap->len);
	name[ap->len] = '\0';
    } else {
	name_len = io_list_len(name_term);
	if (name_len <= 0) {
	    goto error;
	}
	name = erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, name_len + 1);
	if (io_list_to_buf(name_term, name, name_len) != 0) {
	    goto error;
	}
	name[name_len] = '\0';
    }
    return name;
 error:
    if (name != NULL) {
	erts_free(ERTS_ALC_T_DDLL_TMP_BUF, (void *) name);
    }
    return NULL;
}

static int build_proc_info(DE_Handle *dh, ProcEntryInfo **out_pei, Uint filter)
{
    ProcEntryInfo *pei = NULL;
    int num_pei = 0;
    int num_pei_allocated = 0;
    int i;
    DE_ProcEntry *pe;

    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());

    for (pe = dh->procs; pe != NULL; pe = pe->next) {
	Eterm id = pe->proc->id;
	Uint stat = pe->awaiting_status;
	if (stat != filter) {
	    continue;
	}
	for (i = 0; i < num_pei; ++i) {
	    if (pei[i].pid == id && pei[i].status == stat) {
		break;
	    }
	}
	if (i < num_pei) {
	    pei[i].count++;
	} else {
	    if (num_pei >= num_pei_allocated) {
		pei = (pei == NULL) 
		    ? erts_alloc(ERTS_ALC_T_DDLL_TMP_BUF, 
				 sizeof(ProcEntryInfo) * (num_pei_allocated = 10))
		    : erts_realloc(ERTS_ALC_T_DDLL_TMP_BUF, pei,
				   sizeof(ProcEntryInfo) * (num_pei_allocated += 10));
	    }
	    pei[num_pei].pid = id;
	    pei[num_pei].proc = pe->proc;
	    pei[num_pei].status = stat;
	    pei[num_pei].count = 1;
	    ++num_pei;
	}
    }
    *out_pei = pei;
    return num_pei;
}
	    
    

static DE_List *lookup_driver(char *name)
{
    DE_List *de;
    ERTS_SMP_LC_ASSERT(erts_smp_lc_io_is_locked());
    for (de = driver_list; de != NULL && strcmp(de->drv->driver_name,name); de = de->next)
	;
    return de;
}
    
