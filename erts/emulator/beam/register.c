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
 * Manage registered processes.
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

static Hash process_reg;

#define PREG_HASH_SIZE 10

#define REG_HASH(term) ((HashValue) atom_val(term))

static erts_smp_rwmtx_t regtab_rwmtx;

#define reg_lock_init()			erts_smp_rwmtx_init(&regtab_rwmtx, \
							    "reg_tab")
#define reg_try_read_lock()		erts_smp_rwmtx_tryrlock(&regtab_rwmtx)
#define reg_try_write_lock()		erts_smp_rwmtx_tryrwlock(&regtab_rwmtx)
#define reg_read_lock()			erts_smp_rwmtx_rlock(&regtab_rwmtx)
#define reg_write_lock()		erts_smp_rwmtx_rwlock(&regtab_rwmtx)
#define reg_read_unlock()		erts_smp_rwmtx_runlock(&regtab_rwmtx)
#define reg_write_unlock()		erts_smp_rwmtx_rwunlock(&regtab_rwmtx)

#ifdef ERTS_SMP
static ERTS_INLINE void
reg_safe_read_lock(Process *c_p, Uint32 *c_p_locks)
{
    if (c_p && *c_p_locks) {
	ASSERT(c_p_locks);
	ASSERT(*c_p_locks);

	if (reg_try_read_lock() != EBUSY)
	    return;

	/* Release process locks in order to avoid deadlock */
	erts_smp_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    reg_read_lock();
}

static ERTS_INLINE void
reg_safe_write_lock(Process *c_p, Uint32 *c_p_locks)
{
    if (c_p && *c_p_locks) {
	ASSERT(c_p_locks);
	ASSERT(*c_p_locks);

	if (reg_try_write_lock() != EBUSY)
	    return;

	/* Release process locks in order to avoid deadlock */
	erts_smp_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    reg_write_lock();
}

static ERTS_INLINE void
io_safe_lock(Process *c_p, Uint32 *c_p_locks)
{
    ASSERT(c_p_locks);
    if (c_p && *c_p_locks) {
	if (erts_smp_io_trylock() != EBUSY)
	    return;

	/* Release process locks in order to avoid deadlock */
	erts_smp_proc_unlock(c_p, *c_p_locks);
	*c_p_locks = 0;
    }

    erts_smp_io_lock();
}
#endif

void register_info(int to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    hash_info(to, to_arg, &process_reg);
    if (lock)
	reg_read_unlock();
}

static HashValue reg_hash(RegProc* obj)
{
    return REG_HASH(obj->name);
}

static int reg_cmp(RegProc *tmpl, RegProc *obj) {
    return tmpl->name != obj->name;
}

static RegProc* reg_alloc(RegProc *tmpl)
{
    RegProc* obj = (RegProc*) erts_alloc(ERTS_ALC_T_REG_PROC, sizeof(RegProc));
    if (!obj) {
	erl_exit(1, "Can't allocate %d bytes of memory\n", sizeof(RegProc));
    }
    obj->name = tmpl->name;
    obj->p = tmpl->p;
    obj->pt = tmpl->pt;
    return obj;
}

static void reg_free(RegProc *obj)
{
    erts_free(ERTS_ALC_T_REG_PROC, (void*) obj);
}

void init_register_table(void)
{
    HashFunctions f;

    reg_lock_init();

    f.hash = (H_FUN) reg_hash;
    f.cmp  = (HCMP_FUN) reg_cmp;
    f.alloc = (HALLOC_FUN) reg_alloc;
    f.free = (HFREE_FUN) reg_free;

    hash_init(ERTS_ALC_T_REG_TABLE, &process_reg, "process_reg",
	      PREG_HASH_SIZE, f);
}

/*
 * Register a process or port (can't be registered twice).
 * Returns 0 if name, process or port is already registered.
 *
 * When smp support is enabled:
 *   * Assumes that main lock is locked (and only main lock)
 *     on c_p.
 *
 */
int erts_register_name(Process *c_p, Eterm name, Eterm id)
{
    int res = 0;
    Process *proc = NULL;
    Port *port;
    RegProc r, *rp;
#ifdef ERTS_SMP
    Uint32 proc_locks = ERTS_PROC_LOCK_MAIN;
#endif
    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p->id);

    if (is_not_atom(name) || name == am_undefined)
	return res;

#ifdef ERTS_SMP
    if (is_internal_port(id))
	io_safe_lock(c_p, &proc_locks);
    reg_safe_write_lock(c_p, &proc_locks);

    if (c_p && !proc_locks) {
        erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	if (ERTS_PROC_IS_EXITING(c_p))
	    goto done;
    }
    else
	proc_locks = 0;
    /* proc_locks is now used for looked-up process... */
#endif

    if (is_internal_pid(id)) {
	r.p = proc = erts_pid2proc(c_p, ERTS_PROC_LOCK_MAIN,
				   id, ERTS_PROC_LOCK_MAIN);
	if (!proc)
	    goto done;
#ifdef ERTS_SMP
	proc_locks = ERTS_PROC_LOCK_MAIN;
#endif
	if (proc->reg)
	    goto done;
	r.pt = port = NULL;
    }
    else if (is_internal_port(id)) {
	int ix = internal_port_index(id);
	if (INVALID_PORT(&erts_port[ix], id))
	    goto done;
	r.pt = port = &erts_port[ix];
	if (r.pt->reg)
	    goto done;
	r.p = proc = NULL;
    }
    else
	goto done;

    r.name = name;
    
    rp = (RegProc*) hash_put(&process_reg, (void*) &r);
    if (proc && rp->p == proc) {
	if (IS_TRACED_FL(proc, F_TRACE_PROCS)) {
	    trace_proc(c_p, proc, am_register, name);
	}
	proc->reg = rp;
    }
    else if (port && rp->pt == port) {
	port->reg = rp;
    }

    if ((rp->p && rp->p->id == id) || (rp->pt && rp->pt->id == id)) {
	res = 1;
    }

 done:
#ifdef ERTS_SMP
    if (c_p != proc && proc_locks)
	erts_smp_proc_unlock(proc, proc_locks);
    if (is_internal_port(id))
	erts_smp_io_unlock();
#endif
    reg_write_unlock();
    return res;
}

/*
 *
 * When smp support is enabled:
 *   * Assumes that main lock is locked (and only main lock)
 *     on c_p.
 *
 *   * am_undefined is returned if c_p became exiting.
 */

Eterm
erts_whereis_name_to_id(Process *c_p, Eterm name)
{
    Eterm res = am_undefined;
    HashValue hval;
    int ix;
    HashBucket* b;
#ifdef ERTS_SMP
    Uint32 c_p_locks = ERTS_PROC_LOCK_MAIN;

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p->id);
    reg_safe_read_lock(c_p, &c_p_locks);
    if (c_p && !c_p_locks) {
        erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	if (ERTS_PROC_IS_EXITING(c_p)) {
	    reg_read_unlock();
	    return am_undefined;
	}
    }
#endif

    hval = REG_HASH(name);
    ix = hval % process_reg.size;
    b = process_reg.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */
	
    while (b) {
	RegProc* rp = (RegProc *) b;
	if (rp->name == name) {
	    /*
	     * SMP NOTE: No need to lock registered entity since it cannot
	     * be removed without acquiring write reg lock and id on entity
	     * is read only.
	     */
	    if (rp->p)
		res = rp->p->id;
	    else if (rp->pt)
		res = rp->pt->id;
	    break;
	}
	b = b->next;
    }

    reg_read_unlock();

    ASSERT(is_internal_pid(res) || is_internal_port(res) || res==am_undefined);

    return res;
}


void
erts_whereis_name(Process *c_p, Uint32 c_p_locks,
		  int allow_c_p_exiting,
		  int have_io_lock,
		  Eterm name,
		  Process** proc, Uint32 need_locks, int allow_proc_exiting,
		  Port** port)
{
    RegProc* rp = NULL;
    HashValue hval;
    int ix;
    HashBucket* b;
#ifdef ERTS_SMP
    Uint32 current_c_p_locks = c_p_locks;
    int locked_io_lock = 0;
 restart:

    reg_safe_read_lock(c_p, &current_c_p_locks);

    /* Locked locks:
     * - io lock if port != NULL
     * - read reg lock
     * - current_c_p_locks (either c_p_locks or 0) on c_p
     */
#endif

    hval = REG_HASH(name);
    ix = hval % process_reg.size;
    b = process_reg.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */

    while (b) {
	if (((RegProc *) b)->name == name) {
	    rp = (RegProc *) b;
	    break;
	}
	b = b->next;
    }

    if (proc) {
	if (!rp)
	    *proc = NULL;
	else {
#ifdef ERTS_SMP
	    if (!rp->p)
		*proc = NULL;
	    else {
		if (erts_proc_safelock(c_p,
				       current_c_p_locks,
				       c_p_locks,
				       allow_c_p_exiting,
				       rp->p->id,
				       rp->p,
				       0,
				       need_locks,
				       allow_proc_exiting)) {
		    *proc = rp->p;
		    current_c_p_locks = c_p_locks;
		}
		else {
		    *proc = NULL;
		}
	    }
#else
	    if (rp->p && (allow_proc_exiting || rp->p->status != P_EXITING))
		*proc = rp->p;
	    else
		*proc = NULL;
#endif
	}
    }

    if (port) {
	if (!rp || !rp->pt)
	    *port = NULL;
	else {
#ifdef ERTS_SMP
	    if (!have_io_lock) {
		locked_io_lock = 1;
		have_io_lock = 1; /* One way or the other... */
		if (erts_smp_io_trylock() == EBUSY) {
		    /* Unlock all locks, acquire io lock, and restart... */
		    if (current_c_p_locks) {
			erts_smp_proc_unlock(c_p,
					     current_c_p_locks);
			current_c_p_locks = 0;
		    }
		    reg_read_unlock();
		    erts_smp_io_lock();
		    goto restart;
		}
	    }
#endif
	    *port = rp->pt;
	}
    }

#ifdef ERTS_SMP
    if (c_p) {

	if (!current_c_p_locks)
	    erts_smp_proc_lock(c_p, c_p_locks);

	if (!allow_c_p_exiting && ERTS_PROC_IS_EXITING(c_p)) {
	    ASSERT(!proc || !*proc);

	    if (port && *port) {
		if (locked_io_lock)
		    erts_smp_io_unlock();
		*port = NULL;
	    }
	}
    }

#endif

    reg_read_unlock();
}

Process *
erts_whereis_process(Process *c_p,
		     Uint32 c_p_locks,
		     int allow_c_p_exiting,
		     Eterm name,
		     Uint32 need_locks,
		     int allow_exiting)
{
    Process *proc;
    erts_whereis_name(c_p, c_p_locks, allow_c_p_exiting,
		      0, /* Only important when looking up ports */
		      name, &proc, need_locks, allow_exiting,
		      NULL);
    return proc;
}


/*
 * Unregister a name
 * Return 0 if not registered
 * Otherwise returns 1
 *
 */
int erts_unregister_name(Process *c_p, Uint32 c_p_locks, int have_io_lock,
			 Eterm name)
{
    int res = 0;
    RegProc r, *rp;
#ifdef ERTS_SMP
    Uint32 current_c_p_locks = c_p_locks;
    int unlock_io_lock = !have_io_lock;

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(c_p->id);

 restart:

    reg_safe_write_lock(c_p, &current_c_p_locks);
#endif
    
    r.name = name;
    if ((rp = (RegProc*) hash_get(&process_reg, (void*) &r)) != NULL) {
	Process* p = rp->p;
	Port* pt = rp->pt;
	if (pt != NULL) {
#ifdef ERTS_SMP
	    if (!have_io_lock) {
		have_io_lock = 1; /* One way or the other... */
		if (erts_smp_io_trylock() == EBUSY) {
		    /* Unlock all locks, acquire io lock, and restart... */
		    if (current_c_p_locks) {
			erts_smp_proc_unlock(c_p,
					     current_c_p_locks);
			current_c_p_locks = 0;
		    }
		    reg_write_unlock();
		    erts_smp_io_lock();
		    goto restart;
		}
	    }
#endif
	    pt->reg = NULL;
	} else if (p != NULL) {
#ifdef ERTS_SMP
#ifdef DEBUG
	    int lock_res =
#else
	    (void)
#endif
		erts_proc_safelock(c_p,
				   current_c_p_locks,
				   c_p_locks,
				   1,
				   rp->p->id,
				   rp->p,
				   0,
				   ERTS_PROC_LOCK_MAIN,
				   1);
	    ASSERT(lock_res); /* Since we allow both procs exiting
				 and we have the reg tab lock */
	    current_c_p_locks = c_p_locks;
#endif
	    p->reg = NULL;
#ifdef ERTS_SMP
	    if (rp->p != c_p)
		erts_smp_proc_unlock(rp->p, ERTS_PROC_LOCK_MAIN);
#endif
	    if (IS_TRACED_FL(p, F_TRACE_PROCS)) {
		trace_proc(c_p, p, am_unregister, name);
	    }
	}
	hash_erase(&process_reg, (void*) &r);
	res = 1;
    }

    reg_write_unlock();
#ifdef ERTS_SMP
    if (have_io_lock && unlock_io_lock)
	erts_smp_io_unlock();
    if (c_p && !current_c_p_locks)
	erts_smp_proc_lock(c_p, c_p_locks);
#endif
    return res;
}

int process_reg_size(void)
{
    int size;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    size = process_reg.size;
    if (lock)
	reg_read_unlock();
    return size;
}

int process_reg_sz(void)
{
    int sz;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	reg_read_lock();
    sz = hash_table_sz(&process_reg);
    if (lock)
	reg_read_unlock();
    return sz;
}

/**********************************************************************/

#include "bif.h"

/* return a list of the registered processes */

BIF_RETTYPE registered_0(BIF_ALIST_0)
{
    int i;
    Eterm res;
    Uint need;
    Eterm* hp;
    HashBucket **bucket;
#ifdef ERTS_SMP
    Uint32 proc_locks = ERTS_PROC_LOCK_MAIN;

    ERTS_CHK_HAVE_ONLY_MAIN_PROC_LOCK(BIF_P->id);
    reg_safe_read_lock(BIF_P, &proc_locks);
    if (!proc_locks)
	erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
#endif

    bucket = process_reg.bucket;

    /* work out how much heap we need & maybe garb, by scanning through
       the registered process table */
    need = 0;
    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    need += 2;
	    b = b->next;
	}
    }

    if (need == 0) {
	reg_read_unlock();
	BIF_RET(NIL);
    }

    hp = HAlloc(BIF_P, need);
     
     /* scan through again and make the list */ 
    res = NIL;

    for (i = 0; i < process_reg.size; i++) {
	HashBucket *b = bucket[i];
	while (b != NULL) {
	    RegProc *reg = (RegProc *) b;

	    res = CONS(hp, reg->name, res);
	    hp += 2;
	    b = b->next;
	}
    }

    reg_read_unlock();

    BIF_RET(res);
}
