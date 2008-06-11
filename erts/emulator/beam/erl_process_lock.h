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
 * The Initial Developer of the Original Code is Ericsson AB.
 * Portions created by Ericsson are Copyright 2007, Ericsson AB.
 * All Rights Reserved.''
 * 
 *     $Id$
 */


/*
 * Description:	Impementation of Erlang process locks.
 *
 * Author: 	Rickard Green
 */

#ifndef ERTS_PROC_LOCK_TYPE__
#define ERTS_PROC_LOCK_TYPE__

#ifdef ERTS_ENABLE_LOCK_CHECK
#define ERTS_PROC_LOCK_DEBUG
#endif

#include "erl_smp.h"

#define ERTS_PROC_LOCK_ATOMIC_IMPL 0
#define ERTS_PROC_LOCK_SPINLOCK_IMPL 0
#define ERTS_PROC_LOCK_MUTEX_IMPL 0

#if defined(ETHR_HAVE_OPTIMIZED_ATOMIC_OPS)
#  undef ERTS_PROC_LOCK_ATOMIC_IMPL
#  define ERTS_PROC_LOCK_ATOMIC_IMPL 1
#elif defined(ETHR_HAVE_OPTIMIZED_SPINLOCK)
#  undef ERTS_PROC_LOCK_SPINLOCK_IMPL
#  define ERTS_PROC_LOCK_SPINLOCK_IMPL 1
#else
#  undef ERTS_PROC_LOCK_MUTEX_IMPL
#  define ERTS_PROC_LOCK_MUTEX_IMPL 1
#endif

#define ERTS_PROC_LOCK_MAX_BIT 3

typedef Uint32 ErtsProcLocks;

typedef struct erts_proc_lock_queues_t_ erts_proc_lock_queues_t;

typedef struct erts_proc_lock_t_ {
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_smp_atomic_t flags;
#else
    ErtsProcLocks flags;
#endif
    erts_proc_lock_queues_t *queues;
    long refc;
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_smp_atomic_t locked[ERTS_PROC_LOCK_MAX_BIT+1];
#endif
} erts_proc_lock_t;

/* Process lock flags */

/*
 * Main lock:
 *   The main lock is held by the scheduler running a process. It
 *   is used to protect all fields in the process structure except
 *   for those fields protected by other process locks (follows).
 */
#define ERTS_PROC_LOCK_MAIN		(((ErtsProcLocks) 1) << 0)

/*
 * Link lock:
 *   Protects the following fields in the process structure:
 *   * nlinks
 *   * monitors
 *   * suspend_monitors
 */
#define ERTS_PROC_LOCK_LINK		(((ErtsProcLocks) 1) << 1)

/*
 * Message queue lock:
 *   Protects the following fields in the process structure:
 *   * msg_inq
 *   * bif_timers
 */
#define ERTS_PROC_LOCK_MSGQ		(((ErtsProcLocks) 1) << 2)

/*
 * Status lock:
 *   Protects the following fields in the process structure:
 *   * status
 *   * rstatus
 *   * status_flags
 *   * pending_suspenders
 *   * suspendee
 */
#define ERTS_PROC_LOCK_STATUS		(((ErtsProcLocks) 1) << ERTS_PROC_LOCK_MAX_BIT)

/*
 * Special fields:
 *
 *   The following fields are read only and can be read if at
 *   least one process lock (whichever one doesn't matter)
 *   is held, or if the process structure is guaranteed not to
 *   disappear by other means (e.g. pix lock is held):
 *     * id
 *
 *   The following fields are only allowed to be written if
 *   all process locks are held, and are allowed to be read if
 *   at least one process lock (whichever one doesn't matter)
 *   is held:
 *     * tracer_proc
 *     * tracer_flags
 *
 *   The following fields are only allowed to be accessed if
 *   both the schedule queue lock and at least one process lock
 *   (whichever one doesn't matter) are held:
 *     * prio
 *     * next
 *     * scheduler_flags
 */

/*
 * Other rules regarding process locking:
 *
 * Exiting processes:
 *   When changing status to P_EXITING on a process, you are required
 *   to take all process locks (ERTS_PROC_LOCKS_ALL). Thus, by holding
 *   at least one process lock (whichever one doesn't matter) you
 *   are guaranteed that the process won't exit until the lock you are
 *   holding has been released. Appart from all process locks also
 *   the pix lock corresponding to the process has to be held.
 *     At the same time as status is changed to P_EXITING, also the
 *   field 'is_exiting' in the process structure is set to a value != 0.
 *
 * Lock order:
 *   Process locks with low numeric values has to be locked before
 *   process locks with high numeric values. E.g., main locks has
 *   to be locked before message queue locks.
 *
 *   When process locks with the same numeric value are to be locked
 *   on multiple processes, locks on processes with low process ids
 *   have to be locked before locks on processes with high process
 *   ids. E.g., if the main and the message queue locks are to be
 *   locked on processes p1 and p2 and p1->id < p2->id, then locks
 *   should be locked in the following order:
 *     1. main lock on p1
 *     2. main lock on p2
 *     3. message queue lock on p1
 *     4. message queue lock on p2
 */

/* Other lock flags */
#define ERTS_PROC_LOCK_WAITER_SHIFT (ERTS_PROC_LOCK_MAX_BIT + 1)


/* ERTS_PROC_LOCKS_* are combinations of process locks */

#define ERTS_PROC_LOCKS_MSG_RECEIVE	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_MSG_SEND	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)
#define ERTS_PROC_LOCKS_XSIG_SEND	(ERTS_PROC_LOCK_MSGQ		\
					 | ERTS_PROC_LOCK_STATUS)

#define ERTS_PROC_LOCKS_ALL \
  ((((ErtsProcLocks) 1) << (ERTS_PROC_LOCK_MAX_BIT + 1)) - 1)

#define ERTS_PROC_LOCKS_ALL_MINOR	(ERTS_PROC_LOCKS_ALL \
					 & ~ERTS_PROC_LOCK_MAIN)


#define ERTS_PIX_LOCKS_BITS		8
#define ERTS_NO_OF_PIX_LOCKS		(1 << ERTS_PIX_LOCKS_BITS)


#endif /* #ifndef ERTS_PROC_LOCK_TYPE__ */

#ifndef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#ifndef ERTS_PROCESS_LOCK_H__
#define ERTS_PROCESS_LOCK_H__

/* --- Process lock checking ----------------------------------------------- */

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
#define ERTS_SMP_CHK_NO_PROC_LOCKS \
  erts_proc_lc_chk_no_proc_locks(__FILE__, __LINE__)
#define ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(P) \
  erts_proc_lc_chk_only_proc_main((P))
void erts_proc_lc_lock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_trylock(Process *p, ErtsProcLocks locks, int locked);
void erts_proc_lc_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_might_unlock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_have_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_proc_locks(Process *p, ErtsProcLocks locks);
void erts_proc_lc_chk_only_proc_main(Process *p);
void erts_proc_lc_chk_no_proc_locks(char *file, int line);
ErtsProcLocks erts_proc_lc_my_proc_locks(Process *p);
int erts_proc_lc_trylock_force_busy(Process *p, ErtsProcLocks locks);
void erts_proc_lc_require_lock(Process *p, ErtsProcLocks locks);
void erts_proc_lc_unrequire_lock(Process *p, ErtsProcLocks locks);
#else
#define ERTS_SMP_CHK_NO_PROC_LOCKS
#define ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(P)
#endif

#ifdef ERTS_SMP

typedef struct {
    union {
#if ERTS_PROC_LOCK_MUTEX_IMPL
	erts_smp_mtx_t mtx;
#else
	erts_smp_spinlock_t spnlck;
#endif
	char buf[64]; /* Try to get locks in different cache lines */
    } u;
} erts_pix_lock_t;

#define ERTS_PIX2PIXLOCKIX(PIX) \
  ((PIX) & ((1 << ERTS_PIX_LOCKS_BITS) - 1))
#define ERTS_PIX2PIXLOCK(PIX) \
  (&erts_pix_locks[ERTS_PIX2PIXLOCKIX((PIX))])
#define ERTS_PID2PIXLOCK(PID) \
  ERTS_PIX2PIXLOCK(internal_pid_data((PID)))

#if ERTS_PROC_LOCK_ATOMIC_IMPL

#define ERTS_PROC_LOCK_FLGS_BAND_(L, MSK) \
  ((ErtsProcLocks) erts_smp_atomic_band(&(L)->flags, (long) (MSK)))
#define ERTS_PROC_LOCK_FLGS_BOR_(L, MSK) \
  ((ErtsProcLocks) erts_smp_atomic_bor(&(L)->flags, (long) (MSK)))
#define ERTS_PROC_LOCK_FLGS_READ_(L) \
  ((ErtsProcLocks) erts_smp_atomic_read(&(L)->flags))

#else /* no opt atomic ops */

ERTS_GLB_INLINE ErtsProcLocks erts_proc_lock_flags_band(erts_proc_lock_t *,
							ErtsProcLocks);
ERTS_GLB_INLINE ErtsProcLocks erts_proc_lock_flags_bor(erts_proc_lock_t *,
						       ErtsProcLocks);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE ErtsProcLocks
erts_proc_lock_flags_band(erts_proc_lock_t *lck, ErtsProcLocks mask)
{
    ErtsProcLocks res = lck->flags;
    lck->flags &= mask;
    return res;
}

ERTS_GLB_INLINE ErtsProcLocks
erts_proc_lock_flags_bor(erts_proc_lock_t *lck, ErtsProcLocks mask)
{
    ErtsProcLocks res = lck->flags;
    lck->flags |= mask;
    return res;
}

#endif

#define ERTS_PROC_LOCK_FLGS_BAND_(L, MSK) erts_proc_lock_flags_band((L), (MSK))
#define ERTS_PROC_LOCK_FLGS_BOR_(L, MSK) erts_proc_lock_flags_bor((L), (MSK))
#define ERTS_PROC_LOCK_FLGS_READ_(L) ((L)->flags)

#endif /* end no opt atomic ops */

extern erts_pix_lock_t erts_pix_locks[ERTS_NO_OF_PIX_LOCKS];

void erts_init_proc_lock(void);
void erts_proc_lock_prepare_proc_lock_waiter(void);
void erts_proc_lock_failed(Process *,
			   erts_pix_lock_t *,
			   ErtsProcLocks,
			   ErtsProcLocks);
void erts_proc_unlock_failed(Process *,
			     erts_pix_lock_t *,
			     ErtsProcLocks);
void erts_proc_trylock_failed(Process *,
			      erts_pix_lock_t *pix_lck,
			      ErtsProcLocks,
			      ErtsProcLocks);

ERTS_GLB_INLINE void erts_pix_lock(erts_pix_lock_t *);
ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *);
ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *);

ERTS_GLB_INLINE void erts_inc_proc_lock_refc(Process *);
ERTS_GLB_INLINE void erts_dec_proc_lock_refc(Process *);

ERTS_GLB_INLINE void erts_smp_proc_lock__(Process *,
					  erts_pix_lock_t *,
					  ErtsProcLocks);
ERTS_GLB_INLINE void erts_smp_proc_unlock__(Process *,
					    erts_pix_lock_t *,
					    ErtsProcLocks);
ERTS_GLB_INLINE int erts_smp_proc_trylock__(Process *,
					    erts_pix_lock_t *,
					    ErtsProcLocks);

#ifdef ERTS_PROC_LOCK_DEBUG
ERTS_GLB_INLINE void erts_proc_lock_op_debug(Process *, ErtsProcLocks, int);
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void erts_pix_lock(erts_pix_lock_t *pixlck)
{
    ERTS_LC_ASSERT(pixlck);
#if ERTS_PROC_LOCK_MUTEX_IMPL
    erts_smp_mtx_lock(&pixlck->u.mtx);
#else
    erts_smp_spin_lock(&pixlck->u.spnlck);
#endif
}

ERTS_GLB_INLINE void erts_pix_unlock(erts_pix_lock_t *pixlck)
{
    ERTS_LC_ASSERT(pixlck);
#if ERTS_PROC_LOCK_MUTEX_IMPL
    erts_smp_mtx_unlock(&pixlck->u.mtx);
#else
    erts_smp_spin_unlock(&pixlck->u.spnlck);
#endif
}

ERTS_GLB_INLINE int erts_lc_pix_lock_is_locked(erts_pix_lock_t *pixlck)
{
#if ERTS_PROC_LOCK_MUTEX_IMPL
    return erts_smp_lc_mtx_is_locked(&pixlck->u.mtx);
#else
    return erts_smp_lc_spinlock_is_locked(&pixlck->u.spnlck);
#endif
}

ERTS_GLB_INLINE void erts_inc_proc_lock_refc(Process *p)
{
    erts_pix_lock_t *pixlck = ERTS_PID2PIXLOCK(p->id);
    erts_pix_lock(pixlck);
    ERTS_LC_ASSERT(p->lock.refc > 0);
    p->lock.refc++;
    erts_pix_unlock(pixlck);
}

ERTS_GLB_INLINE void erts_dec_proc_lock_refc(Process *p)
{
    Process *fp;
    erts_pix_lock_t *pixlck = ERTS_PID2PIXLOCK(p->id);
    erts_pix_lock(pixlck);
    ERTS_LC_ASSERT(p->lock.refc > 0);
    fp = --p->lock.refc == 0 ? p : NULL; 
    erts_pix_unlock(pixlck);
    if (fp)
	erts_free_proc(fp);
}

ERTS_GLB_INLINE void
erts_smp_proc_lock__(Process *p,
		     erts_pix_lock_t *pix_lck,
		     ErtsProcLocks locks)
{
    ErtsProcLocks waiters, old_lflgs;
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lck);
#endif

    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);
    waiters = locks << ERTS_PROC_LOCK_WAITER_SHIFT;
    old_lflgs = ERTS_PROC_LOCK_FLGS_BOR_(&p->lock, locks);

    if (old_lflgs & (waiters | locks)) {
	/* erts_proc_lock_failed() always returns with pix_lck unlocked. */
	erts_proc_lock_failed(p, pix_lck, locks, old_lflgs);
    }
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    else {
	ERTS_LC_ASSERT(locks == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));
	erts_pix_unlock(pix_lck);
    }
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_lock(p, locks);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 1);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock__(Process *p,
		       erts_pix_lock_t *pix_lck,
		       ErtsProcLocks locks)
{
    ErtsProcLocks old_lflgs, waiters;

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_unlock(p, locks);
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
    erts_proc_lock_op_debug(p, locks, 0);
#endif

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_pix_lock(pix_lck);
#endif

    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);
    ERTS_LC_ASSERT(locks == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));

    old_lflgs = ERTS_PROC_LOCK_FLGS_BAND_(&p->lock, ~locks);
    waiters = (old_lflgs >> ERTS_PROC_LOCK_WAITER_SHIFT) & locks;
    if (waiters) {
	/* erts_proc_unlock_failed() always returns with pix_lck unlocked. */
	erts_proc_unlock_failed(p, pix_lck, waiters);
    }
#if !ERTS_PROC_LOCK_ATOMIC_IMPL
    else {
	erts_pix_unlock(pix_lck);
    }
#endif
}

ERTS_GLB_INLINE int
erts_smp_proc_trylock__(Process *p,
			erts_pix_lock_t *pix_lck,
			ErtsProcLocks locks)
{
    int res;

#ifdef ERTS_ENABLE_LOCK_CHECK
    ERTS_LC_ASSERT((locks & ~ERTS_PROC_LOCKS_ALL) == 0);
    if (erts_proc_lc_trylock_force_busy(p, locks)) {
	res = EBUSY; /* Make sure caller can handle the situation without
			causing a lock order violation to occur */
    }
    else
#endif
    {
	ErtsProcLocks old_lflgs;

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	erts_pix_lock(pix_lck);
#endif

	old_lflgs = ERTS_PROC_LOCK_FLGS_BOR_(&p->lock, locks);
	if (old_lflgs & (locks | (locks << ERTS_PROC_LOCK_WAITER_SHIFT))) {
	    /* Didn't get all locks... */
	    /*
	     * erts_proc_trylock_failed() always returns with pix_lck
	     * unlocked.
	     */
	    erts_proc_trylock_failed(p, pix_lck, locks, old_lflgs);
	    res = EBUSY;
	}
	else {
	    res = 0;

	    ERTS_LC_ASSERT(locks
			   == (ERTS_PROC_LOCK_FLGS_READ_(&p->lock) & locks));

#if !ERTS_PROC_LOCK_ATOMIC_IMPL
	    erts_pix_unlock(pix_lck);
#endif

#ifdef ERTS_PROC_LOCK_DEBUG
	    erts_proc_lock_op_debug(p, locks, 1);
#endif
	}
    }

#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, locks, res == 0);
#endif

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

    return res;
}

#ifdef ERTS_PROC_LOCK_DEBUG
ERTS_GLB_INLINE void 
erts_proc_lock_op_debug(Process *p, ErtsProcLocks locks, int locked)
{
    int i;
    for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++) {
	ErtsProcLocks lock = ((ErtsProcLocks) 1) << i;
	if (locks & lock) {
	    long lock_count;
	    if (locked) {
		lock_count = erts_smp_atomic_inctest(&p->lock.locked[i]);
		ERTS_LC_ASSERT(lock_count == 1);
	    }
	    else {
		lock_count = erts_smp_atomic_dectest(&p->lock.locked[i]);
		ERTS_LC_ASSERT(lock_count == 0);
	    }
	}
    }
}
#endif

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* ERTS_SMP */

ERTS_GLB_INLINE void erts_smp_proc_lock(Process *, ErtsProcLocks);
ERTS_GLB_INLINE void erts_smp_proc_unlock(Process *, ErtsProcLocks);
ERTS_GLB_INLINE int erts_smp_proc_trylock(Process *, ErtsProcLocks);


#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE void
erts_smp_proc_lock(Process *p, ErtsProcLocks locks)
{
#ifdef ERTS_SMP
    erts_smp_proc_lock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			 NULL,
#else
			 ERTS_PID2PIXLOCK(p->id),
#endif
			 locks);
#endif
}

ERTS_GLB_INLINE void
erts_smp_proc_unlock(Process *p, ErtsProcLocks locks)
{
#ifdef ERTS_SMP
    erts_smp_proc_unlock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
			   NULL,
#else
			   ERTS_PID2PIXLOCK(p->id),
#endif
			   locks);
#endif
}

ERTS_GLB_INLINE int
erts_smp_proc_trylock(Process *p, ErtsProcLocks locks)
{
#ifndef ERTS_SMP
    return 0;
#else
    return erts_smp_proc_trylock__(p,
#if ERTS_PROC_LOCK_ATOMIC_IMPL
				   NULL,
#else
				   ERTS_PID2PIXLOCK(p->id),
#endif
				   locks);
#endif
}


#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#ifdef ERTS_SMP
void erts_proc_lock_init(Process *);
void erts_proc_safelock(Process *a_proc,
			ErtsProcLocks a_have_locks,
			ErtsProcLocks a_need_locks,
			Process *b_proc,
			ErtsProcLocks b_have_locks,
			ErtsProcLocks b_need_locks);
#endif

/*
 * --- Process table lookup ------------------------------------------------
 *
 * erts_pid2proc() and friends looks up the process structure of a pid
 * and at the same time acquires process locks in the smp case. Locks
 * on currently executing process and looked up process are taken according
 * to the lock order, i.e., locks on currently executing process may have
 * been released and reacquired.
 *
 * erts_pid2proc_opt() currently accepts the following flags:
 *   ERTS_P2P_FLG_ALLOW_OTHER_X    Lookup process even if it currently
 *                                 is exiting.
 */

#define ERTS_P2P_FLG_ALLOW_OTHER_X	(1 <<  0)
#define ERTS_P2P_FLG_TRY_LOCK		(1 <<  1)

#define ERTS_PROC_LOCK_BUSY ((Process *) &erts_proc_lock_busy)
extern const Process erts_proc_lock_busy;

#define erts_pid2proc(PROC, HL, PID, NL) \
  erts_pid2proc_opt((PROC), (HL), (PID), (NL), 0)

ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *, ErtsProcLocks, Eterm, ErtsProcLocks, int);

#ifdef ERTS_SMP
void
erts_pid2proc_trylock_failed(Process *p,
			     erts_pix_lock_t *pixlck,
			     ErtsProcLocks locks,
			     ErtsProcLocks wlocks,
			     ErtsProcLocks old_lflgs);
void
erts_pid2proc_safelock(Process *c_p,
		       ErtsProcLocks c_p_have_locks,
		       Process **proc,
		       ErtsProcLocks have_locks,
		       ErtsProcLocks need_locks,
		       ErtsProcLocks wait_locks,
		       erts_pix_lock_t *pix_lock,
		       int flags);
ERTS_GLB_INLINE Process *erts_pid2proc_unlocked_opt(Eterm pid, int flags);
#define erts_pid2proc_unlocked(PID) erts_pid2proc_unlocked_opt((PID), 0)
#else
#define erts_pid2proc_unlocked_opt(PID, FLGS) \
  erts_pid2proc_opt(NULL, 0, (PID), 0, FLGS)
#define erts_pid2proc_unlocked(PID) erts_pid2proc_opt(NULL, 0, (PID), 0, 0)
#endif

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Process *
#ifdef ERTS_SMP
erts_pid2proc_unlocked_opt(Eterm pid, int flags)
#else
erts_pid2proc_opt(Process *c_p_unused,
		  ErtsProcLocks c_p_have_locks_unused,
		  Eterm pid,
		  ErtsProcLocks pid_need_locks_unused,
		  int flags)
#endif
{
    Uint pix;
    Process *proc;

    if (is_not_internal_pid(pid))
	return NULL;
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes)
	return NULL;
    proc = process_tab[pix];
    if (proc) {
	if (proc->id != pid
	    || (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
		&& proc->status == P_EXITING))
	    proc = NULL;
    }
    return proc;
}

#ifdef ERTS_SMP

ERTS_GLB_INLINE Process *
erts_pid2proc_opt(Process *c_p,
		  ErtsProcLocks c_p_have_locks,
		  Eterm pid,
		  ErtsProcLocks pid_need_locks,
		  int flags)
{
    erts_pix_lock_t *pix_lock;
    ErtsProcLocks need_locks;
    Uint pix;
    Process *proc;

#ifdef ERTS_ENABLE_LOCK_CHECK
    if (c_p) {
	ErtsProcLocks might_unlock = c_p_have_locks & pid_need_locks;
	if (might_unlock)
	    erts_proc_lc_might_unlock(c_p, might_unlock);
    }
#endif

    if (is_not_internal_pid(pid)) {
	proc = NULL;
	goto done;
    }
    pix = internal_pid_index(pid);
    if(pix >= erts_max_processes) {
	proc = NULL;
	goto done;
    }

    ERTS_LC_ASSERT((pid_need_locks & ERTS_PROC_LOCKS_ALL) == pid_need_locks);
    need_locks = pid_need_locks;

    if (c_p && c_p->id == pid) {
	ASSERT(c_p->id != ERTS_INVALID_PID);
	ASSERT(c_p == process_tab[pix]);
	if (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X) && c_p->is_exiting) {
	    proc = NULL;
	    goto done;
	}
	need_locks &= ~c_p_have_locks;
	if (!need_locks) {
	    proc = c_p;
	    goto done;
	}
    }

    pix_lock = ERTS_PIX2PIXLOCK(pix);
    erts_pix_lock(pix_lock);

    proc = process_tab[pix];
    if (proc) {
	if (proc->id != pid || (!(flags & ERTS_P2P_FLG_ALLOW_OTHER_X)
				&& ERTS_PROC_IS_EXITING(proc))) {
	    proc = NULL;
	}
	else {
	    ErtsProcLocks have_locks, wait_locks;

#ifdef ERTS_ENABLE_LOCK_CHECK
	    if (erts_proc_lc_trylock_force_busy(proc, need_locks)) {
		/* Make sure erts_proc_safelock() is enough to handle
		   a potential lock order violation situation... */
		have_locks = 0;
		wait_locks = 0;
		if (flags & ERTS_P2P_FLG_TRY_LOCK) {
		    erts_pix_unlock(pix_lock);
		    proc = ERTS_PROC_LOCK_BUSY;
		    goto done;
		}
	    }
	    else
#endif
	    {
		/* Try to aquire as many locks as possible... */
		ErtsProcLocks old_lflgs;

		have_locks = need_locks;
		old_lflgs = ERTS_PROC_LOCK_FLGS_BOR_(&proc->lock, need_locks);
		have_locks &= ~old_lflgs;
		wait_locks = old_lflgs >> ERTS_PROC_LOCK_WAITER_SHIFT;
		wait_locks &= have_locks;
		have_locks &= ~wait_locks;
#ifdef ERTS_ENABLE_LOCK_CHECK
		if (flags & ERTS_P2P_FLG_TRY_LOCK) {
		    erts_proc_lc_trylock(proc,
					 need_locks,
					 need_locks == have_locks);
		}
		else {
		    if (need_locks != have_locks)
			erts_proc_lc_trylock(proc, need_locks & ~have_locks, 0);
		    if (have_locks)
			erts_proc_lc_trylock(proc, have_locks, 1);
		}
#endif
#ifdef ERTS_PROC_LOCK_DEBUG
		if (!(flags & ERTS_P2P_FLG_TRY_LOCK)
		    || need_locks == have_locks)
		    erts_proc_lock_op_debug(proc, have_locks, 1);
#endif
		ERTS_SMP_LC_ASSERT(!wait_locks || (need_locks & ~have_locks));

		if ((flags & ERTS_P2P_FLG_TRY_LOCK)
		    && (need_locks & ~have_locks)) {
		    erts_pid2proc_trylock_failed(proc,
						 pix_lock,
						 need_locks,
						 wait_locks,
						 old_lflgs); /* Unlocks
								pixlck */
		    proc = ERTS_PROC_LOCK_BUSY;
		    goto done;
		}

		need_locks &= ~have_locks;
	    }

	    if (need_locks)
		erts_pid2proc_safelock(c_p,
				       c_p_have_locks,
				       &proc,
				       have_locks,
				       pid_need_locks,
				       wait_locks,
				       pix_lock,
				       flags);
	}
    }

    erts_pix_unlock(pix_lock);
#ifdef ERTS_PROC_LOCK_DEBUG
    ERTS_LC_ASSERT(!proc
		   || (pid_need_locks ==
		       (ERTS_PROC_LOCK_FLGS_READ_(&proc->lock)
			& pid_need_locks)));
#endif

 done:

#if ERTS_PROC_LOCK_ATOMIC_IMPL
    ETHR_COMPILER_BARRIER;
#endif

    return proc;
}
#endif /* ERTS_SMP */

#endif /* #if ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* #ifndef ERTS_PROCESS_LOCK_H__ */
#endif /* #ifndef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__ */
