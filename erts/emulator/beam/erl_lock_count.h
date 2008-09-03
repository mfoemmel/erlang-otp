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
 * Description:	Statistics for locks.
 *
 * Author:	Björn-Egil Dahlberg
 * Date:	2008-07-03
 */

#include "sys.h"

#ifndef ERTS_LOCK_COUNT_H__
#define ERTS_LOCK_COUNT_H__

#ifdef  ERTS_ENABLE_LOCK_COUNT
#include "ethread.h"

#define ERTS_LCNT_LT_SPINLOCK   (((Uint16) 1) << 0)
#define ERTS_LCNT_LT_RWSPINLOCK (((Uint16) 1) << 1)
#define ERTS_LCNT_LT_MUTEX      (((Uint16) 1) << 2)
#define ERTS_LCNT_LT_RWMUTEX    (((Uint16) 1) << 3)
#define ERTS_LCNT_LT_PROCLOCK   (((Uint16) 1) << 4)

#define ERTS_LCNT_LO_READ       (((Uint16) 1) << 5)
#define ERTS_LCNT_LO_WRITE      (((Uint16) 1) << 6)

#define ERTS_LCNT_LO_READ_WRITE ( ERTS_LCNT_LO_READ  \
				| ERTS_LCNT_LO_WRITE )

#define ERTS_LCNT_LT_ALL        ( ERTS_LCNT_LT_SPINLOCK   \
				| ERTS_LCNT_LT_RWSPINLOCK \
				| ERTS_LCNT_LT_MUTEX      \
				| ERTS_LCNT_LT_RWMUTEX    \
				| ERTS_LCNT_LT_PROCLOCK   )

/* rw locks uses both states, other locks only uses w_state */
typedef struct erts_lcnt_lock_s {
    char *name;            /* lock name */
    Uint16 flag;           /* lock type */
    Eterm id;              /* id if possible */ 

    /* lock states */    
    ethr_atomic_t w_state; /* 0 not taken, otherwise n threads waiting */
    ethr_atomic_t r_state; /* 0 not taken, > 0 -> writes will wait */

    /* statistics */    
    ethr_atomic_t tries;   /* n tries to get lock */
    ethr_atomic_t colls;   /* n collisions of tries to get lock */

    /* timer, data is hold by the instrumented lock 
     * only modified by post_lock() operations (when the lock is held) */
    unsigned long timer_s;
    unsigned long timer_ns;
    unsigned long timer_n;

    /* chains for list handling */
    /* Data is hold by lcnt_lock() */
    struct erts_lcnt_lock_s *prev;
    struct erts_lcnt_lock_s *next;
} erts_lcnt_lock_t;

typedef struct {
    erts_lcnt_lock_t *head;
    erts_lcnt_lock_t *tail;
    unsigned long n;
} erts_lcnt_lock_list_t;
    
typedef struct {
    erts_lcnt_lock_list_t *current_locks;
    erts_lcnt_lock_list_t *deleted_locks;
} erts_lcnt_data_t;
    
typedef struct {
    unsigned long s;
    unsigned long ns;
} erts_lcnt_time_t;

typedef union {
    SysHrTime hr_time;
    erts_lcnt_time_t lcnt_time;
}  erts_lcnt_time_u;

typedef struct {
    erts_lcnt_time_t timer;	/* timer */
    int timer_set;		/* bool  */
    int id;
    /* function pointer */
    void *(*function)(void *);
    void *argument;
} erts_lcnt_thread_data_t;

void erts_lcnt_init(void);

/* list operations (local)  */
erts_lcnt_lock_list_t *erts_lcnt_list_init(void);
void erts_lcnt_list_clear( erts_lcnt_lock_list_t *list);
void erts_lcnt_list_insert(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock);
void erts_lcnt_list_delete(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock);

/* lock operations (global) */
void erts_lcnt_init_lock(erts_lcnt_lock_t *lock, char *name, Uint16 flag);
void erts_lcnt_init_lock_extra(erts_lcnt_lock_t *lock, char *name, Uint16 flag, Eterm id);
void erts_lcnt_destroy_lock(erts_lcnt_lock_t *lock);

void erts_lcnt_lock(erts_lcnt_lock_t *lock);
void erts_lcnt_lock_opt(erts_lcnt_lock_t *lock, Uint16 option);
void erts_lcnt_lock_post(erts_lcnt_lock_t *lock);

void erts_lcnt_unlock(erts_lcnt_lock_t *lock);
void erts_lcnt_unlock_opt(erts_lcnt_lock_t *lock, Uint16 option);

void erts_lcnt_trylock_opt(erts_lcnt_lock_t *lock, int res, Uint16 option);
void erts_lcnt_trylock(erts_lcnt_lock_t *lock, int res);

/* thread operations */

int erts_lcnt_thr_create(ethr_tid *tid, void * (*function)(void *), void *arg, ethr_thr_opts *opts);

/* bif interface */
void erts_lcnt_clear_counters(void);
erts_lcnt_data_t *erts_lcnt_get_data(void);
char *erts_lcnt_lock_type(Uint16 type);

#endif /* ifdef  ERTS_ENABLE_LOCK_COUNT  */
#endif /* ifndef ERTS_LOCK_COUNT_H__     */
