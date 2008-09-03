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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

/* Needed for VxWorks va_arg */
#include "sys.h"

#ifdef ERTS_ENABLE_LOCK_COUNT

#include "erl_lock_count.h"
#include "ethread.h"
#include "erl_term.h"
#include "atom.h"
#include <stdio.h>

/* globals, dont access these without locks or blocks */

ethr_mutex lcnt_data_lock;

erts_lcnt_data_t *erts_lcnt_data;

static ethr_tsd_key lcnt_thr_data_key;
static int lcnt_n_thr;

/* local functions */

static ERTS_INLINE void lcnt_lock(void) {
    ethr_mutex_lock(&lcnt_data_lock); 
}

static ERTS_INLINE void lcnt_unlock(void) {
    ethr_mutex_unlock(&lcnt_data_lock); 
}


static char* lock_type(Uint16 flag) {
    switch(flag & ERTS_LCNT_LT_ALL) {
    case ERTS_LCNT_LT_SPINLOCK:	  return "spinlock";
    case ERTS_LCNT_LT_RWSPINLOCK: return "rw_spinlock";
    case ERTS_LCNT_LT_MUTEX:      return "mutex";
    case ERTS_LCNT_LT_RWMUTEX:	  return "rw_mutex";
    case ERTS_LCNT_LT_PROCLOCK:	  return "proclock";
    default:                      return "";
    }
}


static void lcnt_time(erts_lcnt_time_t *time) {
    SysHrTime hr_time;
    hr_time  = sys_gethrtime();
    time->s  = (unsigned long)(hr_time / 1000000000LL);
    time->ns = (unsigned long)(hr_time - 1000000000LL*time->s);
}

static erts_lcnt_thread_data_t *lcnt_thread_data_alloc(void) {
    erts_lcnt_thread_data_t *eltd;
    
    eltd = (erts_lcnt_thread_data_t*)malloc(sizeof(erts_lcnt_thread_data_t));
    eltd->timer_set = 0;
    eltd->id = lcnt_n_thr++;
    
    return eltd;
} 

static erts_lcnt_thread_data_t *lcnt_get_thread_data(void) {
    return (erts_lcnt_thread_data_t *)ethr_tsd_get(lcnt_thr_data_key);
}


/* debug */

#if 0
static char* lock_opt(Uint16 flag) {
    if ((flag & ERTS_LCNT_LO_WRITE) && (flag & ERTS_LCNT_LO_READ)) return "rw";
    if (flag & ERTS_LCNT_LO_READ      )                            return "r ";
    if (flag & ERTS_LCNT_LO_WRITE     )                            return " w";
    return "--";
}

static void print_colls(erts_lcnt_lock_t *lock, char *action, Uint16 opt) {
    long r_state, w_state;
    char buffer[256];
    erts_lcnt_thread_data_t *eltd = NULL;
   
    ethr_atomic_read(&lock->r_state, &r_state);
    ethr_atomic_read(&lock->w_state, &w_state);
    
    eltd = lcnt_get_thread_data();
    
    if (eltd) {
        sprintf(buffer, "[%d/%ld] %s %s:", eltd->id, (long)ethr_self(), action, lock_opt(opt));
        print_lock(lock, buffer);
    } else {
	fprintf(stderr, "A null thread =(\r\n");
    }
}

static void print_lock(erts_lcnt_lock_t *lock, char *action) {
    long int colls, tries, w_state, r_state;
    float rate;
    char *type;
    type = lock_type(lock->flag);
    ethr_atomic_read(&lock->tries, &tries);
    ethr_atomic_read(&lock->colls, &colls);
    ethr_atomic_read(&lock->r_state, &r_state);
    ethr_atomic_read(&lock->w_state, &w_state);
    
    if (tries > 0) rate = (float)(colls/(float)tries)*100;
    else rate = 0.0f;
   
    fprintf(stderr, "%8s [%25s] [type %12s] [r/w state %2ld/%2ld] [tries %9ld] [colls %9ld] [rate %3.3f %%] [acc %ld %ld (%ld)]\r\n", 
	    action, lock->name, type, r_state, w_state, tries, colls, rate, lock->timer_s, lock->timer_ns, lock->timer_n);
}

#endif

void erts_lcnt_init() {
    erts_lcnt_thread_data_t *eltd = NULL;
    
    /* init lock */
    if (ethr_mutex_init(&lcnt_data_lock) != 0) abort();

    /* init tsd */    
    lcnt_n_thr = 1;    
    
    ethr_tsd_key_create(&lcnt_thr_data_key);

    eltd = lcnt_thread_data_alloc();
    
    ethr_tsd_set(lcnt_thr_data_key, eltd);
    
    lcnt_lock();
    
    /* init lcnt structure */
    erts_lcnt_data = (erts_lcnt_data_t*)malloc(sizeof(erts_lcnt_data_t));
    erts_lcnt_data->current_locks = erts_lcnt_list_init();
    erts_lcnt_data->deleted_locks = erts_lcnt_list_init();
    
    lcnt_unlock();
}

/* list operations */

/* BEGIN ASSUMPTION: lcnt_data_lock taken */

erts_lcnt_lock_list_t *erts_lcnt_list_init(void) {
    erts_lcnt_lock_list_t *list;
    
    list = (erts_lcnt_lock_list_t*)malloc(sizeof(erts_lcnt_lock_list_t));
    list->head = NULL;
    list->tail = NULL;
    list->n    = 0;
    return list;
}

/* only do this on the list with the deleted locks! */
void erts_lcnt_list_clear(erts_lcnt_lock_list_t *list) {
    erts_lcnt_lock_t *lock = NULL,
		     *next = NULL;

    lock = list->head;
    
    while(lock != NULL) {
	next = lock->next;
	free(lock);
	lock = next;
    }

    list->head = NULL;
    list->tail = NULL;
    list->n    = 0;
}

void erts_lcnt_list_insert(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock) {
    erts_lcnt_lock_t *tail = NULL;

    tail = list->tail;
    if (tail) {
	tail->next = lock;
	lock->prev = tail;
	lock->next = NULL;
    } else {
	/* if the tail is null then head should be null as well. */
	list->head = lock;
	lock->prev = NULL;
    }
    list->tail = lock;
    list->n++;
}

void erts_lcnt_list_delete(erts_lcnt_lock_list_t *list, erts_lcnt_lock_t *lock) {
    
    if (lock->next) lock->next->prev = lock->prev;
    if (lock->prev) lock->prev->next = lock->next;
    if (list->head == lock) list->head = lock->next;
    if (list->tail == lock) list->tail = lock->prev;
    
    lock->prev = NULL;
    lock->next = NULL;
    list->n--;
}
/* END ASSUMPTION: lcnt_data_lock taken */


/* lock operations */

/* interface to erl_threads.h */
/* only lock on init and destroy, all others should use atomics */
void erts_lcnt_init_lock(erts_lcnt_lock_t *lock, char *name, Uint16 flag ) { 
    erts_lcnt_init_lock_extra(lock, name, flag, am_undefined);
}

void erts_lcnt_init_lock_extra(erts_lcnt_lock_t *lock, char *name, Uint16 flag, Eterm id) { 
    lcnt_lock();
    lock->next = NULL;
    lock->prev = NULL;
    lock->flag = flag;
    lock->name = name;
    lock->id = id;

    lcnt_unlock();

    ethr_atomic_init(&lock->r_state, 0);
    ethr_atomic_init(&lock->w_state, 0);
    ethr_atomic_init(&lock->tries, 0);
    ethr_atomic_init(&lock->colls, 0);

    lock->timer_n  = 0;
    lock->timer_s  = 0;
    lock->timer_ns = 0;

    lcnt_lock();
  
    erts_lcnt_list_insert(erts_lcnt_data->current_locks, lock);
    
    lcnt_unlock();
    
}

void erts_lcnt_destroy_lock(erts_lcnt_lock_t *lock) {
    erts_lcnt_lock_t *deleted_lock;
    
    /* copy structure and insert the copy */
    deleted_lock = (erts_lcnt_lock_t*)malloc(sizeof(erts_lcnt_lock_t));

    lcnt_lock();

    memcpy(deleted_lock, lock, sizeof(erts_lcnt_lock_t));
    deleted_lock->next = NULL;
    deleted_lock->prev = NULL;

    erts_lcnt_list_insert(erts_lcnt_data->deleted_locks, deleted_lock);

    /* delete original */
    erts_lcnt_list_delete(erts_lcnt_data->current_locks, lock);
    
    lcnt_unlock();
}

/* lock */

void erts_lcnt_lock_opt(erts_lcnt_lock_t *lock, Uint16 option) {
    long r_state = 0, w_state = 0;
    erts_lcnt_thread_data_t *eltd;
    
    
    ethr_atomic_inc(&lock->tries);
    ethr_atomic_read(&lock->w_state, &w_state);
    if (option & ERTS_LCNT_LO_WRITE) {
        ethr_atomic_read(&lock->r_state, &r_state);
        ethr_atomic_inc( &lock->w_state);
    }
    if (option & ERTS_LCNT_LO_READ) {
        ethr_atomic_inc( &lock->r_state);
    }
    /* we cannot acquire w_lock if either w or r are taken */
    /* we cannot acquire r_lock if w_lock is taken */	
    if ((w_state > 0) || (r_state > 0)){
       	ethr_atomic_inc(&lock->colls);
	eltd = lcnt_get_thread_data();
	lcnt_time(&eltd->timer);
	eltd->timer_set++;
	if (eltd->timer_set > 1) abort();
    }
    
}

void erts_lcnt_lock(erts_lcnt_lock_t *lock) {
    long w_state;
    erts_lcnt_thread_data_t *eltd;
    ethr_atomic_inc(&lock->tries);
    /* perhaps a lock here instead of atomic? */
    ethr_atomic_read(&lock->w_state, &w_state);
    ethr_atomic_inc( &lock->w_state);
    
    if (w_state > 0) {
	ethr_atomic_inc(&lock->colls);
	eltd = lcnt_get_thread_data();
	lcnt_time(&eltd->timer);
	eltd->timer_set++;
	if (eltd->timer_set > 1) abort();
    }

	
}

void erts_lcnt_lock_post(erts_lcnt_lock_t *lock) {
    erts_lcnt_thread_data_t *eltd;
    erts_lcnt_time_t timer;
    long ds, dns;
    
    eltd = lcnt_get_thread_data();
    if (eltd->timer_set) {
	lcnt_time(&timer);
	
	ds  = timer.s  - eltd->timer.s;
	dns = timer.ns - eltd->timer.ns;
	
	if (dns < 0) {
	    ds  -= 1;
	    dns += 1000000000;
	}
	
	eltd->timer_set--;
	
	/* has lock */
	lock->timer_n  += 1;
	lock->timer_s  += ds;
	lock->timer_ns += dns;

	if (lock->timer_ns >  1000000000) {
	    lock->timer_s  += 1;
	    lock->timer_ns -= 1000000000;
	}

	if (eltd->timer_set < 0) abort();
    }
}

/* unlock */

void erts_lcnt_unlock_opt(erts_lcnt_lock_t *lock, Uint16 option) {
    if (option & ERTS_LCNT_LO_WRITE) ethr_atomic_dec(&lock->w_state);
    if (option & ERTS_LCNT_LO_READ ) ethr_atomic_dec(&lock->r_state);
}

void erts_lcnt_unlock(erts_lcnt_lock_t *lock) {
    ethr_atomic_dec(&lock->w_state);
}

/* trylock */

void erts_lcnt_trylock_opt(erts_lcnt_lock_t *lock, int res, Uint16 option) {
    /* Determine lock_state via res instead of state */
    
    ethr_atomic_inc(&lock->tries);
    
    if (res != EBUSY) {
	if (option & ERTS_LCNT_LO_WRITE) ethr_atomic_inc(&lock->w_state);
	if (option & ERTS_LCNT_LO_READ ) ethr_atomic_inc(&lock->r_state);
    } else ethr_atomic_inc(&lock->colls);
}

    

void erts_lcnt_trylock(erts_lcnt_lock_t *lock, int res) {
    /* Determine lock_state via res instead of state */
    
    ethr_atomic_inc(&lock->tries);
    if (res != EBUSY) ethr_atomic_inc(&lock->w_state);
    else ethr_atomic_inc(&lock->colls);
}

/* thread operations */

static void lcnt_thr_init(erts_lcnt_thread_data_t *eltd) {
    void * (*function)(void *);
    void *argument;
    int res;
    function = eltd->function;
    argument = eltd->argument;
    
    res = ethr_tsd_set(lcnt_thr_data_key, eltd);
    
    function(argument);
    free(eltd);
}

    

int erts_lcnt_thr_create(ethr_tid *tid, void * (*function)(void *), void *arg, ethr_thr_opts *opts) {
    erts_lcnt_thread_data_t *eltd;
    
    eltd = lcnt_thread_data_alloc();
    
    eltd->function = function;
    eltd->argument = arg;

    return ethr_thr_create(tid, (void *)lcnt_thr_init, (void *)eltd, opts);
}


/* bindings for bifs */
/* to block or not to block or lock perhaps */


void erts_lcnt_clear_counters(void) {
    erts_lcnt_lock_t *lock;
    erts_lcnt_lock_list_t *list;

    lcnt_lock();

    list = erts_lcnt_data->current_locks;
    for (lock = list->head; lock != NULL; lock = lock->next) {
        ethr_atomic_set(&lock->tries, 0);
        ethr_atomic_set(&lock->colls, 0);
	/* clear timers */
	lock->timer_n  = 0;
	lock->timer_s  = 0;
	lock->timer_ns = 0;
    }

    /* empty deleted locks in lock list */
    erts_lcnt_list_clear(erts_lcnt_data->deleted_locks);

    lcnt_unlock();
}

erts_lcnt_data_t *erts_lcnt_get_data(void) {
    return erts_lcnt_data;
}

char *erts_lcnt_lock_type(Uint16 type) {
    return lock_type(type);
}

#endif /* ifdef ERTS_ENABLE_LOCK_COUNT */
