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
 * hooks to pthreads functions
 * this file will be linked if erl_init_pthreads() is called
 * see erl_init() in erl_locking.h
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if !defined(VXWORKS) && !defined(__WIN32__)
#if defined HAVE_PTHREAD_H || defined HAVE_MIT_PTHREAD_H

#ifdef HAVE_MIT_PTHREAD_H /* linux */
#include <pthread/mit/pthread.h>
#else
#include <pthread.h>
#endif

#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

#include "erl_error.h"
#include "ei_connect.h"

extern int erl_locking_init_done;

static pthread_key_t erl_errno_key;

void ei_init_pthreads()
{
    ei_init();
#ifdef DEBUG
    fprintf(stderr,"erl_interface using POSIX threads\n");
#endif
}

pthread_mutex_t *erl_m_create(void)  
{ 
  pthread_mutex_t *l;
 
#ifdef DEBUG
  fprintf(stderr,"POSIX threads create\n");
#endif

  if (!erl_locking_init_done) return 0;

  if ((l = malloc(sizeof(*l)))) {
    pthread_mutex_init(l,NULL);
  }

  return l;
}

int erl_m_destroy(pthread_mutex_t *l) 
{ 
  int r;
  if (!erl_locking_init_done) return 0;
  r = pthread_mutex_destroy(l);
  free(l);
  
  return r;
}

int erl_m_lock(pthread_mutex_t *l)    
{ 
  if (!erl_locking_init_done) return 0;
  return pthread_mutex_lock(l);
}

int erl_m_trylock(pthread_mutex_t *l) 
{ 
  if (!erl_locking_init_done) return 0;
  return pthread_mutex_trylock(l);
}

int erl_m_unlock(void *l)  
{ 
  if (!erl_locking_init_done) return 0;
  return pthread_mutex_unlock(l);
} 


/*
 * Thread-specific erl_errno variable.
 *
 * The second line below will give a "missing braces around initializer"
 * on Solaris but the code will work.
 */

static pthread_key_t erl_errno_key;
static pthread_once_t erl_errno_key_once = PTHREAD_ONCE_INIT;

/*
 * Destroy per-thread erl_errno locus
 */
static void
erl_errno_destroy (void * ptr)
{
    free(ptr);
}

/*
 * Allocate erl_errno key.
 */
static void
erl_errno_key_alloc (void)
{
    pthread_key_create(&erl_errno_key, erl_errno_destroy);
}

/*
 * Allocate (and initialize) per-thread erl_errno locus.
 */
static void
erl_errno_alloc (void)
{
    pthread_setspecific(erl_errno_key, malloc(sizeof(__erl_errno)));
    *(int *)pthread_getspecific(erl_errno_key) = 0;
}

/*
 * Return a pointer to the erl_errno locus.
 */
volatile int *
__erl_errno_place (void)
{
    pthread_once(&erl_errno_key_once, erl_errno_key_alloc);
    if (pthread_getspecific(erl_errno_key) == NULL)
    {
	erl_errno_alloc();
    }
    return (int *)pthread_getspecific(erl_errno_key);
}

#endif /* HAVE_PTHREAD_H || HAVE_MIT_PTHREAD_H */
#endif /* !VXWORKS && !__WIN32__ */
