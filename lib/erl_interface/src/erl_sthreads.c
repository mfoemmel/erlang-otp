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
 * hooks to solaris threads functions 
 * this file will be linked if erl_init_sthreads() is called
 * see erl_init() in erl_locking.h
 */

#if !defined(VXWORKS) && !defined(__WIN32__)
#if defined (HAVE_THREAD_H) /* solaris */

#include <thread.h>
#include <synch.h>
#include <stdlib.h>

#ifdef DEBUG 
#include <stdio.h>
#endif 

extern int erl_locking_init_done;
extern void erl_common_init(void *, long);
int erl_init_sthreads(void *x, long y)
{
#ifdef DEBUG
  fprintf(stderr,"erl_interface using Solaris threads\n");
#endif

  erl_errno_key_alloc();

  erl_common_init(x,y);
  return 0; /*success */
}

mutex_t *erl_m_create(void)  
{ 
  mutex_t *l;

#ifdef DEBUG
  fprintf(stderr,"Solaris threads create\n");
#endif

  if (!erl_locking_init_done) return 0;

  if ((l = malloc(sizeof(*l)))) {
    mutex_init(l,USYNC_PROCESS,NULL);
  }

  return l;
}

int erl_m_destroy(mutex_t *l) 
{ 
  int r;
  if (!erl_locking_init_done) return 0;
  r = mutex_destroy(l);
  free(l);
  
  return r;
}

int erl_m_lock(mutex_t *l)    
{ 
  if (!erl_locking_init_done) return 0;
  return mutex_lock(l);
}

int erl_m_trylock(mutex_t *l) 
{ 
  if (!erl_locking_init_done) return 0;
  return mutex_trylock(l);
}

int erl_m_unlock(void *l)  
{ 
  if (!erl_locking_init_done) return 0;
  return mutex_unlock(l);
} 


/*
 * Thread-specific erl_errno variable.
 */

static thread_key_t erl_errno_key;

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
    thr_keycreate(&erl_errno_key, erl_errno_destroy);
}

/*
 * Allocate (and initialize) per-thread erl_errno locus.
 */
static void
erl_errno_alloc (void)
{
    int * locus;
    thr_setspecific(erl_errno_key, malloc(sizeof(__erl_errno)));
    thr_getspecific(erl_errno_key, &locus); *locus = 0;
}

/*
 * Return a pointer to the erl_errno locus.
 */
volatile int *
__erl_errno_place (void)
{
    int * locus;
    thr_getspecific(erl_errno_key, &locus);
    if (locus == NULL)
    {
	erl_errno_alloc();
	thr_getspecific(erl_errno_key, &locus);
    }
    return locus;
}

#endif /* THREAD_H */
#endif /* !VXWORKS && !__WIN32__ */

