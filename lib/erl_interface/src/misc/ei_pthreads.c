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

/* FIXME why not use ei_malloc here? */

#include "eidef.h"

#include <stdlib.h>
#include "ei.h"
#include "ei_locking.h"

/* Define (and initialize) the variable __erl_errno */
volatile __declspec(thread) int __erl_errno = 0;

#if defined(VXWORKS)

/* 
   Moved to each of the erl_*threads.c files, as they seem to know how
   to get thread-safety. 
*/

volatile int *__erl_errno_place(void)
{
    /* This check is somewhat insufficient, double task var entries will occur
       if __erl_errno is actually -1, which on the other hand is an invalid 
       error code. */
    if (taskVarGet(taskIdSelf(), &__erl_errno) == ERROR) {
	taskVarAdd(taskIdSelf(), &__erl_errno);
    }
    return &__erl_errno;
}
#endif /* VXWORKS */

#if defined(__WIN32__)

volatile int *__erl_errno_place(void)
{
    return &__erl_errno;
}

#endif /* __WIN32__ */

#if defined(_REENTRANT) && !defined(VXWORKS) && !defined(__WIN32__)

#if defined(HAVE_PTHREAD_H) || defined(HAVE_MIT_PTHREAD_H)

void *ei_m_create(void)  
{ 
  pthread_mutex_t *l;
 
  if ((l = malloc(sizeof(*l)))) { /* FIXME get memory or abort */
    pthread_mutex_init(l,NULL);
  }

  return l;
}

int ei_m_destroy(void *l) 
{ 
  int r = pthread_mutex_destroy(l);
  free(l);
  
  return r;
}

int ei_m_lock(void *l)    
{ 
  return pthread_mutex_lock(l);
}

int ei_m_trylock(void *l) 
{ 
  return pthread_mutex_trylock(l);
}

int ei_m_unlock(void *l)  
{ 
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
static void erl_errno_destroy(void * ptr)
{
    free(ptr);
}

/*
 * Allocate erl_errno key.
 * This will be done once for all threads
 */
static void erl_errno_key_alloc(void)
{
    pthread_key_create(&erl_errno_key, erl_errno_destroy);
}

/*
 * Return a pointer to the erl_errno locus.
 * If pthread functions fail we fall back to using __erl_errno
 * so that the main thread (actually not a thread in all ascpects)
 * still will set and get an erl_errno value.
 * FIXME is this a bit too nice???
 * If -lpthread is not given on Solaris __erl_errno will be used
 * but it costs some....,
 */
volatile int *__erl_errno_place(void)
{
    int *erl_errno_p;

    /* This will create the key once for all threads */
    if (pthread_once(&erl_errno_key_once, erl_errno_key_alloc) != 0)
	return &__erl_errno;

    /* This is the normal case, return the pointer to the data */
    if ((erl_errno_p = pthread_getspecific(erl_errno_key)) != NULL)
	return erl_errno_p;

    /* Case where it is the first time we access this data in this thread. */
    /* FIXME check malloc but what to do????? */
    erl_errno_p = malloc(sizeof(int));

    if (pthread_setspecific(erl_errno_key, erl_errno_p) != 0 ||
	(erl_errno_p = pthread_getspecific(erl_errno_key)) == NULL) {
	free(erl_errno_p);
	return &__erl_errno;
    }

    return erl_errno_p;
}

#endif /* HAVE_PTHREAD_H || HAVE_MIT_PTHREAD_H */

#endif /* _REENTRANT && !VXWORKS && !__WIN32__ */
