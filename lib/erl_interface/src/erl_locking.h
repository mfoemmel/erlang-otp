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
#ifndef _ERL_LOCKING_H
#define _ERL_LOCKING_H

#ifdef __WIN32__
#include <windows.h>
#include <winbase.h>

#elif VXWORKS
#include <vxWorks.h>
#include <semLib.h>

#else /* unix */

#ifdef __cplusplus
extern "C" {
#endif


#ifdef _REENTRANT
#ifdef PTHREADS
extern int erl_init_pthreads(void *x, long y);
#define erl_init(x,y) erl_init_pthreads(x,y)

#elif STHREADS
extern int erl_init_sthreads(void *x, long y);
#define erl_init(x,y) erl_init_sthreads(x,y)

#else /* unspecified threads */
extern int erl_init_pthreads(void *x, long y);
#define erl_init(x,y) erl_init_pthreads(x,y)

#endif /* threads */
#endif /* _REENTRANT */
#endif  /* unix */

#ifndef erl_init
extern void erl_init_nothreads(void *x, long y);
#define erl_init(x,y) erl_init_nothreads(x,y)
#endif /* erl_init */

typedef struct erl_mutex_s {
#ifdef __WIN32__
  HANDLE lock;

#elif VXWORKS
  SEM_ID lock;

#else /* unix */
  void *lock;
#endif 
} erl_mutex_t;

extern int erl_init_locking(void);
extern erl_mutex_t *erl_mutex_create(void);
extern int erl_mutex_free(erl_mutex_t *l, int nblock);
extern int erl_mutex_lock(erl_mutex_t *l, int nblock);
extern int erl_mutex_unlock(erl_mutex_t *l);

#ifdef __cplusplus
}
#endif

#endif /* _ERL_LOCKING_H */
