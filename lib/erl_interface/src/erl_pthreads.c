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

extern void ei_init_pthreads();
extern void erl_common_init(void *, long);

int erl_init_pthreads (void *x, long y)
{
    ei_init_pthreads();
    erl_common_init(x,y);
    return 0; /*success */
}


#endif /* HAVE_PTHREAD_H || HAVE_MIT_PTHREAD_H */
#endif /* !VXWORKS && !__WIN32__ */
