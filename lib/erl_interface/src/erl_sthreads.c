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
extern void ei_init_sthreads();
extern void erl_common_init(void *, long);
int erl_init_sthreads(void *x, long y)
{
    ei_init_sthreads();
    erl_common_init(x,y);
    return 0; /*success */
}

#endif /* HAVE_THREAD_H */
#endif /* !VXWORKS && !__WIN32__ */

