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

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#if (defined(NO_SYSCONF) || !defined(_SC_NPROCESSORS_CONF)) \
    && defined(HAVE_SYS_SYSCTL_H)
#include <sys/types.h> /* Needed on Darwin */
#include <sys/param.h> /* Needed on OpenBSD */
#include <sys/sysctl.h>
#endif

#include "erl_misc_utils.h"

int
erts_no_of_cpus(void)
{
    int ncpus;
#if !defined(NO_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
    ncpus = (int) sysconf(_SC_NPROCESSORS_CONF);
#elif defined(HAVE_SYS_SYSCTL_H) && defined(CTL_HW) && defined(HW_NCPU)
    {
	int mib[2] = {CTL_HW, HW_NCPU};
	size_t ncpus_len = sizeof(int);
	if (sysctl(&mib[0], 2, &ncpus, &ncpus_len, NULL, 0) < 0)
	    ncpus = 1;
    }
#else
    ncpus = 1;
#endif
    return ncpus;
}

