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

#ifdef __WIN32__
#  include <windows.h>
#else
#  include <sys/types.h>
#  include <sys/param.h>
#  ifdef SYS_SELECT_H
#    include <sys/select.h>
#  endif
#  if TIME_WITH_SYS_TIME
#     include <sys/time.h>
#     include <time.h>
#  else
#     if HAVE_SYS_TIME_H
#         include <sys/time.h>
#     else
#         include <time.h>
#     endif
#  endif
#  include <string.h>
#  ifdef HAVE_UNISTD_H
#    include <unistd.h>
#  endif
#  if (defined(NO_SYSCONF) || !defined(_SC_NPROCESSORS_CONF))
#    ifdef HAVE_SYS_SYSCTL_H
#      include <sys/sysctl.h>
#    endif
#  endif
#  include <errno.h>
#endif

#include "erl_misc_utils.h"

int
erts_no_of_cpus(void)
{
    int ncpus;
#ifdef __WIN32__
    SYSTEM_INFO sys_info;
    GetSystemInfo(&sys_info);
    ncpus = (int) sys_info.dwNumberOfProcessors;
#elif !defined(NO_SYSCONF) && defined(_SC_NPROCESSORS_CONF)
    ncpus = (int) sysconf(_SC_NPROCESSORS_CONF);
#elif defined(HAVE_SYS_SYSCTL_H) && defined(CTL_HW) && defined(HW_NCPU)
    {
	int mib[2] = {CTL_HW, HW_NCPU};
	size_t ncpus_len = sizeof(int);
	if (sysctl(&mib[0], 2, &ncpus, &ncpus_len, NULL, 0) < 0)
	    ncpus = -1;
    }
#else
    ncpus = -1;
#endif
    return ncpus;
}

int
erts_milli_sleep(long ms)
{
    if (ms > 0) {
#ifdef __WIN32__
	Sleep((DWORD) ms);
#else
	struct timeval tv;
	tv.tv_sec = ms / 1000;
	tv.tv_usec = (ms % 1000) * 1000;
	if (select(0, NULL, NULL, NULL, &tv) < 0)
	    return errno == EINTR ? 1 : -1;
#endif
    }
    return 0;
}
