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
#ifdef __WIN32__
#include <winsock2.h>
#include <windows.h>
#include <winbase.h>

#elif  VXWORKS
#include <vxWorks.h>
#include <ifLib.h>
#include <sockLib.h>
#include <inetLib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#endif

#include <stdlib.h>
#include <string.h>

#include "eidef.h"
#include "ei_internal.h"
#include "putget.h"
#include "erl_interface.h"
#include "ei_epmd.h"


int ei_unpublish(ei_cnode* ec)
{
    return ei_unpublish_alive(ei_thisalivename(ec)); /* FIXME check error */
}


/* stop the specified node */
int ei_unpublish_alive(const char *alive)
{
    unsigned char buf[EPMDBUF];
    char *s = (char *)buf;
    int len = 1 + strlen(alive);
    int fd;

    put16be(s,len);
    put8(s,EI_EPMD_STOP_REQ);
    strcpy(s, alive);

    if ((fd = ei_epmd_connect(NULL)) < 0) return fd;

    if (writesocket(fd, buf, len+2) != len+2) {
	closesocket(fd);
	return -1;
    }

    EI_TRACE_CONN1("ei_unpublish_alive","-> STOP %s",alive);
  
    if (readsocket(fd, buf, 7) != 7) {
	closesocket(fd);
	return -1; 
    }
    closesocket(fd);
    buf[7]=(char)0;		/* terminate the string */
  
    if (!strcmp("STOPPED",(char *)buf)) {
	EI_TRACE_CONN0("ei_unpublish_alive","<- STOPPED (success)");
	return 0;
    }
    else if (!strcmp("NOEXIST",(char *)buf)) {
	EI_TRACE_ERR0("ei_unpublish_alive","<- NOEXIST (failure)");
	return -1;
    }
    else {
	EI_TRACE_ERR0("ei_unpublish_alive","<- unknown (failure)");
	return -1;		/* this shouldn't happen */
    }
    return 0;
}
