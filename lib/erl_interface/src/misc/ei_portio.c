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

#elif VXWORKS
#include <sys/types.h>
#include <ioLib.h>
#include <unistd.h>

#else /* other unix */
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#endif

/* common includes */
#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include "ei_portio.h"
#include "ei_internal.h"

/* 
 * Fill buffer, return buffer length, 0 for EOF, < 0 (and sets errno)
 * for error.  */
int ei_read_fill(int fd, char* buf, int len)
{
  int i,got=0;
  
  while (got < len) {
    i = readsocket(fd, buf+got, len-got);
    if (i <= 0)
      return (i);
    got += i;
  }
  return (len);
}


/* write entire buffer on fd  or fail (setting errno)
 */
int ei_write_fill (int fd, char *buf, int len)
{
  int i,done=0;
  
  while (done < len) {
    i = writesocket(fd, buf+done, len-done);
    if (i <= 0)
      return (i);
    done += i;
  }
  return (len);
}
