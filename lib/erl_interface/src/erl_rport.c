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
#include "erl_rport.h"
#include "erl_interface.h"

#if defined(HAVE_WRITEV) && defined(SUNOS4)
extern int writev();
#endif

int erl_read_fill(int,char*,int);
int erl_write_fill(int,char*,int);

/* Fill buffer, return buffer length, 0 for EOF, < 0 for error. 
 */
int erl_read_fill(int fd, char* buf, int len)
{
  int i,got=0;
  
  do {
    i = readsocket(fd, buf+got, len-got);
    if (i <= 0)
      return (i);
    got += i;
  } while (got < len);
  return (len);

} /* read_fill */

/* write entire buffer on fd  or fail
 */
int erl_write_fill(int fd, char *buf, int len)
{
  int i,done=0;
  
  do {
    i = writesocket(fd, buf+done, len-done);
    if (i <= 0)
      return (i);
    done += i;
  } while (done < len);
  return (len);
}
/*
 * Whenever a program sits in the other end of an Erlang open_port
 * command that program has its stdio connected to the Erlang-port
 * When data comes from erlang that data is always prepended with a
 * a two byte header in binary integer format msb first.
 * Theese routines provide a way to read and write data to Erlang
 * without having to worry about the two byte headers
 * 
 */

/*
 *  This routine writes buf to an Erlang port in one single write 
 *  The writev does this
 *  Returns: ERL_RPORT_OK when successful, and ERL_RPORT_ERROR on failure
 */
#ifdef HAVE_WRITEV

int erl_tbh_write(int fd, char *buf,int len)
{
  unsigned char lenbuf[2];
  struct iovec vector[2];
  
  if (len > 0xffff) return(ERL_RPORT_ERROR);

  lenbuf[1] = len & 0xff;
  lenbuf[0] = (len >>8) & 0xff;

  vector[0].iov_base = (char*) lenbuf;
  vector[0].iov_len = 2;
  vector[1].iov_base =  buf;
  vector[1].iov_len = len;
  
  if (writev(fd,&vector[0],2) != len + 2) 
    return (ERL_RPORT_ERROR);
  else
    return (ERL_RPORT_OK);
  
} /* tbh_write */

#else

/* And here comes a mongolid version for systems that 
 * don't have writev. We simply do two writes.
 */
int erl_tbh_write(int fd,char *buf,int len) 
{
  unsigned char lenbuf[2];

  if (len > 0xffff) return(ERL_RPORT_ERROR);

  lenbuf[1] = len & 0xff;
  lenbuf[0] = (len >>8) & 0xff;

  if ((erl_write_fill(fd,lenbuf,2) < 0) || (erl_write_fill(fd,buf,len) < 0))
    return (ERL_RPORT_ERROR);
  else 
    return (ERL_RPORT_OK);
} /* tbh_write */

#endif


/* Returns an integer that indicates the number of bytes waiting to be
 * read. To actually read the data, it is (now) the caller's
 * resposibility to call erl_read_fill with a sufficiently large
 * buffer after calling this function to determine the size of buffer
 * needed. 
 */
int erl_tbh_read(int fd) 
{
  unsigned char lenbuf[2];
  int len;

  erl_read_fill(fd,lenbuf,2);
  len = (lenbuf[0] << 8) | lenbuf[1];

  return len;
}

