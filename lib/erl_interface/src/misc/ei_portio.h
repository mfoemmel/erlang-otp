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
#ifndef _EI_PORTIO_H
#define _EI_PORTIO_H

int ei_accept_t(int fd, void   *addr,   void  *addrlen, unsigned ms);
int ei_connect_t(int fd, void *sinp, int sin_siz, unsigned ms);
int ei_read_fill(int fd, char* buf, int len);
int ei_write_fill(int fd, char *buf, int len);
int ei_read_fill_t(int fd, char* buf, int len, unsigned ms);
int ei_write_fill_t(int fd, char *buf, int len, unsigned ms);
#ifdef HAVE_WRITEV
int ei_writev_fill_t(int fd,  const  struct  iovec  *iov,  int iovcnt,
		      unsigned ms);
#endif

#endif /* _EI_PORTIO_H */
