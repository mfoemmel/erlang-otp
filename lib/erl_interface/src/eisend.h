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
#ifndef EISEND_H
#define EISEND_H

#include "eicode.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int ei_send_link(int fd, const erlang_pid *from, const erlang_pid *to);
extern int ei_send_unlink(int fd, const erlang_pid *from, const erlang_pid *to);
erlang_pid *ei_whereis(int fd, const char *name);

extern int ei_send_exit(int fd, const erlang_pid *from, const erlang_pid *to, const char *msg);
extern int ei_send_encoded(int fd, const erlang_pid *to, const char *msg, int msglen);
extern int ei_send_reg_encoded(int fd, const erlang_pid *from, const char *to, const char *msg, int msglen);

/* bufp = address of pointer to dynamically allocated buffer - may be reallocated by
 * this function if it is too small for the message
 * bufsz = in/out value: in=user buffer size, out=new buffer size
 * msglen = nr bytes in received message
 */
extern int ei_receive_encoded(int fd, char **bufp, int *bufsz, erlang_msg *to, int *msglen);

#ifdef __cplusplus
}
#endif

#endif
