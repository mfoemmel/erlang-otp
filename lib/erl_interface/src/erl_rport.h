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
#ifndef _ERL_RPORT_H
#define _ERL_RPORT_H

#ifdef __cplusplus
extern "C" {
#endif


#define ERL_RPORT_OK 0
#define ERL_RPORT_ERROR -1
#define ERL_RPORT_EOF -2

extern int erl_read_fill(int fd, char* buf, int len);
extern int erl_write_fill(int fd, char *buf, int len);
extern int erl_tbh_write(int fd, char *buf,int len);
extern int erl_tbh_read(int fd);

#ifdef __cplusplus
}
#endif

#endif
