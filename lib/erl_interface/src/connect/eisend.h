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
#ifndef _EISEND_H
#define _EISEND_H

/* FIXME strange, is this for debugging?! */
#define EI_HAVE_TIMEOUT 1

int ei_send_exit(int fd, const erlang_pid *from, const erlang_pid *to, 
		 const char *msg);
int ei_send_exit_tmo(int fd, const erlang_pid *from, 
		     const erlang_pid *to, 
		     const char *msg, unsigned ms);

/* FIXME ei_send_*() functions not used */
#if 0
int ei_send_link(int fd, const erlang_pid *from, const erlang_pid *to);
int ei_send_unlink(int fd, const erlang_pid *from, const erlang_pid *to);
int ei_send_link_tmo(int fd, const erlang_pid *from, 
		     const erlang_pid *to, unsigned ms);
int ei_send_unlink_tmo(int fd, const erlang_pid *from, 
		       const erlang_pid *to, unsigned ms);
#endif /* Not used */

#endif /* _EISEND_H */
