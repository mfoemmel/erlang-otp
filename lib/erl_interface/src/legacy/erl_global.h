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
#ifndef _ERL_GLOBAL_H
#define _ERL_GLOBAL_H

char **erl_global_names(int fd, int *count);
ETERM *erl_global_whereis(int fd, const char *name, char *node);
int erl_global_register(int fd, const char *name, ETERM *pid);
int erl_global_unregister(int fd, const char *name);

#endif /* _ERL_GLOBAL_H */
