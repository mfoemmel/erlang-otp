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
#ifndef ERL_CONFIG_H
#define ERL_CONFIG_H

#ifndef ERL_MYPROTO
#define ERL_MYPROTO 0 /* tcp/ip */
#endif

#ifndef ERL_DIST_HIGH
#define ERL_DIST_HIGH 5 /* R4 and later */
#define ERL_DIST_LOW  1 /* R3 and earlier */
#endif

#ifndef EPMD_PORT
#define EPMD_PORT 4369
#endif

#endif
