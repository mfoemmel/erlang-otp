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
#ifndef _ERL_ERROR_H
#define _ERL_ERROR_H

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __STDC__
extern void erl_err_sys(const char*, ... );
extern void erl_err_quit(const char*, ... );
extern void erl_err_msg(const char*, ... );
#else
extern void erl_err_sys();
extern void erl_err_quit();
extern void erl_err_msg();
#endif

#ifdef __cplusplus
}
#endif

#endif
