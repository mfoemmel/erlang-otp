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
#ifndef _ERL_START_H
#define _ERL_START_H

#ifdef __cplusplus
extern "C" {
#endif


#define ERL_START_MSG "gurka" /* make something up */
#define ERL_START_TIME 10000   /* wait this long (ms) */
#define ERL_START_LOGFILE ".erl_start.out" /* basename of logfile */

/* flags used by erl_connect and erl_xconnect */
#define ERL_START_ENODE   0x0001
#define ERL_START_EPMD    0x0002
#define ERL_START_LONG    0x0004
#define ERL_START_COOKIE  0x0008
#define ERL_START_DEBUG   0x0010
#define ERL_START_VERBOSE 0x0020
#define ERL_START_REMOTE  0x0040

/* error return values */
#define ERL_S_TIMEOUT    -51  /* a timeout occurred */
#define ERL_BADARG     -52  /* an argument contained an incorrect value */
#define ERL_SYS_ERROR  -99  /* a system error occurred (check errno) */

/* start an erlang system */
extern int erl_start_sys(char *alive, Erl_IpAddr addr, int flags, char *erl, char *add_args[]);

/* start epmd */
extern int erl_start_epmd(void);

#ifdef __cplusplus
}
#endif

#endif /* _ERL_START_H */

