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
#ifndef ERL_EPMD_H
#define ERL_EPMD_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef INADDR_LOOPBACK  
#define INADDR_LOOPBACK ((u_long) 0x7F000001)
#endif

#ifdef __WIN32__
#define readsocket(fd,buf,len) recv(fd,buf,len,0)
#define writesocket(fd,buf,len) send(fd,buf,len,0)
#else
#define readsocket read
#define writesocket write
#define closesocket close
#endif

/* epmd r3 protocol */
#ifndef ERL_EPMD_ALIVE_REQ
#define ERL_EPMD_ALIVE_REQ     'a'
#define ERL_EPMD_ALIVE_OK_RESP 'Y'
#define ERL_EPMD_PORT_REQ      'p'
#define ERL_EPMD_STOP_REQ      's'
#endif

/* epmd r4 */
#ifndef ERL_EPMD_ALIVE2_REQ
#define ERL_EPMD_ALIVE2_REQ  120
#define ERL_EPMD_ALIVE2_RESP 121
#define ERL_EPMD_PORT2_REQ   122
#define ERL_EPMD_PORT2_RESP  119
#endif

/* the exported functions */
/* erl_publish returns open descriptor on success, or -1 */
extern int erl_publish(int port);

/* returns 0 on success, -1 if node not known to epmd or epmd not reached */
extern int erl_unpublish(const char *alive);

/* internal functions */
extern int erl_epmd_connect(struct in_addr *inaddr);
extern int erl_epmd_publish(int port, const char *alive);
extern int erl_epmd_port(struct in_addr *inaddr, const char *alive, int *dist);

#ifdef __cplusplus
}
#endif



#endif
