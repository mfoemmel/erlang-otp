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
#ifndef _ERL_CONNECT_H
#define _ERL_CONNECT_H

#include "erl_eterm.h"
#include "ei.h"
#ifdef __cplusplus
extern "C" {
#endif

/* Return values */
#define ERL_ERROR -1           /* Error of some kind */
#define ERL_NO_DAEMON -2       /* No contact with EPMD */
#define ERL_NO_PORT -3         /* No port received from EPMD */   
#define ERL_CONNECT_FAIL -4    /* Connect to Erlang Node failed */
#define ERL_TIMEOUT -5         /* A timeout has expired */
#define ERL_NO_REMOTE -6       /* Cannot execute rsh */

#define ERL_TICK 0    
#define ERL_MSG 1    

#define ERL_NO_TIMEOUT -1


#ifndef MAXHOSTLEN
#define MAXHOSTLEN 255
#endif
#ifndef MAXALIVELEN
#define MAXALIVELEN 63
#endif
#define MAXNODELEN MAXHOSTLEN+1+MAXALIVELEN+1

#ifndef MAXREGLEN 
#define MAXREGLEN 255  /* max length of registered (atom) name */
#endif

/* Should be a (struct in_addr *) but we
 * don't want to include socket.h here... 
 */
/* typedef char *Erl_IpAddr;  */
typedef struct in_addr *Erl_IpAddr; 

typedef struct {
  char ipadr[4];             /* stored in network byte order */
  char nodename[MAXNODELEN+1];
} ErlConnect;

#include "eiext.h" /* for message types */

#if (0) /* the old one */

typedef struct {
  char type;               /* e.g ERL_SEND */
  ETERM *msg;              /* the message */
  ETERM *pid; /* <ToPid> when ERL_SEND, <FromPid> when ERL_REG_SEND */
  char to_name[MAXREGLEN+1]; /* registered name, valid only when ERL_REG_SEND */ 
} ErlMessage;

#else /* the new one */

typedef struct {
  int type;   /* one of the message type constants in eiext.h */
  ETERM *msg; /* the actual message */
  ETERM *from;
  ETERM *to;
  char to_name[MAXREGLEN+1];
} ErlMessage;

#endif

extern int erl_connect_init(int, char*,short);
extern int erl_connect_xinit(char*,char*,char*,Erl_IpAddr,char*,short);
extern int erl_connect(char*); 
extern int erl_xconnect(Erl_IpAddr,char *);
extern int erl_close_connection(int);

extern int erl_send(int, ETERM*, ETERM*);
extern int erl_reg_send(int, char*, ETERM*);
extern int erl_receive(int, unsigned char*, int);
extern int erl_receive_msg(int, unsigned char*, int, ErlMessage*);
extern int erl_xreceive_msg(int, unsigned char**, int*, ErlMessage*);

extern ETERM *erl_rpc(int,char*,char*,ETERM*);
extern int erl_rpc_to(int,char*,char*,ETERM*);
extern int erl_rpc_from(int,int,ErlMessage*);

extern int erl_accept(int,ErlConnect*);

extern const char *erl_thisnodename(void);
extern const char *erl_thishostname(void);
extern const char *erl_thisalivename(void);
extern short erl_thiscreation(void);
extern const char *erl_thiscookie(void);
extern erlang_pid *erl_self(void);
extern void erl_set_thiscreation(short);
extern int erl_distversion(int fd);

#ifdef __cplusplus
}
#endif

#endif
