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


#ifndef __ERL_INET_SIG__
#define __ERL_INET_SIG__

#define PID_SIG           (1)
#define SOCK_SELECT       (2)
#define INET_EVENT_WRITE  (3)
#define INET_EVENT_ACK    (4)
#define SOCK_SELECT_ERROR (5)
#define PORT_INFO         (6)

struct PidSig {
  SIGSELECT sig_no;
  PROCESS pid_;
};

/* we use the OSE INET_EVENT_READ signal so that we can
   receive ose_inet indications as well */

struct InetEventWrite {
  SIGSELECT sig_no;
  int sock;
};

struct InetEventAck {
  SIGSELECT sig_no;
  int sock;
};

struct SockSelect {
  SIGSELECT sig_no;
  int sock;
  int mode;
  int on;
};

struct SockSelectError {
  SIGSELECT sig_no;
  int sock;
  int error;
};

struct PortInfo {
  SIGSELECT sig_no;
  int port;
};

#endif /* __ERL_INET_SIG__ */
