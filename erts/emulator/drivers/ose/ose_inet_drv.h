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
#ifndef __OSE_INET_DRV_H__
#define __OSE_INET_DRV_H__

#define MAX_SOCKS 1024

struct servent {
  char  *s_name;		/* official name of service */
  char  **s_aliases;		/* alias list */
  int   s_port;			/* port service resides at */
  char  *s_proto;		/* protocol to use */
};

struct iovec {
  void	*iov_base;
  int    iov_len;
};

/* this is the select function for sockets to be used by the inet driver,
   note that driver_select in sys.c handles signals rather than fds */
int ose_inet_select(ErlDrvPort port, ErlDrvEvent event, int mode, int on);

void add_ose_inet_drv_entry();
void add_ose_tcp_drv_entry(ErlDrvEntry *tcp_inet_driver_entry);
void add_ose_udp_drv_entry(ErlDrvEntry *udp_inet_driver_entry);

int ose_inet_accept(int s, struct sockaddr *addr, int *len);
int ose_inet_send(int s, const void *msg, int len, int flags);
int ose_inet_sendto(int s, const void *msg, int len, int flags, 
			const struct sockaddr *to, int tolen);
int ose_inet_sendv(int sock, SysIOVec *iov, int iovcnt);
int ose_inet_socket(int af, int type, int proto);
int ose_inet_close(int s);
int ose_gethostname(char *name, int namelen);

struct servent *ose_getservbyname(const char *name,  const  char *proto);
struct servent *ose_getservbyport(int port, const char *proto);

#endif

