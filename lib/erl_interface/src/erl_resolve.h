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
#ifndef _ERL_RESOLVE_H
#define _ERL_RESOLVE_H

#ifdef __cplusplus
extern "C" {
#endif



#ifdef VXWORKS
/* we need these definitions - if the user has SENS then he gets them
 * from netdb.h, otherwise we define them ourselves.
 */

/* if you are getting "multiple definition" errors here,
 * make sure you have included <netdb.h> BEFORE "erl_interface.h"
 * or define HAVE_SENS in your CFLAGS.
 */
 
#ifndef HAVE_SENS
#ifndef HOST_NOT_FOUND /* just in case */

struct	hostent {
  char	*h_name;	/* official name of host */
  char	**h_aliases;	/* alias list */
  int	h_addrtype;	/* host address type */
  int	h_length;	/* length of address */
  char	**h_addr_list;	/* list of addresses from name server */
#define	h_addr	h_addr_list[0]	/* address, for backward compatiblity */
  unsigned int unused;  /* SENS defines this as ttl */
};

#define	HOST_NOT_FOUND	1 /* Authoritative Answer Host not found */
#define	TRY_AGAIN	2 /* Non-Authoritive Host not found, or SERVERFAIL */
#define	NO_RECOVERY	3 /* Non recoverable errors, FORMERR, REFUSED, NOTIMP */
#define	NO_DATA		4 /* Valid name, no data record of requested type */
#define	NO_ADDRESS	NO_DATA		/* no address, look for MX record */

#endif /* HOST_NOT_FOUND */
#endif /* SENS */

extern int h_errno;

#endif /* VXWORKS */

struct hostent *erl_gethostbyname(const char *);
struct hostent *erl_gethostbyaddr(const char *, int, int);

#if !defined (__WIN32__)
struct hostent *erl_gethostbyname_r(const char *, struct hostent *, char *, int, int *);
struct hostent *erl_gethostbyaddr_r(const char *, int, int, struct hostent *, char *, int, int *);
#endif /* !__WIN32__ */

#ifdef __cplusplus
}
#endif

#endif /* _ERL_RESOLVE_H */
