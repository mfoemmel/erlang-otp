/**
 * ``The contents of this file are subject to the Erlang Public License,
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
 *
 */
#include "inet.h"

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif


/*
 * Fills in the struct sockaddr_in with the 
 * appropriate internet address and port number
 * for a requested service. If 'proto' parameter
 * is null, then TCP is used. Serv may be an ascii
 * name or a port number.
 * The function returns 0 on error, else 1.
 */
int setsockaddr_in(name, host, serv, proto)
     struct sockaddr_in *name;
     char* host;
     char* serv;
     char *proto;
     
{  
  struct servent* s;
  struct hostent* h;
  
  bzero(name, sizeof(struct sockaddr_in));
  
  if (proto == 0)
    proto = "tcp";
  
  name->sin_family = htons(AF_INET);
  
  if (host == 0)
    name->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  else
    if (isdigit(*host))
      name->sin_addr.s_addr = inet_addr(host);
    else
      if ((h = gethostbyname(host)) == NULL)
	return 0;
  
  bcopy(h->h_addr, (char *) &(name->sin_addr.s_addr), h->h_length);
  
  if (isdigit(*serv))
     name->sin_port = atoi(serv);
  else
  {
     if ((s = getservbyname(serv, proto)) == NULL)
	return 0;
     
     name->sin_port = s->s_port;
  }
  
  return 1;
}
