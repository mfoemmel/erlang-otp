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
/*
 * Purpose: Header file for adaptions to various SSL packages.
 */

#ifndef ESOCK_SSL_H
#define ESOCK_SSL_H

#include <sys/types.h>
#include <stdio.h>
#include "esock.h"

/* Error string to be set by certain functions (see below) */

char *esock_ssl_err_str;

/* Initialization and finalization of SSL */

int esock_ssl_init(void);
void esock_ssl_finish(void);

/* Freeing of SSL resources for a connection */

void esock_ssl_free(Connection *cp);

/* Setting and reading of fd_set masks */
/* XXX Maybe this should be moved to esock.c, since they do not
 * (yet) depend on any specific SSL feature or package. */

int esock_ssl_set_masks(Connection *cp, fd_set *rfds, fd_set *wfds, 
			fd_set *efds, int verbose);
Connection *esock_ssl_read_masks(Connection *cp, Connection **cpnext, 
				 fd_set *rfds, fd_set *wfds, fd_set *efds, 
				 int set_wq_fds);

/* Print error diagnostics to a file pointer */

void esock_ssl_print_errors_fp(FILE *fp);

/* All functions below have to return >= 0 on success, and < 0 on 
 * failure. 
 * 
 * If the return indicates a failure (return value < 0) and the failure
 * is temporary the error context (sock_errno()/sock_set_errno()) must
 * be set to ERRNO_BLOCK. 
 *
 * If the failure is permanent, the error context must be set to something
 * else than ERRNO_BLOCK, and `esock_ssl_err_str' must be set to point to
 * short diagnostic string describing the error.
 */

int esock_ssl_accept_init(Connection *cp);
int esock_ssl_connect_init(Connection *cp);
int esock_ssl_listen_init(Connection *cp);

/* All functions below may involve non-blocking I/O with a temporary
 * failure.  Hence they have to have the error context set to
 * ERRNO_BLOCK, or else have esock_ssl_err_str set to point to a
 * diagnostic string, in case the return value is < 0.  
 */

int esock_ssl_accept(Connection *cp);
int esock_ssl_connect(Connection *cp);

int esock_ssl_read(Connection *cp, char *buf, int len);
int esock_ssl_write(Connection *cp, char *buf, int len);

int esock_ssl_close(Connection *cp);

#endif
