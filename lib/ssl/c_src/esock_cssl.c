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
 * Purpose: Adaptions for the C/SSL package.
 *
 * NOTE: This file is incomplete. We do not support C/SSL (yet).
 * 
 * The CSSL_[read|write]Would[Read|Write] functions are a bit tricky. Here
 * is what it is all about:
 * 
 * Action  Network access by C/SSL would  be
 *
 *	   read		     	   write		   nothing
 *         ------------------------------------------------------------------
 * read	   readWouldWrite = 0	   readWouldWrite = 1	   readWouldWrite = 0
 *	   readWouldRead = 1	   readWouldRead = 0	   readWouldRead = 0
 *
 * write   writeWouldRead = 1	   writeWouldRead = 0	   ----
 *
 * Hence:  SSL wants to read  <==> writeWouldRead = 1
 *         SSL wants to write <==> readWouldWrite = 1.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#include "esock.h"
#include "esock_ssl.h"
#include "debuglog.h"
#include "esock_utils.h"

#include "cssl/ssl.h"
#include "cssl/err.h"
#include "cssl/cssl.h"
#include "cssl/suites.h"

#define FLAGSBUFSIZE	512
#define CLIENT		1
#define SERVER		2
#define CA_ARRAY_SIZE	16


typedef struct {
    CSSL_SupportFactory factory;
    int count;
} EFactory;

typedef struct {
    EFactory *efact;
    CSSL ssl;
} ESSL;

static int check_return_value(Connection *cp, int retval);
static EFactory *make_efactory(Connection *cp, int type);
static void maybe_remove_efactory(EFactory *efact);
static CSSL_SupportFactory make_cssl_factory(Connection *cp, int type);

int esock_ssl_init(void)
{
    time_t t = time(NULL);
    char *tp = ctime(&t);

    CSSL_init(CSSL_initFlag_Singlethreaded);
    CSSL_seed(tp, strlen(tp));
    return 0;
}

void esock_ssl_finish(void)
{
    CSSL_shutdown();
}

/* If cp->flags are set, we create a "support factory" to be used by
 * all connections accepted on this listen socket.
 */
int esock_ssl_listen_init(Connection *cp)
{
    ESSL *essl;
    EFactory *efact;

    if (cp->flags && *cp->flags) {
	essl = cp->opaque = esock_malloc(sizeof(ESSL));
	essl->ssl = NULL;
	if (!(efact = make_efactory(cp, SERVER)))
	    return -1;
	essl->efact = efact;
    }	
    return 0;
}


/* 
 * XXX If we want cached sessions, we must replace (NULL, 0) with
 * a session cache key (and its length), created e.g. by CSSL_Socket_cacheKey.
 * We then have to have ipstring and port of the remote host saved in the 
 * connection structure.
 */
int esock_ssl_connect_init(Connection *cp)
{
    
    ESSL *essl = cp->opaque = esock_malloc(sizeof(ESSL));
    EFactory *efact;

    if (!(efact = make_efactory(cp, CLIENT)))
	return -1;
    essl->efact = efact;
    if (!(essl->ssl = CSSL_Client_new(cp->fd, NULL, 0, 
				      essl->efact->factory)))
	return -1;
    return 0;
}

/*  If there is already a factory created associated with the listen
 *  socket, we will use that. Otherwise we create one, and make the
 *  listen socket connection refer to it. Note that we have reference
 *  counting on factories.
 */
int esock_ssl_accept_init(Connection *cp)
{
    ESSL *essl = cp->opaque = esock_malloc(sizeof(ESSL));
    ESSL *listen_essl;
    Connection *listen_cp;
    EFactory *efact;

    if (!(listen_cp = get_connection(cp->listen_fd)) ||
	!(listen_essl = (ESSL *)listen_cp->opaque) ||
	!listen_essl->efact) {
	if (!(efact = make_efactory(cp, SERVER)))
	    return -1;
	if (listen_cp) {
	    if (!listen_cp->opaque) {
		listen_essl = (ESSL *)listen_cp->opaque = 
		    esock_malloc(sizeof(ESSL));
		listen_essl->ssl = NULL;
		listen_essl->efact = NULL;
	    }
	    if (!listen_essl->efact) {
		listen_essl->efact = efact;
		efact->count++;
	    }
	}
    }
    efact->count++;
    essl->efact = efact;
    if (!(essl->ssl = CSSL_Server_new(cp->fd, essl->efact->factory)))
	return -1;
    return 0;
}


void esock_ssl_free(Connection *cp)
{
    ESSL *essl = cp->opaque;

    if (essl) {
	CSSL_kill(essl->ssl);
	maybe_remove_efactory(essl->efact);
	esock_free(cp->opaque);
	cp->opaque = NULL;
    }
}

int esock_ssl_set_masks(Connection *cp, fd_set *rfds, 
			fd_set *wfds, fd_set *efds)
{
    int i = 0;
    
    DEBUGF(("SETTING: "));
    while (cp) {
	switch (cp->state) {
	case ESOCK_ACTIVE_LISTENING:
	    DEBUGF(("%d (read) ", cp->fd));
	    FD_SET(cp->fd, rfds);
	    break;
	case ESOCK_WAIT_CONNECT:
	    DEBUGF(("%d (write) ", cp->fd));
	    FD_SET(cp->fd, wfds);
#ifdef __WIN32__
	    FD_SET(cp->fd, efds); /* Failure shows in efds */
#endif
	    break;
	case ESOCK_SSL_CONNECT:
	case ESOCK_SSL_ACCEPT:
	    if (cp->ssl_want == ESOCK_SSL_WANT_READ) {
		DEBUGF(("%d (read) ", cp->fd));
		FD_SET(cp->fd, rfds);
	    } else if (cp->ssl_want == ESOCK_SSL_WANT_WRITE) {
		DEBUGF(("%d (write) ", cp->fd));
		FD_SET(cp->fd, wfds);
	    }
	    break;
	case ESOCK_SSL_CLEANUP:
	    DEBUGF(("%d (read) ", cp->proxy->fd));
	    FD_SET(cp->proxy->fd, rfds);
	    break;
	case ESOCK_PROXY_CLEANUP:
	    DEBUGF(("%d (read) ", cp->fd));
	    FD_SET(cp->fd, rfds);
	    break;
	case ESOCK_SSL_CLOSING:
	    if (cp->ssl_want == ESOCK_SSL_WANT_READ) {
		DEBUGF(("%d (read) ", cp->fd));
		FD_SET(cp->fd, rfds);
	    } else if (cp->ssl_want == ESOCK_SSL_WANT_WRITE) {
		DEBUGF(("%d (write) ", cp->fd));
		FD_SET(cp->fd, wfds);
	    }
	    break;
	case ESOCK_JOINED:
	    DEBUGF(("%d (read) ", cp->fd));
	    FD_SET(cp->fd, rfds);
	    DEBUGF(("%d (read) ", cp->proxy->fd));
	    FD_SET(cp->proxy->fd, rfds);
	    break;
	default:
	    break;
	}
	i++;
	cp = cp->next;
    }
    DEBUGF(("\n"));
    return i;
}


Connection *esock_ssl_read_masks(Connection *cp, Connection **cpnext, 
				 fd_set *rfds, fd_set *wfds, fd_set *efds)
{
    while(cp) {
	if(FD_ISSET(cp->fd, rfds) ||
	   (cp->proxy && FD_ISSET(cp->proxy->fd, rfds)) ||
	   (FD_ISSET(cp->fd, wfds)) ||
	   (cp->proxy && FD_ISSET(cp->proxy->fd, wfds))
#ifdef __WIN32__
	   || FD_ISSET(cp->fd, efds) /* Connect failure in WIN32 */
#endif
	    ) {
	    *cpnext = cp->next;
	    return cp;
	}
	cp = cp->next;
    }
    *cpnext = NULL;
    return NULL;
}


/* 
 * esock_ssl_connect(Connection *cp)
 *
 * Returns 0 on success, -1 for fatal error, and -2 for temporary error.
 *
 */
int esock_ssl_connect(Connection *cp)
{
    int ret;
    ESSL *essl = cp->opaque;

    DEBUGF(("esock_ssl_connect: calling CSSL_handshake fd = %d\n", cp->fd));
    ret = CSSL_handshake(essl->ssl);
    DEBUGF(("  CSSL_handshake() = %d\n", ret));
    return check_return_value(cp, ret);
}



/* 
 * esock_ssl_accept(Connection *cp)
 *
 * Returns 0 on success, -1 for fatal error, and -2 for temporary error.
 *
 */
int esock_ssl_accept(Connection *cp)
{
    int ret;
    ESSL *essl = cp->opaque;

    DEBUGF(("esock_ssl_accept: calling CSSL_handshake fd = %d\n", cp->fd));
    ret = CSSL_handshake(essl->ssl);
    DEBUGF(("  CSSL_handshake() = %d\n", ret));
    return check_return_value(cp, ret);
}


/* 
 * esock_ssl_read(Connection *cp, char *buf, int len)
 *
 * Read at most `len' chars into `buf'. Returns number of chars
 * read ( > 0), or 0 at EOF, or -1 on error. Sets errno to ERRNO_BLOCK
 * if temporary error.
 */
int esock_ssl_read(Connection *cp, char *buf, int len)
{
    int ret;
    ESSL *essl = cp->opaque;

    DEBUGF(("esock_ssl_read: calling CSSL_read fd = %d\n", cp->fd));
    ret = CSSL_read(essl->ssl, buf, len);
    DEBUGF(("  CSSL_read = %d\n", ret));
    if (ret > 0) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", ret, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", ret, buf));
    }
    return check_return_value(cp, ret);
}


/* 
 * esock_ssl_write(Connection *cp, char *buf, int len)
 *
 * Writes at most `len' chars from `buf'. Returns number of chars
 * written, or -1 on error. Sets errno to ERRNO_BLOCK if temporary error.
 */
int esock_ssl_write(Connection *cp, char *buf, int len)
{
    int ret;
    ESSL *essl = cp->opaque;

    DEBUGF(("esock_ssl_write: calling CSSL_write fd = %d\n", cp->fd));
    ret = CSSL_write(essl->ssl, buf, len);
    DEBUGF(("  CSSL_write = %d\n", ret));
    if (ret > 0) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", ret, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", ret, buf));
    }
    return check_return_value(cp, ret);
}


int esock_ssl_close(Connection *cp)
{
    int ret;
    ESSL *essl = cp->opaque;

    DEBUGF(("esock_ssl_close: calling CSSL_close fd = %d\n", cp->fd));
    ret = CSSL_close(essl->ssl);
    DEBUGF(("  SSL_close = %d\n", ret));
    return check_return_value(cp, ret);
}

/*
 * Print SSL specific errors.
 */
void esock_ssl_print_errors_fp(FILE *fp)
{
    fprintf(fp, "esock_ssl_print_errors_fp(): Nothing to print for C/SSL.\n");
}


/* Local functions */

static int check_return_value(Connection *cp, int retval)
{
    ESSL *essl = cp->opaque;

    cp->ssl_want = 0;
    if (retval == -2) {
	/* temporary error */
	DEBUGF(("  temporary error = %d\n", CSSL_getErrno(essl->ssl)));
	sock_set_errno(ERRNO_BLOCK);
	if (CSSL_writeWouldRead(essl->ssl))
	    cp->ssl_want = ESOCK_SSL_WANT_READ;
	else if (CSSL_readWouldWrite(essl->ssl)) 
	    cp->ssl_want = ESOCK_SSL_WANT_WRITE;
    } else if (retval == -1) {
	/* fatal error */
	DEBUGF(("  fatal error = %d\n", CSSL_getErrno(essl->ssl)));
	/* XXX Should pick-up errno or similar */
    }
    return retval;
}

/* 
 * Create a "support factory". Type is CLIENT or SERVER.
 *
 * We have the intension to create a new factory for servers, only if
 * the factory corresponding to the listen socket is not valid any more.
 * Is that a safe thing to do, considering that the listen socket can
 * go away at any time?
 *
 */

static EFactory *make_efactory(Connection *cp, int type)
{
    CSSL_SupportFactory factory;
    EFactory *fact = NULL;

    if ((factory = make_cssl_factory(cp, type))) {
	fact = esock_malloc(sizeof(EFactory));
	fact->factory = factory;
	fact->count = 1;
    }
    return fact;
}

static void maybe_remove_efactory(EFactory *efact)
{
    if (efact && --efact->count <= 0) {
	CSSL_SupportFactory_kill(efact->factory);
	esock_free(efact);
    }
}

static CSSL_CertificateVerification verifier(CSSL_SupportFactory factory,
					     CSSL_Certificate
					     *cert_chain)
{
    CSSL_CertificateVerification status;

    /* XXXX */

    return status;
}

static CSSL_SupportFactory make_cssl_factory(Connection *cp, int type)
{
    CSSL_SupportFactory factory = NULL;
    int verify_depth = 0;
    long cache_timeout = -2;
    CSSL_ClientAuthentication authentication = -2;
    CSSL_SupportFlag support_flags;
    CSSL_SessionCache session_cache;
    /* CSSL_CipherSuite *cipher_suite = NULL; XXX Un-used */
    CSSL_CertificateChain cert_chain;
    CSSL_Key key;		/* private key */
    CSSL_Certificate ca_array[CA_ARRAY_SIZE];
    char *log_file = NULL;
    char *key_file = NULL;
    char *key_passw = NULL;
    char *ciphers = NULL;
    char *ca_cert_file = NULL;	/* CA certificates */
    char *cert_file = NULL;	/* chain of certificates */
    int argc, i, n, idx;
    char **argv;

    /* Build argc/argv from ssl flags */
    argc = esock_build_argv(cp->flags, &argv);
    for (i = 0; i < argc; i++) {
	if (strcmp(argv[i], "-verify") == 0) {
	    verify_depth = atoi(argv[++i]);
	    DEBUGF(("Verify depth = %d\n", verify_depth));
	    switch (verify_depth) {
	    case 0:
		authentication = CSSL_clientAuthentication_None;
		break;
	    case 1:
		authentication = CSSL_clientAuthentication_Requested;
		break;
	    case 2:
		authentication = CSSL_clientAuthentication_Required;
		break;
	    default:
		authentication = CSSL_clientAuthentication_None;
		break;
	    }
	    
	} else if (strcmp(argv[i], "-log") == 0) {
	    log_file = argv[++i];
	} else if (strcmp(argv[i], "-cert") == 0) {
	    cert_file = argv[++i];
	} else if (strcmp(argv[i], "-key") == 0) {
	    key_file = argv[++i];
	} else if (strcmp(argv[i], "-passw") == 0) {
	    key_passw = argv[++i];
	} else if (strcmp(argv[i], "-cacert") == 0) {
	    ca_cert_file = argv[++i];
	} else if (strcmp(argv[i], "-cache") == 0) {
	    cache_timeout = atoi(argv[++i]);
	} else if (strcmp(argv[i], "-d") == 0) {
	    /* XXX  debug */
	    i++;
	} else if (strcmp(argv[i], "-cipher") == 0) {
	    ciphers = argv[++i];
	} else {
	    /* XXX Error */
	}
    }

    if (type == CLIENT)
	factory = CSSL_ClientSupportFactory_new();
    else
	factory = CSSL_ServerSupportFactory_new();
    if (!factory) {
	DEBUGF(("Cannot create factory\n"));
	goto error_end;
    }
    
    /* XXX 
     * Cipher suites are not checked (yet)
     */
    CSSL_SupportFactory_setCipherSuites(factory, CSSL_cipherSuites_Standard);

    /* Compatibility flags */
    support_flags = CSSL_supportFlag_IgnorePrematureEOF;
    if (type == CLIENT)
	support_flags |= CSSL_supportFlag_SuppressTLSv1;
    else
	support_flags |= CSSL_serverSupportFlag_SupportSSLv2Hello;
    CSSL_SupportFactory_setFlags(factory, support_flags);

    /* Authentication */
    if (type == SERVER)
	CSSL_ServerSupportFactory_setClientAuthentication(factory, 
							  authentication);
    /* Set session cache */
    session_cache = CSSL_SessionCache_new();
    CSSL_SessionCache_setTimeout(session_cache, cache_timeout);
    CSSL_SupportFactory_setSessionCache(factory, session_cache);

    /* Set certificate verifier */
    CSSL_SupportFactory_setCertificateVerifier(factory, verifier);

    /* 
     * Load chain of certificates 
     */
    if (key_passw) {
	/* 
	 * Password supplied 
	 */
	if (key_file) {
	    DEBUGF(("Error: Cannot use password protected separate key file: "
		   "%s\n", key_file));
	    goto error_end;
	} else {
	    CSSL_CertificateChain_loadPEMpkcs8(&cert_chain, cert_file,
					       key_passw);
	}
    } else {
	/*
	 * No password supplied 
	 */
	if (key_file) {
	    /* Key file supplied */
	    CSSL_Key_load(&key, key_file);
	    CSSL_CertificateChain_loadPEMwithKey(&cert_chain, cert_file,
						 key);
	} else {
	    /* Key in cert file */
	    CSSL_CertificateChain_loadPEM(&cert_chain, cert_file);
	}
    }
    /* Install the certificate chain */
    CSSL_SupportFactory_addCertificateChain(factory, cert_chain);

    /*
     * Load CA certificates
     */

    /* XXX For now we do not support loading from directories. If we have
     * to we should have a separate option, e.g. "-cadir" .
     */
    idx = 0;
    do {
	n = CSSL_Certificate_loadPEMFile(ca_array, CA_ARRAY_SIZE, idx, 
					 cert_file);
	for (i = 0; i < n; i++)
	    CSSL_SupportFactory_addCACertificate(factory, ca_array[i]);
	idx += n;
    } while (n > 0);

    if (0) {
    error_end:
	if (factory) {
	    CSSL_SupportFactory_kill(factory);
	    factory = NULL;
	}
    }
    /* Free arg list */
    for (i = 0; argv[i]; i++)
	esock_free(argv[i]);
    esock_free(argv);
    
    return factory;
}
    
