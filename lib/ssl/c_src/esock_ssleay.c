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
 * Purpose: Adaptions for the SSLeay package.
 *
 * This file implements the functions defined in esock_ssl.h for
 * the SSLeay package. 
 *
 * Since SSLeay comes with very poor documentation, in particular with
 * regards to non-blocking I/O, we have inferred the return values
 * from the various SSL functions by scrutinizing the source code.
 * The following is what we have found:
 *
 *   Function		Return values
 *   -------- 		-------------
 *   SSL_accept()       success: 1, failure: =<0
 *   SSL_connect()      success: 1, failure: =<0
 *   SSL_read()         success: >0, eof: 0, failure: <0 
 *   SSL_write()	success: > 0, failure: =<0 
 *   SSL_shutdown()	success: 1, not finished: 0
 *
 * If the return value of any of the above functions is `retval' and the
 * ssl connection is `ssl', the call
 *
 * 	ssl_error = SSL_get_error(ssl, retval);
 *
 * returns one of the following eight values:
 *
 *   SSL_ERROR_NONE			retval > 0
 *   SSL_ERROR_ZERO_RETURN		retval = 0
 *   SSL_ERROR_WANT_READ		retval < 0 and ssl wants to read
 *   SSL_ERROR_WANT_WRITE		retval < 0 and ssl wants to write
 *   SSL_ERROR_SYSCALL			retval < 0  or retval = 0
 *   SSL_ERROR_SSL			if there was an ssl internal error
 *   SSL_ERROR_WANT_X509_LOOKUP		retval < 0 and ssl wants x509 lookup 
 *   SSL_ERROR_WANT_CONNECT		retval < 0 and ssl wants connect
 *
 * It is the case that SSL_read() sometimes returns -1, even when the 
 * underlying file descriptor is ready for reading.
 * 
 * Also, sometimes we may have SSL_ERROR_SSL in SSL_accept() and SSL_connect()
 * when a retry should be done.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "esock.h"
#include "esock_ssl.h"
#include "debuglog.h"
#include "esock_utils.h"
#include "esock_posix_str.h"

/* SSLeay includes */
#include "crypto.h"
#include "ssl.h"
#include "err.h"

/*
** Compatibility hack for different SSL versions 
** Unfortunately there is no define in OpenSSL to tell 
** the version installed.
** The esock_ssleay.c module is dependent of two changes:
** a) pem_password_cb userdata appeares in 0.9.4 and later
** b) malloc/realloc pointers returned are void * in 0.9.5 and later
**
**/
#ifndef OPENSSL_VERSION
  /* SSL_CTRL_MODE appeared in 0.9.4 */
  #ifndef SSL_CTRL_MODE 
    #define OPENSSL_VERSION 0x000903 /* or earlier */
  #else
    /* The SSL_AD_USER_CANCLED was changed to SSL_AD_USER_CANCELLED in 0.9.5 */
    #ifndef SSL_AD_USER_CANCELLED
      #define OPENSSL_VERSION 0x000904
    #else
      #define OPENSSL_VERSION 0x000905 /* or later */
    #endif
  #endif
#endif

char *esock_ssl_err_str = "";

#define FLAGSBUFSIZE		512
#define X509BUFSIZE		256
#define IS_SSL_CLIENT		0
#define IS_SSL_SERVER		1
#define DEFAULT_VERIFY_DEPTH	1

typedef struct {
    int code;
    char *text;
} err_entry;

static void set_want(Connection *cp, int ssl_error);
static char *ssl_error_str(int error);
static void reset_err_str(void);
static void maybe_set_err_str(char *s);
static int set_ssl_parameters(Connection *cp, int server);
static int verify_callback(int ok, X509_STORE_CTX *ctx);
#if OPENSSL_VERSION < 0x000904
static int passwd_callback(char *buf, int num, int verify);
#else
static int passwd_callback(char *buf, int num, int verify, void *userdata);
#endif
static void info_callback(SSL *s, int where, int ret);

static err_entry errs[] = {    
    {SSL_ERROR_NONE, "SSL_ERROR_NONE"},
    {SSL_ERROR_ZERO_RETURN, "SSL_ERROR_ZERO_RETURN"}, 
    {SSL_ERROR_WANT_READ, "SSL_ERROR_WANT_READ"}, 
    {SSL_ERROR_WANT_WRITE, "SSL_ERROR_WANT_WRITE"}, 
    {SSL_ERROR_SYSCALL, "SSL_ERROR_SYSCALL"},
    {SSL_ERROR_SSL, "SSL_ERROR_SSL"},
    {SSL_ERROR_WANT_X509_LOOKUP, "SSL_ERROR_WANT_X509_LOOKUP"},
    {SSL_ERROR_WANT_CONNECT, "SSL_ERROR_WANT_CONNECT"}
};

static SSL_CTX *ctx = NULL;
static char x509_buf[X509BUFSIZE];
static char *key_passwd = NULL;
static int verify_depth = 0;	/* Communicate depth to verify_callback() */


int esock_ssl_init(void)
{
    SSL_METHOD *meth;
    char *cert_file, *cert_dir;

    /* 
     * Silly casts to avoid compiler warnings: malloc and realloc in 
     * SSLeay should return void * and not char *.  
     */
#if OPENSSL_VERSION < 0x000905
    CRYPTO_set_mem_functions((char *(*)())esock_malloc, 
			     (char *(*)())esock_realloc, 
			     esock_free);
#else
    CRYPTO_set_mem_functions((void *(*)())esock_malloc, 
			     (void *(*)())esock_realloc, 
			     esock_free);
#endif
    meth = SSLv23_method();
    SSL_load_error_strings();
    SSLeay_add_ssl_algorithms();

    if (!(ctx = SSL_CTX_new(meth))) {
	SSLDEBUGF();
	DEBUGF(("Cannot allocate SSL context\n"));
	return -1;
    }

    if (debug) 
	SSL_CTX_set_info_callback(ctx, info_callback);

    SSL_CTX_set_default_passwd_cb(ctx, passwd_callback);

    /* The following call sets the default verify mode of ctx to
     * VERIFY_NONE, and sets the verify callback function to
     * verify_callback. When a certificate is verified it is the
     * callback function set here that is called, not the one set by
     * SSL_set_verify(ssl,...) for each connection.  See also under
     * set_ssl_parameters() below.  
     */
    SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, verify_callback);

    cert_file = getenv("SSL_CERT_FILE");
    cert_dir = getenv("SSL_CERT_DIR");

    if (cert_file || cert_dir) {
	if (!SSL_CTX_load_verify_locations(ctx, cert_file, cert_dir) ||
	    !SSL_CTX_set_default_verify_paths(ctx)) {
	    SSLDEBUGF();
	    DEBUGF(("Cannot set verify locations\n"));
	    return -1;
	}
    }
    return 0;
}


void esock_ssl_finish(void)
{
    SSL_CTX_free(ctx);
}


void esock_ssl_free(Connection *cp)
{
    if (cp->opaque) {
	SSL_free(cp->opaque);
	cp->opaque = NULL;
    }
}

/* Set fd_set masks
 *
 */
int esock_ssl_set_masks(Connection *cp, fd_set *rfds, 
			fd_set *wfds, fd_set *efds, int verbose)
{
    int i = 0;
    
    if (verbose)
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
	    if (!cp->bp) {
		if (cp->wq.len) {
		    DEBUGF(("%d (write) ", cp->fd));
		    FD_SET(cp->fd, wfds);
		} else if (!cp->proxy->eof) {
		    DEBUGF(("%d (read) ", cp->proxy->fd));
		    FD_SET(cp->proxy->fd, rfds);
		}
	    }
	    if (!cp->proxy->bp) {
		if (cp->proxy->wq.len) {
		    DEBUGF(("%d (write) ", cp->proxy->fd));
		    FD_SET(cp->proxy->fd, wfds);
		} else if (!cp->eof) {
		    DEBUGF(("%d (read) ", cp->fd));
		    FD_SET(cp->fd, rfds);
		}
	    }
	    break;
	default:
	    break;
	}
	i++;
	cp = cp->next;
    }
    if (verbose)
	DEBUGF(("\n"));
    return i;
}


Connection *esock_ssl_read_masks(Connection *cp, Connection **cpnext, 
				 fd_set *rfds, fd_set *wfds, fd_set *efds,
				 int set_wq_fds)
{
    while(cp) {
	if (FD_ISSET(cp->fd, rfds) ||
	    (cp->proxy && FD_ISSET(cp->proxy->fd, rfds)) ||
	    (FD_ISSET(cp->fd, wfds)) ||
	    (cp->proxy && FD_ISSET(cp->proxy->fd, wfds))
#ifdef __WIN32__
	    || FD_ISSET(cp->fd, efds) /* Connect failure in WIN32 */
#endif
	    || (set_wq_fds && (cp->wq.len || 
			       (cp->proxy && cp->proxy->wq.len)))) {
	    *cpnext = cp->next;
	    return cp;
	}
	cp = cp->next;
    }
    *cpnext = NULL;
    return NULL;
}

/*
 * Print SSL specific errors.
 */
void esock_ssl_print_errors_fp(FILE *fp)
{
    ERR_print_errors_fp(fp);
}


int esock_ssl_accept_init(Connection *cp)
{
    SSL *ssl = cp->opaque = SSL_new(ctx);
    
    SSL_set_fd(ssl, cp->fd);
    return set_ssl_parameters(cp, IS_SSL_SERVER);
}


int esock_ssl_connect_init(Connection *cp)
{
    SSL *ssl = cp->opaque = SSL_new(ctx);
    
    SSL_set_fd(ssl, cp->fd);
    return set_ssl_parameters(cp, IS_SSL_CLIENT);
}


int esock_ssl_listen_init(Connection *cp)
{
    /* Do nothing */
    return 0;
}

/* 
 * esock_ssl_accept(Connection *cp)
 *
 */
int esock_ssl_accept(Connection *cp)
{
    int retval, ssl_error;
    SSL *ssl = cp->opaque;

    verify_depth = cp->ssl_verify_depth; /* For verify_callback() */
    reset_err_str();

    DEBUGF(("esock_ssl_accept: calling SSL_accept fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    retval = SSL_accept(ssl);
    ssl_error = SSL_get_error(ssl, retval);
    DEBUGF(("  SSL_accept = %d\n"
	    "  error: %s\n"
	    "  state after: %s\n", 
	    retval, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (retval > 0)
	return retval;
    else if (retval == 0) {
	/* permanent accept error */
	sock_set_errno(ERRNO_NONE);
	maybe_set_err_str("esslaccept");
	return -1;
    } else {
	/* possibly temporary accept error */
	set_want(cp, ssl_error);
	switch (ssl_error) {
	case SSL_ERROR_SYSCALL:
	    /* Typically sock_errno() is equal to ERRNO_BLOCK */
	    maybe_set_err_str(esock_posix_str(sock_errno()));
	    break;
	case SSL_ERROR_SSL:
	    SSLDEBUGF();
	    maybe_set_err_str("esslerrssl");
	    break;
	case SSL_ERROR_WANT_X509_LOOKUP:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ex509lookup");
	    break;
	case SSL_ERROR_WANT_CONNECT:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ewantconnect");
	    break;
	}
	return retval;
    }
}

/* 
 * esock_ssl_connect(Connection *cp)
 *
 */
int esock_ssl_connect(Connection *cp)
{
    int retval, ssl_error;
    SSL *ssl = cp->opaque;

    verify_depth = cp->ssl_verify_depth; /* For verify_callback() */
    reset_err_str();

    DEBUGF(("esock_ssl_connect: calling SSL_connect fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    retval = SSL_connect(ssl);
    ssl_error = SSL_get_error(ssl, retval);
    DEBUGF(("  SSL_connect() = %d\n"
	    "  error: %s\n"
	    "  state after: %s\n", 
	    retval, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (retval > 0)
	return retval;
    else if (retval == 0) {
	/* permanent connect error */
	sock_set_errno(ERRNO_NONE);
	maybe_set_err_str("esslconnect");
	return -1;
    } else {
	/* possibly temporary connect error */
	set_want(cp, ssl_error);
	switch (ssl_error) {
	case SSL_ERROR_SYSCALL:
	    /* Typically sock_errno() is equal to ERRNO_BLOCK */
	    maybe_set_err_str(esock_posix_str(sock_errno()));
	    break;
	case SSL_ERROR_SSL:
	    SSLDEBUGF();
	    maybe_set_err_str("esslerrssl");
	    break;
	case SSL_ERROR_WANT_X509_LOOKUP:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ex509lookup");
	    break;
	case SSL_ERROR_WANT_CONNECT:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ewantconnect");
	    break;
	}
	return retval;
    }
}


/* esock_ssl_read(Connection *cp, char *buf, int len)
 *
 * Read at most `len' chars into `buf'. Returns number of chars
 * read ( > 0), or 0 at EOF, or -1 on error.  
 */

int esock_ssl_read(Connection *cp, char *buf, int len)
{
    int retval, ssl_error;
    SSL *ssl = cp->opaque;

    reset_err_str();
    DEBUGF(("esock_ssl_read: calling SSL_read fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    retval = SSL_read(ssl, buf, len);
    ssl_error = SSL_get_error(ssl, retval);
    DEBUGF(("  SSL_read = %d\n"
	    "  error: %s\n"
	    "  state after: %s\n", 
	    retval, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (ssl_error == SSL_ERROR_NONE) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", retval, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", retval, buf));
    }
    if (retval >= 0)
	return retval;
    else {
	/* possibly temporary read error */
	set_want(cp, ssl_error);
	switch (ssl_error) {
	case SSL_ERROR_SYSCALL:
	    /* Typically sock_errno() is equal to ERRNO_BLOCK */
	    maybe_set_err_str(esock_posix_str(sock_errno()));
	    break;
	case SSL_ERROR_SSL:
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("esslerrssl");
	    break;
	case SSL_ERROR_WANT_X509_LOOKUP:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ex509lookup");
	    break;
	case SSL_ERROR_WANT_CONNECT:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ewantconnect");
	    break;
	}
	return retval;
    }
}

/* 
 * esock_ssl_write(Connection *cp, char *buf, int len)
 *
 * Writes at most `len' chars from `buf'. Returns number of chars
 * written, or -1 on error.
 */
int esock_ssl_write(Connection *cp, char *buf, int len)
{
    int retval, ssl_error;
    SSL *ssl = cp->opaque;

    reset_err_str();
    DEBUGF(("esock_ssl_write: calling SSL_write fd = %d\n"
	    "  state before: %s\n", cp->fd, SSL_state_string(ssl)));
    retval = SSL_write(ssl, buf, len);
    ssl_error = SSL_get_error(ssl, retval);
    DEBUGF(("  SSL_write = %d\n"
	    "  error: %s\n"
	    "  state after: %s\n", 
	    retval, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (ssl_error == SSL_ERROR_NONE) {
	DEBUGMSGF(("message (hex) : [%3.*a]\n", retval, buf));
	DEBUGMSGF(("message (char): [%3.*b]\n", retval, buf));
    }
    if (retval >= 0)
	return retval;
    else {
	/* possibly temporary write error */
	set_want(cp, ssl_error);
	switch (ssl_error) {
	case SSL_ERROR_SYSCALL:
	    /* Typically sock_errno() is equal to ERRNO_BLOCK */
	    maybe_set_err_str(esock_posix_str(sock_errno()));
	    break;
	case SSL_ERROR_SSL:
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("esslerrssl");
	    break;
	case SSL_ERROR_WANT_X509_LOOKUP:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ex509lookup");
	    break;
	case SSL_ERROR_WANT_CONNECT:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ewantconnect");
	    break;
	}
	return retval;
    }
}


int esock_ssl_close(Connection *cp)
{
    int retval, ssl_error;
    SSL *ssl = cp->opaque;

    reset_err_str();
    DEBUGF(("esock_ssl_close: calling SSL_shutdown fd = %d\n"
    "  state before: %s\n",  cp->fd, SSL_state_string(ssl)));
    retval = SSL_shutdown(ssl);
    ssl_error = SSL_get_error(ssl, retval);
    DEBUGF(("  SSL_shutdown = %d\n"
	    "  error: %s\n"
	    "  state after: %s\n",
	    retval, ssl_error_str(ssl_error), SSL_state_string(ssl)));
    if (retval >= 0)
	return retval;
    else {
	/* possibly temporary shutdown error */
	set_want(cp, ssl_error);
	switch (ssl_error) {
	case SSL_ERROR_SYSCALL:
	    /* Typically sock_errno() is equal to ERRNO_BLOCK */
	    maybe_set_err_str(esock_posix_str(sock_errno()));
	    break;
	case SSL_ERROR_SSL:
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("esslerrssl");
	    break;
	case SSL_ERROR_WANT_X509_LOOKUP:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ex509lookup");
	    break;
	case SSL_ERROR_WANT_CONNECT:
	    SSLDEBUGF();
	    sock_set_errno(ERRNO_NONE);
	    maybe_set_err_str("ewantconnect");
	    break;
	}
	return retval;
    }
}



/* Local functions */

static void set_want(Connection *cp, int ssl_error)
{
    cp->ssl_want = 0;
    switch(ssl_error) {
    case SSL_ERROR_WANT_READ:
	cp->ssl_want = ESOCK_SSL_WANT_READ;
	break;
    case SSL_ERROR_WANT_WRITE:
	cp->ssl_want = ESOCK_SSL_WANT_WRITE;
	break;
    default: 
	break;
    }
}
   

static char *ssl_error_str(int ssl_error)
{
    int i;
    static char buf[128];

    for (i = 0; i < sizeof(errs)/sizeof(err_entry); i ++) {
	if (ssl_error == errs[i].code)
	    return errs[i].text;
    }
    sprintf(buf, "esock_ssleay: SSL_error unknown: %d", ssl_error);
    return buf;
}

static void reset_err_str(void)
{
    esock_ssl_err_str = "";
}

static void maybe_set_err_str(char *s)
{
    if (!esock_ssl_err_str[0])
	esock_ssl_err_str = s;
}

/* 
 * set_ssl_parameters
 *
 * Set ssl parameters in connection structure. 
 *
 */

static int set_ssl_parameters(Connection *cp, int server)
{
    char *cert_file = NULL;
    char *ca_cert_file = NULL;
    char *key_file = NULL;
    char *cipher_list = NULL;
    int verify = 0, verify_mode;
    SSL *ssl = cp->opaque;
    int i, argc;
    char **argv;

    reset_err_str();
    cp->ssl_verify_depth = DEFAULT_VERIFY_DEPTH;
    key_passwd = NULL;

    argc = esock_build_argv(cp->flags, &argv);

    DEBUGF(("Argv:\n"));
    for (i = 0; i < argc; i++) {
	DEBUGF(("%d:  %s\n", i, argv[i]));
    }

    for (i = 0; i < argc; i++) {
	if (strcmp(argv[i], "-verify") == 0) {
	    verify = atoi(argv[++i]);
	} else if (strcmp(argv[i], "-depth") == 0) {
	    cp->ssl_verify_depth = atoi(argv[++i]);
	} else if (strcmp(argv[i], "-log") == 0) {
	    /* XXX  ignored: logging per connection not supported */
	    i++;
	} else if (strcmp(argv[i], "-cert") == 0) {
	    cert_file = argv[++i];
	} else if (strcmp(argv[i], "-key") == 0) {
	    key_file = argv[++i];
	} else if (strcmp(argv[i], "-passw") == 0) {
	    key_passwd = argv[++i];
	} else if (strcmp(argv[i], "-cacert") == 0) {
	    ca_cert_file = argv[++i];
	} else if (strcmp(argv[i], "-d") == 0) {
	    /* XXX  ignored: debug per connection not supported */
	    i++;
	} else if (strcmp(argv[i], "-cipher") == 0) {
	    cipher_list = argv[++i];
	} else {
	    /* XXX Error: now ignored */
	}
    }

    switch (verify) {
    case 0:
	verify_mode = SSL_VERIFY_NONE;
	break;
    case 1:
	verify_mode = SSL_VERIFY_PEER|SSL_VERIFY_CLIENT_ONCE;
	break;
    case 2:
	verify_mode = SSL_VERIFY_PEER|SSL_VERIFY_FAIL_IF_NO_PEER_CERT|
	    SSL_VERIFY_CLIENT_ONCE;
	break;
    default:
	verify_mode = SSL_VERIFY_NONE;
    }
   

    DEBUGF(("set_params: all arguments read\n"));

    /* Use certificate file if key file has not been set */
    if (!key_file)
	key_file = cert_file;

    /* SSL_set_verify() is broken in SSLeay-0.9.0. The value of
     * verify_callback has no effect, but the value of verify_mode
     * has. The setting of verify_callback was fixed in the call to
     * SSL_CTX_set_verify() in esock_ssl_init() above.  
     */

     DEBUGF(("set_params: SSL_set_verify (verify = %d)\n", verify)); 
     SSL_set_verify(ssl, verify_mode, verify_callback);
    
    if (server && !cert_file) {
	DEBUGF(("ERROR: Server must have certificate\n"));
	maybe_set_err_str("enoservercert");
	goto err_end;
    }
    
    if (cert_file) {
	DEBUGF(("set_params: SSL_use_certificate_file\n"));
	if (SSL_use_certificate_file(ssl, cert_file, SSL_FILETYPE_PEM) <= 0) {
	    DEBUGF(("ERROR: Cannot set certificate file\n"));
	    maybe_set_err_str("ecertfile");
	    goto err_end;
	}
    }

    if (ca_cert_file) {
	DEBUGF(("set_params: SSL_set_client_CA_list\n"));
	SSL_set_client_CA_list(ssl, SSL_load_client_CA_file(ca_cert_file));
	if (SSL_get_client_CA_list(ssl) == NULL) {
	    DEBUGF(("ERROR: Cannot set CA list\n"));
	    maybe_set_err_str("ecacertfile");
	    goto err_end;
	}
    }

    if (key_file) { 
	DEBUGF(("set_params: SSL_use_PrivateKey_file\n"));
	if (SSL_use_PrivateKey_file(ssl, key_file, SSL_FILETYPE_PEM) <= 0) {
	    DEBUGF(("ERROR: Cannot set private key file\n"));
	    maybe_set_err_str("ekeyfile");
	    goto err_end;
	}
    }
    
    if(cert_file && key_file) {
	DEBUGF(("set_params: SSL_check_private_key\n"));
	if (!SSL_check_private_key(ssl)) {
	    DEBUGF(("ERROR: Private key does not match the certificate\n")); 
	    maybe_set_err_str("ekeymismatch");
	    goto err_end;
	}
    }    

    if (cipher_list || (cipher_list = getenv("SSL_CIPHER"))) {
	DEBUGF(("set_params: SSL_set_cipher_list\n"));
	if (!SSL_set_cipher_list(ssl, cipher_list)) {
	    DEBUGF(("ERROR: Cannot set cipher list\n"));
	    maybe_set_err_str("ecipher");
	    goto err_end;
	}
    }

    DEBUGF(("set_params: ready\n"));
    /* Free arg list */
    for (i = 0; argv[i]; i++)
	esock_free(argv[i]);
    esock_free(argv);
    return 0;

err_end:
    DEBUGF(("set_params: in error\n"));
    /* Free arg list */
    for (i = 0; argv[i]; i++)
	esock_free(argv[i]);
    esock_free(argv);
    return -1;
}

/* Call back functions */

static int verify_callback(int ok, X509_STORE_CTX *x509_ctx)
{
    X509 *cert;
    int cert_err, depth;

    cert = X509_STORE_CTX_get_current_cert(x509_ctx);
    cert_err = X509_STORE_CTX_get_error(x509_ctx);
    depth = X509_STORE_CTX_get_error_depth(x509_ctx);

    X509_NAME_oneline(X509_get_subject_name(cert), x509_buf, 
		      sizeof(x509_buf));
    DEBUGF(("  +vfy: depth = %d, subject = %s\n",depth, x509_buf));

    if (!ok) {
	DEBUGF(("  +vfy: error = %d [%s]\n",cert_err,
		X509_verify_cert_error_string(cert_err)));
	if (depth >= verify_depth) /* verify_depth set in accept/connect */
	    ok = 1;
    }
    switch (cert_err) {
    case X509_V_OK:
    case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
	ok = 1;
	break;
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
    case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
	X509_NAME_oneline(X509_get_issuer_name(x509_ctx->current_cert), 
			  x509_buf, sizeof(x509_buf));
	DEBUGF(("  +vfy: issuer = %s\n", x509_buf));
	maybe_set_err_str("enoissuercert");
	break;
    case X509_V_ERR_CERT_HAS_EXPIRED:
	maybe_set_err_str("epeercertexpired");
	break;
    case X509_V_ERR_CERT_NOT_YET_VALID:
    case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
    case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
	maybe_set_err_str("epeercertinvalid");
	break;
    case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
	maybe_set_err_str("eselfsignedcert");
	break;
    case X509_V_ERR_CERT_CHAIN_TOO_LONG:
	maybe_set_err_str("echaintoolong");
	break;
    default:
	maybe_set_err_str("epeercert");
	break;
    }
    DEBUGF(("  +vfy: return = %d\n",ok));
    return ok;
}

#if OPENSSL_VERSION < 0x000904
static int passwd_callback(char *buf, int num, int verify)
#else
static int passwd_callback(char *buf, int num, int verify, void *userdata)
#endif
{
    int len;
    if (key_passwd) {
	DEBUGF(("  +passwd: %s\n", key_passwd));
	strncpy(buf, key_passwd, num);
	len = strlen(key_passwd);
	key_passwd = NULL;
	return len;
    }
    DEBUGF(("  +passwd: ERROR: No password found.%s\n"));
    return 0;
}

static void info_callback(SSL *ssl, int where, int retval)
{
    char *str;

    if (where & SSL_CB_LOOP) {
	DEBUGF(("  info: %s\n",SSL_state_string_long(ssl)));
    } else if (where & SSL_CB_ALERT) {
	str = (where & SSL_CB_READ) ? "read" : "write";
	DEBUGF(("  info: SSL3 alert %s:%s:%s\n", str, 
		SSL_alert_type_string_long(retval),
		SSL_alert_desc_string_long(retval)));
    } else if (where & SSL_CB_EXIT) {
	if (retval == 0) {
	    DEBUGF(("  info: failed in %s\n", SSL_state_string_long(ssl)));
	} else if (retval < 0) {
	    DEBUGF(("  info: error in %s\n", SSL_state_string_long(ssl)));
	}
    }
}


