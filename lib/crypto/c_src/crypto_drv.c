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
 * Purpose:  Dynamically loadable driver for loading cryptographic 
 *           libraries. 
 */

#ifdef __WIN32__
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

#include "des.h"
#include "md5.h"
#include "sha.h"

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

/* shared libs & dlls */
EXTERN void *driver_dl_open(char *);
EXTERN void *driver_dl_sym(void *, char *);
EXTERN int driver_dl_close(void *);
EXTERN char *driver_dl_error(void);

/* Driver interface declarations */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen); 

static void hmac_md5(char *key, int klen, char *dbuf, int dlen, 
		     char *hmacbuf);
static void hmac_sha1(char *key, int klen, char *dbuf, int dlen, 
		      char *hmacbuf);

static ErlDrvEntry crypto_driver_entry = {
    NULL,			/* init */
    start, 
    stop, 
    NULL,			/* output */
    NULL,			/* ready_input */
    NULL,			/* ready_output */ 
    "crypto_drv", 
    NULL,			/* finish */
    NULL,			/* handle */
    control, 
    NULL,			/* timeout */
    NULL			/* outputv */
};

static ErlDrvPort erlang_port = NULL;
static ErlDrvData driver_data = (ErlDrvData) &erlang_port; /* Anything goes */


/* Library declarations */

#define DRV_INFO		0
#define DRV_MD5			1
#define DRV_MD5_INIT		2
#define DRV_MD5_UPDATE		3
#define DRV_MD5_FINAL		4
#define DRV_SHA			5
#define DRV_SHA_INIT		6
#define DRV_SHA_UPDATE		7
#define DRV_SHA_FINAL		8
#define DRV_MD5_MAC		9
#define DRV_MD5_MAC_96		10
#define DRV_SHA_MAC		11
#define DRV_SHA_MAC_96		12
#define DRV_CBC_DES_ENCRYPT	13
#define DRV_CBC_DES_DECRYPT	14

#define NUM_CRYPTO_FUNCS       	14

#define MD5_CTX_LEN	(sizeof(MD5_CTX))
#define MD5_LEN		16
#define MD5_LEN_96	12
#define SHA_CTX_LEN	(sizeof(SHA_CTX))
#define SHA_LEN		20
#define SHA_LEN_96	12
#define HMAC_INT_LEN	64

#define HMAC_IPAD	0x36
#define HMAC_OPAD	0x5c

static void *lib_handle = NULL;
static void *lib_name = NULL;


typedef struct _crypto_funcs {
    int (*CRYPTO_set_mem_functions)(void *, void *, void *);
    unsigned char *(*MD5)(char *, int, char *);
    void (*MD5_Init)(MD5_CTX *);
    void (*MD5_Update)(MD5_CTX *, char *, int);
    void (*MD5_Final)(char *, MD5_CTX *);
    unsigned char *(*SHA1)(char *, int, char *);
    void (*SHA1_Init)(SHA_CTX *);
    void (*SHA1_Update)(SHA_CTX *, char *, int);
    void (*SHA1_Final)(char *, SHA_CTX *);
    int (*des_set_key)(char *, void *);
    void (*des_ncbc_encrypt)(char *, char *, int, void *, char *, int);
} crypto_funcs;

static crypto_funcs cfs;

/* INITIALIZATION AFTER LOADING */

/* 
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to 
 * the driver entry.
 */

DRIVER_INIT(crypto_drv)
{
    return &crypto_driver_entry;
}

/* DRIVER INTERFACE */

/* command = "crypto_drv libname fullname" */
static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    char *buf, *tok, *full_name, *name;
    int i;

    if (erlang_port != NULL)
	return ERL_DRV_ERROR_GENERAL;
    
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

    buf = driver_alloc(strlen(command) + 1);
    strcpy(buf, command);

    tok = strtok(buf, " ");	/* driver name */
    name = strtok(NULL, " ");	/* lib name */
    full_name = strtok(NULL, " ");	
    lib_name = driver_alloc(strlen(name) + 1);
    strcpy(lib_name, name);

    if ((lib_handle = driver_dl_open(full_name)) == NULL) {
	fprintf(stderr, "driver_dl error: %s\r\n", driver_dl_error());
	fprintf(stderr, "crypto_drv:start: cannot load %s\r\n", full_name);
	driver_free(buf);
	driver_free(lib_name);
	lib_name = NULL;
	return ERL_DRV_ERROR_GENERAL;
    }

    cfs.CRYPTO_set_mem_functions = 
	driver_dl_sym(lib_handle, "CRYPTO_set_mem_functions");
    cfs.MD5 = driver_dl_sym(lib_handle, "MD5");
    cfs.MD5_Init = driver_dl_sym(lib_handle, "MD5_Init");
    cfs.MD5_Update = driver_dl_sym(lib_handle, "MD5_Update");
    cfs.MD5_Final = driver_dl_sym(lib_handle, "MD5_Final");
    cfs.SHA1 = driver_dl_sym(lib_handle, "SHA1");
    cfs.SHA1_Init = driver_dl_sym(lib_handle, "SHA1_Init");
    cfs.SHA1_Update = driver_dl_sym(lib_handle, "SHA1_Update");
    cfs.SHA1_Final = driver_dl_sym(lib_handle, "SHA1_Final");
    cfs.des_set_key = driver_dl_sym(lib_handle, "des_set_key");
    cfs.des_ncbc_encrypt = driver_dl_sym(lib_handle, "des_ncbc_encrypt");

    /* Check that all pointer where initialized */
    for (i = 0; i < sizeof(crypto_funcs)/sizeof(void*); i++) {
        if (((char **)&cfs)[i] == NULL) {
	    fprintf(stderr, "crypto_drv:start : function %d not "
		    "initialized\n", i);
	    driver_dl_close(lib_handle);
	    lib_handle = NULL;
	    driver_free(buf);
	    driver_free(lib_name);
	    lib_name = NULL;
	    return ERL_DRV_ERROR_GENERAL;
        }
    }

    (*cfs.CRYPTO_set_mem_functions)(driver_alloc, driver_realloc, 
				    driver_free);

    driver_free(buf);
    erlang_port = port;
    return driver_data;
}

static void stop(ErlDrvData drv_data)
{
    driver_dl_close(lib_handle);
    lib_handle = NULL;
    driver_free(lib_name);
    lib_name = NULL;
    erlang_port = NULL;
    return;
}

/* Since we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen)
{
    int klen, dlen, i, macsize;
    char *key, *dbuf, *ivec;
    ErlDrvBinary *b;
    des_key_schedule schedule;
    char hmacbuf[SHA_LEN];
    MD5_CTX md5_ctx;
    SHA_CTX sha_ctx;

    switch(command) {

    case DRV_INFO:
	*rbuf = (char*)(b = driver_alloc_binary(NUM_CRYPTO_FUNCS));
	for (i = 0; i < NUM_CRYPTO_FUNCS; i++) {
	    b->orig_bytes[i] = i + 1;
	}
	return NUM_CRYPTO_FUNCS;
	break;

    case DRV_MD5:
	*rbuf = (char*)(b = driver_alloc_binary(MD5_LEN));
	(*cfs.MD5)(buf, len, b->orig_bytes);
	return MD5_LEN;
	break;

    case DRV_MD5_INIT:
	*rbuf = (char*)(b = driver_alloc_binary(MD5_CTX_LEN));
	(*cfs.MD5_Init)((MD5_CTX *)b->orig_bytes);
	return MD5_CTX_LEN;		
	break;

    case DRV_MD5_UPDATE:
	if (len < MD5_CTX_LEN)
	    return -1;
	*rbuf = (char*)(b = driver_alloc_binary(MD5_CTX_LEN));
	memcpy(b->orig_bytes, buf, MD5_CTX_LEN);
	(*cfs.MD5_Update)((MD5_CTX *)b->orig_bytes, buf + MD5_CTX_LEN, 
			  len - MD5_CTX_LEN);
	return MD5_CTX_LEN;		
	break;

    case DRV_MD5_FINAL:
	if (len != MD5_CTX_LEN)
	    return -1;
	memcpy(&md5_ctx, buf, MD5_CTX_LEN); /* XXX Use buf only? */
	*rbuf = (char *)(b  = driver_alloc_binary(MD5_LEN));
	(*cfs.MD5_Final)(b->orig_bytes, &md5_ctx);
	return MD5_LEN;		
	break;

    case DRV_SHA:
	*rbuf = (char *)(b = driver_alloc_binary(SHA_LEN));
	(*cfs.SHA1)(buf, len, b->orig_bytes);
	return SHA_LEN;
	break;

    case DRV_SHA_INIT:
	*rbuf = (char *)(b = driver_alloc_binary(SHA_CTX_LEN));
	(*cfs.SHA1_Init)((SHA_CTX *)b->orig_bytes);
	return SHA_CTX_LEN;		
	break;

    case DRV_SHA_UPDATE:
	if (len < SHA_CTX_LEN)
	    return -1;
	*rbuf = (char *)(b = driver_alloc_binary(SHA_CTX_LEN)); 
	memcpy(b->orig_bytes, buf, SHA_CTX_LEN);
	(*cfs.SHA1_Update)((SHA_CTX *)b->orig_bytes, buf + SHA_CTX_LEN, 
			  len - SHA_CTX_LEN);
	return SHA_CTX_LEN;		
	break;

    case DRV_SHA_FINAL:
	if (len != SHA_CTX_LEN)
	    return -1;
	memcpy(&sha_ctx, buf, SHA_CTX_LEN); /* XXX Use buf only? */
	*rbuf = (char *)(b = driver_alloc_binary(SHA_LEN));
	(*cfs.SHA1_Final)(b->orig_bytes, &sha_ctx);
	return SHA_LEN;		
	break;

    case DRV_MD5_MAC:
    case DRV_MD5_MAC_96:
	/* buf = klen[4] key data */
	rlen = MD5_LEN;
	klen = get_int32(buf);
	key = buf + 4;
	dlen = len - klen - 4;
	dbuf = key + klen;
	hmac_md5(key, klen, dbuf, dlen, hmacbuf);
	macsize = (command == DRV_MD5_MAC) ? MD5_LEN : MD5_LEN_96;
	*rbuf = (char *)(b = driver_alloc_binary(macsize));
	memcpy(b->orig_bytes, hmacbuf, macsize);
	return macsize;
	break;

    case DRV_SHA_MAC:
    case DRV_SHA_MAC_96:
	/* buf = klen[4] key data */
	rlen = SHA_LEN;
	klen = get_int32(buf);
	key = buf + 4;
	dlen = len - klen - 4;
	dbuf = key + klen;
	hmac_sha1(key, klen, dbuf, dlen, hmacbuf);
	macsize = (command == DRV_SHA_MAC) ? SHA_LEN : SHA_LEN_96;
	*rbuf = (char *)(b = driver_alloc_binary(macsize));
	memcpy(b->orig_bytes, hmacbuf, macsize);
	return macsize;
	break;

    case DRV_CBC_DES_ENCRYPT:
    case DRV_CBC_DES_DECRYPT:
	/* buf = key[8] ivec[8] data */
	dlen = len - 16;
	if (dlen < 0)
	    return -1;
	key = buf; ivec = buf + 8; dbuf = buf + 16;
	*rbuf = (char *)(b = driver_alloc_binary(dlen));
	(*cfs.des_set_key)(key, (void *)schedule);
	(*cfs.des_ncbc_encrypt)(dbuf, b->orig_bytes, dlen, schedule, ivec, 
				(command == DRV_CBC_DES_ENCRYPT));
	return dlen;
	break;

    default:
	break;
    }
    return -1;
}


/* HMAC */

static void hmac_md5(char *key, int klen, char *dbuf, int dlen, char *hmacbuf)
{
    MD5_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    char nkey[MD5_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	MD5_CTX kctx;

	(*cfs.MD5_Init)(&kctx);
	(*cfs.MD5_Update)(&kctx, key, klen);
	(*cfs.MD5_Final)(nkey, &kctx);
	key = nkey;
	klen = MD5_LEN;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner MD5 */
    (*cfs.MD5_Init)(&ctx);
    (*cfs.MD5_Update)(&ctx, ipad, HMAC_INT_LEN);
    (*cfs.MD5_Update)(&ctx, dbuf, dlen);
    (*cfs.MD5_Final)(hmacbuf, &ctx);
    /* outer MD5 */
    (*cfs.MD5_Init)(&ctx);
    (*cfs.MD5_Update)(&ctx, opad, HMAC_INT_LEN);
    (*cfs.MD5_Update)(&ctx, hmacbuf, MD5_LEN);
    (*cfs.MD5_Final)(hmacbuf, &ctx);
}

static void hmac_sha1(char *key, int klen, char *dbuf, int dlen, 
		      char *hmacbuf)
{
    SHA_CTX ctx;
    char ipad[HMAC_INT_LEN];
    char opad[HMAC_INT_LEN];
    char nkey[SHA_LEN];
    int i;

    /* Change key if longer than 64 bytes */
    if (klen > HMAC_INT_LEN) {
	SHA_CTX kctx;

	(*cfs.SHA1_Init)(&kctx);
	(*cfs.SHA1_Update)(&kctx, key, klen);
	(*cfs.SHA1_Final)(nkey, &kctx);
	key = nkey;
	klen = SHA_LEN;
    }

    memset(ipad, '\0', sizeof(ipad));
    memset(opad, '\0', sizeof(opad));
    memcpy(ipad, key, klen);
    memcpy(opad, key, klen);

    for (i = 0; i < HMAC_INT_LEN; i++) {
	ipad[i] ^= HMAC_IPAD;
	opad[i] ^= HMAC_OPAD;
    }

    /* inner SHA */
    (*cfs.SHA1_Init)(&ctx);
    (*cfs.SHA1_Update)(&ctx, ipad, HMAC_INT_LEN);
    (*cfs.SHA1_Update)(&ctx, dbuf, dlen);
    (*cfs.SHA1_Final)(hmacbuf, &ctx);
    /* outer SHA */
    (*cfs.SHA1_Init)(&ctx);
    (*cfs.SHA1_Update)(&ctx, opad, HMAC_INT_LEN);
    (*cfs.SHA1_Update)(&ctx, hmacbuf, SHA_LEN);
    (*cfs.SHA1_Final)(hmacbuf, &ctx);
}



