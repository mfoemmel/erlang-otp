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
 * Purpose:  Dynamically loadable driver for cryptography libraries. 
 * Based on OpenSSL. 
 */

#ifdef __WIN32__
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "erl_driver.h"

#include <openssl/des.h>
#include <openssl/md5.h>
#include <openssl/sha.h>

#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

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


/* Keep the following definitions in alignment with the FUNC_LIST
 * in crypto.erl. 
 */

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
#define DRV_EDE3_CBC_DES_ENCRYPT 15
#define DRV_EDE3_CBC_DES_DECRYPT 16

#define NUM_CRYPTO_FUNCS       	16

#define MD5_CTX_LEN	(sizeof(MD5_CTX))
#define MD5_LEN		16
#define MD5_LEN_96	12
#define SHA_CTX_LEN	(sizeof(SHA_CTX))
#define SHA_LEN		20
#define SHA_LEN_96	12
#define HMAC_INT_LEN	64

#define HMAC_IPAD	0x36
#define HMAC_OPAD	0x5c


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

static ErlDrvData start(ErlDrvPort port, char *command)
{ 

    if (erlang_port != NULL)
	return ERL_DRV_ERROR_GENERAL;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    CRYPTO_set_mem_functions(driver_alloc, driver_realloc, driver_free);
    erlang_port = port;
    return driver_data;
}

static void stop(ErlDrvData drv_data)
{
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
    char *key, *key2, *key3, *dbuf, *ivec;
    const_DES_cblock *des_key, *des_key2, *des_key3;
    const unsigned char *des_dbuf;
    DES_cblock *des_ivec;
    ErlDrvBinary *bin;
    DES_key_schedule schedule, schedule2, schedule3;
    char hmacbuf[SHA_LEN];
    MD5_CTX md5_ctx;
    SHA_CTX sha_ctx;

    switch(command) {

    case DRV_INFO:
	*rbuf = (char*)(bin = driver_alloc_binary(NUM_CRYPTO_FUNCS));
	for (i = 0; i < NUM_CRYPTO_FUNCS; i++) {
	    bin->orig_bytes[i] = i + 1;
	}
	return NUM_CRYPTO_FUNCS;
	break;

    case DRV_MD5:
	*rbuf = (char*)(bin = driver_alloc_binary(MD5_LEN));
	MD5(buf, len, bin->orig_bytes);
	return MD5_LEN;
	break;

    case DRV_MD5_INIT:
	*rbuf = (char*)(bin = driver_alloc_binary(MD5_CTX_LEN));
	MD5_Init((MD5_CTX *)bin->orig_bytes);
	return MD5_CTX_LEN;		
	break;

    case DRV_MD5_UPDATE:
	if (len < MD5_CTX_LEN)
	    return -1;
	*rbuf = (char*)(bin = driver_alloc_binary(MD5_CTX_LEN));
	memcpy(bin->orig_bytes, buf, MD5_CTX_LEN);
	MD5_Update((MD5_CTX *)bin->orig_bytes, buf + MD5_CTX_LEN, 
		   len - MD5_CTX_LEN);
	return MD5_CTX_LEN;		
	break;

    case DRV_MD5_FINAL:
	if (len != MD5_CTX_LEN)
	    return -1;
	memcpy(&md5_ctx, buf, MD5_CTX_LEN); /* XXX Use buf only? */
	*rbuf = (char *)(bin = driver_alloc_binary(MD5_LEN));
	MD5_Final(bin->orig_bytes, &md5_ctx);
	return MD5_LEN;		
	break;

    case DRV_SHA:
	*rbuf = (char *)(bin = driver_alloc_binary(SHA_LEN));
	SHA1(buf, len, bin->orig_bytes);
	return SHA_LEN;
	break;

    case DRV_SHA_INIT:
	*rbuf = (char *)(bin = driver_alloc_binary(SHA_CTX_LEN));
	SHA1_Init((SHA_CTX *)bin->orig_bytes);
	return SHA_CTX_LEN;		
	break;

    case DRV_SHA_UPDATE:
	if (len < SHA_CTX_LEN)
	    return -1;
	*rbuf = (char *)(bin = driver_alloc_binary(SHA_CTX_LEN)); 
	memcpy(bin->orig_bytes, buf, SHA_CTX_LEN);
	SHA1_Update((SHA_CTX *)bin->orig_bytes, buf + SHA_CTX_LEN, 
			  len - SHA_CTX_LEN);
	return SHA_CTX_LEN;		
	break;

    case DRV_SHA_FINAL:
	if (len != SHA_CTX_LEN)
	    return -1;
	memcpy(&sha_ctx, buf, SHA_CTX_LEN); /* XXX Use buf only? */
	*rbuf = (char *)(bin = driver_alloc_binary(SHA_LEN));
	SHA1_Final(bin->orig_bytes, &sha_ctx);
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
	*rbuf = (char *)(bin = driver_alloc_binary(macsize));
	memcpy(bin->orig_bytes, hmacbuf, macsize);
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
	*rbuf = (char *)(bin = driver_alloc_binary(macsize));
	memcpy(bin->orig_bytes, hmacbuf, macsize);
	return macsize;
	break;

    case DRV_CBC_DES_ENCRYPT:
    case DRV_CBC_DES_DECRYPT:
	/* buf = key[8] ivec[8] data */
	dlen = len - 16;
	if (dlen < 0)
	    return -1;
	des_key = (const_DES_cblock*) buf; 
	des_ivec = (DES_cblock*)(buf + 8); 
	des_dbuf = buf + 16;
	*rbuf = (char *)(bin = driver_alloc_binary(dlen));
	DES_set_key(des_key, &schedule);
	DES_ncbc_encrypt(des_dbuf, bin->orig_bytes, dlen, &schedule, des_ivec, 
			 (command == DRV_CBC_DES_ENCRYPT));
	return dlen;
	break;

    case DRV_EDE3_CBC_DES_ENCRYPT:
    case DRV_EDE3_CBC_DES_DECRYPT:
	dlen = len - 32;
	if (dlen < 0)
	    return -1;
	des_key = (const_DES_cblock*) buf; 
	des_key2 = (const_DES_cblock*) (buf + 8); 
	des_key3 = (const_DES_cblock*) (buf + 16);
	des_ivec = (DES_cblock*) (buf + 24); 
	des_dbuf = buf + 32;
	*rbuf = (char *)(bin = driver_alloc_binary(dlen));
	DES_set_key(des_key, &schedule);
	DES_set_key(des_key2, &schedule2);
	DES_set_key(des_key3, &schedule3);
	DES_ede3_cbc_encrypt(des_dbuf, bin->orig_bytes, dlen, &schedule,
			     &schedule2, &schedule3, des_ivec, 
			     (command == DRV_EDE3_CBC_DES_ENCRYPT));
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

	MD5_Init(&kctx);
	MD5_Update(&kctx, key, klen);
	MD5_Final(nkey, &kctx);
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
    MD5_Init(&ctx);
    MD5_Update(&ctx, ipad, HMAC_INT_LEN);
    MD5_Update(&ctx, dbuf, dlen);
    MD5_Final(hmacbuf, &ctx);
    /* outer MD5 */
    MD5_Init(&ctx);
    MD5_Update(&ctx, opad, HMAC_INT_LEN);
    MD5_Update(&ctx, hmacbuf, MD5_LEN);
    MD5_Final(hmacbuf, &ctx);
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

	SHA1_Init(&kctx);
	SHA1_Update(&kctx, key, klen);
	SHA1_Final(nkey, &kctx);
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
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, ipad, HMAC_INT_LEN);
    SHA1_Update(&ctx, dbuf, dlen);
    SHA1_Final(hmacbuf, &ctx);
    /* outer SHA */
    SHA1_Init(&ctx);
    SHA1_Update(&ctx, opad, HMAC_INT_LEN);
    SHA1_Update(&ctx, hmacbuf, SHA_LEN);
    SHA1_Final(hmacbuf, &ctx);
}



