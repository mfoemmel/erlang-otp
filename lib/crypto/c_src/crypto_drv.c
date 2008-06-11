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

#define OPENSSL_THREAD_DEFINES
#include <openssl/opensslconf.h>
#ifndef OPENSSL_THREADS
#  ifdef __GNUC__
#    warning No thread support by openssl. Driver will use coarse grain locking.
#  endif 
#endif

#include <openssl/crypto.h>
#include <openssl/des.h>
/* #include <openssl/idea.h> This is not supported on the openssl OTP requires */
#include <openssl/dsa.h>
#include <openssl/rsa.h>
#include <openssl/aes.h>
#include <openssl/md5.h>
#include <openssl/sha.h>
#include <openssl/bn.h>
#include <openssl/objects.h>
#include <openssl/rc4.h>
#include <openssl/rc2.h>

#ifdef DEBUG
#  define ASSERT(e) \
    ((void) ((e) ? 1 : (fprintf(stderr,"Assert '%s' failed at %s:%d\n",\
				#e, __FILE__, __LINE__), abort(), 0)))
#else
#  define ASSERT(e) ((void) 1)
#endif

#ifdef __GNUC__
#  define INLINE __inline__
#elif defined(__WIN32__)
#  define INLINE __forceinline
#else
#  define INLINE
#endif


#define get_int32(s) ((((unsigned char*) (s))[0] << 24) | \
                      (((unsigned char*) (s))[1] << 16) | \
                      (((unsigned char*) (s))[2] << 8)  | \
                      (((unsigned char*) (s))[3]))

#define put_int32(s,i) \
{ (s)[0] = (char)(((i) >> 24) & 0xff);\
  (s)[1] = (char)(((i) >> 16) & 0xff);\
  (s)[2] = (char)(((i) >> 8) & 0xff);\
  (s)[3] = (char)((i) & 0xff);\
}

/* Driver interface declarations */
static int init(void);
static void finish(void);
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen); 

/* openssl callbacks */
#ifdef OPENSSL_THREADS
static void locking_function(int mode, int n, const char *file, int line);
static unsigned long id_function(void);
static struct CRYPTO_dynlock_value* dyn_create_function(const char *file,
							int line);
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* ptr,
			      const char *file, int line);
static void dyn_destroy_function(struct CRYPTO_dynlock_value *ptr,
				 const char *file, int line);
#endif /* OPENSSL_THREADS */

/* helpers */
static void hmac_md5(char *key, int klen, char *dbuf, int dlen, 
                     char *hmacbuf);
static void hmac_sha1(char *key, int klen, char *dbuf, int dlen, 
                      char *hmacbuf);

static ErlDrvEntry crypto_driver_entry = {
    init,
    start, 
    stop, 
    NULL,                       /* output */
    NULL,                       /* ready_input */
    NULL,                       /* ready_output */ 
    "crypto_drv", 
    finish,
    NULL,                       /* handle */
    control, 
    NULL,                       /* timeout */
    NULL,                       /* outputv */

    NULL,                       /* ready_async */
    NULL,                       /* flush */
    NULL,                       /* call */
    NULL,                       /* event */
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
#ifdef OPENSSL_THREADS
    ERL_DRV_FLAG_USE_PORT_LOCKING,
#else
    0,
#endif
    NULL,                       /* handle2 */
    NULL                        /* process_exit */
};


/* Keep the following definitions in alignment with the FUNC_LIST
 * in crypto.erl. 
 */

#define DRV_INFO                0
#define DRV_MD5                 1
#define DRV_MD5_INIT            2
#define DRV_MD5_UPDATE          3
#define DRV_MD5_FINAL           4
#define DRV_SHA                 5
#define DRV_SHA_INIT            6
#define DRV_SHA_UPDATE          7
#define DRV_SHA_FINAL           8
#define DRV_MD5_MAC             9
#define DRV_MD5_MAC_96          10
#define DRV_SHA_MAC             11
#define DRV_SHA_MAC_96          12
#define DRV_CBC_DES_ENCRYPT     13
#define DRV_CBC_DES_DECRYPT     14
#define DRV_EDE3_CBC_DES_ENCRYPT 15
#define DRV_EDE3_CBC_DES_DECRYPT 16
#define DRV_AES_CFB_128_ENCRYPT 17
#define DRV_AES_CFB_128_DECRYPT 18
#define DRV_RAND_BYTES          19
#define DRV_RAND_UNIFORM        20
#define DRV_MOD_EXP             21
#define DRV_DSS_VERIFY          22
#define DRV_RSA_VERIFY          23
#define DRV_CBC_AES128_ENCRYPT  24
#define DRV_CBC_AES128_DECRYPT  25
#define DRV_XOR                 26
#define DRV_RC4_ENCRYPT         27 /* no decrypt needed; symmetric */
#define DRV_RC4_SETKEY          28
#define DRV_RC4_ENCRYPT_WITH_STATE 29
#define DRV_CBC_RC2_40_ENCRYPT     30
#define DRV_CBC_RC2_40_DECRYPT     31
#define DRV_CBC_AES256_ENCRYPT  32
#define DRV_CBC_AES256_DECRYPT  33
#define DRV_INFO_LIB            34
/* #define DRV_CBC_IDEA_ENCRYPT    34 */
/* #define DRV_CBC_IDEA_DECRYPT    35 */

#define NUM_CRYPTO_FUNCS        34

#define MD5_CTX_LEN     (sizeof(MD5_CTX))
#define MD5_LEN         16
#define MD5_LEN_96      12
#define SHA_CTX_LEN     (sizeof(SHA_CTX))
#define SHA_LEN         20
#define SHA_LEN_96      12
#define HMAC_INT_LEN    64

#define HMAC_IPAD       0x36
#define HMAC_OPAD       0x5c


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

static ErlDrvRWLock** lock_vec = NULL; /* Static locks used by openssl */

/* DRIVER INTERFACE */

static int init(void)
{
    ErlDrvSysInfo sys_info;
    int i;

    CRYPTO_set_mem_functions(driver_alloc, driver_realloc, driver_free);

#ifdef OPENSSL_THREADS
    driver_system_info(&sys_info, sizeof(sys_info));

    if(sys_info.scheduler_threads > 1) {
	lock_vec = driver_alloc(CRYPTO_num_locks()*sizeof(*lock_vec));
	if (lock_vec==NULL) return -1;
	memset(lock_vec,0,CRYPTO_num_locks()*sizeof(*lock_vec));
    
	for(i=CRYPTO_num_locks()-1; i>=0; --i) {
	    lock_vec[i] = erl_drv_rwlock_create("crypto_drv_stat");
	    if (lock_vec[i]==NULL) return -1;
	}
	CRYPTO_set_locking_callback(locking_function);    
	CRYPTO_set_id_callback(id_function);
	CRYPTO_set_dynlock_create_callback(dyn_create_function);
	CRYPTO_set_dynlock_lock_callback(dyn_lock_function);
	CRYPTO_set_dynlock_destroy_callback(dyn_destroy_function);
    }
    /* else no need for locks */
#endif /* OPENSSL_THREADS */

    return 0;
}

static void finish(void)
{
    /* Moved here from control() as it's not thread safe */
    CRYPTO_cleanup_all_ex_data();

    if(lock_vec != NULL) {
	int i;
	for(i=CRYPTO_num_locks()-1; i>=0; --i) {
	    if (lock_vec[i] != NULL) {
		erl_drv_rwlock_destroy(lock_vec[i]);
	    }
	}
	driver_free(lock_vec);
    }
}

static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return 0; /* not used */
}

static void stop(ErlDrvData drv_data)
{
    return;
}

/* Since we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
                   int len, char **rbuf, int rlen)
{
    int klen, dlen, i, j, macsize, from_len, to_len;
    int base_len, exponent_len, modulo_len;
    int data_len, dsa_p_len, dsa_q_len;
    int dsa_g_len, dsa_y_len;
    int rsa_e_len, rsa_n_len;
    int or_mask;
    unsigned int rsa_s_len;
    char *key, *key2, *dbuf, *p;
    const_DES_cblock *des_key, *des_key2, *des_key3;
    const unsigned char *des_dbuf;
    BIGNUM *bn_from, *bn_to, *bn_rand, *bn_result;
    BIGNUM *bn_base, *bn_exponent, *bn_modulo;
    BIGNUM *dsa_p, *dsa_q, *dsa_r, *dsa_s, *dsa_g, *dsa_y;
    BIGNUM *rsa_n, *rsa_e;
    DES_cblock *des_ivec;
    ErlDrvBinary *bin;
    DES_key_schedule schedule, schedule2, schedule3;
/*     IDEA_KEY_SCHEDULE idea, idea2; */
    unsigned char hmacbuf[SHA_DIGEST_LENGTH];
    unsigned char *rsa_s;
    /* char hmacbuf[SHA_LEN]; */
    MD5_CTX md5_ctx;
    SHA_CTX sha_ctx;
    int new_ivlen = 0;
    BN_CTX *bn_ctx;
    DSA *dsa;
    RSA *rsa;
    DSA_SIG *dsa_sig;
    AES_KEY aes_key;
    RC4_KEY rc4_key;
    RC2_KEY rc2_key;

    switch(command) {

    case DRV_INFO:
        *rbuf = (char*)(bin = driver_alloc_binary(NUM_CRYPTO_FUNCS));
        for (i = 0; i < NUM_CRYPTO_FUNCS; i++) {
            bin->orig_bytes[i] = i + 1;
        }
        return NUM_CRYPTO_FUNCS;

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
	if (dlen % 8 != 0)
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

/*     case DRV_CBC_IDEA_ENCRYPT: */
/*     case DRV_CBC_IDEA_DECRYPT: */
         /* buf = key[16] ivec[8] data */
/*         dlen = len - 24; */
/*         if (dlen < 0) */
/*             return -1; */
/* 	if (dlen % 8 != 0) */
/* 	    return -1; */
/*         *rbuf = (char *)(bin = driver_alloc_binary(dlen)); */
/*         idea_set_encrypt_key(buf, &idea); */
/*  	if (command == DRV_CBC_IDEA_DECRYPT) { */
/*  	    idea_set_decrypt_key(&idea, &idea2); */
/* 	    memcpy(&idea, &idea2, sizeof(idea));  */
/* 	} */
/*         idea_cbc_encrypt(buf + 24, bin->orig_bytes, dlen, &idea, buf + 8, */
/*                          (command == DRV_CBC_IDEA_ENCRYPT)); */
/*         return dlen; */
/*         break; */

    case DRV_CBC_RC2_40_ENCRYPT:
    case DRV_CBC_RC2_40_DECRYPT:
        /* buf = key[5] ivec[8] data */
        dlen = len - 13;
        if (dlen < 0)
            return -1;
        *rbuf = (char *)(bin = driver_alloc_binary(dlen));
        RC2_set_key(&rc2_key, 5, buf, 40);
        RC2_cbc_encrypt(buf + 13, bin->orig_bytes, dlen, &rc2_key, buf + 5,
			(command == DRV_CBC_RC2_40_ENCRYPT));
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

    case DRV_AES_CFB_128_ENCRYPT:
    case DRV_AES_CFB_128_DECRYPT:
        /* buf = key[16] ivec[16] data */
        dlen = len - 32;
        if (dlen < 0)
            return -1;
        *rbuf = (char *)(bin = driver_alloc_binary(dlen));
        AES_set_encrypt_key(buf, 128, &aes_key);
        AES_cfb128_encrypt(buf+32, bin->orig_bytes, dlen, &aes_key,
                           buf+16, &new_ivlen,
                           (command == DRV_AES_CFB_128_ENCRYPT));
        return dlen;

    case DRV_RC4_ENCRYPT:
        /* buf = klen[4] key data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        *rbuf = (char *)(bin = driver_alloc_binary(dlen));
        RC4_set_key(&rc4_key, klen, key);
        RC4(&rc4_key, dlen, dbuf, bin->orig_bytes);
        return dlen;

    case DRV_RC4_SETKEY:
        /* buf = key */
        dlen = sizeof(rc4_key);
        *rbuf = (char *)(bin = driver_alloc_binary(dlen));
        RC4_set_key(&rc4_key, len, buf);
        memcpy(bin->orig_bytes, &rc4_key, dlen);
        return dlen;

    case DRV_RC4_ENCRYPT_WITH_STATE:
        /* buf = statelength[4] state data, return statelength[4] state data */
        klen = get_int32(buf);
        key = buf + 4;
        dlen = len - klen - 4;
        dbuf = key + klen;
        *rbuf = (char *)(bin = driver_alloc_binary(len));
        memcpy(&rc4_key, key, klen);
        RC4(&rc4_key, dlen, dbuf, bin->orig_bytes + klen + 4);
        memcpy(bin->orig_bytes, buf, 4);
        memcpy(bin->orig_bytes + 4, &rc4_key, klen);
        return dlen;

    case DRV_RAND_BYTES:
        /* buf = <<rlen:32/integer,topmask:8/integer,bottommask:8/integer>> */

        if (len != 6)
            return -1;
        rlen = get_int32(buf);
        *rbuf = (char *)(bin = driver_alloc_binary(rlen));
        RAND_pseudo_bytes(bin->orig_bytes,rlen);
        or_mask = ((unsigned char*)buf)[4];
        bin->orig_bytes[rlen-1] |= or_mask; /* topmask */
        or_mask = ((unsigned char*)buf)[5];
        bin->orig_bytes[0] |= or_mask; /* bottommask */
        return rlen;
        break;
      
    case DRV_RAND_UNIFORM:
      /* buf = <<from_len:32/integer,bn_from:from_len/binary,   *
       *         to_len:32/integer,bn_to:to_len/binary>>        */
      if (len < 8)
        return -1;
      from_len = get_int32(buf);
      if (len < (8 + from_len))
        return -1;
      to_len = get_int32(buf + 4 + from_len);
      if (len != (8 + from_len + to_len))
        return -1;
      bn_from = BN_new();
      BN_bin2bn((unsigned char *)(buf + 4), from_len, bn_from);
      bn_rand = BN_new();
      BN_bin2bn((unsigned char *)(buf + 8 + from_len), to_len, bn_rand);
      bn_to = BN_new();
      BN_sub(bn_to, bn_rand, bn_from);
      BN_pseudo_rand_range(bn_rand, bn_to);
      BN_add(bn_rand, bn_rand, bn_from);
      rlen = BN_num_bytes(bn_rand);
      *rbuf = (char *)(bin = driver_alloc_binary(rlen + 4));
      put_int32(bin->orig_bytes, rlen);
      BN_bn2bin(bn_rand,(unsigned char*)(bin->orig_bytes + 4));
      BN_free(bn_rand);
      BN_free(bn_from);
      BN_free(bn_to);
      return rlen + 4;
      break;
      
    case DRV_MOD_EXP:
      /* buf = <<base_len:32/integer,base/binary,          *
       *         exponent_len:32/integer,exponent/binary,  *
       *         modulo_len:32/integer, modulo/binary>>    */
      if (len < 12)
        return -1;
      base_len = get_int32(buf);
      if (len < (12 + base_len))
        return -1;
      exponent_len = get_int32(buf + 4 + base_len);
      if (len < (12 + base_len + exponent_len))
        return -1;
      modulo_len = get_int32(buf + 8 + base_len + exponent_len);
      if (len != (12 + base_len + exponent_len + modulo_len))
        return -1;
      bn_base = BN_new();
      BN_bin2bn((unsigned char *)(buf + 4),
                base_len, bn_base);
      bn_exponent = BN_new();
      BN_bin2bn((unsigned char *)(buf + 8 + base_len),
                exponent_len, bn_exponent);
      bn_modulo = BN_new();
      BN_bin2bn((unsigned char *)(buf + 12 + base_len + exponent_len),
                modulo_len, bn_modulo);
      bn_result = BN_new();
      bn_ctx = BN_CTX_new();
      BN_mod_exp(bn_result, bn_base, bn_exponent,
                 bn_modulo, bn_ctx);
      rlen = BN_num_bytes(bn_result);
      *rbuf = (char *)(bin = driver_alloc_binary(rlen + 4));
      put_int32(bin->orig_bytes, rlen);
      BN_bn2bin(bn_result,(unsigned char*)(bin->orig_bytes + 4));
      BN_free(bn_result);
      BN_free(bn_modulo);
      BN_free(bn_exponent);
      BN_free(bn_base);
      BN_CTX_free(bn_ctx);
      return rlen + 4;
      break;

    case DRV_DSS_VERIFY:
      /* buf = <<data_len:32/integer,data:data_len/binary,
       *         dsa_r:20/binary,
       *         dsa_s:20/binary,
       *         dsa_p_len:32/integer, dsa_p:dsa_p_len/binary,
       *         dsa_q_len:32/integer, dsa_q:dsa_q_len/binary,
       *         dsa_g_len:32/integer, dsa_r:dsa_r_len/binary,
       *         dsa_y_len:32/integer, dsa_y:dsa_y_len/binary>> */
      i = 0;
      j = 0;
      if (len < 60)
        return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 44;
      if (len < (60 + j))
        return -1;
      dsa_p_len = get_int32(buf + i + j);
      j += dsa_p_len; i += 4;
      if (len < (60 + j))
        return -1;
      dsa_q_len = get_int32(buf + i + j);
      j += dsa_q_len; i += 4;
      if (len < (60 + j))
        return -1;
      dsa_g_len = get_int32(buf + i + j);
      j += dsa_g_len; i += 4;
      if (len < (60 + j))
        return -1;
      dsa_y_len = get_int32(buf + i + j);
      j += dsa_y_len;
      if (len < (60 + j))
        return -1;
      i = 4;
      SHA1((unsigned char *) (buf + i), data_len, hmacbuf);
      dsa_sig = DSA_SIG_new();
      dsa_r = BN_new();
      i += data_len;
      BN_bin2bn((unsigned char *)(buf + i), 20, dsa_r);
      i += 20;
      dsa_s = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), 20, dsa_s);
      i += 24;
      dsa_sig->r = dsa_r;
      dsa_sig->s = dsa_s;
      dsa_p = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_p_len, dsa_p);
      i += (dsa_p_len + 4);
      dsa_q = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_q_len, dsa_q);
      i += (dsa_q_len + 4);
      dsa_g = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_g_len, dsa_g);
      i += (dsa_g_len + 4);
      dsa_y = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), dsa_y_len, dsa_y);
      dsa = DSA_new();
      dsa->p = dsa_p;
      dsa->q = dsa_q;
      dsa->g = dsa_g;
      dsa->priv_key = NULL;
      dsa->pub_key = dsa_y;
      i =  DSA_do_verify(hmacbuf, SHA_DIGEST_LENGTH,
                         dsa_sig, dsa);
      *rbuf = (char *)(bin = driver_alloc_binary(1));
      (bin->orig_bytes)[0] = (char)(i & 0xff);
      DSA_free(dsa);
      DSA_SIG_free(dsa_sig);
      return 1;
      break;

    case DRV_RSA_VERIFY:
      /* buf = <<data_len:32/integer, data:data_len/binary,
       *         rsa_s_len:32/integer, rsa_s:rsa_s_len/binary,
       *         rsa_e_len:32/integer, rsa_e:rsa_e_len/binary,
       *         rsa_n_len:32/integer, rsa_n:rsa_n_len/binary>> */
      i = 0;
      j = 0;
      if (len < 16)
        return -1;
      data_len = get_int32(buf + i + j);
      j += data_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_s_len = get_int32(buf + i + j);
      j += rsa_s_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_e_len = get_int32(buf + i + j);
      j += rsa_e_len; i += 4;
      if (len < (16 + j))
        return -1;
      rsa_n_len = get_int32(buf + i + j);
      j += rsa_n_len; i += 4;
      if (len < (16 + j))
        return -1;
      i = 4;
      SHA1((unsigned char *) (buf + i), data_len, hmacbuf);
      i += (data_len + 4);
      rsa_s = (unsigned char *)(buf + i);
      i += (rsa_s_len + 4);
      rsa_e = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_e_len, rsa_e);
      i += (rsa_e_len + 4);
      rsa_n = BN_new();
      BN_bin2bn((unsigned char *)(buf + i), rsa_n_len, rsa_n);
      rsa = RSA_new();
      rsa->n = rsa_n;
      rsa->e = rsa_e;
      i =  RSA_verify(NID_sha1, hmacbuf, SHA_DIGEST_LENGTH,
                      rsa_s, rsa_s_len, rsa);
      *rbuf = (char *)(bin = driver_alloc_binary(1));
      (bin->orig_bytes)[0] = (char)(i & 0xff);
      RSA_free(rsa);
      return 1;
      break;

    case DRV_CBC_AES128_ENCRYPT:
    case DRV_CBC_AES256_ENCRYPT:
    case DRV_CBC_AES128_DECRYPT:
    case DRV_CBC_AES256_DECRYPT:
	/* buf = key[klen] ivec[klen] data */
	if (command == DRV_CBC_AES256_ENCRYPT || command == DRV_CBC_AES256_DECRYPT)
	    klen = 32;
	else 
	    klen = 16;
	dlen = len - klen - 16;
	if (dlen < 0)
	    return -1;
	if (dlen % 16 != 0)
	    return -1;
	if (command == DRV_CBC_AES128_ENCRYPT || command == DRV_CBC_AES256_ENCRYPT) {
	    i = AES_ENCRYPT;
	    AES_set_encrypt_key((unsigned char *) buf, klen*8, &aes_key);
	} else {
	    i = AES_DECRYPT;
	    AES_set_decrypt_key((unsigned char *) buf, klen*8, &aes_key);
	}
	*rbuf = (char *)(bin = driver_alloc_binary(dlen));
	AES_cbc_encrypt((unsigned char *) (buf + klen+16),
			(unsigned char *) bin->orig_bytes,
			dlen,
			&aes_key, 
			(unsigned char *) (buf + klen),
			i);
	return dlen;
	break;
	
/*     case DRV_CBC_AES128_DECRYPT: */
/*     case DRV_CBC_AES256_DECRYPT: */
/* 	/\* buf = key[klen] ivec[16] data *\/ */
/* 	if (command == DRV_CBC_AES256_DECRYPT) */
/* 	    klen = 32; */
/* 	else  */
/* 	    klen = 16; */
/* 	dlen = len - klen - 16; */
/* 	if (dlen < 0) */
/* 	    return -1; */
/* 	*rbuf = (char *)(bin = driver_alloc_binary(dlen)); */
/* 	AES_set_decrypt_key((unsigned char *) buf, klen*8, &aes_key); */
/* 	AES_cbc_encrypt((unsigned char *) (buf + klen+16), */
/* 			(unsigned char *) bin->orig_bytes, */
/* 			dlen, */
/* 			&aes_key,  */
/* 			(unsigned char *) (buf + klen), */
/* 			AES_DECRYPT); */
/* 	return dlen; */
/* 	break; */

    case DRV_XOR:
        /* buf = data1, data2 with same size */
        dlen = len / 2;
        if (len != dlen * 2)
            return -1;
        *rbuf = (char *)(bin = driver_alloc_binary(dlen));
        p = bin->orig_bytes,
        dbuf = buf + dlen;
        for (key = buf, key2 = dbuf; key != dbuf; ++key, ++key2, ++p)
            *p = *key ^ *key2;
        return dlen;
        break;

    case DRV_INFO_LIB:	
	{/* <<DrvVer:8, NameSize:8, Name:NameSize/binary, VerNum:32, VerStr/binary>> */
	    static const char libname[] = "OpenSSL";
	    unsigned name_sz = strlen(libname);
	    const char* ver = SSLeay_version(SSLEAY_VERSION);
	    unsigned ver_sz = strlen(ver);
	    *rbuf = (char*)(bin = driver_alloc_binary(1+1+name_sz+4+ver_sz));
	    p = bin->orig_bytes;
	    *p++ = 0; /* "driver version" for future use */
	    *p++ = name_sz;
	    memcpy(p, libname, name_sz);
	    p += name_sz;
	    put_int32(p,SSLeay()); /* OPENSSL_VERSION_NUMBER */
	    p += 4;
	    memcpy(p, ver, ver_sz);
	}
	return bin->orig_size; 

    default:
	break;
    }
    return -1;
}

#ifdef OPENSSL_THREADS /* vvvvvvvvvvvvvvv OPENSSL_THREADS vvvvvvvvvvvvvvvv */

static INLINE void locking(int mode, ErlDrvRWLock* lock)
{
    switch(mode) {
    case CRYPTO_LOCK|CRYPTO_READ:
	erl_drv_rwlock_rlock(lock);
	break;
    case CRYPTO_LOCK|CRYPTO_WRITE:
	erl_drv_rwlock_rwlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_READ:
	erl_drv_rwlock_runlock(lock);
	break;
    case CRYPTO_UNLOCK|CRYPTO_WRITE:
	erl_drv_rwlock_rwunlock(lock);
	break;
    default:
	ASSERT(!"Invalid lock mode");
    }
}

/* Callback from openssl for static locking
 */
static void locking_function(int mode, int n, const char *file, int line)
{
    ASSERT(n>=0 && n<CRYPTO_num_locks());

    locking(mode, lock_vec[n]);
}

/* Callback from openssl for thread id
 */
static unsigned long id_function(void)
{
    return (unsigned long) erl_drv_thread_self();
}

/* Callbacks for dynamic locking, not used by current openssl version (0.9.8)
 */
static struct CRYPTO_dynlock_value* dyn_create_function(const char *file, int line)
{
    return (struct CRYPTO_dynlock_value*) erl_drv_rwlock_create("crypto_drv_dyn");
}
static void dyn_lock_function(int mode, struct CRYPTO_dynlock_value* ptr,const char *file, int line)
{
    locking(mode, (ErlDrvRWLock*)ptr);
}
static void dyn_destroy_function(struct CRYPTO_dynlock_value *ptr, const char *file, int line)
{
    return erl_drv_rwlock_destroy((ErlDrvRWLock*)ptr);
}

#endif /* ^^^^^^^^^^^^^^^^^^^^^^ OPENSSL_THREADS ^^^^^^^^^^^^^^^^^^^^^^ */

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
