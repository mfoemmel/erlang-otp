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

#ifdef __WIN32__
#include <windows.h>
#endif

#include <erl_driver.h>

#include <openssl/crypto.h>
#include <openssl/sha.h>
#include <openssl/bn.h>
#include <openssl/dsa.h>
#include <openssl/rsa.h>
#include <openssl/objects.h>
#include <openssl/err.h>
#include <openssl/aes.h>

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

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen); 

static ErlDrvEntry crypto_driver_entry = {
  NULL,			/* init */
  start, 		/* called when open_port is invoked */
  stop, 		/* called when port is closed */
  NULL,			/* output */
  NULL,			/* ready_input */
  NULL,			/* ready_output */ 
  "ssh_crypto_drv", 	/* name of this driver */
  NULL,			/* finish */
  NULL,			/* handle */
  control, 		/* called through erlang:port_control */
  NULL,			/* timeout */
  NULL,			/* outputv */
  NULL,			/* ready_aync */
  NULL,			/* flush */
  NULL,			/* call */
  NULL,			/* event */
};

static ErlDrvPort erlang_port = NULL;
static ErlDrvData driver_data = (ErlDrvData) &erlang_port; /* Anything goes */


/* Keep the following definitions in alignment with the FUNC_LIST
 * in ssh_crypto.erl. 
 */

#define DRV_INFO		0
#define DRV_RAND_BYTES		1
#define DRV_RAND_UNIFORM	2
#define DRV_MOD_EXP		3
#define DRV_DSS_VERIFY		4
#define DRV_RSA_VERIFY		5
#define DRV_CBC_AES128_ENCRYPT	6
#define DRV_CBC_AES128_DECRYPT	7

#define NUM_CRYPTO_FUNCS       	7

/* INITIALIZATION AFTER LOADING */

/* 
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to 
 * the driver entry.
 */

DRIVER_INIT(ssh_crypto_drv)
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
  /*
   * This is probably the only place where we have to call
   * CRYPTO_cleanup_all_ex_data();
   * Verify this and remove the other calls, below.
   */
  return;
}

/* Since we are operating in binary mode, the return value from control
 * is irrelevant, as long as it is not negative.
 */

static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
		   int len, char **rbuf, int rlen)
{
  int dlen, i, j, macsize, from_len, to_len;
  int base_len, exponent_len, modulo_len;
  int data_len, digest_len, dsa_p_len, dsa_q_len, dsa_r_len;
  int dsa_s_len, dsa_g_len, dsa_y_len;
  int rsa_e_len, rsa_n_len;
  unsigned int rsa_s_len;
  char *key, *dbuf, *ivec;
  ErlDrvBinary *bin;
  unsigned char hmacbuf[SHA_DIGEST_LENGTH];
  unsigned char *rsa_s;
  BIGNUM *bn_from, *bn_to, *bn_rand, *bn_result;
  BIGNUM *bn_base, *bn_exponent, *bn_modulo;
  BIGNUM *dsa_p, *dsa_q, *dsa_r, *dsa_s, *dsa_g, *dsa_y;
  BIGNUM *rsa_n, *rsa_e;
  BN_CTX *bn_ctx;
  DSA *dsa;
  RSA *rsa;
  DSA_SIG *dsa_sig;
  AES_KEY *aes_key;

  switch(command) {
    
    case DRV_INFO:
      *rbuf = (char*)(bin = driver_alloc_binary(NUM_CRYPTO_FUNCS));
      for (i = 0; i < NUM_CRYPTO_FUNCS; i++) {
	bin->orig_bytes[i] = i + 1;
      }
      return NUM_CRYPTO_FUNCS;
      break;
      
    case DRV_RAND_BYTES:
      /* buf = <<rlen:32/integer>> */

      if (len != 4)
	return -1;
      rlen = get_int32(buf);
      *rbuf = (char *)(bin = driver_alloc_binary(rlen));
      RAND_pseudo_bytes(bin->orig_bytes,rlen);
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
      /* Apparently, the DSA_do_verify operation allocates some space
       * which must be freed this way: */
      CRYPTO_cleanup_all_ex_data();
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
/*       RSA_print_fp(stderr, rsa, 0); */
/*       fprintf(stderr, "data is "); */
/*       for (j = 0; j < SHA_DIGEST_LENGTH; j++) */
/*       { */
/* 	fprintf(stderr, "%.2x", (unsigned char) hmacbuf[j]); */
/*       } */
/*       fprintf(stderr, "\n"); */
/*       fprintf(stderr, "signature is "); */
/*       for (j = 0; j < rsa_s_len; j++) */
/*       { */
/* 	fprintf(stderr, "%.2x", (unsigned char) rsa_s[j]); */
/*       } */
/*       fprintf(stderr, "\n"); */
/*       i =  RSA_verify(NID_sha1, (unsigned char *)(buf + 4), data_len, */
/*  		      rsa_s, rsa_s_len, rsa); */
      i =  RSA_verify(NID_sha1, hmacbuf, SHA_DIGEST_LENGTH,
 		      rsa_s, rsa_s_len, rsa);
      *rbuf = (char *)(bin = driver_alloc_binary(1));
/*       fprintf(stderr, "i = %d\n", i); */
      (bin->orig_bytes)[0] = (char)(i & 0xff);
      RSA_free(rsa);
      /* Apparently, the RSA_verify operation allocates some space
       * which must be freed this way, but perhaps it would suffice to
       * do it in stop()? */
      CRYPTO_cleanup_all_ex_data();
      return 1;
      break;

    case DRV_CBC_AES128_ENCRYPT:
      /* buf = <<key:16/binary, ivec:16/binary, data/binary>> */
      dlen = len - 32;
      if (dlen < 0)
	return -1;
      /* There is no AES_KEY_new, probably meant to be used through
	 EVP_xxx, so: */
      aes_key = driver_alloc(sizeof(AES_KEY));
      *rbuf = (char *)(bin = driver_alloc_binary(dlen));
      AES_set_encrypt_key((unsigned char *) buf,
			  128,
			  aes_key);
      AES_cbc_encrypt((unsigned char *) (buf + 32),
		      (unsigned char *) bin->orig_bytes,
		      dlen,
		      aes_key, 
		      (unsigned char *) (buf + 16),
		      AES_ENCRYPT);
      driver_free(aes_key);
      return dlen;
      break;

    case DRV_CBC_AES128_DECRYPT:
      /* buf = key[16] ivec[16] data */
      dlen = len - 32;
      if (dlen < 0)
	return -1;
      /* There is no AES_KEY_new, probably meant to be used through
	 EVP_xxx, so: */
      aes_key = driver_alloc(sizeof(AES_KEY));
      *rbuf = (char *)(bin = driver_alloc_binary(dlen));
      AES_set_decrypt_key((unsigned char *) buf,
			  128,
			  aes_key);
      AES_cbc_encrypt((unsigned char *) (buf + 32),
		      (unsigned char *) bin->orig_bytes,
		      dlen,
		      aes_key, 
		      (unsigned char *) (buf + 16),
		      AES_DECRYPT);
      driver_free(aes_key);
      return dlen;
      break;

    default:
      break;
  }
  return -1;
}
