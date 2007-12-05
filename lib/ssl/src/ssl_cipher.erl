%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%%----------------------------------------------------------------------
%% Purpose: Help functions for handling the SSL ciphers
%% 
%%----------------------------------------------------------------------

-module(ssl_cipher).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_pkix.hrl").
-include("ssl_debug.hrl").

-export([master_secret/4, setup_keys/9, get_handshake_hashes/4, mac_hash/7,
	 security_parameters/2, suite_definition/1,
	 rsa_encrypt/2, rsa_decrypt/3,
	 digest/2, decipher/4, cipher/4, % hmac_hash/3
	 verify_signature/5, digitally_sign/3,
	 suite/1, suites/1,
	 openssl_suite/1, openssl_suite_name/1,
	 format_encryption_block/3, unformat_encryption_block/2]).

master_secret(#protocol_version{major = 3, minor = 0}, 
	      PremasterSecret, ClientRandom, ServerRandom) ->
    ssl_ssl3:master_secret(PremasterSecret, ClientRandom, ServerRandom);

master_secret(#protocol_version{major = 3, minor = N},
	      PremasterSecret, ClientRandom, ServerRandom) when  N == 1;
								 N == 2 ->
    ssl_tls1:master_secret(PremasterSecret, ClientRandom, ServerRandom).

setup_keys(#protocol_version{major = 3, minor = 0}, Exportable, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, EKML, IVS) ->
    ssl_ssl3:setup_keys(Exportable, MasterSecret, ServerRandom, 
			ClientRandom, HashSize, KML, EKML, IVS);

setup_keys(#protocol_version{major = 3, minor = N}, _Exportable, MasterSecret,
	   ServerRandom, ClientRandom, HashSize, KML, _EKML, IVS) when  N == 1; 
								 N == 2 ->
    ssl_tls1:setup_keys(MasterSecret, ServerRandom, ClientRandom, HashSize, 
			KML, IVS).

get_handshake_hashes(#protocol_version{major = 3, minor = 0},
		Role, MasterSecret, Hashes) ->
    ssl_ssl3:get_handshake_hashes(Role, MasterSecret, Hashes);
get_handshake_hashes(#protocol_version{major = 3, minor = N},
		Role, MasterSecret, Hashes) when  N == 1; 
						  N == 2 ->
    ssl_tls1:get_handshake_hashes(Role, MasterSecret, Hashes).


mac_hash(?NULL, #protocol_version{}, _MacSecret, _SeqNo, _Type,
	 _Length, _Fragment) ->
    <<>>;
mac_hash(MacAlg, #protocol_version{major = 3, minor = 0},
	 MacSecret, SeqNo, Type, Length, Fragment) ->
    ssl_ssl3:mac_hash(MacAlg, MacSecret, SeqNo, Type, Length, Fragment);
mac_hash(MacAlg, #protocol_version{major = 3, minor = N},
	 MacSecret, SeqNo, Type, Length, Fragment)  when  N == 1; N == 2 ->
    ssl_tls1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Length, Fragment).

%%--------------------------------------------------------------------
%% Function: security_parameters(CipherSuite, SecParams) -> 
%%                                              #security_parameters{}
%%
%% CipherSuite - as defined in ssl_cipher.hrl
%% SecParams - #security_parameters{}
%%
%% Description: Returns a security parameters record where the
%% cipher values has been updated according to <CipherSuite> 
%%-------------------------------------------------------------------
security_parameters(CipherSuite, SecParams) ->
    { _, Cipher, Hash, Exportable} = 
	suite_definition(CipherSuite),
    
    SecParams#security_parameters{
      cipher_suite = CipherSuite,
      bulk_cipher_algorithm = bulk_cipher_algorithm(Cipher),
      cipher_type = type(Cipher),
      key_size = effective_key_bits(Cipher),
      expanded_key_material_length = expanded_key_material(Cipher),
      key_material_length = key_material(Cipher),
      iv_size =  iv_size(Cipher),
      mac_algorithm = mac_algorithm(Hash),
      hash_size = hash_size(Hash),
      exportable = Exportable}.

%%--------------------------------------------------------------------
%% Function: cipher(Method, CipherState, Mac, Data) -> {Encrypted, UpdateCipherState}
%%
%% Method - integer() (as defined in ssl_cipher.hrl)
%% CipherState, UpdatedCipherState - #cipher_state{}
%% Data, Encrypted - binary()
%%
%% Description: Encrypts the data and the mac using method, updating
%% the cipher state
%%-------------------------------------------------------------------
cipher(?NULL, CipherState, <<>>, Fragment) ->
    {generic_stream_cipher_to_list(
       #generic_stream_cipher{content=Fragment, mac= <<>>}), CipherState};
cipher(?RC4, CipherState, Mac, Fragment) ->
    State0 = case CipherState#cipher_state.state of
                 undefined -> crypto:rc4_set_key(CipherState#cipher_state.key);
                 S -> S
             end,
    GSC = #generic_stream_cipher{content = Fragment, mac = Mac},
    L = generic_stream_cipher_to_list(GSC),
    ?DBG_HEX(L),
    ?DBG_HEX(State0),
    {State1, T} = 
	crypto:rc4_encrypt_with_state(State0, L),
    ?DBG_HEX(T),
    {T, CipherState#cipher_state{state = State1}};
cipher(?DES, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:des_cbc_encrypt(Key, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment);
cipher(?DES40, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:des_cbc_encrypt(Key, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment);
cipher(?'3DES', CipherState, Mac, Fragment) ->
    block_cipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			 crypto:des3_cbc_encrypt(K1, K2, K3, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment);
cipher(?AES, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) when size(Key) == 16 ->
			 crypto:aes_cbc_128_encrypt(Key, IV, T);
		    (Key, IV, T) when size(Key) == 32 ->
			 crypto:aes_cbc_256_encrypt(Key, IV, T)
		 end, block_size(aes_128_cbc), CipherState, Mac, Fragment);
%% cipher(?IDEA, CipherState, Mac, Fragment) ->
%%     block_cipher(fun(Key, IV, T) ->
%% 			 crypto:idea_cbc_encrypt(Key, IV, T)
%% 		 end, block_size(idea_cbc), CipherState, Mac, Fragment);
cipher(?RC2, CipherState, Mac, Fragment) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:rc2_40_cbc_encrypt(Key, IV, T)
		 end, block_size(rc2_cbc_40), CipherState, Mac, Fragment).

block_cipher(Fun, BlockSz, CipherState0, Mac, Fragment ) ->
    #cipher_state{key=Key, iv=IV} = CipherState0,
    TotSz = size(Mac)+erlang:iolist_size(Fragment)+1,
    {Padding, PaddingLength} = get_padding(TotSz, BlockSz),
    GBC = #generic_block_cipher{content = Fragment,
				mac = Mac,
				padding = Padding,
				padding_length = PaddingLength},
    L = generic_block_cipher_to_list(GBC),
    ?DBG_HEX(Key),
    ?DBG_HEX(IV),
    ?DBG_HEX(L),
    T = Fun(Key, IV, L),
    ?DBG_HEX(T),
    NextIV = next_iv(T, IV),
    CipherState1 = CipherState0#cipher_state{iv=NextIV},
    {T, CipherState1}.

%%--------------------------------------------------------------------
%% Function: decipher(Method, CipherState, Mac, Data) -> {Decrypted, UpdateCipherState}
%%
%% Method - integer() (as defined in ssl_cipher.hrl)
%% CipherState, UpdatedCipherState - #cipher_state{}
%% Data, Encrypted - binary()
%%
%% Description: Decrypts the data and the mac using method, updating
%% the cipher state
%%-------------------------------------------------------------------
decipher(?NULL, _HashSz, CipherState, Fragment) ->
    {Fragment, <<>>, CipherState};
decipher(?RC4, HashSz, CipherState, Fragment) ->
    ?DBG_TERM(CipherState#cipher_state.key),
    State0 = case CipherState#cipher_state.state of
                 undefined -> crypto:rc4_set_key(CipherState#cipher_state.key);
                 S -> S
             end,
    ?DBG_HEX(State0),
    ?DBG_HEX(Fragment),
    {State1, T} = crypto:rc4_encrypt_with_state(State0, Fragment),
    ?DBG_HEX(T),
    GSC = generic_stream_cipher_from_bin(T, HashSz),
    #generic_stream_cipher{content=Content, mac=Mac} = GSC,
    {Content, Mac, CipherState#cipher_state{state=State1}};
decipher(?DES, HashSz, CipherState, Fragment) ->
    block_decipher(fun(Key, IV, T) ->
			   crypto:des_cbc_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment);
decipher(?DES40, HashSz, CipherState, Fragment) ->
    block_decipher(fun(Key, IV, T) ->
			   crypto:des_cbc_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment);
decipher(?'3DES', HashSz, CipherState, Fragment) ->
    block_decipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			   crypto:des3_cbc_decrypt(K1, K2, K3, IV, T)
		   end, CipherState, HashSz, Fragment);
decipher(?AES, HashSz, CipherState, Fragment) ->
    block_decipher(fun(Key, IV, T) when size(Key) == 16 ->
			   crypto:aes_cbc_128_decrypt(Key, IV, T);
		      (Key, IV, T) when size(Key) == 32 ->
			   crypto:aes_cbc_256_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment);
%% decipher(?IDEA, HashSz, CipherState, Fragment) ->
%%     block_decipher(fun(Key, IV, T) ->
%%  			   crypto:idea_cbc_decrypt(Key, IV, T)
%%  		   end, CipherState, HashSz, Fragment);
decipher(?RC2, HashSz, CipherState, Fragment) ->
    block_decipher(fun(Key, IV, T) ->
			   crypto:rc2_40_cbc_decrypt(Key, IV, T)
		   end, CipherState, HashSz, Fragment).

block_decipher(Fun, CipherState0, HashSz, Fragment) ->
    #cipher_state{key=Key, iv=IV} = CipherState0,
    ?DBG_HEX(Key),
    ?DBG_HEX(IV),
    ?DBG_HEX(Fragment),
    T = Fun(Key, IV, Fragment),
    ?DBG_HEX(T),
    GBC = generic_block_cipher_from_bin(T, HashSz),
    ok = check_padding(GBC),  %% TODO kolla också...
    Content = GBC#generic_block_cipher.content,
    Mac = GBC#generic_block_cipher.mac,
    CipherState1 = CipherState0#cipher_state{iv=next_iv(Fragment, IV)},
    {Content, Mac, CipherState1}.

%%--------------------------------------------------------------------
%% Function: rsa_encrypt(Plain, Key) -> Encrypted
%% Function: rsa_encrypt(Plain, Mod, Exp) -> Encrypted
%%
%% Plain - integer()
%% Key -  #'RSAPublicKey'{} | #'RSAPrivateKey'{}
%% Mod -   integer()
%% Exp -   integer()
%% Encrypted - binary()
%%
%% Description: Rsa encrypts <Plain> using the parameters <Mod> and <Exp>
%%-------------------------------------------------------------------
rsa_encrypt(Plain, #'RSAPublicKey'{modulus=M, publicExponent=E}) ->
    rsa_mod_exp(Plain, M, E);
rsa_encrypt(Plain, #'RSAPrivateKey'{modulus=M, privateExponent=E}) ->
    rsa_mod_exp(Plain, M, E).

%%--------------------------------------------------------------------
%% Function: rsa_decrypt(Encrypted, Mod, Exp) -> Plain
%%
%% Encrypted - iolist()
%% Mod -       integer()
%% Exp -       integer()
%% Plain -     binary()
%%
%% Description: Rsa encrypts <Plain> using the parameters <Mod> and <Exp>
%%-------------------------------------------------------------------
rsa_decrypt(Encrypted, Mod, Exp) ->
    rsa_mod_exp(Encrypted, Mod, Exp).

rsa_mod_exp(Encrypted, Mod, Exp) ->
    mpint2binary(crypto:mod_exp(iolist2mpint(Encrypted), crypto:mpint(Exp),
			crypto:mpint(Mod))). % stupid crypto got it backwards!?


%%--------------------------------------------------------------------
%% Function: digest(Algorithm, Cert) -> Digest
%%
%% Algorithm = pkix1 algorithm
%% Cert = binary() - asn1 cert
%% Digest = binary()
%%
%% Description: Calculates the digest of <Cert> using <Algorithm>.  
%%-------------------------------------------------------------------
digest(sha1WithRSAEncryption, Cert) ->
    TBSCert = ssl_pkix:encoded_tbs_cert(Cert),
    crypto:sha(TBSCert);

digest(md5WithRSAEncryption, Cert) ->
    TBSCert = ssl_pkix:encoded_tbs_cert(Cert),
    crypto:md5(TBSCert).

%%--------------------------------------------------------------------
%% Function: verify_signature(Algorithm, Digest, Signature, Key, Params) -> 
%%                                                     true | false
%% Algorithm = pkix1 algorithm
%% Digest = binary()
%% Signature = binary()
%% Key =  pkix1 key
%% Params =  pkix1 params
%%
%% Description: Verifies the signature <Signature> by encrypting
%% the Signature using Key and Params and comapring it to Digest.
%%-------------------------------------------------------------------
verify_signature(rsaEncryption, Digest, Signature, Key, _Params) ->
    B = rsa_encrypt(Signature, Key), 		% 10.2.2, rfc2313
    D = unformat_encryption_block(B, 1),%% TODO:  check that it's really type 1 formatted!
    Dig = ssl_pkix:signature_digest(D),		% 10.2.3
    Dig == Digest.				% 10.2.4
%% TODO: more claused of verify_signature, DSS

%%--------------------------------------------------------------------
%% Function: digitally_sign(Algorithm, Digest, Key, Params) -> 
%%						Signature
%% Algorithm = pkix1 algorithm
%% Digest = binary()
%% Signature = binary()
%% Key =  pkix1 key
%% Params =  pkix1 params
%%
%% Description: create a signature
%%-------------------------------------------------------------------
digitally_sign(rsa, Hashes, Key) ->
    Sz = int_byte_size(Key#'RSAPrivateKey'.modulus),
    F = format_encryption_block(Hashes, Sz+1, 1),
    ?DBG_TERM(Key),
    rsa_encrypt(F, Key).
%% TODO: more claused of digitally_sign, DSS
    

%%--------------------------------------------------------------------
%% Function: suites(Version) -> [Suite]
%%
%% Version = version()
%% Suite = binary() from ssl_cipher.hrl
%%
%% Description: Returns a list of supported cipher suites.
%%--------------------------------------------------------------------
suites(#protocol_version{major = 3, minor = 0}) ->
    ssl_ssl3:suites();
suites(#protocol_version{major = 3, minor = N}) when N == 1; N == 2 ->
    ssl_tls1:suites().

%%--------------------------------------------------------------------
%% Function: suite_definition(CipherSuite) -> 
%%                                {KeyExchange, Cipher, Hash, Exportable}
%%                                             
%%
%% CipherSuite - as defined in ssl_cipher.hrl
%% KeyExchange - rsa | dh_dss | dh_rsa | dh_anon | dhe_dss | dhe_rsa
%%               krb5 | *_export (old ssl)
%% Cipher      - null | rc4_128 | idea_cbc | des_cbc | '3des_ede_cbc'
%%               des40_cbc | dh_dss | aes_128_cbc | aes_256_cbc |
%%               rc2_cbc_40 | rc4_40 
%% Hash        - null | md5 | sha
%% Exportable  - export | no_export | ignore(?)
%%
%% Description: Returns a security parameters record where the
%% cipher values has been updated according to <CipherSuite> 
%% Note: since idea is unsupported on the openssl version used by
%% crypto (as of OTP R12B), we've commented away the idea stuff
%%-------------------------------------------------------------------
%% TLS v1.1 suites
suite_definition(?TLS_NULL_WITH_NULL_NULL) ->
    {null, null, null, ignore};
suite_definition(?TLS_RSA_WITH_NULL_MD5) ->
    {rsa, null, md5, ignore};
suite_definition(?TLS_RSA_WITH_NULL_SHA) ->
    {rsa, null, sha, ignore};			
suite_definition(?TLS_RSA_WITH_RC4_128_MD5) ->	% ok
    {rsa, rc4_128, md5, no_export};
suite_definition(?TLS_RSA_WITH_RC4_128_SHA) ->	% ok
    {rsa, rc4_128, sha, no_export};
%% suite_definition(?TLS_RSA_WITH_IDEA_CBC_SHA) -> % unsupported
%%     {rsa, idea_cbc, sha, no_export};
suite_definition(?TLS_RSA_WITH_DES_CBC_SHA) ->	% ok
    {rsa, des_cbc, sha, no_export}; 
suite_definition(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    {rsa, '3des_ede_cbc', sha, no_export}; 
suite_definition(?TLS_DH_DSS_WITH_DES_CBC_SHA) ->
    {dh_dss, des_cbc, sha, no_export};
suite_definition(?TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA) ->
    {dh_dss, '3des_ede_cbc', sha, no_export};
suite_definition(?TLS_DH_RSA_WITH_DES_CBC_SHA) ->
    {dh_rsa, des_cbc, sha, no_export};
suite_definition(?TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA) ->
    {dh_rsa, '3des_ede_cbc', sha, no_export};
suite_definition(?TLS_DHE_DSS_WITH_DES_CBC_SHA) ->
    {dhe_dss, des_cbc, sha, no_export};
suite_definition(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    {dhe_dss, '3des_ede_cbc', sha, no_export};
suite_definition(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    {dhe_rsa, des_cbc, sha, no_export};
suite_definition(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    {dhe_rsa, '3des_ede_cbc', sha, no_export}; 
suite_definition(?TLS_DH_anon_WITH_RC4_128_MD5) ->
    {dh_anon, rc4_128, md5, no_export};
suite_definition(?TLS_DH_anon_WITH_DES_CBC_SHA) ->
    {dh_anon, des40_cbc, sha, no_export};
suite_definition(?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA) ->
    {dh_anon, '3des_ede_cbc', sha, no_export};

%%% TSL V1.1 AES suites
suite_definition(?TLS_RSA_WITH_AES_128_CBC_SHA) -> % ok
    {rsa, aes_128_cbc, sha, ignore};
suite_definition(?TLS_DH_DSS_WITH_AES_128_CBC_SHA) ->
    {dh_dss, aes_128_cbc, sha, ignore};
suite_definition(?TLS_DH_RSA_WITH_AES_128_CBC_SHA) ->
    {dh_rsa, aes_128_cbc, sha, ignore};
suite_definition(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    {dhe_dss, aes_128_cbc, sha, ignore};
suite_definition(?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    {dhe_rsa, aes_128_cbc, sha, ignore};
suite_definition(?TLS_DH_anon_WITH_AES_128_CBC_SHA) ->
    {dh_anon, aes_128_cbc, sha, ignore};
suite_definition(?TLS_RSA_WITH_AES_256_CBC_SHA) -> % ok
    {rsa, aes_256_cbc, sha, ignore};
suite_definition(?TLS_DH_DSS_WITH_AES_256_CBC_SHA) ->
    {dh_dss, aes_256_cbc, sha, ignore};
suite_definition(?TLS_DH_RSA_WITH_AES_256_CBC_SHA) ->
    {dh_rsa, aes_256_cbc, sha, ignore};
suite_definition(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    {dhe_dss, aes_256_cbc, sha, ignore};
suite_definition(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    {dhe_rsa, aes_256_cbc, sha, ignore};
suite_definition(?TLS_DH_anon_WITH_AES_256_CBC_SHA) ->
    {dh_anon, aes_256_cbc, sha, ignore};

%% TSL V1.1 KRB SUITES
suite_definition(?TLS_KRB5_WITH_DES_CBC_SHA) ->
    {krb5, des_cbc, sha, ignore};
suite_definition(?TLS_KRB5_WITH_3DES_EDE_CBC_SHA) ->
    {krb5, '3des_ede_cbc', sha, ignore};
suite_definition(?TLS_KRB5_WITH_RC4_128_SHA) ->
    {krb5, rc4_128, sha, ignore};
%% suite_definition(?TLS_KRB5_WITH_IDEA_CBC_SHA) ->
%%     {krb5, idea_cbc, sha, ignore};
suite_definition(?TLS_KRB5_WITH_DES_CBC_MD5) ->
    {krb5, des_cbc, md5, ignore};
suite_definition(?TLS_KRB5_WITH_3DES_EDE_CBC_MD5) ->
    {krb5, '3des_ede_cbc', md5, ignore};
suite_definition(?TLS_KRB5_WITH_RC4_128_MD5) ->
    {krb5, rc4_128, md5, ignore};
%% suite_definition(?TLS_KRB5_WITH_IDEA_CBC_MD5) ->
%%     {krb5, idea_cbc, md5, ignore};

suite_definition(?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5) ->
    {rsa, rc4_56, md5, export};
suite_definition(?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5) ->
    {rsa, rc2_cbc_56, md5, export};
suite_definition(?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA) ->
    {rsa, des_cbc, sha, export};
suite_definition(?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA) ->
    {dhe_dss, des_cbc, sha, export};
suite_definition(?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA) ->
    {rsa, rc4_56, sha, export};
suite_definition(?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA) ->
    {dhe_dss, rc4_56, sha, export};
suite_definition(?TLS_DHE_DSS_WITH_RC4_128_SHA) ->
    {dhe_dss, rc4_128, sha, export};

%% Export suites  TLS 1.0 OR SSLv3-only servers.  
suite_definition(?TLS_KRB5_EXPORT_WITH_DES_CBC_40_SHA) ->
    {krb5_export, des40_cbc, sha, export};
suite_definition(?TLS_KRB5_EXPORT_WITH_RC2_CBC_40_SHA) ->
    {krb5_export, rc2_cbc_40, sha, export};
suite_definition(?TLS_KRB5_EXPORT_WITH_RC4_40_SHA) ->
    {krb5_export, des40_cbc, sha, export};
suite_definition(?TLS_KRB5_EXPORT_WITH_DES_CBC_40_MD5) ->
    {krb5_export, des40_cbc, md5, export};
suite_definition(?TLS_KRB5_EXPORT_WITH_RC2_CBC_40_MD5) ->
    {krb5_export, rc2_cbc_40, md5, export};
suite_definition(?TLS_KRB5_EXPORT_WITH_RC4_40_MD5) ->
    {krb5_export, rc2_cbc_40, md5, export};
suite_definition(?TLS_RSA_EXPORT_WITH_RC4_40_MD5) -> % ok
    {rsa, rc4_40, md5, export};	
suite_definition(?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5) -> % ok
    {rsa, rc2_cbc_40, md5, export};
suite_definition(?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA) ->
    {rsa, des40_cbc, sha, export};
suite_definition(?TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA) ->
    {dh_dss, des40_cbc, sha, export};
suite_definition(?TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA) ->
    {dh_rsa, des40_cbc, sha, export};
suite_definition(?TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA) ->
    {dhe_dss, des40_cbc, sha, export};
suite_definition(?TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA) ->
    {dhe_rsa, des40_cbc, sha, export};
suite_definition(?TLS_DH_anon_EXPORT_WITH_RC4_40_MD5) ->
    {dh_anon, rc4_40, md5, export};
suite_definition(?TLS_DH_anon_EXPORT_WITH_DES40_CBC_SHA) ->
    {dh_anon, des40_cbc, sha, export}.

%% TLS v1.1 suites
suite({rsa, null, md5, ignore}) ->
    ?TLS_RSA_WITH_NULL_MD5;
suite({rsa, null, sha, ignore}) ->
    ?TLS_RSA_WITH_NULL_SHA;
suite({rsa, rc4_128, md5, no_export}) ->
    ?TLS_RSA_WITH_RC4_128_MD5;
suite({rsa, rc4_128, sha, no_export}) ->
    ?TLS_RSA_WITH_RC4_128_SHA;
%% suite({rsa, idea_cbc, sha, no_export}) -> 
%%     ?TLS_RSA_WITH_IDEA_CBC_SHA;
suite({rsa, des_cbc, sha, no_export}) ->
    ?TLS_RSA_WITH_DES_CBC_SHA; 
suite({rsa, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA; 
suite({dh_dss, des_cbc, sha, no_export}) ->
    ?TLS_DH_DSS_WITH_DES_CBC_SHA;
suite({dh_dss, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_DH_DSS_WITH_3DES_EDE_CBC_SHA;
suite({dh_rsa, des_cbc, sha, no_export}) ->
    ?TLS_DH_RSA_WITH_DES_CBC_SHA;
suite({dh_rsa, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_DH_RSA_WITH_3DES_EDE_CBC_SHA;
suite({dhe_dss, des_cbc, sha, no_export}) ->
    ?TLS_DHE_DSS_WITH_DES_CBC_SHA;
suite({dhe_dss, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
suite({dhe_rsa, des_cbc, sha, no_export}) ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
suite({dhe_rsa, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA; 
suite({dh_anon, rc4_128, md5, no_export}) ->
    ?TLS_DH_anon_WITH_RC4_128_MD5;
suite({dh_anon, des40_cbc, sha, no_export}) ->
    ?TLS_DH_anon_WITH_DES_CBC_SHA;
suite({dh_anon, '3des_ede_cbc', sha, no_export}) ->
    ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA;

%%% TSL V1.1 AES suites
suite({rsa, aes_128_cbc, sha, ignore}) ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA; 
suite({dh_dss, aes_128_cbc, sha, ignore}) ->
    ?TLS_DH_DSS_WITH_AES_128_CBC_SHA;
suite({dh_rsa, aes_128_cbc, sha, ignore}) ->
     ?TLS_DH_RSA_WITH_AES_128_CBC_SHA;
suite({dhe_dss, aes_128_cbc, sha, ignore}) ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA; 
suite({dhe_rsa, aes_128_cbc, sha, ignore}) ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
suite({dh_anon, aes_128_cbc, sha, ignore}) ->
    ?TLS_DH_anon_WITH_AES_128_CBC_SHA;
suite({rsa, aes_256_cbc, sha, ignore}) ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
suite({dh_dss, aes_256_cbc, sha, ignore}) ->
    ?TLS_DH_DSS_WITH_AES_256_CBC_SHA;
suite({dh_rsa, aes_256_cbc, sha, ignore}) ->
    ?TLS_DH_RSA_WITH_AES_256_CBC_SHA;
suite({dhe_dss, aes_256_cbc, sha, ignore}) ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
suite({dhe_rsa, aes_256_cbc, sha, ignore}) ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
suite({dh_anon, aes_256_cbc, sha, ignore}) ->
    ?TLS_DH_anon_WITH_AES_256_CBC_SHA;

%% TSL V1.1 KRB SUITES
suite({krb5, des_cbc, sha, ignore}) ->
    ?TLS_KRB5_WITH_DES_CBC_SHA;
suite({krb5_cbc, '3des_ede_cbc', sha, ignore}) ->
    ?TLS_KRB5_WITH_3DES_EDE_CBC_SHA;
suite({krb5, rc4_128, sha, ignore}) ->
     ?TLS_KRB5_WITH_RC4_128_SHA;
%% suite({krb5_cbc, idea_cbc, sha, ignore}) ->
%%     ?TLS_KRB5_WITH_IDEA_CBC_SHA;
suite({krb5_cbc, md5, ignore}) ->
    ?TLS_KRB5_WITH_DES_CBC_MD5;
suite({krb5_ede_cbc, des_cbc, md5, ignore}) ->
    ?TLS_KRB5_WITH_3DES_EDE_CBC_MD5;
suite({krb5_128, rc4_128, md5, ignore}) ->
    ?TLS_KRB5_WITH_RC4_128_MD5;
%% suite({krb5, idea_cbc, md5, ignore}) ->
%%     ?TLS_KRB5_WITH_IDEA_CBC_MD5;

%% Export suites  TLS 1.0 OR SSLv3-only servers.  
suite({rsa, rc4_40, md5, export}) ->	
    ?TLS_RSA_EXPORT_WITH_RC4_40_MD5;
suite({rsa, rc2_cbc_40, md5, export}) ->
    ?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5;
suite({rsa, des40_cbc, sha, export}) ->
    ?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA;
suite({rsa, rc4_56, md5, export}) ->
    ?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5;
suite({rsa, rc2_cbc_56, md5, export}) -> 
    ?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5;
suite({rsa, des_cbc, sha, export}) ->
    ?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA;
suite({dhe_dss, des_cbc, sha, export}) ->
    ?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA;
suite({rsa, rc4_56, sha, export}) ->
    ?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA;
suite({dhe_dss, rc4_56, sha, export}) ->
    ?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA;
suite({dhe_dss, rc4_128, sha, export}) ->
    ?TLS_DHE_DSS_WITH_RC4_128_SHA;
suite({krb5_export, des40_cbc, sha, export}) ->
    ?TLS_KRB5_EXPORT_WITH_DES_CBC_40_SHA;
suite({krb5_export, rc2_cbc_40, sha, export}) ->
    ?TLS_KRB5_EXPORT_WITH_RC2_CBC_40_SHA;
suite({krb5_export, rc4_cbc_40, sha, export}) ->
    ?TLS_KRB5_EXPORT_WITH_RC4_40_SHA;
suite({krb5_export, des40_cbc, md5, export}) ->
     ?TLS_KRB5_EXPORT_WITH_DES_CBC_40_MD5;
suite({krb5_export, rc2_cbc_40, md5, export}) ->
    ?TLS_KRB5_EXPORT_WITH_RC2_CBC_40_MD5;
suite({krb5_export, rc4_cbc_40, md5, export}) ->
     ?TLS_KRB5_EXPORT_WITH_RC4_40_MD5;
suite({rsa_export, rc4_cbc_40, md5, export}) ->
    ?TLS_RSA_EXPORT_WITH_RC4_40_MD5;	
suite({rsa_export, rc2_cbc_40, md5, export}) ->
    ?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5;
suite({rsa_export, des40_cbc, sha, export}) ->
    ?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA;
suite({dh_dss_export, des40_cbc, sha, export}) ->
    ?TLS_DH_DSS_EXPORT_WITH_DES40_CBC_SHA;
suite({dh_rsa_export, des40_cbc, sha, export}) ->
    ?TLS_DH_RSA_EXPORT_WITH_DES40_CBC_SHA;
suite({dhe_dss_export, des40_cbc, sha, export}) ->
    ?TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
suite({dhe_rsa_export, des40_cbc, sha, export}) ->
    ?TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
suite({dh_anon_export, rc4_40, md5, export}) ->
    ?TLS_DH_anon_EXPORT_WITH_RC4_40_MD5;
suite({dh_anon_export, des40_cbc, sha, export}) ->
    ?TLS_DH_anon_EXPORT_WITH_DES40_CBC_SHA.


%% translate constants <-> openssl-strings
%% TODO: Is there a pattern in the nameing
%% that is useable to make a nicer function defention?

openssl_suite("DHE-RSA-AES256-SHA") ->
    ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA;
openssl_suite("DHE-DSS-AES256-SHA") ->
    ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA;
openssl_suite("AES256-SHA") ->
    ?TLS_RSA_WITH_AES_256_CBC_SHA;
openssl_suite("EDH-RSA-DES-CBC3-SHA") ->
    ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("EDH-DSS-DES-CBC3-SHA") ->
    ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA;
openssl_suite("DES-CBC3-SHA") ->
    ?TLS_RSA_WITH_3DES_EDE_CBC_SHA;
openssl_suite("DHE-RSA-AES128-SHA") ->
    ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA;
openssl_suite("DHE-DSS-AES128-SHA") ->
    ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA;
openssl_suite("AES128-SHA") ->
    ?TLS_RSA_WITH_AES_128_CBC_SHA;
%% TODO: Do we want to support this?
%% openssl_suite("DHE-DSS-RC4-SHA") ->
%%     ?TLS_DHE_DSS_WITH_RC4_128_SHA;
%%openssl_suite("IDEA-CBC-SHA") ->
%%    ?TLS_RSA_WITH_IDEA_CBC_SHA;
openssl_suite("RC4-SHA") ->
    ?TLS_RSA_WITH_RC4_128_SHA;
openssl_suite("RC4-MD5") -> 
    ?TLS_RSA_WITH_RC4_128_MD5;
%% TODO: Do we want to support this?
openssl_suite("EXP1024-RC4-MD5") ->
    ?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5;
openssl_suite("EXP1024-RC2-CBC-MD5") ->
    ?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5;
openssl_suite("EXP1024-DES-CBC-SHA") ->
    ?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA;
openssl_suite("EXP1024-DHE-DSS-DES-CBC-SHA") ->
    ?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA;
openssl_suite("EXP1024-RC4-SHA") ->
    ?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA;
openssl_suite("EXP1024-DHE-DSS-RC4-SHA") ->
    ?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA;
openssl_suite("DHE-DSS-RC4-SHA") ->
    ?TLS_DHE_DSS_WITH_RC4_128_SHA;

openssl_suite("EDH-RSA-DES-CBC-SHA") ->
    ?TLS_DHE_RSA_WITH_DES_CBC_SHA;
openssl_suite("DES-CBC-SHA") ->
    ?TLS_RSA_WITH_DES_CBC_SHA;
openssl_suite("EXP-EDH-RSA-DES-CBC-SHA") ->
    ?TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA;
openssl_suite("EXP-EDH-DSS-DES-CBC-SHA") ->
    ?TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA;
openssl_suite("EXP-DES-CBC-SHA") ->
    ?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA;
openssl_suite("EXP-RC2-CBC-MD5") ->
    ?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5;
openssl_suite("EXP-RC4-MD5") -> 
    ?TLS_RSA_EXPORT_WITH_RC4_40_MD5.

openssl_suite_name(?TLS_DHE_RSA_WITH_AES_256_CBC_SHA) ->
    "DHE-RSA-AES256-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_256_CBC_SHA) ->
    "DHE-DSS-AES256-SHA";
openssl_suite_name(?TLS_RSA_WITH_AES_256_CBC_SHA) ->
    "AES256-SHA";
openssl_suite_name(?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "EDH-RSA-DES-CBC3-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA) ->
    "EDH-DSS-DES-CBC3-SHA";
openssl_suite_name(?TLS_RSA_WITH_3DES_EDE_CBC_SHA) ->
    "DES-CBC3-SHA";
openssl_suite_name( ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA) ->
    "DHE-RSA-AES128-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_AES_128_CBC_SHA) ->
    "DHE-DSS-AES128-SHA";
openssl_suite_name(?TLS_RSA_WITH_AES_128_CBC_SHA) ->
    "AES128-SHA";
%% openssl_suite_name(?TLS_RSA_WITH_IDEA_CBC_SHA) ->
%%     "IDEA-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_SHA) ->
    "RC4-SHA";
openssl_suite_name(?TLS_RSA_WITH_RC4_128_MD5) -> 
    "RC4-MD5";
openssl_suite_name(?TLS_DHE_RSA_WITH_DES_CBC_SHA) ->
    "EDH-RSA-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_WITH_DES_CBC_SHA) ->
    "DES-CBC-SHA";
openssl_suite_name(?TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA) ->
    "EXP-EDH-RSA-DES-CBC-SHA";
openssl_suite_name(?TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA) ->
    "EXP-EDH-DSS-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA) ->
    "EXP-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5) ->
    "EXP-RC2-CBC-MD5";
openssl_suite_name(?TLS_RSA_EXPORT_WITH_RC4_40_MD5) -> 
    "EXP-RC4-MD5";

openssl_suite_name(?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5) ->
    "EXP1024-RC4-MD5";
openssl_suite_name(?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5) ->
    "EXP1024-RC2-CBC-MD5";
openssl_suite_name(?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA) ->
    "EXP1024-DES-CBC-SHA";
openssl_suite_name(?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA) ->
    "EXP1024-DHE-DSS-DES-CBC-SHA";
openssl_suite_name(?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA) ->
    "EXP1024-RC4-SHA";
openssl_suite_name(?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA) ->
    "EXP1024-DHE-DSS-RC4-SHA";
openssl_suite_name(?TLS_DHE_DSS_WITH_RC4_128_SHA) ->
    "DHE-DSS-RC4-SHA";

%% No oppenssl name
openssl_suite_name(Cipher) ->
    suite_definition(Cipher).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

bulk_cipher_algorithm(null) ->
    ?NULL;
bulk_cipher_algorithm(idea_cbc) ->
    ?IDEA;
bulk_cipher_algorithm(Cipher) when Cipher == rc2_cbc_40;
				   Cipher == rc2_cbc_56 ->
    ?RC2;
bulk_cipher_algorithm(Cipher) when Cipher == rc4_40;
				   Cipher == rc4_56;
				   Cipher == rc4_128 ->
    ?RC4;
bulk_cipher_algorithm(des40_cbc) ->
    ?DES40;
bulk_cipher_algorithm(des_cbc) ->
    ?DES;
bulk_cipher_algorithm('3des_ede_cbc') ->
    ?'3DES';
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_cbc;
				   Cipher == aes_256_cbc ->
    ?AES.

type(Cipher) when Cipher == null;
		  Cipher == rc4_40;
		  Cipher == rc4_56;
		  Cipher == rc4_128 ->
    ?STREAM;

type(Cipher) when Cipher == idea_cbc;
		  Cipher == rc2_cbc_40;
		  Cipher == rc2_cbc_56;
		  Cipher == des40_cbc;
		  Cipher == des_cbc;
		  Cipher == '3des_ede_cbc';
		  Cipher == aes_128_cbc;
		  Cipher == aes_256_cbc ->
    ?BLOCK.

key_material(null) ->
    0;
key_material(Cipher) when Cipher == idea_cbc;
 			  Cipher == rc4_128 ->
    16;
key_material(Cipher) when Cipher == rc2_cbc_56;
			  Cipher == rc4_56 ->
    7;
key_material(Cipher) when Cipher == rc2_cbc_40;
 			  Cipher == rc4_40;
 			  Cipher == des40_cbc ->
    5;
key_material(des_cbc) ->
    8;
key_material('3des_ede_cbc') ->
    24;
key_material(aes_128_cbc) ->
    16;
key_material(aes_256_cbc) ->
    32.

expanded_key_material(null) ->
    0;
expanded_key_material(Cipher) when Cipher == idea_cbc;
 				   Cipher == rc2_cbc_40;
 				   Cipher == rc2_cbc_56;
 				   Cipher == rc4_40;
				   Cipher == rc4_56;
 				   Cipher == rc4_128 ->
    16;
expanded_key_material(Cipher) when Cipher == des_cbc;
 				   Cipher == des40_cbc ->
    8;
expanded_key_material('3des_ede_cbc') ->
    24;
expanded_key_material(Cipher) when Cipher == aes_128_cbc;
 				   Cipher == aes_256_cbc ->
    unknown.  


effective_key_bits(null) ->
    0;
effective_key_bits(Cipher) when Cipher == rc2_cbc_40;
				Cipher == rc4_40;
				Cipher == des40_cbc ->
    40;
effective_key_bits(Cipher) when Cipher == rc2_cbc_56;
				Cipher == rc4_56;
				Cipher == des_cbc ->
    56;
effective_key_bits(Cipher) when Cipher == idea_cbc;
				Cipher == rc4_128;
				Cipher == aes_128_cbc ->
    128;
effective_key_bits('3des_ede_cbc') ->
    168;
effective_key_bits(aes_256_cbc) ->
    256.

iv_size(Cipher) when Cipher == null;
		     Cipher == rc4_40;
		     Cipher == rc4_56;
		     Cipher == rc4_128 ->
    0;
iv_size(Cipher) ->
    block_size(Cipher).

block_size(Cipher) when Cipher == idea_cbc;
			Cipher == rc2_cbc_40;
			Cipher == rc2_cbc_56;
			Cipher == des40_cbc;
			Cipher == des_cbc;
			Cipher == '3des_ede_cbc' -> 
    8;

block_size(Cipher) when Cipher == aes_128_cbc;
			Cipher == aes_256_cbc ->
    16.

mac_algorithm(null) ->
    ?NULL;
mac_algorithm(md5) ->
    ?MD5;
mac_algorithm(sha) ->
    ?SHA.

hash_size(null) ->
    0;
hash_size(md5) ->
    16;
hash_size(sha) ->
    20.

generic_block_cipher_from_bin(T, HashSize) ->
    Sz1 = size(T)-1,
    <<_:Sz1/binary, ?BYTE(PadLength)>> = T,
    CompressedLength = size(T) - PadLength - 1 - HashSize,
    <<Content:CompressedLength/binary, Mac:HashSize/binary,
     Padding:PadLength/binary, ?BYTE(PadLength)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength}.

generic_stream_cipher_from_bin(T, HashSz) ->
    Sz = size(T),
    CompressedLength = Sz - HashSz,
    <<Content:CompressedLength/binary, Mac:HashSz/binary>> = T,
    #generic_stream_cipher{content=Content,
			   mac=Mac}.

check_padding(_GBC) ->
    ok.

generic_block_cipher_to_list(#generic_block_cipher{content=Content,
						   mac=Mac,
						   padding=Padding,
						   padding_length=PaddingLength}) ->
    [Content, Mac, Padding, PaddingLength].

generic_stream_cipher_to_list(#generic_stream_cipher{content=Content,
						     mac=Mac}) ->
    [Content, Mac].

get_padding(Length, BlockSize) ->
    get_padding_aux(BlockSize, Length rem BlockSize).

get_padding_aux(_, 0) ->
    {0, <<>>};
get_padding_aux(BlockSize, PadLength) ->
    N = BlockSize - PadLength,
    {N, list_to_binary(lists:duplicate(N, N))}.

mpint2binary(<<_, _, _, _, 0, Rest/binary>>) ->
    Rest;
mpint2binary(<<_, _, _, _, Rest/binary>>) ->
    Rest.

iolist2mpint(L) ->
    Bin = erlang:iolist_to_binary(L),
    Sz = size(Bin),
    <<?UINT32(Sz), Bin/binary>>.

int_byte_size(I) when is_integer(I) ->
    size(mpint2binary(crypto:mpint(I))).

next_iv(Bin, IV) ->
    BinSz = size(Bin),
    IVSz = size(IV),
    FirstPart = BinSz - IVSz,
    <<_:FirstPart/binary, NextIV:IVSz/binary>> = Bin,
    NextIV.

filler(0, Sz) ->
    lists:duplicate(Sz, 0);
filler(1, Sz) ->
    lists:duplicate(Sz, 255);
filler(2, Sz) ->
    rand_nonzero_bytes(Sz).

format_encryption_block(Data, WantedLength, BlockType) ->
    Sz = erlang:iolist_size(Data),
    Filler = filler(BlockType, WantedLength - Sz - 4),
    [0, BlockType, Filler, 0, Data].

rand_nonzero_bytes(N) ->
    L = binary_to_list(crypto:rand_bytes(N)),
    Lnz = lists:map(fun(0) ->
                            crypto:rand_uniform(1, 256);
                       (B) ->
                            B
                    end, L),
    list_to_binary(Lnz).


after_zero(<<0, Rest/binary>>) ->
    Rest;
after_zero(<<_, Rest/binary>>) ->
    after_zero(Rest);
after_zero(<<>>) ->
    <<>>.

after_initial_zeroes(<<0, Rest/binary>>) ->
    after_initial_zeroes(Rest);
after_initial_zeroes(<<Binary/binary>>) ->
    Binary.

%% TODO kolla att filler verkligen är rätt värde
unformat_encryption_block(Data, 0) ->
    after_initial_zeroes(Data);
unformat_encryption_block(Data, _BlockType) ->
    after_zero(after_initial_zeroes(Data)).
