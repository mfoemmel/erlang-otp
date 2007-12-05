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
%%----------------------------------------------------------------------
%% Purpose: Help funtions for handling the SSL-handshake protocol
%%----------------------------------------------------------------------

-module(ssl_handshake).

-include("ssl_handshake.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_alert.hrl").
-include("ssl_internal.hrl").
-include("ssl_pkix.hrl").
-include("ssl_debug.hrl").

-export([master_secret/4, client_hello/4, server_hello/3, hello/2, 
	 certify/3, certificate/3, 
	 certificate_verify/6, key_exchange/3,  finished/4,
	 verify_connection/5, get_tls_handshake/2,
	 server_hello_done/0, sig_alg/1,
         decode_handshake/3, encode_handshake/2, init_hashes/0, 
         update_hashes/2, decrypt_premaster_secret/2]).

%%====================================================================
%% Internal application API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: client_hello(Host, Port, ConnectionStates, SslOpts) -> 
%%                                                  #client_hello{} 
%%      Host
%%      Port
%%      ConnectionStates = #connection_states{}
%%      SslOpts = #ssl_options{}
%%
%% Description: Creates a client hello message.
%%--------------------------------------------------------------------
client_hello(Host, Port, ConnectionStates, SslOpts) ->
    
    Version = ssl_record:highest_protocol_version(),
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
   
    Id = ssl_manager:client_session_id(Host, 
				  Port, SslOpts),
    #client_hello{session_id = Id, 
		  client_version = Version,
		  cipher_suites = SslOpts#ssl_options.ciphers,
		  compression_methods = ssl_record:compressions(),
		  random = SecParams#security_parameters.client_random
		 }.

%%--------------------------------------------------------------------
%% Function: server_hello(Host, Port, SessionId, 
%%                        Version, ConnectionStates) -> #server_hello{} 
%%      SessionId
%%      Version
%%      ConnectionStates 
%%	
%%
%% Description: Creates a server hello message.
%%--------------------------------------------------------------------
server_hello(SessionId, Version, ConnectionStates) ->
    Pending = ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = Pending#connection_state.security_parameters,
    
    #server_hello{server_version = Version,
		  cipher_suite = SecParams#security_parameters.cipher_suite,
                  compression_method = 
		  SecParams#security_parameters.compression_algorithm,
		  random = SecParams#security_parameters.server_random,
		  session_id = SessionId
		 }.

%%--------------------------------------------------------------------
%% Function: hello(Hello, Info) -> 
%%                                   {Version, Id, NewConnectionStates} |
%%                                   #alert{}
%%
%%      Hello = #client_hello | #server_hello
%%      Info = ConnectionStates | {Port, Session, ConnectionStates}
%%      ConnectionStates = #connection_states{}
%%
%% Description: Handles a recieved hello message
%%--------------------------------------------------------------------
hello(Hello = #server_hello{}, ConnectionStates) ->
    
    Version = Hello#server_hello.server_version,
    SessionId = Hello#server_hello.session_id,
    
    NewConnectionStates =
	hello_pending_connection_states(client, 
					 Hello#server_hello.cipher_suite,
					 Hello#server_hello.random, 
					 Hello#server_hello.compression_method,
					 ConnectionStates),
    
    {Version, SessionId, NewConnectionStates};

hello(Hello = #client_hello{}, {Port, UserSuites,
				Session0, ConnectionStates0}) ->
    
    Version = select_version(Hello#client_hello.client_version),
  
    case ssl_record:is_acceptable_version(Version) of
	true ->
	    {Type, #session{cipher_suite = CipherSuite,
			    compression_method = Compression} = Session} 
	     = select_session(Hello, Port, Session0, Version, UserSuites),
	     
	    case CipherSuite of 
		no_suite ->
		    #alert{level = ?FATAL,
			   description = ?INSUFFICIENT_SECURITY};
		_ ->
		    Random = Hello#client_hello.random, 
		    ConnectionStates =
			hello_pending_connection_states(server, 
							CipherSuite,
						        Random, 
							Compression,
							ConnectionStates0),
		    {Version, {Type, Session}, ConnectionStates}
	    end;
	false ->
	    #alert{level = ?FATAL,
		   description = ?PROTOCOL_VERSION}
    end.

%%--------------------------------------------------------------------
%% Function: certify(Certs, CertDbRef, MaxPathLen) ->
%%                                           #'Certificate'{} | #alert{}
%%
%%      Certs = #certificate{}
%%	CertDbRef = reference()
%%      MaxPathLen = integer() | nolimit
%%
%% Description: Handles a certificate handshake message
%%--------------------------------------------------------------------
certify(#certificate{asn1_certificates = ASN1Certs}, CertDbRef, MaxPathLen) -> 
    [PeerCert | _] = ASN1Certs,
    Certs = lists:map(fun(Cert) ->
			      {ok, DecodedCert} =
				  ssl_pkix:decode_cert(Cert, [ssl]),
			      {Cert, DecodedCert}
		      end, ASN1Certs),
    {{_BinCert, TrustedErlCert}, CertPath} =
	ssl_certificate:trusted_cert_and_path(Certs, CertDbRef),
    InitPathLen = case MaxPathLen of
		      nolimit ->
			  length(CertPath);
		      _ ->
			  MaxPathLen
		  end,
    ValidationState = 
	ssl_certificate:init_validation_state(TrustedErlCert, InitPathLen),
    {ok, PublicKeyInfo} = path_validation(CertPath, ValidationState),
    {PeerCert, PublicKeyInfo}.

%%--------------------------------------------------------------------
%% Function: certificate(OwnCert, CertDbRef, Role) -> #certificate{}
%%
%%      OwnCert = binary()
%%      CertDbRef = term() as returned by ssl_certificate_db:create()
%%
%% Description: Creates a certificate message.
%%--------------------------------------------------------------------
certificate(OwnCert, CertDbRef, client) ->
    Chain =
	case ssl_certificate:certificate_chain(OwnCert, CertDbRef) of
	    {ok, CertChain} ->
		CertChain;
	    {error, _} -> 
		%% If no suitable certificate is available, the client
		%% SHOULD send a certificate message containing no
		%% certificates. (chapter 7.4.6. rfc 4346) 
		[]	 
	end,

    #certificate{asn1_certificates = Chain};

certificate(OwnCert, CertDbRef, server) -> 
    case ssl_certificate:certificate_chain(OwnCert, CertDbRef) of
	{ok, Chain} ->
	    #certificate{asn1_certificates = Chain};
	{error, _} ->
	    #alert{level  = ?FATAL,
		   description =  ?INTERNAL_ERROR}
	end.

%%--------------------------------------------------------------------
%% Function: certificate_verify(Cert, ConnectionStates) -> 
%%                                                #certificate_verify{}
%% Cert             = #certificate{}
%% ConnectionStates = #connection_states{}
%%
%% Description: Creates a certificate_verify message, called by the client.
%%--------------------------------------------------------------------
certificate_verify(OwnCert, ConnectionStates, Version, Algorithm,
		   PrivateKey, {Hashes, _}) ->
    ?DBG_TERM(ConnectionStates),
    case is_fixed_diffie_hellman(OwnCert, ConnectionStates) of
	true ->
	    fixed_diffie_hellman;
	false ->
	    MasterSecret =
		ssl_record:get_pending_master_secret(ConnectionStates),
	    Sig = ssl_cipher:get_handshake_hashes(Version, none,
						     MasterSecret, Hashes),
	    Signed = ssl_cipher:digitally_sign(Algorithm, Sig, PrivateKey),
	    #certificate_verify{signature = Signed}
    end.
%%--------------------------------------------------------------------
%% Function: key_exchange(Role, Secret, Params) -> 
%%                         #client_key_exchange{} | #server_key_exchange{}
%%
%%      Secret -
%%      Params - 
%%
%% Description: Creates a keyexchange message.
%%--------------------------------------------------------------------
key_exchange(client, Secret, {Algorithm, PublicKey, _}) when 
  Algorithm == rsaEncryption;
  Algorithm == md2WithRSAEncryption;
  Algorithm == md5WithRSAEncryption;
  Algorithm == sha1WithRSAEncryption ->
    EncPremasterSecret =
	encrypted_premaster_secret(Secret, PublicKey),
    #client_key_exchange{exchange_keys = EncPremasterSecret};
key_exchange(client, _Secret, _Params = #server_dh_params{}) -> 
    #client_key_exchange{exchange_keys = #client_diffie_hellman_public{}};
key_exchange(client, _, _) ->
    #client_key_exchange{ 
	      %%exchange_keys =  #kerberos_wrapper{}
	     };
key_exchange(server, _, _) ->
    #server_key_exchange{}.

%%--------------------------------------------------------------------
%% Function: master_secret(Version, Session/PremasterSecret, 
%%                         ConnectionStates, Role) -> 
%%                          {MasterSecret, NewConnectionStates} | #alert{}
%%      Version = #protocol_version{}
%%      Session = #session{} (session contains master secret)
%%      PremasterSecret = binary()  
%%      ConnectionStates = #connection_states{}
%%      Role = client | server
%%
%% Description: Sets or calculates the master secret and calculate keys,
%% updating the pending connection states. The Mastersecret and the update
%% connection states are returned or an alert if the calculation fails.
%%-------------------------------------------------------------------
master_secret(Version, #session{master_secret = Mastersecret}, 
	      ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    try master_secret(Version, Mastersecret, SecParams, ConnectionStates, Role) of
	Result ->
	    Result
    catch
	exit:Reason ->
	    error_logger:error_report("Key calculation failed due to ~p",
				      [Reason]),
	    #alert{level = ?FATAL, description = ?HANDSHAKE_FAILURE}
    end;

master_secret(Version, PremasterSecret, ConnectionStates, Role) ->
    ConnectionState = 
	ssl_record:pending_connection_state(ConnectionStates, read),
    SecParams = ConnectionState#connection_state.security_parameters,
    #security_parameters{client_random = ClientRandom,
			 server_random = ServerRandom} = SecParams,		
    try master_secret(Version, 
		      ssl_cipher:master_secret(Version,PremasterSecret,
					       ClientRandom, ServerRandom),
		      SecParams, ConnectionStates, Role) of
	Result ->
	    Result
    
    catch
	exit:Reason ->
	    error_logger:error_report("Master secret calculation failed"
				      " due to ~p", [Reason]),
	    #alert{level = ?FATAL, description = ?HANDSHAKE_FAILURE}
    end.

%%--------------------------------------------------------------------
%% Function: finished(Version, Role, MacSecret, Hashes) -> #finished{}
%%
%%      ConnectionStates = #connection_states{}
%%
%% Description: Creates a handshake finished message
%%-------------------------------------------------------------------
finished(Version, Role, MasterSecret, {Hashes, _}) -> % use the current hashes
    #finished{verify_data = 
	      ssl_cipher:get_handshake_hashes(Version, Role, MasterSecret, Hashes)}.

%%--------------------------------------------------------------------
%% Function: verify_connection(Finished, Role, 
%%                             MasterSecret, Hashes) -> verified | #alert{}
%% 
%% Finished = #finished{}
%% Role = client | server - the role of the process that sent the finished
%% message.
%% MasterSecret = binary()
%% Hashes = binary() -  {md5_hash, sha_hash} 
%%
%%
%% Description: Checks the ssl handshake finished message to verify
%%              the connection.
%%-------------------------------------------------------------------
verify_connection(Version, #finished{verify_data = Data}, 
		  Role, MasterSecret, {_, {MD5, SHA}}) -> % use the previous hashes
    ?DBG_HEX(crypto:md5_final(MD5)),
    ?DBG_HEX(crypto:sha_final(SHA)),
    case ssl_cipher:get_handshake_hashes(Version, Role, MasterSecret, {MD5, SHA}) of
	Data ->
	    verified;
	_ ->
 	    #alert{level = ?FATAL,
 		   description = ?HANDSHAKE_FAILURE}
    end.
	    
%%--------------------------------------------------------------------
%% Function: decode_handshake(BinHandShake, KeyExchange, SigAlg)
%%           -> #client_hello | #server_hello{} | server_hello_done |
%% #certificate{} | #client_key_exchange{} | #finished{} |
%% #client_certify_request{}
%%     
%% decode a binary handshake packet
%%--------------------------------------------------------------------
decode_handshake(#handshake{msg_type = MsgType,
			    body = Contents}, KeyExchangeAlg, SigAlg) ->
    dec_hs(MsgType, Contents, KeyExchangeAlg, SigAlg);
decode_handshake(_, _, _) ->
    not_handshake_packet.

server_hello_done() ->
    #server_hello_done{}.

%%--------------------------------------------------------------------
%% Function: encode_handshake(HandshakeRec) -> BinHandshake
%% HandshakeRec = #client_hello | #server_hello{} | server_hello_done |
%%              #certificate{} | #client_key_exchange{} | #finished{} |
%%              #client_certify_request{}
%%     
%% encode a handshake packet to binary
%%--------------------------------------------------------------------
encode_handshake(Package, SigAlg) ->
    {MsgType, Bin} = enc_hs(Package, SigAlg),
    Len = size(Bin),
    [MsgType, ?uint24(Len), Bin].

%%--------------------------------------------------------------------
%% Function: get_tls_handshake(Data, Buffer) -> Result
%%      Result = {[#handshake{}], [Raw], NewBuffer}
%%      Data = Buffer = NewBuffer = Raw = binary()
%%
%% Description: Given buffered and new data from ssl_record, collects
%% and returns it as a list of #handshake, also returns leftover
%% data.
%%--------------------------------------------------------------------
get_tls_handshake(Data, Buffer) ->
    get_tls_handshake_aux(list_to_binary([Buffer, Data]), [], []).

get_tls_handshake_aux(<<?BYTE(Type), ?UINT24(Length), Body:Length/binary,
		       Rest/binary>>, Acc, RawAcc) ->
    Raw = <<?BYTE(Type), ?UINT24(Length), Body/binary>>,
    H = #handshake{msg_type = Type, length = Length, body = Body},
    get_tls_handshake_aux(Rest, [H | Acc], [Raw | RawAcc]);
get_tls_handshake_aux(Data, Acc, RawAcc) ->
    {lists:reverse(Acc), lists:reverse(RawAcc), Data}.


%%--------------------------------------------------------------------
%% Function: sig_alg(atom()) -> integer()
%%
%% Description: Convert from key exchange as atom to signature
%% algorithm as a ?SIGNATURE_... constant
%%--------------------------------------------------------------------

sig_alg(dh_anon) ->
    ?SIGNATURE_ANONYMOUS;
sig_alg(Alg) when Alg == dhe_rsa; Alg == rsa; Alg == dh_rsa ->
    ?SIGNATURE_RSA;
sig_alg(Alg) when Alg == dh_dss; Alg == dhe_dss ->
    ?SIGNATURE_DSA;
sig_alg(_) ->
    ?NULL.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
select_session(Hello, Port, Session, Version, UserSuites) ->
    SuggestedSessionId = Hello#client_hello.session_id,
    SessionId = ssl_manager:server_session_id(Port, SuggestedSessionId),
    
    Suites = case UserSuites of
		 [] ->
		     ssl_cipher:suites(Version);
		 _ ->
		   UserSuites
	     end,

    case ssl_session:is_new(SuggestedSessionId, SessionId) of
        true ->
	    CipherSuite = 
		select_cipher_suite(Hello#client_hello.cipher_suites, Suites),
	    Compressions = Hello#client_hello.compression_methods,
	    Compression = select_compression(Compressions),
	    {new, Session#session{session_id = SessionId,
				  cipher_suite = CipherSuite,
				  compression_method = Compression}};
	false ->
	    {resumed, ssl_session:cache_lookup(Port, SessionId)}
    end.
	    
%% Update pending connection states with parameters exchanged via 
%% hello messages
%% NOTE : Role is the role of the receiver of the hello message
%%        currently being processed.
hello_pending_connection_states(Role, CipherSuite, Random, Compression,
				 ConnectionStates) ->    
    ReadState =  
	ssl_record:pending_connection_state(ConnectionStates, read),
    WriteState = 
	ssl_record:pending_connection_state(ConnectionStates, write),
    
    NewReadSecParams = 
	hello_security_parameters(Role, ReadState, CipherSuite, 
			    Random, Compression),
    
    NewWriteSecParams =
	hello_security_parameters(Role, WriteState, CipherSuite,
			    Random, Compression),
 
    ssl_record:update_security_params(NewReadSecParams,
				    NewWriteSecParams,
				    ConnectionStates).

hello_security_parameters(client, ConnectionState, CipherSuite, Random,
			  Compression) ->   
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      server_random = Random,
      compression_algorithm = Compression
     };

hello_security_parameters(server, ConnectionState, CipherSuite, Random, 
			  Compression) ->
    SecParams = ConnectionState#connection_state.security_parameters,
    NewSecParams = ssl_cipher:security_parameters(CipherSuite, SecParams),
    NewSecParams#security_parameters{
      client_random = Random,
      compression_algorithm = Compression
     }.

select_version(ClientVersion) ->
    ServerVersion = ssl_record:highest_protocol_version(),
    ssl_record:lowest_protocol_version(ClientVersion, ServerVersion).

select_cipher_suite([], _) ->
   no_suite;
select_cipher_suite([Suite | ClientSuites], SupportedSuites) ->
    case is_member(Suite, SupportedSuites) of
	true ->
	    Suite;
        false ->
	    select_cipher_suite(ClientSuites, SupportedSuites)
    end.

is_member(Suite, SupportedSuites) ->
    lists:member(Suite, SupportedSuites).

select_compression(_CompressionMetodes) ->
    ?NULL.

master_secret(Version, MasterSecret, #security_parameters{
			 client_random = ClientRandom,
			 server_random = ServerRandom,
			 hash_size = HashSize,
			 key_material_length = KML,
			 expanded_key_material_length = EKML,
			 iv_size = IVS,
			 exportable = Exportable},
	      ConnectionStates, Role) ->
    ?DBG_TERM(KML),
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV} =
	ssl_cipher:setup_keys(Version, Exportable, MasterSecret, ServerRandom, 
			      ClientRandom, HashSize, KML, EKML, IVS),
    ?DBG_HEX(ClientWriteKey),
    ?DBG_HEX(ClientIV),
    ConnStates1 = ssl_record:set_master_secret(MasterSecret, ConnectionStates),
    ConnStates2 =
	ssl_record:set_mac_secret(ClientWriteMacSecret, ServerWriteMacSecret,
				  Role, ConnStates1),

    CSCW = #cipher_state{iv = ClientIV, key = ClientWriteKey},
    CSSW = #cipher_state{iv = ServerIV, key = ServerWriteKey}, 
    {MasterSecret, 
     ssl_record:set_pending_cipher_state(ConnStates2, CSSW, CSCW, Role)}.

path_validation([], #path_validation_state{working_public_key_algorithm
					   = Algorithm,
					   working_public_key =
					   PublicKey,
					   working_public_key_parameters 
					   = PublicKeyParams
					  }) ->
    {ok, {Algorithm, PublicKey, PublicKeyParams}};

path_validation([{Cert, ErlCert}| Rest], 
		ValidationState = 
		#path_validation_state{
		  max_path_length = Len}) when Len >= 0 ->
    ssl_certificate:validate_time(ErlCert),
    ssl_certificate:validate_signature(ErlCert, Cert, ValidationState),
    ssl_certificate:validate_issuer(ErlCert, ValidationState),
    ssl_certificate:validate_names(ErlCert, ValidationState),
    ssl_certificate:is_not_revoked(ErlCert),
    TmpValidationState = 
    	ssl_certificate:validate_extensions(ErlCert, ValidationState),
    NewValidationState = 
    	ssl_certificate:prepare_for_next_cert(ErlCert, TmpValidationState),
    path_validation(Rest, NewValidationState);

path_validation(_, _) ->
    throw(#alert{level = ?FATAL,
		 description = ?CERTIFICATE_UNKNOWN}).

dec_hs(?HELLO_REQUEST, <<>>, _, _) ->
    #hello_request{};

%% Client hello v2.
%% The server must be able to receive such messages, from clients that
%% are willing to use ssl v3 or higher, but have ssl v2 compatibility.
dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor),
		       ?UINT16(CSLength), ?UINT16(0),
		       ?UINT16(CDLength), 
		       CipherSuites:CSLength/binary, 
		       ChallengeData:CDLength/binary>>,
       _, _) ->
    ?DBG_HEX(CipherSuites),
    ?DBG_HEX(CipherSuites),
    #client_hello{
	client_version = #protocol_version{major = Major, minor = Minor},
	random = ssl_ssl2:client_random(ChallengeData, CDLength),
	session_id = 0,
	cipher_suites = from_3bytes(CipherSuites),
	compression_methods = [?NULL]};

dec_hs(?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       ?UINT16(Cs_length), CipherSuites:Cs_length/binary,
		       ?BYTE(Cm_length), Comp_methods:Cm_length/binary>>,
       _, _) ->
    #client_hello{
	client_version = #protocol_version{major = Major, minor = Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suites = from_2bytes(CipherSuites),
	compression_methods = Comp_methods};
dec_hs(?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		       ?BYTE(SID_length), Session_ID:SID_length/binary,
		       Cipher_suite:2/binary, ?BYTE(Comp_method)>>, _, _) ->
    #server_hello{
	server_version = #protocol_version{major = Major, minor = Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method};
dec_hs(?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>, _, _) ->
    #certificate{asn1_certificates = certs_to_list(ASN1Certs)};
dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(ModLen), Mod:ModLen/binary,
			      ?UINT16(ExpLen), Exp:ExpLen/binary,
			      Sig/binary>>,
       ?KEY_EXCHANGE_RSA, SigAlg) ->
    #server_key_exchange{
	params = #server_rsa_params{
	  rsa_modulus = Mod,
	  rsa_exponent = Exp},
	signed_params = dec_sig(SigAlg, Sig)}; 	% TODO: error checking
dec_hs(?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			      ?UINT16(GLen), G:GLen/binary,
			      ?UINT16(YLen), Y:YLen/binary,
			      Sig/binary>>,
       ?KEY_EXCHANGE_DIFFIE_HELLMAN, SigAlg) ->
    #server_key_exchange{
	params = #server_dh_params{
	  dh_p = P, dh_g = G, dh_y = Y},
	signed_params = dec_sig(SigAlg, Sig)};
dec_hs(?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes:CertTypesLen/binary,
	?UINT16(CertAuthsLen), CertAuths:CertAuthsLen/binary>>, _, _) ->
    %% TODO: maybe we should chop up CertAuths into a list?
    #certificate_request{
	certificate_types = CertTypes,
	certificate_authorities = CertAuths};
dec_hs(?SERVER_HELLO_DONE, <<>>, _, _) ->
    #server_hello_done{};
dec_hs(?CERTIFICATE_VERIFY, Sig, _, SigAlg)->
    dec_sig(Sig, SigAlg);
dec_hs(?CLIENT_KEY_EXCHANGE, PKEPMS, rsa, _) ->
    #client_key_exchange{
        exchange_keys = #encrypted_premaster_secret{
                            premaster_secret = PKEPMS}};
dec_hs(?CLIENT_KEY_EXCHANGE, <<>>, ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) -> % TODO: Should check whether the cert already contains a suitable DH-key (7.4.7.2)
    implicit_public_value_encoding;
dec_hs(?CLIENT_KEY_EXCHANGE, <<?UINT16(DH_YCLen), DH_YC:DH_YCLen/binary>>,
       ?KEY_EXCHANGE_DIFFIE_HELLMAN, _) ->
    #client_diffie_hellman_public{
	dh_public = DH_YC};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Certificate;
dec_hs(?FINISHED, VerifyData, _, _) ->
    #finished{verify_data = VerifyData};

dec_hs(Other, Data, _, _) ->
    #handshake{msg_type = Other,
	       length = size(Data),
	       body = Data}.

dec_sig(?SIGNATURE_ANONYMOUS, _) ->
    #signature{
	 digitally_signed = #digitally_signed{
	   md5_hash = <<>>,
	   sha_hash = <<>>}};
dec_sig(?SIGNATURE_RSA, <<SHAHash:20/binary>>) ->
    #signature{
	 digitally_signed = #digitally_signed{
	   md5_hash = <<>>,
	   sha_hash = SHAHash}};
dec_sig(?SIGNATURE_DSA, <<MD5Hash:16/binary, SHAHash:20/binary>>) ->
    #signature{
	 digitally_signed = #digitally_signed{
	   md5_hash = MD5Hash,
	   sha_hash = SHAHash}}.

enc_sig(#signature{
	 digitally_signed = #digitally_signed{
	   md5_hash = M,
	   sha_hash = S}}) ->
    <<M/binary, S/binary>>.

is_fixed_diffie_hellman(_, _) ->
    false.

encrypted_premaster_secret(Secret, PublicKey) ->
    %% format block before encrypting
    %% TODO check if SSL3 only?
    %% TODO lots of other checks
    Sz = size(crypto:mpint(PublicKey#'RSAPublicKey'.modulus)) - 4,
    Block = erlang:iolist_to_binary(ssl_cipher:format_encryption_block(Secret, Sz, 2)),
    %%<<_:32/integer, M/binary>> = crypto:mpint(PublicKey#'RSAPublicKey'.modulus),
    %%<<_:32/integer, E/binary>> = crypto:mpint(PublicKey#'RSAPublicKey'.publicExponent),
    PremasterSecret =
	ssl_cipher:rsa_encrypt(Block, PublicKey),
    #encrypted_premaster_secret{premaster_secret = PremasterSecret}.

decrypt_premaster_secret(Secret, Key) ->
    Block = ssl_cipher:rsa_decrypt(Secret,
                                   Key#'RSAPrivateKey'.modulus,
                                   Key#'RSAPrivateKey'.privateExponent),
    ssl_cipher:unformat_encryption_block(Block, 2).

%% encode/decode stream of certificate data to/from list of certificate data 
certs_to_list(ASN1Certs) ->
    certs_to_list(ASN1Certs, []).

certs_to_list(<<?UINT24(CertLen), Cert:CertLen/binary, Rest/binary>>, Acc) ->
    certs_to_list(Rest, [Cert | Acc]);
certs_to_list(<<>>, Acc) ->
    lists:reverse(Acc, []).

certs_from_list(ACList) ->
    list_to_binary([begin
			CertLen = size(Cert),
                        <<?UINT24(CertLen), Cert/binary>>
		    end || Cert <- ACList]).

enc_hs(#hello_request{}, _) ->
    {?HELLO_REQUEST, <<>>};
enc_hs(#client_hello{
	client_version = #protocol_version{major = Major, minor = Minor},
	random = Random,
	session_id = SessionID,
	cipher_suites = CipherSuites,
	compression_methods = CompMethods}, _) ->
    SIDLength = size(SessionID),
    BinCompMethods = list_to_binary(CompMethods),
    CmLength = size(BinCompMethods),
    BinCipherSuites = list_to_binary(CipherSuites),
    CsLength = size(BinCipherSuites),
    {?CLIENT_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SIDLength), SessionID/binary,
		     ?UINT16(CsLength), BinCipherSuites/binary,
		     ?BYTE(CmLength), BinCompMethods/binary>>};
enc_hs(#server_hello{
	server_version = #protocol_version{major = Major, minor = Minor},
	random = Random,
	session_id = Session_ID,
	cipher_suite = Cipher_suite,
	compression_method = Comp_method}, _) ->
    SID_length = size(Session_ID),
    {?SERVER_HELLO, <<?BYTE(Major), ?BYTE(Minor), Random:32/binary,
		     ?BYTE(SID_length), Session_ID/binary,
                     Cipher_suite/binary, ?BYTE(Comp_method)>>};
enc_hs(#certificate{asn1_certificates = ASN1CertList}, _) ->
    ASN1Certs = certs_from_list(ASN1CertList),
    ACLen = erlang:iolist_size(ASN1Certs),
    {?CERTIFICATE, <<?UINT24(ACLen), ASN1Certs:ACLen/binary>>};
enc_hs(#server_key_exchange{params = #server_rsa_params{rsa_modulus = Mod,
							rsa_exponent = Exp},
	signed_params = SignedParams}, _) ->
    EncSig = enc_sig(SignedParams),
    ModLen = size(Mod),
    ExpLen = size(Exp),
    {?SERVER_KEY_EXCHANGE, <<?UINT16(ModLen), Mod/binary,
			    ?UINT16(ExpLen), Exp/binary,
			    EncSig/binary>>};
enc_hs(#server_key_exchange{params = #server_dh_params{
			      dh_p = P, dh_g = G, dh_y = Y},
	signed_params = SignedParams}, _) ->
    EncSig = enc_sig(SignedParams),
    PLen = size(P),
    GLen = size(G),
    YLen = size(Y),
    {?SERVER_KEY_EXCHANGE, <<?UINT16(PLen), P:PLen/binary,
			    ?UINT16(GLen), G:GLen/binary,
			    ?UINT16(YLen), Y:YLen/binary,
			    EncSig/binary>>};
enc_hs(#certificate_request{certificate_types = CertTypes,
			    certificate_authorities = CertAuths}, _) ->
    CertTypesLen = size(CertTypes),
    CertAuthsLen = size(CertAuths),
    {?CERTIFICATE_REQUEST,
       <<?BYTE(CertTypesLen), CertTypes/binary,
	?UINT16(CertAuthsLen), CertAuths/binary>>};
enc_hs(#server_hello_done{}, _) ->
    {?SERVER_HELLO_DONE, <<>>};
enc_hs(#client_key_exchange{exchange_keys = ExchangeKeys}, _) ->
    {?CLIENT_KEY_EXCHANGE, enc_cke(ExchangeKeys)};
enc_hs(#certificate_verify{signature = BinSig0}, SigAlg) ->
    BinSig = enc_bin_sig(SigAlg, BinSig0),
    {?CERTIFICATE_VERIFY, BinSig};
enc_hs(#finished{verify_data = VerifyData}, _) ->
    {?FINISHED, VerifyData}.

enc_cke(#encrypted_premaster_secret{premaster_secret = PKEPMS}) ->
    PKEPMS.
%%    PKEPMSLen = size(PKEPMS),
%%    <<?UINT16(PKEPMSLen), PKEPMS/binary>>.

enc_bin_sig(?SIGNATURE_ANONYMOUS, _) ->
    <<>>;
enc_bin_sig(_, H) ->
    Sz = size(H),
    <<?UINT16(Sz), H/binary>>.

init_hashes() ->
    T = {crypto:md5_init(), crypto:sha_init()},
    {T, T}.

%%first_byte(I) when is_integer(I) -> I;
%%first_byte([F | _])  -> first_byte(F);
%%first_byte(<<?BYTE(B), _/binary>>) -> B.
	
update_hashes(Hashes, % we special-case SSL2 client hello
	      [<<?CLIENT_HELLO, ?UINT24(_), ?BYTE(Major), ?BYTE(Minor),
		       ?UINT16(CSLength), ?UINT16(0),
		       ?UINT16(CDLength), 
		       CipherSuites:CSLength/binary, 
		       ChallengeData:CDLength/binary>>]) ->
    update_hashes(Hashes,
		  <<?CLIENT_HELLO, ?BYTE(Major), ?BYTE(Minor),
		   ?UINT16(CSLength), ?UINT16(0),
		   ?UINT16(CDLength), 
		   CipherSuites:CSLength/binary, 
		   ChallengeData:CDLength/binary>>);
update_hashes({{MD50, SHA0}, _Prev}, Data) ->
    ?DBG_HEX(Data),
    {MD51, SHA1} = {crypto:md5_update(MD50, Data),
		    crypto:sha_update(SHA0, Data)},
    ?DBG_HEX(crypto:md5_final(MD51)),
    ?DBG_HEX(crypto:sha_final(SHA1)),
    {{MD51, SHA1}, {MD50, SHA0}}.

from_3bytes(Bin3) ->
    from_3bytes(Bin3, []).

from_3bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_3bytes(<<?UINT24(N), Rest/binary>>, Acc) ->
    from_3bytes(Rest, [?uint16(N) | Acc]).

from_2bytes(Bin2) ->
    from_2bytes(Bin2, []).

from_2bytes(<<>>, Acc) ->
    lists:reverse(Acc);
from_2bytes(<<?UINT16(N), Rest/binary>>, Acc) ->
    from_2bytes(Rest, [?uint16(N) | Acc]).
