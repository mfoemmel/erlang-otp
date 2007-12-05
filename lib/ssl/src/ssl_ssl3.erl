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
%% Purpose: Handles sslv3 encryption.
%%----------------------------------------------------------------------

-module(ssl_ssl3).

-include("ssl_cipher.hrl").
-include("ssl_debug.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl"). 			% MD5 and SHA

-export([master_secret/3, get_handshake_hashes/3, mac_hash/6, setup_keys/8, suites/0]).


%%====================================================================
%% Internal application API
%%====================================================================

master_secret(PremasterSecret, ClientRandom, ServerRandom) ->
    ?DBG_HEX(PremasterSecret),
    ?DBG_HEX(ClientRandom),
    ?DBG_HEX(ServerRandom),
    B = generate_keyblock(PremasterSecret, ClientRandom, ServerRandom, 48),
    ?DBG_HEX(B),
    B.

get_handshake_hashes(Role, MasterSecret, {MD5Hash, SHAHash}) ->
    Sender = get_sender(Role),
    MD5 = handshake_hash(?MD5, MasterSecret, Sender, MD5Hash),
    SHA = handshake_hash(?SHA, MasterSecret, Sender, SHAHash),
    <<MD5/binary, SHA/binary>>.

mac_hash(Method, Mac_write_secret, Seq_num, Type, Length, Fragment) ->
    case Method of
        ?NULL -> ok;
        _ ->
	    ?DBG_HEX(Mac_write_secret),
	    ?DBG_HEX(hash(Method, Fragment)),
            ok
    end,
    Mac = mac_hash(Method, Mac_write_secret, 
		   [?uint64(Seq_num), ?byte(Type), ?uint16(Length), Fragment]),
    ?DBG_HEX(Mac),
    Mac.

%%
%% {ClientWriteMacSecret, ServerWriteMacSecret,
%%  ClientWriteKey, ServerWriteKey,
%%  ClientIV, ServerIV}
%%
setup_keys(Exportable, MasterSecret, ServerRandom, ClientRandom, HS, KML, _EKML, IVS)
  when Exportable == no_export; Exportable == ignore ->
    KeyBlock = generate_keyblock(MasterSecret, ServerRandom, ClientRandom,
				 2*(HS+KML+IVS)),
    <<ClientWriteMacSecret:HS/binary, ServerWriteMacSecret:HS/binary,
     ClientWriteKey:KML/binary, ServerWriteKey:KML/binary,
     ClientIV:IVS/binary, ServerIV:IVS/binary>> = KeyBlock,
    ?DBG_HEX(ClientWriteMacSecret),
    ?DBG_HEX(ServerWriteMacSecret),
    ?DBG_HEX(ClientWriteKey),
    ?DBG_HEX(ServerWriteKey),
    ?DBG_HEX(ClientIV),
    ?DBG_HEX(ServerIV),
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV};
setup_keys(export, MasterSecret, ServerRandom, ClientRandom, HS, KML, EKML, IVS) ->
    KeyBlock = generate_keyblock(MasterSecret, ServerRandom, ClientRandom,
				 2*(HS+KML)),
    <<ClientWriteMacSecret:HS/binary, ServerWriteMacSecret:HS/binary,
     ClientWriteKey:KML/binary, ServerWriteKey:KML/binary>> = KeyBlock,
    <<ClientIV:IVS/binary, _/binary>> = hash(?MD5, [ClientRandom, ServerRandom]),
    <<ServerIV:IVS/binary, _/binary>> = hash(?MD5, [ServerRandom, ClientRandom]),
    <<FinalClientWriteKey:EKML/binary, _/binary>> = hash(?MD5, [ClientWriteKey, ClientRandom, ServerRandom]),
    <<FinalServerWriteKey:EKML/binary, _/binary>> = hash(?MD5, [ServerWriteKey, ServerRandom, ClientRandom]),
    ?DBG_HEX(ClientWriteMacSecret),
    ?DBG_HEX(ServerWriteMacSecret),
    ?DBG_HEX(FinalClientWriteKey),
    ?DBG_HEX(FinalServerWriteKey),
    ?DBG_HEX(ClientIV),
    ?DBG_HEX(ServerIV),
    {ClientWriteMacSecret, ServerWriteMacSecret, FinalClientWriteKey,
     FinalServerWriteKey, ClientIV, ServerIV}.

suites() ->
    [ 
      %% TODO: uncomment when supported
      %%       ?TLS_DHE_RSA_WITH_AES_256_CBC_SHA,
      %%       ?TLS_DHE_DSS_WITH_AES_256_CBC_SHA,
      %% TODO: Funkar inte, borde: ?TLS_RSA_WITH_AES_256_CBC_SHA,
      %%       ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,
      %%       ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,
      ?TLS_RSA_WITH_3DES_EDE_CBC_SHA,
      %%       ?TLS_DHE_RSA_WITH_AES_128_CBC_SHA,
      %%       ?TLS_DHE_DSS_WITH_AES_128_CBC_SHA,
      ?TLS_RSA_WITH_AES_128_CBC_SHA,
      %%?TLS_DHE_DSS_WITH_RC4_128_SHA, TODO: Support this?
      %% ?TLS_RSA_WITH_IDEA_CBC_SHA, Not supported: in later openssl version than OTP requires

      ?TLS_RSA_WITH_RC4_128_SHA,
      ?TLS_RSA_WITH_RC4_128_MD5,
      ?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5,
      %%?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5,
      ?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA,
      %%?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA,
      ?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA,
      %%?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA,
      %%?TLS_DHE_DSS_WITH_RC4_128_SHA,

      ?TLS_RSA_WITH_DES_CBC_SHA,
      %%       ?TLS_DHE_RSA_EXPORT_WITH_DES40_CBC_SHA,
      %%       ?TLS_DHE_DSS_EXPORT_WITH_DES40_CBC_SHA,
      ?TLS_RSA_EXPORT_WITH_DES40_CBC_SHA,
      %%?TLS_RSA_EXPORT_WITH_RC2_CBC_40_MD5,
      ?TLS_RSA_EXPORT_WITH_RC4_40_MD5
     ].

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

hash(?MD5, Data) -> crypto:md5(Data);
hash(?SHA, Data) -> crypto:sha(Data).

hash_update(?MD5, Ctx, Data) -> crypto:md5_update(Ctx, Data);
hash_update(?SHA, Ctx, Data) -> crypto:sha_update(Ctx, Data).

hash_final(?MD5, Ctx) -> crypto:md5_final(Ctx);
hash_final(?SHA, Ctx) -> crypto:sha_final(Ctx).

%%pad_1(?NULL) ->
%%    "";
pad_1(?MD5) ->
    "666666666666666666666666666666666666666666666666";
pad_1(?SHA) ->
    "6666666666666666666666666666666666666666".

%%pad_2(?NULL) ->
%%    "";
pad_2(?MD5) ->
    "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
    "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\";
pad_2(?SHA) ->
    "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"
    "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\".

mac_hash(?NULL, _Secret, _Data) ->
    <<>>;
mac_hash(Method, Secret, Data) ->
    InnerHash = hash(Method, [Secret, pad_1(Method), Data]),
    hash(Method, [Secret, pad_2(Method), InnerHash]).

handshake_hash(Method, HandshakeHash, Extra) ->
    HSH = hash_update(Method, HandshakeHash, Extra),
    hash_final(Method, HSH).

handshake_hash(Method, MasterSecret, Sender, HandshakeHash) ->
    InnerHash = 
	handshake_hash(Method, HandshakeHash, [Sender, MasterSecret, pad_1(Method)]),
    Hash = hash(Method, [MasterSecret, pad_2(Method), InnerHash]),
    Hash.

get_sender(client) -> "CLNT";
get_sender(server) -> "SRVR";
get_sender(none) -> "".

generate_keyblock(MasterSecret, ServerRandom, ClientRandom, WantedLength) ->
    gen(MasterSecret, [MasterSecret, ServerRandom, ClientRandom],
	WantedLength, 0, $A, 1, []).

gen(_Secret, _All, Wanted, Len, _C, _N, Acc) when Wanted =< Len ->
    <<Block:Wanted/binary, _/binary>> = list_to_binary(lists:reverse(Acc)),
    Block;
gen(Secret, All, Wanted, Len, C, N, Acc) ->
    Prefix = lists:duplicate(N, C),
    SHA = crypto:sha([Prefix, All]),
    MD5 = crypto:md5([Secret, SHA]),
    gen(Secret, All, Wanted, Len + 16, C+1, N+1, [MD5 | Acc]).









