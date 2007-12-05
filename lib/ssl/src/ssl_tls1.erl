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
%% Purpose: Handles tls1 encryption.
%%----------------------------------------------------------------------

-module(ssl_tls1).

-include("ssl_cipher.hrl").
-include("ssl_internal.hrl").
-include("ssl_record.hrl"). 			% MD5 and SHA
-include("ssl_debug.hrl").

-export([master_secret/3, get_handshake_hashes/3, mac_hash/6, setup_keys/6,
	 suites/0]).

%%====================================================================
%% Internal application API
%%====================================================================
master_secret(PreMasterSecret, ClientRandom, ServerRandom) ->
    prf(PreMasterSecret, <<"master secret">>, [ClientRandom, ServerRandom], 48).

get_handshake_hashes(Role, MasterSecret, {MD5Hash, SHAHash}) ->
    MD5 = hash_final(?MD5, MD5Hash),
    SHA = hash_final(?SHA, SHAHash),
    prf(MasterSecret, finished_label(Role), [MD5, SHA], 12).

%%
%% {ClientWriteMacSecret, ServerWriteMacSecret,
%%  ClientWriteKey, ServerWriteKey,
%%  ClientIV, ServerIV}
%%
setup_keys(MasterSecret, ServerRandom, ClientRandom, HS,
	   KML, IVS) ->
    KeyBlock = prf(MasterSecret, "key expansion",
		   [ServerRandom, ClientRandom], 2*(HS+KML+IVS)),
    <<ClientWriteMacSecret:HS/binary, ServerWriteMacSecret:HS/binary,
     ClientWriteKey:KML/binary, ServerWriteKey:KML/binary,
     ClientIV:IVS/binary, ServerIV:IVS/binary>> = KeyBlock,
    {ClientWriteMacSecret, ServerWriteMacSecret, ClientWriteKey,
     ServerWriteKey, ClientIV, ServerIV}.


mac_hash(Method, Mac_write_secret, Seq_num, Type, Length, Fragment) ->
    case Method of
        ?NULL -> ok;
        _ ->
	    ?DBG_HEX(Mac_write_secret),
	    ?DBG_HEX(hash(Method, Fragment)),
            ok
    end,
    Mac = hmac_hash(Method, Mac_write_secret, 
		   [?uint64(Seq_num), ?byte(Type), ?uint16(Length), Fragment]),
    ?DBG_HEX(Mac),
    Mac.

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
      %%       ?TLS_RSA_WITH_IDEA_CBC_SHA,
      ?TLS_RSA_WITH_RC4_128_SHA,
      ?TLS_RSA_WITH_RC4_128_MD5,

      ?TLS_RSA_EXPORT1024_WITH_RC4_56_MD5,
      %%?TLS_RSA_EXPORT1024_WITH_RC2_CBC_56_MD5,
      ?TLS_RSA_EXPORT1024_WITH_DES_CBC_SHA,
      %%?TLS_DHE_DSS_EXPORT1024_WITH_DES_CBC_SHA,
      ?TLS_RSA_EXPORT1024_WITH_RC4_56_SHA,
      %%?TLS_DHE_DSS_EXPORT1024_WITH_RC4_56_SHA,
      %%?TLS_DHE_DSS_WITH_RC4_128_SHA,

      ?TLS_DHE_RSA_WITH_DES_CBC_SHA,
      %% EDH-DSS-DES-CBC-SHA  TODO: ??
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

%% p_hash(Method, Secret, Seed) ->
%%     Ctx0 = hash_init_pad(Method, Secret),
%%     CtxTmp0 = hash_init_pad(Method, Secret),
%%     ok.

%hash_init_pad(Method, Secret) ->
%    Sz = hash_sz(Method),
%    S = list_to_binary(


%% hash_sz(Method) ->
%%     {Sz, _} = hash_sizes(Method).


%% hash_init(md5) ->
%%     crypto:md5_init();
%% hash_init(sha) ->
%%     crypto:sha_init();
%% hash_init(null) ->
%%     null.

%% hash_update(md5, Ctx, Data) ->
%%     crypto:md5_update(Ctx, Data);
%% hash_update(sha, Ctx, Data) ->
%%     crypto:sha_update(Ctx, Data);
%% hash_update(null, null, _Data) ->
%%     null.

%% hash_final(md5, Ctx) ->
%%     crypto:md5_final(Ctx);
%% hash_final(sha, Ctx) ->
%%     crypto:sha_final(Ctx);
%% hash_final(null, null) ->
%%     <<>>.

split_secret(Bin) ->
    L = size(Bin),
    L2 = L div 2,
    L_L2 = L - L2,
    <<A:L_L2/binary, _/binary>> = Bin,
    <<_:L2/binary, B:L_L2/binary>> = Bin,
    {A, B}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% hash(?MD5, Data) -> crypto:md5(Data);
%% hash(?SHA, Data) -> crypto:sha(Data).

%% hash_update(?MD5, Ctx, Data) -> crypto:md5_update(Ctx, Data);
%% hash_update(?SHA, Ctx, Data) -> crypto:sha_update(Ctx, Data).

hash_final(?MD5, Ctx) -> crypto:md5_final(Ctx);
hash_final(?SHA, Ctx) -> crypto:sha_final(Ctx).

finished_label(client) ->
    <<"client finished">>;
finished_label(server) ->
    <<"server finished">>.

a(N, Seed, Secret, Method) ->
    a(0, N, Seed, Secret, Method, Seed).

a(N, N, _Seed, _Secret, _Method, Value) ->
    Value;
a(I, N, Seed, Secret, Method, Value0) ->
    Value1 = hmac_hash(Method, Secret, Value0),
    a(I+1, N, Seed, Secret, Method, Value1).

p_hash(Secret, Seed, WantedLength, Method) ->
    p_hash(Secret, Seed, WantedLength, Method, 0, []).

p_hash(_Secret, _Seed, WantedLength, _Method, _N, [])
  when WantedLength =< 0 ->
    [];
p_hash(_Secret, _Seed, WantedLength, _Method, _N, [Last | Acc])
  when WantedLength =< 0 ->
    Keep = size(Last)+WantedLength,
    <<B:Keep/binary, _/binary>> = Last,
    lists:reverse(Acc, [B]);
p_hash(Secret, Seed, WantedLength, Method, N, Acc) ->
    N1 = N+1,
    B = hmac_hash(Method, Secret, [a(N1, Secret, Seed, Method), Seed]),
    p_hash(Secret, Seed, WantedLength - size(B), Method, N1, [B | Acc]).

prf(Secret, Label, Seed, WantedLength) -> %% TODO: Should be internal ?
    {S1, S2} = split_secret(Secret),
    LS = list_to_binary([Label, Seed]),
    crypto:exor(p_hash(S1, LS, WantedLength, ?MD5),
		p_hash(S2, LS, WantedLength, ?SHA)).


hmac_hash(?NULL, _, _) ->
    <<>>;
hmac_hash(?MD5, Key, Value) ->
    crypto:md5_mac(Key, Value);
hmac_hash(?SHA, Key, Value) ->
    crypto:sha_mac(Key, Value).

