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

%% Purpose : Main Crypto API module.

-module(crypto).

-export([start/0, stop/0, info/0]).
-export([md5/1, md5_init/0, md5_update/2, md5_final/1]).
-export([sha/1, sha_init/0, sha_update/2, sha_final/1]).
-export([md5_mac/2, md5_mac_96/2, sha_mac/2, sha_mac_96/2]).
-export([des_cbc_encrypt/3, des_cbc_decrypt/3, des_cbc_ivec/1]).
-export([des3_cbc_encrypt/5, des3_cbc_decrypt/5]).
-export([des_ede3_cbc_encrypt/5, des_ede3_cbc_decrypt/5]).
-export([aes_cfb_128_encrypt/3, aes_cfb_128_decrypt/3]).
-export([exor/2]).
-export([rc4_encrypt/2, rc4_set_key/1, rc4_encrypt_with_state/2]).
-export([rc2_40_cbc_encrypt/3, rc2_40_cbc_decrypt/3]).
-export([dss_verify/3, rsa_verify/3]).
-export([rand_bytes/1, rand_bytes/3, rand_uniform/2]).
-export([mod_exp/3, mpint/1, erlint/1]).
%% -export([idea_cbc_encrypt/3, idea_cbc_decrypt/3]).
-export([aes_cbc_128_encrypt/3, aes_cbc_128_decrypt/3]).
-export([aes_cbc_256_encrypt/3, aes_cbc_256_decrypt/3]).


-define(INFO,		 0).
-define(MD5,		 1).
-define(MD5_INIT,	 2).
-define(MD5_UPDATE,	 3).
-define(MD5_FINAL,	 4).
-define(SHA,		 5).
-define(SHA_INIT,	 6).
-define(SHA_UPDATE,	 7).
-define(SHA_FINAL,	 8).
-define(MD5_MAC,	 9).
-define(MD5_MAC_96,	 10).
-define(SHA_MAC,	 11).
-define(SHA_MAC_96,	 12).
-define(DES_CBC_ENCRYPT, 13).
-define(DES_CBC_DECRYPT, 14).
-define(DES_EDE3_CBC_ENCRYPT, 15).
-define(DES_EDE3_CBC_DECRYPT, 16).
-define(AES_CFB_128_ENCRYPT, 17).
-define(AES_CFB_128_DECRYPT, 18).
-define(RAND_BYTES,	 19).
-define(RAND_UNIFORM,    20).
-define(MOD_EXP,	 21).
-define(DSS_VERIFY,	 22).
-define(RSA_VERIFY,	 23).
-define(AES_CBC_128_ENCRYPT, 24).
-define(AES_CBC_128_DECRYPT, 25).
-define(XOR,		 26).
-define(RC4_ENCRYPT,     27).
-define(RC4_SET_KEY, 28).
-define(RC4_ENCRYPT_WITH_STATE, 29).
-define(RC2_40_CBC_ENCRYPT, 30).
-define(RC2_40_CBC_DECRYPT, 31).
-define(AES_CBC_256_ENCRYPT, 32).
-define(AES_CBC_256_DECRYPT, 33).
%% -define(IDEA_CBC_ENCRYPT, 34).
%% -define(IDEA_CBC_DECRYPT, 35).

-define(FUNC_LIST, [md5,
		    md5_init,
		    md5_update,
		    md5_final,
		    sha,
		    sha_init,
		    sha_update,
		    sha_final,
		    md5_mac,
		    md5_mac_96,
		    sha_mac,
		    sha_mac_96,
		    des_cbc_encrypt, des_cbc_decrypt,
		    des_ede3_cbc_encrypt, des_ede3_cbc_decrypt,
		    aes_cfb_128_encrypt, aes_cfb_128_decrypt,
		    rand_bytes,
		    rand_uniform,
		    mod_exp,
		    dss_verify,
		    rsa_verify,
		    aes_cbc_128_encrypt, aes_cbc_128_decrypt,
		    exor,
		    rc4_encrypt, rc4_set_key, rc4_encrypt_with_state,
		    rc2_40_cbc_encrypt, rc2_40_cbc_decrypt,
		    %% idea_cbc_encrypt, idea_cbc_decrypt,
		    aes_cbc_256_encrypt, aes_cbc_256_decrypt]).

start() ->
    application:start(crypto).

stop() ->
    application:stop(crypto).

info() ->
    lists:map(fun(I) -> lists:nth(I, ?FUNC_LIST) end, 
	      binary_to_list(control(?INFO, []))).

%% Below Key and Data are binaries or IO-lists. IVec is a binary.
%% Output is always a binary. Context is a binary.

%%
%%  MESSAGE DIGESTS
%%

%%
%%  MD5
%%
md5(Data) ->
    control(?MD5, Data).

md5_init() ->
    control(?MD5_INIT, []).

md5_update(Context, Data) ->
    control(?MD5_UPDATE, [Context, Data]).

md5_final(Context) ->
    control(?MD5_FINAL, Context).

%%
%% SHA
%%
sha(Data) ->
    control(?SHA, Data).

sha_init() ->
    control(?SHA_INIT, []).

sha_update(Context, Data) ->
    control(?SHA_UPDATE, [Context, Data]).

sha_final(Context) ->
    control(?SHA_FINAL, Context).

%%
%%  MESSAGE AUTHENTICATION CODES
%%

%%
%%  MD5_MAC
%%
md5_mac(Key, Data) ->
    control_bin(?MD5_MAC, Key, Data).

md5_mac_96(Key, Data) ->
    control_bin(?MD5_MAC_96, Key, Data).

%%
%%  SHA_MAC
%%
sha_mac(Key, Data) ->
    control_bin(?SHA_MAC, Key, Data).

sha_mac_96(Key, Data) ->
    control_bin(?SHA_MAC_96, Key, Data).

%%
%% CRYPTO FUNCTIONS
%%

%%
%% DES - in cipher block chaining mode (CBC)
%%
des_cbc_encrypt(Key, IVec, Data) ->
    control(?DES_CBC_ENCRYPT, [Key, IVec, Data]).

des_cbc_decrypt(Key, IVec, Data) ->
    control(?DES_CBC_DECRYPT, [Key, IVec, Data]).

%%
%% dec_cbc_ivec(Data) -> binary()
%%
%% Returns the IVec to be used in the next iteration of 
%% des_cbc_[encrypt|decrypt].
%%
des_cbc_ivec(Data) when is_binary(Data) -> 
    {_, IVec} = split_binary(Data, size(Data) - 8),
    IVec;
des_cbc_ivec(Data) when is_list(Data) ->
    des_cbc_ivec(list_to_binary(Data)).

%%
%% DES3 - in cipher block chaining mode (CBC)
%%
des3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_encrypt(Key1, Key2, Key3, IVec, Data) ->
    %%io:format("des_ede3_cbc_encrypt: size(Data)=~p\n", [size(list_to_binary([Data]))]),
    control(?DES_EDE3_CBC_ENCRYPT, [Key1, Key2, Key3, IVec, Data]).

des3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data).
des_ede3_cbc_decrypt(Key1, Key2, Key3, IVec, Data) ->
    control(?DES_EDE3_CBC_DECRYPT, [Key1, Key2, Key3, IVec, Data]).

%%
%% AES in cipher feedback mode (CFB)
%%
aes_cfb_128_encrypt(Key, IVec, Data) ->
    control(?AES_CFB_128_ENCRYPT, [Key, IVec, Data]).

aes_cfb_128_decrypt(Key, IVec, Data) ->
    control(?AES_CFB_128_DECRYPT, [Key, IVec, Data]).    


%% %%
%% %% IDEA - in cipher block chaining mode (CBC)
%% %%
%% idea_cbc_encrypt(Key, IVec, Data) ->
%%     control(?IDEA_CBC_ENCRYPT, [Key, IVec, Data]).

%% idea_cbc_decrypt(Key, IVec, Data) ->
%%     control(?IDEA_CBC_DECRYPT, [Key, IVec, Data]).


%% 
%% RAND - pseudo random numbers using RN_ functions in crypto lib
%%

rand_bytes(Bytes) ->
    rand_bytes(Bytes, 0, 0).
rand_bytes(Bytes, Topmask, Bottommask) ->
    control(?RAND_BYTES,[<<Bytes:32/integer,
			  Topmask:8/integer,
			  Bottommask:8/integer>>]).

rand_uniform(From,To) when is_binary(From), is_binary(To) ->
    case control(?RAND_UNIFORM,[From,To]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end;
rand_uniform(From,To) when is_integer(From),is_integer(To) ->
    BinFrom = mpint(From),
    BinTo = mpint(To),
    case rand_uniform(BinFrom, BinTo) of
        Result when is_binary(Result) ->
            erlint(Result);
        Other ->
            Other
    end.

%%
%% mod_exp - utility for rsa generation
%%
mod_exp(Base, Exponent, Modulo)
  when is_integer(Base), is_integer(Exponent), is_integer(Modulo) ->
    erlint(mod_exp(mpint(Base), mpint(Exponent), mpint(Modulo)));

mod_exp(Base, Exponent, Modulo) ->
    case control(?MOD_EXP,[Base,Exponent,Modulo]) of
	<<Len:32/integer, MSB, Rest/binary>> when MSB > 127 ->
	    <<(Len + 1):32/integer, 0, MSB, Rest/binary>>;
	Whatever ->
	    Whatever
    end.

%%
%% DSS, RSA - verify
%%

dss_verify(Dgst,Signature,Key) ->
    control(?DSS_VERIFY, [Dgst,Signature,Key]) == <<1>>.

rsa_verify(Dgst,Signature,Key) ->
    control(?RSA_VERIFY, [Dgst,Signature,Key]) == <<1>>.

%%
%% AES - with 128 or 256 bit key in cipher block chaining mode (CBC)
%%

aes_cbc_128_encrypt(Key, IVec, Data) ->
    control(?AES_CBC_128_ENCRYPT, [Key, IVec, Data]).

aes_cbc_128_decrypt(Key, IVec, Data) ->
    control(?AES_CBC_128_DECRYPT, [Key, IVec, Data]).

aes_cbc_256_encrypt(Key, IVec, Data) ->
    control(?AES_CBC_256_ENCRYPT, [Key, IVec, Data]).

aes_cbc_256_decrypt(Key, IVec, Data) ->
    control(?AES_CBC_256_DECRYPT, [Key, IVec, Data]).

%%
%% XOR - xor to iolists and return a binary
%% NB doesn't check that they are the same size, just concatenates
%% them and sends them to the driver
%%
exor(A, B) ->
    control(?XOR, [A, B]).

%%
%% RC4 - symmetric stream cipher
%%
rc4_encrypt(Key, Data) ->
    control_bin(?RC4_ENCRYPT, Key, Data).

rc4_set_key(Key) ->
    control(?RC4_SET_KEY, Key).

rc4_encrypt_with_state(State, Data) ->
    <<Sz:32/integer-big-unsigned, S:Sz/binary, D/binary>> = 
        control_bin(?RC4_ENCRYPT_WITH_STATE, State, Data),
    {S, D}.

%%
%% RC2 - 40 bits block cipher
%%
rc2_40_cbc_encrypt(Key, IVec, Data) ->
    control(?RC2_40_CBC_ENCRYPT, [Key, IVec, Data]).

rc2_40_cbc_decrypt(Key, IVec, Data) ->
    control(?RC2_40_CBC_DECRYPT, [Key, IVec, Data]).

%%
%%  LOCAL FUNCTIONS
%%
control_bin(Cmd, Key, Data) ->
    Sz = iolist_size(Key),
    control(Cmd, [<<Sz:32/integer-unsigned>>, Key, Data]).

control(Cmd, Data) ->
    [{port, Port}| _] = ets:lookup(crypto_server_table, port),
    erlang:port_control(Port, Cmd, Data).

%% sizehdr(N) ->
%%     [(N bsr 24) band 255,
%%      (N bsr 16) band 255,
%%      (N bsr  8) band 255,
%%      N band 255].

%% Flat length of IOlist (or binary)
%% flen(L) when binary(L) ->
%%     size(L);
%% flen(L) ->
%%     flen(L, 0).

%% flen([H| T], N) when list(H) ->
%%     flen(H, flen(T, N));
%% flen([H| T], N) when binary(H) ->
%%     flen(T, N + size(H));
%% flen([H| T], N) when integer(H), 0 =< H, H =<  255 ->
%%     flen(T, N + 1);
%% flen([], N) ->
%%     N.

%% large integer in a binary with 32bit length
%% MP representaion  (SSH2)
mpint(X) when X < 0 ->
    case X of
	-1 ->
	    <<0,0,0,1,16#ff>>;	    
       _ ->
	    mpint_neg(X,0,[])
    end;
mpint(X) ->
    case X of 
	0 ->
	    <<0,0,0,0>>;
	_ ->
	    mpint_pos(X,0,[])
    end.

-define(UINT32(X),   X:32/unsigned-big-integer).

mpint_neg(-1,I,Ds=[MSB|_]) ->
    if MSB band 16#80 =/= 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([255|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_neg(X,I,Ds)  ->
    mpint_neg(X bsr 8,I+1,[(X band 255)|Ds]).
    
mpint_pos(0,I,Ds=[MSB|_]) ->
    if MSB band 16#80 == 16#80 ->
	    <<?UINT32((I+1)), (list_to_binary([0|Ds]))/binary>>;
       true ->
	    (<<?UINT32(I), (list_to_binary(Ds))/binary>>)
    end;
mpint_pos(X,I,Ds) ->
    mpint_pos(X bsr 8,I+1,[(X band 255)|Ds]).

%% int from integer in a binary with 32bit length
erlint(<<MPIntSize:32/integer,MPIntValue/binary>>) ->
    Bits= MPIntSize * 8,
    <<Integer:Bits/integer>> = MPIntValue,
    Integer.
